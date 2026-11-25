{-# LANGUAGE CPP #-}
#ifndef __MHS__
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#ifdef __MHS__
{- FOURMOLU_DISABLE -}
module Distribution.Client.Compat.Tar
  ( extractTarGzFile
  , createTarGzFile
  -- * Pure tar reader (for index files)
  , Entry (..)
  , Entries (..)
  , EntryContent (..)
  , TypeCode
  , FormatError (..)
  , TarPath (..)
  , EpochTime
  , Permissions
  , ordinaryFilePermissions
  , toTarPath
  , entryPath
  , entryTarPath
  , read
  , hReadEntry
  , nextEntryOffset
  , foldEntries
  , foldlEntries
  -- * Pure tar writer (for @v2-sdist@)
  , directoryEntry
  , fileEntry
  , write
  ) where
{- FOURMOLU_ENABLE -}

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (digitToInt)

import Distribution.Compat.Directory
  ( createDirectoryIfMissing
  , findExecutable
  )
import qualified Distribution.Compat.Process as Process
import System.FilePath
  ( isAbsolute
  , normalise
  , splitDirectories
  , (</>)
  )
import System.IO
  ( Handle
  , SeekMode (AbsoluteSeek)
#ifndef __MHS__
  , hSeek
#endif
  )
#ifdef __MHS__
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import System.IO.Internal (BFILE, withHandleAny)
#endif
import System.IO.Error (userError)

-- | A validated path inside a tar archive: relative, without @..@
-- components. Build one with 'toTarPath'.
newtype TarPath = TarPath { fromTarPath :: FilePath }
  deriving (Eq, Ord, Show)

-- | Time in seconds since the Unix epoch, as stored in tar headers.
type EpochTime = Int64

-- | File permission bits (the low 12 bits of the mode field).
type Permissions = Int

-- | Default permissions for regular files (0o644).
ordinaryFilePermissions :: Permissions
ordinaryFilePermissions = 420

-- | Default permissions for directories (0o755).
directoryPermissions :: Permissions
directoryPermissions = 493

-- | A tar archive entry: its path, content, permissions and modification time.
data Entry = Entry
  { entryTarPath :: TarPath
  , entryContent :: EntryContent
  , entryPermissions :: Permissions
  , entryTime :: EpochTime
  }
  deriving (Show)

-- | The file path of a tar archive entry.
entryPath :: Entry -> FilePath
entryPath = fromTarPath . entryTarPath

-- | Validate a file path for use inside a tar archive.
toTarPath :: Bool -> FilePath -> Either String TarPath
toTarPath _isDir fp
  | null norm = Left "empty tar path"
  | isAbsolute norm = Left ("absolute tar path: " ++ show fp)
  | ".." `elem` splitDirectories norm = Left ("tar path escapes its directory: " ++ show fp)
  | otherwise = Right (TarPath norm)
  where
    norm = normalise fp

-- | The content of a tar archive entry.
data EntryContent
  = NormalFile LBS.ByteString Int
  | Directory
  | OtherEntryType TypeCode LBS.ByteString Int
  deriving (Show)

-- | Tar type-flag code (a single 'Char').
type TypeCode = Char

-- | Failure to parse a tar archive.
data FormatError = FormatError String
  deriving (Show)

instance Exception FormatError

-- | A (possibly partial) sequence of tar entries.
data Entries e
  = Next Entry (Entries e)
  | Done
  | Fail e
  deriving (Show)

-- | Fold over a sequence of entries.
foldEntries :: (Entry -> a -> a) -> a -> (e -> a) -> Entries e -> a
foldEntries next done fail = go
  where
    go (Next e es) = next e (go es)
    go Done = done
    go (Fail e) = fail e

-- | Strict left fold over a sequence of entries. On a malformed entry
-- the partial accumulator is returned together with the error.
foldlEntries :: (a -> Entry -> a) -> a -> Entries e -> Either (e, a) a
foldlEntries f = go
  where
    go acc (Next e es) = go (f acc e) es
    go acc Done = Right acc
    go acc (Fail e) = Left (e, acc)

-- | Read the entries of an uncompressed tar archive, lazily in the
-- spine of the resulting 'Entries' structure.
read :: LBS.ByteString -> Entries FormatError
read = go Nothing
  where
    go _longName bs
      | LBS.null bs = Done
      | isZeroBlock (LBS.take 512 bs) = Done
      | LBS.length bs < 512 = Fail (FormatError "short tar archive")
      | otherwise = case parseHeader (BS.concat (LBS.toChunks (LBS.take 512 bs))) of
          Left err -> Fail err
          Right (rawName, size, typeCode, perms, mtime) ->
            let body = LBS.take (fromIntegral size) (LBS.drop 512 bs)
                rest = dropPadding size (LBS.drop (512 + fromIntegral size) bs)
             in if LBS.length body < fromIntegral size
                  then Fail (FormatError "short tar entry")
                  else case typeCode of
                    -- GNU long name: the body is the path of the next entry.
                    'L' -> go (Just (normaliseLongName body)) rest
                    -- GNU long linkname: irrelevant for reading, skip it.
                    'K' -> go Nothing rest
                    _ ->
                      let name = fromMaybe rawName _longName
                       in Next (Entry (TarPath name) (mkContent typeCode body size) perms mtime) (go Nothing rest)

#ifdef __MHS__
foreign import ccall "mhs_handle.h mhs_handle_seek"
  c_handleSeek :: Ptr BFILE -> CInt -> Int64 -> IO CInt

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h mode off =
  withHandleAny h $ \p ->
    throwErrnoIfMinus1_ "hSeek" $
      c_handleSeek p (fromIntegral (fromEnum mode)) (fromIntegral off)
#endif

-- | Read a single entry at the given 512-byte block offset.
--
-- Throws 'FormatError' on parse failure.
hReadEntry :: Handle -> Word32 -> IO Entry
hReadEntry h blockNo = do
  hSeek h AbsoluteSeek (fromIntegral blockNo * 512)
  hdr <- readExactly h 512
  (rawName, size, typeCode, perms, mtime) <- either throwIO return (parseHeader hdr)
  body <- readExactlyChunks h size
  return (Entry (TarPath rawName) (mkContent typeCode (LBS.fromChunks body) size) perms mtime)

-- | The block offset of the entry following the given one.
nextEntryOffset :: Entry -> Word32 -> Word32
nextEntryOffset entry blockNo = blockNo + 1 + contentBlocks
  where
    contentBlocks = fromIntegral ((contentSize + 511) `div` 512)
    contentSize :: Int
    contentSize = case entryContent entry of
      NormalFile content _ -> fromIntegral (LBS.length content)
      OtherEntryType _ content _ -> fromIntegral (LBS.length content)
      Directory -> 0

-- | Skip @size@ bytes of content plus padding to a 512-byte boundary.
dropPadding :: Int -> LBS.ByteString -> LBS.ByteString
dropPadding size bs = LBS.drop (fromIntegral padding) bs
  where
    padding = (512 - size `mod` 512) `mod` 512

mkContent :: TypeCode -> LBS.ByteString -> Int -> EntryContent
mkContent typeCode body size
  | typeCode == '0' || typeCode == '\0' = NormalFile body size
  | typeCode == '5' = Directory
  | otherwise = OtherEntryType typeCode body size

normaliseLongName :: LBS.ByteString -> FilePath
normaliseLongName = normalise . BSChar8.unpack . BS.concat . LBS.toChunks . fst . LBS.span (/= 0)

isZeroBlock :: LBS.ByteString -> Bool
isZeroBlock = LBS.all (== 0)

-- | Parse a 512-byte ustar header block into
-- (path, size, type code, permissions, modification time).
parseHeader :: BS.ByteString -> Either FormatError (FilePath, Int, TypeCode, Permissions, EpochTime)
parseHeader blk
  | BS.length blk < 512 = Left (FormatError "short tar header")
  | otherwise = do
      name <- Right (fieldString 0 100)
      prefix <- Right (fieldString 345 155)
      size <- parseNumberField blk 124 12
      mode <- parseNumberField blk 100 8
      mtime <- parseNumberField blk 136 12
      let typeCode = if BS.index blk 156 == 0 then '\0' else BSChar8.index blk 156
          path
            | null prefix = name
            | otherwise = prefix </> name
      return (path, size, typeCode, mode `mod` 4096, fromIntegral mtime)
  where
    fieldString off len =
      BSChar8.unpack (BS.takeWhile (/= 0) (BS.take len (BS.drop off blk)))

-- | Parse an octal (or base-256) numeric header field.
parseNumberField :: BS.ByteString -> Int -> Int -> Either FormatError Int
parseNumberField blk off len = parseField (BS.take len (BS.drop off blk))
  where
    parseField raw
      | BS.null raw = Right 0
      | BS.head raw >= 0x80 = Right (base256 (BS.tail raw))
      | otherwise =
          let s = filter (/= ' ') (BSChar8.unpack (BS.takeWhile (/= 0) raw))
           in if null s
                then Right 0
                else
                  if all (`elem` ("01234567" :: String)) s
                    then Right (foldl (\a c -> a * 8 + digitToInt c) 0 s)
                    else Left (FormatError "bad tar numeric field")

base256 :: BS.ByteString -> Int
base256 = BS.foldl' (\a b -> a * 256 + fromIntegral b) 0

-------------------------------------------------------------------------------
-- Pure tar writer
-------------------------------------------------------------------------------

-- | A directory entry with default directory permissions.
directoryEntry :: TarPath -> Entry
directoryEntry tp = Entry tp Directory directoryPermissions 0

-- | A regular file entry with default file permissions.
fileEntry :: TarPath -> ByteString -> Entry
fileEntry tp content =
  Entry tp (NormalFile content (fromIntegral (LBS.length content))) ordinaryFilePermissions 0

-- | Serialise entries to an uncompressed tar archive, terminated by two
-- zero blocks as usual. Paths that do not fit the ustar name/prefix
-- fields are stored with a GNU longname (@L@) entry, which 'read'
-- understands.
write :: [Entry] -> ByteString
write entries = LBS.concat (map putEntry entries ++ [LBS.replicate 1024 0])

putEntry :: Entry -> ByteString
putEntry entry = LBS.concat (longHeader ++ [hdr, content, pad])
  where
    (typeCode, content) = case entryContent entry of
      NormalFile c _ -> ('0', c)
      Directory -> ('5', LBS.empty)
      OtherEntryType t c _ -> (t, c)
    size = fromIntegral (LBS.length content) :: Int
    rawName = fromTarPath (entryTarPath entry)
    -- Directory entries are stored with a trailing slash.
    -- NOTE: 'last' is NonEmpty-specialised in this Prelude, so compare
    -- the final character via 'drop' instead.
    name = case entryContent entry of
      Directory | not (null rawName) && drop (length rawName - 1) rawName /= "/" -> rawName ++ "/"
      _ -> rawName
    (prefix, shortName, longHeader) = encodeName name
    hdr = LBS.fromStrict (encodeHeader shortName prefix size (entryPermissions entry) (entryTime entry) typeCode)
    pad = LBS.replicate (fromIntegral ((512 - size `mod` 512) `mod` 512)) 0

-- | Split a path for the ustar name/prefix fields, falling back to a GNU
-- longname entry. Returns @(prefix, name, extraHeaders)@.
encodeName :: FilePath -> (String, String, [ByteString])
encodeName name
  | length name <= 100 = ("", name, [])
  | Just (prefix, base) <- splitUstarName name = (prefix, base, [])
  | otherwise =
      let content = LBS.fromStrict (BSChar8.pack (name ++ "\0"))
          longSize = length name + 1
          hdr = LBS.fromStrict (encodeHeader "././@LongLink" "" longSize 0 0 'L')
          pad = LBS.replicate (fromIntegral ((512 - longSize `mod` 512) `mod` 512)) 0
       in ("", take 100 name, [LBS.concat [hdr, content, pad]])

-- | Split a long path at a @/@ so that the prefix fits in 155 bytes and
-- the remainder in 100 bytes, preferring the longest such prefix.
splitUstarName :: FilePath -> Maybe (String, String)
splitUstarName name =
  listToMaybe
    [ (prefix, rest)
    | i <- reverse [1 .. length name - 1]
    , name !! i == '/'
    , let prefix = take i name
    , let rest = drop (i + 1) name
    , not (null rest)
    , length prefix <= 155
    , length rest <= 100
    ]

-- | Encode a single 512-byte ustar header block.
encodeHeader :: FilePath -> FilePath -> Int -> Permissions -> EpochTime -> TypeCode -> BS.ByteString
encodeHeader name prefix size perms mtime typeCode = BSChar8.pack withChecksum
  where
    raw =
      padField name 100
        ++ octalField 8 perms
        ++ octalField 8 0 -- uid
        ++ octalField 8 0 -- gid
        ++ octalField 12 size
        ++ octalField 12 (fromIntegral mtime)
        ++ replicate 8 ' ' -- checksum placeholder
        ++ [typeCode]
        ++ padField "" 100 -- linkname
        ++ "ustar\0"
        ++ "00"
        ++ padField "" 32 -- uname
        ++ padField "" 32 -- gname
        ++ octalField 8 0 -- devmajor
        ++ octalField 8 0 -- devminor
        ++ padField prefix 155
        ++ replicate 12 '\0'
    chk = sum (map ord raw)
    chkField = let s = toOctal chk in replicate (6 - length s) '0' ++ s ++ "\0 "
    withChecksum = take 148 raw ++ chkField ++ drop 156 raw

-- | A NUL-padded string field of the given width.
padField :: String -> Int -> String
padField s w = take w (s ++ repeat '\0')

-- | An octal numeric field of the given width, NUL-terminated.
octalField :: Int -> Int -> String
octalField w v = replicate (w - 1 - length s) '0' ++ s ++ "\0"
  where
    s = toOctal v

-- | Render a non-negative number in octal.
toOctal :: Int -> String
toOctal 0 = "0"
toOctal n = reverse (go n)
  where
    go 0 = []
    go x = "01234567" !! (x `mod` 8) : go (x `div` 8)

readExactly :: Handle -> Int -> IO BS.ByteString
readExactly h n = BS.concat <$> readExactlyChunks h n

readExactlyChunks :: Handle -> Int -> IO [BS.ByteString]
readExactlyChunks _ n | n <= 0 = return []
readExactlyChunks h n = do
  chunk <- BS.hGet h n
  if BS.null chunk
    then throwIO (FormatError "short tar archive")
    else (chunk :) <$> readExactlyChunks h (n - BS.length chunk)

-------------------------------------------------------------------------------
-- External @tar@ for packing and unpacking
-------------------------------------------------------------------------------

-- | Unpack a @.tar.gz@ file into a destination directory, checking for
-- tarbombs: every entry must live under the expected subdirectory, and
-- no entry may be absolute or contain @..@.
extractTarGzFile
  :: FilePath
  -- ^ Destination directory
  -> FilePath
  -- ^ Expected subdir (to check for tarbombs)
  -> FilePath
  -- ^ Tarball
  -> IO ()
extractTarGzFile dir expected tar = do
  tarProg <- findTar
  listing <-
    Process.readCreateProcessWithExitCode
      (Process.proc tarProg ["-tzf", tar])
      ""
  names <- case listing of
    (ExitSuccess, out, _) -> return (lines out)
    (ExitFailure n, _, err) ->
      throwIO $
        userError $
          "tar -tzf failed with exit code " ++ show n ++ ": " ++ err
  traverse_ (checkEntry expected) names
  createDirectoryIfMissing True dir
  Process.callProcess tarProg ["-xzf", tar, "-C", dir]

-- | Create a @.tar.gz@ file from a directory, relative to a base directory.
createTarGzFile
  :: FilePath
  -- ^ Full Tarball path
  -> FilePath
  -- ^ Base directory
  -> FilePath
  -- ^ Directory to archive, relative to base dir
  -> IO ()
createTarGzFile tar base dir = do
  tarProg <- findTar
  Process.callProcess tarProg ["-czf", tar, "-C", base, dir]

findTar :: IO FilePath
findTar = do
  mbTar <- findExecutable "tar"
  case mbTar of
    Just tar -> return tar
    Nothing -> throwIO (userError "The external 'tar' program is required but was not found on PATH.")

checkEntry :: FilePath -> FilePath -> IO ()
checkEntry expected rawName = do
  let name = stripTrailingSlash (stripDotSlash (normalise rawName))
  when (null name || isAbsolute name || (".." `elem` splitDirectories name)) $
    throwIO (userError ("tar entry is not a safe relative path: " ++ show rawName))
  unless (name == expected || (expected ++ "/") `isPrefixOf` name) $
    throwIO (userError ("tar entry outside expected directory " ++ show expected ++ ": " ++ show rawName))

stripDotSlash :: FilePath -> FilePath
stripDotSlash name
  | "./" `isPrefixOf` name = stripDotSlash (drop 2 name)
  | otherwise = name

stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash name
  | "/" `isSuffixOf` name && length name > 1 = stripTrailingSlash (take (length name - 1) name)
  | otherwise = name

#else
{- FOURMOLU_DISABLE -}
module Distribution.Client.Compat.Tar
  ( extractTarGzFile
  , createTarGzFile
#if MIN_VERSION_tar(0,6,0)
  , Tar.Entry
  , Tar.Entries
  , Tar.GenEntries (..)
  , Tar.GenEntryContent (..)
  , Tar.entryContent
#else
  , Tar.Entries (..)
  , Tar.Entry (..)
  , Tar.EntryContent (..)
#endif
  ) where
{- FOURMOLU_ENABLE -}

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar
#if MIN_VERSION_tar(0,6,0)
#else
import qualified Codec.Archive.Tar.Entry as Tar
#endif
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Client.GZipUtils as GZipUtils

instance (Exception a, Exception b) => Exception (Either a b) where
  toException (Left e) = toException e
  toException (Right e) = toException e

  fromException e =
    case fromException e of
      Just e' -> Just (Left e')
      Nothing -> case fromException e of
        Just e' -> Just (Right e')
        Nothing -> Nothing

{- FOURMOLU_DISABLE -}
extractTarGzFile
  :: FilePath
  -- ^ Destination directory
  -> FilePath
  -- ^ Expected subdir (to check for tarbombs)
  -> FilePath
  -- ^ Tarball
  -> IO ()
extractTarGzFile dir expected tar =
#if MIN_VERSION_tar(0,6,0)
  Tar.unpackAndCheck
    ( \x ->
        SomeException <$> Tar.checkEntryTarbomb expected x
          <|> SomeException <$> Tar.checkEntrySecurity x
    )
    dir
#else
  Tar.unpack dir
    . Tar.checkTarbomb expected
#endif
  . Tar.read
  . GZipUtils.maybeDecompress
  =<< BS.readFile tar

createTarGzFile
  :: FilePath
  -- ^ Full Tarball path
  -> FilePath
  -- ^ Base directory
  -> FilePath
  -- ^ Directory to archive, relative to base dir
  -> IO ()
createTarGzFile tar base dir =
#if MIN_VERSION_tar(0,7,0)
  BS.writeFile tar . GZip.compress =<< Tar.write' =<< Tar.pack' base [dir]
#else
  BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base [dir]
#endif

{- FOURMOLU_ENABLE -}
#endif
