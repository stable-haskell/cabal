{-# LANGUAGE CPP #-}
-- | Process spawning primitives for the MHS (MicroHs) build.
--
-- MicroHs has no @process@ or @unix@ packages and its 'Handle's are
-- @stdio@-based, so there is no file-descriptor-to-'Handle'
-- conversion.  Spawning is implemented on top of @posix_spawnp(3)@
-- (via the @mhs_spawn@ C helper in @cbits/mhs_spawn.c@, which hides
-- the platform-specific @posix_spawn_file_actions_t@ layout),
-- termination via @kill(2)@, and waiting via @waitpid(2)@ with
-- @WNOHANG@ polled with 'threadDelay'.
--
-- Pipes ('CreatePipe' streams) fall back to temporary files: the
-- child is redirected to a pre-created temporary file and the parent
-- communicates through 'Handle's opened on that file.  This keeps
-- unidirectional, write-then-wait-then-read protocols working, but it
-- cannot reproduce the blocking/streaming semantics of real pipes;
-- see the note on 'mhsCreatePipe'.
--
-- The imports used here are available both in GHC @base@ and in the
-- MicroHs base libraries (plus the @filepath@ package, which the MHS
-- build installs), so this module compiles unchanged on both sides.
-- It is shared plumbing for "Distribution.Compat.Process" and
-- "Distribution.Compat.CreatePipe"; it is not part of the public API.
module Distribution.Compat.Internal.MhsProcess
  ( MhsRedirect (..)
  , MhsChild
  , mhsSpawn
  , mhsWait
  , mhsTryWait
  , mhsJoin
  , mhsRelease
  , mhsTerminate
  , mhsTempFile
  , mhsRegisterTemp
  , mhsLookupTemp
  , mhsCreatePipe
  , withMhsCwd
#ifdef __MHS__
  , hIsTextMode
  , readBytes
#endif
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch)
import qualified Data.ByteString as BS
#ifdef __MHS__
import Data.Char (chr, ord)
import System.IO.Internal (BFILE, withHandleAny)
#endif
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Distribution.Compat.Directory (doesFileExist, getCurrentDirectory, getTemporaryDirectory, removeFile, setCurrentDirectory)
import Distribution.Compat.Internal.TempFile (openBinaryTempFile)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.String (CString, newCAString, withCAString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, free)
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, pokeElemOff, sizeOf)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering, openFile
#ifdef __MHS__
  , hGetContents, hPutStr
#endif
  )
import System.IO.Unsafe (unsafePerformIO)

-- | How one standard stream of the child is wired up.
data MhsRedirect
  = MhsInherit
  | -- | Inherit the stream from the parent.
    MhsNull
  | -- | Redirect the stream to @\/dev\/null@.
    MhsFd Int
  | -- | Duplicate an existing file descriptor (0, 1 or 2 for the
    -- standard streams passed through as 'UseHandle').
    MhsFile FilePath
  deriving (Eq, Show)

-- | A spawned child: its pid, a cache for its exit code (so repeated
-- waits are cheap and idempotent), the temporary files created
-- for its pipe streams, removed once the child is reaped, and
-- @(path, handle)@ pairs whose contents are appended to the handle
-- on reap (for @UseHandle@ of regular files).
data MhsChild = MhsChild
  { mhsPid :: Int
  , mhsStatus :: IORef (Maybe ExitCode)
  , mhsTempFiles :: [FilePath]
  , mhsCopies :: [(FilePath, Handle)]
  }

foreign import ccall "mhs_spawn.h mhs_spawn" c_mhs_spawn
  :: CString -> Ptr CString -> CString -> CInt -> CString -> CInt -> CString -> CInt -> Ptr CString -> IO CInt
foreign import ccall "mhs_spawn.h mhs_wait_exited" c_mhs_wait_exited
  :: CInt -> Ptr CInt -> IO CInt
foreign import ccall "mhs_spawn.h mhs_terminate" c_mhs_terminate
  :: CInt -> IO CInt

-- | How often, in microseconds, 'mhsWait' polls @waitpid@.
mhsPollMicros :: Int
mhsPollMicros = 10000

-- | Marshal one stream redirection for the C helper: a path to open
-- (or 'nullPtr') plus a file descriptor to duplicate (or -1).
withMhsRedirect :: MhsRedirect -> (CString -> CInt -> IO a) -> IO a
withMhsRedirect MhsInherit io = io nullPtr (-1)
withMhsRedirect (MhsFd fd) io = io nullPtr (fromIntegral fd)
withMhsRedirect (MhsFile path) io =
  withCAString path $ \cpath -> io cpath (-1)
withMhsRedirect MhsNull io =
  withCAString "/dev/null" $ \cnull -> io cnull (-1)

-- | Marshal an argument vector as a NULL-terminated array.
withArgv :: [String] -> (Ptr CString -> IO a) -> IO a
withArgv args io =
  bracket
    (mapM newCAString args)
    (mapM_ free)
    ( \cstrs ->
        allocaBytes ((length cstrs + 1) * ptrSize) $ \arr -> do
          mapM_ (\(i, cs) -> pokeElemOff arr i cs) (zip [0 ..] cstrs)
          pokeElemOff arr (length cstrs) (nullPtr :: CString)
          io arr
    )
  where
    ptrSize = sizeOf (nullPtr :: CString)

-- | Run an action in a different working directory, restoring the
-- previous one afterwards.  Like process creation itself this is not
-- safe against concurrent spawns with different directories from
-- other threads.
withMhsCwd :: Maybe FilePath -> IO a -> IO a
withMhsCwd Nothing io = io
withMhsCwd (Just dir) io =
  bracket
    getCurrentDirectory
    setCurrentDirectory
    (const (setCurrentDirectory dir >> io))

-- | Spawn a child with @posix_spawnp@ (so @prog@ is looked up along
-- @PATH@), wiring its standard streams as directed.  @temps@ lists
-- temporary files to delete once the child is reaped.
mhsSpawn
  :: FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> MhsRedirect
  -> MhsRedirect
  -> MhsRedirect
  -> [FilePath]
  -> [(FilePath, Handle)]
  -- ^ @(path, handle)@ copies performed on reap (see 'MhsChild')
  -> IO MhsChild
-- | Marshal an explicit environment for the child: 'Nothing' passes
-- a null pointer (the child inherits the parent environment),
-- @Just []@ passes an empty environment, and @Just pairs@ passes
-- exactly those bindings.  The parent environment is never modified,
-- so unlike clearing and restoring it this is safe with respect to
-- other threads.
withEnvp :: Maybe [(String, String)] -> (Ptr CString -> IO a) -> IO a
withEnvp Nothing io = io nullPtr
withEnvp (Just pairs) io =
  withCStrings (map (\(k, v) -> k ++ "=" ++ v) pairs) $ \cstrs ->
    withArray0 nullPtr cstrs io
  where
    withCStrings [] f = f []
    withCStrings (s : ss) f =
      withCAString s $ \cs ->
        withCStrings ss $ \css -> f (cs : css)

mhsSpawn prog args mcwd menv redirIn redirOut redirErr temps copies =
  withMhsCwd mcwd $
    withEnvp menv $ \cenvp ->
      withCAString prog $ \cprog ->
        withArgv (prog : args) $ \cargv ->
          withMhsRedirect redirIn $ \cIn fdIn ->
            withMhsRedirect redirOut $ \cOut fdOut ->
              withMhsRedirect redirErr $ \cErr fdErr -> do
                pid <-
                  throwErrnoIfMinus1
                    "mhsSpawn"
                    (c_mhs_spawn cprog cargv cIn fdIn cOut fdOut cErr fdErr cenvp)
                st <- newIORef Nothing
                pure (MhsChild (fromIntegral pid) st temps copies)

-- | Single non-blocking reap probe.
mhsProbe :: MhsChild -> IO (Maybe ExitCode)
mhsProbe (MhsChild pid _ _ _) =
  alloca $ \pcode -> do
    r <- throwErrnoIfMinus1 "mhsWait" (c_mhs_wait_exited (fromIntegral pid) pcode)
    if r == 0
      then pure Nothing
      else do
        code <- peek pcode
        pure (Just (if code == 0 then ExitSuccess else ExitFailure (fromIntegral code)))

-- | Append a file's contents to a 'Handle', for flushing pipe
-- redirections of regular-file 'UseHandle's on reap.  Missing files
-- (already flushed by an earlier reap observation) are skipped
-- silently; any other failure is returned so the caller can report
-- it instead of silently losing the child's output.
copyBack :: FilePath -> Handle -> IO (Maybe IOError)
copyBack fp h = do
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    -- Strict bytes: lazy 'LBS.readFile' needs 'hGetSome', which
    -- MicroHs does not implement.
#ifdef __MHS__
    -- The destination handle may be in text mode (e.g. a build log
    -- opened with 'withFile'), where @c_writeb@ falls back to
    -- per-byte @putb_utf8@ and would double-encode every byte above
    -- 0x7F; 'copyBytes' picks the exact writer per handle mode.
    else catch (BS.readFile fp >>= copyBytes h >> pure Nothing) (pure . Just)
#else
    else catch (BS.readFile fp >>= BS.hPutStr h >> pure Nothing) (pure . Just)
#endif

#ifdef __MHS__
-- | Append bytes to a handle of either text or binary mode.
--
-- 'BS.hPut' uses @c_writeb@, which MicroHs' UTF-8 transducer layer
-- does not implement: on a text-mode handle it falls back to
-- per-byte @putb_utf8@, UTF-8-encoding every byte above 0x7F a
-- second time (mojibake).  Binary handles have a real @writeb@ and
-- take raw bytes.  The mode is detected with 'hIsTextMode' and each
-- side uses its exact writer: text handles get the UTF-8 decoding of
-- the bytes through 'hPutStr' (which re-encodes them identically),
-- binary handles get the bytes untouched.
copyBytes :: Handle -> BS.ByteString -> IO ()
copyBytes h bs = do
  text <- hIsTextMode h
  if text
    then hPutStr h (decodeUtf8Lossy bs)
    else BS.hPut h bs

-- | Read the raw bytes of a handle of either text or binary mode.
--
-- 'BS.hGetContents' uses @c_readb@, which MicroHs' UTF-8 transducer
-- layer does not implement: on a text-mode handle it falls back to
-- per-character @getb_utf8@, truncating every decoded code point
-- above 0x7F to a single byte.  Binary handles have a real @readb@
-- and yield raw bytes.  The mode is detected with 'hIsTextMode' and
-- each side uses its exact reader: text handles are read as
-- characters and the transducer decoding is inverted (see
-- 'charToRawBytes'), binary handles are read untouched.
readBytes :: Handle -> IO BS.ByteString
readBytes h = do
  text <- hIsTextMode h
  if text
    then hGetRawBytes h
    else BS.hGetContents h

-- | Whether a handle carries MicroHs' UTF-8 transducer layer (that
-- is, it was opened with 'openFile', not 'openBinaryFile').
-- MicroHs has no 'hSetBinaryMode' and exposes no mode query; the
-- outermost 'BFILE' of a text handle is the UTF-8 transducer (see
-- @c_add_utf8@), identified through the C helper
-- @mhs_handle_is_text@, which compares its @getb@ implementation.
-- Pinned to the MicroHs 0.16.12.0 runtime layout (the method table
-- starts the @BFILE@ struct); the @T4b@ capture tests cover both
-- handle kinds and fail if a runtime upgrade moves it.
hIsTextMode :: Handle -> IO Bool
hIsTextMode h = withHandleAny h $ \p -> (/= 0) <$> c_handleIsText p

foreign import ccall "mhs_handle.h mhs_handle_is_text" c_handleIsText :: Ptr BFILE -> IO CInt

-- | Decode bytes as UTF-8, mirroring MicroHs' @getb_utf8@: valid
-- sequences become the decoded characters, undecodable bytes become
-- the replacement character (never a surrogate: 'hPutChar' rejects
-- those, and the C layer only round-trips its own escapes).
decodeUtf8Lossy :: BS.ByteString -> String
decodeUtf8Lossy bs
  | BS.null bs = ""
  | otherwise =
      let (ascii, rest) = BS.span (< 0x80) bs
      in map (chr . fromIntegral) (BS.unpack ascii)
          ++ decodeRest rest
  where
    decodeRest r
      | BS.null r = ""
      | otherwise =
          case decodeOne r of
            (c, n) -> c : decodeRest (BS.drop n r)

-- | Decode one UTF-8 sequence (at least one byte consumed), returning
-- the character and the number of bytes consumed.
decodeOne :: BS.ByteString -> (Char, Int)
decodeOne r =
  let b0 = fromIntegral (BS.head r) :: Int
      len = BS.length r
      get i = fromIntegral (BS.index r i) :: Int
      cont i = i < len && 0x80 <= get i && get i < 0xC0
      replacement = ('\xFFFD', 1)
   in if b0 < 0x80
        then (chr b0, 1)
        else
          if b0 < 0xC0 || b0 >= 0xF8 || not (cont 1)
            then replacement
            else
              if b0 < 0xE0
                then
                  let c = (b0 - 0xC0) * 64 + (get 1 - 0x80)
                   in if c < 0x80 then replacement else (chr c, 2)
                else
                  if b0 < 0xF0
                    then
                      if not (cont 2)
                        then replacement
                        else
                          let c = (b0 - 0xE0) * 4096 + (get 1 - 0x80) * 64 + (get 2 - 0x80)
                           in if c < 0x800 || 0xD800 <= c && c <= 0xDFFF
                                then replacement
                                else (chr c, 3)
                    else
                      if not (cont 2 && cont 3)
                        then replacement
                        else
                          let c = (b0 - 0xF0) * 262144 + (get 1 - 0x80) * 4096 + (get 2 - 0x80) * 64 + (get 3 - 0x80)
                           in if c < 0x10000 || c >= 0x110000
                                then replacement
                                else (chr c, 4)

-- | Read the raw bytes of a text-mode handle, inverting MicroHs'
-- UTF-8 transducer decoding (see 'readBytes'):
--
-- * characters below 0x80 are the bytes themselves;
-- * @0xDC80@..@0xDCFF@ are the transducer's own escapes for
--   undecodable bytes;
-- * anything else is UTF-8-encoded back to bytes.
hGetRawBytes :: Handle -> IO BS.ByteString
hGetRawBytes h = do
  str <- hGetContents h
  let bytes = concatMap charToRawBytes str
  length bytes `seq` pure (BS.pack (map fromIntegral bytes))

-- | Map one decoded 'Char' back to the raw bytes it came from (see
-- 'hGetRawBytes').
charToRawBytes :: Char -> [Int]
charToRawBytes c
  | n < 0x80 = [n]
  | 0xDC80 <= n && n <= 0xDCFF = [n - 0xDC00]
  | otherwise = encodeCharUtf8 (if isSurrogate n then 0xFFFD else n)
  where
    n = ord c
    isSurrogate x = 0xD800 <= x && x <= 0xDFFF

-- | UTF-8-encode one 'Char', inverting @getb_utf8@ for every
-- character it can yield.
encodeCharUtf8 :: Int -> [Int]
encodeCharUtf8 n
  | n < 0x800 =
      [ 0xC0 + n `div` 64
      , 0x80 + n `mod` 64
      ]
  | n < 0x10000 =
      [ 0xE0 + n `div` 4096
      , 0x80 + (n `div` 64) `mod` 64
      , 0x80 + n `mod` 64
      ]
  | otherwise =
      [ 0xF0 + n `div` 262144
      , 0x80 + (n `div` 4096) `mod` 64
      , 0x80 + (n `div` 64) `mod` 64
      , 0x80 + n `mod` 64
      ]
#endif

-- | Remove the temporary files of a reaped child.  Pending
-- @(path, handle)@ copies (see 'MhsChild') are performed first, so
-- regular-file redirections land in the destination handle before
-- its temporary file disappears.  A copy failure is reported (never
-- silently swallowed) after the temporary files are still removed.
mhsCleanupChild :: MhsChild -> IO ()
mhsCleanupChild (MhsChild _ _ temps copies) = do
  errs <- mapM (uncurry copyBack) copies
  mapM_ removeQuietly temps
  case [e | Just e <- errs] of
    (e : _) -> ioError e
    [] -> pure ()
  where
    removeQuietly :: FilePath -> IO ()
    removeQuietly fp = catch (removeFile fp) handle
    handle :: IOError -> IO ()
    handle _ = pure ()

-- | Polling reap shared by 'mhsWait', 'mhsTryWait' and 'mhsJoin':
-- wait for the child to exit (or, when @nonBlocking@, take a single
-- probe), caching the exit code.  Never touches temporary files.
mhsPoll :: Bool -> MhsChild -> IO (Maybe ExitCode)
mhsPoll nonBlocking child@(MhsChild _ st _ _) = do
  cached <- readIORef st
  case cached of
    Just ec -> pure (Just ec)
    Nothing -> go
  where
    go = do
      mex <- mhsProbe child
      case mex of
        Just ec -> writeIORef st (Just ec) >> pure (Just ec)
        Nothing
          | nonBlocking -> pure Nothing
          | otherwise -> threadDelay mhsPollMicros >> go

-- | Wait for the child to exit, polling @waitpid@ with @WNOHANG@.
-- The exit code is cached, so repeated calls are cheap.  Temporary
-- files are removed once the child is reaped.
mhsWait :: MhsChild -> IO ExitCode
mhsWait child = do
  Just ec <- mhsPoll False child
  mhsCleanupChild child
  pure ec

-- | Non-blocking exit check.  Caches and cleans up when the child has
-- exited, like 'mhsWait'.
mhsTryWait :: MhsChild -> IO (Maybe ExitCode)
mhsTryWait child = do
  mex <- mhsPoll True child
  case mex of
    Just _ -> mhsCleanupChild child
    Nothing -> pure ()
  pure mex

-- | Wait for the child to exit without touching temporary files.
-- Private capture paths use this to read the child's outputs before
-- 'mhsRelease' removes them.
mhsJoin :: MhsChild -> IO ExitCode
mhsJoin child = do
  Just ec <- mhsPoll False child
  pure ec

-- | Perform pending copies and remove temporary files, like
-- 'mhsCleanupChild'.  A copy failure is reported (never silently
-- swallowed) after the temporary files are still removed; only
-- failures while removing the temporary files themselves are
-- ignored.
mhsRelease :: MhsChild -> IO ()
mhsRelease = mhsCleanupChild

-- | Ask the child to terminate with @SIGTERM@.  Does not wait.
mhsTerminate :: MhsChild -> IO ()
mhsTerminate (MhsChild pid _ _ _) =
  throwErrnoIfMinus1_ "mhsTerminate" (c_mhs_terminate (fromIntegral pid))

-- | Create a fresh empty temporary file, returning its path.  The
-- file exists (so the child can open it without @O_CREAT@ races) and
-- is removed by 'mhsWait' once the owning child is reaped.  It goes
-- through "Distribution.Compat.Internal.TempFile" (atomic
-- @O_CREAT@|@O_EXCL@ creation instead of MicroHs' unhardened
-- check-then-open 'System.IO.openTempFile'), and is binary so piped
-- child output round-trips byte-for-byte.
mhsTempFile :: IO FilePath
mhsTempFile = do
  tmp <- getTemporaryDirectory
  (path, h) <- openBinaryTempFile tmp "cabal-mhs-pipe"
  hClose h
  pure path

-- | Registry remembering which temporary file backs each pipe
-- 'Handle' handed out by 'mhsCreatePipe' or stream setup, so a later
-- @UseHandle@ of such a handle can be redirected to the same file.
-- Entries are never removed; the number of live entries is bounded by
-- the number of pipes created, which is small in practice.
{-# NOINLINE tempRegistry #-}
tempRegistry :: IORef [(Handle, FilePath)]
tempRegistry = unsafePerformIO (newIORef [])

-- | Remember the temporary file backing a pipe 'Handle'.
mhsRegisterTemp :: Handle -> FilePath -> IO ()
mhsRegisterTemp h path = modifyIORef tempRegistry ((h, path) :)

-- | Look up the temporary file backing a 'Handle', if any.
mhsLookupTemp :: Handle -> IO (Maybe FilePath)
mhsLookupTemp h = lookup h <$> readIORef tempRegistry

-- | A 'CreatePipe' replacement for builds without file-descriptor to
-- 'Handle' conversion: a single temporary file with a read 'Handle'
-- and a write 'Handle' opened on it.
--
-- Note the semantic limits of this fallback.  A regular file has no
-- blocking or end-of-stream signalling: a reader that reaches the
-- current end of the file observes end-of-file even when the writer
-- has more to come, so concurrent streaming protocols (writing input
-- while lazily reading output before the child has exited) may observe
-- truncated output.  The reliable pattern is to write the input,
-- close it, wait for the child, and only then read the output, which
-- is exactly how MicroHs' own @System.Process.readProcess@ uses
-- temporary files.
mhsCreatePipe :: IO (Handle, Handle)
mhsCreatePipe = do
  path <- mhsTempFile
  rh <- openFile path ReadMode
  wh <- openFile path WriteMode
  hSetBuffering wh NoBuffering
  mhsRegisterTemp rh path
  mhsRegisterTemp wh path
  pure (rh, wh)
