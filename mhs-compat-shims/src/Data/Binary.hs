-- | Byte-identical reimplementation of @Data.Binary@ from
-- @binary-0.8.9.3@ for MicroHs: the 'Binary' class, its base instances
-- and the top-level encode\/decode helpers.
--
-- Wire formats mirror GHC's @binary@ exactly:
--
-- * fixed-width integers are big-endian (network order), except where
--   an @le@ primitive is used;
-- * 'Int' and 'Word' are 8 bytes big-endian;
-- * 'Integer' uses the portable small-tag\/big-unrolled scheme;
-- * 'Char' is UTF-8, 'String' is a length-prefixed UTF-8 sequence;
-- * lists are an 'Int' length prefix followed by the elements;
-- * 'Maybe' and 'Either' use a leading tag byte;
-- * tuples are the concatenation of their components;
-- * strict and lazy 'ByteString' are an 'Int' length prefix followed by
--   the raw bytes;
-- * strict and lazy 'Text' are an 'Int' UTF-8 byte count followed by the
--   raw UTF-8 bytes.
--
-- Deliberate deviations from GHC's @binary@: the generic-derivation
-- support ('GBinaryGet', 'GBinaryPut' and the @DefaultSignatures@
-- defaults) is absent because MicroHs does not implement
-- @DefaultSignatures@; 'Float'\/'Double' instances are absent because
-- MicroHs @base@ does not export @encodeFloat@\/@decodeFloat@.
-- 'runGet', 'runGetOrFail' and 'ByteOffset' are additionally re-exported
-- here for convenience (GHC's @Data.Binary@ does not re-export them).
module Data.Binary
  ( Binary (..)
  , Get
  , Put
  , ByteOffset
  , runGet
  , runGetOrFail
  , putWord8
  , getWord8
  , encode
  , decode
  , decodeOrFail
  , encodeFile
  , decodeFile
  , decodeFileOrFail
  , module Data.Word
  ) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT

import Data.Binary.Get
import Data.Binary.Put

-- | The 'Binary' class provides 'put' and 'get', methods to encode and
-- decode a Haskell value to a lazy 'ByteString'.
class Binary t where
  -- | Encode a value in the 'Put' monad.
  put :: t -> Put
  -- | Decode a value in the 'Get' monad.
  get :: Get t
  -- | Encode a list of values.  The default may be overridden for
  -- efficiency but must keep the same encoding format.
  putList :: [t] -> Put
  putList xs = put (length xs) <> mapM_ put xs

-- | Encode a list of values with an 'Int' length prefix, without
-- blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany n
  | n < 0 = fail ("Data.Binary.getMany: negative length: " ++ show n)
  | otherwise = go [] n
  where
    go acc 0 = return (reverse acc)
    go acc i = do
      x <- get
      go (x : acc) (i - 1)

-- | The () type need never be written to disk.
instance Binary () where
  put () = return ()
  get = return ()

-- | 'Bool's are encoded as a byte in the range 0 .. 1.
instance Binary Bool where
  put = putWord8 . fromIntegral . fromEnum
  get = do
    b <- getWord8
    case b of
      0 -> return False
      1 -> return True
      _ -> fail ("Could not map value " ++ show b ++ " to Bool")

-- | 'Word8's are written as a single byte.
instance Binary Word8 where
  put = putWord8
  get = getWord8

-- | 'Word16's are written as 2 bytes in big-endian (network) order.
instance Binary Word16 where
  put = putWord16be
  get = getWord16be

-- | 'Word32's are written as 4 bytes in big-endian (network) order.
instance Binary Word32 where
  put = putWord32be
  get = getWord32be

-- | 'Word64's are written as 8 bytes in big-endian (network) order.
instance Binary Word64 where
  put = putWord64be
  get = getWord64be

-- | 'Int8's are written as a single byte.
instance Binary Int8 where
  put = putInt8
  get = getInt8

-- | 'Int16's are written as 2 bytes in big-endian order.
instance Binary Int16 where
  put = putInt16be
  get = getInt16be

-- | 'Int32's are written as 4 bytes in big-endian order.
instance Binary Int32 where
  put = putInt32be
  get = getInt32be

-- | 'Int64's are written as 8 bytes in big-endian order.
instance Binary Int64 where
  put = putInt64be
  get = getInt64be

-- | 'Word's are written as 'Word64's, 8 bytes in big-endian order.
instance Binary Word where
  put = putWord64be . fromIntegral
  get = fromIntegral <$> getWord64be

-- | 'Int's are written as 'Int64's, 8 bytes in big-endian order.
instance Binary Int where
  put = putInt64be . fromIntegral
  get = fromIntegral <$> getInt64be

-- Fixed-size type for the small subset of 'Integer'.
type SmallInt = Int32

-- | Portable, efficient serialisation of 'Integer': values fitting in
-- a 'SmallInt' get a zero tag byte plus the value; larger values get a
-- one tag byte, a sign byte and the unrolled base-256 magnitude.
instance Binary Integer where
  put n
    | n >= lo && n <= hi = putWord8 0 <> put (fromIntegral n :: SmallInt)
    | otherwise = putWord8 1 <> put sign <> put (unroll (abs n))
    where
      lo = fromIntegral (minBound :: SmallInt) :: Integer
      hi = fromIntegral (maxBound :: SmallInt) :: Integer
      sign = fromIntegral (signum n) :: Word8
  get = do
    tag <- (get :: Get Word8)
    if tag == 0
      then fromIntegral <$> (get :: Get SmallInt)
      else do
        sign <- (get :: Get Word8)
        bytes <- (get :: Get [Word8])
        let v = roll bytes
        return (if sign == 1 then v else -v)

-- | Fold an 'Integer' to a list of its base-256 digits, most
-- significant first.
unroll :: Integer -> [Word8]
unroll 0 = []
unroll i = let (q, r) = i `divMod` 256 in fromIntegral r : unroll q

-- | Unfold a list of base-256 digits, most significant first, back to
-- an 'Integer'.
roll :: [Word8] -> Integer
roll = foldl unstep 0 . reverse
  where
    unstep a b = a * 256 + fromIntegral b

-- | 'Char' is serialised as UTF-8.
instance Binary Char where
  put = putCharUtf8
  putList str = put (length str) <> putStringUtf8 str
  get = do
    let getByte = fromIntegral <$> (get :: Get Word8) :: Get Int
        shiftL6 = (`shiftL` 6) :: Int -> Int
    w <- getByte
    r <- if w < 0x80
           then return w
           else if w < 0xE0
             then do
               x <- xor 0x80 <$> getByte
               return (x .|. shiftL6 (xor 0xC0 w))
             else if w < 0xF0
               then do
                 x <- xor 0x80 <$> getByte
                 y <- xor 0x80 <$> getByte
                 return (y .|. shiftL6 (x .|. shiftL6 (xor 0xE0 w)))
               else do
                 x <- xor 0x80 <$> getByte
                 y <- xor 0x80 <$> getByte
                 z <- xor 0x80 <$> getByte
                 return (z .|. shiftL6 (y .|. shiftL6 (x .|. shiftL6 (xor 0xF0 w))))
    if r <= 0x10FFFF
      then return (chr r)
      else fail "Not a valid Unicode code point!"

-- | Lists are an 'Int' length prefix followed by the elements.
instance Binary a => Binary [a] where
  put = putList
  get = do
    n <- (get :: Get Int)
    getMany n

-- | 'Maybe': a tag byte (0 for 'Nothing') followed by the value.
instance Binary a => Binary (Maybe a) where
  put Nothing = putWord8 0
  put (Just x) = putWord8 1 <> put x
  get = do
    w <- getWord8
    if w == 0 then return Nothing else Just <$> get

-- | 'Either': a tag byte (0 for 'Left') followed by the value.
instance (Binary a, Binary b) => Binary (Either a b) where
  put (Left a) = putWord8 0 <> put a
  put (Right b) = putWord8 1 <> put b
  get = do
    w <- getWord8
    if w == 0 then Left <$> get else Right <$> get

-- | Tuples concatenate their components.
instance (Binary a, Binary b) => Binary (a, b) where
  put (a, b) = put a <> put b
  get = do
    a <- get
    b <- get
    return (a, b)

instance (Binary a, Binary b, Binary c) => Binary (a, b, c) where
  put (a, b, c) = put a <> put b <> put c
  get = do
    a <- get
    b <- get
    c <- get
    return (a, b, c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a, b, c, d) where
  put (a, b, c, d) = put a <> put b <> put c <> put d
  get = do
    a <- get
    b <- get
    c <- get
    d <- get
    return (a, b, c, d)

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a, b, c, d, e) where
  put (a, b, c, d, e) = put a <> put b <> put c <> put d <> put e
  get = do
    a <- get
    b <- get
    c <- get
    d <- get
    e <- get
    return (a, b, c, d, e)

-- | 'Set's are an 'Int' size prefix followed by the ascending elements.
-- Matches GHC's @binary@ ('Set.size', 'Set.toAscList',
-- 'Set.fromDistinctAscList').
instance Binary a => Binary (Set.Set a) where
  put s = put (Set.size s) <> mapM_ put (Set.toAscList s)
  get = do
    xs <- get
    return (Set.fromDistinctAscList xs)

-- | 'Map's are an 'Int' size prefix followed by the ascending entries.
-- Matches GHC's @binary@ ('Map.size', 'Map.toAscList',
-- 'Map.fromDistinctAscList').
instance (Binary k, Binary v) => Binary (Map.Map k v) where
  put m = put (Map.size m) <> mapM_ put (Map.toAscList m)
  get = do
    xs <- get
    return (Map.fromDistinctAscList xs)

-- | Strict 'ByteString's are an 'Int' length prefix plus the raw bytes.
instance Binary BS.ByteString where
  put bs = put (BS.length bs) <> putByteString bs
  get = do
    n <- (get :: Get Int)
    getByteString n

-- | Lazy 'ByteString's are an 'Int' length prefix plus the raw bytes.
instance Binary LBS.ByteString where
  put bs = put (fromIntegral (LBS.length bs) :: Int) <> putLazyByteString bs
  get = do
    n <- (get :: Get Int)
    getLazyByteString (fromIntegral n)

-- | Strict 'Text' is an 'Int' UTF-8 byte count plus the raw UTF-8
-- bytes, exactly like @text@'s 'Binary' instance.
instance Binary T.Text where
  put t = put (TE.encodeUtf8 t)
  get = do
    bs <- (get :: Get BS.ByteString)
    let t = TE.decodeUtf8 bs
    -- Reject non-canonical input the way @text@\'s decoder does: valid
    -- UTF-8 re-encodes to itself.
    if TE.encodeUtf8 t == bs
      then return t
      else fail ("Data.Binary.Text: cannot decode byte sequence")

-- | Lazy 'Text' is an 'Int' UTF-8 byte count plus the raw UTF-8 bytes,
-- exactly like @text@'s lazy 'Binary' instance.
instance Binary LT.Text where
  put t = put (LBS.fromChunks (map TE.encodeUtf8 (LT.toChunks t)))
  get = do
    bs <- (get :: Get LBS.ByteString)
    let strict = LBS.toStrict bs
        t = TE.decodeUtf8 strict
    if TE.encodeUtf8 t == strict
      then return (LT.fromStrict t)
      else fail ("Data.Binary.Text: cannot decode byte sequence")

-- | Encode a value to a lazy 'ByteString'.
encode :: Binary a => a -> LBS.ByteString
encode = runPut . put

-- | Decode a value from a lazy 'ByteString', calling 'error' on
-- invalid input.
decode :: Binary a => LBS.ByteString -> a
decode = runGet get

-- | Decode a value, returning 'Left' on failure with the unconsumed
-- input, the byte offset and a message.
decodeOrFail :: Binary a => LBS.ByteString -> Either (LBS.ByteString, ByteOffset, String) (LBS.ByteString, ByteOffset, a)
decodeOrFail = runGetOrFail get

-- | Lazily serialise a value to a file.
encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f v = LBS.writeFile f (encode v)

-- | Decode a value from a file, calling 'error' on failure.
decodeFile :: Binary a => FilePath -> IO a
decodeFile f = do
  result <- decodeFileOrFail f
  case result of
    Right x -> return x
    Left (_, msg) -> error msg

-- | Decode a value from a file, returning the byte offset and message
-- on failure.
--
-- Note: MicroHs does not implement lazy 'LBS.readFile' (it raises
-- @Data.ByteString.hGetSome unimplemented@), so the file is read
-- strictly and converted.  The decoded value is identical; only the
-- incremental\/lazy behaviour differs.
decodeFileOrFail :: Binary a => FilePath -> IO (Either (ByteOffset, String) a)
decodeFileOrFail f = do
  bs <- BS.readFile f
  case runGetOrFail get (LBS.fromStrict bs) of
    Left (_, pos, msg) -> return (Left (pos, msg))
    Right (_, _, a) -> return (Right a)

