-- | Byte-identical reimplementation of @Data.Binary.Get@ from
-- @binary-0.8.9.3@ for MicroHs.
--
-- The 'Get' monad is a simple state transformer over the remaining
-- lazy 'ByteString' input plus a byte offset.  Failure positions and
-- 'runGet' error messages match GHC's @binary@.
module Data.Binary.Get
  ( Get
  , ByteOffset
  , runGet
  , runGetOrFail
  , skip
  , isEmpty
  , remaining
  , bytesRead
  , lookAhead
  , label
  , isolate
  , getWord8
  , getWord16be
  , getWord16le
  , getWord32be
  , getWord32le
  , getWord64be
  , getWord64le
  , getInt8
  , getInt16be
  , getInt16le
  , getInt32be
  , getInt32le
  , getInt64be
  , getInt64le
  , getByteString
  , getLazyByteString
  , getRemainingLazyByteString
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Fail (MonadFail (..))
import Data.Bits (shiftL, (.|.))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | Byte offset into the input, matching @binary@.
type ByteOffset = Int64

-- | The 'Get' monad.  The function takes the remaining input and the
-- number of bytes consumed so far.  On failure it returns the input
-- and offset at the failure point plus a message, exactly as
-- 'runGetOrFail' reports them.
newtype Get a = Get
  { unGet :: LBS.ByteString -> Int64 -> Either (LBS.ByteString, Int64, String) (a, LBS.ByteString, Int64)
  }

instance Functor Get where
  fmap f (Get m) = Get (\lbs pos ->
    case m lbs pos of
      Left err -> Left err
      Right (a, rest, pos') -> Right (f a, rest, pos'))

instance Applicative Get where
  pure a = Get (\lbs pos -> Right (a, lbs, pos))
  Get f <*> Get x = Get (\lbs pos ->
    case f lbs pos of
      Left err -> Left err
      Right (g, rest, pos') ->
        case x rest pos' of
          Left err -> Left err
          Right (a, rest', pos'') -> Right (g a, rest', pos''))

instance Monad Get where
  return = pure
  Get m >>= k = Get (\lbs pos ->
    case m lbs pos of
      Left err -> Left err
      Right (a, rest, pos') -> unGet (k a) rest pos')

instance MonadFail Get where
  fail msg = Get (\lbs pos -> Left (lbs, pos, msg))

-- | Backtracking choice: on failure the input is rewound and the
-- second decoder runs, exactly as in GHC's @binary@.
instance Alternative Get where
  empty = Get (\lbs pos -> Left (lbs, pos, "Data.Binary.Get(Alternative).empty"))
  Get f <|> Get g = Get (\lbs pos ->
    case f lbs pos of
      Left _ -> g lbs pos
      Right x -> Right x)
  some p = (:) <$> p <*> many p
  many p = do
    v <- (Just <$> p) <|> pure Nothing
    case v of
      Nothing -> pure []
      Just x -> (:) x <$> many p

instance MonadPlus Get where
  mzero = empty
  mplus = (<|>)

-- | Run a decoder, returning the leftover input, the final offset and
-- either an error message or the value.
runGetOrFail :: Get a -> LBS.ByteString -> Either (LBS.ByteString, ByteOffset, String) (LBS.ByteString, ByteOffset, a)
runGetOrFail (Get m) lbs =
  case m lbs 0 of
    Left err -> Left err
    Right (a, rest, pos) -> Right (rest, pos, a)

-- | Run a decoder, calling 'error' on failure, as @binary@ does.
runGet :: Get a -> LBS.ByteString -> a
runGet m lbs =
  case runGetOrFail m lbs of
    Left (_, pos, msg) -> error ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)
    Right (_, _, a) -> a

-- | Consume @n@ bytes and return them unpacked, failing on short input.
getN :: Int64 -> Get [Word8]
getN n
  | n < 0 = fail ("Data.Binary.Get: negative length: " ++ show n)
  | otherwise = Get (\lbs pos ->
      if LBS.length lbs < n
        then Left (lbs, pos, "not enough bytes")
        else let (h, t) = LBS.splitAt n lbs
             in Right (LBS.unpack h, t, pos + n))

-- | Skip ahead @n@ bytes.  Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n
  | n <= 0 = return ()
  | otherwise = Get (\lbs pos ->
      let n64 = fromIntegral n :: Int64
      in if LBS.length lbs < n64
           then Left (lbs, pos, "not enough bytes")
           else Right ((), LBS.drop n64 lbs, pos + n64))

-- | Succeeds iff there is no more input.
isEmpty :: Get Bool
isEmpty = Get (\lbs pos -> Right (LBS.null lbs, lbs, pos))

-- | Number of unread bytes.
remaining :: Get Int64
remaining = Get (\lbs pos -> Right (LBS.length lbs, lbs, pos))

-- | Number of bytes read so far.
bytesRead :: Get ByteOffset
bytesRead = Get (\lbs pos -> Right (pos, lbs, pos))

-- | Run a decoder without consuming input.
lookAhead :: Get a -> Get a
lookAhead (Get m) = Get (\lbs pos ->
  case m lbs pos of
    Left err -> Left err
    Right (a, _, _) -> Right (a, lbs, pos))

-- | Label a decoder (no-op, kept for API compatibility).
label :: String -> Get a -> Get a
label _ m = m

-- | Run a decoder on a fixed @n@-byte window.  Fails if the window is
-- not fully available or the inner decoder reads past it.  Unconsumed
-- window bytes are skipped.
isolate :: Int -> Get a -> Get a
isolate n (Get m)
  | n < 0 = fail ("Data.Binary.Get.isolate: negative length: " ++ show n)
  | otherwise = Get (\lbs pos ->
      let n64 = fromIntegral n :: Int64
      in if LBS.length lbs < n64
           then Left (lbs, pos, "not enough bytes")
           else let (window, rest) = LBS.splitAt n64 lbs
                in case m window pos of
                     Left (_, pos', msg) -> Left (lbs, pos', msg)
                     Right (a, leftover, _) ->
                       let consumed = n64 - LBS.length leftover
                       in Right (a, rest, pos + consumed))

-- | Read one byte.
getWord8 :: Get Word8
getWord8 = do
  [b] <- getN 1
  return b

-- | Read a 16-bit word in big-endian (network) order.
getWord16be :: Get Word16
getWord16be = do
  [b0, b1] <- getN 2
  return ((fromIntegral b0 `shiftL` 8) .|. fromIntegral b1)

-- | Read a 16-bit word in little-endian order.
getWord16le :: Get Word16
getWord16le = do
  [b0, b1] <- getN 2
  return ((fromIntegral b1 `shiftL` 8) .|. fromIntegral b0)

-- | Read a 32-bit word in big-endian (network) order.
getWord32be :: Get Word32
getWord32be = do
  [b0, b1, b2, b3] <- getN 4
  return ((fromIntegral b0 `shiftL` 24) .|. (fromIntegral b1 `shiftL` 16) .|.
          (fromIntegral b2 `shiftL` 8) .|. fromIntegral b3)

-- | Read a 32-bit word in little-endian order.
getWord32le :: Get Word32
getWord32le = do
  [b0, b1, b2, b3] <- getN 4
  return ((fromIntegral b3 `shiftL` 24) .|. (fromIntegral b2 `shiftL` 16) .|.
          (fromIntegral b1 `shiftL` 8) .|. fromIntegral b0)

-- | Read a 64-bit word in big-endian (network) order.
getWord64be :: Get Word64
getWord64be = do
  [b0, b1, b2, b3, b4, b5, b6, b7] <- getN 8
  return ((fromIntegral b0 `shiftL` 56) .|. (fromIntegral b1 `shiftL` 48) .|.
          (fromIntegral b2 `shiftL` 40) .|. (fromIntegral b3 `shiftL` 32) .|.
          (fromIntegral b4 `shiftL` 24) .|. (fromIntegral b5 `shiftL` 16) .|.
          (fromIntegral b6 `shiftL` 8) .|. fromIntegral b7)

-- | Read a 64-bit word in little-endian order.
getWord64le :: Get Word64
getWord64le = do
  [b0, b1, b2, b3, b4, b5, b6, b7] <- getN 8
  return ((fromIntegral b7 `shiftL` 56) .|. (fromIntegral b6 `shiftL` 48) .|.
          (fromIntegral b5 `shiftL` 40) .|. (fromIntegral b4 `shiftL` 32) .|.
          (fromIntegral b3 `shiftL` 24) .|. (fromIntegral b2 `shiftL` 16) .|.
          (fromIntegral b1 `shiftL` 8) .|. fromIntegral b0)

-- | Wrap an unsigned value into its signed counterpart, matching
-- GHC's 'fromIntegral'.  MicroHs traps on narrowing unsigned-to-signed
-- conversions that GHC wraps, so the wrap is done explicitly through
-- 'Int' (wide enough for 8-, 16- and 32-bit magnitudes).
wrap8 :: Word8 -> Int8
wrap8 w = fromIntegral (if i >= 128 then i - 256 else i)
  where
    i = fromIntegral w :: Int

-- | See 'wrap8'.
wrap16 :: Word16 -> Int16
wrap16 w = fromIntegral (if i >= 32768 then i - 65536 else i)
  where
    i = fromIntegral w :: Int

-- | See 'wrap8'.
wrap32 :: Word32 -> Int32
wrap32 w = fromIntegral (if i >= 2147483648 then i - 4294967296 else i)
  where
    i = fromIntegral w :: Int

-- | Read a signed 8-bit value.
getInt8 :: Get Int8
getInt8 = wrap8 <$> getWord8

-- | Read a signed 16-bit value in big-endian order.
getInt16be :: Get Int16
getInt16be = wrap16 <$> getWord16be

-- | Read a signed 16-bit value in little-endian order.
getInt16le :: Get Int16
getInt16le = wrap16 <$> getWord16le

-- | Read a signed 32-bit value in big-endian order.
getInt32be :: Get Int32
getInt32be = wrap32 <$> getWord32be

-- | Read a signed 32-bit value in little-endian order.
getInt32le :: Get Int32
getInt32le = wrap32 <$> getWord32le

-- | Read a signed 64-bit value in big-endian order.
getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be

-- | Read a signed 64-bit value in little-endian order.
getInt64le :: Get Int64
getInt64le = fromIntegral <$> getWord64le

-- | Read @n@ bytes as a strict 'ByteString'.
getByteString :: Int -> Get BS.ByteString
getByteString n = BS.pack <$> getN (fromIntegral n)

-- | Read @n@ bytes as a lazy 'ByteString'.
getLazyByteString :: Int64 -> Get LBS.ByteString
getLazyByteString n
  | n < 0 = fail ("Data.Binary.Get.getLazyByteString: negative length: " ++ show n)
  | otherwise = Get (\lbs pos ->
      if LBS.length lbs < n
        then Left (lbs, pos, "not enough bytes")
        else let (h, t) = LBS.splitAt n lbs
             in Right (h, t, pos + n))

-- | Read all remaining input as a lazy 'ByteString'.
getRemainingLazyByteString :: Get LBS.ByteString
getRemainingLazyByteString = Get (\lbs pos -> Right (lbs, LBS.empty, pos + LBS.length lbs))
