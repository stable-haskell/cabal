-- | Byte-identical reimplementation of @Data.Binary.Put@ from
-- @binary-0.8.9.3@ for MicroHs.
--
-- The 'Put' monad here is a difference list of strict 'ByteString'
-- chunks.  Every primitive below emits exactly the bytes that GHC's
-- @binary@ emits for the same value (big-endian network order for the
-- @be@ variants, little-endian for the @le@ variants).
module Data.Binary.Put
  ( Put
  , PutM
  , runPut
  , runPutM
  , putWord8
  , putWord16be
  , putWord16le
  , putWord32be
  , putWord32le
  , putWord64be
  , putWord64le
  , putInt8
  , putInt16be
  , putInt16le
  , putInt32be
  , putInt32le
  , putInt64be
  , putInt64le
  , putByteString
  , putLazyByteString
  , putCharUtf8
  , putStringUtf8
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | The 'Put' monad: a value-producing builder of strict 'ByteString'
-- chunks, accumulated in a difference list.
newtype PutM a = PutM { unPutM :: [BS.ByteString] -> (a, [BS.ByteString]) }

-- | A 'Put' is a 'PutM' producing no interesting value.
type Put = PutM ()

instance Functor PutM where
  fmap f (PutM m) = PutM (\cs -> let (a, cs') = m cs in (f a, cs'))

instance Applicative PutM where
  pure a = PutM (\cs -> (a, cs))
  PutM f <*> PutM x = PutM (\cs -> let (g, cs1) = f cs
                                       (a, cs2) = x cs1
                                   in (g a, cs2))

instance Monad PutM where
  return = pure
  PutM m >>= k = PutM (\cs -> let (a, cs1) = m cs in unPutM (k a) cs1)

instance Semigroup Put where
  (<>) = (>>)

instance Monoid Put where
  mempty = return ()
  mappend = (<>)

-- | Run a 'PutM', returning the value and the serialised bytes.
runPutM :: PutM a -> (a, LBS.ByteString)
runPutM (PutM m) =
  let (a, chunks) = m []
  in (a, LBS.fromChunks (reverse chunks))

-- | Run a 'Put', returning the serialised bytes.
runPut :: Put -> LBS.ByteString
runPut = snd . runPutM

-- | Emit one strict chunk.
tell :: BS.ByteString -> Put
tell bs = PutM (\cs -> ((), bs : cs))

-- | Write a byte.
putWord8 :: Word8 -> Put
putWord8 w = tell (BS.singleton w)

-- | Write a 16-bit word in big-endian (network) order.
putWord16be :: Word16 -> Put
putWord16be w = tell (BS.pack [fromIntegral (w `shiftR` 8), fromIntegral w])

-- | Write a 16-bit word in little-endian order.
putWord16le :: Word16 -> Put
putWord16le w = tell (BS.pack [fromIntegral w, fromIntegral (w `shiftR` 8)])

-- | Write a 32-bit word in big-endian (network) order.
putWord32be :: Word32 -> Put
putWord32be w =
  tell (BS.pack [ fromIntegral (w `shiftR` 24)
                , fromIntegral (w `shiftR` 16)
                , fromIntegral (w `shiftR` 8)
                , fromIntegral w
                ])

-- | Write a 32-bit word in little-endian order.
putWord32le :: Word32 -> Put
putWord32le w =
  tell (BS.pack [ fromIntegral w
                , fromIntegral (w `shiftR` 8)
                , fromIntegral (w `shiftR` 16)
                , fromIntegral (w `shiftR` 24)
                ])

-- | Write a 64-bit word in big-endian (network) order.
putWord64be :: Word64 -> Put
putWord64be w =
  tell (BS.pack [ fromIntegral (w `shiftR` 56)
                , fromIntegral (w `shiftR` 48)
                , fromIntegral (w `shiftR` 40)
                , fromIntegral (w `shiftR` 32)
                , fromIntegral (w `shiftR` 24)
                , fromIntegral (w `shiftR` 16)
                , fromIntegral (w `shiftR` 8)
                , fromIntegral w
                ])

-- | Write a 64-bit word in little-endian order.
putWord64le :: Word64 -> Put
putWord64le w =
  tell (BS.pack [ fromIntegral w
                , fromIntegral (w `shiftR` 8)
                , fromIntegral (w `shiftR` 16)
                , fromIntegral (w `shiftR` 24)
                , fromIntegral (w `shiftR` 32)
                , fromIntegral (w `shiftR` 40)
                , fromIntegral (w `shiftR` 48)
                , fromIntegral (w `shiftR` 56)
                ])

-- | Write a signed 8-bit value.
putInt8 :: Int8 -> Put
putInt8 i = putWord8 (fromIntegral i)

-- | Write a signed 16-bit value in big-endian order.
putInt16be :: Int16 -> Put
putInt16be i = putWord16be (fromIntegral i)

-- | Write a signed 16-bit value in little-endian order.
putInt16le :: Int16 -> Put
putInt16le i = putWord16le (fromIntegral i)

-- | Write a signed 32-bit value in big-endian order.
putInt32be :: Int32 -> Put
putInt32be i = putWord32be (fromIntegral i)

-- | Write a signed 32-bit value in little-endian order.
putInt32le :: Int32 -> Put
putInt32le i = putWord32le (fromIntegral i)

-- | Write a signed 64-bit value in big-endian order.
putInt64be :: Int64 -> Put
putInt64be i = putWord64be (fromIntegral i)

-- | Write a signed 64-bit value in little-endian order.
putInt64le :: Int64 -> Put
putInt64le i = putWord64le (fromIntegral i)

-- | Write a strict 'ByteString' without a length prefix.
putByteString :: BS.ByteString -> Put
putByteString = tell

-- | Write a lazy 'ByteString' without a length prefix.
putLazyByteString :: LBS.ByteString -> Put
putLazyByteString lbs = PutM (\cs -> ((), foldl (flip (:)) cs (LBS.toChunks lbs)))

-- | Write a 'Char' using UTF-8 encoding, byte-identical to
-- @binary-0.8.9.3@ ('Data.Binary.Builder.putCharUtf8').
putCharUtf8 :: Char -> Put
putCharUtf8 c = tell (BS.pack (encodeChar (ord c)))
  where
    encodeChar :: Int -> [Word8]
    encodeChar x
      | x < 0x80 =
          [ fromIntegral x ]
      | x < 0x800 =
          [ fromIntegral (0xC0 + (x `shiftR` 6))
          , fromIntegral (0x80 + (x .&. 0x3F))
          ]
      | x < 0x10000 =
          [ fromIntegral (0xE0 + (x `shiftR` 12))
          , fromIntegral (0x80 + ((x `shiftR` 6) .&. 0x3F))
          , fromIntegral (0x80 + (x .&. 0x3F))
          ]
      | otherwise =
          [ fromIntegral (0xF0 + (x `shiftR` 18))
          , fromIntegral (0x80 + ((x `shiftR` 12) .&. 0x3F))
          , fromIntegral (0x80 + ((x `shiftR` 6) .&. 0x3F))
          , fromIntegral (0x80 + (x .&. 0x3F))
          ]

-- | Write a 'String' using UTF-8 encoding, byte-identical to
-- @binary-0.8.9.3@ ('Data.Binary.Builder.putStringUtf8').
putStringUtf8 :: String -> Put
putStringUtf8 = mapM_ putCharUtf8
