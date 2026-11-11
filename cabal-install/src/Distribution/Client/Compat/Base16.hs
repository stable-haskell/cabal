{-# LANGUAGE CPP #-}

-- | Minimal base-16 (hex) codec, mirroring the API of
-- @Data.ByteString.Base16@ ('encode' and 'decode') that is used
-- elsewhere in @cabal-install@.
--
-- MicroHs has no @base16@ package, so this module is used instead of
-- 'Data.ByteString.Base16' under @__MHS__@ (see the import sites).
-- Behaviour matches the real package on valid input: lowercase hex
-- output, and decoding accepts upper- and lower-case digits.
module Distribution.Client.Compat.Base16
  ( encode
  , decode
  ) where

import qualified Data.ByteString as BS
import Data.Word (Word8)

-- | Encode bytes as lowercase hex.
encode :: BS.ByteString -> BS.ByteString
encode = BS.pack . concatMap byteHex . BS.unpack
  where
    byteHex :: Word8 -> [Word8]
    byteHex w = [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
    hexDigit :: Word8 -> Word8
    hexDigit n = fromIntegral (fromEnum c)
      where
        c = "0123456789abcdef" !! fromIntegral n
-- | Decode hex (upper- or lower-case) to bytes.
decode :: BS.ByteString -> Either String BS.ByteString
decode bs
  | odd (BS.length bs) = Left "Base16.decode: odd-length input"
  | otherwise = BS.pack <$> traverse hexPair (pairs (BS.unpack bs))
  where
    pairs :: [Word8] -> [(Word8, Word8)]
    pairs [] = []
    pairs (h : l : t) = (h, l) : pairs t
    pairs [_] = [] -- unreachable: the length check above rules this out
    hexPair :: (Word8, Word8) -> Either String Word8
    hexPair (h, l) = do
      hi <- hexVal h
      lo <- hexVal l
      return (hi * 16 + lo)
    hexVal :: Word8 -> Either String Word8
    hexVal c
      | c >= 48 && c <= 57 = Right (c - 48) -- '0'-'9'
      | c >= 97 && c <= 102 = Right (c - 87) -- 'a'-'f'
      | c >= 65 && c <= 70 = Right (c - 55) -- 'A'-'F'
      | otherwise = Left ("Base16.decode: invalid hex digit: " ++ show c)
