-- | Missing pieces of "Data.Text.Encoding" for MicroHs.
--
-- MicroHs ships "Data.Text.Encoding" ('decodeUtf8', 'encodeUtf8', ...)
-- but not this module. The re-exported 'decodeUtf8With' lives here
-- (rather than in "Data.Text.Encoding", which MicroHs already
-- provides) so that existing qualified imports keep working.
module Data.Text.Encoding.Error
  ( OnDecodeError
  , lenientDecode
  , strictDecode
  , UnicodeException (..)
  , decodeUtf8With
  ) where

import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Word (Word8)
import qualified Data.Text as T
import Data.Bits ((.&.), (.|.), shiftL)

-- | How to handle a decoding error: the name of the calling function,
-- the offending byte if any, and the replacement character ('Just')
-- or failure ('Nothing').
type OnDecodeError = String -> Maybe Word8 -> Maybe Char

-- | Replace malformed input with the replacement character U+FFFD.
lenientDecode :: OnDecodeError
lenientDecode _ _ = Just '\xFFFD'

-- | Fail on malformed input (the decoder throws instead).
strictDecode :: OnDecodeError
strictDecode _ _ = Nothing

-- | Exception type mirroring @text@'s; carried for API compatibility.
-- The decoder below throws a plain 'error' instead, since none of our
-- call sites catch this type.
data UnicodeException
  = DecodeError String (Maybe Word8)
  | EncodeError String (Maybe Char)
  deriving (Eq, Show)

-- | Decode UTF-8, consulting the 'OnDecodeError' handler for
-- malformed sequences: overlongs, surrogates, code points above
-- U+10FFFF, lone continuation bytes and truncated input.
-- Each reported byte advances past one byte and decoding resumes.
decodeUtf8With :: OnDecodeError -> BS.ByteString -> T.Text
decodeUtf8With onErr = T.pack . decode . map fromIntegral . BS.unpack
  where
    decode :: [Int] -> String
    decode [] = []
    decode (b : bs)
      | b < 0x80 = chr b : decode bs
      | b < 0xC2 = bad b bs
      | b < 0xE0 = two b bs
      | b < 0xF0 = three b bs
      | b < 0xF5 = four b bs
      | otherwise = bad b bs
    -- Report one bad byte and resume after it.
    bad :: Int -> [Int] -> String
    bad b bs = case onErr "Data.Text.Encoding.decodeUtf8With" (Just (fromIntegral b)) of
      Just c  -> c : decode bs
      Nothing -> error "Data.Text.Encoding.decodeUtf8With: invalid input"
    isCont b = b >= 0x80 && b < 0xC0
    two b1 bs = case bs of
      (b2 : bs')
        | isCont b2 ->
            chr (((b1 .&. 0x1F) `shiftL` 6) .|. (b2 .&. 0x3F)) : decode bs'
      _ -> bad b1 bs
    three b1 bs = case bs of
      (b2 : b3 : bs')
        | isCont b2 && isCont b3 && ok3 b1 b2 ->
            chr ((((b1 .&. 0x0F) `shiftL` 12) .|. ((b2 .&. 0x3F) `shiftL` 6)) .|. (b3 .&. 0x3F)) : decode bs'
      _ -> bad b1 bs
    four b1 bs = case bs of
      (b2 : b3 : b4 : bs')
        | isCont b2 && isCont b3 && isCont b4 && ok4 b1 b2 ->
            chr (((((b1 .&. 0x07) `shiftL` 18) .|. ((b2 .&. 0x3F) `shiftL` 12)) .|. ((b3 .&. 0x3F) `shiftL` 6)) .|. (b4 .&. 0x3F)) : decode bs'
      _ -> bad b1 bs
    -- Reject overlongs.
    ok3 b1 b2
      | b1 == 0xE0 = b2 >= 0xA0
      -- Reject UTF-16 surrogates.
      | b1 == 0xED = b2 <= 0x9F
      | otherwise  = True
    -- Reject overlongs and code points past U+10FFFF.
    ok4 b1 b2
      | b1 == 0xF0 = b2 >= 0x90
      | b1 == 0xF4 = b2 <= 0x8F
      | otherwise  = True
