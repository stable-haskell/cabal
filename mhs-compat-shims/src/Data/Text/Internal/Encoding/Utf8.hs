-- | Incremental UTF-8 decoder used by the @.cabal@ file lexer.
--
-- MicroHs has no 'Data.Text.Internal' decoder, so this mirrors the
-- interface that @Distribution.Parsec.FieldLineStream@ expects from
-- @text@: a byte-at-a-time state machine. On well-formed input it
-- agrees with 'Data.Text.Encoding.decodeUtf8' exactly; on malformed
-- input it reports 'Reject' (the caller substitutes U+FFFD and moves
-- on) rather than throwing.
--
-- Note: unlike batch decoding, a rejected byte is consumed, not
-- reprocessed, because the caller advances past every byte it feeds
-- in. Well-formed input decodes identically either way.
module Data.Text.Internal.Encoding.Utf8
  ( Decoder (..)
  , State
  , utf8DecodeStart
  , utf8DecodeContinue
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char (chr)
import Data.Word (Word8)

-- | Incremental decoder state: remaining continuation bytes,
-- accumulated value, and the lead byte (needed to reject overlongs,
-- surrogates and out-of-range code points once the second byte
-- arrives).
data State = S !Int !Int !Word8

data Decoder
  = Accept !Char
  | Reject
  | Incomplete !State !Char

-- | Feed the first byte of a sequence. The 'Char' carried by
-- 'Incomplete' is a dummy; the real state lives in 'State'.
utf8DecodeStart :: Word8 -> Decoder
utf8DecodeStart b0
  | b < 0x80 = Accept (chr b)
  | b < 0xC2 = Reject
  | b < 0xE0 = Incomplete (S 1 (b .&. 0x1F) b0) dummy
  | b < 0xF0 = Incomplete (S 2 (b .&. 0x0F) b0) dummy
  | b < 0xF5 = Incomplete (S 3 (b .&. 0x07) b0) dummy
  | otherwise = Reject
  where
    b :: Int
    b = fromIntegral b0
    dummy = '\0'

-- | Feed a continuation byte. Invalid bytes (and overlong, surrogate
-- or out-of-range sequences) yield 'Reject', which tells the caller
-- to emit U+FFFD and continue with the byte after the one just fed.
utf8DecodeContinue :: Word8 -> State -> Char -> Decoder
utf8DecodeContinue w (S n acc lead) _
  | not (isCont b) = Reject
  | badLead        = Reject
  | n' == 0        = Accept (chr acc')
  | otherwise      = Incomplete (S n' acc' lead) dummy
  where
    b :: Int
    b = fromIntegral w
    isCont x = x >= 0x80 && x < 0xC0
    -- Overlong / surrogate / out-of-range checks apply only to the
    -- first continuation byte (which is when the lead byte's
    -- restrictions become testable).
    badLead = n == totalFor lead && case leadI of
      0xE0 -> b < 0xA0
      0xED -> b > 0x9F
      0xF0 -> b < 0x90
      0xF4 -> b > 0x8F
      _    -> False
    leadI = fromIntegral lead :: Int
    totalFor l
      | l < 0xE0  = 1
      | l < 0xF0  = 2
      | otherwise = 3
    acc' = (acc `shiftL` 6) .|. (b .&. 0x3F)
    n' = n - 1
    dummy = '\0'
