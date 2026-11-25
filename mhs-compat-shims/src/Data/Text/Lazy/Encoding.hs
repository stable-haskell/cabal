-- | Lazy 'ByteString' decoding helpers missing from MicroHs.
--
-- MicroHs ships strict "Data.Text.Encoding" ('decodeUtf8',
-- 'encodeUtf8') but no lazy counterpart; the lazy decoder goes
-- through the strict one, which is fine for the small inputs
-- involved (package descriptions and index fragments).
module Data.Text.Lazy.Encoding
  ( decodeUtf8With
  , encodeUtf8
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Lazy as TL

-- | Decode lazy UTF-8, consulting the 'OnDecodeError' handler.
-- Implemented via the strict decoder, which is fine for the small
-- inputs involved (package descriptions and index fragments).
decodeUtf8With :: E.OnDecodeError -> LBS.ByteString -> TL.Text
decodeUtf8With onErr = TL.fromStrict . E.decodeUtf8With onErr . LBS.toStrict

-- | Encode to lazy UTF-8.
encodeUtf8 :: TL.Text -> LBS.ByteString
encodeUtf8 = LBS.fromStrict . TE.encodeUtf8 . TL.toStrict
