{-# LANGUAGE CPP #-}

module Distribution.Utils.MD5
  ( MD5
  , Fingerprint (..)
  , showMD5
  , md5
  , fingerprintString

    -- * Helpers
  , md5FromInteger

    -- * Binary
  , binaryPutMD5
  , binaryGetMD5
  ) where

import Data.Binary (Get, Put)
import Data.Binary.Get (getWord64le)
import Data.Binary.Put (putWord64le)
import Data.Bits (complement, shiftR, (.&.))
import Foreign.Ptr (castPtr)
#ifndef __MHS__
import GHC.Fingerprint (Fingerprint (..), fingerprintData, fingerprintString)
#endif
#ifdef __MHS__
import Data.Char (ord)
#endif
import Numeric (showHex)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

#ifdef __MHS__
import Control.Exception (bracket)
import Data.Word (Word8, Word64)
import Distribution.Compat.Directory (getTemporaryDirectory, removeFile)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, poke)
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError)
import System.IO.MD5 (MD5CheckSum, md5File)

-- | MicroHS does not provide 'GHC.Fingerprint', so we define our own
-- fingerprint type with the same two-word layout. The first word holds the
-- first eight digest bytes (big-endian) and the second word the last eight,
-- matching the 'GHC.Fingerprint' order observed through 'showMD5'.
data Fingerprint = Fingerprint !Word64 !Word64 deriving (Eq, Ord)
#endif

type MD5 = Fingerprint

-- | Show 'MD5' in human readable form
--
-- >>> showMD5 (Fingerprint 123 456)
-- "000000000000007b00000000000001c8"
--
-- >>> showMD5 $ md5 $ BS.pack [0..127]
-- "37eff01866ba3f538421b30b7cbefcac"
--
-- @since 3.2.0.0
showMD5 :: MD5 -> String
showMD5 (Fingerprint a b) = pad a' ++ pad b'
  where
    a' = showHex a ""
    b' = showHex b ""
    pad s = replicate (16 - length s) '0' ++ s

-- | @since 3.2.0.0
md5 :: BS.ByteString -> MD5
#ifdef __MHS__
-- MicroHS has no 'GHC.Fingerprint.fingerprintData', so the digest is
-- computed by the @md5Array@ function from the MicroHS runtime through our
-- own 'foreign import'. If hashing the input buffer fails, we fall back to
-- spilling the input to a temporary file and hashing that instead.
md5 bs = unsafeDupablePerformIO $
  md5Buffer bs `catchIOError` \_ -> md5TempFile bs

-- | Hash the input bytes directly with the @md5Array@ runtime function.
foreign import ccall "md5Array" c_md5Array :: Ptr Word8 -> Ptr Word64 -> Int -> IO ()

md5Buffer :: BS.ByteString -> IO MD5
md5Buffer bs =
  BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    allocaArray 2 $ \out -> do
      c_md5Array (castPtr ptr) out len
      fromDigestBytes <$> mapM (peekElemOff (castPtr out)) [0 .. 15]

-- | Fallback: spill the input to a temporary file and hash the file with
-- the runtime's file hashing ('System.IO.MD5.md5File').
md5TempFile :: BS.ByteString -> IO MD5
md5TempFile bs = do
  tmpdir <- getTemporaryDirectory
  bracket
    (openTempFile tmpdir "cabal-md5")
    (\(path, h) -> (hClose h `catchIOError` \_ -> pure ()) >> removeFile path)
    (\(path, h) -> do
        BS.hPut h bs
        hClose h
        mcs <- md5File path
        case mcs of
          Nothing -> ioError (userError "Distribution.Utils.MD5.md5: md5File failed")
          Just cs -> checksumToFingerprint cs)

-- | Convert a runtime checksum to a 'Fingerprint' by reading out its 16
-- digest bytes in order.
checksumToFingerprint :: MD5CheckSum -> IO MD5
checksumToFingerprint cs =
  allocaArray 16 $ \out -> do
    poke (castPtr out) cs
    fromDigestBytes <$> mapM (peekElemOff out) [0 .. 15]

-- | Split 16 digest bytes into two big-endian words, matching the word
-- order 'GHC.Fingerprint' exposes through 'showMD5'.
fromDigestBytes :: [Word8] -> MD5
fromDigestBytes ws =
  case splitAt 8 ws of
    (hi, lo) -> Fingerprint (fromBE hi) (fromBE lo)
  where
    fromBE = foldl (\a b -> a * 256 + fromIntegral b) 0
#else
md5 bs = unsafeDupablePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
  fingerprintData (castPtr ptr) len
#endif

#ifdef __MHS__
-- | Hash a 'String' exactly like 'GHC.Fingerprint.fingerprintString':
-- MD5 over the UTF-32BE encoding of the string, split big-endian
-- into two 'Word64's. Verified byte-identical against GHC output.
fingerprintString :: String -> Fingerprint
fingerprintString = md5 . BS.pack . concatMap w32be
  where
    w32be c = [byte 24, byte 16, byte 8, byte 0]
      where
        o = ord c
        byte s = fromIntegral ((o `shiftR` s) .&. 0xff) :: Word8
#endif

-- | @since 3.2.0.0
binaryPutMD5 :: MD5 -> Put
binaryPutMD5 (Fingerprint a b) = do
  putWord64le a
  putWord64le b

-- | @since 3.2.0.0
binaryGetMD5 :: Get MD5
binaryGetMD5 = Fingerprint <$> getWord64le <*> getWord64le

-- |
--
-- >>> showMD5 $ md5FromInteger 0x37eff01866ba3f538421b30b7cbefcac
-- "37eff01866ba3f538421b30b7cbefcac"
--
-- Note: the input is truncated:
--
-- >>> showMD5 $ md5FromInteger 0x1230000037eff01866ba3f538421b30b7cbefcac
-- "37eff01866ba3f538421b30b7cbefcac"
--
-- Yet, negative numbers are not a problem...
--
-- >>> showMD5 $ md5FromInteger (-1)
-- "ffffffffffffffffffffffffffffffff"
--
-- @since 3.4.0.0
md5FromInteger :: Integer -> MD5
md5FromInteger i = Fingerprint hi lo
  where
    mask = complement 0
    lo = mask .&. fromInteger i
    hi = mask .&. fromInteger (i `shiftR` 64)
