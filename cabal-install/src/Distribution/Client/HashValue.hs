{-# LANGUAGE CPP #-}

module Distribution.Client.HashValue
  ( HashValue
  , hashValue
  , unHashValue
  , truncateHash
  , showHashValue
  , readFileHashValue
  , hashFromTUF
  ) where

import Control.Monad ((<=<))
import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.ByteString as BS
#ifdef __MHS__
import qualified Distribution.Client.Compat.Base16 as Base16
#else
import qualified Data.ByteString.Base16 as Base16
#endif
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Distribution.Compat.Internal.LazyByteString (hGetContentsLBS)

#ifndef __MHS__
import qualified Hackage.Security.Client as Sec
import qualified Crypto.Hash.SHA256 as SHA256
#endif

import System.IO (IOMode (..), withBinaryFile)
import System.IO.Unsafe (unsafePerformIO)

#ifdef __MHS__
import Foreign.C.Types (CSize (..), CUChar (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)

-- MicroHs cannot use the @cryptohash-sha256@ package, so the C helper
-- @cabal_sha256@ in @cbits/mhs_sha256.c@ is used instead. On Apple
-- platforms that helper uses CommonCrypto's @CC_SHA256@ (in
-- @libSystem@, no extra library needed); elsewhere it uses OpenSSL's
-- @SHA256()@ (needs @-lcrypto@ at link time).
foreign import ccall "mhs_sha256.h cabal_sha256" c_SHA256 :: Ptr CUChar -> CSize -> Ptr CUChar -> IO (Ptr CUChar)
#endif

-----------------------------------------------
-- The specific choice of hash implementation
--

-- Is a crypto hash necessary here? One thing to consider is who controls the
-- inputs and what's the result of a hash collision. Obviously we should not
-- install packages we don't trust because they can run all sorts of code, but
-- if I've checked there's no TH, no custom Setup etc, is there still a
-- problem? If someone provided us a tarball that hashed to the same value as
-- some other package and we installed it, we could end up re-using that
-- installed package in place of another one we wanted. So yes, in general
-- there is some value in preventing intentional hash collisions in installed
-- package ids.

newtype HashValue = HashValue BS.ByteString
  deriving (Eq, Generic, Show)

-- Cannot do any sensible validation here. Although we use SHA256
-- for stuff we hash ourselves, we can also get hashes from TUF
-- and that can in principle use different hash functions in future.
--
-- Therefore, we simply derive this structurally.
instance Binary HashValue
instance Structured HashValue

-- | Hash some data. Currently uses SHA256.
hashValue :: LBS.ByteString -> HashValue
#ifdef __MHS__
hashValue = HashValue . sha256Bytes . BS.concat . LBS.toChunks
#else
hashValue = HashValue . SHA256.hashlazy
#endif

-- | Project a 'HashValue' to its raw digest bytes.
unHashValue :: HashValue -> BS.ByteString
unHashValue (HashValue digest) = digest

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = BSChar8.unpack (Base16.encode digest)

-- | Hash the content of a file. Uses SHA256.
readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
  withBinaryFile tarball ReadMode (evaluate . hashValue <=< hGetContentsLBS)

#ifdef __MHS__
-- | Convert a base16-encoded hash string from TUF metadata into a
-- 'HashValue'.
--
-- Note that TUF hashes don't necessarily have to be SHA256, since it can
-- support new algorithms in future.
hashFromTUF :: String -> HashValue
hashFromTUF hashstr =
  -- TODO: [code cleanup] either we should get TUF to use raw bytestrings or
  -- perhaps we should also just use a base16 string as the internal rep.
  case Base16.decode (BSChar8.pack hashstr) of
    Right hash -> HashValue hash
    Left _ -> error "hashFromTUF: cannot decode base16"
#else
-- | Convert a hash from TUF metadata into a 'PackageSourceHash'.
--
-- Note that TUF hashes don't necessarily have to be SHA256, since it can
-- support new algorithms in future.
hashFromTUF :: Sec.Hash -> HashValue
hashFromTUF (Sec.Hash hashstr) =
  -- TODO: [code cleanup] either we should get TUF to use raw bytestrings or
  -- perhaps we should also just use a base16 string as the internal rep.
  case Base16.decode (BSChar8.pack hashstr) of
    Right hash -> HashValue hash
    Left _ -> error "hashFromTUF: cannot decode base16"
#endif

-- | Truncate a 32 byte SHA256 hash to
--
-- For example 20 bytes render as 40 hex chars, which we use for unit-ids.
-- Or even 4 bytes for 'hashedInstalledPackageIdShort'
truncateHash :: Int -> HashValue -> HashValue
truncateHash n (HashValue h) = HashValue (BS.take n h)

#ifdef __MHS__
-----------------------------------------------
-- SHA256 via statically linked system libcrypto
--
-- MicroHs cannot use the @cryptohash-sha256@ package, so the system
-- libcrypto (@SHA256()@) is used through a foreign import instead.
-- This needs @-lcrypto@ at link time.
--

sha256DigestSize :: Int
sha256DigestSize = 32

-- | One-shot SHA256 over a strict 'BS.ByteString', via the statically
-- linked system libcrypto (see the 'foreign import' above).
sha256Bytes :: BS.ByteString -> BS.ByteString
sha256Bytes input = unsafePerformIO $
  BS.useAsCStringLen input $ \(ptr, len) ->
    allocaBytes sha256DigestSize $ \out -> do
      _ <- c_SHA256 (castPtr ptr) (fromIntegral len) (castPtr out)
      BS.packCStringLen (castPtr out, sha256DigestSize)
#endif
