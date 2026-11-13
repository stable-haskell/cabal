{-# LANGUAGE CPP #-}

-- | Lazy-'ByteString' file reads that also work under MicroHs.
--
-- In MicroHs 0.16.12.0, 'Data.ByteString.Lazy.readFile' and
-- 'Data.ByteString.Lazy.hGetContents' read in chunks with
-- 'Data.ByteString.hGetSome', which is unimplemented (@bsUnimp@) and
-- throws @Data.ByteString.hGetSome unimplemented@ at runtime.  The
-- strict 'Data.ByteString' operations use @c_readb@, which works on
-- every handle kind, so under MHS the whole file is read strictly
-- and wrapped with 'BSL.fromStrict'.  Use these two helpers instead
-- of 'BSL.readFile'/'BSL.hGetContents' in any module reachable from
-- the MHS build.
module Distribution.Compat.Internal.LazyByteString
  ( readFileLBS
  , hGetContentsLBS
  ) where

#ifdef __MHS__
import qualified Data.ByteString as BS
#endif
import qualified Data.ByteString.Lazy as BSL
import System.IO (Handle)

-- | Read a whole file into a lazy 'ByteString', strictly under MHS.
readFileLBS :: FilePath -> IO BSL.ByteString
#ifdef __MHS__
-- 'BSL.readFile' needs 'hGetSome', which MicroHs does not implement.
readFileLBS f = BSL.fromStrict <$> BS.readFile f
#else
readFileLBS = BSL.readFile
#endif

-- | Read a handle's remaining contents into a lazy 'ByteString',
-- strictly under MHS.
hGetContentsLBS :: Handle -> IO BSL.ByteString
#ifdef __MHS__
-- 'BSL.hGetContents' needs 'hGetSome', which MicroHs does not
-- implement.
hGetContentsLBS h = BSL.fromStrict <$> BS.hGetContents h
#else
hGetContentsLBS = BSL.hGetContents
#endif
