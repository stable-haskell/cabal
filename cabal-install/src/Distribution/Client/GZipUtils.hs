{-# LANGUAGE CPP #-}
-- |
-- Module      :  Distribution.Client.GZipUtils
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides a convenience functions for working with files that may or may not
-- be zipped.
module Distribution.Client.GZipUtils
  ( maybeDecompress
#ifdef __MHS__
  , gzipCompress
#endif
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

#ifdef __MHS__
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Compat.Process as Process
import Distribution.Compat.Directory
  ( getTemporaryDirectory
  , removeFile
  )
import Distribution.Compat.Internal.TempFile (openBinaryTempFile)
import System.IO
  ( hClose
  )
import System.IO.Error (userError)
import System.IO.Unsafe (unsafePerformIO)

import Control.Exception (bracket)
#else
import Codec.Compression.Zlib.Internal
import Control.Exception (throw)
import Control.Monad.ST.Lazy (ST, runST)
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy.Internal as BS (ByteString (Chunk, Empty))
#endif

#ifdef __MHS__
-- | Attempts to decompress the @bytes@ when they start with the gzip
-- magic bytes (@1F 8B@), passing them through the external @gzip -dc@
-- program; anything else is returned as-is. Corrupt gzip input raises
-- an 'IOException'. Callers should make sanity checks to verify that
-- a pass-through result is not, in fact, garbage.
--
-- This used to also handle raw zlib streams (to deal with http proxies
-- that lie to us and transparently decompress without removing the
-- content-encoding header, see
-- <https://github.com/haskell/cabal/issues/678>), but proxy support has
-- been removed and only gzip is used for index files and tarballs, so
-- non-gzip input is now returned untouched.
maybeDecompress :: ByteString -> ByteString
maybeDecompress bs
  | hasGzipMagic bs = unsafePerformIO (gunzip bs)
  | otherwise = bs

hasGzipMagic :: ByteString -> Bool
hasGzipMagic bs = BS.take 2 bs == BS.pack [0x1f, 0x8b]

-- | Decompress gzip data using the external @gzip -dc@ program.
--
-- The input is staged through a temporary file so that the child only
-- ever reads from the file and writes to its stdout pipe; this avoids
-- any stdin/stdout pipe deadlock. Corrupt input (or a missing @gzip@
-- program) raises an 'IOException'. The staging handle is binary:
-- 'BS.hPut' on a text-mode handle would re-encode every byte
-- @0x80..0xFF@ as two UTF-8 bytes.
gunzip :: ByteString -> IO ByteString
gunzip input =
  bracket
    ( do
        tmpDir <- getTemporaryDirectory
        (path, h) <- openBinaryTempFile tmpDir "cabal-gunzip.gz"
        BS.hPut h input
        hClose h
        return path
    )
    (\path -> removeFile path `catchIO` \_ -> return ())
    runGzip

-- | Compress bytes with the external @gzip -n -c@ program (no embedded
-- file name or timestamp, so the output is deterministic).
gzipCompress :: ByteString -> IO ByteString
gzipCompress input =
  bracket
    ( do
        tmpDir <- getTemporaryDirectory
        (path, h) <- openBinaryTempFile tmpDir "cabal-gzip.tar"
        BS.hPut h input
        hClose h
        return path
    )
    (\path -> removeFile path `catchIO` \_ -> return ())
    runGzipCompress

runGzipCompress :: FilePath -> IO ByteString
runGzipCompress path = do
  -- Capture discipline: the child is reaped before its outputs are
  -- read, so temporary-file redirection is complete by construction.
  -- Outputs are read with strict 'StrictBS.readFile' (raw 'c_readb'):
  -- lazy 'BS.readFile' needs 'hGetSome', which MicroHs does not
  -- implement.
  (code, captured) <-
    Process.captureProcess
      (Process.proc "gzip" ["-n", "-c", path])
      Process.NoInput
  out <- BS.fromStrict <$> StrictBS.readFile (Process.capturedOutPath captured)
  _ <- evaluate (BS.length out)
  err <- readFile (Process.capturedErrPath captured)
  _ <- evaluate (length err)
  Process.capturedRelease captured
  case code of
    ExitSuccess -> return out
    ExitFailure n ->
      throwIO $
        userError $
          "gzip -n -c failed with exit code "
            ++ show n
            ++ (if null err then "" else ": " ++ err)

runGzip :: FilePath -> IO ByteString
runGzip path = do
  -- Capture discipline: the child is reaped before its outputs are
  -- read, so temporary-file redirection is complete by construction.
  -- Outputs are read with strict 'StrictBS.readFile' (raw 'c_readb'):
  -- lazy 'BS.readFile' needs 'hGetSome', which MicroHs does not
  -- implement.
  (code, captured) <-
    Process.captureProcess
      (Process.proc "gzip" ["-dc", path])
      Process.NoInput
  out <- BS.fromStrict <$> StrictBS.readFile (Process.capturedOutPath captured)
  _ <- evaluate (BS.length out)
  err <- readFile (Process.capturedErrPath captured)
  _ <- evaluate (length err)
  Process.capturedRelease captured
  case code of
    ExitSuccess -> return out
    ExitFailure n ->
      throwIO $
        userError $
          "gzip -dc failed with exit code "
            ++ show n
            ++ (if null err then "" else ": " ++ err)

#else

-- | Attempts to decompress the `bytes' under the assumption that
-- "data format" error at the very beginning of the stream means
-- that it is already decompressed. Caller should make sanity checks
-- to verify that it is not, in fact, garbage.
--
-- This is to deal with http proxies that lie to us and transparently
-- decompress without removing the content-encoding header. See:
-- <https://github.com/haskell/cabal/issues/678>
maybeDecompress :: ByteString -> ByteString
maybeDecompress bytes = runST (go bytes decompressor)
  where
    decompressor :: DecompressStream (ST s)
    decompressor = decompressST gzipOrZlibFormat defaultDecompressParams

    -- DataError at the beginning of the stream probably means that stream is
    -- not compressed, so we return it as-is.
    -- TODO: alternatively, we might consider looking for the two magic bytes 1F 8B
    -- (https://en.wikipedia.org/wiki/Gzip#File_structure)
    -- at the beginning of the gzip header.  (not an option for zlib, though.)
    go :: Monad m => ByteString -> DecompressStream m -> m ByteString
    go cs (DecompressOutputAvailable bs k) = Chunk bs <$> (go' cs =<< k)
    go _ (DecompressStreamEnd _bs) = return Empty
    go _ (DecompressStreamError _err) = return bytes
    go cs (DecompressInputRequired k) = go cs' =<< k c
      where
        (c, cs') = uncons cs

    -- Once we have received any output though we regard errors as actual errors
    -- and we throw them (as pure exceptions).
    -- TODO: We could (and should) avoid these pure exceptions.
    go' :: Monad m => ByteString -> DecompressStream m -> m ByteString
    go' cs (DecompressOutputAvailable bs k) = Chunk bs <$> (go' cs =<< k)
    go' _ (DecompressStreamEnd _bs) = return Empty
    go' _ (DecompressStreamError err) = throw err
    go' cs (DecompressInputRequired k) = go' cs' =<< k c
      where
        (c, cs') = uncons cs

    uncons :: ByteString -> (Strict.ByteString, ByteString)
    uncons Empty = (Strict.empty, Empty)
    uncons (Chunk c cs) = (c, cs)
#endif
