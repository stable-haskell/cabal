{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Client.GZipUtils
  ( tests
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

#ifdef __MHS__
import Control.Exception (try)
import qualified Data.List as List
import Data.ByteString.Lazy as BSL (pack)
import Data.ByteString.Lazy.Char8 as BSLL (length, pack, take)
import Data.Either (isLeft)
import Distribution.Client.GZipUtils (maybeDecompress)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "maybeDecompress" maybeDecompressUnitTest
  , -- "decompress plain" property is non-trivial to state in general:
    -- any input starting with the gzip magic bytes is handed to @gzip@,
    -- so we only assert the passthrough for inputs without that prefix.
    testProperty "passthrough plain" prop_maybeDecompress_plain
  ]

maybeDecompressUnitTest :: Assertion
maybeDecompressUnitTest =
  assertBool "decompress plain" (maybeDecompress original == original)
    >> assertBool "decompress gzip" (maybeDecompress compressedGZip == original)
    >> assertBool "non-gzip input is returned untouched" (maybeDecompress compressedZlib == compressedZlib)
    >> (runBrokenStream >>= assertBool "decompress broken stream" . isLeft)
  where
    original = BSLL.pack "original uncompressed input"
    -- @gzip -n@ of "original uncompressed input".
    compressedGZip = BSL.pack compressedGZipBytes
    compressedGZipBytes =
      [ 31
      , 139
      , 8
      , 0
      , 0
      , 0
      , 0
      , 0
      , 0
      , 3
      , 203
      , 47
      , 202
      , 76
      , 207
      , 204
      , 75
      , 204
      , 81
      , 40
      , 205
      , 75
      , 206
      , 207
      , 45
      , 40
      , 74
      , 45
      , 46
      , 78
      , 77
      , 81
      , 200
      , 204
      , 43
      , 40
      , 45
      , 1
      , 0
      , 175
      , 184
      , 107
      , 227
      , 27
      , 0
      , 0
      , 0
      ]
    -- Raw zlib stream for the same input. Since proxy support was removed,
    -- only gzip is decompressed, so this must come back untouched.
    compressedZlib =
      BSL.pack
        [ 120
        , 156
        , 203
        , 47
        , 202
        , 76
        , 207
        , 204
        , 75
        , 204
        , 81
        , 40
        , 205
        , 75
        , 206
        , 207
        , 45
        , 40
        , 74
        , 45
        , 46
        , 78
        , 77
        , 81
        , 200
        , 204
        , 43
        , 40
        , 45
        , 1
        , 0
        , 152
        , 35
        , 10
        , 222
        ]

    -- A truncated gzip stream starts with the magic bytes, so it is handed
    -- to @gzip@, which fails: the pure exception surfaces on evaluation.
    runBrokenStream :: IO (Either SomeException ())
    runBrokenStream = try . void . evaluate . BSLL.length . BSL.pack $ List.take 20 compressedGZipBytes

prop_maybeDecompress_plain :: [Word8] -> Property
prop_maybeDecompress_plain ws =
  BSLL.take 2 bs /= BSL.pack [0x1f, 0x8b] ==> maybeDecompress bs === bs
  where
    bs = BSL.pack ws
#else
import Codec.Compression.GZip as GZip
import Codec.Compression.Zlib as Zlib
import Control.Exception (try)
import Data.ByteString as BS (null)
import Data.ByteString.Lazy as BSL (pack, toChunks)
import Data.ByteString.Lazy.Char8 as BSLL (init, length, pack)
import Data.Either (isLeft)
import Distribution.Client.GZipUtils (maybeDecompress)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "maybeDecompress" maybeDecompressUnitTest
  , -- "decompress plain" property is non-trivial to state,
    -- maybeDecompress returns input bytestring only if error occurs right at the beginning of the decompression process
    -- generating such input would essentially duplicate maybeDecompress implementation
    testProperty "decompress zlib" prop_maybeDecompress_zlib
  , testProperty "decompress gzip" prop_maybeDecompress_gzip
  ]

maybeDecompressUnitTest :: Assertion
maybeDecompressUnitTest =
  assertBool "decompress plain" (maybeDecompress original == original)
    >> assertBool "decompress zlib (with show)" (show (maybeDecompress compressedZlib) == show original)
    >> assertBool "decompress gzip (with show)" (show (maybeDecompress compressedGZip) == show original)
    >> assertBool "decompress zlib" (maybeDecompress compressedZlib == original)
    >> assertBool "decompress gzip" (maybeDecompress compressedGZip == original)
    >> assertBool "have no empty chunks" (all (not . BS.null) . BSL.toChunks . maybeDecompress $ compressedZlib)
    >> (runBrokenStream >>= assertBool "decompress broken stream" . isLeft)
  where
    original = BSLL.pack "original uncompressed input"
    compressedZlib = Zlib.compress original
    compressedGZip = GZip.compress original

    runBrokenStream :: IO (Either SomeException ())
    runBrokenStream = try . void . evaluate . BSLL.length $ maybeDecompress (BSLL.init compressedZlib <> BSLL.pack "*")

prop_maybeDecompress_zlib :: [Word8] -> Property
prop_maybeDecompress_zlib ws = property $ maybeDecompress compressedZlib === original
  where
    original = BSL.pack ws
    compressedZlib = Zlib.compress original

prop_maybeDecompress_gzip :: [Word8] -> Property
prop_maybeDecompress_gzip ws = property $ maybeDecompress compressedGZip === original
  where
    original = BSL.pack ws
    compressedGZip = GZip.compress original
#endif
