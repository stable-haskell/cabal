module UnitTests.Distribution.Utils.Structured (tests) where

import Data.Proxy                    (Proxy (..))
import Distribution.Utils.MD5        (md5FromInteger)
import Distribution.Utils.Structured (structureHash, Structured)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.HUnit              (testCase, (@?=), Assertion)

import Distribution.SPDX.License       (License)
import Distribution.Types.VersionRange (VersionRange)

import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.LocalBuildInfo            (LocalBuildInfo)

tests :: TestTree
tests = testGroup "Distribution.Utils.Structured"
    -- This test also verifies that structureHash doesn't loop.
    [ testCase "VersionRange" $
      md5Check (Proxy :: Proxy VersionRange) 0x39396fc4f2d751aaa1f94e6d843f03bd
    , testCase "SPDX.License" $
      md5Check (Proxy :: Proxy License) 0xd3d4a09f517f9f75bc3d16370d5a853a
    -- The difference is in encoding of newtypes
    , testCase "GenericPackageDescription" $ md5CheckGenericPackageDescription (Proxy :: Proxy GenericPackageDescription)
    , testCase "LocalBuildInfo" $ md5CheckLocalBuildInfo (Proxy :: Proxy LocalBuildInfo)
    ]

md5Check :: Structured a => Proxy a -> Integer -> Assertion
md5Check proxy md5Int = structureHash proxy @?= md5FromInteger md5Int

md5CheckGenericPackageDescription :: Proxy GenericPackageDescription -> Assertion
md5CheckGenericPackageDescription proxy = md5Check proxy
    0x77f4c09dc9b1c6967b07516ad35e73af

md5CheckLocalBuildInfo :: Proxy LocalBuildInfo -> Assertion
md5CheckLocalBuildInfo proxy = md5Check proxy
    0x69dab39bf8b56871b28a2c14d423b18a
