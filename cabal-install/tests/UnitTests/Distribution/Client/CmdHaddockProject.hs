module UnitTests.Distribution.Client.CmdHaddockProject (tests) where

import Distribution.Client.CmdHaddockProject (haddockProjectDirectoryUrlPath)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: [TestTree]
tests =
  [ testCase "native Windows separators become URL separators" $
      assertEqual
        "Haddock URL path"
        "a/nested/Nested.html"
        (haddockProjectDirectoryUrlPath "a\\nested\\Nested.html")
  , testCase "POSIX separators remain unchanged" $
      assertEqual
        "Haddock URL path"
        "a/nested/Nested.html"
        (haddockProjectDirectoryUrlPath "a/nested/Nested.html")
  , testCase "mixed separators become URL separators" $
      assertEqual
        "Haddock URL path"
        "a/nested/Nested.html"
        (haddockProjectDirectoryUrlPath "a\\nested/Nested.html")
  ]
