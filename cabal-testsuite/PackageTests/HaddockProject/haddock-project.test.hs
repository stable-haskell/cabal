import Control.Monad (forM_)
import Test.Cabal.Prelude
import System.Directory (doesFileExist, removeDirectory)

main = cabalTest . withRepo "repo" $ do
    skipUnlessGhcVersion ">= 9.4.0"
    env <- getTestEnv
    let testDir = testCurrentDir env

    result <- cabal' "haddock-project" ["all"]
    assertOutputDoesNotContain "missing haddocks for some packages from the store" result
    let asyncHaddocks = "haddocks" </> "async" </> "async.haddock"
    liftIO (doesFileExist (testDir </> asyncHaddocks))
      >>= assertBool ("'" ++ asyncHaddocks ++ "'" ++ "should exist")
    forM_ ["haddocks/index.html", "haddocks/doc-index.html"] $ \index ->
      liftIO (doesFileExist (testDir </> index))
        >>= assertBool ("'" ++ index ++ "'" ++ " should exist")
    shouldExist (testDir </> "haddocks" </> "async" </> "Control-Concurrent-Async.html")
    -- Dependency interfaces are hidden from the project index. The reexport
    -- is indexed through MyLib, whose links must resolve to the copied docs.
    assertFileDoesContain (testDir </> "haddocks" </> "doc-index.html")
      "a/MyLib.html#t:Async"
    -- Haddock index URLs must use `/` even when the native path separator is
    -- `\\` (Windows).
    assertFileDoesContain (testDir </> "haddocks" </> "doc-index.html")
      "a/nested/Nested.html"
    assertFileDoesContain (testDir </> "haddocks" </> "doc-index.html")
      "subonly/nested/SubOnly.html"
    assertFileDoesContain (testDir </> "haddocks" </> "a" </> "MyLib.html")
      "../async/Control-Concurrent-Async.html#v:async"
    let nestedHtml = testDir </> "haddocks" </> "a" </> "nested" </> "Nested.html"
    assertFileDoesContain nestedHtml "../../async/Control-Concurrent-Async.html#v:async"
    assertFileDoesContain nestedHtml "../../index.html"
    assertFileDoesContain nestedHtml "../../doc-index.html"
    shouldExist (testDir </> "haddocks" </> "a" </> "nested" </> "nested.haddock")
    shouldExist (testDir </> "haddocks" </> "subonly" </> "nested" </> "nested.haddock")
    shouldNotExist (testDir </> "haddocks" </> "subonly" </> "subonly.haddock")

