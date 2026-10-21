import Control.Monad (forM_)
import Test.Cabal.Prelude
import System.Directory (doesFileExist, removeDirectory)

main = cabalTest . withRepo "repo" $ do
    skipUnlessGhcVersion ">= 9.4.0"
    env <- getTestEnv
    let testDir = testCurrentDir env

    cabal "haddock-project" ["all"]
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
    assertFileDoesContain (testDir </> "haddocks" </> "a" </> "MyLib.html")
      "../async/Control-Concurrent-Async.html#v:async"

