import Test.Cabal.Prelude
import System.Directory
import System.FilePath
import Distribution.Simple.Utils hiding (withEnv)
import Distribution.Verbosity

-- Test cabal init when git is not installed
main = do
  skipIfWindows "Might fail on windows."
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "bin" $
    \bin -> cabalTest $
      do
        ghc_path <- programPathM ghcProgram
        cabal_path <- programPathM cabalProgram
        gcc_path <- programPathM gccProgram
        withSymlink ghc_path (bin </> "ghc")
          . withSymlink cabal_path (bin </> "cabal")
          . withSymlink gcc_path (bin </> "gcc")
          . withEnv [("PATH", Just bin)] $ do
            cwd <- fmap testSourceCopyDir getTestEnv

            void . withDirectory cwd $ do
              cabalWithStdin "init" ["-i"]
                "2\n\n5\n\n\n2\n\n\n\n\n\n\n\n\n\n"
