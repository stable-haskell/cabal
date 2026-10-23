import Control.Monad.IO.Class
import Data.List (isInfixOf)
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath ((</>))
import Test.Cabal.Prelude

main = cabalTest . recordMode DoNotRecord $ do
  gccPath <- programPathM gccProgram
  runAutoreconf
  cabal
    "v2-build"
    [ "--with-gcc=" ++ gccPath
    , "--configure-option=--host=arm-unknown-linux-gnueabihf"
    , "--configure-option=--build=x86_64-unknown-linux-musl"
    ]
  env <- getTestEnv
  recorded <- liftIO $ lines <$> readFile (testTmpDir env </> "cabal-target-triples")
  case recorded of
    [argv, host, build] -> do
      assertEqual "external Setup receives the full Host machine triple" "arm-unknown-linux-gnueabihf" host
      assertEqual "external Setup receives the full Build machine triple" "x86_64-unknown-linux-musl" build
      assertBool "external Setup invocation carries both machine triples" $
        all
          (`isInfixOf` argv)
          [ "--host=arm-unknown-linux-gnueabihf"
          , "--build=x86_64-unknown-linux-musl"
          ]
    _ -> assertFailure $ "external Setup did not record argv, host, and build triples: " ++ show recorded

runAutoreconf =
  if isWindows
    then do
      (mCI, mSh) <- liftIO $ (,) <$> lookupEnv "CI" <*> lookupEnv "SHELL"
      case (mCI, mSh) of
        (Nothing, Nothing) -> skip "Missing $SHELL"
        (Nothing, Just sh) -> do
          env <- getTestEnv
          void $ shell sh ["-l", "-c", "cd $(cygpath -m '" <> testTmpDir env <> "') && autoreconf -i"]
        (Just{}, _) -> do
          env <- getTestEnv
          void $ shell "C:\\msys64\\usr\\bin\\bash.exe" ["-l", "-c", "cd $(cygpath -m '" <> testTmpDir env <> "') && autoreconf -i"]
    else do
      hasAutoreconf <- liftIO $ fmap isJust $ findExecutable "autoreconf"
      skipUnless "no autoreconf" hasAutoreconf
      void $ shell "autoreconf" ["-i"]
