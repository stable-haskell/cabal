import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List (isInfixOf)
import Data.Maybe
import Distribution.Simple.Setup (configureTargetDefaults)
import System.Directory
import System.Environment
import System.FilePath ((</>))
import Test.Cabal.Prelude

main = setupTest . recordMode DoNotRecord $ do
  gccPath <- programPathM gccProgram
  env <- getTestEnv
  originalConfigure <- liftIO $ readFile (testTmpDir env </> "configure.ac")
  _ <- liftIO $ evaluate (length originalConfigure)
  runAutoconfAliasProbe gccPath
  writeSourceFile "configure.ac" originalConfigure
  runAutoreconf
  let options = configureOptions gccPath
  setup "configure" options
  assertConfigureTargets

  setup "clean" []
  writeSourceFile "Setup.hs" $
    unlines
      [ "import Distribution.Simple"
      , "main = defaultMainWithSetupHooks autoconfSetupHooks"
      ]
  setup "configure" options
  assertConfigureTargets

  setup "clean" []
  profiledTarget <-
    fails $
      setup'
        "configure"
        [ "--with-gcc=" ++ gccPath
        , "--configure-option=--host=arm-unknown-linux-gnueabihf-prof"
        , "--configure-option=--host=arm-unknown-linux-gnueabihf"
        , "--configure-option=--build=x86_64-unknown-linux-musl"
        ]
  assertOutputContains "Autoconf --host must name the machine target platform" profiledTarget

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

configureOptions gccPath =
  [ "--with-gcc=" ++ gccPath
  , "--configure-option=--host=arm-unknown-linux-gnueabihf"
  , "--configure-option=--build=x86_64-unknown-linux-musl"
  ]

assertConfigureTargets = do
  env <- getTestEnv
  recorded <- liftIO $ lines <$> readFile (testTmpDir env </> "cabal-target-triples")
  case recorded of
    [argv, host, build] -> do
      assertEqual "Autoconf receives the full Host machine triple" "arm-unknown-linux-gnueabihf" host
      assertEqual "Autoconf receives the full Build machine triple" "x86_64-unknown-linux-musl" build
      assertBool "configure invocation carries both machine triples" $
        all
          (`isInfixOf` argv)
          [ "--host=arm-unknown-linux-gnueabihf"
          , "--build=x86_64-unknown-linux-musl"
          ]
    _ -> assertFailure $ "configure did not record argv, host, and build triples: " ++ show recorded

runAutoconfAliasProbe gccPath = do
  writeSourceFile "configure.ac" $
    unlines
      [ "AC_INIT([target alias probe], [1.0])"
      , "AC_PROG_CC"
      , "AC_RUN_IFELSE([AC_LANG_PROGRAM([], [])], [echo native > target-probe], [echo failed > target-probe], [echo cross > target-probe])"
      , "AC_CONFIG_FILES([zlib.buildinfo])"
      , "AC_CONFIG_COMMANDS([cabal-target-triples], [printf '%s\\n' \"$ac_configure_args\" \"$host_alias\" \"$build_alias\" > \"$srcdir/cabal-target-triples\"], [ac_configure_args=\"$ac_configure_args\"; host_alias=\"$host_alias\"; build_alias=\"$build_alias\"])"
      , "BUILD_PACKAGE_BOOL=True"
      , "AC_SUBST([BUILD_PACKAGE_BOOL])"
      , "AC_OUTPUT"
      ]
  runAutoreconf
  env <- getTestEnv
  configSubExists <- liftIO $ doesFileExist (testTmpDir env </> "config.sub")
  assertBool "raw alias test must not use config.sub" (not configSubExists)
  let inferred = configureTargetDefaults "x86_64-unknown-linux" "x86_64-unknown-linux-gnu" []
      explicitHost = ["--host=x86_64-unknown-linux-gnu"]
      explicitBuild = ["--build=x86_64-unknown-linux"]
      arguments args = ("--with-gcc=" ++ gccPath) : map ("--configure-option=" ++) args
  probe (arguments inferred) "x86_64-unknown-linux-gnu" "x86_64-unknown-linux-gnu" "native"
  probe
    (arguments (explicitHost ++ configureTargetDefaults "x86_64-unknown-linux-gnu" "x86_64-unknown-linux" explicitHost))
    "x86_64-unknown-linux-gnu"
    "x86_64-unknown-linux-gnu"
    "native"
  setup "clean" []
  probe
    (arguments (explicitBuild ++ configureTargetDefaults "x86_64-unknown-linux-gnu" "x86_64-unknown-linux" explicitBuild))
    "x86_64-unknown-linux"
    "x86_64-unknown-linux"
    "native"
  setup "clean" []
  probe
    (arguments ["--host=x86_64-unknown-linux-gnu", "--build=x86_64-unknown-linux"])
    "x86_64-unknown-linux-gnu"
    "x86_64-unknown-linux"
    "cross"
  setup "clean" []
  probe
    (arguments ["--host=x86_64-unknown-linux-gnu", "--build=x86_64-unknown-linux-musl"])
    "x86_64-unknown-linux-gnu"
    "x86_64-unknown-linux-musl"
    "cross"
  setup "clean" []
  probe
    (arguments $ configureTargetDefaults "x86_64-unknown-linux-gnu" "x86_64-unknown-linux-musl" [])
    "x86_64-unknown-linux-gnu"
    "x86_64-unknown-linux-musl"
    "cross"
  setup "clean" []
  probe
    (arguments $ configureTargetDefaults "arm-unknown-linux-gnueabi" "arm-unknown-linux-gnueabihf" [])
    "arm-unknown-linux-gnueabi"
    "arm-unknown-linux-gnueabihf"
    "cross"
  setup "clean" []
  probe
    (arguments ["--host=arm-unknown-linux-gnueabi", "--build=arm-unknown-linux-gnueabihf"])
    "arm-unknown-linux-gnueabi"
    "arm-unknown-linux-gnueabihf"
    "cross"
  setup "clean" []
  probe
    (arguments (["--host=x86_64-unknown-linux", "--host=x86_64-unknown-linux-gnu"] ++ configureTargetDefaults "x86_64-unknown-linux-gnu" "x86_64-unknown-linux" ["--host=x86_64-unknown-linux", "--host=x86_64-unknown-linux-gnu"]))
    "x86_64-unknown-linux-gnu"
    "x86_64-unknown-linux-gnu"
    "native"
  where
    probe options expectedHost expectedBuild expectedProbe = do
      env <- getTestEnv
      let buildDir = testTmpDir env </> "target-abi-setup.dist/work/dist/build"
          probePath = buildDir </> "target-probe"
      probeExists <- liftIO $ doesFileExist probePath
      when probeExists $ liftIO $ removeFile probePath
      setup "configure" options
      recorded <- liftIO $ lines <$> readFile (testTmpDir env </> "cabal-target-triples")
      case recorded of
        [_, host, build] -> do
          assertEqual "raw Autoconf host alias" expectedHost host
          assertEqual "raw Autoconf build alias" expectedBuild build
        _ -> assertFailure $ "configure did not record the raw aliases: " ++ show recorded
      probeResult <- liftIO $ readFile probePath
      assertEqual "Autoconf process probe branch" (expectedProbe ++ "\n") probeResult
