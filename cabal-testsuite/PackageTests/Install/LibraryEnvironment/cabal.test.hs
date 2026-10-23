import qualified Data.Map as Map
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.Compiler (Compiler (compilerProperties), compilerTargetAbi)
import System.Directory (createDirectoryIfMissing)
import Test.Cabal.Prelude

-- Run with an ordinary compiler, or an intrinsic profiling target and an
-- ordinary --with-build-compiler. Both installs must remain usable after the
-- temporary project has been removed; --builddir only selects build scratch.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let compiler = testCompiler env
      targetPlatform = Map.findWithDefault (prettyShow (testPlatform env)) "Target platform" (compilerProperties compiler)
      targetAbi = compilerTargetAbi compiler
      targetDirectory = targetPlatform ++ if null targetAbi then "" else "-" ++ targetAbi
      extraDb = testWorkDir env </> "extra-package-db"
      explicitStore = testWorkDir env </> "explicit-store"
      buildCompilerArgs = maybe [] (\bc -> ["--with-build-compiler=" ++ bc]) (testBuildCompilerPath env)
      installAndUse name storeRoot storeArgs buildArgs = do
        let envDir = testCurrentDir env </> name
            packageDb = storeRoot </> "host" </> targetDirectory </> "package.conf.d"
            binaryName = "use-environment" <.> exeExtension (testPlatform env)
        liftIO $ createDirectoryIfMissing True envDir
        -- Use the raw runner so the first install has no --builddir override.
        void $
          cabal_raw'
            ( storeArgs
                ++ [ "v2-install"
                   , "-j1"
                   , "--lib"
                   , "--package-env=" ++ envDir
                   , "--package-db=" ++ extraDb
                   , "library-environment"
                   ]
                ++ buildCompilerArgs
                ++ buildArgs
            )
            Nothing
        envFiles <- assertGlobMatches envDir ".ghc.environment.*"
        envFile <- case envFiles of
          [file] -> pure file
          _ -> fail "expected one compiler environment file"
        contents <- liftIO $ readFile envFile
        assertBool "the environment includes the selected persistent store database" $
          ("package-db " ++ packageDb) `elem` lines contents
        assertBool "the environment preserves an explicit package database" $
          ("package-db " ++ extraDb) `elem` lines contents
        shouldExist (packageDb </> "package.cache")
        withDirectory name $ withEnv [("GHC_ENVIRONMENT", Nothing)] $ do
          ghc ["-v0", testCurrentDir env </> "UseEnvironment.hs", "-o", binaryName]
          result <- runM (envDir </> binaryName) [] Nothing
          assertOutputContains "42" result
  void $ ghcPkg_raw' ["init", extraDb]
  installAndUse "default-env" (testCabalDir env </> "store") [] []
  installAndUse "explicit-env" explicitStore ["--store-dir=" ++ explicitStore] ["--builddir=" ++ testDistDir env]
