import Data.List (isPrefixOf)
import Distribution.Simple.BuildPaths (exeExtension)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>))
import Test.Cabal.Prelude

-- Run with an ordinary compiler, or an intrinsic profiling target and an
-- ordinary --with-build-compiler. Both installs must remain usable after the
-- temporary project has been removed; --builddir only selects build scratch.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  let extraDb = testWorkDir env </> "extra-package-db"
      explicitStore = testWorkDir env </> "explicit-store"
      buildCompilerArgs = maybe [] (\bc -> ["--with-build-compiler=" ++ bc]) (testBuildCompilerPath env)
      installAndUse name storeRoot storeArgs buildArgs = do
        let envDir = testCurrentDir env </> name
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
        -- The staged store lays out databases at
        -- <store>/host/<platform>/package.conf.d (with a target-ABI suffix
        -- such as -prof when one applies), so match by store prefix and
        -- package.conf.d suffix rather than a fully spelled-out path.
        let dbLines = [drop (length "package-db ") l | l <- lines contents, "package-db " `isPrefixOf` l]
            storeDbs =
              [ db
              | db <- dbLines
              , (storeRoot </> "host") `isPrefixOf` db
              , takeFileName db == "package.conf.d"
              ]
        assertBool "the environment includes the selected persistent store database" $
          not (null storeDbs)
        assertBool "the environment preserves an explicit package database" $
          ("package-db " ++ extraDb) `elem` lines contents
        case storeDbs of
          (storeDb : _) -> shouldExist (storeDb </> "package.cache")
          [] -> fail "no store database found in environment file"
        withDirectory name $ withEnv [("GHC_ENVIRONMENT", Nothing)] $ do
          ghc ["-v0", testCurrentDir env </> "UseEnvironment.hs", "-o", binaryName]
          result <- runM (envDir </> binaryName) [] Nothing
          assertOutputContains "42" result
  void $ ghcPkg_raw' ["init", extraDb]
  installAndUse "default-env" (testCabalDir env </> "store") [] []
  installAndUse "explicit-env" explicitStore ["--store-dir=" ++ explicitStore] ["--builddir=" ++ testDistDir env]
