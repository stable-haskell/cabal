import Distribution.Simple.Compiler (compilerTargetsProfiling)
import Test.Cabal.Prelude

-- dual-stage-tool is both an install target and the native preprocessor of
-- another install target. Promoted install flags must keep that native copy
-- ordinary, while applying profiling and detail options to both target copies.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  skipUnless "needs an intrinsic profiling target" (compilerTargetsProfiling (testCompiler env))
  withBuildCompiler $ \buildCompiler -> do
    let installArgs =
          [ "dual-stage-tool"
          , "profile-install-app"
          , "--with-build-compiler=" ++ buildCompiler
          , "--enable-profiling"
          , "--profiling-detail=toplevel-functions"
          , "--library-profiling-detail=late"
          , "--installdir=" ++ testPrefixDir env </> "bin"
          , "--install-method=copy"
          , "-v2"
          ]
    result <- cabal' "v2-install" installArgs
    assertOutputContains "ordinary install preprocessor" result
    assertOutputContains "-fprof-auto-top" result
    assertOutputContains "-fprof-late" result
    assertOutputContains "-fprof-cafs" result
    tool <- runInstalledExe' "dual-stage-tool" ["+RTS", "-p", "-RTS"]
    assertOutputContains "installed profiling tool" tool
    app <- runInstalledExe' "profile-install-app" ["+RTS", "-p", "-RTS"]
    assertOutputContains "installed profiling application" app
    shouldExist (testCurrentDir env </> "dual-stage-tool.prof")
    shouldExist (testCurrentDir env </> "profile-install-app.prof")

    -- Genuine package settings still apply to its native copy. They must not
    -- be silently normalized along with the synthetic install-local overlay.
    withProjectFile "cabal.explicit-package.project" $
      withStoreDir (testWorkDir env </> "explicit-package-store") $ do
        rejected <- fails $ cabal' "v2-install" installArgs
        assertOutputContains "Profiling requires a separate profiling-target compiler" rejected
