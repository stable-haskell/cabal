import Distribution.Simple.Compiler (compilerTargetsProfiling)
import Test.Cabal.Prelude

-- Profiling is a target ABI. Both Setup and the client reject obsolete ways
-- on an ordinary compiler; profiling targets use static/shared artifact ways.
main = do
  setupTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    if compilerTargetsProfiling (testCompiler env)
      then do
        skipIfNoSharedLibraries
        setup "configure" ["--enable-profiling", "--enable-profiling-shared", "--enable-shared"]
        result <- setup' "build" []
        assertOutputContains "DynWay" result
        assertOutputContains "StaticWay" result
        assertOutputDoesNotContain "ProfWay" result
        assertOutputDoesNotContain "p_dyn_o" result
      else forM_ profilingFlags $ \flag -> do
        result <- fails $ setup' "configure" [flag]
        assertOutputContains "Profiling requires a separate profiling-target compiler" result
  cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    if compilerTargetsProfiling (testCompiler env)
      then do
        skipIfNoSharedLibraries
        result <- cabal' "v2-build" ["--enable-profiling", "--enable-profiling-shared", "--enable-shared"]
        assertOutputContains "DynWay" result
        assertOutputDoesNotContain "ProfWay" result
        assertOutputDoesNotContain "p_dyn_o" result
      else forM_ profilingFlags $ \flag -> do
        result <- fails $ cabal' "v2-build" [flag]
        assertOutputContains "Profiling requires a separate profiling-target compiler" result

profilingFlags =
  [ "--enable-profiling"
  , "--enable-library-profiling"
  , "--enable-executable-profiling"
  , "--enable-profiling-shared"
  ]
