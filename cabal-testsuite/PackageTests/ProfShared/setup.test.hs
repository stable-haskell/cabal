import qualified Data.Map as Map
import Distribution.Simple.Compiler
  ( Compiler (compilerProperties)
  , compilerTargetName
  , compilerTargetsProfiling
  , profilingDebugSupported
  , profilingDynamicSupported
  , renderTargetName
  , showCompilerIdWithAbi
  )
import Test.Cabal.Prelude

-- Profiling is a target ABI. Both Setup and the client reject obsolete ways
-- on an ordinary compiler; profiling targets use static/shared artifact ways.
main = do
  setupTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    if compilerTargetsProfiling (testCompiler env)
      then do
        target <- either fail (pure . renderTargetName) (compilerTargetName (testPlatform env) (testCompiler env))
        let targetLabel =
              case Map.lookup "Target name" (compilerProperties (testCompiler env)) of
                Just _ -> target
                Nothing -> showCompilerIdWithAbi (testCompiler env)

        -- Every profiling target supports the static variant. Profiling is
        -- the target ABI, so the ordinary build must not acquire a legacy
        -- profiling way or profiling-only filename suffix.
        setup "configure" []
        staticBuild <- setup' "build" []
        assertOutputContains "StaticWay" staticBuild
        assertOutputDoesNotContain "ProfWay" staticBuild
        assertOutputDoesNotContain "p_o" staticBuild
        assertGlobDoesNotMatchTestDir testDistDir "**/*.p_hi"
        assertGlobDoesNotMatchTestDir testDistDir "**/*.p_o"
        assertGlobDoesNotMatchTestDir testDistDir "**/*_p.a"

        -- A redundant profiling request still selects the same static target
        -- artifacts; it does not ask Setup to select the old ProfWay.
        setup "clean" []
        setup "configure" ["--enable-profiling"]
        redundantProfilingBuild <- setup' "build" []
        assertOutputContains "StaticWay" redundantProfilingBuild
        assertOutputDoesNotContain "ProfWay" redundantProfilingBuild
        assertOutputDoesNotContain "p_o" redundantProfilingBuild
        assertGlobDoesNotMatchTestDir testDistDir "**/*.p_hi"
        assertGlobDoesNotMatchTestDir testDistDir "**/*.p_o"
        assertGlobDoesNotMatchTestDir testDistDir "**/*_p.a"

        case profilingDynamicSupported (testCompiler env) of
          Just False -> forM_ sharedVariantFlags $ \(flag, variant) -> do
            result <- fails $ setup' "configure" [flag]
            assertOutputContains
              ("Target " ++ targetLabel ++ " does not support the requested " ++ variant ++ " variant. Supported alternatives: static nondebug libraries and executables")
              result
          _ -> do
            skipIfNoSharedLibraries
            setup "configure" ["--enable-profiling", "--enable-profiling-shared", "--enable-shared"]
            sharedBuild <- setup' "build" []
            assertOutputContains "DynWay" sharedBuild
            assertOutputContains "StaticWay" sharedBuild
            assertOutputDoesNotContain "ProfWay" sharedBuild
            assertOutputDoesNotContain "p_dyn_o" sharedBuild

        when (profilingDebugSupported (testCompiler env) == Just False) $ do
          setup "configure" ["--enable-profiling", "--ghc-option=-debug"]
          debugBuild <- fails $ setup' "build" []
          assertOutputContains
            ("Target " ++ targetLabel ++ " does not support the requested debug profiling objects variant. Supported alternatives: static nondebug libraries and executables")
            debugBuild
      else forM_ profilingFlags $ \flag -> do
        result <- fails $ setup' "configure" [flag]
        assertOutputContains "Profiling requires a separate profiling-target compiler" result
  cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    if compilerTargetsProfiling (testCompiler env)
      then do
        target <- either fail (pure . renderTargetName) (compilerTargetName (testPlatform env) (testCompiler env))
        let targetLabel =
              case Map.lookup "Target name" (compilerProperties (testCompiler env)) of
                Just _ -> target
                Nothing -> showCompilerIdWithAbi (testCompiler env)
        let buildCompilerArgs = maybe [] (\bc -> ["--with-build-compiler=" ++ bc]) (testBuildCompilerPath env)
        case profilingDynamicSupported (testCompiler env) of
          Just False -> forM_ sharedVariantFlags $ \(flag, variant) -> do
            result <- fails $ cabal' "v2-build" (buildCompilerArgs ++ [flag])
            assertOutputContains
              ("Target " ++ targetLabel ++ " does not support the requested " ++ variant ++ " variant. Supported alternatives: static nondebug libraries and executables")
              result
          _ -> do
            skipIfNoSharedLibraries
            result <- cabal' "v2-build" (buildCompilerArgs ++ ["--enable-profiling", "--enable-profiling-shared", "--enable-shared"])
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

sharedVariantFlags =
  [ ("--enable-shared", "shared libraries")
  , ("--enable-profiling-shared", "shared profiling libraries")
  , ("--enable-executable-dynamic", "dynamically linked executables")
  ]
