import Data.List (isSuffixOf)
import Data.Maybe (isNothing)
import Distribution.Simple.Compiler (Compiler (compilerWiredInUnitIds), compilerTargetsProfiling, showCompilerIdWithAbi)
import Test.Cabal.Prelude

-- Run with an intrinsic profiling target as --with-ghc and an ordinary
-- --with-build-compiler. The preprocessor checks its ordinary C ABI before
-- target compilation; the library exercises target TH and target C code.
main = cabalTest $ recordMode DoNotRecord $ do
  env <- getTestEnv
  skipUnless "needs an intrinsic profiling target" (compilerTargetsProfiling (testCompiler env))
  unless ("-profiling" `isSuffixOf` showCompilerIdWithAbi (testCompiler env)) $
    fail "the compiler ABI tag does not include the target ABI"
  unless (isNothing (compilerWiredInUnitIds (testCompiler env))) $
    fail "native compiler wired-in unit IDs leaked into the profiling target"
  skipUnless "needs a separate ordinary build compiler" $ case testBuildCompilerPath env of
    Nothing -> False
    Just _ -> True
  let buildCompilerArgs = maybe [] (\bc -> ["--with-build-compiler=" ++ bc]) (testBuildCompilerPath env)
  defaults <- cabal' "v2-build" (buildCompilerArgs ++ ["all", "-v2"])
  assertOutputDoesNotContain "-hisuf p_hi" defaults
  assertOutputDoesNotContain "-osuf p_o" defaults
  let work = testDistDir env
  void $ assertGlobMatches work "**/ProfileTarget.hi"
  void $ assertGlobMatches work "**/libHSprofiling-target-abi-*.a"
  assertGlobDoesNotMatch work "**/*.p_hi"
  assertGlobDoesNotMatch work "**/*_p.a"
  assertGlobDoesNotMatch work "**/*.p_o"
  assertGlobDoesNotMatch work "**/*.p_dyn_hi"
  assertGlobDoesNotMatch work "**/*.p_dyn_o"
  assertGlobDoesNotMatch work "**/*_p_dyn*"
  runResult <- cabal' "v2-run" (buildCompilerArgs ++ ["profile-target", "--", "+RTS", "-p", "-RTS"])
  assertOutputContains "profiling target ABI" runResult
  shouldExist (testCurrentDir env </> "profile-target.prof")

  -- Profiling requests keep the same intrinsic target universe and
  -- ordinary build tools, while instrumentation options still take effect.
  result <-
    cabal' "v2-build" $
      buildCompilerArgs
        ++ [ "all"
           , "-v2"
           , "--enable-profiling"
           , "--profiling-detail=toplevel-functions"
           , "--library-profiling-detail=late"
           ]
  assertOutputContains "-fprof-auto-top" result
  assertOutputContains "-fprof-late" result
  assertOutputContains "-fprof-cafs" result
  assertOutputDoesNotContain "-hisuf p_hi" result
  assertOutputDoesNotContain "-osuf p_o" result
  assertGlobDoesNotMatch work "**/*.p_hi"
  assertGlobDoesNotMatch work "**/*_p.a"
  assertGlobDoesNotMatch work "**/*.p_o"
  assertGlobDoesNotMatch work "**/*.p_dyn_hi"
  assertGlobDoesNotMatch work "**/*.p_dyn_o"
  assertGlobDoesNotMatch work "**/*_p_dyn*"
