import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (buildDir, interpretSymbolicPathLBI)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Generate the C-- source into the build directory before building, then
-- delegate to the default build. The library lists this file in
-- 'autogen-cmm-sources', so Cabal resolves it relative to the build directory
-- (not the package source tree) and compiles it.
main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \pkg lbi hooks flags -> do
          let dir = interpretSymbolicPathLBI lbi (buildDir lbi)
          createDirectoryIfMissing True dir
          writeFile (dir </> "HeapPrim.cmm") generatedCmm
          buildHook simpleUserHooks pkg lbi hooks flags
      }

generatedCmm :: String
generatedCmm =
  unlines
    [ "#include \"Cmm.h\""
    , ""
    , "aToMyWordzh (P_ clos)"
    , "{"
    , "    return (clos);"
    , "}"
    ]
