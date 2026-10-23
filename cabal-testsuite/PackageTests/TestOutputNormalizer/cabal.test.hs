import Test.Cabal.Prelude
import Test.Cabal.OutputNormalizer (normalizeCallStackFrames, normalizeScriptBuildCacheKeys)

main = cabalTest $ recordMode DoNotRecord $ do
  let expected =
        unlines
          [ "  withMetadata, called at src/Distribution/Simple/Utils.hs:<LINE>:<COL> in <CABAL_UNIT>:Distribution.Simple.Utils"
          , "  dieProgress, called at src/Distribution/Backpack/LinkedComponent.hs:<LINE>:<COL> in <CABAL_UNIT>:Distribution.Backpack.LinkedComponent"
          , "  elaborateInstallPlan, called at src/Distribution/Client/ProjectPlanning.hs:<LINE>:<COL> in <CABAL_INSTALL_UNIT>:Distribution.Client.ProjectPlanning"
          ]
      windowsFrames =
        unlines
          [ "  withMetadata, called at src\\Distribution\\Simple\\Utils.hs:524:12 in Cabal-3.17.0.1-inplace:Distribution.Simple.Utils"
          , "  dieProgress, called at src\\Distribution\\Backpack\\LinkedComponent.hs:167:30 in Cabal-3.17.0.1-inplace:Distribution.Backpack.LinkedComponent"
          , "  elaborateInstallPlan, called at src\\Distribution\\Client\\ProjectPlanning.hs:918:15 in cabal-install-3.17.0.1-inplace:Distribution.Client.ProjectPlanning"
          ]
      unixFrames =
        unlines
          [ "  withMetadata, called at src/Distribution/Simple/Utils.hs:999:2 in Cabal-3.18.0.0-abcdef123456:Distribution.Simple.Utils"
          , "  dieProgress, called at src/Distribution/Backpack/LinkedComponent.hs:8:900 in Cabal-3.18.0.0-abcdef123456:Distribution.Backpack.LinkedComponent"
          , "  elaborateInstallPlan, called at src/Distribution/Client/ProjectPlanning.hs:44:102 in cabal-install-3.18.0.0-abcdef123456:Distribution.Client.ProjectPlanning"
          ]

  assertEqual "Windows stack frames normalize stably" expected (normalizeCallStackFrames windowsFrames)
  assertEqual "Unix stack frames normalize stably" expected (normalizeCallStackFrames unixFrames)
  assertEqual
    "ordinary diagnostic coordinates remain unchanged"
    "src\\Main.hs:27:4: parse error"
    (normalizeCallStackFrames "src\\Main.hs:27:4: parse error")

  let cacheKey = "2b2a3024980f1fe113ce8236eaaa437c409443c5022667ccbc34"
      unixPath = "/tmp/cabal/home/.cabal/script-builds/" ++ cacheKey ++ "/store/host/aarch64-apple-darwin/bin"
      windowsPath = "C:\\tmp\\cabal\\home\\.cabal\\script-builds\\" ++ cacheKey ++ "\\store\\host\\aarch64-apple-darwin\\bin"
      expectedPath = "/tmp/cabal/home/.cabal/script-builds/<HASH>/store/host/aarch64-apple-darwin/bin"
      expectedWindowsPath = "C:/tmp/cabal/home/.cabal/script-builds/<HASH>/store/host/aarch64-apple-darwin/bin"
  assertEqual
    "Unix script cache key is normalized without changing the store path"
    expectedPath
    (normalizeScriptBuildCacheKeys unixPath)
  assertEqual
    "Windows script cache key is normalized without changing the store path"
    expectedWindowsPath
    (normalizeScriptBuildCacheKeys windowsPath)
  let unrelatedPaths =
        "/tmp/cabal/home/.cabal/cache/" ++ cacheKey ++ "/store"
          ++ " /tmp/cabal/home/.cabal/script-builds/short/store"
          ++ " /tmp/cabal/home/.cabal/script-builds/" ++ replicate 52 'g' ++ "/store"
  assertEqual
    "unrelated and malformed hash paths remain visible"
    unrelatedPaths
    (normalizeScriptBuildCacheKeys unrelatedPaths)
