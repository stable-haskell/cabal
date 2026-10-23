import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Test.Cabal.Plan
import Test.Cabal.Prelude

main :: IO ()
main = setupTest $ recordMode DoNotRecord $ do
    let package = mkPackageName "probe"
        component = CExeName (mkUnqualComponentName "probe")
        common = "\"pkg-name\":\"probe\",\"pkg-version\":\"1.0\",\"pkg-revision\":0,\"component-name\":\"exe:probe\""
        entry extra = "{\"type\":\"configured\"," ++ common ++ "," ++ extra ++ "}"
        document entries = BS.pack $ "{\"install-plan\":[" ++ intercalate "," entries ++ "]}"
        parse entries = either fail return $ decodePlan (document entries)
        rejects label entries = case decodePlan (document entries) of
            Left _ -> return ()
            Right _ -> fail $ "accepted invalid plan: " ++ label

    -- Both copies have the same package/component name. Selection must retain
    -- their machine, even when the Build copy occurs first in the plan.
    staged <- parse
        [ entry "\"machine\":\"build\",\"bin-file\":\"build bin/probe\",\"build-info\":\"build info.json\""
        , entry "\"machine\":\"host\",\"bin-file\":\"host bin/probe\",\"build-info\":\"host info.json\""
        ]
    assertEqual "default executable is Host" (BinFile "host bin/probe") (planDistDir staged package component)
    assertEqual "explicit Build executable" (BinFile "build bin/probe") (planDistDirForMachine Build staged package component)
    assertEqual "default build info is Host" "host info.json" (buildInfoFile staged package component)
    assertEqual "explicit Build info" "build info.json" (buildInfoFileForMachine Build staged package component)

    built <- parse [entry "\"machine\":\"host\",\"dist-dir\":\"build directory with spaces\""]
    assertEqual "build directory fallback" (DistDir "build directory with spaces") (planDistDir built package component)

    legacyGlobal <- parse [entry "\"style\":\"global\",\"bin-file\":\"legacy global\""]
    assertEqual "legacy installed executable" (BinFile "legacy global") (planDistDir legacyGlobal package component)
    legacyLocal <- parse [entry "\"style\":\"inplace\",\"dist-dir\":\"legacy local\""]
    assertEqual "legacy inplace component" (DistDir "legacy local") (planDistDir legacyLocal package component)

    rejects "unknown machine" [entry "\"machine\":\"unknown\""]
    rejects "missing machine and style" [entry "\"bin-file\":\"probe\""]
    rejects "unknown legacy style" [entry "\"style\":\"unknown\""]
