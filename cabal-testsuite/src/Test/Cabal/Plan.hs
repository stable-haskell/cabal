{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for understanding @plan.json@.
module Test.Cabal.Plan
  ( Plan (..)
  , DistDirOrBinFile (..)
  , InstallItem (..)
  , ConfiguredInplace (..)
  , ConfiguredGlobal (..)
  , PlanStage (..)
  , Revision (..)
  , decodePlan
  , planDistDir
  , planDistDirForStage
  , buildInfoFile
  , buildInfoFileForStage
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import Distribution.Package
import Distribution.Parsec (eitherParsec, simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.ComponentName
import Distribution.Types.Version

-- TODO: index this
data Plan = Plan {planInstallPlan :: [InstallItem]}
  deriving (Show)

data PlanStage = BuildStage | HostStage
  deriving (Eq, Show)

instance FromJSON PlanStage where
  parseJSON = withText "PlanStage" $ \s -> case s of
    "build" -> pure BuildStage
    "host" -> pure HostStage
    _ -> fail $ "unrecognized package stage: " ++ Text.unpack s

decodePlan :: BS.ByteString -> Either String Plan
decodePlan = eitherDecode

data InstallItem
  = APreExisting
  | AConfiguredGlobal ConfiguredGlobal
  | AConfiguredInplace ConfiguredInplace
  deriving (Show)

-- local or inplace package
data ConfiguredInplace = ConfiguredInplace
  { configuredInplaceStage :: PlanStage
  , configuredInplaceDistDir :: FilePath
  , configuredInplaceBuildInfo :: Maybe FilePath
  , configuredInplacePackageName :: PackageName
  , configuredInplaceVersion :: Version
  , configuredInplaceRevision :: Revision
  , configuredInplaceComponentName :: Maybe ComponentName
  }
  deriving (Show)

data ConfiguredGlobal = ConfiguredGlobal
  { configuredGlobalStage :: PlanStage
  , configuredGlobalBinFile :: Maybe FilePath
  , configuredGlobalDistDir :: Maybe FilePath
  , configuredGlobalBuildInfo :: Maybe FilePath
  , configuredGlobalPackageName :: PackageName
  , configuredGlobalVersion :: Version
  , configuredGlobalRevision :: Revision
  , configuredGlobalComponentName :: Maybe ComponentName
  }
  deriving (Show)

newtype Revision = Revision Int
  deriving (Show, Eq, FromJSON)

instance FromJSON Plan where
  parseJSON (Object v) = fmap Plan (v .: "install-plan")
  parseJSON invalid = typeMismatch "Plan" invalid

instance FromJSON InstallItem where
  parseJSON obj@(Object v) = do
    t <- v .: "type"
    case t :: String of
      "pre-existing" -> return APreExisting
      "configured" -> configured
      "installed" -> configured
      _ -> fail "unrecognized value of 'type' field"
    where
      configured = do
        s <- v .:? "style"
        case s :: Maybe String of
          -- Staged plans use a project-local store for every component and
          -- no longer distinguish installed/inplace build styles.
          Nothing -> do
            _ <- v .: "stage" :: Parser PlanStage
            AConfiguredGlobal <$> parseJSON obj
          Just "global" -> AConfiguredGlobal <$> parseJSON obj
          Just "inplace" -> AConfiguredInplace <$> parseJSON obj
          Just "local" -> AConfiguredInplace <$> parseJSON obj
          Just other -> fail $ "unrecognized value of 'style' field: " ++ other
  parseJSON invalid = typeMismatch "InstallItem" invalid

instance FromJSON ConfiguredInplace where
  parseJSON (Object v) = do
    stage <- v .:? "stage" .!= HostStage
    dist_dir <- v .: "dist-dir"
    build_info <- v .:? "build-info"
    pkg_name <- v .: "pkg-name"
    pkg_version <- v .: "pkg-version"
    pkg_revision <- v .: "pkg-revision"
    component_name <- v .:? "component-name"
    return (ConfiguredInplace stage dist_dir build_info pkg_name pkg_version pkg_revision component_name)
  parseJSON invalid = typeMismatch "ConfiguredInplace" invalid

instance FromJSON ConfiguredGlobal where
  parseJSON (Object v) = do
    stage <- v .:? "stage" .!= HostStage
    bin_file <- v .:? "bin-file"
    dist_dir <- v .:? "dist-dir"
    build_info <- v .:? "build-info"
    pkg_name <- v .: "pkg-name"
    pkg_version <- v .: "pkg-version"
    pkg_revision <- v .: "pkg-revision"
    component_name <- v .:? "component-name"
    return (ConfiguredGlobal stage bin_file dist_dir build_info pkg_name pkg_version pkg_revision component_name)
  parseJSON invalid = typeMismatch "ConfiguredGlobal" invalid

instance FromJSON PackageName where
  parseJSON (String t) = return (mkPackageName (Text.unpack t))
  parseJSON invalid = typeMismatch "PackageName" invalid

instance FromJSON Version where
  parseJSON = withText "Version" $ either fail pure . eitherParsec . Text.unpack

instance FromJSON ComponentName where
  parseJSON (String t) =
    case simpleParsec s of
      Nothing -> fail ("could not parse component-name: " ++ s)
      Just r -> return r
    where
      s = Text.unpack t
  parseJSON invalid = typeMismatch "ComponentName" invalid

data DistDirOrBinFile = DistDir FilePath | BinFile FilePath
  deriving (Eq, Show)

planDistDir :: Plan -> PackageName -> ComponentName -> DistDirOrBinFile
planDistDir = planDistDirForStage HostStage

-- | Select the target component by default; Build copies must be requested
-- explicitly, since their executable ABI need not match the target's.
planDistDirForStage :: PlanStage -> Plan -> PackageName -> ComponentName -> DistDirOrBinFile
planDistDirForStage stage plan pkg_name cname =
  case concatMap p (planInstallPlan plan) of
    [x] -> x
    [] ->
      error $
        "planDistDir: component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " either does not"
          ++ " exist in the install plan or does not have a dist-dir nor bin-file"
    _ ->
      error $
        "planDistDir: found multiple copies of component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " in install plan"
  where
    p APreExisting = []
    p (AConfiguredGlobal conf) = do
      guard (configuredGlobalStage conf == stage)
      guard (configuredGlobalPackageName conf == pkg_name)
      guard $ case configuredGlobalComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      case configuredGlobalBinFile conf of
        Nothing -> maybe [] (pure . DistDir) (configuredGlobalDistDir conf)
        Just bin_file -> return $ BinFile bin_file
    p (AConfiguredInplace conf) = do
      guard (configuredInplaceStage conf == stage)
      guard (configuredInplacePackageName conf == pkg_name)
      guard $ case configuredInplaceComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      return (DistDir (configuredInplaceDistDir conf))

buildInfoFile :: Plan -> PackageName -> ComponentName -> FilePath
buildInfoFile = buildInfoFileForStage HostStage

buildInfoFileForStage :: PlanStage -> Plan -> PackageName -> ComponentName -> FilePath
buildInfoFileForStage stage plan pkg_name cname =
  case concatMap p (planInstallPlan plan) of
    [Just x] -> x
    [Nothing] ->
      error $
        "buildInfoFile: component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " does not"
          ++ " have a build info-file"
    [] ->
      error $
        "buildInfoFile: component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " either does not"
          ++ " exist in the install plan or build info-file"
    _ ->
      error $
        "buildInfoFile: found multiple copies of component "
          ++ prettyShow cname
          ++ " of package "
          ++ prettyShow pkg_name
          ++ " in install plan"
  where
    p APreExisting = []
    p (AConfiguredGlobal conf) = do
      guard (configuredGlobalStage conf == stage)
      guard (configuredGlobalPackageName conf == pkg_name)
      guard $ case configuredGlobalComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      return $ configuredGlobalBuildInfo conf
    p (AConfiguredInplace conf) = do
      guard (configuredInplaceStage conf == stage)
      guard (configuredInplacePackageName conf == pkg_name)
      guard $ case configuredInplaceComponentName conf of
        Nothing -> True
        Just cname' -> cname == cname'
      return $ configuredInplaceBuildInfo conf
