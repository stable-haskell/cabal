-- | A variant is a second set of generated build files for the same
-- project, e.g. GHC's stage 2 next to its stage 1. See Note [Variants].
module Distribution.Client.Buck2.Variant
  ( Variant (..)
  , baseVariant
  , mkVariant
  , variantSuffix
  , variantTargetName
  , variantBzlFile
  , variantAutogenDir
  , variantPlatform
  , variantThirdPartyDir
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Set as Set
import System.FilePath ((</>))

-- Note [Variants]
-- ~~~~~~~~~~~~~~~
-- A staged compiler build compiles the same packages more than once: GHC's
-- stage 2 builds compiler, ghc-boot, ... again, with other flags and with
-- the stage-1 compiler. One tree, two sets of targets. @cabal buck2
-- --variant stage2@ generates:
--
--   * @BUCK.stage2.cabal.bzl@ next to @BUCK.cabal.bzl@, and a call of its
--     @generated_targets@ appended to the package's @BUCK@;
--   * targets and local labels with a @-stage2@ suffix
--     (@//compiler:ghc-stage2@);
--   * autogen files under @cabal-buck2/autogen-stage2/@;
--   * @default_target_platform = "root//buck2/platforms:stage2"@ on every
--     generated rule: the variant's targets build in the platform whose
--     toolchain is the stage-1 compiler (haskell-buck2's platforms/BUCK and
--     toolchains/BUCK).
--
-- Build tools (@build-tool-depends@) are not variant-specific: they run on
-- the build machine and are exec deps, so the variant references the base
-- variant's tool targets (@//utils/genprimopcode:genprimopcode@,
-- @third-party-haskell//:alex-exe@). The variant does not regenerate
-- @third-party/haskell@ either; its own prebuilt closure goes to
-- @third-party/haskell-stage2@, which the rules do not use.
data Variant = Variant
  { variantName :: Maybe String
  -- ^ 'Nothing' for the base (unsuffixed) set of build files.
  , variantBaseTools :: Set String
  -- ^ Tools available as @third-party-haskell//:<tool>-exe@ in the base
  -- variant; see Note [Variants].
  }

baseVariant :: Variant
baseVariant = Variant Nothing Set.empty

mkVariant :: Maybe String -> Set String -> Variant
mkVariant = Variant

-- | @""@, or @"-stage2"@.
variantSuffix :: Variant -> String
variantSuffix v = maybe "" ('-' :) (variantName v)

-- | A target name of the variant: @ghc@, or @ghc-stage2@.
variantTargetName :: Variant -> String -> String
variantTargetName v n = n ++ variantSuffix v

-- | @BUCK.cabal.bzl@, or @BUCK.stage2.cabal.bzl@.
variantBzlFile :: Variant -> FilePath
variantBzlFile v = "BUCK" ++ maybe "" ('.' :) (variantName v) ++ ".cabal.bzl"

-- | @cabal-buck2/autogen@, or @cabal-buck2/autogen-stage2@, relative to the
-- package directory.
variantAutogenDir :: Variant -> FilePath
variantAutogenDir v = "cabal-buck2" </> ("autogen" ++ variantSuffix v)

-- | The target platform of the variant's rules, if any.
variantPlatform :: Variant -> Maybe String
variantPlatform v = fmap ("root//buck2/platforms:" ++) (variantName v)

-- | @third-party/haskell@, or @third-party/haskell-stage2@, relative to the
-- project root.
variantThirdPartyDir :: Variant -> FilePath
variantThirdPartyDir v = "third-party" </> ("haskell" ++ variantSuffix v)
