{-# LANGUAGE CPP #-}
{- FOURMOLU_DISABLE -}
module Distribution.SPDX.LicenseExceptionId (
    LicenseExceptionId (..),
    licenseExceptionId,
    licenseExceptionName,
    mkLicenseExceptionId,
    licenseExceptionIdList,
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Lens (set)
import Distribution.Pretty
import Distribution.Parsec
import Distribution.Utils.Generic (isAsciiAlphaNum)
import Distribution.Utils.Structured (nominalStructure, typeVersion)
import Distribution.SPDX.LicenseListVersion

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Map.Strict as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

#ifdef __MHS__
import Data.Data (Constr, Data (..), DataType, Fixity (Prefix), constrIndex, mkConstrTag, mkDataType)
#endif

-------------------------------------------------------------------------------
-- LicenseExceptionId
-------------------------------------------------------------------------------

-- | SPDX License Exceptions identifiers list v3.28
data LicenseExceptionId
{{ licenseIds }}
  deriving (Eq,
#ifdef __MHS__
    -- No derived Ord/Data: same per-constructor compile cost as
    -- LicenseId at scale; hand-written instances below keep the
    -- semantics and the Data instance downstream deriving requires.
    Enum, Bounded, Show, Read, Generic
#else
    Ord, Enum, Bounded, Show, Read, Data, Generic
#endif
    )

instance Binary LicenseExceptionId where
    put = Binary.putWord8 . fromIntegral . fromEnum
    get = do
        i <- Binary.getWord8
        if i > fromIntegral (fromEnum (maxBound :: LicenseExceptionId))
        then fail "Too large LicenseExceptionId tag"
        else return (toEnum (fromIntegral i))

#ifdef __MHS__
-- Hand-written Ord/Data mirror the derived instances (declaration-order
-- tags, constructor names as declared) without the measured
-- end-to-end compile cost of the derived versions at scale
-- (~10s user each at 150 constructors; root cause not isolated to a
-- compiler phase). Compare via the derived Enum tags; Data round-trips
-- via the tag index. Semantics match the derived instances; Binary
-- (which also uses fromEnum) and all constructors are unchanged.
instance Ord LicenseExceptionId where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Data LicenseExceptionId where
  gfoldl k z x = z x
  gunfold _k z c = z (toEnum (constrIndex c - 1))
  toConstr x = licenseExceptionIdConstrs !! fromEnum x
  dataTypeOf _ = tyLicenseExceptionId

tyLicenseExceptionId :: DataType
tyLicenseExceptionId = mkDataType "Distribution.SPDX.LicenseExceptionId" licenseExceptionIdConstrs

licenseExceptionIdConstrs :: [Constr]
licenseExceptionIdConstrs =
  [ mkConstrTag tyLicenseExceptionId name i [] Prefix
  | (i, name) <- zip [1 ..] licenseExceptionIdConstrNames
  ]

licenseExceptionIdConstrNames :: [String]
licenseExceptionIdConstrNames =
{{ licenseIdNames }}
#endif

-- note: remember to bump version each time the definition changes
instance Structured LicenseExceptionId where
    structure p = set typeVersion 308 $ nominalStructure p

instance Pretty LicenseExceptionId where
    pretty = Disp.text . licenseExceptionId

instance Parsec LicenseExceptionId where
    parsec = do
        n <- some $ P.satisfy $ \c -> isAsciiAlphaNum c || c == '-' || c == '.'
        v <- askCabalSpecVersion
        maybe (fail $ "Unknown SPDX license exception identifier: " ++ n) return $
            mkLicenseExceptionId (cabalSpecVersionToSPDXListVersion v) n

instance NFData LicenseExceptionId where
    rnf l = l `seq` ()

-------------------------------------------------------------------------------
-- License Data
-------------------------------------------------------------------------------

-- | License SPDX identifier, e.g. @"BSD-3-Clause"@.
licenseExceptionId :: LicenseExceptionId -> String
{% for l in licenses %}
licenseExceptionId {{l.constructor}} = {{l.id}}
{% endfor %}

-- | License name, e.g. @"GNU General Public License v2.0 only"@
licenseExceptionName :: LicenseExceptionId -> String
{% for l in licenses %}
licenseExceptionName {{l.constructor}} = {{l.name}}
{% endfor %}

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

licenseExceptionIdList :: LicenseListVersion -> [LicenseExceptionId]
licenseExceptionIdList LicenseListVersion_3_0 =
{{licenseList_perv.v_3_0}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_2 =
{{licenseList_perv.v_3_2}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_6 =
{{licenseList_perv.v_3_6}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_9 =
{{licenseList_perv.v_3_9}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_10 =
{{licenseList_perv.v_3_10}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_16 =
{{licenseList_perv.v_3_16}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_23 =
{{licenseList_perv.v_3_23}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_25 =
{{licenseList_perv.v_3_25}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_26 =
{{licenseList_perv.v_3_26}}
    ++ bulkOfLicenses
licenseExceptionIdList LicenseListVersion_3_28 =
{{licenseList_perv.v_3_28}}
    ++ bulkOfLicenses

-- | Create a 'LicenseExceptionId' from a 'String'.
mkLicenseExceptionId :: LicenseListVersion -> String -> Maybe LicenseExceptionId
mkLicenseExceptionId LicenseListVersion_3_0  s = Map.lookup s stringLookup_3_0
mkLicenseExceptionId LicenseListVersion_3_2  s = Map.lookup s stringLookup_3_2
mkLicenseExceptionId LicenseListVersion_3_6  s = Map.lookup s stringLookup_3_6
mkLicenseExceptionId LicenseListVersion_3_9  s = Map.lookup s stringLookup_3_9
mkLicenseExceptionId LicenseListVersion_3_10 s = Map.lookup s stringLookup_3_10
mkLicenseExceptionId LicenseListVersion_3_16 s = Map.lookup s stringLookup_3_16
mkLicenseExceptionId LicenseListVersion_3_23 s = Map.lookup s stringLookup_3_23
mkLicenseExceptionId LicenseListVersion_3_25 s = Map.lookup s stringLookup_3_25
mkLicenseExceptionId LicenseListVersion_3_26 s = Map.lookup s stringLookup_3_26
mkLicenseExceptionId LicenseListVersion_3_28 s = Map.lookup s stringLookup_3_28

stringLookup_3_0 :: Map String LicenseExceptionId
stringLookup_3_0 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_0

stringLookup_3_2 :: Map String LicenseExceptionId
stringLookup_3_2 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_2

stringLookup_3_6 :: Map String LicenseExceptionId
stringLookup_3_6 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_6

stringLookup_3_9 :: Map String LicenseExceptionId
stringLookup_3_9 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_9

stringLookup_3_10 :: Map String LicenseExceptionId
stringLookup_3_10 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_10

stringLookup_3_16 :: Map String LicenseExceptionId
stringLookup_3_16 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_16

stringLookup_3_23 :: Map String LicenseExceptionId
stringLookup_3_23 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_23

stringLookup_3_25 :: Map String LicenseExceptionId
stringLookup_3_25 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_25

stringLookup_3_26 :: Map String LicenseExceptionId
stringLookup_3_26 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_26

stringLookup_3_28 :: Map String LicenseExceptionId
stringLookup_3_28 = Map.fromList $ map (\i -> (licenseExceptionId i, i)) $
    licenseExceptionIdList LicenseListVersion_3_28

--  | License exceptions in all SPDX License lists
bulkOfLicenses :: [LicenseExceptionId]
bulkOfLicenses =
{{licenseList_all}}
