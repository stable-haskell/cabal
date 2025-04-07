{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.UnitId
  ( UnitId
  , unUnitId
  , mkUnitId
  , isPartialUnitId
  , addPrefixToUnitId
  , addSuffixToUnitId
  , DefUnitId
  , unsafeMkDefUnitId
  , unDefUnitId
  , newSimpleUnitId
  , mkLegacyUnitId
  , getHSLibraryName
  , getAbiTag
  ) where

import Distribution.Compiler (AbiTag (..))
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.ComponentId
import Distribution.Types.PackageId

import Text.PrettyPrint (text)

import GHC.Stack (HasCallStack, prettyCallStack, callStack)
import Data.List (isInfixOf)

import Unsafe.Coerce (unsafeCoerce)

-- | A unit identifier identifies a (possibly instantiated)
-- package/component that can be installed the installed package
-- database.  There are several types of components that can be
-- installed:
--
--  * A traditional library with no holes, so that 'unitIdHash'
--    is @Nothing@.  In the absence of Backpack, 'UnitId'
--    is the same as a 'ComponentId'.
--
--  * An indefinite, Backpack library with holes.  In this case,
--    'unitIdHash' is still @Nothing@, but in the install,
--    there are only interfaces, no compiled objects.
--
--  * An instantiated Backpack library with all the holes
--    filled in.  'unitIdHash' is a @Just@ a hash of the
--    instantiating mapping.
--
-- A unit is a component plus the additional information on how the
-- holes are filled in. Thus there is a one to many relationship: for a
-- particular component there are many different ways of filling in the
-- holes, and each different combination is a unit (and has a separate
-- 'UnitId').
--
-- 'UnitId' is distinct from 'OpenUnitId', in that it is always
-- installed, whereas 'OpenUnitId' are intermediate unit identities
-- that arise during mixin linking, and don't necessarily correspond
-- to any actually installed unit.  Since the mapping is not actually
-- recorded in a 'UnitId', you can't actually substitute over them
-- (but you can substitute over 'OpenUnitId').  See also
-- "Distribution.Backpack.FullUnitId" for a mechanism for expanding an
-- instantiated 'UnitId' to retrieve its mapping.
--
-- Backwards compatibility note: if you need to get the string
-- representation of a UnitId to pass, e.g., as a @-package-id@
-- flag, use the 'display' function, which will work on all
-- versions of Cabal.
data UnitId = UnitId {unitComp :: ShortText, unitId :: ShortText, wasPartial :: Bool }
            | PartialUnitId ShortText
  deriving (Generic, Read, Show, Data)

instance Eq UnitId where
  (UnitId c1 s1 _) == (UnitId c2 s2 _) = c1 == c2 && s1 == s2
  (PartialUnitId s1) == (PartialUnitId s2) = s1 == s2
  _ == _ = False

instance Ord UnitId where
  compare (UnitId c1 s1 _) (UnitId c2 s2 _) = compare (c1, s1) (c2, s2)
  compare (PartialUnitId s1) (PartialUnitId s2) = compare s1 s2
  compare (PartialUnitId _) _ = LT
  compare _ (PartialUnitId _) = GT

instance Binary UnitId
instance Structured UnitId
instance NFData UnitId

-- | The textual format for 'UnitId' coincides with the format
-- GHC accepts for @-package-id@.
instance Pretty UnitId where
  pretty = text . unUnitId

-- | The textual format for 'UnitId' coincides with the format
-- GHC accepts for @-package-id@.
instance Parsec UnitId where
  parsec = P.try (mkUnitId' <$> compid <* P.char '_' <*> P.munch1 isUnitChar <*> return False)
        <|> (mkPartialUnitId <$> P.munch1 isUnitChar)
    where
      compid = (\f v -> f ++ "-" ++ v) <$> P.munch1 isAlpha <* P.char '-' <*> P.munch1 isVerChar
      isVerChar :: Char -> Bool
      isVerChar c = c `elem` '.':['0'..'9']
      -- https://gitlab.haskell.org/ghc/ghc/issues/17752
      isUnitChar '-' = True
      isUnitChar '_' = True
      isUnitChar '.' = True
      isUnitChar '+' = True
      isUnitChar c = isAlphaNum c

isPartialUnitId :: HasCallStack => UnitId -> Bool
isPartialUnitId (PartialUnitId _) = True
isPartialUnitId _ = False

addPrefixToUnitId :: HasCallStack => String -> UnitId -> UnitId
-- addPrefixToUnitId prefix (PartialUnitId s) | s == toShortText "process-1.6.25.0-inplace" = trace ("### addPrefixToUnitId': `" ++ prefix ++ "' `" ++ (fromShortText s) ++ "'.\n" ++ prettyCallStack callStack) $ UnitId (toShortText prefix) s True
addPrefixToUnitId prefix (PartialUnitId s) = UnitId (toShortText prefix) s True
addPrefixToUnitId prefix uid@(UnitId _ _ _) = error $ "addPrefixToUnitId: UnitId " ++ show uid ++ " already has a prefix; can't add: " ++ prefix

addSuffixToUnitId :: HasCallStack => String -> UnitId -> UnitId
addSuffixToUnitId suffix (UnitId c s fromPartial) = UnitId c (s <> toShortText suffix) fromPartial
addSuffixToUnitId suffix (PartialUnitId s) = PartialUnitId (s <> toShortText suffix)


dropPrefixFromUnitId :: HasCallStack => UnitId -> UnitId
dropPrefixFromUnitId (PartialUnitId s) = PartialUnitId s
dropPrefixFromUnitId (UnitId _c s _fromPartial) = PartialUnitId s

-- | If you need backwards compatibility, consider using 'display'
-- instead, which is supported by all versions of Cabal.
unUnitId :: HasCallStack => UnitId -> String
unUnitId (UnitId c s False) = fromShortText c ++ '_':fromShortText s
unUnitId (UnitId c s True) = fromShortText s
unUnitId (PartialUnitId s) = fromShortText s

mkUnitId :: HasCallStack => String -> UnitId
mkUnitId s = case (simpleParsec s) of
    -- Just uid@UnitId{ unitComp = c, unitId = i } | (fromShortText c) == "ghc-9.8.4", (fromShortText i) == "rts-1.0.3-cec100dd" -> trace ("### mkUnitId: `" ++ (fromShortText c) ++ "' `" ++ (fromShortText i) ++ "' is a full one.\n" ++ prettyCallStack callStack) uid
    Just uid@UnitId{} -> uid
    Just uid@PartialUnitId{} -> uid -- error $ "mkUnitId: `" ++ s ++ "' is a partial unit id, not a full one."
    _ -> error $ "Unable to parse UnitId: `" ++ s ++ "'."

mkUnitId' :: HasCallStack => String -> String -> Bool -> UnitId
-- mkUnitId' c i b | c == "ghc-9.8.4", i == "rts-1.0.3-cec100dd" = trace ("### mkUnitId': `" ++ c ++ "' `" ++ i ++ "' is a full one.\n" ++ prettyCallStack callStack) (UnitId (toShortText c) (toShortText i) b)
mkUnitId' c i b = UnitId (toShortText c) (toShortText i) b

mkPartialUnitId :: HasCallStack => String -> UnitId
-- mkPartialUnitId s | s == "process-1.6.25.0-inplace" = trace ("### mkPartialUnitId: `" ++ s ++ "' is a partial unit id, not a full one.\n" ++ prettyCallStack callStack) (PartialUnitId (toShortText s))
mkPartialUnitId s = PartialUnitId (toShortText s)

-- | 'mkUnitId'
--
-- @since 2.0.0.2
instance IsString UnitId where
  fromString = mkUnitId

-- | Create a unit identity with no associated hash directly
-- from a 'ComponentId'.
newSimpleUnitId :: HasCallStack => ComponentId -> UnitId
newSimpleUnitId = unsafeCoerce

-- | Make an old-style UnitId from a package identifier.
-- Assumed to be for the public library
mkLegacyUnitId :: HasCallStack => PackageId -> UnitId
mkLegacyUnitId = newSimpleUnitId . mkComponentId . prettyShow

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: UnitId -> String
getHSLibraryName uid = "HS" ++ prettyShow (dropPrefixFromUnitId uid)

-- | A 'UnitId' for a definite package.  The 'DefUnitId' invariant says
-- that a 'UnitId' identified this way is definite; i.e., it has no
-- unfilled holes.
newtype DefUnitId = DefUnitId {unDefUnitId :: UnitId}
  deriving (Generic, Read, Show, Eq, Ord, Data, Binary, NFData, Pretty)

instance Structured DefUnitId

-- Workaround for a GHC 8.0.1 bug, see
-- https://github.com/haskell/cabal/issues/4793#issuecomment-334258288
instance Parsec DefUnitId where
  parsec = DefUnitId <$> parsec

-- | Unsafely create a 'DefUnitId' from a 'UnitId'.  Your responsibility
-- is to ensure that the 'DefUnitId' invariant holds.
unsafeMkDefUnitId :: UnitId -> DefUnitId
unsafeMkDefUnitId = DefUnitId

-- | The ABI tag is the part of the unit id that comes after the
-- last hyphen.  It is used to distinguish between different
-- versions of the same package that are ABI compatible.
--
-- FIXME: ideally this would be part of the proper structure of the
-- datatype, instead of some heuristic of the string.
getAbiTag :: HasCallStack => UnitId -> AbiTag
getAbiTag (UnitId _c s _) = case takeWhile (/= '-') . reverse . fromShortText $ s of
  [] -> NoAbiTag
  xs -> AbiTag . reverse $ xs
getAbiTag (PartialUnitId s) = case takeWhile (/= '-') . reverse . fromShortText $ s of
  [] -> NoAbiTag
  xs -> AbiTag . reverse $ xs