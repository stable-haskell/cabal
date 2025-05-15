{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ComponentId
  ( ComponentId
  , unComponentId
  , mkComponentId
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
import Text.PrettyPrint (text)

import GHC.Stack (HasCallStack, prettyCallStack, callStack)
-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component (i.e. libraries, executables).
--
-- For non-Backpack components, this corresponds one to one with
-- the 'UnitId', which serves as the basis for install paths,
-- linker symbols, etc.
--
-- Use 'mkComponentId' and 'unComponentId' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
data ComponentId = ComponentId {unitComp :: ShortText, unitId :: ShortText, wasPartial :: Bool }
                 | PartialComponentId ShortText
  deriving (Generic, Read, Show, Data)

instance Eq ComponentId where
  (ComponentId c1 s1 _) == (ComponentId c2 s2 _) = c1 == c2 && s1 == s2
  (PartialComponentId s1) == (PartialComponentId s2) = s1 == s2
  _ == _ = False

instance Ord ComponentId where
  compare (ComponentId c1 s1 _) (ComponentId c2 s2 _) = compare (c1, s1) (c2, s2)
  compare (PartialComponentId s1) (PartialComponentId s2) = compare s1 s2
  compare (PartialComponentId _) _ = LT
  compare _ (PartialComponentId _) = GT


-- | Construct a 'ComponentId' from a 'String'
--
-- 'mkComponentId' is the inverse to 'unComponentId'
--
-- Note: No validations are performed to ensure that the resulting
-- 'ComponentId' is valid
--
-- @since 2.0.0.2
mkComponentId :: HasCallStack => String -> ComponentId
mkComponentId s = case (simpleParsec s) of
    -- Just cid@ComponentId{ unitComp = c, unitId = i } | (fromShortText c) == "ghc-9.8.4", (fromShortText i) == "rts-1.0.3-cec100dd" -> trace ("### ComponentId: `" ++ (fromShortText c) ++ "' `" ++ (fromShortText i) ++ "' is a full one.\n" ++ prettyCallStack callStack) cid
    Just cid@ComponentId{} -> cid
    Just cid@PartialComponentId{} -> error $ "mkPartialComponentId: `" ++ s ++ "' is a partial component id, not a full one.\n" ++ (prettyCallStack callStack)
    _ -> error $ "Unable to parse PartialComponentId: `" ++ s ++ "'."

mkComponentId' :: HasCallStack => String -> String -> Bool -> ComponentId
-- mkComponentId' c i b | c == "ghc-9.8.4", i == "rts-1.0.3-cec100dd" = trace ("### mkComponentId': `" ++ c ++ "' `" ++ i ++ "' is a full one.\n" ++ prettyCallStack callStack) (ComponentId (toShortText c) (toShortText i) b)
mkComponentId' c i b = ComponentId (toShortText c) (toShortText i) b

mkPartialComponentId :: HasCallStack => String -> ComponentId
mkPartialComponentId s = PartialComponentId (toShortText s)

-- | Convert 'ComponentId' to 'String'
--
-- @since 2.0.0.2
unComponentId :: HasCallStack => ComponentId -> String
unComponentId (ComponentId c s False) = fromShortText c ++ '_':fromShortText s
unComponentId (ComponentId c s True) = fromShortText s
unComponentId (PartialComponentId s) = fromShortText s

-- | 'mkComponentId'
--
-- @since 2.0.0.2
instance IsString ComponentId where
  fromString = mkComponentId

instance Binary ComponentId
instance Structured ComponentId

instance Pretty ComponentId where
  pretty = text . unComponentId

instance Parsec ComponentId where
  parsec = P.try (mkComponentId' <$> compid <* P.char '_' <*> P.munch1 abi_char <*> return False)
        <|> mkPartialComponentId <$> P.munch1 abi_char
    where
      compid = (\f v -> f ++ "-" ++ v) <$> P.munch1 isAlpha <* P.char '-' <*> P.munch1 isVerChar
      isVerChar :: Char -> Bool
      isVerChar c = c `elem` '.':['0'..'9']
      abi_char c = isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
  rnf = rnf . unComponentId
