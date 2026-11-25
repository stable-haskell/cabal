{-# LANGUAGE CPP #-}
-- | Utility functions providing extra context to cabal error messages

module Distribution.Solver.Modular.MessageUtils (
  allKnownExtensions,
  cutoffRange,
  mostSimilarElement,
  showUnsupportedExtension,
  showUnsupportedLanguage,
  withinRange
) where

import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Distribution.Pretty (prettyShow) -- from Cabal
import Language.Haskell.Extension
         ( Extension(..), Language(..), knownLanguages, knownExtensions )
#ifdef __MHS__
-- MicroHs has no edit-distance package; provide a small local
-- Levenshtein implementation preserving solver behavior.
#else
import Text.EditDistance ( defaultEditCosts, levenshteinDistance )
#endif

showUnsupportedExtension :: Extension -> String
showUnsupportedExtension (UnknownExtension extStr) = formatMessage cutoffRange "extension" extStr (mostSimilarElement extStr allKnownExtensions)
showUnsupportedExtension extension = unwords [prettyShow extension, "which is not supported"]

showUnsupportedLanguage :: Language -> String
showUnsupportedLanguage (UnknownLanguage langStr) = formatMessage cutoffRange "language" langStr (mostSimilarElement langStr (show <$> knownLanguages))
showUnsupportedLanguage knownLanguage = unwords [prettyShow knownLanguage, "which is not supported"]

allKnownExtensions :: [String]
allKnownExtensions = enabledExtensions ++ disabledExtensions
  where
    enabledExtensions = map (prettyShow . EnableExtension) knownExtensions
    disabledExtensions =  map (prettyShow . DisableExtension) knownExtensions

-- Measure the Levenshtein distance between two strings
#ifdef __MHS__
defaultEditCosts :: ()
defaultEditCosts = ()

levenshteinDistance :: () -> String -> String -> Int
levenshteinDistance _ = lev
  where
    lev s t = table !! ls !! lt
      where
        ls = length s
        lt = length t
        table = [[cell i j | j <- [0 .. lt]] | i <- [0 .. ls]]
        cell 0 j = j
        cell i 0 = i
        cell i j =
          minimum
            [ table !! (i - 1) !! j + 1
            , table !! i !! (j - 1) + 1
            , table !! (i - 1) !! (j - 1) + if s !! (i - 1) == t !! (j - 1) then 0 else 1
            ]

distance :: String -> String -> Int
distance = levenshteinDistance defaultEditCosts
#else
distance :: String -> String -> Int
distance = levenshteinDistance defaultEditCosts
#endif

-- Given an `unknownElement` and a list of `elements` return the element
-- from the list with the closest Levenshtein distance to the `unknownElement`
mostSimilarElement :: String -> [String] -> String
mostSimilarElement unknownElement elements = fst . minimumBy (comparing snd) . map mapDist $ elements
  where
    mapDist element = (element, distance unknownElement element)

-- Cutoff range for giving a suggested spelling
cutoffRange :: Int
cutoffRange = 10

formatMessage :: Int -> String -> String -> String -> String
formatMessage range elementType element suggestion
  | withinRange range element suggestion =
    unwords ["unknown", elementType, element ++ ";", "did you mean", suggestion ++ "?"]
  | otherwise = unwords ["unknown", elementType, element]

-- Check whether the strings are within cutoff range
withinRange :: Int -> String -> String -> Bool
withinRange range element suggestion = distance element suggestion <= range
