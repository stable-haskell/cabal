{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- | This module does two things:
--
-- * Acts as a compatibility layer, like @base-compat@.
--
-- * Provides commonly used imports.
module Distribution.Compat.Prelude
  ( -- * Prelude

  --
  -- Prelude is re-exported, following is hidden:
      module BasePrelude

    -- * Common type-classes
  , Semigroup (..)
  , Typeable
  , TypeRep
  , typeRep
  , Data
#ifdef __MHS__
  , Generic (..)
#else
  , Generic
#endif
  , Generically (..)
  , NFData (..)
  , Binary (..)
  , Structured (..)
  , Alternative (..)
  , MonadPlus (..)
  , IsString (..)

    -- * Some types
  , Map
  , Set
  , NonEmptySet
  , Identity (..)
  , Proxy (..)
  , Const (..)
  , Void

    -- * Data.Either
  , partitionEithers
#ifdef __MHS__
  , lefts
  , rights
#endif

    -- * Data.Maybe
  , catMaybes
  , mapMaybe
  , fromMaybe
  , maybeToList
  , listToMaybe
  , isNothing
  , isJust

    -- * Data.List
  , unfoldr
  , isPrefixOf
  , isSuffixOf
  , intercalate
  , intersperse
  , sort
  , sortBy
  , nub
  , nubBy
  , partition
  , dropWhileEnd

    -- * Data.List.NonEmpty
  , NonEmpty ((:|))
  , nonEmpty
  , foldl1
  , foldr1
  , head
  , tail
  , last
  , init

    -- * Data.Foldable
  -- NOTE: only the class methods are re-exported here (as plain
  -- functions), never the class itself. MicroHs merges a re-exported
  -- class method with a same-named local definition ('foldr1'
  -- and 'foldl1' below) into an ambiguous export. Modules needing
  -- the class (instances, deriving) import it directly. 'fold' is
  -- deliberately not re-exported: several modules define a local
  -- 'fold' ('Distribution.Utils.Progress', 'VersionRange', 'Graph'),
  -- and every use of 'Data.Foldable.fold' imports it directly.
  , foldMap
  , foldMap'
  , foldr
  , foldr'
  , foldl
  , foldl'
  , toList
  , null
  , length
  , elem
  , maximum
  , minimum
  , sum
  , product
  , find
  , for_
  , traverse_
  , any
  , all

    -- * Data.Traversable
  , Traversable (..)
  , for

    -- * Data.Function
  , on

    -- * Data.Ord
  , comparing

    -- * Control.Arrow
  , first

    -- * Control.Monad
  , liftM
  , liftM2
  , unless
  , when
  , ap
  , void
  , foldM
  , filterM
  , join
  , guard

    -- * Control.Exception
  , catch
  , throwIO
  , evaluate
  , Exception (..)
  , IOException
  , SomeException (..)
  , tryIO
  , catchIO
  , catchExit

    -- * Control.DeepSeq
  , deepseq
  , force

    -- * Data.Char
  , isSpace
  , isDigit
  , isUpper
  , isAlpha
  , isAlphaNum
  , chr
  , ord
  , toLower
  , toUpper

    -- * Data.Void
  , absurd
  , vacuous

    -- * Data.Word & Data.Int
  , Word
  , Word8
  , Word16
  , Word32
  , Word64
  , Int8
  , Int16
  , Int32
  , Int64

    -- * Text.PrettyPrint
  , (<<>>)
  , (Disp.<+>)

    -- * System.Exit
  , ExitCode (..)
  , exitWith
  , exitSuccess
  , exitFailure

    -- * Text.Read
  , readMaybe

    -- * Debug.Trace (as deprecated functions)
  , trace
  , traceShow
  , traceShowId
  , traceM
  , traceShowM
  ) where

-- We also could hide few partial function
{- FOURMOLU_DISABLE -}
import Prelude                       as BasePrelude hiding
    ( mapM, mapM_, sequence, any, all, head, tail, last, init
    -- 'foldr' is also imported from 'Data.Foldable' below; hiding
    -- Prelude's copy keeps the export list unambiguous. (Under GHC both
    -- names denote the same entity, so this changes nothing there.)
    , foldr
    -- Same for the other list functions that 'Data.Foldable'
    -- re-exports generically below (all identical to Prelude's
    -- since the Foldable-Traversable Proposal).
    , foldl
    , length
    , null
    , elem
    , maximum
    , minimum
    , product
    , sum
    -- partial functions
    , read
    , foldr1, foldl1
    , Word
    -- We hide them, as we import only some members
    , Traversable, traverse, sequenceA
    , Foldable(..)
    )
{- FOURMOLU_ENABLE -}
-- AMP
import Data.Foldable
  -- NOTE: only the class methods are re-exported here (as plain
  -- functions), never the class itself. MicroHs merges a re-exported
  -- class method with a same-named local definition ('foldr1'
  -- and 'foldl1' below) into an ambiguous export. Modules needing
  -- the class (instances, deriving) import it directly.
  ( foldMap
  , foldMap'
  , foldr
  , foldr'
  , foldl
  , foldl'
  , toList
  , null
  , length
  , elem
  , maximum
  , minimum
  , sum
  , product
  , all
  , any
  , find
  , for_
  , traverse_
  )
import Data.Traversable (Traversable (..), for)


-- Extra exports
import Control.Applicative (Alternative (..), Const (..))
import Control.Arrow (first)
import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Exception (Exception (..), IOException, SomeException (..), catch, evaluate, throwIO)
import Control.Monad (MonadPlus (..), ap, filterM, foldM, guard, join, liftM, liftM2, unless, void, when)
import Data.Char (chr, isAlpha, isAlphaNum, isDigit, isSpace, isUpper, ord, toLower, toUpper)
import Data.Data (Data)
import Data.Either (partitionEithers)
#ifdef __MHS__
lefts :: [Either a b] -> [a]
lefts xs = [x | Left x <- xs]

rights :: [Either a b] -> [b]
rights xs = [x | Right x <- xs]
#endif
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (dropWhileEnd, intercalate, intersperse, isPrefixOf, isSuffixOf, nub, nubBy, partition, sort, sortBy, unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)), head, init, last, nonEmpty, tail)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.String (IsString (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import Data.Void (Void, absurd, vacuous)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Distribution.Compat.Binary (Binary (..))
import GHC.Generics (Generic (..), Generically (..))
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import Text.Read (readMaybe)

import qualified Text.PrettyPrint as Disp

import Distribution.Compat.Exception
import Distribution.Compat.NonEmptySet (NonEmptySet)
import Distribution.Utils.Structured (Structured (..))

import qualified Debug.Trace

#ifdef __MHS__
-- | Re-export the MicroHs compatibility instances (see
-- 'Distribution.Compat.MhsInstances') so that every module importing
-- this 'Prelude' can derive 'Data' over sets and maps.
import Distribution.Compat.MhsInstances ()
#endif

-- | New name for 'Text.PrettyPrint.<>'
(<<>>) :: Disp.Doc -> Disp.Doc -> Disp.Doc
(<<>>) = (Disp.<>)

{-# INLINE foldr1 #-}
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 f (x :| xs) = go x xs
  where
    go y [] = y
    go y (z : zs) = f y (go z zs)

{-# INLINE foldl1 #-}
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 f (x :| xs) = foldl f x xs

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

-- Functions from Debug.Trace
-- but with DEPRECATED pragma, so -Werror will scream on them.

trace :: String -> a -> a
trace = Debug.Trace.trace
{-# DEPRECATED trace "Don't leave me in the code" #-}

traceShowId :: Show a => a -> a
#ifdef __MHS__
-- MicroHs's 'Debug.Trace' has no 'traceShow'.
traceShowId x = Debug.Trace.trace (show x) x
#else
traceShowId x = Debug.Trace.traceShow x x
#endif
{-# DEPRECATED traceShowId "Don't leave me in the code" #-}

traceShow :: Show a => a -> b -> b
#ifdef __MHS__
traceShow x y = Debug.Trace.trace (show x) y
#else
traceShow = Debug.Trace.traceShow
#endif
{-# DEPRECATED traceShow "Don't leave me in the code" #-}

traceM :: Applicative f => String -> f ()
#ifdef __MHS__
-- MicroHs's 'Debug.Trace.traceM' needs 'Monad', so define the more
-- general 'Applicative' version via 'trace'.
traceM s = trace s (pure ())
#else
traceM = Debug.Trace.traceM
#endif
{-# DEPRECATED traceM "Don't leave me in the code" #-}

traceShowM :: (Show a, Applicative f) => a -> f ()
#ifdef __MHS__
-- MicroHs's 'Debug.Trace' has no 'traceShowM', and its 'traceM'
-- needs 'Monad', so go through 'trace' directly.
traceShowM x = Debug.Trace.trace (show x) (pure ())
#else
traceShowM = Debug.Trace.traceShowM
#endif
{-# DEPRECATED traceShowM "Don't leave me in the code" #-}
