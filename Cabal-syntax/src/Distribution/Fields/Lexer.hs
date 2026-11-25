-- This file is pregenerated from Distribution.Fields.Lexer
-- (Cabal-syntax/src/Distribution/Fields/Lexer.x) WITHOUT the alex -g flag,
-- for compilers that cannot parse GHC-specific extensions (mhs).
-- Do not edit it directly; regenerate with:
--   alex src/Distribution/Fields/Lexer.x -o src/Distribution/Fields/Lexer.hs
-- (run from the Cabal-syntax directory, with the alex version pinned in
-- Cabal-syntax.cabal build-tool-depends; alex 3.5.4.2 reproduces the
-- build-tree GHC -g output byte-for-byte).
{- FOURMOLU_DISABLE -}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "src/Distribution/Fields/Lexer.x" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Fields.Lexer
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Lexer for the cabal files.
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef CABAL_PARSEC_DEBUG
{-# LANGUAGE PatternGuards #-}
#endif
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Distribution.Fields.Lexer
  (ltest, lexToken, Token(..), LToken(..)
  ,bol_section, in_section, in_field_layout, in_field_braces
  ,mkLexState) where

import Prelude ()
import qualified Prelude
import Distribution.Compat.Prelude

import Distribution.Fields.LexerMonad
import Distribution.Parsec.Position (Position (..), incPos, retPos)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B.Char8
import qualified Data.Word as Word

#ifdef CABAL_PARSEC_DEBUG
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
#endif
#include "ghcconfig.h"
import qualified Data.Array
import qualified Data.Char
alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Data.Array.Array Int Int
alex_base = Data.Array.listArray (0 :: Int, 48)
  [ -238
  , -7
  , -5
  , 1
  , 47
  , 80
  , 208
  , 0
  , 336
  , 372
  , 0
  , 0
  , 0
  , 0
  , -3
  , 0
  , 3
  , 9
  , 129
  , -1
  , 0
  , 10
  , 499
  , 0
  , 0
  , 5
  , 0
  , 531
  , -170
  , -140
  , -139
  , 248
  , 250
  , 0
  , -169
  , 0
  , 13
  , 566
  , -21
  , 264
  , -134
  , 0
  , 0
  , 106
  , 649
  , 694
  , 822
  , 950
  , 986
  ]

alex_table :: Data.Array.Array Int Int
alex_table = Data.Array.listArray (0 :: Int, 1241)
  [ 0
  , 28
  , 32
  , 35
  , 2
  , 35
  , 36
  , 15
  , 36
  , 20
  , 3
  , 35
  , 16
  , 35
  , 36
  , 26
  , 36
  , 34
  , 17
  , 21
  , 16
  , 31
  , 33
  , 35
  , 37
  , 32
  , 39
  , 2
  , 0
  , 0
  , 0
  , 0
  , 0
  , 3
  , 0
  , 16
  , 0
  , 0
  , 38
  , 0
  , 38
  , 17
  , 21
  , 0
  , 0
  , 0
  , 38
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 21
  , 26
  , -1
  , -1
  , 25
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 21
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 17
  , 20
  , -1
  , -1
  , 19
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 17
  , 0
  , 0
  , 43
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 41
  , 0
  , 42
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 43
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 23
  , 0
  , 24
  , 0
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 30
  , 0
  , 29
  , 0
  , 0
  , 0
  , 0
  , 0
  , 40
  , 0
  , 29
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 43
  , 15
  , -1
  , -1
  , 14
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 43
  , 9
  , 27
  , 9
  , 9
  , 9
  , 9
  , 0
  , 10
  , 10
  , 9
  , 9
  , 9
  , 46
  , 8
  , 9
  , -1
  , 31
  , 35
  , 32
  , 35
  , 36
  , 0
  , 36
  , 0
  , 0
  , 11
  , 0
  , 9
  , 9
  , 9
  , 9
  , 9
  , 39
  , 35
  , 0
  , 0
  , 36
  , 0
  , 0
  , 31
  , 0
  , 32
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 38
  , 39
  , 0
  , 0
  , 10
  , 9
  , 10
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 12
  , 9
  , 13
  , 9
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 9
  , -1
  , 9
  , 9
  , 9
  , 9
  , 0
  , -1
  , -1
  , 9
  , 9
  , 9
  , 8
  , 8
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 0
  , 9
  , 9
  , 9
  , 9
  , 9
  , 0
  , 0
  , 0
  , 0
  , 9
  , 0
  , 9
  , 9
  , 9
  , 9
  , 0
  , 0
  , 0
  , 9
  , 9
  , 9
  , 9
  , 9
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 9
  , -1
  , 9
  , 0
  , 9
  , 9
  , 9
  , 9
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 30
  , 0
  , 30
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 40
  , -1
  , 9
  , -1
  , 9
  , -1
  , 9
  , 0
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 9
  , 0
  , 9
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 7
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 48
  , -1
  , 0
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 9
  , -1
  , 9
  , 9
  , 9
  , 9
  , 0
  , -1
  , -1
  , 9
  , 9
  , 9
  , 44
  , 8
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 0
  , 9
  , 9
  , 9
  , 9
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 9
  , -1
  , 9
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 9
  , -1
  , 9
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 7
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 47
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 48
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 48
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -1
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Data.Array.Array Int Int
alex_check = Data.Array.listArray (0 :: Int, 1241)
  [ -1
  , 239
  , 9
  , 10
  , 9
  , 10
  , 13
  , 10
  , 13
  , 10
  , 9
  , 10
  , 9
  , 10
  , 13
  , 10
  , 13
  , 187
  , 9
  , 9
  , 160
  , 160
  , 191
  , 10
  , 45
  , 32
  , 160
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , 32
  , -1
  , -1
  , 45
  , -1
  , 45
  , 32
  , 32
  , -1
  , -1
  , -1
  , 45
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , -1
  , -1
  , 9
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 123
  , -1
  , 125
  , -1
  , -1
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 32
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 123
  , -1
  , 125
  , -1
  , 127
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 194
  , -1
  , 194
  , -1
  , -1
  , -1
  , -1
  , -1
  , 194
  , -1
  , 194
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 127
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , -1
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 127
  , 9
  , 10
  , 9
  , 10
  , 13
  , -1
  , 13
  , -1
  , -1
  , 58
  , -1
  , 60
  , 61
  , 62
  , 63
  , 64
  , 9
  , 10
  , -1
  , -1
  , 13
  , -1
  , -1
  , 32
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 45
  , 32
  , -1
  , -1
  , 91
  , 92
  , 93
  , 94
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 123
  , 124
  , 125
  , 126
  , 127
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , -1
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 58
  , -1
  , 60
  , 61
  , 62
  , 63
  , 64
  , -1
  , -1
  , -1
  , -1
  , 33
  , -1
  , 35
  , 36
  , 37
  , 38
  , -1
  , -1
  , -1
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 91
  , 92
  , 93
  , 94
  , -1
  , 60
  , 61
  , 62
  , 63
  , 64
  , -1
  , -1
  , -1
  , -1
  , -1
  , 194
  , -1
  , 194
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 194
  , 123
  , 124
  , 125
  , 126
  , 127
  , 92
  , -1
  , 94
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 124
  , -1
  , 126
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , -1
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , -1
  , -1
  , 34
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , -1
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 123
  , 92
  , 125
  , -1
  , 127
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 127
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 127
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , -1
  , 40
  , 41
  , 42
  , 43
  , 44
  , -1
  , -1
  , 47
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 58
  , -1
  , 60
  , 61
  , 62
  , 63
  , 64
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 127
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 91
  , 92
  , 93
  , 94
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 123
  , 124
  , 125
  , 126
  , 127
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , -1
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 58
  , -1
  , 60
  , 61
  , 62
  , 63
  , 64
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 91
  , 92
  , 93
  , 94
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 123
  , 124
  , 125
  , 126
  , 127
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , -1
  , -1
  , 34
  , -1
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , -1
  , -1
  , 34
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 92
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 127
  , 92
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 127
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Data.Array.Array Int Int
alex_deflt = Data.Array.listArray (0 :: Int, 48)
  [ -1
  , -1
  , -1
  , -1
  , 22
  , 18
  , 45
  , -1
  , 45
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 18
  , -1
  , -1
  , -1
  , 22
  , -1
  , -1
  , -1
  , -1
  , 27
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 37
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 44
  , 45
  , 45
  , 27
  , 27
  ]

alex_accept = Data.Array.listArray (0 :: Int, 48)
  [ AlexAcc 30
  , AlexAcc 29
  , AlexAcc 28
  , AlexAcc 27
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 26
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAcc 22
  , AlexAcc 21
  , AlexAcc 20
  , AlexAcc 19
  , AlexAcc 18
  , AlexAcc 17
  , AlexAccSkip
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAccSkip
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 8
  , AlexAccNone
  , AlexAcc 7
  , AlexAcc 6
  , AlexAccSkip
  , AlexAccNone
  , AlexAcc 5
  , AlexAccNone
  , AlexAcc 4
  , AlexAcc 3
  , AlexAccSkip
  , AlexAccSkip
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  , AlexAccNone
  ]

alex_actions = Data.Array.array (0 :: Int, 31)
  [ (30,alex_action_0)
  , (29,alex_action_20)
  , (28,alex_action_16)
  , (27,alex_action_3)
  , (26,alex_action_9)
  , (25,alex_action_8)
  , (24,alex_action_10)
  , (23,alex_action_11)
  , (22,alex_action_12)
  , (21,alex_action_13)
  , (20,alex_action_14)
  , (19,alex_action_15)
  , (18,alex_action_15)
  , (17,alex_action_16)
  , (16,alex_action_18)
  , (15,alex_action_19)
  , (14,alex_action_19)
  , (13,alex_action_22)
  , (12,alex_action_23)
  , (11,alex_action_24)
  , (10,alex_action_25)
  , (9,alex_action_25)
  , (8,alex_action_0)
  , (7,alex_action_1)
  , (6,alex_action_1)
  , (5,alex_action_3)
  , (4,alex_action_4)
  , (3,alex_action_5)
  , (2,alex_action_8)
  , (1,alex_action_8)
  , (0,alex_action_9)
  ]


bol_field_braces,bol_field_layout,bol_section,in_field_braces,in_field_layout,in_section :: Int
bol_field_braces = 1
bol_field_layout = 2
bol_section = 3
in_field_braces = 4
in_field_layout = 5
in_section = 6
alex_action_0 = \pos len _ -> do
              when (len /= 0) $ addWarningAt pos LexWarningBOM
              setPos pos -- reset position as if BOM didn't exist
              setStartCode bol_section
              lexToken
alex_action_1 = \pos len inp -> checkWhitespace pos len inp >> adjustPos retPos >> lexToken
alex_action_3 = \pos len inp -> checkLeadingWhitespace pos len inp >>= \len' ->
                                     -- len' is character whitespace length (counting nbsp as one)
                                     if B.length inp == len
                                       then return (L pos EOF)
                                       else do
                                        -- Small hack: if char and byte length mismatch
                                        -- subtract the difference, so lexToken will count position correctly.
                                        -- Proper (and slower) fix is to count utf8 length in lexToken
                                        when (len' /= len) $ adjustPos (incPos (len' - len))
                                        setStartCode in_section
                                        return (L pos (Indent len'))
alex_action_4 = tok  OpenBrace
alex_action_5 = tok  CloseBrace
alex_action_8 = toki TokSym
alex_action_9 = \pos len inp -> return $! L pos (TokStr (B.take (len - 2) (B.tail inp)))
alex_action_10 = toki TokOther
alex_action_11 = toki TokOther
alex_action_12 = tok  Colon
alex_action_13 = tok  OpenBrace
alex_action_14 = tok  CloseBrace
alex_action_15 = \_ _ _ -> adjustPos retPos >> setStartCode bol_section >> lexToken
alex_action_16 = \pos len inp -> checkLeadingWhitespace pos len inp >>= \len' ->
                                  if B.length inp == len
                                    then return (L pos EOF)
                                    else do
                                      -- Small hack: if char and byte length mismatch
                                      -- subtract the difference, so lexToken will count position correctly.
                                      -- Proper (and slower) fix is to count utf8 length in lexToken
                                      when (len' /= len) $ adjustPos (incPos (len' - len))
                                      setStartCode in_field_layout
                                      return (L pos (Indent len'))
alex_action_18 = toki TokFieldLine
alex_action_19 = \_ _ _ -> adjustPos retPos >> setStartCode bol_field_layout >> lexToken
alex_action_20 = \_ _ _ -> setStartCode in_field_braces >> lexToken
alex_action_22 = toki TokFieldLine
alex_action_23 = tok  OpenBrace
alex_action_24 = tok  CloseBrace
alex_action_25 = \_ _ _ -> adjustPos retPos >> setStartCode bol_field_braces >> lexToken

#define ALEX_LATIN1 1
#define ALEX_NOPRED 1
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

#ifdef ALEX_GHC
#  define ILIT(n) n#
#  define IBOX(n) (I# (n))
#  define FAST_INT Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#  if __GLASGOW_HASKELL__ > 706
#    define CMP_GEQ(n,m) (((n) >=# (m)) :: Int#)
#    define CMP_EQ(n,m) (((n) ==# (m)) :: Int#)
#    define CMP_MKBOOL(x) ((GHC.Exts.tagToEnum# (x)) :: Bool)
#  else
#    define CMP_GEQ(n,m) (((n) >= (m)) :: Bool)
#    define CMP_EQ(n,m) (((n) == (m)) :: Bool)
#    define CMP_MKBOOL(x) ((x) :: Bool)
#  endif
#  define GTE(n,m) CMP_MKBOOL(CMP_GEQ(n,m))
#  define EQ(n,m) CMP_MKBOOL(CMP_EQ(n,m))
#  define PLUS(n,m) (n +# m)
#  define MINUS(n,m) (n -# m)
#  define TIMES(n,m) (n *# m)
#  define NEGATE(n) (negateInt# (n))
#  define IF_GHC(x) (x)
#else
#  define ILIT(n) (n)
#  define IBOX(n) (n)
#  define FAST_INT Int
#  define GTE(n,m) (n >= m)
#  define EQ(n,m) (n == m)
#  define PLUS(n,m) (n + m)
#  define MINUS(n,m) (n - m)
#  define TIMES(n,m) (n * m)
#  define NEGATE(n) (negate (n))
#  define IF_GHC(x)
#endif

#ifdef ALEX_GHC
data AlexAddr = AlexA# Addr#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt16OffAddr (AlexA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  GHC.Exts.int16ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  (GHC.Exts.word16ToInt16# (GHC.Exts.wordToWord16# (GHC.Exts.byteSwap16# (GHC.Exts.word16ToWord# (GHC.Exts.int16ToWord16#
#endif
  (indexInt16OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif
#else
alexIndexInt16OffAddr = (Data.Array.!)
#endif

#ifdef ALEX_GHC
{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt32OffAddr (AlexA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  GHC.Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  (GHC.Exts.word32ToInt32# (GHC.Exts.wordToWord32# (GHC.Exts.byteSwap32# (GHC.Exts.word32ToWord# (GHC.Exts.int32ToWord32#
#endif
  (indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif
#else
alexIndexInt32OffAddr = (Data.Array.!)
#endif

#ifdef ALEX_GHC
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#else
quickIndex = (Data.Array.!)
#endif

-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ IBOX(sc)
  = alexScanUser (error "alex rule requiring context was invoked by alexScan; use alexScanUser instead?") input__ IBOX(sc)

-- If the generated alexScan/alexScanUser functions are called multiple times
-- in the same file, alexScanUser gets broken out into a separate function and
-- increases memory usage. Make sure GHC inlines this function and optimizes it.
{-# INLINE alexScanUser #-}

alexScanUser user__ input__ IBOX(sc)
  = case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->
#ifdef ALEX_DEBUG
                                   Debug.Trace.trace ("End of input.") $
#endif
                                   AlexEOF
      Just _ ->
#ifdef ALEX_DEBUG
                                   Debug.Trace.trace ("Error.") $
#endif
                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->
#ifdef ALEX_DEBUG
    Debug.Trace.trace ("Skipping.") $
#endif
    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->
#ifdef ALEX_DEBUG
    Debug.Trace.trace ("Accept.") $
#endif
    AlexToken input__''' len ((Data.Array.!) alex_actions k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` IBOX(s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->
#ifdef ALEX_DEBUG
      Debug.Trace.trace ("State: " ++ show IBOX(s) ++ ", char: " ++ show c ++ " " ++ (show . Data.Char.chr . fromIntegral) c) $
#endif
      case fromIntegral c of { IBOX(ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = PLUS(base,ord_c)

                new_s = if GTE(offset,ILIT(0))
                          && let check  = alexIndexInt16OffAddr alex_check offset
                             in  EQ(check,ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            ILIT(-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input
#ifdef ALEX_LATIN1
                   PLUS(len,ILIT(1))
                   -- issue 119: in the latin1 encoding, *each* byte is one character
#else
                   (if c < 0x80 || c >= 0xC0 then PLUS(len,ILIT(1)) else len)
                   -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
#endif
                   new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ IBOX(len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ IBOX(len)
#ifndef ALEX_NOPRED
        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastAcc a input__ IBOX(len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastSkip input__ IBOX(len)
           | otherwise
           = check_accs rest
#endif

data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip
#ifndef ALEX_NOPRED
  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr Data.Array.! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext IBOX(sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
#endif
{-# LINE 153 "src/Distribution/Fields/Lexer.x" #-}
-- | Tokens of outer cabal file structure. Field values are treated opaquely.
data Token = TokSym   !ByteString       -- ^ Haskell-like identifier, number or operator
           | TokStr   !ByteString       -- ^ String in quotes
           | TokOther !ByteString       -- ^ Operators and parens
           | Indent   !Int              -- ^ Indentation token
           | TokFieldLine !ByteString   -- ^ Lines after @:@
           | Colon
           | OpenBrace
           | CloseBrace
           | EOF
           | LexicalError InputStream --TODO: add separate string lexical error
  deriving Show

data LToken = L !Position !Token
  deriving Show

toki :: (ByteString -> Token) -> Position -> Int -> ByteString -> Lex LToken
toki t pos  len  input = return $! L pos (t (B.take len input))

tok :: Token -> Position -> Int -> ByteString -> Lex LToken
tok  t pos _len _input = return $! L pos t

checkLeadingWhitespace :: Position -> Int -> ByteString -> Lex Int
checkLeadingWhitespace pos len bs
    | B.any (== 9) (B.take len bs) = do
        addWarningAt pos LexWarningTab
        checkWhitespace pos len bs
    | otherwise = checkWhitespace pos len bs

checkWhitespace :: Position -> Int -> ByteString -> Lex Int
checkWhitespace pos len bs
    -- UTF8 NBSP is 194 160. This function is called on whitespace bytestrings,
    -- therefore counting 194 bytes is enough to count non-breaking spaces.
    -- We subtract the amount of 194 bytes to convert bytes length into char length
    | B.any (== 194) (B.take len bs) = do
        addWarningAt pos LexWarningNBSP
        return $ len - B.count 194 (B.take len bs)
    | otherwise = return len

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = InputStream

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetByte :: AlexInput -> Maybe (Word.Word8,AlexInput)
alexGetByte = B.uncons

lexicalError :: Position -> InputStream -> Lex LToken
lexicalError pos inp = do
  setInput B.empty
  return $! L pos (LexicalError inp)

lexToken :: Lex LToken
lexToken = do
  pos <- getPos
  inp <- getInput
  st  <- getStartCode
  case alexScan inp st of
    AlexEOF -> return (L pos EOF)
    AlexError inp' ->
        let !len_bytes = B.length inp - B.length inp' in
            --FIXME: we want len_chars here really
            -- need to decode utf8 up to this point
        lexicalError (incPos len_bytes pos) inp'
    AlexSkip  inp' len_chars -> do
        checkPosition pos inp inp' len_chars
        adjustPos (incPos len_chars)
        setInput inp'
        lexToken
    AlexToken inp' len_chars action -> do
        checkPosition pos inp inp' len_chars
        adjustPos (incPos len_chars)
        setInput inp'
        let !len_bytes = B.length inp - B.length inp'
        t <- action pos len_bytes inp
        --traceShow t $ return tok
        return t


checkPosition :: Position -> ByteString -> ByteString -> Int -> Lex ()
#ifdef CABAL_PARSEC_DEBUG
checkPosition pos@(Position lineno colno) inp inp' len_chars = do
    text_lines <- getDbgText
    let len_bytes = B.length inp - B.length inp'
        pos_txt   | lineno-1 < V.length text_lines = T.take len_chars (T.drop (colno-1) (text_lines V.! (lineno-1)))
                  | otherwise = T.empty
        real_txt  = B.take len_bytes inp
    when (pos_txt /= T.decodeUtf8 real_txt) $
      traceShow (pos, pos_txt, T.decodeUtf8 real_txt) $
      traceShow (take 3 (V.toList text_lines)) $ return ()
  where
    getDbgText = Lex $ \s@LexState{ dbgText = txt } -> LexResult s txt
#else
checkPosition _ _ _ _ = return ()
#endif

lexAll :: Lex [LToken]
lexAll = do
  t <- lexToken
  case t of
    L _ EOF -> return [t]
    _       -> do ts <- lexAll
                  return (t : ts)

ltest :: Int -> String -> Prelude.IO ()
ltest code s =
  let (ws, xs) = execLexer (setStartCode code >> lexAll) (B.Char8.pack s)
   in traverse_ print ws >> traverse_ print xs


mkLexState :: ByteString -> LexState
mkLexState input = LexState
  { curPos   = Position 1 1
  , curInput = input
  , curCode  = 0
  , warnings = []
#ifdef CABAL_PARSEC_DEBUG
  , dbgText  = V.fromList . lines' . T.decodeUtf8With T.lenientDecode $ input
#endif
  }

#ifdef CABAL_PARSEC_DEBUG
lines' :: T.Text -> [T.Text]
lines' s1
  | T.null s1 = []
  | otherwise = case T.break (\c -> c == '\r' || c == '\n') s1 of
                  (l, s2) | Just (c,s3) <- T.uncons s2
                         -> case T.uncons s3 of
                              Just ('\n', s4) | c == '\r' -> l `T.snoc` '\r' `T.snoc` '\n' : lines' s4
                              _                           -> l `T.snoc` c : lines' s3

                          | otherwise
                         -> [l]
#endif
