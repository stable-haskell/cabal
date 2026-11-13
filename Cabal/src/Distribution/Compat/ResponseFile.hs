{-# LANGUAGE CPP #-}

module Distribution.Compat.ResponseFile (expandResponse, escapeArgs) where

import Distribution.Compat.Prelude

#ifndef __MHS__
import GHC.ResponseFile (escapeArgs, unescapeArgs)
#else
-- MicroHS does not provide 'GHC.ResponseFile'. The pure argument escaping
-- functions ('escapeArgs', 'unescapeArgs') are inlined below instead,
-- adapted from 'GHC.Internal.ResponseFile' (GCC style response files);
-- 'isSpace' and 'foldl'' come from 'Distribution.Compat.Prelude'. The 'IO'
-- entry point ('expandResponse') is unchanged and shared.
#endif

import Prelude ()

import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.IO.Error

-- | This is a more elaborate version of 'GHC.ResponseFile.expandResponse',
-- which not only substitute @\@foo@ with the contents of file @foo@,
-- but performs such substitution recursively.
-- In doing so we keep closer to the reference implementation
-- of @expandargv@ in @argv.c@ from @binutils@, although this additional functionality
-- likely remains unused by Haskell tooling.
expandResponse :: [String] -> IO [String]
expandResponse = go recursionLimit "."
  where
    recursionLimit = 100

    go :: Int -> FilePath -> [String] -> IO [String]
    go n dir
      | n >= 0 = fmap concat . traverse (expand n dir)
      | otherwise = const $ hPutStrLn stderr "Error: response file recursion limit exceeded." >> exitFailure

    expand :: Int -> FilePath -> String -> IO [String]
    expand n dir arg@('@' : f) = readRecursively n (dir </> f) `catchIOError` const (return [arg])
    expand _n _dir x = return [x]

    readRecursively :: Int -> FilePath -> IO [String]
    readRecursively n f = go (n - 1) (takeDirectory f) . unescapeArgs =<< readFile f

#ifdef __MHS__
-- | Given a list of strings, concatenate them into a single string with
-- escaping of certain characters, and the addition of a newline between
-- each string. The escaping is done by adding a single backslash character
-- before any whitespace, single quote, double quote, or backslash
-- character. Unescaped whitespace (in this case, newline) is part of this
-- \"transport\" format to indicate the end of the previous string and the
-- start of a new string.
--
-- While 'unescapeArgs' allows using quoting (i.e. convenient escaping of
-- many characters) by having matching sets of single- or double-quotes,
-- 'escapeArgs' does not use the quoting mechanism, and thus will always
-- escape any whitespace, quotes, and backslashes.
--
-- > escapeArgs ["hello \"world\""] == "hello\\ \\\"world\\\"\n"
escapeArgs :: [String] -> String
escapeArgs = unlines . map escapeArg

-- | Given a string of concatenated strings, separate each by removing a
-- layer of /quoting/ and\/or /escaping/ of certain characters.
--
-- These characters are: any whitespace, single quote, double quote, and
-- the backslash character. The backslash character always escapes (i.e.
-- passes through without further consideration) the character which
-- follows. Characters can also be escaped in blocks by quoting (i.e.
-- surrounding the blocks with matching pairs of either single- or
-- double-quotes which are not themselves escaped).
--
-- Any whitespace which appears outside of either of the quoting and
-- escaping mechanisms, is interpreted as having been added by this special
-- concatenation process to designate where the boundaries are between the
-- original, un-concatenated list of strings. These added whitespace
-- characters are removed from the output.
--
-- > unescapeArgs "hello\\ \\\"world\\\"\n" == ["hello \"world\""]
unescapeArgs :: String -> [String]
unescapeArgs = filter (not . null) . unescape

escapeArg :: String -> String
escapeArg = reverse . foldl' escape []

escape :: String -> Char -> String
escape cs c
  | isSpace c || c == '\\' || c == '\'' || c == '"' = c : '\\' : cs -- n.b., our caller must reverse the result
  | otherwise = c : cs

data Quoting = NoneQ | SngQ | DblQ

unescape :: String -> [String]
unescape args = reverse (map reverse (go args NoneQ False [] []))
  where
    -- n.b., the order of these cases matters; these are cribbed from gcc
    -- case 1: end of input
    go [] _ _ a as = a : as
    -- case 2: back-slash escape in progress
    go (c : cs) q True a as = go cs q False (c : a) as
    -- case 3: no back-slash escape in progress, but got a back-slash
    go (c : cs) q False a as
      | c == '\\' = go cs q True a as
    -- case 4: single-quote escaping in progress
    go (c : cs) SngQ False a as
      | c == '\'' = go cs NoneQ False a as
      | otherwise = go cs SngQ False (c : a) as
    -- case 5: double-quote escaping in progress
    go (c : cs) DblQ False a as
      | c == '"' = go cs NoneQ False a as
      | otherwise = go cs DblQ False (c : a) as
    -- case 6: no escaping is in progress
    go (c : cs) NoneQ False a as
      | isSpace c = go cs NoneQ False [] (a : as)
      | c == '\'' = go cs SngQ False a as
      | c == '"' = go cs DblQ False a as
      | otherwise = go cs NoneQ False (c : a) as
#endif
