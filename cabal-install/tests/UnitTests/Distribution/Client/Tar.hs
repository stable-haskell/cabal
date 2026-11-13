{-# LANGUAGE CPP #-}

module UnitTests.Distribution.Client.Tar (tests) where

#ifdef __MHS__
import Distribution.Client.Compat.Tar
import qualified Distribution.Client.Compat.Tar as Tar
import Distribution.Client.Tar (entriesToList, filterEntries, filterEntriesM)

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

tests :: [TestTree]
tests =
  [ testCase "filterEntries" filterTest
  , testCase "filterEntriesM" filterMTest
  , testCase "writeReadRoundTrip" roundTripTest
  , testCase "foldlEntries" foldlTest
  , testCase "toTarPath" toTarPathTest
  ]

filterTest :: Assertion
filterTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
  assertEqual "Unexpected result for filter" "xz" $
    entriesToString $
      filterEntries unpackCheck $
        Next e1 $
          Next e2 Done
  assertEqual "Unexpected result for filter" "z" $
    entriesToString $
      filterEntries unpackCheck Done
  assertEqual "Unexpected result for filter" "xf" $
    entriesToString $
      filterEntries unpackCheck $
        Next e1 $
          Next e2 $
            Fail "f"

filterMTest :: Assertion
filterMTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
  (r, w) <- runWriterT $ filterEntriesM unpackCheckM $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for filterM" "xz" $ entriesToString r
  assertEqual "Unexpected result for filterM w" "tt" w

  (r1, w1) <- runWriterT $ filterEntriesM unpackCheckM Done
  assertEqual "Unexpected result for filterM" "z" $ entriesToString r1
  assertEqual "Unexpected result for filterM w" "" w1

  (r2, w2) <- runWriterT $ filterEntriesM unpackCheckM $ Next e1 $ Next e2 $ Fail "f"
  assertEqual "Unexpected result for filterM" "xf" $ entriesToString r2
  assertEqual "Unexpected result for filterM w" "tt" w2

getFileEntry :: FilePath -> [Char] -> Entry
getFileEntry pth dta = case toTarPath False pth of
  Left err -> error ("getFileEntry: " ++ err)
  Right tp -> fileEntry tp dta'
  where
    dta' = BS.Char8.pack dta

roundTripTest :: Assertion
roundTripTest = do
  dirTp <- case toTarPath True "pkg-1.0/sub" of
    Left err -> assertFailure ("toTarPath: " ++ err)
    Right tp -> return tp
  let longName = "pkg-1.0/" ++ replicate 120 'l' ++ ".txt"
      entries =
        [ directoryEntry dirTp
        , getFileEntry "pkg-1.0/sub/file.txt" "hello"
        , getFileEntry "pkg-1.0/empty.txt" ""
        , getFileEntry longName "long"
        ]
      decoded = entriesToList (Tar.read (write entries))
  -- Directory entries are stored with a trailing slash; the overlong
  -- name round-trips through a GNU longname entry.
  assertEqual "round trip preserves paths" ("pkg-1.0/sub/" : ["pkg-1.0/sub/file.txt", "pkg-1.0/empty.txt", longName]) (map entryPath decoded)
  assertEqual "round trip preserves contents" ["<dir>", "hello", "", "long"] (map entryBytes decoded)
  assertEqual "round trip preserves permissions" (map entryPermissions entries) (map entryPermissions decoded)
  where
    entryBytes e = case entryContent e of
      NormalFile c _ -> BS.Char8.unpack c
      Directory -> "<dir>"
      OtherEntryType _ c _ -> BS.Char8.unpack c

foldlTest :: Assertion
foldlTest = do
  let e1 = getFileEntry "a" "x"
      e2 = getFileEntry "b" "y"
      entries = Next e1 (Next e2 Done) :: Entries String
  assertEqual "foldlEntries collects paths" (Right ["a", "b"]) $
    foldlEntries (\acc e -> acc ++ [entryPath e]) [] entries
  assertEqual "foldlEntries reports failure" (Left ("boom", ["a"])) $
    foldlEntries (\acc e -> acc ++ [entryPath e]) [] (Next e1 (Fail "boom" :: Entries String))

toTarPathTest :: Assertion
toTarPathTest = do
  assertEqual "relative path accepted" (Right (TarPath "a/b")) (toTarPath False "a/b")
  case toTarPath True "/abs" of
    Left _ -> return ()
    Right _ -> assertFailure "absolute path must be rejected"
  case toTarPath False "a/../../b" of
    Left _ -> return ()
    Right _ -> assertFailure "escaping path must be rejected"

unpackNormalFile :: Entry -> String
unpackNormalFile e = BS.Char8.unpack $ case entryContent e of
  NormalFile dta _ -> dta
  _ -> error "Invalid entryContent"

unpackCheck :: Entry -> Bool
unpackCheck e = unpackNormalFile e /= "y"

unpackCheckM :: Entry -> WriterT String IO Bool
unpackCheckM e = tell "t" >> return (unpackCheck e)

entriesToString :: Entries String -> String
entriesToString = foldEntries (\e acc -> unpackNormalFile e ++ acc) "z" id
#else
import Codec.Archive.Tar (GenEntry, foldEntries)
import Codec.Archive.Tar.Entry (simpleEntry, toTarPath)
import Distribution.Client.Tar (filterEntries, filterEntriesM)

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8

import Distribution.Client.Compat.Tar

tests :: [TestTree]
tests =
  [ testCase "filterEntries" filterTest
  , testCase "filterEntriesM" filterMTest
  ]

filterTest :: Assertion
filterTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
  assertEqual "Unexpected result for filter" "xz" $
    entriesToString $
      filterEntries unpackCheck $
        Next e1 $
          Next e2 Done
  assertEqual "Unexpected result for filter" "z" $
    entriesToString $
      filterEntries unpackCheck Done
  assertEqual "Unexpected result for filter" "xf" $
    entriesToString $
      filterEntries unpackCheck $
        Next e1 $
          Next e2 $
            Fail "f"

filterMTest :: Assertion
filterMTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
  (r, w) <- runWriterT $ filterEntriesM unpackCheckM $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for filterM" "xz" $ entriesToString r
  assertEqual "Unexpected result for filterM w" "tt" w

  (r1, w1) <- runWriterT $ filterEntriesM unpackCheckM Done
  assertEqual "Unexpected result for filterM" "z" $ entriesToString r1
  assertEqual "Unexpected result for filterM w" "" w1

  (r2, w2) <- runWriterT $ filterEntriesM unpackCheckM $ Next e1 $ Next e2 $ Fail "f"
  assertEqual "Unexpected result for filterM" "xf" $ entriesToString r2
  assertEqual "Unexpected result for filterM w" "tt" w2

getFileEntry :: FilePath -> [Char] -> Entry
getFileEntry pth dta =
  simpleEntry tp $ NormalFile dta' $ BS.length dta'
  where
    tp = case toTarPath False pth of
      Right tp' -> tp'
      Left e -> error e
    dta' = BS.Char8.pack dta

unpackNormalFile :: GenEntry BS.ByteString tarPath linkTarget -> String
unpackNormalFile e = BS.Char8.unpack $ case entryContent e of
  NormalFile dta _ -> dta
  _ -> error "Invalid entryContent"

unpackCheck :: GenEntry BS.ByteString tarPath linkTarget -> Bool
unpackCheck e = unpackNormalFile e /= "y"

unpackCheckM :: GenEntry BS.ByteString tarPath linkTarget -> WriterT String IO Bool
unpackCheckM e = tell "t" >> return (unpackCheck e)

entriesToString :: Entries String -> String
entriesToString = foldEntries (\e acc -> unpackNormalFile e ++ acc) "z" id
#endif
