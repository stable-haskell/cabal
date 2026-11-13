module UnitTests.Distribution.Compat.Directory (tests) where

import Control.Exception (IOException, try)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), isAbsolute)
import System.IO.Error (isAlreadyExistsError)

import Distribution.Compat.Directory
import Distribution.Simple.Utils (withTempDirectory)

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "removePathForcibly removes a regular file" removeFileTest
  , testCase "removePathForcibly removes a directory tree" removeTreeTest
  , testCase "removePathForcibly ignores missing paths" removeMissingTest
  , testCase "canonicalizePath tolerates missing paths" canonMissingTest
  , testCase "canonicalizePath resolves existing files" canonExistingTest
  , testCase "createDirectoryIfMissing rejects existing files" mkdirOnFileTest
  , testCase "createDirectoryIfMissing accepts existing directories" mkdirOnDirTest
  , testCase "getPermissions gates searchable on directories" permsSearchableTest
  ]

removeFileTest :: Assertion
removeFileTest =
  withTempDirectory "." "removefile-" $ \dir -> do
    let fileName = dir </> "victim.txt"
    writeFile fileName "content"
    removePathForcibly fileName
    gone <- doesFileExist fileName
    assertBool "expected regular file to be removed" (not gone)

removeTreeTest :: Assertion
removeTreeTest =
  withTempDirectory "." "removetree-" $ \dir -> do
    let sub = dir </> "sub" </> "nested"
    createDirectoryIfMissing True sub
    writeFile (sub </> "f") "x"
    removePathForcibly (dir </> "sub")
    gone <- doesDirectoryExist (dir </> "sub")
    assertBool "expected directory tree to be removed" (not gone)

removeMissingTest :: Assertion
removeMissingTest =
  withTempDirectory "." "removemissing-" $ \dir ->
    -- Must not throw.
    removePathForcibly (dir </> "does-not-exist")

canonMissingTest :: Assertion
canonMissingTest =
  withTempDirectory "." "canonmissing-" $ \dir -> do
    resolved <- canonicalizePath (dir </> "does-not-exist")
    assertBool "expected an absolute path" (isAbsolute resolved)

canonExistingTest :: Assertion
canonExistingTest =
  withTempDirectory "." "canonexisting-" $ \dir -> do
    let fileName = dir </> "present.txt"
    writeFile fileName "content"
    resolved <- canonicalizePath fileName
    assertBool "expected an absolute path" (isAbsolute resolved)
    present <- doesFileExist resolved
    assertBool "expected resolved path to exist" present

mkdirOnFileTest :: Assertion
mkdirOnFileTest =
  withTempDirectory "." "mkdironfile-" $ \dir -> do
    let fileName = dir </> "file.txt"
    writeFile fileName "content"
    result <- try (createDirectoryIfMissing False fileName) :: IO (Either IOException ())
    case result of
      Left err -> assertBool "expected AlreadyExists" (isAlreadyExistsError err)
      Right _ -> assertFailure "expected creating over a file to fail"

mkdirOnDirTest :: Assertion
mkdirOnDirTest =
  withTempDirectory "." "mkdirondir-" $ \dir -> do
    let sub = dir </> "sub"
    createDirectoryIfMissing True sub
    -- Creating it again (and non-recursively) must be a no-op.
    createDirectoryIfMissing False sub
    present <- doesDirectoryExist sub
    assertBool "expected directory to exist" present

-- | Only directories can be searchable, matching @directory@: an
-- owner-executable regular file reports @executable = True@ but
-- @searchable = False@, even when @searchable@ was requested.
permsSearchableTest :: Assertion
permsSearchableTest =
  withTempDirectory "." "permssearch-" $ \dir -> do
    let fileName = dir </> "script.sh"
        sub = dir </> "sub"
    writeFile fileName "x"
    createDirectoryIfMissing False sub
    -- Owner-executable regular file: executable but not searchable.
    -- (Record update: directory hides the Permissions constructor.)
    setPermissions fileName (emptyPermissions { readable = True, executable = True })
    fp <- getPermissions fileName
    assertBool "expected file to be executable" (executable fp)
    assertBool "expected file to not be searchable" (not (searchable fp))
    -- Requesting searchable on a file is a no-op.
    setPermissions fileName (emptyPermissions { readable = True, executable = True, searchable = True })
    fp2 <- getPermissions fileName
    assertBool "expected file to stay executable" (executable fp2)
    assertBool "expected file to stay non-searchable" (not (searchable fp2))
    -- Owner-executable directory: searchable but not executable
    -- (matching @directory@: @executable = x && not isDir@,
    -- @searchable = x && isDir@).
    setPermissions sub (emptyPermissions { readable = True, executable = True, searchable = True })
    dp <- getPermissions sub
    assertBool "expected dir to not be executable" (not (executable dp))
    assertBool "expected dir to be searchable" (searchable dp)
