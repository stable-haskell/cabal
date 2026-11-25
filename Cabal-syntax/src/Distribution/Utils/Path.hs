{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.Utils.Path
  ( -- * Symbolic path endpoints
    FileOrDir
  , File
  , Dir
  , AllowAbsolute
  , OnlyRelative

    -- ** Abstract directory locations
  , CWD
  , Pkg
  , Dist
  , Source
  , Include
  , Lib
  , Framework
  , Build
  , Artifacts
  , PkgDB
  , DataDir
  , Mix
  , Tix
  , Tmp
  , Response
  , PkgConf

    -- * Symbolic paths
  , RelativePath
  , SymbolicPath
  , AbsolutePath (..)
  , SymbolicPathX -- NB: constructor not exposed, to retain type safety.
  , FileLike (..)
  , PathLike (..)

    -- ** Symbolic path API
  , getSymbolicPath
  , getAbsolutePath
  , sameDirectory
  , makeRelativePathEx
  , makeSymbolicPath
  , unsafeMakeSymbolicPath
  , coerceSymbolicPath
  , unsafeCoerceSymbolicPath
  , relativeSymbolicPath
  , symbolicPathRelative_maybe
  , interpretSymbolicPath
  , interpretSymbolicPathAbsolute

    -- ** General filepath API
  , takeDirectorySymbolicPath
  , dropExtensionsSymbolicPath
  , replaceExtensionSymbolicPath
  , normaliseSymbolicPath
  , relativePathMaybe

    -- ** Working directory handling
  , interpretSymbolicPathCWD
  , absoluteWorkingDir
  , tryMakeRelative

    -- ** Module names
  , moduleNameSymbolicPath
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Coerce
import Data.Kind (Type)

import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
  ( toFilePath
  )
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (isAbsoluteOnAnyPlatform)

import qualified Distribution.Compat.CharParsing as P

import qualified Distribution.Compat.Directory as Directory
import qualified System.FilePath as FilePath

import Data.List
  ( stripPrefix
  )
#ifdef __MHS__
-- MicroHs has no 'GHC.Stack'; the 'HasCallStack' constraint on
-- 'makeRelativePathEx' is vestigial there.
class HasCallStack
instance HasCallStack
#else
import GHC.Stack
  ( HasCallStack
  )
#endif

-------------------------------------------------------------------------------

-- * SymbolicPath

-------------------------------------------------------------------------------

{- Note [Symbolic paths]
~~~~~~~~~~~~~~~~~~~~~~~~
We want functions within the Cabal library to support getting the working
directory from their arguments, rather than retrieving it from the current
directory, which depends on the the state of the current process
(via getCurrentDirectory).

With such a constraint, to ensure correctness we need to know, for each relative
filepath, whether it is relative to the passed in working directory or to the
current working directory. We achieve this with the following API:

  - newtype SymbolicPath from to
  - getSymbolicPath :: SymbolicPath from to -> FilePath
  - interpretSymbolicPath
      :: Maybe (SymbolicPath CWD (Dir from)) -> SymbolicPath from to -> FilePath

Note that, in the type @SymbolicPath from to@, @from@ is the name of a directory,
whereas @to@ is either @Dir toDir@ or @File@. For example, a source directory
typically has the type @SymbolicPath Pkg (Dir Source)@, while a source
file has a type such as @SymbolicPath "Source" File@.

Here, a symbolic path refers to an **uninterpreted** file path, i.e. any
passed in working directory **has not** been taken into account.
Whenever we see a symbolic path, it is a sign we must take into account this
working directory in some way.
Thus, whenever we interact with the file system, we do the following:

  - in a direct interaction (e.g. `doesFileExist`), we interpret the
    path relative to a working directory argument, e.g.

      doCheck :: Maybe (SymbolicPath CWD (Dir from))
              -> SymbolicPath from File
              -> Bool
      doCheck mbWorkDir file = doesFileExist $ interpretSymbolicPath mbWorkDir file

  - when invoking a sub-process (such as GHC), we ensure that the working directory
    of the sub-process is the same as the passed-in working directory, in which
    case we interpret the symbolic paths by using `interpretSymbolicPathCWD`:

      callGhc :: Maybe (SymbolicPath CWD (Dir Pkg))
              -> SymbolicPath (Dir Pkg) File
              -> IO ()
      callGhc mbWorkDir inputFile =
        runProgramInvocation $
          programInvocationCwd mbWorkDir ghcProg [interpretSymbolicPathCWD inputFile]

In practice, we often use:

  -- Interpret a symbolic path with respect to the working directory argument
  -- @'mbWorkDir' :: Maybe (SymbolicPath CWD (Dir Pkg))@.
  i :: SymbolicPath Pkg to -> FilePath
  i = interpretSymbolicPath mbWorkDir

  -- Interpret a symbolic path, provided that the current working directory
  -- is the package directory.
  u :: SymbolicPath Pkg to -> FilePath
  u = interpretSymbolicPathCWD

Note [Symbolic relative paths]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines plain phantom marker types (no @DataKinds@
promotion, so this module stays parsable by MicroHs):

  data File
  data Dir (dir :: Type)
  data AllowAbsolute
  data OnlyRelative

  newtype SymbolicPathX (allowAbsolute :: Type) (from :: Type) (to :: Type)
    = SymbolicPath FilePath

  type RelativePath = SymbolicPathX OnlyRelative
  type SymbolicPath = SymbolicPathX AllowAbsolute

A 'SymbolicPath' is thus a symbolic path that is allowed to be absolute, whereas
a 'RelativePath' is a symbolic path that is additionally required to be relative.

This distinction allows us to keep track of which filepaths must be kept
relative.
-}

-- | Phantom marker for 'SymbolicPathX': the path refers to a file.
data File deriving (Data)

-- | Phantom marker for 'SymbolicPathX': the path refers to a
-- directory or category of directories, e.g. the package directory
-- or a source directory. The parameter names which directory.
{- FOURMOLU_DISABLE -}
data Dir (dir :: Type)
#ifndef __MHS__
  deriving Data
#endif
{- FOURMOLU_ENABLE -}

#ifdef __MHS__
-- MicroHs cannot derive 'Data' for parameterised types (it drops the
-- required context), so the instance is spelled out explicitly.
deriving instance Data dir => Data (Dir dir)
#endif

-- | Phantom marker for 'SymbolicPathX': the path may be absolute,
-- or it may be relative.
data AllowAbsolute deriving (Data)

-- | Phantom marker for 'SymbolicPathX': the path must be relative.
data OnlyRelative deriving (Data)

-- | Compatibility shim: 'FileOrDir' used to be a datatype whose
-- constructors were promoted to the type level with @DataKinds@.
-- The markers are now the plain types above; this empty datatype is
-- kept so that existing @FileOrDir (..)@ imports keep working.
data FileOrDir deriving (Data)

-- | A symbolic path, possibly relative to an abstract location specified
-- by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
{- FOURMOLU_DISABLE -}
newtype SymbolicPathX (allowAbsolute :: Type) (from :: Type) (to :: Type)
  = SymbolicPath FilePath
  deriving (Generic, Show, Read, Eq, Ord
#ifndef __MHS__
    , Data
#endif
    )
{- FOURMOLU_ENABLE -}

#ifdef __MHS__
-- MicroHs cannot derive 'Data' for parameterised types (it drops the
-- required context), so the instance is spelled out explicitly.
deriving instance
  (Data allowAbsolute, Data from, Data to) =>
  Data (SymbolicPathX allowAbsolute from to)
#endif

type role SymbolicPathX nominal nominal nominal

-- | A symbolic relative path, relative to an abstract location specified
-- by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type RelativePath = SymbolicPathX OnlyRelative

-- | A symbolic path which is allowed to be absolute.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type SymbolicPath = SymbolicPathX AllowAbsolute

newtype AbsolutePath to = AbsolutePath (forall (from :: Type). SymbolicPath from to)

unsafeMakeAbsolutePath :: FilePath -> AbsolutePath to
unsafeMakeAbsolutePath fp = AbsolutePath (makeSymbolicPath fp)

instance Binary (SymbolicPathX allowAbsolute from to)
instance
  (Typeable allowAbsolute, Typeable from, Typeable to)
  => Structured (SymbolicPathX allowAbsolute from to)
instance NFData (SymbolicPathX allowAbsolute from to)

-- | Extract the 'FilePath' underlying a 'SymbolicPath' or 'RelativePath',
-- without interpreting it.
--
-- Use this function e.g. to validate the underlying filepath.
--
-- When interacting with the file system, you should instead use
-- 'interpretSymbolicPath' or 'interpretSymbolicPathCWD'.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
getSymbolicPath :: SymbolicPathX allowAbsolute from to -> FilePath
getSymbolicPath (SymbolicPath p) = p

-- | A symbolic path from a directory to itself.
sameDirectory :: SymbolicPathX allowAbsolute from (Dir to)
sameDirectory = SymbolicPath "."

-- | Make a 'RelativePath', ensuring the path is not absolute,
-- but performing no further checks.
makeRelativePathEx :: HasCallStack => FilePath -> RelativePath from to
makeRelativePathEx fp
  | isAbsoluteOnAnyPlatform fp =
      error $ "Cabal internal error: makeRelativePathEx: absolute path " ++ fp
  | otherwise =
      SymbolicPath fp

-- | Make a 'SymbolicPath', which may be relative or absolute.
makeSymbolicPath :: FilePath -> SymbolicPath from to
makeSymbolicPath fp = SymbolicPath fp

-- | Make a 'SymbolicPath' which may be relative or absolute,
-- without performing any checks.
--
-- Avoid using this function in new code.
unsafeMakeSymbolicPath :: FilePath -> SymbolicPathX allowAbs from to
unsafeMakeSymbolicPath fp = SymbolicPath fp

-- | Like 'System.FilePath.takeDirectory', for symbolic paths.
takeDirectorySymbolicPath :: SymbolicPathX allowAbsolute from File -> SymbolicPathX allowAbsolute from (Dir to')
takeDirectorySymbolicPath (SymbolicPath fp) = SymbolicPath (FilePath.takeDirectory fp)

-- | Like 'System.FilePath.dropExtensions', for symbolic paths.
dropExtensionsSymbolicPath :: SymbolicPathX allowAbsolute from File -> SymbolicPathX allowAbsolute from File
dropExtensionsSymbolicPath (SymbolicPath fp) = SymbolicPath (FilePath.dropExtensions fp)

-- | Like 'System.FilePath.replaceExtension', for symbolic paths.
replaceExtensionSymbolicPath :: SymbolicPathX allowAbsolute from File -> String -> SymbolicPathX allowAbsolute from File
replaceExtensionSymbolicPath (SymbolicPath fp) ext = SymbolicPath (FilePath.replaceExtension fp ext)

-- | Like 'System.FilePath.normalise', for symbolic paths.
normaliseSymbolicPath :: SymbolicPathX allowAbsolute from to -> SymbolicPathX allowAbsolute from to
normaliseSymbolicPath (SymbolicPath fp) = SymbolicPath (FilePath.normalise fp)

-- | Retrieve the relative symbolic path to a Haskell module.
moduleNameSymbolicPath :: ModuleName -> SymbolicPathX allowAbsolute Source File
moduleNameSymbolicPath modNm = SymbolicPath $ ModuleName.toFilePath modNm

-- | Interpret a symbolic path with respect to the given directory.
--
-- Use this function before directly interacting with the file system in order
-- to take into account a working directory argument.
--
-- NB: when invoking external programs (such as @GHC@), it is preferable to set
-- the working directory of the process and use 'interpretSymbolicPathCWD'
-- rather than calling this function, as this function will turn relative paths
-- into absolute paths if the working directory is an absolute path.
-- This can degrade error messages, or worse, break the behaviour entirely
-- (because the program might expect certain paths to be relative).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
interpretSymbolicPath :: Maybe (SymbolicPath CWD (Dir from)) -> SymbolicPathX allowAbsolute from to -> FilePath
interpretSymbolicPath mbWorkDir (SymbolicPath p) =
  -- Note that this properly handles an absolute symbolic path,
  -- because if @q@ is absolute, then @p </> q = q@.
  maybe p ((</> p) . getSymbolicPath) mbWorkDir

-- | Interpret a symbolic path, **under the assumption that the working
-- directory is the package directory**.
--
-- Use 'interpretSymbolicPath' instead if you need to take into account a
-- working directory argument before directly interacting with the file system.
--
-- Use this function instead of 'interpretSymbolicPath' when invoking a child
-- process: set the working directory of the sub-process, and use this function,
-- e.g.:
--
-- > callGhc :: Maybe (SymbolicPath CWD (Dir Pkg))
-- >         -> SymbolicPath (Dir Pkg) File
-- >         -> IO ()
-- > callGhc mbWorkDir inputFile =
-- >   runProgramInvocation $
-- >     programInvocationCwd mbWorkDir ghcProg [interpretSymbolicPathCWD inputFile]
--
-- In this example, 'programInvocationCwd' sets the working directory, so it is
-- appropriate to use 'interpretSymbolicPathCWD' to provide its arguments.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
interpretSymbolicPathCWD :: SymbolicPathX allowAbsolute from to -> FilePath
interpretSymbolicPathCWD (SymbolicPath p) = p

getAbsolutePath :: AbsolutePath to -> FilePath
getAbsolutePath (AbsolutePath p) = getSymbolicPath p

interpretSymbolicPathAbsolute :: AbsolutePath (Dir Pkg) -> SymbolicPathX allowAbsolute Pkg to -> FilePath
interpretSymbolicPathAbsolute (AbsolutePath p) sym = interpretSymbolicPath (Just p) sym

-- | Change what a symbolic path is pointing to.
coerceSymbolicPath :: SymbolicPathX allowAbsolute from to1 -> SymbolicPathX allowAbsolute from to2
coerceSymbolicPath = coerce

-- | Does the second argument point to a sub-directory of the first one?
-- If so, return the relative portion of the path, relative to the first argument.
relativePathMaybe :: SymbolicPath from (Dir dir) -> SymbolicPath from to -> Maybe (RelativePath dir to)
relativePathMaybe base fp =
  let dirPieces =
        FilePath.splitDirectories $
          FilePath.dropTrailingPathSeparator $
            FilePath.normalise $
              getSymbolicPath base
      pathPieces =
        FilePath.splitDirectories $
          FilePath.normalise $
            getSymbolicPath fp
   in unsafeMakeSymbolicPath . FilePath.joinPath
        <$> stripPrefix dirPieces pathPieces

-- | Change both what a symbolic path is pointing from and pointing to.
--
-- Avoid using this in new code.
unsafeCoerceSymbolicPath :: SymbolicPathX allowAbsolute from1 to1 -> SymbolicPathX allowAbsolute from2 to2
unsafeCoerceSymbolicPath = coerce

-- | Weakening: convert a relative symbolic path to a symbolic path,
-- \"forgetting\" that it is relative.
relativeSymbolicPath :: RelativePath from to -> SymbolicPath from to
relativeSymbolicPath (SymbolicPath fp) = SymbolicPath fp

-- | Is this symbolic path a relative symbolic path?
symbolicPathRelative_maybe :: SymbolicPath from to -> Maybe (RelativePath from to)
symbolicPathRelative_maybe (SymbolicPath fp) =
  if isAbsoluteOnAnyPlatform fp
    then Nothing
    else Just $ SymbolicPath fp

-- | Absolute path to the current working directory.
absoluteWorkingDir :: Maybe (SymbolicPath CWD to) -> IO (AbsolutePath to)
absoluteWorkingDir Nothing = unsafeMakeAbsolutePath <$> Directory.getCurrentDirectory
absoluteWorkingDir (Just wd) = unsafeMakeAbsolutePath <$> Directory.makeAbsolute (getSymbolicPath wd)

-- | Try to make a symbolic path relative.
--
-- This function does nothing if the path is already relative.
--
-- NB: this function may fail to make the path relative.
tryMakeRelative :: Maybe (SymbolicPath CWD (Dir dir)) -> SymbolicPath dir to -> IO (SymbolicPath dir to)
tryMakeRelative mbWorkDir (SymbolicPath fp) = do
  AbsolutePath wd <- absoluteWorkingDir mbWorkDir
  return $ SymbolicPath (FilePath.makeRelative (getSymbolicPath wd) fp)

-------------------------------------------------------------------------------

-- ** Parsing and pretty printing

-------------------------------------------------------------------------------

instance Parsec (SymbolicPathX OnlyRelative from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else
        if isAbsoluteOnAnyPlatform token
          then P.unexpected "absolute FilePath"
          else return (SymbolicPath token)

instance Parsec (SymbolicPathX AllowAbsolute from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else return (SymbolicPath token)

instance Pretty (SymbolicPathX allowAbsolute from to) where
  pretty = showFilePath . getSymbolicPath

-------------------------------------------------------------------------------

-- * Composition

-------------------------------------------------------------------------------

infixr 7 <.>

-- | Types that support 'System.FilePath.<.>'.
class FileLike p where
  -- | Like 'System.FilePath.<.>', but also supporting symbolic paths.
  (<.>) :: p -> String -> p

instance FileLike FilePath where
  (<.>) = (FilePath.<.>)

instance FileLike (SymbolicPathX allowAbsolute dir File) where
  SymbolicPath p <.> ext = SymbolicPath (p <.> ext)

infixr 5 </>

-- | Types that support 'System.FilePath.</>'.
class PathLike p q r | q r -> p, p r -> q, p q -> r where
  -- | Like 'System.FilePath.</>', but also supporting symbolic paths.
  (</>) :: p -> q -> r

instance PathLike FilePath FilePath FilePath where
  (</>) = (FilePath.</>)

-- | This instance ensures we don't accidentally discard a symbolic path
-- in a 'System.FilePath.</>' operation due to the second path being absolute.
--
-- (Recall that @a </> b = b@ whenever @b@ is absolute.)
instance
  (b1 ~ Dir b2, a3 ~ a1, c2 ~ c3, midAbsolute ~ OnlyRelative)
  => PathLike
      (SymbolicPathX allowAbsolute a1 b1)
      (SymbolicPathX midAbsolute b2 c2)
      (SymbolicPathX allowAbsolute a3 c3)
  where
  SymbolicPath p1 </> SymbolicPath p2 = SymbolicPath (p1 </> p2)

instance
  (b1 ~ Dir b2, c2 ~ c3, midAbsolute ~ OnlyRelative)
  => PathLike
      (AbsolutePath b1)
      (SymbolicPathX midAbsolute b2 c2)
      (AbsolutePath c3)
  where
  AbsolutePath (SymbolicPath p1) </> SymbolicPath p2 =
    unsafeMakeAbsolutePath (p1 </> p2)

--------------------------------------------------------------------------------
-- Abstract directory locations.

-- | Abstract directory: current working directory.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data CWD deriving (Data)

-- | Abstract directory: package directory (e.g. a directory containing the @.cabal@ file).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Pkg deriving (Data)

-- | Abstract directory: dist directory (e.g. @dist-newstyle@).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Dist deriving (Data)

-- | Abstract directory: source directory (a search directory for source files).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Source deriving (Data)

-- | Abstract directory: include directory (a search directory for CPP includes like header files, e.g. with @ghc -I@).
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Include deriving (Data)

-- | Abstract directory: search directory for extra libraries.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Lib deriving (Data)

-- | Abstract directory: MacOS framework directory.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Framework deriving (Data)

-- | Abstract directory: build directory.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Build deriving (Data)

-- | Abstract directory: directory for build artifacts, such as documentation or @.hie@ files.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Artifacts deriving (Data)

-- | Abstract directory: package database directory.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data PkgDB deriving (Data)

-- | Abstract directory: data files directory.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data DataDir deriving (Data)

-- | Abstract directory: directory for HPC @.mix@ files.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Mix deriving (Data)

-- | Abstract directory: directory for HPC @.tix@ files.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Tix deriving (Data)

-- | Abstract directory: a temporary directory.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Tmp deriving (Data)

-- | Abstract directory: directory for response files.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data Response deriving (Data)

-- | Abstract directory: directory for pkg-config files.
--
-- See Note [Symbolic paths] in Distribution.Utils.Path.
data PkgConf deriving (Data)
