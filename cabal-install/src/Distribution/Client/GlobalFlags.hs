{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.GlobalFlags
  ( GlobalFlags (..)
  , defaultGlobalFlags
  , RepoContext (..)
  , withRepoContext
  , withRepoContext'
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.HttpUtils
  ( HttpTransport
  , configureTransport
  )
import Distribution.Client.Types
  ( LocalRepo (..)
  , RemoteRepo (..)
  , Repo (..)
  , localRepoCacheKey
  , unRepoName
  )
import Distribution.Simple.Setup
  ( Flag
  , flagToMaybe
  , fromFlag
  , pattern Flag
  )
import Distribution.Simple.Utils
  ( info
  , warn
  )
import Distribution.Utils.NubList
  ( NubList
  , fromNubList
  )

import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )

import Control.Concurrent
  ( MVar
  , modifyMVar
  , newMVar
  )
#ifndef __MHS__
import qualified Data.Map as Map
import qualified Data.Text as Text
import Network.URI
  ( URI
  , uriPath
  , uriScheme
  )
#endif
import System.FilePath
  ( isAbsolute
  , (</>)
  )
import System.IO.Error (userError)

#ifndef __MHS__
import qualified Distribution.Client.Security.DNS as Sec.DNS
import qualified Distribution.Client.Security.HTTP as Sec.HTTP
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Client.Repository.Cache as Sec
import qualified Hackage.Security.Client.Repository.Local as Sec.Local
import qualified Hackage.Security.Client.Repository.Remote as Sec.Remote
import qualified Hackage.Security.Util.Path as Sec
import qualified Hackage.Security.Util.Pretty as Sec
#endif

-- ------------------------------------------------------------

-- * Global flags

-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags
  { globalVersion :: Flag Bool
  , globalFullVersion :: Flag Bool
  , globalNumericVersion :: Flag Bool
  , globalConfigFile :: Flag FilePath
  , globalConstraintsFile :: Flag FilePath
  , globalRemoteRepos :: NubList RemoteRepo
  -- ^ Available Hackage servers.
  , globalCacheDir :: Flag FilePath
  , globalLocalNoIndexRepos :: NubList LocalRepo
  , globalActiveRepos :: Flag ActiveRepos
  , globalLogsDir :: Flag FilePath
  , globalIgnoreExpiry :: Flag Bool
  -- ^ Ignore security expiry dates
  , globalHttpTransport :: Flag String
  , globalStoreDir :: Flag FilePath
  , globalProgPathExtra :: NubList FilePath
  -- ^ Extra program path used for packagedb lookups in a global context (i.e. for http transports)
  }
  deriving (Show, Generic)
  deriving (Semigroup, Monoid) via Generically GlobalFlags

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags =
  GlobalFlags
    { globalVersion = Flag False
    , globalFullVersion = Flag False
    , globalNumericVersion = Flag False
    , globalConfigFile = mempty
    , globalConstraintsFile = mempty
    , globalRemoteRepos = mempty
    , globalCacheDir = mempty
    , globalLocalNoIndexRepos = mempty
    , globalActiveRepos = mempty
    , globalLogsDir = mempty
    , globalIgnoreExpiry = Flag False
    , globalHttpTransport = mempty
    , globalStoreDir = mempty
    , globalProgPathExtra = mempty
    }

-- ------------------------------------------------------------

-- * Repo context

-- ------------------------------------------------------------

-- | Access to repositories
{- FOURMOLU_DISABLE -}
data RepoContext = RepoContext
  { repoContextRepos :: [Repo]
  -- ^ All user-specified repositories
  , repoContextGetTransport :: IO HttpTransport
  -- ^ Get the HTTP transport
  --
  -- The transport will be initialized on the first call to this function.
  --
  -- NOTE: It is important that we don't eagerly initialize the transport.
  -- Initializing the transport is not free, and especially in contexts where
  -- we don't know a priori whether or not we need the transport (for instance
  -- when using cabal in "nix mode") incurring the overhead of transport
  -- initialization on _every_ invocation (eg @cabal build@) is undesirable.
  , repoContextWithSecureRepo
      :: forall a
       . Repo
#ifdef __MHS__
      -> (SecureRepository SecureRepoTag -> IO a)
#else
      -> (forall down. Sec.Repository down -> IO a)
#endif
      -> IO a
  -- ^ Get the (initialized) secure repo
  --
#ifdef __MHS__
  -- Secure repositories are not supported in this MHS build: using this
  -- accessor always fails with an error.
  --
#endif
  -- (the 'Repo' type itself is stateless and must remain so, because it
  -- must be serializable)
  , repoContextIgnoreExpiry :: Bool
  -- ^ Should we ignore expiry times (when checking security)?
  }
{- FOURMOLU_ENABLE -}

#ifdef __MHS__
-- | A secure repository handle.
--
-- Secure repositories are not supported in this build, so this type has
-- no constructors: 'repoContextWithSecureRepo' always fails.
data SecureRepository down

-- | Tag for 'SecureRepository'. The stub accessor above is monomorphic
-- in the tag (instead of rank-2 as upstream) because GHC rejects
-- rank-2 record selectors used as functions.
data SecureRepoTag
#else
-- | Wrapper around 'Repository', hiding the type argument
data SecureRepo = forall down. SecureRepo (Sec.Repository down)
#endif

withRepoContext :: Verbosity -> GlobalFlags -> (RepoContext -> IO a) -> IO a
withRepoContext verbosity globalFlags =
  withRepoContext'
    verbosity
    (fromNubList (globalRemoteRepos globalFlags))
    (fromNubList (globalLocalNoIndexRepos globalFlags))
    (fromFlag (globalCacheDir globalFlags))
    (flagToMaybe (globalHttpTransport globalFlags))
    (flagToMaybe (globalIgnoreExpiry globalFlags))
    (fromNubList (globalProgPathExtra globalFlags))

{- FOURMOLU_DISABLE -}
withRepoContext'
  :: Verbosity
  -> [RemoteRepo]
  -> [LocalRepo]
  -> FilePath
  -> Maybe String
  -> Maybe Bool
  -> [FilePath]
  -> (RepoContext -> IO a)
  -> IO a
withRepoContext'
  verbosity
  remoteRepos
  localNoIndexRepos
  sharedCacheDir
  httpTransport
  ignoreExpiry
  extraPaths = \callback -> do
    for_ localNoIndexRepos $ \local ->
      unless (isAbsolute (localRepoPath local)) $
        warn verbosity $
          "file+noindex " ++ unRepoName (localRepoName local) ++ " repository path is not absolute; this is fragile, and not recommended"

    transportRef <- newMVar Nothing
#ifdef __MHS__
    callback
      RepoContext
        { repoContextRepos =
            allRemoteRepos
              ++ allLocalNoIndexRepos
        , repoContextGetTransport = getTransport transportRef
        , repoContextWithSecureRepo = \_ _ ->
            throwIO $
              userError
                "Secure repositories are not supported in this build of cabal-install."
        , repoContextIgnoreExpiry = fromMaybe False ignoreExpiry
        }
#else
    let httpLib =
          Sec.HTTP.transportAdapter
            verbosity
            (getTransport transportRef)
    initSecureRepos verbosity httpLib secureRemoteRepos $ \secureRepos' ->
      callback
        RepoContext
          { repoContextRepos =
              allRemoteRepos
                ++ allLocalNoIndexRepos
          , repoContextGetTransport = getTransport transportRef
          , repoContextWithSecureRepo = withSecureRepo secureRepos'
          , repoContextIgnoreExpiry = fromMaybe False ignoreExpiry
          }
#endif
    where
#ifndef __MHS__
      secureRemoteRepos =
        [(remote, cacheDir) | RepoSecure remote cacheDir <- allRemoteRepos]

#endif
      allRemoteRepos :: [Repo]
      allRemoteRepos =
        [ (if isSecure then RepoSecure else RepoRemote) remote cacheDir
        | remote <- remoteRepos
        , let cacheDir = sharedCacheDir </> unRepoName (remoteRepoName remote)
              isSecure = remoteRepoSecure remote == Just True
        ]

      allLocalNoIndexRepos :: [Repo]
      allLocalNoIndexRepos =
        [ RepoLocalNoIndex local cacheDir
        | local <- localNoIndexRepos
        , let cacheDir
                | localRepoSharedCache local = sharedCacheDir </> localRepoCacheKey local
                | otherwise = localRepoPath local
        ]

      getTransport :: MVar (Maybe HttpTransport) -> IO HttpTransport
      getTransport transportRef =
        modifyMVar transportRef $ \mTransport -> do
          transport <- case mTransport of
            Just tr -> return tr
            Nothing -> configureTransport verbosity extraPaths httpTransport
          return (Just transport, transport)

#ifndef __MHS__
      withSecureRepo
        :: Map Repo SecureRepo
        -> Repo
        -> (forall down. Sec.Repository down -> IO a)
        -> IO a
      withSecureRepo secureRepos repo callback =
        case Map.lookup repo secureRepos of
          Just (SecureRepo secureRepo) -> callback secureRepo
          Nothing -> throwIO $ userError "repoContextWithSecureRepo: unknown repo"
#endif
{- FOURMOLU_ENABLE -}

#ifndef __MHS__
-- | Initialize the provided secure repositories
--
-- Assumed invariant: `remoteRepoSecure` should be set for all these repos.
initSecureRepos
  :: forall a
   . Verbosity
  -> Sec.HTTP.HttpLib
  -> [(RemoteRepo, FilePath)]
  -> (Map Repo SecureRepo -> IO a)
  -> IO a
initSecureRepos verbosity httpLib repos callback = go Map.empty repos
  where
    go :: Map Repo SecureRepo -> [(RemoteRepo, FilePath)] -> IO a
    go !acc [] = callback acc
    go !acc ((r, cacheDir) : rs) = do
      cachePath <- Sec.makeAbsolute $ Sec.fromFilePath cacheDir
      initSecureRepo verbosity httpLib r cachePath $ \r' ->
        go (Map.insert (RepoSecure r cacheDir) r' acc) rs

-- | Initialize the given secure repo
--
-- The security library has its own concept of a "local" repository, distinct
-- from @cabal-install@'s; these are secure repositories, but live in the local
-- file system. We use the convention that these repositories are identified by
-- URLs of the form @file:/path/to/local/repo@.
initSecureRepo
  :: Verbosity
  -> Sec.HTTP.HttpLib
  -> RemoteRepo
  -- ^ Secure repo ('remoteRepoSecure' assumed)
  -> Sec.Path Sec.Absolute
  -- ^ Cache dir
  -> (SecureRepo -> IO a)
  -- ^ Callback
  -> IO a
initSecureRepo verbosity httpLib RemoteRepo{..} cachePath = \callback -> do
  requiresBootstrap <- withRepo [] Sec.requiresBootstrap

  mirrors <-
    if requiresBootstrap
      then do
        info verbosity $
          "Trying to locate mirrors via DNS for "
            ++ "initial bootstrap of secure "
            ++ "repository '"
            ++ show remoteRepoURI
            ++ "' ..."

        Sec.DNS.queryBootstrapMirrors verbosity remoteRepoURI
      else pure []

  withRepo mirrors $ \r -> do
    when requiresBootstrap $
      Sec.uncheckClientErrors $
        Sec.bootstrap
          r
          (map (Sec.KeyId . Text.unpack) remoteRepoRootKeys)
          (Sec.KeyThreshold (fromIntegral remoteRepoKeyThreshold))
    callback $ SecureRepo r
  where
    -- Initialize local or remote repo depending on the URI
    withRepo :: [URI] -> (forall down. Sec.Repository down -> IO a) -> IO a
    withRepo _ callback | uriScheme remoteRepoURI == "file:" = do
      dir <- Sec.makeAbsolute $ Sec.fromFilePath (uriPath remoteRepoURI)
      Sec.Local.withRepository
        dir
        cache
        Sec.hackageRepoLayout
        Sec.hackageIndexLayout
        logTUF
        callback
    withRepo mirrors callback =
      Sec.Remote.withRepository
        httpLib
        (remoteRepoURI : mirrors)
        Sec.Remote.defaultRepoOpts
        cache
        Sec.hackageRepoLayout
        Sec.hackageIndexLayout
        logTUF
        callback

    cache :: Sec.Cache
    cache =
      Sec.Cache
        { cacheRoot = cachePath
        , cacheLayout =
            Sec.cabalCacheLayout
              { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
              , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
              , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
              }
        }

    cacheFn :: FilePath -> Sec.CachePath
    cacheFn = Sec.rootPath . Sec.fragment

    -- We display any TUF progress only in verbose mode, including any transient
    -- verification errors. If verification fails, then the final exception that
    -- is thrown will of course be shown.
    logTUF :: Sec.LogMessage -> IO ()
    logTUF = info verbosity . Sec.pretty
#endif
