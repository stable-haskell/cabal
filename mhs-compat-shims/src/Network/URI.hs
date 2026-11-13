-- | Minimal @Network.URI@ shim for MicroHs (@mhs@).
--
-- This is a small hand-written reimplementation (base-only, no parsec)
-- of the subset of the @network-uri@ API surface used by the cabal
-- codebase: the 'URI' and 'URIAuth' types with their record fields,
-- 'nullURI', 'nullURIAuth', 'parseURI', 'parseAbsoluteURI',
-- 'uriToString' and 'isUnreserved'.
--
-- Parsing follows RFC 3986 loosely: @scheme \":\" hier-part [ \"?\" query ]
-- [ \"#\" fragment ]@. It is intentionally lenient about registry names
-- (e.g. Windows drive paths in @file@ URIs) and does not validate
-- percent-escapes the way @network-uri@ does. All call sites in the
-- repo pass @id@ as the escape function to 'uriToString', so escaping
-- behaviour is not load-bearing.
module Network.URI
  ( URI (..)
  , URIAuth (..)
  , nullURI
  , nullURIAuth
  , parseURI
  , parseAbsoluteURI
  , uriToString
  , isUnreserved
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)

-- | A Uniform Resource Identifier reference.
data URI = URI
  { uriScheme :: String
  , uriAuthority :: Maybe URIAuth
  , uriPath :: String
  , uriQuery :: String
  , uriFragment :: String
  } deriving (Eq)

-- | Authority component of a 'URI'.
data URIAuth = URIAuth
  { uriUserInfo :: String
  , uriRegName :: String
  , uriPort :: String
  } deriving (Eq)

-- | Show renders the URI in string form (as @network-uri@ does),
-- not record syntax: error messages and cache paths rely on this.
instance Show URI where
  showsPrec _ u = showString (uriToString id u "")

instance Show URIAuth where
  showsPrec _ (URIAuth user reg port) =
    showString (user ++ reg ++ port)

-- | An empty URI value.
nullURI :: URI
nullURI = URI "" Nothing "" "" ""

-- | An empty authority value.
nullURIAuth :: URIAuth
nullURIAuth = URIAuth "" "" ""

-- | Characters that never need percent-encoding (RFC 3986 unreserved).
isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNum c || c `elem` ("-_.~" :: String)

-- | Render a URI. The first argument escapes @path@, @query@ and
-- @fragment@ (all call sites in the repo pass @id@).
uriToString :: (String -> String) -> URI -> String -> String
uriToString esc (URI scheme auth path query frag) rest =
  scheme ++ authStr ++ esc path ++ esc query ++ esc frag ++ rest
  where
    authStr = case auth of
      Nothing -> ""
      Just (URIAuth user reg port) -> "//" ++ user ++ reg ++ port

-- | Parse an absolute URI (a scheme is required).
parseURI :: String -> Maybe URI
parseURI s = do
  (scheme, rest) <- splitScheme s
  (auth, hier, query, frag) <- parseHier rest
  return (URI scheme auth hier query frag)

-- | Parse an absolute URI, rejecting relative references and fragments.
-- (Slightly stricter than 'parseURI'; same as @network-uri@, fragments
-- are not part of an absolute URI.)
parseAbsoluteURI :: String -> Maybe URI
parseAbsoluteURI s = do
  u <- parseURI s
  if null (uriFragment u) then Just u else Nothing

-- Splits "scheme:rest", requiring a valid RFC 3986 scheme.
splitScheme :: String -> Maybe (String, String)
splitScheme s = case break (== ':') s of
  (scheme, ':' : rest)
    | validScheme scheme -> Just (scheme ++ ":", rest)
  _ -> Nothing
  where
    validScheme (c : cs) = isAlpha c && all schemeChar cs
    validScheme [] = False
    schemeChar c = isAlphaNum c || c `elem` ("+-." :: String)

-- Parses hier-part, query and fragment. Query keeps its leading '?',
-- fragment its leading '#', matching @network-uri@ field conventions.
parseHier :: String -> Maybe (Maybe URIAuth, String, String, String)
parseHier s = do
  (auth, rest0) <- parseAuthorityPrefix s
  let (hier, rest1) = break (`elem` ("?#" :: String)) rest0
      (query, frag) = parseQueryFrag rest1
  if validChars hier && validChars query && validChars frag
    then Just (auth, hier, query, frag)
    else Nothing

parseAuthorityPrefix :: String -> Maybe (Maybe URIAuth, String)
parseAuthorityPrefix s
  | "//" `isPrefixOf` s =
      let (authStr, rest) = break (`elem` ("/?#" :: String)) (drop 2 s)
       in case parseAuth authStr of
            Just a -> Just (Just a, rest)
            Nothing -> Nothing
  | otherwise = Just (Nothing, s)
  where
    isPrefixOf pfx str = take (length pfx) str == pfx

parseAuth :: String -> Maybe URIAuth
parseAuth s = do
  let (user, hostport) = splitUserInfo s
      (reg, port) = splitPort hostport
  if validChars user && validChars reg && validPort port
    then Just (URIAuth user reg port)
    else Nothing

-- Splits at the last '@'; user info keeps its trailing '@'.
splitUserInfo :: String -> (String, String)
splitUserInfo s = case break (== '@') (reverse s) of
  (_, []) -> ("", s)
  (revHost, _ : revUser) -> (reverse revUser ++ "@", reverse revHost)

-- Splits reg-name and port; the port keeps its leading ':'.
splitPort :: String -> (String, String)
splitPort s = case break (== ':') (reverse s) of
  (_, []) -> (s, "")
  (revPort, _ : revReg)
    | all isDigit (reverse revPort) -> (reverse revReg, ':' : reverse revPort)
    | otherwise -> (s, "")

validPort :: String -> Bool
validPort "" = True
validPort (':' : ds) = not (null ds) && all isDigit ds
validPort _ = False

parseQueryFrag :: String -> (String, String)
parseQueryFrag s = case s of
  ('?' : rest) ->
    let (q, f) = break (== '#') rest
     in ('?' : q, f)
  ('#' : rest) -> ("", '#' : rest)
  _ -> ("", "")

-- Rejects spaces and control characters only; deliberately lenient
-- otherwise (e.g. backslashes in Windows file URIs are normalised
-- elsewhere in cabal by 'normaliseFileNoIndexURI').
validChars :: String -> Bool
validChars = all (\c -> c > ' ' && c /= '\x7f')
