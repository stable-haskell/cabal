{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Client.Compat.Orphans () where

import Control.Exception (SomeException)
import Data.Typeable (typeRep)
import Distribution.Compat.Binary (Binary (..))
import Distribution.Utils.Structured (Structure (Nominal), Structured (..))
import Network.URI (URI (..), URIAuth (..))
import Prelude (error, return)
#ifdef __MHS__
import Control.DeepSeq (NFData (..))
import Prelude (Ord (..), (<>), seq)
#endif

-------------------------------------------------------------------------------
-- network-uri
-------------------------------------------------------------------------------

#ifdef __MHS__
-- uri-mhs-0.1.0.0 only provides Eq and Show for URI and URIAuth;
-- network-uri provides Ord and NFData as well.
instance Ord URIAuth where
  compare (URIAuth a1 b1 c1) (URIAuth a2 b2 c2) =
    compare a1 a2 <> compare b1 b2 <> compare c1 c2

instance Ord URI where
  compare (URI a1 b1 c1 d1 e1) (URI a2 b2 c2 d2 e2) =
    compare a1 a2 <> compare b1 b2 <> compare c1 c2 <> compare d1 d2 <> compare e1 e2

instance NFData URIAuth where
  rnf (URIAuth a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData URI where
  rnf (URI a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e
#endif

-- note, network-uri-2.6.0.3+ provide a Generic instance but earlier
-- versions do not, so we use manual Binary instances here
instance Binary URI where
  put (URI a b c d e) = do put a; put b; put c; put d; put e
  get = do
    !a <- get
    !b <- get
    !c <- get
    !d <- get
    !e <- get
    return (URI a b c d e)

instance Structured URI where
  structure p = Nominal (typeRep p) 0 "URI" []

instance Binary URIAuth where
  put (URIAuth a b c) = do put a; put b; put c
  get = do !a <- get; !b <- get; !c <- get; return (URIAuth a b c)

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

-- FIXME: Duncan Coutts: this is a total cheat
-- Added in 46aa019ec85e313e257d122a3549cce01996c566
instance Binary SomeException where
  put _ = return ()
  get = error "cannot serialise exceptions"

instance Structured SomeException where
  structure p = Nominal (typeRep p) 0 "SomeException" []
