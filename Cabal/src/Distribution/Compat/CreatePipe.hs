{-# LANGUAGE CPP #-}

#if defined(__MHS__) || defined(MHS)
module Distribution.Compat.CreatePipe
  {-# DEPRECATED "Use System.Process from package process directly" #-}
  (createPipe) where

import Distribution.Compat.Internal.MhsProcess (mhsCreatePipe)
import System.IO (Handle)

-- | A 'CreatePipe' replacement based on temporary files: MHS
-- 'Handle's cannot be built on top of file descriptors, so each pipe
-- is a temporary file with a read 'Handle' and a write 'Handle'
-- opened on it.  See "Distribution.Compat.Internal.MhsProcess" for
-- the semantic limits of this fallback.
createPipe :: IO (Handle, Handle)
createPipe = mhsCreatePipe

#else
module Distribution.Compat.CreatePipe
  {-# DEPRECATED "Use System.Process from package process directly" #-}
  (createPipe) where

import Distribution.Compat.Process (createPipe)
#endif
