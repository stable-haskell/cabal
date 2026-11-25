{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

-- | @since 2.2.0
module Distribution.Utils.IOData
  ( -- * 'IOData' & 'IODataMode' type
    IOData (..)
  , IODataMode (..)
  , KnownIODataMode (..)
  , withIOData
  , null
  , hPutContents
  ) where

import qualified Data.ByteString.Lazy as LBS
import Distribution.Compat.Internal.LazyByteString (hGetContentsLBS)
import Distribution.Compat.Prelude hiding (null)
import qualified System.IO
import qualified Prelude

-- | Represents either textual or binary data passed via I/O functions
-- which support binary/text mode
--
-- @since 2.2
data IOData
  = -- | How Text gets encoded is usually locale-dependent.
    IODataText String
  | -- | Raw binary which gets read/written in binary mode.
    IODataBinary LBS.ByteString

-- | Applies a function polymorphic over 'IODataMode' to an 'IOData' value.
withIOData :: IOData -> (forall mode. IODataMode mode -> mode -> r) -> r
withIOData (IODataText str) k = k IODataModeText str
withIOData (IODataBinary lbs) k = k IODataModeBinary lbs

-- | Test whether 'IOData' is empty
null :: IOData -> Bool
null (IODataText s) = Prelude.null s
null (IODataBinary b) = LBS.null b

instance NFData IOData where
  rnf (IODataText s) = rnf s
  rnf (IODataBinary lbs) = rnf lbs

-- | @since 2.2
class NFData mode => KnownIODataMode mode where
  -- | 'IOData' Wrapper for 'System.IO.hGetContents'
  --
  -- __Note__: This operation uses lazy I/O. Use 'NFData' to force all
  -- data to be read and consequently the internal file handle to be
  -- closed.
  hGetIODataContents :: System.IO.Handle -> Prelude.IO mode

  toIOData :: mode -> IOData
  iodataMode :: IODataMode mode

-- | Phantom-typed GADT representation of the mode of 'IOData', containing no
-- other data.
--
-- @since 3.2
data IODataMode mode where
  IODataModeText :: IODataMode String
  IODataModeBinary :: IODataMode LBS.ByteString

{- FOURMOLU_DISABLE -}
instance a ~ Char => KnownIODataMode [a] where
  hGetIODataContents h = do
#ifdef __MHS__
    -- MicroHs handles have no text/binary mode switch.
    System.IO.hGetContents h
#else
    System.IO.hSetBinaryMode h False
    System.IO.hGetContents h
#endif

  toIOData = IODataText
  iodataMode = IODataModeText
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance KnownIODataMode LBS.ByteString where
  hGetIODataContents h = do
#ifndef __MHS__
    System.IO.hSetBinaryMode h True
#endif
    -- 'hGetContentsLBS' is 'LBS.hGetContents' on GHC; under MHS it
    -- avoids 'hGetSome', which MicroHs does not implement (and
    -- MicroHs handles have no text/binary mode switch to set).
    hGetContentsLBS h

  toIOData = IODataBinary
  iodataMode = IODataModeBinary
{- FOURMOLU_ENABLE -}

-- | 'IOData' Wrapper for 'System.IO.hPutStr' and 'System.IO.hClose'
--
-- This is the dual operation to 'hGetIODataContents',
-- and consequently the handle is closed with `hClose`.
--
-- /Note:/ this performs lazy-IO.
--
-- @since 2.2
{- FOURMOLU_DISABLE -}
hPutContents :: System.IO.Handle -> IOData -> Prelude.IO ()
hPutContents h (IODataText c) = do
#ifdef __MHS__
  -- MicroHs handles have no text/binary mode switch.
  System.IO.hPutStr h c
#else
  System.IO.hSetBinaryMode h False
  System.IO.hPutStr h c
#endif
  System.IO.hClose h
hPutContents h (IODataBinary c) = do
#ifdef __MHS__
  -- MicroHs handles have no text/binary mode switch.
  LBS.hPutStr h c
#else
  System.IO.hSetBinaryMode h True
  LBS.hPutStr h c
#endif
  System.IO.hClose h
{- FOURMOLU_ENABLE -}
