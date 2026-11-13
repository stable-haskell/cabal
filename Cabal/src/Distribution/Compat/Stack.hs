{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}

module Distribution.Compat.Stack
  ( WithCallStack
  , CallStack
  , HasCallStack
  , withFrozenCallStack
  , withLexicalCallStack
  , callStack
  , prettyCallStack
  , parentSrcLocPrefix
  ) where

#ifdef __MHS__

-- MicroHs has no call-stack support: there is no 'GHC.Stack' module
-- and it does not implement 'ImplicitParams'. This variant provides
-- the same API with neutral behaviour (empty call stacks, identity
-- wrappers) so that the rest of the tree typechecks.

type WithCallStack a = a

data CallStack = NoCallStack
  deriving (Show)

class HasCallStack

instance HasCallStack

withFrozenCallStack :: WithCallStack a -> WithCallStack a
withFrozenCallStack x = x

withLexicalCallStack :: (a -> WithCallStack (IO b)) -> WithCallStack (a -> IO b)
withLexicalCallStack f = \x -> f x

callStack :: CallStack
callStack = NoCallStack

prettyCallStack :: CallStack -> String
prettyCallStack _ = ""

-- | Give the *parent* of the person who invoked this;
-- so it's most suitable for being called from a utility function.
-- You probably want to call this using 'withFrozenCallStack'; otherwise
-- it's not very useful.
parentSrcLocPrefix :: WithCallStack String
parentSrcLocPrefix = ""

#else

import GHC.Stack

type WithCallStack a = HasCallStack => a

-- | Give the *parent* of the person who invoked this;
-- so it's most suitable for being called from a utility function.
-- You probably want to call this using 'withFrozenCallStack'; otherwise
-- it's not very useful.
parentSrcLocPrefix :: WithCallStack String
parentSrcLocPrefix =
  case getCallStack callStack of
    (_ : (_, loc) : _) -> showLoc loc
    [(_, loc)] -> showLoc loc
    [] -> error "parentSrcLocPrefix: empty call stack"
  where
    showLoc loc =
      srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ": "

-- Yeah, this uses skivvy implementation details.
withLexicalCallStack :: (a -> WithCallStack (IO b)) -> WithCallStack (a -> IO b)
withLexicalCallStack f =
  let stk = ?callStack
   in \x -> let ?callStack = stk in f x

#endif
