module TargetLayout (layoutProfiling) where

#ifndef PROFILING
#error hsc2hs must use the profiling target C ABI
#endif

layoutProfiling :: Int
layoutProfiling = #{const PROFILING}
