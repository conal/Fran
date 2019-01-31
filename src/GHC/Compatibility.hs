-- this module is for GHC; GSL
module Compatibility
       ( double2Float
       , cacheMatch
       ) where

import Win32(Word32)
import PrelNum(double2Float)
import PrelRead(readDec)
import IOExts ( reallyUnsafePtrEq )

cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = reallyUnsafePtrEq x x'
