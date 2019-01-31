-- this module is for GHC; GSL
module Compatibility
       ( double2Float
       , cacheMatch
       ) where

import PrelNum(double2Float)
import GHC(reallyUnsafePtrEquality#, (==#))

-- Seems to be a GHC bug.  Alastair says:
--   The library docs say Words are readable.
--   ftp://haskell.org/pub/reid/libs971028/libs-5.html
-- Here's the code:  Oops: unknown Word32 and redDec
-- instance Read Word32 where readsPrec p = readDec

-- Per Sigbjorn: It still supported as a primop, but so exotic that it
-- wasn't exported by any of the interfaces. To add it is
-- straightforward, add the following line to GHC.hi exports section (in
-- lib/`hw_os`/ghc-2.0x/imports/ )
--
--	reallyUnsafePtrEquality# and then
--
-- import GHC wherever you want to use it.


cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = let res = x `reallyUnsafePtrEquality#` x'
		    in  not (res ==# 0#)