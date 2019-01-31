-- this module is for GHC; GSL
module Compatibility
       ( double2Float
       , cacheMatch
       , safeTry
       ) where

import Win32(Word32)
import PrelNum(double2Float)
import PrelRead(readDec)
import IOExts ( reallyUnsafePtrEq )

cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = reallyUnsafePtrEq x x'

-- I don't know how to do safeTry with GHC.  On the other hand, it's not
-- as useful as in Hugs's interpretive environment.
safeTry :: IO a -> IO (Either IOError a)
safeTry io = map Right io