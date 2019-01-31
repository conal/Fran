-- Hugs/GHC incompatibility work-arounds.  Hugs version.

module Compatibility ( cacheMatch, Addr, double2Float
                     , trace, safeTry )
where

import Addr (Addr)
import IOExts --( unsafePtrEq )
import Trace

double2Float :: Double -> Float
double2Float = fromDouble

cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = x `unsafePtrEq` x'


-- safeTry swiped from Hugs\lib\Graphics\GraphicsUtilities.hs

----------------------------------------------------------------
-- Safe Try
----------------------------------------------------------------

-- Run a computation and always succeed - even if we hit "error",
-- call "fail", or call "exitWith".
--
-- ToDo: 
--
-- 1) catch heap/stack overflow and ctrl-C
--
-- 2) suspending a thread really shouldn't be treated like a kind
--    of error - but there's no other choice given the current
--    implementation of concurrency.

safeTry :: IO a -> IO (Either IOError a)
safeTry (IO m) = IO $ \ f s -> 
  case catchError (m Hugs_Error Hugs_Return) of
  Just (Hugs_Return a) -> s (Right a)
  r                    -> s (Left (mkErr r))
 where
  mkErr :: Maybe (IOResult a) -> IOError
  mkErr (Just Hugs_SuspendThread) = userError "suspended inside protected code"
  mkErr (Just (Hugs_ExitWith e))  = userError "exited inside protected code"
  mkErr (Just (Hugs_Error e))     = e
  mkErr Nothing                   = userError "pattern match failure inside protected code"

primitive catchError :: a -> Maybe a
