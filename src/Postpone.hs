-- Concurrency without mvars.  This file defines a "postpone"
-- operation that enqueues a given continuation and allows another to run.
-- Computations are scheduled with a queue (FIFO).
--
-- Last modified Sat Sep 07 23:23:09 1996
-- 
-- This implementation uses side-effects to manage the "process queue",
-- but this queue could probably be passed around functionally and
-- implicitly in a monad.  See comment at the end of this file.
--
-- I'd be very happy to replace this module with better concurrency
-- support from Hugs.
-- 
-- Maybe the queue is unnecessary, and I could instead maintain just a
-- single delayed computation, using "(>>ioe)" instead of "(++[ioe])".
-- 
-- Does this stuff screw up failure continuations?
--
-- Should use an efficient representation of queues, say as pair of lists.

module Postpone where

import IORef(Ref,newRef,getRef,setRef)
import IOExtensions(unsafePerformIO, forkIO)

-- Process queue.  Add newly suspended computations to the tail and pull
-- off of the head.
type PQueue = Ref [IO ()] in
  initProcTable :: IO (),
  newPQueue :: IO PQueue,
  postpone :: IO () -> IO (),
  emptyPQueue :: IO Bool,
  activateOne :: IO (),
  activateAll :: IO (),
  tracePQ :: String -> IO ()

withNewPQueue :: (PQueue -> IO ()) -> IO ()

-- naughty hack: instead of passing the suspension queue around,
-- we create a global variable to hold the current set of blocked
-- contexts. 

-- initProcTable :: IO ()
initProcTable = setRef proc_table []

proc_table :: PQueue
proc_table = unsafePerformIO (newPQueue)

newPQueue = newRef (error "newPQueue")

withNewPQueue h =
  newPQueue >>= \pq ->
  h pq      >>
  activateAll


postpone cont =
  --tracePQ " postpone"                     >>
  getRef proc_table                        >>= \ suspended ->
  setRef proc_table (suspended ++ [cont]) >>
  return ()

tracePQ opstr =
  getRef proc_table >>= \ suspended ->
  putStr (opstr ++ ": length pq = " ++ show (length suspended)) >>
  return ()


emptyPQueue =
  --tracePQ " emptyPQueue" >>
  getRef proc_table >>= \ suspended ->
  return (null suspended)

-- Activate head of the queue
activateOne =
  --tracePQ " activateOne" >>
  getRef proc_table     >>= \ suspended ->
  case suspended of
     -- No postponed computations?  All done.
    []  ->  error "{activateOne} Empty pqueue"
     -- otherwise, pop off the head of the queue and yield
     -- that head.
    (firstSuspended : restSuspended) ->
       setRef proc_table restSuspended >>
       firstSuspended

-- Keep activating until nothing left to do.
activateAll =
  --tracePQ "activateAll" >>
  emptyPQueue >>= \isEmpty ->
  if isEmpty then 
     return ()
  else 
     activateOne >> activateAll 


-- Testing

{-
susp1 = withNewPQueue (\pq ->
        let
            writes 0 ch = return ()
            writes n ch = putChar ch >>
                          postpone pq
                            (writes (n-1) ch)
        in
            writes 3 'a' >>
            writes 8 'b')

-}
susp1 = 
 let
  writes 0 ch = return ()
  writes n ch = putChar ch >>
                postpone (writes (n-1) ch)
 in
 writes 3 'a' >>
 writes 8 'b'


susp2 = forkIO (writes 8 'a')   >>
        writes 3 'b'
        where
          writes 0 ch = return ()
          writes n ch = putChar ch >>
                        postpone (writes (n-1) ch)

{- Here's what I'd really like to say. -}
{-
susp2 = forkIO (writes 8 'a')   >>
        writes 3 'b'
        where
          writes 0 ch = return ()
          writes n ch = putChar ch >>
                        suspend  (writes (n-1) ch)
-}

-- I can almost do this, but I need to modify and extend the IO monad,
-- which is an abstract type.  Defining "suspend" requires getting at and
-- postponing the success continuation, which is hidden in the abstract
-- type.  The suspension queue variable would be passed around implicitly
-- and used by fork and suspend.  Could mvar synchronization still work?

