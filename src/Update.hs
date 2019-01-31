-- "Update trees"

module Update (
  UpdateTree(..)
  ,doUpdateTrees
  ,updateRefIO , updateBvrRefIO
  ,TransformM, TransformIO
  ,(.>>=)
  ) where

import BaseTypes
import Behavior
import Event
import IORef


infixr 1  .>>=                          -- See TransformM below.


-- I want to put a "Monad m =>" in here, but don't know how.
type TransformM m a  =  a -> m a

-- I thought I could eta-reduce the "a" away below, but Hugs balked
type TransformIO a = TransformM IO a

(.>>=) :: Monad m => TransformM m a -> TransformM m a -> TransformM m a

(f .>>= g) x  =  f x >>= g

-- A tree of update actions.

data UpdateTree =
    NoUpdate
  | UpdateIO (Time -> Time -> IO ())            -- Action per sample
  | UpdateGroup [UpdateTree] (Event (IO [UpdateTree]))  -- from untilB

-- To do: fix the comments below, which should no longer be specific to
-- sprite trees.

-- Traverse a list of the local sprite tree representations, updating
-- the sprite state and group members.

doUpdateTrees :: Time -> Time -> TransformIO [UpdateTree]

doUpdateTrees t t' [] = return []

doUpdateTrees t t' (NoUpdate : above) = doUpdateTrees t t' above

doUpdateTrees t t' (upd@(UpdateIO update) : above) = do
  -- First update
  update t t'
  -- Then the others
  above' <- doUpdateTrees t t' above
  -- And return the new list
  return (upd : above')

doUpdateTrees t t' (UpdateGroup elems ev : above) =
  -- See whether the event has occurred
  case mbOcc of
    -- If not, just update the group's elements and the rest,
    -- and return the assembled new list
    Nothing ->
      doUpdateTrees t t' elems  >>= \ elems' ->
      doUpdateTrees t t' above  >>= \ above' ->
      return (UpdateGroup elems' ev' : above')
    -- If the event occurred, make the new trees and chain, reset the
    -- sprite group's members, and update the new trees.
    Just (te, makeNewIETrees)  ->
      makeNewIETrees  >>= \ newUpdateTrees ->
      -- The group is now immutable.  Could try to optimize the case of
      -- switching to another untilI.  Requires some restructuring.
      doUpdateTrees t t' (newUpdateTrees ++ above)
 where
  (mbOcc, ev') = ev `occ` t

-- Noting the last few lines may make the immutable sprite group
-- optimization very easy.


updateRefIO :: Ref a -> (a -> IO a) -> IO ()

updateRefIO ref f =
  do oldVal <- getRef ref
     newVal <- f oldVal
     setRef ref newVal

updateBvrRefIO :: Ref (Behavior a) -> Time -> (a -> IO b) -> IO b

updateBvrRefIO ref t action =
  do b <- getRef ref
     let (x, b') = b `at`t in
       do setRef ref b'
          action x
