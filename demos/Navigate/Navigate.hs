-- Two-handed image navigation.  See ReadMe.txt

module Navigate (navigate) where

import Fran hiding (pan)
import qualified StaticTypes as S
import Hand

-- Version with lots of local definitions (works)

navigate :: Hand -> Hand -> Transform2B
navigate (Hand posA graspA releaseA graspingA)
         (Hand posB graspB releaseB graspingB) = xfB
 where
   -- The transform.  Build onto what it was at the last change
   xfB = sinceChange `compose2` atChange 

   atChange = stepper S.identity2 (change `snapshot_` xfB)

   -- Four modes: pan/zoom/rotate, pan with hand A or B, or stand still
   sinceChange = ifB graspingA
                     (ifB graspingB pzr  panA)
                     (ifB graspingB panB identity2)

   -- Position at last change (start of mode) and a relative translation
   (posA0, panA) = startAndPan posA
   (posB0, panB) = startAndPan posB

   startAndPan pos = (pos0, pan)
    where
      pos0 = stepper undefined (change `snapshot_` pos)
      pan  = translate2 (pos .-. pos0)

   -- pan/zoom/rotate (see Readme.txt for derivation)
   pzr = translate2 d `compose2` w
    where
      w = uscale2 (r / r0) `compose2` rotate2 (theta - theta0)
      d = posA .-. w *% posA0

      (r0, theta0) = vector2PolarCoords (posB0 .-. posA0)
      (r , theta ) = vector2PolarCoords (posB  .-. posA )

   -- Mode change
   change = graspA .|. releaseA .|. graspB .|. releaseB


-- There ought to be a simple way to use Transform2B-valued events in this
-- example.


-- Works

{- 
navigate :: Hand -> Hand -> Transform2B
navigate handA handB = xfB
 where
   -- The transform.  Build onto what it was at the last change
   xfB = sinceChange `compose2` atChange 
   atChange = stepper S.identity2 (change `snapshot_` xfB)
   (change, sinceChange) = changeAndSince handA handB

changeAndSince :: Hand -> Hand -> (Event (), Transform2B)
changeAndSince (Hand posA graspA releaseA graspingA)
               (Hand posB graspB releaseB graspingB) = 
  -- Four modes: pan/zoom/rotate, pan with hand A or B, or stand still
  ( change,
    ifB graspingA
        (ifB graspingB
             (pzr posA0 posA posB0 posB)
             (pan posA0 posA))
        (ifB graspingB
             (pan posB0 posB)
             identity2) )
 where
   -- Position at last change (start of mode)
   posA0 = snapshotB change posA
   posB0 = snapshotB change posB

   -- Mode change
   change = graspA .|. releaseA .|. graspB .|. releaseB

-- -- Pan only
pan :: Point2B -> Point2B -> Transform2B
pan pos0 pos = translate2 (pos .-. pos0)

-- Pan/Zoom/rotate (see Readme.txt for derivation)
pzr :: Point2B -> Point2B -> Point2B -> Point2B -> Transform2B
pzr posA0 posA posB0 posB = translate2 d `compose2` w
 where
   w = uscale2 (r / r0) `compose2` rotate2 (theta - theta0)
   d = posA .-. w *% posA0

   (r0, theta0) = vector2PolarCoords (posB0 .-. posA0)
   (r , theta ) = vector2PolarCoords (posB  .-. posA )


snapshotB :: Event () -> Behavior a -> Behavior a
snapshotB e b = stepper undefined (e `snapshot_` b)
-}