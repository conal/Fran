-- Two-handed image navigation.  See ReadMe.txt

module Navigate (navigate) where

import Fran
import qualified StaticTypes as S
import Hand


navigate :: Hand -> Hand -> User -> Transform2B
navigate handA@(Hand posA grabA releaseA grabbingA)
         handB@(Hand posB grabB releaseB grabbingB) _ = xfB
 where
   -- The transform.  Build onto what it was at the last change
   xfB = sinceChange `compose2` atChange 

   atChange = stepper S.identity2 (change `snapshot_` xfB)

   -- Four modes: pan/zoom/rotate, pan with hand A or B, or stand still
   sinceChange = ifB grabbingA
                     (ifB grabbingB pzr  panA)
                     (ifB grabbingB panB identity2)

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
   change = grabA .|. releaseA .|. grabB .|. releaseB
