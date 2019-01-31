-- Higher-level interaction
--
-- Last modified Mon Sep 09 10:03:07 1996

module Interaction where

import qualified MutVar
import PrimInteract
--import Interval
import Behavior
import Event
import Until
import VectorSpace
import Vector2
import Vector2B (Vector2B)
import Point2
import Point2B (Point2B)
import qualified Point2B (lerp2)
import Image (Image)
-- import Point2B
import qualified Pick2Image

lbp :: Time -> Event (Event ())
lbp t0 = primLBP t0 +=> \ t _ -> (primLBR t -=> ())

rbp :: Time -> Event (Event ())
rbp t0 = primRBP t0 +=> \ t _ -> (primRBR t -=> ())


-- Piecewise-constant mouse behavior 

mouse :: Time -> Point2B
mouse t0 = mouse' t0 origin2
 where
  mouse' t0 p =
   lift0 p `untilB` primMousePos t0 +=> mouse'



{- Piecewise-linear mouse behavior

-- This one should be better, but it's too jerky.

-- How long to wait before giving up on mouse motion
mouseMotionTimeOut = 0.2 :: Time

mouse :: Time -> Point2B
mouse t0 = mouse' origin2 t0 origin2 (t0-1)
 where
  mouse' p0 t0 p1 t1 =
   -- Follow a linear interpolation until we get another motion event.  If we
   -- don't get one before long, pretend we saw the same position.
   -- 
   -- Efficiency bug: when the mouse is quiet, we keep spinning.  Fix by
   -- adding an initial peaceful state.
   Point2B.lerp2 (lift0 p0) (lift0 p1)
                 ((time - lift0 t0) / lift0 (t1-t0)) `untilB`
    (primMousePos t1 .|. (timeIs (t1+0.2) -=> p1)) +=> \ t2 p2 ->
   mouse' p1 t1 p2 t2

-}

viewSize :: Time -> Vector2B
viewSize t0 = viewSz' t0 zeroVector
 where
  viewSz' t0 v =
    lift0 v `untilB` primViewSz t0 +=> viewSz'

fps :: Time -> Behavior Double
fps t0 = fps' t0 0
 where
  fps' t0 count =
    lift0 count `untilB` primFPS t0 +=> fps'

{-

visible :: Behavior Image -> Point2B -> Behavior Bool
visible img pointB = 
 Behavior (sample) (noI "visible")
 where
  sample t = Pick2Image.pick (img `at` t) (pointB `at` t)

when :: (Time -> Event a) -> Behavior Bool -> Time -> Event a
when evg predB t0 = 
 let
  ev = evg t0
  evv = unsafePerformIO (MutVar.newVar ev)

  evt t =
     case occ (unsafePerformIO (MutVar.readVar evv)) t of
       Nothing -> Nothing
       Just (tVal,x) ->
         if predB `at` tVal then
            Just (tVal,x)
         else
            unsafePerformIO (
               MutVar.writeVar evv (evg tVal) >>
               return (evt t))
 in
 mkEvt t0 (evt)

moveable :: Behavior Image -> Time -> Behavior Image
moveable img t0 =
    img `untilB` picker img t0 ==> \ (offset, release) ->
      let movingImage = translate (point2Vec offset) img in
        movingImage `untilB`
          (release `snapshot` movingImage) ==> snd ==> lift0 +=> flip (moveable)


picker :: Behavior Image -> Time -> Event (Point2B, Event Point2)
picker img t0 =
  ((lbp `when` visible img (mouse t0)) t0 `snapshot` (mouse t0))
    ==> \ (release, grabPos) -> (((mouse t0) - lift0 grabPos), release)

-}
