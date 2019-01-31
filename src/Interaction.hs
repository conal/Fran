-- Higher-level interaction
--
-- Last modified Tue Nov 05 14:48:16 1996

module Interaction where

import IORef(Ref,newRef,getRef,setRef)
import qualified PrimInteract as Prim
--import Interval
import Behavior
import Event
import Until
import VectorSpace
import Vector2
import Vector2B (Vector2B)
import Point2
import Point2B (Point2B)
import qualified Point2B (linearInterpolate2)
-- import Image (Image)
-- import Point2B
-- import qualified PickImage

lbp,rbp :: Time -> Event (Event ())
lbp t0 = Prim.lbp t0 *=> lbr
rbp t0 = Prim.rbp t0 *=> rbr

lbr, rbr :: Time -> Event ()

lbr t1 = Prim.lbr t1 -=> ()
rbr t1 = Prim.rbr t1 -=> ()

keyPress :: Time -> Event (Char, Event ())
keyPress t0 =
  Prim.keyPress t0 +=> \ t1 ch ->
   (ch, (Prim.keyRelease `suchThat` (== ch)) t0 -=> ())

-- Piecewise-constant mouse behavior 

mouse :: Time -> Point2B
mouse t0 = mouse' t0 origin2
 where
  mouse' t0 p =
   lift0 p `untilB` (Prim.getEvent Prim.mousePos) t0 +=> mouse'



{- Piecewise-linear mouse behavior

-- This one should be better, but it's too jerky.

-- How long to wait before giving up on mouse motion
mouseMotionTimeOut = 0.2 :: Time

mouse :: Time -> Point2B
mouse t0 = mouse' t0 origin2 (t0-1) origin2
 where
  mouse' t0 p0 t1 p1 =
   -- Follow a linear interpolation until we get another motion event.  If we
   -- don't get one before long, pretend we saw the same position.
   -- 
   -- Efficiency bug: when the mouse is quiet, we keep spinning.  Fix by
   -- adding an initial peaceful state.
   Point2B.linearInterpolate2 (lift0 p0) (lift0 p1)
                              ((time - lift0 t0) / lift0 (t1-t0)) `untilB`
    (getEvent mousePos t1 .|. (timeIs (t1+0.2) -=> p1)) +=> mouse' t1 p1

-}

viewSize :: Time -> Vector2B
viewSize t0 = viewSz' t0 initialSize
 where
  viewSz' t0 v =
    lift0 v `untilB` Prim.getEvent Prim.viewSz t0 +=> viewSz'
  -- Hack: we don't really know the initial size here.  This won't work
  -- for ActiveX controls
  initialSize = vector2XY 2 2

fps :: Time -> Behavior Double
fps t0 = fps' t0 0
 where
  fps' t0 count =
    lift0 count `untilB` Prim.getEvent Prim.fps t0 +=> fps'

{-

visible :: Behavior Image -> Point2B -> Behavior Bool
visible img pointB = 
 Behavior (sample) (noI "visible")
 where
  sample t = PickImage.pick (img `at` t) (pointB `at` t)

when :: (Time -> Event a) -> Behavior Bool -> Time -> Event a
when evg predB t0 = 
 let
  ev = evg t0
  evv = unsafePerformIO (newRef ev)

  evt t =
     case occ (unsafePerformIO (getRef evv)) t of
       Nothing -> Nothing
       Just (tVal,x) ->
         if predB `at` tVal then
            Just (tVal,x)
         else
            unsafePerformIO (
               setRef evv (evg tVal) >>
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
