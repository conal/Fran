-- Interactive Image behaviors: coordinate frames and occlusion
-- 
-- Last modified Sat Sep 07 23:23:10 1996

{-
 This version of IImage implements the second approach to interactive
 images discussed in the interaction2.txt notes. 
-}

module IImage where

import Behavior
import ImageB (ImageB)
import qualified ImageB as IB
import Transform2B
import Vector2B
--import Event
--import Until

{-
 Tedious, but necessary encapsulation of IImage function
 inside constructor to make it possible to declare instances
 for IImage.
-}
data IImage = IImage (Transform2B -> ImageB -> ImageB)

getTransform2 :: (Transform2B -> IImage) -> IImage
getTransform2 f = IImage (\ xfb -> appII (f xfb) xfb)

getAbove :: (ImageB -> IImage) -> IImage
getAbove f = IImage (\ xfb above -> appII (f above) xfb above)

appII (IImage imF) xf above = imF xf above


{-
instance GBehavior IImage where
 ((IImage imF) `untilB` e) =
    IImage (\ xfb above -> imF xfb above `untilB`
                             e ==> \ (IImage f) -> f xfb above)
-}

iiLift0 imb = IImage (\ xf above -> imb)

emptyImage            = iiLift0   IB.emptyImage
circle                = iiLift0   IB.circle
square                = iiLift0   IB.square
bitmap size hBmap     = iiLift0 $ IB.bitmap size hBmap
line p0 p1            = iiLift0 $ IB.line p0 p1
polyline ps           = iiLift0 $ IB.polyline ps
bezier p1 p2 p3       = iiLift0 $ IB.bezier p1 p2 p3
renderedText text     = iiLift0 $ IB.renderedText text
ellipse size          = iiLift0 $ IB.ellipse size
rectangle size        = iiLift0 $ IB.rectangle size
importBitmap fileName = iiLift0 $ IB.importBitmap fileName

withColor c (IImage imF) =
  IImage (\ xf above -> IB.withColor c (imF xf above))

bboxed2 p1 p2 (IImage imF) =
  IImage (\ xf above -> IB.bboxed2 p1 p2 (imF xf above))

(IImage imF1) `over` (IImage imF2) = 
  IImage (\ xf above -> 
          let im1 = imF1 xf above in
          im1 `IB.over` (imF2 xf (above `IB.over` im1)))


instance Transformable2 IImage where
  add_xf *% (IImage imF) = 
    IImage (\ xf above -> add_xf *% imF (xf `compose2` add_tr) above)



