{- IImage tests and demos -}

-- Last modified Sat Sep 07 23:23:11 1996
module IImageTest where

import IImage
import ColorB
import ShowImageB
import qualified ImageB

import Behavior
import Event
import ColorB
import qualified Point2
import Point2B hiding (x,y,theta,rho)
import Vector2B hiding (length)
import qualified Point2
import Integral
import Until
import PrimInteract
import Interaction (mouse)
import Interaction2
import Transform2B

{- To have a go, run disp ii{j} where j `elem` [0..10] -}

disp (IImage iim) = showImageB (iim identity2B ImageB.empty)

disp_frames f (IImage iim) = saveImageBFrames f (iim identity2B ImageB.empty)

-- pulsating circle
ii1 = circle (5 + 20*abs (cos (5*time) / 2)) 

-- convenience function
placeAt a b im = translate (vector2XY a b) im

--
-- adding a white disc in the background:
-- Deficiency: eye has hardwired its size 
--  (=> iris has to be smaller)
--
eye :: IImage -> IImage
eye iris = overlay iris (withColor white (circle 30))

-- tracking the movement of other models/iimages.
--
-- ii2 uses the IImage type of being Image behaviors
-- parameterised over a transform behavior that
-- maps local coords. to some global coord. system, to
-- inter-relate iimages.
--
-- One iimage can track the position of the other by
-- mapping the position of the other (`the trackee' or bug) 
-- into the global coord. system and then bring that
-- coord. position into the coordinate frame of the
-- other (`the tracker' or lizard).
-- (see interaction2.txt for more details.)
--
-- ii2 puts up a simple eye that watches the movement of
-- a ii1 (pulsating circle) as it moves across the screen.
--
ii2 = 
 getTransform2 $ \ boxXf ->
 let 
   -- bug motion
  bugPosInBox  = vector2 (30*time) 125
   -- the bug iimage itself
  bugIm        = translate bugPosInBox (pen red ii1)
   -- map bug coord back to global coordinates
  bugPosGlobal = boxXf *% bugPosInBox
  lizard       = getTransform2 $ \ lizardXf ->
                 let 
                  bugPosRelLizard = inverse2B lizardXf *% bugPosGlobal 
                 in
                 translate (vector2Polar 20 (theta bugPosRelLizard))
                           (pen blue $ circle 6)
 in
 overlay
  bugIm
  (placeAt 0 (-90) (eye lizard))

--
-- ii3 does the same tracking as ii2, this time the
-- eye is centred and the bug circles around the eye.
-- Note the difference between ii2 and ii3 in that
-- in ii2 the lizard had an extra transform applied to it
-- (its translation), whereas in ii3 the accumulated transforms
-- for both iimages are the same. 
--
ii3 = 
 getTransform2 $ \ boxXf ->
 let 
  bugMotion    = vector2 (120*sin (4*time)) (-120*cos (4*time))
  bugPosGlobal = boxXf *% bugMotion
  bugIm        = translate bugMotion (pen red ii1)
  lizard       = getTransform2 $ \ lizardXf ->
                 let 
                  bugPosRelLizard = inverse2B lizardXf *% bugPosGlobal 
                 in
                 translate (vector2Polar 20 (theta bugPosRelLizard))
                           (pen red $ circle 6)
 in
 overlay 
    bugIm
    (eye lizard)

{-
 ii4 pushes the iimage/iimage interaction further,
 creating an iimage capturing the lizard's iris
 and then creating multiple instances/invocations
 of it - each of them tracking the position of the bug
 from their vantage point.

 Another change from ii3 is that the motion of the bug
 now tracks mouse movements - making the eyes track
 the mouse pointer - cool!
-}
ii4 = ii4' 0
 where
  ii4' t0 = 
        getTransform2 $ \ boxXf ->
        let 
         bugCoord       = lift1 (Point2.point2Vec) (noI "") (mouse t0)
         bugCoordGlobal = boxXf *% bugCoord
         bug            = translate bugCoord (circle 10) 
         iris           = 
           getTransform2 $ \ irisXf ->
           let 
            bugRelIris = inverse2B irisXf *% bugCoordGlobal 
           in
           translate (vector2Polar 20 (theta bugRelIris))
                     (pen (rgb (abs (sin (theta bugRelIris)))
                               0.0
                               0.0) (circle 6))

        in
        noverlay 
          [pen lightBlue bug,
           placeAt (-50)    0  (eye iris),
           placeAt 10       0  (eye iris),
           placeAt (-20) (-50) (eye iris),
           placeAt (-120)  20  (eye iris),
           placeAt (-20)   50  (eye iris)]

{-
 ii5 is a variation on the bug/lizard theme - creating
 an `xeyes' like animation of having a pair of eyes
 track one object. The bug tracks the mouse position.
-}
ii5 = ii5' ii1 0
 where
  ii5' bugIm t0 = 
   getTransform2 $ \ boxXf ->
   let 
    bugCoord       = lift1 (Point2.point2Vec) (noI "") (mouse t0)
    bugCoordGlobal = boxXf *% bugCoord
    bug            = translate bugCoord bugIm
    iris         = 
      getTransform2 $ \ irisXf ->
      let 
       bugRelIris = inverse2B irisXf *% bugCoordGlobal 
      in
      translate (vector2Polar 20 (theta bugRelIris))
                (circle 6)
   in
   noverlay 
    [pen lightBlue bug,
     placeAt (-50) 0 (eye (pen blue iris)),
     placeAt 10    0 (eye (pen red  iris))]

{-
 ii6 - create new lizards by left-clicking the mouse.
-}
ii6 = ii6' 0
 where
  ii6' t0 = 
   getTransform2 $ \ boxXf ->
   let 
    planetCoord  =
        lift1 (Point2.point2Vec) (noI "") (mouse t0) -- ToDo: present a map interface to lift1
    planetCoordGlobal = boxXf *% planetCoord
    planet       = translate planetCoord (circle 10) 
    iris       = 
      getTransform2 $ \ irisXf ->
      let 
       planetRelIris = inverse2B irisXf *% planetCoordGlobal 
      in
      translate (vector2Polar 20 (theta planetRelIris))
                (pen
                   (rgb (abs (sin (theta planetRelIris)))
                        0.1
                        0.1) (circle 6))

    eyes img t0  =
      img `untilB` primLBP t0 +=> \ t1 pos ->
      eyes (overlay (translate (lift0 (Point2.point2Vec pos) - Vector2B.vector2 2 2) (eye iris)) img) t1
   in
   eyes (pen lightBlue planet) t0

{-
 ii7 rewires ii2 by packaging up the mapping from
 the bug coordinate system to the lizard's inside
 the defn. of getBugPos.
-}

ii7 = 
 getTransform2 $ \ boxXf ->
 let 
   -- bug motion
  bugPosInBox  = vector2 (30*time - 150) 0
  getBugPos f = getTransform2 $ \ localXf ->
                f (inverse2B localXf *% bugPosGlobal)
                where bugPosGlobal = boxXf *% bugPosInBox
   -- the bug iimage itself
  bugIm        = translate bugPosInBox (pen red ii1)

  lizard       = 
    getBugPos     $ \ bugPos ->
    translate (vector2Polar 20 (theta bugPos))
              (pen blue $ circle 6)
 in
 noverlay
  [bugIm, 
   placeAt 0 110 (eye lizard)]

{- As for ii7, this time ii3 has been modified plus we've added some global motion -}
ii8 = 
 translate (vector2 (10*sin (5*time)) (7*sin (7*time))) (
 getTransform2 $ \ boxXf ->
 let 
  bugMotion    = vector2 (120*sin (4*time)) (-120*cos (7*time))
  getBugPos f = getTransform2 $ \ localXf ->
                f (inverse2B localXf *% bugPosGlobal)
                where bugPosGlobal = boxXf *% bugMotion
  bugIm        = translate bugMotion (pen red ii1)
  lizard       = getBugPos $ \ bugPosRelLizard ->
                 translate (vector2Polar 20 (theta bugPosRelLizard))
                           (pen red $ circle 6)
 in
 noverlay 
    [bugIm,
     translate (vector2 (-150*sin time) 0) (eye lizard),
     translate (vector2 (150*sin time) 0) (pen green (eye lizard))])


{-
 ii9 is a reformulation of ii4 using the getGetter2 shorthand that
 hides manipulation of coordinate system frames. Given the motion
 of iimage1 in its local coord. system, getGetter2 returns a function
 that when applied `inside' some other iimage2 will grab out the local
 transform behavior and return the motion behavior of the other
 iimage1 as viewed from the coordinate system iimage2.
-}
ii9 = ii9' 0
 where
  ii9' t0 = 
   let bugMotion        = lift1 (Point2.point2Vec) (noI "") (mouse t0) in
   getGetter2 bugMotion $ \ getBugPos ->
   let
    bug  = translate bugMotion (circle 10) 
    iris = 
     getBugPos $ \ bugPos ->
     translate (vector2Polar 20 (theta bugPos))
               (pen (rgb (abs (sin (theta bugPos)))
                         0.0
                         0.0) (circle 6))

   in
   noverlay 
     [pen lightBlue bug,
      placeAt (-50)    0  (eye iris),
      placeAt 10       0  (eye iris),
      placeAt (-20) (-50) (eye iris),
      placeAt (-120)  20  (eye iris),
      placeAt (-20)   50  (eye iris)]


{-
 Different interaction - the max radius of 
 a pulsating circle is dependent on the distance
 between mouse pointer and circle centre.
-}

ii10 =
 ii10' 0
 where
  ii10' t0 = 
   let bugMotion = lift1 (Point2.point2Vec) (noI "") (mouse t0) in
   getGetter2 bugMotion $ \ getBugPos ->
   let
    bug    = translate bugMotion empty
    lizard = 
     getBugPos $ \ bugPos ->
     circle (30 * abs (cos (300 / (0.1+rho bugPos)*time) / 2))
   in
    overlay
     bug
     (pen red lizard)


{-
 Let's pick and move w/ occlusion support: 
-}
ii11 = ii11' 0
 where
  ii11' t0 = overlay
                (moveable t0 (placeAt (-20) (-20) (pen red ii1)))
                (moveable t0 (placeAt 50 50 (pen blue ii1)))


{- Nested tracking, IImage B tracks IImage A, IImage C tracks IImage B -}
ii12 = 
 translate (vector2 (20*sin (5*time)) (20*sin (7*time))) (
 getTransform2 $ \ boxXf ->
 let 
  bugMotion    = vector2 (120*sin (4*time)) (120*cos (4*time))
  getBugPos motion f = 
     getTransform2 $ \ localXf ->
     f (inverse2B localXf *% bugPosGlobal)
     where bugPosGlobal = boxXf *% motion
  bugIm         = translate bugMotion (pen red ii1)

  eye v iris = overlay iris (pen white (circle v))

  lizard sz motion pupil = 
   getBugPos motion $ \ bugPosRelLizard ->
   translate (vector2Polar (2*sz/3) (theta bugPosRelLizard))
             pupil
  
  pupil sz = pen red (circle (sz/5))

  m1 = vector2 (90*sin (3*time)) (90*cos (3*time))
  m2 = vector2 (70*sin (3*time)) (-70*cos (3*time))
  m3 = vector2 (50*sin (3*time)) (50*cos (3*time))
  m4 = vector2 (30*sin (3*time)) (-30*cos (3*time))
  l1 = translate m1 (eye 20 (lizard 20 bugMotion (pupil 20)))
  l2 = translate m2 (eye 20 (lizard 10 m1 (eye 10 (lizard 10 m1 (pupil 20)))))
  l3 = translate m3 (eye 20 (lizard 10 m2 (eye 10 (lizard 10 m2 (pupil 20)))))
  l4 = lizard 10 m4 (eye 10 (lizard 10 m3 (pupil 20)))
 in
 noverlay [bugIm, l1, l2,l3,l4])

