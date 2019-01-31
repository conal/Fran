{- Interaction with IImages -}

--   Last modified Sat Sep 07 23:23:11 1996
module Interaction2 where

import qualified MutVar
import PrimInteract
import qualified Interaction
import Behavior
import Event
import Until
import Image (Image)
import IImage
--import qualified ImageB
import qualified Point2
import qualified Vector2
import qualified Vector2B (zero)
import Point2B
import qualified Pick2Image
import Transform2B
import Transform2 (Transform2)

{-
  The definition in Version8/IImage.hs parameterises
  an Image behavior over its local-to-global transform
  and the behavior `above' it. 
-}

{-
 Assuming `mouse' is a behavior that reports mouse movement
 in the same coordinate system as the IImage is mapped into,
 getMousePos grabs the local-to-global behavior transform out
 of the IImage and maps mouse movement into IImage space.
-}
getMousePos :: Time -> (Point2B -> IImage) -> IImage
getMousePos t0 f =
 getTransform2 $ \ xf ->
 f (inverse2B xf *% (Interaction.mouse t0))


{-
WORKING HERE.  Drop moveable for now, since we don't have snapshot.  Also,
undo the mapping from screen to world space in ShowImageB, since it's done
here.  Or put that mapping into the WorldTransform.


moveable :: Time -> IImage -> IImage
moveable t0 img =
  getPick2 t0 $ \ pickGen ->
  let
   motion  t0 img =
      getPick2 t0                     $  \ picker ->
      img `untilB` picker t0         ==> \ (offset', release) ->
      let moving = point2Vec offset' in
      translate moving img `untilB`
         (release `snapshot` moving) ==> snd 
                                     ==> lift0 
                                     +=> \ t v -> 
      motion t (translate v img)
  in
  motion t0 img
-}

pick :: Point2B -> ImageB -> Transform2B
     -> IImage -> Time
     -> Event (Point2B,Event Point2.Point2)
pick mousePos above tr iimg t0 =
  ((Interaction.lbp `Interaction.when`
     (Interaction.visible (appII iimg tr above) mousePos &&* 
     notB (Interaction.visible above mousePos))) t0
   `snapshot` mousePos)
   ==> \ (release, grabPos) -> ((mousePos - lift0 grabPos), release)

getPick2 :: Time
         -> ((Time -> Event (Point2B, Event Point2.Point2)) -> IImage)
         -> IImage
getPick2 t0 f =
 img where
       img = 
         getMousePos t0 $ \ mousePos ->
         getAbove       $ \ above    ->
         getTransform2  $ \ tr ->
         f (pick mousePos above tr img)


getGetter2 :: Transformable2B a => a -> ((( a -> IImage) -> IImage) -> IImage) -> IImage
getGetter2 outerX getterConsumer =
  getTransform2 $ \ outerToGlobal ->
  let globalX = outerToGlobal *% outerX in
  getterConsumer (\ innerXConsumer ->
                    getTransform2 $ \ innerToGlobal ->
                    innerXConsumer (inverse2B innerToGlobal *% globalX))



-}
