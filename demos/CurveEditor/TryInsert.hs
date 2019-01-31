-- A first try at adding and deleting control points.


module TryInsert where

import Editor4 hiding (editCurve, editorWith, editor, main)

import Fran
import qualified StaticTypes as S
import Win32Key
import IOExts ( trace )

ed1, ed2 :: User -> ImageB

ed1 u = accumB over emptyImage (addAt ==> newPointIm)
 where
   addAt :: Event (User, S.Point2)
   addAt = nextUser_ (charPress 'a') u `snapshot` mouse u

   newPointIm :: (User, S.Point2) -> ImageB
   newPointIm (u, p0) =
     renderXPoint xp `untilB` delete -=> emptyImage
    where
      xp@(_,closeEnough) = editXPoint u p0 
      delete             = charPress 'd' u `whenE` closeEnough

ed2 u = accumB over emptyImage (addAt ==> edX ==> newPointIm)
 where
   addAt :: Event (User, S.Point2)
   addAt = nextUser_ (charPress 'a') u `snapshot` mouse u

   edX :: (User, S.Point2) -> (XPoint, Event ())
   edX (u, p0) = (xp, delete)
    where
      xp@(_,closeEnough) = editXPoint u p0
      delete             = charPress 'd' u `whenE` closeEnough

   newPointIm :: (XPoint, Event ()) -> ImageB
   newPointIm (xp, delete) = renderXPoint xp `untilB` delete -=> emptyImage

try imF = displayUMon (\ u -> imF u `over` whiteIm)


data CPoint = CPoint XPoint
                     (Event ())         -- delete
                     (Event CPoint)     -- new cpoint


-- Something doesn't work here, and I'm stumped.  If a point and one of
-- its decendents are both grabbable, and I try to add, only the ancestor
-- gets to add.  Any other pair, however, can add simultaneously
-- (siblings, cousins, uncle/niece, etc).

editCPoint :: User -> S.Point2 -> CPoint
editCPoint u p0 = CPoint xp delete new
 where
   xp@(pos, grabbable) = editXPoint u p0

   delete = charPress 'd' u `whenE` grabbable

   new = (nextUser_ (charPress 'a') u `whenE`    grabbable
                                      ==>        trace "edit add\n"
                                      `snapshot` (pos .+^ shift)
                                      ==>        uncurry editCPoint)
         `untilB` delete -=> neverE

shift = vector2XY 0.5 0

renderCPoint :: CPoint -> ImageB
renderCPoint (CPoint xp delete new) =
  newStuff `over` (renderXPoint xp `untilB` delete -=> emptyImage)
 where
   newStuff = accumB over emptyImage (new ==> trace "render add\n" ==> renderCPoint)

ed3 u = renderCPoint (editCPoint u S.origin2)