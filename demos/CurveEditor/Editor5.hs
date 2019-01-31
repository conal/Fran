-- Curve editor, version 5.  Attempt at "undo".

module Editor5 ( XPoint, renderXPoint, editXPoint, editXPointTest
               , pointSize, grabDistance, graphPaper, whiteIm
               , renderCurve, EditCurve, editCurve
               , editorWith, editor, main
               ) where

import Editor4 hiding (editXPoint, editXPointTest, editCurve, editor, main )

import Fran
import qualified StaticTypes as S
import FileUtils
import Stacker


editXPoint :: User -> S.Point2 -> XPoint
editXPoint u p0 = (pos, closeEnough)
 where
   pos = switcher (constantB p0) $
              grab `snapshot` mouse u ==> relMotion
          .|. release                 ==> constantB
    where
      relMotion (p, mp) = constantB p .+^ (mouse u .-. constantB mp)

      grab     = lbp u `whenE` closeEnough `snapshot_` pos
      release  = lbr u `whenE` grabbing    `snapshot_` pos
             .|. resetTo
      grabbing = stepper False (grab -=> True .|. release -=> False)

      -- Stack the grabs and use control-Z to undo
      resetTo  = stacker grab (charPress '\^Z' u)

   closeEnough = distance2 pos (mouse u) <* grabDistance

-- For visualizing the moving point and the undo stack.
editXPointTest u = renderXPoint (editXPoint u S.origin2) `over`
                   whiteIm

-- Everything else is the same, but...
-- 
-- Oops!  How to combine the editXPoints into editCurve?  As it is, a
-- control-Z will undo each control point's most recent change!  Save the
-- solution for the next version.

editXPointTest u = renderXPoint (editXPoint u S.origin2)

editCurve :: [S.Point2] -> User -> [XPoint]
editCurve initPoints u = map (editXPoint u) initPoints

editor :: String -> IO ()
editor = editorWith "5" editCurve


main = editor "double.crv"
