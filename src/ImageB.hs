-- ImageB = 2d Image behaviors
-- 
-- Last modified Wed Oct 23 22:40:15 1996

module ImageB where

import qualified Image as I
import qualified Pick2Image as PI
import Behavior

type ImageB = Behavior I.Image

-- Hugs bug (?) workaround.  See comment in VectorSpaceB.
-- infixl 6 `over`

emptyImage   = lift0 I.emptyImage
circle       = lift0 I.circle
square       = lift0 I.square
line         = lift2 I.line
polyline ls  = lift1 I.polyline (liftLs ls)
polygon ls   = lift1 I.polygon (liftLs ls)
bezier       = lift4 I.bezier
renderedText = lift1 I.renderedText
withColor    = lift2 I.withColor
over         = lift2 I.over
bboxed2      = lift3 I.bboxed2
ellipse      = lift1 I.ellipse
rectangle    = lift1 I.rectangle
regularPolygon = lift1 I.regularPolygon
star         = lift2 I.star
unitBBoxed2  = lift1 I.unitBBoxed2

pick2        = lift2 PI.pick2

-- Importation still acts on strings -- not string behaviors
importBitmap fileName  = lift0 (I.importBitmap fileName)
bitmap size hBmap      = lift0 (I.bitmap size hBmap)

squareMin     = lift0 I.squareMin
squareMax     = lift0 I.squareMax
circleRadius  = lift0 I.circleRadius

-- *% handled in general in Transform2B.hs

