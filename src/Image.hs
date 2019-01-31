{- Definition of 2d static Image type -}

-- Last modified Sat Oct 26 22:51:52 1996

-- To do: be selective about exports

module Image where

import Win32 (HBITMAP)
import qualified Bitmap
import BaseTypes
import Point2
import Vector2
import Transform2
import Color
import Text

infixl 6 `over`

data Image
 = EmptyImage
 | Circle                            -- origin-centered, unit radius
 | Square                            -- origin-centered, unit side
 | Bitmap    Vector2 HBITMAP  -- x v >=0 && y v >=0
 | Line      Point2 Point2
 | Polyline  [Point2]
 | Polygon   [Point2]
 | Bezier    Point2 Point2 Point2 Point2 -- just cubics
 | RenderedText TextT
 | WithColor Color Image   -- add more stuff later.
 | Over      Image Image
 | TransformedImage Transform2 Image
 | BBoxed2   Point2 Point2 Image  -- min xy and max xy

instance Text Image where
  showsPrec p im =
   case im of
     EmptyImage         -> showString "Empty"
     Circle             -> showString "Circle"
     Square             -> showString "Square"
     Bitmap size _      -> showString "Bitmap "  . showsPrec p size
     Line p1 p2         -> showString "Line " . showsPrec 10 p1 .
                           showString " " . showsPrec 10 p2
     Polyline ls        -> showString "Polyline " . showsPrec 10 ls
     Polygon ls         -> showString "Polygon " . showsPrec 10 ls
     Bezier p1 p2 p3 p4 -> showString "Bezier " . shows [p1,p2,p3,p4]
     RenderedText t     -> showsPrec p t
     WithColor c im     -> showString "WithColor " .
                           showsPrec p c . showsPrec p im
     Over im1 im2       -> showString "Over " . showsPrec p im1 .
                           showString " " . showsPrec p im2
     TransformedImage t im
                        -> showString "TransformedImage " . showsPrec p t .
                           showString " " . showsPrec p im
     BBoxed2 p1 p2 im   -> showString "BBoxed2 " . showsPrec p p1 .
                           showsPrec p p2 . showsPrec p im


-- Primitives

emptyImage   = EmptyImage
circle       = Circle
square       = Square
bitmap       = Bitmap
line         = Line
polyline     = Polyline
polygon      = Polygon
bezier       = Bezier
renderedText = RenderedText
withColor    = WithColor
over         = Over
bboxed2      = BBoxed2

instance Transformable2 Image where
 (*%) = TransformedImage

-- Min and max points of "square" of unit "radius"
squareMin = point2Polar 1 (pi/4)
squareMax = point2Polar 1 (pi + pi/4)

squarePoints = map (\ i -> point2Polar 1 (pi/4 + i * pi/2)) [0 .. 4]

-- Could define square as (polyline squarePoints), but keep primitive for
-- picking and eventual faster display.

-- Radius of "circle"
circleRadius = 1 :: RealVal

-- Pixels per length unit, horizontal and vertical
pixelsPerLengthHorizontal = 200 :: Double
pixelsPerLengthVertical   = 200 :: Double

screenSizeToVector2 :: (Int,Int) -> Vector2

screenSizeToVector2 (w,h) =
  vector2XY (fromInt w / pixelsPerLengthHorizontal)
            (fromInt h / pixelsPerLengthVertical)

vector2ToScreenSize :: Vector2 -> (Int,Int)

vector2ToScreenSize v =
  ( round (dx * pixelsPerLengthHorizontal) ,
    round (dy * pixelsPerLengthVertical  ) )
  where
    (dx,dy) = vector2XYCoords v


-- Derived image functions

ellipse :: Vector2 -> Image
ellipse size = scale2 size *% circle

rectangle :: Vector2 -> Image
rectangle size = scale2 size *% square


regularPolygon :: Int -> Image

regularPolygon vertices = star vertices 1

star :: Int -> Int -> Image

star vertices skip = polygon points
 where
  points = [ point2Polar 1 (2 * pi * fromInt i * fromInt skip
                            / fromInt vertices) |
             i <- [0 .. vertices] ]

importBitmap :: String -> Image
importBitmap fileName =
  unsafePerformIO $
   ( Bitmap.loadBitmap fileName >>= \ (bm,w,h) ->
     return (bitmap (screenSizeToVector2 (w,h)) bm) ) `catch` \ e ->
   putStrLn ("Warning: couldn't load bitmap " ++ fileName ++
             ".  Substituting empty image.") >>
   return emptyImage


-- Bounding box wrapper 
unitBBoxed2 :: Image -> Image

unitBBoxed2 = bboxed2 (point2XY (-1) (-1))
                      (point2XY   1    1 )


