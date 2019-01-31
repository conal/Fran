{- RBMH 2D (static) Vectors

 Last modified Fri Sep 13 11:29:46 1996
-}
module Vector2 
        (
         Vector2(..),
           -- instance Vector2 (Eq,Text,Num,VectorSpace,Transformable2)
         xVector2, yVector2,    -- :: Vector2, Vector2  -- unit vectors

         vector2XY,             -- :: RealVal -> RealVal -> Vector2
         vector2Polar,          -- :: Length  -> Radians -> Vector2
         vector2XYCoords,       -- :: Vector2 -> (RealVal, RealVal)
         vector2PolarCoords,    -- :: Vector2 -> (Length, Radians)
         -- instance  VectorSpace Vector2
        ) where

import BaseTypes
import VectorSpace

data Vector2 = Vector2XY Double Double   deriving (Eq,Text)

vector2XY :: RealVal -> RealVal -> Vector2
vector2XY = Vector2XY

xVector2,yVector2 :: Vector2
xVector2 = Vector2XY 1 0
yVector2 = Vector2XY 0 1

vector2XYCoords :: Vector2 -> (RealVal,RealVal)
vector2XYCoords (Vector2XY x y) = (x,y)

vector2Polar :: Length -> Radians -> Vector2
vector2Polar rho theta = Vector2XY (rho * cos theta) (rho * sin theta)

vector2PolarCoords :: Vector2 -> (Length,Radians)
vector2PolarCoords v@(Vector2XY x y) =
 (magnitude v,
  if (x==0 && y==0) then 0 else atan2 y x)

instance  VectorSpace Vector2  where
  zeroVector = Vector2XY 0 0

  scaleVector sc (Vector2XY dx dy) = Vector2XY (sc*dx) (sc*dy)

  addVector (Vector2XY dx1 dy1) (Vector2XY dx2 dy2) =
    Vector2XY (dx1+dx2) (dy1+dy2)

  (Vector2XY dx1 dy1) `dot` (Vector2XY dx2 dy2)  =  dx1*dx2 + dy1*dy2
