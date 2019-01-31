{- RBMH 2D (static) Vectors

 Last modified Sat Sep 07 23:23:10 1996
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

data Vector2 = Vector2 Double Double   deriving (Eq,Text)

xVector2,yVector2 :: Vector2
xVector2 = Vector2 1 0
yVector2 = Vector2 0 1

vector2XY :: RealVal -> RealVal -> Vector2
vector2XY = Vector2

vector2XYCoords :: Vector2 -> (RealVal,RealVal)
vector2XYCoords (Vector2 x y) = (x,y)

vector2Polar :: Length -> Radians -> Vector2
vector2Polar rho theta = Vector2 (rho * cos theta) (rho * sin theta)

vector2PolarCoords :: Vector2 -> (Length,Radians)
vector2PolarCoords v@(Vector2 x y) = (atan2 y x, magnitude v)

instance  VectorSpace Vector2  where
  zeroVector = Vector2 0 0

  scaleVector sc (Vector2 dx dy) = Vector2 (sc*dx) (sc*dy)

  addVector (Vector2 dx1 dy1) (Vector2 dx2 dy2) =
    Vector2 (dx1+dx2) (dy1+dy2)

  (Vector2 dx1 dy1) `dot` (Vector2 dx2 dy2)  =  dx1*dx2 + dy1*dy2
