{- RBMH 2D (static) Vectors

 Last modified Fri Oct 10 15:50:44 1997
-}
module Vector2 
        (
          Vector2(..)
          -- instance Vector2 (Eq,Show,Num,VectorSpace,Transformable2)
        , xVector2, yVector2    -- :: Vector2, Vector2  -- unit vectors

        , vector2XY             -- :: RealVal -> RealVal -> Vector2
        , vector2Polar          -- :: Length  -> Radians -> Vector2
        , vector2XYCoords       -- :: Vector2 -> (RealVal, RealVal)
        , vector2PolarCoords    -- :: Vector2 -> (Length, Radians)
        -- instance  VectorSpace Vector2
        , rotateVector2         -- :: RealVal -> Vector2 -> Vector2
        ) where

import BaseTypes
import VectorSpace
import Force

data Vector2 = Vector2XY Double Double   deriving (Eq,Show)

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
 -- Watch out for zero.  Hmm... Should probably be in behavior atan2
 (magnitude v,
  if (x==0 && y==0) then 0 else atan2 y x)

instance  VectorSpace Vector2  where
  zeroVector = Vector2XY 0 0

  sc *^ (Vector2XY dx dy) = Vector2XY (sc*dx) (sc*dy)

  (Vector2XY dx1 dy1) ^+^ (Vector2XY dx2 dy2) =
    Vector2XY (dx1+dx2) (dy1+dy2)

  (Vector2XY dx1 dy1) `dot` (Vector2XY dx2 dy2)  =  dx1*dx2 + dy1*dy2

instance  Num Vector2  where
  (+)    = (^+^)
  (*)	 = error "bad operator on Vector2: *"
  abs	 = error "bad operation on Vector2: abs"
  signum = error "bad operation on Vector2: signum"
  negate = negateVector
  -- (-) follows from negate and +
  fromInteger = error "Can't interpret integers as Vector2"


instance Forceable Vector2 where
  force v@(Vector2XY x y) = force x `seq` force y `seq` v


-- This is here so we can use + and - on Vector2B.
instance Ord Vector2


rotateVector2 :: RealVal -> Vector2 -> Vector2
rotateVector2 theta' vec = vector2Polar (theta'+theta) rho
 where
  (theta, rho) = vector2PolarCoords vec
