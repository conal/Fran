-- Test space trees

module Main where

import Fran
import qualified StaticTypes as S

import SpaceTree

import Random
import List (zipWith4)
import Maybe (fromMaybe)



-- Testing

data Ball = Ball S.Point2 RealVal S.Color -- center and radius
 deriving Show

instance Bounded2 Ball where
  bbox (Ball center radius _) =
    S.rectFromCenterSize center (S.vector2XY radius radius)

instance Renderable Ball where
  render (Ball center radius color) =
    moveTo  (constantB center)  $
    stretch (constantB radius)  $
    withColor (constantB color) $
    circle

randomBalls :: Integer -> [Ball]
randomBalls s = zipWith4 mkBall xs ys rs hs
 where
   mkBall x y r h = Ball (S.Point2XY x y)
                         (abs r / 10)
                         (S.colorHSL (h * pi) 0.5 0.5)
   xs = randDoubles $ s + 100
   ys = randDoubles $ s + 200
   rs = randDoubles $ s + 333
   hs = randDoubles $ s + 500


randDoubles :: Integer -> [Double]
randDoubles s = map frac (random (0, biggest) s)
 where
   frac i = 2 * (fromInteger i / fromInteger biggest - 0.5)
   biggest = fromInt maxBound

bs1 n = take n $ randomBalls 1000

windowBox = S.rectFromCenterSize S.origin2 (S.vector2XY 2 2)

tst1 n = mkPTree windowBox (bs1 n)

-- The test: place a bunch of random balls and display a small cursor ball
-- that is white when not touching any ball and otherwise gets its color
-- from some ball it touches.
pickTest n u = pickCursor `over` render balls
 where
   pickCursor = moveTo cPos $
                withColor cColor $
                stretch (constantB cRadius) $
                circle

   cPos    = mouse u
   cRadius = 0.1
   cSize   = S.vector2XY cRadius cRadius
   cColor  = lift2 fromMaybe white (lift1 search cPos)
   search cPos = searchPTree ptree cursorRect touchColor
    where
      cursorRect = S.rectFromCenterSize cPos cSize
      -- The color of a ball if being touched
      touchColor (Ball bPos bRadius bColor)
        | S.distance2 bPos cPos < bRadius+cRadius = Just bColor
        | otherwise                                 = Nothing
   ptree  = mkPTree windowBox balls
   balls  = take n $ randomBalls 1000

main = displayU $ pickTest 30