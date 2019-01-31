-- Trimmed version of Tony's equations of motion demos.  Exhibits pretty
-- bad space properties.

module Motion where

import RBMH
import qualified StaticTypes as S

import qualified ShowImageB

disp imF = ShowImageB.disp (imF 0)


-- Given a Behavior function that represents the force on an object, returns
-- a tuple of the (displacement, velocity) of that object.
disVel force s0 v0 m t0 = (s, v)
  where
    s = s0 + (integral v t0)
    v = v0 + (integral a t0)
    a = (1/(lift0 m)) `scaleVector` (force t0)

fGravitationalAttraction m1 m2 s1 s2 = fMag `scaleVector` fDir
  where
    fDir = normalize (-s1Minuss2) 
    fMag = (lift0 (bigG*m1*m2)) / (magnitudeSquared s1Minuss2)
    s1Minuss2 = s1 - s2

fEarthGravity m t0 = (lift0 m) `scaleVector` a
  where
    a = vector2XY 0 (-9.81)    -- Constant acceleration approximates gravity.


-- Projectiles
aBody s t0 = (translate2 (s t0) *% uscale2 0.02 *% withColor blue circle)
	     `timeTransform`
	     (time/1)	

projectile sP0 vP0 mP t0 = fst $ disVel (fEarthGravity mP) sP0 vP0 mP t0

cannon t0 = aBody (projectile sP0 vP0 mP) t0
  where
    sP0 = vector2XY (-1) (-1)
    vP0 = vector2XY (1.4) (5.6)
    mP = 1


-- Gravitational attraction between two bodies.  Body A is fixed.  Body B is
-- free to move and acts under the influence of the gravitational force 
-- between itself and body A.

bigG :: Double
bigG = 1

gravAnim t0 = (translate2 (vector2XY 0 (-0)) *% 
           uscale2 0.02 *% (bodyA t0 `over` bodyB t0)) `timeTransform` 
           (time*1)

bodyA t0 = translate2 (sA t0) *% withColor blue circle

mA :: Double
mA = 1.0

sA t0 = sA'
  where
    (sA', vA) = disVel (\t0' -> 
	                   fGravitationalAttraction mA mB (sA t0') (sB t0')) 
                       sA0 vA0 mA t0
    sA0 = vector2XY (-10) 0    
    vA0 = vector2XY 0.0 (-1.0)

{-sA t0 = sA'
  where
    sA' = sA0 + (integral vA t0)
    vA = vA0 + (integral aA t0)
    aA = (1/(lift0 mA)) `scaleVector` (fA) 
    sA0 = vector2XY (-10) 0    
    vA0 = vector2XY 0.0 (-1.0)

    fA = fMag `scaleVector` fDir
      where
        fDir = normalize (-sAMinussB) 
        fMag = (lift0 (bigG * mA * mB)) / ((magnitudeSquared (sAMinussB)))
        sAMinussB = sA'- sB'
	sB' = sB t0-}
	

-- sB is fixed
bodyB t0 = translate2 (sB t0) *% withColor green circle

mB :: Double
mB = 10.0

sB t0 = vector2XY 0.0 0.0

{-sB t0 = sB'
  where
    sB' = sB0 + (integral vB t0)
    vB = vB0 + (integral aB t0)
    aB = (1/(lift0 mB)) `scaleVector` (fB) 
    sB0 = vector2XY 0.0 0.0    
    vB0 = vector2XY 0.0 (0.0)

    fB = fMag `scaleVector` fDir
      where
        fDir = normalize (sAMinussB) 
        fMag = (lift0 (bigG * mA * mB)) / ((magnitudeSquared (sAMinussB)))
        sAMinussB = sA'- sB'
	sA' = sA t0 -}

