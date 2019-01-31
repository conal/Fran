-- "Possible polynomials", as described in continuous.txt

-- Last modified Thu Feb 27 15:01:07 1997

module Polynomial (
  MaybePoly(..)
  , at, truncatePoly, degreeAtMost
  , fromCoeffs, constant, scale
  , derivative, definiteIntegral, indefiniteIntegral
) where

import BaseTypes
import VectorSpace
import Maybe (fromMaybe)

infixl 7  `scale`

data MaybePoly a =
    ZeroPoly                            -- all done
  | Poly a (MaybePoly a)                -- coefficient and rest
  | NonPoly                             -- oops, not a polynomial

instance Eq (MaybePoly a) where
  (==)  =  opErrPoly "=="

-- Sample at a time.  Gives Nothing if it turns out not to be a
-- polynomial.  Warning: strict!
at :: VectorSpace a => MaybePoly a -> Time -> Maybe a

ZeroPoly `at` t = Just zeroVector

NonPoly  `at` t = Nothing

Poly c p `at` t = map (\ x -> c ^+^ t *^ x) (p `at` t)


-- Truncate a polynomial to at most n terms.  Yields a NonPoly if
-- the argument has a greater degree at least n.
-- Is this function really useful?

truncatePoly :: MaybePoly a -> Int -> MaybePoly a

truncatePoly _       0  = NonPoly
truncatePoly ZeroPoly _ = ZeroPoly
truncatePoly NonPoly  _ = NonPoly
truncatePoly (Poly c p) n = Poly c (truncatePoly p (n-1))


-- See if a real polynomial with at most degree d (assumed non-negative)

degreeAtMost :: MaybePoly a -> Int -> Bool

degreeAtMost ZeroPoly   _ = True
degreeAtMost NonPoly    _ = False
degreeAtMost (Poly _ p) d = d > 0 && degreeAtMost p (d-1)



instance (Num a, Show a) => Show (MaybePoly a) where
  showsPrec _    ZeroPoly = showString "ZeroPoly"
  showsPrec _    NonPoly = showString "NonPoly"
  showsPrec prec (Poly a p') =
     -- showString "<MaybePoly: " . shows a . loop 1 p' . showString ">"
     showParen (prec > 6) (shows a . loop 1 p')
   where
    loop n ZeroPoly = id

    loop n (Poly a p') =
      showString " + " . showCoeff a . showT n . loop (n+1) p'
    loop n NonPoly     = showString " + NonPoly"

    showCoeff 1 = id
    showCoeff a = shows a . showChar ' '

    showT 1 = showString "t"
    showT n = showString "t^" . shows n


fromCoeffs :: [a] -> MaybePoly a

fromCoeffs [] = ZeroPoly

fromCoeffs (c : coeffs') = Poly c (fromCoeffs coeffs')

constant :: a -> MaybePoly a
constant x = Poly x ZeroPoly

scale :: Num a => a -> MaybePoly a -> MaybePoly a
scale s = loop
 where
  loop ZeroPoly   = ZeroPoly
  loop (Poly c p) = Poly (s * c) (loop p)
  loop NonPoly    = NonPoly

timesTPoly :: Num a => MaybePoly a -> MaybePoly a

timesTPoly ZeroPoly = ZeroPoly
timesTPoly p        = Poly 0 p

opNonPoly1 :: MaybePoly a -> MaybePoly a
opNonPoly1 = const NonPoly

opErrPoly :: String -> a
opErrPoly opName = error ("MaybePoly has no " ++ opName)


instance  Num a => Num (MaybePoly a)  where
  ZeroPoly + p' = p'
  p + ZeroPoly  = p
  Poly c p + Poly c' p' = Poly (c + c') (p + p')
  _ + _         = NonPoly

  -- multPoly thanks to Doug McIlroy's "Squinting at Power Series"
  ZeroPoly * _ = ZeroPoly
  _ * ZeroPoly = ZeroPoly
  Poly c p * Poly c' p' =
    Poly (c*c') (c `scale` p' + c' `scale` p +
                 timesTPoly (p * p'))
  _ * _        = NonPoly

  negate       =  scale (-1)

  abs          =  opErrPoly "abs"
  fromInteger  =  constant . fromInteger
  fromInt      =  constant . fromInt


instance  Fractional a => Fractional (MaybePoly a)  where
  -- Division.  Handle the easy case of constant divisor.  Could try
  -- to be a little smarter.  
  p / (Poly a ZeroPoly) = scale (recip a) p
  p / ZeroPoly          = error "MaybePoly: divide by zero"
  p / _                 = NonPoly

  fromRational = constant . fromRational
  fromDouble   = constant . fromDouble


instance  Floating a => Floating (MaybePoly a)  where
  pi    = fromDouble pi
  exp   = opNonPoly1
  log   = opNonPoly1
  sqrt  = opNonPoly1                    -- could try
  sin   = opNonPoly1
  cos   = opNonPoly1
  tan   = opNonPoly1
  asin  = opNonPoly1
  acos  = opNonPoly1
  atan  = opNonPoly1


-- MaybePoly as VectorSpace.  Sadly, I have to repeat scale and (+),
-- with minor changes, because Num and VectorSpace classes are
-- incomparable.

instance VectorSpace a => VectorSpace (MaybePoly a) where
  zeroVector  =  ZeroPoly

  (s *^) = loop
   where
    loop ZeroPoly   = ZeroPoly
    loop (Poly c p) = Poly (s *^ c) (loop p)
    loop NonPoly    = NonPoly

  ZeroPoly ^+^ p' = p'
  p ^+^ ZeroPoly  = p
  Poly c p ^+^ Poly c' p' = Poly (c ^+^ c') (p ^+^ p')
  _ ^+^ _         = NonPoly

  a `dot` b   =  opErrPoly "dot"


-- Differentiation and integration

derivative :: VectorSpace a => MaybePoly a -> MaybePoly a

derivative ZeroPoly   = ZeroPoly

derivative NonPoly    = NonPoly

derivative (Poly c p) = loop 1 p
 where
  loop n ZeroPoly   = ZeroPoly

  loop n (Poly c p) = Poly (fromInt n *^ c) (loop (n+1) p)

  loop _ NonPoly    = NonPoly


-- Indefinite integral

indefiniteIntegral :: VectorSpace a => MaybePoly a -> MaybePoly a

-- This "optimization" injects strictness, breaking recursive integration.

-- indefiniteIntegral ZeroPoly = ZeroPoly

indefiniteIntegral p        = Poly zeroVector (loop 0 p)
 where
  -- Construct result, starting with arg's t^n term and result's
  -- t^(n+1) term.
  loop n ZeroPoly   = ZeroPoly
  loop n (Poly c p) = Poly (c ^/ fromInt (n+1)) (loop (n+1) p)
  loop _ NonPoly    = NonPoly


-- Definite integral.  Warning: strict!

definiteIntegral :: VectorSpace a => MaybePoly a -> Time -> MaybePoly a

definiteIntegral p t0 = intP ^-^ constant intPat
 where
  intP = indefiniteIntegral p
  intPat = fromMaybe (error "definiteIntegral: not a polynomial")
                     (intP `at` t0)


-- Testing

p0, p1, p2 :: MaybePoly Float

p0  = ZeroPoly                          -- 0
p1  = fromCoeffs [2,1]                  -- t + 2
p2  = fromCoeffs [3,1]                  -- t + 3
p3  = p1 * p2                           -- t^2 + 5t + 6
p4  = p1 + p3                           -- t^2 + 6t + 8
p5  = p4 / 3                            -- 1/3 t^2 + 2 t + 8/3
p6  = p3 / p1                           -- NonPoly (better: p2)
p7  = tan p4                            -- NonPoly
p8  = derivative p4                     -- 2 t + 6
p9  = indefiniteIntegral p1             -- 1/2 t^2 + 2 t
p10 = indefiniteIntegral p8             -- t^2 + 6 t
p11 = definiteIntegral p8 3             -- t^2 + 6 t - 27

p12 = truncatePoly p12' 15

p12' :: MaybePoly Float
p12' = 1 + indefiniteIntegral p12'

-- [Just 0, Just 5, Just 30, Nothing, Just 27]
p20 = map (`at` 3) [p0, p1, p3, p7, p10] 
p21 = p3 `degreeAtMost` 1
p22 = p3 `degreeAtMost` 2
p23 = p3 `degreeAtMost` 3
p24 = p12' `degreeAtMost` 10
