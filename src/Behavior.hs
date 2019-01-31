-- Non-reactive behaviors
-- 
-- Last modified Fri Oct 11 14:39:16 1996

module Behavior where

import BaseTypes
import VectorSpace

type Time = Double

-- Behavior map from time streams to value streams, rather than a single
-- time, because some behaviors want to use incremental evaluation
-- techniques (esp. integral and untilB).

-- To do:
--  + Put interval sampling back.
--  + Dynamic constant folding

type Sampler a = [Time] -> [a]

data Behavior a = Behavior (Sampler a)

~(Behavior fl) `ats` ts = fl ts

-- Convenient type synonyms

type TimeB = Behavior Time
type BoolB = Behavior Bool

type RealValB  =  Behavior RealVal
type LengthB   =  Behavior Length
type RadiansB  =  Behavior Radians
type FractionB =  Behavior RealVal


time :: Behavior Time

time = Behavior id


timeTransform :: Behavior a -> Behavior Time -> Behavior a

(Behavior afl) `timeTransform` (Behavior tfl) =  Behavior (afl . tfl)



lift0 x = Behavior (map (const x))

lift1 :: (a -> b) -> (Behavior a) -> (Behavior b)

lift1 f b = Behavior (\ts -> map f (b `ats` ts))

lift2 f b1 b2 =
  Behavior (\ts -> zipWith f (b1 `ats` ts) (b2 `ats` ts))

lift3 f b1 b2 b3 =
  Behavior (\ts -> zipWith3 f (b1 `ats` ts) (b2 `ats` ts) (b3 `ats` ts))


lift4 f (Behavior ats1) (Behavior ats2) (Behavior ats3) (Behavior ats4) =
  Behavior (\ts -> zipWith4 f (ats1 ts) (ats2 ts) (ats3 ts) (ats4 ts))


zipWith4 :: (a -> b -> c -> d -> e)
	 -> [a]
         -> [b]
         -> [c] 
         -> [d] 
         -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _  _ _ _ _ = []

merge :: [[a]] -> [[a]]
merge lss = 
  case decons lss of
    (a,lss') -> a:merge lss'
  where
  --decons :: [[a]] -> [([a],[[a]])]
  decons lss = 
     foldr (\ (a,as) (acc,accs) -> (a:acc,as:accs))
	   ([],[])
	   (map (\ (x:xs) -> (x,xs)) lss)
     

liftLs :: [Behavior a] -> Behavior [a]
liftLs ls = 
 Behavior 
   (\ts -> 
     let
      ass = map (`ats` ts) ls
     in
     merge ass)

-- and so on


-- Define number-valued behaviors as numbers.  Sadly, type restrictions
-- prevent us from defining some of the methods on behaviors.

noOverload suffix name =
 error ("Couldn't overload \"" ++ name ++ "\" for behaviors.  Use \"" ++
        name ++ suffix ++"\" instead.")

noOverloadId = noOverload "B"
noOverloadOp = noOverload "*"


instance (Text a) => Text (Behavior a)

-- Needed because Num derives from Eq and Text.  Similarly below.
instance  (Eq a) => Eq (Behavior a)  where
  (==) = noOverloadOp "=="
  (/=) = noOverloadOp "/="

instance  Num a => Num (Behavior a) where
  (+)          =  lift2 (+)
  (*)          =  lift2 (*)
  negate       =  lift1 negate
  abs          =  lift1 abs
  fromInteger  =  lift0 . fromInteger
  fromInt      =  lift0 . fromInt

fromIntegerB :: Num a => Behavior Integer -> Behavior a
fromIntegerB = lift1 fromInteger

instance  Ord a => Ord (Behavior a)  where
  (<)   =  noOverloadOp "<"
  (<=)  =  noOverloadOp "<="
  (>)   =  noOverloadOp ">"
  (>=)  =  noOverloadOp ">="
  min	=  lift2 min
  max	=  lift2 max

instance  Real a => Real (Behavior a)  where
  toRational = noOverloadId "toRational"

toRationalB ::  Real a => Behavior a -> Behavior Rational
toRationalB = lift1 toRational


instance Integral a => Enum (Behavior a)
instance Integral a => Integral (Behavior a)  where
    quot      = lift2 quot
    rem	      =	lift2 rem
    div	      =	lift2 div
    mod	      =	lift2 mod
    quotRem x y  = pairBSplit (lift2 quotRem x y)
    divMod  x y  = pairBSplit (lift2 divMod  x y)
    toInteger = noOverloadId "toInteger"
    even      = noOverloadId "even"
    odd	      =	noOverloadId "odd"
    toInt     =	noOverloadId "toInt"


toIntegerB :: Integral a => Behavior a -> Behavior Integer
evenB, oddB :: Integral a => Behavior a -> Behavior Bool
toIntB      :: Integral a => Behavior a -> Behavior Int

toIntegerB = lift1 toInteger
evenB	   = lift1 even
oddB	   = lift1 odd
toIntB	   = lift1 toInt

instance (Fractional a) => Fractional (Behavior a) where
  fromDouble   =  lift0 . fromDouble
  fromRational =  lift0 . fromRational
  (/)          =  lift2 (/)

instance (Floating a) => Floating (Behavior a) where
  sin  =  lift1 sin
  cos  =  lift1 cos
  tan  =  lift1 tan
  asin =  lift1 asin
  acos =  lift1 acos
  atan =  lift1 atan
  sinh =  lift1 sinh
  cosh =  lift1 cosh
  tanh =  lift1 tanh
  asinh =  lift1 asinh
  acosh =  lift1 acosh
  atanh =  lift1 atanh

  pi   =  lift0 pi
  exp  =  lift1 exp
  log  =  lift1 log
  sqrt =  lift1 sqrt
  (**) =  lift2 (**)
  logBase = lift2 logBase


-- The types are too general here.  For all (Integral b), we need to
-- handle b, but can only handle Behavior b.  (See prelude.hs.)

instance  RealFrac a => RealFrac (Behavior a)  where
    properFraction = noOverloadId "properFraction"
    truncate	   = noOverloadId "truncate"
    round	   = noOverloadId "round"
    ceiling	   = noOverloadId "ceiling"
    floor	   = noOverloadId "floor"


properFractionB	  :: (RealFrac a, Integral b) => Behavior a -> Behavior (b,a)
truncateB, roundB :: (RealFrac a, Integral b) => Behavior a -> Behavior b
ceilingB, floorB  :: (RealFrac a, Integral b) => Behavior a -> Behavior b

properFractionB = lift1 properFraction
truncateB = lift1 truncate
roundB    = lift1 round
ceilingB  = lift1 ceiling
floorB    = lift1 floor

{-
instance  RealFloat a => RealFloat (Behavior a)  where
  ... types too restrictive to be liftable ...
-}

{- We could also add these.  Need type declarations.

floatRadixB      = lift1 floatRadix
floatDigitsB	 = lift1 floatDigits
floatRangeB	 = pairBSplit . lift1 floatRange
decodeFloatB	 = pairBSplit . lift1 decodeFloat
encodeFloatB	 = lift2 encodeFloat
exponentB	 = lift1 exponent
significandB	 = lift1 significand
scaleFloatB	 = lift2 scaleFloat
isNaNB		 = lift1 isNaN
isInfiniteB	 = lift1 isInfinite
isDenormalizedB  = lift1 isDenormalized
isNegativeZeroB  = lift1 isNegativeZero
isIEEEB		 = lift1 isIEEE

-}
  
-- Non-overloadable numeric functions

-- Various flavors of exponentiation
infixr 8  ^*, ^^*

(^*)	:: (Num a, Integral b) => Behavior a -> Behavior b -> Behavior a
(^^*)	:: (Fractional a, Integral b) => Behavior a -> Behavior b -> Behavior a

(^*)  = lift2 (^)
(^^*) = lift2 (^^)


-- ... fill in Num subclasses ...


-- Equality.  (Can't overload (==) because of result type.  Oh well.)

infix  4 ==*, <*, <=* , >=*, >*
infixr 3 &&*
infixr 2 ||*

{- What we want:

   class Ordered a b where
     (<=) :: a -> a -> b
-}

(==*),(/=*) :: Eq a => Behavior a -> Behavior a -> Behavior Bool
(==*) = lift2 (==)
(/=*) = lift2 (/=)

(<*),(<=*),(>=*),(>*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(<*)  = lift2 (<)
(<=*) = lift2 (<=)
(>=*) = lift2 (>=)
(>*)  = lift2 (>)

cond :: Behavior Bool -> Behavior a -> Behavior a -> Behavior a
cond = lift3 (\ a b c -> if a then b else c)

notB :: Behavior Bool -> Behavior Bool
notB = lift1 (not)

(&&*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&) 
(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(||*) = lift2 (||) 


-- Pair formation and extraction

pairB = lift2 (\ x y -> (x,y))
fstB  = lift1 fst
sndB  = lift1 snd

-- List formation and extraction


pairBSplit :: Behavior (a,b) -> (Behavior a, Behavior b)

pairBSplit b = (fstB b, sndB b)

nilB  = lift0 []
consB = lift2 (:)
headB = lift1 head
tailB = lift1 tail

-- Other

showB :: (Text a) => Behavior a -> Behavior String

showB = lift1 show

-- Testing

tstB b = b `ats` [0, 0.1 .. 3]

b1 = time
b2 = 5 + 3*time
b3 = sin time
