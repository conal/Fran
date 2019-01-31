-- Non-reactive behaviors
-- 
-- Last modified Mon Sep 16 14:33:54 1996

module Behavior where

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

{- There is no zipWith4.  Wait until needed.
lift4 f b1 b2 b3 b4 =
  Behavior (\ts -> zipWith4 f (b1 `ats` ts) (b2 `ats` ts) (b3 `ats` ts)
  (b4 `ats` ts))
-}

-- and so on


-- Needed because Num derives from Eq and Text
instance (Eq a) => Eq (Behavior a)
instance Text (Behavior a)

-- Define number-valued behaviors as numbers

instance  Num a => Num (Behavior a) where
  (+)          =  lift2 (+)
  (*)          =  lift2 (*)
  negate       =  lift1 negate
  abs          =  lift1 abs
  fromInteger  =  lift0 . fromInteger
  fromInt      =  lift0 . fromInt

instance (Fractional a, Ord a) => Fractional (Behavior a) where
  fromDouble  =  lift0 . fromDouble
  (/)         =  lift2 (/)

instance (Floating a, Ord a) => Floating (Behavior a) where
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
  

-- ... fill in Num subclasses ...


-- Equality.  (Can't overload (==) because of result type.  Oh well.)

infix  4 ==* 
infix  4 <* 
infix  4 <=* 
infix  4 >=* 
infix  4 >*
infixr 3 ||*
infixr 4 &&*

{- What we want:

   class Ordered a b where
     (<=) :: a -> a -> b
-}

(<*),(<=*),(==*),(>=*),(>*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(<*)  = lift2 (<)
(<=*) = lift2 (<=)
(==*) = lift2 (==)
(>=*) = lift2 (>=)
(>*)  = lift2 (>)

minB,maxB :: Ord a => Behavior a -> Behavior a -> Behavior a
minB = lift2 (min)
maxB = lift2 (max)

cond :: Ord a => Behavior Bool -> Behavior a -> Behavior a -> Behavior a
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


-- Testing

tstB b = b `ats` [0, 0.1 .. 3]

b1 = time
b2 = 5 + 3*time
b3 = sin time
