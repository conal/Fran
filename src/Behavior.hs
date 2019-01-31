-- Non-reactive behaviors
-- 
-- Last modified Fri Nov 08 09:07:21 1996
-- 
-- To do:
--   + Dynamic constant folding

module Behavior (
        Time, Behavior, at, during,
        lift0, lift1, lift2, lift3, lift4, liftLs,
	BoolB, TimeB,
        time, timeTransform,
        (^*),  (^^*),
        (==*), (<*), (<=*) , (>=*), (>*),
        cond, notB, (&&*), (||*),
        pairB, fstB, sndB, 
        pairBSplit,
        nilB, consB, headB, tailB,
        showB,  

        fromIntegerB,
        toRationalB,
        toIntegerB,
        evenB,
        oddB,
        toIntB,
        properFractionB,
        truncateB,
        roundB,
        ceilingB,
        floorB
        ) where

import BaseTypes
import Fuzzy
import VectorSpace

infixr 8  ^*, ^^*
infix  4 ==*, <*, <=* , >=*, >*
infixr 3 &&*
infixr 2 ||*


-- A behavior maps a time to a value plus and another behavior:

type Sampler a = Time -> (a, Behavior a)
type ISampler a = TimeI -> (Fuzzy a, Behavior a)

data Behavior a = Behavior (Sampler a) (ISampler a)

-- with the understanding that the resulting behavior be given times no
-- earlier than the one that produced it (a monotonicity property).

-- Simon PJ pointed out that this representation should deforest much more
-- easily than my previous representation as a function from time streams
-- to value streams, and should be nicely susceptible to lazy memoization.

-- A behavior is sampled via the "at" and "during" functions:

at :: Behavior a -> Sampler a

(Behavior f fi `at`) = f

during :: Behavior a -> ISampler a

(Behavior f fi `during`) = fi

-- We define the usual assortment of behavior primitives and building
-- blocks.  "time" maps a time to itself, yielding itself as the
-- "new" behavior.

time :: Behavior Time

time = Behavior (\ t -> (t, time)) (\ iv -> (iv , time))

-- Time transformation is semantically equivalent to function composition.
-- Upon sampling, the new behavior is constructed from the new behavior
-- argument and time transformation.

timeTransform :: Behavior a -> Behavior Time -> Behavior a

b `timeTransform` tt = Behavior sample isample
 where
  sample t = (bVal, b' `timeTransform` tt')
   where
    (ttVal, tt') = tt `at` t
    (bVal , b' ) = b  `at` ttVal
  isample iv = (bIv, b' `timeTransform` tt')
   where
    (ttIv, tt') = tt `during` iv
    (bIv,  b' ) = b  `during` ttIv


-- Lifting is not quite as simple as before, but is still reasonable.

lift0 :: a -> Behavior a
lift1 :: (a -> b) ->
         (Fuzzy a -> Fuzzy b) ->
         Behavior a -> Behavior b
lift2 :: (a -> b -> c) ->
         (Fuzzy a -> Fuzzy b -> Fuzzy c) ->
         Behavior a -> Behavior b -> Behavior c
lift3 :: (a -> b -> c -> d) ->
         (Fuzzy a -> Fuzzy b -> Fuzzy c -> Fuzzy d) ->
         Behavior a -> Behavior b -> Behavior c -> Behavior d
lift4 :: (a -> b -> c -> d -> e) -> 
         (Fuzzy a -> Fuzzy b -> Fuzzy c -> Fuzzy d -> Fuzzy e) -> 
         Behavior a -> Behavior b -> Behavior c -> Behavior d -> Behavior e

-- Zero-ary lifting ignores the given time, and gives back the same
-- value/behavior pair:

lift0 x = b
  where
    b     = Behavior (\ t -> vPair) (\ iv -> iPair)
    vPair = (x, b)
    iPair = (singleI x, b)

-- For n>0, n-ary lifting samples the component behaviors applies the
-- unlifted function to the resulting values, and applies the lifted
-- function to the resulting behaviors.

lift1 f fi b = Behavior sample isample
  where sample t = (f x, lift1 f fi b')
          where (x, b') = b `at` t
        isample iv = (fi xi, lift1 f fi b')
          where (xi, b') = b `during` iv

lift2 f fi b1 b2 = Behavior sample isample
  where sample t = (f x1 x2, lift2 f fi b1' b2')
          where (x1, b1') = b1 `at` t
                (x2, b2') = b2 `at` t
        isample iv = (fi xi1 xi2, lift2 f fi b1' b2')
          where (xi1, b1') = b1 `during` iv
                (xi2, b2') = b2 `during` iv

lift3 f fi b1 b2 b3 = Behavior sample isample
  where sample t = (f x1 x2 x3, lift3 f fi b1' b2' b3')
          where (x1, b1') = b1 `at` t
                (x2, b2') = b2 `at` t
                (x3, b3') = b3 `at` t
        isample iv = (fi xi1 xi2 xi3, lift3 f fi b1' b2' b3')
          where (xi1, b1') = b1 `during` iv
                (xi2, b2') = b2 `during` iv
                (xi3, b3') = b3 `during` iv

lift4 f fi b1 b2 b3 b4 = Behavior sample isample
  where sample t = (f x1 x2 x3 x4, lift4 f fi b1' b2' b3' b4')
          where (x1, b1') = b1 `at` t
                (x2, b2') = b2 `at` t
                (x3, b3') = b3 `at` t
                (x4, b4') = b4 `at` t
        isample iv = (fi xi1 xi2 xi3 xi4, lift4 f fi b1' b2' b3' b4')
          where (xi1, b1') = b1 `during` iv
                (xi2, b2') = b2 `during` iv
                (xi3, b3') = b3 `during` iv
                (xi4, b4') = b4 `during` iv

-- and so on.

liftLs :: [Behavior a] -> Behavior [a]
liftLs bs = Behavior sample (noI "liftLS")
  where
    sample t = (xs, liftLs bs')
      where (xs, bs') = unzip (map (`at` t) bs)



-- End of primitives

-- A few convenient type abbreviations:

type BoolB = Behavior Bool
type TimeB = Behavior Time

-- Now we define a bazillion liftings.  The naming policy is to use the
-- same name as the unlifted function if overloaded and the types permit.
-- Sadly, type restrictions prevent us from defining some of the methods
-- on behaviors.  In such cases, add a suffix of "*" or "B" if the name is
-- a syntactic "operator" rather than an identifier.  (Does the Haskell
-- language spec use the terms "operator" and "identifier" in this way?)

noOverloadB   :: String -> String -> a
noOverloadBId :: String -> a
noOverloadBOp :: String -> a

noOverloadB suffix name =
 error ("Couldn't overload \"" ++ name ++ "\" for behaviors.  Use \"" ++
        name ++ suffix ++"\" instead.")

noOverloadBId = noOverloadB "B"
noOverloadBOp = noOverloadB "*"


instance (Show a) => Show (Behavior a)

-- Needed because Num derives from Eq and Show.  Similarly below.
instance  (Eq a) => Eq (Behavior a)  where
  (==) = noOverloadBOp "=="
  (/=) = noOverloadBOp "/="

(==*),(/=*) :: (Ord a, Eq a) => Behavior a -> Behavior a -> BoolB

(==*) = lift2 (==) (==#)
(/=*) = lift2 (/=) (/=#)


instance  Ord a => Ord (Behavior a)  where
  (<)   =  noOverloadBOp "<"
  (<=)  =  noOverloadBOp "<="
  (>)   =  noOverloadBOp ">"
  (>=)  =  noOverloadBOp ">="
  min   =  lift2 min min
  max   =  lift2 max max

(<*),(<=*),(>=*),(>*) :: Ord a => Behavior a -> Behavior a -> BoolB

(<*)  = lift2 (<)  (<#)
(<=*) = lift2 (<=) (<=#)
(>=*) = lift2 (>=) (>=#)
(>*)  = lift2 (>)  (>#)


instance  (Ord a, Num a) => Num (Behavior a)  where
  (+)          =  lift2 (+) (+)
  (*)          =  lift2 (*) (*)
  negate       =  lift1 negate negate
  abs          =  lift1 abs abs
  fromInteger  =  lift0 . fromInteger
  fromInt      =  lift0 . fromInt

fromIntegerB :: Num a => Behavior Integer -> Behavior a
fromIntegerB = lift1 fromInteger fromIntegerI


instance  Real a => Real (Behavior a)  where
  toRational = noOverloadBId "toRational"

toRationalB ::  Real a => Behavior a -> Behavior Rational
toRationalB = lift1 toRational toRationalI


instance Enum a => Enum (Behavior a)
instance (Integral a) => Integral (Behavior a) where
    quot      = lift2 quot quot
    rem       = lift2 rem rem
    div       = lift2 div div
    mod       = lift2 mod mod
    quotRem x y  = pairBSplit (lift2 quotRem quotRemI x y)
    divMod  x y  = pairBSplit (lift2 divMod  divModI  x y)
    toInteger = noOverloadBId "toInteger"
    even      = noOverloadBId "even"
    odd       = noOverloadBId "odd"
    toInt     = noOverloadBId "toInt"

toIntegerB :: Integral a => Behavior a -> Behavior Integer
evenB, oddB :: Integral a => Behavior a -> BoolB
toIntB      :: Integral a => Behavior a -> Behavior Int

toIntegerB = lift1 toInteger toIntegerI
evenB      = lift1 even evenI
oddB       = lift1 odd oddI
toIntB     = lift1 toInt toIntI

instance (Ord a, Fractional a) => Fractional (Behavior a) where
  fromDouble   =  lift0 . fromDouble
  fromRational =  lift0 . fromRational
  (/)          =  lift2 (/) (/)

instance (Ord a, Floating a) => Floating (Behavior a) where
  sin  =  lift1 sin sin
  cos  =  lift1 cos cos
  tan  =  lift1 tan tan
  asin =  lift1 asin asin
  acos =  lift1 acos acos
  atan =  lift1 atan atan
  sinh =  lift1 sinh sinh
  cosh =  lift1 cosh cosh
  tanh =  lift1 tanh tanh
  asinh =  lift1 asinh asinh
  acosh =  lift1 acosh acosh
  atanh =  lift1 atanh atanh

  pi   =  lift0 pi
  exp  =  lift1 exp exp
  log  =  lift1 log log
  sqrt =  lift1 sqrt sqrt
  (**) =  lift2 (**) (**)
  logBase = lift2 logBase logBase


-- The types are too general here.  For all (Integral b), we need to
-- handle b, but can only handle Behavior b.  (See prelude.hs.)

instance  RealFrac a => RealFrac (Behavior a)  where
    properFraction = noOverloadBId "properFraction"
    truncate       = noOverloadBId "truncate"
    round          = noOverloadBId "round"
    ceiling        = noOverloadBId "ceiling"
    floor          = noOverloadBId "floor"

properFractionB   :: (RealFrac a, Integral b) => Behavior a -> Behavior (b,a)
truncateB, roundB :: (RealFrac a, Integral b) => Behavior a -> Behavior b
ceilingB, floorB  :: (RealFrac a, Integral b) => Behavior a -> Behavior b

properFractionB = lift1 properFraction properFractionI
truncateB = lift1 truncate truncateI
roundB    = lift1 round roundI
ceilingB  = lift1 ceiling ceilingI
floorB    = lift1 floor floorI

{-
instance  RealFloat a => RealFloat (Behavior a)  where
  ... types too restrictive to be liftable ...
-}

{- We could also add these.  Need type declarations.

floatRadixB      = lift1 floatRadix floatRadixI
floatDigitsB     = lift1 floatDigits floatDigitsI
floatRangeB      = pairBSplit . lift1 floatRange floatRangeI
decodeFloatB     = pairBSplit . lift1 decodeFloat decodeFloatI
encodeFloatB     = lift2 encodeFloat encodeFloatI
exponentB        = lift1 exponent exponentI
significandB     = lift1 significand significandI
scaleFloatB      = lift2 scaleFloat scaleFloatI
isNaNB           = lift1 isNaN isNaNI
isInfiniteB      = lift1 isInfinite isInfiniteI
isDenormalizedB  = lift1 isDenormalized isDenormalizedI
isNegativeZeroB  = lift1 isNegativeZero isNegativeZeroI
isIEEEB          = lift1 isIEEE isIEEEI

-}
  
-- Non-overloadable numeric functions

-- Various flavors of exponentiation
(^*)    :: (Ord a, Num a, Integral b) =>
	     Behavior a -> Behavior b -> Behavior a
(^^*)   :: (Ord a, Fractional a, Integral b) =>
	     Behavior a -> Behavior b -> Behavior a

(^*)  = lift2 (^) (^)
(^^*) = lift2 (^^) (^^)



cond :: Ord a => BoolB -> Behavior a -> Behavior a -> Behavior a
cond = lift3 (\ a b c -> if a then b else c)
             (\ ai bi ci -> bi `mergeI` ci)

notB :: BoolB -> BoolB
notB = lift1 (not) (notI)

(&&*) :: BoolB -> BoolB -> BoolB
(&&*) = lift2 (&&) (&&#) 
(||*) :: BoolB -> BoolB -> BoolB
(||*) = lift2 (||) (||#)

-- Pair formation and extraction

pairB = lift2 pair pairI
fstB  = lift1 fst fstI
sndB  = lift1 snd sndI

pairBSplit :: Behavior (a,b) -> (Behavior a, Behavior b)

pairBSplit b = (fstB b, sndB b)

-- List formation and extraction

nilB  = lift0 []
consB = lift2 (:) (noI ":")
headB = lift1 head (noI "head")
tailB = lift1 tail (noI "tail")

-- Other

showB :: (Show a) => Behavior a -> Behavior String

showB = lift1 show (noI "show")


-- Testing

b `ats` []      = []
b `ats` (t:ts') = x : (b' `ats` ts')
 where
  (x,b') = b `at` t

tstB b ts = b `ats` [0, 0.1 .. 3]

b1 = time
b2 = 5 + 3*time
b3 = sin time
