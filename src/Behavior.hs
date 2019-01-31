-- Non-reactive behaviors
-- 
-- Last modified Tue Oct 15 22:55:25 1996
-- 
-- To do:
--   + Dynamic constant folding

module Behavior where

import VectorSpace

-- A behavior maps a time to a value plus and another behavior:

type Time  = Double

type Sampler a = Time -> (a, Behavior a)

data Behavior a = Behavior (Sampler a)

-- with the understanding that the resulting behavior be given times no
-- later than the one that produced it (a monotonicity property).

-- Simon PJ pointed out that this representation should deforest much more
-- easily than my previous representation as a function from time streams
-- to value streams, and should be nicely susceptible to lazy memoization.
-- Also, it should eliminate the unsafeInterleaveIO hack I had to use for
-- creating time streams.

-- A behavior is sampled via the "at" function:

at :: Behavior a -> Sampler a

(Behavior f `at`) = f

-- We define the usual assortment of behavior primitives and building
-- blocks.  "time" maps a time to itself, yielding itself as the
-- "new" behavior.

time :: Behavior Time

time = Behavior (\ t -> (t, time))

-- Time transformation is semantically equivalent to function composition.
-- Upon sampling, the new behavior is constructed from the new behavior
-- argument and time transformation.

timeTransform :: Behavior a -> Behavior Time -> Behavior a

b `timeTransform` tt =
  Behavior (\ t ->
    let
      (ttVal, tt') = tt `at` t
      (bVal , b' ) = b  `at` ttVal
    in
      (bVal, b' `timeTransform` tt') )

-- Lifting is not quite as simple as before, but is still reasonable.
-- Zero-ary lifting ignores the given time, and gives back the same
-- value/behavior pair:

lift0 x = b
  where
    b    = Behavior (\ t -> pair)
    pair = (x, b)

-- For n>0, n-ary lifting samples the component behaviors applies the
-- unlifted function to the resulting values, and applies the lifted
-- function to the resulting behaviors.

lift1 f b = Behavior sampler
  where sampler t = (f x, lift1 f b')
          where (x, b') = b `at` t

lift2 f b1 b2 = Behavior sampler
  where sampler t = (f x1 x2, lift2 f b1' b2')
          where (x1, b1') = b1 `at` t
                (x2, b2') = b2 `at` t

lift3 f b1 b2 b3 = Behavior sampler
  where sampler t = (f x1 x2 x3, lift3 f b1' b2' b3')
          where (x1, b1') = b1 `at` t
                (x2, b2') = b2 `at` t
                (x3, b3') = b3 `at` t

lift4 f b1 b2 b3 b4 = Behavior sampler
  where sampler t = (f x1 x2 x3 x4, lift4 f b1' b2' b3' b4')
          where (x1, b1') = b1 `at` t
                (x2, b2') = b2 `at` t
                (x3, b3') = b3 `at` t
                (x4, b4') = b4 `at` t

-- and so on.

liftLs :: [Behavior a] -> Behavior [a]
liftLs bs = Behavior ( \t ->
  let (xs, bs') = unzip (map (`at` t) bs) in
    (xs, liftLs bs') )

-- End of primitives

-- A few convenient type abbreviations:

type BoolB = Behavior Bool
type TimeB = Behavior Time

-- Now we define a bazillion liftings.  The naming policy is to use the same
-- name as the unlifted function if overloaded and the types permit.
-- Otherwise, add a suffix of "*" or "B" if the name is a syntactic
-- "operator" rather than an identifier.  (Does the Haskell language spec use
-- the terms "operator" and "identifier" in this way?)

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
  min   =  lift2 min
  max   =  lift2 max

instance  Real a => Real (Behavior a)  where
  toRational = noOverloadId "toRational"

toRationalB ::  Real a => Behavior a -> Behavior Rational
toRationalB = lift1 toRational


instance Integral a => Enum (Behavior a)
instance Integral a => Integral (Behavior a)  where
    quot      = lift2 quot
    rem       = lift2 rem
    div       = lift2 div
    mod       = lift2 mod
    quotRem x y  = pairBSplit (lift2 quotRem x y)
    divMod  x y  = pairBSplit (lift2 divMod  x y)
    toInteger = noOverloadId "toInteger"
    even      = noOverloadId "even"
    odd       = noOverloadId "odd"
    toInt     = noOverloadId "toInt"


toIntegerB :: Integral a => Behavior a -> Behavior Integer
evenB, oddB :: Integral a => Behavior a -> BoolB
toIntB      :: Integral a => Behavior a -> Behavior Int

toIntegerB = lift1 toInteger
evenB      = lift1 even
oddB       = lift1 odd
toIntB     = lift1 toInt

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
    truncate       = noOverloadId "truncate"
    round          = noOverloadId "round"
    ceiling        = noOverloadId "ceiling"
    floor          = noOverloadId "floor"


properFractionB   :: (RealFrac a, Integral b) => Behavior a -> Behavior (b,a)
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
floatDigitsB     = lift1 floatDigits
floatRangeB      = pairBSplit . lift1 floatRange
decodeFloatB     = pairBSplit . lift1 decodeFloat
encodeFloatB     = lift2 encodeFloat
exponentB        = lift1 exponent
significandB     = lift1 significand
scaleFloatB      = lift2 scaleFloat
isNaNB           = lift1 isNaN
isInfiniteB      = lift1 isInfinite
isDenormalizedB  = lift1 isDenormalized
isNegativeZeroB  = lift1 isNegativeZero
isIEEEB          = lift1 isIEEE

-}
  
-- Non-overloadable numeric functions

-- Various flavors of exponentiation
infixr 8  ^*, ^^*

(^*)    :: (Num a, Integral b) => Behavior a -> Behavior b -> Behavior a
(^^*)   :: (Fractional a, Integral b) => Behavior a -> Behavior b -> Behavior a

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

(==*),(/=*) :: Eq a => Behavior a -> Behavior a -> BoolB
(==*) = lift2 (==)
(/=*) = lift2 (/=)

(<*),(<=*),(>=*),(>*) :: Ord a => Behavior a -> Behavior a -> BoolB
(<*)  = lift2 (<)
(<=*) = lift2 (<=)
(>=*) = lift2 (>=)
(>*)  = lift2 (>)

cond :: BoolB -> Behavior a -> Behavior a -> Behavior a
cond = lift3 (\ a b c -> if a then b else c)

notB :: BoolB -> BoolB
notB = lift1 (not)

(&&*) :: BoolB -> BoolB -> BoolB
(&&*) = lift2 (&&) 
(||*) :: BoolB -> BoolB -> BoolB
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

b `ats` []      = []
b `ats` (t:ts') = x : (b' `ats` ts')
 where
  (x,b') = b `at` t

tstB b ts = b `ats` [0, 0.1 .. 3]

b1 = time
b2 = 5 + 3*time
b3 = sin time
