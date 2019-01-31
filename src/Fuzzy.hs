-- Fuzzy (interval) analysis.
-- 
-- Last modified Thu May 08 12:54:15 1997
-- 
-- I'm inconsistent with naming conventions.  Some derive from "fuzzy",
-- while others from "interval".

module Fuzzy where

import BaseTypes

infix 5 `Upto`

infixr 8  ^#, ^^#
infix  4 ==#, <#, <=# , >=#, >#
infixr 3 &&#
infixr 2 ||#


-- Fuzzy values are currently represented as an interval.  Could generalize,
-- e.g., to ordered disjoint sequences of intervals.

data Fuzzy a =  a `Upto` a  deriving Show

-- We can make a fuzzy value by providing two precise boundary values.

-- between :: Ord a => a -> a -> Fuzzy a

a `between` b  =  if  a > b  then  b `Upto` a  else  a `Upto` b

-- Or "singleton fuzzy value" from a single precise value:

singleI :: a -> Fuzzy a

singleI a = a `Upto` a

-- One common operation is merging two intervals into one:

mergeI :: Ord a => Fuzzy a -> Fuzzy a -> Fuzzy a

(lo `Upto` hi) `mergeI` (lo' `Upto` hi') =
  (lo `min` lo') `Upto` (hi `max` hi')

{-
f@(lo `Upto` hi) `mergeI` f'@(lo' `Upto` hi')
 | lo  > hi  = f'
 | lo' > hi' = f
 | otherwise = (lo `min` lo') `Upto` (hi `max` hi')
-}

-- A few convenient type abbreviations:

type BoolI = Fuzzy Bool
type TimeI = Fuzzy Time


-- Fuzzy mappers for monotonicaly non-decreasing functions

fuzzy1Inc f (lo `Upto` hi) = f lo `Upto` f hi

fuzzy2Inc f (lo1 `Upto` hi1) (lo2 `Upto` hi2) =
 f lo1 lo2 `Upto` f hi1 hi2

-- Fuzzy mappers for monotonicaly non-increasing functions

fuzzy1Dec f (lo `Upto` hi) = f hi `Upto` f lo

fuzzy2Dec f (lo1 `Upto` hi1) (lo2 `Upto` hi2) =
 f hi1 hi2 `Upto` f lo1 lo2

-- Undefined

noI name = error ("No fuzzy evaluation for " ++ name ++ " yet.  Sorry.")

-- Now we define a bazillion interval functions.  The naming policy is
-- to use the same name as the non-fuzzy function if overloaded and the
-- types permit.  Sadly, type restrictions prevent us from defining
-- some of the methods on behaviors.  In such cases, add a suffix of
-- "#" or "I" if the name is a syntactic "operator" rather than an
-- identifier.  (Does the Haskell language spec use the terms
-- "operator" and "identifier" in this way?)

noOverloadI   :: String -> String -> a
noOverloadIId :: String -> a
noOverloadIOp :: String -> a

noOverloadI suffix name =
 error ("Couldn't overload \"" ++ name
        ++ "\" for fuzzy values.  Use \""
        ++ name ++ suffix ++"\" instead.")

noOverloadIId = noOverloadI "I"
noOverloadIOp = noOverloadI "#"


-- Needed because Num derives from Eq and Show.  Similarly below.
instance  (Eq a) => Eq (Fuzzy a)  where
  (==) = noOverloadIOp "=="
  (/=) = noOverloadIOp "/="

(lo1 `Upto` hi1) ==# (lo2 `Upto` hi2)
  | hi1 < lo2 || hi2 < lo1            =  falseFalse  -- never equal
  | lo1==hi1 && lo2==hi2 && lo1==lo2  =  trueTrue    -- equal singletons
  |otherwise                          =  falseTrue   -- maybe

iv /=# iv' = notI (iv ==# iv')


instance  Ord a => Ord (Fuzzy a)  where
  (<)   =  noOverloadIOp "<"
  (<=)  =  noOverloadIOp "<="
  (>)   =  noOverloadIOp ">"
  (>=)  =  noOverloadIOp ">="
  min   =  fuzzy2Inc min
  max   =  fuzzy2Inc max

-- Could some of these use fuzzy2Inc, etc?

(lo1 `Upto` hi1) <# (lo2 `Upto` hi2)
 | lo1 >= hi2  = falseFalse -- i1 >= i2
 | hi1 < lo2   = trueTrue   -- i1 < i2
 | otherwise   = falseTrue

(lo1 `Upto` hi1) <=# (lo2 `Upto` hi2)
 | lo1 > hi2   = falseFalse -- i1 > i2
 | hi1 <= lo2  = trueTrue
 | otherwise   = falseTrue

(lo1 `Upto` hi1) >=# (lo2 `Upto` hi2)
 | hi1 < lo2   = falseFalse -- i1 <  i2
 | lo1 >= hi2  = trueTrue   -- i1 >= i2
 | otherwise   = falseTrue

(lo1 `Upto` hi1) ># (lo2 `Upto` hi2)
 | hi1 < lo2  = falseFalse -- i1 <= i2
 | hi1 > lo2  = trueTrue   -- i1 > i2
 | otherwise  = falseTrue


-- Next we define lots of fuzzy variants of existing functions.  Where
-- possible we overload.

instance (Num a, Ord a) => Num (Fuzzy a) where
  (+) = fuzzy2Inc (+)
  (a `Upto` b) * (c `Upto` d)  =
    least `Upto` most
    where
      ac    =  a*c; ad = a*d; bc = b*c; bd = b*d
      least =  (ac `min` bc) `min` (bc `min` bd)
      most  =  (ac `max` bc) `max` (bc `max` bd)
  -- fill in negate, abs, signum
  negate = fuzzy1Dec negate
  abs (lo `Upto` hi)
    | lo < 0 && 0 < hi = 0 `Upto` (abs lo `max` abs hi)
    | otherwise        = abs lo `between` abs hi
  fromInteger  =  singleI . fromInteger
  fromInt      =  singleI . fromInt

fromIntegerI :: Num a => Fuzzy Integer -> Fuzzy a
fromIntegerI = fuzzy1Inc fromInteger



instance  Real a => Real (Fuzzy a)  where
  toRational = noOverloadIId "toRational"

toRationalI ::  Real a => Fuzzy a -> Fuzzy Rational
toRationalI = fuzzy1Inc toRational


instance Enum a => Enum (Fuzzy a)
instance (Integral a) => Integral (Fuzzy a) where
    quot      = noI "quot"
    rem       = noI "rem"
    div       = noI "div"
    mod       = noI "mod"
    quotRem x y  = noI "quotRem"
    divMod  x y  = noI "divMod"
    toInteger = noOverloadIId "toInteger"
    even      = noOverloadIId "even"
    odd       = noOverloadIId "odd"
    toInt     = noOverloadIId "toInt"

divModI,quotRemI :: Integral a => Fuzzy a -> Fuzzy a -> Fuzzy (a,a)
divModI = error "no divModI, sorry"
quotRemI = error "no quotRemI, sorry"

toIntegerI :: Integral a => Fuzzy a -> Fuzzy Integer
evenI, oddI :: Integral a => Fuzzy a -> BoolI
toIntI      :: Integral a => Fuzzy a -> Fuzzy Int

toIntegerI = fuzzy1Inc toInteger
toIntI     = fuzzy1Inc toInt
oddI       = notI . evenI

evenI (n `Upto` m)  |  n==m      =  singleI (even n)
		    |  otherwise =  falseTrue


-- The Ord here result from the Ord premise in Num (Fuzzy a)

instance (Ord a, Fractional a) => Fractional (Fuzzy a) where
  fromDouble   =  singleI . fromDouble
  fromRational =  singleI . fromRational
  (/)          =  noI "/"

instance (Ord a, Floating a) => Floating (Fuzzy a) where
  sin  =  noI "sin"
  cos  =  noI "cos"
  tan  =  noI "tan"
  asin =  noI "asin"
  acos =  noI "acos"
  atan =  noI "atan"
  sinh =  noI "sinh"
  cosh =  noI "cosh"
  tanh =  noI "tanh"
  asinh =  noI "asinh"
  acosh =  noI "acosh"
  atanh =  noI "atanh"

  pi   =  singleI pi
  exp  =  fuzzy1Inc exp
  log  =  fuzzy1Inc log
  sqrt =  fuzzy1Inc sqrt
  (**) =  noI "**"
  logBase = noI "logBase"


-- The types are too general here.  For all (Integral b), we need to
-- handle b, but can only handle Fuzzy b.  (See prelude.hs.)

instance  RealFrac a => RealFrac (Fuzzy a)  where
    properFraction = noOverloadIId "properFraction"
    truncate       = noOverloadIId "truncate"
    round          = noOverloadIId "round"
    ceiling        = noOverloadIId "ceiling"
    floor          = noOverloadIId "floor"

properFractionI   :: (RealFrac a, Integral b) => Fuzzy a -> Fuzzy (b,a)
truncateI, roundI :: (RealFrac a, Integral b) => Fuzzy a -> Fuzzy b
ceilingI, floorI  :: (RealFrac a, Integral b) => Fuzzy a -> Fuzzy b

properFractionI = noI "properFraction"
truncateI = fuzzy1Inc truncate
roundI    = fuzzy1Inc round
ceilingI  = fuzzy1Inc ceiling
floorI    = fuzzy1Inc floor

{-
instance  RealFloat a => RealFloat (Fuzzy a)  where
  ... types too restrictive to be liftable ...
-}

{- We could also add these.  Need type declarations.

floatRadixI      = noI "floatRadix"
floatDigitsI     = noI "floatDigits"
floatRangeI      = pairBSplit . (noI "floatRange")
decodeFloatI     = pairBSplit . (noI "decodeFloat")
encodeFloatI     = noI "encodeFloat"
exponentI        = noI "exponent"
significandI     = noI "significand"
scaleFloatI      = noI "scaleFloat"
isNaNI           = noI "isNaN"
isInfiniteI      = noI "isInfinite"
isDenormalizedI  = noI "isDenormalized"
isNegativeZeroI  = noI "isNegativeZero"
isIEEEI          = noI "isIEEE"

-}
  
-- Non-overloadable numeric functions

-- Various flavors of exponentiation
(^#)    :: (Num a, Integral b) => Fuzzy a -> Fuzzy b -> Fuzzy a
(^^#)   :: (Fractional a, Integral b) => Fuzzy a -> Fuzzy b -> Fuzzy a

(^#)  = noI "^"
(^^#) = noI "^^"


-- Boolean ops

trueTrue, falseFalse, falseTrue :: Fuzzy Bool

falseFalse = False `Upto` False
trueTrue   = True  `Upto` True 
falseTrue  = False `Upto` True 

notI :: Fuzzy Bool -> Fuzzy Bool

notI (False `Upto` False) = trueTrue
notI (True  `Upto` True ) = falseFalse
notI (False `Upto` True ) = falseTrue

(&&#),(||#) :: Fuzzy Bool -> Fuzzy Bool -> Fuzzy Bool

(False `Upto` False) &&# _   = falseFalse
(True  `Upto` True ) &&# iv' = iv'
(False `Upto` True ) &&# (False `Upto` True ) = falseTrue
iv                   &&# iv' = iv' &&# iv

(False `Upto` False) ||# iv'  = iv'
(True  `Upto` True ) ||# _    = trueTrue
(False `Upto` True ) ||# (False `Upto` True ) = falseTrue
iv                   ||# iv' = iv' ||# iv


-- Pair formation and extraction

pairI = fuzzy2Inc pair

fstI ((lo1,lo2) `Upto` (hi1,hi2)) = lo1 `Upto` hi1
sndI ((lo1,lo2) `Upto` (hi1,hi2)) = lo2 `Upto` hi2

pairISplit :: Fuzzy (a,b) -> (Fuzzy a, Fuzzy b)

pairISplit b = (fstI b, sndI b)



-- Test cases

iv1, iv2, iv3, iv4, iv5 :: Fuzzy Int
iv1 = 2  `between`  4
iv2 = 20 `between` 10
iv3 = 3  `Upto`  6
iv4 = iv1 + iv2
iv5 = iv2 * iv3

