-- Non-reactive behaviors, represented as infinite "behavior trees" of
-- sample values.  This version doesn't do interval analysis.
-- 
-- Last modified Thu Aug 07 09:22:33 1997
--
-- To do:
--
--  + Re-examine the notion of start times for events.  The current
--    implementation is bogus!
--  + untilB: Try to shift work from sampling to construction.
--  + This representation space leaks in Hugs, and I don't know why.  See
--    notes in the test section.
--  + Finish timeTransform.  See notes.
--  + Make sampling do no cons'ing.  See note in "at" definition.
--  + Many of the liftings are unnecessary, because of the default methods
--    in Prelude.hs.


module Behavior where

import qualified BStack as BP   -- defines BPrim
import BaseTypes
import Event
import Trace

infixr 8  ^*, ^^*
infix  4  ==*, <*, <=* , >=*, >*
infixr 3  &&*
infixr 2  ||*


-- A behavior is represented by a single constant, an untilB, or a
-- "primitive behavior".

data Behavior a
  = ConstantB a
  | UntilB (Behavior a) (Event (Behavior a))
  | BPrim (BP.BPrim a)

instance (Show a) => Show (Behavior a) where
  -- Are these two right?  I doubt it.  Needs parens sometimes, right?
  showsPrec p (ConstantB x) = showString "ConstantB " . showsPrec 10 x
  showsPrec p (BPrim bprim) = showString "BPrim " . showsPrec 10 BPrim
  showsPrec p (UntilB b e)  = showParen (p > 1) $
                              shows b . showString " `UntilB` " . shows e

instance GBehavior (Behavior a) where
  untilB          = UntilB              -- For now
  b `afterTime` t = snd (b `at` t)

  startTime (ConstantB _)       = minTime
  startTime (BPrim bprim)       = startTime bprim
  -- When does an UntilB start?
  startTime (b `UntilB` e)      = startTime b -- `max` startTime e


-- The abstract interface: sample a behavior with a time to get a new time
-- and behavior.

at :: Behavior a -> Time -> (a, Behavior a)

b@(ConstantB x) `at` _ = (x, b)

BPrim bprim `at` t = --trace ("Sampling BStack at " ++ show t ++ "\n") $
                     (x, BPrim bprim')
 where
  (x, bprim') = bprim `BP.at` t

-- Simple implementation of UntilB.  Try to shift work from sampling to
-- construction.

(b `UntilB` e) `at` t =
  --trace ("UntilB/at " ++ show t ++ " ") $
  case mbOcc of
    Nothing       -> --trace "non-occurrence\n" $
                     (x, bNext `UntilB` eNext)
    Just (te, b') -> --trace "occurrence\n" $
                     b' `at` t
 where
  (mbOcc, eNext) = e `occ` t
  (x,     bNext) = b `at`  t


-- We define the usual assortment of behavior primitives and building
-- blocks.

-- Our bstacks need start times, so no plain old "time"
timeSince :: Time -> Behavior Time
timeSince t0 = BPrim (BP.timeSince t0)


-- Time transformation is semantically equivalent to function composition.
-- Upon sampling, the new behavior is constructed from the new behavior
-- argument and time transformation.

timeTransform :: Behavior a -> Behavior Time -> Behavior a

timeTransform b@(ConstantB _) _ = b

timeTransform b (ConstantB t) = ConstantB (fst (b `at` t))

-- How to implement the non-constant case?  In particular, what's the
-- start time?  Maybe the time transform's start time, but what if the
-- pre-transformed behavior doesn't start at the time transform's
-- transformed start time?  Other than this possible gotcha, this function
-- is not hard to implement, I think, assuming monotonically increasing
-- time transform.

timeTransform b tt = 
  error "{Behavior} timeTransform: not implemented, sorry"


-- The basics for non-reactivity.  These guys are K and S !!

constantB :: a -> Behavior a
($*) :: Behavior (a -> b) -> Behavior a -> Behavior b

-- lift0, i.e., K

constantB = ConstantB

-- lift2 ($), i.e., S

(ConstantB f) $* (ConstantB x) = ConstantB (f x)

-- Use this optimization with the explicit definition lift1 below (not in
-- terms of ($*), and then optimize here.  I think it will then kick in
-- for behaviors like 3+b.  Think about how to do b+3 efficiently.

ConstantB f $* b = lift1 f b

BPrim bprim $* ConstantB x =  BPrim (bprim `BP.applyToConstant` x)

-- Move UntilB inside of $*

(fb `UntilB` e) $* xb = 
  (fb $* xb) `untilB` (e `afterE` xb) ==> \ (fb',xb') -> fb' $* xb'

fb $* (xb `UntilB` e) = 
  (fb $* xb) `untilB` (e `afterE` fb) ==> \ (xb',fb') -> fb' $* xb'


-- Other cases

BPrim bpf $* BPrim bp = BPrim (bpf `BP.apply` bp)

-- Alternate definition of lift1.  See comments above.  Test sometime to
-- see if it helps.

lift1 f (ConstantB x1) = ConstantB (f x1)

-- Move UntilB inside of lift1
lift1 f (b `UntilB` e) = lift1 f b `untilB` e ==> lift1 f

lift1 f (BPrim bp) = BPrim (f `BP.applyConstant` bp)


-- Lifting.  All derived from constantB and ($*) !!

lift0                        = constantB      {-
lift1 f b1                   = lift0 f $* b1   -}
lift2 f b1 b2                = lift1 f b1 $* b2
lift3 f b1 b2 b3             = lift2 f b1 b2 $* b3
lift4 f b1 b2 b3 b4          = lift3 f b1 b2 b3 $* b4
lift5 f b1 b2 b3 b4 b5       = lift4 f b1 b2 b3 b4 $* b5
lift6 f b1 b2 b3 b4 b5 b6    = lift5 f b1 b2 b3 b4 b5 $* b6
lift7 f b1 b2 b3 b4 b5 b6 b7 = lift6 f b1 b2 b3 b4 b5 b6 $* b7


-- Utilities

stepper :: a -> Event a -> Behavior a
stepper x0 e = switcher (constantB x0) (e ==> constantB)

switcher :: GBehavior bv => bv -> Event bv -> bv
switcher b0 e = b0 `untilB` repeatE e

repeatE :: GBehavior bv => Event bv -> Event bv
repeatE e = withRestE e ==> uncurry switcher

accumB :: GBehavior bv => (bv -> b -> bv) -> bv -> Event b -> bv
accumB f soFar e =
  soFar `untilB` (withRestE e `afterE` soFar) ==> \ ((x,e'),soFar') ->
  accumB f (f soFar' x) e'

-- Here's a much simpler accumB definition, but it has a problem.  See the
-- comment in scanlE.
-- 
-- accumB f soFar e = switcher soFar (scanlE f soFar e)
--
-- Look for a better way to define accumB.


-- A few convenient type abbreviations:

type BoolB = Behavior Bool
type TimeB = Behavior Time
type RealB = Behavior RealVal
type IntB = Behavior Int
type StringB = Behavior String

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


-- Needed because Num derives from Eq and Show.  Similarly below.
instance  (Eq a) => Eq (Behavior a)  where
  (==) = noOverloadBOp "=="
  (/=) = noOverloadBOp "/="

(==*),(/=*) :: Eq a => Behavior a -> Behavior a -> BoolB

(==*) = lift2 (==)
(/=*) = lift2 (/=)


instance  Ord a => Ord (Behavior a)  where
  (<)   =  noOverloadBOp "<"
  (<=)  =  noOverloadBOp "<="
  (>)   =  noOverloadBOp ">"
  (>=)  =  noOverloadBOp ">="
  min   =  lift2 min
  max   =  lift2 max

(<*),(<=*),(>=*),(>*) :: Ord a => Behavior a -> Behavior a -> BoolB

(<*)  = lift2 (<)
(<=*) = lift2 (<=)
(>=*) = lift2 (>=)
(>*)  = lift2 (>)


instance  Num a => Num (Behavior a)  where
  (+)          =  lift2 (+)
  (*)          =  lift2 (*)
  negate       =  lift1 negate
  abs          =  lift1 abs
  fromInteger  =  constantB . fromInteger
  fromInt      =  constantB . fromInt

fromIntegerB :: Num a => Behavior Integer -> Behavior a
fromIntegerB = lift1 fromInteger

fromIntB :: Num a => Behavior Int -> Behavior a
fromIntB = lift1 fromInt


instance  Real a => Real (Behavior a)  where
  toRational = noOverloadBId "toRational"

toRationalB ::  Real a => Behavior a -> Behavior Rational
toRationalB = lift1 toRational


instance Enum a => Enum (Behavior a)
instance (Integral a) => Integral (Behavior a) where
    quot      = lift2 quot
    rem       = lift2 rem
    div       = lift2 div
    mod       = lift2 mod
    quotRem x y  = pairBSplit (lift2 quotRem x y)
    divMod  x y  = pairBSplit (lift2 divMod  x y)
    toInteger = noOverloadBId "toInteger"
    even      = noOverloadBId "even"
    odd       = noOverloadBId "odd"
    toInt     = noOverloadBId "toInt"

toIntegerB :: Integral a => Behavior a -> Behavior Integer
evenB, oddB :: Integral a => Behavior a -> BoolB
toIntB      :: Integral a => Behavior a -> IntB

toIntegerB = lift1 toInteger
evenB      = lift1 even
oddB       = lift1 odd
toIntB     = lift1 toInt

instance Fractional a => Fractional (Behavior a) where
  fromDouble   =  constantB . fromDouble
  fromRational =  constantB . fromRational
  (/)          =  lift2 (/)

instance Floating a => Floating (Behavior a) where
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

  pi   =  constantB pi
  exp  =  lift1 exp
  log  =  lift1 log
  sqrt =  lift1 sqrt
  (**) =  lift2 (**)
  logBase = lift2 logBase


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
(^*)    :: (Num a, Integral b) =>
             Behavior a -> Behavior b -> Behavior a
(^^*)   :: (Fractional a, Integral b) =>
             Behavior a -> Behavior b -> Behavior a

(^*)  = lift2 (^)
(^^*) = lift2 (^^)



cond :: BoolB -> Behavior a -> Behavior a -> Behavior a
cond = lift3 (\ a b c -> if a then b else c)

notB :: BoolB -> BoolB
notB = lift1 (not)

(&&*) :: BoolB -> BoolB -> BoolB
(&&*) = lift2 (&&) 
(||*) :: BoolB -> BoolB -> BoolB
(||*) = lift2 (||)

-- Pair formation and extraction

type PairB a b = Behavior (a,b)

pairB :: Behavior a -> Behavior b -> PairB a b 
fstB  :: PairB a b -> Behavior a
sndB  :: PairB a b -> Behavior b

pairB = lift2 pair
fstB  = lift1 fst
sndB  = lift1 snd

pairBSplit :: PairB a b -> (Behavior a, Behavior b)

pairBSplit b = (fstB b, sndB b)

-- List formation and extraction

nilB  :: Behavior [a]
consB :: Behavior a -> Behavior [a] -> Behavior [a]
headB :: Behavior [a] -> Behavior a
tailB :: Behavior [a] -> Behavior [a]

nilB  = constantB []
consB = lift2 (:)
headB = lift1 head
tailB = lift1 tail

liftL :: ([a] -> b) -> ([Behavior a] -> Behavior b)
liftL f bs = lift1 f (foldr consB nilB bs)

-- Other

showB :: (Show a) => Behavior a -> Behavior String

showB = lift1 show

-- How to implement behavior tracing?

-- traceB :: Show a => String -> Behavior a -> Behavior a


------ Testing

-- This representation space leaks, at least with the tests below, and I
-- don't know why.

ats :: Behavior a -> [Time] -> [a]

b `ats` []      = []
b `ats` (t:ts') = x : (b' `ats` ts')
 where
  (x,b') = b `at` t

-- Make our test behaviors be functions of start time, just to avoid space
-- leaky CAFs.  With a 1Mb heap, using 300 instead of 3 below and trying
-- "tstB b1" eventually crashes Hugs, by overflowing the stack in the Hugs
-- implementation.  With b2, Hugs eventually runs out
-- of heap, although the GC messages are not strictly decreasing !?!

tstB f = f 0 `ats` [0.0, 0.1 .. 3]

-- Just get the nth member.  "tstNth b0 3000" leads to "Control stack
-- overflow".  The usual trick of adding an end enumeration value (1.0e8)
-- didn't help.  (Oddly, "[0 .. ] !! 1000000" doesn't bomb the heap or
-- evaluation stack when I just tried.  Why not?)

tstNth f n = (f 0 `ats` [0.1, 0.2 ..]) !! n

-- To confirm that we do no redundant behavior sampling
sinTB = lift1 sinT
 where sinT x = trace ("(sinT " ++ show x ++ ") ") $ sin x


b0 t0 = constantB 0
b1 t0 = timeSince t0
b2 t0 = 5 + 3 * b1 t0
b3 t0 = sin (b1 t0)
b4 t0 = b + b where b = b3 t0

-- Reactivity.  (This one looks like it switches at 1.0 instead of just
-- after.  The cause is accumulation errors in [0.1, 0.2 .. 3], as
-- revealed by "map (\t -> (t, compare t 1)) [0.1, 0.2 .. 3]".)

b5 t0 = timeSince t0 `untilB` timeIs (t0+1) -=> 0

b6 t0 = 10 * b5 t0

b7 t0 = b5 t0 * 10

b8 t0 = b * b  where  b = b5 t0

-- Initial transition, as in integral.  Wasn't working previously.
b9 t0 = constantB "before" `untilB` timeIs t0 -=> constantB "after"
