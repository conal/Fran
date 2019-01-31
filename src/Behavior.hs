-- Reactive behaviors, represented using memoized functions from time
-- streams to value streams.
-- 
-- Last modified Thu Oct 02 10:59:59 1997
--
-- Notes
--
-- + This version doesn't do interval analysis.
-- + Correctness relies on untilB behaviors getting only monotonically
--   increasing time lists.  Time transformation can violate this
--   assumption.  I don't know of any practical way around this problem.
--
-- To do:
--
--  + Restore lift/untilB and lift/constantB optimization.
--  + Many of the liftings are unnecessary, because of the default methods
--    in Prelude.hs. 
--  + Consider how to exploit UntilB in timeTransform.  If b or tt is
--    reactive, we're losing that knowledge, and afterTimes works very
--    badly!! 


module Behavior where

import BaseTypes
import Event
import Maybe (isJust)
import MutVar
import IOExtensions (unsafePerformIO)
import Trace

infixr 8  ^*, ^^*
infix  4  ==*, <*, <=* , >=*, >*
infixr 3  &&*
infixr 2  ||*

infixr 0  $*

-- Type of samplers and "afterers", mapping time lists to value lists or
-- residual behaviors.
type SamplerB a = [Time] -> [a]
type AftererB a = [Time] -> [BStruct a]

-- Sampler cache
type BCache   a = [([Time],[a])]
type CacheVar a = MutVar (BCache a)

-- A behavior contains structure and possibly a sampler cache.  The
-- structure is used for $*/constantB and $*/untilB optimizations.
-- Together, these optimizations do a lot of constant folding.

data Behavior a = Behavior (BStruct a) (Maybe (MutVar (BCache a)))

data BStruct a
   = ConstantB a                         -- K
   | SamplerB (SamplerB a) (AftererB a)  -- for S, I, etc
   | TimeTransB (Behavior a) (Behavior Time)  -- C
   | UntilB (Behavior a) (Event (Behavior a))
  deriving Show

-- I'd really like to remove SamplerB and add the following two
-- constructors:
--   | TimeB                               -- I
--   | AppB (Behavior (b->a)) (Behavior b) -- S

-- The problem is that TimeB would have type (BStruct Time), and AppB
-- would need existential types (?).  It would be nice to use these
-- constructors instead, for printing, at least.  We could get rid of the
-- Afterer type as well.

-- To show, just say whether cached, and show the structure.
instance (Show a) => Show (Behavior a) where
  showsPrec p (Behavior bstruct mb) =
      showString ("<<" ++ (if isJust mb then "" else "un")
                     ++ "cached Behavior ")
    . showsPrec 0 bstruct . showString ">>"


-- The abstract interface: sample a behavior with a list of times to get a
-- list of values.  N.B.: if the time list is finite, the resulting value
-- list may be infinite anyway.  This allows some cases to be more
-- efficient, and the time lists are almost always infinite.
ats :: Behavior a -> [Time] -> [a]

ats (Behavior struct mbCacheVar) =
  -- Use the cache if it's there, and just atsS otherwise.
  case mbCacheVar of
     Just cacheVar -> cacheLookup (atsS struct) cacheVar
     Nothing       -> atsS struct 

atsS :: BStruct a -> [Time] -> [a]

-- It would be less efficient to use "map (const x)" here, but that would
-- only be correct even for finite time lists.  Usually the time lists are
-- infinite, with the only current exception coming from snapshot.  And
-- snapshot doesn't mind if the length changes.
atsS (ConstantB x) = const (repeat x)

atsS (SamplerB sampler _) = sampler

atsS (TimeTransB b tt) = ats b . ats tt

-- Untilb behaviors change at event occurrences
atsS (b `UntilB` e) = \ ts -> loop ts (b `ats` ts) (e `occs` ts)
 where
   -- First event occurrence.  Discard the rest of the b values and
   -- possible event occurrences, and continue.
   loop ts _ (Just (_, b') : _) = b' `ats` ts

   loop (_:ts') (x:xs') (Nothing:mbOccs') =
     x : loop ts' xs' mbOccs'


instance GBehavior (Behavior a) where
  b `untilB` e = Behavior (b `UntilB` e)
                          (Just (unsafePerformIO (newVar [])))

  Behavior bstruct mbCacheVar `afterTimes` ts =
    zipWith Behavior
            (bstruct `afterTimesSt` ts)
            (case mbCacheVar of
              Nothing       -> repeat Nothing
              Just cacheVar -> map Just (cacheVar `afterTimesCV` ts))

afterTimesSt :: BStruct a -> [Time] -> [BStruct a]

bstruct@(ConstantB _) `afterTimesSt` ts = map (const bstruct) ts

SamplerB sampler afterer `afterTimesSt` ts = afterer ts

TimeTransB b tt `afterTimesSt` ts =
  zipWith TimeTransB
          (b `afterTimes` (tt `ats` ts))
          (tt `afterTimes` ts)

-- UntilB behaviors change at event occurrences
(b `UntilB` e) `afterTimesSt` ts =
  loop ts (b `afterTimes` ts) (e `occs` ts)
          (e `afterTimes` ts)
 where
   -- First occurrence.  Continue with new behavior
   loop ts _ (Just (_, Behavior bstruct' _) : _) _ =
     bstruct' `afterTimesSt` ts

   -- Non-occurrence.  Still an UntilB, and generate more
   loop (te:ts') (bAfter : bAfters')
        (Nothing : mbOccs') (eAfter : eAfters')  =
     bAfter `UntilB` eAfter : loop ts' bAfters' mbOccs' eAfters'


afterTimesCV :: CacheVar a -> [Time] -> [CacheVar a]
afterTimesCV cv [] = []
afterTimesCV cv (t:ts') = cv' : afterTimesCV cv' ts'
 where
   cv' = updateCacheVar cv t

updateCacheVar :: CacheVar a -> Time -> CacheVar a
updateCacheVar cacheVar te = unsafePerformIO $ do
  --putStrLn ("updateCacheVar: " ++ show te)
  cache <- readVar cacheVar
  let pairs = (map (uncurry trim) cache)
  newVar pairs
 where
   -- Trim away all time/value pairs that occur at or before te
   trim []         _          = ([],[])
   trim ts@(t:ts') xs@(x:xs') =
     if t <= te then
       --trace ("trim " ++ show t ++ " ")$
       trim ts' xs'
     else (ts,xs)


-- Look up or compute a value stream
cacheLookup :: SamplerB a -> CacheVar a -> [Time] -> [a]
cacheLookup sampler cacheVar ts = unsafePerformIO $ do
  --putStr "cacheLookup "
  allPairs <- readVar cacheVar
  -- Could probably use the "find" function in the List module, but for
  -- now I want to keep stats.
  let find [] n = do
        --report "miss" n
        let xs = sampler ts
        writeVar cacheVar ((ts,xs) : allPairs)
        return xs

      find ((tsFound,xsFound) : pairs') n =
        if ts `cacheMatch` tsFound then
          --report "hit" n >>
          return xsFound
         else
          --putStrLn ("cache skip " ++ show (head ts, head tsFound)) >>
          find pairs' (n+1)

      report str n = putStrLn ("cache " ++ str ++ " after "
                            ++ show n ++ " entries.  head "
                            ++ show (head ts))

  find allPairs 0

cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = --x `ptrEq` x'
                    getCell x `cellPtrEq` getCell x'

-- The following requires INTERNAL_PRIMS to be set in Hugs/src/options.h.
-- These declarations copied from Hugs/lib/hugs/HugsInternals.hs
-- breaks referential transparency - use with care
primitive ptrEq :: a -> a -> Bool
data Cell
primitive getCell                  :: a -> Cell
primitive cellPtrEq                :: Cell -> Cell -> Bool


-- Utility.  Make the initially empty cache.
samplerB :: SamplerB a -> Behavior a
samplerB sampler =
  Behavior (SamplerB sampler afterer) (Just cv)
 where
   cv = unsafePerformIO (newVar [])
   afterer = map (const (SamplerB sampler afterer))


-- We define the usual assortment of behavior primitives and building
-- blocks.

-- Time is the identity ("I" combinator).

time :: Behavior Time
time = Behavior bstruct Nothing
 where bstruct =  SamplerB id (map (const bstruct))

-- I would put the timeTransform method in GBehavior, but then there would
-- be a cyclic dependency between Behavior and Event.  What's the right
-- thing?
class TimeTransformable bv where
  timeTransform :: bv -> TimeB -> bv

-- Time transformation is semantically equivalent to function composition.

instance TimeTransformable (Behavior a) where
  timeTransform b tt = samplerB (ats b . ats tt)


-- The basics for non-reactivity.  The classic K and S.

constantB :: a -> Behavior a
constantB x = Behavior (ConstantB x) Nothing


-- lift2 ($), i.e., S
($*) :: Behavior (a -> b) -> Behavior a -> Behavior b

-- Fully constant case
Behavior (ConstantB f) _ $* Behavior (ConstantB x) _ =
 constantB (f x)

-- untilB cases.  Especially good for optimizing sometimes-constant
-- behaviors.  (Later generalize to piecewise-polynomial, etc.)

-- (constantB f `untilB` e) $* constantB x
Behavior (Behavior (ConstantB f) _ `UntilB` e) _ $* xb@(Behavior (ConstantB x) _) = 
  constantB (f x) `untilB` e ==> \ fb' -> fb' $* xb

-- constantB f $* (constantB x `untilB` e)
fb@(Behavior (ConstantB f) _) $* Behavior (Behavior (ConstantB x) _  `UntilB` e) _ = 
  constantB (f x) `untilB` e ==> \ xb' -> fb  $* xb'


-- This one seems like it should be useful, but slows down Mover.hs.  :(

{-
-- (constantB f `untilB` fe) $* (constantB x `untilB` xe)
Behavior (Behavior (ConstantB f) _ `UntilB` fe) _ $*
  Behavior (Behavior (ConstantB x) _  `UntilB` xe) _ =
  --trace "untilB $* untilB optimization\n" $
  constantB (f x) `untilB`
        (fe `afterE` xe) ==> (\ (fb',xe') -> fb' $* (constantB x `untilB` xe'))
    .|. (xe `afterE` fe) ==> (\ (xb',fe') -> (constantB f `untilB` fe') $* xb')
-}

{-
-- The most general transformations.  Two problems: (a) there's a bug
-- somewhere that is being tickled here and I can't find; and (b) too much
-- unhelpful transformation.  See constant alternatives above.
Behavior (fb `UntilB` e) _ $* xb = 
  (fb $* xb) `untilB` (e `afterE` xb) ==> \ (fb',xb') -> fb' $* xb'

fb $* Behavior (xb `UntilB` e) _ = 
  (fb $* xb) `untilB` (e `afterE` fb) ==> \ (xb',fb') -> fb' $* xb'
-}

-- constant/no-structure
Behavior (ConstantB f) _ $* b =
  samplerB (\ts -> map f (b `ats` ts))

fb $* Behavior (ConstantB x) _ =
  samplerB (\ts -> map ($ x) (fb `ats` ts))


-- General case
fb $* xb = samplerB (\ts -> zipWith ($) (fb `ats` ts) (xb `ats` ts))


-- Lifting.  All derived from constantB and ($*) !!

lift0 :: a -> Behavior a
lift1 :: (a -> b) ->
         Behavior a -> Behavior b
lift2 :: (a -> b -> c) ->
         Behavior a -> Behavior b -> Behavior c

-- etc

---- End of primitives.

lift0                        = constantB
lift1 f b1                   = lift0 f $* b1
lift2 f b1 b2                = lift1 f b1 $* b2
lift3 f b1 b2 b3             = lift2 f b1 b2 $* b3
lift4 f b1 b2 b3 b4          = lift3 f b1 b2 b3 $* b4
lift5 f b1 b2 b3 b4 b5       = lift4 f b1 b2 b3 b4 $* b5
lift6 f b1 b2 b3 b4 b5 b6    = lift5 f b1 b2 b3 b4 b5 $* b6
lift7 f b1 b2 b3 b4 b5 b6 b7 = lift6 f b1 b2 b3 b4 b5 b6 $* b7



-- Utilities


timeSince :: Time -> Behavior DTime
timeSince t0 = time - constantB t0

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

-- Here's a much simpler accumB definition, but it has the problem that it
-- does't "age" soFar.  See the comment in scanlE.
-- 
-- accumB f soFar e = switcher soFar (scanlE f soFar e)
--
-- Look for a better way to define accumB.


-- A few convenient type abbreviations:

type BoolB   = Behavior Bool
type TimeB   = Behavior Time
type RealB   = Behavior RealVal
type IntB    = Behavior Int
type StringB = Behavior String
type IOB a   = Behavior (IO a)


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



-- The non-strictness of "if" may be a problem here, since there may be a
-- lot of catching up to do when a boolean behavior changes value.
condB :: BoolB -> Behavior a -> Behavior a -> Behavior a
condB = lift3 (\ a b c -> if a then b else c)

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

-- Make our test behaviors be functions of start time, just to avoid space
-- leaky CAFs.

tstB f = take 10 $ f 0 `ats` [0.0, 0.1 ..]
tstBAfter f = take 10 $ f 0 `afterTimes` [0.0, 0.1 ..]


-- Just get the nth member.

tstBNth f n = (f 0 `ats` [0.1, 0.2 ..]) !! n

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
