-- Reactive behaviors, represented using memoized functions from time
-- streams to value streams.
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
import IOExts
import Compatibility
import Monad
import ShowFunctions

infixr 8  ^*, ^^*
infixr 5  ++*
infix  4  ==*, <*, <=* , >=*, >*
infixr 3  &&*
infixr 2  ||*

infixr 1 `untilBB`
infixr 0  $*

-- Type of samplers and "afterers", mapping time lists to value lists or
-- residual behaviors.
type SamplerB a = [Time] -> [a]
type AftererB a = [Time] -> [BStruct a]

-- Sampler cache
type BCache   a = [([Time],[a])]
type CacheVar a = IORef (BCache a)

-- Whether to do caching.  Tweakable for checking space leaks
doCaching :: Bool
doCaching = True

-- This guy has to be a function, to prevent floating by the optimizer in
-- conjunction with unsafePerformIO below.
newCache :: b -> IO (Maybe (CacheVar a))
newCache = const $
 if doCaching then
     --putStrLn "new cache" >>
     fmap Just (newIORef [])
 else return Nothing

-- A behavior contains structure and possibly a sampler cache.  The
-- structure is used for $*/constantB and $*/untilB optimizations.
-- Together, these optimizations do a lot of constant folding.

data Behavior a = Behavior (BStruct a) (Maybe (IORef (BCache a)))

data BStruct a
   = ConstantB a                         -- K
   | SamplerB (SamplerB a) (AftererB a)  -- for S, I, etc
   | TimeTransB (Behavior a) (Behavior Time)  -- C
   | UntilB (Behavior a) (Event (Behavior a))
  deriving Show

-- Make an identical behavior, but without memoization.  This guy is
-- necessary for avoiding humongous space leaks, especially with CAFs like
-- wiggle and waggle.
-- 
-- To do: get the Hugs and GHC garbage collectors fixed to understand some
-- kind of weak pointer notion, and so automatically clear out our caches
-- when appropriate during GC's.

dontMemoizeB :: Behavior a -> Behavior a
dontMemoizeB (Behavior bstruct _) = Behavior bstruct Nothing

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

   loop _ _ _ = error "pattern match failure: atsS in Behavior.hs"


------------------------------------------------------------------
-- untilBB, afterTimesB, timeTransformB
------------------------------------------------------------------

untilBB :: Behavior a -> Event (Behavior a) -> Behavior a
b `untilBB` e = Behavior (b `UntilB` e)
                         (unsafePerformIO (newCache b))

-- the following type signature is very very important: try
-- deleting it and see what happens :-)! GSL
afterTimesB :: Behavior a -> [Time] -> [Behavior a]
Behavior bstruct mbCacheVar `afterTimesB` ts =
  zipWith Behavior
          (bstruct `afterTimesSt` ts)
          (case mbCacheVar of
            Nothing       -> repeat Nothing
            Just cacheVar -> map Just (cacheVar `afterTimesCV` ts))


timeTransformB :: Behavior a -> TimeB -> Behavior a

timeTransformB b@(Behavior (ConstantB x) _) _ = b

-- General case.  Problem: tt is often the identity (whenever no explicit
-- time transform has been applied), but we're not recognizing that fact.
timeTransformB b tt = --trace "timeTransformB\n" $
                      samplerB (ats b . ats tt)


afterTimesSt :: BStruct a -> [Time] -> [BStruct a]

bstruct@(ConstantB _) `afterTimesSt` ts = map (const bstruct) ts

SamplerB sampler afterer `afterTimesSt` ts = afterer ts

TimeTransB b tt `afterTimesSt` ts =
  zipWith TimeTransB
          (b `afterTimesB` (tt `ats` ts))
          (tt `afterTimesB` ts)

-- UntilB behaviors change at event occurrences
(b `UntilB` e) `afterTimesSt` ts =
  loop ts (b `afterTimesB` ts) (e `occs` ts)
          (e `afterTimesE` ts)
 where
   -- First occurrence.  Continue with new behavior
   loop ts _ (Just (_, Behavior bstruct' _) : _) _ =
     bstruct' `afterTimesSt` ts

   -- Non-occurrence.  Still an UntilB, and generate more
   loop (te:ts') (bAfter : bAfters')
        (Nothing : mbOccs') (eAfter : eAfters')  =
     bAfter `UntilB` eAfter : loop ts' bAfters' mbOccs' eAfters'

   loop _ _ _ _ = error "pattern matching failure: UntilB in Behavior.hs"

afterTimesCV :: CacheVar a -> [Time] -> [CacheVar a]
afterTimesCV cv [] = []
afterTimesCV cv (t:ts') = cv' : afterTimesCV cv' ts'
 where
   cv' = updateCacheVar cv t

updateCacheVar :: CacheVar a -> Time -> CacheVar a
updateCacheVar cacheVar te = unsafePerformIO $ do
  --putStrLn ("updateCacheVar: " ++ show te)
  cache <- readIORef cacheVar
  let pairs = (map (uncurry trim) cache)
  newIORef pairs
 where
   -- Trim away all time/value pairs that occur at or before te
   trim []         _          = ([],[])
   trim ts@(t:ts') xs@(x:xs') =
     if t <= te then
       --trace ("trim " ++ show t ++ " ")$
       trim ts' xs'
     else (ts,xs)
   trim _ _ = error "pattern match failure: updateCacheVar in Behavior.hs"


-- Look up or compute a value stream
cacheLookup :: SamplerB a -> CacheVar a -> [Time] -> [a]
cacheLookup sampler cacheVar ts = unsafePerformIO $ do
  --putStr "cacheLookup "
  allPairs <- readIORef cacheVar
  -- Could probably use the "find" function in the List module, but for
  -- now I want to keep stats.
  let find [] n = do
        when (n >= minCacheReport) $ report "miss" n
        let xs = sampler ts
        writeIORef cacheVar ((ts,xs) : allPairs)
        return xs

      find ((tsFound,xsFound) : pairs') n =
        if ts `matches` tsFound then
          --report "hit " n >>
          return xsFound
         else
          --putStrLn ("cache skip " ++ show (head ts, head tsFound)) >>
          n `seq` find pairs' (n+1)

      report str n = 
        case ts of
        hd:tl ->
         putStrLn (
               "cache " ++ str ++ " after " ++ show n ++ " entries. "
--            ++ "ts " ++ show (unsafePtrToInt ts) ++ ". "
--            ++ "head " ++ show hd ++ ". "
--             ++ "tail " ++ show (unsafePtrToInt tl) ++ ". "
--            ++ "cache " ++ show (unsafePtrToInt cacheVar)
            )

      ts `matches` ts' = ts `seq` ts' `seq` ts `cacheMatch` ts'

  find allPairs 0

minCacheReport = 200

-- Utility.  Make the initially empty cache.
samplerB :: SamplerB a -> Behavior a
samplerB sampler =
  --trace "samplerB\n" $
  Behavior bstruct (unsafePerformIO (newCache bstruct))
 where
   bstruct = SamplerB sampler afterer
   afterer = map (const bstruct)


-- Non-memoizing simple sampler behavior.
simpleSamplerB :: (Time -> a) -> Behavior a
simpleSamplerB f = --trace "simpleSamplerB\n" $
                   dontMemoizeB (samplerB (map f))

-- We define the usual assortment of behavior primitives and building
-- blocks.

-- Time is the identity ("I" combinator).

time :: Behavior Time
-- This simple definition has a problem: it uses "map id", which copies
-- its time list argument.  That copy means that time transforming by "time"
-- will cause cache misses.
--time = simpleSamplerB id
time = dontMemoizeB (samplerB id)

-- Was:
-- 
-- time = Behavior bstruct Nothing
--  where bstruct =  SamplerB id (map (const bstruct))

-- The basics for non-reactivity.  The classic K and S.

constantB :: a -> Behavior a
constantB x = Behavior (ConstantB x) Nothing


-- lift2 ($), i.e., S
($*) :: Behavior (a -> b) -> Behavior a -> Behavior b

-- Fully constant case
Behavior (ConstantB f) _ $* Behavior (ConstantB x) _ =
 --trace "constantB f $* constantB x\n" $
 constantB (f x)

-- untilB cases.  Especially good for optimizing sometimes-constant
-- behaviors.  (Later generalize to piecewise-polynomial, etc.)

-- Problem: these optimizations add a bit of strictness that is almost
-- always benign, but breaks demos\Collide.hs

-- (constantB f `untilB` e) $* constantB x
-- Behavior (Behavior (ConstantB f) _ `UntilB` e) _ $* xb@(Behavior (ConstantB x) _) = 
--   constantB (f x) `untilBB` e ==> \ fb' -> fb' $* xb

-- constantB f $* (constantB x `untilB` e)
-- fb@(Behavior (ConstantB f) _) $* Behavior (Behavior (ConstantB x) _  `UntilB` e) _ = 
--   constantB (f x) `untilBB` e ==> \ xb' -> fb  $* xb'


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
  --trace "constantB f $* b\n" $
  samplerB (\ts -> {-_scc_ "$* 1"-} ( map f (b `ats` ts) ))

fb $* Behavior (ConstantB x) _ =
  --trace "fb $* constantB x\n" $
  samplerB (\ts -> {-_scc_ "$* 2"-} ( map ($ x) (fb `ats` ts)) )


-- General case
fb $* xb = --trace "fb $* xb\n" $
           samplerB (\ts -> {-_scc_ "$* 3"-}
                     ( zipWith ($) (fb `ats` ts) (xb `ats` ts) )) 


-- Lifting.  All derived from constantB and ($*) !!

lift0 :: a -> Behavior a
lift1 :: (a -> b) ->
         Behavior a -> Behavior b
lift2 :: (a -> b -> c) ->
         Behavior a -> Behavior b -> Behavior c

-- etc


-- Convenience function for sampling a behavior at a start time (usually
-- corresponding to an event) and a list of later times.  Subtle point:
-- don't use "b `ats` (t0 : ts)", because when "ats" does memoization,
-- we're much more likely to get a hit with ts than with t0:ts.

ats0 :: Behavior a -> Time -> [Time] -> (a, [a])
ats0 b t0 ts = (x0,xs)
 where
   (x0 : _) = b `ats` [t0]
   xs = b `ats` ts


---- End of primitives.

{- BEGIN_NOT_FOR_GHC -}

lift0                        = constantB
lift1 f b1                   = lift0 f $* b1
lift2 f b1 b2                = lift1 f b1 $* b2
lift3 f b1 b2 b3             = lift2 f b1 b2 $* b3
lift4 f b1 b2 b3 b4          = lift3 f b1 b2 b3 $* b4
lift5 f b1 b2 b3 b4 b5       = lift4 f b1 b2 b3 b4 $* b5
lift6 f b1 b2 b3 b4 b5 b6    = lift5 f b1 b2 b3 b4 b5 $* b6
lift7 f b1 b2 b3 b4 b5 b6 b7 = lift6 f b1 b2 b3 b4 b5 b6 $* b7

{- ELSE_FOR_GHC

-- Cost-center versions

lift0 x                      = _scc_ "lift0" ( constantB x )
lift1 f b1                   = _scc_ "lift1" ( lift0 f $* b1 )
lift2 f b1 b2                = _scc_ "lift2" ( lift1 f b1 $* b2 )
lift3 f b1 b2 b3             = _scc_ "lift3" ( lift2 f b1 b2 $* b3 )
lift4 f b1 b2 b3 b4          = _scc_ "lift4" ( lift3 f b1 b2 b3 $* b4)
lift5 f b1 b2 b3 b4 b5       = _scc_ "lift5" ( lift4 f b1 b2 b3 b4 $* b5 )
lift6 f b1 b2 b3 b4 b5 b6    = _scc_ "lift6" ( lift5 f b1 b2 b3 b4 b5 $* b6 )
lift7 f b1 b2 b3 b4 b5 b6 b7 = _scc_ "lift7" ( lift6 f b1 b2 b3 b4 b5 b6 $* b7 )

   END_GHC_ONLY -}



-- Utilities


timeSince :: Time -> Behavior DTime
timeSince t0 = time - constantB t0


-- These guys should be in UtilsB.hs, defined as sin(pi*time) and
-- cos(pi*time), but doing so creates a nasty space leak.  See comments
-- there and at simpleSamplerB above.

wiggle, waggle :: RealB
wiggle = simpleSamplerB $ \ t -> sin (pi * t)
waggle = simpleSamplerB $ \ t -> cos (pi * t)



-- A few convenient type abbreviations:

type BoolB    = Behavior Bool
type TimeB    = Behavior Time
type RealB    = Behavior RealVal
type IntB     = Behavior Int
type StringB  = Behavior String
type MaybeB a = Behavior (Maybe a)
type IOB a    = Behavior (IO a)


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

(==*) = {-_scc_ "(==)"-} ( lift2 (==) )
(/=*) = {-_scc_ "(/=)"-} ( lift2 (/=) )


instance  Ord a => Ord (Behavior a)  where
  (<)   =  noOverloadBOp "<"
  (<=)  =  noOverloadBOp "<="
  (>)   =  noOverloadBOp ">"
  (>=)  =  noOverloadBOp ">="
  min   =  {-_scc_ "min"-} ( lift2 min )
  max   =  {-_scc_ "max"-} ( lift2 max )

(<*),(<=*),(>=*),(>*) :: Ord a => Behavior a -> Behavior a -> BoolB

(<*)  = {-_scc_ "(<)"-} ( lift2 (<) )
(<=*) = {-_scc_ "(<=)"-} ( lift2 (<=) )
(>=*) = {-_scc_ "(>=)"-} ( lift2 (>=) )
(>*)  = {-_scc_ "(>)"-} ( lift2 (>) )


instance  Num a => Num (Behavior a)  where
  (+)          =  {-_scc_ "(+)"-} ( lift2 (+) )
  (-)          =  {-_scc_ "(-)"-} ( lift2 (-) )
  (*)          =  {-_scc_ "(*)"-} ( lift2 (*) )
  negate       =  {-_scc_ "negate"-} ( lift1 negate )
  abs          =  {-_scc_ "abs"-} ( lift1 abs )
  fromInteger  =  constantB . fromInteger
  fromInt      =  constantB . fromInt
  signum       =  noOverloadBId "signum"

fromIntegerB :: Num a => Behavior Integer -> Behavior a
fromIntegerB = {-_scc_ "fromInteger"-} ( lift1 fromInteger )

fromIntB :: Num a => Behavior Int -> Behavior a
fromIntB = {-_scc_ "fromInt"-} ( lift1 fromInt )


instance  Real a => Real (Behavior a)  where
  toRational = noOverloadBId "toRational"

toRationalB ::  Real a => Behavior a -> Behavior Rational
toRationalB = {-_scc_ "toRational"-} ( lift1 toRational )

enumSorry =
 error "Sorry behavior enums currently only work for constant behaviors." 

instance Enum a => Enum (Behavior a) where
    toEnum	  = noOverloadBId "toEnum"
    fromEnum	  = noOverloadBId "fromEnum"
    enumFrom (Behavior (ConstantB from) _) = map constantB (enumFrom from)
    enumFrom _                             = enumSorry
    enumFromThen (Behavior (ConstantB from) _)
                 (Behavior (ConstantB to  ) _) =
       map constantB (enumFromThen from to)
    enumFromThen _ _ = enumSorry

instance (Integral a) => Integral (Behavior a) where
    quot      = {-_scc_ "quot"-} ( lift2 quot )
    rem       = {-_scc_ "rem"-} ( lift2 rem )
    div       = {-_scc_ "div"-} ( lift2 div )
    mod       = {-_scc_ "mod"-} ( lift2 mod )
    quotRem x y  = pairBSplit ({-_scc_ "quotRem"-} ( lift2 quotRem x y) )
    divMod  x y  = pairBSplit ({-_scc_ "divMod"-} ( lift2 divMod  x y) )
    toInteger = noOverloadBId "toInteger"
--    even      = noOverloadBId "even"
--    odd       = noOverloadBId "odd"
    toInt     = noOverloadBId "toInt"

toIntegerB  :: Integral a => Behavior a -> Behavior Integer
evenB, oddB :: Integral a => Behavior a -> BoolB
toIntB      :: Integral a => Behavior a -> IntB

toIntegerB = {-_scc_ "toInteger"-} ( lift1 toInteger )
evenB      = {-_scc_ "even"-} ( lift1 even )
oddB       = {-_scc_ "odd"-} ( lift1 odd )
toIntB     = {-_scc_ "toInt"-} ( lift1 toInt )

instance Fractional a => Fractional (Behavior a) where
  -- fromDouble is not standard Haskell, so GHC objects.  On some tests,
  -- it improved speed by about 10%.
  --fromDouble   =  constantB . fromDouble
  fromRational =  constantB . fromRational
  (/)          =  {-_scc_ "(/)"-} ( lift2 (/) )

instance Floating a => Floating (Behavior a) where
  sin  =  {-_scc_ "sin"-} ( lift1 sin )
  cos  =  {-_scc_ "cos"-} ( lift1 cos )
  tan  =  {-_scc_ "tan"-} ( lift1 tan )
  asin =  {-_scc_ "asin"-} ( lift1 asin )
  acos =  {-_scc_ "acos"-} ( lift1 acos )
  atan =  {-_scc_ "atan"-} ( lift1 atan )
  sinh =  {-_scc_ "sinh"-} ( lift1 sinh )
  cosh =  {-_scc_ "cosh"-} ( lift1 cosh )
  tanh =  {-_scc_ "tanh"-} ( lift1 tanh )
  asinh =  {-_scc_ "asinh"-} ( lift1 asinh )
  acosh =  {-_scc_ "acosh"-} ( lift1 acosh )
  atanh =  {-_scc_ "atanh"-} ( lift1 atanh )

  pi   =  constantB pi
  exp  =  {-_scc_ "exp"-} ( lift1 exp )
  log  =  {-_scc_ "log"-} ( lift1 log )
  sqrt =  {-_scc_ "sqrt"-} ( lift1 sqrt )
  (**) =  {-_scc_ "(**)"-} ( lift2 (**) )
  logBase = {-_scc_ "logBase"-} ( lift2 logBase )


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

properFractionB = {-_scc_ "properFraction"-} ( lift1 properFraction )
truncateB = {-_scc_ "truncate"-} ( lift1 truncate )
roundB    = {-_scc_ "round"-} ( lift1 round )
ceilingB  = {-_scc_ "ceiling"-} ( lift1 ceiling )
floorB    = {-_scc_ "floor"-} ( lift1 floor )


{-
instance  RealFloat a => RealFloat (Behavior a)  where
  ... types too restrictive to be liftable ...
-}

{- We could also add these.  Need type declarations.

floatRadixB      = {-_scc_ "floatRadix"-} ( lift1 floatRadix )
floatDigitsB     = {-_scc_ "floatDigits"-} ( lift1 floatDigits )
floatRangeB      = pairBSplit . {-_scc_ "floatRange"-} ( lift1 floatRange )
decodeFloatB     = pairBSplit . {-_scc_ "decodeFloat"-} ( lift1 decodeFloat )
encodeFloatB     = {-_scc_ "encodeFloat"-} ( lift2 encodeFloat )
exponentB        = {-_scc_ "exponent"-} ( lift1 exponent )
significandB     = {-_scc_ "significand"-} ( lift1 significand )
scaleFloatB      = {-_scc_ "scaleFloat"-} ( lift2 scaleFloat )
isNaNB           = {-_scc_ "isNaN"-} ( lift1 isNaN )
isInfiniteB      = {-_scc_ "isInfinite"-} ( lift1 isInfinite )
isDenormalizedB  = {-_scc_ "isDenormalized"-} ( lift1 isDenormalized )
isNegativeZeroB  = {-_scc_ "isNegativeZero"-} ( lift1 isNegativeZero )
isIEEEB          = {-_scc_ "isIEEE"-} ( lift1 isIEEE )

-}
  
-- Non-overloadable numeric functions

-- Various flavors of exponentiation
(^*)    :: (Num a, Integral b) =>
             Behavior a -> Behavior b -> Behavior a
(^^*)   :: (Fractional a, Integral b) =>
             Behavior a -> Behavior b -> Behavior a

(^*)  = {-_scc_ "(^)"-} ( lift2 (^) )
(^^*) = {-_scc_ "(^^)"-} ( lift2 (^^) )


-- Boolean

trueB, falseB :: BoolB
trueB  = constantB True
falseB = constantB False

notB :: BoolB -> BoolB
notB = {-_scc_ "(not)"-} ( lift1 (not) )

(&&*) :: BoolB -> BoolB -> BoolB
(&&*) = {-_scc_ "(&&)"-} ( lift2 (&&)  )
(||*) :: BoolB -> BoolB -> BoolB
(||*) = {-_scc_ "(||)"-} ( lift2 (||) )

-- condB overloading (see GBehavior)
-- The non-strictness of "if" may be a problem here, since there may be a
-- lot of catching up to do when a boolean behavior changes value.
condBB :: BoolB -> Behavior a -> Behavior a -> Behavior a
condBB = {-_scc_ "condBB"-} ( lift3 cond )

-- Strangely, condBB, and the triple constructor and selectors sometimes
-- yield a weird error from ghc claiming that names like
-- "CC_BehaviorZdcondBB_struct" and "CC_BehaviorZdcondBB" are being
-- redefined.  Seems a fluke.  Try again later.


-- Pair formation and extraction

type PairB a b = Behavior (a,b)

pairB :: Behavior a -> Behavior b -> PairB a b 
fstB  :: PairB a b -> Behavior a
sndB  :: PairB a b -> Behavior b

pairB = {-_scc_ "pair"-} ( lift2 pair )
fstB  = {-_scc_ "fst"-} ( lift1 fst )
sndB  = {-_scc_ "snd"-} ( lift1 snd )

pairBSplit :: PairB a b -> (Behavior a, Behavior b)

pairBSplit b = (fstB b, sndB b)

-- triple formation/extraction

tripleB  :: Behavior a -> Behavior b -> Behavior c -> Behavior (a, b, c)
triple1B :: Behavior (a, b, c) -> Behavior a
triple2B :: Behavior (a, b, c) -> Behavior b
triple3B :: Behavior (a, b, c) -> Behavior c

tripleB  = {-_scc_ "tripleB"-}  ( lift3 (\ a b c -> (a, b, c)) )
triple1B = {-_scc_ "triple1B"-} ( lift1 (\ (a, _, _) -> a) )
triple2B = {-_scc_ "triple2B"-} ( lift1 (\ (_, b, _) -> b) )
triple3B = {-_scc_ "triple3B"-} ( lift1 (\ (_, _, c) -> c) )

tripleBSplit :: Behavior (a, b, c) -> (Behavior a, Behavior b, Behavior c)
tripleBSplit b = (triple1B b, triple2B b, triple3B b)

-- List formation and extraction

nilB  :: Behavior [a]
consB :: Behavior a -> Behavior [a] -> Behavior [a]
headB :: Behavior [a] -> Behavior a
tailB :: Behavior [a] -> Behavior [a]
nullB :: Behavior [a] -> BoolB
(!!*) :: Behavior [a] -> IntB -> Behavior a

nilB  = constantB []
consB = {-_scc_ "(:)"-} ( lift2 (:) )
headB = {-_scc_ "head"-} ( lift1 head )
tailB = {-_scc_ "tail"-} ( lift1 tail )
nullB = {-_scc_ "null"-} ( lift1 null )
(!!*) = {-_scc_ "(!!)"-} ( lift2 (!!) )

-- Turn a list of behaviors into a behavior over list
bListToListB :: [Behavior a] -> Behavior [a]
bListToListB = foldr consB nilB

-- Lift a function over lists into a function over behavior lists
liftL :: ([a] -> b) -> ([Behavior a] -> Behavior b)
liftL f bs = {-_scc_ "f"-} ( lift1 f (bListToListB bs) )


-- Monads

-- To do: see if it makes sense to lift monads to monads and monadPlus's
-- to monadPlus's.

(++*) :: MonadPlus m => Behavior (m a) -> Behavior (m a) -> Behavior (m a)
(++*) = {-_scc_ "mplus"-} ( lift2 mplus )

-- Other

showB :: (Show a) => Behavior a -> Behavior String

showB = {-_scc_ "show"-} ( lift1 show )

-- How to implement behavior tracing?

-- traceB :: Show a => String -> Behavior a -> Behavior a


------ Testing

-- Make our test behaviors be functions of start time, just to avoid space
-- leaky CAFs.

tstB f = take 10 $ f 0 `ats` [0.0, 0.1 ..]
tstBAfter f = take 10 $ f 0 `afterTimesB` [0.0, 0.1 ..]


-- Just get the nth member.

tstBNth f n = (f 0 `ats` [0.1, 0.2 ..]) !! n

-- To confirm that we do no redundant behavior sampling
sinTB = {-_scc_ "sinT"-} ( lift1 sinT )
 where sinT x = trace ("(sinT " ++ show x ++ ") ") $ sin x


b0 t0 = constantB 0
b1 t0 = timeSince t0
b2 t0 = 5 + 3 * b1 t0
b3 t0 = sin (b1 t0)
b4 t0 = b + b where b = b3 t0

-- Reactivity.  (This one looks like it switches at 1.0 instead of just
-- after.  The cause is accumulation errors in [0.1, 0.2 .. 3], as
-- revealed by "map (\t -> (t, compare t 1)) [0.1, 0.2 .. 3]".)

b5 t0 = timeSince t0 `untilBB` timeIs (t0+1) -=> 0

b6 t0 = 10 * b5 t0

b7 t0 = b5 t0 * 10

b8 t0 = b * b  where  b = b5 t0

-- Initial transition, as in integral.  Wasn't working previously.
b9 t0 = constantB "before" `untilBB` timeIs t0 -=> constantB "after"
