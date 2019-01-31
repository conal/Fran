----------------------------------------------------------------
-- Byron Cook -- byron@cse.ogi.edu
-- several flavors of memoization
----------------------------------------------------------------

module Memo(
        memo,  
        memoN,  
        memoFix,
        memoFixN,
        cache, 
        cacheN, 
        cacheFix,
        cacheFixN
        ) where

import ST
import STArray 
import Trace

memo      :: Eval a => (a -> b) -> (a -> b)
memoN     :: Eval a => Int -> (a -> b) -> (a -> b)
memoFix   :: Eval a => ((a -> b) -> (a -> b)) -> (a -> b)
memoFixN  :: Eval a => Int -> ((a -> b) -> (a -> b)) -> (a -> b)
cache     :: Eval a => (a -> b) -> (a -> b)
cacheN    :: Eval a => Int -> (a -> b) -> (a -> b)
cacheFix  :: Eval a => ((a -> b) -> (a -> b)) -> (a -> b)
cacheFixN :: Eval a => Int -> ((a -> b) -> (a -> b)) -> (a -> b)

----------------------------------------------------------------
-- Memoization Functions (memo-tables are hash-tables)
----------------------------------------------------------------
memo          = memoN defaultSize 
memoN         = mkMemo eql hash 

memoFix       = memoFixN defaultSize 
memoFixN n f  = let g = f h
                    h = memoN n g
                in h

----------------------------------------------------------------
-- Caching Functions (memo-tables are caches)
----------------------------------------------------------------
cache          = cacheN defaultSize
cacheN         = mkCache eql hash
cacheFix       = cacheFixN defaultSize
cacheFixN n f  = let g = f h
                     h = cacheN n g
                 in h

----------------------------------------------------------------
-- Type synonyms
----------------------------------------------------------------
type TaintedEq a   = a -> a -> ST Mem Bool
type HashTable a b = MutArr Mem Int [(a,b)]
type Cache a b     = MutArr Mem Int (Maybe (a,b))
type HashSize      = Int
type HashFunc a    = a -> ST Mem Int
type Mem           = ()


----------------------------------------------------------------
-- Foundation functions
----------------------------------------------------------------
defaultSize :: HashSize
defaultSize = 97

memoize :: ST Mem t -> (t -> a -> b -> ST Mem b) -> 
           (a -> b) -> a -> b
memoize new access f = unsafeST $ do 
  t <- new
  return (\x -> unsafeST $ access t x (f x))


mkMemo  :: TaintedEq a -> HashFunc a -> Int -> (a -> c) -> (a -> c)
mkCache :: TaintedEq a -> HashFunc a -> Int -> (a -> c) -> (a -> c)

mkCache e h sz = memoize (newCache sz) (accessCache e h sz)
mkMemo  e h sz = memoize (newHash sz)  (accessHash e  h sz)


----------------------------------------------------------------
-- Hash and Cache Tables
----------------------------------------------------------------
accessHash  :: TaintedEq a ->  
               HashFunc a -> 
               Int -> 
               HashTable a b -> 
               a -> b -> ST Mem b

accessHash equal h sz table x v = do 
  hv' <- h x
  let hv = hv' `mod` sz
  l <- readArr table hv
  find l l hv
 where find l [] hv = do 
         u <- writeArr table  hv ((x,v):l)
         case u of {() -> trace "miss " $
                          return v}
       find l ((x',v'):xs) hv = do
         a <- equal x x'
         if a then trace "hit " $
                   return $ v'
          else find l xs hv

newHash :: Int -> ST Mem (HashTable a b)
newHash n = newArr (0,n) []


accessCache  :: TaintedEq a ->
                HashFunc a ->
                Int ->
                Cache a b ->
                a -> b -> ST Mem b

accessCache equal h sz table x v = do 
  hv' <- h x 
  let hv = hv' `mod` sz 
  l <-  readArr table hv
  case l of
     Nothing      -> do u <- writeArr table hv (Just (x,v))
                        case u of {() -> return v}
     Just (x',y)  -> do e <- equal x' x
                        if e then return y
                         else do u <- writeArr table hv (Just (x,v))
                                 case u of {() -> return v}

newCache :: Int -> ST Mem (Cache a b)
newCache n = newArr (0,n) Nothing

------------------------------------------------------------------
-- These functions are bad! --- don't pay attention to them
primitive primUnsafeCoerce "primUnsafeCoerce" :: a -> b  

unsafeST :: ST s a -> a
unsafeST m = fst (reifyST m ())

reifyST :: ST s a -> (b -> (a,b))
reifyST = primUnsafeCoerce

-- lisp style eql --- as described in "Lazy-memo functions"
primitive eql "STEql" :: a -> a -> ST Mem Bool

-- hash based on addresses (or values if the arg is a base type)
primitive hash "STHash" :: a -> ST Mem Int

------------------------------------------------------------------
