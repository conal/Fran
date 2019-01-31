-- Hugs/GHC incompatibility work-arounds

module Compatibility (cacheMatch, Addr, double2Float)
where

import Addr (Addr)

double2Float :: Double -> Float
double2Float = fromDouble

cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = x `ptrEq` x'
                    -- getCell x `cellPtrEq` getCell x'


primitive ptrEq "reallyUnsafePtrEq" :: a -> a -> Bool

-- The following requires INTERNAL_PRIMS to be set in Hugs/src/options.h.
-- These declarations copied from Hugs/lib/hugs/HugsInternals.hs
-- breaks referential transparency - use with care

-- data Cell
-- primitive getCell                  :: a -> Cell
-- primitive cellPtrEq                :: Cell -> Cell -> Bool
