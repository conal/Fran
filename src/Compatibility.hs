-- Hugs/GHC incompatibility work-arounds

module Compatibility (cacheMatch, Addr, double2Float)
where

import Foreign (Addr)

double2Float :: Double -> Float
double2Float = fromDouble

cacheMatch :: Eval a => a -> a -> Bool
x `cacheMatch` x' = x `ptrEq` x'
                    -- getCell x `cellPtrEq` getCell x'

-- The following requires INTERNAL_PRIMS to be set in Hugs/src/options.h.
-- These declarations copied from Hugs/lib/hugs/HugsInternals.hs
-- breaks referential transparency - use with care

primitive ptrEq :: a -> a -> Bool
data Cell
primitive getCell                  :: a -> Cell
primitive cellPtrEq                :: Cell -> Cell -> Bool
