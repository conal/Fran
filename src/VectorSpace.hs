-- Vector spaces
-- 
-- Doesn't work as well as we'd like.  Lacking type relations, we have to
-- hardwire the scalar type, which means we can't use behaviors, so we
-- can't lift a VectorSpace instance into another one.
--
-- When we do have type relations, introduce an AffineSpace class as well
-- and make Point2, Point3, Point2B, and Point3B be instances.

module VectorSpace where

import Compatibility
import BaseTypes

infixr 7 *^, ^/, `dot`
infixl 6 ^+^, ^-^

class VectorSpace v where
  zeroVector  :: v
  (*^)        :: Scalar -> v -> v
  (^+^)       :: v -> v -> v
  dot         :: v -> v -> Scalar


(^-^) :: VectorSpace v => v -> v -> v

v ^-^ v' = v ^+^ negateVector v'

(^/) :: VectorSpace v => v -> Scalar -> v

v ^/ s = (1/s) *^ v


negateVector :: VectorSpace v =>  v -> v
negateVector v = (-1) *^ v

magnitudeSquared :: VectorSpace v =>  v -> Scalar
magnitudeSquared v = v `dot` v

magnitude :: VectorSpace v =>  v -> Scalar
magnitude = sqrt . magnitudeSquared

normalize :: VectorSpace v =>  v -> v
normalize v | mag /= 0 = v ^/ mag
 where
   mag = magnitude v


-- May be useful sometime:
-- 
-- checkVal pred who x | pred x    = x
--                     | otherwise = error $
--                         "checkVal failure (" ++ who ++ ") " ++ show x

{- Does this make sense?  I get "Illegal type expression" 
  Yes it makes perfect sense to me, but Haskell type classes does
  not allow you to do this (:-(), i.e., declaring default methods for
  instances of some class. Instance decls. work over type
  constructors, hence the msg. 

  In this case, the soln. is to give VectorSpace decls. for all
  the Floating instances (not extensible)...sigh.

instance  Floating a => VectorSpace a  where
  zeroVector  =  0
  (*^)        =  (*)
  (^+^)       =  (+)
  a `dot` b   =  fromRealFrac (a * b)
-}

instance  VectorSpace Double  where
  zeroVector  =  0.0
  (*^)        =  (*)
  (^+^)       =  (+)
  dot         =  (*)

instance VectorSpace Float where
  zeroVector  =  0.0
  d  *^ f     =  (double2Float d) * f
  (^+^)       =  (+)
  a `dot` b   =  fromRealFrac (a * b)

