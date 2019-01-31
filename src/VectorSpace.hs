-- Vector spaces
-- 
-- Last modified Sat Sep 07 23:23:10 1996
-- 
-- Doesn't work as well as we'd like.  Lacking type relations, we have to
-- hardwire the scalar type, which means we can't use behaviors, so we
-- can't lift a VectorSpace instance into another one.
--
-- When we do have type relations, introduce an AffineSpace class as well
-- and make Point2 and Behavior Point2 be instances.

module VectorSpace where

infixr 7 `scaleVector`, `dot`
infixl 6 `addVector`

-- Vector space type class.  Should really be a binary type relation,
-- rather than having Double hardwired.

type Scalar = Double

class VectorSpace v where
  zeroVector         :: v
  scaleVector        :: Scalar -> v -> v
  addVector          :: v -> v -> v
  dot                :: v -> v -> Scalar


magnitudeSquared :: VectorSpace v =>  v -> Scalar
magnitudeSquared v = v `dot` v

magnitude :: VectorSpace v =>  v -> Scalar
magnitude = sqrt . magnitudeSquared

normalize :: VectorSpace v =>  v -> v
normalize v = (1 / magnitude v) `scaleVector` v



{- Does this make sense?  I get "Illegal type expression" 
  Yes it makes perfect sense to me, but Haskell type classes does
  not allow you to do this (:-(), i.e., declaring default methods for
  instances of some class. Instance decls. work over type
  constructors, hence the msg. 

  In this case, the soln. is to give VectorSpace decls. for all
  the Floating instances (not extensible)...sigh.

instance  Floating a => VectorSpace a  where
  zeroVector    =  0
  scaleVector   =  (*)
  addVector     =  (+)
  a `dot` b     =  fromRealFrac (a * b)
-}

instance  VectorSpace Double  where
  zeroVector     =  0.0
  scaleVector    =  (*)
  addVector      =  (+)
  dot            =  (*)

instance VectorSpace Float where
  zeroVector          =  0.0
  d  `scaleVector` f  =  (fromDouble d) * f
  addVector           =  (+)
  a `dot` b           =  fromRealFrac (a * b)


