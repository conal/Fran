-- Behavior-level vector spaces
-- 
-- Last modified Wed Jul 09 09:06:11 1997

module VectorSpaceB where

import BaseTypes
import Behavior
import qualified VectorSpace as VS


-- I think the following should be here, but they cause Hugs to claim that
-- we're redefining the syntax of these operators, even though the
-- importation is done qualified.  Is this a Hugs bug??  If so, be sure to
-- restore the infix declarations below when the bug is fixed.

-- infixr 7 *^, ^/, `dot`
-- infixl 6 ^+^, ^-^

zeroVector :: VS.VectorSpace v => Behavior v
zeroVector  = constantB VS.zeroVector

(*^) :: VS.VectorSpace v => Behavior Scalar
                  -> Behavior v -> Behavior v
(*^) = lift2 (VS.*^)

(^/) :: VS.VectorSpace v => Behavior v ->
           Behavior Scalar -> Behavior v
(^/) = lift2 (VS.^/)

(^+^),(^-^) :: VS.VectorSpace v => Behavior v
                  -> Behavior v -> Behavior v
(^+^) = lift2 (VS.^+^)
(^-^) = lift2 (VS.^-^)

dot :: VS.VectorSpace v => Behavior v
                  -> Behavior v -> Behavior Scalar
dot = lift2 VS.dot

magnitude, magnitudeSquared
   :: VS.VectorSpace v => Behavior v -> Behavior Scalar
magnitudeSquared = lift1 VS.magnitudeSquared
magnitude        = lift1 VS.magnitude

normalize :: VS.VectorSpace v => Behavior v -> Behavior v
normalize = lift1 VS.normalize
