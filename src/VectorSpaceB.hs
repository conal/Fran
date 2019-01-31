-- Behavior-level vector spaces
-- 
-- Last modified Sun Nov 10 16:15:46 1996

module VectorSpaceB where

import Fuzzy
import Behavior
import qualified VectorSpace as VS


-- I think the following should be here, but they cause Hugs to claim that
-- we're redefining the syntax of these operators, even though the
-- importation is done qualified.  Is this a Hugs bug??  If so, be sure to
-- restore the infix declarations below when the bug is fixed.

-- infixr 7 *^, ^/, `dot`
-- infixl 6 ^+^, ^-^

zeroVector :: VS.VectorSpace v => Behavior v
zeroVector  = lift0 VS.zeroVector

(*^) :: VS.VectorSpace v => Behavior VS.Scalar
                  -> Behavior v -> Behavior v
(*^) = lift2 (VS.*^) (noI "(*^)")

(^/) :: VS.VectorSpace v => Behavior v ->
           Behavior VS.Scalar -> Behavior v
(^/) = lift2 (VS.^/) (noI "(^/)")

(^+^),(^-^) :: VS.VectorSpace v => Behavior v
                  -> Behavior v -> Behavior v
(^+^) = lift2 (VS.^+^) (noI "(^+^)")
(^-^) = lift2 (VS.^-^) (noI "(^-^)")

dot :: VS.VectorSpace v => Behavior v
                  -> Behavior v -> Behavior VS.Scalar
dot = lift2 VS.dot (noI "dot")

magnitude, magnitudeSquared
   :: VS.VectorSpace v => Behavior v -> Behavior VS.Scalar
magnitudeSquared = lift1 VS.magnitudeSquared (noI "magnitudeSquared")
magnitude        = lift1 VS.magnitude (noI "magnitude")

normalize :: VS.VectorSpace v => Behavior v -> Behavior v
normalize = lift1 VS.normalize (noI "normalize")
