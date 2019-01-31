-- Behavior-level vector spaces
-- 
-- Last modified Sat Sep 07 23:23:09 1996

module VectorSpaceB where

import Behavior
import qualified VectorSpace as VS


-- I think the following should be here, but they cause Hugs to claim that
-- we're redefining the syntax of these operators, even though the
-- importation is done qualified.  Is this a Hugs bug??  If so, be sure to
-- restore the infix declarations below when the bug is fixed.

-- infix8 7 `scaleVector`, `dot`
-- infixl 6 `addVector`

zeroVector :: VS.VectorSpace v => Behavior v
zeroVector  = lift0 VS.zeroVector

scaleVector :: VS.VectorSpace v => Behavior VS.Scalar
                  -> Behavior v -> Behavior v
scaleVector = lift2 VS.scaleVector

addVector :: VS.VectorSpace v => Behavior v
                  -> Behavior v -> Behavior v
addVector = lift2 VS.addVector

dot :: VS.VectorSpace v => Behavior v
                  -> Behavior v -> Behavior VS.Scalar
dot = lift2 VS.dot

magnitude, magnitudeSquared
   :: VS.VectorSpace v => Behavior v -> Behavior VS.Scalar
magnitudeSquared = lift1 VS.magnitudeSquared
magnitude        = lift1 VS.magnitude

normalize :: VS.VectorSpace v => Behavior v -> Behavior v
normalize = lift1 VS.normalize
