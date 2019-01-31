-- Transform behaviors
-- 
-- Last modified Wed Oct 01 09:29:48 1997

module Transform2B where

import qualified Transform2 as T
import Behavior

-- Hugs bug (?) workaround.  See comment in VectorSpaceB.
-- infixr 7 *%, `compose2`         -- transform apply and compose

type Transform2B = Behavior T.Transform2

factorTransform2 = lift1 T.factorTransform2
identity2  = lift0 T.identity2
translate2 = lift1 T.translate2
rotate2    = lift1 T.rotate2
uscale2    = lift1 T.uscale2
compose2   = lift2 T.compose2
inverse2   = lift1 T.inverse2

class Transformable2B a where
  (*%)  ::  Transform2B -> a -> a

instance  T.Transformable2 a =>  Transformable2B (Behavior a) where
  (*%) =  lift2 (T.*%)
