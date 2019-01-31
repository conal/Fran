-- Lifted 3D transformations
-- 
-- Last modified Wed Jul 09 12:10:35 1997

module Transform3B(
	Transform3B
	,(**%), translate3, scale3
	,identity3, compose3
     -- , inverse3
	,rotate3, uscale3
        ,Translateable3B, Transformable3B, Scaleable3B
	) where

import qualified Transform3 as T
import Vector3B
import Transform3(Transform3, Translateable3, Transformable3, Scaleable3)
import Behavior
import BaseTypes
	
-- Hugs bug (?) workaround.  See comment in VectorSpaceB.
-- infixr 7 **%, `compose3`         -- transform apply and compose

type Transform3B = Behavior Transform3

identity3  :: Transform3B
rotate3    :: Vector3B -> RealB -> Transform3B
compose3   :: Transform3B -> Transform3B -> Transform3B
--inverse3 :: Transform3B -> Transform3B
uscale3    :: RealB -> Transform3B


class Translateable3B a where
 translate3 :: a -> Transform3B

class Scaleable3B a where
 scale3 :: a -> Transform3B

class Transformable3B a where
  (**%)  ::  Transform3B -> a -> a


identity3  = constantB T.identity3
rotate3    = lift2 T.rotate3
compose3   = lift2 T.compose3
--inverse3   = lift1 T.inverse3
uscale3    = lift1 T.uscale3


---

instance  Translateable3 a => Translateable3B (Behavior a)  where
  translate3 =  lift1 T.translate3

instance  Scaleable3 a => Scaleable3B (Behavior a)  where
  scale3 =  lift1 T.scale3

instance  Transformable3 a => Transformable3B (Behavior a)  where
  (**%) =  lift2 (T.**%)
