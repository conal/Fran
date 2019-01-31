
{- The Forceable class containing just one function force.  This provides
hyperstrict evaluation by forcing the whole data structure to normal form
as opposed to just whnf.  It is normally used in conjunction with seq,
e.g.

    force x `seq` E

to ensure that x is evaluated to normal form before E is evaluated.  Note
that force inside a definition will not force anything until the top level
is required, i.e. the following will not force x until the value of x is
needed,

  .... (force x) ...    -- Does not force x until x is needed, probably not useful.

Finally, only structured types require forcing because when the value of a
simple type is needed, and so it is evaluated to whnf, it actually
evaluates all the way to normal form.  Hence instances of force for simple
types are just the identity. -}


module Force where

class Forceable a where
    force :: a -> a


{-class (Forceable a) => ForceableIdentity a where
--    force :: a -> a
    force = id

How can we implement default methods for instances of Forceable when we
say so explicitly (e.g. by saying they are a member of ForceableIdentity)
but not just when we do not give a specific instance method (because it is
too tempting just to forget to give a proper definition for instances)?
Basically, all non-structured types can use the id function to force
(since evaluating them to whnf actualy gives normal form).

instance ForceableIdentity Int
instance ForceableIdentity Float
instance ForceableIdentity Double
-}

instance Forceable Int where
  force = id

instance Forceable Double where
  force = id

instance Forceable Float where
  force = id

instance Forceable Bool where
  force = id

instance Forceable () where
  force = id

instance (Forceable a, Forceable b) => Forceable (a, b) where
  force p@(a, b) = force a `seq` force b `seq` p

instance (Forceable a) => Forceable [a] where
  force [] = []
  force xs@(x:xs') = force x `seq` force xs' `seq` xs




