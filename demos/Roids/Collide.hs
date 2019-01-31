-- Collisions for Roids
module Collide where

import Fran
import qualified StaticTypes as S
import StaticTypes (Vector2)
import IOExts ( trace )

import Media

infixr 1 `untilBE'`

infixl 6 `unionGen`


-- Space stuff.  Includes moving collidables, aggregation and reactivity
data SpaceStuff
  = ColliderSS Vector2B Vector2B Collider  -- motion, velocity
  | NothingSS ImageB                       -- e.g., dying explosion
  | UnionSS SpaceStuff SpaceStuff
  | UntilSS SpaceStuff (Event SpaceStuff)

emptySS :: SpaceStuff
emptySS = NothingSS emptyImage

unionSS, unionSSopt :: SpaceStuff -> SpaceStuff -> SpaceStuff

-- This optimization might introduce too much strictness, so make it easy
-- to remove.
stuff                `unionSSopt` NothingSS EmptyImage = stuff
NothingSS EmptyImage `unionSSopt` stuff'               = stuff'
stuff                `unionSSopt` stuff'               = stuff `UnionSS` stuff'

unionSS = unionSSopt
--unionSS = UnionSS

instance GBehavior SpaceStuff where
  untilB        = UntilSS

  timeTransform = error "no timeTransform yet for SpaceStuff"

  ColliderSS mot vel coll `afterTimes` ts =
    zipWith3 ColliderSS (mot `afterTimes` ts) (vel `afterTimes` ts)
                        (coll `afterTimes` ts)
  NothingSS imB `afterTimes` ts = map NothingSS (imB `afterTimes` ts)
  (ss `UnionSS` ss') `afterTimes` ts =
    zipWith unionSS (ss `afterTimes` ts) (ss' `afterTimes` ts)
  (ss `UntilSS` e) `afterTimes` ts = untilBAfterTimes ss e ts


instance Renderable SpaceStuff where
  render (ColliderSS mot _ coll) = move mot (render coll)
  render (NothingSS imB)         = imB
  render (ss `UnionSS` ss')      = render ss `over` render ss'
  render (ss `UntilSS` e)        = render ss `untilB` e ==> render

-- Individual Collider element
data Collider
  = Ship ImageB
  | Roid Int                            -- size/type (0 is biggest)
  | Missile RealB                       -- radius

instance Renderable Collider where
  render (Ship imB)       = imB
  render (Roid n)         = roidImage n
  render (Missile radius) = missileImage radius

instance GBehavior Collider where
  untilB        = error "no untilB for Collider"
  timeTransform = error "no timeTransform yet for Collider"

  Ship imB `afterTimes` ts = map Ship (imB `afterTimes` ts)
  Missile radius `afterTimes` ts = map Missile (radius `afterTimes` ts)
  c@(Roid _) `afterTimes` _ = repeat c

-- The user is in here for the sake of atRate in colliderGen.  Try to
-- eliminate this need somehow, since it's awkward.
type Env = (User, SpaceStuff)

-- Generator of space stuff, given a user and environment.
type StuffGen = Env -> SpaceStuff

nothingGen :: StuffGen
nothingGen env = emptySS

-- Combine stuff generators
unionGen :: StuffGen -> StuffGen -> StuffGen
unionGen gen gen' (user, envStuff) = new `unionSS` new'
 where
   -- Put each generated stuff in each other's environment
   new  = gen  (user, new' `unionSS` envStuff)
   new' = gen' (user, new  `unionSS` envStuff)

-- Combine generators from an event
eventGen :: Event StuffGen -> StuffGen
eventGen e env@(user, envStuff) = --trace "eventGen\n" $
                                  newStuff
 where
   -- Generate the new stuff in an environment that includes it
   newEnv   = (user, envStuff `unionSS` newStuff)
   newStuff = accumB unionSS emptySS (e `afterE` newEnv ==> uncurry ($))


type Shooter = Event RealVal              -- shoot angle

-- Specifies collider initial position and velocity and the acceleration
-- of a collider.
type ColliderStart = (Vector2, Vector2, Vector2B, Collider, Shooter)

-- Generate stuff from a list of collider starts
collidersGen :: [ColliderStart] -> StuffGen
collidersGen = foldr unionGen nothingGen . map colliderGen

-- Generate from a single collider start
colliderGen :: ColliderStart -> StuffGen
colliderGen (pos0, vel0, acc, collider, shooter) env@(user,_) =
  (collStuff `untilB` explode) `unionSS` shootStuff
 where
   collStuff = ColliderSS pos vel collider
   pos       = constantB pos0 ^+^ atRate vel user
   vel       = constantB vel0 ^+^ atRate acc user ^+^ sumVSE impulse
   collide   = collStuff `collideCE` env
   --collide   = neverE
   impulse   = collide `filterE` collisionBounce
   explode'  = neverE
               -- predicate falseB user -=> emptySS
               -- collide `suchThat` const False -=> emptySS
               -- collide -=> emptySS
               -- explode -=> emptySS
               -- userTimeIs 2 user -=> emptySS
   explode   = collide `filterE` collisionExplode
                       `afterE`  env
                       ==>       uncurry ($)
   -- Stop shooting after we get blown up
   shooter' = shooter `untilB` explode'  -=> neverE
   shootStuff' = eventGen neverE env
   shootStuff = eventGen (--trace "eventGen argument\n" $
                          shooter' `snapshot` tripleB pos vel time
                                   ==>        shootMissile)
                         env
   shootMissile (angleShoot, (motShoot, velShoot, tShoot)) env =
     --trace "shootMissile\n" $
     colliderGen (motShoot, velShoot', zeroVector, Missile radius, neverE) env
        `untilB` timeIs (tShoot + dur) -=> emptySS
    where
      -- The radius goes from max to min in "dur" seconds
      radius    = max - (max-min) * (time - constantB tShoot) / constantB dur
      velShoot' = velShoot + S.vector2Polar boost angleShoot
      boost     = 0.2  -- boost beyond ship velocity
      max = 0.2; min = 0.0; dur = 10

sumVSE :: S.VectorSpace v => Event v -> Behavior v
sumVSE ev = stepper S.zeroVector (scanlE (S.^+^) S.zeroVector ev)


-- Collision information
data Collision
  = Bounce Vector2                    -- impulse vector
  | Explode StuffGen                  -- Explosion

collisionBounce :: Collision -> Maybe Vector2
collisionBounce (Bounce impulse) = Just impulse
collisionBounce _                = Nothing

collisionExplode :: Collision -> Maybe StuffGen
collisionExplode (Explode newGen) = Just newGen
collisionExplode _                = Nothing


-- The event of a moving collider colliding with a given environment (CE)
-- or another collider (CC).  The event says what happens to the first
-- argument.
collideCE, collideCC :: SpaceStuff -> Env -> Event Collision
collStuff `collideCE` env@(user,envStuff) = collide envStuff
 where
   collide (NothingSS _)            = neverE
   collide (stuff `UnionSS` stuff') = collide stuff .|. collide stuff'
   collide (stuff `UntilSS` ev)     =
     collide stuff `untilBE'` 
       ev `afterE` (user, collStuff)
        ==>   \ (envStuff', (user', collStuff')) ->
                 collStuff' `collideCE` (user',envStuff')
   collide collStuff'           = collStuff `collideCC` (user,collStuff')

traceB str = lift1 (trace str)

ColliderSS mot vel coll `collideCC` (user, ~(ColliderSS mot' vel' coll')) = 
  collideE `snapshot_` tripleB mot (vel-vel') time ==> classify
 where
   collideE  = predicate colliding user
   colliding =     magnitude relMot  <* minDist   -- too close
               &&* dot relVel relMot <* 0         -- moving closer
   relMot  = mot - mot'
   relVel  = vel - vel'
   minDist = collRadius coll + collRadius coll'
   classify (motHit, relVelHit, tHit) =
     -- The following line makes the classification always bounce.
     -- Comment it out normally.
     -- const bounce $
     case coll of
       Missile _ -> explode missileExplosion
       Ship _    -> explode shipExplosion
       Roid n    -> case coll' of
                    Missile _  -> 
                      --if n == maxRoidType then roidExplosion else roidSplit n
                      roidExplosion
                    otherwise -> bounce
    where
      bounce        = Bounce  ((-1.9) S.*^ relVelHit)
      explode imF   = Explode (const (NothingSS (imF tHit)))
      roidExplosion = Explode (const emptySS)
      -- Fill this in later
      -- roidSplit n   = Explode (collidersGen [(motHit ...

-- ## Note: the dot product test is wrong.  We shouldn't be using just
-- relMot, but rather something involving minDist as well.  Maybe we need
-- to use "derivative (magnitude relMot) >* 0", which means they are
-- moving apart. ##

collRadius :: Collider -> RealB
collRadius (Ship _)         = shipRadius
collRadius (Roid n)         = roidRadius n
collRadius (Missile radius) = radius

explosion :: HFlipBook -> RealVal -> DTime -> Time -> ImageB
explosion book pagesPerSec dur t0 =
  flipImage book pageB `untilB` timeIs (t0 + dur) -=> emptyImage
 where
   pageB = constantB pagesPerSec * (time - constantB t0)
  
shipExplosion, roidExplosion :: Time -> ImageB
--shipExplosion = ??? shipExpBook shipExpPagesPerSec shipExpDur
--roidExplosion = ??? roidExpBook roidExpPagesPerSec roidExpDur

shipExplosion t0 = emptyImage
roidExplosion t0 = emptyImage
missileExplosion t0 = emptyImage
-- Concerns: don't hang onto whole coll; shields

----

-- Experimental version of untilB for events.  This one terminates more
-- often in mutual recursions.  Correctness is questionable.
-- 
-- Here's the result.  If I call loop1 in the mb2==Nothing case below, I
-- don't get wedged in an infinite loop, but I do 

Event possOccs1 `untilBE'` Event possOccs2 =
  Event (loop1 possOccs1 possOccs2)
 where
   -- loop1 assumes that te1 <= te2, and so can make progress without
   -- pattern matching.
   loop1 (po1 : pos1') pos2 = po1 : loop2 pos1' pos2
   loop1 [] pos2 = possOccsOf (joinEOne (Event pos2))

   loop2 pos1@(po1@(te1,mb1) : pos1')
         pos2@(po2@(te2,mb2) : pos2') =
     if te1 <= te2 then
       -- Still old event.  Emit first possible occurrence.
       --trace (show te1 ++ " <= " ++ show te2 ++ "\n")$
       po1 : loop2 pos1' pos2
     else --trace (show te1 ++ " > " ++ show te2 ++ " and ")$
          case mb2 of
            Just e2' -> -- Real first occurrence.  Switch.
                        --trace "occurrence\n"$
                        possOccsOf e2'
            Nothing  -> --trace "non-occurrence\n"$
                        (te2,Nothing) : loop1 pos1 pos2'
   loop2 [] pos2 = --trace "Event untilB: no more LHS occurrences\n" $
                        possOccsOf (joinEOne (Event pos2))
   loop2 pos1 [] = pos1


-- This next guy should be in GBehavior.hs and reused where possible.

untilBAfterTimes :: GBehavior bv => bv -> Event bv -> [Time] -> [bv]
untilBAfterTimes bv e ts =
  loop ts (bv `afterTimes` ts) (e `occs` ts) (e `afterTimes` ts)
 where
   loop ts _ (Just (_, bv') : _) _ = bv' `afterTimes` ts

   loop (_:ts') (bvAfter : bvAfters')
        (Nothing : mbOccs') ~eAfters@(eAfter : eAfters') =
     -- See snapshot in BehaviorEvent.hs for an explanation of the `seq`.
     -- Unfortunately, it adds too much strictness in this case, and
     -- without it we have a fast space leak.
     (bvAfter `untilB` eAfter)
      : ({-eAfters `seq`-} loop ts' bvAfters' mbOccs' eAfters')
