-- Collisions demo, using spatial search trees from SpaceTree.
--
-- To do:
--  + Time-varying box, tracking the window.
--  + Arbitrary polyline walls.

module Collide where

import Fran
import qualified StaticTypes as S

import SpaceTree

-- Inhabitant.  For now, just ball or a box.
data Inhab =
    BallInhab S.Vector2                 -- motion
              S.Vector2                 -- velocity
              RealVal                   -- radius & mass
              S.Color
  | BoxInhab

instance Bounded2 Inhab where
  bbox BoxInhab = boxRect

  bbox (BallInhab mot _ radius _) =
    S.rectFromCenterSize (S.origin2 S..+^ mot) (S.vector2XY w h)
   where
    w = 2 * radius; h = w

boxRect :: S.Rect
boxRect = S.rectFromCorners (S.point2XY minX minY)
                            (S.point2XY maxX maxY)

-- Behavior-level version.  (Needed only because there's no S.Image, which
-- is somewhat annoying.)  Either a moving ball or a fixed box.  The
-- ball's radius and color are fixed except during it's introduction by
-- addBallWithFeedback.
data InhabB =
    BallInhabB Vector2B                 -- motion
               Vector2B                 -- velocity
               RealB                    -- radius & mass
               ColorB
  | BoxInhabB

instance Renderable InhabB where
  render (BallInhabB mot _ radius color) = 
    move    mot     $
    stretch radius  $
    withColor color $
    circle
  render BoxInhabB = boxIm


boxIm :: ImageB
boxIm = stretch boxSize $
        withColor green $
        polyline points
 where
   points = [ point2Polar r (a * (fromInt i + 0.5)) | i <- [0 .. 4] ]
    where
      a = pi / 2
      r = sqrt 2 / 2


-- A collision environment is a spatially organized set of obstacles.
-- type Env where

-- Make collision environment
collideEnv :: [Inhab] -> Env
-- Cull an environment leaving just those members that might overlap with
-- a given bounding rectangle.
searchEnv :: Env -> S.Rect -> (Inhab -> Maybe b) -> Maybe b

--   Trivial implementation:
-- type Env = [Inhab]
-- collideEnv = id
-- searchEnv inhabs _ test = concat (map test inhabs)

--   Using spatial partition trees
type Env = PTree Inhab
collideEnv = mkPTree boxRect
searchEnv  = searchPTree

-- or abstract:
-- type Env = Inhab -> Maybe Collision


-- Collision info: impulse vector
type Collision = S.Vector2

-- Environment over time.  It will be crucially important to implement
-- these values incrementally, via destructive modification.  For now,
-- just a behavior.
type EnvB = Behavior Env


-- To do: work on velocities only and take relative environment.
startInhab :: EnvB -> User -> Inhab -> InhabB
startInhab envB u BoxInhab = BoxInhabB
startInhab envB u (BallInhab mot0 vel0 radius color) = inhabB
 where
  inhabB  = BallInhabB mot vel (constantB radius) (constantB color)
  mot     = constantB mot0 + atRate vel u
  vel     = stepAccum vel0 (impulse ==> (+))
  impulse = maybeBE (collisionB envB inhabB) u

-- Make a box and add some balls, under user control.  See
-- addBallWithFeedback below.
-- afterE'ing envB below caused a stack overflow when the $*/untilB
-- optimization is enabled in Behavior.hs.  I haven't figured out what's
-- going on.

startAll :: User -> ImageB
startAll u = accumB over ballFeedback (newInhabB ==> render)
 where
   newInhabB = newInhab `afterE`  (u, envB)
                        ==> \ (inhab, (u, envB)) -> startInhab envB u inhab
   envB = lift1 collideEnv $
          switchAccum nilB (newInhabB ==> inhabBToBvr ==> consB)
   newInhab = addBox .|. addBall
   (addBall, ballFeedback) = addBallWithFeedback u
   -- Add the box initially.  (Something of a hack, but what alternative?)
   addBox = userTimeIs 0 u -=> BoxInhab

-- Add-ball event, plus visual feedback.  Tracks mouse and wiggles color
-- and size.  Snapshotted when button released.
addBallWithFeedback :: User -> (Event Inhab, ImageB)
addBallWithFeedback u = (addE, im)
 where
   addE = lbr u `snapshot_` inhabBToBvr ball
   im   = ifB (leftButton u) (render ball) emptyImage
   ball = BallInhabB (mouseMotion u)
                     (rate (mouseMotion u) u)
                     (wiggleRange 0.1 0.3)
                     (colorHSL (time * 1.5) 0.5 (wiggleRange 0.3 0.7))


collision :: Env -> Inhab -> Maybe Collision
collision env BoxInhab = Nothing
collision env inhab@(BallInhab bMot bVel bSize _) =
  {-_scc_ "collision"-} ( searchEnv env (bbox inhab) collWith )
 where
   collWith :: Inhab -> Maybe S.Vector2
   collWith obst = do
     (oVel, iDir, mbOMass) <- checkColl obst
     let oRelVel = oVel - bVel
         mRatio = case mbOMass of
                    Nothing -> 2
                    Just oMass -> 2 * oMass / (bSize + oMass)
         impMag = oRelVel `S.dot` iDir
     return ((mRatio * impMag) S.*^ iDir)

   -- Check for collision.  Yields velocity of obstacle, direction of
   -- impulse as unit vector, and mass of obstacle, or Nothing if infinite.
   checkColl :: Inhab -> Maybe (S.Vector2, S.Vector2, Maybe RealVal)
   checkColl (BallInhab oMot oVel oSize _) =
     if colliding then Just (oVel, iDir, Just oSize) else Nothing
    where
      colliding = S.magnitude  relMot < bSize + oSize &&
                  S.dot relVel relMot < 0
      relMot    = bMot - oMot
      relVel    = bVel - oVel
      iDir      = S.normalize relMot

   checkColl BoxInhab
     | nx==0 && ny==0 = Nothing
     | otherwise      = Just (S.zeroVector, iDir, Nothing)
    where
      nx | x-bSize < minX && velX < 0  = -1
         | x+bSize > maxX && velX > 0  =  1
         | otherwise = 0
      ny | y-bSize < minY && velY < 0  = -1
         | y+bSize > maxY && velY > 0  =  1
         | otherwise = 0

      S.Vector2XY velX velY = bVel
      S.Vector2XY x    y    = bMot

      iDir = S.normalize (S.Vector2XY nx ny)


collisionB :: EnvB -> InhabB -> MaybeB Collision
collisionB envB inhabB = lift2 collision envB (inhabBToBvr inhabB)

inhabBToBvr :: InhabB -> Behavior Inhab
inhabBToBvr BoxInhabB = constantB BoxInhab
inhabBToBvr (BallInhabB motB velB size color) =
  lift4 BallInhab motB velB size color


main = displayU startAll


-- Misc settings

boxSize, minX, maxX, minY, maxY :: Fractional a => a
boxSize = 2
minX = -maxX
maxX = boxSize / 2
maxY = maxX
minY = minX

