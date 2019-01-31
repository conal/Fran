-- "Image behavior" type defined directly, i.e., without Image or Behavior.

module ImageB where

import qualified HSpriteLib as SL
import BaseTypes
import qualified StaticTypes as S
import Behavior
import GBehavior
import Vector2B
import RectB
import ColorB
import Point2B
import TextB
import Transform2B
import Event
import qualified RenderImage as R
import SoundB
import Maybe (fromMaybe)
import Concurrent
import IOExts (trace)

infixl 6 `over`, `under`

defaultColor = red

data ImageB
 = EmptyImage                           -- completely transparent
 | SolidImage                           -- solid color
 | FlipImage  SL.HFlipBook RealB        -- page # behavior
 | RenderImage RenderIO                 -- renders to a DDSurface
 | SoundI     SoundB                    -- embedded sound
 | Over       ImageB   ImageB           -- overlay
 | TransformI Transform2B ImageB        -- transformed image
 | WithColorI ColorB ImageB             -- colored image
 | CropI      RectB ImageB              -- cropped image
 | CondI      BoolB ImageB ImageB       -- conditional image
 | UntilI     ImageB (Event ImageB)	-- "untilB" on ImageB
 | TimeTransI ImageB  TimeB             -- timeTransform on ImageB
 deriving Show


-- Primitives

-- The empty ImageB
emptyImage :: ImageB
emptyImage = EmptyImage

-- Solid color image.  Useful for background for now and later with
-- stenciling.  Use with "withColor".
solidImage :: ImageB
solidImage = SolidImage

-- Flipbook-based ImageB, given page # behavior
flipImage :: SL.HFlipBook -> RealB -> ImageB
flipImage = FlipImage

-- renders to a DDSurface
renderImage :: RenderIO -> ImageB
renderImage = RenderImage

-- Embed a sound
soundImage :: SoundB -> ImageB
soundImage SilentS = EmptyImage
soundImage s       = SoundI s

-- overlay
over, under :: ImageB -> ImageB -> ImageB
EmptyImage `over` bot = --trace "(EmptyImage `over`) optimization\n" $
                         bot
top `over` EmptyImage = --trace "(`over` EmptyImage) optimization\n" $
                         top
top `over` bot       = top `Over` bot

bot `under` top = top `over` bot

-- colored image
withColor :: ColorB -> ImageB -> ImageB
withColor _ EmptyImage = EmptyImage
withColor c im         = WithColorI c im

-- crop image
crop :: RectB -> ImageB -> ImageB
crop _ EmptyImage = EmptyImage
crop r im         = CropI r im

-- transform image
instance Transformable2B ImageB where
  _  *% EmptyImage = EmptyImage
  xf *% im         = xf `TransformI` im


-- Perhaps move this SyncVar stuff elsewhere.

-- Synchronization variables.  These guys always come in request/reply
-- pairs.  True means continue with next step, while False means stop.
-- Use is take a request, do one step of action, and put a reply.
-- Question: could I package up this pattern into a combinator?

type SyncVar = MVar Bool

-- Forward requests to replies.
forwardSyncVars :: SyncVar -> SyncVar -> IO ()
forwardSyncVars requestV replyV = do
  --putStrLn "forwardSyncVars"
  let forward = do --putStrLn "forwarding"
                   continue <- takeMVar requestV
                   putMVar replyV continue
                   if continue then forward else return ()
  forkIO forward


-- Renderer creator (should be forkIO'd).  See Spritify.hs
type RenderIO = RectB -> Maybe ColorB -> Transform2B -> Maybe TimeB -> Time
             -> [Time] -> SyncVar -> SyncVar -> SL.SpriteTreeChain
             -> IO SL.HSimpleSprite


type SurfBGen = RectB -> Maybe ColorB -> Transform2B ->
                Maybe TimeB -> SurfaceULB

-- ## In progress: use the rect so we can pre-crop!  Could save a lot of
-- video memory and rendering time.  Tricky issue:

-- Note: the rendering happens only 10 time per second, while the
-- translation happens at roughly 50.  Inbetween renderings, regions that
-- were cropped away at the time of rendering become exposed and so should
-- be visible but aren't.  Fix for now: scale up the cropping rectangle by
-- some factor.  The appropriate value depends on dynamic factors.  To do:
-- figure out some way to use the largest cropping rectangle needed for
-- each time interval.
syntheticCropStretch = 1.2

syntheticImage :: SurfBGen -> ImageB

syntheticImage f = RenderImage renderIO
 where
  renderIO rectB mbColorB xfB mbTT t0 ts requestV replyV above = do
    let surfaceULB = f (expandRect (constantB syntheticCropStretch) rectB)
                       mbColorB xfB mbTT
        ((R.SurfaceUL surf0 ulX0 ulY0 motX0 motY0), surfULBs) = ats0 surfaceULB t0 ts
        (S.RectLLUR (S.Point2XY llx0 lly0) (S.Point2XY urx0 ury0), rects) =
           ats0 rectB t0 ts
    hSimpleSprite <- SL.newSimpleSprite surf0 ulX0 ulY0
                       llx0 lly0 urx0 ury0
                       motX0 motY0 1 1 above
    --putStrLn "Made new SimpleSprite"
    let update ~(t:ts')
               ~((R.SurfaceUL surf ulX ulY motX motY):surfaceULs')
               ~(S.RectLLUR (S.Point2XY llx lly)
                            (S.Point2XY urx ury) : rects') = do
          --putStrLn "updating simple sprite"
          continue <- takeMVar requestV
          if continue then do
            --putStrLn "renderIO got request"
            --putStrLn $ "surf is " ++ show surf
            --putStrLn "setting surface"
	    -- scale 1 1 for now and (ulX, ulY) is upper-left corner
	    -- of the bounding box in Fran coord system
            --putStrLn ("New surf: (ulX,ulY) == " ++ show (ulX,ulY) ++ ", (motX,motY) == " ++ show (motX,motY))
	    SL.updateSimpleSprite hSimpleSprite
                                  t surf ulX ulY
                                  llx lly urx ury 
                                  motX motY 1 1
            --putStrLn "renderIO replying"
            putMVar replyV True
            update ts' surfaceULs' rects'
           else
            putMVar replyV False
    forkIO $ update ts surfULBs rects
    return hSimpleSprite


instance  GBehavior ImageB  where
  untilB        = UntilI
  afterTimes	= afterTimesI
  timeTransform = TimeTransI
  condBUnOpt    = CondI


afterTimesI :: ImageB -> [Time] -> [ImageB]

EmptyImage `afterTimesI` _ = repeat EmptyImage

SolidImage `afterTimesI` _ = repeat SolidImage

FlipImage book page `afterTimesI` ts =
 map (FlipImage book) (page `afterTimes` ts)

-- Is this one right???
-- im@(SyntheticImageIO f) `afterTimesI` t = im

-- ## Is this one right?
im@(RenderImage f) `afterTimesI` ts =
  repeat im
  -- error "afterTimesI not yet supported for RenderImage, sorry."

-- Crucial efficiency note: use "soundImage", "*%", etc, rather than
-- the constructors, so that optimizations like (EmptyImage `over`) can
-- kick in.

SoundI snd `afterTimesI` ts = map soundImage (snd `afterTimes` ts)

(imb `Over` imb') `afterTimesI` ts =
  zipWith over (imb `afterTimesI` ts) (imb' `afterTimesI` ts)

TransformI xfb imb `afterTimesI` ts =
  zipWith (*%) (xfb `afterTimes` ts) (imb `afterTimesI` ts)

WithColorI c imb `afterTimesI` ts =
  zipWith withColor (c `afterTimes` ts) (imb `afterTimesI` ts)

CropI rectb imb `afterTimesI` ts =
  zipWith crop (rectb `afterTimes` ts) (imb `afterTimesI` ts)

CondI c imb imb' `afterTimesI` ts =
  zipWith3 condB
           (c    `afterTimes`  ts)
           (imb  `afterTimesI` ts)
           (imb' `afterTimesI` ts)

-- ## This one is essentially copied from Behavior.hs, and is almost
-- identical to the GeometryB and SoundB versions.  Figure out how to
-- consolidate.
(imb `UntilI` e) `afterTimesI` ts =
  loop ts (imb `afterTimesI` ts) (e `occs` ts) (e `afterTimes` ts)
 where
   -- ## should the bAfter and eAfter list patterns be lazy??
   loop ts _ (Just (_, imb') : _) _ = imb' `afterTimes` ts

   loop (_:ts') (imbAfter : imbAfters')
        (Nothing : mbOccs') (eAfter : eAfters') =
     (imbAfter `UntilI` eAfter) : loop ts' imbAfters' mbOccs' eAfters'


-- utilities

-- Overlay of list of ImageB's.  First one is on top for overs and on
-- bottom for unders.
overs, unders :: [ImageB] -> ImageB
overs  = foldr over  emptyImage
unders = foldr under emptyImage

-- circle

circleSurface :: SurfBGen
circleSurface rectB mbColorB stretchB _ =
  lift3 R.renderCircle rectB
        (fromMaybe defaultColor mbColorB) stretchB

circle :: ImageB
circle = syntheticImage circleSurface

-----------------------------------------------------------------
-- Naming convection: the ones that are directly lifted are
-- suffixed with a "B" and those that are more frequently used are
-- without.
-----------------------------------------------------------------

-- poly: polygon, polyline, polyBezier

polygonSurfaceB, polylineSurfaceB, polyBezierSurfaceB
  :: Behavior [S.Point2] -> SurfBGen
polygonSurfaceB    = polySurface R.renderPolygon
polylineSurfaceB   = polySurface R.renderPolyline
polyBezierSurfaceB = polySurface R.renderPolyBezier

polygonSurface    :: [Point2B] -> SurfBGen
polylineSurface   :: [Point2B] -> SurfBGen
polyBezierSurface :: [Point2B] -> SurfBGen

polygonSurface    pts = polySurface R.renderPolygon    (bListToListB pts)
polylineSurface   pts = polySurface R.renderPolyline   (bListToListB pts)
polyBezierSurface pts = polySurface R.renderPolyBezier (bListToListB pts)

polySurface :: ([S.Point2] -> R.SurfGen) -> Behavior [S.Point2] -> SurfBGen
polySurface renderF pts cropRectB mbColorB stretchB mbTT =
  -- The color and stretch have been time transformed, but the points haven't.
  lift4 renderF (mbTTrans pts mbTT)
                cropRectB (fromMaybe defaultColor mbColorB) stretchB

polygonB    :: Behavior [S.Point2] -> ImageB
polylineB   :: Behavior [S.Point2] -> ImageB
polyBezierB :: Behavior [S.Point2] -> ImageB

-- These ones allow the number of vertices to change
polygonB    pts = syntheticImage (polygonSurfaceB    pts)
polylineB   pts = syntheticImage (polylineSurfaceB   pts)
polyBezierB pts = syntheticImage (polyBezierSurfaceB pts)

-- These ones are more convenient in the common case
polygon    :: [Point2B] -> ImageB
polyline   :: [Point2B] -> ImageB
polyBezier :: [Point2B] -> ImageB

polygon    pts = syntheticImage (polygonSurface    pts)
polyline   pts = syntheticImage (polylineSurface   pts)
polyBezier pts = syntheticImage (polyBezierSurface pts)

bezier :: Point2B -> Point2B -> Point2B -> Point2B -> ImageB
bezier p1 p2 p3 p4 = polyBezier [p1,p2,p3,p4]

-- square, regularPolygon, star

square :: ImageB
square = polygon $ map f [0 .. 3]
  where
    f i = let theta = pi / 4 * (1 + 2 * fromIntegral i)
	  in  point2Polar 1 theta

-- A regular polygon with given number of vertices
regularPolygon :: IntB -> ImageB
regularPolygon vertices = star 1 vertices

-- Star figure.  Arguments: skip and vertices.  For instance, (star 7 3)
-- is a seven-pointed star connecting every third vertex of what would be
-- a regular 7-gon.
star :: IntB -> IntB -> ImageB
star skip vertices = polygonB (lift2 f vertices skip)
  where
    f :: Int -> Int -> [S.Point2]
    f v s = let theta = 2 * pi * fromInt s / fromInt v
	    in  [ S.point2Polar 1 (theta * fromInt i) | i <- [0 .. v] ]

-- line

lineSurface :: Point2B -> Point2B -> SurfBGen
lineSurface p0 p1 cropRectB mbColorB stretchB mbTT =
  lift5 R.renderLine (mbTTrans p0 mbTT)
                     (mbTTrans p1 mbTT)
                     cropRectB
                     (fromMaybe defaultColor mbColorB) stretchB

line, lineSegment :: Point2B -> Point2B -> ImageB
line p0 p1 = syntheticImage (lineSurface p0 p1)

-- The "line" primitive should be renamed "lineSegment", and there should
-- really be an infinite "line", which`` exploits cropping.

lineSegment = line

-- textB, colorB, stretchB

textSurface :: TextB -> SurfBGen
textSurface textB cropRectB mbColorB xfB mbTT =
  lift4 R.renderText
	(mbTTrans textB mbTT)
	cropRectB
        (fromMaybe defaultColor mbColorB)
	xfB

textImage :: TextB -> ImageB
textImage textB = syntheticImage (textSurface textB)

type SurfaceULB = Behavior R.SurfaceUL


-- Perhaps this should go elsewhere, but where?

flipBookSize :: SL.HFlipBook -> S.Vector2
flipBookSize book =
  S.Vector2XY (fromInt (toInt wPix) / R.importPixelsPerLength)
              (fromInt (toInt hPix) / R.importPixelsPerLength)
 where
   (wPix,hPix) = SL.flipBookSizePixels book
   -- Note: the toInt is because Pixels == Int32


-- Renderable class.  Phasing in.  To do: rename "renderGeometry" and
-- "renderText".

class Renderable a where
  render :: a -> ImageB

instance Renderable a => Renderable [a] where
  render = overs . map render