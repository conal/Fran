{-# OPTIONS -#include <windows.h> #-}

-- "Geometry behavior" type.

-- To do:
--
-- Figure out why the update rate of animations with lots of *differently*
-- colored meshes slows down over time.  Quite puzzling.
-- 
-- Make this stuff and the similar code in Spritify be smart about
-- constant behaviors.  Should be easy, especially if I move to the
-- setterB approach used in places below.
-- 
-- Time transformation and untilB.  Not hard.
-- 
-- Much better code factoring!
-- 
-- Deallocate the renderer, etc.
--
-- Embedded sounds
-- 
-- It looks like D3DRM is locking out DDraw, which kills the whole idea of
-- separating rendering rate from sprite update rate.
-- 
-- Sound should be separated from sprite trees and get "painted"
-- (parameter-tweaked) at its own rate.  But how do we get sounds to be
-- stopped when appropriate?  Maybe we need to augment the UpdateIO
-- constructor with a cleanup action, which would invoke Release()
-- on DirectX objects.  Maybe add reference counting to sprite trees.  Be
-- very careful about keeping proper ref counts.
--
-- Make the geometry surface get built before all the geometry stuff, for
-- reduced latency.
-- 
-- Make sure the DirectX objects with made with functional interfaces get
-- released!  (Use malloc pointers.)
-- 
-- TextureG


module GeometryB where

import BaseTypes
import qualified StaticTypes as S
import qualified HSpriteLib as SL
import Behavior
import GBehavior
import Vector3B
import ColorB
import SoundB
import ImageB
import Event
import Transform3B
--import RenderImage (screenPixelsPerLength)
import Concurrent
import Maybe

-- The fixities of `unionG` and **% are the same as + and *
infixl 6  `unionG`

data GeometryB
   = EmptyGeometry
   | MeshG      SL.HMeshBuilder         -- D3DRM mesh builder
   | LightG     SL.LightType            -- embedded light     
   | SoundG     SoundB                  -- embedded sound
   | UnionG     GeometryB GeometryB
   | TransformG Transform3B GeometryB
   | ColorG     ColorB GeometryB        -- Apply a color
   | TextureG   ImageB GeometryB
   | UntilG     GeometryB (Event GeometryB)     -- "untilB" on GeometryB
   | TimeTransG GeometryB TimeB             -- timeTransform on ImageB
   deriving Show

-- Primitives

-- The empty geometry
emptyG :: GeometryB
emptyG = EmptyGeometry

-- Geometry based on polygon mesh.  See meshBuilder in SpriteLib
meshG :: SL.HMeshBuilder -> GeometryB
meshG = MeshG

-- Light geometry.  See LightType in SpriteLib
lightG :: SL.LightType -> GeometryB
lightG = LightG

-- Sound embedded in Geometry.  Sorry -- not yet available.
-- soundG :: SoundB -> GeometryB
-- soundG = SoundG

-- Form union of two geometric models
unionG :: GeometryB -> GeometryB -> GeometryB
unionG = UnionG

-- unionG over a list
unionGs :: [GeometryB] -> GeometryB
unionGs = foldr UnionG emptyG

withColorG :: ColorB -> GeometryB -> GeometryB
withColorG = ColorG

-- Apply an animated texture.  Sorry -- not yet available.
-- texture :: ImageB -> GeometryB -> GeometryB
-- texture = TextureG

instance  GBehavior GeometryB  where
  untilB	= UntilG
  afterTimes    = error "afterTime not yet implemented for GeometryB, sorry."
  timeTransform = TimeTransG
  condBUnOpt    = error "condBUnOpt not yet implemented for GeometryB, sorry."


--transformG :: Transform3B -> GeometryB -> GeometryB
instance  Transformable3B GeometryB  where
  (**%) = TransformG


-- Convenient constant light geometries.  Apply transforms and colors to
-- these guys.

ambientLightG       = lightG SL.ambientLight
pointLightG         = lightG SL.pointLight
spotLightG          = lightG SL.spotLight
directionalLightG   = lightG SL.directionalLight
parallelPointLightG = lightG SL.parallelPointLight

-- Because of a D3D problem before DirectX 5.0, I need to use a fixed view
-- size (sorry!), multiplied by the initial 2D scale factor.  This
-- constant product give the height and width in Fran units of the
-- rendered image.  2.2 is about big enough to fill up the initial window
geometryRenderSize :: RealVal
geometryRenderSize = 3.0

-- Render a geometry with a transformed default camera.  Currently there
-- is no Camera type, so just provide the transform.

renderGeometry :: GeometryB -> Transform3B -> ImageB

renderGeometry geom cameraXfB = renderImage renderIO
 where
  renderIO rectB mbColorB xfB mbTT t0 ts requestV replyV above = do
    -- Make the scene and camera
    --putStrLn ("rG: making scene from geometry" ++ show geom)
    sceneFrame   <- SL.newScene
    geomRequestV <- newEmptyMVar
    geomReplyV   <- newEmptyMVar
    --putStrLn "rG: fillFrame"
    fillFrame sceneFrame geom' mbTT t0 ts geomRequestV geomReplyV
    cameraFrame  <- SL.newHFrame sceneFrame
    let (S.RectLLUR (S.Point2XY llx0 lly0) (S.Point2XY urx0 ury0), rects) =
           ats0 rectB t0 ts
        (S.Transform2 _ scale0 _, xfs) = ats0 xfB t0 ts

        ulX = - geometryRenderSize/2 * scale0
        ulY =   geometryRenderSize/2 * scale0
    -- Initialization: it's awkward to come up with an initial surface, so
    -- give a null surface.  Improve this stuff later.  Then I'll need to
    -- pass t0 around in fillFrameRec
    -- For now, use a fixed scale.  Fix later to use scaleB.  The problem is
    -- I don't know how to resize the D3DRM device with a tolerable
    -- efficiency.  DirectX 5 fixes this problem.
    --putStrLn $ "new renderer with scale " ++ show scale0 ++ " and renderSize " ++ show geometryRenderSize ++ "\n"
    renderer     <- SL.newRMRenderer sceneFrame cameraFrame scale0
                                     geometryRenderSize
    hSimpleSprite <- SL.newSimpleSprite SL.nullHDDSurface ulX ulY
                     llx0 lly0 urx0 ury0
                     0 0 scale0 scale0 above

    let xts = case mbTT of
                Just tt -> tt `ats` ts
                Nothing -> ts
        update ~(t:ts')
               ~(S.RectLLUR (S.Point2XY llx lly)
                            (S.Point2XY urx ury) : rects')
               ~(cameraXf:cameraXfs')
               ~(S.Transform2 (S.Vector2XY dx dy) scale rotate : xfs') = do
          continue <- takeMVar requestV
          --putStrLn "geometry got request"
          -- Update D3D RM geometry
          putMVar geomRequestV continue
          if continue then do
            -- Sync with the geometry update threads
            -- ## Why can't I say "True <- ..." here?  I get a complaint
            -- that IO isn't in MonadZero.
            _ <- takeMVar geomReplyV
            -- Factor the 2D scale into the camera transform.
            let cameraXfAdjust = S.rotate3 S.yVector3 rotate
            -- Set the camera frame
            S.hFrameSetTransform cameraFrame
                                 (cameraXfAdjust `S.compose3` cameraXf)
            SL.hRendererSetScale renderer (abs scale)
            -- If the scale is negative
            let scaleSign = signum scale
            -- Do the rendering and return the surface
            newSurface <- SL.doRMRenderer renderer
            --putStrLn "renderer produced new surface"
            -- Bogus: using (-1,1) as upper left, because that's what the
            -- renderer does for now.
	    SL.updateSimpleSprite hSimpleSprite t newSurface ulX ulY
                                  llx lly urx ury 
                                  dx dy scaleSign scaleSign
            putMVar replyV True
            update ts' rects' cameraXfs' xfs'
           else do
            _ <- takeMVar geomReplyV
            putMVar replyV False
    forkIO $ update ts rects (cameraXfB `ats` xts) xfs
    return hSimpleSprite

  -- Throw in light.  I was coloring the geometry, but I realized it's
  -- bogus to do so.  I think that the proper, but not very useful,
  -- meaning would be to apply the color and use flat shading.
  geom' = sceneLight `unionG` geom

  sceneLight = withColorG (grey 0.4) ambientLightG `unionG`
               lightTransform **% directionalLightG

  -- Light transform.  Start with default camera transform, so that the light
  -- is pointing directly at the subject.  Then tilt a little down and to
  -- the left so we get light from the over the viewer's right shoulder.
  lightTransform = rotate3 xVector3 ( pi/6) `compose3`
                   rotate3 zVector3 (-pi/6) `compose3`
                   defaultCamera



-- The identity transform leaves us at the origin and looking up (along
-- the positive Z axis).  Instead we want to be looking at the origin from
-- the positive Y axis, standing back far enough to look at a unit sphere
-- comfortably.

defaultCamera :: Transform3B
defaultCamera = rotate3 xVector3 (pi/2) `compose3`
                translate3 (vector3XYZ 0 0 (-4))


-- Construct D3DRM frames and update them

fillFrame :: SL.HFrame -> GeometryB -> Maybe TimeB -> Time -> [Time]
          -> SyncVar -> SyncVar -> IO ()

fillFrame parentFrame = fillFrameRec parentFrame Nothing


fillFrameRec :: SL.HFrame -> Maybe ColorB -> GeometryB -> Maybe TimeB
             -> Time -> [Time]
             -> SyncVar -> SyncVar -> IO ()

fillFrameRec _ _ EmptyGeometry _ = \ _ _ -> forwardSyncVars

fillFrameRec parentFrame mbColorB (MeshG builder) _ =
  coloredLeaf parentFrame mbColorB
              (\ frame -> --putstrLn "adding mesh builder" >>
                          SL.hFrameAddMeshBuilder frame builder)

fillFrameRec parentFrame mbColorB (LightG lightType) _ =
  \ t0 ts requestV replyV -> do
  --putstrLn "adding light"
  hLight <- SL.newHLight parentFrame lightType
  -- No color.  Add the leaf and then just forward requests to replies.
  case mbColorB of
    Nothing -> forwardSyncVars requestV replyV
    Just colorB -> do
      let setterB = lift1 (SL.hLightSetColor hLight . S.colorToD3DColor) colorB
          (setter0, setters) = ats0 setterB t0 ts
          update ~(setter:setters') = do
            continue <- takeMVar requestV
            if continue then do
              setter
              putMVar replyV True
              update setters'
             else
              putMVar replyV False
      setter0
      forkIO $ update setters


{-
fillFrameRec parentFrame _ (SoundG sound) t0 xt0 =
  -- For sound, simply delegate to displaySoundB.  Hey!  How do we get the
  -- sound to be stopped when appropriate?
  ...
-}

fillFrameRec parentFrame mbColorB@(Just _) (ColorG colB geom) mbTT =
  -- Being colored from outside, which paints over the coloring here.
  -- (Unconventional, but simpler functional semantics, in my opinion.)
  fillFrameRec parentFrame mbColorB geom mbTT

fillFrameRec parentFrame Nothing (ColorG colorB geom) mbTT =
  -- No color from the outside.  Just pass down time-transformed color
  fillFrameRec parentFrame (Just (mbTTrans colorB mbTT)) geom mbTT

fillFrameRec parentFrame mbColorB (TransformG xfB geomB) mbTT =
  \ t0 ts requestV replyV -> do
  -- Make a new frame, which will be the parent for geomB.  The update
  -- action sets the frame's transform.  To do: introduce only one frame
  -- for multiple transforms.
  geomRequestV <- newEmptyMVar
  geomReplyV   <- newEmptyMVar
  -- Make a new frame so the color doesn't affect the siblings.
  --putstrLn "making new frame for transformed geometry"
  newFrame <- SL.newHFrame parentFrame
  fillFrameRec newFrame mbColorB geomB mbTT t0 ts geomRequestV geomReplyV
  let (xf0, xfs) = ats0 (mbTTrans xfB mbTT) t0 ts
      update ~(xf:xfs') = do
        continue <- takeMVar requestV
        putMVar geomRequestV continue
        if continue then do
          -- Set the transform and sync with geometry update threads
          S.hFrameSetTransform newFrame xf
          _ <- takeMVar geomReplyV
          putMVar replyV True
          update xfs'
         else do
          _ <- takeMVar geomReplyV
          putMVar replyV False
  S.hFrameSetTransform newFrame xf0
  forkIO $ update xfs

  
fillFrameRec parentFrame mbColorB (geomB `UnionG` geomB') mbTT =
  \ t0 ts requestV replyV -> do
  syncV  <- newEmptyMVar
  -- Add both parts to the frame.
  fillFrameRec parentFrame mbColorB geomB  mbTT t0 ts requestV syncV
  fillFrameRec parentFrame mbColorB geomB' mbTT t0 ts syncV    replyV


fillFrameRec parentFrame mbColorB (TimeTransG geomB ttInner) mbTT =
  fillFrameRec parentFrame mbColorB geomB (Just (mbTTrans ttInner mbTT))


-- Try to factor better to reuse spritifyUntilB in Spritify.hs

fillFrameRec parentFrame mbColorB (geomB1 `UntilG` e) mbTT =
  \ t0 ts requestV replyV -> do
  -- Fork a new thread that watches e and passes requests on to bv1's
  -- threads, but terminates when e occurs.  At that point, use requestV
  -- for the new bv.  (Verify !!)
  requestV1 <- newEmptyMVar                 -- request for bv1
  replyV1   <- newEmptyMVar                 -- reply   for bv1
  fillFrameRec parentFrame mbColorB geomB1 mbTT t0 ts
                           requestV1 replyV1
  let xts = case mbTT of
              Just tt -> tt `ats` ts
              Nothing -> ts
      monitor ~ts@(t:ts') ~(xt:xts') ~(mbOcc:mbOccs') = do
        -- First pass on the next request to bv1's sprite tree threads,
        -- causing an update or termination.
        continue <- takeMVar requestV
        -- Continue bv1 sprite threads if the untilB is being continued
        -- and the event hasn't yet occurred.  Otherwise terminate by
        -- putting a False into the request channel
        putMVar requestV1 (continue && not (isJust mbOcc))
        takeMVar replyV1
        if not continue then
          putMVar replyV False
         else
          -- Now check the event
          case mbOcc of
            Nothing -> do
              -- Non-occurrence.  Report one step done, and repeat.
              putMVar replyV True
              monitor ts' xts' mbOccs'
            Just (xte, (geomB2, mbColorB')) -> do
              -- Don't report one step done.  Instead, spritify bv2 and
              -- start updating it.
              -- Note: using the untilB's request and reply channels
              -- here, so we don't have to forward requests.  Otherwise
              -- with recursive reactive behaviors (e.g., produced by
              -- accumB and friends), the layers of forwarding would pile
              -- up.  Since we took the request and haven't fulfilled it,
              -- we first have to put the request back.
              putMVar requestV True
              -- Empty the frame before refilling.  ## How to implement
              -- emptyFrame??
              --putStrLn "SL.deleteFrameContents"
              -- ## Bug: the following line clears out the frame, but
              -- somehow prevents the fillFrameRec below from having
              -- visible effect.  I'm stumped.
              --SL.deleteFrameContents parentFrame
              -- ### What update time to use???  We just know the sample
              -- time of the event.  Heuristic pseudo-solution: if the
              -- first sample and update times are equal, use xte for te.
              -- Otherwise use t, which is a little later than
              -- appropriate.
              let te | t==xt     = xte
                     | otherwise = t
              --putStrLn "re-filling frame"
              fillFrameRec parentFrame mbColorB' geomB2 mbTT
                           te ts requestV replyV
  -- Avoid a space-time leak here: Don't hang onto the
  -- original color, motion and scale.  Let them age.
  forkIO $ monitor ts xts ((e `afterE` mbColorB) `occs` xts)


-- Add a possibly colored leaf in a given frame.
-- I thought I could use this guy for lights as well as meshes, but I was
-- wrong. 

coloredLeaf :: SL.HFrame -> Maybe ColorB -> (SL.HFrame -> IO ())
            -> Time -> [Time]
            -> SyncVar -> SyncVar -> IO ()

coloredLeaf parentFrame Nothing addLeaf =
  \ t0 ts requestV replyV -> do
  -- No color, so no new frame.  Add the leaf and then just forward
  -- requests to replies.
  --putstrLn "coloredLeaf with no color"
  addLeaf parentFrame
  forwardSyncVars requestV replyV


{-
coloredLeaf parentFrame (Just colorB) addLeaf =
  \ t0 ts requestV replyV -> do
  -- Make a frame containing the leaf and repeatedly set the color in the
  -- frame.
  --putstrLn "coloredLeaf with color.  Making new frame."
  newFrame <- SL.newHFrame parentFrame
  addLeaf newFrame
  let (color0, colors) = ats0 colorB t0 ts
      update ~(color:colors') = do
        continue <- takeMVar requestV
        if continue then do
          SL.hFrameSetColor newFrame (S.colorToD3DColor color)
          putMVar replyV True
          update colors'
         else
          putMVar replyV False
  SL.hFrameSetColor newFrame (S.colorToD3DColor color0)
  forkIO $ update colors
-}

-- Alternate formulation, using an IOB, which eliminates some redundancy.
coloredLeaf parentFrame (Just colorB) addLeaf =
  \ t0 ts requestV replyV -> do
  -- Make a frame containing the leaf and repeatedly set the color in the
  -- frame.
  --putstrLn "coloredLeaf with color.  Making new frame."
  newFrame <- SL.newHFrame parentFrame
  addLeaf newFrame
  let setterB = lift1 (SL.hFrameSetColor newFrame . S.colorToD3DColor) colorB
      setterB' = constantB (return ()) :: IOB()
      setterB'' = constantB (putStrLn "setterB''") :: IOB()
      (setter0, setters) = ats0 setterB t0 ts
      update ~(setter:setters') = do
        continue <- takeMVar requestV
        if continue then do
          -- There something bizarre going on here!!  The setter has a
          -- side-effect, even if I omit the next line, as is apparent
          -- when using setterB or setterB''
          setter
          putMVar replyV True
          update setters'
         else
          putMVar replyV False
  setter0
  forkIO $ update setters
