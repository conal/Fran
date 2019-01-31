-- "Geometry behavior" type defined directly, i.e., without Image or Behavior.
-- 
-- Last modified Fri Oct 03 10:54:35 1997

-- To do:
-- 
-- Much better code factoring!
-- 
-- Deallocate the renderer, etc.
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
-- Color and meshes.  Set the appropriate D3DRM property
-- 
-- TextureG, SoundG


module GeometryB where

import BaseTypes
import qualified StaticTypes as S
import qualified HSpriteLib as SL
import Behavior
import Vector3B
import ColorB
import SoundB
import ImageB
import Event
import Transform3B
import MVar
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

emptyG        = EmptyGeometry
meshG         = MeshG
lightG        = LightG
soundG        = SoundG
unionG        = UnionG
colorG        = ColorG
texture       = TextureG
withColorG    = ColorG

instance  GBehavior GeometryB  where
  untilB     = UntilG
  afterTimes = error "afterTime not yet implemented for GeometryB, sorry."

instance TimeTransformable GeometryB where timeTransform = TimeTransG

instance  Transformable3B GeometryB  where
  (**%) = TransformG


-- Convenient constant light geometries.  Apply transforms and colors to
-- these guys.

ambientLightG       = lightG SL.ambientLight
pointLightG         = lightG SL.pointLight
spotLightG          = lightG SL.spotLight
directionalLightG   = lightG SL.directionalLight
parallelPointLightG = lightG SL.parallelPointLight


-- Render a geometry with a transformed default camera.  Currently there
-- is no Camera type, so just provide the transform.

renderGeometry :: GeometryB -> Transform3B -> ImageB

renderGeometry geom cameraXfB = renderImage renderIO
 where
  renderIO mbColorB xfB t0 xt0 ts xts requestV replyV above = do
    -- Make the scene and camera
    --putStrLn ("rG: making scene from geometry" ++ show geom)
    sceneFrame  <- SL.newScene
    geomRequestV <- newMVar
    geomReplyV   <- newMVar
    --putStrLn "rG: fillFrame"
    fillFrame sceneFrame geom' ts geomRequestV geomReplyV
    cameraFrame  <- SL.newHFrame sceneFrame
    -- For now, use 1 for scale.  Fix later to use scaleB.  The problem is
    -- I don't know how to resize the D3DRM device with a tolerable
    -- efficiency.  DirectX 5 fixes this problem.
    renderer     <- SL.newRMRenderer sceneFrame cameraFrame 1
    -- Initialization: it's awkward to come up with an initial surface, so
    -- give a null surface.  Improve this stuff later.  Then I'll need to
    -- pass t0 around in fillFrameRec
    hSimpleSprite <- SL.newSimpleSprite SL.nullHDDSurface (-1) 1 1 1 above

    let update ~(t:ts') ~(cameraXf:cameraXfs')
               ~(S.Transform2 (S.Vector2XY dx dy) scale rotate : xfs') = do
          continue <- takeMVar requestV
          --putStrLn "geometry got request"
          -- Update D3D RM geometry
          putMVar geomRequestV continue
          if continue then do
            -- Set the camera frame
            S.hFrameSetTransform cameraFrame cameraXf
            -- Sync with the geometry update threads
            -- ## Why can't I say "True <- ..." here?  I get a complaint
            -- that IO isn't in MonadZero.
            _ <- takeMVar geomReplyV
            -- Do the rendering and return the surface
            newSurface <- SL.doRMRenderer renderer
            --putStrLn "renderer produced new surface"
            -- Bogus: using (-1,1) as upper left, because that's what the
            -- renderer does for now.
	    SL.updateSimpleSprite hSimpleSprite t newSurface (-1) 1 1 1
            putMVar replyV True
            update ts' cameraXfs' xfs'
           else do
            _ <- takeMVar geomReplyV
            putMVar replyV False
    forkIO $ update ts (cameraXfB `ats` xts) (xfB `ats` xts)
    return hSimpleSprite

  -- Throw in light.  I was coloring the geometry, but I realized it's
  -- bogus to do so.  I think that the proper, but not very useful,
  -- meaning would be to apply the color and use flat shading.
  geom' = sceneLight `unionG` geom

  sceneLight = withColorG (grey 0.4) ambientLightG `unionG`
               rotate3 yVector3 (pi/4) **% directionalLightG


--  fillFrame sceneFrame geom' ts geomRequestV geomReplyV


-- Construct D3DRM frames and update them

fillFrame :: SL.HFrame -> GeometryB -> [Time]
          -> SyncVar -> SyncVar -> IO ()

fillFrame parentFrame geomB = fillFrameRec parentFrame Nothing geomB


fillFrameRec :: SL.HFrame -> Maybe ColorB -> GeometryB
             -> [Time] -> SyncVar -> SyncVar -> IO ()

-- Code mostly copied from EmptyImage case of spritifyRec in Spritify.hs.
-- Improve abstraction for reuse.
fillFrameRec parentFrame _ EmptyGeometry =
  \ ts requestV replyV -> forwardSyncVars requestV replyV

fillFrameRec parentFrame mbColorB (MeshG builder) =
  coloredLeaf parentFrame mbColorB
              (\ frame -> --putstrLn "adding mesh builder" >>
                          SL.hFrameAddMeshBuilder frame builder)

fillFrameRec parentFrame mbColorB (LightG lightType) =
  \ ts requestV replyV -> do
  --putstrLn "adding light"
  hLight <- SL.newHLight parentFrame lightType
  -- No color.  Add the leaf and then just forward requests to replies.
  case mbColorB of
    Nothing -> forwardSyncVars requestV replyV
    Just colorB -> do
      let update ~(color:colors') = do
            continue <- takeMVar requestV
            if continue then do
              let (r,g,b) = S.colorRGBCoords color
              SL.hLightSetColor hLight (SL.createColorRGB r g b)
              putMVar replyV True
              update colors'
             else
              putMVar replyV False
      forkIO $ update (colorB `ats` ts)


{-
fillFrameRec parentFrame _ (SoundG sound) =
  -- For sound, simply delegate to displaySoundB.  Hey!  How do we get the
  -- sound to be stopped when appropriate?
  displaySoundB
-}

fillFrameRec parentFrame mbColorB@(Just _) (ColorG colB geom) =
  -- Being colored from outside, which paints over the coloring here.
  -- (Unconventional, but simpler functional semantics, in my opinion.)
  fillFrameRec parentFrame mbColorB geom

fillFrameRec parentFrame Nothing (ColorG colorB geom) =
  -- No color from the outside.  Just pass down the color
  fillFrameRec parentFrame (Just colorB) geom

fillFrameRec parentFrame mbColorB (TransformG xfB geomB) =
  \ ts requestV replyV -> do
  -- Make a new frame, which will be the parent for geomB.  The update
  -- action sets the frame's transform.  To do: introduce only one frame
  -- for multiple transforms.
  geomRequestV <- newMVar
  geomReplyV   <- newMVar
  -- Make a new frame so the color doesn't affect the siblings.
  --putstrLn "making new frame for transformed geometry"
  newFrame <- SL.newHFrame parentFrame
  fillFrameRec newFrame mbColorB geomB ts geomRequestV geomReplyV
  let update ~(xf:xfs') = do
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
  forkIO $ update (xfB `ats` ts)

  
fillFrameRec parentFrame mbColorB (geomB `UnionG` geomB') =
  \ ts requestV replyV -> do
  syncV  <- newMVar
  -- Add both parts to the frame.
  fillFrameRec parentFrame mbColorB geomB  ts requestV syncV
  fillFrameRec parentFrame mbColorB geomB' ts syncV    replyV

fillFrameRec parentFrame mbColorB (TimeTransG geomB tt) =
  \ ts -> fillFrameRec parentFrame mbColorB geomB (tt `ats` ts)



-- Add a possibly colored leaf in a given frame.
-- I thought I could use this guy for lights as well as meshes, but I was
-- wrong. 

coloredLeaf :: SL.HFrame -> Maybe ColorB -> (SL.HFrame -> IO ())
            -> [Time] -> SyncVar -> SyncVar -> IO ()

coloredLeaf parentFrame Nothing addLeaf =
  \ ts requestV replyV -> do
  -- No color, so no new frame.  Add the leaf and then just forward
  -- requests to replies.
  --putstrLn "coloredLeaf with no color"
  addLeaf parentFrame
  forwardSyncVars requestV replyV

coloredLeaf parentFrame (Just colorB) addLeaf =
  \ ts requestV replyV -> do
  -- Make a frame containing the leaf and repeatedly set the color in the
  -- frame.
  --putstrLn "coloredLeaf with color.  Making new frame."
  newFrame <- SL.newHFrame parentFrame
  addLeaf newFrame
  let update ~(color:colors') = do
        continue <- takeMVar requestV
        if continue then do
          let (r,g,b) = S.colorRGBCoords color
          SL.hFrameSetColor newFrame (SL.createColorRGB r g b)
          putMVar replyV True
          update colors'
         else
          putMVar replyV False
  forkIO $ update (colorB `ats` ts)
