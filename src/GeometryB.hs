-- "Geometry behavior" type defined directly, i.e., without Geometry or Behavior.
-- 
-- Last modified Tue Oct 07 14:45:51 1997

-- To do:
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
-- Color and meshes.  Set the appropriate D3DRM property
-- 
-- TextureG


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

withColorG :: ColorB -> GeometryB -> GeometryB
withColorG = ColorG

-- Apply an animated texture.  Sorry -- not yet available.
-- texture :: ImageB -> GeometryB -> GeometryB
-- texture = TextureG

instance  GBehavior GeometryB  where
  untilB     = UntilG
  afterTimes = error "afterTime not yet implemented for GeometryB, sorry."

--transformG :: Transform3B -> GeometryB -> GeometryB
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
    fillFrame sceneFrame geom' t0 xt0 ts xts geomRequestV geomReplyV
    cameraFrame  <- SL.newHFrame sceneFrame
    -- For now, use 1 for scale.  Fix later to use scaleB.  The problem is
    -- I don't know how to resize the D3DRM device with a tolerable
    -- efficiency.  DirectX 5 fixes this problem.
    renderer     <- SL.newRMRenderer sceneFrame cameraFrame 1
    -- Initialization: it's awkward to come up with an initial surface, so
    -- give a null surface.  Improve this stuff later.  Then I'll need to
    -- pass t0 around in fillFrameRec
    hSimpleSprite <- SL.newSimpleSprite SL.nullHDDSurface ulX ulY 0 0 1 1 above

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
	    SL.updateSimpleSprite hSimpleSprite t newSurface ulX ulY dx dy 1 1
            putMVar replyV True
            update ts' cameraXfs' xfs'
           else do
            _ <- takeMVar geomReplyV
            putMVar replyV False
    forkIO $ update ts (cameraXfB `ats` xts) (xfB `ats` xts)
    return hSimpleSprite

  ulX = - round screenPixelsPerLength
  ulY = ulX
  -- Throw in light.  I was coloring the geometry, but I realized it's
  -- bogus to do so.  I think that the proper, but not very useful,
  -- meaning would be to apply the color and use flat shading.
  geom' = sceneLight `unionG` geom

  sceneLight = withColorG (grey 0.4) ambientLightG `unionG`
               rotate3 yVector3 (pi/4) **% directionalLightG



-- Construct D3DRM frames and update them

fillFrame :: SL.HFrame -> GeometryB -> Time -> Time -> [Time] -> [Time]
          -> SyncVar -> SyncVar -> IO ()

fillFrame parentFrame geomB = fillFrameRec parentFrame Nothing geomB


fillFrameRec :: SL.HFrame -> Maybe ColorB -> GeometryB
             -> Time -> Time -> [Time] -> [Time]
             -> SyncVar -> SyncVar -> IO ()

fillFrameRec _ _ EmptyGeometry = \ t0 xt0 ts xts -> forwardSyncVars

fillFrameRec parentFrame mbColorB (MeshG builder) =
  coloredLeaf parentFrame mbColorB
              (\ frame -> --putstrLn "adding mesh builder" >>
                          SL.hFrameAddMeshBuilder frame builder)

fillFrameRec parentFrame mbColorB (LightG lightType) =
  \ t0 xt0 ts xts requestV replyV -> do
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
      initColor colorB xt0 (SL.hLightSetColor hLight)
      forkIO $ update (colorB `ats` xts)


{-
fillFrameRec parentFrame _ (SoundG sound) t0 xt0 =
  -- For sound, simply delegate to displaySoundB.  Hey!  How do we get the
  -- sound to be stopped when appropriate?
  ...
-}

fillFrameRec parentFrame mbColorB@(Just _) (ColorG colB geom) =
  -- Being colored from outside, which paints over the coloring here.
  -- (Unconventional, but simpler functional semantics, in my opinion.)
  fillFrameRec parentFrame mbColorB geom

fillFrameRec parentFrame Nothing (ColorG colorB geom) =
  -- No color from the outside.  Just pass down the color
  fillFrameRec parentFrame (Just colorB) geom

fillFrameRec parentFrame mbColorB (TransformG xfB geomB) =
  \ t0 xt0 ts xts requestV replyV -> do
  -- Make a new frame, which will be the parent for geomB.  The update
  -- action sets the frame's transform.  To do: introduce only one frame
  -- for multiple transforms.
  geomRequestV <- newMVar
  geomReplyV   <- newMVar
  -- Make a new frame so the color doesn't affect the siblings.
  --putstrLn "making new frame for transformed geometry"
  newFrame <- SL.newHFrame parentFrame
  fillFrameRec newFrame mbColorB geomB t0 xt0 ts xts geomRequestV geomReplyV
  let xf0:_ = xfB `ats` [xt0]
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
  forkIO $ update (xfB `ats` xts)

  
fillFrameRec parentFrame mbColorB (geomB `UnionG` geomB') =
  \ t0 xt0 ts xts requestV replyV -> do
  syncV  <- newMVar
  -- Add both parts to the frame.
  fillFrameRec parentFrame mbColorB geomB  t0 xt0 ts xts requestV syncV
  fillFrameRec parentFrame mbColorB geomB' t0 xt0 ts xts syncV    replyV

fillFrameRec parentFrame mbColorB (TimeTransG geomB tt) = \ t0 xt0 ts xts ->
  fillFrameRec parentFrame mbColorB geomB
               t0 (head (tt `ats` [xt0])) ts (tt `ats` xts)

-- Try to factor better to reuse spritifyUntilB in Spritify.hs

fillFrameRec parentFrame mbColorB (geomB1 `UntilG` e) =
  \ t0 xt0 ts xts requestV replyV -> do
  -- Fork a new thread that watches e and passes requests on to bv1's
  -- threads, but terminates when e occurs.  At that point, use requestV
  -- for the new bv.  (Verify !!)
  requestV1 <- newMVar                 -- request for bv1
  replyV1   <- newMVar                 -- reply   for bv1
  fillFrameRec parentFrame mbColorB geomB1 t0 xt0 ts xts
                           requestV1 replyV1
  let monitor ~ts@(t:ts') ~xts@(xt:xts') ~(mbOcc:mbOccs') = do
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
              fillFrameRec parentFrame mbColorB' geomB2
                           te xte ts xts requestV replyV
  -- Avoid a space-time leak here: Don't hang onto the
  -- original color, motion and scale.  Let them age.
  forkIO $ monitor ts xts ((e `afterE` mbColorB) `occs` xts)


-- Add a possibly colored leaf in a given frame.
-- I thought I could use this guy for lights as well as meshes, but I was
-- wrong. 

coloredLeaf :: SL.HFrame -> Maybe ColorB -> (SL.HFrame -> IO ())
            -> Time -> Time -> [Time] -> [Time]
            -> SyncVar -> SyncVar -> IO ()

coloredLeaf parentFrame Nothing addLeaf =
  \ t0 xt0 ts xts requestV replyV -> do
  -- No color, so no new frame.  Add the leaf and then just forward
  -- requests to replies.
  --putstrLn "coloredLeaf with no color"
  addLeaf parentFrame
  forwardSyncVars requestV replyV

coloredLeaf parentFrame (Just colorB) addLeaf =
  \ t0 xt0 ts xts requestV replyV -> do
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
  initColor colorB xt0 (SL.hFrameSetColor newFrame)
  forkIO $ update (colorB `ats` xts)



-- Initialize color
initColor :: ColorB -> Time -> (SL.D3DColor -> IO ()) -> IO ()

initColor colorB xt0 initFun = initFun (SL.createColorRGB r0 g0 b0)
 where
   color0:_   = colorB `ats` [xt0]
   (r0,g0,b0) = S.colorRGBCoords color0
