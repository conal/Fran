-- "Geometry behavior" type defined directly, i.e., without Image or Behavior.

-- To do:
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
import Update
import IORef
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
   deriving Show

instance  Show SL.HMeshBuilder  where
  showsPrec p _ = showString "<mesh builder>"


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
  untilB    = UntilG
  afterTime = error "afterTime not yet implemented for GeometryB"
  startTime = error "start not yet implemented for GeometryB"

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

renderGeometry geom cameraXfB = syntheticImageIO render
 where
  render mbColorB scaleB =
    do -- Make the scene and camera
       -- putStrLn "rG: making scene"
       sceneFrame  <- SL.newScene
       -- putStrLn "rG: fillFrame"
       geomUpdates <- fillFrame sceneFrame geom'
       -- Prepare for updating
       geomUpdatesRef <- newRef geomUpdates
       -- scaleBRef      <- newRef scaleB
       cameraXfBRef   <- newRef cameraXfB
       cameraFrame    <- SL.newHFrame sceneFrame
       -- For now, use 1 for scale.  Later fix to use scaleBRef
       renderer       <- SL.newRMRenderer sceneFrame cameraFrame
                            (1 * screenPixelsPerLength)
       -- Return the surface generator / updater action
       -- putStrLn "rG: returning surface generator"
       return $ \ t t' ->
         -- updateBvrRefIO scaleBRef     t $ \ scale ->
         updateBvrRefIO cameraXfBRef t $ \ cameraXf ->
         do -- Do the updates and store new updates
            -- putStrLn "rG: doing geometry update trees"
            updateRefIO geomUpdatesRef (doUpdateTrees t t')
            -- Set the camera frame
            S.hFrameSetTransform cameraFrame cameraXf
            -- Now do the rendering and return the surface
            -- putStrLn "rG: calling renderGeometrySurf"
            SL.doRMRenderer renderer
    where
      -- Throw in light.  I was coloring the geometry, but I realized it's
      -- bogus to do so.  I think that the proper, but not very useful,
      -- meaning would be to apply the color and use flat shading.
      geom' = sceneLight `unionG` geom

      sceneLight = withColorG (grey 0.4) ambientLightG `unionG`
                   rotate3 yVector3 (pi/4) **% directionalLightG


-- Construct D3DRM frames in a top-down fashion

fillFrame :: SL.HFrame -> GeometryB -> IO [UpdateTree]

fillFrame parentFrame geomB =
  fillFrameRec parentFrame Nothing geomB []


fillFrameRec :: SL.HFrame -> Maybe (Ref SL.D3DColor) -> GeometryB
             -> TransformIO [UpdateTree]

fillFrameRec parentFrame mbColorRef EmptyGeometry = \ otherUpdates ->
  -- Return the given updates, without extension.
  return otherUpdates

fillFrameRec parentFrame _ (MeshG builder) = \ otherUpdates ->
  -- This one is very simple.  Just throw in the mesh builder.  There is
  -- nothing new to update
  do SL.hFrameAddMeshBuilder parentFrame builder
     return otherUpdates

fillFrameRec parentFrame mbColorRef (LightG lightType) = \ otherUpdates ->
  -- Add a new light to the frame
  do hLight    <- SL.newHLight parentFrame lightType
     -- Just add the light update to the others
     return (updateColorIO (SL.hLightSetColor hLight) mbColorRef
             : otherUpdates)


{-
fillFrameRec parentFrame _ (SoundG sound) =
  -- For sound, simply delegate to displaySoundB.  Hey!  How do we get the
  -- sound to be stopped when appropriate?
  displaySoundB
-}


fillFrameRec parentFrame mbColorRef@(Just _) (ColorG colB geom) =
  -- Being colored from outside, which paints over the coloring here.
  -- (Unconventional, but simpler functional semantics, in my opinion.)
  fillFrameRec parentFrame mbColorRef geom

fillFrameRec parentFrame Nothing (ColorG colorB geom) =  \ otherUpdates ->
  -- Okay, so we really get to set a color.
  do colorBRef   <- newRef colorB
     d3dColorRef <- newRef (error "d3dColorRef not yet set")
     -- Make a new frame so the color doesn't affect the siblings.
     newFrame <- SL.newHFrame parentFrame
     -- putStrLn "rG: Doing ColorG"
     let
         -- Update: get current color, and set the d3dColorRef
         update t t' = updateBvrRefIO colorBRef t $ \ color -> do
                    let (r,g,b) = S.colorRGBCoords color
                    -- putStrLn ("updating color to " ++ show (r,g,b))
                    setRef d3dColorRef (SL.createColorRGB r g b)

         mbColorRef = Just d3dColorRef
      in do
         -- Put component GeometryB in the new frame, but note that
         -- a color is being applied.  Provide augmented update list so
         -- that the new frame's color gets updated.  In this case, the
         -- color update has to come first, since the geom updates will
         -- depend on it having been done.  All of this hassle is because
         -- color is not inherited down the scene tree in D3DRM.
         geomUpdates <- fillFrameRec newFrame mbColorRef geom
                      (updateColorIO (SL.hFrameSetColor newFrame) mbColorRef
                       : otherUpdates)
         return (UpdateIO update : geomUpdates)

fillFrameRec parentFrame mbColorRef (TransformG xfB geomB) =
  \ otherUpdates ->
  -- Much like withColorG.  Make a new frame, which will be the parent for
  -- geomB.  The update action sets the frame's transform.  To do:
  -- introduce only one frame for multiple transforms.
  do xfBRef <- newRef xfB
     -- Make a new frame so the color doesn't affect the siblings.
     newFrame <- SL.newHFrame parentFrame
     let
         update t t' = updateBvrRefIO xfBRef t
                      (S.hFrameSetTransform newFrame)
      in
         fillFrameRec newFrame mbColorRef geomB
             (updateColorIO (SL.hFrameSetColor newFrame) mbColorRef
              : UpdateIO update : otherUpdates)
  
fillFrameRec parentFrame mbColorRef (geomB `UnionG` geomB') =
  -- Just add geomB and geomB' to parent frame
  fillFrameRec parentFrame mbColorRef geomB  .>>=
  fillFrameRec parentFrame mbColorRef geomB'

-- Catch-all

fillFrameRec _ _ geomB =
  error ("Sorry Geometry not handled: " ++ show geomB)

updateColorIO :: (SL.D3DColor -> IO ()) -> Maybe (Ref SL.D3DColor) -> UpdateTree

updateColorIO setter Nothing =
  -- UpdateIO (const (return ()))
  NoUpdate

updateColorIO setter (Just d3dColorRef) =
  UpdateIO (const $ const $
           --putStrLn "Updating with color" >>
             getRef d3dColorRef >>= setter
             -- return () 
             )
