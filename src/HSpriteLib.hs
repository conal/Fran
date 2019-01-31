-- GreenCard 2.0 Haskell/C interface file for Fran.   Gary Shu Ling

module HSpriteLib where

import StdDIS
import Win32 (nullHANDLE, HWND, HDC, DWORD, UINT, LONG, Word32, COLORREF, SIZE
             -- The rest are due to definitions that should be in Win32.
             , WindowMessage, LPCTSTR, LPARAM, WPARAM
             )

-- Open and Close SpriteLib


-- Argument: how many screen pixels correspond to one length unit
primitive prim_openSpriteLib :: Double -> IO ()
openSpriteLib :: Double -> IO ()
openSpriteLib arg1 =
  prim_openSpriteLib arg1
primitive prim_closeSpriteLib :: IO ()
closeSpriteLib :: IO ()
closeSpriteLib =
  prim_closeSpriteLib

-- For improving the resolution of timeGetTime under NT.  Minimum of 5.

primitive prim_setTimerResolutionMS :: Int -> IO ()
setTimerResolutionMS :: Int -> IO ()
setTimerResolutionMS arg1 =
  prim_setTimerResolutionMS arg1

-- Goal period for vertical blank activities, in milliseconds.  Of course,
-- it should really be the vertical blank.  If negative, use
-- WaitForVerticalBlank, which is implemented very badly (by spinning!!) in
-- current DirectDraw (as of 4/97).  Goes with SetTimerResolutionMS above.
-- On NT, with a timer resolution of 10 ms, the following number will be
-- rounded up to a multiple of 10, or with a resolution of 5 (minimum),
-- then one more than the number will be rounded up to a multiple of 5.

primitive prim_get_vblankPeriodMS :: IO (Int)
get_vblankPeriodMS :: IO Int
get_vblankPeriodMS =
  prim_get_vblankPeriodMS >>= \ (res1) ->
  (return (res1))
primitive prim_set_vblankPeriodMS :: Int -> IO ()
set_vblankPeriodMS :: Int -> IO ()
set_vblankPeriodMS arg1 =
  prim_set_vblankPeriodMS arg1

-- Priority of the vblank handler thread.

type ThreadPriority = Int

primitive prim_threadPriorityIdle :: IO (Int)
threadPriorityIdle :: ThreadPriority
threadPriorityIdle =
  unsafePerformIO(
    prim_threadPriorityIdle >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityLowest :: IO (Int)
threadPriorityLowest :: ThreadPriority
threadPriorityLowest =
  unsafePerformIO(
    prim_threadPriorityLowest >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityBelowNormal :: IO (Int)
threadPriorityBelowNormal :: ThreadPriority
threadPriorityBelowNormal =
  unsafePerformIO(
    prim_threadPriorityBelowNormal >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityNormal :: IO (Int)
threadPriorityNormal :: ThreadPriority
threadPriorityNormal =
  unsafePerformIO(
    prim_threadPriorityNormal >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityAboveNormal :: IO (Int)
threadPriorityAboveNormal :: ThreadPriority
threadPriorityAboveNormal =
  unsafePerformIO(
    prim_threadPriorityAboveNormal >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityHighest :: IO (Int)
threadPriorityHighest :: ThreadPriority
threadPriorityHighest =
  unsafePerformIO(
    prim_threadPriorityHighest >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityTimeCritical :: IO (Int)
threadPriorityTimeCritical :: ThreadPriority
threadPriorityTimeCritical =
  unsafePerformIO(
    prim_threadPriorityTimeCritical >>= \ (res1) ->
    (return (res1)))

primitive prim_setVblankThreadPriority :: Int -> IO ()
setVblankThreadPriority :: ThreadPriority -> IO ()
setVblankThreadPriority arg1 =
  prim_setVblankThreadPriority arg1


type SpriteTime = Double

primitive prim_currentSpriteTime :: IO (Double)
currentSpriteTime :: IO SpriteTime
currentSpriteTime =
  prim_currentSpriteTime >>= \ (res1) ->
  (return (res1))

-- When a behavior is updated with SetGoal(goalTime,goalVal), should the
-- sprite engine interpolate from the *current* time and value, or the
-- previous goal time and value.  Ideally, they would be the same.  Default
-- False.

primitive prim_get_behaviorMakeContinuous :: IO (Int)
get_behaviorMakeContinuous :: IO Bool
get_behaviorMakeContinuous =
  prim_get_behaviorMakeContinuous >>= \ (res1) ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
primitive prim_set_behaviorMakeContinuous :: Int -> IO ()
set_behaviorMakeContinuous :: Bool -> IO ()
set_behaviorMakeContinuous gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_set_behaviorMakeContinuous arg1

-- When a behavior is sampled past its end, should it continue sampling
-- its linear function (true) or stop (false)?  Default False.

primitive prim_get_behaviorSamplePastGoal :: IO (Int)
get_behaviorSamplePastGoal :: IO Bool
get_behaviorSamplePastGoal =
  prim_get_behaviorSamplePastGoal >>= \ (res1) ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))

primitive prim_set_behaviorSamplePastGoal :: Int -> IO ()
set_behaviorSamplePastGoal :: Bool -> IO ()
set_behaviorSamplePastGoal gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_set_behaviorSamplePastGoal arg1

-- GHC needs gcc, which cannot handle some of the .h files transitively
-- included by d3drmdefs.h, so instead, use DXSubst.h, which contains
-- copies of the minimum requirement.
--%#include "d3drmdefs.h"

primitive prim_get_ddhelpTimeTrace :: IO (Int)
get_ddhelpTimeTrace :: IO Bool
get_ddhelpTimeTrace =
  prim_get_ddhelpTimeTrace >>= \ (res1) ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
primitive prim_set_ddhelpTimeTrace :: Int -> IO ()
set_ddhelpTimeTrace :: Bool -> IO ()
set_ddhelpTimeTrace gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_set_ddhelpTimeTrace arg1

type HDDSurface = Word32

-- Sometimes useful for newSimpleSprite, but should be eliminated
primitive prim_nullHDDSurface :: IO (Word32)
nullHDDSurface :: HDDSurface
nullHDDSurface =
  unsafePerformIO(
    prim_nullHDDSurface >>= \ (res1) ->
    (return (res1)))

type HDSBuffer = Word32

type HMeshBuilder = Word32

type HLight = Word32

type HFrame = Word32

primitive prim_get_g_pScratchSurf :: IO (Word32)
get_g_pScratchSurf :: IO HDDSurface
get_g_pScratchSurf =
  prim_get_g_pScratchSurf >>= \ (res1) ->
  (return (res1))
primitive prim_set_g_pScratchSurf :: Word32 -> IO ()
set_g_pScratchSurf :: HDDSurface -> IO ()
set_g_pScratchSurf arg1 =
  prim_set_g_pScratchSurf arg1

primitive prim_getDDrawHDC :: Word32 -> IO (Addr)
getDDrawHDC :: HDDSurface -> IO HDC
getDDrawHDC arg1 =
  prim_getDDrawHDC arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_releaseDDrawHDC :: Word32 -> Addr -> IO ()
releaseDDrawHDC :: HDDSurface -> HDC -> IO ()
releaseDDrawHDC arg1 arg2 =
  prim_releaseDDrawHDC arg1 arg2

primitive prim_clearDDSurface :: Word32 -> Word32 -> IO ()
clearDDSurface :: HDDSurface -> COLORREF -> IO ()
clearDDSurface arg1 arg2 =
  prim_clearDDSurface arg1 arg2

primitive prim_newPlainDDrawSurface :: Int -> Int -> Word32 -> IO (Word32)
newPlainDDrawSurface :: Int -> Int -> COLORREF -> IO HDDSurface
newPlainDDrawSurface arg1 arg2 arg3 =
  prim_newPlainDDrawSurface arg1 arg2 arg3 >>= \ (res1) ->
  (return (res1))

primitive prim_newBitmapDDSurface :: Addr -> IO (Word32,Int,Addr)
newBitmapDDSurface :: String -> IO HDDSurface
newBitmapDDSurface gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newBitmapDDSurface arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))

-- The size in pixels of a surface
primitive prim_ddSurfaceSize :: Word32 -> IO (Int,Int)
ddSurfaceSize :: HDDSurface -> SIZE
ddSurfaceSize arg1 =
  unsafePerformIO(
    prim_ddSurfaceSize arg1 >>= \ (gc_res2,gc_res4) ->
    let gc_res1 = ( intToInt32  gc_res2) in
    let gc_res3 = ( intToInt32  gc_res4) in
    (return ((gc_res1,gc_res3))))

-- To do: do consistent error-reporting

-- Make a surface from a .BMP file.
bitmapDDSurface :: String -> HDDSurface
bitmapDDSurface bmpName = unsafePerformIO $ newBitmapDDSurface bmpName

primitive prim_newWaveDSBuffer :: Addr -> IO (Word32,Int,Addr)
newWaveDSBuffer :: String -> IO HDSBuffer
newWaveDSBuffer gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newWaveDSBuffer arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))

-- Make an sound buffer from a .WAV file
waveDSBuffer :: String -> HDSBuffer
waveDSBuffer fileName = unsafePerformIO $ newWaveDSBuffer fileName

primitive prim_newMeshBuilder :: Addr -> IO (Word32,Int,Addr)
newMeshBuilder :: String -> IO HMeshBuilder
newMeshBuilder gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newMeshBuilder arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))

-- Make a mesh builder from a .X mesh file
meshBuilder :: String -> HMeshBuilder
meshBuilder fileName = unsafePerformIO $ newMeshBuilder fileName

type D3DColor = DWORD

primitive prim_d3dColorRGB :: Double -> Double -> Double -> IO (Word32)
d3dColorRGB :: Double -> Double -> Double -> D3DColor
d3dColorRGB arg1 arg2 arg3 =
  unsafePerformIO(
    prim_d3dColorRGB arg1 arg2 arg3 >>= \ (res1) ->
    (return (res1)))

type LightType = Int

primitive prim_ambientLight :: IO (Int)
ambientLight :: LightType
ambientLight =
  unsafePerformIO(
    prim_ambientLight >>= \ (res1) ->
    (return (res1)))
primitive prim_pointLight :: IO (Int)
pointLight :: LightType
pointLight =
  unsafePerformIO(
    prim_pointLight >>= \ (res1) ->
    (return (res1)))
primitive prim_spotLight :: IO (Int)
spotLight :: LightType
spotLight =
  unsafePerformIO(
    prim_spotLight >>= \ (res1) ->
    (return (res1)))
primitive prim_directionalLight :: IO (Int)
directionalLight :: LightType
directionalLight =
  unsafePerformIO(
    prim_directionalLight >>= \ (res1) ->
    (return (res1)))
primitive prim_parallelPointLight :: IO (Int)
parallelPointLight :: LightType
parallelPointLight =
  unsafePerformIO(
    prim_parallelPointLight >>= \ (res1) ->
    (return (res1)))

primitive prim_newHLight :: Word32 -> Int -> IO (Word32)
newHLight :: HFrame -> LightType -> IO HLight
newHLight arg1 arg2 =
  prim_newHLight arg1 arg2 >>= \ (res1) ->
  (return (res1))
primitive prim_hLightSetColor :: Word32 -> Word32 -> IO ()
hLightSetColor :: HLight -> D3DColor -> IO ()
hLightSetColor arg1 arg2 =
  prim_hLightSetColor arg1 arg2

primitive prim_newHFrame :: Word32 -> IO (Word32)
newHFrame :: HFrame -> IO HFrame
newHFrame arg1 =
  prim_newHFrame arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_newScene :: IO (Word32)
newScene :: IO HFrame
newScene =
  prim_newScene >>= \ (res1) ->
  (return (res1))
primitive prim_deleteFrameContents :: Word32 -> IO ()
deleteFrameContents :: HFrame -> IO ()
deleteFrameContents arg1 =
  prim_deleteFrameContents arg1

primitive prim_hFrameAddMeshBuilder :: Word32 -> Word32 -> IO ()
hFrameAddMeshBuilder :: HFrame -> HMeshBuilder -> IO ()
hFrameAddMeshBuilder arg1 arg2 =
  prim_hFrameAddMeshBuilder arg1 arg2
primitive prim_hFrameSetColor :: Word32 -> Word32 -> IO ()
hFrameSetColor :: HFrame -> D3DColor -> IO ()
hFrameSetColor arg1 arg2 =
  prim_hFrameSetColor arg1 arg2

primitive prim_hFrameClearTransform :: Word32 -> IO ()
hFrameClearTransform :: HFrame -> IO ()
hFrameClearTransform arg1 =
  prim_hFrameClearTransform arg1
primitive prim_hFrameRotate :: Word32 -> Double -> Double -> Double -> Double -> IO ()
hFrameRotate :: HFrame -> Double -> Double -> Double -> Double -> IO ()
hFrameRotate arg1 arg2 arg3 arg4 arg5 =
  prim_hFrameRotate arg1 arg2 arg3 arg4 arg5

primitive prim_hFrameScale :: Word32 -> Double -> Double -> Double -> IO ()
hFrameScale :: HFrame -> Double -> Double -> Double -> IO ()
hFrameScale arg1 arg2 arg3 arg4 =
  prim_hFrameScale arg1 arg2 arg3 arg4
primitive prim_hFrameTranslate :: Word32 -> Double -> Double -> Double -> IO ()
hFrameTranslate :: HFrame -> Double -> Double -> Double -> IO ()
hFrameTranslate arg1 arg2 arg3 arg4 =
  prim_hFrameTranslate arg1 arg2 arg3 arg4

-- %fun renderGeometrySurf :: HFrame -> HFrame -> Double -> IO HDDSurface

-- A "renderer" of a 3D scene.  Current serious limitation: cannot change
-- the scale after creation.  To do: find a way to relax this restriction
-- with tolerable efficiency.

type HRMRenderer = Word32

primitive prim_newRMRenderer :: Word32 -> Word32 -> Double -> Double -> IO (Word32)
newRMRenderer :: HFrame -> HFrame -> Double -> Double -> IO HRMRenderer
newRMRenderer arg1 arg2 arg3 arg4 =
  prim_newRMRenderer arg1 arg2 arg3 arg4 >>= \ (res1) ->
  (return (res1))
--%{ h = newRMRenderer(arg1, arg2, arg3, arg4) %}
--%fail { h == 0 } { ErrorString("newRMRenderer") }
--%result (hRMRenderer h)

primitive prim_hRendererSetScale :: Word32 -> Double -> IO ()
hRendererSetScale :: HRMRenderer -> Double -> IO ()
hRendererSetScale arg1 arg2 =
  prim_hRendererSetScale arg1 arg2

primitive prim_doRMRenderer :: Word32 -> IO (Word32)
doRMRenderer :: HRMRenderer -> IO HDDSurface
doRMRenderer arg1 =
  prim_doRMRenderer arg1 >>= \ (res1) ->
  (return (res1))


type HFlipBook = Word32

type Pixels = LONG

-- Arguments: surface, width and height, X,Y start pos on surface, 
-- number of columns, and rows of pages

primitive prim_newFlipBook :: Word32 -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Word32,Int,Addr)
newFlipBook :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels -> Int -> Int -> IO HFlipBook
newFlipBook arg1 gc_arg1 gc_arg2 gc_arg3 gc_arg4 arg6 arg7 =
  case ( int32ToInt  gc_arg1) of { arg2 ->
  case ( int32ToInt  gc_arg2) of { arg3 ->
  case ( int32ToInt  gc_arg3) of { arg4 ->
  case ( int32ToInt  gc_arg4) of { arg5 ->
  prim_newFlipBook arg1 arg2 arg3 arg4 arg5 arg6 arg7 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))}}}}

primitive prim_flipBookSizePixels :: Word32 -> IO (Int,Int)
flipBookSizePixels :: HFlipBook -> SIZE
flipBookSizePixels arg1 =
  unsafePerformIO(
    prim_flipBookSizePixels arg1 >>= \ (gc_res2,gc_res4) ->
    let gc_res1 = ( intToInt32  gc_res2) in
    let gc_res3 = ( intToInt32  gc_res4) in
    (return ((gc_res1,gc_res3))))

primitive prim_flipBookPages :: Word32 -> IO (Int)
flipBookPages :: HFlipBook -> Int
flipBookPages arg1 =
  unsafePerformIO(
    prim_flipBookPages arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_deleteFlipBook :: Word32 -> IO ()
deleteFlipBook :: HFlipBook -> IO ()
deleteFlipBook arg1 =
  prim_deleteFlipBook arg1


-- Make a flip book given: surface, width and height, X,Y start pos on surface, 
-- number of columns, and rows of pages
flipBook :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels
         -> Int -> Int -> HFlipBook
flipBook surf width height srcXFirst srcYFirst columns rows =
  unsafePerformIO $
  newFlipBook surf width height srcXFirst srcYFirst columns rows

type HSpriteTree = Word32

type SpriteTreeChain = HSpriteTree

primitive prim_emptySpriteTreeChain :: IO (Word32)
emptySpriteTreeChain :: SpriteTreeChain
emptySpriteTreeChain =
  unsafePerformIO(
    prim_emptySpriteTreeChain >>= \ (res1) ->
    (return (res1)))

-- %fun paintAndFlip :: HSpriteTree -> HDDRawEnv -> SpriteTime -> IO ()
primitive prim_deleteSpriteTree :: Word32 -> IO ()
deleteSpriteTree :: HSpriteTree -> IO ()
deleteSpriteTree arg1 =
  prim_deleteSpriteTree arg1


type HFlipSprite = Word32

-- Arguments flip book, cropRegion0, pos0, scale0, page0, rest

primitive prim_newFlipSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Word32 -> IO (Word32)
newFlipSprite :: HFlipBook -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HFlipSprite
newFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 =
  prim_newFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 >>= \ (res1) ->
  (return (res1))

primitive prim_flipSpriteToSpriteTree :: Word32 -> IO (Word32)
flipSpriteToSpriteTree :: HFlipSprite -> HSpriteTree
flipSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_flipSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))

-- Arguments: flip sprite, time, crop, pos, scale, page
primitive prim_updateFlipSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateFlipSprite :: HFlipSprite -> SpriteTime -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 =
  prim_updateFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11

type HSimpleSprite = Word32

-- Arguments surface, ul, crop0, pos0, scale0, rest

primitive prim_newSimpleSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Word32 -> IO (Word32)
newSimpleSprite :: HDDSurface -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HSimpleSprite
newSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 =
  prim_newSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 >>= \ (res1) ->
  (return (res1))

primitive prim_simpleSpriteToSpriteTree :: Word32 -> IO (Word32)
simpleSpriteToSpriteTree :: HSimpleSprite -> HSpriteTree
simpleSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_simpleSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))

primitive prim_updateSimpleSprite :: Word32 -> Double -> Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateSimpleSprite :: HSimpleSprite -> SpriteTime -> HDDSurface -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 =
  prim_updateSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13

primitive prim_get_MinSpriteSize :: IO (Int)
get_MinSpriteSize :: IO Int
get_MinSpriteSize =
  prim_get_MinSpriteSize >>= \ (res1) ->
  (return (res1))
primitive prim_set_MinSpriteSize :: Int -> IO ()
set_MinSpriteSize :: Int -> IO ()
set_MinSpriteSize arg1 =
  prim_set_MinSpriteSize arg1

type HMonochromeSprite = Word32

-- Arguments: crop, color and rest

primitive prim_newMonochromeSprite :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Word32 -> IO (Word32)
newMonochromeSprite :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HMonochromeSprite
newMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 =
  prim_newMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 >>= \ (res1) ->
  (return (res1))

primitive prim_updateMonochromeSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateMonochromeSprite :: HSimpleSprite -> SpriteTime -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_updateMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9


type HSoundSprite = Word32

-- Arguments orig buffer, vol, pan, freq, repeat, rest

primitive prim_newSoundSprite :: Word32 -> Double -> Double -> Double -> Int -> Word32 -> IO (Word32)
newSoundSprite :: HDSBuffer -> Double -> Double -> Double -> Bool -> SpriteTreeChain -> IO HSoundSprite
newSoundSprite arg1 arg2 arg3 arg4 gc_arg1 arg6 =
  (marshall_bool_ gc_arg1) >>= \ (arg5) ->
  prim_newSoundSprite arg1 arg2 arg3 arg4 arg5 arg6 >>= \ (res1) ->
  (return (res1))
--%{ h = newSoundSprite(arg1, arg2, arg3, arg4, arg5, arg6) %}
--%fail { h == 0 } { ErrorString("newSoundSprite") }
--%result (hSoundSprite h)

primitive prim_soundSpriteToSpriteTree :: Word32 -> IO (Word32)
soundSpriteToSpriteTree :: HSoundSprite -> HSpriteTree
soundSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_soundSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))

-- Update methods go here (volume, frequency)

primitive prim_updateSoundSprite :: Word32 -> Double -> Double -> Double -> Double -> IO ()
updateSoundSprite :: HSoundSprite -> SpriteTime -> Double -> Double -> Double -> IO ()
updateSoundSprite arg1 arg2 arg3 arg4 arg5 =
  prim_updateSoundSprite arg1 arg2 arg3 arg4 arg5


type HSpriteGroup = Word32

-- Arguments: elements, rest

primitive prim_newSpriteGroup :: Word32 -> Word32 -> IO (Word32,Int,Addr)
newSpriteGroup :: SpriteTreeChain -> SpriteTreeChain -> IO HSpriteGroup
newSpriteGroup arg1 arg2 =
  prim_newSpriteGroup arg1 arg2 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))

primitive prim_spriteGroupToSpriteTree :: Word32 -> IO (Word32)
spriteGroupToSpriteTree :: HSpriteGroup -> HSpriteTree
spriteGroupToSpriteTree arg1 =
  unsafePerformIO(
    prim_spriteGroupToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))

-- Arguments: sprite group, elements, whether mutable

primitive prim_resetSpriteGroup :: Word32 -> Word32 -> Int -> IO ()
resetSpriteGroup :: HSpriteGroup -> SpriteTreeChain -> Bool -> IO ()
resetSpriteGroup arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_resetSpriteGroup arg1 arg2 arg3


type HCondSpriteTree = Word32

-- Arguments: initial cond, then-trees, else-trees, rest

primitive prim_newCondSpriteTree :: Int -> Word32 -> Word32 -> Word32 -> IO (Word32,Int,Addr)
newCondSpriteTree :: Bool -> SpriteTreeChain -> SpriteTreeChain -> SpriteTreeChain -> IO HCondSpriteTree
newCondSpriteTree gc_arg1 arg2 arg3 arg4 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_newCondSpriteTree arg1 arg2 arg3 arg4 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))

primitive prim_updateCondSpriteTree :: Word32 -> Double -> Int -> IO ()
updateCondSpriteTree :: HCondSpriteTree -> SpriteTime -> Bool -> IO ()
updateCondSpriteTree arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_updateCondSpriteTree arg1 arg2 arg3

primitive prim_condSpriteTreeToSpriteTree :: Word32 -> IO (Word32)
condSpriteTreeToSpriteTree :: HSpriteGroup -> HSpriteTree
condSpriteTreeToSpriteTree arg1 =
  unsafePerformIO(
    prim_condSpriteTreeToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))




type HSpriteEngine = Word32

primitive prim_newSpriteEngine :: Addr -> Word32 -> IO (Word32)
newSpriteEngine :: HWND -> HSpriteTree -> IO HSpriteEngine
newSpriteEngine arg1 arg2 =
  prim_newSpriteEngine arg1 arg2 >>= \ (res1) ->
  (return (res1))
--%{ h = newSpriteEngine(arg1, arg2) %}
--%fail { h == 0 } { ErrorString("newSpriteEngine") }
--%result (hSpriteEngine h)

primitive prim_onResizeSpriteEngine :: Word32 -> IO ()
onResizeSpriteEngine :: HSpriteEngine -> IO ()
onResizeSpriteEngine arg1 =
  prim_onResizeSpriteEngine arg1
primitive prim_deleteSpriteEngine :: Word32 -> IO (Int)
deleteSpriteEngine :: HSpriteEngine -> IO Int
deleteSpriteEngine arg1 =
  prim_deleteSpriteEngine arg1 >>= \ (res1) ->
  (return (res1))

-- Supertype coercions

class  AkoSpriteTree a  where
  toSpriteTree :: a -> HSpriteTree


-- instance  AkoSprite HFlipSprite  where
--   toSprite = flipSpriteToSprite
-- 
-- instance  AkoSpriteTree HSpriteGroup  where
--   toSpriteTree = spriteGroupToSpriteTree
-- 
-- instance  AkoSpriteTree HFlipSprite  where
--   toSpriteTree = toSpriteTree . toSprite
-- 
-- instance  AkoSpriteTree HSimpleSprite  where
--   toSpriteTree = toSpriteTree . toSprite
-- 
-- instance  AkoSpriteTree HSoundSprite  where
--   toSpriteTree = soundSpriteToSpriteTree

-- GSL

primitive prim_wordToSpriteTree :: Word32 -> IO (Word32)
wordToSpriteTree :: Word32 -> HSpriteTree
wordToSpriteTree arg1 =
  unsafePerformIO(
    prim_wordToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))

instance AkoSpriteTree Word32 where
  toSpriteTree = wordToSpriteTree


-- Tablet support.  Doesn't really belong in SpriteLib, but simpler this
-- way.


type   HCTX       = Addr
type MbHCTX       = Maybe HCTX

primitive prim_testForTablet :: Addr -> IO (Int)
testForTablet :: HWND -> Bool
testForTablet arg1 =
  unsafePerformIO(
    prim_testForTablet arg1 >>= \ (res1) ->
    (unmarshall_bool_ res1) >>= \ gc_res1 ->
    (return (gc_res1)))

primitive prim_openTablet :: Addr -> IO (Addr)
openTablet :: HWND -> IO MbHCTX
openTablet arg1 =
  prim_openTablet arg1 >>= \ (res1) ->
  (if nullHANDLE == (res1)
   then return Nothing
   else (return ((Just res1)))) >>= \ gc_res1 ->
  (return (gc_res1))

-- This dis doesn't work, and it causes green-card to die silently :( !

-- %dis WTPKT x = dWORD x
-- type WTPKT   = DWORD

primitive prim_pK_CONTEXT :: IO (Word32)
pK_CONTEXT :: DWORD
pK_CONTEXT =
  unsafePerformIO(
    prim_pK_CONTEXT >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_STATUS :: IO (Word32)
pK_STATUS :: DWORD
pK_STATUS =
  unsafePerformIO(
    prim_pK_STATUS >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_TIME :: IO (Word32)
pK_TIME :: DWORD
pK_TIME =
  unsafePerformIO(
    prim_pK_TIME >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_CHANGED :: IO (Word32)
pK_CHANGED :: DWORD
pK_CHANGED =
  unsafePerformIO(
    prim_pK_CHANGED >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_SERIAL_NUMBER :: IO (Word32)
pK_SERIAL_NUMBER :: DWORD
pK_SERIAL_NUMBER =
  unsafePerformIO(
    prim_pK_SERIAL_NUMBER >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_CURSOR :: IO (Word32)
pK_CURSOR :: DWORD
pK_CURSOR =
  unsafePerformIO(
    prim_pK_CURSOR >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_BUTTONS :: IO (Word32)
pK_BUTTONS :: DWORD
pK_BUTTONS =
  unsafePerformIO(
    prim_pK_BUTTONS >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_X :: IO (Word32)
pK_X :: DWORD
pK_X =
  unsafePerformIO(
    prim_pK_X >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_Y :: IO (Word32)
pK_Y :: DWORD
pK_Y =
  unsafePerformIO(
    prim_pK_Y >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_Z :: IO (Word32)
pK_Z :: DWORD
pK_Z =
  unsafePerformIO(
    prim_pK_Z >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_NORMAL_PRESSURE :: IO (Word32)
pK_NORMAL_PRESSURE :: DWORD
pK_NORMAL_PRESSURE =
  unsafePerformIO(
    prim_pK_NORMAL_PRESSURE >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_TANGENT_PRESSURE :: IO (Word32)
pK_TANGENT_PRESSURE :: DWORD
pK_TANGENT_PRESSURE =
  unsafePerformIO(
    prim_pK_TANGENT_PRESSURE >>= \ (res1) ->
    (return (res1)))
primitive prim_pK_ORIENTATION :: IO (Word32)
pK_ORIENTATION :: DWORD
pK_ORIENTATION =
  unsafePerformIO(
    prim_pK_ORIENTATION >>= \ (res1) ->
    (return (res1)))


primitive prim_wT_PACKET :: IO (Word32)
wT_PACKET :: WindowMessage
wT_PACKET =
  unsafePerformIO(
    prim_wT_PACKET >>= \ (res1) ->
    (return (res1)))

type WTPacket =
  ( DWORD                               -- changed
  , DWORD                               -- buttons
  , UINT                                -- pressure
  , DWORD, DWORD                        -- X, Y
  )


-- See SimpleTablet.cpp comment abou XWT*
primitive prim_getWTPacket :: Int -> Word32 -> IO (Word32,Word32,Word32,Word32,Word32,Int,Addr)
getWTPacket :: LPARAM -> WPARAM -> IO WTPacket
getWTPacket gc_arg1 arg2 =
  case ( int32ToInt  gc_arg1) of { arg1 ->
  prim_getWTPacket arg1 arg2 >>= \ (gc_res1,gc_res2,gc_res3,gc_res4,gc_res5,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return ((gc_res1,gc_res2,gc_res3,gc_res4,gc_res5)))}

primitive prim_wTConfig :: Addr -> Addr -> IO (Int,Addr)
wTConfig :: HCTX -> HWND -> IO ()
wTConfig arg1 arg2 =
  prim_wTConfig arg1 arg2 >>= \ (gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (()))

primitive prim_wTClose :: Addr -> IO (Int,Addr)
wTClose :: HCTX -> IO ()
wTClose arg1 =
  prim_wTClose arg1 >>= \ (gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (()))
needPrims_hugs 2
