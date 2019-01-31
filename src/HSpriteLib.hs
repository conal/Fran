-- GreenCard 2.0 Haskell/C interface file for Fran.   Gary Shu Ling
module HSpriteLib where
import StdDIS
import Win32 (HWND, HDC, DWORD, LONG, Word32, COLORREF, SIZE
             -- The rest are due to definitions that should be in Win32.
             , WindowMessage, LPCTSTR, WPARAM
             )
-- Open and Close SpriteLib
-- Argument: how many screen pixels correspond to one length unit
-- For improving the resolution of timeGetTime under NT.  Minimum of 5.
-- Goal period for vertical blank activities, in milliseconds.  Of course,
-- it should really be the vertical blank.  If negative, use
-- WaitForVerticalBlank, which is implemented very badly (by spinning!!) in
-- current DirectDraw (as of 4/97).  Goes with SetTimerResolutionMS above.
-- On NT, with a timer resolution of 10 ms, the following number will be
-- rounded up to a multiple of 10, or with a resolution of 5 (minimum),
-- then one more than the number will be rounded up to a multiple of 5.
-- Priority of the vblank handler thread.
type ThreadPriority = Int
type SpriteTime = Double
-- When a behavior is updated with SetGoal(goalTime,goalVal), should the
-- sprite engine interpolate from the *current* time and value, or the
-- previous goal time and value.  Ideally, they would be the same.  Default
-- False.
-- When a behavior is sampled past its end, should it continue sampling
-- its linear function (true) or stop (false)?  Default False.
-- GHC needs gcc, which cannot handle some of the .h files transitively
-- included by d3drmdefs.h, so instead, use DXSubst.h, which contains
-- copies of the minimum requirement.
--%#include "d3drmdefs.h"
type HDDSurface = Word32
-- Sometimes useful for newSimpleSprite, but should be eliminated
type HDSBuffer = Word32
type HMeshBuilder = Word32
type HLight = Word32
type HFrame = Word32
-- The size in pixels of a surface
-- To do: do consistent error-reporting
-- Make a surface from a .BMP file.
bitmapDDSurface :: String -> HDDSurface
bitmapDDSurface bmpName = unsafePerformIO $ newBitmapDDSurface bmpName
-- Make an sound buffer from a .WAV file
waveDSBuffer :: String -> HDSBuffer
waveDSBuffer fileName = unsafePerformIO $ newWaveDSBuffer fileName
-- Make a mesh builder from a .X mesh file
meshBuilder :: String -> HMeshBuilder
meshBuilder fileName = unsafePerformIO $ newMeshBuilder fileName
type D3DColor = DWORD
type LightType = Int
-- %fun renderGeometrySurf :: HFrame -> HFrame -> Double -> IO HDDSurface
-- A "renderer" of a 3D scene.  Current serious limitation: cannot change
-- the scale after creation.  To do: find a way to relax this restriction
-- with tolerable efficiency.
type HRMRenderer = Word32
--%{ h = newRMRenderer(arg1, arg2, arg3) %}
--%fail { h == 0 } { ErrorString("newRMRenderer") }
--%result (hRMRenderer h)
type HFlipBook = Word32
type Pixels = LONG
-- Arguments: surface, width and height, X,Y start pos on surface, 
-- number of columns, and rows of pages
-- Make a flip book given: surface, width and height, X,Y start pos on surface, 
-- number of columns, and rows of pages
flipBook :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels
         -> Int -> Int -> HFlipBook
flipBook surf width height srcXFirst srcYFirst columns rows =
  unsafePerformIO $
  newFlipBook surf width height srcXFirst srcYFirst columns rows
type HSpriteTree = Word32
type SpriteTreeChain = HSpriteTree
-- %fun paintAndFlip :: HSpriteTree -> HDDRawEnv -> SpriteTime -> IO ()
type HFlipSprite = Word32
-- Arguments flip book, cropRegion0, pos0, scale0, page0, rest
-- Arguments: flip sprite, time, crop, pos, scale, page
type HSimpleSprite = Word32
-- Arguments surface, ul, crop0, pos0, scale0, rest
type HMonochromeSprite = Word32
-- Arguments: crop, color and rest
type HSoundSprite = Word32
-- Arguments orig buffer, vol, pan, freq, rest
--%{ h = newSoundSprite(arg1, arg2, arg3, arg4, arg5) %}
--%fail { h == 0 } { ErrorString("newSoundSprite") }
--%result (hSoundSprite h)
-- Update methods go here (volume, frequency)
type HSpriteGroup = Word32
-- Arguments: elements, rest
-- Arguments: sprite group, elements, whether mutable
type HSpriteEngine = Word32
--%{ h = newSpriteEngine(arg1, arg2) %}
--%fail { h == 0 } { ErrorString("newSpriteEngine") }
--%result (hSpriteEngine h)
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
instance AkoSpriteTree Word32 where
  toSpriteTree = wordToSpriteTree
{-
-}
-- This stuff really belongs in Win32.  Remove from here when added there.
-- For setting the title bar text.  But inconvenient to make the LPCTSTR
-- More convenient version hiding the marshalling and freeing.  It appears
-- to be safe to free the cstring after SetWindowText.  Needs a name
-- convention.  Here I'm using "A" for "takes care of its own allocation".
setWindowTextA :: HWND -> String -> IO ()
setWindowTextA hwnd str = do
  cstring <- marshall_string_ str
  setWindowText hwnd cstring
  free cstring
-- WM_SIZE message wParam values
needPrims_hugs 2
openSpriteLib :: Double -> IO ()
openSpriteLib arg1 =
  prim_openSpriteLib arg1
primitive prim_openSpriteLib :: Double -> IO ()
closeSpriteLib :: IO ()
closeSpriteLib =
  prim_closeSpriteLib
primitive prim_closeSpriteLib :: IO ()
setTimerResolutionMS :: Int -> IO ()
setTimerResolutionMS arg1 =
  prim_setTimerResolutionMS arg1
primitive prim_setTimerResolutionMS :: Int -> IO ()
get_vblankPeriodMS :: IO Int
get_vblankPeriodMS =
  prim_get_vblankPeriodMS >>= \ (res1) ->
  (return (res1))
primitive prim_get_vblankPeriodMS :: IO (Int)
set_vblankPeriodMS :: Int -> IO ()
set_vblankPeriodMS arg1 =
  prim_set_vblankPeriodMS arg1
primitive prim_set_vblankPeriodMS :: Int -> IO ()
setVblankThreadPriority :: ThreadPriority -> IO ()
setVblankThreadPriority arg1 =
  prim_setVblankThreadPriority arg1
primitive prim_setVblankThreadPriority :: Int -> IO ()
currentSpriteTime :: IO SpriteTime
currentSpriteTime =
  prim_currentSpriteTime >>= \ (res1) ->
  (return (res1))
primitive prim_currentSpriteTime :: IO (Double)
get_behaviorMakeContinuous :: IO Bool
get_behaviorMakeContinuous =
  prim_get_behaviorMakeContinuous >>= \ (res1) ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
primitive prim_get_behaviorMakeContinuous :: IO (Int)
set_behaviorMakeContinuous :: Bool -> IO ()
set_behaviorMakeContinuous gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_set_behaviorMakeContinuous arg1
primitive prim_set_behaviorMakeContinuous :: Int -> IO ()
get_behaviorSamplePastGoal :: IO Bool
get_behaviorSamplePastGoal =
  prim_get_behaviorSamplePastGoal >>= \ (res1) ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
primitive prim_get_behaviorSamplePastGoal :: IO (Int)
set_behaviorSamplePastGoal :: Bool -> IO ()
set_behaviorSamplePastGoal gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_set_behaviorSamplePastGoal arg1
primitive prim_set_behaviorSamplePastGoal :: Int -> IO ()
get_ddhelpTimeTrace :: IO Bool
get_ddhelpTimeTrace =
  prim_get_ddhelpTimeTrace >>= \ (res1) ->
  (unmarshall_bool_ res1) >>= \ gc_res1 ->
  (return (gc_res1))
primitive prim_get_ddhelpTimeTrace :: IO (Int)
set_ddhelpTimeTrace :: Bool -> IO ()
set_ddhelpTimeTrace gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg1) ->
  prim_set_ddhelpTimeTrace arg1
primitive prim_set_ddhelpTimeTrace :: Int -> IO ()
get_g_pScratchSurf :: IO HDDSurface
get_g_pScratchSurf =
  prim_get_g_pScratchSurf >>= \ (res1) ->
  (return (res1))
primitive prim_get_g_pScratchSurf :: IO (Word32)
set_g_pScratchSurf :: HDDSurface -> IO ()
set_g_pScratchSurf arg1 =
  prim_set_g_pScratchSurf arg1
primitive prim_set_g_pScratchSurf :: Word32 -> IO ()
getDDrawHDC :: HDDSurface -> IO HDC
getDDrawHDC arg1 =
  prim_getDDrawHDC arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_getDDrawHDC :: Word32 -> IO (Addr)
releaseDDrawHDC :: HDDSurface -> HDC -> IO ()
releaseDDrawHDC arg1 arg2 =
  prim_releaseDDrawHDC arg1 arg2
primitive prim_releaseDDrawHDC :: Word32 -> Addr -> IO ()
clearDDSurface :: HDDSurface -> COLORREF -> IO ()
clearDDSurface arg1 arg2 =
  prim_clearDDSurface arg1 arg2
primitive prim_clearDDSurface :: Word32 -> Word32 -> IO ()
newPlainDDrawSurface :: Int -> Int -> COLORREF -> IO HDDSurface
newPlainDDrawSurface arg1 arg2 arg3 =
  prim_newPlainDDrawSurface arg1 arg2 arg3 >>= \ (res1) ->
  (return (res1))
primitive prim_newPlainDDrawSurface :: Int -> Int -> Word32 -> IO (Word32)
newBitmapDDSurface :: String -> IO HDDSurface
newBitmapDDSurface gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newBitmapDDSurface arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newBitmapDDSurface :: Addr -> IO (Word32,Int,Addr)
ddSurfaceSize :: HDDSurface -> SIZE
ddSurfaceSize arg1 =
  unsafePerformIO(
    prim_ddSurfaceSize arg1 >>= \ (gc_res2,gc_res4) ->
    (unmarshall_int32_ gc_res2) >>= \ gc_res1 ->
    (unmarshall_int32_ gc_res4) >>= \ gc_res3 ->
    (return ((gc_res1,gc_res3))))
primitive prim_ddSurfaceSize :: Word32 -> IO (Int,Int)
newWaveDSBuffer :: String -> IO HDSBuffer
newWaveDSBuffer gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newWaveDSBuffer arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newWaveDSBuffer :: Addr -> IO (Word32,Int,Addr)
newMeshBuilder :: String -> IO HMeshBuilder
newMeshBuilder gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newMeshBuilder arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newMeshBuilder :: Addr -> IO (Word32,Int,Addr)
d3dColorRGB :: Double -> Double -> Double -> D3DColor
d3dColorRGB arg1 arg2 arg3 =
  unsafePerformIO(
    prim_d3dColorRGB arg1 arg2 arg3 >>= \ (res1) ->
    (return (res1)))
primitive prim_d3dColorRGB :: Double -> Double -> Double -> IO (Word32)
newHLight :: HFrame -> LightType -> IO HLight
newHLight arg1 arg2 =
  prim_newHLight arg1 arg2 >>= \ (res1) ->
  (return (res1))
primitive prim_newHLight :: Word32 -> Int -> IO (Word32)
hLightSetColor :: HLight -> D3DColor -> IO ()
hLightSetColor arg1 arg2 =
  prim_hLightSetColor arg1 arg2
primitive prim_hLightSetColor :: Word32 -> Word32 -> IO ()
newHFrame :: HFrame -> IO HFrame
newHFrame arg1 =
  prim_newHFrame arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_newHFrame :: Word32 -> IO (Word32)
newScene :: IO HFrame
newScene =
  prim_newScene >>= \ (res1) ->
  (return (res1))
primitive prim_newScene :: IO (Word32)
deleteFrameContents :: HFrame -> IO ()
deleteFrameContents arg1 =
  prim_deleteFrameContents arg1
primitive prim_deleteFrameContents :: Word32 -> IO ()
hFrameAddMeshBuilder :: HFrame -> HMeshBuilder -> IO ()
hFrameAddMeshBuilder arg1 arg2 =
  prim_hFrameAddMeshBuilder arg1 arg2
primitive prim_hFrameAddMeshBuilder :: Word32 -> Word32 -> IO ()
hFrameSetColor :: HFrame -> D3DColor -> IO ()
hFrameSetColor arg1 arg2 =
  prim_hFrameSetColor arg1 arg2
primitive prim_hFrameSetColor :: Word32 -> Word32 -> IO ()
hFrameClearTransform :: HFrame -> IO ()
hFrameClearTransform arg1 =
  prim_hFrameClearTransform arg1
primitive prim_hFrameClearTransform :: Word32 -> IO ()
hFrameRotate :: HFrame -> Double -> Double -> Double -> Double -> IO ()
hFrameRotate arg1 arg2 arg3 arg4 arg5 =
  prim_hFrameRotate arg1 arg2 arg3 arg4 arg5
primitive prim_hFrameRotate :: Word32 -> Double -> Double -> Double -> Double -> IO ()
hFrameScale :: HFrame -> Double -> Double -> Double -> IO ()
hFrameScale arg1 arg2 arg3 arg4 =
  prim_hFrameScale arg1 arg2 arg3 arg4
primitive prim_hFrameScale :: Word32 -> Double -> Double -> Double -> IO ()
hFrameTranslate :: HFrame -> Double -> Double -> Double -> IO ()
hFrameTranslate arg1 arg2 arg3 arg4 =
  prim_hFrameTranslate arg1 arg2 arg3 arg4
primitive prim_hFrameTranslate :: Word32 -> Double -> Double -> Double -> IO ()
newRMRenderer :: HFrame -> HFrame -> Double -> IO HRMRenderer
newRMRenderer arg1 arg2 arg3 =
  prim_newRMRenderer arg1 arg2 arg3 >>= \ (res1) ->
  (return (res1))
primitive prim_newRMRenderer :: Word32 -> Word32 -> Double -> IO (Word32)
hRendererSetScale :: HRMRenderer -> Double -> IO ()
hRendererSetScale arg1 arg2 =
  prim_hRendererSetScale arg1 arg2
primitive prim_hRendererSetScale :: Word32 -> Double -> IO ()
doRMRenderer :: HRMRenderer -> IO HDDSurface
doRMRenderer arg1 =
  prim_doRMRenderer arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_doRMRenderer :: Word32 -> IO (Word32)
newFlipBook :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels -> Int -> Int -> IO HFlipBook
newFlipBook arg1 gc_arg1 gc_arg2 gc_arg3 gc_arg4 arg6 arg7 =
  (marshall_int32_ gc_arg1) >>= \ (arg2) ->
  (marshall_int32_ gc_arg2) >>= \ (arg3) ->
  (marshall_int32_ gc_arg3) >>= \ (arg4) ->
  (marshall_int32_ gc_arg4) >>= \ (arg5) ->
  prim_newFlipBook arg1 arg2 arg3 arg4 arg5 arg6 arg7 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newFlipBook :: Word32 -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Word32,Int,Addr)
flipBookSizePixels :: HFlipBook -> SIZE
flipBookSizePixels arg1 =
  unsafePerformIO(
    prim_flipBookSizePixels arg1 >>= \ (gc_res2,gc_res4) ->
    (unmarshall_int32_ gc_res2) >>= \ gc_res1 ->
    (unmarshall_int32_ gc_res4) >>= \ gc_res3 ->
    (return ((gc_res1,gc_res3))))
primitive prim_flipBookSizePixels :: Word32 -> IO (Int,Int)
flipBookPages :: HFlipBook -> Int
flipBookPages arg1 =
  unsafePerformIO(
    prim_flipBookPages arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_flipBookPages :: Word32 -> IO (Int)
deleteFlipBook :: HFlipBook -> IO ()
deleteFlipBook arg1 =
  prim_deleteFlipBook arg1
primitive prim_deleteFlipBook :: Word32 -> IO ()
deleteSpriteTree :: HSpriteTree -> IO ()
deleteSpriteTree arg1 =
  prim_deleteSpriteTree arg1
primitive prim_deleteSpriteTree :: Word32 -> IO ()
newFlipSprite :: HFlipBook -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HFlipSprite
newFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 =
  prim_newFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 >>= \ (res1) ->
  (return (res1))
primitive prim_newFlipSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Word32 -> IO (Word32)
flipSpriteToSpriteTree :: HFlipSprite -> HSpriteTree
flipSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_flipSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_flipSpriteToSpriteTree :: Word32 -> IO (Word32)
updateFlipSprite :: HFlipSprite -> SpriteTime -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 =
  prim_updateFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11
primitive prim_updateFlipSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
newSimpleSprite :: HDDSurface -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HSimpleSprite
newSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 =
  prim_newSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 >>= \ (res1) ->
  (return (res1))
primitive prim_newSimpleSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Word32 -> IO (Word32)
simpleSpriteToSpriteTree :: HSimpleSprite -> HSpriteTree
simpleSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_simpleSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_simpleSpriteToSpriteTree :: Word32 -> IO (Word32)
updateSimpleSprite :: HSimpleSprite -> SpriteTime -> HDDSurface -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 =
  prim_updateSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13
primitive prim_updateSimpleSprite :: Word32 -> Double -> Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
get_MinSpriteSize :: IO Int
get_MinSpriteSize =
  prim_get_MinSpriteSize >>= \ (res1) ->
  (return (res1))
primitive prim_get_MinSpriteSize :: IO (Int)
set_MinSpriteSize :: Int -> IO ()
set_MinSpriteSize arg1 =
  prim_set_MinSpriteSize arg1
primitive prim_set_MinSpriteSize :: Int -> IO ()
newMonochromeSprite :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HMonochromeSprite
newMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 =
  prim_newMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 >>= \ (res1) ->
  (return (res1))
primitive prim_newMonochromeSprite :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Word32 -> IO (Word32)
updateMonochromeSprite :: HSimpleSprite -> SpriteTime -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
updateMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
  prim_updateMonochromeSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
primitive prim_updateMonochromeSprite :: Word32 -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
newSoundSprite :: HDSBuffer -> Double -> Double -> Double -> SpriteTreeChain -> IO HSoundSprite
newSoundSprite arg1 arg2 arg3 arg4 arg5 =
  prim_newSoundSprite arg1 arg2 arg3 arg4 arg5 >>= \ (res1) ->
  (return (res1))
primitive prim_newSoundSprite :: Word32 -> Double -> Double -> Double -> Word32 -> IO (Word32)
soundSpriteToSpriteTree :: HSoundSprite -> HSpriteTree
soundSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_soundSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_soundSpriteToSpriteTree :: Word32 -> IO (Word32)
updateSoundSprite :: HSoundSprite -> SpriteTime -> Double -> Double -> Double -> IO ()
updateSoundSprite arg1 arg2 arg3 arg4 arg5 =
  prim_updateSoundSprite arg1 arg2 arg3 arg4 arg5
primitive prim_updateSoundSprite :: Word32 -> Double -> Double -> Double -> Double -> IO ()
newSpriteGroup :: SpriteTreeChain -> SpriteTreeChain -> IO HSpriteGroup
newSpriteGroup arg1 arg2 =
  prim_newSpriteGroup arg1 arg2 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newSpriteGroup :: Word32 -> Word32 -> IO (Word32,Int,Addr)
spriteGroupToSpriteTree :: HSpriteGroup -> HSpriteTree
spriteGroupToSpriteTree arg1 =
  unsafePerformIO(
    prim_spriteGroupToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_spriteGroupToSpriteTree :: Word32 -> IO (Word32)
resetSpriteGroup :: HSpriteGroup -> SpriteTreeChain -> Bool -> IO ()
resetSpriteGroup arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_resetSpriteGroup arg1 arg2 arg3
primitive prim_resetSpriteGroup :: Word32 -> Word32 -> Int -> IO ()
newSpriteEngine :: HWND -> HSpriteTree -> IO HSpriteEngine
newSpriteEngine arg1 arg2 =
  prim_newSpriteEngine arg1 arg2 >>= \ (res1) ->
  (return (res1))
primitive prim_newSpriteEngine :: Addr -> Word32 -> IO (Word32)
onResizeSpriteEngine :: HSpriteEngine -> IO ()
onResizeSpriteEngine arg1 =
  prim_onResizeSpriteEngine arg1
primitive prim_onResizeSpriteEngine :: Word32 -> IO ()
deleteSpriteEngine :: HSpriteEngine -> IO Int
deleteSpriteEngine arg1 =
  prim_deleteSpriteEngine arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_deleteSpriteEngine :: Word32 -> IO (Int)
wordToSpriteTree :: Word32 -> HSpriteTree
wordToSpriteTree arg1 =
  unsafePerformIO(
    prim_wordToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_wordToSpriteTree :: Word32 -> IO (Word32)
sleep :: DWORD -> IO ()
sleep arg1 =
  prim_sleep arg1
primitive prim_sleep :: Word32 -> IO ()
setWindowText :: HWND -> LPCTSTR -> IO ()
setWindowText arg1 arg2 =
  prim_setWindowText arg1 arg2 >>= \ (gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (()))
primitive prim_setWindowText :: Addr -> Addr -> IO (Int,Addr)
threadPriorityIdle :: ThreadPriority
threadPriorityIdle =
  unsafePerformIO(
    prim_threadPriorityIdle >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityIdle :: IO (Int)
threadPriorityLowest :: ThreadPriority
threadPriorityLowest =
  unsafePerformIO(
    prim_threadPriorityLowest >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityLowest :: IO (Int)
threadPriorityBelowNormal :: ThreadPriority
threadPriorityBelowNormal =
  unsafePerformIO(
    prim_threadPriorityBelowNormal >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityBelowNormal :: IO (Int)
threadPriorityNormal :: ThreadPriority
threadPriorityNormal =
  unsafePerformIO(
    prim_threadPriorityNormal >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityNormal :: IO (Int)
threadPriorityAboveNormal :: ThreadPriority
threadPriorityAboveNormal =
  unsafePerformIO(
    prim_threadPriorityAboveNormal >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityAboveNormal :: IO (Int)
threadPriorityHighest :: ThreadPriority
threadPriorityHighest =
  unsafePerformIO(
    prim_threadPriorityHighest >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityHighest :: IO (Int)
threadPriorityTimeCritical :: ThreadPriority
threadPriorityTimeCritical =
  unsafePerformIO(
    prim_threadPriorityTimeCritical >>= \ (res1) ->
    (return (res1)))
primitive prim_threadPriorityTimeCritical :: IO (Int)
nullHDDSurface :: HDDSurface
nullHDDSurface =
  unsafePerformIO(
    prim_nullHDDSurface >>= \ (res1) ->
    (return (res1)))
primitive prim_nullHDDSurface :: IO (Word32)
ambientLight :: LightType
ambientLight =
  unsafePerformIO(
    prim_ambientLight >>= \ (res1) ->
    (return (res1)))
primitive prim_ambientLight :: IO (Int)
pointLight :: LightType
pointLight =
  unsafePerformIO(
    prim_pointLight >>= \ (res1) ->
    (return (res1)))
primitive prim_pointLight :: IO (Int)
spotLight :: LightType
spotLight =
  unsafePerformIO(
    prim_spotLight >>= \ (res1) ->
    (return (res1)))
primitive prim_spotLight :: IO (Int)
directionalLight :: LightType
directionalLight =
  unsafePerformIO(
    prim_directionalLight >>= \ (res1) ->
    (return (res1)))
primitive prim_directionalLight :: IO (Int)
parallelPointLight :: LightType
parallelPointLight =
  unsafePerformIO(
    prim_parallelPointLight >>= \ (res1) ->
    (return (res1)))
primitive prim_parallelPointLight :: IO (Int)
emptySpriteTreeChain :: SpriteTreeChain
emptySpriteTreeChain =
  unsafePerformIO(
    prim_emptySpriteTreeChain >>= \ (res1) ->
    (return (res1)))
primitive prim_emptySpriteTreeChain :: IO (Word32)
wM_USER :: WindowMessage
wM_USER =
  unsafePerformIO(
    prim_wM_USER >>= \ (res1) ->
    (return (res1)))
primitive prim_wM_USER :: IO (Word32)
sIZE_RESTORED :: WPARAM
sIZE_RESTORED =
  unsafePerformIO(
    prim_sIZE_RESTORED >>= \ (res1) ->
    (return (res1)))
primitive prim_sIZE_RESTORED :: IO (Word32)
sIZE_MINIMIZED :: WPARAM
sIZE_MINIMIZED =
  unsafePerformIO(
    prim_sIZE_MINIMIZED >>= \ (res1) ->
    (return (res1)))
primitive prim_sIZE_MINIMIZED :: IO (Word32)
sIZE_MAXIMIZED :: WPARAM
sIZE_MAXIMIZED =
  unsafePerformIO(
    prim_sIZE_MAXIMIZED >>= \ (res1) ->
    (return (res1)))
primitive prim_sIZE_MAXIMIZED :: IO (Word32)
sIZE_MAXSHOW :: WPARAM
sIZE_MAXSHOW =
  unsafePerformIO(
    prim_sIZE_MAXSHOW >>= \ (res1) ->
    (return (res1)))
primitive prim_sIZE_MAXSHOW :: IO (Word32)
sIZE_MAXHIDE :: WPARAM
sIZE_MAXHIDE =
  unsafePerformIO(
    prim_sIZE_MAXHIDE >>= \ (res1) ->
    (return (res1)))
primitive prim_sIZE_MAXHIDE :: IO (Word32)