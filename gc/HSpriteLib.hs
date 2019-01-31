-- This is the brand new Haskell/C interface file for Fran using
-- Green Card 2.0
-- Gary Shu Ling
module HSpriteLib where
import StdDIS
import Win32 (HWND, HDC, DWORD, LONG, Word32, COLORREF, SIZE)
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
type HDDSurface = Word32
-- Sometimes useful for newSimpleSprite, but should be eliminated
type HDSBuffer = Word32
type HMeshBuilder = Word32
type HLight = Word32
type HFrame = Word32
-- For testing, but phase out
-- To do: do consistent error-reporting
bitmapDDSurface bmpName = unsafePerformIO $ newBitmapDDSurface bmpName
waveDSBuffer fileName = unsafePerformIO $ newWaveDSBuffer fileName
meshBuilder fileName = unsafePerformIO $ newMeshBuilder fileName
type D3DColor = DWORD
type LightType = Int
-- %fun renderGeometrySurf :: HFrame -> HFrame -> Double -> IO HDDSurface
-- A "renderer" of a 3D scene.  Current serious limitation: cannot change
-- the scale after creation.  To do: find a way to relax this restriction
-- with tolerable efficiency.
type HRMRenderer = Word32
type HFlipBook = Word32
type Pixels = LONG
-- Arguments: surface, width and height, X,Y start pos on surface, 
-- number of columns, and rows of pages
-- %fun flipBookWidth :: HFlipBook -> Int
-- %fun flipBookHeight :: HFlipBook -> Int
flipBook surf width height srcXFirst srcYFirst columns rows =
  unsafePerformIO $
  newFlipBook surf width height srcXFirst srcYFirst columns rows
type HSpriteTree = Word32
type SpriteTreeChain = HSpriteTree
-- %fun paintAndFlip :: HSpriteTree -> HDDRawEnv -> SpriteTime -> IO ()
type HSprite = Word32
-- Arguments: sprite, posX, posY, goalTime
type HFlipSprite = Word32
-- Arguments flip book, posX0, posY0, scaleX0, scaleY0, page0, rest
-- Arguments: flip sprite, goal page, goal time
--%fun setGoalPage :: HFlipSprite -> Double -> SpriteTime -> IO ()
-- Arguments: flip sprite, time, ulX, ulY, scaleX, scaleY
type HSimpleSprite = Word32
-- Arguments surface, posX0, posY0, scaleX0, scaleY0, page0, rest
-- Arguments: simple sprite, goal time, new surface, ulX, ulY, scaleX, scaleY
type HSoundSprite = Word32
-- Arguments orig buffer, vol, pan, freq, rest
-- Update methods go here (volume, frequency)
type HSpriteGroup = Word32
-- Arguments: elements, rest
-- Arguments: sprite group, elements, whether mutable
type HSpriteEngine = Word32
-- Supertype coercions
class  AkoSpriteTree a  where
  toSpriteTree :: a -> HSpriteTree
class  AkoSprite a  where
  toSprite :: a -> HSprite
-- instance  AkoSprite HFlipSprite  where
--   toSprite = flipSpriteToSprite
-- 
-- instance  AkoSprite HSimpleSprite where
--   toSprite = simpleSpriteToSprite
-- 
-- instance  AkoSpriteTree HSprite  where
--   toSpriteTree = spriteToSpriteTree
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
instance AkoSpriteTree Word where
  toSpriteTree = wordToSpriteTree
instance AkoSprite     Word where
  toSprite     = wordToSprite
needPrims_hugs
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
primitive prim_get_g_pScratchSurf :: IO (Word)
set_g_pScratchSurf :: HDDSurface -> IO ()
set_g_pScratchSurf arg1 =
  prim_set_g_pScratchSurf arg1
primitive prim_set_g_pScratchSurf :: Word -> IO ()
getDDrawHDC :: HDDSurface -> IO HDC
getDDrawHDC arg1 =
  prim_getDDrawHDC arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_getDDrawHDC :: Word -> IO (Addr)
releaseDDrawHDC :: HDDSurface -> HDC -> IO ()
releaseDDrawHDC arg1 arg2 =
  prim_releaseDDrawHDC arg1 arg2
primitive prim_releaseDDrawHDC :: Word -> Addr -> IO ()
clearDDSurface :: HDDSurface -> COLORREF -> IO ()
clearDDSurface arg1 arg2 =
  prim_clearDDSurface arg1 arg2
primitive prim_clearDDSurface :: Word -> Word -> IO ()
newPlainDDrawSurface :: Int -> Int -> COLORREF -> IO HDDSurface
newPlainDDrawSurface arg1 arg2 arg3 =
  prim_newPlainDDrawSurface arg1 arg2 arg3 >>= \ (res1) ->
  (return (res1))
primitive prim_newPlainDDrawSurface :: Int -> Int -> Word -> IO (Word)
newBitmapDDSurface :: String -> IO HDDSurface
newBitmapDDSurface gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newBitmapDDSurface arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newBitmapDDSurface :: Addr -> IO (Word,Int,Addr)
textDDSurface :: String -> COLORREF -> HDDSurface
textDDSurface gc_arg1 arg2 =
  unsafePerformIO(
    (marshall_string_ gc_arg1) >>= \ (arg1) ->
    prim_textDDSurface arg1 arg2 >>= \ (res1) ->
    (return (res1)))
primitive prim_textDDSurface :: Addr -> Word -> IO (Word)
getDDSurfaceSize :: HDDSurface -> SIZE
getDDSurfaceSize arg1 =
  unsafePerformIO(
    prim_getDDSurfaceSize arg1)
primitive prim_getDDSurfaceSize :: Word -> IO (Int,Int)
newWaveDSBuffer :: String -> IO HDSBuffer
newWaveDSBuffer gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newWaveDSBuffer arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newWaveDSBuffer :: Addr -> IO (Word,Int,Addr)
newMeshBuilder :: String -> IO HMeshBuilder
newMeshBuilder gc_arg1 =
  (marshall_string_ gc_arg1) >>= \ (arg1) ->
  prim_newMeshBuilder arg1 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newMeshBuilder :: Addr -> IO (Word,Int,Addr)
createColorRGB :: Double -> Double -> Double -> D3DColor
createColorRGB arg1 arg2 arg3 =
  unsafePerformIO(
    prim_createColorRGB arg1 arg2 arg3 >>= \ (res1) ->
    (return (res1)))
primitive prim_createColorRGB :: Double -> Double -> Double -> IO (Word)
newHLight :: HFrame -> LightType -> IO HLight
newHLight arg1 arg2 =
  prim_newHLight arg1 arg2 >>= \ (res1) ->
  (return (res1))
primitive prim_newHLight :: Word -> Int -> IO (Word)
hLightSetColor :: HLight -> D3DColor -> IO ()
hLightSetColor arg1 arg2 =
  prim_hLightSetColor arg1 arg2
primitive prim_hLightSetColor :: Word -> Word -> IO ()
newHFrame :: HFrame -> IO HFrame
newHFrame arg1 =
  prim_newHFrame arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_newHFrame :: Word -> IO (Word)
newScene :: IO HFrame
newScene =
  prim_newScene >>= \ (res1) ->
  (return (res1))
primitive prim_newScene :: IO (Word)
hFrameAddMeshBuilder :: HFrame -> HMeshBuilder -> IO ()
hFrameAddMeshBuilder arg1 arg2 =
  prim_hFrameAddMeshBuilder arg1 arg2
primitive prim_hFrameAddMeshBuilder :: Word -> Word -> IO ()
hFrameSetColor :: HFrame -> D3DColor -> IO ()
hFrameSetColor arg1 arg2 =
  prim_hFrameSetColor arg1 arg2
primitive prim_hFrameSetColor :: Word -> Word -> IO ()
hFrameClearTransform :: HFrame -> IO ()
hFrameClearTransform arg1 =
  prim_hFrameClearTransform arg1
primitive prim_hFrameClearTransform :: Word -> IO ()
hFrameRotate :: HFrame -> Double -> Double -> Double -> Double -> IO ()
hFrameRotate arg1 arg2 arg3 arg4 arg5 =
  prim_hFrameRotate arg1 arg2 arg3 arg4 arg5
primitive prim_hFrameRotate :: Word -> Double -> Double -> Double -> Double -> IO ()
hFrameScale :: HFrame -> Double -> Double -> Double -> IO ()
hFrameScale arg1 arg2 arg3 arg4 =
  prim_hFrameScale arg1 arg2 arg3 arg4
primitive prim_hFrameScale :: Word -> Double -> Double -> Double -> IO ()
hFrameTranslate :: HFrame -> Double -> Double -> Double -> IO ()
hFrameTranslate arg1 arg2 arg3 arg4 =
  prim_hFrameTranslate arg1 arg2 arg3 arg4
primitive prim_hFrameTranslate :: Word -> Double -> Double -> Double -> IO ()
newRMRenderer :: HFrame -> HFrame -> Double -> IO HRMRenderer
newRMRenderer arg1 arg2 arg3 =
  prim_newRMRenderer arg1 arg2 arg3 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newRMRenderer :: Word -> Word -> Double -> IO (Word,Int,Addr)
doRMRenderer :: HRMRenderer -> IO HDDSurface
doRMRenderer arg1 =
  prim_doRMRenderer arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_doRMRenderer :: Word -> IO (Word)
newFlipBook :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels -> Int -> Int -> IO HFlipBook
newFlipBook arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  prim_newFlipBook arg1 arg2 arg3 arg4 arg5 arg6 arg7 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newFlipBook :: Word -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Word,Int,Addr)
flipBookSize :: HFlipBook -> SIZE
flipBookSize arg1 =
  unsafePerformIO(
    prim_flipBookSize arg1)
primitive prim_flipBookSize :: Word -> IO (Int,Int)
flipBookPages :: HFlipBook -> Int
flipBookPages arg1 =
  unsafePerformIO(
    prim_flipBookPages arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_flipBookPages :: Word -> IO (Int)
deleteFlipBook :: HFlipBook -> IO ()
deleteFlipBook arg1 =
  prim_deleteFlipBook arg1
primitive prim_deleteFlipBook :: Word -> IO ()
deleteSpriteTree :: HSpriteTree -> IO ()
deleteSpriteTree arg1 =
  prim_deleteSpriteTree arg1
primitive prim_deleteSpriteTree :: Word -> IO ()
setGoalUpperLeft :: HSprite -> Double -> Double -> SpriteTime -> IO ()
setGoalUpperLeft arg1 arg2 arg3 arg4 =
  prim_setGoalUpperLeft arg1 arg2 arg3 arg4
primitive prim_setGoalUpperLeft :: Word -> Double -> Double -> Double -> IO ()
setGoalScale :: HSprite -> Double -> Double -> SpriteTime -> IO ()
setGoalScale arg1 arg2 arg3 arg4 =
  prim_setGoalScale arg1 arg2 arg3 arg4
primitive prim_setGoalScale :: Word -> Double -> Double -> Double -> IO ()
spriteToSpriteTree :: HSprite -> HSpriteTree
spriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_spriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_spriteToSpriteTree :: Word -> IO (Word)
newFlipSprite :: HFlipBook -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HFlipSprite
newFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  prim_newFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newFlipSprite :: Word -> Double -> Double -> Double -> Double -> Double -> Word -> IO (Word,Int,Addr)
flipSpriteToSprite :: HFlipSprite -> HSprite
flipSpriteToSprite arg1 =
  unsafePerformIO(
    prim_flipSpriteToSprite arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_flipSpriteToSprite :: Word -> IO (Word)
updateFlipSprite :: HFlipSprite -> SpriteTime -> Double -> Double -> Double -> Double -> Double -> IO ()
updateFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  prim_updateFlipSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7
primitive prim_updateFlipSprite :: Word -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
newSimpleSprite :: HDDSurface -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HSimpleSprite
newSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 =
  prim_newSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newSimpleSprite :: Word -> Double -> Double -> Double -> Double -> Word -> IO (Word,Int,Addr)
simpleSpriteToSprite :: HSimpleSprite -> HSprite
simpleSpriteToSprite arg1 =
  unsafePerformIO(
    prim_simpleSpriteToSprite arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_simpleSpriteToSprite :: Word -> IO (Word)
updateSimpleSprite :: HSimpleSprite -> SpriteTime -> HDDSurface -> Double -> Double -> Double -> Double -> IO ()
updateSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
  prim_updateSimpleSprite arg1 arg2 arg3 arg4 arg5 arg6 arg7
primitive prim_updateSimpleSprite :: Word -> Double -> Word -> Double -> Double -> Double -> Double -> IO ()
get_MinSpriteSize :: IO Int
get_MinSpriteSize =
  prim_get_MinSpriteSize >>= \ (res1) ->
  (return (res1))
primitive prim_get_MinSpriteSize :: IO (Int)
set_MinSpriteSize :: Int -> IO ()
set_MinSpriteSize arg1 =
  prim_set_MinSpriteSize arg1
primitive prim_set_MinSpriteSize :: Int -> IO ()
newSoundSprite :: HDSBuffer -> Double -> Double -> Double -> SpriteTreeChain -> IO HSoundSprite
newSoundSprite arg1 arg2 arg3 arg4 arg5 =
  prim_newSoundSprite arg1 arg2 arg3 arg4 arg5 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newSoundSprite :: Word -> Double -> Double -> Double -> Word -> IO (Word,Int,Addr)
soundSpriteToSpriteTree :: HSoundSprite -> HSpriteTree
soundSpriteToSpriteTree arg1 =
  unsafePerformIO(
    prim_soundSpriteToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_soundSpriteToSpriteTree :: Word -> IO (Word)
updateSoundSprite :: HSoundSprite -> SpriteTime -> Double -> Double -> Double -> IO ()
updateSoundSprite arg1 arg2 arg3 arg4 arg5 =
  prim_updateSoundSprite arg1 arg2 arg3 arg4 arg5
primitive prim_updateSoundSprite :: Word -> Double -> Double -> Double -> Double -> IO ()
newSpriteGroup :: SpriteTreeChain -> SpriteTreeChain -> IO HSpriteGroup
newSpriteGroup arg1 arg2 =
  prim_newSpriteGroup arg1 arg2 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newSpriteGroup :: Word -> Word -> IO (Word,Int,Addr)
spriteGroupToSpriteTree :: HSpriteGroup -> HSpriteTree
spriteGroupToSpriteTree arg1 =
  unsafePerformIO(
    prim_spriteGroupToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_spriteGroupToSpriteTree :: Word -> IO (Word)
resetSpriteGroup :: HSpriteGroup -> SpriteTreeChain -> Bool -> IO ()
resetSpriteGroup arg1 arg2 gc_arg1 =
  (marshall_bool_ gc_arg1) >>= \ (arg3) ->
  prim_resetSpriteGroup arg1 arg2 arg3
primitive prim_resetSpriteGroup :: Word -> Word -> Int -> IO ()
newSpriteEngine :: HWND -> HSpriteTree -> IO HSpriteEngine
newSpriteEngine arg1 arg2 =
  prim_newSpriteEngine arg1 arg2 >>= \ (h,gc_failed,gc_failstring) ->
  if ( gc_failed /= 0)
  then unmarshall_string_ gc_failstring >>= fail . userError
  else (return (h))
primitive prim_newSpriteEngine :: Addr -> Word -> IO (Word,Int,Addr)
onResizeSpriteEngine :: HSpriteEngine -> IO ()
onResizeSpriteEngine arg1 =
  prim_onResizeSpriteEngine arg1
primitive prim_onResizeSpriteEngine :: Word -> IO ()
deleteSpriteEngine :: HSpriteEngine -> IO Int
deleteSpriteEngine arg1 =
  prim_deleteSpriteEngine arg1 >>= \ (res1) ->
  (return (res1))
primitive prim_deleteSpriteEngine :: Word -> IO (Int)
wordToSpriteTree :: Word -> HSpriteTree
wordToSpriteTree arg1 =
  unsafePerformIO(
    prim_wordToSpriteTree arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_wordToSpriteTree :: Word -> IO (Word)
wordToSprite :: Word -> HSprite
wordToSprite arg1 =
  unsafePerformIO(
    prim_wordToSprite arg1 >>= \ (res1) ->
    (return (res1)))
primitive prim_wordToSprite :: Word -> IO (Word)
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
primitive prim_nullHDDSurface :: IO (Word)
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
primitive prim_emptySpriteTreeChain :: IO (Word)