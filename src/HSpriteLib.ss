# Haskell/C interface for RBMH's underlying sprite engine.
# Notes:
# - primc was generating an importDLL for $file.so.  I changed it to $file.dll

# Standard HUGS imports - these come second so's we can overcome
# duplicate definitions. 
{%
#include "HUGS.h"
#include <windows.h>
%}

{{%
module HSpriteLib where
import Win32 (HWND, HDC, DWORD, LONG, Word32, rtsDummy, COLORREF, SIZE)
import IOExtensions (unsafePerformIO)
        
{- Hack!: to avoid making basic type syns. visible, we
   enclose them in comments - ToDo: figure out the best
   way of conditionally including .ss files, but not let
   them splat stuff into the .hs and/or .c files.
%}}

#
# Win* generic typedefs.
# 

%include StdTypes.ss
%include WinPlatform.ss
%include Handles.ss


# These defs extracted from Windows, instead of %include'ing it.

%newtype : COLORREF : Word32 : () : notypedef

%struct : SIZE : SIZE $0
    LONG : cx
    LONG : cy

{{%
-}
%}}


{%
#include "SpriteLib.h"
%}

%fun OpenSpriteLib  :: IO ()
%fun CloseSpriteLib :: IO ()

# For improving the resolution of timeGetTime under NT.  Minimum of 5.
%fun SetTimerResolutionMS :: Int -> IO ()


{%
#include "VBlankHandler.h"
%}


# Goal period for vertical blank activities, in milliseconds.  Of course,
# it should really be the vertical blank.  If negative, use
# WaitForVerticalBlank, which is implemented very badly (by spinning!!) in
# current DirectDraw (as of 4/97).  Goes with SetTimerResolutionMS above.
# On NT, with a timer resolution of 10 ms, the following number will be
# rounded up to a multiple of 10, or with a resolution of 5 (minimum),
# then one more than the number will be rounded up to a multiple of 5.
%var : vblankPeriodMS : Int : noVarDecl

# Priority of the vblank handler thread.  

%synonym : ThreadPriority : Int

%const ThreadPriority threadPriorityIdle = THREAD_PRIORITY_IDLE
%const ThreadPriority threadPriorityLowest = THREAD_PRIORITY_LOWEST
%const ThreadPriority threadPriorityBelowNormal = THREAD_PRIORITY_BELOW_NORMAL
%const ThreadPriority threadPriorityNormal = THREAD_PRIORITY_NORMAL
%const ThreadPriority threadPriorityAboveNormal = THREAD_PRIORITY_ABOVE_NORMAL
%const ThreadPriority threadPriorityHighest = THREAD_PRIORITY_HIGHEST
%const ThreadPriority threadPriorityTimeCritical = THREAD_PRIORITY_TIME_CRITICAL

%fun SetVblankThreadPriority :: ThreadPriority -> IO ()


{%
#include "Behavior.h"
%}
%synonym : SpriteTime : Double : notypedef

%fun CurrentSpriteTime :: IO SpriteTime

# When a behavior is updated with SetGoal(goalTime,goalVal), should the
# sprite engine interpolate from the *current* time and value, or the
# previous goal time and value.  Ideally, they would be the same.  Default
# False.
%var : behaviorMakeContinuous : Bool : noVarDecl

# When a behavior is sampled past its end, should it continue sampling
# its linear function (true) or stop (false)?  Default False.
%var : behaviorSamplePastGoal : Bool : noVarDecl


{%
#include "d3drm.h"  // for D3DRMLIGHTTYPE, and D3DCOLOR
#include "ddhelp.h"
%}

%var : ddhelpTimeTrace : Bool : noVarDecl

%newtype : HDDSurface : Word32 : () : notypedef
%constructor NullHDDSurface    :: IOError
%error : NullHDDSurface : HDDSurface $0 : !($0) : %constructor(NullHDDSurface);


%newtype : HDSBuffer : Word32 : () : notypedef
%constructor NullHDSBuffer    :: IOError
%error : NullHDSBuffer : HDSBuffer $0 : !($0) : %constructor(NullHDSBuffer);


%newtype : HMeshBuilder : Word32 : () : notypedef
%constructor NullHMeshBuilder    :: IOError
%error : NullHMeshBuilder : HMeshBuilder $0 : !($0) : %constructor(NullHMeshBuilder);

%newtype : HLight : Word32 : () : notypedef
# %constructor NullHLight    :: IOError
# %error : NullHLight : HLight $0 : !($0) : %constructor(NullHLight);

%newtype : HFrame : Word32 : () : notypedef
# %constructor NullHFrame    :: IOError
# %error : NullHFrame : HFrame $0 : !($0) : %constructor(NullHFrame);


%var : g_pScratchSurf : HDDSurface : noVarDecl

%fun GetDDrawHDC :: HDDSurface -> IO HDC
%fun ReleaseDDrawHDC :: HDDSurface -> HDC -> IO ()

%fun clearDDSurface :: HDDSurface -> COLORREF -> IO ()

%fun newPlainDDrawSurface :: Int -> Int -> COLORREF -> IO HDDSurface
%errfun NullHDDSurface newBitmapDDSurface :: String -> IO HDDSurface
# For testing, but phase out
%fun textDDSurface :: String -> COLORREF -> HDDSurface

%fun GetDDSurfaceSize :: HDDSurface -> SIZE

-- To do: do consistent error-reporting
{{%
bitmapDDSurface bmpName = unsafePerformIO $ newBitmapDDSurface bmpName
%}}

%errfun NullHDSBuffer newWaveDSBuffer :: String -> IO HDSBuffer
{{%
waveDSBuffer fileName = unsafePerformIO $ newWaveDSBuffer fileName
%}}

%errfun NullHMeshBuilder newMeshBuilder :: String -> IO HMeshBuilder
{{%
meshBuilder fileName = unsafePerformIO $ newMeshBuilder fileName
%}}

%synonym : D3DColor : DWORD

%fun CreateColorRGB :: Double -> Double -> Double -> D3DColor

%synonym : LightType : Int

%const LightType ambientLight = D3DRMLIGHT_AMBIENT
%const LightType pointLight = D3DRMLIGHT_POINT
%const LightType spotLight = D3DRMLIGHT_SPOT
%const LightType directionalLight = D3DRMLIGHT_DIRECTIONAL
%const LightType parallelPointLight = D3DRMLIGHT_PARALLELPOINT

%fun newHLight :: HFrame -> LightType -> IO HLight
%fun HLightSetColor :: HLight -> D3DColor -> IO ()

%fun newHFrame :: HFrame -> IO HFrame
%fun newScene :: IO HFrame
%fun HFrameAddMeshBuilder :: HFrame -> HMeshBuilder -> IO ()
%fun HFrameSetColor :: HFrame -> D3DColor -> IO ()

%fun HFrameClearTransform :: HFrame -> IO ()
%fun HFrameRotate     :: HFrame -> Double -> Double -> Double -> Double -> IO ()
%fun HFrameScale      :: HFrame -> Double -> Double -> Double -> IO ()
%fun HFrameTranslate  :: HFrame -> Double -> Double -> Double -> IO ()

%fun renderGeometrySurf :: HFrame -> HFrame -> Double -> IO HDDSurface 


# A "renderer" of a 3D scene.  Current serious limitation: can't change
# the scale after creation.  To do: find a way to relax this restriction
# with tolerable efficiency.

%newtype : HRMRenderer : Word32 : () : notypedef

%constructor NullHRMRenderer    :: IOError
%error : NullHRMRenderer : HRMRenderer $0 : !($0) : %constructor(NullHRMRenderer);

%fun newRMRenderer :: HFrame -> HFrame -> Double -> IO HRMRenderer

%fun doRMRenderer :: HRMRenderer -> IO HDDSurface


{%
#include "Sprite.h"
%}
%newtype : HFlipBook : Word32 : () : notypedef

%constructor NullHFlipBook    :: IOError
%error : NullHFlipBook : HFlipBook $0 : !($0) : %constructor(NullHFlipBook);

%synonym : Pixels : LONG

# Arguments: surface, width and height, X,Y start pos on surface, 
# number of columns, and rows of pages
%errfun NullHFlipBook newFlipBook :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels -> Int -> Int -> IO HFlipBook
%fun flipBookWidth  :: HFlipBook -> Int
%fun flipBookHeight :: HFlipBook -> Int
%fun flipBookPages  :: HFlipBook -> Int
%fun deleteFlipBook :: HFlipBook -> IO ()

{{%

flipBook surf width height srcXFirst srcYFirst columns rows =
  unsafePerformIO $
  newFlipBook surf width height srcXFirst srcYFirst columns rows

%}}

# Sprite trees may not be NULL, but sprite chains may.
%newtype : HSpriteTree : Word32 : () : notypedef

%synonym : SpriteTreeChain : HSpriteTree

%constructor NullHSpriteTree    :: IOError
%error : NullHSpriteTree : HSpriteTree $0 : !($0) : %constructor(NullHSpriteTree);


%const SpriteTreeChain emptySpriteTreeChain = 0

#%fun paintAndFlip :: HSpriteTree -> HDDrawEnv -> SpriteTime -> IO ()
%fun deleteSpriteTree :: HSpriteTree -> IO ()

%newtype : HSprite : Word32 : () : notypedef

# Arguments: sprite, posX, posY, goalTime
%fun setGoalPosition :: HSprite -> Double -> Double -> SpriteTime -> IO ()
%fun setGoalScale :: HSprite -> Double -> Double -> SpriteTime -> IO ()
%fun spriteToSpriteTree :: HSprite -> HSpriteTree


%newtype : HFlipSprite : Word32 : () : notypedef

%constructor NullHFlipSprite    :: IOError
%error : NullHFlipSprite : HFlipSprite $0 : !($0) : %constructor(NullHFlipSprite);

# Arguments flip book, posX0, posY0, scaleX0, scaleY0, page0, rest
%errfun NullHFlipSprite newFlipSprite :: HFlipBook -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HFlipSprite
%fun flipSpriteToSprite :: HFlipSprite -> HSprite
# Arguments: flip sprite, goal page, goal time
%fun setGoalPage :: HFlipSprite -> Double -> SpriteTime -> IO ()


%newtype : HSimpleSprite : Word32 : () : notypedef

%constructor NullHSimpleSprite    :: IOError
%error : NullHSimpleSprite : HSimpleSprite $0 : !($0) : %constructor(NullHSimpleSprite);

# Arguments surface, posX0, posY0, scaleX0, scaleY0, page0, rest
%errfun NullHSimpleSprite newSimpleSprite :: HDDSurface -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HSimpleSprite
%fun simpleSpriteToSprite :: HSimpleSprite -> HSprite
%fun setSurface :: HSimpleSprite -> HDDSurface -> IO ()

%var : MinSpriteSize : Int : noVarDecl


%newtype : HSoundSprite : Word32 : () : notypedef

%constructor NullHSoundSprite    :: IOError
%error : NullHSoundSprite : HSoundSprite $0 : !($0) : %constructor(NullHSoundSprite);

# Arguments orig buffer, vol, pan, freq, rest
%errfun NullHSoundSprite newSoundSprite :: HDSBuffer -> Double -> Double -> Double -> SpriteTreeChain -> IO HSoundSprite
%fun soundSpriteToSpriteTree :: HSoundSprite -> HSpriteTree
# Update methods go here (volume, frequency)
%fun updateSoundSprite :: HSoundSprite -> SpriteTime -> Double -> Double -> Double -> IO ()



%newtype : HSpriteGroup : Word32 : () : notypedef

%constructor NullHSpriteGroup    :: IOError
%error : NullHSpriteGroup : HSpriteGroup $0 : !($0) : %constructor(NullHSpriteGroup);

# Arguments: elements, rest
%errfun NullHSpriteGroup newSpriteGroup :: SpriteTreeChain -> SpriteTreeChain -> IO HSpriteGroup
%fun spriteGroupToSpriteTree :: HSpriteGroup -> HSpriteTree
# Arguments: sprite group, elements, whether mutable
%fun ResetSpriteGroup :: HSpriteGroup -> SpriteTreeChain -> Bool -> IO ()


{%
#include "SpriteEngine.h"
%}
%newtype : HSpriteEngine : Word32 : () : notypedef

%constructor NullHSpriteEngine    :: IOError
%error : NullHSpriteEngine : HSpriteEngine $0 : !($0) : %constructor(NullHSpriteEngine);

%errfun NullHSpriteEngine newSpriteEngine :: HWND -> HSpriteTree -> IO HSpriteEngine
%fun onResizeSpriteEngine    :: HSpriteEngine -> IO ()
%fun deleteSpriteEngine    :: HSpriteEngine -> IO Int

{{%
-- Supertype coercions

class  AkoSpriteTree a  where
  toSpriteTree :: a -> HSpriteTree

class  AkoSprite a  where
  toSprite :: a -> HSprite


instance  AkoSprite HFlipSprite  where
  toSprite = flipSpriteToSprite

instance  AkoSprite HSimpleSprite where
  toSprite = simpleSpriteToSprite

instance  AkoSpriteTree HSprite  where
  toSpriteTree = spriteToSpriteTree

instance  AkoSpriteTree HSpriteGroup  where
  toSpriteTree = spriteGroupToSpriteTree

instance  AkoSpriteTree HFlipSprite  where
  toSpriteTree = toSpriteTree . toSprite

instance  AkoSpriteTree HSimpleSprite  where
  toSpriteTree = toSpriteTree . toSprite

instance  AkoSpriteTree HSoundSprite  where
  toSpriteTree = soundSpriteToSpriteTree

%}}

