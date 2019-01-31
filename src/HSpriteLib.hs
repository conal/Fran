module HSpriteLib where
import Win32 (HWND, HDC, DWORD, LONG, Word32, rtsDummy, COLORREF, SIZE)
import IOExtensions (unsafePerformIO)

{- Hack!: to avoid making basic type syns. visible, we
   enclose them in comments - ToDo: figure out the best
   way of conditionally including .ss files, but not let
   them splat stuff into the .hs and/or .c files.
type CHAR = Char
type PString = String -- "persistent" string needs to be explicitly freed
-- Note: we do not consider NULL to be a valid pointer value.
-- Anything that requires a NULL pointer of type "T" should be
-- modified to require a "Maybe T" and an appropriate "Maybe"
-- declaration defined.
newtype Ptr = Ptr Int deriving (Eq)

unPtr :: Ptr -> Int
unPtr (Ptr i) = i

plusPtr  :: Ptr -> Int -> Ptr
plusPtr  p x = Ptr (unPtr p + x)

minusPtr :: Ptr -> Ptr -> Int
minusPtr p q = unPtr p - unPtr q
-- This is used in restricted type synonyms where there are no
-- Haskell functions which need to "look inside the representation".
-- It overcomes a (normally reasonable) restriction that the type be
-- "open" in a non-empty list of variables.
rtsDummy = error "rtsDummy"
type Word8 = Int
type Word16 = Int
type Word32 = Int
type Word = Int
type Int8 = Int
type Int16 = Int
type Int32 = Int
type Int64 = Int
type Word64 = Int
type FLOAT = Float
type BYTE = Word8
type USHORT = Word16
type UINT = Word32
type INT = Int32
type WORD = Word16
type DWORD = Word32
type LONG = Int32
type ATOM = UINT
type WPARAM = UINT
type LPARAM = LONG
type LRESULT = LONG
type LPVOID = Ptr
type LPBYTE = Ptr
type MbString = Maybe String
type MbINT = Maybe INT
type MbFLOAT = Maybe FLOAT
type MbLPVOID = Maybe LPVOID
type MbDWORD = Maybe DWORD
type ListINT   = [INT]
type ListFLOAT = [FLOAT]
newtype NearPtr = NearPtr Ptr deriving (Eq)
newtype FarPtr  = FarPtr  Ptr deriving (Eq)
newtype HACCEL = HACCEL NearPtr deriving (Eq)
newtype HBITMAP = HBITMAP NearPtr deriving (Eq)
newtype HBRUSH = HBRUSH NearPtr deriving (Eq)
newtype HCURSOR = HCURSOR NearPtr deriving (Eq)
newtype HDC = HDC NearPtr deriving (Eq)
newtype HDRVR = HDRVR NearPtr deriving (Eq)
newtype HDWP = HDWP NearPtr deriving (Eq)
newtype HFONT = HFONT NearPtr deriving (Eq)
newtype HGDIOBJ = HGDIOBJ NearPtr deriving (Eq)
newtype HMENU = HMENU NearPtr deriving (Eq)
newtype HMETAFILE = HMETAFILE NearPtr deriving (Eq)
newtype HPALETTE = HPALETTE NearPtr deriving (Eq)
newtype HPEN = HPEN NearPtr deriving (Eq)
newtype HRGN = HRGN NearPtr deriving (Eq)
newtype HRSRC = HRSRC NearPtr deriving (Eq)
newtype HSTR = HSTR NearPtr deriving (Eq)
newtype HTASK = HTASK NearPtr deriving (Eq)
newtype HWND = HWND NearPtr deriving (Eq)
newtype HICON = HICON NearPtr deriving (Eq)
newtype HINSTANCE = HINSTANCE NearPtr deriving (Eq)
newtype HANDLE = HANDLE NearPtr deriving (Eq)
type HMODULE = HINSTANCE
type MbHDC = Maybe HDC
type MbHWND = Maybe HWND
type MbHMENU = Maybe HMENU
type MbHINSTANCE = Maybe HINSTANCE
type MbHMODULE = Maybe HMODULE
type MbHFONT = Maybe HFONT
type MbHPEN = Maybe HPEN
type MbHPALETTE = Maybe HPALETTE
type MbHICON = Maybe HICON
type MbHCURSOR = Maybe HCURSOR
type MbHBRUSH = Maybe HBRUSH
type MbHBITMAP = Maybe HBITMAP
type MbHRGN = Maybe HRGN
newtype COLORREF = COLORREF Word32 deriving ()
type SIZE = (LONG,LONG)
-}
primitive openSpriteLib "XS_OpenSpriteLib" :: IO ()
primitive closeSpriteLib "XS_CloseSpriteLib" :: IO ()
primitive setTimerResolutionMS "XS_SetTimerResolutionMS" :: Int -> IO ()
primitive get_vblankPeriodMS "XSget_vblankPeriodMS" :: IO Int
primitive set_vblankPeriodMS "XSset_vblankPeriodMS" :: Int -> IO ()
type ThreadPriority = Int
threadPriorityIdle :: ThreadPriority
threadPriorityIdle = consts_ThreadPriority 0
threadPriorityLowest :: ThreadPriority
threadPriorityLowest = consts_ThreadPriority 1
threadPriorityBelowNormal :: ThreadPriority
threadPriorityBelowNormal = consts_ThreadPriority 2
threadPriorityNormal :: ThreadPriority
threadPriorityNormal = consts_ThreadPriority 3
threadPriorityAboveNormal :: ThreadPriority
threadPriorityAboveNormal = consts_ThreadPriority 4
threadPriorityHighest :: ThreadPriority
threadPriorityHighest = consts_ThreadPriority 5
threadPriorityTimeCritical :: ThreadPriority
threadPriorityTimeCritical = consts_ThreadPriority 6
primitive setVblankThreadPriority "XS_SetVblankThreadPriority" :: ThreadPriority -> IO ()
type SpriteTime = Double
primitive currentSpriteTime "XS_CurrentSpriteTime" :: IO SpriteTime
primitive get_behaviorMakeContinuous "XSget_behaviorMakeContinuous" :: IO Bool
primitive set_behaviorMakeContinuous "XSset_behaviorMakeContinuous" :: Bool -> IO ()
primitive get_behaviorSamplePastGoal "XSget_behaviorSamplePastGoal" :: IO Bool
primitive set_behaviorSamplePastGoal "XSset_behaviorSamplePastGoal" :: Bool -> IO ()
primitive get_ddhelpTimeTrace "XSget_ddhelpTimeTrace" :: IO Bool
primitive set_ddhelpTimeTrace "XSset_ddhelpTimeTrace" :: Bool -> IO ()
newtype HDDSurface = HDDSurface Word32 deriving ()
primitive isNullHDDSurface "XStest_isNullHDDSurface" :: IOError -> Bool
newtype HDSBuffer = HDSBuffer Word32 deriving ()
primitive isNullHDSBuffer "XStest_isNullHDSBuffer" :: IOError -> Bool
newtype HMeshBuilder = HMeshBuilder Word32 deriving ()
primitive isNullHMeshBuilder "XStest_isNullHMeshBuilder" :: IOError -> Bool
newtype HLight = HLight Word32 deriving ()
newtype HFrame = HFrame Word32 deriving ()
primitive get_g_pScratchSurf "XSget_g_pScratchSurf" :: IO HDDSurface
primitive set_g_pScratchSurf "XSset_g_pScratchSurf" :: HDDSurface -> IO ()
primitive getDDrawHDC "XS_GetDDrawHDC" :: HDDSurface -> IO HDC
primitive releaseDDrawHDC "XS_ReleaseDDrawHDC" :: HDDSurface -> HDC -> IO ()
primitive clearDDSurface "XS_clearDDSurface" :: HDDSurface -> COLORREF -> IO ()
primitive newPlainDDrawSurface "XS_newPlainDDrawSurface" :: Int -> Int -> COLORREF -> IO HDDSurface
primitive getDDSurfaceSize "XS_GetDDSurfaceSize" :: HDDSurface -> SIZE
-- Try loading a media file
tryLoadingDX :: (String -> IO a) -> (String -> a)
tryLoadingDX loader fileName = unsafePerformIO $
 loader fileName `catch` \_ ->
   return (error ("Could not open " ++ fileName))
primitive newBitmapDDSurface "XS_newBitmapDDSurface" :: String -> IO HDDSurface
bitmapDDSurface = tryLoadingDX newBitmapDDSurface
primitive newWaveDSBuffer "XS_newWaveDSBuffer" :: String -> IO HDSBuffer
waveDSBuffer = tryLoadingDX newWaveDSBuffer
primitive newMeshBuilder "XS_newMeshBuilder" :: String -> IO HMeshBuilder
meshBuilder = tryLoadingDX newMeshBuilder
type D3DColor = DWORD
primitive createColorRGB "XS_CreateColorRGB" :: Double -> Double -> Double -> D3DColor
type LightType = Int
ambientLight :: LightType
ambientLight = consts_LightType 0
pointLight :: LightType
pointLight = consts_LightType 1
spotLight :: LightType
spotLight = consts_LightType 2
directionalLight :: LightType
directionalLight = consts_LightType 3
parallelPointLight :: LightType
parallelPointLight = consts_LightType 4
primitive newHLight "XS_newHLight" :: HFrame -> LightType -> IO HLight
primitive hLightSetColor "XS_HLightSetColor" :: HLight -> D3DColor -> IO ()
primitive newHFrame "XS_newHFrame" :: HFrame -> IO HFrame
primitive newScene "XS_newScene" :: IO HFrame
primitive hFrameAddMeshBuilder "XS_HFrameAddMeshBuilder" :: HFrame -> HMeshBuilder -> IO ()
primitive hFrameSetColor "XS_HFrameSetColor" :: HFrame -> D3DColor -> IO ()
primitive hFrameClearTransform "XS_HFrameClearTransform" :: HFrame -> IO ()
primitive hFrameRotate "XS_HFrameRotate" :: HFrame -> Double -> Double -> Double -> Double -> IO ()
primitive hFrameScale "XS_HFrameScale" :: HFrame -> Double -> Double -> Double -> IO ()
primitive hFrameTranslate "XS_HFrameTranslate" :: HFrame -> Double -> Double -> Double -> IO ()
primitive renderGeometrySurf "XS_renderGeometrySurf" :: HFrame -> HFrame -> Double -> IO HDDSurface
newtype HRMRenderer = HRMRenderer Word32 deriving ()
primitive isNullHRMRenderer "XStest_isNullHRMRenderer" :: IOError -> Bool
primitive newRMRenderer "XS_newRMRenderer" :: HFrame -> HFrame -> Double -> IO HRMRenderer
primitive doRMRenderer "XS_doRMRenderer" :: HRMRenderer -> IO HDDSurface
newtype HFlipBook = HFlipBook Word32 deriving ()
primitive isNullHFlipBook "XStest_isNullHFlipBook" :: IOError -> Bool
type Pixels = LONG
primitive newFlipBook "XS_newFlipBook" :: HDDSurface -> Pixels -> Pixels -> Pixels -> Pixels -> Int -> Int -> IO HFlipBook
primitive flipBookWidth "XS_flipBookWidth" :: HFlipBook -> Int
primitive flipBookHeight "XS_flipBookHeight" :: HFlipBook -> Int
primitive flipBookPages "XS_flipBookPages" :: HFlipBook -> Int
primitive deleteFlipBook "XS_deleteFlipBook" :: HFlipBook -> IO ()

flipBook surf width height srcXFirst srcYFirst columns rows =
  unsafePerformIO $
  newFlipBook surf width height srcXFirst srcYFirst columns rows

newtype HSpriteTree = HSpriteTree Word32 deriving ()
type SpriteTreeChain = HSpriteTree
primitive isNullHSpriteTree "XStest_isNullHSpriteTree" :: IOError -> Bool
emptySpriteTreeChain :: SpriteTreeChain
emptySpriteTreeChain = consts_SpriteTreeChain 0
primitive deleteSpriteTree "XS_deleteSpriteTree" :: HSpriteTree -> IO ()
newtype HSprite = HSprite Word32 deriving ()
primitive setGoalPosition "XS_setGoalPosition" :: HSprite -> Double -> Double -> SpriteTime -> IO ()
primitive setGoalScale "XS_setGoalScale" :: HSprite -> Double -> Double -> SpriteTime -> IO ()
primitive spriteToSpriteTree "XS_spriteToSpriteTree" :: HSprite -> HSpriteTree
newtype HFlipSprite = HFlipSprite Word32 deriving ()
primitive isNullHFlipSprite "XStest_isNullHFlipSprite" :: IOError -> Bool
primitive newFlipSprite "XS_newFlipSprite" :: HFlipBook -> Double -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HFlipSprite
primitive flipSpriteToSprite "XS_flipSpriteToSprite" :: HFlipSprite -> HSprite
primitive setGoalPage "XS_setGoalPage" :: HFlipSprite -> Double -> SpriteTime -> IO ()
newtype HSimpleSprite = HSimpleSprite Word32 deriving ()
primitive isNullHSimpleSprite "XStest_isNullHSimpleSprite" :: IOError -> Bool
primitive newSimpleSprite "XS_newSimpleSprite" :: HDDSurface -> Double -> Double -> Double -> Double -> SpriteTreeChain -> IO HSimpleSprite
primitive simpleSpriteToSprite "XS_simpleSpriteToSprite" :: HSimpleSprite -> HSprite
primitive setSurface "XS_setSurface" :: HSimpleSprite -> HDDSurface -> IO ()
primitive get_MinSpriteSize "XSget_MinSpriteSize" :: IO Int
primitive set_MinSpriteSize "XSset_MinSpriteSize" :: Int -> IO ()
newtype HSoundSprite = HSoundSprite Word32 deriving ()
primitive isNullHSoundSprite "XStest_isNullHSoundSprite" :: IOError -> Bool
primitive newSoundSprite "XS_newSoundSprite" :: HDSBuffer -> Double -> Double -> Double -> SpriteTreeChain -> IO HSoundSprite
primitive soundSpriteToSpriteTree "XS_soundSpriteToSpriteTree" :: HSoundSprite -> HSpriteTree
primitive updateSoundSprite "XS_updateSoundSprite" :: HSoundSprite -> SpriteTime -> Double -> Double -> Double -> IO ()
newtype HSpriteGroup = HSpriteGroup Word32 deriving ()
primitive isNullHSpriteGroup "XStest_isNullHSpriteGroup" :: IOError -> Bool
primitive newSpriteGroup "XS_newSpriteGroup" :: SpriteTreeChain -> SpriteTreeChain -> IO HSpriteGroup
primitive spriteGroupToSpriteTree "XS_spriteGroupToSpriteTree" :: HSpriteGroup -> HSpriteTree
primitive resetSpriteGroup "XS_ResetSpriteGroup" :: HSpriteGroup -> SpriteTreeChain -> Bool -> IO ()
newtype HSpriteEngine = HSpriteEngine Word32 deriving ()
primitive isNullHSpriteEngine "XStest_isNullHSpriteEngine" :: IOError -> Bool
primitive newSpriteEngine "XS_newSpriteEngine" :: HWND -> HSpriteTree -> IO HSpriteEngine
primitive onResizeSpriteEngine "XS_onResizeSpriteEngine" :: HSpriteEngine -> IO ()
primitive deleteSpriteEngine "XS_deleteSpriteEngine" :: HSpriteEngine -> IO Int
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

primitive consts_SpriteTreeChain "XSconsts_SpriteTreeChain" :: Int -> SpriteTreeChain
primitive consts_LightType "XSconsts_LightType" :: Int -> LightType
primitive consts_ThreadPriority "XSconsts_ThreadPriority" :: Int -> ThreadPriority

needPrims_hugs
