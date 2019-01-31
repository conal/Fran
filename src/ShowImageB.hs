-- Experimental support for displaying image behaviors in an existing
-- window.  Intended for use with the ActiveX Hugs control
-- 
-- Last modified Tue Oct 15 12:42:31 1996
-- 
-- Recycled bits from ShowImageB.hs.

module ShowImageB
        (
        draw,   -- :: ImageB.Image -> WindowPainter
        disp   -- :: ImageB.Image -> IO ()
        ) where

import Prelude hiding (MutVar,newVar,readVar,writeVar)
import MutVar
import Win32 hiding (writeFile, readFile,rgb,
                     polyline,polygon,arc,ellipse,rectangle)

import Behavior
import Vector2
import Point2
import ImageB (ImageB)
import Image
import Transform2

import qualified RenderImage as Render
import Utils (andOnError)
import PrimInteract
import Postpone


-- Something that repeatedly paints into a window.  Expected to use
-- "postpone"-based pseudo-concurrency.

type WindowPainter = HWND -> IO ()

-- Generate an infinite stream of times, given an initial time in
-- integer milliseconds.

times :: MilliSeconds -> IO [Time]
times startMS =
  unsafeInterleaveIO (
    timeSinceMS startMS                >>= \ t ->
    -- putStrLn ("t = " ++ show t) >>
    unsafeInterleaveIO (times startMS) >>= \ ts ->
    return (t:ts))

timeSinceMS :: MilliSeconds -> IO Time

timeSinceMS startMS =
  timeGetTime                        >>= \ ms ->
  let
      (wholePart,fracPart) = (ms - startMS) `divMod` 1000
      t = (  (fromInt wholePart :: Time)
           + (fromInt fracPart  :: Time) / 1000.0 )
  in
      -- putStrLn ("time " ++ show t) >>
      return t


-- For testing

disp :: ImageB -> IO ()

disp imb =
  timeGetTime >>= \ startMS ->
  makeTestWindow startMS $ draw startMS imb

draw :: MilliSeconds -> ImageB -> WindowPainter

draw startMS imb hwnd =
  -- debugMessage "Entering draw" >>
  times startMS               >>= \ ts ->
  let
  loop (t:ts') (im:ims') =
       -- putStrLn ("draw sample time = " ++ show t) >>
       (invalidateRect (Just hwnd) Nothing eraseBackground >>
        paintWith hwnd (\hdc lpps ->
          windowSize hwnd >>= \ (w',h') ->
          let 
            im' = worldToScreen w' h' *% im
          in
          if doubleBuffered then
            -- (sof) By trial and error, I discovered that you
            -- should create a bitmap compatible with the window
            -- you're going to draw on.  If it is merely compatible
            -- with the buffer dc (which is, in turn, compatible
            -- with the window), you get a monochrome bitmap.
            withDC (Just hwnd) (\ windc ->
              withCompatibleDC (Just windc) (\ buffer ->
                withCompatibleBitmap hdc w' h' (\ bitmap ->
                  selectBitmapIn buffer bitmap (
                    --putStrLn (show im)    >>
                    Win32.bitBlt buffer 0 0 w' h' buffer 0 0 bLACKNESS >>
                    -- debugMessage "Calling Render.draw" >>
                    Render.draw buffer im'  >>
                    bitBlt hdc 0 0 w' h' buffer 0 0 sRCCOPY
                  )
                )
              )
            )
          else
            Render.draw hdc im'))
       >> postpone (loop ts' ims')
  in
      loop ts (imb `ats` ts)

worldToScreen, screenToWorld :: Int -> Int -> Transform2

worldToScreen w' h' =
  translate2 (vector2XY (fromInt w' / 2) (fromInt h' / 2)) `compose2`
  scale2 (vector2XY pixelsPerLengthHorizontal (- pixelsPerLengthVertical))

screenToWorld w' h' = inverse2 (worldToScreen w' h')


-- Testing

makeTestWindow :: MilliSeconds -> (HWND -> IO ()) -> IO ()

-- The hwndConsumer is presumed to postpone most of its work.

makeTestWindow startMS hwndConsumer =
  let demoClass = mkClassName "RBMH Test Window" in
  loadIcon   Nothing iDI_APPLICATION >>= \ icon ->
  loadCursor Nothing iDC_ARROW       >>= \ cursor ->
  getStockBrush bLACK_BRUSH          >>= \ blackBrush ->
  getModuleHandle Nothing            >>= \ mainInstance ->
  registerClass (
        (orbs [cS_HREDRAW, cS_VREDRAW]), -- redraw if window size Changes
        mainInstance,
        (Just icon),
        (Just cursor),
        (Just blackBrush),
        Nothing,
        demoClass)                >>

  --newVar (0,0)		  >>= \ szVar ->
  newVar 0			  >>= \ drawTimeVar ->
  let
    -- Map lParam mouse point to a Point2
    posn hwnd lParam =
      windowSize hwnd            >>= \(w',h') ->
      return $
      let (y,x) = lParam `divMod` 65536 in
        screenToWorld w' h' *% (point2XY (fromInt x) (fromInt y))

    fireButtonEvent hwnd lParam isLeft isDown =
        timeSinceMS startMS         >>= \ t ->
	posn hwnd lParam	    >>= \ p ->
        firePrimBPEv t p isLeft isDown >>
        return 0

    wndProc2 hwnd msg wParam lParam
      | msg == wM_TIMER
      = activateOne		    >>	-- draw a frame
        -- Recalculate frame rate.
        timeSinceMS startMS         >>= \ t ->
	readVar drawTimeVar	    >>= \ prevT ->
	writeVar drawTimeVar t	    >>
	firePrimFPS t (if t==prevT then 0 else (1 / (t - prevT))) >>
        return 0

      | msg == wM_DESTROY
      = set_hugsQuitFlag True     >>
        return 0

      | msg == wM_PAINT
      = -- putStrLn "wM_PAINT" >>
        -- I think this is here to clear the clip list
        paintWith hwnd (\ _ _ ->
        return 0)

      | msg == wM_LBUTTONDOWN || msg == wM_LBUTTONDBLCLK
      = fireButtonEvent hwnd lParam True True

      | msg == wM_LBUTTONUP =
        fireButtonEvent hwnd lParam True False

      | msg == wM_RBUTTONDOWN || msg == wM_RBUTTONDBLCLK =
        fireButtonEvent hwnd lParam False True

      | msg == wM_RBUTTONUP =
        fireButtonEvent hwnd lParam False False

      | msg == wM_MOUSEMOVE =
        timeSinceMS startMS                 >>= \ t ->
	posn hwnd lParam		    >>= \ p ->
        firePrimMouseEv t p		    >>
	return 0

      | msg == wM_KEYDOWN =
        timeSinceMS startMS                 >>= \ t ->
        firePrimKeyEv t (toEnum wParam) True >>
        return 0

      | msg == wM_KEYUP =
        timeSinceMS startMS                 >>= \ t ->
        firePrimKeyEv t (toEnum wParam) False >>
        return 0

      | msg == wM_SIZE =
        timeSinceMS startMS		    >>= \ t ->
{-
	windowSize hwnd			    >>= \ (px, py) ->
	let 
	    (x,y) = point2XYCoords (
		     screenToWorld px py
		       *% (point2XY (fromInt px) (fromInt py)))
        in
-}
	(posn hwnd lParam >>= return . point2XYCoords) >>= \ (x,y) ->
	-- (x,y) is the lower-right corner, so we must flip y and double
	-- both coordinates to get the width and height
	firePrimViewSize t (vector2XY (2*x) (-2*y)) >>
        return 0

      | otherwise
      = defWindowProc (Just hwnd) msg wParam lParam
  in
  createWindow  
        demoClass
        "RBMH Image Animation"
        wS_OVERLAPPEDWINDOW
        Nothing    Nothing    -- x y
        (Just width) (Just height)
        Nothing               -- parent
        Nothing               -- menu
        mainInstance
        wndProc2
                                     >>= \ w ->

  initUser >>
  -- Set the millisecond timer
  setWinTimer w 1 (round (1000/targetFPS)) >>= \ timer ->
  showWindow w sW_SHOWNORMAL         >>
  hwndConsumer w                     >>
  -- sendMessage w wM_SIZE width height >>
  eventLoop w                        >>
  unregisterClass demoClass mainInstance `catch` \_ -> return () >>
  return ()


-- Get the width and height of a window's client area, in pixels.

windowSize :: HWND -> IO (LONG,LONG)

windowSize hwnd =
 alloc 			   >>= \ lprect ->
 getClientRect hwnd lprect >> 
 getLPRECT lprect          >>= \ (l',t',r',b') ->
 free lprect               >>
 return (r' - l', b' - t')



----------------------------------------------------------------
-- Common imperative programming idioms
----------------------------------------------------------------

-- ToDo: factor these out into a utility module


-- Note use of "andOnError" to guarantee that the cleanup code gets executed!

-- ToDo: better names for "with" and "bracketWith"

with :: IO a -> (a -> IO ()) -> (a -> IO ()) -> IO ()
with allocate deallocate use =
  allocate     >>= \ x ->
  use x        
  `andOnError`
  deallocate x

bracketWith :: IO a -> (a -> IO b) -> IO () -> IO ()
bracketWith setup cleanup doSomething =
  setup             >>= \ prevState ->
  doSomething       
  `andOnError`
  cleanup prevState >>
  return ()

----------------------------------------------------------------


withDC :: Maybe HWND -> (HDC -> IO ()) -> IO ()
withDC mhwnd = 
  with (getDC mhwnd) (releaseDC mhwnd)

withCompatibleDC :: Maybe HDC -> (HDC -> IO ()) -> IO ()
withCompatibleDC mhdc = 
  with (createCompatibleDC mhdc) deleteDC

withCompatibleBitmap :: HDC -> Int -> Int -> (HBITMAP -> IO ()) -> IO ()
withCompatibleBitmap hdc w h = 
  with (createCompatibleBitmap hdc w h) deleteBitmap


selectBitmapIn :: HDC -> HBITMAP -> IO () -> IO ()
selectBitmapIn hdc hbmp = 
  bracketWith (selectBitmap hdc hbmp) (selectBitmap hdc)



----------------------------------------------------------------
-- Program parameters
----------------------------------------------------------------

-- do we want to use double buffering?
doubleBuffered = True

-- We can choose whether or not the background gets erased for us.
-- The animation examples tend to be displayed on a large
-- background square so there's no need for the OS to do it too
-- (and we get less flicker).
-- The disadvantage is that if you expand the window to be larger than
-- the animation area, you see trailing at the edges.
eraseBackground = False

-- we can force an immediate update in response to the wM_TIMER call.
forceUpdate = False

-- Background
-- backColor = wHITENESS
backColor = bLACKNESS

-- Pixel size of window on startup.  Big enough to show -1 to 1 in
-- continuous units in X and Y, plus a bit.  Give more vertical space for
-- title bar.
width, height :: Int
(width, height) = vector2ToScreenSize (vector2XY 2.1 2.3)

-- Frames per second target
targetFPS = 40 :: Float

--- Testing


-- import ImageBTest


-- To test, try "disp i{j}", where j `elem` [1..]
