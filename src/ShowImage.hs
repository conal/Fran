-- Toplevel fellow for displaying static Images in a window
--
-- Last modified Thu Sep 19 11:46:52 1996

module ShowImage
        (
        showImage,  -- :: Image -> IO ()

	-- HACK
	windowSize  -- :: HWND -> IO RECT
        ) where

import Win32 hiding (writeFile, readFile,rgb,loadBitmap)

import qualified MutVar

import Image hiding (polyline,polygon,ellipse,rectangle)
import qualified RenderImage as Render
import Transform2

import Channel
import Monad (when)
import Utils (andOnError)
import Color
import Point2
import Pick2Image
import Vector2

-- This code originates from the Active Haskell implementation distributed
-- with the Yale Hugs release. Rewritten to cope with just static Pictures
-- and to better understand the workings of the Win32 interface.  -- sof


showImage :: Image -> IO ()
showImage im =
  display (screen im)

type ScreenHandler = 
   HWND                              -- window handle
   -> Channel (Maybe ScreenRequest)  -- Nothing => shutdown
                                     -- Just x  => perform x command.
   -> IO ()

screen :: Image -> ScreenHandler
screen img = \ hwnd chan ->
  let 
   cmdLoop :: Image -> IO ()
   cmdLoop img = 
    -- get next command
    getChan chan        >>= \ mbRequest ->
    case mbRequest of
      -- shutdown, just drop out of loop.
      Nothing     -> return ()
      Just request ->
        case request of
          Redraw      -> 
            draw hwnd img >>
            cmdLoop img
          _ -> cmdLoop img
  in
  -- clear out window first
  invalidateRect (Just hwnd) Nothing eraseBackground >>
  updateWindow hwnd `when` forceUpdate               >>

  catch (cmdLoop img) (\_ -> destroyWindow hwnd)     >>
  return ()

data ScreenRequest
  = Redraw 


display :: ScreenHandler -> IO ()
display screen =
  let demoClass = mkClassName "RBMH 2d image" in
  loadIcon   Nothing iDI_APPLICATION >>= \ icon ->
  loadCursor Nothing iDC_ARROW       >>= \ cursor ->
  getStockBrush wHITE_BRUSH          >>= \ whiteBrush ->
  getModuleHandle Nothing            >>= \ mainInstance ->
  registerClass (
        (orbs [cS_HREDRAW, cS_VREDRAW]), -- redraw if window size Changes
        mainInstance,
        (Just icon),
        (Just cursor),
        (Just whiteBrush),
        Nothing,
        demoClass)               >>
  -- A channel is used to communicate events from the Win event loop to
  -- the screen handler (i.e. application appearing inside it)
  newChan                        >>= \ chan ->
  let
    -- communication
    send = putChan chan . Just
    kill = putChan chan Nothing

    -- one-shot system event handler
    winEventHandler hwnd msg wParam lParam
      | msg == wM_PAINT =
        send Redraw   >>
        return 0

      | msg == wM_DESTROY =
        set_hugsQuitFlag True     >>
        kill                      >>
        return 0

      | otherwise = defWindowProc (Just hwnd) msg wParam lParam
  in
  createWindow  
        demoClass
        "RBMH 2d image"
        wS_OVERLAPPEDWINDOW
        Nothing    Nothing    -- x y
        (Just width) (Just height)
        Nothing               -- parent
        Nothing               -- menu
        mainInstance
        (winEventHandler)            >>= \ w ->
  forkIO (screen w chan)             >>
  showWindow w sW_SHOWNORMAL         >>
  updateWindow w                     >>
  eventLoop w                        >>
  unregisterClass demoClass mainInstance `catch` \_ -> return () >>
  return ()


draw :: HWND -> Image -> IO ()
draw hwnd im =
  paintWith hwnd (\hdc lpps ->
    -- I'd like to center the text.  Why doesn't the following work?
    setTextAlign hdc tA_CENTER  >>
    windowSize hwnd 		>>= \ (l',t',r',b') ->
    let 
      w' = r' - l'
      h' = b' - t'
      -- Map to screen space.  Can we do this with setWorldTransform?
      stretch = Vector2XY pixelsPerLengthHorizontal pixelsPerLengthVertical
      im' = scale2     stretch            *%
            translate2 (vector2XY 1 1)    *%
            scale2     (vector2XY 1 (-1)) *%  -- flip Y
            im
    in
    if doubleBuffered then
      -- By trial and error, I discovered that you should create
      -- a bitmap compatible with the window you're going to draw on.
      -- If it is merely compatible with the buffer dc (which is, in turn,
      -- compatible with the window), you get a monochrome bitmap.  -sof
      withDC (Just hwnd) (\ windc ->
        withCompatibleDC (Just windc) (\ buffer ->
          withCompatibleBitmap hdc w' h' (\ bitmap ->
            selectBitmapIn buffer bitmap (
  	      Win32.bitBlt buffer 0 0 w' h' buffer 0 0 bLACKNESS >>
              Render.draw buffer im'   >>
              bitBlt hdc 0 0 w' h' buffer 0 0 sRCCOPY
            )
          )
        )
      )
    else
      Render.draw hdc im'
  )


{- ToDo: do this `natively' -}
windowSize :: HWND -> IO RECT
windowSize hwnd =
 alloc 			   >>= \ lprect ->
 getClientRect hwnd lprect >> 
 getLPRECT lprect          >>= \ r ->
 free lprect               >>
 return r

----------------------------------------------------------------
-- Common imperative programming idioms
----------------------------------------------------------------

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
