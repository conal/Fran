-- Experimental support for displaying image behaviors in an existing
-- window.  Intended for use with the ActiveX Hugs control
-- 
-- Last modified Sat Sep 14 16:55:44 1996
-- 
-- Recycled bits from ShowImageB.hs.

module ShowImageB
        (
        draw,   -- :: ImageB.Image -> WindowPainter
        disp   -- :: ImageB.Image -> IO ()
        ) where

import Win32 hiding (writeFile, readFile,rgb,
                     polyline,arc,ellipse,rectangle)

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
import ShowImage(windowSize)


-- Something that repeatedly paints into a window.  Expected to use
-- "postpone"-based pseudo-concurrency.

type WindowPainter = HWND -> IO ()

-- Generate an infinite stream of times, given an initial time in
-- integer milliseconds.

times :: MilliSeconds -> IO [Time]
times startMS =
  unsafeInterleaveIO (
    timeSinceMS startMS                >>= \ t ->
    unsafeInterleaveIO (times startMS) >>= \ ts ->
    return (t:ts))

timeSinceMS :: MilliSeconds -> IO Time

timeSinceMS startMS =
  timeGetTime                        >>= \ ms ->
  let
      (wholePart,fracPart) = (ms - startMS) `divMod` 1000
  in
      return (  (fromInt wholePart :: Time)
              + (fromInt fracPart  :: Time) / 1000.0 )



worldToScreen, screenToWorld :: Transform2

worldToScreen =
  (scale2 (vector2XY pixelsPerLengthHorizontal
                     pixelsPerLengthVertical) `compose2`
  translate2 (vector2XY 1 1))                `compose2`
  scale2     (vector2XY 1 (-1))                   -- flip Y

screenToWorld = inverse2 worldToScreen


draw :: MilliSeconds -> ImageB -> WindowPainter

draw startMS imb hwnd =
  --debugMessage "Entering draw" >>
  times startMS               >>= \ ts ->
  let
  loop (im:ims') =
     postpone (
       (invalidateRect (Just hwnd) Nothing eraseBackground >>
        paintWith hwnd (\hdc lpps ->
          windowSize hwnd >>= \ (l',t',r',b') ->
          let 
            w' = r' - l'
            h' = b' - t'
            -- [l,t,r,b,w,h] = map fromInt [l',t',r',b',w',h']
            -- Map to screen space.  Can we do this with
            -- setWorldTransform?
            im' = worldToScreen *% im
          in
          if doubleBuffered then
            -- (sof) By trial and error, I discovered that you
            -- should create a bitmap compatible with the window
            -- you're going to draw on.  If it is merely compatible
            -- with the buffer dc (which is, in turn, compatible
            -- with the window), you get a monochrome bitmap.
            withDC (Just hwnd) (\ windc ->
              withCompatibleDC (Just windc) (\ buffer ->
                withCompatibleBitmap hdc (r'-l') (b'-t') (\ bitmap ->
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
       >> loop ims' )
  in
      loop (imb `ats` ts)


-- Testing

makeTestWindow :: MilliSeconds -> (HWND -> IO ()) -> IO ()

-- The hwndConsumer is presumed to postpone most of its work.

makeTestWindow startMS hwndConsumer =
  initUser >>
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

  let

    -- Map lParam mouse point to a Point2
    posn lParam = 
      let (y,x) = lParam `divMod` 65536 in
        screenToWorld *% (point2XY (fromInt x) (fromInt y))

    fireButtonEvent lParam isLeft isDown =
        timeSinceMS startMS         >>= \ t ->
        firePrimBPEv t (posn lParam) isLeft isDown >>
        return 0

    -- get position from mouse messages
    wndProc2 hwnd msg wParam lParam
      | msg == wM_TIMER
      = activateOne               >>
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
      = fireButtonEvent lParam True True

      | msg == wM_LBUTTONUP =
        fireButtonEvent lParam True False

      | msg == wM_RBUTTONDOWN || msg == wM_RBUTTONDBLCLK =
        fireButtonEvent lParam False True

      | msg == wM_RBUTTONUP =
        fireButtonEvent lParam False False

      | msg == wM_MOUSEMOVE =
        timeSinceMS startMS                 >>= \ t ->
        firePrimMouseEv t (posn lParam) >>
        return 0
{-
      | msg == wM_SIZE =
        let
         w = loWord lParam
         h = hiWord lParam
        in
        writeVar sz_var (w,h)        >>
        send (Resize w h)            >>
        return 0
-}

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

  -- Set the millisecond timer
  setWinTimer w 1 (round (1000/targetFPS)) >>= \ timer ->
  showWindow w sW_SHOWNORMAL         >>
  hwndConsumer w                     >>
  eventLoop w                        >>
  unregisterClass demoClass mainInstance `catch` \_ -> return () >>
  return ()


-- Next section swiped from ShowImageB.hs

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


disp :: ImageB -> IO ()
disp imb =
  timeGetTime >>= \ startMS ->
  makeTestWindow startMS $ draw startMS imb

-- import ImageBTest


-- To test, try "disp i{j}", where j `elem` [1..]
