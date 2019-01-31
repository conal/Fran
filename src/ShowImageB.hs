-- Experimental support for displaying image behaviors in an existing
-- window.  Intended for use with the ActiveX Hugs control
-- 
-- Last modified Sun Nov 10 17:56:17 1996
-- 
-- Recycled bits from ShowImageB.hs.

module ShowImageB
        (
        draw,   -- :: ImageB.Image -> WindowPainter
        disp   -- :: ImageB.Image -> IO ()
        ) where

-- This module contains a whole lot of Win32 calls, and normally I
-- wouldn't import Win32 qualified.  I did, however, so that all of the
-- foreign library dependencies can be determined simply by grepping for
-- the qualifiers (like "Win32.").

import qualified Win32

import IORef(Ref,newRef,getRef,setRef)
import IOExtensions(unsafeInterleaveIO)

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

type WindowPainter = Win32.HWND -> IO ()

timeSinceMS :: Win32.MilliSeconds -> IO Time

timeSinceMS startMS =
  Win32.timeGetTime                        >>= \ ms ->
  return (fromIntegral (ms - startMS) / 1000.0)


-- For testing

disp :: ImageB -> IO ()

disp imb =
  Win32.timeGetTime >>= \ startMS ->
  makeTestWindow startMS $ draw startMS imb

draw :: Win32.MilliSeconds -> ImageB -> WindowPainter

draw startMS imb hwnd = loop imb
  where
    loop imb =
      getTime              >>= \ t ->
      let (im,imb') = imb `at` t in
        drawOne t im >> postpone (loop imb')

    getTime = timeSinceMS startMS

    drawOne t im = 
       -- putStrLn ("draw sample time = " ++ show t) >>
       (Win32.invalidateRect (Just hwnd) Nothing eraseBackground >>
        Win32.paintWith hwnd (\hdc lpps ->
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
                    Win32.bitBlt buffer 0 0 w' h' buffer 0 0 Win32.bLACKNESS >>
                    -- debugMessage "Calling Render.draw" >>
                    Render.draw buffer im'  >>
                    Win32.bitBlt hdc 0 0 w' h' buffer 0 0 Win32.sRCCOPY
                  )
                )
              )
            )
          else
            Render.draw hdc im'))


{-
-- Pixels per length unit, horizontal and vertical
screenSizeToVector2 :: (Int,Int) -> Vector2

screenSizeToVector2 (w,h) =
  vector2XY (fromInt w / screenPixelsPerLengthHorizontal)
            (fromInt h / screenPixelsPerLengthVertical)
-}

vector2ToScreenSize :: Vector2 -> (Int,Int)

vector2ToScreenSize v =
  ( round (dx * screenPixelsPerLengthHorizontal) ,
    round (dy * screenPixelsPerLengthVertical  ) )
  where
    (dx,dy) = vector2XYCoords v

-- Note that these number do not have to correspond with
-- bitmapPixelsPerLengthHorizontal and bitmapPixelsPerLengthVertical.  If
-- they do, however, the scalings will cancel out.
screenPixelsPerLengthHorizontal = 200 :: Double
screenPixelsPerLengthVertical   = 200 :: Double

worldToScreen, screenToWorld :: Int -> Int -> Transform2

worldToScreen | presentation = worldToScreenStretchy
              | otherwise    = worldToScreenNonStretchy

worldToScreenNonStretchy w' h' =
  translate2 (vector2XY (fromInt w' / 2) (fromInt h' / 2)) `compose2`
  scale2 (vector2XY    screenPixelsPerLengthHorizontal
                    (- screenPixelsPerLengthVertical  ))

-- Presentation version.  Origin-centered, and show -1.2 to 1.2 in shorter
-- dimension.

worldToScreenStretchy w' h' =
  translate2 (vector2XY halfW halfH) `compose2`
  scale2     (vector2XY m     (-m) )
  where
   halfW = fromInt w' / 2
   halfH = fromInt h' / 2
   m     = (halfH `min` halfW)/1.2


screenToWorld w' h' = inverse2 (worldToScreen w' h')


-- Testing

makeTestWindow :: Win32.MilliSeconds -> (Win32.HWND -> IO ()) -> IO ()

-- The hwndConsumer is presumed to postpone most of its work.

makeTestWindow startMS hwndConsumer =
  let demoClass = Win32.mkClassName "RBMH Test Window" in
  Win32.loadIcon   Nothing Win32.iDI_APPLICATION >>= \ icon ->
  Win32.loadCursor Nothing Win32.iDC_ARROW       >>= \ cursor ->
  Win32.getStockBrush Win32.bLACK_BRUSH          >>= \ blackBrush ->
  Win32.getModuleHandle Nothing            >>= \ mainInstance ->
  Win32.registerClass (
        (Win32.orbs [Win32.cS_HREDRAW,
               Win32.cS_VREDRAW]), -- redraw if window size Changes
        mainInstance,
        (Just icon),
        (Just cursor),
        (Just blackBrush),
        Nothing,
        demoClass)                >>

  --newRef (0,0)                  >>= \ szVar ->
  newRef 0                        >>= \ drawTimeVar ->
  let
    -- Map lParam mouse point to a Point2
    posn hwnd lParam =
      windowSize hwnd            >>= \(w',h') ->
      return $
      let (y,x) = lParam `divMod` 65536 in
        screenToWorld w' h' *% (point2XY (fromInt x) (fromInt y))

    fireButtonEvent hwnd lParam isLeft isDown =
        timeSinceMS startMS         >>= \ t ->
        posn hwnd lParam            >>= \ p ->
        -- putStrLn ("Button " ++ show (isLeft,isDown) ++ " at time " ++ show t) >>
        fire button t (p,isLeft,isDown) >>
        return 0

    wndProc2 hwnd msg wParam lParam
      | msg == Win32.wM_TIMER
      = activateOne                 >>  -- draw a frame
        -- Recalculate frame rate.
        timeSinceMS startMS         >>= \ t ->
        getRef drawTimeVar          >>= \ prevT ->
        setRef drawTimeVar t        >>
        fire fps t (if t==prevT then 0 else (1 / (t - prevT))) >>
        return 0

      | msg == Win32.wM_DESTROY
      = Win32.set_hugsQuitFlag True     >>
        return 0

      | msg == Win32.wM_PAINT
      = -- putStrLn "wM_PAINT" >>
        -- I think this is here to clear the clip list
        Win32.paintWith hwnd (\ _ _ ->
        return 0)

      | msg == Win32.wM_LBUTTONDOWN || msg == Win32.wM_LBUTTONDBLCLK
      = fireButtonEvent hwnd lParam True True

      | msg == Win32.wM_LBUTTONUP =
        fireButtonEvent hwnd lParam True False

      | msg == Win32.wM_RBUTTONDOWN || msg == Win32.wM_RBUTTONDBLCLK =
        fireButtonEvent hwnd lParam False True

      | msg == Win32.wM_RBUTTONUP =
        fireButtonEvent hwnd lParam False False

      | msg == Win32.wM_MOUSEMOVE =
        timeSinceMS startMS                 >>= \ t ->
        posn hwnd lParam                    >>= \ p ->
        fire mousePos t p                   >>
        return 0

      | msg == Win32.wM_KEYDOWN =
        timeSinceMS startMS                 >>= \ t ->
        fire key t (toEnum wParam,True)     >>
        return 0

      | msg == Win32.wM_KEYUP =
        timeSinceMS startMS                 >>= \ t ->
        fire key t (toEnum wParam,False)    >>
        return 0 

      | msg == Win32.wM_SIZE =
        timeSinceMS startMS                 >>= \ t ->
{-
        windowSize hwnd                     >>= \ (px, py) ->
        let 
            (x,y) = point2XYCoords (
                     screenToWorld px py
                       *% (point2XY (fromInt px) (fromInt py)))
        in
-}
        -- (posn hwnd lParam >>= return . point2XYCoords) >>= \ (x,y) ->
        map point2XYCoords (posn hwnd lParam)  >>= \ (x,y) ->
        -- (x,y) is the lower-right corner, so we must flip y and double
        -- both coordinates to get the width and height
        fire viewSz t (vector2XY (2*x) (-2*y)) >>
        return 0

      | otherwise
      = Win32.defWindowProc (Just hwnd) msg wParam lParam
  in
   makeWindow demoClass mainInstance wndProc2  >>= \ w ->

  initProcTable                                >>
  initUser >>
  -- Set the millisecond timer
  Win32.setWinTimer w 1 (round (1000/targetFPS)) >>= \ timer ->
  Win32.showWindow w Win32.sW_SHOWNORMAL         >>
  Win32.bringWindowToTop w                       >>
  hwndConsumer w                                 >>
  -- This doesn't work.  It's a little too big, and flipped.
  -- Win32.sendMessage w Win32.wM_SIZE width height >>
  Win32.eventLoop w                        >>
  Win32.unregisterClass demoClass mainInstance `catch` \_ -> return () >>
  return ()

-- Make a window.  If we need the window to stay on top, do so.


makeWindow | presentation = makeWindowPresent
           | otherwise    = makeWindowNormal

makeWindowPresent demoClass mainInstance wndProc2 =
 Win32.createWindowEx Win32.wS_EX_TOPMOST
               demoClass
               "RBMH Image Animation"
               Win32.wS_OVERLAPPEDWINDOW
               -- Nothing    Nothing    -- x y
               (Just (1024-width)) (Just (-17))
               (Just width) (Just height)
               Nothing               -- parent
               Nothing               -- menu
               mainInstance
               wndProc2

makeWindowNormal demoClass mainInstance wndProc2 =
 Win32.createWindow demoClass
              "RBMH Image Animation"
              Win32.wS_OVERLAPPEDWINDOW
              Nothing    Nothing    -- x y
              (Just width) (Just height)
              Nothing               -- parent
              Nothing               -- menu
              mainInstance
              wndProc2



-- Get the width and height of a window's client area, in pixels.

windowSize :: Win32.HWND -> IO (Win32.LONG,Win32.LONG)

windowSize hwnd =
 Win32.getClientRect hwnd >>= \ (l',t',r',b') ->
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


withDC :: Maybe Win32.HWND -> (Win32.HDC -> IO ()) -> IO ()
withDC mhwnd = 
  with (Win32.getDC mhwnd) (Win32.releaseDC mhwnd)

withCompatibleDC :: Maybe Win32.HDC -> (Win32.HDC -> IO ()) -> IO ()
withCompatibleDC mhdc = 
  with (Win32.createCompatibleDC mhdc) Win32.deleteDC

withCompatibleBitmap ::
  Win32.HDC -> Int -> Int -> (Win32.HBITMAP -> IO ()) -> IO ()
withCompatibleBitmap hdc w h = 
  with (Win32.createCompatibleBitmap hdc w h) Win32.deleteBitmap


selectBitmapIn :: Win32.HDC -> Win32.HBITMAP -> IO () -> IO ()
selectBitmapIn hdc hbmp = 
  bracketWith (Win32.selectBitmap hdc hbmp) (Win32.selectBitmap hdc)



----------------------------------------------------------------
-- Program parameters
----------------------------------------------------------------

-- Doing a (probably PowerPoint) presentation.  Make the window stay on
-- top, but in a corner.  Also, resizing the window scales the animation.
presentation = False

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
-- backColor = Win32.wHITENESS
backColor = Win32.bLACKNESS

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
