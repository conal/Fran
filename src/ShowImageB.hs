-- Show a sprite tree in a window.

module ShowImageB ( updatePeriodGoal
                  , showSpriteTree
                  , initialViewSizeVar, setInitialViewSize, withInitialViewSize
                  ) where

import HSpriteLib
import BaseTypes
import Transform2
import Vector2
import VectorSpace
import Point2
import qualified Win32
import IOExts
import Monad (when)
import Concurrent (writeChan)
import Event (EventChannel, newChannelEvent)
import User
import Word(word32ToInt)
import Int(int32ToInt, Int32)
import RenderImage (importPixelsPerLength, screenPixelsPerLength
                   , toPixel32)
import Compatibility (safeTry)
import IO

type UserEventConsumer = Time -> UserAction -> IO ()

-- Window stuff

-- Type of application-specific window proc extension.  Return True if
-- handled.

-- Make a window and route user events

-- Note: if SetMenu gets a GC interface, then there will be no need to
-- pass a menu into makeWindow, etc.

makeWindow :: (Win32.HWND -> IO ())	-- create
           -> IO ()			-- resize
           -> IO ()			-- update
           -> IO ()			-- close
           -> SpriteTime		-- update goal interval
           -> UserEventConsumer		-- receives user events
           -> Win32.MbHMENU
           -> IO Win32.HWND             -- returns a window handle


makeWindow createIO resizeIO updateIO closeIO
           updateInterval userEventConsumer
           mbMenu =
  let 
      send userEvent = do
        -- Get time now.  Bogus, since the event really happened before
        -- now.
        t <- currentSpriteTime
        --putStrLn ("User event " ++ show (t, userEvent))
        --putStr ("ue " ++ show t ++ " ")
        userEventConsumer t userEvent
        return 0

      -- Map lParam mouse point to a Point2
      posn hwnd lParam =
        do (winWidth,winHeight) <- getViewSize hwnd
           -- putStrLn ("posn " ++ show (w',h'))
           ( return $
             let -- Turn window coords into logical coords.
                 -- First compute coords relative to the upper left
                 (yRelWinUL,xRelWinUL)  = lParam `divMod` 65536
                 -- subtract position of window center relative to UL, to get
                 -- coords relative to window center
                 xRelWinCenter = xRelWinUL - winWidth `div` 2
                 yRelWinCenter = yRelWinUL - winHeight `div` 2
             in
                 -- Throw in scaling, recalling that window positive == down
                 point2XY (fromInt32 xRelWinCenter / screenPixelsPerLength)
                          (fromInt32 yRelWinCenter / -screenPixelsPerLength) )

      fireButtonEvent isLeft isDown =
        do --p <- posn hwnd lParam
           --putStrLn ("Button " ++ show (isLeft,isDown) ++ " at pos " ++ show p)
           send (Button isLeft isDown)

      fireKeyEvent wParam isDown =
        do --putStrLn ("fireKeyEvent " ++ show (wParam, char, isDown, t))
           send (Key isDown wParam) -- (Win32.VKey wParam)) -- GSL

      wndProc2 hwnd msg wParam lParam

        | msg == Win32.wM_DESTROY = do
          -- Kill the update timer
          Win32.killTimer (Just hwnd) 1
          return 0

        | msg == Win32.wM_CLOSE = do
          --putStrLn "Got WM_CLOSE"
          -- Put this here to allow one last update.  It's useful for
          -- effects that depend on quit.
          send Quit
          -- We'd like to call updateIO now and see the Quit as having
          -- happened, but there's a problem.  The sampling time will be
          -- so soon after the occurrence time that they may well be
          -- equal, within the OS timer's resolution.  In that case, the
          -- quit won't be in the past and so won't get responded to.
          -- Hack: sleep for a while.  What's a better solution?  Note
          -- that this problem has other symptoms as well.  For instance,
          -- a button release following very shortly after a press will
          -- look like it happened at the same time.
          sleep 100 ; updateIO          -- do final update
          closeIO                       -- and the close action
          return 0

        | msg == Win32.wM_LBUTTONDOWN || msg == Win32.wM_LBUTTONDBLCLK =
          fireButtonEvent True True

        | msg == Win32.wM_LBUTTONUP =
          fireButtonEvent True False

        | msg == Win32.wM_RBUTTONDOWN || msg == Win32.wM_RBUTTONDBLCLK =
          fireButtonEvent False True

        | msg == Win32.wM_RBUTTONUP =
          fireButtonEvent False False

        | msg == Win32.wM_MOUSEMOVE = do
          p <- posn hwnd lParam
          send (MouseMove p)

        | msg == Win32.wM_KEYDOWN =
          fireKeyEvent wParam True

        | msg == Win32.wM_KEYUP =
          fireKeyEvent wParam False

        | msg == Win32.wM_CHAR =
          send (CharKey (toEnum (word32ToInt wParam)))

        | msg == Win32.wM_SIZE = do
          let (hPix,wPix) = lParam `divMod` 65536
          send (Resize (vector2XY (fromInt32 wPix / screenPixelsPerLength)
                                  (fromInt32 hPix / screenPixelsPerLength)))
          resizeIO
          return 0

        -- Timer.  Do the sprite tree updates.
        | msg == Win32.wM_TIMER = do
          -- putStrLn "WM_TIMER"
          -- Handle errors gracefully.
          res <- safeTry updateIO
          case res of
            Left _ -> do putStrLn "Fran: Error while updating behaviors"
                         closeIO
                         return ()
            _      -> return ()
          return 0

        | msg >= {-Win32.-}wM_USER = do
          -- Try extension handler.
          -- Put back the "Win32." when wM_USER moves from HSpriteLib.gc
          -- to Win32WinMessage.gc.
          send (ExtAction msg wParam lParam)

        | otherwise =
          Win32.defWindowProc (Just hwnd) msg wParam lParam

      demoClass = Win32.mkClassName "Fran"

  in do
        icon <- Win32.loadIcon   Nothing Win32.iDI_APPLICATION
        cursor <- Win32.loadCursor Nothing Win32.iDC_ARROW
        blackBrush <- Win32.getStockBrush Win32.bLACK_BRUSH
        mainInstance <- Win32.getModuleHandle Nothing
        Win32.registerClass (
              0, -- Win32.emptyb, -- no extra redraw on resize
              mainInstance,
              (Just icon),
              (Just cursor),
              (Just blackBrush),
              Nothing,
              demoClass)

        --putStrLn "In makeWindow"
        w <- makeWindowNormal demoClass mainInstance wndProc2 mbMenu
        createIO w

        -- Set the (millisecond-based) update timer.
        Win32.setWinTimer w 1 (round (1000 * updateInterval))
        --putStrLn ("Update rate set for " ++ show updateInterval ++ " ms")

        Win32.showWindow w Win32.sW_SHOWNORMAL
        -- This next line doesn't work.  The first time a process creates
        -- a window, it is at the bottom.
        Win32.bringWindowToTop w

        return w

        -- Where should the window class get unregistered??
        -- (Win32.unregisterClass demoClass mainInstance `catch` \_ -> return ())

makeWindowNormal demoClass mainInstance wndProc2 mbMenu = do
  (sizeX,sizeY) <- map vector2XYCoords (readIORef initialViewSizeVar)
  let sizePixX = round (sizeX * screenPixelsPerLength)
      sizePixY = round (sizeY * screenPixelsPerLength)
  Win32.createWindow demoClass
               "Fran"
               Win32.wS_OVERLAPPEDWINDOW
               -- The next two are position.  Use Nothing to let Windows
               -- decide, and Just to specify explicitly, which is useful
               -- when recording.
               Nothing    Nothing
               --(Just 200) (Just (-extraH+6))
               (Just $ sizePixX + extraW)
               (Just $ sizePixY + extraH)
               Nothing                  -- parent
               mbMenu                   -- menu
               mainInstance
               wndProc2
 where
  -- Extra space for window border.  Should probably use AdjustWindowRect
  -- instead.
  extraW = 8
  extraH = extraW + 20

-- Get the width and height of a window's client area, in pixels.

getViewSize :: Win32.HWND -> IO (Win32.LONG,Win32.LONG)

getViewSize hwnd =
 Win32.getClientRect hwnd >>= \ (l',t',r',b') ->
 return (r' - l', b' - t')

-- Misc

updateRefStrict :: Eval a => IORef a -> (a -> a) -> IO ()

updateRefStrict var f =
  readIORef var >>= \ val ->
  -- Force evaluation of val, so computations don't pile up
  val `seq`
  writeIORef var (f val)

updatePeriodGoal :: SpriteTime
updatePeriodGoal = 0.1

-- ## Eliminate the t0 argument.

-- Show a sprite tree

showSpriteTree :: HSpriteTree -> IO () -> UserEventConsumer
               -> Win32.MbHMENU -> IO Win32.HWND

showSpriteTree spriteTree updateIO userEventConsumer mbMenu = do
  spriteEngineVar <- newIORef (error "spriteEngineVar not set")
  updateCountVar  <- newIORef (0::Int)
  windowVar       <- newIORef (error "windowVar not set")

  t0 <- currentSpriteTime
  makeWindow
     -- Create IO
     (\ w ->
       do writeIORef windowVar w
          eng <- newSpriteEngine w spriteTree
          writeIORef spriteEngineVar eng)
     -- Resize IO.  Recreates the back buffer and clippers.
     (do eng <- readIORef spriteEngineVar
         onResizeSpriteEngine eng)
     -- Update IO
     (do -- garbageCollect
         updateIO
         updateRefStrict updateCountVar (+1))
     -- Close IO
     (do --putStrLn "final updateIO"
         --updateIO  -- a last one for quit-based events
         eng   <- readIORef spriteEngineVar
         frameCount <- deleteSpriteEngine eng
         win   <- readIORef windowVar
         Win32.destroyWindow win
         -- Clean up
         deleteSpriteTree spriteTree
         -- Show performance stats
         updateCount <- readIORef updateCountVar
         showStats t0 frameCount updateCount )
     -- update interval in seconds
     updatePeriodGoal
     userEventConsumer
     mbMenu


-- To do: get frame count

showStats :: SpriteTime -> Int -> Int -> IO ()

showStats t0 frameCount updateCount =
 do t1 <- currentSpriteTime
    let dt = t1 - t0
    -- putStrLn (show dt ++ " seconds")
    putStrLn ""
    putStrLn (show dt ++ " seconds elapsed")
    putStrLn (show frameCount ++ " frames == " ++
              show (fromInt frameCount / dt) ++ " fps, " ++
              show (round (1000 * dt / fromInt frameCount)) ++
              " MS average")
    putStrLn (show updateCount ++ " updates == " ++
              show (fromInt updateCount / dt) ++ " ups, " ++
              show (round (1000 * dt / fromInt updateCount)) ++
              " MS average")


----------------------------------------------------------------
-- Program parameters
----------------------------------------------------------------

-- Initial window size.  Find a better way...

-- Given in Fran units, not pixels.  For instance, to exactly fit a unit
-- circle, use vector2XY 2 2.  Below, we include some extra space.

initialViewSizeVar :: IORef Vector2
initialViewSizeVar = Win32.unsafePerformIO $ newIORef $
                     (1 + extra) *^ vector2XY 2 2
 where
   extra = 0.1 -- breathing space
           -- 0  -- For ..\demo\ReplayTut.hs

setInitialViewSize :: RealVal -> RealVal -> IO ()
setInitialViewSize w h = writeIORef initialViewSizeVar (vector2XY w h)

withInitialViewSize :: RealVal -> RealVal -> IO a -> IO a
withInitialViewSize w h io =
  bracket_ (readIORef  initialViewSizeVar)
           (writeIORef initialViewSizeVar) $
  do writeIORef initialViewSizeVar (vector2XY w h)
     io
  
  
  

-- Version that acts on an existing window.  In practice, it seems
-- convenient for the previous one to take two reals and this one to take
-- a static vector.

-- Hey, silly me!!  This guy only tells the wndProc that the window was
-- resized.  It doesn't really do the resizing!  Instead, use the
-- MoveWindow Win32 function, first getting the current upper-left
-- position from GetWindowRect (which doesn't have a Win32 GC interface). 

setViewSize :: Win32.HWND -> Vector2 -> IO ()
setViewSize hwnd (Vector2XY w h) = do
  Win32.sendMessage hwnd Win32.wM_SIZE sIZE_MAXSHOW
                    (toPixel32 h * 65536 + toPixel32 w)
  return ()
