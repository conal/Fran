-- Test the sprite engine from Haskell

module ShowImageB (
  updatePeriodGoal,
  showSpriteTree,
  initialWindowSize
  ) where

import HSpriteLib
import BaseTypes
import Transform2
import Vector2
import Point2
import qualified Win32
import MutVar		-- GSL for unified Var interface
import Monad (when)
import Channel (Channel, putChan)
import Event (EventChannel, newChannelEvent)
import User
import ImageB (importPixelsPerLength, screenPixelsPerLength)
import IOExtensions( garbageCollect )

type UserChannel = EventChannel UserAction

-- Window stuff

-- Make a window and route user events

makeWindow :: (Win32.HWND -> IO ())	-- create
	   -> IO ()			-- resize
	   -> IO ()			-- update
	   -> IO ()			-- close
	   -> SpriteTime		-- update goal interval
	   -> UserChannel		-- receives user events
	   -> IO ()


makeWindow createIO resizeIO updateIO closeIO
	   updateInterval userChan =
  let 
      send userEvent = do
        -- Get time now.  Bogus, since the event really happened before
	-- now.
        t <- currentSpriteTime
        --putStrLn ("User event " ++ show (t, userEvent))
        --putStr ("ue " ++ show t ++ " ")
        putChan userChan (t, Just userEvent)

      -- Map lParam mouse point to a Point2
      posn hwnd lParam =
	do (winWidth,winHeight) <- windowSize hwnd
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
		 point2XY (fromInt xRelWinCenter / screenPixelsPerLength)
			  (fromInt yRelWinCenter / -screenPixelsPerLength) )

      fireButtonEvent hwnd lParam isLeft isDown =
	do p <- posn hwnd lParam
	   --putStrLn ("Button " ++ show (isLeft,isDown) ++ " at pos " ++ show p)
	   send (Button isLeft isDown p)
	   return 0

      fireKeyEvent wParam isDown =
	do --putStrLn ("fireKeyEvent " ++ show (wParam, char, isDown, t))
	   send (Key isDown wParam) -- (Win32.VKey wParam)) -- GSL
	   return 0

      wndProc2 hwnd msg wParam lParam

	| msg == Win32.wM_DESTROY =
	  do -- Win32.set_hugsQuitFlag True -- GSL
	     send Quit
	     return 0

	| msg == Win32.wM_LBUTTONDOWN || msg == Win32.wM_LBUTTONDBLCLK =
	  fireButtonEvent hwnd lParam True True

	| msg == Win32.wM_LBUTTONUP =
	  fireButtonEvent hwnd lParam True False

	| msg == Win32.wM_RBUTTONDOWN || msg == Win32.wM_RBUTTONDBLCLK =
	  fireButtonEvent hwnd lParam False True

	| msg == Win32.wM_RBUTTONUP =
	  fireButtonEvent hwnd lParam False False

	| msg == Win32.wM_MOUSEMOVE =
	  do p <- posn hwnd lParam
	     send (MouseMove p)
	     return 0

	| msg == Win32.wM_KEYDOWN =
	  fireKeyEvent wParam True

	| msg == Win32.wM_KEYUP =
	  fireKeyEvent wParam False

	| msg == Win32.wM_SIZE =
	  do (x,y) <- map point2XYCoords (posn hwnd lParam)
	     -- putStrLn "Resized"
	     -- (x,y) is the lower-right corner, so we must flip y
	     -- and double both coordinates to get the width and height
	     send (Resize (vector2XY (2*x) (-2*y)))
	     resizeIO
	     return 0

	-- Timer.  Do the sprite tree updates.
	| msg == Win32.wM_TIMER =
	  do -- putStrLn "WM_TIMER"
	     -- putStrLn "Timer: sending UserNoOp"
	     -- send UserNoOp		-- filler
	     -- putStrLn "Timer: doing updateIO"
	     updateIO
	     return 0

	| msg == Win32.wM_CLOSE =
	  do -- putStrLn "WM_CLOSE"
	     closeIO
	     return 0

	| otherwise
	= Win32.defWindowProc (Just hwnd) msg wParam lParam

      demoClass = Win32.mkClassName "Fran"

      -- this is to replace the old Win32.eventLoop; GSL
      eventLoop :: Win32.HWND -> IO ()
      eventLoop hwnd = (do
	lpmsg <- Win32.getMessage (Just hwnd)
	Win32.translateMessage lpmsg
	Win32.dispatchMessage  lpmsg
	eventLoop hwnd
	) `catch` (\ _ -> return ())
	
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

	-- putStrLn "In makeWindow"
	w <- makeWindowNormal demoClass mainInstance wndProc2
	createIO w

	-- Set the millisecond timer.
	Win32.setWinTimer w 1 (round (1000 * updateInterval))
	-- putStrLn ("Update rate set for " ++ show updateInterval ++ " ms")

	Win32.showWindow w Win32.sW_SHOWNORMAL
	-- In Win95, this bringWindowToTop doesn't.
	Win32.bringWindowToTop w
	-- Strangely, if I don't print something before invoking
	-- ddraw functions, Hugs bombs on the AST notebook.  (12/6/96)
	--putStrLn ""
	-- There should be a send Resize here
	eventLoop w
	(Win32.unregisterClass demoClass mainInstance `catch` \_ -> return ())


makeWindowNormal demoClass mainInstance wndProc2 =
 Win32.createWindow demoClass
              "Fran"
              Win32.wS_OVERLAPPEDWINDOW
              -- Nothing    Nothing    -- x y
              (Just 300) (Just 26)
              (Just initialWindowSize) (Just initialWindowSize)
              Nothing               -- parent
              Nothing               -- menu
              mainInstance
              wndProc2


-- Get the width and height of a window's client area, in pixels.

windowSize :: Win32.HWND -> IO (Win32.LONG,Win32.LONG)

windowSize hwnd =
 Win32.getClientRect hwnd >>= \ (l',t',r',b') ->
 return (r' - l', b' - t')



-- Misc

updateRefStrict :: Eval a => MutVar a -> (a -> a) -> IO ()

updateRefStrict var f =
  readVar var >>= \ val ->
  -- Force evaluation of val, so computations don't pile up
  val `seq`
  writeVar var (f val)

{- GSL use MutVar instead

updateRefStrict :: Eval a => Ref a -> (a -> a) -> IO ()

updateRefStrict ref f =
  getRef ref >>= \ val ->
  -- Force evaluation of val, so computations don't pile up
  val `seq`
  setRef ref (f val)

-}

updatePeriodGoal :: SpriteTime
updatePeriodGoal = 0.1

-- ## Eliminate the t0 argument.

-- Show a sprite tree

showSpriteTree :: HSpriteTree -> IO () -> UserChannel -> IO ()

showSpriteTree spriteTree updateIO userChan =
 do spriteEngineVar <- newVar (error "spriteEngineVar not set")
    updateCountVar  <- newVar (0::Int)
    frameCountRef   <- newVar (error "frameCountRef not set")
    windowVar	    <- newVar (error "windowVar not set")
    
    t0 <- currentSpriteTime
    makeWindow
       -- Create IO
       (\ w ->
	 do writeVar windowVar w
	    eng <- newSpriteEngine w spriteTree
	    writeVar spriteEngineVar eng)
       -- Resize IO.  Recreates the back buffer and clippers.
       (do eng <- readVar spriteEngineVar
	   onResizeSpriteEngine eng)
       -- Update IO
       (do -- garbageCollect
           updateIO
	   updateRefStrict updateCountVar (+1))
       -- Close IO
       (do eng   <- readVar spriteEngineVar
	   count <- deleteSpriteEngine eng
	   writeVar frameCountRef count
	   win   <- readVar windowVar
	   Win32.destroyWindow win)
       -- update interval in seconds
       updatePeriodGoal
       userChan

    -- Clean up
    deleteSpriteTree spriteTree
    -- Show performance stats
    updateCount <- readVar updateCountVar
    frameCount  <- readVar frameCountRef
    showStats t0 frameCount updateCount
    return ()


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

initialWindowSize = 300 :: Int
