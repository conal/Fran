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
import IORef
import Monad (when)
import Channel (Channel, putChan)
import User
import ImageB (bitmapPixelsPerLength, screenPixelsPerLength)
import IOExtensions( garbageCollect )


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
	-- now.  On the other hand, saying now works well with the
	-- updateDone events generated in Spritify, which are said to be
	-- later but inserted earlier.
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
	   send (Key isDown (Win32.VKey wParam))
	   return 0

      wndProc2 hwnd msg wParam lParam

	| msg == Win32.wM_DESTROY =
	  do Win32.set_hugsQuitFlag True
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

      demoClass = Win32.mkClassName "Fran 3D"

  in do
	icon <- Win32.loadIcon   Nothing Win32.iDI_APPLICATION
	cursor <- Win32.loadCursor Nothing Win32.iDC_ARROW
	blackBrush <- Win32.getStockBrush Win32.bLACK_BRUSH
	mainInstance <- Win32.getModuleHandle Nothing
	Win32.registerClass (
	      Win32.emptyb, -- no extra redraw on resize
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
	putStrLn ""
	-- There should be a send Resize here
	Win32.eventLoop w
	(Win32.unregisterClass demoClass mainInstance `catch` \_ -> return ())


makeWindowNormal demoClass mainInstance wndProc2 =
 Win32.createWindow demoClass
              "Fran 3D"
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

updateRefStrict :: Eval a => Ref a -> (a -> a) -> IO ()

updateRefStrict ref f =
  getRef ref >>= \ val ->
  -- Force evaluation of val, so computations don't pile up
  val `seq`
  setRef ref (f val)


updatePeriodGoal :: SpriteTime
updatePeriodGoal = 0.1

-- Show a sprite tree

showSpriteTree :: HSpriteTree -> IO () -> UserChannel -> SpriteTime -> IO ()

showSpriteTree spriteTree updateIO userChan t0 =
 do spriteEngineVar <- newRef (error "dDrawEnvVar not set")
    updateCountVar  <- newRef (0::Int)
    frameCountRef   <- newRef 0
    windowVar	    <- newRef (error "windowVar not set")
    
    makeWindow
       -- Create IO
       (\ w ->
	 do setRef windowVar w
	    eng <- newSpriteEngine w spriteTree
	    setRef spriteEngineVar eng)
       -- Resize IO.  Recreates the back buffer and clippers.
       (do eng <- getRef spriteEngineVar
	   onResizeSpriteEngine eng)
       -- Update IO
       (do -- garbageCollect
           updateIO
	   updateRefStrict updateCountVar (+1))
       -- Close IO
       (do eng <- getRef spriteEngineVar
	   count <- deleteSpriteEngine eng
	   setRef frameCountRef count
	   win <- getRef windowVar
	   Win32.destroyWindow win)
       -- update interval in seconds
       updatePeriodGoal
       userChan

    -- Clean up
    deleteSpriteTree spriteTree
    -- Show performance stats
    updateCount <- getRef updateCountVar
    frameCount  <- getRef frameCountRef
    showStats t0 frameCount updateCount
    return ()


-- To do: get frame count

showStats :: SpriteTime -> Int -> Int -> IO ()

showStats t0 frameCount updateCount =
 do t1 <- currentSpriteTime
    let dt = t1 - t0 in
      do -- putStrLn (show dt ++ " seconds")
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



-- Testing.  Superceded by Spritify.hs

type STGen = Time -> SpriteTreeChain -> IO SpriteTreeChain

-- Ignores user input

disp :: STGen -> IO ()

disp stGen =
  do t0 <- currentSpriteTime
     (ignoredUser, userChan) <- newUser t0
     spriteTree <- stGen t0 emptySpriteTreeChain
     showSpriteTree spriteTree (return ()) userChan t0

------- Test cases --------


-- donutBmp = "c:\\Dxsdk\\sdk\\samples\\donuts\\donuts.bmp"
donutBmp = "..\\..\\Media\\donuts.bmp"

donutSurface :: HDDSurface
donutSurface = bitmapDDSurface donutBmp

donutFlipBook :: HFlipBook
donutFlipBook = flipBook donutSurface 64 64 0 0 5 6


donut :: Double -> Double -> Double -> Double -> STGen

donut velX velY scaleRate frameRate t0 rest =
  newFlipSprite donutFlipBook 0 0  1 1  0 rest     >>= \ flying ->
  setGoalPosition (toSprite flying) velX' velY' (t0+1)  >>
  setGoalScale (toSprite flying) scale' scale' (t0+1)  >>
  setGoalPage flying frameRate (t0+1)   >>
  return (toSpriteTree flying)
  where
   velX' = velX
   velY' = velY
   scale' = 1 + scaleRate

donut1, donut2, donut3, twoDonuts, threeDonuts :: STGen

donut1 = donut 0.40 0.35 0.0  50
donut2 = donut 0.50 0.45 0.2  70
donut3 = donut 0.45 0.40 0.5 100

-- [donut1, donut2, ...]
-- Could elide "rest" by using .>>=
twoDonuts t0 rest =
  donut2 t0 rest >>=
  donut1 t0

-- [donut3, [donut1, donut2], ...]
threeDonuts t0 rest =
  twoDonuts t0 emptySpriteTreeChain >>= \ two ->
  map toSpriteTree (newSpriteGroup two rest)  >>= \ group ->
  donut3 t0 group

noDonuts t0 rest = return rest




----------------------------------------------------------------
-- Program parameters
----------------------------------------------------------------

initialWindowSize = 300 :: Int
