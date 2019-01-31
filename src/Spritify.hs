-- Mapping ImageB and SoundB to sprite trees
--
-- Here's the idea: instead of repeatedly sampling an image behavior into
-- static images and displaying each such image, break up the image
-- behavior into sprites, and repeatedly sample and "display" sprite
-- parameters behaviors.
--
-- Use "sprite normal form", which for ImageB is
--
--   crop rect $ mbWithColor mbColB $ transformI xfB $ timeTransform imB tt
--
-- where
-- 
--   mbWithColor Nothing  imb = imb
--   mbWithColor (Just c) imb = withColor c imb



module Spritify (displayUs, displayEx, eventLoop) where

import Monad (when, zipWithM_)
import BaseTypes
import qualified StaticTypes as S
import Event
import Behavior
import GBehavior
import BehaviorEvent
import Vector2B
import Point2B
import RectB
import ColorB
import VectorSpaceB
import ImageB
import SoundB
import User
import RenderImage (screenPixelsPerLength, importPixelsPerLength)
import Concurrent
import IOExts
import Maybe (isJust, fromMaybe)
import HSpriteLib
import ShowImageB
import Transform2B
import qualified Win32
-- Implementation notes:
-- 
-- The "ats" sampling function is memoized, so it is vital that
-- the sample time streams not be reconstructed.  For this reason, I keep a
-- separate boolean-valued request variable, which is in sync with the time
-- stream, instead of cutting off the sample time stream.


-- Spritifier.  The t0 and ts arguments are to real (SpriteLib) times.
-- Synchronize via request and reply synchronization variables.  Finally,
-- we have a sprite chain to build on top of.

type Spritifier = Time -> [Time]
               -> SyncVar -> SyncVar -> SpriteTreeChain
               -> IO SpriteTreeChain

-- Make a sprite tree chain.  Accumulate a sprite chain *above* the
-- current imageB and coloring an transformation behaviors.

spritifyImageB :: RectB -> Maybe ColorB -> Transform2B -> ImageB -> TimeB
               -> Spritifier

-- Empty image.  If there's nothing to do next, then just return the
-- request channel and above chain as the reply channel and chain.
spritifyImageB rect _ _ EmptyImage _ _ = \ _ requestV replyV above -> do
  forkIO $ forwardSyncVars requestV replyV
  return above

-- IMPORTANT NOTE: the lazy patterns are crucial in the "update" functions
-- here, because the heads are not available until the (takeMVar requestV)
-- succeeds.

spritifyImageB rectB mbColorB xfB SolidImage _ t0 =
  \ ts requestV replyV above -> do
  let (S.ColorRGB r0 g0 b0, colors) = ats0 colorB t0 ts
      (S.RectLLUR (S.Point2XY llx0 lly0) (S.Point2XY urx0 ury0), rects) =
         ats0 rectB t0 ts
  hMonochromeSprite <- newMonochromeSprite llx0 lly0 urx0 ury0 r0 g0 b0 above
  --putStrLn "Made new MonochromeSprite"
  let update ~(t:ts')
             ~(S.RectLLUR (S.Point2XY llx lly) (S.Point2XY urx ury) : rects')
             ~(S.ColorRGB r g b : colors') = do
         continue <- takeMVar requestV
         if continue then do
           updateMonochromeSprite hMonochromeSprite
                                  t llx lly urx ury r g b
           putMVar replyV True
           update ts' rects' colors'
          else
           putMVar replyV False
  forkIO $ update ts rects colors
  return (toSpriteTree hMonochromeSprite)
 where
   colorB = fromMaybe defaultColor mbColorB

spritifyImageB rectB _ xfB (FlipImage flipBook pageB) _ t0 =
  \ ts requestV replyV above -> do
  let (page0, pages) = ats0 pageB t0 ts
      (S.RectLLUR (S.Point2XY llx0 lly0) (S.Point2XY urx0 ury0), rects) =
         ats0 rectB t0 ts
      (S.Transform2 (S.Vector2XY motX0 motY0) scale0 _, xfs) = ats0 xfB t0 ts
      scaleAdjusted0 = scale0 * flipSpriteScaleAdjust
  hFlipSprite <- newFlipSprite flipBook llx0 lly0 urx0 ury0 motX0 motY0
                               scaleAdjusted0 scaleAdjusted0 page0 above
  --putStrLn "Made new flipSprite"
  let update ~(t:ts')
             ~(S.RectLLUR (S.Point2XY llx lly) (S.Point2XY urx ury) : rects')
             ~(S.Transform2 (S.Vector2XY motX motY) scale rotation : xfs')
             ~(page:pages') = do
         continue <- takeMVar requestV
         if continue then do
           when (rotation /= 0)	
             (putStrLn "Warning: ignoring non-zero rotation of FlipImage")
           let scaleAdjusted = scale * flipSpriteScaleAdjust
           updateFlipSprite hFlipSprite t
                            llx lly urx ury motX motY
             scaleAdjusted scaleAdjusted page
           putMVar replyV True
           update ts' rects' xfs' pages'
          else
           putMVar replyV False
  forkIO $ update ts rects xfs pages
  return (toSpriteTree hFlipSprite)

spritifyImageB rectB mbColorB xfB (RenderImage renderIO) tt t0 =
  \ ts requestV replyV above -> do
  -- Get the render threads going
  hSimpleSprite <- renderIO rectB mbColorB xfB tt t0 ts requestV replyV above
  return (toSpriteTree hSimpleSprite)

spritifyImageB rectB _ xfB (SoundI sound) tt t0 =
 -- Extract initial pan and volume from motion and scale
 spritifySoundB (abs scale) pan 1 sound tt t0
  where
    -- Pan based on x coordinate.  The 7.0 is empirical.
    pan = 7.0 * fst (vector2XYCoords motion)
    (motion, scale, _) = factorTransform2 xfB

spritifyImageB rectB mbColor xfB (imb1 `Over` imb2) tt t0 =
  \ ts requestV replyV above1 -> do
  -- Spritify the upper and then the lower.  Pass requests into imb1's
  -- sprite tree and use replies as request to imb2's sprite tree.  This
  -- threading is unnecessarily sequential, but it doesn't seem to hurt.
  syncV  <- newEmptyMVar
  above2 <- spritifyImageB rectB mbColor xfB imb1 tt t0
            ts requestV syncV above1  
  spritifyImageB rectB mbColor xfB imb2 tt t0 ts syncV replyV above2


-- Treatment of IransformI uses the following fact:
-- 
--     transformI xfB $
--     timeTransform (transformI xfB' imb) tt $
-- 
--       ==
-- 
--     transformI xfB $
--     transformI (timeTransform xfB' tt) $
--     timeTransform imb tt

spritifyImageB rectB mbColor xfB (TransformI xfB' imb) tt t0 =
  spritifyImageB rectB mbColor (xfB `compose2` timeTransform xfB' tt)
                 imb tt t0

-- withColor is similar

spritifyImageB rectB mbColor xfB (WithColorI colorInner imb) tt t0 =
  spritifyImageB rectB (mbColor ++ Just (timeTransform colorInner tt))
                 xfB imb tt t0

-- Cropping. Composes through intersection.

spritifyImageB rectB mbColor xfB (CropI rectBInner imb) tt t0 =
  spritifyImageB (rectB `intersectRect` transformedInner)
                 mbColor xfB imb tt t0
 where
   transformedInner = xfB *% timeTransform rectBInner tt


spritifyImageB rectB mbColor xfB (imb1 `UntilI` e) tt t0 =
  spritifyUntilB (\ (rectB,mbC,xfB) imb1 -> spritifyImageB rectB mbC xfB imb1)
                   (rectB, mbColor, xfB) imb1 e tt t0

-- Remember that timeTransform == (.), semantically.

spritifyImageB rectB mbColor xfB (TimeTransI imb ttInner) tt t0 =
  spritifyImageB rectB mbColor xfB imb (timeTransform ttInner tt) t0

flipSpriteScaleAdjust = screenPixelsPerLength / importPixelsPerLength

-- Generalized spritifier for untilB
spritifyUntilB :: (GBehavior bv, GBehavior ctx)
                 => (ctx -> bv -> TimeB -> Spritifier)
                 -> (ctx -> bv -> Event bv -> TimeB -> Spritifier)

spritifyUntilB spritify ctx bv1 e tt t0 =
  \ ts requestV replyV above -> do
  -- Fork a new thread that watches e and passes requests on to bv1's
  -- threads, but terminates when e occurs.  At that point, use requestV
  -- for the new bv.  (Verify !!)
  requestV1 <- newEmptyMVar                 -- request for bv1
  replyV1   <- newEmptyMVar                 -- reply   for bv1
  spriteChain1 <- spritify ctx bv1 tt t0 ts
                           requestV1 replyV1 emptySpriteTreeChain
  group  <- newSpriteGroup spriteChain1 above
  let xts = tt `ats` ts
      monitor ~ts@(t:ts') ~(xt:xts') ~(mbOcc:mbOccs') = do
        -- First pass on the next request to bv1's sprite tree threads,
        -- causing an update or termination.
        continue <- takeMVar requestV
        -- Continue bv1 sprite threads if the untilB is being continued
        -- and the event hasn't yet occurred.  Otherwise terminate by
        -- putting a False into the request channel
        putMVar requestV1 (continue && not (isJust mbOcc))
        takeMVar replyV1
        if not continue then
          putMVar replyV False
         else
          -- Now check the event
          case mbOcc of
            Nothing -> do
              -- Non-occurrence.  Report one step done, and repeat.
              putMVar replyV True
              monitor ts' xts' mbOccs'
            Just (xte, (bv2, ctx')) -> do
              -- Don't report one step done.  Instead, spritify bv2 and
              -- start updating it.
              -- Note: using the untilB's request and reply channels
              -- here, so we don't have to forward requests.  Otherwise
              -- with recursive reactive behaviors (e.g., produced by
              -- accumB and friends), the layers of forwarding would pile
              -- up.  Since we took the request and haven't fulfilled it,
              -- we first have to put the request back.
              putMVar requestV True
              -- ### What update time to use???  We just know the sample
              -- time of the event.  Heuristic pseudo-solution: if the
              -- first sample and update times are equal, use xte for te.
              -- Otherwise use t, which is a little later than
              -- appropriate.  Doing this correctly requires inverting the
              -- time transform, since we must map from local event time
              -- to global event time.
              let te | t==xt     = xte
                     | otherwise = t
              spriteChain2 <- spritify ctx' bv2 tt te ts
				       requestV replyV emptySpriteTreeChain
              resetSpriteGroup group spriteChain2 False
  -- Avoid a space-time leak here: Don't hang onto the
  -- original color, motion and scale.  Let them age.
  forkIO $ monitor ts xts ((e `afterE` ctx) `occs` xts)
  return (toSpriteTree group)

-- Similar to spritifyImageB, but on SoundB
spritifySoundB :: RealB -> RealB -> RealB -> SoundB -> TimeB -> Spritifier

spritifySoundB _ _ _SilentS _ _ = \ _ requestV replyV above -> do
  forkIO $ forwardSyncVars requestV replyV
  return above


spritifySoundB volB panB pitchB (BufferS buff) tt t0 =
  \ ts requestV replyV above -> do
  let (vol0  , vols   ) = ats0 volB   t0 ts
      (pan0  , pans   ) = ats0 panB   t0 ts
      (pitch0, pitches) = ats0 pitchB t0 ts
  hSoundSprite <- newSoundSprite buff vol0 pan0 pitch0 above
  --putStrLn "Made new SoundSprite" >>
  let update ~(t:ts') ~(vol:vols') ~(pan:pans') ~(pitch:pitches') = do
         continue <- takeMVar requestV
         if continue then do
           updateSoundSprite hSoundSprite t vol pan pitch
           putMVar replyV True
           update ts' vols' pans' pitches'
          else
           putMVar replyV False
  forkIO $ update ts vols pans pitches
  return (toSpriteTree hSoundSprite)


spritifySoundB volB panB pitchB (sound1 `MixS` sound2) tt t0 =
  \ ts requestV replyV above1 -> do
  -- Like `Over`
  syncV  <- newEmptyMVar
  above2 <- spritifySoundB volB panB pitchB sound1 tt t0
            ts requestV syncV above1  
  spritifySoundB volB panB pitchB sound2 tt t0 ts syncV replyV above2

-- For VolumeS, PanS, and PitchS, see comments before spritifyImageB of
-- TransformI.

spritifySoundB volB panB pitchB (VolumeS v sound') tt t0 =
  spritifySoundB (volB * timeTransform v tt) panB pitchB sound' tt t0

-- Pan is in dB and so combines additively.  Is this best??
spritifySoundB volB panB pitchB (PanS p sound') tt t0 =
  spritifySoundB volB (panB + timeTransform p tt) pitchB sound' tt t0

spritifySoundB volB panB pitchB (PitchS p sound') tt t0 =
  spritifySoundB volB panB (pitchB * timeTransform p tt) sound' tt t0


spritifySoundB volB panB pitchB (sound1 `UntilS` e) tt t0 =
  spritifyUntilB (\ (volB,panB,pitchB) sound ->
                     spritifySoundB volB panB pitchB sound)
                 (volB,panB,pitchB) sound1 e tt t0

spritifySoundB volB panB pitchB (TimeTransS sound ttInner) tt t0 =
  spritifySoundB volB panB pitchB sound
                 (timeTransform ttInner tt) t0 


-- Display.

displayUs :: [User -> ImageB] -> IO ()
displayUs imFs = do
  -- Make a list of windows, each with a state variable saying that it is
  -- still running.
  ws <- mapM (\ imF -> displayEx (\u -> (imF u, \ w -> neverE))) imFs
  eventLoops ws

eventLoops :: [Win32.HWND] -> IO ()
eventLoops [w] = eventLoop w
eventLoops ws  = do
  runningVars <- mapM (const (newIORef True)) ws
  -- Keep track of how many are still runnning.  When none, stop
  runningNumVar <- newIORef (length ws)
  let oneEvent w runningVar = do
        running <- readIORef runningVar
        if running then (do
           lpmsg <- Win32.getMessage (Just w)
           Win32.translateMessage lpmsg
           Win32.dispatchMessage  lpmsg
           return ()
          ) `catch` \ _ -> do writeIORef runningVar False
                              --putStrLn "One window stopped"
                              updateRef runningNumVar (subtract 1)
         else return ()
      -- Loop through them all until none remain.
      allEvents = do
        n <- readIORef runningNumVar
        if n > 0 then do
          zipWithM_ oneEvent ws runningVars
          allEvents
         else
          return ()
  allEvents
 where
   updateRef var f = do
     val <- readIORef var
     writeIORef var (f val)


-- Run a window's event loop until it aborts.  Useful in conjunction with
-- displayEx.
eventLoop :: Win32.HWND -> IO ()
eventLoop w = loop `catch` \ _ -> return ()
 where loop = do
         lpmsg <- Win32.getMessage (Just w)
         Win32.translateMessage lpmsg
         Win32.dispatchMessage  lpmsg
         loop

-- Generalized version.  The argument produces not only an ImageB, but
-- also a function from the created window to an effect-valued event.
-- Upon each event occurence, execute the corresponding action.
--
-- Note: using fixIO, one can do without the window argument to the
-- effect-producer, but it's scary looking:
-- 
--   main = mkWindow >>= eventLoop
--    where
--    mkWindow = fixIO $ \ w -> do
--      let imF u = (circle, changeTitle)
--           where
--             changeTitle = (lbp u -=> "left" .|. rbp u -=> "right")
--                              ==> setWindowTextA' w
--      displayEx imF 


displayEx :: (User -> (ImageB, Win32.HWND -> Event (IO ())))
          -> IO Win32.HWND
displayEx imF {-mbMenu-} = do
  let mbMenu = Nothing  -- out for now
  -- garbageCollect
  t0 <- currentSpriteTime
  --putStrLn ("doing spritify for time " ++ show t0)
  (userEv, userActionsChan) <- newChannelEvent t0

  initWinSize <- readIORef initialViewSizeVar

  -- We might like to find out where the mouse is relative to the view,
  -- but we cannot until making the window.
  -- initMousePos <- getCursorPos ...
  -- Instead, start the mouse outside of the view
  let initMousePos = S.origin2 S..+^ 10 S.*^ initWinSize

  let user = makeUser False False initMousePos initWinSize 0.1 userEv
      (imB, effectsF) = imF user
  timeChan <- newChan
  ts       <- getChanContents timeChan
  -- Initialize SpriteLib.  I wish this could be done in SpriteLib's
  -- DllMain, but DSound initialization bombs there.  Doing it here is
  -- shaky, as it relies on the DDSurface and DSBuffer evaluations being
  -- postponed due to laziness.
  openSpriteLib screenPixelsPerLength
  --set_ddhelpTimeTrace True           -- default is False
  requestV <- newEmptyMVar
  replyV   <- newEmptyMVar
  -- Initially the update and sample times are the same, but we might
  -- want to make the sample times be later.
  let (halfWidth,halfHeight) = vector2XYCoords (0.5 *^ viewSize user)
      cropRect = rectFromCorners (point2XY (-halfWidth) (-halfHeight))
                                 (point2XY   halfWidth    halfHeight )
  chain    <- spritifyImageB cropRect Nothing identity2 imB time
              t0 ts requestV replyV emptySpriteTreeChain
  --putStrLn "Spritify done"

  tPrevVar   <- newIORef t0
  fixIO $ \ hWnd -> do
    let effects = effectsF hWnd
    effectsVar <- newIORef effects
    let tick = do
          tNow <- currentSpriteTime
          -- Prepare for sprite tree updating

          -- The benefit of the first choice, which distinguishes these
          -- two times, is that external event times then arrive
          -- monotonically.  On the other hand, this monotonicity is not
          -- genuine, because the time associated with an external event
          -- is the processing, not occurrence, time.
          --let {tSample = tNow; tUpdate = tNow + updatePeriodGoal}
          let {tSample = tUpdate; tUpdate = tNow + updatePeriodGoal}
          --putStrLn ("tick " ++ show tSample)
          -- Add sample time to ts
          writeChan timeChan tSample
          -- Add the update event for tSample.  This will stop the event
          -- search done in doEffects and the updating triggered by putMVar
          -- requestV below, which are only interested in events *before*
          -- tNow.
          tPrev <- readIORef tPrevVar
          writeChan userActionsChan
                    (tSample, Just (UpdateDone (tSample - tPrev)))
          -- Do effects
          updateIORef (doEffects tNow) effectsVar
          -- Request a round of updates from the sprite threads and event
          -- detector threads.
          putMVar requestV True
          -- Wait for them to finish one step
          _ <- takeMVar replyV
          writeIORef tPrevVar tSample
    showSpriteTree chain tick
                   (\ t act -> writeChan userActionsChan (t, Just act))
                   mbMenu



-- Do all of the external effects before t and return residual
doEffects :: Time -> Event (IO ()) -> IO (Event (IO ()))
doEffects t (Event possOccs) = loop possOccs
 where
   loop [] = return neverE
   loop possOccs@((te, mb) : possOccs') = 
     if t <= te then
       -- remaining possible occurrences are in the future
       return (Event possOccs)
     else do
       -- Execute the action if it's there.  I bet there's a more monadic
        -- way to write this, without the case
       case mb of
         Nothing -> return ()
         Just io -> io
       -- and look for more
       loop possOccs'
       

-- Convenient utility.
updateIORef :: (a -> IO a) -> IORef a -> IO ()
updateIORef updater ref = do
  a  <- readIORef ref
  a' <- updater a
  writeIORef ref a'