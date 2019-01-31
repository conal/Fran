-- Mapping ImageB and SoundB to sprite trees
--
--   Last modified Mon Oct 13 11:49:06 1997 by conal
--
-- Here's the idea: instead of repeatedly sampling an image behavior into
-- static images and displaying each such image, break up the image
-- behavior into sprites, and repeatedly sample and "display" sprite
-- parameters behaviors.


module Spritify (disp) where

import Monad(when)
import BaseTypes
import qualified StaticTypes as S
import Event
import Behavior
import BehaviorEvent
import Vector2B
import ColorB
import VectorSpaceB
import ImageB
import SoundB
import User
import Interaction
import Concurrent			-- MVar
import Channel
import MutVar
import Trace
import Maybe (isJust, fromMaybe)
import HSpriteLib
import ShowImageB
import Transform2B

-- Implementation notes:
-- 
-- The "ats" sampling function is memoized, so it is vital that
-- the sample time streams not be reconstructed.  For this reason, I keep a
-- separate boolean-valued request variable, which is in sync with the time
-- stream, instead of cutting off the sample time stream.


-- Spritifier.  There are two start times, t0 and xt0, corresponding to
-- real (spriteLib) time and sample time.  These two may differ due to
-- intentional latency, or to time transformation.  Similarly, there are
-- two times streams, "ts" for a list of update times and "xts" a list of
-- sample times.  Synchronize via request and reply synchronization
-- variables.  Finally, we have a sprite chain to build on top of.

type Spritifier = Time -> Time -> [Time] -> [Time]
               -> SyncVar -> SyncVar -> SpriteTreeChain
               -> IO SpriteTreeChain

-- Make a sprite tree chain.  Accumulate a sprite chain *above* the
-- current imageB and coloring an transformation behaviors.

spritifyImageB :: Maybe ColorB -> Transform2B -> ImageB -> Spritifier

-- Empty image.  If there's nothing to do next, then just return the
-- request channel and above chain as the reply channel and chain.
spritifyImageB _ _ EmptyImage _ _ = \ _ _ requestV replyV above -> do
  forkIO $ forwardSyncVars requestV replyV
  return above

-- IMPORTANT NOTE: the lazy patterns are crucial in the "update" functions
-- here, because the heads are not available until the (takeMVar requestV)
-- succeeds.

spritifyImageB mbColorB xfB SolidImage t0 xt0 =
  \ ts xts requestV replyV above -> do
  hMonochromeSprite <- newMonochromeSprite r0 g0 b0 above
  --putStrLn "Made new MonochromeSprite"
  let update ~(t:ts') ~(S.ColorRGB r g b : colors') = do
         continue <- takeMVar requestV
         if continue then do
           updateMonochromeSprite hMonochromeSprite t r g b
           putMVar replyV True
           update ts' colors'
          else
           putMVar replyV False
  forkIO $ update ts (colorB `ats` xts)
  return (toSpriteTree hMonochromeSprite)
 where
   colorB = fromMaybe defaultColor mbColorB
   S.ColorRGB r0 g0 b0 : _ = colorB `ats` [xt0]

spritifyImageB mbColorB xfB (FlipImage flipBook pageB) t0 xt0 =
  \ ts xts requestV replyV above -> do
  hFlipSprite <- newFlipSprite flipBook motX0 motY0
                     scaleAdjusted0 scaleAdjusted0 page0 above
  --putStrLn "Made new flipSprite"
  let update ~(t:ts')
             ~(S.Transform2 (S.Vector2XY motX motY) scale rotation : xfs')
             ~(page:pages') = do
         continue <- takeMVar requestV
         if continue then do
           when (rotation /= 0)	
             (putStrLn "Warning: ignoring non-zero rotation of FlipImage")
           let scaleAdjusted = scale * scaleAdjust
           updateFlipSprite hFlipSprite t motX motY
             scaleAdjusted scaleAdjusted page
           putMVar replyV True
           update ts' xfs' pages'
          else
           putMVar replyV False
  forkIO $ update ts (xfB `ats` xts) (pageB `ats` xts)
  return (toSpriteTree hFlipSprite)
 where
   scaleAdjust = screenPixelsPerLength / importPixelsPerLength
   S.Transform2 (S.Vector2XY motX0 motY0) scale0 _ : _ = xfB `ats` [xt0]
   scaleAdjusted0 = scale0 * scaleAdjust
   page0 : _ = pageB `ats` [xt0]

spritifyImageB mbColorB xfB (RenderImage renderIO) t0 xt0 =
  \ ts xts requestV replyV above -> do
  -- Get the render threads going
  hSimpleSprite <- renderIO mbColorB xfB t0 xt0 ts xts requestV replyV above
  return (toSpriteTree hSimpleSprite)
 where
   S.Transform2 (S.Vector2XY ulX0 ulY0) scale0 _ : _ = xfB `ats` [xt0]

spritifyImageB _ xfB (SoundI sound) t0 xt0 =
 -- Extract initial pan and volume from motion and scale
 spritifySoundB scale pan 1 sound t0 xt0
  where
    -- Pan based on x coordinate.  The 0.1 is empirical.
    pan = 0.1 * fst (vector2XYCoords motion)
    motion = lift1 (\ (S.Transform2 m _ _) -> m) xfB
    scale =  lift1 (\ (S.Transform2 _ s _) -> s) xfB

spritifyImageB mbColor xfB (imb1 `Over` imb2) t0 xt0 =
  \ ts xts requestV replyV above1 -> do
  -- Spritify the upper and then the lower.  Pass requests into imb1's
  -- sprite tree and use replies as request to imb2's sprite tree.  This
  -- threading is unnecessarily sequential, but it doesn't seem to hurt.
  syncV  <- newMVar
  above2 <- spritifyImageB mbColor xfB imb1 t0 xt0 ts xts requestV syncV above1  
  spritifyImageB mbColor xfB imb2 t0 xt0 ts xts syncV replyV above2

spritifyImageB mbColor xfB (TransformI xfB' imb) t0 xt0 =
  spritifyImageB mbColor (xfB `compose2` xfB') imb t0 xt0

spritifyImageB mbColor xfB (WithColor colorInner imb) t0 xt0 =
  spritifyImageB (mbColor ++ Just colorInner) xfB imb t0 xt0

spritifyImageB mbColor xfB (imb1 `UntilI` e) t0 xt0 =
  spritifyUntilB (\ (mbC,xfB) imb1 -> spritifyImageB mbC xfB imb1)
                   (mbColor, xfB) imb1 e t0 xt0

spritifyImageB mbColor xfB (TimeTransI imb tt) t0 xt0 = \ ts xts ->
  spritifyImageB mbColor xfB imb t0 (head (tt `ats` [xt0])) ts (tt `ats` xts)



-- Generalized spritifier for untilB
spritifyUntilB :: (GBehavior bv, GBehavior ctx)
                 => (ctx -> bv -> Spritifier)
                 -> ctx -> bv -> Event bv -> Spritifier

spritifyUntilB spritify ctx bv1 e t0 xt0 =
  \ ts xts requestV replyV above -> do
  -- Fork a new thread that watches e and passes requests on to bv1's
  -- threads, but terminates when e occurs.  At that point, use requestV
  -- for the new bv.  (Verify !!)
  requestV1 <- newMVar                 -- request for bv1
  replyV1   <- newMVar                 -- reply   for bv1
  spriteChain1 <- spritify ctx bv1 t0 xt0 ts xts
                           requestV1 replyV1 emptySpriteTreeChain
  group  <- newSpriteGroup spriteChain1 above
  let monitor ~ts@(t:ts') ~xts@(xt:xts') ~(mbOcc:mbOccs') = do
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
              -- appropriate.
              let te | t==xt     = xte
                     | otherwise = t
              spriteChain2 <- spritify ctx' bv2 te xte ts xts
				       requestV replyV emptySpriteTreeChain
              resetSpriteGroup group spriteChain2 False
  -- Avoid a space-time leak here: Don't hang onto the
  -- original color, motion and scale.  Let them age.
  forkIO $ monitor ts xts ((e `afterE` ctx) `occs` xts)
  return (toSpriteTree group)


-- Similar to spritifyImageB, but on SoundB
spritifySoundB :: RealB -> RealB -> RealB -> SoundB -> Spritifier


spritifySoundB _ _ _SilentS _ _ = \ _ _ requestV replyV above -> do
  forkIO $ forwardSyncVars requestV replyV
  return above


spritifySoundB volB panB pitchB (BufferS buff) t0 xt0 =
  \ ts xts requestV replyV above -> do
  hSoundSprite <- newSoundSprite buff vol0 pan0 pitch0 above
  --putStrLn "Made new SoundSprite" >>
  let update ~(t:ts') ~(volume:volumes')
             ~(pan:pans') ~(pitch:pitches') = do
         continue <- takeMVar requestV
         if continue then do
           updateSoundSprite hSoundSprite t volume pan pitch
           putMVar replyV True
           update ts' volumes' pans' pitches'
          else
           putMVar replyV False
  forkIO $ update ts (volB `ats` xts) (panB `ats` xts) (pitchB `ats` xts)
  return (toSpriteTree hSoundSprite)
 where
   vol0   : _ = volB   `ats` [xt0]
   pan0   : _ = panB   `ats` [xt0]
   pitch0 : _ = pitchB `ats` [xt0]


spritifySoundB volB panB pitchB (sound1 `MixS` sound2) t0 xt0 =
  \ ts xts requestV replyV above1 -> do
  -- Like `Over`
  syncV  <- newMVar
  above2 <- spritifySoundB volB panB pitchB sound1 t0 xt0
            ts xts requestV syncV above1  
  spritifySoundB volB panB pitchB sound2 t0 xt0 ts xts syncV replyV above2

spritifySoundB volB panB pitchB (VolumeS v sound') t0 xt0 =
  spritifySoundB (volB * v) panB pitchB sound' t0 xt0

spritifySoundB volB panB pitchB (PitchS p sound') t0 xt0 =
  spritifySoundB volB panB (pitchB * p) sound' t0 xt0


spritifySoundB volB panB pitchB (sound1 `UntilS` e) t0 xt0 =
  spritifyUntilB (\ (volB,panB,pitchB) sound ->
                     spritifySoundB volB panB pitchB sound)
                 (volB,panB,pitchB) sound1 e t0 xt0

spritifySoundB volB panB pitchB (TimeTransS sound tt) t0 xt0 = \ ts xts ->
  spritifySoundB volB panB pitchB sound
                t0 (head (tt `ats` [xt0])) ts (tt `ats` xts)


-- Display

-- primitive garbageCollect "primGC" :: IO ()

disp :: (User -> ImageB) -> IO ()

-- Note: partway into modeling GC's effect on timing.

disp imF =
 do -- garbageCollect
    t0 <- currentSpriteTime
    --putStrLn ("doing spritify for time " ++ show t0)
    (user, userChan) <- newChannelEvent t0
    -- Extract the time sequence.
    timeChan <- newChan
    ts       <- getChanContents timeChan
    -- Initialize SpriteLib.  I wish this could be done in SpriteLib's
    -- DllMain, but DSound initialization bombs there.  Doing it here is
    -- shaky, as it relies on the DDSurface and DSBuffer evaluations being
    -- postponed due to laziness.
    openSpriteLib screenPixelsPerLength
    --set_ddhelpTimeTrace True           -- default is False
    requestV <- newMVar
    replyV   <- newMVar
    -- Initially the update and sample times are the same, but we might
    -- want to make the sample times be later.
    chain    <- spritifyImageB Nothing identity2 (imF user)
                t0 t0 ts ts requestV replyV emptySpriteTreeChain
    --putStrLn "Spritify done"

    tPrevVar <- newVar t0
    let tick = do
          tNow <- currentSpriteTime
          tPrev <- readVar tPrevVar
          -- The benefit of the first choice, which distinguishes these
          -- two times, is that external event times then arrive
          -- monotonically.  On the other hand, this monotonicity is not
          -- genuine, because the time associated with an external event
          -- is the processing, not occurrence, time.
          --let {tSample = tNow; tUpdate = tNow + updatePeriodGoal}
          let {tSample = tUpdate; tUpdate = tNow + updatePeriodGoal}
          --putStrLn ("tick " ++ show tSample)
          -- Add sample time to ts
          putChan timeChan tSample
          -- Add the update event for tSample.  This will stop the event
          -- search done by doUpdateTrees below, which is only interested
          -- in events *before* tNow.
          putChan userChan (tSample, Just (UpdateDone (tSample - tPrev)))
          -- Request a round of updates from the sprite threads and event
          -- detector threads.
          putMVar requestV True
          -- Wait for them to finish one step
          _ <- takeMVar replyV
          writeVar tPrevVar tSample
    showSpriteTree chain tick userChan
