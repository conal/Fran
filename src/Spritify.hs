-- Mapping ImageB and SoundB to sprite trees
--
--   Last modified Fri Oct 03 11:05:24 1997 by conal
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
import MVar
import Channel
import MutVar
import Trace
import Maybe (isJust)
import HSpriteLib
import ShowImageB
import Transform2B

-- Implementation notes:
-- 
-- The "ats" sampling function is memoized, so it is vital that
-- the sample time streams not be reconstructed.  For this reason, I keep a
-- separate boolean-valued request variable, which is in sync with the time
-- stream, instead of cutting off the sample time stream.


spritify :: ImageB -> Time -> Time -> [Time] -> [Time] -> SyncVar -> SyncVar
         -> IO SpriteTreeChain

spritify imb t0 xt0 ts xts requestV replyV = do
  --putStrLn ("Starting to spritify " ++ show imb)
  spritifyRec Nothing identity2 imb t0 xt0 ts xts
              requestV replyV emptySpriteTreeChain

-- Make an UpdateTree list and a sprite chain.  Accumulate a sprite chain
-- *above* the current imageB and coloring an transformation behaviors.
-- There are two start times, t0 and xt0, corresponding to real
-- (spriteLib) time and sample time.  These two may differ due to
-- intentional latency, or to time transformation.  Similarly, there are
-- two times streams, "ts" for a list of update times and "xts" a list of
-- sample times.  Synchronize via request and reply synchronization
-- variables.

spritifyRec :: Maybe ColorB -> Transform2B -> ImageB -> Time -> Time
            -> [Time] -> [Time] -> SyncVar -> SyncVar -> SpriteTreeChain
            -> IO SpriteTreeChain

-- Empty image.  If there's nothing to do next, then just return the
-- request channel and above chain as the reply channel and chain.
spritifyRec _ _ EmptyImage _ _ = \ _ _ requestV replyV above -> do
  forkIO $ forwardSyncVars requestV replyV
  return above

-- IMPORTANT NOTE: the lazy patterns are crucial in the "update" functions
-- here, because the heads are not available until the (takeMVar requestV)
-- succeeds.

spritifyRec mbColorB xfB (FlipImage flipBook pageB) t0 xt0 =
  \ ts xts requestV replyV above -> do
  hFlipSprite <- newFlipSprite flipBook
                     (motX0 - halfBookW * absScale0)
                     (motY0 + halfBookH * absScale0)
                     scale0 scale0 page0 above
  --putStrLn "Made new flipSprite"
  let update ~(t:ts')
             ~(S.Transform2 (S.Vector2XY motX motY) scale rotation : xfs')
             ~(page:pages') = do
         continue <- takeMVar requestV
         if continue then do
           when (rotation /= 0)	
             (putStrLn "Warning: ignoring non-zero rotation of FlipImage")
           let scaleAdjusted = scale * scaleAdjust
               absScale      = abs scale
           updateFlipSprite hFlipSprite t
             (motX - halfBookW * absScale) (motY + halfBookH * absScale)
             scaleAdjusted scaleAdjusted
             page
           putMVar replyV True
           update ts' xfs' pages'
          else
           putMVar replyV False
  forkIO $ update ts (xfB `ats` xts) (pageB `ats` xts)
  return (toSpriteTree hFlipSprite)
 where
   (bookWPix,bookHPix) = flipBookSize flipBook
   halfBookW = fromInt bookWPix / importPixelsPerLength / 2
   halfBookH = fromInt bookHPix / importPixelsPerLength / 2
   -- See comment in RenderImage.hs
   scaleAdjust = screenPixelsPerLength / importPixelsPerLength
   S.Transform2 (S.Vector2XY motX0 motY0) scale0 _ : _ = xfB `ats` [xt0]
   absScale0 = abs scale0
   page0 : _ = pageB `ats` [xt0]

spritifyRec mbColorB xfB (RenderImage renderIO) t0 xt0 =
  \ ts xts requestV replyV above -> do
  -- Get the render threads going
  hSimpleSprite <- renderIO mbColorB xfB t0 xt0 ts xts requestV replyV above
  return (toSpriteTree hSimpleSprite)
 where
   S.Transform2 (S.Vector2XY ulX0 ulY0) scale0 _ : _ = xfB `ats` [xt0]

spritifyRec _ xfB (SoundI sound) t0 xt0 =
 -- Extract initial pan and volume from motion and scale
 spritifySoundRec scale pan 1 sound t0 xt0
  where
    -- Pan based on x coordinate.  The 0.1 is empirical.
    pan = 0.1 * fstB (vector2XYCoords motion)
    motion = lift1 (\ (S.Transform2 m _ _) -> m) xfB
    scale =  lift1 (\ (S.Transform2 _ s _) -> s) xfB

spritifyRec mbColor xfB (imb1 `Over` imb2) t0 xt0 =
  \ ts xts requestV replyV above1 -> do
  -- Spritify the upper and then the lower.  Pass requests into imb1's
  -- sprite tree and use replies as request to imb2's sprite tree.  This
  -- threading is unnecessarily sequential, but it doesn't seem to hurt.
  syncV  <- newMVar
  above2 <- spritifyRec mbColor xfB imb1 t0 xt0 ts xts requestV syncV above1  
  spritifyRec mbColor xfB imb2 t0 xt0 ts xts syncV replyV above2

spritifyRec mbColor xfB (TransformI xfB' imb) t0 xt0 =
  spritifyRec mbColor (xfB `compose2` xfB') imb t0 xt0

spritifyRec mbColor xfB (WithColor colorInner imb) t0 xt0 =
  spritifyRec (mbColor ++ Just colorInner) xfB imb t0 xt0

spritifyRec mbColor xfB (imb1 `UntilI` e) t0 xt0 =
  \ ts xts requestV replyV above -> do
  -- Fork a new thread that watches e and passes requests on to imb1's
  -- threads, but terminates when e occurs.  At that point, use requestV
  -- for the new imb.  (Verify !!)
  requestV1 <- newMVar                 -- request for imb1
  replyV1   <- newMVar                 -- reply   for imb1
  spriteChain1 <- spritifyRec mbColor xfB imb1 t0 xt0 ts xts
                              requestV1 replyV1 emptySpriteTreeChain
  group  <- newSpriteGroup spriteChain1 above
  let monitor ~ts@(t:ts') ~xts@(xt:xts') ~(mbOcc:mbOccs') = do
        -- First pass on the next request to imb1's sprite tree threads,
        -- causing an update or termination.
        continue <- takeMVar requestV
        -- Continue imb1 sprite threads if the UntilI is being continued
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
            Just (xte, (imb2, (mbColor', xfB'))) -> do
              -- Don't report one step done.  Instead, spritify imb2 and
              -- start updating it.
              -- Note: using the untilI's request and reply channels
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
              spriteChain2 <- spritifyRec mbColor' xfB' imb2 te xte ts xts
					  requestV replyV emptySpriteTreeChain
              resetSpriteGroup group spriteChain2 False
  -- Avoid a space-time leak here: Don't hang onto the
  -- original color, motion and scale.  Let them age.
  forkIO $ monitor ts xts ((e `afterE` (mbColor, xfB)) `occs` xts)
  return (toSpriteTree group)

spritifyRec mbColor xfB (TimeTransI imb tt) t0 xt0 = \ ts xts ->
  spritifyRec mbColor xfB imb t0 (head (tt `ats` [xt0])) ts (tt `ats` xts)



-- Similar to spritifyRec, but on SoundB
spritifySoundRec :: RealB -> RealB -> RealB -> SoundB -> Time -> Time
                 -> [Time] -> [Time] -> SyncVar -> SyncVar -> SpriteTreeChain
                 -> IO SpriteTreeChain


spritifySoundRec _ _ _SilentS _ _ = \ _ _ requestV replyV above -> do
  forkIO $ forwardSyncVars requestV replyV
  return above


spritifySoundRec volB panB pitchB (BufferS buff) t0 xt0 =
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


spritifySoundRec volB panB pitchB (sound1 `MixS` sound2) t0 xt0 =
  \ ts xts requestV replyV above1 -> do
  -- Like `Over`
  syncV  <- newMVar
  above2 <- spritifySoundRec volB panB pitchB sound1 t0 xt0
            ts xts requestV syncV above1  
  spritifySoundRec volB panB pitchB sound2 t0 xt0 ts xts syncV replyV above2

spritifySoundRec vol pan pitch (VolumeS v sound') t0 xt0 =
  spritifySoundRec (vol * v) pan pitch sound' t0 xt0

spritifySoundRec vol pan pitch (PitchS p sound') t0 xt0 =
  spritifySoundRec vol pan (pitch * p) sound' t0 xt0



-- ## Badly redundant with UntilI.  To do: Abstract!!
spritifySoundRec volB panB pitchB (sound1 `UntilS` e) t0 xt0 =
  \ ts xts requestV replyV above -> do
  -- Fork a new thread that watches e and passes requests on to sound1's
  -- threads, but terminates when e occurs.  At that point, use requestV
  -- for the new imb.  (Verify !!)
  requestV1 <- newMVar                 -- request for sound1
  replyV1   <- newMVar                 -- reply   for sound1
  spriteChain1 <- spritifySoundRec volB panB pitchB sound1 t0 xt0
                      ts xts requestV1 replyV1 emptySpriteTreeChain
  group  <- newSpriteGroup spriteChain1 above
  let monitor ~ts@(t:ts') ~xts@(xt:xts') ~(mbOcc:mbOccs') = do
        -- First pass on the next request to sound1's sprite tree threads,
        -- causing an update or termination.
        continue <- takeMVar requestV
        -- Continue sound1 sprite threads if the UntilI is being continued
        -- and the event hasn't yet occurred.  Otherwise terminate by
        -- putting a False into the request channel
        putMVar requestV1 (continue && not (isJust mbOcc))
        takeMVar replyV1
        if not continue then
          putMVar replyV False
         else
          case mbOcc of
            Nothing -> do
              -- Report one step done, and repeat.
              putMVar replyV True
              monitor ts' xts' mbOccs'
            Just (xte, (sound2, (volB', panB', pitchB'))) -> do
              -- Don't report one step done.  Instead, spritify sound2 and
              -- start updating it.
              -- Note: using the untilI's request and reply channels
              -- here, so we don't have to forward requests.  Otherwise
              -- with recursive reactive behaviors (e.g., produced by
              -- stepper and friends), the layers of forwarding would pile
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
              spriteChain2 <- spritifySoundRec volB' panB' pitchB' sound2
                              te xte ts xts requestV replyV emptySpriteTreeChain
              resetSpriteGroup group spriteChain2 False
  -- Avoid a space/time leak here: Don't hang onto the
  -- original color, motion and scale.  Let them age.
  forkIO $ monitor ts xts ((e `afterE` (volB,panB,pitchB)) `occs` xts)
  return (toSpriteTree group)


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
    chain    <- spritify (imF user) t0 t0 ts ts requestV replyV
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
