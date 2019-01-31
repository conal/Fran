-- "Sound behaviors"

module SoundB where

import qualified HSpriteLib
import BaseTypes (Time)
import Behavior
import Event


data SoundB = SilentS
            | BufferS (HSpriteLib.HDSBuffer)  -- sound buffer
            | MixS    SoundB SoundB
            | VolumeS RealB SoundB            -- dB.  adds
            | PitchS  RealB SoundB            -- multiplies
          --| ImageS  ImageB                  -- listen to an image
            | UntilS  SoundB (Event SoundB)
            | TimeTransS SoundB TimeB         -- timeTransform on SoundB

  deriving Show


-- Primitives

silence :: SoundB
silence = SilentS

bufferSound :: HSpriteLib.HDSBuffer -> SoundB
bufferSound = BufferS

mix :: SoundB -> SoundB -> SoundB
mix = MixS

-- multiplies (intensity, not dB) 
volume :: RealB -> SoundB -> SoundB
volume = VolumeS

-- multiplies
pitch :: RealB -> SoundB -> SoundB
pitch = PitchS


instance  GBehavior SoundB  where
  untilB     = UntilS
  afterTimes = afterTimesS

instance TimeTransformable SoundB where timeTransform = TimeTransS

afterTimesS :: SoundB -> [Time] -> [SoundB]

s@SilentS `afterTimesS` _ = repeat s

s@(BufferS buff) `afterTimesS` _ = repeat s

(snd `MixS` snd') `afterTimesS` ts =
  zipWith MixS (snd `afterTimesS` ts) (snd' `afterTimesS` ts)

VolumeS v snd `afterTimesS` ts =
  zipWith VolumeS (v `afterTimes` ts) (snd `afterTimesS` ts)

PitchS p snd `afterTimesS` ts =
  zipWith PitchS (p `afterTimes` ts) (snd `afterTimesS` ts)

-- ## This guy is wrong!!
(snd `UntilS` e) `afterTimesS` ts =
  -- (snd `afterTimesS` t) `UntilS` (e ==> (`afterTimesS` ts))
  error "No afterTimes yet on SoundB untilB, sorry."

