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
  deriving Show

instance  Show HSpriteLib.HDSBuffer  where
  showsPrec p _ = showString "<sound buffer>"
 

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
  untilB = UntilS
  afterTime = afterTimeS
  startTime = error "startTime not yet implemented for SoundB"


afterTimeS :: SoundB -> Time -> SoundB

SilentS `afterTimeS` _ = SilentS

s@(BufferS buff) `afterTimeS` _ = s

(snd `MixS` snd') `afterTimeS` t =
  (snd `afterTime` t) `MixS` (snd' `afterTime` t)

VolumeS v snd `afterTimeS` t =
  VolumeS (v `afterTime` t) (snd `afterTime` t)

PitchS p snd `afterTimeS` t =
  PitchS (p `afterTime` t) (snd `afterTime` t)

(snd `UntilS` e) `afterTimeS` t =
  (snd `afterTime` t) `UntilS` (e ==> (`afterTime` t))

