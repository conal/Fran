-- "Image behavior" type defined directly, i.e., without Image or Behavior.

module ImageB where

-- To do: figure out how not to need a copy of HSpriteLib in this
-- directory.  I thought the following would work.

-- import qualified "../SpriteLib/HSpriteLib"
import qualified HSpriteLib

import BaseTypes
import qualified StaticTypes as S
import Behavior
import Vector2B
import ColorB
import TextB
import Event
import RenderImage
import SoundB
import Maybe (fromMaybe)
import Update (updateBvrRefIO)
import IORef

infixl 6 `over`


defaultTextColor = red


-- The bare minimum for now

data ImageB
 = EmptyImage
 | FlipImage  HSpriteLib.HFlipBook RealB		-- page # behavior
 -- A synthetic image knows how to map optional color and stretch to a surface
-- | SyntheticImage (Maybe ColorB -> RealB -> SurfaceB)
 | SyntheticImageIO SurfGenIO           -- replacing SyntheticImage
 | SoundI     SoundB                    -- embedded sound
 | Over       ImageB   ImageB
 | Move	      Vector2B ImageB
 | Stretch    RealB    ImageB
 | WithColor  ColorB ImageB
 | UntilI     ImageB (Event ImageB)	-- "untilB" on ImageB
 | TTransI    TimeB  ImageB             -- timeTransform on ImageB
 deriving Show


-- Surface generator.  Takes an optional color behavior and a scale
-- behavior, and gives an IO that does initialization (e.g., creates a 
-- D3DRM frame hierarchy) and gives an IO for constructing DDraw
-- surfaces.  Later, fix interface so we can re-use old surfaces.

type SurfGenIO =
  Maybe ColorB -> RealB -> IO (Time -> Time -> IO HSpriteLib.HDDSurface) 

-- Primitives

emptyImage	=  EmptyImage
flipImage	=  FlipImage
--syntheticImage=  SyntheticImage
syntheticImageIO=  SyntheticImageIO
soundImage      =  SoundI          
over		=  Over
move		=  Move
stretch		=  Stretch
withColor	=  WithColor
timeTransformI  =  TTransI

syntheticImage :: (Maybe ColorB -> RealB -> SurfaceB) -> ImageB

syntheticImage f = syntheticImageIO surfGenIO
 where
  surfGenIO mbColorB scaleB =
    do surfBRef <- newRef (f mbColorB scaleB)
       -- The returned surface generator pulls the current surfaceB out of
       -- surfBRef, samples it to get a new surface and new surfaceB,
       -- stores the new surfaceB in surfBRef, and returns the new surface.
       return ( \t t' ->
         updateBvrRefIO surfBRef t return )


instance  GBehavior ImageB  where
  untilB    = UntilI
  afterTime = afterTimeI
  startTime = error "startTime not yet implemented for ImageB"
  -- To do: move timeTransform into GBehavior and overload here
  -- timeTransform = TTransI


afterTimeI :: ImageB -> Time -> ImageB

EmptyImage `afterTimeI` _ = EmptyImage

FlipImage book page `afterTimeI` t = FlipImage book (page `afterTime` t)

{-
-- Is this one right???
SyntheticImage f `afterTimeI` t = SyntheticImage f'
 where
  f' mbColorB angleB = f mbColorB angleB `afterTime` t
-}

-- Is this one right???
im@(SyntheticImageIO f) `afterTimeI` t = im

SoundI snd `afterTimeI` t = SoundI (snd `afterTime` t)

(imb `Over` imb') `afterTimeI` t =
  (imb `afterTime` t) `Over` (imb' `afterTime` t)

Move v imb `afterTimeI` t =
  Move (v `afterTime` t) (imb `afterTime` t)

Stretch x imb `afterTimeI` t =
  Stretch (x `afterTime` t) (imb `afterTime` t)

WithColor c imb `afterTimeI` t =
  WithColor (c `afterTime` t) (imb `afterTime` t)

(imb `UntilI` e) `afterTimeI` t =
  (imb `afterTime` t) `untilB` e ==> (`afterTime` t)

-- The next definition seems better.
{-
(imb `UntilI` e) `afterTime` t =
  case  e `occ` t  of
    (Nothing, e')     ->
      (imb `afterTime` t) `UntilI` (e' ==> (`afterTime` t))
    (Just imb',  e')  ->  imb' `afterTime` t
-}

-- utilities

overs :: [ImageB] -> ImageB

overs = foldr over emptyImage

{- Eliminated
textSurface1 :: StringB -> Maybe ColorB -> RealB -> SurfaceB

textSurface1 strB mbColB stretchB =
  lift2 HSpriteLib.textDDSurface
	strB (asColorRef (fromMaybe defaultTextColor mbColB))
-}

-- textB, angleB, colorB, stretchB
textSurface :: TextB -> RealB -> Maybe ColorB -> RealB -> SurfaceB

textSurface textB angleB mbColorB stretchB =
  lift4 renderText
	textB angleB
	(fromMaybe defaultTextColor mbColorB)
	stretchB

textImage :: TextB -> RealB -> ImageB

textImage textB angleB = syntheticImage (textSurface textB angleB)

{-  Maybe should use this somewhere

bitmapPixelsPerLengthHorizontal = 100 :: Double
bitmapPixelsPerLengthVertical   = 100 :: Double

bitmapSizeToVector2 :: (Int,Int) -> Vector2

bitmapSizeToVector2 (w,h) =
  vector2XY (fromInt w / bitmapPixelsPerLengthHorizontal)
            (fromInt h / bitmapPixelsPerLengthVertical  )

-}

-- Should really be two constants -- horizontal and vertical.
bitmapPixelsPerLength = 100 :: RealVal

-- Note that screen pixels per world length and screen pixels per world
-- length do not have to agree.  If they do, however, the scalings will
-- cancel out, which makes for much faster display on most cards.

screenPixelsPerLength = bitmapPixelsPerLength :: RealVal
   

-- Misc to go elsewhere

type SurfaceB = Behavior HSpriteLib.HDDSurface

instance  Show HSpriteLib.HFlipBook  where
  showsPrec p _ = showString "<flip book>"
instance  Show HSpriteLib.HDDSurface  where
  showsPrec p _ = showString "<surface>"

