-- Ad hoc collection of definitions. These are here for introductory use,
-- to make it easier to do some simple things. I'd like to make it so that
-- kids can use this simple vocabulary.
--
-- Last modified Fri Oct 25 09:27:15 1996

module UtilsB where

import qualified StaticTypes as S

import RBMH

-- Move

moveHorizontal dx = moveXY dx 0
moveVertical   dy = moveXY 0 dy
moveXY dx dy im   = translate2 (vector2XY dx dy) *% im

-- Resize
bigger sc im = uscale2 sc *% im

smaller sc = bigger (1/sc)

-- Oscillates between -1 and 1

wiggle = sin (pi * time)

-- Shift a behavior to be later or earlier, faster or slower

later dt b = b `timeTransform` (time - lift0 dt)
earlier dt = later (-dt)

faster x b = b `timeTransform` (time * x)
slower x   = faster (1/x)

-- Continuous show, rendered into an image

showIm :: Text a => ColorB -> Behavior a -> ImageB

showIm col = withColor col . renderedText . simpleText . showB
