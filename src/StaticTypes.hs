-- Envelope module for static modeling types.
-- 
-- Last modified Sat Apr 26 10:57:50 1997
-- 
-- Recommended use: "import qualified StaticTypes as S" (as in ImageBTest.hs).

module StaticTypes
  (
    module BaseTypes,
    module VectorSpace,
    module Vector2,
    module Point2,
--    module Transform2,
    module Vector3,
    module Point3,
    module Transform3,
    module Color,
    module Font,
    module Text,
--    module Image,
--    module PickImage
  )
  where

import BaseTypes
import VectorSpace
import Vector2
import Point2
--import Transform2
import Vector3
import Point3 hiding ((.-.), (.+^), (.-^))
import Transform3
import Color
import Font
import Text
--import Image
--import PickImage
