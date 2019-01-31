-- Envelope module for static modeling types.
-- 
-- Last modified Mon Oct 27 15:31:30 1997
-- 
-- Recommended use: "import qualified StaticTypes as S" (as in ImageBTest.hs).

module StaticTypesLoader
  (
    module BaseTypes,
    module VectorSpace,
    module Vector2,
    module Point2,
    module Rect,
    module Transform2,
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
import Transform2
import Vector3
import Point3
import Rect
import Transform3
import Color
import Font
import Text
