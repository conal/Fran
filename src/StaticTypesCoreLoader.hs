-- Envelope module for static modeling types.  Win32-independent
-- subset. Load via ../StaticTypesCore.hs
-- 
-- Recommended use: "import qualified StaticTypesCore as S"

module StaticTypesCoreLoader (
         module BaseTypes
       , module VectorSpace
       , module Vector2
       , module Point2
       , module Rect
       , module Transform2
       , module Vector3
       , module Point3
       , module ColorCore
       , module Transform3
  ) where

import BaseTypes
import VectorSpace
import Vector2
import Point2
import Rect
import Transform2
import Vector3
import Point3
import ColorCore
import Transform3
