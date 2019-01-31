-- Functions for making new curve files.  Use with FileUtils.save.

module NewCurve where

import StaticTypes
import FileUtils

sinPoints :: Int -> [Point2]
sinPoints nCurves = take nPoints
            [point2XY t (0.5 * sin (2 * pi * t)) | t <- [-1, -1+eps .. ]]
 where
   eps = 2 / fromInt (nPoints-1)
   nPoints = 1 + 3 * nCurves


-- Helpers:
saveSingle = save "single.crv" (sinPoints 1)
saveDouble = save "double.crv" (sinPoints 2)
saveTriple = save "triple.crv" (sinPoints 3)
