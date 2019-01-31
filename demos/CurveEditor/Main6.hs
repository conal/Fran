module Main where

import qualified Editor6 as Editor
import Fran (setInitialViewSize)
import NewCurve -- (saveDouble)

main = do
  setInitialViewSize 3.0 1.5
  saveDouble
  Editor.main