-- Test everything

module Main where

-- These ones imported just for static checking and to ease testing
import Fran
import NewCurve
import FileUtils
import Stacker
import InputMonitor

import qualified Editor0
import qualified Editor1
import qualified Editor2
import qualified Editor3
import qualified Editor4
import qualified Editor5
import qualified Editor6
import qualified Editor7
import qualified TryInsert

main = do
  saveDouble
  withBig $ do
   withSmall Editor0.main
   Editor1.main
   dispSmall Editor2.editXPointTest
   Editor2.main
   dispSmall Editor3.spinMessageTest
   Editor3.main
   dispSmall Editor4.editXPointTest
   Editor4.main
   dispSmall Editor5.editXPointTest
   Editor5.main
   --dispSmall Editor6.editXPointTest
   Editor6.main
   saveSingle
   Editor7.main

withSmall = withInitialViewSize 1.5 1.5
withBig   = withInitialViewSize 3.0 1.5
dispSmall = withSmall . displayUMon

setBig   = setInitialViewSize 3.0 1.5
setSmall = setInitialViewSize 1.5 1.5
