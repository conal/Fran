-- Curve editor, version 3.  Adds file saving.

module Editor3 ( XPoint, renderXPoint, editXPoint, pointSize, grabDistance
               , renderCurve, editCurve, graphPaper, whiteIm
               , spinMessage, spinMessageTest, editor, main
               ) where

import Editor2 hiding (editor, main)

import Fran
import qualified StaticTypes as S
import FileUtils
import InputMonitor
import Win32 (setWindowText)

-- Visual feedback to save request.  Message spins & shrinks
spinMessage :: String -> Event a -> ImageB
spinMessage message saveE = stretch   saveSize  $
                            turn      saveAngle $
                            withColor (colorHSL saveAngle 0.5 0.5) $
                            stringIm message
 where
   saveDur   = 3.0    -- artificial duration
   sinceSave = switcher saveDur (timeSinceE saveE)
   -- What fraction of save remains (one down to zero)
   saveLeft  = 0 `max` (saveDur - sinceSave)/saveDur
   saveSize  = 2.5 * saveLeft
   saveAngle = 2 * pi * saveLeft


timeSinceE :: Event a -> Event TimeB
-- timeSinceE e = e `snapshot_` time ==> constantB ==> (time -)
timeSinceE e = e `snapshot_` time ==> since
 where
   since t0 = time - constantB t0

-- Try it out: press any key to save
spinMessageTest u = spinMessage "goodbye" (keyPressAny u) `over` whiteIm



-- This editor version saves when "s" is pressed or window closed.
editor :: String -> IO ()
editor fileName = do
  initPoints <- load fileName
  window     <- displayExMon (editRenderSave initPoints)
  setWindowText window ("Curve editor 3: " ++ fileName)
  eventLoop window
 where
   editRenderSave initPoints u =
     ( spinMessage "Saving ..." saveNow `over`
       renderCurve xPoints              `over`
       graphPaper
     , const doSave )
    where
      xPoints = editCurve initPoints u
      saveNow = charPress 's' u .|. quit u
      doSave  = saveNow `snapshot_` bListToListB (map fst xPoints)
                        ==>         save fileName


main = editor "double.crv"
