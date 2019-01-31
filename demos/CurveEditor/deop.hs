-- Source code for "Declarative Event-Oriented Programming" paper

module Deop where

import IO
import Win32 (setWindowText)

import Fran
import qualified StaticTypes as S


-- A control point is a Fran 2D point behavior and a boolean behavior
-- saying whether grabbable.
type CPoint  = (Point2B, BoolB)

-- The looks of a control point.  This version suitable for gray-scale
-- reproduction.  It uses shape instead of color. 
renderCPoint :: CPoint -> ImageB
renderCPoint (pos, excited) =
  moveTo pos (
    stretch pointSize (
      ifB excited (star 3 5) circle))

pointSize :: RealB
pointSize =  0.07


-- Control point editor.  From a start point, come up with a moving point
-- under user control.  User can grab with left button when the mouse is
-- close enough (within two radii).
editCPoint :: User -> S.Point2 -> CPoint
editCPoint u p0 = (pos, closeEnough)
 where
   pos, lastRelease :: Point2B
   pos = ifB grabbing (mouse u) lastRelease
   lastRelease = stepper p0 (release `snapshot_` pos)

   closeEnough, grabbing :: BoolB
   closeEnough = distance2 pos (mouse u) <* grabDistance
   grabbing = stepper False (grab -=> True .|. release -=> False)

   grab, release :: Event ()
   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing

grabDistance :: RealB
grabDistance = 2 * pointSize                    -- proximity for grabbing

-- Try it out
editCPointTest :: User -> ImageB
editCPointTest u = renderCPoint (editCPoint u S.origin2) `over` whiteIm

whiteIm = withColor white solidImage

main1 = dispSmall editCPointTest


renderCurve :: [CPoint] -> ImageB
renderCurve cpoints =
  overs (map renderCPoint cpoints)   `over`
  withColor blue (polyBezier (map fst cpoints))

editCurve :: [S.Point2] -> User -> [CPoint]
editCurve initPoints u = map (editCPoint u) initPoints


-- Simulated graph paper background
graphPaper, graphPaper' :: ImageB
graphPaper' = withColor lightBlue (horizontal `over` vertical) `over`
              whiteIm
 where
   horizontal = overs [ moveXY 0 (constantB y) hLine
                      | y <- [-2, -2+spacing .. 2]
                      ]
   vertical   = turn (pi/2) horizontal
   hLine      = lineSegment (point2XY (-2) 0) (point2XY 2 0)
   spacing    = 0.2

-- I displayed graphPaper', screen-captured the result, and saved it to
-- "graphPaper.bmp".  Someday, Fran will optimize well enough to make this
-- sort of thing unnecessary.  Shameful hack: I put one black pixel in the
-- upper left, so the white background wouldn't become transparent.
-- Transparency should really be specifiable.
--
-- Also (unrelated), the "line" primitive should be renamed "lineSegment",
-- and there should really be an infinite "line", which`` exploits cropping.

graphPaper = importBitmap "graphPaper.bmp"

editor1 :: [S.Point2] -> User -> ImageB
editor1 initPoints u =
  renderCurve (editCurve initPoints u) `over` graphPaper

main2 = dispBig (editor1 (sinPoints 2))


-- Now add undo

-- Polymorphic, unbounded "stack".  Feed pushes and pop attempts in, and
-- get successful pops out.
stacker :: Event a -> Event () -> Event a
stacker push tryPop = legitPop `snapshot_` headB stack
 where
   legitPop :: Event ()
   legitPop = tryPop `whenE` notB (nullB stack)
   -- changeStack :: Event ([a] -> [a])
   changeStack = legitPop -=> tail .|. push ==> (:)
   -- stack :: Behavior [a]
   stack = stepAccum [] changeStack

-- This one has a problem (see text)
editCPointUndo1 :: User -> S.Point2 -> CPoint
editCPointUndo1 u p0 = (pos, closeEnough)
 where
   pos, lastRelease :: Point2B
   pos = ifB grabbing (mouse u) lastRelease
   lastRelease = stepper p0 (release `snapshot_` pos .|. undo)

   closeEnough, grabbing :: BoolB
   closeEnough = distance2 pos (mouse u) <* grabDistance
   grabbing = stepper False (grab -=> True .|. release -=> False)

   grab, release :: Event ()
   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing

   grabPos, undo :: Event S.Point2
   grabPos = grab `snapshot_` pos
   undo    = stacker grabPos (charPress '\^Z' u)

-- For visualizing the moving point and the undo stack.
editCPointUndo1Test u = renderCPoint (editCPointUndo1 u S.origin2) `over`
                        whiteIm

main3 = dispSmall editCPointUndo1Test


-- Fixed version.
editCPointUndo :: User -> Event S.Point2 -> S.Point2
               -> (CPoint, Event S.Point2)
editCPointUndo u undo p0 = ((pos, closeEnough), grabPos)
 where
   pos = ifB grabbing (mouse u) lastRelease
   lastRelease = stepper p0 (release `snapshot_` pos .|. undo)

   closeEnough = distance2 pos (mouse u) <* grabDistance
   grabbing = stepper False (grab -=> True .|. release -=> False)

   grab     = lbp u `whenE` closeEnough
   release  = lbr u `whenE` grabbing

   grabPos  = grab `snapshot_` pos


-- Manage a shared undo stack that remembers which xPoint was grabbed and
-- its grab position.

type UndoRecord = (Int, S.Point2)

editCurveUndo :: [S.Point2] -> User -> [CPoint]
editCurveUndo initPoints u = cpoints
 where
   -- Tag and merge the CPoint grab events (defined below)
   curveGrab :: Event UndoRecord
   curveGrab = anyE (zipWith tag indices pointGrabs)
    where
      tag i e = e ==> (i `pair`)        -- pair i with e's occurrence data

   indices = [1 .. length initPoints]

   -- The undo event: stack curve grabs and try to restore on control-Z's
   undo :: Event UndoRecord
   undo = stacker curveGrab (charPress '\^Z' u)

   -- Edit an indexed CPoint.
   editCP :: Int -> S.Point2 -> (CPoint, Event S.Point2)
   editCP i p0 = editCPointUndo u undoThis p0
    where
      -- Undo if a point tagged i comes off the undo stack.  Drop tag.
      undoThis = undo `suchThat` ((== i) . fst) ==> snd

   -- Apply editCP to corresponding indices and initial points, and split
   -- (unzip) the resulting cpoints and grabs into two lists.
   (cpoints, pointGrabs) = unzip (zipWith editCP indices initPoints)


editorUndo :: [S.Point2] -> User -> ImageB
editorUndo initPoints u =
  renderCurve (editCurveUndo initPoints u) `over` graphPaper

main4 = dispBig (editorUndo (sinPoints 2))

-- Visual feedback to save request.
-- Message spins & shrinks.
spinMessage :: String -> Event a -> ImageB
spinMessage message saveE = 
  stretch   saveSize  $
  turn      saveAngle $
  withColor (colorHSL saveAngle 0.5 0.5) $
  stringIm message
 where
   saveDur   = 3.0    -- artificial duration
   sinceSave =
     switcher saveDur (timeSinceE saveE)
   -- Fraction remaining (one down to zero)
   saveLeft  =
     0 `max` (saveDur - sinceSave)/saveDur
   saveSize  = 2.5 * saveLeft
   saveAngle = 2 * pi * saveLeft


timeSinceE :: Event a -> Event TimeB
timeSinceE e = e `snapshot_` time ==> since
 where
   since t0 = time - constantB t0

-- Try it out: press any key to save
spinMessageTest u =
  spinMessage "goodbye" (keyPressAny u)
   `over` whiteIm


-- This editor version saves when "s" is
-- pressed or window closed.
editor2 :: String -> IO ()
editor2 fileName = withBig $ do
  initPoints <- load fileName
  displayUIOMon (editRenderSave initPoints)
 where
   editRenderSave initPoints u =
     ( spinMessage "Saving ..." saveNow 
       `over` renderCurve xPoints
       `over` graphPaper
     , doSave )
    where
      xPoints = editCurve initPoints u
      ptsB = bListToListB (map fst xPoints)
      saveNow = charPress 's' u .|. quit u
      doSave  =
        saveNow `snapshot_` ptsB
                 ==>        save fileName

main5 = editor2 "double.crv"


editCPointRel :: User -> S.Point2 -> CPoint
editCPointRel u p0 = (pos, closeEnough)
 where
   pos = 
     switcher (constantB p0) $
      grab `snapshot` mouse u ==> relMotion .|.
      release                 ==> constantB
    where
      -- Oops: mouse u (space-time leak)
      relMotion (p, mp) =
        constantB p .+^
        (mouse u .-. constantB mp)

      grab     = lbp u `whenE` closeEnough
                       `snapshot_` pos
      release  = lbr u `whenE` grabbing
                       `snapshot_` pos
      grabbing = stepper False (
                    grab    -=> True  .|.
                    release -=> False)

   closeEnough =
     distance2 pos (mouse u) <* grabDistance


----- Load and save values

load :: Read a => String -> IO a
load fileName = do
  --putStrLn ("loading from " ++ show fileName)
  hFile <- openFile fileName ReadMode
  str <- hGetContents hFile
  return (read str)
  -- Note: no hClose.  It will happen automatically on eof or gc.

save :: Show a => String -> a -> IO ()
save fileName val = do
  --putStrLn ("saving to " ++ show fileName)
  hFile <- openFile fileName WriteMode
  hPutStr hFile (show val)
  hClose hFile

---- Curve generator

sinPoints :: Int -> [S.Point2]
sinPoints nCurves = take nPoints
            [S.point2XY t (0.5 * sin (2 * pi * t)) | t <- [-1, -1+eps .. ]]
 where
   eps = 2 / fromInt (nPoints-1)
   nPoints = 1 + 3 * nCurves

---- Functions for making new curve files.  Use with FileUtils.save.
saveSingle = save "single.crv" (sinPoints 1)
saveDouble = save "double.crv" (sinPoints 2)
saveTriple = save "triple.crv" (sinPoints 3)


---- Window size settings

withSmall = withInitialViewSize 1.5 1.5
withBig   = withInitialViewSize 3.0 1.5
dispSmall = withSmall . displayUMon
dispBig   = withBig   . displayUMon

