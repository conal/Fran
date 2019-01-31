-- Input faking and paste-up for harcopy


module ReplayTut where

import Fran
import Tutorial hiding (main)
import qualified StaticTypes as S
import Random


backColor = white

-- For captured or faked input:

instantReplay :: (User -> ImageB) -> IO ()
instantReplay imF = do
  actions <- displayU' imF id
  displayU' (substActions actions imF) (const ())

substActions :: [PossOcc UserAction] -> (User -> ImageB) -> (User -> ImageB)
substActions oldOccs imF u@(User{actions=newActions}) = imF u'
 where
   u' = makeUser False False S.origin2 (S.vector2XY 2 2) 0.1 actions'

   oldActions = Event oldOccs
   
   actions' = newActions `suchThat` (not . useOld)
          .|. laterE deltaStart oldActions `suchThat` useOld

   useOld (Button _ _ ) = True
   useOld (MouseMove _) = True
   useOld _             = False

   deltaStart = startTime newActions - startTime oldActions

   startTime (Event ((t0,_):_)) = t0

   laterE dt (Event possOccs) = Event [(t+dt,mb) | (t,mb) <- possOccs]


-- Substitute occurrences with durations
substOccs :: [(DTime, UserAction)] -> (User -> ImageB) -> (User -> ImageB)
substOccs occs = substActions ((0,Nothing) : loop occs 0)
 where
   loop [] _                      =  []
   loop ((dur, action) : rest) t0 =  (t1, Just action) : loop rest t1
                                       where t1 = t0 + dur


-- Remap user time to zero and trim off the Quit and later.
trimActions possOccs = takeWhile (not . isQuit) possOccs
 where
   isQuit (_, Just Quit) = True
   isQuit _              = False


-- Clone a user, replacing the mouse
mouseUser mouse u =
  User { leftButton   = leftButton u
       , rightButton  = rightButton u
       , mouse
       , viewSize     = viewSize u
       , updatePeriod = updatePeriod u
       , actions      = actions u
       }

-- Simple key-framed specification of motions.

-- Each keyframe has a value, a duration to stay there, and a duration to
-- get to the next key frame.
type KeyFrames a = [ (a, DTime, DTime) ]

keyMotion :: S.VectorSpace a => KeyFrames a -> Time -> Behavior a

keyMotion [ (val,_,_) ] _ = constantB val

keyMotion ((x0,stayDur,moveDur) : keys'@((x1,_,_) : _)) t0 =
  constantB x0            `untilB` timeIs t1 -=> (
  interpolate x0 x1 t1 t2 `untilB` timeIs t2 -=> (
  keyMotion keys' t2 ))
 where
   t1 = t0 + stayDur
   t2 = t1 + moveDur

interpolate :: VectorSpace v => v -> v -> Time -> Time -> Behavior v
interpolate x0 x1 t0 t1 =
  constantB x0 ^+^ (time - constantB t0) *^ constantB rate
 where
  rate = (x1 S.^-^ x0) S.^/ (t1  -  t0)


keyMotionPoint2 :: KeyFrames S.Point2 -> Time -> Point2B
keyMotionPoint2 frames t0 = origin2 .+^ keyMotion vFrames t0
 where
   vFrames = [ (p S..-. S.origin2, stay, move) | (p, stay, move) <- frames ]

------- Captured input ------

capture :: (User -> ImageB) -> String -> IO ()
capture imF fileName = do
  actions <- displayU' imF (show . trimActions)
  writeFile fileName actions

{-
-- Problem: reading is so slow in Hugs that I get an abysmal update rate
replay :: (User -> ImageB) -> String -> IO ()
replay imF fileName = do
  actions <- map read (readFile fileName)
  replayActions imF actions

replayActions imF actions = displayU (substActions actions imF)

replayPasteUpActions cols rows cellSize tStart tEnd imF actions =
  replayActions (\u -> pasteUp cols rows cellSize tStart tEnd
                       (showCursor imF u) u)
                actions
-}

patMouse u = move (mouseMotion u) pat

-- Showing the cursor

showCursor imF u =
  -- We want the upper-left corner at the user's mouse position, so shift
  -- left and up by half the cursor image size.
   earlier cursorLead (move (mouseMotion u + vector2XY dx dy) cursor)
   `over` imF u
 where
   dx = constantB (  cursorW / 2)
   dy = constantB (- cursorH / 2)

-- Cursor, with width and height.  Hack: surrounded by a one-pixel black
-- border, in order to get black to be transparent.  Should instead allow
-- explicit back color.
(cursor, cursorW, cursorH) = importBitmapWithSize "../Media/cursor.bmp"
-- How far should the cursor lead?  Simulates interaction lag.
cursorLead :: Floating a => a
cursorLead = 0.05

-- Faked input

keyedMouse :: KeyFrames S.Point2 -> (User -> ImageB) -> (User -> ImageB)
keyedMouse keys imF u = showCursor imF (mouseUser mousePos u)
 where
   -- mousePos = timeTransform (point2Polar 1 (pi * time)) (sin (userTime u))
   mousePos = keyMotionPoint2 keys (userStartTime u)

-- Fixed durations with randomness
keyedMouseReg :: [S.Point2] -> RealVal -> RealVal -> RealVal -> (User -> ImageB)
              -> (User -> ImageB)
keyedMouseReg points stayDur moveDur maxVariance =
  keyedMouse (zipWith3 (\ p eps eps' -> (p, f stayDur eps, f moveDur eps'))
                       points stayRands moveRands )
 where
   stayRands = randomDoubles i1
   moveRands = randomDoubles i2
   f dur r   = dur * (1 + maxVariance * 2 * (r-0.5))
   i1        = round (stayDur * 345)
   i2        = round (moveDur * 678)

randomDoubles s = map f (random (0,max) s)
 where
   f n = fromInteger n / maxR
   max  = 100000
   maxR = fromInteger max

-- Star shapes
keyedMouseStar :: Int -> Int -> RealVal -> RealVal -> RealVal -> (User -> ImageB)
              -> (User -> ImageB)
keyedMouseStar skip vertices = keyedMouseReg (cycle points)
 where
   points = [ S.point2Polar 0.7 (fromInt i * delta) | i <- [0..vertices-1] ]
   delta  = fromInt skip * 2 * pi / fromInt vertices


--                         Pasting up for print
--
-- Make a n-by-m array of still images.  For instance,
--
--   pasteUp 3 3 2.2 0 2 (const charlottePatDance)
--
--   pasteUp 3 3 2.2 0.5 2.0 (keyedMouseStar 1 3 (1/3) (1/3) 0.4 kids)

pasteSizeAdjust :: Floating a => a
pasteSizeAdjust = 0 -- 0.2

pasteUp :: Int -> Int -> Double -> Time -> Time -> (User -> ImageB)
        -> (User -> ImageB)

pasteUp cols rows cellSize tStart tEnd imF u =
  stretch ((vSize - pasteSizeAdjust) / totSize) (
    overs [ moveXY (- width /2 + (fromInt col + 0.5) * size )
                   (  height/2 - (fromInt row + 0.5) * size) $
            cell col row
          | col <- [0 .. cols-1]
          , row <- [0 .. rows-1]
          ] )
  `over` withColor backColor solidImage
 where
   width  = size * fromInt cols
   height = size * fromInt rows
   size   = constantB cellSize
   imB    = imF u
   uStart = userStartTime u

   cell col row =
     imB' `timeTransform`
      constantB (uStart + tStart + fromInt (cols * row + col) * cellDur)

   
   imB' = withColor textColor (polyline ps) `over`
          crop (rectLLUR (ps!!2) (ps!!0)) imB
    where
      ps = [ point2Polar cellRadius (constantB (angle + pi/4))
           | angle <- [0, pi/2 .. 2 * pi + 0.001]
           ]

   dur = tEnd - tStart

   cellRadius = size * sqrt 2 / 2

   cellDur = dur / fromInt (cols * rows)

   vSize = w `min` h where (w,h) = vector2XYCoords (viewSize u)

   totSize = width `min` height

-- Try "displayU $ tstA kids"
tstA = keyedMouseStar 3 8 0.2 0.35 0.8


-- Putting it together.  


imFs = [ ]
       {-
       ++ map const [ leftRightCharlotte, upDownPat, charlottePatDance
                    , charlottePatDoubleDance, dance1, dance2
                    , patOrbitsCharlotte
                    ]
        ++ [ velBecky, accelBecky ]
        ++ map (keyedMouseStar 1 3 (1/3) (1/3) 0.4)
        [ mouseVelBecky, beckyChaseMouse, danceChase
        , springDragBecky]
        ++ [ const orbitAndLater, const orbitAndFaster ]
        -}
        ++ map (keyedMouseStar 3 7 (2/7) (3/7) 0.5)
        [ followMouseAndDelay, kids ]
        ++ [ flows ]
        ++ map (keyedMouseStar 3 5 (2/5) (3/5) 0.5)
        [ flows2 ]
        ++ map (substOccs buttonOccs1)
        [ redBlue, redBlueCycle, tricycle ]
        ++ map (substOccs buttonOccs2)
        [ jumpFlower ]
        ++ map (substOccs buttonOccs3)
        [ growFlower]
        ++ map (substOccs buttonOccs4)
        [ growFlowerExp]
        ++ map rgf
            [ {-const sphere, const teapot,-} const redSpinningPot ]
        ++ map (keyedMouseStar 3 5 (2/5) (3/5) 0.5 . rgf)
            [ mouseSpinningPot ]
        ++ map (substOccs buttonOccs3)
            [ spin1, spin2 ]
        ++ map rg
            [ potAndLight, potAndLights , spiral3D, spiralTurn ]

rgf gf u = renderGeometry (gf u) defaultCamera
rg  g = rgf (const (g ()))

-- Button occurrences.  The first bool is left(True) or right(False), and
-- the second is down(True) or up(False).
buttonOccs1 = cycle [(1.0, Button True True), (0.3, Button True False)]

buttonOccs2 = cycle [(1.0, Button True True), (0.3, Button True False)
                    ,(1.0, Button False True), (0.3, Button False False)]
buttonOccs3 = cycle [(0.5, Button True True), (1.5, Button True False)
                    ,(0.5, Button False True), (2.0, Button False False)]
buttonOccs4 = cycle [(0.5, Button True True), (1.0, Button True False)
                    ,(0.5, Button False True), (2.0, Button False False)]


imFsPasted = map (pasteUp 4 4 2.2 0 2) imFs

main = displayU $ stepThrough imFsPasted