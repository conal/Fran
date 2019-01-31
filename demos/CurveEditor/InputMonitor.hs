-- Monitor input.  Good for demonstrations, etc.

module InputMonitor (displayUMon, displayExMon) where

import Fran

inputMonitor :: User -> ImageB
inputMonitor u = moveXY 0 (- height / 2 + 0.25) $
                 stretch 1.5 $
                 withColor blue $
                 stringBIm message
 where
   message =     ifB (leftButton  u) (constantB "left" ) (constantB "")
             ++* ifB (rightButton u) (constantB "right") (constantB "")
             ++* keyStr
   keyStr = stepper "" (charPressAny u ==> charStr .|. keyReleaseAny u -=> "")
   (width,height) = vector2XYCoords (viewSize u)
   charStr c | c <= '\^Z' = "control-" ++ [controlChar c]
             | otherwise  = [c]

controlChar :: Char -> Char
controlChar c = toEnum (fromEnum c - fromEnum '\^A' + fromEnum 'A')


displayUMon imF = do
  w <- displayExMon (\ u -> (inputMonitor u `over` imF u, const neverE))
  eventLoop w

displayExMon imF = displayEx imF'
 where
   imF' u = (inputMonitor u `over` im, act)
    where
      (im, act) = imF u

timeSinceE :: Event a -> Event TimeB
-- timeSinceE e = e `snapshot_` time ==> constantB ==> (time -)
timeSinceE e = e `snapshot_` time ==> since
 where
   since t0 = time - constantB t0
