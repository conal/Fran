-- Monitor input.  Good for demonstrations, etc.

module InputMonitor (displayUMon, displayExMon) where

import Fran
import Maybe (fromMaybe)
import Win32Key

inputMonitor :: User -> ImageB
inputMonitor u = moveXY 0 (- height / 2 + 0.25) $
                 stretch 1.5 $
                 withColor blue $
                 stringBIm message
 where
   message =     ifB (leftButton  u) (constantB "left " ) (constantB "")
             ++* ifB (rightButton u) (constantB "right ") (constantB "")
             ++* keyStr
   keyStr = stepper "" (  charPressAny u ==> charStr
                      .|. keyPressAny u `assocE` vkeys
                      .|. keyReleaseAny u -=> ""
                       )
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


-- Generated mechanically from Win32Key.gc
vkeys :: [(VKey, String)]
vkeys = [
          (vK_LBUTTON, "lbutton key")
        , (vK_RBUTTON, "rbutton key")
        , (vK_CANCEL, "cancel key")
        , (vK_MBUTTON, "mbutton key")
        , (vK_BACK, "back key")
        , (vK_TAB, "tab key")
        , (vK_CLEAR, "clear key")
        , (vK_RETURN, "return key")
     -- , (vK_SHIFT, "shift key")
     -- , (vK_CONTROL, "control key")
        , (vK_MENU, "menu key")
        , (vK_PAUSE, "pause key")
        , (vK_CAPITAL, "capital key")
        , (vK_ESCAPE, "escape key")
        , (vK_SPACE, "space key")
        , (vK_PRIOR, "prior key")
        , (vK_NEXT, "next key")
        , (vK_END, "end key")
        , (vK_HOME, "home key")
        , (vK_LEFT, "left key")
        , (vK_UP, "up key")
        , (vK_RIGHT, "right key")
        , (vK_DOWN, "down key")
        , (vK_SELECT, "select key")
        , (vK_EXECUTE, "execute key")  
        , (vK_SNAPSHOT, "snapshot key")
        , (vK_INSERT, "insert key")
        , (vK_DELETE, "delete key")
        , (vK_HELP, "help key")
        , (vK_NUMPAD0, "numpad0 key")
        , (vK_NUMPAD1, "numpad1 key")
        , (vK_NUMPAD2, "numpad2 key")
        , (vK_NUMPAD3, "numpad3 key")
        , (vK_NUMPAD4, "numpad4 key")
        , (vK_NUMPAD5, "numpad5 key")
        , (vK_NUMPAD6, "numpad6 key")
        , (vK_NUMPAD7, "numpad7 key")
        , (vK_NUMPAD8, "numpad8 key")
        , (vK_NUMPAD9, "numpad9 key")
        , (vK_MULTIPLY, "multiply key")
        , (vK_ADD, "add key")
        , (vK_SEPARATOR, "separator key")
        , (vK_SUBTRACT, "subtract key")
        , (vK_DECIMAL, "decimal key")
        , (vK_DIVIDE, "divide key")
        , (vK_F1, "f1 key")
        , (vK_F2, "f2 key")
        , (vK_F3, "f3 key")
        , (vK_F4, "f4 key")
        , (vK_F5, "f5 key")
        , (vK_F6, "f6 key")
        , (vK_F7, "f7 key")
        , (vK_F8, "f8 key")
        , (vK_F9, "f9 key")
        , (vK_F10, "f10 key")
        , (vK_F11, "f11 key")
        , (vK_F12, "f12 key")
        , (vK_F13, "f13 key")
        , (vK_F14, "f14 key")  
        , (vK_F15, "f15 key")
        , (vK_F16, "f16 key")
        , (vK_F17, "f17 key")
        , (vK_F18, "f18 key")
        , (vK_F19, "f19 key")
        , (vK_F20, "f20 key")
        , (vK_F21, "f21 key")
        , (vK_F22, "f22 key")
        , (vK_F23, "f23 key")
        , (vK_F24, "f24 key")
        , (vK_NUMLOCK, "numlock key")
        , (vK_SCROLL, "scroll key")
        ]
