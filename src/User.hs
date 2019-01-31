-- The "User" type.  A user is simply a UserAction-valued event.

module User ( UserAction(..), User(..) , makeUser
            , userStartTime
            , userTime, lbp, rbp, lbr, rbr
            , keyPressAny, keyReleaseAny, keyPress, keyRelease
            , charPressAny, charPress
            , resize, mouseMove
            , stylusMove, stylusPressureChange, stylusDown, stylusUp
            , updateDone, quit
            , userTimeIs
            , mouseMotion, stylusMotion
            , stylusPresent
            ) where


import BaseTypes (Time, DTime)
import Event
import qualified StaticTypes as S
import Vector2B
import Point2B
import Win32 (UINT, DWORD, VKey, WindowMessage, LPARAM, WPARAM {-, wM_COMMAND-})
import IOExts

import Behavior
import Event
import GBehavior
import HSpriteLib (WTPacket)

-- A "user" is just a UserAction-valued event.
-- type User = Event UserAction
data User = User { leftButton     :: BoolB
                 , rightButton    :: BoolB
                 , mouse          :: Point2B
                 , stylus         :: Point2B
                 , stylusPressure :: RealB
                 , viewSize       :: Vector2B
                 , updatePeriod   :: Behavior DTime
                 , actions        :: Event UserAction
                 , stylusPresent  :: Bool
                 }

instance GBehavior User where
  untilB        = untilBU
  afterTimes    = afterTimesU
  timeTransform = timeTransformU
  condBUnOpt c 
        (User {leftButton = leftButtonThen, rightButton = rightButtonThen,
               mouse = mouseThen,
               stylus = stylusThen, stylusPressure = stylusPressureThen, 
               viewSize = viewSizeThen,
               updatePeriod = updatePeriodThen, actions = actionsThen,
               stylusPresent = stylusPresentThen})
        (User {leftButton = leftButtonElse, rightButton = rightButtonElse,
               mouse = mouseElse,
               stylus = stylusElse, stylusPressure = stylusPressureElse,
               viewSize = viewSizeElse,
               updatePeriod = updatePeriodElse, actions = actionsElse,
               stylusPresent = stylusPresentElse}) =
    if stylusPresentThen == stylusPresentElse then
      User {leftButton = condBUnOpt c leftButtonThen leftButtonElse,
            rightButton = condBUnOpt c rightButtonThen rightButtonElse,
            mouse = condBUnOpt c mouseThen mouseElse,
            stylus = condBUnOpt c stylusThen stylusElse,
            stylusPressure = condBUnOpt c stylusPressureThen stylusPressureElse,
            viewSize = condBUnOpt c viewSizeThen viewSizeElse,
            updatePeriod = condBUnOpt c updatePeriodThen updatePeriodElse,
            actions = condBUnOpt c actionsThen actionsElse,
            stylusPresent = stylusPresentThen}
     else
       error "Conditional user: Branches disagree about stylusPresent."

makeUser :: Bool -> Bool -> S.Point2 -> S.Point2 -> Double -> S.Vector2
         -> DTime -> Bool
         -> Event UserAction -> User
makeUser lButton0 rButton0 mpos0 spos0 press0 size0
         updatePeriod0 stylusPresent actions = u
  where
    u = User { leftButton   = toggle lButton0 (lbp u) (lbr u)
             , rightButton  = toggle rButton0 (rbp u) (rbr u)
             , mouse        = track mpos0  mouseFilt
             , stylus       = track spos0  stylusFilt
             , stylusPressure = track press0  pressureFilt
             , viewSize     = track size0 sizeFilt
             , updatePeriod = track updatePeriod0 updateFilt
             , actions      = actions
             , stylusPresent = stylusPresent
             }

    mouseFilt  (MouseMove  p)  = Just p
    mouseFilt   _              = Nothing

    stylusFilt (StylusMove p)  = Just p
    stylusFilt _               = Nothing

    pressureFilt (StylusPressure p) = Just p
    pressureFilt _                  = Nothing

    sizeFilt   (Resize size)   = Just size
    sizeFilt   _               = Nothing

    updateFilt (UpdateDone dt) = Just dt
    updateFilt _               = Nothing

    track v0 filt = stepper v0 (actions `filterE` filt)

    toggle :: Bool -> Event a -> Event b -> BoolB
    toggle init go stop =  stepper init (   go   -=> True
                                        .|. stop -=> False)


untilBU :: User -> Event User -> User
untilBU u eu =
  User { leftButton   = leftButton u   `untilB` eu ==> leftButton
       , rightButton  = rightButton u  `untilB` eu ==> rightButton
       , mouse        = mouse u        `untilB` eu ==> mouse
       , stylus       = stylus u       `untilB` eu ==> stylus
       , stylusPressure = stylusPressure u `untilB` eu ==> stylusPressure
       , viewSize     = viewSize u     `untilB` eu ==> viewSize
       , updatePeriod = updatePeriod u `untilB` eu ==> updatePeriod
       , actions      = actions u      `untilB` eu ==> actions
       -- Hope eu doesn't change stylusPresent.  BUG##
       , stylusPresent = stylusPresent u  
       }

afterTimesU :: User -> [Time] -> [User]
afterTimesU u ts = group lbs rbs mms sms sps vss ups acs
  where
    group :: [BoolB] -> [BoolB] -> [Point2B] -> [Point2B] -> [RealB] -> [Vector2B]
          -> [Behavior DTime] -> [Event UserAction] -> [User]
    group (lb:lbs) (rb:rbs) (mm:mms) (sm:sms) (sp:sps) (vs:vss) (up:ups) (ac:acs) =
      User { leftButton   = lb
           , rightButton  = rb
           , mouse        = mm
           , stylus       = sm
           , stylusPressure = sp
           , viewSize     = vs
           , updatePeriod = up
           , actions      = ac
           } : group lbs rbs mms sms sps vss ups acs

    lbs = leftButton u   `afterTimes` ts
    rbs = rightButton u  `afterTimes` ts
    mms = mouse u        `afterTimes` ts
    sms = stylus u       `afterTimes` ts
    sps = stylusPressure u `afterTimes` ts
    vss = viewSize u     `afterTimes` ts
    ups = updatePeriod u `afterTimes` ts
    acs = actions u      `afterTimes` ts


timeTransformU :: User -> TimeB -> User
timeTransformU u tb =
  User { leftButton   = leftButton u   `timeTransform` tb
       , rightButton  = rightButton u  `timeTransform` tb
       , mouse        = mouse u        `timeTransform` tb
       , stylus       = stylus u       `timeTransform` tb
       , stylusPressure = stylusPressure u `timeTransform` tb
       , viewSize     = viewSize u     `timeTransform` tb
       , updatePeriod = updatePeriod u `timeTransform` tb
       , actions      = actions u      `timeTransform` tb
       }


-- Time relative to user.

userTime :: User -> TimeB
userTime u  = timeSince (userStartTime u)

filterButton :: Bool -> Bool  -> Event UserAction -> Event ()

filterButton wantLeft wantDown u = u `suchThat` f -=> ()
 where
  f (Button isLeft isDown) = wantLeft == isLeft && wantDown == isDown
  f _                      = False


lbp, rbp, lbr, rbr :: User -> Event ()

lbp u = filterButton True  True  (actions u)
rbp u = filterButton False True  (actions u)
lbr u = filterButton True  False (actions u)
rbr u = filterButton False False (actions u)

filterKey :: Bool -> User -> Event VKey

filterKey wantDown u = actions u `filterE` f
 where
  f (Key isDown key)
    | isDown == wantDown = Just key
    | otherwise          = Nothing
  f _                    = Nothing


keyPressAny, keyReleaseAny :: User -> Event VKey

keyPressAny   = filterKey True
keyReleaseAny = filterKey False

keyPress, keyRelease :: VKey -> User -> Event ()

keyPress   wantKey u = keyPressAny u   `suchThat_` (== wantKey)
keyRelease wantKey u = keyReleaseAny u `suchThat_` (== wantKey)

charPressAny :: User -> Event Char
charPressAny u = actions u `filterE` f
  where
    f (CharKey c) = Just c
    f _           = Nothing

charPress :: Char -> User -> Event ()
charPress c u = charPressAny u `suchThat_` (== c)

resize :: User -> Event S.Vector2
resize u = actions u `filterE` f
 where
  f (Resize size) = Just size
  f _             = Nothing

mouseMove :: User -> Event S.Point2
mouseMove u = actions u `filterE` f
 where
  f (MouseMove pos) = Just pos
  f _               = Nothing

stylusMove :: User -> Event S.Point2
stylusMove u = actions u `filterE` f
 where
  f (StylusMove pos) = Just pos
  f _                = Nothing

stylusButton :: User -> Event Bool
stylusButton u = actions u `filterE` f
 where
   f (StylusButton upDown) = Just upDown
   f _                     = Nothing

stylusDown, stylusUp :: User -> Event ()
stylusDown u = stylusButton u `suchThat_` id
stylusUp   u = stylusButton u `suchThat_` not

stylusPressureChange :: User -> Event Double
stylusPressureChange u = actions u `filterE` f
 where
   f (StylusPressure pressure) = Just pressure
   f _                         = Nothing

updateDone :: User -> Event Time
updateDone u = actions u `filterE` f
 where
  f (UpdateDone dur) = Just dur
  f _                = Nothing

-- Application-specific actions.  An application can further filter these
-- extAction events and make their data more convenient.

extAction :: User -> Event (WindowMessage, WPARAM, LPARAM)
extAction u = actions u `filterE` f
  where
    f (ExtAction msg wparam lparam) = Just (msg, wparam, lparam)
    f _           = Nothing

extActionIs :: WindowMessage -> User -> Event (WPARAM, LPARAM)
extActionIs target u = extAction u `filterE` f
  where
    f (msg, wparam, lparam) | msg == target = Just (wparam, lparam)
    f _           = Nothing

{-

-- wM_COMMAND, especially menus.  Something is wrong here: WM_COMMAND is
-- not >= WM_USER, so an ExtAction will not be generated.  But this
-- approach doesn't seem right anyway.

wmCommand :: User -> Event (WPARAM, LPARAM)
wmCommand = extActionIs wM_COMMAND

menu :: User -> Event Int
menu u = wmCommand u `filterE` f
 where
   f (wparam, lparam) | hiWord == 0 = Just (toInt loWord)
    where
      (hiWord,loWord) = lparam `divMod` 65536
   f _                              = Nothing

-}

quit :: User -> Event ()
quit u = actions u `filterE` f
 where
  f Quit = -- trace "found quit\n" $
           Just ()
  f _    = Nothing



userTimeIs :: Time -> User -> Event ()

userTimeIs dt u = timeIs (userStartTime u + dt)


data UserAction
  = Resize      -- new size
      S.Vector2
  | Button
      Bool      -- left(True) or right(False)?
      Bool      -- down(True) or up(False)?  (dbl click == down)
  | MouseMove
      S.Point2
  | Key
      Bool      -- press(True) or release(False) ?
      VKey      -- what key (its ASCII code - a first approx.)
  | CharKey
      Char      -- the ASCII key press (no up/down)
  | StylusButtonState
      DWORD     -- Change to button state (temporary)
  | StylusMove
      S.Point2
  | StylusPressure  -- zero is min, one is max
      Double
  | StylusButton
      Bool      -- down(True) or up(False)?
  | ExtAction   -- for application-specific extensions
      WindowMessage
      WPARAM
      LPARAM
  | UpdateDone
      DTime     -- duration
  | Quit
  deriving (Show, Read)  -- GHC can't Read VKey (==Word32)

{-
-- So UserAction can derive Show.  Should be in Win32.hs, and more
-- informative.
instance Show VKey where
  showsPrec p _ = showString "<<VKey>>"
-}


-- Start time of user.
-- ## Bug: violates event abstraction.  Events don't really have start
-- times.  I put it in for convenience, but reconsider.

userStartTime :: User -> Time
userStartTime u = userStartTime' (actions u)
  where
    userStartTime' (Event ((t0,_) : _))  = t0


-- Mouse and stylus motion vectors, i.e., where the mouse or stylus is
-- relative to the origin

mouseMotion :: User -> Vector2B
mouseMotion u = mouse u .-. origin2

stylusMotion :: User -> Vector2B
stylusMotion u = stylus u .-. origin2
