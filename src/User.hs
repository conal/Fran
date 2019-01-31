-- The "User" type.  A user is simply a UserAction-valued event.

module User ( UserAction(..), User(..) , makeUser
            , userStartTime
            , userTime, lbp, rbp, lbr, rbr
            , keyPressAny, keyReleaseAny, keyPress, keyRelease
            , charPressAny, charPress
            -- Could export the following, but let's not
            , resize, mouseMove
            , updateDone, quit
            , userTimeIs
            ) where

import BaseTypes (Time, DTime)
import Event
import qualified StaticTypes as S
import Vector2B
import Point2B
import Win32 (VKey)
import Trace

import Behavior
import Event
import GBehavior

-- A "user" is just a UserAction-valued event.
-- type User = Event UserAction
data User = User { leftButton   :: BoolB
                 , rightButton  :: BoolB
                 , mouse        :: Point2B
                 , viewSize     :: Vector2B
                 , updatePeriod :: Behavior DTime
                 , actions      :: Event UserAction
                 }

instance GBehavior User where
  untilB        = untilBU
  afterTimes    = afterTimesU
  timeTransform = timeTransformU

-- makeUser: make a new user

makeUser :: Bool -> Bool -> S.Point2 -> S.Vector2 -> DTime 
         -> Event UserAction -> User
makeUser lButton0 rButton0 pos0 size0 updatePeriod0 actions = u
  where
    u = User { leftButton   = toggle lButton0 (lbp u) (lbr u)
             , rightButton  = toggle rButton0 (rbp u) (rbr u)
             , mouse        = track pos0  moveFilt
             , viewSize     = track size0 sizeFilt
             , updatePeriod = track updatePeriod0 updateFilt
             , actions
             }

    moveFilt   (MouseMove p)   = Just p
    moveFilt   _               = Nothing

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
       , viewSize     = viewSize u     `untilB` eu ==> viewSize
       , updatePeriod = updatePeriod u `untilB` eu ==> updatePeriod
       , actions      = actions u      `untilB` eu ==> actions
       }

afterTimesU :: User -> [Time] -> [User]
afterTimesU u ts = group lbs rbs mms vss ups acs
  where
    group :: [BoolB] -> [BoolB] -> [Point2B] -> [Vector2B]
          -> [Behavior DTime] -> [Event UserAction] -> [User]
    group (lb:lbs) (rb:rbs) (mm:mms) (vs:vss) (up:ups) (ac:acs) =
      User { leftButton   = lb
           , rightButton  = rb
           , mouse        = mm
           , viewSize     = vs
           , updatePeriod = up
           , actions      = ac
           } : group lbs rbs mms vss ups acs

    lbs = leftButton u   `afterTimes` ts
    rbs = rightButton u  `afterTimes` ts
    mms = mouse u        `afterTimes` ts
    vss = viewSize u     `afterTimes` ts
    ups = updatePeriod u `afterTimes` ts
    acs = actions u      `afterTimes` ts


timeTransformU :: User -> TimeB -> User
timeTransformU u tb =
  User { leftButton   = leftButton u   `timeTransform` tb
       , rightButton  = rightButton u  `timeTransform` tb
       , mouse        = mouse u        `timeTransform` tb
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
  f (Button isLeft isDown _) = wantLeft == isLeft && wantDown == isDown
  f _                        = False


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
  f (MouseMove pos) = --trace ("MouseMove " ++ show pos ++ "\n")$
                      Just pos
  f _               = --trace "not mouse move\n"$
                      Nothing

updateDone :: User -> Event Time
updateDone u = actions u `filterE` f
 where
  f (UpdateDone dur) = Just dur
  f _                = Nothing


quit :: User -> Event ()
quit u = actions u `filterE` f
 where
  f Quit = Just ()
  f _    = Nothing



userTimeIs :: Time -> User -> Event ()

userTimeIs dt u = timeIs (userStartTime u + dt)


data UserAction
  = Resize      -- new size
      S.Vector2
  | Button
      Bool      -- left(True) or right(False)?
      Bool      -- down(True) or up(False)?  (dbl click == down)
      S.Point2
  | MouseMove
      S.Point2
  | Key
      Bool      -- press(True) or release(False) ?
      VKey      -- what key (its ASCII code - a first approx.)
  | CharKey
      Char      -- the ASCII key press (no up/down)
  | UpdateDone
      DTime     -- duration
  | Quit
  deriving (Show {-, Read-})  -- GHC can't Read VKey (==Word32)

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
