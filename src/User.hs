-- User interaction
--
-- To do: perhaps eliminate UserNoOp in favor of UpdateDone

module User (
    UserAction(..)
    , User, UserChannel, newUser
    , addUserNoOp , addUserUpdate
    -- , getChanContents
  ) where

import BaseTypes (Time)
import Event
import Vector2 (Vector2)
import Point2 (Point2)
import Channel
import IOExtensions (unsafeInterleaveIO)
import Win32 (VKey)
import Trace

data UserAction
  = Resize	-- new size
      Vector2
  | Button
      Bool	-- left(True) or right(False)?
      Bool	-- down(True) or up(False)?  (dbl click == down)
      Point2
  | MouseMove
      Point2
  | Key
      Bool	-- press(True) or release(False) ?
      VKey	-- what key (its ASCII code - a first approx.)
  | UpdateDone
      Time	-- duration
  | Quit
  deriving Show

-- {- Something needed for Hugs 1.4
instance Show VKey where
  showsPrec p _ = showString "<VKey>"
-- -}

-- A "user" is just a UserAction-valued event.

type User = Event UserAction

-- User actions come in on a channel, together with buffering.
type UserChannel = Channel (PossOcc UserAction)

-- Make a new user, with an initial no-op.  This lets the user be queried
-- for time t0 even when there isn't yet a thread doing more putChan's.

newUser :: Time -> IO (User, UserChannel)

newUser t0 =
  do ch <- newChan
     putChan ch (t0, Nothing)           -- necessary? ###
     contents <- getChanContents ch
     return (possOccsE contents, ch)

addUserNoOp :: UserChannel -> Time -> IO ()

addUserNoOp userChan t = putChan userChan (t, Nothing)

addUserUpdate :: UserChannel -> Time -> Time -> IO ()

addUserUpdate userChan dur t =
  --trace ("addUserUpdate " ++ show dur ++ " at " ++ show t ++ "\n") $
  --trace ("upd " ++ show t ++ " ") $
  putChan userChan (t, Just (UpdateDone dur))

-- From the Concurrent Haskell Channel, courtesy of Sigbjorn
-- Should really move into libhugs/Channel.hs

getChanContents :: Show a => Channel a -> IO [a]

getChanContents ch =
  unsafeInterleaveIO (
  do -- putStrLn "Doing getChan"
     x  <- getChan ch
     --print x
     xs <- unsafeInterleaveIO (getChanContents ch)
     return  (x:xs) )


testUser :: IO ()

testUser =
  do (user, userChan) <- newUser 0
     print $ take 10 (tstE (const user))
