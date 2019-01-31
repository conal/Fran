-- Polymorphic, unbounded "stack".  Feed pushes and pop attempts in, and
-- get successful pops out.
--
-- Useful for "undo", and perhaps other applications as well.


module Stacker (stacker, stackerTest) where

import Fran

stacker :: Event a -> Event () -> Event a
stacker push tryPop = fst (stackerEx push tryPop)

-- Normally the stack would be purely internal, but stackerEx exposes
-- it for visualization (as in stackerTest).

stackerEx :: Event a -> Event () -> (Event a, Behavior [a])
stackerEx push tryPop =
  -- On legit pop, snapshot the stack's head.
  (legitPop `snapshot_` headB stack, stack)
 where
   -- Legit pop requires a non-empty pop stack.
   legitPop = tryPop `whenE` notB (nullB stack)

   -- Stack changer event.  Occurrences have stack->stack functions.
   --changeStack :: Event ([a] -> [a])
   changeStack = legitPop -=> tail .|. push ==> (:)

   -- The stack: starts empty and applies changeStack as it occurs.
   --stack :: Behavior [a]
   stack = stepAccum [] changeStack

-- Try it out.  Type in letters, hit back-space to undo
stackerTest :: User -> ImageB
stackerTest u = -- Uncomment first line to reveal stack
                moveXY 0 (-0.9) (showBIm stack) `over`
                stretch 2 (stringBIm str)
                -- `over` whiteIm
 where
   str = stepper "" (pop .|. add ==> \ (c, s) -> s ++ [c])
   add = nonBSChar `snapshot` str
   (pop, stack) = stackerEx (add ==> snd) (charPress bsChar u)
   nonBSChar  = charPressAny u `suchThat` (not . (== bsChar))

   bsChar = '\b'                       -- Backspace

main = displayU stackerTest