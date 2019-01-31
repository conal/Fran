-- Test out machines

module Main where

import Machine
import LazyST


-- Example: state is an array, input is the index of an array element to
-- be incremented, and output is the new value at the changed index.

type IncState s = STArray s Int Int
incRange = (0, 7) :: (Int,Int)

incMachine :: ST s (Int -> ST s Int)
incMachine = do
  arr <- newSTArray incRange 0
  return $ \ i -> do
    x <- readSTArray arr i
    writeSTArray arr i (x+1)
    return (x+1)


main :: IO ()
main = print $ take 100 $ zip is $ runMachine incMachine is
 where
   is = [ (j * 3) `mod` (snd incRange + 1) | j <- [0 ..] ]