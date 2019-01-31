-- This is a fairly random collection of functions based on the
-- Random number library.

module RandomIO( randomIO, randoms ) where

import Win32( timeGetTime )
import Random( randomInts )
--import Foreign( wordToInt )
--import Word (wordToInt)

randomIO :: IO [Float]
randomIO = timeGetTime >>= \ ms ->
   let --ms' = wordToInt ms
       ms' = toInt ms
       rs = randomInts (1+(ms' `mod` 10000000))
                       (1+(ms' `mod` 12345678)) in
     return (map (\i -> (fromInt (i `mod` 10000) / (10000 :: Float))) rs)

randoms :: Int -> [Int]
randoms n = map (`mod` n) (randomInts 2374987 1170417)

