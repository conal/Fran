module Main where

import Test
import Fran

main :: IO ()
main = do
  putStrLn "Let's see something now."
  mapM_ displayU [ growExpPat, lotsODonuts
                 , l2, const charlottePatDance, crop1
                 , crop2, crop3, crop4, crop5
                 ]
