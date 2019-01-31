-- "main" for Tutorial

import Fran (display, displayU)
import qualified Tutorial as T
import Concurrent (forkIO)

main = T.main

--main = do { forkIO (display T.upDownPat) ; display T.leftRightCharlotte }