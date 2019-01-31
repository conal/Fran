module TestPred where

import RBMH
import UtilsB
import ImageBTest (disp, seqImF)


tst1 t0 = showTime t0 (dTime t0 ==* 3)

-- Find square roots of 5 (+- 2.23607)
tst2 t0 = showTime t0 ((dTime t0)^2 ==* 5)


dTime t0 = time - lift0 t0

showTime t0 cond =
 timeIm `untilB` stopAt ==> lift0
 where
  timeIm = withColor yellow $ showIm (dTime t0)
  stopAt = predicate cond t0 `snapshot` timeIm ==> snd
