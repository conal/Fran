-- Spatial partitioning trees

module SpaceTree ( Bounded2(..)
                 , PTree, mkPTree, searchPTree
                 ) where

import Fran
import qualified StaticTypes as S

import IOExts (trace)


-- Values having bounding boxes
class Bounded2 a where
 bbox :: a -> S.Rect

-- A spatial partitioning tree, or "PTree", represents a set of bounded
-- objects.  Each node represents the contents over a rectangular region
-- (understood from context), and is partitioned into five subsets: those
-- objects fully contained each quadrant, and the remaining set of
-- "straddlers".  The four quadrant subsets are represented by ptrees.
data PTree a =
    EmptyPTree
  | PTree S.Point2                      -- center point
          [a]                           -- straddlers
          (PTree a) (PTree a)           -- lower-left, lower-right
          (PTree a) (PTree a)           -- upper-left, upper-right
 deriving Show


-- Make a ptree from a bbox and list of bounded objects.
mkPTree :: Bounded2 a => S.Rect -> [a] -> PTree a
mkPTree _   []    = EmptyPTree
mkPTree box elems = loop [] [] [] [] [] elems
 where
   loop straddlers lls lrs uls urs elems =
     case elems of
       [] -> PTree mm
                   straddlers
                   (mkPTree (S.RectLLUR ll mm) lls)
                   (mkPTree (S.RectLLUR lm mr) lrs)
                   (mkPTree (S.RectLLUR ml um) uls)
                   (mkPTree (S.RectLLUR mm ur) urs)
    
       elem : elems' ->
         let 
             S.RectLLUR (S.Point2XY eminx eminy)
                        (S.Point2XY emaxx emaxy) = bbox elem
             addStraddler = loop (elem : straddlers) lls lrs uls urs elems'
         in
           if    emaxx < minx || eminx > maxx
              || emaxy < miny || eminy > maxy then
             addStraddler
           else
             case (cmpIV eminx emaxx midx, cmpIV eminy emaxy midy) of
               (-1, -1)  -> loop straddlers (elem : lls) lrs uls urs elems'
               ( 1, -1)  -> loop straddlers lls (elem : lrs) uls urs elems'
               (-1,  1)  -> loop straddlers lls lrs (elem : uls) urs elems'
               ( 1,  1)  -> loop straddlers lls lrs uls (elem : urs) elems'
               otherwise -> addStraddler

   S.RectLLUR ll@(S.Point2XY minx miny) ur@(S.Point2XY maxx maxy) = box

   ml = S.Point2XY minx midy
   mm = S.Point2XY midx midy
   mr = S.Point2XY maxx midy
   um = S.Point2XY midx maxy
   lm = S.Point2XY midx miny

   midx = minx `av` maxx
   midy = miny `av` maxy

   a `av` b = (a + b) / 2


-- Search a ptree for a member overlapping with a given rectangle and
-- passing a Nothing/Just filter.
searchPTree :: PTree a -> S.Rect -> (a -> Maybe b) -> Maybe b
searchPTree ptree (S.RectLLUR (S.Point2XY minx miny)
                              (S.Point2XY maxx maxy)) test = loop ptree
 where
   loop EmptyPTree = Nothing
   loop (PTree (S.Point2XY cx cy) staddlers ll lr ul ur) =
     --trace "search " $
     foldr (++) Nothing (map test staddlers) ++ subs 
    where
      subs = loop ll `whenMZ` (someLeft  && someLower)
          ++ loop lr `whenMZ` (someRight && someLower)
          ++ loop ul `whenMZ` (someLeft  && someUpper)
          ++ loop ur `whenMZ` (someRight && someUpper)

      someLeft  = minx <= cx
      someRight = maxx >= cx
      someLower = miny <= cy
      someUpper = maxy >= cy

-- Alternative to Monad.when for MonadZero.  Flipped for natural infix
-- reading.
whenMZ :: (MonadZero m) => m a -> Bool -> m a
s `whenMZ` p = if p then s else zero


-- Misc utilities

-- Compare an interval iv with a value x, yielding -1 if iv is fully less
-- than x, 1 if fully greater, and 0 if it straddles x.
cmpIV :: Ord a => a -> a -> a -> Int
cmpIV lo hi x | hi < x    = -1
              | lo > x    =  1
              | otherwise =  0
