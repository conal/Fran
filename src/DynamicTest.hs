-- Simple test harness for dynamic values, as needed by controls
-- 
-- Last modified Fri Sep 13 16:19:11 1996 by 
--
-- To do: Use "activeOnceEach", i.e., everything in the current event
-- queue once.

module DynamicTest where

import qualified ShowImageB
import qualified Postpone

import qualified ImageBTest as I


import qualified Image
instance Typeable Image.Image  where typeOf = const (App (Tycon "Image") [])

import qualified Win32
-- For now, say that HWNDs are Ints
instance Typeable Win32.HWND where typeOf = const (App (Tycon "Int") [])

-- Move the following definitions to Behavior.hs at some point

import Behavior

behaviorTC = Tycon "Behavior"

instance Typeable a => Typeable (Behavior a) where 
  typeOf b = 
    case b `ats` [0] of { [x] ->
    App behaviorTC  [typeOf x]
    }


-- Some dynamically typed values.

draw        = toDynamic ShowImageB.draw
activateAll = toDynamic Postpone.activateAll
activateOne = toDynamic Postpone.activateOne

-- Dynamic variants of ImageBTest.hs examples

[  di1,  di2,  di3,  di4,  di5,  di6,  di7,  di8,  di9, di10,
  di11, di12, di13, di14, di15, di16, di17, di18, di19 ] =

 map toDynamic
  [  I.i1,  I.i2,  I.i3,  I.i4,  I.i5,  I.i6,  I.i7,  I.i8,  I.i9, I.i10,
    I.i11, I.i12, I.i13, I.i14, I.i15, I.i16, I.i17, I.i18, I.i19 ]


testDebug = toDynamic (debugMessage "In testDebug IO")
