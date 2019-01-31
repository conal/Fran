{- Font properties -}
-- Last modified Sat Sep 07 23:23:11 1996
module Font
       (
        Font(..),   
        Family(..),

        system,     -- :: Font
        timesRoman, -- :: Font
        courier,    -- :: Font
        arial,      -- :: Font 
        symbol,     -- :: Font

        bold,       -- :: Font -> Font
        italic      -- :: Font -> Font

       ) where

data Font
 = Font
     Family   
     Bool    -- bold or not
     Bool    -- italics
   deriving Text

system :: Font
system = Font System False False

timesRoman :: Font
timesRoman = Font TimesRoman False False

courier :: Font
courier = Font Courier False False

arial :: Font
arial = Font Arial False False

symbol :: Font
symbol = Font Symbol False False

{- Font operators, adding attributes -}

bold :: Font -> Font
bold (Font fm _ it) = Font fm True it

italic :: Font -> Font
italic (Font fm bold _) = Font fm bold True

{-
 Representation of font families differs from the AxA type,
 in that we specify concretely the name of the family rather
 than its properties (
-}

{- The standard TrueType collection supplied with 95/NT 
   System isn't TrueType, but I'll toss it in for now.
 -}

data Family
 = System     -- default
 | TimesRoman
 | Courier
 | Arial
 | Symbol
   deriving Text
