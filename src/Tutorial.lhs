                   Composing Animations in Haskell

                            Conal Elliott

                Last modified Thu Nov 07 12:44:02 1996


Introduction

Affordable personal computers are capable of very impressive 2D
animation and multi-media.  Very soon we will all have interactive 3D
graphics as well.  That's the good news.  The bad news is that very
few people are able to create interactive graphics, and so what might
otherwise be a widely shared medium of communication is instead a tool
for specialists.

This tutorial describes RBMH (Reactive Behavior Modeling in Haskell),
a high level animation library for simplifying the construction of
interactive animations.  This library is a high level vocabulary that
allows one to describe just the essential nature of an animated model,
omitting details of presentation.  Moreover, because this vocabulary
is embedded in a modern functional programming language (Haskell), the
animation models described are composeable in powerful ways.

I do not assume familiarity with Haskell, and have tried to make this
tutorial mostly self-contained.  This is hard to do well, and requires you
to take some of the details on faith for now.  To learn more about
Haskell, I highly recommend the paper "A Gentle Introduction to Haskell".
You'll find this paper and other resources on the following web page:

  http://www.cs.yale.edu/HTML/YALE/CS/haskell/yale-fp.html


How to use this tutorial

This paper is a "literate program".  After loading it into Hugs (with
":l Samples.lhs"), do "disp allAnims".  Press space, "n", or
right-arrow to advance to the next animation, and "p" or left-arrow to
back up to previous one.

Here are some necessary preliminaries that you don't need to understand.

> module Tutorial where

> import RBMH
> import UtilsB
> --import qualified StaticTypes as S
> import ImageBTest (disp, seqImF)


First example

We'll start with a very simple animation, called "anim0", of a doll
moving back and forth.

> anim0 = moveXY wiggle 0 doll

In case you are new to Haskell, this line defines a name "anim0" to be the
result of applying the function "moveXY" to three arguments.  In most
other programming languages, you would instead say something like
"moveXY(wiggle,0,doll)".

The function moveXY takes x and y values and an image, and produces an
image moved horizontally and vertically by the given values.  All
values may be animated.  In this example, the x value is given by a
predefined smoothly animated number called "wiggle".  Wiggle starts
out at zero, increases to one, decreases back past zero to negative
one, and then increases to zero again, all in the course of two
seconds, and then it repeats, forever.

The image argument to moveXY, "doll", is defined by importing a bitmap
file:

> doll = importBitmap "../Media/doll.bmp"


Combining Animations

Next we move on to a simple animation that my daughter Becky and I
cooked up one evening.  This one contains our horizontally wiggling
doll friend, together with a vertically wiggling dude.

> anim1' =
>   moveXY wiggle 0  doll  `over`
>   moveXY 0 wiggle' dude

> dude = importBitmap "../Media/dude.bmp"

To get the vertical movement, we use a nonzero value for the second
argument to moveXY.  Rather than using wiggle, we use wiggle', which
is defined to be just like wiggle, but delayed by a half second.  This
delay makes the dude be at the center when the doll is at her extremes
and vice versa.

The function "over", written above in infix, is what glues two given
animations into a single one.  The first animation is over the second
one.


Digression: models vs presentations

To give you some contrast, I've sketched out very roughly the steps
one goes through to program an animation.

Init: window, DC, bitmaps
REPEAT until quit:
 get time (t)
 clear back buffer
 FOREACH sprite (back to front):
 compute position at t
  draw to back buffer
  fast copy (blit) back buffer to front
Cleanup: bitmaps, DC, window

These steps are usually carried out with lots of tedious, low-level
code that you have to write.

I think a major source of the difficulty in the common approach to
animation is that it jumbles two logically distinct notions, which I
like to call "models" and "presentations".  A model is the animation
itself, while a presentation is how you go about displaying the model
on a temporally discrete computer.

Examples of presentation tasks include the discrete time sampling,
although time is conceptually continuous; and sequentially updating
sprite parameters, although conceptually these parameters vary in
parallel.

For presentation, imperative languages like C, C++, and Java are
natural, while for modeling, declarative languages like Haskell are
appropriate.

Models also have the advantage of being more composable, as
illustrated in the examples below.


Composition

"Composition" is the idea of putting together simpler things to make
more complex things, then putting these together to make even more
complex things, and so on.  The doll-and-dude example already
illustrates composition, but there is a more powerful version, based
on defining functions.

We redefine the previous example as an invocation of a new function we
call "hvDance" (for horizontally and vertical dance).

> hvDance im1 im2 =
>   moveXY wiggle 0  im1  `over`
>   moveXY 0 wiggle' im2
                
> anim1 = hvDance doll dude

Note that hvDance is just like anim1', but doesn't have the doll and
dude hardwired in.  Instead, it takes any two images as arguments.
This time, we've dropped the parentheses, which are implicit.

Having defined this generalized dance animation, we can go on to more
daring compositions.  In anim2, we take anim1, form a smaller version
of it, named "aSmall" here, and then use hvDance again to make aSmall
dance with itself.  The result was pleasantly surprising to me.

> anim2 = hvDance aSmall aSmall
>  where
>   aSmall = smaller 2 anim1

Now let's look at some more variations.

Anim3 is like anim1, but made bigger by a wiggly amount.  To prevent
negative scaling, we take the absolute value of wiggle.

> anim3 = bigger (abs wiggle) anim1

Anim4 uses hvDance again, but give it wiggly-sized doll and dude.
This time, we haven't prevented negative scaling.  For visual balance,
we use wiggle and wiggle'.

> anim4 = hvDance (bigger wiggle  doll)
>                 (bigger wiggle' dude)

Anim5 is not a kind of hvDance.  Instead, we give the doll a wiggly
scale, and we move the dude in a circle.  To get a circular motion, we
use the moveXY function, with wiggle for x and wiggle' for y.

> anim5 = bigger wiggle doll       `over`
>         moveXY wiggle wiggle' dude

If you remember your trig, you've probably surmised that wiggle and
wiggle' are related to sine and cosine.  Check out UtilsB.hs if you're
curious, and you'll see the definitions of wiggle, wiggle', and
several other helpful little tools.


Rate-based animation

One natural way to define an animation is in terms of rates.  The next
example is our dude following the mouse position.

> anim6 t0 = move offset dude
>  where
>   pos    = origin2 .+^ offset
>   offset = atRate rate t0
>   rate   = mouse t0 .-. pos

This time, we're using the function "move", a variant of moveXY that
takes an animated 2D vector.  A vector like a point, but is sometimes
used to describe a *relative location*, which might also be called an
"offset".  If a vector called "offset" is x units horizontally and y
units vertically, then "move offset" is equivalent to "moveXY x y".

The offset vector starts out as zero in X and Y, and grows at a rate
called "rate", thanks to the "atRate" function.  The rate is defined
to be the difference between the mouse's location and our animation's
position "pos".  This difference between two points (given by the
operator ".-.") yields a vector.  In the "rate" case, the vector
combines speed in X and in Y, or, from another point of view, it
combines direction and and speed in that direction.  As a result, the
fellow always pursues the mouse, and moves faster when the the
distance is greater.

The operation ".+^" adds a point (here origin2) and a vector (here
"offset").

The t0 arguments in this example are start times.  For instance, atRate
starts accumulating at time t0, and the mouse gets read starting at t0.
The argument to anim6 is supplied automatically when we say to display.


Spring with drag

Next let's make a chasing animation that acts like it's attached to
the mouse cursor by a spring.  The definition is very similar, with
position defined by a starting point and a growing offset.  This time,
however, the rate is itself changing at a rate we call "accel" (for
acceleration).  The acceleration is defined in part by the difference
between the mouse position and the doll's position, but we also add
some drag that tends to slow down the doll by adding an acceleration
in the direction opposite to her movement.  (Increasing or decreasing
the "drag factor" of 0.5 below creates more or less drag.)

> anim7 t0 = move offset doll
>  where
>   pos    = origin2 .+^ offset
>   offset = atRate rate t0
>   rate   = atRate accel t0
>   accel  = (mouse t0 .-. pos) - 0.5 *^ rate

The operator "*^" multiplies a number by an offset, yielding a new
offset.


Fun with time shifting

This next example was done by Sigbjorn Finne, and is one of my
favorites.  The definition combines two colored markers and a curve.
The curve is defined by four control points, p1 through p4, using the
bezier function.  The second and third points are used to position the
red and blue markers.  The first and fourth point are at fixed
locations, and the second point is at the mouse cursor.  Now, the
really neat part is that the third point, shown blue, is defined as a
two-second delayed version of the second point, shown red.

> anim8 t0 = marker p2 red  `over`
>            marker p3 blue `over`  curve
>  where
>   curve = bezier p1 p2 p3 p4
>   p1    = point2XY (-1) 0
>   p2    = mouse t0
>   p3    = later 2 p2
>   p4    = point2XY 1 0
>   marker p col =
>     move p (smaller 20
>              (withColor col circle))


More fun with time shifting

In the next example, a sequence of words trail behind the cursor.

> anim9 t0 = mouseTrailWords yellow "Time flows like a river" t0

We will define the function "mouseTrailWords" by means of the function
"delayAnims" below.  This function takes a time delay and a list of
animations.  Each successive member of the given animation list is
delayed by a starting time taken from 0, dt, 2*dt, ...

> delayAnims :: Time -> [ImageB] -> ImageB

> delayAnims dt anims =
>  foldr1 over
>   [  later tStart anim | (tStart,anim) <- zip [0, dt ..] anims ]

The notation "[ ... | ...]" above is called "list comprehension" in
Haskell.  It forms a list of values of the expression of the left side of
the "|", using the right hand side to generate values of the variables.
The function "zip" takes two lists (possibly infinite) and zips them
together into one list of pairs.  The "Gentle Introduction" paper explains
         this notation in detail.

We now specialize delayAnims slightly, to take a motion path and a
list of animations:

> trail dt motion anims =
>   delayAnims dt [move motion anim | anim <- anims]

For convenience, we specialize even further to a function
"trailWords".  This specialization takes a string, breaks it up into
separate words (with the standard Haskell prelude function "words"),
turns each word into an image using "stringIm", moves it along a given
motion path, and colors the whole animation with a given color.
       
> trailWords dt motion color str =
>  withColor color (
>   trail dt motion
>    [stringIm word | word <- words str] )

Finally, for ultimate convenience, we define mouseTrailWords to build
in a half-second delay and the mouse as the motion path.

> mouseTrailWords color str t0 =
>  trailWords 0.5 (mouse t0) color str


One more

As yet another example of trailing animations, the following animation
trails a small circle, c, that has a time-varying hue, h.

> anim10 t0 = delayAnims 1 (take 5 (repeat c))
>  where
>   c = move (mouse t0) (
>        smaller 10 (
>         withColor (hsl h 0.5 0.5) circle ) )
>   h = slower 4 (wiggleRange 0 360)


A reactive animation

The animations above are all what might be called "non-reactive",
meaning that they are always doing the same thing.  A "reactive
animation" is one involving discrete changes, due to events.  As a
very simple example, we make a circle whose color starts off red and
changes to blue when the left mouse button is first pressed.

> anim11 t0 = withColor redBlue circle
>  where
>   redBlue = red `untilB` lbp t0 -=> blue

    The argument t0 given to lbp here says that we're interested in the
first left button press after t0.


Cyclic reactivity

To make the previous example more interesting, let's switch between
red and blue every time the left button is pressed.  We accomplish
this change with the help of a function "cycle" that takes two colors,
c1 and c2, and gives an animated color that starts out as c1.  When
the button is pressed, it swaps c1 and c2 and repeats (using
recursion).

> anim12 t0 = withColor (cycle red blue t0) circle
>  where
>   cycle c1 c2 t0 =
>    c1 `untilB` lbp t0 *=> cycle c2 c1

The t0 argument in the recursive call is filled in automatically by
*=>, whose second argument is a function from event time to a
animations.

Just to show some more variety, let's now use three colors, and have
the circle's size change smoothly.

> anim13 t0 =
>   withColor (cycle3 green yellow red t0) (
>     bigger (wiggleRange 0.5 1)
>       circle )
>  where
>   cycle3 c1 c2 c3 t0 =
>    c1 `untilB` lbp t0 *=> cycle3 c2 c3 c1

Exercise: Tweak cycle to work with a list of colors.


Choice

The next example is a flower that starts out in the center, and moves
to left or right when the left or right mouse button is pressed,
returning to the center when the button is released.

(Warning: if you release a button very quickly after pressing it, the
release goes unnoticed.  If this happens, just press the same button
again, holding it a little longer this time, and release it.  The next
two examples have the same problem as well.  This behavior is a bug,
and I will try to fix it soon.)

> anim14 t0 = moveXY (bSign t0) 0 flower

> flower = importBitmap "../Media/flwrblu.bmp"

> bSign t0 =
>   0 `untilB`
>     lbp t0 *=> nonZero (-1) lbr  .|.
>     rbp t0 *=> nonZero   1  rbr
>  where
>   nonZero r stop t1 =
>    r `untilB` stop t1 *=> bSign

What's different here is that when the flower is in the middle, it is
interested in two events at the same time.  This simultaneous interest
is indicated by the ".|." operator in the definition of bSign above.
Pressing the left (or right) button after t0 leads to a behavior
generated by nonZero, which will be -1 (or 1) until the left (or
right) button is released, at which time we switch back to bSign, but
with a start time equal to the button release event time (supplied by
*=>).


Choice-based growth

We can use the function bSign above to control the rate of growth of
an image.  Pressing the left (or right) button causes the image to
shrink (or grow) until released.  Put another way, the rate of growth
is 0, -1, or 1, according to bSign.

> anim15 = grow flower

> grow im t0 = bigger size im
>  where
>   size = 1 + atRate rate t0
>   rate = bSign t0


Exponential growth variation

A very simple tweak to the grow function above causes the image to
grow or shrink at the rate of its own size.

> anim16 = grow' flower

> grow' im t0 = bigger size im
>  where
>    size = 1 + atRate rate t0
>    rate = bSign t0 * size


Appendix: sequencing and titling

The examples above are collected into a list "anims" and combined with
some title animations, as explained below.  First, the animation list.

> anims :: [Time -> ImageB]

> anims = [l talkTitle,
>        l anim0, l anim1, l anim2, l anim3, l anim4, l anim5, anim6,
>        anim7, anim8, anim9, anim10, anim11, anim12, anim13, anim14,
>        anim15, anim16
>        -- ,l questions
>        ] 
>  where 
>    l = later'

> later' = flip later

Note that later' takes an animation (ImageB) and produces a function
from start time to animation, as required by the type of anims.

Here are the animation titles:

> titles = [
>   "First example"
>   ,"Combining animations"
>   ,"Composition"
>   ,"More variations, A"
>   ,"More variations, B"
>   ,"More variations, C"
>   ,"Rate-based animation"
>   ,"Spring with drag"
>   ,"Fun with time shifting"
>   ,"More fun with time shifting"
>   ,"Another one"
>   ,"A reactive behavior"
>   ,"Cyclic reactivity"
>   ,"More action"
>   ,"Choice"
>   ,"Choice-based growth"
>   ,"Exponential growth variation"
>   -- ,"? ? ?"
>   ]

The talk title animation is made up of a "trailing" animated phrase.
Each word is given a oscillating color.  The motion path is a periodic
curve.

> talkTitle =
>  bigger 1.5 $
>  trail 1.2 motion
>    [ withColor (hsl (lift0 h + wiggleColor) 0.5 0.5) (
>        stringIm str)
>    | (str,h) <- zip titleWords
>                     [0, 360 / fromIntegral (length titleWords) .. 359]  ]
>  where
>   motion = point2XY (slower 5 (1.5*wiggle'/5))
>                     (slower 4 (0.9*wiggle /3))
>   wiggleColor = slower 2 (90 * wiggle)
>   titleWords = words "Composing Animations in Haskell"

Now let's look at some simple title animators

> rockT,growT,orbitT :: ImageB -> ImageB

First, a rocking motion, in which we move down, rotate back and forth
slightly, and move back up.

> rockT = moveXY 0 0.1             .
>         turnRight (wiggle / 10)  .
>         moveXY 0 (-0.2)

Then a pulsating scaling:

> growT = bigger (wiggleRange 0.7 1.3)

And finally a small circular motion:

> orbitT = moveXY (0.3*wiggle) (0.3*wiggle')

To compose the title animators we double up the simple ones plus the
identity animator.

> titleAnimators = [ a . a' | a  <- simples, a'  <- simples ]
>  where
>   simples = [id, rockT, growT, orbitT]


We then interleave title animations with the main animations.  Note
that the overall title animation has no title itself.

> animsAndTitles = interleave anims animTitles
>  where
> 
>   animTitles =
>    [  later' (tAnim (tsize str (withColor col (stringIm str))))
>     | (col,str,tAnim) <- zip3 colors titles
>                               (concat (repeat titleAnimators)) ]
> 
>   colors = [ hsl (lift0 h + 30*wiggle) 0.5 0.5 | h <- [0, sep .. 359] ]
> 
>   sep = fromInt 360 / fromInt (length titles)
> 
>   tsize str = bigger (25 / fromInt (length str))
> 
>   interleave (x:xs') ys = x : interleave ys xs'
>   interleave _       _  = []

Finally, we define allAnims, which puts everything together into one
function from start time to animation.  This version lets you step
forward by pressing space, "n", or the right arrow, or step backward
by pressing "p", or the left arrow.  The heart of the definition is
"allRec", which takes an index i and is the i-th animation (counting
titles) until a keypress that steps to i+1 or i-1.  We use "suchThat"
to allow through only valid indices.

> allAnims t0 = allRec t0 0
>  where
>   allRec t0 i = (animsAndTitles!!i) t0 `untilB` newIndex +=> allRec
>     where
>       newIndex = (plusMinus `suchThat` validIndex) t0
>       validIndex i'  =  0 <= i' && i' < lenAnims
>       plusMinus t0 = keyIn " N'" t0 -=> i+1  .|. -- "<space>n<right>"
>                      keyIn "P%"  t0 -=> i-1      -- "<left>p"
>       keyIn chars =
>         keyPress `suchThat` (\(ch,_) -> ch `elem` chars)
>   lenAnims = length animsAndTitles


Questions?

Here's one final animation for a question and answer time:

> questions =
>   smaller (time/4) (
>    turnRight (time/2) (
>     moveXY 0 0.5 (
>      bigger 1.7 (
>       withColor red (stringIm "Questions?") ) ) ) )

