

		TwoFloorSimBut2.lhs

		Simon Thompson

		May 1998

A small modification of TwoFloorSim1.lhs to accommodate a button iterface
for calling the lift to go up or down. Modified May 1998 to include
better naming of TwoFloorSim2.lhs

Changes
^^^^^^^

All that has to be changed are the definitions of 

	upButton
	downButton

and the import of the appropriate definitions from the module Button. We also
add a new top level definition at the end to tie together the button and
the lift simluation itself. Also have to change the imports.

The new definitions are given at the end of
this file. In the body of the file changes to the definitions are 
marked by comments:	-- redefined.
 

Original begins here
^^^^^^^^^^^^^^^^^^^^

For stages on the way to this solution see FullSimX.lhs for X=1..3.

This is a second version of TwoFloorSim.lhs modified to include a better naming
convention.


Scenario.
^^^^^^^^^

The elevator has two floors, upper and lower. A left button press
represents a call to go Up from lower to upper and a right button
press to go Down, from upper to lower. A call to go Up/Down is
discharged when the lift starts to go up or down. Calls to go up or down
can be made at any point during the operation of the lift; for instance,
we could ask to go up while the lift is itself going up, and this will
result in the lift going down and then up again after completing the
current journey up.

> 	module TwoFloorSimBut2 where			 	-- redefined

>	import Fran
>	import Button						-- redefined


>       main = displayU liftSim
>              -- displayUMonC red liftSim

The upper and lower positions of the lift, and the rate of travel up and
down.

>	upper    = constantB (1 - liftHeight / 2)
>	lower    = - upper
>	upRate   = 0.4
>	downRate = upRate

>	liftIm :: ImageB
>       liftHeight :: RealVal
>       (liftIm,liftHeight) = (stretch 0.3 circle, 0.6)
>       --(liftIm,liftHeight) = (stretch 0.3 im, h * 0.3)
>       --  where (im,_,h) = importBitmapWithSize "outside-lift-small.bmp"

The definition itself
^^^^^^^^^^^^^^^^^^^^^

Note that in this lift simluator, the lift is represented by a red blob.
I have elsewhere done graphics for the lift, but these are beside the
point of the simluation itself. The main simulation is provided by
moving an image by xPos and yPos. The lift image is constant for the
sake of this exercise, as it the xPos (sensibly, the lift just moves
vertically). Note that all the rest of the file is a `where' clause for
this top-level function, largely to allow sharing of the User argument, u.

>	liftSim :: User -> ImageB

>	liftSim u
>	  = moveXY xPos yPos liftIm `over` simBlock   -- redefined
>             `over` withColor yellow solidImage
>	    where

>	    xPos,yPos :: RealB

>	    xPos = constantB 0



Main definition and event stepper
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Although the yPos is varying, it is defined from the variable rate, dy.
This is piecewise constant, and so can be defined from using the stepper
function which pieces together piecewise constant behaviours. It is
simpler to do this than to put together the piecewise linear movements
which could form yPos.

>	    yPos = lower + atRate dy u

>	    dy :: RealB

>	    dy = stepper 0 setRate

The initial velocity is zero, and changed by the setRate function,
which ...

>           setRate :: Event Double

... switches betweem upward and downward movement and no movement at all
according to the three events: stay, goUp, goDown. [Here I am identifying 
bindings a,b,... :: Event () with events (with a small `e'). This appears to
capture their effect.]

>           setRate = stop      -=> 0
>                     .|. 
>		      goUp      -=> upRate
>		      .|. 
>		      goDown    -=> -downRate

To define these events we will need to define a number of different
auxiliary objects, including
	- some Boolean conditions (that is BoolB's)
	- some events, which will be external events or will result from
	  sampling some conditions, or predicates
	- some state variables, which also happen to be BoolB's.

Conditions
^^^^^^^^^^

Various Boolean `conditions', that is values of type Behavior Bool.
Compare with predicates below (see note there).
The conditions should be obvious from their names.

>	    atBottom, atTop :: BoolB

>	    atBottom = yPos <=* lower
>	    atTop    = yPos >=* upper

>	    stopped, waitingBottom, waitingTop :: BoolB

>	    stopped = (dy ==* 0)

>	    waitingBottom = atBottom &&* stopped
>	    waitingTop    = atTop &&* stopped

Predicates and other events
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Conditions may well be True for extended periods of time, as shown by
the examples above. On the other hand we can use BoolB's to generate events 
by means of the predicate function. Call these `predicate conditions' or
simply `predicates'. The conditions are turned into events by means of
sampling according to a clock, and we therefore may well want to make
sure that the predicates are only transitorily True in order not to
swamp the system with events. Hence the definitions of the predicates
which follow: we don't simply check the position -- we also check that
still moving. This event itself will ensure that dy is changed shortly
afterwards, and so an event-burst will be avoided.

>	    arriveBottom, arriveTop :: Event ()

>	    arriveBottom = predicate (atBottom &&* dy <* 0) u
>	    arriveTop    = predicate (atTop    &&* dy >* 0) u

Button press events; these will not suffer from event-burst.

>	    upButton, downButton, eitherButton :: Event ()

>	    -- 		upButton   = lbp u	-- redefined
>	    --		downButton = rbp u	-- redefined 

>	    eitherButton = upButton .|. downButton

The state variables, pending etc.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

>	    upPending, downPending, pending :: Behavior Bool

pending is True if either upPending or downPending is -- this reflects
the fact that you have to move (from top or bottom) if either an up or a
down request is pending.

>	    pending =  upPending ||* downPending

How are the individual pending values  generated? They start off False, and are 
set to True by (respectively) a left or a right button press. They are 
reset to False by performing the pending action. That is, an Up event causes
upPending to become False, and similarly for Down and downPending.

>	    upPending   = stepper False (setUp   .|. resetUp)
>	    downPending = stepper False (setDown .|. resetDown)

>	    setUp, setDown :: Event Bool

>	    setUp   = upButton   -=> True
>	    setDown = downButton -=> True

>	    resetUp, resetDown :: Event Bool

>	    resetUp   = goUp   -=> False
>	    resetDown = goDown -=> False

The main events defined
^^^^^^^^^^^^^^^^^^^^^^^

On the basis of what we have defined thus far we can now define the
three events which set dy. Note the use of `whenE` which filters out
(occurrences of) events according to a Boolean condition.

>	    goDown, goUp, stop :: Event ()

We go down if either we arrive at the top with a pending request, or we
are waiting at the top when a button is pressed ...

>	    goDown     = arriveTop    `whenE` pending
>			 .|. 
>			 eitherButton `whenE` waitingTop

... symmetrically, we go down if we arrive at the bottom with a pending 
request, or we are waiting at the bottom when a button is pressed ...

>	    goUp       = arriveBottom `whenE` pending
>			 .|. 
>			 eitherButton `whenE` waitingBottom

... while we stop if we arrive at top or bottom with no pending requests.

>	    stop       = (arriveTop .|. arriveBottom) `whenE` notB pending


New stuff begins here
^^^^^^^^^^^^^^^^^^^^^

This has to be put in here because of the recursion which this
definition uses, and in particular because the User argument is global
to the where clause, as it were, so we can't put anything outside the
where. If redefining, it would be better to have all defs lifted over
the user. Hmm...

The simBlock is the button block which receives the up and down inputs. 
These are given as a stream of Direction events. Uses the function
buttonBlock to be found in the module

>	    simBlock :: ImageB
>	    directions :: Event Direction

>	    (simBlock,directions) = (buttonBlock ((-1.0,0),(-0.5,0.3))
>		 		       [("Up",goUp,Up),("Down",goDown,Down)]
>				       lbp
>				       u)

Redefinition of functions above; note the comments at the top about
these being badly named.

>	    upButton   = directions `suchThat` (==Up)	-=> ()    -- redefined
>	    downButton = directions `suchThat` (==Down)	-=> ()    -- redefined

A data type for directions of commands, which we filter out of the
output stream directions. Note that this definition is *not* part of the
where clause attached to liftSim.

>	data Direction = Up | Down deriving Eq

