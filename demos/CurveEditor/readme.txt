                   Notes on the Curve Editor Fran Demo

                  Last modified Wed Feb 04 09:45:10 1998


See the accompanying paper, tentatively called "Designing interactive
software using events as first-class values".


Versions:

  1. Simplest.  No visual feedback, no saving.

  2. Add visual feedback (point turns green when grabbable).

  3. Add file saving.

  4. Fixes bug in previous versions: control points recenter on the mouse
     when grabbed.  This version gives them relative motion.  Also factors
     the editor definition for the convenience of later versions.

  5. First try at undo.

  6. Working undo.

  7. First whack at insertion and deletion of curve pieces.


To do:

- Define the types CPoint, CPointB, Curve and CurveB, as they are
  introduced.  Change the definitions as I go, rather than changing the
  names, e.g., "CPoint" instead "S.Point2" and "XPoint".

- Does stacker need to use a stack-valued behavior?  Or can it stick with
  events?

- Change grabbing criterion to control point nearest the mouse.  Since
  this new criterion is global, it must be passed into editXPoint, while
  at the same time depending on information coming out of editXPoint.
  Laziness allows us to keep modularity in spite of this
  interdependence.  (At least hand-wave through a less modular
  alternative.  Is this a straw-man argument?)

- Show tangent lines under some conditions, e.g., when mousing over an
  interior control point.

- Constrain curve endpoints and neighboring interior control points to be
  colinear.

- Enhance to handle insertion and deletion of curve segments.  What
  interface?  Perhaps have a visually distinguished edit point and an
  interface for moving it (e.g., left and right arrow keys).

- Is there a way to avoid copying unchanged definitions from earlier
  modules when they depend (transitively) on changed definitions?  I could
  maybe add some complex parameterization, but it would have to be done
  destructively when the need for change is discovered.  On the other
  hand, this issue is a distraction from my main point, which is to show
  that it's easy to make and evolve these programs.  The evolution can be
  via editing rather than mixed importation and replacement.

- Add a saved/unsaved status indicator to the window frame.  Illustrates
  use of hwnd in the effects function.

- Find out how to give a helpful error message when openFile fails.
  Now we just get "Illegal operation".
