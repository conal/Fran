                          Two-handed Navigation


This Fran demo allows the user to explore and annotate a map using
two-handed interaction and voice input.  It is directly inspired from an
application by Ken Hinckley written in C++, using OpenGL.

To do:

- Put a real map in there, using the MS mapping engine.  (Distributable?)
  Currently there's just a silly geometric picture.

- Textual and/or voice annotations, using the MS Speech API.

- Alter the view transform, e.g., "zoom in", "zoom out", "north", etc.
  Also change to saved view, e.g., "Redmond", "Nebraska", "Europe", etc.

- Save the current view, with name.


Pan/Zoom/Rotate (PZR) Mode

In PZR mode, we synthesize a translate/uniform-scale/rotate transform X,
such that X a0 = a and X b0 = b, for time-varying points a and b and their
initial values a0 and b0.  First, decompose X into translation and linear
components:

    X = translate d . W

Consider the difference vectors, v0 = b0 - a0 and v = b - a.

  v  =  X b0 - X a0
     =  (W b0 + d) - (W a0 + d)
     =  W b0 - W a0
     =  W (b0 - a0)                 by linearity of W
     =  W v0

In mapping v0 to v, W must stretch v0 to v's length, and rotate v0 to v's
orientation.  Considering v0 and v in polar form:

    v0 = vector2polar r0 theta0
    v  = vector2polar r  theta

We have

    W = scale (r/r0) . rotate (theta-theta0)

The translation vector d then makes up the difference between a and W a0
(or between b and W b0), i.e., a = X a0 = W a0 + d, so

    d = a - W a0

The implementation:

    pzrMode (a0, a) (b0, b) = translate2 d `compose2` w
     where
       w = uscale2 (r / r0) `compose2` rotate2 (theta - theta0)
       d = a .-. w *% constantB a0

       (r0, theta0) = S.vector2PolarCoords (b0 S..-. a0)
       (r , theta ) =   vector2PolarCoords (b    .-. a)