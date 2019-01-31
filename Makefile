# Master Fran makefile.  If you're building from a distribution, do "make
# depends" and then "make".  

all	::

all clean depends ::
	cd SpriteLib; make $@
	cd gc/GHC; make $@
	cd src; make $@
	cd demos; make $@

hugs	::
	cd SpriteLib; nmake -f SpriteLib.mak
	cd gc; make
