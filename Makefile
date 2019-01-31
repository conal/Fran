# Master Fran makefile.  If you're building from a distribution, do "make
# depends" and then "make".  

default	::

clean depends default ::
#	echo $(SHELL)
	cd SpriteLib; make $@
	cd gc; cd GHC; make $@
	cd src; make $@
	cd demos; make $@

allGHC :: clean depends default

hugs	::
	cd SpriteLib; nmake -f SpriteLib.mak
	cd gc; make

FRAN = .
include env.mk

win32GHCFresh ::
	cd $(WIN32DIR) ; make clean all


# Try loading every demo into Hugs to check for static errors
# There seems to be a Hugs bug that requires cd'ing to the demo
# directory.  They symptom is a complaint that StaticTypes is "not
# previously loaded".
HUGS=$(HUGSDIR)/hugs
staticTest ::
	cd demos; $(HUGS) Fifteen          <nul | grep ERROR || exit 0
	cd demos; $(HUGS) Grid             <nul | grep ERROR || exit 0
	cd demos; $(HUGS) Spiral3D         <nul | grep ERROR || exit 0
	cd demos; $(HUGS) SpiroKids        <nul | grep ERROR || exit 0
	cd demos; $(HUGS) TestMain         <nul | grep ERROR || exit 0
	cd demos; $(HUGS) TutMain          <nul | grep ERROR || exit 0
	cd demos; $(HUGS) UsersMan         <nul | grep ERROR || exit 0
	cd demos/Roids; $(HUGS) MainRoids  <nul | grep ERROR || exit 0
	cd demos/Sokoban; $(HUGS) SokoMain <nul | grep ERROR || exit 0
	cd demos/CurveEditor; $(HUGS) TestEditor <nul | grep ERROR || exit 0

# Note: there's another rule for "depends" in env.mk, and that rule
# generates the following empty dependency section.  It's also why FRAN
# had to get defined above.

include _depend
