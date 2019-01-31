###############################################################
# Makefile for GHC-compiled Fran demos
################################################################

all		:: test.exe tutorial.exe spirokids.exe usersman.exe fifteen.exe
default		:: all

HS =  Test.hs TestMain.hs Tutorial.hs TutMain.hs SpiroKids.hs \
      UsersMan.hs Mover.hs Grid.hs Fifteen.hs

FRAN = ..
include $(FRAN)/app.mk

# The demos.  Potentially reusable ones have .o's here
test.exe	: Test.$(way_)o TestMain.hs
usersman.exe	: UsersMan.$(way_)o
tutorial.exe	: Tutorial.$(way_)o TutMain.hs
spirokids.exe	: Tutorial.$(way_)o SpiroKids.hs
fifteen.exe	: Mover.$(way_)o Grid.$(way_)o Fifteen.hs


# For subdirectories.
all clean depends ::
	cd CurveEditor; make $@
	cd Sokoban; make $@
	cd Roids; make $@

include _depend
