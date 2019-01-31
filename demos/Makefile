###############################################################
# Makefile for GHC-compiled Fran demos
################################################################

all		:: 

HS =  Test.hs TestMain.hs Tutorial.hs TutMain.hs SpiroKids.hs \
      UsersMan.hs

FRAN = ..
include $(FRAN)/app.mk

all		:: test$(exe) tutorial$(exe) spirokids$(exe) usersman$(exe)
default		:: all

# The 6Mb default compiler heap size is inadequate for Test.hs
GHC_FLAGS +=-H15M

# The demos.  Potentially reusable ones have .o's here
test$(exe)		: Test.$(way_)o TestMain.$(way_)o
usersman$(exe)	: UsersMan.$(way_)o
tutorial$(exe)	: Tutorial.$(way_)o TutMain.$(way_)o
spirokids$(exe)	: Tutorial.$(way_)o SpiroKids.$(way_)o


SUBDIRS = Collide CurveEditor Fifteen LiftSim Navigate Roids

# For subdirectories.
all clean depends ::
	for d in $(SUBDIRS) ; do \
	   (cd $$d && make $@); \
        done

#GHC_FLAGS += -v

include _depend
