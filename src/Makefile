###############################################################
# Makefile for Fran (!GHC!)
#
# To do: reuse a lot of ../app.mk
################################################################

# This goes first to make it the default
default		::

HS		= BaseTypes.hs Behavior.hs VectorSpace.hs \
		  BehaviorEvent.hs GBehavior.hs Color.hs ColorB.hs \
		  Event.hs Font.hs Force.hs GeometryB.hs \
		  ImageB.hs Integral.hs Point2.hs \
		  Point2B.hs Point3.hs Point3B.hs Rect.hs RectB.hs \
		  RenderImage.hs \
		  ShowImageB.hs SoundB.hs Spritify.hs \
		  Text.hs TextB.hs Transform2.hs Transform2B.hs \
		  Transform3.hs Transform3B.hs User.hs \
		  UtilsB.hs Vector2.hs Vector2B.hs Vector3.hs \
		  Vector3B.hs VectorSpaceB.hs \
		  FranLoader.hs StaticTypesLoader.hs \
		  GHC/Fran.hs GHC/StaticTypes.hs GHC/Compatibility.hs

FRAN = ..
include $(FRAN)/env.mk

OBJS	 = $(addsuffix .$(way_)o,  $(basename $(HS)))
OBJS	+=  ../gc/GHC/HSpriteLib.$(way_)o 
OBJS	+=  ../gc/errors.obj

INCLUDES	+= -iGHC:../gc/GHC

default		:: libFran$(_way).a

libFran$(_way).a	: $(OBJS)
		$(RM) $@
		$(AR) $@ $(OBJS)
		$(RANLIB) $@

RANLIB		= ranlib
AR		= ar clqs

GHC_FLAGS	+= -H16M
#GHC_FLAGS	+= -cpp $(INCLUDES)
#GHC_FLAGS	+= -prof -auto
GHC_FLAGS_ONOT	:= $(GHC_FLAGS)
#GHC_FLAGS	+= -O

# The following special case is no longer necessary.  Sigbjorn discovered
# the problem, which was my own fault, for using unsafePerformIO and not
# knowing the consequences.
# 
# Modules to compile *without* optimization.  This works
# around a bug with optimized compilation.
#Behavior.$(way_)o : Behavior.hs
#		$(RM) $@
#		$(GHC) $(GHC_FLAGS_ONOT) -c $< -o $@

#
# Only required if you want to set your own no-runnable-threads hook.
#
GHC_FLAGS_EXTRA	+= -optl-u -optl_NoRunnableThreadsHook
LIBS		= -L. -L$(GCDIR)/win32ghc -L../SpriteLib/Debug \
		  -lFran -lWin32

#UNUSED: GUILIBS =-luser32 -lgdi32 -lwinspool -lcomdlg32 -lcomctl32 -ladvapi32

clean		:
		rm -f *.exe *.o *.hi *.a GHC/*.o GHC/*.hi \
		      *.exe *.$(way_)o *.$(way_)hi

#GHC_FLAGS += -v

# Module-specific compilation flags

# auto-profile
Spritify_GHC_FLAGS = -auto

include _depend