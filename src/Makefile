###############################################################
# Makefile for Fran (!GHC!)
#
# To do: reuse a lot of ../app.mk
################################################################

# This goes first to make it the default
default		::

HS		= BaseTypes.hs Behavior.hs VectorSpace.hs \
		  BehaviorEvent.hs GBehavior.hs Color.hs ColorB.hs \
		  Event.hs Font.hs Force.hs Fran.hs GeometryB.hs \
		  ImageB.hs Integral.hs Point2.hs \
		  Point2B.hs Point3.hs Point3B.hs Rect.hs RectB.hs \
		  RenderImage.hs \
		  ShowImageB.hs SoundB.hs Spritify.hs StaticTypes.hs \
		  Text.hs TextB.hs Transform2.hs Transform2B.hs \
		  Transform3.hs Transform3B.hs User.hs \
		  UtilsB.hs Vector2.hs Vector2B.hs Vector3.hs \
		  Vector3B.hs VectorSpaceB.hs \
		  GHC/Compatibility.hs GHC/RandomIO.hs

FRAN = ..
include $(FRAN)/env.mk

OBJS	 = $(addsuffix .$(way_)o,  $(basename $(HS)))
OBJS	+=  ../gc/GHC/HSpriteLib.o ../gc/errors.obj

# Using hbc first, to override standard Random.hs.  To do: switch to the
# standard one when its ready in Hugs.
# (I've removed the use of hbc, and included the relevant source code
#  in GHC/RandomIO. Reason: -syslib hbc in 2.09 shadows some important
#  ghc/hugs modules (e.g., Word is also the name of an hbc module.)
#   -- sof
#
INCLUDES	= -iGHC:$(WIN32DIR):../gc/GHC

default		:: libFran$(_way).a

libFran$(_way).a	: $(OBJS)
		$(RM) $@
		$(AR) $@ $(OBJS)
		$(RANLIB) $@

RANLIB		= ranlib
AR		= ar clqs

GHC_FLAGS	+= -H16M -cpp
GHC_FLAGS	+= $(INCLUDES)
#GHC_FLAGS	+= -prof -auto
GHC_FLAGS_ONOT	:= $(GHC_FLAGS)
GHC_FLAGS	+= -O

# Modules to compile *without* optimization.  This works
# around a bug with optimized compilation.
Behavior.$(way_)o : Behavior.hs
		$(RM) $@
		$(GHC) $(GHC_FLAGS_ONOT) -c $< -o $@

#
# Only required if you want to set your own no-runnable-threads hook.
#
GHC_FLAGS_EXTRA	+= -optl-u -optl_NoRunnableThreadsHook
LIBS		= -L. -L$(GCDIR)/win32ghc -L../SpriteLib/Debug \
		  -lFran -lWin32

#UNUSED: GUILIBS =-luser32 -lgdi32 -lwinspool -lcomdlg32 -lcomctl32 -ladvapi32

clean		:
		rm -f *.exe *.o *.hi *.a GHC\*.o GHC\*.hi

################################################################
# End of Makefile
################################################################
# DO NOT DELETE: Beginning of Haskell dependencies
BaseTypes.o BaseTypes.pc_o : BaseTypes.hs
Behavior.o Behavior.pc_o : Behavior.hs
Behavior.o : ./BaseTypes.hi
Behavior.pc_o : ./BaseTypes.pc_hi
Behavior.o : ./Event.hi
Behavior.pc_o : ./Event.pc_hi
Behavior.o : GHC/Compatibility.hi
Behavior.pc_o : GHC/Compatibility.pc_hi
VectorSpace.o VectorSpace.pc_o : VectorSpace.hs
VectorSpace.o : GHC/Compatibility.hi
VectorSpace.pc_o : GHC/Compatibility.pc_hi
VectorSpace.o : ./BaseTypes.hi
VectorSpace.pc_o : ./BaseTypes.pc_hi
BehaviorEvent.o BehaviorEvent.pc_o : BehaviorEvent.hs
BehaviorEvent.o : ./BaseTypes.hi
BehaviorEvent.pc_o : ./BaseTypes.pc_hi
BehaviorEvent.o : ./Event.hi
BehaviorEvent.pc_o : ./Event.pc_hi
BehaviorEvent.o : ./User.hi
BehaviorEvent.pc_o : ./User.pc_hi
BehaviorEvent.o : ./Behavior.hi
BehaviorEvent.pc_o : ./Behavior.pc_hi
GBehavior.o GBehavior.pc_o : GBehavior.hs
GBehavior.o : ./BaseTypes.hi
GBehavior.pc_o : ./BaseTypes.pc_hi
GBehavior.o : ./Event.hi
GBehavior.pc_o : ./Event.pc_hi
GBehavior.o : ./Behavior.hi
GBehavior.pc_o : ./Behavior.pc_hi
Color.o Color.pc_o : Color.hs
Color.o : ./BaseTypes.hi
Color.pc_o : ./BaseTypes.pc_hi
Color.o : $(WIN32DIR)/Win32.hi
Color.pc_o : $(WIN32DIR)/Win32.pc_hi
Color.o : ../gc/GHC/HSpriteLib.hi
Color.pc_o : ../gc/GHC/HSpriteLib.pc_hi
ColorB.o ColorB.pc_o : ColorB.hs
ColorB.o : ./BaseTypes.hi
ColorB.pc_o : ./BaseTypes.pc_hi
ColorB.o : ./Color.hi
ColorB.pc_o : ./Color.pc_hi
ColorB.o : ./Behavior.hi
ColorB.pc_o : ./Behavior.pc_hi
ColorB.o : ../gc/GHC/HSpriteLib.hi
ColorB.pc_o : ../gc/GHC/HSpriteLib.pc_hi
Event.o Event.pc_o : Event.hs
Event.o : ./BaseTypes.hi
Event.pc_o : ./BaseTypes.pc_hi
Font.o Font.pc_o : Font.hs
Force.o Force.pc_o : Force.hs
Fran.o Fran.pc_o : Fran.hs
Fran.o : ./BaseTypes.hi
Fran.pc_o : ./BaseTypes.pc_hi
Fran.o : ./Event.hi
Fran.pc_o : ./Event.pc_hi
Fran.o : ./Behavior.hi
Fran.pc_o : ./Behavior.pc_hi
Fran.o : ./GBehavior.hi
Fran.pc_o : ./GBehavior.pc_hi
Fran.o : ./BehaviorEvent.hi
Fran.pc_o : ./BehaviorEvent.pc_hi
Fran.o : ./VectorSpace.hi
Fran.pc_o : ./VectorSpace.pc_hi
Fran.o : ./VectorSpaceB.hi
Fran.pc_o : ./VectorSpaceB.pc_hi
Fran.o : ./Vector2B.hi
Fran.pc_o : ./Vector2B.pc_hi
Fran.o : ./Point2B.hi
Fran.pc_o : ./Point2B.pc_hi
Fran.o : ./RectB.hi
Fran.pc_o : ./RectB.pc_hi
Fran.o : ./Vector3B.hi
Fran.pc_o : ./Vector3B.pc_hi
Fran.o : ./Point3B.hi
Fran.pc_o : ./Point3B.pc_hi
Fran.o : ./ColorB.hi
Fran.pc_o : ./ColorB.pc_hi
Fran.o : ./TextB.hi
Fran.pc_o : ./TextB.pc_hi
Fran.o : ./Transform2B.hi
Fran.pc_o : ./Transform2B.pc_hi
Fran.o : ./Transform3B.hi
Fran.pc_o : ./Transform3B.pc_hi
Fran.o : ./SoundB.hi
Fran.pc_o : ./SoundB.pc_hi
Fran.o : ./ImageB.hi
Fran.pc_o : ./ImageB.pc_hi
Fran.o : ./GeometryB.hi
Fran.pc_o : ./GeometryB.pc_hi
Fran.o : ./Integral.hi
Fran.pc_o : ./Integral.pc_hi
Fran.o : ./User.hi
Fran.pc_o : ./User.pc_hi
Fran.o : ./Spritify.hi
Fran.pc_o : ./Spritify.pc_hi
Fran.o : ../gc/GHC/HSpriteLib.hi
Fran.pc_o : ../gc/GHC/HSpriteLib.pc_hi
Fran.o : ./UtilsB.hi
Fran.pc_o : ./UtilsB.pc_hi
Fran.o : ./ShowImageB.hi
Fran.pc_o : ./ShowImageB.pc_hi
GeometryB.o GeometryB.pc_o : GeometryB.hs
GeometryB.o : ./BaseTypes.hi
GeometryB.pc_o : ./BaseTypes.pc_hi
GeometryB.o : ./StaticTypes.hi
GeometryB.pc_o : ./StaticTypes.pc_hi
GeometryB.o : ../gc/GHC/HSpriteLib.hi
GeometryB.pc_o : ../gc/GHC/HSpriteLib.pc_hi
GeometryB.o : ./Behavior.hi
GeometryB.pc_o : ./Behavior.pc_hi
GeometryB.o : ./GBehavior.hi
GeometryB.pc_o : ./GBehavior.pc_hi
GeometryB.o : ./Vector3B.hi
GeometryB.pc_o : ./Vector3B.pc_hi
GeometryB.o : ./ColorB.hi
GeometryB.pc_o : ./ColorB.pc_hi
GeometryB.o : ./SoundB.hi
GeometryB.pc_o : ./SoundB.pc_hi
GeometryB.o : ./ImageB.hi
GeometryB.pc_o : ./ImageB.pc_hi
GeometryB.o : ./Event.hi
GeometryB.pc_o : ./Event.pc_hi
GeometryB.o : ./Transform3B.hi
GeometryB.pc_o : ./Transform3B.pc_hi
GeometryB.o : ./RenderImage.hi
GeometryB.pc_o : ./RenderImage.pc_hi
ImageB.o ImageB.pc_o : ImageB.hs
ImageB.o : ../gc/GHC/HSpriteLib.hi
ImageB.pc_o : ../gc/GHC/HSpriteLib.pc_hi
ImageB.o : ./BaseTypes.hi
ImageB.pc_o : ./BaseTypes.pc_hi
ImageB.o : ./StaticTypes.hi
ImageB.pc_o : ./StaticTypes.pc_hi
ImageB.o : ./Behavior.hi
ImageB.pc_o : ./Behavior.pc_hi
ImageB.o : ./GBehavior.hi
ImageB.pc_o : ./GBehavior.pc_hi
ImageB.o : ./Vector2B.hi
ImageB.pc_o : ./Vector2B.pc_hi
ImageB.o : ./RectB.hi
ImageB.pc_o : ./RectB.pc_hi
ImageB.o : ./ColorB.hi
ImageB.pc_o : ./ColorB.pc_hi
ImageB.o : ./Point2B.hi
ImageB.pc_o : ./Point2B.pc_hi
ImageB.o : ./TextB.hi
ImageB.pc_o : ./TextB.pc_hi
ImageB.o : ./Transform2B.hi
ImageB.pc_o : ./Transform2B.pc_hi
ImageB.o : ./Event.hi
ImageB.pc_o : ./Event.pc_hi
ImageB.o : ./RenderImage.hi
ImageB.pc_o : ./RenderImage.pc_hi
ImageB.o : ./SoundB.hi
ImageB.pc_o : ./SoundB.pc_hi
Integral.o Integral.pc_o : Integral.hs
Integral.o : ./BaseTypes.hi
Integral.pc_o : ./BaseTypes.pc_hi
Integral.o : ./VectorSpace.hi
Integral.pc_o : ./VectorSpace.pc_hi
Integral.o : ./VectorSpaceB.hi
Integral.pc_o : ./VectorSpaceB.pc_hi
Integral.o : ./Behavior.hi
Integral.pc_o : ./Behavior.pc_hi
Integral.o : ./Event.hi
Integral.pc_o : ./Event.pc_hi
Integral.o : ./BehaviorEvent.hi
Integral.pc_o : ./BehaviorEvent.pc_hi
Integral.o : ./Force.hi
Integral.pc_o : ./Force.pc_hi
Integral.o : ./User.hi
Integral.pc_o : ./User.pc_hi
Point2.o Point2.pc_o : Point2.hs
Point2.o : ./BaseTypes.hi
Point2.pc_o : ./BaseTypes.pc_hi
Point2.o : ./VectorSpace.hi
Point2.pc_o : ./VectorSpace.pc_hi
Point2.o : ./Vector2.hi
Point2.pc_o : ./Vector2.pc_hi
Point2.o : ./Force.hi
Point2.pc_o : ./Force.pc_hi
Point2B.o Point2B.pc_o : Point2B.hs
Point2B.o : ./Point2.hi
Point2B.pc_o : ./Point2.pc_hi
Point2B.o : ./Behavior.hi
Point2B.pc_o : ./Behavior.pc_hi
Point3.o Point3.pc_o : Point3.hs
Point3.o : ./BaseTypes.hi
Point3.pc_o : ./BaseTypes.pc_hi
Point3.o : ./VectorSpace.hi
Point3.pc_o : ./VectorSpace.pc_hi
Point3.o : ./Vector3.hi
Point3.pc_o : ./Vector3.pc_hi
Point3.o : ./Force.hi
Point3.pc_o : ./Force.pc_hi
Point3B.o Point3B.pc_o : Point3B.hs
Point3B.o : ./Point3.hi
Point3B.pc_o : ./Point3.pc_hi
Point3B.o : ./Point3.hi
Point3B.pc_o : ./Point3.pc_hi
Point3B.o : ./Behavior.hi
Point3B.pc_o : ./Behavior.pc_hi
Point3B.o : ./BaseTypes.hi
Point3B.pc_o : ./BaseTypes.pc_hi
Point3B.o : ./Vector3B.hi
Point3B.pc_o : ./Vector3B.pc_hi
Rect.o Rect.pc_o : Rect.hs
Rect.o : ./Point2.hi
Rect.pc_o : ./Point2.pc_hi
Rect.o : ./Transform2.hi
Rect.pc_o : ./Transform2.pc_hi
RectB.o RectB.pc_o : RectB.hs
RectB.o : ./Rect.hi
RectB.pc_o : ./Rect.pc_hi
RectB.o : ./Behavior.hi
RectB.pc_o : ./Behavior.pc_hi
RectB.o : ./Point2B.hi
RectB.pc_o : ./Point2B.pc_hi
RenderImage.o RenderImage.pc_o : RenderImage.hs
RenderImage.o : ./BaseTypes.hi
RenderImage.pc_o : ./BaseTypes.pc_hi
RenderImage.o : ./Point2.hi
RenderImage.pc_o : ./Point2.pc_hi
RenderImage.o : ./Color.hi
RenderImage.pc_o : ./Color.pc_hi
RenderImage.o : ./Transform2.hi
RenderImage.pc_o : ./Transform2.pc_hi
RenderImage.o : ./VectorSpace.hi
RenderImage.pc_o : ./VectorSpace.pc_hi
RenderImage.o : ./Vector2.hi
RenderImage.pc_o : ./Vector2.pc_hi
RenderImage.o : ./Font.hi
RenderImage.pc_o : ./Font.pc_hi
RenderImage.o : ./Text.hi
RenderImage.pc_o : ./Text.pc_hi
RenderImage.o : $(WIN32DIR)/Win32.hi
RenderImage.pc_o : $(WIN32DIR)/Win32.pc_hi
RenderImage.o : ../gc/GHC/HSpriteLib.hi
RenderImage.pc_o : ../gc/GHC/HSpriteLib.pc_hi
ShowImageB.o ShowImageB.pc_o : ShowImageB.hs
ShowImageB.o : ../gc/GHC/HSpriteLib.hi
ShowImageB.pc_o : ../gc/GHC/HSpriteLib.pc_hi
ShowImageB.o : ./BaseTypes.hi
ShowImageB.pc_o : ./BaseTypes.pc_hi
ShowImageB.o : ./Transform2.hi
ShowImageB.pc_o : ./Transform2.pc_hi
ShowImageB.o : ./Vector2.hi
ShowImageB.pc_o : ./Vector2.pc_hi
ShowImageB.o : ./VectorSpace.hi
ShowImageB.pc_o : ./VectorSpace.pc_hi
ShowImageB.o : ./Point2.hi
ShowImageB.pc_o : ./Point2.pc_hi
ShowImageB.o : $(WIN32DIR)/Win32.hi
ShowImageB.pc_o : $(WIN32DIR)/Win32.pc_hi
ShowImageB.o : ./Event.hi
ShowImageB.pc_o : ./Event.pc_hi
ShowImageB.o : ./User.hi
ShowImageB.pc_o : ./User.pc_hi
ShowImageB.o : ./RenderImage.hi
ShowImageB.pc_o : ./RenderImage.pc_hi
SoundB.o SoundB.pc_o : SoundB.hs
SoundB.o : ../gc/GHC/HSpriteLib.hi
SoundB.pc_o : ../gc/GHC/HSpriteLib.pc_hi
SoundB.o : ./BaseTypes.hi
SoundB.pc_o : ./BaseTypes.pc_hi
SoundB.o : ./Behavior.hi
SoundB.pc_o : ./Behavior.pc_hi
SoundB.o : ./Event.hi
SoundB.pc_o : ./Event.pc_hi
SoundB.o : ./GBehavior.hi
SoundB.pc_o : ./GBehavior.pc_hi
Spritify.o Spritify.pc_o : Spritify.hs
Spritify.o : ./BaseTypes.hi
Spritify.pc_o : ./BaseTypes.pc_hi
Spritify.o : ./StaticTypes.hi
Spritify.pc_o : ./StaticTypes.pc_hi
Spritify.o : ./Event.hi
Spritify.pc_o : ./Event.pc_hi
Spritify.o : ./Behavior.hi
Spritify.pc_o : ./Behavior.pc_hi
Spritify.o : ./GBehavior.hi
Spritify.pc_o : ./GBehavior.pc_hi
Spritify.o : ./BehaviorEvent.hi
Spritify.pc_o : ./BehaviorEvent.pc_hi
Spritify.o : ./Vector2B.hi
Spritify.pc_o : ./Vector2B.pc_hi
Spritify.o : ./Point2B.hi
Spritify.pc_o : ./Point2B.pc_hi
Spritify.o : ./RectB.hi
Spritify.pc_o : ./RectB.pc_hi
Spritify.o : ./ColorB.hi
Spritify.pc_o : ./ColorB.pc_hi
Spritify.o : ./VectorSpaceB.hi
Spritify.pc_o : ./VectorSpaceB.pc_hi
Spritify.o : ./ImageB.hi
Spritify.pc_o : ./ImageB.pc_hi
Spritify.o : ./SoundB.hi
Spritify.pc_o : ./SoundB.pc_hi
Spritify.o : ./User.hi
Spritify.pc_o : ./User.pc_hi
Spritify.o : ./RenderImage.hi
Spritify.pc_o : ./RenderImage.pc_hi
Spritify.o : ../gc/GHC/HSpriteLib.hi
Spritify.pc_o : ../gc/GHC/HSpriteLib.pc_hi
Spritify.o : ./ShowImageB.hi
Spritify.pc_o : ./ShowImageB.pc_hi
Spritify.o : ./Transform2B.hi
Spritify.pc_o : ./Transform2B.pc_hi
StaticTypes.o StaticTypes.pc_o : StaticTypes.hs
StaticTypes.o : ./BaseTypes.hi
StaticTypes.pc_o : ./BaseTypes.pc_hi
StaticTypes.o : ./VectorSpace.hi
StaticTypes.pc_o : ./VectorSpace.pc_hi
StaticTypes.o : ./Vector2.hi
StaticTypes.pc_o : ./Vector2.pc_hi
StaticTypes.o : ./Point2.hi
StaticTypes.pc_o : ./Point2.pc_hi
StaticTypes.o : ./Transform2.hi
StaticTypes.pc_o : ./Transform2.pc_hi
StaticTypes.o : ./Vector3.hi
StaticTypes.pc_o : ./Vector3.pc_hi
StaticTypes.o : ./Point3.hi
StaticTypes.pc_o : ./Point3.pc_hi
StaticTypes.o : ./Rect.hi
StaticTypes.pc_o : ./Rect.pc_hi
StaticTypes.o : ./Transform3.hi
StaticTypes.pc_o : ./Transform3.pc_hi
StaticTypes.o : ./Color.hi
StaticTypes.pc_o : ./Color.pc_hi
StaticTypes.o : ./Font.hi
StaticTypes.pc_o : ./Font.pc_hi
StaticTypes.o : ./Text.hi
StaticTypes.pc_o : ./Text.pc_hi
Text.o Text.pc_o : Text.hs
Text.o : ./Font.hi
Text.pc_o : ./Font.pc_hi
TextB.o TextB.pc_o : TextB.hs
TextB.o : ./Text.hi
TextB.pc_o : ./Text.pc_hi
TextB.o : ./Behavior.hi
TextB.pc_o : ./Behavior.pc_hi
Transform2.o Transform2.pc_o : Transform2.hs
Transform2.o : ./BaseTypes.hi
Transform2.pc_o : ./BaseTypes.pc_hi
Transform2.o : ./Vector2.hi
Transform2.pc_o : ./Vector2.pc_hi
Transform2.o : ./Point2.hi
Transform2.pc_o : ./Point2.pc_hi
Transform2.o : ./VectorSpace.hi
Transform2.pc_o : ./VectorSpace.pc_hi
Transform2B.o Transform2B.pc_o : Transform2B.hs
Transform2B.o : ./Transform2.hi
Transform2B.pc_o : ./Transform2.pc_hi
Transform2B.o : ./Behavior.hi
Transform2B.pc_o : ./Behavior.pc_hi
Transform2B.o : ./Vector2B.hi
Transform2B.pc_o : ./Vector2B.pc_hi
Transform3.o Transform3.pc_o : Transform3.hs
Transform3.o : ./BaseTypes.hi
Transform3.pc_o : ./BaseTypes.pc_hi
Transform3.o : ./Vector3.hi
Transform3.pc_o : ./Vector3.pc_hi
Transform3.o : ./Point3.hi
Transform3.pc_o : ./Point3.pc_hi
Transform3.o : ../gc/GHC/HSpriteLib.hi
Transform3.pc_o : ../gc/GHC/HSpriteLib.pc_hi
Transform3B.o Transform3B.pc_o : Transform3B.hs
Transform3B.o : ./Transform3.hi
Transform3B.pc_o : ./Transform3.pc_hi
Transform3B.o : ./Vector3B.hi
Transform3B.pc_o : ./Vector3B.pc_hi
Transform3B.o : ./Transform3.hi
Transform3B.pc_o : ./Transform3.pc_hi
Transform3B.o : ./Behavior.hi
Transform3B.pc_o : ./Behavior.pc_hi
Transform3B.o : ./BaseTypes.hi
Transform3B.pc_o : ./BaseTypes.pc_hi
User.o User.pc_o : User.hs
User.o : ./BaseTypes.hi
User.pc_o : ./BaseTypes.pc_hi
User.o : ./Event.hi
User.pc_o : ./Event.pc_hi
User.o : ./StaticTypes.hi
User.pc_o : ./StaticTypes.pc_hi
User.o : ./Vector2B.hi
User.pc_o : ./Vector2B.pc_hi
User.o : ./Point2B.hi
User.pc_o : ./Point2B.pc_hi
User.o : $(WIN32DIR)/Win32.hi
User.pc_o : $(WIN32DIR)/Win32.pc_hi
User.o : ./Behavior.hi
User.pc_o : ./Behavior.pc_hi
User.o : ./Event.hi
User.pc_o : ./Event.pc_hi
User.o : ./GBehavior.hi
User.pc_o : ./GBehavior.pc_hi
UtilsB.o UtilsB.pc_o : UtilsB.hs
UtilsB.o : ./StaticTypes.hi
UtilsB.pc_o : ./StaticTypes.pc_hi
UtilsB.o : ./Force.hi
UtilsB.pc_o : ./Force.pc_hi
UtilsB.o : ./BaseTypes.hi
UtilsB.pc_o : ./BaseTypes.pc_hi
UtilsB.o : ./Behavior.hi
UtilsB.pc_o : ./Behavior.pc_hi
UtilsB.o : ./GBehavior.hi
UtilsB.pc_o : ./GBehavior.pc_hi
UtilsB.o : ./VectorSpaceB.hi
UtilsB.pc_o : ./VectorSpaceB.pc_hi
UtilsB.o : ./Event.hi
UtilsB.pc_o : ./Event.pc_hi
UtilsB.o : ./Vector2B.hi
UtilsB.pc_o : ./Vector2B.pc_hi
UtilsB.o : ./Vector3B.hi
UtilsB.pc_o : ./Vector3B.pc_hi
UtilsB.o : ./Point2B.hi
UtilsB.pc_o : ./Point2B.pc_hi
UtilsB.o : ./ColorB.hi
UtilsB.pc_o : ./ColorB.pc_hi
UtilsB.o : ./TextB.hi
UtilsB.pc_o : ./TextB.pc_hi
UtilsB.o : ./Transform2B.hi
UtilsB.pc_o : ./Transform2B.pc_hi
UtilsB.o : ./Transform3B.hi
UtilsB.pc_o : ./Transform3B.pc_hi
UtilsB.o : ./ImageB.hi
UtilsB.pc_o : ./ImageB.pc_hi
UtilsB.o : ./SoundB.hi
UtilsB.pc_o : ./SoundB.pc_hi
UtilsB.o : ./GeometryB.hi
UtilsB.pc_o : ./GeometryB.pc_hi
UtilsB.o : ./User.hi
UtilsB.pc_o : ./User.pc_hi
UtilsB.o : ./Integral.hi
UtilsB.pc_o : ./Integral.pc_hi
UtilsB.o : ./RenderImage.hi
UtilsB.pc_o : ./RenderImage.pc_hi
UtilsB.o : ../gc/GHC/HSpriteLib.hi
UtilsB.pc_o : ../gc/GHC/HSpriteLib.pc_hi
UtilsB.o : ./Spritify.hi
UtilsB.pc_o : ./Spritify.pc_hi
Vector2.o Vector2.pc_o : Vector2.hs
Vector2.o : ./BaseTypes.hi
Vector2.pc_o : ./BaseTypes.pc_hi
Vector2.o : ./VectorSpace.hi
Vector2.pc_o : ./VectorSpace.pc_hi
Vector2.o : ./Force.hi
Vector2.pc_o : ./Force.pc_hi
Vector2B.o Vector2B.pc_o : Vector2B.hs
Vector2B.o : ./Vector2.hi
Vector2B.pc_o : ./Vector2.pc_hi
Vector2B.o : ./Behavior.hi
Vector2B.pc_o : ./Behavior.pc_hi
Vector3.o Vector3.pc_o : Vector3.hs
Vector3.o : ./BaseTypes.hi
Vector3.pc_o : ./BaseTypes.pc_hi
Vector3.o : ./VectorSpace.hi
Vector3.pc_o : ./VectorSpace.pc_hi
Vector3.o : ./Force.hi
Vector3.pc_o : ./Force.pc_hi
Vector3B.o Vector3B.pc_o : Vector3B.hs
Vector3B.o : ./Vector3.hi
Vector3B.pc_o : ./Vector3.pc_hi
Vector3B.o : ./Vector3.hi
Vector3B.pc_o : ./Vector3.pc_hi
Vector3B.o : ./Behavior.hi
Vector3B.pc_o : ./Behavior.pc_hi
Vector3B.o : ./BaseTypes.hi
Vector3B.pc_o : ./BaseTypes.pc_hi
VectorSpaceB.o VectorSpaceB.pc_o : VectorSpaceB.hs
VectorSpaceB.o : ./BaseTypes.hi
VectorSpaceB.pc_o : ./BaseTypes.pc_hi
VectorSpaceB.o : ./Behavior.hi
VectorSpaceB.pc_o : ./Behavior.pc_hi
VectorSpaceB.o : ./VectorSpace.hi
VectorSpaceB.pc_o : ./VectorSpace.pc_hi
GHC/Compatibility.o GHC/Compatibility.pc_o : GHC/Compatibility.hs
GHC/Compatibility.o : $(WIN32DIR)/Win32.hi
GHC/Compatibility.pc_o : $(WIN32DIR)/Win32.pc_hi
GHC/RandomIO.o GHC/RandomIO.pc_o : GHC/RandomIO.hs
GHC/RandomIO.o : $(WIN32DIR)/Win32.hi
GHC/RandomIO.pc_o : $(WIN32DIR)/Win32.pc_hi
# DO NOT DELETE: End of Haskell dependencies
