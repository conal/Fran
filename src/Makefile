###############################################################
# Makefile for Fran (!GHC!)
#
# To do: reuse a lot of ../app.mk
################################################################

# This goes first to make it the default
all		: libFran.a

HS		= BaseTypes.hs Behavior.lhs VectorSpace.hs \
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

OBJS		+=  ../gc/GHC/HSpriteLib.o ../gc/errors.obj

# Using hbc first, to override standard Random.hs.  To do: switch to the
# standard one when its ready in Hugs.
INCLUDES	= -iGHC:$(GCDIR)/win32ghc:../gc/GHC:$(GHCLIB)/hslibs/hbc/imports:$(GHCLIB)/imports

libFran.a	: $(OBJS)
		$(RM) $@
		$(AR) $@ $(OBJS)
		$(RANLIB) $@

RANLIB		= ranlib
AR		= ar clqs

GHC_FLAGS	+= -H30M -syslib hbc -cpp
GHC_FLAGS	+= -fglasgow-exts -concurrent -recomp
GHC_FLAGS	+= $(INCLUDES)
#GHC_FLAGS	+= -prof -auto
GHC_FLAGS_ONOT	:= $(GHC_FLAGS)
GHC_FLAGS	+= -O

# Modules to compile *without* optimization.  This works
# around a bug with optimized compilation.
Behavior.o	: Behavior.lhs
		$(RM) $@
		$(GHC) $(GHC_FLAGS_ONOT) -c $<

GHC_FLAGS_EXTRA	+= -optl-u -optl_NoRunnableThreadsHook
LIBS		= -L. -L$(GCDIR)/win32ghc -L../SpriteLib/Debug \
		  -lFran -lWin32
GUILIBS		=-luser32 -lgdi32 -lwinspool -lcomdlg32 -lcomctl32 -ladvapi32

depends	::  	$(HS)
		$(GHCDIR)/mkdependHS -- $(GHC_FLAGS) -- $(HS)

clean		:
		rm -f *.exe *.o *.hi *.a GHC\*.o GHC\*.hi

################################################################
# End of Makefile
################################################################
# DO NOT DELETE: Beginning of Haskell dependencies
BaseTypes.o : BaseTypes.hs
Behavior.o : Behavior.lhs
Behavior.o : ./BaseTypes.hi
Behavior.o : ./Event.hi
Behavior.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Maybe.hi
Behavior.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/MutVar.hi
Behavior.o : /Haskell/green-card/win32ghc/Win32.hi
Behavior.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
Behavior.o : GHC/Compatibility.hi
VectorSpace.o : VectorSpace.hs
VectorSpace.o : GHC/Compatibility.hi
VectorSpace.o : ./BaseTypes.hi
BehaviorEvent.o : BehaviorEvent.hs
BehaviorEvent.o : ./BaseTypes.hi
BehaviorEvent.o : ./Event.hi
BehaviorEvent.o : ./User.hi
BehaviorEvent.o : ./Behavior.hi
BehaviorEvent.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Maybe.hi
BehaviorEvent.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
GBehavior.o : GBehavior.hs
GBehavior.o : ./BaseTypes.hi
GBehavior.o : ./Event.hi
GBehavior.o : ./Behavior.hi
Color.o : Color.hs
Color.o : ./BaseTypes.hi
Color.o : /Haskell/green-card/win32ghc/Win32.hi
Color.o : ../gc/GHC/HSpriteLib.hi
ColorB.o : ColorB.hs
ColorB.o : ./BaseTypes.hi
ColorB.o : ./Color.hi
ColorB.o : ./Behavior.hi
ColorB.o : ../gc/GHC/HSpriteLib.hi
Event.o : Event.hs
Event.o : ./BaseTypes.hi
Event.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Concurrent.hi
Event.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Maybe.hi
Event.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
Font.o : Font.hs
Force.o : Force.hs
Fran.o : Fran.hs
Fran.o : ./BaseTypes.hi
Fran.o : ./Event.hi
Fran.o : ./Behavior.hi
Fran.o : ./GBehavior.hi
Fran.o : ./BehaviorEvent.hi
Fran.o : ./VectorSpace.hi
Fran.o : ./VectorSpaceB.hi
Fran.o : ./Vector2B.hi
Fran.o : ./Point2B.hi
Fran.o : ./RectB.hi
Fran.o : ./Vector3B.hi
Fran.o : ./Point3B.hi
Fran.o : ./ColorB.hi
Fran.o : ./TextB.hi
Fran.o : ./Transform2B.hi
Fran.o : ./Transform3B.hi
Fran.o : ./SoundB.hi
Fran.o : ./ImageB.hi
Fran.o : ./GeometryB.hi
Fran.o : ./Integral.hi
Fran.o : ./User.hi
Fran.o : ./Spritify.hi
Fran.o : ../gc/GHC/HSpriteLib.hi
Fran.o : ./UtilsB.hi
Fran.o : ./ShowImageB.hi
GeometryB.o : GeometryB.hs
GeometryB.o : ./BaseTypes.hi
GeometryB.o : ./StaticTypes.hi
GeometryB.o : ../gc/GHC/HSpriteLib.hi
GeometryB.o : ./Behavior.hi
GeometryB.o : ./GBehavior.hi
GeometryB.o : ./Vector3B.hi
GeometryB.o : ./ColorB.hi
GeometryB.o : ./SoundB.hi
GeometryB.o : ./ImageB.hi
GeometryB.o : ./Event.hi
GeometryB.o : ./Transform3B.hi
GeometryB.o : ./RenderImage.hi
GeometryB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Concurrent.hi
GeometryB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Maybe.hi
ImageB.o : ImageB.hs
ImageB.o : ../gc/GHC/HSpriteLib.hi
ImageB.o : ./BaseTypes.hi
ImageB.o : ./StaticTypes.hi
ImageB.o : ./Behavior.hi
ImageB.o : ./GBehavior.hi
ImageB.o : ./Vector2B.hi
ImageB.o : ./RectB.hi
ImageB.o : ./ColorB.hi
ImageB.o : ./Point2B.hi
ImageB.o : ./TextB.hi
ImageB.o : ./Transform2B.hi
ImageB.o : ./Event.hi
ImageB.o : ./RenderImage.hi
ImageB.o : ./SoundB.hi
ImageB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Maybe.hi
ImageB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Concurrent.hi
ImageB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
Integral.o : Integral.hs
Integral.o : ./BaseTypes.hi
Integral.o : ./VectorSpace.hi
Integral.o : ./VectorSpaceB.hi
Integral.o : ./Behavior.hi
Integral.o : ./Event.hi
Integral.o : ./BehaviorEvent.hi
Integral.o : ./Force.hi
Integral.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
Integral.o : ./User.hi
Point2.o : Point2.hs
Point2.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Prelude.hi
Point2.o : ./BaseTypes.hi
Point2.o : ./VectorSpace.hi
Point2.o : ./Vector2.hi
Point2.o : ./Force.hi
Point2B.o : Point2B.hs
Point2B.o : ./Point2.hi
Point2B.o : ./Behavior.hi
Point3.o : Point3.hs
Point3.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Prelude.hi
Point3.o : ./BaseTypes.hi
Point3.o : ./VectorSpace.hi
Point3.o : ./Vector3.hi
Point3.o : ./Force.hi
Point3B.o : Point3B.hs
Point3B.o : ./Point3.hi
Point3B.o : ./Point3.hi
Point3B.o : ./Behavior.hi
Point3B.o : ./BaseTypes.hi
Point3B.o : ./Vector3B.hi
Rect.o : Rect.hs
Rect.o : ./Point2.hi
Rect.o : ./Transform2.hi
RectB.o : RectB.hs
RectB.o : ./Rect.hi
RectB.o : ./Behavior.hi
RectB.o : ./Point2B.hi
RenderImage.o : RenderImage.hs
RenderImage.o : ./BaseTypes.hi
RenderImage.o : ./Point2.hi
RenderImage.o : ./Color.hi
RenderImage.o : ./Transform2.hi
RenderImage.o : ./VectorSpace.hi
RenderImage.o : ./Vector2.hi
RenderImage.o : ./Font.hi
RenderImage.o : ./Text.hi
RenderImage.o : /Haskell/green-card/win32ghc/Win32.hi
RenderImage.o : /Haskell/green-card/win32ghc/Win32.hi
RenderImage.o : ../gc/GHC/HSpriteLib.hi
RenderImage.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
ShowImageB.o : ShowImageB.hs
ShowImageB.o : ../gc/GHC/HSpriteLib.hi
ShowImageB.o : ./BaseTypes.hi
ShowImageB.o : ./Transform2.hi
ShowImageB.o : ./Vector2.hi
ShowImageB.o : ./VectorSpace.hi
ShowImageB.o : ./Point2.hi
ShowImageB.o : /Haskell/green-card/win32ghc/Win32.hi
ShowImageB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/MutVar.hi
ShowImageB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Monad.hi
ShowImageB.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Concurrent.hi
ShowImageB.o : ./Event.hi
ShowImageB.o : ./User.hi
ShowImageB.o : ./RenderImage.hi
SoundB.o : SoundB.hs
SoundB.o : ../gc/GHC/HSpriteLib.hi
SoundB.o : ./BaseTypes.hi
SoundB.o : ./Behavior.hi
SoundB.o : ./Event.hi
SoundB.o : ./GBehavior.hi
Spritify.o : Spritify.hs
Spritify.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Monad.hi
Spritify.o : ./BaseTypes.hi
Spritify.o : ./StaticTypes.hi
Spritify.o : ./Event.hi
Spritify.o : ./Behavior.hi
Spritify.o : ./GBehavior.hi
Spritify.o : ./BehaviorEvent.hi
Spritify.o : ./Vector2B.hi
Spritify.o : ./Point2B.hi
Spritify.o : ./RectB.hi
Spritify.o : ./ColorB.hi
Spritify.o : ./VectorSpaceB.hi
Spritify.o : ./ImageB.hi
Spritify.o : ./SoundB.hi
Spritify.o : ./User.hi
Spritify.o : ./RenderImage.hi
Spritify.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Concurrent.hi
Spritify.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/MutVar.hi
Spritify.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
Spritify.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/Maybe.hi
Spritify.o : ../gc/GHC/HSpriteLib.hi
Spritify.o : ./ShowImageB.hi
Spritify.o : ./Transform2B.hi
StaticTypes.o : StaticTypes.hs
StaticTypes.o : ./BaseTypes.hi
StaticTypes.o : ./VectorSpace.hi
StaticTypes.o : ./Vector2.hi
StaticTypes.o : ./Point2.hi
StaticTypes.o : ./Transform2.hi
StaticTypes.o : ./Vector3.hi
StaticTypes.o : ./Point3.hi
StaticTypes.o : ./Rect.hi
StaticTypes.o : ./Transform3.hi
StaticTypes.o : ./Color.hi
StaticTypes.o : ./Font.hi
StaticTypes.o : ./Text.hi
Text.o : Text.hs
Text.o : ./Font.hi
TextB.o : TextB.hs
TextB.o : ./Text.hi
TextB.o : ./Behavior.hi
Transform2.o : Transform2.hs
Transform2.o : ./BaseTypes.hi
Transform2.o : ./Vector2.hi
Transform2.o : ./Point2.hi
Transform2.o : ./VectorSpace.hi
Transform2B.o : Transform2B.hs
Transform2B.o : ./Transform2.hi
Transform2B.o : ./Behavior.hi
Transform2B.o : ./Vector2B.hi
Transform3.o : Transform3.hs
Transform3.o : ./BaseTypes.hi
Transform3.o : ./Vector3.hi
Transform3.o : ./Point3.hi
Transform3.o : ../gc/GHC/HSpriteLib.hi
Transform3B.o : Transform3B.hs
Transform3B.o : ./Transform3.hi
Transform3B.o : ./Vector3B.hi
Transform3B.o : ./Transform3.hi
Transform3B.o : ./Behavior.hi
Transform3B.o : ./BaseTypes.hi
User.o : User.hs
User.o : ./BaseTypes.hi
User.o : ./Event.hi
User.o : ./StaticTypes.hi
User.o : ./Vector2B.hi
User.o : ./Point2B.hi
User.o : /Haskell/green-card/win32ghc/Win32.hi
User.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Trace.hi
User.o : ./Behavior.hi
User.o : ./Event.hi
User.o : ./GBehavior.hi
UtilsB.o : UtilsB.hs
UtilsB.o : ./StaticTypes.hi
UtilsB.o : ./Force.hi
UtilsB.o : ./BaseTypes.hi
UtilsB.o : ./Behavior.hi
UtilsB.o : ./GBehavior.hi
UtilsB.o : ./VectorSpaceB.hi
UtilsB.o : ./Event.hi
UtilsB.o : ./Vector2B.hi
UtilsB.o : ./Vector3B.hi
UtilsB.o : ./Point2B.hi
UtilsB.o : ./ColorB.hi
UtilsB.o : ./TextB.hi
UtilsB.o : ./Transform2B.hi
UtilsB.o : ./Transform3B.hi
UtilsB.o : ./ImageB.hi
UtilsB.o : ./SoundB.hi
UtilsB.o : ./GeometryB.hi
UtilsB.o : ./User.hi
UtilsB.o : ./Integral.hi
UtilsB.o : ./RenderImage.hi
UtilsB.o : ../gc/GHC/HSpriteLib.hi
UtilsB.o : ./Spritify.hi
Vector2.o : Vector2.hs
Vector2.o : ./BaseTypes.hi
Vector2.o : ./VectorSpace.hi
Vector2.o : ./Force.hi
Vector2B.o : Vector2B.hs
Vector2B.o : ./Vector2.hi
Vector2B.o : ./Behavior.hi
Vector3.o : Vector3.hs
Vector3.o : ./BaseTypes.hi
Vector3.o : ./VectorSpace.hi
Vector3.o : ./Force.hi
Vector3B.o : Vector3B.hs
Vector3B.o : ./Vector3.hi
Vector3B.o : ./Vector3.hi
Vector3B.o : ./Behavior.hi
Vector3B.o : ./BaseTypes.hi
VectorSpaceB.o : VectorSpaceB.hs
VectorSpaceB.o : ./BaseTypes.hi
VectorSpaceB.o : ./Behavior.hi
VectorSpaceB.o : ./VectorSpace.hi
GHC/Compatibility.o : GHC/Compatibility.hs
GHC/Compatibility.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/PrelNum.hi
GHC/Compatibility.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/imports/GHC.hi
GHC/RandomIO.o : GHC/RandomIO.hs
GHC/RandomIO.o : /Haskell/green-card/win32ghc/Win32.hi
GHC/RandomIO.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.06/hslibs/hbc/imports/Random.hi
# DO NOT DELETE: End of Haskell dependencies
