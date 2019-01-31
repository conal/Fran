#################################################################
#  Makefile for compiling SpriteLib, and generating gcc-linkable
#  libSpriteLib.a
#################################################################

default	: Debug/SpriteLib.lib libSpriteLib.a

# Use VC-generated makefile.  (Also makes .dll !)
Debug/SpriteLib.lib ::
	nmake -f SpriteLib.mak

libSpriteLib.a : Debug/SpriteLib.lib
	./gnulib $<

clean ::
	cd Debug; rm -f libSpriteLib.a SpriteLib.def
	nmake -f SpriteLib.mak clean

# No depends
depends ::