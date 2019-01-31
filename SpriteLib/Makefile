#################################################################
#  Makefile for compiling SpriteLib, and generating gcc-linkable
#  libSpriteLib.a
#################################################################

# choices: Debug, Release, ReleaseStatic.  I want to go with
# ReleaseStatic, but I'm having some problems.
version = ReleaseStatic

default	: $(version)/SpriteLib.lib libSpriteLib.a

# Use VC-generated makefile.  (Also makes .dll !)
$(version)/SpriteLib.lib ::
	nmake -f SpriteLib.mak

libSpriteLib.a : $(version)/SpriteLib.lib
	./gnulib $< -U

#libWintab32.a : Wintab32.lib
#	./gnulib $< -k

# Wintab32.lib requires Wintab32.dll, while Wntab32x.lib probes for it.
# See Readme.doc in the Wintab SDK.
#libWntab32x.a : Wntab32x.lib Makefile
#	dlltool -k --def Wntab32x.def --dllname SpriteLib.dll --output-lib libWntab32x.a

clean ::
	cd $(version); rm -f libSpriteLib.a SpriteLib.def
	nmake -f SpriteLib.mak clean

# No depends
depends ::