# Master Fran makefile.  If you're building from a distribution, do "make
# depends" and then "make".  

default	::

clean depends default ::
	echo $(SHELL)
	cd SpriteLib; make $@
	cd gc; cd GHC; make $@
	cd src; make $@
	cd demos; make $@

allGHC :: win32GHCFresh clean depends default

hugs	::
	cd SpriteLib; nmake -f SpriteLib.mak
	cd gc; make

include env.mk

win32GHCFresh ::
	cd $(WIN32DIR) ; make clean all
