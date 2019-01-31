################################################################
# Makefile for Green Card examples
#
# For now, use the GHC-compiled version, green-card.exe.
################################################################

# This goes first to make it the default
all		: HSpriteLib.dll

FRAN = ..
include $(FRAN)/env.mk

HUGSDIR 	= C:/Haskell/hugs
GC	    	+= --target Hugs
GC_INCLUDES 	= --include-dir $(GREENCARD)/lib/hugs --include-dir C:/Haskell/green-card/win32

.SUFFIXES	:
.SUFFIXES	: .gc .dll .c .obj

DLL_FLAGS	= -shared -nostdlib
INCLUDES	= -I$(HUGSDIR)/src -I../SpriteLib

# Must manually delete the generated .c and .hs files, because
# green-card.exe wants to append to them!!
.gc.c	: 
	rm -f $*.hs $*.c
	$(GC) $(GC_INCLUDES) $*.gc
	cp $*.hs $*.hs.hugs

HSpriteLib.obj : HSpriteLib.c errors.h
	cl /nologo $(INCLUDES) -c HSpriteLib.c

errors.obj : errors.c errors.h
	cl /nologo -c errors.c

HSpriteLib.dll	: HSpriteLib.obj errors.obj ../SpriteLib/Debug/SpriteLib.lib
	cl /nologo /LD $(INCLUDES) -o HSpriteLib.dll HSpriteLib.obj \
	errors.obj ../SpriteLib/Debug/SpriteLib.lib


clean	:
	rm -f HSpriteLib.c HSpriteLib.hs
	rm -f *.dll *.obj *.exp *.lib 
	rm -f *.so *.o 
	rm -f *.hi
	rm -f *.hs

################################################################
# End of Makefile
################################################################
