################################################################
# Makefile for Green Card examples
#
# For now, use the GHC-compiled version, green-card.exe.
################################################################

# This goes first to make it the default
all		: HSpriteLib.dll

GCDIR		= C:/Haskell/green-card
HUGSDIR 	= C:/Haskell/hugs
RUNHUGS		= $(HUGSDIR)/runhugs
#GC	    	= $(RUNHUGS) -h1m $(GCDIR)/src/GreenCard.lhs --target Hugs
#GC_GHC		= $(RUNHUGS) -h1m $(GCDIR)/src/GreenCard.lhs --target ghc
GC	    	= $(GCDIR)/green-card.exe --target Hugs
GC_GHC		= $(GCDIR)/green-card.exe --target ghc
GC_INCLUDES 	= --include-dir $(GCDIR)/lib/hugs --include-dir C:/Haskell/green-card/win32

.SUFFIXES	:
.SUFFIXES	: .gc .dll .c .obj

DLL_FLAGS	= -shared -nostdlib
INCLUDES	= -I$(HUGSDIR)/src -I../SpriteLib

# Must manually delete the generated .c and .hs files, because
# green-card.exe wants to append to them!!
.gc.c	: 
	rm -f $*.hs $*.c
	$(GC) $(GC_INCLUDES) $*.gc

HSpriteLib.obj : HSpriteLib.c
	cl /nologo $(INCLUDES) -c HSpriteLib.c

HSpriteLib.dll	: HSpriteLib.obj ../SpriteLib/Debug/SpriteLib.lib
	cl /nologo /LD $(INCLUDES) -o HSpriteLib.dll HSpriteLib.obj ../SpriteLib/Debug/SpriteLib.lib


clean	:
	rm -f *.dll *.obj *.exp *.lib 
	rm -f *.so *.o 
	rm -f *.hi
	rm -f *.hs *.c

################################################################
# End of Makefile
################################################################
