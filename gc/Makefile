################################################################
# Makefile for Green Card examples
#
# For now, use the GHC-compiled version, green-card.exe.
#
# Note: To compile the automatically-generated HSpriteLib.c, we
# need options.h config.h and GreenCard.h from the Hugs source,
# although these files are not part of the Hugs binary release.
# So, you'll have to get these files somehow.
################################################################

default ::

FRAN = ..
include $(FRAN)/env.mk

FRANSRC = $(FRAN)/src

# This goes first to make it the default
default		:: $(FRANSRC)/HSpriteLib.dll $(FRANSRC)/HSpriteLib.hs

$(FRANSRC)/HSpriteLib.dll : HSpriteLib.dll
	cp $< $@

$(FRANSRC)/HSpriteLib.hs : HSpriteLib.hs
	cp $< $@

GC_FLAGS       += --target Hugs

.SUFFIXES	:
.SUFFIXES	: .gc .dll .c .obj

DLL_FLAGS	= -shared -nostdlib
INCLUDES	= -I$(HUGSDIR)/src -I../SpriteLib -I$(WIN32HUGSDIR)


GC_INCLUDES     = -i$(GCLIBHUGSDIR):$(WIN32HUGSDIR)

# Must manually delete the generated .c and .hs files, because
# green-card.exe wants to append to them!!
.gc.c	:
	$(RM) $*.hs $*.c
	$(GC) $(GC_FLAGS) $(GC_INCLUDES) $*
#	cp $*.hs $*.hs.hugs

HSpriteLib.obj : HSpriteLib.c errors.h
	cl /nologo $(INCLUDES) -c HSpriteLib.c

errors.obj : errors.c errors.h
	cl /nologo -c errors.c

# This one is only here because HSpriteLib.gc contains some stuff at the
# end that should really be in Win32.  Remove later
WIN32LIBS = user32.lib $(WIN32HUGSDIR)/Win32Window.lib

SpriteLibDir = ../SpriteLib/ReleaseStatic

SpriteLibLibs = $(SpriteLibDir)/SpriteLib.lib
# Static version, not working yet
#SpriteLibLibs = $(SpriteLibDir)/SpriteLibStat.lib

HSpriteLib.dll	: HSpriteLib.obj errors.obj $(SpriteLibLibs) 
	cl /nologo /LD $(INCLUDES) -o HSpriteLib.dll HSpriteLib.obj \
	errors.obj ../SpriteLib/wntab32x.lib $(WIN32LIBS) \
	$(SpriteLibLibs)

clean	:
	$(RM) HSpriteLib.c HSpriteLib.hs
	$(RM) *.dll *.obj *.exp *.lib
	$(RM) $(FRANSRC)/HSpriteLib.dll $(FRANSRC)/HSpriteLib.hs
	$(RM) *.hi
	$(RM) *.hs
