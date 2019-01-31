# Modified from hugs/src/Foreign/Makefile

# This rule goes first to make it the default choice
all		::

# .SUFFIXES	: .ss .ps .c .obj 

HUGSDIR		= e:/conal/hugs970411/src
PLUGINS		= $(HUGSDIR)/Foreign
PLUGIN_INCLUDES	= HUGS.h config.h options.h prelude.h storage.h connect.h

HUGS_INCLUDES	= $(HUGSDIR)/HUGS.h \
		  $(HUGSDIR)/prelude.h \
	 	  $(HUGSDIR)/storage.h \
		  $(HUGSDIR)/connect.h \
		  $(HUGSDIR)/config.h

SPRITEDIR	= ../../SpriteLib
SPRITE_INCLUDES	= $(SPRITEDIR)/DDrawEnv.h \
		  $(SPRITEDIR)/Behavior.h \
		  $(SPRITEDIR)/Surface.h \
		  $(SPRITEDIR)/Sprite.h

PERL		= perl
STRUCTC		= $(PERL) $(HUGSDIR)/structc -I$(HUGSDIR) -I$(PLUGINS)
PRIMC		= $(PERL) $(HUGSDIR)/primc

CC		= cl /nologo
CFLAGS		= -Zi -ML -I$(PLUGINS) -I$(SPRITEDIR) -I$(HUGSDIR)

LD		= cl /nologo
LDFLAGS    	= advapi32.lib
#DLL_FLAGS	= /LD $(HUGSDIR)/hugs.lib
DLL_FLAGS	= /LD -Zi

all		:: HSpriteLib.dll

HSpriteLib.c	: HSpriteLib.ss
                  $(STRUCTC) HSpriteLib.ss -oHSpriteLib.ps
		  $(PRIMC) HSpriteLib
HSpriteLib.obj	: HSpriteLib.c $(SPRITE_INCLUDES)
HSpriteLib.dll	: HSpriteLib.obj $(SPRITEDIR)/Debug/SpriteLib.lib
		  $(LD) $(DLL_FLAGS) $**  $(LDFLAGS)

#$(HUGSDIR)/hugs.lib	: $(HUGSDIR)/hugs.exe


HTest.c		: HTest.ss
                  $(STRUCTC) HTest.ss -oHTest.ps
		  $(PRIMC) HTest
HTest.obj	: HTest.c test.h
HTest.dll	: HTest.obj test.obj
		  $(LD) $(DLL_FLAGS) $**  $(LDFLAGS)


# End of Makefile

