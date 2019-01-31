# Hugs, GHC and Cygwin path dependencies.  Set INCLUDES before including.
# Also LIBOBJS, if making a .a


HUGSDIR 	= /Haskell/hugs
GHCDIR		= /fptools/bin/i386-unknown-cygwin32/ghc-2.06
GHCLIB		= /fptools/lib/i386-unknown-cygwin32/ghc-2.06

GHC		= $(GHCDIR)/ghc 
HP2PS		= $(GHCDIR)/hp2ps
RM		= rm -f

GCDIR		= /Haskell/green-card
GC		= $(GCDIR)/green-card.exe
# Note use "/Profile" when profiling, until I have a better scheme
#WIN32DIR	= $(GCDIR)/win32ghc/Profile
WIN32DIR	= $(GCDIR)/win32ghc

AR     		= ar clqs
RANLIB 		= ranlib

PS_VIEWER	= /gstools/gsview/gsview32

# Include directories
INCLUDES	= -i$(GHCLIB):$(FRAN)/src:$(FRAN)/src/GHC:$(FRAN)/gc/GHC:$(GCDIR)/win32GHC

# GHC flags
GHC_FLAGS	+= -syslib hbc
GHC_FLAGS	+= -fglasgow-exts -concurrent -recomp
# Profiling.
#GHC_FLAGS	+= -prof -auto
GHC_FLAGS	+= $(INCLUDES)
# For non-optimized compilation
# GHC_FLAGS_ONOT	:= $(GHC_FLAGS)
#GHC_FLAGS	+= -O


################################################################
# Suffix rules taken from the GHC users guide
################################################################

.SUFFIXES	: .lhs .hs .hi .o .c .a

%.hi		: %.o
		@:

%.o		: %.lhs
		$(RM) $@
		$(GHC) $(GHC_FLAGS) -cpp -c $< 

%.o		: %.hs
		$(RM) $@
		$(GHC) $(GHC_FLAGS) -c $<

%.o		: %.c
		gcc -c $<

%.a		: $(OBJS)
		$(RM) $@
		$(AR) $@ $(OBJS)
		ranlib $@
