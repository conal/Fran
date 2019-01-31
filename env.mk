# Hugs, GHC and Cygwin path dependencies.  Set INCLUDES before including.
# Also LIBOBJS, if making a .a


HUGSDIR 	= C:/hugs
GHCDIR		= /fptools/bin/i386-unknown-cygwin32/ghc-2.06
GHCLIB		= /fptools/lib/i386-unknown-cygwin32/ghc-2.06

GHC		= $(GHCDIR)/ghc 
HP2PS		= $(GHCDIR)/hp2ps
RM		= rm -f

GCDIR		= C:/green-card
GCLIBS		= C:/GCLibs
#GC		= $(HUGSDIR)/runhugs $(GCDIR)/src/GreenCard.lhs
#GC		= $(GCDIR)/green-card
GC		= $(GCDIR)/green-card.exe -i$(WIN32DIR)
# Note: when profiling, be sure to use the appropriate win32ghc bit.  I
# don't know how to manage this well, so for now I maintain two
# directories of .o, .a, and .hi files.
#WIN32DIR	= $(GCDIR)/win32ghc
#WIN32DIR	= /usr/fran/win32ghc
WIN32DIR	= $(GCLIBS)/Win32

AR     		= ar clqs
RANLIB 		= ranlib

PS_VIEWER	= /gstools/gsview/gsview32

# Include directories
INCLUDES	= -i$(FRAN)/src:$(FRAN)/src/GHC:$(FRAN)/gc/GHC:$(WIN32DIR)

# GHC flags
GHC_FLAGS	+= -fglasgow-exts -concurrent -recomp
GHC_FLAGS	+= -cpp $(INCLUDES)

# For non-optimized compilation
GHC_FLAGS_ONOT	:= $(GHC_FLAGS)

#Uncomment this if you want it to be
#the default.
#GHC_FLAGS	+= -O

#
# Combining concurrent and profiled-concurrent builds
# inside the same directory.
#
# If $(way) is set to pc, we're compiling
# profiled concurrent code.
#way=pc

# Generate dependencies both for conc and prof-conc
MKDEPENDHS_FLAGS += -optdep-s -optdep$(way) -optdep-o -optdepo

ifneq "$(way)" ""
GHC_FLAGS	  += -hisuf $(way)_hi -osuf o
way_		  := $(way)_
_way		  := _$(way)
endif

# Add -auto/-auto-all etc. below.
ifeq "$(way)" "pc"
GHC_FLAGS	  += -prof
endif
################################################################
# Suffix rules taken from the GHC users guide
################################################################

.SUFFIXES	: .lhs .hs .hi .o .c .a

%.$(way_)hi	: %.$(way_)o
		@:

%.$(way_)o	: %.lhs
		$(RM) $@
		$(GHC) $(GHC_FLAGS) -c $< -o $@ -ohi $*.$(way_)hi

%.$(way_)o	: %.hs
		$(RM) $@
		$(GHC) $(GHC_FLAGS) -c $< -o $@ -ohi $*.$(way_)hi

%.o		: %.c
		gcc -c $<

%.a		: $(OBJS)
		$(RM) $@
		$(AR) $@ $(OBJS)
		ranlib $@

depends	:: $(HS)
		 $(GHC) -M $(MKDEPENDHS_FLAGS) $(GHC_FLAGS) $(HS)

depend	:: depends
