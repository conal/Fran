# Hugs, GHC and Cygwin path dependencies.  Set FRAN before including.
# Also LIBOBJS, if making a .a

# How to compile: mc = concurrent.  mr = profiled&concurrent.
way=mc


# Note: I'm unsure whether to use c:/, //c/, or just / for these paths.
# NT and Win9x seem to have different preferences.  Needs experimentation.

HUGSDIR 	= c:/hugs98
GHCDIR		= /ghc/ghc-4.08.2
GHCLIB		= /ghc/ghc-4.08.2

# If $(GHCDIR) isn't on your %PATH%, put it on, or use the following commented defs.
#GHC		= $(GHCDIR)/ghc 
#HP2PS		= $(GHCDIR)/hp2ps
GHC		= ghc 
HP2PS		= hp2ps -c
RM		= rm -f

GCDIR		= /Progra~1/GreenCard/2.00
GCSRC		= $(GCDIR)/src
GCLIBGHCDIR     = $(GCDIR)/lib/ghc
GCLIBHUGSDIR    = $(GCDIR)/lib/hugs
WIN32GHCDIR     = /usr/fptools/src/win32
WIN32HUGSDIR    = $(WIN32GHCDIR)/hugs

GC		= $(GCDIR)/green-card.exe

AR     		= ar clqs
RANLIB 		= ranlib

PS_VIEWER	= /gstools/gsview/gsview32

# Include directories
INCLUDES	= -i$(WIN32GHCDIR):$(GCLIBGHCDIR)

# GHC flags
GHC_FLAGS	+= -fglasgow-exts -concurrent -recomp
GHC_FLAGS	+= -cpp $(INCLUDES)

# For non-optimized compilation
GHC_FLAGS_ONOT	:= $(GHC_FLAGS)

#Uncomment this if you want it to be the default.
#GHC_FLAGS	+= -O

MKDEPENDHS_FLAGS += -optdep-o -optdepo

ifneq "$(way)" ""
GHC_FLAGS	  += -hisuf $(way)_hi -osuf o
way_		  := $(way)_
_way		  := _$(way)
MKDEPENDHS_FLAGS  += -optdep-s -optdep$(way)
endif

ifeq "$(way)" "mr"
GHC_FLAGS	  += -prof
# Add -auto/-auto-all etc. below.
GHC_FLAGS	  += -auto-all
endif

# The next two lines cause the dependencies to be written to and read from
# a file called "_depend" in this directory.  Then I'll be distributing a
# cleaner system.  Builds won't write into makefiles.

MKDEPENDHS_FLAGS += -optdep-f -optdep_depend

depends	:: $(HS)
	$(GHC) -M $(MKDEPENDHS_FLAGS) $(GHC_FLAGS) $(HS)

# synonym
depend	:: depends


################################################################
# Suffix rules taken from the GHC users guide
################################################################

.SUFFIXES	: .lhs .hs .hi .o .c .a

%.$(way_)hi	: %.$(way_)o
		@:

%.$(way_)o	: %.lhs
		$(RM) $@
		$(GHC) $(GHC_FLAGS) $($*_GHC_FLAGS) -c $< -o $@ -ohi $*.$(way_)hi

%.$(way_)o	: %.hs
		$(RM) $@
		$(GHC) $(GHC_FLAGS) $($*_GHC_FLAGS) -c $< -o $@ -ohi $*.$(way_)hi

%.o		: %.c
		gcc -c $<

%.a		: $(OBJS)
		$(RM) $@
		$(AR) $@ $(OBJS)
		ranlib $@
