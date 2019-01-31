# Hugs, GHC and Cygwin path dependencies.  Set FRAN before including.
# Also LIBOBJS, if making a .a

# Note: I'm unsure whether to use c:/, //c/, or just / for these paths.
# NT and Win95 seem to have different preferences.  Needs experimentation.

HUGSDIR 	= /hugs
GHCDIR		= /usr/fptools/bin/i386-unknown-cygwin32/ghc-2.10
GHCLIB		= /usr/fptools/lib/i386-unknown-cygwin32/ghc-2.10

# If $(GHCDIR) isn't on your %PATH%, put it on, or use the following commented defs.
#GHC		= $(GHCDIR)/ghc 
#HP2PS		= $(GHCDIR)/hp2ps
GHC		= ghc 
HP2PS		= hp2ps
RM		= rm -f

GCDIR		= /usr/fptools/green-card/src
WIN32DIR        = /usr/fptools/hslibs/win32/src

GC		= $(GCDIR)/green-card.exe -i$(WIN32DIR)

AR     		= ar clqs
RANLIB 		= ranlib

PS_VIEWER	= /gstools/gsview/gsview32

# Include directories
#  Hmm... without the src directory, lots of .hi files are not found.
#  This situation seems wrong.
#INCLUDES	= -i$(FRAN)/src:$(FRAN)/src/GHC:$(FRAN)/gc/GHC:$(WIN32DIR):$(GCLIBDIR)
INCLUDES	= -i$(WIN32DIR)

# GHC flags
GHC_FLAGS	+= -fglasgow-exts -concurrent -recomp
GHC_FLAGS	+= -cpp $(INCLUDES)

# For non-optimized compilation
GHC_FLAGS_ONOT	:= $(GHC_FLAGS)

#Uncomment this if you want it to be the default.
#GHC_FLAGS	+= -O

#
# Combining concurrent and profiled-concurrent builds
# inside the same directory.
#
# If $(way) is set to pc, we're compiling
# profiled concurrent code.
#way=pc

# Generate dependencies both for conc and prof-conc
#  This doesn't work when $(way) is "".
#  Should it go into the next conditional, or should it have pc wired in?
#MKDEPENDHS_FLAGS += -optdep-s -optdep$(way) -optdep-o -optdepo

ifneq "$(way)" ""
GHC_FLAGS	  += -hisuf $(way)_hi -osuf o
way_		  := $(way)_
_way		  := _$(way)
endif

# Add -auto/-auto-all etc. below.
ifeq "$(way)" "pc"
GHC_FLAGS	  += -prof
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
