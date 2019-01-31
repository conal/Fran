################################################################
# Makefile for SpriteLib (GHC)
################################################################

# This goes first to make it the default
all		: HSpriteLib.$(way_)o ../errors.obj
default		: all

SRCS		= HSpriteLib.gc
HS		= $(patsubst %.gc,%.hs,$(SRCS))

FRAN = ../..
include $(FRAN)/env.mk

LIBOBJS		= $(OBJS) 

# Copy from parent directory, so we don't write over the files
# generated for Hugs
HSpriteLib.gc	: ../HSpriteLib.gc
	$(RM) $@ ; cp $< $@

../errors.obj	: ../errors.c ../errors.h
	cd .. ; make errors.obj

# These specify the input files and some of the intermediate files

objs ::
	echo $(LIBOBJS)

################################################################
# Programs
################################################################

CPP	= gcc -P -E -x c -traditional

# The following lines uses GreenCard compiled with GHC or interpreted by
# Hugs.  Otherwise, get GC from $(FRAN)/env.mk
#GC	    	= $(HUGSDIR)/runhugs -h1m  -F"perl hscpp" $(GCDIR)/src/GreenCard.lhs

################################################################

GHC_FLAGS	+= -H10M
GHC_FLAGS	+= -cpp
GHC_FLAGS	+= -fglasgow-exts -fno-prune-tydecls
GHC_FLAGS	+= -concurrent -optc-freg-struct-return
#GHC_FLAGS	+= -keep-hc-file-too
# GHC_FLAGS	+= -optCrts-Sstderr

GHC_FLAGS	+= -I../../SpriteLib -I..
GHC_FLAGS	+= -optc-Wp,-lang-c

# Comment this one out if you don't want an optimized compile.  With
# it, I got the following error in Fran1/src:
#   Transform3.hs:24: Interface-file parse error: line 30 toks= []
#GHC_FLAGS	+= -O

# GC_FLAGS  	= --target Hugs --include-dir $(GCDIR)/lib/hugs 
# GCPP_FLAGS	= -DTARGET_HUGS
# INCLUDES	= -I $(HUGSDIR)/src

GCLIBDIR        = /usr/fptools/green-card/lib/ghc
GC += -i$(GCLIBDIR)

GC_FLAGS	= --target ghc
# Oh dear, what's the difference between -i and --include-dir ?
#GC_FLAGS       += --include-dir $(WIN32DIR)
# Verbosity, for debugging
#GC_FLAGS += -v
GCPP_FLAGS	= -DTARGET_GHC

################################################################
# Suffix rules taken from the GHC users guide
################################################################

# .PRECIOUS	: .o

%.a		: $(LIBOBJS)
		$(RM) $@
		ar clqs $@ $(LIBOBJS)
		ranlib $@

################################################################
# GreenCard suffix rules
################################################################

.SUFFIXES	: .gc .hs

%.hs		: %.gc
		$(CPP) $(GCPP_FLAGS) $< | perl -pe 's#\\n#\n#g' >$*_cpp.gc
		$(GC) $(GC_FLAGS) -I . $*_cpp.gc
#		rm $*_cpp.gc
		mv $*_cpp.hs $@
#		cp $@ $*.hs.ghc
#		mv $*_cpp.c $*.c

clean		:
		rm -f *.o *.hs *.hi *.a


include _depend
