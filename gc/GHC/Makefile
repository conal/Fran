################################################################
# Makefile for SpriteLib (GHC)
################################################################

# This goes first to make it the default
all		: HSpriteLib.o ../errors.obj

SRCS		= HSpriteLib.gc
HS		= $(addsuffix .hs, $(basename $(SRCS)))

FRAN = ../..
include $(FRAN)/env.mk

LIBOBJS		= $(OBJS) 

# Copy from parent directory, so we don't write over the files
# generated for Hugs
HSpriteLib.gc	: ../HSpriteLib.gc
	cp $< $@

../errors.obj	: ../errors.c ../errors.h
	cd ..;make errors.obj

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
GHC_FLAGS	+= -fglasgow-exts
GHC_FLAGS	+= -concurrent -optc-freg-struct-return
#GHC_FLAGS	+= -keep-hc-file-too
# GHC_FLAGS	+= -optCrts-Sstderr

# This makes the OBM_ constants available
GHC_FLAGS   	+= '-optc -DOEMRESOURCE'

GHC_FLAGS	+= -i$(GCDIR)/win32ghc -I$(GCDIR)/win32ghc

GHC_FLAGS	+= -I../../SpriteLib -I..
GHC_FLAGS	+= -optc-Wp,-lang-c

# Comment this one out if you don't want an optimized compile.  With
# it, I got the following error in Fran1/src:
#   Transform3.hs:24: Interface-file parse error: line 30 toks= []
#GHC_FLAGS	+= -O

# GC_FLAGS  	= --target Hugs --include-dir $(GCDIR)/lib/hugs 
# GCPP_FLAGS	= -DTARGET_HUGS
# INCLUDES	= -I $(HUGSDIR)/src

# No need for include dir - I just copied StdDIS.gc over
GC_FLAGS	= --target ghc --include-dir $(GCDIR)/win32ghc
GCPP_FLAGS	= -DTARGET_GHC

################################################################
# Suffix rules taken from the GHC users guide
################################################################

# .PRECIOUS	: .o

depends	:: $(HS)
		 $(GHCDIR)/mkdependHS -- $(GHC_FLAGS) -- $(HS)

%.a		: $(LIBOBJS)
		$(RM) $@
		ar clqs $@ $(LIBOBJS)
		ranlib $@

################################################################
# GreenCard suffix rules
################################################################

.SUFFIXES	: .gc .hs

gre: $(HS)

%.hs		: %.gc
		$(CPP) $(GCPP_FLAGS) $< | perl -pe 's#\\n#\n#g' >$*_cpp.gc
		$(GC) $(GC_FLAGS) -I . $*_cpp.gc
#		rm $*_cpp.gc
		mv $*_cpp.hs $@
#		cp $@ $*.hs.ghc
#		mv $*_cpp.c $*.c

clean		:
		rm -f *.o *.hs *.hi *.a

################################################################
# End of Makefile
################################################################
# DO NOT DELETE: Beginning of Haskell dependencies
HSpriteLib.o : HSpriteLib.hs
HSpriteLib.o : /Haskell/green-card/win32ghc/StdDIS.hi
HSpriteLib.o : /Haskell/green-card/win32ghc/Win32.hi
# DO NOT DELETE: End of Haskell dependencies
