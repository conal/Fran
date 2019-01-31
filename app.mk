# Include makefile for GHC compilation of a Fran-based program.  See the
# makefiles under demos for sample uses.  It's very easy!  One warning:
# Be sure to define FRAN and HS before including this file

include $(FRAN)/env.mk

OBJS	= $(addsuffix .o,  $(basename $(HS)))


GHC_FLAGS_EXTRA	+= -optl-u -optl_NoRunnableThreadsHook

LIBS		= -L$(FRAN)/src -L$(FRAN)/SpriteLib -L$(WIN32DIR) \
		  -lFran -lWin32 -lSpriteLib
GUILIBS		=-luser32 -lgdi32 -lwinspool -lcomdlg32 -lcomctl32 -ladvapi32


FRANLIBS = $(FRAN)/SpriteLib/libSpriteLib.a $(FRAN)/src/libFran.a 

depends	::  	$(HS)
		$(GHCDIR)/mkdependHS -- $(GHC_FLAGS) -- $(HS)

clean		::
		$(RM) *.exe *.o *.hi *.ps *.hp

# Sample apps.  With these two rules, you just need to give a one-liner
# for each app, stating the .o or .hs files it depends on.  The $^ make
# variable will refer to these dependencies.  See the demos for examples.

%.exe	: $(FRANLIBS)
%.exe	: 
	$(GHC) $(GHC_FLAGS) $(GHC_FLAGS_EXTRA) -o $(basename $@) \
	  $^ $(LIBS) $(GUILIBS)


# Profiling.  Move to $(FRAN)/app.mk when ready

# Automatic profiling cost centers for apps
# For now, done in env.mk
#GHC_FLAGS += -auto

#PHEAP = -H20M

# Make a heap profile.  Don't worry if the .exe went bad.  (Without the
# ||, make would abort.)
%.hp : %.exe
	$(basename $<) +RTS $(PHEAP) -F2s -hC || echo "Error, but continuing"

%.prof : %.exe
	$(basename $<) +RTS $(PHEAP) -F2s -pT || echo "Error, but continuing"

%.time : %.exe
	$(basename $<) +RTS $(PHEAP) -F2s -PT || echo "Error, but continuing"

%.ps : %.hp
	$(HP2PS) $<

%.tps : %.time
	$(HP2PS) < $< > $@

# Bogus targets, for convenience.
%.see	: %.ps
	$(PS_VIEWER) $<
%.tsee	: %.tps
	$(PS_VIEWER) $<

# Something I don't understand: if I make a .see file, something in the
# make process removes the .hp and the .ps afterwards.  The "rm" statement
# is even echoed by make.