# Include makefile for GHC compilation of a Fran-based program.  See the
# makefiles under demos for sample uses.  It's very easy!  One warning:
# Be sure to define FRAN and HS before including this file

include $(FRAN)/env.mk

OBJS	= $(addsuffix .$(way_)o,  $(basename $(HS)))


GHC_FLAGS_EXTRA	+= -optl-u -optl_NoRunnableThreadsHook

#ifneq "$(way)" ""
#LIBS		+= $(FRAN)/src/libFran$(_way).a $(WIN32DIR)/libWin32$(_way).a $(GCLIBDIR)/libgreencard$(_way).a
#LIBS		+= $(FRAN)/src/libFran$(_way).a $(WIN32DIR)/libWin32$(_way).a $(GCLIBDIR)/libgreencard$(_way).a
#else
#LIBS		+= -L$(FRAN)/src -L$(WIN32DIR) -L$(GCLIBDIR) -lFran -lWin32 -lgreencard
#LIBS		+= -L$(FRAN)/src -lFran
#endif

LIBS		+= -L$(FRAN)/src -L$(WIN32DIR) -L$(GCLIBDIR) -lFran$(_way) -lWin32$(_way) -lgreencard$(_way)
LIBS		+= -L$(FRAN)/SpriteLib -lSpriteLib 
#LIBS		+= -lWntab32x

GUILIBS		+= -luser32 -lgdi32

# The main modules Fran and StaticTypes are in $(FRAN)/src/GHC.  Omitting
# $(FRAN)/src eliminates name space pollution, but then GHC can't find the
# interface files it needs.  (Why does it need to?  Aren't all the
# required interfaces in $(FRAN)/src/GHC/{Fran,StaticTypes}.hs??
INCLUDES	+= -i$(FRAN)/src/GHC:$(FRAN)/src:$(FRAN)/gc/GHC

FRANLIBS = $(FRAN)/SpriteLib/libSpriteLib.a $(FRAN)/src/libFran$(_way).a 
#FRANLIBS += $(FRAN)/SpriteLib/libWintab32.a

clean		::
		$(RM) *.$(exe) *.$(way_)o *.$(way_)hi *.ps *.hp

# Sample apps.  With these two rules, you just need to give a one-liner
# for each app, stating the .o or .hs files it depends on.  The $^ make
# variable will refer to these dependencies.  See the demos for examples.

# No suffix for just plain concurrent, since all Fran apps are concurrent
ifeq "$(way)" "mc"
exe = .exe
else
exe = $(_way).exe
endif

# This FRANLIBS dependency line doesn't cause recompilation when
# libFran_mr.a is updated :(
%$(exe)	: $(FRANLIBS)
%$(exe)	:
	$(GHC) $(GHC_FLAGS) $(GHC_FLAGS_EXTRA) -o $(basename $@) \
	  $^ $(LIBS) $(GUILIBS)


PHEAP = -H60M -Sstderr

# Make a heap profile.  Don't worry if the .exe went bad.  (Without the
# ||, make would abort.)

# Define intermediates as `precious', i.e., don't let
# make remove them once the chain of rules that needed them
# have completed.
.PRECIOUS: .hp .prof .time .tps .see .tsee

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
# [see .PRECIOUS statement above -- sof]
