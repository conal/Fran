# Makefile for GHC compilation of Curve editor demo

all ::

# To do: automatically generate: EDITORS and MAIN defs, the
# *contents* of Main*.hs, and the main*$(exe) rules.

EDITORS = Editor0.hs Editor1.hs Editor2.hs Editor3.hs Editor4.hs \
          Editor5.hs Editor6.hs Editor7.hs

MAINS = Main0.hs Main1.hs Main2.hs Main3.hs Main4.hs \
        Main5.hs Main6.hs Main7.hs

HS = FileUtils.hs NewCurve.hs Stacker.hs \
     $(EDITORS) $(MAINS)

FRAN = ../..
include $(FRAN)/app.mk

EXES = $(addsuffix $(exe),  $(basename $(MAINS)))

all :: $(EXES)

# It seems a shame to have to list all of the transitive dependencies.

DEPS0 = Editor0.$(way_)o FileUtils.$(way_)o
DEPS1 = Editor1.$(way_)o $(DEPS0)
DEPS2 = Editor2.$(way_)o $(DEPS1)
DEPS3 = Editor3.$(way_)o $(DEPS2)
DEPS4 = Editor4.$(way_)o $(DEPS3)
DEPS5 = Editor5.$(way_)o $(DEPS4) Stacker.$(way_)o
DEPS6 = Editor6.$(way_)o $(DEPS4) Stacker.$(way_)o
DEPS7 = Editor7.$(way_)o $(DEPS4) Stacker.$(way_)o

Main0$(exe) : Main0.$(way_)o $(DEPS0)
Main1$(exe) : Main1.$(way_)o $(DEPS1)
Main2$(exe) : Main2.$(way_)o $(DEPS2)
Main3$(exe) : Main3.$(way_)o $(DEPS3)
Main4$(exe) : Main4.$(way_)o $(DEPS4)
Main5$(exe) : Main5.$(way_)o $(DEPS5)
Main6$(exe) : Main6.$(way_)o $(DEPS6)
Main7$(exe) : Main7.$(way_)o $(DEPS7)

TestEditor$(exe) : TestEditor.$(way_)o $(DEPS7) \
		 Editor6.$(way_)o Editor5.$(way_)o Editor1.$(way_)o NewCurve.$(way_)o

include _depend
