# Makefile for GHC compilation of Curve editor demo

# To do: automatically generate: EDITORS and MAIN defs, the
# *contents* of Main*.hs, and the main*.exe rules.

EDITORS = Editor0.hs Editor1.hs Editor2.hs Editor3.hs Editor4.hs \
          Editor5.hs Editor6.hs Editor7.hs

MAINS = Main0.hs Main1.hs Main2.hs Main3.hs Main4.hs \
        Main5.hs Main6.hs Main7.hs TestEditor.hs

HS = FileUtils.hs NewCurve.hs Stacker.hs \
     $(EDITORS) $(MAINS)

EXES = $(addsuffix .exe,  $(basename $(MAINS)))

all : $(EXES)

FRAN = ../..
include $(FRAN)/app.mk

# It seems a shame to have to list all of the transitive dependencies.

DEPS0 = Editor0.o FileUtils.o InputMonitor.o
DEPS1 = Editor1.o $(DEPS0)
DEPS2 = Editor2.o $(DEPS1)
DEPS3 = Editor3.o $(DEPS2)
DEPS4 = Editor4.o $(DEPS3)
DEPS5 = Editor5.o $(DEPS4) Stacker.o
DEPS6 = Editor6.o $(DEPS4) Stacker.o
DEPS7 = Editor7.o $(DEPS4) Stacker.o

Main0.exe : Main0.o $(DEPS0)
Main1.exe : Main1.o $(DEPS1)
Main2.exe : Main2.o $(DEPS2)
Main3.exe : Main3.o $(DEPS3)
Main4.exe : Main4.o $(DEPS4)
Main5.exe : Main5.o $(DEPS5)
Main6.exe : Main6.o $(DEPS6)
Main7.exe : Main7.o $(DEPS7)

TestEditor.exe : TestEditor.o $(DEPS7) \
		 Editor6.o Editor5.o Editor1.o NewCurve.o

include _depend
