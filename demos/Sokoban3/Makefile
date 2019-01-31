###############################################################
# Makefile for Fran (!GHC!)
################################################################

all	: sokoban.exe

HS	= SokoBoard.hs SokoDemo.hs SokoDraw.hs SokoType.hs \
	  Sokoban.hs SokoMain.hs

FRAN = ../..
include $(FRAN)/app.mk

sokoban.exe	: $(OBJS)

################################################################
# End of Makefile
################################################################
# DO NOT DELETE: Beginning of Haskell dependencies
SokoBoard.o : SokoBoard.hs
SokoDemo.o : SokoDemo.hs
SokoDemo.o : ./SokoType.hi
SokoDemo.o : ./Sokoban.hi
SokoDemo.o : ./SokoDraw.hi
SokoDemo.o : ./SokoBoard.hi
SokoDemo.o : ../../src/Fran.hi
SokoDemo.o : /Haskell/green-card/win32ghc/Win32.hi
SokoDraw.o : SokoDraw.hs
SokoDraw.o : ./SokoType.hi
SokoDraw.o : ./Sokoban.hi
SokoDraw.o : ../../src/Fran.hi
SokoDraw.o : ../../src/StaticTypes.hi
SokoType.o : SokoType.hs
Sokoban.o : Sokoban.hs
Sokoban.o : ../../src/Fran.hi
Sokoban.o : ./SokoType.hi
Sokoban.o : /Haskell/green-card/win32ghc/Win32.hi
SokoMain.o : SokoMain.hs
SokoMain.o : ./SokoDemo.hi
# DO NOT DELETE: End of Haskell dependencies
