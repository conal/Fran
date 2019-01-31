###############################################################
# Makefile for Fran (!GHC!)
################################################################

all ::

HS	= SokoBoard.hs SokoDemo.hs SokoDraw.hs SokoType.hs \
	  Sokoban.hs SokoMain.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: sokoban$(exe)

sokoban$(exe)	: $(OBJS)


include _depend
