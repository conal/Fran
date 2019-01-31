# Makefile for GHC compilation of Fran-based "fifteen puzzle" demo

all ::

HS = Grid.hs Main.hs Puzzle.hs UI.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: fifteen$(exe)

fifteen$(exe) :: $(OBJS)

include _depend