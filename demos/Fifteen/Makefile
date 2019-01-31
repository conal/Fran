# Makefile for GHC compilation of Fran-based "fifteen puzzle" demo

all ::

HS = Fifteen.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: fifteen$(exe)

fifteen$(exe) :: $(OBJS)

include _depend