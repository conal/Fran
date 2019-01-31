# Makefile for GHC compilation of two-handed navigation app

all :: 

HS = Hand.hs Navigate.hs Main.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: navigate$(exe)

navigate$(exe) : $(OBJS)

include _depend