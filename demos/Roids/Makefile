# Makefile for GHC compilation of Fran-based Asteroids game

all :: 

HS = Roids.hs Media.hs Collide.hs MainRoids.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: roids$(exe)

roids$(exe) : $(OBJS)

include _depend