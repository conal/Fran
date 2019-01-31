# Makefile for GHC compilation of Fran-based collisions demo

all ::

HS = Collide.hs SpaceTree.hs SpaceTest.hs Main.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: collide$(exe) spacetest$(exe)

collide$(exe)   : Collide.$(way_)o SpaceTree.$(way_)o Main.$(way_)o
spacetest$(exe) : SpaceTree.$(way_)o SpaceTest.$(way_)o

include _depend