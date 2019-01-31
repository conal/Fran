# Makefile for GHC compilation of Simon Thompson's lift simulator

all :: 

HS = Button.lhs TwoFloorSimBut2.lhs Main.hs

FRAN = ../..
include $(FRAN)/app.mk

all :: liftSim$(exe)

liftSim$(exe) :: $(OBJS)

include _depend