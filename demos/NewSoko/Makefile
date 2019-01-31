all ::

HS = Types.hs Levels.hs ../Machine/Machine.hs Board.hs Play.hs UI.hs Main.hs

FRAN = ../..
include $(FRAN)/app.mk

INCLUDES += -i../Machine

GHC_FLAGS += -O

all :: sokoban$(exe)

sokoban$(exe) : $(OBJS)

include _depend