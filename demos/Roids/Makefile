# Makefile for GHC compilation of Fran-based Asteroids game

all	: roids.exe

HS = Roids.hs Media.hs Collide.hs MainRoids.hs

FRAN = ../..
include $(FRAN)/app.mk

roids.exe : $(OBJS)
# DO NOT DELETE: Beginning of Haskell dependencies
Roids.o : Roids.hs
Roids.o : ../../src/GHC/Fran.hi
Roids.o : ../../src/GHC/StaticTypes.hi
Roids.o : /fptools/lib/i386-unknown-cygwin32/ghc-2.09/win32/Win32Key.hi
Roids.o : ./Media.hi
Roids.o : ./Collide.hi
Media.o : Media.hs
Media.o : ../../src/GHC/Fran.hi
Media.o : ../../src/GHC/StaticTypes.hi
Collide.o : Collide.hs
Collide.o : ../../src/GHC/Fran.hi
Collide.o : ../../src/GHC/StaticTypes.hi
Collide.o : ../../src/GHC/StaticTypes.hi
Collide.o : ./Media.hi
# DO NOT DELETE: End of Haskell dependencies
