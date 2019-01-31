###############################################################
# Makefile for Fran (!GHC!)
################################################################

all		:: test.exe tutorial.exe spirokids.exe usersman.exe fifteen.exe

HS =  Test.hs TestMain.hs Tutorial.hs TutMain.hs SpiroKids.hs \
      UsersMan.hs Mover.hs Grid.hs Fifteen.hs

FRAN = ..
include $(FRAN)/app.mk

# The demos.  Potentially reusable ones have .o's here
test.exe	: Test.o     TestMain.hs
usersman.exe	: Tutorial.o TutMain.hs
tutorial.exe	: UsersMan.o
spirokids.exe	: tutorial.o SpiroKids.hs
fifteen.exe	: Mover.o Grid.o Fifteen.hs


# For subdirectories.
all clean depends ::
	cd Sokoban3; make $@
	cd Roids; make $@

################################################################
# End of Makefile
################################################################
# DO NOT DELETE: Beginning of Haskell dependencies
Test.o : Test.hs
Test.o : ../src/Fran.hi
Test.o : /Haskell/green-card/win32ghc/Win32.hi
Test.o : ../src/StaticTypes.hi
TestMain.o : TestMain.hs
TestMain.o : ./Test.hi
TestMain.o : ../src/Fran.hi
Tutorial.o : Tutorial.hs
Tutorial.o : ../src/Fran.hi
Tutorial.o : /Haskell/green-card/win32ghc/Win32Key.hi
TutMain.o : TutMain.hs
TutMain.o : ../src/Fran.hi
TutMain.o : ./Tutorial.hi
SpiroKids.o : SpiroKids.hs
SpiroKids.o : ../src/Fran.hi
SpiroKids.o : ./Tutorial.hi
UsersMan.o : UsersMan.hs
UsersMan.o : ../src/Fran.hi
UsersMan.o : ../src/StaticTypes.hi
UsersMan.o : /Haskell/green-card/win32ghc/Win32Key.hi
Mover.o : Mover.hs
Mover.o : ../src/Fran.hi
Mover.o : ./Grid.hi
Mover.o : ../src/GHC/RandomIO.hi
Grid.o : Grid.hs
Grid.o : ../src/Fran.hi
Grid.o : ../src/StaticTypes.hi
Fifteen.o : Fifteen.hs
Fifteen.o : ../src/Fran.hi
Fifteen.o : ./Mover.hi
# DO NOT DELETE: End of Haskell dependencies
