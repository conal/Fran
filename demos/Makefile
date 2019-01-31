###############################################################
# Makefile for Fran (!GHC!)
################################################################

all		:: test.exe tutorial.exe spirokids.exe usersman.exe fifteen.exe
default		:: all

HS =  Test.hs TestMain.hs Tutorial.hs TutMain.hs SpiroKids.hs \
      UsersMan.hs Mover.hs Grid.hs Fifteen.hs

FRAN = ..
include $(FRAN)/app.mk

# The demos.  Potentially reusable ones have .o's here
test.exe	: Test.$(way_)o TestMain.hs
usersman.exe	: Tutorial.$(way_)o TutMain.hs
tutorial.exe	: UsersMan.$(way_)o
spirokids.exe	: Tutorial.$(way_)o SpiroKids.hs
fifteen.exe	: Mover.$(way_)o Grid.$(way_)o Fifteen.hs


# For subdirectories.
all clean depends ::
	cd Sokoban; make $@
	cd Roids; make $@

################################################################
# End of Makefile
################################################################
# DO NOT DELETE: Beginning of Haskell dependencies
Test.o Test.pc_o : Test.hs
Test.o : ../src/Fran.hi
Test.pc_o : ../src/Fran.pc_hi
Test.o : $(WIN32DIR)/Win32.hi
Test.pc_o : $(WIN32DIR)/Win32.pc_hi
Test.o : ../src/StaticTypes.hi
Test.pc_o : ../src/StaticTypes.pc_hi
TestMain.o TestMain.pc_o : TestMain.hs
TestMain.o : ./Test.hi
TestMain.pc_o : ./Test.pc_hi
TestMain.o : ../src/Fran.hi
TestMain.pc_o : ../src/Fran.pc_hi
Tutorial.o Tutorial.pc_o : Tutorial.hs
Tutorial.o : ../src/Fran.hi
Tutorial.pc_o : ../src/Fran.pc_hi
Tutorial.o : $(WIN32DIR)/Win32Key.hi
Tutorial.pc_o : $(WIN32DIR)/Win32Key.pc_hi
Tutorial.o : ../src/StaticTypes.hi
Tutorial.pc_o : ../src/StaticTypes.pc_hi
TutMain.o TutMain.pc_o : TutMain.hs
TutMain.o : ../src/Fran.hi
TutMain.pc_o : ../src/Fran.pc_hi
TutMain.o : ./Tutorial.hi
TutMain.pc_o : ./Tutorial.pc_hi
SpiroKids.o SpiroKids.pc_o : SpiroKids.hs
SpiroKids.o : ../src/Fran.hi
SpiroKids.pc_o : ../src/Fran.pc_hi
SpiroKids.o : ./Tutorial.hi
SpiroKids.pc_o : ./Tutorial.pc_hi
UsersMan.o UsersMan.pc_o : UsersMan.hs
UsersMan.o : ../src/Fran.hi
UsersMan.pc_o : ../src/Fran.pc_hi
UsersMan.o : ../src/StaticTypes.hi
UsersMan.pc_o : ../src/StaticTypes.pc_hi
UsersMan.o : $(WIN32DIR)/Win32Key.hi
UsersMan.pc_o : $(WIN32DIR)/Win32Key.pc_hi
Mover.o Mover.pc_o : Mover.hs
Mover.o : ../src/Fran.hi
Mover.pc_o : ../src/Fran.pc_hi
Mover.o : ./Grid.hi
Mover.pc_o : ./Grid.pc_hi
Mover.o : ../src/GHC/RandomIO.hi
Mover.pc_o : ../src/GHC/RandomIO.pc_hi
Grid.o Grid.pc_o : Grid.hs
Grid.o : ../src/Fran.hi
Grid.pc_o : ../src/Fran.pc_hi
Grid.o : ../src/StaticTypes.hi
Grid.pc_o : ../src/StaticTypes.pc_hi
Fifteen.o Fifteen.pc_o : Fifteen.hs
Fifteen.o : ../src/Fran.hi
Fifteen.pc_o : ../src/Fran.pc_hi
Fifteen.o : ./Mover.hi
Fifteen.pc_o : ./Mover.pc_hi
# DO NOT DELETE: End of Haskell dependencies
