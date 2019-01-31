# Microsoft Developer Studio Generated NMAKE File, Based on SpriteLib.dsp
!IF "$(CFG)" == ""
CFG=SpriteLib - Win32 ReleaseStatic
!MESSAGE No configuration specified. Defaulting to SpriteLib - Win32\
 ReleaseStatic.
!ENDIF 

!IF "$(CFG)" != "SpriteLib - Win32 Debug" && "$(CFG)" !=\
 "SpriteLib - Win32 Release" && "$(CFG)" != "SpriteLib - Win32 ReleaseStatic"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "SpriteLib.mak" CFG="SpriteLib - Win32 ReleaseStatic"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SpriteLib - Win32 Debug" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "SpriteLib - Win32 Release" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE "SpriteLib - Win32 ReleaseStatic" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\SpriteLib.dll" "$(OUTDIR)\SpriteLib.bsc"\
 "%windir%\SpriteLib.dll"

!ELSE 

ALL : "$(OUTDIR)\SpriteLib.dll" "$(OUTDIR)\SpriteLib.bsc"\
 "%windir%\SpriteLib.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\Behavior.obj"
	-@erase "$(INTDIR)\Behavior.sbr"
	-@erase "$(INTDIR)\ddcheck.obj"
	-@erase "$(INTDIR)\ddcheck.sbr"
	-@erase "$(INTDIR)\ddhelp.obj"
	-@erase "$(INTDIR)\ddhelp.sbr"
	-@erase "$(INTDIR)\DDrawEnv.obj"
	-@erase "$(INTDIR)\DDrawEnv.sbr"
	-@erase "$(INTDIR)\ddutil.obj"
	-@erase "$(INTDIR)\ddutil.sbr"
	-@erase "$(INTDIR)\SimpleTablet.obj"
	-@erase "$(INTDIR)\SimpleTablet.sbr"
	-@erase "$(INTDIR)\Sprite.obj"
	-@erase "$(INTDIR)\Sprite.sbr"
	-@erase "$(INTDIR)\SpriteEngine.obj"
	-@erase "$(INTDIR)\SpriteEngine.sbr"
	-@erase "$(INTDIR)\SpriteLib.obj"
	-@erase "$(INTDIR)\SpriteLib.pch"
	-@erase "$(INTDIR)\SpriteLib.res"
	-@erase "$(INTDIR)\SpriteLib.sbr"
	-@erase "$(INTDIR)\StdAfx.obj"
	-@erase "$(INTDIR)\StdAfx.sbr"
	-@erase "$(INTDIR)\VBlankHandler.obj"
	-@erase "$(INTDIR)\VBlankHandler.sbr"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(INTDIR)\wave.obj"
	-@erase "$(INTDIR)\wave.sbr"
	-@erase "$(INTDIR)\waveBuffer.obj"
	-@erase "$(INTDIR)\waveBuffer.sbr"
	-@erase "$(OUTDIR)\SpriteLib.bsc"
	-@erase "$(OUTDIR)\SpriteLib.dll"
	-@erase "$(OUTDIR)\SpriteLib.exp"
	-@erase "$(OUTDIR)\SpriteLib.lib"
	-@erase "%windir%\SpriteLib.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /YX"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\SpriteLib.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SpriteLib.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\Behavior.sbr" \
	"$(INTDIR)\ddcheck.sbr" \
	"$(INTDIR)\ddhelp.sbr" \
	"$(INTDIR)\DDrawEnv.sbr" \
	"$(INTDIR)\ddutil.sbr" \
	"$(INTDIR)\SimpleTablet.sbr" \
	"$(INTDIR)\Sprite.sbr" \
	"$(INTDIR)\SpriteEngine.sbr" \
	"$(INTDIR)\SpriteLib.sbr" \
	"$(INTDIR)\StdAfx.sbr" \
	"$(INTDIR)\VBlankHandler.sbr" \
	"$(INTDIR)\wave.sbr" \
	"$(INTDIR)\waveBuffer.sbr"

"$(OUTDIR)\SpriteLib.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=ddraw.lib dsound.lib d3drm.lib winmm.lib wntab32x.lib /nologo\
 /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386\
 /def:".\SpriteLib.def" /out:"$(OUTDIR)\SpriteLib.dll"\
 /implib:"$(OUTDIR)\SpriteLib.lib" 
DEF_FILE= \
	".\SpriteLib.def"
LINK32_OBJS= \
	"$(INTDIR)\Behavior.obj" \
	"$(INTDIR)\ddcheck.obj" \
	"$(INTDIR)\ddhelp.obj" \
	"$(INTDIR)\DDrawEnv.obj" \
	"$(INTDIR)\ddutil.obj" \
	"$(INTDIR)\SimpleTablet.obj" \
	"$(INTDIR)\Sprite.obj" \
	"$(INTDIR)\SpriteEngine.obj" \
	"$(INTDIR)\SpriteLib.obj" \
	"$(INTDIR)\SpriteLib.res" \
	"$(INTDIR)\StdAfx.obj" \
	"$(INTDIR)\VBlankHandler.obj" \
	"$(INTDIR)\wave.obj" \
	"$(INTDIR)\waveBuffer.obj"

"$(OUTDIR)\SpriteLib.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

InputPath=.\Debug\SpriteLib.dll
SOURCE=$(InputPath)

"%windir%\SpriteLib.dll"	 : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy Debug\SpriteLib.dll  %windir%%

!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\SpriteLib.dll" "%windir%\SpriteLib.dll"

!ELSE 

ALL : "$(OUTDIR)\SpriteLib.dll" "%windir%\SpriteLib.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\Behavior.obj"
	-@erase "$(INTDIR)\ddcheck.obj"
	-@erase "$(INTDIR)\ddhelp.obj"
	-@erase "$(INTDIR)\DDrawEnv.obj"
	-@erase "$(INTDIR)\ddutil.obj"
	-@erase "$(INTDIR)\SimpleTablet.obj"
	-@erase "$(INTDIR)\Sprite.obj"
	-@erase "$(INTDIR)\SpriteEngine.obj"
	-@erase "$(INTDIR)\SpriteLib.obj"
	-@erase "$(INTDIR)\SpriteLib.pch"
	-@erase "$(INTDIR)\SpriteLib.res"
	-@erase "$(INTDIR)\StdAfx.obj"
	-@erase "$(INTDIR)\VBlankHandler.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\wave.obj"
	-@erase "$(INTDIR)\waveBuffer.obj"
	-@erase "$(OUTDIR)\SpriteLib.dll"
	-@erase "$(OUTDIR)\SpriteLib.exp"
	-@erase "$(OUTDIR)\SpriteLib.lib"
	-@erase "%windir%\SpriteLib.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\SpriteLib.res" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SpriteLib.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ddraw.lib dsound.lib d3drm.lib winmm.lib wntab32x.lib /nologo\
 /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386\
 /def:".\SpriteLib.def" /out:"$(OUTDIR)\SpriteLib.dll"\
 /implib:"$(OUTDIR)\SpriteLib.lib" 
DEF_FILE= \
	".\SpriteLib.def"
LINK32_OBJS= \
	"$(INTDIR)\Behavior.obj" \
	"$(INTDIR)\ddcheck.obj" \
	"$(INTDIR)\ddhelp.obj" \
	"$(INTDIR)\DDrawEnv.obj" \
	"$(INTDIR)\ddutil.obj" \
	"$(INTDIR)\SimpleTablet.obj" \
	"$(INTDIR)\Sprite.obj" \
	"$(INTDIR)\SpriteEngine.obj" \
	"$(INTDIR)\SpriteLib.obj" \
	"$(INTDIR)\SpriteLib.res" \
	"$(INTDIR)\StdAfx.obj" \
	"$(INTDIR)\VBlankHandler.obj" \
	"$(INTDIR)\wave.obj" \
	"$(INTDIR)\waveBuffer.obj"

"$(OUTDIR)\SpriteLib.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

InputPath=.\Release\SpriteLib.dll
SOURCE=$(InputPath)

"%windir%\SpriteLib.dll"	 : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy Release\SpriteLib.dll  %windir%%

!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

OUTDIR=.\ReleaseStatic
INTDIR=.\ReleaseStatic
# Begin Custom Macros
OutDir=.\.\ReleaseStatic
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\SpriteLib.dll" "%windir%\SpriteLib.dll"

!ELSE 

ALL : "$(OUTDIR)\SpriteLib.dll" "%windir%\SpriteLib.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\Behavior.obj"
	-@erase "$(INTDIR)\ddcheck.obj"
	-@erase "$(INTDIR)\ddhelp.obj"
	-@erase "$(INTDIR)\DDrawEnv.obj"
	-@erase "$(INTDIR)\ddutil.obj"
	-@erase "$(INTDIR)\SimpleTablet.obj"
	-@erase "$(INTDIR)\Sprite.obj"
	-@erase "$(INTDIR)\SpriteEngine.obj"
	-@erase "$(INTDIR)\SpriteLib.obj"
	-@erase "$(INTDIR)\SpriteLib.pch"
	-@erase "$(INTDIR)\SpriteLib.res"
	-@erase "$(INTDIR)\StdAfx.obj"
	-@erase "$(INTDIR)\VBlankHandler.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\wave.obj"
	-@erase "$(INTDIR)\waveBuffer.obj"
	-@erase "$(OUTDIR)\SpriteLib.dll"
	-@erase "$(OUTDIR)\SpriteLib.exp"
	-@erase "$(OUTDIR)\SpriteLib.lib"
	-@erase "%windir%\SpriteLib.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\ReleaseStatic/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\SpriteLib.res" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SpriteLib.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=ddraw.lib dsound.lib d3drm.lib winmm.lib wntab32x.lib /nologo\
 /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386\
 /nodefaultlib:"libc" /def:".\SpriteLib.def" /out:"$(OUTDIR)\SpriteLib.dll"\
 /implib:"$(OUTDIR)\SpriteLib.lib" 
DEF_FILE= \
	".\SpriteLib.def"
LINK32_OBJS= \
	"$(INTDIR)\Behavior.obj" \
	"$(INTDIR)\ddcheck.obj" \
	"$(INTDIR)\ddhelp.obj" \
	"$(INTDIR)\DDrawEnv.obj" \
	"$(INTDIR)\ddutil.obj" \
	"$(INTDIR)\SimpleTablet.obj" \
	"$(INTDIR)\Sprite.obj" \
	"$(INTDIR)\SpriteEngine.obj" \
	"$(INTDIR)\SpriteLib.obj" \
	"$(INTDIR)\SpriteLib.res" \
	"$(INTDIR)\StdAfx.obj" \
	"$(INTDIR)\VBlankHandler.obj" \
	"$(INTDIR)\wave.obj" \
	"$(INTDIR)\waveBuffer.obj"

"$(OUTDIR)\SpriteLib.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\.\ReleaseStatic
InputPath=.\ReleaseStatic\SpriteLib.dll
SOURCE=$(InputPath)

"%windir%\SpriteLib.dll"	 : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(OutDir)\SpriteLib.dll  %windir%%

!ENDIF 

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(CFG)" == "SpriteLib - Win32 Debug" || "$(CFG)" ==\
 "SpriteLib - Win32 Release" || "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"
SOURCE=.\Behavior.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

DEP_CPP_BEHAV=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\GlobalVar.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\Behavior.obj"	"$(INTDIR)\Behavior.sbr" : $(SOURCE) $(DEP_CPP_BEHAV)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_BEHAV=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Behavior.obj" : $(SOURCE) $(DEP_CPP_BEHAV) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_BEHAV=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Behavior.obj" : $(SOURCE) $(DEP_CPP_BEHAV) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\ddcheck.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

DEP_CPP_DDCHE=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddutil.h"\
	".\StdAfx.h"\
	

"$(INTDIR)\ddcheck.obj"	"$(INTDIR)\ddcheck.sbr" : $(SOURCE) $(DEP_CPP_DDCHE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_DDCHE=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddutil.h"\
	".\StdAfx.h"\
	

"$(INTDIR)\ddcheck.obj" : $(SOURCE) $(DEP_CPP_DDCHE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_DDCHE=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddutil.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	

"$(INTDIR)\ddcheck.obj" : $(SOURCE) $(DEP_CPP_DDCHE) "$(INTDIR)"


!ENDIF 

SOURCE=.\ddhelp.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\ddhelp.obj"	"$(INTDIR)\ddhelp.sbr" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_DDHEL=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\SpriteLib.h"\
	".\StdAfx.h"\
	".\wavebuffer.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ddhelp.obj" : $(SOURCE) $(DEP_CPP_DDHEL) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_DDHEL=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\SpriteLib.h"\
	".\StdAfx.h"\
	".\wavebuffer.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ddhelp.obj" : $(SOURCE) $(DEP_CPP_DDHEL) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\DDrawEnv.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\DDrawEnv.obj"	"$(INTDIR)\DDrawEnv.sbr" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_DDRAW=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\DDrawEnv.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DDrawEnv.obj" : $(SOURCE) $(DEP_CPP_DDRAW) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_DDRAW=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\DDrawEnv.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DDrawEnv.obj" : $(SOURCE) $(DEP_CPP_DDRAW) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\ddutil.cpp
DEP_CPP_DDUTI=\
	".\ddutil.h"\
	

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ddutil.obj"	"$(INTDIR)\ddutil.sbr" : $(SOURCE) $(DEP_CPP_DDUTI)\
 "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\SimpleTablet.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

DEP_CPP_SIMPL=\
	".\cdecls.h"\
	".\ddutil.h"\
	".\pktdef.h"\
	".\SimpleTablet.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	

"$(INTDIR)\SimpleTablet.obj"	"$(INTDIR)\SimpleTablet.sbr" : $(SOURCE)\
 $(DEP_CPP_SIMPL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_SIMPL=\
	".\cdecls.h"\
	".\ddutil.h"\
	".\pktdef.h"\
	".\SimpleTablet.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	

"$(INTDIR)\SimpleTablet.obj" : $(SOURCE) $(DEP_CPP_SIMPL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_SIMPL=\
	".\cdecls.h"\
	".\ddutil.h"\
	".\pktdef.h"\
	".\SimpleTablet.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	

"$(INTDIR)\SimpleTablet.obj" : $(SOURCE) $(DEP_CPP_SIMPL) "$(INTDIR)"


!ENDIF 

SOURCE=.\Sprite.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\Sprite.obj"	"$(INTDIR)\Sprite.sbr" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_SPRIT=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\DDrawEnv.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\Sprite.h"\
	".\StdAfx.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Sprite.obj" : $(SOURCE) $(DEP_CPP_SPRIT) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_SPRIT=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\DDrawEnv.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\Sprite.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Sprite.obj" : $(SOURCE) $(DEP_CPP_SPRIT) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\SpriteEngine.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\SpriteEngine.obj"	"$(INTDIR)\SpriteEngine.sbr" : $(SOURCE)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_SPRITE=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\DDrawEnv.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\Sprite.h"\
	".\SpriteEngine.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SpriteEngine.obj" : $(SOURCE) $(DEP_CPP_SPRITE) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_SPRITE=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\DDrawEnv.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\Sprite.h"\
	".\SpriteEngine.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SpriteEngine.obj" : $(SOURCE) $(DEP_CPP_SPRITE) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\SpriteLib.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\SpriteLib.obj"	"$(INTDIR)\SpriteLib.sbr" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_SPRITEL=\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\SpriteLib.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SpriteLib.obj" : $(SOURCE) $(DEP_CPP_SPRITEL) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_SPRITEL=\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\SpriteLib.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SpriteLib.obj" : $(SOURCE) $(DEP_CPP_SPRITEL) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\SpriteLib.rc
DEP_RSC_SPRITELI=\
	".\res\SpriteLib.rc2"\
	

"$(INTDIR)\SpriteLib.res" : $(SOURCE) $(DEP_RSC_SPRITELI) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\StdAfx.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

DEP_CPP_STDAF=\
	".\ddutil.h"\
	".\StdAfx.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\StdAfx.obj"	"$(INTDIR)\StdAfx.sbr"	"$(INTDIR)\SpriteLib.pch" : \
$(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_STDAF=\
	".\ddutil.h"\
	".\StdAfx.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yc"stdafx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\StdAfx.obj"	"$(INTDIR)\SpriteLib.pch" : $(SOURCE) $(DEP_CPP_STDAF)\
 "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_STDAF=\
	".\ddutil.h"\
	".\StdAfx.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yc"stdafx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\StdAfx.obj"	"$(INTDIR)\SpriteLib.pch" : $(SOURCE) $(DEP_CPP_STDAF)\
 "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\VBlankHandler.cpp

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\VBlankHandler.obj"	"$(INTDIR)\VBlankHandler.sbr" : $(SOURCE)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_VBLAN=\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	
CPP_SWITCHES=/nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\VBlankHandler.obj" : $(SOURCE) $(DEP_CPP_VBLAN) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_VBLAN=\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	".\wintab.h"\
	
CPP_SWITCHES=/nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D\
 "_AFXEXT" /D "_WINDLL" /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\VBlankHandler.obj" : $(SOURCE) $(DEP_CPP_VBLAN) "$(INTDIR)"\
 "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\wave.c
DEP_CPP_WAVE_=\
	".\wassert.h"\
	".\wave.h"\
	

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"


"$(INTDIR)\wave.obj"	"$(INTDIR)\wave.sbr" : $(SOURCE) $(DEP_CPP_WAVE_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"


"$(INTDIR)\wave.obj" : $(SOURCE) $(DEP_CPP_WAVE_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"


"$(INTDIR)\wave.obj" : $(SOURCE) $(DEP_CPP_WAVE_) "$(INTDIR)"


!ENDIF 

SOURCE=.\waveBuffer.c

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

DEP_CPP_WAVEB=\
	".\wassert.h"\
	".\wave.h"\
	".\wavebuffer.h"\
	

"$(INTDIR)\waveBuffer.obj"	"$(INTDIR)\waveBuffer.sbr" : $(SOURCE)\
 $(DEP_CPP_WAVEB) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

DEP_CPP_WAVEB=\
	".\wassert.h"\
	".\wave.h"\
	".\wavebuffer.h"\
	

"$(INTDIR)\waveBuffer.obj" : $(SOURCE) $(DEP_CPP_WAVEB) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

DEP_CPP_WAVEB=\
	".\wassert.h"\
	".\wave.h"\
	".\wavebuffer.h"\
	

"$(INTDIR)\waveBuffer.obj" : $(SOURCE) $(DEP_CPP_WAVEB) "$(INTDIR)"


!ENDIF 


!ENDIF 

