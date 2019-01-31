# Microsoft Developer Studio Generated NMAKE File, Based on SpriteLib.dsp
!IF "$(CFG)" == ""
CFG=SpriteLib - Win32 Debug
!MESSAGE No configuration specified. Defaulting to SpriteLib - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "SpriteLib - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "SpriteLib.mak" CFG="SpriteLib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SpriteLib - Win32 Debug" (based on\
 "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

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

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\SpriteLib.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\SpriteLib.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\Behavior.sbr" \
	"$(INTDIR)\ddcheck.sbr" \
	"$(INTDIR)\ddhelp.sbr" \
	"$(INTDIR)\DDrawEnv.sbr" \
	"$(INTDIR)\ddutil.sbr" \
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
LINK32_FLAGS=ddraw.lib dsound.lib d3drm.lib winmm.lib /nologo /base:"0x8000000"\
 /subsystem:windows /dll /profile /debug /machine:I386 /def:".\SpriteLib.def"\
 /out:"$(OUTDIR)\SpriteLib.dll" /implib:"$(OUTDIR)\SpriteLib.lib" 
DEF_FILE= \
	".\SpriteLib.def"
LINK32_OBJS= \
	"$(INTDIR)\Behavior.obj" \
	"$(INTDIR)\ddcheck.obj" \
	"$(INTDIR)\ddhelp.obj" \
	"$(INTDIR)\DDrawEnv.obj" \
	"$(INTDIR)\ddutil.obj" \
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


!IF "$(CFG)" == "SpriteLib - Win32 Debug"
SOURCE=.\Behavior.cpp
DEP_CPP_BEHAV=\
	".\Behavior.h"\
	".\cdecls.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\Behavior.obj"	"$(INTDIR)\Behavior.sbr" : $(SOURCE) $(DEP_CPP_BEHAV)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\ddcheck.cpp
DEP_CPP_DDCHE=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddutil.h"\
	".\StdAfx.h"\
	

"$(INTDIR)\ddcheck.obj"	"$(INTDIR)\ddcheck.sbr" : $(SOURCE) $(DEP_CPP_DDCHE)\
 "$(INTDIR)"


SOURCE=.\ddhelp.cpp
DEP_CPP_DDHEL=\
	".\cdecls.h"\
	".\ddcheck.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\SpriteLib.h"\
	".\StdAfx.h"\
	".\wavebuffer.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\ddhelp.obj"	"$(INTDIR)\ddhelp.sbr" : $(SOURCE) $(DEP_CPP_DDHEL)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\DDrawEnv.cpp
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
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\DDrawEnv.obj"	"$(INTDIR)\DDrawEnv.sbr" : $(SOURCE) $(DEP_CPP_DDRAW)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\ddutil.cpp
DEP_CPP_DDUTI=\
	".\ddutil.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ddutil.obj"	"$(INTDIR)\ddutil.sbr" : $(SOURCE) $(DEP_CPP_DDUTI)\
 "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\Sprite.cpp
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
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\Sprite.obj"	"$(INTDIR)\Sprite.sbr" : $(SOURCE) $(DEP_CPP_SPRIT)\
 "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\SpriteEngine.cpp
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
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\SpriteEngine.obj"	"$(INTDIR)\SpriteEngine.sbr" : $(SOURCE)\
 $(DEP_CPP_SPRITE) "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\SpriteLib.cpp
DEP_CPP_SPRITEL=\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\SpriteLib.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\SpriteLib.obj"	"$(INTDIR)\SpriteLib.sbr" : $(SOURCE)\
 $(DEP_CPP_SPRITEL) "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\SpriteLib.rc
DEP_RSC_SPRITELI=\
	".\res\SpriteLib.rc2"\
	

"$(INTDIR)\SpriteLib.res" : $(SOURCE) $(DEP_RSC_SPRITELI) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\StdAfx.cpp
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


SOURCE=.\VBlankHandler.cpp
DEP_CPP_VBLAN=\
	".\cdecls.h"\
	".\ddhelp.h"\
	".\ddutil.h"\
	".\GlobalVar.h"\
	".\StdAfx.h"\
	".\VBlankHandler.h"\
	
CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\SpriteLib.pch" /Yu"StdAfx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\"\
 /FD /c 

"$(INTDIR)\VBlankHandler.obj"	"$(INTDIR)\VBlankHandler.sbr" : $(SOURCE)\
 $(DEP_CPP_VBLAN) "$(INTDIR)" "$(INTDIR)\SpriteLib.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


SOURCE=.\wave.c
DEP_CPP_WAVE_=\
	".\wassert.h"\
	".\wave.h"\
	

"$(INTDIR)\wave.obj"	"$(INTDIR)\wave.sbr" : $(SOURCE) $(DEP_CPP_WAVE_)\
 "$(INTDIR)"


SOURCE=.\waveBuffer.c
DEP_CPP_WAVEB=\
	".\wassert.h"\
	".\wave.h"\
	".\wavebuffer.h"\
	

"$(INTDIR)\waveBuffer.obj"	"$(INTDIR)\waveBuffer.sbr" : $(SOURCE)\
 $(DEP_CPP_WAVEB) "$(INTDIR)"



!ENDIF 

