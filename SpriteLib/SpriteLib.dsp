# Microsoft Developer Studio Project File - Name="SpriteLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=SpriteLib - Win32 ReleaseStatic
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "SpriteLib.mak".
!MESSAGE 
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

# Begin Project
# PROP Scc_ProjName ""$/RBMH/Fran/SpriteLib", LECAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /Yu"stdafx.h" /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR /YX"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 ddraw.lib dsound.lib d3drm.lib winmm.lib wntab32x.lib /nologo /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386
# Begin Custom Build - copy SpriteLib.dll to %windir%
InputPath=.\Debug\SpriteLib.dll
SOURCE=$(InputPath)

"%windir%\SpriteLib.dll" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy Debug\SpriteLib.dll  %windir%%

# End Custom Build

!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "SpriteLi"
# PROP BASE Intermediate_Dir "SpriteLi"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_AFXEXT" /FR /FD /c
# ADD CPP /nologo /MTd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_AFXEXT" /D "_WINDLL" /FD /c
# SUBTRACT CPP /Fr /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 ddraw.lib dsound.lib d3drm.lib winmm.lib /nologo /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386
# ADD LINK32 ddraw.lib dsound.lib d3drm.lib winmm.lib wntab32x.lib /nologo /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386
# Begin Custom Build - copy SpriteLib.dll to %windir%
InputPath=.\Release\SpriteLib.dll
SOURCE=$(InputPath)

"%windir%\SpriteLib.dll" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy Release\SpriteLib.dll  %windir%%

# End Custom Build

!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "SpriteLi"
# PROP BASE Intermediate_Dir "SpriteLi"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\ReleaseStatic"
# PROP Intermediate_Dir ".\ReleaseStatic"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_AFXEXT" /D "_WINDLL" /D "_AFXDLL" /FD /c
# SUBTRACT BASE CPP /Fr /YX
# ADD CPP /nologo /MD /W3 /GX /Ob1 /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_AFXEXT" /D "_WINDLL" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_AFXDLL"
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 ddraw.lib dsound.lib d3drm.lib winmm.lib /nologo /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386
# ADD LINK32 ddraw.lib dsound.lib d3drm.lib winmm.lib wntab32x.lib /nologo /base:"0x8000000" /subsystem:windows /dll /profile /debug /machine:I386 /nodefaultlib:"libc"
# Begin Custom Build - copy SpriteLib.dll to %windir%
OutDir=.\.\ReleaseStatic
InputPath=.\ReleaseStatic\SpriteLib.dll
SOURCE=$(InputPath)

"%windir%\SpriteLib.dll" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy $(OutDir)\SpriteLib.dll  %windir%%

# End Custom Build
# Begin Special Build Tool
OutDir=.\.\ReleaseStatic
SOURCE=$(InputPath)
PostBuild_Desc=Building (static) SpriteLibStat.lib
PostBuild_Cmds=cd $(Outdir)	lib /subsystem:windows /out:SpriteLibStat.lib *.obj
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "SpriteLib - Win32 Debug"
# Name "SpriteLib - Win32 Release"
# Name "SpriteLib - Win32 ReleaseStatic"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\Behavior.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\ddcheck.cpp
# End Source File
# Begin Source File

SOURCE=.\ddhelp.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\DDrawEnv.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\ddutil.cpp
# SUBTRACT CPP /YX /Yc /Yu
# End Source File
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# Begin Source File

SOURCE=.\SimpleTablet.cpp
# End Source File
# Begin Source File

SOURCE=.\Sprite.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\SpriteEngine.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\SpriteLib.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\SpriteLib.def
# End Source File
# Begin Source File

SOURCE=.\SpriteLib.rc

!IF  "$(CFG)" == "SpriteLib - Win32 Debug"

!ELSEIF  "$(CFG)" == "SpriteLib - Win32 Release"

!ELSEIF  "$(CFG)" == "SpriteLib - Win32 ReleaseStatic"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp
# ADD CPP /Yc"stdafx.h"
# End Source File
# Begin Source File

SOURCE=.\VBlankHandler.cpp
# ADD CPP /Yu"StdAfx.h"
# End Source File
# Begin Source File

SOURCE=.\wave.c
# End Source File
# Begin Source File

SOURCE=.\waveBuffer.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\Behavior.h
# End Source File
# Begin Source File

SOURCE=.\cdecls.h
# End Source File
# Begin Source File

SOURCE=.\ddcheck.h
# End Source File
# Begin Source File

SOURCE=.\ddhelp.h
# End Source File
# Begin Source File

SOURCE=.\DDrawEnv.h
# End Source File
# Begin Source File

SOURCE=.\ddutil.h
# End Source File
# Begin Source File

SOURCE=.\GlobalVar.h
# End Source File
# Begin Source File

SOURCE=.\SimpleTablet.h
# End Source File
# Begin Source File

SOURCE=.\Sprite.h
# End Source File
# Begin Source File

SOURCE=.\SpriteEngine.h
# End Source File
# Begin Source File

SOURCE=.\SpriteLib.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\VBlankHandler.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\SpriteLib.rc2
# End Source File
# End Group
# End Target
# End Project
