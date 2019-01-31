// SpriteLib.cpp : Defines the initialization routines for the DLL.
//

#include "stdafx.h"
#include <afxdllx.h>
#include "ddhelp.h"			// for init and fini
#include "VBlankHandler.h"		// for init and fini
#include "SpriteLib.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// How many screen pixels correspond to one length unit
double g_screenPixelsPerLength = 1;

static AFX_EXTENSION_MODULE SpriteLibDLL = { NULL, NULL };

extern "C" int APIENTRY
DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID lpReserved)
{
	if (dwReason == DLL_PROCESS_ATTACH)
	{
		TRACE0("SPRITELIB.DLL Initializing!\n");

		
		// Extension DLL one-time initialization
		AfxInitExtensionModule(SpriteLibDLL, hInstance);

		// Insert this DLL into the resource chain
		new CDynLinkLibrary(SpriteLibDLL);
               // Added (conal)
               // DDHelpInit();
               // Having trouble with DSound init, so moving to client code
               // Can't do VBlankInit here, because when we're not
               // running in an MFC windowed app, we can't yet make
               // a worker thread.  Moved to VBlankHandler.cpp
               // VBlankInit();
	}
	else if (dwReason == DLL_PROCESS_DETACH)
	{
	    TRACE0("SPRITELIB.DLL Terminating!\n");
	    CloseSpriteLib ();
	}
	return 1;   // ok
}

// Every CWinThread needs a CWinApp.  Make one if necessary (e.g., from
// Hugs).
static CWinApp* pWinAppForNonMFC = NULL;

static void SpriteLibNonMFCInit()
{
    if (!AfxGetApp()) {
        TRACE("Making CWinApp object for non-MFC app.\n");
        pWinAppForNonMFC = new CWinApp("SpriteLib");
    }
}

static void SpriteLibNonMFCFFini()
{
    DELETEIF (pWinAppForNonMFC);
}


// Keep track of whether the library has been initialized.  I would much rather
// do the open and close in DllMain, but the DSound init bombs under NT.
static BOOL g_SpriteLibIsOpen = FALSE;

// For improving the resolution of timeGetTime under NT.
static int timerResolutionMS = 5;

EXT_API(void) OpenSpriteLib (double screenPixelsPerLength)
{
    g_screenPixelsPerLength = screenPixelsPerLength;
    if (! g_SpriteLibIsOpen) {
	// Allows thread creation and message boxes even if
	// run from a console app (like hugs).
	SpriteLibNonMFCInit();
        DDHelpInit();
        VBlankInit();
        timeBeginPeriod(timerResolutionMS);		
        g_SpriteLibIsOpen = TRUE;
    }
}

EXT_API(void) CloseSpriteLib ()
{
    if (g_SpriteLibIsOpen) {
        g_SpriteLibIsOpen = FALSE;
        timeEndPeriod(timerResolutionMS);
        VBlankFini();
        DDHelpFini();		
	SpriteLibNonMFCFFini();
    }
}

EXT_API(void) SetTimerResolutionMS (int newRes)
{
    timeEndPeriod(timerResolutionMS);   // out with the old
    timeBeginPeriod(newRes);            // and in with the new
    timerResolutionMS = newRes;         // for later timeEndPeriod
}


// See if I can safely initialize this way.  Nope.
// static BOOL tryOpenLib = OpenSpriteLib ();
