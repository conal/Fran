// Vertical blank synchronization support

#include "StdAfx.h"
#include "VBlank.h"

#include "DDrawEnv.h"

// The event for sync'ing on
static CEvent g_VBlankEvent (FALSE, TRUE);  // Manual-reset, nonsignaled
static BOOL g_VBlankStop = FALSE;
static CWinThread* g_pVBlankThread = NULL;

static UINT VBlankThreadProc(LPVOID)
{
    ASSERT(g_pDDraw);

    TRACE("Entered VBlank thread.\n");
    while (! g_VBlankStop) {
	// Just wait
        //TRACE("Waiting for vblank...");
	g_pDDraw->WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, NULL);
	// Then set & reset, awakening all threads blocked on the event.
        //TRACE(" Pulsing vblank event...");
	g_VBlankEvent.PulseEvent();
        //TRACE(" Yielding ...");
	::Sleep(0);
        //TRACE(" Checking g_VBlankStop.\n");
    }
    TRACE("Leaving VBlank thread.\n");
    return 0;
}

// #define VBLANK_PRIORITY THREAD_PRIORITY_BELOW_NORMAL
#define VBLANK_PRIORITY THREAD_PRIORITY_NORMAL
// #define VBLANK_PRIORITY THREAD_PRIORITY_HIGHEST

// #define PROCESS_PRIORITY_CLASS HIGH_PRIORITY_CLASS
#define PROCESS_PRIORITY_CLASS NORMAL_PRIORITY_CLASS

void VBlankInit()
{ 
    TRACE ("VBlankInit()\n");
    // Boost the process and thread priority so that our ThreadProc will
    // wake up promptly.  Doesn't help :-(.
    VERIFY(::SetPriorityClass(::GetCurrentProcess(), PROCESS_PRIORITY_CLASS));
    VERIFY (g_pVBlankThread = 
        AfxBeginThread(VBlankThreadProc, NULL, VBLANK_PRIORITY)); 
}

void VBlankFini()
{
    if (g_pVBlankThread) {
	TRACE ("VBlankFini()\n");
	// Terminate the worker thread.
	// First grab the thread handle before the CWinThread vanishes.
	HANDLE hThread = g_pVBlankThread->m_hThread;
	g_VBlankStop = TRUE;

	// Give the vblank thread at most 1 second to stop.  We must wait for
	// it, so that the ddraw object doesn't go away while we're using it.
	TRACE("Waiting for VBlank thread to stop.\n");
	DWORD waitRes = ::WaitForSingleObject (hThread, 1000);
        // The following assert fails, but why??  It seems that the thread
        // dies shortly before the wait, though I don't know why.
	//ASSERT (waitRes != WAIT_TIMEOUT);
        TRACE("Done waiting for VBlank thread.  Result = %d.\n", waitRes);
        DELETEIF(g_pVBlankThread);
    }
}

void BlockUntilVerticalBlank()
{
    if (! g_pVBlankThread)
	VBlankInit();
    g_VBlankEvent.Lock ();
}
