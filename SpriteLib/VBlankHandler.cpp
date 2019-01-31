// Vertical blank synchronization support

#include "StdAfx.h"
#include "VBlankHandler.h"
#include "ddhelp.h"

// #include "DDrawEnv.h"

// The job queue is represented by an array of VBlankJob pointers.  The
// elements 0 .. queueSize-1 may have legitimate pointers, although some
// may be NULL, due to removal.  New jobs get put in the first NULL slot,
// or at queueSize, which would then get incremented.  Note: queueSize is
// zero if and only if there are no jobs, and we can thus suspend the
// handler thread.

const int queueMax = 255;

static int queueSize = 0;
static VBlankJob* jobs[queueMax+1];
static BOOL stopHandler = FALSE;
static CCriticalSection vblankCritSec;

static CWinThread* pVBlankThread = NULL;

// One of THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST,
// THREAD_PRIORITY_BELOW_NORMAL, THREAD_PRIORITY_NORMAL,
// THREAD_PRIORITY_ABOVE_NORMAL, THREAD_PRIORITY_HIGHEST,
// THREAD_PRIORITY_TIME_CRITICAL

static int vblankThreadPriority = THREAD_PRIORITY_NORMAL;

void SetVblankThreadPriority(int priority)
{
    vblankThreadPriority = priority;
    if (pVBlankThread)
        pVBlankThread->SetThreadPriority(vblankThreadPriority);
}

int AddVBlankJob(VBlankJob* pJob)
{
    TRACE("AddVBlankJob(...)\n");
    // if (! pVBlankThread) VBlankInit();
    ASSERT(pVBlankThread);

    int i;
    vblankCritSec.Lock();
    // Find empty slot
    for (i = 0; i < queueSize && jobs[i] != NULL ; i++) ;
    ASSERT (i <= queueMax);		// otherwise overflow
    jobs[i] = pJob;			// insert job
    if (i == queueSize)			// bump queue size if nec
	queueSize ++;
    TRACE("AddVBlankJob: inserted job #%d.  queueSize now %d.\n", i, queueSize);
    if (queueSize == 1)	{		// was empty, hence suspended
        TRACE("AddVBlankJob resuming vblank thread.\n");
	pVBlankThread->ResumeThread();
    }
    vblankCritSec.Unlock();
    return i;
}

void RemoveVBlankJob(int jobNum)
{
    TRACE("RemoveVBlankJob(%d)\n", jobNum);
    vblankCritSec.Lock();
    ASSERT (jobNum < queueSize && jobs[jobNum]); // legit removal?
    jobs[jobNum] = NULL;		// remove job
    // Shrink queue as possible.  May remove multiple elements.
    while (queueSize > 0 && jobs[queueSize-1] == NULL) 
	queueSize --;
    TRACE("RemoveVBlankJob: removed job %d.  queueSize now %d.\n", jobNum, queueSize);
    if (queueSize == 0)	{		// now empty, so suspend
        TRACE("RemoveVBlankJob suspending vblank thread.\n");
	pVBlankThread->SuspendThread();
    }
    vblankCritSec.Unlock();
}

// See note in .h file
int vblankPeriodMS = 29;

// #define VBTRACE TRACE
#define VBTRACE if (FALSE) TRACE

static UINT VBlankThreadProc(LPVOID)
{
    ASSERT(g_pDDraw);

    DWORD beforeWork = timeGetTime();
    TRACE("Entered VBlank thread.\n");
    while (! stopHandler) {
        // Try to sync this loop to run every vblankPeriodMS
        // milliseconds (if non-negative).  When we can't, however,
        // don't try to catch up, because we'd likely just get more
        // and more behind.

//        if (vblankPeriodMS > 0) 
//            beforeWork = timeGetTime();
	vblankCritSec.Lock();
	VBTRACE("VBlank handler: ");
        int i;
        VBTRACE("setup... ");
	for (i = 0 ; i < queueSize ; i++) {
	    if (jobs[i]) jobs[i]->VBlankSetup();
	}
        if (vblankPeriodMS < 0) {
            // WaitForVerticalBlank is very expensive.  It basically spins.
            VBTRACE("waiting... ");
	    g_pDDraw->WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, NULL);
        }
	VBTRACE("OnVBlank... ");
	for (i = 0 ; i < queueSize ; i++) {
	    if (jobs[i]) jobs[i]->OnVBlank();
	}
        //TRACE("unlock... ");
	vblankCritSec.Unlock();
        if (1 /*vblankPeriodMS > 0*/) {
            DWORD afterWork = timeGetTime(),
                  workDur   = afterWork - beforeWork;
            // Note: sleepFor could be negative, so we must use *signed*
            // integers for durations.  Otherwise the Sleep will go away
            // for a *long time*.  This really happened, and totally
            // puzzled me.  To make it even more subtle, the VBTRACE
            // showed a negative number, but that's because it didn't know
            // that sleepFor is unsigned.
            int sleepFor  = vblankPeriodMS - workDur;
            if (sleepFor > 0) {
                VBTRACE("sleeping for %d MS  ...  ", sleepFor);
                // Even if sleep duration is non-positive, do a Sleep(0),
                // so that other threads have some chance to run.
                ::Sleep(sleepFor);
            } else {
                VBTRACE("%d MS, not sleeping ...  ", workDur);
            }
            beforeWork = afterWork;
        }
        VBTRACE("repeating.\n");
    }
    TRACE("Leaving VBlank thread.\n");
    return 0;
}

// #define VBLANK_PROCESS_PRIORITY_CLASS NORMAL_PRIORITY_CLASS

void VBlankInit()
{ 
    TRACE ("VBlankInit()\n");
    // Boost the process and thread priority so that our ThreadProc will
    // wake up promptly.
    // VERIFY(::SetPriorityClass(::GetCurrentProcess(), VBLANK_PROCESS_PRIORITY_CLASS));
    pVBlankThread = AfxBeginThread(VBlankThreadProc, NULL,
				   vblankThreadPriority,
				   CREATE_SUSPENDED);
    // This next line shouldn't be necessary, since I said to create it
    // suspended, but somehow, it is necessary.
    pVBlankThread->SuspendThread();
}

void VBlankFini()
{
    TRACE("VBlankFini()\n");
    // I removed this assertion, because when Hugs quits, the SpriteLib dll
    // gets unloaded, without the jobs getting removed.
    // ASSERT (queueSize == 0);

    // Tell the vblank thread to stop and then wake it up so it will hear.
    // Since the queue size is zero, the thread
    // is suspended.  Signal completion and resume.  First grab the thread
    // handle before the CWinThread vanishes.

    if (pVBlankThread) {
    HANDLE hThread = pVBlankThread->m_hThread;
    stopHandler = TRUE;
    pVBlankThread->ResumeThread();

    // Give the vblank thread at most 5 seconds to stop.  We must wait for
    // it, so that the ddraw object doesn't go away while we're using it.
    TRACE("Waiting for VBlank thread to stop.\n");
    DWORD waitRes = ::WaitForSingleObject (hThread, 5000);
    ASSERT (waitRes != WAIT_TIMEOUT);
    TRACE("VBlank thread stopped.\n");
    // Do not delete the thread.  It was created auto-delete.
    // delete pVBlankThread; 
    pVBlankThread = 0;
    }
}

