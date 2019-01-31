// DirectDraw environments

#include "StdAfx.h"
#include "DDrawEnv.h"
#include "ddcheck.h"
#include "ddhelp.h"
#include "VBlankHandler.h"


// No C/Haskell interfaces


DDrawEnv::DDrawEnv(HWND hWnd)
 : m_hWnd(hWnd), m_pBack(NULL), m_pBackClipper(NULL)
{
    TRACE("DDrawEnv::DDrawEnv\n");
    // Just set up the back surface and clipper.
    OnResize();
}

void DDrawEnv::ReleaseBack()
{
    RELEASEIF (m_pBackClipper);
    RELEASEIF (m_pBack);
}

// Reconstruct back buffer and clipper
void DDrawEnv::OnResize()
{
    //TRACE("DDrawEnv::OnResize\n");
    DDSURFACEDESC ddsd;

    // Be careful to lock, in case another thread is BLT'ing.
    Lock ();
    //{ CSingleLock lock(&m_syncObj, TRUE);
    //TRACE("DDrawEnv::OnResize grabbed lock.\n");

    ReleaseBack();

    memset( &ddsd, 0, sizeof(ddsd) );
    ddsd.dwSize = sizeof( ddsd );
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    // Forces back buffer to be in video memory.  Experimental.
    // ddsd.ddsCaps.dwCaps |= DDSCAPS_VIDEOMEMORY;
    
    GetClientRect(m_hWnd,&m_backRect);
    ddsd.dwWidth  = max (1, m_backRect.Width());
    ddsd.dwHeight = max (1, m_backRect.Height());

    // create the backbuffer separately
    ddcheck (g_pDDraw->CreateSurface( &ddsd, &m_pBack, NULL ));

    // Then give it a clipper.
    ddcheck (g_pDDraw->CreateClipper( 0, &m_pBackClipper, NULL ));

    // Next, set the clipper.

    struct {
	char foo[sizeof(RGNDATA) + sizeof(RECT)];
    } bar;
    RGNDATA *clipList = (RGNDATA *) &bar;
    clipList->rdh.dwSize = sizeof(clipList->rdh);
    clipList->rdh.nCount = 1;
    clipList->rdh.iType = RDH_RECTANGLES;
    clipList->rdh.nRgnSize = sizeof(RECT);
    clipList->rdh.rcBound = m_backRect;
    memcpy(&(clipList->Buffer), m_backRect, sizeof(RECT));

    // Clear any former cliplists
    ddcheck (m_pBackClipper->SetClipList(NULL,0));

    ddcheck (m_pBackClipper->SetClipList(clipList,0));
#if 0
    // Detatches any current clipper. Always generates a complaint
    { TRACE("About to m_pBack->SetClipper( NULL )\n");
    HRESULT ddrval = m_pBack->SetClipper( NULL );
    TRACE("Done with m_pBack->SetClipper( NULL )\n");
    if (ddrval != DDERR_NOCLIPPERATTACHED)
        ddcheck (ddrval);
    }
#endif
    ddcheck (m_pBack->SetClipper(m_pBackClipper));
    
    //TRACE("   DDrawEnv::OnResize releasing lock.\n");
    //} // unlock
    Unlock ();
}

DDrawEnv::~DDrawEnv()
{
    // Note: we might put a lock around ReleaseBack.  Instead, we assume that
    // the client threads are already terminated.  Putting a lock here
    // doesn't work anyway, because the client thread my get terminated
    // after grabbing but before releasing the lock.
    TRACE("DDrawEnv::~DDrawEnv()\n");
    ReleaseBack();
}

const COLORREF backgroundColor = RGB(1,1,1); // RGB(0,0,0);

void DDrawEnv::ClearBack()
{
    clearDDSurface(m_pBack, backgroundColor);
}

void DDrawEnv::Flip()
{
    //TRACE("DDrawEnv::Flip\n");
    RECT rectFront;
    POINT p;

    if (rectFront.top  != rectFront.bottom &&
        rectFront.left != rectFront.right) {
    // first we need to figure out where on the primary surface our
    // window lives.  To do: use MFC rects and points to simplify this code.
    p.x = 0; p.y = 0;
    ClientToScreen(m_hWnd, &p);
    GetClientRect(m_hWnd, &rectFront );
    OffsetRect(&rectFront, p.x, p.y);

#ifdef _VBLANK_H
    BlockUntilVerticalBlank();
#elif defined(_VBLANKHANDLER_H)
    // No sync work needed -- done by VBlankHandler
#else
    BLORT_UNUSED;
    // Experiment: wait for vertical blank, so we don't exceed
    // the video refresh rate.  Catch: if two threads do this, the refresh
    // rate reduces by two.  So, first see whether we're already in the VB.
    // Still may be wasteful, since we don't start BLT'ing outside of VB.
    // In fact, I never get very close to video refresh rate with two
    // display threads, even when each can do > 500 fps running concurrently
    // without the wait for VB.
    BOOL isInVB;
    ddcheck (g_pDDraw->GetVerticalBlankStatus(&isInVB));
    if (!isInVB)
        g_pDDraw->WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, NULL);
#endif // _VBLANK_H
    // Get exclusive access to the front clipper while blitting.
    // Otherwise, another thread could sneak in between our SetHWnd and Blt
    g_frontClipperCriticalSection.Lock ();
    // Set the front clipper to the coordinates from our window
    //TRACE("DDrawEnv::Flip about to SetHWnd....");
    ddcheck (g_pFrontClipper->SetHWnd( 0, m_hWnd ));
    //TRACE("  SetHWnd done.\n");

    ddcheck (g_pFront->Blt(&rectFront, m_pBack, &m_backRect,
                           DDBLT_WAIT,
                           NULL));
    //TRACE("DDrawEnv::Flip did Blt.\n");
    g_frontClipperCriticalSection.Unlock ();
    }
}


void DDrawEnv::Lock ()
{ 
    BOOL b = m_syncObj.Lock(1000);  // Wait up to one second
    if (!b) AfxDebugBreak();
}

void DDrawEnv::Unlock ()
{ 
    BOOL b = m_syncObj.Unlock();
    if (!b) AfxDebugBreak();
}
