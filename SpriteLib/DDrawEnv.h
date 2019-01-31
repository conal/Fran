// DDraw environment object

#ifndef _DDRAWENV_H
#define _DDRAWENV_H

#include "cdecls.h"

// no C/Haskell interface

#ifdef __cplusplus
// C++ interfaces to constructors, destructors, and methods.

#include "Behavior.h"

// A drawing environment.

class AFX_EXT_CLASS DDrawEnv {
public:
    DDrawEnv (HWND hWnd);
    void ClearBack();
    void Flip ();

    // Reconstruct back buffer and clipper.  Use for resizing.
    void OnResize();
    IDirectDrawSurface* GetBack() { return m_pBack; }

    // For non-interference between operations that rebuild
    // and ones that use the back buffer resources.  Wrap around
    // a whole frame generation.
    void Lock ();
    void Unlock ();
    
    ~DDrawEnv ();
private:
    HWND m_hWnd;
    IDirectDrawSurface *m_pBack;
    CRect m_backRect;
    IDirectDrawClipper *m_pBackClipper;
    void ReleaseBack();
public: // for now
	// Using CMutex instead of CCriticalSection to allow timeout.
    // CMutex m_syncObj;
    CCriticalSection m_syncObj;
};

#endif /* __cplusplus */

#endif /* _DDRAWENV_H */
