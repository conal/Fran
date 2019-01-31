// Shared DDraw surfaces.  Thin wrapper around a ddraw surface, with
// the ability to reconstruct it.

#ifndef _SURFACE_H
#define _SURFACE_H

#include "cdecls.h"

// C interface, for Haskell.

EXT_CLASS(Surface);

EXT_API(HSurface) newBitmapSurface(LPCSTR bmpName);
EXT_API(HSurface) newTextSurface(LPCSTR chars, COLORREF);
EXT_API(void) deleteSurface (HSurface);

#ifdef __cplusplus
// C++ interfaces to constructors, destructors, and methods.

class AFX_EXT_CLASS Surface {
public:
    IDirectDrawSurface *GetDDSurface() { return m_pDDSurface; }
    CRect GetCRect() { return m_rect; }
    virtual void OnRestore () = 0;
    // Destructor deletes the ddraw surface
    virtual ~Surface();
protected:
    IDirectDrawSurface *m_pDDSurface;
    CRect m_rect;
};


class AFX_EXT_CLASS BitmapSurface : public Surface {
public:
    BitmapSurface(LPCSTR bmpName);
    void OnRestore ();
private:
    CString m_bmpName;
};

class TextSurface : public Surface {
public:
    TextSurface (LPCSTR string, COLORREF color);
    void OnRestore ();
private:
    CString m_string;
    COLORREF m_color;
};


#endif // __cplusplus

#endif // _SURFACE_H
