// Immutable, self-restoring DDraw surfaces. 

#include "StdAfx.h"
#include "ddcheck.h"
#include "ddhelp.h"
#include "Surface.h"
#include "DDrawEnv.h" // for g_pDDraw

// C interfaces for Haskell

EXT_API void deleteSurface (Surface *pSurface)
{ delete pSurface; }

EXT_API Surface* newBitmapSurface(LPCSTR bmpName)
{ return new BitmapSurface(bmpName); }

EXT_API Surface* newTextSurface(LPCSTR string, COLORREF color)
{ return new TextSurface(string, color); }

// End of C interface


Surface::~Surface()
{
    TRACE("Surface::~Surface\n");
    RELEASEIF (m_pDDSurface);
}

BitmapSurface::BitmapSurface (LPCSTR bmpName)
   : m_bmpName(bmpName)
{
    TRACE("BitmapSurface::BitmapSurface(%s)\n", bmpName);
    m_pDDSurface = DDLoadBitmap(g_pDDraw, m_bmpName, 0, 0);
    // Needs much more friendly error handling.
    ASSERT (m_pDDSurface);
    DDSetColorKey( m_pDDSurface, CLR_INVALID );
}

void BitmapSurface::OnRestore ()
{
    TRACE("BitmapSurface::OnRestore()\n");
    ddcheck (DDReLoadBitmap(m_pDDSurface, m_bmpName));
}


///////  Text surface

TextSurface::TextSurface (LPCSTR string, COLORREF color)
:   m_string(string), m_color(color)
{
    TRACE("TextSurface::TextSurface(%s)\n", string);

    DDSURFACEDESC       ddsd;

    // Figure out how big to make the surface.  Note: I tried doing the
    // GetDC during DDrawEnvInit, but it made DDrawEnv::ClearBack block.
	// I guess GetDC blocks more than just the one surface.
    SIZE size;  HDC scratchHDC;
    ddcheck (g_pScratchSurf->GetDC(&scratchHDC));
    GetTextExtentPoint32(scratchHDC, string, lstrlen(string), &size);
    ddcheck (g_pScratchSurf->ReleaseDC(scratchHDC));

    int width = size.cx, height = size.cy;

    m_rect = CRect(0, 0, width, height);

    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT |DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth = width;
    ddsd.dwHeight = height;

    ddcheck (g_pDDraw->CreateSurface(&ddsd, &m_pDDSurface, NULL));

    OnRestore();
}


void TextSurface::OnRestore ()
{
    TRACE("TextSurface::OnRestore\n");

    // Get a DC for drawing
    HDC hdc;
    ddcheck (m_pDDSurface->GetDC(&hdc));

    SetBkColor( hdc, CLR_INVALID );
    SetTextColor( hdc, m_color );

    TextOut( hdc, 0, 0, m_string, lstrlen(m_string) );

    ddcheck (m_pDDSurface->ReleaseDC(hdc));

    DDSetColorKey( m_pDDSurface, CLR_INVALID );
}

