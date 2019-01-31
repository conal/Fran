
/*******************************************************************************

Copyright (c) 1995_96 Microsoft Corporation

Abstract:

    Minimal declarations of DirectX interfaces used by SpriteLib.  The
    point here is to avoid GreenCard having to pull in any real DirectX
    header files.  These files cause problems for GHC, which is currently
    tied to GCC.  According to Sigbjorn (Oct 19, 1997), a better solution
    is on the way.

    NOTE: This file must be kept up to date with DirectX, but then again,
    I don't think DirectX will be changing these definitions.

*******************************************************************************/


#ifndef _DXSUBST_H
#define _DXSUBST_H

// Copied from d3drmdef.h
typedef DWORD D3DCOLOR, *LPD3DCOLOR;

// Copied from d3drmdef.h
typedef enum _D3DRMLIGHTTYPE
{   D3DRMLIGHT_AMBIENT,
    D3DRMLIGHT_POINT,
    D3DRMLIGHT_SPOT,
    D3DRMLIGHT_DIRECTIONAL,
    D3DRMLIGHT_PARALLELPOINT
} D3DRMLIGHTTYPE, *LPD3DRMLIGHTTYPE;


#endif /* _DXSUBST_H */
