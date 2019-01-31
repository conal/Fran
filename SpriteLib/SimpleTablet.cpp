// Simplified interface to Wintab
// Mostly copied and simplified from wintab sample "rule2"

#include "StdAfx.h"
#include "SimpleTablet.h"


// To do: split OpenTablet into a part with no HWND argument that calls
// WTInfo, and a part with an HWND argument that calls WTOpen.  Move a
// call to the first part into OpenSpriteLib.

EXT_API(BOOL) TestForTablet(HWND hWnd)
{
    LOGCONTEXT lc;
    // Suppress error box, open, and restore error mode
    WORD errmode = SetErrorMode(SEM_NOOPENFILEERRORBOX);
    BOOL initOK  = WTInfo(WTI_DEFCONTEXT, 0, &lc) ;
    SetErrorMode(errmode);
    return initOK;
}


EXT_API(HCTX) OpenTablet (HWND hWnd)
{
    LOGCONTEXT lc;
    // Suppress error box, open, and restore error mode
    WORD errmode = SetErrorMode(SEM_NOOPENFILEERRORBOX);
    BOOL initOK  = WTInfo(WTI_DEFCONTEXT, 0, &lc) ;
    SetErrorMode(errmode);

    if (initOK) {
        strcpy(lc.lcName, "Fran tablet support");
        lc.lcOptions   = CXO_MESSAGES;
        lc.lcPktData   = PACKETDATA;
        lc.lcPktMode   = PACKETMODE;
        lc.lcMoveMask  = PACKETDATA;
        lc.lcBtnUpMask = lc.lcBtnDnMask;

        // Minimum out extent is 2000.  Maintain proportions.
        LONG minInExt = min(lc.lcInExtX, lc.lcInExtY);
        lc.lcOutExtX = (2000 * lc.lcInExtX) / minInExt;
        lc.lcOutExtY = (2000 * lc.lcInExtY) / minInExt;

        // Center of tablet is (65536, 65536)
        lc.lcOutOrgX = 65536 - lc.lcOutExtX / 2;
        lc.lcOutOrgY = 65536 - lc.lcOutExtY / 2;

        return WTOpen(hWnd, &lc, TRUE);
    }
    else return NULL;
}

// Now some renamings of Wintab procedures.  I'd rather use Wintab
// directly, but I ran into a problem with a cygwin32 clash.  See if this
// approach solves the problem.

EXT_API(BOOL) XWTClose (HCTX hCtx)
{ return WTClose(hCtx); }
EXT_API(BOOL) XWTConfig(HCTX hCtx, HWND hWnd)
{ return WTConfig(hCtx, hWnd); }
EXT_API(BOOL) XWTPacket(HCTX hCtx, UINT serial, LPVOID pPacket)
{ return WTPacket(hCtx,serial,pPacket); }
