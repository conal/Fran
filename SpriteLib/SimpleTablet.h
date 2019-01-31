// Simplified interface to Wintab

#ifndef _SIMPLETABLET_H
#define _SIMPLETABLET_H

#include "cdecls.h"

#define PACKETDATA	(PK_X | PK_Y | PK_BUTTONS | PK_CHANGED | PK_NORMAL_PRESSURE)
#define PACKETMODE	PK_BUTTONS
#include "pktdef.h"

typedef HCTX TabCtx;

// Open the tablet.  Returns the tablet context or NULL on failure (e.g.,
// no tablet.
EXT_API(TabCtx) OpenTablet (HWND);

EXT_API(BOOL) XWTClose (TabCtx ctx);
EXT_API(BOOL) XWTConfig(TabCtx ctx, HWND hWnd);
EXT_API(BOOL) XWTPacket(TabCtx ctx, UINT serial, LPVOID pPacket);

EXT_API(BOOL) TestForTablet(HWND hWnd);

#endif /* _SIMPLETABLET_H */
