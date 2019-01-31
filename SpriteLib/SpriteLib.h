#ifndef _SPRITELIB_H
#define _SPRITELIB_H

#include "cdecls.h"

EXT_API void OpenSpriteLib ();
EXT_API void CloseSpriteLib ();

// For improving the resolution of timeGetTime under NT.
EXT_API void SetTimerResolutionMS (int newRes);

#endif // _SPRITELIB_H
