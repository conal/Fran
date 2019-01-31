#ifndef _SPRITELIB_H
#define _SPRITELIB_H

#include "cdecls.h"

// Open the sprite library.  Say how many screen pixels correspond to one
// length unit.
EXT_API(void) OpenSpriteLib (double screenPixelsPerLength);



EXT_API(void) CloseSpriteLib ();


// For improving the resolution of timeGetTime under NT.
EXT_API(void) SetTimerResolutionMS (int newRes);

#endif // _SPRITELIB_H
