// Vertical blank synchronization

#ifndef _VBLANKHANDLER_H
#define _VBLANKHANDLER_H

#include "cdecls.h"
#include "GlobalVar.h"

// Can be set from outside, for experimentation.  Goal period for vertical
// blank activities, in milliseconds.  Of course, it should really be the
// vertical blank.  If negative, use WaitForVerticalBlank, which is
// implemented very badly (by spinning!!) in current DirectDraw (as of
// 4/97).  On NT, with timerResolutionMS in SpriteLib.cpp set to 10, the
// following number will be rounded up to a multiple of 10, or if set to 5
// (minimum), then one more than the number will be rounded up to a
// multiple of 5.
declare_global(int, vblankPeriodMS);


// Set priority of the vblank handler thread.
EXT_API(void) SetVblankThreadPriority(int priority);


// end of C/Haskell interfaces

#ifdef __cplusplus
// C++ interfaces to constructors, destructors, and methods.


// Initialize and finalize vblank.  (Call in DDrawEnvInit and ..)
void VBlankInit();
void VBlankFini();

class VBlankJob {
public:
    virtual void VBlankSetup () = 0;
    virtual void OnVBlank () = 0;
};

// Add or remove jobs

int AddVBlankJob(VBlankJob* pJob);
void RemoveVBlankJob(int);

#endif // __cplusplus

#endif /* _VBLANKHANDLER_H */
