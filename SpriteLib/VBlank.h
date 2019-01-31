// Vertical blank synchronization

#NotUsing // Generate an error if accidentally included.

#ifndef _VBLANK_H
#define _VBLANK_H

// Initialize and finalize vblank.  (Call in DDrawEnvInit and ..)
void VBlankInit();
void VBlankFini();

// Efficiently block until beginning of vertical blank
void BlockUntilVerticalBlank();

#endif /* _VBLANK_H */
