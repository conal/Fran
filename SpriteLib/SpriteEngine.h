// SpriteEngine.h: Sprite engine.  Runs in its own thread, trying to keep
// up with, but not exceed, the video refresh rate.

#ifndef _SPRITEENGINE_H
#define _SPRITEENGINE_H

#include "cdecls.h"

#include "DDrawEnv.h"
#include "Sprite.h"
#include "Behavior.h"
#include "VBlankHandler.h"


// C interface, for Haskell.

EXT_CLASS(SpriteEngine);

EXT_API(HSpriteEngine) newSpriteEngine(HWND, HSpriteTree);
EXT_API(void) onResizeSpriteEngine(HSpriteEngine);
// Stop an engine and return number of frames
EXT_API(int) deleteSpriteEngine(HSpriteEngine);


#ifdef __cplusplus

// C++ interfaces to constructors, destructors, and methods:

class AFX_EXT_CLASS SpriteEngine : public VBlankJob {
public:
    SpriteEngine(HWND hWnd, SpriteTree *pSpriteTree);
    void OnResize() { m_pDDrawEnv->OnResize(); }
    ~SpriteEngine();
    int GetFrameCount() { return m_frameCount; }
    // VBlankJob methods
    void VBlankSetup ();
    void OnVBlank ();
private:
    int m_frameCount;
    SpriteTime m_startTime;
    DDrawEnv *m_pDDrawEnv;
    SpriteTree *m_pSpriteTree;
    int m_vBlankJobNum;
};

#endif // __cplusplus
#endif // _SPRITEENGINE_H
