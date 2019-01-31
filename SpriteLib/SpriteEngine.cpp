// Sprite engine, built on top of VBlankHandler

#include "StdAfx.h"
#include "SpriteEngine.h"

// C interfaces for Haskell


EXT_API HSpriteEngine newSpriteEngine(HWND hWnd, HSpriteTree pTree)
{ return new SpriteEngine(hWnd, pTree); }


EXT_API void onResizeSpriteEngine(HSpriteEngine pEngine)
{ pEngine->OnResize(); }

// Have deletion function return the frame count.
EXT_API int deleteSpriteEngine(HSpriteEngine pEngine)
{ 
    int frameCount = pEngine->GetFrameCount();
    delete pEngine; 
    return frameCount;
}

// End of C interfaces


void SpriteEngine::VBlankSetup()
{
    m_pDDrawEnv->Lock();

    m_pDDrawEnv->ClearBack();
    PaintAll(m_pSpriteTree, m_pDDrawEnv->GetBack(), CurrentSpriteTime());
}

void SpriteEngine::OnVBlank()
{
    m_pDDrawEnv->Flip();
    m_pDDrawEnv->Unlock();
    m_frameCount++;
}


SpriteEngine::SpriteEngine(HWND hWnd, SpriteTree *pSpriteTree)
 :  m_frameCount(0)
{
    TRACE("SpriteEngine::SpriteEngine()\n");

    VERIFY (m_pDDrawEnv = new DDrawEnv(hWnd));
    m_pSpriteTree = pSpriteTree;
    m_startTime = CurrentSpriteTime();
    // Get into the vblank job queue
    m_vBlankJobNum = AddVBlankJob(this);
}

    
SpriteEngine::~SpriteEngine()
{
    // Drop out of the vblank job queue
    RemoveVBlankJob(m_vBlankJobNum);
    double dt = CurrentSpriteTime() - m_startTime;
    // Show stats
    TRACE("SpriteEngine::~SpriteEngine().  %d frames in %f seconds == %f fps\n",
           m_frameCount, dt, dt==0.0? 0 : m_frameCount/dt);
    // The engine creates and destroys the DDrawEnv, but not the SpriteTree
    delete m_pDDrawEnv;
}
