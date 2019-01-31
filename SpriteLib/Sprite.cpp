// Sprites and sprite trees.

// I think the "EXT_API " prefixes are unnecessary.  Try removing sometime.


#include "StdAfx.h"
#include "Sprite.h"
#include "ddcheck.h"
#include "ddhelp.h"

// C interfaces for Haskell


EXT_API FlipBook *newFlipBook(IDirectDrawSurface *pSurface,
                              Pixels width, Pixels height,
                              Pixels srcXFirst, Pixels srcYFirst,
                              int columns, int rows)
{ 
    CSize surfSize = GetDDSurfaceSize(pSurface);
    if (srcXFirst < 0 || srcXFirst + width  * columns > surfSize.cx ||
        srcYFirst < 0 || srcXFirst + height * rows    > surfSize.cy) {
        AfxMessageBox("Bad flip book size");
        return NULL;
    }

    return new FlipBook(pSurface, width, height,
                        srcXFirst, srcYFirst, columns, rows); }

EXT_API int flipBookWidth(FlipBook *pFlipBook)
{ return pFlipBook->Width(); }

EXT_API int flipBookHeight(FlipBook *pFlipBook)
{ return pFlipBook->Height(); }

EXT_API int flipBookPages(FlipBook *pFlipBook)
{ return pFlipBook->Pages(); }

EXT_API void deleteFlipBook(FlipBook *pFlipBook)
{ delete pFlipBook; }


EXT_API void deleteSpriteTree (SpriteTree *pSpriteTree)
{ delete pSpriteTree; }


EXT_API void setGoalPosition(Sprite *pSprite,
                             double posX, double posY,
                             SpriteTime goalTime)
{ pSprite->SetGoalPosition(posX, posY, goalTime); }

EXT_API void setGoalScale(Sprite *pSprite,
                          double scaleX, double scaleY,
                          SpriteTime goalTime)
{ pSprite->SetGoalScale(scaleX, scaleY, goalTime); }

EXT_API SpriteTree *spriteToSpriteTree(Sprite *pSprite)
{ return pSprite; }



EXT_API FlipSprite *
newFlipSprite (FlipBook *pFlipBook,
               double posX0, double posY0, 
               double scaleX0, double scaleY0, 
               double startPage,
               SpriteTreeChain rest)
{ return new FlipSprite(pFlipBook, posX0, posY0, scaleX0, scaleY0, startPage, rest); }

EXT_API Sprite *
flipSpriteToSprite (FlipSprite *pFlipSprite)
{ return pFlipSprite; }

EXT_API void
setGoalPage (FlipSprite *pFlipSprite,
              double goalPage, SpriteTime goalTime)
{ pFlipSprite->SetGoalPage(goalPage, goalTime); }


EXT_API SimpleSprite *
newSimpleSprite (IDirectDrawSurface *pSurface,
                 double posX0, double posY0, 
                 double scaleX0, double scaleY0, 
                 SpriteTreeChain rest)
{ return new SimpleSprite(pSurface, posX0, posY0, scaleX0, scaleY0, rest); }

EXT_API Sprite *
simpleSpriteToSprite (SimpleSprite *pSimple)
{ return pSimple; }

EXT_API void
setSurface (SimpleSprite *pSimple, IDirectDrawSurface *pSurface)
{ pSimple->SetSurface(pSurface); }



EXT_API SoundSprite *
newSoundSprite (
    IDirectSoundBuffer *pOrigBuffer,
    double vol0, double pan0, double freq0, 
    SpriteTreeChain rest)
{ return new SoundSprite (pOrigBuffer, vol0, pan0, freq0, rest); }

// Experimenting with a different style
void updateSoundSprite (
        SoundSprite *p_sSprite, SpriteTime t, double vol, double pan, double freq)
{ p_sSprite->Update(t,vol,pan,freq); }


EXT_API SpriteTree *
soundSpriteToSpriteTree (SoundSprite *pSound)
{ return pSound; }

// Update methods go here (volume, frequency)

EXT_API SpriteGroup *
newSpriteGroup (SpriteTreeChain elems, SpriteTreeChain rest)
{ return new SpriteGroup(elems, rest); }

EXT_API SpriteTree *
spriteGroupToSpriteTree (SpriteGroup *pSpriteGroup)
{ return pSpriteGroup; }

EXT_API void
ResetSpriteGroup (SpriteGroup *pSpriteGroup, SpriteTreeChain elems, BOOL isMutable)
{ pSpriteGroup->Reset(elems, isMutable); }



// End of C interface

// To be obsolete, because of VBlankHandler
void paintAndFlip(SpriteTreeChain chain, DDrawEnv *env, SpriteTime paintTime)
{
    env->Lock();
    //{ CSingleLock lock(&(env->m_syncObj), TRUE);
    //TRACE("paintAndFlip grabbed DDrawEnv lock.\n");

    env->ClearBack();
    PaintAll(chain, env->GetBack(), paintTime);
    env->Flip();

    //TRACE("   paintAndFlip releasing DDrawEnv lock.\n");
    // } // unlock
    env->Unlock();
}

// Iteratively paint all of the sprite trees in a chain (possibly empty/NULL).
void PaintAll(SpriteTreeChain chain, IDirectDrawSurface *pDest, SpriteTime t)
{
    while (chain != NULL) {
        chain->Paint(pDest, t);
        chain = chain->m_rest;
    }
}

EXT_DEF_DATA int MinSpriteSize = 1;

void ImageSprite::Paint (IDirectDrawSurface *pDest, SpriteTime t)
{
    CRect srcRect;
    IDirectDrawSurface *srcSurf;
    Lock();

    // Get source surface and rectangle.
    GetSrc (t, &srcSurf, &srcRect);
    if (srcSurf) {
        CSize destSize = GetDDSurfaceSize(pDest);
        // First, calculate sprite center relative to window upper-left,
        // recalling that posX, posY are of the sprite's center, relative
        // to the window's center, and window coords use positive == down.
        int posXRelDestUL = m_posX.atToInt(t) + destSize.cx/2,
            posYRelDestUL = (-m_posY.atToInt(t)) + destSize.cy/2;
        // Next, the transformed sprite's height and width in pixels
        int width  = (int) (srcRect.Width () * m_scaleX.at(t) + 0.5),
            height = (int) (srcRect.Height() * m_scaleY.at(t) + 0.5);
        // Then sprite upper-left relative to window upper-left.
        int destULx = posXRelDestUL - width/2,
            destULy = posYRelDestUL - height/2;
        CRect destRect (destULx, destULy,
                        destULx + width, destULy + height);
        // Do the BLiT.  Using Blt rather than BltFast, since we're
        // clipping and stretching.  However, first check for a large
        // enough rectangle
        if (destRect.Width() >= MinSpriteSize &&
            destRect.Height() >= MinSpriteSize) {
            ddcheck (pDest->Blt (&destRect, srcSurf, &srcRect,
                                 DDBLT_KEYSRC | DDBLT_WAIT, NULL));
            //TRACE("ImageSprite::Paint did Blt to back buffer\n");
        }
    }
    Unlock();
}

// Flip books

FlipBook::FlipBook (IDirectDrawSurface *pSurface,
                    Pixels width, Pixels height,
                    Pixels srcXFirst, Pixels srcYFirst,
                    int columns, int rows)
:     m_pSurface(pSurface), m_width(width), m_height(height)
    , m_srcXFirst(srcXFirst), m_srcYFirst(srcYFirst)
    , m_numPages(columns * rows), m_pagesPerRow(columns)
{
    TRACE("FlipBook::FlipBook, with %d pages\n", m_numPages);
}

void FlipBook::GetPage (int pageNum, IDirectDrawSurface **pSrc, CRect *pSrcRect)
{
    *pSrc = m_pSurface;

    // Wrap pageNum if necessary.
    pageNum =  pageNum % m_numPages;
    // Apparently, m % n is negative for negative m, so fix here.
    if (pageNum < 0) { pageNum += m_numPages; }

    // Compute and fill in the rectangle.  Should probably cache
    // pageNum and srcRect, and use old srcRect when pageNum hasn't
    // changed.
    const Pixels width = Width(), height = Height();
    int pageRow    = pageNum / m_pagesPerRow;
    int pageColumn = pageNum % m_pagesPerRow;
    int rectMinX    = m_srcXFirst + width  * pageColumn;
    int rectMinY    = m_srcYFirst + height * pageRow;

    pSrcRect->SetRect(rectMinX        , rectMinY,
                      rectMinX + width, rectMinY + height);
}


void FlipSprite::GetSrc (SpriteTime t, IDirectDrawSurface **pSrc,
                         CRect *pSrcRect)
{
    // Figure out which page we're on, and hand off to GetPage.

    m_pFlipBook->GetPage(m_page.atToInt(t), pSrc, pSrcRect);
}



/////////// SimpleSprite

SimpleSprite::SimpleSprite (IDirectDrawSurface *pSurface,
                            double posX0, double posY0, 
                            double scaleX0, double scaleY0, 
                            SpriteTreeChain rest) :
    ImageSprite(posX0, posY0, scaleX0, scaleY0, rest),
    m_pSurface(pSurface)
{
    TRACE("SimpleSprite::SimpleSprite\n");
}


void SimpleSprite::GetSrc (SpriteTime t, IDirectDrawSurface **pSrc,
                           CRect *pSrcRect)
{
    *pSrc = m_pSurface;

    // Get surface size.
    DDSURFACEDESC ddsd;
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    ddcheck (m_pSurface->GetSurfaceDesc(&ddsd));

    *pSrcRect = CRect(0, 0, ddsd.dwWidth, ddsd.dwHeight);
}

 void SimpleSprite::SetSurface (IDirectDrawSurface *pSurface)
{
    Lock();				// lock out Paint

    // If the new surface differs from the old one, free the old.
    // I don't think this is quite
    if (pSurface != m_pSurface) {
	m_pSurface->Release();
	m_pSurface = pSurface;
    }

    Unlock();
}



// Sound sprite.  For now, just "static", i.e., preloaded sounds

SoundSprite::SoundSprite (
    IDirectSoundBuffer *pOrigBuffer,
    double vol0, double pan0, double freq0, 
    SpriteTreeChain rest)
 : m_vol(vol0), m_pan(pan0), m_freq(freq0), SpriteTree(rest)
{
  dscheck (g_pDSound->DuplicateSoundBuffer(pOrigBuffer, &m_pDupBuffer));
  // Always looping for now
  m_pDupBuffer->Play(0,0,DSBPLAY_LOOPING);
  pOrigBuffer->GetFrequency(&m_origFreq);
  // m_pDupBuffer->SetVolume(500);
};


void SoundSprite::Update (SpriteTime t, double vol, double pan, double freq)
{
    m_vol.SetGoalValue(vol,t);
    m_pan.SetGoalValue(pan,t);
    m_freq.SetGoalValue(freq,t);
}

// To "paint" a sound sprite really means updating the dsound (duplicate)
// buffer's parameters.

inline int ds_clamp (double val, int lo, int hi)
{   
    int ival = (DWORD) val; 
    return ival < lo ? lo : ival > hi ? hi : ival; 
}

void SoundSprite::Paint (IDirectDrawSurface *, SpriteTime t)
{
    double vol = m_vol.at(t);
    // Convert vol factor to decibels, using dB = 10 * log10 (I2/I1),
    // where I1 is orig intensity and I2 is new intensity.
    // Bummer: DSound support attenuation, but not amplification :-(
    // DSound wants hundredths of DB
    int volDB = vol < 0 ? -10000 : ds_clamp(log10 (vol) * 1000, -10000, 0);
    dscheck(m_pDupBuffer->SetVolume(volDB));
    // For pan, assume in DB already.  What's best?
    int panDB = ds_clamp(m_pan.at(t) * 100, -10000, 10000);
    dscheck(m_pDupBuffer->SetPan(panDB));
    // Adjust the original frequency
    int freq = ds_clamp(m_freq.at(t) * m_origFreq, 100, 100000);
    dscheck(m_pDupBuffer->SetFrequency(freq));
}


// SpriteGroup

void SpriteGroup::Reset (SpriteTreeChain elems, BOOL isMutable)
{
    Lock();  // Protect Paint
    ASSERT (m_isMutable);  // Must currently be mutable
    // Delete all the elements
    DELETEIF(m_elems);
    m_elems = elems; 
    m_isMutable = isMutable; 
    Unlock();
}

void SpriteGroup::Paint (IDirectDrawSurface *pDest, SpriteTime t)
{
    Lock(); // Protect from Reset
    // To do: optimize away this guy when m_isMutable is FALSE.
    PaintAll(m_elems, pDest, t);
    Unlock();
}
