// Sprites and sprite trees.

#include "StdAfx.h"
#include "Sprite.h"
#include "ddcheck.h"
#include "ddhelp.h"
#include "math.h"

// C interfaces for Haskell


EXT_API(FlipBook *)newFlipBook(IDirectDrawSurface *pSurface,
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

EXT_API(SIZE) flipBookSize(FlipBook *pFlipBook)
{ return pFlipBook->Size(); }

EXT_API(int) flipBookPages(FlipBook *pFlipBook)
{ return pFlipBook->Pages(); }

EXT_API(void) deleteFlipBook(FlipBook *pFlipBook)
{ delete pFlipBook; }


EXT_API(void) deleteSpriteTree (SpriteTree *pSpriteTree)
{ delete pSpriteTree; }


EXT_API(FlipSprite *) newFlipSprite (
    FlipBook *pFlipBook,
    double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
    double posX0, double posY0, 
    double scaleX0, double scaleY0, 
    double startPage,
    SpriteTreeChain rest)
{ return new FlipSprite(pFlipBook, cropLLx0, cropLLy0, cropURx0, cropURy0,
                        posX0, posY0, scaleX0, scaleY0, startPage, rest); }

EXT_API(SpriteTree *)
flipSpriteToSpriteTree (FlipSprite *pFlipSprite)
{ return pFlipSprite; }

EXT_API(void) updateFlipSprite (
    FlipSprite *pFlipSprite,
    SpriteTime t, 
 double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY,
    double page)
{ pFlipSprite->Update(t, cropLLx, cropLLy, cropURx, cropURy,
                      posX, posY, scaleX, scaleY, page); }

EXT_API(SimpleSprite *) newSimpleSprite (
    IDirectDrawSurface *pSurface, double ulX0, double ulY0, 
    double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
    double posX0, double posY0, 
    double scaleX0, double scaleY0, 
    SpriteTreeChain rest)
{ return new SimpleSprite(pSurface, ulX0, ulY0,
                          cropLLx0, cropLLy0, cropURx0, cropURy0,
                          posX0, posY0, scaleX0, scaleY0, rest); }

EXT_API(SpriteTree *)
simpleSpriteToSpriteTree (SimpleSprite *pSimple)
{ return pSimple; }

EXT_API(void) 
updateSimpleSprite (
    SimpleSprite *pSimple,
    SpriteTime t,
    HDDSurface pSurface, double ulX, double ulY,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY)
{ pSimple->Update(t, pSurface, ulX, ulY,
                  cropLLx, cropLLy, cropURx, cropURy,
                  posX, posY, scaleX, scaleY); }


EXT_API(MonochromeSprite *) newMonochromeSprite (
    double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
    double r0, double g0, double b0, 
    SpriteTreeChain rest)
{ return new MonochromeSprite(cropLLx0, cropLLy0, cropURx0, cropURy0,
                              r0,g0,b0,rest); }

EXT_API(SpriteTree*)
monochromeSpriteToSpriteTree (MonochromeSprite *pMono)
{ return pMono; }

EXT_API(void) updateMonochromeSprite (
    MonochromeSprite *pMono, SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double r, double g, double b)
{ pMono->Update(t,cropLLx, cropLLy, cropURx, cropURy, r,g,b); }

EXT_API(SoundSprite *)
newSoundSprite (
    IDirectSoundBuffer *pOrigBuffer,
    double vol0, double pan0, double freq0, 
    SpriteTreeChain rest)
{ return new SoundSprite (pOrigBuffer, vol0, pan0, freq0, rest); }

EXT_API(void) updateSoundSprite (
        SoundSprite *p_sSprite, SpriteTime t, double vol, double pan, double freq)
{ p_sSprite->Update(t,vol,pan,freq); }


EXT_API(SpriteTree *)
soundSpriteToSpriteTree (SoundSprite *pSound)
{ return pSound; }

// Update methods go here (volume, frequency)

EXT_API(SpriteGroup *)
newSpriteGroup (SpriteTreeChain elems, SpriteTreeChain rest)
{ return new SpriteGroup(elems, rest); }

EXT_API(SpriteTree *)
spriteGroupToSpriteTree (SpriteGroup *pSpriteGroup)
{ return pSpriteGroup; }

EXT_API(void)
ResetSpriteGroup (SpriteGroup *pSpriteGroup, SpriteTreeChain elems, BOOL isMutable)
{ pSpriteGroup->Reset(elems, isMutable); }



// End of C interface

// Iteratively paint all of the sprite trees in a chain (possibly empty/NULL).
void PaintAll(SpriteTreeChain chain, IDirectDrawSurface *pDest, SpriteTime t)
{
    while (chain != NULL) {
        chain->Paint(pDest, t);
        chain = chain->m_rest;
    }
}


// This guy shouldn't exist.  I'm having trouble with the linker wanting it.
void ImageSprite::Paint (IDirectDrawSurface *pDest, SpriteTime t)
{
    AfxMessageBox("In ImageSprite::Paint!!");
    ASSERT(FALSE);

}

// Convert to DDraw rectangle coord
static inline int toRectCoord (double x)
{
    return (int) (x  * g_screenPixelsPerLength + 0.5);
}

// Make a CRect from Fran-style min/max X/Y, where the upper-left corner
// has the given position ulX/Y relative to the Fran origin.
CRect makeRect(double ulX, double ulY,
               double minX, double minY, double maxX, double maxY)
{
    // Order of CRect constructor arguments is left, top, right, bottom.
    return CRect(toRectCoord (minX - ulX),
                 toRectCoord (ulY - maxY),
                 toRectCoord (maxX - ulX),
                 toRectCoord (ulY - minY));
}


// MinSpriteSize is global
define_global(int, MinSpriteSize, 1);

void TransformableSprite::Paint (IDirectDrawSurface *pDest, SpriteTime t)
{
    Lock();  // Lock out a would-be deleter.

    // Get source surface and rectangle.
    IDirectDrawSurface *srcSurf;
    CRect origSrcRect;
    GetSrc (t, &srcSurf, &origSrcRect);

    if (srcSurf) {
    CSize srcSize  = origSrcRect.Size(),
          destSize = GetDDSurfaceSize(pDest);

    // Sample sprite scale and motion
    double scaleX = m_scaleX.at(t), scaleY = m_scaleY.at(t),
           moveX  =   m_posX.at(t), moveY  = m_posY.at(t);

    if (scaleX != 0 && scaleY != 0) {
        // Negative scaling requires some special treatment.
        BOOL mirrorX = scaleX<0, mirrorY = scaleY<0;

        // Size of pre-transformed sprite.  (should save)
        double srcW = srcSize.cx / g_screenPixelsPerLength,
               srcH = srcSize.cy / g_screenPixelsPerLength;

        // Compute bounds of the sprite's projection.  Note that the
        // pre-transformed sprite has corners defined by m_ulX, m_ulY-srcH,
        // m__ulX+srcW, and m_ulY,
        double spriteMinX = moveX      + m_ulX * scaleX ,
               spriteMaxX = spriteMinX + srcW  * scaleX,
               spriteMaxY = moveY      + m_ulY * scaleY,
               spriteMinY = spriteMaxY - srcH  * scaleY;

#define REORDER(ty,x,y) { if (y<x) {ty _tmpVal=x;x=y;y=_tmpVal;} }

        // But oops negative scaling reverses min&max
        REORDER(double, spriteMinX, spriteMaxX)
        REORDER(double, spriteMinY, spriteMaxY)

        // Intersect projected sprite with given cropping limits, to
        // yield the cropping region in destination space.
        double destCropMinX = max(spriteMinX,m_cropLLx.at(t)),
               destCropMaxX = min(spriteMaxX,m_cropURx.at(t)),
               destCropMinY = max(spriteMinY,m_cropLLy.at(t)),
               destCropMaxY = min(spriteMaxY,m_cropURy.at(t));

        // ## Note: Rename "LLx", etc to "MinX", etc

        // To get our cropping region in source (sprite) space, map
        // dest cropping rect backwards through move `compose2` scale:
        double srcCropMinX = (destCropMinX - moveX) / scaleX,
               srcCropMaxX = (destCropMaxX - moveX) / scaleX,
               srcCropMinY = (destCropMinY - moveY) / scaleY,
               srcCropMaxY = (destCropMaxY - moveY) / scaleY;

        // And again, account for possible negative scaling
        REORDER(double, srcCropMinX, srcCropMaxX)
        REORDER(double, srcCropMinY, srcCropMaxY)

        // Then form rectangles for blitting.
        CRect srcRect = makeRect(m_ulX, m_ulY,
                                 srcCropMinX, srcCropMinY,
                                 srcCropMaxX, srcCropMaxY);
        // Adjust to be relative to the whole src surface.
        srcRect.OffsetRect(origSrcRect.left, origSrcRect.top);

        CRect destRect = makeRect(-destSize.cx/g_screenPixelsPerLength/2, 
                                  destSize.cy/g_screenPixelsPerLength/2, 
                                  destCropMinX, destCropMinY,
                                  destCropMaxX, destCropMaxY);

        // If the scaled sprite is big enough go on with the blitting.  Allow
        // very narrow scaled sprites, but never a zero width or height, since
        // DDraw bombs with DDERR_INVALIDRECT.
        int srcWPix  = srcRect.Width() , srcHPix  = srcRect.Height(),
            destWPix = destRect.Width(), destHPix = destRect.Height();
        if (srcWPix > 0 && srcHPix > 0 && destWPix > 0 && destHPix > 0 &&
            (destWPix >= MinSpriteSize || destHPix >= MinSpriteSize)) {

            // If the width or height is negative, we have to do mirroring,
            // via the final Blt flag.
            //HRESULT result;
            DDBLTFX dbf;
            BOOL mirror = mirrorX || mirrorY;

            if (mirror) {
                memset( &dbf, 0, sizeof( dbf ) );
                dbf.dwSize = sizeof(dbf);
                if (mirrorX) dbf.dwDDFX |= DDBLTFX_MIRRORLEFTRIGHT;
                if (mirrorY) dbf.dwDDFX |= DDBLTFX_MIRRORUPDOWN;
            }

            // Restore surfaces, if necessary.  Note that restoring the sprite
            // surface is problematic.  Flipbooks should have a special restore
            // method that reloads from a file.  Synthetic ("simple") sprites
            // should probably just wait to be re-rendered.  (But I'd like to
            // optimize away multiple renderings when an image is constant.)
            CheckAndRestoreDDS(srcSurf);
            CheckAndRestoreDDS(pDest);

            // Do the BLiT.  Using Blt rather than BltFast, since we're
            // clipping and stretching.
            ddcheck (pDest->Blt (&destRect, srcSurf, &srcRect,
                                 DDBLT_KEYSRC | DDBLT_WAIT |
                                 (mirror ? DDBLT_DDFX : 0),
                                 mirror ? &dbf : NULL));
            //TRACE("ImageSprite::Paint did Blt to back buffer\n");
        }
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

void ImageSprite::Update (
    SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy)
{
    m_cropLLx.SetGoalValue(cropLLx, t);
    m_cropLLy.SetGoalValue(cropLLy, t);
    m_cropURx.SetGoalValue(cropURx, t);
    m_cropURy.SetGoalValue(cropURy, t);
}

void TransformableSprite::Update (
    SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY)
{
    ImageSprite::Update(t, cropLLx, cropLLy, cropURx, cropURy);

    m_posX.SetGoalValue(posX, t);
    m_posY.SetGoalValue(posY, t);

    m_scaleX.SetGoalValue(scaleX, t);
    m_scaleY.SetGoalValue(scaleY, t);
}

void FlipSprite::Update (
    SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY,
    double page)
{
    TransformableSprite::Update(
        t, cropLLx, cropLLy, cropURx, cropURy,
        posX, posY, scaleX, scaleY);
    m_page.SetGoalValue(page, t);
}

void FlipSprite::GetSrc (SpriteTime t,
                         IDirectDrawSurface **pSrc,
                         CRect *pSrcRect)
{
    // Figure out which page we're on, and hand off to GetPage.
    m_pFlipBook->GetPage(m_page.atToInt(t), pSrc, pSrcRect);
}



/////////// SimpleSprite

void SimpleSprite::GetSrc (SpriteTime t, IDirectDrawSurface **pSrc,
                           CRect *pSrcRect)
{
    *pSrc = m_pSurface;
    // Surface may be uninitialized, in which case bail out...  I'm
    // phasing this out, but still used with 3D
    if (m_pSurface) {
    // Use surface size and point (0,0)
      *pSrcRect = CRect(CPoint(0, 0), GetDDSurfaceSize(m_pSurface));
    }
}

void SimpleSprite::Update (
    SpriteTime t, IDirectDrawSurface *pSurface,
    double ulX,  double ulY,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY)
{
    // Switch the surface, but wait until it's safe.
    // Should probably delay the switch.
    Lock();                             // lock out Paint

    // If there is an old surface and it differs from the new one, free it.
    // WARNING: This is not right!  For instance, suppose we're
    // ping-ponging between two constant surfaces.
    if (m_pSurface && pSurface != m_pSurface) 
        m_pSurface->Release();

    m_pSurface = pSurface;
    m_ulX = ulX; m_ulY = ulY;

    Unlock();
    // And update the behaviors
    TransformableSprite::Update(
        t, cropLLx, cropLLy, cropURx, cropURy,
        posX, posY, scaleX, scaleY);
}



void
MonochromeSprite::Update (
    SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double r, double g, double b)
{
    ImageSprite::Update(t, cropLLx, cropLLy, cropURx, cropURy);
    m_r.SetGoalValue(r,t);
    m_g.SetGoalValue(g,t);
    m_b.SetGoalValue(b,t);
}

static inline BYTE
doubleToByte(double x)
{
    return (BYTE) ((UINT) (x * 255 + 0.5) & 0xff);
}

void
MonochromeSprite::Paint (IDirectDrawSurface *pDest, SpriteTime t)
{
    // Figure out the rectangle and clear it.
    // ## Note: store the Fran size, and use it here and above.  Or,
    // better: figure out how to switch back to pixel-based computations.
    CSize destSize = GetDDSurfaceSize(pDest);
        double minX = m_cropLLx.at(t), minY = m_cropLLy.at(t),
                      maxX = m_cropURx.at(t), maxY = m_cropURy.at(t);
    CRect cropRect = makeRect (-destSize.cx/g_screenPixelsPerLength/2, 
                                destSize.cy/g_screenPixelsPerLength/2, 
                               minX, minY, maxX, maxY);
    if (cropRect.top<cropRect.bottom && cropRect.left<cropRect.right)
        clearDDSurfaceRect (pDest, cropRect,
                            RGB(doubleToByte(m_r.at(t)),
                                doubleToByte(m_g.at(t)),
                                doubleToByte(m_b.at(t))));
}

// Sound sprite.  For now, just "static", i.e., preloaded sounds

SoundSprite::SoundSprite (
    IDirectSoundBuffer *pOrigBuffer,
    double vol0, double pan0, double freq0, 
    SpriteTreeChain rest)
 : m_vol(vol0), m_pan(pan0), m_freq(freq0), SpriteTree(rest)
{
  if (g_pDSound) {
      dscheck (g_pDSound->DuplicateSoundBuffer(pOrigBuffer, &m_pDupBuffer));
      pOrigBuffer->GetFrequency(&m_origFreq);
      // Initialize with a first "paint".  Since there's really not ddraw
      // surface involved, use NULL.  Also, since the initial behaviors
      // are constant ones, the time doesn't matter, so use zero.
      Paint(NULL,0);
      // Always looping for now.  ## To do: fix this.
      m_pDupBuffer->Play(0,0,DSBPLAY_LOOPING);
  }
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
    int volDB = vol <= 0 ? -10000 : ds_clamp(log10 (vol) * 1000, -10000, 0);
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
