// Simple Sprite library

#ifndef _SPRITE_H
#define _SPRITE_H

#include "cdecls.h"
#include "GlobalVar.h"

#include "Behavior.h"
#include "ddhelp.h"
#include "DDrawEnv.h"

typedef LONG Pixels;

// C interface, for Haskell.

EXT_CLASS(FlipBook);
EXT_API(HFlipBook) newFlipBook(HDDSurface, Pixels width, Pixels height,
                              Pixels srcXFirst, Pixels srcYFirst,
                              int columns, int rows);
EXT_API(SIZE) flipBookSize(HFlipBook);
//EXT_API(int) flipBookWidth(HFlipBook);
//EXT_API(int) flipBookHeight(HFlipBook);
EXT_API(int) flipBookPages(HFlipBook);

EXT_API(void) deleteFlipBook(HFlipBook);


EXT_CLASS(SpriteTree);       // Musn't be NULL
typedef HSpriteTree SpriteTreeChain;  // Okay if NULL
EXT_API(void) deleteSpriteTree (HSpriteTree);

declare_global(int, MinSpriteSize);

EXT_CLASS(FlipSprite);
EXT_API(HFlipSprite) newFlipSprite (
    HFlipBook,
    double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
    double posX0, double posY0, 
    double scaleX0, double scaleY0, 
    double page0, SpriteTreeChain rest);
EXT_API(HSpriteTree) flipSpriteToSpriteTree (HFlipSprite);
EXT_API(void) updateFlipSprite (
    HFlipSprite, SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY,
    double page);

// Simple sprites have a second translations.  (ulX,ulY) gives the
// prescaled location of the upper left corner, relative to the
// image's origin.  Unlike (posX,posY), the upper-left is not interpolated,
// because it changes with the DDraw buffer.  Without this tweak, we were
// getting annoying slipping around.

EXT_CLASS(SimpleSprite);
EXT_API(HSimpleSprite) newSimpleSprite (
    HDDSurface, double ulX0, double ulY0,
    double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
    double posX0, double posY0, 
    double scaleX0, double scaleY0, 
    SpriteTreeChain rest);
EXT_API(HSpriteTree) simpleSpriteToSpriteTree (HSimpleSprite);
EXT_API(void) updateSimpleSprite (
    HSimpleSprite, SpriteTime t,
    HDDSurface pSurface, double ulX, double ulY,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double posX, double posY, 
    double scaleX, double scaleY);

EXT_CLASS(MonochromeSprite);
EXT_API(HMonochromeSprite) newMonochromeSprite (
    double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
    double r0, double g0, double b0, 
    SpriteTreeChain rest);
EXT_API(HSpriteTree) monochromeSpriteToSpriteTree (HMonochromeSprite);
EXT_API(void) updateMonochromeSprite (
    HMonochromeSprite,
    SpriteTime t,
    double cropLLx, double cropLLy, double cropURx, double cropURy,
    double r, double g, double b);

EXT_CLASS(SoundSprite);
EXT_API(HSoundSprite) newSoundSprite (
    HDSBuffer origBuffer,
    double vol0, double pan0, double freq0,
    BOOL repeat,
    SpriteTreeChain rest);
EXT_API(HSpriteTree) soundSpriteToSpriteTree (HSoundSprite);
EXT_API(void) updateSoundSprite (
	HSoundSprite, SpriteTime t, double vol, double pan, double freq);

EXT_CLASS(SpriteGroup);
EXT_API(HSpriteGroup) newSpriteGroup
  (SpriteTreeChain elems, SpriteTreeChain rest);
EXT_API(HSpriteTree) spriteGroupToSpriteTree (HSpriteGroup);
EXT_API(void) ResetSpriteGroup (HSpriteGroup hSpriteGroup,
                               SpriteTreeChain elems, BOOL isMutable);

#ifdef __cplusplus
// C++ interfaces to constructors, destructors, and methods.


// In SpriteLib.cpp
extern double g_screenPixelsPerLength;

class AFX_EXT_CLASS FlipBook {
public:
    FlipBook (IDirectDrawSurface *pSurface, Pixels width, Pixels height,
              Pixels srcXFirst, Pixels srcYFirst,
              int columns, int rows);
    void GetPage (int pageNum, IDirectDrawSurface **pSrc, CRect *pSrcRect);
    Pixels Width  () const { return m_width ; }
    Pixels Height () const { return m_height; }
    SIZE Size () const { return CSize(m_width, m_height); }
    int Pages () const { return m_numPages; }
protected:
    IDirectDrawSurface *m_pSurface;
    Pixels m_width, m_height;
    Pixels m_srcXFirst, m_srcYFirst;
    int m_numPages, m_pagesPerRow;
};

class SpriteTree;

// Chain of sprite trees.  Sorted back-to-front.  Could be empty/NULL.
typedef SpriteTree *SpriteTreeChain;

class AFX_EXT_CLASS SpriteTree {
public:
    SpriteTree(SpriteTreeChain rest) : m_rest(rest) { }
    // Paint (BLiT) onto a DDraw surface (probably the back buffer).
    virtual ~SpriteTree() { DELETEIF (m_rest); }
    // 
    friend void PaintAll (SpriteTree *chain, 
        IDirectDrawSurface *pDest, SpriteTime t);
protected:
    virtual void Paint (IDirectDrawSurface *pDest, SpriteTime t) = 0;
private:
    SpriteTreeChain m_rest;
};

void paintAndFlip (SpriteTree*, DDrawEnv*, SpriteTime);

// ImageSprite subclass of SpriteTree.  Is croppable.
class AFX_EXT_CLASS ImageSprite : public SpriteTree {
public:
    ImageSprite (
        double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
        SpriteTreeChain rest)
	  : SpriteTree(rest),
            m_cropLLx(cropLLx0), m_cropLLy(cropLLy0), 
            m_cropURx(cropURx0), m_cropURy(cropURy0) {}
    void Update (
        SpriteTime t, 
        double cropLLx, double cropLLy, double cropURx, double cropURy);
protected:
    virtual void Paint (IDirectDrawSurface *pDest, SpriteTime t) = 0;
    // Cropping
    LinearDouble m_cropLLx, m_cropLLy, m_cropURx, m_cropURy;
    virtual void Lock () { }
    virtual void Unlock () { }
};

// Finite, and thus transformable sprites.  (The infinite ones are
// transformable as well, but more simply.)
class AFX_EXT_CLASS TransformableSprite : public ImageSprite {
public:
    TransformableSprite (
        double ulX0,  double ulY0,
        double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
        double posX0, double posY0,
        double scaleX0, double scaleY0, 
        SpriteTreeChain rest)
        : ImageSprite(cropLLx0, cropLLy0, cropURx0,cropURy0,rest),
          m_ulX(ulX0), m_ulY(ulY0),
          m_posX(posX0), m_posY(posY0), 
          m_scaleX(scaleX0), m_scaleY(scaleY0) {}
    void Update (
        SpriteTime t, 
        double cropLLx, double cropLLy, double cropURx, double cropURy,
        double posX, double posY, 
        double scaleX, double scaleY);
protected:
    void Paint (IDirectDrawSurface *pDest, SpriteTime t);
    // Get a surface and rectangle.
    virtual void GetSrc (SpriteTime t,
                         IDirectDrawSurface **pSrc,
                         CRect *pSrcRect) = 0;
    // Offset in pixels of the sprite's upper left corner with respect to
    // the sprite origin.
    double m_ulX, m_ulY;
    // translation, and scaling
    LinearDouble m_posX, m_posY;
    LinearDouble m_scaleX, m_scaleY;
};


// FlipBook-based sprite.
class AFX_EXT_CLASS FlipSprite : public TransformableSprite {
public:
    FlipSprite (
        FlipBook *pFlipBook,
        double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
        double posX0, double posY0, 
        double scaleX0, double scaleY0, 
        double page0,
        SpriteTreeChain rest) :
        TransformableSprite(-pFlipBook->Width() /g_screenPixelsPerLength/2,
                             pFlipBook->Height()/g_screenPixelsPerLength/2,
                    cropLLx0, cropLLy0, cropURx0, cropURy0,
                    posX0, posY0, scaleX0, scaleY0, rest),
        m_pFlipBook(pFlipBook), m_page(page0) { }
    void Update (
        SpriteTime t, 
        double cropLLx, double cropLLy, double cropURx, double cropURy,
        double posX, double posY, 
        double scaleX, double scaleY,
        double page);
    
protected:
    void GetSrc (SpriteTime t, IDirectDrawSurface **pSrc,
                 CRect *pSrcRect);
    FlipBook *m_pFlipBook;
    LinearDouble m_page;
};    


// Simple sprite, with just a Surface in it.
class AFX_EXT_CLASS SimpleSprite : public TransformableSprite {
public:
    SimpleSprite (
        IDirectDrawSurface *pSurface,
        double ulX0,  double ulY0,
        double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
        double posX0, double posY0, 
        double scaleX0, double scaleY0, 
        SpriteTreeChain rest) :
        TransformableSprite(ulX0, ulY0, cropLLx0, cropLLy0, cropURx0, cropURy0,
                    posX0, posY0, scaleX0, scaleY0, rest),
        m_pSurface(pSurface) { }
    void Update (
        SpriteTime t, 
        IDirectDrawSurface *pSurface, double ulX, double ulY,
        double cropLLx, double cropLLy, double cropURx, double cropURy,
        double posX, double posY, 
        double scaleX, double scaleY);
    ~SimpleSprite() { RELEASEIF(m_pSurface); }
protected:
    void GetSrc (SpriteTime t, IDirectDrawSurface **pSrc,
                 CRect *pSrcRect);
    void Lock   () { m_critSec.Lock()  ; }
    void Unlock () { m_critSec.Unlock(); }

    IDirectDrawSurface *m_pSurface;
    CCriticalSection m_critSec;
};

class AFX_EXT_CLASS MonochromeSprite : public ImageSprite {
public:
    MonochromeSprite (
        double cropLLx0, double cropLLy0, double cropURx0, double cropURy0,
        double r0, double g0, double b0, 
        SpriteTreeChain rest)
        : ImageSprite(cropLLx0, cropLLy0, cropURx0, cropURy0, rest),
          m_r(r0), m_g(g0), m_b(b0) { }
    void Paint (IDirectDrawSurface *pDest, SpriteTime t);
    void Update (
        SpriteTime t,
        double cropLLx, double cropLLy, double cropURx, double cropURy,
        double r, double g, double b);
    ~MonochromeSprite() { }
protected:
    LinearDouble m_r, m_g, m_b;
};


#ifndef NO_SOUND_SPRITE

// Sound sprite.  For now, just "static", i.e., preloaded sounds

class AFX_EXT_CLASS SoundSprite : public SpriteTree {
public:
    SoundSprite (IDirectSoundBuffer *pOrigBuffer,
        double vol0, double pan0, double freq0, BOOL repeat,
        SpriteTreeChain rest);
	// Experimenting with a different style
	void Update (SpriteTime t, double vol, double pan, double freq);
    ~SoundSprite() { m_pDupBuffer->Release(); }
protected:
    void Paint (IDirectDrawSurface *pDest, SpriteTime t);
    IDirectSoundBuffer *m_pDupBuffer;
	DWORD m_origFreq;
	LinearDouble m_vol, m_pan, m_freq;
};

#endif // NO_SOUND_SPRITE

class AFX_EXT_CLASS SpriteGroup : public SpriteTree {
public:
    SpriteGroup (SpriteTreeChain elems, SpriteTreeChain rest,
                 BOOL isMutable=TRUE)
         : SpriteTree(rest), m_elems(elems), m_isMutable(isMutable) { }
    // Reset the elements.  By default, now immutable
    void Reset (SpriteTreeChain elems, BOOL isMutable=TRUE);
    ~SpriteGroup() { DELETEIF(m_elems); }
protected:
    void Paint (IDirectDrawSurface *pDest, SpriteTime t);
private:
    SpriteTreeChain m_elems;
    BOOL m_isMutable;
    void Lock   () { m_critSec.Lock()  ; }
    void Unlock () { m_critSec.Unlock(); }
    CCriticalSection m_critSec;
};

#endif // __cplusplus
#endif // _SPRITE_H 
