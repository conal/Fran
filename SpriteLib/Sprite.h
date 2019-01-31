// Simple Sprite library

#ifndef _SPRITE_H
#define _SPRITE_H

#include "cdecls.h"

#include "Behavior.h"
#include "ddhelp.h"
// #include "Surface.h"
#include "DDrawEnv.h"

typedef LONG Pixels;

// C interface, for Haskell.

EXT_CLASS(FlipBook);
EXT_API HFlipBook newFlipBook(HDDSurface, Pixels width, Pixels height,
                              Pixels srcXFirst, Pixels srcYFirst,
                              int columns, int rows);
EXT_API int flipBookWidth(HFlipBook);
EXT_API int flipBookHeight(HFlipBook);
EXT_API int flipBookPages(HFlipBook);

EXT_API void deleteFlipBook(HFlipBook);


EXT_CLASS(SpriteTree);       // Musn't be NULL
typedef HSpriteTree SpriteTreeChain;  // Okay if NULL
EXT_API void deleteSpriteTree (HSpriteTree);

EXT_CLASS(Sprite);
EXT_API void setGoalPosition(HSprite, double posX, double posY,
                             SpriteTime goalTime);
EXT_API void setGoalScale(HSprite, double scaleX, double scaleY,
                          SpriteTime goalTime);
EXT_API HSpriteTree spriteToSpriteTree(HSprite);
EXT_DECL_DATA int MinSpriteSize;

EXT_CLASS(FlipSprite);

EXT_API HFlipSprite newFlipSprite (HFlipBook,
               double posX0, double posY0, 
               double scaleX0, double scaleY0, 
               double page0, SpriteTreeChain rest);
EXT_API HSprite flipSpriteToSprite (HFlipSprite);
EXT_API void setGoalPage 
  (HFlipSprite, double goalPage, SpriteTime startTime);

EXT_CLASS(SimpleSprite);
EXT_API HSimpleSprite newSimpleSprite (HDDSurface,
		       double posX0, double posY0, 
		       double scaleX0, double scaleY0, 
		       SpriteTreeChain rest);
EXT_API HSprite simpleSpriteToSprite (HSimpleSprite);
EXT_API void setSurface (HSimpleSprite, HDDSurface);

EXT_CLASS(SoundSprite);
EXT_API HSoundSprite newSoundSprite (
    HDSBuffer origBuffer,
    double vol0, double pan0, double freq0, 
    SpriteTreeChain rest);
EXT_API HSprite soundSpriteToSprite (HSoundSprite);
// Experimenting with a different style
EXT_API void updateSoundSprite (
	HSoundSprite, SpriteTime t, double vol, double pan, double freq);

EXT_CLASS(SpriteGroup);
EXT_API HSpriteGroup newSpriteGroup
  (SpriteTreeChain elems, SpriteTreeChain rest);
EXT_API HSpriteTree spriteGroupToSpriteTree (HSpriteGroup);
EXT_API void ResetSpriteGroup (HSpriteGroup hSpriteGroup,
                               SpriteTreeChain elems, BOOL isMutable);

#ifdef __cplusplus
// C++ interfaces to constructors, destructors, and methods.

class AFX_EXT_CLASS FlipBook {
public:
    FlipBook (IDirectDrawSurface *pSurface, Pixels width, Pixels height,
              Pixels srcXFirst, Pixels srcYFirst,
              int columns, int rows);
    void GetPage (int pageNum, IDirectDrawSurface **pSrc, CRect *pSrcRect);
    Pixels Width  () const { return m_width ; }
    Pixels Height () const { return m_height; }
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


// Sprite is a subclass of SpriteTree.  Has translation and scaling
// approximation functions, represented in a form that is easy to
// sample incrementally.

class AFX_EXT_CLASS Sprite : public SpriteTree {
public:
    Sprite (double posX0, double posY0,
            double scaleX0, double scaleY0, 
            SpriteTreeChain rest)
     : SpriteTree(rest),
       m_posX(posX0), m_posY(posY0), 
       m_scaleX(scaleX0), m_scaleY(scaleY0) { }
    void SetGoalPosition (double posXVal, double posYVal, SpriteTime goalTime) {
        m_posX.SetGoalValue(posXVal, goalTime);
        m_posY.SetGoalValue(posYVal, goalTime);
    }
    void SetGoalScale (double scaleXVal, double scaleYVal, SpriteTime goalTime) {
        m_scaleX.SetGoalValue(scaleXVal, goalTime);
        m_scaleY.SetGoalValue(scaleYVal, goalTime);
    }
protected:
    // Grab and release any mutable resources owned by this sprite and not
    // protected on their own.
    virtual void Lock () { }
    virtual void Unlock () { }
    LinearDouble m_posX, m_posY;
    LinearDouble m_scaleX, m_scaleY;
};

// To do: eliminate the Sprite/ImageSprite distinction.  It stemmed from
// SoundSprite, which used to be a subclass of Sprite.

// ImageSprite subclass of Sprite.  Has a GetSrc method that takes a
// time (what units?) and returns a DDraw source surface pointer and XY
// offset.
class AFX_EXT_CLASS ImageSprite : public Sprite {
public:
    ImageSprite (double posX0, double posY0,
                 double scaleX0, double scaleY0, 
                 SpriteTreeChain rest)
	  : Sprite(posX0, posY0, scaleX0, scaleY0, rest) {}
protected:
    void Paint (IDirectDrawSurface *pDest, SpriteTime t);
    // Get a surface and rectangle for blitting.
    virtual void GetSrc (SpriteTime t,
                         IDirectDrawSurface **pSrc,
                         CRect *pSrcRect) = 0;
};


// FlipBook-based sprite.

class AFX_EXT_CLASS FlipSprite : public ImageSprite {
public:
    FlipSprite (FlipBook *pFlipBook,
                double posX0, double posY0, 
                double scaleX0, double scaleY0, 
                double page0,
                SpriteTreeChain rest) :
        ImageSprite(posX0, posY0, scaleX0, scaleY0, rest), 
        m_pFlipBook(pFlipBook), m_page(page0) { }
    void SetGoalPage (double pageVal, SpriteTime goalTime) {
        m_page.SetGoalValue(pageVal, goalTime); }
protected:
    void GetSrc (SpriteTime t, IDirectDrawSurface **pSrc, CRect *pSrcRect);
    FlipBook *m_pFlipBook;
    LinearDouble m_page;
};    


// Simple sprite, with just a Surface in it.

class AFX_EXT_CLASS SimpleSprite : public ImageSprite {
public:
    SimpleSprite (IDirectDrawSurface *pSurface,
		  double posX0, double posY0, 
		  double scaleX0, double scaleY0, 
		  SpriteTreeChain rest);
    void SetSurface (IDirectDrawSurface *pSurface);
    void Render (SpriteTime t) ;
    ~SimpleSprite() { m_pSurface->Release(); }
protected:
    void GetSrc (SpriteTime t, IDirectDrawSurface **pSrc, CRect *pSrcRect);
    void Lock   () { m_critSec.Lock()  ; }
    void Unlock () { m_critSec.Unlock(); }

    IDirectDrawSurface *m_pSurface;
    CCriticalSection m_critSec;
};

#ifndef NO_SOUND_SPRITE

// Sound sprite.  For now, just "static", i.e., preloaded sounds

class AFX_EXT_CLASS SoundSprite : public SpriteTree {
public:
    SoundSprite (IDirectSoundBuffer *pOrigBuffer,
        double vol0, double pan0, double freq0, 
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
