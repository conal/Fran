// More helper functionality for DDraw.  Well, really for 
// DirectX, since we're now using DirectSound as well.  Should rename.


#ifndef _DDHELP_H
#define _DDHELP_H

#include "cdecls.h"

// C/Haskell interface

// Whether to print timing info
EXT_DECL_DATA BOOL ddhelpTimeTrace;

EXT_STRUCT(IDirectDrawSurface);
typedef HIDirectDrawSurface HDDSurface;

EXT_STRUCT(IDirectSoundBuffer);
typedef HIDirectSoundBuffer HDSBuffer;


EXT_STRUCT(IDirect3DRMMeshBuilder);
typedef HIDirect3DRMMeshBuilder HMeshBuilder;

EXT_STRUCT(IDirect3DRMLight);
typedef HIDirect3DRMLight HLight;

EXT_STRUCT(IDirect3DRMFrame);
typedef HIDirect3DRMFrame HFrame;

// We need a small off-screen surface, simply in order to do some
// measurements.  For instance, before making a TextSurface, we need to
// know how large the text's image will be, which requires having a DC.
EXT_DECL_DATA HDDSurface g_pScratchSurf;

EXT_API HDC GetDDrawHDC (HDDSurface);
EXT_API void ReleaseDDrawHDC (HDDSurface, HDC);

// Clear a surface to a given color
EXT_API void clearDDSurface(HDDSurface, COLORREF);

// Mutable surface
EXT_API HDDSurface
 newPlainDDrawSurface (int width, int height, COLORREF backColor);

// Immutable bitmap surface.  Returns NULL if not a legit bitmap file.
EXT_API HDDSurface newBitmapDDSurface (LPCSTR bmpName);

// For testing
EXT_API HDDSurface textDDSurface (LPCSTR string, COLORREF color);

EXT_API SIZE GetDDSurfaceSize(HDDSurface);

// New .wav buffer
EXT_API HDSBuffer newWaveDSBuffer (char *wavName);


// Just a synonym for D3DRMCreateColor.  Eliminate when I straighten out
// the float/double mess.

EXT_API D3DCOLOR CreateColorRGB (double r, double g, double b);

// New meshbuilder, loaded from X file
EXT_API HMeshBuilder newMeshBuilder (char *fileName);

// New light of a given type
EXT_API HLight newHLight (HFrame parent, D3DRMLIGHTTYPE);
EXT_API void HLightSetColor (HLight light, D3DCOLOR);


// New empty frame with a given parent
EXT_API HFrame newHFrame (HFrame parent);

// New scene
EXT_API HFrame newScene ();

EXT_API void HFrameAddMeshBuilder (HFrame parent, HMeshBuilder builder);

EXT_API void HFrameSetColor (HFrame frame, D3DCOLOR);

EXT_API void HFrameClearTransform (HFrame frame);
EXT_API void 
  HFrameRotate (HFrame frame, double x, double y, double z, double theta);
EXT_API void HFrameScale (HFrame frame, double x, double y, double z);
EXT_API void HFrameTranslate (HFrame frame, double x, double y, double z);


// Render a geometry to produce a new DDraw surface
EXT_API HDDSurface 
renderGeometrySurf (HFrame sceneFrame, HFrame cameraFrame, double scale);

// A "renderer" of a 3D scene.  Current serious limitation: can't change
// the scale after creation.  To do: find a way to relax this restriction
// with tolerable efficiency.
EXT_CLASS(RMRenderer);

EXT_API HRMRenderer
newRMRenderer(HFrame sceneFrame, HFrame cameraFrame, double scale);

EXT_API HDDSurface doRMRenderer(HRMRenderer renderer);

// To do: deleters for lots of these types.


#ifdef __cplusplus

// Called by DllMain to create or release the ddraw object
EXT_API void DDHelpInit();
void DDHelpFini();

// Globally available DirectDraw object.  Allows functions like 
// newBitmapSurface to have purely functional interfaces.
extern IDirectDraw *g_pDDraw;

// Primary (front) surface and clipper.  Needs to be shared among
// the blitting in a single process, in order to allow
// blitting from a single source onto multiple DDraw back surfaces.
extern IDirectDrawSurface *g_pFront;
extern IDirectDrawClipper *g_pFrontClipper;

// We must also protect threads from changing the front clipper
// at inopportune moments.
extern CCriticalSection g_frontClipperCriticalSection;

// Global DSound object.
extern IDirectSound *g_pDSound;

// Global D3D$M object
extern IDirect3DRM *g_pD3DRM;

#endif // __cplusplus

#endif // _DDHELP_H
