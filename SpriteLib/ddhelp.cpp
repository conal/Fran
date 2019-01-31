// More helper functionality for DirectDraw, DirectSound, and Direct3DRM.
// (This file should be called dxhelp.cpp.)

#include "GlobalVar.h"

#include "StdAfx.h"
#include "ddhelp.h"
#include "ddcheck.h"
#include "waveBuffer.h"
#include "SpriteLib.h"  // for g_screenPixelsPerLength

// Globals.  See ddhelp.h for explanation.

// Whether to print timing info
define_global(BOOL, ddhelpTimeTrace, FALSE);
#define TTRACE if (ddhelpTimeTrace) printf


IDirectDraw *g_pDDraw = NULL;
IDirectDrawSurface *g_pFront = NULL;
IDirectDrawClipper *g_pFrontClipper = NULL;
define_global(HDDSurface, g_pScratchSurf, NULL);
CCriticalSection g_frontClipperCriticalSection;

IDirectSound *g_pDSound = NULL;
IDirect3DRM  *g_pD3DRM = NULL;

EXT_API(void) DDHelpInit()
{ 
    TTRACE("DDHelpInit()\n");

    ddcheck (DirectDrawCreate(NULL,&g_pDDraw,NULL));
    // using DDSCL_NORMAL means we will run in a window, coexisting with GDI
    ddcheck (g_pDDraw->SetCooperativeLevel(0,DDSCL_NORMAL));

    // The primary surface is not a page flipping surface , because
    // we're running in a window.
    DDSURFACEDESC ddsd;
    memset( &ddsd, 0, sizeof(ddsd) );
    ddsd.dwSize = sizeof( ddsd );
    ddsd.dwFlags = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
    ddcheck (g_pDDraw->CreateSurface( &ddsd, &g_pFront, NULL ));

    // Next, create and attach the front clipper.  Its HWND will get set
    // separately for each DDrawEnv, just before starting to BLT.
    ddcheck (g_pDDraw->CreateClipper( 0, &g_pFrontClipper, NULL ));
    ddcheck (g_pFront->SetClipper( g_pFrontClipper ));

    // Make g_scratchSurfHDC.  First the tiny ddraw surface:
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT |DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    // Teeny-tiny, but ddraw forbids zero size
    ddsd.dwWidth = 1;
    ddsd.dwHeight = 1;
    ddcheck (g_pDDraw->CreateSurface(&ddsd, &g_pScratchSurf, NULL));

    // Now DSound init's
    TTRACE ("Doing DirectSoundCreate\n");
    // dscheck(DirectSoundCreate(0,&g_pDSound,0));
    // Fail gracefully
    g_pDSound = NULL;
    // Try to open the sound device
    BOOL trying = TRUE;
    while (trying) {
        if (DirectSoundCreate(0,&g_pDSound,0) == DS_OK) break;
        switch (AfxMessageBox("The sound device is busy.",
                              MB_ABORTRETRYIGNORE)) {
        case IDIGNORE:
            g_pDSound = NULL;
            trying = FALSE;
            break;
        case IDABORT:
            exit(3);
        }
    }
    if (g_pDSound) {
         // TRACE ("Did DirectSoundCreate\n");
         // Make up a invisible window, just so we can pass its handle
         // to DSound's SetCooperativeLevel.  Silly.
         HMODULE hInst = GetModuleHandle(NULL);
         HWND bogusHWnd =
             ::CreateWindow ("STATIC", // BOGUS_SOUND_CLASS,
                             "Bogus sound window",
                             0, // WS_THICKFRAME,
                             0, 0 ,0, 0,
                             NULL,NULL,hInst,NULL);
         ASSERT (bogusHWnd != NULL) ;
         dscheck(g_pDSound->SetCooperativeLevel(bogusHWnd, DSSCL_NORMAL));
    }

    // Finally, initialize D3DRM:
    d3check(Direct3DRMCreate(&g_pD3DRM));
}

EXT_API(void) DDHelpFini()
{ 
    TTRACE("DDHelpFini()\n");
    RELEASEIF (g_pDSound);
    RELEASEIF (g_pD3DRM);

    RELEASEIF (g_pScratchSurf);

    RELEASEIF (g_pFrontClipper); 
    RELEASEIF (g_pFront); 
    RELEASEIF (g_pDDraw); 
}


// Restore a ddraw surface if needed.
EXT_API(void) CheckAndRestoreDDS (IDirectDrawSurface *pSurface)
{
    if (pSurface->IsLost() == DDERR_SURFACELOST)
        ddcheck (pSurface->Restore());
}

EXT_API(HDC) GetDDrawHDC (IDirectDrawSurface *pSurface)
{
    HDC hdc;
    CheckAndRestoreDDS(pSurface);
    ddcheck (pSurface->GetDC(&hdc));
    return hdc;
}

EXT_API(void) ReleaseDDrawHDC (IDirectDrawSurface *pSurface, HDC hdc)
{ ddcheck (pSurface->ReleaseDC(hdc)); }

EXT_API(IDirectDrawSurface *)
newDDrawSurface (int width, int height, COLORREF backColor, DWORD extraCaps)
{
    DDSURFACEDESC       ddsd;
    IDirectDrawSurface *pSurface;

    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN | extraCaps;
    ddsd.dwWidth = width;
    ddsd.dwHeight = height;

    ddcheck (g_pDDraw->CreateSurface(&ddsd, &pSurface, NULL));

    DDSetColorKey( pSurface, backColor );
    return pSurface;
}

EXT_API(IDirectDrawSurface *)
newPlainDDrawSurface (int width, int height, COLORREF backColor)
{ return newDDrawSurface (width, height, backColor, 0); }

// I'm making this function return a SIZE instead of a CSize so that
// I can call it from C/Haskell.  Automatic conversions to and from
// CSize let it be convenient from C++.
EXT_API(SIZE) GetDDSurfaceSize(IDirectDrawSurface *pSurface)
{
    // Get surface size.
    DDSURFACEDESC destDesc;
    destDesc.dwSize = sizeof(destDesc);
    destDesc.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    ddcheck (pSurface->GetSurfaceDesc(&destDesc));

    return CSize (destDesc.dwWidth, destDesc.dwHeight);
}

// Clears a whole DDraw surface
EXT_API(void) clearDDSurface(IDirectDrawSurface *pSurface, COLORREF color)
{
    clearDDSurfaceRect(pSurface,
                       CRect(CPoint(0,0),GetDDSurfaceSize(pSurface)),
                       color);
}

// This one is more general, taking a rectangle
void clearDDSurfaceRect(IDirectDrawSurface *pSurface, CRect rect,
                        COLORREF color)
{
    DDBLTFX ddbltfx;

    ddbltfx.dwSize = sizeof(ddbltfx);
    ddbltfx.dwFillColor = DDColorMatch(pSurface, color);
    HRESULT ddrval;
    while (DDERR_WASSTILLDRAWING ==
            (ddrval = pSurface->Blt(rect,  NULL, NULL,
                                   DDBLT_COLORFILL, &ddbltfx)))
        ;
                                 
    ddcheck (ddrval);

}

// Load a bitmap file.  Will return NULL if the sound isn't found.
EXT_API(IDirectDrawSurface *)
newBitmapDDSurface (LPCSTR bmpName)
{
    TTRACE("bitmapDDrawSurface(%s)\n", bmpName);
    IDirectDrawSurface *pSurface = DDLoadBitmap(g_pDDraw, bmpName, 0, 0);
    // Needs much more friendly error handling.
    // ASSERT (pSurface);
    // Just return NULL if the load failed.
    if (pSurface)
        ddcheck(DDSetColorKey( pSurface, CLR_INVALID ));
    return pSurface;
}


// New .wav buffer.  Just calls WaveBuffer in WaveBuffer.c.  Will return
// NULL if the sound isn't found.
EXT_API(IDirectSoundBuffer *)
newWaveDSBuffer (char *wavName)
{
    return g_pDSound ? WaveBuffer(g_pDSound, wavName) : NULL;
}


// Just a synonym for D3DRMCreateColor.  Eliminate when I straighten out
// the float/double mess.

EXT_API(D3DCOLOR) d3dColorRGB (double r, double g, double b)
{ return D3DRMCreateColorRGB(D3DVALUE(r), D3DVALUE(g), D3DVALUE(b)); }



// New meshbuilder.  Returns NULL if not a legit mesh file
EXT_API(IDirect3DRMMeshBuilder *)
newMeshBuilder (char *fileName)
{
    IDirect3DRMMeshBuilder *builder;
    d3check(g_pD3DRM->CreateMeshBuilder(&builder));
    TTRACE("Loading mesh builder from %s.\n", fileName);
    if (FAILED (builder->Load(fileName, NULL, D3DRMLOAD_FROMFILE, 
			      NULL, NULL)))
        return NULL;
    else {
        // TRACE("Loaded mesh builder.\n");
        return builder;
    }
}
    
EXT_API(HLight) newHLight (HFrame parent, D3DRMLIGHTTYPE type)
{
    HLight light;
    TTRACE("Making new light.\n");
    d3check(g_pD3DRM->CreateLightRGB(type,D3DVALUE(1), D3DVALUE(1),
                                     D3DVALUE(1), &light));
    d3check(parent->AddLight(light));
    return light;
}

EXT_API(void) HLightSetColor (HLight light, D3DCOLOR col)
{
    // TRACE("Setting light color to %x.\n",col);
    d3check(light->SetColor(col));
    // TRACE("Set light color.\n");
}

EXT_API(HFrame) newHFrame (HFrame parent)
{
    HFrame frame;
    TTRACE("Making new frame.\n");
    d3check(g_pD3DRM->CreateFrame(parent, &frame));
    // TRACE("Made new frame.\n");
    return frame;
}

EXT_API(void) deleteFrameContents (HFrame pFrame)
{
    TTRACE("deleteFrameContents\n");
    IDirect3DRMFrameArray *pChildren;
    ddcheck (pFrame->GetChildren(&pChildren));
    int numChildren = pChildren->GetSize();
    for (int i = 0 ; i < numChildren ; i++) {
        HFrame pChild;
        ddcheck(pChildren->GetElement(i,&pChild));
        TTRACE("Deleting a child\n");
        ddcheck(pFrame->DeleteChild(pChild));
    }
    pChildren->Release();
    // Then similarly for visuals and lights.
}

EXT_API(HFrame) newScene ()
{ return newHFrame (NULL); }

EXT_API(void) HFrameAddMeshBuilder (HFrame parent, HMeshBuilder builder)
{
    TTRACE("Adding mesh builder to frame.\n");
    d3check(parent->AddVisual(builder));
    // TRACE("Added mesh builder to frame.\n");
    // Say to get the color from this frame.  Can I count on
    // inheritance??  Otherwise, I guess I should use
    // meshes and set the color (via group -1)
    d3check(parent->SetMaterialMode(D3DRMMATERIAL_FROMFRAME));
}

EXT_API(void) HFrameSetColor (HFrame frame, D3DCOLOR col)
{
    // TRACE("Setting frame color to %x.\n", col);
    d3check(frame->SetColor(col));
    // TRACE("Set frame color.\n");
}

EXT_API(void) HFrameClearTransform (HFrame frame)
{ // TRACE("Clearing frame transform.\n");
  d3check(frame->AddScale(D3DRMCOMBINE_REPLACE,
                          D3DVALUE(1),D3DVALUE(1),D3DVALUE(1))); }

// To do: check about AFTER vs BEFORE

EXT_API(void) HFrameRotate (HFrame frame,
                   double rvx, double rvy, double rvz, double theta)
{ // TRACE("Rotating frame.\n");
  d3check(frame->AddRotation(D3DRMCOMBINE_AFTER,
                             D3DVALUE(rvx), D3DVALUE(rvy), D3DVALUE(rvz),
                             D3DVALUE(theta))); }

EXT_API(void) HFrameScale (HFrame frame, double x, double y, double z)
{ // TRACE("Scaling frame.\n");
  frame->AddScale(D3DRMCOMBINE_AFTER,
                  D3DVALUE(x), D3DVALUE(y), D3DVALUE(z)); }

EXT_API(void) HFrameTranslate (HFrame frame, double x, double y, double z)
{ // TRACE("Translating frame.\n");
  frame->AddTranslation(D3DRMCOMBINE_AFTER,
                        D3DVALUE(x), D3DVALUE(y), D3DVALUE(z)); }

// Phasing out this version, which creates a D3DRM device on every render.
// It turned out that doing so is very expensive, and for some reason
// double-loads a dll each time.

// To do: make the quality settable
#ifdef use_renderGeometrySurf
EXT_API(HDDSurface)
renderGeometrySurf (HFrame sceneFrame, HFrame cameraFrame, double scale)
{
    // Allocate a surface, create a D3DRM device, make a viewport, render,
    // and clean up.  To do: optimize for reuse.
    // The ddraw surface will space from -scale to scale in X and Y.
    TTRACE("Rendering scene.\n");
    DWORD timeWas, timeIs;
    int trySize = (int) (2 * scale);
    timeWas = timeGetTime();
#define CKTIME {timeIs = timeGetTime(); TTRACE("Took %d MS.\n", timeIs-timeWas); timeWas=timeIs;}
    TTRACE("Creating %dx%d surface...  ", trySize, trySize);
    IDirectDrawSurface *surf =
        newDDrawSurface (trySize, trySize, RGB(0,0,0), DDSCAPS_3DDEVICE);
    CKTIME;

    IDirect3DRMDevice   *dev;
    TTRACE("Creating D3DRM device from surface...  ");
    d3check(g_pD3DRM->CreateDeviceFromSurface(NULL, g_pDDraw, surf,
                                              &dev));
    // Set quality to Gouraud
    d3check(dev->SetQuality(D3DRMRENDER_GOURAUD));
    CKTIME;

    // Create the viewport.  The width and height may have been slightly
    // adjusted, so get them from the device.
    int width  = dev->GetWidth(), height = dev->GetHeight();
    IDirect3DRMViewport *view;
    TTRACE("Creating viewport...  ");
    d3check(g_pD3DRM->CreateViewport(dev, cameraFrame, 0, 0,
                                     width, height, &view));
    // TTRACE("Setting back clipping plane.\n");
    d3check(view->SetBack(D3DVAL(5000.0)));     // Back clipping plane
    // To do: set quality parameters on the device
    d3check(view->Clear());
    CKTIME;

    TTRACE("Doing view->Render()...  ");
    d3check(view->Render(sceneFrame));
    CKTIME;

    // Release viewport and device.  Surface will get released when
    // replaced.
    // TTRACE("Releasing viewport and device.\n");
    view->Release();
    dev->Release();
    return surf;
}
#endif // use_renderGeometrySurf


// New rendering code that avoids making a device for each render

// Encapsulates a D3DRM device, surface, and viewport
class RMRenderer {
public:
    RMRenderer::RMRenderer (HFrame sceneFrame, HFrame cameraFrame,
                            double scale, double renderSize);
    HDDSurface Render ();
    void SetScale (double scale) {
        // To do: check and document the math here.
        d3check(m_view->SetField(D3DVAL(m_renderSize * 0.25 /(scale + 1e-10)))); }
    // To do: make this destructor get called somewhere!
    ~RMRenderer() {
        // Release what the constructor created
        RELEASEIF(m_view); RELEASEIF(m_dev); RELEASEIF(m_surf); }

private:
    int m_width, m_height;
    double m_renderSize;
    HFrame m_sceneFrame;
    IDirectDrawSurface *m_surf;
    IDirect3DRMDevice *m_dev;
    IDirect3DRMViewport *m_view;
};

// Helpful for timing.  Declare the following before use.
//    DWORD timeWas, timeIs;

#define CKTIME {timeIs = timeGetTime(); TTRACE("Took %d MS.\n", timeIs-timeWas); timeWas=timeIs;}

RMRenderer::RMRenderer (HFrame sceneFrame, HFrame cameraFrame,
                        double scale, double renderSize)
    : m_sceneFrame(sceneFrame)
{
    // The ddraw surface will space from -scale to scale in X and Y.
    TTRACE("Makeing Renderer.\n");
    //DWORD timeWas=timeGetTime(), timeIs;
	extern double g_screenPixelsPerLength;
    int trySize = (int) (renderSize * scale * g_screenPixelsPerLength);
    TTRACE("Creating %dx%d surface...  ", trySize, trySize);
    m_surf =
        newDDrawSurface (trySize, trySize, RGB(0,0,0), DDSCAPS_3DDEVICE);
    //CKTIME;

    //TTRACE("Creating D3DRM device from surface...  ");
    d3check(g_pD3DRM->CreateDeviceFromSurface(NULL, g_pDDraw, m_surf,
                                              &m_dev));
    // Set quality to Gouraud
    d3check(m_dev->SetQuality(D3DRMRENDER_GOURAUD));
    //CKTIME;

    // Create the viewport.  The width and height may have been slightly
    // adjusted, so get them from the device.
    m_width  = m_dev->GetWidth();
    m_height = m_dev->GetHeight();
    m_renderSize = renderSize;
    //TTRACE("Creating viewport...  ");
    d3check(g_pD3DRM->CreateViewport(m_dev, cameraFrame, 0, 0,
                                     m_width, m_height, &m_view));
    // TTRACE("Setting back clipping plane.\n");
    d3check(m_view->SetBack(D3DVAL(5000.0)));     // Back clipping plane
    // To do: set quality parameters on the device
}

EXT_API(HRMRenderer) newRMRenderer(HFrame sceneFrame, HFrame cameraFrame,
                                   double scale, double renderSize)
{ return new RMRenderer(sceneFrame, cameraFrame, scale, renderSize); }

EXT_API(void) 
hRendererSetScale(HRMRenderer renderer, double scale)
{ renderer->SetScale(scale); }

EXT_API(HDDSurface) 
doRMRenderer(HRMRenderer renderer)
{ return renderer->Render(); }

HDDSurface
RMRenderer::Render ()
{
    // The ddraw surface will space from -scale to scale in X and Y.
    //TTRACE("Clearing and rendering scene.\n");
    //DWORD timeWas=timeGetTime(), timeIs;

    //TTRACE("Doing view->Clear()...  ");
    d3check(m_view->Clear());
    //CKTIME;

    //TTRACE("Doing view->Render()...  ");
    d3check(m_view->Render(m_sceneFrame));
    //CKTIME;

    //TTRACE("Making new surface and copying to it...  ");
    // Black background ???
    HDDSurface newSurf = newPlainDDrawSurface(m_width, m_height, RGB(0,0,0));
    d3check(newSurf->BltFast(0,0, m_surf, CRect(0,0, m_width, m_height),
                             DDBLTFAST_WAIT | DDBLTFAST_NOCOLORKEY));
    //CKTIME;

    // Release viewport and device.  Surface will get released when
    // replaced.
    //TTRACE("");
    return newSurf;
}

