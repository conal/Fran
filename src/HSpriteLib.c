#include "HUGS.h"
#include <windows.h>
typedef char  CHAR;
static Void XSpack_String(ppArray)
CHAR *ppArray;
{
    int ppSize;
    for (ppSize = 0; !(('\0' == ppArray[ppSize])); ppSize++) {}
    hugsNIL;
    while (--ppSize >= 0) {
        hugsCHAR(ppArray[ppSize]);;;
        hugsCONS;
        hugsAP;
        hugsAP;
    }
}
static Void XSpack_PString(ppArray)
CHAR *ppArray;
{
    int ppSize;
    for (ppSize = 0; !(('\0' == ppArray[ppSize])); ppSize++) {}
    hugsNIL;
    while (--ppSize >= 0) {
        hugsCHAR(ppArray[ppSize]);;;
        hugsCONS;
        hugsAP;
        hugsAP;
    }
}
typedef void* Ptr;

static Void local mkPtr Args((Ptr));
static Void local mkPtr(n)
Ptr n; {
    hugs_push(hugs->makeInt((Int) n));
}

#define hugs_whnfPtr ((void*) hugs_whnfInt)
typedef Int Word8;
typedef Int Word16;
typedef unsigned long Word32;
typedef unsigned int Word;
typedef Int Int8;
typedef Int Int16;
typedef Int Int32;
typedef Int Int64;
typedef Int Word64;
static Cell hugsRunIO Args((Cell));
static Cell hugsRunIO(x)
Cell x; {
    StackPtr old_sp   = hugs_sp;
    Cell     result   = NIL;
    Cell     temp     = NIL;
    hugs_push(hugs->nameIORun);
    hugs_push(x);
    hugsRAP;
    temp = hugs->evalWithNoError(hugs_pop());
    hugs_assert(isNull(temp));
    if (hugs_whnfHead == hugs->nameRight) {
	result = hugs_pop();
    } else { /* Called "exit" */
	hugs->evalFails(old_sp);
    }
    hugs_assert(hugs_sp == old_sp);
    return result;
}

static Int hugsEvalInt Args((Cell));
static Int hugsEvalInt(x)
Cell x; {
    StackPtr old_sp = hugs_sp;
    Cell     temp = hugs->evalWithNoError(x);
    hugs_assert(isNull(temp));
#if 0
    hugs_assert(isInt(whnfHead)); /* Foo - gets confused by CAFs */
#endif
    hugs_assert(hugs_sp == old_sp);
    return hugs_whnfInt;
}

static Bool hugsEvalBool Args((Cell));
static Bool hugsEvalBool(x)
Cell x; {
    StackPtr old_sp = hugs_sp;
    Cell     temp = hugs->evalWithNoError(x);
    hugs_assert(isNull(temp));
    hugs_assert(hugs_whnfHead == hugs->nameTrue
                || hugs_whnfHead == hugs->nameFalse
                );
    hugs_assert(hugs_sp == old_sp);
    return hugs_whnfInt;
}
typedef String MbString;
typedef INT MbINT;
typedef FLOAT MbFLOAT;
typedef LPVOID MbLPVOID;
typedef DWORD MbDWORD;
static Void XSpack_ListINT(ppArray, ppSize)
INT *ppArray;
Int       ppSize;
{
    hugsNIL;
    while (--ppSize >= 0) {
        hugsINT(ppArray[ppSize]);;
        hugsCONS;
        hugsAP;
        hugsAP;
    }
}
static Void XSpack_ListFLOAT(ppArray, ppSize)
FLOAT *ppArray;
Int       ppSize;
{
    hugsNIL;
    while (--ppSize >= 0) {
        hugsFLOAT(ppArray[ppSize]);;
        hugsCONS;
        hugsAP;
        hugsAP;
    }
}
typedef void near* NearPtr;
typedef void far*  FarPtr;
typedef HDC MbHDC;
typedef HWND MbHWND;
typedef HMENU MbHMENU;
typedef HINSTANCE MbHINSTANCE;
typedef HMODULE MbHMODULE;
typedef HFONT MbHFONT;
typedef HPEN MbHPEN;
typedef HPALETTE MbHPALETTE;
typedef HICON MbHICON;
typedef HCURSOR MbHCURSOR;
typedef HBRUSH MbHBRUSH;
typedef HBITMAP MbHBITMAP;
typedef HRGN MbHRGN;
#include "SpriteLib.h"

PROTO_PRIM(XS_OpenSpriteLib);
primFun(XS_OpenSpriteLib) {
    {
    {
    	OpenSpriteLib();
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_CloseSpriteLib);
primFun(XS_CloseSpriteLib) {
    {
    {
    	CloseSpriteLib();
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_SetTimerResolutionMS);
primFun(XS_SetTimerResolutionMS) {
    {
    	Int 	pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	SetTimerResolutionMS(pparg0);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
#include "VBlankHandler.h"

PROTO_PRIM(XSget_vblankPeriodMS);
primFun(XSget_vblankPeriodMS) {
    {
        hugsINT(vblankPeriodMS); hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSset_vblankPeriodMS);
primFun(XSset_vblankPeriodMS) {
    {
        hugs->eval(hugs_primArg(3)); vblankPeriodMS=hugs_whnfInt;
        hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}
typedef Int ThreadPriority;

PROTO_PRIM(XS_SetVblankThreadPriority);
primFun(XS_SetVblankThreadPriority) {
    {
    	Int 	pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	SetVblankThreadPriority(pparg0);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
#include "Behavior.h"

PROTO_PRIM(XS_CurrentSpriteTime);
primFun(XS_CurrentSpriteTime) {
    {
    {
    	Double ppret0;
    	ppret0 = CurrentSpriteTime();
    	hugsFLOAT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XSget_behaviorMakeContinuous);
primFun(XSget_behaviorMakeContinuous) {
    {
        if (behaviorMakeContinuous) {hugsTRUE;} else {hugsFALSE;} hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSset_behaviorMakeContinuous);
primFun(XSset_behaviorMakeContinuous) {
    {
        hugs->eval(hugs_primArg(3)); behaviorMakeContinuous=(hugs_whnfHead==hugs->nameTrue);
        hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSget_behaviorSamplePastGoal);
primFun(XSget_behaviorSamplePastGoal) {
    {
        if (behaviorSamplePastGoal) {hugsTRUE;} else {hugsFALSE;} hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSset_behaviorSamplePastGoal);
primFun(XSset_behaviorSamplePastGoal) {
    {
        hugs->eval(hugs_primArg(3)); behaviorSamplePastGoal=(hugs_whnfHead==hugs->nameTrue);
        hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}
#include "d3drm.h"  // for D3DRMLIGHTTYPE, and D3DCOLOR
#include "ddhelp.h"

PROTO_PRIM(XSget_ddhelpTimeTrace);
primFun(XSget_ddhelpTimeTrace) {
    {
        if (ddhelpTimeTrace) {hugsTRUE;} else {hugsFALSE;} hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSset_ddhelpTimeTrace);
primFun(XSset_ddhelpTimeTrace) {
    {
        hugs->eval(hugs_primArg(3)); ddhelpTimeTrace=(hugs_whnfHead==hugs->nameTrue);
        hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}
Name XSname_NullHDDSurface;

PROTO_PRIM(XStest_isNullHDDSurface);
primFun(XStest_isNullHDDSurface) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHDDSurface) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}
Name XSname_NullHDSBuffer;

PROTO_PRIM(XStest_isNullHDSBuffer);
primFun(XStest_isNullHDSBuffer) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHDSBuffer) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}
Name XSname_NullHMeshBuilder;

PROTO_PRIM(XStest_isNullHMeshBuilder);
primFun(XStest_isNullHMeshBuilder) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHMeshBuilder) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XSget_g_pScratchSurf);
primFun(XSget_g_pScratchSurf) {
    {
        hugsINT(g_pScratchSurf); hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSset_g_pScratchSurf);
primFun(XSset_g_pScratchSurf) {
    {
        hugs->eval(hugs_primArg(3)); g_pScratchSurf=hugs_whnfInt;
        hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XS_GetDDrawHDC);
primFun(XS_GetDDrawHDC) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	NearPtr ppret0;
    	ppret0 = GetDDrawHDC(pparg0);
    	mkPtr((Ptr)ppret0);; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_ReleaseDDrawHDC);
primFun(XS_ReleaseDDrawHDC) {
    {
    	Word32 pparg0;
    	NearPtr pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=(Ptr)hugs_whnfPtr;
    {
    	ReleaseDDrawHDC(pparg0,pparg1);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_clearDDSurface);
primFun(XS_clearDDSurface) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	clearDDSurface(pparg0,pparg1);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_newPlainDDrawSurface);
primFun(XS_newPlainDDrawSurface) {
    {
    	Int 	pparg0;
    	Int 	pparg1;
    	Word32 pparg2;
    	hugs->eval(hugs_primArg(5)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(4)); pparg1=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg2=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newPlainDDrawSurface(pparg0,pparg1,pparg2);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_GetDDSurfaceSize);
primFun(XS_GetDDSurfaceSize) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	SIZE ppret0;
    	ppret0 = GetDDSurfaceSize(pparg0);
    	hugsINT((ppret0).cx); hugsTUPLE(2); hugsAP; hugsINT((ppret0).cy); hugsRUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_newBitmapDDSurface);
primFun(XS_newBitmapDDSurface) {
    {
    	int pparg0_size; char* pparg0;
    		{  int ppindex; 
	    hugs->eval(hugs_primArg(3));
	    for (ppindex = 0; hugs_whnfHead == hugs->nameCons; ++ppindex) {
		Cell ppHead = hugs_pop(); 
		Cell ppTail = hugs_pop(); 
		hugs_push(ppHead);
	        hugs->eval(ppTail);
	    }
           pparg0_size = ppindex; pparg0 = (char *) malloc((ppindex+1) * sizeof(char)); if (!pparg0) { hugs_cantReduce(); } pparg0[ppindex] = '\0';
	    while (--ppindex >= 0) {
               hugs->eval(hugs_pop()); pparg0[ppindex]=hugs->CharOf(hugs_whnfHead);
	    }
	};
    {
    	Word32 ppret0;
    	ppret0 = newBitmapDDSurface(pparg0);
    	if (!(ppret0)) {hugs_push(XSname_NullHDDSurface);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_newWaveDSBuffer);
primFun(XS_newWaveDSBuffer) {
    {
    	int pparg0_size; char* pparg0;
    		{  int ppindex; 
	    hugs->eval(hugs_primArg(3));
	    for (ppindex = 0; hugs_whnfHead == hugs->nameCons; ++ppindex) {
		Cell ppHead = hugs_pop(); 
		Cell ppTail = hugs_pop(); 
		hugs_push(ppHead);
	        hugs->eval(ppTail);
	    }
           pparg0_size = ppindex; pparg0 = (char *) malloc((ppindex+1) * sizeof(char)); if (!pparg0) { hugs_cantReduce(); } pparg0[ppindex] = '\0';
	    while (--ppindex >= 0) {
               hugs->eval(hugs_pop()); pparg0[ppindex]=hugs->CharOf(hugs_whnfHead);
	    }
	};
    {
    	Word32 ppret0;
    	ppret0 = newWaveDSBuffer(pparg0);
    	if (!(ppret0)) {hugs_push(XSname_NullHDSBuffer);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_newMeshBuilder);
primFun(XS_newMeshBuilder) {
    {
    	int pparg0_size; char* pparg0;
    		{  int ppindex; 
	    hugs->eval(hugs_primArg(3));
	    for (ppindex = 0; hugs_whnfHead == hugs->nameCons; ++ppindex) {
		Cell ppHead = hugs_pop(); 
		Cell ppTail = hugs_pop(); 
		hugs_push(ppHead);
	        hugs->eval(ppTail);
	    }
           pparg0_size = ppindex; pparg0 = (char *) malloc((ppindex+1) * sizeof(char)); if (!pparg0) { hugs_cantReduce(); } pparg0[ppindex] = '\0';
	    while (--ppindex >= 0) {
               hugs->eval(hugs_pop()); pparg0[ppindex]=hugs->CharOf(hugs_whnfHead);
	    }
	};
    {
    	Word32 ppret0;
    	ppret0 = newMeshBuilder(pparg0);
    	if (!(ppret0)) {hugs_push(XSname_NullHMeshBuilder);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
typedef DWORD D3DColor;

PROTO_PRIM(XS_CreateColorRGB);
primFun(XS_CreateColorRGB) {
    {
    	Double pparg0;
    	Double pparg1;
    	Double pparg2;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(2)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(1)); pparg2=hugs_whnfFloat;
    {
    	Word32 ppret0;
    	ppret0 = CreateColorRGB(pparg0,pparg1,pparg2);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}
typedef Int LightType;

PROTO_PRIM(XS_newHLight);
primFun(XS_newHLight) {
    {
    	Word32 pparg0;
    	Int 	pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newHLight(pparg0,pparg1);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HLightSetColor);
primFun(XS_HLightSetColor) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	HLightSetColor(pparg0,pparg1);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_newHFrame);
primFun(XS_newHFrame) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newHFrame(pparg0);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_newScene);
primFun(XS_newScene) {
    {
    {
    	Word32 ppret0;
    	ppret0 = newScene();
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HFrameAddMeshBuilder);
primFun(XS_HFrameAddMeshBuilder) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	HFrameAddMeshBuilder(pparg0,pparg1);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HFrameSetColor);
primFun(XS_HFrameSetColor) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	HFrameSetColor(pparg0,pparg1);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HFrameClearTransform);
primFun(XS_HFrameClearTransform) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	HFrameClearTransform(pparg0);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HFrameRotate);
primFun(XS_HFrameRotate) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	Double pparg4;
    	hugs->eval(hugs_primArg(7)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(6)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(5)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg3=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg4=hugs_whnfFloat;
    {
    	HFrameRotate(pparg0,pparg1,pparg2,pparg3,pparg4);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HFrameScale);
primFun(XS_HFrameScale) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	hugs->eval(hugs_primArg(6)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(5)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg3=hugs_whnfFloat;
    {
    	HFrameScale(pparg0,pparg1,pparg2,pparg3);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_HFrameTranslate);
primFun(XS_HFrameTranslate) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	hugs->eval(hugs_primArg(6)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(5)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg3=hugs_whnfFloat;
    {
    	HFrameTranslate(pparg0,pparg1,pparg2,pparg3);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_renderGeometrySurf);
primFun(XS_renderGeometrySurf) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	Double pparg2;
    	hugs->eval(hugs_primArg(5)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(4)); pparg1=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg2=hugs_whnfFloat;
    {
    	Word32 ppret0;
    	ppret0 = renderGeometrySurf(pparg0,pparg1,pparg2);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
Name XSname_NullHRMRenderer;

PROTO_PRIM(XStest_isNullHRMRenderer);
primFun(XStest_isNullHRMRenderer) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHRMRenderer) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_newRMRenderer);
primFun(XS_newRMRenderer) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	Double pparg2;
    	hugs->eval(hugs_primArg(5)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(4)); pparg1=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg2=hugs_whnfFloat;
    {
    	Word32 ppret0;
    	ppret0 = newRMRenderer(pparg0,pparg1,pparg2);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_doRMRenderer);
primFun(XS_doRMRenderer) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = doRMRenderer(pparg0);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
#include "Sprite.h"
Name XSname_NullHFlipBook;

PROTO_PRIM(XStest_isNullHFlipBook);
primFun(XStest_isNullHFlipBook) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHFlipBook) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}
typedef LONG Pixels;

PROTO_PRIM(XS_newFlipBook);
primFun(XS_newFlipBook) {
    {
    	Word32 pparg0;
    	Int 	pparg1;
    	Int 	pparg2;
    	Int 	pparg3;
    	Int 	pparg4;
    	Int 	pparg5;
    	Int 	pparg6;
    	hugs->eval(hugs_primArg(9)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(8)); pparg1=hugs_whnfInt;
    	hugs->eval(hugs_primArg(7)); pparg2=hugs_whnfInt;
    	hugs->eval(hugs_primArg(6)); pparg3=hugs_whnfInt;
    	hugs->eval(hugs_primArg(5)); pparg4=hugs_whnfInt;
    	hugs->eval(hugs_primArg(4)); pparg5=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg6=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newFlipBook(pparg0,pparg1,pparg2,pparg3,pparg4,pparg5,pparg6);
    	if (!(ppret0)) {hugs_push(XSname_NullHFlipBook);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_flipBookWidth);
primFun(XS_flipBookWidth) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Int 	ppret0;
    	ppret0 = flipBookWidth(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_flipBookHeight);
primFun(XS_flipBookHeight) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Int 	ppret0;
    	ppret0 = flipBookHeight(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_flipBookPages);
primFun(XS_flipBookPages) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Int 	ppret0;
    	ppret0 = flipBookPages(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_deleteFlipBook);
primFun(XS_deleteFlipBook) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	deleteFlipBook(pparg0);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
typedef HSpriteTree SpriteTreeChain;
Name XSname_NullHSpriteTree;

PROTO_PRIM(XStest_isNullHSpriteTree);
primFun(XStest_isNullHSpriteTree) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHSpriteTree) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_deleteSpriteTree);
primFun(XS_deleteSpriteTree) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	deleteSpriteTree(pparg0);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_setGoalPosition);
primFun(XS_setGoalPosition) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	hugs->eval(hugs_primArg(6)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(5)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg3=hugs_whnfFloat;
    {
    	setGoalPosition(pparg0,pparg1,pparg2,pparg3);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_setGoalScale);
primFun(XS_setGoalScale) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	hugs->eval(hugs_primArg(6)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(5)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg3=hugs_whnfFloat;
    {
    	setGoalScale(pparg0,pparg1,pparg2,pparg3);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_spriteToSpriteTree);
primFun(XS_spriteToSpriteTree) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = spriteToSpriteTree(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}
Name XSname_NullHFlipSprite;

PROTO_PRIM(XStest_isNullHFlipSprite);
primFun(XStest_isNullHFlipSprite) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHFlipSprite) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_newFlipSprite);
primFun(XS_newFlipSprite) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	Double pparg4;
    	Double pparg5;
    	Word32 pparg6;
    	hugs->eval(hugs_primArg(9)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(8)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(7)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(6)); pparg3=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(5)); pparg4=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg5=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg6=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newFlipSprite(pparg0,pparg1,pparg2,pparg3,pparg4,pparg5,pparg6);
    	if (!(ppret0)) {hugs_push(XSname_NullHFlipSprite);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_flipSpriteToSprite);
primFun(XS_flipSpriteToSprite) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = flipSpriteToSprite(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_setGoalPage);
primFun(XS_setGoalPage) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	hugs->eval(hugs_primArg(5)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(4)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg2=hugs_whnfFloat;
    {
    	setGoalPage(pparg0,pparg1,pparg2);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
Name XSname_NullHSimpleSprite;

PROTO_PRIM(XStest_isNullHSimpleSprite);
primFun(XStest_isNullHSimpleSprite) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHSimpleSprite) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_newSimpleSprite);
primFun(XS_newSimpleSprite) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	Double pparg4;
    	Word32 pparg5;
    	hugs->eval(hugs_primArg(8)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(7)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(6)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(5)); pparg3=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg4=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg5=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newSimpleSprite(pparg0,pparg1,pparg2,pparg3,pparg4,pparg5);
    	if (!(ppret0)) {hugs_push(XSname_NullHSimpleSprite);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_simpleSpriteToSprite);
primFun(XS_simpleSpriteToSprite) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = simpleSpriteToSprite(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_setSurface);
primFun(XS_setSurface) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	setSurface(pparg0,pparg1);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XSget_MinSpriteSize);
primFun(XSget_MinSpriteSize) {
    {
        hugsINT(MinSpriteSize); hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}

PROTO_PRIM(XSset_MinSpriteSize);
primFun(XSset_MinSpriteSize) {
    {
        hugs->eval(hugs_primArg(3)); MinSpriteSize=hugs_whnfInt;
        hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }
    
    {
    }
}
Name XSname_NullHSoundSprite;

PROTO_PRIM(XStest_isNullHSoundSprite);
primFun(XStest_isNullHSoundSprite) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHSoundSprite) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_newSoundSprite);
primFun(XS_newSoundSprite) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	Word32 pparg4;
    	hugs->eval(hugs_primArg(7)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(6)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(5)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg3=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg4=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newSoundSprite(pparg0,pparg1,pparg2,pparg3,pparg4);
    	if (!(ppret0)) {hugs_push(XSname_NullHSoundSprite);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_soundSpriteToSpriteTree);
primFun(XS_soundSpriteToSpriteTree) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = soundSpriteToSpriteTree(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_updateSoundSprite);
primFun(XS_updateSoundSprite) {
    {
    	Word32 pparg0;
    	Double pparg1;
    	Double pparg2;
    	Double pparg3;
    	Double pparg4;
    	hugs->eval(hugs_primArg(7)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(6)); pparg1=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(5)); pparg2=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(4)); pparg3=hugs_whnfFloat;
    	hugs->eval(hugs_primArg(3)); pparg4=hugs_whnfFloat;
    {
    	updateSoundSprite(pparg0,pparg1,pparg2,pparg3,pparg4);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
Name XSname_NullHSpriteGroup;

PROTO_PRIM(XStest_isNullHSpriteGroup);
primFun(XStest_isNullHSpriteGroup) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHSpriteGroup) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_newSpriteGroup);
primFun(XS_newSpriteGroup) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newSpriteGroup(pparg0,pparg1);
    	if (!(ppret0)) {hugs_push(XSname_NullHSpriteGroup);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_spriteGroupToSpriteTree);
primFun(XS_spriteGroupToSpriteTree) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(1)); pparg0=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = spriteGroupToSpriteTree(pparg0);
    	hugsINT(ppret0); hugsUPDATEROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_ResetSpriteGroup);
primFun(XS_ResetSpriteGroup) {
    {
    	Word32 pparg0;
    	Word32 pparg1;
    	Bool 	pparg2;
    	hugs->eval(hugs_primArg(5)); pparg0=hugs_whnfInt;
    	hugs->eval(hugs_primArg(4)); pparg1=hugs_whnfInt;
    	hugs->eval(hugs_primArg(3)); pparg2=(hugs_whnfHead==hugs->nameTrue);
    {
    	ResetSpriteGroup(pparg0,pparg1,pparg2);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}
#include "SpriteEngine.h"
Name XSname_NullHSpriteEngine;

PROTO_PRIM(XStest_isNullHSpriteEngine);
primFun(XStest_isNullHSpriteEngine) {
        hugs->eval(hugs_primArg(1));
    if (hugs_whnfHead==XSname_NullHSpriteEngine) {
	if (1) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;;
    } else {
	if (0) {hugsTRUE;} else {hugsFALSE;} hugsUPDATEROOT;; return;
    }
    {
    }
}

PROTO_PRIM(XS_newSpriteEngine);
primFun(XS_newSpriteEngine) {
    {
    	NearPtr pparg0;
    	Word32 pparg1;
    	hugs->eval(hugs_primArg(4)); pparg0=(Ptr)hugs_whnfPtr;
    	hugs->eval(hugs_primArg(3)); pparg1=hugs_whnfInt;
    {
    	Word32 ppret0;
    	ppret0 = newSpriteEngine(pparg0,pparg1);
    	if (!(ppret0)) {hugs_push(XSname_NullHSpriteEngine);; hugsFAIL; hugsUPDAPROOT;; return;} hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_onResizeSpriteEngine);
primFun(XS_onResizeSpriteEngine) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	onResizeSpriteEngine(pparg0);
    	hugsUNIT; hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XS_deleteSpriteEngine);
primFun(XS_deleteSpriteEngine) {
    {
    	Word32 pparg0;
    	hugs->eval(hugs_primArg(3)); pparg0=hugs_whnfInt;
    {
    	Int 	ppret0;
    	ppret0 = deleteSpriteEngine(pparg0);
    	hugsINT(ppret0); hugsSUCC; hugsUPDAPROOT;; return;
    }}
    
    {
    }
}

PROTO_PRIM(XSconsts_SpriteTreeChain);
primFun(XSconsts_SpriteTreeChain) {
        static SpriteTreeChain consts[] = {
    	0
    	};
        int i;
        hugs->eval(hugs_primArg(1)); i=hugs_whnfInt;
        hugsINT(consts[i]); hugsUPDATEROOT;; return;
    
    {
    }
}

PROTO_PRIM(XSconsts_LightType);
primFun(XSconsts_LightType) {
        static LightType consts[] = {
    	D3DRMLIGHT_AMBIENT,
    	D3DRMLIGHT_POINT,
    	D3DRMLIGHT_SPOT,
    	D3DRMLIGHT_DIRECTIONAL,
    	D3DRMLIGHT_PARALLELPOINT
    	};
        int i;
        hugs->eval(hugs_primArg(1)); i=hugs_whnfInt;
        hugsINT(consts[i]); hugsUPDATEROOT;; return;
    
    {
    }
}

PROTO_PRIM(XSconsts_ThreadPriority);
primFun(XSconsts_ThreadPriority) {
        static ThreadPriority consts[] = {
    	THREAD_PRIORITY_IDLE,
    	THREAD_PRIORITY_LOWEST,
    	THREAD_PRIORITY_BELOW_NORMAL,
    	THREAD_PRIORITY_NORMAL,
    	THREAD_PRIORITY_ABOVE_NORMAL,
    	THREAD_PRIORITY_HIGHEST,
    	THREAD_PRIORITY_TIME_CRITICAL
    	};
        int i;
        hugs->eval(hugs_primArg(1)); i=hugs_whnfInt;
        hugsINT(consts[i]); hugsUPDATEROOT;; return;
    
    {
    }
}

struct primitive primTable[] = {
	    {"XS_OpenSpriteLib", 2, XS_OpenSpriteLib},
	    {"XS_CloseSpriteLib", 2, XS_CloseSpriteLib},
	    {"XS_SetTimerResolutionMS", 3, XS_SetTimerResolutionMS},
	    {"XSget_vblankPeriodMS", 2, XSget_vblankPeriodMS},
	    {"XSset_vblankPeriodMS", 3, XSset_vblankPeriodMS},
	    {"XS_SetVblankThreadPriority", 3, XS_SetVblankThreadPriority},
	    {"XS_CurrentSpriteTime", 2, XS_CurrentSpriteTime},
	    {"XSget_behaviorMakeContinuous", 2, XSget_behaviorMakeContinuous},
	    {"XSset_behaviorMakeContinuous", 3, XSset_behaviorMakeContinuous},
	    {"XSget_behaviorSamplePastGoal", 2, XSget_behaviorSamplePastGoal},
	    {"XSset_behaviorSamplePastGoal", 3, XSset_behaviorSamplePastGoal},
	    {"XSget_ddhelpTimeTrace", 2, XSget_ddhelpTimeTrace},
	    {"XSset_ddhelpTimeTrace", 3, XSset_ddhelpTimeTrace},
	    {"XStest_isNullHDDSurface", 1, XStest_isNullHDDSurface},
	    {"XStest_isNullHDSBuffer", 1, XStest_isNullHDSBuffer},
	    {"XStest_isNullHMeshBuilder", 1, XStest_isNullHMeshBuilder},
	    {"XSget_g_pScratchSurf", 2, XSget_g_pScratchSurf},
	    {"XSset_g_pScratchSurf", 3, XSset_g_pScratchSurf},
	    {"XS_GetDDrawHDC", 3, XS_GetDDrawHDC},
	    {"XS_ReleaseDDrawHDC", 4, XS_ReleaseDDrawHDC},
	    {"XS_clearDDSurface", 4, XS_clearDDSurface},
	    {"XS_newPlainDDrawSurface", 5, XS_newPlainDDrawSurface},
	    {"XS_GetDDSurfaceSize", 1, XS_GetDDSurfaceSize},
	    {"XS_newBitmapDDSurface", 3, XS_newBitmapDDSurface},
	    {"XS_newWaveDSBuffer", 3, XS_newWaveDSBuffer},
	    {"XS_newMeshBuilder", 3, XS_newMeshBuilder},
	    {"XS_CreateColorRGB", 3, XS_CreateColorRGB},
	    {"XS_newHLight", 4, XS_newHLight},
	    {"XS_HLightSetColor", 4, XS_HLightSetColor},
	    {"XS_newHFrame", 3, XS_newHFrame},
	    {"XS_newScene", 2, XS_newScene},
	    {"XS_HFrameAddMeshBuilder", 4, XS_HFrameAddMeshBuilder},
	    {"XS_HFrameSetColor", 4, XS_HFrameSetColor},
	    {"XS_HFrameClearTransform", 3, XS_HFrameClearTransform},
	    {"XS_HFrameRotate", 7, XS_HFrameRotate},
	    {"XS_HFrameScale", 6, XS_HFrameScale},
	    {"XS_HFrameTranslate", 6, XS_HFrameTranslate},
	    {"XS_renderGeometrySurf", 5, XS_renderGeometrySurf},
	    {"XStest_isNullHRMRenderer", 1, XStest_isNullHRMRenderer},
	    {"XS_newRMRenderer", 5, XS_newRMRenderer},
	    {"XS_doRMRenderer", 3, XS_doRMRenderer},
	    {"XStest_isNullHFlipBook", 1, XStest_isNullHFlipBook},
	    {"XS_newFlipBook", 9, XS_newFlipBook},
	    {"XS_flipBookWidth", 1, XS_flipBookWidth},
	    {"XS_flipBookHeight", 1, XS_flipBookHeight},
	    {"XS_flipBookPages", 1, XS_flipBookPages},
	    {"XS_deleteFlipBook", 3, XS_deleteFlipBook},
	    {"XStest_isNullHSpriteTree", 1, XStest_isNullHSpriteTree},
	    {"XS_deleteSpriteTree", 3, XS_deleteSpriteTree},
	    {"XS_setGoalPosition", 6, XS_setGoalPosition},
	    {"XS_setGoalScale", 6, XS_setGoalScale},
	    {"XS_spriteToSpriteTree", 1, XS_spriteToSpriteTree},
	    {"XStest_isNullHFlipSprite", 1, XStest_isNullHFlipSprite},
	    {"XS_newFlipSprite", 9, XS_newFlipSprite},
	    {"XS_flipSpriteToSprite", 1, XS_flipSpriteToSprite},
	    {"XS_setGoalPage", 5, XS_setGoalPage},
	    {"XStest_isNullHSimpleSprite", 1, XStest_isNullHSimpleSprite},
	    {"XS_newSimpleSprite", 8, XS_newSimpleSprite},
	    {"XS_simpleSpriteToSprite", 1, XS_simpleSpriteToSprite},
	    {"XS_setSurface", 4, XS_setSurface},
	    {"XSget_MinSpriteSize", 2, XSget_MinSpriteSize},
	    {"XSset_MinSpriteSize", 3, XSset_MinSpriteSize},
	    {"XStest_isNullHSoundSprite", 1, XStest_isNullHSoundSprite},
	    {"XS_newSoundSprite", 7, XS_newSoundSprite},
	    {"XS_soundSpriteToSpriteTree", 1, XS_soundSpriteToSpriteTree},
	    {"XS_updateSoundSprite", 7, XS_updateSoundSprite},
	    {"XStest_isNullHSpriteGroup", 1, XStest_isNullHSpriteGroup},
	    {"XS_newSpriteGroup", 4, XS_newSpriteGroup},
	    {"XS_spriteGroupToSpriteTree", 1, XS_spriteGroupToSpriteTree},
	    {"XS_ResetSpriteGroup", 5, XS_ResetSpriteGroup},
	    {"XStest_isNullHSpriteEngine", 1, XStest_isNullHSpriteEngine},
	    {"XS_newSpriteEngine", 4, XS_newSpriteEngine},
	    {"XS_onResizeSpriteEngine", 3, XS_onResizeSpriteEngine},
	    {"XS_deleteSpriteEngine", 3, XS_deleteSpriteEngine},
	    {"XSconsts_SpriteTreeChain", 1, XSconsts_SpriteTreeChain},
	    {"XSconsts_LightType", 1, XSconsts_LightType},
	    {"XSconsts_ThreadPriority", 1, XSconsts_ThreadPriority},

	    {0,0,0}
};

static Void controlexterns Args((Int));
static Void local controlexterns(what)
Int what; {
    switch (what) {
	case INSTALL : 
		XSname_NullHDDSurface = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHDSBuffer = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHMeshBuilder = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHRMRenderer = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHFlipBook = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHSpriteTree = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHFlipSprite = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHSimpleSprite = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHSoundSprite = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHSpriteGroup = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);
		XSname_NullHSpriteEngine = hugs->addPrimCfun(hugs->inventText(),0,0,NIL);

                break;
    }
}
static struct primInfo prims = { controlexterns, primTable, 0 };

/* After dynamically loading a module, Hugs looks up this symbol and
 * calls it with a "virtual function table" containing all necessary
 * parts of the Hugs API.  
 * (Much painful experimentation established that this was the most
 *  portable way for a DLL/shared object to access parts of Hugs.)
 *
 * This should be the only symbol exported from this module.
 */
DLLEXPORT(void) initModule(HugsAPI *);
DLLEXPORT(void) initModule(HugsAPI *hugsAPI) {
    hugs = hugsAPI;
    hugs->registerPrims(&prims);
    controlexterns(INSTALL);
}
