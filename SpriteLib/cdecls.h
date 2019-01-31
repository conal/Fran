// Simple defs for integration with C

#ifndef _CDECLS_H
#define _CDECLS_H

#ifdef __cplusplus

// For C++, do extern "C", and then import or export.
#define EXT_API(t) extern "C" t AFX_EXT_API __stdcall
#define EXT_CLASS(className) typedef class className *H##className
#define EXT_STRUCT(structName) typedef struct structName *H##structName
#define EXT_DECL_DATA  extern "C" AFX_EXT_DATA
#define EXT_DEF_DATA AFX_EXT_DATA

#else

#if defined(_MSC_VER) //&& !defined(_STATIC_SPRITELIB)

#define _IMPORT __declspec( dllimport )
#define _STDCALL __stdcall

#else  // GCC
#define _IMPORT __attribute__ ((stdcall))
#define _STDCALL
#endif


// For C, just "extern" and import.
#define EXT_API(t) extern t _IMPORT _STDCALL
#define EXT_CLASS(className) typedef UINT H##className
#define EXT_STRUCT EXT_CLASS
#define EXT_DECL_DATA extern _IMPORT

#endif


// Safe deletion and release.  Doesn't really belong here.
#define DELETEIF(p)  {if (p) {delete (p)    ; (p)=NULL;}}
#define RELEASEIF(p) {if (p) {(p)->Release(); (p)=NULL;}}


#endif /* _CDECLS_H */
