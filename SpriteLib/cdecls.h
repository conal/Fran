// Simple defs for integration with C

#ifndef _CDECLS_H
#define _CDECLS_H

#ifdef __cplusplus

// For C++, do extern "C", and then import or export.
#define EXT_API  extern "C" AFX_EXT_API
#define EXT_CLASS(className) typedef class className *H##className
#define EXT_STRUCT(structName) typedef struct structName *H##structName
#define EXT_DECL_DATA  extern "C" AFX_EXT_DATA
#define EXT_DEF_DATA AFX_EXT_DATA
#else 

// For C, just "extern" and import.
#define EXT_API extern __declspec( dllimport )
#define EXT_CLASS(className) typedef UINT H##className
#define EXT_STRUCT EXT_CLASS
#define EXT_DECL_DATA extern __declspec( dllimport )

#endif


// Safe deletion and release.  Doesn't really belong here.
#define DELETEIF(p)  {if (p) {delete (p)    ; (p)=NULL;}}
#define RELEASEIF(p) {if (p) {(p)->Release(); (p)=NULL;}}


#endif /* _CDECLS_H */
