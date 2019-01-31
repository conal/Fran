/*

Abstract:

  Make a DSound buffer from a Wave file.
  Adapted from DirectX sdk\samples\dsshow\shell.c

  Last modified Fri Nov 07 11:59:33 1997 by conal

*/

#include <windows.h>
#include <windowsx.h>
#include <stdio.h>
#include <dsound.h>

/* In DirectSound, sounds are associated with windows.  In version 2, if
   the window lost visual focus, it went quiet.  This flag, introduced in
   version 3, fixes that problem.  We need the fix, because we don't have
   a real window when DirectSound is initialized.  However, Visual C++,
   version 5 ships with version 2 DirectSound headers, so we have to add
   this one by hand.  (Otherwise, you need to also have the DirectX SDK
   installed, and you must override VC's search order.)
*/
#ifndef DSBCAPS_GLOBALFOCUS
#define DSBCAPS_GLOBALFOCUS         0x00008000 
#endif

#include "wassert.h"
#include "wave.h"
#include "waveBuffer.h"

/*
 * Make a new DSound buffer, given device, data, size, format, and buffer
 * pointer-pointer.
 */
HRESULT NewDirectSoundBuffer(IDirectSound *dev,
                             BYTE *fileData,
                             UINT cbSize,
                             WAVEFORMATEX *pwfx,
                             LPLPDIRECTSOUNDBUFFER ppDSB
                            )
{

    DSBUFFERDESC        dsbd;
    DSBCAPS         dsbc;
    HRESULT         hr;
    LPDIRECTSOUNDBUFFER pDSB;

    BYTE            *pbData         = NULL;
    BYTE            *pbData2        = NULL;
    DWORD           dwLength;
    DWORD           dwLength2;

    pbData         = NULL;

    /* Set up the direct sound buffer. */
    memset(&dsbd, 0, sizeof(DSBUFFERDESC));
    dsbd.dwSize                 = sizeof(DSBUFFERDESC);
    dsbd.dwFlags                =
          DSBCAPS_STATIC        // load full sound
        | DSBCAPS_CTRLDEFAULT   // default vol, pan, freq
        | DSBCAPS_GLOBALFOCUS 	// don't mute on focus loss
//      | DSBCAPS_GETCURRENTPOSITION2
        ;
    dsbd.dwBufferBytes          = cbSize;
    dsbd.lpwfxFormat            = pwfx;
    if ((hr = IDirectSound_CreateSoundBuffer(dev,
              &dsbd,
              &pDSB,
              NULL )) != DS_OK)
    {
    goto ERROR_IN_ROUTINE;
    }

    // Ok, lock the sucker down, and copy the memory to it.

    if ((hr = pDSB->lpVtbl->Lock(pDSB,
            0,
            cbSize,
            &pbData,
            &dwLength,
            &pbData2,
            &dwLength2,
                        0L)) != DS_OK)
    {
    goto ERROR_IN_ROUTINE;
    }

    Assert(pbData != NULL);
    memcpy(pbData, fileData, cbSize);

    // Ok, now unlock the buffer, we don't need it anymore.
    if ((hr = pDSB->lpVtbl->Unlock(pDSB,
                          pbData, cbSize,
                          NULL, 0)) != DS_OK)
    {
    goto ERROR_IN_ROUTINE;
    }

    pbData = NULL;

    dsbc.dwSize = sizeof(dsbc);
    if ( (hr = IDirectSoundBuffer_GetCaps(pDSB, &dsbc)) == DS_OK)
    {
      (*ppDSB) = pDSB;            // Finally set caller's DS buffer ptr
      // pDSB->SetCurrentPosition(0);  // for an old AWE32 driver bug
      return(hr); 
    }

ERROR_IN_ROUTINE:
    if (pbData != NULL)
    {
    hr = pDSB->lpVtbl->Unlock(pDSB, pbData,
                        cbSize, NULL, 0);
    pbData = NULL;
    }

    if (pDSB != NULL)
    {
    pDSB->lpVtbl->Release(pDSB);
    pDSB = NULL;
    }
    (*ppDSB) = pDSB;            // Finally set caller's DS buffer ptr
    return(hr); 
}


/*
 * Given a DirectSound object/device and a wave file name, open the wave
 * file and put the contents into a DSound buffer.  Returns NULL on
 * failure.
 */

LPDIRECTSOUNDBUFFER
WaveBuffer(IDirectSound* dev, char *szFileName)
{
    UINT            cSamples;
    //int             nFileName;

    // From struct _fileinfo in shell.h
    BYTE                *pbData=NULL;  // Pointer to actual data of file.
    UINT                 cbSize;       // Size of data.
    WAVEFORMATEX        *pwfx=NULL;    // Pointer to waveformatex structure.
    LPDIRECTSOUNDBUFFER  pDSB=NULL;    // Pointer to direct sound buffer.

    if (WaveLoadFile(szFileName,
		     &cbSize,
		     &cSamples,
		     &pwfx,
		     &pbData) != DS_OK)
      {
          fprintf(stderr,
		  "Bad wave file or file too big to fit in memory: %s\n",
                  szFileName);
          goto ERROR_DONE_ROUTINE;
      }

    if (NewDirectSoundBuffer(dev,
			     pbData,
			     cbSize,
			     pwfx,
			     &pDSB) != DS_OK)
      {
          fprintf(stderr,
		  "Cannot create new buffer for %s\n",
		  szFileName);
          goto ERROR_DONE_ROUTINE;
      }
    
    Assert(pbData != NULL);
    return pDSB;
       
  ERROR_DONE_ROUTINE:

    fflush(stdout);
    if (pwfx   != NULL) GlobalFreePtr(pwfx);
    if (pbData != NULL) GlobalFreePtr(pbData);
    if (pDSB   != NULL) pDSB->lpVtbl->Release(pDSB);
    return NULL;
}
