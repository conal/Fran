#ifndef _WAVEBUFFER_H
#define _WAVEBUFFER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Given a DirectSound object/device and a wave file name, open the wave
 * file and put the contents into a directsound buffer.  Returns NULL on
 * failure.
 */

LPDIRECTSOUNDBUFFER WaveBuffer(IDirectSound* dev, char *szFileName);

#ifdef __cplusplus
}
#endif


#endif // _WAVEBUFFER_H
