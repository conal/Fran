
#include <stdio.h>

/* There's two ways we can generate error messages - with different tradeoffs:
 * If we do a function call, we have to use a static buffer.
 * If we use a macro and ANSI C's string splicing, we have to use constant
 * strings - and accept a certain amount of overhead from inserting the
 * boilerplate text.
 */

#if 0

static char* ErrorMsg(char* where, char* what);

static char* ErrorMsg(char* where, char* what)
{
    static char buffer[1000];
    _snprintf(buffer, 1000, "Error %s raised in function %s", what, where);
    buffer[999] = '\0'; /* paranoia! */
    return buffer;
}

#else

#define ErrorMsg(where,what) "Error " what " raised in function " where

#endif



#define ErrorString(where) "Error raised in function " where

#define MallocError(where) "malloc failed inside " where


static char* ErrorWin(char* where);

static char* ErrorWin(char* where)
{
    static char buffer[1000]; /* space for our message   */
    static char what[1000];   /* space for Win32 message */
    LPVOID lpMsgBuf = what;

    FormatMessage( 
	FORMAT_MESSAGE_FROM_SYSTEM,
	NULL,
	GetLastError(),
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
	(LPTSTR) &lpMsgBuf,
	1000,
	NULL 
	);
    _snprintf(buffer, 1000, "Error %s raised in function %s", what, where);
    buffer[999] = '\0'; /* paranoia! */
    return buffer;
}

#define BadRgnTest(x) (x == 0 || x == GDI_ERROR)

