#include <stdio.h>

char* CouldNotOpen(char* file)
{
    static char buffer[1000];
    sprintf(buffer, "Could not open file \"%s\"", file);
    buffer[999] = '\0'; /* paranoia! */
    return buffer;
}
