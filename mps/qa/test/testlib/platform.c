#include "mps.h"
#include "platform.h"

#ifdef MPS_OS_SU

void *memmove(void *to, void *from, size_t bytes)
{
 bcopy((char *)from, (char *)to, (int)bytes);
 return to;
}

#endif
