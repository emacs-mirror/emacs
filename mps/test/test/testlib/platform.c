/* $HopeName: MMQA_harness!testlib:platform.c(trunk.3) $
*/

#include "testlib.h"
/* that includes platform.h, so we don't need to */

#ifdef MPS_OS_SU

void *memmove(void *to, void *from, size_t bytes)
{
 bcopy((char *)from, (char *)to, (int)bytes);
 return to;
}

#endif

#ifdef MPS_OS_W3

LONG mySEHFilter(LPEXCEPTION_POINTERS info) {
 LPEXCEPTION_RECORD er;
 int write;
 unsigned long address;

 er = info->ExceptionRecord;

 if (er->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
  write = er->ExceptionInformation[0];
  address = er->ExceptionInformation[1];
  report("memoryerror", "true");
  if (write == 1) {
   report("memoryop", "write");
  } else { /* write == 0 */
   report("memoryop", "read");
  }
  report("memoryaddr", "%ld", address);
  myabort();
 }

 /* otherwise don't interfere */
 return EXCEPTION_CONTINUE_SEARCH;
}

#endif
