/*  impl.c.protw3: PROTECTION FOR WIN32
 *
 *  $HopeName: MMsrc!protw3.c(trunk.15) $
 *  Copyright (C) 1995, 1997 Harlequin Group, all rights reserved
 */

#include "mpm.h"
/* prmcw3.h needed to share MutatorFaultContextStruct declation */
/* with impl.c.prmcw3i3 */
#include "prmcw3.h"

#ifndef MPS_OS_W3
#error "protw3.c is Win32-specific, but MPS_OS_W3 is not set"
#endif
#ifndef PROTECTION
#error "protw3.c implements protection, but PROTECTION is not set"
#endif

#include "mpswin.h"

SRCID(protw3, "$HopeName: MMsrc!protw3.c(trunk.15) $");


void ProtSetup(void)
{
  return;
}


void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  DWORD newProtect;
  DWORD oldProtect;

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base != 0);

  newProtect = PAGE_EXECUTE_READWRITE;
  if((mode & AccessWRITE) != 0)
    newProtect = PAGE_EXECUTE_READ;
  if((mode & AccessREAD) != 0)
    newProtect = PAGE_NOACCESS;

  if(VirtualProtect((LPVOID)base, (DWORD)AddrOffset(base, limit),
                    newProtect, &oldProtect) == 0)
    NOTREACHED;
}


LONG ProtSEHfilter(LPEXCEPTION_POINTERS info)
{
  LPEXCEPTION_RECORD er;
  DWORD iswrite;
  DWORD address;
  AccessSet mode;
  Addr base, limit;
  LONG action;
  MutatorFaultContextStruct context;

  er = info->ExceptionRecord;

  if(er->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return EXCEPTION_CONTINUE_SEARCH;
  
  context.ep = info;

  /* assert that the exception is continuable */
  /* Note that Microsoft say that this field should be 0 or */
  /* EXCEPTION_NONCONTINUABLE, but this is not true */
  AVER((er->ExceptionFlags & EXCEPTION_NONCONTINUABLE) == 0);

  /* er->ExceptionRecord is pointer to next exception in chain */
  /* er->ExceptionAddress is where exception occurred */

  AVER(er->NumberParameters >= 2);

  iswrite = er->ExceptionInformation[0]; /* 0 read; 1 write */
  AVER(iswrite == 0 || iswrite == 1);

  /* Pages cannot be made write-only, so an attempt to write must
   * also cause a read-access if necessary */
  if(iswrite)
    mode = AccessREAD | AccessWRITE;
  else
    mode = AccessREAD;

  address = er->ExceptionInformation[1];

  base = (Addr)address;
  limit = AddrAdd(address, sizeof(Addr));

  if(base < limit) {
    if(ArenaAccess(base, mode, &context))
      action = EXCEPTION_CONTINUE_EXECUTION;
    else
      action = EXCEPTION_CONTINUE_SEARCH;
  } else {
    /* Access on last sizeof(Addr) (ie 4 on this platform) bytes */
    /* in memory.  We assume we can't get this page anyway (see */
    /* impl.c.vmw3.assume.not-last) so it can't be our fault. */
    action = EXCEPTION_CONTINUE_SEARCH;
  }

  return action;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under Win32.
 */

void ProtSync(Arena arena)
{
  UNUSED(arena);
  NOOP;
}


void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  void *result = NULL; /* stop warnings about uninitialized result */

  AVER(resultReturn != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  __try {
    result = f(p, s);
  } __except(ProtSEHfilter(GetExceptionInformation())) {
    NOTREACHED;
  }

  *resultReturn = result;
}
