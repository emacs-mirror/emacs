/*  impl.c.protnt
 *
 *               PROTECTION FOR WIN32
 *  $HopeName: MMsrc!protnt.c(MMdevel_restr.5) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 */

#include "mpm.h"

#ifndef MPS_OS_W3
#error "protnt.c is Win32 specific, but MPS_OS_W3 is not set"
#endif

#include <windows.h>

SRCID(protnt, "$HopeName: MMsrc!protnt.c(MMdevel_restr.5) $");


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
                    newProtect, &oldProtect) != TRUE)
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

  er = info->ExceptionRecord;

  if(er->ExceptionCode != EXCEPTION_ACCESS_VIOLATION)
    return EXCEPTION_CONTINUE_SEARCH;

  AVER(er->ExceptionFlags == 0); /* continuable exception */

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

  AVER(base < limit);  /* nasty case (base = -1): continue search? @@@ */

  if(SpaceAccess(base, mode))
    action = EXCEPTION_CONTINUE_EXECUTION;
  else
    action = EXCEPTION_CONTINUE_SEARCH;

  return action;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under Win32.
 */

void ProtSync(Space space)
{
  NOOP;
}


void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  void *result;

  __try {
    result = f(p, s);
  } __except(ProtSEHfilter(GetExceptionInformation())) {
    NOTREACHED;
  }

  *resultReturn = result;
}
