/*  impl.c.protnt
 *
 *               PROTECTION FOR WIN32
 *  $HopeName: MMsrc!protnt.c(trunk.4) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 */

#include "std.h"
#include "prot.h"
#include "fault.h"

#ifndef MPS_OS_W3
#error "protnt.c is Win32 specific, but MPS_OS_W3 is not set"
#endif

#include <windows.h>

SRCID("$HopeName");


Addr ProtGrain(void)
{
  Addr grain;
  SYSTEM_INFO si;

  AVER(sizeof(DWORD) == sizeof(Addr));

  GetSystemInfo(&si);
  grain = (Addr)si.dwPageSize;
  AVER(IsPoT(grain));

  return grain;
}

void ProtSetup(void)
{
  return;
}

void ProtSet(Addr base, Addr limit, ProtMode mode)
{
#ifdef DEBUG
  Addr grain = ProtGrain();
#endif
  DWORD newProtect;
  DWORD oldProtect;

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base != 0);
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  newProtect = PAGE_EXECUTE_READWRITE;
  if((mode & ProtWRITE) != 0)
    newProtect = PAGE_EXECUTE_READ;
  if((mode & ProtREAD) != 0)
    newProtect = PAGE_NOACCESS;

  if(VirtualProtect((LPVOID)base, (DWORD)(limit - base),
                    newProtect, &oldProtect) != TRUE)
    NOTREACHED;
}

LONG ProtSEHfilter(LPEXCEPTION_POINTERS info)
{
  LPEXCEPTION_RECORD er;
  DWORD iswrite;
  DWORD address;
  ProtMode mode;
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

  if(iswrite)
    mode = ProtWRITE; 
  else
    mode = ProtREAD;

  address = er->ExceptionInformation[1];

  base = (Addr)address;
  limit = (Addr)address + sizeof(Addr);

  AVER(base < limit);  /* nasty case (base = -1): continue search? @@@ */

  if(FaultDispatch(base, limit, mode))
    action = EXCEPTION_CONTINUE_EXECUTION;
  else 
    action = EXCEPTION_CONTINUE_SEARCH;

  return action;
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
