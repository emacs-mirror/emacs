/* protw3.c: PROTECTION FOR WIN32
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
/* prmcw3.h needed to share MutatorFaultContextStruct declation */
/* with <code/prmcw3i3.c> */
#include "prmcw3.h"

#ifndef MPS_OS_W3
#error "protw3.c is Win32-specific, but MPS_OS_W3 is not set"
#endif
#ifndef PROTECTION
#error "protw3.c implements protection, but PROTECTION is not set"
#endif

#include "mpswin.h"

SRCID(protw3, "$Id$");


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
    /* <code/vmw3.c#assume.not-last>) so it can't be our fault. */
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


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
