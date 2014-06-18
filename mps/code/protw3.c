/* protw3.c: PROTECTION FOR WIN32
 *
 *  $Id$
 *  Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
/* prmcw3.h needed to share MutatorFaultContextStruct declation */
/* with <code/prmcw3i3.c> */
#include "prmcw3.h"
#include "vm.h"

#ifndef MPS_OS_W3
#error "protw3.c is Win32-specific, but MPS_OS_W3 is not set"
#endif

#include "mpswin.h"

SRCID(protw3, "$Id$");


void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  DWORD newProtect;
  DWORD oldProtect;

  AVER(base < limit);
  AVER(base != 0);

  newProtect = PAGE_EXECUTE_READWRITE;
  if((mode & AccessWRITE) != 0)
    newProtect = PAGE_EXECUTE_READ;
  if((mode & AccessREAD) != 0)
    newProtect = PAGE_NOACCESS;

  if(VirtualProtect((LPVOID)base, (SIZE_T)AddrOffset(base, limit),
                    newProtect, &oldProtect) == 0)
    NOTREACHED;
}


LONG WINAPI ProtSEHfilter(LPEXCEPTION_POINTERS info)
{
  LPEXCEPTION_RECORD er;
  ULONG_PTR address;
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

  switch (er->ExceptionInformation[0]) {
  case 0:       /* read */
  case 8:       /* execute */
    mode = AccessREAD;
    break;
  case 1:       /* write */
    /* Pages cannot be made write-only, so an attempt to write must
       also cause a read-access if necessary */
    mode = AccessREAD | AccessWRITE;
    break;
  default:
    /* <http://msdn.microsoft.com/en-us/library/aa363082%28VS.85%29.aspx> */
    NOTREACHED;
    mode = AccessREAD | AccessWRITE;
    break;
  }

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


/* ProtSetup -- set up the protection system */

void ProtSetup(void)
{
  void *handler;
  /* See "AddVectoredExceptionHandler function (Windows)"
     <http://msdn.microsoft.com/en-us/library/windows/desktop/ms679274%28v=vs.85%29.aspx> */
  /* ProtSetup is called only once per process, not once per arena, so
     this exception handler is only installed once. */
  handler = AddVectoredExceptionHandler(1uL, ProtSEHfilter);
  AVER(handler != NULL);
}


/* ProtGranularity -- return the granularity of protection */

Size ProtGranularity(void)
{
  /* Individual pages can be protected. */
  return PageSize();
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under Win32.  See protan.c.
 */

void ProtSync(Arena arena)
{
  UNUSED(arena);
  NOOP;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
