/*  impl.c.protfr: PROTECTION FOR FreeBSD
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.
 *
 */

#include "mpm.h"

#ifndef MPS_OS_FR
#error "protfr.c is FreeBSD specific, but MPS_OS_FR is not set"
#endif
#ifndef PROTECTION
#error "protfr.c implements protection, but PROTECTION is not set"
#endif

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>

SRCID(protfr, "$Id$");


/* ProtSet -- set protection
 *
 * This is just a thin veneer on top of mprotect(2).
 */

void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  int flags;
  int res;

  AVER(sizeof(int) == sizeof(Addr));   /* should be redundant; will fail on Alpha */
  AVER(base < limit);
  AVER(base != 0);
  AVER(AddrOffset(base, limit) <= INT_MAX);     /* should be redundant */

#if 0
  /* .flags.trouble: This less strict version of flags (which allows write
   * access unless explicitly told not to) caused mmqa test 37 to fail.
   * This might be a bug in MPS, so for now we go with the stricter
   * version that matches the Win32 implementation. */
  /* .flags.trouble.freebsd: the above comment was in the Linux version
   * of this code; I haven't verified it for FreeBSD.  */
  flags = 0;
  if((mode & AccessREAD) == 0)
    flags |= PROT_READ | PROT_EXEC;
  if((mode & AccessWRITE) == 0)
    flags |= PROT_WRITE;
#endif
  flags = PROT_READ | PROT_WRITE | PROT_EXEC;
  if((mode & AccessWRITE) != 0)
    flags = PROT_READ | PROT_EXEC;
  if((mode & AccessREAD) != 0)
    flags = 0;

  res = mprotect((void *)base, (size_t)AddrOffset(base, limit), flags);
  AVER(res == 0);
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under FreeBSD.
 */

void ProtSync(Arena arena)
{
  NOOP;
}



/* ProtTramp -- protection trampoline
 *
 * The protection trampoline is trivial under FreeBSD, as there is
 * nothing that needs to be done in the dynamic context of the mutator
 * in order to catch faults.  (Contrast this with Win32 Structured
 * Exception Handling.)
 */

void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  AVER(resultReturn != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  *resultReturn = (*f)(p, s);
}
