/*  ==== VIRTUAL MEMORY MAPPING FOR SUNOS 4 ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the virtual memory mapping interface (vm.h)
 *  for SunOS 4.
 *
 *  SunOS does not provide a way of allocating address space without
 *  committing storage to it.  This implementation assumes that the
 *  range between MIN and MAX is safe to use, and only allow it to be
 *  used once.
 *
 *  mmap(2) is used to reserve address space by creating a mapping to
 *  a random readable file (/etc/passwd) but with page access none.
 *
 *  mmap(2) is used to map pages onto store by creating a copy-on-write
 *  mapping to /dev/zero.
 *
 *  Notes
 *   2. This module uses static data.  richard 1995-02-15
 */

#include "std.h"
#include "vm.h"

#ifndef OS_SUNOS
#error "vmsu.c is SunOS 4 specific, but OS_SUNOS is not set"
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/errno.h>


/* Unprototyped system calls.  Note that these are not fixed up */
/* by std.h because that only fixeds up ANSI discrepancies. */

extern int close(int fd);
extern int munmap(caddr_t addr, int len);
extern int getpagesize(void);


static int none_fd = -1;
static int zero_fd = -1;
static Addr highest = 0, lowest = (Addr)-1;


Addr VMGrain(void)
{
  Addr grain;

  grain = (Addr)getpagesize();
  AVER(IsPoT(grain));

  return(grain);
}


Error VMReserve(Addr *baseReturn, Addr *limitReturn, Addr size)
{
  caddr_t addr;
  Addr base, limit;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(IsAligned(grain, size));
  AVER(size != 0);

  if(zero_fd == -1)
  {
    AVER(none_fd == -1);

    zero_fd = open("/dev/zero", O_RDONLY);
    if(zero_fd == -1)
      return(ErrIO);

    none_fd = open("/etc/passwd", O_RDONLY);
    if(none_fd == -1)
    {
      close(zero_fd);
      return(ErrIO);
    }
  }

  addr = mmap(0, size, PROT_NONE, MAP_SHARED, none_fd, 0);
  if((int)addr == -1)
  {
    if(errno == ENOMEM)
      return(ErrRESOURCE);
    else
      return(ErrFAILURE);
  }

  base = (Addr)addr;
  limit = base + size;

  if(base < lowest)
    lowest = base;

  if(limit > highest)
    highest = limit;

  *baseReturn = base;
  *limitReturn = limit;
  return(ErrSUCCESS);
}


void VMRelease(Addr base, Addr limit)
{
  int r;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(base < limit);
  AVER(base >= lowest);
  AVER(limit <= highest);
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(zero_fd != -1 && none_fd != -1);

  r = munmap((caddr_t)base, (int)(limit - base));
  AVER(r == 0);
}


Error VMMap(Addr base, Addr limit)
{
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= lowest);
  AVER(limit <= highest);
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(zero_fd != -1 && none_fd != -1);

  if((int)mmap((caddr_t)base, (int)(limit - base),
	       PROT_READ | PROT_WRITE | PROT_EXEC,
	       MAP_PRIVATE | MAP_FIXED,
	       zero_fd, 0) == -1)
    return(ErrRESMEM);

  return(ErrSUCCESS);
}


void VMUnmap(Addr base, Addr limit)
{
  caddr_t addr;
#ifdef DEBUG_ASSERT
  Addr grain = VMGrain();
#endif

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base >= lowest);
  AVER(limit <= highest);
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));
  AVER(zero_fd != -1 && none_fd != -1);

  addr = mmap((caddr_t)base, (int)(limit - base),
	      PROT_NONE, MAP_SHARED, none_fd, 0);
  AVER((int)addr != -1);
}
