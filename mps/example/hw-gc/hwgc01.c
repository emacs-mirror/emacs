/* Simple C client of the MPS, with ap in MV pool and VM arena */
/* $Id$ */

#include <stdlib.h>  /* for malloc */
#include <stdio.h>  /* for printf */
#include <string.h>  /* for strcpy */

#include "mps.h"
#include "mpsavm.h"   /* for mps_arena_class_vm */
#include "mpscmv.h"   /* for mps_class_mv */

static void reportArena(mps_arena_t Arena);
static void reportPoolmv(mps_pool_t Pool);

static void report(char* comment, mps_arena_t Arena, mps_pool_t Pool)
{
  printf("%s:\n", comment);
  if (Arena)
    reportArena(Arena);
  if (Pool)
    reportPoolmv(Pool);
  printf("--\n\n");
}     

static void reportArena(mps_arena_t Arena)
{
  size_t cbArenaReserved = 0;
  size_t cbArenaCommitted = 0;
  size_t cbArenaSpareCommitted = 0;
  cbArenaReserved = mps_arena_reserved(Arena);
  cbArenaCommitted = mps_arena_committed(Arena);
  cbArenaSpareCommitted = mps_arena_spare_committed(Arena);
  printf("  Arena has:\n    %lu bytes reserved;\n    %lu bytes committed;\n    %lu bytes spare committed.\n", 
    (unsigned long)cbArenaReserved,
    (unsigned long)cbArenaCommitted,
    (unsigned long)cbArenaSpareCommitted
  );
}

static void reportPoolmv(mps_pool_t Pool)
{
  size_t cbPoolSize = 0;
  size_t cbPoolFree = 0;
  cbPoolSize = mps_mv_size(Pool);
  cbPoolFree = mps_mv_free_size(Pool);
  printf("  Pool is:\n    %lu bytes;\n    %lu bytes free; => %lu bytes in use;.\n",
    (unsigned long)cbPoolSize,
    (unsigned long)cbPoolFree,
    (unsigned long)cbPoolSize-cbPoolFree
  );
}

static void exit_if(mps_res_t res, char *comment)
{
  if (res != MPS_RES_OK) {
    printf("%s: failed with res %d.\n", comment, res);
    exit(2);
  }
}

int main(void)
{
  size_t cbArena = 1 * 1024 * 1024 * 1024;  /* 1GB, ie. one quarter of 32-bit address space */
  mps_arena_t ArenaDemo = NULL;
  mps_pool_t PoolDemo = NULL;
  mps_ap_t ApDemo = NULL;

  mps_res_t res;

  {
    /* Create arena */
    
    res = mps_arena_create(&ArenaDemo, mps_arena_class_vm(), cbArena);
    exit_if(res, "mps_arena_create");
    
    report("Created arena", ArenaDemo, NULL);
  }
  
  {
    /* Create pool */
    
    size_t cbPoolExtend = 1024;
    size_t cbObjectAvg = 32;
    size_t cbPoolMax = 64 * 1024;
    
    res = mps_pool_create(&PoolDemo, ArenaDemo, mps_class_mv(), cbPoolExtend, cbObjectAvg, cbPoolMax);
    exit_if(res, "mps_pool_create");
    
    report("Created pool", ArenaDemo, PoolDemo);
  }
  
  {
    /* Create ap */
    
    res = mps_ap_create(&ApDemo, PoolDemo);
    exit_if(res, "mps_ap_create");
    
    report("Created ap", ArenaDemo, PoolDemo);
  }

  {
    /* Allocate memory */
    
    size_t cbBuffer = 256;
    void *p = NULL;

    do {
      res = mps_reserve(&p, ApDemo, cbBuffer);
      exit_if(res, "mps_reserve");
      /* Initialise my object here -- ie. make it valid. */
      /* (In this example, memory is manually managed, so even
          uninitialized memory is already a 'valid object'.  So 
          we can call commit immediately.) */
    } while (! mps_commit(ApDemo, p, cbBuffer));
    
    report("Allocated 256 bytes", ArenaDemo, PoolDemo);

    {
      /* Show that it really is memory */

      char *pbBuffer = (char *)p;
      
      strcpy(pbBuffer, "hello--world\n");
      pbBuffer[5] = ',';
      pbBuffer[6] = ' ';
      printf(pbBuffer);
    }
    
    mps_free(PoolDemo, p, cbBuffer);
    
    report("Freed 256 bytes", ArenaDemo, PoolDemo);
  }
  
  {
    /* Clear up */
    
    mps_ap_destroy(ApDemo);
    report("Destroyed ap", ArenaDemo, PoolDemo);
    mps_pool_destroy(PoolDemo);
    report("Destroyed pool", ArenaDemo, NULL);
    mps_arena_destroy(ArenaDemo);
  }

  printf(
    "Success: The hello-world example code successfully allocated\n"
    "some memory using an allocation point with\n"
    "mps_reserve()..mps_commit(), in an MV pool, in a VM arena,\n"
    "then freed the memory, and destroyed the ap, pool, and arena.\n"
  );
  return 0;
}

/*
COPYRIGHT AND LICENSE

Copyright (C) 2006 Ravenbrook Limited <http://www.ravenbrook.com/>.
All rights reserved.  This is an open source license.  Contact
Ravenbrook for commercial licensing options.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Redistributions in any form must be accompanied by information on how
to obtain complete source code for this software and any
accompanying software that uses this software.  The source code must
either be included in the distribution or be available for no more than
the cost of distribution plus a nominal fee, and must be freely
redistributable under reasonable conditions.  For an executable file,
complete source code means the source code for all modules it contains.
It does not include source code for modules or files that typically
accompany the major components of the operating system on which the
executable file runs.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/