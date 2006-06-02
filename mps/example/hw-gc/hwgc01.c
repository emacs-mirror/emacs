/* Simple C client of the MPS, with MV pool and VM arena */

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
  printf("  Pool has:\n    %lu bytes in use;\n    %lu bytes free.\n",
    (unsigned long)cbPoolSize,
    (unsigned long)cbPoolFree
  );
}

int main(void)
{
  size_t cbArena = 1 * 1024 * 1024 * 1024;  /* 1GB, ie. one quarter of 32-bit address space */
  mps_arena_t ArenaDemo = NULL;
  mps_pool_t PoolDemo = NULL;

  mps_res_t res;

  {
    /* Create arena */
    
    res = mps_arena_create(&ArenaDemo, mps_arena_class_vm(), cbArena);
    if (res != MPS_RES_OK) {
      printf("mps_arena_create: failed with res %d.\n", res);
      exit(2);
    }
    
    report("Created arena", ArenaDemo, NULL);
  }
  
  {
    /* Create pool */
    
    size_t cbPoolExtend = 1024;
    size_t cbObjectAvg = 32;
    size_t cbPoolMax = 64 * 1024;
    
    res = mps_pool_create(&PoolDemo, ArenaDemo, mps_class_mv(), cbPoolExtend, cbObjectAvg, cbPoolMax);
    if (res != MPS_RES_OK) {
      printf("mps_pool_create: failed with res %d.\n", res);
      exit(2);
    }
    
    report("Created pool", ArenaDemo, PoolDemo);
  }

  {
    /* Allocate memory */
    
    size_t cbBuffer = 100;
    void *p = NULL;

    res = mps_alloc(&p, PoolDemo, cbBuffer);
    if (res != MPS_RES_OK) {
      printf("mps_alloc: failed with res %d.\n", res);
      exit(2);
    }
    
    report("Allocated 100 bytes", ArenaDemo, PoolDemo);

    {
      /* Show that it really is memory */

      char *pbBuffer = (char *)p;
      
      strcpy(pbBuffer, "hello--world\n");
      pbBuffer[5] = ',';
      pbBuffer[6] = ' ';
      printf(pbBuffer);
    }
  }

  printf(
    "Success: The hello-world example code successfully allocated\n"
    "some memory using mps_alloc(), in an MV pool, in a VM arena.\n"
  );
  return 0;
}
