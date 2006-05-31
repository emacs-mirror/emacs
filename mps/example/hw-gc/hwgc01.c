/* Demo File: 05alloc.c */
/* Simple C client of the MPS */

#include <stdlib.h>  /* for malloc */
#include <stdio.h>  /* for printf */
#include <string.h>  /* for strcpy */

#include "mps.h"
#include "mpsacl.h"   /* for mps_arena_class_cl */
#include "mpscmv.h"   /* for mps_class_mv */

static void reportPoolmv(mps_pool_t Pool)
{
    size_t cbPoolFree = mps_mv_free_size(Pool);
    printf(
      "Pool has %lu bytes free.\n",
      (unsigned long)cbPoolFree
    );
}

int main(void)
{
  void *pBlock = NULL;
  size_t cbBlock = 1024 * 1024;
  mps_arena_t ArenaDemo = NULL;
  mps_pool_t PoolDemo = NULL;

  mps_res_t res;

  {
    /* Create arena */
    
    pBlock = malloc(cbBlock);
    if(pBlock == NULL) {
      printf("malloc failed!\n");
      exit(1);
    }

    res = mps_arena_create(
      &ArenaDemo,
      mps_arena_class_cl(),
      cbBlock,
      pBlock
    );
    if (res != MPS_RES_OK) {
      printf("mps_arena_create: failed with res %d.\n", res);
      exit(2);
    }
  }
  
  {
    /* Create pool */
    
    size_t cbPoolExtend = 1024;
    size_t cbObjectAvg = 32;
    size_t cbPoolMax = 64 * 1024;
    
    size_t cbPoolFree = 0;

    res = mps_pool_create(
      &PoolDemo,
      ArenaDemo,
      mps_class_mv(),
      cbPoolExtend,
      cbObjectAvg,
      cbPoolMax
    );
    if (res != MPS_RES_OK) {
      printf("mps_pool_create: failed with res %d.\n", res);
      exit(2);
    }
    
    reportPoolmv(PoolDemo);
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
    
    reportPoolmv(PoolDemo);

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
    "some memory using mps_alloc(), in an MV pool, in a CL arena.\n"
  );
  return 0;
}
