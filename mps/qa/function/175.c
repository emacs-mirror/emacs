/* 
TEST_HEADER
 id = $HopeName$
 harness = 2.2
 summary = MV SAC create alloc free destroy
 language = c
 link = testlib.o
 stdin = af_six
 parameters = ITERATIONS=30000 OBJSIZE=1024*1024 EXTEND=65536 AVGSIZE=32 MAXSIZE=65536
OUTPUT_SPEC
 completed = yes
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpsavm.h"

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_sac_t sac;
 mps_res_t res;
 void *p;
 int i;

 struct mps_sac_classes_s sac_classes[] = {
  { OBJSIZE, 30, 10 },
 };

 cdie(mps_arena_create(&arena, mps_arena_class_vmnz(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_pool_create(&pool,arena,mps_class_mv(),EXTEND,AVGSIZE,MAXSIZE),
  "create pool");

 for (i = 0; i < ITERATIONS; i++) {
  if (i % 1000 == 0) comment("%d", i);
  die(mps_sac_create(&sac, pool, 1, sac_classes), "sac create");
  MPS_SAC_ALLOC(res, p, sac, OBJSIZE, 0);
  asserts(res == MPS_RES_OK, "alloc failed");
  MPS_SAC_FREE(sac, p, OBJSIZE);
  mps_sac_destroy(sac);
 }

 mps_pool_destroy(pool);

 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
