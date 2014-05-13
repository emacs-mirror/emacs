/* 
TEST_HEADER
 id = $Id$
 summary = MVT greed test
 language = c
 link = testlib.o
 parameters = OBJECTS=1000 OBJSIZE=8192 DEPTH=2 FRAGLIMIT=50
END_HEADER
*/

#include "testlib.h"
#include "mpscmvt.h"
#include "mpsavm.h"

void *stackpointer;
mps_arena_t arena;

static mps_addr_t objs[OBJECTS];

static mps_res_t mvt_alloc(mps_addr_t *ref, mps_ap_t ap, size_t size) {
 mps_res_t res;

 size = ((size+7)/8)*8;

 do {
  MPS_RESERVE_BLOCK(res, *ref, ap, size);
  if (res != MPS_RES_OK) return res;
 } while (!mps_commit(ap, *ref, size));

 return MPS_RES_OK;
}

static void test (void) {
 mps_thr_t thread;
 mps_pool_t pool;
 mps_ap_t ap;
 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");
 die(
  mps_pool_create(&pool, arena, mps_class_mvt(),
                  OBJSIZE, OBJSIZE, OBJSIZE, DEPTH, FRAGLIMIT),
  "create MVT pool");

 die(mps_ap_create(&ap, pool, mps_rank_ambig()), "create ap");

 for (i = 0; i < OBJECTS; i++) {
  die(mvt_alloc(&objs[i], ap, OBJSIZE), "alloc");
 }
 report("size1", "%ld", mps_arena_committed(arena));

 for (i = 0; i < OBJECTS; i+=2) {
  mps_free(pool, objs[i], OBJSIZE);
  die(mvt_alloc(&objs[i], ap, OBJSIZE), "alloc");
 }
 report("size2", "%ld", mps_arena_committed(arena));
 
 mps_ap_destroy(ap);
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
