/* 
TEST_HEADER
 id = $Id$
 summary = mps_arena_commit_limit tests
 language = c
 link = testlib.o
OUTPUT_SPEC
 create = COMMIT_LIMIT
 commit0 = FAIL
 commit10 = OK
 com_less = FAIL
 commit_min = OK
 alloc_mv = COMMIT_LIMIT
 commit_ext = OK
 alloc_16 = OK
 alloc_big = COMMIT_LIMIT
 poolcr = COMMIT_LIMIT
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"
#include "mpscmv.h"

void *stackpointer;

mps_arena_t arena;
mps_thr_t thread;
mps_pool_t pool;
mps_pool_t pools[1000];

static void test(void) {
 int i;
 mps_addr_t a;
 mps_res_t res;
 
 /* Create an arena with a commit limit that's too small for the
  * essential MPS internal data structures -- this must fail with
  * RES_COMMIT_LIMIT. */

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_COMMIT_LIMIT, 16 * 1024);
   report_res("create", mps_arena_create_k(&arena, mps_arena_class_vm(), args));
 } MPS_ARGS_END(args);

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*20)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

/* set the commit limit to 0MB, then to 10MB+1 byte */

 report_res("commit0",
  mps_arena_commit_limit_set(arena, (size_t) (1024*1024*0)));

 report_res("commit10",
  mps_arena_commit_limit_set(arena, (size_t) (1+1024*1024*10)));

/* create a pool */

 cdie(mps_pool_create(&pool, arena, mps_class_mv(), (size_t) 64, (size_t) 64, (size_t) 64), "pool create");
 
 for (i=0; i<100; i++) {
  die(mps_alloc(&a, pool, (size_t) 64), "alloc");
 }
/* shouldn't be possible to set the commit limit to less than the amount
   currently commited */

 report_res("com_less",
  mps_arena_commit_limit_set(arena, mps_arena_committed(arena)-1));

/* should be possible to set the commit limit to the amount currently
   committed */

 report_res("commit_min",
  mps_arena_commit_limit_set(arena, mps_arena_committed(arena)));

 for (i=0; i<100; i++) {
  res=mps_alloc(&a, pool, (size_t) 1024);
 }

 report_res("alloc_mv", res);

/* extending the commit-limit by 64K should allow an allocation of 16K
   to succeed */

 report_res("commit_ext",
  mps_arena_commit_limit_set(arena,
   mps_arena_commit_limit(arena) + (size_t) (1024*64)));

 report_res("alloc_16", mps_alloc(&a, pool, (size_t) (1024*16)));

/* We'd like to get MPS_RES_RESOURCE by running out of the arena's
   address arena size, not the commit limit, but that's hard to
   arrange on 64-bit machines. See job001152. */

 report_res("alloc_big", mps_alloc(&a, pool, (size_t) (1024*1024*30)));

 mps_pool_destroy(pool);

/* now we'll check that creating a pool can fail on the COMMIT_LIMIT */

 cdie(mps_arena_commit_limit_set(arena, mps_arena_committed(arena)),
  "back to minimum limit");
 
 res = MPS_RES_OK;

 i = 0;

 while (i < sizeof pools / sizeof pools[0]) {
  res = mps_pool_create(&pools[i], arena, mps_class_mv(), (size_t) 64, (size_t) 64, (size_t) 64);
  if (res == MPS_RES_OK) {
    i++;
  } else {
    break;
  }
 }
 report_res("poolcr", res);

 for (i--; i >= 0; i--) {
  mps_pool_destroy(pools[i]);
 }

 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
