/* 
TEST_HEADER
 id = $HopeName$
 summary = EPVM low-memory test
 language = c
 link = testlib.o epvmfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"

#define MAX_SAVE 10
#define INIT_SAVE 0 
#define OBJ_SIZE 8 

void *stackpointer;

static mps_pool_t pooli[1024];

static void tryout(size_t comlimit, int pools) {
 mps_arena_t arena;
 mps_thr_t thread;
 mps_fmt_t format;
 mps_pool_t pool;
 mps_ap_t ap;
 psobj *a;
 int i, p;

 comment("Limit: %lu, pools: %d", (unsigned long) comlimit, pools);
 if (mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*128)))
  goto no_arena;
 
 mps_arena_commit_limit_set(arena, comlimit);

 if (mps_thread_reg(&thread, arena)) goto no_thread;

 if (mps_fmt_create_A(&format, arena, &fmtepvm)) goto no_fmt;

 for (p = 0; p < pools; p ++) {
  if (mps_pool_create(&pooli[p], arena, mps_class_mv(), 4096, 32, 4096)) {
   break;
  };
 }

 if (mps_pool_create(&pool, arena, mps_class_epvm(), format, 10, 0))
  goto no_pool;

 if (mps_ap_create(&ap, pool, 0)) goto no_ap;

 allocrepvm(&a, ap, OBJ_SIZE);
 mps_epvm_save(pool);
 allocrepvm(&a, ap, OBJ_SIZE);
 mps_epvm_save(pool);
 for (i=0; i<100; i++) {
  allocrepvm(&a, ap, OBJ_SIZE);
 }
 mps_epvm_restore(pool, 0);

 mps_ap_destroy(ap);
no_ap:
 mps_pool_destroy(pool);
no_pool:
 for (; p >= 0; p--) {
  if (pooli[p]) {
   mps_pool_destroy(pooli[p]);
  }
 }
 mps_fmt_destroy(format);
no_fmt:
 mps_thread_dereg(thread);
no_thread:
 mps_arena_destroy(arena);
no_arena: 
 a = NULL;
}

static void test(void)
{
 int i;

 for (i = 0; i <1024; i ++) {
  tryout(1024*64, i);
 }
}

int main(void) {
 void *m;
 stackpointer=&m;

 easy_tramp(test);
 pass();
 return 0;
}



