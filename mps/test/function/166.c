/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!166.c(trunk.1) $
 summary = MVFF with AP split allocate from SW log (af_six)
 language = c
 link = testlib.o
 stdin = af_six
 harness = 2.0
 parameters = EXTEND=65536 AVGSIZE=32 ALIGN=8 \
              ARENAHIGH=1 SLOTHIGH=1 FIRST=1 SPLIT1=0 SPLIT2=0
OUTPUT_SPEC
 completed = yes
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

#define MAXOBJS (30000)

mps_addr_t objs[MAXOBJS];
size_t    sizes[MAXOBJS];

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_ap_t ap1, ap2, ap3, ap;
 mps_pool_t pool1, pool2, pool3, pool;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;
 mps_res_t res;

 maxcom = 0;
 cdie(mps_arena_create(&arena, mps_arena_class_vmnz(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_pool_create(&pool1,arena,mps_class_mvff(),
  EXTEND,AVGSIZE,ALIGN,ARENAHIGH,SLOTHIGH,FIRST),
  "create pool1");
 cdie(mps_pool_create(&pool2,arena,mps_class_mvff(),
  EXTEND,AVGSIZE,ALIGN,ARENAHIGH,SLOTHIGH,FIRST),
  "create pool2");
 cdie(mps_pool_create(&pool3,arena,mps_class_mvff(),
  EXTEND,AVGSIZE,ALIGN,ARENAHIGH,SLOTHIGH,FIRST),
  "create pool3");
 cdie(mps_ap_create(&ap1,pool1), "ap 1");
 cdie(mps_ap_create(&ap2,pool2), "ap 2");
 cdie(mps_ap_create(&ap3,pool3), "ap 3");


 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = ((event.alloc.size + ALIGN-1)|(ALIGN-1))^(ALIGN-1);
   ap = (size < SPLIT1) ? ap1 : (size < SPLIT2) ? ap2 : ap3;
   do {
    MPS_RESERVE_BLOCK(res, objs[id], ap, size);
    asserts(res == MPS_RES_OK, "alloc failed");
   } while (!mps_commit(ap, objs[id], size));
   sizes[id] = size;
   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   size = sizes[id];
   pool = (size < SPLIT1) ? pool1 : (size < SPLIT2) ? pool2 : pool3;
   mps_free(pool, objs[id], size);
  }
  size = mps_arena_committed(arena);
  if (size > maxcom) maxcom=size;
 }
 report("maxcom", "%ld", maxcom);

 mps_ap_destroy(ap1);
 mps_ap_destroy(ap2);
 mps_ap_destroy(ap3);
 mps_pool_destroy(pool1);
 mps_pool_destroy(pool2);
 mps_pool_destroy(pool3);

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
