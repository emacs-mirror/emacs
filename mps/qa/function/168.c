/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!168.c(trunk.2) $
 summary = MVFF split allocate from SW log (af_six), with hysteresis control
 language = c
 link = testlib.o
 stdin = af_six
 parameters = EXTEND=65536 AVGSIZE=32 ALIGN=8 \
              ARENAHIGH=1 SLOTHIGH=1 FIRST=1 SPLIT1=0 SPLIT2=0 \
              COMMIT=30000000 SPARE=0
OUTPUT_SPEC
 result = pass
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
 mps_pool_t pool1, pool2, pool3, pool;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;

 maxcom = 0;
 cdie(mps_arena_create(&arena, mps_arena_class_vmnz(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mps_arena_commit_limit_set(arena, COMMIT);
 mps_arena_spare_commit_limit_set(arena, SPARE);

 cdie(mps_pool_create(&pool1,arena,mps_class_mvff(),
  EXTEND,AVGSIZE,ALIGN,ARENAHIGH,SLOTHIGH,FIRST),
  "create pool1");
 cdie(mps_pool_create(&pool2,arena,mps_class_mvff(),
  EXTEND,AVGSIZE,ALIGN,ARENAHIGH,SLOTHIGH,FIRST),
  "create pool2");
 cdie(mps_pool_create(&pool3,arena,mps_class_mvff(),
  EXTEND,AVGSIZE,ALIGN,ARENAHIGH,SLOTHIGH,FIRST),
  "create pool3");

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   pool = (size < SPLIT1) ? pool1 : (size < SPLIT2) ? pool2 : pool3;
   die(mps_alloc(&objs[id], pool, size), "alloc");
/*
   comment("%d %ld %p", id, size, objs[id]);
*/
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
