/* 
TEST_HEADER
 id = $HopeName$
 summary = MV SAC allocate from SW log (af_six)
 language = c
 link = testlib.o
 stdin = af_six
 parameters = EXTEND=65536 AVGSIZE=32 MAXSIZE=256*1024 \
              SPLIT1=128 COUNT1=64 FREQ1=1 SPLIT2=1024 COUNT2=64 FREQ2=2
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpsavm.h"

#define MAXOBJS (30000)

mps_addr_t objs[MAXOBJS];
size_t    sizes[MAXOBJS];

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_sac_t sac;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;
 mps_res_t res;

 struct mps_sac_classes_s sac_classes[] = {
  { SPLIT1, COUNT1, FREQ1 },
  { SPLIT2, COUNT2, FREQ2 }
 };

 maxcom = 0;
 cdie(mps_arena_create(&arena, mps_arena_class_vmnz(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_pool_create(&pool,arena,mps_class_mv(),EXTEND,AVGSIZE,MAXSIZE),
  "create pool");

 cdie(mps_sac_create(&sac, pool, 2, sac_classes), "sac create");

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   MPS_SAC_ALLOC(res, objs[id], sac, size, 0);
   asserts(res == MPS_RES_OK, "alloc");
   sizes[id] = size;
   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   size = sizes[id];
   MPS_SAC_FREE(sac, objs[id], size);
  }
  size = mps_arena_committed(arena);
  if (size > maxcom) maxcom=size;
 }
 report("maxcom", "%ld", maxcom);

 mps_sac_destroy(sac);
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
