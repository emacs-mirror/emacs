/* 
TEST_HEADER
 id = $HopeName$
 summary = MV2 allocate from SW log (af_six)
 language = c
 link = testlib.o
 stdin = af_six
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscmv2.h"
#include "mpsavm.h"

#define MAXOBJS (30000)

#define MINSIZE 8
#define AVGSIZE 128
#define MAXSIZE 8192
#define DEPTH 10
#define FRAGLIMIT 100

mps_addr_t objs[MAXOBJS];
size_t    sizes[MAXOBJS];

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_ap_t ap;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;
 mps_res_t res;

 maxcom = 0;
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&pool, arena, mps_class_mv2(),
                  MINSIZE, AVGSIZE, MAXSIZE, DEPTH, FRAGLIMIT),
  "create MV2 pool");

 cdie(mps_ap_create(&ap, pool, MPS_RANK_AMBIG), "create ap");

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   sizes[id] = size;
   size = ((size+7)/8)*8;
   do {
    MPS_RESERVE_BLOCK(res, objs[id], ap, size);
    asserts(res == MPS_RES_OK, "alloc failed");
   } while (!mps_commit(ap, objs[id], size));

   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   mps_free(pool, objs[id], sizes[id]);
  }
  size = mps_arena_committed(arena);
  if (size > maxcom) maxcom=size;
 }
 report("maxcom", "%ld", maxcom);

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
