/* 
TEST_HEADER
 id = $HopeName$
 summary = MV2 from af_six measuring fragmentation
 language = c
 link = testlib.o
 stdin = af_six
 parameters = COMLIMIT=(1024*1024*100) MINSIZE=8 AVGSIZE=1024 MAXSIZE=65536 LIMIT FRAGLIMIT
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscmv2.h"
#include "mpsavm.h"

#define MAXOBJS (30000)

mps_addr_t objs[MAXOBJS];
size_t    sizes[MAXOBJS];
size_t    total_size;
size_t    committed;
int       fragmentation;

double fragacc;
double commacc;
double sizeacc;
double count;

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
 unsigned long depth;
 mps_res_t res;

 maxcom = 0;
 fragacc = 0;
 commacc = 0;
 sizeacc = 0;
 count = 0;
 depth = LIMIT*2*(MAXSIZE/AVGSIZE)*(100/FRAGLIMIT);
 report("depth", "%ld", depth);

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_arena_commit_limit_set(arena, COMLIMIT), "commit limit");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&pool, arena, mps_class_mv2(),
                  MINSIZE, AVGSIZE, MAXSIZE,
                  depth,
                  FRAGLIMIT),
  "create MV2 pool");

 cdie(mps_ap_create(&ap, pool, MPS_RANK_AMBIG), "create ap");

 committed = mps_arena_committed(arena);

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
   total_size+=sizes[id];
   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   mps_free(pool, objs[id], sizes[id]);
   total_size-=sizes[id];
  }
  committed = mps_arena_committed(arena);
  if (committed > maxcom) maxcom=committed;
  fragacc += ((double) (committed-total_size))/((double) committed);
  commacc += (double) committed;
  sizeacc += (double) total_size;
  count += 1;
 }
 report("maxcom", "%ld", maxcom);
 report("fragavg", "%f", fragacc/count);
 report("fragweighted", "%f", (commacc-sizeacc)/commacc);
 report("commavg", "%f", commacc/count);
 report("sizeavg", "%f", sizeacc/count);

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
