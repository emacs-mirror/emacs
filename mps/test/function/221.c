/* 
TEST_HEADER
 id = $HopeName$
 summary = MV2*3 from af_six measuring fragmentation
 language = c
 link = testlib.o
 stdin = af_six
 parameters = COMLIMIT=(1024*1024*100) MIN1=8 MIN2=256 MIN3=4096 MAX3=32768 LIMIT FRAGLIMIT
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
mps_pool_t pools[MAXOBJS];
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
 mps_pool_t pool1, pool2, pool3;
 mps_ap_t ap1, ap2, ap3, ap;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;
 unsigned long depth1, depth2, depth3;
 size_t avg1, avg2, avg3;
 mps_res_t res;

 maxcom = 0;
 fragacc = 0;
 commacc = 0;
 sizeacc = 0;
 count = 0;

 avg1 = MIN1;
 avg2 = MIN2;
 avg3 = MIN3;

 depth1 = LIMIT*2*(MIN2/avg1)*(100/FRAGLIMIT);
 depth2 = LIMIT*2*(MIN3/avg2)*(100/FRAGLIMIT);
 depth3 = LIMIT*2*(MAX3/avg3)*(100/FRAGLIMIT);

 report("avg1", "%ld", avg1);
 report("depth1", "%ld", depth1);
 report("avg2", "%ld", avg2);
 report("depth2", "%ld", depth2);
 report("avg3", "%ld", avg3);
 report("depth3", "%ld", depth3);

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_arena_commit_limit_set(arena, COMLIMIT), "commit limit");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&pool1, arena, mps_class_mv2(),
                  MIN1, avg1, MIN2,
                  depth1,
                  FRAGLIMIT),
  "create MV2 pool (small)");
 cdie(mps_ap_create(&ap1, pool1, MPS_RANK_AMBIG), "create ap (small)");

 cdie(
  mps_pool_create(&pool2, arena, mps_class_mv2(),
                  MIN2, avg2, MIN3,
                  depth2,
                  FRAGLIMIT),
  "create MV2 pool (medium)");
 cdie(mps_ap_create(&ap2, pool2, MPS_RANK_AMBIG), "create ap (medium)");

 cdie(
  mps_pool_create(&pool3, arena, mps_class_mv2(),
                  MIN3, avg3, MAX3,
                  depth3,
                  FRAGLIMIT),
  "create MV2 pool (big)");
 cdie(mps_ap_create(&ap3, pool3, MPS_RANK_AMBIG), "create ap (big)");

 committed = mps_arena_committed(arena);

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   sizes[id] = size;
   size = ((size+7)/8)*8;
   if (size < MIN2) {
    ap = ap1;
    pools[id] = pool1;
   } else if (size < MIN3) {
    ap = ap2;
    pools[id] = pool2;
   } else {
    ap = ap3;
    pools[id] = pool3;
   }
   do {
    MPS_RESERVE_BLOCK(res, objs[id], ap, size);
    asserts(res == MPS_RES_OK, "alloc failed");
   } while (!mps_commit(ap, objs[id], size));
   total_size+=sizes[id];
   c = objs[id];
   *c = 43;
  } else if (event.type == EVENT_FREE) {
   id = event.free.id;
   mps_free(pools[id], objs[id], sizes[id]);
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
