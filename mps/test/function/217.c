/* 
TEST_HEADER
 id = $HopeName$
 summary = MVFF allocate from SW log (af_six)
 language = c
 link = testlib.o
 stdin = af_six
 parameters = EXTENDBY=65536 AVGSIZE=128 ALIGN=8 ARENAHIGH=1 SLOTHIGH=1 FIRST=1
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
size_t    total_size;
size_t    committed;
int       fragmentation;
int       lastfrag=0;

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 log_event event;
 int id;
 char *c;
 size_t size;
 unsigned long maxcom;

 maxcom = 0;
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&pool, arena, mps_class_mvff(),
                  EXTENDBY, AVGSIZE, ALIGN, ARENAHIGH, SLOTHIGH, FIRST),
  "create MVFF pool");

 committed = mps_arena_committed(arena);

 while (read_event(&event)) {
  if (event.type == EVENT_ALLOC) {
   id = event.alloc.id;
   asserts(id < MAXOBJS, "MAXOBJS too small");
   size = event.alloc.size;
   sizes[id] = size;
   size = ((size+7)/8)*8;
   die(mps_alloc(&objs[id], pool, size), "alloc");

   if (mps_arena_committed(arena) > committed) {
    fragmentation = (100*(committed-total_size)/committed);
    if (fragmentation != lastfrag) {
     lastfrag = fragmentation;
     report("fragmentation", "%d", fragmentation);
    }
   }
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
