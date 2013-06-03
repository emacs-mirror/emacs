/* 
TEST_HEADER
 id = $Id$
 summary = Allocate but never look at objects
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;

static mps_res_t myscan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
 MPS_SCAN_BEGIN(ss)
 MPS_SCAN_END(ss);
 return MPS_RES_OK;
}

static mps_addr_t myskip(mps_addr_t object)
{
 return (mps_addr_t) ((char *) object + 1);
}

static void mycopy(mps_addr_t object, mps_addr_t to)
{
}

static void mypad(mps_addr_t base, size_t size)
{
}

static mps_addr_t myisfwd(mps_addr_t object)
{
 return NULL;
}

static void myfwd(mps_addr_t object, mps_addr_t to)
{
}

struct mps_fmt_A_s fmtA =
{
 1,
 &myscan,
 &myskip,
 &mycopy,
 &myfwd,
 &myisfwd,
 &mypad
};


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t ap;

 mps_addr_t p;

 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
      mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 for(i=0; i<1000; i++)
 {
  do
  { die(mps_reserve(&p, ap, 1024*1024), "Reserve: ");
  }
  while (!mps_commit(ap, p, 1024*1024));
  comment("%i megabytes allocated", i);
 }

 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_fmt_destroy(format);
 mps_chain_destroy(chain);
 mps_root_destroy(root);
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

