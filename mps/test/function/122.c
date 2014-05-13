/* 
TEST_HEADER
 id = $Id$
 summary = test of mps_arena_roots_walk
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 count1 = 6
 countspec > 0
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscawl.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

#define MAGICSIZE (342)


void *stackpointer;
long int rootcount;
long int speccount;

int oldstamp, newstamp;

mps_arena_t arena;
mps_pool_t poolamc, poollo, poolawl;
mps_thr_t thread;
mps_root_t root, root1;

mps_fmt_t format;
mps_ap_t apamc, aplo, apawl;

/* root is the stack root */
/* root1, root2, table roots */
mps_root_t root, root1, root2;


static void root_step(mps_addr_t* ref, mps_root_t r, void *V, size_t S)
{
 mycell *a;
 mycell *spec;

 spec = (mycell *) V;

 asserts(S == MAGICSIZE, "VII. Size didn't get passed!");
 asserts((r == root || r == root1 || r == root2), "Root didn't get passed!");
 a = *ref;
 comment("root: %p -> %p", ref, a);
 rootcount++;
 if (spec == a) {
  speccount++;
 }
 if (r != root) {
  asserts(((a->tag) & 0x3) == MCdata,
          "spurious ref claimed in root at %p->%p", ref, a);
  a->data.checkedflag = newstamp;
 }
}


static void walkroots (mycell *a)
{
 mps_arena_park(arena);
 mps_arena_roots_walk(arena, root_step, (mps_addr_t) a, MAGICSIZE);
 mps_arena_release(arena);
}


/* a is a table of exact roots */
/* b    a table of ambig roots */
mycell *a[4], *b[4];


static void test(void)
{
 mps_chain_t chain;
 mycell *w, *x, *y;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) 1024*1024*30),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(
  mps_root_create_table_masked(&root1, arena, mps_rank_exact(), 0,
                               (mps_addr_t*)&a[0], 4, 0x4),
  "create a root table");

 cdie(
  mps_root_create_table(&root2, arena, mps_rank_ambig(), 0, (mps_addr_t*)&b[0], 4),
  "create b root table");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool");

 die(mmqa_pool_create_chain(&poollo, arena, mps_class_amcz(), format, chain),
     "create pool");

 cdie(
  mps_pool_create(&poolawl, arena, mps_class_awl(), format, getassociated),
  "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&aplo, poollo, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&apawl, poolawl, mps_rank_exact()),
  "create ap");

 newstamp = 0;
 alloccomments = 1;

 die(allocrdumb(&a[0], aplo, 64, mps_rank_exact()), "alloc");
 die(allocrdumb(&a[1], apamc, 64, mps_rank_exact()), "alloc");
 die(allocrdumb(&a[3], apawl, 64, mps_rank_exact()), "alloc");
 a[2] = (mycell *)((mps_word_t)a[3] | 4);

 die(allocrdumb(&b[0], aplo, 64, mps_rank_exact()), "alloc");
 die(allocrdumb(&b[1], apamc, 64, mps_rank_exact()), "alloc");
 b[2] = NULL;
 die(allocrdumb(&b[3], apawl, 64, mps_rank_exact()), "alloc");

 rootcount = 0;
 walkroots(NULL);
 report("count1", "%ld", rootcount);
 
 cdie(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
  mps_stack_scan_ambig, stackpointer, 0), "create stack root");

 x = allocdumb(apamc, 64, mps_rank_exact());
 y = allocdumb(apamc, 64, mps_rank_exact());
 w = allocdumb(apamc, 64, mps_rank_exact());
 rootcount = 0;
 speccount = 0;
 walkroots(x);
 report("count2", "%ld", rootcount);
 report("countspec", "%ld", speccount);

 mps_arena_park(arena);
 mps_ap_destroy(apamc);
 mps_ap_destroy(aplo);
 mps_ap_destroy(apawl);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poollo);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_chain_destroy(chain);

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root2);
 mps_root_destroy(root1);
 mps_root_destroy(root);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 report("result", "pass");
 return 0;
}
