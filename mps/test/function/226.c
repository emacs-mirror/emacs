/* 
TEST_HEADER
 id = $HopeName$
 summary = test of mps_ld_merge
 language = c
 link = testlib.o rankfmt.o
 harness = 3.0
 parameters = MAXLDS=1000 MAXMERGE=100 BLATPERCENT=90 JUNK=100 AMBIGHOLD=900
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscmv.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

void *stackpointer;

mps_arena_t arena;
static mycell *obj_table[MAXLDS];
static mycell *addr_table[MAXLDS];
static mps_ld_t lds[MAXLDS];
static mps_ld_t ldm[MAXLDS];

static int checklds(int merge) {
 int i, j, k, thisstale, stalecount;
 stalecount = 0;
 for (i=0; i < MAXLDS; i++) {
  thisstale = 0;
  for (j=0; j < merge; j++) {
   k = (i+j) % MAXLDS;
   if (obj_table[k] != addr_table[k]) {
    thisstale = 1;
    asserts(mps_ld_isstale(ldm[i], arena, (mps_addr_t) obj_table[k]),
     "%d isn't stale but should be", i);
   }
  }
  stalecount += thisstale;
 }
 return stalecount;
}

static void mergelds(int merge) {
 int i, j, k;
 for (i=0; i < MAXLDS; i++) {
  mps_ld_reset(ldm[i], arena);
  for (j=0; j < merge; j++) {
   k = (i+j) % MAXLDS;
   mps_ld_merge(ldm[i], arena, lds[k]);
  }
 }
}

static void test(void) {
 mps_pool_t poolmv, poolawl, poolamc;
 mps_thr_t thread;
 mps_root_t root0, root1, root2;
 mps_addr_t p;

 mps_fmt_t format;
 mps_ap_t apawl, apamc;

 mycell *a;

 int i,j,merge,stale,prevstale;

 mycell *held[AMBIGHOLD];

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), 100*1024*1024),
  "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(
  mps_root_create_table(&root0, arena, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");
 cdie(
  mps_root_create_table(&root2, arena, MPS_RANK_EXACT, 0,
                        (mps_addr_t *)obj_table, MAXLDS),
  "create table root");
 cdie(
  mps_root_create_reg(&root1, arena, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create register and stack root");
 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");
 cdie(
  mps_pool_create(&poolawl, arena, mps_class_awl(), format),
  "create awl pool");

 cdie(
  mps_pool_create(&poolmv, arena, mps_class_mv(), 0x4000, 128, 0x4000),
  "create mv pool");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_EXACT),
  "create ap");

 cdie(
  mps_pool_create(&poolamc, arena, mps_class_amc(), format),
  "create amc pool");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

/* allocate MAXLDS objects, and make each LD depend on the corresponding
   object
*/

 for (i=0; i < MAXLDS; i++) {
  mps_alloc(&p, poolmv, sizeof(mps_ld_s));
  lds[i] = (mps_ld_t) p;
  mps_alloc(&p, poolmv, sizeof(mps_ld_s));
  ldm[i] = (mps_ld_t) p;
 }

 for (i=0; i < MAXLDS; i++) {
  obj_table[i] = allocone(apamc, ranint(1000), MPS_RANK_EXACT);
  mps_ld_reset(lds[i], arena);
  mps_ld_add(lds[i], arena, (mps_addr_t) obj_table[i]);
  addr_table[i] = obj_table[i];
 }

 for (merge = 1; merge <= MAXMERGE; merge++) {
  comment("Merge %d", merge);

  for (i=0; i < MAXLDS; i++) {
   if (ranint(100) < BLATPERCENT) {
    obj_table[i] = allocone(apamc, ranint(1000), MPS_RANK_EXACT);
    mps_ld_reset(lds[i], arena);
    mps_ld_add(lds[i], arena, (mps_addr_t) obj_table[i]);
    addr_table[i] = obj_table[i];
   }
  }

  mergelds(merge);

  stale = 0;
  prevstale = 0;
  i = 0;

  while (stale < MAXLDS) {
   for (j = 0; j < AMBIGHOLD; j++) {
    held[j] = obj_table[ranint(MAXLDS)];
   }
   i++;
   stale = checklds(merge);
   if (stale > prevstale) {
    prevstale = stale;
    comment("inc to %d at %d", stale, i);
   }
   for (j = 0; j < JUNK; j++) {
    a = allocdumb(apamc, ranint(1000), MPS_RANK_EXACT);
   }
  }
 }

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolmv);
 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 mps_root_destroy(root2);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_space_destroy(arena);
 comment("Destroyed arena.");

}

int main(void) {
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
