/* 
TEST_HEADER
 id = $Id$
 summary = spare_commit_limit tests
 language = c
 link = testlib.o rankfmt.o
 harness = 2.1
 parameters = EXTEND=65536 AVGSIZE=32 BIGSIZE=(5*1024*1024);
OUTPUT_SPEC
 completed = yes
 failed = no
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"


#define MVFF_HI_PARMS EXTEND,AVGSIZE,MPS_PF_ALIGN,1,1,0
#define MVFF_LO_PARMS EXTEND,AVGSIZE,MPS_PF_ALIGN,0,0,1


enum {
 SPARE_EMPTY,
 SPARE_LESS,
 SPARE_EXACT,
 SPARE_MORE
};

enum {
 COMMIT_EXACT,
 COMMIT_NOCHANGE,
 COMMIT_LITTLE,
 COMMIT_PLENTY
};

enum {
 OBJ_SMALL,
 OBJ_BIG
};


mps_arena_t arena;
mps_pool_t poollo, poolhi;
mps_addr_t objlo, objhi;


#define SMALL_SIZE 4096
#define BIG_SIZE   (1024ul*1024ul*10)

#define DIFF_SIZE  65536
#define HUGE (size_t)(1024ul*1024ul*100)

#define SPARE_LIMIT HUGE
#define SPARE_ZERO 0


static void t_alloc(int spare, int spare_total, int commit, int obj_size) { 
 size_t size, hisize, comsize, comlimit;
 size_t spsize = 0, losize = 0; /* stop warnings */
 mps_res_t res, res_expected;

 if (obj_size == OBJ_SMALL) size = SMALL_SIZE; else size = BIG_SIZE;

 switch (spare_total) {
 case SPARE_EMPTY:
  spsize = 0;
  break;
 case SPARE_LESS:
  if (size > DIFF_SIZE) {
   spsize = size-DIFF_SIZE;
  } else {
   spsize = 0;
  }
  break;
 case SPARE_EXACT:
  spsize = size;
  break;
 case SPARE_MORE:
  spsize = size+DIFF_SIZE;
  break;
 default:
  error("Illegal spare.\n");
  break;
 }

 switch (spare) {
 case SPARE_EMPTY:
  losize = 0;
  break;
 case SPARE_LESS:
  if (size > DIFF_SIZE) {
   losize = size-DIFF_SIZE;
  } else {
   losize = 0;
  }
  break;
 case SPARE_EXACT:
  losize = size;
  break;
 case SPARE_MORE:
  losize = size+DIFF_SIZE;
  break;
 }

 if (losize > spsize) {
  losize = spsize;
  hisize = 0;
 } else {
  hisize = spsize-losize;
 }

 /* turn off commit limit for a moment */
 mps_arena_commit_limit_set(arena, HUGE);

 /* create low and high pools */
 
 die(
  mps_pool_create(&poolhi, arena, mps_class_mvff(), MVFF_HI_PARMS),
  "create high pool");

 die(
  mps_pool_create(&poollo, arena, mps_class_mvff(), MVFF_LO_PARMS),
  "create low pool");

 /* flush hysteresis fund, then set limit */

 mps_arena_spare_commit_limit_set(arena, SPARE_ZERO);
 mps_arena_spare_commit_limit_set(arena, SPARE_LIMIT);

 /* allocate something in each pool (to reduce risk of subsidiary
    allocation being neede later */

 die(mps_alloc(&objlo, poollo, EXTEND), "low alloc");
 die(mps_alloc(&objhi, poolhi, EXTEND), "high alloc");

 /* set up spare committed the way we want it */

 if (losize>0) {
  die(mps_alloc(&objlo, poollo, losize), "low setup");
  mps_free(poollo, objlo, losize);
 }

 if (hisize>0) {
  die(mps_alloc(&objhi, poolhi, hisize), "high setup");
  mps_free(poolhi, objhi, hisize);
 }

 /* spare is now set up correctly */
 /* now we need to set the commit limit correctly */

 comsize = arena_committed_and_used(arena);

 /* allow for 1/16th memory overhead in setting commit limit */

 if (commit == COMMIT_EXACT) {
  comlimit = comsize+size+(size/16);
 } else if (commit == COMMIT_NOCHANGE) {
  comlimit = mps_arena_committed(arena);
 } else if (commit == COMMIT_PLENTY) {
  comlimit = HUGE;
 } else /* commit == COMMIT_LITTLE */ {
  if (size > DIFF_SIZE) {
   comlimit = comsize+size+(size/16)+DIFF_SIZE;
  } else {
   comlimit = comsize+size+(size/16);
  }
 }

 die(mps_arena_commit_limit_set(arena, comlimit), "commit limit set");

 res = mps_alloc(&objlo, poollo, size);

 asserts(comlimit >= comsize, "comlimit was less than comsize!");

 if (size <= (comlimit-comsize)) {
  res_expected = MPS_RES_OK;
 } else {
  res_expected = MPS_RES_COMMIT_LIMIT;
 }

 if (res != res_expected) {
  comment("Spare useful/total %i/%i. Limit %i. Size %i. Expected %s. Got %s", spare, spare_total, commit, obj_size, err_text(res_expected), err_text(res));
  report("failed", "yes");
 }

 mps_pool_destroy(poollo);
 mps_pool_destroy(poolhi);
}

static void test(void)
{
 mps_thr_t thread;
 int spare, spare_total, commit, obj;

 /* create a VM arena of 100MB */

 cdie(mps_arena_create(&arena,mps_arena_class_vmnz(),(size_t)(1024*1024*100)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 report("failed", "no");

 for (spare = SPARE_EMPTY; spare <= SPARE_MORE; spare++) {
  for (spare_total = spare; spare_total <= SPARE_MORE; spare_total++) {
   for (commit = COMMIT_EXACT; commit <= COMMIT_PLENTY; commit++) {
    for (obj = OBJ_SMALL; obj <= OBJ_BIG; obj++) {

     t_alloc(spare, spare_total, commit, obj);
 }}}}

 comment("Finishing off.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}
