/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!87.c(trunk.2) $
 summary = EPVM make sure objects are left intact
 language = c
 link = testlib.o epvmfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"

#define MAX_SAVE 1000 
#define INIT_SAVE 12

void *stackpointer;

#define MAXOBJS 5000

mps_addr_t addrs[MAXOBJS];
size_t sizes[MAXOBJS] = {0};
mps_epvm_save_level_t levels[MAXOBJS];

mps_pool_t pool1;

mps_epvm_save_level_t lev1;

mps_ap_t ap1s;

static void myrestore(mps_epvm_save_level_t i) {
 int j;

 comment("restore to %i", i);

 mps_epvm_restore(pool1, i);
 for (j=0; j<MAXOBJS; j++) {
  if (levels[j] > i) {
   comment("free %i", j);
   sizes[j] = (size_t) 0;
  }
 }
}


static void mycheck(psobj *addr, size_t size) {
 int i;
 unsigned long ob, om, pb, pm;

 pb = (unsigned long) addr;
 pm = pb+size;

 for (i=0; i<MAXOBJS; i++) {
  if (sizes[i] != 0) {
   ob = (unsigned long) (addrs[i]);
   om = ob + sizes[i];
   asserts(om <= pb || ob >= pm,
    "overlapping objects: %p, %p", addr, addrs[i]);
  }
 }
}


static void myalloc(int i) {
 size_t s;
 psobj *a;

 s = 8 * (ranint(50)+1);
 a = allocepvm(ap1s, s);

 comment("alloc %i at %p level %i", i, a, lev1);

 mycheck(a, s);

 addrs[i] = a;
 sizes[i] = s;
 levels[i] = lev1;
}


static void mysave(void) {
 if (lev1 < MAX_SAVE) {
  lev1++;
  comment("save to %i", lev1);
  mps_epvm_save(pool1);
 }
}

static int myfindblank(int *ii) {
 int j;

 for (j=0; j<MAXOBJS; j++) {
  if (sizes[j]==0) {
   *ii = j;
   return 1;
  }
 }
 return 0;
}

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;

 int i, j;

/* create an arena that can't grow beyond 64M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*64)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root, arena, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(mps_fmt_create_A(&format, arena, &fmtepvm), "create format");

 cdie(mps_pool_create(&pool1, arena, mps_class_epvm(),
   format, MAX_SAVE, INIT_SAVE), "create pool1");
 
 cdie(mps_ap_create(&ap1s, pool1, 0), "create ap1s");

 lev1 = INIT_SAVE;

 for (j=0; j<100000; j++) {
  if (myfindblank(&i)) {
   myalloc(i);
  } else {
   lev1--;
   while(lev1>0 && ranint(100)!=0) {
    lev1--;
   }
   myrestore(lev1);
   if (lev1 == 0) {
    mysave();
   }
  }
  if (ranint(100)<20) {
   mysave();
  }
 }

 mps_ap_destroy(ap1s);
 mps_pool_destroy(pool1);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena");

}


int main(void) {
 void *m;
 stackpointer=&m;

 easy_tramp(test);
 pass();
 return 0;
}
