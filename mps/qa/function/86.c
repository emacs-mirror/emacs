/* TEST_HEADER
 summary = EPVM error on destroying pools
 language = c
 link = testlib.o epvmfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"

#define MAX_SAVE 20
#define INIT_SAVE 3

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool1, pool2;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap1p, ap1s, ap2p, ap2s;

 psobj *a;

 int i, j;

 mps_epvm_save_level_t lev1, lev2;

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

 cdie(mps_pool_create(&pool2, arena, mps_class_epvm(),
   format, MAX_SAVE, INIT_SAVE), "create pool2");
 
 cdie(mps_ap_create(&ap1p, pool1, 1), "create ap1p");
 cdie(mps_ap_create(&ap1s, pool1, 0), "create ap1s");
 cdie(mps_ap_create(&ap2p, pool2, 1), "create ap2p");
 cdie(mps_ap_create(&ap2s, pool2, 0), "create ap2s");

 lev1 = INIT_SAVE;
 lev2 = INIT_SAVE;

 for (i=0; i<10000; i++) {
  j = ranint(40);
  switch (j)  {
   case 1:
    if (lev1==0) break;
    lev1--;
    comment("Restore 1: %i", lev1);
    while (ranint(2) && (lev1>0)) lev1--;
    mps_epvm_restore(pool1, lev1);
    break;
   case 2:
    if (lev2==0) break;
    lev2--;
    while (ranint(2) && (lev2>0)) lev2--;
    comment("Restore 2: %i", lev2);
    mps_epvm_restore(pool2, lev2);
    break;
   case 3:
   case 4:
    if (lev1<MAX_SAVE) {
     lev1++;
     comment("Save 1 to %i", lev1);
     mps_epvm_save(pool1);
    }
    break;
   case 5:
   case 6:
    if (lev2<MAX_SAVE) {
     lev2++;
     comment("Save 2 to %i", lev2);
     mps_epvm_save(pool2);
    }
    break;
   default:
    if (ranint(2)==0) {
     a = allocepvm(ap1p, 8*(ranint(10)+1));
    } else {
     a = allocepvm(ap2p, 8*(ranint(10)+1));
    }
  }
 }

 mps_ap_destroy(ap1p);
 mps_ap_destroy(ap1s);
 mps_ap_destroy(ap2p);
 mps_ap_destroy(ap2s);
 mps_pool_destroy(pool1);
 mps_pool_destroy(pool2);
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



