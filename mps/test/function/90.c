/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!90.c(trunk.2) $
 summary = various EPVM functional tests
 language = c
 link = testlib.o epvmfmt.o
END_HEADER
*/

/* This test used to use ambiguous roots, but then EPVM was changed
   so as not to "support" them.
*/

#include "testlib.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"

#define MAX_SAVE 32 
#define INIT_SAVE 4 

#define RT_SIZE 1000

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;

 mps_pool_t pool1;
 mps_ap_t ap1s, ap1p;
 
 unsigned int i, j, k;

 psobj *a[RT_SIZE], *b;

/* create an arena that can't grow beyond 64M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*64)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(&root, arena, MPS_RANK_EXACT, 0, &a[0], RT_SIZE),
  "create root");

 cdie(mps_fmt_create_A(&format, arena, &fmtepvm), "create format");

 cdie(mps_pool_create(&pool1, arena, mps_class_epvm(),
   format, MAX_SAVE, INIT_SAVE), "create pool1");
 
 cdie(mps_ap_create(&ap1s, pool1, 0), "create ap1s");

 cdie(mps_ap_create(&ap1p, pool1, 1), "create ap1p");

 asserts(sizeof(unsigned char) == 1, "surprise! -- characters too big!");

 for (k=0; k<20; k++) {
  comment("%i of 20", k);

  comment("allocate string objects of various sizes");

  for (i=0; i<256; i++) {
   a[i] = allocepvm(ap1s, (size_t) (8+8*i));
   for (j=0; j<(8+i*8); j++) {
    *((unsigned char *) a[i]+j) = i;
   }
  }

  comment("allocate psobjects pointing to strings");

  for (i=0; i<256; i++) {
   b = allocepvm(ap1p, (size_t) 8);
   b->obj = a[i];
   b->size = i+1;
   a[i] = b;
  }

  comment("collect");

  mps_epvm_collect(pool1);

  comment("check objects are ok");

  for (i=0; i<256; i++) {
   b = a[i]->obj;
   asserts(a[i]->size == i+1, "object %i wrong size", i);
   for (j=0; j<(8+i*8); j++) {
    asserts(*((unsigned char *) b+j) == i, "object %i corrupt at %i", i, j);
   }
  }
  comment("finished");
 }

 mps_ap_destroy(ap1s);
 mps_ap_destroy(ap1p);
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



