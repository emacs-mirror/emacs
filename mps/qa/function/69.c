/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!69.c(trunk.5) $
 summary = request.dylan.170563 (colour invariant and finalization)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "rankfmt.h"

void *stackpointer;

mps_space_t space;


static void test(void) {
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_addr_t ref;
 mps_message_t message;
 mps_ap_t ap;

 mycell *a, *b;
 tag myTag;

 long int j;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 mps_message_type_enable(space, mps_message_type_finalization());

 b = allocone(ap, 400, 1);

 for (j=0; j<1000; j++) {
  a = allocone(ap, 400, 1);
  setref(a, 0, b);
  b = a;
 }

 a = allocone(ap, 2, 1);
 mps_finalize(space, (mps_addr_t*)&a);
 myTag = a->tag;

 a = allocone(ap, 4000, 1);
 mps_arena_collect(space);

 if (mps_message_get(&message, space, MPS_MESSAGE_TYPE_FINALIZATION)) {
 } else {
  error("No message on queue!");
 }

 for (j=0; j<50; j++) {
  comment("%d of 50", j);
  a = allocdumb(ap, 1024*1024*10, 1);
  mps_message_finalization_ref(&ref, space, message);
  mps_arena_park(space);
  a = ref;
  comment("                   %p", a);
  asserts(a->tag == myTag, "Bad reference!");
  a = NULL;
  ref = NULL;
  mps_arena_release(space);
 }

 mps_message_discard(space, message);

 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}


int main(void) {
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
