/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!111.c(trunk.2) $
 summary = wait until all registered objects are finalized
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 count = 0
 iter  < 4
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "rankfmt.h"


void *stackpointer;

mps_space_t space;

int final_count = 0;


enum {
 FINAL_DISCARD,
 FINAL_REREGISTER,
 FINAL_STORE,
 FINAL_QUEUE
};


mps_message_t mqueue[10000];

int qhd = 0;
int qtl = 0;


static void nq(mps_message_t mess) {
 mqueue[qhd] = mess;
 qhd = (qhd+1) % 10000;
 asserts(qhd != qtl, "No space in message queue.");
}


static void process_mess(mps_message_t message, int faction, mps_addr_t *ref) {
 mps_addr_t ffref;

 switch (faction) {
  case FINAL_DISCARD:
   mps_message_discard(space, message);
   break;
  case FINAL_REREGISTER:
   mps_message_finalization_ref(&ffref, space, message);
   mps_finalize(space, &ffref);
   final_count +=1;
   mps_message_discard(space, message);
   break;
  case FINAL_STORE:
   mps_message_finalization_ref(ref, space, message);
   mps_message_discard(space, message);
   break;
  case FINAL_QUEUE:
   nq(message);
   break;
  default:
   asserts(0, "Unknown finalization action.");
 }
}


static void finalpoll(mycell **ref, int faction) {
 mps_message_t message;

 if (mps_message_get(&message, space, MPS_MESSAGE_TYPE_FINALIZATION)) {
  final_count -=1;
  process_mess(message, faction, (mps_addr_t*)ref);
 }
}


static void test(void) {
 mps_pool_t poolamc, poolawl, poollo;
 mps_thr_t thread;
 mps_root_t root0, root1;

 mps_fmt_t format;
 mps_ap_t apamc, apawl, aplo;

 mycell *a, *b, *c, *d, *z;

 long int i,j;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root0, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");
 
 cdie(
  mps_root_create_table(&root1, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create table root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_pool_create(&poollo, space, mps_class_amcz(), format),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_WEAK),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");
 
 cdie(
  mps_ap_create(&aplo, poollo, MPS_RANK_EXACT),
  "create ap");

 mps_message_type_enable(space, mps_message_type_finalization());

/* register loads of objects for finalization (1000*4) */

 a = allocone(apamc, 2, 1);
 b = a;

 for (j=0; j<1000; j++) {
  a = allocone(apamc, 2, MPS_RANK_EXACT);
  c = allocone(apawl, 2, MPS_RANK_WEAK);
  d = allocone(aplo, 2, MPS_RANK_EXACT); /* rank irrelevant here! */
  mps_finalize(space, (mps_addr_t*)&a);
  mps_finalize(space, (mps_addr_t*)&c);
  mps_finalize(space, (mps_addr_t*)&d);
  mps_finalize(space, (mps_addr_t*)&d);
  final_count += 4;
 }

/* throw them all away and collect everything */

 a = NULL;
 b = NULL;
 c = NULL;
 d = NULL;

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_arena_collect(space);

 i = 0;

 while (final_count != 0 && i < 10) {
  finalpoll(&z, FINAL_DISCARD);
  if (mps_message_poll(space) == 0) {
   i++;
   a = allocdumb(apawl, 1024, MPS_RANK_WEAK);
   a = allocdumb(apamc, 1024, MPS_RANK_EXACT);
   a = allocdumb(aplo,  1024, MPS_RANK_EXACT);
   mps_arena_collect(space);
   comment(" %i", final_count);
  }
 }

/* how many are left? (n.b. ideally this would be 0 but
   there's no guarantee)
*/

 report("count", "%i", final_count);
 report("iter", "%i", i);

/* now to test leaving messages open for a long time! */

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 mps_ap_destroy(aplo);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 mps_pool_destroy(poollo);
 comment("Destroyed pools.");

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
