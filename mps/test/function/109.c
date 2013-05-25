/* 
TEST_HEADER
 id = $Id$
 summary = finalization tests with AMC, AWL and AMCZ
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;

mps_arena_t arena;

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


static void nq(mps_message_t mess)
{
 mqueue[qhd] = mess;
 qhd = (qhd+1) % 10000;
 asserts(qhd != qtl, "No space in message queue.");
}


static int qmt(void)
{
 if (qhd == qtl) {
  return 1;
 } else {
  return 0;
 }
}


static int dq(mps_message_t *mess)
{
 if (qhd == qtl) {
  return 0;
 } else {
  *mess = mqueue[qtl];
  qtl = (qtl+1) % 10000;
  return 1;
 } 
}


static void process_mess(mps_message_t message, int faction, mps_addr_t *ref)
{
 mps_addr_t ffref;

 switch (faction) {
  case FINAL_DISCARD:
   mps_message_discard(arena, message);
   break;
  case FINAL_REREGISTER:
   mps_message_finalization_ref(&ffref, arena, message);
   mps_finalize(arena, &ffref);
   final_count +=1;
   mps_message_discard(arena, message);
   break;
  case FINAL_STORE:
   mps_message_finalization_ref(ref, arena, message);
   mps_message_discard(arena, message);
   break;
  case FINAL_QUEUE:
   nq(message);
   break;
  default:
   asserts(0, "Unknown finalization action.");
 }
}


static void qpoll(mycell **ref, int faction)
{
 mps_message_t message;

 if (dq(&message)) {
  process_mess(message, faction, (mps_addr_t*)ref);
 }
}


static void finalpoll(mycell **ref, int faction)
{
 mps_message_t message;

 if (mps_message_get(&message, arena, mps_message_type_finalization())) {
  final_count -=1;
  process_mess(message, faction, (mps_addr_t*)ref);
 }
}


static void test(void)
{
 mps_pool_t poolamc, poolawl, poollo;
 mps_thr_t thread;
 mps_root_t root0, root1;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apamc, apawl, aplo;

 mycell *a, *b, *c, *d, *z;

 long int j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1024*1024*30),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root0, arena, mps_rank_ambig(), 0, thread,
                          mps_stack_scan_ambig, stackpointer, 0),
      "create root");
 
 cdie(mps_root_create_table(&root1, arena, mps_rank_ambig(), 0,
                            (mps_addr_t*)&exfmt_root, 1),
      "create table root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool(amc)");

 cdie(mps_pool_create(&poolawl, arena, mps_class_awl(), format, getassociated),
      "create pool(awl)");

 cdie(mmqa_pool_create_chain(&poollo, arena, mps_class_amcz(), format, chain),
     "create pool(amcz)");

 cdie(mps_ap_create(&apawl, poolawl, mps_rank_weak()),
      "create ap(awl)");

 cdie(mps_ap_create(&apamc, poolamc, mps_rank_exact()),
      "create ap(amc)");
 
 cdie(mps_ap_create(&aplo, poollo, mps_rank_exact()),
      "create ap(amcz)");

 mps_message_type_enable(arena, mps_message_type_finalization());

 /* register loads of objects for finalization (1000*4) */

 a = allocone(apamc, 2, 1);
 b = a;

 for (j=0; j<1000; j++) {
  a = allocone(apamc, 2, mps_rank_exact());
  c = allocone(apawl, 2, mps_rank_weak());
  d = allocone(aplo, 2, mps_rank_exact()); /* rank irrelevant here! */
  mps_finalize(arena, (mps_addr_t*)&a);
  mps_finalize(arena, (mps_addr_t*)&c);
  mps_finalize(arena, (mps_addr_t*)&d);
  mps_finalize(arena, (mps_addr_t*)&d);
  final_count += 4;
  setref(a, 0, b);
  setref(a, 1, c);
  setref(c, 1, d);
  b = a;
 }

 /* throw them all away and collect everything */

 a = NULL;
 b = NULL;
 c = NULL;
 d = NULL;

 mps_arena_collect(arena);

 while (mps_message_poll(arena)) {
  finalpoll(&z, FINAL_DISCARD);
 }

 /* How many are left? (Ideally, this would be 0 but there's no guarantee.) */

 report("count1", "%i", final_count);

 /* now to test leaving messages open for a long time! */

 for (j=0; j<1000; j++) {
  comment("%d of 1000", j);
  a = allocone(apamc, 10000, mps_rank_exact());
  mps_finalize(arena, (mps_addr_t*)&a);
  final_count +=1;
  comment("finalize");
  finalpoll(&z, FINAL_QUEUE);
 }

 comment("reregister");

 for (j=0; j<500; j++) {
  comment("%d of 500", j);
  qpoll(&z, FINAL_REREGISTER);
 }

 b = a;
 z = a;

 for (j=0; j<1000; j++) {
  comment("%d of 1000", j);
  finalpoll(&z, FINAL_QUEUE);
  qpoll(&z, FINAL_STORE);
  a = allocone(apamc, 2, mps_rank_exact());
  setref(z, 0, b);
  setref(a, 1, z);
  b = a;
 }


 for (j=0; j<1000; j++) {
  a = allocone(apamc, 2, mps_rank_exact());
  qpoll(&z, FINAL_DISCARD);
  finalpoll(&z, FINAL_DISCARD);
  setref(a, 0, b);
  b = a;
 }

 /* Force old objects to be killed */

 while (qmt() == 0) {
  qpoll(&z, FINAL_DISCARD);
 }

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_arena_collect(arena);
 comment("Collected arena.");

 while (mps_message_poll(arena)) {
  finalpoll(&z, FINAL_DISCARD);
 }

 report("count2", "%d", final_count);

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 mps_ap_destroy(aplo);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 mps_pool_destroy(poollo);
 comment("Destroyed pools.");

 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}
