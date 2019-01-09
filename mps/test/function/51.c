/* 
TEST_HEADER
 id = $Id$
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
#include "mpscams.h"
#include "mpsclo.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


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


static void finalpoll(mycell **ref, int faction)
{
 mps_message_t message;

 if (mps_message_get(&message, arena, mps_message_type_finalization())) {
  final_count -=1;
  process_mess(message, faction, (mps_addr_t*)ref);
 }
}


static void test(void *stack_pointer)
{
 mps_pool_t poolamc, poolamcz, poolams, poolawl, poollo;
 mps_thr_t thread;
 mps_root_t root0, root1;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apamc, apamcz, apams, apawl, aplo;

 mycell *a, *b, *c, *d, *e, *z;

 long int i,j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1024*1024*30),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root0, arena, thread, stack_pointer), "thread root"); 
 cdie(mps_root_create_table(&root1, arena, mps_rank_ambig(), 0,
                            (mps_addr_t *)&exfmt_root, 1),
      "create table root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool");

 die(mmqa_pool_create_chain(&poolamcz, arena, mps_class_amcz(), format, chain),
     "create pool");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
   MPS_ARGS_ADD(args, MPS_KEY_GEN, 0);
   cdie(mps_pool_create_k(&poolams, arena, mps_class_ams(), args),
        "create pool");
 } MPS_ARGS_END(args);

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
   MPS_ARGS_ADD(args, MPS_KEY_GEN, 0);
   MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, getassociated);
   cdie(mps_pool_create_k(&poolawl, arena, mps_class_awl(), args),
        "create pool");
 } MPS_ARGS_END(args);

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
   MPS_ARGS_ADD(args, MPS_KEY_GEN, 0);
   cdie(mps_pool_create_k(&poollo, arena, mps_class_lo(), args),
        "create pool");
 } MPS_ARGS_END(args);

 cdie(mps_ap_create(&apawl, poolawl, mps_rank_weak()), "create ap");
 cdie(mps_ap_create(&apamc, poolamc, mps_rank_exact()), "create ap");
 cdie(mps_ap_create(&apamcz, poolamcz, mps_rank_exact()), "create ap");
 cdie(mps_ap_create(&apams, poolams, mps_rank_exact()), "create ap");
 cdie(mps_ap_create(&aplo, poollo, mps_rank_exact()), "create ap");

 mps_message_type_enable(arena, mps_message_type_finalization());

 /* register loads of objects for finalization (1000*4) */

 a = allocone(apamc, 2, 1);
 b = a;

 for (j=0; j<1000; j++) {
  a = allocone(apamc, 2, mps_rank_exact());
  b = allocone(apamcz, 2, mps_rank_exact()); /* rank irrelevant here! */
  c = allocone(apams, 2, mps_rank_exact());
  d = allocone(apawl, 2, mps_rank_weak());
  e = allocone(aplo, 2, mps_rank_exact()); /* rank irrelevant here! */
  mps_finalize(arena, (mps_addr_t*)&a);
  mps_finalize(arena, (mps_addr_t*)&b);
  mps_finalize(arena, (mps_addr_t*)&c);
  mps_finalize(arena, (mps_addr_t*)&d);
  mps_finalize(arena, (mps_addr_t*)&e);
  mps_finalize(arena, (mps_addr_t*)&e);
  final_count += 6;
 }

 /* throw them all away and collect everything */

 a = NULL;
 b = NULL;
 c = NULL;
 d = NULL;

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_arena_collect(arena);

 i = 0;

 while (final_count != 0 && i < 10) {
  finalpoll(&z, FINAL_DISCARD);
  if (final_count != 0 && mps_message_poll(arena) == 0) {
   i++;
   a = allocdumb(apamc, 1024, mps_rank_exact());
   a = allocdumb(apamcz, 1024, mps_rank_exact());
   a = allocdumb(apams, 1024, mps_rank_exact());
   a = allocdumb(apawl, 1024, mps_rank_weak());
   a = allocdumb(aplo,  1024, mps_rank_exact());
   mps_arena_collect(arena);
   comment(" %i", final_count);
  }
 }

 /* How many are left? (Ideally, this would be 0 but there's no guarantee.) */

 report("count", "%i", final_count);
 report("iter", "%i", i);

 /* now to test leaving messages open for a long time! */

 mps_arena_park(arena);
 mps_ap_destroy(apamc);
 mps_ap_destroy(apamcz);
 mps_ap_destroy(apams);
 mps_ap_destroy(apawl);
 mps_ap_destroy(aplo);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolamcz);
 mps_pool_destroy(poolams);
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
 run_test(test);
 pass();
 return 0;
}
