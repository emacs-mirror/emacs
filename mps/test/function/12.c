/* 
TEST_HEADER
 id = $Id$
 summary = lots of APs with interleaved reserve and 2-stage commit
 language = c
 link = testlib.o newfmt.o
 parameters = VERBOSE=0 NCELLS=100 NAPS=100 ITERATIONS=50
END_HEADER
*/

/*
   This test needs some explanation. The object 'cells' contains
   NCELLS references to other objects. NAPS allocation points
   are created. At each step, we choose one and nudge it on
   a little. Each allocation point goes through the following
   cycle of steps: reserve, init, begin commit, end commit.
   At the init step, each reference in the reserved object is
   set to point randomly to one of the objects referenced by
   cells or (with probability PNULL) to NULL. If commit fails,
   the object is thrown away. If it succeeds, it is written into
   a random reference in cells. Repeat.

   Because it has to deal directly with reserve and commit,
   this test can't use the nice functions provided by newfmt
   all the time.
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"


#define PNULL (ranint(100)<25)
#define NUMREFS (ranint(20))
#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


mps_ap_t ap[NAPS];
mycell *p[NAPS];
size_t s[NAPS];
int nrefs[NAPS];
int ap_state[NAPS];


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;

 mycell *cells;
 int h,i,j,k,l;
 mycell *pobj;

 mycell *ambig[NAPS];

 size_t bytes;
 size_t alignment;
 mps_addr_t q;
 int nextid = 0x1000000;

 /* turn on comments about copying and scanning */
 formatcomments = VERBOSE;
 fixcomments = VERBOSE;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 for (i=0; i<NAPS; i++) {
  die(mps_ap_create(&ap[i], pool, mps_rank_exact()), "create ap");
  ap_state[i] = 0;
 }

 cells = allocone(ap[0], NCELLS);

 /* ap_state can have the following values:
  0 before reserve
  1 after reverse
  2 after init
  3 after I=A
  0 after commit
 */

 for(h=0; h<ITERATIONS; h++) {
   comment("%i of %i", h, ITERATIONS);

   for(j=0; j<1000; j++) {
     if (j == 500) {
       mps_arena_collect(arena);
     }
     i = ranint(NAPS);

     switch (ap_state[i]) {
     case 0:
       nrefs[i] = NUMREFS;
       bytes = offsetof(struct data, ref)+nrefs[i]*sizeof(struct refitem);
       alignment = MPS_PF_ALIGN;
       bytes = (bytes+alignment-1)&~(alignment-1);
       s[i] = bytes;
       die(mps_reserve(&q, ap[i], s[i]), "reserve: ");
       p[i] = q;
       p[i]->data.tag = 0xD033E2A6;
       p[i]->data.id = nextid;
       ap_state[i] = 1;
       commentif(VERBOSE, "%i: reserve %li at %p", i, nextid, q);
       nextid +=1;
       break;
     case 1:
       commentif(VERBOSE, "%i: init %li", i, p[i]->data.id);
       p[i]->data.tag = MCdata;
       p[i]->data.numrefs = nrefs[i];
       p[i]->data.size = s[i];
       ap_state[i] = 2;
       for (k=0; k<nrefs[i]; k++) {
         if PNULL {
           p[i]->data.ref[k].addr = NULL;
           p[i]->data.ref[k].id   = 0;
         } else {
           l = ranint(NCELLS);
           pobj = getref(cells, l);
           p[i]->data.ref[k].addr = pobj;
           p[i]->data.ref[k].id = (pobj==NULL ? 0 : pobj->data.id);
         }
         commentif(VERBOSE, "    ref %i -> %li", k, p[i]->data.ref[k].id);
       }
       break;
     case 2:
       commentif(VERBOSE, "%i: begin commit %li", i, p[i]->data.id);
       ambig[i] = p[i];
       ap[i]->init = ap[i]->alloc;
       ap_state[i] = 3;
       break;
     case 3:
       commentif(VERBOSE, "%i: end commit %li", i, p[i]->data.id);
       q = p[i];
       if (ap[i]->limit != 0 || mps_ap_trip(ap[i], p[i], s[i])) {
         l = ranint(NCELLS);
         setref(cells, l, q);
         commentif(VERBOSE, "%i -> %i", i, l);
       }
       ap_state[i] = 0;
       ambig[i] = NULL;
       break;
     }
   }
   checkfrom(cells);
 }

 comment("ambig[i] = %p", ambig[i]); /* stop compiler optimizing ambig away */
 comment("Finished main loop");

 for (i=0; i<NAPS; i++) {
  switch (ap_state[i]) {
   case 1:
    commentif(VERBOSE, "%i init", i);
    p[i]->data.tag = MCdata;
    p[i]->data.numrefs = 0;
    p[i]->data.size = s[i];
   case 2:
    commentif(VERBOSE, "%i begin commit", i);
    ap[i]->init = ap[i]->alloc;
   case 3:
    commentif(VERBOSE, "%i end commit", i);
    (void) (ap[i]->limit != 0 || mps_ap_trip(ap[i], p[i], s[i]));
  }
  mps_ap_destroy(ap[i]);
 }

 mps_arena_park(arena);
 mps_pool_destroy(pool);
 comment("Destroyed pool.");
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
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
