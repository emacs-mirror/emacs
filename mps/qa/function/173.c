/* 
TEST_HEADER
 id = $HopeName$
 harness = 3.0
 summary = Get the last page in a chunk into the hysteresis fund
 language = c
 link = testlib.o
 parameters = CHUNK_SIZE=(1024*1024) OBJ_FROM=(1024*1024*2) \
              OBJ_TO=(1024*1024*3)
OUTPUT_SPEC
 completed = yes
END_HEADER
*/

/* This test was written to provoke the assertion in
   request.epcore.160256 (Assertion failure while purging the
   hysteresis fund.)

   drj suggested that the assertion would be provoked if we
   arranged for an arena chunk to have less than one whole
   page description on the last page occupied by the page table,
   arranged for the page corresponding to this page description
   to be in the hysteresis fund, and then purged the fund.

   This test attempts to cause this situation by creating an arena
   with a small chunk size, allocating an object that's bigger
   than the chunk size (hence causing a new chunk to be created),
   freeing it, then destroying the arena again. We repeat with
   different object sizes, until (we hope) eventually there is
   only a partial page descriptor in the last page occupied by
   the page table.

   We use an EPDR pool to increase the chance of the object covering
   the last page (but it's a VM arena so we can't be sure what zones
   will be used). Of course, the test is sensitive to the algorithm
   by which the arena decides how big to make the new chunk.
*/

#include "testlib.h"
#include "mpsavm.h"
#include "mpscepdl.h"

mps_arena_t arena;
mps_thr_t thread;
mps_root_t root;

static void test(void)
{
 mps_pool_t pool;
 mps_addr_t a;
 size_t objsize;

 for (objsize = OBJ_FROM; objsize < OBJ_TO; objsize += 4096) {

  report("objsize", "%d", objsize);

  die(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) CHUNK_SIZE),
   "create arena with 1MB chunk size");
  die(mps_thread_reg(&thread, arena), "register thread");
  die(mps_pool_create(&pool, arena, mps_class_epdr(), 65536, 32, 8),
   "create EPDR pool");

  mps_arena_spare_commit_limit_set(arena, (size_t) OBJ_TO);

  die(mps_alloc(&a, pool, objsize), "alloc");
  mps_free(pool, a, objsize);

  mps_pool_destroy(pool);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);
 }
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}
