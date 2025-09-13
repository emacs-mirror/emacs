/* 
TEST_HEADER
 id = $Id$
 summary = test malloc and free (for the manual)
 language = c
 link = testlib.o
 parameters = POINTERS=1000 ITERATIONS=10000
END_HEADER
*/

#include "mpscmvff.h"
#include "testlib.h"

static mps_pool_t malloc_pool;

typedef union {
  size_t size;
  char alignment[MPS_PF_ALIGN];
} header_u;

static void *xmalloc(size_t size)
{
  mps_res_t res;
  mps_addr_t p;
  header_u *header;
  size += sizeof *header;
  res = mps_alloc(&p, malloc_pool, size);
  if (res != MPS_RES_OK)
    return NULL;
  header = p;
  header->size = size;
  return header + 1;
}

static void xfree(void *p)
{
  if (p) {
    header_u *header = ((header_u *)p) - 1;
    mps_free(malloc_pool, header, header->size);
  }
}

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 size_t i, j;
 void *p[POINTERS] = {0};

 cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "create arena");
 cdie(mps_pool_create_k(&malloc_pool, arena, mps_class_mvff(), mps_args_none),
      "create pool");

 for (i = 0; i < ITERATIONS; ++i) {
   j = ranint(POINTERS);
   xfree(p[j]);
   p[j] = xmalloc(ranint(POINTERS));
 }
 for (j = 0; j < POINTERS; ++j) {
   xfree(p[j]);
 }
 asserts(mps_pool_free_size(malloc_pool) == mps_pool_total_size(malloc_pool),
         "free size != total_size");

 mps_pool_destroy(malloc_pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}
