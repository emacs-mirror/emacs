/* djban.c -- "DJ" Benchmark on ANSI C library
 * $Id$
 */

#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#define DJRUN dj_malloc
#define ALLOC(p, s) do { p = malloc(s); } while(0)
#define FREE(p, s)  do { free(p); } while(0)
#include "djb.c"
#undef DJRUN
#undef ALLOC
#undef FREE

#include "mps.c"

static mps_arena_t arena;
static mps_pool_t pool;
static mps_ap_t ap;

#define DJRUN dj_alloc
#define ALLOC(p, s) do { mps_alloc(&p, pool, s); } while(0)
#define FREE(p, s)  do { mps_free(pool, p, s); } while(0)
#include "djb.c"
#undef DJRUN
#undef ALLOC
#undef FREE

#define ALIGN_UP(s, a) (((s) + ((a) - 1)) & ~((a) - 1))

#define DJRUN dj_reserve
#define ALLOC(p, s) \
  do { \
    size_t _s = ALIGN_UP(s, (size_t)MPS_PF_ALIGN); \
    mps_reserve(&p, ap, _s); \
    mps_commit(ap, p, _s); \
  } while(0)
#define FREE(p, s)  do { mps_free(pool, p, s); } while(0)
#include "djb.c"
#undef DJRUN
#undef ALLOC
#undef FREE

#define MUST(expr) \
  do { \
    mps_res_t res = (expr); \
    if (res != MPS_RES_OK) { \
      fprintf(stderr, #expr " returned %d\n", res); \
      exit(EXIT_FAILURE); \
    } \
  } while(0)


typedef void (*dj_t)(unsigned, unsigned, unsigned, unsigned, double);

static void wrap(dj_t dj, const char *name)
{
  clock_t start, finish;
  
  start = clock();
  dj(100, 1000, 1000, 18, 0.2);
  finish = clock();
  
  printf("%s: %g\n", name, (double)(finish - start) / CLOCKS_PER_SEC);
}


static void arena_wrap(dj_t dj, mps_class_t pool_class, const char *name)
{
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 256ul * 1024 * 1024); /* FIXME: Why is there no default? */
    MPS_ARGS_DONE(args);
    MUST(mps_arena_create_k(&arena, mps_arena_class_vm(), args));
  } MPS_ARGS_END(args);
  MUST(mps_pool_create_k(&pool, arena, pool_class, mps_args_none));
  MUST(mps_ap_create_k(&ap, pool, mps_args_none));
  wrap(dj, name);
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);
}


int main(void) {
  arena_wrap(dj_reserve, mps_class_mvt(), "mvt");
  arena_wrap(dj_reserve, mps_class_mvff(), "mvff");
  arena_wrap(dj_alloc, mps_class_mv(), "mv");
  wrap(dj_malloc, "an");
  
  return EXIT_SUCCESS;
}
