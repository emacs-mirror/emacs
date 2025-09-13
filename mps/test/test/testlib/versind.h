/* $Id$
 *
 * If you define mps_ symbols here, you also need to list them in
 * MMQA_harness!testlib:assumed.
 */


/* In release.dylan.honeybee, space was renamed arena in lots of functions
   We'll use the "destroy" functions to test which versions are available
*/

#define mmqaArenaSIZE ((size_t)1<<30)

#ifdef MMQA_DEFINED_mps_arena_destroy

#ifdef MMQA_DEFINED_mps_space_destroy

/* bizarrely, both versions seem to be present! */

#else

/* need to define "space" variants */
#define mps_space_clamp(a)     mps_arena_clamp(a)
#define mps_space_release(a)   mps_arena_release(a)
#define mps_space_park(a)      mps_arena_park(a)
#define mps_space_collect(a)   mps_arena_collect(a)
#define mps_space_destroy(a)   mps_arena_destroy(a)
#define mps_space_reserved(a)  mps_arena_reserved(a)
#define mps_space_committed(a) mps_arena_committed(a)
#define mps_space_t mps_arena_t
#include "mpsavm.h"
#define mps_space_create(space) \
  mps_arena_create(space, mps_arena_class_vm(), mmqaArenaSIZE)

#endif

#else

#ifdef MMQA_DEFINED_mps_space_destroy

/* need to define "arena" variants */
#define mps_arena_clamp(a)     mps_space_clamp(a)
#define mps_arena_release(a)   mps_space_release(a)
#define mps_arena_park(a)      mps_space_park(a)
#define mps_arena_collect(a)   mps_space_collect(a)
#define mps_arena_destroy(a)   mps_space_destroy(a)
#define mps_arena_reserved(a)  mps_space_reserved(a)
#define mps_arena_committed(a) mps_space_committed(a)
#define mps_arena_t mps_space_t

#else

/* Neither exists -- error! */
#error Could find either mps_arena_destroy or mps_space_destroy

#endif

#endif


/* Before release.epcore.brisling / release.dylan.kinglet
   there was no spare committed fund
*/

#ifndef MMQA_DEFINED_mps_arena_spare_committed
#define mps_arena_spare_committed(a) ((size_t) 0)
#endif

#ifndef MMQA_DEFINED_mps_arena_spare_commit_limit
#define mps_arena_spare_commit_limit(a) ((size_t) 0)
#endif

#ifndef MMQA_DEFINED_mps_arena_spare_commit_limit_set
#define mps_arena_spare_commit_limit_set(a, l) \
 asserts(0, \
 "MPS interface versions before HU do not support the spare committed fund")
#endif


/*
  The following defs can't be done automatically. I don't think
  this is a problem, but here they are just in case.

  Before modern interface;
   - no closure arguments for mps_root_create_reg
   - no rank argument for mps_ap_create (ever)

  #define mps_root_create_reg(ro, sp, ra, rm, thr, scan, p, _s) \
   (mps_root_create_reg)(ro, sp, ra, rm, thr, scan, p)

  #define mps_ap_create(a, p, _r) \
   (mps_ap_create)(a, p)
*/


#ifndef MMQA_DEFINED_mps_chain_create


#define mps_chain_t void*

typedef struct mps_gen_param_s {
  size_t mps_capacity;
  double mps_mortality;
} mps_gen_param_s;

#define mps_chain_create(p, a, s, par) (*(p) = NULL, (void)(par), MPS_RES_OK)
#define mps_chain_destroy(p) do {} while(0)

#define mmqa_pool_create_chain(pool_o, arena, class, format, chain) \
  mps_pool_create(pool_o, arena, class, format)


#else


#define mmqa_pool_create_chain(pool_o, arena, class, format, chain) \
  mps_pool_create(pool_o, arena, class, format, chain)


#endif
