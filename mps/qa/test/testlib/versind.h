/* $HopeName: MMQA_harness!testlib:versind.h(MMQA_harness_dev.2) $
versind.h
   This file used to read header files depending on the
   value of the MPS_INTERFACE_VERSION parameter (aka MMQA_VERS_XX).
   Now it works using MMQA_SYMBOL_xxx tests.

   Almost all the existing functionality can be done just as well
   this way. The only difference is the change in parameters
   between 'oldstyle' and 'modern', which is no longer relevant.
   We don't need to test oldstyle MMs any more.

   For the record, the old versions were:
    MO : modern -- i.e. as in thursday afternoon
    OS : oldstyle -- dylan.incr.patch.11
    GR : grotesque -- dylan.honeybee (space -> arena)
    BQ : baroque -- dylan.meadowlark (have to include mpsw3.h)
    HU : humanist -- dylan.kinglet (spare committed fund)
*/

/* In release.dylan.honeybee, space was renamed arena in lots of functions
   We'll use the "destroy" functions to test which versions are available
*/

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

