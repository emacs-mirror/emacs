/* impl.h.mps: RAVENBROOK MEMORY POOL SYSTEM C INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .readership: customers, MPS developers.
 * .sources: design.mps.interface.c.
 */

#ifndef mps_h
#define mps_h

#include "mpstd.h"              /* detect platform */
#include <stddef.h>
#include <stdarg.h>
#include <limits.h>


/* Abstract Types */

typedef struct mps_arena_s  *mps_arena_t;  /* arena */
typedef mps_arena_t          mps_space_t;  /* space, for backward comp. */
typedef struct mps_arena_class_s *mps_arena_class_t;  /* arena class */
typedef struct mps_pool_s   *mps_pool_t;   /* pool */
typedef struct mps_chain_s  *mps_chain_t;  /* chain */
typedef struct mps_fmt_s    *mps_fmt_t;    /* object format */
typedef struct mps_root_s   *mps_root_t;   /* root */
typedef struct mps_class_s  *mps_class_t;  /* pool class */
typedef struct mps_thr_s    *mps_thr_t;    /* thread registration */
typedef struct mps_ap_s     *mps_ap_t;     /* allocation point */
typedef struct mps_ld_s     *mps_ld_t;     /* location dependency */
typedef struct mps_ss_s     *mps_ss_t;     /* scan state */
typedef struct mps_message_s
  *mps_message_t;                          /* message */
typedef struct mps_alloc_pattern_s
  *mps_alloc_pattern_t;                    /* allocation patterns */
typedef struct mps_frame_s
  *mps_frame_t;                            /* allocation frames */

/* Concrete Types */

typedef MPS_T_WORD mps_word_t;  /* machine word (target dep.) */
typedef int mps_bool_t;         /* boolean (int) */
typedef int mps_res_t;          /* result code (int) */
typedef unsigned mps_shift_t;   /* shift amount (unsigned int) */
typedef void *mps_addr_t;       /* managed address (void *) */
typedef size_t mps_align_t;     /* alignment (size_t) */
typedef unsigned mps_rm_t;      /* root mode (unsigned) */
typedef unsigned mps_rank_t;    /* ranks (unsigned) */
typedef unsigned mps_message_type_t;    /* message type (unsigned) */

/* Result Codes */
/* .result-codes: Keep in sync with impl.h.mpmtypes.result-codes */
/* and the check in impl.c.mpsi.check.rc */

enum {
  MPS_RES_OK = 0,               /* success (always zero) */
  MPS_RES_FAIL,                 /* unspecified failure */
  MPS_RES_RESOURCE,             /* unable to obtain resources */
  MPS_RES_MEMORY,               /* unable to obtain memory */
  MPS_RES_LIMIT,                /* limitation reached */
  MPS_RES_UNIMPL,               /* unimplemented facility */
  MPS_RES_IO,                   /* system I/O error */
  MPS_RES_COMMIT_LIMIT,         /* arena commit limit exceeded */
  MPS_RES_PARAM                 /* illegal user parameter value */
};

/* .message.types: Keep in sync with impl.h.mpmtypes.message.types */
/* Not meant to be used by clients, they should use the macros below. */
enum {
  MPS_MESSAGE_TYPE_FINALIZATION,
  MPS_MESSAGE_TYPE_GC
};

/* Message Types
 * This is what clients should use. */
#define mps_message_type_finalization() MPS_MESSAGE_TYPE_FINALIZATION
#define mps_message_type_gc() MPS_MESSAGE_TYPE_GC


/* Reference Ranks
 *
 * See protocol.mps.reference. */

extern mps_rank_t mps_rank_ambig(void);
extern mps_rank_t mps_rank_exact(void);
extern mps_rank_t mps_rank_weak(void);

/* These upper case symbolic forms are obsolescent. */
/* Provided for source compatibility only. */
#define MPS_RANK_AMBIG mps_rank_ambig()
#define MPS_RANK_EXACT mps_rank_exact()
#define MPS_RANK_WEAK mps_rank_weak()

/* Root Modes */
/* .rm: Keep in sync with impl.h.mpmtypes.rm */

#define MPS_RM_CONST    (((mps_rm_t)1<<0))
#define MPS_RM_PROT     (((mps_rm_t)1<<1))


/* Allocation Point */
/* .ap: Keep in sync with impl.h.mpmst.ap. */

typedef struct mps_ap_s {       /* allocation point descriptor */
  mps_addr_t init;              /* limit of initialized memory */
  mps_addr_t alloc;             /* limit of allocated memory */
  mps_addr_t limit;             /* limit of available memory */
  mps_addr_t frameptr;          /* lightweight frame pointer */
  mps_bool_t enabled;           /* lightweight frame status */
  mps_bool_t lwpoppending;      /* lightweight pop pending? */
} mps_ap_s;


/* Segregated-fit Allocation Caches */
/* .sac: Keep in sync with impl.h.sac. */

typedef struct mps_sac_s *mps_sac_t;

#define MPS_SAC_CLASS_LIMIT ((size_t)8)

typedef struct mps_sac_freelist_block_s {
  size_t mps_size;
  size_t mps_count;
  size_t mps_count_max;
  mps_addr_t mps_blocks;
} mps_sac_freelist_block_s;

typedef struct mps_sac_s {
  size_t mps_middle;
  mps_bool_t mps_trapped;
  mps_sac_freelist_block_s mps_freelists[2 * MPS_SAC_CLASS_LIMIT];
} mps_sac_s;

/* .sacc: Keep in sync with impl.h.sac. */
typedef struct mps_sac_classes_s {
  size_t mps_block_size;
  size_t mps_cached_count;
  unsigned mps_frequency;
} mps_sac_classes_s;


/* Location Dependency */
/* .ld: Keep in sync with impl.h.mpmst.ld.struct. */

typedef struct mps_ld_s {       /* location dependency descriptor */
  mps_word_t w0, w1;
} mps_ld_s;


/* Format and Root Method Types */
/* .fmt-methods: Keep in sync with impl.h.mpmtypes.fmt-methods */
/* .root-methods: Keep in sync with impl.h.mpmtypes.root-methods */

typedef mps_res_t (*mps_root_scan_t)(mps_ss_t, void *, size_t);
typedef mps_res_t (*mps_fmt_scan_t)(mps_ss_t, mps_addr_t, mps_addr_t);
typedef mps_res_t (*mps_reg_scan_t)(mps_ss_t, mps_thr_t,
                                    void *, size_t);
typedef mps_addr_t (*mps_fmt_skip_t)(mps_addr_t);
typedef void (*mps_fmt_copy_t)(mps_addr_t, mps_addr_t);
typedef void (*mps_fmt_fwd_t)(mps_addr_t, mps_addr_t);
typedef mps_addr_t (*mps_fmt_isfwd_t)(mps_addr_t);
typedef void (*mps_fmt_pad_t)(mps_addr_t, size_t);
typedef mps_addr_t (*mps_fmt_class_t)(mps_addr_t);


/* Scan State */
/* .ss: See also impl.c.mpsi.check.ss and impl.h.mpmst.ss. */

typedef struct mps_ss_s {
  mps_res_t (*fix)(mps_ss_t, mps_addr_t *);
  mps_word_t w0, w1, w2;
} mps_ss_s;


/* Format Variants */

typedef struct mps_fmt_A_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
} mps_fmt_A_s;
typedef struct mps_fmt_A_s *mps_fmt_A_t;  /* deprecated */

typedef struct mps_fmt_B_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
  mps_fmt_class_t mps_class;
} mps_fmt_B_s;
typedef struct mps_fmt_B_s *mps_fmt_B_t;  /* deprecated */


typedef struct mps_fmt_auto_header_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
  size_t          mps_headerSize;
} mps_fmt_auto_header_s;


/* Internal Definitions */

#define MPS_BEGIN       do {
#define MPS_END         } while(0)
/* MPS_END might cause compiler warnings about constant conditionals.
 * This could be avoided with some loss of efficiency by replacing 0
 * with a variable always guaranteed to be 0.  In Visual C, the
 * warning can be turned off using:
 * #pragma warning(disable: 4127)
 */


/* Assertion Handling */

typedef void (*mps_assert_t)(const char *, const char *, const char *,
                             unsigned);

extern mps_assert_t mps_assert_install(mps_assert_t);
extern mps_assert_t mps_assert_default(void);


/* arenas */

extern void mps_arena_clamp(mps_arena_t);
extern void mps_arena_release(mps_arena_t);
extern void mps_arena_park(mps_arena_t);
extern mps_res_t mps_arena_collect(mps_arena_t);
extern mps_bool_t mps_arena_step(mps_arena_t, double);
extern void mps_space_clamp(mps_space_t);
extern void mps_space_release(mps_space_t);
extern void mps_space_park(mps_space_t);
extern mps_res_t mps_space_collect(mps_space_t);

extern mps_res_t mps_arena_create(mps_arena_t *, mps_arena_class_t, ...);
extern mps_res_t mps_arena_create_v(mps_arena_t *, mps_arena_class_t, va_list);
extern void mps_arena_destroy(mps_arena_t);

/* these two for backward compatibility */
extern mps_res_t mps_space_create(mps_space_t *);
extern void mps_space_destroy(mps_space_t);

extern size_t mps_arena_reserved(mps_arena_t);
extern size_t mps_arena_committed(mps_arena_t);
extern size_t mps_arena_spare_committed(mps_arena_t);

extern size_t mps_arena_commit_limit(mps_arena_t);
extern mps_res_t mps_arena_commit_limit_set(mps_arena_t, size_t);
extern void mps_arena_spare_commit_limit_set(mps_arena_t, size_t);
extern size_t mps_arena_spare_commit_limit(mps_arena_t);

extern size_t mps_space_reserved(mps_space_t);
extern size_t mps_space_committed(mps_space_t);

extern mps_bool_t mps_arena_has_addr(mps_arena_t, mps_addr_t);

/* Client memory arenas */
extern mps_res_t mps_arena_extend(mps_arena_t, mps_addr_t, size_t);
extern mps_res_t mps_arena_retract(mps_arena_t, mps_addr_t, size_t);


/* Object Formats */

extern mps_res_t mps_fmt_create_A(mps_fmt_t *, mps_arena_t,
                                  mps_fmt_A_s *);
extern mps_res_t mps_fmt_create_B(mps_fmt_t *, mps_arena_t,
                                  mps_fmt_B_s *);
extern mps_res_t mps_fmt_create_auto_header(mps_fmt_t *, mps_arena_t,
                                            mps_fmt_auto_header_s *);
extern void mps_fmt_destroy(mps_fmt_t);


/* Pools */

extern mps_res_t mps_pool_create(mps_pool_t *, mps_arena_t,
                                 mps_class_t, ...);
extern mps_res_t mps_pool_create_v(mps_pool_t *, mps_arena_t,
                                   mps_class_t, va_list);
extern void mps_pool_destroy(mps_pool_t);

/* .gen-param: This structure must match impl.h.chain.gen-param. */
typedef struct mps_gen_param_s {
  size_t mps_capacity;
  double mps_mortality;
} mps_gen_param_s;

extern mps_res_t mps_chain_create(mps_chain_t *, mps_arena_t,
                                  size_t, mps_gen_param_s *);
extern void mps_chain_destroy(mps_chain_t);

extern mps_res_t mps_alloc(mps_addr_t *, mps_pool_t, size_t, ...);
extern mps_res_t mps_alloc_v(mps_addr_t *, mps_pool_t, size_t, va_list);
extern void mps_free(mps_pool_t, mps_addr_t, size_t);


/* Allocation Points */

extern mps_res_t mps_ap_create(mps_ap_t *, mps_pool_t, ...);
extern mps_res_t mps_ap_create_v(mps_ap_t *, mps_pool_t, va_list);
extern void mps_ap_destroy(mps_ap_t);

extern mps_res_t (mps_reserve)(mps_addr_t *, mps_ap_t, size_t);
extern mps_bool_t (mps_commit)(mps_ap_t, mps_addr_t, size_t);

extern mps_res_t mps_ap_fill(mps_addr_t *, mps_ap_t, size_t);
extern mps_res_t mps_ap_fill_with_reservoir_permit(mps_addr_t *,
                                                   mps_ap_t,
                                                   size_t);

extern mps_res_t (mps_ap_frame_push)(mps_frame_t *, mps_ap_t);
extern mps_res_t (mps_ap_frame_pop)(mps_ap_t, mps_frame_t);

extern mps_bool_t mps_ap_trip(mps_ap_t, mps_addr_t, size_t);

extern mps_alloc_pattern_t mps_alloc_pattern_ramp(void);
extern mps_alloc_pattern_t mps_alloc_pattern_ramp_collect_all(void);
extern mps_res_t mps_ap_alloc_pattern_begin(mps_ap_t, mps_alloc_pattern_t);
extern mps_res_t mps_ap_alloc_pattern_end(mps_ap_t, mps_alloc_pattern_t);
extern mps_res_t mps_ap_alloc_pattern_reset(mps_ap_t);


/* Segregated-fit Allocation Caches */

extern mps_res_t mps_sac_create(mps_sac_t *, mps_pool_t, size_t,
                                mps_sac_classes_s *);
extern void mps_sac_destroy(mps_sac_t);
extern mps_res_t mps_sac_alloc(mps_addr_t *, mps_sac_t, size_t, mps_bool_t);
extern void mps_sac_free(mps_sac_t, mps_addr_t, size_t);
extern void mps_sac_flush(mps_sac_t);

/* Direct access to mps_sac_fill and mps_sac_empty is not supported. */
extern mps_res_t mps_sac_fill(mps_addr_t *, mps_sac_t, size_t, mps_bool_t);
extern void mps_sac_empty(mps_sac_t, mps_addr_t, size_t);

#define MPS_SAC_ALLOC_FAST(res_o, p_o, sac, size, has_reservoir_permit) \
  MPS_BEGIN \
    size_t _mps_i, _mps_s; \
    \
    _mps_s = (size); \
    if (_mps_s > (sac)->mps_middle) { \
      _mps_i = 0; \
      while (_mps_s > (sac)->mps_freelists[_mps_i].mps_size) \
        _mps_i += 2; \
    } else { \
      _mps_i = 1; \
      while (_mps_s <= (sac)->mps_freelists[_mps_i].mps_size) \
        _mps_i += 2; \
    } \
    if ((sac)->mps_freelists[_mps_i].mps_count != 0) { \
      (p_o) = (sac)->mps_freelists[_mps_i].mps_blocks; \
      (sac)->mps_freelists[_mps_i].mps_blocks = *(mps_addr_t *)(p_o); \
      --(sac)->mps_freelists[_mps_i].mps_count; \
      (res_o) = MPS_RES_OK; \
    } else \
      (res_o) = mps_sac_fill(&(p_o), sac, _mps_s, \
                             has_reservoir_permit); \
  MPS_END

#define MPS_SAC_FREE_FAST(sac, p, size) \
  MPS_BEGIN \
    size_t _mps_i, _mps_s; \
    \
    _mps_s = (size); \
    if (_mps_s > (sac)->mps_middle) { \
      _mps_i = 0; \
      while (_mps_s > (sac)->mps_freelists[_mps_i].mps_size) \
        _mps_i += 2; \
    } else { \
      _mps_i = 1; \
      while (_mps_s <= (sac)->mps_freelists[_mps_i].mps_size) \
        _mps_i += 2; \
    } \
    if ((sac)->mps_freelists[_mps_i].mps_count \
        < (sac)->mps_freelists[_mps_i].mps_count_max) { \
       *(mps_addr_t *)(p) = (sac)->mps_freelists[_mps_i].mps_blocks; \
      (sac)->mps_freelists[_mps_i].mps_blocks = (p); \
      ++(sac)->mps_freelists[_mps_i].mps_count; \
    } else \
      mps_sac_empty(sac, p, _mps_s); \
  MPS_END

/* backward compatibility */
#define MPS_SAC_ALLOC(res_o, p_o, sac, size, has_reservoir_permit) \
      MPS_SAC_ALLOC_FAST(res_o, p_o, sac, size, has_reservoir_permit)
#define MPS_SAC_FREE(sac, p, size) MPS_SAC_FREE_FAST(sac, p, size)


/* Low memory reservoir */

extern void mps_reservoir_limit_set(mps_arena_t, size_t);
extern size_t mps_reservoir_limit(mps_arena_t);
extern size_t mps_reservoir_available(mps_arena_t);
extern mps_res_t mps_reserve_with_reservoir_permit(mps_addr_t *,
                                                   mps_ap_t,
                                                   size_t);


/* Reserve Macros */
/* .reserve: Keep in sync with impl.c.buffer.reserve. */

#define mps_reserve(_p_o, _mps_ap, _size) \
  ((char *)(_mps_ap)->alloc + (_size) > (char *)(_mps_ap)->alloc && \
   (char *)(_mps_ap)->alloc + (_size) <= (char *)(_mps_ap)->limit ? \
     ((_mps_ap)->alloc = \
       (mps_addr_t)((char *)(_mps_ap)->alloc + (_size)), \
      *(_p_o) = (_mps_ap)->init, \
      MPS_RES_OK) : \
     mps_ap_fill(_p_o, _mps_ap, _size))


#define MPS_RESERVE_BLOCK(_res_v, _p_v, _mps_ap, _size) \
  MPS_BEGIN \
    char *_alloc = (char *)(_mps_ap)->alloc; \
    char *_next = _alloc + (_size); \
    if(_next > _alloc && _next <= (char *)(_mps_ap)->limit) { \
      (_mps_ap)->alloc = (mps_addr_t)_next; \
      (_p_v) = (_mps_ap)->init; \
      (_res_v) = MPS_RES_OK; \
    } else \
      (_res_v) = mps_ap_fill(&(_p_v), _mps_ap, _size); \
  MPS_END


#define MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK(_res_v, _p_v, _mps_ap, _size) \
  MPS_BEGIN \
    char *_alloc = (char *)(_mps_ap)->alloc; \
    char *_next = _alloc + (_size); \
    if(_next > _alloc && _next <= (char *)(_mps_ap)->limit) { \
      (_mps_ap)->alloc = (mps_addr_t)_next; \
      (_p_v) = (_mps_ap)->init; \
      (_res_v) = MPS_RES_OK; \
    } else \
      (_res_v) = mps_ap_fill_with_reservoir_permit(&(_p_v), _mps_ap, _size); \
  MPS_END


/* Commit Macros */
/* .commit: Keep in sync with impl.c.buffer.commit. */

#define mps_commit(_mps_ap, _p, _size) \
  ((_mps_ap)->init = (_mps_ap)->alloc, \
   (_mps_ap)->limit != 0 || mps_ap_trip(_mps_ap, _p, _size))


/* Root Creation and Destruction */

extern mps_res_t mps_root_create(mps_root_t *, mps_arena_t, mps_rank_t,
                                 mps_rm_t, mps_root_scan_t,
                                 void *, size_t);
extern mps_res_t mps_root_create_table(mps_root_t *, mps_arena_t,
                                       mps_rank_t, mps_rm_t,
                                       mps_addr_t *, size_t);
extern mps_res_t mps_root_create_table_masked(mps_root_t *, mps_arena_t,
                                              mps_rank_t, mps_rm_t,
                                              mps_addr_t *, size_t,
                                              mps_word_t);
extern mps_res_t mps_root_create_fmt(mps_root_t *, mps_arena_t,
                                     mps_rank_t, mps_rm_t,
                                     mps_fmt_scan_t, mps_addr_t,
                                     mps_addr_t);
extern mps_res_t mps_root_create_reg(mps_root_t *, mps_arena_t,
                                     mps_rank_t, mps_rm_t, mps_thr_t,
                                     mps_reg_scan_t, void *, size_t);
extern void mps_root_destroy(mps_root_t);

extern mps_res_t mps_stack_scan_ambig(mps_ss_t, mps_thr_t,
                                      void *, size_t);


/* Protection Trampoline and Thread Registration */

typedef void *(*mps_tramp_t)(void *, size_t);

extern void (mps_tramp)(void **, mps_tramp_t, void *, size_t);

#ifndef mps_tramp /* If a platform-specific version hasn't been defined */

#define mps_tramp(r_o, f, p, s) \
  MPS_BEGIN \
    void **_r_o = (r_o); \
    mps_tramp_t _f = (f); \
    void *_p = (p); \
    size_t _s = (s); \
    *_r_o = (*_f)(_p, _s); \
  MPS_END

#endif

extern mps_res_t mps_thread_reg(mps_thr_t *, mps_arena_t);
extern void mps_thread_dereg(mps_thr_t);


/* Location Dependency */

extern void mps_ld_reset(mps_ld_t, mps_arena_t);
extern void mps_ld_add(mps_ld_t, mps_arena_t, mps_addr_t);
extern void mps_ld_merge(mps_ld_t, mps_arena_t, mps_ld_t);
extern mps_bool_t mps_ld_isstale(mps_ld_t, mps_arena_t, mps_addr_t);

extern mps_word_t mps_collections(mps_arena_t);


/* Messages */

extern mps_bool_t mps_message_poll(mps_arena_t);
extern void mps_message_type_enable(mps_arena_t, mps_message_type_t);
extern void mps_message_type_disable(mps_arena_t, mps_message_type_t);
extern mps_bool_t mps_message_get(mps_message_t *,
                                  mps_arena_t, mps_message_type_t);
extern void mps_message_discard(mps_arena_t, mps_message_t);
extern mps_bool_t mps_message_queue_type(mps_message_type_t *, mps_arena_t);
extern mps_message_type_t mps_message_type(mps_arena_t, mps_message_t);

/* Message Type Specific Methods */

/* MPS_MESSAGE_TYPE_FINALIZATION */

extern void mps_message_finalization_ref(mps_addr_t *,
                                         mps_arena_t, mps_message_t);

/* MPS_MESSAGE_TYPE_GC */

extern size_t mps_message_gc_live_size(mps_arena_t, mps_message_t);

extern size_t mps_message_gc_condemned_size(mps_arena_t, mps_message_t);

extern size_t mps_message_gc_not_condemned_size(mps_arena_t,
                                                mps_message_t);


/* Finalization */

extern mps_res_t mps_finalize(mps_arena_t, mps_addr_t *);
extern void mps_definalize(mps_arena_t, mps_addr_t *);


/* Telemetry */

extern mps_word_t mps_telemetry_control(mps_word_t, mps_word_t);
extern mps_word_t mps_telemetry_intern(const char *);
extern void mps_telemetry_label(mps_addr_t, mps_word_t);
extern void mps_telemetry_flush(void);


/* Heap Walking */

typedef void (*mps_formatted_objects_stepper_t)(mps_addr_t, mps_fmt_t,
                                                mps_pool_t,
                                                void *, size_t);
extern void mps_arena_formatted_objects_walk(mps_arena_t,
                                             mps_formatted_objects_stepper_t,
                                             void *, size_t);


/* Root Walking */

typedef void (*mps_roots_stepper_t)(mps_addr_t *,
                                    mps_root_t,
                                    void *, size_t);
extern void mps_arena_roots_walk(mps_arena_t,
                                 mps_roots_stepper_t,
                                 void *, size_t);


/* Fenceposting */


typedef struct mps_pool_debug_option_s {
  void* fence_template;
  size_t fence_size;
} mps_pool_debug_option_s;

extern void mps_pool_check_fenceposts(mps_pool_t);


/* Scanner Support */

extern mps_res_t mps_fix(mps_ss_t, mps_addr_t *);

#define MPS_SCAN_BEGIN(ss) \
  MPS_BEGIN \
    mps_ss_t _ss = (ss); \
    mps_word_t _mps_w0 = (_ss)->w0; \
    mps_word_t _mps_w1 = (_ss)->w1; \
    mps_word_t _mps_w2 = (_ss)->w2; \
    mps_word_t _mps_wt; \
    {

#define MPS_FIX1(ss, ref) \
  (_mps_wt = 1uL<<((mps_word_t)(ref)>>_mps_w0&(MPS_WORD_WIDTH-1)), \
   _mps_w2 |= _mps_wt, \
   _mps_w1 & _mps_wt)

#define MPS_FIX2(ss, ref_io) \
  ((*(ss)->fix)(ss, ref_io))

#define MPS_FIX12(ss, ref_io) \
  (MPS_FIX1(ss, *(ref_io)) ? \
   MPS_FIX2(ss, ref_io) : MPS_RES_OK)

/* MPS_FIX is deprecated */
#define MPS_FIX(ss, ref_io) MPS_FIX12(ss, ref_io)

#define MPS_FIX_CALL(ss, call) \
  MPS_BEGIN \
    (call); _mps_w2 |= (ss)->w2; \
  MPS_END

#define MPS_SCAN_END(ss) \
   } \
   (ss)->w2 = _mps_w2; \
  MPS_END


#endif /* mps_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
