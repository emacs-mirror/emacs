/* mps.h: RAVENBROOK MEMORY POOL SYSTEM C INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * THIS HEADER IS NOT DOCUMENTATION.
 * Please refer to the [MPS Manual](../manual/).
 *
 * But if you are a human reading this, please note:
 *
 * .naming: The MPS interface only uses identifiers beginning `mps_`,
 * `MPS_` or `_mps_` and may use any identifiers with these prefixes in
 * future.
 *
 * .naming.internal: Any identifier beginning with an underscore is for
 * internal use within the interface and may change or be withdrawn without
 * warning.
 *
 * .readership: compilers, MPS developers.
 *
 * .sources: [The design of the MPS Interface to C](../design/interface-c).
 */

#ifndef mps_h
#define mps_h

#include <stddef.h>
#include <stdarg.h>
#include <limits.h>


/* Platform Dependencies
 *
 * We went for over ten years without any platform ifdefs in this header.
 * Then Microsoft made unsigned long shorter than a pointer on Win64.  Ugh.
 */

#if defined(_MSC_VER) && defined(_WIN32) && defined(_WIN64) && defined(_M_X64)
typedef unsigned __int64 mps_word_t;
#else
typedef unsigned long mps_word_t;
#endif


/* Abstract Types */

typedef struct mps_arena_s  *mps_arena_t;  /* arena */
typedef struct mps_arena_class_s *mps_arena_class_t;  /* arena class */
typedef struct mps_pool_s   *mps_pool_t;   /* pool */
typedef struct mps_chain_s  *mps_chain_t;  /* chain */
typedef struct mps_fmt_s    *mps_fmt_t;    /* object format */
typedef struct mps_root_s   *mps_root_t;   /* root */
typedef struct mps_pool_class_s  *mps_pool_class_t;  /* pool class */
typedef mps_pool_class_t mps_class_t;      /* deprecated alias */
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
typedef const struct mps_key_s *mps_key_t; /* argument key */

/* Concrete Types */

typedef int mps_bool_t;         /* boolean (int) */
typedef int mps_res_t;          /* result code (int) */
typedef void *mps_addr_t;       /* managed address (void *) */
typedef size_t mps_align_t;     /* alignment (size_t) */
typedef unsigned mps_rm_t;      /* root mode (unsigned) */
typedef unsigned mps_rank_t;    /* ranks (unsigned) */
typedef unsigned mps_message_type_t;    /* message type (unsigned) */
typedef mps_word_t mps_clock_t;  /* processor time */
typedef mps_word_t mps_label_t;  /* telemetry label */

/* Result Codes */

#define _mps_RES_ENUM(R, X) \
  R(X, OK,            "success (always zero)") \
  R(X, FAIL,          "unspecified failure") \
  R(X, RESOURCE,      "unable to obtain resources") \
  R(X, MEMORY,        "unable to obtain memory") \
  R(X, LIMIT,         "limitation reached") \
  R(X, UNIMPL,        "unimplemented facility") \
  R(X, IO,            "system I/O error") \
  R(X, COMMIT_LIMIT,  "arena commit limit exceeded") \
  R(X, PARAM,         "illegal user parameter value")

#define _mps_ENUM_DEF_ROW(prefix, ident, doc) prefix##ident,
#define _mps_ENUM_DEF(REL, prefix) \
  enum { \
    REL(_mps_ENUM_DEF_ROW, prefix) \
    _mps_##prefix##LIMIT \
  };
_mps_ENUM_DEF(_mps_RES_ENUM, MPS_RES_)

/* Format and Root Method Types */
/* see <design/root-interface> */
/* see <design/format-interface> */

typedef struct mps_scan_tag_s *mps_scan_tag_t;
typedef struct mps_scan_tag_s {
  mps_word_t mask;
  mps_word_t pattern;
} mps_scan_tag_s;

typedef mps_res_t (*mps_root_scan_t)(mps_ss_t, void *, size_t);
typedef mps_res_t (*mps_area_scan_t)(mps_ss_t, void *, void *, void *);
typedef mps_res_t (*mps_fmt_scan_t)(mps_ss_t, mps_addr_t, mps_addr_t);
typedef mps_res_t (*mps_reg_scan_t)(mps_ss_t, mps_thr_t,
                                    void *, size_t);
typedef mps_addr_t (*mps_fmt_skip_t)(mps_addr_t);
typedef void (*mps_fmt_copy_t)(mps_addr_t, mps_addr_t);
typedef void (*mps_fmt_fwd_t)(mps_addr_t, mps_addr_t);
typedef mps_addr_t (*mps_fmt_isfwd_t)(mps_addr_t);
typedef void (*mps_fmt_pad_t)(mps_addr_t, size_t);
typedef mps_addr_t (*mps_fmt_class_t)(mps_addr_t);


/* Keyword argument lists */

typedef void (*mps_fun_t)(void);

typedef struct mps_arg_s {
  mps_key_t key;
  union {
    mps_bool_t b;
    char c;
    const char *string;
    int i;
    unsigned u;
    long l;
    unsigned long ul;
    float f;
    double d;
    size_t size;
    mps_fun_t fun;
    mps_addr_t addr;
    mps_fmt_t format;
    mps_chain_t chain;
    struct mps_pool_debug_option_s *pool_debug_options;
    mps_addr_t (*addr_method)(mps_addr_t);
    mps_align_t align;
    mps_word_t count;
    void *p;
    mps_rank_t rank;
    mps_fmt_scan_t fmt_scan;
    mps_fmt_skip_t fmt_skip;
    mps_fmt_fwd_t fmt_fwd;
    mps_fmt_isfwd_t fmt_isfwd;
    mps_fmt_pad_t fmt_pad;
    mps_fmt_class_t fmt_class;
    mps_pool_t pool;
  } val;
} mps_arg_s;

extern const struct mps_key_s _mps_key_ARGS_END;
#define MPS_KEY_ARGS_END        (&_mps_key_ARGS_END)
extern mps_arg_s mps_args_none[];

extern const struct mps_key_s _mps_key_ARENA_GRAIN_SIZE;
#define MPS_KEY_ARENA_GRAIN_SIZE (&_mps_key_ARENA_GRAIN_SIZE)
#define MPS_KEY_ARENA_GRAIN_SIZE_FIELD size
extern const struct mps_key_s _mps_key_ARENA_SIZE;
#define MPS_KEY_ARENA_SIZE      (&_mps_key_ARENA_SIZE)
#define MPS_KEY_ARENA_SIZE_FIELD size
extern const struct mps_key_s _mps_key_ARENA_ZONED;
#define MPS_KEY_ARENA_ZONED     (&_mps_key_ARENA_ZONED)
#define MPS_KEY_ARENA_ZONED_FIELD b
extern const struct mps_key_s _mps_key_FORMAT;
#define MPS_KEY_FORMAT          (&_mps_key_FORMAT)
#define MPS_KEY_FORMAT_FIELD    format
extern const struct mps_key_s _mps_key_CHAIN;
#define MPS_KEY_CHAIN           (&_mps_key_CHAIN)
#define MPS_KEY_CHAIN_FIELD     chain
extern const struct mps_key_s _mps_key_GEN;
#define MPS_KEY_GEN             (&_mps_key_GEN)
#define MPS_KEY_GEN_FIELD       u
extern const struct mps_key_s _mps_key_RANK;
#define MPS_KEY_RANK            (&_mps_key_RANK)
#define MPS_KEY_RANK_FIELD      rank
extern const struct mps_key_s _mps_key_COMMIT_LIMIT;
#define MPS_KEY_COMMIT_LIMIT (&_mps_key_COMMIT_LIMIT)
#define MPS_KEY_COMMIT_LIMIT_FIELD size
extern const struct mps_key_s _mps_key_SPARE_COMMIT_LIMIT;
#define MPS_KEY_SPARE_COMMIT_LIMIT (&_mps_key_SPARE_COMMIT_LIMIT)
#define MPS_KEY_SPARE_COMMIT_LIMIT_FIELD size
extern const struct mps_key_s _mps_key_PAUSE_TIME;
#define MPS_KEY_PAUSE_TIME      (&_mps_key_PAUSE_TIME)
#define MPS_KEY_PAUSE_TIME_FIELD d

extern const struct mps_key_s _mps_key_EXTEND_BY;
#define MPS_KEY_EXTEND_BY       (&_mps_key_EXTEND_BY)
#define MPS_KEY_EXTEND_BY_FIELD size
extern const struct mps_key_s _mps_key_LARGE_SIZE;
#define MPS_KEY_LARGE_SIZE      (&_mps_key_LARGE_SIZE)
#define MPS_KEY_LARGE_SIZE_FIELD size
extern const struct mps_key_s _mps_key_MIN_SIZE;
#define MPS_KEY_MIN_SIZE        (&_mps_key_MIN_SIZE)
#define MPS_KEY_MIN_SIZE_FIELD  size
extern const struct mps_key_s _mps_key_MEAN_SIZE;
#define MPS_KEY_MEAN_SIZE       (&_mps_key_MEAN_SIZE)
#define MPS_KEY_MEAN_SIZE_FIELD size
extern const struct mps_key_s _mps_key_MAX_SIZE;
#define MPS_KEY_MAX_SIZE        (&_mps_key_MAX_SIZE)
#define MPS_KEY_MAX_SIZE_FIELD  size
extern const struct mps_key_s _mps_key_ALIGN;
#define MPS_KEY_ALIGN           (&_mps_key_ALIGN)
#define MPS_KEY_ALIGN_FIELD     align
extern const struct mps_key_s _mps_key_SPARE;
#define MPS_KEY_SPARE           (&_mps_key_SPARE)
#define MPS_KEY_SPARE_FIELD     d
extern const struct mps_key_s _mps_key_INTERIOR;
#define MPS_KEY_INTERIOR        (&_mps_key_INTERIOR)
#define MPS_KEY_INTERIOR_FIELD  b

extern const struct mps_key_s _mps_key_VMW3_TOP_DOWN;
#define MPS_KEY_VMW3_TOP_DOWN   (&_mps_key_VMW3_TOP_DOWN)
#define MPS_KEY_VMW3_TOP_DOWN_FIELD b

extern const struct mps_key_s _mps_key_FMT_ALIGN;
#define MPS_KEY_FMT_ALIGN   (&_mps_key_FMT_ALIGN)
#define MPS_KEY_FMT_ALIGN_FIELD align
extern const struct mps_key_s _mps_key_FMT_HEADER_SIZE;
#define MPS_KEY_FMT_HEADER_SIZE   (&_mps_key_FMT_HEADER_SIZE)
#define MPS_KEY_FMT_HEADER_SIZE_FIELD size
extern const struct mps_key_s _mps_key_FMT_SCAN;
#define MPS_KEY_FMT_SCAN   (&_mps_key_FMT_SCAN)
#define MPS_KEY_FMT_SCAN_FIELD fmt_scan
extern const struct mps_key_s _mps_key_FMT_SKIP;
#define MPS_KEY_FMT_SKIP   (&_mps_key_FMT_SKIP)
#define MPS_KEY_FMT_SKIP_FIELD fmt_skip
extern const struct mps_key_s _mps_key_FMT_FWD;
#define MPS_KEY_FMT_FWD   (&_mps_key_FMT_FWD)
#define MPS_KEY_FMT_FWD_FIELD fmt_fwd
extern const struct mps_key_s _mps_key_FMT_ISFWD;
#define MPS_KEY_FMT_ISFWD   (&_mps_key_FMT_ISFWD)
#define MPS_KEY_FMT_ISFWD_FIELD fmt_isfwd
extern const struct mps_key_s _mps_key_FMT_PAD;
#define MPS_KEY_FMT_PAD   (&_mps_key_FMT_PAD)
#define MPS_KEY_FMT_PAD_FIELD fmt_pad
extern const struct mps_key_s _mps_key_FMT_CLASS;
#define MPS_KEY_FMT_CLASS   (&_mps_key_FMT_CLASS)
#define MPS_KEY_FMT_CLASS_FIELD fmt_class

/* Maximum length of a keyword argument list. */
#define MPS_ARGS_MAX          32

extern void _mps_args_set_key(mps_arg_s args[MPS_ARGS_MAX], unsigned i,
                              mps_key_t key);

#define MPS_ARGS_BEGIN(_var) \
  MPS_BEGIN \
    mps_arg_s _var[MPS_ARGS_MAX]; \
    unsigned _var##_i = 0; \
    _mps_args_set_key(_var, _var##_i, MPS_KEY_ARGS_END); \
    MPS_BEGIN

#define MPS_ARGS_ADD_FIELD(_var, _key, _field, _val)  \
  MPS_BEGIN \
    _mps_args_set_key(_var, _var##_i, _key); \
    _var[_var##_i].val._field = (_val); \
    ++_var##_i; \
    _mps_args_set_key(_var, _var##_i, MPS_KEY_ARGS_END); \
  MPS_END

#define MPS_ARGS_ADD(_var, _key, _val) \
  MPS_ARGS_ADD_FIELD(_var, _key, _key##_FIELD, _val)

#define MPS_ARGS_DONE(_var) \
  MPS_BEGIN \
    _mps_args_set_key(_var, _var##_i, MPS_KEY_ARGS_END); \
    _var##_i = MPS_ARGS_MAX; \
  MPS_END

#define MPS_ARGS_END(_var) \
    MPS_END; \
  MPS_END


/* <a id="message.types"> Keep in sync with
 * <code/mpmtypes.h#message.types> */
/* Not meant to be used by clients, they should use the macros below. */
enum {
  _mps_MESSAGE_TYPE_FINALIZATION,
  _mps_MESSAGE_TYPE_GC,
  _mps_MESSAGE_TYPE_GC_START
};

/* Message Types
 * This is what clients should use. */
#define mps_message_type_finalization() _mps_MESSAGE_TYPE_FINALIZATION
#define mps_message_type_gc() _mps_MESSAGE_TYPE_GC
#define mps_message_type_gc_start() _mps_MESSAGE_TYPE_GC_START


/* Reference Ranks
 *
 * See protocol.mps.reference. */

extern mps_rank_t mps_rank_ambig(void);
extern mps_rank_t mps_rank_exact(void);
extern mps_rank_t mps_rank_weak(void);


/* Root Modes */
/* .rm: Keep in sync with <code/mpmtypes.h#rm> */

#define MPS_RM_CONST      (((mps_rm_t)1<<0))
#define MPS_RM_PROT       (((mps_rm_t)1<<1))
#define MPS_RM_PROT_INNER (((mps_rm_t)1<<1))


/* Allocation Point */

typedef struct mps_ap_s {       /* allocation point descriptor */
  mps_addr_t init;              /* limit of initialized memory */
  mps_addr_t alloc;             /* limit of allocated memory */
  mps_addr_t limit;             /* limit of available memory */
} mps_ap_s;


/* Segregated-fit Allocation Caches */
/* .sac: Keep in sync with <code/sac.h>. */

typedef struct _mps_sac_s *mps_sac_t;

#define MPS_SAC_CLASS_LIMIT ((size_t)8)

typedef struct _mps_sac_freelist_block_s {
  size_t _size;
  size_t _count;
  size_t _count_max;
  mps_addr_t _blocks;
} _mps_sac_freelist_block_s;

typedef struct _mps_sac_s {
  size_t _middle;
  mps_bool_t _trapped;
  _mps_sac_freelist_block_s _freelists[2 * MPS_SAC_CLASS_LIMIT];
} _mps_sac_s;

/* .sacc: Keep in sync with <code/sac.h>. */
typedef struct mps_sac_class_s {
  size_t mps_block_size;
  size_t mps_cached_count;
  unsigned mps_frequency;
} mps_sac_class_s;

#define mps_sac_classes_s mps_sac_class_s


/* Location Dependency */
/* .ld: Keep in sync with <code/mpmst.h#ld.struct>. */

typedef struct mps_ld_s {       /* location dependency descriptor */
  mps_word_t _epoch, _rs;
} mps_ld_s;


/* Scan State */
/* .ss: See also <code/mpmst.h#ss>. */

typedef struct mps_ss_s {
  mps_word_t _zs, _w, _ufs;
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
typedef struct mps_fmt_A_s *mps_fmt_A_t;
/* type-name mps_fmt_A_t is deprecated: use mps_fmt_A_s* instead */

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
typedef struct mps_fmt_B_s *mps_fmt_B_t;
/* type-name mps_fmt_B_t is deprecated: use mps_fmt_B_s* instead */


typedef struct mps_fmt_auto_header_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
  size_t          mps_headerSize;
} mps_fmt_auto_header_s;


typedef struct mps_fmt_fixed_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
} mps_fmt_fixed_s;


/* Internal Definitions */

#define MPS_BEGIN       do {
#define MPS_END         } while(0)
/* MPS_END might cause compiler warnings about constant conditionals.
 * This could be avoided with some loss of efficiency by replacing 0
 * with a variable always guaranteed to be 0.  In Visual C, the
 * warning can be turned off using:
 * #pragma warning(disable: 4127)
 */


/* arenas */

extern void mps_arena_clamp(mps_arena_t);
extern void mps_arena_release(mps_arena_t);
extern void mps_arena_park(mps_arena_t);
extern void mps_arena_postmortem(mps_arena_t);
extern void mps_arena_expose(mps_arena_t);
extern void mps_arena_unsafe_expose_remember_protection(mps_arena_t);
extern void mps_arena_unsafe_restore_protection(mps_arena_t);
extern mps_res_t mps_arena_start_collect(mps_arena_t);
extern mps_res_t mps_arena_collect(mps_arena_t);
extern mps_bool_t mps_arena_step(mps_arena_t, double, double);

extern mps_res_t mps_arena_create(mps_arena_t *, mps_arena_class_t, ...);
extern mps_res_t mps_arena_create_v(mps_arena_t *, mps_arena_class_t, va_list);
extern mps_res_t mps_arena_create_k(mps_arena_t *, mps_arena_class_t,
                                    mps_arg_s []);
extern void mps_arena_destroy(mps_arena_t);

extern size_t mps_arena_reserved(mps_arena_t);
extern size_t mps_arena_committed(mps_arena_t);
extern size_t mps_arena_spare_committed(mps_arena_t);

extern size_t mps_arena_commit_limit(mps_arena_t);
extern mps_res_t mps_arena_commit_limit_set(mps_arena_t, size_t);
extern double mps_arena_spare(mps_arena_t);
extern void mps_arena_spare_set(mps_arena_t, double);
extern void mps_arena_spare_commit_limit_set(mps_arena_t, size_t);
extern size_t mps_arena_spare_commit_limit(mps_arena_t);

extern double mps_arena_pause_time(mps_arena_t);
extern void mps_arena_pause_time_set(mps_arena_t, double);

extern mps_bool_t mps_arena_busy(mps_arena_t);
extern mps_bool_t mps_arena_has_addr(mps_arena_t, mps_addr_t);
extern mps_bool_t mps_addr_pool(mps_pool_t *, mps_arena_t, mps_addr_t);
extern mps_bool_t mps_addr_fmt(mps_fmt_t *, mps_arena_t, mps_addr_t);

/* Client memory arenas */
extern mps_res_t mps_arena_extend(mps_arena_t, mps_addr_t, size_t);
#if 0
/* There's no implementation for this function. */
extern mps_res_t mps_arena_retract(mps_arena_t, mps_addr_t, size_t);
#endif


/* Object Formats */

extern mps_res_t mps_fmt_create_k(mps_fmt_t *, mps_arena_t, mps_arg_s []);
extern mps_res_t mps_fmt_create_A(mps_fmt_t *, mps_arena_t,
                                  mps_fmt_A_s *);
extern mps_res_t mps_fmt_create_B(mps_fmt_t *, mps_arena_t,
                                  mps_fmt_B_s *);
extern mps_res_t mps_fmt_create_auto_header(mps_fmt_t *, mps_arena_t,
                                            mps_fmt_auto_header_s *);
extern mps_res_t mps_fmt_create_fixed(mps_fmt_t *, mps_arena_t,
                                      mps_fmt_fixed_s *);
extern void mps_fmt_destroy(mps_fmt_t);


/* Pools */

extern mps_res_t mps_pool_create(mps_pool_t *, mps_arena_t,
                                 mps_pool_class_t, ...);
extern mps_res_t mps_pool_create_v(mps_pool_t *, mps_arena_t,
                                   mps_pool_class_t, va_list);
extern mps_res_t mps_pool_create_k(mps_pool_t *, mps_arena_t,
                                   mps_pool_class_t, mps_arg_s []);
extern void mps_pool_destroy(mps_pool_t);
extern size_t mps_pool_total_size(mps_pool_t);
extern size_t mps_pool_free_size(mps_pool_t);


/* Chains */

/* .gen-param: This structure must match <code/locus.h#gen-param>. */
typedef struct mps_gen_param_s {
  size_t mps_capacity;
  double mps_mortality;
} mps_gen_param_s;

extern mps_res_t mps_chain_create(mps_chain_t *, mps_arena_t,
                                  size_t, mps_gen_param_s *);
extern void mps_chain_destroy(mps_chain_t);


/* Manual Allocation */

extern mps_res_t mps_alloc(mps_addr_t *, mps_pool_t, size_t);
extern mps_res_t mps_alloc_v(mps_addr_t *, mps_pool_t, size_t, va_list);
extern void mps_free(mps_pool_t, mps_addr_t, size_t);


/* Allocation Points */

extern mps_res_t mps_ap_create(mps_ap_t *, mps_pool_t, ...);
extern mps_res_t mps_ap_create_v(mps_ap_t *, mps_pool_t, va_list);
extern mps_res_t mps_ap_create_k(mps_ap_t *, mps_pool_t, mps_arg_s []);
extern void mps_ap_destroy(mps_ap_t);

extern mps_res_t (mps_reserve)(mps_addr_t *, mps_ap_t, size_t);
extern mps_bool_t (mps_commit)(mps_ap_t, mps_addr_t, size_t);

extern mps_res_t mps_ap_fill(mps_addr_t *, mps_ap_t, size_t);

/* mps_ap_fill_with_reservoir_permit is deprecated */
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
    if (_mps_s > (sac)->_middle) { \
      _mps_i = 0; \
      while (_mps_s > (sac)->_freelists[_mps_i]._size) \
        _mps_i += 2; \
    } else { \
      _mps_i = 1; \
      while (_mps_s <= (sac)->_freelists[_mps_i]._size) \
        _mps_i += 2; \
    } \
    if ((sac)->_freelists[_mps_i]._count != 0) { \
      (p_o) = (sac)->_freelists[_mps_i]._blocks; \
      (sac)->_freelists[_mps_i]._blocks = *(mps_addr_t *)(p_o); \
      --(sac)->_freelists[_mps_i]._count; \
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
    if (_mps_s > (sac)->_middle) { \
      _mps_i = 0; \
      while (_mps_s > (sac)->_freelists[_mps_i]._size) \
        _mps_i += 2; \
    } else { \
      _mps_i = 1; \
      while (_mps_s <= (sac)->_freelists[_mps_i]._size) \
        _mps_i += 2; \
    } \
    if ((sac)->_freelists[_mps_i]._count \
        < (sac)->_freelists[_mps_i]._count_max) { \
       *(mps_addr_t *)(p) = (sac)->_freelists[_mps_i]._blocks; \
      (sac)->_freelists[_mps_i]._blocks = (p); \
      ++(sac)->_freelists[_mps_i]._count; \
    } else \
      mps_sac_empty(sac, p, _mps_s); \
  MPS_END

/* backward compatibility */
#define MPS_SAC_ALLOC(res_o, p_o, sac, size, has_reservoir_permit) \
      MPS_SAC_ALLOC_FAST(res_o, p_o, sac, size, has_reservoir_permit)
#define MPS_SAC_FREE(sac, p, size) MPS_SAC_FREE_FAST(sac, p, size)


/* Low memory reservoir (deprecated) */

extern void mps_reservoir_limit_set(mps_arena_t, size_t);
extern size_t mps_reservoir_limit(mps_arena_t);
extern size_t mps_reservoir_available(mps_arena_t);
extern mps_res_t mps_reserve_with_reservoir_permit(mps_addr_t *,
                                                   mps_ap_t,
                                                   size_t);


/* Reserve Macros */
/* .reserve: Keep in sync with <code/buffer.c#reserve>. */

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


#define MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK MPS_RESERVE_BLOCK


/* Commit Macros */
/* .commit: Keep in sync with <code/buffer.c#commit>. */

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
extern mps_res_t mps_root_create_area(mps_root_t *, mps_arena_t,
                                      mps_rank_t, mps_rm_t,
                                      void *, void *,
                                      mps_area_scan_t, void *);
extern mps_res_t mps_root_create_area_tagged(mps_root_t *, mps_arena_t,
                                             mps_rank_t, mps_rm_t,
                                             void *, void *,
                                             mps_area_scan_t,
                                             mps_word_t, mps_word_t);
extern mps_res_t mps_root_create_fmt(mps_root_t *, mps_arena_t,
                                     mps_rank_t, mps_rm_t,
                                     mps_fmt_scan_t, mps_addr_t,
                                     mps_addr_t);
extern mps_res_t mps_root_create_reg(mps_root_t *, mps_arena_t,
                                     mps_rank_t, mps_rm_t, mps_thr_t,
                                     mps_reg_scan_t, void *, size_t);
extern mps_res_t mps_root_create_thread(mps_root_t *, mps_arena_t,
                                        mps_thr_t, void *);
extern mps_res_t mps_root_create_thread_scanned(mps_root_t *, mps_arena_t,
                                                mps_rank_t, mps_rm_t, mps_thr_t,
                                                mps_area_scan_t,
                                                void *,
                                                void *);
extern mps_res_t mps_root_create_thread_tagged(mps_root_t *, mps_arena_t,
                                               mps_rank_t, mps_rm_t, mps_thr_t,
                                               mps_area_scan_t,
                                               mps_word_t, mps_word_t,
                                               void *);
extern void mps_root_destroy(mps_root_t);

extern mps_res_t mps_stack_scan_ambig(mps_ss_t, mps_thr_t,
                                      void *, size_t);


/* Protection Trampoline and Thread Registration */

typedef void *(*mps_tramp_t)(void *, size_t);
extern void (mps_tramp)(void **, mps_tramp_t, void *, size_t);

extern mps_res_t mps_thread_reg(mps_thr_t *, mps_arena_t);
extern void mps_thread_dereg(mps_thr_t);


/* Location Dependency */

extern void mps_ld_reset(mps_ld_t, mps_arena_t);
extern void mps_ld_add(mps_ld_t, mps_arena_t, mps_addr_t);
extern void mps_ld_merge(mps_ld_t, mps_arena_t, mps_ld_t);
extern mps_bool_t mps_ld_isstale(mps_ld_t, mps_arena_t, mps_addr_t);
extern mps_bool_t mps_ld_isstale_any(mps_ld_t, mps_arena_t);

extern mps_word_t mps_collections(mps_arena_t);


/* Messages */

extern void mps_message_type_enable(mps_arena_t, mps_message_type_t);
extern void mps_message_type_disable(mps_arena_t, mps_message_type_t);
extern mps_bool_t mps_message_poll(mps_arena_t);
extern mps_bool_t mps_message_queue_type(mps_message_type_t *, mps_arena_t);
extern mps_bool_t mps_message_get(mps_message_t *,
                                  mps_arena_t, mps_message_type_t);
extern void mps_message_discard(mps_arena_t, mps_message_t);

/* Message Methods */

/* -- All Message Types */
extern mps_message_type_t mps_message_type(mps_arena_t, mps_message_t);
extern mps_clock_t mps_message_clock(mps_arena_t, mps_message_t);

/* -- mps_message_type_finalization */
extern void mps_message_finalization_ref(mps_addr_t *,
                                         mps_arena_t, mps_message_t);

/* -- mps_message_type_gc */
extern size_t mps_message_gc_live_size(mps_arena_t, mps_message_t);
extern size_t mps_message_gc_condemned_size(mps_arena_t, mps_message_t);
extern size_t mps_message_gc_not_condemned_size(mps_arena_t,
                                                mps_message_t);

/* -- mps_message_type_gc_start */
extern const char *mps_message_gc_start_why(mps_arena_t, mps_message_t);


/* Finalization */

extern mps_res_t mps_finalize(mps_arena_t, mps_addr_t *);
extern mps_res_t mps_definalize(mps_arena_t, mps_addr_t *);


/* Telemetry */

extern mps_word_t mps_telemetry_control(mps_word_t, mps_word_t);
extern void mps_telemetry_set(mps_word_t);
extern void mps_telemetry_reset(mps_word_t);
extern mps_word_t mps_telemetry_get(void);
extern mps_label_t mps_telemetry_intern(const char *);
extern void mps_telemetry_label(mps_addr_t, mps_label_t);
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


/* Allocation debug options */


typedef struct mps_pool_debug_option_s {
  const void *fence_template;
  size_t fence_size;
  const void *free_template;
  size_t free_size;
} mps_pool_debug_option_s;

extern const struct mps_key_s _mps_key_POOL_DEBUG_OPTIONS;
#define MPS_KEY_POOL_DEBUG_OPTIONS (&_mps_key_POOL_DEBUG_OPTIONS)
#define MPS_KEY_POOL_DEBUG_OPTIONS_FIELD pool_debug_options

extern void mps_pool_check_fenceposts(mps_pool_t);
extern void mps_pool_check_free_space(mps_pool_t);


/* Scanner Support */

extern mps_res_t mps_scan_area(mps_ss_t, void *, void *, void *);
extern mps_res_t mps_scan_area_masked(mps_ss_t, void *, void *, void *);
extern mps_res_t mps_scan_area_tagged(mps_ss_t, void *, void *, void *);
extern mps_res_t mps_scan_area_tagged_or_zero(mps_ss_t, void *, void *, void *);

extern mps_res_t mps_fix(mps_ss_t, mps_addr_t *);

#define MPS_SCAN_BEGIN(ss) \
  MPS_BEGIN \
    mps_ss_t _ss = (ss); \
    mps_word_t _mps_zs = (_ss)->_zs; \
    mps_word_t _mps_w = (_ss)->_w; \
    mps_word_t _mps_ufs = (_ss)->_ufs; \
    mps_word_t _mps_wt; \
    {

#define MPS_FIX1(ss, ref) \
  (_mps_wt = (mps_word_t)1 << ((mps_word_t)(ref) >> _mps_zs \
                               & (sizeof(mps_word_t) * CHAR_BIT - 1)), \
   _mps_ufs |= _mps_wt, \
   (_mps_w & _mps_wt) != 0)

extern mps_res_t _mps_fix2(mps_ss_t, mps_addr_t *);
#define MPS_FIX2(ss, ref_io) _mps_fix2(ss, ref_io)

#define MPS_FIX12(ss, ref_io) \
  (MPS_FIX1(ss, *(ref_io)) ? \
   MPS_FIX2(ss, ref_io) : MPS_RES_OK)

/* MPS_FIX is deprecated */
#define MPS_FIX(ss, ref_io) MPS_FIX12(ss, ref_io)

#define MPS_FIX_CALL(ss, call) \
  MPS_BEGIN \
    (call); _mps_ufs |= (ss)->_ufs; \
  MPS_END

#define MPS_SCAN_END(ss) \
   } \
   (ss)->_ufs = _mps_ufs; \
  MPS_END


#endif /* mps_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
