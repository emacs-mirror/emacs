/* impl.h.mps: HARLEQUIN MEMORY POOL SYSTEM C INTERFACE
 *
 * $HopeName: MMsrc!mps.h(trunk.34) $
 * Copyright (C) 1997, 1998 The Harlequin Group Limited.  All rights reserved.
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
typedef struct mps_fmt_s    *mps_fmt_t;    /* object format */
typedef struct mps_root_s   *mps_root_t;   /* root */
typedef struct mps_class_s  *mps_class_t;  /* pool class */
typedef struct mps_thr_s    *mps_thr_t;    /* thread registration */
typedef struct mps_ap_s     *mps_ap_t;     /* allocation point */
typedef struct mps_ld_s     *mps_ld_t;     /* location dependency */
typedef struct mps_reg_s    *mps_reg_t;    /* register file */
typedef struct mps_ss_s     *mps_ss_t;     /* scan state */
typedef struct mps_message_s *mps_message_t; /* message */

/* Concrete Types */

typedef MPS_T_WORD mps_word_t;  /* machine word (target dep.) */
typedef int mps_bool_t;         /* boolean (int) */
typedef int mps_res_t;          /* result code (int) */
typedef unsigned mps_shift_t;   /* shift amount (unsigned int) */
typedef void *mps_addr_t;       /* managed address (void *) */
typedef size_t mps_align_t;     /* alignment (size_t) */
typedef unsigned mps_rm_t;      /* root mode (unsigned) */
typedef unsigned mps_rank_t;    /* ranks (unsigned) */
typedef unsigned mps_message_type_t;	/* message type (unsigned) */

/* Result Codes */
/* .result-codes: Keep in sync with impl.h.mpmtypes.result-codes */

enum {
  MPS_RES_OK = 0,               /* success (always zero) */
  MPS_RES_FAIL,                 /* unspecified failure */
  MPS_RES_RESOURCE,             /* unable to obtain resources */
  MPS_RES_MEMORY,               /* unable to obtain memory */
  MPS_RES_LIMIT,                /* limitation reached */
  MPS_RES_UNIMPL,               /* unimplemented facility */
  MPS_RES_IO                    /* system I/O error */
};

/* .message.types: Keep in sync with impl.h.mpmtypes.message.types */
/* Not meant to be used by clients, they should use the macros below. */
enum {
  MPS_MESSAGE_TYPE_FINALIZATION
};

/* Message Types
 * This is what clients should use. */
#define mps_message_type_finalization() MPS_MESSAGE_TYPE_FINALIZATION


/* Reference Ranks */
/* .ranks: Keep in sync with impl.h.mpmtypes.ranks */

enum {
  MPS_RANK_AMBIG = 0,           /* ambiguous reference */
  MPS_RANK_EXACT = 1,           /* exact reference */
  MPS_RANK_FINAL = 2,           /* final reference */
  MPS_RANK_WEAK = 3             /* weak reference */
};

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
} mps_ap_s;


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

typedef struct mps_fmt_A_s *mps_fmt_A_t;
typedef struct mps_fmt_A_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
} mps_fmt_A_s;

typedef struct mps_fmt_B_s *mps_fmt_B_t;
typedef struct mps_fmt_B_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
  mps_fmt_class_t class;
} mps_fmt_B_s;


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
                             unsigned line);

extern mps_assert_t mps_assert_install(mps_assert_t);
extern mps_assert_t mps_assert_default(void);


/* arenas */

extern void mps_arena_clamp(mps_arena_t);
extern void mps_arena_release(mps_arena_t);
extern void mps_arena_park(mps_arena_t);
extern mps_res_t mps_arena_collect(mps_arena_t);
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
 
extern size_t mps_space_reserved(mps_space_t);
extern size_t mps_space_committed(mps_space_t);

/* Client memory arenas */
extern mps_res_t mps_arena_extend(mps_arena_t, mps_addr_t, size_t);
extern mps_res_t mps_arena_retract(mps_arena_t, mps_addr_t, size_t);


/* Object Formats */

extern mps_res_t mps_fmt_create_A(mps_fmt_t *, mps_arena_t,
                                  mps_fmt_A_t);
extern mps_res_t mps_fmt_create_B(mps_fmt_t *, mps_arena_t,
                                  mps_fmt_B_t);
extern void mps_fmt_destroy(mps_fmt_t);


/* Pools */

extern mps_res_t mps_pool_create(mps_pool_t *, mps_arena_t,
                                 mps_class_t, ...);
extern mps_res_t mps_pool_create_v(mps_pool_t *, mps_arena_t,
                                   mps_class_t, va_list);
extern void mps_pool_destroy(mps_pool_t);

extern mps_res_t mps_alloc(mps_addr_t *, mps_pool_t, size_t, ...);
extern mps_res_t mps_alloc_v(mps_addr_t *, mps_pool_t, size_t,
                             va_list);
extern void mps_free(mps_pool_t, mps_addr_t, size_t);


/* Allocation Points */

extern mps_res_t mps_ap_create(mps_ap_t *, mps_pool_t, ...);
extern mps_res_t mps_ap_create_v(mps_ap_t *, mps_pool_t, va_list);
extern void mps_ap_destroy(mps_ap_t);

extern mps_res_t (mps_reserve)(mps_addr_t *, mps_ap_t, size_t);
extern mps_bool_t (mps_commit)(mps_ap_t, mps_addr_t, size_t);

extern mps_res_t mps_ap_fill(mps_addr_t *, mps_ap_t, size_t);
extern mps_bool_t mps_ap_trip(mps_ap_t, mps_addr_t, size_t);

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
extern mps_bool_t mps_message_get(mps_message_t *,
                                  mps_arena_t, mps_message_type_t);
extern void mps_message_discard(mps_arena_t, mps_message_t);
extern mps_bool_t mps_message_queue_type(mps_message_type_t *, mps_arena_t);
extern mps_message_type_t mps_message_type(mps_arena_t, mps_message_t);

/* Message Type Specific Methods */

/* MPS_MESSAGE_TYPE_FINALIZATION */

extern void mps_message_finalization_ref(mps_addr_t *,
					 mps_arena_t, mps_message_t);


/* Finalization */

extern mps_res_t mps_finalize(mps_arena_t, mps_addr_t *);
extern void mps_definalize(mps_arena_t, mps_addr_t *);


/* Telemetry */

mps_word_t mps_telemetry_control(mps_word_t, mps_word_t);
mps_word_t mps_telemetry_intern(char *);
void mps_telemetry_label(mps_addr_t, mps_word_t);


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

#define MPS_FIX(ss, ref_io) \
  (MPS_FIX1(ss, *(ref_io)) ? \
   MPS_FIX2(ss, ref_io) : MPS_RES_OK)

#define MPS_FIX_CALL(ss, call) \
  MPS_BEGIN \
    (call); _mps_w2 |= (ss)->w2; \
  MPS_END

#define MPS_SCAN_END(ss) \
   } \
   (ss)->w2 = _mps_w2; \
  MPS_END


#endif /* mps_h */
