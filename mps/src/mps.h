/* impl.h.mps: HARLEQUIN MEMORY POOL SYSTEM INTERFACE
 *
 * $HopeName: MMsrc!mps.h(trunk.15) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
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
#ifdef MPS_OS_W3
#include <windows.h>            /* needed for SEH filter type */
#endif /* MPS_OS_W3 */


/* Abstract Types */

typedef struct mps_space_s  *mps_space_t;  /* space */
typedef struct mps_pool_s   *mps_pool_t;   /* pool */
typedef struct mps_fmt_s    *mps_fmt_t;    /* object format */
typedef struct mps_root_s   *mps_root_t;   /* root */
typedef struct mps_class_s  *mps_class_t;  /* pool class */
typedef struct mps_thr_s    *mps_thr_t;    /* thread registration */
typedef struct mps_ap_s     *mps_ap_t;     /* allocation point */
typedef struct mps_ld_s     *mps_ld_t;     /* location dependency */
typedef struct mps_reg_s    *mps_reg_t;    /* register file */
typedef struct mps_ss_s     *mps_ss_t;     /* scan state */


/* Concrete Types */

typedef MPS_T_WORD mps_word_t;  /* machine word (target dep.) */
typedef int mps_bool_t;         /* boolean (int) */
typedef int mps_res_t;          /* result code (int) */
typedef unsigned mps_shift_t;   /* shift amount (unsigned int) */
typedef void *mps_addr_t;       /* managed address (void *) */
typedef size_t mps_align_t;     /* alignment (size_t) */
typedef unsigned mps_rm_t;      /* root mode (unsigned) */
typedef unsigned mps_rank_t;	/* ranks (unsigned) */


/* Result Codes */
/* .result-codes: Keep in sync with impl.h.mpmtypes.result-codes */

enum {
  MPS_RES_OK = 0,               /* success (always zero) */
  MPS_RES_FAIL,                 /* unspecified failure */
  MPS_RES_RESOURCE,             /* unable to obtain resources */
  MPS_RES_MEMORY,               /* unable to obtain memory */
  MPS_RES_LIMIT,                /* internal limitation reached */
  MPS_RES_UNIMPL,               /* unimplemented facility */
  MPS_RES_IO                    /* system I/O error */
};


/* Reference Ranks */
/* .ranks: Keep in sync with impl.h.mpmtypes.ranks */

enum {
  MPS_RANK_AMBIG = 0,           /* ambiguous reference */
  MPS_RANK_EXACT = 1,           /* exact reference */
  MPS_RANK_WEAK = 2,            /* weak reference */
  MPS_RANK_FINAL = 3            /* final reference */
};


/* Root Modes */
/* .rm: Keep in sync with impl.h.mpmtypes.rm */

#define MPS_RM_CONST    ((mps_rm_t)1)
#define MPS_RM_PROT     ((mps_rm_t)2)


/* Allocation Point */
/* .ap: Keep in sync with impl.h.mpmst.ap. */

typedef struct mps_ap_s {       /* allocation point descriptor */
  mps_addr_t init;              /* limit of initialized memory */
  mps_addr_t alloc;             /* limit of reserved memory */
  mps_addr_t limit;             /* limit of buffered memory */
} mps_ap_s;


/* Location Dependency */
/* .ld: Keep in sync with impl.h.mpmst.ld.struct. */

typedef struct mps_ld_s {       /* location dependency descriptor */
  mps_word_t w0, w1;
} mps_ld_s;


/* Format and Root Method Types */
/* .fmt-methods: Keep in sync with impl.h.mpmtypes.fmt-methods */

typedef mps_res_t  (*mps_root_scan_t)  (mps_ss_t mps_ss,
                                        void *p, size_t s);
typedef mps_res_t  (*mps_fmt_scan_t)   (mps_ss_t mps_ss,
                                        mps_addr_t base,
                                        mps_addr_t limit);
typedef mps_res_t  (*mps_reg_scan_t)   (mps_ss_t mps_ss,
                                        mps_thr_t mps_thr,
                                        void *p,
                                        size_t s);
typedef mps_addr_t (*mps_fmt_skip_t)   (mps_addr_t object);
typedef void       (*mps_fmt_copy_t)   (mps_addr_t old, mps_addr_t new);
typedef void       (*mps_fmt_fwd_t)    (mps_addr_t old, mps_addr_t new);
typedef mps_addr_t (*mps_fmt_isfwd_t)  (mps_addr_t object);
typedef void       (*mps_fmt_pad_t)    (mps_addr_t base, size_t size);


/* Scan State */
/* .ss: See also impl.c.mpsi.check.ss and impl.h.mpmst.ss. */

typedef struct mps_ss_s {
  mps_res_t (*fix)(mps_ss_t mps_ss, mps_addr_t *ref_io);
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


/* Internal Definitions */

#define MPS_BEGIN       do {
#define MPS_END         } while(0)

extern mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap,
                             size_t size);
extern mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p,
                              size_t size);

#ifdef MPS_OS_W3
extern LONG mps_SEH_filter(LPEXCEPTION_POINTERS info,
                           void **hp_o, size_t *hs_o);
extern void mps_SEH_handler(void *p, size_t s);
#endif /* MPS_OS_W3 */


/* Assertion Handling */

typedef void (*mps_assert_t)(const char *cond,
                             const char *id,
                             const char *file,
                             unsigned line);

extern mps_assert_t mps_assert_install(mps_assert_t handler);
extern mps_assert_t mps_assert_default(void);


/* Spaces */

extern mps_res_t mps_space_create_wmem(mps_space_t *mps_space_o,
                                       mps_addr_t base, size_t size);
extern mps_res_t mps_space_create(mps_space_t *mps_space_o);
extern void mps_space_destroy(mps_space_t mps_space);


/* Object Formats */

extern mps_res_t mps_fmt_create_A(mps_fmt_t *mps_fmt_o,
                                  mps_space_t mps_space,
                                  mps_fmt_A_t mps_fmt_A);
extern void mps_fmt_destroy(mps_fmt_t mps_fmt);


/* Pools */

extern mps_res_t mps_pool_create(mps_pool_t *mps_pool_o,
                                 mps_space_t mps_space,
                                 mps_class_t class, ...);
extern mps_res_t mps_pool_create_v(mps_pool_t *mps_pool_o,
                                   mps_space_t mps_space,
                                   mps_class_t class,
                                   va_list args);
extern void mps_pool_destroy (mps_pool_t mps_pool);

extern mps_res_t mps_alloc(mps_addr_t *p_o,
                           mps_pool_t mps_pool,
                           size_t size, ...);
extern mps_res_t mps_alloc_v(mps_addr_t *p_o,
                             mps_pool_t mps_pool,
                             size_t size,
                             va_list args);
extern void mps_free(mps_pool_t mps_pool, mps_addr_t p, size_t size);


/* Allocation Points */

extern mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t mps_pool,
                               mps_rank_t mps_rank, ...);
extern mps_res_t mps_ap_create_v(mps_ap_t *mps_ap_o, mps_pool_t mps_pool,
                                 mps_rank_t mps_rank, va_list args);
extern void mps_ap_destroy(mps_ap_t mps_ap);

extern mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap,
                               size_t size);
extern mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p,
                               size_t size);

#define mps_reserve(_p_o, _mps_ap, _size) \
  ((char *)(_mps_ap)->alloc + (_size) > (char *)(_mps_ap)->alloc && \
   (char *)(_mps_ap)->alloc + (_size) <= (char *)(_mps_ap)->limit ? \
     ((_mps_ap)->alloc = \
       (mps_addr_t)((char *)(_mps_ap)->alloc + (_size)), \
      *(_p_o) = (_mps_ap)->init, \
      MPS_RES_OK) : \
     mps_ap_fill(_p_o, _mps_ap, _size))

#define mps_commit(_mps_ap, _p, _size) \
  ((_mps_ap)->init = (_mps_ap)->alloc, \
   (_mps_ap)->limit != 0 || mps_ap_trip(_mps_ap, _p, _size))

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


/* Root Creation and Destruction */

extern mps_res_t mps_root_create(mps_root_t *mps_root_o,
                                 mps_space_t mps_space,
                                 mps_rank_t mps_rank,
                                 mps_rm_t mps_rm,
                                 mps_root_scan_t mps_root_scan,
                                 void *p, size_t s);
extern mps_res_t mps_root_create_table(mps_root_t *mps_root_o,
                                       mps_space_t mps_space,
                                       mps_rank_t mps_rank,
                                       mps_rm_t mps_rm,
                                       mps_addr_t *base, size_t size);
extern mps_res_t mps_root_create_fmt(mps_root_t *mps_root_o,
                                     mps_space_t mps_space,
                                     mps_rank_t mps_rank,
                                     mps_rm_t mps_rm,
                                     mps_fmt_scan_t mps_fmt_scan,
                                     mps_addr_t base,
                                     mps_addr_t limit);
extern mps_res_t mps_root_create_reg(mps_root_t *mps_root_o,
                                     mps_space_t mps_space,
                                     mps_rank_t mps_rank,
                                     mps_rm_t mps_rm,
                                     mps_thr_t mps_thr,
                                     mps_reg_scan_t mps_reg_scan,
                                     void *reg_scan_p,
                                     size_t mps_size);
extern void mps_root_destroy(mps_root_t root);

extern mps_res_t mps_stack_scan_ambig(mps_ss_t ss, mps_thr_t thr,
                                      void *p, size_t s);


/* Protection Trampoline and Thread Registration */

extern void (mps_tramp)(void **r_o,
                        void *(*f)(void *p, size_t s),
                        void *p,
                        size_t s);

#ifdef MPS_OS_W3

#define mps_tramp(r_o, f, p, s) \
  MPS_BEGIN \
    void **_r_o = (r_o); \
    void *(*_f)(void *, size_t) = (f); \
    void *_p = (p); \
    size_t _s = (s); \
    void *_hp; size_t _hs; \
    __try { \
      *(_r_o) = (*(_f))(_p, _s); \
    } __except(mps_SEH_filter(GetExceptionInformation(), \
               &_hp, &_hs)) { \
      mps_SEH_handler(_hp, _hs); \
    } \
  MPS_END

#else /* MPS_OS_W3 */

#define mps_tramp(r_o, f, p, s) \
  MPS_BEGIN \
    void **_r_o = (r_o); \
    void *(*_f)(void *, size_t) = (f); \
    void *_p = (p); \
    size_t _s = (s); \
    *(_r_o) = (*(_f))(_p, _s); \
  MPS_END

#endif /* MPS_OS_W3 */

extern mps_res_t mps_thread_reg(mps_thr_t *mps_thr_o,
                                mps_space_t mps_space);
extern void mps_thread_dereg(mps_thr_t mps_thr);


/* Location Dependency */

extern void mps_ld_reset(mps_ld_t mps_ld, mps_space_t space);
extern void mps_ld_add(mps_ld_t mps_ld,
                       mps_space_t mps_space,
                       mps_addr_t addr);
extern mps_bool_t mps_ld_isstale(mps_ld_t mps_ld,
                                 mps_space_t mps_space,
                                 mps_addr_t addr);

extern mps_word_t mps_collections(mps_space_t mps_space);


/* Scanner Support */

extern mps_res_t mps_fix(mps_ss_t mps_ss, mps_addr_t *ref_io);

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

#define MPS_SCAN_END(ss) \
   } \
   (ss)->w2 = _mps_w2; \
  MPS_END


#endif /* mps_h */
