/* impl.c.mpsi: MEMORY POOL SYSTEM INTERFACE LAYER */

#include "std.h"
#include "mps.h"
#include "ref.h"
#include "space.h"
#include "format.h"
#include "pool.h"
#include "poolclas.h"
#include "buffer.h"
#include "root.h"
#include "prot.h"
#include "th.h"
#include "mps.h"
#include <stdarg.h>
#include <stddef.h>


/* Interface Checking */

#ifdef DEBUG_ASSERT      /* @@@@ do this? */
#define TARGET_MPSI_ASSERT
#endif

#ifdef TARGET_MPSI_ASSERT
#define mpsi_aver(_cond)        AVER(_cond) 
#else
#define mpsi_aver(_cond)        NOOP
#endif


/* Version Information */

static mps_ver_s ver_s =
{
  "Harlequin Memory Pool System\n"
  "Copyright 1996 Harlequin Group, all rights reserved.\n",
  (unsigned)1,
  (unsigned)0,
  (unsigned)0   /* @@@@ Calculate from build somehow. */
};


/* Check consistency of interface mappings. */

#ifdef TARGET_MPSI_ASSERT
static Bool mpsi_check(void)
{
  /* Check that external and internal result codes match. */
  AVER(MPS_RES_OK == ErrSUCCESS);
  AVER(MPS_RES_FAIL == ErrFAILURE);
  AVER(MPS_RES_RESOURCE == ErrRESOURCE);
  AVER(MPS_RES_MEMORY == ErrRESMEM);
  AVER(MPS_RES_LIMIT == ErrLIMIT);
  AVER(MPS_RES_UNIMPL == ErrUNIMPL);
  AVER(MPS_RES_IO == ErrIO);

  /* Check that external and internal rank numbers match. */
  AVER(MPS_RANK_AMBIG == RefRankAMBIG);
  AVER(MPS_RANK_EXACT == RefRankEXACT);
  AVER(MPS_RANK_WEAK == RefRankWEAK);
  AVER(MPS_RANK_FINAL == RefRankFINAL);
  
  /* The external idea of a word width and the internal one */
  /* had better match. */
  AVER(MPS_WORD_WIDTH == ADDRWIDTH);
  AVER(sizeof(mps_word_t) == sizeof(void *));
  AVER(sizeof(mps_word_t) == sizeof(Addr));

  /* Check ap_s/ApStruct compatibility by hand */
  AVER(offsetof(mps_ap_s, init) == offsetof(ApStruct, init));
  AVER(offsetof(mps_ap_s, alloc) == offsetof(ApStruct, alloc));
  AVER(offsetof(mps_ap_s, limit) == offsetof(ApStruct, limit));

  return TRUE;
}
#endif

mps_ver_t mps_ver(void)
{
  return &ver_s;
}

mps_assert_t mps_assert_install(mps_assert_t handler)
{
  mpsi_aver(handler != NULL);
  return AssertInstall(handler);
}

mps_assert_t mps_assert_default(void)
{
  return AssertDefault();
}

mps_res_t mps_space_create(mps_space_t *mps_space_o)
{
  Space *spaceReturn = (Space *)mps_space_o;
  /* This is the first real call that the client will have to make, */
  /* so check static consistency here. */
  mpsi_aver(mpsi_check());
  mpsi_aver(spaceReturn != NULL);
  return SpaceCreate(spaceReturn);
}

void mps_space_destroy(mps_space_t mps_space)
{
  Space space = (Space)mps_space;
  mpsi_aver(ISVALID(Space, space));
  SpaceDestroy(space);
}

mps_res_t mps_form_create_A(mps_form_t *mps_form_o,
                            mps_space_t mps_space,
                            mps_form_A_t mps_form_A)
{
  Format *formatReturn = (Format *)mps_form_o;
  Error res;

  res = FormatCreate(formatReturn,
                     (Space)mps_space,
                     (Addr)mps_form_A->align,
                     (FormatScanMethod)mps_form_A->scan,
                     (FormatSkipMethod)mps_form_A->skip,
                     (FormatMoveMethod)mps_form_A->fwd,
                     (FormatIsMovedMethod)mps_form_A->isfwd,
                     (FormatCopyMethod)mps_form_A->copy);
  return res;
}
                              
void mps_form_destroy(mps_form_t mps_form)
{
  FormatDestroy((Format)mps_form);
}

mps_res_t mps_pool_create(mps_pool_t *mps_pool_o,
                          mps_space_t mps_space,
                          mps_class_t mps_class, ...)
{
  Pool *poolReturn = (Pool *)mps_pool_o;
  Space space = (Space)mps_space;
  PoolClass class = (PoolClass)mps_class;
  va_list args;
  Error res;

  mpsi_aver(poolReturn != NULL);
  mpsi_aver(ISVALID(Space, space));
  mpsi_aver(ISVALID(PoolClass, class));

  va_start(args, size);
  res = PoolCreateV(poolReturn, class, space, args);
  va_end(args);

  return res;
}

mps_res_t mps_pool_create_v(mps_pool_t *mps_pool_o,
                            mps_space_t mps_space,
                            mps_class_t mps_class,
                            va_list args)
{
  Pool *poolReturn = (Pool *)mps_pool_o;
  Space space = (Space)mps_space;
  PoolClass class = (PoolClass)mps_class;
  Error res;

  mpsi_aver(poolReturn != NULL);
  mpsi_aver(ISVALID(Space, space));
  mpsi_aver(ISVALID(PoolClass, class));

  res = PoolCreateV(poolReturn, class, space, args);

  return res;
}

void mps_pool_destroy(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  mpsi_aver(ISVALID(Pool, pool));
  PoolDestroy(pool);
}

mps_res_t mps_alloc(mps_addr_t *p_o,
                    mps_pool_t mps_pool,
                    size_t size, ...)
{
  Pool pool = (Pool)mps_pool;
  Error res;

  mpsi_aver(p_o != NULL);
  mpsi_aver(ISVALID(Pool, pool));
  mpsi_aver(size > 0);
  /* Note: class may allow unaligned size. */

  /* varargs ignored */
  res = PoolAlloc((Addr *)p_o, pool, size);
  
  return res;
}

mps_res_t mps_alloc_v(mps_addr_t *p_o,
                      mps_pool_t mps_pool,
                      size_t size, va_list args)
{
  Pool pool = (Pool)mps_pool;
  Error res;

  mpsi_aver(p_o != NULL);
  mpsi_aver(ISVALID(Pool, pool));
  mpsi_aver(size > 0);
  UNUSED(args);
  /* Note: class may allow unaligned size. */

  res = PoolAlloc((Addr *)p_o, pool, size);
  return res;
}

void mps_free(mps_pool_t mps_pool, mps_addr_t p, size_t size)
{
  Pool pool = (Pool)mps_pool;

  mpsi_aver(ISVALID(Pool, pool));
  mpsi_aver(size > 0);
  /* Note: class may allow unaligned size. */
  /* @@@@ Check that p belongs to pool? */

  PoolFree(pool, (Addr)p, size);
}

mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t mps_pool, ...)
{
  Ap *apReturn = (Ap *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Buffer buf;
  Error res;

  mpsi_aver(apReturn != NULL);
  mpsi_aver(ISVALID(Pool, pool));

  /* varargs ignored */
  res = BufferCreate(&buf, pool);
  if(res != MPS_RES_OK)
    return res;
  *apReturn = BufferAp(buf);
  return MPS_RES_OK;
}

mps_res_t mps_ap_create_v(mps_ap_t *mps_ap_o,
                          mps_pool_t mps_pool,
                          va_list args)
{
  Ap *apReturn = (Ap *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Buffer buf;
  Error res;

  mpsi_aver(apReturn != NULL);
  mpsi_aver(ISVALID(Pool, pool));
  UNUSED(args);

  res = BufferCreate(&buf, pool);
  if(res != MPS_RES_OK)
    return res;
  *apReturn = BufferAp(buf);
  return MPS_RES_OK;
}

void mps_ap_destroy(mps_ap_t mps_ap)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  mpsi_aver(ISVALID(Buffer, buf));
  BufferDestroy(buf);
}

mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);

  mpsi_aver(p_o != NULL);
/*  mpsi_aver(ISVALID(Buffer, buf));  can't do this - thread safe */
  mpsi_aver(size > 0);
  mpsi_aver(IsAligned(BufferPool(buf)->alignment, size));
    
  return (BufferReserve)((Addr *)p_o, buf, size);
}

mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);

/*  mpsi_aver(ISVALID(Buffer, buf));  can't do this - thread safe */
  mpsi_aver(size > 0);
  mpsi_aver(IsAligned(BufferPool(buf)->alignment, size));

  return (BufferCommit)(buf, (Addr)p, size);
}

mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Error res;

  mpsi_aver(p_o != NULL);
/*  mpsi_aver(ISVALID(Buffer, buf));  can't do this - thread safe */
  mpsi_aver(size > 0);
  mpsi_aver(IsAligned(BufferPool(buf)->alignment, size));

  res = BufferFill((Addr *)p_o, buf, size);
  return res;
}

mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Bool b;

/*  mpsi_aver(ISVALID(Buffer, buf));  can't do this - thread safe */
  mpsi_aver(size > 0);
  mpsi_aver(IsAligned(BufferPool(buf)->alignment, size));

  b = BufferTrip(buf, (Addr)p, size);
  return b;
}

mps_res_t mps_root_create(mps_root_t *mps_root_o,
                          mps_space_t mps_space,
                          mps_rank_t mps_rank,
                          mps_root_scan_t mps_root_scan,
                          void *p, size_t s)
{
  Root *rootReturn = (Root *)mps_root_o;
  Space space = (Space)mps_space;
  RefRank rank = (RefRank)mps_rank;
  Error res;

  mpsi_aver(rootReturn != NULL);
  mpsi_aver(ISVALID(Space, space));
  mpsi_aver(mps_root_scan != NULL);
  res = RootCreate(rootReturn, space, rank, /* mode */ 0,
    (Error (*)(ScanState, void *, size_t))mps_root_scan, p, s);
  return res;
}

mps_res_t mps_root_create_table(mps_root_t *mps_root_o,
                                mps_space_t mps_space,
                                mps_rank_t mps_rank,
                                mps_rm_t mps_rm,
                                mps_addr_t *base, size_t size)
{
  Root *rootReturn = (Root *)mps_root_o;
  Space space = (Space)mps_space;
  RefRank rank = (RefRank)mps_rank;
  Error res;

  mpsi_aver(mps_root_o != NULL);
  mpsi_aver(ISVALID(Space, space));
  mpsi_aver(base != NULL);
  mpsi_aver((unsigned long)size > 0);
  mpsi_aver(IsAligned(sizeof(mps_addr_t), size)); /* ?? */
  res = RootCreateTable(rootReturn, space, rank,
      /* mode */ 0, (Addr *)base, (Addr *)base + size);
  return res;
}

mps_res_t mps_root_create_form(mps_root_t *mps_root_o,
                               mps_space_t mps_space,
                               mps_rank_t mps_rank,
                               mps_rm_t mps_rm,
                               mps_form_scan_t mps_form_scan,
                               mps_addr_t base,
                               mps_addr_t limit)
{
  NOTREACHED;
  return MPS_RES_UNIMPL;
}

mps_res_t mps_root_create_reg(mps_root_t *mps_root_o,
                              mps_space_t mps_space,
                              mps_rank_t mps_rank,
                              mps_thr_t mps_thr,
                              mps_reg_scan_t mps_reg_scan,
                              void *reg_scan_p)
{
  NOTREACHED;
  return MPS_RES_UNIMPL;
}

void mps_root_destroy(mps_root_t mps_root)
{
  Root root = (Root)mps_root;
  mpsi_aver(ISVALID(Root, root));
  RootDestroy(root);
}

void (mps_tramp)(void **r_o,
                 void *(*f)(void *p, size_t s),
                 void *p,
                 size_t s)
{
  ProtTramp(r_o, f, p, s);
}

mps_res_t mps_thread_reg(mps_thr_t *mps_thr_o,
                         mps_space_t mps_space)
{
  Thread *threadReturn = (Thread *)mps_thr_o;
  Space space = (Space)mps_space;
  Error res;

  mpsi_aver(mps_thr_o != NULL);
  mpsi_aver(ISVALID(Space, space));
  res = ThreadRegister(threadReturn, space, NULL); /* @@@@ wrong */
  return res;
}

void mps_thread_dereg(mps_thr_t mps_thr)
{
  Thread thread = (Thread)mps_thr;
  Space space = ThreadSpace(thread);
  ThreadDeregister(thread, space);
}

/* @@@@ not done from here on */
void mps_ld_init(mps_ld_t mps_ld, mps_space_t space)
{
  NOTREACHED;
}

void mps_ld_finish(mps_ld_t mps_ld)
{
  NOTREACHED;
}

void mps_ld_add(mps_ld_t mps_ld, mps_space_t mps_space, mps_addr_t addr)
{
  NOTREACHED;
}

mps_bool_t mps_ld_isinvalid(mps_ld_t mps_ld,
                            mps_space_t mps_space,
                            mps_addr_t addr)
{
  NOTREACHED;
  return TRUE;
}

mps_bool_t mps_msg_next(mps_space_t mps_space,
                    mps_mc_t *mps_mc_o,
                    void **msg_data_o)
{
  NOTREACHED;
  return FALSE;
}

mps_bool_t mps_msg_peek(mps_space_t mps_space,
                    mps_mc_t *mps_mc_o,
                    void **msg_data_o)
{
  NOTREACHED;
  return FALSE;
}

mps_msg_handler_t mps_msg_handler(mps_space_t mps_space,
                                  mps_msg_handler_t mps_handler)
{
  NOTREACHED;
  return NULL;
}

mps_res_t mps_fix(mps_ss_t mps_ss, mps_addr_t *ref_io)
{
  mps_res_t e;

  MPS_SCAN_BEGIN(mps_ss) {
  e = MPS_FIX(mps_ss, ref_io);
  } MPS_SCAN_END(mps_ss);

  return e;
}
