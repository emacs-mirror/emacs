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


/* Check consistency of interface mappings. */

#ifdef DEBUG_ASSERT

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
  AVER(sizeof(mps_ap_s) == sizeof(ApStruct));
  AVER(offsetof(mps_ap_s, init) == offsetof(ApStruct, init));
  AVER(offsetof(mps_ap_s, alloc) == offsetof(ApStruct, alloc));
  AVER(offsetof(mps_ap_s, limit) == offsetof(ApStruct, limit));

  /* Check ss_s/ScanStateStruct compatibility by hand */
  AVER(sizeof(mps_ss_s) == sizeof(ScanStateStruct));
  AVER(offsetof(mps_ss_s, fix) == offsetof(ScanStateStruct, fix));
  AVER(offsetof(mps_ss_s, w0) == offsetof(ScanStateStruct, zoneShift));
  AVER(offsetof(mps_ss_s, w1) == offsetof(ScanStateStruct, condemned));
  AVER(offsetof(mps_ss_s, w2) == offsetof(ScanStateStruct, summary));

  return TRUE;
}

#endif /* DEBUG_ASSERT */

mps_assert_t mps_assert_install(mps_assert_t handler)
{
  AVER(handler != NULL);
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
  AVER(mpsi_check());
  AVER(spaceReturn != NULL);
  return SpaceCreate(spaceReturn);
}

void mps_space_destroy(mps_space_t mps_space)
{
  Space space = (Space)mps_space;
  AVER(ISVALID(Space, space));
  SpaceDestroy(space);
}

mps_res_t mps_form_create_A(mps_form_t *mps_form_o,
                            mps_space_t mps_space,
                            mps_form_A_t mps_form_A)
{
  Format *formatReturn = (Format *)mps_form_o;
  Error e;
  AVER(mps_form_A != NULL);
  e = FormatCreate(formatReturn,
                   (Space)mps_space,
                   (Addr)mps_form_A->align,
                   (FormatScanMethod)mps_form_A->scan,
                   (FormatSkipMethod)mps_form_A->skip,
                   (FormatMoveMethod)mps_form_A->fwd,
                   (FormatIsMovedMethod)mps_form_A->isfwd,
                   (FormatCopyMethod)mps_form_A->copy);
  return e;
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
  Error e;

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(ISVALID(PoolClass, class));

  va_start(args, mps_class);
  e = PoolCreateV(poolReturn, class, space, args);
  va_end(args);

  return e;
}

mps_res_t mps_pool_create_v(mps_pool_t *mps_pool_o,
                            mps_space_t mps_space,
                            mps_class_t mps_class,
                            va_list args)
{
  Pool *poolReturn = (Pool *)mps_pool_o;
  Space space = (Space)mps_space;
  PoolClass class = (PoolClass)mps_class;
  Error e;

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(ISVALID(PoolClass, class));

  e = PoolCreateV(poolReturn, class, space, args);

  return e;
}

void mps_pool_destroy(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  AVER(ISVALID(Pool, pool));
  PoolDestroy(pool);
}

mps_res_t mps_alloc(mps_addr_t *p_o,
                    mps_pool_t mps_pool,
                    size_t size, ...)
{
  Pool pool = (Pool)mps_pool;
  Error e;

  AVER(p_o != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(size > 0);
  /* Note: class may allow unaligned size. */

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = PoolAlloc((Addr *)p_o, pool, size);
  
  return e;
}

mps_res_t mps_alloc_v(mps_addr_t *p_o,
                      mps_pool_t mps_pool,
                      size_t size, va_list args)
{
  Pool pool = (Pool)mps_pool;
  Error e;

  AVER(p_o != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(size > 0);
  UNUSED(args);
  /* Note: class may allow unaligned size. */

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = PoolAlloc((Addr *)p_o, pool, size);
  return e;
}

void mps_free(mps_pool_t mps_pool, mps_addr_t p, size_t size)
{
  Pool pool = (Pool)mps_pool;

  AVER(ISVALID(Pool, pool));
  AVER(size > 0);
  /* Note: class may allow unaligned size. */

  PoolFree(pool, (Addr)p, size);
}

mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t mps_pool, ...)
{
  Ap *apReturn = (Ap *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Buffer buf;
  Error e;

  AVER(apReturn != NULL);
  AVER(ISVALID(Pool, pool));

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = BufferCreate(&buf, pool);
  if(e != ErrSUCCESS)
    return e;

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
  Error e;

  AVER(apReturn != NULL);
  AVER(ISVALID(Pool, pool));
  UNUSED(args);

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = BufferCreate(&buf, pool);
  if(e != ErrSUCCESS)
    return e;

  *apReturn = BufferAp(buf);
  return MPS_RES_OK;
}

void mps_ap_destroy(mps_ap_t mps_ap)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  AVER(ISVALID(Buffer, buf));
  BufferDestroy(buf);
}

mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);

  AVER(p_o != NULL);
  AVER(ISVALID(Buffer, buf));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buf)->alignment, size));
    
  return (BufferReserve)((Addr *)p_o, buf, size);
}

mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);

  AVER(ISVALID(Buffer, buf));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buf)->alignment, size));

  return (BufferCommit)(buf, (Addr)p, size);
}

mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Error e;

  AVER(p_o != NULL);
  AVER(ISVALID(Buffer, buf));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buf)->alignment, size));

  e = BufferFill((Addr *)p_o, buf, size);
  return e;
}

mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Bool b;

  AVER(ISVALID(Buffer, buf));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buf)->alignment, size));

  b = BufferTrip(buf, (Addr)p, size);
  return b;
}

mps_res_t mps_root_create(mps_root_t *mps_root_o,
                          mps_space_t mps_space,
                          mps_rank_t mps_rank,
                          mps_rm_t mps_rm,
                          mps_root_scan_t mps_root_scan,
                          void *p, size_t s)
{
  Root *rootReturn = (Root *)mps_root_o;
  Space space = (Space)mps_space;
  RefRank rank = (RefRank)mps_rank;

  AVER(rootReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(mps_root_scan != NULL);
  AVER(mps_rm = (mps_rm_t)0);

  /* The root mode is ignored. */
  return RootCreate(rootReturn, space, rank,
                    (RootScanMethod)mps_root_scan, p, s);
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

  AVER(mps_root_o != NULL);
  AVER(ISVALID(Space, space));
  AVER(base != NULL);
  AVER((unsigned long)size > 0);
  /* Note, size is the length of the array at base, not */
  /* the size in bytes.  However, RootCreateTable expects */
  /* base and limit pointers.  Be careful. */
  return RootCreateTable(rootReturn, space, rank,
                         (Addr *)base, (Addr *)base + size);
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
                              mps_rm_t mps_rm,
                              mps_thr_t mps_thr,
                              mps_reg_scan_t mps_reg_scan,
                              void *reg_scan_p)
{
  Root *rootReturn = (Root *)mps_root_o;
  Space space = (Space)mps_space;
  RefRank rank = (RefRank)mps_rank;
  Thread thread = (Thread)mps_thr;

  AVER(mps_root_o != NULL);
  AVER(ISVALID(Space, space));
  AVER(ISVALID(Thread, thread));
  AVER(mps_reg_scan != NULL);

  /* At present, we only support register scanning using our */
  /* own ambiguous register and stack scanning method. */
  AVER(mps_reg_scan == mps_stack_scan_ambig);
  AVER(reg_scan_p != NULL); /* stackBot */
  AVER(rank == MPS_RANK_AMBIG);
  AVER(mps_rm == (mps_rm_t)0);

  return RootCreateReg(rootReturn, space, rank, thread,
                       (RootScanRegMethod)mps_reg_scan,
                       reg_scan_p);
}

mps_res_t mps_stack_scan_ambig(mps_ss_t mps_ss,
                               mps_reg_t mps_reg, void *p)
{
  ScanState ss = (ScanState)mps_ss;
  Thread thread = (Thread)mps_reg;
  return ThreadScan(ss, thread, p);
}

void mps_root_destroy(mps_root_t mps_root)
{
  Root root = (Root)mps_root;
  AVER(ISVALID(Root, root));
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
  AVER(mps_thr_o != NULL);
  AVER(ISVALID(Space, space));
  return ThreadRegister(threadReturn, space);
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
  mps_res_t res;

  MPS_SCAN_BEGIN(mps_ss) {
    res = MPS_FIX(mps_ss, ref_io);
  } MPS_SCAN_END(mps_ss);

  return res;
}
