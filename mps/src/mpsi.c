/* impl.c.mpsi: MEMORY POOL SYSTEM INTERFACE LAYER
 *
 * $HopeName: MMsrc!mpsi.c(trunk.10) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .thread-safety: Most calls through this interface lock the space
 * and therefore make the MPM single-threaded.  In order to do this
 * they must recover the space from their parameters.  Methods such
 * as ThreadSpace() must therefore be callable when the space is
 * _not_ locked.  These methods are tagged with the tag of this note.
 */

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
#include "ld.h"
#include "mps.h"
#include <stdarg.h>
#include <stddef.h>

SRCID("$HopeName: MMsrc!mpsi.c(trunk.10) $");


/* Check consistency of interface mappings. */

#define CHECKLVALUE(lv1, lv2) \
  (sizeof((lv1) = (lv2)), sizeof((lv2) = (lv1)), TRUE)

#define CHECKTYPE(t1, t2) \
  (sizeof(t1) == sizeof(t2) && \
   CHECKLVALUE(*((t1 *)0), *((t2 *)0)))

#define CHECKFIELDAPPROX(s1, f1, s2, f2) \
  (sizeof(((s1 *)0)->f1) == sizeof(((s2 *)0)->f2) && \
   offsetof(s1, f1) == offsetof(s2, f2))

#define CHECKFIELD(s1, f1, s2, f2) \
  (CHECKFIELDAPPROX(s1, f1, s2, f2) && \
   CHECKLVALUE(((s1 *)0)->f1, ((s2 *)0)->f2))

static Bool mpsi_check(void)
{
  /* Check that external and internal result codes match. */
  AVER(CHECKTYPE(mps_res_t, Error));
  AVER(MPS_RES_OK == ErrSUCCESS);
  AVER(MPS_RES_FAIL == ErrFAILURE);
  AVER(MPS_RES_RESOURCE == ErrRESOURCE);
  AVER(MPS_RES_MEMORY == ErrRESMEM);
  AVER(MPS_RES_LIMIT == ErrLIMIT);
  AVER(MPS_RES_UNIMPL == ErrUNIMPL);
  AVER(MPS_RES_IO == ErrIO);

  /* Check that external and internal rank numbers match. */
  AVER(CHECKTYPE(mps_rank_t, RefRank));
  AVER(MPS_RANK_AMBIG == RefRankAMBIG);
  AVER(MPS_RANK_EXACT == RefRankEXACT);
  AVER(MPS_RANK_WEAK == RefRankWEAK);
  AVER(MPS_RANK_FINAL == RefRankFINAL);
  
  /* The external idea of a word width and the internal one */
  /* had better match. */
  AVER(MPS_WORD_WIDTH == ADDRWIDTH);
  AVER(sizeof(mps_word_t) == sizeof(void *));
  AVER(CHECKTYPE(mps_word_t, Addr));

  /* Check ap_s/ApStruct compatibility by hand */
  /* .check.ap: See also impl.h.buffer.ap. */
  AVER(sizeof(mps_ap_s) == sizeof(ApStruct));
  AVER(CHECKFIELDAPPROX(mps_ap_s, init,  ApStruct, init));
  AVER(CHECKFIELDAPPROX(mps_ap_s, alloc, ApStruct, alloc));
  AVER(CHECKFIELDAPPROX(mps_ap_s, limit, ApStruct, limit));

  /* Check ss_s/ScanStateStruct compatibility by hand */
  /* .check.ss: See also impl.h.trace.ss. */
  AVER(CHECKFIELDAPPROX(mps_ss_s, fix, ScanStateStruct, fix));
  AVER(CHECKFIELD(mps_ss_s, w0, ScanStateStruct, zoneShift));
  AVER(CHECKFIELD(mps_ss_s, w1, ScanStateStruct, condemned));
  AVER(CHECKFIELD(mps_ss_s, w2, ScanStateStruct, summary));

  /* Check ld_s/LDStruct compatibility by hand */
  /* .check.ld: See also impl.h.ld.struct */
  AVER(sizeof(mps_ld_s) == sizeof(LDStruct));
  AVER(CHECKFIELD(mps_ld_s, w0, LDStruct, epoch));
  AVER(CHECKFIELD(mps_ld_s, w1, LDStruct, rs));

  return TRUE;
}

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

mps_res_t mps_fmt_create_A(mps_fmt_t *mps_fmt_o,
                           mps_space_t mps_space,
                           mps_fmt_A_t mps_fmt_A)
{
  Format *formatReturn = (Format *)mps_fmt_o;
  Space space = (Space)mps_space;
  Error e;
  SpaceLockClaim(space);
  AVER(mps_fmt_A != NULL);
  e = FormatCreate(formatReturn,
                   (Space)mps_space,
                   (Addr)mps_fmt_A->align,
                   (FormatScanMethod)mps_fmt_A->scan,
                   (FormatSkipMethod)mps_fmt_A->skip,
                   (FormatMoveMethod)mps_fmt_A->fwd,
                   (FormatIsMovedMethod)mps_fmt_A->isfwd,
                   (FormatCopyMethod)mps_fmt_A->copy,
                   (FormatPadMethod)mps_fmt_A->pad);
  SpaceLockRelease(space);
  return e;
}

void mps_fmt_destroy(mps_fmt_t mps_fmt)
{
  Format format = (Format)mps_fmt;
  Space space = FormatSpace(format);
  SpaceLockClaim(space);
  AVER(ISVALID(Format, format));
  FormatDestroy((Format)mps_fmt);
  SpaceLockRelease(space);
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

  SpaceLockClaim(space);

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(ISVALID(PoolClass, class));

  va_start(args, mps_class);
  e = PoolCreateV(poolReturn, class, space, args);
  va_end(args);

  SpaceLockRelease(space);
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

  SpaceLockClaim(space);

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(ISVALID(PoolClass, class));

  e = PoolCreateV(poolReturn, class, space, args);

  SpaceLockRelease(space);
  return e;
}

void mps_pool_destroy(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);

  SpaceLockClaim(space);

  AVER(ISVALID(Pool, pool));
  PoolDestroy(pool);

  SpaceLockRelease(space);
}

mps_res_t mps_alloc(mps_addr_t *p_o,
                    mps_pool_t mps_pool,
                    size_t size, ...)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);
  Error e;

  SpaceLockClaim(space);

  /* Give the space the opportunity to steal CPU time. */
  SpacePoll(space);
  
  AVER(p_o != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(size > 0);
  /* Note: class may allow unaligned size. */

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = PoolAlloc((Addr *)p_o, pool, size);

  SpaceLockRelease(space);
  return e;
}

mps_res_t mps_alloc_v(mps_addr_t *p_o,
                      mps_pool_t mps_pool,
                      size_t size, va_list args)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);
  Error e;

  SpaceLockClaim(space);

  /* Give the space the opportunity to steal CPU time. */
  SpacePoll(space);
  
  AVER(p_o != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(size > 0);
  UNUSED(args);
  /* Note: class may allow unaligned size. */

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = PoolAlloc((Addr *)p_o, pool, size);

  SpaceLockRelease(space);
  return e;
}

void mps_free(mps_pool_t mps_pool, mps_addr_t p, size_t size)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);

  SpaceLockClaim(space);

  AVER(ISVALID(Pool, pool));
  AVER(size > 0);
  /* Note: class may allow unaligned size. */

  PoolFree(pool, (Addr)p, size);
  SpaceLockRelease(space);
}

mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t mps_pool, ...)
{
  Ap *apReturn = (Ap *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);
  Buffer buf;
  Error e;

  SpaceLockClaim(space);

  AVER(apReturn != NULL);
  AVER(ISVALID(Pool, pool));

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = BufferCreate(&buf, pool);
  if(e != ErrSUCCESS)
    return e;

  *apReturn = BufferAp(buf);
  SpaceLockRelease(space);
  return MPS_RES_OK;
}

mps_res_t mps_ap_create_v(mps_ap_t *mps_ap_o,
                          mps_pool_t mps_pool,
                          va_list args)
{
  Ap *apReturn = (Ap *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);
  Buffer buf;
  Error e;

  SpaceLockClaim(space);

  AVER(apReturn != NULL);
  AVER(ISVALID(Pool, pool));
  UNUSED(args);

  /* Varargs are ignored at the moment -- none of the pool */
  /* implementations use them, and they're not passed through. */
  e = BufferCreate(&buf, pool);
  if(e != ErrSUCCESS)
    return e;

  *apReturn = BufferAp(buf);
  SpaceLockRelease(space);
  return MPS_RES_OK;
}

void mps_ap_destroy(mps_ap_t mps_ap)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Space space = BufferSpace(buf);

  SpaceLockClaim(space);

  AVER(ISVALID(Buffer, buf));
  BufferDestroy(buf);
  SpaceLockRelease(space);
}

mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  mps_res_t res;

  /* mps_reserve does not call BufferReserve, but instead uses the */
  /* in-line macro from impl.h.mps.  This is so that it calls mps_ap_fill */
  /* and thence SpacePoll.  The consistency checks here are the ones */
  /* which can be done outside the interface. */

  AVER(p_o != NULL);
  AVER(size > 0);
  AVER(mps_ap != NULL);
  AVER(mps_ap->init == mps_ap->alloc);

  MPS_RESERVE_BLOCK(res, *p_o, mps_ap, size);

  return res;
}

mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  /* mps_commit does not call BufferCommit, but instead uses the */
  /* in-line commit macro from impl.h.mps.  This is so that it calls */
  /* mps_ap_trip.  The consistency checks here are the ones which can be */
  /* done outside the interface. */

  AVER(mps_ap != NULL);
  AVER(p != NULL);
  AVER(size > 0);
  AVER(p == mps_ap->init);
  AVER((void *)((char *)mps_ap->init + size) == mps_ap->alloc);

  return mps_commit(mps_ap, p, size);
}

mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Space space = BufferSpace(buf);
  Error e;

  SpaceLockClaim(space);

  /* Give the space the opportunity to steal CPU time. */
  SpacePoll(space);

  AVER(p_o != NULL);
  AVER(ISVALID(Buffer, buf));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buf)->alignment, size));

  e = BufferFill((Addr *)p_o, buf, size);

  SpaceLockRelease(space);
  return e;
}

mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAp((Ap)mps_ap);
  Space space = BufferSpace(buf);
  Bool b;

  SpaceLockClaim(space);

  AVER(ISVALID(Buffer, buf));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buf)->alignment, size));

  b = BufferTrip(buf, (Addr)p, size);
  SpaceLockRelease(space);
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
  Error e;

  SpaceLockClaim(space);

  AVER(rootReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(mps_root_scan != NULL);
  AVER(mps_rm == (mps_rm_t)0);

  /* The root mode is ignored. */
  e = RootCreate(rootReturn, space, rank,
                 (RootScanMethod)mps_root_scan, p, s);
  SpaceLockRelease(space);
  return e;
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
  Error e;

  SpaceLockClaim(space);

  AVER(mps_root_o != NULL);
  AVER(ISVALID(Space, space));
  AVER(base != NULL);
  AVER((unsigned long)size > 0);
  /* Note, size is the length of the array at base, not */
  /* the size in bytes.  However, RootCreateTable expects */
  /* base and limit pointers.  Be careful. */
  /* The root mode is ignored. */
  e = RootCreateTable(rootReturn, space, rank,
                      (Addr *)base, (Addr *)base + size);
  SpaceLockRelease(space);
  return e;
}

mps_res_t mps_root_create_fmt(mps_root_t *mps_root_o,
                              mps_space_t mps_space,
                              mps_rank_t mps_rank,
                              mps_rm_t mps_rm,
                              mps_fmt_scan_t mps_fmt_scan,
                              mps_addr_t base,
                              mps_addr_t limit)
{
  Root *rootReturn = (Root *)mps_root_o;
  Space space = (Space)mps_space;
  RefRank rank = (RefRank)mps_rank;
  FormatScanMethod scan = (FormatScanMethod)mps_fmt_scan;

  AVER(mps_root_o != NULL);
  AVER(ISVALID(Space, space));
  AVER(scan != NULL);
  AVER(base != NULL);
  AVER(base < limit);
  /* The root mode is ignored. */
  return RootCreateFmt(rootReturn, space, rank, scan,
                       (Addr)base, (Addr)limit);
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
  Error e;

  SpaceLockClaim(space);

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

  e = RootCreateReg(rootReturn, space, rank, thread,
                    (RootScanRegMethod)mps_reg_scan,
                    reg_scan_p);
  SpaceLockRelease(space);
  return e;
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
  Space space = RootSpace(root);
  SpaceLockClaim(space);
  AVER(ISVALID(Root, root));
  RootDestroy(root);
  SpaceLockRelease(space);
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
  Error e;

  SpaceLockClaim(space);

  AVER(mps_thr_o != NULL);
  AVER(ISVALID(Space, space));

  e = ThreadRegister(threadReturn, space);
  SpaceLockRelease(space);
  return e;
}

void mps_thread_dereg(mps_thr_t mps_thr)
{
  Thread thread = (Thread)mps_thr;
  Space space = ThreadSpace(thread);
  SpaceLockClaim(space);
  ThreadDeregister(thread, space);
  SpaceLockRelease(space);
}

void mps_ld_reset(mps_ld_t mps_ld, mps_space_t mps_space)
{
  Space space = (Space)mps_space;
  LD ld = (LD)mps_ld;
  SpaceLockClaim(space);
  LDReset(ld, space);
  SpaceLockRelease(space);
}

/* @@@@ We should be able to avoid locking the space for ld code */
void mps_ld_add(mps_ld_t mps_ld, mps_space_t mps_space, mps_addr_t addr)
{
  Space space = (Space)mps_space;
  LD ld = (LD)mps_ld;
  SpaceLockClaim(space);
  LDAdd(ld, space, (Addr)addr);
  SpaceLockRelease(space);
}

mps_bool_t mps_ld_isstale(mps_ld_t mps_ld,
                          mps_space_t mps_space,
                          mps_addr_t addr)
{
  Space space = (Space)mps_space;
  LD ld = (LD)mps_ld;
  Bool b;
  SpaceLockClaim(space);
  b = LDIsStale(ld, space, (Addr)addr);
  SpaceLockRelease(space);
  return (mps_bool_t)b;
}

mps_res_t mps_fix(mps_ss_t mps_ss, mps_addr_t *ref_io)
{
  mps_res_t res;

  MPS_SCAN_BEGIN(mps_ss) {
    res = MPS_FIX(mps_ss, ref_io);
  } MPS_SCAN_END(mps_ss);

  return res;
}

mps_word_t mps_collections(mps_space_t mps_space)
{
  Space space = (Space)mps_space;
  return SpaceEpoch(space);  /* thread safe: see impl.h.space.epoch.ts */
}

/* @@@@ not done from here on */
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
