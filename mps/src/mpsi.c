/* impl.c.mpsi: MEMORY POOL SYSTEM INTERFACE LAYER
 *
 * $HopeName: MMsrc!mpsi.c(trunk.15) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * PURPOSE
 *
 * .purpose: This code bridges between the MPS interface to C,
 * impl.h.mpsi, and the internal MPM interfaces, as defined by
 * impl.h.mpm.  .purpose.check: It performs checking of the C client's
 * usage of the MPS Interface.  .purpose.thread: It excludes multiple
 * threads from the MPM by locking the Space (see .thread-safety).
 *
 * NOTES
 *
 * .thread-safety: Most calls through this interface lock the space
 * and therefore make the MPM single-threaded.  In order to do this
 * they must recover the space from their parameters.  Methods such
 * as ThreadSpace() must therefore be callable when the space is
 * _not_ locked.  These methods are tagged with the tag of this note.
 *
 * .lock-free: Certain functions inside the MPM are thread-safe and do
 * not need to be serialized by using locks.  They are tagged with this
 * the tag of this note.
 *
 * .form: Almost all functions in this implementation simply cast their
 * arguments to the equivalent internal types, and cast results back
 * to the external type, where necessary.  Only exceptions are noted
 * in comments.
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .check.protocol: (rule.impl.req) More could be done in this code to
 * check that protocols are obeyed by the client.  It probably doesn't
 * meet checking requirements.
 *
 * .varargs: (rule.universal.complete) The varags passed to mps_alloc(_v)
 * and mps_ap_create(_v) are ignored at the moment.  None of the pool
 * implementations use them, and they're not passed through.
 *
 * .poll: (rule.universal.complete) Various allocation methods call
 * SpacePoll to allow the MPM to "steal" CPU time and get on with
 * background tasks such as incremental GC.  This mechanism hasn't been
 * well thought out.
 *
 * .root-mode: (rule.universal.complete) The root "mode", which specifies
 * things like the protectability of roots, is ignored at present.  This
 * is because the MPM doesn't ever try to protect them.  In future, it
 * will.
 *
 * .reg-scan: (rule.universal.complete) At present, we only support
 * register scanning using our own ambiguous register and stack scanning
 * method, mps_stack_scan_ambig.  This may never change, but the way the
 * interface is designed allows for the possibility of change.
 *
 * .msg.unimpl: (rule.universal.complete) Aynchronous messages from the
 * MPS to the client are not implemented.
 */

#include "mpm.h"
#include "mps.h"

SRCID(mpsi, "$HopeName: MMsrc!mpsi.c(trunk.15) $");


/* mpsi_check -- check consistency of interface mappings
 *
 * .check.purpose: The mpsi_check function attempts to check whether
 * the defintions in impl.h.mpsi match the equivalent definition in
 * the MPM.  It is checking the assumptions made in the other functions
 * in this implementation.
 *
 * .check.macros: The CHECK* macros use some C trickery to attempt to
 * verify that certain types and fields are equivalent.  The do not
 * do a complete job.  This trickery is justified by the security gained
 * in knowing that impl.h.mps matches the MPM.  See also
 * mail.richard.1996-08-07.09-49.  [This paragraph is intended to
 * satisfy rule.impl.trick.]
 *
 * .check.empty: Note that mpsi_check compiles away to almost nothing.
 */

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
  CHECKL(CHECKTYPE(mps_res_t, Res));
  CHECKL(MPS_RES_OK == ResOK);
  CHECKL(MPS_RES_FAIL == ResFAIL);
  CHECKL(MPS_RES_RESOURCE == ResRESOURCE);
  CHECKL(MPS_RES_MEMORY == ResMEMORY);
  CHECKL(MPS_RES_LIMIT == ResLIMIT);
  CHECKL(MPS_RES_UNIMPL == ResUNIMPL);
  CHECKL(MPS_RES_IO == ResIO);

  /* Check that external and internal rank numbers match. */
  CHECKL(CHECKTYPE(mps_rank_t, Rank));
  CHECKL(MPS_RANK_AMBIG == RankAMBIG);
  CHECKL(MPS_RANK_EXACT == RankEXACT);
  CHECKL(MPS_RANK_WEAK == RankWEAK);
  CHECKL(MPS_RANK_FINAL == RankFINAL);

  /* The external idea of a word width and the internal one */
  /* had better match. */
  CHECKL(MPS_WORD_WIDTH == WORD_WIDTH);
  CHECKL(sizeof(mps_word_t) == sizeof(void *));
  CHECKL(CHECKTYPE(mps_word_t, Word));

  /* Check ap_s/APStruct compatibility by hand */
  /* .check.ap: See also impl.h.buffer.ap. */
  CHECKL(sizeof(mps_ap_s) == sizeof(APStruct));
  CHECKL(CHECKFIELD(mps_ap_s, init,  APStruct, init));
  CHECKL(CHECKFIELD(mps_ap_s, alloc, APStruct, alloc));
  CHECKL(CHECKFIELD(mps_ap_s, limit, APStruct, limit));

  /* Check ss_s/ScanStateStruct compatibility by hand */
  /* .check.ss: See also impl.h.trace.ss. */
  CHECKL(CHECKFIELDAPPROX(mps_ss_s, fix, ScanStateStruct, fix));
  CHECKL(CHECKFIELD(mps_ss_s, w0, ScanStateStruct, zoneShift));
  CHECKL(CHECKFIELD(mps_ss_s, w1, ScanStateStruct, condemned));
  CHECKL(CHECKFIELD(mps_ss_s, w2, ScanStateStruct, summary));

  /* Check ld_s/LDStruct compatibility by hand */
  /* .check.ld: See also impl.h.ld.struct */
  CHECKL(sizeof(mps_ld_s) == sizeof(LDStruct));
  CHECKL(CHECKFIELD(mps_ld_s, w0, LDStruct, epoch));
  CHECKL(CHECKFIELD(mps_ld_s, w1, LDStruct, rs));

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


mps_res_t mps_space_create_wmem(mps_space_t *mps_space_o,
                                mps_addr_t base, size_t size)
{
  Space *spaceReturn = (Space *)mps_space_o;

  /* This is the first real call that the client will have to make, */
  /* so check static consistency here. */
  AVER(mpsi_check());

  AVER(spaceReturn != NULL);
  AVER(base != NULL);

  return SpaceCreate(spaceReturn, (Addr)base, (Size)size);
}

mps_res_t mps_space_create(mps_space_t *mps_space_o)
{
  Space *spaceReturn = (Space *)mps_space_o;

  /* This is the first real call that the client will have to make, */
  /* so check static consistency here. */
  AVER(mpsi_check());

  AVER(spaceReturn != NULL);

  return SpaceCreate(spaceReturn, (Addr)0, (Size)0);
}

void mps_space_destroy(mps_space_t mps_space)
{
  Space space = (Space)mps_space;
  AVERT(Space, space);
  SpaceDestroy(space);
}


/* mps_fmt_create_A -- create an object format of variety A
 *
 * .fmt.create.A.purpose: This function converts an object format
 * spec of variant "A" into an MPM Format object.  See
 * design.mps.interface.c.fmt.extend for justification of the
 * way that the format structure is declared as "mps_fmt_A".
 */ 

mps_res_t mps_fmt_create_A(mps_fmt_t *mps_fmt_o,
                           mps_space_t mps_space,
                           mps_fmt_A_t mps_fmt_A)
{
  Format *formatReturn = (Format *)mps_fmt_o;
  Space space = (Space)mps_space;
  Res res;
  SpaceEnter(space);
  AVER(mps_fmt_A != NULL);
  res = FormatCreate(formatReturn,
                     (Space)mps_space,
                     (Align)mps_fmt_A->align,
                     (FormatScanMethod)mps_fmt_A->scan,
                     (FormatSkipMethod)mps_fmt_A->skip,
                     (FormatMoveMethod)mps_fmt_A->fwd,
                     (FormatIsMovedMethod)mps_fmt_A->isfwd,
                     (FormatCopyMethod)mps_fmt_A->copy,
                     (FormatPadMethod)mps_fmt_A->pad);
  SpaceLeave(space);
  return res;
}

void mps_fmt_destroy(mps_fmt_t mps_fmt)
{
  Format format = (Format)mps_fmt;
  Space space = FormatSpace(format);
  SpaceEnter(space);
  AVERT(Format, format);
  FormatDestroy((Format)mps_fmt);
  SpaceLeave(space);
}


mps_res_t mps_pool_create(mps_pool_t *mps_pool_o,
                          mps_space_t mps_space,
                          mps_class_t mps_class, ...)
{
  Pool *poolReturn = (Pool *)mps_pool_o;
  Space space = (Space)mps_space;
  PoolClass class = (PoolClass)mps_class;
  va_list args;
  Res res;

  SpaceEnter(space);

  AVER(poolReturn != NULL);
  AVERT(Space, space);
  AVERT(PoolClass, class);

  va_start(args, mps_class);
  res = PoolCreateV(poolReturn, class, space, args);
  va_end(args);

  SpaceLeave(space);
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
  Res res;

  SpaceEnter(space);

  AVER(poolReturn != NULL);
  AVERT(Space, space);
  AVERT(PoolClass, class);

  res = PoolCreateV(poolReturn, class, space, args);

  SpaceLeave(space);
  return res;
}

void mps_pool_destroy(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);

  SpaceEnter(space);

  AVERT(Pool, pool);
  PoolDestroy(pool);

  SpaceLeave(space);
}


mps_res_t mps_alloc(mps_addr_t *p_o,
                    mps_pool_t mps_pool,
                    size_t size, ...)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);
  Res res;

  SpaceEnter(space);

  SpacePoll(space);                     /* .poll */

  AVER(p_o != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  /* Note: class may allow unaligned size, see */
  /* design.mps.class-interface.alloc.size.align. */

  /* See .varargs. */
  res = PoolAlloc((Addr *)p_o, pool, size);

  SpaceLeave(space);
  return res;
}

mps_res_t mps_alloc_v(mps_addr_t *p_o,
                      mps_pool_t mps_pool,
                      size_t size, va_list args)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);
  Res res;

  SpaceEnter(space);

  SpacePoll(space);                     /* .poll */

  AVER(p_o != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  UNUSED(args);
  /* Note: class may allow unaligned size, see */
  /* design.mps.class-interface.alloc.size.align. */

  /* See .varargs. */
  res = PoolAlloc((Addr *)p_o, pool, size);

  SpaceLeave(space);
  return res;
}

void mps_free(mps_pool_t mps_pool, mps_addr_t p, size_t size)
{
  Pool pool = (Pool)mps_pool;
  Space space = PoolSpace(pool);

  SpaceEnter(space);

  AVERT(Pool, pool);
  AVER(size > 0);
  /* Note: class may allow unaligned size, see */
  /* design.mps.class-interface.alloc.size.align. */

  PoolFree(pool, (Addr)p, size);
  SpaceLeave(space);
}


mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t mps_pool,
                        mps_rank_t mps_rank, ...)
{
  AP *apReturn = (AP *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Rank rank = (Rank)mps_rank;
  Space space = PoolSpace(pool);
  Buffer buf;
  Res res;
  va_list args;

  SpaceEnter(space);

  AVER(apReturn != NULL);
  AVERT(Pool, pool);

  /* See .varargs. */
  va_start(args, mps_rank);
  res = BufferCreate(&buf, pool, rank);
  va_end(args);
  if(res != ResOK)
    return res;

  *apReturn = BufferAP(buf);
  SpaceLeave(space);
  return MPS_RES_OK;
}

mps_res_t mps_ap_create_v(mps_ap_t *mps_ap_o, mps_pool_t mps_pool,
                          mps_rank_t mps_rank, va_list args)
{
  AP *apReturn = (AP *)mps_ap_o;
  Pool pool = (Pool)mps_pool;
  Rank rank = (Rank)mps_rank;
  Space space = PoolSpace(pool);
  Buffer buf;
  Res res;

  SpaceEnter(space);

  AVER(apReturn != NULL);
  AVERT(Pool, pool);
  UNUSED(args);

  /* See .varargs. */
  res = BufferCreate(&buf, pool, rank);
  if(res != ResOK)
    return res;

  *apReturn = BufferAP(buf);
  SpaceLeave(space);
  return MPS_RES_OK;
}

void mps_ap_destroy(mps_ap_t mps_ap)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Space space = BufferSpace(buf);

  SpaceEnter(space);

  AVERT(Buffer, buf);
  BufferDestroy(buf);
  SpaceLeave(space);
}


/* mps_reserve -- allocate store in preparation for initialization
 *
 * .reserve.call: mps_reserve does not call BufferReserve, but instead
 * uses the in-line macro from impl.h.mps.  This is so that it calls
 * mps_ap_fill and thence SpacePoll (.poll).  The consistency checks here
 * are the ones which can be done outside the MPM.  See also .commit.call.
 */

mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  mps_res_t res;

  AVER(p_o != NULL);
  AVER(size > 0);
  AVER(mps_ap != NULL);
  AVER(mps_ap->init == mps_ap->alloc);

  MPS_RESERVE_BLOCK(res, *p_o, mps_ap, size);

  return res;
}


/* mps_commit -- commit initialized object, finishing allocation
 *
 * .commit.call: mps_commit does not call BufferCommit, but instead
 * uses the in-line commit macro from impl.h.mps.  This is so that it
 * calls mps_ap_trip and thence SpacePoll in future (.poll).  The
 * consistency checks here are the ones which can be done outside the
 * MPM.  See also .reserve.call.
 */

mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  AVER(mps_ap != NULL);
  AVER(p != NULL);
  AVER(size > 0);
  AVER(p == mps_ap->init);
  AVER((void *)((char *)mps_ap->init + size) == mps_ap->alloc);

  return mps_commit(mps_ap, p, size);
}


/* mps_ap_fill -- called by mps_reserve when an AP hasn't enough space
 *
 * .ap.fill.internal: Note that mps_ap_fill should never be "called"
 * directly by the client code.  It is invoked by the mps_reserve macro.
 */

mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Space space = BufferSpace(buf);
  Res res;

  SpaceEnter(space);

  SpacePoll(space);                     /* .poll */

  AVER(p_o != NULL);
  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  res = BufferFill((Addr *)p_o, buf, size);

  SpaceLeave(space);
  return res;
}


/* mps_trip -- called by mps_commit when an AP is tripped
 *
 * .ap.trip.internal: Note that mps_ap_trip should never be "called"
 * directly by the client code.  It is invoked by the mps_commit macro.
 */

mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Space space = BufferSpace(buf);
  Bool b;

  SpaceEnter(space);

  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  b = BufferTrip(buf, (Addr)p, size);
  SpaceLeave(space);
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
  Rank rank = (Rank)mps_rank;
  Res res;

  SpaceEnter(space);

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(mps_root_scan != NULL);
  AVER(mps_rm == (mps_rm_t)0);

  /* See .root-mode. */
  res = RootCreate(rootReturn, space, rank,
                 (RootScanMethod)mps_root_scan, p, s);
  SpaceLeave(space);
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
  Rank rank = (Rank)mps_rank;
  Res res;

  SpaceEnter(space);

  AVER(mps_root_o != NULL);
  AVERT(Space, space);
  AVER(base != NULL);
  AVER((unsigned long)size > 0);

  /* Note, size is the length of the array at base, not */
  /* the size in bytes.  However, RootCreateTable expects */
  /* base and limit pointers.  Be careful. */

  /* See .root-mode. */
  res = RootCreateTable(rootReturn, space, rank,
                        (Addr *)base, (Addr *)base + size);
  SpaceLeave(space);
  return res;
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
  Rank rank = (Rank)mps_rank;
  FormatScanMethod scan = (FormatScanMethod)mps_fmt_scan;

  AVER(mps_root_o != NULL);
  AVERT(Space, space);
  AVER(scan != NULL);
  AVER(base != NULL);
  AVER(base < limit);

  /* See .root-mode. */
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
  Rank rank = (Rank)mps_rank;
  Thread thread = (Thread)mps_thr;
  Res res;

  SpaceEnter(space);

  AVER(mps_root_o != NULL);
  AVERT(Space, space);
  AVERT(Thread, thread);
  AVER(mps_reg_scan != NULL);
  AVER(mps_reg_scan == mps_stack_scan_ambig); /* .reg.scan */
  AVER(reg_scan_p != NULL); /* stackBot */
  AVER(rank == MPS_RANK_AMBIG);
  AVER(mps_rm == (mps_rm_t)0);

  /* See .root-mode. */
  res = RootCreateReg(rootReturn, space, rank, thread,
                    (RootScanRegMethod)mps_reg_scan,
                    reg_scan_p);
  SpaceLeave(space);
  return res;
}


/* mps_stack_scan_ambig -- scan the thread state ambiguously
 *
 * See .reg-scan.
 */

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
  SpaceEnter(space);
  AVERT(Root, root);
  RootDestroy(root);
  SpaceLeave(space);
}


void (mps_tramp)(void **r_o,
                 void *(*f)(void *p, size_t s),
                 void *p,
                 size_t s)
{
  /* No parameter validation is possible.  We place no constraints on */
  /* the value of these parameters. */

  ProtTramp(r_o, f, p, s);
}


mps_res_t mps_thread_reg(mps_thr_t *mps_thr_o,
                         mps_space_t mps_space)
{
  Thread *threadReturn = (Thread *)mps_thr_o;
  Space space = (Space)mps_space;
  Res res;

  SpaceEnter(space);

  AVER(mps_thr_o != NULL);
  AVERT(Space, space);

  res = ThreadRegister(threadReturn, space);
  SpaceLeave(space);
  return res;
}

void mps_thread_dereg(mps_thr_t mps_thr)
{
  Thread thread = (Thread)mps_thr;
  Space space = ThreadSpace(thread);
  SpaceEnter(space);
  ThreadDeregister(thread, space);
  SpaceLeave(space);
}

void mps_ld_reset(mps_ld_t mps_ld, mps_space_t mps_space)
{
  Space space = (Space)mps_space;
  LD ld = (LD)mps_ld;
  SpaceEnter(space);
  LDReset(ld, space);
  SpaceLeave(space);
}


/* mps_ld_add -- add a reference to a location dependency
 *
 * See .lock-free.
 */

void mps_ld_add(mps_ld_t mps_ld, mps_space_t mps_space, mps_addr_t addr)
{
  Space space = (Space)mps_space;
  LD ld = (LD)mps_ld;
  LDAdd(ld, space, (Addr)addr);
}


/* mps_ld_isstale -- check whether a location dependency is "stale"
 *
 * See .lock-free.
 */

mps_bool_t mps_ld_isstale(mps_ld_t mps_ld,
                          mps_space_t mps_space,
                          mps_addr_t addr)
{
  Space space = (Space)mps_space;
  LD ld = (LD)mps_ld;
  Bool b;
  b = LDIsStale(ld, space, (Addr)addr);
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


/* Unimplemented -- see .msg.unimpl */

mps_bool_t mps_msg_next(mps_space_t mps_space,
                        mps_mc_t *mps_mc_o,
                        void **msg_data_o)
{
  NOTREACHED;
  return FALSE;
}


/* Unimplemented -- see .msg.unimpl */

mps_bool_t mps_msg_peek(mps_space_t mps_space,
                    mps_mc_t *mps_mc_o,
                    void **msg_data_o)
{
  NOTREACHED;
  return FALSE;
}


/* Unimplemented -- see .msg.unimpl */

mps_msg_handler_t mps_msg_handler(mps_space_t mps_space,
                                  mps_msg_handler_t mps_handler)
{
  NOTREACHED;
  return NULL;
}
