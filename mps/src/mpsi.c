/* impl.c.mpsi: MEMORY POOL SYSTEM C INTERFACE LAYER
 *
 * $HopeName: MMsrc!mpsi.c(trunk.60) $
 * Copyright (C) 1997. Harlequin Group plc. All rights reserved.
 *
 * .purpose: This code bridges between the MPS interface to C,
 * impl.h.mps, and the internal MPM interfaces, as defined by
 * impl.h.mpm.  .purpose.check: It performs checking of the C client's
 * usage of the MPS Interface.  .purpose.thread: It excludes multiple
 * threads from the MPM by locking the Arena (see .thread-safety).
 *
 * .readership: MPS developers
 * .design: design.mps.interface.c
 *
 *
 * NOTES
 *
 * .note.break-out: Take care not to return when "inside" the Arena
 * (between ArenaEnter and ArenaLeave) as this will leave the Arena in
 * an unsuitable state for re-entry.
 *
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .check.protocol: (rule.impl.req) More could be done in this code to
 * check that protocols are obeyed by the client.  It probably doesn't
 * meet checking requirements.
 *
 * .varargs: (rule.universal.complete) The varargs passed to
 * mps_alloc(_v) are ignored at the moment.  None of the pool
 * implementations use them.
 *
 * .poll: (rule.universal.complete) Various allocation methods call
 * ArenaPoll to allow the MPM to "steal" CPU time and get on with
 * background tasks such as incremental GC.  This mechanism hasn't been
 * well thought out.
 *
 * .root-mode: (rule.universal.complete) The root "mode", which
 * specifies things like the protectability of roots, is ignored at
 * present.  This is because the MPM doesn't ever try to protect them.
 * In future, it will.
 *
 * .reg-scan: (rule.universal.complete) At present, we only support
 * register scanning using our own ambiguous register and stack scanning
 * method, mps_stack_scan_ambig.  This may never change, but the way the
 * interface is designed allows for the possibility of change.
 *
 * .naming: (rule.impl.guide) The exported identifiers do not follow the
 * normal MPS naming conventions.  See design.mps.interface.c.naming.  */

#include "mpm.h"
#include "mps.h"
#include "mpsavm.h" /* only for mps_space_create */

SRCID(mpsi, "$HopeName: MMsrc!mpsi.c(trunk.60) $");


/* mpsi_check -- check consistency of interface mappings
 *
 * .check.purpose: The mpsi_check function attempts to check whether
 * the defintions in impl.h.mpsi match the equivalent definition in
 * the MPM.  It is checking the assumptions made in the other functions
 * in this implementation.
 *
 * .check.empty: Note that mpsi_check compiles away to almost nothing.
 * 
 * .check.enum.cast: enum comparisons have to be cast to avoid a warning
 * from the SunPro C compiler. See builder.sc.warn.enum.
 */

static Bool mpsi_check(void)
{
  /* .check.rc: Check that external and internal result codes match. */
  /* See impl.h.mps.result-codes and impl.h.mpmtypes.result-codes. */
  /* Also see .check.enum.cast. */
  CHECKL(CHECKTYPE(mps_res_t, Res));
  CHECKL((int)MPS_RES_OK == (int)ResOK);
  CHECKL((int)MPS_RES_FAIL == (int)ResFAIL);
  CHECKL((int)MPS_RES_RESOURCE == (int)ResRESOURCE);
  CHECKL((int)MPS_RES_MEMORY == (int)ResMEMORY);
  CHECKL((int)MPS_RES_LIMIT == (int)ResLIMIT);
  CHECKL((int)MPS_RES_UNIMPL == (int)ResUNIMPL);
  CHECKL((int)MPS_RES_IO == (int)ResIO);
  CHECKL((int)MPS_RES_COMMIT_LIMIT == (int)ResCOMMIT_LIMIT);

  /* Check that external and internal rank numbers match. */
  /* See impl.h.mps.ranks and impl.h.mpmtypes.ranks. */
  /* Also see .check.enum.cast. */
  CHECKL(CHECKTYPE(mps_rank_t, Rank));
  CHECKL((int)MPS_RANK_AMBIG == (int)RankAMBIG);
  CHECKL((int)MPS_RANK_EXACT == (int)RankEXACT);
  CHECKL((int)MPS_RANK_WEAK == (int)RankWEAK);

  /* The external idea of a word width and the internal one */
  /* had better match.  See design.mps.interface.c.cons. */
  CHECKL(sizeof(mps_word_t) == sizeof(void *));
  CHECKL(CHECKTYPE(mps_word_t, Word));
  
  /* The external idea of size and the internal one had */
  /* better match.  See design.mps.interface.c.cons.size */
  /* and design.mps.interface.c.pun.size. */
  CHECKL(CHECKTYPE(size_t, Size));

  /* Check ap_s/APStruct compatibility by hand */
  /* .check.ap: See impl.h.mps.ap and impl.h.buffer.ap. */
  CHECKL(sizeof(mps_ap_s) == sizeof(APStruct));
  CHECKL(CHECKFIELD(mps_ap_s, init,  APStruct, init));
  CHECKL(CHECKFIELD(mps_ap_s, alloc, APStruct, alloc));
  CHECKL(CHECKFIELD(mps_ap_s, limit, APStruct, limit));

  /* Check ss_s/ScanStateStruct compatibility by hand */
  /* .check.ss: See impl.h.mps.ss and impl.h.mpmst.ss. */
  /* Note that the size of the mps_ss_s and ScanStateStruct */
  /* are not equal.  See impl.h.mpmst.ss.  CHECKFIELDAPPROX */
  /* is used on the fix field because its type is punned and */
  /* therefore isn't exactly checkable.  See */
  /* design.mps.interface.c.pun.addr. */
  CHECKL(CHECKFIELDAPPROX(mps_ss_s, fix, ScanStateStruct, fix));
  CHECKL(CHECKFIELD(mps_ss_s, w0, ScanStateStruct, zoneShift));
  CHECKL(CHECKFIELD(mps_ss_s, w1, ScanStateStruct, white));
  CHECKL(CHECKFIELD(mps_ss_s, w2, ScanStateStruct, unfixedSummary));

  /* Check ld_s/LDStruct compatibility by hand */
  /* .check.ld: See also impl.h.mpmst.ld.struct and impl.h.mps.ld */
  CHECKL(sizeof(mps_ld_s) == sizeof(LDStruct));
  CHECKL(CHECKFIELD(mps_ld_s, w0, LDStruct, epoch));
  CHECKL(CHECKFIELD(mps_ld_s, w1, LDStruct, rs));

  return TRUE;
}

/* Ranks
 * 
 * Here a rank returning function is defined for all client visible
 * ranks.
 * 
 * .rank.final.not: RankFINAL does not have a corresponding function as it
 * is only used internally.
 */

mps_rank_t mps_rank_ambig(void)
{
  return RankAMBIG;
}

mps_rank_t mps_rank_exact(void)
{
  return RankEXACT;
}

mps_rank_t mps_rank_weak(void)
{
  return RankWEAK;
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


mps_res_t mps_arena_extend(mps_arena_t mps_arena,
                           mps_addr_t base, size_t size)
{
  Arena arena = (Arena)mps_arena;
  Res res;

  ArenaEnter(arena);
  AVER(size > 0);
  res = ArenaExtend(arena, (Addr)base, (Size)size);
  ArenaLeave(arena);

  return (mps_res_t)res;
}

size_t mps_arena_reserved(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  Size size;

  ArenaEnter(arena);
  size = ArenaReserved(arena);
  ArenaLeave(arena);

  return (size_t)size;
}

/* for backward compatibility */
size_t mps_space_reserved(mps_space_t mps_space)
{
  return mps_arena_reserved(mps_space);
}

size_t mps_arena_committed(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  Size size;

  ArenaEnter(arena);
  size = ArenaCommitted(arena);
  ArenaLeave(arena);

  return (size_t)size;
}

/* for backward compatibility */
size_t mps_space_committed(mps_space_t mps_space)
{
  return mps_arena_committed(mps_space);
}

size_t mps_arena_commit_limit(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  Size size;

  ArenaEnter(arena);
  size = ArenaCommitLimit(arena);
  ArenaLeave(arena);

  return size;
}
  
mps_res_t mps_arena_commit_limit_set(mps_arena_t mps_arena, size_t limit)
{
  Res res;
  Arena arena = (Arena)mps_arena;

  ArenaEnter(arena);
  res = ArenaSetCommitLimit(arena, limit);
  ArenaLeave(arena);

  return res;
}

void mps_arena_clamp(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  ArenaEnter(arena);
  ArenaClamp(arena);
  ArenaLeave(arena);
}

/* for backward compatibility */
void mps_space_clamp(mps_space_t mps_space)
{
  mps_arena_clamp(mps_space);
}
 
   
void mps_arena_release(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  ArenaEnter(arena);
  ArenaRelease(arena);
  ArenaLeave(arena);
}

/* for backward compatibility */
void mps_space_release(mps_space_t mps_space)
{
  mps_arena_release(mps_space);
}
  

void mps_arena_park(mps_space_t mps_space)
{
  Arena arena = (Arena)mps_space;
  ArenaEnter(arena);
  ArenaPark(arena);
  ArenaLeave(arena);
}

/* for backward compatibility */
void mps_space_park(mps_space_t mps_space)
{
  mps_arena_park(mps_space);
}
 

mps_res_t mps_arena_collect(mps_space_t mps_space)
{
  Res res;
  Arena arena = (Arena)mps_space;
  ArenaEnter(arena);
  res = ArenaCollect(arena);
  ArenaLeave(arena);
  return res;
}

/* for backward compatibility */
mps_res_t mps_space_collect(mps_space_t mps_space)
{
  return mps_arena_collect(mps_space);
}
    

/* mps_arena_create -- create an arena object */

mps_res_t mps_arena_create(mps_arena_t *mps_arena_o,
                           mps_arena_class_t mps_arena_class,
                           ...)
{
  mps_res_t res;
  va_list args;

  va_start(args, mps_arena_class);
  res = mps_arena_create_v(mps_arena_o, mps_arena_class, args);
  va_end(args);
  return res;
}


/* mps_arena_create_v -- create an arena object */

mps_res_t mps_arena_create_v(mps_arena_t *mps_arena_o,
                             mps_arena_class_t mps_arena_class,
                             va_list args)
{
  Arena arena;
  Res res;

  /* This is the first real call that the client will have to make, */
  /* so check static consistency here. */
  AVER(mpsi_check());

  AVER(mps_arena_o != NULL);

  res = ArenaCreateV(&arena, (ArenaClass)mps_arena_class, args);

  if(res != ResOK)
    return res;
  ArenaLeave(arena);
  *mps_arena_o = (mps_arena_t)arena;
  return MPS_RES_OK;
}

#ifdef MPS_PROD_DYLAN
mps_res_t mps_space_create(mps_space_t *mps_space_o)
{
  return mps_arena_create(mps_space_o, mps_arena_class_vm(), ARENA_SIZE);
}
#endif


/* mps_arena_destroy -- destroy an arena object
 *
 * .space.destroy: This function has no locking of the arena,
 * and cannot have any, since it destroys the thing which contains
 * the lock.  In any case, any other thread which is using the
 * arena at the time it is destroyed, will fall over.
 */

void mps_arena_destroy(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  ArenaEnter(arena);
  AVERT(Arena, arena);
  ArenaDestroy(arena);
}

#ifdef MPS_PROD_DYLAN
void mps_space_destroy(mps_space_t mps_space)
{
  mps_arena_destroy(mps_space);
}
#endif


/* mps_fmt_create_A -- create an object format of variant A
 *
 * .fmt.create.A.purpose: This function converts an object format
 * spec of variant "A" into an MPM Format object.  See
 * design.mps.interface.c.fmt.extend for justification of the
 * way that the format structure is declared as "mps_fmt_A".
 */ 

mps_res_t mps_fmt_create_A(mps_fmt_t *mps_fmt_o,
                           mps_arena_t mps_arena,
                           mps_fmt_A_t mps_fmt_A)
{
  Arena arena = (Arena)mps_arena;
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_A != NULL);

  res = FormatCreate(&format,
                     arena,
                     (Align)mps_fmt_A->align,
                     FormatVarietyA,
                     (FormatScanMethod)mps_fmt_A->scan,
                     (FormatSkipMethod)mps_fmt_A->skip,
                     (FormatMoveMethod)mps_fmt_A->fwd,
                     (FormatIsMovedMethod)mps_fmt_A->isfwd,
                     (FormatCopyMethod)mps_fmt_A->copy,
                     (FormatPadMethod)mps_fmt_A->pad,
                     NULL);

  ArenaLeave(arena);

  if(res != ResOK) return res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}

/* mps_fmt_create_B -- create an object format of variant B
 *
 * .fmt.create.B.purpose: This function converts an object format
 * spec of variant "B" into an MPM Format object. 
 */ 

mps_res_t mps_fmt_create_B(mps_fmt_t *mps_fmt_o,
                           mps_arena_t mps_arena,
                           mps_fmt_B_t mps_fmt_B)
{
  Arena arena = (Arena)mps_arena;
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_B != NULL);

  res = FormatCreate(&format,
                     arena,
                     (Align)mps_fmt_B->align,
                     FormatVarietyB,
                     (FormatScanMethod)mps_fmt_B->scan,
                     (FormatSkipMethod)mps_fmt_B->skip,
                     (FormatMoveMethod)mps_fmt_B->fwd,
                     (FormatIsMovedMethod)mps_fmt_B->isfwd,
                     (FormatCopyMethod)mps_fmt_B->copy,
                     (FormatPadMethod)mps_fmt_B->pad,
                     (FormatClassMethod)mps_fmt_B->class);

  ArenaLeave(arena);

  if(res != ResOK) return res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}

void mps_fmt_destroy(mps_fmt_t mps_fmt)
{
  Format format = (Format)mps_fmt;
  Arena arena;
  
  AVER(CHECKT(Format, format));
  arena = FormatArena(format);

  ArenaEnter(arena);

  AVERT(Format, format);

  FormatDestroy(format);

  ArenaLeave(arena);
}


mps_res_t mps_pool_create(mps_pool_t *mps_pool_o,
                          mps_arena_t mps_arena,
                          mps_class_t mps_class,
                          ...)
{
  mps_res_t res;
  va_list args;
  va_start(args, mps_class);
  res = mps_pool_create_v(mps_pool_o, mps_arena, mps_class, args);
  va_end(args);
  return res;
}

mps_res_t mps_pool_create_v(mps_pool_t *mps_pool_o,
                            mps_arena_t mps_arena,
                            mps_class_t mps_class,
                            va_list args)
{
  Arena arena = (Arena)mps_arena;
  Pool pool;
  PoolClass class = (PoolClass)mps_class;
  Res res;

  ArenaEnter(arena);

  AVER(mps_pool_o != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, class);

  res = PoolCreateV(&pool, arena, class, args);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_pool_o = (mps_pool_t)pool;
  return res;
}

void mps_pool_destroy(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);
  PoolDestroy(pool);

  ArenaLeave(arena);
}

mps_res_t mps_alloc(mps_addr_t *p_o,
                    mps_pool_t mps_pool,
                    size_t size, ...)
{
  mps_res_t res;
  va_list args;
  va_start(args, size);
  res = mps_alloc_v(p_o, mps_pool, size, args);
  va_end(args);
  return res;
}

mps_res_t mps_alloc_v(mps_addr_t *p_o, mps_pool_t mps_pool, size_t size,
                      va_list args)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  Addr p;
  Res res;
  
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  ArenaPoll(arena);                     /* .poll */

  AVER(p_o != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  UNUSED(args);
  /* Note: class may allow unaligned size, see */
  /* design.mps.class-interface.alloc.size.align. */

  /* See .varargs. */
  /* @@@@ There is currently no requirement for reservoirs to work */
  /* with unbuffered allocation. */
  res = PoolAlloc(&p, pool, size, /* withReservoirPermit */ FALSE);

  ArenaLeave(arena);

  if(res != ResOK) return res;
  *p_o = (mps_addr_t)p;
  return MPS_RES_OK;
}

void mps_free(mps_pool_t mps_pool, mps_addr_t p, size_t size)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(PoolHasAddr(pool, p));
  AVER(size > 0);
  /* Note: class may allow unaligned size, see */
  /* design.mps.class-interface.alloc.size.align. */

  PoolFree(pool, (Addr)p, size);
  ArenaLeave(arena);
}


/* mps_ap_create -- create an allocation point */

mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t mps_pool, ...)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  Buffer buf;
  Res res;
  va_list args;
  
  AVER(mps_ap_o != NULL);
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);

  va_start(args, mps_pool);
  res = BufferCreateV(&buf, pool, TRUE, args);
  va_end(args);

  ArenaLeave(arena);

  if(res != ResOK)
    return res;
  *mps_ap_o = (mps_ap_t)BufferAP(buf);
  return MPS_RES_OK;
}


/* mps_ap_create_v -- create an allocation point, with varargs */

mps_res_t mps_ap_create_v(mps_ap_t *mps_ap_o, mps_pool_t mps_pool,
                          va_list args)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  Buffer buf;
  Res res;
  
  AVER(mps_ap_o != NULL);
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);

  res = BufferCreateV(&buf, pool, TRUE, args);

  ArenaLeave(arena);

  if(res != ResOK)
    return res;
  *mps_ap_o = (mps_ap_t)BufferAP(buf);
  return MPS_RES_OK;
}

void mps_ap_destroy(mps_ap_t mps_ap)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Arena arena;

  AVER(mps_ap != NULL);  
  AVER(CHECKT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  AVERT(Buffer, buf);
  BufferDestroy(buf);

  ArenaLeave(arena);
}


/* mps_reserve -- allocate store in preparation for initialization
 *
 * .reserve.call: mps_reserve does not call BufferReserve, but instead
 * uses the in-line macro from impl.h.mps.  This is so that it calls
 * mps_ap_fill and thence ArenaPoll (.poll).  The consistency checks
 * are those which can be done outside the MPM.  See also .commit.call.
 */

mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  mps_res_t res;

  AVER(p_o != NULL);
  AVER(size > 0);
  AVER(mps_ap != NULL);
  AVER(CHECKT(Buffer, BufferOfAP((AP)mps_ap)));
  AVER(mps_ap->init == mps_ap->alloc);

  MPS_RESERVE_BLOCK(res, *p_o, mps_ap, size);

  return res;
}



mps_res_t mps_reserve_with_reservoir_permit(mps_addr_t *p_o, 
                                            mps_ap_t mps_ap, size_t size)
{
  mps_res_t res;

  AVER(p_o != NULL);
  AVER(size > 0);
  AVER(mps_ap != NULL);
  AVER(CHECKT(Buffer, BufferOfAP((AP)mps_ap)));
  AVER(mps_ap->init == mps_ap->alloc);

  MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK(res, *p_o, mps_ap, size);

  return res;
}



/* mps_commit -- commit initialized object, finishing allocation
 *
 * .commit.call: mps_commit does not call BufferCommit, but instead
 * uses the in-line commit macro from impl.h.mps.  This is so that it
 * calls mps_ap_trip and thence ArenaPoll in future (.poll).  The
 * consistency checks here are the ones which can be done outside the
 * MPM.  See also .reserve.call.
 */

mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  AVER(mps_ap != NULL);
  AVER(CHECKT(Buffer, BufferOfAP((AP)mps_ap)));
  AVER(p != NULL);
  AVER(size > 0);
  AVER(p == mps_ap->init);
  AVER((void *)((char *)mps_ap->init + size) == mps_ap->alloc);

  return mps_commit(mps_ap, p, size);
}


/* mps_ap_fill -- called by mps_reserve when an AP hasn't enough arena
 *
 * .ap.fill.internal: Note that mps_ap_fill should never be "called"
 * directly by the client code.  It is invoked by the mps_reserve macro.
 */

mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Arena arena;
  Addr p;
  Res res;

  AVER(mps_ap != NULL);  
  AVER(CHECKT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  ArenaPoll(arena);                     /* .poll */

  AVER(p_o != NULL);
  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  res = BufferFill(&p, buf, size, /* withReservoirPermit */ FALSE);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *p_o = (mps_addr_t)p;
  return MPS_RES_OK;
}


mps_res_t mps_ap_fill_with_reservoir_permit(mps_addr_t *p_o, 
                                            mps_ap_t mps_ap, 
                                            size_t size)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Arena arena;
  Addr p;
  Res res;

  AVER(mps_ap != NULL);  
  AVER(CHECKT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  ArenaPoll(arena);                     /* .poll */

  AVER(p_o != NULL);
  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  res = BufferFill(&p, buf, size, /* withReservoirPermit */ TRUE);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *p_o = (mps_addr_t)p;
  return MPS_RES_OK;
}


/* mps_trip -- called by mps_commit when an AP is tripped
 *
 * .ap.trip.internal: Note that mps_ap_trip should never be "called"
 * directly by the client code.  It is invoked by the mps_commit macro.
 */

mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAP((AP)mps_ap);
  Arena arena;
  Bool b;

  AVER(mps_ap != NULL);  
  AVER(CHECKT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  b = BufferTrip(buf, (Addr)p, size);

  ArenaLeave(arena);

  return b;
}


mps_res_t mps_root_create(mps_root_t *mps_root_o,
                          mps_arena_t mps_arena,
                          mps_rank_t mps_rank,
                          mps_rm_t mps_rm,
                          mps_root_scan_t mps_root_scan,
                          void *p, size_t s)
{
  Arena arena = (Arena)mps_arena;
  Rank rank = (Rank)mps_rank;
  Root root;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(mps_rm == (mps_rm_t)0);

  /* See .root-mode. */
  res = RootCreateFun(&root, arena, rank,
                      (RootScanMethod)mps_root_scan, p, s);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_table(mps_root_t *mps_root_o,
                                mps_arena_t mps_arena,
                                mps_rank_t mps_rank,
                                mps_rm_t mps_rm,
                                mps_addr_t *base, size_t size)
{
  Arena arena = (Arena)mps_arena;
  Rank rank = (Rank)mps_rank;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(base != NULL);
  AVER(size > 0);

  /* .root.table-size: size is the length of the array at base, not */
  /* the size in bytes.  However, RootCreateTable expects base and */
  /* limit pointers.  Be careful. */

  res = RootCreateTable(&root, arena, rank, mode,
                        (Addr *)base, (Addr *)base + size);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_table_masked(mps_root_t *mps_root_o,
                                       mps_arena_t mps_arena,
                                       mps_rank_t mps_rank,
                                       mps_rm_t mps_rm,
                                       mps_addr_t *base, size_t size,
                                       mps_word_t mask)
{
  Arena arena = (Arena)mps_arena;
  Rank rank = (Rank)mps_rank;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(base != NULL);
  AVER(size > 0);
  /* Can't check anything about mask */

  /* See .root.table-size. */

  res = RootCreateTableMasked(&root, arena, rank, mode,
                              (Addr *)base, (Addr *)base + size,
                              mask);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_fmt(mps_root_t *mps_root_o,
                              mps_arena_t mps_arena,
                              mps_rank_t mps_rank,
                              mps_rm_t mps_rm,
                              mps_fmt_scan_t mps_fmt_scan,
                              mps_addr_t base, mps_addr_t limit)
{
  Arena arena = (Arena)mps_arena;
  Rank rank = (Rank)mps_rank;
  FormatScanMethod scan = (FormatScanMethod)mps_fmt_scan;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;
  
  ArenaEnter(arena);

  AVER(mps_root_o != NULL);

  res = RootCreateFmt(&root, arena, rank, mode, scan,
                      (Addr)base, (Addr)limit);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_reg(mps_root_t *mps_root_o,
                              mps_arena_t mps_arena,
                              mps_rank_t mps_rank,
                              mps_rm_t mps_rm,
                              mps_thr_t mps_thr,
                              mps_reg_scan_t mps_reg_scan,
                              void *reg_scan_p,
                              size_t mps_size)
{
  Arena arena = (Arena)mps_arena;
  Rank rank = (Rank)mps_rank;
  Thread thread = (Thread)mps_thr;
  Root root;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(mps_reg_scan != NULL);
  AVER(mps_reg_scan == mps_stack_scan_ambig); /* .reg.scan */
  AVER(reg_scan_p != NULL); /* stackBot */
  AVER(rank == MPS_RANK_AMBIG);
  AVER(mps_rm == (mps_rm_t)0);

  /* See .root-mode. */
  res = RootCreateReg(&root, arena, rank, thread,
                      (RootScanRegMethod)mps_reg_scan,
                      reg_scan_p, mps_size);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}


/* mps_stack_scan_ambig -- scan the thread state ambiguously
 *
 * See .reg-scan.
 */

mps_res_t mps_stack_scan_ambig(mps_ss_t mps_ss,
                               mps_thr_t mps_thr, void *p, size_t s)
{
  ScanState ss = (ScanState)mps_ss;
  Thread thread = (Thread)mps_thr;

  UNUSED(s);
  return ThreadScan(ss, thread, p);
}


void mps_root_destroy(mps_root_t mps_root)
{
  Root root = (Root)mps_root;
  Arena arena;
  
  AVER(CHECKT(Root, root));
  arena = RootArena(root);

  ArenaEnter(arena);

  AVERT(Root, root);

  RootDestroy(root);

  ArenaLeave(arena);
}


void (mps_tramp)(void **r_o,
                 void *(*f)(void *p, size_t s),
                 void *p, size_t s)
{
  AVER(r_o != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  ProtTramp(r_o, f, p, s);
}


mps_res_t mps_thread_reg(mps_thr_t *mps_thr_o, mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  Thread thread;
  Res res;

  ArenaEnter(arena);

  AVER(mps_thr_o != NULL);
  AVERT(Arena, arena);

  res = ThreadRegister(&thread, arena);

  ArenaLeave(arena);
  
  if(res != ResOK) return res;
  *mps_thr_o = (mps_thr_t)thread;
  return MPS_RES_OK;
}

void mps_thread_dereg(mps_thr_t mps_thr)
{
  Thread thread = (Thread)mps_thr;
  Arena arena;
  
  AVER(CHECKT(Thread, thread));
  arena = ThreadArena(thread);

  ArenaEnter(arena);

  ThreadDeregister(thread, arena);

  ArenaLeave(arena);
}

void mps_ld_reset(mps_ld_t mps_ld, mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  LD ld = (LD)mps_ld;

  ArenaEnter(arena);

  LDReset(ld, arena);

  ArenaLeave(arena);
}


/* mps_ld_add -- add a reference to a location dependency
 *
 * See design.mps.interface.c.lock-free.
 */

void mps_ld_add(mps_ld_t mps_ld, mps_arena_t mps_arena, mps_addr_t addr)
{
  Arena arena = (Arena)mps_arena;
  LD ld = (LD)mps_ld;

  LDAdd(ld, arena, (Addr)addr);
}


/* mps_ld_merge -- merge two location dependencies
 *
 * See design.mps.interface.c.lock-free.
 */

void mps_ld_merge(mps_ld_t mps_ld, mps_arena_t mps_arena,
                  mps_ld_t mps_from)
{
  Arena arena = (Arena)mps_arena;
  LD ld = (LD)mps_ld;
  LD from = (LD)mps_from;

  LDMerge(ld, arena, from);
}


/* mps_ld_isstale -- check whether a location dependency is "stale"
 *
 * See design.mps.interface.c.lock-free.
 */

mps_bool_t mps_ld_isstale(mps_ld_t mps_ld, mps_arena_t mps_arena,
                          mps_addr_t addr)
{
  Arena arena = (Arena)mps_arena;
  LD ld = (LD)mps_ld;
  Bool b;

  b = LDIsStale(ld, arena, (Addr)addr);

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

mps_word_t mps_collections(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  return ArenaEpoch(arena); /* thread safe: see impl.h.arena.epoch.ts */
}


/* mps_finalize -- register for finalize
 */

mps_res_t mps_finalize(mps_arena_t mps_arena, mps_addr_t *refref)
{
  Res res;
  Addr object;
  Arena arena = (Arena)mps_arena;

  ArenaEnter(arena);

  object = (Addr)ArenaPeek(arena, (Addr)refref);
  res = ArenaFinalize(arena, object);

  ArenaLeave(arena);

  return res;
}

void mps_definalize(mps_arena_t arena, mps_addr_t *refref)
{
  /* Not yet implemented */
  UNUSED(arena); UNUSED(refref);
  NOTREACHED;
}


/* Messages */


mps_bool_t mps_message_poll(mps_arena_t mps_arena)
{
  Bool b;
  Arena arena = (Arena)mps_arena;

  ArenaEnter(arena);

  b = MessagePoll(arena);

  ArenaLeave(arena);

  return b;
}


mps_message_type_t mps_message_type(mps_arena_t mps_arena,
                                    mps_message_t mps_message)
{
  Arena arena = (Arena)mps_arena;
  Message message = (Message)mps_message;
  MessageType type;

  ArenaEnter(arena);

  type = MessageGetType(message);

  ArenaLeave(arena);

  return (mps_message_type_t)type;
}

void mps_message_discard(mps_arena_t mps_arena,
                         mps_message_t mps_message)
{
  Arena arena = (Arena)mps_arena;
  Message message = (Message)mps_message;

  ArenaEnter(arena);

  MessageDiscard(arena, message);

  ArenaLeave(arena);
}

void mps_message_type_enable(mps_arena_t mps_arena,
                             mps_message_type_t mps_type)
{
  Arena arena = (Arena)mps_arena;
  MessageType type = (MessageType)mps_type;

  ArenaEnter(arena);

  MessageTypeEnable(arena, type);

  ArenaLeave(arena);
}

mps_bool_t mps_message_get(mps_message_t *mps_message_return,
                           mps_arena_t mps_arena,
                           mps_message_type_t mps_type)
{
  Bool b;
  Arena arena = (Arena)mps_arena;
  MessageType type = (MessageType)mps_type;
  Message message;

  ArenaEnter(arena);

  b = MessageGet(&message, arena, type);

  ArenaLeave(arena);

  if(b) {
    *mps_message_return = (mps_message_t)message;
  }
  return b;
}

mps_bool_t mps_message_queue_type(mps_message_type_t *mps_message_type_return,
                                  mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  MessageType type;
  Bool b;

  ArenaEnter(arena);

  b = MessageQueueType(&type, arena);

  ArenaLeave(arena);

  if(b) {
    *mps_message_type_return = (mps_message_type_t)type;
  }
  return b;
}


/* Message Type Specific Methods */

/* MPS_MESSAGE_TYPE_FINALIZATION */

void mps_message_finalization_ref(mps_addr_t *mps_addr_return,
                                  mps_arena_t mps_arena,
                                  mps_message_t mps_message)
{
  Arena arena = (Arena)mps_arena;
  Message message = (Message)mps_message;
  Ref ref;

  AVER(mps_addr_return != NULL);
  
  ArenaEnter(arena);

  AVERT(Arena, arena);
  MessageFinalizationRef(&ref, arena, message);
  ArenaPoke(arena, (Addr)mps_addr_return, ref);

  ArenaLeave(arena);
}

/* MPS_MESSAGE_TYPE_COLLECTION_STATS */

size_t mps_message_collection_stats_live_size(mps_arena_t mps_arena, 
                                              mps_message_t mps_message)
{
  Arena arena = (Arena)mps_arena;
  Message message = (Message)mps_message;
  Size size;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  size = MessageCollectionStatsLiveSize(message);

  ArenaLeave(arena);

  return (size_t)size;
}

size_t mps_message_collection_stats_condemned_size(mps_arena_t mps_arena,
                                                   mps_message_t mps_message)
{
  Arena arena = (Arena)mps_arena;
  Message message = (Message)mps_message;
  Size size;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  size = MessageCollectionStatsCondemnedSize(message);

  ArenaLeave(arena);

  return (size_t)size;
}

size_t mps_message_collection_stats_not_condemned_size(mps_arena_t mps_arena,
                                                       mps_message_t mps_message)
{
  Arena arena = (Arena)mps_arena;
  Message message = (Message)mps_message;
  Size size;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  size = MessageCollectionStatsNotCondemnedSize(message);

  ArenaLeave(arena);

  return (size_t)size;
}


/* Telemetry */

mps_word_t mps_telemetry_control(mps_word_t resetMask, 
                                 mps_word_t flipMask)
{
  /* Doesn't require locking and isn't arena-specific. */
  return EventControl((Word)resetMask, (Word)flipMask);
}

mps_word_t mps_telemetry_intern(const char *label)
{
  AVER(label != NULL);
  return (mps_word_t)EventInternString(label);
}

void mps_telemetry_label(mps_addr_t addr, mps_word_t intern_id)
{
  EventLabelAddr((Addr)addr, (Word)intern_id);
}


/* Allocation Patterns */


mps_alloc_pattern_t mps_alloc_pattern_ramp(void)
{
  return (mps_alloc_pattern_t)AllocPatternRamp();
}

mps_alloc_pattern_t mps_alloc_pattern_ramp_collect_all(void)
{
  return (mps_alloc_pattern_t)AllocPatternRampCollectAll();
}


/* mps_ap_alloc_pattern_begin -- signal start of an allocation pattern
 *
 * .ramp.hack: There are only two allocation patterns, both ramps.
 * So we assume it's a ramp, and call BufferRampBegin/End directly,
 * without dispatching.  No point in creating a mechanism for that.
 */

mps_res_t mps_ap_alloc_pattern_begin(mps_ap_t mps_ap,
                                     mps_alloc_pattern_t alloc_pattern)
{
  Buffer buf;
  Arena arena;

  AVER(mps_ap != NULL);
  buf = BufferOfAP((AP)mps_ap);
  AVER(CHECKT(Buffer, buf));

  arena = BufferArena(buf);
  ArenaEnter(arena);

  BufferRampBegin(buf, (AllocPattern)alloc_pattern);

  ArenaLeave(arena);
  return MPS_RES_OK;
}


mps_res_t mps_ap_alloc_pattern_end(mps_ap_t mps_ap,
                                   mps_alloc_pattern_t alloc_pattern)
{
  Buffer buf;
  Arena arena;
  Res res;

  AVER(mps_ap != NULL);
  buf = BufferOfAP((AP)mps_ap);
  AVER(CHECKT(Buffer, buf));
  UNUSED(alloc_pattern); /* .ramp.hack */

  arena = BufferArena(buf);
  ArenaEnter(arena);

  res = BufferRampEnd(buf);
  ArenaPoll(arena);                     /* .poll */

  ArenaLeave(arena);
  return res;
}


mps_res_t mps_ap_alloc_pattern_reset(mps_ap_t mps_ap)
{
  Buffer buf;
  Arena arena;

  AVER(mps_ap != NULL);
  buf = BufferOfAP((AP)mps_ap);
  AVER(CHECKT(Buffer, buf));

  arena = BufferArena(buf);
  ArenaEnter(arena);

  BufferRampReset(buf);
  ArenaPoll(arena);                     /* .poll */

  ArenaLeave(arena);
  return MPS_RES_OK;
}


/* Low memory reservoir */
void mps_reservoir_limit_set(mps_arena_t mps_arena, size_t size)
{
  Arena arena = (Arena)mps_arena;

  ArenaEnter(arena);
  ArenaReservoirLimitSet(arena, size);
  ArenaLeave(arena);
}

size_t mps_reservoir_limit(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  Size size;

  ArenaEnter(arena);
  size = ArenaReservoirLimit(arena);
  ArenaLeave(arena);

  return size;
}

size_t mps_reservoir_available(mps_arena_t mps_arena)
{
  Arena arena = (Arena)mps_arena;
  Size size;

  ArenaEnter(arena);
  size = ArenaReservoirAvailable(arena);
  ArenaLeave(arena);

  return size;
}
