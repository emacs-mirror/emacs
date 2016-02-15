/* mpsi.c: MEMORY POOL SYSTEM C INTERFACE LAYER
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * .purpose: This code bridges between the MPS interface to C,
 * <code/mps.h>, and the internal MPM interfaces, as defined by
 * <code/mpm.h>.  .purpose.check: It performs checking of the C client's
 * usage of the MPS Interface.  .purpose.thread: It excludes multiple
 * threads from the MPM by locking the Arena (see <design/thread-safety/>).
 *
 * .design: <design/interface-c/>
 *
 *
 * NOTES
 *
 * .note.break-out: Take care not to return when "inside" the Arena
 * (between ArenaEnter and ArenaLeave) as this will leave the Arena in
 * an unsuitable state for re-entry.
 *
 * .note.avert: Use AVERT only when "inside" the Arena (between
 * ArenaEnter and ArenaLeave), as it's not thread-safe in all
 * varieties. Use AVER(TESTT) otherwise. See
 * <design/sig/#check.arg.unlocked>.
 *
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .check.protocol: (rule.impl.req) More could be done in this code to
 * check that protocols are obeyed by the client.  It probably doesn't
 * meet checking requirements.
 *
 * .poll: (rule.universal.complete) Various allocation methods call
 * ArenaPoll to allow the MPM to "steal" CPU time and get on with
 * background tasks such as incremental GC.
 *
 * .root-mode: (rule.universal.complete) The root "mode", which
 * specifies things like the protectability of roots, is ignored at
 * present.  This is because the MPM doesn't ever try to protect them.
 * In future, it will.
 *
 * .naming: (rule.impl.guide) The exported identifiers do not follow the
 * normal MPS naming conventions.  See <design/interface-c/#naming>.
 */

#include "mpm.h"
#include "mps.h"
#include "sac.h"

#include <stdarg.h>


SRCID(mpsi, "$Id$");


/* mpsi_check -- check consistency of interface mappings
 *
 * .check.purpose: The mpsi_check function attempts to check whether
 * the defintions in <code/mpsi.h> match the equivalent definition in
 * the MPM.  It is checking the assumptions made in the other functions
 * in this implementation.
 *
 * .check.empty: Note that mpsi_check compiles away to almost nothing.
 *
 * .check.enum.cast: enum comparisons have to be cast to avoid a warning
 * from the SunPro C compiler.  See builder.sc.warn.enum.  */

ATTRIBUTE_UNUSED
static Bool mpsi_check(void)
{
  CHECKL(COMPATTYPE(mps_res_t, Res));

  /* Check that external and internal message types match. */
  /* See <code/mps.h#message.types> and */
  /* <code/mpmtypes.h#message.types>. */
  /* Also see .check.enum.cast. */
  CHECKL(COMPATTYPE(mps_message_type_t, MessageType));
  CHECKL((int)MessageTypeFINALIZATION
         == (int)_mps_MESSAGE_TYPE_FINALIZATION);
  CHECKL((int)MessageTypeGC
         == (int)_mps_MESSAGE_TYPE_GC);
  CHECKL((int)MessageTypeGCSTART
         == (int)_mps_MESSAGE_TYPE_GC_START);

  /* The external idea of a word width and the internal one */
  /* had better match.  See <design/interface-c/#cons>. */
  CHECKL(sizeof(mps_word_t) == sizeof(void *));
  CHECKL(COMPATTYPE(mps_word_t, Word));

  /* The external idea of an address and the internal one */
  /* had better match. */
  CHECKL(COMPATTYPE(mps_addr_t, Addr));

  /* The external idea of size and the internal one had */
  /* better match.  See <design/interface-c/#cons.size> */
  /* and <design/interface-c/#pun.size>. */
  CHECKL(COMPATTYPE(size_t, Size));

  /* Clock values are passed from external to internal and back */
  /* out to external. */
  CHECKL(COMPATTYPE(mps_clock_t, Clock));

  return TRUE;
}


/* Ranks
 *
 * Here a rank returning function is defined for all client visible
 * ranks.
 *
 * .rank.final.not: RankFINAL does not have a corresponding function as
 * it is only used internally.  */

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


mps_res_t mps_arena_extend(mps_arena_t arena,
                           mps_addr_t base, size_t size)
{
  Res res;

  ArenaEnter(arena);
  AVER(size > 0);
  res = ArenaExtend(arena, (Addr)base, (Size)size);
  ArenaLeave(arena);

  return (mps_res_t)res;
}

size_t mps_arena_reserved(mps_arena_t arena)
{
  Size size;

  ArenaEnter(arena);
  size = ArenaReserved(arena);
  ArenaLeave(arena);

  return (size_t)size;
}

size_t mps_arena_committed(mps_arena_t arena)
{
  Size size;

  ArenaEnter(arena);
  size = ArenaCommitted(arena);
  ArenaLeave(arena);

  return (size_t)size;
}

size_t mps_arena_spare_committed(mps_arena_t arena)
{
  Size size;

  ArenaEnter(arena);
  size = ArenaSpareCommitted(arena);
  ArenaLeave(arena);

  return (size_t)size;
}

size_t mps_arena_commit_limit(mps_arena_t arena)
{
  Size size;

  ArenaEnter(arena);
  size = ArenaCommitLimit(arena);
  ArenaLeave(arena);

  return size;
}

mps_res_t mps_arena_commit_limit_set(mps_arena_t arena, size_t limit)
{
  Res res;

  ArenaEnter(arena);
  res = ArenaSetCommitLimit(arena, limit);
  ArenaLeave(arena);

  return (mps_res_t)res;
}

void mps_arena_spare_commit_limit_set(mps_arena_t arena, size_t limit)
{
  ArenaEnter(arena);
  ArenaSetSpareCommitLimit(arena, limit);
  ArenaLeave(arena);

  return;
}

size_t mps_arena_spare_commit_limit(mps_arena_t arena)
{
  size_t limit;

  ArenaEnter(arena);
  limit = ArenaSpareCommitLimit(arena);
  ArenaLeave(arena);

  return limit;
}

void mps_arena_clamp(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaClamp(ArenaGlobals(arena));
  ArenaLeave(arena);
}


void mps_arena_release(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaRelease(ArenaGlobals(arena));
  ArenaLeave(arena);
}


void mps_arena_park(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaPark(ArenaGlobals(arena));
  ArenaLeave(arena);
}


void mps_arena_expose(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaExposeRemember(ArenaGlobals(arena), FALSE);
  ArenaLeave(arena);
}

/* Null implementations of remember and restore */
void mps_arena_unsafe_expose_remember_protection(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaExposeRemember(ArenaGlobals(arena), TRUE);
  ArenaLeave(arena);
}

void mps_arena_unsafe_restore_protection(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaRestoreProtection(ArenaGlobals(arena));
  ArenaLeave(arena);
}


mps_res_t mps_arena_start_collect(mps_arena_t arena)
{
  Res res;
  ArenaEnter(arena);
  res = ArenaStartCollect(ArenaGlobals(arena), TraceStartWhyCLIENTFULL_INCREMENTAL);
  ArenaLeave(arena);
  return (mps_res_t)res;
}

mps_res_t mps_arena_collect(mps_arena_t arena)
{
  Res res;
  ArenaEnter(arena);
  res = ArenaCollect(ArenaGlobals(arena), TraceStartWhyCLIENTFULL_BLOCK);
  ArenaLeave(arena);
  return (mps_res_t)res;
}

mps_bool_t mps_arena_step(mps_arena_t arena,
                          double interval,
                          double multiplier)
{
  Bool b;
  ArenaEnter(arena);
  b = ArenaStep(ArenaGlobals(arena), interval, multiplier);
  ArenaLeave(arena);
  return b;
}


/* mps_arena_create -- create an arena object */

mps_res_t mps_arena_create(mps_arena_t *mps_arena_o,
                           mps_arena_class_t mps_arena_class, ...)
{
  mps_res_t res;
  va_list varargs;
  va_start(varargs, mps_arena_class);
  res = mps_arena_create_v(mps_arena_o, mps_arena_class, varargs);
  va_end(varargs);
  return (mps_res_t)res;
}


/* mps_arena_create_v -- create an arena object */

mps_res_t mps_arena_create_v(mps_arena_t *mps_arena_o,
                             mps_arena_class_t arena_class,
                             va_list varargs)
{
  mps_arg_s args[MPS_ARGS_MAX];
  AVER(TESTT(ArenaClass, arena_class));
  arena_class->varargs(args, varargs);
  return mps_arena_create_k(mps_arena_o, arena_class, args);
}


/* mps_arena_create_k -- create an arena object */

mps_res_t mps_arena_create_k(mps_arena_t *mps_arena_o,
                             mps_arena_class_t arena_class,
                             mps_arg_s mps_args[])
{
  Arena arena;
  Res res;

  /* This is the first real call that the client will have to make, */
  /* so check static consistency here. */
  AVER(mpsi_check());

  AVER(mps_arena_o != NULL);

  res = ArenaCreate(&arena, arena_class, mps_args);
  if (res != ResOK)
    return (mps_res_t)res;

  ArenaLeave(arena);
  *mps_arena_o = (mps_arena_t)arena;
  return MPS_RES_OK;
}


/* mps_arena_destroy -- destroy an arena object */

void mps_arena_destroy(mps_arena_t arena)
{
  ArenaEnter(arena);
  ArenaDestroy(arena);
}


/* mps_arena_has_addr -- is this address managed by this arena? */

mps_bool_t mps_arena_has_addr(mps_arena_t arena, mps_addr_t p)
{
    Bool b;

    /* One of the few functions that can be called
       during the call to an MPS function.  IE this function
       can be called when walking the heap. */
    ArenaEnterRecursive(arena);
    AVERT(Arena, arena);
    b = ArenaHasAddr(arena, (Addr)p);
    ArenaLeaveRecursive(arena);
    return b;
}


/* mps_addr_pool -- return the pool containing the given address
 *
 * Wrapper for PoolOfAddr.  Note: may return an MPS-internal pool.
 */

mps_bool_t mps_addr_pool(mps_pool_t *mps_pool_o,
                         mps_arena_t arena,
                         mps_addr_t p)
{
    Bool b;
    Pool pool;

    AVER(mps_pool_o != NULL);
    /* mps_arena -- will be checked by ArenaEnterRecursive */
    /* p -- cannot be checked */

    /* One of the few functions that can be called
       during the call to an MPS function.  IE this function
       can be called when walking the heap. */
    ArenaEnterRecursive(arena);
    b = PoolOfAddr(&pool, arena, (Addr)p);
    ArenaLeaveRecursive(arena);

    if(b)
      *mps_pool_o = (mps_pool_t)pool;

    return b;
}


/* mps_addr_fmt -- what format might this address have?
 *
 * .per-pool: There's no reason why all objects in a pool should have 
 * the same format.  But currently, MPS internals support at most one 
 * format per pool.
 *
 * If the address is in a pool and has a format, returns TRUE and 
 * updates *mps_fmt_o to be that format.  Otherwise, returns FALSE 
 * and does not update *mps_fmt_o.
 *
 * Note: may return an MPS-internal format.
 */
mps_bool_t mps_addr_fmt(mps_fmt_t *mps_fmt_o,
                        mps_arena_t arena,
                        mps_addr_t p)
{
    Bool b;
    Pool pool;
    Format format = 0;

    AVER(mps_fmt_o != NULL);
    /* mps_arena -- will be checked by ArenaEnterRecursive */
    /* p -- cannot be checked */

    /* One of the few functions that can be called
       during the call to an MPS function.  IE this function
       can be called when walking the heap. */
    ArenaEnterRecursive(arena);
    /* .per-pool */
    b = PoolOfAddr(&pool, arena, (Addr)p);
    if(b)
      b = PoolFormat(&format, pool);
    ArenaLeaveRecursive(arena);

    if(b)
      *mps_fmt_o = (mps_fmt_t)format;

    return b;
}


/* mps_fmt_create_k -- create an object format using keyword arguments */

mps_res_t mps_fmt_create_k(mps_fmt_t *mps_fmt_o,
                           mps_arena_t arena,
                           mps_arg_s args[])
{
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_o != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);

  res = FormatCreate(&format, arena, args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}


/* mps_fmt_create_A -- create an object format of variant A
 *
 * .fmt.create.A.purpose: This function converts an object format spec
 * of variant "A" into an MPM Format object.  See
 * <design/interface-c/#fmt.extend> for justification of the way that
 * the format structure is declared as "mps_fmt_A".  */

mps_res_t mps_fmt_create_A(mps_fmt_t *mps_fmt_o,
                           mps_arena_t arena,
                           mps_fmt_A_s *mps_fmt_A)
{
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_o != NULL);
  AVERT(Arena, arena);
  AVER(mps_fmt_A != NULL);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, mps_fmt_A->align);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, mps_fmt_A->scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, mps_fmt_A->skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, mps_fmt_A->fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, mps_fmt_A->isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, mps_fmt_A->pad);
    res = FormatCreate(&format, arena, args);
  } MPS_ARGS_END(args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}


/* mps_fmt_create_B -- create an object format of variant B */

mps_res_t mps_fmt_create_B(mps_fmt_t *mps_fmt_o,
                           mps_arena_t arena,
                           mps_fmt_B_s *mps_fmt_B)
{
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_o != NULL);
  AVERT(Arena, arena);
  AVER(mps_fmt_B != NULL);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, mps_fmt_B->align);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, mps_fmt_B->scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, mps_fmt_B->skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, mps_fmt_B->fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, mps_fmt_B->isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, mps_fmt_B->pad);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_CLASS, mps_fmt_B->mps_class);
    res = FormatCreate(&format, arena, args);
  } MPS_ARGS_END(args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}


/* mps_fmt_create_auto_header -- create a format of variant auto_header */

mps_res_t mps_fmt_create_auto_header(mps_fmt_t *mps_fmt_o,
                                     mps_arena_t arena,
                                     mps_fmt_auto_header_s *mps_fmt)
{
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_o != NULL);
  AVERT(Arena, arena);
  AVER(mps_fmt != NULL);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, mps_fmt->align);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_HEADER_SIZE, mps_fmt->mps_headerSize);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, mps_fmt->scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, mps_fmt->skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, mps_fmt->fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, mps_fmt->isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, mps_fmt->pad);
    res = FormatCreate(&format, arena, args);
  } MPS_ARGS_END(args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}


/* mps_fmt_create_fixed -- create an object format of variant fixed */

mps_res_t mps_fmt_create_fixed(mps_fmt_t *mps_fmt_o,
                               mps_arena_t arena,
                               mps_fmt_fixed_s *mps_fmt_fixed)
{
  Format format;
  Res res;

  ArenaEnter(arena);

  AVER(mps_fmt_o != NULL);
  AVERT(Arena, arena);
  AVER(mps_fmt_fixed != NULL);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, mps_fmt_fixed->align);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, mps_fmt_fixed->scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, mps_fmt_fixed->fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, mps_fmt_fixed->isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, mps_fmt_fixed->pad);
    res = FormatCreate(&format, arena, args);
  } MPS_ARGS_END(args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_fmt_o = (mps_fmt_t)format;
  return MPS_RES_OK;
}


/* mps_fmt_destroy -- destroy a format object */

void mps_fmt_destroy(mps_fmt_t format)
{
  Arena arena;

  AVER(TESTT(Format, format));
  arena = FormatArena(format);

  ArenaEnter(arena);

  FormatDestroy(format);

  ArenaLeave(arena);
}


mps_res_t mps_pool_create(mps_pool_t *mps_pool_o, mps_arena_t arena,
                          mps_pool_class_t mps_class, ...)
{
  mps_res_t res;
  va_list varargs;
  va_start(varargs, mps_class);
  res = mps_pool_create_v(mps_pool_o, arena, mps_class, varargs);
  va_end(varargs);
  return res;
}

mps_res_t mps_pool_create_v(mps_pool_t *mps_pool_o, mps_arena_t arena,
                            mps_pool_class_t pool_class, va_list varargs)
{
  mps_arg_s args[MPS_ARGS_MAX];
  AVER(TESTT(PoolClass, pool_class));
  pool_class->varargs(args, varargs);
  return mps_pool_create_k(mps_pool_o, arena, pool_class, args);
}

mps_res_t mps_pool_create_k(mps_pool_t *mps_pool_o, mps_arena_t arena,
                            mps_pool_class_t pool_class, mps_arg_s args[])
{
  Pool pool;
  Res res;

  ArenaEnter(arena);

  AVER(mps_pool_o != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, pool_class);
  AVERT(ArgList, args);

  res = PoolCreate(&pool, arena, pool_class, args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_pool_o = (mps_pool_t)pool;
  return MPS_RES_OK;
}

void mps_pool_destroy(mps_pool_t pool)
{
  Arena arena;

  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  PoolDestroy(pool);

  ArenaLeave(arena);
}

size_t mps_pool_total_size(mps_pool_t pool)
{
  Arena arena;
  Size size;

  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  size = PoolTotalSize(pool);

  ArenaLeave(arena);

  return (size_t)size;
}

size_t mps_pool_free_size(mps_pool_t pool)
{
  Arena arena;
  Size size;

  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  size = PoolFreeSize(pool);

  ArenaLeave(arena);

  return (size_t)size;
}


mps_res_t mps_alloc(mps_addr_t *p_o, mps_pool_t pool, size_t size)
{
  Arena arena;
  Addr p;
  Res res;

  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  ArenaPoll(ArenaGlobals(arena)); /* .poll */

  AVER(p_o != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  /* Note: class may allow unaligned size, see */
  /* <design/class-interface/#alloc.size.align>. */
  /* Rest ignored, see .varargs. */

  /* @@@@ There is currently no requirement for reservoirs to work */
  /* with unbuffered allocation. */
  res = PoolAlloc(&p, pool, size, FALSE);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *p_o = (mps_addr_t)p;
  return MPS_RES_OK;
}


/* mps_alloc_v -- allocate in pool with varargs.  Deprecated in 1.112. */

mps_res_t mps_alloc_v(mps_addr_t *p_o, mps_pool_t mps_pool, size_t size,
                      va_list args)
{
  mps_res_t res;

  UNUSED(args); /* See .varargs. */
  res = mps_alloc(p_o, mps_pool, size);
  return res;
}


void mps_free(mps_pool_t pool, mps_addr_t p, size_t size)
{
  Arena arena;

  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);
  AVER(size > 0);
  /* Note: class may allow unaligned size, see */
  /* <design/class-interface/#alloc.size.align>. */

  PoolFree(pool, (Addr)p, size);
  ArenaLeave(arena);
}


/* mps_ap_create -- create an allocation point */

mps_res_t mps_ap_create(mps_ap_t *mps_ap_o, mps_pool_t pool, ...)
{
  mps_res_t res;
  va_list varargs;
  va_start(varargs, pool);
  res = mps_ap_create_v(mps_ap_o, pool, varargs);
  va_end(varargs);
  return res;
}


/* mps_ap_create_v -- create an allocation point, with varargs */

mps_res_t mps_ap_create_v(mps_ap_t *mps_ap_o, mps_pool_t pool,
                          va_list varargs)
{
  Arena arena;
  BufferClass bufclass;
  mps_arg_s args[MPS_ARGS_MAX];

  AVER(mps_ap_o != NULL);
  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);
  
  ArenaEnter(arena);
  AVERT(Pool, pool);
  bufclass = PoolDefaultBufferClass(pool);
  bufclass->varargs(args, varargs);
  ArenaLeave(arena);

  return mps_ap_create_k(mps_ap_o, pool, args);
}

/* mps_ap_create_k -- create an allocation point, with keyword args */

mps_res_t mps_ap_create_k(mps_ap_t *mps_ap_o,
                          mps_pool_t pool,
                          mps_arg_s args[]) {
  Arena arena;
  Buffer buf;
  BufferClass bufclass;
  Res res;

  AVER(mps_ap_o != NULL);
  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);

  bufclass = PoolDefaultBufferClass(pool);
  res = BufferCreate(&buf, bufclass, pool, TRUE, args);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;

  *mps_ap_o = BufferAP(buf);
  return MPS_RES_OK;
}

void mps_ap_destroy(mps_ap_t mps_ap)
{
  Buffer buf = BufferOfAP(mps_ap);
  Arena arena;

  AVER(mps_ap != NULL);
  AVER(TESTT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  BufferDestroy(buf);

  ArenaLeave(arena);
}


/* mps_reserve -- allocate store in preparation for initialization
 *
 * .reserve.call: mps_reserve does not call BufferReserve, but instead
 * uses the in-line macro from <code/mps.h>.  This is so that it calls
 * mps_ap_fill and thence ArenaPoll (.poll).  The consistency checks are
 * those which can be done outside the MPM.  See also .commit.call.  */

mps_res_t (mps_reserve)(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  mps_res_t res;

  AVER(p_o != NULL);
  AVER(mps_ap != NULL);
  AVER(TESTT(Buffer, BufferOfAP(mps_ap)));
  AVER(mps_ap->init == mps_ap->alloc);
  AVER(size > 0);

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
  AVER(TESTT(Buffer, BufferOfAP(mps_ap)));
  AVER(mps_ap->init == mps_ap->alloc);

  MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK(res, *p_o, mps_ap, size);

  return res;
}



/* mps_commit -- commit initialized object, finishing allocation
 *
 * .commit.call: mps_commit does not call BufferCommit, but instead uses
 * the in-line commit macro from <code/mps.h>.  This is so that it calls
 * mps_ap_trip and thence ArenaPoll in future (.poll).  The consistency
 * checks here are the ones which can be done outside the MPM.  See also
 * .reserve.call.  */

mps_bool_t (mps_commit)(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  AVER(mps_ap != NULL);
  AVER(TESTT(Buffer, BufferOfAP(mps_ap)));
  AVER(p != NULL);
  AVER(size > 0);
  AVER(p == mps_ap->init);
  AVER(PointerAdd(mps_ap->init, size) == mps_ap->alloc);

  return mps_commit(mps_ap, p, size);
}


/* Allocation frame support
 *
 * These are candidates for being inlineable as macros.
 * These functions are easier to maintain, so we'll avoid
 * macros for now.  */


/* mps_ap_frame_push -- push a new allocation frame
 *
 * See <design/alloc-frame/#lw-frame.push>. */

mps_res_t (mps_ap_frame_push)(mps_frame_t *frame_o, mps_ap_t mps_ap)
{
  AVER(frame_o != NULL);
  AVER(mps_ap != NULL);

  /* Fail if between reserve & commit */
  if ((char *)mps_ap->alloc != (char *)mps_ap->init) {
    return MPS_RES_FAIL;
  }

  if (!mps_ap->_lwpoppending) {
    /* Valid state for a lightweight push */
    *frame_o = (mps_frame_t)mps_ap->init;
    return MPS_RES_OK;
  } else {
    /* Need a heavyweight push */
    Buffer buf = BufferOfAP(mps_ap);
    Arena arena;
    AllocFrame frame;
    Res res;

    AVER(TESTT(Buffer, buf));
    arena = BufferArena(buf);

    ArenaEnter(arena);
    AVERT(Buffer, buf);

    res = BufferFramePush(&frame, buf);

    if (res == ResOK) {
      *frame_o = (mps_frame_t)frame;
    }
    ArenaLeave(arena);
    return (mps_res_t)res;
  }
}

/* mps_ap_frame_pop -- push a new allocation frame
 *
 * See <design/alloc-frame/#lw-frame.pop>.  */

mps_res_t (mps_ap_frame_pop)(mps_ap_t mps_ap, mps_frame_t frame)
{
  AVER(mps_ap != NULL);
  /* Can't check frame because it's an arbitrary value */

  /* Fail if between reserve & commit */
  if ((char *)mps_ap->alloc != (char *)mps_ap->init) {
    return MPS_RES_FAIL;
  }

  if (mps_ap->_enabled) {
    /* Valid state for a lightweight pop */
    mps_ap->_frameptr = (mps_addr_t)frame; /* record pending pop */
    mps_ap->_lwpoppending = TRUE;
    mps_ap->limit = (mps_addr_t)0; /* trap the buffer */
    return MPS_RES_OK;

  } else {
    /* Need a heavyweight pop */
    Buffer buf = BufferOfAP(mps_ap);
    Arena arena;
    Res res;

    AVER(TESTT(Buffer, buf));
    arena = BufferArena(buf);

    ArenaEnter(arena);
    AVERT(Buffer, buf);

    res = BufferFramePop(buf, (AllocFrame)frame);

    ArenaLeave(arena);
    return (mps_res_t)res;
  }
}


/* mps_ap_fill -- called by mps_reserve when an AP hasn't enough arena
 *
 * .ap.fill.internal: mps_ap_fill is normally invoked by the
 * mps_reserve macro, but may be "called" directly by the client code
 * if necessary. See <manual/topic/allocation> */

mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t mps_ap, size_t size)
{
  Buffer buf = BufferOfAP(mps_ap);
  Arena arena;
  Addr p;
  Res res;

  AVER(mps_ap != NULL);
  AVER(TESTT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  ArenaPoll(ArenaGlobals(arena)); /* .poll */

  AVER(p_o != NULL);
  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  res = BufferFill(&p, buf, size, FALSE);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *p_o = (mps_addr_t)p;
  return MPS_RES_OK;
}


mps_res_t mps_ap_fill_with_reservoir_permit(mps_addr_t *p_o, mps_ap_t mps_ap,
                                            size_t size)
{
  Buffer buf = BufferOfAP(mps_ap);
  Arena arena;
  Addr p;
  Res res;

  AVER(mps_ap != NULL);
  AVER(TESTT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  ArenaPoll(ArenaGlobals(arena)); /* .poll */

  AVER(p_o != NULL);
  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  res = BufferFill(&p, buf, size, TRUE);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *p_o = (mps_addr_t)p;
  return MPS_RES_OK;
}


/* mps_ap_trip -- called by mps_commit when an AP is tripped
 *
 * .ap.trip.internal: mps_ap_trip is normally invoked by the
 * mps_commit macro, but may be "called" directly by the client code
 * if necessary. See <manual/topic/allocation> */

mps_bool_t mps_ap_trip(mps_ap_t mps_ap, mps_addr_t p, size_t size)
{
  Buffer buf = BufferOfAP(mps_ap);
  Arena arena;
  Bool b;

  AVER(mps_ap != NULL);
  AVER(TESTT(Buffer, buf));
  arena = BufferArena(buf);

  ArenaEnter(arena);

  AVERT(Buffer, buf);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buf)->alignment));

  b = BufferTrip(buf, (Addr)p, size);

  ArenaLeave(arena);

  return b;
}


/* mps_sac_create -- create an SAC object */

mps_res_t mps_sac_create(mps_sac_t *mps_sac_o, mps_pool_t pool,
                         size_t classes_count, mps_sac_classes_s *classes)
{
  Arena arena;
  SAC sac;
  Res res;

  AVER(mps_sac_o != NULL);
  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  res = SACCreate(&sac, pool, (Count)classes_count, classes);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_sac_o = ExternalSACOfSAC(sac);
  return (mps_res_t)res;
}


/* mps_sac_destroy -- destroy an SAC object */

void mps_sac_destroy(mps_sac_t mps_sac)
{
  SAC sac = SACOfExternalSAC(mps_sac);
  Arena arena;

  AVER(TESTT(SAC, sac));
  arena = SACArena(sac);

  ArenaEnter(arena);

  SACDestroy(sac);

  ArenaLeave(arena);
}


/* mps_sac_flush -- flush an SAC, releasing all memory held in it */

void mps_sac_flush(mps_sac_t mps_sac)
{
  SAC sac = SACOfExternalSAC(mps_sac);
  Arena arena;

  AVER(TESTT(SAC, sac));
  arena = SACArena(sac);

  ArenaEnter(arena);

  SACFlush(sac);

  ArenaLeave(arena);
}


/* mps_sac_fill -- alloc an object, and perhaps fill the cache */

mps_res_t mps_sac_fill(mps_addr_t *p_o, mps_sac_t mps_sac, size_t size,
                       mps_bool_t has_reservoir_permit)
{
  SAC sac = SACOfExternalSAC(mps_sac);
  Arena arena;
  Addr p = NULL;        /* suppress "may be used uninitialized" */
  Res res;

  AVER(p_o != NULL);
  AVER(TESTT(SAC, sac));
  arena = SACArena(sac);

  ArenaEnter(arena);

  res = SACFill(&p, sac, size, (has_reservoir_permit != 0));

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *p_o = (mps_addr_t)p;
  return (mps_res_t)res;
}


/* mps_sac_empty -- free an object, and perhaps empty the cache */

void mps_sac_empty(mps_sac_t mps_sac, mps_addr_t p, size_t size)
{
  SAC sac = SACOfExternalSAC(mps_sac);
  Arena arena;

  AVER(TESTT(SAC, sac));
  arena = SACArena(sac);

  ArenaEnter(arena);

  SACEmpty(sac, (Addr)p, (Size)size);

  ArenaLeave(arena);
}


/* mps_sac_alloc -- alloc an object, using cached space if possible */

mps_res_t mps_sac_alloc(mps_addr_t *p_o, mps_sac_t mps_sac, size_t size,
                        mps_bool_t has_reservoir_permit)
{
  Res res;

  AVER(p_o != NULL);
  AVER(TESTT(SAC, SACOfExternalSAC(mps_sac)));
  AVER(size > 0);

  MPS_SAC_ALLOC_FAST(res, *p_o, mps_sac, size, (has_reservoir_permit != 0));
  return (mps_res_t)res;
}


/* mps_sac_free -- free an object, to the cache if possible */

void mps_sac_free(mps_sac_t mps_sac, mps_addr_t p, size_t size)
{
  AVER(TESTT(SAC, SACOfExternalSAC(mps_sac)));
  /* Can't check p outside arena lock */
  AVER(size > 0);

  MPS_SAC_FREE_FAST(mps_sac, p, size);
}


/* Roots */


mps_res_t mps_root_create(mps_root_t *mps_root_o, mps_arena_t arena,
                          mps_rank_t mps_rank, mps_rm_t mps_rm,
                          mps_root_scan_t mps_root_scan, void *p, size_t s)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(mps_rm == (mps_rm_t)0);

  /* See .root-mode. */
  res = RootCreateFun(&root, arena, rank, mps_root_scan, p, s);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_table(mps_root_t *mps_root_o, mps_arena_t arena,
                                mps_rank_t mps_rank, mps_rm_t mps_rm,
                                mps_addr_t *base, size_t size)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(base != NULL);
  AVER(size > 0);

  /* .root.table-size: size is the length of the array at base, not */
  /* the size in bytes.  However, RootCreateArea expects base and */
  /* limit pointers.  Be careful. */

  res = RootCreateArea(&root, arena, rank, mode,
		       (Word *)base, (Word *)base + size,
		       mps_scan_area, NULL, 0);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

/* FIXME: document */
mps_res_t mps_root_create_area(mps_root_t *mps_root_o,
			       mps_arena_t arena,
			       mps_rank_t mps_rank, mps_rm_t mps_rm,
			       mps_word_t *base, mps_word_t *limit,
			       mps_area_scan_t scan_area,
			       void *closure, size_t closure_size)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);
  AVER(FUNCHECK(scan_area));
  /* Can't check anything about closure */

  res = RootCreateArea(&root, arena, rank, mode,
		       base, limit,
		       scan_area, closure, closure_size);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_table_tagged(mps_root_t *mps_root_o,
                                       mps_arena_t arena,
                                       mps_rank_t mps_rank, mps_rm_t mps_rm,
                                       mps_addr_t *base, size_t size,
				       mps_area_scan_t scan_area,
                                       mps_word_t mask,
				       mps_word_t pattern)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(base != NULL);
  AVER(size > 0);
  AVER(FUNCHECK(scan_area));
  /* Can't check anything about mask. */
  AVER((pattern & mask) == pattern);

  /* .root.table-size */
  res = RootCreateAreaTagged(&root, arena, rank, mode,
			     (Word *)base, (Word *)base + size,
			     scan_area, mask, pattern);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_table_masked(mps_root_t *mps_root_o,
                                       mps_arena_t arena,
                                       mps_rank_t mps_rank, mps_rm_t mps_rm,
                                       mps_addr_t *base, size_t size,
                                       mps_word_t mask)
{
  return mps_root_create_table_tagged(mps_root_o, arena, mps_rank, mps_rm,
				      base, size, mps_scan_area_masked,
				      mask, 0);
}

mps_res_t mps_root_create_fmt(mps_root_t *mps_root_o, mps_arena_t arena,
                              mps_rank_t mps_rank, mps_rm_t mps_rm,
                              mps_fmt_scan_t scan,
                              mps_addr_t base, mps_addr_t limit)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  RootMode mode = (RootMode)mps_rm;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);

  res = RootCreateFmt(&root, arena, rank, mode, scan, (Addr)base, (Addr)limit);

  ArenaLeave(arena);
  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}

mps_res_t mps_root_create_reg(mps_root_t *mps_root_o, mps_arena_t arena,
                              mps_rank_t mps_rank, mps_rm_t mps_rm,
                              mps_thr_t thread, mps_reg_scan_t mps_reg_scan,
                              void *stack, size_t mps_size)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(mps_reg_scan != NULL);
  AVER(mps_reg_scan == mps_stack_scan_ambig); /* .reg.scan */
  AVER(stack != NULL); /* stackBot */
  AVER(AddrIsAligned(stack, sizeof(Word)));
  AVER(rank == mps_rank_ambig());
  AVER(mps_rm == (mps_rm_t)0);

  UNUSED(mps_size);

  /* See .root-mode. */
  res = RootCreateThreadTagged(&root, arena, rank, thread,
			       mps_scan_area_tagged,
			       sizeof(mps_word_t) - 1, 0,
			       (Word *)stack);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}


/* FIXME: re-document */
mps_res_t mps_root_create_stack(mps_root_t *mps_root_o, mps_arena_t arena,
                                mps_rank_t mps_rank, mps_rm_t mps_rm,
                                mps_thr_t thread,
				mps_area_scan_t scan_area,
				mps_word_t mask, mps_word_t pattern,
				void *stack)
{
  Rank rank = (Rank)mps_rank;
  Root root;
  Res res;

  ArenaEnter(arena);

  AVER(mps_root_o != NULL);
  AVER(stack != NULL); /* stackBot */
  AVER(AddrIsAligned(stack, sizeof(Word)));
  AVER(rank == mps_rank_ambig());
  AVER(mps_rm == (mps_rm_t)0);
  AVER((~mask & pattern) == 0);

  /* See .root-mode. */
  res = RootCreateThreadTagged(&root, arena, rank, thread,
			       scan_area, mask, pattern,
			       (Word *)stack);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_root_o = (mps_root_t)root;
  return MPS_RES_OK;
}


/* mps_stack_scan_ambig -- scan the thread state ambiguously
 *
 * This is a helper function for the deprecated mps_root_create_reg
 * and should no longer be reached since that has been reimplemented
 * in terms of the more general RootCreateThreadTagged.
 */

mps_res_t mps_stack_scan_ambig(mps_ss_t mps_ss,
                               mps_thr_t thread, void *p, size_t s)
{
  UNUSED(mps_ss);
  UNUSED(thread);
  UNUSED(p);
  UNUSED(s);

  NOTREACHED;

  return ResUNIMPL;
}


void mps_root_destroy(mps_root_t mps_root)
{
  Root root = (Root)mps_root;
  Arena arena;

  arena = RootArena(root);

  ArenaEnter(arena);

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

  *r_o = (*f)(p, s);
}


mps_res_t mps_thread_reg(mps_thr_t *mps_thr_o, mps_arena_t arena)
{
  Thread thread;
  Res res;

  ArenaEnter(arena);

  AVER(mps_thr_o != NULL);
  AVERT(Arena, arena);

  res = ThreadRegister(&thread, arena);

  ArenaLeave(arena);

  if (res != ResOK)
    return (mps_res_t)res;
  *mps_thr_o = (mps_thr_t)thread;
  return MPS_RES_OK;
}

void mps_thread_dereg(mps_thr_t thread)
{
  Arena arena;

  AVER(ThreadCheckSimple(thread));
  arena = ThreadArena(thread);

  ArenaEnter(arena);

  ThreadDeregister(thread, arena);

  ArenaLeave(arena);
}

void mps_ld_reset(mps_ld_t ld, mps_arena_t arena)
{
  ArenaEnter(arena);
  LDReset(ld, arena);
  ArenaLeave(arena);
}


/* mps_ld_add -- add a reference to a location dependency
 *
 * See <design/interface-c/#lock-free>.  */

void mps_ld_add(mps_ld_t ld, mps_arena_t arena, mps_addr_t addr)
{
  LDAdd(ld, arena, (Addr)addr);
}


/* mps_ld_merge -- merge two location dependencies
 *
 * See <design/interface-c/#lock-free>.  */

void mps_ld_merge(mps_ld_t ld, mps_arena_t arena,
                  mps_ld_t from)
{
  LDMerge(ld, arena, from);
}


/* mps_ld_isstale -- check whether a location dependency is "stale"
 *
 * See <design/interface-c/#lock-free>.  */

mps_bool_t mps_ld_isstale(mps_ld_t ld, mps_arena_t arena,
                          mps_addr_t addr)
{
  Bool b;

  b = LDIsStale(ld, arena, (Addr)addr);

  return (mps_bool_t)b;
}

mps_bool_t mps_ld_isstale_any(mps_ld_t ld, mps_arena_t arena)
{
  Bool b;

  b = LDIsStaleAny(ld, arena);

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

mps_word_t mps_collections(mps_arena_t arena)
{
  return ArenaEpoch(arena); /* thread safe: see <code/arena.h#epoch.ts> */
}


/* mps_finalize -- register for finalization */

mps_res_t mps_finalize(mps_arena_t arena, mps_addr_t *refref)
{
  Res res;
  Addr object;

  ArenaEnter(arena);

  object = (Addr)ArenaPeek(arena, (Ref *)refref);
  res = ArenaFinalize(arena, object);

  ArenaLeave(arena);
  return (mps_res_t)res;
}


/* mps_definalize -- deregister for finalization */

mps_res_t mps_definalize(mps_arena_t arena, mps_addr_t *refref)
{
  Res res;
  Addr object;

  ArenaEnter(arena);

  object = (Addr)ArenaPeek(arena, (Ref *)refref);
  res = ArenaDefinalize(arena, object);

  ArenaLeave(arena);
  return (mps_res_t)res;
}


/* Messages */


void mps_message_type_enable(mps_arena_t arena,
                             mps_message_type_t mps_type)
{
  MessageType type = (MessageType)mps_type;

  ArenaEnter(arena);

  MessageTypeEnable(arena, type);

  ArenaLeave(arena);
}

void mps_message_type_disable(mps_arena_t arena,
                              mps_message_type_t mps_type)
{
  MessageType type = (MessageType)mps_type;

  ArenaEnter(arena);

  MessageTypeDisable(arena, type);

  ArenaLeave(arena);
}

mps_bool_t mps_message_poll(mps_arena_t arena)
{
  Bool b;

  ArenaEnter(arena);

  b = MessagePoll(arena);

  ArenaLeave(arena);
  return b;
}

mps_bool_t mps_message_queue_type(mps_message_type_t *mps_message_type_return,
                                  mps_arena_t arena)
{
  MessageType type;
  Bool b;

  ArenaEnter(arena);

  b = MessageQueueType(&type, arena);

  ArenaLeave(arena);
  if (b) {
    *mps_message_type_return = (mps_message_type_t)type;
  }
  return b;
}

mps_bool_t mps_message_get(mps_message_t *mps_message_return,
                           mps_arena_t arena,
                           mps_message_type_t mps_type)
{
  Bool b;
  MessageType type = (MessageType)mps_type;
  Message message;

  ArenaEnter(arena);

  b = MessageGet(&message, arena, type);

  ArenaLeave(arena);
  if (b) {
    *mps_message_return = (mps_message_t)message;
  }
  return b;
}

void mps_message_discard(mps_arena_t arena,
                         mps_message_t message)
{
  ArenaEnter(arena);

  MessageDiscard(arena, message);

  ArenaLeave(arena);
}


/* Message Methods */

/* -- All Message Types */

mps_message_type_t mps_message_type(mps_arena_t arena,
                                    mps_message_t message)
{
  MessageType type;

  ArenaEnter(arena);

  type = MessageGetType(message);

  ArenaLeave(arena);

  return (mps_message_type_t)type;
}

mps_clock_t mps_message_clock(mps_arena_t arena,
                              mps_message_t message)
{
  Clock postedClock;

  ArenaEnter(arena);

  postedClock = MessageGetClock(message);

  ArenaLeave(arena);

  return (mps_clock_t)postedClock;
}


/* -- mps_message_type_finalization */

void mps_message_finalization_ref(mps_addr_t *mps_addr_return,
                                  mps_arena_t arena,
                                  mps_message_t message)
{
  Ref ref;

  AVER(mps_addr_return != NULL);

  ArenaEnter(arena);

  AVERT(Arena, arena);
  MessageFinalizationRef(&ref, arena, message);
  ArenaPoke(arena, (Ref *)mps_addr_return, ref);

  ArenaLeave(arena);
}

/* -- mps_message_type_gc */

size_t mps_message_gc_live_size(mps_arena_t arena,
                                              mps_message_t message)
{
  Size size;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  size = MessageGCLiveSize(message);

  ArenaLeave(arena);
  return (size_t)size;
}

size_t mps_message_gc_condemned_size(mps_arena_t arena,
                                     mps_message_t message)
{
  Size size;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  size = MessageGCCondemnedSize(message);

  ArenaLeave(arena);
  return (size_t)size;
}

size_t mps_message_gc_not_condemned_size(mps_arena_t arena,
                                         mps_message_t message)
{
  Size size;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  size = MessageGCNotCondemnedSize(message);

  ArenaLeave(arena);
  return (size_t)size;
}

/* -- mps_message_type_gc_start */

const char *mps_message_gc_start_why(mps_arena_t arena,
  mps_message_t message)
{
  const char *s;

  ArenaEnter(arena);

  AVERT(Arena, arena);

  s = MessageGCStartWhy(message);

  ArenaLeave(arena);

  return s;
}


/* Telemetry */

/* TODO: need to consider locking. See job003387, job003388. */

mps_word_t mps_telemetry_control(mps_word_t resetMask, mps_word_t flipMask)
{
  return EventControl((Word)resetMask, (Word)flipMask);
}

void mps_telemetry_set(mps_word_t setMask)
{
  (void)EventControl((Word)setMask, (Word)setMask);
}

void mps_telemetry_reset(mps_word_t resetMask)
{
  (void)EventControl((Word)resetMask, 0);
}

mps_word_t mps_telemetry_get(void)
{
  return EventControl(0, 0);
}

mps_label_t mps_telemetry_intern(const char *label)
{
  AVER(label != NULL);
  return (mps_label_t)EventInternString(label);
}

void mps_telemetry_label(mps_addr_t addr, mps_label_t intern_id)
{
  EventLabelAddr((Addr)addr, (Word)intern_id);
}

void mps_telemetry_flush(void)
{
  /* Telemetry does its own concurrency control, so none here. */
  EventSync();
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
 * .ramp.hack: There are only two allocation patterns, both ramps.  So
 * we assume it's a ramp, and call BufferRampBegin/End directly, without
 * dispatching.  No point in creating a mechanism for that.  */

mps_res_t mps_ap_alloc_pattern_begin(mps_ap_t mps_ap,
                                     mps_alloc_pattern_t alloc_pattern)
{
  Buffer buf;
  Arena arena;

  AVER(mps_ap != NULL);
  buf = BufferOfAP(mps_ap);
  AVER(TESTT(Buffer, buf));

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
  buf = BufferOfAP(mps_ap);
  AVER(TESTT(Buffer, buf));
  UNUSED(alloc_pattern); /* .ramp.hack */

  arena = BufferArena(buf);
  ArenaEnter(arena);

  res = BufferRampEnd(buf);
  ArenaPoll(ArenaGlobals(arena)); /* .poll */

  ArenaLeave(arena);
  return (mps_res_t)res;
}


mps_res_t mps_ap_alloc_pattern_reset(mps_ap_t mps_ap)
{
  Buffer buf;
  Arena arena;

  AVER(mps_ap != NULL);
  buf = BufferOfAP(mps_ap);
  AVER(TESTT(Buffer, buf));

  arena = BufferArena(buf);
  ArenaEnter(arena);

  BufferRampReset(buf);
  ArenaPoll(ArenaGlobals(arena)); /* .poll */

  ArenaLeave(arena);
  return MPS_RES_OK;
}


/* Low memory reservoir */


/* mps_reservoir_limit_set -- set the reservoir size */

void mps_reservoir_limit_set(mps_arena_t arena, size_t size)
{
  ArenaEnter(arena);
  ReservoirSetLimit(ArenaReservoir(arena), size);
  ArenaLeave(arena);
}


/* mps_reservoir_limit -- return the reservoir size */

size_t mps_reservoir_limit(mps_arena_t arena)
{
  Size size;

  ArenaEnter(arena);

  size = ReservoirLimit(ArenaReservoir(arena));

  ArenaLeave(arena);
  return size;
}


/* mps_reservoir_available -- return memory available in the reservoir */

size_t mps_reservoir_available(mps_arena_t arena)
{
  Size size;

  ArenaEnter(arena);

  size = ReservoirAvailable(ArenaReservoir(arena));

  ArenaLeave(arena);
  return size;
}


/* Chains */


/* mps_chain_create -- create a chain */

mps_res_t mps_chain_create(mps_chain_t *chain_o, mps_arena_t arena,
                           size_t gen_count, mps_gen_param_s *params)
{
  Chain chain;
  Res res;

  ArenaEnter(arena);

  AVER(gen_count > 0);
  res = ChainCreate(&chain, arena, gen_count, (GenParamStruct *)params);

  ArenaLeave(arena);
  if (res != ResOK)
    return (mps_res_t)res;
  *chain_o = (mps_chain_t)chain;
  return MPS_RES_OK;
}


/* mps_chain_destroy -- destroy a chain */

void mps_chain_destroy(mps_chain_t chain)
{
  Arena arena;

  AVER(TESTT(Chain, chain));
  arena = chain->arena;

  ArenaEnter(arena);
  ChainDestroy(chain);
  ArenaLeave(arena);
}


/* _mps_args_set_key -- set the key for a keyword argument 
 *
 * This sets the key for the i'th keyword argument in the array args,
 * with bounds checking on i. It is used by the MPS_ARGS_BEGIN,
 * MPS_ARGS_ADD, and MPS_ARGS_DONE macros in mps.h.
 *
 * We implement this in a function here, rather than in a macro in
 * mps.h, so that we can use AVER to do the bounds checking.
 */

void _mps_args_set_key(mps_arg_s args[MPS_ARGS_MAX], unsigned i,
                       mps_key_t key)
{
  AVER(i < MPS_ARGS_MAX);
  args[i].key = key;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
