/* impl.h.mpm: MEMORY POOL MANAGER DEFINITIONS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .trans.bufferinit: The Buffer data structure has an Init field and
 * an Init method, there's a name clash.  We resolve this by calling the
 * accessor BufferGetInit. */

#ifndef mpm_h
#define mpm_h

#include "config.h"
#include "misc.h"
#include "check.h"

#include "event.h"
#include "lock.h"
#include "th.h"
#include "ss.h"
#include "mpslib.h"
#include "ring.h"
#include "tract.h" /* only for certain Seg macros */
#include "mpmtypes.h"
#include "mpmst.h"


/* MPMCheck -- check MPM assumptions */

extern Bool MPMCheck(void);


/* Miscellaneous Checks -- see impl.c.mpm */

/* design.mps.type.bool.check */
#define BoolCheck(b) ((unsigned)(b) <= 1)

extern Bool FunCheck(Fun f);
#define FUNCHECK(f)     (FunCheck((Fun)f))

extern Bool ShiftCheck(Shift shift);
extern Bool AttrCheck(Attr attr);
extern Bool RootVarCheck(RootVar rootVar);


/* Address/Size Interface -- see impl.c.mpm */

extern Bool AlignCheck(Align align);

extern Bool (WordIsAligned)(Word word, Align align);
#define WordIsAligned(w, a)     (((w) & ((a) - 1)) == 0)

extern Word (WordAlignUp)(Word word, Align align);
#define WordAlignUp(w, a)       (((w) + (a) - 1) & ~((Word)(a) - 1))

/* Rounds w up to a multiple of r, see impl.c.mpm for exact behaviour */
extern Word (WordRoundUp)(Word word, Size round);
#define WordRoundUp(w, r)       (((w)+(r)-1) - ((w)+(r)-1)%(r))

extern Word (WordAlignDown)(Word word, Align align);
#define WordAlignDown(w, a)     ((w) & ~((Word)(a) - 1))

#define size_tAlignUp(s, a) ((size_t)WordAlignUp((Word)(s), a))

#define PointerAdd(p, s) ((void *)((char *)(p) + (s)))
#define PointerSub(p, s) ((void *)((char *)(p) - (s)))

#define PointerOffset(base, limit) \
  ((size_t)((char *)(limit) - (char *)(base)))

#define PointerAlignUp(p, s) \
  ((void *)WordAlignUp((Word)(p), (Align)(s)))

#define AddrAdd(p, s) ((Addr)PointerAdd((void *)(p), s))
#define AddrSub(p, s) ((Addr)PointerSub((void *)(p), s))

#define AddrOffset(b, l) \
  ((Size)(PointerOffset((void *)(b), (void *)(l))))

extern Addr (AddrAlignDown)(Addr addr, Align align);
#define AddrAlignDown(p, a) ((Addr)WordAlignDown((Word)(p), a))

#define AlignWord(s)            ((Word)(s))

#define AddrIsAligned(p, a)     WordIsAligned((Word)(p), a)
#define AddrAlignUp(p, a)       ((Addr)WordAlignUp((Word)(p), a))

#define SizeIsAligned(s, a)     WordIsAligned((Word)(s), a)
#define SizeAlignUp(s, a)       ((Size)WordAlignUp((Word)(s), a))
#define SizeAlignDown(s, a)     ((Size)WordAlignDown((Word)(s), a))
/* r not required to be a power of 2 */
#define SizeRoundUp(s, r)       ((Size)WordRoundUp((Word)(s), (Size)(r)))

#define IndexIsAligned(s, a)    WordIsAligned((Word)(s), a)
#define IndexAlignUp(s, a)      ((Index)WordAlignUp((Word)(s), a))
#define IndexAlignDown(s, a)    ((Index)WordAlignDown((Word)(s), a))

#define AlignIsAligned(a1, a2)  WordIsAligned((Word)(a1), a2)


extern Addr (AddrSet)(Addr target, Byte value, Size size);
/* This is one of the places that implements Addr, so it's allowed to */
/* convert to void *, see design.mps.type.addr.ops.mem. */
#define AddrSet(target, value, size) \
  mps_lib_memset(target, (int)(value), size)

extern Addr (AddrCopy)(Addr target, Addr source, Size size);
#define AddrCopy(target, source, size) \
  mps_lib_memcpy(target, source, size)

extern int (AddrComp)(Addr a, Addr b, Size size);
#define AddrComp(a, b, size) \
  mps_lib_memcmp(a, b, size)


/* ADDR_PTR -- turns an Addr into a pointer to the given type */

#define ADDR_PTR(type, addr) ((type *)(addr))


/* Result codes */

extern Bool ResIsAllocFailure(Res res);


/* Logs and Powers
 *
 * SizeIsP2 returns TRUE if and only if size is a non-negative integer
 * power of 2, and FALSE otherwise.
 *
 * SizeLog2 returns the logarithm in base 2 of size.  size must be a
 * power of 2.
 *
 * SizeFloorLog2 returns the floor of the logarithm in base 2 of size.
 * size can be any positive non-zero value.  */

extern Bool SizeIsP2(Size size);
extern Shift SizeLog2(Size size);
extern Shift SizeFloorLog2(Size size);


/* Formatted Output -- see design.mps.writef, impl.c.mpm */

extern Res WriteF(mps_lib_FILE *stream, ...);


/* Miscellaneous support -- see impl.c.mpm */

extern size_t StringLength(const char *s);


/* Version Determination
 *
 * See design.mps.version-library.  */

extern char *MPSVersion(void);


/* Bit Table Interface -- see design.mps.bt.if.* for doc */

/* design.mps.bt.if.size */
extern size_t (BTSize)(unsigned long length);
#define BTSize(n) (((n) + MPS_WORD_WIDTH-1) / MPS_WORD_WIDTH * sizeof(Word))


/* design.mps.bt.if.get */
extern Bool (BTGet)(BT bt, Index index);
#define BTGet(a, i) \
  ((Bool)(((a)[((i) >> MPS_WORD_SHIFT)] \
           >> ((i) & ~((Word)-1 << MPS_WORD_SHIFT))) \
          & (Word)1))

/* design.mps.bt.if.set */
extern void (BTSet)(BT bt, Index index);
#define BTSet(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] |= (Word)1<<((i)&~((Word)-1<<MPS_WORD_SHIFT)); \
  END

/* design.mps.bt.if.res */
extern void (BTRes)(BT bt, Index index);
#define BTRes(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] &= \
      ~((Word)1 << ((i) & ~((Word)-1<<MPS_WORD_SHIFT))); \
  END

extern Res BTCreate(BT *btReturn, Arena arena, Count length);
extern void BTDestroy(BT bt, Arena arena, Count length);
extern void BTSetRange(BT bt, Index base, Index limit);
extern Bool BTIsSetRange(BT bt, Index base, Index limit);
extern void BTResRange(BT bt, Index base, Index limit);
extern Bool BTIsResRange(BT bt, Index base, Index limit);
extern Bool BTFindShortResRange(Index *baseReturn, Index *limitReturn,
                                BT bt, Index searchBase, Index searchLimit,
                                unsigned long length);
extern Bool BTFindShortResRangeHigh(Index *baseReturn, Index *limitReturn,
                                    BT bt, Index searchBase, Index searchLimit,
                                    unsigned long length);
extern Bool BTFindLongResRange(Index *baseReturn, Index *limitReturn,
                               BT bt, Index searchBase, Index searchLimit,
                               unsigned long length);
extern Bool BTFindLongResRangeHigh(Index *baseReturn, Index *limitReturn,
                                   BT bt, Index searchBase, Index searchLimit,
                                   unsigned long length);
extern Bool BTRangesSame(BT BTx, BT BTy, Index base, Index limit);
extern void BTCopyInvertRange(BT fromBT, BT toBT, Index base, Index limit);
extern void BTCopyRange(BT fromBT, BT toBT, Index base, Index limit);
extern void BTCopyOffsetRange(BT fromBT, BT toBT,
                              Index fromBase, Index fromLimit,
                              Index toBase, Index toLimit);


/* Pool Interface -- see impl.c.pool */

extern Res PoolInit(Pool pool, Arena arena, PoolClass class, ...);
extern Res PoolInitV(Pool pool, Arena arena, PoolClass class, va_list args);
extern void PoolFinish(Pool pool);
extern Bool PoolClassCheck(PoolClass class);
extern Bool PoolCheck(Pool pool);
extern Res PoolDescribe(Pool pool, mps_lib_FILE *stream);

#define PoolArena(pool)         ((pool)->arena)
#define PoolAlignment(pool)     ((pool)->alignment)
#define PoolSegRing(pool)       (&(pool)->segRing)

extern Bool PoolFormat(Format *formatReturn, Pool pool);

extern double PoolMutatorAllocSize(Pool pool);

extern Bool PoolOfAddr(Pool *poolReturn, Arena arena, Addr addr);
extern Bool PoolHasAddr(Pool pool, Addr addr);

extern Res PoolCreate(Pool *poolReturn, Arena arena, PoolClass class, ...);
extern Res PoolCreateV(Pool *poolReturn, Arena arena, PoolClass class,
                       va_list arg);
extern void PoolDestroy(Pool pool);
extern BufferClass PoolDefaultBufferClass(Pool pool);
extern Res PoolAlloc(Addr *pReturn, Pool pool, Size size,
                     Bool withReservoirPermit);
extern void PoolFree(Pool pool, Addr old, Size size);
extern Res PoolTraceBegin(Pool pool, Trace trace);
extern Res PoolAccess(Pool pool, Seg seg, Addr addr,
                      AccessSet mode, MutatorFaultContext context);
extern Res PoolWhiten(Pool pool, Trace trace, Seg seg);
extern void PoolGrey(Pool pool, Trace trace, Seg seg);
extern void PoolBlacken(Pool pool, TraceSet traceSet, Seg seg);
extern Res PoolScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg);
extern Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO);
#define PoolFix(pool, ss, seg, refIO) \
  ((*(pool)->fix)(pool, ss, seg, refIO))
extern void PoolFixEmergency(Pool pool, ScanState ss, Seg seg, Addr *refIO);
extern void PoolReclaim(Pool pool, Trace trace, Seg seg);
extern void PoolWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                     void *v, unsigned long s);
extern Res PoolTrivInit(Pool pool, va_list arg);
extern void PoolTrivFinish(Pool pool);
extern Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size,
                       Bool withReservoirPermit);
extern Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size,
                         Bool withReservoirPermit);
extern void PoolNoFree(Pool pool, Addr old, Size size);
extern void PoolTrivFree(Pool pool, Addr old, Size size);
extern Res PoolNoBufferFill(Addr *baseReturn, Addr *limitReturn,
                            Pool pool, Buffer buffer, Size size,
                            Bool withReservoirPermit);
extern Res PoolTrivBufferFill(Addr *baseReturn, Addr *limitReturn,
                              Pool pool, Buffer buffer, Size size,
                              Bool withReservoirPermit);
extern void PoolNoBufferEmpty(Pool pool, Buffer buffer,
                              Addr init, Addr limit);
extern void PoolTrivBufferEmpty(Pool pool, Buffer buffer,
                                Addr init, Addr limit);
extern Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream);
extern Res PoolNoTraceBegin(Pool pool, Trace trace);
extern Res PoolTrivTraceBegin(Pool pool, Trace trace);
extern Res PoolNoAccess(Pool pool, Seg seg, Addr addr,
                        AccessSet mode, MutatorFaultContext context);
extern Res PoolSegAccess(Pool pool, Seg seg, Addr addr,
                         AccessSet mode, MutatorFaultContext context);
extern Res PoolSingleAccess(Pool pool, Seg seg, Addr addr,
                            AccessSet mode, MutatorFaultContext context);
extern Res PoolNoWhiten(Pool pool, Trace trace, Seg seg);
extern Res PoolTrivWhiten(Pool pool, Trace trace, Seg seg);
extern void PoolNoGrey(Pool pool, Trace trace, Seg seg);
extern void PoolTrivGrey(Pool pool, Trace trace, Seg seg);
extern void PoolNoBlacken(Pool pool, TraceSet traceSet, Seg seg);
extern void PoolTrivBlacken(Pool pool, TraceSet traceSet, Seg seg);
extern Res PoolNoScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg);
extern Res PoolNoFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
extern void PoolNoReclaim(Pool pool, Trace trace, Seg seg);
extern void PoolNoRampBegin(Pool pool, Buffer buf, Bool collectAll);
extern void PoolTrivRampBegin(Pool pool, Buffer buf, Bool collectAll);
extern void PoolNoRampEnd(Pool pool, Buffer buf);
extern void PoolTrivRampEnd(Pool pool, Buffer buf);
extern Res PoolNoFramePush(AllocFrame *frameReturn, Pool pool, Buffer buf);
extern Res PoolTrivFramePush(AllocFrame *frameReturn, Pool pool, Buffer buf);
extern Res PoolNoFramePop(Pool pool, Buffer buf, AllocFrame frame);
extern Res PoolTrivFramePop(Pool pool, Buffer buf, AllocFrame frame);
extern void PoolNoFramePopPending(Pool pool, Buffer buf, AllocFrame frame);
extern void PoolNoWalk(Pool pool, Seg seg, FormattedObjectsStepMethod step,
		       void *p, unsigned long s);
extern PoolDebugMixin PoolNoDebugMixin(Pool pool);
extern BufferClass PoolNoBufferClass(void);

#define ClassOfPool(pool) ((pool)->class)
#define SuperclassOfPool(pool) \
  ((PoolClass)ProtocolClassSuperclassPoly((pool)->class))


/* Abstract Pool Classes Interface -- see impl.c.poolabs */
extern void PoolClassMixInAllocFree(PoolClass class);
extern void PoolClassMixInBuffer(PoolClass class);
extern void PoolClassMixInScan(PoolClass class);
extern void PoolClassMixInFormat(PoolClass class);
extern void PoolClassMixInCollect(PoolClass class);
extern AbstractPoolClass AbstractPoolClassGet(void);
extern AbstractAllocFreePoolClass AbstractAllocFreePoolClassGet(void);
extern AbstractBufferPoolClass AbstractBufferPoolClassGet(void);
extern AbstractBufferPoolClass AbstractSegBufPoolClassGet(void);
extern AbstractScanPoolClass AbstractScanPoolClassGet(void);
extern AbstractCollectPoolClass AbstractCollectPoolClassGet(void);

/* DEFINE_POOL_CLASS
 *
 * Convenience macro -- see design.mps.protocol.int.define-special. */

#define DEFINE_POOL_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, PoolClass, var)

#define POOL_SUPERCLASS(className) \
  ((PoolClass)SUPERCLASS(className))


/* Message Interface -- see design.mps.message */

extern Bool MessageCheck(Message message);
extern Bool MessageClassCheck(MessageClass class);
extern Bool MessageTypeCheck(MessageType type);
extern MessageClass MessageGetClass(Message message);
extern Arena MessageArena(Message message);
extern void MessageInit(Arena arena, Message message,
                        MessageClass class, MessageType type);
extern void MessageFinish(Message message);
extern void MessagePost(Arena arena, Message message);
extern Bool MessagePoll(Arena arena);
extern MessageType MessageGetType(Message message);
extern void MessageDiscard(Arena arena, Message message);
extern void MessageEmpty(Arena arena);
extern Bool MessageGet(Message *messageReturn, Arena arena,
                       MessageType type);
extern Bool MessageQueueType(MessageType *typeReturn, Arena arena);
extern void MessageTypeEnable(Arena arena, MessageType type);
extern void MessageTypeDisable(Arena arena, MessageType type);

/* Message methods */

/* Method dispatchers */
extern void MessageFinalizationRef(Ref *refReturn,
                                   Arena arena, Message message);
extern Size MessageGCLiveSize(Message message);
extern Size MessageGCCondemnedSize(Message message);
extern Size MessageGCNotCondemnedSize(Message message);

/* Convenience methods */
extern void MessageNoFinalizationRef(Ref *refReturn,
                                     Arena arena, Message message);
extern Size MessageNoGCLiveSize(Message message);
extern Size MessageNoGCCondemnedSize(Message message);
extern Size MessageNoGCNotCondemnedSize(Message message);


/* Trace Interface -- see impl.c.trace */

#define TraceSetSingle(trace)       BS_SINGLE(TraceSet, (trace)->ti)
#define TraceSetIsSingle(ts)        BS_IS_SINGLE(ts)
#define TraceSetIsMember(ts, trace) BS_IS_MEMBER(ts, (trace)->ti)
#define TraceSetAdd(ts, trace)      BS_ADD(TraceSet, ts, (trace)->ti)
#define TraceSetDel(ts, trace)      BS_DEL(TraceSet, ts, (trace)->ti)
#define TraceSetUnion(ts1, ts2)     BS_UNION(ts1, ts2)
#define TraceSetInter(ts1, ts2)     BS_INTER(ts1, ts2)
#define TraceSetDiff(ts1, ts2)      BS_DIFF(ts1, ts2)
#define TraceSetSuper(ts1, ts2)     BS_SUPER(ts1, ts2)
#define TraceSetSub(ts1, ts2)       BS_SUB(ts1, ts2)
#define TraceSetComp(ts)            BS_COMP(ts)

#define TRACE_SET_ITER(ti, trace, ts, arena) \
  for(ti = 0, trace = ArenaTrace(arena, ti); ti < TraceLIMIT; \
      ++ti, trace = ArenaTrace(arena, ti)) BEGIN \
    if (TraceSetIsMember(ts, trace)) {

#define TRACE_SET_ITER_END(ti, trace, ts, arena) } END


extern void ScanStateInit(ScanState ss, TraceSet ts, Arena arena,
                          Rank rank, ZoneSet white);
extern void ScanStateFinish(ScanState ss);
extern Bool ScanStateCheck(ScanState ss);
extern void ScanStateSetSummary(ScanState ss, RefSet summary);
extern RefSet ScanStateSummary(ScanState ss);

extern Bool TraceIdCheck(TraceId id);
extern Bool TraceSetCheck(TraceSet ts);
extern Bool TraceCheck(Trace trace);
extern Res TraceCreate(Trace *traceReturn, Arena arena);
extern void TraceDestroy(Trace trace);

extern Res TraceAddWhite(Trace trace, Seg seg);
extern Res TraceCondemnZones(Trace trace, ZoneSet condemnedSet);
extern void TraceStart(Trace trace, double mortality, double finishingTime);
extern Bool TracePoll(Globals globals);

extern void TraceSegAccess(Arena arena, Seg seg, AccessSet mode);
extern Res TraceFix(ScanState ss, Ref *refIO);
extern Res TraceFixEmergency(ScanState ss, Ref *refIO);


/* Collection control parameters */

extern double TraceTopGenMortality;
extern double TraceWorkFactor;


/* Equivalent to impl.h.mps MPS_SCAN_BEGIN */

#define TRACE_SCAN_BEGIN(ss) \
  BEGIN \
    Shift SCANzoneShift = (ss)->zoneShift; \
    ZoneSet SCANwhite = (ss)->white; \
    RefSet SCANsummary = (ss)->unfixedSummary; \
    Word SCANt; \
    {

/* Equivalent to impl.h.mps MPS_FIX1 */

#define TRACE_FIX1(ss, ref) \
  (SCANt = (Word)1 << ((Word)(ref) >> SCANzoneShift & (MPS_WORD_WIDTH-1)), \
   SCANsummary |= SCANt, \
   SCANwhite & SCANt)

/* Equivalent to impl.h.mps MPS_FIX2 */

#define TRACE_FIX2(ss, refIO) \
  ((*(ss)->fix)(ss, refIO))

/* Equivalent to impl.h.mps MPS_FIX */

#define TRACE_FIX(ss, refIO) \
  (TRACE_FIX1(ss, *(refIO)) ? TRACE_FIX2(ss, refIO) : ResOK)

/* Equivalent to impl.h.mps MPS_SCAN_END */

#define TRACE_SCAN_END(ss) \
    } \
    (ss)->unfixedSummary = SCANsummary; \
  END

extern Res TraceScanArea(ScanState ss, Addr *base, Addr *limit);
extern Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit);
extern Res TraceScanAreaMasked(ScanState ss,
                               Addr *base, Addr *limit, Word mask);
extern void TraceScanSingleRef(TraceSet ts, Rank rank, Arena arena,
                               Seg seg, Ref *refIO);


/* Arena Interface -- see impl.c.arena */

/* DEFINE_ARENA_CLASS
 *
 * Convenience macro -- see design.mps.protocol.int.define-special. */

#define DEFINE_ARENA_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, ArenaClass, var)

#define ARENA_SUPERCLASS(className) \
  ((ArenaClass)SUPERCLASS(className))

extern AbstractArenaClass AbstractArenaClassGet(void);
extern Bool ArenaClassCheck(ArenaClass class);

extern Bool ArenaCheck(Arena arena);
extern Res ArenaCreateV(Arena *arenaReturn, ArenaClass class, va_list args);
extern void ArenaDestroy(Arena arena);
extern Res ArenaInit(Arena arena, ArenaClass class);
extern void ArenaFinish(Arena arena);
extern Res ArenaDescribe(Arena arena, mps_lib_FILE *stream);
extern Res ArenaDescribeTracts(Arena arena, mps_lib_FILE *stream);
extern Bool ArenaAccess(Addr addr, AccessSet mode, MutatorFaultContext context);

extern Bool GlobalsCheck(Globals arena);
extern Res GlobalsInit(Globals arena);
extern void GlobalsFinish(Globals arena);
extern Res GlobalsCompleteCreate(Globals arenaGlobals);
extern void GlobalsPrepareToDestroy(Globals arenaGlobals);
extern Res GlobalsDescribe(Globals arena, mps_lib_FILE *stream);

#define ArenaGlobals(arena) (&(arena)->globals)
#define GlobalsArena(glob) PARENT(ArenaStruct, globals, glob)

#define ArenaRootRing(arena)    (&(arena)->rootRing)
#define ArenaTraceRing(arena)   (&(arena)->traceRing)
#define ArenaThreadRing(arena)  (&(arena)->threadRing)
#define ArenaEpoch(arena)       ((arena)->epoch) /* .epoch.ts */
#define ArenaTrace(arena, ti)   (&(arena)->trace[ti])
#define ArenaZoneShift(arena)   ((arena)->zoneShift)
#define ArenaAlign(arena)       ((arena)->alignment)
#define ArenaGreyRing(arena, rank) (&(arena)->greyRing[rank])


extern void (ArenaEnter)(Arena arena);
extern void (ArenaLeave)(Arena arena);

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
#define ArenaEnter(arena)  UNUSED(arena)
#define ArenaLeave(arena)  UNUSED(arena)
#endif


extern void (ArenaPoll)(Globals globals);
#ifdef MPS_PROD_EPCORE
#define ArenaPoll(globals)  UNUSED(globals)
#endif
/* .nogc.why: ScriptWorks doesn't use MM-provided incremental GC, so */
/* doesn't need to poll when allocating. */

extern Bool (ArenaStep)(Globals globals, double interval);
extern void ArenaClamp(Globals globals);
extern void ArenaRelease(Globals globals);
extern void ArenaPark(Globals globals);
extern Res ArenaCollect(Globals globals);
extern Bool ArenaHasAddr(Arena arena, Addr addr);

extern Res ControlInit(Arena arena);
extern void ControlFinish(Arena arena);
extern Res ControlAlloc(void **baseReturn, Arena arena, size_t size,
                        Bool withReservoirPermit);
extern void ControlFree(Arena arena, void *base, size_t size);


/* Peek/Poke
 *
 * These are provided so that modules in the MPS can make occasional
 * access to client data.  They perform the appropriate shield and
 * summary manipulations that are necessary.
 *
 * Note that Peek and Poke can be called with address that may or
 * may not be in arena managed memory.  */

/* Peek reads a value */
extern Ref ArenaPeek(Arena arena, Addr addr);
/* Same, but addr must be in seg */
extern Ref ArenaPeekSeg(Arena arena, Seg seg, Addr addr);
/* Poke stores a value */
extern void ArenaPoke(Arena arena, Addr addr, Ref ref);
/* Same, but addr must be in seg */
extern void ArenaPokeSeg(Arena arena, Seg seg, Addr addr, Ref ref);


/* Read/Write
 *
 * These simulate mutator reads and writes to locations.
 * They are effectively a software barrier, and maintain the tricolor
 * invariant (hence performing any scanning or color manipulation
 * necessary).
 *
 * Only Read provided right now.  */

Ref ArenaRead(Arena arena, Addr addr);


extern Size ArenaReserved(Arena arena);
extern Size ArenaCommitted(Arena arena);
extern Size ArenaSpareCommitted(Arena arena);

extern Size ArenaCommitLimit(Arena arena);
extern Res ArenaSetCommitLimit(Arena arena, Size limit);
extern Size ArenaSpareCommitLimit(Arena arena);
extern void ArenaSetSpareCommitLimit(Arena arena, Size limit);
extern void ArenaNoSpareCommitExceeded(Arena arena);

extern double ArenaMutatorAllocSize(Arena arena);
extern Size ArenaAvail(Arena arena);

extern Res ArenaExtend(Arena, Addr base, Size size);

extern Res ArenaFinalize(Arena arena, Ref obj);

extern Bool ArenaIsReservedAddr(Arena arena, Addr addr);

#define ArenaReservoir(arena) (&(arena)->reservoirStruct)

extern Bool ReservoirCheck(Reservoir reservoir);
extern Res ReservoirInit(Reservoir reservoir, Arena arena);
extern void ReservoirFinish (Reservoir reservoir);
extern Size ReservoirLimit(Reservoir reservoir);
extern void ReservoirSetLimit(Reservoir reservoir, Size size);
extern Size ReservoirAvailable(Reservoir reservoir);
extern Res ReservoirEnsureFull(Reservoir reservoir);
extern void ReservoirDeposit(Reservoir reservoir, Addr base, Size size);
extern Res ReservoirWithdraw(Addr *baseReturn, Tract *baseTractReturn,
                             Reservoir reservoir, Size size, Pool pool);

extern Res ArenaAlloc(Addr *baseReturn, SegPref pref,
                      Size size, Pool pool, Bool withReservoirPermit);
extern void ArenaFree(Addr base, Size size, Pool pool);

extern Res ArenaNoExtend(Arena arena, Addr base, Size size);


/* Locus interface */

extern Bool SegPrefCheck(SegPref pref);
extern SegPref SegPrefDefault(void);
extern Res SegPrefExpress(SegPref pref, SegPrefKind kind, void *p);

extern void LocusInit(Arena arena);
extern void LocusFinish(Arena arena);
extern Bool LocusCheck(Arena arena);


/* Segment interface */

extern Res SegAlloc(Seg *segReturn, SegClass class, SegPref pref,
                    Size size, Pool pool, Bool withReservoirPermit, ...);
extern void SegFree(Seg seg);
extern Bool SegOfAddr(Seg *segReturn, Arena arena, Addr addr);
extern Bool SegFirst(Seg *segReturn, Arena arena);
extern Bool SegNext(Seg *segReturn, Arena arena, Addr addr);
extern void SegSetWhite(Seg seg, TraceSet white);
extern void SegSetGrey(Seg seg, TraceSet grey);
extern void SegSetRankSet(Seg seg, RankSet rankSet);
extern void SegSetRankAndSummary(Seg seg, RankSet rankSet, RefSet summary);
extern Res SegMerge(Seg *mergedSegReturn, Seg segLo, Seg segHi,
                    Bool withReservoirPermit, ...);
extern Res SegSplit(Seg *segLoReturn, Seg *segHiReturn, Seg seg, Addr at,
                    Bool withReservoirPermit, ...);
extern Res SegDescribe(Seg seg, mps_lib_FILE *stream);
extern void SegSetSummary(Seg seg, RefSet summary);
extern Buffer SegBuffer(Seg seg);
extern void SegSetBuffer(Seg seg, Buffer buffer);
extern Bool SegCheck(Seg seg);
extern Bool GCSegCheck(GCSeg gcseg);
extern Bool SegClassCheck(SegClass class);
extern SegClass SegClassGet(void);
extern SegClass GCSegClassGet(void);
extern void SegClassMixInNoSplitMerge(SegClass class);


/* DEFINE_SEG_CLASS -- define a segment class */

#define DEFINE_SEG_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, SegClass, var)


#define SEG_SUPERCLASS(className) \
  ((SegClass)SUPERCLASS(className))

extern Size SegSize(Seg seg);
extern Addr (SegBase)(Seg seg);
extern Addr (SegLimit)(Seg seg);
#define SegBase(seg)            (TractBase((seg)->firstTract))
#define SegLimit(seg)           ((seg)->limit)
#define SegPool(seg)            (TractPool((seg)->firstTract))
/* .bitfield.promote: The bit field accesses need to be cast to the */
/* right type, otherwise they'll be promoted to signed int, see */
/* standard.ansic.6.2.1.1. */
#define SegRankSet(seg)         ((RankSet)(seg)->rankSet)
#define SegPM(seg)              ((AccessSet)(seg)->pm)
#define SegSM(seg)              ((AccessSet)(seg)->sm)
#define SegDepth(seg)           ((unsigned)(seg)->depth)
#define SegGrey(seg)            ((TraceSet)(seg)->grey)
#define SegWhite(seg)           ((TraceSet)(seg)->white)
#define SegNailed(seg)          ((TraceSet)(seg)->nailed)
#define SegOfPoolRing(node)     (RING_ELT(Seg, poolRing, (node)))
#define SegOfGreyRing(node)     (&(RING_ELT(GCSeg, greyRing, (node)) \
                                   ->segStruct))

#define SegSummary(seg)         (((GCSeg)(seg))->summary)

#define SegSetPM(seg, mode)     ((void)((seg)->pm = (mode)))
#define SegSetSM(seg, mode)     ((void)((seg)->sm = (mode)))
#define SegSetDepth(seg, d)     ((void)((seg)->depth = (d)))
#define SegSetNailed(seg, ts)   ((void)((seg)->nailed = (ts)))


/* Buffer Interface -- see impl.c.buffer */

extern Res BufferCreate(Buffer *bufferReturn, BufferClass class,
                        Pool pool, Bool isMutator, ...);
extern Res BufferCreateV(Buffer *bufferReturn, BufferClass class,
                         Pool pool, Bool isMutator, va_list args);
extern void BufferDestroy(Buffer buffer);
extern Bool BufferCheck(Buffer buffer);
extern Bool SegBufCheck(SegBuf segbuf);
extern Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream);
extern Res BufferReserve(Addr *pReturn, Buffer buffer, Size size,
                         Bool withReservoirPermit);
/* macro equivalent for BufferReserve, keep in sync with impl.c.buffer */
#define BUFFER_RESERVE(pReturn, buffer, size, withReservoirPermit) \
  (AddrAdd(BufferAlloc(buffer), size) > BufferAlloc(buffer) && \
   AddrAdd(BufferAlloc(buffer), size) <= BufferAP(buffer)->limit ? \
     (*(pReturn) = BufferAlloc(buffer), \
      BufferAP(buffer)->alloc = AddrAdd(BufferAlloc(buffer), size), \
      ResOK) : \
   BufferFill(pReturn, buffer, size, withReservoirPermit))

extern Res BufferFill(Addr *pReturn, Buffer buffer, Size size,
                      Bool withReservoirPermit);

extern Bool BufferCommit(Buffer buffer, Addr p, Size size);
/* macro equivalent for BufferCommit, keep in sync with impl.c.buffer */
#define BUFFER_COMMIT(buffer, p, size) \
  (BufferAP(buffer)->init = BufferAlloc(buffer), \
   BufferAP(buffer)->limit != 0 || BufferTrip(buffer, p, size))

extern Bool BufferTrip(Buffer buffer, Addr p, Size size);
extern void BufferFinish(Buffer buffer);
extern Bool BufferIsReset(Buffer buffer);
extern Bool BufferIsReady(Buffer buffer);
extern Bool BufferIsMutator(Buffer buffer);
extern void BufferSetAllocAddr(Buffer buffer, Addr addr);
extern void BufferAttach(Buffer buffer,
                         Addr base, Addr limit, Addr init, Size size);
extern void BufferDetach(Buffer buffer, Pool pool);
extern void BufferFlip(Buffer buffer);

extern AP (BufferAP)(Buffer buffer);
#define BufferAP(buffer)        (&(buffer)->apStruct)
extern Buffer BufferOfAP(AP ap);
#define BufferOfAP(ap)          PARENT(BufferStruct, apStruct, ap)

#define BufferArena(buffer) ((buffer)->arena)
#define BufferPool(buffer)  ((buffer)->pool)

extern Seg BufferSeg(Buffer buffer);

extern RankSet BufferRankSet(Buffer buffer);
extern void BufferSetRankSet(Buffer buffer, RankSet rankset);

#define BufferBase(buffer)      ((buffer)->base)
#define BufferGetInit(buffer) /* see .trans.bufferinit */ \
  (BufferAP(buffer)->init)
#define BufferAlloc(buffer)     (BufferAP(buffer)->alloc)
#define BufferLimit(buffer)     ((buffer)->poolLimit)
extern Addr BufferScanLimit(Buffer buffer);

extern void BufferReassignSeg(Buffer buffer, Seg seg);

extern Bool BufferIsTrapped(Buffer buffer);
extern Bool BufferIsTrappedByMutator(Buffer buffer);

extern void BufferRampBegin(Buffer buffer, AllocPattern pattern);
extern Res BufferRampEnd(Buffer buffer);
extern void BufferRampReset(Buffer buffer);

extern Res BufferFramePush(AllocFrame *frameReturn, Buffer buffer);
extern Res BufferFramePop(Buffer buffer, AllocFrame frame);
extern FrameState BufferFrameState(Buffer buffer);
extern void BufferFrameSetState(Buffer buffer, FrameState state);


/* DEFINE_BUFFER_CLASS -- define a buffer class */

#define DEFINE_BUFFER_CLASS(className, var) \
  DEFINE_ALIAS_CLASS(className, BufferClass, var)

#define BUFFER_SUPERCLASS(className) \
  ((BufferClass)SUPERCLASS(className))

extern Bool BufferClassCheck(BufferClass class);
extern BufferClass BufferClassGet(void);
extern BufferClass SegBufClassGet(void);
extern BufferClass RankBufClassGet(void);

extern AllocPattern AllocPatternRamp(void);
extern AllocPattern AllocPatternRampCollectAll(void);


/* Format Interface -- see impl.c.format */

extern Bool FormatCheck(Format format);
extern Res FormatCreate(Format *formatReturn, Arena arena,
                        Align alignment,
			FormatVariety variety,
                        FormatScanMethod scan,
                        FormatSkipMethod skip,
                        FormatMoveMethod move,
                        FormatIsMovedMethod isMoved,
                        FormatCopyMethod copy,
                        FormatPadMethod pad,
			FormatClassMethod class,
                        Size headerSize);
extern void FormatDestroy(Format format);
extern Arena FormatArena(Format format);
extern Res FormatDescribe(Format format, mps_lib_FILE *stream);


/* Reference Interface -- see impl.c.ref */

extern Bool RankCheck(Rank rank);
extern Bool RankSetCheck(RankSet rankSet);

#define RankSetIsMember(rs, r)  BS_IS_MEMBER((rs), (r))
#define RankSetSingle(r)        BS_SINGLE(RankSet, (r))
#define RankSetIsSingle(r)      BS_IS_SINGLE(r)
#define RankSetUnion(rs1, rs2)  BS_UNION((rs1), (rs2))
#define RankSetDel(rs, r)       BS_DEL(RankSet, (rs), (r))

#define AddrZone(arena, addr) \
  (((Word)(addr) >> (arena)->zoneShift) & (MPS_WORD_WIDTH - 1))

#define RefSetUnion(rs1, rs2)   BS_UNION((rs1), (rs2))
#define RefSetInter(rs1, rs2)   BS_INTER((rs1), (rs2))
#define RefSetDiff(rs1, rs2)    BS_DIFF((rs1), (rs2))
#define RefSetAdd(arena, rs, addr) \
  BS_ADD(RefSet, rs, AddrZone(arena, addr))
#define RefSetIsMember(arena, rs, addr) \
  BS_IS_MEMBER(rs, AddrZone(arena, addr))
#define RefSetSuper(rs1, rs2)   BS_SUPER((rs1), (rs2))
#define RefSetSub(rs1, rs2)     BS_SUB((rs1), (rs2))


/* Zone sets -- see design.mps.refset */

#define ZoneSetUnion(zs1, zs2) BS_UNION(zs1, zs2)
#define ZoneSetInter(zs1, zs2) BS_INTER(zs1, zs2)
#define ZoneSetDiff(zs1, zs2)  BS_DIFF(zs1, zs2)
#define ZoneSetAdd(arena, zs, addr) \
  BS_ADD(ZoneSet, zs, AddrZone(arena, addr))
#define ZoneSetIsMember(arena, zs, addr) \
  BS_IS_MEMBER(zs, AddrZone(arena, addr))
#define ZoneSetSub(zs1, zs2)   BS_SUB(zs1, zs2)
#define ZoneSetSuper(zs1, zs2) BS_SUPER(zs1, zs2)
#define ZoneSetComp(zs)        BS_COMP(zs)

extern ZoneSet ZoneSetOfRange(Arena arena, Addr base, Addr limit);
extern ZoneSet ZoneSetOfSeg(Arena arena, Seg seg);


/* Shield Interface -- see impl.c.shield */

extern void (ShieldRaise)(Arena arena, Seg seg, AccessSet mode);
extern void (ShieldLower)(Arena arena, Seg seg, AccessSet mode);
extern void (ShieldEnter)(Arena arena);
extern void (ShieldLeave)(Arena arena);
extern void (ShieldExpose)(Arena arena, Seg seg);
extern void (ShieldCover)(Arena arena, Seg seg);
extern void (ShieldSuspend)(Arena arena);
extern void (ShieldResume)(Arena arena);
extern void (ShieldFlush)(Arena arena);

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
#define ShieldRaise(arena, seg, mode) \
  BEGIN UNUSED(arena); UNUSED(seg); UNUSED(mode); END
#define ShieldLower(arena, seg, mode) \
  BEGIN UNUSED(arena); UNUSED(seg); UNUSED(mode); END
#define ShieldEnter(arena) BEGIN UNUSED(arena); END
#define ShieldLeave(arena) BEGIN UNUSED(arena); END
#define ShieldExpose(arena, seg)  \
  BEGIN UNUSED(arena); UNUSED(seg); END
#define ShieldCover(arena, seg) \
  BEGIN UNUSED(arena); UNUSED(seg); END
#define ShieldSuspend(arena) BEGIN UNUSED(arena); END
#define ShieldResume(arena) BEGIN UNUSED(arena); END
#define ShieldFlush(arena) BEGIN UNUSED(arena); END
#endif


/* Protection Interface
 *
 * See design.mps.prot for the design of the generic interface including
 * the contracts for these functions.
 *
 * This interface has several different implementations, typically one
 * per platform, see impl.c.prot* for the various implementations, and
 * design.mps.prot* for the corresponding designs. */

extern void ProtSetup(void);

extern void ProtSet(Addr base, Addr limit, AccessSet mode);
extern void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
                      void *p, size_t s);
extern void ProtSync(Arena arena);
extern Bool ProtCanStepInstruction(MutatorFaultContext context);
extern Res ProtStepInstruction(MutatorFaultContext context);


/* Location Dependency -- see impl.c.ld */

extern void LDReset(LD ld, Arena arena);
extern void LDAdd(LD ld, Arena arena, Addr addr);
extern Bool LDIsStale(LD ld, Arena arena, Addr addr);
extern void LDAge(Arena arena, RefSet moved);
extern void LDMerge(LD ld, Arena arena, LD from);


/* Root Interface -- see impl.c.root */

extern Res RootCreateTable(Root *rootReturn, Arena arena,
                           Rank rank, RootMode mode,
                           Addr *base, Addr *limit);
extern Res RootCreateTableMasked(Root *rootReturn, Arena arena,
                                 Rank rank, RootMode mode,
                                 Addr *base, Addr *limit,
                                 Word mask);
extern Res RootCreateReg(Root *rootReturn, Arena arena,
                           Rank rank, Thread thread,
                           RootScanRegMethod scan,
                           void *p, size_t s);
extern Res RootCreateFmt(Root *rootReturn, Arena arena,
                           Rank rank, RootMode mode,
                           FormatScanMethod scan,
                           Addr base, Addr limit);
extern Res RootCreateFun(Root *rootReturn, Arena arena,
                        Rank rank, RootScanMethod scan,
                        void *p, size_t s);
extern void RootDestroy(Root root);
extern Bool RootModeCheck(RootMode mode);
extern Bool RootCheck(Root root);
extern Res RootDescribe(Root root, mps_lib_FILE *stream);
extern Res RootsDescribe(Globals arenaGlobals, mps_lib_FILE *stream);
extern Rank RootRank(Root root);
extern AccessSet RootPM(Root root);
extern RefSet RootSummary(Root root);
extern void RootGrey(Root root, Trace trace);
extern Res RootScan(ScanState ss, Root root);
extern Arena RootArena(Root root);
extern Bool RootOfAddr(Root *root, Arena arena, Addr addr);
extern void RootAccess(Root root, AccessSet mode);
typedef Res (*RootIterateFn)(Root root, void *p);
extern Res RootsIterate(Globals arena, RootIterateFn f, void *p);


/* VM Interface -- see impl.c.vm* */

extern Align VMAlign(VM vm);
extern Bool VMCheck(VM vm);
extern Res VMCreate(VM *VMReturn, Size size);
extern void VMDestroy(VM vm);
extern Addr VMBase(VM vm);
extern Addr VMLimit(VM vm);
extern Res VMMap(VM vm, Addr base, Addr limit);
extern void VMUnmap(VM vm, Addr base, Addr limit);
extern Size VMReserved(VM vm);
extern Size VMMapped(VM vm);


/* Stack Probe */

extern void StackProbe(Size depth);


/* STATISTIC -- gather diagnostics (in some varieties)
 *
 * The argument of STATISTIC is an expression; the expansion followed by
 * a semicolon is syntactically a statement.
 *
 * The argument of STATISTIC_STAT is a statement; the expansion followed by
 * a semicolon is syntactically a statement.
 *
 * STATISTIC_WRITE is inserted in WriteF arguments to output the values
 * of diagnostic fields.
 *
 * .statistic.whitehot: The implementation of STATISTIC for
 * non-statistical varieties passes the parameter to DISCARD to ensure
 * the parameter is syntactically an expression.  The parameter is
 * passed as part of a comma-expression so that its type is not
 * important.  This permits an expression of type void.  */

#if defined(DIAGNOSTICS)

#define STATISTIC(gather) BEGIN (gather); END
#define STATISTIC_STAT(gather) BEGIN gather; END
#define STATISTIC_WRITE(format, arg) (format), (arg),

#elif defined(DIAGNOSTICS_NONE)

#define STATISTIC(gather) DISCARD(((gather), 0))
#define STATISTIC_STAT(gather) DISCARD_STAT(gather)
#define STATISTIC_WRITE(format, arg)

#else

#error "No diagnostics configured."

#endif


#endif /* mpm_h */


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
