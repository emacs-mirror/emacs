/* impl.h.mpm: MEMORY POOL MANAGER DEFINITIONS
 *
 * $HopeName: MMsrc!mpm.h(trunk.33) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 */

#ifndef mpm_h
#define mpm_h

#include "config.h"     /* this must come first: it defines target options */
#include "misc.h"       /* miscellaneous non-specific bits and bobs */
#include "check.h"      /* assertion and consistency checking support */

#include "mpmtypes.h"
#include "mpmst.h"
#include "event.h"
#include "lock.h"
#include "th.h"
#include "poolmv.h"
#include "poolmfs.h"
#include "ss.h"
#include "mpslib.h"


/* AVER, AVERT -- MPM assertions
 *
 * AVER and AVERT are used to assert conditions within the MPM (as
 * opposed to in the MPS Interface layer, impl.c.mpsi).  This allows
 * control over internal and interface checking.
 */

#ifdef ASSERT_MPM               /* design.mps.config, impl.h.config */
#define AVER                    ASSERT
#define AVERT(type, val)        ASSERT(type ## Check(val))
#else
#define AVER(cond)              NOCHECK(cond)
#define AVERT(type, val)        NOCHECK(type ## Check(val))
#endif


/* MPMCheck -- check MPM assumptions */

extern Bool MPMCheck(void);


/* Miscellaneous Checks -- see impl.c.mpm */

extern Bool BoolCheck(Bool b);
extern Bool FunCheck(Fun f);
extern Bool AttrCheck(Attr attr);
extern Bool RootVarCheck(RootVar rootVar);
#define FUNCHECK(f)     (FunCheck((Fun)f))


/* Address/Size Interface -- see impl.c.mpm */

extern Bool (WordIsAligned)(Word word, Align align);
#define WordIsAligned(w, a)     (((w) & ((a) - 1)) == 0)

extern Word (WordAlignUp)(Word word, Align align);
#define WordAlignUp(w, a)       (((w) + (a) - 1) & ~((Word)(a) - 1))

extern Word (WordAlignDown)(Word word, Align align);
#define WordAlignDown(w, a)     ((w) & ~((Word)(a) - 1))

extern Bool AlignCheck(Align align);

extern Pointer (PointerAdd)(Pointer p, Size s);
#define PointerAdd(p, s)        ((Pointer)((char *)(p) + (s)))

extern Pointer (PointerSub)(Pointer p, Size s);
#define PointerSub(p, s)        ((Pointer)((char *)(p) - (s)))

extern Size (PointerOffset)(Pointer base, Pointer limit);
#define PointerOffset(base, limit) \
                                ((Size)((char *)(limit) - (char *)(base)))

extern Addr (AddrAdd)(Addr addr, Size size);
#define AddrAdd(p, s)           ((Addr)PointerAdd((Pointer)p, s))

extern Addr (AddrSub)(Addr addr, Size size);
#define AddrSub(p, s)           ((Addr)PointerSub((Pointer)p, s))

extern Size (AddrOffset)(Addr base, Addr limit);
#define AddrOffset(b, l)        (PointerOffset((Pointer)b, (Pointer)l))

extern Addr (AddrAlignDown)(Addr addr, Align align);
#define AddrAlignDown(p, a)	((Addr)WordAlignDown((Word)p, a))


/* Logs and Powers
 * 
 * SizeIsP2 returns TRUE if and only if size is a non-negative integer
 * power of 2, and FALSE otherwise.
 * 
 * SizeLog2 returns the logarithm in base 2 of size.  size must be a
 * power of 2.
 * 
 * SizeFloorLog2 returns the floor of the logarithm in base 2 of size.
 * size can be any positive non-zero value.
 */
extern Bool SizeIsP2(Size size);
extern Shift SizeLog2(Size size);
extern Shift SizeFloorLog2(Size size);

#define AddrWord(a)             ((Word)(a))
#define SizeWord(s)             ((Word)(s))
#define AddrIsAligned(p, a)     WordIsAligned(AddrWord(p), a)
#define AddrAlignUp(p, a)       ((Addr)WordAlignUp(AddrWord(p), a))
#define SizeIsAligned(s, a)     WordIsAligned(SizeWord(s), a)
#define SizeAlignUp(s, a)       ((Size)WordAlignUp(SizeWord(s), a))


/* Formatted Output -- see design.mps.writef, impl.c.mpm */

extern Res WriteF(mps_lib_FILE *stream, ...);


/* Ring Interface -- see design.mps.ring, impl.c.ring */

extern Bool RingCheck(Ring ring);
extern Bool RingCheckSingle(Ring ring);
extern Bool RingIsSingle(Ring ring);

/* .ring.init: */
extern void (RingInit)(Ring ring);
#define RingInit(ring) \
  BEGIN \
    Ring _ring = (ring); \
    AVER(NULL != _ring); \
    _ring->next = _ring; \
    _ring->prev = _ring; \
    AVER(RingCheck(_ring)); \
  END

/* .ring.finish: */
extern void (RingFinish)(Ring ring);
#define RingFinish(ring) \
  BEGIN \
    Ring _ring = (ring); \
    AVER(RingCheckSingle(_ring)); \
    _ring->next = RingNONE; \
    _ring->prev = RingNONE; \
  END

/* .ring.append: */
extern void (RingAppend)(Ring ring, Ring new);
#define RingAppend(ring, new) \
  BEGIN \
    Ring _ring = (ring), _new = (new); \
    AVER(RingCheck(_ring)); \
    AVER(RingCheckSingle(_new)); \
    _new->prev = _ring->prev; \
    _new->next = _ring; \
    _ring->prev->next = _new; \
    _ring->prev = _new; \
  END

/* .ring.insert: */
extern void (RingInsert)(Ring ring, Ring new);
#define RingInsert(ring, new) \
  BEGIN \
    Ring _ring = (ring), _new = (new); \
    AVER(RingCheck(_ring)); \
    AVER(RingCheckSingle(_new)); \
    _new->prev = _ring; \
    _new->next = _ring->next; \
    _ring->next->prev = _new; \
    _ring->next = _new; \
  END

/* .ring.remove: */
extern void (RingRemove)(Ring old);
#define RingRemove(old) \
  BEGIN \
    Ring _old = (old); \
    AVER(RingCheck(_old)); \
    AVER(!RingIsSingle(_old)); \
    _old->next->prev = _old->prev; \
    _old->prev->next = _old->next; \
    _old->next = _old; \
    _old->prev = _old; \
  END

/* .ring.next: */
extern Ring (RingNext)(Ring ring);
#define RingNext(ring)  ((ring)->next)

/* .ring.elt: */
#define RING_ELT(type, field, node) \
   ((type)((char *)(node) - (size_t)(&((type)0)->field)))

/* .ring.for */
#define RING_FOR(var, ring) \
  for(var = RingNext(ring); \
      var != (ring); \
      var = RingNext(var))


/* Bit Table Interface -- see design.mps.bt.if.* for the interface doc */

/* design.mps.bt.if.size */
extern Size BTSize(unsigned long length);

/* design.mps.bt.if.get */
extern int (BTGet)(BT bt, Index index);
#define BTGet(a, i) ((a)[((i)>>MPS_WORD_SHIFT)] >> \
                     ((i)&~((Word)-1<<MPS_WORD_SHIFT)) & (Word)1)

/* design.mps.bt.if.set */
extern void (BTSet)(BT bt, Index index);
#define BTSet(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] |= \
    (Word)1<<((i)&~((Word)-1<<MPS_WORD_SHIFT)); \
  END

/* design.mps.bt.if.res */
extern void (BTRes)(BT bt, Index index);
#define BTRes(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] &= \
    ~((Word)1<<((i)&~((Word)-1<<MPS_WORD_SHIFT))); \
  END

extern void BTSetRange(BT bt, Index base, Index limit);
extern void BTResRange(BT bt, Index base, Index limit);
extern Bool BTFindResRange(Index *baseReturn, Index *limitReturn,
                           BT bt,
                           Index searchBase, Index searchLimit,
                           unsigned long length);


/* Pool Interface -- see impl.c.pool */

extern Res PoolInit(Pool pool, Space space,
                    PoolClass class, ...);
extern Res PoolInitV(Pool pool, Space space, 
                     PoolClass class, va_list args);
extern void PoolFinish(Pool pool);
extern Bool PoolClassCheck(PoolClass class);
extern Bool PoolCheck(Pool pool);
extern Res PoolDescribe(Pool pool, mps_lib_FILE *stream);

extern Space (PoolSpace)(Pool pool);
#define PoolSpace(pool)         ((pool)->space)

extern Align (PoolAlignment)(Pool pool);
#define PoolAlignment(pool)     ((pool)->alignment)

extern Ring (PoolSegRing)(Pool pool);
#define PoolSegRing(pool)       (&(pool)->segRing)

extern Res PoolSegAlloc(Seg *segReturn, SegPref pref, Pool pool, Size size);
extern void PoolSegFree(Pool pool, Seg seg);
extern Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr);
extern Bool PoolHasAddr(Pool pool, Addr addr);

extern Res PoolCreate(Pool *poolReturn, Space space, 
                      PoolClass class, ...);
extern Res PoolCreateV(Pool *poolReturn, Space space,
                       PoolClass class, va_list arg);
extern void PoolDestroy(Pool pool);
extern Res PoolAlloc(Addr *pReturn, Pool pool, Size size);
extern void PoolFree(Pool pool, Addr old, Size size);
extern Res PoolTraceBegin(Pool pool, Trace trace, Action action);
extern Res PoolCondemn(Pool pool, Trace trace, Seg seg, Action action);
extern void PoolGrey(Pool pool, Trace trace, Seg seg);
extern Res PoolScan(ScanState ss, Pool pool, Seg seg);
extern Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO);
#define PoolFix(pool, ss, seg, refIO) \
  ((*(pool)->class->fix)(pool, ss, seg, refIO))
extern void PoolReclaim(Pool pool, Trace trace, Seg seg);
extern void PoolTraceEnd(Pool pool, Trace trace, Action action);
extern double PoolBenefit(Pool pool, Action action);

extern void PoolTrivFinish(Pool pool);
extern Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size);
extern Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size);
extern void PoolNoFree(Pool pool, Addr old, Size size);
extern void PoolTrivFree(Pool pool, Addr old, Size size);
extern Res PoolNoBufferInit(Pool pool, Buffer buf);
extern Res PoolTrivBufferInit(Pool pool, Buffer buf);
extern void PoolNoBufferFinish(Pool pool, Buffer buf);
extern void PoolTrivBufferFinish(Pool pool, Buffer buf);
extern Res PoolNoBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                            Pool pool, Buffer buffer, Size size);
extern Res PoolTrivBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                              Pool pool, Buffer buffer, Size size);
extern void PoolNoBufferEmpty(Pool pool, Buffer buffer);
extern void PoolTrivBufferEmpty(Pool pool, Buffer buffer);
extern Res PoolNoDescribe(Pool pool, mps_lib_FILE *stream);
extern Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream);
extern Res PoolNoTraceBegin(Pool pool, Trace trace, Action action);
extern Res PoolTrivTraceBegin(Pool pool, Trace trace, Action action);
extern Res PoolNoCondemn(Pool pool, Trace trace, Seg seg, Action action);
extern void PoolNoGrey(Pool pool, Trace trace, Seg seg);
extern void PoolTrivGrey(Pool pool, Trace trace, Seg seg);
extern Res PoolNoScan(ScanState ss, Pool pool, Seg seg);
extern Res PoolNoFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
extern void PoolNoReclaim(Pool pool, Trace trace, Seg seg);
extern void PoolNoTraceEnd(Pool pool, Trace trace, Action action);
extern void PoolTrivTraceEnd(Pool pool, Trace trace, Action action);
extern double PoolNoBenefit(Pool pool, Action action);


/* Trace Interface -- see impl.c.trace */

#define TraceSetSingle(ti)	BS_SINGLE(TraceSet, ti)
#define TraceSetIsMember(ts, ti)BS_IS_MEMBER(ts, ti)
#define TraceSetAdd(ts, ti)     BS_ADD(TraceSet, ts, ti)
#define TraceSetDel(ts, ti)     BS_DEL(TraceSet, ts, ti)
#define TraceSetUnion(ts1, ts2) BS_UNION(ts1, ts2)
#define TraceSetInter(ts1, ts2)	BS_INTER(ts1, ts2)
#define TraceSetDiff(ts1, ts2)	BS_DIFF(ts1, ts2)
#define TraceSetSuper(ts1, ts2)	BS_SUPER(ts1, ts2)

extern TraceSet (TraceSetAdd)(TraceSet ts, TraceId id);
extern TraceSet (TraceSetDel)(TraceSet ts, TraceId id);
extern TraceSet (TraceSetUnion)(TraceSet ts1, TraceSet ts2);
extern Bool (TraceSetIsMember)(TraceSet ts, TraceId id);

extern Bool ScanStateCheck(ScanState ss);
extern Bool TraceIdCheck(TraceId id);
extern Bool TraceSetCheck(TraceSet ts);
extern Bool TraceCheck(Trace trace);

extern Res TraceCreate(Trace *traceReturn, Space space, Action action);
extern void TraceDestroy(Trace trace);
extern Res TracePoll(Trace trace);
extern void TraceAccess(Space space, Seg seg, AccessSet mode);

extern Res TraceFix(ScanState ss, Ref *refIO);
extern void TraceSegGreyen(Space space, Seg seg, TraceSet ts);
extern void TraceSetSummary(Space space, Seg seg, RefSet summary);
extern Size TraceGreyEstimate(Space space, RefSet refSet);

/* Equivalent to impl.h.mps MPS_SCAN_BEGIN */

#define TRACE_SCAN_BEGIN(ss) \
  BEGIN \
    Shift SCANzoneShift = (ss)->zoneShift; \
    RefSet SCANwhite = (ss)->white; \
    RefSet SCANsummary = (ss)->summary; \
    Word SCANt; \
    {

/* Equivalent to impl.h.mps MPS_FIX1 */

#define TRACE_FIX1(ss, ref) \
  (SCANt = (Word)1<<((Word)(ref)>>SCANzoneShift&(MPS_WORD_WIDTH-1)), \
   SCANsummary |= SCANt, \
   SCANwhite & SCANt)

/* Equivalent to impl.h.mps MPS_FIX2 */

#define TRACE_FIX2(ss, refIO) \
  ((*(ss)->fix)((ss), (refIO)))

/* Equivalent to impl.h.mps MPS_FIX */

#define TRACE_FIX(ss, refIO) \
  (TRACE_FIX1((ss), *(refIO)) ? \
   TRACE_FIX2((ss), (refIO)) : ResOK)

/* Equivalent to impl.h.mps MPS_SCAN_END */

#define TRACE_SCAN_END(ss) \
   } \
   (ss)->summary = SCANsummary; \
  END

extern Res TraceScanArea(ScanState ss, Addr *base, Addr *limit);
extern Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit);
extern Res TraceScanAreaMasked(ScanState ss, Addr *base, Addr *limit, Word mask);


/* Action Interface -- see design.mps.action */

extern Bool ActionCheck(Action action);
extern void ActionInit(Action action, Pool pool);
extern void ActionFinish(Action action);
extern void ActionPoll(Space space);


/* Space Interface -- see impl.c.space */

extern Res SpaceCreate(Space *spaceReturn, Addr base, Size size);
extern void SpaceDestroy(Space space);
extern Bool SpaceCheck(Space space);
extern Res SpaceDescribe(Space space, mps_lib_FILE *stream);
extern Bool SpaceAccess(Addr addr, AccessSet mode);
extern void SpaceEnter(Space space);
extern void SpaceLeave(Space space);
extern void SpacePoll(Space space);
extern Res SpaceAlloc(void **baseReturn, Space space, Size size);
extern void SpaceFree(Space space, Addr base, Size size);

#define SpacePoolRing(space)    (&(space)->poolRing)
#define SpaceRootRing(space)    (&(space)->rootRing)
#define SpaceTraceRing(space)   (&(space)->traceRing)
#define SpaceThreadRing(space)  (&(space)->threadRing)
#define SpaceEpoch(space)       ((space)->epoch) /* .epoch.ts */
#define SpaceTrace(space, ti)	(&(space)->trace[ti])
#define SpaceZoneShift(space)	((space)->zoneShift)

/* Arena Interface -- see impl.c.arena* */

extern Res ArenaCreate(Space *spaceReturn, Size size, Addr base);
extern void ArenaDestroy(Space space);
extern Bool ArenaCheck(Arena arena);
extern Align ArenaAlign(Space space);
extern Size ArenaReserved(Space space);
extern Size ArenaCommitted(Space space);

extern Res ArenaExtend(Space, Addr /* base */, Size /* size */);
extern Res ArenaRetract(Space, Addr /* base */, Size /* size */);

extern Bool SegPrefCheck(SegPref pref);
extern SegPref SegPrefDefault (void);
extern Res SegPrefExpress (SegPref, SegPrefKind, void *);

extern Res SegAlloc(Seg *segReturn, SegPref pref,
                    Space space, Size size, Pool pool);
extern void SegFree(Space space, Seg seg);
extern Addr SegBase(Space space, Seg seg);
extern Addr SegLimit(Space space, Seg seg);
extern Size SegSize(Space space, Seg seg);
extern Bool SegOfAddr(Seg *segReturn, Space space, Addr addr);
extern Bool SegFirst(Seg *segReturn, Space space);
extern Bool SegNext(Seg *segReturn, Space space, Addr addr);
extern Bool SegCheck(Seg seg);
extern void SegInit(Seg seg, Pool pool);
extern void SegFinish(Seg seg);

#define SegPool(seg)		((seg)->_pool)
#define SegSingle(seg)		((seg)->_single)
#define SegRankSet(seg)		((seg)->_rankSet)
#define SegPM(seg)		((seg)->_pm)
#define SegSM(seg)		((seg)->_sm)
#define SegDepth(seg)		((seg)->_depth)
#define SegP(seg)		((seg)->_p)
#define SegGrey(seg)		((seg)->_grey)
#define SegWhite(seg)		((seg)->_white)
#define SegSummary(seg)		((seg)->_summary)
#define SegBuffer(seg)		((seg)->_buffer)
#define SegPoolRing(seg)	(&(seg)->_poolRing)
#define SegOfPoolRing(node)	RING_ELT(Seg, _poolRing, node)

#define SegSetPool(seg, pool)   ((void)((seg)->_pool = (pool)))
#define SegSetSingle(seg, s)	((void)((seg)->_single = (s)))
#define SegSetRankSet(seg, rs)	((void)((seg)->_rankSet = (rs)))
#define SegSetPM(seg, mode)	((void)((seg)->_pm = (mode)))
#define SegSetSM(seg, mode)	((void)((seg)->_sm = (mode)))
#define SegSetDepth(seg, d)	((void)((seg)->_depth = (d)))
#define SegSetP(seg, pp)	((void)((seg)->_p = (pp)))
#define SegSetGrey(seg, ts)	((void)((seg)->_grey = (ts)))
#define SegSetWhite(seg, ts)	((void)((seg)->_white = (ts)))
#define SegSetSummary(seg, rs)	((void)((seg)->_summary = (rs)))
#define SegSetBuffer(seg, b)	((void)((seg)->_buffer = (b)))


/* Buffer Interface -- see impl.c.buffer */

extern Res BufferCreate(Buffer *bufferReturn, Pool pool, Rank rank);
extern void BufferDestroy(Buffer buffer);
extern Bool BufferCheck(Buffer buffer);
extern Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream);
extern Res BufferReserve(Addr *pReturn, Buffer buffer, Size size);
extern Res BufferFill(Addr *pReturn, Buffer buffer, Size size);
extern Bool BufferCommit(Buffer buffer, Addr p, Size size);
extern Bool BufferTrip(Buffer buffer, Addr p, Size size);
extern Res BufferInit(Buffer buffer, Pool pool, Rank rank);
extern void BufferFinish(Buffer buffer);
extern Bool BufferIsReset(Buffer buffer);
extern Bool BufferIsReady(Buffer buffer);
extern void BufferFlip(Buffer buffer);
extern Addr BufferScanLimit(Buffer buffer);
extern AP (BufferAP)(Buffer buffer);
#define BufferAP(buffer)        (&(buffer)->apStruct)
extern Buffer BufferOfAP(AP ap);
#define BufferOfAP(ap)		PARENT(BufferStruct, apStruct, ap)
extern Space BufferSpace(Buffer buffer);
#define BufferSpace(buffer)	((buffer)->space)
extern Pool (BufferPool)(Buffer buffer);
#define BufferPool(buffer)      ((buffer)->pool)
extern Seg (BufferSeg)(Buffer buffer);
#define BufferSeg(buffer)       ((buffer)->seg)
extern Rank (BufferRankSet)(Buffer buffer);
#define BufferRankSet(buffer)   ((buffer)->rankSet)
extern Addr (BufferBase)(Buffer buffer);
#define BufferBase(buffer)      ((buffer)->base)
extern Addr (BufferGetInit)(Buffer buffer);
#define BufferGetInit(buffer)   (BufferAP(buffer)->init)
extern Addr (BufferAlloc)(Buffer buffer);
#define BufferAlloc(buffer)     (BufferAP(buffer)->alloc)
extern Addr (BufferLimit)(Buffer buffer);
#define BufferLimit(buffer)     ((buffer)->poolLimit)


/* Format Interface -- see impl.c.format */

extern Bool FormatCheck(Format format);
extern Res FormatCreate(Format *formatReturn, Space space,
                        Align alignment,
                        FormatScanMethod scan,
                        FormatSkipMethod skip,
                        FormatMoveMethod move,
                        FormatIsMovedMethod isMoved,
                        FormatCopyMethod copy,
                        FormatPadMethod pad);
extern void FormatDestroy(Format format);
extern Space FormatSpace(Format format);
extern Res FormatDescribe(Format format, mps_lib_FILE *stream);


/* Reference Interface -- see impl.c.ref */

extern Bool RankCheck(Rank rank);
extern Bool RankSetCheck(RankSet rankSet);

#define RankSetIsMember(rs, r)	BS_IS_MEMBER(rs, r)
#define RankSetSingle(r)	BS_SINGLE(RankSet, r)
#define RankSetIsSingle(r)	BS_IS_SINGLE(r)

#define RefSetZone(space, addr) \
  (((Word)(addr) >> space->zoneShift) & (MPS_WORD_WIDTH - 1))
#define RefSetUnion(rs1, rs2)   BS_UNION(rs1, rs2)
#define RefSetInter(rs1, rs2)   BS_INTER(rs1, rs2)
#define RefSetAdd(space, rs, addr) \
  BS_ADD(RefSet, rs, RefSetZone(space, addr))
#define RefSetIsMember(space, rs, addr) \
  BS_IS_MEMBER(rs, RefSetZone(space, addr))
#define RefSetSuper(rs1, rs2)   BS_SUPER(rs1, rs2)
#define RefSetDiff(rs1, rs2)	BS_DIFF(rs1, rs2)
#define RefSetSub(rs1, rs2)	BS_SUB(rs1, rs2)

extern RefSet RefSetOfSeg(Space space, Seg seg);


/* Shield Interface -- see impl.c.shield */

extern void ShieldRaise(Space space, Seg seg, AccessSet mode);
extern void ShieldLower(Space space, Seg seg, AccessSet mode);
extern void ShieldEnter(Space space);
extern void ShieldLeave(Space space);
extern void ShieldExpose(Space space, Seg seg);
extern void ShieldCover(Space space, Seg seg);
extern void ShieldSuspend(Space space);
extern void ShieldResume(Space space);
extern void ShieldFlush(Space space);


/* Protection Interface -- see impl.c.prot* */

extern void ProtSetup(void);

extern void ProtSet(Addr base, Addr limit, AccessSet mode);
extern void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
                      void *p, size_t s);
extern void ProtSync(Space space);


/* Location Dependency -- see impl.c.ld */

extern void LDReset(LD ld, Space space);
extern void LDAdd(LD ld, Space space, Addr addr);
extern Bool LDIsStale(LD ld, Space space, Addr addr);
extern void LDAge(Space space, RefSet moved);
extern void LDMerge(LD ld, Space space, LD from);


/* Root Interface -- see impl.c.root */

extern Res RootCreateTable(Root *rootReturn, Space space,
                           Rank rank, Addr *base, Addr *limit);
extern Res RootCreateTableMasked(Root *rootReturn, Space space,
                                 Rank rank, Addr *base, Addr *limit,
                                 Word mask);
extern Res RootCreateReg(Root *rootReturn, Space space,
                           Rank rank, Thread thread,
                           RootScanRegMethod scan,
                           void *p, size_t s);
extern Res RootCreateFmt(Root *rootReturn, Space space,
                           Rank rank, FormatScanMethod scan,
                           Addr base, Addr limit);
extern Res RootCreateFun(Root *rootReturn, Space space,
                        Rank rank, RootScanMethod scan,
                        void *p, size_t s);
extern void RootDestroy(Root root);
extern Bool RootCheck(Root root);
extern Res RootDescribe(Root root, mps_lib_FILE *stream);
extern Bool RootIsAtomic(Root root);
extern Rank RootRank(Root root);
extern void RootGrey(Root root, Trace trace);
extern Res RootScan(ScanState ss, Root root);
extern Space RootSpace(Root root);


/* VM Interface -- see impl.c.vm* */

extern Align VMAlign(void);
extern Bool VMCheck(VM vm);
extern Res VMCreate(Space *spaceReturn, Size size, Addr base);
extern void VMDestroy(Space space);
extern Addr VMBase(Space space);
extern Addr VMLimit(Space space);
extern Res VMMap(Space space, Addr base, Addr limit);
extern void VMUnmap(Space space, Addr base, Addr limit);
extern Size VMReserved(Space space);
extern Size VMMapped(Space space);

#endif /* mpm_h */
