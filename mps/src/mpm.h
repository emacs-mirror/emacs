/* impl.h.mpm: MEMORY POOL MANAGER DEFINITIONS
 *
 * $HopeName: MMsrc!mpm.h(trunk.12) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 */

#ifndef mpm_h
#define mpm_h

#include "mpmconf.h"
#include "mpmtypes.h"
#include "mpmst.h"
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

#ifdef TARGET_MPM_ASSERT        /* impl.h.target */
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
#define FUNCHECK(f)	(FunCheck((Fun)f))


/* Address/Size Interface -- see impl.c.mpm */

extern Bool (WordIsAligned)(Word word, Align align);
#define WordIsAligned(w, a)     (((w) & ((a) - 1)) == 0)

extern Word (WordAlignUp)(Word word, Align align);
#define WordAlignUp(w, a)       (((w) + (a) - 1) & ~((a) - 1))

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


/* Logs and Powers
 * 
 * SizeIsP2 returns TRUE if and only if size is a non-negative integer
 * power of 2, and FALSE otherwise.
 * 
 * SizeLog2 returns the logarithm in base 2 of size.  size must be a
 * power of 2.
 * 
 * SizeFloorLog2 returns the floor of the logarithm in base 2 of size.
 * size can be any value.
 */
extern Bool SizeIsP2(Size size);
extern Shift SizeLog2(Size size);
extern Shift SizeFloorLog2(Size size);

#define AddrWord(a)             ((Word)a)
#define SizeWord(s)             ((Word)s)
#define AddrIsAligned(p, a)     WordIsAligned(AddrWord(p), a)
#define AddrAlignUp(p, a)       ((Addr)WordAlignUp(AddrWord(p), a))
#define SizeIsAligned(s, a)     WordIsAligned(SizeWord(s), a)
#define SizeAlignUp(s, a)       ((Size)WordAlignUp(SizeWord(s), a))

extern Res WriteF(mps_lib_FILE *stream, ...);


/* Ring Interface -- see impl.c.ring */

extern Bool RingCheck(Ring ring);
extern Bool RingCheckSingle(Ring ring);

extern void (RingInit)(Ring ring);
#define RingInit(ring) \
  BEGIN \
    AVER(NULL != (ring)); \
    (ring)->next = (ring); \
    (ring)->prev = (ring); \
    AVER(RingCheck(ring)); \
  END

extern void (RingFinish)(Ring ring);
#define RingFinish(ring) \
  BEGIN \
    AVER(RingCheckSingle(ring)); \
    (ring)->next = RingNONE; \
    (ring)->prev = RingNONE; \
  END

extern void (RingAppend)(Ring ring, Ring new);
#define RingAppend(ring, new) \
  BEGIN \
    AVER(RingCheck(ring)); \
    AVER(RingCheckSingle(new)); \
    (new)->prev = (ring)->prev; \
    (new)->next = (ring); \
    (ring)->prev->next = (new); \
    (ring)->prev = (new); \
  END

extern void (RingRemove)(Ring old);
#define RingRemove(old) \
  BEGIN \
    AVER(RingCheck(old)); \
    (old)->next->prev = (old)->prev; \
    (old)->prev->next = (old)->next; \
    (old)->next = (old); \
    (old)->prev = (old); \
  END

extern Ring (RingNext)(Ring ring);
#define RingNext(ring)  ((ring)->next)

#define RING_ELT(type, field, node) \
   ((type)((char *)(node) - (size_t)(&((type)0)->field)))

#define RING_FOR(var, ring) \
  for(var = RingNext(ring); \
      var != (ring); \
      var = RingNext(var))


/* Pool Interface -- see impl.c.pool */

extern Res PoolInit(Pool pool, Space space, PoolClass class, ...);
extern Res PoolInitV(Pool pool, Space space, PoolClass class, va_list args);
extern void PoolFinish(Pool pool);
extern Bool PoolClassCheck(PoolClass class);
extern Bool PoolCheck(Pool pool);
extern Res PoolDescribe(Pool pool, mps_lib_FILE *stream);

extern Space (PoolSpace)(Pool pool);
#define PoolSpace(pool)         ((pool)->space)

extern Align (PoolAlignment)(Pool pool);
#define PoolAlignment(pool)     ((pool)->alignment)

extern Res PoolSegAlloc(Seg *segReturn, Pool pool, Size size);
extern void PoolSegFree(Pool pool, Seg seg);
extern Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr);
extern Bool PoolHasAddr(Pool pool, Addr addr);
extern Res PoolCreate(Pool *poolReturn, PoolClass class,
                        Space space, ...);
extern Res PoolCreateV(Pool *poolReturn, PoolClass class,
                         Space space, va_list arg);
extern void PoolDestroy(Pool pool);
extern Res PoolAlloc(Addr *pReturn, Pool pool, Size size);
extern void PoolFree(Pool pool, Addr old, Size size);
extern Res PoolCondemn(RefSet *condemnedReturn, Pool pool,
                         Space space, TraceId ti);
extern void PoolGrey(Pool pool, Space space, TraceId ti);
extern Res PoolScan(ScanState ss, Pool pool, Bool *finishedReturn);
extern Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO);
#define PoolFix(pool, ss, seg, refIO) \
  ((*(pool)->class->fix)(pool, ss, seg, refIO))

extern void PoolReclaim(Pool pool, Space space, TraceId ti);
extern void PoolAccess(Pool pool, Seg seg, AccessSet mode);

extern void PoolTrivFinish(Pool pool);
extern Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size);
extern Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size);
extern void PoolNoFree(Pool pool, Addr old, Size size);
extern void PoolTrivFree(Pool pool, Addr old, Size size);
extern Res PoolNoBufferInit(Pool pool, Buffer buf);
extern Res PoolTrivBufferInit(Pool pool, Buffer buf);
extern void PoolNoBufferFinish(Pool pool, Buffer buf);
extern void PoolTrivBufferFinish(Pool pool, Buffer buf);
extern Res PoolNoBufferFill(Addr *baseReturn, Pool pool, Buffer buffer, Size size);
extern Bool PoolNoBufferTrip(Pool pool, Buffer buffer, Addr base, Size size);
extern void PoolNoBufferExpose(Pool pool, Buffer buffer);
extern void PoolNoBufferCover(Pool pool, Buffer buffer);
extern Res PoolNoDescribe(Pool pool, mps_lib_FILE *stream);
extern Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream);
extern Res PoolNoCondemn(RefSet *condemnedReturn, Pool pool, Space space, TraceId ti);
extern void PoolNoGrey(Pool pool, Space space, TraceId ti);
extern Res PoolNoScan(ScanState ss, Pool pool, Bool *finishedReturn);
extern Res PoolNoFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
extern void PoolNoReclaim(Pool pool, Space space, TraceId ti);
extern void PoolNoAccess(Pool pool, Seg seg, AccessSet mode);


/* Trace Interface -- see impl.c.trace */

extern TraceSet (TraceSetAdd)(TraceSet set, TraceId id);
#define TraceSetAdd(set, id)            ((set) | ((TraceSet)1 << (id)))

extern TraceSet (TraceSetDelete)(TraceSet set, TraceId id);
#define TraceSetDelete(set, id)         ((set) & ~((TraceSet)1 << (id)))

extern Bool (TraceSetIsMember)(TraceSet set, TraceId id);
#define TraceSetIsMember(set, id)       (((set) >> (id)) & 1)

extern TraceSet (TraceSetUnion)(TraceSet set1, TraceSet set2);
#define TraceSetUnion(set1, set2)       ((set1) | (set2))

extern Res TraceCreate(TraceId *tiReturn, Space space);
extern void TraceDestroy(Space space, TraceId ti);

extern Bool ScanStateCheck(ScanState ss);
extern Bool TraceIdCheck(TraceId id);
extern Bool TraceSetCheck(TraceSet ts);

extern Res TraceFlip(Space space, TraceId ti, RefSet condemned);
extern Size TracePoll(Space space, TraceId ti);

extern Res TraceRunAtomic(Space space, TraceId ti);
extern Res TraceRun(Space space, TraceId ti, Bool *finishedReturn);

extern Res TraceFix(ScanState ss, Ref *refIO);

/* Equivalent to impl.h.mps MPS_SCAN_BEGIN */

#define TRACE_SCAN_BEGIN(ss) \
  BEGIN \
    Shift SCANzoneShift = (ss)->zoneShift; \
    RefSet SCANcondemned = (ss)->condemned; \
    RefSet SCANsummary = (ss)->summary; \
    Word SCANt; \
    {

/* Equivalent to impl.h.mps MPS_FIX1 */

#define TRACE_FIX1(ss, ref) \
  (SCANt = (Word)1<<((Word)(ref)>>SCANzoneShift&(WORD_WIDTH-1)), \
   SCANsummary |= SCANt, \
   SCANcondemned & SCANt)

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


/* Arena Interface -- see impl.c.arena* */

extern Res ArenaCreate(Space *spaceReturn, Size size, Addr base);
extern void ArenaDestroy(Space space);
extern Bool ArenaCheck(Arena arena);
extern Align ArenaAlign(Space space);
extern Size ArenaReserved(Space space);
extern Size ArenaCommitted(Space space);
extern Res SegAlloc(Seg *segReturn, Space space, Size size, Pool pool);
extern void SegFree(Space space, Seg seg);
extern Addr SegBase(Space space, Seg seg);
extern Addr SegLimit(Space space, Seg seg);
extern Size SegSize(Space space, Seg seg);
extern Bool SegOfAddr(Seg *segReturn, Space space, Addr addr);
extern Seg SegFirst(Space space);
extern Seg SegNext(Space space, Seg seg);
extern Bool SegCheck(Seg seg);


/* Buffer Interface -- see impl.c.buffer */

extern Res BufferCreate(Buffer *bufferReturn, Pool pool, Rank rank);
extern void BufferDestroy(Buffer buffer);
extern Bool BufferCheck(Buffer buffer);
extern Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream);
extern Res BufferReserve(Addr *pReturn, Buffer buffer, Size size);
extern Res BufferFill(Addr *pReturn, Buffer buffer, Size size);
extern Bool BufferCommit(Buffer buffer, Addr p, Size size);
extern Bool BufferTrip(Buffer buffer, Addr p, Size size);
extern void BufferExpose(Buffer buffer);
extern void BufferCover(Buffer buffer);
extern void BufferInit(Buffer buffer, Pool pool, Rank rank);
extern void BufferFinish(Buffer buffer);
extern void BufferSet(Buffer buffer, Seg seg, Addr base, Addr init, Addr limit);
extern void BufferReset(Buffer buffer);
extern Bool BufferIsReset(Buffer buffer);
extern Bool BufferIsReady(Buffer buffer);
extern AP BufferAP(Buffer buffer);
extern Buffer BufferOfAP(AP ap);
extern Space BufferSpace(Buffer buffer);
extern Pool (BufferPool)(Buffer buffer);
#define BufferPool(buffer) ((buffer)->pool)


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

#define RefSetEmpty             ((RefSet)0)
#define RefSetUniv              ((RefSet)-1)
#define RefSetUnion(rs1, rs2)   ((rs1) | (rs2))
#define RefSetInter(rs1, rs2)   ((rs1) & (rs2))
#define RefSetZone(space, addr) \
  (((Word)(addr) >> space->zoneShift) & (WORD_WIDTH - 1))
#define RefSetAdd(space, rs, addr) \
  ((rs) | ((RefSet)1 << RefSetZone(space, addr)))
#define RefSetIsMember(space, rs, addr) \
  (((rs) >> RefSetZone(space, addr)) & 1)
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


/* Root Interface -- see impl.c.root */

extern Res RootCreateTable(Root *rootReturn, Space space,
                             Rank rank, Addr *base, Addr *limit);
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
extern void RootGrey(Root root, TraceId ti);
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
