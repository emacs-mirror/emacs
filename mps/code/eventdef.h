/* <code/eventdef.h> -- Event Logging Definitions
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/telemetry/>
 *
 * .desc: This file declares macros that define the types of events and their
 * properties.
 *
 * TRANSGRESSIONS
 *
 * .kind.abuse: A few events have a kind which is not obvious from the
 * type of the objects that the event relates to.  They are given the
 * kind that that have on the grounds of expected use.  The kinds are
 * used in controlling the overall volume of telemetry and these events are
 * given kinds so that they are grouped under the same control as events
 * you are likely to want to see them with.  (So for example, lots of
 * scanner events have the same kind, Seg, because if you are interested
 * in one then you're probably interested in them all and it's a similar
 * amount of data).
 */

#ifndef eventdef_h
#define eventdef_h


/* EVENT_VERSION_* -- three part version number
 *
 * Increment the minor version when adding new events,
 * the median version when changing an existing event,
 * and the major version when changing the format of the event file.
 */

#define EVENT_VERSION_MAJOR  ((unsigned)1)
#define EVENT_VERSION_MEDIAN ((unsigned)0)
#define EVENT_VERSION_MINOR  ((unsigned)0)


/* EVENT_LIST -- list of event types and general properties
 *
 * These specify:
 *   - Type: The name of the event type, without the leading "Event";
 *   - Code: The unique 16-bit code associated with this event type;
 *   - Always: Whether this event type should appear in optimised
 *     varieties, not currently used;
 *   - Kind: Category into which this event falls, without the
 *     leading "EventKind";
 *
 * TODO: Add a doc string to each event type.
 *
 * See also EVENT_*_PARAMS for definition of event parameters.
 */
 
#define EventNameMAX ((size_t)19)
#define EventCodeMAX ((EventCode)0x0071)

/* TODO: Work out why not-in-use events were not in use and restore or delete them. */

#define EVENT_LIST(EVENT, X) \
  /*       0123456789012345678 <- don't exceed without changing EventNameMAX */ \
  EVENT(X, AMCGenCreate       , 0x0001,  TRUE, Pool) \
  EVENT(X, AMCGenDestroy      , 0x0002,  TRUE, Pool) \
  EVENT(X, AMCInit            , 0x0003,  TRUE, Pool) \
  EVENT(X, AMCFinish          , 0x0004,  TRUE, Pool) \
  EVENT(X, ArenaCreateVM      , 0x0005,  TRUE, Arena) \
  EVENT(X, ArenaCreateVMNZ    , 0x0006,  TRUE, Arena) \
  EVENT(X, ArenaWriteFaults   , 0x0007,  TRUE, Trace) \
  EVENT(X, MeterInit          , 0x0008,  TRUE, Pool) \
  EVENT(X, MeterValues        , 0x0009,  TRUE, Pool) \
  EVENT(X, AMCScanBegin       , 0x000a,  TRUE, Seg) \
  EVENT(X, AMCScanEnd         , 0x000b,  TRUE, Seg) \
  EVENT(X, AMCFix             , 0x000c, FALSE, Ref) \
  EVENT(X, AMCFixInPlace      , 0x000d, FALSE, Ref) \
  EVENT(X, AMCFixForward      , 0x000e, FALSE, Ref) \
  EVENT(X, AMCReclaim         , 0x000f,  TRUE, Seg) \
  /* EVENT(X, AMCTraceEnd        , 0x0010, TRUE, Trace) */ \
  EVENT(X, ArenaCreateCL      , 0x0011,  TRUE, Arena) \
  EVENT(X, ArenaDestroy       , 0x0012,  TRUE, Arena) \
  EVENT(X, SegAlloc           , 0x0013,  TRUE, Seg) \
  EVENT(X, SegFree            , 0x0014,  TRUE, Seg) \
  EVENT(X, PoolInit           , 0x0015,  TRUE, Pool) \
  EVENT(X, PoolFinish         , 0x0016,  TRUE, Pool) \
  EVENT(X, PoolAlloc          , 0x0017,  TRUE, Object) \
  EVENT(X, PoolFree           , 0x0018,  TRUE, Object) \
  EVENT(X, CBSInit            , 0x0019,  TRUE, Pool) \
  EVENT(X, Intern             , 0x001a,  TRUE, User) \
  EVENT(X, Label              , 0x001b,  TRUE, User) \
  /* EVENT(X, TraceStart         , 0x001c,  TRUE, Trace) */ \
  /* EVENT(X, TraceCreate        , 0x001d,  TRUE, Trace) */ \
  EVENT(X, TraceDestroy       , 0x001e,  TRUE, Trace) \
  EVENT(X, SegSetGrey         , 0x001f,  TRUE, Seg) \
  EVENT(X, TraceFlipBegin     , 0x0020,  TRUE, Trace) \
  EVENT(X, TraceFlipEnd       , 0x0021,  TRUE, Trace) \
  EVENT(X, TraceReclaim       , 0x0022,  TRUE, Seg) \
  /* EVENT(X, TraceScan          , 0x0023, TRUE, Seg) */ \
  EVENT(X, TraceAccess        , 0x0024,  TRUE, Seg) \
  /* TracePoll's kind isn't really Trace, but then it isn't Seg either */ \
  /* EVENT(X, TracePoll          , 0x0025,  TRUE, Trace) */ \
  EVENT(X, TraceFix           , 0x0026, FALSE, Ref) \
  EVENT(X, TraceFixSeg        , 0x0027, FALSE, Ref) \
  EVENT(X, TraceFixWhite      , 0x0028, FALSE, Ref) \
  /* TraceScanArea{Tagged} abuses kind, see .kind.abuse */ \
  EVENT(X, TraceScanArea      , 0x0029,  TRUE, Seg) \
  EVENT(X, TraceScanAreaTagged, 0x002a,  TRUE, Seg) \
  EVENT(X, VMCreate           , 0x002b,  TRUE, Arena) \
  EVENT(X, VMDestroy          , 0x002c,  TRUE, Arena) \
  EVENT(X, VMMap              , 0x002d,  TRUE, Seg) \
  EVENT(X, VMUnmap            , 0x002e,  TRUE, Seg) \
  EVENT(X, ArenaExtend        , 0x002f,  TRUE, Arena) \
  /* EVENT(X, ArenaRetract       , 0x0030,  TRUE, Arena) */ \
  /* EVENT(X, TraceSegGreyen     , 0x0031,  TRUE, Seg) */ \
  /* RootScanned abuses kind, see .kind.abuse */ \
  EVENT(X, RootScan           , 0x0032, TRUE, Seg) \
  /* TraceStep abuses kind, see .kind.abuse */ \
  /* EVENT(X, TraceStep          , 0x0033,  TRUE, Seg) */ \
  EVENT(X, BufferReserve      , 0x0034,  TRUE, Object) \
  EVENT(X, BufferCommit       , 0x0035,  TRUE, Object) \
  /* BufferInit/Finish abuse kind, see .kind.abuse */ \
  EVENT(X, BufferInit         , 0x0036,  TRUE, Pool) \
  EVENT(X, BufferFinish       , 0x0037,  TRUE, Pool) \
  /* EVENT(X, MVTFinish          , 0x0038, TRUE, Pool) */ \
  EVENT(X, BufferFill         , 0x0039,  TRUE, Seg) \
  EVENT(X, BufferEmpty        , 0x003A,  TRUE, Seg) \
  EVENT(X, SegAllocFail       , 0x003B,  TRUE, Seg) \
  EVENT(X, TraceScanSeg       , 0x003C,  TRUE, Seg) \
  /* TraceScanSingleRef abuses kind, see .kind.abuse */ \
  EVENT(X, TraceScanSingleRef , 0x003D,  TRUE, Seg) \
  EVENT(X, TraceStatCondemn   , 0x003E,  TRUE, Trace) \
  EVENT(X, TraceStatScan      , 0x003F,  TRUE, Trace) \
  EVENT(X, TraceStatFix       , 0x0040,  TRUE, Trace) \
  EVENT(X, TraceStatReclaim   , 0x0041,  TRUE, Trace) \
  EVENT(X, PoolInitMVFF       , 0x0042,  TRUE, Pool) \
  EVENT(X, PoolInitMV         , 0x0043,  TRUE, Pool) \
  EVENT(X, PoolInitMFS        , 0x0044,  TRUE, Pool) \
  /* EVENT(X, PoolInitEPVM       , 0x0045,  TRUE, Pool) */ \
  /* EVENT(X, PoolInitEPDL       , 0x0046,  TRUE, Pool) */ \
  EVENT(X, PoolInitAMS        , 0x0047,  TRUE, Pool) \
  EVENT(X, PoolInitAMC        , 0x0048,  TRUE, Pool) \
  EVENT(X, PoolInitAMCZ       , 0x0049,  TRUE, Pool) \
  EVENT(X, PoolInitAWL        , 0x004A,  TRUE, Pool) \
  EVENT(X, PoolInitLO         , 0x004B,  TRUE, Pool) \
  EVENT(X, PoolInitSNC        , 0x004C,  TRUE, Pool) \
  EVENT(X, PoolInitMVT        , 0x004D,  TRUE, Pool) \
  /* EVENT(X, BufferInitEPVM     , 0x0050,  TRUE, Pool) */ \
  EVENT(X, BufferInitSeg      , 0x0051,  TRUE, Pool) \
  EVENT(X, BufferInitRank     , 0x0052,  TRUE, Pool) \
  /* PoolPush/Pop go under Object, because they're user ops. */ \
  /* EVENT(X, PoolPush           , 0x0060,  TRUE, Object) */ \
  /* EVENT(X, PoolPop            , 0x0061,  TRUE, Object) */ \
  EVENT(X, ReservoirLimitSet  , 0x0062,  TRUE, Arena) \
  EVENT(X, CommitLimitSet     , 0x0063,  TRUE, Arena) \
  EVENT(X, SpareCommitLimitSet, 0x0064,  TRUE, Arena) \
  EVENT(X, ArenaAlloc         , 0x0065,  TRUE, Arena) \
  EVENT(X, ArenaFree          , 0x0066,  TRUE, Arena) \
  EVENT(X, ArenaAllocFail     , 0x0067,  TRUE, Arena) \
  EVENT(X, SegMerge           , 0x0068,  TRUE, Seg) \
  EVENT(X, SegSplit           , 0x0069,  TRUE, Seg) \
  /* Events converted from RHSK's diagnostics */ \
  EVENT(X, vmArenaExtendStart , 0x006A,  TRUE, Arena) \
  EVENT(X, vmArenaExtendFail  , 0x006B,  TRUE, Arena) \
  EVENT(X, vmArenaExtendDone  , 0x006C,  TRUE, Arena) \
  EVENT(X, MessagesDropped    , 0x006D,  TRUE, Arena) \
  EVENT(X, MessagesExist      , 0x006E,  TRUE, Arena) \
  EVENT(X, ChainCondemnAuto   , 0x006F,  TRUE, Trace) \
  EVENT(X, TraceFindGrey      , 0x0070,  TRUE, Trace) \
  EVENT(X, TraceBandAdvance   , 0x0071,  TRUE, Trace)
  

/* Remember to update EventNameMAX and EventCodeMAX in eventcom.h! 
   (These are checked in EventInit.) */


/* EVENT_*_PARAMS -- definition of event parameters
 *
 * For each event type in EVENT_LIST, these macros list the parameters of
 * the event.  THe columns are:
 *   - the positional index of the parameter in the list, used to define
 *     numeric field names using the C preprocessor
 *   - the parameter sort, similar to writef (Pointer, Addr, Word, Unsigned,
 *     String, Double, Bool)
 *   - a parameter identifier for display or use in code
 *
 * TODO: Add a doc string to each parameter.
 */

#define EVENT_AMCGenCreate_PARAMS(PARAM, X) \
  PARAM(X,  0, P, amc) \
  PARAM(X,  1, P, gen)

#define EVENT_AMCGenDestroy_PARAMS(PARAM, X) \
  PARAM(X,  0, P, gen)

#define EVENT_AMCInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, amc)

#define EVENT_AMCFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, amc)

#define EVENT_AMCFix_PARAMS(PARAM, X)

#define EVENT_ArenaCreateVM_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, userSize) \
  PARAM(X,  2, W, chunkSize)

#define EVENT_ArenaCreateVMNZ_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, userSize) \
  PARAM(X,  2, W, chunkSize)

#define EVENT_ArenaWriteFaults_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, writeBarrierHitCount)

#define EVENT_MeterInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, meter) \
  PARAM(X,  1, P, owner)

#define EVENT_MeterValues_PARAMS(PARAM, X) \
  PARAM(X,  0, P, meter) \
  PARAM(X,  1, D, total) \
  PARAM(X,  2, D, meanSquared) \
  PARAM(X,  3, W, count) \
  PARAM(X,  4, W, max) \
  PARAM(X,  5, W, min)

#define EVENT_AMCScanBegin_PARAMS(PARAM, X) \
  PARAM(X,  0, P, amc) \
  PARAM(X,  1, P, seg) \
  PARAM(X,  2, P, ss)

#define EVENT_AMCScanEnd_PARAMS(PARAM, X) \
  PARAM(X,  0, P, amc) \
  PARAM(X,  1, P, seg) \
  PARAM(X,  2, P, ss)

#define EVENT_AMCFix_PARAMS(PARAM, X)

#define EVENT_AMCFixInPlace_PARAMS(PARAM, X)

#define EVENT_AMCFixForward_PARAMS(PARAM, X) \
  PARAM(X,  0, A, newRef)

#define EVENT_AMCReclaim_PARAMS(PARAM, X) \
  PARAM(X,  0, P, gen) \
  PARAM(X,  1, P, trace) \
  PARAM(X,  2, P, seg)

#define EVENT_ArenaCreateCL_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, size) \
  PARAM(X,  2, A, base)

#define EVENT_ArenaDestroy_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena)

#define EVENT_SegAlloc_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, P, seg) \
  PARAM(X,  2, A, base) \
  PARAM(X,  3, W, size) \
  PARAM(X,  4, P, pool)

#define EVENT_SegFree_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, P, seg)

#define EVENT_PoolInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, arena) \
  PARAM(X,  2, P, poolClass)

#define EVENT_PoolFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool)

#define EVENT_PoolAlloc_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, A, pReturn) \
  PARAM(X,  2, W, size)

#define EVENT_PoolFree_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, A, old) \
  PARAM(X,  2, W, size)

#define EVENT_CBSInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, cbs) \
  PARAM(X,  1, P, owner)

#define EVENT_Intern_PARAMS(PARAM, X) \
  PARAM(X,  0, W, stringId) \
  PARAM(X,  1, S, string)

#define EVENT_Label_PARAMS(PARAM, X) \
  PARAM(X,  0, A, address) \
  PARAM(X,  1, W, stringId)

#define EVENT_TraceDestroy_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace)

#define EVENT_SegSetGrey_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, P, seg) \
  PARAM(X,  2, U, grey)

#define EVENT_TraceFlipBegin_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace) \
  PARAM(X,  1, P, arena)

#define EVENT_TraceFlipEnd_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace) \
  PARAM(X,  1, P, arena)

#define EVENT_TraceReclaim_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace)

#define EVENT_TraceAccess_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, P, seg) \
  PARAM(X,  2, U, mode)

#define EVENT_TraceFix_PARAMS(PARAM, X) \
  PARAM(X,  0, P, ss) \
  PARAM(X,  1, P, refIO) \
  PARAM(X,  2, A, ref) \
  PARAM(X,  3, U, rank)

#define EVENT_TraceFixSeg_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg)

#define EVENT_TraceFixWhite_PARAMS(PARAM, X)

#define EVENT_TraceScanArea_PARAMS(PARAM, X) \
  PARAM(X,  0, P, ss) \
  PARAM(X,  1, P, base) \
  PARAM(X,  2, P, limit)

#define EVENT_TraceScanAreaTagged_PARAMS(PARAM, X) \
  PARAM(X,  0, P, ss) \
  PARAM(X,  1, P, base) \
  PARAM(X,  2, P, limit)

#define EVENT_VMCreate_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm) \
  PARAM(X,  1, A, base) \
  PARAM(X,  2, A, limit)

#define EVENT_VMDestroy_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm)

#define EVENT_VMMap_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm) \
  PARAM(X,  1, A, base) \
  PARAM(X,  2, A, limit)

#define EVENT_VMUnmap_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm) \
  PARAM(X,  1, A, base) \
  PARAM(X,  2, A, limit)

#define EVENT_ArenaExtend_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, A, base) \
  PARAM(X,  2, W, size)

#define EVENT_RootScan_PARAMS(PARAM, X) \
  PARAM(X,  0, P, root) \
  PARAM(X,  1, W, ts) \
  PARAM(X,  2, W, summary)

#define EVENT_BufferReserve_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, A, init) \
  PARAM(X,  2, W, size)

#define EVENT_BufferCommit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, A, p) \
  PARAM(X,  2, W, size) \
  PARAM(X,  3, A, clientClass)

#define EVENT_BufferInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, P, pool) \
  PARAM(X,  2, B, isMutator)

#define EVENT_BufferFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer)

#define EVENT_BufferFill_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, W, size) \
  PARAM(X,  2, A, base) \
  PARAM(X,  3, W, filled)

#define EVENT_BufferEmpty_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, W, spare)

#define EVENT_SegAllocFail_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, size) \
  PARAM(X,  2, P, pool)

#define EVENT_TraceScanSeg_PARAMS(PARAM, X) \
  PARAM(X,  0, U, ts) \
  PARAM(X,  1, U, rank) \
  PARAM(X,  2, P, arena) \
  PARAM(X,  3, P, seg)

#define EVENT_TraceScanSingleRef_PARAMS(PARAM, X) \
  PARAM(X,  0, U, ts) \
  PARAM(X,  1, U, rank) \
  PARAM(X,  2, P, arena) \
  PARAM(X,  3, A, refIO)

#define EVENT_TraceStatCondemn_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace) \
  PARAM(X,  1, W, condemned) \
  PARAM(X,  2, W, notCondemned) \
  PARAM(X,  3, W, foundation) \
  PARAM(X,  4, W, rate) \
  PARAM(X,  5, D, mortality) \
  PARAM(X,  6, D, finishingTime)

#define EVENT_TraceStatScan_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace) \
  PARAM(X,  1, W, rootScanCount) \
  PARAM(X,  2, W, rootScanSize) \
  PARAM(X,  3, W, rootCopiedSize) \
  PARAM(X,  4, W, segScanCount) \
  PARAM(X,  5, W, segScanSize) \
  PARAM(X,  6, W, segCopiedSize) \
  PARAM(X,  7, W, singleScanCount) \
  PARAM(X,  8, W, singleScanSize) \
  PARAM(X,  9, W, singleCopiedSize) \
  PARAM(X, 10, W, readBarrierHitCount) \
  PARAM(X, 11, W, greySegMax) \
  PARAM(X, 12, W, pointlessScanCount)

#define EVENT_TraceStatFix_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace) \
  PARAM(X,  1, W, fixRefCount) \
  PARAM(X,  2, W, segRefCount) \
  PARAM(X,  3, W, whiteSegRefCount) \
  PARAM(X,  4, W, nailCount) \
  PARAM(X,  5, W, snapCount) \
  PARAM(X,  6, W, forwardedCount) \
  PARAM(X,  7, W, forwardedSize) \
  PARAM(X,  8, W, preservedInPlaceCount) \
  PARAM(X,  9, W, preservedInPlaceSize)

#define EVENT_TraceStatReclaim_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace) \
  PARAM(X,  1, W, reclaimCount) \
  PARAM(X,  2, W, reclaimSize)

#define EVENT_PoolInitMVFF_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, arena) \
  PARAM(X,  2, W, extendBy) \
  PARAM(X,  3, W, avgSize) \
  PARAM(X,  4, W, align) \
  PARAM(X,  5, B, slotHigh) \
  PARAM(X,  6, B, arenaHigh) \
  PARAM(X,  7, B, firstFit)

#define EVENT_PoolInitMV_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, arena) \
  PARAM(X,  2, W, extendBy) \
  PARAM(X,  3, W, avgSize) \
  PARAM(X,  4, W, maxSize)

#define EVENT_PoolInitMFS_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, arena) \
  PARAM(X,  2, W, extendBy) \
  PARAM(X,  3, W, unitSize)

#define EVENT_PoolInitAMS_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, arena) \
  PARAM(X,  2, P, format)

#define EVENT_PoolInitAMC_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, format)

#define EVENT_PoolInitAMCZ_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, format)

#define EVENT_PoolInitAWL_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, format)

#define EVENT_PoolInitLO_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, format)

#define EVENT_PoolInitSNC_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, P, format)

#define EVENT_PoolInitMVT_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool) \
  PARAM(X,  1, W, minSize) \
  PARAM(X,  2, W, meanSize) \
  PARAM(X,  3, W, maxSize) \
  PARAM(X,  4, W, reserveDepth) \
  PARAM(X,  5, W, fragLimig)

#define EVENT_BufferInitSeg_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, P, pool) \
  PARAM(X,  2, B, isMutator)

#define EVENT_BufferInitRank_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer) \
  PARAM(X,  1, P, pool) \
  PARAM(X,  2, B, isMutator) \
  PARAM(X,  3, U, rank)

#define EVENT_ReservoirLimitSet_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, size)

#define EVENT_CommitLimitSet_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, limit) \
  PARAM(X,  2, U, OK)

#define EVENT_SpareCommitLimitSet_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, limit)

#define EVENT_ArenaAlloc_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, P, baseTract) \
  PARAM(X,  2, A, base) \
  PARAM(X,  3, W, size) \
  PARAM(X,  4, P, pool)

#define EVENT_ArenaFree_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, A, base) \
  PARAM(X,  2, W, size)

#define EVENT_ArenaAllocFail_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, size) \
  PARAM(X,  2, P, pool)

#define EVENT_SegMerge_PARAMS(PARAM, X) \
  PARAM(X,  0, P, segLo) \
  PARAM(X,  1, P, segHi) \
  PARAM(X,  2, B, withReservoirPermit)

#define EVENT_SegSplit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg) \
  PARAM(X,  1, P, segLo) \
  PARAM(X,  2, P, segHi) \
  PARAM(X,  3, A, at)

#define EVENT_vmArenaExtendStart_PARAMS(PARAM, X) \
  PARAM(X,  0, W, size) /* size to accommodate */ \
  PARAM(X,  1, W, chunkSize) /* chunkSize to try */ \
  PARAM(X,  2, W, reserved) /* current VMArenaReserved */

#define EVENT_vmArenaExtendFail_PARAMS(PARAM, X) \
  PARAM(X,  0, W, chunkMin) /* no remaining address space chunk >= chunkMin */ \
  PARAM(X,  1, W, reserved) /* current VMArenaReserved */

#define EVENT_vmArenaExtendDone_PARAMS(PARAM, X) \
  PARAM(X,  0, W, chunkSize) /* request succeeded for chunkSize bytes */ \
  PARAM(X,  1, W, reserved) /* new VMArenaReserved */

#define EVENT_MessagesDropped_PARAMS(PARAM, X) \
  PARAM(X,  0, W, count) /* count of messages dropped */

#define EVENT_MessagesExist_PARAMS(PARAM, X)

#define EVENT_ChainCondemnAuto_PARAMS(PARAM, X) \
  PARAM(X,  0, P, chain) /* chain with gens being condemned */ \
  PARAM(X,  1, W, topCondemnedGenSerial) /* condemned gens [0..this] */ \
  PARAM(X,  2, W, genCount) /* total gens in chain */

#define EVENT_TraceFindGrey_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, ti) \
  PARAM(X,  2, P, seg) \
  PARAM(X,  3, W, rank)

#define EVENT_TraceBandAdvance_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena) \
  PARAM(X,  1, W, ti) \
  PARAM(X,  2, W, rank)


#endif /* eventdef_h */

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
