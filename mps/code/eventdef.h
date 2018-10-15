/* <code/eventdef.h> -- Event Logging Definitions
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
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
 *
 * These are passed as parameters to the EventInit event at the start
 * of a telemetry stream, allowing that stream to be identified.
 */

#define EVENT_VERSION_MAJOR  ((unsigned)2)
#define EVENT_VERSION_MEDIAN ((unsigned)0)
#define EVENT_VERSION_MINOR  ((unsigned)0)


/* EVENT_LIST -- list of event types and general properties
 *
 * The columns are:
 *
 * 1. Type: The name of the event type, without the leading "Event";
 * 2. Code: The unique 16-bit code associated with this event type;
 * 3. Used: Whether this event type is used at all;
 * 4. Kind: Category into which this event falls, without the "EventKind";
 *
 * When you retire an event type, don't delete it from the list, but
 * set the "Used" column to FALSE. This serves as documentation for
 * what the event code means in older logs, and prevents the codes
 * being re-used. See <design/telemetry/#.reg.code>.
 *
 * When you add an event type, you must also add an EVENT_*_PARAMS
 * macro specifying its parameters.
 *
 * TODO: Add a doc string to each event type.
 */

#define EventNameMAX ((size_t)19)
#define EventCodeMAX ((EventCode)0x005c)

#define EVENT_LIST(EVENT, X) \
  /*       0123456789012345678 <- don't exceed without changing EventNameMAX */ \
  EVENT(X, AMCScanNailed      , 0x0001,  TRUE, Seg) \
  EVENT(X, AWLDeclineSeg      , 0x0002,  TRUE, Seg) \
  EVENT(X, AWLDeclineTotal    , 0x0003,  TRUE, Seg) \
  EVENT(X, ArenaAccessBegin   , 0x0004,  TRUE, Arena) \
  EVENT(X, ArenaAccessEnd     , 0x0005,  TRUE, Arena) \
  EVENT(X, ArenaAlloc         , 0x0006,  TRUE, Arena) \
  EVENT(X, ArenaAllocFail     , 0x0007,  TRUE, Arena) \
  EVENT(X, ArenaCreateCL      , 0x0008,  TRUE, Arena) \
  EVENT(X, ArenaCreateVM      , 0x0009,  TRUE, Arena) \
  EVENT(X, ArenaDestroy       , 0x000a,  TRUE, Arena) \
  EVENT(X, ArenaExtend        , 0x000b,  TRUE, Arena) \
  EVENT(X, ArenaFree          , 0x000c,  TRUE, Arena) \
  EVENT(X, ArenaPollBegin     , 0x000d,  TRUE, Arena) \
  EVENT(X, ArenaPollEnd       , 0x000e,  TRUE, Arena) \
  EVENT(X, ArenaSetEmergency  , 0x000f,  TRUE, Arena) \
  EVENT(X, ArenaSetSpare      , 0x0010,  TRUE, Arena) \
  EVENT(X, ArenaUseFreeZone   , 0x0011,  TRUE, Arena) \
  EVENT(X, BufferCommit       , 0x0012,  TRUE, Object) \
  EVENT(X, BufferEmpty        , 0x0013,  TRUE, Seg) \
  EVENT(X, BufferFill         , 0x0014,  TRUE, Seg) \
  EVENT(X, BufferFinish       , 0x0015,  TRUE, Pool) /* see .kind.abuse */ \
  EVENT(X, BufferInit         , 0x0016,  TRUE, Pool) /* see .kind.abuse */ \
  EVENT(X, BufferInitRank     , 0x0017,  TRUE, Pool) \
  EVENT(X, BufferInitSeg      , 0x0018,  TRUE, Pool) \
  EVENT(X, BufferReserve      , 0x0019,  TRUE, Object) \
  EVENT(X, ChainCondemnAuto   , 0x001a,  TRUE, Trace) \
  EVENT(X, CommitLimitSet     , 0x001b,  TRUE, Arena) \
  EVENT(X, EventClockSync     , 0x001c,  TRUE, Arena) \
  EVENT(X, EventInit          , 0x001d,  TRUE, Arena) \
  EVENT(X, GenFinish          , 0x001e,  TRUE, Arena) \
  EVENT(X, GenInit            , 0x001f,  TRUE, Arena) \
  EVENT(X, GenZoneSet         , 0x0020,  TRUE, Arena) \
  EVENT(X, Intern             , 0x0021,  TRUE, User) \
  EVENT(X, Label              , 0x0022,  TRUE, User) \
  EVENT(X, LabelPointer       , 0x0023,  TRUE, User) \
  EVENT(X, LandInit           , 0x0024,  TRUE, Pool) \
  EVENT(X, MessagesDropped    , 0x0025,  TRUE, Arena) \
  EVENT(X, MessagesExist      , 0x0026,  TRUE, Arena) \
  EVENT(X, MeterInit          , 0x0027,  TRUE, Pool) \
  EVENT(X, MeterValues        , 0x0028,  TRUE, Pool) \
  EVENT(X, PauseTimeSet       , 0x0029,  TRUE, Arena) \
  EVENT(X, PoolAlloc          , 0x002a,  TRUE, Object) \
  EVENT(X, PoolFinish         , 0x002b,  TRUE, Pool) \
  EVENT(X, PoolFree           , 0x002c,  TRUE, Object) \
  EVENT(X, PoolInit           , 0x002d,  TRUE, Pool) \
  EVENT(X, PoolInitAMC        , 0x002e,  TRUE, Pool) \
  EVENT(X, PoolInitAMCZ       , 0x002f,  TRUE, Pool) \
  EVENT(X, PoolInitAMS        , 0x0030,  TRUE, Pool) \
  EVENT(X, PoolInitAWL        , 0x0031,  TRUE, Pool) \
  EVENT(X, PoolInitLO         , 0x0032,  TRUE, Pool) \
  EVENT(X, PoolInitMFS        , 0x0033,  TRUE, Pool) \
  EVENT(X, PoolInitMVFF       , 0x0034,  TRUE, Pool) \
  EVENT(X, PoolInitMVT        , 0x0035,  TRUE, Pool) \
  EVENT(X, PoolInitSNC        , 0x0036,  TRUE, Pool) \
  EVENT(X, RootScan           , 0x0037,  TRUE, Seg) /* see .kind.abuse */ \
  EVENT(X, SegAlloc           , 0x0038,  TRUE, Seg) \
  EVENT(X, SegAllocFail       , 0x0039,  TRUE, Seg) \
  EVENT(X, SegFree            , 0x003a,  TRUE, Seg) \
  EVENT(X, SegMerge           , 0x003b,  TRUE, Seg) \
  EVENT(X, SegReclaim         , 0x003c,  TRUE, Seg) \
  EVENT(X, SegScan            , 0x003d,  TRUE, Seg) \
  EVENT(X, SegSetGrey         , 0x003e,  TRUE, Seg) \
  EVENT(X, SegSetSummary      , 0x003f,  TRUE, Seg) \
  EVENT(X, SegSplit           , 0x0040,  TRUE, Seg) \
  EVENT(X, TraceAccess        , 0x0041,  TRUE, Seg) \
  EVENT(X, TraceBandAdvance   , 0x0042,  TRUE, Trace) \
  EVENT(X, TraceCondemnAll    , 0x0043,  TRUE, Trace) \
  EVENT(X, TraceCreate        , 0x0044,  TRUE, Trace) \
  EVENT(X, TraceCreatePoolGen , 0x0045,  TRUE, Trace) \
  EVENT(X, TraceDestroy       , 0x0046,  TRUE, Trace) \
  EVENT(X, TraceEndGen        , 0x0047,  TRUE, Trace) \
  EVENT(X, TraceFindGrey      , 0x0048,  TRUE, Seg) \
  EVENT(X, TraceFix           , 0x0049,  TRUE, Ref) \
  EVENT(X, TraceFixSeg        , 0x004a,  TRUE, Ref) \
  EVENT(X, TraceFlipBegin     , 0x004b,  TRUE, Trace) \
  EVENT(X, TraceFlipEnd       , 0x004c,  TRUE, Trace) \
  EVENT(X, TraceReclaim       , 0x004d,  TRUE, Trace) \
  EVENT(X, TraceScanArea      , 0x004e,  TRUE, Seg) /* see .kind.abuse */ \
  EVENT(X, TraceScanAreaTagged, 0x004f,  TRUE, Seg) /* see .kind.abuse */ \
  EVENT(X, TraceScanSingleRef , 0x0050,  TRUE, Seg) /* see .kind.abuse */ \
  EVENT(X, TraceStart         , 0x0051,  TRUE, Trace) \
  EVENT(X, TraceStatFix       , 0x0052,  TRUE, Trace) \
  EVENT(X, TraceStatReclaim   , 0x0053,  TRUE, Trace) \
  EVENT(X, TraceStatScan      , 0x0054,  TRUE, Trace) \
  EVENT(X, VMArenaExtendDone  , 0x0055,  TRUE, Arena) \
  EVENT(X, VMArenaExtendFail  , 0x0056,  TRUE, Arena) \
  EVENT(X, VMArenaExtendStart , 0x0057,  TRUE, Arena) \
  EVENT(X, VMCompact          , 0x0058,  TRUE, Arena) \
  EVENT(X, VMFinish           , 0x0059,  TRUE, Arena) \
  EVENT(X, VMInit             , 0x005a,  TRUE, Arena) \
  EVENT(X, VMMap              , 0x005b,  TRUE, Seg) \
  EVENT(X, VMUnmap            , 0x005c,  TRUE, Seg)


/* Remember to update EventNameMAX and EventCodeMAX above!
   (These are checked in EventInit.) */


/* EVENT_*_PARAMS -- definition of event parameters
 *
 * For each event type in EVENT_LIST, these macros list the parameters of
 * the event.  The columns are:
 *
 * 1. index (used to define numeric field names);
 * 2. sort (Pointer, Addr, Word, Unsigned, String, Double, Bool);
 * 3. identifier (for display or use in code).
 * 4. documentation.
 */

#define EVENT_AMCScanNailed_PARAMS(PARAM, X) \
  PARAM(X,  0, W, loops, "number of times around the loop") \
  PARAM(X,  1, W, summary, "summary of segment being scanned") \
  PARAM(X,  2, W, white, "scan state white set") \
  PARAM(X,  3, W, unfixed, "scan state unfixed summary") \
  PARAM(X,  4, W, fixed, "scan state fixed summary") \
  PARAM(X,  5, W, refset, "scan state refset")

#define EVENT_AWLDeclineSeg_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg, "segment declined single access") \
  PARAM(X,  1, W, singleAccesses, "single accesses this cycle")

#define EVENT_AWLDeclineTotal_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg, "segment declined single access") \
  PARAM(X,  1, W, succAccesses, "total successive accesses")

#define EVENT_ArenaAccessBegin_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, P, addr, "address that was accessed") \
  PARAM(X,  2, U, mode, "set of access modes")

#define EVENT_ArenaAccessEnd_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena")

#define EVENT_ArenaAlloc_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, P, baseTract, "first allocated tract") \
  PARAM(X,  2, A, base, "base of the allocated block") \
  PARAM(X,  3, W, size, "size of the allocated block in bytes") \
  PARAM(X,  4, P, pool, "pool that requested the allocation")

#define EVENT_ArenaAllocFail_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, W, size, "requested allocation size") \
  PARAM(X,  2, P, pool, "pool that requested allocation") \
  PARAM(X,  3, U, res, "result code")

#define EVENT_ArenaCreateCL_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, W, size, "size of memory given to arena in bytes") \
  PARAM(X,  2, A, base, "base address of memory given to arena") \
  PARAM(X,  3, W, grainSize, "arena's grain size in bytes") \
  PARAM(X,  4, P, arenaClass, "arena's class") \
  PARAM(X,  5, W, systemPools, "number of system pools") \
  PARAM(X,  6, U, serial, "arena's serial number")

#define EVENT_ArenaCreateVM_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, W, userSize, "requested address space in bytes") \
  PARAM(X,  2, W, chunkSize, "arena's chunk size in bytes") \
  PARAM(X,  3, W, grainSize, "arena's grain size in bytes") \
  PARAM(X,  4, P, arenaClass, "arena's class") \
  PARAM(X,  5, W, systemPools, "number of system pools") \
  PARAM(X,  6, U, serial, "arena's serial number")

#define EVENT_ArenaDestroy_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena")

#define EVENT_ArenaExtend_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, A, base, "base of new chunk") \
  PARAM(X,  2, W, size, "size of new chunk")

#define EVENT_ArenaFree_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, A, base, "base of the freed block") \
  PARAM(X,  2, W, size, "size of the freed block in bytes") \
  PARAM(X,  3, P, pool, "pool that freed the block")

#define EVENT_ArenaPollBegin_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "arena about to be polled")

#define EVENT_ArenaPollEnd_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "arena that was polled") \
  PARAM(X,  1, B, workWasDone, "any collection work done in poll?")

#define EVENT_ArenaSetEmergency_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, B, emergency, "emergency mode?")

#define EVENT_ArenaSetSpare_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, D, spare, "spare committed fraction")

#define EVENT_ArenaUseFreeZone_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, W, zoneSet, "zones that aren't free any longer")

#define EVENT_BufferCommit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, A, p, "committed object") \
  PARAM(X,  2, W, size, "size of committed object") \
  PARAM(X,  3, A, clientClass, "format's class of 0 if no format")

#define EVENT_BufferEmpty_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, W, spare, "remaining free memory")

#define EVENT_BufferFill_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, W, size, "size of client request") \
  PARAM(X,  2, A, base, "base of pool allocation") \
  PARAM(X,  3, W, filled, "size of pool allocation")

#define EVENT_BufferFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer")

#define EVENT_BufferInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, P, pool, "buffer's pool") \
  PARAM(X,  2, B, isMutator, "belongs to client program?")

#define EVENT_BufferInitRank_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, P, pool, "buffer's pool") \
  PARAM(X,  2, B, isMutator, "belongs to client program?") \
  PARAM(X,  3, U, rank, "rank of references in buffer")

#define EVENT_BufferInitSeg_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, P, pool, "buffer's pool") \
  PARAM(X,  2, B, isMutator, "belongs to client program?")

#define EVENT_BufferReserve_PARAMS(PARAM, X) \
  PARAM(X,  0, P, buffer, "the buffer") \
  PARAM(X,  1, A, init, "buffer's init pointer") \
  PARAM(X,  2, W, size, "size of client request")

#define EVENT_ChainCondemnAuto_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "chain's arena") \
  PARAM(X,  1, P, chain, "chain with gens being condemned") \
  PARAM(X,  2, P, trace, "trace for which gens condemned") \
  PARAM(X,  3, W, topCondemnedGenIndex, "condemned gens [0..this]") \
  PARAM(X,  4, W, genCount, "total gens in chain")

#define EVENT_CommitLimitSet_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, W, limit, "new commit limit") \
  PARAM(X,  2, U, res, "result code")

#define EVENT_EventClockSync_PARAMS(PARAM, X) \
  PARAM(X,  0, W, clock, "mps_clock() value")

#define EVENT_EventInit_PARAMS(PARAM, X) \
  PARAM(X,  0, U, major, "EVENT_VERSION_MAJOR") \
  PARAM(X,  1, U, median, "EVENT_VERSION_MEDIAN") \
  PARAM(X,  2, U, minor, "EVENT_VERSION_MINOR") \
  PARAM(X,  3, U, maxCode, "EventCodeMAX") \
  PARAM(X,  4, U, maxNameLen, "EventNameMAX") \
  PARAM(X,  5, U, wordWidth, "MPS_WORD_WIDTH") \
  PARAM(X,  6, W, clocksPerSec, "mps_clocks_per_sec()")

#define EVENT_GenFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "generation's arena") \
  PARAM(X,  1, P, gen, "the generation") \
  PARAM(X,  2, U, serial, "serial number within arena")

#define EVENT_GenInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "generation's arena") \
  PARAM(X,  1, P, gen, "the generation") \
  PARAM(X,  2, U, serial, "serial number within arena") \
  PARAM(X,  3, W, capacity, "capacity in bytes") \
  PARAM(X,  4, D, mortality, "initial mortality estimate")

#define EVENT_GenZoneSet_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "generation's arena") \
  PARAM(X,  1, P, gen, "the generation") \
  PARAM(X,  2, W, zoneSet, "generation's new summary")

#define EVENT_Intern_PARAMS(PARAM, X) \
  PARAM(X,  0, W, stringId, "identifier of interned string") \
  PARAM(X,  1, S, string, "the interned string")

#define EVENT_Label_PARAMS(PARAM, X) \
  PARAM(X,  0, A, address, "address") \
  PARAM(X,  1, W, stringId, "string identifier of its label")

#define EVENT_LabelPointer_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pointer, "pointer") \
  PARAM(X,  1, W, stringId, "string identifier of its label")

#define EVENT_LandInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, land, "the land") \
  PARAM(X,  1, P, owner, "owner pointer")

#define EVENT_MessagesDropped_PARAMS(PARAM, X) \
  PARAM(X,  0, W, count, "count of messages dropped")

#define EVENT_MessagesExist_PARAMS(PARAM, X)

#define EVENT_MeterInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, meter, "the meter") \
  PARAM(X,  1, P, owner, "owner pointer")

#define EVENT_MeterValues_PARAMS(PARAM, X) \
  PARAM(X,  0, P, meter, "the meter") \
  PARAM(X,  1, D, total, "sum of metered amounts") \
  PARAM(X,  2, D, meanSquared, "mean square of metered amounts") \
  PARAM(X,  3, W, count, "number of metered amounts") \
  PARAM(X,  4, W, max, "maximum metered amount") \
  PARAM(X,  5, W, min, "minimum metered amount")

#define EVENT_PauseTimeSet_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, D, pauseTime, "the new maximum pause time, in seconds")

#define EVENT_PoolAlloc_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, A, pReturn, "base of allocated memory") \
  PARAM(X,  2, W, size, "size of client request")

#define EVENT_PoolFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, arena, "pool's arena")

#define EVENT_PoolFree_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, A, old, "base of freed memory") \
  PARAM(X,  2, W, size, "size of client request")

#define EVENT_PoolInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, arena, "pool's arena") \
  PARAM(X,  2, P, poolClass, "pool's class") \
  PARAM(X,  3, U, serial, "pool's serial number within the arena")

#define EVENT_PoolInitAMCZ_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, format, "pool's format")

#define EVENT_PoolInitAMC_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, format, "pool's format")

#define EVENT_PoolInitAMS_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, format, "pool's format")

#define EVENT_PoolInitAWL_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, format, "pool's format")

#define EVENT_PoolInitLO_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, format, "pool's format")

#define EVENT_PoolInitMFS_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, W, extendBy, "size of extents") \
  PARAM(X,  2, B, extendSelf, "automatically extend?") \
  PARAM(X,  3, W, unitSize, "size of allocations")

#define EVENT_PoolInitMVFF_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, W, extendBy, "size of extents") \
  PARAM(X,  2, W, avgSize, "predicted mean size of blocks") \
  PARAM(X,  3, W, align, "alignment of blocks") \
  PARAM(X,  4, B, slotHigh, "allocate at high addresses within extents?") \
  PARAM(X,  5, B, arenaHigh, "allocate extents at high addresses?") \
  PARAM(X,  6, B, firstFit, "allocate from first free block?")

#define EVENT_PoolInitMVT_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, W, minSize, "predicted minimum size of blocks") \
  PARAM(X,  2, W, meanSize, "predicted mean size of blocks") \
  PARAM(X,  3, W, maxSize, "predicted maximum size of blocks") \
  PARAM(X,  4, W, reserveDepth, "reserve space for this many allocations") \
  PARAM(X,  5, W, fragLimig, "fragmentation limit")

#define EVENT_PoolInitSNC_PARAMS(PARAM, X) \
  PARAM(X,  0, P, pool, "the pool") \
  PARAM(X,  1, P, format, "pool's format")

#define EVENT_RootScan_PARAMS(PARAM, X) \
  PARAM(X,  0, P, root, "the root") \
  PARAM(X,  1, W, ts, "scanning for this set of traces") \
  PARAM(X,  2, W, summary, "summary after scan")

#define EVENT_SegAlloc_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, P, seg, "new segment") \
  PARAM(X,  2, A, base, "base address") \
  PARAM(X,  3, W, size, "segment size") \
  PARAM(X,  4, P, pool, "pool making request")

#define EVENT_SegAllocFail_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, W, size, "requested segment size") \
  PARAM(X,  2, P, pool, "pool making request") \
  PARAM(X,  3, U, res, "result code")

#define EVENT_SegFree_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "segment's arena") \
  PARAM(X,  1, P, seg, "the segment")

#define EVENT_SegMerge_PARAMS(PARAM, X) \
  PARAM(X,  0, P, segLo, "low segment") \
  PARAM(X,  1, P, segHi, "high segment")

#define EVENT_SegReclaim_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg, "the segment") \
  PARAM(X,  1, P, pool, "segment's pool") \
  PARAM(X,  2, P, arena, "pool's arena") \
  PARAM(X,  3, P, trace, "reclaiming for this trace")

#define EVENT_SegScan_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg, "the segment") \
  PARAM(X,  1, P, pool, "segment's pool") \
  PARAM(X,  2, P, arena, "pool's arena") \
  PARAM(X,  3, U, ts, "scanning for this set of traces") \
  PARAM(X,  4, U, rank, "scanning at this rank")

#define EVENT_SegSetGrey_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "segment's arena") \
  PARAM(X,  1, P, seg, "the segment") \
  PARAM(X,  2, U, grey, "greyen for this set of traces")

#define EVENT_SegSetSummary_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "segment's arena") \
  PARAM(X,  1, P, seg, "the segment") \
  PARAM(X,  2, W, size, "its size in bytes") \
  PARAM(X,  3, W, oldSummary, "old summary") \
  PARAM(X,  4, W, newSummary, "new summary")

#define EVENT_SegSplit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg, "old segment") \
  PARAM(X,  1, P, segLo, "new low segment") \
  PARAM(X,  2, P, segHi, "new high segment") \
  PARAM(X,  3, A, at, "split address")

#define EVENT_TraceAccess_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, P, seg, "segment accessed") \
  PARAM(X,  2, U, mode, "set of access modes")

#define EVENT_TraceBandAdvance_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, P, trace, "the trace") \
  PARAM(X,  2, W, rank, "new rank")

#define EVENT_TraceCondemnAll_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "trace's arena") \
  PARAM(X,  1, P, trace, "trace")

#define EVENT_TraceCreate_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "trace that was created") \
  PARAM(X,  1, P, arena, "arena in which created") \
  PARAM(X,  2, U, why, "reason for creation")

#define EVENT_TraceCreatePoolGen_PARAMS(PARAM, X) \
  PARAM(X,  0, P, gen, "generation") \
  PARAM(X,  1, W, capacity, "capacity of generation") \
  PARAM(X,  2, D, mortality, "mortality of generation") \
  PARAM(X,  3, W, zone, "zone set of generation") \
  PARAM(X,  4, P, pool, "pool") \
  PARAM(X,  5, W, totalSize, "total size of pool gen") \
  PARAM(X,  6, W, freeSize, "free size of pool gen") \
  PARAM(X,  7, W, newSize, "new size of pool gen") \
  PARAM(X,  8, W, oldSize, "old size of pool gen") \
  PARAM(X,  9, W, newDeferredSize, "new size (deferred) of pool gen") \
  PARAM(X, 10, W, oldDeferredSize, "old size (deferred) of pool gen")

#define EVENT_TraceDestroy_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "trace's arena") \
  PARAM(X,  1, P, trace, "the trace")

#define EVENT_TraceEndGen_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "the arena") \
  PARAM(X,  1, P, trace, "the trace") \
  PARAM(X,  2, P, gen, "the generation") \
  PARAM(X,  3, W, condemned, "bytes condemned in generation") \
  PARAM(X,  4, W, forwarded, "bytes forwarded from generation") \
  PARAM(X,  5, W, preservedInPlace, "bytes preserved in generation") \
  PARAM(X,  6, D, mortalityTrace, "mortality (in last trace only)") \
  PARAM(X,  7, D, mortalityAverage, "mortality (moving average)")

#define EVENT_TraceFindGrey_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "trace's arena") \
  PARAM(X,  1, P, trace, "the trace") \
  PARAM(X,  2, P, seg, "grey segment found") \
  PARAM(X,  3, W, rank, "current rank")

#define EVENT_TraceFix_PARAMS(PARAM, X) \
  PARAM(X,  0, P, ss, "the scan state") \
  PARAM(X,  1, P, refIO, "pointer to reference") \
  PARAM(X,  2, A, ref, "reference fixed") \
  PARAM(X,  3, U, rank, "current rank")

#define EVENT_TraceFixSeg_PARAMS(PARAM, X) \
  PARAM(X,  0, P, seg, "the segment")

#define EVENT_TraceFlipBegin_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "the trace") \
  PARAM(X,  1, P, arena, "trace's arena")

#define EVENT_TraceFlipEnd_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "the trace") \
  PARAM(X,  1, P, arena, "trace's arena")

#define EVENT_TraceReclaim_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "the trace") \
  PARAM(X,  1, P, arena, "trace's arena")

#define EVENT_TraceScanArea_PARAMS(PARAM, X) \
  PARAM(X,  0, P, ss, "the scan state") \
  PARAM(X,  1, P, base, "base of scanned area") \
  PARAM(X,  2, P, limit, "limit of scanned area")

#define EVENT_TraceScanAreaTagged_PARAMS(PARAM, X) \
  PARAM(X,  0, P, ss, "the scan state") \
  PARAM(X,  1, P, base, "base of scanned area") \
  PARAM(X,  2, P, limit, "limit of scanned area")

#define EVENT_TraceScanSingleRef_PARAMS(PARAM, X) \
  PARAM(X,  0, U, ts, "set of traces") \
  PARAM(X,  1, U, rank, "current rank") \
  PARAM(X,  2, P, arena, "traces' arena") \
  PARAM(X,  3, P, refIO, "pointer to reference")

#define EVENT_TraceStart_PARAMS(PARAM, X) \
  PARAM(X,  0, P, arena, "trace's arena") \
  PARAM(X,  1, P, trace, "trace being started") \
  PARAM(X,  2, D, mortality, "as passed to TraceStart") \
  PARAM(X,  3, D, finishingTime, "as passed to TraceStart") \
  PARAM(X,  4, W, condemned, "condemned bytes") \
  PARAM(X,  5, W, notCondemned, "collectible but not condemned bytes") \
  PARAM(X,  6, W, foundation, "foundation size") \
  PARAM(X,  7, W, white, "white reference set") \
  PARAM(X,  8, W, quantumWork, "tracing work to be done in each poll")

#define EVENT_TraceStatFix_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "the trace") \
  PARAM(X,  1, P, arena, "trace's arena") \
  PARAM(X,  2, W, fixRefCount, "references which pass zone check") \
  PARAM(X,  3, W, segRefCount, "referencess which refer to segments") \
  PARAM(X,  4, W, whiteSegRefCount, "references which refer to white segments") \
  PARAM(X,  5, W, nailCount, "segments nailed by ambiguous references") \
  PARAM(X,  6, W, snapCount, "references snapped to forwarded objects") \
  PARAM(X,  7, W, forwardedCount, "objects preserved by moving") \
  PARAM(X,  8, W, forwardedSize, "bytes preserved by moving") \
  PARAM(X,  9, W, preservedInPlaceCount, "objects preserved in place") \
  PARAM(X, 10, W, preservedInPlaceSize, "bytes preserved in place")

#define EVENT_TraceStatReclaim_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "the trace") \
  PARAM(X,  1, P, arena, "trace's arena") \
  PARAM(X,  2, W, reclaimCount, "segments reclaimed") \
  PARAM(X,  3, W, reclaimSize, "bytes reclaimed")

#define EVENT_TraceStatScan_PARAMS(PARAM, X) \
  PARAM(X,  0, P, trace, "the trace") \
  PARAM(X,  1, P, arena, "trace's arena") \
  PARAM(X,  2, W, rootScanCount, "number of roots scanned") \
  PARAM(X,  3, W, rootScanSize, "total size of scanned roots") \
  PARAM(X,  4, W, rootCopiedSize, "bytes copied by scanning roots ") \
  PARAM(X,  5, W, segScanCount, "number of segments scanned") \
  PARAM(X,  6, W, segScanSize, "total size of scanned segments") \
  PARAM(X,  7, W, segCopiedSize, "bytes copied by scanning segments") \
  PARAM(X,  8, W, singleScanCount, "number of single references scanned") \
  PARAM(X,  9, W, singleScanSize, "total size of single references scanned") \
  PARAM(X, 10, W, singleCopiedSize, "bytes copied by scanning single references") \
  PARAM(X, 11, W, readBarrierHitCount, "read barrier faults") \
  PARAM(X, 12, W, greySegMax, "maximum number of grey segments") \
  PARAM(X, 13, W, pointlessScanCount, "pointless segment scans")

#define EVENT_VMArenaExtendDone_PARAMS(PARAM, X) \
  PARAM(X,  0, W, chunkSize, "request succeeded for chunkSize bytes") \
  PARAM(X,  1, W, reserved, "new VMArenaReserved")

#define EVENT_VMArenaExtendFail_PARAMS(PARAM, X) \
  PARAM(X,  0, W, chunkMin, "no remaining address space chunk >= chunkMin") \
  PARAM(X,  1, W, reserved, "current VMArenaReserved")

#define EVENT_VMArenaExtendStart_PARAMS(PARAM, X) \
  PARAM(X,  0, W, size, "size to accommodate") \
  PARAM(X,  1, W, chunkSize, "chunkSize to try") \
  PARAM(X,  2, W, reserved, "current VMArenaReserved")

#define EVENT_VMCompact_PARAMS(PARAM, X) \
  PARAM(X,  0, W, vmem0, "pre-collection reserved size") \
  PARAM(X,  1, W, vmem1, "pre-compact reseved size") \
  PARAM(X,  2, W, vmem2, "post-compact reserved size")

#define EVENT_VMFinish_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm, "the VM")

#define EVENT_VMInit_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm, "the VM") \
  PARAM(X,  1, A, base, "base of VM") \
  PARAM(X,  2, A, limit, "limit of VM")

#define EVENT_VMMap_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm, "the VM") \
  PARAM(X,  1, A, base, "base of mapped addresses") \
  PARAM(X,  2, A, limit, "limit of mapped addresses")

#define EVENT_VMUnmap_PARAMS(PARAM, X) \
  PARAM(X,  0, P, vm, "the VM") \
  PARAM(X,  1, A, base, "base of unmapped addresses") \
  PARAM(X,  2, A, limit, "limit of unmapped addresses")

#endif /* eventdef_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
