/* impl.h.eventdef -- Event Logging Definitions
 *
 * $HopeName: MMsrc!eventdef.h(trunk.22) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .readership: MPS developers.
 * .source: design.mps.telemetry
 *
 * .desc: This file declares relationships that define the various 
 * event types.  It is intended to be used with clever definitions 
 * of the RELATION macro. 
 *
 * TRANSGRESSIONS
 *
 * .trans.nocppguard: This file has no #ifdef guard around the entire file.
 * This is so that the file can be included multiple times.  This is
 * useful because each inclusion can use a different definition of
 * RELATION.  However this may be slightly shot by having the version
 * defined here.
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

/* No #ifndef eventdef_h, see .trans.nocppguard. */


/* EVENT_VERSION_* -- three part version number
 *
 * Increment the minor version when adding new events,
 * the median version when changing an existing event,
 * and the major version when changing the format of the event file.
 */

#define EVENT_VERSION_MAJOR  ((unsigned)0)
#define EVENT_VERSION_MEDIAN ((unsigned)0)
#define EVENT_VERSION_MINOR  ((unsigned)0)


/* Relations -- Generic definitions of events
 * 
 * These specify:
 *   - Type: The name of the event type, without the leading "Event";
 *   - Code: The unique 16-bit code associated with this event type,
 *     not currently used (see impl.h.eventcom);
 *   - Always: Whether this event type should appear in optimised 
 *     varieties, not currently used;
 *   - Kind: Category into which this event falls, without the
 *     leading "EventKind";
 *   - Format: Character sequence indicating the format of the event
 *     parameters, similar to writef (Pointer, Addr, Word, Unsigned, 
 *     String, Double).
 */

RELATION(AMCGenCreate                    , 0x0001, TRUE, Pool, PP)
RELATION(AMCGenDestroy                   , 0x0002, TRUE, Pool, P)
RELATION(AMCInit                         , 0x0003, TRUE, Pool, PP)
RELATION(AMCFinish                       , 0x0004, TRUE, Pool, P)
#if 0 /* Not in use */
RELATION(AMCBufferInit                   , 0x0005, TRUE, Pool, PP)
RELATION(AMCBufferFill                   , 0x0006, TRUE, Seg, PPWAW)
#endif
RELATION(ArenaWriteFaults                , 0x0007, TRUE, Seg, PW)
RELATION(MeterInit                       , 0x0008, TRUE, Pool, PP)
RELATION(MeterValues                     , 0x0009, TRUE, Pool, PDDWWW)
RELATION(AMCScanBegin                    , 0x000a, TRUE, Seg, PPP)
RELATION(AMCScanEnd                      , 0x000b, TRUE, Seg, PPP)
RELATION(AMCFix                          , 0x000c, TRUE, Ref, 0)
RELATION(AMCFixInPlace                   , 0x000d, TRUE, Ref, 0)
RELATION(AMCFixForward                   , 0x000e, TRUE, Ref, A)
RELATION(AMCReclaim                      , 0x000f, TRUE, Seg, PPP)
RELATION(AMCTraceEnd                     , 0x0010, TRUE, Trace, PPP)
RELATION(ArenaCreate                     , 0x0011, TRUE, Arena, PP)
RELATION(ArenaDestroy                    , 0x0012, TRUE, Arena, P)
RELATION(SegAlloc                        , 0x0013, TRUE, Seg, PPAWP)
RELATION(SegFree                         , 0x0014, TRUE, Seg, PP)
RELATION(PoolInit                        , 0x0015, TRUE, Pool, PPP)
RELATION(PoolFinish                      , 0x0016, TRUE, Pool, P)
RELATION(PoolAlloc                       , 0x0017, TRUE, Object, PAW)
RELATION(PoolFree                        , 0x0018, TRUE, Object, PAW)
RELATION(CBSInit                         , 0x0019, TRUE, Pool, PP)
RELATION(Intern                          , 0x001a, TRUE, User, WS)
RELATION(Label                           , 0x001b, TRUE, User, AW)
RELATION(TraceStart                      , 0x001c, TRUE, Trace, PPP)
#if 0 /* Not in use */
RELATION(TraceCreate                     , 0x001d, TRUE, Trace, PPPU)
#endif
RELATION(TraceDestroy                    , 0x001e, TRUE, Trace, P)
RELATION(SegSetGrey                      , 0x001f, TRUE, Seg, PPU)
RELATION(TraceFlipBegin                  , 0x0020, TRUE, Trace, PP)
RELATION(TraceFlipEnd                    , 0x0021, TRUE, Trace, PP)
RELATION(TraceReclaim                    , 0x0022, TRUE, Seg, P)
#if 0 /* not in use */
RELATION(TraceScan                       , 0x0023, TRUE, Seg, UUPPP)
#endif
RELATION(TraceAccess                     , 0x0024, TRUE, Seg, PPU)
/* TracePoll's kind isn't really Trace, but then it isn't Seg either */
RELATION(TracePoll                       , 0x0025, TRUE, Trace, PP)
RELATION(TraceFix                        , 0x0026, TRUE, Ref, PPAU)
RELATION(TraceFixSeg                     , 0x0027, TRUE, Ref, P)
RELATION(TraceFixWhite                   , 0x0028, TRUE, Ref, 0)
/* TraceScanArea{Tagged} abuses kind, see .kind.abuse */
RELATION(TraceScanArea                   , 0x0029, TRUE, Seg, PPP)
RELATION(TraceScanAreaTagged             , 0x002a, TRUE, Seg, PPP)
RELATION(VMCreate                        , 0x002b, TRUE, Arena, PAA)
RELATION(VMDestroy                       , 0x002c, TRUE, Arena, P)
RELATION(VMMap                           , 0x002d, TRUE, Seg, PAA)
RELATION(VMUnmap                         , 0x002e, TRUE, Seg, PAA)
RELATION(ArenaExtend                     , 0x002f, TRUE, Arena, PAW)
RELATION(ArenaRetract                    , 0x0030, TRUE, Arena, PAW)
RELATION(TraceSegGreyen                  , 0x0031, TRUE, Seg, PPU)
/* RootScanned abuses kind, see .kind.abuse */
RELATION(RootScan                        , 0x0032, TRUE, Seg, PWW)
/* TraceStep abuses kind, see .kind.abuse */
RELATION(TraceStep                       , 0x0033, TRUE, Seg, PP)
RELATION(BufferReserve                   , 0x0034, TRUE, Object, PAW)
RELATION(BufferCommit                    , 0x0035, TRUE, Object, PAWA)
/* BufferInit/Finish abuse kind, see .kind.abuse */
RELATION(BufferInit                      , 0x0036, TRUE, Pool, PPU)
RELATION(BufferFinish                    , 0x0037, TRUE, Pool, P)
RELATION(MV2Finish                       , 0x0038, TRUE, Pool, P)
RELATION(BufferFill                      , 0x0039, TRUE, Seg, PWAW)
RELATION(BufferEmpty                     , 0x003A, TRUE, Seg, PW)
RELATION(SegAllocFail                    , 0x003B, TRUE, Seg, PWP)
RELATION(TraceScanSeg                    , 0x003C, TRUE, Seg, UUPP)
/* TraceScanSingleRef abuses kind, see .kind.abuse */
RELATION(TraceScanSingleRef              , 0x003D, TRUE, Seg, UUPA)
RELATION(TraceStatCondemn                , 0x003E, TRUE, Trace, PWWWWDD)
RELATION(TraceStatScan                   , 0x003F, TRUE, Trace, PWWWWWWWWWWWW)
RELATION(TraceStatFix                    , 0x0040, TRUE, Trace, PWWWWWWWWW)
RELATION(TraceStatReclaim                , 0x0041, TRUE, Trace, PWW)
RELATION(ArenaAlloc                      , 0x0042, TRUE, Arena, PPAWP)
RELATION(ArenaFree                       , 0x0043, TRUE, Arena, PAW)
RELATION(ArenaAllocFail                  , 0x0044, TRUE, Arena, PWP)
RELATION(SegMerge                        , 0x0045, TRUE, Seg, PPP)
RELATION(SegSplit                        , 0x0046, TRUE, Seg, PPPA)
