/* impl.h.eventdef -- Event Logging Definitions
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!eventdef.h(MMdevel_event_string.2) $
 *
 * .readership: MPS developers.
 * .source: design.mps.telemetry
 *
 * .desc: This file declares relationships that define the various 
 * event types.  It is intended to be used with clever definitions 
 * of the RELATION macro. 
 */

/* No protection because file is re-entrant. */


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
RELATION(AMCBufferInit                   , 0x0005, TRUE, Object, PP)
RELATION(AMCBufferFill                   , 0x0006, TRUE, Object, PPWAW)
RELATION(AMCBufferEmpty                  , 0x0007, TRUE, Object, PPW)
RELATION(AMCTraceBegin                   , 0x0008, TRUE, Trace, PPPP)
RELATION(AMCCondemn                      , 0x0009, TRUE, Seg, PPPP)
RELATION(AMCScanBegin                    , 0x000a, TRUE, Trace, PPP)
RELATION(AMCScanEnd                      , 0x000b, TRUE, Trace, PPP)
RELATION(AMCFix                          , 0x000c, TRUE, Ref, 0)
RELATION(AMCFixAmbig                     , 0x000d, TRUE, Ref, 0)
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
RELATION(SpaceCreate                     , 0x001a, TRUE, Arena, P)
RELATION(SpaceDestroy                    , 0x001b, TRUE, Arena, P)
RELATION(TraceStart                      , 0x001c, TRUE, Trace, PPP)
RELATION(TraceCreate                     , 0x001d, TRUE, Trace, PPPU)
RELATION(TraceDestroy                    , 0x001e, TRUE, Trace, P)
RELATION(TraceSegGreyen                  , 0x001f, TRUE, Trace, PPU)
RELATION(TraceFlipBegin                  , 0x0020, TRUE, Trace, PP)
RELATION(TraceFlipEnd                    , 0x0021, TRUE, Trace, PP)
RELATION(TraceReclaim                    , 0x0022, TRUE, Trace, P)
RELATION(TraceScan                       , 0x0023, TRUE, Trace, UUPPP)
RELATION(TraceAccess                     , 0x0024, TRUE, Seg, PPU)
RELATION(TracePoll                       , 0x0025, TRUE, Trace, PP)
RELATION(TraceFix                        , 0x0026, TRUE, Ref, PPAU)
RELATION(TraceFixSeg                     , 0x0027, TRUE, Ref, P)
RELATION(TraceFixWhite                   , 0x0028, TRUE, Ref, 0)
RELATION(TraceScanArea                   , 0x0029, TRUE, Trace, PPP)
RELATION(TraceScanAreaTagged             , 0x002a, TRUE, Trace, PPP)
RELATION(VMCreate                        , 0x002b, TRUE, Arena, PPAA)
RELATION(VMDestroy                       , 0x002c, TRUE, Arena, P)
RELATION(VMMap                           , 0x002d, TRUE, Seg, PAA)
RELATION(VMUnmap                         , 0x002e, TRUE, Seg, PAA)
