/* <code/eventdef.h> -- Event Logging Definitions
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/telemetry/>
 *
 * .desc: This file declares relationships that define the various
 * event types.  It is intended to be used with clever definitions
 * of the RELATION macro.
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

#define EVENT_VERSION_MAJOR  ((unsigned)0)
#define EVENT_VERSION_MEDIAN ((unsigned)0)
#define EVENT_VERSION_MINOR  ((unsigned)0)


/* Relations -- Generic definitions of events
 *
 * These specify:
 *   - Type: The name of the event type, without the leading "Event";
 *   - Code: The unique 16-bit code associated with this event type,
 *     not currently used (see <code/eventcom.h>);
 *   - Always: Whether this event type should appear in optimised
 *     varieties, not currently used;
 *   - Kind: Category into which this event falls, without the
 *     leading "EventKind";
 *   - Parameter Count
 *   - Format: Tuple indicating the format of the event
 *     parameters, similar to writef (Pointer, Addr, Word, Unsigned,
 *     String, Double).
 */
 
/* FIXME: Work out why not-in-use events were not in use and restore or delete them. */

#define EVENT_LIST(EVENT, X) \
  EVENT(X, AMCGenCreate                    , 0x0001, TRUE, Pool, 2, (P,P)) \
  EVENT(X, AMCGenDestroy                   , 0x0002, TRUE, Pool, 1, (P)) \
  EVENT(X, AMCInit                         , 0x0003, TRUE, Pool, 2, (P,P)) \
  EVENT(X, AMCFinish                       , 0x0004, TRUE, Pool, 1, (P)) \
  EVENT(X, ArenaCreateVM                   , 0x0005, TRUE, Arena, 3, (P,W,W)) \
  EVENT(X, ArenaCreateVMNZ                 , 0x0006, TRUE, Arena, 3, (P,W,W)) \
  EVENT(X, ArenaWriteFaults                , 0x0007, TRUE, Trace, 2, (P,W)) \
  EVENT(X, MeterInit                       , 0x0008, TRUE, Pool, 2, (P,P)) \
  EVENT(X, MeterValues                     , 0x0009, TRUE, Pool, 6, (P,D,D,W,W,W)) \
  EVENT(X, AMCScanBegin                    , 0x000a, TRUE, Seg, 3, (P,P,P)) \
  EVENT(X, AMCScanEnd                      , 0x000b, TRUE, Seg, 3, (P,P,P)) \
  EVENT(X, AMCFix                          , 0x000c, TRUE, Ref, 0, ()) \
  EVENT(X, AMCFixInPlace                   , 0x000d, TRUE, Ref, 0, ()) \
  EVENT(X, AMCFixForward                   , 0x000e, TRUE, Ref, 1, (A)) \
  EVENT(X, AMCReclaim                      , 0x000f, TRUE, Seg, 3, (P,P,P)) \
  /* EVENT(X, AMCTraceEnd                     , 0x0010, TRUE, Trace, 3, (P,P,P)) */ \
  EVENT(X, ArenaCreateCL                   , 0x0011, TRUE, Arena, 3, (P,W,A)) \
  EVENT(X, ArenaDestroy                    , 0x0012, TRUE, Arena, 1, (P)) \
  EVENT(X, SegAlloc                        , 0x0013, TRUE, Seg, 5, (P,P,A,W,P)) \
  EVENT(X, SegFree                         , 0x0014, TRUE, Seg, 2, (P,P)) \
  EVENT(X, PoolInit                        , 0x0015, TRUE, Pool, 3, (P,P,P)) \
  EVENT(X, PoolFinish                      , 0x0016, TRUE, Pool, 1, (P)) \
  EVENT(X, PoolAlloc                       , 0x0017, TRUE, Object, 3, (P,A,W)) \
  EVENT(X, PoolFree                        , 0x0018, TRUE, Object, 3, (P,A,W)) \
  EVENT(X, CBSInit                         , 0x0019, TRUE, Pool, 2, (P,P)) \
  EVENT(X, Intern                          , 0x001a, TRUE, User, 2, (W,S)) \
  EVENT(X, Label                           , 0x001b, TRUE, User, 2, (A,W)) \
  EVENT(X, TraceStart                      , 0x001c, TRUE, Trace, 3, (P,P,P)) \
  EVENT(X, TraceCreate                     , 0x001d, TRUE, Trace, 4, (P,P,P,U)) \
  EVENT(X, TraceDestroy                    , 0x001e, TRUE, Trace, 1, (P)) \
  EVENT(X, SegSetGrey                      , 0x001f, TRUE, Seg, 3, (P,P,U)) \
  EVENT(X, TraceFlipBegin                  , 0x0020, TRUE, Trace, 2, (P,P)) \
  EVENT(X, TraceFlipEnd                    , 0x0021, TRUE, Trace, 2, (P,P)) \
  EVENT(X, TraceReclaim                    , 0x0022, TRUE, Seg, 1, (P)) \
  /* EVENT(X, TraceScan                       , 0x0023, TRUE, Seg, 5, (U,U,P,P,P)) */ \
  EVENT(X, TraceAccess                     , 0x0024, TRUE, Seg, 3, (P,P,U)) \
  /* TracePoll's kind isn't really Trace, but then it isn't Seg either */ \
  EVENT(X, TracePoll                       , 0x0025, TRUE, Trace, 2, (P,P)) \
  EVENT(X, TraceFix                        , 0x0026, TRUE, Ref, 4, (P,P,A,U)) \
  EVENT(X, TraceFixSeg                     , 0x0027, TRUE, Ref, 1, (P)) \
  EVENT(X, TraceFixWhite                   , 0x0028, TRUE, Ref, 0, ()) \
  /* TraceScanArea{Tagged} abuses kind, see .kind.abuse */ \
  EVENT(X, TraceScanArea                   , 0x0029, TRUE, Seg, 3, (P,P,P)) \
  EVENT(X, TraceScanAreaTagged             , 0x002a, TRUE, Seg, 3, (P,P,P)) \
  EVENT(X, VMCreate                        , 0x002b, TRUE, Arena, 3, (P,A,A)) \
  EVENT(X, VMDestroy                       , 0x002c, TRUE, Arena, 1, (P)) \
  EVENT(X, VMMap                           , 0x002d, TRUE, Seg, 3, (P,A,A)) \
  EVENT(X, VMUnmap                         , 0x002e, TRUE, Seg, 3, (P,A,A)) \
  EVENT(X, ArenaExtend                     , 0x002f, TRUE, Arena, 3, (P,A,W)) \
  EVENT(X, ArenaRetract                    , 0x0030, TRUE, Arena, 3, (P,A,W)) \
  EVENT(X, TraceSegGreyen                  , 0x0031, TRUE, Seg, 3, (P,P,U)) \
  /* RootScanned abuses kind, see .kind.abuse */ \
  EVENT(X, RootScan                        , 0x0032, TRUE, Seg, 3, (P,W,W)) \
  /* TraceStep abuses kind, see .kind.abuse */ \
  EVENT(X, TraceStep                       , 0x0033, TRUE, Seg, 2, (P,P)) \
  EVENT(X, BufferReserve                   , 0x0034, TRUE, Object, 3, (P,A,W)) \
  EVENT(X, BufferCommit                    , 0x0035, TRUE, Object, 4, (P,A,W,A)) \
  /* BufferInit/Finish abuse kind, see .kind.abuse */ \
  EVENT(X, BufferInit                      , 0x0036, TRUE, Pool, 3, (P,P,U)) \
  EVENT(X, BufferFinish                    , 0x0037, TRUE, Pool, 1, (P)) \
  /* EVENT(X, MVTFinish                       , 0x0038, TRUE, Pool, 1, (P)) */ \
  EVENT(X, BufferFill                      , 0x0039, TRUE, Seg, 4, (P,W,A,W)) \
  EVENT(X, BufferEmpty                     , 0x003A, TRUE, Seg, 2, (P,W)) \
  EVENT(X, SegAllocFail                    , 0x003B, TRUE, Seg, 3, (P,W,P)) \
  EVENT(X, TraceScanSeg                    , 0x003C, TRUE, Seg, 4, (U,U,P,P)) \
  /* TraceScanSingleRef abuses kind, see .kind.abuse */ \
  EVENT(X, TraceScanSingleRef              , 0x003D, TRUE, Seg, 4, (U,U,P,A)) \
  EVENT(X, TraceStatCondemn                , 0x003E, TRUE, Trace, 7, (P,W,W,W,W,D,D)) \
  EVENT(X, TraceStatScan                   , 0x003F, TRUE, Trace, 13, (P,W,W,W,W,W,W,W,W,W,W,W,W)) \
  EVENT(X, TraceStatFix                    , 0x0040, TRUE, Trace, 10, (P,W,W,W,W,W,W,W,W,W)) \
  EVENT(X, TraceStatReclaim                , 0x0041, TRUE, Trace, 3, (P,W,W)) \
  EVENT(X, PoolInitMVFF                    , 0x0042, TRUE, Pool, 8, (P,P,W,W,W,U,U,U)) \
  EVENT(X, PoolInitMV                      , 0x0043, TRUE, Pool, 5, (P,P,W,W,W)) \
  EVENT(X, PoolInitMFS                     , 0x0044, TRUE, Pool, 4, (P,P,W,W)) \
  EVENT(X, PoolInitEPVM                    , 0x0045, TRUE, Pool, 5, (P,P,P,U,U)) \
  EVENT(X, PoolInitEPDL                    , 0x0046, TRUE, Pool, 6, (P,P,U,W,W,W)) \
  EVENT(X, PoolInitAMS                     , 0x0047, TRUE, Pool, 3, (P,P,P)) \
  EVENT(X, PoolInitAMC                     , 0x0048, TRUE, Pool, 2, (P,P)) \
  EVENT(X, PoolInitAMCZ                    , 0x0049, TRUE, Pool, 2, (P,P)) \
  EVENT(X, PoolInitAWL                     , 0x004A, TRUE, Pool, 2, (P,P)) \
  EVENT(X, PoolInitLO                      , 0x004B, TRUE, Pool, 2, (P,P)) \
  EVENT(X, PoolInitSNC                     , 0x004C, TRUE, Pool, 2, (P,P)) \
  EVENT(X, PoolInitMVT                     , 0x004D, TRUE, Pool, 6, (P,W,W,W,W,W)) \
  EVENT(X, BufferInitEPVM                  , 0x0050, TRUE, Pool, 3, (P,P,U)) \
  EVENT(X, BufferInitSeg                   , 0x0051, TRUE, Pool, 3, (P,P,U)) \
  EVENT(X, BufferInitRank                  , 0x0052, TRUE, Pool, 4, (P,P,U,U)) \
  /* PoolPush/Pop go under Object, because they're user ops. */ \
  EVENT(X, PoolPush                        , 0x0060, TRUE, Object, 1, (P)) \
  EVENT(X, PoolPop                         , 0x0061, TRUE, Object, 2, (P,U)) \
  EVENT(X, ReservoirLimitSet               , 0x0062, TRUE, Arena, 2, (P,W)) \
  EVENT(X, CommitLimitSet                  , 0x0063, TRUE, Arena, 3, (P,W,U)) \
  EVENT(X, SpareCommitLimitSet             , 0x0064, TRUE, Arena, 2, (P,W)) \
  EVENT(X, ArenaAlloc                      , 0x0065, TRUE, Arena, 5, (P,P,A,W,P)) \
  EVENT(X, ArenaFree                       , 0x0066, TRUE, Arena, 3, (P,A,W)) \
  EVENT(X, ArenaAllocFail                  , 0x0067, TRUE, Arena, 3, (P,W,P)) \
  EVENT(X, SegMerge                        , 0x0068, TRUE, Seg, 3, (P,P,P)) \
  EVENT(X, SegSplit                        , 0x0069, TRUE, Seg, 4, (P,P,P,A))

/* Remember to update EventNameMAX and EventCodeMAX in eventcom.h! */


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
