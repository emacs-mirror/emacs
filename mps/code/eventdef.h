/* impl.h.eventdef -- Event Logging Definitions
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
RELATION(ArenaCreateVM                   , 0x0005, TRUE, Arena, PWW)
RELATION(ArenaCreateVMNZ                 , 0x0006, TRUE, Arena, PWW)
RELATION(ArenaWriteFaults                , 0x0007, TRUE, Trace, PW)
RELATION(MeterInit                       , 0x0008, TRUE, Pool, PP)
RELATION(MeterValues                     , 0x0009, TRUE, Pool, PDDWWW)
RELATION(AMCScanBegin                    , 0x000a, TRUE, Seg, PPP)
RELATION(AMCScanEnd                      , 0x000b, TRUE, Seg, PPP)
RELATION(AMCFix                          , 0x000c, TRUE, Ref, 0)
RELATION(AMCFixInPlace                   , 0x000d, TRUE, Ref, 0)
RELATION(AMCFixForward                   , 0x000e, TRUE, Ref, A)
RELATION(AMCReclaim                      , 0x000f, TRUE, Seg, PPP)
#if 0 /* Not in use */
RELATION(AMCTraceEnd                     , 0x0010, TRUE, Trace, PPP)
#endif
RELATION(ArenaCreateCL                   , 0x0011, TRUE, Arena, PWA)
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
#if 0 /* not in use */
RELATION(MVTFinish                       , 0x0038, TRUE, Pool, P)
#endif
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

RELATION(PoolInitMVFF                    , 0x0042, TRUE, Pool, PPWWWUUU)
RELATION(PoolInitMV                      , 0x0043, TRUE, Pool, PPWWW)
RELATION(PoolInitMFS                     , 0x0044, TRUE, Pool, PPWW)
RELATION(PoolInitEPVM                    , 0x0045, TRUE, Pool, PPPUU)
RELATION(PoolInitEPDL                    , 0x0046, TRUE, Pool, PPUWWW)
RELATION(PoolInitAMS                     , 0x0047, TRUE, Pool, PPP)
RELATION(PoolInitAMC                     , 0x0048, TRUE, Pool, PP)
RELATION(PoolInitAMCZ                    , 0x0049, TRUE, Pool, PP)
RELATION(PoolInitAWL                     , 0x004A, TRUE, Pool, PP)
RELATION(PoolInitLO                      , 0x004B, TRUE, Pool, PP)
RELATION(PoolInitSNC                     , 0x004C, TRUE, Pool, PP)
RELATION(PoolInitMVT                     , 0x004D, TRUE, Pool, PWWWWW)

RELATION(BufferInitEPVM                  , 0x0050, TRUE, Pool, PPU)
RELATION(BufferInitSeg                   , 0x0051, TRUE, Pool, PPU)
RELATION(BufferInitRank                  , 0x0052, TRUE, Pool, PPUU)

/* PoolPush/Pop go under Object, because they're user ops. */
RELATION(PoolPush                        , 0x0060, TRUE, Object, P)
RELATION(PoolPop                         , 0x0061, TRUE, Object, PU)
RELATION(ReservoirLimitSet               , 0x0062, TRUE, Arena, PW)
RELATION(CommitLimitSet                  , 0x0063, TRUE, Arena, PWU)
RELATION(SpareCommitLimitSet             , 0x0064, TRUE, Arena, PW)
RELATION(ArenaAlloc                      , 0x0065, TRUE, Arena, PPAWP)
RELATION(ArenaFree                       , 0x0066, TRUE, Arena, PAW)
RELATION(ArenaAllocFail                  , 0x0067, TRUE, Arena, PWP)
RELATION(SegMerge                        , 0x0068, TRUE, Seg, PPP)
RELATION(SegSplit                        , 0x0069, TRUE, Seg, PPPA)

/* Remember to update EventNameMAX and EventCodeMAX in eventcom.h! */


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
