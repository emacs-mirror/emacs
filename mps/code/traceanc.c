/* traceanc.c: ANCILLARY SUPPORT FOR TRACER
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.
 * See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * All this ancillary stuff was making trace.c very cluttered.
 * Put it here instead.  RHSK 2008-12-09.
 *
 * CONTENTS
 *
 *   - TraceStartMessage.  Posted when a trace starts.
 *
 *   - TraceMessage.  Posted when a trace ends.
 *
 *   - TraceIdMessages.  Pre-allocated messages for traceid.
 *
 *   - ArenaRelease, ArenaClamp, ArenaPark.
 */

#include "mpm.h"



/* --------  TraceStartMessage  -------- */


/* TraceStartMessage -- posted when a trace starts
 *
 * Internal names:
 *   trace start
 *   TraceStartMessage, tsMessage (struct *)
 *   MessageTypeGCSTART (enum)
 *
 * External names:
 *   mps_message_type_gc_start (enum macro)
 *   MPS_MESSAGE_TYPE_GC_START (enum)
 *
 * (Note: this should properly be called "trace begin", but it's much
 * too late to change it now!)
 *
 * <design/message-gc>.
 */

#define TraceStartMessageSig ((Sig)0x51926535) /* SIGnature TRaceStartMeSsage */

/* .whybuf:
 * .whybuf.len: Length (in chars) of a char buffer used to store the
 * reason why a collection started in the TraceStartMessageStruct
 * (used by mps_message_type_gc_start).  If the reason is too long to
 * fit, it must be truncated.
 * .whybuf.nul: Check insists that the last char in the array is NUL
 * (even if there is another NUL earlier in the buffer); this makes
 * the NUL-termination check fast.
 */
#define TRACE_START_MESSAGE_WHYBUF_LEN 128

typedef struct TraceStartMessageStruct {
  Sig sig;
  char why[TRACE_START_MESSAGE_WHYBUF_LEN];  /* .whybuf */
  MessageStruct messageStruct;
} TraceStartMessageStruct;

#define TraceStartMessageMessage(traceStartMessage) \
  (&((traceStartMessage)->messageStruct))
#define MessageTraceStartMessage(message) \
  (PARENT(TraceStartMessageStruct, messageStruct, message))

Bool TraceStartMessageCheck(TraceStartMessage tsMessage)
{
  CHECKS(TraceStartMessage, tsMessage);
  CHECKD(Message, TraceStartMessageMessage(tsMessage));
  CHECKL(MessageGetType(TraceStartMessageMessage(tsMessage)) ==
         MessageTypeGCSTART);

  /* Check that why is NUL terminated.  See .whybuf.nul */
  CHECKL(tsMessage->why[NELEMS(tsMessage->why)-1] == '\0');

  return TRUE;
}

static void TraceStartMessageDelete(Message message)
{
  TraceStartMessage tsMessage;
  Arena arena;

  AVERT(Message, message);
  tsMessage = MessageTraceStartMessage(message);
  AVERT(TraceStartMessage, tsMessage);

  arena = MessageArena(message);
  tsMessage->sig = SigInvalid;
  MessageFinish(message);

  ControlFree(arena, (void *)tsMessage, sizeof(TraceStartMessageStruct));
}

static const char *TraceStartMessageWhy(Message message)
{
  TraceStartMessage tsMessage;

  AVERT(Message, message);
  tsMessage = MessageTraceStartMessage(message);
  AVERT(TraceStartMessage, tsMessage);

  return tsMessage->why;
}

static MessageClassStruct TraceStartMessageClassStruct = {
  MessageClassSig,               /* sig */
  "TraceGCStart",                /* name */
  MessageTypeGCSTART,            /* Message Type */
  TraceStartMessageDelete,       /* Delete */
  MessageNoFinalizationRef,      /* FinalizationRef */
  MessageNoGCLiveSize,           /* GCLiveSize */
  MessageNoGCCondemnedSize,      /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize,   /* GCNotCondemnedSize */
  TraceStartMessageWhy,          /* GCStartWhy */
  MessageClassSig                /* <design/message#.class.sig.double> */
};

static void traceStartMessageInit(Arena arena, TraceStartMessage tsMessage)
{
  AVERT(Arena, arena);

  MessageInit(arena, TraceStartMessageMessage(tsMessage),
              &TraceStartMessageClassStruct, MessageTypeGCSTART);
  tsMessage->why[0] = '\0';
  tsMessage->why[NELEMS(tsMessage->why)-1] = '\0';  /* .whybuf.nul */

  tsMessage->sig = TraceStartMessageSig;
  AVERT(TraceStartMessage, tsMessage);
}

/* TraceStartWhyToString -- why-code to text
 *
 * Converts a TraceStartWhy* code into a constant string describing
 * why a trace started.
 */

const char *TraceStartWhyToString(TraceStartWhy why)
{
  const char *r;

  switch (why) {
#define X(WHY, SHORT, LONG) case TraceStartWhy ## WHY: r = (LONG); break;
    TRACE_START_WHY_LIST(X)
#undef X
  default:
    NOTREACHED;
    r = "Unknown reason (internal error).";
    break;
  }

  /* Must fit in buffer without truncation; see .whybuf.len */
  AVER(StringLength(r) < TRACE_START_MESSAGE_WHYBUF_LEN);

  return r;
}


/* traceStartWhyToTextBuffer
 *
 * Converts a TraceStartWhy* code into a string describing why a trace
 * started, and copies that into the text buffer the caller provides.
 * s specifies the beginning of the buffer to write the string
 * into, len specifies the length of the buffer.
 * The string written will be NUL terminated, and truncated if
 * necessary.
 */

static void traceStartWhyToTextBuffer(char *s, size_t len, TraceStartWhy why)
{
  const char *r;
  size_t i;

  AVER(s);
  /* len can be anything, including 0. */
  AVER(why < TraceStartWhyLIMIT);

  r = TraceStartWhyToString(why);

  for(i=0; i<len; ++i) {
    s[i] = r[i];
    if(r[i] == '\0')
      break;
  }
  s[len-1] = '\0';  /* .whybuf.nul */
}

/* TracePostStartMessage -- complete and post trace start message
 *
 */

void TracePostStartMessage(Trace trace)
{
  Arena arena;
  TraceId ti;
  TraceStartMessage tsMessage;

  AVERT(Trace, trace);
  AVER(trace->state == TraceUNFLIPPED);

  arena = trace->arena;
  AVERT(Arena, arena);

  ti = trace->ti;
  AVERT(TraceId, ti);

  tsMessage = arena->tsMessage[ti];
  if(tsMessage) {
    AVERT(TraceStartMessage, tsMessage);

    traceStartWhyToTextBuffer(tsMessage->why,
                              sizeof tsMessage->why, trace->why);

    arena->tsMessage[ti] = NULL;
    MessagePost(arena, TraceStartMessageMessage(tsMessage));
  } else {
    arena->droppedMessages += 1;
  }

  /* We have consumed the pre-allocated message */
  AVER(!arena->tsMessage[ti]);
}



/* --------  TraceMessage (trace end)  -------- */


/* TraceMessage -- posted when a trace ends
 *
 * Internal names:
 *   trace end
 *   TraceMessage, tMessage (struct *)
 *   MessageTypeGC (enum)
 *
 * External names:
 *   mps_message_type_gc (enum macro)
 *   MPS_MESSAGE_TYPE_GC (enum)
 *
 * (Note: this should properly be called "trace end", but it's much
 * too late to change it now!)
 *
 * <design/message-gc>.
 */


/* TraceMessage -- type of trace end messages */

#define TraceMessageSig ((Sig)0x51926359) /* SIGnature TRace MeSsaGe */

typedef struct TraceMessageStruct  {
  Sig sig;
  Size liveSize;
  Size condemnedSize;
  Size notCondemnedSize;
  MessageStruct messageStruct;
} TraceMessageStruct;

#define TraceMessageMessage(traceMessage) (&((traceMessage)->messageStruct))
#define MessageTraceMessage(message) \
  (PARENT(TraceMessageStruct, messageStruct, message))

Bool TraceMessageCheck(TraceMessage tMessage)
{
  CHECKS(TraceMessage, tMessage);
  CHECKD(Message, TraceMessageMessage(tMessage));
  CHECKL(MessageGetType(TraceMessageMessage(tMessage)) ==
         MessageTypeGC);
  /* We can't check anything about the statistics.  In particular, */
  /* liveSize may exceed condemnedSize because they are only estimates. */

  return TRUE;
}

static void TraceMessageDelete(Message message)
{
  TraceMessage tMessage;
  Arena arena;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  arena = MessageArena(message);
  tMessage->sig = SigInvalid;
  MessageFinish(message);

  ControlFree(arena, (void *)tMessage, sizeof(TraceMessageStruct));
}

static Size TraceMessageLiveSize(Message message)
{
  TraceMessage tMessage;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  return tMessage->liveSize;
}

static Size TraceMessageCondemnedSize(Message message)
{
  TraceMessage tMessage;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  return tMessage->condemnedSize;
}

static Size TraceMessageNotCondemnedSize(Message message)
{
  TraceMessage tMessage;

  AVERT(Message, message);
  tMessage = MessageTraceMessage(message);
  AVERT(TraceMessage, tMessage);

  return tMessage->notCondemnedSize;
}

static MessageClassStruct TraceMessageClassStruct = {
  MessageClassSig,               /* sig */
  "TraceGC",                     /* name */
  MessageTypeGC,                 /* Message Type */
  TraceMessageDelete,            /* Delete */
  MessageNoFinalizationRef,      /* FinalizationRef */
  TraceMessageLiveSize,          /* GCLiveSize */
  TraceMessageCondemnedSize,     /* GCCondemnedSize */
  TraceMessageNotCondemnedSize,  /* GCNotCondemnedSize */
  MessageNoGCStartWhy,           /* GCStartWhy */
  MessageClassSig                /* <design/message#.class.sig.double> */
};

static void traceMessageInit(Arena arena, TraceMessage tMessage)
{
  AVERT(Arena, arena);

  MessageInit(arena, TraceMessageMessage(tMessage),
              &TraceMessageClassStruct, MessageTypeGC);
  tMessage->liveSize = (Size)0;
  tMessage->condemnedSize = (Size)0;
  tMessage->notCondemnedSize = (Size)0;

  tMessage->sig = TraceMessageSig;
  AVERT(TraceMessage, tMessage);
}

/* TracePostMessage -- complete and post trace end message
 *
 * .message.data: The trace end message contains the live size
 * (forwardedSize + preservedInPlaceSize), the condemned size
 * (condemned), and the not-condemned size (notCondemned).
 */

void TracePostMessage(Trace trace)
{
  Arena arena;
  TraceId ti;
  TraceMessage tMessage;

  AVERT(Trace, trace);
  AVER(trace->state == TraceFINISHED);

  arena = trace->arena;
  AVERT(Arena, arena);

  ti = trace->ti;
  AVERT(TraceId, ti);

  tMessage = arena->tMessage[ti];
  if(tMessage) {
    AVERT(TraceMessage, tMessage);

    tMessage->liveSize = trace->forwardedSize + trace->preservedInPlaceSize;
    tMessage->condemnedSize = trace->condemned;
    tMessage->notCondemnedSize = trace->notCondemned;

    arena->tMessage[ti] = NULL;
    MessagePost(arena, TraceMessageMessage(tMessage));
  } else {
    arena->droppedMessages += 1;
  }

  /* We have consumed the pre-allocated message */
  AVER(!arena->tMessage[ti]);
}



/* --------  TraceIdMessages  -------- */


/* TraceIdMessagesCheck - pre-allocated messages for this traceid.
 *
 * Messages are absent when already sent, or when (exceptionally)
 * ControlAlloc failed at the end of the previous trace.  If present,
 * they must be valid.
 *
 * Messages are pre-allocated all-or-nothing.  So if we've got a
 * start but no end, that's wrong.
 *
 * Note: this function does not take a pointer to a struct, so it is
 * not a 'proper' _Check function.  It can be used in CHECKL, but
 * not CHECKD etc.
 */

Bool TraceIdMessagesCheck(Arena arena, TraceId ti)
{
  CHECKL(!arena->tsMessage[ti] || TraceStartMessageCheck(arena->tsMessage[ti]));
  CHECKL(!arena->tsMessage[ti] || arena->tMessage[ti]);
  CHECKL(!arena->tMessage[ti] || TraceMessageCheck(arena->tMessage[ti]));

  return TRUE;
}

/* TraceIdMessagesCreate -- pre-allocate all messages for this traceid
 *
 * <design/message-gc#.lifecycle>.
 *
 * For remote control of ControlAlloc, to simulate low memory:
 *  #define ControlAlloc !TIMCA_remote() ? ResFAIL : ControlAlloc
 *  extern Bool TIMCA_remote(void);
 * See TIMCA_remote() in zmess.c
 */

Res TraceIdMessagesCreate(Arena arena, TraceId ti)
{
  void *p;
  TraceStartMessage tsMessage;
  TraceMessage tMessage;
  Res res;

  /* Ensure we don't leak memory */
  AVER(!arena->tsMessage[ti]);
  AVER(!arena->tMessage[ti]);

  res = ControlAlloc(&p, arena, sizeof(TraceStartMessageStruct));
  if(res != ResOK)
    goto failTraceStartMessage;
  tsMessage = p;

  res = ControlAlloc(&p, arena, sizeof(TraceMessageStruct));
  if(res != ResOK)
    goto failTraceMessage;
  tMessage = p;

  traceStartMessageInit(arena, tsMessage);
  AVERT(TraceStartMessage, tsMessage);

  traceMessageInit(arena, tMessage);
  AVERT(TraceMessage, tMessage);

  arena->tsMessage[ti] = tsMessage;
  arena->tMessage[ti] = tMessage;

  AVER(TraceIdMessagesCheck(arena, ti));

  return ResOK;

failTraceMessage:
  ControlFree(arena, tsMessage, sizeof(TraceStartMessageStruct));
failTraceStartMessage:
  AVER(TraceIdMessagesCheck(arena, ti));
  return res;
}

/* TraceIdMessagesDestroy -- destroy any pre-allocated messages
 *
 * Only used during ArenaDestroy.
 *
 * <design/message-gc#.lifecycle>.
 */

void TraceIdMessagesDestroy(Arena arena, TraceId ti)
{
  TraceStartMessage tsMessage;
  TraceMessage tMessage;

  AVER(TraceIdMessagesCheck(arena, ti));

  tsMessage = arena->tsMessage[ti];
  if(tsMessage) {
    arena->tsMessage[ti] = NULL;
    TraceStartMessageDelete(TraceStartMessageMessage(tsMessage));
  }

  tMessage = arena->tMessage[ti];
  if(tMessage) {
    arena->tMessage[ti] = NULL;
    TraceMessageDelete(TraceMessageMessage(tMessage));
  }

  AVER(!arena->tsMessage[ti]);
  AVER(!arena->tMessage[ti]);
  AVER(TraceIdMessagesCheck(arena, ti));
}



/* -----  ArenaRelease, ArenaClamp, ArenaPark, ArenaPostmortem  ----- */


/* ArenaRelease, ArenaClamp, ArenaPark, ArenaPostmortem --
 * allow/prevent collection work.
 */


/* ArenaClamp -- clamp the arena (no optional collection increments) */

void ArenaClamp(Globals globals)
{
  AVERT(Globals, globals);
  globals->clamped = TRUE;
}


/* ArenaRelease -- release the arena (allow optional collection
 * increments) */

void ArenaRelease(Globals globals)
{
  AVERT(Globals, globals);
  globals->clamped = FALSE;
  ArenaPoll(globals);
}


/* ArenaPark -- finish all current collections and clamp the arena,
 * thus leaving the arena parked. */

void ArenaPark(Globals globals)
{
  TraceId ti;
  Trace trace;
  Arena arena;
  Clock start;

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  globals->clamped = TRUE;
  start = ClockNow();

  while(arena->busyTraces != TraceSetEMPTY) {
    /* Advance all active traces. */
    TRACE_SET_ITER(ti, trace, arena->busyTraces, arena)
      TraceAdvance(trace);
      if(trace->state == TraceFINISHED) {
        TraceDestroyFinished(trace);
      }
    TRACE_SET_ITER_END(ti, trace, arena->busyTraces, arena);
  }

  ArenaAccumulateTime(arena, start, ClockNow());

  /* All traces have finished so there must not be an emergency. */
  AVER(!ArenaEmergency(arena));
}


/* arenaExpose -- discard all protection from MPS-managed memory
 *
 * This is called by ArenaPostmortem, which we expect only to be used
 * after a fatal error. So we use the lowest-level description of the
 * MPS-managed memory (the chunk ring page tables) to avoid the risk
 * of higher-level structures (like the segments) having been
 * corrupted.
 *
 * After calling this function memory may not be in a consistent
 * state, so it is not safe to continue running the MPS.
 */

static void arenaExpose(Arena arena)
{
  Ring node, next;
  RING_FOR(node, &arena->chunkRing, next) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    Index i;
    for (i = 0; i < chunk->pages; ++i) {
      if (Method(Arena, arena, chunkPageMapped)(chunk, i)) {
        ProtSet(PageIndexBase(chunk, i), PageIndexBase(chunk, i + 1),
                AccessSetEMPTY);
      }
    }
  }
}


/* ArenaPostmortem -- enter the postmortem state */

void ArenaPostmortem(Globals globals)
{
  Arena arena = GlobalsArena(globals);

  /* Ensure lock is released. */
  while (LockIsHeld(globals->lock)) {
    LockReleaseRecursive(globals->lock);
  }

  /* Remove the arena from the global arena ring so that it no longer
   * handles protection faults. (Don't call arenaDenounce because that
   * needs to claim the global ring lock, but that might already be
   * held, for example if we are inside ArenaAccess.) */
  RingRemove(&globals->globalRing);

  /* Clamp the arena so that ArenaPoll does nothing. */
  ArenaClamp(globals);

  /* Remove all protection from mapped pages. */
  arenaExpose(arena);
}


/* ArenaStartCollect -- start a collection of everything in the
 * arena; leave unclamped. */

Res ArenaStartCollect(Globals globals, TraceStartWhy why)
{
  Arena arena;
  Res res;
  Trace trace;

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  ArenaPark(globals);
  res = TraceStartCollectAll(&trace, arena, why);
  if(res != ResOK)
    goto failStart;
  ArenaRelease(globals);
  return ResOK;

failStart:
  ArenaRelease(globals);
  return res;
}

/* ArenaCollect -- collect everything in arena; leave parked */

Res ArenaCollect(Globals globals, TraceStartWhy why)
{
  Res res;

  AVERT(Globals, globals);
  res = ArenaStartCollect(globals, why);
  if(res != ResOK)
    return res;

  ArenaPark(globals);
  return ResOK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
