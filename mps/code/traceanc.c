/* traceanc.c: ANCILLARY SUPPORT FOR TRACER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.
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
 *
 *   - ArenaExposeRemember and ArenaRestoreProtection.
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
 * See <design/message-gc/>.
 */

#define TraceStartMessageSig ((Sig)0x51926535) /* SIG TRaceStartMeSsage */

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
  MessageClassSig                /* <design/message/#class.sig.double> */
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
 
const char *TraceStartWhyToString(int why)
{
  const char *r;

  switch(why) {
  case TraceStartWhyCHAIN_GEN0CAP:
    r = "Generation 0 of a chain has reached capacity:"
        " start a minor collection.";
    break;
  case TraceStartWhyDYNAMICCRITERION:
    r = "Need to start full collection now, or there won't be enough"
        " memory (ArenaAvail) to complete it.";
    break;
  case TraceStartWhyOPPORTUNISM:
    r = "Opportunism: client predicts plenty of idle time,"
        " so start full collection.";
    break;
  case TraceStartWhyCLIENTFULL_INCREMENTAL:
    r = "Client requests: start incremental full collection now.";
    break;
  case TraceStartWhyCLIENTFULL_BLOCK:
    r = "Client requests: immediate full collection.";
    break;
  case TraceStartWhyWALK:
    r = "Walking all live objects.";
    break;
  case TraceStartWhyEXTENSION:
    r = "Extension: an MPS extension started the trace.";
    break;
  default:
    NOTREACHED;
    r = "Unknown reason (internal error).";
    break;
  }

  /* Should fit in buffer without truncation; see .whybuf.len */
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

static void traceStartWhyToTextBuffer(char *s, size_t len, int why)
{
  const char *r;
  size_t i;

  AVER(s);
  /* len can be anything, including 0. */
  AVER(TraceStartWhyBASE <= why);
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
 * See <design/message-gc/>.
 */


/* TraceMessage -- type of trace end messages */

#define TraceMessageSig ((Sig)0x51926359)

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
  MessageClassSig                /* <design/message/#class.sig.double> */
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
  CHECKL(!arena->tsMessage[ti]
         || TraceStartMessageCheck(arena->tsMessage[ti]));
  CHECKL(!arena->tMessage[ti]
         || TraceMessageCheck(arena->tMessage[ti]));
  CHECKL(! (arena->tsMessage[ti] && !arena->tMessage[ti]) );

  return TRUE;
}

/* TraceIdMessagesCreate -- pre-allocate all messages for this traceid
 *
 * See <design/message-gc/#lifecycle>.
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
 * See <design/message-gc/#lifecycle>.
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



/* --------  ArenaRelease, ArenaClamp, ArenaPark  -------- */


/* ArenaRelease, ArenaClamp, ArenaPark -- allow/prevent collection work.
 *
 * These functions allow or prevent collection work.
 */


/* Forward Declarations */
static void arenaForgetProtection(Globals globals);


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
  arenaForgetProtection(globals);
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

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  globals->clamped = TRUE;

  while(arena->busyTraces != TraceSetEMPTY) {
    /* Advance all active traces. */
    TRACE_SET_ITER(ti, trace, arena->busyTraces, arena)
      TraceAdvance(trace);
      if(trace->state == TraceFINISHED) {
        TraceDestroy(trace);
      }
    TRACE_SET_ITER_END(ti, trace, arena->busyTraces, arena);
  }
  
  /* Clear any emergency flag so that the next collection starts normally.
     Any traces that have been finished may have reclaimed memory. */
  ArenaSetEmergency(arena, FALSE);
}

/* ArenaStartCollect -- start a collection of everything in the
 * arena; leave unclamped. */

Res ArenaStartCollect(Globals globals, int why)
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

Res ArenaCollect(Globals globals, int why)
{
  Res res;

  AVERT(Globals, globals);
  res = ArenaStartCollect(globals, why);
  if(res != ResOK)
    return res;

  ArenaPark(globals);
  return ResOK;
}



/* --------  ExposeRemember and RestoreProtection  -------- */


/* Low level stuff for Expose / Remember / Restore */

typedef struct RememberedSummaryBlockStruct {
  RingStruct globalRing;        /* link on globals->rememberedSummaryRing */
  struct SummaryPair {
    Addr base;
    RefSet summary;
  } the[RememberedSummaryBLOCK];
} RememberedSummaryBlockStruct;

typedef struct RememberedSummaryBlockStruct *RememberedSummaryBlock;

static void rememberedSummaryBlockInit(struct RememberedSummaryBlockStruct *block)
{
  size_t i;

  RingInit(&block->globalRing);
  for(i = 0; i < RememberedSummaryBLOCK; ++ i) {
    block->the[i].base = (Addr)0;
    block->the[i].summary = RefSetUNIV;
  }
}

static Res arenaRememberSummaryOne(Globals global, Addr base, RefSet summary)
{
  Arena arena;
  RememberedSummaryBlock block;

  AVER(summary != RefSetUNIV);

  arena = GlobalsArena(global);

  if(global->rememberedSummaryIndex == 0) {
    void *p;
    RememberedSummaryBlock newBlock;
    int res;

    res = ControlAlloc(&p, arena, sizeof *newBlock);
    if(res != ResOK) {
      return res;
    }
    newBlock = p;
    rememberedSummaryBlockInit(newBlock);
    RingAppend(GlobalsRememberedSummaryRing(global),
      &newBlock->globalRing);
  }
  block = RING_ELT(RememberedSummaryBlock, globalRing,
    RingPrev(GlobalsRememberedSummaryRing(global)));
  AVER(global->rememberedSummaryIndex < RememberedSummaryBLOCK);
  AVER(block->the[global->rememberedSummaryIndex].base == (Addr)0);
  AVER(block->the[global->rememberedSummaryIndex].summary == RefSetUNIV);
  block->the[global->rememberedSummaryIndex].base = base;
  block->the[global->rememberedSummaryIndex].summary = summary;
  ++ global->rememberedSummaryIndex;
  if(global->rememberedSummaryIndex >= RememberedSummaryBLOCK) {
    AVER(global->rememberedSummaryIndex == RememberedSummaryBLOCK);
    global->rememberedSummaryIndex = 0;
  }

  return ResOK;
}

/* ArenaExposeRemember -- park arena and then lift all protection
   barriers.  Parameter 'remember' specifies whether to remember the
   protection state or not (for later restoration with
   ArenaRestoreProtection).
   */
void ArenaExposeRemember(Globals globals, Bool remember)
{
  Seg seg;
  Arena arena;

  AVERT(Globals, globals);
  AVERT(Bool, remember);

  ArenaPark(globals);

  arena = GlobalsArena(globals);
  if(SegFirst(&seg, arena)) {
    Addr base;

    do {
      base = SegBase(seg);
      if(IsSubclassPoly(ClassOfSeg(seg), GCSegClassGet())) {
        if(remember) {
          RefSet summary;

          summary = SegSummary(seg);
          if(summary != RefSetUNIV) {
            Res res = arenaRememberSummaryOne(globals, base, summary);
            if(res != ResOK) {
              /* If we got an error then stop trying to remember any
              protections. */
              remember = 0;
            }
          }
        }
        SegSetSummary(seg, RefSetUNIV);
        AVER(SegSM(seg) == AccessSetEMPTY);
      }
    } while(SegNext(&seg, arena, seg));
  }
}

void ArenaRestoreProtection(Globals globals)
{
  Ring node, next;
  Arena arena;

  arena = GlobalsArena(globals);

  RING_FOR(node, GlobalsRememberedSummaryRing(globals), next) {
    RememberedSummaryBlock block =
      RING_ELT(RememberedSummaryBlock, globalRing, node);
    size_t i;

    for(i = 0; i < RememberedSummaryBLOCK; ++ i) {
      Seg seg;
      Bool b;

      if(block->the[i].base == (Addr)0) {
        AVER(block->the[i].summary == RefSetUNIV);
        continue;
      }
      b = SegOfAddr(&seg, arena, block->the[i].base);
      if(b && SegBase(seg) == block->the[i].base) {
        AVER(IsSubclassPoly(ClassOfSeg(seg), GCSegClassGet()));
        SegSetSummary(seg, block->the[i].summary);
      } else {
        /* Either seg has gone or moved, both of which are */
        /* client errors. */
        NOTREACHED;
      }
    }
  }

  arenaForgetProtection(globals);
}

static void arenaForgetProtection(Globals globals)
{
  Ring node, next;
  Arena arena;

  arena = GlobalsArena(globals);
  /* Setting this early means that we preserve the invariant
     <code/global.c#remembered.summary> */
  globals->rememberedSummaryIndex = 0;
  RING_FOR(node, GlobalsRememberedSummaryRing(globals), next) {
    RememberedSummaryBlock block =
      RING_ELT(RememberedSummaryBlock, globalRing, node);

    RingRemove(node);
    ControlFree(arena, block, sizeof *block);
  }
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited
 * <http://www.ravenbrook.com/>.
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
