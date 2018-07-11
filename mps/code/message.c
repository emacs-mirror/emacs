/* message.c: MPS/CLIENT MESSAGES
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: See <design/message/> (it really exists).
 *
 * PURPOSE
 *
 * .purpose: Provide the generic part of the MPS / Client message
 * interface.  Messages are instances of Message Classes; much of the
 * "real work" goes on in the modules that provide the actual messages.
 *
 * TODO: Consider using protocol classes for messages.
 */

#include "bt.h"
#include "mpm.h"

SRCID(message, "$Id$");


/* Maps from a Ring pointer to the message */
#define MessageNodeMessage(node) \
  PARENT(MessageStruct, queueRing, node)


/* forward declarations */
static Bool MessageTypeEnabled(Arena arena, MessageType type);
static void MessageDelete(Message message);


/* Internal (MPM) Interface -- functions for message originator
 *
 */


Bool MessageTypeCheck(MessageType type)
{
  CHECKL(type < MessageTypeLIMIT);
  UNUSED(type); /* <code/mpm.c#check.unused> */

  return TRUE;
}

/* See .message.clocked.  Currently finalization messages are the */
/* only ones that can be numerous. */
#define MessageIsClocked(message) \
  ((message)->klass->type != MessageTypeFINALIZATION)

Bool MessageCheck(Message message)
{
  CHECKS(Message, message);
  CHECKU(Arena, message->arena);
  CHECKD(MessageClass, message->klass);
  CHECKD_NOSIG(Ring, &message->queueRing);
  /* postedClock is uncheckable for clocked message types, */
  /* but must be 0 for unclocked message types: */
  CHECKL(MessageIsClocked(message) || (message->postedClock == 0));

  return TRUE;
}

Bool MessageClassCheck(MessageClass klass)
{
  CHECKS(MessageClass, klass);
  CHECKL(klass->name != NULL);
  CHECKL(MessageTypeCheck(klass->type));
  CHECKL(FUNCHECK(klass->delete));
  CHECKL(FUNCHECK(klass->finalizationRef));
  CHECKL(FUNCHECK(klass->gcLiveSize));
  CHECKL(FUNCHECK(klass->gcCondemnedSize));
  CHECKL(FUNCHECK(klass->gcNotCondemnedSize));
  CHECKL(FUNCHECK(klass->gcStartWhy));
  CHECKL(klass->endSig == MessageClassSig);

  return TRUE;
}

void MessageInit(Arena arena, Message message, MessageClass klass,
                 MessageType type)
{
  AVERT(Arena, arena);
  AVER(message != NULL);
  AVERT(MessageClass, klass);
  AVERT(MessageType, type);

  message->arena = arena;
  message->klass = klass;
  RingInit(&message->queueRing);
  message->postedClock = 0;
  message->sig = MessageSig;

  AVERT(Message, message);
  AVER(MessageGetType(message) == type);
}

void MessageFinish(Message message)
{
  AVERT(Message, message);
  AVER(RingIsSingle(&message->queueRing));

  message->sig = SigInvalid;
  RingFinish(&message->queueRing);
}

Arena MessageArena(Message message)
{
  AVERT(Message, message);

  return message->arena;
}

Bool MessageOnQueue(Message message)
{
  AVERT(Message, message);

  /* message is on queue if and only if its ring is not a singleton. */
  return !RingIsSingle(&message->queueRing);
}

/* Post a message to the arena's queue of pending messages */
void MessagePost(Arena arena, Message message)
{
  AVERT(Arena, arena);
  AVERT(Message, message);

  /* queueRing field must be a singleton, see */
  /* <design/message/#fun.post.singleton> */
  AVER(!MessageOnQueue(message));
  if(MessageTypeEnabled(arena, MessageGetType(message))) {
    /* .message.clocked: Reading the clock with ClockNow() */
    /* involves an mpslib call, so we avoid it for message */
    /* types that may be numerous. */
    if(MessageIsClocked(message)) {
      message->postedClock = ClockNow();
    }
    RingAppend(&arena->messageRing, &message->queueRing);
  } else {
    /* discard message immediately if client hasn't enabled that type */
    MessageDiscard(arena, message);
  }
}

/* Return the message at the head of the arena's queue */
static Message MessageHead(Arena arena)
{
  AVERT(Arena, arena);
  AVER(!RingIsSingle(&arena->messageRing));

  return MessageNodeMessage(RingNext(&arena->messageRing));
}

/* Delete the message at the head of the queue (helper function). */
static void MessageDeleteHead(Arena arena)
{
  Message message;

  AVERT(Arena, arena);
  AVER(!RingIsSingle(&arena->messageRing));

  message = MessageHead(arena);
  AVERT(Message, message);
  RingRemove(&message->queueRing);
  MessageDelete(message);
}

/* Empty the queue by discarding all messages */
void MessageEmpty(Arena arena)
{
  AVERT(Arena, arena);

  while(!RingIsSingle(&arena->messageRing)) {
    MessageDeleteHead(arena);
  }
}


/* Delivery (Client) Interface -- functions for recipient
 *
 * Most of these functions are exposed through the external MPS 
 * interface.
 */


static Bool MessageTypeEnabled(Arena arena, MessageType type)
{
  AVERT(Arena, arena);
  AVERT(MessageType, type);

  return BTGet(arena->enabledMessageTypes, type);
}

void MessageTypeEnable(Arena arena, MessageType type)
{
  AVERT(Arena, arena);
  AVERT(MessageType, type);

  BTSet(arena->enabledMessageTypes, type);
}

void MessageTypeDisable(Arena arena, MessageType type)
{
  Message message;

  AVERT(Arena, arena);
  AVERT(MessageType, type);

  /* Flush existing messages of this type */
  while(MessageGet(&message, arena, type)) {
    MessageDelete(message);
  }

  BTRes(arena->enabledMessageTypes, type);
}

/* Any messages on the queue? */
Bool MessagePoll(Arena arena)
{
  AVERT(Arena, arena);

  if(RingIsSingle(&arena->messageRing)) {
    return FALSE;
  } else {
    return TRUE;
  }
}

/* Return the type of the message at the head of the queue, if any */
Bool MessageQueueType(MessageType *typeReturn, Arena arena)
{
  Message message;
  MessageType type;

  AVER(typeReturn != NULL);
  AVERT(Arena, arena);

  if(!MessagePoll(arena)) {
    return FALSE;
  }
  message = MessageHead(arena);
  type = MessageGetType(message);
  *typeReturn = type;

  return TRUE;
}

/* Get next message of specified type, removing it from the queue */
Bool MessageGet(Message *messageReturn, Arena arena, MessageType type)
{
  Ring node, next;

  AVER(messageReturn != NULL);
  AVERT(Arena, arena);
  AVERT(MessageType, type);

  RING_FOR(node, &arena->messageRing, next) {
    Message message = RING_ELT(Message, queueRing, node);
    if(MessageGetType(message) == type) {
      RingRemove(&message->queueRing);
      *messageReturn = message;
      return TRUE;
    }
  }
  return FALSE;
}

/* Discard a message (recipient has finished using it). */
void MessageDiscard(Arena arena, Message message)
{
  AVERT(Arena, arena);
  AVERT(Message, message);

  AVER(!MessageOnQueue(message));

  MessageDelete(message);
}


/* Message Methods, Generic
 *
 * (Some of these dispatch on message->klass).
 */


/* Return the type of a message */
MessageType MessageGetType(Message message)
{
  MessageClass klass;
  AVERT(Message, message);
  
  klass = message->klass;
  AVERT(MessageClass, klass);

  return klass->type;
}

/* Return the class of a message */
MessageClass MessageGetClass(Message message)
{
  AVERT(Message, message);

  return message->klass;
}

Clock MessageGetClock(Message message)
{
  AVERT(Message, message);

  return message->postedClock;
}

static void MessageDelete(Message message)
{
  AVERT(Message, message);

  (*message->klass->delete)(message);
}


/* Message Method Dispatchers, Type-specific
 *
 */


void MessageFinalizationRef(Ref *refReturn, Arena arena,
                            Message message)
{
  AVER(refReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Message, message);
  AVER(MessageGetType(message) == MessageTypeFINALIZATION);

  (*message->klass->finalizationRef)(refReturn, arena, message);
}

Size MessageGCLiveSize(Message message)
{
  AVERT(Message, message);
  AVER(MessageGetType(message) == MessageTypeGC);

  return (*message->klass->gcLiveSize)(message);
}

Size MessageGCCondemnedSize(Message message)
{
  AVERT(Message, message);
  AVER(MessageGetType(message) == MessageTypeGC);

  return (*message->klass->gcCondemnedSize)(message);
}

Size MessageGCNotCondemnedSize(Message message)
{
  AVERT(Message, message);
  AVER(MessageGetType(message) == MessageTypeGC);

  return (*message->klass->gcNotCondemnedSize)(message);
}

const char *MessageGCStartWhy(Message message)
{
  AVERT(Message, message);
  AVER(MessageGetType(message) == MessageTypeGCSTART);

  return (*message->klass->gcStartWhy)(message);
}


/* Message Method Stubs, Type-specific
 *
 */


void MessageNoFinalizationRef(Ref *refReturn, Arena arena,
                              Message message)
{
  AVER(refReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Message, message);

  NOTREACHED;
}

Size MessageNoGCLiveSize(Message message)
{
  AVERT(Message, message);
  UNUSED(message);

  NOTREACHED;

  return (Size)0;
}

Size MessageNoGCCondemnedSize(Message message)
{
  AVERT(Message, message);
  UNUSED(message);

  NOTREACHED;

  return (Size)0;
}

Size MessageNoGCNotCondemnedSize(Message message)
{
  AVERT(Message, message);
  UNUSED(message);

  NOTREACHED;

  return (Size)0;
}

const char *MessageNoGCStartWhy(Message message)
{
  AVERT(Message, message);
  UNUSED(message);

  NOTREACHED;

  return NULL;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
