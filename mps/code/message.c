/* impl.c.message: MPS/CLIENT MESSAGES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * DESIGN
 *
 * .design: See design.mps.message (it really exists).
 *
 * PURPOSE
 *
 * .purpose: Provide the generic part of the MPS / Client message
 * interface.  Messages are instances of Message Classes; much of the
 * "real work" goes on in the modules that provide the actual messages.
 */

#include "mpm.h"

SRCID(message, "$Id$");


/* Maps from a Ring pointer to the message */
#define MessageNodeMessage(node) \
  PARENT(MessageStruct, queueRing, node)


/* forward declarations */
static Bool MessageTypeEnabled(Arena arena, MessageType type);
static void MessageDelete(Message message);


/* MessageOnQueue -- is the message on the queue?
 *
 * Message is on queue if and only if its ring is not a singleton.
 */

static Bool MessageOnQueue(Message message)
{
  AVERT(Message, message);

  return !RingIsSingle(&message->queueRing);
}


/* Checking Functions */


Bool MessageTypeCheck(MessageType type)
{
  CHECKL(type < MessageTypeLIMIT);
  UNUSED(type); /* impl.c.mpm.check.unused */

  return TRUE;
}


Bool MessageCheck(Message message)
{
  CHECKS(Message, message);
  CHECKU(Arena, message->arena);
  CHECKL(MessageTypeCheck(message->type));
  CHECKU(MessageClass, message->class);
  CHECKL(RingCheck(&message->queueRing));

  return TRUE;
}


Bool MessageClassCheck(MessageClass class)
{
  CHECKS(MessageClass, class);
  CHECKL(class->name != NULL);
  CHECKL(FUNCHECK(class->delete));
  CHECKL(FUNCHECK(class->finalizationRef));
  CHECKL(class->endSig == MessageClassSig);

  return TRUE;
}


/* Internal Functions */


/* returns the arena associated with a message */
Arena MessageArena(Message message)
{
  AVERT(Message, message);

  return message->arena;
}


/* return the class of a message */
MessageClass MessageGetClass(Message message)
{
  AVERT(Message, message);

  return message->class;
}


/* Initialises a message */
void MessageInit(Arena arena, Message message, MessageClass class,
                 MessageType type)
{
  AVERT(Arena, arena);
  AVER(message != NULL);
  AVERT(MessageClass, class);
  AVERT(MessageType, type);

  message->arena = arena;
  message->class = class;
  RingInit(&message->queueRing);
  message->type = type;
  message->sig = MessageSig;

  AVERT(Message, message);
}


/* Finishes a message */
void MessageFinish(Message message)
{
  AVERT(Message, message);
  AVER(RingIsSingle(&message->queueRing));

  message->sig = SigInvalid;
  RingFinish(&message->queueRing);
}


/* Posts a message to the arena's queue of pending messages */
void MessagePost(Arena arena, Message message)
{
  AVERT(Arena, arena);
  AVERT(Message, message);

  /* queueRing field must be a singleton, see */
  /* design.mps.message.fun.post.singleton */
  AVER(!MessageOnQueue(message));
  if(MessageTypeEnabled(arena, message->type)) {
    RingAppend(&arena->messageRing, &message->queueRing);
  } else {
    /* discard message immediately if client hasn't enabled that type */
    MessageDiscard(arena, message);
  }
}


/* returns the Message at the head of the queue */
static Message MessageHead(Arena arena)
{
  AVERT(Arena, arena);
  AVER(!RingIsSingle(&arena->messageRing));

  return MessageNodeMessage(RingNext(&arena->messageRing));
}


/* returns the type of a message */
MessageType MessageGetType(Message message)
{
  AVERT(Message, message);

  return message->type;
}


/* External Functions
 *
 * These are actually the internal implementations of functions
 * exposed through the external interface */


/* Determines whether the queue has any messages on it */
Bool MessagePoll(Arena arena)
{
  AVERT(Arena, arena);

  if(RingIsSingle(&arena->messageRing)) {
    return FALSE;
  } else {
    return TRUE;
  }
}


/* Determines the type of a message at the head of the queue */
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


/* Discards a message
 * (called from external interface) */
void MessageDiscard(Arena arena, Message message)
{
  AVERT(Arena, arena);
  AVERT(Message, message);

  AVER(!MessageOnQueue(message));

  MessageDelete(message);
}


/* Deletes the message at the head of the queue.
 * Internal function. */
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

/* Empties the queue by discarding all messages */
void MessageEmpty(Arena arena)
{
  AVERT(Arena, arena);

  while(!RingIsSingle(&arena->messageRing)) {
    MessageDeleteHead(arena);
  }
}

Bool MessageGet(Message *messageReturn, Arena arena, MessageType type)
{
  Ring node, next;

  AVER(messageReturn != NULL);
  AVERT(Arena, arena);
  AVER(MessageTypeCheck(type));

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


static Bool MessageTypeEnabled(Arena arena, MessageType type)
{
  AVERT(Arena, arena);
  AVER(MessageTypeCheck(type));

  return BTGet(arena->enabledMessageTypes, type);
}
 

void MessageTypeEnable(Arena arena, MessageType type)
{
  AVERT(Arena, arena);
  AVER(MessageTypeCheck(type));

  BTSet(arena->enabledMessageTypes, type);
}


void MessageTypeDisable(Arena arena, MessageType type)
{
  Message message;

  AVERT(Arena, arena);
  AVER(MessageTypeCheck(type));

  /* Flush existing messages of this type */
  while(MessageGet(&message, arena, type)) {
    MessageDelete(message);
  }

  BTRes(arena->enabledMessageTypes, type);
}



/* Dispatch Methods */


/* generic message delete dispatch */
static void MessageDelete(Message message)
{
  AVERT(Message, message);

  (*message->class->delete)(message);
}


/* type specific dispatch methods */

void MessageFinalizationRef(Ref *refReturn, Arena arena,
                            Message message)
{
  AVER(refReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFINALIZATION);

  (*message->class->finalizationRef)(refReturn, arena, message);

  return;
}


Size MessageGCLiveSize(Message message)
{
  AVERT(Message, message);
  AVER(message->type == MessageTypeGC);

  return (*message->class->gcLiveSize)(message);
}

Size MessageGCCondemnedSize(Message message)
{
  AVERT(Message, message);
  AVER(message->type == MessageTypeGC);

  return (*message->class->gcCondemnedSize)(message);
}

Size MessageGCNotCondemnedSize(Message message)
{
  AVERT(Message, message);
  AVER(message->type == MessageTypeGC);

  return (*message->class->gcNotCondemnedSize)(message);
}


/* type-specific stub methods */


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
