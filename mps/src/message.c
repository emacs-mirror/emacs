/* impl.c.message: MPS / CLIENT MESSAGES
 *
 * $HopeName: MMsrc!message.c(MMdevel_drj_message.8) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All Rights Reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
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


SRCID(message, "$HopeName: MMsrc!message.c(MMdevel_drj_message.8) $");


/* Maps from a Ring pointer to the message */
#define MessageNodeMessage(node) \
  PARENT(MessageStruct, queueRing, node)

/* commented out as it causes compiler warnings */
#if 0
static Message (MessageNodeMessage)(Ring node)
{
  Message message;

  AVERT(Ring, node);
  message = MessageNodeMessage(node);
  AVERT(Message, message);

  return message;
}
#endif

/* forward declarations */
static Bool MessageTypeEnabled(Arena arena, MessageType type);
static void MessageDelete(Message message);

/* is the message on the queue?
 * message on queue if and only if it's ring is not a singleton */
static Bool MessageOnQueue(Message message)
{
  AVERT(Message, message);

  return !RingIsSingle(&message->queueRing);
}


/* Checking Functions */


Bool MessageTypeCheck(MessageType type)
{
  CHECKL(type < MessageTypeMAX);

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
void MessageInit(Arena arena, Message message, MessageClass class)
{
  AVERT(Arena, arena);
  /* we are initialising the message so we can't check it */
  AVERT(MessageClass, class);

  message->arena = arena;
  message->class = class;
  RingInit(&message->queueRing);
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


/* Checks the type of the message at the head of the queue.
 *
 * returns FALSE if there is no message at the head of the queue
 * or if the type of the message at the head of the queue doesn't
 * match.
 *
 * Used internally by the implementations of the external
 * functions.
 */
static Bool MessageHeadIsType(Arena arena, MessageType type)
{
  Message message;

  AVERT(Arena, arena);
  AVER(MessageTypeCheck(type));

  if(!MessagePoll(arena)) {
    return FALSE;
  }
  message = MessageHead(arena);
  if(MessageGetType(message) != type) {
    return FALSE;
  }

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
  Message message;

  AVER(messageReturn != NULL);
  AVERT(Arena, arena);
  AVER(MessageTypeCheck(type));

  if(MessageHeadIsType(arena, type)) {
    message = MessageHead(arena);
    RingRemove(&message->queueRing);
    *messageReturn = message;
    return TRUE;
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

  AVER(message->type == MessageTypeFinalization);

  (*message->class->finalizationRef)(refReturn, arena, message);
}


/* type specific stub methods */

void MessageNoFinalizationRef(Ref *refReturn, Arena arena, 
                              Message message)
{
  AVER(refReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFinalization);

  NOTREACHED;
}
