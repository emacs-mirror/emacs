/*  impl.c.messtest: MESSAGE TEST
 *
 *  $HopeName: !messtest.c(trunk.1) $
 * Copyright (C) 1999 Harlequin Ltd.  All rights reserved.
 */

#include "mpm.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

SRCID(messtest, "$HopeName: !messtest.c(trunk.1) $");



static void die_true(Bool res, const char *s)
{
  die_expect((mps_res_t)res, (mps_res_t)TRUE, s);
}

static void die_false(Bool res, const char *s)
{
  die_expect((mps_res_t)res, (mps_res_t)FALSE, s);
}



/* Basic infrastructure for creating dummy messages */

static void dfMessageDelete(Message message)
{
  Arena arena;
  arena = MessageArena(message);
  ControlFree(arena, (void *)message, sizeof(MessageStruct));
}


/* DFMessageClassStruct -- dummy finalization message class */

static MessageClassStruct DFMessageClassStruct = {
  MessageClassSig,             /* sig */
  "DummyFinal",                /* name */
  dfMessageDelete,             /* Delete */
  MessageNoFinalizationRef,    /* FinalizationRef */
  MessageNoGCLiveSize,         /* GCLiveSize */
  MessageNoGCCondemnedSize,    /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize, /* GCNoteCondemnedSize */
  MessageClassSig              /* design.mps.message.class.sig.double */
};


/* DGCMessageClassStruct -- dummy GC message class */

static MessageClassStruct DGCMessageClassStruct = {
  MessageClassSig,             /* sig */
  "DummyGC",                   /* name */
  dfMessageDelete,             /* Delete */
  MessageNoFinalizationRef,    /* FinalizationRef */
  MessageNoGCLiveSize,         /* GCLiveSize */
  MessageNoGCCondemnedSize,    /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize, /* GCNoteCondemnedSize */
  MessageClassSig              /* design.mps.message.class.sig.double */
};


static void checkNoMessages(Arena arena)
{
  die_false(MessagePoll(arena), "Queue not empty");
}

static void topMessageType(MessageType *typeReturn, Arena arena)
{
  die_true(MessageQueueType(typeReturn, arena), "Queue empty");
}


/* postDummyMessage -- post a dummy finalization message */

static void postDummyMessage(Arena arena, MessageClass class,
                             MessageType type)
{
  void *p;
  Message message;

  die((mps_res_t)ControlAlloc(&p, arena, sizeof(MessageStruct), FALSE),
      "AllocMessage");
  message = (Message)p;
  MessageInit(arena, message, class, type);
  MessagePost(arena, message);
  return;
}


/* postFinalizationMessage -- post dummy finalization message */

static void postFinalizationMessage(Arena arena)
{
  postDummyMessage(arena, &DFMessageClassStruct,
                   MessageTypeFinalization);
}

/* postGCMessage -- post dummy GC message */

static void postGCMessage(Arena arena)
{
  postDummyMessage(arena, &DGCMessageClassStruct,
                   MessageTypeGC);
}


/* postInterleavedMessages -- post a couple of each message type */

static void postInterleavedMessages(Arena arena)
{
  postFinalizationMessage(arena);
  postGCMessage(arena);
  postFinalizationMessage(arena);
  postGCMessage(arena);
}


/* eatMessageOfType -- get a message of a specified type
 *
 * There must be at least 1 message of that type on the queue.
 */

static void eatMessageOfType(Arena arena, MessageType type)
{
  Message message;
  die_true(MessageGet(&message, arena, type), "No message");
  MessageDiscard(arena, message);
}


/* eatHiddenMessage -- get a message which isn't at top of queue
 *
 * Assumes there is at least 1 message of each of Finalization
 * and GC types.
 */

static void eatHiddenMessage(Arena arena)
{
  MessageType type, eatType;

  topMessageType(&type, arena);
  if (MessageTypeGC != type) {
    eatType = MessageTypeGC;
  } else {
    eatType = MessageTypeFinalization;
  }
  eatMessageOfType(arena, eatType);
}


/* eatTopMessageOfType -- get a message which is at top of queue
 *
 * The message must be of the specified type.
 * Assumes there is at least 1 message on the queue.
 */

static void eatTopMessageOfType(Arena arena, MessageType type)
{
  MessageType topType;

  topMessageType(&topType, arena);
  die_true((topType == type), "Unexpected type");
  eatMessageOfType(arena, type);
}


/* eatTopMessage -- get a message which is at top of queue
 *
 * Assumes there is at least 1 message on the queue.
 */

static void eatTopMessage(Arena arena)
{
  MessageType type;

  topMessageType(&type, arena);
  eatMessageOfType(arena, type);
}



/* testInterleaving -- test interleaving messages of different types
 *
 * See request.dylan.160204
 * must be able to retrieve a message even if a message of
 * another type is at the head of the queue.
 */

static void testInterleaving(Arena arena)
{
  MessageEmpty(arena);

  /* enable both types of message */
  MessageTypeEnable(arena, MessageTypeGC);
  MessageTypeEnable(arena, MessageTypeFinalization);

  /* post a couple of interleaved messages of each type */
  postInterleavedMessages(arena);

  /* check that we can pull out 2 messages not at the head */
  eatHiddenMessage(arena);
  eatHiddenMessage(arena);

  /* check that we can pull out 2 messages which are at the head */
  eatTopMessage(arena);
  eatTopMessage(arena);
}


/* testDisabling -- test message types can be disabled
 *
 * See request.dylan.160204
 */

static void testDisabling(Arena arena)
{
  MessageEmpty(arena);

  /* enable both types of message */
  MessageTypeEnable(arena, MessageTypeGC);
  MessageTypeEnable(arena, MessageTypeFinalization);

  /* post a couple of interleaved messages of each type */
  postInterleavedMessages(arena);

  /* Disable one of the types */
  MessageTypeDisable(arena, MessageTypeFinalization);

  /* check that we can pull out 2 messages of the other type */
  eatTopMessageOfType(arena, MessageTypeGC);
  eatTopMessageOfType(arena, MessageTypeGC);

  /* check that the queue is empty */
  checkNoMessages(arena);

  /* Post a disabled message */
  postFinalizationMessage(arena);

  /* check that the queue is still empty */
  checkNoMessages(arena);
}


/* testGetEmpty -- test we don't AVER when getting a non-existent message
 *
 */

static void testGetEmpty(Arena arena)
{
  Message message;

  MessageEmpty(arena);
  checkNoMessages(arena);
  die_false(MessageGet(&message, arena, MessageTypeGC),
           "Got non-existent message");
}


extern int main(int argc, char *argv[])
{
  mps_arena_t mpsArena;
  Arena arena; /* an arena for managing the message queue */

  testlib_unused(argc);
  testlib_unused(argv);

  die((mps_res_t)mps_arena_create(&mpsArena, mps_arena_class_vm()),
      "Failed to create arena");
  arena = (Arena)mpsArena;

  testGetEmpty(arena);
  testInterleaving(arena);
  testDisabling(arena);

  printf("\nNo problems detected.\n");
  return 0;
}
