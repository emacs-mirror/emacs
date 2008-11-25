/* messtest.c: MESSAGE TEST
 *
 * $Id$
 * Copyright (c) 2001-2003 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdlib.h>
#include <stdarg.h>

SRCID(messtest, "$Id$");


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
  MessageTypeFINALIZATION,     /* Message Type */
  dfMessageDelete,             /* Delete */
  MessageNoFinalizationRef,    /* FinalizationRef */
  MessageNoGCLiveSize,         /* GCLiveSize */   
  MessageNoGCCondemnedSize,    /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize, /* GCNotCondemnedSize */
  MessageNoGCStartWhy,         /* GCStartWhy */
  MessageClassSig              /* <design/message/#class.sig.double> */
};


/* DGCMessageClassStruct -- dummy GC message class */

static MessageClassStruct DGCMessageClassStruct = {
  MessageClassSig,             /* sig */
  "DummyGC",                   /* name */
  MessageTypeGC,               /* Message Type */
  dfMessageDelete,             /* Delete */
  MessageNoFinalizationRef,    /* FinalizationRef */
  MessageNoGCLiveSize,         /* GCLiveSize */   
  MessageNoGCCondemnedSize,    /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize, /* GCNoteCondemnedSize */
  MessageNoGCStartWhy,         /* GCStartWhy */
  MessageClassSig              /* <design/message/#class.sig.double> */
};


static void checkNoMessages(Arena arena)
{
  cdie(!MessagePoll(arena), "Queue not empty");
}


static void topMessageType(MessageType *typeReturn, Arena arena)
{
  cdie(MessageQueueType(typeReturn, arena), "Queue empty");
}


/* postDummyMessage -- post a dummy message */

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
  postDummyMessage(arena, &DFMessageClassStruct, MessageTypeFINALIZATION);
}

/* postGCMessage -- post dummy GC message */

static void postGCMessage(Arena arena)
{
  postDummyMessage(arena, &DGCMessageClassStruct, MessageTypeGC);
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
  cdie(MessageGet(&message, arena, type), "No message");
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
  if (type != MessageTypeGC) {
    eatType = MessageTypeGC;
  } else {
    eatType = MessageTypeFINALIZATION;
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
  cdie((topType == type), "Unexpected type");
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
  MessageTypeEnable(arena, MessageTypeFINALIZATION);

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
  MessageTypeEnable(arena, MessageTypeFINALIZATION);

  /* post a couple of interleaved messages of each type */
  postInterleavedMessages(arena);

  /* Disable one of the types */
  MessageTypeDisable(arena, MessageTypeFINALIZATION);

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


/* testGetEmpty -- test we don't AVER when getting a non-existent message */

static void testGetEmpty(Arena arena)
{
  Message message;

  MessageEmpty(arena);
  checkNoMessages(arena);
  cdie(!MessageGet(&message, arena, MessageTypeGC), "Got non-existent message");
}


#define testArenaSIZE (((size_t)64)<<20)

extern int main(int argc, char *argv[])
{
  mps_arena_t mpsArena;
  Arena arena;

  testlib_unused(argc);
  testlib_unused(argv);

  die(mps_arena_create(&mpsArena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  arena = (Arena)mpsArena;

  testGetEmpty(arena);
  testInterleaving(arena);
  testDisabling(arena);

  printf("\nNo problems detected.\n");
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2003 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
