/* protxc.c: PROTECTION EXCPETION HANDLER FOR OS X MACH
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * This is the protection exception handling code for Mac OS X using the
 * Mach interface (not pthreads).
 *
 * In Mach, a thread that hits protected memory is suspended, and a message
 * is sent to a separate handler thread.
 * The handler thread can fix things up and continue the suspended thread by
 * sending back a "success" reply.  It can forward the message to another
 * handler of the same kind, or it can forward the message to another handler
 * at the next level out (the levels are thread, task, host) by sending a
 * "fail" reply.
 *
 * In Mac OS X, pthreads are implemented by Mach threads.  (The implementation
 * is part of the XNU source code at opensource.apple.com.  [copy to import?])
 * So we can use some pthread interfaces for convenience in setting up threads.
 *
 * This module sets up an exception handling thread for the EXC_BAD_ACCESS
 * exceptions that will be caused by the MPS shield (read/write barriers).
 * That thread calls the MPS to resolve the condition and allow the mutator
 * thread to progress.
 *
 * That part is fairly simple.  Most of the code in this module is concerned
 * with decoding Mach messages and re-encoding them in order to forward them
 * on to other exception handlers.
 *
 *
 * SOURCES
 *
 * .source.man: <http://felinemenace.org/~nemo/mach/manpages/>
 *              <http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/>
 *
 *
 * REFERENCES
 *
 * [Fuller_2013] "Mach Exception Handlers"; Landon Fuller;
 *               <http://www.mikeash.com/pyblog/friday-qa-2013-01-11-mach-exception-handlers.html>
 *
 *
 * TRANSGRESSIONS
 *
 * .trans.stdlib: It's OK to use the C library from here because we know
 * we're on OS X and not freestanding.  In particular, we use memcpy.
 *
 * .trans.must: Various OS calls are asserted to succeed, since there isn't
 * really a dynamic reason they should fail, so it must be a static error.
 * In a RASH build, they don't stop the program, just in case it's able to
 * limp along.
 */

#include "mpm.h"
#include "prmcxc.h"
#include "protxc.h"

#include <stdlib.h> /* see .trans.stdlib */

#include <pthread.h>

#include <mach/mach_port.h>
#include <mach/mach_init.h>
#include <mach/task.h>
#include <mach/thread_act.h>
#include <mach/thread_status.h>
#include <mach/mach_error.h>
#include <mach/i386/thread_status.h>
#include <mach/exc.h>

#if !defined(MPS_OS_XC)
#error "protxc.c is OS X specific"
#endif
#ifndef PROTECTION
#error "protxc.c implements protection, but PROTECTION is not set"
#endif

SRCID(protxc, "$Id$");


/* Exception request message structure
 *
 * The following declaration is extracted by running the Mach Interface
 * Generator like this:
 *
 *   mig /usr/include/mach/mach_exc.defs
 *
 * then copying it from the resulting mach_exc.h file.  This gets the
 * structure with 64-bit code fields, corresponding to the exception
 * behaviour EXCEPTION_STATE_IDENTITY | MACH_EXCEPTION_CODES.  Only the
 * 32-bit structures are available in /usr/include/mach.
 */

#ifdef  __MigPackStructs
#pragma pack(4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                /* start of the kernel processed data */
                mach_msg_body_t msgh_body;
                mach_msg_port_descriptor_t thread;
                mach_msg_port_descriptor_t task;
                /* end of the kernel processed data */
                NDR_record_t NDR;
                exception_type_t exception;
                mach_msg_type_number_t codeCnt;
                int64_t code[2];
                int flavor;
                mach_msg_type_number_t old_stateCnt;
                natural_t old_state[224];
        } __Request__mach_exception_raise_state_identity_t;
#ifdef  __MigPackStructs
#pragma pack()
#endif


/* Local short names, for convenience. */
typedef __Request__mach_exception_raise_state_identity_t protRequestStruct;
typedef __Reply__exception_raise_state_identity_t protReplyStruct;


/* protExcPort -- exception message receiving Mach port
 *
 * This will be the port that will receive messages for our exception
 * handler, initialized by protSetup.
 */

static mach_port_name_t protExcPort = MACH_PORT_NULL;


/* protBuildReply -- build a reply message based on a request. */

static void protBuildReply(protReplyStruct *reply,
                           protRequestStruct *request,
                           kern_return_t ret_code)
{
  mach_msg_size_t state_size;
  reply->Head.msgh_bits =
    MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(request->Head.msgh_bits), 0);
  reply->Head.msgh_remote_port = request->Head.msgh_remote_port;
  reply->Head.msgh_local_port = MACH_PORT_NULL;
  reply->Head.msgh_reserved = 0;
  reply->Head.msgh_id = request->Head.msgh_id + 100;
  reply->NDR = request->NDR;
  reply->RetCode = ret_code;
  reply->flavor = request->flavor;
  reply->new_stateCnt = request->old_stateCnt;
  state_size = reply->new_stateCnt * sizeof(natural_t);
  AVER(sizeof(reply->new_state) >= state_size);
  memcpy(reply->new_state, request->old_state, state_size);
  /* If you use sizeof(reply) for reply->Head.msgh_size then the state
     gets ignored. */
  reply->Head.msgh_size = offsetof(protReplyStruct, new_state) + state_size;
}


/* protMustSend -- send a Mach message without fail, probably */

static void protMustSend(mach_msg_header_t *head)
{
  kern_return_t kr;
  kr = mach_msg(head,
                MACH_SEND_MSG,
                head->msgh_size,
                /* recv_size */ 0,
                MACH_PORT_NULL,
                MACH_MSG_TIMEOUT_NONE,
                MACH_PORT_NULL);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS mach_msg send", kr); /* .trans.must */
}


/* protCatchOne -- catch one EXC_BAD_ACCESS exception message.
 *
 * Mac OS X provides a function exc_server (in
 * /usr/lib/system/libsystem_kernel.dylib) that's documented in the XNU
 * sources and generated by the Mach Interface Generator (mig).  It unpacks
 * an exception message structure and calls one of several handler functions.
 * We can't use it because:
 *
 *   1. It's hard-wired to call certain functions by name.  The MPS can't
 *      steal those names in case the client program is using them too.
 *
 *   2. It fails anyway in Xcode's default "Release" build with hidden
 *      symbols, because it uses dlsym to find those handler functins, and
 *      dlsym can't find them.
 *
 * So instead this function duplicates the work of exc_server, and is shorter
 * because it's specialised for protection exceptions of a single behaviour
 * and flavour.  It is also more flexible and can dispatch to any function
 * we want.  The downside is that it depends on various unpublished stuff
 * like the code numbers for certain messages.
 */

static void protCatchOne(void)
{
  protRequestStruct request;
  mach_msg_return_t mr;
  protReplyStruct reply;

  mr = mach_msg(&request.Head,
                MACH_RCV_MSG,
                /* send_size */ 0,
                /* receive_size */ sizeof(request),
                protExcPort,
                /* timeout */ 0,
                /* notify */ MACH_PORT_NULL);
  AVER(mr == MACH_MSG_SUCCESS);
  if (mr != MACH_MSG_SUCCESS)
    mach_error("ERROR: MPS mach_msg recv\n", mr);  /* .trans.must */

  /* 2407 is the id for 64-bit exception requests with state and identity
     information, determined by experimentation and confirmed by 
     running mig on /usr/include/mach/mach_exc.defs */
  AVER(request.Head.msgh_id == 2407);
  AVER(request.Head.msgh_local_port == protExcPort);
  AVER(request.task.name == mach_task_self());
  AVER(request.exception == EXC_BAD_ACCESS);
  AVER(request.codeCnt == 2);
  AVER(request.old_stateCnt == THREAD_STATE_COUNT);
  AVER(request.flavor == THREAD_STATE_FLAVOR);
  
  /* TODO: This could dispatch to separate worker threads, in order to
     spread scanning work across several cores once the MPS can be
     re-entered. */

  if (request.code[0] == KERN_PROTECTION_FAILURE) {
    MutatorFaultContextStruct mfcStruct;

    mfcStruct.address = (Addr)request.code[1];
    AVER(sizeof(mfcStruct.thread_state) == sizeof(THREAD_STATE_T));
    /* FIXME: Pass a pointer in mfcStruct instead, to avoid copies. */
    memcpy(&mfcStruct.thread_state, request.old_state, sizeof(mfcStruct.thread_state));
  
    if (ArenaAccess(mfcStruct.address,
                    AccessREAD | AccessWRITE,
                    &mfcStruct)) {
      /* Send a reply that will cause the thread to continue. */
      memcpy(request.old_state, &mfcStruct.thread_state, sizeof(mfcStruct.thread_state));
      protBuildReply(&reply, &request, KERN_SUCCESS);
      protMustSend(&reply.Head);
      return;
    }
  }

  /* We didn't handle the exception -- it wasn't one of ours. */

  /* .assume.only-port: We assume that there was no previously installed
     exception port.  (This is checked in ProtThreadRegister.)  If there
     were, we must arrange to forward the exception message to the
     previous port.  This module used to do that because it installed a
     task-wide exception handler, but the code is pretty hairy and not
     necessary as long as the MPS is registering threads individually.
     If we ever need to reinstate that code, look at
     //info.ravenbrook.com/project/mps/branch/2013-06-18/macosx-threads/code/protxc.c#3 */
  
  protBuildReply(&reply, &request, KERN_FAILURE);
  protMustSend(&reply.Head);
}


/* protCatchThread -- exception handler thread loop.
 *
 * Note that this thread does *not* have a thread-specific exception handler
 * installed.  This means that an exception-causing bug in the exception
 * handler won't cause a deadlock.
 */

static void *protCatchThread(void *p) {
  for (;;)
    protCatchOne();
  return p;
}


/* ProtThreadRegister -- register a thread for protection exception handling */

extern void ProtThreadRegister(Bool setup)
{
  kern_return_t kr;
  mach_msg_type_number_t old_cnt;
  exception_mask_t old_mask;
  exception_behavior_t behaviour;
  mach_port_t old_port;
  exception_behavior_t old_behaviour;
  thread_state_flavor_t old_flavor;
  mach_port_t self;
  static mach_port_t setupThread = MACH_PORT_NULL;

  self = mach_thread_self();
  
  /* Avoid setting up the exception handler for the setup thread twice,
     in the case where the mutator registers that thread twice. */
  if (setup) {
    AVER(setupThread == MACH_PORT_NULL);
    setupThread = self;
  } else {
    AVER(setupThread != MACH_PORT_NULL);
    if (self == setupThread)
      return;
  }
  
  /* Ask to receive EXC_BAD_ACCESS exceptions on our port, complete
     with thread state and identity information in the message.
     The MACH_EXCEPTION_CODES flag causes the code fields to be
     passed 64-bits wide, matching protRequestStruct. */
  behaviour = (exception_behavior_t)(EXCEPTION_STATE_IDENTITY | MACH_EXCEPTION_CODES);
  kr = thread_swap_exception_ports(self,
                                   EXC_MASK_BAD_ACCESS,
                                   protExcPort,
                                   behaviour,
                                   THREAD_STATE_FLAVOR,
                                   &old_mask,
                                   &old_cnt,
                                   &old_port,
                                   &old_behaviour,
                                   &old_flavor);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS thread_swap_exception_ports", kr); /* .trans.must */
  AVER(old_mask == EXC_MASK_BAD_ACCESS);
  AVER(old_cnt == 1);
  AVER(old_port == MACH_PORT_NULL); /* .assume.only-port */
}


/* ProtSetup -- set up protection exception handling */

static void protSetup(void)
{
  kern_return_t kr;
  int pr;
  pthread_t excThread;

  /* Create a port to send and receive exceptions. */
  kr = mach_port_allocate(mach_task_self(),
                          MACH_PORT_RIGHT_RECEIVE,
                          &protExcPort);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS mach_port_allocate", kr); /* .trans.must */
  
  /* Allow me to send exceptions on this port. */
  /* TODO: Find out why this is necessary. */
  kr = mach_port_insert_right(mach_task_self(),
                              protExcPort, protExcPort    ,
                              MACH_MSG_TYPE_MAKE_SEND);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS mach_port_insert_right", kr); /* .trans.must */
  
  ProtThreadRegister(TRUE);

  /* Launch the exception handling thread.  We use pthread_create because
     it's much simpler than setting up a thread from scratch using Mach,
     and that's basically what it does (by inspection of the XNU source
     code). */
  pr = pthread_create(&excThread, NULL, protCatchThread, NULL);
  AVER(pr == 0);
  if (pr != 0)
    fprintf(stderr, "ERROR: MPS pthread_create: %d\n", pr); /* .trans.must */
}

void ProtSetup(void)
{
  int pr;
  static pthread_once_t prot_setup_once = PTHREAD_ONCE_INIT;

  /* ProtSetup may be called several times if the client creates more than
     one arena, but we still only want one exception handling thread. */
  pr = pthread_once(&prot_setup_once, protSetup);
  AVER(pr == 0);
  if (pr != 0)
    fprintf(stderr, "ERROR: MPS pthread_once: %d\n", pr); /* .trans.must */
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
