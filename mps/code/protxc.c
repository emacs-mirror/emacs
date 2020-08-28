/* protxc.c: PROTECTION EXCEPTION HANDLER (macOS)
 *
 * $Id$
 * Copyright (c) 2013-2020 Ravenbrook Limited.  See end of file for license.
 *
 * This is the protection exception handling code for macOS using the
 * Mach interface (not pthreads).
 *
 * In Mach, a thread that hits protected memory is suspended, and a message
 * is sent to a separate handler thread.
 *
 * The handler thread can fix things up and continue the suspended thread by
 * sending back a "success" reply.  It can forward the message to another
 * handler of the same kind, or it can forward the message to another handler
 * at the next level out (the levels are thread, task, host) by sending a
 * "fail" reply.
 *
 * In macOS, pthreads are implemented by Mach threads. (The implementation is
 * part of the XNU source code at opensource.apple.com. [copy to import?]) So
 * we can use some pthread interfaces (pthread_create, pthread_once) for
 * convenience in setting up threads.
 *
 * This module sets up an exception handling thread for the EXC_BAD_ACCESS
 * exceptions that will be caused by the MPS shield (read/write barriers).
 * That thread calls the MPS to resolve the condition and allow the mutator
 * thread to progress.
 *
 *
 * REFERENCES
 *
 * [Fuller_2013] "Mach Exception Handlers"; Landon Fuller;
 *               <https://www.mikeash.com/pyblog/friday-qa-2013-01-11-mach-exception-handlers.html>.
 *
 * [XNU] "xnu-2050.22.13" source code;
 *       Apple Computer;
 *       <https://opensource.apple.com/source/xnu/xnu-2050.22.13/>.
 *
 * [Mach_man]  Mach man pages within XNU;
 *             Apple Computer;
 *             <https://opensource.apple.com/source/xnu/xnu-2050.22.13/osfmk/man/>.
 *
 * [Libc] "Libc-825.26" source code;
 *        Apple Computer;
 *        <https://opensource.apple.com/source/Libc/Libc-825.26/>.
 *
 *
 * TRANSGRESSIONS
 *
 * .trans.stdlib: It's OK to use the C library from here because we know
 * we're on macOS and not freestanding.  In particular, we use memcpy.
 *
 * .trans.must: Various OS calls are asserted to succeed, since there isn't
 * really a dynamic reason they should fail, so it must be a static error.
 * In a RASH build, they don't stop the program, just in case it's able to
 * limp along.
 */

#include "mpm.h"

#if !defined(MPS_OS_XC)
#error "protxc.c is specific to MPS_OS_XC"
#endif

#include "prmcxc.h"
#include "protxc.h"

#include <mach/mach_port.h>
#include <mach/mach_init.h>
#include <mach/task.h>
#include <mach/thread_act.h>
#include <mach/thread_status.h>
#include <mach/mach_error.h>
#include <mach/i386/thread_status.h>
#include <mach/exc.h>
#include <pthread.h>
#include <stdlib.h> /* see .trans.stdlib */
#include <stdio.h> /* see .trans.stdlib */

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
 * 32-bit structures are available in /usr/include/mach.  Note that these
 * 32- and 64-bit message structures are independent of the architecture
 * word width, so we choose to use 64-bit on both I3 and I6.
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
 * handler, initialized by protSetupInner.
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
 * macOS provides a function exc_server (in
 * /usr/lib/system/libsystem_kernel.dylib) that's documented in the XNU
 * sources <https://opensource.apple.com/source/xnu/xnu-4570.41.2/osfmk/man/exc_server.html.auto.html>
 * and generated by the Mach Interface Generator (mig).  It unpacks
 * an exception message structure and calls one of several handler functions.
 * We can't use it because:
 *
 *   1. It's hard-wired to call certain functions by name.  The MPS can't
 *      steal those names in case the client program is using them too.
 *
 *   2. It fails anyway in Xcode's default "Release" build with hidden
 *      symbols, because it uses dlsym to find those handler functions, and
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

  AVER(MACH_PORT_VALID(protExcPort));
  mr = mach_msg(&request.Head,
                /* option */ MACH_RCV_MSG,
                /* send_size */ 0,
                /* receive_limit */ sizeof(request),
                /* receive_name */ protExcPort,
                /* timeout */ MACH_MSG_TIMEOUT_NONE,
                /* notify */ MACH_PORT_NULL);
  AVER(mr == MACH_MSG_SUCCESS);
  if (mr != MACH_MSG_SUCCESS)
    mach_error("ERROR: MPS mach_msg recv\n", mr);  /* .trans.must */

  /* 2407 is the id for the 64-bit exception requests we asked for in
     ProtThreadRegister, with state and identity
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
    MutatorContextStruct context;

    /* The cast via Word suppresses "cast to pointer from integer of
       different size" warnings in GCC, for the  XCI3GC build. */
    MutatorContextInitFault(&context, (Addr)(Word)request.code[1],
                            (void *)request.old_state);

    if (ArenaAccess(context.address,
                    AccessREAD | AccessWRITE,
                    &context)) {
      /* Send a reply that will cause the thread to continue.
         Note that ArenaAccess may have updated request.old_state
         via context.thread_state, and that will get copied to the
         reply and affect the state the thread resumes in. */
      protBuildReply(&reply, &request, KERN_SUCCESS);
      protMustSend(&reply.Head);
      return;
    }
  }

  /* We didn't handle the exception -- it wasn't one of ours. */

  /* .assume.only-port: We assume that there was no previously installed
     exception port.  This is checked in ProtThreadRegister, and we don't
     check it again here to avoid the extra system call.  If there
     were, we must arrange to forward the exception message to the
     previous port.  This module used to do that because it installed a
     task-wide exception handler, but the code is pretty hairy and not
     necessary as long as the MPS is registering threads individually.
     If we ever need to reinstate that code, look at
     https://info.ravenbrook.com/project/mps/prototype/2013-06-24/machtest */

  protBuildReply(&reply, &request, KERN_FAILURE);
  protMustSend(&reply.Head);
}


/* protCatchThread -- exception handler thread loop.
 *
 * Note that this thread does *not* have a thread-specific exception handler
 * installed.  This means that an exception-causing bug in the exception
 * handler won't cause a deadlock.
 */

ATTRIBUTE_NORETURN
static void *protCatchThread(void *p)
{
  UNUSED(p);
  for (;;)
    protCatchOne();
}


/* ProtThreadRegister -- register a thread for protection exception handling */

void ProtThreadRegister(void)
{
  kern_return_t kr;
  mach_msg_type_number_t old_exception_count = 1;
  exception_mask_t old_exception_masks;
  exception_behavior_t behavior;
  mach_port_t old_exception_ports;
  exception_behavior_t old_behaviors;
  thread_state_flavor_t old_flavors;
  mach_port_t self;

  self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));

  /* Ask to receive EXC_BAD_ACCESS exceptions on our port, complete
     with thread state and identity information in the message.
     The MACH_EXCEPTION_CODES flag causes the code fields to be
     passed 64-bits wide, matching protRequestStruct [Fuller_2013]. */
  behavior = (exception_behavior_t)(EXCEPTION_STATE_IDENTITY | MACH_EXCEPTION_CODES);
  AVER(MACH_PORT_VALID(protExcPort));
  kr = thread_swap_exception_ports(self,
                                   EXC_MASK_BAD_ACCESS,
                                   protExcPort,
                                   behavior,
                                   THREAD_STATE_FLAVOR,
                                   &old_exception_masks,
                                   &old_exception_count,
                                   &old_exception_ports,
                                   &old_behaviors,
                                   &old_flavors);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS thread_swap_exception_ports", kr); /* .trans.must */
  AVER(old_exception_masks == EXC_MASK_BAD_ACCESS);
  AVER(old_exception_count == 1);
  AVER(old_exception_ports == MACH_PORT_NULL
       || old_exception_ports == protExcPort); /* .assume.only-port */
}


/* protExcThreadStart -- create exception port, register the current
 * thread with that port, and create a thread to handle exception
 * messages.
 */

static void protExcThreadStart(void)
{
  kern_return_t kr;
  mach_port_t self;
  pthread_t excThread;
  int pr;

  /* Create a port to send and receive exceptions. */
  self = mach_task_self();
  AVER(MACH_PORT_VALID(self));
  kr = mach_port_allocate(self,
                          MACH_PORT_RIGHT_RECEIVE,
                          &protExcPort);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS mach_port_allocate", kr); /* .trans.must */
  AVER(MACH_PORT_VALID(protExcPort));

  /* Allow me to send exceptions on this port. */
  /* TODO: Find out why this is necessary. */
  self = mach_task_self();
  AVER(MACH_PORT_VALID(self));
  kr = mach_port_insert_right(self,
                              protExcPort, protExcPort,
                              MACH_MSG_TYPE_MAKE_SEND);
  AVER(kr == KERN_SUCCESS);
  if (kr != KERN_SUCCESS)
    mach_error("ERROR: MPS mach_port_insert_right", kr); /* .trans.must */

  /* We don't require the mutator to register the sole thread in a
   * single-threaded program, so register it automatically now. */
  ProtThreadRegister();

  /* Launch the exception handling thread. We use pthread_create
   * because it's much simpler than setting up a thread from scratch
   * using Mach, and that's basically what it does. See [Libc]
   * <https://opensource.apple.com/source/Libc/Libc-825.26/pthreads/pthread.c> */
  pr = pthread_create(&excThread, NULL, protCatchThread, NULL);
  AVER(pr == 0);
  if (pr != 0)
    fprintf(stderr, "ERROR: MPS pthread_create: %d\n", pr); /* .trans.must */
}


/* protAtForkChild -- support for fork()
 * <design/thread-safety#.sol.fork.exc-thread>
 */

static void protAtForkChild(void)
{
  /* Restart the exception handling thread
     <design/thread-safety#.sol.fork.exc-thread>. */
  protExcThreadStart();
}


/* ProtSetup -- set up protection exception handling */

static void protSetupInner(void)
{
  protExcThreadStart();

  /* Install fork handlers <design/thread-safety#.sol.fork.atfork>. */
  pthread_atfork(NULL, NULL, protAtForkChild);
}

void ProtSetup(void)
{
  int pr;
  static pthread_once_t prot_setup_once = PTHREAD_ONCE_INIT;

  /* ProtSetup may be called several times if the client creates more than
     one arena, but we still only want one exception handling thread. */
  pr = pthread_once(&prot_setup_once, protSetupInner);
  AVER(pr == 0);
  if (pr != 0)
    fprintf(stderr, "ERROR: MPS pthread_once: %d\n", pr); /* .trans.must */
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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
