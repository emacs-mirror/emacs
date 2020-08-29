/* testthr.h: MULTI-THREADED TEST INTERFACE
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Simple interface to threads that makes it possible to
 * write test cases that are portable between Windows (using the
 * implementation in testthrw3.c) and Unix (using the implementation
 * in testthrix.c). <design/testthr>.
 */

#ifndef testthr_h
#define testthr_h

#include "mpstd.h"


/* testthr_routine_t -- type of thread routines
 *
 * Use the pthread type here and convert back and forth in the Windows
 * implementation.
 */
typedef void *(*testthr_routine_t)(void *);


/* testthr_t -- type of thread identifiers
 *
 * It is necessary to define it here (even though it requires some
 * #ifdefs) so that clients can allocate storage for them.
 */

#if defined(MPS_OS_W3)

#include "mpswin.h"

/* On Windows, a thread is identified by a HANDLE.
 * <https://docs.microsoft.com/en-gb/windows/desktop/WinProg/windows-data-types>
 * But use a structure so that the thread has somewhere to store its
 * result for use by testthr_join.
 */
typedef struct testthr_t {
  HANDLE handle;
  testthr_routine_t start;
  void *arg;                  /* argument to pass to start */
  void *result;               /* result returned from start */
} testthr_t;

#elif defined(MPS_OS_FR) || defined(MPS_OS_LI) || defined(MPS_OS_XC)

#include <pthread.h>

/* In pthreads, a thread is identified by a pthread_t, which is
 * allowed "to be defined as a structure" [IEEE Std 1003.1, sys/types.h]
 * <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_types.h.html>
 */
typedef pthread_t testthr_t;

#else
#error "Unknown platform: can't determine a type for testthr_t."
#endif


/* testthr_create -- create a thread
 *
 * Store the identifier of the newly created thread in *thread_o, and
 * call start, passing arg as the single parameter.
 */

void testthr_create(testthr_t *thread_o, testthr_routine_t start, void *arg);


/* testthr_join -- wait for a thread to complete
 *
 * Suspend execution of the calling thread until the target thread
 * terminates (if necessary), and if result_o is non-NULL, update
 * *result_o with the return value of the thread's start.
 */

void testthr_join(testthr_t *thread, void **result_o);

#endif /* testthr_h */


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
