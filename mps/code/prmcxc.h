/* prmcxc.h: MUTATOR CONTEXT (macOS)
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .readership: MPS developers.
 */

#ifndef prmcxc_h
#define prmcxc_h

#include "mpm.h"

#include <mach/mach_types.h>
#if defined(MPS_ARCH_A6)
#include <mach/arm/thread_status.h>
#elif defined(MPS_ARCH_I3) || defined(MPS_ARCH_I6)
#include <mach/i386/thread_status.h>
#else
#error "Unknown macOS architecture"
#endif

typedef struct MutatorContextStruct {
  Sig sig;                      /* <design/sig> */
  MutatorContextVar var;        /* Discriminator. */
  Addr address;                 /* Fault address, if stopped by protection
                                 * fault; NULL if stopped by thread manager. */
  THREAD_STATE_S *threadState;
  /* FIXME: Might need to get the floats in case the compiler stashes
     intermediate values in them. */
} MutatorContextStruct;

extern void MutatorContextInitFault(MutatorContext context, Addr address, THREAD_STATE_S *threadState);
extern void MutatorContextInitThread(MutatorContext context, THREAD_STATE_S *threadState);

#endif /* prmcxc_h */


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
