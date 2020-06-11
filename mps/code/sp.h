/* sp.h: STACK PROBE INTERFACE
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 */

#ifndef sp_h
#define sp_h

#include "mpmtypes.h"


/* StackProbe -- probe above the stack to provoke early stack overflow
 *
 * This function should check that the stack has at least depth words
 * available, and if not, then provoke a stack overflow exception or
 * protection fault. The purpose is to ensure that the exception is
 * generated before taking the arena lock where it can be handled
 * safely, rather than at some later point where the arena lock is
 * held and so handling the exception may cause the MPS to be entered
 * recursively.
 */

extern void StackProbe(Size depth);


#endif /* sp_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
