/* spw3i6.c: STACK PROBE FOR 64-BIT WINDOWS
 *
 * $Id$
 * Copyright (c) 2013-2020 Ravenbrook Limited.  See end of file for license.
 *
 * The function StackProbe ensures that the stack has at least depth
 * words available. It achieves this by exploiting an obscure but
 * documented feature of Microsoft's function _alloca: "A stack
 * overflow exception is generated if the space cannot be allocated."
 * <https://docs.microsoft.com/en-gb/cpp/c-runtime-library/reference/alloca>
 */

#include "mpm.h"

#if !defined(MPS_OS_W3)
#error "spw3i6.c is specific to MPS_OS_W3"
#endif

#include <stdlib.h> /* _alloca */


void StackProbe(Size depth)
{
  (void)_alloca(depth * sizeof(Word));
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
