/* airtest.c: AMBIGUOUS INTERIOR REFERENCE TEST
 *
 * $Id: //info.ravenbrook.com/project/mps/branch/2014-01-15/nailboard/code/fotest.c#1 $
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * This test case creates a bunch of string objects, registers them
 * for finalization, and then discards the base pointers to those
 * objects, keeping only ambiguous interior references.
 *
 * If any of these objects are finalized, then this means that the
 * ambiguous interior references has failed to keep the object alive.
 */

#include "mps.h"
#include "fmtscheme.h"
#include "testlib.h"

void test_main(void)
{
  size_t size = 1u << 8;
  size_t i, j;
  char *s[10];
  for (j = 0; j < 10; ++j) {
    obj_t obj = scheme_make_string(size, NULL);
    mps_addr_t ref = obj;
    mps_finalize(scheme_arena, &ref);
    s[j] = obj->string.string;
    *s[j]++ = '0' + (char)j;
    *s[j] = '\0';
  }
  mps_message_type_enable(scheme_arena, mps_message_type_finalization());
  for (i = 0; i + 3 < size; ++i) {
    mps_message_t msg;
    for (j = 0; j < 10; ++j) {
      *s[j]++ = '.';
      *s[j] = '\0';
    }
    mps_arena_collect(scheme_arena);
    mps_arena_release(scheme_arena);
    if (mps_message_get(&msg, scheme_arena, mps_message_type_finalization())) {
      mps_addr_t ref;
      obj_t o;
      mps_message_finalization_ref(&ref, scheme_arena, msg);
      o = ref;
      error("wrongly finalized '%s' at %p", o->string.string, o);
    }
  }
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

