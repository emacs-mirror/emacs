/* diag.c: MEMORY POOL MANAGER DIAGNOSTICS
 *
 * $Id$
 * Copyright (c) 2007 Ravenbrook Limited.  See end of file for license.
 *
 */

#include <stdarg.h>

#include "mpm.h"
#include "mpslib.h" /* for mps_lib_stdout */


/* DIAG_WITH_STREAM_AND_WRITEF -- Diagnostic output channel
 *
 * Only used for DIAG_WITH_STREAM_AND_WRITEF; see config.h.
 */

Bool DiagEnabledGlobal = TRUE;

Bool DiagIsOn(void)
{
  return DiagEnabledGlobal;
}

mps_lib_FILE *DiagStream(void)
{
  return mps_lib_stdout;
}

const char *DiagTagGlobal = NULL;

static void DiagTagBegin(const char *tag)
{
  Res res;

  AVER(DiagTagGlobal == NULL);
  DiagTagGlobal = tag;
  res = WriteF(DiagStream(), "MPS.$S { ", tag, NULL);
  AVER(res == ResOK);
}

static void DiagTagEnd(const char *tag)
{
  Res res;

  AVER(DiagTagGlobal != NULL);
  /* AVER(strequal(DiagTagGlobal, tag)); */
  res = WriteF(DiagStream(), "} MPS.$S\n", tag, NULL);
  DiagTagGlobal = NULL;
  AVER(DiagTagGlobal == NULL);
}

void DiagSingleF(const char *tag, ...)
{
  va_list args;
  Res res;

  DiagTagBegin(tag);

  va_start(args, tag);
  res = WriteF_v(DiagStream(), args);
  AVER(res == ResOK);
  va_end(args);

  DiagTagEnd(tag);
}

void DiagFirstF(const char *tag, ...)
{
  va_list args;
  Res res;

  DiagTagBegin(tag);

  va_start(args, tag);
  res = WriteF_v(DiagStream(), args);
  AVER(res == ResOK);
  va_end(args);
}

void DiagMoreF(const char *firstformat, ...)
{
  va_list args;
  Res res;

  /* ISO C says there must be at least one named parameter: hence */
  /* the named firstformat.  It only looks different: there is no */
  /* change from the expected WriteF protocol.  (In particular, */
  /* firstformat may legally be NULL, with the variable part empty). */

  va_start(args, firstformat);
  res = WriteF_firstformat_v(DiagStream(), firstformat, args);
  AVER(res == ResOK);
  va_end(args);
}

void DiagEnd(const char *tag)
{
  DiagTagEnd(tag);
}

extern void diag_test(void);

void diag_test(void)
{
  DIAG_SINGLEF(( "TestTag1", "text $U.\n", 42, NULL ));

  DIAG_FIRSTF(( "TestTag2", "text $U.\n", 42, NULL ));
  DIAG_MOREF(( NULL ));
  DIAG_MOREF(( "string $S.\n", "fooey!", NULL ));
  DIAG_MOREF(( NULL ));
  DIAG_MOREF(( "Another string $S.\n", "baloney!", NULL ));
  DIAG_END( "TestTag2" );
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2007 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
