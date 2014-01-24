/* mpsioan.c: RAVENBROOK MEMORY POOL SYSTEM I/O IMPLEMENTATION (ANSI)
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: <design/io/>
 */

#include "mpsio.h"
#include "mpstd.h"

/* We don't want to use the ANSI assert() to check that the interface
 * is being used correctly, because it's not controlled by the MPS
 * variety mechanism: we might end up with assertions being turned on
 * in the HOT variety or turned off in the COOL variety (depending on
 * whether or not the client program compiles the MPS with NDEBUG
 * defined). So we include "check.h" and use AVER() instead. See
 * job003504. If you are developing your own plinth, you should
 * consider whether to use your own preferred assertion mechanism
 * instead.
 */
#include "check.h"
#include "config.h"  /* to get platform configurations */

#include <stdio.h>
#include <stdlib.h>


static FILE *ioFile = NULL;

#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: fopen.  See job001934. */
#pragma warning( disable : 4996 )
#endif

mps_res_t mps_io_create(mps_io_t *mps_io_r)
{
  FILE *f;
  const char *filename;

  if(ioFile != NULL) /* See <code/event.c#trans.log> */
    return MPS_RES_LIMIT; /* Cannot currently open more than one log */

  filename = getenv("MPS_TELEMETRY_FILENAME");
  if(filename == NULL)
    filename = "mpsio.log";

  f = fopen(filename, "wb");
  if(f == NULL)
    return MPS_RES_IO;
 
  *mps_io_r = (mps_io_t)f;
  ioFile = f;
  return MPS_RES_OK;
}


void mps_io_destroy(mps_io_t mps_io)
{
  FILE *f = (FILE *)mps_io;
  AVER(f == ioFile);
  AVER(f != NULL);

  ioFile = NULL;
  (void)fclose(f);
}


mps_res_t mps_io_write(mps_io_t mps_io, void *buf, size_t size)
{
  FILE *f = (FILE *)mps_io;
  size_t n;
  AVER(f == ioFile);
  AVER(f != NULL);

  n = fwrite(buf, size, 1, f);
  if(n != 1)
    return MPS_RES_IO;
 
  return MPS_RES_OK;
}


mps_res_t mps_io_flush(mps_io_t mps_io)
{
  FILE *f = (FILE *)mps_io;
  int e;
  AVER(f == ioFile);
  AVER(f != NULL);
 
  e = fflush(f);
  if(e == EOF)
    return MPS_RES_IO;
 
  return MPS_RES_OK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
