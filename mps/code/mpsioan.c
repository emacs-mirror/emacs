/* impl.c.mpsioan: RAVENBROOK MEMORY POOL SYSTEM I/O IMPLEMENTATION (ANSI)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: design.mps.io
 */

#include "mpsio.h"

#include "mpstd.h"

#ifdef MPS_OS_SU

extern int fclose (FILE *stream);
extern int fflush (FILE *stream);
extern size_t fwrite (const void *ptr, size_t size, size_t nmemb, FILE *stream);

/* These functions are used in the macro definitions of putc and getc
 * but not declared in stdio.h.
 */
extern int _filbuf(FILE *stream);
extern int _flsbuf(unsigned char c, FILE *stream);

#endif

#ifdef MPS_OS_XC
#include "osxc.h"
#endif

#include <stdio.h>
#include "config.h"  /* to get platform configurations */


static FILE *ioFile = NULL;


mps_res_t mps_io_create(mps_io_t *mps_io_r)
{
  FILE *f;

  if(ioFile != NULL) /* See impl.c.event.trans.log */
    return MPS_RES_LIMIT; /* Cannot currently open more than one log */

  f = fopen("mpsio.log", "wb");
  if(f == NULL)
    return MPS_RES_IO;
 
  *mps_io_r = (mps_io_t)f;
  ioFile = f;
  return MPS_RES_OK;
}


void mps_io_destroy(mps_io_t mps_io)
{
  FILE *f = (FILE *)mps_io;
  ioFile = NULL; /* Should check f == ioFile */
  (void)fclose(f);
}


mps_res_t mps_io_write(mps_io_t mps_io, void *buf, size_t size)
{
  FILE *f = (FILE *)mps_io; /* Should check f == ioFile */
  size_t n;

  n = fwrite(buf, size, 1, f);
  if(n != 1)
    return MPS_RES_IO;
 
  return MPS_RES_OK;
}


mps_res_t mps_io_flush(mps_io_t mps_io)
{
  FILE *f = (FILE *)mps_io; /* Should check f == ioFile */
  int e;
 
  e = fflush(f);
  if(e == EOF)
    return MPS_RES_IO;
 
  return MPS_RES_OK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
