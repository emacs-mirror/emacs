/* impl.c.mpsioan: HARLEQUIN MEMORY POOL SYSTEM I/O IMPLEMENTATION (ANSI)
 *
 * $HopeName: MMsrc!mpsioan.c(trunk.4) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
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
