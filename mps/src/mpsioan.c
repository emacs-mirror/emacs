/* impl.c.mpsioan: HARLEQUIN MEMORY POOL SYSTEM I/O IMPLEMENTATION (ANSI)
 *
 * $HopeName: MMsrc!mpsioan.c(MMdevel_event.4) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS developers.
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * There's no way this meets all the reqiurements yet.
 */

#include "mpsio.h"
#include <stdio.h>

#include "mpstd.h"		/* .sunos.warn */
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

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

mps_res_t mps_io_write(mps_io_t mps_io, void *mps_buf, size_t mps_size)
{
  FILE *f = (FILE *)mps_io; /* Should check f == ioFile */
  size_t n;

  n = fwrite(mps_buf, mps_size, 1, f);
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
