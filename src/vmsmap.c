/* VMS mapping of data and alloc arena for GNU Emacs.
   Copyright (C) 1986, 1987 Free Software Foundation, Inc.
   
   This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Mukesh Prasad.  */

#ifdef VMS

#include <config.h>
#include "lisp.h"
#include <rab.h>
#include <fab.h>
#include <rmsdef.h>
#include <secdef.h>

/* RMS block size */
#define	BLOCKSIZE	512

/* Maximum number of bytes to be written in one RMS write.
 * Must be a multiple of BLOCKSIZE.
 */
#define	MAXWRITE	(BLOCKSIZE * 30)

/* This funniness is to ensure that sdata occurs alphabetically BEFORE the
   $DATA psect and that edata occurs after ALL Emacs psects.  This is
   because the VMS linker sorts all psects in a cluster alphabetically
   during the linking, unless you use the cluster_psect command.  Emacs
   uses the cluster command to group all Emacs psects into one cluster;
   this keeps the dumped data separate from any loaded libraries. */

globaldef {"$D$ATA"} char sdata[512]; /* Start of saved data area */
globaldef {"__DATA"} char edata[512]; /* End of saved data area */

/* Structure to write into first block of map file.
 */

struct map_data
{
  char * sdata;			/* Start of data area */
  char * edata;			/* End of data area */
  int  datablk;			/* Block in file to map data area from/to */
};

static void fill_fab (), fill_rab ();
static int write_data ();

extern char *start_of_data ();
extern int vms_out_initial;	/* Defined in malloc.c */

/* Maps in the data and alloc area from the map file.
 */

int
mapin_data (name)
     char * name;
{
  struct FAB fab;
  struct RAB rab;
  int status, size;
  int inadr[2];
  struct map_data map_data;
  
  /* Open map file. */
  fab = cc$rms_fab;
  fab.fab$b_fac = FAB$M_BIO|FAB$M_GET;
  fab.fab$l_fna = name;
  fab.fab$b_fns = strlen (name);
  status = sys$open (&fab);
  if (status != RMS$_NORMAL)
    {
      printf ("Map file not available, running bare Emacs....\n");
      return 0;			/* Map file not available */
    }
  /* Connect the RAB block */
  rab = cc$rms_rab;
  rab.rab$l_fab = &fab;
  rab.rab$b_rac = RAB$C_SEQ;
  rab.rab$l_rop = RAB$M_BIO;
  status = sys$connect (&rab);
  if (status != RMS$_NORMAL)
    lib$stop (status);
  /* Read the header data */
  rab.rab$l_ubf = &map_data;
  rab.rab$w_usz = sizeof (map_data);
  rab.rab$l_bkt = 0;
  status = sys$read (&rab);
  if (status != RMS$_NORMAL)
    lib$stop (status);
  status = sys$close (&fab);
  if (status != RMS$_NORMAL)
    lib$stop (status);
  if (map_data.sdata != start_of_data ())
    {
      printf ("Start of data area has moved: cannot map in data.\n");
      return 0;
    }
  if (map_data.edata != edata)
    {
      printf ("End of data area has moved: cannot map in data.\n");
      return 0;
    }
  fab.fab$l_fop |= FAB$M_UFO;
  status = sys$open (&fab);
  if (status != RMS$_NORMAL)
    lib$stop (status);
  /* Map data area. */
  inadr[0] = map_data.sdata;
  inadr[1] = map_data.edata;
  status = sys$crmpsc (inadr, 0, 0, SEC$M_CRF | SEC$M_WRT, 0, 0, 0,
		       fab.fab$l_stv, 0, map_data.datablk, 0, 0);
  if (! (status & 1))
    lib$stop (status);
}

/* Writes the data and alloc area to the map file.
 */
mapout_data (into)
     char * into;
{
  struct FAB fab;
  struct RAB rab;
  int status;
  struct map_data map_data;
  int datasize, msize;
 
  if (vms_out_initial)
    {
      error ("Out of initial allocation. Must rebuild emacs with more memory (VMS_ALLOCATION_SIZE).");
      return 0;
    }
  map_data.sdata = start_of_data ();
  map_data.edata = edata;
  datasize = map_data.edata - map_data.sdata + 1;
  map_data.datablk = 2 + (sizeof (map_data) + BLOCKSIZE - 1) / BLOCKSIZE;
  /* Create map file. */
  fab = cc$rms_fab;
  fab.fab$b_fac = FAB$M_BIO|FAB$M_PUT;
  fab.fab$l_fna = into;
  fab.fab$b_fns = strlen (into);
  fab.fab$l_fop = FAB$M_CBT;
  fab.fab$b_org = FAB$C_SEQ;
  fab.fab$b_rat = 0;
  fab.fab$b_rfm = FAB$C_VAR;
  fab.fab$l_alq = 1 + map_data.datablk +
		      ((datasize + BLOCKSIZE - 1) / BLOCKSIZE);
  status = sys$create (&fab);
  if (status != RMS$_NORMAL)
    {
      error ("Could not create map file");
      return 0;
    }
  /* Connect the RAB block */
  rab = cc$rms_rab;
  rab.rab$l_fab = &fab;
  rab.rab$b_rac = RAB$C_SEQ;
  rab.rab$l_rop = RAB$M_BIO;
  status = sys$connect (&rab);
  if (status != RMS$_NORMAL)
    {
      error ("RMS connect to map file failed");
      return 0;
    }
  /* Write the header */
  rab.rab$l_rbf = &map_data;
  rab.rab$w_rsz = sizeof (map_data);
  status = sys$write (&rab);
  if (status != RMS$_NORMAL)
    {
      error ("RMS write (header) to map file failed");
      return 0;
    }
  if (! write_data (&rab, map_data.datablk, map_data.sdata, datasize))
    return 0;
  status = sys$close (&fab);
  if (status != RMS$_NORMAL)
    {
      error ("RMS close on map file failed");
      return 0;
    }
  return 1;
}

static int
write_data (rab, firstblock, data, length)
     struct RAB * rab;
     char * data;
{
  int status;
  
  rab->rab$l_bkt = firstblock;
  while (length > 0)
    {
      rab->rab$l_rbf = data;
      rab->rab$w_rsz = length > MAXWRITE ? MAXWRITE : length;
      status = sys$write (rab, 0, 0);
      if (status != RMS$_NORMAL)
	{
	  error ("RMS write to map file failed");
	  return 0;
	}
      data = &data[MAXWRITE];
      length -= MAXWRITE;
      rab->rab$l_bkt = 0;
    }
  return 1;
}				/* write_data */

#endif /* VMS */

