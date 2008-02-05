/* Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


/*
 * unexec.c - Convert a running program into an a.out file.
 *
 * Author:	Spencer W. Thomas
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified heavily since then.
 *
 * Synopsis:
 *	unexec (new_name, a_name, data_start, bss_start, entry_address)
 *	char *new_name, *a_name;
 *	unsigned data_start, bss_start, entry_address;
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If a_name is non-NULL, the symbol table will be taken from the given file.
 * On some machines, an existing a_name file is required.
 *
 * The boundaries within the a.out file may be adjusted with the data_start
 * and bss_start arguments.  Either or both may be given as 0 for defaults.
 *
 * Data_start gives the boundary between the text segment and the data
 * segment of the program.  The text segment can contain shared, read-only
 * program code and literal data, while the data segment is always unshared
 * and unprotected.  Data_start gives the lowest unprotected address.
 * The value you specify may be rounded down to a suitable boundary
 * as required by the machine you are using.
 *
 * Specifying zero for data_start means the boundary between text and data
 * should not be the same as when the program was loaded.
 * If NO_REMAP is defined, the argument data_start is ignored and the
 * segment boundaries are never changed.
 *
 * Bss_start indicates how much of the data segment is to be saved in the
 * a.out file and restored when the program is executed.  It gives the lowest
 * unsaved address, and is rounded up to a page boundary.  The default when 0
 * is given assumes that the entire data segment is to be stored, including
 * the previous data and bss as well as any additional storage allocated with
 * break (2).
 *
 * The new file is set up to start at entry_address.
 *
 * If you make improvements I'd like to get them too.
 * harpo!utah-cs!thomas, thomas@Utah-20
 *
 */

/* Modified to support SysVr3 shared libraries by James Van Artsdalen
 * of Dell Computer Corporation.  james@bigtex.cactus.org.
 */

/* There are several compilation parameters affecting unexec:

* COFF

Define this if your system uses COFF for executables.

* COFF_ENCAPSULATE

Define this if you are using the GNU coff encapsulated a.out format.
This is closer to a.out than COFF. You should *not* define COFF if
you define COFF_ENCAPSULATE

Otherwise we assume you use Berkeley format.

* NO_REMAP

Define this if you do not want to try to save Emacs's pure data areas
as part of the text segment.

Saving them as text is good because it allows users to share more.

However, on machines that locate the text area far from the data area,
the boundary cannot feasibly be moved.  Such machines require
NO_REMAP.

Also, remapping can cause trouble with the built-in startup routine
/lib/crt0.o, which defines `environ' as an initialized variable.
Dumping `environ' as pure does not work!  So, to use remapping,
you must write a startup routine for your machine in Emacs's crt0.c.
If NO_REMAP is defined, Emacs uses the system's crt0.o.

* SECTION_ALIGNMENT

Some machines that use COFF executables require that each section
start on a certain boundary *in the COFF file*.  Such machines should
define SECTION_ALIGNMENT to a mask of the low-order bits that must be
zero on such a boundary.  This mask is used to control padding between
segments in the COFF file.

If SECTION_ALIGNMENT is not defined, the segments are written
consecutively with no attempt at alignment.  This is right for
unmodified system V.

* SEGMENT_MASK

Some machines require that the beginnings and ends of segments
*in core* be on certain boundaries.  For most machines, a page
boundary is sufficient.  That is the default.  When a larger
boundary is needed, define SEGMENT_MASK to a mask of
the bits that must be zero on such a boundary.

* A_TEXT_OFFSET(HDR)

Some machines count the a.out header as part of the size of the text
segment (a_text); they may actually load the header into core as the
first data in the text segment.  Some have additional padding between
the header and the real text of the program that is counted in a_text.

For these machines, define A_TEXT_OFFSET(HDR) to examine the header
structure HDR and return the number of bytes to add to `a_text'
before writing it (above and beyond the number of bytes of actual
program text).  HDR's standard fields are already correct, except that
this adjustment to the `a_text' field has not yet been made;
thus, the amount of offset can depend on the data in the file.

* A_TEXT_SEEK(HDR)

If defined, this macro specifies the number of bytes to seek into the
a.out file before starting to write the text segment.

* EXEC_MAGIC

For machines using COFF, this macro, if defined, is a value stored
into the magic number field of the output file.

* ADJUST_EXEC_HEADER

This macro can be used to generate statements to adjust or
initialize nonstandard fields in the file header

* ADDR_CORRECT(ADDR)

Macro to correct an int which is the bit pattern of a pointer to a byte
into an int which is the number of a byte.

This macro has a default definition which is usually right.
This default definition is a no-op on most machines (where a
pointer looks like an int) but not on all machines.

*/

#ifndef emacs
#define PERROR(arg) perror (arg); return -1
#else
#define IN_UNEXEC
#include <config.h>
#define PERROR(file) report_error (file, new)
#endif

#ifndef CANNOT_DUMP  /* all rest of file!  */

#if defined(COFF) && defined(HAVE_COFF_H)
#include <coff.h>
#ifdef MSDOS
#if __DJGPP__ > 1
#include <fcntl.h>  /* for O_RDONLY, O_RDWR */
#include <crt0.h>   /* for _crt0_startup_flags and its bits */
static int save_djgpp_startup_flags;
#endif /* __DJGPP__ > 1 */
#define filehdr external_filehdr
#define scnhdr external_scnhdr
#define syment external_syment
#define auxent external_auxent
#define n_numaux e_numaux
#define n_type e_type
struct aouthdr
{
  unsigned short	magic;	/* type of file				*/
  unsigned short	vstamp;	/* version stamp			*/
  unsigned long		tsize;	/* text size in bytes, padded to FW bdry*/
  unsigned long		dsize;	/* initialized data "  "		*/
  unsigned long		bsize;	/* uninitialized data "   "		*/
  unsigned long		entry;	/* entry pt.				*/
  unsigned long	 	text_start;/* base of text used for this file */
  unsigned long	 	data_start;/* base of data used for this file */
};
#endif /* not MSDOS */
#else  /* not COFF */
#ifdef COFF_ENCAPSULATE
int need_coff_header = 1;
#include <coff-encap/a.out.encap.h> /* The location might be a poor assumption */
#else  /* not COFF_ENCAPSULATE */
#include <a.out.h>
#endif /* not COFF_ENCAPSULATE */
#endif /* not COFF */

/* Define getpagesize if the system does not.
   Note that this may depend on symbols defined in a.out.h.  */
#include "getpagesize.h"

#ifndef makedev			/* Try to detect types.h already loaded */
#include <sys/types.h>
#endif /* makedev */
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>

#include <sys/file.h>	/* Must be after sys/types.h for USG and BSD4_1*/

#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif
#ifndef O_RDWR
#define O_RDWR 2
#endif


extern char *start_of_text ();		/* Start of text */
extern char *start_of_data ();		/* Start of initialized data */

#ifdef COFF
static long block_copy_start;		/* Old executable start point */
static struct filehdr f_hdr;		/* File header */
static struct aouthdr f_ohdr;		/* Optional file header (a.out) */
long bias;			/* Bias to add for growth */
long lnnoptr;			/* Pointer to line-number info within file */
#define SYMS_START block_copy_start

static long text_scnptr;
static long data_scnptr;

static long coff_offset;

#else /* not COFF */

#ifdef HPUX
extern void *sbrk ();
#else
#if 0
/* Some systems with __STDC__ compilers still declare this `char *' in some
   header file, and our declaration conflicts.  The return value is always
   cast, so it should be harmless to leave it undefined.  Hopefully
   machines with different size pointers and ints declare sbrk in a header
   file.  */
#ifdef __STDC__
extern void *sbrk ();
#else
extern char *sbrk ();
#endif /* __STDC__ */
#endif
#endif /* HPUX */

#define SYMS_START ((long) N_SYMOFF (ohdr))

/* Some machines override the structure name for an a.out header.  */
#ifndef EXEC_HDR_TYPE
#define EXEC_HDR_TYPE struct exec
#endif

#ifdef HPUX
#ifdef HP9000S200_ID
#define MY_ID HP9000S200_ID
#else
#include <model.h>
#define MY_ID MYSYS
#endif /* no HP9000S200_ID */
static MAGIC OLDMAGIC = {MY_ID, SHARE_MAGIC};
static MAGIC NEWMAGIC = {MY_ID, DEMAND_MAGIC};
#define N_TXTOFF(x) TEXT_OFFSET(x)
#define N_SYMOFF(x) LESYM_OFFSET(x)
static EXEC_HDR_TYPE hdr, ohdr;

#else /* not HPUX */

#if defined (USG) && !defined (IBMAIX) && !defined (IRIS) && !defined (COFF_ENCAPSULATE) && !defined (GNU_LINUX)
static struct bhdr hdr, ohdr;
#define a_magic fmagic
#define a_text tsize
#define a_data dsize
#define a_bss bsize
#define a_syms ssize
#define a_trsize rtsize
#define a_drsize rdsize
#define a_entry entry
#define	N_BADMAG(x) \
    (((x).fmagic)!=OMAGIC && ((x).fmagic)!=NMAGIC &&\
     ((x).fmagic)!=FMAGIC && ((x).fmagic)!=IMAGIC)
#define NEWMAGIC FMAGIC
#else /* IRIS or IBMAIX or not USG */
static EXEC_HDR_TYPE hdr, ohdr;
#define NEWMAGIC ZMAGIC
#endif /* IRIS or IBMAIX not USG */
#endif /* not HPUX */

static int unexec_text_start;
static int unexec_data_start;

#ifdef COFF_ENCAPSULATE
/* coffheader is defined in the GNU a.out.encap.h file.  */
struct coffheader coffheader;
#endif

#endif /* not COFF */

static int pagemask;

/* Correct an int which is the bit pattern of a pointer to a byte
   into an int which is the number of a byte.
   This is a no-op on ordinary machines, but not on all.  */

#ifndef ADDR_CORRECT   /* Let m-*.h files override this definition */
#define ADDR_CORRECT(x) ((char *)(x) - (char*)0)
#endif

#ifdef emacs

#include "lisp.h"

static
report_error (file, fd)
     char *file;
     int fd;
{
  if (fd)
    close (fd);
  report_file_error ("Cannot unexec", Fcons (build_string (file), Qnil));
}
#endif /* emacs */

#define ERROR0(msg) report_error_1 (new, msg, 0, 0); return -1
#define ERROR1(msg,x) report_error_1 (new, msg, x, 0); return -1
#define ERROR2(msg,x,y) report_error_1 (new, msg, x, y); return -1

static
report_error_1 (fd, msg, a1, a2)
     int fd;
     char *msg;
     int a1, a2;
{
  close (fd);
#ifdef emacs
  error (msg, a1, a2);
#else
  fprintf (stderr, msg, a1, a2);
  fprintf (stderr, "\n");
#endif
}

static int make_hdr ();
static int copy_text_and_data ();
static int copy_sym ();
static void mark_x ();

/* ****************************************************************
 * make_hdr
 *
 * Make the header in the new a.out from the header in core.
 * Modify the text and data sizes.
 */
static int
make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name)
     int new, a_out;
     unsigned data_start, bss_start, entry_address;
     char *a_name;
     char *new_name;
{
  int tem;
#ifdef COFF
  auto struct scnhdr f_thdr;		/* Text section header */
  auto struct scnhdr f_dhdr;		/* Data section header */
  auto struct scnhdr f_bhdr;		/* Bss section header */
  auto struct scnhdr scntemp;		/* Temporary section header */
  register int scns;
#endif /* COFF */
#ifdef USG_SHARED_LIBRARIES
  extern unsigned int bss_end;
#else
  unsigned int bss_end;
#endif

  pagemask = getpagesize () - 1;

  /* Adjust text/data boundary. */
#ifdef NO_REMAP
  data_start = (int) start_of_data ();
#else /* not NO_REMAP */
  if (!data_start)
    data_start = (int) start_of_data ();
#endif /* not NO_REMAP */
  data_start = ADDR_CORRECT (data_start);

#ifdef SEGMENT_MASK
  data_start = data_start & ~SEGMENT_MASK; /* (Down) to segment boundary. */
#else
  data_start = data_start & ~pagemask; /* (Down) to page boundary. */
#endif

  bss_end = ADDR_CORRECT (sbrk (0)) + pagemask;
  bss_end &= ~ pagemask;

  /* Adjust data/bss boundary. */
  if (bss_start != 0)
    {
      bss_start = (ADDR_CORRECT (bss_start) + pagemask);
      /* (Up) to page bdry. */
      bss_start &= ~ pagemask;
      if (bss_start > bss_end)
	{
	  ERROR1 ("unexec: Specified bss_start (%u) is past end of program",
		  bss_start);
	}
    }
  else
    bss_start = bss_end;

  if (data_start > bss_start)	/* Can't have negative data size. */
    {
      ERROR2 ("unexec: data_start (%u) can't be greater than bss_start (%u)",
	      data_start, bss_start);
    }

#ifdef COFF
  coff_offset = 0L;		/* stays zero, except in DJGPP */

  /* Salvage as much info from the existing file as possible */
  if (a_out >= 0)
    {
#ifdef MSDOS
#if __DJGPP__ > 1
      /* Support the coff-go32-exe format with a prepended stub, since
	 this is what GCC 2.8.0 and later generates by default in DJGPP.  */
      unsigned short mz_header[3];

      if (read (a_out, &mz_header, sizeof (mz_header)) != sizeof (mz_header))
	{
	  PERROR (a_name);
	}
      if (mz_header[0] == 0x5a4d || mz_header[0] == 0x4d5a) /* "MZ" or "ZM" */
	{
	  coff_offset = (long)mz_header[2] * 512L;
	  if (mz_header[1])
	    coff_offset += (long)mz_header[1] - 512L;
	  lseek (a_out, coff_offset, 0);
	}
      else
	lseek (a_out, 0L, 0);
#endif /* __DJGPP__ > 1 */
#endif /* MSDOS */
      if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
	{
	  PERROR (a_name);
	}
      block_copy_start += sizeof (f_hdr);
      if (f_hdr.f_opthdr > 0)
	{
	  if (read (a_out, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
	    {
	      PERROR (a_name);
	    }
	  block_copy_start += sizeof (f_ohdr);
	}
      /* Loop through section headers, copying them in */
      lseek (a_out, coff_offset + sizeof (f_hdr) + f_hdr.f_opthdr, 0);
      for (scns = f_hdr.f_nscns; scns > 0; scns--) {
	if (read (a_out, &scntemp, sizeof (scntemp)) != sizeof (scntemp))
	  {
	    PERROR (a_name);
	  }
	if (scntemp.s_scnptr > 0L)
	  {
            if (block_copy_start < scntemp.s_scnptr + scntemp.s_size)
	      block_copy_start = scntemp.s_scnptr + scntemp.s_size;
	  }
	if (strcmp (scntemp.s_name, ".text") == 0)
	  {
	    f_thdr = scntemp;
	  }
	else if (strcmp (scntemp.s_name, ".data") == 0)
	  {
	    f_dhdr = scntemp;
	  }
	else if (strcmp (scntemp.s_name, ".bss") == 0)
	  {
	    f_bhdr = scntemp;
	  }
      }
    }
  else
    {
      ERROR0 ("can't build a COFF file from scratch yet");
    }

  /* Now we alter the contents of all the f_*hdr variables
     to correspond to what we want to dump.  */

#ifdef USG_SHARED_LIBRARIES

  /* The amount of data we're adding to the file is distance from the
   * end of the original .data space to the current end of the .data
   * space.
   */

  bias = bss_start - (f_ohdr.data_start + f_dhdr.s_size);

#endif

  f_hdr.f_flags |= (F_RELFLG | F_EXEC);
#ifdef TPIX
  f_hdr.f_nscns = 3;
#endif
#ifdef EXEC_MAGIC
  f_ohdr.magic = EXEC_MAGIC;
#endif
#ifndef NO_REMAP
  f_ohdr.text_start = (long) start_of_text ();
  f_ohdr.tsize = data_start - f_ohdr.text_start;
  f_ohdr.data_start = data_start;
#endif /* NO_REMAP */
  f_ohdr.dsize = bss_start - f_ohdr.data_start;
  f_ohdr.bsize = bss_end - bss_start;
#ifndef KEEP_OLD_TEXT_SCNPTR
  /* On some machines, the old values are right.
     ??? Maybe on all machines with NO_REMAP.  */
  f_thdr.s_size = f_ohdr.tsize;
  f_thdr.s_scnptr = sizeof (f_hdr) + sizeof (f_ohdr);
  f_thdr.s_scnptr += (f_hdr.f_nscns) * (sizeof (f_thdr));
#endif /* KEEP_OLD_TEXT_SCNPTR */
#ifdef ADJUST_TEXT_SCNHDR_SIZE
  /* On some machines, `text size' includes all headers.  */
  f_thdr.s_size -= f_thdr.s_scnptr;
#endif /* ADJUST_TEST_SCNHDR_SIZE */
  lnnoptr = f_thdr.s_lnnoptr;
#ifdef SECTION_ALIGNMENT
  /* Some systems require special alignment
     of the sections in the file itself.  */
  f_thdr.s_scnptr
    = (f_thdr.s_scnptr + SECTION_ALIGNMENT) & ~SECTION_ALIGNMENT;
#endif /* SECTION_ALIGNMENT */
#ifdef TPIX
  f_thdr.s_scnptr = 0xd0;
#endif
  text_scnptr = f_thdr.s_scnptr;
#ifdef ADJUST_TEXTBASE
  text_scnptr = sizeof (f_hdr) + sizeof (f_ohdr) + (f_hdr.f_nscns) * (sizeof (f_thdr));
#endif
#ifndef KEEP_OLD_PADDR
  f_dhdr.s_paddr = f_ohdr.data_start;
#endif /* KEEP_OLD_PADDR */
  f_dhdr.s_vaddr = f_ohdr.data_start;
  f_dhdr.s_size = f_ohdr.dsize;
  f_dhdr.s_scnptr = f_thdr.s_scnptr + f_thdr.s_size;
#ifdef SECTION_ALIGNMENT
  /* Some systems require special alignment
     of the sections in the file itself.  */
  f_dhdr.s_scnptr
    = (f_dhdr.s_scnptr + SECTION_ALIGNMENT) & ~SECTION_ALIGNMENT;
#endif /* SECTION_ALIGNMENT */
#ifdef DATA_SECTION_ALIGNMENT
  /* Some systems require special alignment
     of the data section only.  */
  f_dhdr.s_scnptr
    = (f_dhdr.s_scnptr + DATA_SECTION_ALIGNMENT) & ~DATA_SECTION_ALIGNMENT;
#endif /* DATA_SECTION_ALIGNMENT */
  data_scnptr = f_dhdr.s_scnptr;
#ifndef KEEP_OLD_PADDR
  f_bhdr.s_paddr = f_ohdr.data_start + f_ohdr.dsize;
#endif /* KEEP_OLD_PADDR */
  f_bhdr.s_vaddr = f_ohdr.data_start + f_ohdr.dsize;
  f_bhdr.s_size = f_ohdr.bsize;
  f_bhdr.s_scnptr = 0L;
#ifndef USG_SHARED_LIBRARIES
  bias = f_dhdr.s_scnptr + f_dhdr.s_size - block_copy_start;
#endif

  if (f_hdr.f_symptr > 0L)
    {
      f_hdr.f_symptr += bias;
    }

  if (f_thdr.s_lnnoptr > 0L)
    {
      f_thdr.s_lnnoptr += bias;
    }

#ifdef ADJUST_EXEC_HEADER
  ADJUST_EXEC_HEADER;
#endif /* ADJUST_EXEC_HEADER */

  if (write (new, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
    {
      PERROR (new_name);
    }

#ifndef USG_SHARED_LIBRARIES

  if (write (new, &f_thdr, sizeof (f_thdr)) != sizeof (f_thdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_dhdr, sizeof (f_dhdr)) != sizeof (f_dhdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_bhdr, sizeof (f_bhdr)) != sizeof (f_bhdr))
    {
      PERROR (new_name);
    }

#else /* USG_SHARED_LIBRARIES */

  /* The purpose of this code is to write out the new file's section
   * header table.
   *
   * Scan through the original file's sections.  If the encountered
   * section is one we know (.text, .data or .bss), write out the
   * correct header.  If it is a section we do not know (such as
   * .lib), adjust the address of where the section data is in the
   * file, and write out the header.
   *
   * If any section precedes .text or .data in the file, this code
   * will not adjust the file pointer for that section correctly.
   */

  /* This used to use sizeof (f_ohdr) instead of .f_opthdr.
     .f_opthdr is said to be right when there is no optional header.  */
  lseek (a_out, sizeof (f_hdr) + f_hdr.f_opthdr, 0);

  for (scns = f_hdr.f_nscns; scns > 0; scns--)
    {
      if (read (a_out, &scntemp, sizeof (scntemp)) != sizeof (scntemp))
	PERROR (a_name);

      if (!strcmp (scntemp.s_name, f_thdr.s_name))	/* .text */
	{
	  if (write (new, &f_thdr, sizeof (f_thdr)) != sizeof (f_thdr))
	    PERROR (new_name);
	}
      else if (!strcmp (scntemp.s_name, f_dhdr.s_name))	/* .data */
	{
	  if (write (new, &f_dhdr, sizeof (f_dhdr)) != sizeof (f_dhdr))
	    PERROR (new_name);
	}
      else if (!strcmp (scntemp.s_name, f_bhdr.s_name))	/* .bss */
	{
	  if (write (new, &f_bhdr, sizeof (f_bhdr)) != sizeof (f_bhdr))
	    PERROR (new_name);
	}
      else
	{
	  if (scntemp.s_scnptr)
	    scntemp.s_scnptr += bias;
	  if (write (new, &scntemp, sizeof (scntemp)) != sizeof (scntemp))
	    PERROR (new_name);
	}
    }
#endif /* USG_SHARED_LIBRARIES */

  return (0);

#else /* if not COFF */

  /* Get symbol table info from header of a.out file if given one. */
  if (a_out >= 0)
    {
#ifdef COFF_ENCAPSULATE
      if (read (a_out, &coffheader, sizeof coffheader) != sizeof coffheader)
	{
	  PERROR(a_name);
	}
      if (coffheader.f_magic != COFF_MAGIC)
	{
	  ERROR1("%s doesn't have valid coff magic number\n", a_name);
	}
#endif
      if (read (a_out, &ohdr, sizeof hdr) != sizeof hdr)
	{
	  PERROR (a_name);
	}

      if (N_BADMAG (ohdr))
	{
	  ERROR1 ("invalid magic number in %s", a_name);
	}
      hdr = ohdr;
    }
  else
    {
#ifdef COFF_ENCAPSULATE
      /* We probably could without too much trouble. The code is in gld
       * but I don't have that much time or incentive.
       */
      ERROR0 ("can't build a COFF file from scratch yet");
#else
#ifdef MSDOS	/* Demacs 1.1.1 91/10/16 HIRANO Satoshi */
      bzero ((void *)&hdr, sizeof hdr);
#else
      bzero (&hdr, sizeof hdr);
#endif
#endif
    }

  unexec_text_start = (long) start_of_text ();
  unexec_data_start = data_start;

  /* Machine-dependent fixup for header, or maybe for unexec_text_start */
#ifdef ADJUST_EXEC_HEADER
  ADJUST_EXEC_HEADER;
#endif /* ADJUST_EXEC_HEADER */

  hdr.a_trsize = 0;
  hdr.a_drsize = 0;
  if (entry_address != 0)
    hdr.a_entry = entry_address;

  hdr.a_bss = bss_end - bss_start;
  hdr.a_data = bss_start - data_start;
#ifdef NO_REMAP
  hdr.a_text = ohdr.a_text;
#else /* not NO_REMAP */
  hdr.a_text = data_start - unexec_text_start;

#ifdef A_TEXT_OFFSET
  hdr.a_text += A_TEXT_OFFSET (ohdr);
#endif

#endif /* not NO_REMAP */

#ifdef COFF_ENCAPSULATE
  /* We are encapsulating BSD format within COFF format.  */
  {
    struct coffscn *tp, *dp, *bp;
    tp = &coffheader.scns[0];
    dp = &coffheader.scns[1];
    bp = &coffheader.scns[2];
    tp->s_size = hdr.a_text + sizeof(struct exec);
    dp->s_paddr = data_start;
    dp->s_vaddr = data_start;
    dp->s_size = hdr.a_data;
    bp->s_paddr = dp->s_vaddr + dp->s_size;
    bp->s_vaddr = bp->s_paddr;
    bp->s_size = hdr.a_bss;
    coffheader.tsize = tp->s_size;
    coffheader.dsize = dp->s_size;
    coffheader.bsize = bp->s_size;
    coffheader.text_start = tp->s_vaddr;
    coffheader.data_start = dp->s_vaddr;
  }
  if (write (new, &coffheader, sizeof coffheader) != sizeof coffheader)
    {
      PERROR(new_name);
    }
#endif /* COFF_ENCAPSULATE */

  if (write (new, &hdr, sizeof hdr) != sizeof hdr)
    {
      PERROR (new_name);
    }

#if 0 /* This #ifndef caused a bug on GNU/Linux when using QMAGIC.  */
  /* This adjustment was done above only #ifndef NO_REMAP,
     so only undo it now #ifndef NO_REMAP.  */
  /* #ifndef NO_REMAP  */
#endif
#ifdef A_TEXT_OFFSET
  hdr.a_text -= A_TEXT_OFFSET (ohdr);
#endif

  return 0;

#endif /* not COFF */
}

write_segment (new, ptr, end)
     int new;
     register char *ptr, *end;
{
  register int i, nwrite, ret;
  char buf[80];
#ifndef USE_CRT_DLL
  extern int errno;
#endif
  /* This is the normal amount to write at once.
     It is the size of block that NFS uses.  */
  int writesize = 1 << 13;
  int pagesize = getpagesize ();
  char zeros[1 << 13];

  bzero (zeros, sizeof (zeros));

  for (i = 0; ptr < end;)
    {
      /* Distance to next multiple of writesize.  */
      nwrite = (((int) ptr + writesize) & -writesize) - (int) ptr;
      /* But not beyond specified end.  */
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      /* If write gets a page fault, it means we reached
	 a gap between the old text segment and the old data segment.
	 This gap has probably been remapped into part of the text segment.
	 So write zeros for it.  */
      if (ret == -1
#ifdef EFAULT
	  && errno == EFAULT
#endif
	  )
	{
	  /* Write only a page of zeros at once,
	     so that we we don't overshoot the start
	     of the valid memory in the old data segment.  */
	  if (nwrite > pagesize)
	    nwrite = pagesize;
	  write (new, zeros, nwrite);
	}
#if 0 /* Now that we have can ask `write' to write more than a page,
	 it is legit for write do less than the whole amount specified.  */
      else if (nwrite != ret)
	{
	  sprintf (buf,
		   "unexec write failure: addr 0x%x, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		   ptr, new, nwrite, ret, errno);
	  PERROR (buf);
	}
#endif
      i += nwrite;
      ptr += nwrite;
    }
}
/* ****************************************************************
 * copy_text_and_data
 *
 * Copy the text and data segments from memory to the new a.out
 */
static int
copy_text_and_data (new, a_out)
     int new, a_out;
{
  register char *end;
  register char *ptr;

#ifdef COFF

#ifdef USG_SHARED_LIBRARIES

  int scns;
  struct scnhdr scntemp;		/* Temporary section header */

  /* The purpose of this code is to write out the new file's section
   * contents.
   *
   * Step through the section table.  If we know the section (.text,
   * .data) do the appropriate thing.  Otherwise, if the section has
   * no allocated space in the file (.bss), do nothing.  Otherwise,
   * the section has space allocated in the file, and is not a section
   * we know.  So just copy it.
   */

  lseek (a_out, sizeof (struct filehdr) + sizeof (struct aouthdr), 0);

  for (scns = f_hdr.f_nscns; scns > 0; scns--)
    {
      if (read (a_out, &scntemp, sizeof (scntemp)) != sizeof (scntemp))
	PERROR ("temacs");

      if (!strcmp (scntemp.s_name, ".text"))
	{
	  lseek (new, (long) text_scnptr, 0);
	  ptr = (char *) f_ohdr.text_start;
	  end = ptr + f_ohdr.tsize;
	  write_segment (new, ptr, end);
	}
      else if (!strcmp (scntemp.s_name, ".data"))
	{
	  lseek (new, (long) data_scnptr, 0);
	  ptr = (char *) f_ohdr.data_start;
	  end = ptr + f_ohdr.dsize;
	  write_segment (new, ptr, end);
	}
      else if (!scntemp.s_scnptr)
	; /* do nothing - no data for this section */
      else
	{
	  char page[BUFSIZ];
	  int size, n;
	  long old_a_out_ptr = lseek (a_out, 0, 1);

	  lseek (a_out, scntemp.s_scnptr, 0);
	  for (size = scntemp.s_size; size > 0; size -= sizeof (page))
	    {
	      n = size > sizeof (page) ? sizeof (page) : size;
	      if (read (a_out, page, n) != n || write (new, page, n) != n)
		PERROR ("emacs");
	    }
	  lseek (a_out, old_a_out_ptr, 0);
	}
    }

#else /* COFF, but not USG_SHARED_LIBRARIES */

#ifdef MSDOS
#if __DJGPP__ >= 2
  /* Dump the original table of exception handlers, not the one
     where our exception hooks are registered.  */
  __djgpp_exception_toggle ();

  /* Switch off startup flags that might have been set at runtime
     and which might change the way that dumped Emacs works.  */
  save_djgpp_startup_flags = _crt0_startup_flags;
  _crt0_startup_flags &= ~(_CRT0_FLAG_NO_LFN | _CRT0_FLAG_NEARPTR);
#endif
#endif

  lseek (new, (long) text_scnptr, 0);
  ptr = (char *) f_ohdr.text_start;
#ifdef HEADER_INCL_IN_TEXT
  /* For Gould UTX/32, text starts after headers */
  ptr = (char *) (ptr + text_scnptr);
#endif /* HEADER_INCL_IN_TEXT */
  end = ptr + f_ohdr.tsize;
  write_segment (new, ptr, end);

  lseek (new, (long) data_scnptr, 0);
  ptr = (char *) f_ohdr.data_start;
  end = ptr + f_ohdr.dsize;
  write_segment (new, ptr, end);

#ifdef MSDOS
#if __DJGPP__ >= 2
  /* Restore our exception hooks.  */
  __djgpp_exception_toggle ();

  /* Restore the startup flags.  */
  _crt0_startup_flags = save_djgpp_startup_flags;
#endif
#endif

#endif /* USG_SHARED_LIBRARIES */

#else /* if not COFF */

/* Some machines count the header as part of the text segment.
   That is to say, the header appears in core
   just before the address that start_of_text returns.
   For them, N_TXTOFF is the place where the header goes.
   We must adjust the seek to the place after the header.
   Note that at this point hdr.a_text does *not* count
   the extra A_TEXT_OFFSET bytes, only the actual bytes of code.  */

#ifdef A_TEXT_SEEK
  lseek (new, (long) A_TEXT_SEEK (hdr), 0);
#else
  lseek (new, (long) N_TXTOFF (hdr), 0);
#endif /* no A_TEXT_SEEK */

#ifdef RISCiX

  /* Acorn's RISC-iX has a wacky way of initialising the position of the heap.
   * There is a little table in crt0.o that is filled at link time with
   * the min and current brk positions, among other things.  When start
   * runs, it copies the table to where these parameters live during
   * execution.  This data is in text space, so it cannot be modified here
   * before saving the executable, so the data is written manually.  In
   * addition, the table does not have a label, and the nearest accessible
   * label (mcount) is not prefixed with a '_', thus making it inaccessible
   * from within C programs.  To overcome this, emacs's executable is passed
   * through the command 'nm %s | fgrep mcount' into a pipe, and the
   * resultant output is then used to find the address of 'mcount'.  As far as
   * is possible to determine, in RISC-iX releases prior to 1.2, the negative
   * offset of the table from mcount is 0x2c, whereas from 1.2 onwards it is
   * 0x30.  bss_end has been rounded up to page boundary.  This solution is
   * based on suggestions made by Kevin Welton and Steve Hunt of Acorn, and
   * avoids the need for a custom version of crt0.o for emacs which has its
   * table in data space.
   */

  {
    char command[1024];
    char errbuf[1024];
    char address_text[32];
    int  proforma[4];
    FILE *pfile;
    char *temp_ptr;
    char c;
    int mcount_address, mcount_offset, count;
    extern char *_execname;


    /* The use of _execname is incompatible with RISCiX 1.1 */
    sprintf (command, "nm %s | fgrep mcount", _execname);

    if ( (pfile = popen(command, "r")) == NULL)
    {
      sprintf (errbuf, "Could not open pipe");
      PERROR (errbuf);
    }

    count=0;
    while ( ((c=getc(pfile)) != EOF) && (c != ' ') && (count < 31))
      address_text[count++]=c;
    address_text[count]=0;

    if ((count == 0) || pclose(pfile) != NULL)
    {
      sprintf (errbuf, "Failed to execute the command '%s'\n", command);
      PERROR (errbuf);
    }

    sscanf(address_text, "%x", &mcount_address);
    ptr = (char *) unexec_text_start;
    mcount_offset = (char *)mcount_address - ptr;

#ifdef RISCiX_1_1
#define EDATA_OFFSET 0x2c
#else
#define EDATA_OFFSET 0x30
#endif

    end = ptr + mcount_offset - EDATA_OFFSET;

    write_segment (new, ptr, end);

    proforma[0] = bss_end;	/* becomes _edata */
    proforma[1] = bss_end;	/* becomes _end */
    proforma[2] = bss_end;	/* becomes _minbrk */
    proforma[3] = bss_end;	/* becomes _curbrk */

    write (new, proforma, 16);

    temp_ptr = ptr;
    ptr = end + 16;
    end = temp_ptr + hdr.a_text;

    write_segment (new, ptr, end);
  }

#else /* !RISCiX */
  ptr = (char *) unexec_text_start;
  end = ptr + hdr.a_text;
  write_segment (new, ptr, end);
#endif /* RISCiX */

  ptr = (char *) unexec_data_start;
  end = ptr + hdr.a_data;
/*  This lseek is certainly incorrect when A_TEXT_OFFSET
    and I believe it is a no-op otherwise.
    Let's see if its absence ever fails.  */
/*  lseek (new, (long) N_TXTOFF (hdr) + hdr.a_text, 0); */
  write_segment (new, ptr, end);

#endif /* not COFF */

  return 0;
}

/* ****************************************************************
 * copy_sym
 *
 * Copy the relocation information and symbol table from the a.out to the new
 */
static int
copy_sym (new, a_out, a_name, new_name)
     int new, a_out;
     char *a_name, *new_name;
{
  char page[1024];
  int n;

  if (a_out < 0)
    return 0;

#ifdef COFF
  if (SYMS_START == 0L)
    return 0;
#endif  /* COFF */

#ifdef COFF
  if (lnnoptr)			/* if there is line number info */
    lseek (a_out, coff_offset + lnnoptr, 0);	/* start copying from there */
  else
    lseek (a_out, coff_offset + SYMS_START, 0);	/* Position a.out to symtab. */
#else  /* not COFF */
  lseek (a_out, SYMS_START, 0);	/* Position a.out to symtab. */
#endif /* not COFF */

  while ((n = read (a_out, page, sizeof page)) > 0)
    {
      if (write (new, page, n) != n)
	{
	  PERROR (new_name);
	}
    }
  if (n < 0)
    {
      PERROR (a_name);
    }
  return 0;
}

/* ****************************************************************
 * mark_x
 *
 * After successfully building the new a.out, mark it executable
 */
static void
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um;
  int new = 0;  /* for PERROR */

  um = umask (777);
  umask (um);
  if (stat (name, &sbuf) == -1)
    {
      PERROR (name);
    }
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) == -1)
    PERROR (name);
}

#ifdef COFF
#ifndef COFF_BSD_SYMBOLS

/*
 *	If the COFF file contains a symbol table and a line number section,
 *	then any auxiliary entries that have values for x_lnnoptr must
 *	be adjusted by the amount that the line number section has moved
 *	in the file (bias computed in make_hdr).  The #@$%&* designers of
 *	the auxiliary entry structures used the absolute file offsets for
 *	the line number entry rather than an offset from the start of the
 *	line number section!
 *
 *	When I figure out how to scan through the symbol table and pick out
 *	the auxiliary entries that need adjustment, this routine will
 *	be fixed.  As it is now, all such entries are wrong and sdb
 *	will complain.   Fred Fish, UniSoft Systems Inc.
 */

/* This function is probably very slow.  Instead of reopening the new
   file for input and output it should copy from the old to the new
   using the two descriptors already open (WRITEDESC and READDESC).
   Instead of reading one small structure at a time it should use
   a reasonable size buffer.  But I don't have time to work on such
   things, so I am installing it as submitted to me.  -- RMS.  */

adjust_lnnoptrs (writedesc, readdesc, new_name)
     int writedesc;
     int readdesc;
     char *new_name;
{
  register int nsyms;
  register int new;
#if defined (amdahl_uts) || defined (pfa)
  SYMENT symentry;
  AUXENT auxentry;
#else
  struct syment symentry;
  union auxent auxentry;
#endif

  if (!lnnoptr || !f_hdr.f_symptr)
    return 0;

#ifdef MSDOS
  if ((new = writedesc) < 0)
#else
  if ((new = open (new_name, O_RDWR)) < 0)
#endif
    {
      PERROR (new_name);
      return -1;
    }

  lseek (new, f_hdr.f_symptr, 0);
  for (nsyms = 0; nsyms < f_hdr.f_nsyms; nsyms++)
    {
      read (new, &symentry, SYMESZ);
      if (symentry.n_numaux)
	{
	  read (new, &auxentry, AUXESZ);
	  nsyms++;
	  if (ISFCN (symentry.n_type) || symentry.n_type == 0x2400)
	    {
	      auxentry.x_sym.x_fcnary.x_fcn.x_lnnoptr += bias;
	      lseek (new, -AUXESZ, 1);
	      write (new, &auxentry, AUXESZ);
	    }
	}
    }
#ifndef MSDOS
  close (new);
#endif
  return 0;
}

#endif /* COFF_BSD_SYMBOLS */

#endif /* COFF */

/* ****************************************************************
 * unexec
 *
 * driving logic.
 */
unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int new, a_out = -1;

  if (a_name && (a_out = open (a_name, O_RDONLY)) < 0)
    {
      PERROR (a_name);
    }
  if ((new = creat (new_name, 0666)) < 0)
    {
      PERROR (new_name);
    }

  if (make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name) < 0
      || copy_text_and_data (new, a_out) < 0
      || copy_sym (new, a_out, a_name, new_name) < 0
#ifdef COFF
#ifndef COFF_BSD_SYMBOLS
      || adjust_lnnoptrs (new, a_out, new_name) < 0
#endif
#endif
      )
    {
      close (new);
      /* unlink (new_name);	    	/* Failed, unlink new a.out */
      return -1;
    }

  close (new);
  if (a_out >= 0)
    close (a_out);
  mark_x (new_name);
  return 0;
}

#endif /* not CANNOT_DUMP */

/* arch-tag: 62409b69-e27a-4a7c-9413-0210d6b54e7f
   (do not change this comment) */
