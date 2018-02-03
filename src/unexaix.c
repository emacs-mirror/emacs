/* Dump an executable image.
   Copyright (C) 1985-1988, 1999, 2001-2018 Free Software Foundation,
   Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/*
In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/* Originally based on the COFF unexec.c by Spencer W. Thomas.
 *
 * Subsequently hacked on by
 * Bill Mann <Bill_Man@praxisint.com>
 * Andrew Vignaux <Andrew.Vignaux@comp.vuw.ac.nz>
 * Mike Sperber <sperber@informatik.uni-tuebingen.de>
 *
 * Synopsis:
 *	unexec (const char *new_name, const *old_name);
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If a_name is non-NULL, the symbol table will be taken from the given file.
 * On some machines, an existing a_name file is required.
 *
 */

#include <config.h>
#include "unexec.h"
#include "lisp.h"

#define PERROR(file) report_error (file, new)
#include <a.out.h>
/* Define getpagesize () if the system does not.
   Note that this may depend on symbols defined in a.out.h
 */
#include "getpagesize.h"

#include <sys/types.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>

extern char _data[];
extern char _text[];

#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>

static struct filehdr f_hdr;		/* File header */
static struct aouthdr f_ohdr;		/* Optional file header (a.out) */
static off_t bias;			/* Bias to add for growth */
static off_t lnnoptr;			/* Pointer to line-number info within file */

static off_t text_scnptr;
static off_t data_scnptr;
#define ALIGN(val, pwr) (((val) + ((1L<<(pwr))-1)) & ~((1L<<(pwr))-1))
static off_t load_scnptr;
static off_t orig_load_scnptr;
static off_t orig_data_scnptr;
static int unrelocate_symbols (int, int, const char *, const char *);

#ifndef MAX_SECTIONS
#define MAX_SECTIONS	10
#endif

static int adjust_lnnoptrs (int, int, const char *);

static int pagemask;

#include "lisp.h"

static _Noreturn void
report_error (const char *file, int fd)
{
  int err = errno;
  if (fd)
    emacs_close (fd);
  report_file_errno ("Cannot unexec", build_string (file), err);
}

#define ERROR0(msg) report_error_1 (new, msg)
#define ERROR1(msg,x) report_error_1 (new, msg, x)
#define ERROR2(msg,x,y) report_error_1 (new, msg, x, y)

static _Noreturn void ATTRIBUTE_FORMAT_PRINTF (2, 3)
report_error_1 (int fd, const char *msg, ...)
{
  va_list ap;
  emacs_close (fd);
  va_start (ap, msg);
  verror (msg, ap);
  va_end (ap);
}

static int make_hdr (int, int, const char *, const char *);
static void mark_x (const char *);
static int copy_text_and_data (int);
static int copy_sym (int, int, const char *, const char *);
static void write_segment (int, char *, char *);

/* ****************************************************************
 * unexec
 *
 * driving logic.
 */
void
unexec (const char *new_name, const char *a_name)
{
  int new = -1, a_out = -1;

  if (a_name && (a_out = emacs_open (a_name, O_RDONLY, 0)) < 0)
    {
      PERROR (a_name);
    }
  if ((new = emacs_open (new_name, O_WRONLY | O_CREAT | O_TRUNC, 0777)) < 0)
    {
      PERROR (new_name);
    }
  if (make_hdr (new, a_out,
		a_name, new_name) < 0
      || copy_text_and_data (new) < 0
      || copy_sym (new, a_out, a_name, new_name) < 0
      || adjust_lnnoptrs (new, a_out, new_name) < 0
      || unrelocate_symbols (new, a_out, a_name, new_name) < 0)
    {
      emacs_close (new);
      return;
    }

  emacs_close (new);
  if (a_out >= 0)
    emacs_close (a_out);
}

/* ****************************************************************
 * make_hdr
 *
 * Make the header in the new a.out from the header in core.
 * Modify the text and data sizes.
 */
static int
make_hdr (int new, int a_out,
	  const char *a_name, const char *new_name)
{
  int scns;
  uintptr_t bss_start;
  uintptr_t data_start;

  struct scnhdr section[MAX_SECTIONS];
  struct scnhdr * f_thdr;		/* Text section header */
  struct scnhdr * f_dhdr;		/* Data section header */
  struct scnhdr * f_bhdr;		/* Bss section header */
  struct scnhdr * f_lhdr;		/* Loader section header */
  struct scnhdr * f_tchdr;		/* Typechk section header */
  struct scnhdr * f_dbhdr;		/* Debug section header */
  struct scnhdr * f_xhdr;		/* Except section header */

  load_scnptr = orig_load_scnptr = lnnoptr = 0;
  pagemask = getpagesize () - 1;

  /* Adjust text/data boundary. */
  data_start = (uintptr_t) _data;

  data_start = data_start & ~pagemask; /* (Down) to page boundary. */

  bss_start = (uintptr_t) sbrk (0) + pagemask;
  bss_start &= ~ pagemask;

  if (data_start > bss_start)	/* Can't have negative data size. */
    {
      ERROR2 (("unexec: data_start (0x%"PRIxPTR
	       ") can't be greater than bss_start (0x%"PRIxPTR")"),
	      data_start, bss_start);
    }

  /* Salvage as much info from the existing file as possible */
  f_thdr = NULL; f_dhdr = NULL; f_bhdr = NULL;
  f_lhdr = NULL; f_tchdr = NULL; f_dbhdr = NULL; f_xhdr = NULL;
  if (a_out >= 0)
    {
      if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
	{
	  PERROR (a_name);
	}
      if (f_hdr.f_opthdr > 0)
	{
	  if (read (a_out, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
	    {
	      PERROR (a_name);
	    }
	}
      if (f_hdr.f_nscns > MAX_SECTIONS)
	{
	  ERROR0 ("unexec: too many section headers -- increase MAX_SECTIONS");
	}
      /* Loop through section headers */
      for (scns = 0; scns < f_hdr.f_nscns; scns++) {
	struct scnhdr *s = &section[scns];
	if (read (a_out, s, sizeof (*s)) != sizeof (*s))
	  {
	    PERROR (a_name);
	  }

#define CHECK_SCNHDR(ptr, name, flags) \
  if (strcmp (s->s_name, name) == 0) { \
    if (s->s_flags != flags) { \
      fprintf (stderr, "unexec: %lx flags where %x expected in %s section.\n", \
               (unsigned long)s->s_flags, flags, name);                 \
    } \
    if (ptr) { \
      fprintf (stderr, "unexec: duplicate section header for section %s.\n", \
               name);                                                   \
    } \
    ptr = s; \
  }
	CHECK_SCNHDR (f_thdr, _TEXT, STYP_TEXT);
	CHECK_SCNHDR (f_dhdr, _DATA, STYP_DATA);
	CHECK_SCNHDR (f_bhdr, _BSS, STYP_BSS);
	CHECK_SCNHDR (f_lhdr, _LOADER, STYP_LOADER);
	CHECK_SCNHDR (f_dbhdr, _DEBUG,  STYP_DEBUG);
	CHECK_SCNHDR (f_tchdr, _TYPCHK,  STYP_TYPCHK);
	CHECK_SCNHDR (f_xhdr, _EXCEPT,  STYP_EXCEPT);
      }

      if (f_thdr == 0)
	{
	  ERROR1 ("unexec: couldn't find \"%s\" section", _TEXT);
	}
      if (f_dhdr == 0)
	{
	  ERROR1 ("unexec: couldn't find \"%s\" section", _DATA);
	}
      if (f_bhdr == 0)
	{
	  ERROR1 ("unexec: couldn't find \"%s\" section", _BSS);
	}
    }
  else
    {
      ERROR0 ("can't build a COFF file from scratch yet");
    }
  orig_data_scnptr = f_dhdr->s_scnptr;
  orig_load_scnptr = f_lhdr ? f_lhdr->s_scnptr : 0;

  /* Now we alter the contents of all the f_*hdr variables
     to correspond to what we want to dump.  */

  /* Indicate that the reloc information is no longer valid for ld (bind);
     we only update it enough to fake out the exec-time loader.  */
  f_hdr.f_flags |= (F_RELFLG | F_EXEC);

  f_ohdr.dsize = bss_start - f_ohdr.data_start;
  f_ohdr.bsize = 0;

  f_dhdr->s_size = f_ohdr.dsize;
  f_bhdr->s_size = f_ohdr.bsize;
  f_bhdr->s_paddr = f_ohdr.data_start + f_ohdr.dsize;
  f_bhdr->s_vaddr = f_ohdr.data_start + f_ohdr.dsize;

  /* fix scnptr's */
  {
    off_t ptr = section[0].s_scnptr;

    bias = -1;
    for (scns = 0; scns < f_hdr.f_nscns; scns++)
      {
	struct scnhdr *s = &section[scns];

	if (s->s_flags & STYP_PAD)        /* .pad sections omitted in AIX 4.1 */
	  {
	    /*
	     * the text_start should probably be o_algntext but that doesn't
	     * seem to change
	     */
	    if (f_ohdr.text_start != 0) /* && scns != 0 */
	      {
		s->s_size = 512 - (ptr % 512);
		if (s->s_size == 512)
		  s->s_size = 0;
	      }
	    s->s_scnptr = ptr;
	  }
	else if (s->s_flags & STYP_DATA)
	  s->s_scnptr = ptr;
	else if (!(s->s_flags & (STYP_TEXT | STYP_BSS)))
	  {
	    if (bias == -1)                /* if first section after bss */
	      bias = ptr - s->s_scnptr;

	    s->s_scnptr += bias;
	    ptr = s->s_scnptr;
	  }

	ptr = ptr + s->s_size;
      }
  }

  /* fix other pointers */
  for (scns = 0; scns < f_hdr.f_nscns; scns++)
    {
      struct scnhdr *s = &section[scns];

      if (s->s_relptr != 0)
	{
	  s->s_relptr += bias;
	}
      if (s->s_lnnoptr != 0)
	{
	  if (lnnoptr == 0) lnnoptr = s->s_lnnoptr;
	  s->s_lnnoptr += bias;
	}
    }

  if (f_hdr.f_symptr > 0L)
    {
      f_hdr.f_symptr += bias;
    }

  text_scnptr = f_thdr->s_scnptr;
  data_scnptr = f_dhdr->s_scnptr;
  load_scnptr = f_lhdr ? f_lhdr->s_scnptr : 0;

  if (write (new, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (new_name);
    }

  if (f_hdr.f_opthdr > 0)
    {
      if (write (new, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
	{
	  PERROR (new_name);
	}
    }

  for (scns = 0; scns < f_hdr.f_nscns; scns++) {
    struct scnhdr *s = &section[scns];
    if (write (new, s, sizeof (*s)) != sizeof (*s))
      {
	PERROR (new_name);
      }
  }

  return (0);
}

/* ****************************************************************

 *
 * Copy the text and data segments from memory to the new a.out
 */
static int
copy_text_and_data (int new)
{
  char *end;
  char *ptr;

  lseek (new, text_scnptr, SEEK_SET);
  ptr = _text;
  end = ptr + f_ohdr.tsize;
  write_segment (new, ptr, end);

  lseek (new, data_scnptr, SEEK_SET);
  ptr = (char *) (ptrdiff_t) f_ohdr.data_start;
  end = ptr + f_ohdr.dsize;
  write_segment (new, ptr, end);

  return 0;
}

#define UnexBlockSz (1<<12)			/* read/write block size */
static void
write_segment (int new, char *ptr, char *end)
{
  int i, nwrite, ret;
  char zeros[UnexBlockSz];

  for (i = 0; ptr < end;)
    {
      /* distance to next block.  */
      nwrite = (((ptrdiff_t) ptr + UnexBlockSz) & -UnexBlockSz) - (ptrdiff_t) ptr;
      /* But not beyond specified end.  */
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      /* If write gets a page fault, it means we reached
	 a gap between the old text segment and the old data segment.
	 This gap has probably been remapped into part of the text segment.
	 So write zeros for it.  */
      if (ret == -1 && errno == EFAULT)
	{
	  memset (zeros, 0, nwrite);
	  write (new, zeros, nwrite);
	}
      else if (nwrite != ret)
	{
	  int write_errno = errno;
	  char buf[1000];
	  void *addr = ptr;
	  sprintf (buf,
		   "unexec write failure: addr %p, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		   addr, new, nwrite, ret, errno);
	  errno = write_errno;
	  PERROR (buf);
	}
      i += nwrite;
      ptr += nwrite;
    }
}

/* ****************************************************************
 * copy_sym
 *
 * Copy the relocation information and symbol table from the a.out to the new
 */
static int
copy_sym (int new, int a_out, const char *a_name, const char *new_name)
{
  char page[UnexBlockSz];
  int n;

  if (a_out < 0)
    return 0;

  if (orig_load_scnptr == 0L)
    return 0;

  if (lnnoptr && lnnoptr < orig_load_scnptr) /* if there is line number info  */
    lseek (a_out, lnnoptr, SEEK_SET);  /* start copying from there */
  else
    lseek (a_out, orig_load_scnptr, SEEK_SET); /* Position a.out to symtab. */

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

static int
adjust_lnnoptrs (int writedesc, int readdesc, const char *new_name)
{
  int nsyms;
  int naux;
  int new;
  struct syment symentry;
  union auxent auxentry;

  if (!lnnoptr || !f_hdr.f_symptr)
    return 0;

  if ((new = emacs_open (new_name, O_RDWR, 0)) < 0)
    {
      PERROR (new_name);
      return -1;
    }

  lseek (new, f_hdr.f_symptr, SEEK_SET);
  for (nsyms = 0; nsyms < f_hdr.f_nsyms; nsyms++)
    {
      read (new, &symentry, SYMESZ);
      if (symentry.n_sclass == C_BINCL || symentry.n_sclass == C_EINCL)
	{
	  symentry.n_value += bias;
	  lseek (new, -SYMESZ, SEEK_CUR);
	  write (new, &symentry, SYMESZ);
	}

      for (naux = symentry.n_numaux; naux-- != 0; )
	{
	  read (new, &auxentry, AUXESZ);
	  nsyms++;
	  if (naux != 0              /* skip csect auxentry (last entry) */
              && (symentry.n_sclass == C_EXT || symentry.n_sclass == C_HIDEXT))
            {
              auxentry.x_sym.x_fcnary.x_fcn.x_lnnoptr += bias;
              lseek (new, -AUXESZ, SEEK_CUR);
              write (new, &auxentry, AUXESZ);
            }
	}
    }
  emacs_close (new);

  return 0;
}

static int
unrelocate_symbols (int new, int a_out,
		    const char *a_name, const char *new_name)
{
  int i;
  LDHDR ldhdr;
  LDREL ldrel;
  off_t t_reloc = (intptr_t) _text - f_ohdr.text_start;
#ifndef ALIGN_DATA_RELOC
  off_t d_reloc = (intptr_t) _data - f_ohdr.data_start;
#else
  /* This worked (and was needed) before AIX 4.2.
     I have no idea why. -- Mike */
  off_t d_reloc = (intptr_t) _data - ALIGN (f_ohdr.data_start, 2);
#endif
  int * p;

  if (load_scnptr == 0)
    return 0;

  lseek (a_out, orig_load_scnptr, SEEK_SET);
  if (read (a_out, &ldhdr, sizeof (ldhdr)) != sizeof (ldhdr))
    {
      PERROR (new_name);
    }

#define SYMNDX_TEXT	0
#define SYMNDX_DATA	1
#define SYMNDX_BSS	2

  for (i = 0; i < ldhdr.l_nreloc; i++)
    {
      lseek (a_out,
	     orig_load_scnptr + LDHDRSZ + LDSYMSZ*ldhdr.l_nsyms + LDRELSZ*i,
	     SEEK_SET);

      if (read (a_out, &ldrel, LDRELSZ) != LDRELSZ)
	{
	  PERROR (a_name);
	}

      /* move the BSS loader symbols to the DATA segment */
      if (ldrel.l_symndx == SYMNDX_BSS)
	{
	  ldrel.l_symndx = SYMNDX_DATA;

	  lseek (new,
		 load_scnptr + LDHDRSZ + LDSYMSZ*ldhdr.l_nsyms + LDRELSZ*i,
		 SEEK_SET);

	  if (write (new, &ldrel, LDRELSZ) != LDRELSZ)
	    {
	      PERROR (new_name);
	    }
	}

      if (ldrel.l_rsecnm == f_ohdr.o_sndata)
	{
	  int orig_int;

	  lseek (a_out,
                 orig_data_scnptr + (ldrel.l_vaddr - f_ohdr.data_start),
		 SEEK_SET);

	  if (read (a_out, (void *) &orig_int, sizeof (orig_int))
	      != sizeof (orig_int))
	    {
	      PERROR (a_name);
	    }

          p = (int *) (intptr_t) (ldrel.l_vaddr + d_reloc);

	  switch (ldrel.l_symndx) {
	  case SYMNDX_TEXT:
	    orig_int = * p - t_reloc;
	    break;

	  case SYMNDX_DATA:
	  case SYMNDX_BSS:
	    orig_int = * p - d_reloc;
	    break;
	  }

          if (orig_int != * p)
            {
              lseek (new,
                     data_scnptr + (ldrel.l_vaddr - f_ohdr.data_start),
		     SEEK_SET);
              if (write (new, (void *) &orig_int, sizeof (orig_int))
                  != sizeof (orig_int))
                {
                  PERROR (new_name);
                }
            }
	}
    }
  return 0;
}
