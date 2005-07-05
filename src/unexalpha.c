/* Unexec for DEC alpha.  schoepf@sc.ZIB-Berlin.DE (Rainer Schoepf).

   Copyright (C) 1994, 2000 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include <config.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#if !defined (__NetBSD__) && !defined (__OpenBSD__)
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#ifndef __linux__
# include <reloc.h>
# include <elf_abi.h>
#endif
#else /* __NetBSD__ or __OpenBSD__ */
/*
 * NetBSD/Alpha does not have 'normal' user-land ECOFF support because
 * there's no desire to support ECOFF as the executable format in the
 * long term.
 */
#include <sys/exec_ecoff.h>

/* Structures, constants, etc., that NetBSD defines strangely. */
#define	filehdr		ecoff_filehdr
#define	aouthdr		ecoff_aouthdr
#define	scnhdr		ecoff_scnhdr
#define	HDRR		struct ecoff_symhdr
#define	pHDRR		HDRR *
#define	cbHDRR		sizeof(HDRR)
#ifdef __OpenBSD__
#define	ALPHAMAGIC	ECOFF_MAGIC_NATIVE_ALPHA
#else
#define	ALPHAMAGIC	ECOFF_MAGIC_NETBSD_ALPHA
#endif
#define	ZMAGIC		ECOFF_ZMAGIC

/* Misc. constants that NetBSD doesn't define at all. */
#define	ALPHAUMAGIC	0617
#define	_MIPS_NSCNS_MAX	35
#define	STYP_TEXT	0x00000020
#define	STYP_DATA	0x00000040
#define	STYP_BSS	0x00000080
#define	STYP_RDATA	0x00000100
#define	STYP_SDATA	0x00000200
#define	STYP_SBSS	0x00000400
#define	STYP_INIT	0x80000000
#define	_TEXT		".text"
#define	_DATA		".data"
#define	_BSS		".bss"
#define	_INIT		".init"
#define	_RDATA		".rdata"
#define	_SDATA		".sdata"
#define	_SBSS		".sbss"
#endif /* __NetBSD__ || __OpenBSD__ */

static void fatal_unexec __P ((char *, char *));
static void mark_x __P ((char *));

static void update_dynamic_symbols __P ((char *, char *, int, struct aouthdr));

#define READ(_fd, _buffer, _size, _error_message, _error_arg) \
	errno = EEOF; \
	if (read (_fd, _buffer, _size) != _size) \
	  fatal_unexec (_error_message, _error_arg);

#define WRITE(_fd, _buffer, _size, _error_message, _error_arg) \
	if (write (_fd, _buffer, _size) != _size) \
	  fatal_unexec (_error_message, _error_arg);

#define SEEK(_fd, _position, _error_message, _error_arg) \
	errno = EEOF; \
	if (lseek (_fd, _position, L_SET) != _position) \
	  fatal_unexec (_error_message, _error_arg);

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
void *sbrk ();
#endif

#define EEOF -1

static struct scnhdr *text_section;
static struct scnhdr *rel_dyn_section;
static struct scnhdr *dynstr_section;
static struct scnhdr *dynsym_section;
static struct scnhdr *init_section;
static struct scnhdr *finit_section;
static struct scnhdr *rdata_section;
static struct scnhdr *rconst_section;
static struct scnhdr *data_section;
static struct scnhdr *pdata_section;
static struct scnhdr *xdata_section;
static struct scnhdr *got_section;
static struct scnhdr *lit8_section;
static struct scnhdr *lit4_section;
static struct scnhdr *sdata_section;
static struct scnhdr *sbss_section;
static struct scnhdr *bss_section;

static struct scnhdr old_data_scnhdr;

static unsigned long Brk;

struct headers {
    struct filehdr fhdr;
    struct aouthdr aout;
    struct scnhdr section[_MIPS_NSCNS_MAX];
};



/* Define name of label for entry point for the dumped executable.  */

#ifndef DEFAULT_ENTRY_ADDRESS
#define DEFAULT_ENTRY_ADDRESS __start
#endif

void
unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned long data_start, bss_start, entry_address;
{
  int new, old;
  char * oldptr;
  struct headers ohdr, nhdr;
  struct stat stat;
  long pagesize, brk;
  long newsyms, symrel;
  int nread;
  int i;
  long vaddr, scnptr;
#define BUFSIZE 8192
  char buffer[BUFSIZE];

  if ((old = open (a_name, O_RDONLY)) < 0)
    fatal_unexec ("opening %s", a_name);

  new = creat (new_name, 0666);
  if (new < 0) fatal_unexec ("creating %s", new_name);

  if ((fstat (old, &stat) == -1))
    fatal_unexec ("fstat %s", a_name);

  oldptr = (char *)mmap (0, stat.st_size, PROT_READ, MAP_FILE|MAP_SHARED, old, 0);

  if (oldptr == (char *)-1)
    fatal_unexec ("mmap %s", a_name);

  close (old);

  /* This is a copy of the a.out header of the original executable */

  ohdr = (*(struct headers *)oldptr);

  /* This is where we build the new header from the in-memory copy */

  nhdr = *((struct headers *)TEXT_START);

  /* First do some consistency checks */

  if (nhdr.fhdr.f_magic != ALPHAMAGIC
      && nhdr.fhdr.f_magic != ALPHAUMAGIC)
    {
      fprintf (stderr, "unexec: input file magic number is %x, not %x or %x.\n",
	       nhdr.fhdr.f_magic, ALPHAMAGIC, ALPHAUMAGIC);
      exit (1);
    }

  if (nhdr.fhdr.f_opthdr != sizeof (nhdr.aout))
    {
      fprintf (stderr, "unexec: input a.out header is %d bytes, not %d.\n",
	       nhdr.fhdr.f_opthdr, (int)sizeof (nhdr.aout));
      exit (1);
    }
  if (nhdr.aout.magic != ZMAGIC)
    {
      fprintf (stderr, "unexec: input file a.out magic number is %o, not %o.\n",
	       nhdr.aout.magic, ZMAGIC);
      exit (1);
    }


  /* Now check the existence of certain header section and grab
     their addresses. */

#define CHECK_SCNHDR(ptr, name, flags)					\
  ptr = NULL;								\
  for (i = 0; i < nhdr.fhdr.f_nscns && !ptr; i++)			\
    if (strncmp (nhdr.section[i].s_name, name, 8) == 0)			\
      {									\
	if (nhdr.section[i].s_flags != flags)				\
	  fprintf (stderr, "unexec: %x flags (%x expected) in %s section.\n", \
		   nhdr.section[i].s_flags, flags, name);		\
	ptr = nhdr.section + i;						\
      }									\

  CHECK_SCNHDR (text_section,  _TEXT,  STYP_TEXT);
  CHECK_SCNHDR (init_section,  _INIT,  STYP_INIT);
#ifdef _REL_DYN
  CHECK_SCNHDR (rel_dyn_section, _REL_DYN,  STYP_REL_DYN);
#endif /* _REL_DYN */
#ifdef _DYNSYM
  CHECK_SCNHDR (dynsym_section, _DYNSYM,  STYP_DYNSYM);
#endif /* _REL_DYN */
#ifdef _DYNSTR
  CHECK_SCNHDR (dynstr_section, _DYNSTR,  STYP_DYNSTR);
#endif /* _REL_DYN */
#ifdef _FINI
  CHECK_SCNHDR (finit_section, _FINI,  STYP_FINI);
#endif /* _FINI */
  CHECK_SCNHDR (rdata_section, _RDATA, STYP_RDATA);
#ifdef _RCONST
  CHECK_SCNHDR (rconst_section, _RCONST, STYP_RCONST);
#endif
#ifdef _PDATA
  CHECK_SCNHDR (pdata_section, _PDATA, STYP_PDATA);
#endif /* _PDATA */
#ifdef _GOT
  CHECK_SCNHDR (got_section,   _GOT,   STYP_GOT);
#endif /* _GOT */
  CHECK_SCNHDR (data_section,  _DATA,  STYP_DATA);
#ifdef _XDATA
  CHECK_SCNHDR (xdata_section, _XDATA, STYP_XDATA);
#endif /* _XDATA */
#ifdef _LIT8
  CHECK_SCNHDR (lit8_section,  _LIT8,  STYP_LIT8);
  CHECK_SCNHDR (lit4_section,  _LIT4,  STYP_LIT4);
#endif /* _LIT8 */
  CHECK_SCNHDR (sdata_section, _SDATA, STYP_SDATA);
  CHECK_SCNHDR (sbss_section,  _SBSS,  STYP_SBSS);
  CHECK_SCNHDR (bss_section,   _BSS,   STYP_BSS);


  pagesize = getpagesize ();
  brk = (((long) (sbrk (0))) + pagesize - 1) & (-pagesize);

  /* Remember the current break */

  Brk = brk;

  bcopy (data_section, &old_data_scnhdr, sizeof (old_data_scnhdr));

  nhdr.aout.dsize = brk - DATA_START;
  nhdr.aout.bsize = 0;
  if (entry_address == 0)
    {
      extern DEFAULT_ENTRY_ADDRESS ();
      nhdr.aout.entry = (unsigned long)DEFAULT_ENTRY_ADDRESS;
    }
  else
    nhdr.aout.entry = entry_address;

  nhdr.aout.bss_start = nhdr.aout.data_start + nhdr.aout.dsize;

  if (rdata_section != NULL)
    {
      rdata_section->s_size = data_start - DATA_START;

      /* Adjust start and virtual addresses of rdata_section, too.  */
      rdata_section->s_vaddr = DATA_START;
      rdata_section->s_paddr = DATA_START;
      rdata_section->s_scnptr = text_section->s_scnptr + nhdr.aout.tsize;
    }

  data_section->s_vaddr = data_start;
  data_section->s_paddr = data_start;
  data_section->s_size = brk - data_start;

  if (rdata_section != NULL)
    {
      data_section->s_scnptr = rdata_section->s_scnptr + rdata_section->s_size;
    }

  vaddr = data_section->s_vaddr + data_section->s_size;
  scnptr = data_section->s_scnptr + data_section->s_size;
  if (lit8_section != NULL)
    {
      lit8_section->s_vaddr = vaddr;
      lit8_section->s_paddr = vaddr;
      lit8_section->s_size = 0;
      lit8_section->s_scnptr = scnptr;
    }
  if (lit4_section != NULL)
    {
      lit4_section->s_vaddr = vaddr;
      lit4_section->s_paddr = vaddr;
      lit4_section->s_size = 0;
      lit4_section->s_scnptr = scnptr;
    }
  if (sdata_section != NULL)
    {
      sdata_section->s_vaddr = vaddr;
      sdata_section->s_paddr = vaddr;
      sdata_section->s_size = 0;
      sdata_section->s_scnptr = scnptr;
    }
#ifdef _XDATA
  if (xdata_section != NULL)
    {
      xdata_section->s_vaddr = vaddr;
      xdata_section->s_paddr = vaddr;
      xdata_section->s_size = 0;
      xdata_section->s_scnptr = scnptr;
    }
#endif
#ifdef _GOT
  if (got_section != NULL)
    {
      bcopy (got_section, buffer, sizeof (struct scnhdr));

      got_section->s_vaddr = vaddr;
      got_section->s_paddr = vaddr;
      got_section->s_size = 0;
      got_section->s_scnptr = scnptr;
    }
#endif /*_GOT */
  if (sbss_section != NULL)
    {
      sbss_section->s_vaddr = vaddr;
      sbss_section->s_paddr = vaddr;
      sbss_section->s_size = 0;
      sbss_section->s_scnptr = scnptr;
    }
  if (bss_section != NULL)
    {
      bss_section->s_vaddr = vaddr;
      bss_section->s_paddr = vaddr;
      bss_section->s_size = 0;
      bss_section->s_scnptr = scnptr;
    }

  WRITE (new, (char *)TEXT_START, nhdr.aout.tsize,
	 "writing text section to %s", new_name);
  WRITE (new, (char *)DATA_START, nhdr.aout.dsize,
	 "writing data section to %s", new_name);

#ifdef _GOT
#define old_got_section ((struct scnhdr *)buffer)

  if (got_section != NULL)
    {
      SEEK (new, old_got_section->s_scnptr,
	    "seeking to start of got_section in %s", new_name);
      WRITE (new, oldptr + old_got_section->s_scnptr, old_got_section->s_size,
	     "writing new got_section of %s", new_name);
      SEEK (new, nhdr.aout.tsize + nhdr.aout.dsize,
	    "seeking to end of data section of %s", new_name);
    }

#undef old_got_section
#endif

  /*
   * Construct new symbol table header
   */

  bcopy (oldptr + nhdr.fhdr.f_symptr, buffer, cbHDRR);

#define symhdr ((pHDRR)buffer)
  newsyms = nhdr.aout.tsize + nhdr.aout.dsize;
  symrel = newsyms - nhdr.fhdr.f_symptr;
  nhdr.fhdr.f_symptr = newsyms;
  symhdr->cbLineOffset += symrel;
  symhdr->cbDnOffset += symrel;
  symhdr->cbPdOffset += symrel;
  symhdr->cbSymOffset += symrel;
  symhdr->cbOptOffset += symrel;
  symhdr->cbAuxOffset += symrel;
  symhdr->cbSsOffset += symrel;
  symhdr->cbSsExtOffset += symrel;
  symhdr->cbFdOffset += symrel;
  symhdr->cbRfdOffset += symrel;
  symhdr->cbExtOffset += symrel;

  WRITE (new, buffer, cbHDRR, "writing symbol table header of %s", new_name);

  /*
   * Copy the symbol table and line numbers
   */
  WRITE (new, oldptr + ohdr.fhdr.f_symptr + cbHDRR,
	 stat.st_size - ohdr.fhdr.f_symptr - cbHDRR,
	 "writing symbol table of %s", new_name);

#ifdef _REL_DYN
  if (rel_dyn_section)
    update_dynamic_symbols (oldptr, new_name, new, nhdr.aout);
#endif

#undef symhdr

  SEEK (new, 0, "seeking to start of header in %s", new_name);
  WRITE (new, &nhdr, sizeof (nhdr),
	 "writing header of %s", new_name);

  close (old);
  close (new);
  mark_x (new_name);
}


static void
update_dynamic_symbols (old, new_name, new, aout)
     char *old;			/* Pointer to old executable */
     char *new_name;            /* Name of new executable */
     int new;			/* File descriptor for new executable */
     struct aouthdr aout;	/* a.out info from the file header */
{
#if !defined (__linux__) && !defined (__NetBSD__) && !defined (__OpenBSD__)

  typedef struct dynrel_info {
    char * addr;
    unsigned type:8;
    unsigned index:24;
    unsigned info:8;
    unsigned pad:8;
  } dr_info;

  int nsyms = rel_dyn_section->s_size / sizeof (struct dynrel_info);
  int i;
  dr_info * rd_base = (dr_info *) (old + rel_dyn_section->s_scnptr);
  Elf32_Sym * ds_base = (Elf32_Sym *) (old + dynsym_section->s_scnptr);

  for (i = 0; i < nsyms; i++) {
    register Elf32_Sym x;

    if (rd_base[i].index == 0)
      continue;

    x = ds_base[rd_base[i].index];

#if 0
      fprintf (stderr, "Object inspected: %s, addr = %lx, shndx = %x",
	       old + dynstr_section->s_scnptr + x.st_name, rd_base[i].addr, x.st_shndx);
#endif


    if ((ELF32_ST_BIND (x.st_info) == STB_GLOBAL)
	&& (x.st_shndx == 0)
	/* && (x.st_value == NULL) */
	) {
      /* OK, this is probably a reference to an object in a shared
	 library, so copy the old value. This is done in several steps:
	 1. reladdr is the address of the location in question relative to
            the start of the data section,
         2. oldref is the addr is the mapped in temacs executable,
         3. newref is the address of the location in question in the
            undumped executable,
         4. len is the size of the object reference in bytes --
            currently only 4 (long) and 8 (quad) are supported.
	    */
      register unsigned long reladdr = (long)rd_base[i].addr - old_data_scnhdr.s_vaddr;
      char * oldref = old + old_data_scnhdr.s_scnptr + reladdr;
      unsigned long newref = aout.tsize + reladdr;
      int len;

#if 0
      fprintf (stderr, "...relocated\n");
#endif

      if (rd_base[i].type == R_REFLONG)
	len = 4;
      else if (rd_base[i].type == R_REFQUAD)
	len = 8;
      else
	fatal_unexec ("unrecognized relocation type in .dyn.rel section (symbol #%d)", (char *) i);

      SEEK (new, newref, "seeking to dynamic symbol in %s", new_name);
      WRITE (new, oldref, len, "writing old dynrel info in %s", new_name);
    }

#if 0
    else
      fprintf (stderr, "...not relocated\n");
#endif

  }

#endif /* not __linux__ and not __NetBSD__ and not __OpenBSD__ */
}


/*
 * mark_x
 *
 * After successfully building the new a.out, mark it executable
 */

static void
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um = umask (777);
  umask (um);
  if (stat (name, &sbuf) < 0)
    fatal_unexec ("getting protection on %s", name);
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) < 0)
    fatal_unexec ("setting protection on %s", name);
}

static void
fatal_unexec (s, arg)
     char *s;
     char *arg;
{
  if (errno == EEOF)
    fputs ("unexec: unexpected end of file, ", stderr);
  else
    fprintf (stderr, "unexec: %s, ", strerror (errno));
  fprintf (stderr, s, arg);
  fputs (".\n", stderr);
  exit (1);
}

/* arch-tag: 46316c49-ee08-4aa3-942b-00798902f5bd
   (do not change this comment) */
