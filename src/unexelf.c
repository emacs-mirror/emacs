/* Copyright (C) 1985-1988, 1990, 1992, 1999-2017 Free Software
   Foundation, Inc.

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


/*
 * unexec.c - Convert a running program into an a.out file.
 *
 * Author:	Spencer W. Thomas
 *		Computer Science Dept.
 *		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified heavily since then.
 *
 * Synopsis:
 *	unexec (const char *new_name, const char *old_name);
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If old_name is non-NULL, the symbol table will be taken from the given file.
 * On some machines, an existing old_name file is required.
 *
 */

/* We do not use mmap because that fails with NFS.
   Instead we read the whole file, modify it, and write it out.  */

#include <config.h>
#include "unexec.h"
#include "lisp.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <memory.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __QNX__
# include <sys/elf.h>
#elif !defined __NetBSD__ && !defined __OpenBSD__
# include <elf.h>
#endif
#include <sys/mman.h>
#if defined (_SYSTYPE_SYSV)
#include <sys/elf_mips.h>
#include <sym.h>
#endif /* _SYSTYPE_SYSV */

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON 0
#endif
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#if defined (__alpha__) && !defined (__NetBSD__) && !defined (__OpenBSD__)
/* Declare COFF debugging symbol table.  This used to be in
   /usr/include/sym.h, but this file is no longer included in Red Hat
   5.0 and presumably in any other glibc 2.x based distribution.  */
typedef struct {
	short magic;
	short vstamp;
	int ilineMax;
	int idnMax;
	int ipdMax;
	int isymMax;
	int ioptMax;
	int iauxMax;
	int issMax;
	int issExtMax;
	int ifdMax;
	int crfd;
	int iextMax;
	long cbLine;
	long cbLineOffset;
	long cbDnOffset;
	long cbPdOffset;
	long cbSymOffset;
	long cbOptOffset;
	long cbAuxOffset;
	long cbSsOffset;
	long cbSsExtOffset;
	long cbFdOffset;
	long cbRfdOffset;
	long cbExtOffset;
} HDRR, *pHDRR;
#define cbHDRR sizeof (HDRR)
#define hdrNil ((pHDRR)0)
#endif

#ifdef __NetBSD__
/*
 * NetBSD does not have normal-looking user-land ELF support.
 */
# if defined __alpha__ || defined __sparc_v9__ || defined _LP64
#  define ELFSIZE	64
# else
#  define ELFSIZE	32
# endif
# include <sys/exec_elf.h>

# ifndef PT_LOAD
#  define PT_LOAD	Elf_pt_load
#  if 0						/* was in pkgsrc patches for 20.7 */
#   define SHT_PROGBITS Elf_sht_progbits
#  endif
#  define SHT_SYMTAB	Elf_sht_symtab
#  define SHT_DYNSYM	Elf_sht_dynsym
#  define SHT_NULL	Elf_sht_null
#  define SHT_NOBITS	Elf_sht_nobits
#  define SHT_REL	Elf_sht_rel
#  define SHT_RELA	Elf_sht_rela

#  define SHN_UNDEF	Elf_eshn_undefined
#  define SHN_ABS	Elf_eshn_absolute
#  define SHN_COMMON	Elf_eshn_common
# endif /* !PT_LOAD */

# ifdef __alpha__
#  include <sys/exec_ecoff.h>
#  define HDRR		struct ecoff_symhdr
#  define pHDRR		HDRR *
# endif /* __alpha__ */

#ifdef __mips__			/* was in pkgsrc patches for 20.7 */
# define SHT_MIPS_DEBUG	DT_MIPS_FLAGS
# define HDRR		struct Elf_Shdr
#endif /* __mips__ */
#endif /* __NetBSD__ */

#ifdef __OpenBSD__
# include <sys/exec_elf.h>
#endif

#if __GNU_LIBRARY__ - 0 >= 6
# include <link.h>	/* get ElfW etc */
#endif

#ifndef ElfW
# define ElfBitsW(bits, type) Elf##bits##_##type
# ifndef ELFSIZE
#  ifdef _LP64
#   define ELFSIZE 64
#  else
#   define ELFSIZE 32
#  endif
# endif
  /* This macro expands `bits' before invoking ElfBitsW.  */
# define ElfExpandBitsW(bits, type) ElfBitsW (bits, type)
# define ElfW(type) ElfExpandBitsW (ELFSIZE, type)
#endif

/* The code often converts ElfW (Half) values like e_shentsize to ptrdiff_t;
   check that this doesn't lose information.  */
#include <intprops.h>
#include <verify.h>
verify ((! TYPE_SIGNED (ElfW (Half))
	 || PTRDIFF_MIN <= TYPE_MINIMUM (ElfW (Half)))
	&& TYPE_MAXIMUM (ElfW (Half)) <= PTRDIFF_MAX);

#ifdef UNEXELF_DEBUG
# define DEBUG_LOG(expr) fprintf (stderr, #expr " 0x%jx\n", (uintmax_t) (expr))
#endif

/* Get the address of a particular section or program header entry,
 * accounting for the size of the entries.
 */

static void *
entry_address (void *section_h, ptrdiff_t idx, ptrdiff_t entsize)
{
  char *h = section_h;
  return h + idx * entsize;
}

#define OLD_SECTION_H(n) \
  (*(ElfW (Shdr) *) entry_address (old_section_h, n, old_file_h->e_shentsize))
#define NEW_SECTION_H(n) \
  (*(ElfW (Shdr) *) entry_address (new_section_h, n, new_file_h->e_shentsize))
#define OLD_PROGRAM_H(n) \
  (*(ElfW (Phdr) *) entry_address (old_program_h, n, old_file_h->e_phentsize))

typedef unsigned char byte;

/* ****************************************************************
 * unexec
 *
 * driving logic.
 *
 * In ELF, this works by replacing the old bss SHT_NOBITS section with
 * a new, larger, SHT_PROGBITS section.
 *
 */
void
unexec (const char *new_name, const char *old_name)
{
  int new_file, old_file;
  off_t new_file_size;

  /* Pointers to the base of the image of the two files.  */
  caddr_t old_base, new_base;

#if MAP_ANON == 0
  int mmap_fd;
#else
# define mmap_fd -1
#endif

  /* Pointers to the file, program and section headers for the old and
     new files.  */
  ElfW (Ehdr) *old_file_h, *new_file_h;
  ElfW (Phdr) *old_program_h, *new_program_h;
  ElfW (Shdr) *old_section_h, *new_section_h;

  /* Point to the section name table.  */
  char *old_section_names, *new_section_names;

  ElfW (Phdr) *old_bss_seg, *new_bss_seg;
  ElfW (Addr) old_bss_addr, new_bss_addr;
  ElfW (Word) old_bss_size, bss_size_growth, new_data2_size;
  ElfW (Off) old_bss_offset, new_data2_offset;

  ptrdiff_t n;
  ptrdiff_t old_bss_index;
  struct stat stat_buf;
  off_t old_file_size;

  /* Open the old file, allocate a buffer of the right size, and read
     in the file contents.  */

  old_file = emacs_open (old_name, O_RDONLY, 0);

  if (old_file < 0)
    fatal ("Can't open %s for reading: %s", old_name, strerror (errno));

  if (fstat (old_file, &stat_buf) != 0)
    fatal ("Can't fstat (%s): %s", old_name, strerror (errno));

#if MAP_ANON == 0
  mmap_fd = emacs_open ("/dev/zero", O_RDONLY, 0);
  if (mmap_fd < 0)
    fatal ("Can't open /dev/zero for reading: %s", strerror (errno));
#endif

  /* We cannot use malloc here because that may use sbrk.  If it does,
     we'd dump our temporary buffers with Emacs, and we'd have to be
     extra careful to use the correct value of sbrk(0) after
     allocating all buffers in the code below, which we aren't.  */
  old_file_size = stat_buf.st_size;
  if (! (0 <= old_file_size && old_file_size <= SIZE_MAX))
    fatal ("File size out of range");
  old_base = mmap (NULL, old_file_size, PROT_READ | PROT_WRITE,
		   MAP_ANON | MAP_PRIVATE, mmap_fd, 0);
  if (old_base == MAP_FAILED)
    fatal ("Can't allocate buffer for %s: %s", old_name, strerror (errno));

  if (read (old_file, old_base, old_file_size) != old_file_size)
    fatal ("Didn't read all of %s: %s", old_name, strerror (errno));

  /* Get pointers to headers & section names */

  old_file_h = (ElfW (Ehdr) *) old_base;
  old_program_h = (ElfW (Phdr) *) ((byte *) old_base + old_file_h->e_phoff);
  old_section_h = (ElfW (Shdr) *) ((byte *) old_base + old_file_h->e_shoff);
  old_section_names = (char *) old_base
    + OLD_SECTION_H (old_file_h->e_shstrndx).sh_offset;

  /* Find the PT_LOAD header covering the highest address.  This
     segment will be where bss sections are located, past p_filesz.  */
  old_bss_seg = 0;
  for (n = old_file_h->e_phnum; --n >= 0; )
    {
      ElfW (Phdr) *seg = &OLD_PROGRAM_H (n);
      if (seg->p_type == PT_LOAD
	  && (old_bss_seg == 0
	      || seg->p_vaddr > old_bss_seg->p_vaddr))
	old_bss_seg = seg;
    }

  /* Note that old_bss_addr may be lower than the first bss section
     address, since the section may need aligning.  */
  old_bss_addr = old_bss_seg->p_vaddr + old_bss_seg->p_filesz;
  old_bss_offset = old_bss_seg->p_offset + old_bss_seg->p_filesz;
  old_bss_size = old_bss_seg->p_memsz - old_bss_seg->p_filesz;

  /* Find the last bss style section in the bss segment range.  */
  old_bss_index = -1;
  for (n = old_file_h->e_shnum; --n > 0; )
    {
      ElfW (Shdr) *shdr = &OLD_SECTION_H (n);
      if (shdr->sh_type == SHT_NOBITS
	  && shdr->sh_addr >= old_bss_addr
	  && shdr->sh_addr + shdr->sh_size <= old_bss_addr + old_bss_size
	  && (old_bss_index == -1
	      || OLD_SECTION_H (old_bss_index).sh_addr < shdr->sh_addr))
	old_bss_index = n;
    }

  if (old_bss_index == -1)
    fatal ("no bss section found");

  void *no_break = (void *) (intptr_t) -1;
  void *new_break = no_break;
#ifdef HAVE_SBRK
  new_break = sbrk (0);
#endif
  if (new_break == no_break)
    new_break = (byte *) old_bss_addr + old_bss_size;
  new_bss_addr = (ElfW (Addr)) new_break;
  bss_size_growth = new_bss_addr - old_bss_addr;
  new_data2_size = bss_size_growth;
  new_data2_size += alignof (ElfW (Shdr)) - 1;
  new_data2_size -= new_data2_size % alignof (ElfW (Shdr));

  new_data2_offset = old_bss_offset;

#ifdef UNEXELF_DEBUG
  fprintf (stderr, "old_bss_index %td\n", old_bss_index);
  DEBUG_LOG (old_bss_addr);
  DEBUG_LOG (old_bss_size);
  DEBUG_LOG (old_bss_offset);
  DEBUG_LOG (new_bss_addr);
  DEBUG_LOG (new_data2_size);
  DEBUG_LOG (new_data2_offset);
#endif

  if (new_bss_addr < old_bss_addr + old_bss_size)
    fatal (".bss shrank when undumping");

  /* Set the output file to the right size.  Allocate a buffer to hold
     the image of the new file.  Set pointers to various interesting
     objects.  */

  new_file = emacs_open (new_name, O_RDWR | O_CREAT, 0777);
  if (new_file < 0)
    fatal ("Can't creat (%s): %s", new_name, strerror (errno));

  new_file_size = old_file_size + new_data2_size;

  if (ftruncate (new_file, new_file_size))
    fatal ("Can't ftruncate (%s): %s", new_name, strerror (errno));

  new_base = mmap (NULL, new_file_size, PROT_READ | PROT_WRITE,
		   MAP_ANON | MAP_PRIVATE, mmap_fd, 0);
  if (new_base == MAP_FAILED)
    fatal ("Can't allocate buffer for %s: %s", old_name, strerror (errno));

  /* Make our new file, program and section headers as copies of the
     originals.  */

  new_file_h = (ElfW (Ehdr) *) new_base;
  memcpy (new_file_h, old_file_h, old_file_h->e_ehsize);

  /* Fix up file header.  Section header is further away now.  */

  if (new_file_h->e_shoff >= old_bss_offset)
    new_file_h->e_shoff += new_data2_size;

  new_program_h = (ElfW (Phdr) *) ((byte *) new_base + new_file_h->e_phoff);
  new_section_h = (ElfW (Shdr) *) ((byte *) new_base + new_file_h->e_shoff);

  memcpy (new_program_h, old_program_h,
	  old_file_h->e_phnum * old_file_h->e_phentsize);
  memcpy (new_section_h, old_section_h,
	  old_file_h->e_shnum * old_file_h->e_shentsize);

#ifdef UNEXELF_DEBUG
  DEBUG_LOG (old_file_h->e_shoff);
  fprintf (stderr, "Old section count %td\n", (ptrdiff_t) old_file_h->e_shnum);
  DEBUG_LOG (new_file_h->e_shoff);
  fprintf (stderr, "New section count %td\n", (ptrdiff_t) new_file_h->e_shnum);
#endif

  /* Fix up program header.  Extend the writable data segment so
     that the bss area is covered too.  */

  new_bss_seg = new_program_h + (old_bss_seg - old_program_h);
  new_bss_seg->p_filesz = new_bss_addr - new_bss_seg->p_vaddr;
  new_bss_seg->p_memsz = new_bss_seg->p_filesz;

  /* Copy over what we have in memory now for the bss area. */
  memcpy (new_base + new_data2_offset, (caddr_t) old_bss_addr,
	  bss_size_growth);

  /* Walk through all section headers, copying data and updating.  */
  for (n = 1; n < old_file_h->e_shnum; n++)
    {
      caddr_t src;
      ElfW (Shdr) *old_shdr = &OLD_SECTION_H (n);
      ElfW (Shdr) *new_shdr = &NEW_SECTION_H (n);

      if (new_shdr->sh_type == SHT_NOBITS
	  && new_shdr->sh_addr >= old_bss_addr
	  && (new_shdr->sh_addr + new_shdr->sh_size
	      <= old_bss_addr + old_bss_size))
	{
	  /* This section now has file backing.  */
	  new_shdr->sh_type = SHT_PROGBITS;

	  /* SHT_NOBITS sections do not need a valid sh_offset, so it
	     might be incorrect.  Write the correct value.  */
	  new_shdr->sh_offset = (new_shdr->sh_addr - new_bss_seg->p_vaddr
				 + new_bss_seg->p_offset);

	  /* If this is was a SHT_NOBITS .plt section, then it is
	     probably a PowerPC PLT.  If it is PowerPC64 ELFv1 then
	     glibc ld.so doesn't initialize the toc pointer word.  A
	     non-zero toc pointer word can defeat Power7 thread safety
	     during lazy update of a PLT entry.  This only matters if
	     emacs becomes multi-threaded.  */
	  if (strcmp (old_section_names + new_shdr->sh_name, ".plt") == 0)
	    memset (new_shdr->sh_offset + new_base, 0, new_shdr->sh_size);

	  /* Extend the size of the last bss section to cover dumped
	     data.  */
	  if (n == old_bss_index)
	    new_shdr->sh_size = new_bss_addr - new_shdr->sh_addr;

	  /* We have already copied this section from the current
	     process.  */
	  continue;
	}

      /* Any section that was originally placed after the .bss
	 section should now be offset by NEW_DATA2_SIZE.  */
      if (new_shdr->sh_offset >= old_bss_offset)
	new_shdr->sh_offset += new_data2_size;

      /* Now, start to copy the content of sections.  */
      if (new_shdr->sh_type == SHT_NULL
	  || new_shdr->sh_type == SHT_NOBITS)
	continue;

      /* Some sections are copied from the current process instead of
	 the old file.  */
      if (!strcmp (old_section_names + new_shdr->sh_name, ".data")
	  || !strcmp (old_section_names + new_shdr->sh_name, ".sdata")
	  || !strcmp (old_section_names + new_shdr->sh_name, ".lit4")
	  || !strcmp (old_section_names + new_shdr->sh_name, ".lit8")
	  || !strcmp (old_section_names + new_shdr->sh_name, ".sdata1")
	  || !strcmp (old_section_names + new_shdr->sh_name, ".data1"))
	src = (caddr_t) old_shdr->sh_addr;
      else
	src = old_base + old_shdr->sh_offset;

      memcpy (new_shdr->sh_offset + new_base, src, new_shdr->sh_size);

#if (defined __alpha__ && !defined __OpenBSD__) || defined _SYSTYPE_SYSV
      /* Update Alpha and MIPS COFF debug symbol table.  */
      if (strcmp (old_section_names + new_shdr->sh_name, ".mdebug") == 0
	  && new_shdr->sh_offset - old_shdr->sh_offset != 0
#if defined _SYSTYPE_SYSV
	  && new_shdr->sh_type == SHT_MIPS_DEBUG
#endif
	  )
	{
	  ptrdiff_t diff = new_shdr->sh_offset - old_shdr->sh_offset;
	  HDRR *phdr = (HDRR *) (new_shdr->sh_offset + new_base);

	  phdr->cbLineOffset += diff;
	  phdr->cbDnOffset += diff;
	  phdr->cbPdOffset += diff;
	  phdr->cbSymOffset += diff;
	  phdr->cbOptOffset += diff;
	  phdr->cbAuxOffset += diff;
	  phdr->cbSsOffset += diff;
	  phdr->cbSsExtOffset += diff;
	  phdr->cbFdOffset += diff;
	  phdr->cbRfdOffset += diff;
	  phdr->cbExtOffset += diff;
	}
#endif /* __alpha__ || _SYSTYPE_SYSV */
    }

  /* Update the symbol values of _edata and _end.  */
  for (n = new_file_h->e_shnum; 0 < --n; )
    {
      byte *symnames;
      ElfW (Sym) *symp, *symendp;
      ElfW (Shdr) *sym_shdr = &NEW_SECTION_H (n);

      if (sym_shdr->sh_type != SHT_DYNSYM
	  && sym_shdr->sh_type != SHT_SYMTAB)
	continue;

      symnames = ((byte *) new_base
		  + NEW_SECTION_H (sym_shdr->sh_link).sh_offset);
      symp = (ElfW (Sym) *) (sym_shdr->sh_offset + new_base);
      symendp = (ElfW (Sym) *) ((byte *) symp + sym_shdr->sh_size);

      for (; symp < symendp; symp ++)
	{
	  if (strcmp ((char *) (symnames + symp->st_name), "_end") == 0
	      || strcmp ((char *) (symnames + symp->st_name), "end") == 0
	      || strcmp ((char *) (symnames + symp->st_name), "_edata") == 0
	      || strcmp ((char *) (symnames + symp->st_name), "edata") == 0)
	    memcpy (&symp->st_value, &new_bss_addr, sizeof (new_bss_addr));

	  /* Strictly speaking, #ifdef below is not necessary.  But we
	     keep it to indicate that this kind of change may also be
	     necessary for other unexecs to support GNUstep.  */
#ifdef NS_IMPL_GNUSTEP
	  /* ObjC runtime modifies the values of some data structures
	     such as classes and selectors in the .data section after
	     loading.  As the dump process copies the .data section
	     from the current process, that causes problems when the
	     modified classes are reinitialized in the dumped
	     executable.  We copy such data from the old file, not
	     from the current process.  */
	  if (strncmp ((char *) (symnames + symp->st_name),
		       "_OBJC_", sizeof ("_OBJC_") - 1) == 0)
	    {
	      ElfW (Shdr) *new_shdr = &NEW_SECTION_H (symp->st_shndx);
	      if (new_shdr->sh_type != SHT_NOBITS)
		{
		  ElfW (Shdr) *old_shdr = &OLD_SECTION_H (symp->st_shndx);
		  ptrdiff_t reladdr = symp->st_value - new_shdr->sh_addr;
		  ptrdiff_t newoff = reladdr + new_shdr->sh_offset;

		  if (old_shdr->sh_type == SHT_NOBITS)
		    memset (new_base + newoff, 0, symp->st_size);
		  else
		    {
		      ptrdiff_t oldoff = reladdr + old_shdr->sh_offset;
		      memcpy (new_base + newoff, old_base + oldoff,
			      symp->st_size);
		    }
		}
	    }
#endif
	}
    }

  /* Modify the names of sections we changed from SHT_NOBITS to
     SHT_PROGBITS.  This is really just cosmetic, but some tools that
     (wrongly) operate on section names rather than types might be
     confused by a SHT_PROGBITS .bss section.  */
  new_section_names = ((char *) new_base
		       + NEW_SECTION_H (new_file_h->e_shstrndx).sh_offset);
  for (n = new_file_h->e_shnum; 0 < --n; )
    {
      ElfW (Shdr) *old_shdr = &OLD_SECTION_H (n);
      ElfW (Shdr) *new_shdr = &NEW_SECTION_H (n);

      /* Replace the leading '.' with ','.  When .shstrtab is string
	 merged this will rename both .bss and .rela.bss to ,bss and
	 .rela,bss.  */
      if (old_shdr->sh_type == SHT_NOBITS
	  && new_shdr->sh_type == SHT_PROGBITS)
	*(new_section_names + new_shdr->sh_name) = ',';
    }

  /* This loop seeks out relocation sections for the data section, so
     that it can undo relocations performed by the runtime loader.

     The following approach does not work on x86 platforms that use
     the GNU Gold linker, which can generate .rel.dyn relocation
     sections containing R_386_32 entries that the following code does
     not grok.  Emacs works around this problem by avoiding C
     constructs that generate such entries, which is horrible hack.

     FIXME: Presumably more problems like this will crop up as linkers
     get fancier.  We really need to stop assuming that Emacs can grok
     arbitrary linker output.  See Bug#27248.  */
  for (n = new_file_h->e_shnum; 0 < --n; )
    {
      ElfW (Shdr) *rel_shdr = &NEW_SECTION_H (n);
      ElfW (Shdr) *shdr;

      switch (rel_shdr->sh_type)
	{
	default:
	  break;
	case SHT_REL:
	case SHT_RELA:
	  /* This code handles two different size structs, but there should
	     be no harm in that provided that r_offset is always the first
	     member.  */
	  shdr = &NEW_SECTION_H (rel_shdr->sh_info);
	  if (!strcmp (old_section_names + shdr->sh_name, ".data")
	      || !strcmp (old_section_names + shdr->sh_name, ".sdata")
	      || !strcmp (old_section_names + shdr->sh_name, ".lit4")
	      || !strcmp (old_section_names + shdr->sh_name, ".lit8")
	      || !strcmp (old_section_names + shdr->sh_name, ".sdata1")
	      || !strcmp (old_section_names + shdr->sh_name, ".data1"))
	    {
	      ElfW (Addr) offset = shdr->sh_addr - shdr->sh_offset;
	      caddr_t reloc = old_base + rel_shdr->sh_offset, end;
	      for (end = reloc + rel_shdr->sh_size;
		   reloc < end;
		   reloc += rel_shdr->sh_entsize)
		{
		  ElfW (Addr) addr = ((ElfW (Rel) *) reloc)->r_offset - offset;
		  /* Ignore R_*_NONE relocs.  */
		  if (((ElfW (Rel) *) reloc)->r_offset == 0)
		    continue;
		  /* Assume reloc applies to a word.
		     ??? This is not always true, eg. TLS module/index
		     pair in .got which occupies two words.  */
		  memcpy (new_base + addr, old_base + addr,
			  sizeof (ElfW (Addr)));
		}
	    }
	  break;
	}
    }

  /* Write out new_file, and free the buffers.  */

  if (write (new_file, new_base, new_file_size) != new_file_size)
    fatal ("Didn't write %lu bytes to %s: %s",
	   (unsigned long) new_file_size, new_name, strerror (errno));
  munmap (old_base, old_file_size);
  munmap (new_base, new_file_size);

  /* Close the files and make the new file executable.  */

#if MAP_ANON == 0
  emacs_close (mmap_fd);
#endif

  if (emacs_close (old_file) != 0)
    fatal ("Can't close (%s): %s", old_name, strerror (errno));

  if (emacs_close (new_file) != 0)
    fatal ("Can't close (%s): %s", new_name, strerror (errno));
}
