/* Unexec for Siemens machines running Sinix (modified SVR4).
   Copyright (C) 1985, 1986, 1987, 1988, 1990, 1992, 1993, 1994
   Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


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

/* Even more heavily modified by james@bigtex.cactus.org of Dell Computer Co.
 * ELF support added.
 *
 * Basic theory: the data space of the running process needs to be
 * dumped to the output file.  Normally we would just enlarge the size
 * of .data, scooting everything down.  But we can't do that in ELF,
 * because there is often something between the .data space and the
 * .bss space.
 *
 * In the temacs dump below, notice that the Global Offset Table
 * (.got) and the Dynamic link data (.dynamic) come between .data1 and
 * .bss.  It does not work to overlap .data with these fields.
 *
 * The solution is to create a new .data segment.  This segment is
 * filled with data from the current process.  Since the contents of
 * various sections refer to sections by index, the new .data segment
 * is made the last in the table to avoid changing any existing index.
 */

/* Modified by wtien@urbana.mcd.mot.com of Motorola Inc. 
 * 
 * The above mechanism does not work if the unexeced ELF file is being
 * re-layout by other applications (such as `strip'). All the applications 
 * that re-layout the internal of ELF will layout all sections in ascending
 * order of their file offsets. After the re-layout, the data2 section will 
 * still be the LAST section in the section header vector, but its file offset 
 * is now being pushed far away down, and causes part of it not to be mapped
 * in (ie. not covered by the load segment entry in PHDR vector), therefore 
 * causes the new binary to fail.
 *
 * The solution is to modify the unexec algorithm to insert the new data2
 * section header right before the new bss section header, so their file
 * offsets will be in the ascending order. Since some of the section's (all 
 * sections AFTER the bss section) indexes are now changed, we also need to 
 * modify some fields to make them point to the right sections. This is done 
 * by macro PATCH_INDEX. All the fields that need to be patched are:
 * 
 * 1. ELF header e_shstrndx field.
 * 2. section header sh_link and sh_info field.
 * 3. symbol table entry st_shndx field.
 */

/*
 * New modifications for Siemens Nixdorf's MIPS-based machines.
 * Marco.Walther@mch.sni.de
 *
 * The problem: Before the bss segment we have a so called sbss segment
 *              (small bss) and maybe an sdata segment. These segments
 *              must also be handled correct.
 *
 * /home1/marco/emacs/emacs-19.22/src
 * dump -hv temacs
 * 
 * temacs:
 *
 *	   **** SECTION HEADER TABLE ****
 * [No]	Type	Flags	Addr         Offset       Size        	Name
 *	Link	Info	Adralgn      Entsize
 *
 * [1]	PBIT    -A--	0x4000f4     0xf4         0x13         	.interp
 *	0	0	0x1          0            
 *
 * [2]	REGI    -A--	0x400108     0x108        0x18         	.reginfo
 *	0	0	0x4          0x18         
 *
 * [3]	DYNM    -A--	0x400120     0x120        0xb8         	.dynamic
 *	6	0	0x4          0x8          
 *
 * [4]	HASH    -A--	0x4001d8     0x1d8        0x8a0        	.hash
 *	5	0	0x4          0x4          
 *
 * [5]	DYNS    -A--	0x400a78     0xa78        0x11f0       	.dynsym
 *	6	2	0x4          0x10         
 *
 * [6]	STRT    -A--	0x401c68     0x1c68       0xbf9        	.dynstr
 *	0	0	0x1          0            
 *
 * [7]	REL     -A--	0x402864     0x2864       0x18         	.rel.dyn
 *	5	14	0x4          0x8          
 *
 * [8]	PBIT    -AI-	0x402880     0x2880       0x60         	.init
 *	0	0	0x10         0x1          
 *
 * [9]	PBIT    -AI-	0x4028e0     0x28e0       0x1234       	.plt
 *	0	0	0x4          0x4          
 *
 * [10]	PBIT    -AI-	0x403b20     0x3b20       0xee400      	.text
 *	0	0	0x20         0x1          
 *
 * [11]	PBIT    -AI-	0x4f1f20     0xf1f20      0x60         	.fini
 *	0	0	0x10         0x1          
 *
 * [12]	PBIT    -A--	0x4f1f80     0xf1f80      0xd90        	.rdata
 *	0	0	0x10         0x1          
 *
 * [13]	PBIT    -A--	0x4f2d10     0xf2d10      0x17e0       	.rodata
 *	0	0	0x10         0x1          
 *
 * [14]	PBIT    WA--	0x5344f0     0xf44f0      0x4b3e4      	.data  <<<<<
 *	0	0	0x10         0x1          
 *
 * [15]	PBIT    WA-G	0x57f8d4     0x13f8d4     0x2a84       	.got
 *	0	0	0x4          0x4          
 *
 * [16]	PBIT    WA-G	0x582360     0x142360     0x10         	.sdata <<<<<
 *	0	0	0x10         0x1          
 *
 * [17]	NOBI    WA-G	0x582370     0x142370     0xb84        	.sbss  <<<<<
 *	0	0	0x4          0            
 *
 * [18]	NOBI    WA--	0x582f00     0x142370     0x27ec0      	.bss   <<<<<
 *	0	0	0x10         0x1          
 *
 * [19]	SYMT    ----	0            0x142370     0x10e40      	.symtab
 *	20	1108	0x4          0x10         
 *
 * [20]	STRT    ----	0            0x1531b0     0xed9e       	.strtab
 *	0	0	0x1          0            
 *
 * [21]	STRT    ----	0            0x161f4e     0xb5         	.shstrtab
 *	0	0	0x1          0            
 *
 * [22]	PBIT    ----	0            0x162003     0x28e2a      	.comment
 *	0	0	0x1          0x1          
 *
 * [23]	PBIT    ----	0            0x18ae2d     0x592        	.debug
 *	0	0	0x1          0            
 *
 * [24]	PBIT    ----	0            0x18b3bf     0x80         	.line
 *	0	0	0x1          0            
 *
 * [25]	MDBG    ----	0            0x18b440     0x60         	.mdebug
 *	0	0	0x4          0            
 *
 *
 * dump -hv emacs
 * 
 * emacs:
 *
 *	   **** SECTION HEADER TABLE ****
 * [No]	Type	Flags	Addr         Offset       Size        	Name
 *	Link	Info	Adralgn      Entsize
 *
 * [1]	PBIT    -A--	0x4000f4     0xf4         0x13         	.interp
 *	0	0	0x1          0            
 *
 * [2]	REGI    -A--	0x400108     0x108        0x18         	.reginfo
 *	0	0	0x4          0x18         
 *
 * [3]	DYNM    -A--	0x400120     0x120        0xb8         	.dynamic
 *	6	0	0x4          0x8          
 *
 * [4]	HASH    -A--	0x4001d8     0x1d8        0x8a0        	.hash
 *	5	0	0x4          0x4          
 *
 * [5]	DYNS    -A--	0x400a78     0xa78        0x11f0       	.dynsym
 *	6	2	0x4          0x10         
 *
 * [6]	STRT    -A--	0x401c68     0x1c68       0xbf9        	.dynstr
 *	0	0	0x1          0            
 *
 * [7]	REL     -A--	0x402864     0x2864       0x18         	.rel.dyn
 *	5	14	0x4          0x8          
 *
 * [8]	PBIT    -AI-	0x402880     0x2880       0x60         	.init
 *	0	0	0x10         0x1          
 *
 * [9]	PBIT    -AI-	0x4028e0     0x28e0       0x1234       	.plt
 *	0	0	0x4          0x4          
 *
 * [10]	PBIT    -AI-	0x403b20     0x3b20       0xee400      	.text
 *	0	0	0x20         0x1          
 *
 * [11]	PBIT    -AI-	0x4f1f20     0xf1f20      0x60         	.fini
 *	0	0	0x10         0x1          
 *
 * [12]	PBIT    -A--	0x4f1f80     0xf1f80      0xd90        	.rdata
 *	0	0	0x10         0x1          
 *
 * [13]	PBIT    -A--	0x4f2d10     0xf2d10      0x17e0       	.rodata
 *	0	0	0x10         0x1          
 *
 * [14]	PBIT    WA--	0x5344f0     0xf44f0      0x4b3e4      	.data  <<<<<
 *	0	0	0x10         0x1          
 *
 * [15]	PBIT    WA-G	0x57f8d4     0x13f8d4     0x2a84       	.got
 *	0	0	0x4          0x4          
 *
 * [16]	PBIT    WA-G	0x582360     0x142360     0xb94        	.sdata <<<<<
 *	0	0	0x10         0x1          
 *
 * [17]	PBIT    WA--	0x582f00     0x142f00     0x94100      	.data  <<<<<
 *	0	0	0x10         0x1          
 *
 * [18]	NOBI    WA-G	0x617000     0x1d7000     0            	.sbss  <<<<<
 *	0	0	0x4          0            
 *
 * [19]	NOBI    WA--	0x617000     0x1d7000     0            	.bss   <<<<<
 *	0	0	0x4          0x1          
 *
 * [20]	SYMT    ----	0            0x1d7000     0x10e40      	.symtab
 *	21	1109	0x4          0x10         
 *
 * [21]	STRT    ----	0            0x1e7e40     0xed9e       	.strtab
 *	0	0	0x1          0            
 *
 * [22]	STRT    ----	0            0x1f6bde     0xb5         	.shstrtab
 *	0	0	0x1          0            
 *
 * [23]	PBIT    ----	0            0x1f6c93     0x28e2a      	.comment
 *	0	0	0x1          0x1          
 *
 * [24]	PBIT    ----	0            0x21fabd     0x592        	.debug
 *	0	0	0x1          0            
 *
 * [25]	PBIT    ----	0            0x22004f     0x80         	.line
 *	0	0	0x1          0            
 *
 * [26]	MDBG    ----	0            0x2200d0     0x60         	.mdebug
 *	0	0	0x4          0            
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <memory.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>
#include <sys/mman.h>

#ifndef emacs
#define fatal(a, b, c) fprintf(stderr, a, b, c), exit(1)
#else
extern void fatal(char *, ...);
#endif

/* Get the address of a particular section or program header entry,
 * accounting for the size of the entries.
 */

#define OLD_SECTION_H(n) \
     (*(Elf32_Shdr *) ((byte *) old_section_h + old_file_h->e_shentsize * (n)))
#define NEW_SECTION_H(n) \
     (*(Elf32_Shdr *) ((byte *) new_section_h + new_file_h->e_shentsize * (n)))
#define OLD_PROGRAM_H(n) \
     (*(Elf32_Phdr *) ((byte *) old_program_h + old_file_h->e_phentsize * (n)))
#define NEW_PROGRAM_H(n) \
     (*(Elf32_Phdr *) ((byte *) new_program_h + new_file_h->e_phentsize * (n)))

#define PATCH_INDEX(n) \
  do { \
	 if ((n) >= old_sbss_index) \
	   (n) += 1 + (old_sdata_index ? 0 : 1); } while (0)

typedef unsigned char byte;

/* Round X up to a multiple of Y.  */

int
round_up (x, y)
     int x, y;
{
  int rem = x % y;
  if (rem == 0)
    return x;
  return x - rem + y;
}

/* ****************************************************************
 * unexec
 *
 * driving logic.
 *
 * In ELF, this works by replacing the old .bss section with a new
 * .data section, and inserting an empty .bss immediately afterwards.
 *
 */
void
unexec (new_name, old_name, data_start, bss_start, entry_address)
     char *new_name, *old_name;
     unsigned data_start, bss_start, entry_address;
{
  extern unsigned int bss_end;
  int new_file, old_file, new_file_size;

  /* Pointers to the base of the image of the two files. */
  caddr_t old_base, new_base;

  /* Pointers to the file, program and section headers for the old and new
   * files.
   */
  Elf32_Ehdr *old_file_h, *new_file_h;
  Elf32_Phdr *old_program_h, *new_program_h;
  Elf32_Shdr *old_section_h, *new_section_h;

  /* Point to the section name table in the old file */
  char *old_section_names;

  Elf32_Addr old_bss_addr, new_bss_addr;
  Elf32_Addr old_sbss_addr;
  Elf32_Word old_bss_size, new_data2_size;
  Elf32_Word old_sbss_size, new_data3_size;
  Elf32_Off  new_data2_offset;
  Elf32_Off  new_data3_offset;
  Elf32_Addr new_data2_addr;
  Elf32_Addr new_data3_addr;

  Elf32_Word old_sdata_size, new_sdata_size;
  int old_sdata_index = 0;

  int n, nn, old_data_index, new_data2_align;
  int old_bss_index;
  int old_sbss_index;
  int old_bss_padding;
  struct stat stat_buf;

  /* Open the old file & map it into the address space. */

  old_file = open (old_name, O_RDONLY);

  if (old_file < 0)
    fatal ("Can't open %s for reading: errno %d\n", old_name, errno);

  if (fstat (old_file, &stat_buf) == -1)
    fatal ("Can't fstat(%s): errno %d\n", old_name, errno);

  old_base = mmap (0, stat_buf.st_size, PROT_READ, MAP_SHARED, old_file, 0);

  if (old_base == (caddr_t) -1)
    fatal ("Can't mmap(%s): errno %d\n", old_name, errno);

#ifdef DEBUG
  fprintf (stderr, "mmap(%s, %x) -> %x\n", old_name, stat_buf.st_size,
	   old_base);
#endif

  /* Get pointers to headers & section names */

  old_file_h = (Elf32_Ehdr *) old_base;
  old_program_h = (Elf32_Phdr *) ((byte *) old_base + old_file_h->e_phoff);
  old_section_h = (Elf32_Shdr *) ((byte *) old_base + old_file_h->e_shoff);
  old_section_names = (char *) old_base
    + OLD_SECTION_H(old_file_h->e_shstrndx).sh_offset;

  /* Find the old .sbss section.
   */

  for (old_sbss_index = 1; old_sbss_index < old_file_h->e_shnum;
       old_sbss_index++)
    {
#ifdef DEBUG
      fprintf (stderr, "Looking for .sbss - found %s\n",
	       old_section_names + OLD_SECTION_H(old_sbss_index).sh_name);
#endif
      if (!strcmp (old_section_names + OLD_SECTION_H(old_sbss_index).sh_name,
		   ".sbss"))
	break;
    }
  if (old_sbss_index == old_file_h->e_shnum)
    fatal ("Can't find .sbss in %s.\n", old_name, 0);

  if (!strcmp(old_section_names + OLD_SECTION_H(old_sbss_index - 1).sh_name,
	       ".sdata"))
    {
      old_sdata_index = old_sbss_index - 1;
    }
  

  /* Find the old .bss section.
   */

  for (old_bss_index = 1; old_bss_index < old_file_h->e_shnum; old_bss_index++)
    {
#ifdef DEBUG
      fprintf (stderr, "Looking for .bss - found %s\n",
	       old_section_names + OLD_SECTION_H(old_bss_index).sh_name);
#endif
      if (!strcmp (old_section_names + OLD_SECTION_H(old_bss_index).sh_name,
		   ".bss"))
	break;
    }
  if (old_bss_index == old_file_h->e_shnum)
    fatal ("Can't find .bss in %s.\n", old_name, 0);

  if (old_sbss_index != (old_bss_index - 1))
    fatal (".sbss should come immediatly before .bss in %s.\n", old_name, 0);

  /* Figure out parameters of the new data3 and data2 sections.
   * Change the sbss and bss sections.
   */

  old_bss_addr = OLD_SECTION_H(old_bss_index).sh_addr;
  old_bss_size = OLD_SECTION_H(old_bss_index).sh_size;

  old_sbss_addr = OLD_SECTION_H(old_sbss_index).sh_addr;
  old_sbss_size = OLD_SECTION_H(old_sbss_index).sh_size;

  if (old_sdata_index)
    {
    old_sdata_size = OLD_SECTION_H(old_sdata_index).sh_size;
    }

#if defined(emacs) || !defined(DEBUG)
  bss_end = (unsigned int) sbrk (0);
  new_bss_addr = (Elf32_Addr) bss_end;
#else
  new_bss_addr = old_bss_addr + old_bss_size + 0x1234;
#endif
  if (old_sdata_index)
    {
    new_sdata_size = OLD_SECTION_H(old_sbss_index).sh_offset -
		     OLD_SECTION_H(old_sdata_index).sh_offset + old_sbss_size;
    }

  new_data3_addr = old_sbss_addr;
  new_data3_size = old_sbss_size;
  new_data3_offset = OLD_SECTION_H(old_sbss_index).sh_offset;

  new_data2_addr = old_bss_addr;
  new_data2_size = new_bss_addr - old_bss_addr;
  new_data2_align = (new_data3_offset + old_sbss_size) %
		    OLD_SECTION_H(old_bss_index).sh_addralign;
  new_data2_align = new_data2_align ?
		    OLD_SECTION_H(old_bss_index).sh_addralign - new_data2_align :
		    0;
  new_data2_offset = new_data3_offset + old_sbss_size + new_data2_align;

  old_bss_padding = OLD_SECTION_H(old_bss_index).sh_offset -
		    OLD_SECTION_H(old_sbss_index).sh_offset;
#ifdef DEBUG
  fprintf (stderr, "old_bss_index %d\n", old_bss_index);
  fprintf (stderr, "old_bss_addr %x\n", old_bss_addr);
  fprintf (stderr, "old_bss_size %x\n", old_bss_size);
  fprintf (stderr, "new_bss_addr %x\n", new_bss_addr);
  fprintf (stderr, "new_data2_addr %x\n", new_data2_addr);
  fprintf (stderr, "new_data2_size %x\n", new_data2_size);
  fprintf (stderr, "new_data2_offset %x\n", new_data2_offset);
  fprintf (stderr, "old_sbss_index %d\n", old_sbss_index);
  fprintf (stderr, "old_sbss_addr %x\n", old_sbss_addr);
  fprintf (stderr, "old_sbss_size %x\n", old_sbss_size);
  if (old_sdata_index)
    {
    fprintf (stderr, "old_sdata_size %x\n", old_sdata_size);
    fprintf (stderr, "new_sdata_size %x\n", new_sdata_size);
    }
  else
    {
    fprintf (stderr, "new_data3_addr %x\n", new_data3_addr);
    fprintf (stderr, "new_data3_size %x\n", new_data3_size);
    fprintf (stderr, "new_data3_offset %x\n", new_data3_offset);
    }
#endif

  if ((unsigned) new_bss_addr < (unsigned) old_bss_addr + old_bss_size)
    fatal (".bss shrank when undumping???\n", 0, 0);

  /* Set the output file to the right size and mmap(2) it.  Set
   * pointers to various interesting objects.  stat_buf still has
   * old_file data.
   */

  new_file = open (new_name, O_RDWR | O_CREAT, 0666);
  if (new_file < 0)
    fatal ("Can't creat(%s): errno %d\n", new_name, errno);

  new_file_size = stat_buf.st_size +
		  ((1 + (old_sdata_index ? 0 : 1)) * old_file_h->e_shentsize) +
		  new_data2_size + new_data3_size + new_data2_align;

  if (ftruncate (new_file, new_file_size))
    fatal ("Can't ftruncate(%s): errno %d\n", new_name, errno);

  new_base = mmap (0, new_file_size, PROT_READ | PROT_WRITE, MAP_SHARED,
		   new_file, 0);

  if (new_base == (caddr_t) -1)
    fatal ("Can't mmap(%s): errno %d\n", new_name, errno);

  new_file_h = (Elf32_Ehdr *) new_base;
  new_program_h = (Elf32_Phdr *) ((byte *) new_base + old_file_h->e_phoff);
  new_section_h = (Elf32_Shdr *) ((byte *) new_base +
				  old_file_h->e_shoff +
				  new_data2_size +
				  new_data2_align +
				  new_data3_size);

  /* Make our new file, program and section headers as copies of the
   * originals.
   */

  memcpy (new_file_h, old_file_h, old_file_h->e_ehsize);
  memcpy (new_program_h, old_program_h,
	  old_file_h->e_phnum * old_file_h->e_phentsize);

  /* Modify the e_shstrndx if necessary. */
  PATCH_INDEX (new_file_h->e_shstrndx);

  /* Fix up file header.  We'll add one section.  Section header is
   * further away now.
   */

  new_file_h->e_shoff += new_data2_size + new_data2_align + new_data3_size;
  new_file_h->e_shnum += 1 + (old_sdata_index ? 0 : 1);

#ifdef DEBUG
  fprintf (stderr, "Old section offset %x\n", old_file_h->e_shoff);
  fprintf (stderr, "Old section count %d\n", old_file_h->e_shnum);
  fprintf (stderr, "New section offset %x\n", new_file_h->e_shoff);
  fprintf (stderr, "New section count %d\n", new_file_h->e_shnum);
#endif

  /* Fix up a new program header.  Extend the writable data segment so
   * that the bss area is covered too. Find that segment by looking
   * for a segment that ends just before the .bss area.  Make sure
   * that no segments are above the new .data2.  Put a loop at the end
   * to adjust the offset and address of any segment that is above
   * data2, just in case we decide to allow this later.
   */

  for (n = new_file_h->e_phnum - 1; n >= 0; n--)
    {
      /* Compute maximum of all requirements for alignment of section.  */
      int alignment = (NEW_PROGRAM_H (n)).p_align;
      if ((OLD_SECTION_H (old_bss_index)).sh_addralign > alignment)
	alignment = OLD_SECTION_H (old_bss_index).sh_addralign;

      if ((OLD_SECTION_H (old_sbss_index)).sh_addralign > alignment)
	alignment = OLD_SECTION_H (old_sbss_index).sh_addralign;

      /* Supposedly this condition is okay for the SGI.  */
#if 0
      if (NEW_PROGRAM_H(n).p_vaddr + NEW_PROGRAM_H(n).p_filesz > old_bss_addr)
	fatal ("Program segment above .bss in %s\n", old_name, 0);
#endif

      if (NEW_PROGRAM_H(n).p_type == PT_LOAD
	  && (round_up ((NEW_PROGRAM_H (n)).p_vaddr
			+ (NEW_PROGRAM_H (n)).p_filesz,
			alignment)
	      == round_up (old_bss_addr, alignment)))
	break;
    }
  if (n < 0)
    fatal ("Couldn't find segment next to .bss in %s\n", old_name, 0);

  NEW_PROGRAM_H(n).p_filesz += new_data2_size + new_data2_align +
    new_data3_size;
  NEW_PROGRAM_H(n).p_memsz = NEW_PROGRAM_H(n).p_filesz;

#if 1 /* Maybe allow section after data2 - does this ever happen? */
  for (n = new_file_h->e_phnum - 1; n >= 0; n--)
    {
      if (NEW_PROGRAM_H(n).p_vaddr
	  && NEW_PROGRAM_H(n).p_vaddr >= new_data3_addr)
	NEW_PROGRAM_H(n).p_vaddr += new_data2_size - old_bss_size +
				    new_data3_size - old_sbss_size;

      if (NEW_PROGRAM_H(n).p_offset >= new_data3_offset)
	NEW_PROGRAM_H(n).p_offset += new_data2_size + new_data2_align +
	  new_data3_size;
    }
#endif

  /* Fix up section headers based on new .data2 section.  Any section
   * whose offset or virtual address is after the new .data2 section
   * gets its value adjusted.  .bss size becomes zero and new address
   * is set.  data2 section header gets added by copying the existing
   * .data header and modifying the offset, address and size.
   */
  for (old_data_index = 1; old_data_index < old_file_h->e_shnum;
       old_data_index++)
    if (!strcmp (old_section_names + OLD_SECTION_H(old_data_index).sh_name,
		 ".data"))
      break;
  if (old_data_index == old_file_h->e_shnum)
    fatal ("Can't find .data in %s.\n", old_name, 0);

  /* Walk through all section headers, insert the new data2 section right 
     before the new bss section. */
  for (n = 1, nn = 1; n < old_file_h->e_shnum; n++, nn++)
    {
      caddr_t src;

      if (n == old_sbss_index)

      /* If it is sbss section, insert the new data3 section before it. */
	{
	  /* Steal the data section header for this data3 section. */
	  if (!old_sdata_index)
	    {
	    memcpy (&NEW_SECTION_H(nn), &OLD_SECTION_H(old_data_index),
		    new_file_h->e_shentsize);
	  
	    NEW_SECTION_H(nn).sh_addr = new_data3_addr;
	    NEW_SECTION_H(nn).sh_offset = new_data3_offset;
	    NEW_SECTION_H(nn).sh_size = new_data3_size;
	    NEW_SECTION_H(nn).sh_flags = OLD_SECTION_H(n).sh_flags;
	    /* Use the sbss section's alignment. This will assure that the
	       new data3 section always be placed in the same spot as the old
	       sbss section by any other application. */
	    NEW_SECTION_H(nn).sh_addralign = OLD_SECTION_H(n).sh_addralign;

	    /* Now copy over what we have in the memory now. */
	    memcpy (NEW_SECTION_H(nn).sh_offset + new_base, 
		    (caddr_t) OLD_SECTION_H(n).sh_addr, 
		    new_data3_size);
		  /* the new .data2 section should also come before the
		   * new .sbss section */
	    nn += 2;
	    }
	  else
	    {
	    /* We always have a .sdata section: append the contents of the
	     * old .sbss section.
	     */
	    memcpy (new_data3_offset + new_base, 
		    (caddr_t) OLD_SECTION_H(n).sh_addr, 
		    new_data3_size);
	    nn ++;
	    }
	}
      else if (n == old_bss_index)
      
      /* If it is bss section, insert the new data2 section before it. */
	{
	  Elf32_Word tmp_align;
	  Elf32_Addr tmp_addr;

	  tmp_align = OLD_SECTION_H(n).sh_addralign;
	  tmp_addr = OLD_SECTION_H(n).sh_addr;

	  nn -= 2;
	  /* Steal the data section header for this data2 section. */
	  memcpy (&NEW_SECTION_H(nn), &OLD_SECTION_H(old_data_index),
		  new_file_h->e_shentsize);
	  
	  NEW_SECTION_H(nn).sh_addr = new_data2_addr;
	  NEW_SECTION_H(nn).sh_offset = new_data2_offset;
	  NEW_SECTION_H(nn).sh_size = new_data2_size;
	  /* Use the bss section's alignment. This will assure that the
	     new data2 section always be placed in the same spot as the old
	     bss section by any other application. */
	  NEW_SECTION_H(nn).sh_addralign = tmp_align;

	  /* Now copy over what we have in the memory now. */
	  memcpy (NEW_SECTION_H(nn).sh_offset + new_base, 
		  (caddr_t) tmp_addr, new_data2_size);
	  nn += 2;
	}
      
      memcpy (&NEW_SECTION_H(nn), &OLD_SECTION_H(n), 
	      old_file_h->e_shentsize);
      
      if (old_sdata_index && n == old_sdata_index)
	/* The old .sdata section has now a new size */
	NEW_SECTION_H(nn).sh_size = new_sdata_size;

      /* The new bss section's size is zero, and its file offset and virtual
	 address should be off by NEW_DATA2_SIZE. */
      if (n == old_sbss_index)
	{
	  /* NN should be `old_sbss_index + 2' at this point. */
	  NEW_SECTION_H(nn).sh_offset += new_data2_size + new_data2_align +
	    new_data3_size;
	  NEW_SECTION_H(nn).sh_addr += new_data2_size + new_data2_align +
	    new_data3_size;
	  /* Let the new bss section address alignment be the same as the
	     section address alignment followed the old bss section, so 
	     this section will be placed in exactly the same place. */
	  NEW_SECTION_H(nn).sh_addralign =
	    OLD_SECTION_H(nn + (old_sdata_index ? 1 : 0)).sh_addralign;
	  NEW_SECTION_H(nn).sh_size = 0;
	}
      else if (n == old_bss_index)
	{
	  /* NN should be `old_bss_index + 2' at this point. */
	  NEW_SECTION_H(nn).sh_offset += new_data2_size + new_data2_align +
	    new_data3_size - old_bss_padding;
	  NEW_SECTION_H(nn).sh_addr += new_data2_size;
	  /* Let the new bss section address alignment be the same as the
	     section address alignment followed the old bss section, so 
	     this section will be placed in exactly the same place. */
	  NEW_SECTION_H(nn).sh_addralign =
	    OLD_SECTION_H((nn - (old_sdata_index ? 0 : 1))).sh_addralign;
	  NEW_SECTION_H(nn).sh_size = 0;
	}
      /* Any section that was original placed AFTER the bss section should now
	 be off by NEW_DATA2_SIZE. */
      else if (NEW_SECTION_H(nn).sh_offset >= new_data3_offset)
	NEW_SECTION_H(nn).sh_offset += new_data2_size +
				       new_data2_align +
				       new_data3_size -
				       old_bss_padding;
      
      /* If any section hdr refers to the section after the new .data
	 section, make it refer to next one because we have inserted 
	 a new section in between. */
      
      PATCH_INDEX(NEW_SECTION_H(nn).sh_link);
      PATCH_INDEX(NEW_SECTION_H(nn).sh_info);
      
      /* Now, start to copy the content of sections. */
      if (NEW_SECTION_H(nn).sh_type == SHT_NULL
	  || NEW_SECTION_H(nn).sh_type == SHT_NOBITS)
	continue;
      
      /* Write out the sections. .data, .data1 and .sdata get copied from
       * the current process instead of the old file.
       */
      if (!strcmp (old_section_names + OLD_SECTION_H(n).sh_name, ".data") ||
	  !strcmp (old_section_names + OLD_SECTION_H(n).sh_name, ".data1") ||
	  (old_sdata_index && (n == old_sdata_index)))
	src = (caddr_t) OLD_SECTION_H(n).sh_addr;
      else
	src = old_base + OLD_SECTION_H(n).sh_offset;
      
      memcpy (NEW_SECTION_H(nn).sh_offset + new_base, src,
	      ((n == old_sdata_index) ?
	       old_sdata_size :
	       NEW_SECTION_H(nn).sh_size));

      /* If it is the symbol table, its st_shndx field needs to be patched. */
      if (NEW_SECTION_H(nn).sh_type == SHT_SYMTAB
	  || NEW_SECTION_H(nn).sh_type == SHT_DYNSYM)
	{
	  Elf32_Shdr *spt = &NEW_SECTION_H(nn);
	  unsigned int num = spt->sh_size / spt->sh_entsize;
	  Elf32_Sym * sym = (Elf32_Sym *) (NEW_SECTION_H(nn).sh_offset + 
					   new_base);
	  for (; num--; sym++)
	    {
	      if ((sym->st_shndx == SHN_UNDEF)
		  || (sym->st_shndx == SHN_ABS)
		  || (sym->st_shndx == SHN_COMMON))
		continue;
	
	      PATCH_INDEX(sym->st_shndx);
	    }
	}
    }

  /* Close the files and make the new file executable */

  if (close (old_file))
    fatal ("Can't close(%s): errno %d\n", old_name, errno);

  if (close (new_file))
    fatal ("Can't close(%s): errno %d\n", new_name, errno);

  if (stat (new_name, &stat_buf) == -1)
    fatal ("Can't stat(%s): errno %d\n", new_name, errno);

  n = umask (777);
  umask (n);
  stat_buf.st_mode |= 0111 & ~n;
  if (chmod (new_name, stat_buf.st_mode) == -1)
    fatal ("Can't chmod(%s): errno %d\n", new_name, errno);
}
