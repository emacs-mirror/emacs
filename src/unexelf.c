/* Copyright (C) 1985, 1986, 1987, 1988, 1990, 1992
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

 * This is an example of how the section headers are changed.  "Addr"
 * is a process virtual address.  "Offset" is a file offset.

raid:/nfs/raid/src/dist-18.56/src> dump -h temacs

temacs:

           **** SECTION HEADER TABLE ****
[No]    Type    Flags   Addr         Offset       Size          Name
        Link    Info    Adralgn      Entsize

[1]     1       2       0x80480d4    0xd4         0x13          .interp
        0       0       0x1          0            

[2]     5       2       0x80480e8    0xe8         0x388         .hash
        3       0       0x4          0x4          

[3]     11      2       0x8048470    0x470        0x7f0         .dynsym
        4       1       0x4          0x10         

[4]     3       2       0x8048c60    0xc60        0x3ad         .dynstr
        0       0       0x1          0            

[5]     9       2       0x8049010    0x1010       0x338         .rel.plt
        3       7       0x4          0x8          

[6]     1       6       0x8049348    0x1348       0x3           .init
        0       0       0x4          0            

[7]     1       6       0x804934c    0x134c       0x680         .plt
        0       0       0x4          0x4          

[8]     1       6       0x80499cc    0x19cc       0x3c56f       .text
        0       0       0x4          0            

[9]     1       6       0x8085f3c    0x3df3c      0x3           .fini
        0       0       0x4          0            

[10]    1       2       0x8085f40    0x3df40      0x69c         .rodata
        0       0       0x4          0            

[11]    1       2       0x80865dc    0x3e5dc      0xd51         .rodata1
        0       0       0x4          0            

[12]    1       3       0x8088330    0x3f330      0x20afc       .data
        0       0       0x4          0            

[13]    1       3       0x80a8e2c    0x5fe2c      0x89d         .data1
        0       0       0x4          0            

[14]    1       3       0x80a96cc    0x606cc      0x1a8         .got
        0       0       0x4          0x4          

[15]    6       3       0x80a9874    0x60874      0x80          .dynamic
        4       0       0x4          0x8          

[16]    8       3       0x80a98f4    0x608f4      0x449c        .bss
        0       0       0x4          0            

[17]    2       0       0            0x608f4      0x9b90        .symtab
        18      371     0x4          0x10         

[18]    3       0       0            0x6a484      0x8526        .strtab
        0       0       0x1          0            

[19]    3       0       0            0x729aa      0x93          .shstrtab
        0       0       0x1          0            

[20]    1       0       0            0x72a3d      0x68b7        .comment
        0       0       0x1          0            

raid:/nfs/raid/src/dist-18.56/src> dump -h xemacs

xemacs:

           **** SECTION HEADER TABLE ****
[No]    Type    Flags   Addr         Offset       Size          Name
        Link    Info    Adralgn      Entsize

[1]     1       2       0x80480d4    0xd4         0x13          .interp
        0       0       0x1          0            

[2]     5       2       0x80480e8    0xe8         0x388         .hash
        3       0       0x4          0x4          

[3]     11      2       0x8048470    0x470        0x7f0         .dynsym
        4       1       0x4          0x10         

[4]     3       2       0x8048c60    0xc60        0x3ad         .dynstr
        0       0       0x1          0            

[5]     9       2       0x8049010    0x1010       0x338         .rel.plt
        3       7       0x4          0x8          

[6]     1       6       0x8049348    0x1348       0x3           .init
        0       0       0x4          0            

[7]     1       6       0x804934c    0x134c       0x680         .plt
        0       0       0x4          0x4          

[8]     1       6       0x80499cc    0x19cc       0x3c56f       .text
        0       0       0x4          0            

[9]     1       6       0x8085f3c    0x3df3c      0x3           .fini
        0       0       0x4          0            

[10]    1       2       0x8085f40    0x3df40      0x69c         .rodata
        0       0       0x4          0            

[11]    1       2       0x80865dc    0x3e5dc      0xd51         .rodata1
        0       0       0x4          0            

[12]    1       3       0x8088330    0x3f330      0x20afc       .data
        0       0       0x4          0            

[13]    1       3       0x80a8e2c    0x5fe2c      0x89d         .data1
        0       0       0x4          0            

[14]    1       3       0x80a96cc    0x606cc      0x1a8         .got
        0       0       0x4          0x4          

[15]    6       3       0x80a9874    0x60874      0x80          .dynamic
        4       0       0x4          0x8          

[16]    8       3       0x80c6800    0x7d800      0             .bss
        0       0       0x4          0            

[17]    2       0       0            0x7d800      0x9b90        .symtab
        18      371     0x4          0x10         

[18]    3       0       0            0x87390      0x8526        .strtab
        0       0       0x1          0            

[19]    3       0       0            0x8f8b6      0x93          .shstrtab
        0       0       0x1          0            

[20]    1       0       0            0x8f949      0x68b7        .comment
        0       0       0x1          0            

[21]    1       3       0x80a98f4    0x608f4      0x1cf0c       .data
        0       0       0x4          0            

 * This is an example of how the file header is changed.  "Shoff" is
 * the section header offset within the file.  Since that table is
 * after the new .data section, it is moved.  "Shnum" is the number of
 * sections, which we increment.
 *
 * "Phoff" is the file offset to the program header.  "Phentsize" and
 * "Shentsz" are the program and section header entries sizes respectively.
 * These can be larger than the apparent struct sizes.

raid:/nfs/raid/src/dist-18.56/src> dump -f temacs

temacs:

                    **** ELF HEADER ****
Class        Data       Type         Machine     Version
Entry        Phoff      Shoff        Flags       Ehsize
Phentsize    Phnum      Shentsz      Shnum       Shstrndx

1            1          2            3           1
0x80499cc    0x34       0x792f4      0           0x34
0x20         5          0x28         21          19

raid:/nfs/raid/src/dist-18.56/src> dump -f xemacs

xemacs:

                    **** ELF HEADER ****
Class        Data       Type         Machine     Version
Entry        Phoff      Shoff        Flags       Ehsize
Phentsize    Phnum      Shentsz      Shnum       Shstrndx

1            1          2            3           1
0x80499cc    0x34       0x96200      0           0x34
0x20         5          0x28         22          19

 * These are the program headers.  "Offset" is the file offset to the
 * segment.  "Vaddr" is the memory load address.  "Filesz" is the
 * segment size as it appears in the file, and "Memsz" is the size in
 * memory.  Below, the third segment is the code and the fourth is the
 * data: the difference between Filesz and Memsz is .bss

raid:/nfs/raid/src/dist-18.56/src> dump -o temacs

temacs:
 ***** PROGRAM EXECUTION HEADER *****
Type        Offset      Vaddr       Paddr
Filesz      Memsz       Flags       Align

6           0x34        0x8048034   0           
0xa0        0xa0        5           0           

3           0xd4        0           0           
0x13        0           4           0           

1           0x34        0x8048034   0           
0x3f2f9     0x3f2f9     5           0x1000      

1           0x3f330     0x8088330   0           
0x215c4     0x25a60     7           0x1000      

2           0x60874     0x80a9874   0           
0x80        0           7           0           

raid:/nfs/raid/src/dist-18.56/src> dump -o xemacs

xemacs:
 ***** PROGRAM EXECUTION HEADER *****
Type        Offset      Vaddr       Paddr
Filesz      Memsz       Flags       Align

6           0x34        0x8048034   0           
0xa0        0xa0        5           0           

3           0xd4        0           0           
0x13        0           4           0           

1           0x34        0x8048034   0           
0x3f2f9     0x3f2f9     5           0x1000      

1           0x3f330     0x8088330   0           
0x3e4d0     0x3e4d0     7           0x1000      

2           0x60874     0x80a9874   0           
0x80        0           7           0           


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
 *
 * The above example now should look like:

           **** SECTION HEADER TABLE ****
[No]    Type    Flags   Addr         Offset       Size          Name
        Link    Info    Adralgn      Entsize

[1]     1       2       0x80480d4    0xd4         0x13          .interp
        0       0       0x1          0            

[2]     5       2       0x80480e8    0xe8         0x388         .hash
        3       0       0x4          0x4          

[3]     11      2       0x8048470    0x470        0x7f0         .dynsym
        4       1       0x4          0x10         

[4]     3       2       0x8048c60    0xc60        0x3ad         .dynstr
        0       0       0x1          0            

[5]     9       2       0x8049010    0x1010       0x338         .rel.plt
        3       7       0x4          0x8          

[6]     1       6       0x8049348    0x1348       0x3           .init
        0       0       0x4          0            

[7]     1       6       0x804934c    0x134c       0x680         .plt
        0       0       0x4          0x4          

[8]     1       6       0x80499cc    0x19cc       0x3c56f       .text
        0       0       0x4          0            

[9]     1       6       0x8085f3c    0x3df3c      0x3           .fini
        0       0       0x4          0            

[10]    1       2       0x8085f40    0x3df40      0x69c         .rodata
        0       0       0x4          0            

[11]    1       2       0x80865dc    0x3e5dc      0xd51         .rodata1
        0       0       0x4          0            

[12]    1       3       0x8088330    0x3f330      0x20afc       .data
        0       0       0x4          0            

[13]    1       3       0x80a8e2c    0x5fe2c      0x89d         .data1
        0       0       0x4          0            

[14]    1       3       0x80a96cc    0x606cc      0x1a8         .got
        0       0       0x4          0x4          

[15]    6       3       0x80a9874    0x60874      0x80          .dynamic
        4       0       0x4          0x8          

[16]    1       3       0x80a98f4    0x608f4      0x1cf0c       .data
        0       0       0x4          0            

[17]    8       3       0x80c6800    0x7d800      0             .bss
        0       0       0x4          0            

[18]    2       0       0            0x7d800      0x9b90        .symtab
        19      371     0x4          0x10         

[19]    3       0       0            0x87390      0x8526        .strtab
        0       0       0x1          0            

[20]    3       0       0            0x8f8b6      0x93          .shstrtab
        0       0       0x1          0            

[21]    1       0       0            0x8f949      0x68b7        .comment
        0       0       0x1          0            

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
#define fatal(a, b, c) fprintf (stderr, a, b, c), exit (1)
#else
#include "config.h"
extern void fatal (char *, ...);
#endif

#ifndef ELF_BSS_SECTION_NAME
#define ELF_BSS_SECTION_NAME ".bss"
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
	 if ((int) (n) >= old_bss_index) \
	   (n)++; } while (0)
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
  Elf32_Word old_bss_size, new_data2_size;
  Elf32_Off  new_data2_offset;
  Elf32_Addr new_data2_addr;

  int n, nn, old_bss_index, old_data_index, new_data2_index;
  struct stat stat_buf;

  /* Open the old file & map it into the address space. */

  old_file = open (old_name, O_RDONLY);

  if (old_file < 0)
    fatal ("Can't open %s for reading: errno %d\n", old_name, errno);

  if (fstat (old_file, &stat_buf) == -1)
    fatal ("Can't fstat (%s): errno %d\n", old_name, errno);

  old_base = mmap (0, stat_buf.st_size, PROT_READ, MAP_SHARED, old_file, 0);

  if (old_base == (caddr_t) -1)
    fatal ("Can't mmap (%s): errno %d\n", old_name, errno);

#ifdef DEBUG
  fprintf (stderr, "mmap (%s, %x) -> %x\n", old_name, stat_buf.st_size,
	   old_base);
#endif

  /* Get pointers to headers & section names */

  old_file_h = (Elf32_Ehdr *) old_base;
  old_program_h = (Elf32_Phdr *) ((byte *) old_base + old_file_h->e_phoff);
  old_section_h = (Elf32_Shdr *) ((byte *) old_base + old_file_h->e_shoff);
  old_section_names = (char *) old_base
    + OLD_SECTION_H (old_file_h->e_shstrndx).sh_offset;

  /* Find the old .bss section.  Figure out parameters of the new
   * data2 and bss sections.
   */

  for (old_bss_index = 1; old_bss_index < (int) old_file_h->e_shnum;
       old_bss_index++)
    {
#ifdef DEBUG
      fprintf (stderr, "Looking for .bss - found %s\n",
	       old_section_names + OLD_SECTION_H (old_bss_index).sh_name);
#endif
      if (!strcmp (old_section_names + OLD_SECTION_H (old_bss_index).sh_name,
		   ELF_BSS_SECTION_NAME))
	break;
    }
  if (old_bss_index == old_file_h->e_shnum)
    fatal ("Can't find .bss in %s.\n", old_name, 0);

  old_bss_addr = OLD_SECTION_H (old_bss_index).sh_addr;
  old_bss_size = OLD_SECTION_H (old_bss_index).sh_size;
#if defined(emacs) || !defined(DEBUG)
  new_bss_addr = (Elf32_Addr) sbrk (0);
#else
  new_bss_addr = old_bss_addr + old_bss_size + 0x1234;
#endif
  new_data2_addr = old_bss_addr;
  new_data2_size = new_bss_addr - old_bss_addr;
  new_data2_offset = OLD_SECTION_H (old_bss_index).sh_offset;

#ifdef DEBUG
  fprintf (stderr, "old_bss_index %d\n", old_bss_index);
  fprintf (stderr, "old_bss_addr %x\n", old_bss_addr);
  fprintf (stderr, "old_bss_size %x\n", old_bss_size);
  fprintf (stderr, "new_bss_addr %x\n", new_bss_addr);
  fprintf (stderr, "new_data2_addr %x\n", new_data2_addr);
  fprintf (stderr, "new_data2_size %x\n", new_data2_size);
  fprintf (stderr, "new_data2_offset %x\n", new_data2_offset);
#endif

  if ((unsigned) new_bss_addr < (unsigned) old_bss_addr + old_bss_size)
    fatal (".bss shrank when undumping???\n", 0, 0);

  /* Set the output file to the right size and mmap it.  Set
   * pointers to various interesting objects.  stat_buf still has
   * old_file data.
   */

  new_file = open (new_name, O_RDWR | O_CREAT, 0666);
  if (new_file < 0)
    fatal ("Can't creat (%s): errno %d\n", new_name, errno);

  new_file_size = stat_buf.st_size + old_file_h->e_shentsize + new_data2_size;

  if (ftruncate (new_file, new_file_size))
    fatal ("Can't ftruncate (%s): errno %d\n", new_name, errno);

  new_base = mmap (0, new_file_size, PROT_READ | PROT_WRITE, MAP_SHARED,
		   new_file, 0);

  if (new_base == (caddr_t) -1)
    fatal ("Can't mmap (%s): errno %d\n", new_name, errno);

  new_file_h = (Elf32_Ehdr *) new_base;
  new_program_h = (Elf32_Phdr *) ((byte *) new_base + old_file_h->e_phoff);
  new_section_h = (Elf32_Shdr *)
    ((byte *) new_base + old_file_h->e_shoff + new_data2_size);

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

  new_file_h->e_shoff += new_data2_size;
  new_file_h->e_shnum += 1;

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

      if (NEW_PROGRAM_H (n).p_vaddr + NEW_PROGRAM_H (n).p_filesz > old_bss_addr)
	fatal ("Program segment above .bss in %s\n", old_name, 0);

      if (NEW_PROGRAM_H (n).p_type == PT_LOAD
	  && (round_up ((NEW_PROGRAM_H (n)).p_vaddr
			+ (NEW_PROGRAM_H (n)).p_filesz,
			alignment)
	      == round_up (old_bss_addr, alignment)))
	break;
    }
  if (n < 0)
    fatal ("Couldn't find segment next to .bss in %s\n", old_name, 0);

  NEW_PROGRAM_H (n).p_filesz += new_data2_size;
  NEW_PROGRAM_H (n).p_memsz = NEW_PROGRAM_H (n).p_filesz;

#if 0 /* Maybe allow section after data2 - does this ever happen? */
  for (n = new_file_h->e_phnum - 1; n >= 0; n--)
    {
      if (NEW_PROGRAM_H (n).p_vaddr
	  && NEW_PROGRAM_H (n).p_vaddr >= new_data2_addr)
	NEW_PROGRAM_H (n).p_vaddr += new_data2_size - old_bss_size;

      if (NEW_PROGRAM_H (n).p_offset >= new_data2_offset)
	NEW_PROGRAM_H (n).p_offset += new_data2_size;
    }
#endif

  /* Fix up section headers based on new .data2 section.  Any section
   * whose offset or virtual address is after the new .data2 section
   * gets its value adjusted.  .bss size becomes zero and new address
   * is set.  data2 section header gets added by copying the existing
   * .data header and modifying the offset, address and size.
   */
  for (old_data_index = 1; old_data_index < (int) old_file_h->e_shnum;
       old_data_index++)
    if (!strcmp (old_section_names + OLD_SECTION_H (old_data_index).sh_name,
		 ".data"))
      break;
  if (old_data_index == old_file_h->e_shnum)
    fatal ("Can't find .data in %s.\n", old_name, 0);

  /* Walk through all section headers, insert the new data2 section right 
     before the new bss section. */
  for (n = 1, nn = 1; n < (int) old_file_h->e_shnum; n++, nn++)
    {
      caddr_t src;
      /* If it is bss section, insert the new data2 section before it. */
      if (n == old_bss_index)
	{
	  /* Steal the data section header for this data2 section. */
	  memcpy (&NEW_SECTION_H (nn), &OLD_SECTION_H (old_data_index),
		  new_file_h->e_shentsize);
	  
	  NEW_SECTION_H (nn).sh_addr = new_data2_addr;
	  NEW_SECTION_H (nn).sh_offset = new_data2_offset;
	  NEW_SECTION_H (nn).sh_size = new_data2_size;
	  /* Use the bss section's alignment. This will assure that the
	     new data2 section always be placed in the same spot as the old
	     bss section by any other application. */
	  NEW_SECTION_H (nn).sh_addralign = OLD_SECTION_H (n).sh_addralign;

	  /* Now copy over what we have in the memory now. */
	  memcpy (NEW_SECTION_H (nn).sh_offset + new_base, 
		  (caddr_t) OLD_SECTION_H (n).sh_addr, 
		  new_data2_size);
	  nn++;
	}
      
      memcpy (&NEW_SECTION_H (nn), &OLD_SECTION_H (n), 
	      old_file_h->e_shentsize);
      
      /* The new bss section's size is zero, and its file offset and virtual
	 address should be off by NEW_DATA2_SIZE. */
      if (n == old_bss_index)
	{
	  /* NN should be `old_bss_index + 1' at this point. */
	  NEW_SECTION_H (nn).sh_offset += new_data2_size;
	  NEW_SECTION_H (nn).sh_addr += new_data2_size;
	  /* Let the new bss section address alignment be the same as the
	     section address alignment followed the old bss section, so 
	     this section will be placed in exactly the same place. */
	  NEW_SECTION_H (nn).sh_addralign = OLD_SECTION_H (nn).sh_addralign;
	  NEW_SECTION_H (nn).sh_size = 0;
	}
      else
	{
	  /* Any section that was original placed AFTER the bss
	     section should now be off by NEW_DATA2_SIZE. */
	  if (NEW_SECTION_H (nn).sh_offset >= new_data2_offset)
	    NEW_SECTION_H (nn).sh_offset += new_data2_size;
	  /* Any section that was originally placed after the section
	     header table should now be off by the size of one section
	     header table entry.  */
	  if (NEW_SECTION_H (nn).sh_offset > new_file_h->e_shoff)
	    NEW_SECTION_H (nn).sh_offset += new_file_h->e_shentsize;
	}

      /* If any section hdr refers to the section after the new .data
	 section, make it refer to next one because we have inserted 
	 a new section in between.  */
      
      PATCH_INDEX (NEW_SECTION_H (nn).sh_link);
      /* For symbol tables, info is a symbol table index,
	 so don't change it.  */
      if (NEW_SECTION_H (nn).sh_type != SHT_SYMTAB
	  && NEW_SECTION_H (nn).sh_type != SHT_DYNSYM)
	PATCH_INDEX (NEW_SECTION_H (nn).sh_info);

      /* Now, start to copy the content of sections.  */
      if (NEW_SECTION_H (nn).sh_type == SHT_NULL
	  || NEW_SECTION_H (nn).sh_type == SHT_NOBITS)
	continue;
      
      /* Write out the sections. .data and .data1 (and data2, called
	 ".data" in the strings table) get copied from the current process
	 instead of the old file.  */
      if (!strcmp (old_section_names + NEW_SECTION_H (n).sh_name, ".data")
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".data1"))
	src = (caddr_t) OLD_SECTION_H (n).sh_addr;
      else
	src = old_base + OLD_SECTION_H (n).sh_offset;
      
      memcpy (NEW_SECTION_H (nn).sh_offset + new_base, src,
	      NEW_SECTION_H (nn).sh_size);

      /* If it is the symbol table, its st_shndx field needs to be patched.  */
      if (NEW_SECTION_H (nn).sh_type == SHT_SYMTAB
	  || NEW_SECTION_H (nn).sh_type == SHT_DYNSYM)
	{
	  Elf32_Shdr *spt = &NEW_SECTION_H (nn);
	  unsigned int num = spt->sh_size / spt->sh_entsize;
	  Elf32_Sym * sym = (Elf32_Sym *) (NEW_SECTION_H (nn).sh_offset + 
					   new_base);
	  for (; num--; sym++)
	    {
	      if ((sym->st_shndx == SHN_UNDEF)
		  || (sym->st_shndx == SHN_ABS)
		  || (sym->st_shndx == SHN_COMMON))
		continue;
	
	      PATCH_INDEX (sym->st_shndx);
	    }
	}
    }

  /* Update the symbol values of _edata and _end.  */
  for (n = new_file_h->e_shnum - 1; n; n--)
    {
      byte *symnames;
      Elf32_Sym *symp, *symendp;

      if (NEW_SECTION_H (n).sh_type != SHT_DYNSYM
	  && NEW_SECTION_H (n).sh_type != SHT_SYMTAB)
	continue;

      symnames = ((byte *) new_base
		  + NEW_SECTION_H (NEW_SECTION_H (n).sh_link).sh_offset);
      symp = (Elf32_Sym *) (NEW_SECTION_H (n).sh_offset + new_base);
      symendp = (Elf32_Sym *) ((byte *)symp + NEW_SECTION_H (n).sh_size);

      for (; symp < symendp; symp ++)
	if (strcmp ((char *) (symnames + symp->st_name), "_end") == 0
	    || strcmp ((char *) (symnames + symp->st_name), "_edata") == 0)
	  memcpy (&symp->st_value, &new_bss_addr, sizeof (new_bss_addr));
    }

  /* Close the files and make the new file executable.  */

  if (close (old_file))
    fatal ("Can't close (%s): errno %d\n", old_name, errno);

  if (close (new_file))
    fatal ("Can't close (%s): errno %d\n", new_name, errno);

  if (stat (new_name, &stat_buf) == -1)
    fatal ("Can't stat (%s): errno %d\n", new_name, errno);

  n = umask (777);
  umask (n);
  stat_buf.st_mode |= 0111 & ~n;
  if (chmod (new_name, stat_buf.st_mode) == -1)
    fatal ("Can't chmod (%s): errno %d\n", new_name, errno);
}
