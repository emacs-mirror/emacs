/* Program execution for Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#include <config.h>

#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <sys/ptrace.h>
#include <sys/param.h>
#include <sys/mman.h>

#include "exec.h"

#if defined __mips__ && !defined MIPS_NABI
#include "mipsfpu.h"
#endif /* defined __mips__ && !defined MIPS_NABI */




/* Define replacements for required string functions.  */

#if !defined HAVE_STPCPY || !defined HAVE_DECL_STPCPY

/* Copy SRC to DEST, returning the address of the terminating '\0' in
   DEST.  */

static char *
rpl_stpcpy (char *dest, const char *src)
{
  register char *d;
  register const char *s;

  d = dest;
  s = src;

  do
    *d++ = *s;
  while (*s++ != '\0');

  return d - 1;
}

#define stpcpy rpl_stpcpy
#endif /* !defined HAVE_STPCPY || !defined HAVE_DECL_STPCPY */



/* Executable reading functions.
   These functions extract information from an executable that is
   about to be loaded.

   `exec_0' takes the name of the program, determines whether or not
   its format is correct, and if so, returns the list of actions that
   the loader should perform.

   The actions include:

     - Making the stack executable, if PT_GNU_STACK.
     - Mapping PT_LOAD sections into the executable with the correct
       memory protection.
     - On MIPS, setting the floating point register size.
     - Transferring control to the interpreter or executable.  */


/* Check whether or not FD starts with a #!, and return the executable
   to load if it does.  Value is NAME if no interpreter character was
   found, or the interpreter otherwise.  Value is NULL upon an IO
   error.

   If an additional command line argument is specified, place it in
   *EXTRA.  */

static const char *
check_interpreter (const char *name, int fd, const char **extra)
{
  static char buffer[PATH_MAX], *start;
  char first[2], *end, *ws;
  ssize_t rc;

  /* Read the first character.  */
  rc = read (fd, &first, 2);

  if (rc != 2)
    goto fail;

  if (first[0] != '#' || first[1] != '!')
    goto nomatch;

  rc = read (fd, buffer, PATH_MAX);

  if (rc < 0)
    goto fail;

  /* Strip leading whitespace.  */
  start = buffer;
  while (start < buffer + rc && (*start == ' ' || *start == '\t'))
    ++start;

  /* Look for a newline character.  */
  end = memchr (start, '\n', buffer + rc - start);

  if (!end)
    goto fail;

  /* The string containing the interpreter is now in start.  NULL
     terminate it.  */
  *end = '\0';

  /* Now look for any whitespace characters.  */
  for (ws = start; *ws && *ws != ' ' && *ws != '\t'; ws++)
    continue;

  /* If there's no whitespace, return the entire start.  */

  if (!*ws)
    {
      if (lseek (fd, 0, SEEK_SET))
	goto fail;

      return start;
    }

  /* Otherwise, split the string at the whitespace and return the
     additional argument.  */
  *ws = '\0';

  if (lseek (fd, 0, SEEK_SET))
    goto fail;

  *extra = ws + 1;
  return start;

 nomatch:
  /* There's no interpreter.  */
  if (lseek (fd, 0, SEEK_SET))
    goto fail;

  return name;

 fail:
  errno = ENOEXEC;
  return NULL;
}

/* Static area used to store data placed on the loader's stack.  */
static char loader_area[65536];

/* Number of bytes used in that area.  */
static int loader_area_used;



/* Structure definitions for commands placed in the loader area.
   Arrange these so that each member is naturally aligned.  */

struct exec_open_command
{
  /* Word identifying the type of this command.  */
  USER_WORD command;

  /* NULL-terminated file name follows, padded to the size of a user
     word.  */
};

struct exec_map_command
{
  /* Word identifying the type of this command.  */
  USER_WORD command;

  /* Where the file will be mapped.  */
  USER_WORD vm_address;

  /* Offset into the file to map from.  */
  USER_WORD file_offset;

  /* Memory protection for mprotect.  */
  USER_WORD protection;

  /* Number of bytes to be mapped.  */
  USER_WORD length;

  /* Flags for mmap.  */
  USER_WORD flags;

  /* Number of bytes to clear at the end of this mapping.  */
  USER_WORD clear;
};

struct exec_jump_command
{
  /* Word identifying the type of this command.  */
  USER_WORD command;

  /* Address to jump to.  */
  USER_WORD entry;

  /* The value of AT_ENTRY inside the aux vector.  */
  USER_WORD at_entry;

  /* The value of AT_PHENT inside the aux vector.  */
  USER_WORD at_phent;

  /* The value of AT_PHNUM inside the aux vector.  */
  USER_WORD at_phnum;

  /* The value of AT_PHDR inside the aux vector.  */
  USER_WORD at_phdr;

  /* The value of AT_BASE inside the aux vector.  */
  USER_WORD at_base;

#if defined __mips__ && !defined __LP64__
  /* The FPU mode to apply.  Not used when !MIPS_NABI.  */
  USER_WORD fpu_mode;
#endif /* defined __mips__ && !defined __LP64__ */
};



/* Write a command to open the file NAME to the loader area.
   If ALTERNATE is true, then use the command code 16 instead
   of 0.  Value is 1 upon failure, else 0.  */

static int
write_open_command (const char *name, bool alternate)
{
  struct exec_open_command command;
  size_t size;

  /* First, write the command to open NAME.  This is followed by NAME
     itself, padded to sizeof (USER_WORD) bytes.  */

  command.command = alternate ? 16 : 0;
  if (sizeof loader_area - loader_area_used < sizeof command)
    return 1;
  memcpy (loader_area + loader_area_used, &command, sizeof command);
  loader_area_used += sizeof command;

  /* Calculate the length of NAME.  */
  size = strlen (name) + 1;

  /* Round it up.  */
  size = ((size + (sizeof (USER_WORD) - 1))
	  & ~(sizeof (USER_WORD) - 1));

  if (sizeof loader_area - loader_area_used < size)
    return 1;

  /* Now copy name to the loader area, filling the padding with NULL
     bytes.  */
  strncpy (loader_area + loader_area_used, name, size);

  /* Increase loader_area_used.  */
  loader_area_used += size;
  return 0;
}

/* Write the commands necessary to map the executable file into memory
   for the given PT_LOAD program HEADER.  Value is 1 upon failure,
   else 0.  If USE_ALTERNATE, use the command code 17 instead of
   1.

   Apply the given OFFSET to virtual addresses that will be mapped.  */

static int
write_load_command (program_header *header, bool use_alternate,
		    USER_WORD offset)
{
  struct exec_map_command command;
  struct exec_map_command command1;
  USER_WORD start, end;
  bool need_command1;
#ifndef PAGE_MASK
  static long pagesize;
#endif /* !PAGE_MASK */

  /* First, write the commands necessary to map the specified segment
     itself.

     This is the area between header->p_vaddr and header->p_filesz,
     rounded up to the page size.  */

#ifndef PAGE_MASK
  /* This system doesn't define a fixed page size.  */

#ifdef HAVE_GETPAGESIZE
  if (!pagesize)
    pagesize = getpagesize ();
#else /* !HAVE_GETPAGESIZE */
  if (!pagesize)
    pagesize = sysconf (_SC_PAGESIZE);
#endif /* !HAVE_GETPAGESIZE */

#define PAGE_MASK (~(pagesize - 1))
#define PAGE_SIZE (pagesize)
#endif /* !PAGE_MASK */

  start = header->p_vaddr & PAGE_MASK;
  end = ((header->p_vaddr + header->p_filesz
	  + PAGE_SIZE)
	 & PAGE_MASK);

  command.command = use_alternate ? 17 : 1;
  command.vm_address = start;
  command.file_offset = header->p_offset & PAGE_MASK;
  command.protection = 0;
  command.length = end - start;
  command.clear = 0;
  command.flags = MAP_PRIVATE | MAP_FIXED;

  /* Apply the memory protection specified in the header.  */

  if (header->p_flags & 4) /* PF_R */
    command.protection |= PROT_READ;

  if (header->p_flags & 2) /* PF_W */
    command.protection |= PROT_WRITE;

  if (header->p_flags & 1) /* PF_X */
    command.protection |= PROT_EXEC;

  /* Next, write any command necessary to map pages in the area
     between p_filesz and p_memsz.  */
  need_command1 = false;

  if (header->p_memsz > header->p_filesz)
    {
      /* If there are bytes after end which need to be initialized, do
	 that now.  */
      command.clear = end - header->p_vaddr - header->p_filesz;
      start = end;
      end = header->p_vaddr + header->p_memsz + PAGE_SIZE;
      end &= PAGE_MASK;

      if (end > start)
	{
	  command1.command = 4;
	  command1.vm_address = start;
	  command1.file_offset = 0;
	  command1.length = end - start;
	  command1.clear = 0;
	  command1.protection = command.protection;
	  command1.flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED;
	  need_command1 = true;
	}
    }

  /* Apply the offset to both commands if necessary.  */

  if (offset)
    {
      if (need_command1)
	command1.vm_address += offset;

      command.vm_address += offset;
    }

  /* Write both commands.  */

  if (sizeof loader_area - loader_area_used < sizeof command)
    return 1;

  memcpy (loader_area + loader_area_used, &command,
	  sizeof command);
  loader_area_used += sizeof command;

  if (!need_command1)
    return 0;

  if (sizeof loader_area - loader_area_used < sizeof command1)
    return 1;

  memcpy (loader_area + loader_area_used, &command1,
	  sizeof command1);
  loader_area_used += sizeof command1;

  return 0;
}

#if defined __mips__ && !defined MIPS_NABI

/* Static storage used for MIPS ABI flags.  */
static struct mips_elf_abi_flags exec_abi, interpreter_abi;

/* Static storage for interpreter headers.  */
static elf_header exec_interpreter_header;

/* Pointer to the ELF header of this executable's interpreter.  */
static elf_header *interpreter_header;

/* Pointer to any PT_MIPS_ABIFLAGS program header found in the
   executable itself.  */
static struct mips_elf_abi_flags *exec_abiflags;

/* Pointer to any PT_MIPS_ABIFLAGS program header found in the
   executable's ELF interpreter.  */
static struct mips_elf_abi_flags *interpreter_abiflags;

#endif /* defined __mips__ && !defined MIPS_NABI */

/* Process the specified program HEADER; HEADER is from the ELF
   interpreter of another executable.  FD is the executable file from
   which it is being read, NAME is its file name, and ELF_HEADER is
   its header.

   If ELF_HEADER->e_type is ET_DYN, add the base address for position
   independent interpreter code to virtual addresses.

   Value is 1 upon failure, else 0.  */

static int
process_interpreter_1 (const char *name, int fd,
		       program_header *header,
		       elf_header *elf_header)
{
  int rc;
#if defined __mips__ && !defined MIPS_NABI
  ssize_t rc1;
#endif /* defined __mips__ && !defined MIPS_NABI */

  switch (header->p_type)
    {
    default: /* PT_NULL, PT_NOTE, PT_DYNAMIC, PT_INTERP, et cetera */
      rc = 0;
      break;

    case 1: /* PT_LOAD */
      /* This describes a segment in the file that must be loaded.
	 Write the appropriate load command.  */

      if (elf_header->e_type == 3) /* ET_DYN */
	rc = write_load_command (header, true,
				 INTERPRETER_BASE);
      else
	rc = write_load_command (header, true, 0);

      break;

#if defined __mips__ && !defined MIPS_NABI
    case 0x70000003: /* PT_MIPS_ABIFLAGS */
      /* Record this header for later use.  */
      rc1 = pread (fd, &interpreter_abi, sizeof interpreter_abi,
		   header->p_offset);

      if (rc1 != sizeof interpreter_abi)
	return 1;

      interpreter_abiflags = &interpreter_abi;
      rc = 0;
#endif /* defined __mips__ && !defined MIPS_NABI */
    }

  return rc;
}

/* Read the ELF interpreter specified in the given program header from
   FD, and append the commands necessary to load it to the load area.
   Then, return the interpreter entry point in *ENTRY.

   Value is 1 upon failure, else 0.  */

static int
process_interpreter (int fd, program_header *prog_header,
		     USER_WORD *entry)
{
  char buffer[PATH_MAX + 1];
  int rc, size, i;
  elf_header header;
  program_header program;

  /* Read the interpreter name.  */
  size = MIN (prog_header->p_filesz, PATH_MAX);
  rc = pread (fd, buffer, size, prog_header->p_offset);
  if (rc < size)
    return 1;

  /* Make sure the name is NULL terminated.  */
  buffer[size] = '\0';

  /* Check if the file is executable.  This is unfortunately not
     atomic.  */

  if (access (buffer, X_OK))
    return 1;

  /* Read the interpreter's header much like exec_0.

     However, use special command codes in `process_program_header' if
     it is position independent.  That way, the loader knows it should
     use the open interpreter instead.  */

  fd = open (buffer, O_RDONLY);

  if (fd < 0)
    return 1;

  rc = read (fd, &header, sizeof header);

  if (rc < sizeof header)
    goto fail;

#if defined __mips__ && !defined MIPS_NABI
  /* Record this interpreter's header for later use determining the
     floating point ABI.  */
  exec_interpreter_header = header;
  interpreter_header = &exec_interpreter_header;
#endif /* defined __mips__ && !defined MIPS_NABI */

  /* Verify that this is indeed an ELF file.  */

  if (header.e_ident[0] != 0x7f
      || header.e_ident[1] != 'E'
      || header.e_ident[2] != 'L'
      || header.e_ident[3] != 'F')
    goto fail;

  /* Now check that the class is correct.  */
#ifdef EXEC_64
  if (header.e_ident[4] != 2)
    goto fail;
#else /* !EXEC_64 */
  if (header.e_ident[4] != 1)
    goto fail;
#endif /* EXEC_64 */

  /* And the endianness.  */
#ifndef WORDS_BIGENDIAN
  if (header.e_ident[5] != 1)
    goto fail;
#else /* WORDS_BIGENDIAN */
  if (header.e_ident[5] != 2)
    goto fail;
#endif /* EXEC_64 */

  /* Check that this is an executable.  */
  if (header.e_type != 2 && header.e_type != 3)
    goto fail;

  /* Now check that the ELF program header makes sense.  */
  if (header.e_phnum > 0xffff
      || (header.e_phentsize
	  != sizeof (program_header)))
    goto fail;

  if (write_open_command (buffer, true))
    goto fail;

  for (i = 0; i < header.e_phnum; ++i)
    {
      rc = read (fd, &program, sizeof program);
      if (rc < sizeof program)
	goto fail;

      if (process_interpreter_1 (buffer, fd, &program,
				 &header))
	goto fail;
    }

  if (header.e_type == 3) /* ET_DYN */
    *entry = header.e_entry + INTERPRETER_BASE;
  else
    *entry = header.e_entry;

  close (fd);
  return 0;

 fail:
  close (fd);
  return 1;
}

/* Process the specified program HEADER.  FD is the executable file
   from which it is being read, NAME is its file name, and ELF_HEADER
   is its header.

   If ELF_HEADER->e_type is ET_DYN, add the base address for position
   independent code to virtual addresses.

   If OFFSET is non-NULL, and *OFFSET is -1, write the virtual address
   of HEADER if it describes a PT_LOAD segment.

   If an interpreter is found, set *ENTRY to its entry point.

   Value is 1 upon failure, else 0.  */

static int
process_program_header (const char *name, int fd,
			program_header *header,
			elf_header *elf_header,
			USER_WORD *entry,
			USER_WORD *offset)
{
  int rc;
#if defined __mips__ && !defined MIPS_NABI
  ssize_t rc1;
#endif /* defined __mips__ && !defined MIPS_NABI */

  switch (header->p_type)
    {
    default: /* PT_NULL, PT_NOTE, PT_DYNAMIC, et cetera */
      rc = 0;
      break;

    case 1: /* PT_LOAD */
      /* This describes a segment in the file that must be loaded.
	 Write the appropriate load command.  */

      if (elf_header->e_type == 3) /* ET_DYN */
	{
	  rc = write_load_command (header, false,
				   EXECUTABLE_BASE);

	  if (!rc && offset && *offset == (USER_WORD) -1)
	    *offset = EXECUTABLE_BASE + header->p_vaddr;
	}
      else
	{
	  rc = write_load_command (header, false, 0);

	  if (!rc && offset && *offset == (USER_WORD) -1)
	    *offset = header->p_vaddr;
	}

      break;

    case 3: /* PT_INTERP */
      /* This describes another executable that must be loaded.  Open
	 the interpreter and process each of its headers as well.  */
      rc = process_interpreter (fd, header, entry);
      break;

    case 1685382481: /* PT_GNU_STACK */
      /* TODO */
      rc = 0;
      break;

#if defined __mips__ && !defined MIPS_NABI
    case 0x70000003: /* PT_MIPS_ABIFLAGS */
      /* Record this header for later use.  */
      rc1 = pread (fd, &exec_abi, sizeof exec_abi,
		   header->p_offset);

      if (rc1 != sizeof exec_abi)
	return 1;

      exec_abiflags = &exec_abi;
      rc = 0;
#endif /* defined __mips__ && !defined MIPS_NABI */
    }

  return rc;
}

/* Prepend one or two extra arguments ARG1 and ARG2 to a pending
   execve system call.  Replace the argument immediately after
   with ARG3.

   TRACEE is the tracee performing the system call, and REGS are its
   current user registers.  Value is 1 upon failure, else 0.  */

static int
insert_args (struct exec_tracee *tracee, USER_REGS_STRUCT *regs,
	     const char *arg1, const char *arg2, const char *arg3)
{
  USER_WORD argv, argc, word, new;
  USER_WORD new1, new2, new3, i;
  size_t text_size, effective_size;
  USER_REGS_STRUCT original;

  /* First, get a pointer to the current argument vector.  */
  argv = regs->SYSCALL_ARG1_REG;

  /* Now figure out how many arguments there are.  */
  argc = 0;
  while (true)
    {
      /* Clear errno.  PTRACE_PEEKDATA returns the word read the same
	 way failure indications are returned, so the only way to
	 catch IO errors is by clearing errno before the call to
	 ptrace and checking it afterwards.  */

      errno = 0;
      word = ptrace (PTRACE_PEEKDATA, tracee->pid,
		     (void *) argv, NULL);
      argv += sizeof (USER_WORD);

      if (errno)
	return 1;

      if (!word)
	break;

      ++argc;
    };

  /* Allocate enough to hold that many arguments, alongside the argc
     text.  */

  text_size = (strlen (arg1) + 1
	       + (arg2 ? strlen (arg2) + 1 : 0)
	       + strlen (arg3) + 1);

  /* Round it up to the user word size.  */
  text_size += sizeof (USER_WORD) - 1;
  text_size &= ~(sizeof (USER_WORD) - 1);

  /* Now allocate the new argv.  Make sure argc is at least 1; it
     needs to hold ARG3.  */

  effective_size = sizeof word * (MAX (1, argc) + 2) + text_size;

  if (arg2)
    effective_size += sizeof word;

  /* Copy regs to original so that user_alloca knows it should append
     the ABI red zone.  */

  memcpy (&original, regs, sizeof *regs);
  new = user_alloca (tracee, &original, regs,
		     effective_size);

  if (!new)
    goto fail;

  /* Figure out where argv starts.  */

  new3 = new + text_size;

  /* Now write the first two strings.  */

  new1 = new + strlen (arg1) + 1;
  new2 = new1 + (arg2 ? strlen (arg2) + 1 : 0);

  if (user_copy (tracee, (const unsigned char *) arg1,
		 new, new1 - new))
    goto fail;

  if (arg2 && user_copy (tracee, (const unsigned char *) arg2,
			 new1, new2 - new1))
    goto fail;

  /* Write the replacement arg3, the file name of the executable.  */

  if (user_copy (tracee, (const unsigned char *) arg3,
		 new2, new3 - new2))
    goto fail;

  /* Start copying argv back to new2.  First, write the one or two new
     arguments.  */

  if (ptrace (PTRACE_POKETEXT, tracee->pid,
	      (void *) new3, (void *) new))
    goto fail;

  new3 += sizeof new3;

  if (arg2 && ptrace (PTRACE_POKETEXT, tracee->pid,
		      (void *) new3, (void *) new1))
    goto fail;
  else if (arg2)
    new3 += sizeof new3;

  /* Next, write the third argument.  */

  if (ptrace (PTRACE_POKETEXT, tracee->pid, (void *) new3,
	      (void *) new2))
    goto fail;

  new3 += sizeof new3;

  /* Copy the remaining arguments back.  */

  argv = regs->SYSCALL_ARG1_REG;

  if (argc)
    {
      /* Make sure the trailing NULL is included.  */
      argc += 1;

      /* Now copy each argument in argv, starting from argv[1].  */

      for (i = 1; i < argc; ++i)
	{
	  /* Read one argument.  */
	  word = ptrace (PTRACE_PEEKDATA, tracee->pid,
			 (void *) (argv + i * sizeof argv), NULL);

	  /* Write one argument, then increment new3.  */

	  if (ptrace (PTRACE_POKETEXT, tracee->pid,
		      (void *) new3, (void *) word))
	    goto fail;

	  new3 += sizeof new3;
	}
    }
  else
    {
      /* Just write the trailing NULL.  */

      if (ptrace (PTRACE_POKETEXT, tracee->pid,
		  (void *) new3, (void *) 0))
	goto fail;

      new3 += sizeof new3;
    }

  /* Assert that new3 is not out of bounds.  */
  assert (new3 == new + effective_size);

  /* And that it is properly aligned.  */
  assert (!(new3 & (sizeof new3 - 1)));

  /* Now modify the system call argument to point to new +
     text_size.  */

  regs->SYSCALL_ARG1_REG = new + text_size;

#ifdef __aarch64__
  if (aarch64_set_regs (tracee->pid, regs, false))
    goto fail;
#else /* !__aarch64__ */
  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL, regs))
    goto fail;
#endif /* __aarch64__ */

  /* Success.  */

  return 0;

 fail:
  /* Restore the original stack pointer.  */
#ifdef __aarch64__
  aarch64_set_regs (tracee->pid, &original, false);
#else /* !__aarch64__ */
  ptrace (PTRACE_SETREGS, tracee->pid, NULL, &original);
#endif /* __aarch64__ */
  errno = ENOMEM;
  return 1;
}



/* Format PID, an unsigned process identifier, in base 10.  Place the
   result in *IN, and return a pointer to the byte after the
   result.  REM should be NULL.  */

char *
format_pid (char *in, unsigned int pid)
{
  unsigned int digits[32], *fill;

  fill = digits;

  for (; pid != 0; pid = pid / 10)
    *fill++ = pid % 10;

  /* Insert 0 if the number would otherwise be empty.  */

  if (fill == digits)
    *fill++ = 0;

  while (fill != digits)
    {
      --fill;
      *in++ = '0' + *fill;
    }

  *in = '\0';
  return in;
}

/* Return a sequence of actions required to load the executable under
   the file NAME for the given TRACEE.  First, see if the file starts
   with #!; in that case, find the program to open and use that
   instead.

   Next, read the executable header, and add the necessary memory
   mappings for each file.  Finally, return the action data and its
   size in *SIZE.

   Finally, use REGS to add the required interpreter arguments to the
   caller's argv.

   Value is NULL upon failure, with errno set accordingly.  */

char *
exec_0 (char *name, struct exec_tracee *tracee,
	size_t *size, USER_REGS_STRUCT *regs)
{
  int fd, rc, i;
  elf_header header;
  const char *interpreter_name, *extra;
  program_header program;
  USER_WORD entry, program_entry, offset;
  USER_WORD header_offset;
  USER_WORD name_len, aligned_len;
  struct exec_jump_command jump;
  /* This also encompasses !__LP64__.  */
#if defined __mips__ && !defined MIPS_NABI
  int fpu_mode;
#endif /* defined __mips__ && !defined MIPS_NABI */
  char buffer[80], buffer1[PATH_MAX + 80], *rewrite;
  ssize_t link_size;
  size_t remaining;

  /* If the process is trying to run /proc/self/exe, make it run
     itself instead.  */

  if (!strcmp (name, "/proc/self/exe") && tracee->exec_file)
    {
      strncpy (name, tracee->exec_file, PATH_MAX - 1);
      name[PATH_MAX] = '\0';
    }
  else
    {
      /* If name is not absolute, then make it relative to TRACEE's
	 cwd.  Do not use sprintf at it is not reentrant and it
	 mishandles results longer than INT_MAX.  */

      if (name[0] && name[0] != '/')
	{
	  /* Clear both buffers.  */
	  memset (buffer, 0, sizeof buffer);
	  memset (buffer1, 0, sizeof buffer1);

	  /* Copy over /proc, the PID, and /cwd/.  */
	  rewrite = stpcpy (buffer, "/proc/");
	  rewrite = format_pid (rewrite, tracee->pid);
	  strcpy (rewrite, "/cwd");

	  /* Resolve this symbolic link.  */

	  link_size = readlink (buffer, buffer1,
				PATH_MAX + 1);

	  if (link_size < 0)
	    return NULL;

	  /* Check that the name is a reasonable size.  */

	  if (link_size > PATH_MAX)
	    {
	      /* The name is too long.  */
	      errno = ENAMETOOLONG;
	      return NULL;
	    }

	  /* Add a directory separator if necessary.  */

	  if (!link_size || buffer1[link_size - 1] != '/')
	    buffer1[link_size] = '/', link_size++;

	  rewrite = buffer1 + link_size;
	  remaining = buffer1 + sizeof buffer1 - rewrite - 1;
	  memcpy (rewrite, name, strnlen (name, remaining));

	  /* Replace name with buffer1.  */
	  strcpy (name, buffer1);
	}
    }

  /* Check that the file is accessible and executable.  */

  if (access (name, X_OK))
    return NULL;

  fd = open (name, O_RDONLY);
  if (fd < 0)
    return NULL;

  /* Now read the header.  */

  extra = NULL;
  interpreter_name = check_interpreter (name, fd, &extra);
  if (!interpreter_name)
    goto fail;

  /* Open the interpreter instead, if necessary.  */
  if (interpreter_name != name)
    {
      close (fd);
      fd = open (interpreter_name, O_RDONLY);
      if (fd < 0)
	return NULL;

      /* Now, rewrite the argument list to include `interpreter_name'
	 and perhaps `extra'.  */

      if (insert_args (tracee, regs, interpreter_name,
		       extra, name))
	goto fail1;
    }

  rc = read (fd, &header, sizeof header);

  if (rc < sizeof header)
    goto fail1;

  /* Verify that this is indeed an ELF file.  */

  if (header.e_ident[0] != 0x7f
      || header.e_ident[1] != 'E'
      || header.e_ident[2] != 'L'
      || header.e_ident[3] != 'F')
    goto fail1;

  /* Now check that the class is correct.  */
#ifdef EXEC_64
  if (header.e_ident[4] != 2)
    goto fail1;
#else /* !EXEC_64 */
  if (header.e_ident[4] != 1)
    goto fail1;
#endif /* EXEC_64 */

  /* And the endianness.  */
#ifndef WORDS_BIGENDIAN
  if (header.e_ident[5] != 1)
    goto fail1;
#else /* WORDS_BIGENDIAN */
  if (header.e_ident[5] != 2)
    goto fail1;
#endif /* EXEC_64 */

  /* Check that this is an executable.  */
  if (header.e_type != 2 && header.e_type != 3)
    goto fail1;

  /* Now check that the ELF program header makes sense.  */
  if (header.e_phnum > 0xffff
      || (header.e_phentsize
	  != sizeof (program_header)))
    goto fail1;

  /* Seek to the first program header and read each one.  */
  rc = lseek (fd, header.e_phoff, SEEK_SET);
  if (rc < 0)
    goto fail1;
  loader_area_used = 0;

  /* Write the command used to open the executable.  */
  if (write_open_command (interpreter_name, false))
    goto fail1;

  /* Apply base addresses for PIC code.  */

  if (header.e_type == 3) /* ET_DYN */
    offset = EXECUTABLE_BASE;
  else
    offset = 0;

  /* entry and program_entry are initially the same, but entry may be
     set to that of the interpreter if one is present.  */

  entry = header.e_entry + offset;
  program_entry = header.e_entry;

#if defined __mips__ && !defined MIPS_NABI
  /* Clear MIPS ABI flags.  */
  exec_abiflags = NULL;
  interpreter_abiflags = NULL;
  interpreter_header = NULL;
#endif /* defined __mips__ && !defined MIPS_NABI */

  /* Set header_offset to -1; `process_program_header' then updates it
     to that of the first mapping.  */
  header_offset = -1;

  for (i = 0; i < header.e_phnum; ++i)
    {
      rc = read (fd, &program, sizeof program);
      if (rc < sizeof program)
	goto fail1;

      if (process_program_header (interpreter_name, fd,
				  &program, &header,
				  &entry, &header_offset))
	goto fail1;
    }

  /* Write the entry point and program entry.  */

  jump.command = 3;
  jump.entry = entry;

  /* Now calculate values for the aux vector.  */

  jump.at_entry = program_entry + offset;
  jump.at_phent = header.e_phentsize;
  jump.at_phnum = header.e_phnum;
  jump.at_base = (entry == header.e_entry + offset
		  ? EXECUTABLE_BASE
		  : INTERPRETER_BASE);

#if defined __mips__ && !defined MIPS_NABI
  /* Finally, calculate the FPU mode wanted by the executable.  */

  if (determine_fpu_mode (&header, interpreter_header,
			  &fpu_mode, exec_abiflags,
			  interpreter_abiflags))
    /* N.B. that `determine_fpu_mode' sets errno.  */
    goto fail;

  /* If the processor is too new to support FR0 operation, place the
     executable in floating point emulation mode.  */

  if (fpu_mode == FP_FR0 && !cpu_supports_fr0_p ())
    fpu_mode = FP_FRE;

  jump.fpu_mode = fpu_mode;
#elif defined __mips__ && !defined __LP64__
  jump.fpu_mode = 0;
#endif /* defined __mips__ && defined MIPS_NABI && !defined __LP64__ */

  /* The offset used for at_phdr should be that of the first
     mapping.  */

  if (header_offset == (USER_WORD) -1)
    header_offset = 0;

  jump.at_phdr = header.e_phoff + header_offset;

  if (sizeof loader_area - loader_area_used < sizeof jump)
    goto fail1;

  memcpy (loader_area + loader_area_used, &jump,
	  sizeof jump);
  loader_area_used += sizeof jump;

  /* Copy the length of NAME and NAME itself to the loader area.  */
  name_len = strlen (name);
  aligned_len = ((name_len + 1 + sizeof name_len - 1)
		 & -sizeof name_len);
  if (sizeof loader_area - loader_area_used
      < aligned_len + sizeof name_len)
    goto fail1;
  memcpy (loader_area + loader_area_used, &name_len, sizeof name_len);
  loader_area_used += sizeof name_len;
  memcpy (loader_area + loader_area_used, name, name_len + 1);
  loader_area_used += name_len + 1;

  /* Properly align the loader area.  */
  offset = aligned_len - (name_len + 1);
  while (offset--)
    loader_area[loader_area_used++] = '\0';

  /* Close the file descriptor and return the number of bytes
     used.  */

  close (fd);
  *size = loader_area_used;

  /* Make sure the loader area is properly aligned.  */
  assert (!(loader_area_used & (sizeof (USER_WORD) - 1)));
  return loader_area;

 fail1:
  errno = ENOEXEC;
 fail:
  close (fd);
  return NULL;
}
