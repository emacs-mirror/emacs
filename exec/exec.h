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



#ifndef _EXEC_H_
#define _EXEC_H_

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif /* HAVE_STDINT_H */

#include <sys/types.h>

#include USER_HEADER

/* Define a replacement for `uint64_t' if it's not present in the C
   library.  */

#ifndef UINT64_MAX

typedef struct
{
  uint32_t word1;
  uint32_t word2;
} xint64_t;

#else /* UINT64_MAX */
typedef uint64_t xint64_t;
#endif /* !UINT64_MAX */



/* 32-bit ELF headers.  */

struct elf_header_32
{
  unsigned char e_ident[16];
  uint16_t      e_type;
  uint16_t      e_machine;
  uint32_t      e_version;
  uint32_t      e_entry;
  uint32_t      e_phoff;
  uint32_t      e_shoff;
  uint32_t      e_flags;
  uint16_t      e_ehsize;
  uint16_t      e_phentsize;
  uint16_t      e_phnum;
  uint16_t      e_shentsize;
  uint16_t      e_shnum;
  uint16_t      e_shstrndx;
};

struct program_header_32
{
  uint32_t p_type;
  uint32_t p_offset;
  uint32_t p_vaddr;
  uint32_t p_paddr;
  uint32_t p_filesz;
  uint32_t p_memsz;
  uint32_t p_flags;
  uint32_t p_align;
};

struct dt_entry_32
{
  uint32_t d_tag;
  uint32_t d_val;
};



struct elf_header_64
{
  unsigned char e_ident[16];
  uint16_t      e_type;
  uint16_t      e_machine;
  uint32_t      e_version;
  xint64_t      e_entry;
  xint64_t      e_phoff;
  xint64_t      e_shoff;
  uint32_t      e_flags;
  uint16_t      e_ehsize;
  uint16_t      e_phentsize;
  uint16_t      e_phnum;
  uint16_t      e_shentsize;
  uint16_t      e_shnum;
  uint16_t      e_shstrndx;
};

struct program_header_64
{
  uint32_t p_type;
  uint32_t p_flags;
  xint64_t p_offset;
  xint64_t p_vaddr;
  xint64_t p_paddr;
  xint64_t p_filesz;
  xint64_t p_memsz;
  xint64_t p_align;
};

struct dt_entry_64
{
  xint64_t d_tag;
  xint64_t d_val;
};



/* Define some types to the correct values.  */

#ifdef EXEC_64
typedef struct elf_header_64 elf_header;
typedef struct program_header_64 program_header;
typedef struct dt_entry_64 dt_entry;
#else /* !EXEC_64 */
typedef struct elf_header_32 elf_header;
typedef struct program_header_32 program_header;
typedef struct dt_entry_32 dt_entry;
#endif /* EXEC_64 */



/* Defined in trace.c.  */

/* Structure describing a process being traced.  */

struct exec_tracee
{
  /* The next process being traced.  */
  struct exec_tracee *next;

  /* Address of any stack pointer to restore after system call
     completion.  */
  USER_WORD sp;

  /* ID of the system call that is pending completion.  This value is
     not available as the call number is overwritten on success.  */
  USER_WORD callno;

  /* Name of the executable being run.  */
  char *exec_file;

  /* Pointer to a storage area holding instructions for loading an
     executable if an `exec' system call is outstanding, or NULL.  */
  char *exec_data;

  /* Number of bytes in exec_data.  */
  size_t data_size;

  /* The thread ID of this process.  */
  pid_t pid;

  /* Whether or not the tracee is currently waiting for a system call
     to complete.  */
  bool waiting_for_syscall : 1;

  /* Whether or not the tracee has been created but is not yet
     processed by `handle_clone'.  */
  bool new_child : 1;
};



#ifdef __aarch64__

extern int aarch64_get_regs (pid_t, USER_REGS_STRUCT *);
extern int aarch64_set_regs (pid_t, USER_REGS_STRUCT *, bool);

#endif /* __aarch64__ */



extern char *format_pid (char *, unsigned int);
extern USER_WORD user_alloca (struct exec_tracee *, USER_REGS_STRUCT *,
			      USER_REGS_STRUCT *, USER_WORD);
extern int user_copy (struct exec_tracee *, const unsigned char *,
		      USER_WORD, USER_WORD);
extern void exec_init (const char *);



extern int tracing_execve (const char *, char *const *,
			   char *const *);
extern int after_fork (pid_t);
extern pid_t exec_waitpid (pid_t, int *, int);



/* Defined in exec.c.  */

extern char *exec_0 (char *, struct exec_tracee *,
		     size_t *, USER_REGS_STRUCT *);



#endif /* _EXEC_H_ */
