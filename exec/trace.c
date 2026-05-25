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

#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <limits.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>

#include "exec.h"

#include SYSCALL_HEADER
#include USER_HEADER

#ifdef __aarch64__
#include <sys/uio.h> /* for struct iovec */
#include <linux/elf.h> /* for NT_* */
#endif /* __aarch64__ */

#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h> /* for process_vm_readv */
#ifndef HAVE_PROCESS_VM
#include <dlfcn.h>
#endif /* !HAVE_PROCESS_VM */
#endif /* HAVE_SYS_UIO_H */

#ifndef SYS_SECCOMP
#define SYS_SECCOMP 1
#endif /* !defined SYS_SECCOMP */

#ifndef PTRACE_GETEVENTMSG
#define PTRACE_GETEVENTMSG 0x4201
#endif /* !defined PTRACE_GETEVENTMSG */

#ifdef HAVE_SECCOMP
#include <linux/seccomp.h>
#include <linux/filter.h>

#include <sys/utsname.h>
#include <sys/prctl.h>

#include <stdio.h>
#endif /* !defined HAVE_SECCOMP */



/* Program tracing functions.

   The main entry point is the function `tracing_execve', which traces
   the thread and calls exec.  Each time that thread calls `clone',
   the new child is traced as well.

   Instead of calling `waitpid', call `exec_waitpid' instead.  */



/* Number of tracees children are allowed to create.  */
#define MAX_TRACEES 4096

#if defined HAVE_SYS_UIO_H && !defined HAVE_PROCESS_VM

/* Load have_process_vm dynamically if possible to avoid PTRACE_PEEKDATA
   restrictions on Android 15 QPR2+.  */

static ssize_t (*process_vm_readv) (pid_t, const struct iovec *,
				    unsigned long,
				    const struct iovec *,
				    unsigned long, unsigned long);
static ssize_t (*process_vm_writev) (pid_t, const struct iovec *,
				     unsigned long,
				     const struct iovec *,
				     unsigned long, unsigned long);

#endif /* HAVE_SYS_UIO_H && !HAVE_PROCESS_VM */

#ifdef HAVE_SECCOMP

/* Whether to enable seccomp acceleration.  */
static bool use_seccomp_p;

#else /* !HAVE_SECCOMP */
#define use_seccomp_p (false)
#endif /* HAVE_SECCOMP */

#ifdef __aarch64__

/* Place PID's registers into *REGS.  Return 1 upon failure, else
   0.  */

int
aarch64_get_regs (pid_t pid, USER_REGS_STRUCT *regs)
{
  struct iovec iov;

  iov.iov_base = regs;
  iov.iov_len = sizeof *regs;

  return (ptrace (PTRACE_GETREGSET, pid, NT_PRSTATUS,
		  &iov) != 0);
}

/* Set PID's registers to *REGS.  If SYSCALL_P, also update the
   current system call number to the `x8' register.

   Value is 1 upon failure, else 0.  */

int
aarch64_set_regs (pid_t pid, USER_REGS_STRUCT *regs,
		  bool syscall_p)
{
  struct iovec iov;
  USER_WORD callno;
  long rc;

  /* Write the user registers.  */

  iov.iov_base = regs;
  iov.iov_len = sizeof *regs;

  rc = ptrace (PTRACE_SETREGSET, pid, NT_PRSTATUS, &iov);
  if (rc < 0)
    return 1;

  /* Now, write the system call number if necessary.  */

  if (syscall_p)
    {
      callno = regs->regs[8];
      iov.iov_base = &callno;
      iov.iov_len = sizeof callno;

      return (ptrace (PTRACE_SETREGSET, pid, NT_ARM_SYSTEM_CALL,
		      &iov) != 0);
    }

  return 0;
}

#endif /* __aarch64__ */



/* List of all processes which are being traced.  */
static struct exec_tracee *tracing_processes;



/* Read N bytes from TRACEE's memory, starting at the specified user
   ADDRESS.  Return its contents in BUFFER.

   If there are unreadable pages within ADDRESS + N, the contents of
   BUFFER after the first such page become undefined.  */

static void
read_memory (struct exec_tracee *tracee, char *buffer,
	     USER_WORD n, USER_WORD address)
{
  USER_WORD word, n_words, n_bytes, i;
  long rc;
#ifdef HAVE_SYS_UIO_H
  struct iovec iov, remote;

  /* If `process_vm_readv' is available, use it instead.  */

  iov.iov_base = buffer;
  iov.iov_len = n;
  remote.iov_base = (void *) address;
  remote.iov_len = n;

  /* Return immediately if successful.  As long as some bytes were
     read, consider the read to have been a success.  */

  if (n <= SSIZE_MAX
#ifndef HAVE_PROCESS_VM
      && process_vm_readv
#endif /* !HAVE_PROCESS_VM */
      && (process_vm_readv (tracee->pid, &iov, 1,
			    &remote, 1, 0) != -1))
    return;

#endif /* !HAVE_SYS_UIO_H */

  /* First, read entire words from the tracee.  */
  n_words = n & ~(sizeof (USER_WORD) - 1);

  /* Next, determine the number of bytes to read from the last
     word.  */
  n_bytes = n & (sizeof (USER_WORD) - 1);

  /* Start reading words.  */
  i = 0;
  while (n_words)
    {
      rc = ptrace (PTRACE_PEEKTEXT, tracee->pid,
		   (void *) address + i, NULL);
      word = rc;
      memcpy (buffer, &word, sizeof word);
      buffer += sizeof word;
      i += sizeof word;
      n_words -= sizeof word;
    }

  /* Now, read the remaining bytes.  */
  assert (n_bytes < sizeof (word));

  if (n_bytes)
    {
      rc = ptrace (PTRACE_PEEKTEXT, tracee->pid,
		   (void *) address + i, NULL);
      word = rc;

      /* Copy only n_bytes to the caller.  */
      memcpy (buffer, &word, n_bytes);
    }
}

/* Allocate N bytes of memory from TRACEE's stack.  Return the address
   of that memory upon success, else 0.

   Place the updated user-mode registers of TRACEE in *NEW_REGS, which
   should initially contain the current stack pointer of TRACEE.

   REGS should contain the user mode registers of TRACEE prior to the
   system call starting; it is not updated to reflect any changes.  */

USER_WORD
user_alloca (struct exec_tracee *tracee, USER_REGS_STRUCT *regs,
	     USER_REGS_STRUCT *new_regs, USER_WORD n)
{
  USER_WORD sp, old_sp;

  /* Get the current stack pointer.  */
  old_sp = sp = new_regs->STACK_POINTER;

#if RED_ZONE_SIZE
  /* Some ABI rules specify a ``red zone'' around the stack pointer
     that is reserved for compiler optimizations.  */

#ifdef STACK_GROWS_DOWNWARDS
  if (sp == regs->STACK_POINTER)
    sp -= RED_ZONE_SIZE;
#else /* !STACK_GROWS_DOWNWARDS */
  if (sp == regs->STACK_POINTER)
    sp += RED_ZONE_SIZE;
#endif /* STACK_GROWS_DOWNWARDS */
#endif /* RED_ZONE_SIZE */

  /* Now take N off the stack.  */

#ifdef STACK_GROWS_DOWNWARDS
  sp = sp - n;

  /* Check for overflow.  */

  if (sp > new_regs->STACK_POINTER)
    return 0;
#else /* !STACK_GROWS_DOWNWARDS */
  sp = sp + n;

  /* Check for overflow.  */

  if (sp < new_regs->STACK_POINTER)
    return 0;
#endif /* STACK_GROWS_DOWNWARDS */

  /* Set the stack pointer.  */
  new_regs->STACK_POINTER = sp;

#ifdef __aarch64__
  if (aarch64_set_regs (tracee->pid, new_regs, false))
    goto fail;
#else /* !__aarch64__ */
  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL,
	      new_regs))
    goto fail;
#endif /* __aarch64__ */

  /* Now return the start of the new area.  */
#ifdef STACK_GROWS_DOWNWARDS
  return sp;
#else /* !STACK_GROWS_DOWNWARDS */
  return sp - n;
#endif /* STACK_GROWS_DOWNWARDS */

 fail:
  /* Restore the old stack pointer.  */
  new_regs->STACK_POINTER = old_sp;
  return 0;
}

/* Copy N bytes to ADDRESS in TRACEE's address space from BUFFER.
   Value is 0 upon success, else 1.  */

int
user_copy (struct exec_tracee *tracee, const unsigned char *buffer,
	   USER_WORD address, USER_WORD n)
{
  USER_WORD start, end, word;
  unsigned char *bytes;
#ifdef HAVE_SYS_UIO_H
  struct iovec iov, remote;

  /* Try to use `process_vm_writev' if possible, but fall back to
     ptrace if something bad happens.  */

  iov.iov_base = (void *) buffer;
  iov.iov_len = n;
  remote.iov_base = (void *) address;
  remote.iov_len = n;

  if (n <= SSIZE_MAX
#ifndef HAVE_PROCESS_VM
      && process_vm_writev
#endif /* !HAVE_PROCESS_VM */
      && (process_vm_writev (tracee->pid, &iov, 1,
			     &remote, 1, 0) == n))
    return 0;
#endif /* HAVE_SYS_UIO_H */

  /* Calculate the start and end positions for the write.  */

  start = address;
  end = address + n;

  /* Write from start to the last word.  */

  while (start < end)
    {
      if (start + sizeof word <= end)
	{
	  /* Write a word by itself and increment start.  */
	  memcpy (&word, buffer, sizeof word);
	  buffer += sizeof word;

	  if (ptrace (PTRACE_POKEDATA, tracee->pid,
		      (void *) start, (void *) word))
	    return 1;

	  start += sizeof word;
	}
      else
	{
	  /* Only end - start bytes should be written.
	     Read the word at start from tracee->pid, then write
	     it back with changes.  */

	  word = ptrace (PTRACE_PEEKDATA, tracee->pid,
			 (void *) start, NULL);
	  bytes = (unsigned char *) &word;
	  memcpy (bytes, buffer, end - start);

	  if (ptrace (PTRACE_POKEDATA, tracee->pid,
		      (void *) start, (void *) word))
	    return 1;

	  /* Writing was successful.  */
	  return 0;
	}
    }

  return 0;
}



/* Chain of free exec_tracee structures.  */
static struct exec_tracee *free_tracees;

/* Remove the specified TRACEE from the chain of all processes being
   traced.  */

static void
remove_tracee (struct exec_tracee *tracee)
{
  struct exec_tracee **last;

  last = &tracing_processes;
  while (*last)
    {
      if (*last == tracee)
	{
	  *last = tracee->next;

	  /* Link the tracee onto the list of free tracees.  */
	  tracee->next = free_tracees;

	  /* Free the exec file, if any.  */
	  free (tracee->exec_file);
	  tracee->exec_file = NULL;

	  /* Likewise with any loader instructions that might be
	     present.  */
	  free (tracee->exec_data);
	  tracee->exec_data = NULL;

	  /* Return this tracee to the list of free ones.  */
	  free_tracees = tracee;
	  return;
	}
      else
	last = &(*last)->next;
    }
}



/* Child process tracing.  */

/* Array of `struct exec_tracees' that they are allocated from.  */
static struct exec_tracee static_tracees[MAX_TRACEES];

/* Number of tracees currently allocated.  */
static int tracees;

/* Return the `struct exec_tracee' corresponding to the specified
   PROCESS.  */

static struct exec_tracee *
find_tracee (pid_t process)
{
  struct exec_tracee *tracee;

  for (tracee = tracing_processes; tracee; tracee = tracee->next)
    {
      if (tracee->pid == process)
	return tracee;
    }

  return NULL;
}

/* Prepare to handle the completion of a `clone' system call.

   If the new clone is not yet being traced, create a new tracee for
   PARENT's child, copying over its current command line.  Then, set
   `new_child' in the new tracee.  Otherwise, continue it until the
   next syscall.  */

static void
handle_clone_prepare (struct exec_tracee *parent)
{
  long rc;
  unsigned long pid;
  struct exec_tracee *tracee;

  rc = ptrace (PTRACE_GETEVENTMSG, parent->pid, NULL,
	       &pid);
  if (rc)
    return;

  /* See if the tracee already exists.  */
  tracee = find_tracee (pid);

  if (tracee)
    {
      /* Continue the tracee.  Record its command line, as that has
	 not yet been done.  */

      assert (tracee->new_child);
      tracee->new_child = false;
      tracee->exec_file = NULL;
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
	      tracee->pid, 0, 0);

      if (parent->exec_file)
	tracee->exec_file = strdup (parent->exec_file);
      return;
    }

  if (free_tracees)
    {
      tracee = free_tracees;
      free_tracees = free_tracees->next;
    }
  else if (tracees < MAX_TRACEES)
    {
      tracee = &static_tracees[tracees];
      tracees++;
    }
  /* Try to allocate a tracee using `malloc'.  */
  else if ((tracee = malloc (sizeof *tracee)))
    ;
  else
    return;

  tracee->pid = pid;
  tracee->next = tracing_processes;
  tracee->waiting_for_syscall = false;
  tracee->new_child = true;
  tracee->exec_file = NULL;
  tracing_processes = tracee;

  /* Copy over the command line.  */

  if (parent->exec_file)
    tracee->exec_file = strdup (parent->exec_file);
}

/* Handle the completion of a `clone' or `clone3' system call,
   resulting in the creation of the process PID.  If TRACEE is NULL,
   allocate a new tracee structure from a static area for the
   processes's pid, then set TRACEE->new_child to true and await the
   parent's corresponding ptrace event to arrive; otherwise, just
   clear TRACEE->new_child.

   Value is 0 upon success, 2 if TRACEE should remain suspended until
   the parent's ptrace-stop, and 1 otherwise.  */

static int
handle_clone (struct exec_tracee *tracee, pid_t pid)
{
  long rc;
  int flags, value;

  /* Now allocate a new tracee, either from static_tracees or the free
     list, if no tracee was supplied.  */

  value = 0;

  if (!tracee)
    {
      if (free_tracees)
	{
	  tracee = free_tracees;
	  free_tracees = free_tracees->next;
	}
      else if (tracees < MAX_TRACEES)
	{
	  tracee = &static_tracees[tracees];
	  tracees++;
	}
      /* Try to allocate a tracee using `malloc'.  */
      else if ((tracee = malloc (sizeof *tracee)))
	;
      else
	return 1;

      tracee->pid = pid;
      tracee->next = tracing_processes;
      tracee->waiting_for_syscall = false;
      tracee->exec_file = NULL;
      tracee->exec_data = NULL;
      tracee->data_size = 0;
      tracing_processes = tracee;
      tracee->new_child = true;

      /* Wait for the ptrace-stop to happen in the parent.  */
      value = 2;
    }
  else
    /* Clear the flag saying that this is a newly created child
       process.  */
    tracee->new_child = false;

  /* Apply required options to the child, so that the kernel
     automatically traces children and makes it easy to differentiate
     between system call traps and other kinds of traps.  */

  flags  = PTRACE_O_TRACECLONE;
  flags |= PTRACE_O_TRACEVFORK;
  flags |= PTRACE_O_TRACEFORK;
  flags |= PTRACE_O_TRACESYSGOOD;
  flags |= PTRACE_O_TRACEEXIT;

#ifdef HAVE_SECCOMP
  if (use_seccomp_p)
    flags |= PTRACE_O_TRACESECCOMP;
#endif /* HAVE_SECCOMP */

  rc = ptrace (PTRACE_SETOPTIONS, pid, 0, flags);

  if (rc)
    goto bail;

  if (value != 2)
    {
      /* The new tracee is currently stopped.  Continue it until the next
	 system call.  */

      rc = ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		   pid, 0, 0);

      if (rc)
	goto bail;
    }

  return value;

 bail:
  remove_tracee (tracee);
  return 1;
}



/* NOTICE: none of these functions should ever call `malloc' or
   another async signal unsafe function.  */

/* File name of the loader binary.  */
static const char *loader_name;



/* Return whether or not the trap signal described by SIGNAL is
   generated by a system call being attempted by a tracee.  */

static bool
syscall_trap_p (siginfo_t *signal)
{
  /* SIGTRAP delivered by the kernel means this is a system call
     stop.  */
  return (signal->si_code == SIGTRAP
	  || signal->si_code == (SIGTRAP | SI_KERNEL));
}

/* Check if the wait status STATUS indicates a system call trap.
   TRACEE is the process whose stop STATUS describes.  If TRACEE exits
   while this information is being determined, return -1; if STATUS
   indicates some other kind of stop, return 1 after continuing
   TRACEE.  Value is 0 otherwise.  */

static int
check_signal (struct exec_tracee *tracee, int status)
{
  siginfo_t siginfo;

  switch ((status & 0xfff00) >> 8)
    {
    case SIGTRAP:
      /* Now, use PTRACE_GETSIGINFO to determine whether or not the
	 signal was delivered in response to a system call.  */

      if (ptrace (PTRACE_GETSIGINFO, tracee->pid, 0, &siginfo))
	return -1;

      if (!syscall_trap_p (&siginfo))
	{
	  if (siginfo.si_code < 0)
	    /* SIGTRAP delivered from userspace.  Pass it on.  */
	    ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		    tracee->pid, 0, SIGTRAP);
	  else
	    ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		    tracee->pid, 0, 0);

	  return 1;
	}

    case SIGTRAP | 0x80: /* SIGTRAP | 0x80 specifically refers to
			    system call traps.  */
      break;

#ifdef SIGSYS
    case SIGSYS:
      if (ptrace (PTRACE_GETSIGINFO, tracee->pid, 0, &siginfo))
	return -1;

      /* Continue the process until the next syscall, but don't
	 pass through the signal if an emulated syscall led to
	 it.  */
#ifdef HAVE_SIGINFO_T_SI_SYSCALL
#ifndef __arm__
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL), tracee->pid,
	      0, ((siginfo.si_code == SYS_SECCOMP
		   && siginfo.si_syscall == -1)
		  ? 0 : status));
#else /* __arm__ */
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL), tracee->pid,
	      0, ((siginfo.si_code == SYS_SECCOMP
		   && siginfo.si_syscall == 222)
		  ? 0 : status));
#endif /* !__arm__ */
#else /* !HAVE_SIGINFO_T_SI_SYSCALL */
      /* Drop this signal, since what caused it is unknown.  */
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL), tracee->pid,
	      0, 0);
#endif /* HAVE_SIGINFO_T_SI_SYSCALL */
      return 1;
#endif /* SIGSYS */

    default:
      /* Continue the process until the next syscall.  */
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
	      tracee->pid, 0, status);
      return 1;
    }

  return 0;
}



/* Handle the first stage of an `exec' system call from the given
   TRACEE.  REGS are the tracee's current user-mode registers.

   Rewrite the system call arguments to use the loader binary.  Then,
   resume the process till the loader is loaded and about to begin
   execution.  Save instructions to load the original executable into
   TRACEE->exec_data.

   Value is 0 upon success, 1 upon a generic failure before the loader
   is loaded.

   Set errno appropriately upon returning a generic failure.  */

static int
handle_exec (struct exec_tracee *tracee, USER_REGS_STRUCT *regs)
{
  char buffer[PATH_MAX + 80], *area;
  USER_REGS_STRUCT original;
  size_t size, loader_size;
  USER_WORD loader;

  /* Read the file name.  */
  read_memory (tracee, buffer, PATH_MAX, regs->SYSCALL_ARG_REG);

  /* Make sure BUFFER is NULL terminated.  */

  if (!memchr (buffer, '\0', PATH_MAX))
    {
      errno = ENAMETOOLONG;
      return 1;
    }

  /* Copy over the registers as they originally were.  */
  memcpy (&original, regs, sizeof *regs);

  /* Figure out what the loader needs to do.  */
 again1:
  area = exec_0 (buffer, tracee, &size, regs);

  if (!area)
    {
      /* Handle SIGINTR errors caused by IO.  */
      if (errno == EINTR)
	goto again1;

      return 1;
    }

  /* Save this area in the tracee.  */
  assert (!tracee->exec_data);
  tracee->exec_data = malloc (size);
  if (!tracee->exec_data)
    {
      errno = ENOMEM;
      return 1;
    }
  memcpy (tracee->exec_data, area, size);
  tracee->data_size = size;

  /* Rewrite the first argument to point to the loader.  */
  loader_size = strlen (loader_name) + 1;
  loader = user_alloca (tracee, &original, regs,
			loader_size);

  if (!loader)
    {
      errno = ENOMEM;
      goto free_data_error;
    }

  if (user_copy (tracee, (unsigned char *) loader_name,
		 loader, loader_size))
    {
      errno = EIO;
      goto free_data_error;
    }

  regs->SYSCALL_ARG_REG = loader;

#ifdef __aarch64__

  if (aarch64_set_regs (tracee->pid, regs, false))
    {
      errno = EIO;
      goto free_data_error;
    }

#else /* !__aarch64__ */

  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL, regs))
    {
      errno = EIO;
      goto free_data_error;
    }

#endif /* __aarch64__ */

  /* Resume the process till the loader is executed.  */

  if (ptrace (PTRACE_SYSCALL, tracee->pid, NULL, NULL))
    {
      errno = EIO;
      goto free_data_error;
    }

  /* Now that the loader has been executed, record the value to
     substitute for /proc/self/exe.  Don't give up just because strdup
     fails.

     Note that exec_0 copies the absolute file name into buffer.  */

  if (tracee->exec_file)
    free (tracee->exec_file);
  tracee->exec_file = strdup (buffer);
  return 0;

 free_data_error:
  free (tracee->exec_data);
  tracee->exec_data = NULL;
  return 1;
}

/* Complete an `exec' system call issued by TRACEE.  Write the
   instructions stored in TRACEE->exec_data to an appropriate location
   in TRACEE's stack, and resume TRACEE, releasing TRACEE->exec_data.
   REGS should be the TRACEE's user registers.  If the reissued system
   call did not succeed in starting the executable loader, restore
   TRACEE->sp (recorded by process_system_call or seccomp_system_call),
   and resume execution, so that the failure may be reported.  */

static void
finish_exec (struct exec_tracee *tracee, USER_REGS_STRUCT *regs)
{
  USER_WORD size1, loader;
  USER_REGS_STRUCT original;

  size1 = tracee->data_size;

  /* Record the registers' values as they originally were.  */
  memcpy (&original, regs, sizeof *regs);

  /* Any non-zero value of `original.SYSCALL_RET_REG' indicates that the
     reissued `exec' call was unsuccessful, and the loader is not
     executing.  Restore the previous stack pointer and permit the
     tracee to run to completion.  */

  if (original.SYSCALL_RET_REG)
    {
      regs->STACK_POINTER = tracee->sp;
#ifdef __aarch64__
      aarch64_set_regs (tracee->pid, regs, false);
#else /* !__aarch64__ */
      ptrace (PTRACE_SETREGS, tracee->pid, NULL, regs);
#endif /* __aarch64__ */

      /* Continue; not much in the way of remediation is available if
	 either of PTRACE_SETREGS and this resumption fails.  */
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
	      tracee->pid, 0, 0);
      goto error;
    }

  /* Write the loader area to the stack, followed by its size and the
     original stack pointer.  */

  loader = user_alloca (tracee, &original, regs,
			size1 + sizeof loader * 2);
  if (!loader)
    goto error;

#ifndef STACK_GROWS_DOWNWARDS
  not implemented, you lose.
#else /* STACK_GROWS_DOWNWARDS */

  if (user_copy (tracee, (unsigned char *) tracee->exec_data,
		 loader + sizeof size1 * 2, size1)
      || user_copy (tracee, (unsigned char *) &size1,
		    loader + sizeof size1, sizeof size1))
    goto error;

  size1 = original.STACK_POINTER;

  if (user_copy (tracee, (unsigned char *) &size1,
		 loader, sizeof size1))
    goto error;

#endif /* STACK_GROWS_DOWNWARDS */

  /* Continue.  */
  if (ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
	      tracee->pid, 0, 0))
    goto error;

  /* Enable this block to debug the executable loader.  */
#if 0
  {
    int rc, wstatus;
  again1:
    rc = waitpid (tracee->pid, &wstatus, __WALL);
    if (rc == -1 && errno == EINTR)
      goto again1;
    ptrace (PTRACE_DETACH, tracee->pid, 0, 0);
  }
#endif /* 0 */

 error:
  free (tracee->exec_data);
  tracee->exec_data = NULL;
}



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



/* Modify BUFFER, of size SIZE, so that it holds the absolute name of
   the file identified by BUFFER, relative to the current working
   directory of TRACEE if FD be AT_FDCWD, or the file referenced by FD
   otherwise.

   Value is 1 if this information is unavailable (of which there are
   variety of causes), and 0 on success.  */

static int
canon_path (struct exec_tracee *tracee, int fd, char *buffer,
	    ptrdiff_t size)
{
  char link[sizeof "/proc//fd/" + 48], *p; /* Or /proc/pid/cwd.  */
  char target[PATH_MAX];
  ssize_t rc, length;

  if (buffer[0] == '/')
    /* Absolute file name; return immediately.  */
    return 0;
  else if (fd == AT_FDCWD)
    {
      p = stpcpy (link, "/proc/");
      p = format_pid (p, tracee->pid);
      stpcpy (p, "/cwd");
    }
  else if (fd < 0)
    /* Invalid file descriptor.  */
    return 1;
  else
    {
      p = stpcpy (link, "/proc/");
      p = format_pid (p, tracee->pid);
      p = stpcpy (p, "/fd/");
      format_pid (p, fd);
    }

  /* Read LINK's target, and should it be oversized, punt.  */
  rc = readlink (link, target, PATH_MAX);
  if (rc < 0 || rc >= PATH_MAX)
    return 1;

  /* Consider the amount by which BUFFER's existing contents should be
     displaced.  */

  length = strlen (buffer) + 1;
  if ((length + rc + (target[rc - 1] != '/')) > size)
    /* Punt if this would overflow.  */
    return 1;

  memmove ((buffer + rc + (target[rc - 1] != '/')),
	   buffer, length);

  /* Copy the new file name into BUFFER.  */
  memcpy (buffer, target, rc);

  /* Insert separator in between if need be.  */
  if (target[rc - 1] != '/')
    buffer[rc] = '/';

  return 0;
}

/* Handle a `readlink' or `readlinkat' system call.

   CALLNO is the system call number, and REGS are the current user
   registers of the TRACEE.

   If the file name specified in either a `readlink' or `readlinkat'
   system call is `/proc/self/exe', write the name of the executable
   being run into the buffer specified in the system call.  Do not
   handle relative file names at the moment.

   Return the number of bytes written to the tracee's buffer in
   *RESULT.

   Value is 0 upon success.  Value is 1 upon failure, and 2 if the
   system call has been emulated.  */

static int
handle_readlinkat (USER_WORD callno, USER_REGS_STRUCT *regs,
		   struct exec_tracee *tracee, USER_WORD *result)
{
  char buffer[PATH_MAX + 1];
  USER_WORD address, return_buffer, size;
  size_t length;
  char proc_pid_exe[sizeof "/proc//exe" + 24], *p;
  int dirfd;

  /* Read the file name.  */

#ifdef READLINK_SYSCALL
  if (callno == READLINK_SYSCALL)
    {
      dirfd	    = AT_FDCWD;
      address	    = regs->SYSCALL_ARG_REG;
      return_buffer = regs->SYSCALL_ARG1_REG;
      size	    = regs->SYSCALL_ARG2_REG;
    }
  else
#endif /* READLINK_SYSCALL */
    {
      dirfd	    = (USER_SWORD) regs->SYSCALL_ARG_REG;
      address	    = regs->SYSCALL_ARG1_REG;
      return_buffer = regs->SYSCALL_ARG2_REG;
      size	    = regs->SYSCALL_ARG3_REG;
    }

  read_memory (tracee, buffer, PATH_MAX, address);

  /* Make sure BUFFER is NULL terminated.  */

  if (!memchr (buffer, '\0', PATH_MAX))
    {
      errno = ENAMETOOLONG;
      return 1;
    }

  /* Expand BUFFER into an absolute file name.  TODO:
     AT_SYMLINK_FOLLOW? */

  if (canon_path (tracee, dirfd, buffer, sizeof buffer))
    return 0;

  /* Now check if the caller is looking for /proc/self/exe or its
     equivalent with the PID made explicit.

     dirfd can be ignored, as for now only absolute file names are
     handled.  FIXME.  */

  p = stpcpy (proc_pid_exe, "/proc/");
  p = format_pid (p, tracee->pid);
  stpcpy (p, "/exe");

  if ((strcmp (buffer, "/proc/self/exe")
       && strcmp (buffer, proc_pid_exe))
      || !tracee->exec_file)
    return 0;

  /* Copy over tracee->exec_file.  Truncate it to PATH_MAX, length, or
     size, whichever is smaller.  */

  length = strlen (tracee->exec_file);
  length = MIN (size, MIN (PATH_MAX, length));
  strncpy (buffer, tracee->exec_file, length);

  if (user_copy (tracee, (unsigned char *) buffer,
		 return_buffer, length))
    {
      errno = EIO;
      return 1;
    }

  *result = length;
  return 2;
}

/* Handle an `open' or `openat' system call.

   CALLNO is the system call number, and REGS are the current user
   registers of the TRACEE.

   If the file name specified in such system call is `/proc/self/exe',
   replace the file name with the executable loaded into the process
   issuing this system call.

   Value is 0 upon success and 1 upon failure.  */

static int
handle_openat (USER_WORD callno, USER_REGS_STRUCT *regs,
	       struct exec_tracee *tracee, USER_WORD *result)
{
  char buffer[PATH_MAX + 1];
  USER_WORD address;
  size_t length;
  USER_REGS_STRUCT original;
  char proc_pid_exe[sizeof "/proc//exe" + 24], *p;
  int dirfd;

  /* Read the file name.  */

#ifdef OPEN_SYSCALL
  if (callno == OPEN_SYSCALL)
    {
      dirfd   = AT_FDCWD;
      address = regs->SYSCALL_ARG_REG;
    }
  else
#endif /* OPEN_SYSCALL */
    {
      dirfd   = (USER_SWORD) regs->SYSCALL_ARG_REG;
      address = regs->SYSCALL_ARG1_REG;
    }

  /* Read the file name into the buffer and verify that it is NULL
     terminated.  */
  read_memory (tracee, buffer, PATH_MAX, address);

  if (!memchr (buffer, '\0', PATH_MAX))
    {
      errno = ENAMETOOLONG;
      return 1;
    }

  /* Expand BUFFER into an absolute file name.  TODO:
     AT_SYMLINK_FOLLOW? */

  if (canon_path (tracee, dirfd, buffer, sizeof buffer))
    return 0;

  /* Now check if the caller is looking for /proc/self/exe or its
     equivalent with the PID made explicit.  */

  p = stpcpy (proc_pid_exe, "/proc/");
  p = format_pid (p, tracee->pid);
  stpcpy (p, "/exe");

  if ((strcmp (buffer, "/proc/self/exe")
       && strcmp (buffer, proc_pid_exe))
      || !tracee->exec_file)
    return 0;

  /* Copy over tracee->exec_file.  This doesn't correctly handle the
     scenario where tracee->exec_file is longer than PATH_MAX, but
     that has yet to be encountered in practice.  */

  original = *regs;
  length   = strlen (tracee->exec_file);
  address  = user_alloca (tracee, &original, regs, length + 1);

  if (!address
      || user_copy (tracee, (unsigned char *) tracee->exec_file,
		    address, length + 1))
    goto fail;

  /* Replace the file name buffer with ADDRESS.  */

#ifdef OPEN_SYSCALL
  if (callno == OPEN_SYSCALL)
    regs->SYSCALL_ARG_REG = address;
  else
#endif /* OPEN_SYSCALL */
    regs->SYSCALL_ARG1_REG = address;

#ifdef __aarch64__
  if (aarch64_set_regs (tracee->pid, regs, false))
    goto fail;
#else /* !__aarch64__ */
  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL, regs))
    goto fail;
#endif /* __aarch64__ */

  /* Resume the system call.  */
  return 0;

 fail:
  errno = EIO;
  return 1;
}

/* Process the system call at which TRACEE is stopped.  If the system
   call is not known or not exec, send TRACEE on its way.  Otherwise,
   rewrite it to load the loader and perform an appropriate action.  */

static void
process_system_call (struct exec_tracee *tracee)
{
  USER_REGS_STRUCT regs;
  int rc, wstatus, save_errno;
  USER_WORD callno, sp;
  USER_WORD result;
  bool reporting_error;

#ifdef __aarch64__
  rc = aarch64_get_regs (tracee->pid, &regs);
#else /* !__aarch64__ */
  rc = ptrace (PTRACE_GETREGS, tracee->pid, NULL,
	       &regs);
#endif /* __aarch64__ */

  /* TODO: what to do if this fails? */
  if (rc < 0)
    return;

  /* Save the stack pointer.  */
  sp = regs.STACK_POINTER;

  /* Now dispatch based on the system call.  If TRACEE->exec_data is
     set, this must be exec, whatever the value of SYSCALL_NUM_REG,
     which is erased when exec loads another image.  */

  callno = (!tracee->exec_data
	    ? (!tracee->waiting_for_syscall
	       ? regs.SYSCALL_NUM_REG : tracee->callno)
	    : EXEC_SYSCALL);
  tracee->callno = callno;
  switch (callno)
    {
    case EXEC_SYSCALL:

      if (!tracee->waiting_for_syscall)
	{
	  /* The outstanding syscall flag must not be inconsistent with
	     the presence of instructions for the loader.  */
	  assert (!tracee->exec_data);
	  rc = handle_exec (tracee, &regs);

	  if (rc)
	    /* An error has occurred; errno is set to the error.  */
	    goto report_syscall_error;

	  /* The process has been resumed.  Assert that the instructions
	     for loading this executable have been generated and
	     recorded, and set waiting_for_syscall.  */
	  tracee->waiting_for_syscall = true;
	  assert (tracee->exec_data);

	  /* Record the initial stack pointer also.  */
	  tracee->sp = sp;
	}
      else
	{
	  assert (tracee->exec_data);
	  finish_exec (tracee, &regs);

	  /* The process has been resumed and has become capable of
	     executing independently.  */
	  tracee->waiting_for_syscall = false;
	}

      break;

#ifdef READLINK_SYSCALL
    case READLINK_SYSCALL:
#endif /* READLINK_SYSCALL */
    case READLINKAT_SYSCALL:

      /* This system call is already in progress if
	 TRACEE->waiting_for_syscall is true.  */

      if (!tracee->waiting_for_syscall)
	{
	  /* Handle this readlinkat system call.  */
	  rc = handle_readlinkat (callno, &regs, tracee,
				  &result);

	  /* rc means the same as in `handle_exec'.  */

	  if (rc == 1)
	    goto report_syscall_error;
	  else if (rc == 2)
	    goto emulate_syscall;
	}

      goto continue_syscall;

#ifdef OPEN_SYSCALL
    case OPEN_SYSCALL:
#endif /* OPEN_SYSCALL */
    case OPENAT_SYSCALL:

      /* This system call is already in progress if
	 TRACEE->waiting_for_syscall is true.  */

      if (!tracee->waiting_for_syscall)
	{
	  /* Handle this open system call.  */
	  rc = handle_openat (callno, &regs, tracee, &result);

	  /* rc means the same as in `handle_exec', except that `open'
	     is never emulated.  */

	  if (rc == 1)
	    goto report_syscall_error;

	  /* The stack pointer must be restored after it was modified
	     by `user_alloca'; record sp in TRACEE, which will be
	     restored after this system call completes.  */
	  tracee->sp = sp;
	}
      else
	{
	  /* Restore that stack pointer.  */
	  regs.STACK_POINTER = tracee->sp;

#ifdef __aarch64__
	  if (aarch64_set_regs (tracee->pid, &regs, false))
	    return;
#else /* !__aarch64__ */
	  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs))
	    return;
#endif /* __aarch64__ */
	}

      /* Fallthrough.  */

    default:
    continue_syscall:
      /* Don't wait for the system call to finish; instead, the system
	 will DTRT upon the next call to PTRACE_SYSCALL after the
	 syscall-trap signal is delivered.  */

      rc = ptrace (((use_seccomp_p
		     /* open and openat are not processed synchronously,
			nor can they afford to dispense with
			post-syscall finalization.  */

		     && ((callno != OPENAT_SYSCALL
#ifdef OPEN_SYSCALL
			  && callno != OPEN_SYSCALL
#endif /* OPEN_SYSCALL */
			  )
			 /* Since syscall initialization should be
			    reserved for seccomp_system_call, resume the
			    process if this system call is already
			    complete.  */
			 || !tracee->waiting_for_syscall))
		     ? PTRACE_CONT : PTRACE_SYSCALL), tracee->pid,
		   NULL, NULL);
      if (rc < 0)
	return;

#ifdef HAVE_SECCOMP
      if (!(use_seccomp_p
	    && ((callno != OPENAT_SYSCALL
#ifdef OPEN_SYSCALL
		 && callno != OPEN_SYSCALL
#endif /* OPEN_SYSCALL */
		 )
		|| !tracee->waiting_for_syscall)))
#endif /* !HAVE_SECCOMP */
      tracee->waiting_for_syscall = !tracee->waiting_for_syscall;
    }

  return;

 report_syscall_error:
  reporting_error = true;
  goto common;

 emulate_syscall:
  reporting_error = false;
 common:

  /* Reporting an error or emulating a system call works by replacing
     the system call number with -1 or another nonexistent syscall,
     letting it continue, and then substituting errno for ENOSYS in the
     case of an error.

     Make sure that the stack pointer is restored to its original
     position upon exit, or bad things can happen.  */

  /* First, save errno; system calls below will clobber it.  */
  save_errno = errno;

  regs.SYSCALL_NUM_REG = -1;
  regs.STACK_POINTER   = sp;

#ifdef __aarch64__
  if (aarch64_set_regs (tracee->pid, &regs, true))
    return;
#else /* !__aarch64__ */

#ifdef __arm__
  /* On ARM systems, a special request is used to update the system
     call number as known to the kernel.  In addition, the system call
     number must be valid, so use `tuxcall'.  Hopefully, nobody will
     run this on a kernel with Tux.  */

  if (ptrace (PTRACE_SET_SYSCALL, tracee->pid, NULL, 222))
    return;
#endif /* __arm__ */

  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs))
    return;
#endif /* __aarch64__ */

  /* Do this invalid system call.  */
  if (ptrace (PTRACE_SYSCALL, tracee->pid, NULL, NULL))
    return;

 again1:
  rc = waitpid (tracee->pid, &wstatus, __WALL);
  if (rc == -1 && errno == EINTR)
    goto again1;

  /* Return if waitpid fails.  */

  if (rc == -1)
    return;

  /* If the process received a signal, see if the signal is SIGSYS and
     from seccomp.  If so, discard it.  */

  if (WIFSTOPPED (wstatus))
    {
      rc = check_signal (tracee, wstatus);

      if (rc == -1)
	return;
      else if (rc)
	goto again1;
    }

  if (!WIFSTOPPED (wstatus))
    /* The process has been killed in response to a signal.  In this
       case, simply unlink the tracee and return.  */
    remove_tracee (tracee);
  else if (reporting_error)
    {
#ifdef __mips__
      /* MIPS systems place errno in v0 and set a3 to 1.  */
      regs.gregs[2] = save_errno;
      regs.gregs[7] = 1;
#else /* !__mips__ */
      regs.SYSCALL_RET_REG = -save_errno;
#endif /* __mips__ */

      /* Report errno.  */
#ifdef __aarch64__
      aarch64_set_regs (tracee->pid, &regs, false);
#else /* !__aarch64__ */
      ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs);
#endif /* __aarch64__ */

      /* Now wait for the next system call to happen.  */
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
	      tracee->pid, NULL, NULL);
    }
  else
    {
      /* No error is being reported.  Return the result in the
	 appropriate registers.  */
#ifdef __mips__
      /* MIPS systems place errno in v0 and set a3 to 1.  */
      regs.gregs[2] = result;
      regs.gregs[7] = 0;
#else /* !__mips__ */
      regs.SYSCALL_RET_REG = result;
#endif /* __mips__ */

      /* Report errno.  */
#ifdef __aarch64__
      aarch64_set_regs (tracee->pid, &regs, false);
#else /* !__aarch64__ */
      ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs);
#endif /* __aarch64__ */

      /* Now wait for the next system call to happen.  */
      ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
	      tracee->pid, NULL, NULL);
    }
}



#ifdef HAVE_SECCOMP

/* Seccomp acceleration.

   Seccomp enables selectively filtering signals so that the tracing
   process is only notified of such system calls as it is interested in
   intercepting, i.e., exec and open*.  This improves performance
   enormously over the traditional approach of pausing the tracee before
   each system call.  */

/* Whether the kernel's version is 4.7.x or earlier.  */
static bool kernel_4_7_or_earlier;

/* Array of system calls this module is interested in intercepting.  */
static int interesting_syscalls[] =
  {
    EXEC_SYSCALL,
#ifdef OPEN_SYSCALL
    OPEN_SYSCALL,
#endif /* OPEN_SYSCALL */
    OPENAT_SYSCALL,
#ifdef READLINK_SYSCALL
    READLINK_SYSCALL,
#endif /* READLINK_SYSCALL */
    READLINKAT_SYSCALL,
  };

/* Number of elements in an array.  */
#define ARRAYELTS(arr) (sizeof (arr) / sizeof (arr)[0])

/* Install a secure computing filter that will notify attached tracers
   when a system call of interest to this module is received.  Value is
   0 if successful, 1 otherwise.  */

static int
establish_seccomp_filter (void)
{
  struct sock_filter statements[1 + ARRAYELTS (interesting_syscalls) + 2];
  struct sock_fprog program;
  int index, rc;

  index = 0;

  /* As the exec wrapper will reject executables for an inappropriate
     architecture, verifying the same here would only be redundant.
     Proceed to load the current system call number.  */

  statements[index++] = ((struct sock_filter)
			 BPF_STMT (BPF_LD + BPF_W + BPF_ABS,
				   offsetof (struct seccomp_data, nr)));

  /* Search for system calls of interest.  */

  statements[index]
    = ((struct sock_filter)
       BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, EXEC_SYSCALL,
		 ARRAYELTS (interesting_syscalls), 0)); index++;
#ifdef OPEN_SYSCALL
  statements[index]
    = ((struct sock_filter)
       BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, OPEN_SYSCALL,
		 ARRAYELTS (interesting_syscalls) - index + 1, 0)); index++;
#endif /* OPEN_SYSCALL */
  statements[index]
    = ((struct sock_filter)
       BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, OPENAT_SYSCALL,
		 ARRAYELTS (interesting_syscalls) - index + 1, 0)); index++;
#ifdef READLINK_SYSCALL
  statements[index]
    = ((struct sock_filter)
       BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, READLINK_SYSCALL,
		 ARRAYELTS (interesting_syscalls) - index + 1, 0)); index++;
#endif /* READLINK_SYSCALL */
  statements[index]
    = ((struct sock_filter)
       BPF_JUMP (BPF_JMP + BPF_JEQ + BPF_K, READLINKAT_SYSCALL,
		 ARRAYELTS (interesting_syscalls) - index + 1, 0)); index++;

  /* If not intercepted above, permit this system call to execute as
     normal.  */
  statements[index++]
    = (struct sock_filter) BPF_STMT (BPF_RET + BPF_K, SECCOMP_RET_ALLOW);
  statements[index++]
    = (struct sock_filter) BPF_STMT (BPF_RET + BPF_K, SECCOMP_RET_TRACE);

  rc = prctl (PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0);
  if (rc)
    return 1;

  program.len = ARRAYELTS (statements);
  program.filter = statements;
  rc = prctl (PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &program);
  if (rc)
    return 1;

  return 0;
}

/* Intercept or resume and dismiss the system call at which TRACEE is
   paused, similarly to process_system_call.  */

static void
seccomp_system_call (struct exec_tracee *tracee)
{
  USER_REGS_STRUCT regs;
  int rc, wstatus, save_errno;
  USER_WORD callno, sp;
  USER_WORD result;
  bool reporting_error;

  if (kernel_4_7_or_earlier)
    {
      /* On kernel 4.7 and earlier, following a PTRACE_EVENT_SECCOMP by
	 a PTRACE_SYSCALL will give rise to a syscall-entry stop event,
	 and seccomp filters will be suppressed till the system call
	 runs its course.  */
      ptrace (PTRACE_SYSCALL, tracee->pid, 0, 0);
      return;
    }

#ifdef __aarch64__
  rc = aarch64_get_regs (tracee->pid, &regs);
#else /* !__aarch64__ */
  rc = ptrace (PTRACE_GETREGS, tracee->pid, NULL,
	       &regs);
#endif /* __aarch64__ */

  /* TODO: what to do if this fails? */
  if (rc < 0)
    return;

  /* On kernel 4.8, processes resumed after being paused so as to
     produce a PTRACE_EVENT_SECCOMP will execute till the system call
     completes, or indefinitely if resumed with PTRACE_CONT.

     In this context processes are resumed with PTRACE_CONT unless it is
     an `open' syscall that is being intercepted, which, if successfully
     intercepted, they must receive adjustments to their stack pointer
     upon completion of said system call.  */
  assert (!tracee->waiting_for_syscall);

  /* Save the stack pointer.  */
  sp = regs.STACK_POINTER;

  /* Now dispatch based on the system call.  */
  callno = regs.SYSCALL_NUM_REG;

  /* Record the call number, which may be required if one of the
     following handlers should arrange for process_system_call to
     intercede after the system call completes.  */
  tracee->callno = callno;
  switch (callno)
    {
    case EXEC_SYSCALL:
      assert (!tracee->exec_data);
      rc = handle_exec (tracee, &regs);

      if (rc)
	/* An error has occurred; errno is set to the error.  */
	goto report_syscall_error;

      /* The process has been resumed.  Assert that the instructions for
	 loading this executable have been generated and recorded, and
	 set waiting_for_syscall.  */
      tracee->waiting_for_syscall = true;
      assert (tracee->exec_data);

      /* Record the initial stack pointer also.  */
      tracee->sp = sp;
      break;

#ifdef READLINK_SYSCALL
    case READLINK_SYSCALL:
#endif /* READLINK_SYSCALL */
    case READLINKAT_SYSCALL:
      /* Handle this readlinkat system call.  */
      rc = handle_readlinkat (callno, &regs, tracee,
			      &result);

      /* rc means the same as in `handle_exec'.  */

      if (rc == 1)
	goto report_syscall_error;
      else if (rc == 2)
	goto emulate_syscall;

      goto continue_syscall;

#ifdef OPEN_SYSCALL
    case OPEN_SYSCALL:
#endif /* OPEN_SYSCALL */
    case OPENAT_SYSCALL:
      /* Handle this open system call.  */
      rc = handle_openat (callno, &regs, tracee, &result);

      /* rc means the same as in `handle_exec', except that `open'
	 is never emulated.  */

      if (rc == 1)
	goto report_syscall_error;

      /* The stack pointer must be restored after it was modified
	 by `user_alloca'; record sp in TRACEE, which will be
	 restored after this system call completes.  */
      tracee->sp = sp;

      /* As such, arrange to enter `process_system_call' on its
	 completion.  */
      rc = ptrace (PTRACE_SYSCALL, tracee->pid,
		   NULL, NULL);
      if (rc < 0)
	return;

      tracee->waiting_for_syscall = true;
      break;

    default:
    continue_syscall:
      rc = ptrace (PTRACE_CONT, tracee->pid, NULL, NULL);
      if (rc < 0)
	return;
    }

  return;

 report_syscall_error:
  reporting_error = true;
  goto common;

 emulate_syscall:
  reporting_error = false;
 common:

  /* Reporting an error or emulating a system call works by replacing
     the system call number with -1 or another nonexistent syscall,
     letting it continue, and then substituting errno for ENOSYS in the
     case of an error.

     Make sure that the stack pointer is restored to its original
     position upon exit, or bad things can happen.  */

  /* First, save errno; system calls below will clobber it.  */
  save_errno = errno;

  regs.SYSCALL_NUM_REG = -1;
  regs.STACK_POINTER   = sp;

#ifdef __aarch64__
  if (aarch64_set_regs (tracee->pid, &regs, true))
    return;
#else /* !__aarch64__ */

#ifdef __arm__
  /* On ARM systems, a special request is used to update the system
     call number as known to the kernel.  In addition, the system call
     number must be valid, so use `tuxcall'.  Hopefully, nobody will
     run this on a kernel with Tux.  */

  if (ptrace (PTRACE_SET_SYSCALL, tracee->pid, NULL, 222))
    return;
#endif /* __arm__ */

  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs))
    return;
#endif /* __aarch64__ */

  /* Do this invalid system call.  */
  if (ptrace (PTRACE_SYSCALL, tracee->pid, NULL, NULL))
    return;

 again1:
  rc = waitpid (tracee->pid, &wstatus, __WALL);
  if (rc == -1 && errno == EINTR)
    goto again1;

  /* Return if waitpid fails.  */

  if (rc == -1)
    return;

  /* If the process received a signal, see if the signal is SIGSYS
     and/or from seccomp.  If so, discard it.  */

  if (WIFSTOPPED (wstatus))
    {
      rc = check_signal (tracee, wstatus);

      if (rc == -1)
	return;
      else if (rc)
	goto again1;
    }

  if (!WIFSTOPPED (wstatus))
    /* The process has been killed in response to a signal.  In this
       case, simply unlink the tracee and return.  */
    remove_tracee (tracee);
  else if (reporting_error)
    {
#ifdef __mips__
      /* MIPS systems place errno in v0 and set a3 to 1.  */
      regs.gregs[2] = save_errno;
      regs.gregs[7] = 1;
#else /* !__mips__ */
      regs.SYSCALL_RET_REG = -save_errno;
#endif /* __mips__ */

      /* Report errno.  */
#ifdef __aarch64__
      aarch64_set_regs (tracee->pid, &regs, false);
#else /* !__aarch64__ */
      ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs);
#endif /* __aarch64__ */

      /* Resume the process till the next interception by its filter.  */
      ptrace (PTRACE_CONT, tracee->pid, NULL, NULL);
    }
  else
    {
      /* No error is being reported.  Return the result in the
	 appropriate registers.  */

#ifdef __mips__
      /* MIPS systems place errno in v0 and set a3 to 1.  */
      regs.gregs[2] = result;
      regs.gregs[7] = 0;
#else /* !__mips__ */
      regs.SYSCALL_RET_REG = result;
#endif /* __mips__ */

      /* Report errno.  */
#ifdef __aarch64__
      aarch64_set_regs (tracee->pid, &regs, false);
#else /* !__aarch64__ */
      ptrace (PTRACE_SETREGS, tracee->pid, NULL, &regs);
#endif /* __aarch64__ */

      /* Resume the process till the next interception by its filter.  */
      ptrace (PTRACE_CONT, tracee->pid, NULL, NULL);
    }
}

#ifndef PTRACE_EVENT_SECCOMP
#define PTRACE_EVENT_SECCOMP 7
#endif /* !PTRACE_EVENT_SECCOMP */

#ifndef PTRACE_O_TRACESECCOMP
#define PTRACE_O_TRACESECCOMP (1 << PTRACE_EVENT_SECCOMP)
#endif /* !PTRACE_O_TRACESECCOMP */

#ifndef SIGSYS
#define SIGSYS 31
#endif /* !SIGSYS */
#endif /* HAVE_SECCOMP */



/* Like `execve', but asks the parent to begin tracing this thread.
   Fail by returning a non-zero value if tracing is unsuccessful.  */

int
tracing_execve (const char *file, char *const *argv,
		char *const *envp)
{
  int rc;

  /* Start tracing self.  */
  rc = ptrace (PTRACE_TRACEME, 0, NULL, NULL);
  if (rc)
    return rc;

  /* Notify the parent to enter signal-delivery-stop.  */
  raise (SIGSTOP);

#ifdef HAVE_SECCOMP
  /* Install the seccomp filter.  */
  if (use_seccomp_p && establish_seccomp_filter ())
    return 1;
#endif /* HAVE_SECCOMP */

  return execve (file, argv, envp);
}

/* Wait for PID to trace itself, and make a record of that process.
   Value is 1 or 2 upon failure, 0 otherwise.  Make sure that SIGCHLD
   is blocked around calls to this function.

   If failure occurs because PID exited, value is 2; upon any other
   kind of failure, value is 1.  */

int
after_fork (pid_t pid)
{
  int wstatus, rc, flags;
  struct exec_tracee *tracee;
#if defined HAVE_SECCOMP && __ANDROID__
  int statusarg;
  USER_REGS_STRUCT regs;
#endif /* defined HAVE_SECCOMP && __ANDROID__ */

  /* First, wait for something to happen to PID.  */
 again:
  rc = waitpid (pid, &wstatus, __WALL);
  if (rc != pid && errno == EINTR)
    goto again;

  if (rc != pid)
    return 1;

  /* If the child exited (or in general wasn't traced), return 2.  */

  if (!WIFSTOPPED (wstatus))
    return 2;

  /* Apply required options to the child, so that the kernel
     automatically traces children and makes it easy to differentiate
     between system call traps and other kinds of traps.  */

  flags  = PTRACE_O_TRACECLONE;
  flags |= PTRACE_O_TRACEVFORK;
  flags |= PTRACE_O_TRACEFORK;
  flags |= PTRACE_O_TRACESYSGOOD;
  flags |= PTRACE_O_TRACEEXIT;
#ifdef HAVE_SECCOMP
  if (use_seccomp_p)
    flags |= PTRACE_O_TRACESECCOMP;
#endif /* HAVE_SECCOMP */

  rc = ptrace (PTRACE_SETOPTIONS, pid, 0, flags);

  if (rc)
    {
      /* If the kernel can't trace child processes upon creation and
	 exit, then it can't work reliably.  */
      ptrace (PTRACE_DETACH, pid, 0, 0);
      return 1;
    }

#if defined HAVE_SECCOMP && __ANDROID__
  /* Certain Android kernels have received backports of those new
     PTRACE_EVENT_SECCOMP semantics which were introduced in kernel
     version 4.8, so that it is necessary to actively establish which
     variant is in place.  */

  if (kernel_4_7_or_earlier && use_seccomp_p)
    {
      /* Request that the child stop upon the next `exec' system call,
	 one of which is assumed to always be issued by the child, as
	 below, but await the next stop, and examine its contents.
	 Anciently, the syscall-stop preceded events marked
	 PTRACE_EVENT_SECCOMP, whereas this sequence is reversed in
	 4.8+, and in releases with these changes backported.  */

      rc = ptrace (PTRACE_SYSCALL, pid, 0, 0);
      if (rc)
	return 1;

      while (true)
	{
	  rc = waitpid (pid, &wstatus, __WALL);
	  if (rc != pid)
	    return 1;

	  if (WIFSTOPPED (wstatus))
	    {
	      /* Verify that this system call is `exec', not one issued
		 between PTRACE_TRACEME and `exec' intercepted by
		 PTRACE_SYSCALL.  */
#ifdef __aarch64__
	      rc = aarch64_get_regs (pid, &regs);
#else /* !__aarch64__ */
	      rc = ptrace (PTRACE_GETREGS, pid, NULL, &regs);
#endif /* __aarch64__ */
	      if (rc)
		return 1;

	      if (regs.SYSCALL_NUM_REG == EXEC_SYSCALL)
		{
		  statusarg = ((wstatus & 0xfff00) >> 8);

		  if (statusarg == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8)))
		    {
		      /* The first event to be delivered is a seccomp
			 stop, indicating that this is an unmodified
			 <4.7 kernel.  Return to await the subsequent
			 syscall-stop, which should be received and
			 acted on by process_system_call.  */
		      rc = ptrace (PTRACE_SYSCALL, pid, 0, 0);
		      break;
		    }
		  else if (statusarg == (SIGTRAP | 0x80))
		    {
		      /* Syscall-traps take priority.  This is a
			 doctored 4.7 kernel.  */
		      kernel_4_7_or_earlier = false;
		      rc = ptrace (PTRACE_CONT, pid, 0, 0);
		      break;
		    }
		}

	      rc = ptrace (PTRACE_SYSCALL, pid, 0, 0);
	      if (rc)
		return 1;
	    }
	  else
	    return 1;
	}
    }
  else
#endif /* HAVE_SECCOMP && __ANDROID__ */
    /* Request that the child stop upon the next system call, or the
       next filter event.  */
    rc = ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		 pid, 0, 0);
  if (rc)
    return 1;

  /* Enroll the child into `tracing_processes'.  */

  if (free_tracees)
    {
      tracee = free_tracees;
      free_tracees = free_tracees->next;
    }
  else
    tracee = malloc (sizeof *tracee);

  if (!tracee)
    return 1;

  tracee->pid = pid;
  tracee->callno = 0;
  tracee->next = tracing_processes;
  tracee->waiting_for_syscall = false;
  tracee->new_child = false;
  tracee->exec_file = NULL;
  tracee->exec_data = NULL;
  tracee->data_size = 0;
  tracing_processes = tracee;
  return 0;
}

/* Wait for a child process to exit, like `waitpid'.  However, if a
   child stops to perform a system call, send it on its way and return
   -1.  OPTIONS must not contain WUNTRACED.  */

pid_t
exec_waitpid (pid_t pid, int *wstatus, int options)
{
  int status;
  struct exec_tracee *tracee;
  siginfo_t siginfo;

  pid = waitpid (pid, &status, options | __WALL);
  if (pid < 0)
    return pid;

  /* Copy status into *WSTATUS if specified.  */
  if (wstatus)
    *wstatus = status;

  /* WIFSTOPPED (status) means that the process has been stopped in
     response to a system call.  Find its tracee and process the
     system call.  */

  if (WIFSTOPPED (status))
    {
      tracee = find_tracee (pid);

      if (!tracee || tracee->new_child)
	{
	  if (WSTOPSIG (status) == SIGSTOP)
	    /* A new process has been created and stopped.  Record
	       it now.  */
	    handle_clone (tracee, pid);

	  return -1;
	}

      /* Now extract the stop signal, including ptrace event bits.  */
      status &= 0xfff00;
      status = status >> 8;

      switch (status)
	{
	case SIGTRAP:
	  /* Now, use PTRACE_GETSIGINFO to determine whether or not the
	     signal was delivered in response to a system call.  */

	  if (ptrace (PTRACE_GETSIGINFO, pid, 0, &siginfo))
	    return -1;

	  if (!syscall_trap_p (&siginfo))
	    {
	      if (siginfo.si_code < 0)
		/* SIGTRAP delivered from userspace.  Pass it on.  */
		ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
			pid, 0, SIGTRAP);
	      else
		ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
			pid, 0, 0);

	      return -1;
	    }

	case SIGTRAP | 0x80: /* SIGTRAP | 0x80 specifically refers to
				system call traps.  */
	  /* Otherwise, process the system call and continue waiting.  */
	  process_system_call (tracee);
	  return -1;

	case SIGTRAP | (PTRACE_EVENT_EXIT << 8):
	  /* The tracee has exited.  Make it finish correctly.  */
	  ptrace (PTRACE_SYSCALL, pid, 0, 0);
	  remove_tracee (tracee);
	  return -1;

	case SIGTRAP | (PTRACE_EVENT_FORK << 8):
	case SIGTRAP | (PTRACE_EVENT_VFORK << 8):
	case SIGTRAP | (PTRACE_EVENT_CLONE << 8):

	  /* Both PTRACE_EVENT_CLONE and SIGSTOP must arrive before a
	     process is continued.  Otherwise, its parent's cmdline
	     cannot be obtained and propagated.

	     If the PID of the new process is currently not being
	     traced, create a new tracee.  Set `new_child' to true,
	     and copy over the old command line in preparation for a
	     SIGSTOP signal being delivered to it.

	     Otherwise, start the tracee running until the next
	     syscall.  */

	  handle_clone_prepare (tracee);

	  /* These events are handled by tracing SIGSTOP signals sent
	     to unknown tracees.  Make sure not to pass through
	     status, as there's no signal really being delivered.  */
	  ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL), pid, 0, 0);
	  return -1;

#ifdef HAVE_SECCOMP
	case SIGTRAP | (PTRACE_EVENT_SECCOMP << 8):
	  /* Intercept and process this system call if the event was
	     produced by our filter.  */
	  seccomp_system_call (tracee);
	  return -1;
#endif /* HAVE_SECCOMP */

#ifdef SIGSYS
	case SIGSYS:
	  if (ptrace (PTRACE_GETSIGINFO, pid, 0, &siginfo))
	    return -1;

	  /* Continue the process until the next syscall, but don't
	     pass through the signal if an emulated syscall led to
	     it.  */
#ifdef HAVE_SIGINFO_T_SI_SYSCALL
#ifndef __arm__
	  ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		  pid, 0, ((siginfo.si_code == SYS_SECCOMP
			    && siginfo.si_syscall == -1)
			   ? 0 : status));
#else /* __arm__ */
	  ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		  pid, 0, ((siginfo.si_code == SYS_SECCOMP
			    && siginfo.si_syscall == 222)
			   ? 0 : status));
#endif /* !__arm__ */
#else /* !HAVE_SIGINFO_T_SI_SYSCALL */
	  /* Drop this signal, since what caused it is unknown.  */
	  ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL), pid,
		  0, 0);
#endif /* HAVE_SIGINFO_T_SI_SYSCALL */
	  return -1;
#endif /* SIGSYS */

	default:
	  /* Resume the process as appropriate.  */
	  ptrace ((use_seccomp_p ? PTRACE_CONT : PTRACE_SYSCALL),
		  pid, 0, status);
	  return -1;
	}
    }
  else
    {
      /* The process has exited.  Unlink the associated tracee.  */
      tracee = find_tracee (pid);

      if (tracee)
	remove_tracee (tracee);

      return pid;
    }
}



/* Initialize the exec library.  LOADER should be the file name of the
   loader binary; it is not copied.  */

void
exec_init (const char *loader)
{
#ifdef HAVE_SECCOMP
  struct utsname u;
  int major, minor;
#endif /* HAVE_SECCOMP */

  loader_name = loader;
#ifdef HAVE_SECCOMP
  errno = 0;
  prctl (PR_GET_SECCOMP);

  /* PR_GET_SECCOMP should not set errno if the kernel was configured
     with support for seccomp.  */
  if (!errno)
    use_seccomp_p = true;
  else
    return;

  /* Establish whether the kernel is 4.7.x or older.  */
  uname (&u);
  if ((sscanf (u.release, "%d.%d", &major, &minor) == 2))
    {
      /* Certain required ptrace features were introduced in kernel
	 3.5.  */
      if (major < 3 || (major == 3 && minor < 5))
	use_seccomp_p = false;
      else
	{
	  if (major < 4 || (major == 4 && minor <= 7))
	    kernel_4_7_or_earlier = true;
	}
    }
#endif /* HAVE_SECCOMP */
#if defined HAVE_SYS_UIO_H && !defined HAVE_PROCESS_VM
  {
    *(void **) (&process_vm_readv)
      = dlsym (RTLD_DEFAULT, "process_vm_readv");
    *(void **) (&process_vm_writev)
      = dlsym (RTLD_DEFAULT, "process_vm_writev");
  }
#endif /* HAVE_SYS_UIO_H && !HAVE_PROCESS_VM */
}
