/* Program execution for Emacs.

Copyright (C) 2023 Free Software Foundation, Inc.

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

#include "exec.h"

#include SYSCALL_HEADER
#include USER_HEADER

#ifdef __aarch64__
#include <sys/uio.h> /* for struct iovec */
#include <linux/elf.h> /* for NT_* */
#endif /* __aarch64__ */

#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h> /* for process_vm_readv */
#endif /* HAVE_SYS_UIO_H */



/* Program tracing functions.

   The main entry point is the function `tracing_execve', which traces
   the thread and calls exec.  Each time that thread calls `clone',
   the new child is traced as well.

   Instead of calling `waitpid', call `exec_waitpid' instead.  */



/* Number of tracees children are allowed to create.  */
#define MAX_TRACEES 1024

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

  rc = ptrace (PTRACE_SETREGSET, pid, NT_PRSTATUS,
	       &iov);
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
   BUFFER after the first such page becomes undefined.  */

static void
read_memory (struct exec_tracee *tracee, char *buffer,
	     USER_WORD n, USER_WORD address)
{
  USER_WORD word, n_words, n_bytes, i;
  long rc;
#ifdef HAVE_PROCESS_VM
  struct iovec iov, remote;

  /* If `process_vm_readv' is available, use it instead.  */

  iov.iov_base = buffer;
  iov.iov_len = n;
  remote.iov_base = (void *) address;
  remote.iov_len = n;

  /* Return immediately if successful.  As long as some bytes were
     read, consider the read to have been a success.  */

  if (n <= SSIZE_MAX
      && ((size_t) process_vm_readv (tracee->pid, &iov, 1,
				     &remote, 1, 0) != -1))
    return;

#endif /* HAVE_PROCESS_VM */

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
#ifdef HAVE_PROCESS_VM
  struct iovec iov, remote;

  /* Try to use `process_vm_writev' if possible, but fall back to
     ptrace if something bad happens.  */

  iov.iov_base = (void *) buffer;
  iov.iov_len = n;
  remote.iov_base = (void *) address;
  remote.iov_len = n;

  if (n <= SSIZE_MAX
      && ((size_t) process_vm_writev (tracee->pid, &iov, 1,
				      &remote, 1, 0) == n))
    return 0;
#endif /* HAVE_PROCESS_VM */

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

#ifndef REENTRANT
	  /* Free the exec file, if any.  */
	  free (tracee->exec_file);
	  tracee->exec_file = NULL;
#endif /* REENTRANT */

	  free_tracees = tracee;

	  return;
	}
      else
	last = &(*last)->next;
    }
}



/* Child process tracing.  */

/* Handle the completion of a `clone' or `clone3' system call,
   resulting in the creation of the process PID.  Allocate a new
   tracee structure from a static area for the processes's pid.

   Value is 0 upon success, 1 otherwise.  */

static int
handle_clone (pid_t pid)
{
  static struct exec_tracee static_tracees[MAX_TRACEES];
  static int tracees;
  struct exec_tracee *tracee;
  long rc;
  int flags;

  /* Now allocate a new tracee, either from static_tracees or the free
     list.  */

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
  else
    return 1;

  tracee->pid = pid;
  tracee->next = tracing_processes;
  tracee->waiting_for_syscall = false;
  tracing_processes = tracee;

  /* Apply required options to the child, so that the kernel
     automatically traces children and makes it easy to differentiate
     between system call traps and other kinds of traps.  */

  flags  = PTRACE_O_TRACECLONE;
  flags |= PTRACE_O_TRACEVFORK;
  flags |= PTRACE_O_TRACEFORK;
  flags |= PTRACE_O_TRACESYSGOOD;
  flags |= PTRACE_O_TRACEEXIT;

  rc = ptrace (PTRACE_SETOPTIONS, pid, 0, flags);

  if (rc)
    goto bail;

  /* The new tracee is currently stopped.  Continue it until the next
     system call.  */

  rc = ptrace (PTRACE_SYSCALL, pid, 0, 0);

  if (rc)
    goto bail;

  return 0;

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
	    ptrace (PTRACE_SYSCALL, tracee->pid, 0, SIGTRAP);
	  else
	    ptrace (PTRACE_SYSCALL, tracee->pid, 0, 0);

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
      ptrace (PTRACE_SYSCALL, tracee->pid,
	      0, ((siginfo.si_code == SYS_SECCOMP
		   && siginfo.si_syscall == -1)
		  ? 0 : status));
#else /* __arm__ */
      ptrace (PTRACE_SYSCALL, tracee->pid,
	      0, ((siginfo.si_code == SYS_SECCOMP
		   && siginfo.si_syscall == 222)
		  ? 0 : status));
#endif /* !__arm__ */
#else /* !HAVE_SIGINFO_T_SI_SYSCALL */
      /* Drop this signal, since what caused it is unknown.  */
      ptrace (PTRACE_SYSCALL, tracee->pid, 0, 0);
#endif /* HAVE_SIGINFO_T_SI_SYSCALL */
      return 1;
#endif /* SIGSYS */

    default:
      /* Continue the process until the next syscall.  */
      ptrace (PTRACE_SYSCALL, tracee->pid, 0, status);
      return 1;
    }

  return 0;
}



/* Handle an `exec' system call from the given TRACEE.  REGS are the
   tracee's current user-mode registers.

   Rewrite the system call arguments to use the loader binary.  Then,
   continue the system call until the loader is loaded.  Write the
   information necessary to load the original executable into the
   loader's stack.

   Value is 0 upon success, 1 upon a generic failure before the loader
   is loaded, 2 if the process has stopped, and 3 if something failed,
   but it is too late to handle it.

   Set errno appropriately upon returning a generic failure.  */

static int
handle_exec (struct exec_tracee *tracee, USER_REGS_STRUCT *regs)
{
  char buffer[PATH_MAX + 80], *area;
  USER_REGS_STRUCT original;
  size_t size, loader_size;
  USER_WORD loader, size1, sp;
  int rc, wstatus;
  siginfo_t siginfo;

  /* Save the old stack pointer.  */
  sp = regs->STACK_POINTER;

  /* Read the file name.  */
  read_memory (tracee, buffer, PATH_MAX,
	       regs->SYSCALL_ARG_REG);

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

  /* Rewrite the first argument to point to the loader.  */

  loader_size = strlen (loader_name) + 1;
  loader = user_alloca (tracee, &original, regs,
			loader_size);

  if (!loader)
    {
      errno = ENOMEM;
      return 1;
    }

  if (user_copy (tracee, (unsigned char *) loader_name,
		 loader, loader_size))
    {
      errno = EIO;
      return 1;
    }

  regs->SYSCALL_ARG_REG = loader;

#ifdef __aarch64__

  if (aarch64_set_regs (tracee->pid, regs, false))
    {
      errno = EIO;
      return 1;
    }

#else /* !__aarch64__ */

  if (ptrace (PTRACE_SETREGS, tracee->pid, NULL,
	      regs))
    {
      errno = EIO;
      return 1;
    }

#endif /* __aarch64__ */

  /* Continue the system call until loader starts.  */

  if (ptrace (PTRACE_SYSCALL, tracee->pid, NULL, NULL))
    {
      errno = EIO;
      return 1;
    }

#ifndef REENTRANT
  /* Now that the loader has started, record the value to use for
     /proc/self/exe.  Don't give up just because strdup fails.

     Note that exec_0 copies the absolute file name into buffer.  */

  if (tracee->exec_file)
    free (tracee->exec_file);
  tracee->exec_file = strdup (buffer);
#endif /* REENTRANT */

 again:
  rc = waitpid (tracee->pid, &wstatus, __WALL);
  if (rc == -1 && errno == EINTR)
    goto again;

  if (rc < 0)
    return 1;

  if (!WIFSTOPPED (wstatus))
    /* The process has been killed in response to a signal.
       In this case, simply return 2.  */
    return 2;
  else
    {
      /* Then, check if STATUS is not a syscall-stop, and try again if
	 it isn't.  */
      rc = check_signal (tracee, wstatus);

      if (rc == -1)
	return 2;
      else if (rc)
	goto again;

      /* Retrieve the signal information and determine whether or not
	 the system call has completed.  */

      if (ptrace (PTRACE_GETSIGINFO, tracee->pid, 0,
		  &siginfo))
	return 3;

      if (!syscall_trap_p (&siginfo))
	{
	  /* Continue.  */
	  if (ptrace (PTRACE_SYSCALL, tracee->pid, 0, 0))
	    return 3;

	  goto again;
	}
    }

#ifdef __aarch64__

  if (aarch64_get_regs (tracee->pid, &original))
    return 3;

#else /* !__aarch64__ */

  /* The system call has now completed.  Get the registers again.  */

  if (ptrace (PTRACE_GETREGS, tracee->pid, NULL,
	      &original))
    return 3;

#endif /* __aarch64__ */

  *regs = original;

  /* Upon failure, wait for the next system call and return
     success.  */

  if (original.SYSCALL_RET_REG)
    {
      /* Restore the original stack pointer.  */
      regs->STACK_POINTER = sp;

#ifdef __aarch64__
      aarch64_set_regs (tracee->pid, regs, false);
#else /* !__aarch64__ */
      ptrace (PTRACE_SETREGS, tracee->pid, NULL, regs);
#endif /* __aarch64__ */

      goto exec_failure;
    }

  /* Write the loader area to the stack, followed by its size and the
     original stack pointer.  */

  loader = user_alloca (tracee, &original, regs,
			size + sizeof loader * 2);
  if (!loader)
    return 3;

  size1 = size;

#ifndef STACK_GROWS_DOWNWARDS

  NOT_IMPLEMENTED;

#else /* STACK_GROWS_DOWNWARDS */

  if (user_copy (tracee, (unsigned char *) area,
		 loader + sizeof size1 * 2, size)
      || user_copy (tracee, (unsigned char *) &size1,
		    loader + sizeof size1, sizeof size1))
    return 3;

  size1 = original.STACK_POINTER;

  if (user_copy (tracee, (unsigned char *) &size1,
		 loader, sizeof size1))
    return 3;

#endif /* STACK_GROWS_DOWNWARDS */

  /* Continue.  */
  if (ptrace (PTRACE_SYSCALL, tracee->pid, 0, 0))
    return 3;

  return 0;

 exec_failure:
  return 3;
}

/* Handle a `readlink' or `readlinkat' system call.

   CALLNO is the system call number, and REGS are the current user
   registers of the TRACEE.

   If the first argument of a `readlinkat' system call is AT_FDCWD,
   and the file name specified in either a `readlink' or `readlinkat'
   system call is `/proc/self/exe', write the name of the executable
   being run into the buffer specified in the system call.

   Return the number of bytes written to the tracee's buffer in
   *RESULT.

   Value is 0 upon success.  Value is 1 upon failure, and 2 if the
   system call has been emulated.  */

static int
handle_readlinkat (USER_WORD callno, USER_REGS_STRUCT *regs,
		   struct exec_tracee *tracee, USER_WORD *result)
{
#ifdef REENTRANT
  /* readlinkat cannot be handled specially when the library is built
     to be reentrant, as the file name information cannot be
     recorded.  */
  return 0;
#else /* !REENTRANT */

  char buffer[PATH_MAX + 1];
  USER_WORD address, return_buffer, size;
  size_t length;

  /* Read the file name.  */

#ifdef READLINK_SYSCALL
  if (callno == READLINK_SYSCALL)
    {
      address = regs->SYSCALL_ARG_REG;
      return_buffer = regs->SYSCALL_ARG1_REG;
      size = regs->SYSCALL_ARG2_REG;
    }
  else
#endif /* READLINK_SYSCALL */
    {
      address = regs->SYSCALL_ARG1_REG;
      return_buffer = regs->SYSCALL_ARG2_REG;
      size = regs->SYSCALL_ARG3_REG;
    }

  read_memory (tracee, buffer, PATH_MAX, address);

  /* Make sure BUFFER is NULL terminated.  */

  if (!memchr (buffer, '\0', PATH_MAX))
    {
      errno = ENAMETOOLONG;
      return 1;
    }

  /* Now check if the caller is looking for /proc/self/exe.

     dirfd can be ignored, as for now only absolute file names are
     handled.  FIXME.  */

  if (strcmp (buffer, "/proc/self/exe") || !tracee->exec_file)
    return 0;

  /* Copy over tracee->exec_file.  Truncate it to PATH_MAX, length, or
     size, whichever is less.  */

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
#endif /* REENTRANT */
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

  /* Now dispatch based on the system call.  */
  callno = regs.SYSCALL_NUM_REG;
  switch (callno)
    {
    case EXEC_SYSCALL:

      /* exec system calls should be handled synchronously.  */
      assert (!tracee->waiting_for_syscall);
      rc = handle_exec (tracee, &regs);

      switch (rc)
	{
	case 3:
	  /* It's too late to do anything about this error,.  */
	  break;

	case 2:
	  /* The process has gone away.  */
	  remove_tracee (tracee);
	  break;

	case 1:
	  /* An error has occured; errno is set to the error.  */
	  goto report_syscall_error;
	}

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

      /* Fallthrough.  */

    default:
      /* Don't wait for the system call to finish; instead, the system
	 will DTRT upon the next call to PTRACE_SYSCALL after the
	 syscall-trap signal is delivered.  */

      rc = ptrace (PTRACE_SYSCALL, tracee->pid,
		   NULL, NULL);
      if (rc < 0)
	return;

      tracee->waiting_for_syscall = !tracee->waiting_for_syscall;
    }

  return;

 report_syscall_error:
  reporting_error = true;
  goto common;

 emulate_syscall:
  reporting_error = false;
 common:

  /* Reporting an error or emulating a system call works by setting
     the system call number to -1, letting it continue, and then
     substituting errno for ENOSYS in the case of an error.

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
      ptrace (PTRACE_SYSCALL, tracee->pid, NULL, NULL);
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
      ptrace (PTRACE_SYSCALL, tracee->pid, NULL, NULL);
    }
}



/* Like `execve', but asks the parent to begin tracing this thread.
   Fail if tracing is unsuccessful.  */

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

  rc = ptrace (PTRACE_SETOPTIONS, pid, 0, flags);

  if (rc)
    {
      /* If the kernel can't trace child processes upon creation and
	 exit, then it can't work reliably.  */
      ptrace (PTRACE_DETACH, pid, 0, 0);
      return 1;
    }

  /* Request that the child stop upon the next system call.  */
  rc = ptrace (PTRACE_SYSCALL, pid, 0, 0);
  if (rc)
    return 1;

  /* Enter the child in `tracing_processes'.  */

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
  tracee->next = tracing_processes;
  tracee->waiting_for_syscall = false;
#ifndef REENTRANT
  tracee->exec_file = NULL;
#endif /* REENTRANT */
  tracing_processes = tracee;
  return 0;
}

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

      if (!tracee)
	{
	  if (WSTOPSIG (status) == SIGSTOP)
	    /* A new process has been created and stopped.  Record
	       it now.  */
	    handle_clone (pid);

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
		ptrace (PTRACE_SYSCALL, pid, 0, SIGTRAP);
	      else
		ptrace (PTRACE_SYSCALL, pid, 0, 0);

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
	  /* These events are handled by tracing SIGSTOP signals sent
	     to unknown tracees.  Make sure not to pass through
	     status, as there's no signal really being delivered.  */
	  ptrace (PTRACE_SYSCALL, pid, 0, 0);
	  return -1;

#ifdef SIGSYS
	case SIGSYS:
	  if (ptrace (PTRACE_GETSIGINFO, pid, 0, &siginfo))
	    return -1;

	  /* Continue the process until the next syscall, but don't
	     pass through the signal if an emulated syscall led to
	     it.  */
#ifdef HAVE_SIGINFO_T_SI_SYSCALL
#ifndef __arm__
	  ptrace (PTRACE_SYSCALL, pid, 0, ((siginfo.si_code == SYS_SECCOMP
					    && siginfo.si_syscall == -1)
					   ? 0 : status));
#else /* __arm__ */
	  ptrace (PTRACE_SYSCALL, pid, 0, ((siginfo.si_code == SYS_SECCOMP
					    && siginfo.si_syscall == 222)
					   ? 0 : status));
#endif /* !__arm__ */
#else /* !HAVE_SIGINFO_T_SI_SYSCALL */
	  /* Drop this signal, since what caused it is unknown.  */
	  ptrace (PTRACE_SYSCALL, pid, 0, 0);
#endif /* HAVE_SIGINFO_T_SI_SYSCALL */
	  return -1;
#endif /* SIGSYS */

	default:
	  /* Continue the process until the next syscall.  */
	  ptrace (PTRACE_SYSCALL, pid, 0, status);
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
  loader_name = loader;
}
