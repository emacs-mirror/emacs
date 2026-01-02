/* Generate a Secure Computing filter definition file.

Copyright (C) 2020-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

GNU Emacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see
<https://www.gnu.org/licenses/>.  */

/* This program creates a small Secure Computing filter usable for a
typical minimal Emacs sandbox.  See the man page for `seccomp' for
details about Secure Computing filters.  This program requires the
`libseccomp' library.  However, the resulting filter file requires
only a Linux kernel supporting the Secure Computing extension.

Usage:

  seccomp-filter out.bpf out.pfc out-exec.bpf out-exec.pfc

This writes the raw `struct sock_filter' array to out.bpf and a
human-readable representation to out.pfc.  Additionally, it writes
variants of those files that can be used to sandbox Emacs before
'execve' to out-exec.bpf and out-exec.pfc.  */

#include "config.h"

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <asm/termbits.h>  /* mandatory accordingly to latest ioctl_tty(2) */
#include <time.h>

#include <asm/prctl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <linux/futex.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <fcntl.h>
#include <sched.h>
#include <seccomp.h>
#include <unistd.h>

#include <attribute.h>

#ifndef ARCH_CET_STATUS
#define ARCH_CET_STATUS 0x3001
#endif

/* https://github.com/torvalds/linux/commit/9651fcedf7b92d3f7f1ab179e8ab55b85ee10fc1 */
#ifndef MAP_DROPPABLE
#define MAP_DROPPABLE 0x08
#endif

static ATTRIBUTE_FORMAT_PRINTF (2, 3) _Noreturn void
fail (int error, const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  vfprintf (stderr, format, ap);
  va_end (ap);
  if (error == 0)
    fputc ('\n', stderr);
  else
    {
      fputs (": ", stderr);
      errno = error;
      perror (NULL);
    }
  fflush (NULL);
  exit (EXIT_FAILURE);
}

/* This binary is trivial, so we use a single global filter context
   object that we release using `atexit'.  */

static scmp_filter_ctx ctx;

static void
release_context (void)
{
  seccomp_release (ctx);
}

/* Wrapper functions and macros for libseccomp functions.  We exit
   immediately upon any error to avoid error checking noise.  */

static void
set_attribute (enum scmp_filter_attr attr, uint32_t value)
{
  int status = seccomp_attr_set (ctx, attr, value);
  if (status < 0)
    fail (-status, "seccomp_attr_set (ctx, %u, %u)", attr, value);
}

/* Like `seccomp_rule_add (ACTION, SYSCALL, ...)', except that you
   don't have to specify the number of comparator arguments, and any
   failure will exit the process.  */

#define RULE(action, syscall, ...)                                   \
  do                                                                 \
    {                                                                \
      const struct scmp_arg_cmp arg_array[] = {__VA_ARGS__};         \
      enum { arg_cnt = sizeof arg_array / sizeof *arg_array };       \
      int status = seccomp_rule_add_array (ctx, action, syscall,     \
                                           arg_cnt, arg_array);      \
      if (status < 0)                                                \
        fail (-status, "seccomp_rule_add_array (%s, %s, %d, {%s})",  \
              #action, #syscall, arg_cnt, #__VA_ARGS__);             \
    }                                                                \
  while (false)
#define RULE0(action, syscall)					     \
  do								     \
    {								     \
      int status = seccomp_rule_add (ctx, action, syscall, 0);	     \
      if (status < 0)						     \
	fail (-status, "seccomp_rule_add (%s, %s, 0)",		     \
	      #action, #syscall);				     \
    }								     \
  while (false)

static void
export_filter (const char *file,
               int (*function) (const scmp_filter_ctx, int),
               const char *name)
{
  int fd;
  do
    fd = open (file,
               O_WRONLY | O_CREAT | O_TRUNC | O_BINARY | O_CLOEXEC,
               0644);
  while (fd < 0 && errno == EINTR);
  if (fd < 0)
    fail (errno, "open %s", file);
  int status = function (ctx, fd);
  if (status < 0)
    fail (-status, "%s", name);
  if (close (fd) != 0)
    fail (errno, "close");
}

#define EXPORT_FILTER(file, function) \
  export_filter (file, function, #function)

int
main (int argc, char **argv)
{
  if (argc != 5)
    fail (0, "usage: %s out.bpf out.pfc out-exec.bpf out-exec.pfc",
          argv[0]);

  /* Any unhandled syscall should abort the Emacs process.  */
  ctx = seccomp_init (SCMP_ACT_KILL_PROCESS);
  if (ctx == NULL)
    fail (0, "seccomp_init");
  atexit (release_context);

  /* We want to abort immediately if the architecture is unknown.  */
  set_attribute (SCMP_FLTATR_ACT_BADARCH, SCMP_ACT_KILL_PROCESS);
  set_attribute (SCMP_FLTATR_CTL_NNP, 1);
  set_attribute (SCMP_FLTATR_CTL_TSYNC, 1);

  static_assert (CHAR_BIT == 8);
  static_assert (sizeof (int) == 4 && INT_MIN == INT32_MIN
		 && INT_MAX == INT32_MAX);
  static_assert (sizeof (long) == 8 && LONG_MIN == INT64_MIN
		 && LONG_MAX == INT64_MAX);
  static_assert (sizeof (void *) == 8);
  assert ((uintptr_t) NULL == 0);

  /* Allow a clean exit.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (exit));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (exit_group));

  /* Allow `mmap' and friends.  This is necessary for dynamic loading,
     reading the portable dump file, and thread creation.  We don't
     allow pages to be both writable and executable.  */
  static_assert (MAP_PRIVATE != 0);
  static_assert (MAP_SHARED != 0);
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (mmap),
        SCMP_A2_32 (SCMP_CMP_MASKED_EQ,
                    ~(PROT_NONE | PROT_READ | PROT_WRITE)),
        /* Only support known flags.  MAP_DENYWRITE is ignored, but
           some versions of the dynamic loader still use it.  Also
           allow allocating thread stacks.  */
        SCMP_A3_32 (SCMP_CMP_MASKED_EQ,
                    ~(MAP_SHARED | MAP_PRIVATE | MAP_FILE | MAP_DROPPABLE
                      | MAP_ANONYMOUS | MAP_FIXED | MAP_DENYWRITE
                      | MAP_STACK | MAP_NORESERVE),
                    0));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (mmap),
        SCMP_A2_32 (SCMP_CMP_MASKED_EQ,
                    ~(PROT_NONE | PROT_READ | PROT_EXEC)),
        /* Only support known flags.  MAP_DENYWRITE is ignored, but
           some versions of the dynamic loader still use it. */
        SCMP_A3_32 (SCMP_CMP_MASKED_EQ,
                    ~(MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED
                      | MAP_DENYWRITE),
                    0));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (munmap));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (mprotect),
        /* Don't allow making pages executable.  */
        SCMP_A2_32 (SCMP_CMP_MASKED_EQ,
                    ~(PROT_NONE | PROT_READ | PROT_WRITE), 0));

  /* Allow restartable sequences.  The dynamic linker uses them.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (rseq));

  /* Futexes are used everywhere.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (futex),
        SCMP_A1_32 (SCMP_CMP_EQ, FUTEX_WAKE_PRIVATE));

  /* Allow basic dynamic memory management.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (brk));

  /* Allow some status inquiries.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (uname));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (getuid));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (geteuid));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (getpid));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (gettid));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (getpgrp));

  /* Allow operations on open file descriptors.  File descriptors are
     capabilities, and operating on them shouldn't cause security
     issues.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (read));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (pread64));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (write));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (close));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (lseek));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (dup));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (dup2));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (fstat));

  /* Allow read operations on the filesystem.  If necessary, these
     should be further restricted using mount namespaces.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (access));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (faccessat));
#ifdef __NR_faccessat2
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (faccessat2));
#endif
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (stat));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (stat64));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (lstat));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (lstat64));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (fstatat64));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (newfstatat));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (readlink));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (readlinkat));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (getcwd));

  /* Allow opening files, assuming they are only opened for
     reading.  */
  static_assert (O_WRONLY != 0);
  static_assert (O_RDWR != 0);
  static_assert (O_CREAT != 0);
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (open),
        SCMP_A1_32 (SCMP_CMP_MASKED_EQ,
                    ~(O_RDONLY | O_BINARY | O_CLOEXEC | O_PATH
                      | O_DIRECTORY | O_NOFOLLOW),
                    0));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (openat),
        SCMP_A2_32 (SCMP_CMP_MASKED_EQ,
                    ~(O_RDONLY | O_BINARY | O_CLOEXEC | O_PATH
                      | O_DIRECTORY | O_NOFOLLOW),
                    0));

  /* Allow `tcgetpgrp'.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (ioctl),
        SCMP_A0_32 (SCMP_CMP_EQ, STDIN_FILENO),
        SCMP_A1_32 (SCMP_CMP_EQ, TIOCGPGRP));

  /* Allow `tcgetattr' call of glibc on physical terminal devices. */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (ioctl),
        SCMP_A0_32 (SCMP_CMP_EQ, STDERR_FILENO),
        SCMP_A1_32 (SCMP_CMP_EQ, TCGETS));

  /* Allow reading (but not setting) file flags.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (fcntl),
        SCMP_A1_32 (SCMP_CMP_EQ, F_GETFL));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (fcntl64),
        SCMP_A1_32 (SCMP_CMP_EQ, F_GETFL));

  /* Allow reading random numbers from the kernel.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (getrandom));

  /* Changing the umask is uncritical.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (umask));

  /* Allow creation of pipes.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (pipe));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (pipe2));

  /* Allow reading (but not changing) resource limits.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (getrlimit));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (prlimit64),
	SCMP_A0_32 (SCMP_CMP_EQ, 0) /* pid == 0 (current process) */,
        SCMP_A2_64 (SCMP_CMP_EQ, 0) /* new_limit == NULL */);

  /* Block changing resource limits, but don't crash.  */
  RULE (SCMP_ACT_ERRNO (EPERM), SCMP_SYS (prlimit64),
        SCMP_A0_32 (SCMP_CMP_EQ, 0) /* pid == 0 (current process) */,
        SCMP_A2_64 (SCMP_CMP_NE, 0) /* new_limit != NULL */);

  /* Emacs installs signal handlers, which is harmless.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (sigaction));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (rt_sigaction));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (sigprocmask));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (rt_sigprocmask));

  /* Allow reading the current time.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (clock_gettime),
        SCMP_A0_32 (SCMP_CMP_EQ, CLOCK_REALTIME));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (time));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (gettimeofday));

  /* Allow timer support.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (timer_create));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (timerfd_create));

  /* Allow thread creation.  See the NOTES section in the manual page
     for the `clone' function.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (clone),
        SCMP_A0_64 (SCMP_CMP_MASKED_EQ,
                    /* Flags needed to create threads.  See
                       create_thread in libc.  */
                    ~(CLONE_VM | CLONE_FS | CLONE_FILES
                      | CLONE_SYSVSEM | CLONE_SIGHAND | CLONE_THREAD
                      | CLONE_SETTLS | CLONE_PARENT_SETTID
                      | CLONE_CHILD_CLEARTID),
                    0));
  /* glibc 2.34+ pthread_create uses clone3.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (clone3));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (sigaltstack));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (set_robust_list));

  /* Allow setting the process name for new threads.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (prctl),
        SCMP_A0_32 (SCMP_CMP_EQ, PR_SET_NAME));

  /* Allow some event handling functions used by glib.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (eventfd));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (eventfd2));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (wait4));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (poll));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (pidfd_open),
	SCMP_A1_32 (SCMP_CMP_EQ, 0));

  /* Don't allow creating sockets (network access would be extremely
     dangerous), but also don't crash.  */
  RULE0 (SCMP_ACT_ERRNO (EACCES), SCMP_SYS (socket));

  EXPORT_FILTER (argv[1], seccomp_export_bpf);
  EXPORT_FILTER (argv[2], seccomp_export_pfc);

  /* When applying a Seccomp filter before executing the Emacs binary
     (e.g. using the `bwrap' program), we need to allow further system
     calls.  Firstly, the wrapper binary will need to `execve' the
     Emacs binary.  Furthermore, the C library requires some system
     calls at startup time to set up thread-local storage.  */
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (execve));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (set_tid_address));
  RULE (SCMP_ACT_ERRNO (EINVAL), SCMP_SYS (prctl),
	SCMP_A0_32 (SCMP_CMP_EQ, PR_CAPBSET_READ));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (arch_prctl),
        SCMP_A0_32 (SCMP_CMP_EQ, ARCH_SET_FS));
  RULE (SCMP_ACT_ERRNO (EINVAL), SCMP_SYS (arch_prctl),
        SCMP_A0_32 (SCMP_CMP_EQ, ARCH_CET_STATUS));
  RULE0 (SCMP_ACT_ALLOW, SCMP_SYS (statfs));

  /* We want to allow starting the Emacs binary itself with the
     --seccomp flag, so we need to allow the `prctl' and `seccomp'
     system calls.  */
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (prctl),
        SCMP_A0_32 (SCMP_CMP_EQ, PR_SET_NO_NEW_PRIVS),
        SCMP_A1_64 (SCMP_CMP_EQ, 1), SCMP_A2_64 (SCMP_CMP_EQ, 0),
        SCMP_A3_64 (SCMP_CMP_EQ, 0), SCMP_A4_64 (SCMP_CMP_EQ, 0));
  RULE (SCMP_ACT_ALLOW, SCMP_SYS (seccomp),
        SCMP_A0_32 (SCMP_CMP_EQ, SECCOMP_SET_MODE_FILTER),
        SCMP_A1_32 (SCMP_CMP_EQ, SECCOMP_FILTER_FLAG_TSYNC));

  EXPORT_FILTER (argv[3], seccomp_export_bpf);
  EXPORT_FILTER (argv[4], seccomp_export_pfc);
}
