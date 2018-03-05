/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985-1988, 1993-1995, 1999-2018 Free Software
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

#include <config.h>

#include <execinfo.h>
#include "sysstdio.h"
#ifdef HAVE_PWD_H
#include <pwd.h>
#include <grp.h>
#endif /* HAVE_PWD_H */
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>

#include <c-ctype.h>
#include <utimens.h>

#include "lisp.h"
#include "sheap.h"
#include "sysselect.h"
#include "blockinput.h"

#ifdef HAVE_LINUX_FS_H
# include <linux/fs.h>
# include <sys/syscall.h>
#endif

#ifdef CYGWIN
# include <cygwin/fs.h>
#endif

#if defined DARWIN_OS || defined __FreeBSD__
# include <sys/sysctl.h>
#endif

#ifdef __FreeBSD__
/* Sparc/ARM machine/frame.h has 'struct frame' which conflicts with Emacs's
   'struct frame', so rename it.  */
# define frame freebsd_frame
# include <sys/user.h>
# undef frame

# include <math.h>
#endif

#ifdef HAVE_SOCKETS
#include <sys/socket.h>
#include <netdb.h>
#endif /* HAVE_SOCKETS */

#ifdef WINDOWSNT
#define read sys_read
#define write sys_write
#ifndef STDERR_FILENO
#define STDERR_FILENO fileno(GetStdHandle(STD_ERROR_HANDLE))
#endif
#include "w32.h"
#endif /* WINDOWSNT */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

/* Get SI_SRPC_DOMAIN, if it is available.  */
#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif

#ifdef MSDOS	/* Demacs 1.1.2 91/10/20 Manabu Higashida, MW Aug 1993 */
#include "msdos.h"
#endif

#include <sys/param.h>
#include <sys/file.h>
#include <fcntl.h>

#include "systty.h"
#include "syswait.h"

#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#include <memory.h>
#endif /* HAVE_SYS_UTSNAME_H */

#include "keyboard.h"
#include "frame.h"
#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "process.h"
#include "cm.h"

#include "gnutls.h"
/* MS-Windows loads GnuTLS at run time, if available; we don't want to
   do that during startup just to call gnutls_rnd.  */
#if defined HAVE_GNUTLS && !defined WINDOWSNT
# include <gnutls/crypto.h>
#else
# define emacs_gnutls_global_init() Qnil
# define gnutls_rnd(level, data, len) (-1)
#endif

#ifdef WINDOWSNT
#include <direct.h>
/* In process.h which conflicts with the local copy.  */
#define _P_WAIT 0
int _cdecl _spawnlp (int, const char *, const char *, ...);
/* The following is needed for O_CLOEXEC, F_SETFD, FD_CLOEXEC, and
   several prototypes of functions called below.  */
#include <sys/socket.h>
#endif

#include "syssignal.h"
#include "systime.h"

/* ULLONG_MAX is missing on Red Hat Linux 7.3; see Bug#11781.  */
#ifndef ULLONG_MAX
#define ULLONG_MAX TYPE_MAXIMUM (unsigned long long int)
#endif

/* Declare here, including term.h is problematic on some systems.  */
extern void tputs (const char *, int, int (*)(int));

static const int baud_convert[] =
  {
    0, 50, 75, 110, 135, 150, 200, 300, 600, 1200,
    1800, 2400, 4800, 9600, 19200, 38400
  };

#ifdef HAVE_PERSONALITY_ADDR_NO_RANDOMIZE
# include <sys/personality.h>

/* Disable address randomization in the current process.  Return true
   if addresses were randomized but this has been disabled, false
   otherwise. */
bool
disable_address_randomization (void)
{
  int pers = personality (0xffffffff);
  if (pers < 0)
    return false;
  int desired_pers = pers | ADDR_NO_RANDOMIZE;

  /* Call 'personality' twice, to detect buggy platforms like WSL
     where 'personality' always returns 0.  */
  return (pers != desired_pers
	  && personality (desired_pers) == pers
	  && personality (0xffffffff) == desired_pers);
}
#endif

/* Execute the program in FILE, with argument vector ARGV and environ
   ENVP.  Return an error number if unsuccessful.  This is like execve
   except it reenables ASLR in the executed program if necessary, and
   on error it returns an error number rather than -1.  */
int
emacs_exec_file (char const *file, char *const *argv, char *const *envp)
{
#ifdef HAVE_PERSONALITY_ADDR_NO_RANDOMIZE
  int pers = getenv ("EMACS_HEAP_EXEC") ? personality (0xffffffff) : -1;
  bool change_personality = 0 <= pers && pers & ADDR_NO_RANDOMIZE;
  if (change_personality)
    personality (pers & ~ADDR_NO_RANDOMIZE);
#endif

  execve (file, argv, envp);
  int err = errno;

#ifdef HAVE_PERSONALITY_ADDR_NO_RANDOMIZE
  if (change_personality)
    personality (pers);
#endif

  return err;
}

/* If FD is not already open, arrange for it to be open with FLAGS.  */
static void
force_open (int fd, int flags)
{
  if (dup2 (fd, fd) < 0 && errno == EBADF)
    {
      int n = open (NULL_DEVICE, flags);
      if (n < 0 || (fd != n && (dup2 (n, fd) < 0 || emacs_close (n) != 0)))
	{
	  emacs_perror (NULL_DEVICE);
	  exit (EXIT_FAILURE);
	}
    }
}

/* Make sure stdin, stdout, and stderr are open to something, so that
   their file descriptors are not hijacked by later system calls.  */
void
init_standard_fds (void)
{
  /* Open stdin for *writing*, and stdout and stderr for *reading*.
     That way, any attempt to do normal I/O will result in an error,
     just as if the files were closed, and the file descriptors will
     not be reused by later opens.  */
  force_open (STDIN_FILENO, O_WRONLY);
  force_open (STDOUT_FILENO, O_RDONLY);
  force_open (STDERR_FILENO, O_RDONLY);
}

/* Return the current working directory.  The result should be freed
   with 'free'.  Return NULL (setting errno) on errors.  If the
   current directory is unreachable, return either NULL or a string
   beginning with '('.  */

static char *
get_current_dir_name_or_unreachable (void)
{
  /* Use malloc, not xmalloc, since this function can be called before
     the xmalloc exception machinery is available.  */

  char *pwd;

  /* The maximum size of a directory name, including the terminating null.
     Leave room so that the caller can append a trailing slash.  */
  ptrdiff_t dirsize_max = min (PTRDIFF_MAX, SIZE_MAX) - 1;

  /* The maximum size of a buffer for a file name, including the
     terminating null.  This is bounded by MAXPATHLEN, if available.  */
  ptrdiff_t bufsize_max = dirsize_max;
#ifdef MAXPATHLEN
  bufsize_max = min (bufsize_max, MAXPATHLEN);
#endif

# if HAVE_GET_CURRENT_DIR_NAME && !BROKEN_GET_CURRENT_DIR_NAME
#  ifdef HYBRID_MALLOC
  bool use_libc = bss_sbrk_did_unexec;
#  else
  bool use_libc = true;
#  endif
  if (use_libc)
    {
      /* For an unreachable directory, this returns a string that starts
	 with "(unreachable)"; see Bug#27871.  */
      pwd = get_current_dir_name ();
      if (pwd)
	{
	  if (strlen (pwd) < dirsize_max)
	    return pwd;
	  free (pwd);
	  errno = ERANGE;
	}
      return NULL;
    }
# endif

  size_t pwdlen;
  struct stat dotstat, pwdstat;
  pwd = getenv ("PWD");

  /* If PWD is accurate, use it instead of calling getcwd.  PWD is
     sometimes a nicer name, and using it may avoid a fatal error if a
     parent directory is searchable but not readable.  */
  if (pwd
      && (pwdlen = strlen (pwd)) < bufsize_max
      && IS_DIRECTORY_SEP (pwd[pwdlen && IS_DEVICE_SEP (pwd[1]) ? 2 : 0])
      && stat (pwd, &pwdstat) == 0
      && stat (".", &dotstat) == 0
      && dotstat.st_ino == pwdstat.st_ino
      && dotstat.st_dev == pwdstat.st_dev)
    {
      char *buf = malloc (pwdlen + 1);
      if (!buf)
        return NULL;
      return memcpy (buf, pwd, pwdlen + 1);
    }
  else
    {
      ptrdiff_t buf_size = min (bufsize_max, 1024);
      char *buf = malloc (buf_size);
      if (!buf)
        return NULL;
      for (;;)
        {
          if (getcwd (buf, buf_size) == buf)
	    return buf;
	  int getcwd_errno = errno;
	  if (getcwd_errno != ERANGE || buf_size == bufsize_max)
            {
              free (buf);
	      errno = getcwd_errno;
              return NULL;
            }
	  buf_size = buf_size <= bufsize_max / 2 ? 2 * buf_size : bufsize_max;
          buf = realloc (buf, buf_size);
          if (!buf)
            return NULL;
        }
    }
}

/* Return the current working directory.  The result should be freed
   with 'free'.  Return NULL (setting errno) on errors; an unreachable
   directory (e.g., its name starts with '(') counts as an error.  */

char *
emacs_get_current_dir_name (void)
{
  char *dir = get_current_dir_name_or_unreachable ();
  if (dir && *dir == '(')
    {
      free (dir);
      errno = ENOENT;
      return NULL;
    }
  return dir;
}


/* Discard pending input on all input descriptors.  */

void
discard_tty_input (void)
{
#ifndef WINDOWSNT
  struct emacs_tty buf;

  if (noninteractive)
    return;

#ifdef MSDOS    /* Demacs 1.1.1 91/10/16 HIRANO Satoshi */
  while (dos_keyread () != -1)
    ;
#else /* not MSDOS */
  {
    struct tty_display_info *tty;
    for (tty = tty_list; tty; tty = tty->next)
      {
        if (tty->input)         /* Is the device suspended? */
          {
            emacs_get_tty (fileno (tty->input), &buf);
            emacs_set_tty (fileno (tty->input), &buf, 0);
          }
      }
  }
#endif /* not MSDOS */
#endif /* not WINDOWSNT */
}


#ifdef SIGTSTP

/* Arrange for character C to be read as the next input from
   the terminal.
   XXX What if we have multiple ttys?
*/

void
stuff_char (char c)
{
  if (! (FRAMEP (selected_frame)
	 && FRAME_LIVE_P (XFRAME (selected_frame))
	 && FRAME_TERMCAP_P (XFRAME (selected_frame))))
    return;

/* Should perhaps error if in batch mode */
#ifdef TIOCSTI
  ioctl (fileno (CURTTY()->input), TIOCSTI, &c);
#else /* no TIOCSTI */
  error ("Cannot stuff terminal input characters in this version of Unix");
#endif /* no TIOCSTI */
}

#endif /* SIGTSTP */

void
init_baud_rate (int fd)
{
  int emacs_ospeed;

  if (noninteractive)
    emacs_ospeed = 0;
  else
    {
#ifdef DOS_NT
    emacs_ospeed = 15;
#else  /* not DOS_NT */
      struct termios sg;

      sg.c_cflag = B9600;
      tcgetattr (fd, &sg);
      emacs_ospeed = cfgetospeed (&sg);
#endif /* not DOS_NT */
    }

  baud_rate = (emacs_ospeed < ARRAYELTS (baud_convert)
	       ? baud_convert[emacs_ospeed] : 9600);
  if (baud_rate == 0)
    baud_rate = 1200;
}



#ifndef MSDOS

/* Wait for the subprocess with process id CHILD to terminate or change status.
   CHILD must be a child process that has not been reaped.
   If STATUS is non-null, store the waitpid-style exit status into *STATUS
   and tell wait_reading_process_output that it needs to look around.
   Use waitpid-style OPTIONS when waiting.
   If INTERRUPTIBLE, this function is interruptible by a signal.

   Return CHILD if successful, 0 if no status is available, and a
   negative value (setting errno) if waitpid is buggy.  */
static pid_t
get_child_status (pid_t child, int *status, int options, bool interruptible)
{
  pid_t pid;

  /* Invoke waitpid only with a known process ID; do not invoke
     waitpid with a nonpositive argument.  Otherwise, Emacs might
     reap an unwanted process by mistake.  For example, invoking
     waitpid (-1, ...) can mess up glib by reaping glib's subprocesses,
     so that another thread running glib won't find them.  */
  eassert (child > 0);

  while (true)
    {
      /* Note: the MS-Windows emulation of waitpid calls maybe_quit
	 internally.  */
      if (interruptible)
	maybe_quit ();

      pid = waitpid (child, status, options);
      if (0 <= pid)
	break;
      if (errno != EINTR)
	{
	  /* Most likely, waitpid is buggy and the operating system
	     lost track of the child somehow.  Return -1 and let the
	     caller try to figure things out.  Possibly the bug could
	     cause Emacs to kill the wrong process.  Oh well.  */
	  return pid;
	}
    }

  /* If successful and status is requested, tell wait_reading_process_output
     that it needs to wake up and look around.  */
  if (pid && status && input_available_clear_time)
    *input_available_clear_time = make_timespec (0, 0);

  return pid;
}

/* Wait for the subprocess with process id CHILD to terminate.
   CHILD must be a child process that has not been reaped.
   If STATUS is non-null, store the waitpid-style exit status into *STATUS
   and tell wait_reading_process_output that it needs to look around.
   If INTERRUPTIBLE, this function is interruptible by a signal.
   Return true if successful, false (setting errno) if CHILD cannot be
   waited for because waitpid is buggy.  */
bool
wait_for_termination (pid_t child, int *status, bool interruptible)
{
  return 0 <= get_child_status (child, status, 0, interruptible);
}

/* Report whether the subprocess with process id CHILD has changed status.
   Termination counts as a change of status.
   CHILD must be a child process that has not been reaped.
   If STATUS is non-null, store the waitpid-style exit status into *STATUS
   and tell wait_reading_process_output that it needs to look around.
   Use waitpid-style OPTIONS to check status, but do not wait.

   Return CHILD if successful, 0 if no status is available because
   the process's state has not changed.  */
pid_t
child_status_changed (pid_t child, int *status, int options)
{
  return get_child_status (child, status, WNOHANG | options, 0);
}


/*  Set up the terminal at the other end of a pseudo-terminal that
    we will be controlling an inferior through.
    It should not echo or do line-editing, since that is done
    in Emacs.  No padding needed for insertion into an Emacs buffer.  */

void
child_setup_tty (int out)
{
#ifndef WINDOWSNT
  struct emacs_tty s;

  emacs_get_tty (out, &s);
  s.main.c_oflag |= OPOST;	/* Enable output postprocessing */
  s.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL on output */
#ifdef NLDLY
  /* https://lists.gnu.org/r/emacs-devel/2008-05/msg00406.html
     Some versions of GNU Hurd do not have FFDLY?  */
#ifdef FFDLY
  s.main.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
  				/* No output delays */
#else
  s.main.c_oflag &= ~(NLDLY|CRDLY|TABDLY|BSDLY|VTDLY);
  				/* No output delays */
#endif
#endif
  s.main.c_lflag &= ~ECHO;	/* Disable echo */
  s.main.c_lflag |= ISIG;	/* Enable signals */
#ifdef IUCLC
  s.main.c_iflag &= ~IUCLC;	/* Disable downcasing on input.  */
#endif
#ifdef ISTRIP
  s.main.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
#ifdef OLCUC
  s.main.c_oflag &= ~OLCUC;	/* Disable upcasing on output.  */
#endif
  s.main.c_oflag &= ~TAB3;	/* Disable tab expansion */
  s.main.c_cflag = (s.main.c_cflag & ~CSIZE) | CS8; /* Don't strip 8th bit */
  s.main.c_cc[VERASE] = CDISABLE;	/* disable erase processing */
  s.main.c_cc[VKILL] = CDISABLE;	/* disable kill processing */

#ifdef HPUX
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* HPUX */

#ifdef SIGNALS_VIA_CHARACTERS
  /* the QUIT and INTR character are used in process_send_signal
     so set them here to something useful.  */
  if (s.main.c_cc[VQUIT] == CDISABLE)
    s.main.c_cc[VQUIT] = '\\'&037;	/* Control-\ */
  if (s.main.c_cc[VINTR] == CDISABLE)
    s.main.c_cc[VINTR] = 'C'&037;	/* Control-C */
#endif /* not SIGNALS_VIA_CHARACTERS */

#ifdef AIX
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.  */
  s.main.c_iflag &= ~IGNBRK;
  s.main.c_iflag &= ~BRKINT;
  /* rms: Formerly it set s.main.c_cc[VINTR] to 0377 here
     unconditionally.  Then a SIGNALS_VIA_CHARACTERS conditional
     would force it to 0377.  That looks like duplicated code.  */
  s.main.c_cflag = (s.main.c_cflag & ~CBAUD) | B9600; /* baud rate sanity */
#endif /* AIX */

  /* We originally enabled ICANON (and set VEOF to 04), and then had
     process.c send additional EOF chars to flush the output when faced
     with long lines, but this leads to weird effects when the
     subprocess has disabled ICANON and ends up seeing those spurious
     extra EOFs.  So we don't send EOFs any more in
     process.c:send_process.  First we tried to disable ICANON by
     default, so if a subsprocess sets up ICANON, it's his problem (or
     the Elisp package that talks to it) to deal with lines that are
     too long.  But this disables some features, such as the ability
     to send EOF signals.  So we re-enabled ICANON but there is no
     more "send eof to flush" going on (which is wrong and unportable
     in itself).  The correct way to handle too much output is to
     buffer what could not be written and then write it again when
     select returns ok for writing.  This has it own set of
     problems.  Write is now asynchronous, is that a problem?  How much
     do we buffer, and what do we do when that limit is reached?  */

  s.main.c_lflag |= ICANON;	/* Enable line editing and eof processing */
  s.main.c_cc[VEOF] = 'D'&037;	/* Control-D */
#if 0	    /* These settings only apply to non-ICANON mode. */
  s.main.c_cc[VMIN] = 1;
  s.main.c_cc[VTIME] = 0;
#endif

  emacs_set_tty (out, &s, 0);
#endif /* not WINDOWSNT */
}
#endif	/* not MSDOS */


/* Record a signal code and the action for it.  */
struct save_signal
{
  int code;
  struct sigaction action;
};

static void save_signal_handlers (struct save_signal *);
static void restore_signal_handlers (struct save_signal *);

/* Suspend the Emacs process; give terminal to its superior.  */

void
sys_suspend (void)
{
#ifndef DOS_NT
  kill (0, SIGTSTP);
#else
/* On a system where suspending is not implemented,
   instead fork a subshell and let it talk directly to the terminal
   while we wait.  */
  sys_subshell ();

#endif
}

/* Fork a subshell.  */

void
sys_subshell (void)
{
#ifdef DOS_NT	/* Demacs 1.1.2 91/10/20 Manabu Higashida */
#ifdef MSDOS
  int st;
  char oldwd[MAXPATHLEN+1]; /* Fixed length is safe on MSDOS.  */
#else
  char oldwd[MAX_UTF8_PATH];
#endif	/* MSDOS */
#else	/* !DOS_NT */
  int status;
#endif
  pid_t pid;
  struct save_signal saved_handlers[5];
  char *str = SSDATA (encode_current_directory ());

#ifdef DOS_NT
  pid = 0;
#else
  {
    char *volatile str_volatile = str;
    pid = vfork ();
    str = str_volatile;
  }
#endif

  if (pid < 0)
    error ("Can't spawn subshell");

  saved_handlers[0].code = SIGINT;
  saved_handlers[1].code = SIGQUIT;
  saved_handlers[2].code = SIGTERM;
#ifdef USABLE_SIGIO
  saved_handlers[3].code = SIGIO;
  saved_handlers[4].code = 0;
#else
  saved_handlers[3].code = 0;
#endif

#ifdef DOS_NT
  save_signal_handlers (saved_handlers);
#endif

  if (pid == 0)
    {
      const char *sh = 0;

#ifdef DOS_NT    /* MW, Aug 1993 */
      getcwd (oldwd, sizeof oldwd);
      if (sh == 0)
	sh = egetenv ("SUSPEND");	/* KFS, 1994-12-14 */
#endif
      if (sh == 0)
	sh = egetenv ("SHELL");
      if (sh == 0)
	sh = "sh";

      /* Use our buffer's default directory for the subshell.  */
      if (chdir (str) != 0)
	{
#ifndef DOS_NT
	  emacs_perror (str);
	  _exit (EXIT_CANCELED);
#endif
	}

#ifdef MSDOS    /* Demacs 1.1.2 91/10/20 Manabu Higashida */
      {
	char *epwd = getenv ("PWD");
	char old_pwd[MAXPATHLEN+1+4];

	/* If PWD is set, pass it with corrected value.  */
	if (epwd)
	  {
	    strcpy (old_pwd, epwd);
	    setenv ("PWD", str, 1);
	  }
	st = system (sh);
	chdir (oldwd);	/* FIXME: Do the right thing on chdir failure.  */
	if (epwd)
	  putenv (old_pwd);	/* restore previous value */
      }
#else /* not MSDOS */
#ifdef  WINDOWSNT
      /* Waits for process completion */
      pid = _spawnlp (_P_WAIT, sh, sh, NULL);
      chdir (oldwd);	/* FIXME: Do the right thing on chdir failure.  */
      if (pid == -1)
	write (1, "Can't execute subshell", 22);
#else   /* not WINDOWSNT */
      execlp (sh, sh, (char *) 0);
      emacs_perror (sh);
      _exit (errno == ENOENT ? EXIT_ENOENT : EXIT_CANNOT_INVOKE);
#endif  /* not WINDOWSNT */
#endif /* not MSDOS */
    }

  /* Do this now if we did not do it before.  */
#ifndef MSDOS
  save_signal_handlers (saved_handlers);
#endif

#ifndef DOS_NT
  wait_for_termination (pid, &status, 0);
#endif
  restore_signal_handlers (saved_handlers);
}

static void
save_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      struct sigaction action;
      emacs_sigaction_init (&action, SIG_IGN);
      sigaction (saved_handlers->code, &action, &saved_handlers->action);
      saved_handlers++;
    }
}

static void
restore_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      sigaction (saved_handlers->code, &saved_handlers->action, 0);
      saved_handlers++;
    }
}

#ifdef USABLE_SIGIO
static int old_fcntl_flags[FD_SETSIZE];
#endif

void
init_sigio (int fd)
{
#ifdef USABLE_SIGIO
  old_fcntl_flags[fd] = fcntl (fd, F_GETFL, 0) & ~FASYNC;
  fcntl (fd, F_SETFL, old_fcntl_flags[fd] | FASYNC);
  interrupts_deferred = 0;
#endif
}

#ifndef DOS_NT
static void
reset_sigio (int fd)
{
#ifdef USABLE_SIGIO
  fcntl (fd, F_SETFL, old_fcntl_flags[fd]);
#endif
}
#endif

void
request_sigio (void)
{
#ifdef USABLE_SIGIO
  sigset_t unblocked;

  if (noninteractive)
    return;

  sigemptyset (&unblocked);
# ifdef SIGWINCH
  sigaddset (&unblocked, SIGWINCH);
# endif
  sigaddset (&unblocked, SIGIO);
  pthread_sigmask (SIG_UNBLOCK, &unblocked, 0);

  interrupts_deferred = 0;
#endif
}

void
unrequest_sigio (void)
{
#ifdef USABLE_SIGIO
  sigset_t blocked;

  if (noninteractive)
    return;

  sigemptyset (&blocked);
# ifdef SIGWINCH
  sigaddset (&blocked, SIGWINCH);
# endif
  sigaddset (&blocked, SIGIO);
  pthread_sigmask (SIG_BLOCK, &blocked, 0);
  interrupts_deferred = 1;
#endif
}

#ifndef MSDOS
/* Block SIGCHLD.  */

void
block_child_signal (sigset_t *oldset)
{
  sigset_t blocked;
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGCHLD);
  sigaddset (&blocked, SIGINT);
  pthread_sigmask (SIG_BLOCK, &blocked, oldset);
}

/* Unblock SIGCHLD.  */

void
unblock_child_signal (sigset_t const *oldset)
{
  pthread_sigmask (SIG_SETMASK, oldset, 0);
}

#endif	/* !MSDOS */

/* Block SIGINT.  */
void
block_interrupt_signal (sigset_t *oldset)
{
  sigset_t blocked;
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGINT);
  pthread_sigmask (SIG_BLOCK, &blocked, oldset);
}

/* Restore previously saved signal mask.  */
void
restore_signal_mask (sigset_t const *oldset)
{
  pthread_sigmask (SIG_SETMASK, oldset, 0);
}


/* Saving and restoring the process group of Emacs's terminal.  */

/* The process group of which Emacs was a member when it initially
   started.

   If Emacs was in its own process group (i.e. inherited_pgroup ==
   getpid ()), then we know we're running under a shell with job
   control (Emacs would never be run as part of a pipeline).
   Everything is fine.

   If Emacs was not in its own process group, then we know we're
   running under a shell (or a caller) that doesn't know how to
   separate itself from Emacs (like sh).  Emacs must be in its own
   process group in order to receive SIGIO correctly.  In this
   situation, we put ourselves in our own pgroup, forcibly set the
   tty's pgroup to our pgroup, and make sure to restore and reinstate
   the tty's pgroup just like any other terminal setting.  If
   inherited_group was not the tty's pgroup, then we'll get a
   SIGTTmumble when we try to change the tty's pgroup, and a CONT if
   it goes foreground in the future, which is what should happen.  */

static pid_t inherited_pgroup;

void
init_foreground_group (void)
{
  pid_t pgrp = getpgrp ();
  inherited_pgroup = getpid () == pgrp ? 0 : pgrp;
}

/* Block and unblock SIGTTOU.  */

void
block_tty_out_signal (sigset_t *oldset)
{
#ifdef SIGTTOU
  sigset_t blocked;
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGTTOU);
  pthread_sigmask (SIG_BLOCK, &blocked, oldset);
#endif
}

void
unblock_tty_out_signal (sigset_t const *oldset)
{
#ifdef SIGTTOU
  pthread_sigmask (SIG_SETMASK, oldset, 0);
#endif
}

/* Safely set a controlling terminal FD's process group to PGID.
   If we are not in the foreground already, POSIX requires tcsetpgrp
   to deliver a SIGTTOU signal, which would stop us.  This is an
   annoyance, so temporarily ignore the signal.

   In practice, platforms lacking SIGTTOU also lack tcsetpgrp, so
   skip all this unless SIGTTOU is defined.  */
static void
tcsetpgrp_without_stopping (int fd, pid_t pgid)
{
#ifdef SIGTTOU
  sigset_t oldset;
  block_input ();
  block_tty_out_signal (&oldset);
  tcsetpgrp (fd, pgid);
  unblock_tty_out_signal (&oldset);
  unblock_input ();
#endif
}

/* Split off the foreground process group to Emacs alone.  When we are
   in the foreground, but not started in our own process group,
   redirect the tty device handle FD to point to our own process
   group.  FD must be the file descriptor of the controlling tty.  */
static void
narrow_foreground_group (int fd)
{
  if (inherited_pgroup && setpgid (0, 0) == 0)
    tcsetpgrp_without_stopping (fd, getpid ());
}

/* Set the tty to our original foreground group.  */
static void
widen_foreground_group (int fd)
{
  if (inherited_pgroup && setpgid (0, inherited_pgroup) == 0)
    tcsetpgrp_without_stopping (fd, inherited_pgroup);
}

/* Getting and setting emacs_tty structures.  */

/* Set *TC to the parameters associated with the terminal FD,
   or clear it if the parameters are not available.
   Return 0 on success, -1 on failure.  */
int
emacs_get_tty (int fd, struct emacs_tty *settings)
{
  /* Retrieve the primary parameters - baud rate, character size, etcetera.  */
  memset (&settings->main, 0, sizeof (settings->main));
#ifdef DOS_NT
#ifdef WINDOWSNT
  HANDLE h = (HANDLE)_get_osfhandle (fd);
  DWORD console_mode;

  if (h && h != INVALID_HANDLE_VALUE && GetConsoleMode (h, &console_mode))
    {
      settings->main = console_mode;
      return 0;
    }
#endif	/* WINDOWSNT */
  return -1;
#else	/* !DOS_NT */
  /* We have those nifty POSIX tcmumbleattr functions.  */
  return tcgetattr (fd, &settings->main);
#endif
}


/* Set the parameters of the tty on FD according to the contents of
   *SETTINGS.  If FLUSHP, discard input.
   Return 0 if all went well, and -1 (setting errno) if anything failed.  */

int
emacs_set_tty (int fd, struct emacs_tty *settings, bool flushp)
{
  /* Set the primary parameters - baud rate, character size, etcetera.  */
#ifdef DOS_NT
#ifdef WINDOWSNT
  HANDLE h = (HANDLE)_get_osfhandle (fd);

  if (h && h != INVALID_HANDLE_VALUE)
    {
      DWORD new_mode;

      /* Assume the handle is open for input.  */
      if (flushp)
	FlushConsoleInputBuffer (h);
      new_mode = settings->main;
      SetConsoleMode (h, new_mode);
    }
#endif	/* WINDOWSNT */
#else  /* !DOS_NT */
  int i;
  /* We have those nifty POSIX tcmumbleattr functions.
     William J. Smith <wjs@wiis.wang.com> writes:
     "POSIX 1003.1 defines tcsetattr to return success if it was
     able to perform any of the requested actions, even if some
     of the requested actions could not be performed.
     We must read settings back to ensure tty setup properly.
     AIX requires this to keep tty from hanging occasionally."  */
  /* This make sure that we don't loop indefinitely in here.  */
  for (i = 0 ; i < 10 ; i++)
    if (tcsetattr (fd, flushp ? TCSAFLUSH : TCSADRAIN, &settings->main) < 0)
      {
	if (errno == EINTR)
	  continue;
	else
	  return -1;
      }
    else
      {
	struct termios new;

	memset (&new, 0, sizeof (new));
	/* Get the current settings, and see if they're what we asked for.  */
	tcgetattr (fd, &new);
	/* We cannot use memcmp on the whole structure here because under
	 * aix386 the termios structure has some reserved field that may
	 * not be filled in.
	 */
	if (   new.c_iflag == settings->main.c_iflag
	    && new.c_oflag == settings->main.c_oflag
	    && new.c_cflag == settings->main.c_cflag
	    && new.c_lflag == settings->main.c_lflag
	    && memcmp (new.c_cc, settings->main.c_cc, NCCS) == 0)
	  break;
	else
	  continue;
      }
#endif

  /* We have survived the tempest.  */
  return 0;
}



#ifdef F_SETOWN
static int old_fcntl_owner[FD_SETSIZE];
#endif /* F_SETOWN */

/* This may also be defined in stdio,
   but if so, this does no harm,
   and using the same name avoids wasting the other one's space.  */

#if defined (USG)
unsigned char _sobuf[BUFSIZ+8];
#else
char _sobuf[BUFSIZ];
#endif

/* Initialize the terminal mode on all tty devices that are currently
   open. */

void
init_all_sys_modes (void)
{
  struct tty_display_info *tty;
  for (tty = tty_list; tty; tty = tty->next)
    init_sys_modes (tty);
}

/* Initialize the terminal mode on the given tty device. */

void
init_sys_modes (struct tty_display_info *tty_out)
{
  struct emacs_tty tty;
#ifndef DOS_NT
  Lisp_Object terminal;
#endif

  Vtty_erase_char = Qnil;

  if (noninteractive)
    return;

  if (!tty_out->output)
    return;                     /* The tty is suspended. */

  narrow_foreground_group (fileno (tty_out->input));

  if (! tty_out->old_tty)
    tty_out->old_tty = xmalloc (sizeof *tty_out->old_tty);

  emacs_get_tty (fileno (tty_out->input), tty_out->old_tty);

  tty = *tty_out->old_tty;

#if !defined (DOS_NT)
  XSETINT (Vtty_erase_char, tty.main.c_cc[VERASE]);

  tty.main.c_iflag |= (IGNBRK);	/* Ignore break condition */
  tty.main.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
#ifdef INLCR  /* I'm just being cautious,
		 since I can't check how widespread INLCR is--rms.  */
  tty.main.c_iflag &= ~INLCR;	/* Disable map of NL to CR on input */
#endif
#ifdef ISTRIP
  tty.main.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
  tty.main.c_lflag &= ~ECHO;	/* Disable echo */
  tty.main.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
#ifdef IEXTEN
  tty.main.c_lflag &= ~IEXTEN;	/* Disable other editing characters.  */
#endif
  tty.main.c_lflag |= ISIG;	/* Enable signals */
  if (tty_out->flow_control)
    {
      tty.main.c_iflag |= IXON;	/* Enable start/stop output control */
#ifdef IXANY
      tty.main.c_iflag &= ~IXANY;
#endif /* IXANY */
    }
  else
    tty.main.c_iflag &= ~IXON;	/* Disable start/stop output control */
  tty.main.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL
                                   on output */
  tty.main.c_oflag &= ~TAB3;	/* Disable tab expansion */
#ifdef CS8
  if (tty_out->meta_key)
    {
      tty.main.c_cflag |= CS8;	/* allow 8th bit on input */
      tty.main.c_cflag &= ~PARENB;/* Don't check parity */
    }
#endif

  XSETTERMINAL(terminal, tty_out->terminal);
  if (!NILP (Fcontrolling_tty_p (terminal)))
    {
      tty.main.c_cc[VINTR] = quit_char;	/* C-g (usually) gives SIGINT */
      /* Set up C-g for both SIGQUIT and SIGINT.
         We don't know which we will get, but we handle both alike
         so which one it really gives us does not matter.  */
      tty.main.c_cc[VQUIT] = quit_char;
    }
  else
    {
      /* We normally don't get interrupt or quit signals from tty
         devices other than our controlling terminal; therefore,
         we must handle C-g as normal input.  Unfortunately, this
         means that the interrupt and quit feature must be
         disabled on secondary ttys, or we would not even see the
         keypress.

         Note that even though emacsclient could have special code
         to pass SIGINT to Emacs, we should _not_ enable
         interrupt/quit keys for emacsclient frames.  This means
         that we can't break out of loops in C code from a
         secondary tty frame, but we can always decide what
         display the C-g came from, which is more important from a
         usability point of view.  (Consider the case when two
         people work together using the same Emacs instance.)  */
      tty.main.c_cc[VINTR] = CDISABLE;
      tty.main.c_cc[VQUIT] = CDISABLE;
    }
  tty.main.c_cc[VMIN] = 1;	/* Input should wait for at least 1 char */
  tty.main.c_cc[VTIME] = 0;	/* no matter how long that takes.  */
#ifdef VSWTCH
  tty.main.c_cc[VSWTCH] = CDISABLE;	/* Turn off shell layering use
					   of C-z */
#endif /* VSWTCH */

#ifdef VSUSP
  tty.main.c_cc[VSUSP] = CDISABLE;	/* Turn off handling of C-z.  */
#endif /* VSUSP */
#ifdef V_DSUSP
  tty.main.c_cc[V_DSUSP] = CDISABLE; /* Turn off handling of C-y.  */
#endif /* V_DSUSP */
#ifdef VDSUSP /* Some systems have VDSUSP, some have V_DSUSP.  */
  tty.main.c_cc[VDSUSP] = CDISABLE;
#endif /* VDSUSP */
#ifdef VLNEXT
  tty.main.c_cc[VLNEXT] = CDISABLE;
#endif /* VLNEXT */
#ifdef VREPRINT
  tty.main.c_cc[VREPRINT] = CDISABLE;
#endif /* VREPRINT */
#ifdef VWERASE
  tty.main.c_cc[VWERASE] = CDISABLE;
#endif /* VWERASE */
#ifdef VDISCARD
  tty.main.c_cc[VDISCARD] = CDISABLE;
#endif /* VDISCARD */

  if (tty_out->flow_control)
    {
#ifdef VSTART
      tty.main.c_cc[VSTART] = '\021';
#endif /* VSTART */
#ifdef VSTOP
      tty.main.c_cc[VSTOP] = '\023';
#endif /* VSTOP */
    }
  else
    {
#ifdef VSTART
      tty.main.c_cc[VSTART] = CDISABLE;
#endif /* VSTART */
#ifdef VSTOP
      tty.main.c_cc[VSTOP] = CDISABLE;
#endif /* VSTOP */
    }

#ifdef AIX
  tty.main.c_cc[VSTRT] = CDISABLE;
  tty.main.c_cc[VSTOP] = CDISABLE;
  tty.main.c_cc[VSUSP] = CDISABLE;
  tty.main.c_cc[VDSUSP] = CDISABLE;
  if (tty_out->flow_control)
    {
#ifdef VSTART
      tty.main.c_cc[VSTART] = '\021';
#endif /* VSTART */
#ifdef VSTOP
      tty.main.c_cc[VSTOP] = '\023';
#endif /* VSTOP */
    }
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.
     This really serves a purpose only if running in an XTERM window
     or via TELNET or the like, but does no harm elsewhere.  */
  tty.main.c_iflag &= ~IGNBRK;
  tty.main.c_iflag &= ~BRKINT;
#endif
#endif /* not DOS_NT */

#ifdef MSDOS	/* Demacs 1.1.2 91/10/20 Manabu Higashida, MW Aug 1993 */
  if (!tty_out->term_initted)
    internal_terminal_init ();
  dos_ttraw (tty_out);
#endif

  emacs_set_tty (fileno (tty_out->input), &tty, 0);

  /* This code added to insure that, if flow-control is not to be used,
     we have an unlocked terminal at the start. */

#ifdef TCXONC
  if (!tty_out->flow_control) ioctl (fileno (tty_out->input), TCXONC, 1);
#endif
#ifdef TIOCSTART
  if (!tty_out->flow_control) ioctl (fileno (tty_out->input), TIOCSTART, 0);
#endif

#if !defined (DOS_NT)
#ifdef TCOON
  if (!tty_out->flow_control) tcflow (fileno (tty_out->input), TCOON);
#endif
#endif

#ifdef F_GETOWN
  if (interrupt_input)
    {
      old_fcntl_owner[fileno (tty_out->input)] =
        fcntl (fileno (tty_out->input), F_GETOWN, 0);
      fcntl (fileno (tty_out->input), F_SETOWN, getpid ());
      init_sigio (fileno (tty_out->input));
#ifdef HAVE_GPM
      if (gpm_tty == tty_out)
	{
	  /* Arrange for mouse events to give us SIGIO signals.  */
	  fcntl (gpm_fd, F_SETOWN, getpid ());
	  fcntl (gpm_fd, F_SETFL, fcntl (gpm_fd, F_GETFL, 0) | O_NONBLOCK);
	  init_sigio (gpm_fd);
	}
#endif /* HAVE_GPM */
    }
#endif /* F_GETOWN */

#ifdef _IOFBF
  /* This symbol is defined on recent USG systems.
     Someone says without this call USG won't really buffer the file
     even with a call to setbuf. */
  setvbuf (tty_out->output, (char *) _sobuf, _IOFBF, sizeof _sobuf);
#else
  setbuf (tty_out->output, (char *) _sobuf);
#endif

  if (tty_out->terminal->set_terminal_modes_hook)
    tty_out->terminal->set_terminal_modes_hook (tty_out->terminal);

  if (!tty_out->term_initted)
    {
      Lisp_Object tail, frame;
      FOR_EACH_FRAME (tail, frame)
        {
          /* XXX This needs to be revised. */
          if (FRAME_TERMCAP_P (XFRAME (frame))
              && FRAME_TTY (XFRAME (frame)) == tty_out)
            init_frame_faces (XFRAME (frame));
        }
    }

  if (tty_out->term_initted && no_redraw_on_reenter)
    {
      /* We used to call "direct_output_forward_char(0)" here,
	 but it's not clear why, since it may not do anything anyway.  */
    }
  else
    {
      Lisp_Object tail, frame;
      frame_garbaged = 1;
      FOR_EACH_FRAME (tail, frame)
        {
          if ((FRAME_TERMCAP_P (XFRAME (frame))
	       || FRAME_MSDOS_P (XFRAME (frame)))
              && FRAME_TTY (XFRAME (frame)) == tty_out)
            FRAME_GARBAGED_P (XFRAME (frame)) = 1;
        }
    }

  tty_out->term_initted = 1;
}

/* Return true if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */

bool
tabs_safe_p (int fd)
{
  struct emacs_tty etty;

  emacs_get_tty (fd, &etty);
#ifndef DOS_NT
#ifdef TABDLY
  return ((etty.main.c_oflag & TABDLY) != TAB3);
#else /* not TABDLY */
  return 1;
#endif /* not TABDLY */
#else /* DOS_NT */
  return 0;
#endif /* DOS_NT */
}

/* Discard echoing.  */

void
suppress_echo_on_tty (int fd)
{
  struct emacs_tty etty;

  emacs_get_tty (fd, &etty);
#ifdef DOS_NT
  /* Set raw input mode.  */
  etty.main = 0;
#else
  etty.main.c_lflag &= ~ICANON;	/* Disable buffering */
  etty.main.c_lflag &= ~ECHO;	/* Disable echoing */
#endif /* ! WINDOWSNT */
  emacs_set_tty (fd, &etty, 0);
}

/* Get terminal size from system.
   Store number of lines into *HEIGHTP and width into *WIDTHP.
   We store 0 if there's no valid information.  */

void
get_tty_size (int fd, int *widthp, int *heightp)
{
#if defined TIOCGWINSZ

  /* BSD-style.  */
  struct winsize size;

  if (ioctl (fd, TIOCGWINSZ, &size) == -1)
    *widthp = *heightp = 0;
  else
    {
      *widthp = size.ws_col;
      *heightp = size.ws_row;
    }

#elif defined TIOCGSIZE

  /* SunOS - style.  */
  struct ttysize size;

  if (ioctl (fd, TIOCGSIZE, &size) == -1)
    *widthp = *heightp = 0;
  else
    {
      *widthp = size.ts_cols;
      *heightp = size.ts_lines;
    }

#elif defined WINDOWSNT

  CONSOLE_SCREEN_BUFFER_INFO info;
  if (GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info))
    {
      *widthp = info.srWindow.Right - info.srWindow.Left + 1;
      *heightp = info.srWindow.Bottom - info.srWindow.Top + 1;
    }
  else
    *widthp = *heightp = 0;

#elif defined MSDOS

  *widthp = ScreenCols ();
  *heightp = ScreenRows ();

#else /* system doesn't know size */

  *widthp = 0;
  *heightp = 0;

#endif
}

/* Set the logical window size associated with descriptor FD
   to HEIGHT and WIDTH.  This is used mainly with ptys.
   Return a negative value on failure.  */

int
set_window_size (int fd, int height, int width)
{
#ifdef TIOCSWINSZ

  /* BSD-style.  */
  struct winsize size;
  size.ws_row = height;
  size.ws_col = width;

  return ioctl (fd, TIOCSWINSZ, &size);

#else
#ifdef TIOCSSIZE

  /* SunOS - style.  */
  struct ttysize size;
  size.ts_lines = height;
  size.ts_cols = width;

  return ioctl (fd, TIOCGSIZE, &size);
#else
  return -1;
#endif /* not SunOS-style */
#endif /* not BSD-style */
}



/* Prepare all terminal devices for exiting Emacs. */

void
reset_all_sys_modes (void)
{
  struct tty_display_info *tty;
  for (tty = tty_list; tty; tty = tty->next)
    reset_sys_modes (tty);
}

/* Prepare the terminal for closing it; move the cursor to the
   bottom of the frame, turn off interrupt-driven I/O, etc.  */

void
reset_sys_modes (struct tty_display_info *tty_out)
{
  if (noninteractive)
    {
      fflush_unlocked (stdout);
      return;
    }
  if (!tty_out->term_initted)
    return;

  if (!tty_out->output)
    return;                     /* The tty is suspended. */

  /* Go to and clear the last line of the terminal. */

  cmgoto (tty_out, FrameRows (tty_out) - 1, 0);

  /* Code adapted from tty_clear_end_of_line. */
  if (tty_out->TS_clr_line)
    {
      emacs_tputs (tty_out, tty_out->TS_clr_line, 1, cmputc);
    }
  else
    {			/* have to do it the hard way */
      tty_turn_off_insert (tty_out);

      for (int i = cursorX (tty_out); i < FrameCols (tty_out) - 1; i++)
	fputc_unlocked (' ', tty_out->output);
    }

  cmgoto (tty_out, FrameRows (tty_out) - 1, 0);
  fflush_unlocked (tty_out->output);

  if (tty_out->terminal->reset_terminal_modes_hook)
    tty_out->terminal->reset_terminal_modes_hook (tty_out->terminal);

  /* Avoid possible loss of output when changing terminal modes.  */
  while (fdatasync (fileno (tty_out->output)) != 0 && errno == EINTR)
    continue;

#ifndef DOS_NT
#ifdef F_SETOWN
  if (interrupt_input)
    {
      reset_sigio (fileno (tty_out->input));
      fcntl (fileno (tty_out->input), F_SETOWN,
             old_fcntl_owner[fileno (tty_out->input)]);
    }
#endif /* F_SETOWN */
  fcntl (fileno (tty_out->input), F_SETFL,
         fcntl (fileno (tty_out->input), F_GETFL, 0) & ~O_NONBLOCK);
#endif

  if (tty_out->old_tty)
    while (emacs_set_tty (fileno (tty_out->input),
                          tty_out->old_tty, 0) < 0 && errno == EINTR)
      ;

#ifdef MSDOS	/* Demacs 1.1.2 91/10/20 Manabu Higashida */
  dos_ttcooked ();
#endif

  widen_foreground_group (fileno (tty_out->input));
}

#ifdef HAVE_PTYS

/* Set up the proper status flags for use of a pty.  */

void
setup_pty (int fd)
{
  /* I'm told that TOICREMOTE does not mean control chars
     "can't be sent" but rather that they don't have
     input-editing or signaling effects.
     That should be good, because we have other ways
     to do those things in Emacs.
     However, telnet mode seems not to work on 4.2.
     So TIOCREMOTE is turned off now. */

  /* Under hp-ux, if TIOCREMOTE is turned on, some calls
     will hang.  In particular, the "timeout" feature (which
     causes a read to return if there is no data available)
     does this.  Also it is known that telnet mode will hang
     in such a way that Emacs must be stopped (perhaps this
     is the same problem).

     If TIOCREMOTE is turned off, then there is a bug in
     hp-ux which sometimes loses data.  Apparently the
     code which blocks the master process when the internal
     buffer fills up does not work.  Other than this,
     though, everything else seems to work fine.

     Since the latter lossage is more benign, we may as well
     lose that way.  -- cph */
#ifdef FIONBIO
#if defined (UNIX98_PTYS)
  {
    int on = 1;
    ioctl (fd, FIONBIO, &on);
  }
#endif
#endif
}
#endif /* HAVE_PTYS */

void
init_system_name (void)
{
  if (!build_details)
    {
      /* Set system-name to nil so that the build is deterministic.  */
      Vsystem_name = Qnil;
      return;
    }
  char *hostname_alloc = NULL;
  char *hostname;
#ifndef HAVE_GETHOSTNAME
  struct utsname uts;
  uname (&uts);
  hostname = uts.nodename;
#else /* HAVE_GETHOSTNAME */
  char hostname_buf[256];
  ptrdiff_t hostname_size = sizeof hostname_buf;
  hostname = hostname_buf;

  /* Try to get the host name; if the buffer is too short, try
     again.  Apparently, the only indication gethostname gives of
     whether the buffer was large enough is the presence or absence
     of a '\0' in the string.  Eech.  */
  for (;;)
    {
      gethostname (hostname, hostname_size - 1);
      hostname[hostname_size - 1] = '\0';

      /* Was the buffer large enough for the '\0'?  */
      if (strlen (hostname) < hostname_size - 1)
	break;

      hostname = hostname_alloc = xpalloc (hostname_alloc, &hostname_size, 1,
					   min (PTRDIFF_MAX, SIZE_MAX), 1);
    }
#endif /* HAVE_GETHOSTNAME */
  char *p;
  for (p = hostname; *p; p++)
    if (*p == ' ' || *p == '\t')
      *p = '-';
  if (! (STRINGP (Vsystem_name) && SBYTES (Vsystem_name) == p - hostname
	 && strcmp (SSDATA (Vsystem_name), hostname) == 0))
    Vsystem_name = build_string (hostname);
  xfree (hostname_alloc);
}

sigset_t empty_mask;

static struct sigaction process_fatal_action;

static int
emacs_sigaction_flags (void)
{
#ifdef SA_RESTART
  /* SA_RESTART causes interruptible functions with timeouts (e.g.,
     'select') to reset their timeout on some platforms (e.g.,
     HP-UX 11), which is not what we want.  Also, when Emacs is
     interactive, we don't want SA_RESTART because we need to poll
     for pending input so we need long-running syscalls to be interrupted
     after a signal that sets pending_signals.

     Non-interactive keyboard input goes through stdio, where we
     always want restartable system calls.  */
  if (noninteractive)
    return SA_RESTART;
#endif
  return 0;
}

/* Store into *ACTION a signal action suitable for Emacs, with handler
   HANDLER.  */
void
emacs_sigaction_init (struct sigaction *action, signal_handler_t handler)
{
  sigemptyset (&action->sa_mask);

  /* When handling a signal, block nonfatal system signals that are caught
     by Emacs.  This makes race conditions less likely.  */
  sigaddset (&action->sa_mask, SIGALRM);
#ifdef SIGCHLD
  sigaddset (&action->sa_mask, SIGCHLD);
#endif
#ifdef SIGDANGER
  sigaddset (&action->sa_mask, SIGDANGER);
#endif
#ifdef PROFILER_CPU_SUPPORT
  sigaddset (&action->sa_mask, SIGPROF);
#endif
#ifdef SIGWINCH
  sigaddset (&action->sa_mask, SIGWINCH);
#endif
  if (! noninteractive)
    {
      sigaddset (&action->sa_mask, SIGINT);
      sigaddset (&action->sa_mask, SIGQUIT);
#ifdef USABLE_SIGIO
      sigaddset (&action->sa_mask, SIGIO);
#endif
    }

  action->sa_handler = handler;
  action->sa_flags = emacs_sigaction_flags ();
}

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
static pthread_t main_thread_id;
#endif

/* SIG has arrived at the current process.  Deliver it to the main
   thread, which should handle it with HANDLER.  (Delivering the
   signal to some other thread might not work if the other thread is
   about to exit.)

   If we are on the main thread, handle the signal SIG with HANDLER.
   Otherwise, redirect the signal to the main thread, blocking it from
   this thread.  POSIX says any thread can receive a signal that is
   associated with a process, process group, or asynchronous event.
   On GNU/Linux the main thread typically gets a process signal unless
   it's blocked, but other systems (FreeBSD at least) can deliver the
   signal to other threads.  */
void
deliver_process_signal (int sig, signal_handler_t handler)
{
  /* Preserve errno, to avoid race conditions with signal handlers that
     might change errno.  Races can occur even in single-threaded hosts.  */
  int old_errno = errno;

  bool on_main_thread = true;
#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
  if (! pthread_equal (pthread_self (), main_thread_id))
    {
      sigset_t blocked;
      sigemptyset (&blocked);
      sigaddset (&blocked, sig);
      pthread_sigmask (SIG_BLOCK, &blocked, 0);
      pthread_kill (main_thread_id, sig);
      on_main_thread = false;
    }
#endif
  if (on_main_thread)
    handler (sig);

  errno = old_errno;
}

/* Static location to save a fatal backtrace in a thread.
   FIXME: If two subsidiary threads fail simultaneously, the resulting
   backtrace may be garbage.  */
enum { BACKTRACE_LIMIT_MAX = 500 };
static void *thread_backtrace_buffer[BACKTRACE_LIMIT_MAX + 1];
static int thread_backtrace_npointers;

/* SIG has arrived at the current thread.
   If we are on the main thread, handle the signal SIG with HANDLER.
   Otherwise, this is a fatal error in the handling thread.  */
static void
deliver_thread_signal (int sig, signal_handler_t handler)
{
  int old_errno = errno;

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
  if (! pthread_equal (pthread_self (), main_thread_id))
    {
      thread_backtrace_npointers
	= backtrace (thread_backtrace_buffer, BACKTRACE_LIMIT_MAX);
      sigaction (sig, &process_fatal_action, 0);
      pthread_kill (main_thread_id, sig);

      /* Avoid further damage while the main thread is exiting.  */
      while (1)
	sigsuspend (&empty_mask);
    }
#endif

  handler (sig);
  errno = old_errno;
}

#if !HAVE_DECL_SYS_SIGLIST
# undef sys_siglist
# ifdef _sys_siglist
#  define sys_siglist _sys_siglist
# elif HAVE_DECL___SYS_SIGLIST
#  define sys_siglist __sys_siglist
# else
#  define sys_siglist my_sys_siglist
static char const *sys_siglist[NSIG];
# endif
#endif

#ifdef _sys_nsig
# define sys_siglist_entries _sys_nsig
#else
# define sys_siglist_entries NSIG
#endif

/* Handle bus errors, invalid instruction, etc.  */
static void
handle_fatal_signal (int sig)
{
  terminate_due_to_signal (sig, 40);
}

static void
deliver_fatal_signal (int sig)
{
  deliver_process_signal (sig, handle_fatal_signal);
}

static void
deliver_fatal_thread_signal (int sig)
{
  deliver_thread_signal (sig, handle_fatal_signal);
}

static _Noreturn void
handle_arith_signal (int sig)
{
  pthread_sigmask (SIG_SETMASK, &empty_mask, 0);
  xsignal0 (Qarith_error);
}

#if defined HAVE_STACK_OVERFLOW_HANDLING && !defined WINDOWSNT

/* Alternate stack used by SIGSEGV handler below.  */

static unsigned char sigsegv_stack[SIGSTKSZ];


/* Return true if SIGINFO indicates a stack overflow.  */

static bool
stack_overflow (siginfo_t *siginfo)
{
  if (!attempt_stack_overflow_recovery)
    return false;

  /* In theory, a more-accurate heuristic can be obtained by using
     GNU/Linux pthread_getattr_np along with POSIX pthread_attr_getstack
     and pthread_attr_getguardsize to find the location and size of the
     guard area.  In practice, though, these functions are so hard to
     use reliably that they're not worth bothering with.  E.g., see:
     https://sourceware.org/bugzilla/show_bug.cgi?id=16291
     Other operating systems also have problems, e.g., Solaris's
     stack_violation function is tailor-made for this problem, but it
     doesn't work on Solaris 11.2 x86-64 with a 32-bit executable.

     GNU libsigsegv is overkill for Emacs; otherwise it might be a
     candidate here.  */

  if (!siginfo)
    return false;

  /* The faulting address.  */
  char *addr = siginfo->si_addr;
  if (!addr)
    return false;

  /* The known top and bottom of the stack.  The actual stack may
     extend a bit beyond these boundaries.  */
  char *bot = stack_bottom;
  char *top = current_thread->stack_top;

  /* Log base 2 of the stack heuristic ratio.  This ratio is the size
     of the known stack divided by the size of the guard area past the
     end of the stack top.  The heuristic is that a bad address is
     considered to be a stack overflow if it occurs within
     stacksize>>LG_STACK_HEURISTIC bytes above the top of the known
     stack.  This heuristic is not exactly correct but it's good
     enough in practice.  */
  enum { LG_STACK_HEURISTIC = 8 };

  if (bot < top)
    return 0 <= addr - top && addr - top < (top - bot) >> LG_STACK_HEURISTIC;
  else
    return 0 <= top - addr && top - addr < (bot - top) >> LG_STACK_HEURISTIC;
}


/* Attempt to recover from SIGSEGV caused by C stack overflow.  */

static void
handle_sigsegv (int sig, siginfo_t *siginfo, void *arg)
{
  /* Hard GC error may lead to stack overflow caused by
     too nested calls to mark_object.  No way to survive.  */
  bool fatal = gc_in_progress;

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
  if (!fatal && !pthread_equal (pthread_self (), main_thread_id))
    fatal = true;
#endif

  if (!fatal && stack_overflow (siginfo))
    siglongjmp (return_to_command_loop, 1);

  /* Otherwise we can't do anything with this.  */
  deliver_fatal_thread_signal (sig);
}

/* Return true if we have successfully set up SIGSEGV handler on alternate
   stack.  Otherwise we just treat SIGSEGV among the rest of fatal signals.  */

static bool
init_sigsegv (void)
{
  struct sigaction sa;
  stack_t ss;

  ss.ss_sp = sigsegv_stack;
  ss.ss_size = sizeof (sigsegv_stack);
  ss.ss_flags = 0;
  if (sigaltstack (&ss, NULL) < 0)
    return 0;

  sigfillset (&sa.sa_mask);
  sa.sa_sigaction = handle_sigsegv;
  sa.sa_flags = SA_SIGINFO | SA_ONSTACK | emacs_sigaction_flags ();
  return sigaction (SIGSEGV, &sa, NULL) < 0 ? 0 : 1;
}

#else /* not HAVE_STACK_OVERFLOW_HANDLING or WINDOWSNT */

static bool
init_sigsegv (void)
{
  return 0;
}

#endif /* HAVE_STACK_OVERFLOW_HANDLING && !WINDOWSNT */

static void
deliver_arith_signal (int sig)
{
  deliver_thread_signal (sig, handle_arith_signal);
}

#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
static void
handle_danger_signal (int sig)
{
  malloc_warning ("Operating system warns that virtual memory is running low.\n");

  /* It might be unsafe to call do_auto_save now.  */
  force_auto_save_soon ();
}

static void
deliver_danger_signal (int sig)
{
  deliver_process_signal (sig, handle_danger_signal);
}
#endif

/* Treat SIG as a terminating signal, unless it is already ignored and
   we are in --batch mode.  Among other things, this makes nohup work.  */
static void
maybe_fatal_sig (int sig)
{
  bool catch_sig = !noninteractive;
  if (!catch_sig)
    {
      struct sigaction old_action;
      sigaction (sig, 0, &old_action);
      catch_sig = old_action.sa_handler != SIG_IGN;
    }
  if (catch_sig)
    sigaction (sig, &process_fatal_action, 0);
}

void
init_signals (bool dumping)
{
  struct sigaction thread_fatal_action;
  struct sigaction action;

  sigemptyset (&empty_mask);

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
  main_thread_id = pthread_self ();
#endif

#if !HAVE_DECL_SYS_SIGLIST && !defined _sys_siglist
  if (! initialized)
    {
      sys_siglist[SIGABRT] = "Aborted";
# ifdef SIGAIO
      sys_siglist[SIGAIO] = "LAN I/O interrupt";
# endif
      sys_siglist[SIGALRM] = "Alarm clock";
# ifdef SIGBUS
      sys_siglist[SIGBUS] = "Bus error";
# endif
# ifdef SIGCHLD
      sys_siglist[SIGCHLD] = "Child status changed";
# endif
# ifdef SIGCONT
      sys_siglist[SIGCONT] = "Continued";
# endif
# ifdef SIGDANGER
      sys_siglist[SIGDANGER] = "Swap space dangerously low";
# endif
# ifdef SIGDGNOTIFY
      sys_siglist[SIGDGNOTIFY] = "Notification message in queue";
# endif
# ifdef SIGEMT
      sys_siglist[SIGEMT] = "Emulation trap";
# endif
      sys_siglist[SIGFPE] = "Arithmetic exception";
# ifdef SIGFREEZE
      sys_siglist[SIGFREEZE] = "SIGFREEZE";
# endif
# ifdef SIGGRANT
      sys_siglist[SIGGRANT] = "Monitor mode granted";
# endif
      sys_siglist[SIGHUP] = "Hangup";
      sys_siglist[SIGILL] = "Illegal instruction";
      sys_siglist[SIGINT] = "Interrupt";
# ifdef SIGIO
      sys_siglist[SIGIO] = "I/O possible";
# endif
# ifdef SIGIOINT
      sys_siglist[SIGIOINT] = "I/O intervention required";
# endif
# ifdef SIGIOT
      sys_siglist[SIGIOT] = "IOT trap";
# endif
      sys_siglist[SIGKILL] = "Killed";
# ifdef SIGLOST
      sys_siglist[SIGLOST] = "Resource lost";
# endif
# ifdef SIGLWP
      sys_siglist[SIGLWP] = "SIGLWP";
# endif
# ifdef SIGMSG
      sys_siglist[SIGMSG] = "Monitor mode data available";
# endif
# ifdef SIGPHONE
      sys_siglist[SIGWIND] = "SIGPHONE";
# endif
      sys_siglist[SIGPIPE] = "Broken pipe";
# ifdef SIGPOLL
      sys_siglist[SIGPOLL] = "Pollable event occurred";
# endif
# ifdef SIGPROF
      sys_siglist[SIGPROF] = "Profiling timer expired";
# endif
# ifdef SIGPTY
      sys_siglist[SIGPTY] = "PTY I/O interrupt";
# endif
# ifdef SIGPWR
      sys_siglist[SIGPWR] = "Power-fail restart";
# endif
      sys_siglist[SIGQUIT] = "Quit";
# ifdef SIGRETRACT
      sys_siglist[SIGRETRACT] = "Need to relinquish monitor mode";
# endif
# ifdef SIGSAK
      sys_siglist[SIGSAK] = "Secure attention";
# endif
      sys_siglist[SIGSEGV] = "Segmentation violation";
# ifdef SIGSOUND
      sys_siglist[SIGSOUND] = "Sound completed";
# endif
# ifdef SIGSTOP
      sys_siglist[SIGSTOP] = "Stopped (signal)";
# endif
# ifdef SIGSTP
      sys_siglist[SIGSTP] = "Stopped (user)";
# endif
# ifdef SIGSYS
      sys_siglist[SIGSYS] = "Bad argument to system call";
# endif
      sys_siglist[SIGTERM] = "Terminated";
# ifdef SIGTHAW
      sys_siglist[SIGTHAW] = "SIGTHAW";
# endif
# ifdef SIGTRAP
      sys_siglist[SIGTRAP] = "Trace/breakpoint trap";
# endif
# ifdef SIGTSTP
      sys_siglist[SIGTSTP] = "Stopped (user)";
# endif
# ifdef SIGTTIN
      sys_siglist[SIGTTIN] = "Stopped (tty input)";
# endif
# ifdef SIGTTOU
      sys_siglist[SIGTTOU] = "Stopped (tty output)";
# endif
# ifdef SIGURG
      sys_siglist[SIGURG] = "Urgent I/O condition";
# endif
# ifdef SIGUSR1
      sys_siglist[SIGUSR1] = "User defined signal 1";
# endif
# ifdef SIGUSR2
      sys_siglist[SIGUSR2] = "User defined signal 2";
# endif
# ifdef SIGVTALRM
      sys_siglist[SIGVTALRM] = "Virtual timer expired";
# endif
# ifdef SIGWAITING
      sys_siglist[SIGWAITING] = "Process's LWPs are blocked";
# endif
# ifdef SIGWINCH
      sys_siglist[SIGWINCH] = "Window size changed";
# endif
# ifdef SIGWIND
      sys_siglist[SIGWIND] = "SIGWIND";
# endif
# ifdef SIGXCPU
      sys_siglist[SIGXCPU] = "CPU time limit exceeded";
# endif
# ifdef SIGXFSZ
      sys_siglist[SIGXFSZ] = "File size limit exceeded";
# endif
    }
#endif /* !HAVE_DECL_SYS_SIGLIST && !_sys_siglist */

  /* Don't alter signal handlers if dumping.  On some machines,
     changing signal handlers sets static data that would make signals
     fail to work right when the dumped Emacs is run.  */
  if (dumping)
    return;

  sigfillset (&process_fatal_action.sa_mask);
  process_fatal_action.sa_handler = deliver_fatal_signal;
  process_fatal_action.sa_flags = emacs_sigaction_flags ();

  sigfillset (&thread_fatal_action.sa_mask);
  thread_fatal_action.sa_handler = deliver_fatal_thread_signal;
  thread_fatal_action.sa_flags = process_fatal_action.sa_flags;

  /* SIGINT may need special treatment on MS-Windows.  See
     https://lists.gnu.org/r/emacs-devel/2010-09/msg01062.html
     Please update the doc of kill-emacs, kill-emacs-hook, and
     NEWS if you change this.  */

  maybe_fatal_sig (SIGHUP);
  maybe_fatal_sig (SIGINT);
  maybe_fatal_sig (SIGTERM);

  /* Emacs checks for write errors, so it can safely ignore SIGPIPE.
     However, in batch mode leave SIGPIPE alone, as that causes Emacs
     to behave more like typical batch applications do.  */
  if (! noninteractive)
    signal (SIGPIPE, SIG_IGN);

  sigaction (SIGQUIT, &process_fatal_action, 0);
  sigaction (SIGILL, &thread_fatal_action, 0);
  sigaction (SIGTRAP, &thread_fatal_action, 0);

  /* Typically SIGFPE is thread-specific and is fatal, like SIGILL.
     But on a non-IEEE host SIGFPE can come from a trap in the Lisp
     interpreter's floating point operations, so treat SIGFPE as an
     arith-error if it arises in the main thread.  */
  if (IEEE_FLOATING_POINT)
    sigaction (SIGFPE, &thread_fatal_action, 0);
  else
    {
      emacs_sigaction_init (&action, deliver_arith_signal);
      sigaction (SIGFPE, &action, 0);
    }

#ifdef SIGUSR1
  add_user_signal (SIGUSR1, "sigusr1");
#endif
#ifdef SIGUSR2
  add_user_signal (SIGUSR2, "sigusr2");
#endif
  sigaction (SIGABRT, &thread_fatal_action, 0);
#ifdef SIGPRE
  sigaction (SIGPRE, &thread_fatal_action, 0);
#endif
#ifdef SIGORE
  sigaction (SIGORE, &thread_fatal_action, 0);
#endif
#ifdef SIGUME
  sigaction (SIGUME, &thread_fatal_action, 0);
#endif
#ifdef SIGDLK
  sigaction (SIGDLK, &process_fatal_action, 0);
#endif
#ifdef SIGCPULIM
  sigaction (SIGCPULIM, &process_fatal_action, 0);
#endif
#ifdef SIGIOT
  sigaction (SIGIOT, &thread_fatal_action, 0);
#endif
#ifdef SIGEMT
  sigaction (SIGEMT, &thread_fatal_action, 0);
#endif
#ifdef SIGBUS
  sigaction (SIGBUS, &thread_fatal_action, 0);
#endif
  if (!init_sigsegv ())
    sigaction (SIGSEGV, &thread_fatal_action, 0);
#ifdef SIGSYS
  sigaction (SIGSYS, &thread_fatal_action, 0);
#endif
  sigaction (SIGTERM, &process_fatal_action, 0);
#ifdef SIGPROF
  signal (SIGPROF, SIG_IGN);
#endif
#ifdef SIGVTALRM
  sigaction (SIGVTALRM, &process_fatal_action, 0);
#endif
#ifdef SIGXCPU
  sigaction (SIGXCPU, &process_fatal_action, 0);
#endif
#ifdef SIGXFSZ
  sigaction (SIGXFSZ, &process_fatal_action, 0);
#endif

#ifdef SIGDANGER
  /* This just means available memory is getting low.  */
  emacs_sigaction_init (&action, deliver_danger_signal);
  sigaction (SIGDANGER, &action, 0);
#endif

  /* AIX-specific signals.  */
#ifdef SIGGRANT
  sigaction (SIGGRANT, &process_fatal_action, 0);
#endif
#ifdef SIGMIGRATE
  sigaction (SIGMIGRATE, &process_fatal_action, 0);
#endif
#ifdef SIGMSG
  sigaction (SIGMSG, &process_fatal_action, 0);
#endif
#ifdef SIGRETRACT
  sigaction (SIGRETRACT, &process_fatal_action, 0);
#endif
#ifdef SIGSAK
  sigaction (SIGSAK, &process_fatal_action, 0);
#endif
#ifdef SIGSOUND
  sigaction (SIGSOUND, &process_fatal_action, 0);
#endif
#ifdef SIGTALRM
  sigaction (SIGTALRM, &thread_fatal_action, 0);
#endif
}

#ifndef HAVE_RANDOM
#ifdef random
#define HAVE_RANDOM
#endif
#endif

/* Figure out how many bits the system's random number generator uses.
   `random' and `lrand48' are assumed to return 31 usable bits.
   BSD `rand' returns a 31 bit value but the low order bits are unusable;
   so we'll shift it and treat it like the 15-bit USG `rand'.  */

#ifndef RAND_BITS
# ifdef HAVE_RANDOM
#  define RAND_BITS 31
# else /* !HAVE_RANDOM */
#  ifdef HAVE_LRAND48
#   define RAND_BITS 31
#   define random lrand48
#  else /* !HAVE_LRAND48 */
#   define RAND_BITS 15
#   if RAND_MAX == 32767
#    define random rand
#   else /* RAND_MAX != 32767 */
#    if RAND_MAX == 2147483647
#     define random() (rand () >> 16)
#    else /* RAND_MAX != 2147483647 */
#     ifdef USG
#      define random rand
#     else
#      define random() (rand () >> 16)
#     endif /* !USG */
#    endif /* RAND_MAX != 2147483647 */
#   endif /* RAND_MAX != 32767 */
#  endif /* !HAVE_LRAND48 */
# endif /* !HAVE_RANDOM */
#endif /* !RAND_BITS */

#ifdef HAVE_RANDOM
typedef unsigned int random_seed;
static void set_random_seed (random_seed arg) { srandom (arg); }
#elif defined HAVE_LRAND48
/* Although srand48 uses a long seed, this is unsigned long to avoid
   undefined behavior on signed integer overflow in init_random.  */
typedef unsigned long int random_seed;
static void set_random_seed (random_seed arg) { srand48 (arg); }
#else
typedef unsigned int random_seed;
static void set_random_seed (random_seed arg) { srand (arg); }
#endif

void
seed_random (void *seed, ptrdiff_t seed_size)
{
  random_seed arg = 0;
  unsigned char *argp = (unsigned char *) &arg;
  unsigned char *seedp = seed;
  for (ptrdiff_t i = 0; i < seed_size; i++)
    argp[i % sizeof arg] ^= seedp[i];
  set_random_seed (arg);
}

void
init_random (void)
{
  random_seed v;
  bool success = false;

  /* First, try seeding the PRNG from the operating system's entropy
     source.  This approach is both fast and secure.  */
#ifdef WINDOWSNT
  success = w32_init_random (&v, sizeof v) == 0;
#else
  int fd = emacs_open ("/dev/urandom", O_RDONLY, 0);
  if (0 <= fd)
    {
      success = emacs_read (fd, &v, sizeof v) == sizeof v;
      close (fd);
    }
#endif

  /* If that didn't work, try using GnuTLS, which is secure, but on
     some systems, can be somewhat slow.  */
  if (!success)
    success = EQ (emacs_gnutls_global_init (), Qt)
      && gnutls_rnd (GNUTLS_RND_NONCE, &v, sizeof v) == 0;

  /* If _that_ didn't work, just use the current time value and PID.
     It's at least better than XKCD 221.  */
  if (!success)
    {
      struct timespec t = current_timespec ();
      v = getpid () ^ t.tv_sec ^ t.tv_nsec;
    }

  set_random_seed (v);
}

/*
 * Return a nonnegative random integer out of whatever we've got.
 * It contains enough bits to make a random (signed) Emacs fixnum.
 * This suffices even for a 64-bit architecture with a 15-bit rand.
 */
EMACS_INT
get_random (void)
{
  EMACS_UINT val = 0;
  int i;
  for (i = 0; i < (FIXNUM_BITS + RAND_BITS - 1) / RAND_BITS; i++)
    val = (random () ^ (val << RAND_BITS)
	   ^ (val >> (EMACS_INT_WIDTH - RAND_BITS)));
  val ^= val >> (EMACS_INT_WIDTH - FIXNUM_BITS);
  return val & INTMASK;
}

#ifndef HAVE_SNPRINTF
/* Approximate snprintf as best we can on ancient hosts that lack it.  */
int
snprintf (char *buf, size_t bufsize, char const *format, ...)
{
  ptrdiff_t size = min (bufsize, PTRDIFF_MAX);
  ptrdiff_t nbytes = size - 1;
  va_list ap;

  if (size)
    {
      va_start (ap, format);
      nbytes = doprnt (buf, size, format, 0, ap);
      va_end (ap);
    }

  if (nbytes == size - 1)
    {
      /* Calculate the length of the string that would have been created
	 had the buffer been large enough.  */
      char stackbuf[4000];
      char *b = stackbuf;
      ptrdiff_t bsize = sizeof stackbuf;
      va_start (ap, format);
      nbytes = evxprintf (&b, &bsize, stackbuf, -1, format, ap);
      va_end (ap);
      if (b != stackbuf)
	xfree (b);
    }

  if (INT_MAX < nbytes)
    {
#ifdef EOVERFLOW
      errno = EOVERFLOW;
#else
      errno = EDOM;
#endif
      return -1;
    }
  return nbytes;
}
#endif

/* If a backtrace is available, output the top lines of it to stderr.
   Do not output more than BACKTRACE_LIMIT or BACKTRACE_LIMIT_MAX lines.
   This function may be called from a signal handler, so it should
   not invoke async-unsafe functions like malloc.

   If BACKTRACE_LIMIT is -1, initialize tables that 'backtrace' uses
   but do not output anything.  This avoids some problems that can
   otherwise occur if the malloc arena is corrupted before 'backtrace'
   is called, since 'backtrace' may call malloc if the tables are not
   initialized.

   If the static variable THREAD_BACKTRACE_NPOINTERS is nonzero, a
   fatal error has occurred in some other thread; generate a thread
   backtrace instead, ignoring BACKTRACE_LIMIT.  */
void
emacs_backtrace (int backtrace_limit)
{
  void *main_backtrace_buffer[BACKTRACE_LIMIT_MAX + 1];
  int bounded_limit = min (backtrace_limit, BACKTRACE_LIMIT_MAX);
  void *buffer;
  int npointers;

  if (thread_backtrace_npointers)
    {
      buffer = thread_backtrace_buffer;
      npointers = thread_backtrace_npointers;
    }
  else
    {
      buffer = main_backtrace_buffer;

      /* Work around 'backtrace' bug; see Bug#19959 and glibc bug#18084.  */
      if (bounded_limit < 0)
	{
	  backtrace (buffer, 1);
	  return;
	}

      npointers = backtrace (buffer, bounded_limit + 1);
    }

  if (npointers)
    {
      emacs_write (STDERR_FILENO, "\nBacktrace:\n", 12);
      backtrace_symbols_fd (buffer, npointers, STDERR_FILENO);
      if (bounded_limit < npointers)
	emacs_write (STDERR_FILENO, "...\n", 4);
    }
}

#ifndef HAVE_NTGUI
void
emacs_abort (void)
{
  terminate_due_to_signal (SIGABRT, 40);
}
#endif

/* Open FILE for Emacs use, using open flags OFLAG and mode MODE.
   Use binary I/O on systems that care about text vs binary I/O.
   Arrange for subprograms to not inherit the file descriptor.
   Prefer a method that is multithread-safe, if available.
   Do not fail merely because the open was interrupted by a signal.
   Allow the user to quit.  */

int
emacs_open (const char *file, int oflags, int mode)
{
  int fd;
  if (! (oflags & O_TEXT))
    oflags |= O_BINARY;
  oflags |= O_CLOEXEC;
  while ((fd = open (file, oflags, mode)) < 0 && errno == EINTR)
    maybe_quit ();
  return fd;
}

/* Open FILE as a stream for Emacs use, with mode MODE.
   Act like emacs_open with respect to threads, signals, and quits.  */

FILE *
emacs_fopen (char const *file, char const *mode)
{
  int fd, omode, oflags;
  int bflag = 0;
  char const *m = mode;

  switch (*m++)
    {
    case 'r': omode = O_RDONLY; oflags = 0; break;
    case 'w': omode = O_WRONLY; oflags = O_CREAT | O_TRUNC; break;
    case 'a': omode = O_WRONLY; oflags = O_CREAT | O_APPEND; break;
    default: emacs_abort ();
    }

  while (*m)
    switch (*m++)
      {
      case '+': omode = O_RDWR; break;
      case 't': bflag = O_TEXT; break;
      default: /* Ignore.  */ break;
      }

  fd = emacs_open (file, omode | oflags | bflag, 0666);
  return fd < 0 ? 0 : fdopen (fd, mode);
}

/* Create a pipe for Emacs use.  */

int
emacs_pipe (int fd[2])
{
#ifdef MSDOS
  return pipe (fd);
#else  /* !MSDOS */
  return pipe2 (fd, O_BINARY | O_CLOEXEC);
#endif	/* !MSDOS */
}

/* Approximate posix_close and POSIX_CLOSE_RESTART well enough for Emacs.
   For the background behind this mess, please see Austin Group defect 529
   <http://austingroupbugs.net/view.php?id=529>.  */

#ifndef POSIX_CLOSE_RESTART
# define POSIX_CLOSE_RESTART 1
static int
posix_close (int fd, int flag)
{
  /* Only the POSIX_CLOSE_RESTART case is emulated.  */
  eassert (flag == POSIX_CLOSE_RESTART);

  /* Things are tricky if close (fd) returns -1 with errno == EINTR
     on a system that does not define POSIX_CLOSE_RESTART.

     In this case, in some systems (e.g., GNU/Linux, AIX) FD is
     closed, and retrying the close could inadvertently close a file
     descriptor allocated by some other thread.  In other systems
     (e.g., HP/UX) FD is not closed.  And in still other systems
     (e.g., macOS, Solaris), maybe FD is closed, maybe not, and in a
     multithreaded program there can be no way to tell.

     So, in this case, pretend that the close succeeded.  This works
     well on systems like GNU/Linux that close FD.  Although it may
     leak a file descriptor on other systems, the leak is unlikely and
     it's better to leak than to close a random victim.  */
  return close (fd) == 0 || errno == EINTR ? 0 : -1;
}
#endif

/* Close FD, retrying if interrupted.  If successful, return 0;
   otherwise, return -1 and set errno to a non-EINTR value.  Consider
   an EINPROGRESS error to be successful, as that's merely a signal
   arriving.  FD is always closed when this function returns, even
   when it returns -1.

   Do not call this function if FD is nonnegative and might already be closed,
   as that might close an innocent victim opened by some other thread.  */

int
emacs_close (int fd)
{
  while (1)
    {
      int r = posix_close (fd, POSIX_CLOSE_RESTART);
      if (r == 0)
	return r;
      if (!POSIX_CLOSE_RESTART || errno != EINTR)
	{
	  eassert (errno != EBADF || fd < 0);
	  return errno == EINPROGRESS ? 0 : r;
	}
    }
}

/* Maximum number of bytes to read or write in a single system call.
   This works around a serious bug in Linux kernels before 2.6.16; see
   <https://bugzilla.redhat.com/show_bug.cgi?format=multiple&id=612839>.
   It's likely to work around similar bugs in other operating systems, so do it
   on all platforms.  Round INT_MAX down to a page size, with the conservative
   assumption that page sizes are at most 2**18 bytes (any kernel with a
   page size larger than that shouldn't have the bug).  */
#ifndef MAX_RW_COUNT
#define MAX_RW_COUNT (INT_MAX >> 18 << 18)
#endif

/* Verify that MAX_RW_COUNT fits in the relevant standard types.  */
#ifndef SSIZE_MAX
# define SSIZE_MAX TYPE_MAXIMUM (ssize_t)
#endif
verify (MAX_RW_COUNT <= PTRDIFF_MAX);
verify (MAX_RW_COUNT <= SIZE_MAX);
verify (MAX_RW_COUNT <= SSIZE_MAX);

#ifdef WINDOWSNT
/* Verify that Emacs read requests cannot cause trouble, even in
   64-bit builds.  The last argument of 'read' is 'unsigned int', and
   the return value's type (see 'sys_read') is 'int'.  */
verify (MAX_RW_COUNT <= INT_MAX);
verify (MAX_RW_COUNT <= UINT_MAX);
#endif

/* Read from FD to a buffer BUF with size NBYTE.
   If interrupted, process any quits and pending signals immediately
   if INTERRUPTIBLE, and then retry the read unless quitting.
   Return the number of bytes read, which might be less than NBYTE.
   On error, set errno to a value other than EINTR, and return -1.  */
static ptrdiff_t
emacs_intr_read (int fd, void *buf, ptrdiff_t nbyte, bool interruptible)
{
  /* No caller should ever pass a too-large size to emacs_read.  */
  eassert (nbyte <= MAX_RW_COUNT);

  ssize_t result;

  do
    {
      if (interruptible)
	maybe_quit ();
      result = read (fd, buf, nbyte);
    }
  while (result < 0 && errno == EINTR);

  return result;
}

/* Read from FD to a buffer BUF with size NBYTE.
   If interrupted, retry the read.  Return the number of bytes read,
   which might be less than NBYTE.  On error, set errno to a value
   other than EINTR, and return -1.  */
ptrdiff_t
emacs_read (int fd, void *buf, ptrdiff_t nbyte)
{
  return emacs_intr_read (fd, buf, nbyte, false);
}

/* Like emacs_read, but also process quits and pending signals.  */
ptrdiff_t
emacs_read_quit (int fd, void *buf, ptrdiff_t nbyte)
{
  return emacs_intr_read (fd, buf, nbyte, true);
}

/* Write to FILEDES from a buffer BUF with size NBYTE, retrying if
   interrupted or if a partial write occurs.  Process any quits
   immediately if INTERRUPTIBLE is positive, and process any pending
   signals immediately if INTERRUPTIBLE is nonzero.  Return the number
   of bytes written; if this is less than NBYTE, set errno to a value
   other than EINTR.  */
static ptrdiff_t
emacs_full_write (int fd, char const *buf, ptrdiff_t nbyte,
		  int interruptible)
{
  ptrdiff_t bytes_written = 0;

  while (nbyte > 0)
    {
      ssize_t n = write (fd, buf, min (nbyte, MAX_RW_COUNT));

      if (n < 0)
	{
	  if (errno != EINTR)
	    break;

	  if (interruptible)
	    {
	      if (0 < interruptible)
		maybe_quit ();
	      if (pending_signals)
		process_pending_signals ();
	    }
	}
      else
	{
	  buf += n;
	  nbyte -= n;
	  bytes_written += n;
	}
    }

  return bytes_written;
}

/* Write to FD from a buffer BUF with size NBYTE, retrying if
   interrupted or if a partial write occurs.  Do not process quits or
   pending signals.  Return the number of bytes written, setting errno
   if this is less than NBYTE.  */
ptrdiff_t
emacs_write (int fd, void const *buf, ptrdiff_t nbyte)
{
  return emacs_full_write (fd, buf, nbyte, 0);
}

/* Like emacs_write, but also process pending signals.  */
ptrdiff_t
emacs_write_sig (int fd, void const *buf, ptrdiff_t nbyte)
{
  return emacs_full_write (fd, buf, nbyte, -1);
}

/* Like emacs_write, but also process quits and pending signals.  */
ptrdiff_t
emacs_write_quit (int fd, void const *buf, ptrdiff_t nbyte)
{
  return emacs_full_write (fd, buf, nbyte, 1);
}

/* Write a diagnostic to standard error that contains MESSAGE and a
   string derived from errno.  Preserve errno.  Do not buffer stderr.
   Do not process quits or pending signals if interrupted.  */
void
emacs_perror (char const *message)
{
  int err = errno;
  char const *error_string = emacs_strerror (err);
  char const *command = (initial_argv && initial_argv[0]
			 ? initial_argv[0] : "emacs");
  /* Write it out all at once, if it's short; this is less likely to
     be interleaved with other output.  */
  char buf[BUFSIZ];
  int nbytes = snprintf (buf, sizeof buf, "%s: %s: %s\n",
			 command, message, error_string);
  if (0 <= nbytes && nbytes < BUFSIZ)
    emacs_write (STDERR_FILENO, buf, nbytes);
  else
    {
      emacs_write (STDERR_FILENO, command, strlen (command));
      emacs_write (STDERR_FILENO, ": ", 2);
      emacs_write (STDERR_FILENO, message, strlen (message));
      emacs_write (STDERR_FILENO, ": ", 2);
      emacs_write (STDERR_FILENO, error_string, strlen (error_string));
      emacs_write (STDERR_FILENO, "\n", 1);
    }
  errno = err;
}

/* Return a struct timeval that is roughly equivalent to T.
   Use the least timeval not less than T.
   Return an extremal value if the result would overflow.  */
struct timeval
make_timeval (struct timespec t)
{
  struct timeval tv;
  tv.tv_sec = t.tv_sec;
  tv.tv_usec = t.tv_nsec / 1000;

  if (t.tv_nsec % 1000 != 0)
    {
      if (tv.tv_usec < 999999)
	tv.tv_usec++;
      else if (tv.tv_sec < TYPE_MAXIMUM (time_t))
	{
	  tv.tv_sec++;
	  tv.tv_usec = 0;
	}
    }

  return tv;
}

/* Set the access and modification time stamps of FD (a.k.a. FILE) to be
   ATIME and MTIME, respectively.
   FD must be either negative -- in which case it is ignored --
   or a file descriptor that is open on FILE.
   If FD is nonnegative, then FILE can be NULL.  */
int
set_file_times (int fd, const char *filename,
		struct timespec atime, struct timespec mtime)
{
  struct timespec timespec[2];
  timespec[0] = atime;
  timespec[1] = mtime;
  return fdutimens (fd, filename, timespec);
}

/* Rename directory SRCFD's entry SRC to directory DSTFD's entry DST.
   This is like renameat except that it fails if DST already exists,
   or if this operation is not supported atomically.  Return 0 if
   successful, -1 (setting errno) otherwise.  */
int
renameat_noreplace (int srcfd, char const *src, int dstfd, char const *dst)
{
#if defined SYS_renameat2 && defined RENAME_NOREPLACE
  return syscall (SYS_renameat2, srcfd, src, dstfd, dst, RENAME_NOREPLACE);
#elif defined CYGWIN && defined RENAME_NOREPLACE
  return renameat2 (srcfd, src, dstfd, dst, RENAME_NOREPLACE);
#elif defined RENAME_EXCL
  return renameatx_np (srcfd, src, dstfd, dst, RENAME_EXCL);
#else
# ifdef WINDOWSNT
  if (srcfd == AT_FDCWD && dstfd == AT_FDCWD)
    return sys_rename_replace (src, dst, 0);
# endif
  errno = ENOSYS;
  return -1;
#endif
}

/* Like strsignal, except async-signal-safe, and this function typically
   returns a string in the C locale rather than the current locale.  */
char const *
safe_strsignal (int code)
{
  char const *signame = 0;

  if (0 <= code && code < sys_siglist_entries)
    signame = sys_siglist[code];
  if (! signame)
    signame = "Unknown signal";

  return signame;
}

#ifndef DOS_NT
/* For make-serial-process  */
int
serial_open (Lisp_Object port)
{
  int fd = emacs_open (SSDATA (port), O_RDWR | O_NOCTTY | O_NONBLOCK, 0);
  if (fd < 0)
    report_file_error ("Opening serial port", port);
#ifdef TIOCEXCL
  ioctl (fd, TIOCEXCL, (char *) 0);
#endif

  return fd;
}

#if !defined (HAVE_CFMAKERAW)
/* Workaround for targets which are missing cfmakeraw.  */
/* Pasted from man page.  */
static void
cfmakeraw (struct termios *termios_p)
{
    termios_p->c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
    termios_p->c_oflag &= ~OPOST;
    termios_p->c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
    termios_p->c_cflag &= ~(CSIZE|PARENB);
    termios_p->c_cflag |= CS8;
}
#endif /* !defined (HAVE_CFMAKERAW */

#if !defined (HAVE_CFSETSPEED)
/* Workaround for targets which are missing cfsetspeed.  */
static int
cfsetspeed (struct termios *termios_p, speed_t vitesse)
{
  return (cfsetispeed (termios_p, vitesse)
	  + cfsetospeed (termios_p, vitesse));
}
#endif

/* For serial-process-configure  */
void
serial_configure (struct Lisp_Process *p,
		  Lisp_Object contact)
{
  Lisp_Object childp2 = Qnil;
  Lisp_Object tem = Qnil;
  struct termios attr;
  int err;
  char summary[4] = "???"; /* This usually becomes "8N1".  */

  childp2 = Fcopy_sequence (p->childp);

  /* Read port attributes and prepare default configuration.  */
  err = tcgetattr (p->outfd, &attr);
  if (err != 0)
    report_file_error ("Failed tcgetattr", Qnil);
  cfmakeraw (&attr);
#if defined (CLOCAL)
  attr.c_cflag |= CLOCAL;
#endif
#if defined (CREAD)
  attr.c_cflag |= CREAD;
#endif

  /* Configure speed.  */
  if (!NILP (Fplist_member (contact, QCspeed)))
    tem = Fplist_get (contact, QCspeed);
  else
    tem = Fplist_get (p->childp, QCspeed);
  CHECK_NUMBER (tem);
  err = cfsetspeed (&attr, XINT (tem));
  if (err != 0)
    report_file_error ("Failed cfsetspeed", tem);
  childp2 = Fplist_put (childp2, QCspeed, tem);

  /* Configure bytesize.  */
  if (!NILP (Fplist_member (contact, QCbytesize)))
    tem = Fplist_get (contact, QCbytesize);
  else
    tem = Fplist_get (p->childp, QCbytesize);
  if (NILP (tem))
    tem = make_number (8);
  CHECK_NUMBER (tem);
  if (XINT (tem) != 7 && XINT (tem) != 8)
    error (":bytesize must be nil (8), 7, or 8");
  summary[0] = XINT (tem) + '0';
#if defined (CSIZE) && defined (CS7) && defined (CS8)
  attr.c_cflag &= ~CSIZE;
  attr.c_cflag |= ((XINT (tem) == 7) ? CS7 : CS8);
#else
  /* Don't error on bytesize 8, which should be set by cfmakeraw.  */
  if (XINT (tem) != 8)
    error ("Bytesize cannot be changed");
#endif
  childp2 = Fplist_put (childp2, QCbytesize, tem);

  /* Configure parity.  */
  if (!NILP (Fplist_member (contact, QCparity)))
    tem = Fplist_get (contact, QCparity);
  else
    tem = Fplist_get (p->childp, QCparity);
  if (!NILP (tem) && !EQ (tem, Qeven) && !EQ (tem, Qodd))
    error (":parity must be nil (no parity), `even', or `odd'");
#if defined (PARENB) && defined (PARODD) && defined (IGNPAR) && defined (INPCK)
  attr.c_cflag &= ~(PARENB | PARODD);
  attr.c_iflag &= ~(IGNPAR | INPCK);
  if (NILP (tem))
    {
      summary[1] = 'N';
    }
  else if (EQ (tem, Qeven))
    {
      summary[1] = 'E';
      attr.c_cflag |= PARENB;
      attr.c_iflag |= (IGNPAR | INPCK);
    }
  else if (EQ (tem, Qodd))
    {
      summary[1] = 'O';
      attr.c_cflag |= (PARENB | PARODD);
      attr.c_iflag |= (IGNPAR | INPCK);
    }
#else
  /* Don't error on no parity, which should be set by cfmakeraw.  */
  if (!NILP (tem))
    error ("Parity cannot be configured");
#endif
  childp2 = Fplist_put (childp2, QCparity, tem);

  /* Configure stopbits.  */
  if (!NILP (Fplist_member (contact, QCstopbits)))
    tem = Fplist_get (contact, QCstopbits);
  else
    tem = Fplist_get (p->childp, QCstopbits);
  if (NILP (tem))
    tem = make_number (1);
  CHECK_NUMBER (tem);
  if (XINT (tem) != 1 && XINT (tem) != 2)
    error (":stopbits must be nil (1 stopbit), 1, or 2");
  summary[2] = XINT (tem) + '0';
#if defined (CSTOPB)
  attr.c_cflag &= ~CSTOPB;
  if (XINT (tem) == 2)
    attr.c_cflag |= CSTOPB;
#else
  /* Don't error on 1 stopbit, which should be set by cfmakeraw.  */
  if (XINT (tem) != 1)
    error ("Stopbits cannot be configured");
#endif
  childp2 = Fplist_put (childp2, QCstopbits, tem);

  /* Configure flowcontrol.  */
  if (!NILP (Fplist_member (contact, QCflowcontrol)))
    tem = Fplist_get (contact, QCflowcontrol);
  else
    tem = Fplist_get (p->childp, QCflowcontrol);
  if (!NILP (tem) && !EQ (tem, Qhw) && !EQ (tem, Qsw))
    error (":flowcontrol must be nil (no flowcontrol), `hw', or `sw'");
#if defined (CRTSCTS)
  attr.c_cflag &= ~CRTSCTS;
#endif
#if defined (CNEW_RTSCTS)
  attr.c_cflag &= ~CNEW_RTSCTS;
#endif
#if defined (IXON) && defined (IXOFF)
  attr.c_iflag &= ~(IXON | IXOFF);
#endif
  if (NILP (tem))
    {
      /* Already configured.  */
    }
  else if (EQ (tem, Qhw))
    {
#if defined (CRTSCTS)
      attr.c_cflag |= CRTSCTS;
#elif defined (CNEW_RTSCTS)
      attr.c_cflag |= CNEW_RTSCTS;
#else
      error ("Hardware flowcontrol (RTS/CTS) not supported");
#endif
    }
  else if (EQ (tem, Qsw))
    {
#if defined (IXON) && defined (IXOFF)
      attr.c_iflag |= (IXON | IXOFF);
#else
      error ("Software flowcontrol (XON/XOFF) not supported");
#endif
    }
  childp2 = Fplist_put (childp2, QCflowcontrol, tem);

  /* Activate configuration.  */
  err = tcsetattr (p->outfd, TCSANOW, &attr);
  if (err != 0)
    report_file_error ("Failed tcsetattr", Qnil);

  childp2 = Fplist_put (childp2, QCsummary, build_string (summary));
  pset_childp (p, childp2);
}
#endif /* not DOS_NT  */

/* System depended enumeration of and access to system processes a-la ps(1).  */

#ifdef HAVE_PROCFS

/* Process enumeration and access via /proc.  */

Lisp_Object
list_system_processes (void)
{
  Lisp_Object procdir, match, proclist, next;
  Lisp_Object tail;

  /* For every process on the system, there's a directory in the
     "/proc" pseudo-directory whose name is the numeric ID of that
     process.  */
  procdir = build_string ("/proc");
  match = build_string ("[0-9]+");
  proclist = directory_files_internal (procdir, Qnil, match, Qt, false, Qnil);

  /* `proclist' gives process IDs as strings.  Destructively convert
     each string into a number.  */
  for (tail = proclist; CONSP (tail); tail = next)
    {
      next = XCDR (tail);
      XSETCAR (tail, Fstring_to_number (XCAR (tail), Qnil));
    }

  /* directory_files_internal returns the files in reverse order; undo
     that.  */
  proclist = Fnreverse (proclist);
  return proclist;
}

#elif defined DARWIN_OS || defined __FreeBSD__

Lisp_Object
list_system_processes (void)
{
#ifdef DARWIN_OS
  int mib[] = {CTL_KERN, KERN_PROC, KERN_PROC_ALL};
#else
  int mib[] = {CTL_KERN, KERN_PROC, KERN_PROC_PROC};
#endif
  size_t len;
  struct kinfo_proc *procs;
  size_t i;

  Lisp_Object proclist = Qnil;

  if (sysctl (mib, 3, NULL, &len, NULL, 0) != 0)
    return proclist;

  procs = xmalloc (len);
  if (sysctl (mib, 3, procs, &len, NULL, 0) != 0)
    {
      xfree (procs);
      return proclist;
    }

  len /= sizeof (struct kinfo_proc);
  for (i = 0; i < len; i++)
    {
#ifdef DARWIN_OS
      proclist = Fcons (make_fixnum_or_float (procs[i].kp_proc.p_pid), proclist);
#else
      proclist = Fcons (make_fixnum_or_float (procs[i].ki_pid), proclist);
#endif
    }

  xfree (procs);

  return  proclist;
}

/* The WINDOWSNT implementation is in w32.c.
   The MSDOS implementation is in dosfns.c.  */
#elif !defined (WINDOWSNT) && !defined (MSDOS)

Lisp_Object
list_system_processes (void)
{
  return Qnil;
}

#endif /* !defined (WINDOWSNT) */

#if defined GNU_LINUX && defined HAVE_LONG_LONG_INT
static struct timespec
time_from_jiffies (unsigned long long tval, long hz)
{
  unsigned long long s = tval / hz;
  unsigned long long frac = tval % hz;
  int ns;

  if (TYPE_MAXIMUM (time_t) < s)
    time_overflow ();
  if (LONG_MAX - 1 <= ULLONG_MAX / TIMESPEC_RESOLUTION
      || frac <= ULLONG_MAX / TIMESPEC_RESOLUTION)
    ns = frac * TIMESPEC_RESOLUTION / hz;
  else
    {
      /* This is reachable only in the unlikely case that HZ * HZ
	 exceeds ULLONG_MAX.  It calculates an approximation that is
	 guaranteed to be in range.  */
      long hz_per_ns = (hz / TIMESPEC_RESOLUTION
			+ (hz % TIMESPEC_RESOLUTION != 0));
      ns = frac / hz_per_ns;
    }

  return make_timespec (s, ns);
}

static Lisp_Object
ltime_from_jiffies (unsigned long long tval, long hz)
{
  struct timespec t = time_from_jiffies (tval, hz);
  return make_lisp_time (t);
}

static struct timespec
get_up_time (void)
{
  FILE *fup;
  struct timespec up = make_timespec (0, 0);

  block_input ();
  fup = emacs_fopen ("/proc/uptime", "r");

  if (fup)
    {
      unsigned long long upsec, upfrac, idlesec, idlefrac;
      int upfrac_start, upfrac_end, idlefrac_start, idlefrac_end;

      if (fscanf (fup, "%llu.%n%llu%n %llu.%n%llu%n",
		  &upsec, &upfrac_start, &upfrac, &upfrac_end,
		  &idlesec, &idlefrac_start, &idlefrac, &idlefrac_end)
	  == 4)
	{
	  if (TYPE_MAXIMUM (time_t) < upsec)
	    {
	      upsec = TYPE_MAXIMUM (time_t);
	      upfrac = TIMESPEC_RESOLUTION - 1;
	    }
	  else
	    {
	      int upfraclen = upfrac_end - upfrac_start;
	      for (; upfraclen < LOG10_TIMESPEC_RESOLUTION; upfraclen++)
		upfrac *= 10;
	      for (; LOG10_TIMESPEC_RESOLUTION < upfraclen; upfraclen--)
		upfrac /= 10;
	      upfrac = min (upfrac, TIMESPEC_RESOLUTION - 1);
	    }
	  up = make_timespec (upsec, upfrac);
	}
      fclose (fup);
    }
  unblock_input ();

  return up;
}

#define MAJOR(d) (((unsigned)(d) >> 8) & 0xfff)
#define MINOR(d) (((unsigned)(d) & 0xff) | (((unsigned)(d) & 0xfff00000) >> 12))

static Lisp_Object
procfs_ttyname (int rdev)
{
  FILE *fdev;
  char name[PATH_MAX];

  block_input ();
  fdev = emacs_fopen ("/proc/tty/drivers", "r");
  name[0] = 0;

  if (fdev)
    {
      unsigned major;
      unsigned long minor_beg, minor_end;
      char minor[25];	/* 2 32-bit numbers + dash */
      char *endp;

      for (; !feof_unlocked (fdev) && !ferror_unlocked (fdev); name[0] = 0)
	{
	  if (fscanf (fdev, "%*s %s %u %s %*s\n", name, &major, minor) >= 3
	      && major == MAJOR (rdev))
	    {
	      minor_beg = strtoul (minor, &endp, 0);
	      if (*endp == '\0')
		minor_end = minor_beg;
	      else if (*endp == '-')
		minor_end = strtoul (endp + 1, &endp, 0);
	      else
		continue;

	      if (MINOR (rdev) >= minor_beg && MINOR (rdev) <= minor_end)
		{
		  sprintf (name + strlen (name), "%u", MINOR (rdev));
		  break;
		}
	    }
	}
      fclose (fdev);
    }
  unblock_input ();
  return build_string (name);
}

static uintmax_t
procfs_get_total_memory (void)
{
  FILE *fmem;
  uintmax_t retval = 2 * 1024 * 1024; /* default: 2 GiB */
  int c;

  block_input ();
  fmem = emacs_fopen ("/proc/meminfo", "r");

  if (fmem)
    {
      uintmax_t entry_value;
      bool done;

      do
	switch (fscanf (fmem, "MemTotal: %"SCNuMAX, &entry_value))
	  {
	  case 1:
	    retval = entry_value;
	    done = 1;
	    break;

	  case 0:
	    while ((c = getc_unlocked (fmem)) != EOF && c != '\n')
	      continue;
	    done = c == EOF;
	    break;

	  default:
	    done = 1;
	    break;
	  }
      while (!done);

      fclose (fmem);
    }
  unblock_input ();
  return retval;
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  char procfn[PATH_MAX], fn[PATH_MAX];
  struct stat st;
  struct passwd *pw;
  struct group *gr;
  long clocks_per_sec;
  char *procfn_end;
  char procbuf[1025], *p, *q;
  int fd;
  ssize_t nread;
  static char const default_cmd[] = "???";
  const char *cmd = default_cmd;
  int cmdsize = sizeof default_cmd - 1;
  char *cmdline = NULL;
  ptrdiff_t cmdline_size;
  char c;
  printmax_t proc_id;
  int ppid, pgrp, sess, tty, tpgid, thcount;
  uid_t uid;
  gid_t gid;
  unsigned long long u_time, s_time, cutime, cstime, start;
  long priority, niceness, rss;
  unsigned long minflt, majflt, cminflt, cmajflt, vsize;
  struct timespec tnow, tstart, tboot, telapsed, us_time;
  double pcpu, pmem;
  Lisp_Object attrs = Qnil;
  Lisp_Object decoded_cmd;
  ptrdiff_t count;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, pid_t, proc_id);
  sprintf (procfn, "/proc/%"pMd, proc_id);
  if (stat (procfn, &st) < 0)
    return attrs;

  /* euid egid */
  uid = st.st_uid;
  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (uid)), attrs);
  block_input ();
  pw = getpwuid (uid);
  unblock_input ();
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  gid = st.st_gid;
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (gid)), attrs);
  block_input ();
  gr = getgrgid (gid);
  unblock_input ();
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  count = SPECPDL_INDEX ();
  strcpy (fn, procfn);
  procfn_end = fn + strlen (fn);
  strcpy (procfn_end, "/stat");
  fd = emacs_open (fn, O_RDONLY, 0);
  if (fd < 0)
    nread = 0;
  else
    {
      record_unwind_protect_int (close_file_unwind, fd);
      nread = emacs_read_quit (fd, procbuf, sizeof procbuf - 1);
    }
  if (0 < nread)
    {
      procbuf[nread] = '\0';
      p = procbuf;

      p = strchr (p, '(');
      if (p != NULL)
	{
	  q = strrchr (p + 1, ')');
	  /* comm */
	  if (q != NULL)
	    {
	      cmd = p + 1;
	      cmdsize = q - cmd;
	    }
	}
      else
	q = NULL;
      /* Command name is encoded in locale-coding-system; decode it.  */
      AUTO_STRING_WITH_LEN (cmd_str, cmd, cmdsize);
      decoded_cmd = code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
      attrs = Fcons (Fcons (Qcomm, decoded_cmd), attrs);

      /* state ppid pgrp sess tty tpgid . minflt cminflt majflt cmajflt
	 utime stime cutime cstime priority nice thcount . start vsize rss */
      if (q
	  && (sscanf (q + 2, ("%c %d %d %d %d %d %*u %lu %lu %lu %lu "
			      "%Lu %Lu %Lu %Lu %ld %ld %d %*d %Lu %lu %ld"),
		      &c, &ppid, &pgrp, &sess, &tty, &tpgid,
		      &minflt, &cminflt, &majflt, &cmajflt,
		      &u_time, &s_time, &cutime, &cstime,
		      &priority, &niceness, &thcount, &start, &vsize, &rss)
	      == 20))
	{
	  char state_str[2];
	  state_str[0] = c;
	  state_str[1] = '\0';
	  attrs = Fcons (Fcons (Qstate, build_string (state_str)), attrs);
	  attrs = Fcons (Fcons (Qppid, make_fixnum_or_float (ppid)), attrs);
	  attrs = Fcons (Fcons (Qpgrp, make_fixnum_or_float (pgrp)), attrs);
	  attrs = Fcons (Fcons (Qsess, make_fixnum_or_float (sess)), attrs);
	  attrs = Fcons (Fcons (Qttname, procfs_ttyname (tty)), attrs);
	  attrs = Fcons (Fcons (Qtpgid, make_fixnum_or_float (tpgid)), attrs);
	  attrs = Fcons (Fcons (Qminflt, make_fixnum_or_float (minflt)), attrs);
	  attrs = Fcons (Fcons (Qmajflt, make_fixnum_or_float (majflt)), attrs);
	  attrs = Fcons (Fcons (Qcminflt, make_fixnum_or_float (cminflt)),
			 attrs);
	  attrs = Fcons (Fcons (Qcmajflt, make_fixnum_or_float (cmajflt)),
			 attrs);
	  clocks_per_sec = sysconf (_SC_CLK_TCK);
	  if (clocks_per_sec < 0)
	    clocks_per_sec = 100;
	  attrs = Fcons (Fcons (Qutime,
				ltime_from_jiffies (u_time, clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qstime,
				ltime_from_jiffies (s_time, clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qtime,
				ltime_from_jiffies (s_time + u_time,
						    clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qcutime,
				ltime_from_jiffies (cutime, clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qcstime,
				ltime_from_jiffies (cstime, clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qctime,
				ltime_from_jiffies (cstime + cutime,
						    clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qpri, make_number (priority)), attrs);
	  attrs = Fcons (Fcons (Qnice, make_number (niceness)), attrs);
	  attrs = Fcons (Fcons (Qthcount, make_fixnum_or_float (thcount)),
			 attrs);
	  tnow = current_timespec ();
	  telapsed = get_up_time ();
	  tboot = timespec_sub (tnow, telapsed);
	  tstart = time_from_jiffies (start, clocks_per_sec);
	  tstart = timespec_add (tboot, tstart);
	  attrs = Fcons (Fcons (Qstart, make_lisp_time (tstart)), attrs);
	  attrs = Fcons (Fcons (Qvsize, make_fixnum_or_float (vsize / 1024)),
			 attrs);
	  attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (4 * rss)), attrs);
	  telapsed = timespec_sub (tnow, tstart);
	  attrs = Fcons (Fcons (Qetime, make_lisp_time (telapsed)), attrs);
	  us_time = time_from_jiffies (u_time + s_time, clocks_per_sec);
	  pcpu = timespectod (us_time) / timespectod (telapsed);
	  if (pcpu > 1.0)
	    pcpu = 1.0;
	  attrs = Fcons (Fcons (Qpcpu, make_float (100 * pcpu)), attrs);
	  pmem = 4.0 * 100 * rss / procfs_get_total_memory ();
	  if (pmem > 100)
	    pmem = 100;
	  attrs = Fcons (Fcons (Qpmem, make_float (pmem)), attrs);
	}
    }
  unbind_to (count, Qnil);

  /* args */
  strcpy (procfn_end, "/cmdline");
  fd = emacs_open (fn, O_RDONLY, 0);
  if (fd >= 0)
    {
      ptrdiff_t readsize, nread_incr;
      record_unwind_protect_int (close_file_unwind, fd);
      record_unwind_protect_nothing ();
      nread = cmdline_size = 0;

      do
	{
	  cmdline = xpalloc (cmdline, &cmdline_size, 2, STRING_BYTES_BOUND, 1);
	  set_unwind_protect_ptr (count + 1, xfree, cmdline);

	  /* Leave room even if every byte needs escaping below.  */
	  readsize = (cmdline_size >> 1) - nread;

	  nread_incr = emacs_read_quit (fd, cmdline + nread, readsize);
	  nread += max (0, nread_incr);
	}
      while (nread_incr == readsize);

      if (nread)
	{
	  /* We don't want trailing null characters.  */
	  for (p = cmdline + nread; cmdline < p && !p[-1]; p--)
	    continue;

	  /* Escape-quote whitespace and backslashes.  */
	  q = cmdline + cmdline_size;
	  while (cmdline < p)
	    {
	      char c = *--p;
	      *--q = c ? c : ' ';
	      if (c_isspace (c) || c == '\\')
		*--q = '\\';
	    }

	  nread = cmdline + cmdline_size - q;
	}

      if (!nread)
	{
	  nread = cmdsize + 2;
	  cmdline_size = nread + 1;
	  q = cmdline = xrealloc (cmdline, cmdline_size);
	  set_unwind_protect_ptr (count + 1, xfree, cmdline);
	  sprintf (cmdline, "[%.*s]", cmdsize, cmd);
	}
      /* Command line is encoded in locale-coding-system; decode it.  */
      AUTO_STRING_WITH_LEN (cmd_str, q, nread);
      decoded_cmd = code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
      unbind_to (count, Qnil);
      attrs = Fcons (Fcons (Qargs, decoded_cmd), attrs);
    }

  return attrs;
}

#elif defined (SOLARIS2) && defined (HAVE_PROCFS)

/* The <procfs.h> header does not like to be included if _LP64 is defined and
   __FILE_OFFSET_BITS == 64.  This is an ugly workaround that.  */
#if !defined (_LP64) && defined (_FILE_OFFSET_BITS) &&  (_FILE_OFFSET_BITS  ==  64)
#define PROCFS_FILE_OFFSET_BITS_HACK 1
#undef _FILE_OFFSET_BITS
#else
#define PROCFS_FILE_OFFSET_BITS_HACK 0
#endif

#include <procfs.h>

#if PROCFS_FILE_OFFSET_BITS_HACK ==  1
#define _FILE_OFFSET_BITS 64
#ifdef _FILE_OFFSET_BITS /* Avoid unused-macro warnings.  */
#endif
#endif /* PROCFS_FILE_OFFSET_BITS_HACK ==  1 */

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  char procfn[PATH_MAX], fn[PATH_MAX];
  struct stat st;
  struct passwd *pw;
  struct group *gr;
  char *procfn_end;
  struct psinfo pinfo;
  int fd;
  ssize_t nread;
  printmax_t proc_id;
  uid_t uid;
  gid_t gid;
  Lisp_Object attrs = Qnil;
  Lisp_Object decoded_cmd;
  ptrdiff_t count;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, pid_t, proc_id);
  sprintf (procfn, "/proc/%"pMd, proc_id);
  if (stat (procfn, &st) < 0)
    return attrs;

  /* euid egid */
  uid = st.st_uid;
  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (uid)), attrs);
  block_input ();
  pw = getpwuid (uid);
  unblock_input ();
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  gid = st.st_gid;
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (gid)), attrs);
  block_input ();
  gr = getgrgid (gid);
  unblock_input ();
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  count = SPECPDL_INDEX ();
  strcpy (fn, procfn);
  procfn_end = fn + strlen (fn);
  strcpy (procfn_end, "/psinfo");
  fd = emacs_open (fn, O_RDONLY, 0);
  if (fd < 0)
    nread = 0;
  else
    {
      record_unwind_protect_int (close_file_unwind, fd);
      nread = emacs_read_quit (fd, &pinfo, sizeof pinfo);
    }

  if (nread == sizeof pinfo)
    {
      attrs = Fcons (Fcons (Qppid, make_fixnum_or_float (pinfo.pr_ppid)), attrs);
      attrs = Fcons (Fcons (Qpgrp, make_fixnum_or_float (pinfo.pr_pgid)), attrs);
      attrs = Fcons (Fcons (Qsess, make_fixnum_or_float (pinfo.pr_sid)), attrs);

      {
	char state_str[2];
	state_str[0] = pinfo.pr_lwp.pr_sname;
	state_str[1] = '\0';
	attrs = Fcons (Fcons (Qstate, build_string (state_str)), attrs);
      }

      /* FIXME: missing Qttyname. psinfo.pr_ttydev is a dev_t,
	 need to get a string from it. */

      /* FIXME: missing: Qtpgid */

      /* FIXME: missing:
	    Qminflt
	    Qmajflt
	    Qcminflt
	    Qcmajflt

	    Qutime
	    Qcutime
	    Qstime
	    Qcstime
	    Are they available? */

      attrs = Fcons (Fcons (Qtime, make_lisp_time (pinfo.pr_time)), attrs);
      attrs = Fcons (Fcons (Qctime, make_lisp_time (pinfo.pr_ctime)), attrs);
      attrs = Fcons (Fcons (Qpri, make_number (pinfo.pr_lwp.pr_pri)), attrs);
      attrs = Fcons (Fcons (Qnice, make_number (pinfo.pr_lwp.pr_nice)), attrs);
      attrs = Fcons (Fcons (Qthcount, make_fixnum_or_float (pinfo.pr_nlwp)),
		     attrs);

      attrs = Fcons (Fcons (Qstart, make_lisp_time (pinfo.pr_start)), attrs);
      attrs = Fcons (Fcons (Qvsize, make_fixnum_or_float (pinfo.pr_size)),
		     attrs);
      attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (pinfo.pr_rssize)),
		     attrs);

      /* pr_pctcpu and pr_pctmem are unsigned integers in the
	 range 0 .. 2**15, representing 0.0 .. 1.0.  */
      attrs = Fcons (Fcons (Qpcpu,
			    make_float (100.0 / 0x8000 * pinfo.pr_pctcpu)),
		     attrs);
      attrs = Fcons (Fcons (Qpmem,
			    make_float (100.0 / 0x8000 * pinfo.pr_pctmem)),
		     attrs);

      AUTO_STRING (fname, pinfo.pr_fname);
      decoded_cmd = code_convert_string_norecord (fname,
						  Vlocale_coding_system, 0);
      attrs = Fcons (Fcons (Qcomm, decoded_cmd), attrs);
      AUTO_STRING (psargs, pinfo.pr_psargs);
      decoded_cmd = code_convert_string_norecord (psargs,
						  Vlocale_coding_system, 0);
      attrs = Fcons (Fcons (Qargs, decoded_cmd), attrs);
    }
  unbind_to (count, Qnil);
  return attrs;
}

#elif defined __FreeBSD__

static struct timespec
timeval_to_timespec (struct timeval t)
{
  return make_timespec (t.tv_sec, t.tv_usec * 1000);
}

static Lisp_Object
make_lisp_timeval (struct timeval t)
{
  return make_lisp_time (timeval_to_timespec (t));
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  int proc_id;
  int pagesize = getpagesize ();
  unsigned long npages;
  int fscale;
  struct passwd *pw;
  struct group  *gr;
  char *ttyname;
  size_t len;
  char args[MAXPATHLEN];
  struct timespec t, now;

  int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID};
  struct kinfo_proc proc;
  size_t proclen = sizeof proc;

  Lisp_Object attrs = Qnil;
  Lisp_Object decoded_comm;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, int, proc_id);
  mib[3] = proc_id;

  if (sysctl (mib, 4, &proc, &proclen, NULL, 0) != 0)
    return attrs;

  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (proc.ki_uid)), attrs);

  block_input ();
  pw = getpwuid (proc.ki_uid);
  unblock_input ();
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (proc.ki_svgid)), attrs);

  block_input ();
  gr = getgrgid (proc.ki_svgid);
  unblock_input ();
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  AUTO_STRING (comm, proc.ki_comm);
  decoded_comm = code_convert_string_norecord (comm, Vlocale_coding_system, 0);

  attrs = Fcons (Fcons (Qcomm, decoded_comm), attrs);
  {
    char state[2] = {'\0', '\0'};
    switch (proc.ki_stat)
      {
      case SRUN:
	state[0] = 'R';
	break;

      case SSLEEP:
	state[0] = 'S';
	break;

      case SLOCK:
	state[0] = 'D';
	break;

      case SZOMB:
	state[0] = 'Z';
	break;

      case SSTOP:
	state[0] = 'T';
	break;
      }
    attrs = Fcons (Fcons (Qstate, build_string (state)), attrs);
  }

  attrs = Fcons (Fcons (Qppid, make_fixnum_or_float (proc.ki_ppid)), attrs);
  attrs = Fcons (Fcons (Qpgrp, make_fixnum_or_float (proc.ki_pgid)), attrs);
  attrs = Fcons (Fcons (Qsess, make_fixnum_or_float (proc.ki_sid)),  attrs);

  block_input ();
  ttyname = proc.ki_tdev == NODEV ? NULL : devname (proc.ki_tdev, S_IFCHR);
  unblock_input ();
  if (ttyname)
    attrs = Fcons (Fcons (Qtty, build_string (ttyname)), attrs);

  attrs = Fcons (Fcons (Qtpgid,   make_fixnum_or_float (proc.ki_tpgid)), attrs);
  attrs = Fcons (Fcons (Qminflt,  make_fixnum_or_float (proc.ki_rusage.ru_minflt)), attrs);
  attrs = Fcons (Fcons (Qmajflt,  make_fixnum_or_float (proc.ki_rusage.ru_majflt)), attrs);
  attrs = Fcons (Fcons (Qcminflt, make_number (proc.ki_rusage_ch.ru_minflt)), attrs);
  attrs = Fcons (Fcons (Qcmajflt, make_number (proc.ki_rusage_ch.ru_majflt)), attrs);

  attrs = Fcons (Fcons (Qutime, make_lisp_timeval (proc.ki_rusage.ru_utime)),
		 attrs);
  attrs = Fcons (Fcons (Qstime, make_lisp_timeval (proc.ki_rusage.ru_stime)),
		 attrs);
  t = timespec_add (timeval_to_timespec (proc.ki_rusage.ru_utime),
		    timeval_to_timespec (proc.ki_rusage.ru_stime));
  attrs = Fcons (Fcons (Qtime, make_lisp_time (t)), attrs);

  attrs = Fcons (Fcons (Qcutime,
			make_lisp_timeval (proc.ki_rusage_ch.ru_utime)),
		 attrs);
  attrs = Fcons (Fcons (Qcstime,
			make_lisp_timeval (proc.ki_rusage_ch.ru_utime)),
		 attrs);
  t = timespec_add (timeval_to_timespec (proc.ki_rusage_ch.ru_utime),
		    timeval_to_timespec (proc.ki_rusage_ch.ru_stime));
  attrs = Fcons (Fcons (Qctime, make_lisp_time (t)), attrs);

  attrs = Fcons (Fcons (Qthcount, make_fixnum_or_float (proc.ki_numthreads)),
		 attrs);
  attrs = Fcons (Fcons (Qpri,   make_number (proc.ki_pri.pri_native)), attrs);
  attrs = Fcons (Fcons (Qnice,  make_number (proc.ki_nice)), attrs);
  attrs = Fcons (Fcons (Qstart, make_lisp_timeval (proc.ki_start)), attrs);
  attrs = Fcons (Fcons (Qvsize, make_number (proc.ki_size >> 10)), attrs);
  attrs = Fcons (Fcons (Qrss,   make_number (proc.ki_rssize * pagesize >> 10)),
		 attrs);

  now = current_timespec ();
  t = timespec_sub (now, timeval_to_timespec (proc.ki_start));
  attrs = Fcons (Fcons (Qetime, make_lisp_time (t)), attrs);

  len = sizeof fscale;
  if (sysctlbyname ("kern.fscale", &fscale, &len, NULL, 0) == 0)
    {
      double pcpu;
      fixpt_t ccpu;
      len = sizeof ccpu;
      if (sysctlbyname ("kern.ccpu", &ccpu, &len, NULL, 0) == 0)
      	{
      	  pcpu = (100.0 * proc.ki_pctcpu / fscale
		  / (1 - exp (proc.ki_swtime * log ((double) ccpu / fscale))));
  	  attrs = Fcons (Fcons (Qpcpu, make_fixnum_or_float (pcpu)), attrs);
      	}
    }

  len = sizeof npages;
  if (sysctlbyname ("hw.availpages", &npages, &len, NULL, 0) == 0)
    {
      double pmem = (proc.ki_flag & P_INMEM
		     ? 100.0 * proc.ki_rssize / npages
		     : 0);
      attrs = Fcons (Fcons (Qpmem, make_fixnum_or_float (pmem)), attrs);
    }

  mib[2] = KERN_PROC_ARGS;
  len = MAXPATHLEN;
  if (sysctl (mib, 4, args, &len, NULL, 0) == 0)
    {
      int i;
      for (i = 0; i < len; i++)
	{
	  if (! args[i] && i < len - 1)
	    args[i] = ' ';
	}

      AUTO_STRING (comm, args);
      decoded_comm = code_convert_string_norecord (comm,
						   Vlocale_coding_system, 0);

      attrs = Fcons (Fcons (Qargs, decoded_comm), attrs);
    }

  return attrs;
}

#elif defined DARWIN_OS

static struct timespec
timeval_to_timespec (struct timeval t)
{
  return make_timespec (t.tv_sec, t.tv_usec * 1000);
}

static Lisp_Object
make_lisp_timeval (struct timeval t)
{
  return make_lisp_time (timeval_to_timespec (t));
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  int proc_id;
  struct passwd *pw;
  struct group  *gr;
  char *ttyname;
  struct timeval starttime;
  struct timespec t, now;
  struct rusage *rusage;
  dev_t tdev;
  uid_t uid;
  gid_t gid;

  int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID};
  struct kinfo_proc proc;
  size_t proclen = sizeof proc;

  Lisp_Object attrs = Qnil;
  Lisp_Object decoded_comm;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, int, proc_id);
  mib[3] = proc_id;

  if (sysctl (mib, 4, &proc, &proclen, NULL, 0) != 0)
    return attrs;

  uid = proc.kp_eproc.e_ucred.cr_uid;
  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (uid)), attrs);

  block_input ();
  pw = getpwuid (uid);
  unblock_input ();
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  gid = proc.kp_eproc.e_pcred.p_svgid;
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (gid)), attrs);

  block_input ();
  gr = getgrgid (gid);
  unblock_input ();
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  decoded_comm = (code_convert_string_norecord
		  (build_unibyte_string (proc.kp_proc.p_comm),
		   Vlocale_coding_system, 0));

  attrs = Fcons (Fcons (Qcomm, decoded_comm), attrs);
  {
    char state[2] = {'\0', '\0'};
    switch (proc.kp_proc.p_stat)
      {
      case SRUN:
	state[0] = 'R';
	break;

      case SSLEEP:
	state[0] = 'S';
	break;

      case SZOMB:
	state[0] = 'Z';
	break;

      case SSTOP:
	state[0] = 'T';
	break;

      case SIDL:
	state[0] = 'I';
	break;
      }
    attrs = Fcons (Fcons (Qstate, build_string (state)), attrs);
  }

  attrs = Fcons (Fcons (Qppid, make_fixnum_or_float (proc.kp_eproc.e_ppid)),
		 attrs);
  attrs = Fcons (Fcons (Qpgrp, make_fixnum_or_float (proc.kp_eproc.e_pgid)),
		 attrs);

  tdev = proc.kp_eproc.e_tdev;
  block_input ();
  ttyname = tdev == NODEV ? NULL : devname (tdev, S_IFCHR);
  unblock_input ();
  if (ttyname)
    attrs = Fcons (Fcons (Qtty, build_string (ttyname)), attrs);

  attrs = Fcons (Fcons (Qtpgid,   make_fixnum_or_float (proc.kp_eproc.e_tpgid)),
		 attrs);

  rusage = proc.kp_proc.p_ru;
  if (rusage)
    {
      attrs = Fcons (Fcons (Qminflt,  make_fixnum_or_float (rusage->ru_minflt)),
		     attrs);
      attrs = Fcons (Fcons (Qmajflt,  make_fixnum_or_float (rusage->ru_majflt)),
		     attrs);

      attrs = Fcons (Fcons (Qutime, make_lisp_timeval (rusage->ru_utime)),
		     attrs);
      attrs = Fcons (Fcons (Qstime, make_lisp_timeval (rusage->ru_stime)),
		     attrs);
      t = timespec_add (timeval_to_timespec (rusage->ru_utime),
			timeval_to_timespec (rusage->ru_stime));
      attrs = Fcons (Fcons (Qtime, make_lisp_time (t)), attrs);
    }

  starttime = proc.kp_proc.p_starttime;
  attrs = Fcons (Fcons (Qnice,  make_number (proc.kp_proc.p_nice)), attrs);
  attrs = Fcons (Fcons (Qstart, make_lisp_timeval (starttime)), attrs);

  now = current_timespec ();
  t = timespec_sub (now, timeval_to_timespec (starttime));
  attrs = Fcons (Fcons (Qetime, make_lisp_time (t)), attrs);

  return attrs;
}

/* The WINDOWSNT implementation is in w32.c.
   The MSDOS implementation is in dosfns.c.  */
#elif !defined (WINDOWSNT) && !defined (MSDOS)

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  return Qnil;
}

#endif	/* !defined (WINDOWSNT) */

/* Wide character string collation.  */

#ifdef __STDC_ISO_10646__
# include <wchar.h>
# include <wctype.h>

# if defined HAVE_NEWLOCALE || defined HAVE_SETLOCALE
#  include <locale.h>
# endif
# ifndef LC_COLLATE
#  define LC_COLLATE 0
# endif
# ifndef LC_COLLATE_MASK
#  define LC_COLLATE_MASK 0
# endif
# ifndef LC_CTYPE
#  define LC_CTYPE 0
# endif
# ifndef LC_CTYPE_MASK
#  define LC_CTYPE_MASK 0
# endif

# ifndef HAVE_NEWLOCALE
#  undef freelocale
#  undef locale_t
#  undef newlocale
#  undef wcscoll_l
#  undef towlower_l
#  define freelocale emacs_freelocale
#  define locale_t emacs_locale_t
#  define newlocale emacs_newlocale
#  define wcscoll_l emacs_wcscoll_l
#  define towlower_l emacs_towlower_l

typedef char const *locale_t;

static locale_t
newlocale (int category_mask, char const *locale, locale_t loc)
{
  return locale;
}

static void
freelocale (locale_t loc)
{
}

static char *
emacs_setlocale (int category, char const *locale)
{
#  ifdef HAVE_SETLOCALE
  errno = 0;
  char *loc = setlocale (category, locale);
  if (loc || errno)
    return loc;
  errno = EINVAL;
#  else
  errno = ENOTSUP;
#  endif
  return 0;
}

static int
wcscoll_l (wchar_t const *a, wchar_t const *b, locale_t loc)
{
  int result = 0;
  char *oldloc = emacs_setlocale (LC_COLLATE, NULL);
  int err;

  if (! oldloc)
    err = errno;
  else
    {
      USE_SAFE_ALLOCA;
      char *oldcopy = SAFE_ALLOCA (strlen (oldloc) + 1);
      strcpy (oldcopy, oldloc);
      if (! emacs_setlocale (LC_COLLATE, loc))
	err = errno;
      else
	{
	  errno = 0;
	  result = wcscoll (a, b);
	  err = errno;
	  if (! emacs_setlocale (LC_COLLATE, oldcopy))
	    err = errno;
	}
      SAFE_FREE ();
    }

  errno = err;
  return result;
}

static wint_t
towlower_l (wint_t wc, locale_t loc)
{
  wint_t result = wc;
  char *oldloc = emacs_setlocale (LC_CTYPE, NULL);

  if (oldloc)
    {
      USE_SAFE_ALLOCA;
      char *oldcopy = SAFE_ALLOCA (strlen (oldloc) + 1);
      strcpy (oldcopy, oldloc);
      if (emacs_setlocale (LC_CTYPE, loc))
	{
	  result = towlower (wc);
	  emacs_setlocale (LC_COLLATE, oldcopy);
	}
      SAFE_FREE ();
    }

  return result;
}
# endif

int
str_collate (Lisp_Object s1, Lisp_Object s2,
	     Lisp_Object locale, Lisp_Object ignore_case)
{
  int res, err;
  ptrdiff_t len, i, i_byte;
  wchar_t *p1, *p2;

  USE_SAFE_ALLOCA;

  /* Convert byte stream to code points.  */
  len = SCHARS (s1); i = i_byte = 0;
  SAFE_NALLOCA (p1, 1, len + 1);
  while (i < len)
    FETCH_STRING_CHAR_ADVANCE (*(p1+i-1), s1, i, i_byte);
  *(p1+len) = 0;

  len = SCHARS (s2); i = i_byte = 0;
  SAFE_NALLOCA (p2, 1, len + 1);
  while (i < len)
    FETCH_STRING_CHAR_ADVANCE (*(p2+i-1), s2, i, i_byte);
  *(p2+len) = 0;

  if (STRINGP (locale))
    {
      locale_t loc = newlocale (LC_COLLATE_MASK | LC_CTYPE_MASK,
				SSDATA (locale), 0);
      if (!loc)
	error ("Invalid locale %s: %s", SSDATA (locale), emacs_strerror (errno));

      if (! NILP (ignore_case))
	for (int i = 1; i < 3; i++)
	  {
	    wchar_t *p = (i == 1) ? p1 : p2;
	    for (; *p; p++)
	      *p = towlower_l (*p, loc);
	  }

      errno = 0;
      res = wcscoll_l (p1, p2, loc);
      err = errno;
      freelocale (loc);
    }
  else
    {
      if (! NILP (ignore_case))
	for (int i = 1; i < 3; i++)
	  {
	    wchar_t *p = (i == 1) ? p1 : p2;
	    for (; *p; p++)
	      *p = towlower (*p);
	  }

      errno = 0;
      res = wcscoll (p1, p2);
      err = errno;
    }
#  ifndef HAVE_NEWLOCALE
  if (err)
    error ("Invalid locale or string for collation: %s", emacs_strerror (err));
#  else
  if (err)
    error ("Invalid string for collation: %s", emacs_strerror (err));
#  endif

  SAFE_FREE ();
  return res;
}
#endif  /* __STDC_ISO_10646__ */

#ifdef WINDOWSNT
int
str_collate (Lisp_Object s1, Lisp_Object s2,
	     Lisp_Object locale, Lisp_Object ignore_case)
{

  char *loc = STRINGP (locale) ? SSDATA (locale) : NULL;
  int res, err = errno;

  errno = 0;
  res = w32_compare_strings (SSDATA (s1), SSDATA (s2), loc, !NILP (ignore_case));
  if (errno)
    error ("Invalid string for collation: %s", strerror (errno));

  errno = err;
  return res;
}
#endif	/* WINDOWSNT */
