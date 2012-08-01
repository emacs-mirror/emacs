/* Interfaces to system-dependent kernel and library entries.
   Copyright (C) 1985-1988, 1993-1995, 1999-2012
                 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <ctype.h>
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#include <grp.h>
#endif /* HAVE_PWD_H */
#include <limits.h>
#include <unistd.h>

#include <allocator.h>
#include <careadlinkat.h>
#include <ignore-value.h>
#include <utimens.h>

#include "lisp.h"
#include "sysselect.h"
#include "blockinput.h"

#ifdef BSD_SYSTEM
#include <sys/param.h>
#include <sys/sysctl.h>
#endif

#ifdef __FreeBSD__
#include <sys/user.h>
#include <sys/resource.h>
#include <math.h>
#endif

#ifdef WINDOWSNT
#define read sys_read
#define write sys_write
#include <windows.h>
#endif /* not WINDOWSNT */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

/* Get SI_SRPC_DOMAIN, if it is available.  */
#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
#endif

#ifdef MSDOS	/* Demacs 1.1.2 91/10/20 Manabu Higashida, MW Aug 1993 */
#include "msdos.h"
#include <sys/param.h>
#endif

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
#include "window.h"
#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "dispextern.h"
#include "process.h"
#include "cm.h"  /* for reset_sys_modes */

#ifdef WINDOWSNT
#include <direct.h>
/* In process.h which conflicts with the local copy.  */
#define _P_WAIT 0
int _cdecl _spawnlp (int, const char *, const char *, ...);
int _cdecl _getpid (void);
extern char *getwd (char *);
#endif

#include "syssignal.h"
#include "systime.h"

static int emacs_get_tty (int, struct emacs_tty *);
static int emacs_set_tty (int, struct emacs_tty *, int);
#if defined TIOCNOTTY || defined USG5 || defined CYGWIN
static _Noreturn void croak (char *);
#endif

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


#if !defined (HAVE_GET_CURRENT_DIR_NAME) || defined (BROKEN_GET_CURRENT_DIR_NAME)

/* Return the current working directory.  Returns NULL on errors.
   Any other returned value must be freed with free. This is used
   only when get_current_dir_name is not defined on the system.  */
char*
get_current_dir_name (void)
{
  char *buf;
  char *pwd;
  struct stat dotstat, pwdstat;
  /* If PWD is accurate, use it instead of calling getwd.  PWD is
     sometimes a nicer name, and using it may avoid a fatal error if a
     parent directory is searchable but not readable.  */
    if ((pwd = getenv ("PWD")) != 0
      && (IS_DIRECTORY_SEP (*pwd) || (*pwd && IS_DEVICE_SEP (pwd[1])))
      && stat (pwd, &pwdstat) == 0
      && stat (".", &dotstat) == 0
      && dotstat.st_ino == pwdstat.st_ino
      && dotstat.st_dev == pwdstat.st_dev
#ifdef MAXPATHLEN
      && strlen (pwd) < MAXPATHLEN
#endif
      )
    {
      buf = malloc (strlen (pwd) + 1);
      if (!buf)
        return NULL;
      strcpy (buf, pwd);
    }
#ifdef HAVE_GETCWD
  else
    {
      size_t buf_size = 1024;
      buf = malloc (buf_size);
      if (!buf)
        return NULL;
      for (;;)
        {
          if (getcwd (buf, buf_size) == buf)
            break;
          if (errno != ERANGE)
            {
              int tmp_errno = errno;
              free (buf);
              errno = tmp_errno;
              return NULL;
            }
          buf_size *= 2;
          buf = realloc (buf, buf_size);
          if (!buf)
            return NULL;
        }
    }
#else
  else
    {
      /* We need MAXPATHLEN here.  */
      buf = malloc (MAXPATHLEN + 1);
      if (!buf)
        return NULL;
      if (getwd (buf) == NULL)
        {
          int tmp_errno = errno;
          free (buf);
          errno = tmp_errno;
          return NULL;
        }
    }
#endif
  return buf;
}
#endif


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
  if (! FRAME_TERMCAP_P (SELECTED_FRAME ()))
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

  baud_rate = (emacs_ospeed < sizeof baud_convert / sizeof baud_convert[0]
	       ? baud_convert[emacs_ospeed] : 9600);
  if (baud_rate == 0)
    baud_rate = 1200;
}



/* Set nonzero to make following function work under dbx
   (at least for bsd).  */
int wait_debugging EXTERNALLY_VISIBLE;

#ifndef MSDOS

static void
wait_for_termination_1 (pid_t pid, int interruptible)
{
  while (1)
    {
#if (defined (BSD_SYSTEM) || defined (HPUX)) && !defined (__GNU__)
      /* Note that kill returns -1 even if the process is just a zombie now.
	 But inevitably a SIGCHLD interrupt should be generated
	 and child_sig will do wait3 and make the process go away. */
      /* There is some indication that there is a bug involved with
	 termination of subprocesses, perhaps involving a kernel bug too,
	 but no idea what it is.  Just as a hunch we signal SIGCHLD to see
	 if that causes the problem to go away or get worse.  */
      sigsetmask (sigmask (SIGCHLD));
      if (0 > kill (pid, 0))
	{
	  sigsetmask (SIGEMPTYMASK);
	  kill (getpid (), SIGCHLD);
	  break;
	}
      if (wait_debugging)
	sleep (1);
      else
	sigpause (SIGEMPTYMASK);
#else /* not BSD_SYSTEM, and not HPUX version >= 6 */
#ifdef WINDOWSNT
      wait (0);
      break;
#else /* not WINDOWSNT */
      sigblock (sigmask (SIGCHLD));
      errno = 0;
      if (kill (pid, 0) == -1 && errno == ESRCH)
	{
	  sigunblock (sigmask (SIGCHLD));
	  break;
	}

      sigsuspend (&empty_mask);
#endif /* not WINDOWSNT */
#endif /* not BSD_SYSTEM, and not HPUX version >= 6 */
      if (interruptible)
	QUIT;
    }
}

/* Wait for subprocess with process id `pid' to terminate and
   make sure it will get eliminated (not remain forever as a zombie) */

void
wait_for_termination (pid_t pid)
{
  wait_for_termination_1 (pid, 0);
}

/* Like the above, but allow keyboard interruption. */
void
interruptible_wait_for_termination (pid_t pid)
{
  wait_for_termination_1 (pid, 1);
}

/*
 *	flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */

void
flush_pending_output (int channel)
{
  /* FIXME: maybe this function should be removed */
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
  /* http://lists.gnu.org/archive/html/emacs-devel/2008-05/msg00406.html
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


/* Record a signal code and the handler for it.  */
struct save_signal
{
  int code;
  void (*handler) (int);
};

static void save_signal_handlers (struct save_signal *);
static void restore_signal_handlers (struct save_signal *);

/* Suspend the Emacs process; give terminal to its superior.  */

void
sys_suspend (void)
{
#if defined (SIGTSTP) && !defined (MSDOS)

  {
    int pgrp = EMACS_GETPGRP (0);
    EMACS_KILLPG (pgrp, SIGTSTP);
  }

#else /* No SIGTSTP */
/* On a system where suspending is not implemented,
   instead fork a subshell and let it talk directly to the terminal
   while we wait.  */
  sys_subshell ();

#endif /* no SIGTSTP */
}

/* Fork a subshell.  */

void
sys_subshell (void)
{
#ifdef DOS_NT	/* Demacs 1.1.2 91/10/20 Manabu Higashida */
  int st;
  char oldwd[MAXPATHLEN+1]; /* Fixed length is safe on MSDOS.  */
#endif
  int pid;
  struct save_signal saved_handlers[5];
  Lisp_Object dir;
  unsigned char *volatile str_volatile = 0;
  unsigned char *str;
  int len;

  saved_handlers[0].code = SIGINT;
  saved_handlers[1].code = SIGQUIT;
  saved_handlers[2].code = SIGTERM;
#ifdef SIGIO
  saved_handlers[3].code = SIGIO;
  saved_handlers[4].code = 0;
#else
  saved_handlers[3].code = 0;
#endif

  /* Mentioning current_buffer->buffer would mean including buffer.h,
     which somehow wedges the hp compiler.  So instead...  */

  dir = intern ("default-directory");
  if (NILP (Fboundp (dir)))
    goto xyzzy;
  dir = Fsymbol_value (dir);
  if (!STRINGP (dir))
    goto xyzzy;

  dir = expand_and_dir_to_file (Funhandled_file_name_directory (dir), Qnil);
  str_volatile = str = alloca (SCHARS (dir) + 2);
  len = SCHARS (dir);
  memcpy (str, SDATA (dir), len);
  if (str[len - 1] != '/') str[len++] = '/';
  str[len] = 0;
 xyzzy:

#ifdef DOS_NT
  pid = 0;
  save_signal_handlers (saved_handlers);
  synch_process_alive = 1;
#else
  pid = vfork ();
  if (pid == -1)
    error ("Can't spawn subshell");
#endif

  if (pid == 0)
    {
      const char *sh = 0;

#ifdef DOS_NT    /* MW, Aug 1993 */
      getwd (oldwd);
      if (sh == 0)
	sh = (char *) egetenv ("SUSPEND");	/* KFS, 1994-12-14 */
#endif
      if (sh == 0)
	sh = (char *) egetenv ("SHELL");
      if (sh == 0)
	sh = "sh";

      /* Use our buffer's default directory for the subshell.  */
      str = str_volatile;
      if (str && chdir ((char *) str) != 0)
	{
#ifndef DOS_NT
	  ignore_value (write (1, "Can't chdir\n", 12));
	  _exit (1);
#endif
	}

      close_process_descs ();	/* Close Emacs's pipes/ptys */

#ifdef MSDOS    /* Demacs 1.1.2 91/10/20 Manabu Higashida */
      {
	char *epwd = getenv ("PWD");
	char old_pwd[MAXPATHLEN+1+4];

	/* If PWD is set, pass it with corrected value.  */
	if (epwd)
	  {
	    strcpy (old_pwd, epwd);
	    if (str[len - 1] == '/')
	      str[len - 1] = '\0';
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
      ignore_value (write (1, "Can't execute subshell", 22));
      _exit (1);
#endif  /* not WINDOWSNT */
#endif /* not MSDOS */
    }

  /* Do this now if we did not do it before.  */
#ifndef MSDOS
  save_signal_handlers (saved_handlers);
  synch_process_alive = 1;
#endif

#ifndef DOS_NT
  wait_for_termination (pid);
#endif
  restore_signal_handlers (saved_handlers);
  synch_process_alive = 0;
}

static void
save_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      saved_handlers->handler
        = (void (*) (int)) signal (saved_handlers->code, SIG_IGN);
      saved_handlers++;
    }
}

static void
restore_signal_handlers (struct save_signal *saved_handlers)
{
  while (saved_handlers->code)
    {
      signal (saved_handlers->code, saved_handlers->handler);
      saved_handlers++;
    }
}

#ifndef SIGIO
/* If SIGIO is broken, don't do anything. */
void
init_sigio (int fd)
{
}

static void
reset_sigio (int fd)
{
}

void
request_sigio (void)
{
}

void
unrequest_sigio (void)
{
}

#else
#ifdef F_SETFL

static int old_fcntl_flags[MAXDESC];

void
init_sigio (int fd)
{
#ifdef FASYNC
  old_fcntl_flags[fd] = fcntl (fd, F_GETFL, 0) & ~FASYNC;
  fcntl (fd, F_SETFL, old_fcntl_flags[fd] | FASYNC);
#endif
  interrupts_deferred = 0;
}

static void
reset_sigio (int fd)
{
#ifdef FASYNC
  fcntl (fd, F_SETFL, old_fcntl_flags[fd]);
#endif
}

#ifdef FASYNC		/* F_SETFL does not imply existence of FASYNC */
/* XXX Uhm, FASYNC is not used anymore here. */
/* XXX Yeah, but you need it for SIGIO, don't you? */

void
request_sigio (void)
{
  if (noninteractive)
    return;

#ifdef SIGWINCH
  sigunblock (sigmask (SIGWINCH));
#endif
  sigunblock (sigmask (SIGIO));

  interrupts_deferred = 0;
}

void
unrequest_sigio (void)
{
  if (noninteractive)
    return;

#if 0 /* XXX What's wrong with blocking SIGIO under X?  */
  if (x_display_list)
    return;
#endif

#ifdef SIGWINCH
  sigblock (sigmask (SIGWINCH));
#endif
  sigblock (sigmask (SIGIO));
  interrupts_deferred = 1;
}

#else /* no FASYNC */
#ifndef MSDOS

void
request_sigio (void)
{
  if (noninteractive || read_socket_hook)
    return;

  croak ("request_sigio");
}

void
unrequest_sigio (void)
{
  if (noninteractive || read_socket_hook)
    return;

  croak ("unrequest_sigio");
}

#endif /* MSDOS */
#endif /* FASYNC */
#endif /* F_SETFL */
#endif /* SIGIO */


/* Getting and setting emacs_tty structures.  */

/* Set *TC to the parameters associated with the terminal FD.
   Return zero if all's well, or -1 if we ran into an error we
   couldn't deal with.  */
int
emacs_get_tty (int fd, struct emacs_tty *settings)
{
  /* Retrieve the primary parameters - baud rate, character size, etcetera.  */
#ifndef DOS_NT
  /* We have those nifty POSIX tcmumbleattr functions.  */
  memset (&settings->main, 0, sizeof (settings->main));
  if (tcgetattr (fd, &settings->main) < 0)
    return -1;
#endif

  /* We have survived the tempest.  */
  return 0;
}


/* Set the parameters of the tty on FD according to the contents of
   *SETTINGS.  If FLUSHP is non-zero, we discard input.
   Return 0 if all went well, and -1 if anything failed.  */

int
emacs_set_tty (int fd, struct emacs_tty *settings, int flushp)
{
  /* Set the primary parameters - baud rate, character size, etcetera.  */
#ifndef DOS_NT
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
static int old_fcntl_owner[MAXDESC];
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
  Lisp_Object terminal;

  Vtty_erase_char = Qnil;

  if (noninteractive)
    return;

  if (!tty_out->output)
    return;                     /* The tty is suspended. */

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

#ifdef F_SETFL
#ifdef F_GETOWN		/* F_SETFL does not imply existence of F_GETOWN */
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
#endif /* F_SETFL */

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

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */

int
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
   to HEIGHT and WIDTH.  This is used mainly with ptys.  */

int
set_window_size (int fd, int height, int width)
{
#ifdef TIOCSWINSZ

  /* BSD-style.  */
  struct winsize size;
  size.ws_row = height;
  size.ws_col = width;

  if (ioctl (fd, TIOCSWINSZ, &size) == -1)
    return 0; /* error */
  else
    return 1;

#else
#ifdef TIOCSSIZE

  /* SunOS - style.  */
  struct ttysize size;
  size.ts_lines = height;
  size.ts_cols = width;

  if (ioctl (fd, TIOCGSIZE, &size) == -1)
    return 0;
  else
    return 1;
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
      fflush (stdout);
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
      int i;
      tty_turn_off_insert (tty_out);

      for (i = curX (tty_out); i < FrameCols (tty_out) - 1; i++)
        {
          fputc (' ', tty_out->output);
        }
    }

  cmgoto (tty_out, FrameRows (tty_out) - 1, 0);
  fflush (tty_out->output);

  if (tty_out->terminal->reset_terminal_modes_hook)
    tty_out->terminal->reset_terminal_modes_hook (tty_out->terminal);

#ifdef BSD_SYSTEM
  /* Avoid possible loss of output when changing terminal modes.  */
  fsync (fileno (tty_out->output));
#endif

#ifdef F_SETFL
#ifdef F_SETOWN		/* F_SETFL does not imply existence of F_SETOWN */
  if (interrupt_input)
    {
      reset_sigio (fileno (tty_out->input));
      fcntl (fileno (tty_out->input), F_SETOWN,
             old_fcntl_owner[fileno (tty_out->input)]);
    }
#endif /* F_SETOWN */
#ifdef O_NDELAY
  fcntl (fileno (tty_out->input), F_SETFL,
         fcntl (fileno (tty_out->input), F_GETFL, 0) & ~O_NDELAY);
#endif
#endif /* F_SETFL */

  if (tty_out->old_tty)
    while (emacs_set_tty (fileno (tty_out->input),
                          tty_out->old_tty, 0) < 0 && errno == EINTR)
      ;

#ifdef MSDOS	/* Demacs 1.1.2 91/10/20 Manabu Higashida */
  dos_ttcooked ();
#endif

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

#ifdef HAVE_SOCKETS
#include <sys/socket.h>
#include <netdb.h>
#endif /* HAVE_SOCKETS */

#ifdef TRY_AGAIN
#ifndef HAVE_H_ERRNO
extern int h_errno;
#endif
#endif /* TRY_AGAIN */

void
init_system_name (void)
{
#ifndef HAVE_GETHOSTNAME
  struct utsname uts;
  uname (&uts);
  Vsystem_name = build_string (uts.nodename);
#else /* HAVE_GETHOSTNAME */
  unsigned int hostname_size = 256;
  char *hostname = alloca (hostname_size);

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

      hostname_size <<= 1;
      hostname = alloca (hostname_size);
    }
#ifdef HAVE_SOCKETS
  /* Turn the hostname into the official, fully-qualified hostname.
     Don't do this if we're going to dump; this can confuse system
     libraries on some machines and make the dumped emacs core dump. */
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* not CANNOT_DUMP */
    if (! strchr (hostname, '.'))
      {
	int count;
#ifdef HAVE_GETADDRINFO
        struct addrinfo *res;
        struct addrinfo hints;
        int ret;

        memset (&hints, 0, sizeof (hints));
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = AI_CANONNAME;

	for (count = 0;; count++)
	  {
            if ((ret = getaddrinfo (hostname, NULL, &hints, &res)) == 0
                || ret != EAI_AGAIN)
              break;

            if (count >= 5)
	      break;
	    Fsleep_for (make_number (1), Qnil);
	  }

        if (ret == 0)
          {
            struct addrinfo *it = res;
            while (it)
              {
                char *fqdn = it->ai_canonname;
                if (fqdn && strchr (fqdn, '.')
                    && strcmp (fqdn, "localhost.localdomain") != 0)
                  break;
                it = it->ai_next;
              }
            if (it)
              {
                hostname = alloca (strlen (it->ai_canonname) + 1);
                strcpy (hostname, it->ai_canonname);
              }
            freeaddrinfo (res);
          }
#else /* !HAVE_GETADDRINFO */
        struct hostent *hp;
	for (count = 0;; count++)
	  {

#ifdef TRY_AGAIN
	    h_errno = 0;
#endif
	    hp = gethostbyname (hostname);
#ifdef TRY_AGAIN
	    if (! (hp == 0 && h_errno == TRY_AGAIN))
#endif

	      break;

	    if (count >= 5)
	      break;
	    Fsleep_for (make_number (1), Qnil);
	  }

	if (hp)
	  {
	    char *fqdn = (char *) hp->h_name;

	    if (!strchr (fqdn, '.'))
	      {
		/* We still don't have a fully qualified domain name.
		   Try to find one in the list of alternate names */
		char **alias = hp->h_aliases;
		while (*alias
		       && (!strchr (*alias, '.')
			   || !strcmp (*alias, "localhost.localdomain")))
		  alias++;
		if (*alias)
		  fqdn = *alias;
	      }
	    hostname = fqdn;
	  }
#endif /* !HAVE_GETADDRINFO */
      }
#endif /* HAVE_SOCKETS */
  Vsystem_name = build_string (hostname);
#endif /* HAVE_GETHOSTNAME */
  {
    unsigned char *p;
    for (p = SDATA (Vsystem_name); *p; p++)
      if (*p == ' ' || *p == '\t')
	*p = '-';
  }
}

/* POSIX signals support - DJB */
/* Anyone with POSIX signals should have ANSI C declarations */

sigset_t empty_mask;

#ifndef WINDOWSNT

signal_handler_t
sys_signal (int signal_number, signal_handler_t action)
{
  struct sigaction new_action, old_action;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_handler = action;
  new_action.sa_flags = 0;
#if defined (SA_RESTART)
  /* Emacs mostly works better with restartable system services. If this
     flag exists, we probably want to turn it on here.
     However, on some systems (only hpux11 at present) this resets the
     timeout of `select' which means that `select' never finishes if
     it keeps getting signals.
     We define BROKEN_SA_RESTART on those systems.  */
  /* It's not clear why the comment above says "mostly works better".  --Stef
     When SYNC_INPUT is set, we don't want SA_RESTART because we need to poll
     for pending input so we need long-running syscalls to be interrupted
     after a signal that sets the interrupt_input_pending flag.  */
  /* Non-interactive keyboard input goes through stdio, where we always
     want restartable system calls.  */
# if defined (BROKEN_SA_RESTART) || defined (SYNC_INPUT)
  if (noninteractive)
# endif
    new_action.sa_flags = SA_RESTART;
#endif
  sigaction (signal_number, &new_action, &old_action);
  return (old_action.sa_handler);
}

#endif	/* WINDOWSNT */

#ifndef __GNUC__
/* If we're compiling with GCC, we don't need this function, since it
   can be written as a macro.  */
sigset_t
sys_sigmask (int sig)
{
  sigset_t mask;
  sigemptyset (&mask);
  sigaddset (&mask, sig);
  return mask;
}
#endif

/* I'd like to have these guys return pointers to the mask storage in here,
   but there'd be trouble if the code was saving multiple masks.  I'll be
   safe and pass the structure.  It normally won't be more than 2 bytes
   anyhow. - DJB */

sigset_t
sys_sigblock (sigset_t new_mask)
{
  sigset_t old_mask;
  pthread_sigmask (SIG_BLOCK, &new_mask, &old_mask);
  return (old_mask);
}

sigset_t
sys_sigunblock (sigset_t new_mask)
{
  sigset_t old_mask;
  pthread_sigmask (SIG_UNBLOCK, &new_mask, &old_mask);
  return (old_mask);
}

sigset_t
sys_sigsetmask (sigset_t new_mask)
{
  sigset_t old_mask;
  pthread_sigmask (SIG_SETMASK, &new_mask, &old_mask);
  return (old_mask);
}


#if !defined HAVE_STRSIGNAL && !HAVE_DECL_SYS_SIGLIST
static char *my_sys_siglist[NSIG];
# ifdef sys_siglist
#  undef sys_siglist
# endif
# define sys_siglist my_sys_siglist
#endif

void
init_signals (void)
{
  sigemptyset (&empty_mask);

#if !defined HAVE_STRSIGNAL && !HAVE_DECL_SYS_SIGLIST
  if (! initialized)
    {
# ifdef SIGABRT
      sys_siglist[SIGABRT] = "Aborted";
# endif
# ifdef SIGAIO
      sys_siglist[SIGAIO] = "LAN I/O interrupt";
# endif
# ifdef SIGALRM
      sys_siglist[SIGALRM] = "Alarm clock";
# endif
# ifdef SIGBUS
      sys_siglist[SIGBUS] = "Bus error";
# endif
# ifdef SIGCLD
      sys_siglist[SIGCLD] = "Child status changed";
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
# ifdef SIGFPE
      sys_siglist[SIGFPE] = "Arithmetic exception";
# endif
# ifdef SIGFREEZE
      sys_siglist[SIGFREEZE] = "SIGFREEZE";
# endif
# ifdef SIGGRANT
      sys_siglist[SIGGRANT] = "Monitor mode granted";
# endif
# ifdef SIGHUP
      sys_siglist[SIGHUP] = "Hangup";
# endif
# ifdef SIGILL
      sys_siglist[SIGILL] = "Illegal instruction";
# endif
# ifdef SIGINT
      sys_siglist[SIGINT] = "Interrupt";
# endif
# ifdef SIGIO
      sys_siglist[SIGIO] = "I/O possible";
# endif
# ifdef SIGIOINT
      sys_siglist[SIGIOINT] = "I/O intervention required";
# endif
# ifdef SIGIOT
      sys_siglist[SIGIOT] = "IOT trap";
# endif
# ifdef SIGKILL
      sys_siglist[SIGKILL] = "Killed";
# endif
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
# ifdef SIGPIPE
      sys_siglist[SIGPIPE] = "Broken pipe";
# endif
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
# ifdef SIGQUIT
      sys_siglist[SIGQUIT] = "Quit";
# endif
# ifdef SIGRETRACT
      sys_siglist[SIGRETRACT] = "Need to relinquish monitor mode";
# endif
# ifdef SIGSAK
      sys_siglist[SIGSAK] = "Secure attention";
# endif
# ifdef SIGSEGV
      sys_siglist[SIGSEGV] = "Segmentation violation";
# endif
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
# ifdef SIGTERM
      sys_siglist[SIGTERM] = "Terminated";
# endif
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
#endif /* !defined HAVE_STRSIGNAL && !defined HAVE_DECL_SYS_SIGLIST */
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

void
seed_random (long int arg)
{
#ifdef HAVE_RANDOM
  srandom ((unsigned int)arg);
#else
# ifdef HAVE_LRAND48
  srand48 (arg);
# else
  srand ((unsigned int)arg);
# endif
#endif
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
	   ^ (val >> (BITS_PER_EMACS_INT - RAND_BITS)));
  val ^= val >> (BITS_PER_EMACS_INT - FIXNUM_BITS);
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

int
emacs_open (const char *path, int oflag, int mode)
{
  register int rtnval;

  while ((rtnval = open (path, oflag, mode)) == -1
	 && (errno == EINTR))
    QUIT;
  return (rtnval);
}

int
emacs_close (int fd)
{
  int did_retry = 0;
  register int rtnval;

  while ((rtnval = close (fd)) == -1
	 && (errno == EINTR))
    did_retry = 1;

  /* If close is interrupted SunOS 4.1 may or may not have closed the
     file descriptor.  If it did the second close will fail with
     errno = EBADF.  That means we have succeeded.  */
  if (rtnval == -1 && did_retry && errno == EBADF)
    return 0;

  return rtnval;
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

/* Read from FILEDESC to a buffer BUF with size NBYTE, retrying if interrupted.
   Return the number of bytes read, which might be less than NBYTE.
   On error, set errno and return -1.  */
ptrdiff_t
emacs_read (int fildes, char *buf, ptrdiff_t nbyte)
{
  register ssize_t rtnval;

  /* There is no need to check against MAX_RW_COUNT, since no caller ever
     passes a size that large to emacs_read.  */

  while ((rtnval = read (fildes, buf, nbyte)) == -1
	 && (errno == EINTR))
    QUIT;
  return (rtnval);
}

/* Write to FILEDES from a buffer BUF with size NBYTE, retrying if interrupted
   or if a partial write occurs.  Return the number of bytes written, setting
   errno if this is less than NBYTE.  */
ptrdiff_t
emacs_write (int fildes, const char *buf, ptrdiff_t nbyte)
{
  ssize_t rtnval;
  ptrdiff_t bytes_written;

  bytes_written = 0;

  while (nbyte > 0)
    {
      rtnval = write (fildes, buf, min (nbyte, MAX_RW_COUNT));

      if (rtnval < 0)
	{
	  if (errno == EINTR)
	    {
#ifdef SYNC_INPUT
	      /* I originally used `QUIT' but that might causes files to
		 be truncated if you hit C-g in the middle of it.  --Stef  */
	      process_pending_signals ();
#endif
	      continue;
	    }
	  else
	    break;
	}

      buf += rtnval;
      nbyte -= rtnval;
      bytes_written += rtnval;
    }

  return (bytes_written);
}

static struct allocator const emacs_norealloc_allocator =
  { xmalloc, NULL, xfree, memory_full };

/* Get the symbolic link value of FILENAME.  Return a pointer to a
   NUL-terminated string.  If readlink fails, return NULL and set
   errno.  If the value fits in INITIAL_BUF, return INITIAL_BUF.
   Otherwise, allocate memory and return a pointer to that memory.  If
   memory allocation fails, diagnose and fail without returning.  If
   successful, store the length of the symbolic link into *LINKLEN.  */
char *
emacs_readlink (char const *filename, char initial_buf[READLINK_BUFSIZE])
{
  return careadlinkat (AT_FDCWD, filename, initial_buf, READLINK_BUFSIZE,
		       &emacs_norealloc_allocator, careadlinkatcwd);
}

#ifdef USG
/*
 *	All of the following are for USG.
 *
 *	On USG systems the system calls are INTERRUPTIBLE by signals
 *	that the user program has elected to catch.  Thus the system call
 *	must be retried in these cases.  To handle this without massive
 *	changes in the source code, we remap the standard system call names
 *	to names for our own functions in sysdep.c that do the system call
 *	with retries.  Actually, for portability reasons, it is good
 *	programming practice, as this example shows, to limit all actual
 *	system calls to a single occurrence in the source.  Sure, this
 *	adds an extra level of function call overhead but it is almost
 *	always negligible.   Fred Fish, Unisoft Systems Inc.
 */

/*
 *	Warning, this function may not duplicate 4.2 action properly
 *	under error conditions.
 */

#ifndef HAVE_GETWD

#ifndef MAXPATHLEN
/* In 4.1, param.h fails to define this.  */
#define MAXPATHLEN 1024
#endif

char *
getwd (char *pathname)
{
  char *npath, *spath;
  extern char *getcwd (char *, size_t);

  BLOCK_INPUT;			/* getcwd uses malloc */
  spath = npath = getcwd ((char *) 0, MAXPATHLEN);
  if (spath == 0)
    {
      UNBLOCK_INPUT;
      return spath;
    }
  /* On Altos 3068, getcwd can return @hostname/dir, so discard
     up to first slash.  Should be harmless on other systems.  */
  while (*npath && *npath != '/')
    npath++;
  strcpy (pathname, npath);
  free (spath);			/* getcwd uses malloc */
  UNBLOCK_INPUT;
  return pathname;
}

#endif /* HAVE_GETWD */

/*
 *	This function will go away as soon as all the stubs fixed. (fnf)
 */

void
croak (char *badfunc)
{
  printf ("%s not yet implemented\r\n", badfunc);
  reset_all_sys_modes ();
  exit (1);
}

#endif /* USG */

/* Directory routines for systems that don't have them. */

#ifdef HAVE_DIRENT_H

#include <dirent.h>

#if !defined (HAVE_CLOSEDIR)

int
closedir (DIR *dirp /* stream from opendir */)
{
  int rtnval;

  rtnval = emacs_close (dirp->dd_fd);
  xfree ((char *) dirp);

  return rtnval;
}
#endif /* not HAVE_CLOSEDIR */
#endif /* HAVE_DIRENT_H */


/* Return a struct timeval that is roughly equivalent to T.
   Use the least timeval not less than T.
   Return an extremal value if the result would overflow.  */
struct timeval
make_timeval (EMACS_TIME t)
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
		EMACS_TIME atime, EMACS_TIME mtime)
{
  struct timespec timespec[2];
  timespec[0] = atime;
  timespec[1] = mtime;
  return fdutimens (fd, filename, timespec);
}

#ifndef HAVE_STRSIGNAL
char *
strsignal (int code)
{
  char *signame = 0;

  if (0 <= code && code < NSIG)
    {
      /* Cast to suppress warning if the table has const char *.  */
      signame = (char *) sys_siglist[code];
    }

  return signame;
}
#endif /* HAVE_STRSIGNAL */

#ifndef DOS_NT
/* For make-serial-process  */
int
serial_open (char *port)
{
  int fd = -1;

  fd = emacs_open ((char*) port,
		   O_RDWR
#ifdef O_NONBLOCK
		   | O_NONBLOCK
#else
		   | O_NDELAY
#endif
#ifdef O_NOCTTY
		   | O_NOCTTY
#endif
		   , 0);
  if (fd < 0)
    {
      error ("Could not open %s: %s",
	     port, emacs_strerror (errno));
    }
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
  int err = -1;
  char summary[4] = "???"; /* This usually becomes "8N1".  */

  childp2 = Fcopy_sequence (PVAR (p, childp));

  /* Read port attributes and prepare default configuration.  */
  err = tcgetattr (p->outfd, &attr);
  if (err != 0)
    error ("tcgetattr() failed: %s", emacs_strerror (errno));
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
    tem = Fplist_get (PVAR (p, childp), QCspeed);
  CHECK_NUMBER (tem);
  err = cfsetspeed (&attr, XINT (tem));
  if (err != 0)
    error ("cfsetspeed(%"pI"d) failed: %s", XINT (tem),
	   emacs_strerror (errno));
  childp2 = Fplist_put (childp2, QCspeed, tem);

  /* Configure bytesize.  */
  if (!NILP (Fplist_member (contact, QCbytesize)))
    tem = Fplist_get (contact, QCbytesize);
  else
    tem = Fplist_get (PVAR (p, childp), QCbytesize);
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
    tem = Fplist_get (PVAR (p, childp), QCparity);
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
    tem = Fplist_get (PVAR (p, childp), QCstopbits);
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
    tem = Fplist_get (PVAR (p, childp), QCflowcontrol);
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
    error ("tcsetattr() failed: %s", emacs_strerror (errno));

  childp2 = Fplist_put (childp2, QCsummary, build_string (summary));
  PVAR (p, childp) = childp2;

}
#endif /* not DOS_NT  */

/* System depended enumeration of and access to system processes a-la ps(1).  */

#ifdef HAVE_PROCFS

/* Process enumeration and access via /proc.  */

Lisp_Object
list_system_processes (void)
{
  Lisp_Object procdir, match, proclist, next;
  struct gcpro gcpro1, gcpro2;
  register Lisp_Object tail;

  GCPRO2 (procdir, match);
  /* For every process on the system, there's a directory in the
     "/proc" pseudo-directory whose name is the numeric ID of that
     process.  */
  procdir = build_string ("/proc");
  match = build_string ("[0-9]+");
  proclist = directory_files_internal (procdir, Qnil, match, Qt, 0, Qnil);

  /* `proclist' gives process IDs as strings.  Destructively convert
     each string into a number.  */
  for (tail = proclist; CONSP (tail); tail = next)
    {
      next = XCDR (tail);
      XSETCAR (tail, Fstring_to_number (XCAR (tail), Qnil));
    }
  UNGCPRO;

  /* directory_files_internal returns the files in reverse order; undo
     that.  */
  proclist = Fnreverse (proclist);
  return proclist;
}

#elif defined BSD_SYSTEM

Lisp_Object
list_system_processes (void)
{
#if defined DARWIN_OS || defined __NetBSD__ || defined __OpenBSD__
  int mib[] = {CTL_KERN, KERN_PROC, KERN_PROC_ALL};
#else
  int mib[] = {CTL_KERN, KERN_PROC, KERN_PROC_PROC};
#endif
  size_t len;
  struct kinfo_proc *procs;
  size_t i;

  struct gcpro gcpro1;
  Lisp_Object proclist = Qnil;

  if (sysctl (mib, 3, NULL, &len, NULL, 0) != 0)
    return proclist;

  procs = xmalloc (len);
  if (sysctl (mib, 3, procs, &len, NULL, 0) != 0)
    {
      xfree (procs);
      return proclist;
    }

  GCPRO1 (proclist);
  len /= sizeof (struct kinfo_proc);
  for (i = 0; i < len; i++)
    {
#if defined DARWIN_OS || defined __NetBSD__
      proclist = Fcons (make_fixnum_or_float (procs[i].kp_proc.p_pid), proclist);
#elif defined __OpenBSD__
      proclist = Fcons (make_fixnum_or_float (procs[i].p_pid), proclist);
#else
      proclist = Fcons (make_fixnum_or_float (procs[i].ki_pid), proclist);
#endif
    }
  UNGCPRO;

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

#ifdef GNU_LINUX
static EMACS_TIME
time_from_jiffies (unsigned long long tval, long hz)
{
  unsigned long long s = tval / hz;
  unsigned long long frac = tval % hz;
  int ns;

  if (TYPE_MAXIMUM (time_t) < s)
    time_overflow ();
  if (LONG_MAX - 1 <= ULLONG_MAX / EMACS_TIME_RESOLUTION
      || frac <= ULLONG_MAX / EMACS_TIME_RESOLUTION)
    ns = frac * EMACS_TIME_RESOLUTION / hz;
  else
    {
      /* This is reachable only in the unlikely case that HZ * HZ
	 exceeds ULLONG_MAX.  It calculates an approximation that is
	 guaranteed to be in range.  */
      long hz_per_ns = (hz / EMACS_TIME_RESOLUTION
			+ (hz % EMACS_TIME_RESOLUTION != 0));
      ns = frac / hz_per_ns;
    }

  return make_emacs_time (s, ns);
}

static Lisp_Object
ltime_from_jiffies (unsigned long long tval, long hz)
{
  EMACS_TIME t = time_from_jiffies (tval, hz);
  return make_lisp_time (t);
}

static EMACS_TIME
get_up_time (void)
{
  FILE *fup;
  EMACS_TIME up = make_emacs_time (0, 0);

  BLOCK_INPUT;
  fup = fopen ("/proc/uptime", "r");

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
	      upfrac = EMACS_TIME_RESOLUTION - 1;
	    }
	  else
	    {
	      int upfraclen = upfrac_end - upfrac_start;
	      for (; upfraclen < LOG10_EMACS_TIME_RESOLUTION; upfraclen++)
		upfrac *= 10;
	      for (; LOG10_EMACS_TIME_RESOLUTION < upfraclen; upfraclen--)
		upfrac /= 10;
	      upfrac = min (upfrac, EMACS_TIME_RESOLUTION - 1);
	    }
	  up = make_emacs_time (upsec, upfrac);
	}
      fclose (fup);
    }
  UNBLOCK_INPUT;

  return up;
}

#define MAJOR(d) (((unsigned)(d) >> 8) & 0xfff)
#define MINOR(d) (((unsigned)(d) & 0xff) | (((unsigned)(d) & 0xfff00000) >> 12))

static Lisp_Object
procfs_ttyname (int rdev)
{
  FILE *fdev = NULL;
  char name[PATH_MAX];

  BLOCK_INPUT;
  fdev = fopen ("/proc/tty/drivers", "r");

  if (fdev)
    {
      unsigned major;
      unsigned long minor_beg, minor_end;
      char minor[25];	/* 2 32-bit numbers + dash */
      char *endp;

      while (!feof (fdev) && !ferror (fdev))
	{
	  if (3 <= fscanf (fdev, "%*s %s %u %s %*s\n", name, &major, minor)
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
  UNBLOCK_INPUT;
  return build_string (name);
}

static unsigned long
procfs_get_total_memory (void)
{
  FILE *fmem = NULL;
  unsigned long retval = 2 * 1024 * 1024; /* default: 2GB */

  BLOCK_INPUT;
  fmem = fopen ("/proc/meminfo", "r");

  if (fmem)
    {
      unsigned long entry_value;
      char entry_name[20];	/* the longest I saw is 13+1 */

      while (!feof (fmem) && !ferror (fmem))
	{
	  if (2 <= fscanf (fmem, "%s %lu kB\n", entry_name, &entry_value)
	      && strcmp (entry_name, "MemTotal:") == 0)
	    {
	      retval = entry_value;
	      break;
	    }
	}
      fclose (fmem);
    }
  UNBLOCK_INPUT;
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
  unsigned char c;
  printmax_t proc_id;
  int ppid, pgrp, sess, tty, tpgid, thcount;
  uid_t uid;
  gid_t gid;
  unsigned long long u_time, s_time, cutime, cstime, start;
  long priority, niceness, rss;
  unsigned long minflt, majflt, cminflt, cmajflt, vsize;
  EMACS_TIME tnow, tstart, tboot, telapsed, us_time;
  double pcpu, pmem;
  Lisp_Object attrs = Qnil;
  Lisp_Object cmd_str, decoded_cmd, tem;
  struct gcpro gcpro1, gcpro2;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, pid_t, proc_id);
  sprintf (procfn, "/proc/%"pMd, proc_id);
  if (stat (procfn, &st) < 0)
    return attrs;

  GCPRO2 (attrs, decoded_cmd);

  /* euid egid */
  uid = st.st_uid;
  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (uid)), attrs);
  BLOCK_INPUT;
  pw = getpwuid (uid);
  UNBLOCK_INPUT;
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  gid = st.st_gid;
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (gid)), attrs);
  BLOCK_INPUT;
  gr = getgrgid (gid);
  UNBLOCK_INPUT;
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  strcpy (fn, procfn);
  procfn_end = fn + strlen (fn);
  strcpy (procfn_end, "/stat");
  fd = emacs_open (fn, O_RDONLY, 0);
  if (fd >= 0 && (nread = emacs_read (fd, procbuf, sizeof (procbuf) - 1)) > 0)
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
      cmd_str = make_unibyte_string (cmd, cmdsize);
      decoded_cmd = code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
      attrs = Fcons (Fcons (Qcomm, decoded_cmd), attrs);

      if (q)
	{
	  EMACS_INT ppid_eint, pgrp_eint, sess_eint, tpgid_eint, thcount_eint;
	  p = q + 2;
	  /* state ppid pgrp sess tty tpgid . minflt cminflt majflt cmajflt utime stime cutime cstime priority nice thcount . start vsize rss */
	  sscanf (p, "%c %d %d %d %d %d %*u %lu %lu %lu %lu %Lu %Lu %Lu %Lu %ld %ld %d %*d %Lu %lu %ld",
		  &c, &ppid, &pgrp, &sess, &tty, &tpgid,
		  &minflt, &cminflt, &majflt, &cmajflt,
		  &u_time, &s_time, &cutime, &cstime,
		  &priority, &niceness, &thcount, &start, &vsize, &rss);
	  {
	    char state_str[2];

	    state_str[0] = c;
	    state_str[1] = '\0';
	    tem =  build_string (state_str);
	    attrs = Fcons (Fcons (Qstate, tem), attrs);
	  }
	  /* Stops GCC whining about limited range of data type.  */
	  ppid_eint = ppid;
	  pgrp_eint = pgrp;
	  sess_eint = sess;
	  tpgid_eint = tpgid;
	  thcount_eint = thcount;
	  attrs = Fcons (Fcons (Qppid, make_fixnum_or_float (ppid_eint)), attrs);
	  attrs = Fcons (Fcons (Qpgrp, make_fixnum_or_float (pgrp_eint)), attrs);
	  attrs = Fcons (Fcons (Qsess, make_fixnum_or_float (sess_eint)), attrs);
	  attrs = Fcons (Fcons (Qttname, procfs_ttyname (tty)), attrs);
	  attrs = Fcons (Fcons (Qtpgid, make_fixnum_or_float (tpgid_eint)), attrs);
	  attrs = Fcons (Fcons (Qminflt, make_fixnum_or_float (minflt)), attrs);
	  attrs = Fcons (Fcons (Qmajflt, make_fixnum_or_float (majflt)), attrs);
	  attrs = Fcons (Fcons (Qcminflt, make_fixnum_or_float (cminflt)), attrs);
	  attrs = Fcons (Fcons (Qcmajflt, make_fixnum_or_float (cmajflt)), attrs);
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
				ltime_from_jiffies (cstime+cutime, clocks_per_sec)),
			 attrs);
	  attrs = Fcons (Fcons (Qpri, make_number (priority)), attrs);
	  attrs = Fcons (Fcons (Qnice, make_number (niceness)), attrs);
	  attrs = Fcons (Fcons (Qthcount, make_fixnum_or_float (thcount_eint)), attrs);
	  tnow = current_emacs_time ();
	  telapsed = get_up_time ();
	  tboot = sub_emacs_time (tnow, telapsed);
	  tstart = time_from_jiffies (start, clocks_per_sec);
	  tstart = add_emacs_time (tboot, tstart);
	  attrs = Fcons (Fcons (Qstart, make_lisp_time (tstart)), attrs);
	  attrs = Fcons (Fcons (Qvsize, make_fixnum_or_float (vsize/1024)), attrs);
	  attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (4*rss)), attrs);
	  telapsed = sub_emacs_time (tnow, tstart);
	  attrs = Fcons (Fcons (Qetime, make_lisp_time (telapsed)), attrs);
	  us_time = time_from_jiffies (u_time + s_time, clocks_per_sec);
	  pcpu = (EMACS_TIME_TO_DOUBLE (us_time)
		  / EMACS_TIME_TO_DOUBLE (telapsed));
	  if (pcpu > 1.0)
	    pcpu = 1.0;
	  attrs = Fcons (Fcons (Qpcpu, make_float (100 * pcpu)), attrs);
	  pmem = 4.0 * 100 * rss / procfs_get_total_memory ();
	  if (pmem > 100)
	    pmem = 100;
	  attrs = Fcons (Fcons (Qpmem, make_float (pmem)), attrs);
	}
    }
  if (fd >= 0)
    emacs_close (fd);

  /* args */
  strcpy (procfn_end, "/cmdline");
  fd = emacs_open (fn, O_RDONLY, 0);
  if (fd >= 0)
    {
      char ch;
      for (cmdline_size = 0; cmdline_size < STRING_BYTES_BOUND; cmdline_size++)
	{
	  if (emacs_read (fd, &ch, 1) != 1)
	    break;
	  c = ch;
	  if (isspace (c) || c == '\\')
	    cmdline_size++;	/* for later quoting, see below */
	}
      if (cmdline_size)
	{
	  cmdline = xmalloc (cmdline_size + 1);
	  lseek (fd, 0L, SEEK_SET);
	  cmdline[0] = '\0';
	  if ((nread = read (fd, cmdline, cmdline_size)) >= 0)
	    cmdline[nread++] = '\0';
	  else
	    {
	      /* Assigning zero to `nread' makes us skip the following
		 two loops, assign zero to cmdline_size, and enter the
		 following `if' clause that handles unknown command
		 lines.  */
	      nread = 0;
	    }
	  /* We don't want trailing null characters.  */
	  for (p = cmdline + nread; p > cmdline + 1 && !p[-1]; p--)
	    nread--;
	  for (p = cmdline; p < cmdline + nread; p++)
	    {
	      /* Escape-quote whitespace and backslashes.  */
	      if (isspace (*p) || *p == '\\')
		{
		  memmove (p + 1, p, nread - (p - cmdline));
		  nread++;
		  *p++ = '\\';
		}
	      else if (*p == '\0')
		*p = ' ';
	    }
	  cmdline_size = nread;
	}
      if (!cmdline_size)
	{
	  cmdline_size = cmdsize + 2;
	  cmdline = xmalloc (cmdline_size + 1);
	  sprintf (cmdline, "[%.*s]", cmdsize, cmd);
	}
      emacs_close (fd);
      /* Command line is encoded in locale-coding-system; decode it.  */
      cmd_str = make_unibyte_string (cmdline, cmdline_size);
      decoded_cmd = code_convert_string_norecord (cmd_str,
						  Vlocale_coding_system, 0);
      xfree (cmdline);
      attrs = Fcons (Fcons (Qargs, decoded_cmd), attrs);
    }

  UNGCPRO;
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
  Lisp_Object decoded_cmd, tem;
  struct gcpro gcpro1, gcpro2;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, pid_t, proc_id);
  sprintf (procfn, "/proc/%"pMd, proc_id);
  if (stat (procfn, &st) < 0)
    return attrs;

  GCPRO2 (attrs, decoded_cmd);

  /* euid egid */
  uid = st.st_uid;
  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (uid)), attrs);
  BLOCK_INPUT;
  pw = getpwuid (uid);
  UNBLOCK_INPUT;
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  gid = st.st_gid;
  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (gid)), attrs);
  BLOCK_INPUT;
  gr = getgrgid (gid);
  UNBLOCK_INPUT;
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  strcpy (fn, procfn);
  procfn_end = fn + strlen (fn);
  strcpy (procfn_end, "/psinfo");
  fd = emacs_open (fn, O_RDONLY, 0);
  if (fd >= 0
      && (nread = read (fd, (char*)&pinfo, sizeof (struct psinfo)) > 0))
    {
          attrs = Fcons (Fcons (Qppid, make_fixnum_or_float (pinfo.pr_ppid)), attrs);
	  attrs = Fcons (Fcons (Qpgrp, make_fixnum_or_float (pinfo.pr_pgid)), attrs);
	  attrs = Fcons (Fcons (Qsess, make_fixnum_or_float (pinfo.pr_sid)), attrs);

	  {
	    char state_str[2];
	    state_str[0] =  pinfo.pr_lwp.pr_sname;
	    state_str[1] =  '\0';
	    tem =   build_string (state_str);
	    attrs =  Fcons (Fcons (Qstate,  tem),  attrs);
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
	  attrs = Fcons (Fcons (Qthcount, make_fixnum_or_float (pinfo.pr_nlwp)), attrs);

	  attrs = Fcons (Fcons (Qstart, make_lisp_time (pinfo.pr_start)), attrs);
	  attrs = Fcons (Fcons (Qvsize, make_fixnum_or_float (pinfo.pr_size)), attrs);
	  attrs = Fcons (Fcons (Qrss, make_fixnum_or_float (pinfo.pr_rssize)), attrs);

	  /* pr_pctcpu and pr_pctmem are unsigned integers in the
	     range 0 .. 2**15, representing 0.0 .. 1.0.  */
	  attrs = Fcons (Fcons (Qpcpu, make_float (100.0 / 0x8000 * pinfo.pr_pctcpu)), attrs);
	  attrs = Fcons (Fcons (Qpmem, make_float (100.0 / 0x8000 * pinfo.pr_pctmem)), attrs);

	  decoded_cmd
	    =  code_convert_string_norecord (make_unibyte_string (pinfo.pr_fname,
								  strlen (pinfo.pr_fname)),
					     Vlocale_coding_system,  0);
	  attrs =  Fcons (Fcons (Qcomm,  decoded_cmd),  attrs);
	  decoded_cmd
	    =  code_convert_string_norecord (make_unibyte_string (pinfo.pr_psargs,
								  strlen (pinfo.pr_psargs)),
					     Vlocale_coding_system,  0);
	  attrs =  Fcons (Fcons (Qargs,  decoded_cmd),  attrs);
    }

  if (fd >= 0)
    emacs_close (fd);

  UNGCPRO;
  return attrs;
}

#elif defined __FreeBSD__

static EMACS_TIME
timeval_to_EMACS_TIME (struct timeval t)
{
  return make_emacs_time (t.tv_sec, t.tv_usec * 1000);
}

static Lisp_Object
make_lisp_timeval (struct timeval t)
{
  return make_lisp_time (timeval_to_EMACS_TIME (t));
}

Lisp_Object
system_process_attributes (Lisp_Object pid)
{
  int proc_id;
  int pagesize = getpagesize ();
  int npages;
  int fscale;
  struct passwd *pw;
  struct group  *gr;
  char *ttyname;
  size_t len;
  char args[MAXPATHLEN];
  EMACS_TIME t, now;

  int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID};
  struct kinfo_proc proc;
  size_t proclen = sizeof proc;

  struct gcpro gcpro1, gcpro2;
  Lisp_Object attrs = Qnil;
  Lisp_Object decoded_comm;

  CHECK_NUMBER_OR_FLOAT (pid);
  CONS_TO_INTEGER (pid, int, proc_id);
  mib[3] = proc_id;

  if (sysctl (mib, 4, &proc, &proclen, NULL, 0) != 0)
    return attrs;

  GCPRO2 (attrs, decoded_comm);

  attrs = Fcons (Fcons (Qeuid, make_fixnum_or_float (proc.ki_uid)), attrs);

  BLOCK_INPUT;
  pw = getpwuid (proc.ki_uid);
  UNBLOCK_INPUT;
  if (pw)
    attrs = Fcons (Fcons (Quser, build_string (pw->pw_name)), attrs);

  attrs = Fcons (Fcons (Qegid, make_fixnum_or_float (proc.ki_svgid)), attrs);

  BLOCK_INPUT;
  gr = getgrgid (proc.ki_svgid);
  UNBLOCK_INPUT;
  if (gr)
    attrs = Fcons (Fcons (Qgroup, build_string (gr->gr_name)), attrs);

  decoded_comm = code_convert_string_norecord
    (make_unibyte_string (proc.ki_comm, strlen (proc.ki_comm)),
     Vlocale_coding_system, 0);

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

  BLOCK_INPUT;
  ttyname = proc.ki_tdev == NODEV ? NULL : devname (proc.ki_tdev, S_IFCHR);
  UNBLOCK_INPUT;
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
  t = add_emacs_time (timeval_to_EMACS_TIME (proc.ki_rusage.ru_utime),
		      timeval_to_EMACS_TIME (proc.ki_rusage.ru_stime));
  attrs = Fcons (Fcons (Qtime, make_lisp_time (t)), attrs);

  attrs = Fcons (Fcons (Qcutime,
			make_lisp_timeval (proc.ki_rusage_ch.ru_utime)),
		 attrs);
  attrs = Fcons (Fcons (Qcstime,
			make_lisp_timeval (proc.ki_rusage_ch.ru_utime)),
		 attrs);
  t = add_emacs_time (timeval_to_EMACS_TIME (proc.ki_rusage_ch.ru_utime),
		      timeval_to_EMACS_TIME (proc.ki_rusage_ch.ru_stime));
  attrs = Fcons (Fcons (Qctime, make_lisp_time (t)), attrs);

  attrs = Fcons (Fcons (Qthcount, make_fixnum_or_float (proc.ki_numthreads)),
		 attrs);
  attrs = Fcons (Fcons (Qpri,   make_number (proc.ki_pri.pri_native)), attrs);
  attrs = Fcons (Fcons (Qnice,  make_number (proc.ki_nice)), attrs);
  attrs = Fcons (Fcons (Qstart, make_lisp_timeval (proc.ki_start)), attrs);
  attrs = Fcons (Fcons (Qvsize, make_number (proc.ki_size >> 10)), attrs);
  attrs = Fcons (Fcons (Qrss,   make_number (proc.ki_rssize * pagesize >> 10)),
		 attrs);

  now = current_emacs_time ();
  t = sub_emacs_time (now, timeval_to_EMACS_TIME (proc.ki_start));
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

      decoded_comm =
	(code_convert_string_norecord
	 (build_unibyte_string (args),
	  Vlocale_coding_system, 0));

      attrs = Fcons (Fcons (Qargs, decoded_comm), attrs);
    }

  UNGCPRO;
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
