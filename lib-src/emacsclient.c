/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1994, 1999, 2000, 2001, 2003
   Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#define NO_SHORTNAMES

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#undef signal

#include <ctype.h>
#include <stdio.h>
#include <getopt.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef VMS
# include "vms-pwd.h"
#else
# include <pwd.h>
#endif /* not VMS */


/****************************************/

#include <errno.h>
#include <signal.h>

#ifndef INCLUDED_FCNTL
#define INCLUDED_FCNTL
#include <fcntl.h>
#endif

#ifdef HAVE_TERMIOS
#ifndef NO_TERMIO
#include <termio.h>
#endif
#include <termios.h>
#endif /* not HAVE_TERMIOS */

#ifdef __GNU_LIBRARY__
#include <sys/ioctl.h>
#include <termios.h>
#endif

#if (defined (POSIX) || defined (NEED_UNISTD_H)) && defined (HAVE_UNISTD_H)
#include <unistd.h>
#endif



/* Try to establish the correct character to disable terminal functions
   in a system-independent manner.  Note that USG (at least) define
   _POSIX_VDISABLE as 0!  */

#ifdef _POSIX_VDISABLE
#define CDISABLE _POSIX_VDISABLE
#else /* not _POSIX_VDISABLE */
#ifdef CDEL
#undef CDISABLE
#define CDISABLE CDEL
#else /* not CDEL */
#define CDISABLE 255
#endif /* not CDEL */
#endif /* not _POSIX_VDISABLE */



/****************************************/

char *getenv (), *getwd ();
char *getcwd ();

/* This is defined with -D from the compilation command,
   which extracts it from ../lisp/version.el.  */

#ifndef VERSION
#define VERSION "unspecified"
#endif

/* Name used to invoke this program.  */
char *progname;

/* The first argument to main. */
int main_argc;

/* The second argument to main. */
char **main_argv;

/* Nonzero means don't wait for a response from Emacs.  --no-wait.  */
int nowait = 0;

/* Nonzero means args are expressions to be evaluated.  --eval.  */
int eval = 0;

/* The display on which Emacs should work.  --display.  */
char *display = NULL;

/* Nonzero means open a new Emacs frame on the current terminal. */
int frame = 0;

/* If non-NULL, the name of an editor to fallback to if the server
   is not running.  --alternate-editor.   */
const char * alternate_editor = NULL;

/* If non-NULL, the filename of the UNIX socket.  */
char *socket_name = NULL;

void print_help_and_exit ();

struct option longopts[] =
{
  { "no-wait",	no_argument,	   NULL, 'n' },
  { "eval",	no_argument,	   NULL, 'e' },
  { "help",	no_argument,	   NULL, 'H' },
  { "version",	no_argument,	   NULL, 'V' },
  { "frame",	no_argument,       NULL, 'f' },
  { "alternate-editor", required_argument, NULL, 'a' },
  { "socket-name",	required_argument, NULL, 's' },
  { "display",	required_argument, NULL, 'd' },
  { 0, 0, 0, 0 }
};

/* Decode the options from argv and argc.
   The global variable `optind' will say how many arguments we used up.  */

void
decode_options (argc, argv)
     int argc;
     char **argv;
{
  while (1)
    {
      int opt = getopt_long (argc, argv,
			     "VHnea:s:d:t", longopts, 0);

      if (opt == EOF)
	break;

      alternate_editor = getenv ("ALTERNATE_EDITOR");

      switch (opt)
	{
	case 0:
	  /* If getopt returns 0, then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	case 'a':
	  alternate_editor = optarg;
	  break;

	case 's':
	  socket_name = optarg;
	  break;

	case 'd':
	  display = optarg;
	  break;

	case 'n':
	  nowait = 1;
	  break;

	case 'e':
	  eval = 1;
	  break;

	case 'V':
	  printf ("emacsclient %s\n", VERSION);
	  exit (0);
	  break;

        case 't':
          frame = 1;
          break;
          
	case 'H':
	  print_help_and_exit ();
	  break;

	default:
	  fprintf (stderr, "Try `%s --help' for more information\n", progname);
	  exit (1);
	  break;
	}
    }

  if (frame) {
    nowait = 0;
    display = 0;
  }
  
}

void
print_help_and_exit ()
{
  printf (
	  "Usage: %s [OPTIONS] FILE...\n\
Tell the Emacs server to visit the specified files.\n\
Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.\n\
\n\
The following OPTIONS are accepted:\n\
-V, --version           Just print a version info and return\n\
-H, --help              Print this usage information message\n\
-t, --tty               Open a new Emacs frame on the current terminal\n\
-n, --no-wait           Don't wait for the server to return\n\
-e, --eval              Evaluate the FILE arguments as ELisp expressions\n\
-d, --display=DISPLAY   Visit the file in the given display\n\
-s, --socket-name=FILENAME\n\
                        Set the filename of the UNIX socket for communication\n\
-a, --alternate-editor=EDITOR\n\
                        Editor to fallback to if the server is not running\n\
\n\
Report bugs to bug-gnu-emacs@gnu.org.\n", progname);
  exit (0);
}

/* In NAME, insert a & before each &, each space, each newline, and
   any initial -.  Change spaces to underscores, too, so that the
   return value never contains a space.  */

void
quote_file_name (name, stream)
     char *name;
     FILE *stream;
{
  char *copy = (char *) malloc (strlen (name) * 2 + 1);
  char *p, *q;

  p = name;
  q = copy;
  while (*p)
    {
      if (*p == ' ')
	{
	  *q++ = '&';
	  *q++ = '_';
	  p++;
	}
      else if (*p == '\n')
	{
	  *q++ = '&';
	  *q++ = 'n';
	  p++;
	}
      else
	{
	  if (*p == '&' || (*p == '-' && p == name))
	    *q++ = '&';
	  *q++ = *p++;
	}
    }
  *q++ = 0;

  fprintf (stream, copy);

  free (copy);
}

/* Like malloc but get fatal error if memory is exhausted.  */

long *
xmalloc (size)
     unsigned int size;
{
  long *result = (long *) malloc (size);
  if (result == NULL)
  {
    perror ("malloc");
    exit (1);
  }
  return result;
}

/*
  Try to run a different command, or --if no alternate editor is
  defined-- exit with an errorcode.
*/
void
fail (void)
{
  if (alternate_editor)
    {
      int i = optind - 1;
      execvp (alternate_editor, main_argv + i);
      return;
    }
  else
    {
      exit (1);
    }
}


#ifdef HAVE_TERMIOS

/* Adapted from emacs_get_tty() in sysdep.c. */
int
ec_get_tty (int fd, struct termios *settings)
{
  bzero (settings, sizeof (struct termios));
  if (tcgetattr (fd, settings) < 0)
    return -1;
  return 0;
}

/* Adapted from emacs_set_tty() in sysdep.c. */
int
ec_set_tty (int fd, struct termios *settings, int flushp)
{
  /* Set the primary parameters - baud rate, character size, etcetera.  */

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
    if (tcsetattr (fd, flushp ? TCSAFLUSH : TCSADRAIN, settings) < 0)
      {
	if (errno == EINTR)
	  continue;
	else
	  return -1;
      }
    else
      {
	struct termios new;
        
	bzero (&new, sizeof (new));
	/* Get the current settings, and see if they're what we asked for.  */
	tcgetattr (fd, &new);
	/* We cannot use memcmp on the whole structure here because under
	 * aix386 the termios structure has some reserved field that may
	 * not be filled in.
	 */
	if (   new.c_iflag == settings->c_iflag
	    && new.c_oflag == settings->c_oflag
	    && new.c_cflag == settings->c_cflag
	    && new.c_lflag == settings->c_lflag
	    && memcmp (new.c_cc, settings->c_cc, NCCS) == 0)
	  break;
	else
	  continue;
      }
  return 0;
}

int master;
char *pty_name;

struct termios old_tty;
struct termios tty;
int old_tty_valid;

int tty_erase_char;
int quit_char = 'g' & 037;
int flow_control = 0;
int meta_key = 0;
char _sobuf[BUFSIZ];
int emacs_pid;

/* Adapted from init_sys_modes() in sysdep.c. */
int
init_tty ()
{
  if (! isatty (0))
    {
      fprintf (stderr, "%s: Input is not a terminal", "init_tty");
      return 0;
    }
  
  ec_get_tty (0, &old_tty);
  old_tty_valid = 1;
  tty = old_tty;
  
  tty_erase_char = old_tty.c_cc[VERASE];
  
  tty.c_iflag |= (IGNBRK);	/* Ignore break condition */
  tty.c_iflag &= ~ICRNL;	/* Disable map of CR to NL on input */
#ifdef INLCR
  tty.c_iflag &= ~INLCR;	/* Disable map of NL to CR on input */
#endif
#ifdef ISTRIP
  tty.c_iflag &= ~ISTRIP;	/* don't strip 8th bit on input */
#endif
  tty.c_lflag &= ~ECHO;         /* Disable echo */
  tty.c_lflag &= ~ICANON;	/* Disable erase/kill processing */
#ifdef IEXTEN
  tty.c_lflag &= ~IEXTEN;	/* Disable other editing characters.  */
#endif
  tty.c_lflag |= ISIG;          /* Enable signals */
  if (flow_control)
    {
      tty.c_iflag |= IXON;	/* Enable start/stop output control */
#ifdef IXANY
      tty.c_iflag &= ~IXANY;
#endif /* IXANY */
    }
  else
    tty.c_iflag &= ~IXON;	/* Disable start/stop output control */
  tty.c_oflag &= ~ONLCR;	/* Disable map of NL to CR-NL
                                   on output */
  tty.c_oflag &= ~TAB3;         /* Disable tab expansion */
#ifdef CS8
  if (meta_key)
    {
      tty.c_cflag |= CS8;	/* allow 8th bit on input */
      tty.c_cflag &= ~PARENB;   /* Don't check parity */
    }
#endif
  tty.c_cc[VINTR] = quit_char;	/* C-g (usually) gives SIGINT */
  /* Set up C-g for both SIGQUIT and SIGINT.
     We don't know which we will get, but we handle both alike
     so which one it really gives us does not matter.  */
  tty.c_cc[VQUIT] = quit_char;
  tty.c_cc[VMIN] = 1;      /* Input should wait for at least 1 char */
  tty.c_cc[VTIME] = 0;          /* no matter how long that takes.  */
#ifdef VSWTCH
  tty.c_cc[VSWTCH] = CDISABLE;	/* Turn off shell layering use of C-z */
#endif

#ifdef VSUSP
  tty.c_cc[VSUSP] = CDISABLE;	/* Turn off mips handling of C-z.  */
#endif /* VSUSP */
#ifdef V_DSUSP
  tty.c_cc[V_DSUSP] = CDISABLE; /* Turn off mips handling of C-y.  */
#endif /* V_DSUSP */
#ifdef VDSUSP /* Some systems have VDSUSP, some have V_DSUSP.  */
  tty.c_cc[VDSUSP] = CDISABLE;
#endif /* VDSUSP */
#ifdef VLNEXT
  tty.c_cc[VLNEXT] = CDISABLE;
#endif /* VLNEXT */
#ifdef VREPRINT
  tty.c_cc[VREPRINT] = CDISABLE;
#endif /* VREPRINT */
#ifdef VWERASE
  tty.c_cc[VWERASE] = CDISABLE;
#endif /* VWERASE */
#ifdef VDISCARD
  tty.c_cc[VDISCARD] = CDISABLE;
#endif /* VDISCARD */

  if (flow_control)
    {
#ifdef VSTART
      tty.c_cc[VSTART] = '\021';
#endif /* VSTART */
#ifdef VSTOP
      tty.c_cc[VSTOP] = '\023';
#endif /* VSTOP */
    }
  else
    {
#ifdef VSTART
      tty.c_cc[VSTART] = CDISABLE;
#endif /* VSTART */
#ifdef VSTOP
      tty.c_cc[VSTOP] = CDISABLE;
#endif /* VSTOP */
    }
  
#ifdef SET_LINE_DISCIPLINE
  /* Need to explicitly request TERMIODISC line discipline or
     Ultrix's termios does not work correctly.  */
  tty.c_line = SET_LINE_DISCIPLINE;
#endif
  
#ifdef AIX
#ifndef IBMR2AIX
  /* AIX enhanced edit loses NULs, so disable it.  */
  tty.c_line = 0;
  tty.c_iflag &= ~ASCEDIT;
#else
  tty.c_cc[VSTRT] = 255;
  tty.c_cc[VSTOP] = 255;
  tty.c_cc[VSUSP] = 255;
  tty.c_cc[VDSUSP] = 255;
#endif /* IBMR2AIX */
  if (flow_control)
    {
#ifdef VSTART
      tty.c_cc[VSTART] = '\021';
#endif /* VSTART */
#ifdef VSTOP
      tty.c_cc[VSTOP] = '\023';
#endif /* VSTOP */
    }
  /* Also, PTY overloads NUL and BREAK.
     don't ignore break, but don't signal either, so it looks like NUL.
     This really serves a purpose only if running in an XTERM window
     or via TELNET or the like, but does no harm elsewhere.  */
  tty.c_iflag &= ~IGNBRK;
  tty.c_iflag &= ~BRKINT;
#endif /* AIX */
  
  ec_set_tty (0, &tty, 0);

      /* This code added to insure that, if flow-control is not to be used,
	 we have an unlocked terminal at the start. */

#ifdef TCXONC
  if (!flow_control) ioctl (0, TCXONC, 1);
#endif
#ifndef APOLLO
#ifdef TIOCSTART
  if (!flow_control) ioctl (0, TIOCSTART, 0);
#endif
#endif

#if defined (HAVE_TERMIOS) || defined (HPUX9)
#ifdef TCOON
  if (!flow_control) tcflow (0, TCOON);
#endif
#endif
  
#ifdef _IOFBF
  /* This symbol is defined on recent USG systems.
     Someone says without this call USG won't really buffer the file
     even with a call to setbuf. */
  setvbuf (stdout, (char *) _sobuf, _IOFBF, sizeof _sobuf);
#else
  setbuf (stdout, (char *) _sobuf);
#endif

  return 1;
}

void
window_change ()
{
  int width = 0, height = 0;

#ifdef TIOCGWINSZ
  {
    /* BSD-style.  */
    struct winsize size;
    
    if (ioctl (0, TIOCGWINSZ, &size) == -1)
      width = height = 0;
    else
      {
        width = size.ws_col;
        height = size.ws_row;
      }
  }
#else
#ifdef TIOCGSIZE
  {
    /* SunOS - style.  */
    struct ttysize size;
    
    if (ioctl (0, TIOCGSIZE, &size) == -1)
      width = height = 0;
    else
      {
        width = size.ts_cols;
        height = size.ts_lines;
      }
  }
#endif /* not SunOS-style */
#endif /* not BSD-style */

#ifdef TIOCSWINSZ
  {
    /* BSD-style.  */
    struct winsize size;
    size.ws_row = height;
    size.ws_col = width;
    
    ioctl (master, TIOCSWINSZ, &size);
  }
#else
#ifdef TIOCSSIZE
  {
    /* SunOS - style.  */
    struct ttysize size;
    size.ts_lines = height;
    size.ts_cols = width;
    
    ioctl (master, TIOCGSIZE, &size);
  }
#endif /* not SunOS-style */
#endif /* not BSD-style */

  if (emacs_pid && width && height)
    kill (emacs_pid, SIGWINCH);
}

int in_conversation = 0;
int quit_conversation = 0;

SIGTYPE
hang_up_signal (int signalnum)
{
  int old_errno = errno;
  
  if (! in_conversation)
    return;

  quit_conversation = 1;
  
  errno = old_errno;
}

SIGTYPE
window_change_signal (int signalnum)
{
  int old_errno = errno;

  if (! in_conversation)
    goto end;

  window_change();

 end:
  signal (SIGWINCH, window_change_signal);
  errno = old_errno;
}

SIGTYPE
interrupt_signal (int signalnum)
{
  int old_errno = errno;
  
  /* Forward it to Emacs. */
  if (emacs_pid)
    kill (emacs_pid, SIGINT);

  errno = old_errno;
}

int
init_signals ()
{
  /* Set up signal handlers. */
  signal (SIGWINCH, window_change_signal);
  signal (SIGHUP, hang_up_signal);
  signal (SIGINT, interrupt_signal);
  return 1;
}



/* Adapted from reset_sys_modes in sysdep.c. */
int
reset_tty ()
{
  fflush (stdout);
#ifdef BSD_SYSTEM
#ifndef BSD4_1
  /* Avoid possible loss of output when changing terminal modes.  */
  fsync (fileno (stdout));
#endif
#endif

#ifdef F_SETFL
#ifdef O_NDELAY
  fcntl (0, F_SETFL, fcntl (0, F_GETFL, 0) & ~O_NDELAY);
#endif
#endif /* F_SETFL */

  if (old_tty_valid)
    while (ec_set_tty (0, &old_tty, 0) < 0 && errno == EINTR)
      ;

  return 1;
}


int
init_pty ()
{
  master = getpt ();
  if (master < 0)
    return 0;

  if (grantpt (master) < 0 || unlockpt (master) < 0)
    goto close_master;
  pty_name = strdup (ptsname (master));
  if (! pty_name)
    goto close_master;

  /* Propagate window size. */
  window_change ();
  
  return 1;
  
 close_master:
  close (master);
  return 0;
}

int
copy_from_to (int in, int out, int sigio)
{
  static char buf[BUFSIZ];
  int nread = read (in, &buf, BUFSIZ);
  if (nread == 0)
    return 1;                   /* EOF */
  else if (nread < 0 && errno != EAGAIN)
    return 0;
  else if (nread > 0)
    {
      int r = 0;
      int written = 0;

      do {
        r = write (out, &buf, nread);
      } while ((r < 0 && errno == EINTR)
               || (r > 0 && (written += r) && written != nread));
      
      if (r < 0)
        return 0;

      if (sigio && emacs_pid)
        kill (emacs_pid, SIGIO);
    }
  return 1;
}

int
pty_conversation (FILE *in)
{
  char *str;
  char string[BUFSIZ];              
  fd_set set, rset;
  int res;

  FD_ZERO (&set);
  FD_SET (master, &set);
  FD_SET (1, &set);
  FD_SET (fileno (in), &set);

  in_conversation = 1;
  
  while (! quit_conversation) {
    rset = set;
    res = select (FD_SETSIZE, &rset, NULL, NULL, NULL);
    if (res < 0 && errno != EINTR)
      {
          reset_tty ();
          fprintf (stderr, "%s: ", progname);
          perror ("select");
          return 0;             /* Error */
      }
    else if (res > 0)
      {
        if (FD_ISSET (master, &rset))
          {
            /* Copy Emacs output to stdout. */
            if (! copy_from_to (master, 0, 0))
              {
                FD_CLR (master, &set);
              }
          }
        if (FD_ISSET (1, &rset))
          {
            /* Forward user input to Emacs. */
            if (! copy_from_to (1, master, 1))
              {
                FD_CLR (master, &set);
              }
          }
        if (FD_ISSET (fileno (in), &rset))
          {
            do {
              res = read (fileno (in), string, BUFSIZ-1);
            } while (res < 0 && errno == EINTR);
            if (res < 0)
              {
                reset_tty ();
                fprintf (stderr, "%s: ", progname);
                perror ("read");
                return 0;
              }
            if (!res)
              {
                return 1;
              }
            
            string[res] = 0;
            if (string[res-1] == '\n')
              string[res-1] = 0;
            
            if (! emacs_pid)
              {
                /* Get the pid of the Emacs process.
                   XXX Is there some nifty libc/kernel feature for doing this?
                */
                if (! string[0])
                  {
                    reset_tty ();
                    fprintf (stderr, "%s: could not get Emacs process id\n"
                             "Maybe this Emacs does not support multiple terminals.\n", progname);
                    return 0;
                  }
                emacs_pid = strtol (string, NULL, 10);
              }
            
            if (! emacs_pid)    /* emacs_pid should be set above */
              {
                reset_tty ();
                fprintf (stderr, "%s: %s\n", progname, string);
                return 0;
              }
          }
      }
  }
  return 1;
}

#endif /* HAVE_TERMIOS */


#if !defined (HAVE_SOCKETS) || defined (NO_SOCKETS_IN_FILE_SYSTEM)

int
main (argc, argv)
     int argc;
     char **argv;
{
  fprintf (stderr, "%s: Sorry, the Emacs server is supported only\n",
	   argv[0]);
  fprintf (stderr, "on systems with Berkeley sockets.\n");

  fail ();
}

#else /* HAVE_SOCKETS */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <errno.h>

extern char *strerror ();
extern int errno;

/* Three possibilities:
   2 - can't be `stat'ed		(sets errno)
   1 - isn't owned by us
   0 - success: none of the above */

static int
socket_status (socket_name)
     char *socket_name;
{
  struct stat statbfr;

  if (stat (socket_name, &statbfr) == -1)
    return 2;

  if (statbfr.st_uid != geteuid ())
    return 1;

  return 0;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  char *system_name;
  int system_name_length;
  int s, i, needlf = 0;
  FILE *out, *in;
  struct sockaddr_un server;
  char *cwd, *str;
  char string[BUFSIZ];

  main_argc = argc;
  main_argv = argv;
  progname = argv[0];

  /* Process options.  */
  decode_options (argc, argv);

  if ((argc - optind < 1) && !eval && !frame)
    {
      fprintf (stderr, "%s: file name or argument required\n", progname);
      fprintf (stderr, "Try `%s --help' for more information\n", progname);
      exit (1);
    }

  /*
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("socket");
      fail ();
    }

  server.sun_family = AF_UNIX;

  {
    char *dot;
    system_name_length = 32;

    while (1)
      {
	system_name = (char *) xmalloc (system_name_length + 1);

	/* system_name must be null-terminated string.  */
	system_name[system_name_length] = '\0';

 	if (gethostname (system_name, system_name_length) == 0)
	  break;

	free (system_name);
	system_name_length *= 2;
      }

    /* We always use the non-dotted host name, for simplicity.  */
    dot = index (system_name, '.');
    if (dot)
      *dot = '\0';
  }

  {
    int sock_status = 0;
    int oerrno = 0;
    
    if (! socket_name)
      {
	socket_name = alloca (system_name_length + 100);
	sprintf (socket_name, "/tmp/emacs%d-%s/server",
		 (int) geteuid (), system_name);
      }

    if (strlen (socket_name) < sizeof (server.sun_path))
      strcpy (server.sun_path, socket_name);
    else
      {
        fprintf (stderr, "%s: socket-name %s too long",
                 argv[0], socket_name);
        fail ();
      }

    /* See if the socket exists, and if it's owned by us. */
    sock_status = socket_status (server.sun_path);
    oerrno = errno;
    if (sock_status)
      {
	/* Failing that, see if LOGNAME or USER exist and differ from
	   our euid.  If so, look for a socket based on the UID
	   associated with the name.  This is reminiscent of the logic
	   that init_editfns uses to set the global Vuser_full_name.  */

	char *user_name = (char *) getenv ("LOGNAME");
	if (!user_name)
	  user_name = (char *) getenv ("USER");

	if (user_name)
	  {
	    struct passwd *pw = getpwnam (user_name);
	    if (pw && (pw->pw_uid != geteuid ()))
	      {
		/* We're running under su, apparently. */
		sprintf (server.sun_path, "/tmp/esrv%d-%s",
			 (int) pw->pw_uid, system_name);
		sock_status = socket_status (server.sun_path);
                oerrno = errno;
	      }
	  }
      }

     switch (sock_status)
       {
       case 1:
	 /* There's a socket, but it isn't owned by us.  This is OK if
	    we are root. */
	 if (0 != geteuid ())
	   {
	     fprintf (stderr, "%s: Invalid socket owner\n", argv[0]);
	     fail ();
	   }
	 break;

       case 2:
	 /* `stat' failed */
	 if (errno == ENOENT)
	   fprintf (stderr,
		    "%s: Can't find socket; have you started the server?\n\
To start the server in Emacs, type \"M-x server-start\".\n",
		    argv[0]);
	 else
	   fprintf (stderr, "%s: Can't stat %s: %s\n",
		    argv[0], server.sun_path, strerror (oerrno));
	 fail ();
	 break;
       }
  }

  if (connect (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2)
      < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("connect");
      fail ();
    }

  /* We use the stream OUT to send our command to the server.  */
  if ((out = fdopen (s, "r+")) == NULL)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("fdopen");
      fail ();
    }

  /* We use the stream IN to read the response.
     We used to use just one stream for both output and input
     on the socket, but reversing direction works nonportably:
     on some systems, the output appears as the first input;
     on other systems it does not.  */
  if ((in = fdopen (s, "r+")) == NULL)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("fdopen");
      fail ();
    }

#ifdef HAVE_GETCWD
  cwd = getcwd (string, sizeof string);
#else
  cwd = getwd (string);
#endif
  if (cwd == 0)
    {
      /* getwd puts message in STRING if it fails.  */

#ifdef HAVE_GETCWD
      fprintf (stderr, "%s: %s (%s)\n", argv[0],
	       "Cannot get current working directory", strerror (errno));
#else
      fprintf (stderr, "%s: %s (%s)\n", argv[0], string, strerror (errno));
#endif
      fail ();
    }

  if (nowait)
    fprintf (out, "-nowait ");

  if (eval)
    fprintf (out, "-eval ");

  if (display)
    {
      fprintf (out, "-display ");
      quote_file_name (display, out);
      fprintf (out, " ");
    }

  if (frame)
    {
      if (! init_signals ())
        {
          fprintf (stderr, "%s: ", argv[0]);
          perror ("fdopen");
          fail ();
        }
        
      if (! init_tty ())
        {
          reset_tty ();
          fprintf (stderr, "%s: ", argv[0]);
          perror ("fdopen");
          fail ();
        }
      
      if (! init_pty ())
        {
          reset_tty ();
          fprintf (stderr, "%s: ", argv[0]);
          perror ("fdopen");
          fail ();
        }
      
      fprintf (out, "-tty ");
      quote_file_name (pty_name, out);
      fprintf (out, " ");
      quote_file_name (getenv("TERM"), out);
      fprintf (out, " ");
    }
  
  if ((argc - optind > 0))
    {
      for (i = optind; i < argc; i++)
	{
	  if (eval)
	    ; /* Don't prepend any cwd or anything like that.  */
	  else if (*argv[i] == '+')
	    {
	      char *p = argv[i] + 1;
	      while (isdigit ((unsigned char) *p) || *p == ':') p++;
	      if (*p != 0)
		{
		  quote_file_name (cwd, out);
		  fprintf (out, "/");
		}
	    }
	  else if (*argv[i] != '/')
	    {
	      quote_file_name (cwd, out);
	      fprintf (out, "/");
	    }

	  quote_file_name (argv[i], out);
	  fprintf (out, " ");
	}
    }
  else
    {
      if (!frame)
        {
          while ((str = fgets (string, BUFSIZ, stdin)))
            {
              quote_file_name (str, out);
            }
          fprintf (out, " ");
        }
    }
  
  fprintf (out, "\n");
  fflush (out);

  /* Maybe wait for an answer.   */
  if (nowait)
    {
      reset_tty ();
      return 0;
    }

  if (frame)
    {
      if (! pty_conversation (out))
        {
          reset_tty ();
          fail ();
        }
      reset_tty ();
      return 0;
    }
  
  if (!eval)
    {
      printf ("Waiting for Emacs...");
      needlf = 2;
    }
  fflush (stdout);

  /* Now, wait for an answer and print any messages.  */
  while ((str = fgets (string, BUFSIZ, in)))
    {
      if (needlf == 2)
	printf ("\n");
      printf ("%s", str);
      needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
    }

  if (needlf)
    printf ("\n");
  fflush (stdout);

  reset_tty ();
  return 0;
}

#endif /* HAVE_SOCKETS */

#ifndef HAVE_STRERROR
char *
strerror (errnum)
     int errnum;
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}

#endif /* ! HAVE_STRERROR */

/* arch-tag: f39bb9c4-73eb-477e-896d-50832e2ca9a7
   (do not change this comment) */
