/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1994, 1999, 2000, 2001, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


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

#include <signal.h>
#include <errno.h>


char *getenv (), *getwd ();
char *(getcwd) ();

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

/* Nonzero means don't open a new frame.  --current-frame.  */
int current_frame = 0;

/* Nonzero means open a new graphical frame. */
int window_system = 0;

/* The display on which Emacs should work.  --display.  */
char *display = NULL;

/* Nonzero means open a new Emacs frame on the current terminal. */
int tty = 0;

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
  { "tty",	no_argument,       NULL, 't' },
  { "current-frame", no_argument,  NULL, 'c' },
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
  alternate_editor = getenv ("ALTERNATE_EDITOR");
  display = getenv ("DISPLAY");
  if (display && strlen (display) == 0)
    display = NULL;

  while (1)
    {
      int opt = getopt_long (argc, argv,
			     "VHnea:s:d:tc", longopts, 0);

      if (opt == EOF)
	break;

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
	  exit (EXIT_SUCCESS);
	  break;

        case 't':
          tty = 1;
          break;

        case 'c':
          current_frame = 1;
          break;

	case 'H':
	  print_help_and_exit ();
	  break;

	default:
	  fprintf (stderr, "Try `%s --help' for more information\n", progname);
	  exit (EXIT_FAILURE);
	  break;
	}
    }

  if (!tty && display)
    window_system = 1;
  else
    tty = 1;

  /* --no-wait implies --current-frame on ttys when there are file
       arguments or expressions given.  */
  if (nowait && tty && argc - optind > 0)
    current_frame = 1;

  if (current_frame)
    {
      tty = 0;
      window_system = 0;
    }

  if (tty)
    window_system = 0;
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
-c, --current-frame	Do not create a new frame; use the current Emacs frame\n\
-n, --no-wait           Don't wait for the server to return\n\
-e, --eval              Evaluate the FILE arguments as ELisp expressions\n\
-d, --display=DISPLAY   Visit the file in the given display\n\
-s, --socket-name=FILENAME\n\
                        Set the filename of the UNIX socket for communication\n\
-a, --alternate-editor=EDITOR\n\
                        Editor to fallback to if the server is not running\n\
\n\
Report bugs to bug-gnu-emacs@gnu.org.\n", progname);
  exit (EXIT_SUCCESS);
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
      exit (EXIT_FAILURE);
    }
  return result;
}

/* Like strdup but get a fatal error if memory is exhausted. */

char *
xstrdup (const char *s)
{
  char *result = strdup (s);
  if (result == NULL)
    {
      perror ("strdup");
      exit (EXIT_FAILURE);
    }
  return result;
}

/* In STR, insert a & before each &, each space, each newline, and
   any initial -.  Change spaces to underscores, too, so that the
   return value never contains a space.

   Does not change the string.  Outputs the result to STREAM.  */

void
quote_argument (str, stream)
     char *str;
     FILE *stream;
{
  char *copy = (char *) xmalloc (strlen (str) * 2 + 1);
  char *p, *q;

  p = str;
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
	  if (*p == '&' || (*p == '-' && p == str))
	    *q++ = '&';
	  *q++ = *p++;
	}
    }
  *q++ = 0;

  fprintf (stream, "%s", copy);

  free (copy);
}


/* The inverse of quote_argument.  Removes quoting in string STR by
   modifying the string in place.   Returns STR. */

char *
unquote_argument (str)
     char *str;
{
  char *p, *q;

  if (! str)
    return str;

  p = str;
  q = str;
  while (*p)
    {
      if (*p == '&')
        {
          p++;
          if (*p == '&')
            *p = '&';
          else if (*p == '_')
            *p = ' ';
          else if (*p == 'n')
            *p = '\n';
          else if (*p == '-')
            *p = '-';
        }
      *q++ = *p++;
    }
  *q = 0;
  return str;
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
      exit (EXIT_FAILURE);
    }
}

/* The process id of Emacs. */
int emacs_pid;

/* File handles for communicating with Emacs. */
FILE *out, *in;

/* A signal handler that passes the signal to the Emacs process.
   Useful for SIGWINCH.  */

SIGTYPE
pass_signal_to_emacs (int signalnum)
{
  int old_errno = errno;

  if (emacs_pid)
    kill (emacs_pid, signalnum);

  signal (signalnum, pass_signal_to_emacs);
  errno = old_errno;
}

/* Signal handler for SIGCONT; notify the Emacs process that it can
   now resume our tty frame.  */

SIGTYPE
handle_sigcont (int signalnum)
{
  int old_errno = errno;

  if (tcgetpgrp (1) == getpgrp ())
    {
      /* We are in the foreground. */
      fprintf (out, "-resume \n");
      fflush (out);
      fsync (fileno (out));
    }
  else
    {
      /* We are in the background; cancel the continue. */
      kill (getpid (), SIGSTOP);
    }

  signal (signalnum, handle_sigcont);
  errno = old_errno;
}

/* Signal handler for SIGTSTP; notify the Emacs process that we are
   going to sleep.  Normally the suspend is initiated by Emacs via
   server-handle-suspend-tty, but if the server gets out of sync with
   reality, we may get a SIGTSTP on C-z.  Handling this signal and
   notifying Emacs about it should get things under control again. */

SIGTYPE
handle_sigtstp (int signalnum)
{
  int old_errno = errno;
  sigset_t set;
  
  if (out)
    {
      fprintf (out, "-suspend \n");
      fflush (out);
      fsync (fileno (out));
    }

  /* Unblock this signal and call the default handler by temprarily
     changing the handler and resignalling. */
  sigprocmask (SIG_BLOCK, NULL, &set);
  sigdelset (&set, signalnum);
  signal (signalnum, SIG_DFL);
  kill (getpid (), signalnum);
  sigprocmask (SIG_SETMASK, &set, NULL); /* Let's the above signal through. */
  signal (signalnum, handle_sigtstp);

  errno = old_errno;
}

/* Set up signal handlers before opening a frame on the current tty.  */

void
init_signals (void)
{
  /* Set up signal handlers. */
  signal (SIGWINCH, pass_signal_to_emacs);

  /* Don't pass SIGINT and SIGQUIT to Emacs, because it has no way of
     deciding which terminal the signal came from.  C-g is now a
     normal input event on secondary terminals.  */
#if 0
  signal (SIGINT, pass_signal_to_emacs);
  signal (SIGQUIT, pass_signal_to_emacs);
#endif

  signal (SIGCONT, handle_sigcont);
  signal (SIGTSTP, handle_sigtstp);
  signal (SIGTTOU, handle_sigtstp);
}


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

/* Returns 1 if PREFIX is a prefix of STRING. */
static int
strprefix (char *prefix, char *string)
{
  int i;
  if (! prefix)
    return 1;

  if (!string)
    return 0;

  for (i = 0; prefix[i]; i++)
    if (!string[i] || string[i] != prefix[i])
      return 0;
  return 1;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int s, i, needlf = 0;
  struct sockaddr_un server;
  char *cwd, *str;
  char string[BUFSIZ];

  main_argc = argc;
  main_argv = argv;
  progname = argv[0];

  /* Process options.  */
  decode_options (argc, argv);

  if ((argc - optind < 1) && !eval && !tty && !window_system)
    {
      fprintf (stderr, "%s: file name or argument required\n", progname);
      fprintf (stderr, "Try `%s --help' for more information\n", progname);
      exit (EXIT_FAILURE);
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
    int sock_status = 0;
    int default_sock = !socket_name;
    int saved_errno = 0;

     char *server_name = "server";

     if (socket_name && !index (socket_name, '/') && !index (socket_name, '\\'))
       { /* socket_name is a file name component.  */
 	server_name = socket_name;
 	socket_name = NULL;
 	default_sock = 1;	/* Try both UIDs.  */
       }

     if (default_sock)
      {
 	socket_name = alloca (100 + strlen (server_name));
 	sprintf (socket_name, "/tmp/emacs%d/%s",
 		 (int) geteuid (), server_name);
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
    saved_errno = errno;
    if (sock_status && default_sock)
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
		socket_name = alloca (100 + strlen (server_name));
		sprintf (socket_name, "/tmp/emacs%d/%s",
			 (int) pw->pw_uid, server_name);

		if (strlen (socket_name) < sizeof (server.sun_path))
		  strcpy (server.sun_path, socket_name);
		else
		  {
		    fprintf (stderr, "%s: socket-name %s too long",
			     argv[0], socket_name);
		    exit (EXIT_FAILURE);
		  }

		sock_status = socket_status (server.sun_path);
                saved_errno = errno;
	      }
	    else
	      errno = saved_errno;
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
	 if (saved_errno == ENOENT)
	   fprintf (stderr,
		    "%s: can't find socket; have you started the server?\n\
To start the server in Emacs, type \"M-x server-start\".\n",
		    argv[0]);
	 else
	   fprintf (stderr, "%s: can't stat %s: %s\n",
		    argv[0], server.sun_path, strerror (saved_errno));
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

  /* We use the stream OUT to send our commands to the server.  */
  if ((out = fdopen (s, "r+")) == NULL)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("fdopen");
      fail ();
    }

  /* We use the stream IN to read the responses.
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
	       "cannot get current working directory", strerror (errno));
#else
      fprintf (stderr, "%s: %s (%s)\n", argv[0], string, strerror (errno));
#endif
      fail ();
    }

  /* First of all, send our version number for verification. */
  fprintf (out, "-version %s ", VERSION);

  /* Send over our environment. */
  {
    extern char **environ;
    int i;
    for (i = 0; environ[i]; i++)
      {
        char *name = xstrdup (environ[i]);
        char *value = strchr (name, '=');
        if (value && strlen (value) > 1)
          {
            *value++ = 0;
            fprintf (out, "-env ");
            quote_argument (name, out);
            fprintf (out, " ");
            quote_argument (value, out);
            fprintf (out, " ");
            fflush (out);
          }
        free (name);
      }
  }

 retry:
  if (nowait)
    fprintf (out, "-nowait ");

  if (current_frame)
    fprintf (out, "-current-frame ");
  
  if (display)
    {
      fprintf (out, "-display ");
      quote_argument (display, out);
      fprintf (out, " ");
    }

  if (tty)
    {
      char *tty_name = ttyname (fileno (stdin));
      char *type = getenv ("TERM");

      if (! tty_name)
        {
          fprintf (stderr, "%s: could not get terminal name\n", progname);
          fail ();
        }

      if (! type)
        {
          fprintf (stderr, "%s: please set the TERM variable to your terminal type\n",
                   progname);
          fail ();
        }

      if (! strcmp (type, "eterm"))
        {
          /* This causes nasty, MULTI_KBOARD-related input lockouts. */
          fprintf (stderr, "%s: opening a frame in an Emacs term buffer"
                   " is not supported\n", progname);
          fail ();
        }

      init_signals ();

      fprintf (out, "-tty ");
      quote_argument (tty_name, out);
      fprintf (out, " ");
      quote_argument (type, out);
      fprintf (out, " ");
    }

  if (window_system)
    fprintf (out, "-window-system ");

  if ((argc - optind > 0))
    {
      for (i = optind; i < argc; i++)
	{
          int relative = 0;

	  if (eval)
            {
              /* Don't prepend any cwd or anything like that.  */
              fprintf (out, "-eval ");
              quote_argument (argv[i], out);
              fprintf (out, " ");
              continue;
            }

          if (*argv[i] == '+')
            {
	      char *p = argv[i] + 1;
	      while (isdigit ((unsigned char) *p) || *p == ':') p++;
	      if (*p == 0)
                {
                  fprintf (out, "-position ");
                  quote_argument (argv[i], out);
                  fprintf (out, " ");
                  continue;
                }
              else
                relative = 1;
            }
          else if (*argv[i] != '/')
            relative = 1;

          fprintf (out, "-file ");
          if (relative)
            {
              quote_argument (cwd, out);
              fprintf (out, "/");
            }
          quote_argument (argv[i], out);
          fprintf (out, " ");
        }
    }
  else
    {
      if (!tty && !window_system)
        {
          while ((str = fgets (string, BUFSIZ, stdin)))
            {
              if (eval)
                fprintf (out, "-eval ");
              else
                fprintf (out, "-file ");
              quote_argument (str, out);
            }
          fprintf (out, " ");
        }
    }

  fprintf (out, "\n");
  fflush (out);
  fsync (fileno (out));

  /* Wait for an answer. */
  if (!eval && !tty && !nowait)
    {
      printf ("Waiting for Emacs...");
      needlf = 2;
    }
  fflush (stdout);
  fsync (1);

  /* Now, wait for an answer and print any messages.  */
  while ((str = fgets (string, BUFSIZ, in)))
    {
      char *p = str + strlen (str) - 1;
      while (p > str && *p == '\n')
        *p-- = 0;

      if (strprefix ("-good-version ", str))
        {
          /* -good-version: The versions match. */
        }
      else if (strprefix ("-emacs-pid ", str))
        {
          /* -emacs-pid PID: The process id of the Emacs process. */
          emacs_pid = strtol (string + strlen ("-emacs-pid"), NULL, 10);
        }
      else if (strprefix ("-window-system-unsupported ", str))
        {
          /* -window-system-unsupported: Emacs was compiled without X
              support.  Try again on the terminal. */
          window_system = 0;
          nowait = 0;
          tty = 1;
          goto retry;
        }
      else if (strprefix ("-print ", str))
        {
          /* -print STRING: Print STRING on the terminal. */
          str = unquote_argument (str + strlen ("-print "));
          if (needlf)
            printf ("\n");
          printf ("%s", str);
          needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
        }
      else if (strprefix ("-error ", str))
        {
          /* -error DESCRIPTION: Signal an error on the terminal. */
          str = unquote_argument (str + strlen ("-error "));
          if (needlf)
            printf ("\n");
          printf ("*ERROR*: %s", str);
          needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
        }
      else if (strprefix ("-suspend ", str))
        {
          /* -suspend: Suspend this terminal, i.e., stop the process. */
          if (needlf)
            printf ("\n");
          needlf = 0;
          kill (0, SIGSTOP);
        }
      else
        {
          /* Unknown command. */
          if (needlf)
            printf ("\n");
          printf ("*ERROR*: Unknown message: %s", str);
          needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
        }
    }

  if (needlf)
    printf ("\n");
  fflush (stdout);
  fsync (1);

  return EXIT_SUCCESS;
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

/* emacsclient.c ends here */
