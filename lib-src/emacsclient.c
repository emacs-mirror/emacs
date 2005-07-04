/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1994, 1999, 2000, 2001, 2003, 2004
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

char *getenv (), *getwd ();
char *(getcwd) ();

#ifndef VERSION
#define VERSION "unspecified"
#endif

/* Name used to invoke this program.  */
char *progname;

/* Nonzero means don't wait for a response from Emacs.  --no-wait.  */
int nowait = 0;

/* Nonzero means args are expressions to be evaluated.  --eval.  */
int eval = 0;

/* The display on which Emacs should work.  --display.  */
char *display = NULL;

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

  while (1)
    {
      int opt = getopt_long (argc, argv,
			     "VHnea:s:d:", longopts, 0);

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

	case 'H':
	  print_help_and_exit ();
	  break;

	default:
	  fprintf (stderr, "Try `%s --help' for more information\n", progname);
	  exit (EXIT_FAILURE);
	  break;
	}
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

  fprintf (stream, "%s", copy);

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
    exit (EXIT_FAILURE);
  }
  return result;
}

/*
  Try to run a different command, or --if no alternate editor is
  defined-- exit with an errorcode.
*/
void
fail (argc, argv)
     int argc;
     char **argv;
{
  if (alternate_editor)
    {
      int i = optind - 1;
      execvp (alternate_editor, argv + i);
      return;
    }
  else
    {
      exit (EXIT_FAILURE);
    }
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

  fail (argc, argv);
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
  int s, i, needlf = 0;
  FILE *out, *in;
  struct sockaddr_un server;
  char *cwd, *str;
  char string[BUFSIZ];

  progname = argv[0];

  /* Process options.  */
  decode_options (argc, argv);

  if ((argc - optind < 1) && !eval)
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
      fail (argc, argv);
    }

  server.sun_family = AF_UNIX;

  {
    int sock_status = 0;
    int default_sock = !socket_name;
    int saved_errno;
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
	exit (EXIT_FAILURE);
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
	     fail (argc, argv);
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
	 fail (argc, argv);
	 break;
       }
  }

  if (connect (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2)
      < 0)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("connect");
      fail (argc, argv);
    }

  /* We use the stream OUT to send our command to the server.  */
  if ((out = fdopen (s, "r+")) == NULL)
    {
      fprintf (stderr, "%s: ", argv[0]);
      perror ("fdopen");
      fail (argc, argv);
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
      fail (argc, argv);
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
      fail (argc, argv);
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
      while ((str = fgets (string, BUFSIZ, stdin)))
	{
	  quote_file_name (str, out);
	}
      fprintf (out, " ");
    }

  fprintf (out, "\n");
  fflush (out);

  /* Maybe wait for an answer.   */
  if (nowait)
    return EXIT_SUCCESS;

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
