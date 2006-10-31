/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1994, 1999, 2000, 2001, 2002, 2003, 2004,
                 2005, 2006 Free Software Foundation, Inc.

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

#ifdef WINDOWSNT
#define HAVE_SOCKETS
#define NO_SOCKETS_IN_FILE_SYSTEM
#endif

#ifdef WINDOWSNT
# define HSOCKET SOCKET
# define CLOSE_SOCKET closesocket
# define IOCTL ioctlsocket
# define INITIALIZE() (initialize_sockets ())
typedef unsigned long IOCTL_BOOL_ARG;
#else
# include <netinet/in.h>
# include <sys/ioctl.h>
# define INVALID_SOCKET -1
# define HSOCKET int
# define CLOSE_SOCKET close
# define IOCTL ioctl
# define INITIALIZE()
typedef int IOCTL_BOOL_ARG;
#endif

#undef signal

#include <ctype.h>
#include <stdio.h>
#include "getopt.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef VMS
# include "vms-pwd.h"
#else /* not VMS */
#ifdef WINDOWSNT
# include <io.h>
#else /* not WINDOWSNT */
# include <pwd.h>
#endif /* not WINDOWSNT */
#endif /* not VMS */

char *getenv (), *getwd ();
char *(getcwd) ();

#ifndef VERSION
#define VERSION "unspecified"
#endif

#define SEND_STRING(data) (send_to_emacs (s, (data)))
#define SEND_QUOTED(data) (quote_file_name (s, (data)))

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef NO_RETURN
#define NO_RETURN
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

/* If non-NULL, the filename of the authentication file.  */
char *server_file = NULL;

void print_help_and_exit () NO_RETURN;

struct option longopts[] =
{
  { "no-wait",	no_argument,	   NULL, 'n' },
  { "eval",	no_argument,	   NULL, 'e' },
  { "help",	no_argument,	   NULL, 'H' },
  { "version",	no_argument,	   NULL, 'V' },
  { "alternate-editor", required_argument, NULL, 'a' },
  { "socket-name",	required_argument, NULL, 's' },
  { "server-file",	required_argument, NULL, 'f' },
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
  server_file = getenv ("EMACS_SERVER_FILE");

  while (1)
    {
      int opt = getopt_long (argc, argv,
			     "VHnea:s:f:d:", longopts, 0);

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

	case 'f':
	  server_file = optarg;
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
-d, --display=DISPLAY   Visit the file in the given display\n"
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
"-s, --socket-name=FILENAME\n\
                        Set the filename of the UNIX socket for communication\n"
#endif
"-f, --server-file=FILENAME\n\
			Set the filename of the TCP configuration file\n\
-a, --alternate-editor=EDITOR\n\
                        Editor to fallback to if the server is not running\n\
\n\
Report bugs to bug-gnu-emacs@gnu.org.\n", progname);
  exit (EXIT_SUCCESS);
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
      fprintf (stderr, "%s: error executing alternate editor \"%s\"\n",
               progname, alternate_editor);
    }
  else
    {
      fprintf (stderr, "%s: No socket or alternate editor.  Please use:\n\n"
#if !defined (NO_SOCKETS_IN_FILE_SYSTEM)
"\t--socket-name\n"
#endif
"\t--server-file      (or environment variable EMACS_SERVER_FILE)\n\
\t--alternate-editor (or environment variable ALTERNATE_EDITOR)\n",
               progname);
    }
  exit (EXIT_FAILURE);
}


#if !defined (HAVE_SOCKETS)

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

#ifdef WINDOWSNT
# include <winsock2.h>
#else
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/un.h>
# include <sys/stat.h>
# include <errno.h>
#endif

#define AUTH_KEY_LENGTH      64
#define SEND_BUFFER_SIZE   4096

extern char *strerror ();
extern int errno;

/* Buffer to accumulate data to send in TCP connections.  */
char send_buffer[SEND_BUFFER_SIZE + 1];
int sblen = 0;	/* Fill pointer for the send buffer.  */

/* Let's send the data to Emacs when either
   - the data ends in "\n", or
   - the buffer is full (but this shouldn't happen)
   Otherwise, we just accumulate it.  */
void send_to_emacs (s, data)
     HSOCKET s;
     char *data;
{
  while (data)
    {
      int dlen = strlen (data);
      if (dlen + sblen >= SEND_BUFFER_SIZE)
	{
	  int part = SEND_BUFFER_SIZE - sblen;
	  strncpy (&send_buffer[sblen], data, part);
	  data += part;
	  sblen = SEND_BUFFER_SIZE;
	}
      else if (dlen)
	{
	  strcpy (&send_buffer[sblen], data);
	  data = NULL;
	  sblen += dlen;
	}
      else
	break;

      if (sblen == SEND_BUFFER_SIZE
	  || (sblen > 0 && send_buffer[sblen-1] == '\n'))
	{
	  int sent = send (s, send_buffer, sblen, 0);
	  if (sent != sblen)
	    strcpy (send_buffer, &send_buffer[sent]);
	  sblen -= sent;
	}
    }
}

/* In NAME, insert a & before each &, each space, each newline, and
   any initial -.  Change spaces to underscores, too, so that the
   return value never contains a space.  */
void
quote_file_name (s, name)
     HSOCKET s;
     char *name;
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

  SEND_STRING (copy);

  free (copy);
}

#ifdef WINDOWSNT
/* Wrapper to make WSACleanup a cdecl, as required by atexit().	 */
void close_winsock ()
{
  WSACleanup ();
}

void initialize_sockets ()
{
  WSADATA wsaData;

  /* Initialize the WinSock2 library.  */
  if (WSAStartup (MAKEWORD (2, 0), &wsaData))
    {
      fprintf (stderr, "%s: error initializing WinSock2", progname);
      exit (EXIT_FAILURE);
    }

  atexit (close_winsock);
}
#endif /* WINDOWSNT */

/*
 * Read the information needed to set up a TCP comm channel with
 * the Emacs server: host, port and authentication string.
*/
int get_server_config (server, authentication)
     struct sockaddr_in *server;
     char *authentication;
{
  FILE *config;
  char dotted[32];
  char *port;

  if (! (config = fopen (server_file, "rb")))
    {
      char *home = getenv ("HOME");
#ifdef WINDOWSNT
      if (! home)
          home = getenv ("APPDATA");
#endif
      if (home)
        {
          char *path = alloca (32 + strlen (home) + strlen (server_file));
          sprintf (path, "%s/.emacs.d/server/%s", home, server_file);
          config = fopen (path, "rb");
        }
    }

  if (! config)
    return FALSE;

  if (fgets (dotted, sizeof dotted, config)
      && (port = strchr (dotted, ':')))
    {
      *port++ = '\0';
    }
  else
    {
      fprintf (stderr, "%s: invalid configuration info", progname);
      exit (EXIT_FAILURE);
    }

  server->sin_family = AF_INET;
  server->sin_addr.s_addr = inet_addr (dotted);
  server->sin_port = htons (atoi (port));

  if (! fread (authentication, AUTH_KEY_LENGTH, 1, config))
    {
      fprintf (stderr, "%s: cannot read authentication info", progname);
      exit (EXIT_FAILURE);
    }

  fclose (config);

  return TRUE;
}

HSOCKET
set_tcp_socket ()
{
  HSOCKET s;
  struct sockaddr_in server;
  IOCTL_BOOL_ARG c_arg = 0;
  struct linger l_arg = {1, 1};
  char auth_string[AUTH_KEY_LENGTH + 1];

  INITIALIZE ();

  if (! get_server_config (&server, auth_string))
    return INVALID_SOCKET;

  /*
   * Open up an AF_INET socket
   */
  if ((s = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
    {
      fprintf (stderr, "%s: ", progname);
      perror ("socket");
      return INVALID_SOCKET;
    }

  /*
   * Set up the socket
   */
  if (connect (s, (struct sockaddr *) &server, sizeof server) < 0)
    {
      fprintf (stderr, "%s: ", progname);
      perror ("connect");
      return INVALID_SOCKET;
    }

  IOCTL (s, FIONBIO, &c_arg);
  setsockopt (s, SOL_SOCKET, SO_LINGER, (char *) &l_arg, sizeof l_arg);

  /*
   * Send the authentication
   */
  auth_string[AUTH_KEY_LENGTH] = '\0';

  SEND_STRING ("-auth ");
  SEND_STRING (auth_string);
  SEND_STRING ("\n");

  return s;
}

#if !defined (NO_SOCKETS_IN_FILE_SYSTEM)

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

HSOCKET
set_local_socket ()
{
  HSOCKET s;
  struct sockaddr_un server;

  /*
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      fprintf (stderr, "%s: ", progname);
      perror ("socket");
      return INVALID_SOCKET;
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
		 progname, socket_name);
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
			     progname, socket_name);
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
            fprintf (stderr, "%s: Invalid socket owner\n", progname);
	    return INVALID_SOCKET;
          }
        break;

      case 2:
        /* `stat' failed */
        if (saved_errno == ENOENT)
          fprintf (stderr,
                   "%s: can't find socket; have you started the server?\n\
To start the server in Emacs, type \"M-x server-start\".\n",
		   progname);
        else
          fprintf (stderr, "%s: can't stat %s: %s\n",
		   progname, server.sun_path, strerror (saved_errno));
	return INVALID_SOCKET;
      }
  }

  if (connect (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2)
      < 0)
    {
      fprintf (stderr, "%s: ", progname);
      perror ("connect");
      return INVALID_SOCKET;
    }

  return s;
}
#endif /* ! NO_SOCKETS_IN_FILE_SYSTEM */

HSOCKET
set_socket ()
{
  if (server_file)
    return set_tcp_socket ();
  else
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
    return set_local_socket ();
#else
    {
      server_file = "server";
      return set_tcp_socket ();
    }
#endif
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  HSOCKET s;
  int i, rl, needlf = 0;
  char *cwd;
  char string[BUFSIZ+1];

  progname = argv[0];

  /* Process options.  */
  decode_options (argc, argv);

  if ((argc - optind < 1) && !eval)
    {
      fprintf (stderr, "%s: file name or argument required\n", progname);
      fprintf (stderr, "Try `%s --help' for more information\n", progname);
      exit (EXIT_FAILURE);
    }

  if ((s = set_socket ()) == INVALID_SOCKET)
    fail (argc, argv);

#ifdef HAVE_GETCWD
  cwd = getcwd (string, sizeof string);
#else
  cwd = getwd (string);
#endif
  if (cwd == 0)
    {
      /* getwd puts message in STRING if it fails.  */
#ifdef HAVE_GETCWD
      fprintf (stderr, "%s: %s (%s)\n", progname,
	       "Cannot get current working directory", strerror (errno));
#else
      fprintf (stderr, "%s: %s (%s)\n", progname, string, strerror (errno));
#endif
      fail (argc, argv);
    }

  if (nowait)
    SEND_STRING ("-nowait ");

  if (eval)
    SEND_STRING ("-eval ");

  if (display)
    {
      SEND_STRING ("-display ");
      SEND_QUOTED (display);
      SEND_STRING (" ");
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
		  SEND_QUOTED (cwd);
		  SEND_STRING ("/");
		}
	    }
#ifndef WINDOWSNT
	  else if (*argv[i] != '/')
#else
	  else if ((*argv[i] != '/')
		   /* Absolute paths can also start with backslash
		      or drive letters.	 */
		   && (*argv[i] != '\\')
		   && (!islower (tolower (*argv[i]))
		       || (argv[i][1] != ':')))
#endif
	    {
	      SEND_QUOTED (cwd);
	      SEND_STRING ("/");
	    }

	  SEND_QUOTED (argv[i]);
	  SEND_STRING (" ");
	}
    }
  else
    {
      while (fgets (string, BUFSIZ, stdin))
	{
	  SEND_QUOTED (string);
	}
      SEND_STRING (" ");
    }

  SEND_STRING ("\n");

  /* Maybe wait for an answer.   */
  if (!nowait)
    {
      if (!eval)
        {
          printf ("Waiting for Emacs...");
          needlf = 2;
        }
      fflush (stdout);

      /* Now, wait for an answer and print any messages.  */
      while ((rl = recv (s, string, BUFSIZ, 0)) > 0)
        {
	  string[rl] = '\0';
          if (needlf == 2)
            printf ("\n");
	  printf ("%s", string);
	  needlf = string[0] == '\0' ? needlf : string[strlen (string) - 1] != '\n';
        }

      if (needlf)
        printf ("\n");
      fflush (stdout);
    }

  CLOSE_SOCKET (s);
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
