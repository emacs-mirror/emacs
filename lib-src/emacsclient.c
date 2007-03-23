/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986, 1987, 1994, 1999, 2000, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007 Free Software Foundation, Inc.

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

/* config.h defines these, which disables sockets altogether! */
# undef _WINSOCKAPI_
# undef _WINSOCK_H

# include <malloc.h>
# include <stdlib.h>
# include <windows.h>

# define NO_SOCKETS_IN_FILE_SYSTEM

# define HSOCKET SOCKET
# define CLOSE_SOCKET closesocket
# define INITIALIZE() (initialize_sockets ())

#else /* !WINDOWSNT */

# include <sys/types.h>

# ifdef HAVE_INET_SOCKETS
#  include <netinet/in.h>
# endif

# define INVALID_SOCKET -1
# define HSOCKET int
# define CLOSE_SOCKET close
# define INITIALIZE()

#endif /* !WINDOWSNT */

#undef signal

#include <stdarg.h>
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
const char *alternate_editor = NULL;

/* If non-NULL, the filename of the UNIX socket.  */
char *socket_name = NULL;

/* If non-NULL, the filename of the authentication file.  */
char *server_file = NULL;

/* PID of the Emacs server process.  */
int emacs_pid = 0;

void print_help_and_exit () NO_RETURN;

struct option longopts[] =
{
  { "no-wait",	no_argument,	   NULL, 'n' },
  { "eval",	no_argument,	   NULL, 'e' },
  { "help",	no_argument,	   NULL, 'H' },
  { "version",	no_argument,	   NULL, 'V' },
  { "alternate-editor", required_argument, NULL, 'a' },
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
  { "socket-name",	required_argument, NULL, 's' },
#endif
  { "server-file",	required_argument, NULL, 'f' },
  { "display",	required_argument, NULL, 'd' },
  { 0, 0, 0, 0 }
};

/* Message functions. */

#ifdef WINDOWSNT
int
w32_window_app ()
{
  static int window_app = -1;
  char szTitle[MAX_PATH];

  if (window_app < 0)
    /* Checking for STDOUT does not work; it's a valid handle also in
       nonconsole apps.  Testing for the console title seems to work. */
    window_app = (GetConsoleTitleA (szTitle, MAX_PATH) == 0);

  return window_app;
}
#endif

void
message (int is_error, char *message, ...)
{
  char msg [2048];
  va_list args;

  va_start (args, message);
  vsprintf (msg, message, args);
  va_end (args);

#ifdef WINDOWSNT
  if (w32_window_app ())
    {
      if (is_error)
	MessageBox (NULL, msg, "Emacsclient ERROR", MB_ICONERROR);
      else
	MessageBox (NULL, msg, "Emacsclient", MB_ICONINFORMATION);
    }
  else
#endif
    {
      FILE *f = is_error ? stderr : stdout;

      fputs (msg, f);
      fflush (f);
    }
}

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
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
			     "VHnea:s:f:d:",
#else
                             "VHnea:f:d:",
#endif
                             longopts, 0);

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

#ifndef NO_SOCKETS_IN_FILE_SYSTEM
	case 's':
	  socket_name = optarg;
	  break;
#endif

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
	  message (FALSE, "emacsclient %s\n", VERSION);
	  exit (EXIT_SUCCESS);
	  break;

	case 'H':
	  print_help_and_exit ();
	  break;

	default:
	  message (TRUE, "Try `%s --help' for more information\n", progname);
	  exit (EXIT_FAILURE);
	  break;
	}
    }
}

void
print_help_and_exit ()
{
  message (FALSE,
	  "Usage: %s [OPTIONS] FILE...\n\
Tell the Emacs server to visit the specified files.\n\
Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.\n\
\n\
The following OPTIONS are accepted:\n\
\n\
-V, --version		Just print version info and return\n\
-H, --help   		Print this usage information message\n\
-e, --eval   		Evaluate FILE arguments as Lisp expressions\n\
-n, --no-wait		Don't wait for the server to return\n\
-d, --display=DISPLAY	Visit the file in the given display\n"
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
"-s, --socket-name=FILENAME\n\
			Set filename of the UNIX socket for communication\n"
#endif
"-f, --server-file=FILENAME\n\
			Set filename of the TCP authentication file\n\
-a, --alternate-editor=EDITOR\n\
			Editor to fallback to if server is not running\n\
\n\
Report bugs to bug-gnu-emacs@gnu.org.\n", progname);
  exit (EXIT_SUCCESS);
}


#ifdef WINDOWSNT

/*
  execvp wrapper for Windows. Quotes arguments with embedded spaces.

  This is necessary due to the broken implementation of exec* routines in
  the Microsoft libraries: they concatenate the arguments together without
  quoting special characters, and pass the result to CreateProcess, with
  predictably bad results.  By contrast, Posix execvp passes the arguments
  directly into the argv array of the child process.
*/
int
w32_execvp (path, argv)
     char *path;
     char **argv;
{
  int i;

  /* Required to allow a .BAT script as alternate editor.  */
  argv[0] = (char *) alternate_editor;

  for (i = 0; argv[i]; i++)
    if (strchr (argv[i], ' '))
      {
	char *quoted = alloca (strlen (argv[i]) + 3);
	sprintf (quoted, "\"%s\"", argv[i]);
	argv[i] = quoted;
      }

  return execvp (path, argv);
}

#undef execvp
#define execvp w32_execvp

#endif /* WINDOWSNT */

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
      message (TRUE, "%s: error executing alternate editor \"%s\"\n",
               progname, alternate_editor);
    }
  exit (EXIT_FAILURE);
}


#if !defined (HAVE_SOCKETS) || !defined (HAVE_INET_SOCKETS)

int
main (argc, argv)
     int argc;
     char **argv;
{
  message (TRUE, "%s: Sorry, the Emacs server is supported only\non systems with Berkely sockets.\n",
	   argv[0]);

  fail (argc, argv);
}

#else /* HAVE_SOCKETS && HAVE_INET_SOCKETS */

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
void
send_to_emacs (s, data)
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

int
file_name_absolute_p (filename)
     const unsigned char *filename;
{
  /* Sanity check, it shouldn't happen.  */
  if (! filename) return FALSE;

  /* /xxx is always an absolute path.  */
  if (filename[0] == '/') return TRUE;

  /* Empty filenames (which shouldn't happen) are relative.  */
  if (filename[0] == '\0') return FALSE;

#ifdef WINDOWSNT
  /* X:\xxx is always absolute.  */
  if (isalpha (filename[0])
      && filename[1] == ':' && (filename[2] == '\\' || filename[2] == '/'))
    return TRUE;

  /* Both \xxx and \\xxx\yyy are absolute.  */
  if (filename[0] == '\\') return TRUE;

  /*
    FIXME:  There's a corner case not dealt with, "x:y", where:

    1) x is a valid drive designation (usually a letter in the A-Z range)
       and y is a path, relative to the current directory on drive x.  This
       is absolute, *after* fixing the y part to include the current
       directory in x.

    2) x is a relative file name, and y is an NTFS stream name.  This is a
       correct relative path, but it is very unusual.

    The trouble is that first case items are also valid examples of the
    second case, i.e., "c:test" can be understood as drive:path or as
    file:stream.

    The "right" fix would involve checking whether
    - the current drive/partition is NTFS,
    - x is a valid (and accesible) drive designator,
    - x:y already exists as a file:stream in the current directory,
    - y already exists on the current directory of drive x,
    - the auspices are favorable,
    and then taking an "informed decision" based on the above.

    Whatever the result, Emacs currently does a very bad job of dealing
    with NTFS file:streams: it cannot visit them, and the only way to
    create one is by setting `buffer-file-name' to point to it (either
    manually or with emacsclient). So perhaps resorting to 1) and ignoring
    2) for now is the right thing to do.

    Anyway, something to decide After the Release.
  */
#endif

  return FALSE;
}

#ifdef WINDOWSNT
/* Wrapper to make WSACleanup a cdecl, as required by atexit.  */
void
__cdecl close_winsock ()
{
  WSACleanup ();
}

/* Initialize the WinSock2 library.  */
void
initialize_sockets ()
{
  WSADATA wsaData;

  if (WSAStartup (MAKEWORD (2, 0), &wsaData))
    {
      message (TRUE, "%s: error initializing WinSock2", progname);
      exit (EXIT_FAILURE);
    }

  atexit (close_winsock);
}
#endif /* WINDOWSNT */

/*
 * Read the information needed to set up a TCP comm channel with
 * the Emacs server: host, port, pid and authentication string.
 */
int
get_server_config (server, authentication)
     struct sockaddr_in *server;
     char *authentication;
{
  char dotted[32];
  char *port;
  char *pid;
  FILE *config = NULL;

  if (file_name_absolute_p (server_file))
    config = fopen (server_file, "rb");
  else
    {
      char *home = getenv ("HOME");

      if (home)
        {
          char *path = alloca (32 + strlen (home) + strlen (server_file));
          sprintf (path, "%s/.emacs.d/server/%s", home, server_file);
          config = fopen (path, "rb");
        }
#ifdef WINDOWSNT
      if (!config && (home = getenv ("APPDATA")))
        {
          char *path = alloca (32 + strlen (home) + strlen (server_file));
          sprintf (path, "%s/.emacs.d/server/%s", home, server_file);
          config = fopen (path, "rb");
        }
#endif
    }

  if (! config)
    return FALSE;

  if (fgets (dotted, sizeof dotted, config)
      && (port = strchr (dotted, ':'))
      && (pid = strchr (port, ' ')))
    {
      *port++ = '\0';
      *pid++  = '\0';
    }
  else
    {
      message (TRUE, "%s: invalid configuration info", progname);
      exit (EXIT_FAILURE);
    }

  server->sin_family = AF_INET;
  server->sin_addr.s_addr = inet_addr (dotted);
  server->sin_port = htons (atoi (port));

  if (! fread (authentication, AUTH_KEY_LENGTH, 1, config))
    {
      message (TRUE, "%s: cannot read authentication info", progname);
      exit (EXIT_FAILURE);
    }

  fclose (config);

  emacs_pid = atoi (pid);

  return TRUE;
}

HSOCKET
set_tcp_socket ()
{
  HSOCKET s;
  struct sockaddr_in server;
  struct linger l_arg = {1, 1};
  char auth_string[AUTH_KEY_LENGTH + 1];

  if (! get_server_config (&server, auth_string))
    return INVALID_SOCKET;

  if (server.sin_addr.s_addr != inet_addr ("127.0.0.1"))
    message (FALSE, "%s: connected to remote socket at %s\n",
             progname, inet_ntoa (server.sin_addr));

  /*
   * Open up an AF_INET socket
   */
  if ((s = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
    {
      message (TRUE, "%s: socket: %s\n", progname, strerror (errno));
      return INVALID_SOCKET;
    }

  /*
   * Set up the socket
   */
  if (connect (s, (struct sockaddr *) &server, sizeof server) < 0)
    {
      message (TRUE, "%s: connect: %s\n", progname, strerror (errno));
      return INVALID_SOCKET;
    }

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
      message (TRUE, "%s: socket: %s\n", progname, strerror (errno));
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
	message (TRUE, "%s: socket-name %s too long",
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
		    message (TRUE, "%s: socket-name %s too long",
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
            message (TRUE, "%s: Invalid socket owner\n", progname);
	    return INVALID_SOCKET;
          }
        break;

      case 2:
        /* `stat' failed */
        if (saved_errno == ENOENT)
          message (TRUE,
                   "%s: can't find socket; have you started the server?\n\
To start the server in Emacs, type \"M-x server-start\".\n",
		   progname);
        else
          message (TRUE, "%s: can't stat %s: %s\n",
		   progname, server.sun_path, strerror (saved_errno));
        return INVALID_SOCKET;
      }
  }

  if (connect (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2)
      < 0)
    {
      message (TRUE, "%s: connect: %s\n", progname, strerror (errno));
      return INVALID_SOCKET;
    }

  return s;
}
#endif /* ! NO_SOCKETS_IN_FILE_SYSTEM */

HSOCKET
set_socket ()
{
  HSOCKET s;

  INITIALIZE ();

#ifndef NO_SOCKETS_IN_FILE_SYSTEM
  /* Explicit --socket-name argument.  */
  if (socket_name)
    {
      s = set_local_socket ();
      if ((s != INVALID_SOCKET) || alternate_editor)
        return s;

      message (TRUE, "%s: error accessing socket \"%s\"",
               progname, socket_name);
      exit (EXIT_FAILURE);
    }
#endif

  /* Explicit --server-file arg or EMACS_SERVER_FILE variable.  */
  if (!server_file)
    server_file = getenv ("EMACS_SERVER_FILE");

  if (server_file)
    {
      s = set_tcp_socket ();
      if ((s != INVALID_SOCKET) || alternate_editor)
        return s;

      message (TRUE, "%s: error accessing server file \"%s\"",
               progname, server_file);
      exit (EXIT_FAILURE);
    }

#ifndef NO_SOCKETS_IN_FILE_SYSTEM
  /* Implicit local socket.  */
  s = set_local_socket ();
  if (s != INVALID_SOCKET)
    return s;
#endif

  /* Implicit server file.  */
  server_file = "server";
  s = set_tcp_socket ();
  if ((s != INVALID_SOCKET) || alternate_editor)
    return s;

  /* No implicit or explicit socket, and no alternate editor.  */
  message (TRUE, "%s: No socket or alternate editor.  Please use:\n\n"
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
"\t--socket-name\n"
#endif
"\t--server-file      (or environment variable EMACS_SERVER_FILE)\n\
\t--alternate-editor (or environment variable ALTERNATE_EDITOR)\n",
           progname);
  exit (EXIT_FAILURE);
}

#ifdef WINDOWSNT
FARPROC set_fg;  /* Pointer to AllowSetForegroundWindow.  */
FARPROC get_wc;  /* Pointer to RealGetWindowClassA.  */

BOOL CALLBACK
w32_find_emacs_process (hWnd, lParam)
     HWND hWnd;
     LPARAM lParam;
{
  DWORD pid;
  char class[6];

  /* Reject any window not of class "Emacs".  */
  if (! get_wc (hWnd, class, sizeof (class))
      || strcmp (class, "Emacs"))
    return TRUE;

  /* We only need the process id, not the thread id.  */
  (void) GetWindowThreadProcessId (hWnd, &pid);

  /* Not the one we're looking for.  */
  if (pid != (DWORD) emacs_pid) return TRUE;

  /* OK, let's raise it.  */
  set_fg (emacs_pid);

  /* Stop enumeration.  */
  return FALSE;
}

/*
 * Search for a window of class "Emacs" and owned by a process with
 * process id = emacs_pid.  If found, allow it to grab the focus.
 */
void
w32_give_focus ()
{
  HMODULE hUser32;

  /* It shouldn't happen when dealing with TCP sockets.  */
  if (!emacs_pid) return;

  if (!(hUser32 = LoadLibrary ("user32.dll"))) return;

  /* Modern Windows restrict which processes can set the foreground window.
     emacsclient can allow Emacs to grab the focus by calling the function
     AllowSetForegroundWindow.  Unfortunately, older Windows (W95, W98 and
     NT) lack this function, so we have to check its availability.  */
  if ((set_fg = GetProcAddress (hUser32, "AllowSetForegroundWindow"))
      && (get_wc = GetProcAddress (hUser32, "RealGetWindowClassA")))
    EnumWindows (w32_find_emacs_process, (LPARAM) 0);

  FreeLibrary (hUser32);
}
#endif

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
      message (TRUE, "%s: file name or argument required\nTry `%s --help' for more information\n",
              progname, progname);
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
      message (TRUE, "%s: %s (%s)\n", progname,
#ifdef HAVE_GETCWD
	       "Cannot get current working directory",
#else
	       string,
#endif
	       strerror (errno));
      fail (argc, argv);
    }

#ifdef WINDOWSNT
  w32_give_focus ();
#endif

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
          else if (! file_name_absolute_p (argv[i]))
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

#endif /* HAVE_SOCKETS && HAVE_INET_SOCKETS */

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
