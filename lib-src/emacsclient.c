/* Client process that communicates with GNU Emacs acting as server.

Copyright (C) 1986-1987, 1994, 1999-2020 Free Software Foundation, Inc.

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

#ifdef WINDOWSNT

/* ms-w32.h defines these, which disables sockets altogether!  */
# undef _WINSOCKAPI_
# undef _WINSOCK_H

# include <malloc.h>
# include <windows.h>
# include <commctrl.h>
# include <io.h>
# include <winsock2.h>

# define HSOCKET SOCKET
# define CLOSE_SOCKET closesocket
# define INITIALIZE() initialize_sockets ()

char *w32_getenv (const char *);
# define egetenv(VAR) w32_getenv (VAR)

# undef signal

#else /* !WINDOWSNT */

# ifdef HAVE_NTGUI
#  include <windows.h>
# endif

# include "syswait.h"

# include <arpa/inet.h>
# include <fcntl.h>
# include <netinet/in.h>
# include <sys/socket.h>
# include <sys/un.h>

# define SOCKETS_IN_FILE_SYSTEM

# define INVALID_SOCKET (-1)
# define HSOCKET int
# define CLOSE_SOCKET close
# define INITIALIZE()

# define egetenv(VAR) getenv (VAR)

#endif /* !WINDOWSNT */

#include <ctype.h>
#include <errno.h>
#include <getopt.h>
#include <inttypes.h>
#include <pwd.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <dosname.h>
#include <intprops.h>
#include <min-max.h>
#include <pathmax.h>
#include <unlocked-io.h>

/* Work around GCC bug 88251.  */
#if GNUC_PREREQ (7, 0, 0)
# pragma GCC diagnostic ignored "-Wformat-truncation=2"
#endif


/* Name used to invoke this program.  */
static char const *progname;

/* The first argument to main.  */
static int main_argc;

/* The second argument to main.  */
static char *const *main_argv;

/* True means don't wait for a response from Emacs.  --no-wait.  */
static bool nowait;

/* True means don't print messages for successful operations.  --quiet.  */
static bool quiet;

/* True means don't print values returned from emacs. --suppress-output.  */
static bool suppress_output;

/* True means args are expressions to be evaluated.  --eval.  */
static bool eval;

/* True means open a new frame.  --create-frame etc.  */
static bool create_frame;

/* The display on which Emacs should work.  --display.  */
static char const *display;

/* The alternate display we should try if Emacs does not support display.  */
static char const *alt_display;

/* The parent window ID, if we are opening a frame via XEmbed.  */
static char *parent_id;

/* True means open a new Emacs frame on the current terminal.  */
static bool tty;

/* If non-NULL, the name of an editor to fallback to if the server
   is not running.  --alternate-editor.   */
static char *alternate_editor;

#ifdef SOCKETS_IN_FILE_SYSTEM
/* If non-NULL, the filename of the UNIX socket.  */
static char const *socket_name;
#endif

/* If non-NULL, the filename of the authentication file.  */
static char const *server_file;

/* If non-NULL, the tramp prefix emacs must use to find the files.  */
static char const *tramp_prefix;

/* If nonzero, PID of the Emacs server process.  */
static pid_t emacs_pid;

/* If non-NULL, a string that should form a frame parameter alist to
   be used for the new frame.  */
static char const *frame_parameters;

static _Noreturn void print_help_and_exit (void);

/* Long command-line options.  */

static struct option const longopts[] =
{
  { "no-wait",	no_argument,	   NULL, 'n' },
  { "quiet",	no_argument,	   NULL, 'q' },
  { "suppress-output", no_argument, NULL, 'u' },
  { "eval",	no_argument,	   NULL, 'e' },
  { "help",	no_argument,	   NULL, 'H' },
  { "version",	no_argument,	   NULL, 'V' },
  { "tty",	no_argument,       NULL, 't' },
  { "nw",	no_argument,       NULL, 't' },
  { "create-frame", no_argument,   NULL, 'c' },
  { "alternate-editor", required_argument, NULL, 'a' },
  { "frame-parameters", required_argument, NULL, 'F' },
#ifdef SOCKETS_IN_FILE_SYSTEM
  { "socket-name",	required_argument, NULL, 's' },
#endif
  { "server-file",	required_argument, NULL, 'f' },
  { "display",	required_argument, NULL, 'd' },
  { "parent-id", required_argument, NULL, 'p' },
  { "tramp",	required_argument, NULL, 'T' },
  { 0, 0, 0, 0 }
};

/* Short options, in the same order as the corresponding long options.
   There is no '-p' short option.  */
static char const shortopts[] =
  "nqueHVtca:F:"
#ifdef SOCKETS_IN_FILE_SYSTEM
  "s:"
#endif
  "f:d:T:";


/* Like malloc but get fatal error if memory is exhausted.  */

static void * ATTRIBUTE_MALLOC
xmalloc (size_t size)
{
  void *result = malloc (size);
  if (result == NULL)
    {
      perror ("malloc");
      exit (EXIT_FAILURE);
    }
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */

static void *
xrealloc (void *ptr, size_t size)
{
  void *result = realloc (ptr, size);
  if (result == NULL)
    {
      perror ("realloc");
      exit (EXIT_FAILURE);
    }
  return result;
}

/* Like strdup but get a fatal error if memory is exhausted. */

static char * ATTRIBUTE_MALLOC
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

/* From sysdep.c */
#if !defined HAVE_GET_CURRENT_DIR_NAME || defined BROKEN_GET_CURRENT_DIR_NAME

char *get_current_dir_name (void);

/* Return the current working directory.  Returns NULL on errors.
   Any other returned value must be freed with free.  This is used
   only when get_current_dir_name is not defined on the system.  */
char *
get_current_dir_name (void)
{
  /* The maximum size of a directory name, including the terminating NUL.
     Leave room so that the caller can append a trailing slash.  */
  ptrdiff_t dirsize_max = min (PTRDIFF_MAX, SIZE_MAX) - 1;

  /* The maximum size of a buffer for a file name, including the
     terminating NUL.  This is bounded by PATH_MAX, if available.  */
  ptrdiff_t bufsize_max = dirsize_max;
#ifdef PATH_MAX
  bufsize_max = min (bufsize_max, PATH_MAX);
#endif

  char *buf;
  struct stat dotstat, pwdstat;
  size_t pwdlen;
  /* If PWD is accurate, use it instead of calling getcwd.  PWD is
     sometimes a nicer name, and using it may avoid a fatal error if a
     parent directory is searchable but not readable.  */
  char const *pwd = egetenv ("PWD");
  if (pwd
      && (pwdlen = strnlen (pwd, bufsize_max)) < bufsize_max
      && IS_DIRECTORY_SEP (pwd[pwdlen && IS_DEVICE_SEP (pwd[1]) ? 2 : 0])
      && stat (pwd, &pwdstat) == 0
      && stat (".", &dotstat) == 0
      && dotstat.st_ino == pwdstat.st_ino
      && dotstat.st_dev == pwdstat.st_dev)
    {
      buf = xmalloc (strlen (pwd) + 1);
      strcpy (buf, pwd);
    }
  else
    {
      size_t buf_size = 1024;
      for (;;)
        {
	  int tmp_errno;
	  buf = malloc (buf_size);
	  if (! buf)
	    break;
          if (getcwd (buf, buf_size) == buf)
            break;
	  tmp_errno = errno;
	  free (buf);
	  if (tmp_errno != ERANGE)
            {
              errno = tmp_errno;
              return NULL;
            }
          buf_size *= 2;
	  if (! buf_size)
	    {
	      errno = ENOMEM;
	      return NULL;
	    }
        }
    }
  return buf;
}
#endif

#ifdef WINDOWSNT

# define REG_ROOT "SOFTWARE\\GNU\\Emacs"

char *w32_get_resource (HKEY, const char *, LPDWORD);

/* Retrieve an environment variable from the Emacs subkeys of the registry.
   Return NULL if the variable was not found, or it was empty.
   This code is based on w32_get_resource (w32.c).  */
char *
w32_get_resource (HKEY predefined, const char *key, LPDWORD type)
{
  HKEY hrootkey = NULL;
  char *result = NULL;
  DWORD cbData;

  if (RegOpenKeyEx (predefined, REG_ROOT, 0, KEY_READ, &hrootkey)
      == ERROR_SUCCESS)
    {
      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData)
	  == ERROR_SUCCESS)
	{
	  result = xmalloc (cbData);

	  if ((RegQueryValueEx (hrootkey, key, NULL, type, (LPBYTE) result,
				&cbData)
	       != ERROR_SUCCESS)
	      || *result == 0)
	    {
	      free (result);
	      result = NULL;
	    }
	}

      RegCloseKey (hrootkey);
    }

  return result;
}

/*
  getenv wrapper for Windows

  Value is allocated on the heap, and can be free'd.

  This is needed to duplicate Emacs's behavior, which is to look for
  environment variables in the registry if they don't appear in the
  environment.  */
char *
w32_getenv (const char *envvar)
{
  char *value;
  DWORD dwType;

  if ((value = getenv (envvar)))
    /* Found in the environment.  strdup it, because values returned
       by getenv cannot be free'd.  */
    return xstrdup (value);

  if (! (value = w32_get_resource (HKEY_CURRENT_USER, envvar, &dwType)) &&
      ! (value = w32_get_resource (HKEY_LOCAL_MACHINE, envvar, &dwType)))
    {
      /* "w32console" is what Emacs on Windows uses for tty-type under -nw.  */
      if (strcmp (envvar, "TERM") == 0)
	return xstrdup ("w32console");
      /* Found neither in the environment nor in the registry.  */
      return NULL;
    }

  if (dwType == REG_SZ)
    /* Registry; no need to expand.  */
    return value;

  if (dwType == REG_EXPAND_SZ)
    {
      DWORD size;

      if ((size = ExpandEnvironmentStrings (value, NULL, 0)))
	{
	  char *buffer = xmalloc (size);
	  if (ExpandEnvironmentStrings (value, buffer, size))
	    {
	      /* Found and expanded.  */
	      free (value);
	      return buffer;
	    }

	  /* Error expanding.  */
	  free (buffer);
	}
    }

  /* Not the right type, or not correctly expanded.  */
  free (value);
  return NULL;
}

int w32_window_app (void);

int
w32_window_app (void)
{
  static int window_app = -1;
  char szTitle[MAX_PATH];

  if (window_app < 0)
    {
      /* Checking for STDOUT does not work; it's a valid handle also in
         nonconsole apps.  Testing for the console title seems to work. */
      window_app = (GetConsoleTitleA (szTitle, MAX_PATH) == 0);
      if (window_app)
        InitCommonControls ();
    }

  return window_app;
}

/* execvp wrapper for Windows.  Quotes arguments with embedded spaces.

  This is necessary due to the broken implementation of exec* routines in
  the Microsoft libraries: they concatenate the arguments together without
  quoting special characters, and pass the result to CreateProcess, with
  predictably bad results.  By contrast, POSIX execvp passes the arguments
  directly into the argv array of the child process.  */

int w32_execvp (const char *, char **);

int
w32_execvp (const char *path, char **argv)
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

# undef execvp
# define execvp w32_execvp

/* Emulation of ttyname for Windows.  */
const char *ttyname (int);
const char *
ttyname (int fd)
{
  return "CONOUT$";
}

#endif /* WINDOWSNT */

/* Display a normal or error message.
   On Windows, use a message box if compiled as a Windows app.  */
static void message (bool, const char *, ...) ATTRIBUTE_FORMAT_PRINTF (2, 3);
static void
message (bool is_error, const char *format, ...)
{
  va_list args;

  va_start (args, format);

#ifdef WINDOWSNT
  if (w32_window_app ())
    {
      char msg[2048];
      vsnprintf (msg, sizeof msg, format, args);
      msg[sizeof msg - 1] = '\0';

      if (is_error)
	MessageBox (NULL, msg, "Emacsclient ERROR", MB_ICONERROR);
      else
	MessageBox (NULL, msg, "Emacsclient", MB_ICONINFORMATION);
    }
  else
#endif
    {
      FILE *f = is_error ? stderr : stdout;

      vfprintf (f, format, args);
      fflush (f);
    }

  va_end (args);
}

/* Decode the options from argv and argc.
   The global variable 'optind' will say how many arguments we used up.  */

static void
decode_options (int argc, char **argv)
{
  alternate_editor = egetenv ("ALTERNATE_EDITOR");
  tramp_prefix = egetenv ("EMACSCLIENT_TRAMP");

  while (true)
    {
      int opt = getopt_long_only (argc, argv, shortopts, longopts, NULL);
      if (opt < 0)
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

#ifdef SOCKETS_IN_FILE_SYSTEM
	case 's':
	  socket_name = optarg;
	  break;
#endif

	case 'f':
	  server_file = optarg;
	  break;

	  /* We used to disallow this argument in w32, but it seems better
	     to allow it, for the occasional case where the user is
	     connecting with a w32 client to a server compiled with X11
	     support.  */
	case 'd':
	  display = optarg;
	  break;

	case 'n':
	  nowait = true;
	  break;

	case 'e':
	  eval = true;
	  break;

	case 'q':
	  quiet = true;
	  break;

	case 'u':
	  suppress_output = true;
	  break;

	case 'V':
	  message (false, "emacsclient %s\n", PACKAGE_VERSION);
	  exit (EXIT_SUCCESS);
	  break;

        case 't':
	  tty = true;
	  create_frame = true;
          break;

        case 'c':
	  create_frame = true;
          break;

	case 'p':
	  parent_id = optarg;
	  create_frame = true;
	  break;

	case 'H':
	  print_help_and_exit ();
	  break;

        case 'F':
          frame_parameters = optarg;
          break;

        case 'T':
          tramp_prefix = optarg;
          break;

	default:
	  message (true, "Try '%s --help' for more information\n", progname);
	  exit (EXIT_FAILURE);
	  break;
	}
    }

  /* If the -c option is used (without -t) and no --display argument
     is provided, try $DISPLAY.
     Without the -c option, we used to set 'display' to $DISPLAY by
     default, but this changed the default behavior and is sometimes
     inconvenient.  So we force users to use "--display $DISPLAY" if
     they want Emacs to connect to their current display.

     Some window systems have a notion of default display not
     reflected in the DISPLAY variable.  If the user didn't give us an
     explicit display, try this platform-specific after trying the
     display in DISPLAY (if any).  */
  if (create_frame && !tty && !display)
    {
      /* Set these here so we use a default_display only when the user
         didn't give us an explicit display.  */
#if defined (NS_IMPL_COCOA)
      alt_display = "ns";
#elif defined (HAVE_NTGUI)
      alt_display = "w32";
#endif

      display = egetenv ("DISPLAY");
    }

  if (!display)
    {
      display = alt_display;
      alt_display = NULL;
    }

  /* A null-string display is invalid.  */
  if (display && !display[0])
    display = NULL;

  /* If no display is available, new frames are tty frames.  */
  if (create_frame && !display)
    tty = true;

#ifdef WINDOWSNT
  /* Emacs on Windows does not support graphical and text terminal
     frames in the same instance.  So, treat the -t and -c options as
     equivalent, and open a new frame on the server's terminal.
     Ideally, we would set tty = true only if the server is running in a
     console, but alas we don't know that.  As a workaround, always
     ask for a tty frame, and let server.el figure it out.  */
  if (create_frame)
    {
      display = NULL;
      tty = true;
    }
#endif /* WINDOWSNT */
}


static _Noreturn void
print_help_and_exit (void)
{
  /* Spaces and tabs are significant in this message; they're chosen so the
     message aligns properly both in a tty and in a Windows message box.
     Please try to preserve them; otherwise the output is very hard to read
     when using emacsclientw.  */
  message (false,
	   "Usage: %s [OPTIONS] FILE...\n%s%s%s", progname, "\
Tell the Emacs server to visit the specified files.\n\
Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.\n\
\n\
The following OPTIONS are accepted:\n\
-V, --version		Just print version info and return\n\
-H, --help    		Print this usage information message\n\
-nw, -t, --tty 		Open a new Emacs frame on the current terminal\n\
-c, --create-frame    	Create a new frame instead of trying to\n\
			use the current Emacs frame\n\
", "\
-F ALIST, --frame-parameters=ALIST\n\
			Set the parameters of a new frame\n\
-e, --eval    		Evaluate the FILE arguments as ELisp expressions\n\
-n, --no-wait		Don't wait for the server to return\n\
-q, --quiet		Don't display messages on success\n\
-u, --suppress-output   Don't display return values from the server\n\
-d DISPLAY, --display=DISPLAY\n\
			Visit the file in the given display\n\
", "\
--parent-id=ID          Open in parent window ID, via XEmbed\n"
#ifdef SOCKETS_IN_FILE_SYSTEM
"-s SOCKET, --socket-name=SOCKET\n\
			Set filename of the UNIX socket for communication\n"
#endif
"-f SERVER, --server-file=SERVER\n\
			Set filename of the TCP authentication file\n\
-a EDITOR, --alternate-editor=EDITOR\n\
			Editor to fallback to if the server is not running\n"
"			If EDITOR is the empty string, start Emacs in daemon\n\
			mode and try connecting again\n"
"-T PREFIX, --tramp=PREFIX\n\
                        PREFIX to prepend to filenames sent by emacsclient\n\
                        for locating files remotely via Tramp\n"
"\n\
Report bugs with M-x report-emacs-bug.\n");
  exit (EXIT_SUCCESS);
}

/* Try to run a different command, or --if no alternate editor is
   defined-- exit with an error code.
   Uses argv, but gets it from the global variable main_argv.  */

static _Noreturn void
fail (void)
{
  if (alternate_editor)
    {
      size_t extra_args_size = (main_argc - optind + 1) * sizeof (char *);
      size_t new_argv_size = extra_args_size;
      char **new_argv = xmalloc (new_argv_size);
      char *s = xstrdup (alternate_editor);
      ptrdiff_t toks = 0;

      /* Unpack alternate_editor's space-separated tokens into new_argv.  */
      for (char *tok = s; tok != NULL && *tok != '\0';)
        {
          /* Allocate new token.  */
          ++toks;
          new_argv = xrealloc (new_argv,
			       new_argv_size + toks * sizeof (char *));

          /* Skip leading delimiters, and set separator, skipping any
             opening quote.  */
          size_t skip = strspn (tok, " \"");
          tok += skip;
          char sep = (skip > 0 && tok[-1] == '"') ? '"' : ' ';

          /* Record start of token.  */
          new_argv[toks - 1] = tok;

          /* Find end of token and overwrite it with NUL.  */
          tok = strchr (tok, sep);
          if (tok != NULL)
            *tok++ = '\0';
        }

      /* Append main_argv arguments to new_argv.  */
      memcpy (&new_argv[toks], main_argv + optind, extra_args_size);

      execvp (*new_argv, new_argv);
      message (true, "%s: error executing alternate editor \"%s\"\n",
	       progname, alternate_editor);
    }
  exit (EXIT_FAILURE);
}


#ifdef SOCKETS_IN_FILE_SYSTEM
static void act_on_signals (HSOCKET);
#else
static void act_on_signals (HSOCKET s) {}
static void init_signals (void) {}
#endif

enum { AUTH_KEY_LENGTH = 64 };

static void
sock_err_message (const char *function_name)
{
#ifdef WINDOWSNT
  /* On Windows, the socket library was historically separate from the
     standard C library, so errors are handled differently.  */

  if (w32_window_app () && alternate_editor)
    return;

  char *msg = NULL;

  FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM
                 | FORMAT_MESSAGE_ALLOCATE_BUFFER
                 | FORMAT_MESSAGE_ARGUMENT_ARRAY,
                 NULL, WSAGetLastError (), 0, (LPTSTR)&msg, 0, NULL);

  message (true, "%s: %s: %s\n", progname, function_name, msg);

  LocalFree (msg);
#else
  message (true, "%s: %s: %s\n", progname, function_name, strerror (errno));
#endif
}


/* Send to S the data in *DATA when either
   - the data's last byte is '\n', or
   - the buffer is full (but this shouldn't happen)
   Otherwise, just accumulate the data.  */
static void
send_to_emacs (HSOCKET s, const char *data)
{
  enum { SEND_BUFFER_SIZE = 4096 };

  /* Buffer to accumulate data to send in TCP connections.  */
  static char send_buffer[SEND_BUFFER_SIZE + 1];

  /* Fill pointer for the send buffer.  */
  static int sblen;

  for (ptrdiff_t dlen = strlen (data); dlen != 0; )
    {
      int part = min (dlen, SEND_BUFFER_SIZE - sblen);
      memcpy (&send_buffer[sblen], data, part);
      data += part;
      sblen += part;

      if (sblen == SEND_BUFFER_SIZE
	  || (0 < sblen && send_buffer[sblen - 1] == '\n'))
	{
	  int sent;
	  while ((sent = send (s, send_buffer, sblen, 0)) < 0)
	    {
	      if (errno != EINTR)
		{
		  message (true, "%s: failed to send %d bytes to socket: %s\n",
			   progname, sblen, strerror (errno));
		  fail ();
		}
	      /* Act on signals not requiring communication to Emacs,
		 but defer action on the others to avoid confusing the
		 communication currently in progress.  */
	      act_on_signals (INVALID_SOCKET);
	    }
	  sblen -= sent;
	  memmove (send_buffer, &send_buffer[sent], sblen);
	}

      dlen -= part;
    }
}


/* In STR, insert a & before each &, each space, each newline, and
   any initial -.  Change spaces to underscores, too, so that the
   return value never contains a space.

   Does not change the string.  Outputs the result to S.  */
static void
quote_argument (HSOCKET s, const char *str)
{
  char *copy = xmalloc (strlen (str) * 2 + 1);
  char *q = copy;
  if (*str == '-')
    *q++ = '&', *q++ = *str++;
  for (; *str; str++)
    {
      char c = *str;
      if (c == ' ')
	*q++ = '&', c = '_';
      else if (c == '\n')
	*q++ = '&', c = 'n';
      else if (c == '&')
	*q++ = '&';
      *q++ = c;
    }
  *q = 0;

  send_to_emacs (s, copy);

  free (copy);
}


/* The inverse of quote_argument.  Remove quoting in string STR by
   modifying the addressed string in place.  Return STR.  */

static char *
unquote_argument (char *str)
{
  char const *p = str;
  char *q = str;
  char c;

  do
    {
      c = *p++;
      if (c == '&')
	{
	  c = *p++;
	  if (c == '_')
	    c = ' ';
	  else if (c == 'n')
	    c = '\n';
	}
      *q++ = c;
    }
  while (c);

  return str;
}


#ifdef WINDOWSNT
/* Wrapper to make WSACleanup a cdecl, as required by atexit.  */
void __cdecl close_winsock (void);
void __cdecl
close_winsock (void)
{
  WSACleanup ();
}

/* Initialize the WinSock2 library.  */
void initialize_sockets (void);
void
initialize_sockets (void)
{
  WSADATA wsaData;

  if (WSAStartup (MAKEWORD (2, 0), &wsaData))
    {
      message (true, "%s: error initializing WinSock2\n", progname);
      exit (EXIT_FAILURE);
    }

  atexit (close_winsock);
}
#endif /* WINDOWSNT */


/* If the home directory is HOME, and XDG_CONFIG_HOME's value is XDG,
   return the configuration file with basename CONFIG_FILE.  Fail if
   the configuration file could not be opened.  */

static FILE *
open_config (char const *home, char const *xdg, char const *config_file)
{
  ptrdiff_t xdgsubdirsize = xdg ? strlen (xdg) + sizeof "/emacs/server/" : 0;
  ptrdiff_t homesuffixsizemax = max (sizeof "/.config/emacs/server/",
				     sizeof "/.emacs.d/server/");
  ptrdiff_t homesubdirsizemax = home ? strlen (home) + homesuffixsizemax : 0;
  char *configname = xmalloc (max (xdgsubdirsize, homesubdirsizemax)
			      + strlen (config_file));
  FILE *config;

  if (home)
    {
      strcpy (stpcpy (stpcpy (configname, home), "/.emacs.d/server/"),
              config_file);
      config = fopen (configname, "rb");
    }
  else
    config = NULL;

  if (! config && (xdg || home))
    {
      strcpy ((xdg
               ? stpcpy (stpcpy (configname, xdg), "/emacs/server/")
               : stpcpy (stpcpy (configname, home), "/.config/emacs/server/")),
              config_file);
      config = fopen (configname, "rb");
    }

  free (configname);
  return config;
}

/* Read the information needed to set up a TCP comm channel with
   the Emacs server: host, port, and authentication string.  */

static bool
get_server_config (const char *config_file, struct sockaddr_in *server,
		   char *authentication)
{
  char dotted[32];
  char *port;
  FILE *config;

  if (IS_ABSOLUTE_FILE_NAME (config_file))
    config = fopen (config_file, "rb");
  else
    {
      char const *xdg = egetenv ("XDG_CONFIG_HOME");
      config = open_config (egetenv ("HOME"), xdg, config_file);
#ifdef WINDOWSNT
      if (!config)
	config = open_config (egetenv ("APPDATA"), xdg, config_file);
#endif
    }

  if (! config)
    return false;

  if (fgets (dotted, sizeof dotted, config)
      && (port = strchr (dotted, ':')))
    *port++ = '\0';
  else
    {
      message (true, "%s: invalid configuration info\n", progname);
      exit (EXIT_FAILURE);
    }

  memset (server, 0, sizeof *server);
  server->sin_family = AF_INET;
  server->sin_addr.s_addr = inet_addr (dotted);
  server->sin_port = htons (atoi (port));

  if (! fread (authentication, AUTH_KEY_LENGTH, 1, config))
    {
      message (true, "%s: cannot read authentication info\n", progname);
      exit (EXIT_FAILURE);
    }

  fclose (config);

  return true;
}

/* Like socket (DOMAIN, TYPE, PROTOCOL), except arrange for the
   resulting file descriptor to be close-on-exec.  */

static HSOCKET
cloexec_socket (int domain, int type, int protocol)
{
#ifdef SOCK_CLOEXEC
  return socket (domain, type | SOCK_CLOEXEC, protocol);
#else
  HSOCKET s = socket (domain, type, protocol);
# ifndef WINDOWSNT
  if (0 <= s)
    fcntl (s, F_SETFD, FD_CLOEXEC);
# endif
  return s;
#endif
}

static HSOCKET
set_tcp_socket (const char *local_server_file)
{
  union {
    struct sockaddr_in in;
    struct sockaddr sa;
  } server;
  struct linger l_arg = { .l_onoff = 1, .l_linger = 1 };
  char auth_string[AUTH_KEY_LENGTH + 1];

  if (! get_server_config (local_server_file, &server.in, auth_string))
    return INVALID_SOCKET;

  if (server.in.sin_addr.s_addr != inet_addr ("127.0.0.1") && !quiet)
    message (false, "%s: connected to remote socket at %s\n",
	     progname, inet_ntoa (server.in.sin_addr));

  /* Open up an AF_INET socket.  */
  HSOCKET s = cloexec_socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (s < 0)
    {
      /* Since we have an alternate to try out, this is not an error
	 yet; popping out a modal dialog at this stage would make -a
	 option totally useless for emacsclientw -- the user will
	 still get an error message if the alternate editor fails.  */
      sock_err_message ("socket");
      return INVALID_SOCKET;
    }

  /* Set up the socket.  */
  if (connect (s, &server.sa, sizeof server.in) != 0)
    {
      sock_err_message ("connect");
      CLOSE_SOCKET (s);
      return INVALID_SOCKET;
    }

  /* The cast to 'const char *' is to avoid a compiler warning when
     compiling for MS-Windows sockets.  */
  setsockopt (s, SOL_SOCKET, SO_LINGER, (const char *) &l_arg, sizeof l_arg);

  /* Send the authentication.  */
  auth_string[AUTH_KEY_LENGTH] = '\0';

  send_to_emacs (s, "-auth ");
  send_to_emacs (s, auth_string);
  send_to_emacs (s, " ");

  return s;
}


/* Return true if PREFIX is a prefix of STRING. */
static bool
strprefix (const char *prefix, const char *string)
{
  return !strncmp (prefix, string, strlen (prefix));
}

/* Get tty name and type.  If successful, store the type into
   *TTY_TYPE and the name into *TTY_NAME, and return true.
   Otherwise, fail if NOABORT is zero, or return false if NOABORT.  */

static bool
find_tty (const char **tty_type, const char **tty_name, bool noabort)
{
  const char *type = egetenv ("TERM");
  const char *name = ttyname (STDOUT_FILENO);

  if (!name)
    {
      if (noabort)
	return false;
      message (true, "%s: could not get terminal name\n", progname);
      fail ();
    }

  if (!type)
    {
      if (noabort)
	return false;
      message (true, "%s: please set the TERM variable to your terminal type\n",
	       progname);
      fail ();
    }

  const char *inside_emacs = egetenv ("INSIDE_EMACS");
  if (inside_emacs && strstr (inside_emacs, ",term:")
      && strprefix ("eterm", type))
    {
      if (noabort)
	return false;
      /* This causes nasty, MULTI_KBOARD-related input lockouts. */
      message (true, ("%s: opening a frame in an Emacs term buffer"
		      " is not supported\n"),
	       progname);
      fail ();
    }

  *tty_name = name;
  *tty_type = type;
  return true;
}

/* Return the process group if in the foreground, the negative of the
   process group if in the background, and zero if there is no
   foreground process group for the controlling terminal.
   Unfortunately, use of this function introduces an unavoidable race,
   since whether the process is in the foreground or background can
   change at any time.  */

static pid_t
process_grouping (void)
{
#ifdef SOCKETS_IN_FILE_SYSTEM
  pid_t tcpgrp = tcgetpgrp (STDOUT_FILENO);
  if (0 <= tcpgrp)
    {
      pid_t pgrp = getpgrp ();
      return tcpgrp == pgrp ? pgrp : -pgrp;
    }
#endif
  return 0;
}

#ifdef SOCKETS_IN_FILE_SYSTEM

/* Return the file status of NAME, ordinarily a socket.
   It should be owned by UID.  Return one of the following:
  >0 - 'stat' failed with this errno value
  -1 - isn't owned by us
   0 - success: none of the above */

static int
socket_status (const char *name, uid_t uid)
{
  struct stat statbfr;

  if (stat (name, &statbfr) != 0)
    return errno;

  if (statbfr.st_uid != uid)
    return -1;

  return 0;
}


/* Signal handlers merely set a flag, to avoid race conditions on
   POSIXish systems.  Non-POSIX platforms lacking sigaction make do
   with traditional calls to 'signal'; races are rare so this usually
   works.  Although this approach may treat multiple deliveries of SIG
   as a single delivery and may act on signals in a different order
   than received, that is OK for emacsclient.  Also, this approach may
   omit output if a printf call is interrupted by a signal, but printf
   output is not that important (emacsclient does not check for printf
   errors, after all) so this is also OK for emacsclient.  */

/* Reinstall for SIG the signal handler HANDLER if needed.  It is
   needed on a non-POSIX or traditional platform where an interrupt
   resets the signal handler to SIG_DFL.  */
static void
reinstall_handler_if_needed (int sig, void (*handler) (int))
{
# ifndef SA_RESETHAND
  /* This is a platform without POSIX's sigaction.  */
  signal (sig, handler);
# endif
}

/* Flags for each signal, and handlers that set the flags.  */

static sig_atomic_t volatile
  got_sigcont, got_sigtstp, got_sigttou, got_sigwinch;

static void
handle_sigcont (int sig)
{
  got_sigcont = 1;
  reinstall_handler_if_needed (sig, handle_sigcont);
}
static void
handle_sigtstp (int sig)
{
  got_sigtstp = 1;
  reinstall_handler_if_needed (sig, handle_sigtstp);
}
static void
handle_sigttou (int sig)
{
  got_sigttou = 1;
  reinstall_handler_if_needed (sig, handle_sigttou);
}
static void
handle_sigwinch (int sig)
{
  got_sigwinch = 1;
  reinstall_handler_if_needed (sig, handle_sigwinch);
}

/* Install for signal SIG the handler HANDLER.  However, if FLAG is
   non-null and if the signal is currently being ignored, do not
   install the handler and keep *FLAG zero.  */

static void
install_handler (int sig, void (*handler) (int), sig_atomic_t volatile *flag)
{
# ifdef SA_RESETHAND
  if (flag)
    {
      struct sigaction oact;
      if (sigaction (sig, NULL, &oact) == 0 && oact.sa_handler == SIG_IGN)
	return;
    }
  struct sigaction act = { .sa_handler = handler };
  sigemptyset (&act.sa_mask);
  sigaction (sig, &act, NULL);
# else
  void (*ohandler) (int) = signal (sig, handler);
  if (flag)
    {
      if (ohandler == SIG_IGN)
	{
	  signal (sig, SIG_IGN);
	  /* While HANDLER was mistakenly installed a signal may have
	     arrived and set *FLAG, so clear *FLAG now.  */
	  *flag = 0;
	}
    }
# endif
}

/* Initial installation of signal handlers.  */

static void
init_signals (void)
{
  install_handler (SIGCONT, handle_sigcont, &got_sigcont);
  install_handler (SIGTSTP, handle_sigtstp, &got_sigtstp);
  install_handler (SIGTTOU, handle_sigttou, &got_sigttou);
  install_handler (SIGWINCH, handle_sigwinch, &got_sigwinch);
  /* Don't mess with SIGINT and SIGQUIT, as Emacs has no way to
     determine which terminal the signal came from.  C-g is a normal
     input event on secondary terminals.  */
}

/* Act on delivered tty-related signal SIG that normally has handler
   HANDLER.  EMACS_SOCKET connects to Emacs.  */

static void
act_on_tty_signal (int sig, void (*handler) (int), HSOCKET emacs_socket)
{
  /* Notify Emacs that we are going to sleep.  Normally the suspend is
     initiated by Emacs via server-handle-suspend-tty, but if the
     server gets out of sync with reality, we may get a SIGTSTP on
     C-z.  Handling this signal and notifying Emacs about it should
     get things under control again.  */
  send_to_emacs (emacs_socket, "-suspend \n");

  /* Execute the default action by temporarily changing handling to
     the default and resignaling.  */
  install_handler (sig, SIG_DFL, NULL);
  raise (sig);
  install_handler (sig, handler, NULL);
}

/* Act on delivered signals if possible.  If EMACS_SOCKET is valid,
   use it to communicate to Emacs.  */

static void
act_on_signals (HSOCKET emacs_socket)
{
  while (true)
    {
      bool took_action = false;

      if (emacs_socket != INVALID_SOCKET)
	{
	  if (got_sigcont)
	    {
	      got_sigcont = 0;
	      took_action = true;
	      pid_t grouping = process_grouping ();
	      if (grouping < 0)
		{
		  if (tty)
		    {
		      /* Cancel the continue.  */
		      kill (grouping, SIGTTIN);
		    }
		}
	      else
		send_to_emacs (emacs_socket, "-resume \n");
	    }

	  if (got_sigtstp)
	    {
	      got_sigtstp = 0;
	      took_action = true;
	      act_on_tty_signal (SIGTSTP, handle_sigtstp, emacs_socket);
	    }
	  if (got_sigttou)
	    {
	      got_sigttou = 0;
	      took_action = true;
	      act_on_tty_signal (SIGTTOU, handle_sigttou, emacs_socket);
	    }
	}

      if (emacs_pid && got_sigwinch)
	{
	  got_sigwinch = 0;
	  took_action = true;
	  kill (emacs_pid, SIGWINCH);
	}

      if (!took_action)
	break;
    }
}

/* Create in SOCKNAME (of size SOCKNAMESIZE) a name for a local socket.
   The first TMPDIRLEN bytes of SOCKNAME are already initialized to be
   the name of a temporary directory.  Use UID and SERVER_NAME to
   concoct the name.  Return the total length of the name if successful,
   -1 if it does not fit (and store a truncated name in that case).
   Fail if TMPDIRLEN is out of range.  */

static int
local_sockname (char *sockname, int socknamesize, int tmpdirlen,
		uintmax_t uid, char const *server_name)
{
  /* If ! (0 <= TMPDIRLEN && TMPDIRLEN < SOCKNAMESIZE) the truncated
     temporary directory name is already in SOCKNAME, so nothing more
     need be stored.  */
  if (0 <= tmpdirlen)
    {
      int remaining = socknamesize - tmpdirlen;
      if (0 < remaining)
	{
	  int suffixlen = snprintf (&sockname[tmpdirlen], remaining,
				    "/emacs%"PRIuMAX"/%s", uid, server_name);
	  if (0 <= suffixlen && suffixlen < remaining)
	    return tmpdirlen + suffixlen;
	}
    }
  return -1;
}

/* Create a local socket for SERVER_NAME and connect it to Emacs.  If
   SERVER_NAME is a file name component, the local socket name
   relative to a well-known location in a temporary directory.
   Otherwise, the local socket name is SERVER_NAME.  */

static HSOCKET
set_local_socket (char const *server_name)
{
  union {
    struct sockaddr_un un;
    struct sockaddr sa;
  } server = {{ .sun_family = AF_UNIX }};
  char *sockname = server.un.sun_path;
  enum { socknamesize = sizeof server.un.sun_path };
  int tmpdirlen = -1;
  int socknamelen = -1;
  uid_t uid = geteuid ();
  bool tmpdir_used = false;

  if (strchr (server_name, '/')
      || (ISSLASH ('\\') && strchr (server_name, '\\')))
    socknamelen = snprintf (sockname, socknamesize, "%s", server_name);
  else
    {
      /* socket_name is a file name component.  */
      char const *xdg_runtime_dir = egetenv ("XDG_RUNTIME_DIR");
      if (xdg_runtime_dir)
	socknamelen = snprintf (sockname, socknamesize, "%s/emacs/%s",
				xdg_runtime_dir, server_name);
      else
	{
	  char const *tmpdir = egetenv ("TMPDIR");
	  if (tmpdir)
	    tmpdirlen = snprintf (sockname, socknamesize, "%s", tmpdir);
	  else
	    {
# ifdef DARWIN_OS
#  ifndef _CS_DARWIN_USER_TEMP_DIR
#   define _CS_DARWIN_USER_TEMP_DIR 65537
#  endif
	      size_t n = confstr (_CS_DARWIN_USER_TEMP_DIR,
				  sockname, socknamesize);
	      if (0 < n && n < (size_t) -1)
	        tmpdirlen = min (n - 1, socknamesize);
# endif
	      if (tmpdirlen < 0)
		tmpdirlen = snprintf (sockname, socknamesize, "/tmp");
	    }
	  socknamelen = local_sockname (sockname, socknamesize, tmpdirlen,
					uid, server_name);
	  tmpdir_used = true;
	}
    }

  if (! (0 <= socknamelen && socknamelen < socknamesize))
    {
      message (true, "%s: socket-name %s... too long\n", progname, sockname);
      fail ();
    }

  /* See if the socket exists, and if it's owned by us. */
  int sock_status = socket_status (sockname, uid);
  if (sock_status)
    {
      /* Failing that, see if LOGNAME or USER exist and differ from
	 our euid.  If so, look for a socket based on the UID
	 associated with the name.  This is reminiscent of the logic
	 that init_editfns uses to set the global Vuser_full_name.  */

      char const *user_name = egetenv ("LOGNAME");

      if (!user_name)
	user_name = egetenv ("USER");

      if (user_name)
	{
	  struct passwd *pw = getpwnam (user_name);

	  if (pw && pw->pw_uid != uid)
	    {
	      /* We're running under su, apparently. */
	      socknamelen = local_sockname (sockname, socknamesize, tmpdirlen,
					    pw->pw_uid, server_name);
	      if (socknamelen < 0)
		{
		  message (true, "%s: socket-name %s... too long\n",
			   progname, sockname);
		  exit (EXIT_FAILURE);
		}

	      sock_status = socket_status (sockname, uid);
	    }
	}
    }

  if (sock_status == 0)
    {
      HSOCKET s = cloexec_socket (AF_UNIX, SOCK_STREAM, 0);
      if (s < 0)
	{
	  message (true, "%s: socket: %s\n", progname, strerror (errno));
	  return INVALID_SOCKET;
	}
      if (connect (s, &server.sa, sizeof server.un) != 0)
	{
	  message (true, "%s: connect: %s\n", progname, strerror (errno));
	  CLOSE_SOCKET (s);
	  return INVALID_SOCKET;
	}

      struct stat connect_stat;
      if (fstat (s, &connect_stat) != 0)
	sock_status = errno;
      else if (connect_stat.st_uid == uid)
	return s;
      else
	sock_status = -1;

      CLOSE_SOCKET (s);
    }

  if (sock_status < 0)
    message (true, "%s: Invalid socket owner\n", progname);
  else if (sock_status == ENOENT)
    {
      if (tmpdir_used)
	{
	  uintmax_t id = uid;
	  char sockdirname[socknamesize];
	  int sockdirnamelen = snprintf (sockdirname, sizeof sockdirname,
					 "/run/user/%"PRIuMAX, id);
	  if (0 <= sockdirnamelen && sockdirnamelen < sizeof sockdirname
	      && faccessat (AT_FDCWD, sockdirname, X_OK, AT_EACCESS) == 0)
	    message
	      (true,
	       ("%s: Should XDG_RUNTIME_DIR='%s' be in the environment?\n"
		"%s: (Be careful: XDG_RUNTIME_DIR is security-related.)\n"),
	       progname, sockdirname, progname);
	}
      message (true,
	       ("%s: can't find socket; have you started the server?\n"
		"%s: To start the server in Emacs,"
		" type \"M-x server-start\".\n"),
	       progname, progname);
    }
  else
    message (true, "%s: can't stat %s: %s\n",
	     progname, sockname, strerror (sock_status));

  return INVALID_SOCKET;
}
#endif /* SOCKETS_IN_FILE_SYSTEM */

static HSOCKET
set_socket (bool no_exit_if_error)
{
  HSOCKET s;
  const char *local_server_file = server_file;

  INITIALIZE ();

#ifdef SOCKETS_IN_FILE_SYSTEM
  if (!socket_name)
    socket_name = egetenv ("EMACS_SOCKET_NAME");

  if (socket_name)
    {
      /* Explicit --socket-name argument, or environment variable.  */
      s = set_local_socket (socket_name);
      if (s != INVALID_SOCKET || no_exit_if_error)
	return s;
      message (true, "%s: error accessing socket \"%s\"\n",
	       progname, socket_name);
      exit (EXIT_FAILURE);
    }
#endif

  /* Explicit --server-file arg or EMACS_SERVER_FILE variable.  */
  if (!local_server_file)
    local_server_file = egetenv ("EMACS_SERVER_FILE");

  if (local_server_file)
    {
      s = set_tcp_socket (local_server_file);
      if (s != INVALID_SOCKET || no_exit_if_error)
	return s;

      message (true, "%s: error accessing server file \"%s\"\n",
	       progname, local_server_file);
      exit (EXIT_FAILURE);
    }

#ifdef SOCKETS_IN_FILE_SYSTEM
  /* Implicit local socket.  */
  s = set_local_socket ("server");
  if (s != INVALID_SOCKET)
    return s;
#endif

  /* Implicit server file.  */
  s = set_tcp_socket ("server");
  if (s != INVALID_SOCKET || no_exit_if_error)
    return s;

  /* No implicit or explicit socket, and no alternate editor.  */
  message (true, "%s: No socket or alternate editor.  Please use:\n\n"
#ifdef SOCKETS_IN_FILE_SYSTEM
"\t--socket-name\n"
#endif
"\t--server-file      (or environment variable EMACS_SERVER_FILE)\n\
\t--alternate-editor (or environment variable ALTERNATE_EDITOR)\n",
           progname);
  exit (EXIT_FAILURE);
}

#ifdef HAVE_NTGUI
FARPROC set_fg;  /* Pointer to AllowSetForegroundWindow.  */
FARPROC get_wc;  /* Pointer to RealGetWindowClassA.  */

void w32_set_user_model_id (void);

void
w32_set_user_model_id (void)
{
  HMODULE shell;
  HRESULT (WINAPI * set_user_model) (const wchar_t * id);

  /* On Windows 7 and later, we need to set the user model ID
     to associate emacsclient launched files with Emacs frames
     in the UI.  */
  shell = LoadLibrary ("shell32.dll");
  if (shell)
    {
      set_user_model
	= (void *) GetProcAddress (shell,
				   "SetCurrentProcessExplicitAppUserModelID");
      /* If the function is defined, then we are running on Windows 7
	 or newer, and the UI uses this to group related windows
	 together.  Since emacs, runemacs, emacsclient are related, we
	 want them grouped even though the executables are different,
	 so we need to set a consistent ID between them.  */
      if (set_user_model)
	set_user_model (L"GNU.Emacs");

      FreeLibrary (shell);
    }
}

BOOL CALLBACK w32_find_emacs_process (HWND, LPARAM);

BOOL CALLBACK
w32_find_emacs_process (HWND hWnd, LPARAM lParam)
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

/* Search for a window of class "Emacs" and owned by a process with
   process id = emacs_pid.  If found, allow it to grab the focus.  */
void w32_give_focus (void);

void
w32_give_focus (void)
{
  HANDLE user32;

  /* It shouldn't happen when dealing with TCP sockets.  */
  if (!emacs_pid) return;

  user32 = GetModuleHandle ("user32.dll");

  if (!user32)
    return;

  /* Modern Windows restrict which processes can set the foreground window.
     emacsclient can allow Emacs to grab the focus by calling the function
     AllowSetForegroundWindow.  Unfortunately, older Windows (W95, W98 and
     NT) lack this function, so we have to check its availability.  */
  if ((set_fg = GetProcAddress (user32, "AllowSetForegroundWindow"))
      && (get_wc = GetProcAddress (user32, "RealGetWindowClassA")))
    EnumWindows (w32_find_emacs_process, (LPARAM) 0);
}
#endif /* HAVE_NTGUI */

/* Start the emacs daemon and try to connect to it.  */

static HSOCKET
start_daemon_and_retry_set_socket (void)
{
#ifndef WINDOWSNT
  pid_t dpid;
  int status;

  dpid = fork ();

  if (dpid > 0)
    {
      pid_t w = waitpid (dpid, &status, WUNTRACED | WCONTINUED);

      if (w < 0 || !WIFEXITED (status) || WEXITSTATUS (status))
	{
	  message (true, "Error: Could not start the Emacs daemon\n");
	  exit (EXIT_FAILURE);
	}

      /* Try connecting, the daemon should have started by now.  */
      message (true,
	       "Emacs daemon should have started, trying to connect again\n");
    }
  else if (dpid < 0)
    {
      fprintf (stderr, "Error: Cannot fork!\n");
      exit (EXIT_FAILURE);
    }
  else
    {
      char emacs[] = "emacs";
      char daemon_option[] = "--daemon";
      char *d_argv[3];
      d_argv[0] = emacs;
      d_argv[1] = daemon_option;
      d_argv[2] = 0;
# ifdef SOCKETS_IN_FILE_SYSTEM
      if (socket_name != NULL)
	{
	  /* Pass  --daemon=socket_name as argument.  */
	  const char *deq = "--daemon=";
	  char *daemon_arg = xmalloc (strlen (deq)
				      + strlen (socket_name) + 1);
	  strcpy (stpcpy (daemon_arg, deq), socket_name);
	  d_argv[1] = daemon_arg;
	}
# endif
      execvp ("emacs", d_argv);
      message (true, "%s: error starting emacs daemon\n", progname);
      exit (EXIT_FAILURE);
    }
#else  /* WINDOWSNT */
  DWORD wait_result;
  HANDLE w32_daemon_event;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  ZeroMemory (&si, sizeof si);
  si.cb = sizeof si;
  ZeroMemory (&pi, sizeof pi);

  /* We start Emacs in daemon mode, and then wait for it to signal us
     it is ready to accept client connections, by asserting an event
     whose name is known to the daemon (defined by nt/inc/ms-w32.h).  */

  if (!CreateProcess (NULL, (LPSTR)"emacs --daemon", NULL, NULL, FALSE,
                      CREATE_NO_WINDOW, NULL, NULL, &si, &pi))
    {
      char* msg = NULL;

      FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM
		     | FORMAT_MESSAGE_ALLOCATE_BUFFER
		     | FORMAT_MESSAGE_ARGUMENT_ARRAY,
		     NULL, GetLastError (), 0, (LPTSTR)&msg, 0, NULL);
      message (true, "%s: error starting emacs daemon (%s)\n", progname, msg);
      exit (EXIT_FAILURE);
    }

  w32_daemon_event = CreateEvent (NULL, TRUE, FALSE, W32_DAEMON_EVENT);
  if (w32_daemon_event == NULL)
    {
      message (true, "Couldn't create Windows daemon event");
      exit (EXIT_FAILURE);
    }
  if ((wait_result = WaitForSingleObject (w32_daemon_event, INFINITE))
      != WAIT_OBJECT_0)
    {
      const char *msg = NULL;

      switch (wait_result)
	{
	case WAIT_ABANDONED:
	  msg = "The daemon exited unexpectedly";
	  break;
	case WAIT_TIMEOUT:
	  /* Can't happen due to INFINITE.  */
	default:
	case WAIT_FAILED:
	  FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM
			 | FORMAT_MESSAGE_ALLOCATE_BUFFER
			 | FORMAT_MESSAGE_ARGUMENT_ARRAY,
			 NULL, GetLastError (), 0, (LPTSTR)&msg, 0, NULL);
	  break;
	}
      message (true, "Error: Could not start the Emacs daemon: %s\n", msg);
      exit (EXIT_FAILURE);
    }
  CloseHandle (w32_daemon_event);

  /* Try connecting, the daemon should have started by now.  */
  /* It's just a progress message, so don't pop a dialog if this is
     emacsclientw.  */
  if (!w32_window_app ())
    message (true,
	     "Emacs daemon should have started, trying to connect again\n");
#endif /* WINDOWSNT */

  HSOCKET emacs_socket = set_socket (true);
  if (emacs_socket == INVALID_SOCKET)
    {
      message (true,
	       "Error: Cannot connect even after starting the Emacs daemon\n");
      exit (EXIT_FAILURE);
    }
  return emacs_socket;
}

int
main (int argc, char **argv)
{
  main_argc = argc;
  main_argv = argv;
  progname = argv[0] ? argv[0] : "emacsclient";

  int rl = 0;
  bool skiplf = true;
  char string[BUFSIZ + 1];
  int exit_status = EXIT_SUCCESS;

#ifdef HAVE_NTGUI
  /* On Windows 7 and later, we need to explicitly associate
     emacsclient with emacs so the UI behaves sensibly.  This
     association does no harm if we're not actually connecting to an
     Emacs using a window display.  */
  w32_set_user_model_id ();
#endif

  /* Process options.  */
  decode_options (argc, argv);

  if (! (optind < argc || eval || create_frame))
    {
      message (true, ("%s: file name or argument required\n"
		      "Try '%s --help' for more information\n"),
	       progname, progname);
      exit (EXIT_FAILURE);
    }

#ifdef SOCKETS_IN_FILE_SYSTEM
  if (tty)
    {
      pid_t grouping = process_grouping ();
      if (grouping < 0)
	kill (grouping, SIGTTIN);
    }
#endif

  /* If alternate_editor is the empty string, start the emacs daemon
     in case of failure to connect.  */
  bool start_daemon_if_needed = alternate_editor && !alternate_editor[0];

  HSOCKET emacs_socket = set_socket (alternate_editor
				     || start_daemon_if_needed);
  if (emacs_socket == INVALID_SOCKET)
    {
      if (! start_daemon_if_needed)
	fail ();

      emacs_socket = start_daemon_and_retry_set_socket ();
    }

  char *cwd = get_current_dir_name ();
  if (cwd == 0)
    {
      message (true, "%s: %s\n", progname,
	       "Cannot get current working directory");
      fail ();
    }

#ifdef HAVE_NTGUI
  if (display && !strcmp (display, "w32"))
  w32_give_focus ();
#endif

  /* Send over our environment and current directory. */
  if (create_frame)
    {
      for (char *const *e = environ; *e; e++)
        {
          send_to_emacs (emacs_socket, "-env ");
          quote_argument (emacs_socket, *e);
          send_to_emacs (emacs_socket, " ");
        }
    }
  send_to_emacs (emacs_socket, "-dir ");
  if (tramp_prefix)
    quote_argument (emacs_socket, tramp_prefix);
  quote_argument (emacs_socket, cwd);
  free (cwd);
  send_to_emacs (emacs_socket, "/");
  send_to_emacs (emacs_socket, " ");

 retry:
  if (nowait)
    send_to_emacs (emacs_socket, "-nowait ");

  if (!create_frame)
    send_to_emacs (emacs_socket, "-current-frame ");

  if (display)
    {
      send_to_emacs (emacs_socket, "-display ");
      quote_argument (emacs_socket, display);
      send_to_emacs (emacs_socket, " ");
    }

  if (parent_id)
    {
      send_to_emacs (emacs_socket, "-parent-id ");
      quote_argument (emacs_socket, parent_id);
      send_to_emacs (emacs_socket, " ");
    }

  if (frame_parameters && create_frame)
    {
      send_to_emacs (emacs_socket, "-frame-parameters ");
      quote_argument (emacs_socket, frame_parameters);
      send_to_emacs (emacs_socket, " ");
    }

  /* Unless we are certain we don't want to occupy the tty, send our
     tty information to Emacs.  For example, in daemon mode Emacs may
     need to occupy this tty if no other frame is available.  */
  if (create_frame || !eval)
    {
      const char *tty_type, *tty_name;

      if (find_tty (&tty_type, &tty_name, !tty))
	{
	  /* Install signal handlers before opening a frame on the
	     current tty.  */
	  init_signals ();

	  send_to_emacs (emacs_socket, "-tty ");
	  quote_argument (emacs_socket, tty_name);
	  send_to_emacs (emacs_socket, " ");
	  quote_argument (emacs_socket, tty_type);
	  send_to_emacs (emacs_socket, " ");
	}
    }

  if (create_frame && !tty)
    send_to_emacs (emacs_socket, "-window-system ");

  if (optind < argc)
    {
      for (int i = optind; i < argc; i++)
	{

	  if (eval)
            {
              /* Don't prepend cwd or anything like that.  */
              send_to_emacs (emacs_socket, "-eval ");
              quote_argument (emacs_socket, argv[i]);
              send_to_emacs (emacs_socket, " ");
              continue;
            }

	  char *p = argv[i];
	  if (*p == '+')
            {
	      unsigned char c;
	      do
		c = *++p;
	      while (isdigit (c) || c == ':');

	      if (c == 0)
                {
                  send_to_emacs (emacs_socket, "-position ");
                  quote_argument (emacs_socket, argv[i]);
                  send_to_emacs (emacs_socket, " ");
                  continue;
                }
            }
#ifdef WINDOWSNT
	  else if (! IS_ABSOLUTE_FILE_NAME (argv[i])
		   && (isalpha (argv[i][0]) && argv[i][1] == ':'))
	    /* Windows can have a different default directory for each
	       drive, so the cwd passed via "-dir" is not sufficient
	       to account for that.
	       If the user uses <drive>:<relpath>, we hence need to be
	       careful to expand <relpath> with the default directory
	       corresponding to <drive>.  */
	    {
	      char *filename = xmalloc (MAX_PATH);
	      DWORD size;

	      size = GetFullPathName (argv[i], MAX_PATH, filename, NULL);
	      if (size > 0 && size < MAX_PATH)
		argv[i] = filename;
	      else
		free (filename);
	    }
#endif

          send_to_emacs (emacs_socket, "-file ");
	  if (tramp_prefix && IS_ABSOLUTE_FILE_NAME (argv[i]))
	    quote_argument (emacs_socket, tramp_prefix);
          quote_argument (emacs_socket, argv[i]);
          send_to_emacs (emacs_socket, " ");
        }
    }
  else if (eval)
    {
      /* Read expressions interactively.  */
      while (fgets (string, BUFSIZ, stdin))
	{
	  send_to_emacs (emacs_socket, "-eval ");
	  quote_argument (emacs_socket, string);
	}
      send_to_emacs (emacs_socket, " ");
    }

  send_to_emacs (emacs_socket, "\n");

  /* Wait for an answer. */
  if (!eval && !tty && !nowait && !quiet && 0 <= process_grouping ())
    {
      printf ("Waiting for Emacs...");
      skiplf = false;
    }
  fflush (stdout);

  /* Now, wait for an answer and print any messages.  */
  while (exit_status == EXIT_SUCCESS)
    {
      do
	{
	  act_on_signals (emacs_socket);
	  rl = recv (emacs_socket, string, BUFSIZ, 0);
	}
      while (rl < 0 && errno == EINTR);

      if (rl <= 0)
        break;

      string[rl] = '\0';

      /* Loop over all NL-terminated messages.  */
      char *p = string;
      for (char *end_p = p; end_p && *end_p != '\0'; p = end_p)
	{
	  end_p = strchr (p, '\n');
	  if (end_p != NULL)
	    *end_p++ = '\0';

          if (strprefix ("-emacs-pid ", p))
            {
              /* -emacs-pid PID: The process id of the Emacs process. */
	      emacs_pid = strtoumax (p + strlen ("-emacs-pid"), NULL, 10);
            }
          else if (strprefix ("-window-system-unsupported ", p))
            {
              /* -window-system-unsupported: Emacs was compiled without support
                 for whatever window system we tried.  Try the alternate
                 display, or, failing that, try the terminal.  */
              if (alt_display)
                {
                  display = alt_display;
                  alt_display = NULL;
                }
              else
                {
                  nowait = false;
                  tty = true;
                }

              goto retry;
            }
          else if (strprefix ("-print ", p))
            {
              /* -print STRING: Print STRING on the terminal. */
	      if (!suppress_output)
		{
		  char *str = unquote_argument (p + strlen ("-print "));
		  printf (&"\n%s"[skiplf], str);
		  if (str[0])
		    skiplf = str[strlen (str) - 1] == '\n';
		}
	    }
          else if (strprefix ("-print-nonl ", p))
            {
              /* -print-nonl STRING: Print STRING on the terminal.
                 Used to continue a preceding -print command.  */
	      if (!suppress_output)
		{
		  char *str = unquote_argument (p + strlen ("-print-nonl "));
		  printf ("%s", str);
		  if (str[0])
		    skiplf = str[strlen (str) - 1] == '\n';
		}
            }
          else if (strprefix ("-error ", p))
            {
              /* -error DESCRIPTION: Signal an error on the terminal. */
              char *str = unquote_argument (p + strlen ("-error "));
              if (!skiplf)
                printf ("\n");
              fprintf (stderr, "*ERROR*: %s", str);
              if (str[0])
	        skiplf = str[strlen (str) - 1] == '\n';
              exit_status = EXIT_FAILURE;
            }
#ifndef WINDOWSNT
	  else if (strprefix ("-suspend ", p))
	    {
	      /* -suspend: Suspend this terminal, i.e., stop the process. */
	      if (!skiplf)
		printf ("\n");
	      skiplf = true;
	      kill (0, SIGSTOP);
	    }
#endif
	  else
	    {
	      /* Unknown command. */
	      printf (&"\n*ERROR*: Unknown message: %s\n"[skiplf], p);
	      skiplf = true;
	    }
	}
    }

  if (!skiplf && 0 <= process_grouping ())
    printf ("\n");

  if (rl < 0)
    exit_status = EXIT_FAILURE;

  CLOSE_SOCKET (emacs_socket);
  return exit_status;
}
