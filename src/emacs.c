/* Fully extensible Emacs, running on Unix, intended for GNU.

Copyright (C) 1985-1987, 1993-1995, 1997-1999, 2001-2018 Free Software
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

#define INLINE EXTERN_INLINE
#include <config.h>

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>

#include <sys/file.h>
#include <unistd.h>

#include <close-stream.h>

#define MAIN_PROGRAM
#include "lisp.h"
#include "sysstdio.h"

#ifdef WINDOWSNT
#include <fcntl.h>
#include <sys/socket.h>
#include <mbstring.h>
#include "w32.h"
#include "w32heap.h"
#endif

#if defined WINDOWSNT || defined HAVE_NTGUI
#include "w32select.h"
#include "w32font.h"
#include "w32common.h"
#endif

#if defined CYGWIN
#include "cygw32.h"
#endif

#ifdef MSDOS
#include <binary-io.h>
#include "dosfns.h"
#endif

#ifdef HAVE_LIBSYSTEMD
# include <systemd/sd-daemon.h>
# include <sys/socket.h>
#endif

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#include "intervals.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "xwidget.h"
#include "atimer.h"
#include "blockinput.h"
#include "syssignal.h"
#include "process.h"
#include "frame.h"
#include "termhooks.h"
#include "keyboard.h"
#include "keymap.h"
#include "category.h"
#include "charset.h"
#include "composite.h"
#include "dispextern.h"
#include "ptr-bounds.h"
#include "regex.h"
#include "sheap.h"
#include "syntax.h"
#include "sysselect.h"
#include "systime.h"
#include "puresize.h"

#include "getpagesize.h"
#include "gnutls.h"

#if (defined PROFILING \
     && (defined __FreeBSD__ || defined GNU_LINUX || defined __MINGW32__))
# include <sys/gmon.h>
extern void moncontrol (int mode);
#endif

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif

#if HAVE_WCHAR_H
# include <wchar.h>
#endif

#ifdef HAVE_SETRLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

static const char emacs_version[] = PACKAGE_VERSION;
static const char emacs_copyright[] = COPYRIGHT;
static const char emacs_bugreport[] = PACKAGE_BUGREPORT;

/* Empty lisp strings.  To avoid having to build any others.  */
Lisp_Object empty_unibyte_string, empty_multibyte_string;

#ifdef WINDOWSNT
/* Cache for externally loaded libraries.  */
Lisp_Object Vlibrary_cache;
#endif

/* Set after Emacs has started up the first time.
   Prevents reinitialization of the Lisp world and keymaps
   on subsequent starts.  */
bool initialized;

#ifndef CANNOT_DUMP
/* Set to true if this instance of Emacs might dump.  */
# ifndef DOUG_LEA_MALLOC
static
# endif
bool might_dump;
#endif

/* If true, Emacs should not attempt to use a window-specific code,
   but instead should use the virtual terminal under which it was started.  */
bool inhibit_window_system;

/* If true, a filter or a sentinel is running.  Tested to save the match
   data on the first attempt to change it inside asynchronous code.  */
bool running_asynch_code;

#if defined (HAVE_X_WINDOWS) || defined (HAVE_NS)
/* If true, -d was specified, meaning we're using some window system.  */
bool display_arg;
#endif

#if defined GNU_LINUX && !defined CANNOT_DUMP
/* The gap between BSS end and heap start as far as we can tell.  */
static uprintmax_t heap_bss_diff;
#endif

/* To run as a background daemon under Cocoa or Windows,
   we must do a fork+exec, not a simple fork.

   On Cocoa, CoreFoundation lib fails in forked process:
   http://developer.apple.com/ReleaseNotes/
   CoreFoundation/CoreFoundation.html)

   On Windows, a Cygwin fork child cannot access the USER subsystem.

   We mark being in the exec'd process by a daemon name argument of
   form "--daemon=\nFD0,FD1\nNAME" where FD are the pipe file descriptors,
   NAME is the original daemon name, if any. */
#if defined NS_IMPL_COCOA || (defined HAVE_NTGUI && defined CYGWIN)
# define DAEMON_MUST_EXEC
#endif

/* True means running Emacs without interactive terminal.  */
bool noninteractive;

/* True means remove site-lisp directories from load-path.  */
bool no_site_lisp;

/* True means put details like time stamps into builds.  */
bool build_details;

/* Name for the server started by the daemon.*/
static char *daemon_name;

/* 0 not a daemon, 1 new-style (foreground), 2 old-style (background).  */
int daemon_type;

#ifndef WINDOWSNT
/* Pipe used to send exit notification to the background daemon parent at
   startup.  On Windows, we use a kernel event instead.  */
static int daemon_pipe[2];
#else
HANDLE w32_daemon_event;
#endif

/* Save argv and argc.  */
char **initial_argv;
int initial_argc;

static void sort_args (int argc, char **argv);
static void syms_of_emacs (void);

/* C99 needs each string to be at most 4095 characters, and the usage
   strings below are split to not overflow this limit.  */
static char const *const usage_message[] =
  { "\
\n\
Run Emacs, the extensible, customizable, self-documenting real-time\n\
display editor.  The recommended way to start Emacs for normal editing\n\
is with no options at all.\n\
\n\
Run M-x info RET m emacs RET m emacs invocation RET inside Emacs to\n\
read the main documentation for these command-line arguments.\n\
\n\
Initialization options:\n\
\n\
",
    "\
--batch                     do not do interactive display; implies -q\n\
--chdir DIR                 change to directory DIR\n\
--daemon, --bg-daemon[=NAME] start a (named) server in the background\n\
--fg-daemon[=NAME]          start a (named) server in the foreground\n\
--debug-init                enable Emacs Lisp debugger for init file\n\
--display, -d DISPLAY       use X server DISPLAY\n\
",
#ifdef HAVE_MODULES
    "\
--module-assertions         assert behavior of dynamic modules\n\
",
#endif
    "\
--no-build-details          do not add build details such as time stamps\n\
--no-desktop                do not load a saved desktop\n\
--no-init-file, -q          load neither ~/.emacs nor default.el\n\
--no-loadup, -nl            do not load loadup.el into bare Emacs\n\
--no-site-file              do not load site-start.el\n\
--no-x-resources            do not load X resources\n\
--no-site-lisp, -nsl        do not add site-lisp directories to load-path\n\
--no-splash                 do not display a splash screen on startup\n\
--no-window-system, -nw     do not communicate with X, ignoring $DISPLAY\n\
",
    "\
--quick, -Q                 equivalent to:\n\
                              -q --no-site-file --no-site-lisp --no-splash\n\
                              --no-x-resources\n\
--script FILE               run FILE as an Emacs Lisp script\n\
--terminal, -t DEVICE       use DEVICE for terminal I/O\n\
--user, -u USER             load ~USER/.emacs instead of your own\n\
\n\
",
    "\
Action options:\n\
\n\
FILE                    visit FILE\n\
+LINE                   go to line LINE in next FILE\n\
+LINE:COLUMN            go to line LINE, column COLUMN, in next FILE\n\
--directory, -L DIR     prepend DIR to load-path (with :DIR, append DIR)\n\
--eval EXPR             evaluate Emacs Lisp expression EXPR\n\
--execute EXPR          evaluate Emacs Lisp expression EXPR\n\
",
    "\
--file FILE             visit FILE\n\
--find-file FILE        visit FILE\n\
--funcall, -f FUNC      call Emacs Lisp function FUNC with no arguments\n\
--insert FILE           insert contents of FILE into current buffer\n\
--kill                  exit without asking for confirmation\n\
--load, -l FILE         load Emacs Lisp FILE using the load function\n\
--visit FILE            visit FILE\n\
\n\
",
    "\
Display options:\n\
\n\
--background-color, -bg COLOR   window background color\n\
--basic-display, -D             disable many display features;\n\
                                  used for debugging Emacs\n\
--border-color, -bd COLOR       main border color\n\
--border-width, -bw WIDTH       width of main border\n\
",
    "\
--color, --color=MODE           override color mode for character terminals;\n\
                                  MODE defaults to `auto', and\n\
                                  can also be `never', `always',\n\
                                  or a mode name like `ansi8'\n\
--cursor-color, -cr COLOR       color of the Emacs cursor indicating point\n\
--font, -fn FONT                default font; must be fixed-width\n\
--foreground-color, -fg COLOR   window foreground color\n\
",
    "\
--fullheight, -fh               make the first frame high as the screen\n\
--fullscreen, -fs               make the first frame fullscreen\n\
--fullwidth, -fw                make the first frame wide as the screen\n\
--maximized, -mm                make the first frame maximized\n\
--geometry, -g GEOMETRY         window geometry\n\
",
    "\
--no-bitmap-icon, -nbi          do not use picture of gnu for Emacs icon\n\
--iconic                        start Emacs in iconified state\n\
--internal-border, -ib WIDTH    width between text and main border\n\
--line-spacing, -lsp PIXELS     additional space to put between lines\n\
--mouse-color, -ms COLOR        mouse cursor color in Emacs window\n\
--name NAME                     title for initial Emacs frame\n\
",
    "\
--no-blinking-cursor, -nbc      disable blinking cursor\n\
--reverse-video, -r, -rv        switch foreground and background\n\
--title, -T TITLE               title for initial Emacs frame\n\
--vertical-scroll-bars, -vb     enable vertical scroll bars\n\
--xrm XRESOURCES                set additional X resources\n\
--parent-id XID                 set parent window\n\
--help                          display this help and exit\n\
--version                       output version information and exit\n\
\n\
",
    "\
You can generally also specify long option names with a single -; for\n\
example, -batch as well as --batch.  You can use any unambiguous\n\
abbreviation for a --option.\n\
\n\
Various environment variables and window system resources also affect\n\
the operation of Emacs.  See the main documentation.\n\
\n\
Report bugs to " PACKAGE_BUGREPORT ".  First, please see the Bugs\n\
section of the Emacs manual or the file BUGS.\n"
  };


/* True if handling a fatal error already.  */
bool fatal_error_in_progress;

#ifdef HAVE_NS
/* NS autrelease pool, for memory management.  */
static void *ns_pool;
#endif

#if !HAVE_SETLOCALE
static char *
setlocale (int cat, char const *locale)
{
  return 0;
}
#endif

/* True if the current system locale uses UTF-8 encoding.  */
static bool
using_utf8 (void)
{
#ifdef HAVE_WCHAR_H
  wchar_t wc;
  mbstate_t mbs = { 0 };
  return mbrtowc (&wc, "\xc4\x80", 2, &mbs) == 2 && wc == 0x100;
#else
  return false;
#endif
}


/* Report a fatal error due to signal SIG, output a backtrace of at
   most BACKTRACE_LIMIT lines, and exit.  */
_Noreturn void
terminate_due_to_signal (int sig, int backtrace_limit)
{
  signal (sig, SIG_DFL);

  if (attempt_orderly_shutdown_on_fatal_signal)
    {
      /* If fatal error occurs in code below, avoid infinite recursion.  */
      if (! fatal_error_in_progress)
        {
          fatal_error_in_progress = 1;

          totally_unblock_input ();
          if (sig == SIGTERM || sig == SIGHUP || sig == SIGINT)
            Fkill_emacs (make_number (sig));

          shut_down_emacs (sig, Qnil);
          emacs_backtrace (backtrace_limit);
        }
    }

  /* Signal the same code; this time it will really be fatal.
     Since we're in a signal handler, the signal is blocked, so we
     have to unblock it if we want to really receive it.  */
#ifndef MSDOS
  {
    sigset_t unblocked;
    sigemptyset (&unblocked);
    sigaddset (&unblocked, sig);
    pthread_sigmask (SIG_UNBLOCK, &unblocked, 0);
  }
#endif

  emacs_raise (sig);

  /* This shouldn't be executed, but it prevents a warning.  */
  exit (1);
}

/* Code for dealing with Lisp access to the Unix command line.  */

static void
init_cmdargs (int argc, char **argv, int skip_args, char *original_pwd)
{
  int i;
  Lisp_Object name, dir, handler;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object raw_name;
  AUTO_STRING (slash_colon, "/:");

  initial_argv = argv;
  initial_argc = argc;

#ifdef WINDOWSNT
  /* Must use argv[0] converted to UTF-8, as it begets many standard
     file and directory names.  */
  {
    char argv0[MAX_UTF8_PATH];

    if (filename_from_ansi (argv[0], argv0) == 0)
      raw_name = build_unibyte_string (argv0);
    else
      raw_name = build_unibyte_string (argv[0]);
  }
#else
  raw_name = build_unibyte_string (argv[0]);
#endif

  /* Add /: to the front of the name
     if it would otherwise be treated as magic.  */
  handler = Ffind_file_name_handler (raw_name, Qt);
  if (! NILP (handler))
    raw_name = concat2 (slash_colon, raw_name);

  Vinvocation_name = Ffile_name_nondirectory (raw_name);
  Vinvocation_directory = Ffile_name_directory (raw_name);

  /* If we got no directory in argv[0], search PATH to find where
     Emacs actually came from.  */
  if (NILP (Vinvocation_directory))
    {
      Lisp_Object found;
      int yes = openp (Vexec_path, Vinvocation_name,
		       Vexec_suffixes, &found, make_number (X_OK), false);
      if (yes == 1)
	{
	  /* Add /: to the front of the name
	     if it would otherwise be treated as magic.  */
	  handler = Ffind_file_name_handler (found, Qt);
	  if (! NILP (handler))
	    found = concat2 (slash_colon, found);
	  Vinvocation_directory = Ffile_name_directory (found);
	}
    }

  if (!NILP (Vinvocation_directory)
      && NILP (Ffile_name_absolute_p (Vinvocation_directory)))
    /* Emacs was started with relative path, like ./emacs.
       Make it absolute.  */
    {
      Lisp_Object odir =
	original_pwd ? build_unibyte_string (original_pwd) : Qnil;

      Vinvocation_directory = Fexpand_file_name (Vinvocation_directory, odir);
    }

  Vinstallation_directory = Qnil;

  if (!NILP (Vinvocation_directory))
    {
      if (NILP (Vpurify_flag) && !NILP (Ffboundp (Qfile_truename)))
        Vinvocation_directory = call1 (Qfile_truename, Vinvocation_directory);

      dir = Vinvocation_directory;
#ifdef WINDOWSNT
      /* If we are running from the build directory, set DIR to the
	 src subdirectory of the Emacs tree, like on Posix
	 platforms.  */
      if (SBYTES (dir) > sizeof ("/i386/") - 1
	  && 0 == strcmp (SSDATA (dir) + SBYTES (dir) - sizeof ("/i386/") + 1,
			  "/i386/"))
	dir = Fexpand_file_name (build_string ("../.."), dir);
#else  /* !WINDOWSNT */
#endif
      name = Fexpand_file_name (Vinvocation_name, dir);
      while (1)
	{
	  Lisp_Object tem, lib_src_exists;
	  Lisp_Object etc_exists, info_exists;

	  /* See if dir contains subdirs for use by Emacs.
	     Check for the ones that would exist in a build directory,
	     not including lisp and info.  */
	  tem = Fexpand_file_name (build_string ("lib-src"), dir);
	  lib_src_exists = Ffile_exists_p (tem);

#ifdef MSDOS
	  /* MSDOS installations frequently remove lib-src, but we still
	     must set installation-directory, or else info won't find
	     its files (it uses the value of installation-directory).  */
	  tem = Fexpand_file_name (build_string ("info"), dir);
	  info_exists = Ffile_exists_p (tem);
#else
	  info_exists = Qnil;
#endif

	  if (!NILP (lib_src_exists) || !NILP (info_exists))
	    {
	      tem = Fexpand_file_name (build_string ("etc"), dir);
	      etc_exists = Ffile_exists_p (tem);
	      if (!NILP (etc_exists))
		{
		  Vinstallation_directory
		    = Ffile_name_as_directory (dir);
		  break;
		}
	    }

	  /* See if dir's parent contains those subdirs.  */
	  tem = Fexpand_file_name (build_string ("../lib-src"), dir);
	  lib_src_exists = Ffile_exists_p (tem);


#ifdef MSDOS
	  /* See the MSDOS commentary above.  */
	  tem = Fexpand_file_name (build_string ("../info"), dir);
	  info_exists = Ffile_exists_p (tem);
#else
	  info_exists = Qnil;
#endif

	  if (!NILP (lib_src_exists) || !NILP (info_exists))
	    {
	      tem = Fexpand_file_name (build_string ("../etc"), dir);
	      etc_exists = Ffile_exists_p (tem);
	      if (!NILP (etc_exists))
		{
		  tem = Fexpand_file_name (build_string (".."), dir);
		  Vinstallation_directory
		    = Ffile_name_as_directory (tem);
		  break;
		}
	    }

	  /* If the Emacs executable is actually a link,
	     next try the dir that the link points into.  */
	  tem = Ffile_symlink_p (name);
	  if (!NILP (tem))
	    {
	      name = Fexpand_file_name (tem, dir);
	      dir = Ffile_name_directory (name);
	    }
	  else
	    break;
	}
    }

  Vcommand_line_args = Qnil;

  for (i = argc - 1; i >= 0; i--)
    {
      if (i == 0 || i > skip_args)
	/* For the moment, we keep arguments as is in unibyte strings.
	   They are decoded in the function command-line after we know
	   locale-coding-system.  */
	Vcommand_line_args
	  = Fcons (build_unibyte_string (argv[i]), Vcommand_line_args);
    }

  unbind_to (count, Qnil);
}

DEFUN ("invocation-name", Finvocation_name, Sinvocation_name, 0, 0, 0,
       doc: /* Return the program name that was used to run Emacs.
Any directory names are omitted.  */)
  (void)
{
  return Fcopy_sequence (Vinvocation_name);
}

DEFUN ("invocation-directory", Finvocation_directory, Sinvocation_directory,
       0, 0, 0,
       doc: /* Return the directory name in which the Emacs executable was located.  */)
  (void)
{
  return Fcopy_sequence (Vinvocation_directory);
}


/* Test whether the next argument in ARGV matches SSTR or a prefix of
   LSTR (at least MINLEN characters).  If so, then if VALPTR is non-null
   (the argument is supposed to have a value) store in *VALPTR either
   the next argument or the portion of this one after the equal sign.
   ARGV is read starting at position *SKIPPTR; this index is advanced
   by the number of arguments used.

   Too bad we can't just use getopt for all of this, but we don't have
   enough information to do it right.  */

static bool
argmatch (char **argv, int argc, const char *sstr, const char *lstr,
          int minlen, char **valptr, int *skipptr)
{
  char *p = NULL;
  ptrdiff_t arglen;
  char *arg;

  /* Don't access argv[argc]; give up in advance.  */
  if (argc <= *skipptr + 1)
    return 0;

  arg = argv[*skipptr+1];
  if (arg == NULL)
    return 0;
  if (strcmp (arg, sstr) == 0)
    {
      if (valptr != NULL)
	{
	  *valptr = argv[*skipptr+2];
	  *skipptr += 2;
	}
      else
	*skipptr += 1;
      return 1;
    }
  arglen = (valptr != NULL && (p = strchr (arg, '=')) != NULL
	    ? p - arg : strlen (arg));
  if (lstr == 0 || arglen < minlen || strncmp (arg, lstr, arglen) != 0)
    return 0;
  else if (valptr == NULL)
    {
      *skipptr += 1;
      return 1;
    }
  else if (p != NULL)
    {
      *valptr = p+1;
      *skipptr += 1;
      return 1;
    }
  else if (argv[*skipptr+2] != NULL)
    {
      *valptr = argv[*skipptr+2];
      *skipptr += 2;
      return 1;
    }
  else
    {
      return 0;
    }
}

/* Close standard output and standard error, reporting any write
   errors as best we can.  This is intended for use with atexit.  */
static void
close_output_streams (void)
{
  if (close_stream (stdout) != 0)
    {
      emacs_perror ("Write error to standard output");
      _exit (EXIT_FAILURE);
    }

  /* Do not close stderr if addresses are being sanitized, as the
     sanitizer might report to stderr after this function is
     invoked.  */
  if (!ADDRESS_SANITIZER && close_stream (stderr) != 0)
    _exit (EXIT_FAILURE);
}

/* ARGSUSED */
int
main (int argc, char **argv)
{
  /* Variable near the bottom of the stack, and aligned appropriately
     for pointers.  */
  void *stack_bottom_variable;

  bool do_initial_setlocale;
  bool dumping;
  int skip_args = 0;
  bool no_loadup = false;
  char *junk = 0;
  char *dname_arg = 0;
#ifdef DAEMON_MUST_EXEC
  char dname_arg2[80];
#endif
  char *ch_to_dir = 0;

  /* If we use --chdir, this records the original directory.  */
  char *original_pwd = 0;

  /* Record (approximately) where the stack begins.  */
  stack_bottom = (char *) &stack_bottom_variable;

#ifndef CANNOT_DUMP
  dumping = !initialized && (strcmp (argv[argc - 1], "dump") == 0
			     || strcmp (argv[argc - 1], "bootstrap") == 0);
#else
  dumping = false;
#endif

  /* True if address randomization interferes with memory allocation.  */
# ifdef __PPC64__
  bool disable_aslr = true;
# else
  bool disable_aslr = dumping;
# endif

  if (disable_aslr && disable_address_randomization ())
    {
      /* Set this so the personality will be reverted before execs
	 after this one.  */
      xputenv ("EMACS_HEAP_EXEC=true");

      /* Address randomization was enabled, but is now disabled.
	 Re-execute Emacs to get a clean slate.  */
      execvp (argv[0], argv);

      /* If the exec fails, warn and then try anyway.  */
      perror (argv[0]);
    }

#ifndef CANNOT_DUMP
  might_dump = !initialized;

# ifdef GNU_LINUX
  if (!initialized)
    {
      char *heap_start = my_heap_start ();
      heap_bss_diff = heap_start - max (my_endbss, my_endbss_static);
    }
# endif
#endif

#if defined WINDOWSNT || defined HAVE_NTGUI
  /* Set global variables used to detect Windows version.  Do this as
     early as possible.  (unexw32.c calls this function as well, but
     the additional call here is harmless.) */
  cache_system_info ();
#ifdef WINDOWSNT
  /* On Windows 9X, we have to load UNICOWS.DLL as early as possible,
     to have non-stub implementations of APIs we need to convert file
     names between UTF-8 and the system's ANSI codepage.  */
  maybe_load_unicows_dll ();
  /* Initialize the codepage for file names, needed to decode
     non-ASCII file names during startup.  */
  w32_init_file_name_codepage ();
#endif
  w32_init_main_thread ();
#endif

#ifdef RUN_TIME_REMAP
  if (initialized)
    run_time_remap (argv[0]);
#endif

/* If using unexmacosx.c (set by s/darwin.h), we must do this. */
#if defined DARWIN_OS && !defined CANNOT_DUMP
  if (!initialized)
    unexec_init_emacs_zone ();
#endif

  init_standard_fds ();
  atexit (close_output_streams);

  sort_args (argc, argv);
  argc = 0;
  while (argv[argc]) argc++;

  if (argmatch (argv, argc, "-version", "--version", 3, NULL, &skip_args))
    {
      const char *version, *copyright;
      if (initialized)
	{
	  Lisp_Object tem, tem2;
	  tem = Fsymbol_value (intern_c_string ("emacs-version"));
	  tem2 = Fsymbol_value (intern_c_string ("emacs-copyright"));
	  if (!STRINGP (tem))
	    {
	      fprintf (stderr, "Invalid value of 'emacs-version'\n");
	      exit (1);
	    }
	  if (!STRINGP (tem2))
	    {
	      fprintf (stderr, "Invalid value of 'emacs-copyright'\n");
	      exit (1);
	    }
	  else
	    {
	      version = SSDATA (tem);
	      copyright = SSDATA (tem2);
	    }
	}
      else
	{
	  version = emacs_version;
	  copyright = emacs_copyright;
	}
      printf ("%s %s\n", PACKAGE_NAME, version);
      printf ("%s\n", copyright);
      printf ("%s comes with ABSOLUTELY NO WARRANTY.\n", PACKAGE_NAME);
      printf ("You may redistribute copies of %s\n", PACKAGE_NAME);
      printf ("under the terms of the GNU General Public License.\n");
      printf ("For more information about these matters, ");
      printf ("see the file named COPYING.\n");
      exit (0);
    }

  if (argmatch (argv, argc, "-chdir", "--chdir", 4, &ch_to_dir, &skip_args))
    {
#ifdef WINDOWSNT
      /* argv[] array is kept in its original ANSI codepage encoding,
	 we need to convert to UTF-8, for chdir to work.  */
      char newdir[MAX_UTF8_PATH];

      filename_from_ansi (ch_to_dir, newdir);
      ch_to_dir = newdir;
#endif
      original_pwd = emacs_get_current_dir_name ();
      if (chdir (ch_to_dir) != 0)
        {
          fprintf (stderr, "%s: Can't chdir to %s: %s\n",
                   argv[0], ch_to_dir, strerror (errno));
          exit (1);
        }
    }

#if defined (HAVE_SETRLIMIT) && defined (RLIMIT_STACK) && !defined (CYGWIN)
  /* Extend the stack space available.  Don't do that if dumping,
     since some systems (e.g. DJGPP) might define a smaller stack
     limit at that time.  And it's not needed on Cygwin, since emacs
     is built with an 8MB stack.  Moreover, the setrlimit call can
     cause problems on Cygwin
     (https://www.cygwin.com/ml/cygwin/2015-07/msg00096.html).  */
  struct rlimit rlim;
  if (getrlimit (RLIMIT_STACK, &rlim) == 0
      && 0 <= rlim.rlim_cur && rlim.rlim_cur <= LONG_MAX)
    {
      rlim_t lim = rlim.rlim_cur;

      /* Approximate the amount regex.c needs per unit of
	 emacs_re_max_failures, then add 33% to cover the size of the
	 smaller stacks that regex.c successively allocates and
	 discards on its way to the maximum.  */
      int min_ratio = 20 * sizeof (char *);
      int ratio = min_ratio + min_ratio / 3;

      /* Extra space to cover what we're likely to use for other
         reasons.  For example, a typical GC might take 30K stack
         frames.  */
      int extra = (30 * 1000) * 50;

      bool try_to_grow_stack = true;
#ifndef CANNOT_DUMP
      try_to_grow_stack = !noninteractive || initialized;
#endif

      if (try_to_grow_stack)
	{
	  rlim_t newlim = emacs_re_max_failures * ratio + extra;

	  /* Round the new limit to a page boundary; this is needed
	     for Darwin kernel 15.4.0 (see Bug#23622) and perhaps
	     other systems.  Do not shrink the stack and do not exceed
	     rlim_max.  Don't worry about exact values of
	     RLIM_INFINITY etc. since in practice when they are
	     nonnegative they are so large that the code does the
	     right thing anyway.  */
	  long pagesize = getpagesize ();
	  newlim += pagesize - 1;
	  if (0 <= rlim.rlim_max && rlim.rlim_max < newlim)
	    newlim = rlim.rlim_max;
	  newlim -= newlim % pagesize;

	  if (pagesize <= newlim - lim)
	    {
	      rlim.rlim_cur = newlim;
	      if (setrlimit (RLIMIT_STACK, &rlim) == 0)
		lim = newlim;
	    }
	}
      /* If the stack is big enough, let regex.c more of it before
         falling back to heap allocation.  */
      emacs_re_safe_alloca = max
        (min (lim - extra, SIZE_MAX) * (min_ratio / ratio),
         MAX_ALLOCA);
    }
#endif /* HAVE_SETRLIMIT and RLIMIT_STACK and not CYGWIN */

  clearerr_unlocked (stdin);

  emacs_backtrace (-1);

#if !defined SYSTEM_MALLOC && !defined HYBRID_MALLOC
  /* Arrange to get warning messages as memory fills up.  */
  memory_warnings (0, malloc_warning);

  /* Call malloc at least once, to run malloc_initialize_hook.
     Also call realloc and free for consistency.  */
  free (realloc (malloc (4), 4));

#endif	/* not SYSTEM_MALLOC and not HYBRID_MALLOC */

#ifdef MSDOS
  set_binary_mode (STDIN_FILENO, O_BINARY);
  fflush (stdout);
  set_binary_mode (STDOUT_FILENO, O_BINARY);
#endif /* MSDOS */

  /* Skip initial setlocale if LC_ALL is "C", as it's not needed in that case.
     The build procedure uses this while dumping, to ensure that the
     dumped Emacs does not have its system locale tables initialized,
     as that might cause screwups when the dumped Emacs starts up.  */
  {
    char *lc_all = getenv ("LC_ALL");
    do_initial_setlocale = ! lc_all || strcmp (lc_all, "C");
  }

  /* Set locale now, so that initial error messages are localized properly.
     fixup_locale must wait until later, since it builds strings.  */
  if (do_initial_setlocale)
    setlocale (LC_ALL, "");
  text_quoting_flag = using_utf8 ();

  inhibit_window_system = 0;

  /* Handle the -t switch, which specifies filename to use as terminal.  */
  while (1)
    {
      char *term;
      if (argmatch (argv, argc, "-t", "--terminal", 4, &term, &skip_args))
	{
	  emacs_close (STDIN_FILENO);
	  emacs_close (STDOUT_FILENO);
	  int result = emacs_open (term, O_RDWR, 0);
	  if (result != STDIN_FILENO
	      || (fcntl (STDIN_FILENO, F_DUPFD_CLOEXEC, STDOUT_FILENO)
		  != STDOUT_FILENO))
	    {
	      char *errstring = strerror (errno);
	      fprintf (stderr, "%s: %s: %s\n", argv[0], term, errstring);
	      exit (EXIT_FAILURE);
	    }
	  if (! isatty (STDIN_FILENO))
	    {
	      fprintf (stderr, "%s: %s: not a tty\n", argv[0], term);
	      exit (EXIT_FAILURE);
	    }
	  fprintf (stderr, "Using %s\n", term);
#ifdef HAVE_WINDOW_SYSTEM
	  inhibit_window_system = true; /* -t => -nw */
#endif
	}
      else
	break;
    }

  /* Command line option --no-windows is deprecated and thus not mentioned
     in the manual and usage information.  */
  if (argmatch (argv, argc, "-nw", "--no-window-system", 6, NULL, &skip_args)
      || argmatch (argv, argc, "-nw", "--no-windows", 6, NULL, &skip_args))
    inhibit_window_system = 1;

  /* Handle the -batch switch, which means don't do interactive display.  */
  noninteractive = 0;
  if (argmatch (argv, argc, "-batch", "--batch", 5, NULL, &skip_args))
    {
      noninteractive = 1;
      Vundo_outer_limit = Qnil;
    }
  if (argmatch (argv, argc, "-script", "--script", 3, &junk, &skip_args))
    {
      noninteractive = 1;	/* Set batch mode.  */
      /* Convert --script to -scriptload, un-skip it, and sort again
	 so that it will be handled in proper sequence.  */
      /* FIXME broken for --script=FILE - is that supposed to work?  */
      argv[skip_args - 1] = (char *) "-scriptload";
      skip_args -= 2;
      sort_args (argc, argv);
    }

  /* Handle the --help option, which gives a usage message.  */
  if (argmatch (argv, argc, "-help", "--help", 3, NULL, &skip_args))
    {
      int i;
      printf ("Usage: %s [OPTION-OR-FILENAME]...\n", argv[0]);
      for (i = 0; i < ARRAYELTS (usage_message); i++)
	fputs_unlocked (usage_message[i], stdout);
      exit (0);
    }

  daemon_type = 0;

#ifndef WINDOWSNT
  /* Make sure IS_DAEMON starts up as false.  */
  daemon_pipe[1] = 0;
#else
  w32_daemon_event = NULL;
#endif


  int sockfd = -1;

  if (argmatch (argv, argc, "-fg-daemon", "--fg-daemon", 10, NULL, &skip_args)
      || argmatch (argv, argc, "-fg-daemon", "--fg-daemon", 10, &dname_arg, &skip_args))
    {
      daemon_type = 1;           /* foreground */
    }
  else if (argmatch (argv, argc, "-daemon", "--daemon", 5, NULL, &skip_args)
      || argmatch (argv, argc, "-daemon", "--daemon", 5, &dname_arg, &skip_args)
      || argmatch (argv, argc, "-bg-daemon", "--bg-daemon", 10, NULL, &skip_args)
      || argmatch (argv, argc, "-bg-daemon", "--bg-daemon", 10, &dname_arg, &skip_args))
    {
      daemon_type = 2;          /* background */
    }


  if (daemon_type > 0)
    {
#ifndef DOS_NT
      if (daemon_type == 2)
        {
          /* Start as a background daemon: fork a new child process which
             will run the rest of the initialization code, then exit.

             Detaching a daemon requires the following steps:
             - fork
             - setsid
             - exit the parent
             - close the tty file-descriptors

             We only want to do the last 2 steps once the daemon is ready to
             serve requests, i.e. after loading .emacs (initialization).
             OTOH initialization may start subprocesses (e.g. ispell) and these
             should be run from the proper process (the one that will end up
             running as daemon) and with the proper "session id" in order for
             them to keep working after detaching, so fork and setsid need to be
             performed before initialization.

             We want to avoid exiting before the server socket is ready, so
             use a pipe for synchronization.  The parent waits for the child
             to close its end of the pipe (using `daemon-initialized')
             before exiting.  */
          if (emacs_pipe (daemon_pipe) != 0)
            {
              fprintf (stderr, "Cannot pipe!\n");
              exit (1);
            }
        } /* daemon_type == 2 */

#ifdef HAVE_LIBSYSTEMD
      /* Read the number of sockets passed through by systemd.  */
      int systemd_socket = sd_listen_fds (1);

      if (systemd_socket > 1)
        fprintf (stderr,
		 ("\n"
		  "Warning: systemd passed more than one socket to Emacs.\n"
		  "Try 'Accept=false' in the Emacs socket unit file.\n"));
      else if (systemd_socket == 1
	       && (0 < sd_is_socket (SD_LISTEN_FDS_START,
				     AF_UNSPEC, SOCK_STREAM, 1)))
	sockfd = SD_LISTEN_FDS_START;
#endif /* HAVE_LIBSYSTEMD */

#ifdef USE_GTK
      fprintf (stderr, "\nWarning: due to a long standing Gtk+ bug\nhttp://bugzilla.gnome.org/show_bug.cgi?id=85715\n\
Emacs might crash when run in daemon mode and the X11 connection is unexpectedly lost.\n\
Using an Emacs configured with --with-x-toolkit=lucid does not have this problem.\n");
#endif /* USE_GTK */

      if (daemon_type == 2)
        {
          pid_t f;
#ifndef DAEMON_MUST_EXEC

          f = fork ();
#else /* DAEMON_MUST_EXEC */
          if (!dname_arg || !strchr (dname_arg, '\n'))
            f = fork ();  /* in orig */
          else
            f = 0;  /* in exec'd */
#endif /* !DAEMON_MUST_EXEC */
          if (f > 0)
            {
              int retval;
              char buf[1];

              /* Close unused writing end of the pipe.  */
              emacs_close (daemon_pipe[1]);

              /* Just wait for the child to close its end of the pipe.  */
              do
                {
                  retval = read (daemon_pipe[0], &buf, 1);
                }
              while (retval == -1 && errno == EINTR);

              if (retval < 0)
                {
                  fprintf (stderr, "Error reading status from child\n");
                  exit (1);
                }
              else if (retval == 0)
                {
                  fprintf (stderr, "Error: server did not start correctly\n");
                  exit (1);
                }

              emacs_close (daemon_pipe[0]);
              exit (0);
            }
          if (f < 0)
            {
              emacs_perror ("fork");
              exit (EXIT_CANCELED);
            }

#ifdef DAEMON_MUST_EXEC
          {
            /* In orig process, forked as child, OR in exec'd. */
            if (!dname_arg || !strchr (dname_arg, '\n'))
              {  /* In orig, child: now exec w/special daemon name. */
                char fdStr[80];
                int fdStrlen =
                  snprintf (fdStr, sizeof fdStr,
                            "--bg-daemon=\n%d,%d\n%s", daemon_pipe[0],
                            daemon_pipe[1], dname_arg ? dname_arg : "");

                if (! (0 <= fdStrlen && fdStrlen < sizeof fdStr))
                  {
                    fprintf (stderr, "daemon: child name too long\n");
                    exit (EXIT_CANNOT_INVOKE);
                  }

                argv[skip_args] = fdStr;

                fcntl (daemon_pipe[0], F_SETFD, 0);
                fcntl (daemon_pipe[1], F_SETFD, 0);
                execvp (argv[0], argv);
                emacs_perror (argv[0]);
                exit (errno == ENOENT ? EXIT_ENOENT : EXIT_CANNOT_INVOKE);
              }

            /* In exec'd: parse special dname into pipe and name info. */
            if (!dname_arg || !strchr (dname_arg, '\n')
                || strlen (dname_arg) < 1 || strlen (dname_arg) > 70)
          {
            fprintf (stderr, "emacs daemon: daemon name absent or too long\n");
            exit (EXIT_CANNOT_INVOKE);
          }
            dname_arg2[0] = '\0';
            sscanf (dname_arg, "\n%d,%d\n%s", &(daemon_pipe[0]), &(daemon_pipe[1]),
                    dname_arg2);
            dname_arg = *dname_arg2 ? dname_arg2 : NULL;
            fcntl (daemon_pipe[1], F_SETFD, FD_CLOEXEC);
          }
#endif /* DAEMON_MUST_EXEC */

          /* Close unused reading end of the pipe.  */
          emacs_close (daemon_pipe[0]);

          setsid ();
        } /* daemon_type == 2 */
#elif defined(WINDOWSNT)
      /* Indicate that we want daemon mode.  */
      w32_daemon_event = CreateEvent (NULL, TRUE, FALSE, W32_DAEMON_EVENT);
      if (w32_daemon_event == NULL)
        {
          fprintf (stderr, "Couldn't create MS-Windows event for daemon: %s\n",
		   w32_strerror (0));
          exit (1);
        }
#else /* MSDOS */
      fprintf (stderr, "This platform does not support daemon mode.\n");
      exit (1);
#endif /* MSDOS */
      if (dname_arg)
	daemon_name = xstrdup (dname_arg);
    }

#if defined HAVE_PTHREAD && !defined SYSTEM_MALLOC \
  && !defined DOUG_LEA_MALLOC && !defined HYBRID_MALLOC
# ifndef CANNOT_DUMP
  /* Do not make gmalloc thread-safe when creating bootstrap-emacs, as
     that causes an infinite recursive loop with FreeBSD.  See
     Bug#14569.  The part of this bug involving Cygwin is no longer
     relevant, now that Cygwin defines HYBRID_MALLOC.  */
  if (!noninteractive || initialized)
# endif
    malloc_enable_thread ();
#endif

  init_signals (dumping);

  noninteractive1 = noninteractive;

  /* Perform basic initializations (not merely interning symbols).  */

  if (!initialized)
    {
      init_alloc_once ();
      init_threads_once ();
      init_obarray ();
      init_eval_once ();
      init_charset_once ();
      init_coding_once ();
      init_syntax_once ();	/* Create standard syntax table.  */
      init_category_once ();	/* Create standard category table.  */
      init_casetab_once ();	/* Must be done before init_buffer_once.  */
      init_buffer_once ();	/* Create buffer table and some buffers.  */
      init_minibuf_once ();	/* Create list of minibuffers.  */
				/* Must precede init_window_once.  */

      /* Call syms_of_xfaces before init_window_once because that
	 function creates Vterminal_frame.  Termcap frames now use
	 faces, and the face implementation uses some symbols as
	 face names.  */
      syms_of_xfaces ();
      /* XXX syms_of_keyboard uses some symbols in keymap.c.  It would
         be better to arrange things not to have this dependency.  */
      syms_of_keymap ();
      /* Call syms_of_keyboard before init_window_once because
	 keyboard sets up symbols that include some face names that
	 the X support will want to use.  This can happen when
	 CANNOT_DUMP is defined.  */
      syms_of_keyboard ();

      /* Called before syms_of_fileio, because it sets up Qerror_condition.  */
      syms_of_data ();
      syms_of_fns ();  /* Before syms_of_charset which uses hash tables.  */
      syms_of_fileio ();
      /* Before syms_of_coding to initialize Vgc_cons_threshold.  */
      syms_of_alloc ();
      /* May call Ffuncall and so GC, thus the latter should be initialized.  */
      init_print_once ();
      /* Before syms_of_coding because it initializes Qcharsetp.  */
      syms_of_charset ();
      /* Before init_window_once, because it sets up the
	 Vcoding_system_hash_table.  */
      syms_of_coding ();	/* This should be after syms_of_fileio.  */

      init_window_once ();	/* Init the window system.  */
#ifdef HAVE_WINDOW_SYSTEM
      init_fringe_once ();	/* Swap bitmaps if necessary.  */
#endif /* HAVE_WINDOW_SYSTEM */
    }

  init_alloc ();
  init_threads ();

  if (do_initial_setlocale)
    {
      fixup_locale ();
      Vsystem_messages_locale = Vprevious_system_messages_locale;
      Vsystem_time_locale = Vprevious_system_time_locale;
    }

  init_eval ();
  init_atimer ();
  running_asynch_code = 0;
  init_random ();

#if defined HAVE_JSON && !defined WINDOWSNT
  init_json ();
#endif

  no_loadup
    = argmatch (argv, argc, "-nl", "--no-loadup", 6, NULL, &skip_args);

  no_site_lisp
    = argmatch (argv, argc, "-nsl", "--no-site-lisp", 11, NULL, &skip_args);

  build_details = ! argmatch (argv, argc, "-no-build-details",
			      "--no-build-details", 7, NULL, &skip_args);

#ifdef HAVE_MODULES
  bool module_assertions
    = argmatch (argv, argc, "-module-assertions", "--module-assertions", 15,
                NULL, &skip_args);
  if (dumping && module_assertions)
    {
      fputs ("Module assertions are not supported during dumping\n", stderr);
      exit (1);
    }
  init_module_assertions (module_assertions);
#endif

#ifdef HAVE_NS
  ns_pool = ns_alloc_autorelease_pool ();
#ifdef NS_IMPL_GNUSTEP
  /* GNUstep stupidly resets our locale settings after we made them.  */
  fixup_locale ();
#endif

  if (!noninteractive)
    {
#ifdef NS_IMPL_COCOA
      /* Started from GUI? */
      /* FIXME: Do the right thing if getenv returns NULL, or if
         chdir fails.  */
      if (! inhibit_window_system && ! isatty (STDIN_FILENO) && ! ch_to_dir)
        chdir (getenv ("HOME"));
      if (skip_args < argc)
        {
          if (!strncmp (argv[skip_args], "-psn", 4))
            {
              skip_args += 1;
              if (! ch_to_dir) chdir (getenv ("HOME"));
            }
          else if (skip_args+1 < argc && !strncmp (argv[skip_args+1], "-psn", 4))
            {
              skip_args += 2;
              if (! ch_to_dir) chdir (getenv ("HOME"));
            }
        }
#endif  /* COCOA */
    }
#endif /* HAVE_NS */

#ifdef HAVE_X_WINDOWS
  /* Stupid kludge to catch command-line display spec.  We can't
     handle this argument entirely in window system dependent code
     because we don't even know which window system dependent code
     to run until we've recognized this argument.  */
  {
    char *displayname = 0;
    int count_before = skip_args;

    /* Skip any number of -d options, but only use the last one.  */
    while (1)
      {
	int count_before_this = skip_args;

	if (argmatch (argv, argc, "-d", "--display", 3, &displayname, &skip_args))
	  display_arg = 1;
	else if (argmatch (argv, argc, "-display", 0, 3, &displayname, &skip_args))
	  display_arg = 1;
	else
	  break;

	count_before = count_before_this;
      }

    /* If we have the form --display=NAME,
       convert it into  -d name.
       This requires inserting a new element into argv.  */
    if (displayname && count_before < skip_args)
      {
	if (skip_args == count_before + 1)
	  {
	    memmove (argv + count_before + 3, argv + count_before + 2,
		     (argc - (count_before + 2)) * sizeof *argv);
	    argv[count_before + 2] = displayname;
	    argc++;
	  }
	argv[count_before + 1] = (char *) "-d";
      }

    if (! no_site_lisp)
      {
        if (argmatch (argv, argc, "-Q", "--quick", 3, NULL, &skip_args)
            || argmatch (argv, argc, "-quick", 0, 2, NULL, &skip_args))
          no_site_lisp = 1;
      }

    /* Don't actually discard this arg.  */
    skip_args = count_before;
  }
#else  /* !HAVE_X_WINDOWS */
  if (! no_site_lisp)
  {
    int count_before = skip_args;

    if (argmatch (argv, argc, "-Q", "--quick", 3, NULL, &skip_args)
        || argmatch (argv, argc, "-quick", 0, 2, NULL, &skip_args))
      no_site_lisp = 1;

    skip_args = count_before;
  }
#endif

  /* argmatch must not be used after here,
     except when building temacs
     because the -d argument has not been skipped in skip_args.  */

#ifdef MSDOS
  /* Call early 'cause init_environment needs it.  */
  init_dosfns ();
  /* Set defaults for several environment variables.  */
  if (initialized)
    init_environment (argc, argv, skip_args);
  else
    tzset ();
#endif /* MSDOS */

#ifdef HAVE_KQUEUE
  globals_of_kqueue ();
#endif

#ifdef HAVE_GFILENOTIFY
  globals_of_gfilenotify ();
#endif

#ifdef HAVE_NS
  /* Initialize the locale from user defaults.  */
  ns_init_locale ();
#endif

  /* Initialize and GC-protect Vinitial_environment and
     Vprocess_environment before set_initial_environment fills them
     in.  */
  if (!initialized)
    syms_of_callproc ();
  /* egetenv is a pretty low-level facility, which may get called in
     many circumstances; it seems flimsy to put off initializing it
     until calling init_callproc.  Do not do it when dumping.  */
  if (! dumping)
    set_initial_environment ();

#ifdef WINDOWSNT
  globals_of_w32 ();
#ifdef HAVE_W32NOTIFY
  globals_of_w32notify ();
#endif
  /* Initialize environment from registry settings.  Make sure to do
     this only after calling set_initial_environment so that
     Vinitial_environment and Vprocess_environment will contain only
     variables from the parent process without modifications from
     Emacs.  */
  init_environment (argv);
  init_ntproc (dumping); /* must precede init_editfns.  */
#endif

  /* AIX crashes are reported in system versions 3.2.3 and 3.2.4
     if this is not done.  Do it after set_global_environment so that we
     don't pollute Vglobal_environment.  */
  /* Setting LANG here will defeat the startup locale processing...  */
#ifdef AIX
  xputenv ("LANG=C");
#endif

  /* Init buffer storage and default directory of main buffer.  */
  init_buffer (initialized);

  init_callproc_1 ();	/* Must precede init_cmdargs and init_sys_modes.  */

  /* Must precede init_lread.  */
  init_cmdargs (argc, argv, skip_args, original_pwd);

  if (initialized)
    {
      /* Erase any pre-dump messages in the message log, to avoid confusion.  */
      Lisp_Object old_log_max;
      old_log_max = Vmessage_log_max;
      XSETFASTINT (Vmessage_log_max, 0);
      message_dolog ("", 0, 1, 0);
      Vmessage_log_max = old_log_max;
    }

  init_callproc ();	/* Must follow init_cmdargs but not init_sys_modes.  */
  init_fileio ();
  init_lread ();
#ifdef WINDOWSNT
  /* Check to see if Emacs has been installed correctly.  */
  check_windows_init_file ();
#endif

  /* Intern the names of all standard functions and variables;
     define standard keys.  */

  if (!initialized)
    {
      /* The basic levels of Lisp must come first.  Note that
	 syms_of_data and some others have already been called.  */
      syms_of_chartab ();
      syms_of_lread ();
      syms_of_print ();
      syms_of_eval ();
      syms_of_floatfns ();

      syms_of_buffer ();
      syms_of_bytecode ();
      syms_of_callint ();
      syms_of_casefiddle ();
      syms_of_casetab ();
      syms_of_category ();
      syms_of_ccl ();
      syms_of_character ();
      syms_of_cmds ();
      syms_of_dired ();
      syms_of_display ();
      syms_of_doc ();
      syms_of_editfns ();
      syms_of_emacs ();
      syms_of_filelock ();
      syms_of_indent ();
      syms_of_insdel ();
      /* syms_of_keymap (); */
      syms_of_macros ();
      syms_of_marker ();
      syms_of_minibuf ();
      syms_of_process ();
      syms_of_search ();
      syms_of_frame ();
      syms_of_syntax ();
      syms_of_terminal ();
      syms_of_term ();
      syms_of_undo ();

#ifdef HAVE_MODULES
      syms_of_module ();
#endif

#ifdef HAVE_SOUND
      syms_of_sound ();
#endif
      syms_of_textprop ();
      syms_of_composite ();
#ifdef WINDOWSNT
      syms_of_ntproc ();
#endif /* WINDOWSNT */
#if defined CYGWIN
      syms_of_cygw32 ();
#endif
      syms_of_window ();
      syms_of_xdisp ();
      syms_of_font ();
#ifdef HAVE_WINDOW_SYSTEM
      syms_of_fringe ();
      syms_of_image ();
#endif /* HAVE_WINDOW_SYSTEM */
#ifdef HAVE_X_WINDOWS
      syms_of_xterm ();
      syms_of_xfns ();
      syms_of_xmenu ();
      syms_of_fontset ();
      syms_of_xwidget ();
      syms_of_xsettings ();
#ifdef HAVE_X_SM
      syms_of_xsmfns ();
#endif
#ifdef HAVE_X11
      syms_of_xselect ();
#endif
#endif /* HAVE_X_WINDOWS */

      syms_of_xml ();

#ifdef HAVE_LCMS2
      syms_of_lcms2 ();
#endif

#ifdef HAVE_ZLIB
      syms_of_decompress ();
#endif

      syms_of_menu ();

#ifdef HAVE_NTGUI
      syms_of_w32term ();
      syms_of_w32fns ();
      syms_of_w32menu ();
      syms_of_fontset ();
#endif /* HAVE_NTGUI */

#if defined HAVE_NTGUI || defined CYGWIN
      syms_of_w32cygwinx ();
#endif

#if defined WINDOWSNT || defined HAVE_NTGUI
      syms_of_w32select ();
#endif

#ifdef MSDOS
      syms_of_xmenu ();
      syms_of_dosfns ();
      syms_of_msdos ();
      syms_of_win16select ();
#endif	/* MSDOS */

#ifdef HAVE_NS
      syms_of_nsterm ();
      syms_of_nsfns ();
      syms_of_nsmenu ();
      syms_of_nsselect ();
      syms_of_fontset ();
#endif /* HAVE_NS */

      syms_of_gnutls ();

#ifdef HAVE_INOTIFY
      syms_of_inotify ();
#endif /* HAVE_INOTIFY */

#ifdef HAVE_KQUEUE
      syms_of_kqueue ();
#endif /* HAVE_KQUEUE */

#ifdef HAVE_GFILENOTIFY
      syms_of_gfilenotify ();
#endif /* HAVE_GFILENOTIFY */

#ifdef HAVE_DBUS
      syms_of_dbusbind ();
#endif /* HAVE_DBUS */

#ifdef WINDOWSNT
      syms_of_ntterm ();
#ifdef HAVE_W32NOTIFY
      syms_of_w32notify ();
#endif /* HAVE_W32NOTIFY */
#endif /* WINDOWSNT */

      syms_of_threads ();
      syms_of_profiler ();

#ifdef HAVE_JSON
      syms_of_json ();
#endif

      keys_of_casefiddle ();
      keys_of_cmds ();
      keys_of_buffer ();
      keys_of_keyboard ();
      keys_of_keymap ();
      keys_of_window ();
    }
  else
    {
      /* Initialization that must be done even if the global variable
	 initialized is non zero.  */
#ifdef HAVE_NTGUI
      globals_of_w32font ();
      globals_of_w32fns ();
      globals_of_w32menu ();
#endif  /* HAVE_NTGUI */

#if defined WINDOWSNT || defined HAVE_NTGUI
      globals_of_w32select ();
#endif
    }

  init_charset ();

  /* This calls putenv and so must precede init_process_emacs.  Also,
     it sets Voperating_system_release, which init_process_emacs uses.  */
  init_editfns (dumping);

  /* These two call putenv.  */
#ifdef HAVE_DBUS
  init_dbusbind ();
#endif
#ifdef USE_GTK
  init_xterm ();
#endif

  /* This can create a thread that may call getenv, so it must follow
     all calls to putenv and setenv.  Also, this sets up
     add_keyboard_wait_descriptor, which init_display uses.  */
  init_process_emacs (sockfd);

  init_keyboard ();	/* This too must precede init_sys_modes.  */
  if (!noninteractive)
    init_display ();	/* Determine terminal type.  Calls init_sys_modes.  */
#if HAVE_W32NOTIFY
  else
    init_crit ();	/* w32notify.c needs this in batch mode.  */
#endif	/* HAVE_W32NOTIFY */
  init_xdisp ();
#ifdef HAVE_WINDOW_SYSTEM
  init_fringe ();
#endif /* HAVE_WINDOW_SYSTEM */
  init_macros ();
  init_window ();
  init_font ();

  if (!initialized)
    {
      char *file;
      /* Handle -l loadup, args passed by Makefile.  */
      if (argmatch (argv, argc, "-l", "--load", 3, &file, &skip_args))
	{
#ifdef WINDOWSNT
	  char file_utf8[MAX_UTF8_PATH];

	  if (filename_from_ansi (file, file_utf8) == 0)
	    file = file_utf8;
#endif
	  Vtop_level = list2 (Qload, build_unibyte_string (file));
	}
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (! no_loadup)
	Vtop_level = list2 (Qload, build_string ("loadup.el"));
    }

  /* Set up for profiling.  This is known to work on FreeBSD,
     GNU/Linux and MinGW.  It might work on some other systems too.
     Give it a try and tell us if it works on your system.  To compile
     for profiling, use the configure option --enable-profiling.  */
#if defined (__FreeBSD__) || defined (GNU_LINUX) || defined (__MINGW32__)
#ifdef PROFILING
  if (initialized)
    {
#ifdef __MINGW32__
      extern unsigned char etext asm ("etext");
#else
      extern char etext;
#endif

      atexit (_mcleanup);
      monstartup ((uintptr_t) __executable_start, (uintptr_t) &etext);
    }
  else
    moncontrol (0);
#endif
#endif

  initialized = 1;

  /* Enter editor command loop.  This never returns.  */
  Frecursive_edit ();
  /* NOTREACHED */
  return 0;
}

/* Sort the args so we can find the most important ones
   at the beginning of argv.  */

/* First, here's a table of all the standard options.  */

struct standard_args
{
  const char *name;
  const char *longname;
  int priority;
  int nargs;
};

static const struct standard_args standard_args[] =
{
  { "-version", "--version", 150, 0 },
  { "-chdir", "--chdir", 130, 1 },
  { "-t", "--terminal", 120, 1 },
  { "-nw", "--no-window-system", 110, 0 },
  { "-nw", "--no-windows", 110, 0 },
  { "-batch", "--batch", 100, 0 },
  { "-script", "--script", 100, 1 },
  { "-daemon", "--daemon", 99, 0 },
  { "-bg-daemon", "--bg-daemon", 99, 0 },
  { "-fg-daemon", "--fg-daemon", 99, 0 },
  { "-help", "--help", 90, 0 },
  { "-nl", "--no-loadup", 70, 0 },
  { "-nsl", "--no-site-lisp", 65, 0 },
  { "-no-build-details", "--no-build-details", 63, 0 },
#ifdef HAVE_MODULES
  { "-module-assertions", "--module-assertions", 62, 0 },
#endif
  /* -d must come last before the options handled in startup.el.  */
  { "-d", "--display", 60, 1 },
  { "-display", 0, 60, 1 },
  /* Now for the options handled in `command-line' (startup.el).  */
  /* (Note that to imply -nsl, -Q is partially handled here.)  */
  { "-Q", "--quick", 55, 0 },
  { "-quick", 0, 55, 0 },
  { "-q", "--no-init-file", 50, 0 },
  { "-no-init-file", 0, 50, 0 },
  { "-no-x-resources", "--no-x-resources", 40, 0 },
  { "-no-site-file", "--no-site-file", 40, 0 },
  { "-u", "--user", 30, 1 },
  { "-user", 0, 30, 1 },
  { "-debug-init", "--debug-init", 20, 0 },
  { "-iconic", "--iconic", 15, 0 },
  { "-D", "--basic-display", 12, 0},
  { "-basic-display", 0, 12, 0},
  { "-nbc", "--no-blinking-cursor", 12, 0 },
  /* Now for the options handled in `command-line-1' (startup.el).  */
  { "-nbi", "--no-bitmap-icon", 10, 0 },
  { "-bg", "--background-color", 10, 1 },
  { "-background", 0, 10, 1 },
  { "-fg", "--foreground-color", 10, 1 },
  { "-foreground", 0, 10, 1 },
  { "-bd", "--border-color", 10, 1 },
  { "-bw", "--border-width", 10, 1 },
  { "-ib", "--internal-border", 10, 1 },
  { "-ms", "--mouse-color", 10, 1 },
  { "-cr", "--cursor-color", 10, 1 },
  { "-fn", "--font", 10, 1 },
  { "-font", 0, 10, 1 },
  { "-fs", "--fullscreen", 10, 0 },
  { "-fw", "--fullwidth", 10, 0 },
  { "-fh", "--fullheight", 10, 0 },
  { "-mm", "--maximized", 10, 0 },
  { "-g", "--geometry", 10, 1 },
  { "-geometry", 0, 10, 1 },
  { "-T", "--title", 10, 1 },
  { "-title", 0, 10, 1 },
  { "-name", "--name", 10, 1 },
  { "-xrm", "--xrm", 10, 1 },
  { "-parent-id", "--parent-id", 10, 1 },
  { "-r", "--reverse-video", 5, 0 },
  { "-rv", 0, 5, 0 },
  { "-reverse", 0, 5, 0 },
  { "-hb", "--horizontal-scroll-bars", 5, 0 },
  { "-vb", "--vertical-scroll-bars", 5, 0 },
  { "-color", "--color", 5, 0},
  { "-no-splash", "--no-splash", 3, 0 },
  { "-no-desktop", "--no-desktop", 3, 0 },
#ifdef HAVE_NS
  { "-NSAutoLaunch", 0, 5, 1 },
  { "-NXAutoLaunch", 0, 5, 1 },
  { "-_NSMachLaunch", 0, 85, 1 },
  { "-MachLaunch", 0, 85, 1 },
  { "-macosx", 0, 85, 0 },
  { "-NSHost", 0, 85, 1 },
#endif
  /* These have the same priority as ordinary file name args,
     so they are not reordered with respect to those.  */
  { "-L", "--directory", 0, 1 },
  { "-directory", 0, 0, 1 },
  { "-l", "--load", 0, 1 },
  { "-load", 0, 0, 1 },
  /* This has no longname, because using --scriptload confuses sort_args,
     because then the --script long option seems to match twice; ie
     you can't have a long option which is a prefix of another long
     option.  In any case, this is entirely an internal option.  */
  { "-scriptload", NULL, 0, 1 },
  { "-f", "--funcall", 0, 1 },
  { "-funcall", 0, 0, 1 },
  { "-eval", "--eval", 0, 1 },
  { "-execute", "--execute", 0, 1 },
  { "-find-file", "--find-file", 0, 1 },
  { "-visit", "--visit", 0, 1 },
  { "-file", "--file", 0, 1 },
  { "-insert", "--insert", 0, 1 },
#ifdef HAVE_NS
  { "-NXOpen", 0, 0, 1 },
  { "-NXOpenTemp", 0, 0, 1 },
  { "-NSOpen", 0, 0, 1 },
  { "-NSOpenTemp", 0, 0, 1 },
  { "-GSFilePath", 0, 0, 1 },
#endif
  /* This should be processed after ordinary file name args and the like.  */
  { "-kill", "--kill", -10, 0 },
};

/* Reorder the elements of ARGV (assumed to have ARGC elements)
   so that the highest priority ones come first.
   Do not change the order of elements of equal priority.
   If an option takes an argument, keep it and its argument together.

   If an option that takes no argument appears more
   than once, eliminate all but one copy of it.  */

static void
sort_args (int argc, char **argv)
{
  char **new = xmalloc (argc * sizeof *new);
  /* For each element of argv,
     the corresponding element of options is:
     0 for an option that takes no arguments,
     1 for an option that takes one argument, etc.
     -1 for an ordinary non-option argument.  */
  int *options = xnmalloc (argc, sizeof *options);
  int *priority = xnmalloc (argc, sizeof *priority);
  int to = 1;
  int incoming_used = 1;
  int from;
  int i;

  /* Categorize all the options,
     and figure out which argv elts are option arguments.  */
  for (from = 1; from < argc; from++)
    {
      options[from] = -1;
      priority[from] = 0;
      if (argv[from][0] == '-')
	{
	  int match;

	  /* If we have found "--", don't consider
	     any more arguments as options.  */
	  if (argv[from][1] == '-' && argv[from][2] == 0)
	    {
	      /* Leave the "--", and everything following it, at the end.  */
	      for (; from < argc; from++)
		{
		  priority[from] = -100;
		  options[from] = -1;
		}
	      break;
	    }

	  /* Look for a match with a known old-fashioned option.  */
	  for (i = 0; i < ARRAYELTS (standard_args); i++)
	    if (!strcmp (argv[from], standard_args[i].name))
	      {
		options[from] = standard_args[i].nargs;
		priority[from] = standard_args[i].priority;
		if (from + standard_args[i].nargs >= argc)
		  fatal ("Option '%s' requires an argument\n", argv[from]);
		from += standard_args[i].nargs;
		goto done;
	      }

	  /* Look for a match with a known long option.
	     MATCH is -1 if no match so far, -2 if two or more matches so far,
	     >= 0 (the table index of the match) if just one match so far.  */
	  if (argv[from][1] == '-')
	    {
	      char const *equals = strchr (argv[from], '=');
	      ptrdiff_t thislen =
		equals ? equals - argv[from] : strlen (argv[from]);

	      match = -1;

	      for (i = 0; i < ARRAYELTS (standard_args); i++)
		if (standard_args[i].longname
		    && !strncmp (argv[from], standard_args[i].longname,
				 thislen))
		  {
		    if (match == -1)
		      match = i;
		    else
		      match = -2;
		  }

	      /* If we found exactly one match, use that.  */
	      if (match >= 0)
		{
		  options[from] = standard_args[match].nargs;
		  priority[from] = standard_args[match].priority;
		  /* If --OPTION=VALUE syntax is used,
		     this option uses just one argv element.  */
		  if (equals != 0)
		    options[from] = 0;
		  if (from + options[from] >= argc)
		    fatal ("Option '%s' requires an argument\n", argv[from]);
		  from += options[from];
		}
	      else if (match == -2)
		{
		  /* This is an internal error.
		     Eg if one long option is a prefix of another.  */
		  fprintf (stderr, "Option '%s' matched multiple standard arguments\n", argv[from]);
		}
	      /* Should we not also warn if there was no match?	 */
	    }
	done: ;
	}
    }

  /* Copy the arguments, in order of decreasing priority, to NEW.  */
  new[0] = argv[0];
  while (incoming_used < argc)
    {
      int best = -1;
      int best_priority = -9999;

      /* Find the highest priority remaining option.
	 If several have equal priority, take the first of them.  */
      for (from = 1; from < argc; from++)
	{
	  if (argv[from] != 0 && priority[from] > best_priority)
	    {
	      best_priority = priority[from];
	      best = from;
	    }
	  /* Skip option arguments--they are tied to the options.  */
	  if (options[from] > 0)
	    from += options[from];
	}

      if (best < 0)
	emacs_abort ();

      /* Copy the highest priority remaining option, with its args, to NEW.
         Unless it is a duplicate of the previous one.  */
      if (! (options[best] == 0
	     && ! strcmp (new[to - 1], argv[best])))
	{
	  new[to++] = argv[best];
	  for (i = 0; i < options[best]; i++)
	    new[to++] = argv[best + i + 1];
	}

      incoming_used += 1 + (options[best] > 0 ? options[best] : 0);

      /* Clear out this option in ARGV.  */
      argv[best] = 0;
      for (i = 0; i < options[best]; i++)
	argv[best + i + 1] = 0;
    }

  /* If duplicate options were deleted, fill up extra space with null ptrs.  */
  while (to < argc)
    new[to++] = 0;

  memcpy (argv, new, sizeof (char *) * argc);

  xfree (options);
  xfree (new);
  xfree (priority);
}

DEFUN ("kill-emacs", Fkill_emacs, Skill_emacs, 0, 1, "P",
       doc: /* Exit the Emacs job and kill it.
If ARG is an integer, return ARG as the exit program code.
If ARG is a string, stuff it as keyboard input.

This function is called upon receipt of the signals SIGTERM
or SIGHUP, and upon SIGINT in batch mode.

The value of `kill-emacs-hook', if not void,
is a list of functions (of no args),
all of which are called before Emacs is actually killed.  */
       attributes: noreturn)
  (Lisp_Object arg)
{
  int exit_code;

  /* Fsignal calls emacs_abort () if it sees that waiting_for_input is
     set.  */
  waiting_for_input = 0;
  if (noninteractive)
    safe_run_hooks (Qkill_emacs_hook);
  else
    run_hook (Qkill_emacs_hook);

#ifdef HAVE_X_WINDOWS
  /* Transfer any clipboards we own to the clipboard manager.  */
  x_clipboard_manager_save_all ();
#endif

  shut_down_emacs (0, (STRINGP (arg) && !feof (stdin)) ? arg : Qnil);

#ifdef HAVE_NS
  ns_release_autorelease_pool (ns_pool);
#endif

  /* If we have an auto-save list file,
     kill it because we are exiting Emacs deliberately (not crashing).
     Do it after shut_down_emacs, which does an auto-save.  */
  if (STRINGP (Vauto_save_list_file_name))
    {
      Lisp_Object listfile;
      listfile = Fexpand_file_name (Vauto_save_list_file_name, Qnil);
      unlink (SSDATA (listfile));
    }

  if (INTEGERP (arg))
    exit_code = (XINT (arg) < 0
		 ? XINT (arg) | INT_MIN
		 : XINT (arg) & INT_MAX);
  else
    exit_code = EXIT_SUCCESS;
  exit (exit_code);
}


/* Perform an orderly shutdown of Emacs.  Autosave any modified
   buffers, kill any child processes, clean up the terminal modes (if
   we're in the foreground), and other stuff like that.  Don't perform
   any redisplay; this may be called when Emacs is shutting down in
   the background, or after its X connection has died.

   If SIG is a signal number, print a message for it.

   This is called by fatal signal handlers, X protocol error handlers,
   and Fkill_emacs.  */

void
shut_down_emacs (int sig, Lisp_Object stuff)
{
  /* Prevent running of hooks from now on.  */
  Vrun_hooks = Qnil;

  /* Don't update display from now on.  */
  Vinhibit_redisplay = Qt;

  /* If we are controlling the terminal, reset terminal modes.  */
#ifndef DOS_NT
  {
    pid_t pgrp = getpgrp ();
    pid_t tpgrp = tcgetpgrp (0);
    if ((tpgrp != -1) && tpgrp == pgrp)
      {
	reset_all_sys_modes ();
	if (sig && sig != SIGTERM)
	  {
	    static char const format[] = "Fatal error %d: ";
	    char buf[sizeof format - 2 + INT_STRLEN_BOUND (int)];
	    int buflen = sprintf (buf, format, sig);
	    char const *sig_desc = safe_strsignal (sig);
	    emacs_write (STDERR_FILENO, buf, buflen);
	    emacs_write (STDERR_FILENO, sig_desc, strlen (sig_desc));
	  }
      }
  }
#else
  fflush (stdout);
  reset_all_sys_modes ();
#endif

  stuff_buffered_input (stuff);

  inhibit_sentinels = 1;
  kill_buffer_processes (Qnil);
  Fdo_auto_save (Qt, Qnil);

  unlock_all_files ();

  /* There is a tendency for a SIGIO signal to arrive within exit,
     and cause a SIGHUP because the input descriptor is already closed.  */
  unrequest_sigio ();

  /* Do this only if terminating normally, we want glyph matrices
     etc. in a core dump.  */
  if (sig == 0 || sig == SIGTERM)
    {
      check_glyph_memory ();
      check_message_stack ();
    }

#ifdef MSDOS
  dos_cleanup ();
#endif

#ifdef HAVE_NS
  ns_term_shutdown (sig);
#endif

#ifdef HAVE_LIBXML2
  xml_cleanup_parser ();
#endif

#ifdef WINDOWSNT
  term_ntproc (0);
#endif
}



#ifndef CANNOT_DUMP

#include "unexec.h"

DEFUN ("dump-emacs", Fdump_emacs, Sdump_emacs, 2, 2, 0,
       doc: /* Dump current state of Emacs into executable file FILENAME.
Take symbols from SYMFILE (presumably the file you executed to run Emacs).
This is used in the file `loadup.el' when building Emacs.

You must run Emacs in batch mode in order to dump it.  */)
  (Lisp_Object filename, Lisp_Object symfile)
{
  Lisp_Object tem;
  Lisp_Object symbol;
  ptrdiff_t count = SPECPDL_INDEX ();

  check_pure_size ();

  if (! noninteractive)
    error ("Dumping Emacs works only in batch mode");

  if (!might_dump)
    error ("Emacs can be dumped only once");

#if defined GNU_LINUX && !defined CANNOT_DUMP

  /* Warn if the gap between BSS end and heap start is larger than this.  */
# define MAX_HEAP_BSS_DIFF (1024*1024)

  if (heap_bss_diff > MAX_HEAP_BSS_DIFF)
    {
      fprintf (stderr, "**************************************************\n");
      fprintf (stderr, "Warning: Your system has a gap between BSS and the\n");
      fprintf (stderr, "heap (%"pMu" bytes).  This usually means that exec-shield\n",
               heap_bss_diff);
      fprintf (stderr, "or something similar is in effect.  The dump may\n");
      fprintf (stderr, "fail because of this.  See the section about\n");
      fprintf (stderr, "exec-shield in etc/PROBLEMS for more information.\n");
      fprintf (stderr, "**************************************************\n");
    }
#endif /* GNU_LINUX */

  /* Bind `command-line-processed' to nil before dumping,
     so that the dumped Emacs will process its command line
     and set up to work with X windows if appropriate.  */
  symbol = intern ("command-line-processed");
  specbind (symbol, Qnil);

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);
  filename = ENCODE_FILE (filename);
  if (!NILP (symfile))
    {
      CHECK_STRING (symfile);
      if (SCHARS (symfile))
	{
	  symfile = Fexpand_file_name (symfile, Qnil);
	  symfile = ENCODE_FILE (symfile);
	}
    }

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

#ifdef HYBRID_MALLOC
  {
    static char const fmt[] = "%d of %d static heap bytes used";
    char buf[sizeof fmt + 2 * (INT_STRLEN_BOUND (int) - 2)];
    int max_usage = max_bss_sbrk_ptr - bss_sbrk_buffer;
    sprintf (buf, fmt, max_usage, STATIC_HEAP_SIZE);
    /* Don't log messages, because at this point buffers cannot be created.  */
    message1_nolog (buf);
  }
#endif

  fflush_unlocked (stdout);
  /* Tell malloc where start of impure now is.  */
  /* Also arrange for warnings when nearly out of space.  */
#if !defined SYSTEM_MALLOC && !defined HYBRID_MALLOC
#ifndef WINDOWSNT
  /* On Windows, this was done before dumping, and that once suffices.
     Meanwhile, my_edata is not valid on Windows.  */
  memory_warnings (my_edata, malloc_warning);
#endif /* not WINDOWSNT */
#endif /* not SYSTEM_MALLOC and not HYBRID_MALLOC */

  alloc_unexec_pre ();

  unexec (SSDATA (filename), !NILP (symfile) ? SSDATA (symfile) : 0);

  alloc_unexec_post ();

#ifdef WINDOWSNT
  Vlibrary_cache = Qnil;
#endif
#ifdef HAVE_WINDOW_SYSTEM
  reset_image_types ();
#endif

  Vpurify_flag = tem;

  return unbind_to (count, Qnil);
}

#endif /* not CANNOT_DUMP */

#if HAVE_SETLOCALE
/* Recover from setlocale (LC_ALL, "").  */
void
fixup_locale (void)
{
  /* The Emacs Lisp reader needs LC_NUMERIC to be "C",
     so that numbers are read and printed properly for Emacs Lisp.  */
  setlocale (LC_NUMERIC, "C");
}

/* Set system locale CATEGORY, with previous locale *PLOCALE, to
   DESIRED_LOCALE.  */
static void
synchronize_locale (int category, Lisp_Object *plocale, Lisp_Object desired_locale)
{
  if (! EQ (*plocale, desired_locale))
    {
      *plocale = desired_locale;
#ifdef WINDOWSNT
      /* Changing categories like LC_TIME usually requires specifying
	 an encoding suitable for the new locale, but MS-Windows's
	 'setlocale' will only switch the encoding when LC_ALL is
	 specified.  So we ignore CATEGORY, use LC_ALL instead, and
	 then restore LC_NUMERIC to "C", so reading and printing
	 numbers is unaffected.  */
      setlocale (LC_ALL, (STRINGP (desired_locale)
			  ? SSDATA (desired_locale)
			  : ""));
      fixup_locale ();
#else  /* !WINDOWSNT */
      setlocale (category, (STRINGP (desired_locale)
			    ? SSDATA (desired_locale)
			    : ""));
#endif	/* !WINDOWSNT */
    }
}

/* Set system time locale to match Vsystem_time_locale, if possible.  */
void
synchronize_system_time_locale (void)
{
  synchronize_locale (LC_TIME, &Vprevious_system_time_locale,
		      Vsystem_time_locale);
}

/* Set system messages locale to match Vsystem_messages_locale, if
   possible.  */
void
synchronize_system_messages_locale (void)
{
#ifdef LC_MESSAGES
  synchronize_locale (LC_MESSAGES, &Vprevious_system_messages_locale,
		      Vsystem_messages_locale);
#endif
}
#endif /* HAVE_SETLOCALE */

/* Return a diagnostic string for ERROR_NUMBER, in the wording
   and encoding appropriate for the current locale.  */
char *
emacs_strerror (int error_number)
{
  synchronize_system_messages_locale ();
  return strerror (error_number);
}


Lisp_Object
decode_env_path (const char *evarname, const char *defalt, bool empty)
{
  const char *path, *p;
  Lisp_Object lpath, element, tem;
  /* Default is to use "." for empty path elements.
     But if argument EMPTY is true, use nil instead.  */
  Lisp_Object empty_element = empty ? Qnil : build_string (".");
#ifdef WINDOWSNT
  bool defaulted = 0;
  static const char *emacs_dir_env = "%emacs_dir%/";
  const size_t emacs_dir_len = strlen (emacs_dir_env);
  const char *edir = egetenv ("emacs_dir");
  char emacs_dir[MAX_UTF8_PATH];

  /* egetenv looks in process-environment, which holds the variables
     in their original system-locale encoding.  We need emacs_dir to
     be in UTF-8.  */
  if (edir)
    filename_from_ansi (edir, emacs_dir);
#endif

  /* It's okay to use getenv here, because this function is only used
     to initialize variables when Emacs starts up, and isn't called
     after that.  */
  if (evarname != 0)
    path = getenv (evarname);
  else
    path = 0;
  if (!path)
    {
      path = defalt;
#ifdef WINDOWSNT
      defaulted = 1;
#endif
    }
#ifdef DOS_NT
  /* Ensure values from the environment use the proper directory separator.  */
  if (path)
    {
      char *path_copy;

#ifdef WINDOWSNT
      char *path_utf8, *q, *d;
      int cnv_result;

      /* Convert each element of PATH to UTF-8.  */
      p = path_copy = alloca (strlen (path) + 1);
      strcpy (path_copy, path);
      d = path_utf8 = alloca (4 * strlen (path) + 1);
      *d = '\0';
      do {
	q = _mbschr (p, SEPCHAR);
	if (q)
	  *q = '\0';
	cnv_result = filename_from_ansi (p, d);
	if (q)
	  {
	    *q++ = SEPCHAR;
	    p = q;
	    /* If conversion of this PATH elements fails, make sure
	       destination pointer will stay put, thus effectively
	       ignoring the offending element.  */
	    if (cnv_result == 0)
	      {
		d += strlen (d);
		*d++ = SEPCHAR;
	      }
	  }
	else if (cnv_result != 0 && d > path_utf8)
	  d[-1] = '\0';	/* remove last semi-colon and null-terminate PATH */
      } while (q);
      path_copy = path_utf8;
#else  /* MSDOS */
      path_copy = alloca (strlen (path) + 1);
      strcpy (path_copy, path);
#endif
      dostounix_filename (path_copy);
      path = path_copy;
    }
#endif
  lpath = Qnil;
  while (1)
    {
      p = strchr (path, SEPCHAR);
      if (!p)
	p = path + strlen (path);
      element = ((p - path) ? make_unibyte_string (path, p - path)
		 : empty_element);
      if (! NILP (element))
        {
#ifdef WINDOWSNT
          /* Relative file names in the default path are interpreted as
             being relative to $emacs_dir.  */
          if (edir && defaulted
              && strncmp (path, emacs_dir_env, emacs_dir_len) == 0)
            element = Fexpand_file_name (Fsubstring
                                         (element,
                                          make_number (emacs_dir_len),
                                          Qnil),
                                         build_unibyte_string (emacs_dir));
#endif

          /* Add /: to the front of the name
             if it would otherwise be treated as magic.  */
          tem = Ffind_file_name_handler (element, Qt);

          /* However, if the handler says "I'm safe",
             don't bother adding /:.  */
          if (SYMBOLP (tem))
            {
              Lisp_Object prop;
              prop = Fget (tem, intern ("safe-magic"));
              if (! NILP (prop))
                tem = Qnil;
            }

          if (! NILP (tem))
	    {
	      AUTO_STRING (slash_colon, "/:");
	      element = concat2 (slash_colon, element);
	    }
        } /* !NILP (element) */

      lpath = Fcons (element, lpath);
      if (*p)
	path = p + 1;
      else
	break;
    }
  return Fnreverse (lpath);
}

DEFUN ("daemonp", Fdaemonp, Sdaemonp, 0, 0, 0,
       doc: /* Return non-nil if the current emacs process is a daemon.
If the daemon was given a name argument, return that name. */)
  (void)
{
  if (IS_DAEMON)
    if (daemon_name)
      return build_string (daemon_name);
    else
      return Qt;
  else
    return Qnil;
}

DEFUN ("daemon-initialized", Fdaemon_initialized, Sdaemon_initialized, 0, 0, 0,
       doc: /* Mark the Emacs daemon as being initialized.
This finishes the daemonization process by doing the other half of detaching
from the parent process and its tty file descriptors.  */)
  (void)
{
  bool err = 0;

  if (!IS_DAEMON)
    error ("This function can only be called if emacs is run as a daemon");

  if (!DAEMON_RUNNING)
    error ("The daemon has already been initialized");

  if (NILP (Vafter_init_time))
    error ("This function can only be called after loading the init files");
#ifndef WINDOWSNT

  if (daemon_type == 2)
    {
      int nfd;

      /* Get rid of stdin, stdout and stderr.  */
      nfd = emacs_open ("/dev/null", O_RDWR, 0);
      err |= nfd < 0;
      err |= dup2 (nfd, STDIN_FILENO) < 0;
      err |= dup2 (nfd, STDOUT_FILENO) < 0;
      err |= dup2 (nfd, STDERR_FILENO) < 0;
      err |= emacs_close (nfd) != 0;

      /* Closing the pipe will notify the parent that it can exit.
         FIXME: In case some other process inherited the pipe, closing it here
         won't notify the parent because it's still open elsewhere, so we
         additionally send a byte, just to make sure the parent really exits.
         Instead, we should probably close the pipe in start-process and
         call-process to make sure the pipe is never inherited by
         subprocesses.  */
      err |= write (daemon_pipe[1], "\n", 1) < 0;
      err |= emacs_close (daemon_pipe[1]) != 0;
    }

  /* Set it to an invalid value so we know we've already run this function.  */
  daemon_type = -1;

#else  /* WINDOWSNT */
  /* Signal the waiting emacsclient process.  */
  err |= SetEvent (w32_daemon_event) == 0;
  err |= CloseHandle (w32_daemon_event) == 0;
  /* Set it to an invalid value so we know we've already run this function.  */
  w32_daemon_event = INVALID_HANDLE_VALUE;
#endif

  if (err)
    error ("I/O error during daemon initialization");
  return Qt;
}

void
syms_of_emacs (void)
{
  DEFSYM (Qfile_name_handler_alist, "file-name-handler-alist");
  DEFSYM (Qrisky_local_variable, "risky-local-variable");
  DEFSYM (Qkill_emacs, "kill-emacs");
  DEFSYM (Qkill_emacs_hook, "kill-emacs-hook");

#ifndef CANNOT_DUMP
  defsubr (&Sdump_emacs);
#endif

  defsubr (&Skill_emacs);

  defsubr (&Sinvocation_name);
  defsubr (&Sinvocation_directory);
  defsubr (&Sdaemonp);
  defsubr (&Sdaemon_initialized);

  DEFVAR_LISP ("command-line-args", Vcommand_line_args,
	       doc: /* Args passed by shell to Emacs, as a list of strings.
Many arguments are deleted from the list as they are processed.  */);

  DEFVAR_LISP ("system-type", Vsystem_type,
	       doc: /* The value is a symbol indicating the type of operating system you are using.
Special values:
  `gnu'          compiled for a GNU Hurd system.
  `gnu/linux'    compiled for a GNU/Linux system.
  `gnu/kfreebsd' compiled for a GNU system with a FreeBSD kernel.
  `darwin'       compiled for Darwin (GNU-Darwin, macOS, ...).
  `ms-dos'       compiled as an MS-DOS application.
  `windows-nt'   compiled as a native W32 application.
  `cygwin'       compiled using the Cygwin library.
Anything else (in Emacs 26, the possibilities are: aix, berkeley-unix,
hpux, usg-unix-v) indicates some sort of Unix system.  */);
  Vsystem_type = intern_c_string (SYSTEM_TYPE);
  /* See configure.ac for the possible SYSTEM_TYPEs.  */

  DEFVAR_LISP ("system-configuration", Vsystem_configuration,
	       doc: /* Value is string indicating configuration Emacs was built for.  */);
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

  DEFVAR_LISP ("system-configuration-options", Vsystem_configuration_options,
	       doc: /* String containing the configuration options Emacs was built with.  */);
  Vsystem_configuration_options = build_string (EMACS_CONFIG_OPTIONS);

  DEFVAR_LISP ("system-configuration-features", Vsystem_configuration_features,
	       doc: /* String listing some of the main features this Emacs was compiled with.
An element of the form \"FOO\" generally means that HAVE_FOO was
defined during the build.

This is mainly intended for diagnostic purposes in bug reports.
Don't rely on it for testing whether a feature you want to use is available.  */);
  Vsystem_configuration_features = build_string (EMACS_CONFIG_FEATURES);

  DEFVAR_BOOL ("noninteractive", noninteractive1,
	       doc: /* Non-nil means Emacs is running without interactive terminal.  */);

  DEFVAR_LISP ("kill-emacs-hook", Vkill_emacs_hook,
	       doc: /* Hook run when `kill-emacs' is called.
Since `kill-emacs' may be invoked when the terminal is disconnected (or
in other similar situations), functions placed on this hook should not
expect to be able to interact with the user.  To ask for confirmation,
see `kill-emacs-query-functions' instead.

Before Emacs 24.1, the hook was not run in batch mode, i.e., if
`noninteractive' was non-nil.  */);
  Vkill_emacs_hook = Qnil;

  DEFVAR_LISP ("path-separator", Vpath_separator,
	       doc: /* String containing the character that separates directories in
search paths, such as PATH and other similar environment variables.  */);
  {
    char c = SEPCHAR;
    Vpath_separator = make_string (&c, 1);
  }

  DEFVAR_LISP ("invocation-name", Vinvocation_name,
	       doc: /* The program name that was used to run Emacs.
Any directory names are omitted.  */);

  DEFVAR_LISP ("invocation-directory", Vinvocation_directory,
	       doc: /* The directory in which the Emacs executable was found, to run it.
The value is nil if that directory's name is not known.  */);

  DEFVAR_LISP ("installation-directory", Vinstallation_directory,
	       doc: /* A directory within which to look for the `lib-src' and `etc' directories.
In an installed Emacs, this is normally nil.  It is non-nil if
both `lib-src' (on MS-DOS, `info') and `etc' directories are found
within the variable `invocation-directory' or its parent.  For example,
this is the case when running an uninstalled Emacs executable from its
build directory.  */);
  Vinstallation_directory = Qnil;

  DEFVAR_LISP ("system-messages-locale", Vsystem_messages_locale,
	       doc: /* System locale for messages.  */);
  Vsystem_messages_locale = Qnil;

  DEFVAR_LISP ("previous-system-messages-locale",
	       Vprevious_system_messages_locale,
	       doc: /* Most recently used system locale for messages.  */);
  Vprevious_system_messages_locale = Qnil;

  DEFVAR_LISP ("system-time-locale", Vsystem_time_locale,
	       doc: /* System locale for time.  */);
  Vsystem_time_locale = Qnil;

  DEFVAR_LISP ("previous-system-time-locale", Vprevious_system_time_locale,
	       doc: /* Most recently used system locale for time.  */);
  Vprevious_system_time_locale = Qnil;

  DEFVAR_LISP ("before-init-time", Vbefore_init_time,
	       doc: /* Value of `current-time' before Emacs begins initialization.  */);
  Vbefore_init_time = Qnil;

  DEFVAR_LISP ("after-init-time", Vafter_init_time,
	       doc: /* Value of `current-time' after loading the init files.
This is nil during initialization.  */);
  Vafter_init_time = Qnil;

  DEFVAR_BOOL ("inhibit-x-resources", inhibit_x_resources,
	       doc: /* If non-nil, X resources, Windows Registry settings, and NS defaults are not used.  */);
  inhibit_x_resources = 0;

  DEFVAR_LISP ("emacs-copyright", Vemacs_copyright,
	       doc: /* Short copyright string for this version of Emacs.  */);
  Vemacs_copyright = build_string (emacs_copyright);

  DEFVAR_LISP ("emacs-version", Vemacs_version,
	       doc: /* Version numbers of this version of Emacs.
This has the form: MAJOR.MINOR[.MICRO], where MAJOR/MINOR/MICRO are integers.
MICRO is only present in unreleased development versions,
and is not especially meaningful.  Prior to Emacs 26.1, an extra final
component .BUILD is present.  This is now stored separately in
`emacs-build-number'.  */);
  Vemacs_version = build_string (emacs_version);

  DEFVAR_LISP ("report-emacs-bug-address", Vreport_emacs_bug_address,
	       doc: /* Address of mailing list for GNU Emacs bugs.  */);
  Vreport_emacs_bug_address = build_string (emacs_bugreport);

  DEFVAR_LISP ("dynamic-library-alist", Vdynamic_library_alist,
    doc: /* Alist of dynamic libraries vs external files implementing them.
Each element is a list (LIBRARY FILE...), where the car is a symbol
representing a supported external library, and the rest are strings giving
alternate filenames for that library.

Emacs tries to load the library from the files in the order they appear on
the list; if none is loaded, the running session of Emacs won't have access
to that library.

Note that image types `pbm' and `xbm' do not need entries in this variable
because they do not depend on external libraries and are always available.

Also note that this is not a generic facility for accessing external
libraries; only those already known by Emacs will be loaded.  */);
  Vdynamic_library_alist = Qnil;
  Fput (intern_c_string ("dynamic-library-alist"), Qrisky_local_variable, Qt);

#ifdef WINDOWSNT
  Vlibrary_cache = Qnil;
  staticpro (&Vlibrary_cache);
#endif
}
