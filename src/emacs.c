/* Fully extensible Emacs, running on Unix, intended for GNU.
   Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <signal.h>
#include <errno.h>

#include <config.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/file.h>

#ifdef VMS
#include <ssdef.h>
#endif

#ifdef BSD
#include <sys/ioctl.h>
#endif

#include "lisp.h"
#include "commands.h"
#include "intervals.h"

#include "systty.h"
#include "syssignal.h"
#include "process.h"

#ifndef O_RDWR
#define O_RDWR 2
#endif

extern void malloc_warning ();
extern char *index ();
extern char *strerror ();

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* The name under which Emacs was invoked, with any leading directory
   names discarded.  */
Lisp_Object Vinvocation_name;

/* The directory name from which Emacs was invoked.  */
Lisp_Object Vinvocation_directory;

/* The directory name in which to find subdirs such as lisp and etc.
   nil means get them only from PATH_LOADSEARCH.  */
Lisp_Object Vinstallation_directory;

/* Hook run by `kill-emacs' before it does really anything.  */
Lisp_Object Vkill_emacs_hook;

/* Set nonzero after Emacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

/* Variable whose value is symbol giving operating system type.  */
Lisp_Object Vsystem_type;

/* Variable whose value is string giving configuration built for.  */
Lisp_Object Vsystem_configuration;

/* Variable whose value is string giving configuration options,
   for use when reporting bugs.  */
Lisp_Object Vsystem_configuration_options;

/* If non-zero, emacs should not attempt to use an window-specific code,
   but instead should use the virtual terminal under which it was started */
int inhibit_window_system;

/* If nonzero, set Emacs to run at this priority.  This is also used
   in child_setup and sys_suspend to make sure subshells run at normal
   priority; Those functions have their own extern declaration.  */
int emacs_priority;

/* If non-zero a filter or a sentinel is running.  Tested to save the match
   data on the first attempt to change it inside asynchronous code. */
int running_asynch_code;

#ifdef BSD_PGRPS
/* See sysdep.c.  */
extern int inherited_pgroup;
#endif

#ifdef HAVE_X_WINDOWS
/* If non-zero, -d was specified, meaning we're using some window system. */
int display_arg;
#endif

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;

#ifdef HAVE_X_WINDOWS
extern Lisp_Object Vwindow_system;
#endif /* HAVE_X_WINDOWS */

extern Lisp_Object Vauto_save_list_file_name;

#ifdef USG_SHARED_LIBRARIES
/* If nonzero, this is the place to put the end of the writable segment
   at startup.  */

unsigned int bss_end = 0;
#endif

/* Nonzero means running Emacs without interactive terminal.  */

int noninteractive;

/* Value of Lisp variable `noninteractive'.
   Normally same as C variable `noninteractive'
   but nothing terrible happens if user sets this one.  */

int noninteractive1;

/* Save argv and argc.  */
char **initial_argv;
int initial_argc;

static void sort_args ();

/* Signal code for the fatal signal that was received */
int fatal_error_code;

/* Nonzero if handling a fatal error already */
int fatal_error_in_progress;

/* Handle bus errors, illegal instruction, etc. */
SIGTYPE
fatal_error_signal (sig)
     int sig;
{
  fatal_error_code = sig;
  signal (sig, SIG_DFL);

  /* If fatal error occurs in code below, avoid infinite recursion.  */
  if (! fatal_error_in_progress)
    {
      fatal_error_in_progress = 1;

      shut_down_emacs (sig, 0, Qnil);
    }

#ifdef VMS
  LIB$STOP (SS$_ABORT);
#else
  /* Signal the same code; this time it will really be fatal.
     Remember that since we're in a signal handler, the signal we're
     going to send is probably blocked, so we have to unblock it if we
     want to really receive it.  */
#ifndef MSDOS
  sigunblock (sigmask (fatal_error_code));
#endif
  kill (getpid (), fatal_error_code);
#endif /* not VMS */
}

#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
SIGTYPE
memory_warning_signal (sig)
     int sig;
{
  signal (sig, memory_warning_signal);

  malloc_warning ("Operating system warns that virtual memory is running low.\n");

  /* It might be unsafe to call do_auto_save now.  */
  force_auto_save_soon ();
}
#endif

/* Code for dealing with Lisp access to the Unix command line */

static
init_cmdargs (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  register int i;
  Lisp_Object name, dir;

  initial_argv = argv;
  initial_argc = argc;

  Vinvocation_name = Ffile_name_nondirectory (build_string (argv[0]));
  Vinvocation_directory = Ffile_name_directory (build_string (argv[0]));
  /* If we got no directory in argv[0], search PATH to find where
     Emacs actually came from.  */
  if (NILP (Vinvocation_directory))
    {
      Lisp_Object found;
      int yes = openp (Vexec_path, Vinvocation_name,
		       EXEC_SUFFIXES, &found, 1);
      if (yes == 1)
	Vinvocation_directory = Ffile_name_directory (found);
    }

  Vinstallation_directory = Qnil;

  if (!NILP (Vinvocation_directory))
    {
      dir = Vinvocation_directory;
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
	  if (!NILP (lib_src_exists))
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
	  if (!NILP (lib_src_exists))
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
	Vcommand_line_args
	  = Fcons (build_string (argv[i]), Vcommand_line_args);
    }
}

DEFUN ("invocation-name", Finvocation_name, Sinvocation_name, 0, 0, 0,
  "Return the program name that was used to run Emacs.\n\
Any directory names are omitted.")
  ()
{
  return Fcopy_sequence (Vinvocation_name);
}

DEFUN ("invocation-directory", Finvocation_directory, Sinvocation_directory,
  0, 0, 0,
  "Return the directory name in which the Emacs executable was located")
  ()
{
  return Fcopy_sequence (Vinvocation_directory);
}


#ifdef VMS
#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
extern noshare char **environ;
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */
#endif /* VMS */

#ifndef ORDINARY_LINK
/* We don't include crtbegin.o and crtend.o in the link,
   so these functions and variables might be missed.
   Provide dummy definitions to avoid error.
   (We don't have any real constructors or destructors.)  */
#ifdef __GNUC__
#ifndef GCC_CTORS_IN_LIBC
__do_global_ctors ()
{}
__do_global_ctors_aux ()
{}
__do_global_dtors ()
{}
/* Linux has a bug in its library; avoid an error.  */
#ifndef LINUX
char * __CTOR_LIST__[2] = { (char *) (-1), 0 };
#endif
char * __DTOR_LIST__[2] = { (char *) (-1), 0 };
#endif /* GCC_CTORS_IN_LIBC */
__main ()
{}
#endif /* __GNUC__ */
#endif /* ORDINARY_LINK */

/* Test whether the next argument in ARGV matches SSTR or a prefix of
   LSTR (at least MINLEN characters).  If so, then if VALPTR is non-null
   (the argument is supposed to have a value) store in *VALPTR either
   the next argument or the portion of this one after the equal sign.
   ARGV is read starting at position *SKIPPTR; this index is advanced
   by the number of arguments used.

   Too bad we can't just use getopt for all of this, but we don't have
   enough information to do it right.  */

static int
argmatch (argv, argc, sstr, lstr, minlen, valptr, skipptr)
     char **argv;
     int argc;
     char *sstr;
     char *lstr;
     int minlen;
     char **valptr;
     int *skipptr;
{
  char *p;
  int arglen;
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
  arglen = (valptr != NULL && (p = index (arg, '=')) != NULL
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

/* ARGSUSED */
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
  char stack_bottom_variable;
  int skip_args = 0;
  extern int errno;
  extern sys_nerr;

  sort_args (argc, argv);

  if (argmatch (argv, argc, "-version", "--version", 3, NULL, &skip_args))
    {
      Lisp_Object tem;
      tem = Fsymbol_value (intern ("emacs-version"));
      if (!STRINGP (tem))
	{
	  fprintf (stderr, "Invalid value of `emacs-version'\n");
	  exit (1);
	}
      else
	{
	  printf ("%s\n", XSTRING (tem)->data);
	  exit (0);
	}
    }

/* Map in shared memory, if we are using that.  */
#ifdef HAVE_SHM
  if (argmatch (argv, argc, "-nl", "--no-shared-memory", 6, NULL, &skip_args))
    {
      map_in_data (0);
      /* The shared memory was just restored, which clobbered this.  */
      skip_args = 1;
    }
  else
    {
      map_in_data (1);
      /* The shared memory was just restored, which clobbered this.  */
      skip_args = 0;
    }
#endif

#ifdef NeXT
  {
    extern int malloc_cookie;
    /* This helps out unexnext.c.  */
    if (initialized)
      if (malloc_jumpstart (malloc_cookie) != 0)
	printf ("malloc jumpstart failed!\n");
  }
#endif /* NeXT */

#ifdef VMS
  /* If -map specified, map the data file in */
  {
    char *file;
    if (argmatch (argv, argc, "-map", "--map-data", 3, &mapin_file, &skip_args))
      mapin_data (file);
  }

#ifdef LINK_CRTL_SHARE
#ifdef SHAREABLE_LIB_BUG
  /* Bletcherous shared libraries! */
  if (!stdin)
    stdin = fdopen (0, "r");
  if (!stdout)
    stdout = fdopen (1, "w");
  if (!stderr)
    stderr = fdopen (2, "w");
  if (!environ)
    environ = envp;
#endif /* SHAREABLE_LIB_BUG */
#endif /* LINK_CRTL_SHARE */
#endif /* VMS */

  /* Record (approximately) where the stack begins.  */
  stack_bottom = &stack_bottom_variable;

#ifdef RUN_TIME_REMAP
  if (initialized)
    run_time_remap (argv[0]);
#endif

#ifdef USG_SHARED_LIBRARIES
  if (bss_end)
    brk ((void *)bss_end);
#endif

  clearerr (stdin);

#ifndef SYSTEM_MALLOC
  if (! initialized)
    {
      /* Arrange to get warning messages as memory fills up.  */
      memory_warnings (0, malloc_warning);

      /* Arrange to disable interrupt input while malloc and friends are
	 running.  */
      uninterrupt_malloc ();
    }
#endif	/* not SYSTEM_MALLOC */

#ifdef MSDOS
  /* We do all file input/output as binary files.  When we need to translate
     newlines, we do that manually.  */
  _fmode = O_BINARY;
  (stdin)->_flag &= ~_IOTEXT;
  (stdout)->_flag &= ~_IOTEXT;
  (stderr)->_flag &= ~_IOTEXT;
#endif /* MSDOS */

#ifdef SET_EMACS_PRIORITY
  if (emacs_priority)
    nice (emacs_priority);
  setuid (getuid ());
#endif /* SET_EMACS_PRIORITY */

#ifdef EXTRA_INITIALIZE
  EXTRA_INITIALIZE;
#endif

  inhibit_window_system = 0;

  /* Handle the -t switch, which specifies filename to use as terminal */
  {
    char *term;
    if (argmatch (argv, argc, "-t", "--terminal", 4, &term, &skip_args))
      {
	int result;
	close (0);
	close (1);
	result = open (term, O_RDWR, 2 );
	if (result < 0)
	  {
	    char *errstring = strerror (errno);
	    fprintf (stderr, "emacs: %s: %s\n", term, errstring);
	    exit (1);
	  }
	dup (0);
	if (! isatty (0))
	  {
	    fprintf (stderr, "emacs: %s: not a tty\n", term);
	    exit (1);
	  }
	fprintf (stderr, "Using %s\n", term);
#ifdef HAVE_X_WINDOWS
	inhibit_window_system = 1; /* -t => -nw */
#endif
      }
  }
  if (argmatch (argv, argc, "-nw", "--no-windows", 6, NULL, &skip_args))
    inhibit_window_system = 1;

  /* Handle the -batch switch, which means don't do interactive display.  */
  noninteractive = 0;
  if (argmatch (argv, argc, "-batch", "--batch", 5, NULL, &skip_args))
    noninteractive = 1;

  /* Handle the --help option, which gives a usage message..  */
  if (argmatch (argv, argc, "-help", "--help", 3, NULL, &skip_args))
    {
      printf ("\
Usage: %s [-t term] [--terminal term]  [-nw] [--no-windows]  [--batch]\n\
      [-q] [--no-init-file]  [-u user] [--user user]  [--debug-init]\n\
\(Arguments above this line must be first; those below may be in any order)\n\
      [-f func] [--funcall func]  [-l file] [--load file]  [--insert file]\n\
      file-to-visit  [--kill]\n", argv[0]);
      exit (0);
    }

#ifdef HAVE_X_WINDOWS
  /* Stupid kludge to catch command-line display spec.  We can't
     handle this argument entirely in window system dependent code
     because we don't even know which window system dependent code
     to run until we've recognized this argument.  */
  {
    char *displayname;
    int i;
    int count_before = skip_args;

    if (argmatch (argv, argc, "-d", "--display", 3, &displayname, &skip_args))
      display_arg = 1;
    else if (argmatch (argv, argc, "-display", 0, 3, &displayname, &skip_args))
      display_arg = 1;

    /* If we have the form --display=NAME,
       convert it into  -d name.
       This requires inserting a new element into argv.  */
    if (displayname != 0 && skip_args - count_before == 1)
      {
	char **new = (char **) xmalloc (sizeof (char *) * (argc + 2));
	int j;

	for (j = 0; j < count_before + 1; j++)
	  new[j] = argv[j];
	new[count_before + 1] = "-d";
	new[count_before + 2] = displayname;
	for (j = count_before + 2; j <argc; j++)
	  new[j + 1] = argv[j];
	argv = new;
	argc++;
      }
    /* Change --display to -d, when its arg is separate.  */
    else if (displayname != 0 && skip_args > count_before
	     && argv[count_before + 1][1] == '-')
      argv[count_before] = "-d";

    /* Don't actually discard this arg.  */
    skip_args = count_before;
  }
#endif

  if (! noninteractive)
    {
#ifdef BSD_PGRPS
      if (initialized)
	{
	  inherited_pgroup = EMACS_GETPGRP (0);
	  setpgrp (0, getpid ());
	}
#else
#if defined (USG5) && defined (INTERRUPT_INPUT)
      setpgrp ();
#endif
#endif
    }

#ifdef POSIX_SIGNALS
  init_signals ();
#endif

  if (
#ifndef CANNOT_DUMP
      ! noninteractive || initialized
#else
      1
#endif
      )
    {
      /* Don't catch these signals in batch mode if not initialized.
	 On some machines, this sets static data that would make
	 signal fail to work right when the dumped Emacs is run.  */
      signal (SIGHUP, fatal_error_signal);
      signal (SIGQUIT, fatal_error_signal);
      signal (SIGILL, fatal_error_signal);
      signal (SIGTRAP, fatal_error_signal);
#ifdef SIGABRT
      signal (SIGABRT, fatal_error_signal);
#endif
#ifdef SIGHWE
      signal (SIGHWE, fatal_error_signal);
#endif
#ifdef SIGPRE
      signal (SIGPRE, fatal_error_signal);
#endif
#ifdef SIGORE
      signal (SIGORE, fatal_error_signal);
#endif
#ifdef SIGUME
      signal (SIGUME, fatal_error_signal);
#endif
#ifdef SIGDLK
      signal (SIGDLK, fatal_error_signal);
#endif
#ifdef SIGCPULIM
      signal (SIGCPULIM, fatal_error_signal);
#endif
#ifdef SIGIOT
      /* This is missing on some systems - OS/2, for example.  */
      signal (SIGIOT, fatal_error_signal);
#endif
#ifdef SIGEMT
      signal (SIGEMT, fatal_error_signal);
#endif
      signal (SIGFPE, fatal_error_signal);
#ifdef SIGBUS
      signal (SIGBUS, fatal_error_signal);
#endif
      signal (SIGSEGV, fatal_error_signal);
#ifdef SIGSYS
      signal (SIGSYS, fatal_error_signal);
#endif
      signal (SIGTERM, fatal_error_signal);
#ifdef SIGXCPU
      signal (SIGXCPU, fatal_error_signal);
#endif
#ifdef SIGXFSZ
      signal (SIGXFSZ, fatal_error_signal);
#endif /* SIGXFSZ */

#ifdef SIGDANGER
      /* This just means available memory is getting low.  */
      signal (SIGDANGER, memory_warning_signal);
#endif

#ifdef AIX
/* 20 is SIGCHLD, 21 is SIGTTIN, 22 is SIGTTOU.  */
      signal (SIGXCPU, fatal_error_signal);
#ifndef _I386
      signal (SIGIOINT, fatal_error_signal);
#endif
      signal (SIGGRANT, fatal_error_signal);
      signal (SIGRETRACT, fatal_error_signal);
      signal (SIGSOUND, fatal_error_signal);
      signal (SIGMSG, fatal_error_signal);
#endif /* AIX */
    }

  noninteractive1 = noninteractive;

/* Perform basic initializations (not merely interning symbols) */

  if (!initialized)
    {
      init_alloc_once ();
      init_obarray ();
      init_eval_once ();
      init_syntax_once ();	/* Create standard syntax table.  */
		      /* Must be done before init_buffer */
      init_casetab_once ();
      init_buffer_once ();	/* Create buffer table and some buffers */
      init_minibuf_once ();	/* Create list of minibuffers */
			      /* Must precede init_window_once */
      init_window_once ();	/* Init the window system */
    }

  init_alloc ();
  init_eval ();
  init_data ();
  running_asynch_code = 0;

#ifdef MSDOS
  /* Call early 'cause init_environment needs it.  */
  init_dosfns ();
  /* Set defaults for several environment variables.  */
  if (initialized) init_environment (argc, argv, skip_args);
#endif

  /* egetenv is a pretty low-level facility, which may get called in
     many circumstances; it seems flimsy to put off initializing it
     until calling init_callproc.  */
  set_process_environment ();
  /* AIX crashes are reported in system versions 3.2.3 and 3.2.4
     if this is not done.  Do it after set_process_environment so that we
     don't pollute Vprocess_environment.  */
#ifdef AIX
  putenv ("LANG=C");
#endif

  init_buffer ();	/* Init default directory of main buffer */

  init_callproc_1 ();	/* Must precede init_cmdargs and init_sys_modes.  */
  init_cmdargs (argc, argv, skip_args);	/* Must precede init_lread.  */
  init_callproc ();	/* Must follow init_cmdargs but not init_sys_modes.  */
  init_lread ();

  if (!noninteractive)
    {
#ifdef VMS
      init_vms_input ();/* init_display calls get_frame_size, that needs this */
#endif /* VMS */
      init_display ();	/* Determine terminal type.  init_sys_modes uses results */
    }
  init_keyboard ();	/* This too must precede init_sys_modes */
#ifdef VMS
  init_vmsproc ();	/* And this too. */
#endif /* VMS */
  init_sys_modes ();	/* Init system terminal modes (RAW or CBREAK, etc.) */
  init_xdisp ();
  init_macros ();
  init_editfns ();
#ifdef LISP_FLOAT_TYPE
  init_floatfns ();
#endif
#ifdef VMS
  init_vmsfns ();
#endif /* VMS */
  init_process ();
#ifdef CLASH_DETECTION
  init_filelock ();
#endif /* CLASH_DETECTION */

/* Intern the names of all standard functions and variables; define standard keys */

  if (!initialized)
    {
      /* The basic levels of Lisp must come first */
      /* And data must come first of all
	 for the sake of symbols like error-message */
      syms_of_data ();
      syms_of_alloc ();
      syms_of_lread ();
      syms_of_print ();
      syms_of_eval ();
      syms_of_fns ();
      syms_of_floatfns ();

      syms_of_abbrev ();
      syms_of_buffer ();
      syms_of_bytecode ();
      syms_of_callint ();
      syms_of_casefiddle ();
      syms_of_casetab ();
      syms_of_callproc ();
      syms_of_cmds ();
#ifndef NO_DIR_LIBRARY
      syms_of_dired ();
#endif /* not NO_DIR_LIBRARY */
      syms_of_display ();
      syms_of_doc ();
      syms_of_editfns ();
      syms_of_emacs ();
      syms_of_fileio ();
#ifdef CLASH_DETECTION
      syms_of_filelock ();
#endif /* CLASH_DETECTION */
      syms_of_indent ();
      syms_of_keyboard ();
      syms_of_keymap ();
      syms_of_macros ();
      syms_of_marker ();
      syms_of_minibuf ();
      syms_of_mocklisp ();
      syms_of_process ();
      syms_of_search ();
      syms_of_frame ();
      syms_of_syntax ();
      syms_of_term ();
      syms_of_undo ();

      /* Only defined if Emacs is compiled with USE_TEXT_PROPERTIES */
      syms_of_textprop ();
#ifdef VMS
      syms_of_vmsproc ();
#endif /* VMS */
      syms_of_window ();
      syms_of_xdisp ();
#ifdef HAVE_X_WINDOWS
      syms_of_xterm ();
      syms_of_xfns ();
      syms_of_xfaces ();
#ifdef HAVE_X11
      syms_of_xselect ();
#endif
#ifdef HAVE_X_MENU
      syms_of_xmenu ();
#endif /* HAVE_X_MENU */
#endif /* HAVE_X_WINDOWS */

#if defined (MSDOS) && !defined (HAVE_X_WINDOWS)
      syms_of_xfaces ();
      syms_of_xmenu ();
#endif

#ifdef SYMS_SYSTEM
      SYMS_SYSTEM;
#endif

#ifdef SYMS_MACHINE
      SYMS_MACHINE;
#endif

      keys_of_casefiddle ();
      keys_of_cmds ();
      keys_of_buffer ();
      keys_of_keyboard ();
      keys_of_keymap ();
      keys_of_macros ();
      keys_of_minibuf ();
      keys_of_window ();
      keys_of_frame ();
    }

  if (!initialized)
    {
      char *file;
      /* Handle -l loadup-and-dump, args passed by Makefile. */
      if (argmatch (argv, argc, "-l", "--load", 3, &file, &skip_args))
	Vtop_level = Fcons (intern ("load"),
			    Fcons (build_string (file), Qnil));
#ifdef CANNOT_DUMP
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (!argmatch (argv, argc, "-nl", "--no-loadup", 6, NULL, &skip_args))
	Vtop_level = Fcons (intern ("load"),
			    Fcons (build_string ("loadup.el"), Qnil));
#endif /* CANNOT_DUMP */
    }

  if (initialized)
    {
      /* Erase any pre-dump messages in the message log, to avoid confusion */
      Lisp_Object old_log_max;
      old_log_max = Vmessage_log_max;
      XSETFASTINT (Vmessage_log_max, 0);
      message_dolog ("", 0, 1);
      Vmessage_log_max = old_log_max;
    }

  initialized = 1;

#if defined (sun) || defined (LOCALTIME_CACHE)
  /* sun's localtime has a bug.  it caches the value of the time
     zone rather than looking it up every time.  Since localtime() is
     called to bolt the undumping time into the undumped emacs, this
     results in localtime ignoring the TZ environment variable.
     This flushes the new TZ value into localtime. */
  tzset ();
#endif /* defined (sun) || defined (LOCALTIME_CACHE) */

  /* Enter editor command loop.  This never returns.  */
  Frecursive_edit ();
  /* NOTREACHED */
}

/* Sort the args so we can find the most important ones
   at the beginning of argv.  */

/* First, here's a table of all the standard options.  */

struct standard_args
{
  char *name;
  char *longname;
  int priority;
  int nargs;
};

struct standard_args standard_args[] =
{
  { "-version", "--version", 110, 0 },
  { "-help", "--help", 110, 0 },
  { "-nl", "--no-shared-memory", 100, 0 },
#ifdef VMS
  { "-map", "--map-data", 100, 0 },
#endif
  { "-t", "--terminal", 90, 1 },
  { "-d", "--display", 80, 1 },
  { "-display", 0, 80, 1 },
  { "-nw", "--no-windows", 70, 0 },
  { "-batch", "--batch", 60, 0 },
  { "-q", "--no-init-file", 50, 0 },
  { "-no-init-file", 0, 50, 0 },
  { "-no-site-file", "--no-site-file", 40, 0 },
  { "-u", "--user", 30, 1 },
  { "-user", 0, 30, 1 },
  { "-debug-init", "--debug-init", 20, 0 },
  { "-i", "--icon-type", 15, 1 },
  { "-itype", 0, 15, 1 },
  { "-iconic", "--iconic", 15, 0 },
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
  { "-g", "--geometry", 10, 1 },
  { "-geometry", 0, 10, 1 },
  { "-T", "--title", 10, 1 },
  { "-name", "--name", 10, 1 },
  { "-xrm", "--xrm", 10, 1 },
  { "-r", "--reverse-video", 5, 0 },
  { "-rv", 0, 5, 0 },
  { "-reverse", 0, 5, 0 },
  { "-vb", "--vertical-scroll-bars", 5, 0 },
  /* These have the same priority as ordinary file name args,
     so they are not reordered with respect to those.  */
  { "-L", "--directory", 0, 1 },
  { "-directory", 0, 0, 1 },
  { "-l", "--load", 0, 1 },
  { "-load", 0, 0, 1 },
  { "-f", "--funcall", 0, 1 },
  { "-funcall", 0, 0, 1 },
  { "-insert", "--insert", 0, 1 },
  /* This should be processed after ordinary file name args and the like.  */
  { "-kill", "--kill", -10, 0 },
};

/* Reorder the elements of ARGV (assumed to have ARGC elements)
   so that the highest priority ones come first.
   Do not change the order of elements of equal priority.
   If an option takes an argument, keep it and its argument together.  */

static void
sort_args (argc, argv)
     int argc;
     char **argv;
{
  char **new = (char **) xmalloc (sizeof (char *) * argc);
  /* For each element of argv,
     the corresponding element of options is:
     0 for an option that takes no arguments,
     1 for an option that takes one argument, etc.
     -1 for an ordinary non-option argument.  */
  int *options = (int *) xmalloc (sizeof (int) * argc);
  int *priority = (int *) xmalloc (sizeof (int) * argc);
  int to = 1;
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
	  int match, thislen;
	  char *equals;

	  /* Look for a match with a known old-fashioned option.  */
	  for (i = 0; i < sizeof (standard_args) / sizeof (standard_args[0]); i++)
	    if (!strcmp (argv[from], standard_args[i].name))
	      {
		options[from] = standard_args[i].nargs;
		priority[from] = standard_args[i].priority;
		from += standard_args[i].nargs;
		goto done;
	      }

	  /* Look for a match with a known long option.
	     MATCH is -1 if no match so far, -2 if two or more matches so far,
	     >= 0 (the table index of the match) if just one match so far.  */
	  if (argv[from][1] == '-')
	    {
	      match = -1;
	      thislen = strlen (argv[from]);
	      equals = index (argv[from], '=');
	      if (equals != 0)
		thislen = equals - argv[from];

	      for (i = 0;
		   i < sizeof (standard_args) / sizeof (standard_args[0]); i++)
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
		  from += options[from];
		}
	    }
	done: ;
	}
    }

  /* Copy the arguments, in order of decreasing priority, to NEW.  */
  new[0] = argv[0];
  while (to < argc)
    {
      int best = -1;
      int best_priority = -2;

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
	abort ();

      /* Copy the highest priority remaining option, with its args, to NEW.  */
      new[to++] = argv[best];
      for (i = 0; i < options[best]; i++)
	new[to++] = argv[best + i + 1];

      /* Clear out this option in ARGV.  */
      argv[best] = 0;
      for (i = 0; i < options[best]; i++)
	argv[best + i + 1] = 0;
    }

  bcopy (new, argv, sizeof (char *) * argc);
}

DEFUN ("kill-emacs", Fkill_emacs, Skill_emacs, 0, 1, "P",
  "Exit the Emacs job and kill it.\n\
If ARG is an integer, return ARG as the exit program code.\n\
If ARG is a  string, stuff it as keyboard input.\n\n\
The value of `kill-emacs-hook', if not void,\n\
is a list of functions (of no args),\n\
all of which are called before Emacs is actually killed.")
  (arg)
     Lisp_Object arg;
{
  Lisp_Object hook, hook1;
  int i;
  struct gcpro gcpro1;

  GCPRO1 (arg);

  if (feof (stdin))
    arg = Qt;

  if (!NILP (Vrun_hooks) && !noninteractive)
    call1 (Vrun_hooks, intern ("kill-emacs-hook"));

  /* If we have an auto-save list file,
     kill it because we are exiting Emacs deliberately (not crashing).  */
  if (STRINGP (Vauto_save_list_file_name))
    unlink (XSTRING (Vauto_save_list_file_name)->data);

  UNGCPRO;

/* Is it really necessary to do this deassign
   when we are going to exit anyway?  */
/* #ifdef VMS
  stop_vms_input ();
 #endif  */

  shut_down_emacs (0, 0, STRINGP (arg) ? arg : Qnil);

  exit (INTEGERP (arg) ? XINT (arg)
#ifdef VMS
	: 1
#else
	: 0
#endif
	);
  /* NOTREACHED */
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
shut_down_emacs (sig, no_x, stuff)
     int sig, no_x;
     Lisp_Object stuff;
{
  /* Prevent running of hooks from now on.  */
  Vrun_hooks = Qnil;

  /* If we are controlling the terminal, reset terminal modes */
#ifdef EMACS_HAVE_TTY_PGRP
  {
    int pgrp = EMACS_GETPGRP (0);

    int tpgrp;
    if (EMACS_GET_TTY_PGRP (0, &tpgrp) != -1
	&& tpgrp == pgrp)
      {
	fflush (stdout);
	reset_sys_modes ();
	if (sig && sig != SIGTERM)
	  fprintf (stderr, "Fatal error (%d).", sig);
      }
  }
#else
  fflush (stdout);
  reset_sys_modes ();
#endif

  stuff_buffered_input (stuff);

  kill_buffer_processes (Qnil);
  Fdo_auto_save (Qt, Qnil);

#ifdef CLASH_DETECTION
  unlock_all_files ();
#endif

#ifdef VMS
  kill_vms_processes ();
#endif

#if 0 /* This triggers a bug in XCloseDisplay and is not needed.  */
#ifdef HAVE_X_WINDOWS
  /* It's not safe to call intern here.  Maybe we are crashing.  */
  if (!noninteractive && SYMBOLP (Vwindow_system)
      && XSYMBOL (Vwindow_system)->name->size == 1
      && XSYMBOL (Vwindow_system)->name->data[0] == 'x'
      && ! no_x)
    Fx_close_current_connection ();
#endif /* HAVE_X_WINDOWS */
#endif

#ifdef SIGIO
  /* There is a tendency for a SIGIO signal to arrive within exit,
     and cause a SIGHUP because the input descriptor is already closed.  */
  unrequest_sigio ();
  signal (SIGIO, SIG_IGN);
#endif
}



#ifndef CANNOT_DUMP

#ifdef HAVE_SHM

DEFUN ("dump-emacs-data", Fdump_emacs_data, Sdump_emacs_data, 1, 1, 0,
  "Dump current state of Emacs into data file FILENAME.\n\
This function exists on systems that use HAVE_SHM.")
  (intoname)
     Lisp_Object intoname;
{
  extern char my_edata[];
  Lisp_Object tem;

  CHECK_STRING (intoname, 0);
  intoname = Fexpand_file_name (intoname, Qnil);

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

  fflush (stdout);
  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (my_edata, malloc_warning);
#endif
  map_out_data (XSTRING (intoname)->data);

  Vpurify_flag = tem;

  return Qnil;
}

#else /* not HAVE_SHM */

DEFUN ("dump-emacs", Fdump_emacs, Sdump_emacs, 2, 2, 0,
  "Dump current state of Emacs into executable file FILENAME.\n\
Take symbols from SYMFILE (presumably the file you executed to run Emacs).\n\
This is used in the file `loadup.el' when building Emacs.\n\
\n\
Bind `command-line-processed' to nil before dumping,\n\
if you want the dumped Emacs to process its command line\n\
and announce itself normally when it is run.")
  (intoname, symname)
     Lisp_Object intoname, symname;
{
  extern char my_edata[];
  Lisp_Object tem;

  CHECK_STRING (intoname, 0);
  intoname = Fexpand_file_name (intoname, Qnil);
  if (!NILP (symname))
    {
      CHECK_STRING (symname, 0);
      if (XSTRING (symname)->size)
	symname = Fexpand_file_name (symname, Qnil);
    }

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

  fflush (stdout);
#ifdef VMS
  mapout_data (XSTRING (intoname)->data);
#else
  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
#ifndef WINDOWSNT
  /* On Windows, this was done before dumping, and that once suffices.
     Meanwhile, my_edata is not valid on Windows.  */
  memory_warnings (my_edata, malloc_warning);
#endif /* not WINDOWSNT */
#endif
  unexec (XSTRING (intoname)->data,
	  !NILP (symname) ? XSTRING (symname)->data : 0, &my_edata, 0, 0);
#endif /* not VMS */

  Vpurify_flag = tem;

  return Qnil;
}

#endif /* not HAVE_SHM */

#endif /* not CANNOT_DUMP */

#ifndef SEPCHAR
#define SEPCHAR ':'
#endif

Lisp_Object
decode_env_path (evarname, defalt)
     char *evarname, *defalt;
{
  register char *path, *p;

  Lisp_Object lpath;

  /* It's okay to use getenv here, because this function is only used
     to initialize variables when Emacs starts up, and isn't called
     after that.  */
  if (evarname != 0)
    path = (char *) getenv (evarname);
  else
    path = 0;
  if (!path)
    path = defalt;
  lpath = Qnil;
  while (1)
    {
      p = index (path, SEPCHAR);
      if (!p) p = path + strlen (path);
      lpath = Fcons (p - path ? make_string (path, p - path) : Qnil,
		     lpath);
      if (*p)
	path = p + 1;
      else
	break;
    }
  return Fnreverse (lpath);
}

syms_of_emacs ()
{
#ifndef CANNOT_DUMP
#ifdef HAVE_SHM
  defsubr (&Sdump_emacs_data);
#else
  defsubr (&Sdump_emacs);
#endif
#endif

  defsubr (&Skill_emacs);

  defsubr (&Sinvocation_name);
  defsubr (&Sinvocation_directory);

  DEFVAR_LISP ("command-line-args", &Vcommand_line_args,
    "Args passed by shell to Emacs, as a list of strings.");

  DEFVAR_LISP ("system-type", &Vsystem_type,
    "Value is symbol indicating type of operating system you are using.");
  Vsystem_type = intern (SYSTEM_TYPE);

  DEFVAR_LISP ("system-configuration", &Vsystem_configuration,
    "Value is string indicating configuration Emacs was built for.");
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

  DEFVAR_LISP ("system-configuration-options", &Vsystem_configuration_options,
    "String containing the configuration options Emacs was built with.");
  Vsystem_configuration_options = build_string (EMACS_CONFIG_OPTIONS);

  DEFVAR_BOOL ("noninteractive", &noninteractive1,
    "Non-nil means Emacs is running without interactive terminal.");

  DEFVAR_LISP ("kill-emacs-hook", &Vkill_emacs_hook,
    "Hook to be run whenever kill-emacs is called.\n\
Since kill-emacs may be invoked when the terminal is disconnected (or\n\
in other similar situations), functions placed on this hook should not\n\
expect to be able to interact with the user.  To ask for confirmation,\n\
see `kill-emacs-query-functions' instead.");
  Vkill_emacs_hook = Qnil;

  DEFVAR_INT ("emacs-priority", &emacs_priority,
    "Priority for Emacs to run at.\n\
This value is effective only if set before Emacs is dumped,\n\
and only if the Emacs executable is installed with setuid to permit\n\
it to change priority.  (Emacs sets its uid back to the real uid.)\n\
Currently, you need to define SET_EMACS_PRIORITY in `config.h'\n\
before you compile Emacs, to enable the code for this feature.");
  emacs_priority = 0;

  DEFVAR_LISP ("invocation-name", &Vinvocation_name,
    "The program name that was used to run Emacs.\n\
Any directory names are omitted.");

  DEFVAR_LISP ("invocation-directory", &Vinvocation_directory,
    "The directory in which the Emacs executable was found, to run it.\n\
The value is nil if that directory's name is not known.");

  DEFVAR_LISP ("installation-directory", &Vinstallation_directory,
    "A directory within which to look for the `lib-src' and `etc' directories.\n\
This is non-nil when we can't find those directories in their standard\n\
installed locations, but we can find them\n\
near where the Emacs executable was found.");
  Vinstallation_directory = Qnil;
}
