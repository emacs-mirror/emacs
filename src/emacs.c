/* Fully extensible Emacs, running on Unix, intended for GNU.
   Copyright (C) 1985, 1986, 1987, 1993 Free Software Foundation, Inc.

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

#ifdef APOLLO
#ifndef APOLLO_SR10
#include <default_acl.h>
#endif
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

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* The name under which Emacs was invoked, with any leading directory
   names discarded.  */
Lisp_Object Vinvocation_name;

/* The directory name from which Emacs was invoked.  */
Lisp_Object Vinvocation_directory;

/* Hook run by `kill-emacs' before it does really anything.  */
Lisp_Object Vkill_emacs_hook;

/* Set nonzero after Emacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

/* Variable whose value is symbol giving operating system type */
Lisp_Object Vsystem_type;
  
/* If non-zero, emacs should not attempt to use an window-specific code,
   but instead should use the virtual terminal under which it was started */
int inhibit_window_system;

/* If nonzero, set Emacs to run at this priority.  This is also used
   in child_setup and sys_suspend to make sure subshells run at normal
   priority; Those functions have their own extern declaration.  */
int emacs_priority;

#ifdef BSD
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
  sigunblock (sigmask (fatal_error_code));
  kill (getpid (), fatal_error_code);
#endif /* not VMS */
}

/* Code for dealing with Lisp access to the Unix command line */

static
init_cmdargs (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  register int i;

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
__main ()
{}
#endif /* __GNUC__ */
#endif /* ORDINARY_LINK */

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
  extern char *sys_errlist[];
  extern void malloc_warning ();

/* Map in shared memory, if we are using that.  */
#ifdef HAVE_SHM
  if (argc > 1 && !strcmp (argv[1], "-nl"))
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
  extern int malloc_cookie;

  /* This helps out unexnext.c.  */
  if (initialized)
    if (malloc_jumpstart (malloc_cookie) != 0)
      printf ("malloc jumpstart failed!\n");
#endif /* NeXT */

#ifdef HAVE_X_WINDOWS
  /* Stupid kludge to catch command-line display spec.  We can't
     handle this argument entirely in window system dependent code
     because we don't even know which window system dependent code
     to run until we've recognized this argument.  */
  {
    int i;

    for (i = 1; (i < argc && ! display_arg); i++)
      if (!strcmp (argv[i], "-d") || !strcmp (argv[i], "-display"))
	display_arg = 1;
  }
#endif

#ifdef VMS
  /* If -map specified, map the data file in */
  if (argc > 2 && ! strcmp (argv[1], "-map"))
    {
      skip_args = 2;
      mapin_data (argv[2]);
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
    brk (bss_end);
#endif

  clearerr (stdin);

#ifdef BSD
  {
    inherited_pgroup = EMACS_GETPGRP (0);
    setpgrp (0, getpid ());
  }
#endif


#ifdef APOLLO
#ifndef APOLLO_SR10
  /* If USE_DOMAIN_ACLS environment variable exists,
     use ACLs rather than UNIX modes. */
  if (egetenv ("USE_DOMAIN_ACLS"))
    default_acl (USE_DEFACL);
#endif
#endif /* APOLLO */

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

#ifdef PRIO_PROCESS
  if (emacs_priority)
    nice (emacs_priority);
  setuid (getuid ());
#endif /* PRIO_PROCESS */

  inhibit_window_system = 0;

  /* Handle the -t switch, which specifies filename to use as terminal */
  if (skip_args + 2 < argc && !strcmp (argv[skip_args + 1], "-t"))
    {
      int result;
      skip_args += 2;
      close (0);
      close (1);
      result = open (argv[skip_args], O_RDWR, 2 );
      if (result < 0)
	{
	  char *errstring;

	  if (errno >= 0 && errno < sys_nerr)
	    errstring = sys_errlist[errno];
	  else
	    errstring = "undocumented error code";
	  fprintf (stderr, "emacs: %s: %s\n", argv[skip_args], errstring);
	  exit (1);
	}
      dup (0);
      if (! isatty (0))
	{
	  fprintf (stderr, "emacs: %s: not a tty\n", argv[skip_args]);
	  exit (1);
	}
      fprintf (stderr, "Using %s\n", argv[skip_args]);
#ifdef HAVE_X_WINDOWS
      inhibit_window_system = 1;	/* -t => -nw */
#endif
    }

  if (skip_args + 1 < argc
      && (!strcmp (argv[skip_args + 1], "-nw")))
    {
      skip_args += 1;
      inhibit_window_system = 1;
    }

/* Handle the -batch switch, which means don't do interactive display.  */
  noninteractive = 0;
  if (skip_args + 1 < argc && !strcmp (argv[skip_args + 1], "-batch"))
    {
      skip_args += 1;
      noninteractive = 1;
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

#ifdef AIX
      signal (SIGDANGER, fatal_error_signal);
      signal (20, fatal_error_signal);
      signal (21, fatal_error_signal);
      signal (22, fatal_error_signal);
      signal (24, fatal_error_signal);
#if 0 /* mvn@library.ucla.edu says these are SIGIO on AIX 3.2.4.  */
      signal (23, fatal_error_signal);
#ifdef SIGIO
      signal (SIGAIO, fatal_error_signal);
      signal (SIGPTY, fatal_error_signal);
#endif
#endif
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

  /* egetenv is a pretty low-level facility, which may get called in
     many circumstances; it seems flimsy to put off initializing it
     until calling init_callproc.  */
  set_process_environment ();

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
      /* Handle -l loadup-and-dump, args passed by Makefile. */
      if (argc > 2 + skip_args && !strcmp (argv[1 + skip_args], "-l"))
	Vtop_level = Fcons (intern ("load"),
			    Fcons (build_string (argv[2 + skip_args]), Qnil));
#ifdef CANNOT_DUMP
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (!(argc > 1 + skip_args && !strcmp (argv[1 + skip_args], "-nl")))
	Vtop_level = Fcons (intern ("load"),
			    Fcons (build_string ("loadup.el"), Qnil));
#endif /* CANNOT_DUMP */
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

  UNGCPRO;

/* Is it really necessary to do this deassign
   when we are going to exit anyway?  */
/* #ifdef VMS
  stop_vms_input ();
 #endif  */

  shut_down_emacs (0, 0, STRINGP (arg) ? arg : Qnil);

  exit ((XTYPE (arg) == Lisp_Int) ? XINT (arg)
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

#ifdef HAVE_X_WINDOWS
  if (!noninteractive && EQ (Vwindow_system, intern ("x")) && ! no_x)
    Fx_close_current_connection ();
#endif /* HAVE_X_WINDOWS */

#ifdef SIGIO
  /* There is a tendency for a SIGIO signal to arrive within exit,
     and cause a SIGHUP because the input descriptor is already closed.  */
  unrequest_sigio ();
  signal (SIGIO, SIG_IGN);
#endif
}



#ifndef CANNOT_DUMP
/* Nothing like this can be implemented on an Apollo.
   What a loss!  */

#ifdef HAVE_SHM

DEFUN ("dump-emacs-data", Fdump_emacs_data, Sdump_emacs_data, 1, 1, 0,
  "Dump current state of Emacs into data file FILENAME.\n\
This function exists on systems that use HAVE_SHM.")
  (intoname)
     Lisp_Object intoname;
{
  extern int my_edata;
  Lisp_Object tem;
  extern void malloc_warning ();

  CHECK_STRING (intoname, 0);
  intoname = Fexpand_file_name (intoname, Qnil);

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

  fflush (stdout);
  /* Tell malloc where start of impure now is */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
  memory_warnings (&my_edata, malloc_warning);
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
  extern int my_edata;
  Lisp_Object tem;
  extern void malloc_warning ();

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
  memory_warnings (&my_edata, malloc_warning);
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
  extern char *index ();

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

  DEFVAR_BOOL ("noninteractive", &noninteractive1,
    "Non-nil means Emacs is running without interactive terminal.");

  DEFVAR_LISP ("kill-emacs-hook", &Vkill_emacs_hook,
    "Hook to be run whenever kill-emacs is called.\n\
Since kill-emacs may be invoked when the terminal is disconnected (or\n\
in other similar situations), functions placed on this hook should not\n\
expect to be able to interact with the user.");
  Vkill_emacs_hook = Qnil;

  DEFVAR_INT ("emacs-priority", &emacs_priority,
    "Priority for Emacs to run at.\n\
This value is effective only if set before Emacs is dumped,\n\
and only if the Emacs executable is installed with setuid to permit\n\
it to change priority.  (Emacs sets its uid back to the real uid.)");
  emacs_priority = 0;

  staticpro (&Vinvocation_name);
  Vinvocation_name = Qnil;
  staticpro (&Vinvocation_directory);
  Vinvocation_directory = Qnil;
}
