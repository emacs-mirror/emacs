/* Synchronous subprocess invocation for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1993 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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

extern int errno;
#ifndef VMS
extern char *sys_errlist[];
#endif

/* Define SIGCHLD as an alias for SIGCLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD */

#include <sys/types.h>

#include <sys/file.h>
#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include <paths.h>
#include "process.h"
#include "syssignal.h"

#ifdef VMS
extern noshare char **environ;
#else
extern char **environ;
#endif

#define max(a, b) ((a) > (b) ? (a) : (b))

Lisp_Object Vexec_path, Vexec_directory, Vdata_directory;
Lisp_Object Vconfigure_info_directory;

Lisp_Object Vshell_file_name;

Lisp_Object Vprocess_environment;

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
int synch_process_retcode;

extern Lisp_Object Vdoc_file_name;

#ifndef VMS  /* VMS version is in vmsproc.c.  */

static Lisp_Object
call_process_kill (fdpid)
     Lisp_Object fdpid;
{
  close (XFASTINT (Fcar (fdpid)));
  EMACS_KILLPG (XFASTINT (Fcdr (fdpid)), SIGKILL);
  synch_process_alive = 0;
  return Qnil;
}

Lisp_Object
call_process_cleanup (fdpid)
     Lisp_Object fdpid;
{
  register int pid = XFASTINT (Fcdr (fdpid));

  if (EMACS_KILLPG (pid, SIGINT) == 0)
    {
      int count = specpdl_ptr - specpdl;
      record_unwind_protect (call_process_kill, fdpid);
      message1 ("Waiting for process to die...(type C-g again to kill it instantly)");
      immediate_quit = 1;
      QUIT;
      wait_for_termination (pid);
      immediate_quit = 0;
      specpdl_ptr = specpdl + count; /* Discard the unwind protect.  */
      message1 ("Waiting for process to die...done");
    }
  synch_process_alive = 0;
  close (XFASTINT (Fcar (fdpid)));
  return Qnil;
}

DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
  "Call PROGRAM synchronously in separate process.\n\
The program's input comes from file INFILE (nil means `/dev/null').\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining arguments are strings passed as command arguments to PROGRAM.\n\
If BUFFER is 0, returns immediately with value nil.\n\
Otherwise waits for PROGRAM to terminate\n\
and returns a numeric exit status or a signal description string.\n\
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object infile, buffer, current_dir, display, path;
  int fd[2];
  int filefd;
  register int pid;
  char buf[1024];
  int count = specpdl_ptr - specpdl;
  register unsigned char **new_argv
    = (unsigned char **) alloca ((max (2, nargs - 2)) * sizeof (char *));
  struct buffer *old = current_buffer;
#if 0
  int mask;
#endif
  CHECK_STRING (args[0], 0);

  if (nargs >= 2 && ! NILP (args[1]))
    {
      infile = Fexpand_file_name (args[1], current_buffer->directory);
      CHECK_STRING (infile, 1);
    }
  else
    infile = build_string (NULL_DEVICE);

  if (nargs >= 3)
    {
      register Lisp_Object tem;

      buffer = tem = args[2];
      if (!(EQ (tem, Qnil)
	    || EQ (tem, Qt)
	    || XFASTINT (tem) == 0))
	{
	  buffer = Fget_buffer (tem);
	  CHECK_BUFFER (buffer, 2);
	}
    }
  else 
    buffer = Qnil;

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.

     We have to GCPRO around this because Fexpand_file_name,
     Funhandled_file_name_directory, and Ffile_accessible_directory_p
     might call a file name handling function.  The argument list is
     protected by the caller, so all we really have to worry about is
     buffer.  */
  {
    struct gcpro gcpro1, gcpro2, gcpro3;

    current_dir = current_buffer->directory;

    GCPRO3 (infile, buffer, current_dir);

    current_dir = 
      expand_and_dir_to_file
	(Funhandled_file_name_directory (current_dir), Qnil);
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error ("Setting current directory",
			 Fcons (current_buffer->directory, Qnil));

    UNGCPRO;
  }

  display = nargs >= 4 ? args[3] : Qnil;

  {
    register int i;
    for (i = 4; i < nargs; i++)
      {
	CHECK_STRING (args[i], i);
	new_argv[i - 3] = XSTRING (args[i])->data;
      }
    /* Program name is first command arg */
    new_argv[0] = XSTRING (args[0])->data;
    new_argv[i - 3] = 0;
  }

  filefd = open (XSTRING (infile)->data, O_RDONLY, 0);
  if (filefd < 0)
    {
      report_file_error ("Opening process input file", Fcons (infile, Qnil));
    }
  /* Search for program; barf if not found.  */
  openp (Vexec_path, args[0], EXEC_SUFFIXES, &path, 1);
  if (NILP (path))
    {
      close (filefd);
      report_file_error ("Searching for program", Fcons (args[0], Qnil));
    }
  new_argv[0] = XSTRING (path)->data;

  if (XTYPE (buffer) == Lisp_Int)
    fd[1] = open (NULL_DEVICE, O_WRONLY), fd[0] = -1;
  else
    {
      pipe (fd);
#if 0
      /* Replaced by close_process_descs */
      set_exclusive_use (fd[0]);
#endif
    }

  {
    /* child_setup must clobber environ in systems with true vfork.
       Protect it from permanent change.  */
    register char **save_environ = environ;
    register int fd1 = fd[1];

#if 0  /* Some systems don't have sigblock.  */
    mask = sigblock (sigmask (SIGCHLD));
#endif

    /* Record that we're about to create a synchronous process.  */
    synch_process_alive = 1;

    /* These vars record information from process termination.
       Clear them now before process can possibly terminate,
       to avoid timing error if process terminates soon.  */
    synch_process_death = 0;
    synch_process_retcode = 0;

    pid = vfork ();

    if (pid == 0)
      {
	if (fd[0] >= 0)
	  close (fd[0]);
#ifdef USG
        setpgrp ();
#else
        setpgrp (pid, pid);
#endif /* USG */
	child_setup (filefd, fd1, fd1, new_argv, 0, current_dir);
      }

#if 0
    /* Tell SIGCHLD handler to look for this pid.  */
    synch_process_pid = pid;
    /* Now let SIGCHLD come through.  */
    sigsetmask (mask);
#endif

    environ = save_environ;

    close (filefd);
    close (fd1);
  }

  if (pid < 0)
    {
      close (fd[0]);
      report_file_error ("Doing vfork", Qnil);
    }

  if (XTYPE (buffer) == Lisp_Int)
    {
#ifndef subprocesses
      /* If Emacs has been built with asynchronous subprocess support,
	 we don't need to do this, I think because it will then have
	 the facilities for handling SIGCHLD.  */
      wait_without_blocking ();
#endif /* subprocesses */
      return Qnil;
    }

  record_unwind_protect (call_process_cleanup,
			 Fcons (make_number (fd[0]), make_number (pid)));


  if (XTYPE (buffer) == Lisp_Buffer)
    Fset_buffer (buffer);

  immediate_quit = 1;
  QUIT;

  {
    register int nread;

    while ((nread = read (fd[0], buf, sizeof buf)) > 0)
      {
	immediate_quit = 0;
	if (!NILP (buffer))
	  insert (buf, nread);
	if (!NILP (display) && INTERACTIVE)
	  redisplay_preserve_echo_area ();
	immediate_quit = 1;
	QUIT;
      }
  }

  /* Wait for it to terminate, unless it already has.  */
  wait_for_termination (pid);

  immediate_quit = 0;

  set_buffer_internal (old);

  unbind_to (count, Qnil);

  if (synch_process_death)
    return build_string (synch_process_death);
  return make_number (synch_process_retcode);
}
#endif

static Lisp_Object
delete_temp_file (name)
     Lisp_Object name;
{
  unlink (XSTRING (name)->data);
}

DEFUN ("call-process-region", Fcall_process_region, Scall_process_region,
  3, MANY, 0,
  "Send text from START to END to a synchronous process running PROGRAM.\n\
Delete the text if fourth arg DELETE is non-nil.\n\
Insert output in BUFFER before point; t means current buffer;\n\
 nil for BUFFER means discard it; 0 means discard and don't wait.\n\
Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.\n\
Remaining args are passed to PROGRAM at startup as command args.\n\
If BUFFER is nil, returns immediately with value nil.\n\
Otherwise waits for PROGRAM to terminate\n\
and returns a numeric exit status or a signal description string.\n\
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object filename_string, start, end;
  char tempfile[20];
  int count = specpdl_ptr - specpdl;

#ifdef VMS
  strcpy (tempfile, "tmp:emacsXXXXXX.");
#else
  strcpy (tempfile, "/tmp/emacsXXXXXX");
#endif
  mktemp (tempfile);

  filename_string = build_string (tempfile);
  start = args[0];
  end = args[1];
  Fwrite_region (start, end, filename_string, Qnil, Qlambda);
  record_unwind_protect (delete_temp_file, filename_string);

  if (!NILP (args[3]))
    Fdelete_region (start, end);

  args[3] = filename_string;

  return unbind_to (count, Fcall_process (nargs - 2, args + 2));
}

#ifndef VMS /* VMS version is in vmsproc.c.  */

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   This function may change environ for the superior process.
   Therefore, the superior process must save and restore the value
   of environ around the vfork and the call to this function.

   ENV is the environment for the subprocess.

   SET_PGRP is nonzero if we should put the subprocess into a separate
   process group.  

   CURRENT_DIR is an elisp string giving the path of the current
   directory the subprocess should have.  Since we can't really signal
   a decent error from within the child, this should be verified as an
   executable directory by the parent.  */

child_setup (in, out, err, new_argv, set_pgrp, current_dir)
     int in, out, err;
     register char **new_argv;
     int set_pgrp;
     Lisp_Object current_dir;
{
  char **env;

  register int pid = getpid();

  {
    extern int emacs_priority;

    nice (- emacs_priority);
  }

#ifdef subprocesses
  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();
#endif

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA it is safe because that changes
     the superior's static variables as if the superior had done alloca
     and will be cleaned up in the usual way.  */
  {
    register unsigned char *temp;
    register int i;

    i = XSTRING (current_dir)->size;
    temp = (unsigned char *) alloca (i + 2);
    bcopy (XSTRING (current_dir)->data, temp, i);
    if (temp[i - 1] != '/') temp[i++] = '/';
    temp[i] = 0;

    /* We can't signal an Elisp error here; we're in a vfork.  Since
       the callers check the current directory before forking, this
       should only return an error if the directory's permissions
       are changed between the check and this chdir, but we should
       at least check.  */
    if (chdir (temp) < 0)
      exit (errno);
  }

  /* Set `env' to a vector of the strings in Vprocess_environment.  */
  {
    register Lisp_Object tem;
    register char **new_env;
    register int new_length;

    new_length = 0;
    for (tem = Vprocess_environment;
	 (XTYPE (tem) == Lisp_Cons
	  && XTYPE (XCONS (tem)->car) == Lisp_String);
	 tem = XCONS (tem)->cdr)
      new_length++;

    /* new_length + 1 to include terminating 0.  */
    env = new_env = (char **) alloca ((new_length + 1) * sizeof (char *));

    /* Copy the Vprocess_environment strings into new_env.  */
    for (tem = Vprocess_environment;
	 (XTYPE (tem) == Lisp_Cons
	  && XTYPE (XCONS (tem)->car) == Lisp_String);
	 tem = XCONS (tem)->cdr)
      {
	char **ep = env;
	char *string = (char *) XSTRING (XCONS (tem)->car)->data;
	/* See if this string duplicates any string already in the env.
	   If so, don't put it in.
	   When an env var has multiple definitions,
	   we keep the definition that comes first in process-environment.  */
	for (; ep != new_env; ep++)
	  {
	    char *p = *ep, *q = string;
	    while (1)
	      {
		if (*q == 0)
		  /* The string is malformed; might as well drop it.  */
		  goto duplicate;
		if (*q != *p)
		  break;
		if (*q == '=')
		  goto duplicate;
		p++, q++;
	      }
	  }
	*new_env++ = string;
      duplicate: ;
      }
    *new_env = 0;
  }

  /* Make sure that in, out, and err are not actually already in
     descriptors zero, one, or two; this could happen if Emacs is
     started with its standard in, our, or error closed, as might
     happen under X.  */
  in = relocate_fd (in, 3);
  out = relocate_fd (out, 3);
  err = relocate_fd (err, 3);

  close (0);
  close (1);
  close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  close (in);
  close (out);
  close (err);

#ifdef USG
#ifndef SETPGRP_RELEASES_CTTY
  setpgrp ();			/* No arguments but equivalent in this case */
#endif
#else
  setpgrp (pid, pid);
#endif /* USG */
  setpgrp_of_tty (pid);

#ifdef vipc
  something missing here;
#endif /* vipc */

  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  environ = env;
  execvp (new_argv[0], new_argv);

  write (1, "Couldn't exec the program ", 26);
  write (1, new_argv[0], strlen (new_argv[0]));
  _exit (1);
}

/* Move the file descriptor FD so that its number is not less than MIN.
   If the file descriptor is moved at all, the original is freed.  */
int
relocate_fd (fd, min)
     int fd, min;
{
  if (fd >= min)
    return fd;
  else
    {
      int new = dup (fd);
      if (new == -1)
	{
	  char *message1 = "Error while setting up child: ";
	  char *message2 = "\n";
	  write (2, message1, strlen (message1));
	  write (2, sys_errlist[errno], strlen (sys_errlist[errno]));
	  write (2, message2, strlen (message2));
	  _exit (1);
	}
      /* Note that we hold the original FD open while we recurse,
	 to guarantee we'll get a new FD if we need it.  */
      new = relocate_fd (new, min);
      close (fd);
      return new;
    }
}

static int
getenv_internal (var, varlen, value, valuelen)
     char *var;
     int varlen;
     char **value;
     int *valuelen;
{
  Lisp_Object scan;

  for (scan = Vprocess_environment; CONSP (scan); scan = XCONS (scan)->cdr)
    {
      Lisp_Object entry = XCONS (scan)->car;
      
      if (XTYPE (entry) == Lisp_String
	  && XSTRING (entry)->size > varlen
	  && XSTRING (entry)->data[varlen] == '='
	  && ! bcmp (XSTRING (entry)->data, var, varlen))
	{
	  *value    = (char *) XSTRING (entry)->data + (varlen + 1);
	  *valuelen = XSTRING (entry)->size - (varlen + 1);
	  return 1;
	}
    }

  return 0;
}

DEFUN ("getenv", Fgetenv, Sgetenv, 1, 2, 0,
  "Return the value of environment variable VAR, as a string.\n\
VAR should be a string.  Value is nil if VAR is undefined in the environment.\n\
This function consults the variable ``process-environment'' for its value.")
  (var)
     Lisp_Object var;
{
  char *value;
  int valuelen;

  CHECK_STRING (var, 0);
  if (getenv_internal (XSTRING (var)->data, XSTRING (var)->size,
		       &value, &valuelen))
    return make_string (value, valuelen);
  else
    return Qnil;
}

/* A version of getenv that consults process_environment, easily
   callable from C.  */
char *
egetenv (var)
     char *var;
{
  char *value;
  int valuelen;

  if (getenv_internal (var, strlen (var), &value, &valuelen))
    return value;
  else
    return 0;
}

#endif /* not VMS */

/* This is run before init_cmdargs.  */

init_callproc_1 ()
{
  char *data_dir = egetenv ("EMACSDATA");
    
  Vdata_directory
    = Ffile_name_as_directory (build_string (data_dir ? data_dir
					     : PATH_DATA));

  /* Check the EMACSPATH environment variable, defaulting to the
     PATH_EXEC path from paths.h.  */
  Vexec_path = decode_env_path ("EMACSPATH", PATH_EXEC);
  Vexec_directory = Ffile_name_as_directory (Fcar (Vexec_path));
  Vexec_path = nconc2 (decode_env_path ("PATH", ""), Vexec_path);
}

/* This is run after init_cmdargs, so that Vinvocation_directory is valid.  */

init_callproc ()
{
  char *data_dir = egetenv ("EMACSDATA");
    
  register char * sh;
  Lisp_Object tempdir;

  if (initialized && !NILP (Vinvocation_directory))
    {
      /* Add to the path the ../lib-src dir of the Emacs executable,
	 if that dir exists.  */
      Lisp_Object tem, tem1;
      tem = Fexpand_file_name (build_string ("../lib-src"),
			       Vinvocation_directory);
      tem1 = Ffile_exists_p (tem);
      if (!NILP (tem1) && NILP (Fmember (tem, Vexec_path)))
	{
	  Vexec_path = nconc2 (Vexec_path, Fcons (tem, Qnil));
	  Vexec_directory = Ffile_name_as_directory (tem);

	  /* If we use ../lib-src, maybe use ../etc as well.
	     Do so if ../etc exists and has our DOC-... file in it.  */
	  if (data_dir == 0)
	    {
	      Lisp_Object tem, tem2, tem3;
	      tem = Fexpand_file_name (build_string ("../etc"),
				       Vinvocation_directory);
	      tem2 = Fexpand_file_name (Vdoc_file_name, tem);
	      tem3 = Ffile_exists_p (tem2);
	      if (!NILP (tem3))
		Vdata_directory = Ffile_name_as_directory (tem);
	    }
	}
    }

  tempdir = Fdirectory_file_name (Vexec_directory);
  if (access (XSTRING (tempdir)->data, 0) < 0)
    {
      printf ("Warning: arch-dependent data dir (%s) does not exist.\n",
	      XSTRING (Vexec_directory)->data);
      sleep (2);
    }

  tempdir = Fdirectory_file_name (Vdata_directory);
  if (access (XSTRING (tempdir)->data, 0) < 0)
    {
      printf ("Warning: arch-independent data dir (%s) does not exist.\n",
	      XSTRING (Vdata_directory)->data);
      sleep (2);
    }

#ifdef VMS
  Vshell_file_name = build_string ("*dcl*");
#else
  sh = (char *) getenv ("SHELL");
  Vshell_file_name = build_string (sh ? sh : "/bin/sh");
#endif
}

set_process_environment ()
{
  register char **envp;

  Vprocess_environment = Qnil;
#ifndef CANNOT_DUMP
  if (initialized)
#endif
    for (envp = environ; *envp; envp++)
      Vprocess_environment = Fcons (build_string (*envp),
				    Vprocess_environment);
}

syms_of_callproc ()
{
  DEFVAR_LISP ("shell-file-name", &Vshell_file_name,
    "*File name to load inferior shells from.\n\
Initialized from the SHELL environment variable.");

  DEFVAR_LISP ("exec-path", &Vexec_path,
    "*List of directories to search programs to run in subprocesses.\n\
Each element is a string (directory name) or nil (try default directory).");

  DEFVAR_LISP ("exec-directory", &Vexec_directory,
    "Directory of architecture-dependent files that come with GNU Emacs,\n\
especially executable programs intended for Emacs to invoke.");

  DEFVAR_LISP ("data-directory", &Vdata_directory,
    "Directory of architecture-independent files that come with GNU Emacs,\n\
intended for Emacs to use.");

  DEFVAR_LISP ("configure-info-directory", &Vconfigure_info_directory,
    "For internal use by the build procedure only.\n\
This is the name of the directory in which the build procedure installed\n\
Emacs's info files; the default value for Info-default-directory-list\n\
includes this.");
  Vconfigure_info_directory = build_string (PATH_INFO);

  DEFVAR_LISP ("process-environment", &Vprocess_environment,
    "List of environment variables for subprocesses to inherit.\n\
Each element should be a string of the form ENVVARNAME=VALUE.\n\
The environment which Emacs inherits is placed in this variable\n\
when Emacs starts.");

#ifndef VMS
  defsubr (&Scall_process);
  defsubr (&Sgetenv);
#endif
  defsubr (&Scall_process_region);
}
