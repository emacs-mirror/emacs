/* Synchronous subprocess invocation for GNU Emacs.

Copyright (C) 1985-1988, 1993-1995, 1999-2017 Free Software Foundation,
Inc.

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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include <sys/file.h>
#include <fcntl.h>

#include "lisp.h"

#ifdef WINDOWSNT
#include <sys/socket.h>	/* for fcntl */
#include <windows.h>
#include "w32.h"
#define _P_NOWAIT 1	/* from process.h */
#endif

#ifdef MSDOS	/* Demacs 1.1.1 91/10/16 HIRANO Satoshi */
#include <sys/stat.h>
#include <sys/param.h>
#endif /* MSDOS */

#include "commands.h"
#include "buffer.h"
#include "coding.h"
#include <epaths.h>
#include "process.h"
#include "syssignal.h"
#include "syswait.h"
#include "blockinput.h"
#include "frame.h"
#include "systty.h"
#include "keyboard.h"

#ifdef MSDOS
#include "msdos.h"
#endif

#ifdef HAVE_NS
#include "nsterm.h"
#endif

/* Pattern used by call-process-region to make temp files.  */
static Lisp_Object Vtemp_file_name_pattern;

/* The next two variables are used while record-unwind-protect is in place
   during call-process for a subprocess for which record_deleted_pid has
   not yet been called.  At other times, synch_process_pid is zero and
   synch_process_tempfile's contents are irrelevant.  Doing this via static
   C variables is more convenient than putting them into the arguments
   of record-unwind-protect, as they need to be updated at randomish
   times in the code, and Lisp cannot always store these values as
   Emacs integers.  It's safe to use static variables here, as the
   code is never invoked reentrantly.  */

/* If nonzero, a process-ID that has not been reaped.  */
static pid_t synch_process_pid;

/* If a string, the name of a temp file that has not been removed.  */
#ifdef MSDOS
static Lisp_Object synch_process_tempfile;
#else
# define synch_process_tempfile make_number (0)
#endif

/* Indexes of file descriptors that need closing on call_process_kill.  */
enum
  {
    /* The subsidiary process's stdout and stderr.  stdin is handled
       separately, in either Fcall_process_region or create_temp_file.  */
    CALLPROC_STDOUT, CALLPROC_STDERR,

    /* How to read from a pipe (or substitute) from the subsidiary process.  */
    CALLPROC_PIPEREAD,

    /* A bound on the number of file descriptors.  */
    CALLPROC_FDS
  };

static Lisp_Object call_process (ptrdiff_t, Lisp_Object *, int, ptrdiff_t);

/* Return the current buffer's working directory, or the home
   directory if it's unreachable, as a string suitable for a system call.
   Signal an error if the result would not be an accessible directory.  */

Lisp_Object
encode_current_directory (void)
{
  Lisp_Object dir;

  dir = BVAR (current_buffer, directory);

  dir = Funhandled_file_name_directory (dir);

  /* If the file name handler says that dir is unreachable, use
     a sensible default. */
  if (NILP (dir))
    dir = build_string ("~");

  dir = expand_and_dir_to_file (dir);

  if (NILP (Ffile_accessible_directory_p (dir)))
    report_file_error ("Setting current directory",
		       BVAR (current_buffer, directory));

  /* Remove "/:" from DIR and encode it.  */
  dir = ENCODE_FILE (remove_slash_colon (dir));

  if (! file_accessible_directory_p (dir))
    report_file_error ("Setting current directory",
		       BVAR (current_buffer, directory));

  return dir;
}

/* If P is reapable, record it as a deleted process and kill it.
   Do this in a critical section.  Unless PID is wedged it will be
   reaped on receipt of the first SIGCHLD after the critical section.  */

void
record_kill_process (struct Lisp_Process *p, Lisp_Object tempfile)
{
#ifndef MSDOS
  sigset_t oldset;
  block_child_signal (&oldset);

  if (p->alive)
    {
      record_deleted_pid (p->pid, tempfile);
      p->alive = 0;
      kill (- p->pid, SIGKILL);
    }

  unblock_child_signal (&oldset);
#endif	/* !MSDOS */
}

/* Clean up files, file descriptors and processes created by Fcall_process.  */

static void
delete_temp_file (Lisp_Object name)
{
  unlink (SSDATA (name));
}

static void
call_process_kill (void *ptr)
{
  int *callproc_fd = ptr;
  int i;
  for (i = 0; i < CALLPROC_FDS; i++)
    if (0 <= callproc_fd[i])
      emacs_close (callproc_fd[i]);

  if (synch_process_pid)
    {
      struct Lisp_Process proc;
      proc.alive = 1;
      proc.pid = synch_process_pid;
      record_kill_process (&proc, synch_process_tempfile);
      synch_process_pid = 0;
    }
  else if (STRINGP (synch_process_tempfile))
    delete_temp_file (synch_process_tempfile);
}

/* Clean up when exiting Fcall_process: restore the buffer, and
   kill the subsidiary process group if the process still exists.  */

static void
call_process_cleanup (Lisp_Object buffer)
{
  Fset_buffer (buffer);

#ifndef MSDOS
  if (synch_process_pid)
    {
      kill (-synch_process_pid, SIGINT);
      message1 ("Waiting for process to die...(type C-g again to kill it instantly)");

      /* This will quit on C-g.  */
      bool wait_ok = wait_for_termination (synch_process_pid, NULL, true);
      synch_process_pid = 0;
      message1 (wait_ok
		? "Waiting for process to die...done"
		: "Waiting for process to die...internal error");
    }
#endif	/* !MSDOS */
}

#ifdef DOS_NT
static mode_t const default_output_mode = S_IREAD | S_IWRITE;
#else
static mode_t const default_output_mode = 0666;
#endif

DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
       doc: /* Call PROGRAM synchronously in separate process.
The remaining arguments are optional.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in DESTINATION before point; t means current buffer; nil for DESTINATION
 means discard it; 0 means discard and don't wait; and `(:file FILE)', where
 FILE is a file name string, means that it should be written to that file
 (if the file already exists it is overwritten).
DESTINATION can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If executable PROGRAM can't be found as an executable, `call-process'
signals a Lisp error.  `call-process' reports errors in execution of
the program only through its return and output.

If DESTINATION is 0, `call-process' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.

The process runs in `default-directory' if that is local (as
determined by `unhandled-file-name-directory'), or "~" otherwise.  If
you want to run a process in a remote directory use `process-file'.

usage: (call-process PROGRAM &optional INFILE DESTINATION DISPLAY &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object infile, encoded_infile;
  int filefd;
  ptrdiff_t count = SPECPDL_INDEX ();

  if (nargs >= 2 && ! NILP (args[1]))
    {
      infile = Fexpand_file_name (args[1], BVAR (current_buffer, directory));
      CHECK_STRING (infile);
    }
  else
    infile = build_string (NULL_DEVICE);

  encoded_infile = ENCODE_FILE (infile);

  filefd = emacs_open (SSDATA (encoded_infile), O_RDONLY, 0);
  if (filefd < 0)
    report_file_error ("Opening process input file", infile);
  record_unwind_protect_int (close_file_unwind, filefd);
  return unbind_to (count, call_process (nargs, args, filefd, -1));
}

/* Like Fcall_process (NARGS, ARGS), except use FILEFD as the input file.

   If TEMPFILE_INDEX is nonnegative, it is the specpdl index of an
   unwinder that is intended to remove the input temporary file; in
   this case NARGS must be at least 2 and ARGS[1] is the file's name.

   At entry, the specpdl stack top entry must be close_file_unwind (FILEFD).  */

static Lisp_Object
call_process (ptrdiff_t nargs, Lisp_Object *args, int filefd,
	      ptrdiff_t tempfile_index)
{
  Lisp_Object buffer, current_dir, path;
  bool display_p;
  int fd0;
  int callproc_fd[CALLPROC_FDS];
  int status;
  ptrdiff_t i;
  ptrdiff_t count = SPECPDL_INDEX ();
  USE_SAFE_ALLOCA;

  char **new_argv;
  /* File to use for stderr in the child.
     t means use same as standard output.  */
  Lisp_Object error_file;
  Lisp_Object output_file = Qnil;
#ifdef MSDOS	/* Demacs 1.1.1 91/10/16 HIRANO Satoshi */
  char *tempfile = NULL;
#else
  sigset_t oldset;
  pid_t pid;
#endif
  int child_errno;
  int fd_output, fd_error;
  struct coding_system process_coding; /* coding-system of process output */
  struct coding_system argument_coding;	/* coding-system of arguments */
  /* Set to the return value of Ffind_operation_coding_system.  */
  Lisp_Object coding_systems;
  bool discard_output;

  if (synch_process_pid)
    error ("call-process invoked recursively");

  /* Qt denotes that Ffind_operation_coding_system is not yet called.  */
  coding_systems = Qt;

  CHECK_STRING (args[0]);

  error_file = Qt;

#ifndef subprocesses
  /* Without asynchronous processes we cannot have BUFFER == 0.  */
  if (nargs >= 3
      && (INTEGERP (CONSP (args[2]) ? XCAR (args[2]) : args[2])))
    error ("Operating system cannot handle asynchronous subprocesses");
#endif /* subprocesses */

  /* Decide the coding-system for giving arguments.  */
  {
    Lisp_Object val, *args2;

    /* If arguments are supplied, we may have to encode them.  */
    if (nargs >= 5)
      {
	bool must_encode = 0;
	Lisp_Object coding_attrs;

	for (i = 4; i < nargs; i++)
	  CHECK_STRING (args[i]);

	for (i = 4; i < nargs; i++)
	  if (STRING_MULTIBYTE (args[i]))
	    must_encode = 1;

	if (!NILP (Vcoding_system_for_write))
	  val = Vcoding_system_for_write;
	else if (! must_encode)
	  val = Qraw_text;
	else
	  {
	    SAFE_NALLOCA (args2, 1, nargs + 1);
	    args2[0] = Qcall_process;
	    for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	    coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
	    val = CONSP (coding_systems) ? XCDR (coding_systems) : Qnil;
	  }
	val = complement_process_encoding_system (val);
	setup_coding_system (Fcheck_coding_system (val), &argument_coding);
	coding_attrs = CODING_ID_ATTRS (argument_coding.id);
	if (NILP (CODING_ATTR_ASCII_COMPAT (coding_attrs)))
	  {
	    /* We should not use an ASCII incompatible coding system.  */
	    val = raw_text_coding_system (val);
	    setup_coding_system (val, &argument_coding);
	  }
      }
  }

  if (nargs < 3)
    buffer = Qnil;
  else
    {
      buffer = args[2];

      /* If BUFFER is a list, its meaning is (BUFFER-FOR-STDOUT
	 FILE-FOR-STDERR), unless the first element is :file, in which case see
	 the next paragraph. */
      if (CONSP (buffer) && !EQ (XCAR (buffer), QCfile))
	{
	  if (CONSP (XCDR (buffer)))
	    {
	      Lisp_Object stderr_file;
	      stderr_file = XCAR (XCDR (buffer));

	      if (NILP (stderr_file) || EQ (Qt, stderr_file))
		error_file = stderr_file;
	      else
		error_file = Fexpand_file_name (stderr_file, Qnil);
	    }

	  buffer = XCAR (buffer);
	}

      /* If the buffer is (still) a list, it might be a (:file "file") spec. */
      if (CONSP (buffer) && EQ (XCAR (buffer), QCfile))
	{
	  output_file = Fexpand_file_name (XCAR (XCDR (buffer)),
					   BVAR (current_buffer, directory));
	  CHECK_STRING (output_file);
	  buffer = Qnil;
	}

      if (! (NILP (buffer) || EQ (buffer, Qt) || INTEGERP (buffer)))
	{
	  Lisp_Object spec_buffer;
	  spec_buffer = buffer;
	  buffer = Fget_buffer_create (buffer);
	  /* Mention the buffer name for a better error message.  */
	  if (NILP (buffer))
	    CHECK_BUFFER (spec_buffer);
	  CHECK_BUFFER (buffer);
	}
    }

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.  */
  current_dir = encode_current_directory ();

  if (STRINGP (error_file))
    error_file = ENCODE_FILE (error_file);
  if (STRINGP (output_file))
    output_file = ENCODE_FILE (output_file);

  display_p = INTERACTIVE && nargs >= 4 && !NILP (args[3]);

  for (i = 0; i < CALLPROC_FDS; i++)
    callproc_fd[i] = -1;
#ifdef MSDOS
  synch_process_tempfile = make_number (0);
#endif
  record_unwind_protect_ptr (call_process_kill, callproc_fd);

  /* Search for program; barf if not found.  */
  {
    int ok;

    ok = openp (Vexec_path, args[0], Vexec_suffixes, &path,
		make_number (X_OK), false);
    if (ok < 0)
      report_file_error ("Searching for program", args[0]);
  }

  /* Remove "/:" from PATH.  */
  path = remove_slash_colon (path);

  SAFE_NALLOCA (new_argv, 1, nargs < 4 ? 2 : nargs - 2);

  if (nargs > 4)
    {
      ptrdiff_t i;

      argument_coding.dst_multibyte = 0;
      for (i = 4; i < nargs; i++)
	{
	  argument_coding.src_multibyte = STRING_MULTIBYTE (args[i]);
	  if (CODING_REQUIRE_ENCODING (&argument_coding))
	    /* We must encode this argument.  */
	    args[i] = encode_coding_string (&argument_coding, args[i], 1);
	}
      for (i = 4; i < nargs; i++)
	new_argv[i - 3] = SSDATA (args[i]);
      new_argv[i - 3] = 0;
    }
  else
    new_argv[1] = 0;
  path = ENCODE_FILE (path);
  new_argv[0] = SSDATA (path);

  discard_output = INTEGERP (buffer) || (NILP (buffer) && NILP (output_file));

#ifdef MSDOS
  if (! discard_output && ! STRINGP (output_file))
    {
      char const *tmpdir = egetenv ("TMPDIR");
      char const *outf = tmpdir ? tmpdir : "";
      tempfile = alloca (strlen (outf) + 20);
      strcpy (tempfile, outf);
      dostounix_filename (tempfile);
      if (*tempfile == '\0' || tempfile[strlen (tempfile) - 1] != '/')
	strcat (tempfile, "/");
      strcat (tempfile, "emXXXXXX");
      mktemp (tempfile);
      if (!*tempfile)
	report_file_error ("Opening process output file", Qnil);
      output_file = build_string (tempfile);
      synch_process_tempfile = output_file;
    }
#endif

  if (discard_output)
    {
      fd_output = emacs_open (NULL_DEVICE, O_WRONLY, 0);
      if (fd_output < 0)
	report_file_error ("Opening null device", Qnil);
    }
  else if (STRINGP (output_file))
    {
      fd_output = emacs_open (SSDATA (output_file),
			      O_WRONLY | O_CREAT | O_TRUNC | O_TEXT,
			      default_output_mode);
      if (fd_output < 0)
	{
	  int open_errno = errno;
	  output_file = DECODE_FILE (output_file);
	  report_file_errno ("Opening process output file",
			     output_file, open_errno);
	}
    }
  else
    {
      int fd[2];
      if (emacs_pipe (fd) != 0)
	report_file_error ("Creating process pipe", Qnil);
      callproc_fd[CALLPROC_PIPEREAD] = fd[0];
      fd_output = fd[1];
    }
  callproc_fd[CALLPROC_STDOUT] = fd_output;

  fd_error = fd_output;

  if (STRINGP (error_file) || (NILP (error_file) && !discard_output))
    {
      fd_error = emacs_open ((STRINGP (error_file)
			      ? SSDATA (error_file)
			      : NULL_DEVICE),
			     O_WRONLY | O_CREAT | O_TRUNC | O_TEXT,
			     default_output_mode);
      if (fd_error < 0)
	{
	  int open_errno = errno;
	  report_file_errno ("Cannot redirect stderr",
			     (STRINGP (error_file)
			      ? DECODE_FILE (error_file)
			      : build_string (NULL_DEVICE)),
			     open_errno);
	}
      callproc_fd[CALLPROC_STDERR] = fd_error;
    }

#ifdef MSDOS /* MW, July 1993 */
  status = child_setup (filefd, fd_output, fd_error, new_argv, 0, current_dir);

  if (status < 0)
    {
      child_errno = errno;
      unbind_to (count, Qnil);
      synchronize_system_messages_locale ();
      return
	code_convert_string_norecord (build_string (strerror (child_errno)),
				      Vlocale_coding_system, 0);
    }

  for (i = 0; i < CALLPROC_FDS; i++)
    if (0 <= callproc_fd[i])
      {
	emacs_close (callproc_fd[i]);
	callproc_fd[i] = -1;
      }
  emacs_close (filefd);
  clear_unwind_protect (count - 1);

  if (tempfile)
    {
      /* Since CRLF is converted to LF within `decode_coding', we
	 can always open a file with binary mode.  */
      callproc_fd[CALLPROC_PIPEREAD] = emacs_open (tempfile, O_RDONLY, 0);
      if (callproc_fd[CALLPROC_PIPEREAD] < 0)
	{
	  int open_errno = errno;
	  report_file_errno ("Cannot re-open temporary file",
			     build_string (tempfile), open_errno);
	}
    }

#endif /* MSDOS */

  /* Do the unwind-protect now, even though the pid is not known, so
     that no storage allocation is done in the critical section.
     The actual PID will be filled in during the critical section.  */
  record_unwind_protect (call_process_cleanup, Fcurrent_buffer ());

#ifndef MSDOS

  block_input ();
  block_child_signal (&oldset);

#ifdef WINDOWSNT
  pid = child_setup (filefd, fd_output, fd_error, new_argv, 0, current_dir);
#else  /* not WINDOWSNT */

  /* vfork, and prevent local vars from being clobbered by the vfork.  */
  {
    Lisp_Object volatile buffer_volatile = buffer;
    Lisp_Object volatile coding_systems_volatile = coding_systems;
    Lisp_Object volatile current_dir_volatile = current_dir;
    bool volatile display_p_volatile = display_p;
    bool volatile sa_must_free_volatile = sa_must_free;
    int volatile fd_error_volatile = fd_error;
    int volatile filefd_volatile = filefd;
    ptrdiff_t volatile count_volatile = count;
    ptrdiff_t volatile sa_avail_volatile = sa_avail;
    ptrdiff_t volatile sa_count_volatile = sa_count;
    char **volatile new_argv_volatile = new_argv;
    int volatile callproc_fd_volatile[CALLPROC_FDS];
    for (i = 0; i < CALLPROC_FDS; i++)
      callproc_fd_volatile[i] = callproc_fd[i];

    pid = vfork ();

    buffer = buffer_volatile;
    coding_systems = coding_systems_volatile;
    current_dir = current_dir_volatile;
    display_p = display_p_volatile;
    sa_must_free = sa_must_free_volatile;
    fd_error = fd_error_volatile;
    filefd = filefd_volatile;
    count = count_volatile;
    sa_avail = sa_avail_volatile;
    sa_count = sa_count_volatile;
    new_argv = new_argv_volatile;

    for (i = 0; i < CALLPROC_FDS; i++)
      callproc_fd[i] = callproc_fd_volatile[i];
    fd_output = callproc_fd[CALLPROC_STDOUT];
  }

  if (pid == 0)
    {
#ifdef DARWIN_OS
      /* Work around a macOS bug, where SIGCHLD is apparently
	 delivered to a vforked child instead of to its parent.  See:
	 https://lists.gnu.org/r/emacs-devel/2017-05/msg00342.html
      */
      signal (SIGCHLD, SIG_DFL);
#endif

      unblock_child_signal (&oldset);

#ifdef DARWIN_OS
      /* Darwin doesn't let us run setsid after a vfork, so use
         TIOCNOTTY when necessary. */
      int j = emacs_open (DEV_TTY, O_RDWR, 0);
      if (j >= 0)
        {
          ioctl (j, TIOCNOTTY, 0);
          emacs_close (j);
        }
#else
      setsid ();
#endif

      /* Emacs ignores SIGPIPE, but the child should not.  */
      signal (SIGPIPE, SIG_DFL);
      /* Likewise for SIGPROF.  */
#ifdef SIGPROF
      signal (SIGPROF, SIG_DFL);
#endif

      child_setup (filefd, fd_output, fd_error, new_argv, 0, current_dir);
    }

#endif /* not WINDOWSNT */

  child_errno = errno;

  if (pid > 0)
    {
      synch_process_pid = pid;

      if (INTEGERP (buffer))
	{
	  if (tempfile_index < 0)
	    record_deleted_pid (pid, Qnil);
	  else
	    {
	      eassert (1 < nargs);
	      record_deleted_pid (pid, args[1]);
	      clear_unwind_protect (tempfile_index);
	    }
	  synch_process_pid = 0;
	}
    }

  unblock_child_signal (&oldset);
  unblock_input ();

  if (pid < 0)
    report_file_errno ("Doing vfork", Qnil, child_errno);

  /* Close our file descriptors, except for callproc_fd[CALLPROC_PIPEREAD]
     since we will use that to read input from.  */
  for (i = 0; i < CALLPROC_FDS; i++)
    if (i != CALLPROC_PIPEREAD && 0 <= callproc_fd[i])
      {
	emacs_close (callproc_fd[i]);
	callproc_fd[i] = -1;
      }
  emacs_close (filefd);
  clear_unwind_protect (count - 1);

#endif /* not MSDOS */

  if (INTEGERP (buffer))
    return unbind_to (count, Qnil);

  if (BUFFERP (buffer))
    Fset_buffer (buffer);

  fd0 = callproc_fd[CALLPROC_PIPEREAD];

  if (0 <= fd0)
    {
      Lisp_Object val, *args2;

      val = Qnil;
      if (!NILP (Vcoding_system_for_read))
	val = Vcoding_system_for_read;
      else
	{
	  if (EQ (coding_systems, Qt))
	    {
	      ptrdiff_t i;

	      SAFE_NALLOCA (args2, 1, nargs + 1);
	      args2[0] = Qcall_process;
	      for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	      coding_systems
		= Ffind_operation_coding_system (nargs + 1, args2);
	    }
	  if (CONSP (coding_systems))
	    val = XCAR (coding_systems);
	  else if (CONSP (Vdefault_process_coding_system))
	    val = XCAR (Vdefault_process_coding_system);
	  else
	    val = Qnil;
	}
      Fcheck_coding_system (val);
      /* In unibyte mode, character code conversion should not take
	 place but EOL conversion should.  So, setup raw-text or one
	 of the subsidiary according to the information just setup.  */
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
	  && !NILP (val))
	val = raw_text_coding_system (val);
      setup_coding_system (val, &process_coding);
      process_coding.dst_multibyte
	= ! NILP (BVAR (current_buffer, enable_multibyte_characters));
      process_coding.src_multibyte = 0;
    }

  if (0 <= fd0)
    {
      enum { CALLPROC_BUFFER_SIZE_MIN = 16 * 1024 };
      enum { CALLPROC_BUFFER_SIZE_MAX = 4 * CALLPROC_BUFFER_SIZE_MIN };
      char buf[CALLPROC_BUFFER_SIZE_MAX];
      int bufsize = CALLPROC_BUFFER_SIZE_MIN;
      int nread;
      EMACS_INT total_read = 0;
      int carryover = 0;
      bool display_on_the_fly = display_p;
      struct coding_system saved_coding = process_coding;

      while (1)
	{
	  /* Repeatedly read until we've filled as much as possible
	     of the buffer size we have.  But don't read
	     less than 1024--save that for the next bufferful.  */
	  nread = carryover;
	  while (nread < bufsize - 1024)
	    {
	      int this_read = emacs_read_quit (fd0, buf + nread,
					       bufsize - nread);

	      if (this_read < 0)
		goto give_up;

	      if (this_read == 0)
		{
		  process_coding.mode |= CODING_MODE_LAST_BLOCK;
		  break;
		}

	      nread += this_read;
	      total_read += this_read;

	      if (display_on_the_fly)
		break;
	    }

	  /* Now NREAD is the total amount of data in the buffer.  */

	  if (!nread)
	    ;
	  else if (NILP (BVAR (current_buffer, enable_multibyte_characters))
		   && ! CODING_MAY_REQUIRE_DECODING (&process_coding))
	    insert_1_both (buf, nread, nread, 0, 1, 0);
	  else
	    {			/* We have to decode the input.  */
	      Lisp_Object curbuf;
	      ptrdiff_t count1 = SPECPDL_INDEX ();

	      XSETBUFFER (curbuf, current_buffer);
	      /* FIXME: Call signal_after_change!  */
	      prepare_to_modify_buffer (PT, PT, NULL);
	      /* We cannot allow after-change-functions be run
		 during decoding, because that might modify the
		 buffer, while we rely on process_coding.produced to
		 faithfully reflect inserted text until we
		 TEMP_SET_PT_BOTH below.  */
	      specbind (Qinhibit_modification_hooks, Qt);
	      decode_coding_c_string (&process_coding,
				      (unsigned char *) buf, nread, curbuf);
	      unbind_to (count1, Qnil);
	      if (display_on_the_fly
		  && CODING_REQUIRE_DETECTION (&saved_coding)
		  && ! CODING_REQUIRE_DETECTION (&process_coding))
		{
		  /* We have detected some coding system, but the
		     detection may have been via insufficient data.
		     So give up displaying on the fly.  */
		  if (process_coding.produced > 0)
		    del_range_2 (process_coding.dst_pos,
				 process_coding.dst_pos_byte,
				 (process_coding.dst_pos
				  + process_coding.produced_char),
				 (process_coding.dst_pos_byte
				  + process_coding.produced),
				 0);
		  display_on_the_fly = false;
		  process_coding = saved_coding;
		  carryover = nread;
		  /* Make the above condition always fail in the future.  */
		  saved_coding.common_flags
		    &= ~CODING_REQUIRE_DETECTION_MASK;
		  continue;
		}

	      TEMP_SET_PT_BOTH (PT + process_coding.produced_char,
				PT_BYTE + process_coding.produced);
	      carryover = process_coding.carryover_bytes;
	      if (carryover > 0)
		memcpy (buf, process_coding.carryover,
			process_coding.carryover_bytes);
	    }

	  if (process_coding.mode & CODING_MODE_LAST_BLOCK)
	    break;

	  /* Make the buffer bigger as we continue to read more data,
	     but not past CALLPROC_BUFFER_SIZE_MAX.  */
	  if (bufsize < CALLPROC_BUFFER_SIZE_MAX && total_read > 32 * bufsize)
	    if ((bufsize *= 2) > CALLPROC_BUFFER_SIZE_MAX)
	      bufsize = CALLPROC_BUFFER_SIZE_MAX;

	  if (display_p)
	    {
	      redisplay_preserve_echo_area (1);
	      /* This variable might have been set to 0 for code
		 detection.  In that case, set it back to 1 because
		 we should have already detected a coding system.  */
	      display_on_the_fly = true;
	    }
	}
    give_up: ;

      Vlast_coding_system_used = CODING_ID_NAME (process_coding.id);
      /* If the caller required, let the buffer inherit the
	 coding-system used to decode the process output.  */
      if (inherit_process_coding_system)
	call1 (intern ("after-insert-file-set-buffer-file-coding-system"),
	       make_number (total_read));
    }

  bool wait_ok = true;
#ifndef MSDOS
  /* Wait for it to terminate, unless it already has.  */
  wait_ok = wait_for_termination (pid, &status, fd0 < 0);
#endif

  /* Don't kill any children that the subprocess may have left behind
     when exiting.  */
  synch_process_pid = 0;

  SAFE_FREE ();
  unbind_to (count, Qnil);

  if (!wait_ok)
    return build_unibyte_string ("internal error");

  if (WIFSIGNALED (status))
    {
      const char *signame;

      synchronize_system_messages_locale ();
      signame = strsignal (WTERMSIG (status));

      if (signame == 0)
	signame = "unknown";

      return code_convert_string_norecord (build_string (signame),
					   Vlocale_coding_system, 0);
    }

  eassert (WIFEXITED (status));
  return make_number (WEXITSTATUS (status));
}

/* Create a temporary file suitable for storing the input data of
   call-process-region.  NARGS and ARGS are the same as for
   call-process-region.  Store into *FILENAME_STRING_PTR a Lisp string
   naming the file, and return a file descriptor for reading.
   Unwind-protect the file, so that the file descriptor will be closed
   and the file removed when the caller unwinds the specpdl stack.  */

static int
create_temp_file (ptrdiff_t nargs, Lisp_Object *args,
		  Lisp_Object *filename_string_ptr)
{
  int fd;
  Lisp_Object filename_string;
  Lisp_Object val, start, end;
  Lisp_Object tmpdir;

  if (STRINGP (Vtemporary_file_directory))
    tmpdir = Vtemporary_file_directory;
  else
    {
      char *outf;
#ifndef DOS_NT
      outf = getenv ("TMPDIR");
      tmpdir = build_string (outf ? outf : "/tmp/");
#else /* DOS_NT */
      if ((outf = egetenv ("TMPDIR"))
	  || (outf = egetenv ("TMP"))
	  || (outf = egetenv ("TEMP")))
	tmpdir = build_string (outf);
      else
	tmpdir = Ffile_name_as_directory (build_string ("c:/temp"));
#endif
    }

  {
    Lisp_Object pattern = Fexpand_file_name (Vtemp_file_name_pattern, tmpdir);
    char *tempfile;
    ptrdiff_t count;

#ifdef WINDOWSNT
    /* Cannot use the result of Fexpand_file_name, because it
       downcases the XXXXXX part of the pattern, and mktemp then
       doesn't recognize it.  */
    if (!NILP (Vw32_downcase_file_names))
      {
	Lisp_Object dirname = Ffile_name_directory (pattern);

	if (NILP (dirname))
	  pattern = Vtemp_file_name_pattern;
	else
	  pattern = concat2 (dirname, Vtemp_file_name_pattern);
      }
#endif

    filename_string = Fcopy_sequence (ENCODE_FILE (pattern));
    tempfile = SSDATA (filename_string);

    count = SPECPDL_INDEX ();
    record_unwind_protect_nothing ();
    fd = mkostemp (tempfile, O_BINARY | O_CLOEXEC);
    if (fd < 0)
      report_file_error ("Failed to open temporary file using pattern",
			 pattern);
    set_unwind_protect (count, delete_temp_file, filename_string);
    record_unwind_protect_int (close_file_unwind, fd);
  }

  start = args[0];
  end = args[1];
  /* Decide coding-system of the contents of the temporary file.  */
  if (!NILP (Vcoding_system_for_write))
    val = Vcoding_system_for_write;
  else if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    val = Qraw_text;
  else
    {
      Lisp_Object coding_systems;
      Lisp_Object *args2;
      USE_SAFE_ALLOCA;
      SAFE_NALLOCA (args2, 1, nargs + 1);
      args2[0] = Qcall_process_region;
      memcpy (args2 + 1, args, nargs * sizeof *args);
      coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
      val = CONSP (coding_systems) ? XCDR (coding_systems) : Qnil;
      SAFE_FREE ();
    }
  val = complement_process_encoding_system (val);

  {
    ptrdiff_t count1 = SPECPDL_INDEX ();

    specbind (intern ("coding-system-for-write"), val);
    /* POSIX lets mk[s]temp use "."; don't invoke jka-compr if we
       happen to get a ".Z" suffix.  */
    specbind (Qfile_name_handler_alist, Qnil);
    write_region (start, end, filename_string, Qnil, Qlambda, Qnil, Qnil, fd);

    unbind_to (count1, Qnil);
  }

  if (lseek (fd, 0, SEEK_SET) < 0)
    report_file_error ("Setting file position", filename_string);

  /* Note that Fcall_process takes care of binding
     coding-system-for-read.  */

  *filename_string_ptr = filename_string;
  return fd;
}

DEFUN ("call-process-region", Fcall_process_region, Scall_process_region,
       3, MANY, 0,
       doc: /* Send text from START to END to a synchronous process running PROGRAM.

START and END are normally buffer positions specifying the part of the
buffer to send to the process.
If START is nil, that means to use the entire buffer contents; END is
ignored.
If START is a string, then send that string to the process
instead of any buffer contents; END is ignored.
The remaining arguments are optional.
Delete the text if fourth arg DELETE is non-nil.

Insert output in BUFFER before point; t means current buffer; nil for
 BUFFER means discard it; 0 means discard and don't wait; and `(:file
 FILE)', where FILE is a file name string, means that it should be
 written to that file (if the file already exists it is overwritten).
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining args are passed to PROGRAM at startup as command args.

If BUFFER is 0, `call-process-region' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.

usage: (call-process-region START END PROGRAM &optional DELETE BUFFER DISPLAY &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object infile, val;
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object start = args[0];
  Lisp_Object end = args[1];
  bool empty_input;
  int fd;

  if (STRINGP (start))
    empty_input = SCHARS (start) == 0;
  else if (NILP (start))
    empty_input = BEG == Z;
  else
    {
      validate_region (&args[0], &args[1]);
      start = args[0];
      end = args[1];
      empty_input = XINT (start) == XINT (end);
    }

  if (!empty_input)
    fd = create_temp_file (nargs, args, &infile);
  else
    {
      infile = Qnil;
      fd = emacs_open (NULL_DEVICE, O_RDONLY, 0);
      if (fd < 0)
	report_file_error ("Opening null device", Qnil);
      record_unwind_protect_int (close_file_unwind, fd);
    }

  if (nargs > 3 && !NILP (args[3]))
    Fdelete_region (start, end);

  if (nargs > 3)
    {
      args += 2;
      nargs -= 2;
    }
  else
    {
      args[0] = args[2];
      nargs = 2;
    }
  args[1] = infile;

  val = call_process (nargs, args, fd, empty_input ? -1 : count);
  return unbind_to (count, val);
}

static char **
add_env (char **env, char **new_env, char *string)
{
  char **ep;
  bool ok = 1;
  if (string == NULL)
    return new_env;

  /* See if this string duplicates any string already in the env.
     If so, don't put it in.
     When an env var has multiple definitions,
     we keep the definition that comes first in process-environment.  */
  for (ep = env; ok && ep != new_env; ep++)
    {
      char *p = *ep, *q = string;
      while (ok)
	{
	  if (*p && *q != *p)
	    break;
	  if (*q == 0)
	    /* The string is a lone variable name; keep it for now, we
	       will remove it later.  It is a placeholder for a
	       variable that is not to be included in the environment.  */
	    break;
	  if (*q == '=')
	    ok = 0;
	  p++, q++;
	}
    }
  if (ok)
    *new_env++ = string;
  return new_env;
}

#ifndef DOS_NT

/* 'exec' failed inside a child running NAME, with error number ERR.
   Possibly a vforked child needed to allocate a large vector on the
   stack; such a child cannot fall back on malloc because that might
   mess up the allocator's data structures in the parent.
   Report the error and exit the child.  */

static _Noreturn void
exec_failed (char const *name, int err)
{
  /* Avoid deadlock if the child's perror writes to a full pipe; the
     pipe's reader is the parent, but with vfork the parent can't
     run until the child exits.  Truncate the diagnostic instead.  */
  fcntl (STDERR_FILENO, F_SETFL, O_NONBLOCK);

  errno = err;
  emacs_perror (name);
  _exit (err == ENOENT ? EXIT_ENOENT : EXIT_CANNOT_INVOKE);
}

#else

/* Do nothing.  There is no need to fail, as DOS_NT platforms do not
   fork and exec, and handle alloca exhaustion in a different way.  */

static void
exec_failed (char const *name, int err)
{
}

#endif

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   If SET_PGRP, put the subprocess into a separate process group.

   CURRENT_DIR is an elisp string giving the path of the current
   directory the subprocess should have.  Since we can't really signal
   a decent error from within the child, this should be verified as an
   executable directory by the parent.

   On GNUish hosts, either exec or return an error number.
   On MS-Windows, either return a pid or signal an error.
   On MS-DOS, either return an exit status or signal an error.  */

CHILD_SETUP_TYPE
child_setup (int in, int out, int err, char **new_argv, bool set_pgrp,
	     Lisp_Object current_dir)
{
  char **env;
  char *pwd_var;
#ifdef WINDOWSNT
  int cpid;
  HANDLE handles[3];
#else
  pid_t pid = getpid ();
#endif /* WINDOWSNT */

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA (when Emacs used to include
     src/alloca.c) it is safe because that changes the superior's
     static variables as if the superior had done alloca and will be
     cleaned up in the usual way. */
  {
    char *temp;
    ptrdiff_t i;

    i = SBYTES (current_dir);
#ifdef MSDOS
    /* MSDOS must have all environment variables malloc'ed, because
       low-level libc functions that launch subsidiary processes rely
       on that.  */
    pwd_var = xmalloc (i + 5);
#else
    if (MAX_ALLOCA - 5 < i)
      exec_failed (new_argv[0], ENOMEM);
    pwd_var = alloca (i + 5);
#endif
    temp = pwd_var + 4;
    memcpy (pwd_var, "PWD=", 4);
    lispstpcpy (temp, current_dir);

#ifndef DOS_NT
    /* We can't signal an Elisp error here; we're in a vfork.  Since
       the callers check the current directory before forking, this
       should only return an error if the directory's permissions
       are changed between the check and this chdir, but we should
       at least check.  */
    if (chdir (temp) < 0)
      _exit (EXIT_CANCELED);
#else /* DOS_NT */
    /* Get past the drive letter, so that d:/ is left alone.  */
    if (i > 2 && IS_DEVICE_SEP (temp[1]) && IS_DIRECTORY_SEP (temp[2]))
      {
	temp += 2;
	i -= 2;
      }
#endif /* DOS_NT */

    /* Strip trailing slashes for PWD, but leave "/" and "//" alone.  */
    while (i > 2 && IS_DIRECTORY_SEP (temp[i - 1]))
      temp[--i] = 0;
  }

  /* Set `env' to a vector of the strings in the environment.  */
  {
    register Lisp_Object tem;
    register char **new_env;
    char **p, **q;
    register int new_length;
    Lisp_Object display = Qnil;

    new_length = 0;

    for (tem = Vprocess_environment;
	 CONSP (tem) && STRINGP (XCAR (tem));
	 tem = XCDR (tem))
      {
	if (strncmp (SSDATA (XCAR (tem)), "DISPLAY", 7) == 0
	    && (SDATA (XCAR (tem)) [7] == '\0'
		|| SDATA (XCAR (tem)) [7] == '='))
	  /* DISPLAY is specified in process-environment.  */
	  display = Qt;
	new_length++;
      }

    /* If not provided yet, use the frame's DISPLAY.  */
    if (NILP (display))
      {
	Lisp_Object tmp = Fframe_parameter (selected_frame, Qdisplay);
	if (!STRINGP (tmp) && CONSP (Vinitial_environment))
	  /* If still not found, Look for DISPLAY in Vinitial_environment.  */
	  tmp = Fgetenv_internal (build_string ("DISPLAY"),
				  Vinitial_environment);
	if (STRINGP (tmp))
	  {
	    display = tmp;
	    new_length++;
	  }
      }

    /* new_length + 2 to include PWD and terminating 0.  */
    if (MAX_ALLOCA / sizeof *env - 2 < new_length)
      exec_failed (new_argv[0], ENOMEM);
    env = new_env = alloca ((new_length + 2) * sizeof *env);
    /* If we have a PWD envvar, pass one down,
       but with corrected value.  */
    if (egetenv ("PWD"))
      *new_env++ = pwd_var;

    if (STRINGP (display))
      {
	if (MAX_ALLOCA - sizeof "DISPLAY=" < SBYTES (display))
	  exec_failed (new_argv[0], ENOMEM);
	char *vdata = alloca (sizeof "DISPLAY=" + SBYTES (display));
	lispstpcpy (stpcpy (vdata, "DISPLAY="), display);
	new_env = add_env (env, new_env, vdata);
      }

    /* Overrides.  */
    for (tem = Vprocess_environment;
	 CONSP (tem) && STRINGP (XCAR (tem));
	 tem = XCDR (tem))
      new_env = add_env (env, new_env, SSDATA (XCAR (tem)));

    *new_env = 0;

    /* Remove variable names without values.  */
    p = q = env;
    while (*p != 0)
      {
	while (*q != 0 && strchr (*q, '=') == NULL)
	  q++;
	*p = *q++;
	if (*p != 0)
	  p++;
      }
  }


#ifdef WINDOWSNT
  prepare_standard_handles (in, out, err, handles);
  set_process_dir (SSDATA (current_dir));
  /* Spawn the child.  (See w32proc.c:sys_spawnve).  */
  cpid = spawnve (_P_NOWAIT, new_argv[0], new_argv, env);
  reset_standard_handles (in, out, err, handles);
  if (cpid == -1)
    /* An error occurred while trying to spawn the process.  */
    report_file_error ("Spawning child process", Qnil);
  return cpid;

#else  /* not WINDOWSNT */

#ifndef MSDOS

  restore_nofile_limit ();

  /* Redirect file descriptors and clear the close-on-exec flag on the
     redirected ones.  IN, OUT, and ERR are close-on-exec so they
     need not be closed explicitly.  */
  dup2 (in, STDIN_FILENO);
  dup2 (out, STDOUT_FILENO);
  dup2 (err, STDERR_FILENO);

  setpgid (0, 0);
  tcsetpgrp (0, pid);

  int errnum = emacs_exec_file (new_argv[0], new_argv, env);
  exec_failed (new_argv[0], errnum);

#else /* MSDOS */
  pid = run_msdos_command (new_argv, pwd_var + 4, in, out, err, env);
  xfree (pwd_var);
  if (pid == -1)
    /* An error occurred while trying to run the subprocess.  */
    report_file_error ("Spawning child process", Qnil);
  return pid;
#endif  /* MSDOS */
#endif  /* not WINDOWSNT */
}

static bool
getenv_internal_1 (const char *var, ptrdiff_t varlen, char **value,
		   ptrdiff_t *valuelen, Lisp_Object env)
{
  for (; CONSP (env); env = XCDR (env))
    {
      Lisp_Object entry = XCAR (env);
      if (STRINGP (entry)
	  && SBYTES (entry) >= varlen
#ifdef WINDOWSNT
	  /* NT environment variables are case insensitive.  */
	  && ! strnicmp (SSDATA (entry), var, varlen)
#else  /* not WINDOWSNT */
	  && ! memcmp (SDATA (entry), var, varlen)
#endif /* not WINDOWSNT */
	  )
	{
	  if (SBYTES (entry) > varlen && SREF (entry, varlen) == '=')
	    {
	      *value = SSDATA (entry) + (varlen + 1);
	      *valuelen = SBYTES (entry) - (varlen + 1);
	      return 1;
	    }
	  else if (SBYTES (entry) == varlen)
	    {
	      /* Lone variable names in Vprocess_environment mean that
		 variable should be removed from the environment. */
	      *value = NULL;
	      return 1;
	    }
	}
    }
  return 0;
}

static bool
getenv_internal (const char *var, ptrdiff_t varlen, char **value,
		 ptrdiff_t *valuelen, Lisp_Object frame)
{
  /* Try to find VAR in Vprocess_environment first.  */
  if (getenv_internal_1 (var, varlen, value, valuelen,
			 Vprocess_environment))
    return *value ? 1 : 0;

  /* On Windows we make some modifications to Emacs' environment
     without recording them in Vprocess_environment.  */
#ifdef WINDOWSNT
  {
    char *tmpval = getenv (var);
    if (tmpval)
      {
        *value = tmpval;
        *valuelen = strlen (tmpval);
        return 1;
      }
  }
#endif

  /* For DISPLAY try to get the values from the frame or the initial env.  */
  if (strcmp (var, "DISPLAY") == 0)
    {
      Lisp_Object display
	= Fframe_parameter (NILP (frame) ? selected_frame : frame, Qdisplay);
      if (STRINGP (display))
	{
	  *value    = SSDATA (display);
	  *valuelen = SBYTES (display);
	  return 1;
	}
      /* If still not found, Look for DISPLAY in Vinitial_environment.  */
      if (getenv_internal_1 (var, varlen, value, valuelen,
			     Vinitial_environment))
	return *value ? 1 : 0;
    }

  return 0;
}

DEFUN ("getenv-internal", Fgetenv_internal, Sgetenv_internal, 1, 2, 0,
       doc: /* Get the value of environment variable VARIABLE.
VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
the environment.  Otherwise, value is a string.

This function searches `process-environment' for VARIABLE.

If optional parameter ENV is a list, then search this list instead of
`process-environment', and return t when encountering a negative entry
\(an entry for a variable with no value).  */)
  (Lisp_Object variable, Lisp_Object env)
{
  char *value;
  ptrdiff_t valuelen;

  CHECK_STRING (variable);
  if (CONSP (env))
    {
      if (getenv_internal_1 (SSDATA (variable), SBYTES (variable),
			     &value, &valuelen, env))
	return value ? make_string (value, valuelen) : Qt;
      else
	return Qnil;
    }
  else if (getenv_internal (SSDATA (variable), SBYTES (variable),
			    &value, &valuelen, env))
    return make_string (value, valuelen);
  else
    return Qnil;
}

/* A version of getenv that consults the Lisp environment lists,
   easily callable from C.  This is usually called from egetenv.  */
char *
egetenv_internal (const char *var, ptrdiff_t len)
{
  char *value;
  ptrdiff_t valuelen;

  if (getenv_internal (var, len, &value, &valuelen, Qnil))
    return value;
  else
    return 0;
}


/* This is run before init_cmdargs.  */

void
init_callproc_1 (void)
{
#ifdef HAVE_NS
  const char *etc_dir = ns_etc_directory ();
  const char *path_exec = ns_exec_path ();
#endif

  Vdata_directory = decode_env_path ("EMACSDATA",
#ifdef HAVE_NS
                                             etc_dir ? etc_dir :
#endif
                                             PATH_DATA, 0);
  Vdata_directory = Ffile_name_as_directory (Fcar (Vdata_directory));

  Vdoc_directory = decode_env_path ("EMACSDOC",
#ifdef HAVE_NS
                                             etc_dir ? etc_dir :
#endif
                                             PATH_DOC, 0);
  Vdoc_directory = Ffile_name_as_directory (Fcar (Vdoc_directory));

  /* Check the EMACSPATH environment variable, defaulting to the
     PATH_EXEC path from epaths.h.  */
  Vexec_path = decode_env_path ("EMACSPATH",
#ifdef HAVE_NS
                                path_exec ? path_exec :
#endif
                                PATH_EXEC, 0);
  Vexec_directory = Ffile_name_as_directory (Fcar (Vexec_path));
  /* FIXME?  For ns, path_exec should go at the front?  */
  Vexec_path = nconc2 (decode_env_path ("PATH", "", 0), Vexec_path);
}

/* This is run after init_cmdargs, when Vinstallation_directory is valid.  */

void
init_callproc (void)
{
  bool data_dir = egetenv ("EMACSDATA") != 0;

  char *sh;
  Lisp_Object tempdir;
#ifdef HAVE_NS
  if (data_dir == 0)
    data_dir = ns_etc_directory () != 0;
#endif

  if (!NILP (Vinstallation_directory))
    {
      /* Add to the path the lib-src subdir of the installation dir.  */
      Lisp_Object tem;
      tem = Fexpand_file_name (build_string ("lib-src"),
			       Vinstallation_directory);
#ifndef MSDOS
	  /* MSDOS uses wrapped binaries, so don't do this.  */
      if (NILP (Fmember (tem, Vexec_path)))
	{
#ifdef HAVE_NS
	  const char *path_exec = ns_exec_path ();
#endif
	  /* Running uninstalled, so default to tem rather than PATH_EXEC.  */
	  Vexec_path = decode_env_path ("EMACSPATH",
#ifdef HAVE_NS
					path_exec ? path_exec :
#endif
					SSDATA (tem), 0);
	  Vexec_path = nconc2 (decode_env_path ("PATH", "", 0), Vexec_path);
	}

      Vexec_directory = Ffile_name_as_directory (tem);
#endif /* not MSDOS */

      /* Maybe use ../etc as well as ../lib-src.  */
      if (data_dir == 0)
	{
	  tem = Fexpand_file_name (build_string ("etc"),
				   Vinstallation_directory);
	  Vdoc_directory = Ffile_name_as_directory (tem);
	}
    }

  /* Look for the files that should be in etc.  We don't use
     Vinstallation_directory, because these files are never installed
     near the executable, and they are never in the build
     directory when that's different from the source directory.

     Instead, if these files are not in the nominal place, we try the
     source directory.  */
  if (data_dir == 0)
    {
      Lisp_Object tem, tem1, srcdir;
      Lisp_Object lispdir = Fcar (decode_env_path (0, PATH_DUMPLOADSEARCH, 0));

      srcdir = Fexpand_file_name (build_string ("../src/"), lispdir);

      tem = Fexpand_file_name (build_string ("NEWS"), Vdata_directory);
      tem1 = Ffile_exists_p (tem);
      if (!NILP (Fequal (srcdir, Vinvocation_directory)) || NILP (tem1))
	{
	  Lisp_Object newdir;
	  newdir = Fexpand_file_name (build_string ("../etc/"), lispdir);
	  tem = Fexpand_file_name (build_string ("NEWS"), newdir);
	  tem1 = Ffile_exists_p (tem);
	  if (!NILP (tem1))
	    Vdata_directory = newdir;
	}
    }

#ifndef CANNOT_DUMP
  if (initialized)
#endif
    {
      tempdir = Fdirectory_file_name (Vexec_directory);
      if (! file_accessible_directory_p (tempdir))
	dir_warning ("arch-dependent data dir", Vexec_directory);
    }

  tempdir = Fdirectory_file_name (Vdata_directory);
  if (! file_accessible_directory_p (tempdir))
    dir_warning ("arch-independent data dir", Vdata_directory);

  sh = getenv ("SHELL");
  Vshell_file_name = build_string (sh ? sh : "/bin/sh");

  Lisp_Object gamedir = Qnil;
  if (PATH_GAME)
    {
      Lisp_Object path_game = build_unibyte_string (PATH_GAME);
      if (file_accessible_directory_p (path_game))
	gamedir = path_game;
    }
  Vshared_game_score_directory = gamedir;
}

void
set_initial_environment (void)
{
  char **envp;
  for (envp = environ; *envp; envp++)
    Vprocess_environment = Fcons (build_string (*envp),
				  Vprocess_environment);
  /* Ideally, the `copy' shouldn't be necessary, but it seems it's frequent
     to use `delete' and friends on process-environment.  */
  Vinitial_environment = Fcopy_sequence (Vprocess_environment);
}

void
syms_of_callproc (void)
{
#ifndef DOS_NT
  Vtemp_file_name_pattern = build_string ("emacsXXXXXX");
#else  /* DOS_NT */
  Vtemp_file_name_pattern = build_string ("emXXXXXX");
#endif
  staticpro (&Vtemp_file_name_pattern);

#ifdef MSDOS
  synch_process_tempfile = make_number (0);
  staticpro (&synch_process_tempfile);
#endif

  DEFVAR_LISP ("shell-file-name", Vshell_file_name,
	       doc: /* File name to load inferior shells from.
Initialized from the SHELL environment variable, or to a system-dependent
default if SHELL is unset.  See Info node `(elisp)Security Considerations'.  */);

  DEFVAR_LISP ("exec-path", Vexec_path,
	       doc: /* List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or nil (try default directory).

By default the last element of this list is `exec-directory'. The
last element is not always used, for example in shell completion
\(`shell-dynamic-complete-command').  */);

  DEFVAR_LISP ("exec-suffixes", Vexec_suffixes,
	       doc: /* List of suffixes to try to find executable file names.
Each element is a string.  */);
  Vexec_suffixes = Qnil;

  DEFVAR_LISP ("exec-directory", Vexec_directory,
	       doc: /* Directory for executables for Emacs to invoke.
More generally, this includes any architecture-dependent files
that are built and installed from the Emacs distribution.  */);

  DEFVAR_LISP ("data-directory", Vdata_directory,
	       doc: /* Directory of machine-independent files that come with GNU Emacs.
These are files intended for Emacs to use while it runs.  */);

  DEFVAR_LISP ("doc-directory", Vdoc_directory,
	       doc: /* Directory containing the DOC file that comes with GNU Emacs.
This is usually the same as `data-directory'.  */);

  DEFVAR_LISP ("configure-info-directory", Vconfigure_info_directory,
	       doc: /* For internal use by the build procedure only.
This is the name of the directory in which the build procedure installed
Emacs's info files; the default value for `Info-default-directory-list'
includes this.  */);
  Vconfigure_info_directory = build_string (PATH_INFO);

  DEFVAR_LISP ("shared-game-score-directory", Vshared_game_score_directory,
	       doc: /* Directory of score files for games which come with GNU Emacs.
If this variable is nil, then Emacs is unable to use a shared directory.  */);

  DEFVAR_LISP ("initial-environment", Vinitial_environment,
	       doc: /* List of environment variables inherited from the parent process.
Each element should be a string of the form ENVVARNAME=VALUE.
The elements must normally be decoded (using `locale-coding-system') for use.  */);
  Vinitial_environment = Qnil;

  DEFVAR_LISP ("process-environment", Vprocess_environment,
	       doc: /* List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.

Entries in this list take precedence to those in the frame-local
environments.  Therefore, let-binding `process-environment' is an easy
way to temporarily change the value of an environment variable,
irrespective of where it comes from.  To use `process-environment' to
remove an environment variable, include only its name in the list,
without "=VALUE".

This variable is set to nil when Emacs starts.

If multiple entries define the same variable, the first one always
takes precedence.

Non-ASCII characters are encoded according to the initial value of
`locale-coding-system', i.e. the elements must normally be decoded for
use.

See `setenv' and `getenv'.  */);
  Vprocess_environment = Qnil;

  defsubr (&Scall_process);
  defsubr (&Sgetenv_internal);
  defsubr (&Scall_process_region);
}
