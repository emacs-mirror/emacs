/* Definitions for asynchronous process control in GNU Emacs.
   Copyright (C) 1985, 1994 Free Software Foundation, Inc.

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


/*
 * Structure records pertinent information about open channels.
 * There is one channel associated with each process.
 */

struct Lisp_Process
  {
    EMACS_INT size;
    struct Lisp_Vector *v_next;
    /* Descriptor by which we read from this process */
    Lisp_Object infd;
    /* Descriptor by which we write to this process */
    Lisp_Object outfd;
    /* Descriptor for the tty which this process is using.
       nil if we didn't record it (on some systems, there's no need).  */
    Lisp_Object subtty;
    /* Name of subprocess terminal.  */
    Lisp_Object tty_name;
    /* Name of this process */
    Lisp_Object name;
    /* List of command arguments that this process was run with */
    Lisp_Object command;
    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once */
    Lisp_Object filter;
    /* (funcall SENTINEL PROCESS) when process state changes */
    Lisp_Object sentinel;
    /* Buffer that output is going to */
    Lisp_Object buffer;
    /* Number of this process */
    Lisp_Object pid;
    /* Non-nil if this is really a command channel */
    Lisp_Object command_channel_p;
    /* Non-nil if this is really a child process */
    Lisp_Object childp;
    /* Marker set to end of last buffer-inserted output from this process */
    Lisp_Object mark;
    /* Non-nil means kill silently if Emacs is exited.  */
    Lisp_Object kill_without_query;
    /* Record the process status in the raw form in which it comes from `wait'.
       This is to avoid consing in a signal handler.  */
    Lisp_Object raw_status_low;
    Lisp_Object raw_status_high;
    /* Symbol indicating status of process.
       This may be a symbol: run, open, or closed.
       Or it may be a list, whose car is stop, exit or signal
       and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
       or (SIGNAL_NUMBER . COREDUMP_FLAG).  */
    Lisp_Object status;
    /* Non-nil if communicating through a pty.  */
    Lisp_Object pty_flag;
    /* Event-count of last event in which this process changed status.  */
    Lisp_Object tick;
    /* Event-count of last such event reported.  */
    Lisp_Object update_tick;
};

#define ChannelMask(n) (1<<(n))

/* Indexed by descriptor, gives the process (if any) for that descriptor.  */
extern Lisp_Object chan_process[];

/* Alist of elements (NAME . PROCESS).  */
extern Lisp_Object Vprocess_alist;

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
extern int synch_process_alive;

/* Communicate exit status of synch process to from sigchld_handler
   to Fcall_process.  */
extern int synch_process_retcode;
extern char *synch_process_death;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
extern char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
extern int synch_process_retcode;

/* The name of the file open to get a null file, or a data sink.
   VMS, MS-DOS, and OS/2 redefine this.  */
#ifndef NULL_DEVICE
#define NULL_DEVICE "/dev/null"
#endif

/* A string listing the possible suffixes used for executable files,
   separated by colons.  VMS, MS-DOS, and OS/2 redefine this.  */
#ifndef EXEC_SUFFIXES
#define EXEC_SUFFIXES ""
#endif
