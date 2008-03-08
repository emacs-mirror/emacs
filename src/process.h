/* Definitions for asynchronous process control in GNU Emacs.
   Copyright (C) 1985, 1994, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This structure records information about a subprocess
   or network connection.

   Every field in this structure except for the first two
   must be a Lisp_Object, for GC's sake.  */

struct Lisp_Process
  {
    EMACS_INT size;
    struct Lisp_Vector *v_next;
    /* Descriptor by which we read from this process */
    Lisp_Object infd;
    /* Descriptor by which we write to this process */
    Lisp_Object outfd;
    /* Name of subprocess terminal.  */
    Lisp_Object tty_name;
    /* Name of this process */
    Lisp_Object name;
    /* List of command arguments that this process was run with.
       Is set to t for a stopped network process; nil otherwise. */
    Lisp_Object command;
    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once */
    Lisp_Object filter;
    /* (funcall SENTINEL PROCESS) when process state changes */
    Lisp_Object sentinel;
    /* (funcall LOG SERVER CLIENT MESSAGE) when a server process
       accepts a connection from a client.  */
    Lisp_Object log;
    /* Buffer that output is going to */
    Lisp_Object buffer;
    /* t if this is a real child process.
       For a net connection, it is a plist based on the arguments to make-network-process.  */
    Lisp_Object childp;
    /* Plist for programs to keep per-process state information, parameters, etc.  */
    Lisp_Object plist;
    /* Marker set to end of last buffer-inserted output from this process */
    Lisp_Object mark;
    /* Non-nil means kill silently if Emacs is exited.
       This is the inverse of the `query-on-exit' flag.  */
    Lisp_Object kill_without_query;
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
    /* Coding-system for decoding the input from this process.  */
    Lisp_Object decode_coding_system;
    /* Working buffer for decoding.  */
    Lisp_Object decoding_buf;
    /* Size of carryover in decoding.  */
    Lisp_Object decoding_carryover;
    /* Coding-system for encoding the output to this process.  */
    Lisp_Object encode_coding_system;
    /* Working buffer for encoding.  */
    Lisp_Object encoding_buf;
    /* Size of carryover in encoding.  */
    Lisp_Object encoding_carryover;
    /* Flag to set coding-system of the process buffer from the
       coding_system used to decode process output.  */
    Lisp_Object inherit_coding_system_flag;
    /* Flat to decide the multibyteness of a string given to the
       filter (if any).  It is initialized to the value of
       `default-enable-multibyte-characters' when the process is
       generated, and can be changed by the function
       `set-process-fileter-multibyte'. */
    Lisp_Object filter_multibyte;
    /* Should we delay reading output from this process.
       Initialized from `Vprocess_adaptive_read_buffering'.  */
    Lisp_Object adaptive_read_buffering;
    /* Hysteresis to try to read process output in larger blocks.
       On some systems, e.g. GNU/Linux, Emacs is seen as 
       an interactive app also when reading process output, meaning
       that process output can be read in as little as 1 byte at a
       time.  Value is micro-seconds to delay reading output from
       this process.  Range is 0 .. 50000.  */
    Lisp_Object read_output_delay;
    /* Skip reading this process on next read.  */
    Lisp_Object read_output_skip;

    /* After this point, there are no Lisp_Objects any more.  */

    /* Number of this process.
       allocate_process assumes this is the first non-Lisp_Object field.
       A value 0 is used for pseudo-processes such as network connections.  */
    pid_t pid;
    /* Record the process status in the raw form in which it comes from `wait'.
       This is to avoid consing in a signal handler.  The `raw_status_new'
       flag indicates that `raw_status' contains a new status that still
       needs to be synced to `status'.  */
    unsigned int raw_status_new : 1;
    int raw_status;
};

/* Every field in the preceding structure except for the first two
   must be a Lisp_Object, for GC's sake.  */

#define ChannelMask(n) (1<<(n))

/* Indexed by descriptor, gives the process (if any) for that descriptor.  */
extern Lisp_Object chan_process[];

/* Alist of elements (NAME . PROCESS).  */
extern Lisp_Object Vprocess_alist;

/* True if we are about to fork off a synchronous process or if we
   are waiting for it.  */
extern int synch_process_alive;

/* Communicate exit status of sync process to from sigchld_handler
   to Fcall_process.  */

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
extern char *synch_process_death;

/* Nonzero => this is the signal number that terminated the subprocess.  */
extern int synch_process_termsig;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
extern int synch_process_retcode;

/* The name of the file open to get a null file, or a data sink.
   VMS, MS-DOS, and OS/2 redefine this.  */
#ifndef NULL_DEVICE
#define NULL_DEVICE "/dev/null"
#endif

/* Nonzero means don't run process sentinels.  This is used
   when exiting.  */
extern int inhibit_sentinels;

/* arch-tag: dffedfc4-d7bc-4b58-a26f-c16155449c72
   (do not change this comment) */
