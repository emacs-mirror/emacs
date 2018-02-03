/* Definitions for asynchronous process control in GNU Emacs.
   Copyright (C) 1985, 1994, 2001-2018 Free Software Foundation, Inc.

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

#ifndef EMACS_PROCESS_H
#define EMACS_PROCESS_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <unistd.h>

#ifdef HAVE_GNUTLS
#include "gnutls.h"
#endif

INLINE_HEADER_BEGIN

/* Bound on number of file descriptors opened on behalf of a process,
   that need to be closed.  */

enum { PROCESS_OPEN_FDS = 6 };

/* This structure records information about a subprocess
   or network connection.  */

struct Lisp_Process
  {
    union vectorlike_header header;

    /* Name of subprocess terminal.  */
    Lisp_Object tty_name;

    /* Name of this process.  */
    Lisp_Object name;

    /* List of command arguments that this process was run with.
       Is set to t for a stopped network process; nil otherwise.  */
    Lisp_Object command;

    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once.  */
    Lisp_Object filter;

    /* (funcall SENTINEL PROCESS) when process state changes.  */
    Lisp_Object sentinel;

    /* (funcall LOG SERVER CLIENT MESSAGE) when a server process
       accepts a connection from a client.  */
    Lisp_Object log;

    /* Buffer that output is going to.  */
    Lisp_Object buffer;

    /* t if this is a real child process.  For a network or serial
       connection, it is a plist based on the arguments to
       make-network-process or make-serial-process.  */

    Lisp_Object childp;

    /* Plist for programs to keep per-process state information, parameters, etc.  */
    Lisp_Object plist;

    /* Symbol indicating the type of process: real, network, serial.  */
    Lisp_Object type;

    /* Marker set to end of last buffer-inserted output from this process.  */
    Lisp_Object mark;

    /* Symbol indicating status of process.
       This may be a symbol: run, open, closed, listen, or failed.
       Or it may be a pair (connect . ADDRINFOS) where ADDRINFOS is
       a list of remaining (PROTOCOL . ADDRINFO) pairs to try.
       Or it may be (failed ERR) where ERR is an integer, string or symbol.
       Or it may be a list, whose car is stop, exit or signal
       and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
       or (SIGNAL_NUMBER . COREDUMP_FLAG).  */
    Lisp_Object status;

    /* Coding-system for decoding the input from this process.  */
    Lisp_Object decode_coding_system;

    /* Working buffer for decoding.  */
    Lisp_Object decoding_buf;

    /* Coding-system for encoding the output to this process.  */
    Lisp_Object encode_coding_system;

    /* Working buffer for encoding.  */
    Lisp_Object encoding_buf;

    /* Queue for storing waiting writes.  */
    Lisp_Object write_queue;

#ifdef HAVE_GNUTLS
    Lisp_Object gnutls_cred_type;
    Lisp_Object gnutls_boot_parameters;
#endif

    /* Pipe process attached to the standard error of this process.  */
    Lisp_Object stderrproc;

    /* The thread a process is linked to, or nil for any thread.  */
    Lisp_Object thread;

    /* After this point, there are no Lisp_Objects any more.  */
    /* alloc.c assumes that `pid' is the first such non-Lisp slot.  */

    /* Process ID.  A positive value is a child process ID.
       Zero is for pseudo-processes such as network or serial connections,
       or for processes that have not been fully created yet.
       -1 is for a process that was not created successfully.
       -2 is for a pty with no process, e.g., for GDB.  */
    pid_t pid;
    /* Descriptor by which we read from this process.  */
    int infd;
    /* Descriptor by which we write to this process.  */
    int outfd;
    /* Descriptors that were created for this process and that need
       closing.  Unused entries are negative.  */
    int open_fd[PROCESS_OPEN_FDS];
    /* Event-count of last event in which this process changed status.  */
    EMACS_INT tick;
    /* Event-count of last such event reported.  */
    EMACS_INT update_tick;
    /* Size of carryover in decoding.  */
    int decoding_carryover;
    /* Hysteresis to try to read process output in larger blocks.
       On some systems, e.g. GNU/Linux, Emacs is seen as
       an interactive app also when reading process output, meaning
       that process output can be read in as little as 1 byte at a
       time.  Value is nanoseconds to delay reading output from
       this process.  Range is 0 .. 50 * 1000 * 1000.  */
    int read_output_delay;
    /* Should we delay reading output from this process.
       Initialized from `Vprocess_adaptive_read_buffering'.
       0 = nil, 1 = t, 2 = other.  */
    unsigned int adaptive_read_buffering : 2;
    /* Skip reading this process on next read.  */
    bool_bf read_output_skip : 1;
    /* True means kill silently if Emacs is exited.
       This is the inverse of the `query-on-exit' flag.  */
    bool_bf kill_without_query : 1;
    /* True if communicating through a pty.  */
    bool_bf pty_flag : 1;
    /* Flag to set coding-system of the process buffer from the
       coding_system used to decode process output.  */
    bool_bf inherit_coding_system_flag : 1;
    /* Whether the process is alive, i.e., can be waited for.  Running
       processes can be waited for, but exited and fake processes cannot.  */
    bool_bf alive : 1;
    /* Record the process status in the raw form in which it comes from `wait'.
       This is to avoid consing in a signal handler.  The `raw_status_new'
       flag indicates that `raw_status' contains a new status that still
       needs to be synced to `status'.  */
    bool_bf raw_status_new : 1;
    /* Whether this is a nonblocking socket. */
    bool_bf is_non_blocking_client : 1;
    /* Whether this is a server or a client socket. */
    bool_bf is_server : 1;
    int raw_status;
    /* The length of the socket backlog. */
    int backlog;
    /* The port number. */
    int port;
    /* The socket type. */
    int socktype;

#ifdef HAVE_GETADDRINFO_A
    /* Whether the socket is waiting for response from an asynchronous
       DNS call. */
    struct gaicb *dns_request;
#endif

#ifdef HAVE_GNUTLS
    gnutls_initstage_t gnutls_initstage;
    gnutls_session_t gnutls_state;
    gnutls_certificate_client_credentials gnutls_x509_cred;
    gnutls_anon_client_credentials_t gnutls_anon_cred;
    gnutls_x509_crt_t gnutls_certificate;
    unsigned int gnutls_peer_verification;
    unsigned int gnutls_extra_peer_verification;
    int gnutls_log_level;
    int gnutls_handshakes_tried;
    bool_bf gnutls_p : 1;
    bool_bf gnutls_complete_negotiation_p : 1;
#endif
};

INLINE bool
PROCESSP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_PROCESS);
}

INLINE void
CHECK_PROCESS (Lisp_Object x)
{
  CHECK_TYPE (PROCESSP (x), Qprocessp, x);
}

INLINE struct Lisp_Process *
XPROCESS (Lisp_Object a)
{
  eassert (PROCESSP (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

/* Every field in the preceding structure except for the first two
   must be a Lisp_Object, for GC's sake.  */

#define ChannelMask(n) (1 << (n))

/* Most code should use these functions to set Lisp fields in struct
   process.  */

INLINE void
pset_childp (struct Lisp_Process *p, Lisp_Object val)
{
  p->childp = val;
}

INLINE void
pset_status (struct Lisp_Process *p, Lisp_Object val)
{
  p->status = val;
}

#ifdef HAVE_GNUTLS
INLINE void
pset_gnutls_cred_type (struct Lisp_Process *p, Lisp_Object val)
{
  p->gnutls_cred_type = val;
}
#endif

/* True means don't run process sentinels.  This is used
   when exiting.  */
extern bool inhibit_sentinels;

/* Exit statuses for GNU programs that exec other programs.  */
enum
{
  EXIT_CANCELED = 125, /* Internal error prior to exec attempt.  */
  EXIT_CANNOT_INVOKE = 126, /* Program located, but not usable.  */
  EXIT_ENOENT = 127 /* Could not find program to exec.  */
};

/* Defined in callproc.c.  */

extern Lisp_Object encode_current_directory (void);
extern void record_kill_process (struct Lisp_Process *, Lisp_Object);

/* Defined in sysdep.c.  */

extern Lisp_Object list_system_processes (void);
extern Lisp_Object system_process_attributes (Lisp_Object);

/* Defined in process.c.  */

extern void record_deleted_pid (pid_t, Lisp_Object);
struct sockaddr;
extern Lisp_Object conv_sockaddr_to_lisp (struct sockaddr *, ptrdiff_t);
extern void hold_keyboard_input (void);
extern void unhold_keyboard_input (void);
extern bool kbd_on_hold_p (void);

typedef void (*fd_callback) (int fd, void *data);

extern void add_read_fd (int fd, fd_callback func, void *data);
extern void delete_read_fd (int fd);
extern void add_write_fd (int fd, fd_callback func, void *data);
extern void delete_write_fd (int fd);
extern void catch_child_signal (void);
extern void restore_nofile_limit (void);

#ifdef WINDOWSNT
extern Lisp_Object network_interface_list (void);
extern Lisp_Object network_interface_info (Lisp_Object);
#endif

extern Lisp_Object remove_slash_colon (Lisp_Object);

extern void update_processes_for_thread_death (Lisp_Object);

INLINE_HEADER_END

#endif /* EMACS_PROCESS_H */
