/* Asynchronous subprocess control for GNU Emacs.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95, 96, 98, 1999,
      2001, 2002 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#define _GNU_SOURCE		/* to get strsignal declared with glibc 2 */
#include <config.h>
#include <signal.h>

/* This file is split into two parts by the following preprocessor
   conditional.  The 'then' clause contains all of the support for
   asynchronous subprocesses.  The 'else' clause contains stub
   versions of some of the asynchronous subprocess routines that are
   often called elsewhere in Emacs, so we don't have to #ifdef the
   sections that call them.  */


#ifdef subprocesses

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#include <sys/file.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined(WINDOWSNT) || defined(UNIX98_PTYS)
#include <stdlib.h>
#include <fcntl.h>
#endif /* not WINDOWSNT */

#ifdef HAVE_SOCKETS	/* TCP connection support, if kernel can do it */
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#ifdef NEED_NET_ERRNO_H
#include <net/errno.h>
#endif /* NEED_NET_ERRNO_H */

/* Are local (unix) sockets supported?  */
#if defined (HAVE_SYS_UN_H) && !defined (NO_SOCKETS_IN_FILE_SYSTEM)
#if !defined (AF_LOCAL) && defined (AF_UNIX)
#define AF_LOCAL AF_UNIX
#endif
#ifdef AF_LOCAL
#define HAVE_LOCAL_SOCKETS
#include <sys/un.h>
#endif
#endif
#endif /* HAVE_SOCKETS */

/* TERM is a poor-man's SLIP, used on GNU/Linux.  */
#ifdef TERM
#include <client.h>
#endif

/* On some systems, e.g. DGUX, inet_addr returns a 'struct in_addr'. */
#ifdef HAVE_BROKEN_INET_ADDR
#define IN_ADDR struct in_addr
#define NUMERIC_ADDR_ERROR (numeric_addr.s_addr == -1)
#else
#define IN_ADDR unsigned long
#define NUMERIC_ADDR_ERROR (numeric_addr == -1)
#endif

#if defined(BSD_SYSTEM) || defined(STRIDE)
#include <sys/ioctl.h>
#if !defined (O_NDELAY) && defined (HAVE_PTYS) && !defined(USG5)
#include <fcntl.h>
#endif /* HAVE_PTYS and no O_NDELAY */
#endif /* BSD_SYSTEM || STRIDE */

#ifdef BROKEN_O_NONBLOCK
#undef O_NONBLOCK
#endif /* BROKEN_O_NONBLOCK */

#ifdef NEED_BSDTTY
#include <bsdtty.h>
#endif

#ifdef IRIS
#include <sys/sysmacros.h>	/* for "minor" */
#endif /* not IRIS */

#include "systime.h"
#include "systty.h"

#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"
#include "process.h"
#include "termhooks.h"
#include "termopts.h"
#include "commands.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "dispextern.h"
#include "composite.h"
#include "atimer.h"

Lisp_Object Qprocessp;
Lisp_Object Qrun, Qstop, Qsignal;
Lisp_Object Qopen, Qclosed, Qconnect, Qfailed, Qlisten;
Lisp_Object Qlocal, Qdatagram;
Lisp_Object QCname, QCbuffer, QChost, QCservice, QCtype;
Lisp_Object QClocal, QCremote, QCcoding;
Lisp_Object QCserver, QCnowait, QCnoquery, QCstop;
Lisp_Object QCsentinel, QClog, QCoptions;
Lisp_Object Qlast_nonmenu_event;
/* QCfamily is declared and initialized in xfaces.c,
   QCfilter in keyboard.c.  */
extern Lisp_Object QCfamily, QCfilter;

/* Qexit is declared and initialized in eval.c.  */

/* QCfamily is defined in xfaces.c.  */
extern Lisp_Object QCfamily;
/* QCfilter is defined in keyboard.c.  */
extern Lisp_Object QCfilter;

/* a process object is a network connection when its childp field is neither
   Qt nor Qnil but is instead a cons cell (HOSTNAME PORTNUM).  */

#ifdef HAVE_SOCKETS
#define NETCONN_P(p) (GC_CONSP (XPROCESS (p)->childp))
#define NETCONN1_P(p) (GC_CONSP ((p)->childp))
#else
#define NETCONN_P(p) 0
#define NETCONN1_P(p) 0
#endif /* HAVE_SOCKETS */

/* Define first descriptor number available for subprocesses.  */
#ifdef VMS
#define FIRST_PROC_DESC 1
#else /* Not VMS */
#define FIRST_PROC_DESC 3
#endif

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif /* SIGCLD */

#include "syssignal.h"

#include "syswait.h"

extern void set_waiting_for_input P_ ((EMACS_TIME *));

#ifndef USE_CRT_DLL
extern int errno;
#endif
#ifdef VMS
extern char *sys_errlist[];
#endif

#ifndef HAVE_H_ERRNO
extern int h_errno;
#endif

/* t means use pty, nil means use a pipe,
   maybe other values to come.  */
static Lisp_Object Vprocess_connection_type;

#ifdef SKTPAIR
#ifndef HAVE_SOCKETS
#include <sys/socket.h>
#endif
#endif /* SKTPAIR */

/* These next two vars are non-static since sysdep.c uses them in the
   emulation of `select'.  */
/* Number of events of change of status of a process.  */
int process_tick;
/* Number of events for which the user or sentinel has been notified.  */
int update_tick;

/* Define NON_BLOCKING_CONNECT if we can support non-blocking connects.  */

#ifdef BROKEN_NON_BLOCKING_CONNECT
#undef NON_BLOCKING_CONNECT
#else
#ifndef NON_BLOCKING_CONNECT
#ifdef HAVE_SOCKETS
#ifdef HAVE_SELECT
#if defined (HAVE_GETPEERNAME) || defined (GNU_LINUX)
#if defined (O_NONBLOCK) || defined (O_NDELAY)
#if defined (EWOULDBLOCK) || defined (EINPROGRESS)
#define NON_BLOCKING_CONNECT
#endif /* EWOULDBLOCK || EINPROGRESS */
#endif /* O_NONBLOCK || O_NDELAY */
#endif /* HAVE_GETPEERNAME || GNU_LINUX */
#endif /* HAVE_SELECT */
#endif /* HAVE_SOCKETS */
#endif /* NON_BLOCKING_CONNECT */
#endif /* BROKEN_NON_BLOCKING_CONNECT */

/* Define DATAGRAM_SOCKETS if datagrams can be used safely on
   this system.  We need to read full packets, so we need a
   "non-destructive" select.  So we require either native select,
   or emulation of select using FIONREAD.  */

#ifdef BROKEN_DATAGRAM_SOCKETS
#undef DATAGRAM_SOCKETS
#else
#ifndef DATAGRAM_SOCKETS
#ifdef HAVE_SOCKETS
#if defined (HAVE_SELECT) || defined (FIONREAD)
#if defined (HAVE_SENDTO) && defined (HAVE_RECVFROM) && defined (EMSGSIZE)
#define DATAGRAM_SOCKETS
#endif /* HAVE_SENDTO && HAVE_RECVFROM && EMSGSIZE */
#endif /* HAVE_SELECT || FIONREAD */
#endif /* HAVE_SOCKETS */
#endif /* DATAGRAM_SOCKETS */
#endif /* BROKEN_DATAGRAM_SOCKETS */

#ifdef TERM
#undef NON_BLOCKING_CONNECT
#undef DATAGRAM_SOCKETS
#endif


#include "sysselect.h"

extern int keyboard_bit_set P_ ((SELECT_TYPE *));

/* If we support a window system, turn on the code to poll periodically
   to detect C-g.  It isn't actually used when doing interrupt input.  */
#ifdef HAVE_WINDOW_SYSTEM
#define POLL_FOR_INPUT
#endif

/* Mask of bits indicating the descriptors that we wait for input on.  */

static SELECT_TYPE input_wait_mask;

/* Mask that excludes keyboard input descriptor (s).  */

static SELECT_TYPE non_keyboard_wait_mask;

/* Mask that excludes process input descriptor (s).  */

static SELECT_TYPE non_process_wait_mask;

/* Mask of bits indicating the descriptors that we wait for connect to
   complete on.  Once they complete, they are removed from this mask
   and added to the input_wait_mask and non_keyboard_wait_mask.  */

static SELECT_TYPE connect_wait_mask;

/* Number of bits set in connect_wait_mask.  */
static int num_pending_connects;

/* The largest descriptor currently in use for a process object.  */
static int max_process_desc;

/* The largest descriptor currently in use for keyboard input.  */
static int max_keyboard_desc;

/* Nonzero means delete a process right away if it exits.  */
static int delete_exited_processes;

/* Indexed by descriptor, gives the process (if any) for that descriptor */
Lisp_Object chan_process[MAXDESC];

/* Alist of elements (NAME . PROCESS) */
Lisp_Object Vprocess_alist;

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */

/* Don't make static; need to access externally.  */
int proc_buffered_char[MAXDESC];

/* Table of `struct coding-system' for each process.  */
static struct coding_system *proc_decode_coding_system[MAXDESC];
static struct coding_system *proc_encode_coding_system[MAXDESC];

#ifdef DATAGRAM_SOCKETS
/* Table of `partner address' for datagram sockets.  */
struct sockaddr_and_len {
  struct sockaddr *sa;
  int len;
} datagram_address[MAXDESC];
#define DATAGRAM_CHAN_P(chan)	(datagram_address[chan].sa != 0)
#define DATAGRAM_CONN_P(proc)	(PROCESSP (proc) && datagram_address[XINT (XPROCESS (proc)->infd)].sa != 0)
#else
#define DATAGRAM_CHAN_P(chan)	(0)
#define DATAGRAM_CONN_P(proc)	(0)
#endif

static Lisp_Object get_process ();
static void exec_sentinel ();

extern EMACS_TIME timer_check ();
extern int timers_run;

/* Maximum number of bytes to send to a pty without an eof.  */
static int pty_max_bytes;

extern Lisp_Object Vfile_name_coding_system, Vdefault_file_name_coding_system;

#ifdef HAVE_PTYS
/* The file name of the pty opened by allocate_pty.  */

static char pty_name[24];
#endif

/* Compute the Lisp form of the process status, p->status, from
   the numeric status that was returned by `wait'.  */

Lisp_Object status_convert ();

void
update_status (p)
     struct Lisp_Process *p;
{
  union { int i; WAITTYPE wt; } u;
  u.i = XFASTINT (p->raw_status_low) + (XFASTINT (p->raw_status_high) << 16);
  p->status = status_convert (u.wt);
  p->raw_status_low = Qnil;
  p->raw_status_high = Qnil;
}

/*  Convert a process status word in Unix format to 
    the list that we use internally.  */

Lisp_Object
status_convert (w)
     WAITTYPE w;
{
  if (WIFSTOPPED (w))
    return Fcons (Qstop, Fcons (make_number (WSTOPSIG (w)), Qnil));
  else if (WIFEXITED (w))
    return Fcons (Qexit, Fcons (make_number (WRETCODE (w)),
				WCOREDUMP (w) ? Qt : Qnil));
  else if (WIFSIGNALED (w))
    return Fcons (Qsignal, Fcons (make_number (WTERMSIG (w)),
				  WCOREDUMP (w) ? Qt : Qnil));
  else
    return Qrun;
}

/* Given a status-list, extract the three pieces of information
   and store them individually through the three pointers.  */

void
decode_status (l, symbol, code, coredump)
     Lisp_Object l;
     Lisp_Object *symbol;
     int *code;
     int *coredump;
{
  Lisp_Object tem;

  if (SYMBOLP (l))
    {
      *symbol = l;
      *code = 0;
      *coredump = 0;
    }
  else
    {
      *symbol = XCAR (l);
      tem = XCDR (l);
      *code = XFASTINT (XCAR (tem));
      tem = XCDR (tem);
      *coredump = !NILP (tem);
    }
}

/* Return a string describing a process status list.  */

Lisp_Object 
status_message (status)
     Lisp_Object status;
{
  Lisp_Object symbol;
  int code, coredump;
  Lisp_Object string, string2;

  decode_status (status, &symbol, &code, &coredump);

  if (EQ (symbol, Qsignal) || EQ (symbol, Qstop))
    {
      char *signame;
      synchronize_system_messages_locale ();
      signame = strsignal (code);
      if (signame == 0)
	signame = "unknown";
      string = build_string (signame);
      string2 = build_string (coredump ? " (core dumped)\n" : "\n");
      XSTRING (string)->data[0] = DOWNCASE (XSTRING (string)->data[0]);
      return concat2 (string, string2);
    }
  else if (EQ (symbol, Qexit))
    {
      if (code == 0)
	return build_string ("finished\n");
      string = Fnumber_to_string (make_number (code));
      string2 = build_string (coredump ? " (core dumped)\n" : "\n");
      return concat3 (build_string ("exited abnormally with code "),
		      string, string2);
    }
  else if (EQ (symbol, Qfailed))
    {
      string = Fnumber_to_string (make_number (code));
      string2 = build_string ("\n");
      return concat3 (build_string ("failed with code "),
		      string, string2);
    }
  else
    return Fcopy_sequence (Fsymbol_name (symbol));
}

#ifdef HAVE_PTYS

/* Open an available pty, returning a file descriptor.
   Return -1 on failure.
   The file name of the terminal corresponding to the pty
   is left in the variable pty_name.  */

int
allocate_pty ()
{
  struct stat stb;
  register int c, i;
  int fd;

  /* Some systems name their pseudoterminals so that there are gaps in
     the usual sequence - for example, on HP9000/S700 systems, there
     are no pseudoterminals with names ending in 'f'.  So we wait for
     three failures in a row before deciding that we've reached the
     end of the ptys.  */
  int failed_count = 0;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
#ifdef IRIS
	/* Unusual IRIS code */
 	*ptyv = emacs_open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
 	if (fd < 0)
 	  return -1;
	if (fstat (fd, &stb) < 0)
	  return -1;
#else /* not IRIS */
	if (stat (pty_name, &stb) < 0)
	  {
	    failed_count++;
	    if (failed_count >= 3)
	      return -1;
	  }
	else
	  failed_count = 0;
#ifdef O_NONBLOCK
	fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#else
	fd = emacs_open (pty_name, O_RDWR | O_NDELAY, 0);
#endif
#endif /* not IRIS */
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
            sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* no PTY_TTY_NAME_SPRINTF */
#ifndef UNIPLUS
	    if (access (pty_name, 6) != 0)
	      {
		emacs_close (fd);
#if !defined(IRIS) && !defined(__sgi)
		continue;
#else
		return -1;
#endif /* IRIS */
	      }
#endif /* not UNIPLUS */
	    setup_pty (fd);
	    return fd;
	  }
      }
  return -1;
}
#endif /* HAVE_PTYS */

Lisp_Object
make_process (name)
     Lisp_Object name;
{
  register Lisp_Object val, tem, name1;
  register struct Lisp_Process *p;
  char suffix[10];
  register int i;

  p = allocate_process ();

  XSETINT (p->infd, -1);
  XSETINT (p->outfd, -1);
  XSETFASTINT (p->pid, 0);
  XSETFASTINT (p->tick, 0);
  XSETFASTINT (p->update_tick, 0);
  p->raw_status_low = Qnil;
  p->raw_status_high = Qnil;
  p->status = Qrun;
  p->mark = Fmake_marker ();

  /* If name is already in use, modify it until it is unused.  */

  name1 = name;
  for (i = 1; ; i++)
    {
      tem = Fget_process (name1);
      if (NILP (tem)) break;
      sprintf (suffix, "<%d>", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;
  XSETPROCESS (val, p);
  Vprocess_alist = Fcons (Fcons (name, val), Vprocess_alist);
  return val;
}

void
remove_process (proc)
     register Lisp_Object proc;
{
  register Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);

  deactivate_process (proc);
}

DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
       doc: /* Return t if OBJECT is a process.  */)
     (object)
     Lisp_Object object;
{
  return PROCESSP (object) ? Qt : Qnil;
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
       doc: /* Return the process named NAME, or nil if there is none.  */)
     (name)
     register Lisp_Object name;
{
  if (PROCESSP (name))
    return name;
  CHECK_STRING (name);
  return Fcdr (Fassoc (name, Vprocess_alist));
}

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
       doc: /* Return the (or a) process associated with BUFFER.
BUFFER may be a buffer or the name of one.  */)
     (buffer)
     register Lisp_Object buffer;
{
  register Lisp_Object buf, tail, proc;

  if (NILP (buffer)) return Qnil;
  buf = Fget_buffer (buffer);
  if (NILP (buf)) return Qnil;

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      proc = Fcdr (Fcar (tail));
      if (PROCESSP (proc) && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
  return Qnil;
}

/* This is how commands for the user decode process arguments.  It
   accepts a process, a process name, a buffer, a buffer name, or nil.
   Buffers denote the first process in the buffer, and nil denotes the
   current buffer.  */

static Lisp_Object
get_process (name)
     register Lisp_Object name;
{
  register Lisp_Object proc, obj;
  if (STRINGP (name))
    {
      obj = Fget_process (name);
      if (NILP (obj))
	obj = Fget_buffer (name);
      if (NILP (obj))
	error ("Process %s does not exist", XSTRING (name)->data);
    }
  else if (NILP (name))
    obj = Fcurrent_buffer ();
  else
    obj = name;

  /* Now obj should be either a buffer object or a process object.
   */
  if (BUFFERP (obj))
    {
      proc = Fget_buffer_process (obj);
      if (NILP (proc))
	error ("Buffer %s has no process", XSTRING (XBUFFER (obj)->name)->data);
    }
  else
    {
      CHECK_PROCESS (obj);
      proc = obj;
    }
  return proc;
}

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
       doc: /* Delete PROCESS: kill it and forget about it immediately.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.  */)
     (process)
     register Lisp_Object process;
{
  process = get_process (process);
  XPROCESS (process)->raw_status_low = Qnil;
  XPROCESS (process)->raw_status_high = Qnil;
  if (NETCONN_P (process))
    {
      XPROCESS (process)->status = Fcons (Qexit, Fcons (make_number (0), Qnil));
      XSETINT (XPROCESS (process)->tick, ++process_tick);
    }
  else if (XINT (XPROCESS (process)->infd) >= 0)
    {
      Fkill_process (process, Qnil);
      /* Do this now, since remove_process will make sigchld_handler do nothing.  */
      XPROCESS (process)->status 
	= Fcons (Qsignal, Fcons (make_number (SIGKILL), Qnil));
      XSETINT (XPROCESS (process)->tick, ++process_tick);
      status_notify ();
    }
  remove_process (process);
  return Qnil;
}

DEFUN ("process-status", Fprocess_status, Sprocess_status, 1, 1, 0,
       doc: /* Return the status of PROCESS.
The returned value is one of the following symbols:
run  -- for a process that is running.
stop -- for a process stopped but continuable.
exit -- for a process that has exited.
signal -- for a process that has got a fatal signal.
open -- for a network stream connection that is open.
listen -- for a network stream server that is listening.
closed -- for a network stream connection that is closed.
connect -- when waiting for a non-blocking connection to complete.
failed -- when a non-blocking connection has failed.
nil -- if arg is a process name and no such process exists.
PROCESS may be a process, a buffer, the name of a process, or
nil, indicating the current buffer's process.  */)
     (process)
     register Lisp_Object process;
{
  register struct Lisp_Process *p;
  register Lisp_Object status;

  if (STRINGP (process))
    process = Fget_process (process);
  else
    process = get_process (process);

  if (NILP (process))
    return process;

  p = XPROCESS (process);
  if (!NILP (p->raw_status_low))
    update_status (p);
  status = p->status;
  if (CONSP (status))
    status = XCAR (status);
  if (NETCONN1_P (p))
    {
      if (EQ (status, Qexit))
	status = Qclosed;
      else if (EQ (p->command, Qt))
	status = Qstop;
      else if (EQ (status, Qrun))
	status = Qopen;
    }
  return status;
}

DEFUN ("process-exit-status", Fprocess_exit_status, Sprocess_exit_status,
       1, 1, 0,
       doc: /* Return the exit status of PROCESS or the signal number that killed it.
If PROCESS has not yet exited or died, return 0.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  if (!NILP (XPROCESS (process)->raw_status_low))
    update_status (XPROCESS (process));
  if (CONSP (XPROCESS (process)->status))
    return XCAR (XCDR (XPROCESS (process)->status));
  return make_number (0);
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
       doc: /* Return the process id of PROCESS.
This is the pid of the Unix process which PROCESS uses or talks to.
For a network connection, this value is nil.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->pid;
}

DEFUN ("process-name", Fprocess_name, Sprocess_name, 1, 1, 0,
       doc: /* Return the name of PROCESS, as a string.
This is the name of the program invoked in PROCESS,
possibly modified to make it unique among process names.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->name;
}

DEFUN ("process-command", Fprocess_command, Sprocess_command, 1, 1, 0,
       doc: /* Return the command that was executed to start PROCESS.
This is a list of strings, the first string being the program executed
and the rest of the strings being the arguments given to it.
For a non-child channel, this is nil.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->command;
}

DEFUN ("process-tty-name", Fprocess_tty_name, Sprocess_tty_name, 1, 1, 0,
       doc: /* Return the name of the terminal PROCESS uses, or nil if none.
This is the terminal that the process itself reads and writes on,
not the name of the pty that Emacs uses to talk with that terminal.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->tty_name;
}

DEFUN ("set-process-buffer", Fset_process_buffer, Sset_process_buffer,
       2, 2, 0,
       doc: /* Set buffer associated with PROCESS to BUFFER (a buffer, or nil).  */)
     (process, buffer)
     register Lisp_Object process, buffer;
{
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer);
  p = XPROCESS (process);
  p->buffer = buffer;
  if (NETCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCbuffer, buffer);
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, Sprocess_buffer,
       1, 1, 0,
       doc: /* Return the buffer PROCESS is associated with.
Output from PROCESS is inserted in this buffer unless PROCESS has a filter.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, Sprocess_mark,
       1, 1, 0,
       doc: /* Return the marker for the end of the last output from PROCESS.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->mark;
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
       2, 2, 0,
       doc: /* Give PROCESS the filter function FILTER; nil means no filter.
t means stop accepting output from the process.
When a process has a filter, each time it does output
the entire string of output is passed to the filter.
The filter gets two arguments: the process and the string of output.
If the process has a filter, its buffer is not used for output.  */)
     (process, filter)
     register Lisp_Object process, filter;
{
  struct Lisp_Process *p;
  
  CHECK_PROCESS (process);
  p = XPROCESS (process);

  /* Don't signal an error if the process' input file descriptor
     is closed.  This could make debugging Lisp more difficult,
     for example when doing something like

     (setq process (start-process ...))
     (debug)
     (set-process-filter process ...)  */
  
  if (XINT (p->infd) >= 0)
    {
      if (EQ (filter, Qt) && !EQ (p->status, Qlisten))
	{
	  FD_CLR (XINT (p->infd), &input_wait_mask);
	  FD_CLR (XINT (p->infd), &non_keyboard_wait_mask);
	}
      else if (EQ (p->filter, Qt)
	       && !EQ (p->command, Qt)) /* Network process not stopped. */
	{
	  FD_SET (XINT (p->infd), &input_wait_mask);
	  FD_SET (XINT (p->infd), &non_keyboard_wait_mask);
	}
    }
  
  p->filter = filter;
  if (NETCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCfilter, filter);
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, Sprocess_filter,
       1, 1, 0,
       doc: /* Returns the filter function of PROCESS; nil if none.
See `set-process-filter' for more info on filter functions.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->filter;
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, Sset_process_sentinel,
       2, 2, 0,
       doc: /* Give PROCESS the sentinel SENTINEL; nil for none.
The sentinel is called as a function when the process changes state.
It gets two arguments: the process, and a string describing the change.  */)
     (process, sentinel)
     register Lisp_Object process, sentinel;
{
  CHECK_PROCESS (process);
  XPROCESS (process)->sentinel = sentinel;
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, Sprocess_sentinel,
       1, 1, 0,
       doc: /* Return the sentinel of PROCESS; nil if none.
See `set-process-sentinel' for more info on sentinels.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->sentinel;
}

DEFUN ("set-process-window-size", Fset_process_window_size,
       Sset_process_window_size, 3, 3, 0,
       doc: /* Tell PROCESS that it has logical window size HEIGHT and WIDTH.  */)
     (process, height, width)
     register Lisp_Object process, height, width;
{
  CHECK_PROCESS (process);
  CHECK_NATNUM (height);
  CHECK_NATNUM (width);
  
  if (XINT (XPROCESS (process)->infd) < 0
      || set_window_size (XINT (XPROCESS (process)->infd),
			  XINT (height), XINT (width)) <= 0)
    return Qnil;
  else
    return Qt;
}

DEFUN ("set-process-inherit-coding-system-flag",
       Fset_process_inherit_coding_system_flag,
       Sset_process_inherit_coding_system_flag, 2, 2, 0,
       doc: /* Determine whether buffer of PROCESS will inherit coding-system.
If the second argument FLAG is non-nil, then the variable
`buffer-file-coding-system' of the buffer associated with PROCESS
will be bound to the value of the coding system used to decode
the process output.

This is useful when the coding system specified for the process buffer
leaves either the character code conversion or the end-of-line conversion
unspecified, or if the coding system used to decode the process output
is more appropriate for saving the process buffer.

Binding the variable `inherit-process-coding-system' to non-nil before
starting the process is an alternative way of setting the inherit flag
for the process which will run.  */)
     (process, flag)
     register Lisp_Object process, flag;
{
  CHECK_PROCESS (process);
  XPROCESS (process)->inherit_coding_system_flag = flag;
  return flag;
}

DEFUN ("process-inherit-coding-system-flag",
       Fprocess_inherit_coding_system_flag, Sprocess_inherit_coding_system_flag,
       1, 1, 0,
       doc: /* Return the value of inherit-coding-system flag for PROCESS.
If this flag is t, `buffer-file-coding-system' of the buffer
associated with PROCESS will inherit the coding system used to decode
the process output.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->inherit_coding_system_flag;
}

DEFUN ("set-process-query-on-exit-flag",
       Fset_process_query_on_exit_flag, Sset_process_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for PROCESS when Emacs is exited.
If the second argument FLAG is non-nil, emacs will query the user before
exiting if PROCESS is running.  */)
     (process, flag)
     register Lisp_Object process, flag;
{
  CHECK_PROCESS (process);
  XPROCESS (process)->kill_without_query = Fnull (flag);
  return flag;
}

DEFUN ("process-query-on-exit-flag",
       Fprocess_query_on_exit_flag, Sprocess_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of query on exit flag for PROCESS.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return Fnull (XPROCESS (process)->kill_without_query);
}

#ifdef DATAGRAM_SOCKETS
Lisp_Object Fprocess_datagram_address ();
#endif

DEFUN ("process-contact", Fprocess_contact, Sprocess_contact,
       1, 2, 0,
       doc: /* Return the contact info of PROCESS; t for a real child.
For a net connection, the value depends on the optional KEY arg.
If KEY is nil, value is a cons cell of the form (HOST SERVICE),
if KEY is t, the complete contact information for the connection is
returned, else the specific value for the keyword KEY is returned.
See `make-network-process' for a list of keywords.  */)
     (process, key)
     register Lisp_Object process, key;
{
  Lisp_Object contact;

  CHECK_PROCESS (process);
  contact = XPROCESS (process)->childp;

#ifdef DATAGRAM_SOCKETS
  if (DATAGRAM_CONN_P (process)
      && (EQ (key, Qt) || EQ (key, QCremote)))
    contact = Fplist_put (contact, QCremote, 
			  Fprocess_datagram_address (process));
#endif

  if (!NETCONN_P (process) || EQ (key, Qt))
    return contact;
  if (NILP (key))
    return Fcons (Fplist_get (contact, QChost),
		  Fcons (Fplist_get (contact, QCservice), Qnil));
  return Fplist_get (contact, key);
}

#if 0 /* Turned off because we don't currently record this info
	 in the process.  Perhaps add it.  */
DEFUN ("process-connection", Fprocess_connection, Sprocess_connection, 1, 1, 0,
       doc: /* Return the connection type of PROCESS.
The value is nil for a pipe, t or `pty' for a pty, or `stream' for
a socket connection.  */)
     (process)
     Lisp_Object process;
{
  return XPROCESS (process)->type;
}
#endif

Lisp_Object
list_processes_1 (query_only)
     Lisp_Object query_only;
{
  register Lisp_Object tail, tem;
  Lisp_Object proc, minspace, tem1;
  register struct Lisp_Process *p;
  char tembuf[300];
  int w_proc, w_buffer, w_tty;
  Lisp_Object i_status, i_buffer, i_tty, i_command;

  w_proc = 4;    /* Proc   */
  w_buffer = 6;  /* Buffer */
  w_tty = 0;     /* Omit if no ttys */

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      int i;

      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);
      if (NILP (p->childp))
	continue;
      if (!NILP (query_only) && !NILP (p->kill_without_query))
	continue;
      if (STRINGP (p->name)
	  && ( i = XSTRING (p->name)->size, (i > w_proc)))
	w_proc = i;
      if (!NILP (p->buffer))
	{
	  if (NILP (XBUFFER (p->buffer)->name) && w_buffer < 8)
	    w_buffer = 8;  /* (Killed) */
	  else if ((i = XSTRING (XBUFFER (p->buffer)->name)->size, (i > w_buffer)))
	    w_buffer = i;
	}
      if (STRINGP (p->tty_name)
	  && (i = XSTRING (p->tty_name)->size, (i > w_tty)))
	w_tty = i;
    }

  XSETFASTINT (i_status, w_proc + 1);
  XSETFASTINT (i_buffer, XFASTINT (i_status) + 9);
  if (w_tty)
    {
      XSETFASTINT (i_tty, XFASTINT (i_buffer) + w_buffer + 1);
      XSETFASTINT (i_command, XFASTINT (i_buffer) + w_tty + 1);
    } else {
      i_tty = Qnil;
      XSETFASTINT (i_command, XFASTINT (i_buffer) + w_buffer + 1);
    }

  XSETFASTINT (minspace, 1);

  set_buffer_internal (XBUFFER (Vstandard_output));
  Fbuffer_disable_undo (Vstandard_output);

  current_buffer->truncate_lines = Qt;

  write_string ("Proc", -1);
  Findent_to (i_status, minspace); write_string ("Status", -1);
  Findent_to (i_buffer, minspace); write_string ("Buffer", -1);
  if (!NILP (i_tty))
    {
      Findent_to (i_tty, minspace); write_string ("Tty", -1);
    }
  Findent_to (i_command, minspace); write_string ("Command", -1);
  write_string ("\n", -1);

  write_string ("----", -1);
  Findent_to (i_status, minspace); write_string ("------", -1);
  Findent_to (i_buffer, minspace); write_string ("------", -1);
  if (!NILP (i_tty))
    {
      Findent_to (i_tty, minspace); write_string ("---", -1);
    }
  Findent_to (i_command, minspace); write_string ("-------", -1);
  write_string ("\n", -1);

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object symbol;

      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);
      if (NILP (p->childp))
	continue;
      if (!NILP (query_only) && !NILP (p->kill_without_query))
	continue;

      Finsert (1, &p->name);
      Findent_to (i_status, minspace);

      if (!NILP (p->raw_status_low))
	update_status (p);
      symbol = p->status;
      if (CONSP (p->status))
	symbol = XCAR (p->status);

      
      if (EQ (symbol, Qsignal))
	{
	  Lisp_Object tem;
	  tem = Fcar (Fcdr (p->status));
#ifdef VMS
	  if (XINT (tem) < NSIG)
	    write_string (sys_errlist [XINT (tem)], -1);
	  else
#endif
	    Fprinc (symbol, Qnil);
	}
      else if (NETCONN1_P (p))
	{
	  if (EQ (symbol, Qexit))
	    write_string ("closed", -1);
	  else if (EQ (p->command, Qt))
	    write_string ("stopped", -1);
	  else if (EQ (symbol, Qrun))
	    write_string ("open", -1);
	  else
	    Fprinc (symbol, Qnil);
	}
      else
	Fprinc (symbol, Qnil);

      if (EQ (symbol, Qexit))
	{
	  Lisp_Object tem;
	  tem = Fcar (Fcdr (p->status));
	  if (XFASTINT (tem))
	    {
	      sprintf (tembuf, " %d", (int) XFASTINT (tem));
	      write_string (tembuf, -1);
	    }
	}

      if (EQ (symbol, Qsignal) || EQ (symbol, Qexit))
	remove_process (proc);

      Findent_to (i_buffer, minspace);
      if (NILP (p->buffer))
	insert_string ("(none)");
      else if (NILP (XBUFFER (p->buffer)->name))
	insert_string ("(Killed)");
      else
	Finsert (1, &XBUFFER (p->buffer)->name);

      if (!NILP (i_tty))
	{
	  Findent_to (i_tty, minspace);
	  if (STRINGP (p->tty_name))
	    Finsert (1, &p->tty_name);
	}

      Findent_to (i_command, minspace);

      if (EQ (p->status, Qlisten))
	{
	  Lisp_Object port = Fplist_get (p->childp, QCservice);
	  if (INTEGERP (port))
	    port = Fnumber_to_string (port);
	  sprintf (tembuf, "(network %s server on %s)\n",
		   (DATAGRAM_CHAN_P (XINT (p->infd)) ? "datagram" : "stream"),
		   XSTRING (port)->data);
	  insert_string (tembuf);
	}
      else if (NETCONN1_P (p))
        {
	  /* For a local socket, there is no host name,
	     so display service instead.  */
	  Lisp_Object host = Fplist_get (p->childp, QChost);
	  if (!STRINGP (host))
	    {
	      host = Fplist_get (p->childp, QCservice);
	      if (INTEGERP (host))
		host = Fnumber_to_string (host);
	    }
	  sprintf (tembuf, "(network %s connection to %s)\n",
		   (DATAGRAM_CHAN_P (XINT (p->infd)) ? "datagram" : "stream"),
		   XSTRING (host)->data);
	  insert_string (tembuf);
        }
      else 
	{
	  tem = p->command;
	  while (1)
	    {
	      tem1 = Fcar (tem);
	      Finsert (1, &tem1);
	      tem = Fcdr (tem);
	      if (NILP (tem))
		break;
	      insert_string (" ");
	    }
	  insert_string ("\n");
       }
    }
  return Qnil;
}

DEFUN ("list-processes", Flist_processes, Slist_processes, 0, 1, "P",
       doc: /* Display a list of all processes.
If optional argument QUERY-ONLY is non-nil, only processes with
the query-on-exit flag set will be listed.
Any process listed as exited or signaled is actually eliminated
after the listing is made.  */)
     (query_only)
     Lisp_Object query_only;
{
  internal_with_output_to_temp_buffer ("*Process List*",
				       list_processes_1, query_only);
  return Qnil;
}

DEFUN ("process-list", Fprocess_list, Sprocess_list, 0, 0, 0,
       doc: /* Return a list of all processes.  */)
     ()
{
  return Fmapcar (Qcdr, Vprocess_alist);
}

/* Starting asynchronous inferior processes.  */

static Lisp_Object start_process_unwind ();

DEFUN ("start-process", Fstart_process, Sstart_process, 3, MANY, 0,
       doc: /* Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
Third arg is program file name.  It is searched for in PATH.
Remaining arguments are strings to give program as arguments.

usage: (start-process NAME BUFFER PROGRAM &rest PROGRAM-ARGS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  Lisp_Object buffer, name, program, proc, current_dir, tem;
#ifdef VMS
  register unsigned char *new_argv;
  int len;
#else
  register unsigned char **new_argv;
#endif
  register int i;
  int count = specpdl_ptr - specpdl;

  buffer = args[1];
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.

     We have to GCPRO around this because Fexpand_file_name and
     Funhandled_file_name_directory might call a file name handling
     function.  The argument list is protected by the caller, so all
     we really have to worry about is buffer.  */
  {
    struct gcpro gcpro1, gcpro2;

    current_dir = current_buffer->directory;

    GCPRO2 (buffer, current_dir);

    current_dir 
      = expand_and_dir_to_file (Funhandled_file_name_directory (current_dir),
				Qnil);
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error ("Setting current directory",
			 Fcons (current_buffer->directory, Qnil));

    UNGCPRO;
  }

  name = args[0];
  CHECK_STRING (name);

  program = args[2];

  CHECK_STRING (program);

  proc = make_process (name);
  /* If an error occurs and we can't start the process, we want to
     remove it from the process list.  This means that each error
     check in create_process doesn't need to call remove_process
     itself; it's all taken care of here.  */
  record_unwind_protect (start_process_unwind, proc);

  XPROCESS (proc)->childp = Qt;
  XPROCESS (proc)->command_channel_p = Qnil;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

  /* Make the process marker point into the process buffer (if any).  */
  if (!NILP (buffer))
    set_marker_both (XPROCESS (proc)->mark, buffer,
		     BUF_ZV (XBUFFER (buffer)),
		     BUF_ZV_BYTE (XBUFFER (buffer)));

  {
    /* Decide coding systems for communicating with the process.  Here
       we don't setup the structure coding_system nor pay attention to
       unibyte mode.  They are done in create_process.  */

    /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
    Lisp_Object coding_systems = Qt;
    Lisp_Object val, *args2;
    struct gcpro gcpro1, gcpro2;

    val = Vcoding_system_for_read;
    if (NILP (val))
      {
	args2 = (Lisp_Object *) alloca ((nargs + 1) * sizeof *args2);
	args2[0] = Qstart_process;
	for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	GCPRO2 (proc, current_dir);
	coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
	UNGCPRO;
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
      }
    XPROCESS (proc)->decode_coding_system = val;

    val = Vcoding_system_for_write;
    if (NILP (val))
      {
	if (EQ (coding_systems, Qt))
	  {
	    args2 = (Lisp_Object *) alloca ((nargs + 1) * sizeof args2);
	    args2[0] = Qstart_process;
	    for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	    GCPRO2 (proc, current_dir);
	    coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
	    UNGCPRO;
	  }
	if (CONSP (coding_systems))
	  val = XCDR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCDR (Vdefault_process_coding_system);
      }
    XPROCESS (proc)->encode_coding_system = val;
  }

#ifdef VMS
  /* Make a one member argv with all args concatenated
     together separated by a blank.  */
  len = STRING_BYTES (XSTRING (program)) + 2;
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      len += STRING_BYTES (XSTRING (tem)) + 1;	/* count the blank */
    }
  new_argv = (unsigned char *) alloca (len);
  strcpy (new_argv, XSTRING (program)->data);
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      strcat (new_argv, " ");
      strcat (new_argv, XSTRING (tem)->data);
    }
  /* Need to add code here to check for program existence on VMS */
  
#else /* not VMS */
  new_argv = (unsigned char **) alloca ((nargs - 1) * sizeof (char *));

  /* If program file name is not absolute, search our path for it */
  if (!IS_DIRECTORY_SEP (XSTRING (program)->data[0])
      && !(XSTRING (program)->size > 1
	   && IS_DEVICE_SEP (XSTRING (program)->data[1])))
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

      tem = Qnil;
      GCPRO4 (name, program, buffer, current_dir);
      openp (Vexec_path, program, Vexec_suffixes, &tem, make_number (X_OK));
      UNGCPRO;
      if (NILP (tem))
	report_file_error ("Searching for program", Fcons (program, Qnil));
      tem = Fexpand_file_name (tem, Qnil);
      tem = ENCODE_FILE (tem);
      new_argv[0] = XSTRING (tem)->data;
    }
  else
    {
      if (!NILP (Ffile_directory_p (program)))
	error ("Specified program for new process is a directory");

      tem = ENCODE_FILE (program);
      new_argv[0] = XSTRING (tem)->data;
    }

  /* Here we encode arguments by the coding system used for sending
     data to the process.  We don't support using different coding
     systems for encoding arguments and for encoding data sent to the
     process.  */

  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      if (STRING_MULTIBYTE (tem))
	tem = (code_convert_string_norecord
	       (tem, XPROCESS (proc)->encode_coding_system, 1));
      new_argv[i - 2] = XSTRING (tem)->data;
    }
  new_argv[i - 2] = 0;
#endif /* not VMS */

  XPROCESS (proc)->decoding_buf = make_uninit_string (0);
  XPROCESS (proc)->decoding_carryover = make_number (0);
  XPROCESS (proc)->encoding_buf = make_uninit_string (0);
  XPROCESS (proc)->encoding_carryover = make_number (0);

  XPROCESS (proc)->inherit_coding_system_flag
    = (NILP (buffer) || !inherit_process_coding_system
       ? Qnil : Qt);

  create_process (proc, (char **) new_argv, current_dir);

  return unbind_to (count, proc);
}

/* This function is the unwind_protect form for Fstart_process.  If
   PROC doesn't have its pid set, then we know someone has signaled
   an error and the process wasn't started successfully, so we should
   remove it from the process list.  */
static Lisp_Object
start_process_unwind (proc)
     Lisp_Object proc;
{
  if (!PROCESSP (proc))
    abort ();

  /* Was PROC started successfully?  */
  if (XINT (XPROCESS (proc)->pid) <= 0)
    remove_process (proc);

  return Qnil;
}

void
create_process_1 (timer)
     struct atimer *timer;
{
  /* Nothing to do.  */
}


#if 0  /* This doesn't work; see the note before sigchld_handler.  */
#ifdef USG
#ifdef SIGCHLD
/* Mimic blocking of signals on system V, which doesn't really have it.  */

/* Nonzero means we got a SIGCHLD when it was supposed to be blocked.  */
int sigchld_deferred;

SIGTYPE
create_process_sigchld ()
{
  signal (SIGCHLD, create_process_sigchld);

  sigchld_deferred = 1;
}
#endif
#endif
#endif

#ifndef VMS /* VMS version of this function is in vmsproc.c.  */
void
create_process (process, new_argv, current_dir)
     Lisp_Object process;
     char **new_argv;
     Lisp_Object current_dir;
{
  int pid, inchannel, outchannel;
  int sv[2];
#ifdef POSIX_SIGNALS
  sigset_t procmask;
  sigset_t blocked;
  struct sigaction sigint_action;
  struct sigaction sigquit_action;
#ifdef AIX
  struct sigaction sighup_action;
#endif
#else /* !POSIX_SIGNALS */
#if 0
#ifdef SIGCHLD
  SIGTYPE (*sigchld)();
#endif
#endif /* 0 */
#endif /* !POSIX_SIGNALS */
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  volatile int forkin, forkout;
  volatile int pty_flag = 0;
#ifndef USE_CRT_DLL
  extern char **environ;
#endif

  inchannel = outchannel = -1;

#ifdef HAVE_PTYS
  if (!NILP (Vprocess_connection_type))
    outchannel = inchannel = allocate_pty ();

  if (inchannel >= 0)
    {
#ifndef USG 
      /* On USG systems it does not work to open the pty's tty here
	       and then close and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      forkout = forkin = emacs_open (pty_name, O_RDWR, 0);
#endif
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
#else
      forkin = forkout = -1;
#endif /* not USG */
      pty_flag = 1;
    }
  else
#endif /* HAVE_PTYS */
#ifdef SKTPAIR
    {
      if (socketpair (AF_UNIX, SOCK_STREAM, 0, sv) < 0)
	report_file_error ("Opening socketpair", Qnil);
      outchannel = inchannel = sv[0];
      forkout = forkin = sv[1];
    }
#else /* not SKTPAIR */
    {
      int tem;
      tem = pipe (sv);
      if (tem < 0)
	report_file_error ("Creating pipe", Qnil);
      inchannel = sv[0];
      forkout = sv[1];
      tem = pipe (sv);
      if (tem < 0)
	{
	  emacs_close (inchannel);
	  emacs_close (forkout);
	  report_file_error ("Creating pipe", Qnil);
	}
      outchannel = sv[1];
      forkin = sv[0];
    }
#endif /* not SKTPAIR */

#if 0
  /* Replaced by close_process_descs */
  set_exclusive_use (inchannel);
  set_exclusive_use (outchannel);
#endif

/* Stride people say it's a mystery why this is needed
   as well as the O_NDELAY, but that it fails without this.  */
#if defined (STRIDE) || (defined (pfa) && defined (HAVE_PTYS))
  {
    int one = 1;
    ioctl (inchannel, FIONBIO, &one);
  }
#endif

#ifdef O_NONBLOCK
  fcntl (inchannel, F_SETFL, O_NONBLOCK);
  fcntl (outchannel, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inchannel, F_SETFL, O_NDELAY);
  fcntl (outchannel, F_SETFL, O_NDELAY);
#endif
#endif

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  chan_process[inchannel] = process;
  XSETINT (XPROCESS (process)->infd, inchannel);
  XSETINT (XPROCESS (process)->outfd, outchannel);
  /* Record the tty descriptor used in the subprocess.  */
  if (forkin < 0)
    XPROCESS (process)->subtty = Qnil;
  else
    XSETFASTINT (XPROCESS (process)->subtty, forkin);
  XPROCESS (process)->pty_flag = (pty_flag ? Qt : Qnil);
  XPROCESS (process)->status = Qrun;
  if (!proc_decode_coding_system[inchannel])
    proc_decode_coding_system[inchannel]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (XPROCESS (process)->decode_coding_system,
		       proc_decode_coding_system[inchannel]);
  if (!proc_encode_coding_system[outchannel])
    proc_encode_coding_system[outchannel]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (XPROCESS (process)->encode_coding_system,
		       proc_encode_coding_system[outchannel]);

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
#ifdef POSIX_SIGNALS
  sigemptyset (&blocked);
#ifdef SIGCHLD
  sigaddset (&blocked, SIGCHLD);
#endif
#ifdef HAVE_WORKING_VFORK
  /* On many hosts (e.g. Solaris 2.4), if a vforked child calls `signal',
     this sets the parent's signal handlers as well as the child's.
     So delay all interrupts whose handlers the child might munge,
     and record the current handlers so they can be restored later.  */
  sigaddset (&blocked, SIGINT );  sigaction (SIGINT , 0, &sigint_action );
  sigaddset (&blocked, SIGQUIT);  sigaction (SIGQUIT, 0, &sigquit_action);
#ifdef AIX
  sigaddset (&blocked, SIGHUP );  sigaction (SIGHUP , 0, &sighup_action );
#endif
#endif /* HAVE_WORKING_VFORK */
  sigprocmask (SIG_BLOCK, &blocked, &procmask);
#else /* !POSIX_SIGNALS */
#ifdef SIGCHLD
#ifdef BSD4_1
  sighold (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD_SYSTEM) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (sigmask (SIGCHLD));
#else /* ordinary USG */
#if 0
  sigchld_deferred = 0;
  sigchld = signal (SIGCHLD, create_process_sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

  FD_SET (inchannel, &input_wait_mask);
  FD_SET (inchannel, &non_keyboard_wait_mask);
  if (inchannel > max_process_desc)
    max_process_desc = inchannel;

  /* Until we store the proper pid, enable sigchld_handler
     to recognize an unknown pid as standing for this process.
     It is very important not to let this `marker' value stay
     in the table after this function has returned; if it does
     it might cause call-process to hang and subsequent asynchronous
     processes to get their return values scrambled.  */
  XSETINT (XPROCESS (process)->pid, -1);

  BLOCK_INPUT;
  
  {
    /* child_setup must clobber environ on systems with true vfork.
       Protect it from permanent change.  */
    char **save_environ = environ;

    current_dir = ENCODE_FILE (current_dir);

#ifndef WINDOWSNT
    pid = vfork ();
    if (pid == 0)
#endif /* not WINDOWSNT */
      {
	int xforkin = forkin;
	int xforkout = forkout;

#if 0 /* This was probably a mistake--it duplicates code later on,
	 but fails to handle all the cases.  */
	/* Make sure SIGCHLD is not blocked in the child.  */
	sigsetmask (SIGEMPTYMASK);
#endif

	/* Make the pty be the controlling terminal of the process.  */
#ifdef HAVE_PTYS
	/* First, disconnect its current controlling terminal.  */
#ifdef HAVE_SETSID
	/* We tried doing setsid only if pty_flag, but it caused
	   process_set_signal to fail on SGI when using a pipe.  */
	setsid ();
	/* Make the pty's terminal the controlling terminal.  */
	if (pty_flag)
	  {
#ifdef TIOCSCTTY
	    /* We ignore the return value
	       because faith@cs.unc.edu says that is necessary on Linux.  */
	    ioctl (xforkin, TIOCSCTTY, 0);
#endif
	  }
#else /* not HAVE_SETSID */
#ifdef USG
	/* It's very important to call setpgrp here and no time
	   afterwards.  Otherwise, we lose our controlling tty which
	   is set when we open the pty. */
	setpgrp ();
#endif /* USG */
#endif /* not HAVE_SETSID */
#if defined (HAVE_TERMIOS) && defined (LDISC1)
	if (pty_flag && xforkin >= 0)
	  {
	    struct termios t;
	    tcgetattr (xforkin, &t);
	    t.c_lflag = LDISC1;
	    if (tcsetattr (xforkin, TCSANOW, &t) < 0)
	      emacs_write (1, "create_process/tcsetattr LDISC1 failed\n", 39);
	  }
#else
#if defined (NTTYDISC) && defined (TIOCSETD)
	if (pty_flag && xforkin >= 0)
	  {
	    /* Use new line discipline.  */
	    int ldisc = NTTYDISC;
	    ioctl (xforkin, TIOCSETD, &ldisc);
	  }
#endif
#endif
#ifdef TIOCNOTTY 
	/* In 4.3BSD, the TIOCSPGRP bug has been fixed, and now you
	   can do TIOCSPGRP only to the process's controlling tty.  */
	if (pty_flag)
	  {
	    /* I wonder: would just ioctl (0, TIOCNOTTY, 0) work here? 
	       I can't test it since I don't have 4.3.  */
	    int j = emacs_open ("/dev/tty", O_RDWR, 0);
	    ioctl (j, TIOCNOTTY, 0);
	    emacs_close (j);
#ifndef USG
	    /* In order to get a controlling terminal on some versions
	       of BSD, it is necessary to put the process in pgrp 0
	       before it opens the terminal.  */
#ifdef HAVE_SETPGID
	    setpgid (0, 0);
#else
	    setpgrp (0, 0);
#endif
#endif
	  }
#endif /* TIOCNOTTY */

#if !defined (RTU) && !defined (UNIPLUS) && !defined (DONT_REOPEN_PTY)
/*** There is a suggestion that this ought to be a
     conditional on TIOCSPGRP,
     or !(defined (HAVE_SETSID) && defined (TIOCSCTTY)).
     Trying the latter gave the wrong results on Debian GNU/Linux 1.1;
     that system does seem to need this code, even though
     both HAVE_SETSID and TIOCSCTTY are defined.  */
	/* Now close the pty (if we had it open) and reopen it.
	   This makes the pty the controlling terminal of the subprocess.  */
	if (pty_flag)
	  {
#ifdef SET_CHILD_PTY_PGRP
	    int pgrp = getpid ();
#endif

	    /* I wonder if emacs_close (emacs_open (pty_name, ...))
	       would work?  */
	    if (xforkin >= 0)
	      emacs_close (xforkin);
	    xforkout = xforkin = emacs_open (pty_name, O_RDWR, 0);

	    if (xforkin < 0)
	      {
		emacs_write (1, "Couldn't open the pty terminal ", 31);
		emacs_write (1, pty_name, strlen (pty_name));
		emacs_write (1, "\n", 1);
		_exit (1);
	      }

#ifdef SET_CHILD_PTY_PGRP
	    ioctl (xforkin, TIOCSPGRP, &pgrp);
	    ioctl (xforkout, TIOCSPGRP, &pgrp);
#endif
	  }
#endif /* not UNIPLUS and not RTU and not DONT_REOPEN_PTY */

#ifdef SETUP_SLAVE_PTY
	if (pty_flag)
	  {
	    SETUP_SLAVE_PTY;
	  }
#endif /* SETUP_SLAVE_PTY */
#ifdef AIX
	/* On AIX, we've disabled SIGHUP above once we start a child on a pty.
	   Now reenable it in the child, so it will die when we want it to.  */
	if (pty_flag)
	  signal (SIGHUP, SIG_DFL);
#endif
#endif /* HAVE_PTYS */

	signal (SIGINT, SIG_DFL);
	signal (SIGQUIT, SIG_DFL);

	/* Stop blocking signals in the child.  */
#ifdef POSIX_SIGNALS
	sigprocmask (SIG_SETMASK, &procmask, 0);
#else /* !POSIX_SIGNALS */
#ifdef SIGCHLD
#ifdef BSD4_1
	sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD_SYSTEM) || defined (UNIPLUS) || defined (HPUX)
	sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
	signal (SIGCHLD, sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

	if (pty_flag)
	  child_setup_tty (xforkout);
#ifdef WINDOWSNT
	pid = child_setup (xforkin, xforkout, xforkout,
			   new_argv, 1, current_dir);
#else  /* not WINDOWSNT */	
	child_setup (xforkin, xforkout, xforkout,
		     new_argv, 1, current_dir);
#endif /* not WINDOWSNT */
      }
    environ = save_environ;
  }

  UNBLOCK_INPUT;

  /* This runs in the Emacs process.  */
  if (pid < 0)
    {
      if (forkin >= 0)
	emacs_close (forkin);
      if (forkin != forkout && forkout >= 0)
	emacs_close (forkout);
    }
  else
    {
      /* vfork succeeded.  */
      XSETFASTINT (XPROCESS (process)->pid, pid);

#ifdef WINDOWSNT
      register_child (pid, inchannel);
#endif /* WINDOWSNT */

      /* If the subfork execv fails, and it exits,
	 this close hangs.  I don't know why.
	 So have an interrupt jar it loose.  */
      {
	struct atimer *timer;
	EMACS_TIME offset;
	
	stop_polling ();
	EMACS_SET_SECS_USECS (offset, 1, 0);
	timer = start_atimer (ATIMER_RELATIVE, offset, create_process_1, 0);
	
	XPROCESS (process)->subtty = Qnil;
	if (forkin >= 0)
	  emacs_close (forkin);

	cancel_atimer (timer);
	start_polling ();
      }
      
      if (forkin != forkout && forkout >= 0)
	emacs_close (forkout);

#ifdef HAVE_PTYS
      if (pty_flag)
	XPROCESS (process)->tty_name = build_string (pty_name);
      else
#endif
	XPROCESS (process)->tty_name = Qnil;
    }

  /* Restore the signal state whether vfork succeeded or not.
     (We will signal an error, below, if it failed.)  */
#ifdef POSIX_SIGNALS
#ifdef HAVE_WORKING_VFORK
  /* Restore the parent's signal handlers.  */
  sigaction (SIGINT, &sigint_action, 0);
  sigaction (SIGQUIT, &sigquit_action, 0);
#ifdef AIX
  sigaction (SIGHUP, &sighup_action, 0);
#endif
#endif /* HAVE_WORKING_VFORK */
  /* Stop blocking signals in the parent.  */
  sigprocmask (SIG_SETMASK, &procmask, 0);
#else /* !POSIX_SIGNALS */
#ifdef SIGCHLD
#ifdef BSD4_1
  sigrelse (SIGCHLD);
#else /* not BSD4_1 */
#if defined (BSD_SYSTEM) || defined (UNIPLUS) || defined (HPUX)
  sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
  signal (SIGCHLD, sigchld);
  /* Now really handle any of these signals
     that came in during this function.  */
  if (sigchld_deferred)
    kill (getpid (), SIGCHLD);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

  /* Now generate the error if vfork failed.  */
  if (pid < 0)
    report_file_error ("Doing vfork", Qnil);
}
#endif /* not VMS */


#ifdef HAVE_SOCKETS

/* Convert an internal struct sockaddr to a lisp object (vector or string).
   The address family of sa is not included in the result.  */

static Lisp_Object
conv_sockaddr_to_lisp (sa, len)
     struct sockaddr *sa;
     int len;
{
  Lisp_Object address;
  int i;
  unsigned char *cp;
  register struct Lisp_Vector *p;

  switch (sa->sa_family)
    {
    case AF_INET:
      {
	struct sockaddr_in *sin = (struct sockaddr_in *) sa;
	len = sizeof (sin->sin_addr) + 1;
	address = Fmake_vector (make_number (len), Qnil);
	p = XVECTOR (address);
	p->contents[--len] = make_number (ntohs (sin->sin_port));
	cp = (unsigned char *)&sin->sin_addr;
	break;
      }
#ifdef HAVE_LOCAL_SOCKETS
    case AF_LOCAL:
      {
	struct sockaddr_un *sockun = (struct sockaddr_un *) sa;
	for (i = 0; i < sizeof (sockun->sun_path); i++)
	  if (sockun->sun_path[i] == 0)
	    break;
	return make_unibyte_string (sockun->sun_path, i);
      }
#endif
    default:
      len -= sizeof (sa->sa_family);
      address = Fcons (make_number (sa->sa_family),
		       Fmake_vector (make_number (len), Qnil));
      p = XVECTOR (XCDR (address));
      cp = (unsigned char *) sa + sizeof (sa->sa_family);
      break;
    }

  i = 0;
  while (i < len)
    p->contents[i++] = make_number (*cp++);

  return address;
}


/* Get family and required size for sockaddr structure to hold ADDRESS.  */

static int
get_lisp_to_sockaddr_size (address, familyp)
     Lisp_Object address;
     int *familyp;
{
  register struct Lisp_Vector *p;

  if (VECTORP (address))
    {
      p = XVECTOR (address);
      if (p->size == 5)
	{
	  *familyp = AF_INET;
	  return sizeof (struct sockaddr_in);
	}
    }
#ifdef HAVE_LOCAL_SOCKETS
  else if (STRINGP (address))
    {
      *familyp = AF_LOCAL;
      return sizeof (struct sockaddr_un);
    }
#endif
  else if (CONSP (address) && INTEGERP (XCAR (address)) && VECTORP (XCDR (address)))
    {
      struct sockaddr *sa;
      *familyp = XINT (XCAR (address));
      p = XVECTOR (XCDR (address));
      return p->size + sizeof (sa->sa_family);
    }
  return 0;
}

/* Convert an address object (vector or string) to an internal sockaddr.
   Format of address has already been validated by size_lisp_to_sockaddr.  */

static void
conv_lisp_to_sockaddr (family, address, sa, len)
     int family;
     Lisp_Object address;
     struct sockaddr *sa;
     int len;
{
  register struct Lisp_Vector *p;
  register unsigned char *cp;
  register int i;

  bzero (sa, len);
  sa->sa_family = family;

  if (VECTORP (address))
    {
      p = XVECTOR (address);
      if (family == AF_INET)
	{
	  struct sockaddr_in *sin = (struct sockaddr_in *) sa;
	  len = sizeof (sin->sin_addr) + 1;
	  i = XINT (p->contents[--len]);
	  sin->sin_port = htons (i);
	  cp = (unsigned char *)&sin->sin_addr;
	}
    }
  else if (STRINGP (address))
    {
#ifdef HAVE_LOCAL_SOCKETS
      if (family == AF_LOCAL)
	{
	  struct sockaddr_un *sockun = (struct sockaddr_un *) sa;
	  cp = XSTRING (address)->data;
	  for (i = 0; i < sizeof (sockun->sun_path) && *cp; i++)
	    sockun->sun_path[i] = *cp++;
	}
#endif
      return;
    }
  else
    {
      p = XVECTOR (XCDR (address));
      cp = (unsigned char *)sa + sizeof (sa->sa_family);
    }

  for (i = 0; i < len; i++)
    if (INTEGERP (p->contents[i]))
      *cp++ = XFASTINT (p->contents[i]) & 0xff;
}

#ifdef DATAGRAM_SOCKETS
DEFUN ("process-datagram-address", Fprocess_datagram_address, Sprocess_datagram_address,
       1, 1, 0,
       doc: /* Get the current datagram address associated with PROCESS.  */)
       (process)
       Lisp_Object process;
{
  int channel;

  CHECK_PROCESS (process);

  if (!DATAGRAM_CONN_P (process))
    return Qnil;

  channel = XINT (XPROCESS (process)->infd);
  return conv_sockaddr_to_lisp (datagram_address[channel].sa,
				datagram_address[channel].len);
}

DEFUN ("set-process-datagram-address", Fset_process_datagram_address, Sset_process_datagram_address,
       2, 2, 0,
       doc: /* Set the datagram address for PROCESS to ADDRESS.
Returns nil upon error setting address, ADDRESS otherwise.  */)
       (process, address)
       Lisp_Object process, address;
{
  int channel;
  int family, len;

  CHECK_PROCESS (process);

  if (!DATAGRAM_CONN_P (process))
    return Qnil;

  channel = XINT (XPROCESS (process)->infd);

  len = get_lisp_to_sockaddr_size (address, &family);
  if (datagram_address[channel].len != len)
    return Qnil;
  conv_lisp_to_sockaddr (family, address, datagram_address[channel].sa, len);
  return address;
}
#endif


static struct socket_options {
  /* The name of this option.  Should be lowercase version of option
     name without SO_ prefix. */ 
  char *name;
  /* Length of name.  */
  int nlen;
  /* Option level SOL_... */
  int optlevel;
  /* Option number SO_... */
  int optnum;
  enum { SOPT_UNKNOWN, SOPT_BOOL, SOPT_INT, SOPT_STR, SOPT_LINGER } opttype;
} socket_options[] =
  {
#ifdef SO_BINDTODEVICE
    { "bindtodevice", 12, SOL_SOCKET, SO_BINDTODEVICE, SOPT_STR },
#endif
#ifdef SO_BROADCAST
    { "broadcast", 9, SOL_SOCKET, SO_BROADCAST, SOPT_BOOL },
#endif
#ifdef SO_DONTROUTE
    { "dontroute", 9, SOL_SOCKET, SO_DONTROUTE, SOPT_BOOL },
#endif
#ifdef SO_KEEPALIVE
    { "keepalive", 9, SOL_SOCKET, SO_KEEPALIVE, SOPT_BOOL },
#endif
#ifdef SO_LINGER
    { "linger", 6, SOL_SOCKET, SO_LINGER, SOPT_LINGER },
#endif
#ifdef SO_OOBINLINE
    { "oobinline", 9, SOL_SOCKET, SO_OOBINLINE, SOPT_BOOL },
#endif
#ifdef SO_PRIORITY
    { "priority", 8, SOL_SOCKET, SO_PRIORITY, SOPT_INT },
#endif
#ifdef SO_REUSEADDR
    { "reuseaddr", 9, SOL_SOCKET, SO_REUSEADDR, SOPT_BOOL },
#endif
    { 0, 0, 0, 0, SOPT_UNKNOWN }
  };

/* Process list of socket options OPTS on socket S.
   Only check if options are supported is S < 0.
   If NO_ERROR is non-zero, continue silently if an option
   cannot be set.

   Each element specifies one option.  An element is either a string
   "OPTION=VALUE" or a cons (OPTION . VALUE) where OPTION is a string
   or a symbol.  */

static int
set_socket_options (s, opts, no_error)
     int s;
     Lisp_Object opts;
     int no_error;
{
  if (!CONSP (opts))
    opts = Fcons (opts, Qnil);

  while (CONSP (opts))
    {
      Lisp_Object opt;
      Lisp_Object val;
      char *name, *arg;
      struct socket_options *sopt;
      int ret = 0;

      opt = XCAR (opts);
      opts = XCDR (opts);

      name = 0;
      val = Qt;
      if (CONSP (opt))
	{
	  val = XCDR (opt);
	  opt = XCAR (opt);
	}
      if (STRINGP (opt))
	name = (char *) XSTRING (opt)->data;
      else if (SYMBOLP (opt))
	name = (char *) XSYMBOL (opt)->name->data;
      else {
	error ("Mal-formed option list");
	return 0;
      }

      if (strncmp (name, "no", 2) == 0)
	{
	  val = Qnil;
	  name += 2;
	}

      arg = 0;
      for (sopt = socket_options; sopt->name; sopt++)
	if (strncmp (name, sopt->name, sopt->nlen) == 0)
	  {
	    if (name[sopt->nlen] == 0)
	      break;
	    if (name[sopt->nlen] == '=')
	      {
		arg = name + sopt->nlen + 1;
		break;
	      }
	  }

      switch (sopt->opttype)
	{
	case SOPT_BOOL:
	  {
	    int optval;
	    if (s < 0)
	      return 1;
	    if (arg)
	      optval = (*arg == '0' || *arg == 'n') ? 0 : 1;
	    else if (INTEGERP (val))
	      optval = XINT (val) == 0 ? 0 : 1;
	    else
	      optval = NILP (val) ? 0 : 1;
	    ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			      &optval, sizeof (optval));
	    break;
	  }

	case SOPT_INT:
	  {
	    int optval;
	    if (arg)
	      optval = atoi(arg);
	    else if (INTEGERP (val))
	      optval = XINT (val);
	    else
	      error ("Bad option argument for %s", name);
	    if (s < 0)
	      return 1;
	    ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			      &optval, sizeof (optval));
	    break;
	  }

	case SOPT_STR:
	  {
	    if (!arg)
	      {
		if (NILP (val))
		  arg = "";
		else if (STRINGP (val))
		  arg = (char *) XSTRING (val)->data;
		else if (XSYMBOL (val))
		  arg = (char *) XSYMBOL (val)->name->data;
		else 
		  error ("Invalid argument to %s option", name);
	      }
	    ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			      arg, strlen (arg));
	  }

#ifdef SO_LINGER	    
	case SOPT_LINGER:
	  {
	    struct linger linger;

	    linger.l_onoff = 1;
	    linger.l_linger = 0;

	    if (s < 0)
	      return 1;

	    if (arg)
	      {
		if (*arg == 'n' || *arg == 't' || *arg == 'y')
		  linger.l_onoff = (*arg == 'n') ? 0 : 1;
		else
		  linger.l_linger = atoi(arg);
	      }
	    else if (INTEGERP (val))
	      linger.l_linger = XINT (val);
	    else
	      linger.l_onoff = NILP (val) ? 0 : 1;
	    ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			      &linger, sizeof (linger));
	    break;
	  }
#endif
	default:
	  if (s < 0)
	    return 0;
	  if (no_error)
	    continue;
	  error ("Unsupported option: %s", name);
	}
      if (ret < 0 && ! no_error)
	  report_file_error ("Cannot set network option: %s", opt);
    }
  return 1;
}

DEFUN ("set-network-process-options",
       Fset_network_process_options, Sset_network_process_options,
       1, MANY, 0, 
       doc: /* Set one or more options for network process PROCESS.
Each option is either a string "OPT=VALUE" or a cons (OPT . VALUE).
A boolean value is false if it either zero or nil, true otherwise.

The following options are known.  Consult the relevant system manual
pages for more information.

bindtodevice=NAME -- bind to interface NAME, or remove binding if nil.
broadcast=BOOL -- Allow send and receive of datagram broadcasts.
dontroute=BOOL -- Only send to directly connected hosts.
keepalive=BOOL -- Send keep-alive messages on network stream. 
linger=BOOL or TIMEOUT -- Send queued messages before closing.
oobinline=BOOL -- Place out-of-band data in receive data stream. 
priority=INT -- Set protocol defined priority for sent packets.
reuseaddr=BOOL -- Allow reusing a recently used address.

usage: (set-network-process-options PROCESS &rest OPTIONS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object process;
  Lisp_Object opts;

  process = args[0];
  CHECK_PROCESS (process);
  if (nargs > 1 && XINT (XPROCESS (process)->infd) >= 0)
    {
      opts = Flist (nargs, args);
      set_socket_options (XINT (XPROCESS (process)->infd), opts, 0);
    }
  return process;
}

/* A version of request_sigio suitable for a record_unwind_protect.  */

Lisp_Object
unwind_request_sigio (dummy)
     Lisp_Object dummy;
{
  if (interrupt_input)
    request_sigio ();
  return Qnil;
}

/* Create a network stream/datagram client/server process.  Treated
   exactly like a normal process when reading and writing.  Primary
   differences are in status display and process deletion.  A network
   connection has no PID; you cannot signal it.  All you can do is
   stop/continue it and deactivate/close it via delete-process */

DEFUN ("make-network-process", Fmake_network_process, Smake_network_process, 
       0, MANY, 0, 
       doc: /* Create and return a network server or client process.

In Emacs, network connections are represented by process objects, so
input and output work as for subprocesses and `delete-process' closes
a network connection.  However, a network process has no process id,
it cannot be signalled, and the status codes are different from normal
processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify an output stream or filter function to handle the output.
BUFFER may be also nil, meaning that this process is not associated
with any buffer.

:host HOST -- HOST is name of the host to connect to, or its IP
address.  The symbol `local' specifies the local host.  If specified
for a server process, it must be a valid name or address for the local
host, and only clients connecting to that address will be accepted.

:service SERVICE -- SERVICE is name of the service desired, or an
integer specifying a port number to connect to.  If SERVICE is t,
a random port number is selected for the server.

:type TYPE -- TYPE is the type of connection.  The default (nil) is a
stream type connection, `datagram' creates a datagram type connection.

:family FAMILY -- FAMILY is the address (and protocol) family for the
service specified by HOST and SERVICE.  The default address family is
Inet (or IPv4) for the host and port number specified by HOST and
SERVICE.  Other address families supported are:
  local -- for a local (i.e. UNIX) address specified by SERVICE.

:local ADDRESS -- ADDRESS is the local address used for the connection.
This parameter is ignored when opening a client process. When specified
for a server process, the FAMILY, HOST and SERVICE args are ignored.

:remote ADDRESS -- ADDRESS is the remote partner's address for the
connection.  This parameter is ignored when opening a stream server
process.  For a datagram server process, it specifies the initial
setting of the remote datagram address.  When specified for a client
process, the FAMILY, HOST, and SERVICE args are ignored.

The format of ADDRESS depends on the address family:
- An IPv4 address is represented as an vector of integers [A B C D P]
corresponding to numeric IP address A.B.C.D and port number P.
- A local address is represented as a string with the address in the
local address space.
- An "unsupported family" address is represented by a cons (F . AV)
where F is the family number and AV is a vector containing the socket
address data with one element per address data byte.  Do not rely on
this format in portable code, as it may depend on implementation
defined constants, data sizes, and data structure alignment.

:coding CODING -- CODING is coding system for this process.

:options OPTIONS -- Set the specified options for the network process.
See `set-network-process-options' for details.

:nowait BOOL -- If BOOL is non-nil for a stream type client process,
return without waiting for the connection to complete; instead, the
sentinel function will be called with second arg matching "open" (if
successful) or "failed" when the connect completes.  Default is to use
a blocking connect (i.e. wait) for stream type connections.

:noquery BOOL -- Query the user unless BOOL is non-nil, and process is
running when emacs is exited.

:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.
In the stopped state, a server process does not accept new
connections, and a client process does not handle incoming traffic.
The stopped state is cleared by `continue-process' and set by
`stop-process'.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:log LOG -- Install LOG as the server process log function.  This
function is called as when the server accepts a network connection from a
client.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER
is the server process, CLIENT is the new process for the connection,
and MESSAGE is a string.

:server BOOL -- if BOOL is non-nil, create a server process for the
specified FAMILY, SERVICE, and connection type (stream or datagram).
Default is a client process.

A server process will listen for and accept connections from
clients.  When a client connection is accepted, a new network process
is created for the connection with the following parameters: 
- The client's process name is constructed by concatenating the server
process' NAME and a client identification string.
- If the FILTER argument is non-nil, the client process will not get a
separate process buffer; otherwise, the client's process buffer is a newly
created buffer named after the server process' BUFFER name or process
NAME concatenated with the client identification string.  
- The connection type and the process filter and sentinel parameters are
inherited from the server process' TYPE, FILTER and SENTINEL.
- The client process' contact info is set according to the client's
addressing information (typically an IP address and a port number).

Notice that the FILTER and SENTINEL args are never used directly by
the server process.  Also, the BUFFER argument is not used directly by
the server process, but via `network-server-log-function' hook, a log
of the accepted (and failed) connections may be recorded in the server
process' buffer.

usage: (make-network-process &rest ARGS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object proc;
  Lisp_Object contact;
  struct Lisp_Process *p;
#ifdef HAVE_GETADDRINFO
  struct addrinfo ai, *res, *lres;
      struct addrinfo hints;
      char *portstring, portbuf[128];
#else /* HAVE_GETADDRINFO */
  struct _emacs_addrinfo
  {
    int ai_family;
    int ai_socktype;
    int ai_protocol;
    int ai_addrlen;
    struct sockaddr *ai_addr;
    struct _emacs_addrinfo *ai_next;
  } ai, *res, *lres;
#endif /* HAVE_GETADDRINFO */
  struct sockaddr_in address_in;
#ifdef HAVE_LOCAL_SOCKETS
  struct sockaddr_un address_un;
#endif
  int port;
  int ret = 0;
  int xerrno = 0;
  int s = -1, outch, inch;
  struct gcpro gcpro1;
  int retry = 0;
  int count = specpdl_ptr - specpdl;
  int count1;
  Lisp_Object QCaddress;  /* one of QClocal or QCremote */
  Lisp_Object tem;
  Lisp_Object name, buffer, host, service, address;
  Lisp_Object filter, sentinel;
  int is_non_blocking_client = 0;
  int is_server = 0;
  int socktype;
  int family = -1;

  if (nargs == 0)
    return Qnil;

  /* Save arguments for process-contact and clone-process.  */
  contact = Flist (nargs, args);
  GCPRO1 (contact);

#ifdef WINDOWSNT
  /* Ensure socket support is loaded if available. */
  init_winsock (TRUE);
#endif

  /* :type TYPE  (nil: stream, datagram */
  tem = Fplist_get (contact, QCtype);
  if (NILP (tem))
    socktype = SOCK_STREAM;
#ifdef DATAGRAM_SOCKETS
  else if (EQ (tem, Qdatagram))
    socktype = SOCK_DGRAM;
#endif
  else
    error ("Unsupported connection type");

  /* :server BOOL */
  tem = Fplist_get (contact, QCserver);
  if (!NILP (tem))
    {
#ifdef TERM
      error ("Network servers not supported");
#else
      is_server = 1;
#endif
    }

  /* Make QCaddress an alias for :local (server) or :remote (client).  */
  QCaddress = is_server ? QClocal : QCremote;

  /* :wait BOOL */
  if (!is_server && socktype == SOCK_STREAM
      && (tem = Fplist_get (contact, QCnowait), !NILP (tem)))
    {
#ifndef NON_BLOCKING_CONNECT
      error ("Non-blocking connect not supported");
#else
      is_non_blocking_client = 1;
#endif
    }

  name = Fplist_get (contact, QCname);
  buffer = Fplist_get (contact, QCbuffer);
  filter = Fplist_get (contact, QCfilter);
  sentinel = Fplist_get (contact, QCsentinel);

  CHECK_STRING (name);

#ifdef TERM
  /* Let's handle TERM before things get complicated ...   */
  host = Fplist_get (contact, QChost);
  CHECK_STRING (host);
  
  service = Fplist_get (contact, QCservice);
  if (INTEGERP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      struct servent *svc_info;
      CHECK_STRING (service);
      svc_info = getservbyname (XSTRING (service)->data, "tcp");
      if (svc_info == 0)
	error ("Unknown service: %s", XSTRING (service)->data);
      port = svc_info->s_port;
    }

  s = connect_server (0);
  if (s < 0)
    report_file_error ("error creating socket", Fcons (name, Qnil));
  send_command (s, C_PORT, 0, "%s:%d", XSTRING (host)->data, ntohs (port));
  send_command (s, C_DUMB, 1, 0);

#else  /* not TERM */

  /* Initialize addrinfo structure in case we don't use getaddrinfo.  */
  ai.ai_socktype = socktype;
  ai.ai_protocol = 0;
  ai.ai_next = NULL;
  res = &ai;

  /* :local ADDRESS or :remote ADDRESS */
  address = Fplist_get (contact, QCaddress);
  if (!NILP (address))
    {
      host = service = Qnil;

      if (!(ai.ai_addrlen = get_lisp_to_sockaddr_size (address, &family)))
	error ("Malformed :address");
      ai.ai_family = family;
      ai.ai_addr = alloca (ai.ai_addrlen);
      conv_lisp_to_sockaddr (family, address, ai.ai_addr, ai.ai_addrlen);
      goto open_socket;
    }

  /* :family FAMILY -- nil (for Inet), local, or integer.  */
  tem = Fplist_get (contact, QCfamily);
  if (INTEGERP (tem))
    family = XINT (tem);
  else
    {
      if (NILP (tem))
	family = AF_INET;
#ifdef HAVE_LOCAL_SOCKETS
      else if (EQ (tem, Qlocal))
	family = AF_LOCAL;
#endif
    }
  if (family < 0)
    error ("Unknown address family");
  ai.ai_family = family;

  /* :service SERVICE -- string, integer (port number), or t (random port).  */
  service = Fplist_get (contact, QCservice);

#ifdef HAVE_LOCAL_SOCKETS
  if (family == AF_LOCAL)
    {
      /* Host is not used.  */
      host = Qnil;
      CHECK_STRING (service);
      bzero (&address_un, sizeof address_un);
      address_un.sun_family = AF_LOCAL;
      strncpy (address_un.sun_path, XSTRING (service)->data, sizeof address_un.sun_path);
      ai.ai_addr = (struct sockaddr *) &address_un;
      ai.ai_addrlen = sizeof address_un;
      goto open_socket;
    }
#endif

  /* :host HOST -- hostname, ip address, or 'local for localhost.  */
  host = Fplist_get (contact, QChost);
  if (!NILP (host))
    {
      if (EQ (host, Qlocal))
	host = build_string ("localhost");
      CHECK_STRING (host);
    }

  /* Slow down polling to every ten seconds.
     Some kernels have a bug which causes retrying connect to fail
     after a connect.  Polling can interfere with gethostbyname too.  */
#ifdef POLL_FOR_INPUT
  if (socktype == SOCK_STREAM)
    {
      record_unwind_protect (unwind_stop_other_atimers, Qnil);
      bind_polling_period (10);
    }
#endif

#ifdef HAVE_GETADDRINFO
  /* If we have a host, use getaddrinfo to resolve both host and service.
     Otherwise, use getservbyname to lookup the service.  */
  if (!NILP (host))
    {

      /* SERVICE can either be a string or int.
	 Convert to a C string for later use by getaddrinfo.  */
      if (EQ (service, Qt))
	portstring = "0";
      else if (INTEGERP (service))
	{
	  sprintf (portbuf, "%ld", (long) XINT (service));
	  portstring = portbuf;
	}
      else
	{
	  CHECK_STRING (service);
	  portstring = XSTRING (service)->data;
	}

      immediate_quit = 1;
      QUIT;
      memset (&hints, 0, sizeof (hints));
      hints.ai_flags = 0;
      hints.ai_family = NILP (Fplist_member (contact, QCfamily)) ? AF_UNSPEC : family;
      hints.ai_socktype = socktype;
      hints.ai_protocol = 0;
      ret = getaddrinfo (XSTRING (host)->data, portstring, &hints, &res);
      if (ret)
#ifdef HAVE_GAI_STRERROR
	error ("%s/%s %s", XSTRING (host)->data, portstring, gai_strerror(ret));
#else
        error ("%s/%s getaddrinfo error %d", XSTRING (host)->data, portstring, ret);
#endif
      immediate_quit = 0;

      goto open_socket;
    }
#endif /* HAVE_GETADDRINFO */

  /* We end up here if getaddrinfo is not defined, or in case no hostname
     has been specified (e.g. for a local server process).  */

  if (EQ (service, Qt))
    port = 0;
  else if (INTEGERP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      struct servent *svc_info;
      CHECK_STRING (service);
      svc_info = getservbyname (XSTRING (service)->data, 
				(socktype == SOCK_DGRAM ? "udp" : "tcp"));
      if (svc_info == 0)
	error ("Unknown service: %s", XSTRING (service)->data);
      port = svc_info->s_port;
    }

  bzero (&address_in, sizeof address_in);
  address_in.sin_family = family;
  address_in.sin_addr.s_addr = INADDR_ANY;
  address_in.sin_port = port;

#ifndef HAVE_GETADDRINFO
  if (!NILP (host))
    {
      struct hostent *host_info_ptr;

      /* gethostbyname may fail with TRY_AGAIN, but we don't honour that,
	 as it may `hang' emacs for a very long time.  */
      immediate_quit = 1;
      QUIT;
      host_info_ptr = gethostbyname (XSTRING (host)->data);
      immediate_quit = 0;
  
      if (host_info_ptr)
	{
	  bcopy (host_info_ptr->h_addr, (char *) &address_in.sin_addr,
		 host_info_ptr->h_length);
	  family = host_info_ptr->h_addrtype;
	  address_in.sin_family = family;
	}
      else
	/* Attempt to interpret host as numeric inet address */
	{
	  IN_ADDR numeric_addr;
	  numeric_addr = inet_addr ((char *) XSTRING (host)->data);
	  if (NUMERIC_ADDR_ERROR)
	    error ("Unknown host \"%s\"", XSTRING (host)->data);

	  bcopy ((char *)&numeric_addr, (char *) &address_in.sin_addr,
		 sizeof (address_in.sin_addr));
	}

    }
#endif /* not HAVE_GETADDRINFO */

  ai.ai_family = family;
  ai.ai_addr = (struct sockaddr *) &address_in;
  ai.ai_addrlen = sizeof address_in;

 open_socket:

  /* Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
     when connect is interrupted.  So let's not let it get interrupted.
     Note we do not turn off polling, because polling is only used
     when not interrupt_input, and thus not normally used on the systems
     which have this bug.  On systems which use polling, there's no way
     to quit if polling is turned off.  */
  if (interrupt_input
      && !is_server && socktype == SOCK_STREAM)
    {
      /* Comment from KFS: The original open-network-stream code
	 didn't unwind protect this, but it seems like the proper
	 thing to do.  In any case, I don't see how it could harm to
	 do this -- and it makes cleanup (using unbind_to) easier.  */
      record_unwind_protect (unwind_request_sigio, Qnil);
      unrequest_sigio ();
    }

  /* Do this in case we never enter the for-loop below.  */
  count1 = specpdl_ptr - specpdl;
  s = -1;

  for (lres = res; lres; lres = lres->ai_next)
    {
      s = socket (lres->ai_family, lres->ai_socktype, lres->ai_protocol);
      if (s < 0)
	{
	  xerrno = errno;
	  continue;
	}

#ifdef DATAGRAM_SOCKETS
      if (!is_server && socktype == SOCK_DGRAM)
	break;
#endif /* DATAGRAM_SOCKETS */

#ifdef NON_BLOCKING_CONNECT
      if (is_non_blocking_client)
	{
#ifdef O_NONBLOCK
	  ret = fcntl (s, F_SETFL, O_NONBLOCK);
#else
	  ret = fcntl (s, F_SETFL, O_NDELAY);
#endif
	  if (ret < 0)
	    {
	      xerrno = errno;
	      emacs_close (s);
	      s = -1;
	      continue;
	    }
	}
#endif
      
      /* Make us close S if quit.  */
      record_unwind_protect (close_file_unwind, make_number (s));

      if (is_server)
	{
	  /* Configure as a server socket.  */
#ifdef HAVE_LOCAL_SOCKETS
	  if (family != AF_LOCAL)
#endif
	    {
	      int optval = 1;
	      if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval))
		report_file_error ("Cannot set reuse option on server socket.", Qnil);
	    }
      
	  if (bind (s, lres->ai_addr, lres->ai_addrlen))
	    report_file_error ("Cannot bind server socket", Qnil);

#ifdef HAVE_GETSOCKNAME
	  if (EQ (service, Qt))
	    {
	      struct sockaddr_in sa1;
	      int len1 = sizeof (sa1);
	      if (getsockname (s, (struct sockaddr *)&sa1, &len1) == 0)
		{
		  ((struct sockaddr_in *)(lres->ai_addr))->sin_port = sa1.sin_port;
		  service = make_number (sa1.sin_port);
		  contact = Fplist_put (contact, QCservice, service);
		}
	    }
#endif

	  if (socktype == SOCK_STREAM && listen (s, 5))
	    report_file_error ("Cannot listen on server socket", Qnil);

	  break;
	}

    retry_connect:

      immediate_quit = 1;
      QUIT;

      /* This turns off all alarm-based interrupts; the
	 bind_polling_period call above doesn't always turn all the
	 short-interval ones off, especially if interrupt_input is
	 set.

	 It'd be nice to be able to control the connect timeout
	 though.  Would non-blocking connect calls be portable? 

	 This used to be conditioned by HAVE_GETADDRINFO.  Why?  */

      turn_on_atimers (0);

      ret = connect (s, lres->ai_addr, lres->ai_addrlen);
      xerrno = errno;

      turn_on_atimers (1);

      if (ret == 0 || xerrno == EISCONN)
	{
	  /* The unwind-protect will be discarded afterwards.
	     Likewise for immediate_quit.  */
	  break;
	}

#ifdef NON_BLOCKING_CONNECT
#ifdef EINPROGRESS
      if (is_non_blocking_client && xerrno == EINPROGRESS)
	break;
#else
#ifdef EWOULDBLOCK
      if (is_non_blocking_client && xerrno == EWOULDBLOCK)
	break;
#endif
#endif
#endif

      immediate_quit = 0;

      if (xerrno == EINTR)
	goto retry_connect;
      if (xerrno == EADDRINUSE && retry < 20)
	{
	  /* A delay here is needed on some FreeBSD systems,
	     and it is harmless, since this retrying takes time anyway
	     and should be infrequent.  */
	  Fsleep_for (make_number (1), Qnil);
	  retry++;
	  goto retry_connect;
	}

      /* Discard the unwind protect closing S.  */
      specpdl_ptr = specpdl + count1;
      emacs_close (s);
      s = -1;
    }

  if (s >= 0)
    {
#ifdef DATAGRAM_SOCKETS
      if (socktype == SOCK_DGRAM)
	{
	  if (datagram_address[s].sa)
	    abort ();
	  datagram_address[s].sa = (struct sockaddr *) xmalloc (lres->ai_addrlen);
	  datagram_address[s].len = lres->ai_addrlen;
	  if (is_server)
	    {
	      Lisp_Object remote;
	      bzero (datagram_address[s].sa, lres->ai_addrlen);
	      if (remote = Fplist_get (contact, QCremote), !NILP (remote))
		{
		  int rfamily, rlen;
		  rlen = get_lisp_to_sockaddr_size (remote, &rfamily);
		  if (rfamily == lres->ai_family && rlen == lres->ai_addrlen)
		    conv_lisp_to_sockaddr (rfamily, remote,
					   datagram_address[s].sa, rlen);
		}
	    }
	  else
	    bcopy (lres->ai_addr, datagram_address[s].sa, lres->ai_addrlen);
	}
#endif
      contact = Fplist_put (contact, QCaddress, 
			    conv_sockaddr_to_lisp (lres->ai_addr, lres->ai_addrlen));
    }

#ifdef HAVE_GETADDRINFO
  if (res != &ai)
    freeaddrinfo (res);
#endif

  immediate_quit = 0;

  /* Discard the unwind protect for closing S, if any.  */
  specpdl_ptr = specpdl + count1;

  /* Unwind bind_polling_period and request_sigio.  */
  unbind_to (count, Qnil);

  if (s < 0)
    {
      /* If non-blocking got this far - and failed - assume non-blocking is
	 not supported after all.  This is probably a wrong assumption, but
	 the normal blocking calls to open-network-stream handles this error
	 better.  */
      if (is_non_blocking_client)
	  return Qnil;

      errno = xerrno;
      if (is_server)
	report_file_error ("make server process failed", contact);
      else
	report_file_error ("make client process failed", contact);
    }

  tem = Fplist_get (contact, QCoptions);
  if (!NILP (tem))
    set_socket_options (s, tem, 1);

#endif /* not TERM */

  inch = s;
  outch = s;

  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process (name);

  chan_process[inch] = proc;

#ifdef O_NONBLOCK
  fcntl (inch, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inch, F_SETFL, O_NDELAY);
#endif
#endif

  p = XPROCESS (proc);

  p->childp = contact;
  p->buffer = buffer;
  p->sentinel = sentinel;
  p->filter = filter;
  p->log = Fplist_get (contact, QClog);
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = Qt;
  if ((tem = Fplist_get (contact, QCstop), !NILP (tem)))
    p->command = Qt;
  p->pid = Qnil;
  XSETINT (p->infd, inch);
  XSETINT (p->outfd, outch);
  if (is_server && socktype == SOCK_STREAM)
    p->status = Qlisten;

#ifdef NON_BLOCKING_CONNECT
  if (is_non_blocking_client)
    {
      /* We may get here if connect did succeed immediately.  However,
	 in that case, we still need to signal this like a non-blocking
	 connection.  */
      p->status = Qconnect;
      if (!FD_ISSET (inch, &connect_wait_mask))
	{
	  FD_SET (inch, &connect_wait_mask);
	  num_pending_connects++;
	}
    }
  else
#endif
    /* A server may have a client filter setting of Qt, but it must
       still listen for incoming connects unless it is stopped.  */
    if ((!EQ (p->filter, Qt) && !EQ (p->command, Qt))
	|| (EQ (p->status, Qlisten) && NILP (p->command)))
      {
	FD_SET (inch, &input_wait_mask);
	FD_SET (inch, &non_keyboard_wait_mask);
      }

  if (inch > max_process_desc)
    max_process_desc = inch;

  tem = Fplist_member (contact, QCcoding);
  if (!NILP (tem) && (!CONSP (tem) || !CONSP (XCDR (tem))))
    tem = Qnil;  /* No error message (too late!).  */

  {
    /* Setup coding systems for communicating with the network stream.  */
    struct gcpro gcpro1;
    /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
    Lisp_Object coding_systems = Qt;
    Lisp_Object args[5], val;

    if (!NILP (tem))
      val = XCAR (XCDR (tem));
    else if (!NILP (Vcoding_system_for_read))
      val = Vcoding_system_for_read;
    else if ((!NILP (buffer) && NILP (XBUFFER (buffer)->enable_multibyte_characters))
	     || (NILP (buffer) && NILP (buffer_defaults.enable_multibyte_characters)))
      /* We dare not decode end-of-line format by setting VAL to
	 Qraw_text, because the existing Emacs Lisp libraries
	 assume that they receive bare code including a sequene of
	 CR LF.  */
      val = Qnil;
    else
      {
	args[0] = Qopen_network_stream, args[1] = name,
	  args[2] = buffer, args[3] = host, args[4] = service;
	GCPRO1 (proc);
	coding_systems = Ffind_operation_coding_system (5, args);
	UNGCPRO;
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    p->decode_coding_system = val;

    if (!NILP (tem))
      val = XCAR (XCDR (tem));
    else if (!NILP (Vcoding_system_for_write))
      val = Vcoding_system_for_write;
    else if (NILP (current_buffer->enable_multibyte_characters))
      val = Qnil;
    else
      {
	if (EQ (coding_systems, Qt))
	  {
	    args[0] = Qopen_network_stream, args[1] = name,
	      args[2] = buffer, args[3] = host, args[4] = service;
	    GCPRO1 (proc);
	    coding_systems = Ffind_operation_coding_system (5, args);
	    UNGCPRO;
	  }
	if (CONSP (coding_systems))
	  val = XCDR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCDR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    p->encode_coding_system = val;
  }

  if (!proc_decode_coding_system[inch])
    proc_decode_coding_system[inch]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->decode_coding_system,
		       proc_decode_coding_system[inch]);
  if (!proc_encode_coding_system[outch])
    proc_encode_coding_system[outch]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->encode_coding_system,
		       proc_encode_coding_system[outch]);

  p->decoding_buf = make_uninit_string (0);
  p->decoding_carryover = make_number (0);
  p->encoding_buf = make_uninit_string (0);
  p->encoding_carryover = make_number (0);

  p->inherit_coding_system_flag
    = (!NILP (tem) || NILP (buffer) || !inherit_process_coding_system
       ? Qnil : Qt);

  UNGCPRO;
  return proc;
}
#endif	/* HAVE_SOCKETS */

void
deactivate_process (proc)
     Lisp_Object proc;
{
  register int inchannel, outchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

  inchannel = XINT (p->infd);
  outchannel = XINT (p->outfd);

  if (inchannel >= 0)
    {
      /* Beware SIGCHLD hereabouts. */
      flush_pending_output (inchannel);
#ifdef VMS
      {
	VMS_PROC_STUFF *get_vms_process_pointer (), *vs;
	sys$dassgn (outchannel);
	vs = get_vms_process_pointer (p->pid);
	if (vs)
	  give_back_vms_process_stuff (vs);
      }
#else
      emacs_close (inchannel);
      if (outchannel >= 0 && outchannel != inchannel)
 	emacs_close (outchannel);
#endif

      XSETINT (p->infd, -1);
      XSETINT (p->outfd, -1);
#ifdef DATAGRAM_SOCKETS
      if (DATAGRAM_CHAN_P (inchannel))
	{
	  xfree (datagram_address[inchannel].sa);
	  datagram_address[inchannel].sa = 0;
	  datagram_address[inchannel].len = 0;
	}
#endif
      chan_process[inchannel] = Qnil;
      FD_CLR (inchannel, &input_wait_mask);
      FD_CLR (inchannel, &non_keyboard_wait_mask);
      if (FD_ISSET (inchannel, &connect_wait_mask))
	{
	  FD_CLR (inchannel, &connect_wait_mask);
	  if (--num_pending_connects < 0)
	    abort ();
	}
      if (inchannel == max_process_desc)
	{
	  int i;
	  /* We just closed the highest-numbered process input descriptor,
	     so recompute the highest-numbered one now.  */
	  max_process_desc = 0;
	  for (i = 0; i < MAXDESC; i++)
	    if (!NILP (chan_process[i]))
	      max_process_desc = i;
	}
    }
}

/* Close all descriptors currently in use for communication
   with subprocess.  This is used in a newly-forked subprocess
   to get rid of irrelevant descriptors.  */

void
close_process_descs ()
{
#ifndef WINDOWSNT
  int i;
  for (i = 0; i < MAXDESC; i++)
    {
      Lisp_Object process;
      process = chan_process[i];
      if (!NILP (process))
	{
	  int in = XINT (XPROCESS (process)->infd);
	  int out = XINT (XPROCESS (process)->outfd);
	  if (in >= 0)
	    emacs_close (in);
	  if (out >= 0 && in != out)
	    emacs_close (out);
	}
    }
#endif
}

DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
       0, 3, 0,
       doc: /* Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Non-nil arg PROCESS means do not return until some output has been received
from PROCESS.
Non-nil second arg TIMEOUT and third arg TIMEOUT-MSECS are number of
seconds and microseconds to wait; return after that much time whether
or not there is input.
Return non-nil iff we received any output before the timeout expired.  */)
     (process, timeout, timeout_msecs)
     register Lisp_Object process, timeout, timeout_msecs;
{
  int seconds;
  int useconds;

  if (! NILP (process))
    CHECK_PROCESS (process);

  if (! NILP (timeout_msecs))
    {
      CHECK_NUMBER (timeout_msecs);
      useconds = XINT (timeout_msecs);
      if (!INTEGERP (timeout))
	XSETINT (timeout, 0);

      {
	int carry = useconds / 1000000;

	XSETINT (timeout, XINT (timeout) + carry);
	useconds -= carry * 1000000;

	/* I think this clause is necessary because C doesn't
	   guarantee a particular rounding direction for negative
	   integers.  */
	if (useconds < 0)
	  {
	    XSETINT (timeout, XINT (timeout) - 1);
	    useconds += 1000000;
	  }
      }
    }
  else
    useconds = 0;

  if (! NILP (timeout))
    {
      CHECK_NUMBER (timeout);
      seconds = XINT (timeout);
      if (seconds < 0 || (seconds == 0 && useconds == 0))
	seconds = -1;
    }
  else
    {
      if (NILP (process))
	seconds = -1;
      else
	seconds = 0;
    }

  if (NILP (process))
    XSETFASTINT (process, 0);

  return
    (wait_reading_process_input (seconds, useconds, process, 0)
     ? Qt : Qnil);
}

/* Accept a connection for server process SERVER on CHANNEL.  */

static int connect_counter = 0;

static void
server_accept_connection (server, channel)
     Lisp_Object server;
     int channel;
{
  Lisp_Object proc, caller, name, buffer;
  Lisp_Object contact, host, service;
  struct Lisp_Process *ps= XPROCESS (server);
  struct Lisp_Process *p;
  int s;
  union u_sockaddr {
    struct sockaddr sa;
    struct sockaddr_in in;
#ifdef HAVE_LOCAL_SOCKETS
    struct sockaddr_un un;
#endif
  } saddr;
  int len = sizeof saddr;

  s = accept (channel, &saddr.sa, &len);

  if (s < 0)
    {
      int code = errno;

      if (code == EAGAIN)
	return;
#ifdef EWOULDBLOCK
      if (code == EWOULDBLOCK)
	return;
#endif

      if (!NILP (ps->log))
	call3 (ps->log, server, Qnil,
	       concat3 (build_string ("accept failed with code"),
			Fnumber_to_string (make_number (code)),
			build_string ("\n")));
      return;
    }

  connect_counter++;

  /* Setup a new process to handle the connection.  */

  /* Generate a unique identification of the caller, and build contact
     information for this process.  */
  host = Qt;
  service = Qnil;
  switch (saddr.sa.sa_family)
    {
    case AF_INET:
      {
	Lisp_Object args[5];
	unsigned char *ip = (unsigned char *)&saddr.in.sin_addr.s_addr;
	args[0] = build_string ("%d.%d.%d.%d");
	args[1] = make_number (*ip++);
	args[2] = make_number (*ip++);
	args[3] = make_number (*ip++);
	args[4] = make_number (*ip++);
	host = Fformat (5, args);
	service = make_number (ntohs (saddr.in.sin_port));

	args[0] = build_string (" <%s:%d>");
	args[1] = host;
	args[2] = service;
	caller = Fformat (3, args);
      }
      break;

#ifdef HAVE_LOCAL_SOCKETS
    case AF_LOCAL:
#endif
    default:
      caller = Fnumber_to_string (make_number (connect_counter));
      caller = concat3 (build_string (" <*"), caller, build_string ("*>"));
      break;
    }

  /* Create a new buffer name for this process if it doesn't have a
     filter.  The new buffer name is based on the buffer name or
     process name of the server process concatenated with the caller
     identification.  */

  if (!NILP (ps->filter) && !EQ (ps->filter, Qt))
    buffer = Qnil;
  else
    {
      buffer = ps->buffer;
      if (!NILP (buffer))
	buffer = Fbuffer_name (buffer);
      else
	buffer = ps->name;
      if (!NILP (buffer))
	{
	  buffer = concat2 (buffer, caller);
	  buffer = Fget_buffer_create (buffer);
	}
    }

  /* Generate a unique name for the new server process.  Combine the
     server process name with the caller identification.  */

  name = concat2 (ps->name, caller);
  proc = make_process (name);

  chan_process[s] = proc;

#ifdef O_NONBLOCK
  fcntl (s, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (s, F_SETFL, O_NDELAY);
#endif
#endif

  p = XPROCESS (proc);

  /* Build new contact information for this setup.  */
  contact = Fcopy_sequence (ps->childp);
  contact = Fplist_put (contact, QCserver, Qnil);
  contact = Fplist_put (contact, QChost, host);
  if (!NILP (service))
    contact = Fplist_put (contact, QCservice, service);
  contact = Fplist_put (contact, QCremote, 
			conv_sockaddr_to_lisp (&saddr.sa, len));
#ifdef HAVE_GETSOCKNAME
  len = sizeof saddr;
  if (getsockname (channel, &saddr.sa, &len) == 0)
    contact = Fplist_put (contact, QClocal, 
			  conv_sockaddr_to_lisp (&saddr.sa, len));
#endif

  p->childp = contact;
  p->buffer = buffer;
  p->sentinel = ps->sentinel;
  p->filter = ps->filter;
  p->command = Qnil;
  p->pid = Qnil;
  XSETINT (p->infd, s);
  XSETINT (p->outfd, s);
  p->status = Qrun;

  /* Client processes for accepted connections are not stopped initially.  */
  if (!EQ (p->filter, Qt))
    {
      FD_SET (s, &input_wait_mask);
      FD_SET (s, &non_keyboard_wait_mask);
    }

  if (s > max_process_desc)
    max_process_desc = s;

  /* Setup coding system for new process based on server process.  
     This seems to be the proper thing to do, as the coding system
     of the new process should reflect the settings at the time the
     server socket was opened; not the current settings. */

  p->decode_coding_system = ps->decode_coding_system;
  p->encode_coding_system = ps->encode_coding_system;

  if (!proc_decode_coding_system[s])
    proc_decode_coding_system[s]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->decode_coding_system,
		       proc_decode_coding_system[s]);
  if (!proc_encode_coding_system[s])
    proc_encode_coding_system[s]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->encode_coding_system,
		       proc_encode_coding_system[s]);

  p->decoding_buf = make_uninit_string (0);
  p->decoding_carryover = make_number (0);
  p->encoding_buf = make_uninit_string (0);
  p->encoding_carryover = make_number (0);

  p->inherit_coding_system_flag
    = (NILP (buffer) ? Qnil : ps->inherit_coding_system_flag);

  if (!NILP (ps->log))
      call3 (ps->log, server, proc,
	     concat3 (build_string ("accept from "),
		      (STRINGP (host) ? host : build_string ("-")),
		      build_string ("\n")));

  if (!NILP (p->sentinel))
    exec_sentinel (proc, 
		   concat3 (build_string ("open from "),
			    (STRINGP (host) ? host : build_string ("-")),
			    build_string ("\n")));
}

/* This variable is different from waiting_for_input in keyboard.c.
   It is used to communicate to a lisp process-filter/sentinel (via the
   function Fwaiting_for_user_input_p below) whether emacs was waiting
   for user-input when that process-filter was called.
   waiting_for_input cannot be used as that is by definition 0 when
   lisp code is being evalled.
   This is also used in record_asynch_buffer_change.
   For that purpose, this must be 0
   when not inside wait_reading_process_input.  */
static int waiting_for_user_input_p;

/* This is here so breakpoints can be put on it.  */
static void
wait_reading_process_input_1 ()
{
}

/* Read and dispose of subprocess output while waiting for timeout to
   elapse and/or keyboard input to be available.

   TIME_LIMIT is:
     timeout in seconds, or
     zero for no limit, or
     -1 means gobble data immediately available but don't wait for any.

   MICROSECS is:
     an additional duration to wait, measured in microseconds.
     If this is nonzero and time_limit is 0, then the timeout
     consists of MICROSECS only.

   READ_KBD is a lisp value:
     0 to ignore keyboard input, or
     1 to return when input is available, or
     -1 meaning caller will actually read the input, so don't throw to
       the quit handler, or
     a cons cell, meaning wait until its car is non-nil
       (and gobble terminal input into the buffer if any arrives), or
     a process object, meaning wait until something arrives from that
       process.  The return value is true iff we read some input from
       that process.

   DO_DISPLAY != 0 means redisplay should be done to show subprocess
   output that arrives.

   If READ_KBD is a pointer to a struct Lisp_Process, then the
     function returns true iff we received input from that process
     before the timeout elapsed.
   Otherwise, return true iff we received input from any process.  */

int
wait_reading_process_input (time_limit, microsecs, read_kbd, do_display)
     int time_limit, microsecs;
     Lisp_Object read_kbd;
     int do_display;
{
  register int channel, nfds;
  static SELECT_TYPE Available;
  static SELECT_TYPE Connecting;
  int check_connect, no_avail;
  int xerrno;
  Lisp_Object proc;
  EMACS_TIME timeout, end_time;
  int wait_channel = -1;
  struct Lisp_Process *wait_proc = 0;
  int got_some_input = 0;
  /* Either nil or a cons cell, the car of which is of interest and
     may be changed outside of this routine.  */
  Lisp_Object wait_for_cell = Qnil;

  FD_ZERO (&Available);
  FD_ZERO (&Connecting);

  /* If read_kbd is a process to watch, set wait_proc and wait_channel
     accordingly.  */
  if (PROCESSP (read_kbd))
    {
      wait_proc = XPROCESS (read_kbd);
      wait_channel = XINT (wait_proc->infd);
      XSETFASTINT (read_kbd, 0);
    }

  /* If waiting for non-nil in a cell, record where.  */
  if (CONSP (read_kbd))
    {
      wait_for_cell = read_kbd;
      XSETFASTINT (read_kbd, 0);
    }

  waiting_for_user_input_p = XINT (read_kbd);

  /* Since we may need to wait several times,
     compute the absolute time to return at.  */
  if (time_limit || microsecs)
    {
      EMACS_GET_TIME (end_time);
      EMACS_SET_SECS_USECS (timeout, time_limit, microsecs);
      EMACS_ADD_TIME (end_time, end_time, timeout);
    }
#ifdef hpux
  /* AlainF 5-Jul-1996
     HP-UX 10.10 seem to have problems with signals coming in
     Causes "poll: interrupted system call" messages when Emacs is run
     in an X window
     Turn off periodic alarms (in case they are in use) */
  turn_on_atimers (0);
#endif

  while (1)
    {
      int timeout_reduced_for_timers = 0;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (XINT (read_kbd) >= 0)
	QUIT;

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

      /* Compute time from now till when time limit is up */
      /* Exit if already run out */
      if (time_limit == -1)
	{
	  /* -1 specified for timeout means
	     gobble output available now
	     but don't wait at all. */

	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	}
      else if (time_limit || microsecs)
	{
	  EMACS_GET_TIME (timeout);
	  EMACS_SUB_TIME (timeout, end_time, timeout);
	  if (EMACS_TIME_NEG_P (timeout))
	    break;
	}
      else
	{
	  EMACS_SET_SECS_USECS (timeout, 100000, 0);
	}

      /* Normally we run timers here.
	 But not if wait_for_cell; in those cases,
	 the wait is supposed to be short,
	 and those callers cannot handle running arbitrary Lisp code here.  */
      if (NILP (wait_for_cell))
	{
	  EMACS_TIME timer_delay;

	  do
	    {
	      int old_timers_run = timers_run;
	      struct buffer *old_buffer = current_buffer;
	      
	      timer_delay = timer_check (1);

	      /* If a timer has run, this might have changed buffers
		 an alike.  Make read_key_sequence aware of that.  */
	      if (timers_run != old_timers_run
		  && old_buffer != current_buffer
		  && waiting_for_user_input_p == -1)
		record_asynch_buffer_change ();
	      
	      if (timers_run != old_timers_run && do_display)
		/* We must retry, since a timer may have requeued itself
		   and that could alter the time_delay.  */
		redisplay_preserve_echo_area (9);
	      else
		break;
	    }
	  while (!detect_input_pending ());

	  /* If there is unread keyboard input, also return.  */
	  if (XINT (read_kbd) != 0
	      && requeued_events_pending_p ())
	    break;

	  if (! EMACS_TIME_NEG_P (timer_delay) && time_limit != -1)
	    {
	      EMACS_TIME difference;
	      EMACS_SUB_TIME (difference, timer_delay, timeout);
	      if (EMACS_TIME_NEG_P (difference))
		{
		  timeout = timer_delay;
		  timeout_reduced_for_timers = 1;
		}
	    }
	  /* If time_limit is -1, we are not going to wait at all.  */
	  else if (time_limit != -1)
	    {
	      /* This is so a breakpoint can be put here.  */
	      wait_reading_process_input_1 ();
	    }
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout.

	 It is important that we do this before checking for process
	 activity.  If we get a SIGCHLD after the explicit checks for
	 process activity, timeout is the only way we will know.  */
      if (XINT (read_kbd) < 0)
	set_waiting_for_input (&timeout);

      /* If status of something has changed, and no input is
	 available, notify the user of the change right away.  After
	 this explicit check, we'll let the SIGCHLD handler zap
	 timeout to get our attention.  */
      if (update_tick != process_tick && do_display)
	{
	  SELECT_TYPE Atemp, Ctemp;

	  Atemp = input_wait_mask;
#ifdef MAC_OSX
          /* On Mac OS X, the SELECT system call always says input is
             present (for reading) at stdin, even when none is.  This
             causes the the call to SELECT below to return 1 and
             status_notify not to be called.  As a result output of
             subprocesses are incorrectly discarded.  */
          FD_CLR (0, &Atemp);
#endif
	  Ctemp = connect_wait_mask;
	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	  if ((select (max (max_process_desc, max_keyboard_desc) + 1,
		       &Atemp, 
		       (num_pending_connects > 0 ? &Ctemp : (SELECT_TYPE *)0),
		       (SELECT_TYPE *)0, &timeout)
	       <= 0))
	    {
	      /* It's okay for us to do this and then continue with
		 the loop, since timeout has already been zeroed out.  */
	      clear_waiting_for_input ();
	      status_notify ();
	    }
	}

      /* Don't wait for output from a non-running process.  Just
         read whatever data has already been received.  */
      if (wait_proc != 0 && !NILP (wait_proc->raw_status_low))
	update_status (wait_proc);
      if (wait_proc != 0
	  && ! EQ (wait_proc->status, Qrun)
	  && ! EQ (wait_proc->status, Qconnect))
	{
	  int nread, total_nread = 0;

	  clear_waiting_for_input ();
	  XSETPROCESS (proc, wait_proc);

	  /* Read data from the process, until we exhaust it.  */
	  while (XINT (wait_proc->infd) >= 0)
	    {
	      nread = read_process_output (proc, XINT (wait_proc->infd));

	      if (nread == 0)
		break;

              if (0 < nread) 
                total_nread += nread;
#ifdef EIO
	      else if (nread == -1 && EIO == errno)
                break;
#endif
#ifdef EAGAIN
	      else if (nread == -1 && EAGAIN == errno)
                break;
#endif
#ifdef EWOULDBLOCK
	      else if (nread == -1 && EWOULDBLOCK == errno)
                break;
#endif
	    }
	  if (total_nread > 0 && do_display)
	    redisplay_preserve_echo_area (10);

	  break;
	}

      /* Wait till there is something to do */

      if (!NILP (wait_for_cell))
	{
	  Available = non_process_wait_mask;
	  check_connect = 0;
	}
      else
	{
	  if (! XINT (read_kbd))
	    Available = non_keyboard_wait_mask;
	  else
	    Available = input_wait_mask;
	  check_connect = (num_pending_connects > 0);
	}

      /* If frame size has changed or the window is newly mapped,
	 redisplay now, before we start to wait.  There is a race
	 condition here; if a SIGIO arrives between now and the select
	 and indicates that a frame is trashed, the select may block
	 displaying a trashed screen.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (11);
	  if (XINT (read_kbd) < 0)
	    set_waiting_for_input (&timeout);
	}

      no_avail = 0;
      if (XINT (read_kbd) && detect_input_pending ())
	{
	  nfds = 0;
	  no_avail = 1;
	}
      else
	{
	  if (check_connect)
	    Connecting = connect_wait_mask;
	  nfds = select (max (max_process_desc, max_keyboard_desc) + 1,
			 &Available, 
			 (check_connect ? &Connecting : (SELECT_TYPE *)0),
			 (SELECT_TYPE *)0, &timeout);
	}

      xerrno = errno;

      /* Make C-g and alarm signals set flags again */
      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (time_limit && nfds == 0 && ! timeout_reduced_for_timers)
	/* We wanted the full specified time, so return now.  */
	break;
      if (nfds < 0)
	{
	  if (xerrno == EINTR)
	    no_avail = 1;
#ifdef ultrix
	  /* Ultrix select seems to return ENOMEM when it is
	     interrupted.  Treat it just like EINTR.  Bleah.  Note
	     that we want to test for the "ultrix" CPP symbol, not
	     "__ultrix__"; the latter is only defined under GCC, but
	     not by DEC's bundled CC.  -JimB  */
	  else if (xerrno == ENOMEM)
	    no_avail = 1;
#endif
#ifdef ALLIANT
	  /* This happens for no known reason on ALLIANT.
	     I am guessing that this is the right response. -- RMS.  */
	  else if (xerrno == EFAULT)
	    no_avail = 1;
#endif
	  else if (xerrno == EBADF)
	    {
#ifdef AIX
	      /* AIX doesn't handle PTY closure the same way BSD does.  On AIX,
		 the child's closure of the pts gives the parent a SIGHUP, and
		 the ptc file descriptor is automatically closed,
		 yielding EBADF here or at select() call above.
		 So, SIGHUP is ignored (see def of PTY_TTY_NAME_SPRINTF
		 in m/ibmrt-aix.h), and here we just ignore the select error.
		 Cleanup occurs c/o status_notify after SIGCLD. */
	      no_avail = 1; /* Cannot depend on values returned */
#else
	      abort ();
#endif
	    }
	  else
	    error ("select error: %s", emacs_strerror (xerrno));
	}

      if (no_avail)
	{
	  FD_ZERO (&Available);
	  check_connect = 0;
	}

#if defined(sun) && !defined(USG5_4)
      if (nfds > 0 && keyboard_bit_set (&Available)
	  && interrupt_input)
	/* System sometimes fails to deliver SIGIO.

	   David J. Mackenzie says that Emacs doesn't compile under
	   Solaris if this code is enabled, thus the USG5_4 in the CPP
	   conditional.  "I haven't noticed any ill effects so far.
	   If you find a Solaris expert somewhere, they might know
	   better." */
	kill (getpid (), SIGIO);
#endif

#if 0 /* When polling is used, interrupt_input is 0,
	 so get_input_pending should read the input.
	 So this should not be needed.  */
      /* If we are using polling for input,
	 and we see input available, make it get read now.
	 Otherwise it might not actually get read for a second.
	 And on hpux, since we turn off polling in wait_reading_process_input,
	 it might never get read at all if we don't spend much time
	 outside of wait_reading_process_input.  */
      if (XINT (read_kbd) && interrupt_input
	  && keyboard_bit_set (&Available)
	  && input_polling_used ())
	kill (getpid (), SIGALRM);
#endif

      /* Check for keyboard input */
      /* If there is any, return immediately
	 to give it higher priority than subprocesses */

      if (XINT (read_kbd) != 0)
	{
	  int old_timers_run = timers_run;
	  struct buffer *old_buffer = current_buffer;
	  int leave = 0;
	
	  if (detect_input_pending_run_timers (do_display))
	    {
	      swallow_events (do_display);
	      if (detect_input_pending_run_timers (do_display))
		leave = 1;
	    }

	  /* If a timer has run, this might have changed buffers
	     an alike.  Make read_key_sequence aware of that.  */
	  if (timers_run != old_timers_run
	      && waiting_for_user_input_p == -1
	      && old_buffer != current_buffer)
	    record_asynch_buffer_change ();

	  if (leave)
	    break;
	}    
      
      /* If there is unread keyboard input, also return.  */
      if (XINT (read_kbd) != 0
	  && requeued_events_pending_p ())
	break;

      /* If we are not checking for keyboard input now,
	 do process events (but don't run any timers).
	 This is so that X events will be processed.
	 Otherwise they may have to wait until polling takes place.
	 That would causes delays in pasting selections, for example.

	 (We used to do this only if wait_for_cell.)  */
      if (XINT (read_kbd) == 0 && detect_input_pending ())
	{
	  swallow_events (do_display);
#if 0  /* Exiting when read_kbd doesn't request that seems wrong, though.  */
	  if (detect_input_pending ())
	    break;
#endif
	}

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

#ifdef SIGIO
      /* If we think we have keyboard input waiting, but didn't get SIGIO,
	 go read it.  This can happen with X on BSD after logging out.
	 In that case, there really is no input and no SIGIO,
	 but select says there is input.  */

      if (XINT (read_kbd) && interrupt_input
	  && keyboard_bit_set (&Available))
	kill (getpid (), SIGIO);
#endif

      if (! wait_proc)
	got_some_input |= nfds > 0;

      /* If checking input just got us a size-change event from X,
	 obey it now if we should.  */
      if (XINT (read_kbd) || ! NILP (wait_for_cell))
	do_pending_window_change (0);

      /* Check for data from a process.  */
      if (no_avail || nfds == 0)
	continue;

      /* Really FIRST_PROC_DESC should be 0 on Unix,
	 but this is safer in the short run.  */
      for (channel = 0; channel <= max_process_desc; channel++)
	{
	  if (FD_ISSET (channel, &Available)
	      && FD_ISSET (channel, &non_keyboard_wait_mask))
	    {
	      int nread;

	      /* If waiting for this channel, arrange to return as
		 soon as no more input to be processed.  No more
		 waiting.  */
	      if (wait_channel == channel)
		{
		  wait_channel = -1;
		  time_limit = -1;
		  got_some_input = 1;
		}
	      proc = chan_process[channel];
	      if (NILP (proc))
		continue;

	      /* If this is a server stream socket, accept connection.  */
	      if (EQ (XPROCESS (proc)->status, Qlisten))
		{
		  server_accept_connection (proc, channel);
		  continue;
		}

	      /* Read data from the process, starting with our
		 buffered-ahead character if we have one.  */

	      nread = read_process_output (proc, channel);
	      if (nread > 0)
		{
		  /* Since read_process_output can run a filter,
		     which can call accept-process-output,
		     don't try to read from any other processes
		     before doing the select again.  */
		  FD_ZERO (&Available);

		  if (do_display)
		    redisplay_preserve_echo_area (12);
		}
#ifdef EWOULDBLOCK
	      else if (nread == -1 && errno == EWOULDBLOCK)
		;
#endif
	      /* ISC 4.1 defines both EWOULDBLOCK and O_NONBLOCK,
		 and Emacs uses O_NONBLOCK, so what we get is EAGAIN.  */
#ifdef O_NONBLOCK
	      else if (nread == -1 && errno == EAGAIN)
		;
#else
#ifdef O_NDELAY
	      else if (nread == -1 && errno == EAGAIN)
		;
	      /* Note that we cannot distinguish between no input
		 available now and a closed pipe.
		 With luck, a closed pipe will be accompanied by
		 subprocess termination and SIGCHLD.  */
	      else if (nread == 0 && !NETCONN_P (proc))
		;
#endif				/* O_NDELAY */
#endif				/* O_NONBLOCK */
#ifdef HAVE_PTYS
	      /* On some OSs with ptys, when the process on one end of
		 a pty exits, the other end gets an error reading with
		 errno = EIO instead of getting an EOF (0 bytes read).
		 Therefore, if we get an error reading and errno =
		 EIO, just continue, because the child process has
		 exited and should clean itself up soon (e.g. when we
		 get a SIGCHLD).

		 However, it has been known to happen that the SIGCHLD
		 got lost.  So raise the signl again just in case.
		 It can't hurt.  */
	      else if (nread == -1 && errno == EIO)
		kill (getpid (), SIGCHLD);
#endif				/* HAVE_PTYS */
	      /* If we can detect process termination, don't consider the process
		 gone just because its pipe is closed.  */
#ifdef SIGCHLD
	      else if (nread == 0 && !NETCONN_P (proc))
		;
#endif
	      else
		{
		  /* Preserve status of processes already terminated.  */
		  XSETINT (XPROCESS (proc)->tick, ++process_tick);
		  deactivate_process (proc);
		  if (!NILP (XPROCESS (proc)->raw_status_low))
		    update_status (XPROCESS (proc));
		  if (EQ (XPROCESS (proc)->status, Qrun))
		    XPROCESS (proc)->status
		      = Fcons (Qexit, Fcons (make_number (256), Qnil));
		}
	    }
#ifdef NON_BLOCKING_CONNECT	  
	  if (check_connect && FD_ISSET (channel, &Connecting))
	    {
	      struct Lisp_Process *p;

	      FD_CLR (channel, &connect_wait_mask);
	      if (--num_pending_connects < 0)
		abort ();

	      proc = chan_process[channel];
	      if (NILP (proc))
		continue;

	      p = XPROCESS (proc);

#ifdef GNU_LINUX
	      /* getsockopt(,,SO_ERROR,,) is said to hang on some systems.
	         So only use it on systems where it is known to work.  */
	      {
		int xlen = sizeof(xerrno);
		if (getsockopt(channel, SOL_SOCKET, SO_ERROR, &xerrno, &xlen))
		  xerrno = errno;
	      }
#else
	      {
		struct sockaddr pname;
		int pnamelen = sizeof(pname);

		/* If connection failed, getpeername will fail.  */
		xerrno = 0;
		if (getpeername(channel, &pname, &pnamelen) < 0)
		  {
		    /* Obtain connect failure code through error slippage.  */
		    char dummy;
		    xerrno = errno;
		    if (errno == ENOTCONN && read(channel, &dummy, 1) < 0)
		      xerrno = errno;
		  }
	      }
#endif
	      if (xerrno)
		{
		  XSETINT (p->tick, ++process_tick);
		  p->status = Fcons (Qfailed, Fcons (make_number (xerrno), Qnil));
		  deactivate_process (proc);
		}
	      else
		{
		  p->status = Qrun;
		  /* Execute the sentinel here.  If we had relied on
		     status_notify to do it later, it will read input
		     from the process before calling the sentinel.  */
		  exec_sentinel (proc, build_string ("open\n"));
		  if (!EQ (p->filter, Qt) && !EQ (p->command, Qt))
		    {
		      FD_SET (XINT (p->infd), &input_wait_mask);
		      FD_SET (XINT (p->infd), &non_keyboard_wait_mask);
		    }
		}
	    }
#endif /* NON_BLOCKING_CONNECT */
	}			/* end for each file descriptor */
    }				/* end while exit conditions not met */

  waiting_for_user_input_p = 0;

  /* If calling from keyboard input, do not quit
     since we want to return C-g as an input character.
     Otherwise, do pending quit if requested.  */
  if (XINT (read_kbd) >= 0)
    {
      /* Prevent input_pending from remaining set if we quit.  */
      clear_input_pending ();
      QUIT;
    }
#ifdef hpux
  /* AlainF 5-Jul-1996
     HP-UX 10.10 seems to have problems with signals coming in
     Causes "poll: interrupted system call" messages when Emacs is run
     in an X window
     Turn periodic alarms back on */
  start_polling ();
#endif

  return got_some_input;
}

/* Given a list (FUNCTION ARGS...), apply FUNCTION to the ARGS.  */

static Lisp_Object
read_process_output_call (fun_and_args)
     Lisp_Object fun_and_args;
{
  return apply1 (XCAR (fun_and_args), XCDR (fun_and_args));
}

static Lisp_Object
read_process_output_error_handler (error)
     Lisp_Object error;
{
  cmd_error_internal (error, "error in process filter: ");
  Vinhibit_quit = Qt;
  update_echo_area ();
  Fsleep_for (make_number (2), Qnil);
  return Qt;
}

/* Read pending output from the process channel,
   starting with our buffered-ahead character if we have one.
   Yield number of decoded characters read.

   This function reads at most 1024 characters.
   If you want to read all available subprocess output,
   you must call it repeatedly until it returns zero.

   The characters read are decoded according to PROC's coding-system
   for decoding.  */

int
read_process_output (proc, channel)
     Lisp_Object proc;
     register int channel;
{
  register int nchars, nbytes;
  char *chars;
  register Lisp_Object outstream;
  register struct buffer *old = current_buffer;
  register struct Lisp_Process *p = XPROCESS (proc);
  register int opoint;
  struct coding_system *coding = proc_decode_coding_system[channel];
  int carryover = XINT (p->decoding_carryover);
  int readmax = 1024;

#ifdef VMS
  VMS_PROC_STUFF *vs, *get_vms_process_pointer();

  vs = get_vms_process_pointer (p->pid);
  if (vs)
    {
      if (!vs->iosb[0])
	return (0);		/* Really weird if it does this */
      if (!(vs->iosb[0] & 1))
	return -1;		/* I/O error */
    }
  else
    error ("Could not get VMS process pointer");
  chars = vs->inputBuffer;
  nbytes = clean_vms_buffer (chars, vs->iosb[1]);
  if (nbytes <= 0)
    {
      start_vms_process_read (vs); /* Crank up the next read on the process */
      return 1;			/* Nothing worth printing, say we got 1 */
    }
  if (carryover > 0)
    {
      /* The data carried over in the previous decoding (which are at
         the tail of decoding buffer) should be prepended to the new
         data read to decode all together.  */
      chars = (char *) alloca (nbytes + carryover);
      bcopy (XSTRING (p->decoding_buf)->data, buf, carryover);
      bcopy (vs->inputBuffer, chars + carryover, nbytes);
    }
#else /* not VMS */

#ifdef DATAGRAM_SOCKETS
  /* A datagram is one packet; allow at least 1500+ bytes of data
     corresponding to the typical Ethernet frame size.  */
  if (DATAGRAM_CHAN_P (channel))
    {
      /* carryover = 0; */  /* Does carryover make sense for datagrams? */
      readmax += 1024;
    }
#endif

  chars = (char *) alloca (carryover + readmax);
  if (carryover)
    /* See the comment above.  */
    bcopy (XSTRING (p->decoding_buf)->data, chars, carryover);

#ifdef DATAGRAM_SOCKETS
  /* We have a working select, so proc_buffered_char is always -1.  */
  if (DATAGRAM_CHAN_P (channel))
    {
      int len = datagram_address[channel].len;
      nbytes = recvfrom (channel, chars + carryover, readmax - carryover,
			 0, datagram_address[channel].sa, &len);
    }
  else
#endif
  if (proc_buffered_char[channel] < 0)
    nbytes = emacs_read (channel, chars + carryover, readmax - carryover);
  else
    {
      chars[carryover] = proc_buffered_char[channel];
      proc_buffered_char[channel] = -1;
      nbytes = emacs_read (channel, chars + carryover + 1,  readmax - 1 - carryover);
      if (nbytes < 0)
	nbytes = 1;
      else
	nbytes = nbytes + 1;
    }
#endif /* not VMS */

  XSETINT (p->decoding_carryover, 0);

  /* At this point, NBYTES holds number of bytes just received
     (including the one in proc_buffered_char[channel]).  */
  if (nbytes <= 0)
    {
      if (nbytes < 0 || coding->mode & CODING_MODE_LAST_BLOCK)
	return nbytes;
      coding->mode |= CODING_MODE_LAST_BLOCK;
    }

  /* Now set NBYTES how many bytes we must decode.  */
  nbytes += carryover;

  /* Read and dispose of the process output.  */
  outstream = p->filter;
  if (!NILP (outstream))
    {
      /* We inhibit quit here instead of just catching it so that 
	 hitting ^G when a filter happens to be running won't screw
	 it up.  */
      int count = specpdl_ptr - specpdl;
      Lisp_Object odeactivate;
      Lisp_Object obuffer, okeymap;
      Lisp_Object text;
      int outer_running_asynch_code = running_asynch_code;
      int waiting = waiting_for_user_input_p;

      /* No need to gcpro these, because all we do with them later
	 is test them for EQness, and none of them should be a string.  */
      odeactivate = Vdeactivate_mark;
      XSETBUFFER (obuffer, current_buffer);
      okeymap = current_buffer->keymap;

      specbind (Qinhibit_quit, Qt);
      specbind (Qlast_nonmenu_event, Qt);

      /* In case we get recursively called,
	 and we already saved the match data nonrecursively,
	 save the same match data in safely recursive fashion.  */
      if (outer_running_asynch_code)
	{
	  Lisp_Object tem;
	  /* Don't clobber the CURRENT match data, either!  */
	  tem = Fmatch_data (Qnil, Qnil);
	  restore_match_data ();
	  record_unwind_protect (Fset_match_data, Fmatch_data (Qnil, Qnil));
	  Fset_match_data (tem);
	}

      /* For speed, if a search happens within this code,
	 save the match data in a special nonrecursive fashion.  */
      running_asynch_code = 1;

      text = decode_coding_string (make_unibyte_string (chars, nbytes),
				   coding, 0);
      if (NILP (buffer_defaults.enable_multibyte_characters))
	/* We had better return unibyte string.  */
	text = string_make_unibyte (text);

      Vlast_coding_system_used = coding->symbol;
      /* A new coding system might be found.  */
      if (!EQ (p->decode_coding_system, coding->symbol))
	{
	  p->decode_coding_system = coding->symbol;

	  /* Don't call setup_coding_system for
	     proc_decode_coding_system[channel] here.  It is done in
	     detect_coding called via decode_coding above.  */

	  /* If a coding system for encoding is not yet decided, we set
	     it as the same as coding-system for decoding.

	     But, before doing that we must check if
	     proc_encode_coding_system[p->outfd] surely points to a
	     valid memory because p->outfd will be changed once EOF is
	     sent to the process.  */
	  if (NILP (p->encode_coding_system)
	      && proc_encode_coding_system[XINT (p->outfd)])
	    {
	      p->encode_coding_system = coding->symbol;
	      setup_coding_system (coding->symbol,
				   proc_encode_coding_system[XINT (p->outfd)]);
	    }
	}

      carryover = nbytes - coding->consumed;
      bcopy (chars + coding->consumed, XSTRING (p->decoding_buf)->data,
	     carryover);
      XSETINT (p->decoding_carryover, carryover);
      nbytes = STRING_BYTES (XSTRING (text));
      nchars = XSTRING (text)->size;
      if (nbytes > 0)
	internal_condition_case_1 (read_process_output_call,
				   Fcons (outstream,
					  Fcons (proc, Fcons (text, Qnil))),
				   !NILP (Vdebug_on_error) ? Qnil : Qerror,
				   read_process_output_error_handler);

      /* If we saved the match data nonrecursively, restore it now.  */
      restore_match_data ();
      running_asynch_code = outer_running_asynch_code;

      /* Handling the process output should not deactivate the mark.  */
      Vdeactivate_mark = odeactivate;

      /* Restore waiting_for_user_input_p as it was
	 when we were called, in case the filter clobbered it.  */
      waiting_for_user_input_p = waiting;

#if 0 /* Call record_asynch_buffer_change unconditionally,
	 because we might have changed minor modes or other things
	 that affect key bindings.  */
      if (! EQ (Fcurrent_buffer (), obuffer)
	  || ! EQ (current_buffer->keymap, okeymap))
#endif
	/* But do it only if the caller is actually going to read events.
	   Otherwise there's no need to make him wake up, and it could
	   cause trouble (for example it would make Fsit_for return).  */
	if (waiting_for_user_input_p == -1)
	  record_asynch_buffer_change ();

#ifdef VMS
      start_vms_process_read (vs);
#endif
      unbind_to (count, Qnil);
      return nchars;
    }

  /* If no filter, write into buffer if it isn't dead.  */
  if (!NILP (p->buffer) && !NILP (XBUFFER (p->buffer)->name))
    {
      Lisp_Object old_read_only;
      int old_begv, old_zv;
      int old_begv_byte, old_zv_byte;
      Lisp_Object odeactivate;
      int before, before_byte;
      int opoint_byte;
      Lisp_Object text;
      struct buffer *b;

      odeactivate = Vdeactivate_mark;

      Fset_buffer (p->buffer);
      opoint = PT;
      opoint_byte = PT_BYTE;
      old_read_only = current_buffer->read_only;
      old_begv = BEGV;
      old_zv = ZV;
      old_begv_byte = BEGV_BYTE;
      old_zv_byte = ZV_BYTE;

      current_buffer->read_only = Qnil;

      /* Insert new output into buffer
	 at the current end-of-output marker,
	 thus preserving logical ordering of input and output.  */
      if (XMARKER (p->mark)->buffer)
	SET_PT_BOTH (clip_to_bounds (BEGV, marker_position (p->mark), ZV),
		     clip_to_bounds (BEGV_BYTE, marker_byte_position (p->mark),
				     ZV_BYTE));
      else
	SET_PT_BOTH (ZV, ZV_BYTE);
      before = PT;
      before_byte = PT_BYTE;

      /* If the output marker is outside of the visible region, save
	 the restriction and widen.  */
      if (! (BEGV <= PT && PT <= ZV))
	Fwiden ();

      text = decode_coding_string (make_unibyte_string (chars, nbytes),
				   coding, 0);
      Vlast_coding_system_used = coding->symbol;
      /* A new coding system might be found.  See the comment in the
	 similar code in the previous `if' block.  */
      if (!EQ (p->decode_coding_system, coding->symbol))
	{
	  p->decode_coding_system = coding->symbol;
	  if (NILP (p->encode_coding_system)
	      && proc_encode_coding_system[XINT (p->outfd)])
	    {
	      p->encode_coding_system = coding->symbol;
	      setup_coding_system (coding->symbol,
				   proc_encode_coding_system[XINT (p->outfd)]);
	    }
	}
      carryover = nbytes - coding->consumed;
      bcopy (chars + coding->consumed, XSTRING (p->decoding_buf)->data,
	     carryover);
      XSETINT (p->decoding_carryover, carryover);
      /* Adjust the multibyteness of TEXT to that of the buffer.  */
      if (NILP (current_buffer->enable_multibyte_characters)
	  != ! STRING_MULTIBYTE (text))
	text = (STRING_MULTIBYTE (text)
		? Fstring_as_unibyte (text)
		: Fstring_as_multibyte (text));
      nbytes = STRING_BYTES (XSTRING (text));
      nchars = XSTRING (text)->size;
      /* Insert before markers in case we are inserting where
	 the buffer's mark is, and the user's next command is Meta-y.  */
      insert_from_string_before_markers (text, 0, 0, nchars, nbytes, 0);

      /* Make sure the process marker's position is valid when the
	 process buffer is changed in the signal_after_change above.
	 W3 is known to do that.  */
      if (BUFFERP (p->buffer)
	  && (b = XBUFFER (p->buffer), b != current_buffer))
	set_marker_both (p->mark, p->buffer, BUF_PT (b), BUF_PT_BYTE (b));
      else
	set_marker_both (p->mark, p->buffer, PT, PT_BYTE);

      update_mode_lines++;

      /* Make sure opoint and the old restrictions
	 float ahead of any new text just as point would.  */
      if (opoint >= before)
	{
	  opoint += PT - before;
	  opoint_byte += PT_BYTE - before_byte;
	}
      if (old_begv > before)
	{
	  old_begv += PT - before;
	  old_begv_byte += PT_BYTE - before_byte;
	}
      if (old_zv >= before)
	{
	  old_zv += PT - before;
	  old_zv_byte += PT_BYTE - before_byte;
	}

      /* If the restriction isn't what it should be, set it.  */
      if (old_begv != BEGV || old_zv != ZV)
	Fnarrow_to_region (make_number (old_begv), make_number (old_zv));

      /* Handling the process output should not deactivate the mark.  */
      Vdeactivate_mark = odeactivate;

      current_buffer->read_only = old_read_only;
      SET_PT_BOTH (opoint, opoint_byte);
      set_buffer_internal (old);
    }
#ifdef VMS
  start_vms_process_read (vs);
#endif
  return nbytes;
}

DEFUN ("waiting-for-user-input-p", Fwaiting_for_user_input_p, Swaiting_for_user_input_p,
       0, 0, 0,
       doc: /* Returns non-nil if emacs is waiting for input from the user.
This is intended for use by asynchronous process output filters and sentinels.  */)
     ()
{
  return (waiting_for_user_input_p ? Qt : Qnil);
}

/* Sending data to subprocess */

jmp_buf send_process_frame;
Lisp_Object process_sent_to;

SIGTYPE
send_process_trap ()
{
#ifdef BSD4_1
  sigrelse (SIGPIPE);
  sigrelse (SIGALRM);
#endif /* BSD4_1 */
  longjmp (send_process_frame, 1);
}

/* Send some data to process PROC.
   BUF is the beginning of the data; LEN is the number of characters.
   OBJECT is the Lisp object that the data comes from.  If OBJECT is
   nil or t, it means that the data comes from C string.

   If OBJECT is not nil, the data is encoded by PROC's coding-system
   for encoding before it is sent.

   This function can evaluate Lisp code and can garbage collect.  */

void
send_process (proc, buf, len, object)
     volatile Lisp_Object proc;
     unsigned char *volatile buf;
     volatile int len;
     volatile Lisp_Object object;
{
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  int rv;
  struct coding_system *coding;
  struct gcpro gcpro1;

  GCPRO1 (object);

#ifdef VMS
  struct Lisp_Process *p = XPROCESS (proc);
  VMS_PROC_STUFF *vs, *get_vms_process_pointer();
#endif /* VMS */

  if (! NILP (XPROCESS (proc)->raw_status_low))
    update_status (XPROCESS (proc));
  if (! EQ (XPROCESS (proc)->status, Qrun))
    error ("Process %s not running",
	   XSTRING (XPROCESS (proc)->name)->data);
  if (XINT (XPROCESS (proc)->outfd) < 0)
    error ("Output file descriptor of %s is closed",
	   XSTRING (XPROCESS (proc)->name)->data);

  coding = proc_encode_coding_system[XINT (XPROCESS (proc)->outfd)];
  Vlast_coding_system_used = coding->symbol;

  if ((STRINGP (object) && STRING_MULTIBYTE (object))
      || (BUFFERP (object)
	  && !NILP (XBUFFER (object)->enable_multibyte_characters))
      || EQ (object, Qt))
    {
      if (!EQ (coding->symbol, XPROCESS (proc)->encode_coding_system))
	/* The coding system for encoding was changed to raw-text
	   because we sent a unibyte text previously.  Now we are
	   sending a multibyte text, thus we must encode it by the
	   original coding system specified for the current
	   process.  */
	setup_coding_system (XPROCESS (proc)->encode_coding_system, coding);
      /* src_multibyte should be set to 1 _after_ a call to
	 setup_coding_system, since it resets src_multibyte to
	 zero.  */
      coding->src_multibyte = 1;
    }
  else
    {
      /* For sending a unibyte text, character code conversion should
	 not take place but EOL conversion should.  So, setup raw-text
	 or one of the subsidiary if we have not yet done it.  */
      if (coding->type != coding_type_raw_text)
	{
	  if (CODING_REQUIRE_FLUSHING (coding))
	    {
	      /* But, before changing the coding, we must flush out data.  */
	      coding->mode |= CODING_MODE_LAST_BLOCK;
	      send_process (proc, "", 0, Qt);
	    }
	  coding->src_multibyte = 0;
	  setup_raw_text_coding_system (coding);
	}
    }
  coding->dst_multibyte = 0;

  if (CODING_REQUIRE_ENCODING (coding))
    {
      int require = encoding_buffer_size (coding, len);
      int from_byte = -1, from = -1, to = -1;
      unsigned char *temp_buf = NULL;

      if (BUFFERP (object))
	{
	  from_byte = BUF_PTR_BYTE_POS (XBUFFER (object), buf);
	  from = buf_bytepos_to_charpos (XBUFFER (object), from_byte);
	  to = buf_bytepos_to_charpos (XBUFFER (object), from_byte + len);
	}
      else if (STRINGP (object))
	{
	  from_byte = buf - XSTRING (object)->data;
	  from = string_byte_to_char (object, from_byte);
	  to =  string_byte_to_char (object, from_byte + len);
	}

      if (coding->composing != COMPOSITION_DISABLED)
	{
	  if (from_byte >= 0)
	    coding_save_composition (coding, from, to, object);
	  else
	    coding->composing = COMPOSITION_DISABLED;
	}

      if (STRING_BYTES (XSTRING (XPROCESS (proc)->encoding_buf)) < require)
	XPROCESS (proc)->encoding_buf = make_uninit_string (require);

      if (from_byte >= 0)
	buf = (BUFFERP (object)
	       ? BUF_BYTE_ADDRESS (XBUFFER (object), from_byte)
	       : XSTRING (object)->data + from_byte);

      object = XPROCESS (proc)->encoding_buf;
      encode_coding (coding, (char *) buf, XSTRING (object)->data,
		     len, STRING_BYTES (XSTRING (object)));
      len = coding->produced;
      buf = XSTRING (object)->data;
      if (temp_buf)
	xfree (temp_buf);
    }

#ifdef VMS
  vs = get_vms_process_pointer (p->pid);
  if (vs == 0)
    error ("Could not find this process: %x", p->pid);
  else if (write_to_vms_process (vs, buf, len))
    ;
#else /* not VMS */

  if (pty_max_bytes == 0)
    {
#if defined (HAVE_FPATHCONF) && defined (_PC_MAX_CANON)
      pty_max_bytes = fpathconf (XFASTINT (XPROCESS (proc)->outfd),
				 _PC_MAX_CANON);
      if (pty_max_bytes < 0)
	pty_max_bytes = 250;
#else
      pty_max_bytes = 250;
#endif
      /* Deduct one, to leave space for the eof.  */
      pty_max_bytes--;
    }

  /* 2000-09-21: Emacs 20.7, sparc-sun-solaris-2.6, GCC 2.95.2,
     CFLAGS="-g -O": The value of the parameter `proc' is clobbered
     when returning with longjmp despite being declared volatile.  */
  if (!setjmp (send_process_frame))
    {
      process_sent_to = proc;
      while (len > 0)
	{
	  int this = len;
	  SIGTYPE (*old_sigpipe)();

	  /* Decide how much data we can send in one batch.
	     Long lines need to be split into multiple batches.  */
	  if (!NILP (XPROCESS (proc)->pty_flag))
	    {
	      /* Starting this at zero is always correct when not the first
                 iteration because the previous iteration ended by sending C-d.
		 It may not be correct for the first iteration
		 if a partial line was sent in a separate send_process call.
		 If that proves worth handling, we need to save linepos
		 in the process object.  */
	      int linepos = 0;
	      unsigned char *ptr = (unsigned char *) buf;
	      unsigned char *end = (unsigned char *) buf + len;

	      /* Scan through this text for a line that is too long.  */
	      while (ptr != end && linepos < pty_max_bytes)
		{
		  if (*ptr == '\n')
		    linepos = 0;
		  else
		    linepos++;
		  ptr++;
		}
	      /* If we found one, break the line there
		 and put in a C-d to force the buffer through.  */
	      this = ptr - buf;
	    }

	  /* Send this batch, using one or more write calls.  */
	  while (this > 0)
	    {
	      int outfd = XINT (XPROCESS (proc)->outfd);
	      old_sigpipe = (SIGTYPE (*) ()) signal (SIGPIPE, send_process_trap);
#ifdef DATAGRAM_SOCKETS
	      if (DATAGRAM_CHAN_P (outfd))
		{
		  rv = sendto (outfd, (char *) buf, this,
			       0, datagram_address[outfd].sa,
			       datagram_address[outfd].len);
		  if (rv < 0 && errno == EMSGSIZE)
		    report_file_error ("sending datagram", Fcons (proc, Qnil));
		}
	      else
#endif
		rv = emacs_write (outfd, (char *) buf, this);
	      signal (SIGPIPE, old_sigpipe);

	      if (rv < 0)
		{
		  if (0
#ifdef EWOULDBLOCK
		      || errno == EWOULDBLOCK
#endif
#ifdef EAGAIN
		      || errno == EAGAIN
#endif
		      )
		    /* Buffer is full.  Wait, accepting input; 
		       that may allow the program
		       to finish doing output and read more.  */
		    {
		      Lisp_Object zero;
		      int offset = 0;

#ifdef BROKEN_PTY_READ_AFTER_EAGAIN
		      /* A gross hack to work around a bug in FreeBSD.
			 In the following sequence, read(2) returns
			 bogus data:

			 write(2)	 1022 bytes
			 write(2)   954 bytes, get EAGAIN
			 read(2)   1024 bytes in process_read_output
			 read(2)     11 bytes in process_read_output

			 That is, read(2) returns more bytes than have
			 ever been written successfully.  The 1033 bytes
			 read are the 1022 bytes written successfully
			 after processing (for example with CRs added if
			 the terminal is set up that way which it is
			 here).  The same bytes will be seen again in a
			 later read(2), without the CRs.  */
		    
		      if (errno == EAGAIN)
			{
			  int flags = FWRITE;
			  ioctl (XINT (XPROCESS (proc)->outfd), TIOCFLUSH,
				 &flags);
			}
#endif /* BROKEN_PTY_READ_AFTER_EAGAIN */
		    
		      /* Running filters might relocate buffers or strings.
			 Arrange to relocate BUF.  */
		      if (BUFFERP (object))
			offset = BUF_PTR_BYTE_POS (XBUFFER (object), buf);
		      else if (STRINGP (object))
			offset = buf - XSTRING (object)->data;

		      XSETFASTINT (zero, 0);
#ifdef EMACS_HAS_USECS
		      wait_reading_process_input (0, 20000, zero, 0);
#else
		      wait_reading_process_input (1, 0, zero, 0);
#endif

		      if (BUFFERP (object))
			buf = BUF_BYTE_ADDRESS (XBUFFER (object), offset);
		      else if (STRINGP (object))
			buf = offset + XSTRING (object)->data;

		      rv = 0;
		    }
		  else
		    /* This is a real error.  */
		    report_file_error ("writing to process", Fcons (proc, Qnil));
		}
	      buf += rv;
	      len -= rv;
	      this -= rv;
	    }

	  /* If we sent just part of the string, put in an EOF
	     to force it through, before we send the rest.  */
	  if (len > 0)
	    Fprocess_send_eof (proc);
	}
    }
#endif /* not VMS */
  else
    {
#ifndef VMS
      proc = process_sent_to;
#endif
      XPROCESS (proc)->raw_status_low = Qnil;
      XPROCESS (proc)->raw_status_high = Qnil;
      XPROCESS (proc)->status = Fcons (Qexit, Fcons (make_number (256), Qnil));
      XSETINT (XPROCESS (proc)->tick, ++process_tick);
      deactivate_process (proc);
#ifdef VMS
      error ("Error writing to process %s; closed it", 
	     XSTRING (XPROCESS (proc)->name)->data);
#else
      error ("SIGPIPE raised on process %s; closed it",
	     XSTRING (XPROCESS (proc)->name)->data);
#endif
    }

  UNGCPRO;
}

DEFUN ("process-send-region", Fprocess_send_region, Sprocess_send_region,
       3, 3, 0,
       doc: /* Send current contents of region as input to PROCESS.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
Called from program, takes three arguments, PROCESS, START and END.
If the region is more than 500 characters long,
it is sent in several bunches.  This may happen even for shorter regions.
Output from processes can arrive in between bunches.  */)
     (process, start, end)
     Lisp_Object process, start, end;
{
  Lisp_Object proc;
  int start1, end1;

  proc = get_process (process);
  validate_region (&start, &end);

  if (XINT (start) < GPT && XINT (end) > GPT)
    move_gap (XINT (start));

  start1 = CHAR_TO_BYTE (XINT (start));
  end1 = CHAR_TO_BYTE (XINT (end));
  send_process (proc, BYTE_POS_ADDR (start1), end1 - start1,
		Fcurrent_buffer ());

  return Qnil;
}

DEFUN ("process-send-string", Fprocess_send_string, Sprocess_send_string,
       2, 2, 0,
       doc: /* Send PROCESS the contents of STRING as input.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If STRING is more than 500 characters long,
it is sent in several bunches.  This may happen even for shorter strings.
Output from processes can arrive in between bunches.  */)
     (process, string)
     Lisp_Object process, string;
{
  Lisp_Object proc;
  CHECK_STRING (string);
  proc = get_process (process);
  send_process (proc, XSTRING (string)->data,
		STRING_BYTES (XSTRING (string)), string);
  return Qnil;
}

DEFUN ("process-running-child-p", Fprocess_running_child_p,
       Sprocess_running_child_p, 0, 1, 0,
       doc: /* Return t if PROCESS has given the terminal to a child.
If the operating system does not make it possible to find out,
return t unconditionally.  */)
     (process)
     Lisp_Object process;
{
  /* Initialize in case ioctl doesn't exist or gives an error,
     in a way that will cause returning t.  */
  int gid = 0;
  Lisp_Object proc;
  struct Lisp_Process *p;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->childp, Qt))
    error ("Process %s is not a subprocess",
	   XSTRING (p->name)->data);
  if (XINT (p->infd) < 0)
    error ("Process %s is not active",
	   XSTRING (p->name)->data);

#ifdef TIOCGPGRP 
  if (!NILP (p->subtty))
    ioctl (XFASTINT (p->subtty), TIOCGPGRP, &gid);
  else
    ioctl (XINT (p->infd), TIOCGPGRP, &gid);
#endif /* defined (TIOCGPGRP ) */

  if (gid == XFASTINT (p->pid))
    return Qnil;
  return Qt;
}

/* send a signal number SIGNO to PROCESS.
   If CURRENT_GROUP is t, that means send to the process group
   that currently owns the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If CURRENT_GROUP is lambda, that means send to the process group
   that currently owns the terminal, but only if it is NOT the shell itself.

   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.

   If we can, we try to signal PROCESS by sending control characters
   down the pty.  This allows us to signal inferiors who have changed
   their uid, for which killpg would return an EPERM error.  */

static void
process_send_signal (process, signo, current_group, nomsg)
     Lisp_Object process;
     int signo;
     Lisp_Object current_group;
     int nomsg;
{
  Lisp_Object proc;
  register struct Lisp_Process *p;
  int gid;
  int no_pgrp = 0;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->childp, Qt))
    error ("Process %s is not a subprocess",
	   XSTRING (p->name)->data);
  if (XINT (p->infd) < 0)
    error ("Process %s is not active",
	   XSTRING (p->name)->data);

  if (NILP (p->pty_flag))
    current_group = Qnil;

  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (!NILP (current_group))
    {
#ifdef SIGNALS_VIA_CHARACTERS
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */

      /* TERMIOS is the latest and bestest, and seems most likely to
         work.  If the system has it, use it.  */
#ifdef HAVE_TERMIOS
      struct termios t;

      switch (signo)
	{
	case SIGINT:
	  tcgetattr (XINT (p->infd), &t);
	  send_process (proc, &t.c_cc[VINTR], 1, Qnil);
	  return;

	case SIGQUIT:
	  tcgetattr (XINT (p->infd), &t);
  	  send_process (proc, &t.c_cc[VQUIT], 1, Qnil);
  	  return;

  	case SIGTSTP:
	  tcgetattr (XINT (p->infd), &t);
#if defined (VSWTCH) && !defined (PREFER_VSUSP)
  	  send_process (proc, &t.c_cc[VSWTCH], 1, Qnil);
#else
	  send_process (proc, &t.c_cc[VSUSP], 1, Qnil);
#endif
  	  return;
	}

#else /* ! HAVE_TERMIOS */

      /* On Berkeley descendants, the following IOCTL's retrieve the
	 current control characters.  */
#if defined (TIOCGLTC) && defined (TIOCGETC)

      struct tchars c;
      struct ltchars lc;

      switch (signo)
	{
	case SIGINT:
	  ioctl (XINT (p->infd), TIOCGETC, &c);
	  send_process (proc, &c.t_intrc, 1, Qnil);
	  return;
	case SIGQUIT:
	  ioctl (XINT (p->infd), TIOCGETC, &c);
	  send_process (proc, &c.t_quitc, 1, Qnil);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (XINT (p->infd), TIOCGLTC, &lc);
	  send_process (proc, &lc.t_suspc, 1, Qnil);
	  return;
#endif /* ! defined (SIGTSTP) */
	}

#else /* ! defined (TIOCGLTC) && defined (TIOCGETC) */

      /* On SYSV descendants, the TCGETA ioctl retrieves the current control
	 characters.  */
#ifdef TCGETA
      struct termio t;
      switch (signo)
	{
	case SIGINT:
	  ioctl (XINT (p->infd), TCGETA, &t);
	  send_process (proc, &t.c_cc[VINTR], 1, Qnil);
	  return;
	case SIGQUIT:
	  ioctl (XINT (p->infd), TCGETA, &t);
	  send_process (proc, &t.c_cc[VQUIT], 1, Qnil);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (XINT (p->infd), TCGETA, &t);
	  send_process (proc, &t.c_cc[VSWTCH], 1, Qnil);
	  return;
#endif /* ! defined (SIGTSTP) */
	}
#else /* ! defined (TCGETA) */
      Your configuration files are messed up.
      /* If your system configuration files define SIGNALS_VIA_CHARACTERS,
	 you'd better be using one of the alternatives above!  */
#endif /* ! defined (TCGETA) */
#endif /* ! defined (TIOCGLTC) && defined (TIOCGETC) */
#endif /* ! defined HAVE_TERMIOS */
#endif /* ! defined (SIGNALS_VIA_CHARACTERS) */

#ifdef TIOCGPGRP 
      /* Get the pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.
	 On pfa systems, saka@pfu.fujitsu.co.JP writes:
	 "TIOCGPGRP symbol defined in sys/ioctl.h at E50.
	 But, TIOCGPGRP does not work on E50 ;-P works fine on E60"
	 His patch indicates that if TIOCGPGRP returns an error, then
	 we should just assume that p->pid is also the process group id.  */
      {
	int err;

	if (!NILP (p->subtty))
	  err = ioctl (XFASTINT (p->subtty), TIOCGPGRP, &gid);
	else
	  err = ioctl (XINT (p->infd), TIOCGPGRP, &gid);

#ifdef pfa
	if (err == -1)
	  gid = - XFASTINT (p->pid);
#endif /* ! defined (pfa) */
      }
      if (gid == -1)
	no_pgrp = 1;
      else
	gid = - gid;
#else  /* ! defined (TIOCGPGRP ) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = - XFASTINT (p->pid);
#endif /* ! defined (TIOCGPGRP ) */

      /* If current_group is lambda, and the shell owns the terminal,
	 don't send any signal.  */
      if (EQ (current_group, Qlambda) && gid == - XFASTINT (p->pid))
	return;
    }
  else
    gid = - XFASTINT (p->pid);

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      p->raw_status_low = Qnil;
      p->raw_status_high = Qnil;
      p->status = Qrun;
      XSETINT (p->tick, ++process_tick);
      if (!nomsg)
	status_notify ();
      break;
#endif /* ! defined (SIGCONT) */
    case SIGINT:
#ifdef VMS
      send_process (proc, "\003", 1, Qnil);	/* ^C */
      goto whoosh;
#endif
    case SIGQUIT:
#ifdef VMS
      send_process (proc, "\031", 1, Qnil);	/* ^Y */
      goto whoosh;
#endif
    case SIGKILL:
#ifdef VMS
      sys$forcex (&(XFASTINT (p->pid)), 0, 1);
      whoosh:
#endif
      flush_pending_output (XINT (p->infd));
      break;
    }

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  if (no_pgrp)
    {
      kill (XFASTINT (p->pid), signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (!NILP (current_group))
    ioctl (XINT (p->infd), TIOCSIGSEND, signo);
  else
    {
      gid = - XFASTINT (p->pid);
      kill (gid, signo);
    }
#else /* ! defined (TIOCSIGSEND) */
  EMACS_KILLPG (-gid, signo);
#endif /* ! defined (TIOCSIGSEND) */
}

DEFUN ("interrupt-process", Finterrupt_process, Sinterrupt_process, 0, 2, 0,
       doc: /* Interrupt process PROCESS.
PROCESS may be a process, a buffer, or the name of a process or buffer.
nil or no arg means current buffer's process.
Second arg CURRENT-GROUP non-nil means send signal to
the current process-group of the process's controlling terminal
rather than to the process's own process group.
If the process is a shell, this means interrupt current subjob
rather than the shell.

If CURRENT-GROUP is `lambda', and if the shell owns the terminal,
don't send the signal.  */)
     (process, current_group)
     Lisp_Object process, current_group;
{
  process_send_signal (process, SIGINT, current_group, 0);
  return process;
}

DEFUN ("kill-process", Fkill_process, Skill_process, 0, 2, 0,
       doc: /* Kill process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  */)
     (process, current_group)
     Lisp_Object process, current_group;
{
  process_send_signal (process, SIGKILL, current_group, 0);
  return process;
}

DEFUN ("quit-process", Fquit_process, Squit_process, 0, 2, 0,
       doc: /* Send QUIT signal to process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  */)
     (process, current_group)
     Lisp_Object process, current_group;
{
  process_send_signal (process, SIGQUIT, current_group, 0);
  return process;
}

DEFUN ("stop-process", Fstop_process, Sstop_process, 0, 2, 0,
       doc: /* Stop process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  
If PROCESS is a network process, inhibit handling of incoming traffic.  */)
     (process, current_group)
     Lisp_Object process, current_group;
{
#ifdef HAVE_SOCKETS
  if (PROCESSP (process) && NETCONN_P (process))
    {
      struct Lisp_Process *p;
  
      p = XPROCESS (process);
      if (NILP (p->command)
	  && XINT (p->infd) >= 0)
	{
	  FD_CLR (XINT (p->infd), &input_wait_mask);
	  FD_CLR (XINT (p->infd), &non_keyboard_wait_mask);
	}
      p->command = Qt;
      return process;
    }
#endif
#ifndef SIGTSTP
  error ("no SIGTSTP support");
#else
  process_send_signal (process, SIGTSTP, current_group, 0);
#endif
  return process;
}

DEFUN ("continue-process", Fcontinue_process, Scontinue_process, 0, 2, 0,
       doc: /* Continue process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  
If PROCESS is a network process, resume handling of incoming traffic.  */)
     (process, current_group)
     Lisp_Object process, current_group;
{
#ifdef HAVE_SOCKETS
  if (PROCESSP (process) && NETCONN_P (process))
    {
      struct Lisp_Process *p;

      p = XPROCESS (process);
      if (EQ (p->command, Qt)
	  && XINT (p->infd) >= 0
	  && (!EQ (p->filter, Qt) || EQ (p->status, Qlisten)))
	{
	  FD_SET (XINT (p->infd), &input_wait_mask);
	  FD_SET (XINT (p->infd), &non_keyboard_wait_mask);
	}
      p->command = Qnil;
      return process;
    }
#endif
#ifdef SIGCONT
    process_send_signal (process, SIGCONT, current_group, 0);
#else
    error ("no SIGCONT support");
#endif
  return process;
}

DEFUN ("signal-process", Fsignal_process, Ssignal_process,
       2, 2, "nProcess number: \nnSignal code: ",
       doc: /* Send the process with process id PID the signal with code SIGCODE.
PID must be an integer.  The process need not be a child of this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.  */)
     (pid, sigcode)
     Lisp_Object pid, sigcode;
{
  CHECK_NUMBER (pid);

#define handle_signal(NAME, VALUE)		\
  else if (!strcmp (name, NAME))		\
    XSETINT (sigcode, VALUE)

  if (INTEGERP (sigcode))
    ;
  else
    {
      unsigned char *name;

      CHECK_SYMBOL (sigcode);
      name = XSYMBOL (sigcode)->name->data;

      if (0)
	;
#ifdef SIGHUP
      handle_signal ("SIGHUP", SIGHUP);
#endif
#ifdef SIGINT
      handle_signal ("SIGINT", SIGINT);
#endif
#ifdef SIGQUIT
      handle_signal ("SIGQUIT", SIGQUIT);
#endif
#ifdef SIGILL
      handle_signal ("SIGILL", SIGILL);
#endif
#ifdef SIGABRT
      handle_signal ("SIGABRT", SIGABRT);
#endif
#ifdef SIGEMT
      handle_signal ("SIGEMT", SIGEMT);
#endif
#ifdef SIGKILL
      handle_signal ("SIGKILL", SIGKILL);
#endif
#ifdef SIGFPE
      handle_signal ("SIGFPE", SIGFPE);
#endif
#ifdef SIGBUS
      handle_signal ("SIGBUS", SIGBUS);
#endif
#ifdef SIGSEGV
      handle_signal ("SIGSEGV", SIGSEGV);
#endif
#ifdef SIGSYS
      handle_signal ("SIGSYS", SIGSYS);
#endif
#ifdef SIGPIPE
      handle_signal ("SIGPIPE", SIGPIPE);
#endif
#ifdef SIGALRM
      handle_signal ("SIGALRM", SIGALRM);
#endif
#ifdef SIGTERM
      handle_signal ("SIGTERM", SIGTERM);
#endif
#ifdef SIGURG
      handle_signal ("SIGURG", SIGURG);
#endif
#ifdef SIGSTOP
      handle_signal ("SIGSTOP", SIGSTOP);
#endif
#ifdef SIGTSTP
      handle_signal ("SIGTSTP", SIGTSTP);
#endif
#ifdef SIGCONT
      handle_signal ("SIGCONT", SIGCONT);
#endif
#ifdef SIGCHLD
      handle_signal ("SIGCHLD", SIGCHLD);
#endif
#ifdef SIGTTIN
      handle_signal ("SIGTTIN", SIGTTIN);
#endif
#ifdef SIGTTOU
      handle_signal ("SIGTTOU", SIGTTOU);
#endif
#ifdef SIGIO
      handle_signal ("SIGIO", SIGIO);
#endif
#ifdef SIGXCPU
      handle_signal ("SIGXCPU", SIGXCPU);
#endif
#ifdef SIGXFSZ
      handle_signal ("SIGXFSZ", SIGXFSZ);
#endif
#ifdef SIGVTALRM
      handle_signal ("SIGVTALRM", SIGVTALRM);
#endif
#ifdef SIGPROF
      handle_signal ("SIGPROF", SIGPROF);
#endif
#ifdef SIGWINCH
      handle_signal ("SIGWINCH", SIGWINCH);
#endif
#ifdef SIGINFO
      handle_signal ("SIGINFO", SIGINFO);
#endif
#ifdef SIGUSR1
      handle_signal ("SIGUSR1", SIGUSR1);
#endif
#ifdef SIGUSR2
      handle_signal ("SIGUSR2", SIGUSR2);
#endif
      else
	error ("Undefined signal name %s", name);
    }

#undef handle_signal

  return make_number (kill (XINT (pid), XINT (sigcode)));
}

DEFUN ("process-send-eof", Fprocess_send_eof, Sprocess_send_eof, 0, 1, 0,
       doc: /* Make PROCESS see end-of-file in its input.
EOF comes after any text already sent to it.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If PROCESS is a network connection, or is a process communicating
through a pipe (as opposed to a pty), then you cannot send any more
text to PROCESS after you call this function.  */)
     (process)
     Lisp_Object process;
{
  Lisp_Object proc;
  struct coding_system *coding;

  if (DATAGRAM_CONN_P (process))
    return process;

  proc = get_process (process);
  coding = proc_encode_coding_system[XINT (XPROCESS (proc)->outfd)];

  /* Make sure the process is really alive.  */
  if (! NILP (XPROCESS (proc)->raw_status_low))
    update_status (XPROCESS (proc));
  if (! EQ (XPROCESS (proc)->status, Qrun))
    error ("Process %s not running", XSTRING (XPROCESS (proc)->name)->data);

  if (CODING_REQUIRE_FLUSHING (coding))
    {
      coding->mode |= CODING_MODE_LAST_BLOCK;
      send_process (proc, "", 0, Qnil);
    }

#ifdef VMS
  send_process (proc, "\032", 1, Qnil); 	/* ^z */
#else
  if (!NILP (XPROCESS (proc)->pty_flag))
    send_process (proc, "\004", 1, Qnil);
  else
    {
      int old_outfd, new_outfd;

#ifdef HAVE_SHUTDOWN
      /* If this is a network connection, or socketpair is used
	 for communication with the subprocess, call shutdown to cause EOF.
	 (In some old system, shutdown to socketpair doesn't work.
	 Then we just can't win.)  */
      if (NILP (XPROCESS (proc)->pid)
	  || XINT (XPROCESS (proc)->outfd) == XINT (XPROCESS (proc)->infd))
	shutdown (XINT (XPROCESS (proc)->outfd), 1);
      /* In case of socketpair, outfd == infd, so don't close it.  */
      if (XINT (XPROCESS (proc)->outfd) != XINT (XPROCESS (proc)->infd))
	emacs_close (XINT (XPROCESS (proc)->outfd));
#else /* not HAVE_SHUTDOWN */
      emacs_close (XINT (XPROCESS (proc)->outfd));
#endif /* not HAVE_SHUTDOWN */
      new_outfd = emacs_open (NULL_DEVICE, O_WRONLY, 0);
      old_outfd = XINT (XPROCESS (proc)->outfd);

      if (!proc_encode_coding_system[new_outfd])
	proc_encode_coding_system[new_outfd]
	  = (struct coding_system *) xmalloc (sizeof (struct coding_system));
      bcopy (proc_encode_coding_system[old_outfd],
	     proc_encode_coding_system[new_outfd],
	     sizeof (struct coding_system));
      bzero (proc_encode_coding_system[old_outfd],
	     sizeof (struct coding_system));

      XSETINT (XPROCESS (proc)->outfd, new_outfd);
    }
#endif /* VMS */
  return process;
}

/* Kill all processes associated with `buffer'.
   If `buffer' is nil, kill all processes  */

void
kill_buffer_processes (buffer)
     Lisp_Object buffer;
{
  Lisp_Object tail, proc;

  for (tail = Vprocess_alist; GC_CONSP (tail); tail = XCDR (tail))
    {
      proc = XCDR (XCAR (tail));
      if (GC_PROCESSP (proc)
	  && (NILP (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	{
	  if (NETCONN_P (proc))
	    Fdelete_process (proc);
	  else if (XINT (XPROCESS (proc)->infd) >= 0)
	    process_send_signal (proc, SIGHUP, Qnil, 1);
	}
    }
}

/* On receipt of a signal that a child status has changed, loop asking
   about children with changed statuses until the system says there
   are no more.
   
   All we do is change the status; we do not run sentinels or print
   notifications.  That is saved for the next time keyboard input is
   done, in order to avoid timing errors.

   ** WARNING: this can be called during garbage collection.
   Therefore, it must not be fooled by the presence of mark bits in
   Lisp objects.

   ** USG WARNING: Although it is not obvious from the documentation
   in signal(2), on a USG system the SIGCLD handler MUST NOT call
   signal() before executing at least one wait(), otherwise the
   handler will be called again, resulting in an infinite loop.  The
   relevant portion of the documentation reads "SIGCLD signals will be
   queued and the signal-catching function will be continually
   reentered until the queue is empty".  Invoking signal() causes the
   kernel to reexamine the SIGCLD queue.  Fred Fish, UniSoft Systems
   Inc. */

SIGTYPE
sigchld_handler (signo)
     int signo;
{
  int old_errno = errno;
  Lisp_Object proc;
  register struct Lisp_Process *p;
  extern EMACS_TIME *input_available_clear_time;

#ifdef BSD4_1
  extern int sigheld;
  sigheld |= sigbit (SIGCHLD);
#endif

  while (1)
    {
      register int pid;
      WAITTYPE w;
      Lisp_Object tail;

#ifdef WNOHANG
#ifndef WUNTRACED
#define WUNTRACED 0
#endif /* no WUNTRACED */
      /* Keep trying to get a status until we get a definitive result.  */
      do 
	{
	  errno = 0;
	  pid = wait3 (&w, WNOHANG | WUNTRACED, 0);
	}
      while (pid < 0 && errno == EINTR);

      if (pid <= 0)
	{
	  /* PID == 0 means no processes found, PID == -1 means a real
	     failure.  We have done all our job, so return.  */

	  /* USG systems forget handlers when they are used;
	     must reestablish each time */
#if defined (USG) && !defined (POSIX_SIGNALS)
	  signal (signo, sigchld_handler);   /* WARNING - must come after wait3() */
#endif
#ifdef  BSD4_1
	  sigheld &= ~sigbit (SIGCHLD);
	  sigrelse (SIGCHLD);
#endif
	  errno = old_errno;
	  return;
	}
#else
      pid = wait (&w);
#endif /* no WNOHANG */

      /* Find the process that signaled us, and record its status.  */

      p = 0;
      for (tail = Vprocess_alist; GC_CONSP (tail); tail = XCDR (tail))
	{
	  proc = XCDR (XCAR (tail));
	  p = XPROCESS (proc);
	  if (GC_EQ (p->childp, Qt) && XINT (p->pid) == pid)
	    break;
	  p = 0;
	}

      /* Look for an asynchronous process whose pid hasn't been filled
	 in yet.  */
      if (p == 0)
	for (tail = Vprocess_alist; GC_CONSP (tail); tail = XCDR (tail))
	  {
	    proc = XCDR (XCAR (tail));
	    p = XPROCESS (proc);
	    if (GC_INTEGERP (p->pid) && XINT (p->pid) == -1)
	      break;
	    p = 0;
	  }
      
      /* Change the status of the process that was found.  */
      if (p != 0)
	{
	  union { int i; WAITTYPE wt; } u;
	  int clear_desc_flag = 0;
	  
	  XSETINT (p->tick, ++process_tick);
	  u.wt = w;
	  XSETINT (p->raw_status_low, u.i & 0xffff);
	  XSETINT (p->raw_status_high, u.i >> 16);
	  
	  /* If process has terminated, stop waiting for its output.  */
	  if ((WIFSIGNALED (w) || WIFEXITED (w))
	      && XINT (p->infd) >= 0)
	    clear_desc_flag = 1;

	  /* We use clear_desc_flag to avoid a compiler bug in Microsoft C.  */
	  if (clear_desc_flag)
	    {
	      FD_CLR (XINT (p->infd), &input_wait_mask);
	      FD_CLR (XINT (p->infd), &non_keyboard_wait_mask);
	    }

	  /* Tell wait_reading_process_input that it needs to wake up and
	     look around.  */
	  if (input_available_clear_time)
	    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
	}

	/* There was no asynchronous process found for that id.  Check
	   if we have a synchronous process.  */
      else
	{
	  synch_process_alive = 0;

	  /* Report the status of the synchronous process.  */
	  if (WIFEXITED (w))
	    synch_process_retcode = WRETCODE (w);
	  else if (WIFSIGNALED (w))
	    {
	      int code = WTERMSIG (w);
	      char *signame;

	      synchronize_system_messages_locale ();
	      signame = strsignal (code);

	      if (signame == 0)
		signame = "unknown";

	      synch_process_death = signame;
	    }

	  /* Tell wait_reading_process_input that it needs to wake up and
	     look around.  */
	  if (input_available_clear_time)
	    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
	}

      /* On some systems, we must return right away.
	 If any more processes want to signal us, we will
	 get another signal.
	 Otherwise (on systems that have WNOHANG), loop around
	 to use up all the processes that have something to tell us.  */
#if (defined WINDOWSNT \
     || (defined USG && !defined GNU_LINUX \
         && !(defined HPUX && defined WNOHANG)))
#if defined (USG) && ! defined (POSIX_SIGNALS)
      signal (signo, sigchld_handler);
#endif
      errno = old_errno;
      return;
#endif /* USG, but not HPUX with WNOHANG */
    }
}


static Lisp_Object
exec_sentinel_unwind (data)
     Lisp_Object data;
{
  XPROCESS (XCAR (data))->sentinel = XCDR (data);
  return Qnil;
}

static Lisp_Object
exec_sentinel_error_handler (error)
     Lisp_Object error;
{
  cmd_error_internal (error, "error in process sentinel: ");
  Vinhibit_quit = Qt;
  update_echo_area ();
  Fsleep_for (make_number (2), Qnil);
  return Qt;
}

static void
exec_sentinel (proc, reason)
     Lisp_Object proc, reason;
{
  Lisp_Object sentinel, obuffer, odeactivate, okeymap;
  register struct Lisp_Process *p = XPROCESS (proc);
  int count = specpdl_ptr - specpdl;
  int outer_running_asynch_code = running_asynch_code;
  int waiting = waiting_for_user_input_p;

  /* No need to gcpro these, because all we do with them later
     is test them for EQness, and none of them should be a string.  */
  odeactivate = Vdeactivate_mark;
  XSETBUFFER (obuffer, current_buffer);
  okeymap = current_buffer->keymap;

  sentinel = p->sentinel;
  if (NILP (sentinel))
    return;

  /* Zilch the sentinel while it's running, to avoid recursive invocations;
     assure that it gets restored no matter how the sentinel exits.  */
  p->sentinel = Qnil;
  record_unwind_protect (exec_sentinel_unwind, Fcons (proc, sentinel));
  /* Inhibit quit so that random quits don't screw up a running filter.  */
  specbind (Qinhibit_quit, Qt);
  specbind (Qlast_nonmenu_event, Qt);

  /* In case we get recursively called,
     and we already saved the match data nonrecursively,
     save the same match data in safely recursive fashion.  */
  if (outer_running_asynch_code)
    {
      Lisp_Object tem;
      tem = Fmatch_data (Qnil, Qnil);
      restore_match_data ();
      record_unwind_protect (Fset_match_data, Fmatch_data (Qnil, Qnil));
      Fset_match_data (tem);
    }

  /* For speed, if a search happens within this code,
     save the match data in a special nonrecursive fashion.  */
  running_asynch_code = 1;

  internal_condition_case_1 (read_process_output_call,
			     Fcons (sentinel,
				    Fcons (proc, Fcons (reason, Qnil))),
			     !NILP (Vdebug_on_error) ? Qnil : Qerror,
			     exec_sentinel_error_handler);

  /* If we saved the match data nonrecursively, restore it now.  */
  restore_match_data ();
  running_asynch_code = outer_running_asynch_code;

  Vdeactivate_mark = odeactivate;

  /* Restore waiting_for_user_input_p as it was
     when we were called, in case the filter clobbered it.  */
  waiting_for_user_input_p = waiting;

#if 0
  if (! EQ (Fcurrent_buffer (), obuffer)
      || ! EQ (current_buffer->keymap, okeymap))
#endif
    /* But do it only if the caller is actually going to read events.
       Otherwise there's no need to make him wake up, and it could
       cause trouble (for example it would make Fsit_for return).  */
    if (waiting_for_user_input_p == -1)
      record_asynch_buffer_change ();

  unbind_to (count, Qnil);
}

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is usually done while Emacs is waiting for keyboard input
   but can be done at other times.  */

void
status_notify ()
{
  register Lisp_Object proc, buffer;
  Lisp_Object tail, msg;
  struct gcpro gcpro1, gcpro2;

  tail = Qnil;
  msg = Qnil;
  /* We need to gcpro tail; if read_process_output calls a filter
     which deletes a process and removes the cons to which tail points
     from Vprocess_alist, and then causes a GC, tail is an unprotected
     reference.  */
  GCPRO2 (tail, msg);

  /* Set this now, so that if new processes are created by sentinels
     that we run, we get called again to handle their status changes.  */
  update_tick = process_tick;

  for (tail = Vprocess_alist; !NILP (tail); tail = Fcdr (tail))
    {
      Lisp_Object symbol;
      register struct Lisp_Process *p;

      proc = Fcdr (Fcar (tail));
      p = XPROCESS (proc);

      if (XINT (p->tick) != XINT (p->update_tick))
	{
	  XSETINT (p->update_tick, XINT (p->tick));

	  /* If process is still active, read any output that remains.  */
	  while (! EQ (p->filter, Qt)
		 && ! EQ (p->status, Qconnect)
		 && ! EQ (p->status, Qlisten)
		 && ! EQ (p->command, Qt)  /* Network process not stopped.  */
		 && XINT (p->infd) >= 0
		 && read_process_output (proc, XINT (p->infd)) > 0);

	  buffer = p->buffer;

	  /* Get the text to use for the message.  */
	  if (!NILP (p->raw_status_low))
	    update_status (p);
	  msg = status_message (p->status);

	  /* If process is terminated, deactivate it or delete it.  */
	  symbol = p->status;
	  if (CONSP (p->status))
	    symbol = XCAR (p->status);

	  if (EQ (symbol, Qsignal) || EQ (symbol, Qexit)
	      || EQ (symbol, Qclosed))
	    {
	      if (delete_exited_processes)
		remove_process (proc);
	      else
		deactivate_process (proc);
	    }

	  /* The actions above may have further incremented p->tick.
	     So set p->update_tick again
	     so that an error in the sentinel will not cause
	     this code to be run again.  */
	  XSETINT (p->update_tick, XINT (p->tick));
	  /* Now output the message suitably.  */
	  if (!NILP (p->sentinel))
	    exec_sentinel (proc, msg);
	  /* Don't bother with a message in the buffer
	     when a process becomes runnable.  */
	  else if (!EQ (symbol, Qrun) && !NILP (buffer))
	    {
	      Lisp_Object ro, tem;
	      struct buffer *old = current_buffer;
	      int opoint, opoint_byte;
	      int before, before_byte;

	      ro = XBUFFER (buffer)->read_only;

	      /* Avoid error if buffer is deleted
		 (probably that's why the process is dead, too) */
	      if (NILP (XBUFFER (buffer)->name))
		continue;
	      Fset_buffer (buffer);

	      opoint = PT;
	      opoint_byte = PT_BYTE;
	      /* Insert new output into buffer
		 at the current end-of-output marker,
		 thus preserving logical ordering of input and output.  */
	      if (XMARKER (p->mark)->buffer)
		Fgoto_char (p->mark);
	      else
		SET_PT_BOTH (ZV, ZV_BYTE);

	      before = PT;
	      before_byte = PT_BYTE;

	      tem = current_buffer->read_only;
	      current_buffer->read_only = Qnil;
	      insert_string ("\nProcess ");
	      Finsert (1, &p->name);
	      insert_string (" ");
	      Finsert (1, &msg);
	      current_buffer->read_only = tem;
	      set_marker_both (p->mark, p->buffer, PT, PT_BYTE);

	      if (opoint >= before)
		SET_PT_BOTH (opoint + (PT - before),
			     opoint_byte + (PT_BYTE - before_byte));
	      else
		SET_PT_BOTH (opoint, opoint_byte);

	      set_buffer_internal (old);
	    }
	}
    } /* end for */

  update_mode_lines++;  /* in case buffers use %s in mode-line-format */
  redisplay_preserve_echo_area (13);

  UNGCPRO;
}


DEFUN ("set-process-coding-system", Fset_process_coding_system,
       Sset_process_coding_system, 1, 3, 0,
       doc: /* Set coding systems of PROCESS to DECODING and ENCODING.
DECODING will be used to decode subprocess output and ENCODING to
encode subprocess input.  */)
     (proc, decoding, encoding)
     register Lisp_Object proc, decoding, encoding;
{
  register struct Lisp_Process *p;

  CHECK_PROCESS (proc);
  p = XPROCESS (proc);
  if (XINT (p->infd) < 0)
    error ("Input file descriptor of %s closed", XSTRING (p->name)->data);
  if (XINT (p->outfd) < 0)
    error ("Output file descriptor of %s closed", XSTRING (p->name)->data);

  p->decode_coding_system = Fcheck_coding_system (decoding);
  p->encode_coding_system = Fcheck_coding_system (encoding);
  setup_coding_system (decoding,
		       proc_decode_coding_system[XINT (p->infd)]);
  setup_coding_system (encoding,
		       proc_encode_coding_system[XINT (p->outfd)]);

  return Qnil;
}

DEFUN ("process-coding-system",
       Fprocess_coding_system, Sprocess_coding_system, 1, 1, 0,
       doc: /* Return a cons of coding systems for decoding and encoding of PROCESS.  */)
     (proc)
     register Lisp_Object proc;
{
  CHECK_PROCESS (proc);
  return Fcons (XPROCESS (proc)->decode_coding_system,
		XPROCESS (proc)->encode_coding_system);
}

/* The first time this is called, assume keyboard input comes from DESC
   instead of from where we used to expect it.
   Subsequent calls mean assume input keyboard can come from DESC
   in addition to other places.  */

static int add_keyboard_wait_descriptor_called_flag;

void
add_keyboard_wait_descriptor (desc)
     int desc;
{
  if (! add_keyboard_wait_descriptor_called_flag)
    FD_CLR (0, &input_wait_mask);
  add_keyboard_wait_descriptor_called_flag = 1;
  FD_SET (desc, &input_wait_mask);
  FD_SET (desc, &non_process_wait_mask);
  if (desc > max_keyboard_desc)
    max_keyboard_desc = desc;
}

/* From now on, do not expect DESC to give keyboard input.  */

void
delete_keyboard_wait_descriptor (desc)
     int desc;
{
  int fd;
  int lim = max_keyboard_desc;

  FD_CLR (desc, &input_wait_mask);
  FD_CLR (desc, &non_process_wait_mask);

  if (desc == max_keyboard_desc)
    for (fd = 0; fd < lim; fd++)
      if (FD_ISSET (fd, &input_wait_mask)
	  && !FD_ISSET (fd, &non_keyboard_wait_mask))
	max_keyboard_desc = fd;
}

/* Return nonzero if *MASK has a bit set
   that corresponds to one of the keyboard input descriptors.  */

int
keyboard_bit_set (mask)
     SELECT_TYPE *mask;
{
  int fd;

  for (fd = 0; fd <= max_keyboard_desc; fd++)
    if (FD_ISSET (fd, mask) && FD_ISSET (fd, &input_wait_mask)
	&& !FD_ISSET (fd, &non_keyboard_wait_mask))
      return 1;

  return 0;
}

void
init_process ()
{
  register int i;

#ifdef SIGCHLD
#ifndef CANNOT_DUMP
  if (! noninteractive || initialized)
#endif
    signal (SIGCHLD, sigchld_handler);
#endif

  FD_ZERO (&input_wait_mask);
  FD_ZERO (&non_keyboard_wait_mask);
  FD_ZERO (&non_process_wait_mask);
  max_process_desc = 0;

  FD_SET (0, &input_wait_mask);

  Vprocess_alist = Qnil;
  for (i = 0; i < MAXDESC; i++)
    {
      chan_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
  bzero (proc_decode_coding_system, sizeof proc_decode_coding_system);
  bzero (proc_encode_coding_system, sizeof proc_encode_coding_system);
#ifdef DATAGRAM_SOCKETS
  bzero (datagram_address, sizeof datagram_address);
#endif

#ifdef HAVE_SOCKETS
 {
   Lisp_Object subfeatures = Qnil;
#define ADD_SUBFEATURE(key, val) \
  subfeatures = Fcons (Fcons (key, Fcons (val, Qnil)), subfeatures)

#ifdef NON_BLOCKING_CONNECT
   ADD_SUBFEATURE (QCnowait, Qt);
#endif
#ifdef DATAGRAM_SOCKETS
   ADD_SUBFEATURE (QCtype, Qdatagram);
#endif
#ifdef HAVE_LOCAL_SOCKETS
   ADD_SUBFEATURE (QCfamily, Qlocal);
#endif
#ifdef HAVE_GETSOCKNAME
   ADD_SUBFEATURE (QCservice, Qt);
#endif
#ifndef TERM
   ADD_SUBFEATURE (QCserver, Qt);
#endif
#ifdef SO_BINDTODEVICE
   ADD_SUBFEATURE (QCoptions, intern ("bindtodevice"));
#endif
#ifdef SO_BROADCAST
   ADD_SUBFEATURE (QCoptions, intern ("broadcast"));
#endif
#ifdef SO_DONTROUTE
   ADD_SUBFEATURE (QCoptions, intern ("dontroute"));
#endif
#ifdef SO_KEEPALIVE
   ADD_SUBFEATURE (QCoptions, intern ("keepalive"));
#endif
#ifdef SO_LINGER
   ADD_SUBFEATURE (QCoptions, intern ("linger"));
#endif
#ifdef SO_OOBINLINE
   ADD_SUBFEATURE (QCoptions, intern ("oobinline"));
#endif
#ifdef SO_PRIORITY
   ADD_SUBFEATURE (QCoptions, intern ("priority"));
#endif
#ifdef SO_REUSEADDR
   ADD_SUBFEATURE (QCoptions, intern ("reuseaddr"));
#endif
   Fprovide (intern ("make-network-process"), subfeatures);
 }
#endif /* HAVE_SOCKETS */
}

void
syms_of_process ()
{
  Qprocessp = intern ("processp");
  staticpro (&Qprocessp);
  Qrun = intern ("run");
  staticpro (&Qrun);
  Qstop = intern ("stop");
  staticpro (&Qstop);
  Qsignal = intern ("signal");
  staticpro (&Qsignal);

  /* Qexit is already staticpro'd by syms_of_eval; don't staticpro it
     here again.

     Qexit = intern ("exit");
     staticpro (&Qexit); */

  Qopen = intern ("open");
  staticpro (&Qopen);
  Qclosed = intern ("closed");
  staticpro (&Qclosed);
  Qconnect = intern ("connect");
  staticpro (&Qconnect);
  Qfailed = intern ("failed");
  staticpro (&Qfailed);
  Qlisten = intern ("listen");
  staticpro (&Qlisten);
  Qlocal = intern ("local");
  staticpro (&Qlocal);
  Qdatagram = intern ("datagram");
  staticpro (&Qdatagram);

  QCname = intern (":name");
  staticpro (&QCname);
  QCbuffer = intern (":buffer");
  staticpro (&QCbuffer);
  QChost = intern (":host");
  staticpro (&QChost);
  QCservice = intern (":service");
  staticpro (&QCservice);
  QCtype = intern (":type");
  staticpro (&QCtype);
  QClocal = intern (":local");
  staticpro (&QClocal);
  QCremote = intern (":remote");
  staticpro (&QCremote);
  QCcoding = intern (":coding");
  staticpro (&QCcoding);
  QCserver = intern (":server");
  staticpro (&QCserver);
  QCnowait = intern (":nowait");
  staticpro (&QCnowait);
  QCsentinel = intern (":sentinel");
  staticpro (&QCsentinel);
  QClog = intern (":log");
  staticpro (&QClog);
  QCnoquery = intern (":noquery");
  staticpro (&QCnoquery);
  QCstop = intern (":stop");
  staticpro (&QCstop);
  QCoptions = intern (":options");
  staticpro (&QCoptions);
    
  Qlast_nonmenu_event = intern ("last-nonmenu-event");
  staticpro (&Qlast_nonmenu_event);

  staticpro (&Vprocess_alist);

  DEFVAR_BOOL ("delete-exited-processes", &delete_exited_processes,
	       doc: /* *Non-nil means delete processes immediately when they exit.
nil means don't delete them until `list-processes' is run.  */);

  delete_exited_processes = 1;

  DEFVAR_LISP ("process-connection-type", &Vprocess_connection_type,
	       doc: /* Control type of device used to communicate with subprocesses.
Values are nil to use a pipe, or t or `pty' to use a pty.
The value has no effect if the system has no ptys or if all ptys are busy:
then a pipe is used in any case.
The value takes effect when `start-process' is called.  */);
  Vprocess_connection_type = Qt;

  defsubr (&Sprocessp);
  defsubr (&Sget_process);
  defsubr (&Sget_buffer_process);
  defsubr (&Sdelete_process);
  defsubr (&Sprocess_status);
  defsubr (&Sprocess_exit_status);
  defsubr (&Sprocess_id);
  defsubr (&Sprocess_name);
  defsubr (&Sprocess_tty_name);
  defsubr (&Sprocess_command);
  defsubr (&Sset_process_buffer);
  defsubr (&Sprocess_buffer);
  defsubr (&Sprocess_mark);
  defsubr (&Sset_process_filter);
  defsubr (&Sprocess_filter);
  defsubr (&Sset_process_sentinel);
  defsubr (&Sprocess_sentinel);
  defsubr (&Sset_process_window_size);
  defsubr (&Sset_process_inherit_coding_system_flag);
  defsubr (&Sprocess_inherit_coding_system_flag);
  defsubr (&Sset_process_query_on_exit_flag);
  defsubr (&Sprocess_query_on_exit_flag);
  defsubr (&Sprocess_contact);
  defsubr (&Slist_processes);
  defsubr (&Sprocess_list);
  defsubr (&Sstart_process);
#ifdef HAVE_SOCKETS
  defsubr (&Sset_network_process_options);
  defsubr (&Smake_network_process);
#endif /* HAVE_SOCKETS */
#ifdef DATAGRAM_SOCKETS
  defsubr (&Sprocess_datagram_address);
  defsubr (&Sset_process_datagram_address);
#endif
  defsubr (&Saccept_process_output);
  defsubr (&Sprocess_send_region);
  defsubr (&Sprocess_send_string);
  defsubr (&Sinterrupt_process);
  defsubr (&Skill_process);
  defsubr (&Squit_process);
  defsubr (&Sstop_process);
  defsubr (&Scontinue_process);
  defsubr (&Sprocess_running_child_p);
  defsubr (&Sprocess_send_eof);
  defsubr (&Ssignal_process);
  defsubr (&Swaiting_for_user_input_p);
/*  defsubr (&Sprocess_connection); */
  defsubr (&Sset_process_coding_system);
  defsubr (&Sprocess_coding_system);
}


#else /* not subprocesses */

#include <sys/types.h>
#include <errno.h>

#include "lisp.h"
#include "systime.h"
#include "charset.h"
#include "coding.h"
#include "termopts.h"
#include "sysselect.h"

extern int frame_garbaged;

extern EMACS_TIME timer_check ();
extern int timers_run;

Lisp_Object QCtype;

/* As described above, except assuming that there are no subprocesses:

   Wait for timeout to elapse and/or keyboard input to be available.

   time_limit is:
     timeout in seconds, or
     zero for no limit, or
     -1 means gobble data immediately available but don't wait for any.

   read_kbd is a Lisp_Object:
     0 to ignore keyboard input, or
     1 to return when input is available, or
     -1 means caller will actually read the input, so don't throw to
       the quit handler.
     a cons cell, meaning wait until its car is non-nil
       (and gobble terminal input into the buffer if any arrives), or
     We know that read_kbd will never be a Lisp_Process, since
     `subprocesses' isn't defined.

   do_display != 0 means redisplay should be done to show subprocess
   output that arrives.

   Return true iff we received input from any process.  */

int
wait_reading_process_input (time_limit, microsecs, read_kbd, do_display)
     int time_limit, microsecs;
     Lisp_Object read_kbd;
     int do_display;
{
  register int nfds;
  EMACS_TIME end_time, timeout;
  SELECT_TYPE waitchannels;
  int xerrno;
  /* Either nil or a cons cell, the car of which is of interest and
     may be changed outside of this routine.  */
  Lisp_Object wait_for_cell = Qnil;

  /* If waiting for non-nil in a cell, record where.  */
  if (CONSP (read_kbd))
    {
      wait_for_cell = read_kbd;
      XSETFASTINT (read_kbd, 0);
    }

  /* What does time_limit really mean?  */
  if (time_limit || microsecs)
    {
      EMACS_GET_TIME (end_time);
      EMACS_SET_SECS_USECS (timeout, time_limit, microsecs);
      EMACS_ADD_TIME (end_time, end_time, timeout);
    }

  /* Turn off periodic alarms (in case they are in use)
     because the select emulator uses alarms.  */
  turn_on_atimers (0);

  while (1)
    {
      int timeout_reduced_for_timers = 0;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (XINT (read_kbd) >= 0)
	QUIT;

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

      /* Compute time from now till when time limit is up */
      /* Exit if already run out */
      if (time_limit == -1)
	{
	  /* -1 specified for timeout means
	     gobble output available now
	     but don't wait at all. */

	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	}
      else if (time_limit || microsecs)
	{
	  EMACS_GET_TIME (timeout);
	  EMACS_SUB_TIME (timeout, end_time, timeout);
	  if (EMACS_TIME_NEG_P (timeout))
	    break;
	}
      else
	{
	  EMACS_SET_SECS_USECS (timeout, 100000, 0);
	}

      /* If our caller will not immediately handle keyboard events,
	 run timer events directly.
	 (Callers that will immediately read keyboard events
	 call timer_delay on their own.)  */
      if (NILP (wait_for_cell))
	{
	  EMACS_TIME timer_delay;

	  do
	    {
	      int old_timers_run = timers_run;
	      timer_delay = timer_check (1);
	      if (timers_run != old_timers_run && do_display)
		/* We must retry, since a timer may have requeued itself
		   and that could alter the time delay.  */
		redisplay_preserve_echo_area (14);
	      else
		break;
	    }
	  while (!detect_input_pending ());

	  /* If there is unread keyboard input, also return.  */
	  if (XINT (read_kbd) != 0
	      && requeued_events_pending_p ())
	    break;

	  if (! EMACS_TIME_NEG_P (timer_delay) && time_limit != -1)
	    {
	      EMACS_TIME difference;
	      EMACS_SUB_TIME (difference, timer_delay, timeout);
	      if (EMACS_TIME_NEG_P (difference))
		{
		  timeout = timer_delay;
		  timeout_reduced_for_timers = 1;
		}
	    }
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout.  */
      if (XINT (read_kbd) < 0)
	set_waiting_for_input (&timeout);

      /* Wait till there is something to do.  */

      if (! XINT (read_kbd) && NILP (wait_for_cell))
	FD_ZERO (&waitchannels);
      else
	FD_SET (0, &waitchannels);

      /* If a frame has been newly mapped and needs updating,
	 reprocess its display stuff.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (15);
	  if (XINT (read_kbd) < 0)
	    set_waiting_for_input (&timeout);
	}

      if (XINT (read_kbd) && detect_input_pending ())
	{
	  nfds = 0;
	  FD_ZERO (&waitchannels);
	}
      else
	nfds = select (1, &waitchannels, (SELECT_TYPE *)0, (SELECT_TYPE *)0,
		       &timeout);

      xerrno = errno;

      /* Make C-g and alarm signals set flags again */
      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (time_limit && nfds == 0 && ! timeout_reduced_for_timers)
	/* We waited the full specified time, so return now.  */
	break;

      if (nfds == -1)
	{
	  /* If the system call was interrupted, then go around the
	     loop again.  */
	  if (xerrno == EINTR)
	    FD_ZERO (&waitchannels);
	  else
	    error ("select error: %s", emacs_strerror (xerrno));
	}
#ifdef sun
      else if (nfds > 0 && (waitchannels & 1)  && interrupt_input)
	/* System sometimes fails to deliver SIGIO.  */
	kill (getpid (), SIGIO);
#endif
#ifdef SIGIO
      if (XINT (read_kbd) && interrupt_input && (waitchannels & 1))
	kill (getpid (), SIGIO);
#endif

      /* Check for keyboard input */

      if ((XINT (read_kbd) != 0)
	  && detect_input_pending_run_timers (do_display))
	{
	  swallow_events (do_display);
	  if (detect_input_pending_run_timers (do_display))
	    break;
	}

      /* If there is unread keyboard input, also return.  */
      if (XINT (read_kbd) != 0
	  && requeued_events_pending_p ())
	break;

      /* If wait_for_cell. check for keyboard input
	 but don't run any timers.
	 ??? (It seems wrong to me to check for keyboard
	 input at all when wait_for_cell, but the code
	 has been this way since July 1994.
	 Try changing this after version 19.31.)  */
      if (! NILP (wait_for_cell)
	  && detect_input_pending ())
	{
	  swallow_events (do_display);
	  if (detect_input_pending ())
	    break;
	}

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;
    }

  start_polling ();

  return 0;
}


/* Don't confuse make-docfile by having two doc strings for this function.
   make-docfile does not pay attention to #if, for good reason!  */
DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
       0)
     (name)
     register Lisp_Object name;
{
  return Qnil;
}

  /* Don't confuse make-docfile by having two doc strings for this function.
     make-docfile does not pay attention to #if, for good reason!  */
DEFUN ("process-inherit-coding-system-flag",
       Fprocess_inherit_coding_system_flag, Sprocess_inherit_coding_system_flag,
       1, 1, 0,
       0)
     (process)
     register Lisp_Object process;
{
  /* Ignore the argument and return the value of
     inherit-process-coding-system.  */
  return inherit_process_coding_system ? Qt : Qnil;
}

/* Kill all processes associated with `buffer'.
   If `buffer' is nil, kill all processes.
   Since we have no subprocesses, this does nothing.  */

void
kill_buffer_processes (buffer)
     Lisp_Object buffer;
{
}

void
init_process ()
{
}

void
syms_of_process ()
{
  QCtype = intern (":type");
  staticpro (&QCtype);

  defsubr (&Sget_buffer_process);
  defsubr (&Sprocess_inherit_coding_system_flag);
}


#endif /* not subprocesses */
