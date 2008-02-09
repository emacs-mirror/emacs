/* Asynchronous subprocess control for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1993, 1994, 1995,
                 1996, 1998, 1999, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
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

/* Are local (unix) sockets supported?  */
#if defined (HAVE_SYS_UN_H)
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

#if defined(BSD_SYSTEM)
#include <sys/ioctl.h>
#if !defined (O_NDELAY) && defined (HAVE_PTYS) && !defined(USG5)
#include <fcntl.h>
#endif /* HAVE_PTYS and no O_NDELAY */
#endif /* BSD_SYSTEM */

#ifdef BROKEN_O_NONBLOCK
#undef O_NONBLOCK
#endif /* BROKEN_O_NONBLOCK */

#ifdef NEED_BSDTTY
#include <bsdtty.h>
#endif

/* Can we use SIOCGIFCONF and/or SIOCGIFADDR */
#ifdef HAVE_SOCKETS
#if defined(HAVE_SYS_IOCTL_H) && defined(HAVE_NET_IF_H)
/* sys/ioctl.h may have been included already */
#ifndef SIOCGIFADDR
#include <sys/ioctl.h>
#endif
#include <net/if.h>
#endif
#endif

#ifdef IRIS
#include <sys/sysmacros.h>	/* for "minor" */
#endif /* not IRIS */

#ifdef HAVE_SYS_WAIT
#include <sys/wait.h>
#endif

#ifdef HAVE_RES_INIT
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>
#endif

#include "lisp.h"
#include "systime.h"
#include "systty.h"

#include "window.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "process.h"
#include "frame.h"
#include "termhooks.h"
#include "termopts.h"
#include "commands.h"
#include "keyboard.h"
#include "blockinput.h"
#include "dispextern.h"
#include "composite.h"
#include "atimer.h"

Lisp_Object Qprocessp;
Lisp_Object Qrun, Qstop, Qsignal;
Lisp_Object Qopen, Qclosed, Qconnect, Qfailed, Qlisten;
Lisp_Object Qlocal, Qipv4, Qdatagram;
#ifdef AF_INET6
Lisp_Object Qipv6;
#endif
Lisp_Object QCname, QCbuffer, QChost, QCservice, QCtype;
Lisp_Object QClocal, QCremote, QCcoding;
Lisp_Object QCserver, QCnowait, QCnoquery, QCstop;
Lisp_Object QCsentinel, QClog, QCoptions, QCplist;
Lisp_Object QCfilter_multibyte;
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
   Qt nor Qnil but is instead a property list (KEY VAL ...).  */

#ifdef HAVE_SOCKETS
#define NETCONN_P(p) (CONSP (XPROCESS (p)->childp))
#define NETCONN1_P(p) (CONSP ((p)->childp))
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

extern char *get_operating_system_release ();

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

#if !defined (ADAPTIVE_READ_BUFFERING) && !defined (NO_ADAPTIVE_READ_BUFFERING)
#ifdef EMACS_HAS_USECS
#define ADAPTIVE_READ_BUFFERING
#endif
#endif

#ifdef ADAPTIVE_READ_BUFFERING
#define READ_OUTPUT_DELAY_INCREMENT 10000
#define READ_OUTPUT_DELAY_MAX       (READ_OUTPUT_DELAY_INCREMENT * 5)
#define READ_OUTPUT_DELAY_MAX_MAX   (READ_OUTPUT_DELAY_INCREMENT * 7)

/* Number of processes which have a non-zero read_output_delay,
   and therefore might be delayed for adaptive read buffering.  */

static int process_output_delay_count;

/* Non-zero if any process has non-nil read_output_skip.  */

static int process_output_skip;

/* Non-nil means to delay reading process output to improve buffering.
   A value of t means that delay is reset after each send, any other
   non-nil value does not reset the delay.  A value of nil disables
   adaptive read buffering completely.  */
static Lisp_Object Vprocess_adaptive_read_buffering;
#else
#define process_output_delay_count 0
#endif


#include "sysselect.h"

static int keyboard_bit_set P_ ((SELECT_TYPE *));
static void deactivate_process P_ ((Lisp_Object));
static void status_notify P_ ((struct Lisp_Process *));
static int read_process_output P_ ((Lisp_Object, int));

/* If we support a window system, turn on the code to poll periodically
   to detect C-g.  It isn't actually used when doing interrupt input.  */
#ifdef HAVE_WINDOW_SYSTEM
#define POLL_FOR_INPUT
#endif

static Lisp_Object get_process ();
static void exec_sentinel ();

extern EMACS_TIME timer_check ();
extern int timers_run;

/* Mask of bits indicating the descriptors that we wait for input on.  */

static SELECT_TYPE input_wait_mask;

/* Mask that excludes keyboard input descriptor(s).  */

static SELECT_TYPE non_keyboard_wait_mask;

/* Mask that excludes process input descriptor(s).  */

static SELECT_TYPE non_process_wait_mask;

/* Mask for the gpm mouse input descriptor.  */

static SELECT_TYPE gpm_wait_mask;

#ifdef NON_BLOCKING_CONNECT
/* Mask of bits indicating the descriptors that we wait for connect to
   complete on.  Once they complete, they are removed from this mask
   and added to the input_wait_mask and non_keyboard_wait_mask.  */

static SELECT_TYPE connect_wait_mask;

/* Number of bits set in connect_wait_mask.  */
static int num_pending_connects;

#define IF_NON_BLOCKING_CONNECT(s) s
#else
#define IF_NON_BLOCKING_CONNECT(s)
#endif

/* The largest descriptor currently in use for a process object.  */
static int max_process_desc;

/* The largest descriptor currently in use for keyboard input.  */
static int max_keyboard_desc;

/* The largest descriptor currently in use for gpm mouse input.  */
static int max_gpm_desc;

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
#define DATAGRAM_CONN_P(proc)	(PROCESSP (proc) && datagram_address[XPROCESS (proc)->infd].sa != 0)
#else
#define DATAGRAM_CHAN_P(chan)	(0)
#define DATAGRAM_CONN_P(proc)	(0)
#endif

/* Maximum number of bytes to send to a pty without an eof.  */
static int pty_max_bytes;

/* Nonzero means don't run process sentinels.  This is used
   when exiting.  */
int inhibit_sentinels;

#ifdef HAVE_PTYS
#ifdef HAVE_PTY_H
#include <pty.h>
#endif
/* The file name of the pty opened by allocate_pty.  */

static char pty_name[24];
#endif

/* Compute the Lisp form of the process status, p->status, from
   the numeric status that was returned by `wait'.  */

static Lisp_Object status_convert ();

static void
update_status (p)
     struct Lisp_Process *p;
{
  union { int i; WAITTYPE wt; } u;
  eassert (p->raw_status_new);
  u.i = p->raw_status;
  p->status = status_convert (u.wt);
  p->raw_status_new = 0;
}

/*  Convert a process status word in Unix format to
    the list that we use internally.  */

static Lisp_Object
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

static void
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

static Lisp_Object
status_message (p)
     struct Lisp_Process *p;
{
  Lisp_Object status = p->status;
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
      SSET (string, 0, DOWNCASE (SREF (string, 0)));
      return concat2 (string, string2);
    }
  else if (EQ (symbol, Qexit))
    {
      if (NETCONN1_P (p))
	return build_string (code == 0 ? "deleted\n" : "connection broken by remote peer\n");
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

static int
allocate_pty ()
{
  register int c, i;
  int fd;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
	struct stat stb;	/* Used in some PTY_OPEN.  */
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
	{
# ifdef IRIS
	  /* Unusual IRIS code */
	  *ptyv = emacs_open ("/dev/ptc", O_RDWR | O_NDELAY, 0);
	  if (fd < 0)
	    return -1;
	  if (fstat (fd, &stb) < 0)
	    return -1;
# else /* not IRIS */
	  { /* Some systems name their pseudoterminals so that there are gaps in
	       the usual sequence - for example, on HP9000/S700 systems, there
	       are no pseudoterminals with names ending in 'f'.  So we wait for
	       three failures in a row before deciding that we've reached the
	       end of the ptys.  */
	    int failed_count = 0;

	    if (stat (pty_name, &stb) < 0)
	      {
		failed_count++;
		if (failed_count >= 3)
		  return -1;
	      }
	    else
	      failed_count = 0;
	  }
#  ifdef O_NONBLOCK
	  fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#  else
	  fd = emacs_open (pty_name, O_RDWR | O_NDELAY, 0);
#  endif
# endif /* not IRIS */
	}
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
	    if (access (pty_name, 6) != 0)
	      {
		emacs_close (fd);
# if !defined(IRIS) && !defined(__sgi)
		continue;
# else
		return -1;
# endif /* IRIS */
	      }
	    setup_pty (fd);
	    return fd;
	  }
      }
  return -1;
}
#endif /* HAVE_PTYS */

static Lisp_Object
make_process (name)
     Lisp_Object name;
{
  register Lisp_Object val, tem, name1;
  register struct Lisp_Process *p;
  char suffix[10];
  register int i;

  p = allocate_process ();

  p->infd = -1;
  p->outfd = -1;
  p->tick = 0;
  p->update_tick = 0;
  p->pid = 0;
  p->pty_flag = 0;
  p->raw_status_new = 0;
  p->status = Qrun;
  p->mark = Fmake_marker ();

#ifdef ADAPTIVE_READ_BUFFERING
  p->adaptive_read_buffering = 0;
  p->read_output_delay = 0;
  p->read_output_skip = 0;
#endif

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

static void
remove_process (proc)
     register Lisp_Object proc;
{
  register Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);

  deactivate_process (proc);
}

/* Setup coding systems of PROCESS.  */

void
setup_process_coding_systems (process)
     Lisp_Object process;
{
  struct Lisp_Process *p = XPROCESS (process);
  int inch = p->infd;
  int outch = p->outfd;
  Lisp_Object coding_system;

  if (inch < 0 || outch < 0)
    return;

  if (!proc_decode_coding_system[inch])
    proc_decode_coding_system[inch]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  coding_system = p->decode_coding_system;
  if (! NILP (p->filter))
    {
      if (!p->filter_multibyte)
	coding_system = raw_text_coding_system (coding_system);
    }
  else if (BUFFERP (p->buffer))
    {
      if (NILP (XBUFFER (p->buffer)->enable_multibyte_characters))
	coding_system = raw_text_coding_system (coding_system);
    }
  setup_coding_system (coding_system, proc_decode_coding_system[inch]);

  if (!proc_encode_coding_system[outch])
    proc_encode_coding_system[outch]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->encode_coding_system,
		       proc_encode_coding_system[outch]);
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

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      proc = Fcdr (XCAR (tail));
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
	error ("Process %s does not exist", SDATA (name));
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
	error ("Buffer %s has no process", SDATA (XBUFFER (obj)->name));
    }
  else
    {
      CHECK_PROCESS (obj);
      proc = obj;
    }
  return proc;
}


#ifdef SIGCHLD
/* Fdelete_process promises to immediately forget about the process, but in
   reality, Emacs needs to remember those processes until they have been
   treated by sigchld_handler; otherwise this handler would consider the
   process as being synchronous and say that the synchronous process is
   dead.  */
static Lisp_Object deleted_pid_list;
#endif

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
       doc: /* Delete PROCESS: kill it and forget about it immediately.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.  */)
     (process)
     register Lisp_Object process;
{
  register struct Lisp_Process *p;

  process = get_process (process);
  p = XPROCESS (process);

  p->raw_status_new = 0;
  if (NETCONN1_P (p))
    {
      p->status = Fcons (Qexit, Fcons (make_number (0), Qnil));
      p->tick = ++process_tick;
      status_notify (p);
    }
  else if (p->infd >= 0)
    {
#ifdef SIGCHLD
      Lisp_Object symbol;
      /* Assignment to EMACS_INT stops GCC whining about limited range
	 of data type.  */
      EMACS_INT pid = p->pid;

      /* No problem storing the pid here, as it is still in Vprocess_alist.  */
      deleted_pid_list = Fcons (make_fixnum_or_float (pid),
				/* GC treated elements set to nil.  */
				Fdelq (Qnil, deleted_pid_list));
      /* If the process has already signaled, remove it from the list.  */
      if (p->raw_status_new)
	update_status (p);
      symbol = p->status;
      if (CONSP (p->status))
	symbol = XCAR (p->status);
      if (EQ (symbol, Qsignal) || EQ (symbol, Qexit))
	deleted_pid_list
	  = Fdelete (make_fixnum_or_float (pid), deleted_pid_list);
      else
#endif
	{
	  Fkill_process (process, Qnil);
	  /* Do this now, since remove_process will make sigchld_handler do nothing.  */
	  p->status
	    = Fcons (Qsignal, Fcons (make_number (SIGKILL), Qnil));
	  p->tick = ++process_tick;
	  status_notify (p);
	}
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
  if (p->raw_status_new)
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
  if (XPROCESS (process)->raw_status_new)
    update_status (XPROCESS (process));
  if (CONSP (XPROCESS (process)->status))
    return XCAR (XCDR (XPROCESS (process)->status));
  return make_number (0);
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
       doc: /* Return the process id of PROCESS.
This is the pid of the external process which PROCESS uses or talks to.
For a network connection, this value is nil.  */)
     (process)
     register Lisp_Object process;
{
  /* Assignment to EMACS_INT stops GCC whining about limited range of
     data type.  */
  EMACS_INT pid;

  CHECK_PROCESS (process);
  pid = XPROCESS (process)->pid;
  return (pid ? make_fixnum_or_float (pid) : Qnil);
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
  setup_process_coding_systems (process);
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
A value of t means stop accepting output from the process.

When a process has a filter, its buffer is not used for output.
Instead, each time it does output, the entire string of output is
passed to the filter.

The filter gets two arguments: the process and the string of output.
The string argument is normally a multibyte string, except:
- if the process' input coding system is no-conversion or raw-text,
  it is a unibyte string (the non-converted input), or else
- if `default-enable-multibyte-characters' is nil, it is a unibyte
  string (the result of converting the decoded input multibyte
  string to unibyte with `string-make-unibyte').  */)
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

  if (p->infd >= 0)
    {
      if (EQ (filter, Qt) && !EQ (p->status, Qlisten))
	{
	  FD_CLR (p->infd, &input_wait_mask);
	  FD_CLR (p->infd, &non_keyboard_wait_mask);
	}
      else if (EQ (p->filter, Qt)
	       && !EQ (p->command, Qt)) /* Network process not stopped. */
	{
	  FD_SET (p->infd, &input_wait_mask);
	  FD_SET (p->infd, &non_keyboard_wait_mask);
	}
    }

  p->filter = filter;
  if (NETCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCfilter, filter);
  setup_process_coding_systems (process);
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
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);

  p->sentinel = sentinel;
  if (NETCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCsentinel, sentinel);
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

  if (XPROCESS (process)->infd < 0
      || set_window_size (XPROCESS (process)->infd,
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
  XPROCESS (process)->inherit_coding_system_flag = !NILP (flag);
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
  return XPROCESS (process)->inherit_coding_system_flag ? Qt : Qnil;
}

DEFUN ("set-process-query-on-exit-flag",
       Fset_process_query_on_exit_flag, Sset_process_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for PROCESS when Emacs is exited.
If the second argument FLAG is non-nil, Emacs will query the user before
exiting if PROCESS is running.  */)
     (process, flag)
     register Lisp_Object process, flag;
{
  CHECK_PROCESS (process);
  XPROCESS (process)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("process-query-on-exit-flag",
       Fprocess_query_on_exit_flag, Sprocess_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of query-on-exit flag for PROCESS.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return (XPROCESS (process)->kill_without_query ? Qnil : Qt);
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

DEFUN ("process-plist", Fprocess_plist, Sprocess_plist,
       1, 1, 0,
       doc: /* Return the plist of PROCESS.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->plist;
}

DEFUN ("set-process-plist", Fset_process_plist, Sset_process_plist,
       2, 2, 0,
       doc: /* Replace the plist of PROCESS with PLIST.  Returns PLIST.  */)
     (process, plist)
     register Lisp_Object process, plist;
{
  CHECK_PROCESS (process);
  CHECK_LIST (plist);

  XPROCESS (process)->plist = plist;
  return plist;
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

#ifdef HAVE_SOCKETS
DEFUN ("format-network-address", Fformat_network_address, Sformat_network_address,
       1, 2, 0,
       doc: /* Convert network ADDRESS from internal format to a string.
A 4 or 5 element vector represents an IPv4 address (with port number).
An 8 or 9 element vector represents an IPv6 address (with port number).
If optional second argument OMIT-PORT is non-nil, don't include a port
number in the string, even when present in ADDRESS.
Returns nil if format of ADDRESS is invalid.  */)
     (address, omit_port)
     Lisp_Object address, omit_port;
{
  if (NILP (address))
    return Qnil;

  if (STRINGP (address))  /* AF_LOCAL */
    return address;

  if (VECTORP (address))  /* AF_INET or AF_INET6 */
    {
      register struct Lisp_Vector *p = XVECTOR (address);
      Lisp_Object args[10];
      int nargs, i;

      if (p->size == 4 || (p->size == 5 && !NILP (omit_port)))
	{
	  args[0] = build_string ("%d.%d.%d.%d");
	  nargs = 4;
	}
      else if (p->size == 5)
	{
	  args[0] = build_string ("%d.%d.%d.%d:%d");
	  nargs = 5;
	}
      else if (p->size == 8 || (p->size == 9 && !NILP (omit_port)))
	{
	  args[0] = build_string ("%x:%x:%x:%x:%x:%x:%x:%x");
	  nargs = 8;
	}
      else if (p->size == 9)
	{
	  args[0] = build_string ("[%x:%x:%x:%x:%x:%x:%x:%x]:%d");
	  nargs = 9;
	}
      else
	return Qnil;

      for (i = 0; i < nargs; i++)
	{
	  EMACS_INT element = XINT (p->contents[i]);

	  if (element < 0 || element > 65535)
	    return Qnil;

	  if (nargs <= 5         /* IPv4 */
	      && i < 4           /* host, not port */
	      && element > 255)
	    return Qnil;

	  args[i+1] = p->contents[i];
	}

      return Fformat (nargs+1, args);
    }

  if (CONSP (address))
    {
      Lisp_Object args[2];
      args[0] = build_string ("<Family %d>");
      args[1] = Fcar (address);
      return Fformat (2, args);
    }

  return Qnil;
}
#endif

static Lisp_Object
list_processes_1 (query_only)
     Lisp_Object query_only;
{
  register Lisp_Object tail, tem;
  Lisp_Object proc, minspace, tem1;
  register struct Lisp_Process *p;
  char tembuf[300];
  int w_proc, w_buffer, w_tty;
  int exited = 0;
  Lisp_Object i_status, i_buffer, i_tty, i_command;

  w_proc = 4;    /* Proc   */
  w_buffer = 6;  /* Buffer */
  w_tty = 0;     /* Omit if no ttys */

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      int i;

      proc = Fcdr (XCAR (tail));
      p = XPROCESS (proc);
      if (NILP (p->childp))
	continue;
      if (!NILP (query_only) && p->kill_without_query)
	continue;
      if (STRINGP (p->name)
	  && ( i = SCHARS (p->name), (i > w_proc)))
	w_proc = i;
      if (!NILP (p->buffer))
	{
	  if (NILP (XBUFFER (p->buffer)->name))
	    {
	      if (w_buffer < 8)
		w_buffer = 8;  /* (Killed) */
	    }
	  else if ((i = SCHARS (XBUFFER (p->buffer)->name), (i > w_buffer)))
	    w_buffer = i;
	}
      if (STRINGP (p->tty_name)
	  && (i = SCHARS (p->tty_name), (i > w_tty)))
	w_tty = i;
    }

  XSETFASTINT (i_status, w_proc + 1);
  XSETFASTINT (i_buffer, XFASTINT (i_status) + 9);
  if (w_tty)
    {
      XSETFASTINT (i_tty, XFASTINT (i_buffer) + w_buffer + 1);
      XSETFASTINT (i_command, XFASTINT (i_tty) + w_tty + 1);
    }
  else
    {
      i_tty = Qnil;
      XSETFASTINT (i_command, XFASTINT (i_buffer) + w_buffer + 1);
    }

  XSETFASTINT (minspace, 1);

  set_buffer_internal (XBUFFER (Vstandard_output));
  current_buffer->undo_list = Qt;

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

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object symbol;

      proc = Fcdr (XCAR (tail));
      p = XPROCESS (proc);
      if (NILP (p->childp))
	continue;
      if (!NILP (query_only) && p->kill_without_query)
	continue;

      Finsert (1, &p->name);
      Findent_to (i_status, minspace);

      if (p->raw_status_new)
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

      if (EQ (symbol, Qsignal) || EQ (symbol, Qexit) || EQ (symbol, Qclosed))
	exited++;

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
	  if (NILP (port))
	    port = Fformat_network_address (Fplist_get (p->childp, QClocal), Qnil);
	  sprintf (tembuf, "(network %s server on %s)\n",
		   (DATAGRAM_CHAN_P (p->infd) ? "datagram" : "stream"),
		   (STRINGP (port) ? (char *)SDATA (port) : "?"));
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
	  if (NILP (host))
	    host = Fformat_network_address (Fplist_get (p->childp, QCremote), Qnil);
	  sprintf (tembuf, "(network %s connection to %s)\n",
		   (DATAGRAM_CHAN_P (p->infd) ? "datagram" : "stream"),
		   (STRINGP (host) ? (char *)SDATA (host) : "?"));
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
  if (exited)
    status_notify (NULL);
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
BUFFER is the buffer (or buffer name) to associate with the process.

Process output (both standard output and standard error streams) goes
at end of BUFFER, unless you specify an output stream or filter
function to handle the output.  BUFFER may also be nil, meaning that
this process is not associated with any buffer.

PROGRAM is the program file name.  It is searched for in PATH.
Remaining arguments are strings to give program as arguments.

If you want to separate standard output from standard error, invoke
the command through a shell and redirect one of them using the shell
syntax.

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
  int count = SPECPDL_INDEX ();

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

    current_dir = Funhandled_file_name_directory (current_dir);
    if (NILP (current_dir))
      /* If the file name handler says that current_dir is unreachable, use
	 a sensible default. */
      current_dir = build_string ("~/");
    current_dir = expand_and_dir_to_file (current_dir, Qnil);
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
  XPROCESS (proc)->plist = Qnil;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
  XPROCESS (proc)->filter_multibyte
    = !NILP (buffer_defaults.enable_multibyte_characters);
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

#ifdef ADAPTIVE_READ_BUFFERING
  XPROCESS (proc)->adaptive_read_buffering
    = (NILP (Vprocess_adaptive_read_buffering) ? 0
       : EQ (Vprocess_adaptive_read_buffering, Qt) ? 1 : 2);
#endif

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (buffer))
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
  len = SBYTES (program) + 2;
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      len += SBYTES (tem) + 1;	/* count the blank */
    }
  new_argv = (unsigned char *) alloca (len);
  strcpy (new_argv, SDATA (program));
  for (i = 3; i < nargs; i++)
    {
      tem = args[i];
      CHECK_STRING (tem);
      strcat (new_argv, " ");
      strcat (new_argv, SDATA (tem));
    }
  /* Need to add code here to check for program existence on VMS */

#else /* not VMS */
  new_argv = (unsigned char **) alloca ((nargs - 1) * sizeof (char *));

  /* If program file name is not absolute, search our path for it.
     Put the name we will really use in TEM.  */
  if (!IS_DIRECTORY_SEP (SREF (program, 0))
      && !(SCHARS (program) > 1
	   && IS_DEVICE_SEP (SREF (program, 1))))
    {
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

      tem = Qnil;
      GCPRO4 (name, program, buffer, current_dir);
      openp (Vexec_path, program, Vexec_suffixes, &tem, make_number (X_OK));
      UNGCPRO;
      if (NILP (tem))
	report_file_error ("Searching for program", Fcons (program, Qnil));
      tem = Fexpand_file_name (tem, Qnil);
    }
  else
    {
      if (!NILP (Ffile_directory_p (program)))
	error ("Specified program for new process is a directory");
      tem = program;
    }

  /* If program file name starts with /: for quoting a magic name,
     discard that.  */
  if (SBYTES (tem) > 2 && SREF (tem, 0) == '/'
      && SREF (tem, 1) == ':')
    tem = Fsubstring (tem, make_number (2), Qnil);

  /* Encode the file name and put it in NEW_ARGV.
     That's where the child will use it to execute the program.  */
  tem = ENCODE_FILE (tem);
  new_argv[0] = SDATA (tem);

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
      new_argv[i - 2] = SDATA (tem);
    }
  new_argv[i - 2] = 0;
#endif /* not VMS */

  XPROCESS (proc)->decoding_buf = make_uninit_string (0);
  XPROCESS (proc)->decoding_carryover = 0;
  XPROCESS (proc)->encoding_buf = make_uninit_string (0);

  XPROCESS (proc)->inherit_coding_system_flag
    = !(NILP (buffer) || !inherit_process_coding_system);

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
  if (XPROCESS (proc)->pid <= 0)
    remove_process (proc);

  return Qnil;
}

static void
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
  int inchannel, outchannel;
  pid_t pid;
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
#if ! defined (USG) || defined (USG_SUBTTY_WORKS)
      /* On most USG systems it does not work to open the pty's tty here,
	 then close it and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      forkout = forkin = emacs_open (pty_name, O_RDWR, 0);
#endif
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
#if defined (DONT_REOPEN_PTY)
      /* In the case that vfork is defined as fork, the parent process
	 (Emacs) may send some data before the child process completes
	 tty options setup.  So we setup tty before forking.  */
      child_setup_tty (forkout);
#endif /* DONT_REOPEN_PTY */
#else
      forkin = forkout = -1;
#endif /* not USG, or USG_SUBTTY_WORKS */
      pty_flag = 1;
    }
  else
#endif /* HAVE_PTYS */
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

#if 0
  /* Replaced by close_process_descs */
  set_exclusive_use (inchannel);
  set_exclusive_use (outchannel);
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
  XPROCESS (process)->infd = inchannel;
  XPROCESS (process)->outfd = outchannel;

  /* Previously we recorded the tty descriptor used in the subprocess.
     It was only used for getting the foreground tty process, so now
     we just reopen the device (see emacs_get_tty_pgrp) as this is
     more portable (see USG_SUBTTY_WORKS above).  */

  XPROCESS (process)->pty_flag = pty_flag;
  XPROCESS (process)->status = Qrun;
  setup_process_coding_systems (process);

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
#if defined (BSD_SYSTEM) || defined (HPUX)
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
  XPROCESS (process)->pid = -1;

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

#if !defined (DONT_REOPEN_PTY)
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
#endif /* not DONT_REOPEN_PTY */

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
#if defined (BSD_SYSTEM) || defined (HPUX)
	sigsetmask (SIGEMPTYMASK);
#else /* ordinary USG */
#if 0
	signal (SIGCHLD, sigchld);
#endif
#endif /* ordinary USG */
#endif /* not BSD4_1 */
#endif /* SIGCHLD */
#endif /* !POSIX_SIGNALS */

#if !defined (DONT_REOPEN_PTY)
	if (pty_flag)
	  child_setup_tty (xforkout);
#endif /* not DONT_REOPEN_PTY */
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
      XPROCESS (process)->pid = pid;

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
#if defined (BSD_SYSTEM) || defined (HPUX)
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
#ifdef AF_INET6
    case AF_INET6:
      {
	struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) sa;
	uint16_t *ip6 = (uint16_t *)&sin6->sin6_addr;
	len = sizeof (sin6->sin6_addr)/2 + 1;
	address = Fmake_vector (make_number (len), Qnil);
	p = XVECTOR (address);
	p->contents[--len] = make_number (ntohs (sin6->sin6_port));
	for (i = 0; i < len; i++)
	  p->contents[i] = make_number (ntohs (ip6[i]));
	return address;
      }
#endif
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
#ifdef AF_INET6
      else if (p->size == 9)
	{
	  *familyp = AF_INET6;
	  return sizeof (struct sockaddr_in6);
	}
#endif
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

   The address format has been basically validated by
   get_lisp_to_sockaddr_size, but this does not mean FAMILY is valid;
   it could have come from user data.  So if FAMILY is not valid,
   we return after zeroing *SA.  */

static void
conv_lisp_to_sockaddr (family, address, sa, len)
     int family;
     Lisp_Object address;
     struct sockaddr *sa;
     int len;
{
  register struct Lisp_Vector *p;
  register unsigned char *cp = NULL;
  register int i;

  bzero (sa, len);

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
	  sa->sa_family = family;
	}
#ifdef AF_INET6
      else if (family == AF_INET6)
	{
	  struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) sa;
	  uint16_t *ip6 = (uint16_t *)&sin6->sin6_addr;
	  len = sizeof (sin6->sin6_addr) + 1;
	  i = XINT (p->contents[--len]);
	  sin6->sin6_port = htons (i);
	  for (i = 0; i < len; i++)
	    if (INTEGERP (p->contents[i]))
	      {
		int j = XFASTINT (p->contents[i]) & 0xffff;
		ip6[i] = ntohs (j);
	      }
	  sa->sa_family = family;
	}
#endif
      return;
    }
  else if (STRINGP (address))
    {
#ifdef HAVE_LOCAL_SOCKETS
      if (family == AF_LOCAL)
	{
	  struct sockaddr_un *sockun = (struct sockaddr_un *) sa;
	  cp = SDATA (address);
	  for (i = 0; i < sizeof (sockun->sun_path) && *cp; i++)
	    sockun->sun_path[i] = *cp++;
	  sa->sa_family = family;
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

  channel = XPROCESS (process)->infd;
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

  channel = XPROCESS (process)->infd;

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
  /* Option level SOL_... */
  int optlevel;
  /* Option number SO_... */
  int optnum;
  enum { SOPT_UNKNOWN, SOPT_BOOL, SOPT_INT, SOPT_IFNAME, SOPT_LINGER } opttype;
  enum { OPIX_NONE=0, OPIX_MISC=1, OPIX_REUSEADDR=2 } optbit;
} socket_options[] =
  {
#ifdef SO_BINDTODEVICE
    { ":bindtodevice", SOL_SOCKET, SO_BINDTODEVICE, SOPT_IFNAME, OPIX_MISC },
#endif
#ifdef SO_BROADCAST
    { ":broadcast", SOL_SOCKET, SO_BROADCAST, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_DONTROUTE
    { ":dontroute", SOL_SOCKET, SO_DONTROUTE, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_KEEPALIVE
    { ":keepalive", SOL_SOCKET, SO_KEEPALIVE, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_LINGER
    { ":linger", SOL_SOCKET, SO_LINGER, SOPT_LINGER, OPIX_MISC },
#endif
#ifdef SO_OOBINLINE
    { ":oobinline", SOL_SOCKET, SO_OOBINLINE, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_PRIORITY
    { ":priority", SOL_SOCKET, SO_PRIORITY, SOPT_INT, OPIX_MISC },
#endif
#ifdef SO_REUSEADDR
    { ":reuseaddr", SOL_SOCKET, SO_REUSEADDR, SOPT_BOOL, OPIX_REUSEADDR },
#endif
    { 0, 0, 0, SOPT_UNKNOWN, OPIX_NONE }
  };

/* Set option OPT to value VAL on socket S.

   Returns (1<<socket_options[OPT].optbit) if option is known, 0 otherwise.
   Signals an error if setting a known option fails.
*/

static int
set_socket_option (s, opt, val)
     int s;
     Lisp_Object opt, val;
{
  char *name;
  struct socket_options *sopt;
  int ret = 0;

  CHECK_SYMBOL (opt);

  name = (char *) SDATA (SYMBOL_NAME (opt));
  for (sopt = socket_options; sopt->name; sopt++)
    if (strcmp (name, sopt->name) == 0)
      break;

  switch (sopt->opttype)
    {
    case SOPT_BOOL:
      {
	int optval;
	optval = NILP (val) ? 0 : 1;
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  &optval, sizeof (optval));
	break;
      }

    case SOPT_INT:
      {
	int optval;
	if (INTEGERP (val))
	  optval = XINT (val);
	else
	  error ("Bad option value for %s", name);
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  &optval, sizeof (optval));
	break;
      }

#ifdef SO_BINDTODEVICE
    case SOPT_IFNAME:
      {
	char devname[IFNAMSIZ+1];

	/* This is broken, at least in the Linux 2.4 kernel.
	   To unbind, the arg must be a zero integer, not the empty string.
	   This should work on all systems.   KFS. 2003-09-23.  */
	bzero (devname, sizeof devname);
	if (STRINGP (val))
	  {
	    char *arg = (char *) SDATA (val);
	    int len = min (strlen (arg), IFNAMSIZ);
	    bcopy (arg, devname, len);
	  }
	else if (!NILP (val))
	  error ("Bad option value for %s", name);
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  devname, IFNAMSIZ);
	break;
      }
#endif

#ifdef SO_LINGER
    case SOPT_LINGER:
      {
	struct linger linger;

	linger.l_onoff = 1;
	linger.l_linger = 0;
	if (INTEGERP (val))
	  linger.l_linger = XINT (val);
	else
	  linger.l_onoff = NILP (val) ? 0 : 1;
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  &linger, sizeof (linger));
	break;
      }
#endif

    default:
      return 0;
    }

  if (ret < 0)
    report_file_error ("Cannot set network option",
		       Fcons (opt, Fcons (val, Qnil)));
  return (1 << sopt->optbit);
}


DEFUN ("set-network-process-option",
       Fset_network_process_option, Sset_network_process_option,
       3, 4, 0,
       doc: /* For network process PROCESS set option OPTION to value VALUE.
See `make-network-process' for a list of options and values.
If optional fourth arg NO-ERROR is non-nil, don't signal an error if
OPTION is not a supported option, return nil instead; otherwise return t.  */)
     (process, option, value, no_error)
     Lisp_Object process, option, value;
     Lisp_Object no_error;
{
  int s;
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  if (!NETCONN1_P (p))
    error ("Process is not a network process");

  s = p->infd;
  if (s < 0)
    error ("Process is not running");

  if (set_socket_option (s, option, value))
    {
      p->childp = Fplist_put (p->childp, option, value);
      return Qt;
    }

  if (NILP (no_error))
    error ("Unknown or unsupported option");

  return Qnil;
}


/* A version of request_sigio suitable for a record_unwind_protect.  */

#ifdef __ultrix__
static Lisp_Object
unwind_request_sigio (dummy)
     Lisp_Object dummy;
{
  if (interrupt_input)
    request_sigio ();
  return Qnil;
}
#endif

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
it cannot be signaled, and the status codes are different from normal
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
a random port number is selected for the server.  (If Emacs was
compiled with getaddrinfo, a port number can also be specified as a
string, e.g. "80", as well as an integer.  This is not portable.)

:type TYPE -- TYPE is the type of connection.  The default (nil) is a
stream type connection, `datagram' creates a datagram type connection.

:family FAMILY -- FAMILY is the address (and protocol) family for the
service specified by HOST and SERVICE.  The default (nil) is to use
whatever address family (IPv4 or IPv6) that is defined for the host
and port number specified by HOST and SERVICE.  Other address families
supported are:
  local -- for a local (i.e. UNIX) address specified by SERVICE.
  ipv4  -- use IPv4 address family only.
  ipv6  -- use IPv6 address family only.

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

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:nowait BOOL -- If BOOL is non-nil for a stream type client process,
return without waiting for the connection to complete; instead, the
sentinel function will be called with second arg matching "open" (if
successful) or "failed" when the connect completes.  Default is to use
a blocking connect (i.e. wait) for stream type connections.

:noquery BOOL -- Query the user unless BOOL is non-nil, and process is
running when Emacs is exited.

:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.
In the stopped state, a server process does not accept new
connections, and a client process does not handle incoming traffic.
The stopped state is cleared by `continue-process' and set by
`stop-process'.

:filter FILTER -- Install FILTER as the process filter.

:filter-multibyte BOOL -- If BOOL is non-nil, strings given to the
process filter are multibyte, otherwise they are unibyte.
If this keyword is not specified, the strings are multibyte if
`default-enable-multibyte-characters' is non-nil.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:log LOG -- Install LOG as the server process log function.  This
function is called when the server accepts a network connection from a
client.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER
is the server process, CLIENT is the new process for the connection,
and MESSAGE is a string.

:plist PLIST -- Install PLIST as the new process' initial plist.

:server QLEN -- if QLEN is non-nil, create a server process for the
specified FAMILY, SERVICE, and connection type (stream or datagram).
If QLEN is an integer, it is used as the max. length of the server's
pending connection queue (also known as the backlog); the default
queue length is 5.  Default is to create a client process.

The following network options can be specified for this connection:

:broadcast BOOL    -- Allow send and receive of datagram broadcasts.
:dontroute BOOL    -- Only send to directly connected hosts.
:keepalive BOOL    -- Send keep-alive messages on network stream.
:linger BOOL or TIMEOUT -- Send queued messages before closing.
:oobinline BOOL    -- Place out-of-band data in receive data stream.
:priority INT      -- Set protocol defined priority for sent packets.
:reuseaddr BOOL    -- Allow reusing a recently used local address
                      (this is allowed by default for a server process).
:bindtodevice NAME -- bind to interface NAME.  Using this may require
                      special privileges on some systems.

Consult the relevant system programmer's manual pages for more
information on using these options.


A server process will listen for and accept connections from clients.
When a client connection is accepted, a new network process is created
for the connection with the following parameters:

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
- The client process' plist is initialized from the server's plist.

Notice that the FILTER and SENTINEL args are never used directly by
the server process.  Also, the BUFFER argument is not used directly by
the server process, but via the optional :log function, accepted (and
failed) connections may be logged in the server process' buffer.

The original argument list, modified with the actual connection
information, is available via the `process-contact' function.

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
  int count = SPECPDL_INDEX ();
  int count1;
  Lisp_Object QCaddress;  /* one of QClocal or QCremote */
  Lisp_Object tem;
  Lisp_Object name, buffer, host, service, address;
  Lisp_Object filter, sentinel;
  int is_non_blocking_client = 0;
  int is_server = 0, backlog = 5;
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
      /* Don't support network sockets when non-blocking mode is
	 not available, since a blocked Emacs is not useful.  */
#if defined(TERM) || (!defined(O_NONBLOCK) && !defined(O_NDELAY))
      error ("Network servers not supported");
#else
      is_server = 1;
      if (INTEGERP (tem))
	backlog = XINT (tem);
#endif
    }

  /* Make QCaddress an alias for :local (server) or :remote (client).  */
  QCaddress = is_server ? QClocal : QCremote;

  /* :nowait BOOL */
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
      svc_info = getservbyname (SDATA (service), "tcp");
      if (svc_info == 0)
	error ("Unknown service: %s", SDATA (service));
      port = svc_info->s_port;
    }

  s = connect_server (0);
  if (s < 0)
    report_file_error ("error creating socket", Fcons (name, Qnil));
  send_command (s, C_PORT, 0, "%s:%d", SDATA (host), ntohs (port));
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
  if (NILP (tem))
    {
#if defined(HAVE_GETADDRINFO) && defined(AF_INET6)
      family = AF_UNSPEC;
#else
      family = AF_INET;
#endif
    }
#ifdef HAVE_LOCAL_SOCKETS
  else if (EQ (tem, Qlocal))
    family = AF_LOCAL;
#endif
#ifdef AF_INET6
  else if (EQ (tem, Qipv6))
    family = AF_INET6;
#endif
  else if (EQ (tem, Qipv4))
    family = AF_INET;
  else if (INTEGERP (tem))
    family = XINT (tem);
  else
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
      strncpy (address_un.sun_path, SDATA (service), sizeof address_un.sun_path);
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
	  portstring = SDATA (service);
	}

      immediate_quit = 1;
      QUIT;
      memset (&hints, 0, sizeof (hints));
      hints.ai_flags = 0;
      hints.ai_family = family;
      hints.ai_socktype = socktype;
      hints.ai_protocol = 0;

#ifdef HAVE_RES_INIT
      res_init ();
#endif

      ret = getaddrinfo (SDATA (host), portstring, &hints, &res);
      if (ret)
#ifdef HAVE_GAI_STRERROR
	error ("%s/%s %s", SDATA (host), portstring, gai_strerror(ret));
#else
        error ("%s/%s getaddrinfo error %d", SDATA (host), portstring, ret);
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
      svc_info = getservbyname (SDATA (service),
				(socktype == SOCK_DGRAM ? "udp" : "tcp"));
      if (svc_info == 0)
	error ("Unknown service: %s", SDATA (service));
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
	 as it may `hang' Emacs for a very long time.  */
      immediate_quit = 1;
      QUIT;

#ifdef HAVE_RES_INIT
      res_init ();
#endif

      host_info_ptr = gethostbyname (SDATA (host));
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
	  unsigned long numeric_addr;
	  numeric_addr = inet_addr ((char *) SDATA (host));
	  if (numeric_addr == -1)
	    error ("Unknown host \"%s\"", SDATA (host));

	  bcopy ((char *)&numeric_addr, (char *) &address_in.sin_addr,
		 sizeof (address_in.sin_addr));
	}

    }
#endif /* not HAVE_GETADDRINFO */

  ai.ai_family = family;
  ai.ai_addr = (struct sockaddr *) &address_in;
  ai.ai_addrlen = sizeof address_in;

 open_socket:

#ifdef __ultrix__
  /* Previously this was compiled unconditionally, but that seems
     unnecessary on modern systems, and `unrequest_sigio' was a noop
     under X anyway. --lorentey */
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
#endif

  /* Do this in case we never enter the for-loop below.  */
  count1 = SPECPDL_INDEX ();
  s = -1;

  for (lres = res; lres; lres = lres->ai_next)
    {
      int optn, optbits;

    retry_connect:

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

      /* Parse network options in the arg list.
	 We simply ignore anything which isn't a known option (including other keywords).
         An error is signalled if setting a known option fails.  */
      for (optn = optbits = 0; optn < nargs-1; optn += 2)
	optbits |= set_socket_option (s, args[optn], args[optn+1]);

      if (is_server)
	{
	  /* Configure as a server socket.  */

	  /* SO_REUSEADDR = 1 is default for server sockets; must specify
	     explicit :reuseaddr key to override this.  */
#ifdef HAVE_LOCAL_SOCKETS
	  if (family != AF_LOCAL)
#endif
	    if (!(optbits & (1 << OPIX_REUSEADDR)))
	      {
		int optval = 1;
		if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval))
		  report_file_error ("Cannot set reuse option on server socket", Qnil);
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
		  service = make_number (ntohs (sa1.sin_port));
		  contact = Fplist_put (contact, QCservice, service);
		}
	    }
#endif

	  if (socktype == SOCK_STREAM && listen (s, backlog))
	    report_file_error ("Cannot listen on server socket", Qnil);

	  break;
	}

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

      /* Discard the unwind protect closing S.  */
      specpdl_ptr = specpdl + count1;
      emacs_close (s);
      s = -1;

      if (xerrno == EINTR)
	goto retry_connect;
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
#ifdef HAVE_GETSOCKNAME
      if (!is_server)
	{
	  struct sockaddr_in sa1;
	  int len1 = sizeof (sa1);
	  if (getsockname (s, (struct sockaddr *)&sa1, &len1) == 0)
	    contact = Fplist_put (contact, QClocal,
				  conv_sockaddr_to_lisp (&sa1, len1));
	}
#endif
    }

  immediate_quit = 0;

#ifdef HAVE_GETADDRINFO
  if (res != &ai)
    {
      BLOCK_INPUT;
      freeaddrinfo (res);
      UNBLOCK_INPUT;
    }
#endif

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
  p->plist = Fcopy_sequence (Fplist_get (contact, QCplist));

  p->buffer = buffer;
  p->sentinel = sentinel;
  p->filter = filter;
  p->filter_multibyte = !NILP (buffer_defaults.enable_multibyte_characters);
  /* Override the above only if :filter-multibyte is specified.  */
  if (! NILP (Fplist_member (contact, QCfilter_multibyte)))
    p->filter_multibyte = !NILP (Fplist_get (contact, QCfilter_multibyte));
  p->log = Fplist_get (contact, QClog);
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = 1;
  if ((tem = Fplist_get (contact, QCstop), !NILP (tem)))
    p->command = Qt;
  p->pid = 0;
  p->infd  = inch;
  p->outfd = outch;
  if (is_server && socktype == SOCK_STREAM)
    p->status = Qlisten;

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (buffer))
    set_marker_both (p->mark, buffer,
		     BUF_ZV (XBUFFER (buffer)),
		     BUF_ZV_BYTE (XBUFFER (buffer)));

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
      {
	val = XCAR (XCDR (tem));
	if (CONSP (val))
	  val = XCAR (val);
      }
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
	if (NILP (host) || NILP (service))
	  coding_systems = Qnil;
	else
	  {
	    args[0] = Qopen_network_stream, args[1] = name,
	      args[2] = buffer, args[3] = host, args[4] = service;
	    GCPRO1 (proc);
	    coding_systems = Ffind_operation_coding_system (5, args);
	    UNGCPRO;
	  }
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    p->decode_coding_system = val;

    if (!NILP (tem))
      {
	val = XCAR (XCDR (tem));
	if (CONSP (val))
	  val = XCDR (val);
      }
    else if (!NILP (Vcoding_system_for_write))
      val = Vcoding_system_for_write;
    else if (NILP (current_buffer->enable_multibyte_characters))
      val = Qnil;
    else
      {
	if (EQ (coding_systems, Qt))
	  {
	    if (NILP (host) || NILP (service))
	      coding_systems = Qnil;
	    else
	      {
		args[0] = Qopen_network_stream, args[1] = name,
		  args[2] = buffer, args[3] = host, args[4] = service;
		GCPRO1 (proc);
		coding_systems = Ffind_operation_coding_system (5, args);
		UNGCPRO;
	      }
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
  setup_process_coding_systems (proc);

  p->decoding_buf = make_uninit_string (0);
  p->decoding_carryover = 0;
  p->encoding_buf = make_uninit_string (0);

  p->inherit_coding_system_flag
    = !(!NILP (tem) || NILP (buffer) || !inherit_process_coding_system);

  UNGCPRO;
  return proc;
}
#endif	/* HAVE_SOCKETS */


#if defined(HAVE_SOCKETS) && defined(HAVE_NET_IF_H) && defined(HAVE_SYS_IOCTL_H)

#ifdef SIOCGIFCONF
DEFUN ("network-interface-list", Fnetwork_interface_list, Snetwork_interface_list, 0, 0, 0,
       doc: /* Return an alist of all network interfaces and their network address.
Each element is a cons, the car of which is a string containing the
interface name, and the cdr is the network address in internal
format; see the description of ADDRESS in `make-network-process'.  */)
     ()
{
  struct ifconf ifconf;
  struct ifreq *ifreqs = NULL;
  int ifaces = 0;
  int buf_size, s;
  Lisp_Object res;

  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0)
    return Qnil;

 again:
  ifaces += 25;
  buf_size = ifaces * sizeof(ifreqs[0]);
  ifreqs = (struct ifreq *)xrealloc(ifreqs, buf_size);
  if (!ifreqs)
    {
      close (s);
      return Qnil;
    }

  ifconf.ifc_len = buf_size;
  ifconf.ifc_req = ifreqs;
  if (ioctl (s, SIOCGIFCONF, &ifconf))
    {
      close (s);
      return Qnil;
    }

  if (ifconf.ifc_len == buf_size)
    goto again;

  close (s);
  ifaces = ifconf.ifc_len / sizeof (ifreqs[0]);

  res = Qnil;
  while (--ifaces >= 0)
    {
      struct ifreq *ifq = &ifreqs[ifaces];
      char namebuf[sizeof (ifq->ifr_name) + 1];
      if (ifq->ifr_addr.sa_family != AF_INET)
	continue;
      bcopy (ifq->ifr_name, namebuf, sizeof (ifq->ifr_name));
      namebuf[sizeof (ifq->ifr_name)] = 0;
      res = Fcons (Fcons (build_string (namebuf),
			  conv_sockaddr_to_lisp (&ifq->ifr_addr,
						 sizeof (struct sockaddr))),
		   res);
    }

  return res;
}
#endif /* SIOCGIFCONF */

#if defined(SIOCGIFADDR) || defined(SIOCGIFHWADDR) || defined(SIOCGIFFLAGS)

struct ifflag_def {
  int flag_bit;
  char *flag_sym;
};

static struct ifflag_def ifflag_table[] = {
#ifdef IFF_UP
  { IFF_UP,		"up" },
#endif
#ifdef IFF_BROADCAST
  { IFF_BROADCAST,	"broadcast" },
#endif
#ifdef IFF_DEBUG
  { IFF_DEBUG,		"debug" },
#endif
#ifdef IFF_LOOPBACK
  { IFF_LOOPBACK,	"loopback" },
#endif
#ifdef IFF_POINTOPOINT
  { IFF_POINTOPOINT,	"pointopoint" },
#endif
#ifdef IFF_RUNNING
  { IFF_RUNNING,	"running" },
#endif
#ifdef IFF_NOARP
  { IFF_NOARP,		"noarp" },
#endif
#ifdef IFF_PROMISC
  { IFF_PROMISC,	"promisc" },
#endif
#ifdef IFF_NOTRAILERS
  { IFF_NOTRAILERS,	"notrailers" },
#endif
#ifdef IFF_ALLMULTI
  { IFF_ALLMULTI,	"allmulti" },
#endif
#ifdef IFF_MASTER
  { IFF_MASTER,		"master" },
#endif
#ifdef IFF_SLAVE
  { IFF_SLAVE,		"slave" },
#endif
#ifdef IFF_MULTICAST
  { IFF_MULTICAST,	"multicast" },
#endif
#ifdef IFF_PORTSEL
  { IFF_PORTSEL,	"portsel" },
#endif
#ifdef IFF_AUTOMEDIA
  { IFF_AUTOMEDIA,	"automedia" },
#endif
#ifdef IFF_DYNAMIC
  { IFF_DYNAMIC,	"dynamic" },
#endif
#ifdef IFF_OACTIVE
  { IFF_OACTIVE,	"oactive" },	/* OpenBSD: transmission in progress */
#endif
#ifdef IFF_SIMPLEX
  { IFF_SIMPLEX,	"simplex" },	/* OpenBSD: can't hear own transmissions */
#endif
#ifdef IFF_LINK0
  { IFF_LINK0,		"link0" },	/* OpenBSD: per link layer defined bit */
#endif
#ifdef IFF_LINK1
  { IFF_LINK1,		"link1" },	/* OpenBSD: per link layer defined bit */
#endif
#ifdef IFF_LINK2
  { IFF_LINK2,		"link2" },	/* OpenBSD: per link layer defined bit */
#endif
  { 0, 0 }
};

DEFUN ("network-interface-info", Fnetwork_interface_info, Snetwork_interface_info, 1, 1, 0,
       doc: /* Return information about network interface named IFNAME.
The return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),
where ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,
NETMASK is the layer 3 network mask, HWADDR is the layer 2 addres, and
FLAGS is the current flags of the interface.  */)
     (ifname)
     Lisp_Object ifname;
{
  struct ifreq rq;
  Lisp_Object res = Qnil;
  Lisp_Object elt;
  int s;
  int any = 0;

  CHECK_STRING (ifname);

  bzero (rq.ifr_name, sizeof rq.ifr_name);
  strncpy (rq.ifr_name, SDATA (ifname), sizeof (rq.ifr_name));

  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0)
    return Qnil;

  elt = Qnil;
#if defined(SIOCGIFFLAGS) && defined(HAVE_STRUCT_IFREQ_IFR_FLAGS)
  if (ioctl (s, SIOCGIFFLAGS, &rq) == 0)
    {
      int flags = rq.ifr_flags;
      struct ifflag_def *fp;
      int fnum;

      any++;
      for (fp = ifflag_table; flags != 0 && fp->flag_sym; fp++)
	{
	  if (flags & fp->flag_bit)
	    {
	      elt = Fcons (intern (fp->flag_sym), elt);
	      flags -= fp->flag_bit;
	    }
	}
      for (fnum = 0; flags && fnum < 32; fnum++)
	{
	  if (flags & (1 << fnum))
	    {
	      elt = Fcons (make_number (fnum), elt);
	    }
	}
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined(SIOCGIFHWADDR) && defined(HAVE_STRUCT_IFREQ_IFR_HWADDR)
  if (ioctl (s, SIOCGIFHWADDR, &rq) == 0)
    {
      Lisp_Object hwaddr = Fmake_vector (make_number (6), Qnil);
      register struct Lisp_Vector *p = XVECTOR (hwaddr);
      int n;

      any++;
      for (n = 0; n < 6; n++)
	p->contents[n] = make_number (((unsigned char *)&rq.ifr_hwaddr.sa_data[0])[n]);
      elt = Fcons (make_number (rq.ifr_hwaddr.sa_family), hwaddr);
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined(SIOCGIFNETMASK) && (defined(HAVE_STRUCT_IFREQ_IFR_NETMASK) || defined(HAVE_STRUCT_IFREQ_IFR_ADDR))
  if (ioctl (s, SIOCGIFNETMASK, &rq) == 0)
    {
      any++;
#ifdef HAVE_STRUCT_IFREQ_IFR_NETMASK
      elt = conv_sockaddr_to_lisp (&rq.ifr_netmask, sizeof (rq.ifr_netmask));
#else
      elt = conv_sockaddr_to_lisp (&rq.ifr_addr, sizeof (rq.ifr_addr));
#endif
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined(SIOCGIFBRDADDR) && defined(HAVE_STRUCT_IFREQ_IFR_BROADADDR)
  if (ioctl (s, SIOCGIFBRDADDR, &rq) == 0)
    {
      any++;
      elt = conv_sockaddr_to_lisp (&rq.ifr_broadaddr, sizeof (rq.ifr_broadaddr));
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined(SIOCGIFADDR) && defined(HAVE_STRUCT_IFREQ_IFR_ADDR)
  if (ioctl (s, SIOCGIFADDR, &rq) == 0)
    {
      any++;
      elt = conv_sockaddr_to_lisp (&rq.ifr_addr, sizeof (rq.ifr_addr));
    }
#endif
  res = Fcons (elt, res);

  close (s);

  return any ? res : Qnil;
}
#endif
#endif	/* HAVE_SOCKETS */

/* Turn off input and output for process PROC.  */

void
deactivate_process (proc)
     Lisp_Object proc;
{
  register int inchannel, outchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

  inchannel  = p->infd;
  outchannel = p->outfd;

#ifdef ADAPTIVE_READ_BUFFERING
  if (p->read_output_delay > 0)
    {
      if (--process_output_delay_count < 0)
	process_output_delay_count = 0;
      p->read_output_delay = 0;
      p->read_output_skip = 0;
    }
#endif

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

      p->infd  = -1;
      p->outfd = -1;
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
#ifdef NON_BLOCKING_CONNECT
      if (FD_ISSET (inchannel, &connect_wait_mask))
	{
	  FD_CLR (inchannel, &connect_wait_mask);
	  if (--num_pending_connects < 0)
	    abort ();
	}
#endif
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
	  int in  = XPROCESS (process)->infd;
	  int out = XPROCESS (process)->outfd;
	  if (in >= 0)
	    emacs_close (in);
	  if (out >= 0 && in != out)
	    emacs_close (out);
	}
    }
#endif
}

DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
       0, 4, 0,
       doc: /* Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Non-nil arg PROCESS means do not return until some output has been received
from PROCESS.

Non-nil second arg SECONDS and third arg MILLISEC are number of
seconds and milliseconds to wait; return after that much time whether
or not there is input.  If SECONDS is a floating point number,
it specifies a fractional number of seconds to wait.

If optional fourth arg JUST-THIS-ONE is non-nil, only accept output
from PROCESS, suspending reading output from other processes.
If JUST-THIS-ONE is an integer, don't run any timers either.
Return non-nil if we received any output before the timeout expired.  */)
     (process, seconds, millisec, just_this_one)
     register Lisp_Object process, seconds, millisec, just_this_one;
{
  int secs, usecs = 0;

  if (! NILP (process))
    CHECK_PROCESS (process);
  else
    just_this_one = Qnil;

  if (!NILP (seconds))
    {
      if (INTEGERP (seconds))
	secs = XINT (seconds);
      else if (FLOATP (seconds))
	{
	  double timeout = XFLOAT_DATA (seconds);
	  secs = (int) timeout;
	  usecs = (int) ((timeout - (double) secs) * 1000000);
	}
      else
	wrong_type_argument (Qnumberp, seconds);

      if (INTEGERP (millisec))
	{
	  int carry;
	  usecs += XINT (millisec) * 1000;
	  carry = usecs / 1000000;
	  secs += carry;
	  if ((usecs -= carry * 1000000) < 0)
	    {
	      secs--;
	      usecs += 1000000;
	    }
	}

      if (secs < 0 || (secs == 0 && usecs == 0))
	secs = -1, usecs = 0;
    }
  else
    secs = NILP (process) ? -1 : 0;

  return
    (wait_reading_process_output (secs, usecs, 0, 0,
				  Qnil,
				  !NILP (process) ? XPROCESS (process) : NULL,
				  NILP (just_this_one) ? 0 :
				  !INTEGERP (just_this_one) ? 1 : -1)
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
#ifdef AF_INET6
    struct sockaddr_in6 in6;
#endif
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

#ifdef AF_INET6
    case AF_INET6:
      {
	Lisp_Object args[9];
	uint16_t *ip6 = (uint16_t *)&saddr.in6.sin6_addr;
	int i;
	args[0] = build_string ("%x:%x:%x:%x:%x:%x:%x:%x");
	for (i = 0; i < 8; i++)
	  args[i+1] = make_number (ntohs(ip6[i]));
	host = Fformat (9, args);
	service = make_number (ntohs (saddr.in.sin_port));

	args[0] = build_string (" <[%s]:%d>");
	args[1] = host;
	args[2] = service;
	caller = Fformat (3, args);
      }
      break;
#endif

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
  if (getsockname (s, &saddr.sa, &len) == 0)
    contact = Fplist_put (contact, QClocal,
			  conv_sockaddr_to_lisp (&saddr.sa, len));
#endif

  p->childp = contact;
  p->plist = Fcopy_sequence (ps->plist);

  p->buffer = buffer;
  p->sentinel = ps->sentinel;
  p->filter = ps->filter;
  p->command = Qnil;
  p->pid = 0;
  p->infd  = s;
  p->outfd = s;
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
  setup_process_coding_systems (proc);

  p->decoding_buf = make_uninit_string (0);
  p->decoding_carryover = 0;
  p->encoding_buf = make_uninit_string (0);

  p->inherit_coding_system_flag
    = (NILP (buffer) ? 0 : ps->inherit_coding_system_flag);

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
   function Fwaiting_for_user_input_p below) whether Emacs was waiting
   for user-input when that process-filter was called.
   waiting_for_input cannot be used as that is by definition 0 when
   lisp code is being evalled.
   This is also used in record_asynch_buffer_change.
   For that purpose, this must be 0
   when not inside wait_reading_process_output.  */
static int waiting_for_user_input_p;

static Lisp_Object
wait_reading_process_output_unwind (data)
     Lisp_Object data;
{
  waiting_for_user_input_p = XINT (data);
  return Qnil;
}

/* This is here so breakpoints can be put on it.  */
static void
wait_reading_process_output_1 ()
{
}

/* Use a wrapper around select to work around a bug in gdb 5.3.
   Normally, the wrapper is optimzed away by inlining.

   If emacs is stopped inside select, the gdb backtrace doesn't
   show the function which called select, so it is practically
   impossible to step through wait_reading_process_output.  */

#ifndef select
static INLINE int
select_wrapper (n, rfd, wfd, xfd, tmo)
  int n;
  SELECT_TYPE *rfd, *wfd, *xfd;
  EMACS_TIME *tmo;
{
  return select (n, rfd, wfd, xfd, tmo);
}
#define select select_wrapper
#endif

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

   DO_DISPLAY != 0 means redisplay should be done to show subprocess
     output that arrives.

   If WAIT_FOR_CELL is a cons cell, wait until its car is non-nil
     (and gobble terminal input into the buffer if any arrives).

   If WAIT_PROC is specified, wait until something arrives from that
     process.  The return value is true if we read some input from
     that process.

   If JUST_WAIT_PROC is non-nil, handle only output from WAIT_PROC
     (suspending output from other processes).  A negative value
     means don't run any timers either.

   If WAIT_PROC is specified, then the function returns true if we
     received input from that process before the timeout elapsed.
   Otherwise, return true if we received input from any process.  */

int
wait_reading_process_output (time_limit, microsecs, read_kbd, do_display,
			     wait_for_cell, wait_proc, just_wait_proc)
     int time_limit, microsecs, read_kbd, do_display;
     Lisp_Object wait_for_cell;
     struct Lisp_Process *wait_proc;
     int just_wait_proc;
{
  register int channel, nfds;
  SELECT_TYPE Available;
#ifdef NON_BLOCKING_CONNECT
  SELECT_TYPE Connecting;
  int check_connect;
#endif
  int check_delay, no_avail;
  int xerrno;
  Lisp_Object proc;
  EMACS_TIME timeout, end_time;
  int wait_channel = -1;
  int got_some_input = 0;
  int count = SPECPDL_INDEX ();

  FD_ZERO (&Available);
#ifdef NON_BLOCKING_CONNECT
  FD_ZERO (&Connecting);
#endif

  /* If wait_proc is a process to watch, set wait_channel accordingly.  */
  if (wait_proc != NULL)
    wait_channel = wait_proc->infd;

  record_unwind_protect (wait_reading_process_output_unwind,
			 make_number (waiting_for_user_input_p));
  waiting_for_user_input_p = read_kbd;

  /* Since we may need to wait several times,
     compute the absolute time to return at.  */
  if (time_limit || microsecs)
    {
      EMACS_GET_TIME (end_time);
      EMACS_SET_SECS_USECS (timeout, time_limit, microsecs);
      EMACS_ADD_TIME (end_time, end_time, timeout);
    }
#ifdef POLL_INTERRUPTED_SYS_CALL
  /* AlainF 5-Jul-1996
     HP-UX 10.10 seem to have problems with signals coming in
     Causes "poll: interrupted system call" messages when Emacs is run
     in an X window
     Turn off periodic alarms (in case they are in use),
     and then turn off any other atimers.  */
  stop_polling ();
  turn_on_atimers (0);
#endif /* POLL_INTERRUPTED_SYS_CALL */

  while (1)
    {
      int timeout_reduced_for_timers = 0;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	QUIT;
#ifdef SYNC_INPUT
      else if (interrupt_input_pending)
	handle_async_input ();
#endif

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
      if (NILP (wait_for_cell)
	  && just_wait_proc >= 0)
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
	  if (read_kbd != 0
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
	      wait_reading_process_output_1 ();
	    }
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout.

	 It is important that we do this before checking for process
	 activity.  If we get a SIGCHLD after the explicit checks for
	 process activity, timeout is the only way we will know.  */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* If status of something has changed, and no input is
	 available, notify the user of the change right away.  After
	 this explicit check, we'll let the SIGCHLD handler zap
	 timeout to get our attention.  */
      if (update_tick != process_tick && do_display)
	{
	  SELECT_TYPE Atemp;
#ifdef NON_BLOCKING_CONNECT
	  SELECT_TYPE Ctemp;
#endif

	  Atemp = input_wait_mask;
#if 0
          /* On Mac OS X 10.0, the SELECT system call always says input is
             present (for reading) at stdin, even when none is.  This
             causes the call to SELECT below to return 1 and
             status_notify not to be called.  As a result output of
             subprocesses are incorrectly discarded.
	  */
          FD_CLR (0, &Atemp);
#endif
	  IF_NON_BLOCKING_CONNECT (Ctemp = connect_wait_mask);

	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	  if ((select (max (max (max_process_desc, max_keyboard_desc),
			      max_gpm_desc) + 1,
		       &Atemp,
#ifdef NON_BLOCKING_CONNECT
		       (num_pending_connects > 0 ? &Ctemp : (SELECT_TYPE *)0),
#else
		       (SELECT_TYPE *)0,
#endif
		       (SELECT_TYPE *)0, &timeout)
	       <= 0))
	    {
	      /* It's okay for us to do this and then continue with
		 the loop, since timeout has already been zeroed out.  */
	      clear_waiting_for_input ();
	      status_notify (NULL);
	    }
	}

      /* Don't wait for output from a non-running process.  Just
         read whatever data has already been received.  */
      if (wait_proc && wait_proc->raw_status_new)
	update_status (wait_proc);
      if (wait_proc
	  && ! EQ (wait_proc->status, Qrun)
	  && ! EQ (wait_proc->status, Qconnect))
	{
	  int nread, total_nread = 0;

	  clear_waiting_for_input ();
	  XSETPROCESS (proc, wait_proc);

	  /* Read data from the process, until we exhaust it.  */
	  while (wait_proc->infd >= 0)
	    {
	      nread = read_process_output (proc, wait_proc->infd);

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

      if (wait_proc && just_wait_proc)
	{
	  if (wait_proc->infd < 0)  /* Terminated */
	    break;
	  FD_SET (wait_proc->infd, &Available);
	  check_delay = 0;
	  IF_NON_BLOCKING_CONNECT (check_connect = 0);
	}
      else if (!NILP (wait_for_cell))
	{
	  Available = non_process_wait_mask;
	  check_delay = 0;
	  IF_NON_BLOCKING_CONNECT (check_connect = 0);
	}
      else
	{
	  if (! read_kbd)
	    Available = non_keyboard_wait_mask;
	  else
	    Available = input_wait_mask;
	  IF_NON_BLOCKING_CONNECT (check_connect = (num_pending_connects > 0));
 	  check_delay = wait_channel >= 0 ? 0 : process_output_delay_count;
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
	  if (read_kbd < 0)
	    set_waiting_for_input (&timeout);
	}

      no_avail = 0;
      if (read_kbd && detect_input_pending ())
	{
	  nfds = 0;
	  no_avail = 1;
	}
      else
	{
#ifdef NON_BLOCKING_CONNECT
	  if (check_connect)
	    Connecting = connect_wait_mask;
#endif

#ifdef ADAPTIVE_READ_BUFFERING
	  /* Set the timeout for adaptive read buffering if any
	     process has non-zero read_output_skip and non-zero
	     read_output_delay, and we are not reading output for a
	     specific wait_channel.  It is not executed if
	     Vprocess_adaptive_read_buffering is nil.  */
	  if (process_output_skip && check_delay > 0)
	    {
	      int usecs = EMACS_USECS (timeout);
	      if (EMACS_SECS (timeout) > 0 || usecs > READ_OUTPUT_DELAY_MAX)
		usecs = READ_OUTPUT_DELAY_MAX;
	      for (channel = 0; check_delay > 0 && channel <= max_process_desc; channel++)
		{
		  proc = chan_process[channel];
		  if (NILP (proc))
		    continue;
		  /* Find minimum non-zero read_output_delay among the
		     processes with non-zero read_output_skip.  */
		  if (XPROCESS (proc)->read_output_delay > 0)
		    {
		      check_delay--;
		      if (!XPROCESS (proc)->read_output_skip)
			continue;
		      FD_CLR (channel, &Available);
		      XPROCESS (proc)->read_output_skip = 0;
		      if (XPROCESS (proc)->read_output_delay < usecs)
			usecs = XPROCESS (proc)->read_output_delay;
		    }
		}
	      EMACS_SET_SECS_USECS (timeout, 0, usecs);
	      process_output_skip = 0;
	    }
#endif

	  nfds = select (max (max (max_process_desc, max_keyboard_desc),
			      max_gpm_desc) + 1,
			 &Available,
#ifdef NON_BLOCKING_CONNECT
			 (check_connect ? &Connecting : (SELECT_TYPE *)0),
#else
			 (SELECT_TYPE *)0,
#endif
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
	  IF_NON_BLOCKING_CONNECT (check_connect = 0);
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
	 And on hpux, since we turn off polling in wait_reading_process_output,
	 it might never get read at all if we don't spend much time
	 outside of wait_reading_process_output.  */
      if (read_kbd && interrupt_input
	  && keyboard_bit_set (&Available)
	  && input_polling_used ())
	kill (getpid (), SIGALRM);
#endif

      /* Check for keyboard input */
      /* If there is any, return immediately
	 to give it higher priority than subprocesses */

      if (read_kbd != 0)
	{
	  int old_timers_run = timers_run;
	  struct buffer *old_buffer = current_buffer;
	  Lisp_Object old_window = selected_window;
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
	      && (old_buffer != current_buffer
	      || !EQ (old_window, selected_window)))
	    record_asynch_buffer_change ();

	  if (leave)
	    break;
	}

      /* If there is unread keyboard input, also return.  */
      if (read_kbd != 0
	  && requeued_events_pending_p ())
	break;

      /* If we are not checking for keyboard input now,
	 do process events (but don't run any timers).
	 This is so that X events will be processed.
	 Otherwise they may have to wait until polling takes place.
	 That would causes delays in pasting selections, for example.

	 (We used to do this only if wait_for_cell.)  */
      if (read_kbd == 0 && detect_input_pending ())
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

      if (read_kbd && interrupt_input
	  && keyboard_bit_set (&Available) && ! noninteractive)
	kill (getpid (), SIGIO);
#endif

      if (! wait_proc)
	got_some_input |= nfds > 0;

      /* If checking input just got us a size-change event from X,
	 obey it now if we should.  */
      if (read_kbd || ! NILP (wait_for_cell))
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
#endif /* O_NDELAY */
#endif /* O_NONBLOCK */
#ifdef HAVE_PTYS
	      /* On some OSs with ptys, when the process on one end of
		 a pty exits, the other end gets an error reading with
		 errno = EIO instead of getting an EOF (0 bytes read).
		 Therefore, if we get an error reading and errno =
		 EIO, just continue, because the child process has
		 exited and should clean itself up soon (e.g. when we
		 get a SIGCHLD).

		 However, it has been known to happen that the SIGCHLD
		 got lost.  So raise the signal again just in case.
		 It can't hurt.  */
	      else if (nread == -1 && errno == EIO)
		{
		  /* Clear the descriptor now, so we only raise the signal once.  */
		  FD_CLR (channel, &input_wait_mask);
		  FD_CLR (channel, &non_keyboard_wait_mask);

		  kill (getpid (), SIGCHLD);
		}
#endif /* HAVE_PTYS */
	      /* If we can detect process termination, don't consider the process
		 gone just because its pipe is closed.  */
#ifdef SIGCHLD
	      else if (nread == 0 && !NETCONN_P (proc))
		;
#endif
	      else
		{
		  /* Preserve status of processes already terminated.  */
		  XPROCESS (proc)->tick = ++process_tick;
		  deactivate_process (proc);
		  if (XPROCESS (proc)->raw_status_new)
		    update_status (XPROCESS (proc));
		  if (EQ (XPROCESS (proc)->status, Qrun))
		    XPROCESS (proc)->status
		      = Fcons (Qexit, Fcons (make_number (256), Qnil));
		}
	    }
#ifdef NON_BLOCKING_CONNECT
	  if (check_connect && FD_ISSET (channel, &Connecting)
	      && FD_ISSET (channel, &connect_wait_mask))
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
		  p->tick = ++process_tick;
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
		      FD_SET (p->infd, &input_wait_mask);
		      FD_SET (p->infd, &non_keyboard_wait_mask);
		    }
		}
	    }
#endif /* NON_BLOCKING_CONNECT */
	}			/* end for each file descriptor */
    }				/* end while exit conditions not met */

  unbind_to (count, Qnil);

  /* If calling from keyboard input, do not quit
     since we want to return C-g as an input character.
     Otherwise, do pending quit if requested.  */
  if (read_kbd >= 0)
    {
      /* Prevent input_pending from remaining set if we quit.  */
      clear_input_pending ();
      QUIT;
    }
#ifdef POLL_INTERRUPTED_SYS_CALL
  /* AlainF 5-Jul-1996
     HP-UX 10.10 seems to have problems with signals coming in
     Causes "poll: interrupted system call" messages when Emacs is run
     in an X window
     Turn periodic alarms back on */
  start_polling ();
#endif /* POLL_INTERRUPTED_SYS_CALL */

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

   This function reads at most 4096 characters.
   If you want to read all available subprocess output,
   you must call it repeatedly until it returns zero.

   The characters read are decoded according to PROC's coding-system
   for decoding.  */

static int
read_process_output (proc, channel)
     Lisp_Object proc;
     register int channel;
{
  register int nbytes;
  char *chars;
  register Lisp_Object outstream;
  register struct buffer *old = current_buffer;
  register struct Lisp_Process *p = XPROCESS (proc);
  register int opoint;
  struct coding_system *coding = proc_decode_coding_system[channel];
  int carryover = p->decoding_carryover;
  int readmax = 4096;

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
      bcopy (SDATA (p->decoding_buf), buf, carryover);
      bcopy (vs->inputBuffer, chars + carryover, nbytes);
    }
#else /* not VMS */

  chars = (char *) alloca (carryover + readmax);
  if (carryover)
    /* See the comment above.  */
    bcopy (SDATA (p->decoding_buf), chars, carryover);

#ifdef DATAGRAM_SOCKETS
  /* We have a working select, so proc_buffered_char is always -1.  */
  if (DATAGRAM_CHAN_P (channel))
    {
      int len = datagram_address[channel].len;
      nbytes = recvfrom (channel, chars + carryover, readmax,
			 0, datagram_address[channel].sa, &len);
    }
  else
#endif
  if (proc_buffered_char[channel] < 0)
    {
      nbytes = emacs_read (channel, chars + carryover, readmax);
#ifdef ADAPTIVE_READ_BUFFERING
      if (nbytes > 0 && p->adaptive_read_buffering)
	{
	  int delay = p->read_output_delay;
	  if (nbytes < 256)
	    {
	      if (delay < READ_OUTPUT_DELAY_MAX_MAX)
		{
		  if (delay == 0)
		    process_output_delay_count++;
		  delay += READ_OUTPUT_DELAY_INCREMENT * 2;
		}
	    }
	  else if (delay > 0 && (nbytes == readmax))
	    {
	      delay -= READ_OUTPUT_DELAY_INCREMENT;
	      if (delay == 0)
		process_output_delay_count--;
	    }
	  p->read_output_delay = delay;
	  if (delay)
	    {
	      p->read_output_skip = 1;
	      process_output_skip = 1;
	    }
	}
#endif
    }
  else
    {
      chars[carryover] = proc_buffered_char[channel];
      proc_buffered_char[channel] = -1;
      nbytes = emacs_read (channel, chars + carryover + 1,  readmax - 1);
      if (nbytes < 0)
	nbytes = 1;
      else
	nbytes = nbytes + 1;
    }
#endif /* not VMS */

  p->decoding_carryover = 0;

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
      int count = SPECPDL_INDEX ();
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
	  tem = Fmatch_data (Qnil, Qnil, Qnil);
	  restore_search_regs ();
	  record_unwind_save_match_data ();
	  Fset_match_data (tem, Qt);
	}

      /* For speed, if a search happens within this code,
	 save the match data in a special nonrecursive fashion.  */
      running_asynch_code = 1;

      decode_coding_c_string (coding, chars, nbytes, Qt);
      text = coding->dst_object;
      Vlast_coding_system_used = CODING_ID_NAME (coding->id);
      /* A new coding system might be found.  */
      if (!EQ (p->decode_coding_system, Vlast_coding_system_used))
	{
	  p->decode_coding_system = Vlast_coding_system_used;

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
	      && proc_encode_coding_system[p->outfd])
	    {
	      p->encode_coding_system
		= coding_inherit_eol_type (Vlast_coding_system_used, Qnil);
	      setup_coding_system (p->encode_coding_system,
				   proc_encode_coding_system[p->outfd]);
	    }
	}

      if (coding->carryover_bytes > 0)
	{
	  if (SCHARS (p->decoding_buf) < coding->carryover_bytes)
	    p->decoding_buf = make_uninit_string (coding->carryover_bytes);
	  bcopy (coding->carryover, SDATA (p->decoding_buf),
		 coding->carryover_bytes);
	  p->decoding_carryover = coding->carryover_bytes;
	}
      /* Adjust the multibyteness of TEXT to that of the filter.  */
      if (!p->filter_multibyte != !STRING_MULTIBYTE (text))
	text = (STRING_MULTIBYTE (text)
		? Fstring_as_unibyte (text)
		: Fstring_to_multibyte (text));
      if (SBYTES (text) > 0)
	internal_condition_case_1 (read_process_output_call,
				   Fcons (outstream,
					  Fcons (proc, Fcons (text, Qnil))),
				   !NILP (Vdebug_on_error) ? Qnil : Qerror,
				   read_process_output_error_handler);

      /* If we saved the match data nonrecursively, restore it now.  */
      restore_search_regs ();
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
	   cause trouble (for example it would make sit_for return).  */
	if (waiting_for_user_input_p == -1)
	  record_asynch_buffer_change ();

#ifdef VMS
      start_vms_process_read (vs);
#endif
      unbind_to (count, Qnil);
      return nbytes;
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

      decode_coding_c_string (coding, chars, nbytes, Qt);
      text = coding->dst_object;
      Vlast_coding_system_used = CODING_ID_NAME (coding->id);
      /* A new coding system might be found.  See the comment in the
	 similar code in the previous `if' block.  */
      if (!EQ (p->decode_coding_system, Vlast_coding_system_used))
	{
	  p->decode_coding_system = Vlast_coding_system_used;
	  if (NILP (p->encode_coding_system)
	      && proc_encode_coding_system[p->outfd])
	    {
	      p->encode_coding_system
		= coding_inherit_eol_type (Vlast_coding_system_used, Qnil);
	      setup_coding_system (p->encode_coding_system,
				   proc_encode_coding_system[p->outfd]);
	    }
	}
      if (coding->carryover_bytes > 0)
	{
	  if (SCHARS (p->decoding_buf) < coding->carryover_bytes)
	    p->decoding_buf = make_uninit_string (coding->carryover_bytes);
	  bcopy (coding->carryover, SDATA (p->decoding_buf),
		 coding->carryover_bytes);
	  p->decoding_carryover = coding->carryover_bytes;
	}
      /* Adjust the multibyteness of TEXT to that of the buffer.  */
      if (NILP (current_buffer->enable_multibyte_characters)
	  != ! STRING_MULTIBYTE (text))
	text = (STRING_MULTIBYTE (text)
		? Fstring_as_unibyte (text)
		: Fstring_to_multibyte (text));
      /* Insert before markers in case we are inserting where
	 the buffer's mark is, and the user's next command is Meta-y.  */
      insert_from_string_before_markers (text, 0, 0,
					 SCHARS (text), SBYTES (text), 0);

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
       doc: /* Returns non-nil if Emacs is waiting for input from the user.
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
  SIGNAL_THREAD_CHECK (SIGPIPE);
#ifdef BSD4_1
  sigrelse (SIGPIPE);
  sigrelse (SIGALRM);
#endif /* BSD4_1 */
  sigunblock (sigmask (SIGPIPE));
  longjmp (send_process_frame, 1);
}

/* Send some data to process PROC.
   BUF is the beginning of the data; LEN is the number of characters.
   OBJECT is the Lisp object that the data comes from.  If OBJECT is
   nil or t, it means that the data comes from C string.

   If OBJECT is not nil, the data is encoded by PROC's coding-system
   for encoding before it is sent.

   This function can evaluate Lisp code and can garbage collect.  */

static void
send_process (proc, buf, len, object)
     volatile Lisp_Object proc;
     unsigned char *volatile buf;
     volatile int len;
     volatile Lisp_Object object;
{
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  struct Lisp_Process *p = XPROCESS (proc);
  int rv;
  struct coding_system *coding;
  struct gcpro gcpro1;
  SIGTYPE (*volatile old_sigpipe) ();

  GCPRO1 (object);

#ifdef VMS
  VMS_PROC_STUFF *vs, *get_vms_process_pointer();
#endif /* VMS */

  if (p->raw_status_new)
    update_status (p);
  if (! EQ (p->status, Qrun))
    error ("Process %s not running", SDATA (p->name));
  if (p->outfd < 0)
    error ("Output file descriptor of %s is closed", SDATA (p->name));

  coding = proc_encode_coding_system[p->outfd];
  Vlast_coding_system_used = CODING_ID_NAME (coding->id);

  if ((STRINGP (object) && STRING_MULTIBYTE (object))
      || (BUFFERP (object)
	  && !NILP (XBUFFER (object)->enable_multibyte_characters))
      || EQ (object, Qt))
    {
      if (!EQ (Vlast_coding_system_used, p->encode_coding_system))
	/* The coding system for encoding was changed to raw-text
	   because we sent a unibyte text previously.  Now we are
	   sending a multibyte text, thus we must encode it by the
	   original coding system specified for the current process.  */
	setup_coding_system (p->encode_coding_system, coding);
      coding->src_multibyte = 1;
    }
  else
    {
      /* For sending a unibyte text, character code conversion should
	 not take place but EOL conversion should.  So, setup raw-text
	 or one of the subsidiary if we have not yet done it.  */
      if (CODING_REQUIRE_ENCODING (coding))
	{
	  if (CODING_REQUIRE_FLUSHING (coding))
	    {
	      /* But, before changing the coding, we must flush out data.  */
	      coding->mode |= CODING_MODE_LAST_BLOCK;
	      send_process (proc, "", 0, Qt);
	      coding->mode &= CODING_MODE_LAST_BLOCK;
	    }
	  setup_coding_system (raw_text_coding_system
			       (Vlast_coding_system_used),
			       coding);
	  coding->src_multibyte = 0;
	}
    }
  coding->dst_multibyte = 0;

  if (CODING_REQUIRE_ENCODING (coding))
    {
      coding->dst_object = Qt;
      if (BUFFERP (object))
	{
	  int from_byte, from, to;
	  int save_pt, save_pt_byte;
	  struct buffer *cur = current_buffer;

	  set_buffer_internal (XBUFFER (object));
	  save_pt = PT, save_pt_byte = PT_BYTE;

	  from_byte = PTR_BYTE_POS (buf);
	  from = BYTE_TO_CHAR (from_byte);
	  to = BYTE_TO_CHAR (from_byte + len);
	  TEMP_SET_PT_BOTH (from, from_byte);
	  encode_coding_object (coding, object, from, from_byte,
				to, from_byte + len, Qt);
	  TEMP_SET_PT_BOTH (save_pt, save_pt_byte);
	  set_buffer_internal (cur);
	}
      else if (STRINGP (object))
	{
	  encode_coding_string (coding, object, 1);
	}
      else
	{
	  coding->dst_object = make_unibyte_string (buf, len);
	  coding->produced = len;
	}

      len = coding->produced;
      buf = SDATA (coding->dst_object);
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
      pty_max_bytes = fpathconf (p->outfd, _PC_MAX_CANON);
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

	  /* Decide how much data we can send in one batch.
	     Long lines need to be split into multiple batches.  */
	  if (p->pty_flag)
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
	      int outfd = p->outfd;
	      old_sigpipe = (SIGTYPE (*) ()) signal (SIGPIPE, send_process_trap);
#ifdef DATAGRAM_SOCKETS
	      if (DATAGRAM_CHAN_P (outfd))
		{
		  rv = sendto (outfd, (char *) buf, this,
			       0, datagram_address[outfd].sa,
			       datagram_address[outfd].len);
		  if (rv < 0 && errno == EMSGSIZE)
		    {
		      signal (SIGPIPE, old_sigpipe);
		      report_file_error ("sending datagram",
					 Fcons (proc, Qnil));
		    }
		}
	      else
#endif
		{
		  rv = emacs_write (outfd, (char *) buf, this);
#ifdef ADAPTIVE_READ_BUFFERING
		  if (p->read_output_delay > 0
		      && p->adaptive_read_buffering == 1)
		    {
		      p->read_output_delay = 0;
		      process_output_delay_count--;
		      p->read_output_skip = 0;
		    }
#endif
		}
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
			  ioctl (p->outfd, TIOCFLUSH, &flags);
			}
#endif /* BROKEN_PTY_READ_AFTER_EAGAIN */

		      /* Running filters might relocate buffers or strings.
			 Arrange to relocate BUF.  */
		      if (BUFFERP (object))
			offset = BUF_PTR_BYTE_POS (XBUFFER (object), buf);
		      else if (STRINGP (object))
			offset = buf - SDATA (object);

#ifdef EMACS_HAS_USECS
		      wait_reading_process_output (0, 20000, 0, 0, Qnil, NULL, 0);
#else
		      wait_reading_process_output (1, 0, 0, 0, Qnil, NULL, 0);
#endif

		      if (BUFFERP (object))
			buf = BUF_BYTE_ADDRESS (XBUFFER (object), offset);
		      else if (STRINGP (object))
			buf = offset + SDATA (object);

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
      signal (SIGPIPE, old_sigpipe);
#ifndef VMS
      proc = process_sent_to;
      p = XPROCESS (proc);
#endif
      p->raw_status_new = 0;
      p->status = Fcons (Qexit, Fcons (make_number (256), Qnil));
      p->tick = ++process_tick;
      deactivate_process (proc);
#ifdef VMS
      error ("Error writing to process %s; closed it", SDATA (p->name));
#else
      error ("SIGPIPE raised on process %s; closed it", SDATA (p->name));
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
  send_process (proc, SDATA (string),
		SBYTES (string), string);
  return Qnil;
}

/* Return the foreground process group for the tty/pty that
   the process P uses.  */
static int
emacs_get_tty_pgrp (p)
     struct Lisp_Process *p;
{
  int gid = -1;

#ifdef TIOCGPGRP
  if (ioctl (p->infd, TIOCGPGRP, &gid) == -1 && ! NILP (p->tty_name))
    {
      int fd;
      /* Some OS:es (Solaris 8/9) does not allow TIOCGPGRP from the
	 master side.  Try the slave side.  */
      fd = emacs_open (XSTRING (p->tty_name)->data, O_RDONLY, 0);

      if (fd != -1)
	{
	  ioctl (fd, TIOCGPGRP, &gid);
	  emacs_close (fd);
	}
    }
#endif /* defined (TIOCGPGRP ) */

  return gid;
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
  int gid;
  Lisp_Object proc;
  struct Lisp_Process *p;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->childp, Qt))
    error ("Process %s is not a subprocess",
	   SDATA (p->name));
  if (p->infd < 0)
    error ("Process %s is not active",
	   SDATA (p->name));

  gid = emacs_get_tty_pgrp (p);

  if (gid == p->pid)
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
	   SDATA (p->name));
  if (p->infd < 0)
    error ("Process %s is not active",
	   SDATA (p->name));

  if (!p->pty_flag)
    current_group = Qnil;

  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (NILP (current_group))
    /* Send the signal to the shell's process group.  */
    gid = p->pid;
  else
    {
#ifdef SIGNALS_VIA_CHARACTERS
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */

      /* TERMIOS is the latest and bestest, and seems most likely to
         work.  If the system has it, use it.  */
#ifdef HAVE_TERMIOS
      struct termios t;
      cc_t *sig_char = NULL;

      tcgetattr (p->infd, &t);

      switch (signo)
	{
	case SIGINT:
	  sig_char = &t.c_cc[VINTR];
	  break;

	case SIGQUIT:
	  sig_char = &t.c_cc[VQUIT];
	  break;

  	case SIGTSTP:
#if defined (VSWTCH) && !defined (PREFER_VSUSP)
	  sig_char = &t.c_cc[VSWTCH];
#else
	  sig_char = &t.c_cc[VSUSP];
#endif
	  break;
	}

      if (sig_char && *sig_char != CDISABLE)
	{
	  send_process (proc, sig_char, 1, Qnil);
	  return;
	}
      /* If we can't send the signal with a character,
	 fall through and send it another way.  */
#else /* ! HAVE_TERMIOS */

      /* On Berkeley descendants, the following IOCTL's retrieve the
	 current control characters.  */
#if defined (TIOCGLTC) && defined (TIOCGETC)

      struct tchars c;
      struct ltchars lc;

      switch (signo)
	{
	case SIGINT:
	  ioctl (p->infd, TIOCGETC, &c);
	  send_process (proc, &c.t_intrc, 1, Qnil);
	  return;
	case SIGQUIT:
	  ioctl (p->infd, TIOCGETC, &c);
	  send_process (proc, &c.t_quitc, 1, Qnil);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (p->infd, TIOCGLTC, &lc);
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
	  ioctl (p->infd, TCGETA, &t);
	  send_process (proc, &t.c_cc[VINTR], 1, Qnil);
	  return;
	case SIGQUIT:
	  ioctl (p->infd, TCGETA, &t);
	  send_process (proc, &t.c_cc[VQUIT], 1, Qnil);
	  return;
#ifdef SIGTSTP
	case SIGTSTP:
	  ioctl (p->infd, TCGETA, &t);
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
	/* In this case, the code above should alway returns.  */
	abort ();
#endif /* ! defined HAVE_TERMIOS */

      /* The code above may fall through if it can't
	 handle the signal.  */
#endif /* defined (SIGNALS_VIA_CHARACTERS) */

#ifdef TIOCGPGRP
      /* Get the current pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.
	 On pfa systems, saka@pfu.fujitsu.co.JP writes:
	 "TIOCGPGRP symbol defined in sys/ioctl.h at E50.
	 But, TIOCGPGRP does not work on E50 ;-P works fine on E60"
	 His patch indicates that if TIOCGPGRP returns an error, then
	 we should just assume that p->pid is also the process group id.  */

      gid = emacs_get_tty_pgrp (p);

      if (gid == -1)
	/* If we can't get the information, assume
	   the shell owns the tty.  */
	gid = p->pid;

      /* It is not clear whether anything really can set GID to -1.
	 Perhaps on some system one of those ioctls can or could do so.
	 Or perhaps this is vestigial.  */
      if (gid == -1)
	no_pgrp = 1;
#else  /* ! defined (TIOCGPGRP ) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = p->pid;
#endif /* ! defined (TIOCGPGRP ) */

      /* If current_group is lambda, and the shell owns the terminal,
	 don't send any signal.  */
      if (EQ (current_group, Qlambda) && gid == p->pid)
	return;
    }

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      p->raw_status_new = 0;
      p->status = Qrun;
      p->tick = ++process_tick;
      if (!nomsg)
	status_notify (NULL);
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
      sys$forcex (&(p->pid), 0, 1);
      whoosh:
#endif
      flush_pending_output (p->infd);
      break;
    }

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  if (no_pgrp)
    {
      kill (p->pid, signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (!NILP (current_group))
    {
      if (ioctl (p->infd, TIOCSIGSEND, signo) == -1)
	EMACS_KILLPG (gid, signo);
    }
  else
    {
      gid = - p->pid;
      kill (gid, signo);
    }
#else /* ! defined (TIOCSIGSEND) */
  EMACS_KILLPG (gid, signo);
#endif /* ! defined (TIOCSIGSEND) */
}

DEFUN ("interrupt-process", Finterrupt_process, Sinterrupt_process, 0, 2, 0,
       doc: /* Interrupt process PROCESS.
PROCESS may be a process, a buffer, or the name of a process or buffer.
No arg or nil means current buffer's process.
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
	  && p->infd >= 0)
	{
	  FD_CLR (p->infd, &input_wait_mask);
	  FD_CLR (p->infd, &non_keyboard_wait_mask);
	}
      p->command = Qt;
      return process;
    }
#endif
#ifndef SIGTSTP
  error ("No SIGTSTP support");
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
	  && p->infd >= 0
	  && (!EQ (p->filter, Qt) || EQ (p->status, Qlisten)))
	{
	  FD_SET (p->infd, &input_wait_mask);
	  FD_SET (p->infd, &non_keyboard_wait_mask);
	}
      p->command = Qnil;
      return process;
    }
#endif
#ifdef SIGCONT
    process_send_signal (process, SIGCONT, current_group, 0);
#else
    error ("No SIGCONT support");
#endif
  return process;
}

DEFUN ("signal-process", Fsignal_process, Ssignal_process,
       2, 2, "sProcess (name or number): \nnSignal code: ",
       doc: /* Send PROCESS the signal with code SIGCODE.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.  */)
     (process, sigcode)
     Lisp_Object process, sigcode;
{
  pid_t pid;

  if (INTEGERP (process))
    {
      pid = XINT (process);
      goto got_it;
    }

  if (FLOATP (process))
    {
      pid = (pid_t) XFLOAT_DATA (process);
      goto got_it;
    }

  if (STRINGP (process))
    {
      Lisp_Object tem;
      if (tem = Fget_process (process), NILP (tem))
	{
	  pid = XINT (Fstring_to_number (process, make_number (10)));
	  if (pid > 0)
	    goto got_it;
	}
      process = tem;
    }
  else
    process = get_process (process);

  if (NILP (process))
    return process;

  CHECK_PROCESS (process);
  pid = XPROCESS (process)->pid;
  if (pid <= 0)
    error ("Cannot signal process %s", SDATA (XPROCESS (process)->name));

 got_it:

#define parse_signal(NAME, VALUE)		\
  else if (!xstricmp (name, NAME))		\
    XSETINT (sigcode, VALUE)

  if (INTEGERP (sigcode))
    ;
  else
    {
      unsigned char *name;

      CHECK_SYMBOL (sigcode);
      name = SDATA (SYMBOL_NAME (sigcode));

      if (!strncmp(name, "SIG", 3) || !strncmp(name, "sig", 3))
	name += 3;

      if (0)
	;
#ifdef SIGUSR1
      parse_signal ("usr1", SIGUSR1);
#endif
#ifdef SIGUSR2
      parse_signal ("usr2", SIGUSR2);
#endif
#ifdef SIGTERM
      parse_signal ("term", SIGTERM);
#endif
#ifdef SIGHUP
      parse_signal ("hup", SIGHUP);
#endif
#ifdef SIGINT
      parse_signal ("int", SIGINT);
#endif
#ifdef SIGQUIT
      parse_signal ("quit", SIGQUIT);
#endif
#ifdef SIGILL
      parse_signal ("ill", SIGILL);
#endif
#ifdef SIGABRT
      parse_signal ("abrt", SIGABRT);
#endif
#ifdef SIGEMT
      parse_signal ("emt", SIGEMT);
#endif
#ifdef SIGKILL
      parse_signal ("kill", SIGKILL);
#endif
#ifdef SIGFPE
      parse_signal ("fpe", SIGFPE);
#endif
#ifdef SIGBUS
      parse_signal ("bus", SIGBUS);
#endif
#ifdef SIGSEGV
      parse_signal ("segv", SIGSEGV);
#endif
#ifdef SIGSYS
      parse_signal ("sys", SIGSYS);
#endif
#ifdef SIGPIPE
      parse_signal ("pipe", SIGPIPE);
#endif
#ifdef SIGALRM
      parse_signal ("alrm", SIGALRM);
#endif
#ifdef SIGURG
      parse_signal ("urg", SIGURG);
#endif
#ifdef SIGSTOP
      parse_signal ("stop", SIGSTOP);
#endif
#ifdef SIGTSTP
      parse_signal ("tstp", SIGTSTP);
#endif
#ifdef SIGCONT
      parse_signal ("cont", SIGCONT);
#endif
#ifdef SIGCHLD
      parse_signal ("chld", SIGCHLD);
#endif
#ifdef SIGTTIN
      parse_signal ("ttin", SIGTTIN);
#endif
#ifdef SIGTTOU
      parse_signal ("ttou", SIGTTOU);
#endif
#ifdef SIGIO
      parse_signal ("io", SIGIO);
#endif
#ifdef SIGXCPU
      parse_signal ("xcpu", SIGXCPU);
#endif
#ifdef SIGXFSZ
      parse_signal ("xfsz", SIGXFSZ);
#endif
#ifdef SIGVTALRM
      parse_signal ("vtalrm", SIGVTALRM);
#endif
#ifdef SIGPROF
      parse_signal ("prof", SIGPROF);
#endif
#ifdef SIGWINCH
      parse_signal ("winch", SIGWINCH);
#endif
#ifdef SIGINFO
      parse_signal ("info", SIGINFO);
#endif
      else
	error ("Undefined signal name %s", name);
    }

#undef parse_signal

  return make_number (kill (pid, XINT (sigcode)));
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
  coding = proc_encode_coding_system[XPROCESS (proc)->outfd];

  /* Make sure the process is really alive.  */
  if (XPROCESS (proc)->raw_status_new)
    update_status (XPROCESS (proc));
  if (! EQ (XPROCESS (proc)->status, Qrun))
    error ("Process %s not running", SDATA (XPROCESS (proc)->name));

  if (CODING_REQUIRE_FLUSHING (coding))
    {
      coding->mode |= CODING_MODE_LAST_BLOCK;
      send_process (proc, "", 0, Qnil);
    }

#ifdef VMS
  send_process (proc, "\032", 1, Qnil); 	/* ^z */
#else
  if (XPROCESS (proc)->pty_flag)
    send_process (proc, "\004", 1, Qnil);
  else
    {
      int old_outfd, new_outfd;

#ifdef HAVE_SHUTDOWN
      /* If this is a network connection, or socketpair is used
	 for communication with the subprocess, call shutdown to cause EOF.
	 (In some old system, shutdown to socketpair doesn't work.
	 Then we just can't win.)  */
      if (XPROCESS (proc)->pid == 0
	  || XPROCESS (proc)->outfd == XPROCESS (proc)->infd)
	shutdown (XPROCESS (proc)->outfd, 1);
      /* In case of socketpair, outfd == infd, so don't close it.  */
      if (XPROCESS (proc)->outfd != XPROCESS (proc)->infd)
	emacs_close (XPROCESS (proc)->outfd);
#else /* not HAVE_SHUTDOWN */
      emacs_close (XPROCESS (proc)->outfd);
#endif /* not HAVE_SHUTDOWN */
      new_outfd = emacs_open (NULL_DEVICE, O_WRONLY, 0);
      if (new_outfd < 0)
	abort ();
      old_outfd = XPROCESS (proc)->outfd;

      if (!proc_encode_coding_system[new_outfd])
	proc_encode_coding_system[new_outfd]
	  = (struct coding_system *) xmalloc (sizeof (struct coding_system));
      bcopy (proc_encode_coding_system[old_outfd],
	     proc_encode_coding_system[new_outfd],
	     sizeof (struct coding_system));
      bzero (proc_encode_coding_system[old_outfd],
	     sizeof (struct coding_system));

      XPROCESS (proc)->outfd = new_outfd;
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

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      proc = XCDR (XCAR (tail));
      if (PROCESSP (proc)
	  && (NILP (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	{
	  if (NETCONN_P (proc))
	    Fdelete_process (proc);
	  else if (XPROCESS (proc)->infd >= 0)
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
   Inc.

   ** Malloc WARNING: This should never call malloc either directly or
   indirectly; if it does, that is a bug  */

#ifdef SIGCHLD
SIGTYPE
sigchld_handler (signo)
     int signo;
{
  int old_errno = errno;
  Lisp_Object proc;
  register struct Lisp_Process *p;
  extern EMACS_TIME *input_available_clear_time;

  SIGNAL_THREAD_CHECK (signo);

#ifdef BSD4_1
  extern int sigheld;
  sigheld |= sigbit (SIGCHLD);
#endif

  while (1)
    {
      pid_t pid;
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

      /* The process can have been deleted by Fdelete_process.  */
      for (tail = deleted_pid_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object xpid = XCAR (tail);
	  if ((INTEGERP (xpid) && pid == (pid_t) XINT (xpid))
	      || (FLOATP (xpid) && pid == (pid_t) XFLOAT_DATA (xpid)))
	    {
	      XSETCAR (tail, Qnil);
	      goto sigchld_end_of_loop;
	    }
	}

      /* Otherwise, if it is asynchronous, it is in Vprocess_alist.  */
      p = 0;
      for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
	{
	  proc = XCDR (XCAR (tail));
	  p = XPROCESS (proc);
	  if (EQ (p->childp, Qt) && p->pid == pid)
	    break;
	  p = 0;
	}

      /* Look for an asynchronous process whose pid hasn't been filled
	 in yet.  */
      if (p == 0)
	for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
	  {
	    proc = XCDR (XCAR (tail));
	    p = XPROCESS (proc);
	    if (p->pid == -1)
	      break;
	    p = 0;
	  }

      /* Change the status of the process that was found.  */
      if (p != 0)
	{
	  union { int i; WAITTYPE wt; } u;
	  int clear_desc_flag = 0;

	  p->tick = ++process_tick;
	  u.wt = w;
	  p->raw_status = u.i;
	  p->raw_status_new = 1;

	  /* If process has terminated, stop waiting for its output.  */
	  if ((WIFSIGNALED (w) || WIFEXITED (w))
	      && p->infd >= 0)
	    clear_desc_flag = 1;

	  /* We use clear_desc_flag to avoid a compiler bug in Microsoft C.  */
	  if (clear_desc_flag)
	    {
	      FD_CLR (p->infd, &input_wait_mask);
	      FD_CLR (p->infd, &non_keyboard_wait_mask);
	    }

	  /* Tell wait_reading_process_output that it needs to wake up and
	     look around.  */
	  if (input_available_clear_time)
	    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
	}

      /* There was no asynchronous process found for that pid: we have
	 a synchronous process.  */
      else
	{
	  synch_process_alive = 0;

	  /* Report the status of the synchronous process.  */
	  if (WIFEXITED (w))
	    synch_process_retcode = WRETCODE (w);
	  else if (WIFSIGNALED (w))
            synch_process_termsig = WTERMSIG (w);

	  /* Tell wait_reading_process_output that it needs to wake up and
	     look around.  */
	  if (input_available_clear_time)
	    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
	}

    sigchld_end_of_loop:
      ;

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
#endif /* SIGCHLD */


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
  int count = SPECPDL_INDEX ();
  int outer_running_asynch_code = running_asynch_code;
  int waiting = waiting_for_user_input_p;

  if (inhibit_sentinels)
    return;

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
      tem = Fmatch_data (Qnil, Qnil, Qnil);
      restore_search_regs ();
      record_unwind_save_match_data ();
      Fset_match_data (tem, Qt);
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
  restore_search_regs ();
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
       cause trouble (for example it would make sit_for return).  */
    if (waiting_for_user_input_p == -1)
      record_asynch_buffer_change ();

  unbind_to (count, Qnil);
}

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is usually done while Emacs is waiting for keyboard input
   but can be done at other times.  */

static void
status_notify (deleting_process)
     struct Lisp_Process *deleting_process;
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

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object symbol;
      register struct Lisp_Process *p;

      proc = Fcdr (XCAR (tail));
      p = XPROCESS (proc);

      if (p->tick != p->update_tick)
	{
	  p->update_tick = p->tick;

	  /* If process is still active, read any output that remains.  */
	  while (! EQ (p->filter, Qt)
		 && ! EQ (p->status, Qconnect)
		 && ! EQ (p->status, Qlisten)
		 && ! EQ (p->command, Qt)  /* Network process not stopped.  */
		 && p->infd >= 0
		 && p != deleting_process
		 && read_process_output (proc, p->infd) > 0);

	  buffer = p->buffer;

	  /* Get the text to use for the message.  */
	  if (p->raw_status_new)
	    update_status (p);
	  msg = status_message (p);

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
	  p->update_tick = p->tick;
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
     (process, decoding, encoding)
     register Lisp_Object process, decoding, encoding;
{
  register struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  if (p->infd < 0)
    error ("Input file descriptor of %s closed", SDATA (p->name));
  if (p->outfd < 0)
    error ("Output file descriptor of %s closed", SDATA (p->name));
  Fcheck_coding_system (decoding);
  Fcheck_coding_system (encoding);
  encoding = coding_inherit_eol_type (encoding, Qnil);
  p->decode_coding_system = decoding;
  p->encode_coding_system = encoding;
  setup_process_coding_systems (process);

  return Qnil;
}

DEFUN ("process-coding-system",
       Fprocess_coding_system, Sprocess_coding_system, 1, 1, 0,
       doc: /* Return a cons of coding systems for decoding and encoding of PROCESS.  */)
     (process)
     register Lisp_Object process;
{
  CHECK_PROCESS (process);
  return Fcons (XPROCESS (process)->decode_coding_system,
		XPROCESS (process)->encode_coding_system);
}

DEFUN ("set-process-filter-multibyte", Fset_process_filter_multibyte,
       Sset_process_filter_multibyte, 2, 2, 0,
       doc: /* Set multibyteness of the strings given to PROCESS's filter.
If FLAG is non-nil, the filter is given multibyte strings.
If FLAG is nil, the filter is given unibyte strings.  In this case,
all character code conversion except for end-of-line conversion is
suppressed.  */)
     (process, flag)
     Lisp_Object process, flag;
{
  register struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  p->filter_multibyte = !NILP (flag);
  setup_process_coding_systems (process);

  return Qnil;
}

DEFUN ("process-filter-multibyte-p", Fprocess_filter_multibyte_p,
       Sprocess_filter_multibyte_p, 1, 1, 0,
       doc: /* Return t if a multibyte string is given to PROCESS's filter.*/)
     (process)
     Lisp_Object process;
{
  register struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);

  return (p->filter_multibyte ? Qt : Qnil);
}



/* Add DESC to the set of keyboard input descriptors.  */

void
add_keyboard_wait_descriptor (desc)
     int desc;
{
  FD_SET (desc, &input_wait_mask);
  FD_SET (desc, &non_process_wait_mask);
  if (desc > max_keyboard_desc)
    max_keyboard_desc = desc;
}

static int add_gpm_wait_descriptor_called_flag;

void
add_gpm_wait_descriptor (desc)
     int desc;
{
  if (! add_gpm_wait_descriptor_called_flag)
    FD_CLR (0, &input_wait_mask);
  add_gpm_wait_descriptor_called_flag = 1;
  FD_SET (desc, &input_wait_mask);
  FD_SET (desc, &gpm_wait_mask);
  if (desc > max_gpm_desc)
    max_gpm_desc = desc;
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
	  && !FD_ISSET (fd, &non_keyboard_wait_mask)
	  && !FD_ISSET (fd, &gpm_wait_mask))
	max_keyboard_desc = fd;
}

void
delete_gpm_wait_descriptor (desc)
     int desc;
{
  int fd;
  int lim = max_gpm_desc;

  FD_CLR (desc, &input_wait_mask);
  FD_CLR (desc, &non_process_wait_mask);

  if (desc == max_gpm_desc)
    for (fd = 0; fd < lim; fd++)
      if (FD_ISSET (fd, &input_wait_mask)
	  && !FD_ISSET (fd, &non_keyboard_wait_mask)
	  && !FD_ISSET (fd, &non_process_wait_mask))
	max_gpm_desc = fd;
}

/* Return nonzero if *MASK has a bit set
   that corresponds to one of the keyboard input descriptors.  */

static int
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

  inhibit_sentinels = 0;

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

#ifdef NON_BLOCKING_CONNECT
  FD_ZERO (&connect_wait_mask);
  num_pending_connects = 0;
#endif

#ifdef ADAPTIVE_READ_BUFFERING
  process_output_delay_count = 0;
  process_output_skip = 0;
#endif

  /* Don't do this, it caused infinite select loops.  The display
     method should call add_keyboard_wait_descriptor on stdin if it
     needs that.  */
#if 0
  FD_SET (0, &input_wait_mask);
#endif

  Vprocess_alist = Qnil;
#ifdef SIGCHLD
  deleted_pid_list = Qnil;
#endif
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
   struct socket_options *sopt;

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
   ADD_SUBFEATURE (QCfamily, Qipv4);
#ifdef AF_INET6
   ADD_SUBFEATURE (QCfamily, Qipv6);
#endif
#ifdef HAVE_GETSOCKNAME
   ADD_SUBFEATURE (QCservice, Qt);
#endif
#if !defined(TERM) && (defined(O_NONBLOCK) || defined(O_NDELAY))
   ADD_SUBFEATURE (QCserver, Qt);
#endif

   for (sopt = socket_options; sopt->name; sopt++)
     subfeatures = Fcons (intern (sopt->name), subfeatures);

   Fprovide (intern ("make-network-process"), subfeatures);
 }
#endif /* HAVE_SOCKETS */

#if defined (DARWIN) || defined (MAC_OSX)
  /* PTYs are broken on Darwin < 6, but are sometimes useful for interactive
     processes.  As such, we only change the default value.  */
 if (initialized)
  {
    char *release = get_operating_system_release();
    if (!release || !release[0] || (release[0] < MIN_PTY_KERNEL_VERSION
				    && release[1] == '.')) {
      Vprocess_connection_type = Qnil;
    }
  }
#endif
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
  Qipv4 = intern ("ipv4");
  staticpro (&Qipv4);
#ifdef AF_INET6
  Qipv6 = intern ("ipv6");
  staticpro (&Qipv6);
#endif
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
  QCplist = intern (":plist");
  staticpro (&QCplist);
  QCfilter_multibyte = intern (":filter-multibyte");
  staticpro (&QCfilter_multibyte);

  Qlast_nonmenu_event = intern ("last-nonmenu-event");
  staticpro (&Qlast_nonmenu_event);

  staticpro (&Vprocess_alist);
#ifdef SIGCHLD
  staticpro (&deleted_pid_list);
#endif

  DEFVAR_BOOL ("delete-exited-processes", &delete_exited_processes,
	       doc: /* *Non-nil means delete processes immediately when they exit.
A value of nil means don't delete them until `list-processes' is run.  */);

  delete_exited_processes = 1;

  DEFVAR_LISP ("process-connection-type", &Vprocess_connection_type,
	       doc: /* Control type of device used to communicate with subprocesses.
Values are nil to use a pipe, or t or `pty' to use a pty.
The value has no effect if the system has no ptys or if all ptys are busy:
then a pipe is used in any case.
The value takes effect when `start-process' is called.  */);
  Vprocess_connection_type = Qt;

#ifdef ADAPTIVE_READ_BUFFERING
  DEFVAR_LISP ("process-adaptive-read-buffering", &Vprocess_adaptive_read_buffering,
	       doc: /* If non-nil, improve receive buffering by delaying after short reads.
On some systems, when Emacs reads the output from a subprocess, the output data
is read in very small blocks, potentially resulting in very poor performance.
This behavior can be remedied to some extent by setting this variable to a
non-nil value, as it will automatically delay reading from such processes, to
allow them to produce more output before Emacs tries to read it.
If the value is t, the delay is reset after each write to the process; any other
non-nil value means that the delay is not reset on write.
The variable takes effect when `start-process' is called.  */);
  Vprocess_adaptive_read_buffering = Qt;
#endif

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
  defsubr (&Sprocess_plist);
  defsubr (&Sset_process_plist);
  defsubr (&Slist_processes);
  defsubr (&Sprocess_list);
  defsubr (&Sstart_process);
#ifdef HAVE_SOCKETS
  defsubr (&Sset_network_process_option);
  defsubr (&Smake_network_process);
  defsubr (&Sformat_network_address);
#endif /* HAVE_SOCKETS */
#if defined(HAVE_SOCKETS) && defined(HAVE_NET_IF_H) && defined(HAVE_SYS_IOCTL_H)
#ifdef SIOCGIFCONF
  defsubr (&Snetwork_interface_list);
#endif
#if defined(SIOCGIFADDR) || defined(SIOCGIFHWADDR) || defined(SIOCGIFFLAGS)
  defsubr (&Snetwork_interface_info);
#endif
#endif /* HAVE_SOCKETS ... */
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
  defsubr (&Sset_process_filter_multibyte);
  defsubr (&Sprocess_filter_multibyte_p);
}


#else /* not subprocesses */

#include <sys/types.h>
#include <errno.h>

#include "lisp.h"
#include "systime.h"
#include "character.h"
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

   see full version for other parameters. We know that wait_proc will
     always be NULL, since `subprocesses' isn't defined.

   do_display != 0 means redisplay should be done to show subprocess
   output that arrives.

   Return true if we received input from any process.  */

int
wait_reading_process_output (time_limit, microsecs, read_kbd, do_display,
			     wait_for_cell, wait_proc, just_wait_proc)
     int time_limit, microsecs, read_kbd, do_display;
     Lisp_Object wait_for_cell;
     struct Lisp_Process *wait_proc;
     int just_wait_proc;
{
  register int nfds;
  EMACS_TIME end_time, timeout;
  SELECT_TYPE waitchannels;
  int xerrno;

  /* What does time_limit really mean?  */
  if (time_limit || microsecs)
    {
      EMACS_GET_TIME (end_time);
      EMACS_SET_SECS_USECS (timeout, time_limit, microsecs);
      EMACS_ADD_TIME (end_time, end_time, timeout);
    }

  /* Turn off periodic alarms (in case they are in use)
     and then turn off any other atimers,
     because the select emulator uses alarms.  */
  stop_polling ();
  turn_on_atimers (0);

  while (1)
    {
      int timeout_reduced_for_timers = 0;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
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
	  if (read_kbd != 0
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
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* Wait till there is something to do.  */

      if (! read_kbd && NILP (wait_for_cell))
	FD_ZERO (&waitchannels);
      else
	FD_SET (0, &waitchannels);

      /* If a frame has been newly mapped and needs updating,
	 reprocess its display stuff.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (15);
	  if (read_kbd < 0)
	    set_waiting_for_input (&timeout);
	}

      if (read_kbd && detect_input_pending ())
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
      if (read_kbd && interrupt_input && (waitchannels & 1))
	kill (getpid (), SIGIO);
#endif

      /* Check for keyboard input */

      if (read_kbd
	  && detect_input_pending_run_timers (do_display))
	{
	  swallow_events (do_display);
	  if (detect_input_pending_run_timers (do_display))
	    break;
	}

      /* If there is unread keyboard input, also return.  */
      if (read_kbd
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

/* arch-tag: 3706c011-7b9a-4117-bd4f-59e7f701a4c4
   (do not change this comment) */
