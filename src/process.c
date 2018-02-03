/* Asynchronous subprocess control for GNU Emacs.

Copyright (C) 1985-1988, 1993-1996, 1998-1999, 2001-2018 Free Software
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


#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>		/* Some typedefs are used in sys/file.h.  */
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "lisp.h"

/* Only MS-DOS does not define `subprocesses'.  */
#ifdef subprocesses

#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif	/* subprocesses */

#ifdef HAVE_SETRLIMIT
# include <sys/resource.h>

/* If NOFILE_LIMIT.rlim_cur is greater than FD_SETSIZE, then
   NOFILE_LIMIT is the initial limit on the number of open files,
   which should be restored in child processes.  */
static struct rlimit nofile_limit;
#endif

#ifdef subprocesses

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

#include <sys/ioctl.h>
#if defined (HAVE_NET_IF_H)
#include <net/if.h>
#endif /* HAVE_NET_IF_H */

#if defined (HAVE_IFADDRS_H)
/* Must be after net/if.h */
#include <ifaddrs.h>

/* We only use structs from this header when we use getifaddrs.  */
#if defined (HAVE_NET_IF_DL_H)
#include <net/if_dl.h>
#endif

#endif

#ifdef NEED_BSDTTY
#include <bsdtty.h>
#endif

#ifdef USG5_4
# include <sys/stream.h>
# include <sys/stropts.h>
#endif

#ifdef HAVE_UTIL_H
#include <util.h>
#endif

#ifdef HAVE_PTY_H
#include <pty.h>
#endif

#include <c-ctype.h>
#include <flexmember.h>
#include <sig2str.h>
#include <verify.h>

#endif	/* subprocesses */

#include "systime.h"
#include "systty.h"

#include "window.h"
#include "character.h"
#include "buffer.h"
#include "coding.h"
#include "process.h"
#include "frame.h"
#include "termopts.h"
#include "keyboard.h"
#include "blockinput.h"
#include "atimer.h"
#include "sysselect.h"
#include "syssignal.h"
#include "syswait.h"
#ifdef HAVE_GNUTLS
#include "gnutls.h"
#endif

#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_GLIB
#include "xgselect.h"
#ifndef WINDOWSNT
#include <glib.h>
#endif
#endif

#if defined HAVE_GETADDRINFO_A || defined HAVE_GNUTLS
/* This is 0.1s in nanoseconds. */
#define ASYNC_RETRY_NSEC 100000000
#endif

#ifdef WINDOWSNT
extern int sys_select (int, fd_set *, fd_set *, fd_set *,
                       const struct timespec *, const sigset_t *);
#endif

/* Work around GCC 4.3.0 bug with strict overflow checking; see
   <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52904>.
   This bug appears to be fixed in GCC 5.1, so don't work around it there.  */
#if GNUC_PREREQ (4, 3, 0) && ! GNUC_PREREQ (5, 1, 0)
# pragma GCC diagnostic ignored "-Wstrict-overflow"
#endif

/* True if keyboard input is on hold, zero otherwise.  */

static bool kbd_is_on_hold;

/* Nonzero means don't run process sentinels.  This is used
   when exiting.  */
bool inhibit_sentinels;

#ifdef subprocesses

#ifndef SOCK_CLOEXEC
# define SOCK_CLOEXEC 0
#endif
#ifndef SOCK_NONBLOCK
# define SOCK_NONBLOCK 0
#endif

/* True if ERRNUM represents an error where the system call would
   block if a blocking variant were used.  */
static bool
would_block (int errnum)
{
#ifdef EWOULDBLOCK
  if (EWOULDBLOCK != EAGAIN && errnum == EWOULDBLOCK)
    return true;
#endif
  return errnum == EAGAIN;
}

#ifndef HAVE_ACCEPT4

/* Emulate GNU/Linux accept4 and socket well enough for this module.  */

static int
close_on_exec (int fd)
{
  if (0 <= fd)
    fcntl (fd, F_SETFD, FD_CLOEXEC);
  return fd;
}

# undef accept4
# define accept4(sockfd, addr, addrlen, flags) \
    process_accept4 (sockfd, addr, addrlen, flags)
static int
accept4 (int sockfd, struct sockaddr *addr, socklen_t *addrlen, int flags)
{
  return close_on_exec (accept (sockfd, addr, addrlen));
}

static int
process_socket (int domain, int type, int protocol)
{
  return close_on_exec (socket (domain, type, protocol));
}
# undef socket
# define socket(domain, type, protocol) process_socket (domain, type, protocol)
#endif

#define NETCONN_P(p) (EQ (XPROCESS (p)->type, Qnetwork))
#define NETCONN1_P(p) (EQ (p->type, Qnetwork))
#define SERIALCONN_P(p) (EQ (XPROCESS (p)->type, Qserial))
#define SERIALCONN1_P(p) (EQ (p->type, Qserial))
#define PIPECONN_P(p) (EQ (XPROCESS (p)->type, Qpipe))
#define PIPECONN1_P(p) (EQ (p->type, Qpipe))

/* Number of events of change of status of a process.  */
static EMACS_INT process_tick;
/* Number of events for which the user or sentinel has been notified.  */
static EMACS_INT update_tick;

/* Define DATAGRAM_SOCKETS if datagrams can be used safely on
   this system.  We need to read full packets, so we need a
   "non-destructive" select.  So we require either native select,
   or emulation of select using FIONREAD.  */

#ifndef BROKEN_DATAGRAM_SOCKETS
# if defined HAVE_SELECT || defined USABLE_FIONREAD
#  if defined HAVE_SENDTO && defined HAVE_RECVFROM && defined EMSGSIZE
#   define DATAGRAM_SOCKETS
#  endif
# endif
#endif

#if defined HAVE_LOCAL_SOCKETS && defined DATAGRAM_SOCKETS
# define HAVE_SEQPACKET
#endif

#define READ_OUTPUT_DELAY_INCREMENT (TIMESPEC_RESOLUTION / 100)
#define READ_OUTPUT_DELAY_MAX       (READ_OUTPUT_DELAY_INCREMENT * 5)
#define READ_OUTPUT_DELAY_MAX_MAX   (READ_OUTPUT_DELAY_INCREMENT * 7)

/* Number of processes which have a non-zero read_output_delay,
   and therefore might be delayed for adaptive read buffering.  */

static int process_output_delay_count;

/* True if any process has non-nil read_output_skip.  */

static bool process_output_skip;

static void start_process_unwind (Lisp_Object);
static void create_process (Lisp_Object, char **, Lisp_Object);
#ifdef USABLE_SIGIO
static bool keyboard_bit_set (fd_set *);
#endif
static void deactivate_process (Lisp_Object);
static int status_notify (struct Lisp_Process *, struct Lisp_Process *);
static int read_process_output (Lisp_Object, int);
static void create_pty (Lisp_Object);
static void exec_sentinel (Lisp_Object, Lisp_Object);

/* Number of bits set in connect_wait_mask.  */
static int num_pending_connects;

/* The largest descriptor currently in use; -1 if none.  */
static int max_desc;

/* Set the external socket descriptor for Emacs to use when
   `make-network-process' is called with a non-nil
   `:use-external-socket' option.  The value should be either -1, or
   the file descriptor of a socket that is already bound.  */
static int external_sock_fd;

/* Indexed by descriptor, gives the process (if any) for that descriptor.  */
static Lisp_Object chan_process[FD_SETSIZE];
static void wait_for_socket_fds (Lisp_Object, char const *);

/* Alist of elements (NAME . PROCESS).  */
static Lisp_Object Vprocess_alist;

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */

static int proc_buffered_char[FD_SETSIZE];

/* Table of `struct coding-system' for each process.  */
static struct coding_system *proc_decode_coding_system[FD_SETSIZE];
static struct coding_system *proc_encode_coding_system[FD_SETSIZE];

#ifdef DATAGRAM_SOCKETS
/* Table of `partner address' for datagram sockets.  */
static struct sockaddr_and_len {
  struct sockaddr *sa;
  ptrdiff_t len;
} datagram_address[FD_SETSIZE];
#define DATAGRAM_CHAN_P(chan)	(datagram_address[chan].sa != 0)
#define DATAGRAM_CONN_P(proc)                                           \
  (PROCESSP (proc) &&                                                   \
   XPROCESS (proc)->infd >= 0 &&                                        \
   datagram_address[XPROCESS (proc)->infd].sa != 0)
#else
#define DATAGRAM_CONN_P(proc)	(0)
#endif

/* FOR_EACH_PROCESS (LIST_VAR, PROC_VAR) followed by a statement is
   a `for' loop which iterates over processes from Vprocess_alist.  */

#define FOR_EACH_PROCESS(list_var, proc_var)			\
  FOR_EACH_ALIST_VALUE (Vprocess_alist, list_var, proc_var)

/* These setters are used only in this file, so they can be private.  */
static void
pset_buffer (struct Lisp_Process *p, Lisp_Object val)
{
  p->buffer = val;
}
static void
pset_command (struct Lisp_Process *p, Lisp_Object val)
{
  p->command = val;
}
static void
pset_decode_coding_system (struct Lisp_Process *p, Lisp_Object val)
{
  p->decode_coding_system = val;
}
static void
pset_decoding_buf (struct Lisp_Process *p, Lisp_Object val)
{
  p->decoding_buf = val;
}
static void
pset_encode_coding_system (struct Lisp_Process *p, Lisp_Object val)
{
  p->encode_coding_system = val;
}
static void
pset_encoding_buf (struct Lisp_Process *p, Lisp_Object val)
{
  p->encoding_buf = val;
}
static void
pset_filter (struct Lisp_Process *p, Lisp_Object val)
{
  p->filter = NILP (val) ? Qinternal_default_process_filter : val;
}
static void
pset_log (struct Lisp_Process *p, Lisp_Object val)
{
  p->log = val;
}
static void
pset_mark (struct Lisp_Process *p, Lisp_Object val)
{
  p->mark = val;
}
static void
pset_thread (struct Lisp_Process *p, Lisp_Object val)
{
  p->thread = val;
}
static void
pset_name (struct Lisp_Process *p, Lisp_Object val)
{
  p->name = val;
}
static void
pset_plist (struct Lisp_Process *p, Lisp_Object val)
{
  p->plist = val;
}
static void
pset_sentinel (struct Lisp_Process *p, Lisp_Object val)
{
  p->sentinel = NILP (val) ? Qinternal_default_process_sentinel : val;
}
static void
pset_tty_name (struct Lisp_Process *p, Lisp_Object val)
{
  p->tty_name = val;
}
static void
pset_type (struct Lisp_Process *p, Lisp_Object val)
{
  p->type = val;
}
static void
pset_write_queue (struct Lisp_Process *p, Lisp_Object val)
{
  p->write_queue = val;
}
static void
pset_stderrproc (struct Lisp_Process *p, Lisp_Object val)
{
  p->stderrproc = val;
}


static Lisp_Object
make_lisp_proc (struct Lisp_Process *p)
{
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

enum fd_bits
{
  /* Read from file descriptor.  */
  FOR_READ = 1,
  /* Write to file descriptor.  */
  FOR_WRITE = 2,
  /* This descriptor refers to a keyboard.  Only valid if FOR_READ is
     set.  */
  KEYBOARD_FD = 4,
  /* This descriptor refers to a process.  */
  PROCESS_FD = 8,
  /* A non-blocking connect.  Only valid if FOR_WRITE is set.  */
  NON_BLOCKING_CONNECT_FD = 16
};

static struct fd_callback_data
{
  fd_callback func;
  void *data;
  /* Flags from enum fd_bits.  */
  int flags;
  /* If this fd is locked to a certain thread, this points to it.
     Otherwise, this is NULL.  If an fd is locked to a thread, then
     only that thread is permitted to wait on it.  */
  struct thread_state *thread;
  /* If this fd is currently being selected on by a thread, this
     points to the thread.  Otherwise it is NULL.  */
  struct thread_state *waiting_thread;
} fd_callback_info[FD_SETSIZE];


/* Add a file descriptor FD to be monitored for when read is possible.
   When read is possible, call FUNC with argument DATA.  */

void
add_read_fd (int fd, fd_callback func, void *data)
{
  add_keyboard_wait_descriptor (fd);

  fd_callback_info[fd].func = func;
  fd_callback_info[fd].data = data;
}

static void
add_non_keyboard_read_fd (int fd)
{
  eassert (fd >= 0 && fd < FD_SETSIZE);
  eassert (fd_callback_info[fd].func == NULL);

  fd_callback_info[fd].flags &= ~KEYBOARD_FD;
  fd_callback_info[fd].flags |= FOR_READ;
  if (fd > max_desc)
    max_desc = fd;
}

static void
add_process_read_fd (int fd)
{
  add_non_keyboard_read_fd (fd);
  fd_callback_info[fd].flags |= PROCESS_FD;
}

/* Stop monitoring file descriptor FD for when read is possible.  */

void
delete_read_fd (int fd)
{
  delete_keyboard_wait_descriptor (fd);

  if (fd_callback_info[fd].flags == 0)
    {
      fd_callback_info[fd].func = 0;
      fd_callback_info[fd].data = 0;
    }
}

/* Add a file descriptor FD to be monitored for when write is possible.
   When write is possible, call FUNC with argument DATA.  */

void
add_write_fd (int fd, fd_callback func, void *data)
{
  eassert (fd >= 0 && fd < FD_SETSIZE);

  fd_callback_info[fd].func = func;
  fd_callback_info[fd].data = data;
  fd_callback_info[fd].flags |= FOR_WRITE;
  if (fd > max_desc)
    max_desc = fd;
}

static void
add_non_blocking_write_fd (int fd)
{
  eassert (fd >= 0 && fd < FD_SETSIZE);
  eassert (fd_callback_info[fd].func == NULL);

  fd_callback_info[fd].flags |= FOR_WRITE | NON_BLOCKING_CONNECT_FD;
  if (fd > max_desc)
    max_desc = fd;
  ++num_pending_connects;
}

static void
recompute_max_desc (void)
{
  int fd;

  for (fd = max_desc; fd >= 0; --fd)
    {
      if (fd_callback_info[fd].flags != 0)
	{
	  max_desc = fd;
	  break;
	}
    }
}

/* Stop monitoring file descriptor FD for when write is possible.  */

void
delete_write_fd (int fd)
{
  if ((fd_callback_info[fd].flags & NON_BLOCKING_CONNECT_FD) != 0)
    {
      if (--num_pending_connects < 0)
	emacs_abort ();
    }
  fd_callback_info[fd].flags &= ~(FOR_WRITE | NON_BLOCKING_CONNECT_FD);
  if (fd_callback_info[fd].flags == 0)
    {
      fd_callback_info[fd].func = 0;
      fd_callback_info[fd].data = 0;

      if (fd == max_desc)
	recompute_max_desc ();
    }
}

static void
compute_input_wait_mask (fd_set *mask)
{
  int fd;

  FD_ZERO (mask);
  for (fd = 0; fd <= max_desc; ++fd)
    {
      if (fd_callback_info[fd].thread != NULL
	  && fd_callback_info[fd].thread != current_thread)
	continue;
      if (fd_callback_info[fd].waiting_thread != NULL
	  && fd_callback_info[fd].waiting_thread != current_thread)
	continue;
      if ((fd_callback_info[fd].flags & FOR_READ) != 0)
	{
	  FD_SET (fd, mask);
	  fd_callback_info[fd].waiting_thread = current_thread;
	}
    }
}

static void
compute_non_process_wait_mask (fd_set *mask)
{
  int fd;

  FD_ZERO (mask);
  for (fd = 0; fd <= max_desc; ++fd)
    {
      if (fd_callback_info[fd].thread != NULL
	  && fd_callback_info[fd].thread != current_thread)
	continue;
      if (fd_callback_info[fd].waiting_thread != NULL
	  && fd_callback_info[fd].waiting_thread != current_thread)
	continue;
      if ((fd_callback_info[fd].flags & FOR_READ) != 0
	  && (fd_callback_info[fd].flags & PROCESS_FD) == 0)
	{
	  FD_SET (fd, mask);
	  fd_callback_info[fd].waiting_thread = current_thread;
	}
    }
}

static void
compute_non_keyboard_wait_mask (fd_set *mask)
{
  int fd;

  FD_ZERO (mask);
  for (fd = 0; fd <= max_desc; ++fd)
    {
      if (fd_callback_info[fd].thread != NULL
	  && fd_callback_info[fd].thread != current_thread)
	continue;
      if (fd_callback_info[fd].waiting_thread != NULL
	  && fd_callback_info[fd].waiting_thread != current_thread)
	continue;
      if ((fd_callback_info[fd].flags & FOR_READ) != 0
	  && (fd_callback_info[fd].flags & KEYBOARD_FD) == 0)
	{
	  FD_SET (fd, mask);
	  fd_callback_info[fd].waiting_thread = current_thread;
	}
    }
}

static void
compute_write_mask (fd_set *mask)
{
  int fd;

  FD_ZERO (mask);
  for (fd = 0; fd <= max_desc; ++fd)
    {
      if (fd_callback_info[fd].thread != NULL
	  && fd_callback_info[fd].thread != current_thread)
	continue;
      if (fd_callback_info[fd].waiting_thread != NULL
	  && fd_callback_info[fd].waiting_thread != current_thread)
	continue;
      if ((fd_callback_info[fd].flags & FOR_WRITE) != 0)
	{
	  FD_SET (fd, mask);
	  fd_callback_info[fd].waiting_thread = current_thread;
	}
    }
}

static void
clear_waiting_thread_info (void)
{
  int fd;

  for (fd = 0; fd <= max_desc; ++fd)
    {
      if (fd_callback_info[fd].waiting_thread == current_thread)
	fd_callback_info[fd].waiting_thread = NULL;
    }
}


/* Compute the Lisp form of the process status, p->status, from
   the numeric status that was returned by `wait'.  */

static Lisp_Object status_convert (int);

static void
update_status (struct Lisp_Process *p)
{
  eassert (p->raw_status_new);
  pset_status (p, status_convert (p->raw_status));
  p->raw_status_new = 0;
}

/*  Convert a process status word in Unix format to
    the list that we use internally.  */

static Lisp_Object
status_convert (int w)
{
  if (WIFSTOPPED (w))
    return Fcons (Qstop, Fcons (make_number (WSTOPSIG (w)), Qnil));
  else if (WIFEXITED (w))
    return Fcons (Qexit, Fcons (make_number (WEXITSTATUS (w)),
				WCOREDUMP (w) ? Qt : Qnil));
  else if (WIFSIGNALED (w))
    return Fcons (Qsignal, Fcons (make_number (WTERMSIG (w)),
				  WCOREDUMP (w) ? Qt : Qnil));
  else
    return Qrun;
}

/* True if STATUS is that of a process attempting connection.  */

static bool
connecting_status (Lisp_Object status)
{
  return CONSP (status) && EQ (XCAR (status), Qconnect);
}

/* Given a status-list, extract the three pieces of information
   and store them individually through the three pointers.  */

static void
decode_status (Lisp_Object l, Lisp_Object *symbol, Lisp_Object *code,
	       bool *coredump)
{
  Lisp_Object tem;

  if (connecting_status (l))
    l = XCAR (l);

  if (SYMBOLP (l))
    {
      *symbol = l;
      *code = make_number (0);
      *coredump = 0;
    }
  else
    {
      *symbol = XCAR (l);
      tem = XCDR (l);
      *code = XCAR (tem);
      tem = XCDR (tem);
      *coredump = !NILP (tem);
    }
}

/* Return a string describing a process status list.  */

static Lisp_Object
status_message (struct Lisp_Process *p)
{
  Lisp_Object status = p->status;
  Lisp_Object symbol, code;
  bool coredump;
  Lisp_Object string;

  decode_status (status, &symbol, &code, &coredump);

  if (EQ (symbol, Qsignal) || EQ (symbol, Qstop))
    {
      char const *signame;
      synchronize_system_messages_locale ();
      signame = strsignal (XFASTINT (code));
      if (signame == 0)
	string = build_string ("unknown");
      else
	{
	  int c1, c2;

	  string = build_unibyte_string (signame);
	  if (! NILP (Vlocale_coding_system))
	    string = (code_convert_string_norecord
		      (string, Vlocale_coding_system, 0));
	  c1 = STRING_CHAR (SDATA (string));
	  c2 = downcase (c1);
	  if (c1 != c2)
	    Faset (string, make_number (0), make_number (c2));
	}
      AUTO_STRING (suffix, coredump ? " (core dumped)\n" : "\n");
      return concat2 (string, suffix);
    }
  else if (EQ (symbol, Qexit))
    {
      if (NETCONN1_P (p))
	return build_string (XFASTINT (code) == 0
			     ? "deleted\n"
			     : "connection broken by remote peer\n");
      if (XFASTINT (code) == 0)
	return build_string ("finished\n");
      AUTO_STRING (prefix, "exited abnormally with code ");
      string = Fnumber_to_string (code);
      AUTO_STRING (suffix, coredump ? " (core dumped)\n" : "\n");
      return concat3 (prefix, string, suffix);
    }
  else if (EQ (symbol, Qfailed))
    {
      AUTO_STRING (format, "failed with code %s\n");
      return CALLN (Fformat, format, code);
    }
  else
    return Fcopy_sequence (Fsymbol_name (symbol));
}

enum { PTY_NAME_SIZE = 24 };

/* Open an available pty, returning a file descriptor.
   Store into PTY_NAME the file name of the terminal corresponding to the pty.
   Return -1 on failure.  */

static int
allocate_pty (char pty_name[PTY_NAME_SIZE])
{
#ifdef HAVE_PTYS
  int fd;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  register int c, i;
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
	fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
	    sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* no PTY_TTY_NAME_SPRINTF */

	    /* Set FD's close-on-exec flag.  This is needed even if
	       PT_OPEN calls posix_openpt with O_CLOEXEC, since POSIX
	       doesn't require support for that combination.
	       Do this after PTY_TTY_NAME_SPRINTF, which on some platforms
	       doesn't work if the close-on-exec flag is set (Bug#20555).
	       Multithreaded platforms where posix_openpt ignores
	       O_CLOEXEC (or where PTY_OPEN doesn't call posix_openpt)
	       have a race condition between the PTY_OPEN and here.  */
	    fcntl (fd, F_SETFD, FD_CLOEXEC);

	    /* Check to make certain that both sides are available.
	       This avoids a nasty yet stupid bug in rlogins.  */
	    if (faccessat (AT_FDCWD, pty_name, R_OK | W_OK, AT_EACCESS) != 0)
	      {
		emacs_close (fd);
		continue;
	      }
	    setup_pty (fd);
	    return fd;
	  }
      }
#endif /* HAVE_PTYS */
  return -1;
}

/* Allocate basically initialized process.  */

static struct Lisp_Process *
allocate_process (void)
{
  return ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Process, pid, PVEC_PROCESS);
}

static Lisp_Object
make_process (Lisp_Object name)
{
  struct Lisp_Process *p = allocate_process ();
  /* Initialize Lisp data.  Note that allocate_process initializes all
     Lisp data to nil, so do it only for slots which should not be nil.  */
  pset_status (p, Qrun);
  pset_mark (p, Fmake_marker ());
  pset_thread (p, Fcurrent_thread ());

  /* Initialize non-Lisp data.  Note that allocate_process zeroes out all
     non-Lisp data, so do it only for slots which should not be zero.  */
  p->infd = -1;
  p->outfd = -1;
  for (int i = 0; i < PROCESS_OPEN_FDS; i++)
    p->open_fd[i] = -1;

#ifdef HAVE_GNUTLS
  verify (GNUTLS_STAGE_EMPTY == 0);
  eassert (p->gnutls_initstage == GNUTLS_STAGE_EMPTY);
  eassert (NILP (p->gnutls_boot_parameters));
#endif

  /* If name is already in use, modify it until it is unused.  */

  Lisp_Object name1 = name;
  for (printmax_t i = 1; ; i++)
    {
      Lisp_Object tem = Fget_process (name1);
      if (NILP (tem))
	break;
      char const suffix_fmt[] = "<%"pMd">";
      char suffix[sizeof suffix_fmt + INT_STRLEN_BOUND (printmax_t)];
      AUTO_STRING_WITH_LEN (lsuffix, suffix, sprintf (suffix, suffix_fmt, i));
      name1 = concat2 (name, lsuffix);
    }
  name = name1;
  pset_name (p, name);
  pset_sentinel (p, Qinternal_default_process_sentinel);
  pset_filter (p, Qinternal_default_process_filter);
  Lisp_Object val;
  XSETPROCESS (val, p);
  Vprocess_alist = Fcons (Fcons (name, val), Vprocess_alist);
  return val;
}

static void
remove_process (register Lisp_Object proc)
{
  register Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);

  deactivate_process (proc);
}

void
update_processes_for_thread_death (Lisp_Object dying_thread)
{
  Lisp_Object pair;

  for (pair = Vprocess_alist; !NILP (pair); pair = XCDR (pair))
    {
      Lisp_Object process = XCDR (XCAR (pair));
      if (EQ (XPROCESS (process)->thread, dying_thread))
	{
	  struct Lisp_Process *proc = XPROCESS (process);

	  pset_thread (proc, Qnil);
	  if (proc->infd >= 0)
	    fd_callback_info[proc->infd].thread = NULL;
	  if (proc->outfd >= 0)
	    fd_callback_info[proc->outfd].thread = NULL;
	}
    }
}

#ifdef HAVE_GETADDRINFO_A
static void
free_dns_request (Lisp_Object proc)
{
  struct Lisp_Process *p = XPROCESS (proc);

  if (p->dns_request->ar_result)
    freeaddrinfo (p->dns_request->ar_result);
  xfree (p->dns_request);
  p->dns_request = NULL;
}
#endif


DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
       doc: /* Return t if OBJECT is a process.  */)
  (Lisp_Object object)
{
  return PROCESSP (object) ? Qt : Qnil;
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
       doc: /* Return the process named NAME, or nil if there is none.  */)
  (register Lisp_Object name)
{
  if (PROCESSP (name))
    return name;
  CHECK_STRING (name);
  return Fcdr (Fassoc (name, Vprocess_alist, Qnil));
}

/* This is how commands for the user decode process arguments.  It
   accepts a process, a process name, a buffer, a buffer name, or nil.
   Buffers denote the first process in the buffer, and nil denotes the
   current buffer.  */

static Lisp_Object
get_process (register Lisp_Object name)
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

  /* Now obj should be either a buffer object or a process object.  */
  if (BUFFERP (obj))
    {
      if (NILP (BVAR (XBUFFER (obj), name)))
        error ("Attempt to get process for a dead buffer");
      proc = Fget_buffer_process (obj);
      if (NILP (proc))
        error ("Buffer %s has no process", SDATA (BVAR (XBUFFER (obj), name)));
    }
  else
    {
      CHECK_PROCESS (obj);
      proc = obj;
    }
  return proc;
}


/* Fdelete_process promises to immediately forget about the process, but in
   reality, Emacs needs to remember those processes until they have been
   treated by the SIGCHLD handler and waitpid has been invoked on them;
   otherwise they might fill up the kernel's process table.

   Some processes created by call-process are also put onto this list.

   Members of this list are (process-ID . filename) pairs.  The
   process-ID is a number; the filename, if a string, is a file that
   needs to be removed after the process exits.  */
static Lisp_Object deleted_pid_list;

void
record_deleted_pid (pid_t pid, Lisp_Object filename)
{
  deleted_pid_list = Fcons (Fcons (make_fixnum_or_float (pid), filename),
			    /* GC treated elements set to nil.  */
			    Fdelq (Qnil, deleted_pid_list));

}

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
       doc: /* Delete PROCESS: kill it and forget about it immediately.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.  */)
  (register Lisp_Object process)
{
  register struct Lisp_Process *p;

  process = get_process (process);
  p = XPROCESS (process);

#ifdef HAVE_GETADDRINFO_A
  if (p->dns_request)
    {
      /* Cancel the request.  Unless shutting down, wait until
	 completion.  Free the request if completely canceled. */

      bool canceled = gai_cancel (p->dns_request) != EAI_NOTCANCELED;
      if (!canceled && !inhibit_sentinels)
	{
	  struct gaicb const *req = p->dns_request;
	  while (gai_suspend (&req, 1, NULL) != 0)
	    continue;
	  canceled = true;
	}
      if (canceled)
	free_dns_request (process);
    }
#endif

  p->raw_status_new = 0;
  if (NETCONN1_P (p) || SERIALCONN1_P (p) || PIPECONN1_P (p))
    {
      pset_status (p, list2 (Qexit, make_number (0)));
      p->tick = ++process_tick;
      status_notify (p, NULL);
      redisplay_preserve_echo_area (13);
    }
  else
    {
      if (p->alive)
	record_kill_process (p, Qnil);

      if (p->infd >= 0)
	{
	  /* Update P's status, since record_kill_process will make the
	     SIGCHLD handler update deleted_pid_list, not *P.  */
	  Lisp_Object symbol;
	  if (p->raw_status_new)
	    update_status (p);
	  symbol = CONSP (p->status) ? XCAR (p->status) : p->status;
	  if (! (EQ (symbol, Qsignal) || EQ (symbol, Qexit)))
	    pset_status (p, list2 (Qsignal, make_number (SIGKILL)));

	  p->tick = ++process_tick;
	  status_notify (p, NULL);
	  redisplay_preserve_echo_area (13);
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
  (register Lisp_Object process)
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
  if (NETCONN1_P (p) || SERIALCONN1_P (p) || PIPECONN1_P (p))
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
  (register Lisp_Object process)
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
For a network, serial, and pipe connections, this value is nil.  */)
  (register Lisp_Object process)
{
  pid_t pid;

  CHECK_PROCESS (process);
  pid = XPROCESS (process)->pid;
  return (pid ? make_fixnum_or_float (pid) : Qnil);
}

DEFUN ("process-name", Fprocess_name, Sprocess_name, 1, 1, 0,
       doc: /* Return the name of PROCESS, as a string.
This is the name of the program invoked in PROCESS,
possibly modified to make it unique among process names.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->name;
}

DEFUN ("process-command", Fprocess_command, Sprocess_command, 1, 1, 0,
       doc: /* Return the command that was executed to start PROCESS.
This is a list of strings, the first string being the program executed
and the rest of the strings being the arguments given to it.
For a network or serial or pipe connection, this is nil (process is running)
or t (process is stopped).  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->command;
}

DEFUN ("process-tty-name", Fprocess_tty_name, Sprocess_tty_name, 1, 1, 0,
       doc: /* Return the name of the terminal PROCESS uses, or nil if none.
This is the terminal that the process itself reads and writes on,
not the name of the pty that Emacs uses to talk with that terminal.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->tty_name;
}

DEFUN ("set-process-buffer", Fset_process_buffer, Sset_process_buffer,
       2, 2, 0,
       doc: /* Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
Return BUFFER.  */)
  (register Lisp_Object process, Lisp_Object buffer)
{
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer);
  p = XPROCESS (process);
  pset_buffer (p, buffer);
  if (NETCONN1_P (p) || SERIALCONN1_P (p) || PIPECONN1_P (p))
    pset_childp (p, Fplist_put (p->childp, QCbuffer, buffer));
  setup_process_coding_systems (process);
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, Sprocess_buffer,
       1, 1, 0,
       doc: /* Return the buffer PROCESS is associated with.
The default process filter inserts output from PROCESS into this buffer.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, Sprocess_mark,
       1, 1, 0,
       doc: /* Return the marker for the end of the last output from PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->mark;
}

static void
set_process_filter_masks (struct Lisp_Process *p)
{
  if (EQ (p->filter, Qt) && !EQ (p->status, Qlisten))
    delete_read_fd (p->infd);
  else if (EQ (p->filter, Qt)
	   /* Network or serial process not stopped:  */
	   && !EQ (p->command, Qt))
    add_process_read_fd (p->infd);
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
       2, 2, 0,
       doc: /* Give PROCESS the filter function FILTER; nil means default.
A value of t means stop accepting output from the process.

When a process has a non-default filter, its buffer is not used for output.
Instead, each time it does output, the entire string of output is
passed to the filter.

The filter gets two arguments: the process and the string of output.
The string argument is normally a multibyte string, except:
- if the process's input coding system is no-conversion or raw-text,
  it is a unibyte string (the non-converted input).  */)
  (Lisp_Object process, Lisp_Object filter)
{
  CHECK_PROCESS (process);
  struct Lisp_Process *p = XPROCESS (process);

  /* Don't signal an error if the process's input file descriptor
     is closed.  This could make debugging Lisp more difficult,
     for example when doing something like

     (setq process (start-process ...))
     (debug)
     (set-process-filter process ...)  */

  if (NILP (filter))
    filter = Qinternal_default_process_filter;

  pset_filter (p, filter);

  if (p->infd >= 0)
    set_process_filter_masks (p);

  if (NETCONN1_P (p) || SERIALCONN1_P (p) || PIPECONN1_P (p))
    pset_childp (p, Fplist_put (p->childp, QCfilter, filter));
  setup_process_coding_systems (process);
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, Sprocess_filter,
       1, 1, 0,
       doc: /* Return the filter function of PROCESS.
See `set-process-filter' for more info on filter functions.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->filter;
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, Sset_process_sentinel,
       2, 2, 0,
       doc: /* Give PROCESS the sentinel SENTINEL; nil for default.
The sentinel is called as a function when the process changes state.
It gets two arguments: the process, and a string describing the change.  */)
  (register Lisp_Object process, Lisp_Object sentinel)
{
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);

  if (NILP (sentinel))
    sentinel = Qinternal_default_process_sentinel;

  pset_sentinel (p, sentinel);
  if (NETCONN1_P (p) || SERIALCONN1_P (p) || PIPECONN1_P (p))
    pset_childp (p, Fplist_put (p->childp, QCsentinel, sentinel));
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, Sprocess_sentinel,
       1, 1, 0,
       doc: /* Return the sentinel of PROCESS.
See `set-process-sentinel' for more info on sentinels.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->sentinel;
}

DEFUN ("set-process-thread", Fset_process_thread, Sset_process_thread,
       2, 2, 0,
       doc: /* Set the locking thread of PROCESS to be THREAD.
If THREAD is nil, the process is unlocked.  */)
  (Lisp_Object process, Lisp_Object thread)
{
  struct Lisp_Process *proc;
  struct thread_state *tstate;

  CHECK_PROCESS (process);
  if (NILP (thread))
    tstate = NULL;
  else
    {
      CHECK_THREAD (thread);
      tstate = XTHREAD (thread);
    }

  proc = XPROCESS (process);
  pset_thread (proc, thread);
  if (proc->infd >= 0)
    fd_callback_info[proc->infd].thread = tstate;
  if (proc->outfd >= 0)
    fd_callback_info[proc->outfd].thread = tstate;

  return thread;
}

DEFUN ("process-thread", Fprocess_thread, Sprocess_thread,
       1, 1, 0,
       doc: /* Ret the locking thread of PROCESS.
If PROCESS is unlocked, this function returns nil.  */)
  (Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->thread;
}

DEFUN ("set-process-window-size", Fset_process_window_size,
       Sset_process_window_size, 3, 3, 0,
       doc: /* Tell PROCESS that it has logical window size WIDTH by HEIGHT.
Value is t if PROCESS was successfully told about the window size,
nil otherwise.  */)
  (Lisp_Object process, Lisp_Object height, Lisp_Object width)
{
  CHECK_PROCESS (process);

  /* All known platforms store window sizes as 'unsigned short'.  */
  CHECK_RANGED_INTEGER (height, 0, USHRT_MAX);
  CHECK_RANGED_INTEGER (width, 0, USHRT_MAX);

  if (NETCONN_P (process)
      || XPROCESS (process)->infd < 0
      || (set_window_size (XPROCESS (process)->infd,
			   XINT (height), XINT (width))
	  < 0))
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
for the process which will run.

This function returns FLAG.  */)
  (register Lisp_Object process, Lisp_Object flag)
{
  CHECK_PROCESS (process);
  XPROCESS (process)->inherit_coding_system_flag = !NILP (flag);
  return flag;
}

DEFUN ("set-process-query-on-exit-flag",
       Fset_process_query_on_exit_flag, Sset_process_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for PROCESS when Emacs is exited.
If the second argument FLAG is non-nil, Emacs will query the user before
exiting or killing a buffer if PROCESS is running.  This function
returns FLAG.  */)
  (register Lisp_Object process, Lisp_Object flag)
{
  CHECK_PROCESS (process);
  XPROCESS (process)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("process-query-on-exit-flag",
       Fprocess_query_on_exit_flag, Sprocess_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of query-on-exit flag for PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return (XPROCESS (process)->kill_without_query ? Qnil : Qt);
}

DEFUN ("process-contact", Fprocess_contact, Sprocess_contact,
       1, 2, 0,
       doc: /* Return the contact info of PROCESS; t for a real child.
For a network or serial or pipe connection, the value depends on the
optional KEY arg.  If KEY is nil, value is a cons cell of the form
\(HOST SERVICE) for a network connection or (PORT SPEED) for a serial
connection; it is t for a pipe connection.  If KEY is t, the complete
contact information for the connection is returned, else the specific
value for the keyword KEY is returned.  See `make-network-process',
`make-serial-process', or `make-pipe-process' for the list of keywords.
If PROCESS is a non-blocking network process that hasn't been fully
set up yet, this function will block until socket setup has completed.  */)
  (Lisp_Object process, Lisp_Object key)
{
  Lisp_Object contact;

  CHECK_PROCESS (process);
  contact = XPROCESS (process)->childp;

#ifdef DATAGRAM_SOCKETS

  if (NETCONN_P (process))
    wait_for_socket_fds (process, "process-contact");

  if (DATAGRAM_CONN_P (process)
      && (EQ (key, Qt) || EQ (key, QCremote)))
    contact = Fplist_put (contact, QCremote,
			  Fprocess_datagram_address (process));
#endif

  if ((!NETCONN_P (process) && !SERIALCONN_P (process) && !PIPECONN_P (process))
      || EQ (key, Qt))
    return contact;
  if (NILP (key) && NETCONN_P (process))
    return list2 (Fplist_get (contact, QChost),
		  Fplist_get (contact, QCservice));
  if (NILP (key) && SERIALCONN_P (process))
    return list2 (Fplist_get (contact, QCport),
		  Fplist_get (contact, QCspeed));
  /* FIXME: Return a meaningful value (e.g., the child end of the pipe)
     if the pipe process is useful for purposes other than receiving
     stderr.  */
  if (NILP (key) && PIPECONN_P (process))
    return Qt;
  return Fplist_get (contact, key);
}

DEFUN ("process-plist", Fprocess_plist, Sprocess_plist,
       1, 1, 0,
       doc: /* Return the plist of PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->plist;
}

DEFUN ("set-process-plist", Fset_process_plist, Sset_process_plist,
       2, 2, 0,
       doc: /* Replace the plist of PROCESS with PLIST.  Return PLIST.  */)
  (Lisp_Object process, Lisp_Object plist)
{
  CHECK_PROCESS (process);
  CHECK_LIST (plist);

  pset_plist (XPROCESS (process), plist);
  return plist;
}

#if 0 /* Turned off because we don't currently record this info
	 in the process.  Perhaps add it.  */
DEFUN ("process-connection", Fprocess_connection, Sprocess_connection, 1, 1, 0,
       doc: /* Return the connection type of PROCESS.
The value is nil for a pipe, t or `pty' for a pty, or `stream' for
a socket connection.  */)
  (Lisp_Object process)
{
  return XPROCESS (process)->type;
}
#endif

DEFUN ("process-type", Fprocess_type, Sprocess_type, 1, 1, 0,
       doc: /* Return the connection type of PROCESS.
The value is either the symbol `real', `network', `serial', or `pipe'.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.  */)
  (Lisp_Object process)
{
  Lisp_Object proc;
  proc = get_process (process);
  return XPROCESS (proc)->type;
}

DEFUN ("format-network-address", Fformat_network_address, Sformat_network_address,
       1, 2, 0,
       doc: /* Convert network ADDRESS from internal format to a string.
A 4 or 5 element vector represents an IPv4 address (with port number).
An 8 or 9 element vector represents an IPv6 address (with port number).
If optional second argument OMIT-PORT is non-nil, don't include a port
number in the string, even when present in ADDRESS.
Return nil if format of ADDRESS is invalid.  */)
  (Lisp_Object address, Lisp_Object omit_port)
{
  if (NILP (address))
    return Qnil;

  if (STRINGP (address))  /* AF_LOCAL */
    return address;

  if (VECTORP (address))  /* AF_INET or AF_INET6 */
    {
      register struct Lisp_Vector *p = XVECTOR (address);
      ptrdiff_t size = p->header.size;
      Lisp_Object args[10];
      int nargs, i;
      char const *format;

      if (size == 4 || (size == 5 && !NILP (omit_port)))
	{
	  format = "%d.%d.%d.%d";
	  nargs = 4;
	}
      else if (size == 5)
	{
	  format = "%d.%d.%d.%d:%d";
	  nargs = 5;
	}
      else if (size == 8 || (size == 9 && !NILP (omit_port)))
	{
	  format = "%x:%x:%x:%x:%x:%x:%x:%x";
	  nargs = 8;
	}
      else if (size == 9)
	{
	  format = "[%x:%x:%x:%x:%x:%x:%x:%x]:%d";
	  nargs = 9;
	}
      else
	return Qnil;

      AUTO_STRING (format_obj, format);
      args[0] = format_obj;

      for (i = 0; i < nargs; i++)
	{
	  if (! RANGED_INTEGERP (0, p->contents[i], 65535))
	    return Qnil;

	  if (nargs <= 5         /* IPv4 */
	      && i < 4           /* host, not port */
	      && XINT (p->contents[i]) > 255)
	    return Qnil;

	  args[i + 1] = p->contents[i];
	}

      return Fformat (nargs + 1, args);
    }

  if (CONSP (address))
    {
      AUTO_STRING (format, "<Family %d>");
      return CALLN (Fformat, format, Fcar (address));
    }

  return Qnil;
}

DEFUN ("process-list", Fprocess_list, Sprocess_list, 0, 0, 0,
       doc: /* Return a list of all processes that are Emacs sub-processes.  */)
  (void)
{
  return Fmapcar (Qcdr, Vprocess_alist);
}

/* Starting asynchronous inferior processes.  */

DEFUN ("make-process", Fmake_process, Smake_process, 0, MANY, 0,
       doc: /* Start a program in a subprocess.  Return the process object for it.

This is similar to `start-process', but arguments are specified as
keyword/argument pairs.  The following arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify an output stream or filter function to handle the output.
BUFFER may be also nil, meaning that this process is not associated
with any buffer.

:command COMMAND -- COMMAND is a list starting with the program file
name, followed by strings to give to the program as arguments.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.
In the stopped state, a process does not accept incoming data, but you
can send outgoing data.  The stopped state is cleared by
`continue-process' and set by `stop-process'.

:connection-type TYPE -- TYPE is control type of device used to
communicate with subprocesses.  Values are `pipe' to use a pipe, `pty'
to use a pty, or nil to use the default specified through
`process-connection-type'.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:stderr STDERR -- STDERR is either a buffer or a pipe process attached
to the standard error of subprocess.  Specifying this implies
`:connection-type' is set to `pipe'.

usage: (make-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object buffer, name, command, program, proc, contact, current_dir, tem;
  Lisp_Object xstderr, stderrproc;
  ptrdiff_t count = SPECPDL_INDEX ();

  if (nargs == 0)
    return Qnil;

  /* Save arguments for process-contact and clone-process.  */
  contact = Flist (nargs, args);

  buffer = Fplist_get (contact, QCbuffer);
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.  */
  current_dir = encode_current_directory ();

  name = Fplist_get (contact, QCname);
  CHECK_STRING (name);

  command = Fplist_get (contact, QCcommand);
  if (CONSP (command))
    program = XCAR (command);
  else
    program = Qnil;

  if (!NILP (program))
    CHECK_STRING (program);

  bool query_on_exit = NILP (Fplist_get (contact, QCnoquery));

  stderrproc = Qnil;
  xstderr = Fplist_get (contact, QCstderr);
  if (PROCESSP (xstderr))
    {
      if (!PIPECONN_P (xstderr))
	error ("Process is not a pipe process");
      stderrproc = xstderr;
    }
  else if (!NILP (xstderr))
    {
      CHECK_STRING (program);
      stderrproc = CALLN (Fmake_pipe_process,
			  QCname,
			  concat2 (name, build_string (" stderr")),
			  QCbuffer,
			  Fget_buffer_create (xstderr),
			  QCnoquery,
			  query_on_exit ? Qnil : Qt);
    }

  proc = make_process (name);
  record_unwind_protect (start_process_unwind, proc);

  pset_childp (XPROCESS (proc), Qt);
  eassert (NILP (XPROCESS (proc)->plist));
  pset_type (XPROCESS (proc), Qreal);
  pset_buffer (XPROCESS (proc), buffer);
  pset_sentinel (XPROCESS (proc), Fplist_get (contact, QCsentinel));
  pset_filter (XPROCESS (proc), Fplist_get (contact, QCfilter));
  pset_command (XPROCESS (proc), Fcopy_sequence (command));

  if (!query_on_exit)
    XPROCESS (proc)->kill_without_query = 1;
  if (tem = Fplist_get (contact, QCstop), !NILP (tem))
    pset_command (XPROCESS (proc), Qt);

  tem = Fplist_get (contact, QCconnection_type);
  if (EQ (tem, Qpty))
    XPROCESS (proc)->pty_flag = true;
  else if (EQ (tem, Qpipe))
    XPROCESS (proc)->pty_flag = false;
  else if (NILP (tem))
    XPROCESS (proc)->pty_flag = !NILP (Vprocess_connection_type);
  else
    report_file_error ("Unknown connection type", tem);

  if (!NILP (stderrproc))
    {
      pset_stderrproc (XPROCESS (proc), stderrproc);

      XPROCESS (proc)->pty_flag = false;
    }

#ifdef HAVE_GNUTLS
  /* AKA GNUTLS_INITSTAGE(proc).  */
  verify (GNUTLS_STAGE_EMPTY == 0);
  eassert (XPROCESS (proc)->gnutls_initstage == GNUTLS_STAGE_EMPTY);
  eassert (NILP (XPROCESS (proc)->gnutls_cred_type));
#endif

  XPROCESS (proc)->adaptive_read_buffering
    = (NILP (Vprocess_adaptive_read_buffering) ? 0
       : EQ (Vprocess_adaptive_read_buffering, Qt) ? 1 : 2);

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (buffer))
    set_marker_both (XPROCESS (proc)->mark, buffer,
		     BUF_ZV (XBUFFER (buffer)),
		     BUF_ZV_BYTE (XBUFFER (buffer)));

  USE_SAFE_ALLOCA;

  {
    /* Decide coding systems for communicating with the process.  Here
       we don't setup the structure coding_system nor pay attention to
       unibyte mode.  They are done in create_process.  */

    /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
    Lisp_Object coding_systems = Qt;
    Lisp_Object val, *args2;

    tem = Fplist_get (contact, QCcoding);
    if (!NILP (tem))
      {
	val = tem;
	if (CONSP (val))
	  val = XCAR (val);
      }
    else
      val = Vcoding_system_for_read;
    if (NILP (val))
      {
	ptrdiff_t nargs2 = 3 + XINT (Flength (command));
	Lisp_Object tem2;
	SAFE_ALLOCA_LISP (args2, nargs2);
	ptrdiff_t i = 0;
	args2[i++] = Qstart_process;
	args2[i++] = name;
	args2[i++] = buffer;
	for (tem2 = command; CONSP (tem2); tem2 = XCDR (tem2))
	  args2[i++] = XCAR (tem2);
	if (!NILP (program))
	  coding_systems = Ffind_operation_coding_system (nargs2, args2);
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
      }
    pset_decode_coding_system (XPROCESS (proc), val);

    if (!NILP (tem))
      {
	val = tem;
	if (CONSP (val))
	  val = XCDR (val);
      }
    else
      val = Vcoding_system_for_write;
    if (NILP (val))
      {
	if (EQ (coding_systems, Qt))
	  {
	    ptrdiff_t nargs2 = 3 + XINT (Flength (command));
	    Lisp_Object tem2;
	    SAFE_ALLOCA_LISP (args2, nargs2);
	    ptrdiff_t i = 0;
	    args2[i++] = Qstart_process;
	    args2[i++] = name;
	    args2[i++] = buffer;
	    for (tem2 = command; CONSP (tem2); tem2 = XCDR (tem2))
	      args2[i++] = XCAR (tem2);
	    if (!NILP (program))
	      coding_systems = Ffind_operation_coding_system (nargs2, args2);
	  }
	if (CONSP (coding_systems))
	  val = XCDR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCDR (Vdefault_process_coding_system);
      }
    pset_encode_coding_system (XPROCESS (proc), val);
    /* Note: At this moment, the above coding system may leave
       text-conversion or eol-conversion unspecified.  They will be
       decided after we read output from the process and decode it by
       some coding system, or just before we actually send a text to
       the process.  */
  }


  pset_decoding_buf (XPROCESS (proc), empty_unibyte_string);
  eassert (XPROCESS (proc)->decoding_carryover == 0);
  pset_encoding_buf (XPROCESS (proc), empty_unibyte_string);

  XPROCESS (proc)->inherit_coding_system_flag
    = !(NILP (buffer) || !inherit_process_coding_system);

  if (!NILP (program))
    {
      Lisp_Object program_args = XCDR (command);

      /* If program file name is not absolute, search our path for it.
	 Put the name we will really use in TEM.  */
      if (!IS_DIRECTORY_SEP (SREF (program, 0))
	  && !(SCHARS (program) > 1
	       && IS_DEVICE_SEP (SREF (program, 1))))
	{
	  tem = Qnil;
	  openp (Vexec_path, program, Vexec_suffixes, &tem,
		 make_number (X_OK), false);
	  if (NILP (tem))
	    report_file_error ("Searching for program", program);
	  tem = Fexpand_file_name (tem, Qnil);
	}
      else
	{
	  if (!NILP (Ffile_directory_p (program)))
	    error ("Specified program for new process is a directory");
	  tem = program;
	}

      /* Remove "/:" from TEM.  */
      tem = remove_slash_colon (tem);

      Lisp_Object arg_encoding = Qnil;

      /* Encode the file name and put it in NEW_ARGV.
	 That's where the child will use it to execute the program.  */
      tem = list1 (ENCODE_FILE (tem));
      ptrdiff_t new_argc = 1;

      /* Here we encode arguments by the coding system used for sending
	 data to the process.  We don't support using different coding
	 systems for encoding arguments and for encoding data sent to the
	 process.  */

      for (Lisp_Object tem2 = program_args; CONSP (tem2); tem2 = XCDR (tem2))
	{
	  Lisp_Object arg = XCAR (tem2);
	  CHECK_STRING (arg);
	  if (STRING_MULTIBYTE (arg))
	    {
	      if (NILP (arg_encoding))
		arg_encoding = (complement_process_encoding_system
				(XPROCESS (proc)->encode_coding_system));
	      arg = code_convert_string_norecord (arg, arg_encoding, 1);
	    }
	  tem = Fcons (arg, tem);
	  new_argc++;
	}

      /* Now that everything is encoded we can collect the strings into
	 NEW_ARGV.  */
      char **new_argv;
      SAFE_NALLOCA (new_argv, 1, new_argc + 1);
      new_argv[new_argc] = 0;

      for (ptrdiff_t i = new_argc - 1; i >= 0; i--)
	{
	  new_argv[i] = SSDATA (XCAR (tem));
	  tem = XCDR (tem);
	}

      create_process (proc, new_argv, current_dir);
    }
  else
    create_pty (proc);

  SAFE_FREE ();
  return unbind_to (count, proc);
}

/* If PROC doesn't have its pid set, then an error was signaled and
   the process wasn't started successfully, so remove it.  */
static void
start_process_unwind (Lisp_Object proc)
{
  if (XPROCESS (proc)->pid <= 0 && XPROCESS (proc)->pid != -2)
    remove_process (proc);
}

/* If *FD_ADDR is nonnegative, close it, and mark it as closed.  */

static void
close_process_fd (int *fd_addr)
{
  int fd = *fd_addr;
  if (0 <= fd)
    {
      *fd_addr = -1;
      emacs_close (fd);
    }
}

/* Indexes of file descriptors in open_fds.  */
enum
  {
    /* The pipe from Emacs to its subprocess.  */
    SUBPROCESS_STDIN,
    WRITE_TO_SUBPROCESS,

    /* The main pipe from the subprocess to Emacs.  */
    READ_FROM_SUBPROCESS,
    SUBPROCESS_STDOUT,

    /* The pipe from the subprocess to Emacs that is closed when the
       subprocess execs.  */
    READ_FROM_EXEC_MONITOR,
    EXEC_MONITOR_OUTPUT
  };

verify (PROCESS_OPEN_FDS == EXEC_MONITOR_OUTPUT + 1);

static void
create_process (Lisp_Object process, char **new_argv, Lisp_Object current_dir)
{
  struct Lisp_Process *p = XPROCESS (process);
  int inchannel, outchannel;
  pid_t pid;
  int vfork_errno;
  int forkin, forkout, forkerr = -1;
  bool pty_flag = 0;
  char pty_name[PTY_NAME_SIZE];
  Lisp_Object lisp_pty_name = Qnil;
  sigset_t oldset;

  inchannel = outchannel = -1;

  if (p->pty_flag)
    outchannel = inchannel = allocate_pty (pty_name);

  if (inchannel >= 0)
    {
      p->open_fd[READ_FROM_SUBPROCESS] = inchannel;
#if ! defined (USG) || defined (USG_SUBTTY_WORKS)
      /* On most USG systems it does not work to open the pty's tty here,
	 then close it and reopen it in the child.  */
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
      p->open_fd[SUBPROCESS_STDIN] = forkin;
#else
      forkin = forkout = -1;
#endif /* not USG, or USG_SUBTTY_WORKS */
      pty_flag = 1;
      lisp_pty_name = build_string (pty_name);
    }
  else
    {
      if (emacs_pipe (p->open_fd + SUBPROCESS_STDIN) != 0
	  || emacs_pipe (p->open_fd + READ_FROM_SUBPROCESS) != 0)
	report_file_error ("Creating pipe", Qnil);
      forkin = p->open_fd[SUBPROCESS_STDIN];
      outchannel = p->open_fd[WRITE_TO_SUBPROCESS];
      inchannel = p->open_fd[READ_FROM_SUBPROCESS];
      forkout = p->open_fd[SUBPROCESS_STDOUT];

      if (!NILP (p->stderrproc))
	{
	  struct Lisp_Process *pp = XPROCESS (p->stderrproc);

	  forkerr = pp->open_fd[SUBPROCESS_STDOUT];

	  /* Close unnecessary file descriptors.  */
	  close_process_fd (&pp->open_fd[WRITE_TO_SUBPROCESS]);
	  close_process_fd (&pp->open_fd[SUBPROCESS_STDIN]);
	}
    }

#ifndef WINDOWSNT
  if (emacs_pipe (p->open_fd + READ_FROM_EXEC_MONITOR) != 0)
    report_file_error ("Creating pipe", Qnil);
#endif

  fcntl (inchannel, F_SETFL, O_NONBLOCK);
  fcntl (outchannel, F_SETFL, O_NONBLOCK);

  /* Record this as an active process, with its channels.  */
  chan_process[inchannel] = process;
  p->infd = inchannel;
  p->outfd = outchannel;

  /* Previously we recorded the tty descriptor used in the subprocess.
     It was only used for getting the foreground tty process, so now
     we just reopen the device (see emacs_get_tty_pgrp) as this is
     more portable (see USG_SUBTTY_WORKS above).  */

  p->pty_flag = pty_flag;
  pset_status (p, Qrun);

  if (!EQ (p->command, Qt))
    add_process_read_fd (inchannel);

  /* This may signal an error.  */
  setup_process_coding_systems (process);

  block_input ();
  block_child_signal (&oldset);

#ifndef WINDOWSNT
  /* vfork, and prevent local vars from being clobbered by the vfork.  */
  Lisp_Object volatile current_dir_volatile = current_dir;
  Lisp_Object volatile lisp_pty_name_volatile = lisp_pty_name;
  char **volatile new_argv_volatile = new_argv;
  int volatile forkin_volatile = forkin;
  int volatile forkout_volatile = forkout;
  int volatile forkerr_volatile = forkerr;
  struct Lisp_Process *p_volatile = p;

#ifdef DARWIN_OS
  /* Darwin doesn't let us run setsid after a vfork, so use fork when
     necessary.  Also, reset SIGCHLD handling after a vfork, as
     apparently macOS can mistakenly deliver SIGCHLD to the child.  */
  if (pty_flag)
    pid = fork ();
  else
    {
      pid = vfork ();
      if (pid == 0)
	signal (SIGCHLD, SIG_DFL);
    }
#else
  pid = vfork ();
#endif

  current_dir = current_dir_volatile;
  lisp_pty_name = lisp_pty_name_volatile;
  new_argv = new_argv_volatile;
  forkin = forkin_volatile;
  forkout = forkout_volatile;
  forkerr = forkerr_volatile;
  p = p_volatile;

  pty_flag = p->pty_flag;

  if (pid == 0)
#endif /* not WINDOWSNT */
    {
      /* Make the pty be the controlling terminal of the process.  */
#ifdef HAVE_PTYS
      /* First, disconnect its current controlling terminal.  */
      if (pty_flag)
	setsid ();
      /* Make the pty's terminal the controlling terminal.  */
      if (pty_flag && forkin >= 0)
	{
#ifdef TIOCSCTTY
	  /* We ignore the return value
	     because faith@cs.unc.edu says that is necessary on Linux.  */
	  ioctl (forkin, TIOCSCTTY, 0);
#endif
	}
#if defined (LDISC1)
      if (pty_flag && forkin >= 0)
	{
	  struct termios t;
	  tcgetattr (forkin, &t);
	  t.c_lflag = LDISC1;
	  if (tcsetattr (forkin, TCSANOW, &t) < 0)
	    emacs_perror ("create_process/tcsetattr LDISC1");
	}
#else
#if defined (NTTYDISC) && defined (TIOCSETD)
      if (pty_flag && forkin >= 0)
	{
	  /* Use new line discipline.  */
	  int ldisc = NTTYDISC;
	  ioctl (forkin, TIOCSETD, &ldisc);
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
	  int j = emacs_open (DEV_TTY, O_RDWR, 0);
	  if (j >= 0)
	    {
	      ioctl (j, TIOCNOTTY, 0);
	      emacs_close (j);
	    }
	}
#endif /* TIOCNOTTY */

#if !defined (DONT_REOPEN_PTY)
/*** There is a suggestion that this ought to be a
     conditional on TIOCSPGRP, or !defined TIOCSCTTY.
     Trying the latter gave the wrong results on Debian GNU/Linux 1.1;
     that system does seem to need this code, even though
     both TIOCSCTTY is defined.  */
	/* Now close the pty (if we had it open) and reopen it.
	   This makes the pty the controlling terminal of the subprocess.  */
      if (pty_flag)
	{

	  /* I wonder if emacs_close (emacs_open (SSDATA (lisp_pty_name), ...))
	     would work?  */
	  if (forkin >= 0)
	    emacs_close (forkin);
	  forkout = forkin = emacs_open (SSDATA (lisp_pty_name), O_RDWR, 0);

	  if (forkin < 0)
	    {
	      emacs_perror (SSDATA (lisp_pty_name));
	      _exit (EXIT_CANCELED);
	    }

	}
#endif /* not DONT_REOPEN_PTY */

#ifdef SETUP_SLAVE_PTY
      if (pty_flag)
	{
	  SETUP_SLAVE_PTY;
	}
#endif /* SETUP_SLAVE_PTY */
#endif /* HAVE_PTYS */

      signal (SIGINT, SIG_DFL);
      signal (SIGQUIT, SIG_DFL);
#ifdef SIGPROF
      signal (SIGPROF, SIG_DFL);
#endif

      /* Emacs ignores SIGPIPE, but the child should not.  */
      signal (SIGPIPE, SIG_DFL);

      /* Stop blocking SIGCHLD in the child.  */
      unblock_child_signal (&oldset);

      if (pty_flag)
	child_setup_tty (forkout);

      if (forkerr < 0)
	forkerr = forkout;
#ifdef WINDOWSNT
      pid = child_setup (forkin, forkout, forkerr, new_argv, 1, current_dir);
#else  /* not WINDOWSNT */
      child_setup (forkin, forkout, forkerr, new_argv, 1, current_dir);
#endif /* not WINDOWSNT */
    }

  /* Back in the parent process.  */

  vfork_errno = errno;
  p->pid = pid;
  if (pid >= 0)
    p->alive = 1;

  /* Stop blocking in the parent.  */
  unblock_child_signal (&oldset);
  unblock_input ();

  if (pid < 0)
    report_file_errno ("Doing vfork", Qnil, vfork_errno);
  else
    {
      /* vfork succeeded.  */

      /* Close the pipe ends that the child uses, or the child's pty.  */
      close_process_fd (&p->open_fd[SUBPROCESS_STDIN]);
      close_process_fd (&p->open_fd[SUBPROCESS_STDOUT]);

#ifdef WINDOWSNT
      register_child (pid, inchannel);
#endif /* WINDOWSNT */

      pset_tty_name (p, lisp_pty_name);

#ifndef WINDOWSNT
      /* Wait for child_setup to complete in case that vfork is
	 actually defined as fork.  The descriptor
	 XPROCESS (proc)->open_fd[EXEC_MONITOR_OUTPUT]
	 of a pipe is closed at the child side either by close-on-exec
	 on successful execve or the _exit call in child_setup.  */
      {
	char dummy;

	close_process_fd (&p->open_fd[EXEC_MONITOR_OUTPUT]);
	emacs_read (p->open_fd[READ_FROM_EXEC_MONITOR], &dummy, 1);
	close_process_fd (&p->open_fd[READ_FROM_EXEC_MONITOR]);
      }
#endif
      if (!NILP (p->stderrproc))
	{
	  struct Lisp_Process *pp = XPROCESS (p->stderrproc);
	  close_process_fd (&pp->open_fd[SUBPROCESS_STDOUT]);
	}
    }
}

static void
create_pty (Lisp_Object process)
{
  struct Lisp_Process *p = XPROCESS (process);
  char pty_name[PTY_NAME_SIZE];
  int pty_fd = !p->pty_flag ? -1 : allocate_pty (pty_name);

  if (pty_fd >= 0)
    {
      p->open_fd[SUBPROCESS_STDIN] = pty_fd;
#if ! defined (USG) || defined (USG_SUBTTY_WORKS)
      /* On most USG systems it does not work to open the pty's tty here,
	 then close it and reopen it in the child.  */
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      int forkout = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
      if (forkout < 0)
	report_file_error ("Opening pty", Qnil);
      p->open_fd[WRITE_TO_SUBPROCESS] = forkout;
#if defined (DONT_REOPEN_PTY)
      /* In the case that vfork is defined as fork, the parent process
	 (Emacs) may send some data before the child process completes
	 tty options setup.  So we setup tty before forking.  */
      child_setup_tty (forkout);
#endif /* DONT_REOPEN_PTY */
#endif /* not USG, or USG_SUBTTY_WORKS */

      fcntl (pty_fd, F_SETFL, O_NONBLOCK);

      /* Record this as an active process, with its channels.
	 As a result, child_setup will close Emacs's side of the pipes.  */
      chan_process[pty_fd] = process;
      p->infd = pty_fd;
      p->outfd = pty_fd;

      /* Previously we recorded the tty descriptor used in the subprocess.
	 It was only used for getting the foreground tty process, so now
	 we just reopen the device (see emacs_get_tty_pgrp) as this is
	 more portable (see USG_SUBTTY_WORKS above).  */

      p->pty_flag = 1;
      pset_status (p, Qrun);
      setup_process_coding_systems (process);

      add_process_read_fd (pty_fd);

      pset_tty_name (p, build_string (pty_name));
    }

  p->pid = -2;
}

DEFUN ("make-pipe-process", Fmake_pipe_process, Smake_pipe_process,
       0, MANY, 0,
       doc: /* Create and return a bidirectional pipe process.

In Emacs, pipes are represented by process objects, so input and
output work as for subprocesses, and `delete-process' closes a pipe.
However, a pipe process has no process id, it cannot be signaled,
and the status codes are different from normal processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:name NAME -- NAME is the name of the process.  It is modified if necessary to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at the end of that buffer,
unless you specify an output stream or filter function to handle the
output.  If BUFFER is not given, the value of NAME is used.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.
In the stopped state, a pipe process does not accept incoming data,
but you can send outgoing data.  The stopped state is cleared by
`continue-process' and set by `stop-process'.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

usage:  (make-pipe-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object proc, contact;
  struct Lisp_Process *p;
  Lisp_Object name, buffer;
  Lisp_Object tem;
  ptrdiff_t specpdl_count;
  int inchannel, outchannel;

  if (nargs == 0)
    return Qnil;

  contact = Flist (nargs, args);

  name = Fplist_get (contact, QCname);
  CHECK_STRING (name);
  proc = make_process (name);
  specpdl_count = SPECPDL_INDEX ();
  record_unwind_protect (remove_process, proc);
  p = XPROCESS (proc);

  if (emacs_pipe (p->open_fd + SUBPROCESS_STDIN) != 0
      || emacs_pipe (p->open_fd + READ_FROM_SUBPROCESS) != 0)
    report_file_error ("Creating pipe", Qnil);
  outchannel = p->open_fd[WRITE_TO_SUBPROCESS];
  inchannel = p->open_fd[READ_FROM_SUBPROCESS];

  fcntl (inchannel, F_SETFL, O_NONBLOCK);
  fcntl (outchannel, F_SETFL, O_NONBLOCK);

#ifdef WINDOWSNT
  register_aux_fd (inchannel);
#endif

  /* Record this as an active process, with its channels.  */
  chan_process[inchannel] = proc;
  p->infd = inchannel;
  p->outfd = outchannel;

  if (inchannel > max_desc)
    max_desc = inchannel;

  buffer = Fplist_get (contact, QCbuffer);
  if (NILP (buffer))
    buffer = name;
  buffer = Fget_buffer_create (buffer);
  pset_buffer (p, buffer);

  pset_childp (p, contact);
  pset_plist (p, Fcopy_sequence (Fplist_get (contact, QCplist)));
  pset_type (p, Qpipe);
  pset_sentinel (p, Fplist_get (contact, QCsentinel));
  pset_filter (p, Fplist_get (contact, QCfilter));
  eassert (NILP (p->log));
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = 1;
  if (tem = Fplist_get (contact, QCstop), !NILP (tem))
    pset_command (p, Qt);
  eassert (! p->pty_flag);

  if (!EQ (p->command, Qt))
    add_process_read_fd (inchannel);
  p->adaptive_read_buffering
    = (NILP (Vprocess_adaptive_read_buffering) ? 0
       : EQ (Vprocess_adaptive_read_buffering, Qt) ? 1 : 2);

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (buffer))
    set_marker_both (p->mark, buffer,
		     BUF_ZV (XBUFFER (buffer)),
		     BUF_ZV_BYTE (XBUFFER (buffer)));

  {
    /* Setup coding systems for communicating with the network stream.  */

    /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
    Lisp_Object coding_systems = Qt;
    Lisp_Object val;

    tem = Fplist_get (contact, QCcoding);
    val = Qnil;
    if (!NILP (tem))
      {
	val = tem;
	if (CONSP (val))
	  val = XCAR (val);
      }
    else if (!NILP (Vcoding_system_for_read))
      val = Vcoding_system_for_read;
    else if ((!NILP (buffer) && NILP (BVAR (XBUFFER (buffer), enable_multibyte_characters)))
	     || (NILP (buffer) && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
      /* We dare not decode end-of-line format by setting VAL to
	 Qraw_text, because the existing Emacs Lisp libraries
	 assume that they receive bare code including a sequence of
	 CR LF.  */
      val = Qnil;
    else
      {
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    pset_decode_coding_system (p, val);

    if (!NILP (tem))
      {
	val = tem;
	if (CONSP (val))
	  val = XCDR (val);
      }
    else if (!NILP (Vcoding_system_for_write))
      val = Vcoding_system_for_write;
    else if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
      val = Qnil;
    else
      {
	if (CONSP (coding_systems))
	  val = XCDR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCDR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    pset_encode_coding_system (p, val);
  }
  /* This may signal an error.  */
  setup_process_coding_systems (proc);

  specpdl_ptr = specpdl + specpdl_count;

  return proc;
}


/* Convert an internal struct sockaddr to a lisp object (vector or string).
   The address family of sa is not included in the result.  */

Lisp_Object
conv_sockaddr_to_lisp (struct sockaddr *sa, ptrdiff_t len)
{
  Lisp_Object address;
  ptrdiff_t i;
  unsigned char *cp;
  struct Lisp_Vector *p;

  /* Workaround for a bug in getsockname on BSD: Names bound to
     sockets in the UNIX domain are inaccessible; getsockname returns
     a zero length name.  */
  if (len < offsetof (struct sockaddr, sa_family) + sizeof (sa->sa_family))
    return empty_unibyte_string;

  switch (sa->sa_family)
    {
    case AF_INET:
      {
	DECLARE_POINTER_ALIAS (sin, struct sockaddr_in, sa);
	len = sizeof (sin->sin_addr) + 1;
	address = Fmake_vector (make_number (len), Qnil);
	p = XVECTOR (address);
	p->contents[--len] = make_number (ntohs (sin->sin_port));
	cp = (unsigned char *) &sin->sin_addr;
	break;
      }
#ifdef AF_INET6
    case AF_INET6:
      {
	DECLARE_POINTER_ALIAS (sin6, struct sockaddr_in6, sa);
	DECLARE_POINTER_ALIAS (ip6, uint16_t, &sin6->sin6_addr);
	len = sizeof (sin6->sin6_addr) / 2 + 1;
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
	DECLARE_POINTER_ALIAS (sockun, struct sockaddr_un, sa);
        ptrdiff_t name_length = len - offsetof (struct sockaddr_un, sun_path);
        /* If the first byte is NUL, the name is a Linux abstract
           socket name, and the name can contain embedded NULs.  If
           it's not, we have a NUL-terminated string.  Be careful not
           to walk past the end of the object looking for the name
           terminator, however.  */
        if (name_length > 0 && sockun->sun_path[0] != '\0')
          {
            const char *terminator
	      = memchr (sockun->sun_path, '\0', name_length);

            if (terminator)
              name_length = terminator - (const char *) sockun->sun_path;
          }

	return make_unibyte_string (sockun->sun_path, name_length);
      }
#endif
    default:
      len -= offsetof (struct sockaddr, sa_family) + sizeof (sa->sa_family);
      address = Fcons (make_number (sa->sa_family),
		       Fmake_vector (make_number (len), Qnil));
      p = XVECTOR (XCDR (address));
      cp = (unsigned char *) &sa->sa_family + sizeof (sa->sa_family);
      break;
    }

  i = 0;
  while (i < len)
    p->contents[i++] = make_number (*cp++);

  return address;
}

/* Convert an internal struct addrinfo to a Lisp object.  */

static Lisp_Object
conv_addrinfo_to_lisp (struct addrinfo *res)
{
  Lisp_Object protocol = make_number (res->ai_protocol);
  eassert (XINT (protocol) == res->ai_protocol);
  return Fcons (protocol, conv_sockaddr_to_lisp (res->ai_addr, res->ai_addrlen));
}


/* Get family and required size for sockaddr structure to hold ADDRESS.  */

static ptrdiff_t
get_lisp_to_sockaddr_size (Lisp_Object address, int *familyp)
{
  struct Lisp_Vector *p;

  if (VECTORP (address))
    {
      p = XVECTOR (address);
      if (p->header.size == 5)
	{
	  *familyp = AF_INET;
	  return sizeof (struct sockaddr_in);
	}
#ifdef AF_INET6
      else if (p->header.size == 9)
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
  else if (CONSP (address) && TYPE_RANGED_INTEGERP (int, XCAR (address))
	   && VECTORP (XCDR (address)))
    {
      struct sockaddr *sa;
      p = XVECTOR (XCDR (address));
      if (MAX_ALLOCA - sizeof sa->sa_family < p->header.size)
	return 0;
      *familyp = XINT (XCAR (address));
      return p->header.size + sizeof (sa->sa_family);
    }
  return 0;
}

/* Convert an address object (vector or string) to an internal sockaddr.

   The address format has been basically validated by
   get_lisp_to_sockaddr_size, but this does not mean FAMILY is valid;
   it could have come from user data.  So if FAMILY is not valid,
   we return after zeroing *SA.  */

static void
conv_lisp_to_sockaddr (int family, Lisp_Object address, struct sockaddr *sa, int len)
{
  register struct Lisp_Vector *p;
  register unsigned char *cp = NULL;
  register int i;
  EMACS_INT hostport;

  memset (sa, 0, len);

  if (VECTORP (address))
    {
      p = XVECTOR (address);
      if (family == AF_INET)
	{
	  DECLARE_POINTER_ALIAS (sin, struct sockaddr_in, sa);
	  len = sizeof (sin->sin_addr) + 1;
	  hostport = XINT (p->contents[--len]);
	  sin->sin_port = htons (hostport);
	  cp = (unsigned char *)&sin->sin_addr;
	  sa->sa_family = family;
	}
#ifdef AF_INET6
      else if (family == AF_INET6)
	{
	  DECLARE_POINTER_ALIAS (sin6, struct sockaddr_in6, sa);
	  DECLARE_POINTER_ALIAS (ip6, uint16_t, &sin6->sin6_addr);
	  len = sizeof (sin6->sin6_addr) / 2 + 1;
	  hostport = XINT (p->contents[--len]);
	  sin6->sin6_port = htons (hostport);
	  for (i = 0; i < len; i++)
	    if (INTEGERP (p->contents[i]))
	      {
		int j = XFASTINT (p->contents[i]) & 0xffff;
		ip6[i] = ntohs (j);
	      }
	  sa->sa_family = family;
	  return;
	}
#endif
      else
	return;
    }
  else if (STRINGP (address))
    {
#ifdef HAVE_LOCAL_SOCKETS
      if (family == AF_LOCAL)
	{
	  DECLARE_POINTER_ALIAS (sockun, struct sockaddr_un, sa);
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
       doc: /* Get the current datagram address associated with PROCESS.
If PROCESS is a non-blocking network process that hasn't been fully
set up yet, this function will block until socket setup has completed.  */)
  (Lisp_Object process)
{
  int channel;

  CHECK_PROCESS (process);

  if (NETCONN_P (process))
    wait_for_socket_fds (process, "process-datagram-address");

  if (!DATAGRAM_CONN_P (process))
    return Qnil;

  channel = XPROCESS (process)->infd;
  return conv_sockaddr_to_lisp (datagram_address[channel].sa,
				datagram_address[channel].len);
}

DEFUN ("set-process-datagram-address", Fset_process_datagram_address, Sset_process_datagram_address,
       2, 2, 0,
       doc: /* Set the datagram address for PROCESS to ADDRESS.
Return nil upon error setting address, ADDRESS otherwise.

If PROCESS is a non-blocking network process that hasn't been fully
set up yet, this function will block until socket setup has completed.  */)
  (Lisp_Object process, Lisp_Object address)
{
  int channel;
  int family;
  ptrdiff_t len;

  CHECK_PROCESS (process);

  if (NETCONN_P (process))
    wait_for_socket_fds (process, "set-process-datagram-address");

  if (!DATAGRAM_CONN_P (process))
    return Qnil;

  channel = XPROCESS (process)->infd;

  len = get_lisp_to_sockaddr_size (address, &family);
  if (len == 0 || datagram_address[channel].len != len)
    return Qnil;
  conv_lisp_to_sockaddr (family, address, datagram_address[channel].sa, len);
  return address;
}
#endif


static const struct socket_options {
  /* The name of this option.  Should be lowercase version of option
     name without SO_ prefix.  */
  const char *name;
  /* Option level SOL_...  */
  int optlevel;
  /* Option number SO_...  */
  int optnum;
  enum { SOPT_UNKNOWN, SOPT_BOOL, SOPT_INT, SOPT_IFNAME, SOPT_LINGER } opttype;
  enum { OPIX_NONE = 0, OPIX_MISC = 1, OPIX_REUSEADDR = 2 } optbit;
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

   Return (1<<socket_options[OPT].optbit) if option is known, 0 otherwise.
   Signals an error if setting a known option fails.
*/

static int
set_socket_option (int s, Lisp_Object opt, Lisp_Object val)
{
  char *name;
  const struct socket_options *sopt;
  int ret = 0;

  CHECK_SYMBOL (opt);

  name = SSDATA (SYMBOL_NAME (opt));
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
	if (TYPE_RANGED_INTEGERP (int, val))
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
	char devname[IFNAMSIZ + 1];

	/* This is broken, at least in the Linux 2.4 kernel.
	   To unbind, the arg must be a zero integer, not the empty string.
	   This should work on all systems.   KFS. 2003-09-23.  */
	memset (devname, 0, sizeof devname);
	if (STRINGP (val))
	  {
	    char *arg = SSDATA (val);
	    int len = min (strlen (arg), IFNAMSIZ);
	    memcpy (devname, arg, len);
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
	if (TYPE_RANGED_INTEGERP (int, val))
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
    {
      int setsockopt_errno = errno;
      report_file_errno ("Cannot set network option", list2 (opt, val),
			 setsockopt_errno);
    }

  return (1 << sopt->optbit);
}


DEFUN ("set-network-process-option",
       Fset_network_process_option, Sset_network_process_option,
       3, 4, 0,
       doc: /* For network process PROCESS set option OPTION to value VALUE.
See `make-network-process' for a list of options and values.
If optional fourth arg NO-ERROR is non-nil, don't signal an error if
OPTION is not a supported option, return nil instead; otherwise return t.

If PROCESS is a non-blocking network process that hasn't been fully
set up yet, this function will block until socket setup has completed. */)
  (Lisp_Object process, Lisp_Object option, Lisp_Object value, Lisp_Object no_error)
{
  int s;
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  if (!NETCONN1_P (p))
    error ("Process is not a network process");

  wait_for_socket_fds (process, "set-network-process-option");

  s = p->infd;
  if (s < 0)
    error ("Process is not running");

  if (set_socket_option (s, option, value))
    {
      pset_childp (p, Fplist_put (p->childp, option, value));
      return Qt;
    }

  if (NILP (no_error))
    error ("Unknown or unsupported option");

  return Qnil;
}


DEFUN ("serial-process-configure",
       Fserial_process_configure,
       Sserial_process_configure,
       0, MANY, 0,
       doc: /* Configure speed, bytesize, etc. of a serial process.

Arguments are specified as keyword/argument pairs.  Attributes that
are not given are re-initialized from the process's current
configuration (available via the function `process-contact') or set to
reasonable default values.  The following arguments are defined:

:process PROCESS
:name NAME
:buffer BUFFER
:port PORT
-- Any of these arguments can be given to identify the process that is
to be configured.  If none of these arguments is given, the current
buffer's process is used.

:speed SPEED -- SPEED is the speed of the serial port in bits per
second, also called baud rate.  Any value can be given for SPEED, but
most serial ports work only at a few defined values between 1200 and
115200, with 9600 being the most common value.  If SPEED is nil, the
serial port is not configured any further, i.e., all other arguments
are ignored.  This may be useful for special serial ports such as
Bluetooth-to-serial converters which can only be configured through AT
commands.  A value of nil for SPEED can be used only when passed
through `make-serial-process' or `serial-term'.

:bytesize BYTESIZE -- BYTESIZE is the number of bits per byte, which
can be 7 or 8.  If BYTESIZE is not given or nil, a value of 8 is used.

:parity PARITY -- PARITY can be nil (don't use parity), the symbol
`odd' (use odd parity), or the symbol `even' (use even parity).  If
PARITY is not given, no parity is used.

:stopbits STOPBITS -- STOPBITS is the number of stopbits used to
terminate a byte transmission.  STOPBITS can be 1 or 2.  If STOPBITS
is not given or nil, 1 stopbit is used.

:flowcontrol FLOWCONTROL -- FLOWCONTROL determines the type of
flowcontrol to be used, which is either nil (don't use flowcontrol),
the symbol `hw' (use RTS/CTS hardware flowcontrol), or the symbol `sw'
\(use XON/XOFF software flowcontrol).  If FLOWCONTROL is not given, no
flowcontrol is used.

`serial-process-configure' is called by `make-serial-process' for the
initial configuration of the serial port.

Examples:

\(serial-process-configure :process "/dev/ttyS0" :speed 1200)

\(serial-process-configure
    :buffer "COM1" :stopbits 1 :parity \\='odd :flowcontrol \\='hw)

\(serial-process-configure :port "\\\\.\\COM13" :bytesize 7)

usage: (serial-process-configure &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct Lisp_Process *p;
  Lisp_Object contact = Qnil;
  Lisp_Object proc = Qnil;

  contact = Flist (nargs, args);

  proc = Fplist_get (contact, QCprocess);
  if (NILP (proc))
    proc = Fplist_get (contact, QCname);
  if (NILP (proc))
    proc = Fplist_get (contact, QCbuffer);
  if (NILP (proc))
    proc = Fplist_get (contact, QCport);
  proc = get_process (proc);
  p = XPROCESS (proc);
  if (!EQ (p->type, Qserial))
    error ("Not a serial process");

  if (NILP (Fplist_get (p->childp, QCspeed)))
    return Qnil;

  serial_configure (p, contact);
  return Qnil;
}

DEFUN ("make-serial-process", Fmake_serial_process, Smake_serial_process,
       0, MANY, 0,
       doc: /* Create and return a serial port process.

In Emacs, serial port connections are represented by process objects,
so input and output work as for subprocesses, and `delete-process'
closes a serial port connection.  However, a serial process has no
process id, it cannot be signaled, and the status codes are different
from normal processes.

`make-serial-process' creates a process and a buffer, on which you
probably want to use `process-send-string'.  Try \\[serial-term] for
an interactive terminal.  See below for examples.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:port PORT -- (mandatory) PORT is the path or name of the serial port.
For example, this could be "/dev/ttyS0" on Unix.  On Windows, this
could be "COM1", or "\\\\.\\COM10" for ports higher than COM9 (double
the backslashes in strings).

:speed SPEED -- (mandatory) is handled by `serial-process-configure',
which this function calls.

:name NAME -- NAME is the name of the process.  If NAME is not given,
the value of PORT is used.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at the end of that buffer,
unless you specify an output stream or filter function to handle the
output.  If BUFFER is not given, the value of NAME is used.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- Start process in the `stopped' state if BOOL is non-nil.
In the stopped state, a serial process does not accept incoming data,
but you can send outgoing data.  The stopped state is cleared by
`continue-process' and set by `stop-process'.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:plist PLIST -- Install PLIST as the initial plist of the process.

:bytesize
:parity
:stopbits
:flowcontrol
-- This function calls `serial-process-configure' to handle these
arguments.

The original argument list, possibly modified by later configuration,
is available via the function `process-contact'.

Examples:

\(make-serial-process :port "/dev/ttyS0" :speed 9600)

\(make-serial-process :port "COM1" :speed 115200 :stopbits 2)

\(make-serial-process :port "\\\\.\\COM13" :speed 1200 :bytesize 7 :parity \\='odd)

\(make-serial-process :port "/dev/tty.BlueConsole-SPP-1" :speed nil)

usage:  (make-serial-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  int fd = -1;
  Lisp_Object proc, contact, port;
  struct Lisp_Process *p;
  Lisp_Object name, buffer;
  Lisp_Object tem, val;
  ptrdiff_t specpdl_count;

  if (nargs == 0)
    return Qnil;

  contact = Flist (nargs, args);

  port = Fplist_get (contact, QCport);
  if (NILP (port))
    error ("No port specified");
  CHECK_STRING (port);

  if (NILP (Fplist_member (contact, QCspeed)))
    error (":speed not specified");
  if (!NILP (Fplist_get (contact, QCspeed)))
    CHECK_NUMBER (Fplist_get (contact, QCspeed));

  name = Fplist_get (contact, QCname);
  if (NILP (name))
    name = port;
  CHECK_STRING (name);
  proc = make_process (name);
  specpdl_count = SPECPDL_INDEX ();
  record_unwind_protect (remove_process, proc);
  p = XPROCESS (proc);

  fd = serial_open (port);
  p->open_fd[SUBPROCESS_STDIN] = fd;
  p->infd = fd;
  p->outfd = fd;
  if (fd > max_desc)
    max_desc = fd;
  chan_process[fd] = proc;

  buffer = Fplist_get (contact, QCbuffer);
  if (NILP (buffer))
    buffer = name;
  buffer = Fget_buffer_create (buffer);
  pset_buffer (p, buffer);

  pset_childp (p, contact);
  pset_plist (p, Fcopy_sequence (Fplist_get (contact, QCplist)));
  pset_type (p, Qserial);
  pset_sentinel (p, Fplist_get (contact, QCsentinel));
  pset_filter (p, Fplist_get (contact, QCfilter));
  eassert (NILP (p->log));
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = 1;
  if (tem = Fplist_get (contact, QCstop), !NILP (tem))
    pset_command (p, Qt);
  eassert (! p->pty_flag);

  if (!EQ (p->command, Qt))
    add_process_read_fd (fd);

  if (BUFFERP (buffer))
    {
      set_marker_both (p->mark, buffer,
		       BUF_ZV (XBUFFER (buffer)),
		       BUF_ZV_BYTE (XBUFFER (buffer)));
    }

  tem = Fplist_member (contact, QCcoding);
  if (!NILP (tem) && (!CONSP (tem) || !CONSP (XCDR (tem))))
    tem = Qnil;

  val = Qnil;
  if (!NILP (tem))
    {
      val = XCAR (XCDR (tem));
      if (CONSP (val))
	val = XCAR (val);
    }
  else if (!NILP (Vcoding_system_for_read))
    val = Vcoding_system_for_read;
  else if ((!NILP (buffer) && NILP (BVAR (XBUFFER (buffer), enable_multibyte_characters)))
	   || (NILP (buffer) && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
    val = Qnil;
  pset_decode_coding_system (p, val);

  val = Qnil;
  if (!NILP (tem))
    {
      val = XCAR (XCDR (tem));
      if (CONSP (val))
	val = XCDR (val);
    }
  else if (!NILP (Vcoding_system_for_write))
    val = Vcoding_system_for_write;
  else if ((!NILP (buffer) && NILP (BVAR (XBUFFER (buffer), enable_multibyte_characters)))
	   || (NILP (buffer) && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
    val = Qnil;
  pset_encode_coding_system (p, val);

  setup_process_coding_systems (proc);
  pset_decoding_buf (p, empty_unibyte_string);
  eassert (p->decoding_carryover == 0);
  pset_encoding_buf (p, empty_unibyte_string);
  p->inherit_coding_system_flag
    = !(!NILP (tem) || NILP (buffer) || !inherit_process_coding_system);

  Fserial_process_configure (nargs, args);

  specpdl_ptr = specpdl + specpdl_count;

  return proc;
}

static void
set_network_socket_coding_system (Lisp_Object proc, Lisp_Object host,
				  Lisp_Object service, Lisp_Object name)
{
  Lisp_Object tem;
  struct Lisp_Process *p = XPROCESS (proc);
  Lisp_Object contact = p->childp;
  Lisp_Object coding_systems = Qt;
  Lisp_Object val;

  tem = Fplist_member (contact, QCcoding);
  if (!NILP (tem) && (!CONSP (tem) || !CONSP (XCDR (tem))))
    tem = Qnil;  /* No error message (too late!).  */

  /* Setup coding systems for communicating with the network stream.  */
  /* Qt denotes we have not yet called Ffind_operation_coding_system.  */

  if (!NILP (tem))
    {
      val = XCAR (XCDR (tem));
      if (CONSP (val))
	val = XCAR (val);
    }
  else if (!NILP (Vcoding_system_for_read))
    val = Vcoding_system_for_read;
  else if ((!NILP (p->buffer)
	    && NILP (BVAR (XBUFFER (p->buffer), enable_multibyte_characters)))
	   || (NILP (p->buffer)
	       && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
    /* We dare not decode end-of-line format by setting VAL to
       Qraw_text, because the existing Emacs Lisp libraries
       assume that they receive bare code including a sequence of
       CR LF.  */
    val = Qnil;
  else
    {
      if (NILP (host) || NILP (service))
	coding_systems = Qnil;
      else
	coding_systems = CALLN (Ffind_operation_coding_system,
				Qopen_network_stream, name, p->buffer,
				host, service);
      if (CONSP (coding_systems))
	val = XCAR (coding_systems);
      else if (CONSP (Vdefault_process_coding_system))
	val = XCAR (Vdefault_process_coding_system);
      else
	val = Qnil;
    }
  pset_decode_coding_system (p, val);

  if (!NILP (tem))
    {
      val = XCAR (XCDR (tem));
      if (CONSP (val))
	val = XCDR (val);
    }
  else if (!NILP (Vcoding_system_for_write))
    val = Vcoding_system_for_write;
  else if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    val = Qnil;
  else
    {
      if (EQ (coding_systems, Qt))
	{
	  if (NILP (host) || NILP (service))
	    coding_systems = Qnil;
	  else
	    coding_systems = CALLN (Ffind_operation_coding_system,
				    Qopen_network_stream, name, p->buffer,
				    host, service);
	}
      if (CONSP (coding_systems))
	val = XCDR (coding_systems);
      else if (CONSP (Vdefault_process_coding_system))
	val = XCDR (Vdefault_process_coding_system);
      else
	val = Qnil;
    }
  pset_encode_coding_system (p, val);

  pset_decoding_buf (p, empty_unibyte_string);
  p->decoding_carryover = 0;
  pset_encoding_buf (p, empty_unibyte_string);

  p->inherit_coding_system_flag
    = !(!NILP (tem) || NILP (p->buffer) || !inherit_process_coding_system);
}

#ifdef HAVE_GNUTLS
static void
finish_after_tls_connection (Lisp_Object proc)
{
  struct Lisp_Process *p = XPROCESS (proc);
  Lisp_Object contact = p->childp;
  Lisp_Object result = Qt;

  if (!NILP (Ffboundp (Qnsm_verify_connection)))
    result = call3 (Qnsm_verify_connection,
		    proc,
		    Fplist_get (contact, QChost),
		    Fplist_get (contact, QCservice));

  if (NILP (result))
    {
      pset_status (p, list2 (Qfailed,
			     build_string ("The Network Security Manager stopped the connections")));
      deactivate_process (proc);
    }
  else if (p->outfd < 0)
    {
      /* The counterparty may have closed the connection (especially
	 if the NSM prompt above take a long time), so recheck the file
	 descriptor here. */
      pset_status (p, Qfailed);
      deactivate_process (proc);
    }
  else if ((fd_callback_info[p->outfd].flags & NON_BLOCKING_CONNECT_FD) == 0)
    {
      /* If we cleared the connection wait mask before we did the TLS
	 setup, then we have to say that the process is finally "open"
	 here. */
      pset_status (p, Qrun);
      /* Execute the sentinel here.  If we had relied on status_notify
	 to do it later, it will read input from the process before
	 calling the sentinel.  */
      exec_sentinel (proc, build_string ("open\n"));
    }
}
#endif

static void
connect_network_socket (Lisp_Object proc, Lisp_Object addrinfos,
                        Lisp_Object use_external_socket_p)
{
  ptrdiff_t count = SPECPDL_INDEX ();
  int s = -1, outch, inch;
  int xerrno = 0;
  int family;
  struct sockaddr *sa = NULL;
  int ret;
  ptrdiff_t addrlen;
  struct Lisp_Process *p = XPROCESS (proc);
  Lisp_Object contact = p->childp;
  int optbits = 0;
  int socket_to_use = -1;

  if (!NILP (use_external_socket_p))
    {
      socket_to_use = external_sock_fd;

      /* Ensure we don't consume the external socket twice.  */
      external_sock_fd = -1;
    }

  /* Do this in case we never enter the while-loop below.  */
  s = -1;

  while (!NILP (addrinfos))
    {
      Lisp_Object addrinfo = XCAR (addrinfos);
      addrinfos = XCDR (addrinfos);
      int protocol = XINT (XCAR (addrinfo));
      Lisp_Object ip_address = XCDR (addrinfo);

#ifdef WINDOWSNT
    retry_connect:
#endif

      addrlen = get_lisp_to_sockaddr_size (ip_address, &family);
      if (sa)
	free (sa);
      sa = xmalloc (addrlen);
      conv_lisp_to_sockaddr (family, ip_address, sa, addrlen);

      s = socket_to_use;
      if (s < 0)
	{
	  int socktype = p->socktype | SOCK_CLOEXEC;
	  if (p->is_non_blocking_client)
	    socktype |= SOCK_NONBLOCK;
	  s = socket (family, socktype, protocol);
	  if (s < 0)
	    {
	      xerrno = errno;
	      continue;
	    }
	}

      if (p->is_non_blocking_client && ! (SOCK_NONBLOCK && socket_to_use < 0))
	{
	  ret = fcntl (s, F_SETFL, O_NONBLOCK);
	  if (ret < 0)
	    {
	      xerrno = errno;
	      emacs_close (s);
	      s = -1;
	      if (0 <= socket_to_use)
		break;
	      continue;
	    }
	}

#ifdef DATAGRAM_SOCKETS
      if (!p->is_server && p->socktype == SOCK_DGRAM)
	break;
#endif /* DATAGRAM_SOCKETS */

      /* Make us close S if quit.  */
      record_unwind_protect_int (close_file_unwind, s);

      /* Parse network options in the arg list.  We simply ignore anything
	 which isn't a known option (including other keywords).  An error
	 is signaled if setting a known option fails.  */
      {
	Lisp_Object params = contact, key, val;

	while (!NILP (params))
	  {
	    key = XCAR (params);
	    params = XCDR (params);
	    val = XCAR (params);
	    params = XCDR (params);
	    optbits |= set_socket_option (s, key, val);
	  }
      }

      if (p->is_server)
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

          /* If passed a socket descriptor, it should be already bound. */
	  if (socket_to_use < 0 && bind (s, sa, addrlen) != 0)
	    report_file_error ("Cannot bind server socket", Qnil);

#ifdef HAVE_GETSOCKNAME
	  if (p->port == 0
#ifdef HAVE_LOCAL_SOCKETS
	      && family != AF_LOCAL
#endif
	      )
	    {
	      struct sockaddr_in sa1;
	      socklen_t len1 = sizeof (sa1);
#ifdef AF_INET6
	      /* The code below assumes the port is at the same offset
		 and of the same width in both IPv4 and IPv6
		 structures, but the standards don't guarantee that,
		 so verify it here.  */
	      struct sockaddr_in6 sa6;
	      verify ((offsetof (struct sockaddr_in, sin_port)
		       == offsetof (struct sockaddr_in6, sin6_port))
		      && sizeof (sa1.sin_port) == sizeof (sa6.sin6_port));
#endif
	      DECLARE_POINTER_ALIAS (psa1, struct sockaddr, &sa1);
	      if (getsockname (s, psa1, &len1) == 0)
		{
		  Lisp_Object service = make_number (ntohs (sa1.sin_port));
		  contact = Fplist_put (contact, QCservice, service);
		  /* Save the port number so that we can stash it in
		     the process object later.  */
		  DECLARE_POINTER_ALIAS (psa, struct sockaddr_in, sa);
		  psa->sin_port = sa1.sin_port;
		}
	    }
#endif

	  if (p->socktype != SOCK_DGRAM && listen (s, p->backlog))
	    report_file_error ("Cannot listen on server socket", Qnil);

	  break;
	}

      maybe_quit ();

      ret = connect (s, sa, addrlen);
      xerrno = errno;

      if (ret == 0 || xerrno == EISCONN)
	{
	  /* The unwind-protect will be discarded afterwards.  */
	  break;
	}

      if (p->is_non_blocking_client && xerrno == EINPROGRESS)
	break;

#ifndef WINDOWSNT
      if (xerrno == EINTR)
	{
	  /* Unlike most other syscalls connect() cannot be called
	     again.  (That would return EALREADY.)  The proper way to
	     wait for completion is pselect().  */
	  int sc;
	  socklen_t len;
	  fd_set fdset;
	retry_select:
	  FD_ZERO (&fdset);
	  FD_SET (s, &fdset);
	  maybe_quit ();
	  sc = pselect (s + 1, NULL, &fdset, NULL, NULL, NULL);
	  if (sc == -1)
	    {
	      if (errno == EINTR)
		goto retry_select;
	      else
		report_file_error ("Failed select", Qnil);
	    }
	  eassert (sc > 0);

	  len = sizeof xerrno;
	  eassert (FD_ISSET (s, &fdset));
	  if (getsockopt (s, SOL_SOCKET, SO_ERROR, &xerrno, &len) < 0)
	    report_file_error ("Failed getsockopt", Qnil);
	  if (xerrno == 0)
	    break;
	  if (NILP (addrinfos))
	    report_file_errno ("Failed connect", Qnil, xerrno);
	}
#endif /* !WINDOWSNT */

      /* Discard the unwind protect closing S.  */
      specpdl_ptr = specpdl + count;
      emacs_close (s);
      s = -1;
      if (0 <= socket_to_use)
	break;

#ifdef WINDOWSNT
      if (xerrno == EINTR)
	goto retry_connect;
#endif
    }

  if (s >= 0)
    {
#ifdef DATAGRAM_SOCKETS
      if (p->socktype == SOCK_DGRAM)
	{
	  if (datagram_address[s].sa)
	    emacs_abort ();

	  datagram_address[s].sa = xmalloc (addrlen);
	  datagram_address[s].len = addrlen;
	  if (p->is_server)
	    {
	      Lisp_Object remote;
	      memset (datagram_address[s].sa, 0, addrlen);
	      if (remote = Fplist_get (contact, QCremote), !NILP (remote))
		{
		  int rfamily;
		  ptrdiff_t rlen = get_lisp_to_sockaddr_size (remote, &rfamily);
		  if (rlen != 0 && rfamily == family
		      && rlen == addrlen)
		    conv_lisp_to_sockaddr (rfamily, remote,
					   datagram_address[s].sa, rlen);
		}
	    }
	  else
	    memcpy (datagram_address[s].sa, sa, addrlen);
	}
#endif

      contact = Fplist_put (contact, p->is_server? QClocal: QCremote,
			    conv_sockaddr_to_lisp (sa, addrlen));
#ifdef HAVE_GETSOCKNAME
      if (!p->is_server)
	{
	  struct sockaddr_storage sa1;
	  socklen_t len1 = sizeof (sa1);
	  DECLARE_POINTER_ALIAS (psa1, struct sockaddr, &sa1);
	  if (getsockname (s, psa1, &len1) == 0)
	    contact = Fplist_put (contact, QClocal,
				  conv_sockaddr_to_lisp (psa1, len1));
	}
#endif
    }

  if (s < 0)
    {
      /* If non-blocking got this far - and failed - assume non-blocking is
	 not supported after all.  This is probably a wrong assumption, but
	 the normal blocking calls to open-network-stream handles this error
	 better.  */
      if (p->is_non_blocking_client)
	return;

      report_file_errno ((p->is_server
			  ? "make server process failed"
			  : "make client process failed"),
			 contact, xerrno);
    }

  inch = s;
  outch = s;

  chan_process[inch] = proc;

  fcntl (inch, F_SETFL, O_NONBLOCK);

  p = XPROCESS (proc);
  p->open_fd[SUBPROCESS_STDIN] = inch;
  p->infd  = inch;
  p->outfd = outch;

  /* Discard the unwind protect for closing S, if any.  */
  specpdl_ptr = specpdl + count;

  if (p->is_server && p->socktype != SOCK_DGRAM)
    pset_status (p, Qlisten);

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (p->buffer))
    set_marker_both (p->mark, p->buffer,
		     BUF_ZV (XBUFFER (p->buffer)),
		     BUF_ZV_BYTE (XBUFFER (p->buffer)));

  if (p->is_non_blocking_client)
    {
      /* We may get here if connect did succeed immediately.  However,
	 in that case, we still need to signal this like a non-blocking
	 connection.  */
      if (! (connecting_status (p->status)
	     && EQ (XCDR (p->status), addrinfos)))
	pset_status (p, Fcons (Qconnect, addrinfos));
      if ((fd_callback_info[inch].flags & NON_BLOCKING_CONNECT_FD) == 0)
	add_non_blocking_write_fd (inch);
    }
  else
    /* A server may have a client filter setting of Qt, but it must
       still listen for incoming connects unless it is stopped.  */
    if ((!EQ (p->filter, Qt) && !EQ (p->command, Qt))
	|| (EQ (p->status, Qlisten) && NILP (p->command)))
      add_process_read_fd (inch);

  if (inch > max_desc)
    max_desc = inch;

  /* Set up the masks based on the process filter. */
  set_process_filter_masks (p);

  setup_process_coding_systems (proc);

#ifdef HAVE_GNUTLS
  /* Continue the asynchronous connection. */
  if (!NILP (p->gnutls_boot_parameters))
    {
      Lisp_Object boot, params = p->gnutls_boot_parameters;

      boot = Fgnutls_boot (proc, XCAR (params), XCDR (params));
      p->gnutls_boot_parameters = Qnil;

      if (p->gnutls_initstage == GNUTLS_STAGE_READY)
	/* Run sentinels, etc. */
	finish_after_tls_connection (proc);
      else if (p->gnutls_initstage != GNUTLS_STAGE_HANDSHAKE_TRIED)
	{
	  deactivate_process (proc);
	  if (NILP (boot))
	    pset_status (p, list2 (Qfailed,
				   build_string ("TLS negotiation failed")));
	  else
	    pset_status (p, list2 (Qfailed, boot));
	}
    }
#endif

}

/* Create a network stream/datagram client/server process.  Treated
   exactly like a normal process when reading and writing.  Primary
   differences are in status display and process deletion.  A network
   connection has no PID; you cannot signal it.  All you can do is
   stop/continue it and deactivate/close it via delete-process.  */

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
a random port number is selected for the server.  A port number can
be specified as an integer string, e.g., "80", as well as an integer.

:type TYPE -- TYPE is the type of connection.  The default (nil) is a
stream type connection, `datagram' creates a datagram type connection,
`seqpacket' creates a reliable datagram connection.

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

:nowait BOOL -- If NOWAIT is non-nil for a stream type client
process, return without waiting for the connection to complete;
instead, the sentinel function will be called with second arg matching
"open" (if successful) or "failed" when the connect completes.
Default is to use a blocking connect (i.e. wait) for stream type
connections.

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
If this keyword is not specified, the strings are multibyte.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:log LOG -- Install LOG as the server process log function.  This
function is called when the server accepts a network connection from a
client.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER
is the server process, CLIENT is the new process for the connection,
and MESSAGE is a string.

:plist PLIST -- Install PLIST as the new process's initial plist.

:tls-parameters LIST -- is a list that should be supplied if you're
opening a TLS connection.  The first element is the TLS type (either
`gnutls-x509pki' or `gnutls-anon'), and the remaining elements should
be a keyword list accepted by gnutls-boot (as returned by
`gnutls-boot-parameters').

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
:use-external-socket BOOL -- Use any pre-allocated sockets that have
                             been passed to Emacs.  If Emacs wasn't
                             passed a socket, this option is silently
                             ignored.


Consult the relevant system programmer's manual pages for more
information on using these options.


A server process will listen for and accept connections from clients.
When a client connection is accepted, a new network process is created
for the connection with the following parameters:

- The client's process name is constructed by concatenating the server
process's NAME and a client identification string.
- If the FILTER argument is non-nil, the client process will not get a
separate process buffer; otherwise, the client's process buffer is a newly
created buffer named after the server process's BUFFER name or process
NAME concatenated with the client identification string.
- The connection type and the process filter and sentinel parameters are
inherited from the server process's TYPE, FILTER and SENTINEL.
- The client process's contact info is set according to the client's
addressing information (typically an IP address and a port number).
- The client process's plist is initialized from the server's plist.

Notice that the FILTER and SENTINEL args are never used directly by
the server process.  Also, the BUFFER argument is not used directly by
the server process, but via the optional :log function, accepted (and
failed) connections may be logged in the server process's buffer.

The original argument list, modified with the actual connection
information, is available via the `process-contact' function.

usage: (make-network-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object proc;
  Lisp_Object contact;
  struct Lisp_Process *p;
  const char *portstring UNINIT;
  char portbuf[INT_BUFSIZE_BOUND (EMACS_INT)];
#ifdef HAVE_LOCAL_SOCKETS
  struct sockaddr_un address_un;
#endif
  EMACS_INT port = 0;
  Lisp_Object tem;
  Lisp_Object name, buffer, host, service, address;
  Lisp_Object filter, sentinel, use_external_socket_p;
  Lisp_Object addrinfos = Qnil;
  int socktype;
  int family = -1;
  enum { any_protocol = 0 };
#ifdef HAVE_GETADDRINFO_A
  struct gaicb *dns_request = NULL;
#endif
  ptrdiff_t count = SPECPDL_INDEX ();

  if (nargs == 0)
    return Qnil;

  /* Save arguments for process-contact and clone-process.  */
  contact = Flist (nargs, args);

#ifdef WINDOWSNT
  /* Ensure socket support is loaded if available.  */
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
#ifdef HAVE_SEQPACKET
  else if (EQ (tem, Qseqpacket))
    socktype = SOCK_SEQPACKET;
#endif
  else
    error ("Unsupported connection type");

  name = Fplist_get (contact, QCname);
  buffer = Fplist_get (contact, QCbuffer);
  filter = Fplist_get (contact, QCfilter);
  sentinel = Fplist_get (contact, QCsentinel);
  use_external_socket_p = Fplist_get (contact, QCuse_external_socket);

  CHECK_STRING (name);

  /* :local ADDRESS or :remote ADDRESS */
  tem = Fplist_get (contact, QCserver);
  if (NILP (tem))
    address = Fplist_get (contact, QCremote);
  else
    address = Fplist_get (contact, QClocal);
  if (!NILP (address))
    {
      host = service = Qnil;

      if (!get_lisp_to_sockaddr_size (address, &family))
	error ("Malformed :address");

      addrinfos = list1 (Fcons (make_number (any_protocol), address));
      goto open_socket;
    }

  /* :family FAMILY -- nil (for Inet), local, or integer.  */
  tem = Fplist_get (contact, QCfamily);
  if (NILP (tem))
    {
#ifdef AF_INET6
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
  else if (TYPE_RANGED_INTEGERP (int, tem))
    family = XINT (tem);
  else
    error ("Unknown address family");

  /* :service SERVICE -- string, integer (port number), or t (random port).  */
  service = Fplist_get (contact, QCservice);

  /* :host HOST -- hostname, ip address, or 'local for localhost.  */
  host = Fplist_get (contact, QChost);
  if (NILP (host))
    {
      /* The "connection" function gets it bind info from the address we're
	 given, so use this dummy address if nothing is specified. */
#ifdef HAVE_LOCAL_SOCKETS
      if (family != AF_LOCAL)
#endif
	host = build_string ("127.0.0.1");
    }
  else
    {
      if (EQ (host, Qlocal))
	/* Depending on setup, "localhost" may map to different IPv4 and/or
	   IPv6 addresses, so it's better to be explicit (Bug#6781).  */
	host = build_string ("127.0.0.1");
      CHECK_STRING (host);
    }

#ifdef HAVE_LOCAL_SOCKETS
  if (family == AF_LOCAL)
    {
      if (!NILP (host))
	{
	  message (":family local ignores the :host property");
	  contact = Fplist_put (contact, QChost, Qnil);
	  host = Qnil;
	}
      CHECK_STRING (service);
      if (sizeof address_un.sun_path <= SBYTES (service))
	error ("Service name too long");
      addrinfos = list1 (Fcons (make_number (any_protocol), service));
      goto open_socket;
    }
#endif

  /* Slow down polling to every ten seconds.
     Some kernels have a bug which causes retrying connect to fail
     after a connect.  Polling can interfere with gethostbyname too.  */
#ifdef POLL_FOR_INPUT
  if (socktype != SOCK_DGRAM)
    {
      record_unwind_protect_void (run_all_atimers);
      bind_polling_period (10);
    }
#endif

  if (!NILP (host))
    {
      ptrdiff_t portstringlen ATTRIBUTE_UNUSED;

      /* SERVICE can either be a string or int.
	 Convert to a C string for later use by getaddrinfo.  */
      if (EQ (service, Qt))
	{
	  portstring = "0";
	  portstringlen = 1;
	}
      else if (INTEGERP (service))
	{
	  portstring = portbuf;
	  portstringlen = sprintf (portbuf, "%"pI"d", XINT (service));
	}
      else
	{
	  CHECK_STRING (service);
	  portstring = SSDATA (service);
	  portstringlen = SBYTES (service);
	}

#ifdef HAVE_GETADDRINFO_A
      if (!NILP (Fplist_get (contact, QCnowait)))
	{
	  ptrdiff_t hostlen = SBYTES (host);
	  struct req
	  {
	    struct gaicb gaicb;
	    struct addrinfo hints;
	    char str[FLEXIBLE_ARRAY_MEMBER];
	  } *req = xmalloc (FLEXSIZEOF (struct req, str,
					hostlen + 1 + portstringlen + 1));
	  dns_request = &req->gaicb;
	  dns_request->ar_name = req->str;
	  dns_request->ar_service = req->str + hostlen + 1;
	  dns_request->ar_request = &req->hints;
	  dns_request->ar_result = NULL;
	  memset (&req->hints, 0, sizeof req->hints);
	  req->hints.ai_family = family;
	  req->hints.ai_socktype = socktype;
	  strcpy (req->str, SSDATA (host));
	  strcpy (req->str + hostlen + 1, portstring);

	  int ret = getaddrinfo_a (GAI_NOWAIT, &dns_request, 1, NULL);
	  if (ret)
	    error ("%s/%s getaddrinfo_a error %d",
		   SSDATA (host), portstring, ret);

	  goto open_socket;
	}
#endif /* HAVE_GETADDRINFO_A */
    }

  /* If we have a host, use getaddrinfo to resolve both host and service.
     Otherwise, use getservbyname to lookup the service.  */

  if (!NILP (host))
    {
      struct addrinfo *res, *lres;
      int ret;

      maybe_quit ();

      struct addrinfo hints;
      memset (&hints, 0, sizeof hints);
      hints.ai_family = family;
      hints.ai_socktype = socktype;

      ret = getaddrinfo (SSDATA (host), portstring, &hints, &res);
      if (ret)
#ifdef HAVE_GAI_STRERROR
	{
	  synchronize_system_messages_locale ();
	  char const *str = gai_strerror (ret);
	  if (! NILP (Vlocale_coding_system))
	    str = SSDATA (code_convert_string_norecord
			  (build_string (str), Vlocale_coding_system, 0));
	  error ("%s/%s %s", SSDATA (host), portstring, str);
	}
#else
	error ("%s/%s getaddrinfo error %d", SSDATA (host), portstring, ret);
#endif

      for (lres = res; lres; lres = lres->ai_next)
	addrinfos = Fcons (conv_addrinfo_to_lisp (lres), addrinfos);

      addrinfos = Fnreverse (addrinfos);

      freeaddrinfo (res);

      goto open_socket;
    }

  /* No hostname has been specified (e.g., a local server process).  */

  if (EQ (service, Qt))
    port = 0;
  else if (INTEGERP (service))
    port = XINT (service);
  else
    {
      CHECK_STRING (service);

      port = -1;
      if (SBYTES (service) != 0)
	{
	  /* Allow the service to be a string containing the port number,
	     because that's allowed if you have getaddrbyname.  */
	  char *service_end;
	  long int lport = strtol (SSDATA (service), &service_end, 10);
	  if (service_end == SSDATA (service) + SBYTES (service))
	    port = lport;
	  else
	    {
	      struct servent *svc_info
		= getservbyname (SSDATA (service),
				 socktype == SOCK_DGRAM ? "udp" : "tcp");
	      if (svc_info)
		port = ntohs (svc_info->s_port);
	    }
	}
    }

  if (! (0 <= port && port < 1 << 16))
    {
      AUTO_STRING (unknown_service, "Unknown service: %s");
      xsignal1 (Qerror, CALLN (Fformat, unknown_service, service));
    }

 open_socket:

  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  /* Unwind bind_polling_period.  */
  unbind_to (count, Qnil);

  proc = make_process (name);
  record_unwind_protect (remove_process, proc);
  p = XPROCESS (proc);
  pset_childp (p, contact);
  pset_plist (p, Fcopy_sequence (Fplist_get (contact, QCplist)));
  pset_type (p, Qnetwork);

  pset_buffer (p, buffer);
  pset_sentinel (p, sentinel);
  pset_filter (p, filter);
  pset_log (p, Fplist_get (contact, QClog));
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = 1;
  if ((tem = Fplist_get (contact, QCstop), !NILP (tem)))
    pset_command (p, Qt);
  eassert (p->pid == 0);
  p->backlog = 5;
  eassert (! p->is_non_blocking_client);
  eassert (! p->is_server);
  p->port = port;
  p->socktype = socktype;
#ifdef HAVE_GETADDRINFO_A
  eassert (! p->dns_request);
#endif
#ifdef HAVE_GNUTLS
  tem = Fplist_get (contact, QCtls_parameters);
  CHECK_LIST (tem);
  p->gnutls_boot_parameters = tem;
#endif

  set_network_socket_coding_system (proc, host, service, name);

  /* :server BOOL */
  tem = Fplist_get (contact, QCserver);
  if (!NILP (tem))
    {
      /* Don't support network sockets when non-blocking mode is
	 not available, since a blocked Emacs is not useful.  */
      p->is_server = true;
      if (TYPE_RANGED_INTEGERP (int, tem))
	p->backlog = XINT (tem);
    }

  /* :nowait BOOL */
  if (!p->is_server && socktype != SOCK_DGRAM
      && !NILP (Fplist_get (contact, QCnowait)))
    p->is_non_blocking_client = true;

  bool postpone_connection = false;
#ifdef HAVE_GETADDRINFO_A
  /* With async address resolution, the list of addresses is empty, so
     postpone connecting to the server. */
  if (!p->is_server && NILP (addrinfos))
    {
      p->dns_request = dns_request;
      p->status = list1 (Qconnect);
      postpone_connection = true;
    }
#endif
  if (! postpone_connection)
    connect_network_socket (proc, addrinfos, use_external_socket_p);

  specpdl_ptr = specpdl + count;
  return proc;
}


#ifdef HAVE_NET_IF_H

#ifdef SIOCGIFCONF
static Lisp_Object
network_interface_list (void)
{
  struct ifconf ifconf;
  struct ifreq *ifreq;
  void *buf = NULL;
  ptrdiff_t buf_size = 512;
  int s;
  Lisp_Object res;
  ptrdiff_t count;

  s = socket (AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
  if (s < 0)
    return Qnil;
  count = SPECPDL_INDEX ();
  record_unwind_protect_int (close_file_unwind, s);

  do
    {
      buf = xpalloc (buf, &buf_size, 1, INT_MAX, 1);
      ifconf.ifc_buf = buf;
      ifconf.ifc_len = buf_size;
      if (ioctl (s, SIOCGIFCONF, &ifconf))
	{
	  emacs_close (s);
	  xfree (buf);
	  return Qnil;
	}
    }
  while (ifconf.ifc_len == buf_size);

  res = unbind_to (count, Qnil);
  ifreq = ifconf.ifc_req;
  while ((char *) ifreq < (char *) ifconf.ifc_req + ifconf.ifc_len)
    {
      struct ifreq *ifq = ifreq;
#ifdef HAVE_STRUCT_IFREQ_IFR_ADDR_SA_LEN
#define SIZEOF_IFREQ(sif)						\
      ((sif)->ifr_addr.sa_len < sizeof (struct sockaddr)		\
       ? sizeof (*(sif)) : sizeof ((sif)->ifr_name) + (sif)->ifr_addr.sa_len)

      int len = SIZEOF_IFREQ (ifq);
#else
      int len = sizeof (*ifreq);
#endif
      char namebuf[sizeof (ifq->ifr_name) + 1];
      ifreq = (struct ifreq *) ((char *) ifreq + len);

      if (ifq->ifr_addr.sa_family != AF_INET)
	continue;

      memcpy (namebuf, ifq->ifr_name, sizeof (ifq->ifr_name));
      namebuf[sizeof (ifq->ifr_name)] = 0;
      res = Fcons (Fcons (build_string (namebuf),
			  conv_sockaddr_to_lisp (&ifq->ifr_addr,
						 sizeof (struct sockaddr))),
		   res);
    }

  xfree (buf);
  return res;
}
#endif /* SIOCGIFCONF */

#if defined (SIOCGIFADDR) || defined (SIOCGIFHWADDR) || defined (SIOCGIFFLAGS)

struct ifflag_def {
  int flag_bit;
  const char *flag_sym;
};

static const struct ifflag_def ifflag_table[] = {
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
#ifdef NS_IMPL_COCOA
  /* Really means smart, notrailers is obsolete.  */
  { IFF_NOTRAILERS,	"smart" },
#else
  { IFF_NOTRAILERS,	"notrailers" },
#endif
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
  { IFF_OACTIVE,	"oactive" }, /* OpenBSD: transmission in progress.  */
#endif
#ifdef IFF_SIMPLEX
  { IFF_SIMPLEX,	"simplex" }, /* OpenBSD: can't hear own transmissions.  */
#endif
#ifdef IFF_LINK0
  { IFF_LINK0,		"link0" }, /* OpenBSD: per link layer defined bit.  */
#endif
#ifdef IFF_LINK1
  { IFF_LINK1,		"link1" }, /* OpenBSD: per link layer defined bit.  */
#endif
#ifdef IFF_LINK2
  { IFF_LINK2,		"link2" }, /* OpenBSD: per link layer defined bit.  */
#endif
  { 0, 0 }
};

static Lisp_Object
network_interface_info (Lisp_Object ifname)
{
  struct ifreq rq;
  Lisp_Object res = Qnil;
  Lisp_Object elt;
  int s;
  bool any = 0;
  ptrdiff_t count;
#if (! (defined SIOCGIFHWADDR && defined HAVE_STRUCT_IFREQ_IFR_HWADDR)	\
     && defined HAVE_GETIFADDRS && defined LLADDR)
  struct ifaddrs *ifap;
#endif

  CHECK_STRING (ifname);

  if (sizeof rq.ifr_name <= SBYTES (ifname))
    error ("interface name too long");
  lispstpcpy (rq.ifr_name, ifname);

  s = socket (AF_INET, SOCK_STREAM | SOCK_CLOEXEC, 0);
  if (s < 0)
    return Qnil;
  count = SPECPDL_INDEX ();
  record_unwind_protect_int (close_file_unwind, s);

  elt = Qnil;
#if defined (SIOCGIFFLAGS) && defined (HAVE_STRUCT_IFREQ_IFR_FLAGS)
  if (ioctl (s, SIOCGIFFLAGS, &rq) == 0)
    {
      int flags = rq.ifr_flags;
      const struct ifflag_def *fp;
      int fnum;

      /* If flags is smaller than int (i.e. short) it may have the high bit set
         due to IFF_MULTICAST.  In that case, sign extending it into
         an int is wrong.  */
      if (flags < 0 && sizeof (rq.ifr_flags) < sizeof (flags))
        flags = (unsigned short) rq.ifr_flags;

      any = 1;
      for (fp = ifflag_table; flags != 0 && fp->flag_sym; fp++)
	{
	  if (flags & fp->flag_bit)
	    {
	      elt = Fcons (intern (fp->flag_sym), elt);
	      flags -= fp->flag_bit;
	    }
	}
      for (fnum = 0; flags && fnum < 32; flags >>= 1, fnum++)
	{
	  if (flags & 1)
	    {
	      elt = Fcons (make_number (fnum), elt);
	    }
	}
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFHWADDR) && defined (HAVE_STRUCT_IFREQ_IFR_HWADDR)
  if (ioctl (s, SIOCGIFHWADDR, &rq) == 0)
    {
      Lisp_Object hwaddr = Fmake_vector (make_number (6), Qnil);
      register struct Lisp_Vector *p = XVECTOR (hwaddr);
      int n;

      any = 1;
      for (n = 0; n < 6; n++)
	p->contents[n] = make_number (((unsigned char *)
				       &rq.ifr_hwaddr.sa_data[0])
				      [n]);
      elt = Fcons (make_number (rq.ifr_hwaddr.sa_family), hwaddr);
    }
#elif defined (HAVE_GETIFADDRS) && defined (LLADDR)
  if (getifaddrs (&ifap) != -1)
    {
      Lisp_Object hwaddr = Fmake_vector (make_number (6), Qnil);
      register struct Lisp_Vector *p = XVECTOR (hwaddr);
      struct ifaddrs *it;

      for (it = ifap; it != NULL; it = it->ifa_next)
        {
	  DECLARE_POINTER_ALIAS (sdl, struct sockaddr_dl, it->ifa_addr);
          unsigned char linkaddr[6];
          int n;

          if (it->ifa_addr->sa_family != AF_LINK
              || strcmp (it->ifa_name, SSDATA (ifname)) != 0
              || sdl->sdl_alen != 6)
            continue;

          memcpy (linkaddr, LLADDR (sdl), sdl->sdl_alen);
          for (n = 0; n < 6; n++)
            p->contents[n] = make_number (linkaddr[n]);

          elt = Fcons (make_number (it->ifa_addr->sa_family), hwaddr);
          break;
        }
    }
#ifdef HAVE_FREEIFADDRS
  freeifaddrs (ifap);
#endif

#endif /* HAVE_GETIFADDRS && LLADDR */

  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFNETMASK) && (defined (HAVE_STRUCT_IFREQ_IFR_NETMASK) || defined (HAVE_STRUCT_IFREQ_IFR_ADDR))
  if (ioctl (s, SIOCGIFNETMASK, &rq) == 0)
    {
      any = 1;
#ifdef HAVE_STRUCT_IFREQ_IFR_NETMASK
      elt = conv_sockaddr_to_lisp (&rq.ifr_netmask, sizeof (rq.ifr_netmask));
#else
      elt = conv_sockaddr_to_lisp (&rq.ifr_addr, sizeof (rq.ifr_addr));
#endif
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFBRDADDR) && defined (HAVE_STRUCT_IFREQ_IFR_BROADADDR)
  if (ioctl (s, SIOCGIFBRDADDR, &rq) == 0)
    {
      any = 1;
      elt = conv_sockaddr_to_lisp (&rq.ifr_broadaddr, sizeof (rq.ifr_broadaddr));
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFADDR) && defined (HAVE_STRUCT_IFREQ_IFR_ADDR)
  if (ioctl (s, SIOCGIFADDR, &rq) == 0)
    {
      any = 1;
      elt = conv_sockaddr_to_lisp (&rq.ifr_addr, sizeof (rq.ifr_addr));
    }
#endif
  res = Fcons (elt, res);

  return unbind_to (count, any ? res : Qnil);
}
#endif	/* !SIOCGIFADDR && !SIOCGIFHWADDR && !SIOCGIFFLAGS */
#endif	/* defined (HAVE_NET_IF_H) */

DEFUN ("network-interface-list", Fnetwork_interface_list,
       Snetwork_interface_list, 0, 0, 0,
       doc: /* Return an alist of all network interfaces and their network address.
Each element is a cons, the car of which is a string containing the
interface name, and the cdr is the network address in internal
format; see the description of ADDRESS in `make-network-process'.

If the information is not available, return nil.  */)
  (void)
{
#if (defined HAVE_NET_IF_H && defined SIOCGIFCONF) || defined WINDOWSNT
  return network_interface_list ();
#else
  return Qnil;
#endif
}

DEFUN ("network-interface-info", Fnetwork_interface_info,
       Snetwork_interface_info, 1, 1, 0,
       doc: /* Return information about network interface named IFNAME.
The return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),
where ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,
NETMASK is the layer 3 network mask, HWADDR is the layer 2 address, and
FLAGS is the current flags of the interface.

Data that is unavailable is returned as nil.  */)
  (Lisp_Object ifname)
{
#if ((defined HAVE_NET_IF_H			       \
      && (defined SIOCGIFADDR || defined SIOCGIFHWADDR \
	  || defined SIOCGIFFLAGS))		       \
     || defined WINDOWSNT)
  return network_interface_info (ifname);
#else
  return Qnil;
#endif
}

/* Turn off input and output for process PROC.  */

static void
deactivate_process (Lisp_Object proc)
{
  int inchannel;
  struct Lisp_Process *p = XPROCESS (proc);
  int i;

#ifdef HAVE_GNUTLS
  /* Delete GnuTLS structures in PROC, if any.  */
  emacs_gnutls_deinit (proc);
#endif /* HAVE_GNUTLS */

  if (p->read_output_delay > 0)
    {
      if (--process_output_delay_count < 0)
	process_output_delay_count = 0;
      p->read_output_delay = 0;
      p->read_output_skip = 0;
    }

  /* Beware SIGCHLD hereabouts.  */

  for (i = 0; i < PROCESS_OPEN_FDS; i++)
    close_process_fd (&p->open_fd[i]);

  inchannel = p->infd;
  if (inchannel >= 0)
    {
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
      delete_read_fd (inchannel);
      if ((fd_callback_info[inchannel].flags & NON_BLOCKING_CONNECT_FD) != 0)
	delete_write_fd (inchannel);
      if (inchannel == max_desc)
	recompute_max_desc ();
    }
}


DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
       0, 4, 0,
       doc: /* Allow any pending output from subprocesses to be read by Emacs.
It is given to their filter functions.
Optional argument PROCESS means do not return until output has been
received from PROCESS.

Optional second argument SECONDS and third argument MILLISEC
specify a timeout; return after that much time even if there is
no subprocess output.  If SECONDS is a floating point number,
it specifies a fractional number of seconds to wait.
The MILLISEC argument is obsolete and should be avoided.

If optional fourth argument JUST-THIS-ONE is non-nil, accept output
from PROCESS only, suspending reading output from other processes.
If JUST-THIS-ONE is an integer, don't run any timers either.
Return non-nil if we received any output from PROCESS (or, if PROCESS
is nil, from any process) before the timeout expired.  */)
  (Lisp_Object process, Lisp_Object seconds, Lisp_Object millisec,
   Lisp_Object just_this_one)
{
  intmax_t secs;
  int nsecs;

  if (! NILP (process))
    {
      CHECK_PROCESS (process);
      struct Lisp_Process *proc = XPROCESS (process);

      /* Can't wait for a process that is dedicated to a different
	 thread.  */
      if (!EQ (proc->thread, Qnil) && !EQ (proc->thread, Fcurrent_thread ()))
	{
	  Lisp_Object proc_thread_name = XTHREAD (proc->thread)->name;

	  if (STRINGP (proc_thread_name))
	    error ("Attempt to accept output from process %s locked to thread %s",
		   SDATA (proc->name), SDATA (proc_thread_name));
	  else
	    error ("Attempt to accept output from process %s locked to thread %p",
		   SDATA (proc->name), XTHREAD (proc->thread));
	}
    }
  else
    just_this_one = Qnil;

  if (!NILP (millisec))
    { /* Obsolete calling convention using integers rather than floats.  */
      CHECK_NUMBER (millisec);
      if (NILP (seconds))
	seconds = make_float (XINT (millisec) / 1000.0);
      else
	{
	  CHECK_NUMBER (seconds);
	  seconds = make_float (XINT (millisec) / 1000.0 + XINT (seconds));
	}
    }

  secs = 0;
  nsecs = -1;

  if (!NILP (seconds))
    {
      if (INTEGERP (seconds))
	{
	  if (XINT (seconds) > 0)
	    {
	      secs = XINT (seconds);
	      nsecs = 0;
	    }
	}
      else if (FLOATP (seconds))
	{
	  if (XFLOAT_DATA (seconds) > 0)
	    {
	      struct timespec t = dtotimespec (XFLOAT_DATA (seconds));
	      secs = min (t.tv_sec, WAIT_READING_MAX);
	      nsecs = t.tv_nsec;
	    }
	}
      else
	wrong_type_argument (Qnumberp, seconds);
    }
  else if (! NILP (process))
    nsecs = 0;

  return
    ((wait_reading_process_output (secs, nsecs, 0, 0,
				   Qnil,
				   !NILP (process) ? XPROCESS (process) : NULL,
				   (NILP (just_this_one) ? 0
				    : !INTEGERP (just_this_one) ? 1 : -1))
      <= 0)
     ? Qnil : Qt);
}

/* Accept a connection for server process SERVER on CHANNEL.  */

static EMACS_INT connect_counter = 0;

static void
server_accept_connection (Lisp_Object server, int channel)
{
  Lisp_Object buffer;
  Lisp_Object contact, host, service;
  struct Lisp_Process *ps = XPROCESS (server);
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
  socklen_t len = sizeof saddr;
  ptrdiff_t count;

  s = accept4 (channel, &saddr.sa, &len, SOCK_CLOEXEC);

  if (s < 0)
    {
      int code = errno;
      if (!would_block (code) && !NILP (ps->log))
	call3 (ps->log, server, Qnil,
	       concat3 (build_string ("accept failed with code"),
			Fnumber_to_string (make_number (code)),
			build_string ("\n")));
      return;
    }

  count = SPECPDL_INDEX ();
  record_unwind_protect_int (close_file_unwind, s);

  connect_counter++;

  /* Setup a new process to handle the connection.  */

  /* Generate a unique identification of the caller, and build contact
     information for this process.  */
  host = Qt;
  service = Qnil;
  Lisp_Object args[11];
  int nargs = 0;
  AUTO_STRING (procname_format_in, "%s <%d.%d.%d.%d:%d>");
  AUTO_STRING (procname_format_in6, "%s <[%x:%x:%x:%x:%x:%x:%x:%x]:%d>");
  AUTO_STRING (procname_format_default, "%s <%d>");
  switch (saddr.sa.sa_family)
    {
    case AF_INET:
      {
	args[nargs++] = procname_format_in;
	nargs++;
	unsigned char *ip = (unsigned char *)&saddr.in.sin_addr.s_addr;
	service = make_number (ntohs (saddr.in.sin_port));
	for (int i = 0; i < 4; i++)
	  args[nargs++] = make_number (ip[i]);
	args[nargs++] = service;
      }
      break;

#ifdef AF_INET6
    case AF_INET6:
      {
	args[nargs++] = procname_format_in6;
	nargs++;
	DECLARE_POINTER_ALIAS (ip6, uint16_t, &saddr.in6.sin6_addr);
	service = make_number (ntohs (saddr.in.sin_port));
	for (int i = 0; i < 8; i++)
	  args[nargs++] = make_number (ip6[i]);
	args[nargs++] = service;
      }
      break;
#endif

    default:
      args[nargs++] = procname_format_default;
      nargs++;
      args[nargs++] = make_number (connect_counter);
      break;
    }

  /* Create a new buffer name for this process if it doesn't have a
     filter.  The new buffer name is based on the buffer name or
     process name of the server process concatenated with the caller
     identification.  */

  if (!(EQ (ps->filter, Qinternal_default_process_filter)
	|| EQ (ps->filter, Qt)))
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
	  args[1] = buffer;
	  buffer = Fget_buffer_create (Fformat (nargs, args));
	}
    }

  /* Generate a unique name for the new server process.  Combine the
     server process name with the caller identification.  */

  args[1] = ps->name;
  Lisp_Object name = Fformat (nargs, args);
  Lisp_Object proc = make_process (name);

  chan_process[s] = proc;

  fcntl (s, F_SETFL, O_NONBLOCK);

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

  pset_childp (p, contact);
  pset_plist (p, Fcopy_sequence (ps->plist));
  pset_type (p, Qnetwork);

  pset_buffer (p, buffer);
  pset_sentinel (p, ps->sentinel);
  pset_filter (p, ps->filter);
  eassert (NILP (p->command));
  eassert (p->pid == 0);

  /* Discard the unwind protect for closing S.  */
  specpdl_ptr = specpdl + count;

  p->open_fd[SUBPROCESS_STDIN] = s;
  p->infd  = s;
  p->outfd = s;
  pset_status (p, Qrun);

  /* Client processes for accepted connections are not stopped initially.  */
  if (!EQ (p->filter, Qt))
    add_process_read_fd (s);
  if (s > max_desc)
    max_desc = s;

  /* Setup coding system for new process based on server process.
     This seems to be the proper thing to do, as the coding system
     of the new process should reflect the settings at the time the
     server socket was opened; not the current settings.  */

  pset_decode_coding_system (p, ps->decode_coding_system);
  pset_encode_coding_system (p, ps->encode_coding_system);
  setup_process_coding_systems (proc);

  pset_decoding_buf (p, empty_unibyte_string);
  eassert (p->decoding_carryover == 0);
  pset_encoding_buf (p, empty_unibyte_string);

  p->inherit_coding_system_flag
    = (NILP (buffer) ? 0 : ps->inherit_coding_system_flag);

  AUTO_STRING (dash, "-");
  AUTO_STRING (nl, "\n");
  Lisp_Object host_string = STRINGP (host) ? host : dash;

  if (!NILP (ps->log))
    {
      AUTO_STRING (accept_from, "accept from ");
      call3 (ps->log, server, proc, concat3 (accept_from, host_string, nl));
    }

  AUTO_STRING (open_from, "open from ");
  exec_sentinel (proc, concat3 (open_from, host_string, nl));
}

#ifdef HAVE_GETADDRINFO_A
static Lisp_Object
check_for_dns (Lisp_Object proc)
{
  struct Lisp_Process *p = XPROCESS (proc);
  Lisp_Object addrinfos = Qnil;

  /* Sanity check. */
  if (! p->dns_request)
    return Qnil;

  int ret = gai_error (p->dns_request);
  if (ret == EAI_INPROGRESS)
    return Qt;

  /* We got a response. */
  if (ret == 0)
    {
      struct addrinfo *res;

      for (res = p->dns_request->ar_result; res; res = res->ai_next)
	addrinfos = Fcons (conv_addrinfo_to_lisp (res), addrinfos);

      addrinfos = Fnreverse (addrinfos);
    }
  /* The DNS lookup failed. */
  else if (connecting_status (p->status))
    {
      deactivate_process (proc);
      pset_status (p, (list2
		       (Qfailed,
			concat3 (build_string ("Name lookup of "),
				 build_string (p->dns_request->ar_name),
				 build_string (" failed")))));
    }

  free_dns_request (proc);

  /* This process should not already be connected (or killed). */
  if (! connecting_status (p->status))
    return Qnil;

  return addrinfos;
}

#endif /* HAVE_GETADDRINFO_A */

static void
wait_for_socket_fds (Lisp_Object process, char const *name)
{
  while (XPROCESS (process)->infd < 0
	 && connecting_status (XPROCESS (process)->status))
    {
      add_to_log ("Waiting for socket from %s...", build_string (name));
      wait_reading_process_output (0, 20 * 1000 * 1000, 0, 0, Qnil, NULL, 0);
    }
}

static void
wait_while_connecting (Lisp_Object process)
{
  while (connecting_status (XPROCESS (process)->status))
    {
      add_to_log ("Waiting for connection...");
      wait_reading_process_output (0, 20 * 1000 * 1000, 0, 0, Qnil, NULL, 0);
    }
}

static void
wait_for_tls_negotiation (Lisp_Object process)
{
#ifdef HAVE_GNUTLS
  while (XPROCESS (process)->gnutls_p
	 && XPROCESS (process)->gnutls_initstage != GNUTLS_STAGE_READY)
    {
      add_to_log ("Waiting for TLS...");
      wait_reading_process_output (0, 20 * 1000 * 1000, 0, 0, Qnil, NULL, 0);
    }
#endif
}

static void
wait_reading_process_output_unwind (int data)
{
  clear_waiting_thread_info ();
  waiting_for_user_input_p = data;
}

/* This is here so breakpoints can be put on it.  */
static void
wait_reading_process_output_1 (void)
{
}

/* Read and dispose of subprocess output while waiting for timeout to
   elapse and/or keyboard input to be available.

   TIME_LIMIT is:
     timeout in seconds
     If negative, gobble data immediately available but don't wait for any.

   NSECS is:
     an additional duration to wait, measured in nanoseconds
     If TIME_LIMIT is zero, then:
       If NSECS == 0, there is no limit.
       If NSECS > 0, the timeout consists of NSECS only.
       If NSECS < 0, gobble data immediately, as if TIME_LIMIT were negative.

   READ_KBD is:
     0 to ignore keyboard input, or
     1 to return when input is available, or
    -1 meaning caller will actually read the input, so don't throw to
       the quit handler

   DO_DISPLAY means redisplay should be done to show subprocess
     output that arrives.

   If WAIT_FOR_CELL is a cons cell, wait until its car is non-nil
     (and gobble terminal input into the buffer if any arrives).

   If WAIT_PROC is specified, wait until something arrives from that
     process.

   If JUST_WAIT_PROC is nonzero, handle only output from WAIT_PROC
     (suspending output from other processes).  A negative value
     means don't run any timers either.

   Return positive if we received input from WAIT_PROC (or from any
   process if WAIT_PROC is null), zero if we attempted to receive
   input but got none, and negative if we didn't even try.  */

int
wait_reading_process_output (intmax_t time_limit, int nsecs, int read_kbd,
			     bool do_display,
			     Lisp_Object wait_for_cell,
			     struct Lisp_Process *wait_proc, int just_wait_proc)
{
  int channel, nfds;
  fd_set Available;
  fd_set Writeok;
  bool check_write;
  int check_delay;
  bool no_avail;
  int xerrno;
  Lisp_Object proc;
  struct timespec timeout, end_time, timer_delay;
  struct timespec got_output_end_time = invalid_timespec ();
  enum { MINIMUM = -1, TIMEOUT, INFINITY } wait;
  int got_some_output = -1;
#if defined HAVE_GETADDRINFO_A || defined HAVE_GNUTLS
  bool retry_for_async;
#endif
  ptrdiff_t count = SPECPDL_INDEX ();

  /* Close to the current time if known, an invalid timespec otherwise.  */
  struct timespec now = invalid_timespec ();

  eassert (wait_proc == NULL
	   || EQ (wait_proc->thread, Qnil)
	   || XTHREAD (wait_proc->thread) == current_thread);

  FD_ZERO (&Available);
  FD_ZERO (&Writeok);

  if (time_limit == 0 && nsecs == 0 && wait_proc && !NILP (Vinhibit_quit)
      && !(CONSP (wait_proc->status)
	   && EQ (XCAR (wait_proc->status), Qexit)))
    message1 ("Blocking call to accept-process-output with quit inhibited!!");

  record_unwind_protect_int (wait_reading_process_output_unwind,
			     waiting_for_user_input_p);
  waiting_for_user_input_p = read_kbd;

  if (TYPE_MAXIMUM (time_t) < time_limit)
    time_limit = TYPE_MAXIMUM (time_t);

  if (time_limit < 0 || nsecs < 0)
    wait = MINIMUM;
  else if (time_limit > 0 || nsecs > 0)
    {
      wait = TIMEOUT;
      now = current_timespec ();
      end_time = timespec_add (now, make_timespec (time_limit, nsecs));
    }
  else
    wait = INFINITY;

  while (1)
    {
      bool process_skipped = false;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	maybe_quit ();
      else if (pending_signals)
	process_pending_signals ();

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

#if defined HAVE_GETADDRINFO_A || defined HAVE_GNUTLS
      {
	Lisp_Object process_list_head, aproc;
	struct Lisp_Process *p;

	retry_for_async = false;
	FOR_EACH_PROCESS(process_list_head, aproc)
	  {
	    p = XPROCESS (aproc);

	    if (! wait_proc || p == wait_proc)
	      {
#ifdef HAVE_GETADDRINFO_A
		/* Check for pending DNS requests. */
		if (p->dns_request)
		  {
		    Lisp_Object addrinfos = check_for_dns (aproc);
		    if (!NILP (addrinfos) && !EQ (addrinfos, Qt))
		      connect_network_socket (aproc, addrinfos, Qnil);
		    else
		      retry_for_async = true;
		  }
#endif
#ifdef HAVE_GNUTLS
		/* Continue TLS negotiation. */
		if (p->gnutls_initstage == GNUTLS_STAGE_HANDSHAKE_TRIED
		    && p->is_non_blocking_client)
		  {
		    gnutls_try_handshake (p);
		    p->gnutls_handshakes_tried++;

		    if (p->gnutls_initstage == GNUTLS_STAGE_READY)
		      {
			gnutls_verify_boot (aproc, Qnil);
			finish_after_tls_connection (aproc);
		      }
		    else
		      {
			retry_for_async = true;
			if (p->gnutls_handshakes_tried
			    > GNUTLS_EMACS_HANDSHAKES_LIMIT)
			  {
			    deactivate_process (aproc);
			    pset_status (p, list2 (Qfailed,
						   build_string ("TLS negotiation failed")));
			  }
		      }
		  }
#endif
	      }
	  }
      }
#endif /* GETADDRINFO_A or GNUTLS */

      /* Compute time from now till when time limit is up.  */
      /* Exit if already run out.  */
      if (wait == TIMEOUT)
	{
	  if (!timespec_valid_p (now))
	    now = current_timespec ();
	  if (timespec_cmp (end_time, now) <= 0)
	    break;
	  timeout = timespec_sub (end_time, now);
	}
      else
	timeout = make_timespec (wait < TIMEOUT ? 0 : 100000, 0);

      /* Normally we run timers here.
	 But not if wait_for_cell; in those cases,
	 the wait is supposed to be short,
	 and those callers cannot handle running arbitrary Lisp code here.  */
      if (NILP (wait_for_cell)
	  && just_wait_proc >= 0)
	{
	  do
	    {
	      unsigned old_timers_run = timers_run;
	      struct buffer *old_buffer = current_buffer;
	      Lisp_Object old_window = selected_window;

	      timer_delay = timer_check ();

	      /* If a timer has run, this might have changed buffers
		 an alike.  Make read_key_sequence aware of that.  */
	      if (timers_run != old_timers_run
		  && (old_buffer != current_buffer
		      || !EQ (old_window, selected_window))
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

          /* This is so a breakpoint can be put here.  */
          if (!timespec_valid_p (timer_delay))
              wait_reading_process_output_1 ();
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
      if (update_tick != process_tick)
	{
	  fd_set Atemp;
	  fd_set Ctemp;

          if (kbd_on_hold_p ())
            FD_ZERO (&Atemp);
          else
            compute_input_wait_mask (&Atemp);
	  compute_write_mask (&Ctemp);

	  timeout = make_timespec (0, 0);
	  if ((thread_select (pselect, max_desc + 1,
			      &Atemp,
			      (num_pending_connects > 0 ? &Ctemp : NULL),
			      NULL, &timeout, NULL)
	       <= 0))
	    {
	      /* It's okay for us to do this and then continue with
		 the loop, since timeout has already been zeroed out.  */
	      clear_waiting_for_input ();
	      got_some_output = status_notify (NULL, wait_proc);
	      if (do_display) redisplay_preserve_echo_area (13);
	    }
	}

      /* Don't wait for output from a non-running process.  Just
	 read whatever data has already been received.  */
      if (wait_proc && wait_proc->raw_status_new)
	update_status (wait_proc);
      if (wait_proc
	  && ! EQ (wait_proc->status, Qrun)
	  && ! connecting_status (wait_proc->status))
	{
	  bool read_some_bytes = false;

	  clear_waiting_for_input ();

	  /* If data can be read from the process, do so until exhausted.  */
	  if (wait_proc->infd >= 0)
	    {
	      XSETPROCESS (proc, wait_proc);

	      while (true)
		{
		  int nread = read_process_output (proc, wait_proc->infd);
		  if (nread < 0)
		    {
		      if (errno == EIO || would_block (errno))
			break;
		    }
		  else
		    {
		      if (got_some_output < nread)
			got_some_output = nread;
		      if (nread == 0)
			break;
		      read_some_bytes = true;
		    }
		}
	    }

	  if (read_some_bytes && do_display)
	    redisplay_preserve_echo_area (10);

	  break;
	}

      /* Wait till there is something to do.  */

      if (wait_proc && just_wait_proc)
	{
	  if (wait_proc->infd < 0)  /* Terminated.  */
	    break;
	  FD_SET (wait_proc->infd, &Available);
	  check_delay = 0;
          check_write = 0;
	}
      else if (!NILP (wait_for_cell))
	{
	  compute_non_process_wait_mask (&Available);
	  check_delay = 0;
	  check_write = 0;
	}
      else
	{
	  if (! read_kbd)
	    compute_non_keyboard_wait_mask (&Available);
	  else
	    compute_input_wait_mask (&Available);
	  compute_write_mask (&Writeok);
 	  check_delay = wait_proc ? 0 : process_output_delay_count;
	  check_write = true;
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

      /* Skip the `select' call if input is available and we're
	 waiting for keyboard input or a cell change (which can be
	 triggered by processing X events).  In the latter case, set
	 nfds to 1 to avoid breaking the loop.  */
      no_avail = 0;
      if ((read_kbd || !NILP (wait_for_cell))
	  && detect_input_pending ())
	{
	  nfds = read_kbd ? 0 : 1;
	  no_avail = 1;
	  FD_ZERO (&Available);
	}
      else
	{
	  /* Set the timeout for adaptive read buffering if any
	     process has non-zero read_output_skip and non-zero
	     read_output_delay, and we are not reading output for a
	     specific process.  It is not executed if
	     Vprocess_adaptive_read_buffering is nil.  */
	  if (process_output_skip && check_delay > 0)
	    {
	      int adaptive_nsecs = timeout.tv_nsec;
	      if (timeout.tv_sec > 0 || adaptive_nsecs > READ_OUTPUT_DELAY_MAX)
		adaptive_nsecs = READ_OUTPUT_DELAY_MAX;
	      for (channel = 0; check_delay > 0 && channel <= max_desc; channel++)
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
		      process_skipped = true;
		      XPROCESS (proc)->read_output_skip = 0;
		      if (XPROCESS (proc)->read_output_delay < adaptive_nsecs)
			adaptive_nsecs = XPROCESS (proc)->read_output_delay;
		    }
		}
	      timeout = make_timespec (0, adaptive_nsecs);
	      process_output_skip = 0;
	    }

	  /* If we've got some output and haven't limited our timeout
	     with adaptive read buffering, limit it. */
	  if (got_some_output > 0 && !process_skipped
	      && (timeout.tv_sec
		  || timeout.tv_nsec > READ_OUTPUT_DELAY_INCREMENT))
	    timeout = make_timespec (0, READ_OUTPUT_DELAY_INCREMENT);


	  if (NILP (wait_for_cell) && just_wait_proc >= 0
	      && timespec_valid_p (timer_delay)
	      && timespec_cmp (timer_delay, timeout) < 0)
	    {
	      if (!timespec_valid_p (now))
		now = current_timespec ();
	      struct timespec timeout_abs = timespec_add (now, timeout);
	      if (!timespec_valid_p (got_output_end_time)
		  || timespec_cmp (timeout_abs, got_output_end_time) < 0)
		got_output_end_time = timeout_abs;
	      timeout = timer_delay;
	    }
	  else
	    got_output_end_time = invalid_timespec ();

	  /* NOW can become inaccurate if time can pass during pselect.  */
	  if (timeout.tv_sec > 0 || timeout.tv_nsec > 0)
	    now = invalid_timespec ();

#if defined HAVE_GETADDRINFO_A || defined HAVE_GNUTLS
	  if (retry_for_async
	      && (timeout.tv_sec > 0 || timeout.tv_nsec > ASYNC_RETRY_NSEC))
	    {
	      timeout.tv_sec = 0;
	      timeout.tv_nsec = ASYNC_RETRY_NSEC;
	    }
#endif

/* Non-macOS HAVE_GLIB builds call thread_select in xgselect.c.  */
#if defined HAVE_GLIB && !defined HAVE_NS
	  nfds = xg_select (max_desc + 1,
			    &Available, (check_write ? &Writeok : 0),
			    NULL, &timeout, NULL);
#elif defined HAVE_NS
          /* And NS builds call thread_select in ns_select. */
          nfds = ns_select (max_desc + 1,
			    &Available, (check_write ? &Writeok : 0),
			    NULL, &timeout, NULL);
#else  /* !HAVE_GLIB */
	  nfds = thread_select (pselect, max_desc + 1,
				&Available,
				(check_write ? &Writeok : 0),
				NULL, &timeout, NULL);
#endif	/* !HAVE_GLIB */

#ifdef HAVE_GNUTLS
          /* GnuTLS buffers data internally.  In lowat mode it leaves
             some data in the TCP buffers so that select works, but
             with custom pull/push functions we need to check if some
             data is available in the buffers manually.  */
          if (nfds == 0)
	    {
	      fd_set tls_available;
	      int set = 0;

	      FD_ZERO (&tls_available);
	      if (! wait_proc)
		{
		  /* We're not waiting on a specific process, so loop
		     through all the channels and check for data.
		     This is a workaround needed for some versions of
		     the gnutls library -- 2.12.14 has been confirmed
		     to need it.  See
		     http://comments.gmane.org/gmane.emacs.devel/145074 */
		  for (channel = 0; channel < FD_SETSIZE; ++channel)
		    if (! NILP (chan_process[channel]))
		      {
			struct Lisp_Process *p =
			  XPROCESS (chan_process[channel]);
			if (p && p->gnutls_p && p->gnutls_state
			    && ((emacs_gnutls_record_check_pending
				 (p->gnutls_state))
				> 0))
			  {
			    nfds++;
			    eassert (p->infd == channel);
			    FD_SET (p->infd, &tls_available);
			    set++;
			  }
		      }
		}
	      else
		{
		  /* Check this specific channel.  */
		  if (wait_proc->gnutls_p /* Check for valid process.  */
		      && wait_proc->gnutls_state
		      /* Do we have pending data?  */
		      && ((emacs_gnutls_record_check_pending
			   (wait_proc->gnutls_state))
			  > 0))
		    {
		      nfds = 1;
		      eassert (0 <= wait_proc->infd);
		      /* Set to Available.  */
		      FD_SET (wait_proc->infd, &tls_available);
		      set++;
		    }
		}
	      if (set)
		Available = tls_available;
	    }
#endif
	}

      xerrno = errno;

      /* Make C-g and alarm signals set flags again.  */
      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (nfds == 0)
	{
          /* Exit the main loop if we've passed the requested timeout,
             or aren't skipping processes and got some output and
             haven't lowered our timeout due to timers or SIGIO and
             have waited a long amount of time due to repeated
             timers.  */
	  struct timespec huge_timespec
	    = make_timespec (TYPE_MAXIMUM (time_t), 2 * TIMESPEC_RESOLUTION);
	  struct timespec cmp_time = huge_timespec;
	  if (wait < TIMEOUT)
	    break;
	  if (wait == TIMEOUT)
	    cmp_time = end_time;
	  if (!process_skipped && got_some_output > 0
	      && (timeout.tv_sec > 0 || timeout.tv_nsec > 0))
	    {
	      if (!timespec_valid_p (got_output_end_time))
		break;
	      if (timespec_cmp (got_output_end_time, cmp_time) < 0)
		cmp_time = got_output_end_time;
	    }
	  if (timespec_cmp (cmp_time, huge_timespec) < 0)
	    {
	      now = current_timespec ();
	      if (timespec_cmp (cmp_time, now) <= 0)
		break;
	    }
	}

      if (nfds < 0)
	{
	  if (xerrno == EINTR)
	    no_avail = 1;
	  else if (xerrno == EBADF)
	    emacs_abort ();
	  else
	    report_file_errno ("Failed select", Qnil, xerrno);
	}

      /* Check for keyboard input.  */
      /* If there is any, return immediately
	 to give it higher priority than subprocesses.  */

      if (read_kbd != 0)
	{
	  unsigned old_timers_run = timers_run;
	  struct buffer *old_buffer = current_buffer;
	  Lisp_Object old_window = selected_window;
	  bool leave = false;

	  if (detect_input_pending_run_timers (do_display))
	    {
	      swallow_events (do_display);
	      if (detect_input_pending_run_timers (do_display))
		leave = true;
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

#ifdef USABLE_SIGIO
      /* If we think we have keyboard input waiting, but didn't get SIGIO,
	 go read it.  This can happen with X on BSD after logging out.
	 In that case, there really is no input and no SIGIO,
	 but select says there is input.  */

      if (read_kbd && interrupt_input
	  && keyboard_bit_set (&Available) && ! noninteractive)
	handle_input_available_signal (SIGIO);
#endif

      /* If checking input just got us a size-change event from X,
	 obey it now if we should.  */
      if (read_kbd || ! NILP (wait_for_cell))
	do_pending_window_change (0);

      /* Check for data from a process.  */
      if (no_avail || nfds == 0)
	continue;

      for (channel = 0; channel <= max_desc; ++channel)
        {
          struct fd_callback_data *d = &fd_callback_info[channel];
          if (d->func
	      && ((d->flags & FOR_READ
		   && FD_ISSET (channel, &Available))
		  || ((d->flags & FOR_WRITE)
		      && FD_ISSET (channel, &Writeok))))
            d->func (channel, d->data);
	}

      for (channel = 0; channel <= max_desc; channel++)
	{
	  if (FD_ISSET (channel, &Available)
	      && ((fd_callback_info[channel].flags & (KEYBOARD_FD | PROCESS_FD))
		  == PROCESS_FD))
	    {
	      int nread;

	      /* If waiting for this channel, arrange to return as
		 soon as no more input to be processed.  No more
		 waiting.  */
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
	      if ((!wait_proc || wait_proc == XPROCESS (proc))
		  && got_some_output < nread)
		got_some_output = nread;
	      if (nread > 0)
		{
		  /* Vacuum up any leftovers without waiting.  */
		  if (wait_proc == XPROCESS (proc))
		    wait = MINIMUM;
		  /* Since read_process_output can run a filter,
		     which can call accept-process-output,
		     don't try to read from any other processes
		     before doing the select again.  */
		  FD_ZERO (&Available);

		  if (do_display)
		    redisplay_preserve_echo_area (12);
		}
	      else if (nread == -1 && would_block (errno))
		;
#ifdef HAVE_PTYS
	      /* On some OSs with ptys, when the process on one end of
		 a pty exits, the other end gets an error reading with
		 errno = EIO instead of getting an EOF (0 bytes read).
		 Therefore, if we get an error reading and errno =
		 EIO, just continue, because the child process has
		 exited and should clean itself up soon (e.g. when we
		 get a SIGCHLD).  */
	      else if (nread == -1 && errno == EIO)
		{
		  struct Lisp_Process *p = XPROCESS (proc);

		  /* Clear the descriptor now, so we only raise the
		     signal once.  */
		  delete_read_fd (channel);

		  if (p->pid == -2)
		    {
		      /* If the EIO occurs on a pty, the SIGCHLD handler's
			 waitpid call will not find the process object to
			 delete.  Do it here.  */
		      p->tick = ++process_tick;
		      pset_status (p, Qfailed);
		    }
		}
#endif /* HAVE_PTYS */
	      /* If we can detect process termination, don't consider the
		 process gone just because its pipe is closed.  */
	      else if (nread == 0 && !NETCONN_P (proc) && !SERIALCONN_P (proc)
		       && !PIPECONN_P (proc))
		;
	      else if (nread == 0 && PIPECONN_P (proc))
		{
		  /* Preserve status of processes already terminated.  */
		  XPROCESS (proc)->tick = ++process_tick;
		  deactivate_process (proc);
		  if (EQ (XPROCESS (proc)->status, Qrun))
		    pset_status (XPROCESS (proc),
				 list2 (Qexit, make_number (0)));
		}
	      else
		{
		  /* Preserve status of processes already terminated.  */
		  XPROCESS (proc)->tick = ++process_tick;
		  deactivate_process (proc);
		  if (XPROCESS (proc)->raw_status_new)
		    update_status (XPROCESS (proc));
		  if (EQ (XPROCESS (proc)->status, Qrun))
		    pset_status (XPROCESS (proc),
				 list2 (Qexit, make_number (256)));
		}
	    }
	  if (FD_ISSET (channel, &Writeok)
	      && (fd_callback_info[channel].flags
		  & NON_BLOCKING_CONNECT_FD) != 0)
	    {
	      struct Lisp_Process *p;

	      delete_write_fd (channel);

	      proc = chan_process[channel];
	      if (NILP (proc))
		continue;

	      p = XPROCESS (proc);

#ifndef WINDOWSNT
	      {
		socklen_t xlen = sizeof (xerrno);
		if (getsockopt (channel, SOL_SOCKET, SO_ERROR, &xerrno, &xlen))
		  xerrno = errno;
	      }
#else
	      /* On MS-Windows, getsockopt clears the error for the
		 entire process, which may not be the right thing; see
		 w32.c.  Use getpeername instead.  */
	      {
		struct sockaddr pname;
		socklen_t pnamelen = sizeof (pname);

		/* If connection failed, getpeername will fail.  */
		xerrno = 0;
		if (getpeername (channel, &pname, &pnamelen) < 0)
		  {
		    /* Obtain connect failure code through error slippage.  */
		    char dummy;
		    xerrno = errno;
		    if (errno == ENOTCONN && read (channel, &dummy, 1) < 0)
		      xerrno = errno;
		  }
	      }
#endif
	      if (xerrno)
		{
		  Lisp_Object addrinfos
		    = connecting_status (p->status) ? XCDR (p->status) : Qnil;
		  if (!NILP (addrinfos))
		    XSETCDR (p->status, XCDR (addrinfos));
		  else
		    {
		      p->tick = ++process_tick;
		      pset_status (p, list2 (Qfailed, make_number (xerrno)));
		    }
		  deactivate_process (proc);
		  if (!NILP (addrinfos))
		    connect_network_socket (proc, addrinfos, Qnil);
		}
	      else
		{
#ifdef HAVE_GNUTLS
		  /* If we have an incompletely set up TLS connection,
		     then defer the sentinel signaling until
		     later. */
		  if (NILP (p->gnutls_boot_parameters)
		      && !p->gnutls_p)
#endif
		    {
		      pset_status (p, Qrun);
		      /* Execute the sentinel here.  If we had relied on
			 status_notify to do it later, it will read input
			 from the process before calling the sentinel.  */
		      exec_sentinel (proc, build_string ("open\n"));
		    }

		  if (0 <= p->infd && !EQ (p->filter, Qt)
		      && !EQ (p->command, Qt))
		    add_process_read_fd (p->infd);
		}
	    }
	}			/* End for each file descriptor.  */
    }				/* End while exit conditions not met.  */

  unbind_to (count, Qnil);

  /* If calling from keyboard input, do not quit
     since we want to return C-g as an input character.
     Otherwise, do pending quit if requested.  */
  if (read_kbd >= 0)
    {
      /* Prevent input_pending from remaining set if we quit.  */
      clear_input_pending ();
      maybe_quit ();
    }

  return got_some_output;
}

/* Given a list (FUNCTION ARGS...), apply FUNCTION to the ARGS.  */

static Lisp_Object
read_process_output_call (Lisp_Object fun_and_args)
{
  return apply1 (XCAR (fun_and_args), XCDR (fun_and_args));
}

static Lisp_Object
read_process_output_error_handler (Lisp_Object error_val)
{
  cmd_error_internal (error_val, "error in process filter: ");
  Vinhibit_quit = Qt;
  update_echo_area ();
  Fsleep_for (make_number (2), Qnil);
  return Qt;
}

static void
read_and_dispose_of_process_output (struct Lisp_Process *p, char *chars,
				    ssize_t nbytes,
				    struct coding_system *coding);

/* Read pending output from the process channel,
   starting with our buffered-ahead character if we have one.
   Yield number of decoded characters read.

   This function reads at most 4096 characters.
   If you want to read all available subprocess output,
   you must call it repeatedly until it returns zero.

   The characters read are decoded according to PROC's coding-system
   for decoding.  */

static int
read_process_output (Lisp_Object proc, int channel)
{
  ssize_t nbytes;
  struct Lisp_Process *p = XPROCESS (proc);
  struct coding_system *coding = proc_decode_coding_system[channel];
  int carryover = p->decoding_carryover;
  enum { readmax = 4096 };
  ptrdiff_t count = SPECPDL_INDEX ();
  Lisp_Object odeactivate;
  char chars[sizeof coding->carryover + readmax];

  if (carryover)
    /* See the comment above.  */
    memcpy (chars, SDATA (p->decoding_buf), carryover);

#ifdef DATAGRAM_SOCKETS
  /* We have a working select, so proc_buffered_char is always -1.  */
  if (DATAGRAM_CHAN_P (channel))
    {
      socklen_t len = datagram_address[channel].len;
      nbytes = recvfrom (channel, chars + carryover, readmax,
			 0, datagram_address[channel].sa, &len);
    }
  else
#endif
    {
      bool buffered = proc_buffered_char[channel] >= 0;
      if (buffered)
	{
	  chars[carryover] = proc_buffered_char[channel];
	  proc_buffered_char[channel] = -1;
	}
#ifdef HAVE_GNUTLS
      if (p->gnutls_p && p->gnutls_state)
	nbytes = emacs_gnutls_read (p, chars + carryover + buffered,
				    readmax - buffered);
      else
#endif
	nbytes = emacs_read (channel, chars + carryover + buffered,
			     readmax - buffered);
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
	  else if (delay > 0 && nbytes == readmax - buffered)
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
      nbytes += buffered;
      nbytes += buffered && nbytes <= 0;
    }

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

  odeactivate = Vdeactivate_mark;
  /* There's no good reason to let process filters change the current
     buffer, and many callers of accept-process-output, sit-for, and
     friends don't expect current-buffer to be changed from under them.  */
  record_unwind_current_buffer ();

  read_and_dispose_of_process_output (p, chars, nbytes, coding);

  /* Handling the process output should not deactivate the mark.  */
  Vdeactivate_mark = odeactivate;

  unbind_to (count, Qnil);
  return nbytes;
}

static void
read_and_dispose_of_process_output (struct Lisp_Process *p, char *chars,
				    ssize_t nbytes,
				    struct coding_system *coding)
{
  Lisp_Object outstream = p->filter;
  Lisp_Object text;
  bool outer_running_asynch_code = running_asynch_code;
  int waiting = waiting_for_user_input_p;

#if 0
  Lisp_Object obuffer, okeymap;
  XSETBUFFER (obuffer, current_buffer);
  okeymap = BVAR (current_buffer, keymap);
#endif

  /* We inhibit quit here instead of just catching it so that
     hitting ^G when a filter happens to be running won't screw
     it up.  */
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

  decode_coding_c_string (coding, (unsigned char *) chars, nbytes, Qt);
  text = coding->dst_object;
  Vlast_coding_system_used = CODING_ID_NAME (coding->id);
  /* A new coding system might be found.  */
  if (!EQ (p->decode_coding_system, Vlast_coding_system_used))
    {
      pset_decode_coding_system (p, Vlast_coding_system_used);

      /* Don't call setup_coding_system for
	 proc_decode_coding_system[channel] here.  It is done in
	 detect_coding called via decode_coding above.  */

      /* If a coding system for encoding is not yet decided, we set
	 it as the same as coding-system for decoding.

	 But, before doing that we must check if
	 proc_encode_coding_system[p->outfd] surely points to a
	 valid memory because p->outfd will be changed once EOF is
	 sent to the process.  */
      if (NILP (p->encode_coding_system) && p->outfd >= 0
	  && proc_encode_coding_system[p->outfd])
	{
	  pset_encode_coding_system
	    (p, coding_inherit_eol_type (Vlast_coding_system_used, Qnil));
	  setup_coding_system (p->encode_coding_system,
			       proc_encode_coding_system[p->outfd]);
	}
    }

  if (coding->carryover_bytes > 0)
    {
      if (SCHARS (p->decoding_buf) < coding->carryover_bytes)
	pset_decoding_buf (p, make_uninit_string (coding->carryover_bytes));
      memcpy (SDATA (p->decoding_buf), coding->carryover,
	      coding->carryover_bytes);
      p->decoding_carryover = coding->carryover_bytes;
    }
  if (SBYTES (text) > 0)
    /* FIXME: It's wrong to wrap or not based on debug-on-error, and
       sometimes it's simply wrong to wrap (e.g. when called from
       accept-process-output).  */
    internal_condition_case_1 (read_process_output_call,
			       list3 (outstream, make_lisp_proc (p), text),
			       !NILP (Vdebug_on_error) ? Qnil : Qerror,
			       read_process_output_error_handler);

  /* If we saved the match data nonrecursively, restore it now.  */
  restore_search_regs ();
  running_asynch_code = outer_running_asynch_code;

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
}

DEFUN ("internal-default-process-filter", Finternal_default_process_filter,
       Sinternal_default_process_filter, 2, 2, 0,
       doc: /* Function used as default process filter.
This inserts the process's output into its buffer, if there is one.
Otherwise it discards the output.  */)
  (Lisp_Object proc, Lisp_Object text)
{
  struct Lisp_Process *p;
  ptrdiff_t opoint;

  CHECK_PROCESS (proc);
  p = XPROCESS (proc);
  CHECK_STRING (text);

  if (!NILP (p->buffer) && BUFFER_LIVE_P (XBUFFER (p->buffer)))
    {
      Lisp_Object old_read_only;
      ptrdiff_t old_begv, old_zv;
      ptrdiff_t old_begv_byte, old_zv_byte;
      ptrdiff_t before, before_byte;
      ptrdiff_t opoint_byte;
      struct buffer *b;

      Fset_buffer (p->buffer);
      opoint = PT;
      opoint_byte = PT_BYTE;
      old_read_only = BVAR (current_buffer, read_only);
      old_begv = BEGV;
      old_zv = ZV;
      old_begv_byte = BEGV_BYTE;
      old_zv_byte = ZV_BYTE;

      bset_read_only (current_buffer, Qnil);

      /* Insert new output into buffer at the current end-of-output
	 marker, thus preserving logical ordering of input and output.  */
      if (XMARKER (p->mark)->buffer)
	set_point_from_marker (p->mark);
      else
	SET_PT_BOTH (ZV, ZV_BYTE);
      before = PT;
      before_byte = PT_BYTE;

      /* If the output marker is outside of the visible region, save
	 the restriction and widen.  */
      if (! (BEGV <= PT && PT <= ZV))
	Fwiden ();

      /* Adjust the multibyteness of TEXT to that of the buffer.  */
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
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

      update_mode_lines = 23;

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

      bset_read_only (current_buffer, old_read_only);
      SET_PT_BOTH (opoint, opoint_byte);
    }
  return Qnil;
}

/* Sending data to subprocess.  */

/* In send_process, when a write fails temporarily,
   wait_reading_process_output is called.  It may execute user code,
   e.g. timers, that attempts to write new data to the same process.
   We must ensure that data is sent in the right order, and not
   interspersed half-completed with other writes (Bug#10815).  This is
   handled by the write_queue element of struct process.  It is a list
   with each entry having the form

   (string . (offset . length))

   where STRING is a lisp string, OFFSET is the offset into the
   string's byte sequence from which we should begin to send, and
   LENGTH is the number of bytes left to send.  */

/* Create a new entry in write_queue.
   INPUT_OBJ should be a buffer, string Qt, or Qnil.
   BUF is a pointer to the string sequence of the input_obj or a C
   string in case of Qt or Qnil.  */

static void
write_queue_push (struct Lisp_Process *p, Lisp_Object input_obj,
                  const char *buf, ptrdiff_t len, bool front)
{
  ptrdiff_t offset;
  Lisp_Object entry, obj;

  if (STRINGP (input_obj))
    {
      offset = buf - SSDATA (input_obj);
      obj = input_obj;
    }
  else
    {
      offset = 0;
      obj = make_unibyte_string (buf, len);
    }

  entry = Fcons (obj, Fcons (make_number (offset), make_number (len)));

  if (front)
    pset_write_queue (p, Fcons (entry, p->write_queue));
  else
    pset_write_queue (p, nconc2 (p->write_queue, list1 (entry)));
}

/* Remove the first element in the write_queue of process P, put its
   contents in OBJ, BUF and LEN, and return true.  If the
   write_queue is empty, return false.  */

static bool
write_queue_pop (struct Lisp_Process *p, Lisp_Object *obj,
		 const char **buf, ptrdiff_t *len)
{
  Lisp_Object entry, offset_length;
  ptrdiff_t offset;

  if (NILP (p->write_queue))
    return 0;

  entry = XCAR (p->write_queue);
  pset_write_queue (p, XCDR (p->write_queue));

  *obj = XCAR (entry);
  offset_length = XCDR (entry);

  *len = XINT (XCDR (offset_length));
  offset = XINT (XCAR (offset_length));
  *buf = SSDATA (*obj) + offset;

  return 1;
}

/* Send some data to process PROC.
   BUF is the beginning of the data; LEN is the number of characters.
   OBJECT is the Lisp object that the data comes from.  If OBJECT is
   nil or t, it means that the data comes from C string.

   If OBJECT is not nil, the data is encoded by PROC's coding-system
   for encoding before it is sent.

   This function can evaluate Lisp code and can garbage collect.  */

static void
send_process (Lisp_Object proc, const char *buf, ptrdiff_t len,
	      Lisp_Object object)
{
  struct Lisp_Process *p = XPROCESS (proc);
  ssize_t rv;
  struct coding_system *coding;

  if (NETCONN_P (proc))
    {
      wait_while_connecting (proc);
      wait_for_tls_negotiation (proc);
    }

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
	  && !NILP (BVAR (XBUFFER (object), enable_multibyte_characters)))
      || EQ (object, Qt))
    {
      pset_encode_coding_system
	(p, complement_process_encoding_system (p->encode_coding_system));
      if (!EQ (Vlast_coding_system_used, p->encode_coding_system))
	{
	  /* The coding system for encoding was changed to raw-text
	     because we sent a unibyte text previously.  Now we are
	     sending a multibyte text, thus we must encode it by the
	     original coding system specified for the current process.

	     Another reason we come here is that the coding system
	     was just complemented and a new one was returned by
	     complement_process_encoding_system.  */
	  setup_coding_system (p->encode_coding_system, coding);
	  Vlast_coding_system_used = p->encode_coding_system;
	}
      coding->src_multibyte = 1;
    }
  else
    {
      coding->src_multibyte = 0;
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
	  ptrdiff_t from_byte, from, to;
	  ptrdiff_t save_pt, save_pt_byte;
	  struct buffer *cur = current_buffer;

	  set_buffer_internal (XBUFFER (object));
	  save_pt = PT, save_pt_byte = PT_BYTE;

	  from_byte = PTR_BYTE_POS ((unsigned char *) buf);
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
	  encode_coding_object (coding, object, 0, 0, SCHARS (object),
				SBYTES (object), Qt);
	}
      else
	{
	  coding->dst_object = make_unibyte_string (buf, len);
	  coding->produced = len;
	}

      len = coding->produced;
      object = coding->dst_object;
      buf = SSDATA (object);
    }

  /* If there is already data in the write_queue, put the new data
     in the back of queue.  Otherwise, ignore it.  */
  if (!NILP (p->write_queue))
    write_queue_push (p, object, buf, len, 0);

  do   /* while !NILP (p->write_queue) */
    {
      ptrdiff_t cur_len = -1;
      const char *cur_buf;
      Lisp_Object cur_object;

      /* If write_queue is empty, ignore it.  */
      if (!write_queue_pop (p, &cur_object, &cur_buf, &cur_len))
	{
	  cur_len = len;
	  cur_buf = buf;
	  cur_object = object;
	}

      while (cur_len > 0)
	{
	  /* Send this batch, using one or more write calls.  */
	  ptrdiff_t written = 0;
	  int outfd = p->outfd;
#ifdef DATAGRAM_SOCKETS
	  if (DATAGRAM_CHAN_P (outfd))
	    {
	      rv = sendto (outfd, cur_buf, cur_len,
			   0, datagram_address[outfd].sa,
			   datagram_address[outfd].len);
	      if (rv >= 0)
		written = rv;
	      else if (errno == EMSGSIZE)
		report_file_error ("Sending datagram", proc);
	    }
	  else
#endif
	    {
#ifdef HAVE_GNUTLS
	      if (p->gnutls_p && p->gnutls_state)
		written = emacs_gnutls_write (p, cur_buf, cur_len);
	      else
#endif
		written = emacs_write_sig (outfd, cur_buf, cur_len);
	      rv = (written ? 0 : -1);
	      if (p->read_output_delay > 0
		  && p->adaptive_read_buffering == 1)
		{
		  p->read_output_delay = 0;
		  process_output_delay_count--;
		  p->read_output_skip = 0;
		}
	    }

	  if (rv < 0)
	    {
	      if (would_block (errno))
		/* Buffer is full.  Wait, accepting input;
		   that may allow the program
		   to finish doing output and read more.  */
		{
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

		  /* Put what we should have written in wait_queue.  */
		  write_queue_push (p, cur_object, cur_buf, cur_len, 1);
		  wait_reading_process_output (0, 20 * 1000 * 1000,
					       0, 0, Qnil, NULL, 0);
		  /* Reread queue, to see what is left.  */
		  break;
		}
	      else if (errno == EPIPE)
		{
		  p->raw_status_new = 0;
		  pset_status (p, list2 (Qexit, make_number (256)));
		  p->tick = ++process_tick;
		  deactivate_process (proc);
		  error ("process %s no longer connected to pipe; closed it",
			 SDATA (p->name));
		}
	      else
		/* This is a real error.  */
		report_file_error ("Writing to process", proc);
	    }
	  cur_buf += written;
	  cur_len -= written;
	}
    }
  while (!NILP (p->write_queue));
}

DEFUN ("process-send-region", Fprocess_send_region, Sprocess_send_region,
       3, 3, 0,
       doc: /* Send current contents of region as input to PROCESS.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
Called from program, takes three arguments, PROCESS, START and END.
If the region is more than 500 characters long,
it is sent in several bunches.  This may happen even for shorter regions.
Output from processes can arrive in between bunches.

If PROCESS is a non-blocking network process that hasn't been fully
set up yet, this function will block until socket setup has completed.  */)
  (Lisp_Object process, Lisp_Object start, Lisp_Object end)
{
  Lisp_Object proc = get_process (process);
  ptrdiff_t start_byte, end_byte;

  validate_region (&start, &end);

  start_byte = CHAR_TO_BYTE (XINT (start));
  end_byte = CHAR_TO_BYTE (XINT (end));

  if (XINT (start) < GPT && XINT (end) > GPT)
    move_gap_both (XINT (start), start_byte);

  if (NETCONN_P (proc))
    wait_while_connecting (proc);

  send_process (proc, (char *) BYTE_POS_ADDR (start_byte),
		end_byte - start_byte, Fcurrent_buffer ());

  return Qnil;
}

DEFUN ("process-send-string", Fprocess_send_string, Sprocess_send_string,
       2, 2, 0,
       doc: /* Send PROCESS the contents of STRING as input.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If STRING is more than 500 characters long,
it is sent in several bunches.  This may happen even for shorter strings.
Output from processes can arrive in between bunches.

If PROCESS is a non-blocking network process that hasn't been fully
set up yet, this function will block until socket setup has completed.  */)
  (Lisp_Object process, Lisp_Object string)
{
  CHECK_STRING (string);
  Lisp_Object proc = get_process (process);
  send_process (proc, SSDATA (string),
		SBYTES (string), string);
  return Qnil;
}

/* Return the foreground process group for the tty/pty that
   the process P uses.  */
static pid_t
emacs_get_tty_pgrp (struct Lisp_Process *p)
{
  pid_t gid = -1;

#ifdef TIOCGPGRP
  if (ioctl (p->infd, TIOCGPGRP, &gid) == -1 && ! NILP (p->tty_name))
    {
      int fd;
      /* Some OS:es (Solaris 8/9) does not allow TIOCGPGRP from the
	 master side.  Try the slave side.  */
      fd = emacs_open (SSDATA (p->tty_name), O_RDONLY, 0);

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
       doc: /* Return non-nil if PROCESS has given the terminal to a
child.  If the operating system does not make it possible to find out,
return t.  If we can find out, return the numeric ID of the foreground
process group.  */)
  (Lisp_Object process)
{
  /* Initialize in case ioctl doesn't exist or gives an error,
     in a way that will cause returning t.  */
  Lisp_Object proc = get_process (process);
  struct Lisp_Process *p = XPROCESS (proc);

  if (!EQ (p->type, Qreal))
    error ("Process %s is not a subprocess",
	   SDATA (p->name));
  if (p->infd < 0)
    error ("Process %s is not active",
	   SDATA (p->name));

  pid_t gid = emacs_get_tty_pgrp (p);

  if (gid == p->pid)
    return Qnil;
  if (gid != -1)
    return make_number (gid);
  return Qt;
}

/* Send a signal number SIGNO to PROCESS.
   If CURRENT_GROUP is t, that means send to the process group
   that currently owns the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If CURRENT_GROUP is lambda, that means send to the process group
   that currently owns the terminal, but only if it is NOT the shell itself.

   If NOMSG is false, insert signal-announcements into process's buffers
   right away.

   If we can, we try to signal PROCESS by sending control characters
   down the pty.  This allows us to signal inferiors who have changed
   their uid, for which kill would return an EPERM error.  */

static void
process_send_signal (Lisp_Object process, int signo, Lisp_Object current_group,
		     bool nomsg)
{
  Lisp_Object proc;
  struct Lisp_Process *p;
  pid_t gid;
  bool no_pgrp = 0;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->type, Qreal))
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
#ifdef VSWTCH
	  sig_char = &t.c_cc[VSWTCH];
#else
	  sig_char = &t.c_cc[VSUSP];
#endif
	  break;
	}

      if (sig_char && *sig_char != CDISABLE)
	{
	  send_process (proc, (char *) sig_char, 1, Qnil);
	  return;
	}
      /* If we can't send the signal with a character,
	 fall through and send it another way.  */

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
#else  /* ! defined (TIOCGPGRP) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = p->pid;
#endif /* ! defined (TIOCGPGRP) */

      /* If current_group is lambda, and the shell owns the terminal,
	 don't send any signal.  */
      if (EQ (current_group, Qlambda) && gid == p->pid)
	return;
    }

#ifdef SIGCONT
  if (signo == SIGCONT)
    {
      p->raw_status_new = 0;
      pset_status (p, Qrun);
      p->tick = ++process_tick;
      if (!nomsg)
	{
	  status_notify (NULL, NULL);
	  redisplay_preserve_echo_area (13);
	}
    }
#endif

#ifdef TIOCSIGSEND
  /* Work around a HP-UX 7.0 bug that mishandles signals to subjobs.
     We don't know whether the bug is fixed in later HP-UX versions.  */
  if (! NILP (current_group) && ioctl (p->infd, TIOCSIGSEND, signo) != -1)
    return;
#endif

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  pid_t pid = no_pgrp ? gid : - gid;

  /* Do not kill an already-reaped process, as that could kill an
     innocent bystander that happens to have the same process ID.  */
  sigset_t oldset;
  block_child_signal (&oldset);
  if (p->alive)
    kill (pid, signo);
  unblock_child_signal (&oldset);
}

DEFUN ("internal-default-interrupt-process",
       Finternal_default_interrupt_process,
       Sinternal_default_interrupt_process, 0, 2, 0,
       doc: /* Default function to interrupt process PROCESS.
It shall be the last element in list `interrupt-process-functions'.
See function `interrupt-process' for more details on usage.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  process_send_signal (process, SIGINT, current_group, 0);
  return process;
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
don't send the signal.

This function calls the functions of `interrupt-process-functions' in
the order of the list, until one of them returns non-`nil'.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  return CALLN (Frun_hook_with_args_until_success, Qinterrupt_process_functions,
		process, current_group);
}

DEFUN ("kill-process", Fkill_process, Skill_process, 0, 2, 0,
       doc: /* Kill process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  process_send_signal (process, SIGKILL, current_group, 0);
  return process;
}

DEFUN ("quit-process", Fquit_process, Squit_process, 0, 2, 0,
       doc: /* Send QUIT signal to process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  process_send_signal (process, SIGQUIT, current_group, 0);
  return process;
}

DEFUN ("stop-process", Fstop_process, Sstop_process, 0, 2, 0,
       doc: /* Stop process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
If PROCESS is a network or serial or pipe connection, inhibit handling
of incoming traffic.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  if (PROCESSP (process) && (NETCONN_P (process) || SERIALCONN_P (process)
			     || PIPECONN_P (process)))
    {
      struct Lisp_Process *p;

      p = XPROCESS (process);
      if (NILP (p->command)
	  && p->infd >= 0)
	delete_read_fd (p->infd);
      pset_command (p, Qt);
      return process;
    }
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
If PROCESS is a network or serial process, resume handling of incoming
traffic.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  if (PROCESSP (process) && (NETCONN_P (process) || SERIALCONN_P (process)
			     || PIPECONN_P (process)))
    {
      struct Lisp_Process *p;

      p = XPROCESS (process);
      if (EQ (p->command, Qt)
	  && p->infd >= 0
	  && (!EQ (p->filter, Qt) || EQ (p->status, Qlisten)))
	{
	  add_process_read_fd (p->infd);
#ifdef WINDOWSNT
	  if (fd_info[ p->infd ].flags & FILE_SERIAL)
	    PurgeComm (fd_info[ p->infd ].hnd, PURGE_RXABORT | PURGE_RXCLEAR);
#else /* not WINDOWSNT */
	  tcflush (p->infd, TCIFLUSH);
#endif /* not WINDOWSNT */
	}
      pset_command (p, Qnil);
      return process;
    }
#ifdef SIGCONT
    process_send_signal (process, SIGCONT, current_group, 0);
#else
    error ("No SIGCONT support");
#endif
  return process;
}

/* Return the integer value of the signal whose abbreviation is ABBR,
   or a negative number if there is no such signal.  */
static int
abbr_to_signal (char const *name)
{
  int i, signo;
  char sigbuf[20]; /* Large enough for all valid signal abbreviations.  */

  if (!strncmp (name, "SIG", 3) || !strncmp (name, "sig", 3))
    name += 3;

  for (i = 0; i < sizeof sigbuf; i++)
    {
      sigbuf[i] = c_toupper (name[i]);
      if (! sigbuf[i])
	return str2sig (sigbuf, &signo) == 0 ? signo : -1;
    }

  return -1;
}

DEFUN ("signal-process", Fsignal_process, Ssignal_process,
       2, 2, "sProcess (name or number): \nnSignal code: ",
       doc: /* Send PROCESS the signal with code SIGCODE.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.  */)
  (Lisp_Object process, Lisp_Object sigcode)
{
  pid_t pid;
  int signo;

  if (STRINGP (process))
    {
      Lisp_Object tem = Fget_process (process);
      if (NILP (tem))
	{
	  Lisp_Object process_number
	    = string_to_number (SSDATA (process), 10, 1);
	  if (NUMBERP (process_number))
	    tem = process_number;
	}
      process = tem;
    }
  else if (!NUMBERP (process))
    process = get_process (process);

  if (NILP (process))
    return process;

  if (NUMBERP (process))
    CONS_TO_INTEGER (process, pid_t, pid);
  else
    {
      CHECK_PROCESS (process);
      pid = XPROCESS (process)->pid;
      if (pid <= 0)
	error ("Cannot signal process %s", SDATA (XPROCESS (process)->name));
    }

  if (INTEGERP (sigcode))
    {
      CHECK_TYPE_RANGED_INTEGER (int, sigcode);
      signo = XINT (sigcode);
    }
  else
    {
      char *name;

      CHECK_SYMBOL (sigcode);
      name = SSDATA (SYMBOL_NAME (sigcode));

      signo = abbr_to_signal (name);
      if (signo < 0)
	error ("Undefined signal name %s", name);
    }

  return make_number (kill (pid, signo));
}

DEFUN ("process-send-eof", Fprocess_send_eof, Sprocess_send_eof, 0, 1, 0,
       doc: /* Make PROCESS see end-of-file in its input.
EOF comes after any text already sent to it.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If PROCESS is a network connection, or is a process communicating
through a pipe (as opposed to a pty), then you cannot send any more
text to PROCESS after you call this function.
If PROCESS is a serial process, wait until all output written to the
process has been transmitted to the serial port.  */)
  (Lisp_Object process)
{
  Lisp_Object proc;
  struct coding_system *coding = NULL;
  int outfd;

  proc = get_process (process);

  if (NETCONN_P (proc))
    wait_while_connecting (proc);

  if (DATAGRAM_CONN_P (proc))
    return process;


  outfd = XPROCESS (proc)->outfd;
  if (outfd >= 0)
    coding = proc_encode_coding_system[outfd];

  /* Make sure the process is really alive.  */
  if (XPROCESS (proc)->raw_status_new)
    update_status (XPROCESS (proc));
  if (! EQ (XPROCESS (proc)->status, Qrun))
    error ("Process %s not running", SDATA (XPROCESS (proc)->name));

  if (coding && CODING_REQUIRE_FLUSHING (coding))
    {
      coding->mode |= CODING_MODE_LAST_BLOCK;
      send_process (proc, "", 0, Qnil);
    }

  if (XPROCESS (proc)->pty_flag)
    send_process (proc, "\004", 1, Qnil);
  else if (EQ (XPROCESS (proc)->type, Qserial))
    {
#ifndef WINDOWSNT
      if (tcdrain (XPROCESS (proc)->outfd) != 0)
	report_file_error ("Failed tcdrain", Qnil);
#endif /* not WINDOWSNT */
      /* Do nothing on Windows because writes are blocking.  */
    }
  else
    {
      struct Lisp_Process *p = XPROCESS (proc);
      int old_outfd = p->outfd;
      int new_outfd;

#ifdef HAVE_SHUTDOWN
      /* If this is a network connection, or socketpair is used
	 for communication with the subprocess, call shutdown to cause EOF.
	 (In some old system, shutdown to socketpair doesn't work.
	 Then we just can't win.)  */
      if (0 <= old_outfd
	  && (EQ (p->type, Qnetwork) || p->infd == old_outfd))
	shutdown (old_outfd, 1);
#endif
      close_process_fd (&p->open_fd[WRITE_TO_SUBPROCESS]);
      new_outfd = emacs_open (NULL_DEVICE, O_WRONLY, 0);
      if (new_outfd < 0)
	report_file_error ("Opening null device", Qnil);
      p->open_fd[WRITE_TO_SUBPROCESS] = new_outfd;
      p->outfd = new_outfd;

      if (!proc_encode_coding_system[new_outfd])
	proc_encode_coding_system[new_outfd]
	  = xmalloc (sizeof (struct coding_system));
      if (old_outfd >= 0)
	{
	  *proc_encode_coding_system[new_outfd]
	    = *proc_encode_coding_system[old_outfd];
	  memset (proc_encode_coding_system[old_outfd], 0,
		  sizeof (struct coding_system));
	}
      else
	setup_coding_system (p->encode_coding_system,
			     proc_encode_coding_system[new_outfd]);
    }
  return process;
}

/* The main Emacs thread records child processes in three places:

   - Vprocess_alist, for asynchronous subprocesses, which are child
     processes visible to Lisp.

   - deleted_pid_list, for child processes invisible to Lisp,
     typically because of delete-process.  These are recorded so that
     the processes can be reaped when they exit, so that the operating
     system's process table is not cluttered by zombies.

   - the local variable PID in Fcall_process, call_process_cleanup and
     call_process_kill, for synchronous subprocesses.
     record_unwind_protect is used to make sure this process is not
     forgotten: if the user interrupts call-process and the child
     process refuses to exit immediately even with two C-g's,
     call_process_kill adds PID's contents to deleted_pid_list before
     returning.

   The main Emacs thread invokes waitpid only on child processes that
   it creates and that have not been reaped.  This avoid races on
   platforms such as GTK, where other threads create their own
   subprocesses which the main thread should not reap.  For example,
   if the main thread attempted to reap an already-reaped child, it
   might inadvertently reap a GTK-created process that happened to
   have the same process ID.  */

/* LIB_CHILD_HANDLER is a SIGCHLD handler that Emacs calls while doing
   its own SIGCHLD handling.  On POSIXish systems, glib needs this to
   keep track of its own children.  GNUstep is similar.  */

static void dummy_handler (int sig) {}
static signal_handler_t volatile lib_child_handler;

/* Handle a SIGCHLD signal by looking for known child processes of
   Emacs whose status have changed.  For each one found, record its
   new status.

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
   indirectly; if it does, that is a bug.  */

static void
handle_child_signal (int sig)
{
  Lisp_Object tail, proc;

  /* Find the process that signaled us, and record its status.  */

  /* The process can have been deleted by Fdelete_process, or have
     been started asynchronously by Fcall_process.  */
  for (tail = deleted_pid_list; CONSP (tail); tail = XCDR (tail))
    {
      bool all_pids_are_fixnums
	= (MOST_NEGATIVE_FIXNUM <= TYPE_MINIMUM (pid_t)
	   && TYPE_MAXIMUM (pid_t) <= MOST_POSITIVE_FIXNUM);
      Lisp_Object head = XCAR (tail);
      Lisp_Object xpid;
      if (! CONSP (head))
	continue;
      xpid = XCAR (head);
      if (all_pids_are_fixnums ? INTEGERP (xpid) : NUMBERP (xpid))
	{
	  pid_t deleted_pid;
	  if (INTEGERP (xpid))
	    deleted_pid = XINT (xpid);
	  else
	    deleted_pid = XFLOAT_DATA (xpid);
	  if (child_status_changed (deleted_pid, 0, 0))
	    {
	      if (STRINGP (XCDR (head)))
		unlink (SSDATA (XCDR (head)));
	      XSETCAR (tail, Qnil);
	    }
	}
    }

  /* Otherwise, if it is asynchronous, it is in Vprocess_alist.  */
  FOR_EACH_PROCESS (tail, proc)
    {
      struct Lisp_Process *p = XPROCESS (proc);
      int status;

      if (p->alive
	  && child_status_changed (p->pid, &status, WUNTRACED | WCONTINUED))
	{
	  /* Change the status of the process that was found.  */
	  p->tick = ++process_tick;
	  p->raw_status = status;
	  p->raw_status_new = 1;

	  /* If process has terminated, stop waiting for its output.  */
	  if (WIFSIGNALED (status) || WIFEXITED (status))
	    {
	      bool clear_desc_flag = 0;
	      p->alive = 0;
	      if (p->infd >= 0)
		clear_desc_flag = 1;

	      /* clear_desc_flag avoids a compiler bug in Microsoft C.  */
	      if (clear_desc_flag)
		delete_read_fd (p->infd);
	    }
	}
    }

  lib_child_handler (sig);
#ifdef NS_IMPL_GNUSTEP
  /* NSTask in GNUstep sets its child handler each time it is called.
     So we must re-set ours.  */
  catch_child_signal ();
#endif
}

static void
deliver_child_signal (int sig)
{
  deliver_process_signal (sig, handle_child_signal);
}


static Lisp_Object
exec_sentinel_error_handler (Lisp_Object error_val)
{
  /* Make sure error_val is a cons cell, as all the rest of error
     handling expects that, and will barf otherwise.  */
  if (!CONSP (error_val))
    error_val = Fcons (Qerror, error_val);
  cmd_error_internal (error_val, "error in process sentinel: ");
  Vinhibit_quit = Qt;
  update_echo_area ();
  Fsleep_for (make_number (2), Qnil);
  return Qt;
}

static void
exec_sentinel (Lisp_Object proc, Lisp_Object reason)
{
  Lisp_Object sentinel, odeactivate;
  struct Lisp_Process *p = XPROCESS (proc);
  ptrdiff_t count = SPECPDL_INDEX ();
  bool outer_running_asynch_code = running_asynch_code;
  int waiting = waiting_for_user_input_p;

  if (inhibit_sentinels)
    return;

  odeactivate = Vdeactivate_mark;
#if 0
  Lisp_Object obuffer, okeymap;
  XSETBUFFER (obuffer, current_buffer);
  okeymap = BVAR (current_buffer, keymap);
#endif

  /* There's no good reason to let sentinels change the current
     buffer, and many callers of accept-process-output, sit-for, and
     friends don't expect current-buffer to be changed from under them.  */
  record_unwind_current_buffer ();

  sentinel = p->sentinel;

  /* Inhibit quit so that random quits don't screw up a running filter.  */
  specbind (Qinhibit_quit, Qt);
  specbind (Qlast_nonmenu_event, Qt); /* Why? --Stef  */

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
			     list3 (sentinel, proc, reason),
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
   but can be done at other times.

   Return positive if any input was received from WAIT_PROC (or from
   any process if WAIT_PROC is null), zero if input was attempted but
   none received, and negative if we didn't even try.  */

static int
status_notify (struct Lisp_Process *deleting_process,
	       struct Lisp_Process *wait_proc)
{
  Lisp_Object proc;
  Lisp_Object tail, msg;
  int got_some_output = -1;

  tail = Qnil;
  msg = Qnil;

  /* Set this now, so that if new processes are created by sentinels
     that we run, we get called again to handle their status changes.  */
  update_tick = process_tick;

  FOR_EACH_PROCESS (tail, proc)
    {
      Lisp_Object symbol;
      register struct Lisp_Process *p = XPROCESS (proc);

      if (p->tick != p->update_tick)
	{
	  p->update_tick = p->tick;

	  /* If process is still active, read any output that remains.  */
	  while (! EQ (p->filter, Qt)
		 && ! connecting_status (p->status)
		 && ! EQ (p->status, Qlisten)
		 /* Network or serial process not stopped:  */
		 && ! EQ (p->command, Qt)
		 && p->infd >= 0
		 && p != deleting_process)
	    {
	      int nread = read_process_output (proc, p->infd);
	      if ((!wait_proc || wait_proc == XPROCESS (proc))
		  && got_some_output < nread)
		got_some_output = nread;
	      if (nread <= 0)
		break;
	    }

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
	     So set p->update_tick again so that an error in the sentinel will
	     not cause this code to be run again.  */
	  p->update_tick = p->tick;
	  /* Now output the message suitably.  */
	  exec_sentinel (proc, msg);
	  if (BUFFERP (p->buffer))
	    /* In case it uses %s in mode-line-format.  */
	    bset_update_mode_line (XBUFFER (p->buffer));
	}
    } /* end for */

  return got_some_output;
}

DEFUN ("internal-default-process-sentinel", Finternal_default_process_sentinel,
       Sinternal_default_process_sentinel, 2, 2, 0,
       doc: /* Function used as default sentinel for processes.
This inserts a status message into the process's buffer, if there is one.  */)
     (Lisp_Object proc, Lisp_Object msg)
{
  Lisp_Object buffer, symbol;
  struct Lisp_Process *p;
  CHECK_PROCESS (proc);
  p = XPROCESS (proc);
  buffer = p->buffer;
  symbol = p->status;
  if (CONSP (symbol))
    symbol = XCAR (symbol);

  if (!EQ (symbol, Qrun) && !NILP (buffer))
    {
      Lisp_Object tem;
      struct buffer *old = current_buffer;
      ptrdiff_t opoint, opoint_byte;
      ptrdiff_t before, before_byte;

      /* Avoid error if buffer is deleted
	 (probably that's why the process is dead, too).  */
      if (!BUFFER_LIVE_P (XBUFFER (buffer)))
	return Qnil;
      Fset_buffer (buffer);

      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	msg = (code_convert_string_norecord
	       (msg, Vlocale_coding_system, 1));

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

      tem = BVAR (current_buffer, read_only);
      bset_read_only (current_buffer, Qnil);
      insert_string ("\nProcess ");
      { /* FIXME: temporary kludge.  */
	Lisp_Object tem2 = p->name; Finsert (1, &tem2); }
      insert_string (" ");
      Finsert (1, &msg);
      bset_read_only (current_buffer, tem);
      set_marker_both (p->mark, p->buffer, PT, PT_BYTE);

      if (opoint >= before)
	SET_PT_BOTH (opoint + (PT - before),
		     opoint_byte + (PT_BYTE - before_byte));
      else
	SET_PT_BOTH (opoint, opoint_byte);

      set_buffer_internal (old);
    }
  return Qnil;
}


DEFUN ("set-process-coding-system", Fset_process_coding_system,
       Sset_process_coding_system, 1, 3, 0,
       doc: /* Set coding systems of PROCESS to DECODING and ENCODING.
DECODING will be used to decode subprocess output and ENCODING to
encode subprocess input. */)
  (Lisp_Object process, Lisp_Object decoding, Lisp_Object encoding)
{
  CHECK_PROCESS (process);

  struct Lisp_Process *p = XPROCESS (process);

  Fcheck_coding_system (decoding);
  Fcheck_coding_system (encoding);
  encoding = coding_inherit_eol_type (encoding, Qnil);
  pset_decode_coding_system (p, decoding);
  pset_encode_coding_system (p, encoding);

  /* If the sockets haven't been set up yet, the final setup part of
     this will be called asynchronously. */
  if (p->infd < 0 || p->outfd < 0)
    return Qnil;

  setup_process_coding_systems (process);

  return Qnil;
}

DEFUN ("process-coding-system",
       Fprocess_coding_system, Sprocess_coding_system, 1, 1, 0,
       doc: /* Return a cons of coding systems for decoding and encoding of PROCESS.  */)
  (register Lisp_Object process)
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
  (Lisp_Object process, Lisp_Object flag)
{
  CHECK_PROCESS (process);

  struct Lisp_Process *p = XPROCESS (process);
  if (NILP (flag))
    pset_decode_coding_system
      (p, raw_text_coding_system (p->decode_coding_system));

  /* If the sockets haven't been set up yet, the final setup part of
     this will be called asynchronously. */
  if (p->infd < 0 || p->outfd < 0)
    return Qnil;

  setup_process_coding_systems (process);

  return Qnil;
}

DEFUN ("process-filter-multibyte-p", Fprocess_filter_multibyte_p,
       Sprocess_filter_multibyte_p, 1, 1, 0,
       doc: /* Return t if a multibyte string is given to PROCESS's filter.*/)
  (Lisp_Object process)
{
  CHECK_PROCESS (process);
  struct Lisp_Process *p = XPROCESS (process);
  if (p->infd < 0)
    return Qnil;
  struct coding_system *coding = proc_decode_coding_system[p->infd];
  return (CODING_FOR_UNIBYTE (coding) ? Qnil : Qt);
}




# ifdef HAVE_GPM

void
add_gpm_wait_descriptor (int desc)
{
  add_keyboard_wait_descriptor (desc);
}

void
delete_gpm_wait_descriptor (int desc)
{
  delete_keyboard_wait_descriptor (desc);
}

# endif

# ifdef USABLE_SIGIO

/* Return true if *MASK has a bit set
   that corresponds to one of the keyboard input descriptors.  */

static bool
keyboard_bit_set (fd_set *mask)
{
  int fd;

  for (fd = 0; fd <= max_desc; fd++)
    if (FD_ISSET (fd, mask)
	&& ((fd_callback_info[fd].flags & (FOR_READ | KEYBOARD_FD))
	    == (FOR_READ | KEYBOARD_FD)))
      return 1;

  return 0;
}
# endif

#else  /* not subprocesses */

/* This is referenced in thread.c:run_thread (which is never actually
   called, since threads are not enabled for this configuration.  */
void
update_processes_for_thread_death (Lisp_Object dying_thread)
{
}

/* Defined in msdos.c.  */
extern int sys_select (int, fd_set *, fd_set *, fd_set *,
		       struct timespec *, void *);

/* Implementation of wait_reading_process_output, assuming that there
   are no subprocesses.  Used only by the MS-DOS build.

   Wait for timeout to elapse and/or keyboard input to be available.

   TIME_LIMIT is:
     timeout in seconds
     If negative, gobble data immediately available but don't wait for any.

   NSECS is:
     an additional duration to wait, measured in nanoseconds
     If TIME_LIMIT is zero, then:
       If NSECS == 0, there is no limit.
       If NSECS > 0, the timeout consists of NSECS only.
       If NSECS < 0, gobble data immediately, as if TIME_LIMIT were negative.

   READ_KBD is:
     0 to ignore keyboard input, or
     1 to return when input is available, or
     -1 means caller will actually read the input, so don't throw to
       the quit handler.

   see full version for other parameters. We know that wait_proc will
     always be NULL, since `subprocesses' isn't defined.

   DO_DISPLAY means redisplay should be done to show subprocess
   output that arrives.

   Return -1 signifying we got no output and did not try.  */

int
wait_reading_process_output (intmax_t time_limit, int nsecs, int read_kbd,
			     bool do_display,
			     Lisp_Object wait_for_cell,
			     struct Lisp_Process *wait_proc, int just_wait_proc)
{
  register int nfds;
  struct timespec end_time, timeout;
  enum { MINIMUM = -1, TIMEOUT, INFINITY } wait;

  if (TYPE_MAXIMUM (time_t) < time_limit)
    time_limit = TYPE_MAXIMUM (time_t);

  if (time_limit < 0 || nsecs < 0)
    wait = MINIMUM;
  else if (time_limit > 0 || nsecs > 0)
    {
      wait = TIMEOUT;
      end_time = timespec_add (current_timespec (),
                               make_timespec (time_limit, nsecs));
    }
  else
    wait = INFINITY;

  /* Turn off periodic alarms (in case they are in use)
     and then turn off any other atimers,
     because the select emulator uses alarms.  */
  stop_polling ();
  turn_on_atimers (0);

  while (1)
    {
      bool timeout_reduced_for_timers = false;
      fd_set waitchannels;
      int xerrno;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	maybe_quit ();

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

      /* Compute time from now till when time limit is up.  */
      /* Exit if already run out.  */
      if (wait == TIMEOUT)
	{
	  struct timespec now = current_timespec ();
	  if (timespec_cmp (end_time, now) <= 0)
	    break;
	  timeout = timespec_sub (end_time, now);
	}
      else
	timeout = make_timespec (wait < TIMEOUT ? 0 : 100000, 0);

      /* If our caller will not immediately handle keyboard events,
	 run timer events directly.
	 (Callers that will immediately read keyboard events
	 call timer_delay on their own.)  */
      if (NILP (wait_for_cell))
	{
	  struct timespec timer_delay;

	  do
	    {
	      unsigned old_timers_run = timers_run;
	      timer_delay = timer_check ();
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

	  if (timespec_valid_p (timer_delay))
	    {
	      if (timespec_cmp (timer_delay, timeout) < 0)
		{
		  timeout = timer_delay;
		  timeout_reduced_for_timers = true;
		}
	    }
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout.  */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* If a frame has been newly mapped and needs updating,
	 reprocess its display stuff.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (15);
	  if (read_kbd < 0)
	    set_waiting_for_input (&timeout);
	}

      /* Wait till there is something to do.  */
      FD_ZERO (&waitchannels);
      if (read_kbd && detect_input_pending ())
	nfds = 0;
      else
	{
	  if (read_kbd || !NILP (wait_for_cell))
	    FD_SET (0, &waitchannels);
	  nfds = pselect (1, &waitchannels, NULL, NULL, &timeout, NULL);
	}

      xerrno = errno;

      /* Make C-g and alarm signals set flags again.  */
      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (wait < INFINITY && nfds == 0 && ! timeout_reduced_for_timers)
	/* We waited the full specified time, so return now.  */
	break;

      if (nfds == -1)
	{
	  /* If the system call was interrupted, then go around the
	     loop again.  */
	  if (xerrno == EINTR)
	    FD_ZERO (&waitchannels);
	  else
	    report_file_errno ("Failed select", Qnil, xerrno);
	}

      /* Check for keyboard input.  */

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

  return -1;
}

#endif	/* not subprocesses */

/* The following functions are needed even if async subprocesses are
   not supported.  Some of them are no-op stubs in that case.  */

#ifdef HAVE_TIMERFD

/* Add FD, which is a descriptor returned by timerfd_create,
   to the set of non-keyboard input descriptors.  */

void
add_timer_wait_descriptor (int fd)
{
  add_read_fd (fd, timerfd_callback, NULL);
  fd_callback_info[fd].flags &= ~KEYBOARD_FD;
}

#endif /* HAVE_TIMERFD */

/* If program file NAME starts with /: for quoting a magic
   name, remove that, preserving the multibyteness of NAME.  */

Lisp_Object
remove_slash_colon (Lisp_Object name)
{
  return
    (SREF (name, 0) == '/' && SREF (name, 1) == ':'
     ? make_specified_string (SSDATA (name) + 2, SCHARS (name) - 2,
			      SBYTES (name) - 2, STRING_MULTIBYTE (name))
     : name);
}

/* Add DESC to the set of keyboard input descriptors.  */

void
add_keyboard_wait_descriptor (int desc)
{
#ifdef subprocesses /* Actually means "not MSDOS".  */
  eassert (desc >= 0 && desc < FD_SETSIZE);
  fd_callback_info[desc].flags &= ~PROCESS_FD;
  fd_callback_info[desc].flags |= (FOR_READ | KEYBOARD_FD);
  if (desc > max_desc)
    max_desc = desc;
#endif
}

/* From now on, do not expect DESC to give keyboard input.  */

void
delete_keyboard_wait_descriptor (int desc)
{
#ifdef subprocesses
  eassert (desc >= 0 && desc < FD_SETSIZE);

  fd_callback_info[desc].flags &= ~(FOR_READ | KEYBOARD_FD | PROCESS_FD);

  if (desc == max_desc)
    recompute_max_desc ();
#endif
}

/* Setup coding systems of PROCESS.  */

void
setup_process_coding_systems (Lisp_Object process)
{
#ifdef subprocesses
  struct Lisp_Process *p = XPROCESS (process);
  int inch = p->infd;
  int outch = p->outfd;
  Lisp_Object coding_system;

  if (inch < 0 || outch < 0)
    return;

  if (!proc_decode_coding_system[inch])
    proc_decode_coding_system[inch] = xmalloc (sizeof (struct coding_system));
  coding_system = p->decode_coding_system;
  if (EQ (p->filter, Qinternal_default_process_filter)
      && BUFFERP (p->buffer))
    {
      if (NILP (BVAR (XBUFFER (p->buffer), enable_multibyte_characters)))
	coding_system = raw_text_coding_system (coding_system);
    }
  setup_coding_system (coding_system, proc_decode_coding_system[inch]);

  if (!proc_encode_coding_system[outch])
    proc_encode_coding_system[outch] = xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->encode_coding_system,
		       proc_encode_coding_system[outch]);
#endif
}

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
       doc: /* Return the (or a) live process associated with BUFFER.
BUFFER may be a buffer or the name of one.
Return nil if all processes associated with BUFFER have been
deleted or killed.  */)
  (register Lisp_Object buffer)
{
#ifdef subprocesses
  register Lisp_Object buf, tail, proc;

  if (NILP (buffer)) return Qnil;
  buf = Fget_buffer (buffer);
  if (NILP (buf)) return Qnil;

  FOR_EACH_PROCESS (tail, proc)
    if (EQ (XPROCESS (proc)->buffer, buf))
      return proc;
#endif	/* subprocesses */
  return Qnil;
}

DEFUN ("process-inherit-coding-system-flag",
       Fprocess_inherit_coding_system_flag, Sprocess_inherit_coding_system_flag,
       1, 1, 0,
       doc: /* Return the value of inherit-coding-system flag for PROCESS.
If this flag is t, `buffer-file-coding-system' of the buffer
associated with PROCESS will inherit the coding system used to decode
the process output.  */)
  (register Lisp_Object process)
{
#ifdef subprocesses
  CHECK_PROCESS (process);
  return XPROCESS (process)->inherit_coding_system_flag ? Qt : Qnil;
#else
  /* Ignore the argument and return the value of
     inherit-process-coding-system.  */
  return inherit_process_coding_system ? Qt : Qnil;
#endif
}

/* Kill all processes associated with `buffer'.
   If `buffer' is nil, kill all processes.  */

void
kill_buffer_processes (Lisp_Object buffer)
{
#ifdef subprocesses
  Lisp_Object tail, proc;

  FOR_EACH_PROCESS (tail, proc)
    if (NILP (buffer) || EQ (XPROCESS (proc)->buffer, buffer))
      {
	if (NETCONN_P (proc) || SERIALCONN_P (proc) || PIPECONN_P (proc))
	  Fdelete_process (proc);
	else if (XPROCESS (proc)->infd >= 0)
	  process_send_signal (proc, SIGHUP, Qnil, 1);
      }
#else  /* subprocesses */
  /* Since we have no subprocesses, this does nothing.  */
#endif /* subprocesses */
}

DEFUN ("waiting-for-user-input-p", Fwaiting_for_user_input_p,
       Swaiting_for_user_input_p, 0, 0, 0,
       doc: /* Return non-nil if Emacs is waiting for input from the user.
This is intended for use by asynchronous process output filters and sentinels.  */)
  (void)
{
#ifdef subprocesses
  return (waiting_for_user_input_p ? Qt : Qnil);
#else
  return Qnil;
#endif
}

/* Stop reading input from keyboard sources.  */

void
hold_keyboard_input (void)
{
  kbd_is_on_hold = 1;
}

/* Resume reading input from keyboard sources.  */

void
unhold_keyboard_input (void)
{
  kbd_is_on_hold = 0;
}

/* Return true if keyboard input is on hold, zero otherwise.  */

bool
kbd_on_hold_p (void)
{
  return kbd_is_on_hold;
}


/* Enumeration of and access to system processes a-la ps(1).  */

DEFUN ("list-system-processes", Flist_system_processes, Slist_system_processes,
       0, 0, 0,
       doc: /* Return a list of numerical process IDs of all running processes.
If this functionality is unsupported, return nil.

See `process-attributes' for getting attributes of a process given its ID.  */)
  (void)
{
  return list_system_processes ();
}

DEFUN ("process-attributes", Fprocess_attributes,
       Sprocess_attributes, 1, 1, 0,
       doc: /* Return attributes of the process given by its PID, a number.

Value is an alist where each element is a cons cell of the form

    (KEY . VALUE)

If this functionality is unsupported, the value is nil.

See `list-system-processes' for getting a list of all process IDs.

The KEYs of the attributes that this function may return are listed
below, together with the type of the associated VALUE (in parentheses).
Not all platforms support all of these attributes; unsupported
attributes will not appear in the returned alist.
Unless explicitly indicated otherwise, numbers can have either
integer or floating point values.

 euid    -- Effective user User ID of the process (number)
 user    -- User name corresponding to euid (string)
 egid    -- Effective user Group ID of the process (number)
 group   -- Group name corresponding to egid (string)
 comm    -- Command name (executable name only) (string)
 state   -- Process state code, such as "S", "R", or "T" (string)
 ppid    -- Parent process ID (number)
 pgrp    -- Process group ID (number)
 sess    -- Session ID, i.e. process ID of session leader (number)
 ttname  -- Controlling tty name (string)
 tpgid   -- ID of foreground process group on the process's tty (number)
 minflt  -- number of minor page faults (number)
 majflt  -- number of major page faults (number)
 cminflt -- cumulative number of minor page faults (number)
 cmajflt -- cumulative number of major page faults (number)
 utime   -- user time used by the process, in (current-time) format,
              which is a list of integers (HIGH LOW USEC PSEC)
 stime   -- system time used by the process (current-time)
 time    -- sum of utime and stime (current-time)
 cutime  -- user time used by the process and its children (current-time)
 cstime  -- system time used by the process and its children (current-time)
 ctime   -- sum of cutime and cstime (current-time)
 pri     -- priority of the process (number)
 nice    -- nice value of the process (number)
 thcount -- process thread count (number)
 start   -- time the process started (current-time)
 vsize   -- virtual memory size of the process in KB's (number)
 rss     -- resident set size of the process in KB's (number)
 etime   -- elapsed time the process is running, in (HIGH LOW USEC PSEC) format
 pcpu    -- percents of CPU time used by the process (floating-point number)
 pmem    -- percents of total physical memory used by process's resident set
              (floating-point number)
 args    -- command line which invoked the process (string).  */)
  ( Lisp_Object pid)
{
  return system_process_attributes (pid);
}

#ifdef subprocesses
/* Arrange to catch SIGCHLD if this hasn't already been arranged.
   Invoke this after init_process_emacs, and after glib and/or GNUstep
   futz with the SIGCHLD handler, but before Emacs forks any children.
   This function's caller should block SIGCHLD.  */

void
catch_child_signal (void)
{
  struct sigaction action, old_action;
  sigset_t oldset;
  emacs_sigaction_init (&action, deliver_child_signal);
  block_child_signal (&oldset);
  sigaction (SIGCHLD, &action, &old_action);
  eassert (old_action.sa_handler == SIG_DFL || old_action.sa_handler == SIG_IGN
	   || ! (old_action.sa_flags & SA_SIGINFO));

  if (old_action.sa_handler != deliver_child_signal)
    lib_child_handler
      = (old_action.sa_handler == SIG_DFL || old_action.sa_handler == SIG_IGN
	 ? dummy_handler
	 : old_action.sa_handler);
  unblock_child_signal (&oldset);
}
#endif	/* subprocesses */

/* Limit the number of open files to the value it had at startup.  */

void
restore_nofile_limit (void)
{
#ifdef HAVE_SETRLIMIT
  if (FD_SETSIZE < nofile_limit.rlim_cur)
    setrlimit (RLIMIT_NOFILE, &nofile_limit);
#endif
}


/* This is not called "init_process" because that is the name of a
   Mach system call, so it would cause problems on Darwin systems.  */
void
init_process_emacs (int sockfd)
{
#ifdef subprocesses
  int i;

  inhibit_sentinels = 0;

#ifndef CANNOT_DUMP
  if (! noninteractive || initialized)
#endif
    {
#if defined HAVE_GLIB && !defined WINDOWSNT
      /* Tickle glib's child-handling code.  Ask glib to wait for Emacs itself;
	 this should always fail, but is enough to initialize glib's
	 private SIGCHLD handler, allowing catch_child_signal to copy
	 it into lib_child_handler.  */
      g_source_unref (g_child_watch_source_new (getpid ()));
#endif
      catch_child_signal ();
    }

#ifdef HAVE_SETRLIMIT
  /* Don't allocate more than FD_SETSIZE file descriptors for Emacs itself.  */
  if (getrlimit (RLIMIT_NOFILE, &nofile_limit) != 0)
    nofile_limit.rlim_cur = 0;
  else if (FD_SETSIZE < nofile_limit.rlim_cur)
    {
      struct rlimit rlim = nofile_limit;
      rlim.rlim_cur = FD_SETSIZE;
      if (setrlimit (RLIMIT_NOFILE, &rlim) != 0)
	nofile_limit.rlim_cur = 0;
    }
#endif

  external_sock_fd = sockfd;
  max_desc = -1;
  memset (fd_callback_info, 0, sizeof (fd_callback_info));

  num_pending_connects = 0;

  process_output_delay_count = 0;
  process_output_skip = 0;

  /* Don't do this, it caused infinite select loops.  The display
     method should call add_keyboard_wait_descriptor on stdin if it
     needs that.  */
#if 0
  FD_SET (0, &input_wait_mask);
#endif

  Vprocess_alist = Qnil;
  deleted_pid_list = Qnil;
  for (i = 0; i < FD_SETSIZE; i++)
    {
      chan_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
  memset (proc_decode_coding_system, 0, sizeof proc_decode_coding_system);
  memset (proc_encode_coding_system, 0, sizeof proc_encode_coding_system);
#ifdef DATAGRAM_SOCKETS
  memset (datagram_address, 0, sizeof datagram_address);
#endif

#if defined (DARWIN_OS)
  /* PTYs are broken on Darwin < 6, but are sometimes useful for interactive
     processes.  As such, we only change the default value.  */
 if (initialized)
  {
    char const *release = (STRINGP (Voperating_system_release)
			   ? SSDATA (Voperating_system_release)
			   : 0);
    if (!release || !release[0] || (release[0] < '7' && release[1] == '.')) {
      Vprocess_connection_type = Qnil;
    }
  }
#endif
#endif	/* subprocesses */
  kbd_is_on_hold = 0;
}

void
syms_of_process (void)
{
#ifdef subprocesses

  DEFSYM (Qprocessp, "processp");
  DEFSYM (Qrun, "run");
  DEFSYM (Qstop, "stop");
  DEFSYM (Qsignal, "signal");

  /* Qexit is already staticpro'd by syms_of_eval; don't staticpro it
     here again.  */

  DEFSYM (Qopen, "open");
  DEFSYM (Qclosed, "closed");
  DEFSYM (Qconnect, "connect");
  DEFSYM (Qfailed, "failed");
  DEFSYM (Qlisten, "listen");
  DEFSYM (Qlocal, "local");
  DEFSYM (Qipv4, "ipv4");
#ifdef AF_INET6
  DEFSYM (Qipv6, "ipv6");
#endif
  DEFSYM (Qdatagram, "datagram");
  DEFSYM (Qseqpacket, "seqpacket");

  DEFSYM (QCport, ":port");
  DEFSYM (QCspeed, ":speed");
  DEFSYM (QCprocess, ":process");

  DEFSYM (QCbytesize, ":bytesize");
  DEFSYM (QCstopbits, ":stopbits");
  DEFSYM (QCparity, ":parity");
  DEFSYM (Qodd, "odd");
  DEFSYM (Qeven, "even");
  DEFSYM (QCflowcontrol, ":flowcontrol");
  DEFSYM (Qhw, "hw");
  DEFSYM (Qsw, "sw");
  DEFSYM (QCsummary, ":summary");

  DEFSYM (Qreal, "real");
  DEFSYM (Qnetwork, "network");
  DEFSYM (Qserial, "serial");
  DEFSYM (QCbuffer, ":buffer");
  DEFSYM (QChost, ":host");
  DEFSYM (QCservice, ":service");
  DEFSYM (QClocal, ":local");
  DEFSYM (QCremote, ":remote");
  DEFSYM (QCcoding, ":coding");
  DEFSYM (QCserver, ":server");
  DEFSYM (QCnowait, ":nowait");
  DEFSYM (QCsentinel, ":sentinel");
  DEFSYM (QCuse_external_socket, ":use-external-socket");
  DEFSYM (QCtls_parameters, ":tls-parameters");
  DEFSYM (Qnsm_verify_connection, "nsm-verify-connection");
  DEFSYM (QClog, ":log");
  DEFSYM (QCnoquery, ":noquery");
  DEFSYM (QCstop, ":stop");
  DEFSYM (QCplist, ":plist");
  DEFSYM (QCcommand, ":command");
  DEFSYM (QCconnection_type, ":connection-type");
  DEFSYM (QCstderr, ":stderr");
  DEFSYM (Qpty, "pty");
  DEFSYM (Qpipe, "pipe");

  DEFSYM (Qlast_nonmenu_event, "last-nonmenu-event");

  staticpro (&Vprocess_alist);
  staticpro (&deleted_pid_list);

#endif	/* subprocesses */

  DEFSYM (QCname, ":name");
  DEFSYM (QCtype, ":type");

  DEFSYM (Qeuid, "euid");
  DEFSYM (Qegid, "egid");
  DEFSYM (Quser, "user");
  DEFSYM (Qgroup, "group");
  DEFSYM (Qcomm, "comm");
  DEFSYM (Qstate, "state");
  DEFSYM (Qppid, "ppid");
  DEFSYM (Qpgrp, "pgrp");
  DEFSYM (Qsess, "sess");
  DEFSYM (Qttname, "ttname");
  DEFSYM (Qtpgid, "tpgid");
  DEFSYM (Qminflt, "minflt");
  DEFSYM (Qmajflt, "majflt");
  DEFSYM (Qcminflt, "cminflt");
  DEFSYM (Qcmajflt, "cmajflt");
  DEFSYM (Qutime, "utime");
  DEFSYM (Qstime, "stime");
  DEFSYM (Qtime, "time");
  DEFSYM (Qcutime, "cutime");
  DEFSYM (Qcstime, "cstime");
  DEFSYM (Qctime, "ctime");
#ifdef subprocesses
  DEFSYM (Qinternal_default_process_sentinel,
	  "internal-default-process-sentinel");
  DEFSYM (Qinternal_default_process_filter,
	  "internal-default-process-filter");
#endif
  DEFSYM (Qpri, "pri");
  DEFSYM (Qnice, "nice");
  DEFSYM (Qthcount, "thcount");
  DEFSYM (Qstart, "start");
  DEFSYM (Qvsize, "vsize");
  DEFSYM (Qrss, "rss");
  DEFSYM (Qetime, "etime");
  DEFSYM (Qpcpu, "pcpu");
  DEFSYM (Qpmem, "pmem");
  DEFSYM (Qargs, "args");

  DEFVAR_BOOL ("delete-exited-processes", delete_exited_processes,
	       doc: /* Non-nil means delete processes immediately when they exit.
A value of nil means don't delete them until `list-processes' is run.  */);

  delete_exited_processes = 1;

#ifdef subprocesses
  DEFVAR_LISP ("process-connection-type", Vprocess_connection_type,
	       doc: /* Control type of device used to communicate with subprocesses.
Values are nil to use a pipe, or t or `pty' to use a pty.
The value has no effect if the system has no ptys or if all ptys are busy:
then a pipe is used in any case.
The value takes effect when `start-process' is called.  */);
  Vprocess_connection_type = Qt;

  DEFVAR_LISP ("process-adaptive-read-buffering", Vprocess_adaptive_read_buffering,
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

  DEFVAR_LISP ("interrupt-process-functions", Vinterrupt_process_functions,
	       doc: /* List of functions to be called for `interrupt-process'.
The arguments of the functions are the same as for `interrupt-process'.
These functions are called in the order of the list, until one of them
returns non-`nil'.  */);
  Vinterrupt_process_functions = list1 (Qinternal_default_interrupt_process);

  DEFSYM (Qinternal_default_interrupt_process,
	  "internal-default-interrupt-process");
  DEFSYM (Qinterrupt_process_functions, "interrupt-process-functions");

  defsubr (&Sprocessp);
  defsubr (&Sget_process);
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
  defsubr (&Sset_process_thread);
  defsubr (&Sprocess_thread);
  defsubr (&Sset_process_window_size);
  defsubr (&Sset_process_inherit_coding_system_flag);
  defsubr (&Sset_process_query_on_exit_flag);
  defsubr (&Sprocess_query_on_exit_flag);
  defsubr (&Sprocess_contact);
  defsubr (&Sprocess_plist);
  defsubr (&Sset_process_plist);
  defsubr (&Sprocess_list);
  defsubr (&Smake_process);
  defsubr (&Smake_pipe_process);
  defsubr (&Sserial_process_configure);
  defsubr (&Smake_serial_process);
  defsubr (&Sset_network_process_option);
  defsubr (&Smake_network_process);
  defsubr (&Sformat_network_address);
  defsubr (&Snetwork_interface_list);
  defsubr (&Snetwork_interface_info);
#ifdef DATAGRAM_SOCKETS
  defsubr (&Sprocess_datagram_address);
  defsubr (&Sset_process_datagram_address);
#endif
  defsubr (&Saccept_process_output);
  defsubr (&Sprocess_send_region);
  defsubr (&Sprocess_send_string);
  defsubr (&Sinternal_default_interrupt_process);
  defsubr (&Sinterrupt_process);
  defsubr (&Skill_process);
  defsubr (&Squit_process);
  defsubr (&Sstop_process);
  defsubr (&Scontinue_process);
  defsubr (&Sprocess_running_child_p);
  defsubr (&Sprocess_send_eof);
  defsubr (&Ssignal_process);
  defsubr (&Swaiting_for_user_input_p);
  defsubr (&Sprocess_type);
  defsubr (&Sinternal_default_process_sentinel);
  defsubr (&Sinternal_default_process_filter);
  defsubr (&Sset_process_coding_system);
  defsubr (&Sprocess_coding_system);
  defsubr (&Sset_process_filter_multibyte);
  defsubr (&Sprocess_filter_multibyte_p);

 {
   Lisp_Object subfeatures = Qnil;
   const struct socket_options *sopt;

#define ADD_SUBFEATURE(key, val) \
  subfeatures = pure_cons (pure_cons (key, pure_cons (val, Qnil)), subfeatures)

   ADD_SUBFEATURE (QCnowait, Qt);
#ifdef DATAGRAM_SOCKETS
   ADD_SUBFEATURE (QCtype, Qdatagram);
#endif
#ifdef HAVE_SEQPACKET
   ADD_SUBFEATURE (QCtype, Qseqpacket);
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
   ADD_SUBFEATURE (QCserver, Qt);

   for (sopt = socket_options; sopt->name; sopt++)
     subfeatures = pure_cons (intern_c_string (sopt->name), subfeatures);

   Fprovide (intern_c_string ("make-network-process"), subfeatures);
 }

#endif	/* subprocesses */

  defsubr (&Sget_buffer_process);
  defsubr (&Sprocess_inherit_coding_system_flag);
  defsubr (&Slist_system_processes);
  defsubr (&Sprocess_attributes);
}
