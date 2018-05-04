/* Process support for GNU Emacs on the Microsoft Windows API.

Copyright (C) 1992, 1995, 1999-2018 Free Software Foundation, Inc.

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

/*
   Drew Bliss                   Oct 14, 1993
     Adapted from alarm.c by Tim Fleehart
*/

#define DEFER_MS_W32_H
#include <config.h>

#include <mingw_time.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <io.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <sys/file.h>
#include <mbstring.h>
#include <locale.h>

/* Include CRT headers *before* ms-w32.h.  */
#include <ms-w32.h>

#undef signal
#undef wait
#undef spawnve
#undef select
#undef kill

#include <windows.h>

#ifdef HAVE_LANGINFO_CODESET
#include <nl_types.h>
#include <langinfo.h>
#endif

#include "lisp.h"
#include "w32.h"
#include "w32common.h"
#include "w32heap.h"
#include "syswait.h"	/* for WNOHANG */
#include "syssignal.h"
#include "w32term.h"
#include "coding.h"

#define RVA_TO_PTR(var,section,filedata) \
  ((void *)((section)->PointerToRawData					\
	    + ((DWORD_PTR)(var) - (section)->VirtualAddress)		\
	    + (filedata).file_base))

extern BOOL g_b_init_compare_string_w;
extern BOOL g_b_init_debug_break_process;

int sys_select (int, SELECT_TYPE *, SELECT_TYPE *, SELECT_TYPE *,
		const struct timespec *, const sigset_t *);

/* Signal handlers...SIG_DFL == 0 so this is initialized correctly.  */
static signal_handler sig_handlers[NSIG];

static sigset_t sig_mask;

static CRITICAL_SECTION crit_sig;

/* Improve on the CRT 'signal' implementation so that we could record
   the SIGCHLD handler and fake interval timers.  */
signal_handler
sys_signal (int sig, signal_handler handler)
{
  signal_handler old;

  /* SIGCHLD is needed for supporting subprocesses, see sys_kill
     below.  SIGALRM and SIGPROF are used by setitimer.  All the
     others are the only ones supported by the MS runtime.  */
  if (!(sig == SIGINT || sig == SIGSEGV || sig == SIGILL
	|| sig == SIGFPE || sig == SIGABRT || sig == SIGTERM
	|| sig == SIGCHLD || sig == SIGALRM || sig == SIGPROF))
    {
      errno = EINVAL;
      return SIG_ERR;
    }
  old = sig_handlers[sig];
  /* SIGABRT is treated specially because w32.c installs term_ntproc
     as its handler, so we don't want to override that afterwards.
     Aborting Emacs works specially anyway: either by calling
     emacs_abort directly or through terminate_due_to_signal, which
     calls emacs_abort through emacs_raise.  */
  if (!(sig == SIGABRT && old == term_ntproc))
    {
      sig_handlers[sig] = handler;
      if (!(sig == SIGCHLD || sig == SIGALRM || sig == SIGPROF))
	signal (sig, handler);
    }
  return old;
}

/* Emulate sigaction. */
int
sigaction (int sig, const struct sigaction *act, struct sigaction *oact)
{
  signal_handler old = SIG_DFL;
  int retval = 0;

  if (act)
    old = sys_signal (sig, act->sa_handler);
  else if (oact)
    old = sig_handlers[sig];

  if (old == SIG_ERR)
    {
      errno = EINVAL;
      retval = -1;
    }
  if (oact)
    {
      oact->sa_handler = old;
      oact->sa_flags = 0;
      oact->sa_mask = empty_mask;
    }
  return retval;
}

/* Emulate signal sets and blocking of signals used by timers.  */

int
sigemptyset (sigset_t *set)
{
  *set = 0;
  return 0;
}

int
sigaddset (sigset_t *set, int signo)
{
  if (!set)
    {
      errno = EINVAL;
      return -1;
    }
  if (signo < 0 || signo >= NSIG)
    {
      errno = EINVAL;
      return -1;
    }

  *set |= (1U << signo);

  return 0;
}

int
sigfillset (sigset_t *set)
{
  if (!set)
    {
      errno = EINVAL;
      return -1;
    }

  *set = 0xFFFFFFFF;
  return 0;
}

int
sigprocmask (int how, const sigset_t *set, sigset_t *oset)
{
  if (!(how == SIG_BLOCK || how == SIG_UNBLOCK || how == SIG_SETMASK))
    {
      errno = EINVAL;
      return -1;
    }

  if (oset)
    *oset = sig_mask;

  if (!set)
    return 0;

  switch (how)
    {
    case SIG_BLOCK:
      sig_mask |= *set;
      break;
    case SIG_SETMASK:
      sig_mask = *set;
      break;
    case SIG_UNBLOCK:
      /* FIXME: Catch signals that are blocked and reissue them when
	 they are unblocked.  Important for SIGALRM and SIGPROF only.  */
      sig_mask &= ~(*set);
      break;
    }

  return 0;
}

int
pthread_sigmask (int how, const sigset_t *set, sigset_t *oset)
{
  if (sigprocmask (how, set, oset) == -1)
    return EINVAL;
  return 0;
}

int
sigismember (const sigset_t *set, int signo)
{
  if (signo < 0 || signo >= NSIG)
    {
      errno = EINVAL;
      return -1;
    }
  if (signo > sizeof (*set) * CHAR_BIT)
    emacs_abort ();

  return (*set & (1U << signo)) != 0;
}

pid_t
getpgrp (void)
{
  return getpid ();
}

pid_t
tcgetpgrp (int fd)
{
  return getpid ();
}

int
setpgid (pid_t pid, pid_t pgid)
{
  return 0;
}

pid_t
setsid (void)
{
  return getpid ();
}

/* Emulations of interval timers.

   Limitations: only ITIMER_REAL and ITIMER_PROF are supported.

   Implementation: a separate thread is started for each timer type,
   the thread calls the appropriate signal handler when the timer
   expires, after stopping the thread which installed the timer.  */

struct itimer_data {
  volatile ULONGLONG expire;
  volatile ULONGLONG reload;
  volatile int terminate;
  int type;
  HANDLE caller_thread;
  HANDLE timer_thread;
};

static ULONGLONG ticks_now;
static struct itimer_data real_itimer, prof_itimer;
static ULONGLONG clocks_min;
/* If non-zero, itimers are disabled.  Used during shutdown, when we
   delete the critical sections used by the timer threads.  */
static int disable_itimers;

static CRITICAL_SECTION crit_real, crit_prof;

/* GetThreadTimes is not available on Windows 9X and possibly also on 2K.  */
typedef BOOL (WINAPI *GetThreadTimes_Proc) (
  HANDLE hThread,
  LPFILETIME lpCreationTime,
  LPFILETIME lpExitTime,
  LPFILETIME lpKernelTime,
  LPFILETIME lpUserTime);

static GetThreadTimes_Proc s_pfn_Get_Thread_Times;

#define MAX_SINGLE_SLEEP    30
#define TIMER_TICKS_PER_SEC 1000

/* Return a suitable time value, in 1-ms units, for THREAD, a handle
   to a thread.  If THREAD is NULL or an invalid handle, return the
   current wall-clock time since January 1, 1601 (UTC).  Otherwise,
   return the sum of kernel and user times used by THREAD since it was
   created, plus its creation time.  */
static ULONGLONG
w32_get_timer_time (HANDLE thread)
{
  ULONGLONG retval;
  int use_system_time = 1;
  /* The functions below return times in 100-ns units.  */
  const int tscale = 10 * TIMER_TICKS_PER_SEC;

  if (thread && thread != INVALID_HANDLE_VALUE
      && s_pfn_Get_Thread_Times != NULL)
    {
      FILETIME creation_ftime, exit_ftime, kernel_ftime, user_ftime;
      ULARGE_INTEGER temp_creation, temp_kernel, temp_user;

      if (s_pfn_Get_Thread_Times (thread, &creation_ftime, &exit_ftime,
				  &kernel_ftime, &user_ftime))
	{
	  use_system_time = 0;
	  temp_creation.LowPart = creation_ftime.dwLowDateTime;
	  temp_creation.HighPart = creation_ftime.dwHighDateTime;
	  temp_kernel.LowPart = kernel_ftime.dwLowDateTime;
	  temp_kernel.HighPart = kernel_ftime.dwHighDateTime;
	  temp_user.LowPart = user_ftime.dwLowDateTime;
	  temp_user.HighPart = user_ftime.dwHighDateTime;
	  retval =
	    temp_creation.QuadPart / tscale + temp_kernel.QuadPart / tscale
	    + temp_user.QuadPart / tscale;
	}
      else
	DebPrint (("GetThreadTimes failed with error code %lu\n",
		   GetLastError ()));
    }

  if (use_system_time)
    {
      FILETIME current_ftime;
      ULARGE_INTEGER temp;

      GetSystemTimeAsFileTime (&current_ftime);

      temp.LowPart = current_ftime.dwLowDateTime;
      temp.HighPart = current_ftime.dwHighDateTime;

      retval = temp.QuadPart / tscale;
    }

  return retval;
}

/* Thread function for a timer thread.  */
static DWORD WINAPI
timer_loop (LPVOID arg)
{
  struct itimer_data *itimer = (struct itimer_data *)arg;
  int which = itimer->type;
  int sig = (which == ITIMER_REAL) ? SIGALRM : SIGPROF;
  CRITICAL_SECTION *crit = (which == ITIMER_REAL) ? &crit_real : &crit_prof;
  const DWORD max_sleep = MAX_SINGLE_SLEEP * 1000 / TIMER_TICKS_PER_SEC;
  HANDLE hth = (which == ITIMER_REAL) ? NULL : itimer->caller_thread;

  while (1)
    {
      DWORD sleep_time;
      signal_handler handler;
      ULONGLONG now, expire, reload;

      /* Load new values if requested by setitimer.  */
      EnterCriticalSection (crit);
      expire = itimer->expire;
      reload = itimer->reload;
      LeaveCriticalSection (crit);
      if (itimer->terminate)
	return 0;

      if (expire == 0)
	{
	  /* We are idle.  */
	  Sleep (max_sleep);
	  continue;
	}

      if (expire > (now = w32_get_timer_time (hth)))
	sleep_time = expire - now;
      else
	sleep_time = 0;
      /* Don't sleep too long at a time, to be able to see the
	 termination flag without too long a delay.  */
      while (sleep_time > max_sleep)
	{
	  if (itimer->terminate)
	    return 0;
	  Sleep (max_sleep);
	  EnterCriticalSection (crit);
	  expire = itimer->expire;
	  LeaveCriticalSection (crit);
	  sleep_time =
	    (expire > (now = w32_get_timer_time (hth))) ? expire - now : 0;
	}
      if (itimer->terminate)
	return 0;
      if (sleep_time > 0)
	{
	  Sleep (sleep_time * 1000 / TIMER_TICKS_PER_SEC);
	  /* Always sleep past the expiration time, to make sure we
	     never call the handler _before_ the expiration time,
	     always slightly after it.  Sleep(5) makes sure we don't
	     hog the CPU by calling 'w32_get_timer_time' with high
	     frequency, and also let other threads work.  */
	  while (w32_get_timer_time (hth) < expire)
	    Sleep (5);
	}

      EnterCriticalSection (crit);
      expire = itimer->expire;
      LeaveCriticalSection (crit);
      if (expire == 0)
	continue;

      /* Time's up.  */
      handler = sig_handlers[sig];
      if (!(handler == SIG_DFL || handler == SIG_IGN || handler == SIG_ERR)
	  /* FIXME: Don't ignore masked signals.  Instead, record that
	     they happened and reissue them when the signal is
	     unblocked.  */
	  && !sigismember (&sig_mask, sig)
	  /* Simulate masking of SIGALRM and SIGPROF when processing
	     fatal signals.  */
	  && !fatal_error_in_progress
	  && itimer->caller_thread)
	{
	  /* Simulate a signal delivered to the thread which installed
	     the timer, by suspending that thread while the handler
	     runs.  */
	  HANDLE th = itimer->caller_thread;
	  DWORD result = SuspendThread (th);

	  if (result == (DWORD)-1)
	    return 2;

	  handler (sig);
	  ResumeThread (th);
	}

      /* Update expiration time and loop.  */
      EnterCriticalSection (crit);
      expire = itimer->expire;
      if (expire == 0)
	{
	  LeaveCriticalSection (crit);
	  continue;
	}
      reload = itimer->reload;
      if (reload > 0)
	{
	  now = w32_get_timer_time (hth);
	  if (expire <= now)
	    {
	      ULONGLONG lag = now - expire;

	      /* If we missed some opportunities (presumably while
		 sleeping or while the signal handler ran), skip
		 them.  */
	      if (lag > reload)
		expire = now - (lag % reload);

	      expire += reload;
	    }
	}
      else
	expire = 0;	/* become idle */
      itimer->expire = expire;
      LeaveCriticalSection (crit);
    }
  return 0;
}

static void
stop_timer_thread (int which)
{
  struct itimer_data *itimer =
    (which == ITIMER_REAL) ? &real_itimer : &prof_itimer;
  int i;
  DWORD err = 0, exit_code = 255;
  BOOL status;

  /* Signal the thread that it should terminate.  */
  itimer->terminate = 1;

  if (itimer->timer_thread == NULL)
    return;

  /* Wait for the timer thread to terminate voluntarily, then kill it
     if it doesn't.  This loop waits twice more than the maximum
     amount of time a timer thread sleeps, see above.  */
  for (i = 0; i < MAX_SINGLE_SLEEP / 5; i++)
    {
      if (!((status = GetExitCodeThread (itimer->timer_thread, &exit_code))
	    && exit_code == STILL_ACTIVE))
	break;
      Sleep (10);
    }
  if ((status == FALSE && (err = GetLastError ()) == ERROR_INVALID_HANDLE)
      || exit_code == STILL_ACTIVE)
    {
      if (!(status == FALSE && err == ERROR_INVALID_HANDLE))
	TerminateThread (itimer->timer_thread, 0);
    }

  /* Clean up.  */
  CloseHandle (itimer->timer_thread);
  itimer->timer_thread = NULL;
  if (itimer->caller_thread)
    {
      CloseHandle (itimer->caller_thread);
      itimer->caller_thread = NULL;
    }
}

/* This is called at shutdown time from term_ntproc.  */
void
term_timers (void)
{
  if (real_itimer.timer_thread)
    stop_timer_thread (ITIMER_REAL);
  if (prof_itimer.timer_thread)
    stop_timer_thread (ITIMER_PROF);

  /* We are going to delete the critical sections, so timers cannot
     work after this.  */
  disable_itimers = 1;

  DeleteCriticalSection (&crit_real);
  DeleteCriticalSection (&crit_prof);
  DeleteCriticalSection (&crit_sig);
}

/* This is called at initialization time from init_ntproc.  */
void
init_timers (void)
{
  /* GetThreadTimes is not available on all versions of Windows, so
     need to probe for its availability dynamically, and call it
     through a pointer.  */
  s_pfn_Get_Thread_Times = NULL; /* in case dumped Emacs comes with a value */
  if (os_subtype != OS_9X)
    s_pfn_Get_Thread_Times =
      (GetThreadTimes_Proc)GetProcAddress (GetModuleHandle ("kernel32.dll"),
					   "GetThreadTimes");

  /* Make sure we start with zeroed out itimer structures, since
     dumping may have left there traces of threads long dead.  */
  memset (&real_itimer, 0, sizeof real_itimer);
  memset (&prof_itimer, 0, sizeof prof_itimer);

  InitializeCriticalSection (&crit_real);
  InitializeCriticalSection (&crit_prof);
  InitializeCriticalSection (&crit_sig);

  disable_itimers = 0;
}

static int
start_timer_thread (int which)
{
  DWORD exit_code, tid;
  HANDLE th;
  struct itimer_data *itimer =
    (which == ITIMER_REAL) ? &real_itimer : &prof_itimer;

  if (itimer->timer_thread
      && GetExitCodeThread (itimer->timer_thread, &exit_code)
      && exit_code == STILL_ACTIVE)
    return 0;

  /* Clean up after possibly exited thread.  */
  if (itimer->timer_thread)
    {
      CloseHandle (itimer->timer_thread);
      itimer->timer_thread = NULL;
    }
  if (itimer->caller_thread)
    {
      CloseHandle (itimer->caller_thread);
      itimer->caller_thread = NULL;
    }

  /* Start a new thread.  */
  if (!DuplicateHandle (GetCurrentProcess (), GetCurrentThread (),
			GetCurrentProcess (), &th, 0, FALSE,
			DUPLICATE_SAME_ACCESS))
    {
      errno = ESRCH;
      return -1;
    }
  itimer->terminate = 0;
  itimer->type = which;
  itimer->caller_thread = th;
  /* Request that no more than 64KB of stack be reserved for this
     thread, to avoid reserving too much memory, which would get in
     the way of threads we start to wait for subprocesses.  See also
     new_child below.  */
  itimer->timer_thread = CreateThread (NULL, 64 * 1024, timer_loop,
				       (void *)itimer, 0x00010000, &tid);

  if (!itimer->timer_thread)
    {
      CloseHandle (itimer->caller_thread);
      itimer->caller_thread = NULL;
      errno = EAGAIN;
      return -1;
    }

  /* This is needed to make sure that the timer thread running for
     profiling gets CPU as soon as the Sleep call terminates. */
  if (which == ITIMER_PROF)
    SetThreadPriority (itimer->timer_thread, THREAD_PRIORITY_TIME_CRITICAL);

  return 0;
}

/* Most of the code of getitimer and setitimer (but not of their
   subroutines) was shamelessly stolen from itimer.c in the DJGPP
   library, see www.delorie.com/djgpp.  */
int
getitimer (int which, struct itimerval *value)
{
  volatile ULONGLONG *t_expire;
  volatile ULONGLONG *t_reload;
  ULONGLONG expire, reload;
  __int64 usecs;
  CRITICAL_SECTION *crit;
  struct itimer_data *itimer;

  if (disable_itimers)
    return -1;

  if (!value)
    {
      errno = EFAULT;
      return -1;
    }

  if (which != ITIMER_REAL && which != ITIMER_PROF)
    {
      errno = EINVAL;
      return -1;
    }

  itimer = (which == ITIMER_REAL) ? &real_itimer : &prof_itimer;

  ticks_now = w32_get_timer_time ((which == ITIMER_REAL)
				  ? NULL
				  : GetCurrentThread ());

  t_expire = &itimer->expire;
  t_reload = &itimer->reload;
  crit = (which == ITIMER_REAL) ? &crit_real : &crit_prof;

  EnterCriticalSection (crit);
  reload = *t_reload;
  expire = *t_expire;
  LeaveCriticalSection (crit);

  if (expire)
    expire -= ticks_now;

  value->it_value.tv_sec    = expire / TIMER_TICKS_PER_SEC;
  usecs =
    (expire % TIMER_TICKS_PER_SEC) * (__int64)1000000 / TIMER_TICKS_PER_SEC;
  value->it_value.tv_usec   = usecs;
  value->it_interval.tv_sec = reload / TIMER_TICKS_PER_SEC;
  usecs =
    (reload % TIMER_TICKS_PER_SEC) * (__int64)1000000 / TIMER_TICKS_PER_SEC;
  value->it_interval.tv_usec= usecs;

  return 0;
}

int
setitimer(int which, struct itimerval *value, struct itimerval *ovalue)
{
  volatile ULONGLONG *t_expire, *t_reload;
  ULONGLONG expire, reload, expire_old, reload_old;
  __int64 usecs;
  CRITICAL_SECTION *crit;
  struct itimerval tem, *ptem;

  if (disable_itimers)
    return -1;

  /* Posix systems expect timer values smaller than the resolution of
     the system clock be rounded up to the clock resolution.  First
     time we are called, measure the clock tick resolution.  */
  if (!clocks_min)
    {
      ULONGLONG t1, t2;

      for (t1 = w32_get_timer_time (NULL);
	   (t2 = w32_get_timer_time (NULL)) == t1; )
	;
      clocks_min = t2 - t1;
    }

  if (ovalue)
    ptem = ovalue;
  else
    ptem = &tem;

  if (getitimer (which, ptem)) /* also sets ticks_now */
    return -1;		       /* errno already set */

  t_expire =
    (which == ITIMER_REAL) ? &real_itimer.expire : &prof_itimer.expire;
  t_reload =
    (which == ITIMER_REAL) ? &real_itimer.reload : &prof_itimer.reload;

  crit = (which == ITIMER_REAL) ? &crit_real : &crit_prof;

  if (!value
      || (value->it_value.tv_sec == 0 && value->it_value.tv_usec == 0))
    {
      EnterCriticalSection (crit);
      /* Disable the timer.  */
      *t_expire = 0;
      *t_reload = 0;
      LeaveCriticalSection (crit);
      return 0;
    }

  reload = value->it_interval.tv_sec * TIMER_TICKS_PER_SEC;

  usecs = value->it_interval.tv_usec;
  if (value->it_interval.tv_sec == 0
      && usecs && usecs * TIMER_TICKS_PER_SEC < clocks_min * 1000000)
    reload = clocks_min;
  else
    {
      usecs *= TIMER_TICKS_PER_SEC;
      reload += usecs / 1000000;
    }

  expire = value->it_value.tv_sec * TIMER_TICKS_PER_SEC;
  usecs = value->it_value.tv_usec;
  if (value->it_value.tv_sec == 0
      && usecs * TIMER_TICKS_PER_SEC < clocks_min * 1000000)
    expire = clocks_min;
  else
    {
      usecs *= TIMER_TICKS_PER_SEC;
      expire += usecs / 1000000;
    }

  expire += ticks_now;

  EnterCriticalSection (crit);
  expire_old = *t_expire;
  reload_old = *t_reload;
  if (!(expire == expire_old && reload == reload_old))
    {
      *t_reload = reload;
      *t_expire = expire;
    }
  LeaveCriticalSection (crit);

  return start_timer_thread (which);
}

int
alarm (int seconds)
{
#ifdef HAVE_SETITIMER
  struct itimerval new_values, old_values;

  new_values.it_value.tv_sec = seconds;
  new_values.it_value.tv_usec = 0;
  new_values.it_interval.tv_sec = new_values.it_interval.tv_usec = 0;

  if (setitimer (ITIMER_REAL, &new_values, &old_values) < 0)
    return 0;
  return old_values.it_value.tv_sec;
#else
  return seconds;
#endif
}



/* Here's an overview of how support for subprocesses and
   network/serial streams is implemented on MS-Windows.

   The management of both subprocesses and network/serial streams
   circles around the child_procs[] array, which can record up to the
   grand total of MAX_CHILDREN (= 32) of these.  (The reasons for the
   32 limitation will become clear below.)  Each member of
   child_procs[] is a child_process structure, defined on w32.h.

   A related data structure is the fd_info[] array, which holds twice
   as many members, 64, and records the information about file
   descriptors used for communicating with subprocesses and
   network/serial devices.  Each member of the array is the filedesc
   structure, which records the Windows handle for communications,
   such as the read end of the pipe to a subprocess, a socket handle,
   etc.

   Both these arrays reference each other: there's a member of
   child_process structure that records the corresponding file
   descriptor, and there's a member of filedesc structure that holds a
   pointer to the corresponding child_process.

   Whenever Emacs starts a subprocess or opens a network/serial
   stream, the function new_child is called to prepare a new
   child_process structure.  new_child looks for the first vacant slot
   in the child_procs[] array, initializes it, and starts a "reader
   thread" that will watch the output of the subprocess/stream and its
   status.  (If no vacant slot can be found, new_child returns a
   failure indication to its caller, and the higher-level Emacs
   primitive that called it will then fail with EMFILE or EAGAIN.)

   The reader thread started by new_child communicates with the main
   (a.k.a. "Lisp") thread via two event objects and a status, all of
   them recorded by the members of the child_process structure in
   child_procs[].  The event objects serve as semaphores between the
   reader thread and the 'pselect' emulation in sys_select, as follows:

     . Initially, the reader thread is waiting for the char_consumed
       event to become signaled by sys_select, which is an indication
       for the reader thread to go ahead and try reading more stuff
       from the subprocess/stream.

     . The reader thread then attempts to read by calling a
       blocking-read function.  When the read call returns, either
       successfully or with some failure indication, the reader thread
       updates the status of the read accordingly, and signals the 2nd
       event object, char_avail, on whose handle sys_select is
       waiting.  This tells sys_select that the file descriptor
       allocated for the subprocess or the stream is ready to be
       read from.

   When the subprocess exits or the network/serial stream is closed,
   the reader thread sets the status accordingly and exits.  It also
   exits when the main thread sets the status to STATUS_READ_ERROR
   and/or the char_avail and char_consumed event handles become NULL;
   this is how delete_child, called by Emacs when a subprocess or a
   stream is terminated, terminates the reader thread as part of
   deleting the child_process object.

   The sys_select function emulates the Posix 'pselect' functionality;
   it is needed because the Windows 'select' function supports only
   network sockets, while Emacs expects 'pselect' to work for any file
   descriptor, including pipes and serial streams.

   When sys_select is called, it uses the information in fd_info[]
   array to convert the file descriptors which it was asked to watch
   into Windows handles.  In general, the handle to watch is the
   handle of the char_avail event of the child_process structure that
   corresponds to the file descriptor.  In addition, for subprocesses,
   sys_select watches one more handle: the handle for the subprocess,
   so that it could emulate the SIGCHLD signal when the subprocess
   exits.

   If file descriptor zero (stdin) doesn't have its bit set in the
   'rfds' argument to sys_select, the function always watches for
   keyboard interrupts, to be able to interrupt the wait and return
   when the user presses C-g.

   Having collected the handles to watch, sys_select calls
   WaitForMultipleObjects to wait for any one of them to become
   signaled.  Since WaitForMultipleObjects can only watch up to 64
   handles, Emacs on Windows is limited to maximum 32 child_process
   objects (since a subprocess consumes 2 handles to be watched, see
   above).

   When any of the handles become signaled, sys_select does whatever
   is appropriate for the corresponding child_process object:

     . If it's a handle to the char_avail event, sys_select marks the
       corresponding bit in 'rfds', and Emacs will then read from that
       file descriptor.

     . If it's a handle to the process, sys_select calls the SIGCHLD
       handler, to inform Emacs of the fact that the subprocess
       exited.

   The waitpid emulation works very similar to sys_select, except that
   it only watches handles of subprocesses, and doesn't synchronize
   with the reader thread.

   Because socket descriptors on Windows are handles, while Emacs
   expects them to be file descriptors, all low-level I/O functions,
   such as 'read' and 'write', and all socket operations, like
   'connect', 'recvfrom', 'accept', etc., are redirected to the
   corresponding 'sys_*' functions, which must convert a file
   descriptor to a handle using the fd_info[] array, and then invoke
   the corresponding Windows API on the handle.  Most of these
   redirected 'sys_*' functions are implemented on w32.c.

   When the file descriptor was produced by functions such as 'open',
   the corresponding handle is obtained by calling _get_osfhandle.  To
   produce a file descriptor for a socket handle, which has no file
   descriptor as far as Windows is concerned, the function
   socket_to_fd opens the null device; the resulting file descriptor
   will never be used directly in any I/O API, but serves as an index
   into the fd_info[] array, where the socket handle is stored.  The
   SOCK_HANDLE macro retrieves the handle when given the file
   descriptor.

   The function sys_kill emulates the Posix 'kill' functionality to
   terminate other processes.  It does that by attaching to the
   foreground window of the process and sending a Ctrl-C or Ctrl-BREAK
   signal to the process; if that doesn't work, then it calls
   TerminateProcess to forcibly terminate the process.  Note that this
   only terminates the immediate process whose PID was passed to
   sys_kill; it doesn't terminate the child processes of that process.
   This means, for example, that an Emacs subprocess run through a
   shell might not be killed, because sys_kill will only terminate the
   shell.  (In practice, however, such problems are very rare.)  */

/* Defined in <process.h> which conflicts with the local copy */
#define _P_NOWAIT 1

/* Child process management list.  */
int child_proc_count = 0;
child_process child_procs[ MAX_CHILDREN ];

static DWORD WINAPI reader_thread (void *arg);

/* Find an unused process slot.  */
child_process *
new_child (void)
{
  child_process *cp;
  DWORD id;

  for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
    if (!CHILD_ACTIVE (cp) && cp->procinfo.hProcess == NULL)
      goto Initialize;
  if (child_proc_count == MAX_CHILDREN)
    {
      int i = 0;
      child_process *dead_cp = NULL;

      DebPrint (("new_child: No vacant slots, looking for dead processes\n"));
      for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
	if (!CHILD_ACTIVE (cp) && cp->procinfo.hProcess)
	  {
	    DWORD status = 0;

	    if (!GetExitCodeProcess (cp->procinfo.hProcess, &status))
	      {
		DebPrint (("new_child.GetExitCodeProcess: error %lu for PID %lu\n",
			   GetLastError (), cp->procinfo.dwProcessId));
		status = STILL_ACTIVE;
	      }
	    if (status != STILL_ACTIVE
		|| WaitForSingleObject (cp->procinfo.hProcess, 0) == WAIT_OBJECT_0)
	      {
		DebPrint (("new_child: Freeing slot of dead process %d, fd %d\n",
			   cp->procinfo.dwProcessId, cp->fd));
		CloseHandle (cp->procinfo.hProcess);
		cp->procinfo.hProcess = NULL;
		CloseHandle (cp->procinfo.hThread);
		cp->procinfo.hThread = NULL;
		/* Free up to 2 dead slots at a time, so that if we
		   have a lot of them, they will eventually all be
		   freed when the tornado ends.  */
		if (i == 0)
		  dead_cp = cp;
		else
		  break;
		i++;
	      }
	  }
      if (dead_cp)
	{
	  cp = dead_cp;
	  goto Initialize;
	}
    }
  if (child_proc_count == MAX_CHILDREN)
    return NULL;
  cp = &child_procs[child_proc_count++];

 Initialize:
  /* Last opportunity to avoid leaking handles before we forget them
     for good.  */
  if (cp->procinfo.hProcess)
    CloseHandle (cp->procinfo.hProcess);
  if (cp->procinfo.hThread)
    CloseHandle (cp->procinfo.hThread);
  memset (cp, 0, sizeof (*cp));
  cp->fd = -1;
  cp->pid = -1;
  cp->procinfo.hProcess = NULL;
  cp->status = STATUS_READ_ERROR;

  /* use manual reset event so that select() will function properly */
  cp->char_avail = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->char_avail)
    {
      cp->char_consumed = CreateEvent (NULL, FALSE, FALSE, NULL);
      if (cp->char_consumed)
        {
	  /* The 0x00010000 flag is STACK_SIZE_PARAM_IS_A_RESERVATION.
	     It means that the 64K stack we are requesting in the 2nd
	     argument is how much memory should be reserved for the
	     stack.  If we don't use this flag, the memory requested
	     by the 2nd argument is the amount actually _committed_,
	     but Windows reserves 8MB of memory for each thread's
	     stack.  (The 8MB figure comes from the -stack
	     command-line argument we pass to the linker when building
	     Emacs, but that's because we need a large stack for
	     Emacs's main thread.)  Since we request 2GB of reserved
	     memory at startup (see w32heap.c), which is close to the
	     maximum memory available for a 32-bit process on Windows,
	     the 8MB reservation for each thread causes failures in
	     starting subprocesses, because we create a thread running
	     reader_thread for each subprocess.  As 8MB of stack is
	     way too much for reader_thread, forcing Windows to
	     reserve less wins the day.  */
	  cp->thrd = CreateThread (NULL, 64 * 1024, reader_thread, cp,
				   0x00010000, &id);
	  if (cp->thrd)
	    return cp;
	}
    }
  delete_child (cp);
  return NULL;
}

void
delete_child (child_process *cp)
{
  int i;

  /* Should not be deleting a child that is still needed. */
  for (i = 0; i < MAXDESC; i++)
    if (fd_info[i].cp == cp)
      emacs_abort ();

  if (!CHILD_ACTIVE (cp) && cp->procinfo.hProcess == NULL)
    return;

  /* reap thread if necessary */
  if (cp->thrd)
    {
      DWORD rc;

      if (GetExitCodeThread (cp->thrd, &rc) && rc == STILL_ACTIVE)
        {
	  /* let the thread exit cleanly if possible */
	  cp->status = STATUS_READ_ERROR;
	  SetEvent (cp->char_consumed);
#if 0
          /* We used to forcibly terminate the thread here, but it
             is normally unnecessary, and in abnormal cases, the worst that
             will happen is we have an extra idle thread hanging around
             waiting for the zombie process.  */
	  if (WaitForSingleObject (cp->thrd, 1000) != WAIT_OBJECT_0)
	    {
	      DebPrint (("delete_child.WaitForSingleObject (thread) failed "
			 "with %lu for fd %ld\n", GetLastError (), cp->fd));
	      TerminateThread (cp->thrd, 0);
	    }
#endif
	}
      CloseHandle (cp->thrd);
      cp->thrd = NULL;
    }
  if (cp->char_avail)
    {
      CloseHandle (cp->char_avail);
      cp->char_avail = NULL;
    }
  if (cp->char_consumed)
    {
      CloseHandle (cp->char_consumed);
      cp->char_consumed = NULL;
    }

  /* update child_proc_count (highest numbered slot in use plus one) */
  if (cp == child_procs + child_proc_count - 1)
    {
      for (i = child_proc_count-1; i >= 0; i--)
	if (CHILD_ACTIVE (&child_procs[i])
	    || child_procs[i].procinfo.hProcess != NULL)
	  {
	    child_proc_count = i + 1;
	    break;
	  }
    }
  if (i < 0)
    child_proc_count = 0;
}

/* Find a child by pid.  */
static child_process *
find_child_pid (DWORD pid)
{
  child_process *cp;

  for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
    if ((CHILD_ACTIVE (cp) || cp->procinfo.hProcess != NULL)
	&& pid == cp->pid)
      return cp;
  return NULL;
}

void
release_listen_threads (void)
{
  int i;

  for (i = child_proc_count - 1; i >= 0; i--)
    {
      if (CHILD_ACTIVE (&child_procs[i])
	  && (fd_info[child_procs[i].fd].flags & FILE_LISTEN))
	child_procs[i].status = STATUS_READ_ERROR;
    }
}

/* Thread proc for child process and socket reader threads. Each thread
   is normally blocked until woken by select() to check for input by
   reading one char.  When the read completes, char_avail is signaled
   to wake up the select emulator and the thread blocks itself again. */
static DWORD WINAPI
reader_thread (void *arg)
{
  child_process *cp;

  /* Our identity */
  cp = (child_process *)arg;

  /* We have to wait for the go-ahead before we can start */
  if (cp == NULL
      || WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0
      || cp->fd < 0)
    return 1;

  for (;;)
    {
      int rc;

      if (cp->fd >= 0 && (fd_info[cp->fd].flags & FILE_CONNECT) != 0)
	rc = _sys_wait_connect (cp->fd);
      else if (cp->fd >= 0 && (fd_info[cp->fd].flags & FILE_LISTEN) != 0)
	rc = _sys_wait_accept (cp->fd);
      else
	rc = _sys_read_ahead (cp->fd);

      /* Don't bother waiting for the event if we already have been
	 told to exit by delete_child.  */
      if (cp->status == STATUS_READ_ERROR || !cp->char_avail)
	break;

      /* The name char_avail is a misnomer - it really just means the
	 read-ahead has completed, whether successfully or not. */
      if (!SetEvent (cp->char_avail))
        {
	  DebPrint (("reader_thread.SetEvent(0x%x) failed with %lu for fd %ld (PID %d)\n",
		     (DWORD_PTR)cp->char_avail, GetLastError (),
		     cp->fd, cp->pid));
	  return 1;
	}

      if (rc == STATUS_READ_ERROR || rc == STATUS_CONNECT_FAILED)
	return 2;

      /* If the read died, the child has died so let the thread die */
      if (rc == STATUS_READ_FAILED)
	break;

      /* Don't bother waiting for the acknowledge if we already have
	 been told to exit by delete_child.  */
      if (cp->status == STATUS_READ_ERROR || !cp->char_consumed)
	break;

      /* Wait until our input is acknowledged before reading again */
      if (WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
        {
	  DebPrint (("reader_thread.WaitForSingleObject failed with "
		     "%lu for fd %ld\n", GetLastError (), cp->fd));
	  break;
        }
      /* delete_child sets status to STATUS_READ_ERROR when it wants
	 us to exit.  */
      if (cp->status == STATUS_READ_ERROR)
	break;
    }
  return 0;
}

/* To avoid Emacs changing directory, we just record here the
   directory the new process should start in.  This is set just before
   calling sys_spawnve, and is not generally valid at any other time.
   Note that this directory's name is UTF-8 encoded.  */
static char * process_dir;

static BOOL
create_child (char *exe, char *cmdline, char *env, int is_gui_app,
	      pid_t * pPid, child_process *cp)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
#if 0
  SECURITY_DESCRIPTOR sec_desc;
#endif
  DWORD flags;
  char dir[ MAX_PATH ];
  char *p;
  const char *ext;

  if (cp == NULL) emacs_abort ();

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);

#ifdef HAVE_NTGUI
  if (NILP (Vw32_start_process_show_window) && !is_gui_app)
    start.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  else
    start.dwFlags = STARTF_USESTDHANDLES;
  start.wShowWindow = SW_HIDE;

  start.hStdInput = GetStdHandle (STD_INPUT_HANDLE);
  start.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  start.hStdError = GetStdHandle (STD_ERROR_HANDLE);
#endif /* HAVE_NTGUI */

#if 0
  /* Explicitly specify no security */
  if (!InitializeSecurityDescriptor (&sec_desc, SECURITY_DESCRIPTOR_REVISION))
    goto EH_Fail;
  if (!SetSecurityDescriptorDacl (&sec_desc, TRUE, NULL, FALSE))
    goto EH_Fail;
#endif
  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL /* &sec_desc */;
  sec_attrs.bInheritHandle = FALSE;

  filename_to_ansi (process_dir, dir);
  /* Can't use unixtodos_filename here, since that needs its file name
     argument encoded in UTF-8.  OTOH, process_dir, which _is_ in
     UTF-8, points, to the directory computed by our caller, and we
     don't want to modify that, either.  */
  for (p = dir; *p; p = CharNextA (p))
    if (*p == '/')
      *p = '\\';

  /* CreateProcess handles batch files as exe specially.  This special
     handling fails when both the batch file and arguments are quoted.
     We pass NULL as exe to avoid the special handling. */
  if (exe && cmdline[0] == '"' &&
      (ext = strrchr (exe, '.')) &&
      (xstrcasecmp (ext, ".bat") == 0
       || xstrcasecmp (ext, ".cmd") == 0))
      exe = NULL;

  flags = (!NILP (Vw32_start_process_share_console)
	   ? CREATE_NEW_PROCESS_GROUP
	   : CREATE_NEW_CONSOLE);
  if (NILP (Vw32_start_process_inherit_error_mode))
    flags |= CREATE_DEFAULT_ERROR_MODE;
  if (!CreateProcessA (exe, cmdline, &sec_attrs, NULL, TRUE,
		       flags, env, dir, &start, &cp->procinfo))
    goto EH_Fail;

  cp->pid = (int) cp->procinfo.dwProcessId;

  /* Hack for Windows 95, which assigns large (ie negative) pids */
  if (cp->pid < 0)
    cp->pid = -cp->pid;

  *pPid = cp->pid;

  return TRUE;

 EH_Fail:
  DebPrint (("create_child.CreateProcess failed: %ld\n", GetLastError ()););
  return FALSE;
}

/* create_child doesn't know what emacs's file handle will be for waiting
   on output from the child, so we need to make this additional call
   to register the handle with the process
   This way the select emulator knows how to match file handles with
   entries in child_procs.  */
void
register_child (pid_t pid, int fd)
{
  child_process *cp;

  cp = find_child_pid ((DWORD)pid);
  if (cp == NULL)
    {
      DebPrint (("register_child unable to find pid %lu\n", pid));
      return;
    }

#ifdef FULL_DEBUG
  DebPrint (("register_child registered fd %d with pid %lu\n", fd, pid));
#endif

  cp->fd = fd;

  /* thread is initially blocked until select is called; set status so
     that select will release thread */
  cp->status = STATUS_READ_ACKNOWLEDGED;

  /* attach child_process to fd_info */
  if (fd_info[fd].cp != NULL)
    {
      DebPrint (("register_child: fd_info[%d] apparently in use!\n", fd));
      emacs_abort ();
    }

  fd_info[fd].cp = cp;
}

/* Called from waitpid when a process exits.  */
static void
reap_subprocess (child_process *cp)
{
  if (cp->procinfo.hProcess)
    {
      /* Reap the process */
#ifdef FULL_DEBUG
      /* Process should have already died before we are called.  */
      if (WaitForSingleObject (cp->procinfo.hProcess, 0) != WAIT_OBJECT_0)
	DebPrint (("reap_subprocess: child for fd %d has not died yet!", cp->fd));
#endif
      CloseHandle (cp->procinfo.hProcess);
      cp->procinfo.hProcess = NULL;
      CloseHandle (cp->procinfo.hThread);
      cp->procinfo.hThread = NULL;
    }

  /* If cp->fd was not closed yet, we might be still reading the
     process output, so don't free its resources just yet.  The call
     to delete_child on behalf of this subprocess will be made by
     sys_read when the subprocess output is fully read.  */
  if (cp->fd < 0)
    delete_child (cp);
}

/* Wait for a child process specified by PID, or for any of our
   existing child processes (if PID is nonpositive) to die.  When it
   does, close its handle.  Return the pid of the process that died
   and fill in STATUS if non-NULL.  */

pid_t
waitpid (pid_t pid, int *status, int options)
{
  DWORD active, retval;
  int nh;
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAX_CHILDREN];
  DWORD timeout_ms;
  int dont_wait = (options & WNOHANG) != 0;

  nh = 0;
  /* According to Posix:

     PID = -1 means status is requested for any child process.

     PID > 0 means status is requested for a single child process
     whose pid is PID.

     PID = 0 means status is requested for any child process whose
     process group ID is equal to that of the calling process.  But
     since Windows has only a limited support for process groups (only
     for console processes and only for the purposes of passing
     Ctrl-BREAK signal to them), and since we have no documented way
     of determining whether a given process belongs to our group, we
     treat 0 as -1.

     PID < -1 means status is requested for any child process whose
     process group ID is equal to the absolute value of PID.  Again,
     since we don't support process groups, we treat that as -1.  */
  if (pid > 0)
    {
      int our_child = 0;

      /* We are requested to wait for a specific child.  */
      for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
	{
	  /* Some child_procs might be sockets; ignore them.  Also
	     ignore subprocesses whose output is not yet completely
	     read.  */
	  if (CHILD_ACTIVE (cp)
	      && cp->procinfo.hProcess
	      && cp->pid == pid)
	    {
	      our_child = 1;
	      break;
	    }
	}
      if (our_child)
	{
	  if (cp->fd < 0 || (fd_info[cp->fd].flags & FILE_AT_EOF) != 0)
	    {
	      wait_hnd[nh] = cp->procinfo.hProcess;
	      cps[nh] = cp;
	      nh++;
	    }
	  else if (dont_wait)
	    {
	      /* PID specifies our subprocess, but its status is not
		 yet available.  */
	      return 0;
	    }
	}
      if (nh == 0)
	{
	  /* No such child process, or nothing to wait for, so fail.  */
	  errno = ECHILD;
	  return -1;
	}
    }
  else
    {
      for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
	{
	  if (CHILD_ACTIVE (cp)
	      && cp->procinfo.hProcess
	      && (cp->fd < 0 || (fd_info[cp->fd].flags & FILE_AT_EOF) != 0))
	    {
	      wait_hnd[nh] = cp->procinfo.hProcess;
	      cps[nh] = cp;
	      nh++;
	    }
	}
      if (nh == 0)
	{
	  /* Nothing to wait on, so fail.  */
	  errno = ECHILD;
	  return -1;
	}
    }

  if (dont_wait)
    timeout_ms = 0;
  else
    timeout_ms = 1000;	/* check for quit about once a second. */

  do
    {
      /* When child_status_changed calls us with WNOHANG in OPTIONS,
	 we are supposed to be non-interruptible, so don't allow
	 quitting in that case.  */
      if (!dont_wait)
	maybe_quit ();
      active = WaitForMultipleObjects (nh, wait_hnd, FALSE, timeout_ms);
    } while (active == WAIT_TIMEOUT && !dont_wait);

  if (active == WAIT_FAILED)
    {
      errno = EBADF;
      return -1;
    }
  else if (active == WAIT_TIMEOUT && dont_wait)
    {
      /* PID specifies our subprocess, but it didn't exit yet, so its
	 status is not yet available.  */
#ifdef FULL_DEBUG
      DebPrint (("Wait: PID %d not reap yet\n", cp->pid));
#endif
      return 0;
    }
  else if (active >= WAIT_OBJECT_0
	   && active < WAIT_OBJECT_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_OBJECT_0;
    }
  else if (active >= WAIT_ABANDONED_0
	   && active < WAIT_ABANDONED_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_ABANDONED_0;
    }
  else
    emacs_abort ();

  if (!GetExitCodeProcess (wait_hnd[active], &retval))
    {
      DebPrint (("Wait.GetExitCodeProcess failed with %lu\n",
		 GetLastError ()));
      retval = 1;
    }
  if (retval == STILL_ACTIVE)
    {
      /* Should never happen.  But it does, with invoking git-gui.exe
	 asynchronously.  So we punt, and just report this process as
	 exited with exit code 259, when we are called with WNOHANG
	 from child_status_changed, because in that case we already
	 _know_ the process has died.  */
      DebPrint (("Wait.WaitForMultipleObjects returned an active process\n"));
      if (!(pid > 0 && dont_wait))
	{
	  errno = EINVAL;
	  return -1;
	}
    }

  /* Massage the exit code from the process to match the format expected
     by the WIFSTOPPED et al macros in syswait.h.  Only WIFSIGNALED and
     WIFEXITED are supported; WIFSTOPPED doesn't make sense under NT.  */

  if (retval == STATUS_CONTROL_C_EXIT)
    retval = SIGINT;
  else
    retval <<= 8;

  if (pid > 0 && active != 0)
    emacs_abort ();
  cp = cps[active];
  pid = cp->pid;
#ifdef FULL_DEBUG
  DebPrint (("Wait signaled with process pid %d\n", cp->pid));
#endif

  if (status)
    *status = retval;
  reap_subprocess (cp);

  return pid;
}

/* Old versions of w32api headers don't have separate 32-bit and
   64-bit defines, but the one they have matches the 32-bit variety.  */
#ifndef IMAGE_NT_OPTIONAL_HDR32_MAGIC
# define IMAGE_NT_OPTIONAL_HDR32_MAGIC IMAGE_NT_OPTIONAL_HDR_MAGIC
# define IMAGE_OPTIONAL_HEADER32 IMAGE_OPTIONAL_HEADER
#endif

/* Implementation note: This function works with file names encoded in
   the current ANSI codepage.  */
static int
w32_executable_type (char * filename,
		     int * is_dos_app,
		     int * is_cygnus_app,
		     int * is_msys_app,
		     int * is_gui_app)
{
  file_data executable;
  char * p;
  int retval = 0;

  /* Default values in case we can't tell for sure.  */
  *is_dos_app = FALSE;
  *is_cygnus_app = FALSE;
  *is_msys_app = FALSE;
  *is_gui_app = FALSE;

  if (!open_input_file (&executable, filename))
    return -1;

  p = strrchr (filename, '.');

  /* We can only identify DOS .com programs from the extension. */
  if (p && xstrcasecmp (p, ".com") == 0)
    *is_dos_app = TRUE;
  else if (p && (xstrcasecmp (p, ".bat") == 0
		 || xstrcasecmp (p, ".cmd") == 0))
    {
      /* A DOS shell script - it appears that CreateProcess is happy to
	 accept this (somewhat surprisingly); presumably it looks at
	 COMSPEC to determine what executable to actually invoke.
	 Therefore, we have to do the same here as well. */
      /* Actually, I think it uses the program association for that
	 extension, which is defined in the registry.  */
      p = egetenv ("COMSPEC");
      if (p)
	retval = w32_executable_type (p, is_dos_app, is_cygnus_app, is_msys_app,
				      is_gui_app);
    }
  else
    {
      /* Look for DOS .exe signature - if found, we must also check that
	 it isn't really a 16- or 32-bit Windows exe, since both formats
	 start with a DOS program stub.  Note that 16-bit Windows
	 executables use the OS/2 1.x format. */

      IMAGE_DOS_HEADER * dos_header;
      IMAGE_NT_HEADERS * nt_header;

      dos_header = (PIMAGE_DOS_HEADER) executable.file_base;
      if (dos_header->e_magic != IMAGE_DOS_SIGNATURE)
	goto unwind;

      nt_header = (PIMAGE_NT_HEADERS) ((unsigned char *) dos_header + dos_header->e_lfanew);

      if ((char *) nt_header > (char *) dos_header + executable.size)
	{
	  /* Some dos headers (pkunzip) have bogus e_lfanew fields.  */
	  *is_dos_app = TRUE;
	}
      else if (nt_header->Signature != IMAGE_NT_SIGNATURE
	       && LOWORD (nt_header->Signature) != IMAGE_OS2_SIGNATURE)
  	{
	  *is_dos_app = TRUE;
  	}
      else if (nt_header->Signature == IMAGE_NT_SIGNATURE)
  	{
          IMAGE_DATA_DIRECTORY *data_dir = NULL;
          if (nt_header->OptionalHeader.Magic == IMAGE_NT_OPTIONAL_HDR32_MAGIC)
            {
              /* Ensure we are using the 32 bit structure.  */
              IMAGE_OPTIONAL_HEADER32 *opt
                = (IMAGE_OPTIONAL_HEADER32*) &(nt_header->OptionalHeader);
              data_dir = opt->DataDirectory;
              *is_gui_app = (opt->Subsystem == IMAGE_SUBSYSTEM_WINDOWS_GUI);
            }
          /* MingW 3.12 has the required 64 bit structs, but in case older
             versions don't, only check 64 bit exes if we know how.  */
#ifdef IMAGE_NT_OPTIONAL_HDR64_MAGIC
          else if (nt_header->OptionalHeader.Magic
                   == IMAGE_NT_OPTIONAL_HDR64_MAGIC)
            {
              IMAGE_OPTIONAL_HEADER64 *opt
                = (IMAGE_OPTIONAL_HEADER64*) &(nt_header->OptionalHeader);
              data_dir = opt->DataDirectory;
              *is_gui_app = (opt->Subsystem == IMAGE_SUBSYSTEM_WINDOWS_GUI);
            }
#endif
          if (data_dir)
            {
              /* Look for Cygwin DLL in the DLL import list. */
              IMAGE_DATA_DIRECTORY import_dir =
                data_dir[IMAGE_DIRECTORY_ENTRY_IMPORT];

	      /* Import directory can be missing in .NET DLLs.  */
	      if (import_dir.VirtualAddress != 0)
		{
		  IMAGE_IMPORT_DESCRIPTOR * imports =
		    RVA_TO_PTR (import_dir.VirtualAddress,
				rva_to_section (import_dir.VirtualAddress,
						nt_header),
				executable);

		  for ( ; imports->Name; imports++)
		    {
		      IMAGE_SECTION_HEADER * section =
			rva_to_section (imports->Name, nt_header);
		      char * dllname = RVA_TO_PTR (imports->Name, section,
						   executable);

		      /* The exact name of the Cygwin DLL has changed with
			 various releases, but hopefully this will be
			 reasonably future-proof.  */
		      if (strncmp (dllname, "cygwin", 6) == 0)
			{
			  *is_cygnus_app = TRUE;
			  break;
			}
		      else if (strncmp (dllname, "msys-", 5) == 0)
			{
			  /* This catches both MSYS 1.x and MSYS2
			     executables (the DLL name is msys-1.0.dll and
			     msys-2.0.dll, respectively).  There doesn't
			     seem to be a reason to distinguish between
			     the two, for now.  */
			  *is_msys_app = TRUE;
			  break;
			}
		    }
		}
            }
  	}
    }

unwind:
  close_file_data (&executable);
  return retval;
}

static int
compare_env (const void *strp1, const void *strp2)
{
  const char *str1 = *(const char **)strp1, *str2 = *(const char **)strp2;

  while (*str1 && *str2 && *str1 != '=' && *str2 != '=')
    {
      /* Sort order in command.com/cmd.exe is based on uppercasing
         names, so do the same here.  */
      if (toupper (*str1) > toupper (*str2))
	return 1;
      else if (toupper (*str1) < toupper (*str2))
	return -1;
      str1++, str2++;
    }

  if (*str1 == '=' && *str2 == '=')
    return 0;
  else if (*str1 == '=')
    return -1;
  else
    return 1;
}

static void
merge_and_sort_env (char **envp1, char **envp2, char **new_envp)
{
  char **optr, **nptr;
  int num;

  nptr = new_envp;
  optr = envp1;
  while (*optr)
    *nptr++ = *optr++;
  num = optr - envp1;

  optr = envp2;
  while (*optr)
    *nptr++ = *optr++;
  num += optr - envp2;

  qsort (new_envp, num, sizeof (char *), compare_env);

  *nptr = NULL;
}

/* When a new child process is created we need to register it in our list,
   so intercept spawn requests.  */
int
sys_spawnve (int mode, char *cmdname, char **argv, char **envp)
{
  Lisp_Object program, full;
  char *cmdline, *env, *parg, **targ;
  int arglen, numenv;
  pid_t pid;
  child_process *cp;
  int is_dos_app, is_cygnus_app, is_msys_app, is_gui_app;
  int do_quoting = 0;
  /* We pass our process ID to our children by setting up an environment
     variable in their environment.  */
  char ppid_env_var_buffer[64];
  char *extra_env[] = {ppid_env_var_buffer, NULL};
  /* These are the characters that cause an argument to need quoting.
     Arguments with whitespace characters need quoting to prevent the
     argument being split into two or more. Arguments with wildcards
     are also quoted, for consistency with posix platforms, where wildcards
     are not expanded if we run the program directly without a shell.
     Some extra whitespace characters need quoting in Cygwin/MSYS programs,
     so this list is conditionally modified below.  */
  const char *sepchars = " \t*?";
  /* This is for native w32 apps; modified below for Cygwin/MSUS apps.  */
  char escape_char = '\\';
  char cmdname_a[MAX_PATH];

  /* We don't care about the other modes */
  if (mode != _P_NOWAIT)
    {
      errno = EINVAL;
      return -1;
    }

  /* Handle executable names without an executable suffix.  The caller
     already searched exec-path and verified the file is executable,
     but start-process doesn't do that for file names that are already
     absolute.  So we double-check this here, just in case.  */
  if (faccessat (AT_FDCWD, cmdname, X_OK, AT_EACCESS) != 0)
    {
      program = build_string (cmdname);
      full = Qnil;
      openp (Vexec_path, program, Vexec_suffixes, &full, make_number (X_OK), 0);
      if (NILP (full))
	{
	  errno = EINVAL;
	  return -1;
	}
      program = ENCODE_FILE (full);
      cmdname = SSDATA (program);
    }
  else
    {
      char *p = alloca (strlen (cmdname) + 1);

      /* Don't change the command name we were passed by our caller
	 (unixtodos_filename below will destructively mirror forward
	 slashes).  */
      cmdname = strcpy (p, cmdname);
    }

  /* make sure argv[0] and cmdname are both in DOS format */
  unixtodos_filename (cmdname);
  /* argv[0] was encoded by caller using ENCODE_FILE, so it is in
     UTF-8.  All the other arguments are encoded by ENCODE_SYSTEM or
     some such, and are in some ANSI codepage.  We need to have
     argv[0] encoded in ANSI codepage.  */
  filename_to_ansi (cmdname, cmdname_a);
  /* We explicitly require that the command's file name be encodable
     in the current ANSI codepage, because we will be invoking it via
     the ANSI APIs.  */
  if (_mbspbrk ((unsigned char *)cmdname_a, (const unsigned char *)"?"))
    {
      errno = ENOENT;
      return -1;
    }
  /* From here on, CMDNAME is an ANSI-encoded string.  */
  cmdname = cmdname_a;
  argv[0] = cmdname;

  /* Determine whether program is a 16-bit DOS executable, or a 32-bit
     Windows executable that is implicitly linked to the Cygnus or
     MSYS dll (implying it was compiled with the Cygnus/MSYS GNU
     toolchain and hence relies on cygwin.dll or MSYS DLL to parse the
     command line - we use this to decide how to escape quote chars in
     command line args that must be quoted).

     Also determine whether it is a GUI app, so that we don't hide its
     initial window unless specifically requested.  */
  w32_executable_type (cmdname, &is_dos_app, &is_cygnus_app, &is_msys_app,
		       &is_gui_app);

  /* On Windows 95, if cmdname is a DOS app, we invoke a helper
     application to start it by specifying the helper app as cmdname,
     while leaving the real app name as argv[0].  */
  if (is_dos_app)
    {
      char *p;

      cmdname = alloca (MAX_PATH);
      if (egetenv ("CMDPROXY"))
	{
	  /* Implementation note: since process-environment, where
	     'egetenv' looks, is encoded in the system codepage, we
	     don't need to encode the cmdproxy file name if we get it
	     from the environment.  */
	  strcpy (cmdname, egetenv ("CMDPROXY"));
	}
      else
	{
	  char *q = lispstpcpy (cmdname,
				/* exec-directory needs to be encoded.  */
				ansi_encode_filename (Vexec_directory));
	  /* If we are run from the source tree, use cmdproxy.exe from
	     the same source tree.  */
	  for (p = q - 2; p > cmdname; p = CharPrevA (cmdname, p))
	    if (*p == '/')
	      break;
	  if (*p == '/' && xstrcasecmp (p, "/lib-src/") == 0)
	    q = stpcpy (p, "/nt/");
	  strcpy (q, "cmdproxy.exe");
	}

      /* Can't use unixtodos_filename here, since that needs its file
	 name argument encoded in UTF-8.  */
      for (p = cmdname; *p; p = CharNextA (p))
	if (*p == '/')
	  *p = '\\';
    }

  /* we have to do some conjuring here to put argv and envp into the
     form CreateProcess wants...  argv needs to be a space separated/null
     terminated list of parameters, and envp is a null
     separated/double-null terminated list of parameters.

     Additionally, zero-length args and args containing whitespace or
     quote chars need to be wrapped in double quotes - for this to work,
     embedded quotes need to be escaped as well.  The aim is to ensure
     the child process reconstructs the argv array we start with
     exactly, so we treat quotes at the beginning and end of arguments
     as embedded quotes.

     The w32 GNU-based library from Cygnus doubles quotes to escape
     them, while MSVC uses backslash for escaping.  (Actually the MSVC
     startup code does attempt to recognize doubled quotes and accept
     them, but gets it wrong and ends up requiring three quotes to get a
     single embedded quote!)  So by default we decide whether to use
     quote or backslash as the escape character based on whether the
     binary is apparently a Cygnus compiled app.

     Note that using backslash to escape embedded quotes requires
     additional special handling if an embedded quote is already
     preceded by backslash, or if an arg requiring quoting ends with
     backslash.  In such cases, the run of escape characters needs to be
     doubled.  For consistency, we apply this special handling as long
     as the escape character is not quote.

     Since we have no idea how large argv and envp are likely to be we
     figure out list lengths on the fly and allocate them.  */

  if (!NILP (Vw32_quote_process_args))
    {
      do_quoting = 1;
      /* Override escape char by binding w32-quote-process-args to
	 desired character, or use t for auto-selection.  */
      if (INTEGERP (Vw32_quote_process_args))
	escape_char = XINT (Vw32_quote_process_args);
      else
	escape_char = (is_cygnus_app || is_msys_app) ? '"' : '\\';
    }

  /* Cygwin/MSYS apps need quoting a bit more often.  */
  if (escape_char == '"')
    sepchars = "\r\n\t\f '";

  /* do argv...  */
  arglen = 0;
  targ = argv;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;
      int escape_char_run = 0;

      if (*p == 0)
	need_quotes = 1;
      for ( ; *p; p++)
	{
	  if (escape_char == '"' && *p == '\\')
	    /* If it's a Cygwin/MSYS app, \ needs to be escaped.  */
	    arglen++;
	  else if (*p == '"')
	    {
	      /* allow for embedded quotes to be escaped */
	      arglen++;
	      need_quotes = 1;
	      /* handle the case where the embedded quote is already escaped */
	      if (escape_char_run > 0)
		{
		  /* To preserve the arg exactly, we need to double the
		     preceding escape characters (plus adding one to
		     escape the quote character itself).  */
		  arglen += escape_char_run;
		}
	    }
	  else if (strchr (sepchars, *p) != NULL)
	    {
	      need_quotes = 1;
	    }

	  if (*p == escape_char && escape_char != '"')
	    escape_char_run++;
	  else
	    escape_char_run = 0;
	}
      if (need_quotes)
	{
	  arglen += 2;
	  /* handle the case where the arg ends with an escape char - we
	     must not let the enclosing quote be escaped.  */
	  if (escape_char_run > 0)
	    arglen += escape_char_run;
	}
      arglen += strlen (*targ++) + 1;
    }
  cmdline = alloca (arglen);
  targ = argv;
  parg = cmdline;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;

      if (*p == 0)
	need_quotes = 1;

      if (do_quoting)
	{
	  for ( ; *p; p++)
	    if ((strchr (sepchars, *p) != NULL) || *p == '"')
	      need_quotes = 1;
	}
      if (need_quotes)
	{
	  int escape_char_run = 0;
	  /* char * first; */
	  /* char * last; */

	  p = *targ;
	  /* first = p; */
	  /* last = p + strlen (p) - 1; */
	  *parg++ = '"';
#if 0
	  /* This version does not escape quotes if they occur at the
	     beginning or end of the arg - this could lead to incorrect
	     behavior when the arg itself represents a command line
	     containing quoted args.  I believe this was originally done
	     as a hack to make some things work, before
	     `w32-quote-process-args' was added.  */
	  while (*p)
	    {
	      if (*p == '"' && p > first && p < last)
		*parg++ = escape_char;	/* escape embedded quotes */
	      *parg++ = *p++;
	    }
#else
	  for ( ; *p; p++)
	    {
	      if (*p == '"')
		{
		  /* double preceding escape chars if any */
		  while (escape_char_run > 0)
		    {
		      *parg++ = escape_char;
		      escape_char_run--;
		    }
		  /* escape all quote chars, even at beginning or end */
		  *parg++ = escape_char;
		}
	      else if (escape_char == '"' && *p == '\\')
		*parg++ = '\\';
	      *parg++ = *p;

	      if (*p == escape_char && escape_char != '"')
		escape_char_run++;
	      else
		escape_char_run = 0;
	    }
	  /* double escape chars before enclosing quote */
	  while (escape_char_run > 0)
	    {
	      *parg++ = escape_char;
	      escape_char_run--;
	    }
#endif
	  *parg++ = '"';
	}
      else
	{
	  strcpy (parg, *targ);
	  parg += strlen (*targ);
	}
      *parg++ = ' ';
      targ++;
    }
  *--parg = '\0';

  /* and envp...  */
  arglen = 1;
  targ = envp;
  numenv = 1; /* for end null */
  while (*targ)
    {
      arglen += strlen (*targ++) + 1;
      numenv++;
    }
  /* extra env vars... */
  sprintf (ppid_env_var_buffer, "EM_PARENT_PROCESS_ID=%lu",
	   GetCurrentProcessId ());
  arglen += strlen (ppid_env_var_buffer) + 1;
  numenv++;

  /* merge env passed in and extra env into one, and sort it.  */
  targ = (char **) alloca (numenv * sizeof (char *));
  merge_and_sort_env (envp, extra_env, targ);

  /* concatenate env entries.  */
  env = alloca (arglen);
  parg = env;
  while (*targ)
    {
      strcpy (parg, *targ);
      parg += strlen (*targ++);
      *parg++ = '\0';
    }
  *parg++ = '\0';
  *parg = '\0';

  cp = new_child ();
  if (cp == NULL)
    {
      errno = EAGAIN;
      return -1;
    }

  /* Now create the process.  */
  if (!create_child (cmdname, cmdline, env, is_gui_app, &pid, cp))
    {
      delete_child (cp);
      errno = ENOEXEC;
      return -1;
    }

  return pid;
}

/* Emulate the select call.
   Wait for available input on any of the given rfds, or timeout if
   a timeout is given and no input is detected.  wfds are supported
   only for asynchronous 'connect' calls.  efds are not supported
   and must be NULL.

   For simplicity, we detect the death of child processes here and
   synchronously call the SIGCHLD handler.  Since it is possible for
   children to be created without a corresponding pipe handle from which
   to read output, we wait separately on the process handles as well as
   the char_avail events for each process pipe.  We only call
   wait/reap_process when the process actually terminates.

   To reduce the number of places in which Emacs can be hung such that
   C-g is not able to interrupt it, we always wait on interrupt_handle
   (which is signaled by the input thread when C-g is detected).  If we
   detect that we were woken up by C-g, we return -1 with errno set to
   EINTR as on Unix.  */

/* From w32console.c */
extern HANDLE keyboard_handle;

/* From w32xfns.c */
extern HANDLE interrupt_handle;

/* From process.c */
extern int proc_buffered_char[];

int
sys_select (int nfds, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds,
	    const struct timespec *timeout, const sigset_t *ignored)
{
  SELECT_TYPE orfds, owfds;
  DWORD timeout_ms, start_time;
  int i, nh, nc, nr;
  DWORD active;
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAXDESC + MAX_CHILDREN];
  int fdindex[MAXDESC];   /* mapping from wait handles back to descriptors */

  timeout_ms =
    timeout ? (timeout->tv_sec * 1000 + timeout->tv_nsec / 1000000) : INFINITE;

  /* If the descriptor sets are NULL but timeout isn't, then just Sleep.  */
  if (rfds == NULL && wfds == NULL && efds == NULL && timeout != NULL)
    {
      Sleep (timeout_ms);
      return 0;
    }

  /* Otherwise, we only handle rfds and wfds, so fail otherwise.  */
  if ((rfds == NULL && wfds == NULL) || efds != NULL)
    {
      errno = EINVAL;
      return -1;
    }

  if (rfds)
    {
      orfds = *rfds;
      FD_ZERO (rfds);
    }
  else
    FD_ZERO (&orfds);
  if (wfds)
    {
      owfds = *wfds;
      FD_ZERO (wfds);
    }
  else
    FD_ZERO (&owfds);
  nr = 0;

  /* If interrupt_handle is available and valid, always wait on it, to
     detect C-g (quit).  */
  nh = 0;
  if (interrupt_handle && interrupt_handle != INVALID_HANDLE_VALUE)
    {
      wait_hnd[0] = interrupt_handle;
      fdindex[0] = -1;
      nh++;
    }

  /* Build a list of pipe handles to wait on.  */
  for (i = 0; i < nfds; i++)
    if (FD_ISSET (i, &orfds) || FD_ISSET (i, &owfds))
      {
	if (i == 0)
	  {
	    if (keyboard_handle)
	      {
		/* Handle stdin specially */
		wait_hnd[nh] = keyboard_handle;
		fdindex[nh] = i;
		nh++;
	      }

	    /* Check for any emacs-generated input in the queue since
	       it won't be detected in the wait */
	    if (rfds && detect_input_pending ())
	      {
		FD_SET (i, rfds);
		return 1;
	      }
	    else if (noninteractive)
	      {
		if (handle_file_notifications (NULL))
		  return 1;
	      }
	  }
	else
	  {
	    /* Child process and socket/comm port input.  */
	    cp = fd_info[i].cp;
	    if (FD_ISSET (i, &owfds)
		&& cp
		&& (fd_info[i].flags & FILE_CONNECT) == 0)
	      {
		DebPrint (("sys_select: fd %d is in wfds, but FILE_CONNECT is reset!\n", i));
		cp = NULL;
	      }
	    if (cp)
	      {
		int current_status = cp->status;

		if (current_status == STATUS_READ_ACKNOWLEDGED)
		  {
		    /* Tell reader thread which file handle to use. */
		    cp->fd = i;
		    /* Zero out the error code.  */
		    cp->errcode = 0;
		    /* Wake up the reader thread for this process */
		    cp->status = STATUS_READ_READY;
		    if (!SetEvent (cp->char_consumed))
		      DebPrint (("sys_select.SetEvent failed with "
				 "%lu for fd %ld\n", GetLastError (), i));
		  }

#ifdef CHECK_INTERLOCK
		/* slightly crude cross-checking of interlock between threads */

		current_status = cp->status;
		if (WaitForSingleObject (cp->char_avail, 0) == WAIT_OBJECT_0)
		  {
		    /* char_avail has been signaled, so status (which may
		       have changed) should indicate read has completed
		       but has not been acknowledged. */
		    current_status = cp->status;
		    if (current_status != STATUS_READ_SUCCEEDED
			&& current_status != STATUS_READ_FAILED)
		      DebPrint (("char_avail set, but read not completed: status %d\n",
				 current_status));
		  }
		else
		  {
		    /* char_avail has not been signaled, so status should
		       indicate that read is in progress; small possibility
		       that read has completed but event wasn't yet signaled
		       when we tested it (because a context switch occurred
		       or if running on separate CPUs). */
		    if (current_status != STATUS_READ_READY
			&& current_status != STATUS_READ_IN_PROGRESS
			&& current_status != STATUS_READ_SUCCEEDED
			&& current_status != STATUS_READ_FAILED)
		      DebPrint (("char_avail reset, but read status is bad: %d\n",
				 current_status));
		  }
#endif
		wait_hnd[nh] = cp->char_avail;
		fdindex[nh] = i;
		if (!wait_hnd[nh]) emacs_abort ();
		nh++;
#ifdef FULL_DEBUG
		DebPrint (("select waiting on child %d fd %d\n",
			   cp-child_procs, i));
#endif
	      }
	    else
	      {
		/* Unable to find something to wait on for this fd, skip */

		/* Note that this is not a fatal error, and can in fact
		   happen in unusual circumstances.  Specifically, if
		   sys_spawnve fails, eg. because the program doesn't
		   exist, and debug-on-error is t so Fsignal invokes a
		   nested input loop, then the process output pipe is
		   still included in input_wait_mask with no child_proc
		   associated with it.  (It is removed when the debugger
		   exits the nested input loop and the error is thrown.)  */

		DebPrint (("sys_select: fd %ld is invalid! ignoring\n", i));
	      }
	  }
      }

count_children:
  /* Add handles of child processes.  */
  nc = 0;
  for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
    /* Some child_procs might be sockets; ignore them.  Also some
       children may have died already, but we haven't finished reading
       the process output; ignore them too.  */
    if ((CHILD_ACTIVE (cp) && cp->procinfo.hProcess)
	&& (cp->fd < 0
	    || (fd_info[cp->fd].flags & FILE_SEND_SIGCHLD) == 0
	    || (fd_info[cp->fd].flags & FILE_AT_EOF) != 0)
	)
      {
	wait_hnd[nh + nc] = cp->procinfo.hProcess;
	cps[nc] = cp;
	nc++;
      }

  /* Nothing to look for, so we didn't find anything */
  if (nh + nc == 0)
    {
      if (timeout)
	Sleep (timeout_ms);
      if (noninteractive)
	{
	  if (handle_file_notifications (NULL))
	    return 1;
	}
      return 0;
    }

  start_time = GetTickCount ();

  /* Wait for input or child death to be signaled.  If user input is
     allowed, then also accept window messages.  */
  if (FD_ISSET (0, &orfds))
    active = MsgWaitForMultipleObjects (nh + nc, wait_hnd, FALSE, timeout_ms,
					QS_ALLINPUT);
  else
    active = WaitForMultipleObjects (nh + nc, wait_hnd, FALSE, timeout_ms);

  if (active == WAIT_FAILED)
    {
      DebPrint (("select.WaitForMultipleObjects (%d, %lu) failed with %lu\n",
		 nh + nc, timeout_ms, GetLastError ()));
      /* don't return EBADF - this causes wait_reading_process_output to
	 abort; WAIT_FAILED is returned when single-stepping under
	 Windows 95 after switching thread focus in debugger, and
	 possibly at other times. */
      errno = EINTR;
      return -1;
    }
  else if (active == WAIT_TIMEOUT)
    {
      if (noninteractive)
	{
	  if (handle_file_notifications (NULL))
	    return 1;
	}
      return 0;
    }
  else if (active >= WAIT_OBJECT_0
	   && active < WAIT_OBJECT_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_OBJECT_0;
    }
  else if (active >= WAIT_ABANDONED_0
	   && active < WAIT_ABANDONED_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_ABANDONED_0;
    }
  else
    emacs_abort ();

  /* Loop over all handles after active (now officially documented as
     being the first signaled handle in the array).  We do this to
     ensure fairness, so that all channels with data available will be
     processed - otherwise higher numbered channels could be starved. */
  do
    {
      if (active == nh + nc)
	{
	  /* There are messages in the lisp thread's queue; we must
             drain the queue now to ensure they are processed promptly,
             because if we don't do so, we will not be woken again until
             further messages arrive.

	     NB. If ever we allow window message procedures to callback
	     into lisp, we will need to ensure messages are dispatched
	     at a safe time for lisp code to be run (*), and we may also
	     want to provide some hooks in the dispatch loop to cater
	     for modeless dialogs created by lisp (ie. to register
	     window handles to pass to IsDialogMessage).

	     (*) Note that MsgWaitForMultipleObjects above is an
	     internal dispatch point for messages that are sent to
	     windows created by this thread.  */
	  if (drain_message_queue ()
	      /* If drain_message_queue returns non-zero, that means
		 we received a WM_EMACS_FILENOTIFY message.  If this
		 is a TTY frame, we must signal the caller that keyboard
		 input is available, so that w32_console_read_socket
		 will be called to pick up the notifications.  If we
		 don't do that, file notifications will only work when
		 the Emacs TTY frame has focus.  */
	      && FRAME_TERMCAP_P (SELECTED_FRAME ())
	      /* they asked for stdin reads */
	      && FD_ISSET (0, &orfds)
	      /* the stdin handle is valid */
	      && keyboard_handle)
	    {
	      FD_SET (0, rfds);
	      if (nr == 0)
		nr = 1;
	    }
	}
      else if (active >= nh)
	{
	  cp = cps[active - nh];

	  /* We cannot always signal SIGCHLD immediately; if we have not
	     finished reading the process output, we must delay sending
	     SIGCHLD until we do.  */

	  if (cp->fd >= 0 && (fd_info[cp->fd].flags & FILE_AT_EOF) == 0)
	    fd_info[cp->fd].flags |= FILE_SEND_SIGCHLD;
	  /* SIG_DFL for SIGCHLD is ignored */
	  else if (sig_handlers[SIGCHLD] != SIG_DFL &&
		   sig_handlers[SIGCHLD] != SIG_IGN)
	    {
#ifdef FULL_DEBUG
	      DebPrint (("select calling SIGCHLD handler for pid %d\n",
			 cp->pid));
#endif
	      sig_handlers[SIGCHLD] (SIGCHLD);
	    }
	}
      else if (fdindex[active] == -1)
	{
	  /* Quit (C-g) was detected.  */
	  errno = EINTR;
	  return -1;
	}
      else if (rfds && fdindex[active] == 0)
	{
	  /* Keyboard input available */
	  FD_SET (0, rfds);
	  nr++;
	}
      else
	{
	  /* Must be a socket or pipe - read ahead should have
             completed, either succeeding or failing.  If this handle
             was waiting for an async 'connect', reset the connect
             flag, so it could read from now on.  */
	  if (wfds && (fd_info[fdindex[active]].flags & FILE_CONNECT) != 0)
	    {
	      cp = fd_info[fdindex[active]].cp;
	      if (cp)
		{
		  /* Don't reset the FILE_CONNECT bit and don't
		     acknowledge the read if the status is
		     STATUS_CONNECT_FAILED or some other
		     failure. That's because the thread exits in those
		     cases, so it doesn't need the ACK, and we want to
		     keep the FILE_CONNECT bit as evidence that the
		     connect failed, to be checked in sys_read.  */
		  if (cp->status == STATUS_READ_SUCCEEDED)
		    {
		      fd_info[cp->fd].flags &= ~FILE_CONNECT;
		      cp->status = STATUS_READ_ACKNOWLEDGED;
		    }
		  ResetEvent (cp->char_avail);
		}
	      FD_SET (fdindex[active], wfds);
	    }
	  else if (rfds)
	    FD_SET (fdindex[active], rfds);
	  nr++;
	}

      /* Even though wait_reading_process_output only reads from at most
	 one channel, we must process all channels here so that we reap
	 all children that have died.  */
      while (++active < nh + nc)
	if (WaitForSingleObject (wait_hnd[active], 0) == WAIT_OBJECT_0)
	  break;
    } while (active < nh + nc);

  if (noninteractive)
    {
      if (handle_file_notifications (NULL))
	nr++;
    }

  /* If no input has arrived and timeout hasn't expired, wait again.  */
  if (nr == 0)
    {
      DWORD elapsed = GetTickCount () - start_time;

      if (timeout_ms > elapsed)	/* INFINITE is MAX_UINT */
	{
	  if (timeout_ms != INFINITE)
	    timeout_ms -= elapsed;
	  goto count_children;
	}
    }

  return nr;
}

/* Substitute for certain kill () operations */

static BOOL CALLBACK
find_child_console (HWND hwnd, LPARAM arg)
{
  child_process * cp = (child_process *) arg;
  DWORD process_id;

  GetWindowThreadProcessId (hwnd, &process_id);
  if (process_id == cp->procinfo.dwProcessId)
    {
      char window_class[32];

      GetClassName (hwnd, window_class, sizeof (window_class));
      if (strcmp (window_class,
		  (os_subtype == OS_9X)
		  ? "tty"
		  : "ConsoleWindowClass") == 0)
	{
	  cp->hwnd = hwnd;
	  return FALSE;
	}
    }
  /* keep looking */
  return TRUE;
}

typedef BOOL (WINAPI * DebugBreakProcess_Proc) (
    HANDLE hProcess);

/* Emulate 'kill', but only for other processes.  */
int
sys_kill (pid_t pid, int sig)
{
  child_process *cp;
  HANDLE proc_hand;
  int need_to_free = 0;
  int rc = 0;

  /* Each process is in its own process group.  */
  if (pid < 0)
    pid = -pid;

  /* Only handle signals that can be mapped to a similar behavior on Windows */
  if (sig != 0
      && sig != SIGINT && sig != SIGKILL && sig != SIGQUIT && sig != SIGHUP && sig != SIGTRAP)
    {
      errno = EINVAL;
      return -1;
    }

  if (sig == 0)
    {
      /* It will take _some_ time before PID 4 or less on Windows will
	 be Emacs...  */
      if (pid <= 4)
	{
	  errno = EPERM;
	  return -1;
	}
      proc_hand = OpenProcess (PROCESS_QUERY_INFORMATION, 0, pid);
      if (proc_hand == NULL)
        {
	  DWORD err = GetLastError ();

	  switch (err)
	    {
	    case ERROR_ACCESS_DENIED: /* existing process, but access denied */
	      errno = EPERM;
	      return -1;
	    case ERROR_INVALID_PARAMETER: /* process PID does not exist */
	      errno = ESRCH;
	      return -1;
	    }
	}
      else
	CloseHandle (proc_hand);
      return 0;
    }

  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      /* We were passed a PID of something other than our subprocess.
	 If that is our own PID, we will send to ourself a message to
	 close the selected frame, which does not necessarily
	 terminates Emacs.  But then we are not supposed to call
	 sys_kill with our own PID.  */

      DWORD desiredAccess =
	(sig == SIGTRAP) ? PROCESS_ALL_ACCESS : PROCESS_TERMINATE;

      proc_hand = OpenProcess (desiredAccess, 0, pid);
      if (proc_hand == NULL)
        {
	  errno = EPERM;
	  return -1;
	}
      need_to_free = 1;
    }
  else
    {
      proc_hand = cp->procinfo.hProcess;
      pid = cp->procinfo.dwProcessId;

      /* Try to locate console window for process. */
      EnumWindows (find_child_console, (LPARAM) cp);
    }

  if (sig == SIGINT || sig == SIGQUIT)
    {
      if (NILP (Vw32_start_process_share_console) && cp && cp->hwnd)
	{
	  BYTE control_scan_code = (BYTE) MapVirtualKey (VK_CONTROL, 0);
	  /* Fake Ctrl-C for SIGINT, and Ctrl-Break for SIGQUIT.  */
	  BYTE vk_break_code = (sig == SIGINT) ? 'C' : VK_CANCEL;
	  BYTE break_scan_code = (BYTE) MapVirtualKey (vk_break_code, 0);
	  HWND foreground_window;

	  if (break_scan_code == 0)
	    {
	      /* Fake Ctrl-C for SIGQUIT if we can't manage Ctrl-Break. */
	      vk_break_code = 'C';
	      break_scan_code = (BYTE) MapVirtualKey (vk_break_code, 0);
	    }

	  foreground_window = GetForegroundWindow ();
	  if (foreground_window)
	    {
              /* NT 5.0, and apparently also Windows 98, will not allow
		 a Window to be set to foreground directly without the
		 user's involvement. The workaround is to attach
		 ourselves to the thread that owns the foreground
		 window, since that is the only thread that can set the
		 foreground window.  */
              DWORD foreground_thread, child_thread;
              foreground_thread =
		GetWindowThreadProcessId (foreground_window, NULL);
	      if (foreground_thread == GetCurrentThreadId ()
                  || !AttachThreadInput (GetCurrentThreadId (),
                                         foreground_thread, TRUE))
                foreground_thread = 0;

              child_thread = GetWindowThreadProcessId (cp->hwnd, NULL);
	      if (child_thread == GetCurrentThreadId ()
                  || !AttachThreadInput (GetCurrentThreadId (),
                                         child_thread, TRUE))
                child_thread = 0;

              /* Set the foreground window to the child.  */
              if (SetForegroundWindow (cp->hwnd))
                {
		  /* Record the state of the Ctrl key: the user could
		     have it depressed while we are simulating Ctrl-C,
		     in which case we will have to leave the state of
		     Ctrl depressed when we are done.  */
		  short ctrl_state = GetKeyState (VK_CONTROL) & 0x8000;

                  /* Generate keystrokes as if user had typed Ctrl-Break or
                     Ctrl-C.  */
                  keybd_event (VK_CONTROL, control_scan_code, 0, 0);
                  keybd_event (vk_break_code, break_scan_code,
		    (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY), 0);
                  keybd_event (vk_break_code, break_scan_code,
                    (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY)
                    | KEYEVENTF_KEYUP, 0);
                  keybd_event (VK_CONTROL, control_scan_code,
                               KEYEVENTF_KEYUP, 0);

                  /* Sleep for a bit to give time for Emacs frame to respond
                     to focus change events (if Emacs was active app).  */
                  Sleep (100);

                  SetForegroundWindow (foreground_window);
		  /* If needed, restore the state of Ctrl.  */
		  if (ctrl_state != 0)
		    keybd_event (VK_CONTROL, control_scan_code, 0, 0);
                }
              /* Detach from the foreground and child threads now that
                 the foreground switching is over.  */
              if (foreground_thread)
                AttachThreadInput (GetCurrentThreadId (),
                                   foreground_thread, FALSE);
              if (child_thread)
                AttachThreadInput (GetCurrentThreadId (),
                                   child_thread, FALSE);
            }
        }
      /* Ctrl-Break is NT equivalent of SIGINT.  */
      else if (!GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid))
        {
	  DebPrint (("sys_kill.GenerateConsoleCtrlEvent return %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
	}
    }
  else if (sig == SIGTRAP)
    {
      static DebugBreakProcess_Proc s_pfn_Debug_Break_Process = NULL;

      if (g_b_init_debug_break_process == 0)
	{
	  g_b_init_debug_break_process = 1;
	  s_pfn_Debug_Break_Process = (DebugBreakProcess_Proc)
	    GetProcAddress (GetModuleHandle ("kernel32.dll"),
			    "DebugBreakProcess");
	}

      if (s_pfn_Debug_Break_Process == NULL)
	{
	  errno = ENOTSUP;
	  rc = -1;
	}
      else if (!s_pfn_Debug_Break_Process (proc_hand))
	{
	  DWORD err = GetLastError ();

	  DebPrint (("sys_kill.DebugBreakProcess return %d "
		     "for pid %lu\n", err, pid));

	  switch (err)
	    {
	    case ERROR_ACCESS_DENIED:
	      errno = EPERM;
	      break;
	    default:
	      errno = EINVAL;
	      break;
	    }

	  rc = -1;
	}
    }
  else
    {
      if (NILP (Vw32_start_process_share_console) && cp && cp->hwnd)
	{
#if 1
	  if (os_subtype == OS_9X)
	    {
/*
   Another possibility is to try terminating the VDM out-right by
   calling the Shell VxD (id 0x17) V86 interface, function #4
   "SHELL_Destroy_VM", ie.

     mov edx,4
     mov ebx,vm_handle
     call shellapi

   First need to determine the current VM handle, and then arrange for
   the shellapi call to be made from the system vm (by using
   Switch_VM_and_callback).

   Could try to invoke DestroyVM through CallVxD.

*/
#if 0
	      /* On Windows 95, posting WM_QUIT causes the 16-bit subsystem
		 to hang when cmdproxy is used in conjunction with
		 command.com for an interactive shell.  Posting
		 WM_CLOSE pops up a dialog that, when Yes is selected,
		 does the same thing.  TerminateProcess is also less
		 than ideal in that subprocesses tend to stick around
		 until the machine is shutdown, but at least it
		 doesn't freeze the 16-bit subsystem.  */
	      PostMessage (cp->hwnd, WM_QUIT, 0xff, 0);
#endif
	      if (!TerminateProcess (proc_hand, 0xff))
		{
		  DebPrint (("sys_kill.TerminateProcess returned %d "
			     "for pid %lu\n", GetLastError (), pid));
		  errno = EINVAL;
		  rc = -1;
		}
	    }
	  else
#endif
	    PostMessage (cp->hwnd, WM_CLOSE, 0, 0);
	}
      /* Kill the process.  On W32 this doesn't kill child processes
	 so it doesn't work very well for shells which is why it's not
	 used in every case.  */
      else if (!TerminateProcess (proc_hand, 0xff))
        {
	  DebPrint (("sys_kill.TerminateProcess returned %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
        }
    }

  if (need_to_free)
    CloseHandle (proc_hand);

  return rc;
}

/* The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are *not* inheritable, we make them
   stdin, stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the std handles to inheritable duplicates of the ones being passed in.
     (Note that _get_osfhandle() is an io.h procedure that retrieves the
     NT file handle for a crt file descriptor.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Close the std handles passed to the child.
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

void
prepare_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  HANDLE parent;
  HANDLE newstdin, newstdout, newstderr;

  parent = GetCurrentProcess ();

  handles[0] = GetStdHandle (STD_INPUT_HANDLE);
  handles[1] = GetStdHandle (STD_OUTPUT_HANDLE);
  handles[2] = GetStdHandle (STD_ERROR_HANDLE);

  /* make inheritable copies of the new handles */
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (in),
		       parent,
		       &newstdin,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating input handle for child", Qnil);

  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (out),
		       parent,
		       &newstdout,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating output handle for child", Qnil);

  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (err),
		       parent,
		       &newstderr,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating error handle for child", Qnil);

  /* and store them as our std handles */
  if (!SetStdHandle (STD_INPUT_HANDLE, newstdin))
    report_file_error ("Changing stdin handle", Qnil);

  if (!SetStdHandle (STD_OUTPUT_HANDLE, newstdout))
    report_file_error ("Changing stdout handle", Qnil);

  if (!SetStdHandle (STD_ERROR_HANDLE, newstderr))
    report_file_error ("Changing stderr handle", Qnil);
}

void
reset_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  /* close the duplicated handles passed to the child */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  /* now restore parent's saved std handles */
  SetStdHandle (STD_INPUT_HANDLE, handles[0]);
  SetStdHandle (STD_OUTPUT_HANDLE, handles[1]);
  SetStdHandle (STD_ERROR_HANDLE, handles[2]);
}

void
set_process_dir (char * dir)
{
  process_dir = dir;
}

/* To avoid problems with winsock implementations that work over dial-up
   connections causing or requiring a connection to exist while Emacs is
   running, Emacs no longer automatically loads winsock on startup if it
   is present.  Instead, it will be loaded when open-network-stream is
   first called.

   To allow full control over when winsock is loaded, we provide these
   two functions to dynamically load and unload winsock.  This allows
   dial-up users to only be connected when they actually need to use
   socket services.  */

/* From w32.c */
extern HANDLE winsock_lib;
extern BOOL term_winsock (void);

DEFUN ("w32-has-winsock", Fw32_has_winsock, Sw32_has_winsock, 0, 1, 0,
       doc: /* Test for presence of the Windows socket library `winsock'.
Returns non-nil if winsock support is present, nil otherwise.

If the optional argument LOAD-NOW is non-nil, the winsock library is
also loaded immediately if not already loaded.  If winsock is loaded,
the winsock local hostname is returned (since this may be different from
the value of `system-name' and should supplant it), otherwise t is
returned to indicate winsock support is present.  */)
  (Lisp_Object load_now)
{
  int have_winsock;

  have_winsock = init_winsock (!NILP (load_now));
  if (have_winsock)
    {
      if (winsock_lib != NULL)
	{
	  /* Return new value for system-name.  The best way to do this
	     is to call init_system_name, saving and restoring the
	     original value to avoid side-effects.  */
	  Lisp_Object orig_hostname = Vsystem_name;
	  Lisp_Object hostname;

	  init_system_name ();
	  hostname = Vsystem_name;
	  Vsystem_name = orig_hostname;
	  return hostname;
	}
      return Qt;
    }
  return Qnil;
}

DEFUN ("w32-unload-winsock", Fw32_unload_winsock, Sw32_unload_winsock,
       0, 0, 0,
       doc: /* Unload the Windows socket library `winsock' if loaded.
This is provided to allow dial-up socket connections to be disconnected
when no longer needed.  Returns nil without unloading winsock if any
socket connections still exist.  */)
  (void)
{
  return term_winsock () ? Qt : Qnil;
}


/* Some miscellaneous functions that are Windows specific, but not GUI
   specific (ie. are applicable in terminal or batch mode as well).  */

DEFUN ("w32-short-file-name", Fw32_short_file_name, Sw32_short_file_name, 1, 1, 0,
       doc: /* Return the short file name version (8.3) of the full path of FILENAME.
If FILENAME does not exist, return nil.
All path elements in FILENAME are converted to their short names.  */)
  (Lisp_Object filename)
{
  char shortname[MAX_PATH];

  CHECK_STRING (filename);

  /* first expand it.  */
  filename = Fexpand_file_name (filename, Qnil);

  /* luckily, this returns the short version of each element in the path.  */
  if (w32_get_short_filename (SSDATA (ENCODE_FILE (filename)),
			      shortname, MAX_PATH) == 0)
    return Qnil;

  dostounix_filename (shortname);

  /* No need to DECODE_FILE, because 8.3 names are pure ASCII.   */
  return build_string (shortname);
}


DEFUN ("w32-long-file-name", Fw32_long_file_name, Sw32_long_file_name,
       1, 1, 0,
       doc: /* Return the long file name version of the full path of FILENAME.
If FILENAME does not exist, return nil.
All path elements in FILENAME are converted to their long names.  */)
  (Lisp_Object filename)
{
  char longname[ MAX_UTF8_PATH ];
  int drive_only = 0;

  CHECK_STRING (filename);

  if (SBYTES (filename) == 2
      && *(SDATA (filename) + 1) == ':')
    drive_only = 1;

  /* first expand it.  */
  filename = Fexpand_file_name (filename, Qnil);

  if (!w32_get_long_filename (SSDATA (ENCODE_FILE (filename)), longname,
			      MAX_UTF8_PATH))
    return Qnil;

  dostounix_filename (longname);

  /* If we were passed only a drive, make sure that a slash is not appended
     for consistency with directories.  Allow for drive mapping via SUBST
     in case expand-file-name is ever changed to expand those.  */
  if (drive_only && longname[1] == ':' && longname[2] == '/' && !longname[3])
    longname[2] = '\0';

  return DECODE_FILE (build_unibyte_string (longname));
}

DEFUN ("w32-set-process-priority", Fw32_set_process_priority,
       Sw32_set_process_priority, 2, 2, 0,
       doc: /* Set the priority of PROCESS to PRIORITY.
If PROCESS is nil, the priority of Emacs is changed, otherwise the
priority of the process whose pid is PROCESS is changed.
PRIORITY should be one of the symbols high, normal, or low;
any other symbol will be interpreted as normal.

If successful, the return value is t, otherwise nil.  */)
  (Lisp_Object process, Lisp_Object priority)
{
  HANDLE proc_handle = GetCurrentProcess ();
  DWORD  priority_class = NORMAL_PRIORITY_CLASS;
  Lisp_Object result = Qnil;

  CHECK_SYMBOL (priority);

  if (!NILP (process))
    {
      DWORD pid;
      child_process *cp;

      CHECK_NUMBER (process);

      /* Allow pid to be an internally generated one, or one obtained
	 externally.  This is necessary because real pids on Windows 95 are
	 negative.  */

      pid = XINT (process);
      cp = find_child_pid (pid);
      if (cp != NULL)
	pid = cp->procinfo.dwProcessId;

      proc_handle = OpenProcess (PROCESS_SET_INFORMATION, FALSE, pid);
    }

  if (EQ (priority, Qhigh))
    priority_class = HIGH_PRIORITY_CLASS;
  else if (EQ (priority, Qlow))
    priority_class = IDLE_PRIORITY_CLASS;

  if (proc_handle != NULL)
    {
      if (SetPriorityClass (proc_handle, priority_class))
	result = Qt;
      if (!NILP (process))
	CloseHandle (proc_handle);
    }

  return result;
}

DEFUN ("w32-application-type", Fw32_application_type,
       Sw32_application_type, 1, 1, 0,
       doc: /* Return the type of an MS-Windows PROGRAM.

Knowing the type of an executable could be useful for formatting
file names passed to it or for quoting its command-line arguments.

PROGRAM should specify an executable file, including the extension.

The value is one of the following:

`dos'        -- a DOS .com program or some other non-PE executable
`cygwin'     -- a Cygwin program that depends on Cygwin DLL
`msys'       -- an MSYS 1.x or MSYS2 program
`w32-native' -- a native Windows application
`unknown'    -- a file that doesn't exist, or cannot be open, or whose
                name is not encodable in the current ANSI codepage.

Note that for .bat and .cmd batch files the function returns the type
of their command interpreter, as specified by the \"COMSPEC\"
environment variable.

This function returns `unknown' for programs whose file names
include characters not supported by the current ANSI codepage, as
such programs cannot be invoked by Emacs anyway.  */)
     (Lisp_Object program)
{
  int is_dos_app, is_cygwin_app, is_msys_app, dummy;
  Lisp_Object encoded_progname;
  char *progname, progname_a[MAX_PATH];

  program = Fexpand_file_name (program, Qnil);
  encoded_progname = ENCODE_FILE (program);
  progname = SSDATA (encoded_progname);
  unixtodos_filename (progname);
  filename_to_ansi (progname, progname_a);
  /* Reject file names that cannot be encoded in the current ANSI
     codepage.  */
  if (_mbspbrk ((unsigned char *)progname_a, (const unsigned char *)"?"))
    return Qunknown;

  if (w32_executable_type (progname_a, &is_dos_app, &is_cygwin_app,
			   &is_msys_app, &dummy) != 0)
    return Qunknown;
  if (is_dos_app)
    return Qdos;
  if (is_cygwin_app)
    return Qcygwin;
  if (is_msys_app)
    return Qmsys;
  return Qw32_native;
}

#ifdef HAVE_LANGINFO_CODESET
/* Emulation of nl_langinfo.  Used in fns.c:Flocale_info.  */
char *
nl_langinfo (nl_item item)
{
  /* Conversion of Posix item numbers to their Windows equivalents.  */
  static const LCTYPE w32item[] = {
    LOCALE_IDEFAULTANSICODEPAGE,
    LOCALE_SDAYNAME1, LOCALE_SDAYNAME2, LOCALE_SDAYNAME3,
    LOCALE_SDAYNAME4, LOCALE_SDAYNAME5, LOCALE_SDAYNAME6, LOCALE_SDAYNAME7,
    LOCALE_SMONTHNAME1, LOCALE_SMONTHNAME2, LOCALE_SMONTHNAME3,
    LOCALE_SMONTHNAME4, LOCALE_SMONTHNAME5, LOCALE_SMONTHNAME6,
    LOCALE_SMONTHNAME7, LOCALE_SMONTHNAME8, LOCALE_SMONTHNAME9,
    LOCALE_SMONTHNAME10, LOCALE_SMONTHNAME11, LOCALE_SMONTHNAME12
  };

  static char *nl_langinfo_buf = NULL;
  static int   nl_langinfo_len = 0;

  if (nl_langinfo_len <= 0)
    nl_langinfo_buf = xmalloc (nl_langinfo_len = 1);

  if (item < 0 || item >= _NL_NUM)
    nl_langinfo_buf[0] = 0;
  else
    {
      LCID cloc = GetThreadLocale ();
      int need_len = GetLocaleInfo (cloc, w32item[item] | LOCALE_USE_CP_ACP,
				    NULL, 0);

      if (need_len <= 0)
	nl_langinfo_buf[0] = 0;
      else
	{
	  if (item == CODESET)
	    {
	      need_len += 2;	/* for the "cp" prefix */
	      if (need_len < 8)	/* for the case we call GetACP */
		need_len = 8;
	    }
	  if (nl_langinfo_len <= need_len)
	    nl_langinfo_buf = xrealloc (nl_langinfo_buf,
					nl_langinfo_len = need_len);
	  if (!GetLocaleInfo (cloc, w32item[item] | LOCALE_USE_CP_ACP,
			      nl_langinfo_buf, nl_langinfo_len))
	    nl_langinfo_buf[0] = 0;
	  else if (item == CODESET)
	    {
	      if (strcmp (nl_langinfo_buf, "0") == 0 /* CP_ACP */
		  || strcmp (nl_langinfo_buf, "1") == 0) /* CP_OEMCP */
		sprintf (nl_langinfo_buf, "cp%u", GetACP ());
	      else
		{
		  memmove (nl_langinfo_buf + 2, nl_langinfo_buf,
			   strlen (nl_langinfo_buf) + 1);
		  nl_langinfo_buf[0] = 'c';
		  nl_langinfo_buf[1] = 'p';
		}
	    }
	}
    }
  return nl_langinfo_buf;
}
#endif	/* HAVE_LANGINFO_CODESET */

DEFUN ("w32-get-locale-info", Fw32_get_locale_info,
       Sw32_get_locale_info, 1, 2, 0,
       doc: /* Return information about the Windows locale LCID.
By default, return a three letter locale code which encodes the default
language as the first two characters, and the country or regional variant
as the third letter.  For example, ENU refers to `English (United States)',
while ENC means `English (Canadian)'.

If the optional argument LONGFORM is t, the long form of the locale
name is returned, e.g. `English (United States)' instead; if LONGFORM
is a number, it is interpreted as an LCTYPE constant and the corresponding
locale information is returned.

If LCID (a 16-bit number) is not a valid locale, the result is nil.  */)
  (Lisp_Object lcid, Lisp_Object longform)
{
  int got_abbrev;
  int got_full;
  char abbrev_name[32] = { 0 };
  char full_name[256] = { 0 };

  CHECK_NUMBER (lcid);

  if (!IsValidLocale (XINT (lcid), LCID_SUPPORTED))
    return Qnil;

  if (NILP (longform))
    {
      got_abbrev = GetLocaleInfo (XINT (lcid),
				  LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
				  abbrev_name, sizeof (abbrev_name));
      if (got_abbrev)
	return build_string (abbrev_name);
    }
  else if (EQ (longform, Qt))
    {
      got_full = GetLocaleInfo (XINT (lcid),
				LOCALE_SLANGUAGE | LOCALE_USE_CP_ACP,
				full_name, sizeof (full_name));
      if (got_full)
	return DECODE_SYSTEM (build_string (full_name));
    }
  else if (NUMBERP (longform))
    {
      got_full = GetLocaleInfo (XINT (lcid),
				XINT (longform),
				full_name, sizeof (full_name));
      /* GetLocaleInfo's return value includes the terminating null
	 character, when the returned information is a string, whereas
	 make_unibyte_string needs the string length without the
	 terminating null.  */
      if (got_full)
	return make_unibyte_string (full_name, got_full - 1);
    }

  return Qnil;
}


DEFUN ("w32-get-current-locale-id", Fw32_get_current_locale_id,
       Sw32_get_current_locale_id, 0, 0, 0,
       doc: /* Return Windows locale id for current locale setting.
This is a numerical value; use `w32-get-locale-info' to convert to a
human-readable form.  */)
  (void)
{
  return make_number (GetThreadLocale ());
}

static DWORD
int_from_hex (char * s)
{
  DWORD val = 0;
  static char hex[] = "0123456789abcdefABCDEF";
  char * p;

  while (*s && (p = strchr (hex, *s)) != NULL)
    {
      unsigned digit = p - hex;
      if (digit > 15)
	digit -= 6;
      val = val * 16 + digit;
      s++;
    }
  return val;
}

/* We need to build a global list, since the EnumSystemLocale callback
   function isn't given a context pointer.  */
Lisp_Object Vw32_valid_locale_ids;

static BOOL CALLBACK ALIGN_STACK
enum_locale_fn (LPTSTR localeNum)
{
  DWORD id = int_from_hex (localeNum);
  Vw32_valid_locale_ids = Fcons (make_number (id), Vw32_valid_locale_ids);
  return TRUE;
}

DEFUN ("w32-get-valid-locale-ids", Fw32_get_valid_locale_ids,
       Sw32_get_valid_locale_ids, 0, 0, 0,
       doc: /* Return list of all valid Windows locale ids.
Each id is a numerical value; use `w32-get-locale-info' to convert to a
human-readable form.  */)
  (void)
{
  Vw32_valid_locale_ids = Qnil;

  EnumSystemLocales (enum_locale_fn, LCID_SUPPORTED);

  Vw32_valid_locale_ids = Fnreverse (Vw32_valid_locale_ids);
  return Vw32_valid_locale_ids;
}


DEFUN ("w32-get-default-locale-id", Fw32_get_default_locale_id, Sw32_get_default_locale_id, 0, 1, 0,
       doc: /* Return Windows locale id for default locale setting.
By default, the system default locale setting is returned; if the optional
parameter USERP is non-nil, the user default locale setting is returned.
This is a numerical value; use `w32-get-locale-info' to convert to a
human-readable form.  */)
  (Lisp_Object userp)
{
  if (NILP (userp))
    return make_number (GetSystemDefaultLCID ());
  return make_number (GetUserDefaultLCID ());
}


DEFUN ("w32-set-current-locale", Fw32_set_current_locale, Sw32_set_current_locale, 1, 1, 0,
       doc: /* Make Windows locale LCID be the current locale setting for Emacs.
If successful, the new locale id is returned, otherwise nil.  */)
  (Lisp_Object lcid)
{
  CHECK_NUMBER (lcid);

  if (!IsValidLocale (XINT (lcid), LCID_SUPPORTED))
    return Qnil;

  if (!SetThreadLocale (XINT (lcid)))
    return Qnil;

  /* Need to set input thread locale if present.  */
  if (dwWindowsThreadId)
    /* Reply is not needed.  */
    PostThreadMessage (dwWindowsThreadId, WM_EMACS_SETLOCALE, XINT (lcid), 0);

  return make_number (GetThreadLocale ());
}


/* We need to build a global list, since the EnumCodePages callback
   function isn't given a context pointer.  */
Lisp_Object Vw32_valid_codepages;

static BOOL CALLBACK ALIGN_STACK
enum_codepage_fn (LPTSTR codepageNum)
{
  DWORD id = atoi (codepageNum);
  Vw32_valid_codepages = Fcons (make_number (id), Vw32_valid_codepages);
  return TRUE;
}

DEFUN ("w32-get-valid-codepages", Fw32_get_valid_codepages,
       Sw32_get_valid_codepages, 0, 0, 0,
       doc: /* Return list of all valid Windows codepages.  */)
  (void)
{
  Vw32_valid_codepages = Qnil;

  EnumSystemCodePages (enum_codepage_fn, CP_SUPPORTED);

  Vw32_valid_codepages = Fnreverse (Vw32_valid_codepages);
  return Vw32_valid_codepages;
}


DEFUN ("w32-get-console-codepage", Fw32_get_console_codepage,
       Sw32_get_console_codepage, 0, 0, 0,
       doc: /* Return current Windows codepage for console input.  */)
  (void)
{
  return make_number (GetConsoleCP ());
}


DEFUN ("w32-set-console-codepage", Fw32_set_console_codepage,
       Sw32_set_console_codepage, 1, 1, 0,
       doc: /* Make Windows codepage CP be the codepage for Emacs tty keyboard input.
This codepage setting affects keyboard input in tty mode.
If successful, the new CP is returned, otherwise nil.  */)
  (Lisp_Object cp)
{
  CHECK_NUMBER (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (!SetConsoleCP (XINT (cp)))
    return Qnil;

  return make_number (GetConsoleCP ());
}


DEFUN ("w32-get-console-output-codepage", Fw32_get_console_output_codepage,
       Sw32_get_console_output_codepage, 0, 0, 0,
       doc: /* Return current Windows codepage for console output.  */)
  (void)
{
  return make_number (GetConsoleOutputCP ());
}


DEFUN ("w32-set-console-output-codepage", Fw32_set_console_output_codepage,
       Sw32_set_console_output_codepage, 1, 1, 0,
       doc: /* Make Windows codepage CP be the codepage for Emacs console output.
This codepage setting affects display in tty mode.
If successful, the new CP is returned, otherwise nil.  */)
  (Lisp_Object cp)
{
  CHECK_NUMBER (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (!SetConsoleOutputCP (XINT (cp)))
    return Qnil;

  return make_number (GetConsoleOutputCP ());
}


DEFUN ("w32-get-codepage-charset", Fw32_get_codepage_charset,
       Sw32_get_codepage_charset, 1, 1, 0,
       doc: /* Return charset ID corresponding to codepage CP.
Returns nil if the codepage is not valid or its charset ID could
not be determined.

Note that this function is only guaranteed to work with ANSI
codepages; most console codepages are not supported and will
yield nil.  */)
  (Lisp_Object cp)
{
  CHARSETINFO info;
  DWORD_PTR dwcp;

  CHECK_NUMBER (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  /* Going through a temporary DWORD_PTR variable avoids compiler warning
     about cast to pointer from integer of different size, when
     building --with-wide-int or building for 64bit.  */
  dwcp = XINT (cp);
  if (TranslateCharsetInfo ((DWORD *) dwcp, &info, TCI_SRCCODEPAGE))
    return make_number (info.ciCharset);

  return Qnil;
}


DEFUN ("w32-get-valid-keyboard-layouts", Fw32_get_valid_keyboard_layouts,
       Sw32_get_valid_keyboard_layouts, 0, 0, 0,
       doc: /* Return list of Windows keyboard languages and layouts.
The return value is a list of pairs of language id and layout id.  */)
  (void)
{
  int num_layouts = GetKeyboardLayoutList (0, NULL);
  HKL * layouts = (HKL *) alloca (num_layouts * sizeof (HKL));
  Lisp_Object obj = Qnil;

  if (GetKeyboardLayoutList (num_layouts, layouts) == num_layouts)
    {
      while (--num_layouts >= 0)
	{
	  HKL kl = layouts[num_layouts];

	  obj = Fcons (Fcons (make_number (LOWORD (kl)),
			      make_number (HIWORD (kl))),
		       obj);
	}
    }

  return obj;
}


DEFUN ("w32-get-keyboard-layout", Fw32_get_keyboard_layout,
       Sw32_get_keyboard_layout, 0, 0, 0,
       doc: /* Return current Windows keyboard language and layout.
The return value is the cons of the language id and the layout id.  */)
  (void)
{
  HKL kl = GetKeyboardLayout (dwWindowsThreadId);

  return Fcons (make_number (LOWORD (kl)),
		make_number (HIWORD (kl)));
}


DEFUN ("w32-set-keyboard-layout", Fw32_set_keyboard_layout,
       Sw32_set_keyboard_layout, 1, 1, 0,
       doc: /* Make LAYOUT be the current keyboard layout for Emacs.
The keyboard layout setting affects interpretation of keyboard input.
If successful, the new layout id is returned, otherwise nil.  */)
  (Lisp_Object layout)
{
  HKL kl;

  CHECK_CONS (layout);
  CHECK_NUMBER_CAR (layout);
  CHECK_NUMBER_CDR (layout);

  kl = (HKL) (UINT_PTR) ((XINT (XCAR (layout)) & 0xffff)
			 | (XINT (XCDR (layout)) << 16));

  /* Synchronize layout with input thread.  */
  if (dwWindowsThreadId)
    {
      if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_SETKEYBOARDLAYOUT,
			     (WPARAM) kl, 0))
	{
	  MSG msg;
	  GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);

	  if (msg.wParam == 0)
	    return Qnil;
	}
    }
  else if (!ActivateKeyboardLayout (kl, 0))
    return Qnil;

  return Fw32_get_keyboard_layout ();
}

/* Two variables to interface between get_lcid and the EnumLocales
   callback function below.  */
#ifndef LOCALE_NAME_MAX_LENGTH
# define LOCALE_NAME_MAX_LENGTH 85
#endif
static LCID found_lcid;
static char lname[3 * LOCALE_NAME_MAX_LENGTH + 1 + 1];

/* Callback function for EnumLocales.  */
static BOOL CALLBACK
get_lcid_callback (LPTSTR locale_num_str)
{
  char *endp;
  char locval[2 * LOCALE_NAME_MAX_LENGTH + 1 + 1];
  LCID try_lcid = strtoul (locale_num_str, &endp, 16);

  if (GetLocaleInfo (try_lcid, LOCALE_SABBREVLANGNAME,
		     locval, LOCALE_NAME_MAX_LENGTH))
    {
      size_t locval_len;

      /* This is for when they only specify the language, as in "ENU".  */
      if (stricmp (locval, lname) == 0)
	{
	  found_lcid = try_lcid;
	  return FALSE;
	}
      locval_len = strlen (locval);
      strcpy (locval + locval_len, "_");
      if (GetLocaleInfo (try_lcid, LOCALE_SABBREVCTRYNAME,
			 locval + locval_len + 1, LOCALE_NAME_MAX_LENGTH))
	{
	  locval_len = strlen (locval);
	  if (strnicmp (locval, lname, locval_len) == 0
	      && (lname[locval_len] == '.'
		  || lname[locval_len] == '\0'))
	    {
	      found_lcid = try_lcid;
	      return FALSE;
	    }
	}
    }
  return TRUE;
}

/* Return the Locale ID (LCID) number given the locale's name, a
   string, in LOCALE_NAME.  This works by enumerating all the locales
   supported by the system, until we find one whose name matches
   LOCALE_NAME.  */
static LCID
get_lcid (const char *locale_name)
{
  /* A simple cache.  */
  static LCID last_lcid;
  static char last_locale[1000];

  /* The code below is not thread-safe, as it uses static variables.
     But this function is called only from the Lisp thread.  */
  if (last_lcid > 0 && strcmp (locale_name, last_locale) == 0)
    return last_lcid;

  strncpy (lname, locale_name, sizeof (lname) - 1);
  lname[sizeof (lname) - 1] = '\0';
  found_lcid = 0;
  EnumSystemLocales (get_lcid_callback, LCID_SUPPORTED);
  if (found_lcid > 0)
    {
      last_lcid = found_lcid;
      strcpy (last_locale, locale_name);
    }
  return found_lcid;
}

#ifndef _NLSCMPERROR
# define _NLSCMPERROR INT_MAX
#endif
#ifndef LINGUISTIC_IGNORECASE
# define LINGUISTIC_IGNORECASE  0x00000010
#endif

typedef int (WINAPI *CompareStringW_Proc)
  (LCID, DWORD, LPCWSTR, int, LPCWSTR, int);

int
w32_compare_strings (const char *s1, const char *s2, char *locname,
		     int ignore_case)
{
  LCID lcid = GetThreadLocale ();
  wchar_t *string1_w, *string2_w;
  int val, needed;
  static CompareStringW_Proc pCompareStringW;
  DWORD flags = 0;

  USE_SAFE_ALLOCA;

  /* The LCID machinery doesn't seem to support the "C" locale, so we
     need to do that by hand.  */
  if (locname
      && ((locname[0] == 'C' && (locname[1] == '\0' || locname[1] == '.'))
	  || strcmp (locname, "POSIX") == 0))
    return (ignore_case ? stricmp (s1, s2) : strcmp (s1, s2));

  if (!g_b_init_compare_string_w)
    {
      if (os_subtype == OS_9X)
	{
	  pCompareStringW =
            (CompareStringW_Proc) GetProcAddress (LoadLibrary ("Unicows.dll"),
                                                  "CompareStringW");
	  if (!pCompareStringW)
	    {
	      errno = EINVAL;
	      /* This return value is compatible with wcscoll and
		 other MS CRT functions.  */
	      return _NLSCMPERROR;
	    }
	}
      else
	pCompareStringW = CompareStringW;

      g_b_init_compare_string_w = 1;
    }

  needed = pMultiByteToWideChar (CP_UTF8, MB_ERR_INVALID_CHARS, s1, -1, NULL, 0);
  if (needed > 0)
    {
      SAFE_NALLOCA (string1_w, 1, needed + 1);
      pMultiByteToWideChar (CP_UTF8, MB_ERR_INVALID_CHARS, s1, -1,
			    string1_w, needed);
    }
  else
    {
      errno = EINVAL;
      return _NLSCMPERROR;
    }

  needed = pMultiByteToWideChar (CP_UTF8, MB_ERR_INVALID_CHARS, s2, -1, NULL, 0);
  if (needed > 0)
    {
      SAFE_NALLOCA (string2_w, 1, needed + 1);
      pMultiByteToWideChar (CP_UTF8, MB_ERR_INVALID_CHARS, s2, -1,
			    string2_w, needed);
    }
  else
    {
      SAFE_FREE ();
      errno = EINVAL;
      return _NLSCMPERROR;
    }

  if (locname)
    {
      /* Convert locale name string to LCID.  We don't want to use
	 LocaleNameToLCID because (a) it is only available since
	 Vista, and (b) it doesn't accept locale names returned by
	 'setlocale' and 'GetLocaleInfo'.  */
      LCID new_lcid = get_lcid (locname);

      if (new_lcid > 0)
	lcid = new_lcid;
      else
	error ("Invalid locale %s: Invalid argument", locname);
    }

  if (ignore_case)
    {
      /* NORM_IGNORECASE ignores any tertiary distinction, not just
	 case variants.  LINGUISTIC_IGNORECASE is more selective, and
	 is sensitive to the locale's language, but it is not
	 available before Vista.  */
      if (w32_major_version >= 6)
	flags |= LINGUISTIC_IGNORECASE;
      else
	flags |= NORM_IGNORECASE;
    }
  /* This approximates what glibc collation functions do when the
     locale's codeset is UTF-8.  */
  if (!NILP (Vw32_collate_ignore_punctuation))
    flags |= NORM_IGNORESYMBOLS;
  val = pCompareStringW (lcid, flags, string1_w, -1, string2_w, -1);
  SAFE_FREE ();
  if (!val)
    {
      errno = EINVAL;
      return _NLSCMPERROR;
    }
  return val - 2;
}


void
syms_of_ntproc (void)
{
  DEFSYM (Qhigh, "high");
  DEFSYM (Qlow, "low");
  DEFSYM (Qcygwin, "cygwin");
  DEFSYM (Qmsys, "msys");
  DEFSYM (Qw32_native, "w32-native");

  defsubr (&Sw32_has_winsock);
  defsubr (&Sw32_unload_winsock);

  defsubr (&Sw32_short_file_name);
  defsubr (&Sw32_long_file_name);
  defsubr (&Sw32_set_process_priority);
  defsubr (&Sw32_application_type);
  defsubr (&Sw32_get_locale_info);
  defsubr (&Sw32_get_current_locale_id);
  defsubr (&Sw32_get_default_locale_id);
  defsubr (&Sw32_get_valid_locale_ids);
  defsubr (&Sw32_set_current_locale);

  defsubr (&Sw32_get_console_codepage);
  defsubr (&Sw32_set_console_codepage);
  defsubr (&Sw32_get_console_output_codepage);
  defsubr (&Sw32_set_console_output_codepage);
  defsubr (&Sw32_get_valid_codepages);
  defsubr (&Sw32_get_codepage_charset);

  defsubr (&Sw32_get_valid_keyboard_layouts);
  defsubr (&Sw32_get_keyboard_layout);
  defsubr (&Sw32_set_keyboard_layout);

  DEFVAR_LISP ("w32-quote-process-args", Vw32_quote_process_args,
	       doc: /* Non-nil enables quoting of process arguments to ensure correct parsing.
Because Windows does not directly pass argv arrays to child processes,
programs have to reconstruct the argv array by parsing the command
line string.  For an argument to contain a space, it must be enclosed
in double quotes or it will be parsed as multiple arguments.

If the value is a character, that character will be used to escape any
quote characters that appear, otherwise a suitable escape character
will be chosen based on the type of the program.  */);
  Vw32_quote_process_args = Qt;

  DEFVAR_LISP ("w32-start-process-show-window",
	       Vw32_start_process_show_window,
	       doc: /* When nil, new child processes hide their windows.
When non-nil, they show their window in the method of their choice.
This variable doesn't affect GUI applications, which will never be hidden.  */);
  Vw32_start_process_show_window = Qnil;

  DEFVAR_LISP ("w32-start-process-share-console",
	       Vw32_start_process_share_console,
	       doc: /* When nil, new child processes are given a new console.
When non-nil, they share the Emacs console; this has the limitation of
allowing only one DOS subprocess to run at a time (whether started directly
or indirectly by Emacs), and preventing Emacs from cleanly terminating the
subprocess group, but may allow Emacs to interrupt a subprocess that doesn't
otherwise respond to interrupts from Emacs.  */);
  Vw32_start_process_share_console = Qnil;

  DEFVAR_LISP ("w32-start-process-inherit-error-mode",
	       Vw32_start_process_inherit_error_mode,
	       doc: /* When nil, new child processes revert to the default error mode.
When non-nil, they inherit their error mode setting from Emacs, which stops
them blocking when trying to access unmounted drives etc.  */);
  Vw32_start_process_inherit_error_mode = Qt;

  DEFVAR_INT ("w32-pipe-read-delay", w32_pipe_read_delay,
	      doc: /* Forced delay before reading subprocess output.
This is done to improve the buffering of subprocess output, by
avoiding the inefficiency of frequently reading small amounts of data.

If positive, the value is the number of milliseconds to sleep before
reading the subprocess output.  If negative, the magnitude is the number
of time slices to wait (effectively boosting the priority of the child
process temporarily).  A value of zero disables waiting entirely.  */);
  w32_pipe_read_delay = 50;

  DEFVAR_INT ("w32-pipe-buffer-size", w32_pipe_buffer_size,
	      doc: /* Size of buffer for pipes created to communicate with subprocesses.
The size is in bytes, and must be non-negative.  The default is zero,
which lets the OS use its default size, usually 4KB (4096 bytes).
Any negative value means to use the default value of zero.  */);
  w32_pipe_buffer_size = 0;

  DEFVAR_LISP ("w32-downcase-file-names", Vw32_downcase_file_names,
	       doc: /* Non-nil means convert all-upper case file names to lower case.
This applies when performing completions and file name expansion.
Note that the value of this setting also affects remote file names,
so you probably don't want to set to non-nil if you use case-sensitive
filesystems via ange-ftp.  */);
  Vw32_downcase_file_names = Qnil;

#if 0
  DEFVAR_LISP ("w32-generate-fake-inodes", Vw32_generate_fake_inodes,
	       doc: /* Non-nil means attempt to fake realistic inode values.
This works by hashing the truename of files, and should detect
aliasing between long and short (8.3 DOS) names, but can have
false positives because of hash collisions.  Note that determining
the truename of a file can be slow.  */);
  Vw32_generate_fake_inodes = Qnil;
#endif

  DEFVAR_LISP ("w32-get-true-file-attributes", Vw32_get_true_file_attributes,
	       doc: /* Non-nil means determine accurate file attributes in `file-attributes'.
This option controls whether to issue additional system calls to determine
accurate link counts, file type, and ownership information.  It is more
useful for files on NTFS volumes, where hard links and file security are
supported, than on volumes of the FAT family.

Without these system calls, link count will always be reported as 1 and file
ownership will be attributed to the current user.
The default value `local' means only issue these system calls for files
on local fixed drives.  A value of nil means never issue them.
Any other non-nil value means do this even on remote and removable drives
where the performance impact may be noticeable even on modern hardware.  */);
  Vw32_get_true_file_attributes = Qlocal;

  DEFVAR_LISP ("w32-collate-ignore-punctuation",
	       Vw32_collate_ignore_punctuation,
	       doc: /* Non-nil causes string collation functions ignore punctuation on MS-Windows.
On Posix platforms, `string-collate-lessp' and `string-collate-equalp'
ignore punctuation characters when they compare strings, if the
locale's codeset is UTF-8, as in \"en_US.UTF-8\".  Binding this option
to a non-nil value will achieve a similar effect on MS-Windows, where
locales with UTF-8 codeset are not supported.

Note that setting this to non-nil will also ignore blanks and symbols
in the strings.  So do NOT use this option when comparing file names
for equality, only when you need to sort them.  */);
  Vw32_collate_ignore_punctuation = Qnil;

  staticpro (&Vw32_valid_locale_ids);
  staticpro (&Vw32_valid_codepages);
}
/* end of w32proc.c */
