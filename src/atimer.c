/* Asynchronous timers.
   Copyright (C) 2000-2017 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "keyboard.h"
#include "syssignal.h"
#include "systime.h"
#include "atimer.h"
#include <unistd.h>

#ifdef HAVE_TIMERFD
#include <errno.h>
# include <sys/timerfd.h>
#endif

#ifdef MSDOS
#include "msdos.h"
#endif

/* Free-list of atimer structures.  */

static struct atimer *free_atimers;

/* List of currently not running timers due to a call to
   lock_atimer.  */

static struct atimer *stopped_atimers;

/* List of active atimers, sorted by expiration time.  The timer that
   will become ripe next is always at the front of this list.  */

static struct atimer *atimers;

#ifdef HAVE_ITIMERSPEC
/* The alarm timer and whether it was properly initialized, if
   POSIX timers are available.  */
static timer_t alarm_timer;
static bool alarm_timer_ok;

# ifdef HAVE_TIMERFD
/* File descriptor for timer, or -1 if it could not be created.  */
static int timerfd;
# else
enum { timerfd = -1 };
# endif
#endif

/* Block/unblock SIGALRM.  */

static void
block_atimers (sigset_t *oldset)
{
  sigset_t blocked;
  sigemptyset (&blocked);
  sigaddset (&blocked, SIGALRM);
  sigaddset (&blocked, SIGINT);
  pthread_sigmask (SIG_BLOCK, &blocked, oldset);
}
static void
unblock_atimers (sigset_t const *oldset)
{
  pthread_sigmask (SIG_SETMASK, oldset, 0);
}

/* Function prototypes.  */

static void set_alarm (void);
static void schedule_atimer (struct atimer *);
static struct atimer *append_atimer_lists (struct atimer *,
                                           struct atimer *);

/* Start a new atimer of type TYPE.  TIMESTAMP specifies when the timer is
   ripe.  FN is the function to call when the timer fires.
   CLIENT_DATA is stored in the client_data member of the atimer
   structure returned and so made available to FN when it is called.

   If TYPE is ATIMER_ABSOLUTE, TIMESTAMP is the absolute time at which the
   timer fires.

   If TYPE is ATIMER_RELATIVE, the timer is ripe TIMESTAMP seconds in the
   future.

   In both cases, the timer is automatically freed after it has fired.

   If TYPE is ATIMER_CONTINUOUS, the timer fires every TIMESTAMP seconds.

   Value is a pointer to the atimer started.  It can be used in calls
   to cancel_atimer; don't free it yourself.  */

struct atimer *
start_atimer (enum atimer_type type, struct timespec timestamp,
	      atimer_callback fn, void *client_data)
{
  struct atimer *t;
  sigset_t oldset;

  /* Round TIMESTAMP up to the next full second if we don't have itimers.  */
#ifndef HAVE_SETITIMER
  if (timestamp.tv_nsec != 0 && timestamp.tv_sec < TYPE_MAXIMUM (time_t))
    timestamp = make_timespec (timestamp.tv_sec + 1, 0);
#endif /* not HAVE_SETITIMER */

  /* Get an atimer structure from the free-list, or allocate
     a new one.  */
  if (free_atimers)
    {
      t = free_atimers;
      free_atimers = t->next;
    }
  else
    t = xmalloc (sizeof *t);

  /* Fill the atimer structure.  */
  memset (t, 0, sizeof *t);
  t->type = type;
  t->fn = fn;
  t->client_data = client_data;

  block_atimers (&oldset);

  /* Compute the timer's expiration time.  */
  switch (type)
    {
    case ATIMER_ABSOLUTE:
      t->expiration = timestamp;
      break;

    case ATIMER_RELATIVE:
      t->expiration = timespec_add (current_timespec (), timestamp);
      break;

    case ATIMER_CONTINUOUS:
      t->expiration = timespec_add (current_timespec (), timestamp);
      t->interval = timestamp;
      break;
    }

  /* Insert the timer in the list of active atimers.  */
  schedule_atimer (t);
  unblock_atimers (&oldset);

  /* Arrange for a SIGALRM at the time the next atimer is ripe.  */
  set_alarm ();

  return t;
}


/* Cancel and free atimer TIMER.  */

void
cancel_atimer (struct atimer *timer)
{
  int i;
  sigset_t oldset;

  block_atimers (&oldset);

  for (i = 0; i < 2; ++i)
    {
      struct atimer *t, *prev;
      struct atimer **list = i ? &stopped_atimers : &atimers;

      /* See if TIMER is active or stopped.  */
      for (t = *list, prev = NULL; t && t != timer; prev = t, t = t->next)
	;

      /* If it is, take it off its list, and put in on the free-list.
	 We don't bother to arrange for setting a different alarm time,
	 since a too early one doesn't hurt.  */
      if (t)
	{
	  if (prev)
	    prev->next = t->next;
	  else
	    *list = t->next;

	  t->next = free_atimers;
	  free_atimers = t;
	  break;
	}
    }

  unblock_atimers (&oldset);
}


/* Append two lists of atimers LIST_1 and LIST_2 and return the
   result list.  */

static struct atimer *
append_atimer_lists (struct atimer *list_1, struct atimer *list_2)
{
  if (list_1 == NULL)
    return list_2;
  else if (list_2 == NULL)
    return list_1;
  else
    {
      struct atimer *p;

      for (p = list_1; p->next; p = p->next)
	;
      p->next = list_2;
      return list_1;
    }
}


/* Stop all timers except timer T.  T null means stop all timers.  */

void
stop_other_atimers (struct atimer *t)
{
  sigset_t oldset;
  block_atimers (&oldset);

  if (t)
    {
      struct atimer *p, *prev;

      /* See if T is active.  */
      for (p = atimers, prev = NULL; p && p != t; prev = p, p = p->next)
	;

      if (p == t)
	{
	  if (prev)
	    prev->next = t->next;
	  else
	    atimers = t->next;
	  t->next = NULL;
	}
      else
	/* T is not active.  Let's handle this like T == 0.  */
	t = NULL;
    }

  stopped_atimers = append_atimer_lists (atimers, stopped_atimers);
  atimers = t;
  unblock_atimers (&oldset);
}


/* Run all timers again, if some have been stopped with a call to
   stop_other_atimers.  */

void
run_all_atimers (void)
{
  if (stopped_atimers)
    {
      struct atimer *t = atimers;
      struct atimer *next;
      sigset_t oldset;

      block_atimers (&oldset);
      atimers = stopped_atimers;
      stopped_atimers = NULL;

      while (t)
	{
	  next = t->next;
	  schedule_atimer (t);
	  t = next;
	}

      unblock_atimers (&oldset);
    }
}


/* Arrange for a SIGALRM to arrive when the next timer is ripe.  */

static void
set_alarm (void)
{
  if (atimers)
    {
#ifdef HAVE_SETITIMER
      struct itimerval it;
#endif
      struct timespec now, interval;

#ifdef HAVE_ITIMERSPEC
      if (0 <= timerfd || alarm_timer_ok)
	{
	  struct itimerspec ispec;
	  ispec.it_value = atimers->expiration;
	  ispec.it_interval.tv_sec = ispec.it_interval.tv_nsec = 0;
# ifdef HAVE_TIMERFD
	  if (timerfd_settime (timerfd, TFD_TIMER_ABSTIME, &ispec, 0) == 0)
	    {
	      add_timer_wait_descriptor (timerfd);
	      return;
	    }
# endif
	  if (alarm_timer_ok
	      && timer_settime (alarm_timer, TIMER_ABSTIME, &ispec, 0) == 0)
	    return;
	}
#endif

      /* Determine interval till the next timer is ripe.
	 Don't set the interval to 0; this disables the timer.  */
      now = current_timespec ();
      interval = (timespec_cmp (atimers->expiration, now) <= 0
		  ? make_timespec (0, 1000 * 1000)
		  : timespec_sub (atimers->expiration, now));

#ifdef HAVE_SETITIMER

      memset (&it, 0, sizeof it);
      it.it_value = make_timeval (interval);
      setitimer (ITIMER_REAL, &it, 0);
#else /* not HAVE_SETITIMER */
      alarm (max (interval.tv_sec, 1));
#endif /* not HAVE_SETITIMER */
    }
}


/* Insert timer T into the list of active atimers `atimers', keeping
   the list sorted by expiration time.  T must not be in this list
   already.  */

static void
schedule_atimer (struct atimer *t)
{
  struct atimer *a = atimers, *prev = NULL;

  /* Look for the first atimer that is ripe after T.  */
  while (a && timespec_cmp (a->expiration, t->expiration) < 0)
    prev = a, a = a->next;

  /* Insert T in front of the atimer found, if any.  */
  if (prev)
    prev->next = t;
  else
    atimers = t;

  t->next = a;
}

static void
run_timers (void)
{
  struct timespec now = current_timespec ();

  while (atimers && timespec_cmp (atimers->expiration, now) <= 0)
    {
      struct atimer *t = atimers;
      atimers = atimers->next;
      t->fn (t);

      if (t->type == ATIMER_CONTINUOUS)
	{
	  t->expiration = timespec_add (now, t->interval);
	  schedule_atimer (t);
	}
      else
	{
	  t->next = free_atimers;
	  free_atimers = t;
	}
    }

  set_alarm ();
}


/* Signal handler for SIGALRM.  SIGNO is the signal number, i.e.
   SIGALRM.  */

static void
handle_alarm_signal (int sig)
{
  pending_signals = 1;
}

#ifdef HAVE_TIMERFD

/* Called from wait_reading_process_output when FD, which
   should be equal to TIMERFD, is available for reading.  */

void
timerfd_callback (int fd, void *arg)
{
  ptrdiff_t nbytes;
  uint64_t expirations;

  eassert (fd == timerfd);
  nbytes = emacs_read (fd, &expirations, sizeof (expirations));

  if (nbytes == sizeof (expirations))
    {
      /* Timer should expire just once.  */
      eassert (expirations == 1);
      do_pending_atimers ();
    }
  else if (nbytes < 0)
    /* For some not yet known reason, we may get weird event and no
       data on timer descriptor.  This can break Gnus at least, see:
       https://lists.gnu.org/r/emacs-devel/2014-07/msg00503.html.  */
    eassert (errno == EAGAIN);
  else
    /* I don't know what else can happen with this descriptor.  */
    emacs_abort ();
}

#endif /* HAVE_TIMERFD */

/* Do pending timers.  */

void
do_pending_atimers (void)
{
  if (atimers)
    {
      sigset_t oldset;
      block_atimers (&oldset);
      run_timers ();
      unblock_atimers (&oldset);
    }
}


/* Turn alarms on/off.  This seems to be temporarily necessary on
   some systems like HPUX (see process.c).  */

void
turn_on_atimers (bool on)
{
  if (on)
    set_alarm ();
  else
    {
#ifdef HAVE_ITIMERSPEC
      struct itimerspec ispec;
      memset (&ispec, 0, sizeof ispec);
      if (alarm_timer_ok)
	timer_settime (alarm_timer, TIMER_ABSTIME, &ispec, 0);
# ifdef HAVE_TIMERFD
      timerfd_settime (timerfd, TFD_TIMER_ABSTIME, &ispec, 0);
# endif
#endif
      alarm (0);
    }
}

/* This is intended to use from automated tests.  */

#ifdef ENABLE_CHECKING

#define MAXTIMERS 10

struct atimer_result
{
  /* Time when we expect this timer to trigger.  */
  struct timespec expected;

  /* Timer status: -1 if not triggered, 0 if triggered
     too early or too late, 1 if triggered timely.  */
  int intime;
};

static void
debug_timer_callback (struct atimer *t)
{
  struct timespec now = current_timespec ();
  struct atimer_result *r = (struct atimer_result *) t->client_data;
  int result = timespec_cmp (now, r->expected);

  if (result < 0)
    /* Too early.  */
    r->intime = 0;
  else if (result >= 0)
    {
#ifdef HAVE_SETITIMER
      struct timespec delta = timespec_sub (now, r->expected);
      /* Too late if later than expected + 0.02s.  FIXME:
	 this should depend from system clock resolution.  */
      if (timespec_cmp (delta, make_timespec (0, 20000000)) > 0)
	r->intime = 0;
      else
#endif /* HAVE_SETITIMER */
	r->intime = 1;
    }
}

DEFUN ("debug-timer-check", Fdebug_timer_check, Sdebug_timer_check, 0, 0, 0,
       doc: /* Run internal self-tests to check timers subsystem.
Return t if all self-tests are passed, nil otherwise.  */)
  (void)
{
  int i, ok;
  struct atimer *timer;
  struct atimer_result *results[MAXTIMERS];
  struct timespec t = make_timespec (0, 0);

  /* Arm MAXTIMERS relative timers to trigger with 0.1s intervals.  */
  for (i = 0; i < MAXTIMERS; i++)
    {
      results[i] = xmalloc (sizeof (struct atimer_result));
      t = timespec_add (t, make_timespec (0, 100000000));
      results[i]->expected = timespec_add (current_timespec (), t);
      results[i]->intime = -1;
      timer = start_atimer (ATIMER_RELATIVE, t,
			    debug_timer_callback, results[i]);
    }

#ifdef HAVE_TIMERFD
  /* Wait for 1s but process timers.  */
  wait_reading_process_output (1, 0, 0, false, Qnil, NULL, 0);
#else
  /* If timerfd is not supported, wait_reading_process_output won't
     pay attention to timers that expired, and the callbacks won't be
     called.  So we need to run the expired timers' callbacks by
     hand.  */
  /* Wait 1.2 sec for the timers to expire.  */
  struct timespec tend =
    timespec_add (current_timespec (), make_timespec (1, 200000000));

  while (timespec_cmp (current_timespec (), tend) < 0)
    {
      /* Wait for 5 msec between iterations.  */
      wait_reading_process_output (0, 5000000, 0, false, Qnil, NULL, 0);
      if (pending_signals)
	do_pending_atimers ();
    }
#endif
  /* Shut up the compiler by "using" this variable.  */
  (void) timer;

  for (i = 0, ok = 0; i < MAXTIMERS; i++)
    ok += results[i]->intime, xfree (results[i]);

  return ok == MAXTIMERS ? Qt : Qnil;
}

#endif /* ENABLE_CHECKING */

void
init_atimer (void)
{
#ifdef HAVE_ITIMERSPEC
# ifdef HAVE_TIMERFD
  /* Until this feature is considered stable, you can ask to not use it.  */
  timerfd = (egetenv ("EMACS_IGNORE_TIMERFD") ? -1 :
	     timerfd_create (CLOCK_REALTIME, TFD_NONBLOCK | TFD_CLOEXEC));
# endif
  if (timerfd < 0)
    {
      struct sigevent sigev;
      sigev.sigev_notify = SIGEV_SIGNAL;
      sigev.sigev_signo = SIGALRM;
      sigev.sigev_value.sival_ptr = &alarm_timer;
      alarm_timer_ok
	= timer_create (CLOCK_REALTIME, &sigev, &alarm_timer) == 0;
    }
#endif
  free_atimers = stopped_atimers = atimers = NULL;

  /* pending_signals is initialized in init_keyboard.  */
  struct sigaction action;
  emacs_sigaction_init (&action, handle_alarm_signal);
  sigaction (SIGALRM, &action, 0);

#ifdef ENABLE_CHECKING
  defsubr (&Sdebug_timer_check);
#endif
}
