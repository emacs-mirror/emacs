/* Asynchronous timers.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007, 2008  Free Software Foundation, Inc.

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
#include <stdio.h>
#include <lisp.h>
#include <syssignal.h>
#include <systime.h>
#include <blockinput.h>
#include <atimer.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* Free-list of atimer structures.  */

static struct atimer *free_atimers;

/* List of currently not running timers due to a call to
   lock_atimer.  */

static struct atimer *stopped_atimers;

/* List of active atimers, sorted by expiration time.  The timer that
   will become ripe next is always at the front of this list.  */

static struct atimer *atimers;

/* Non-zero means alarm_signal_handler has found ripe timers but
   interrupt_input_blocked was non-zero.  In this case, timer
   functions are not called until the next UNBLOCK_INPUT because timer
   functions are expected to call X, and X cannot be assumed to be
   reentrant.  */

int pending_atimers;

/* Block/unblock SIGALRM.  */

#define BLOCK_ATIMERS   sigblock (sigmask (SIGALRM))
#define UNBLOCK_ATIMERS sigunblock (sigmask (SIGALRM))

/* Function prototypes.  */

static void set_alarm P_ ((void));
static void schedule_atimer P_ ((struct atimer *));
static struct atimer *append_atimer_lists P_ ((struct atimer *,
					       struct atimer *));
SIGTYPE alarm_signal_handler ();


/* Start a new atimer of type TYPE.  TIME specifies when the timer is
   ripe.  FN is the function to call when the timer fires.
   CLIENT_DATA is stored in the client_data member of the atimer
   structure returned and so made available to FN when it is called.

   If TYPE is ATIMER_ABSOLUTE, TIME is the absolute time at which the
   timer fires.

   If TYPE is ATIMER_RELATIVE, the timer is ripe TIME s/us in the
   future.

   In both cases, the timer is automatically freed after it has fired.

   If TYPE is ATIMER_CONTINUOUS, the timer fires every TIME s/us.

   Value is a pointer to the atimer started.  It can be used in calls
   to cancel_atimer; don't free it yourself.  */

struct atimer *
start_atimer (type, time, fn, client_data)
     enum atimer_type type;
     EMACS_TIME time;
     atimer_callback fn;
     void *client_data;
{
  struct atimer *t;

  /* Round TIME up to the next full second if we don't have
     itimers.  */
#ifndef HAVE_SETITIMER
  if (EMACS_USECS (time) != 0)
    {
      EMACS_SET_USECS (time, 0);
      EMACS_SET_SECS (time, EMACS_SECS (time) + 1);
    }
#endif /* not HAVE_SETITIMER */

  /* Get an atimer structure from the free-list, or allocate
     a new one.  */
  if (free_atimers)
    {
      t = free_atimers;
      free_atimers = t->next;
    }
  else
    t = (struct atimer *) xmalloc (sizeof *t);

  /* Fill the atimer structure.  */
  bzero (t, sizeof *t);
  t->type = type;
  t->fn = fn;
  t->client_data = client_data;

  BLOCK_ATIMERS;

  /* Compute the timer's expiration time.  */
  switch (type)
    {
    case ATIMER_ABSOLUTE:
      t->expiration = time;
      break;

    case ATIMER_RELATIVE:
      EMACS_GET_TIME (t->expiration);
      EMACS_ADD_TIME (t->expiration, t->expiration, time);
      break;

    case ATIMER_CONTINUOUS:
      EMACS_GET_TIME (t->expiration);
      EMACS_ADD_TIME (t->expiration, t->expiration, time);
      t->interval = time;
      break;
    }

  /* Insert the timer in the list of active atimers.  */
  schedule_atimer (t);
  UNBLOCK_ATIMERS;

  /* Arrange for a SIGALRM at the time the next atimer is ripe.  */
  set_alarm ();

  return t;
}


/* Cancel and free atimer TIMER.  */

void
cancel_atimer (timer)
     struct atimer *timer;
{
  int i;

  BLOCK_ATIMERS;

  for (i = 0; i < 2; ++i)
    {
      struct atimer *t, *prev;
      struct atimer **list = i ? &stopped_atimers : &atimers;

      /* See if TIMER is active or stopped.  */
      for (t = *list, prev = NULL; t && t != timer; prev = t, t = t->next)
	;

      /* If it is, take it off the its list, and put in on the
	 free-list.  We don't bother to arrange for setting a
	 different alarm time, since a too early one doesn't hurt.  */
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

  UNBLOCK_ATIMERS;
}


/* Append two lists of atimers LIST1 and LIST2 and return the
   result list.  */

static struct atimer *
append_atimer_lists (list1, list2)
     struct atimer *list1, *list2;
{
  if (list1 == NULL)
    return list2;
  else if (list2 == NULL)
    return list1;
  else
    {
      struct atimer *p;

      for (p = list1; p->next; p = p->next)
	;
      p->next = list2;
      return list1;
    }
}


/* Stop all timers except timer T.  T null means stop all timers.  */

void
stop_other_atimers (t)
     struct atimer *t;
{
  BLOCK_ATIMERS;

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
  UNBLOCK_ATIMERS;
}


/* Run all timers again, if some have been stopped with a call to
   stop_other_atimers.  */

void
run_all_atimers ()
{
  if (stopped_atimers)
    {
      struct atimer *t = atimers;
      struct atimer *next;

      BLOCK_ATIMERS;
      atimers = stopped_atimers;
      stopped_atimers = NULL;

      while (t)
	{
	  next = t->next;
	  schedule_atimer (t);
	  t = next;
	}

      UNBLOCK_ATIMERS;
    }
}


/* A version of run_all_timers suitable for a record_unwind_protect.  */

Lisp_Object
unwind_stop_other_atimers (dummy)
     Lisp_Object dummy;
{
  run_all_atimers ();
  return Qnil;
}


/* Arrange for a SIGALRM to arrive when the next timer is ripe.  */

static void
set_alarm ()
{
#if defined (USG) && !defined (POSIX_SIGNALS)
  /* USG systems forget handlers when they are used;
     must reestablish each time.  */
  signal (SIGALRM, alarm_signal_handler);
#endif /* USG */

  if (atimers)
    {
      EMACS_TIME now, time;
#ifdef HAVE_SETITIMER
      struct itimerval it;
#endif

      /* Determine s/us till the next timer is ripe.  */
      EMACS_GET_TIME (now);
      EMACS_SUB_TIME (time, atimers->expiration, now);

#ifdef HAVE_SETITIMER
      /* Don't set the interval to 0; this disables the timer.  */
      if (EMACS_TIME_LE (atimers->expiration, now))
	{
	  EMACS_SET_SECS (time, 0);
	  EMACS_SET_USECS (time, 1000);
	}

      bzero (&it, sizeof it);
      it.it_value = time;
      setitimer (ITIMER_REAL, &it, 0);
#else /* not HAVE_SETITIMER */
      alarm (max (EMACS_SECS (time), 1));
#endif /* not HAVE_SETITIMER */
    }
}


/* Insert timer T into the list of active atimers `atimers', keeping
   the list sorted by expiration time.  T must not be in this list
   already.  */

static void
schedule_atimer (t)
     struct atimer *t;
{
  struct atimer *a = atimers, *prev = NULL;

  /* Look for the first atimer that is ripe after T.  */
  while (a && EMACS_TIME_GT (t->expiration, a->expiration))
    prev = a, a = a->next;

  /* Insert T in front of the atimer found, if any.  */
  if (prev)
    prev->next = t;
  else
    atimers = t;

  t->next = a;
}

static void
run_timers ()
{
  EMACS_TIME now;

  EMACS_GET_TIME (now);

  while (atimers
	 && (pending_atimers = interrupt_input_blocked) == 0
	 && EMACS_TIME_LE (atimers->expiration, now))
    {
      struct atimer *t;

      t = atimers;
      atimers = atimers->next;
#ifndef MAC_OSX
      t->fn (t);
#endif

      if (t->type == ATIMER_CONTINUOUS)
	{
	  EMACS_ADD_TIME (t->expiration, now, t->interval);
	  schedule_atimer (t);
	}
      else
	{
	  t->next = free_atimers;
	  free_atimers = t;
	}
#ifdef MAC_OSX
      /* Fix for Ctrl-G.  Perhaps this should apply to all platforms. */
      t->fn (t); 
#endif

      EMACS_GET_TIME (now);
    }

  if (! pending_atimers)
    set_alarm ();
}


/* Signal handler for SIGALRM.  SIGNO is the signal number, i.e.
   SIGALRM.  */

SIGTYPE
alarm_signal_handler (signo)
     int signo;
{
  pending_atimers = 1;
#ifndef SYNC_INPUT
  run_timers ();
#endif
}


/* Call alarm_signal_handler for pending timers.  */

void
do_pending_atimers ()
{
  if (pending_atimers)
    {
      BLOCK_ATIMERS;
      run_timers ();
      UNBLOCK_ATIMERS;
    }
}


/* Turn alarms on/off.  This seems to be temporarily necessary on
   some systems like HPUX (see process.c).  */

void
turn_on_atimers (on)
     int on;
{
  if (on)
    {
      signal (SIGALRM, alarm_signal_handler);
      set_alarm ();
    }
  else
    alarm (0);
}


void
init_atimer ()
{
  free_atimers = atimers = NULL;
  pending_atimers = 0;
  signal (SIGALRM, alarm_signal_handler);
}

/* arch-tag: e6308261-eec6-404b-89fb-6e5909518d70
   (do not change this comment) */
