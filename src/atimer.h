/* Asynchronous timers.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* Forward declaration.  */

struct atimer;

/* Types of timers.  */

enum atimer_type
{
  /* Timer is ripe at some absolute time.  */
  ATIMER_ABSOLUTE,

  /* Timer is ripe at now plus an offset.  */
  ATIMER_RELATIVE,

  /* Timer runs continously.  */
  ATIMER_CONTINUOUS
};

/* Type of timer callback functions.  */

typedef void (* atimer_callback) P_ ((struct atimer *timer));

/* Structure describing an asynchronous timer.  */

struct atimer
{
  /* The type of this timer.  */
  enum atimer_type type;
  
  /* Time when this timer is ripe.  */
  EMACS_TIME expiration;

  /* Interval of this timer.  */
  EMACS_TIME interval;

  /* Function to call when timer is ripe.  Interupt input is
     garanteed to not be blocked when this function is called.  */
  atimer_callback fn;

  /* Additional user-specified data to pass to FN.  */
  void *client_data;

  /* Next in list of active or free atimers.  */
  struct atimer *next;
};

/* Function prototypes.  */

struct atimer *start_atimer P_ ((enum atimer_type, EMACS_TIME,
				 atimer_callback, void *));
void cancel_atimer P_ ((struct atimer *));
void do_pending_atimers P_ ((void));
void init_atimer P_ ((void));
void turn_on_atimers P_ ((int));

