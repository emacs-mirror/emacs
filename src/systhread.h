/* System thread definitions
Copyright (C) 2012-2017 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef SYSTHREAD_H
#define SYSTHREAD_H

#ifdef THREADS_ENABLED

#ifdef HAVE_PTHREAD

#include <pthread.h>

/* A system mutex is just a pthread mutex.  This is only used for the
   GIL.  */
typedef pthread_mutex_t sys_mutex_t;

typedef pthread_cond_t sys_cond_t;

/* A system thread.  */
typedef pthread_t sys_thread_t;

#else /* HAVE_PTHREAD */

#ifdef WINDOWSNT

/* This header is indirectly included in every source file.  We don't
   want to include windows.h in every source file, so we repeat
   declarations of the few necessary data types here (under different
   names, to avoid conflicts with files that do include
   windows.h).  */

typedef struct {
  struct _CRITICAL_SECTION_DEBUG *DebugInfo;
  long LockCount;
  long RecursionCount;
  void *OwningThread;
  void *LockSemaphore;
  unsigned long SpinCount;
} w32thread_critsect;

enum { CONDV_SIGNAL = 0, CONDV_BROADCAST = 1, CONDV_MAX = 2 };

typedef struct {
  /* Count of threads that are waiting for this condition variable.  */
  unsigned wait_count;
  /* Critical section to protect changes to the count above.  */
  w32thread_critsect wait_count_lock;
  /* Handles of events used for signal and broadcast.  */
  void *events[CONDV_MAX];
  bool initialized;
} w32thread_cond_t;

typedef w32thread_critsect sys_mutex_t;

typedef w32thread_cond_t sys_cond_t;

typedef unsigned long sys_thread_t;

#else  /* !WINDOWSNT */

#error port me

#endif	/* WINDOWSNT */
#endif /* HAVE_PTHREAD */

#else /* THREADS_ENABLED */

/* For the no-threads case we can simply use dummy definitions.  */
typedef int sys_mutex_t;
typedef int sys_cond_t;
typedef int sys_thread_t;

#endif /* THREADS_ENABLED */

typedef void *(thread_creation_function) (void *);

extern void sys_mutex_init (sys_mutex_t *);
extern void sys_mutex_lock (sys_mutex_t *);
extern void sys_mutex_unlock (sys_mutex_t *);

extern void sys_cond_init (sys_cond_t *);
extern void sys_cond_wait (sys_cond_t *, sys_mutex_t *);
extern void sys_cond_signal (sys_cond_t *);
extern void sys_cond_broadcast (sys_cond_t *);
extern void sys_cond_destroy (sys_cond_t *);

extern sys_thread_t sys_thread_self (void);

extern int sys_thread_create (sys_thread_t *, const char *,
			      thread_creation_function *,
			      void *);

extern void sys_thread_yield (void);

#endif /* SYSTHREAD_H */
