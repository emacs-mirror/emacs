/* System thread definitions
Copyright (C) 2012-2026 Free Software Foundation, Inc.

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

#include <attribute.h>

#ifdef THREADS_ENABLED

#ifdef HAVE_PTHREAD

#include <pthread.h>

/* Deal with platforms like GNU/Linux on 32-bit HPPA, where pthread_mutex_t
   and pthread_cond_t have alignments stricter than what malloc guarantees.
   Unfortunately POSIX allows this curious situation.
   Do this by allocating possibly-poorly-aligned objects a bit larger
   than pthread_mutex_t and pthread_cond_t, and then aligning pointers
   to these objects at runtime.

   Although since glibc 2.4 (2006) the stricter alignment has not been
   needed by the underlying 32-bit HPPA code so the types are overaligned,
   the overalignment is still present in glibc 2.42 (2025) to avoid
   changing ABI offsets in structs that other libraries make visible.
   Address the issue for all platforms that overalign the two types.
   Do not bother to optimize for glibc 2.4+ on 32-bit HPPA even though
   as of 2025 it is the only maintained platform known to overalign and
   it does not need the overalignment.  */

#if (ALIGNOF_PTHREAD_COND_T <= ALIGNOF_MAX_ALIGN_T \
     && ALIGNOF_PTHREAD_MUTEX_T <= ALIGNOF_MAX_ALIGN_T)
/* The typical case.  Align PTR for TYPE *, where PTR is of type TYPE *
   and is already aligned properly.  */
# define SYSTHREAD_ALIGN_PTR(type, ptr) (ptr)
#else
/* An unusual case, e.g., GNU/Linux 32-bit HPPA with glibc 2.42.
   Aligning SYSTHREAD_ALIGN_ROOM (TYPE) * up for TYPE * results in a
   valid pointer.  TYPE's alignment must be at least that of double;
   in practice it is always greater than that of max_align_t.  */
# define SYSTHREAD_ALIGN_ROOM(type) \
    union \
    { \
      double i; \
      char room[sizeof (type) + alignof (type) - alignof (double)]; \
    }
/* Align PTR up for TYPE *.
   PTR should be of type SYSTHREAD_ALIGN_ROOM (TYPE) *.  */
# define SYSTHREAD_ALIGN_PTR(type, ptr) \
    ((type *) ((uintptr_t) ((ptr)->room + (alignof (type) - alignof (double))) \
	       & ~(alignof (type) - alignof (double))))
#endif

/* A system mutex is just a pthread mutex, possibly with alignment slop.
   It is used only for the GIL.  */
#if ALIGNOF_PTHREAD_MUTEX_T <= ALIGNOF_MAX_ALIGN_T
typedef pthread_mutex_t sys_mutex_t;
#else
typedef SYSTHREAD_ALIGN_ROOM (pthread_mutex_t) sys_mutex_t;
#endif

#if ALIGNOF_PTHREAD_COND_T <= ALIGNOF_MAX_ALIGN_T
typedef pthread_cond_t sys_cond_t;
#else
typedef SYSTHREAD_ALIGN_ROOM (pthread_cond_t) sys_cond_t;
#endif

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

NODISCARD extern sys_thread_t sys_thread_self (void);
NODISCARD extern bool sys_thread_equal (sys_thread_t, sys_thread_t);

NODISCARD extern bool sys_thread_create (sys_thread_t *,
					 thread_creation_function *, void *);

extern void sys_thread_yield (void);
extern void sys_thread_set_name (const char *);

#endif /* SYSTHREAD_H */
