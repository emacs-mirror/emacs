/* Android initialization for GNU Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

#include <allocator.h>
#include <assert.h>
#include <careadlinkat.h>
#include <errno.h>
#include <fcntl.h>
#include <fingerprint.h>
#include <intprops.h>
#include <libgen.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#include <stat-time.h>
#include <stdckdint.h>
#include <string.h>
#include <timespec.h>
#include <unistd.h>

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/select.h>

/* Old NDK versions lack MIN and MAX.  */
#include <minmax.h>

#include "android.h"
#include "androidgui.h"

#include "lisp.h"
#include "blockinput.h"
#include "coding.h"
#include "epaths.h"
#include "systime.h"

/* Whether or not Emacs is running inside the application process and
   Android windowing should be enabled.  */
bool android_init_gui;

#ifndef ANDROID_STUBIFY

#include <android/bitmap.h>
#include <android/log.h>

#include <linux/unistd.h>

#include <sys/syscall.h>

#ifdef __aarch64__
#include <arm_neon.h>
#endif /* __aarch64__ */

struct android_emacs_pixmap
{
  jclass class;
  jmethodID constructor_mutable;
};

struct android_graphics_point
{
  jclass class;
  jmethodID constructor;
};

struct android_emacs_drawable
{
  jclass class;
  jmethodID get_bitmap;
};

struct android_emacs_window
{
  jclass class;
  jmethodID swap_buffers;
  jmethodID toggle_on_screen_keyboard;
  jmethodID lookup_string;
  jmethodID set_fullscreen;
  jmethodID change_window_background;
  jmethodID reparent_to;
  jmethodID map_window;
  jmethodID unmap_window;
  jmethodID resize_window;
  jmethodID move_window;
  jmethodID move_resize_window;
  jmethodID make_input_focus;
  jmethodID raise;
  jmethodID lower;
  jmethodID reconfigure;
  jmethodID get_window_geometry;
  jmethodID translate_coordinates;
  jmethodID set_dont_accept_focus;
  jmethodID set_dont_focus_on_map;
  jmethodID define_cursor;
  jmethodID damage_rect;
  jmethodID recreate_activity;
  jmethodID clear_window;
  jmethodID clear_area;
  jmethodID set_wm_name;
};

struct android_emacs_cursor
{
  jclass class;
  jmethodID constructor;
};

struct android_key_character_map
{
  jclass class;
  jmethodID get_dead_char;
};

struct android_emacs_handle
{
  jclass class;
  jmethodID destroy_handle;
  jfieldID handle;
};

/* The API level of the current device.  */
static int android_api_level;

/* The directory used to store site-lisp.  */
char *android_site_load_path;

/* The directory used to store native libraries.  */
char *android_lib_dir;

/* The directory used to store game files.  */
char *android_game_path;

/* The directory used to store temporary files.  */
char *android_cache_dir;

/* The list of archive files within which the Java virtual macine
   looks for class files.  */
static char *android_class_path;

/* The display's pixel densities.  */
double android_pixel_density_x, android_pixel_density_y;

/* The display pixel density used to convert between point and pixel
   font sizes.  */
double android_scaled_pixel_density;

/* The display's current UI mode.  */
int android_ui_mode;

/* The Android application data directory.  */
static char *android_files_dir;

/* The Java environment being used for the main thread.  */
JNIEnv *android_java_env;

#ifdef THREADS_ENABLED

/* The Java VM new threads attach to.  */
JavaVM *android_jvm;

#endif /* THREADS_ENABLED */

/* The EmacsGC class.  */
static jclass emacs_gc_class;

/* Various fields.  */
static jfieldID emacs_gc_foreground, emacs_gc_background;
static jfieldID emacs_gc_function, emacs_gc_clip_rects;
static jfieldID emacs_gc_clip_x_origin, emacs_gc_clip_y_origin;
static jfieldID emacs_gc_stipple, emacs_gc_clip_mask;
static jfieldID emacs_gc_fill_style, emacs_gc_ts_origin_x;
static jfieldID emacs_gc_ts_origin_y, emacs_gc_line_style;
static jfieldID emacs_gc_line_width, emacs_gc_dash_offset;
static jfieldID emacs_gc_dashes;

/* The constructor and one function.  */
static jmethodID emacs_gc_constructor, emacs_gc_mark_dirty;

/* The Rect class.  */
static jclass android_rect_class;

/* Its constructor.  */
static jmethodID android_rect_constructor;

/* The EmacsService object.  */
jobject emacs_service;

/* Various methods associated with the EmacsService.  */
struct android_emacs_service service_class;

/* Various methods associated with the EmacsPixmap class.  */
static struct android_emacs_pixmap pixmap_class;

/* Various methods associated with the Point class.  */
static struct android_graphics_point point_class;

/* Various methods associated with the EmacsDrawable class.  */
static struct android_emacs_drawable drawable_class;

/* Various methods associated with the EmacsWindow class.  */
static struct android_emacs_window window_class;

/* Various methods associated with the EmacsCursor class.  */
static struct android_emacs_cursor cursor_class;

/* Various methods associated with the KeyCharacterMap class.  */
static struct android_key_character_map key_character_map_class;

/* Various methods and fields associated with the EmacsHandleObject
   class.  */
static struct android_emacs_handle handle_class;

/* The time at which Emacs was installed, which also supplies the
   mtime of asset files.  */
struct timespec emacs_installation_time;

/* The last event serial used.  This is a 32 bit value, but it is
   stored in unsigned long to be consistent with X.  */
unsigned int event_serial;

#ifdef __i386__

/* Unused pointer used to control compiler optimizations.  */
void *unused_pointer;

#endif /* __i386__ */

/* Whether or not the default signal mask has been changed.  If so,
   the signal mask must be restored before calling
   android_emacs_init.  */
static bool signal_mask_changed_p;

/* The signal mask at the time Emacs was started.  */
static sigset_t startup_signal_mask;



/* Event handling functions.  Events are stored on a (circular) queue
   that is read synchronously.  The Android port replaces pselect with
   a function android_select, which runs pselect in a separate thread,
   but more importantly also waits for events to be available on the
   android event queue.  */

struct android_event_container
{
  /* The next and last events in this queue.  */
  struct android_event_container *next, *last;

  /* The event itself.  */
  union android_event event;
};

/* Thread-specific component of the Android event queue.  */

struct android_thread_event_queue
{
  /* Mutex protecting the select data.  */
  pthread_mutex_t select_mutex;

  /* The thread used to run select.  */
  pthread_t select_thread;

  /* Arguments to pselect used by the select thread.  */
  fd_set *select_readfds;
  fd_set *select_writefds;
  fd_set *select_exceptfds;
  struct timespec *select_timeout;
  int select_nfds;

  /* Semaphores posted around invocations of pselect.  */
  sem_t start_sem;
  sem_t select_sem;

  /* Value of pselect.  */
  int select_rc;

#if __ANDROID_API__ < 21
  /* Select self-pipe.  */
  int select_pipe[2];
#else /* __ANDROID_API__ >= 21 */
  /* Whether a signal has been received to cancel pselect in this
     thread.  */
  volatile sig_atomic_t cancel_signal_received;
#endif /* __ANDROID_API__ >= 21 */

  /* Whether this thread must exit.  */
  int canceled;
};

#if __ANDROID_API__ >= 21
#define SELECT_SIGNAL SIGUSR1
#endif /* __ANDROID_API__ >= 21 */

struct android_event_queue
{
  /* Mutex protecting the event queue.  */
  pthread_mutex_t mutex;

  /* Condition variables for the reading side.  */
  pthread_cond_t read_var;

  /* Circular queue of events.  */
  struct android_event_container events;

#ifndef THREADS_ENABLED
  /* If threads are disabled, the thread-specific component of the main
     and only thread.  */
  struct android_thread_event_queue thread;
#endif /* !THREADS_ENABLED */

  /* The number of events in the queue.  */
  int num_events;
};

/* The global event queue.  */
static struct android_event_queue event_queue;

/* Main select loop of select threads.  */
static void *android_run_select_thread (void *);

/* Initialize a thread-local component of the Android event queue
   THREAD.  Create and initialize a thread whose purpose is to execute
   `select' in an interruptible manner, and initialize variables or file
   descriptors with which to communicate with it.  */

static void
android_init_thread_events (struct android_thread_event_queue *thread)
{
  thread->canceled = false;
  thread->select_readfds = NULL;
  thread->select_writefds = NULL;
  thread->select_exceptfds = NULL;
  thread->select_timeout = NULL;
  thread->select_nfds = 0;

  if (pthread_mutex_init (&thread->select_mutex, NULL))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "pthread_mutex_init: %s",
			   strerror (errno));
      emacs_abort ();
    }

  sem_init (&thread->select_sem, 0, 0);
  sem_init (&thread->start_sem, 0, 0);

#if __ANDROID_API__ < 21
  /* Set up the file descriptor used to wake up pselect.  */
  if (pipe2 (thread->select_pipe, O_CLOEXEC) < 0)
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "pipe2: %s", strerror (errno));
      emacs_abort ();
    }

  /* Make sure the read end will fit in fd_set.  */
  if (thread->select_pipe[0] >= FD_SETSIZE)
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "read end of select pipe"
			   " exceeds FD_SETSIZE!");
      emacs_abort ();
    }
#endif /* __ANDROID_API__ < 21 */

  /* Start the select thread.  */
  if (pthread_create (&thread->select_thread, NULL,
		      android_run_select_thread, thread))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "pthread_create: %s",
			   strerror (errno));
      emacs_abort ();
    }

  /* Wait for the thread to be initialized.  */
  while (sem_wait (&thread->select_sem) < 0)
    ;;
}

#ifdef THREADS_ENABLED

/* Destroy a thread-local component of the Android event queue provided
   as DATA, and release DATA's storage itself.  Must be invoked at a
   time when the select thread is idle, i.e., awaiting
   DATA->start_sem.  */

static void
android_finalize_thread_events (void *data)
{
  int rc;
  struct android_thread_event_queue *thread;

  /* Cancel the thread and pause till it exits.  */
  thread = data;
  thread->canceled = 1;
  sem_post (&thread->start_sem);
  rc = pthread_join (thread->select_thread, NULL);
  if (rc)
    emacs_abort ();

  /* Release the select thread, semaphores, etc.  */
  pthread_mutex_destroy (&thread->select_mutex);
  sem_close (&thread->select_sem);
  sem_close (&thread->start_sem);
#if __ANDROID_API__ < 21
  close (thread->select_pipe[0]);
  close (thread->select_pipe[1]);
#endif /* __ANDROID_API__ < 21 */
  xfree (thread);
}

/* TLS keys associating polling threads with Emacs threads.  */
static pthread_key_t poll_thread, poll_thread_internal;

#endif /* THREADS_ENABLED */

/* Return the thread-specific component of the event queue appertaining
   to this thread, or create it as well as a polling thread if
   absent.  */

static struct android_thread_event_queue *
android_get_poll_thread (void)
{
#ifndef THREADS_ENABLED
  return &event_queue.thread;
#else /* THREADS_ENABLED */
  struct android_thread_event_queue *queue;

  queue = pthread_getspecific (poll_thread);
  if (!queue)
    {
      queue = xmalloc (sizeof *queue);
      android_init_thread_events (queue);
      pthread_setspecific (poll_thread, queue);
    }
  return queue;
#endif /* !THREADS_ENABLED */
}

/* Set the task name of the current task to NAME, a string at most 21
   characters in length.

   This name is displayed as that of the task (LWP)'s pthread in
   GDB.  */

static void
android_set_task_name (const char *name)
{
  char proc_name[INT_STRLEN_BOUND (long)
		 + sizeof "/proc/self/task//comm"];
  int fd;
  pid_t lwp;
  size_t length;

  lwp = gettid ();
  sprintf (proc_name, "/proc/self/task/%ld/comm", (long) lwp);
  fd = open (proc_name, O_WRONLY | O_TRUNC);

  if (fd < 0)
    goto failure;

  length = strlen (name);

  if (write (fd, name, MIN (16, length)) < 0)
    goto failure;

  close (fd);
  return;

 failure:
  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Failed to set task name for LWP %ld: %s",
		       (long) lwp, strerror (errno));

  /* Close the file descriptor if it is already set.  */
  if (fd >= 0)
    close (fd);
}

static void *
android_run_select_thread (void *thread_data)
{
  /* Apparently this is required too.  */
  JNI_STACK_ALIGNMENT_PROLOGUE;

  int rc;
  struct android_thread_event_queue *data;
#if __ANDROID_API__ < 21
  int nfds;
  fd_set readfds;
  char byte;
#else /* __ANDROID_API__ >= 21 */
  sigset_t signals, waitset;
  int sig;
#endif /* __ANDROID_API__ >= 21 */

  /* Set the name of this thread's LWP for debugging purposes.  */
  android_set_task_name ("Emacs polling thread");
  data = thread_data;

#if __ANDROID_API__ < 21
  /* A completely different implementation is used when building for
     Android versions earlier than 21, because pselect with a signal
     mask does not work properly: the signal mask is truncated on APIs
     <= 16, and elsewhere, the signal mask is applied in userspace
     before issuing a select system call, between which SELECT_SIGNAL
     may arrive.  Instead of blocking SELECT_SIGNAL and unblocking it
     inside pselect, a file descriptor is selected.  Data is written to
     the file descriptor whenever select is supposed to return.  */

  /* Release the user after initialization.  */
  sem_post (&data->select_sem);

  while (true)
    {
      /* Wait for the thread to be released.  */
      while (sem_wait (&data->start_sem) < 0)
	;;
      if (data->canceled)
	return NULL;

      /* Get the select lock and call pselect.  API 8 does not have
	 working pselect in any sense.  Instead, pselect wakes up on
	 select_pipe[0].  */

      pthread_mutex_lock (&data->select_mutex);
      nfds = data->select_nfds;

      if (data->select_readfds)
	readfds = *data->select_readfds;
      else
	FD_ZERO (&readfds);

      if (nfds < data->select_pipe[0] + 1)
	nfds = data->select_pipe[0] + 1;
      FD_SET (data->select_pipe[0], &readfds);

      rc = pselect (nfds, &readfds,
		    data->select_writefds,
		    data->select_exceptfds,
		    data->select_timeout,
		    NULL);

      /* Subtract 1 from rc if readfds contains the select pipe, and
	 also remove it from that set.  */

      if (rc != -1 && FD_ISSET (data->select_pipe[0], &readfds))
	{
	  rc -= 1;
	  FD_CLR (data->select_pipe[0], &readfds);

	  /* If no file descriptors aside from the select pipe are
	     ready, then pretend that an error has occurred.  */
	  if (!rc)
	    rc = -1;
	}

      /* Save the read file descriptor set back again.  */

      if (data->select_readfds)
	*data->select_readfds = readfds;

      data->select_rc = rc;
      pthread_mutex_unlock (&data->select_mutex);

      /* Signal the main thread that there is now data to read.  Hold
         the event queue lock during this process to make sure this
         does not happen before the main thread begins to wait for the
         condition variable.  */

      pthread_mutex_lock (&event_queue.mutex);
      pthread_cond_broadcast (&event_queue.read_var);
      pthread_mutex_unlock (&event_queue.mutex);

      /* Read a single byte from the select pipe.  */
      read (data->select_pipe[0], &byte, 1);

      /* Signal the Emacs thread that pselect is done.  If read_var
	 was signaled by android_write_event, event_queue.mutex could
	 still be locked, so this must come before.  */
      sem_post (&data->select_sem);
    }
#else /* __ANDROID_API__ >= 21 */
  sigfillset (&signals);
  if (pthread_sigmask (SIG_BLOCK, &signals, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_sigmask: %s",
			 strerror (errno));

  sigdelset (&signals, SELECT_SIGNAL);
  sigemptyset (&waitset);
  sigaddset (&waitset, SELECT_SIGNAL);
#ifdef THREADS_ENABLED
  pthread_setspecific (poll_thread_internal, thread_data);
#endif /* THREADS_ENABLED */
  /* Release the user after initialization.  */
  sem_post (&data->select_sem);

  while (true)
    {
      /* Wait for the thread to be released.  */
      while (sem_wait (&data->start_sem) < 0)
	;;
      if (data->canceled)
	return NULL;

      /* Get the select lock and call pselect.  */
      data->cancel_signal_received = 0;
      pthread_mutex_lock (&data->select_mutex);
      rc = pselect (data->select_nfds,
		    data->select_readfds,
		    data->select_writefds,
		    data->select_exceptfds,
		    data->select_timeout,
		    &signals);
      data->select_rc = rc;
      pthread_mutex_unlock (&data->select_mutex);

      /* Signal the main thread that there is now data to read.  Hold
         the event queue lock during this process to make sure this
         does not happen before the main thread begins to wait for the
         condition variable.  */

      pthread_mutex_lock (&event_queue.mutex);
      pthread_cond_broadcast (&event_queue.read_var);
      pthread_mutex_unlock (&event_queue.mutex);

      /* Test a separate flag `data->cancel_signal_received' rather than
	 rc and errno.

         This is because `pselect' does not return an rc of -1 upon
         being interrupted in some versions of Android, but does set
         signal masks correctly.  */
      if (!data->cancel_signal_received)
	/* Now, wait for SELECT_SIGNAL, unless pselect was interrupted
	   and the signal has already been delivered.  The Emacs thread
	   will always send this signal after read_var is triggered or
	   the UI thread has sent an event.  */
	sigwait (&waitset, &sig);

      /* Signal the Emacs thread that pselect is done.  If read_var
	 was signaled by android_write_event, event_queue.mutex could
	 still be locked, so this must come before.  */
      sem_post (&data->select_sem);
    }
#endif /* __ANDROID_API__ >= 21 */

  return NULL;
}

#if __ANDROID_API__ >= 21

static void
android_handle_poll_signal (int sig, siginfo_t *siginfo, void *arg)
{
  struct android_thread_event_queue *queue;

  /* Although pthread_getspecific is not AS-safe, its implementation has
     been verified to be safe to invoke from a single handler called
     within pselect in a controlled manner, and this is the only means
     of retrieving thread-specific data from a signal handler, as the
     POSIX real-time signal system calls are unavailable to Android
     applications.  */
#ifdef THREADS_ENABLED
  queue = pthread_getspecific (poll_thread_internal);
#else /* !THREADS_ENABLED */
  queue = &event_queue.thread;
#endif /* !THREADS_ENABLED */
  queue->cancel_signal_received = 1;
}

#endif /* __ANDROID_API__ >= 21 */

/* Semaphore used to indicate completion of a query.
   This should ideally be defined further down.  */
static sem_t android_query_sem;

/* ID of the Emacs thread.  */
static pthread_t main_thread_id;

/* Set up the global event queue by initializing the mutex and two
   condition variables, and the linked list of events.  This must be
   called before starting the Emacs thread.  Also, initialize the
   thread used to run pselect.

   These functions must also use the C library malloc and free,
   because xmalloc is not thread safe.  */

static void
android_init_events (void)
{
#if __ANDROID_API__ >= 21
  struct sigaction sa;
#endif /* __ANDROID_API__ >= 21 */

  if (pthread_mutex_init (&event_queue.mutex, NULL))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "pthread_mutex_init: %s",
			   strerror (errno));
      emacs_abort ();
    }

  if (pthread_cond_init (&event_queue.read_var, NULL))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "pthread_cond_init: %s",
			   strerror (errno));
      emacs_abort ();
    }

  event_queue.events.next = &event_queue.events;
  event_queue.events.last = &event_queue.events;

  main_thread_id = pthread_self ();

#if __ANDROID_API__ >= 21
  /* Before any event threads are initialized, guarantee that the
     disposition of SELECT_SIGNAL is correct.  */
  sigfillset (&sa.sa_mask);
  sa.sa_sigaction = android_handle_poll_signal;
  sa.sa_flags = SA_SIGINFO;
  if (sigaction (SELECT_SIGNAL, &sa, NULL))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "sigaction: %s",
			   strerror (errno));
      emacs_abort ();
    }
#endif /* __ANDROID_API__ >= 21 */
#ifndef THREADS_ENABLED
  android_init_thread_events (&event_queue.thread);
#else /* THREADS_ENABLED */
  if (pthread_key_create (&poll_thread, android_finalize_thread_events)
      || pthread_key_create (&poll_thread_internal, NULL))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "pthread_key_create: %s",
			   strerror (errno));
      emacs_abort ();
    }
#endif /* THREADS_ENABLED */
}

int
android_pending (void)
{
  int i;

  pthread_mutex_lock (&event_queue.mutex);
  i = event_queue.num_events;
  pthread_mutex_unlock (&event_queue.mutex);

  return i;
}

/* Wait for events to become available synchronously.  Return once an
   event arrives.  Also, reply to the UI thread whenever it requires a
   response.  */

void
android_wait_event (void)
{
  /* Run queries from the UI thread to the Emacs thread.  */
  android_check_query ();

  pthread_mutex_lock (&event_queue.mutex);

  /* Wait for events to appear if there are none available to
     read.  */
  if (!event_queue.num_events)
    pthread_cond_wait (&event_queue.read_var,
		       &event_queue.mutex);

  pthread_mutex_unlock (&event_queue.mutex);

  /* Check for queries again.  If a query is sent after the call to
     `android_check_query' above, `read_var' will be signaled.  */
  android_check_query ();
}

void
android_next_event (union android_event *event_return)
{
  struct android_event_container *container;

  pthread_mutex_lock (&event_queue.mutex);

  /* Wait for events to appear if there are none available to
     read.  */
  if (!event_queue.num_events)
    pthread_cond_wait (&event_queue.read_var,
		       &event_queue.mutex);

  /* Obtain the event from the end of the queue.  */
  container = event_queue.events.last;
  eassert (container != &event_queue.events);

  /* Remove the event from the queue and copy it to the caller
     supplied buffer.  */
  container->last->next = container->next;
  container->next->last = container->last;
  *event_return = container->event;
  event_queue.num_events--;

  /* Free the container.  */
  free (container);

  /* Unlock the queue.  */
  pthread_mutex_unlock (&event_queue.mutex);
}

bool
android_check_if_event (union android_event *event_return,
			bool (*predicate) (union android_event *,
					   void *),
			void *arg)
{
  struct android_event_container *container;

  pthread_mutex_lock (&event_queue.mutex);

  /* Loop over each event.  */
  container = event_queue.events.last;
  for (; container != &event_queue.events; container = container->last)
    {
      /* See if the predicate matches.  */
      if ((*predicate) (&container->event, arg))
	{
	  /* Copy out the event and return true.  */
	  *event_return = container->event;
	  --event_queue.num_events;

	  /* Unlink container.  */
	  container->last->next = container->next;
	  container->next->last = container->last;
	  free (container);
	  pthread_mutex_unlock (&event_queue.mutex);
	  return true;
	}
    }

  pthread_mutex_unlock (&event_queue.mutex);
  return false;
}

void
android_write_event (union android_event *event)
{
  struct android_event_container *container;

  container = malloc (sizeof *container);

  if (!container)
    return;

  /* If the event queue hasn't been initialized yet, return false.  */
  if (!event_queue.events.next)
    return;

  pthread_mutex_lock (&event_queue.mutex);
  container->next = event_queue.events.next;
  container->last = &event_queue.events;
  container->next->last = container;
  container->last->next = container;
  container->event = *event;
  event_queue.num_events++;
  pthread_cond_broadcast (&event_queue.read_var);
  pthread_mutex_unlock (&event_queue.mutex);

  /* Now set pending_signals to true, and raise SIGIO to interrupt any
     ongoing reads if the event is important.  */
  pending_signals = true;

  switch (event->type)
    {
      /* Key press and window action events are considered important,
	 as they either end up quitting or asking for responses to the
	 IME.  */
    case ANDROID_KEY_PRESS:
    case ANDROID_WINDOW_ACTION:
      kill (getpid (), SIGIO);
      break;

    default:
      break;
    }
}



/* Whether or not the UI thread has been waiting for a significant
   amount of time for a function to run in the main thread, and Emacs
   should answer the query ASAP.  */
static bool android_urgent_query;

int
android_select (int nfds, fd_set *readfds, fd_set *writefds,
		fd_set *exceptfds, struct timespec *timeout)
{
  int nfds_return, nevents;
#if __ANDROID_API__ < 21
  static char byte;
#endif
  struct android_thread_event_queue *data;

  /* When threads are enabled, the following is executed before the
     global lock is released.  */
#ifndef THREADS_ENABLED
  android_before_select ();
#endif /* !THREADS_ENABLED */

  pthread_mutex_lock (&event_queue.mutex);

  if (event_queue.num_events)
    {
      /* Zero READFDS, WRITEFDS and EXCEPTFDS, lest the caller
	 mistakenly interpret this return value as indicating that an
	 inotify file descriptor is readable, and try to poll an
	 unready one.  */

      if (readfds)
	FD_ZERO (readfds);

      if (writefds)
	FD_ZERO (writefds);

      if (exceptfds)
	FD_ZERO (exceptfds);
      pthread_mutex_unlock (&event_queue.mutex);
      return 1;
    }

  nfds_return = 0;

  data = android_get_poll_thread ();

  pthread_mutex_lock (&data->select_mutex);
  data->select_nfds = nfds;
  data->select_readfds = readfds;
  data->select_writefds = writefds;
  data->select_exceptfds = exceptfds;
  data->select_timeout = timeout;
  pthread_mutex_unlock (&data->select_mutex);

  /* Release the select thread.  */
  sem_post (&data->start_sem);

  /* Start waiting for the event queue condition to be set.  */
  pthread_cond_wait (&event_queue.read_var, &event_queue.mutex);

#if __ANDROID_API__ >= 21
  /* Interrupt the select thread now, in case it's still in
     pselect.  */
  pthread_kill (data->select_thread, SELECT_SIGNAL);
#else /* __ANDROID_API__ < 21 */
  /* Interrupt the select thread by writing to the select pipe.  */
  if (write (data->select_pipe[1], &byte, 1) != 1)
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "write: %s", strerror (errno));
#endif /* __ANDROID_API__ < 21 */

  /* Are there any events in the event queue?  */
  nevents = event_queue.num_events;
  pthread_mutex_unlock (&event_queue.mutex);

  /* Wait for pselect to return in any case.  This must be done with the
     event queue mutex unlocked.  Otherwise, the pselect thread can hang
     if it tries to lock the event queue mutex to signal read_var after
     the UI thread has already done so.  */
  while (sem_wait (&data->select_sem) < 0)
    ;;

  /* If there are now events in the queue, return 1.  */
  if (nevents)
    nfds_return = 1;

  /* Add the return value of pselect if it has also discovered ready
     file descriptors.  */

  if (data->select_rc >= 0)
    nfds_return += data->select_rc;
  else if (!nfds_return)
    /* If pselect was interrupted and nfds_return is 0 (meaning that no
       events have been read), indicate that an error has taken
       place.  */
    nfds_return = data->select_rc;

  if ((data->select_rc < 0) && nfds_return >= 0)
    {
      /* Clear the file descriptor sets if events will be delivered
	 but no file descriptors have become ready to prevent the
	 caller from misinterpreting a non-zero return value.  */

      if (readfds)
	FD_ZERO (readfds);

      if (writefds)
	FD_ZERO (writefds);

      if (exceptfds)
	FD_ZERO (exceptfds);
    }

  /* This is to shut up process.c when pselect gets EINTR.  */
  if (nfds_return < 0)
    errno = EINTR;

#ifndef THREADS_ENABLED
  /* Now check for and run anything the UI thread wants to run in the
     main thread.  */
  android_check_query ();
#endif /* THREADS_ENABLED */

  return nfds_return;
}



static void *
android_run_debug_thread (void *data)
{
  FILE *file;
  int fd;
  char *line;
  size_t n;

  /* Set the name of this thread's LWP for debugging purposes.  */
  android_set_task_name ("`android_debug'");

  fd = (int) (intptr_t) data;
  file = fdopen (fd, "r");

  if (!file)
    return NULL;

  line = NULL;

  while (true)
    {
      if (getline (&line, &n, file) < 0)
	{
	  free (line);
	  break;
	}

      __android_log_print (ANDROID_LOG_INFO, __func__, "%s", line);
    }

  fclose (file);
  return NULL;
}



/* Intercept USER_FULL_NAME and return something that makes sense if
   pw->pw_gecos is NULL.  */

char *
android_user_full_name (struct passwd *pw)
{
#ifdef HAVE_STRUCT_PASSWD_PW_GECOS
  if (!pw->pw_gecos)
    return (char *) "Android user";

  return pw->pw_gecos;
#else /* !HAVE_STRUCT_PASSWD_PW_GECOS */
  return "Android user";
#endif /* HAVE_STRUCT_PASSWD_PW_GECOS */
}



/* Return whether or not the specified file NAME designates a file in
   the directory DIR, which should be an absolute file name.  NAME
   must be in canonical form.  */

bool
android_is_special_directory (const char *name, const char *dir)
{
  size_t len;

  /* Compare up to strlen (DIR) bytes of NAME with DIR.  */

  len = strlen (dir);
  if (strncmp (name, dir, len))
    return false;

  /* Now see if the character of NAME after len is either a directory
     separator or a terminating NULL.  */

  name += len;
  switch (*name)
    {
    case '\0': /* NAME is an exact match for DIR.  */
    case '/':  /* NAME is a constituent of DIR.  */
      return true;
    }

  /* The file name doesn't match.  */
  return false;
}

#if 0

/* URL-encode N bytes of the specified STRING into at most N bytes of
   BUFFER; STRING is assumed to be encoded in a `utf-8-emacs'
   compatible coding system.  Value is the number of bytes encoded
   (excluding the trailing null byte placed at the end of the encoded
   text) or -1 upon failure.  */

static ssize_t
android_url_encode (const char *restrict string, size_t length,
		    char *restrict buffer, size_t n)
{
  int len, character;
  size_t num_encoded;
  char *end;
  char format[1 + 25];

  /* For each multibyte character... */

  end = string + length;
  num_encoded = 0;

  while (string < end)
    {
      /* XXX: Android documentation claims that a URI is to be encoded
	 according to the ``Unicode'' scheme, but what this means in
	 reality is that the URI is encoded in UTF-8, and then each of
	 the resulting bytes is separately URI-encoded.  */
      /* Find the length of the multibyte character at STRING.  */
      len = /* multibyte_length (string, end, true, true) */ 1;

      /* 0 means that STRING is not a valid multibyte string.  */
      if (!len || string + len > end)
	goto failure;

      /* Now fetch the character and increment string.  */
      /* character = /\* STRING_CHAR ((unsigned char *) string) *\/; */
      character = *(unsigned char *) string;
      string += len;

      /* If CHARACTER is not a letter or an unreserved character,
	 escape it.  */

      if (!((character >= 'A'
	     && character <= 'Z')
	    || (character >= 'a'
		&& character <= 'z')
	    || (character >= '0'
		&& character <= '9')
	    || character == '_'
	    || character == '-'
	    || character == '!'
	    || character == '.'
	    || character == '~'
	    || character == '\''
	    || character == '('
	    || character == ')'
	    || character == '*'))
	{
	  len = sprintf (format, "%%%X", (unsigned int) character);
	  if (len < 0)
	    goto failure;

	  /* See if there is enough space left to hold the encoded
	     string.  */

	  if (n < len)
	    goto failure;

	  n -= len;
	  num_encoded += len;

	  /* Copy the encoded string to STRING.  */
	  memcpy (buffer, format, n);
	  buffer += len;
	}
      else
	{
	  /* No more space within BUFFER.  */
	  if (!n)
	    goto failure;

	  /* Don't encode this ASCII character; just store it.  */
	  n--, num_encoded++;
	  *(buffer++) = character;
	}
    }

  /* If there's no space for a trailing null byte or more bytes have
     been encoded than representable in ssize_t, fail.  */

  if (!n || num_encoded > SSIZE_MAX)
    goto failure;

  /* Store the terminating NULL byte.  */
  *buffer = '\0';
  return num_encoded;

 failure:
  return -1;
}

/* Return the content URI corresponding to a `/content' file name,
   or NULL if it is not a content URI.

   This function is not reentrant.  */

static const char *
android_get_content_name (const char *filename)
{
  static char buffer[PATH_MAX + 1];
  char *head, *token, *next, *saveptr, *copy, *mark, *mark1;
  ssize_t rc;
  size_t n, length;

  /* Find the file name described if it starts with `/content'.  If
     just the directory is described, return content://.  */

  filename = android_is_special_directory (filename, "/content");

  if (!filename)
    return NULL;

  if (!*filename)
    return "content://";

  /* Now copy FILENAME into a buffer and convert it into a content
     URI.  */

  copy = xstrdup (filename);
  mark = saveptr = NULL;
  head = stpcpy (buffer, "content:/");

  /* Split FILENAME by slashes.  */

  token = strtok_r (copy, "/", &saveptr);

  while (token)
    {
      /* Compute the number of bytes remaining in buffer excluding a
	 trailing null byte.  */
      n = PATH_MAX - (head - buffer);

      /* Write / to the buffer.  Return failure if there is no space
	 for it.  */

      if (!n)
	goto failure;

      *head++ = '/';
      n--;

      /* Find the next token now.  */
      next = strtok_r (NULL, "/", &saveptr);

      /* Detect and avoid encoding an encoded URL query affixed to the
	 end of the last component within the content file name.

         Content URIs can include a query describing parameters that
         must be provided to the content provider.  They are separated
         from the rest of the URI by a single question mark character,
         which should not be encoded.

         However, the distinction between the separator and question
         marks that appear inside file name components is lost when a
         content URI is decoded into a content path.  To compensate
         for this loss of information, Emacs assumes that the last
         question mark is always a URI separator, and suffixes content
         file names which contain question marks with a trailing
         question mark.  */

      if (!next)
	{
	  /* Find the last question mark character.  */

	  mark1 = strchr (token, '?');

	  while (mark1)
	    {
	      mark = mark1;
	      mark1 = strchr (mark + 1, '?');
	    }
	}

      if (mark)
	{
	  /* First, encode the part leading to the question mark
	     character.  */

	  rc = 0;
	  if (mark > token)
	    rc = android_url_encode (token, mark - token,
				     head, n + 1);

	  /* If this fails, bail out.  */

	  if (rc < 0)
	    goto failure;

	  /* Copy mark to the file name.  */

	  n -= rc, head += rc;
	  length = strlen (mark);

	  if (n < length)
	    goto failure;

	  strcpy (head, mark);

	  /* Now break out of the loop, since this is the last
	     component anyway.  */
	  break;
	}
      else
	/* Now encode this file name component into the buffer.  */
	rc = android_url_encode (token, strlen (token),
				 head, n + 1);

      if (rc < 0)
	goto failure;

      head += rc;
      token = next;
    }

  /* buffer must have been null terminated by
     `android_url_encode'.  */
  xfree (copy);
  return buffer;

 failure:
  xfree (copy);
  return NULL;
}

#endif /* 0 */

/* Return the current user's ``home'' directory, which is actually the
   app data directory on Android.  */

const char *
android_get_home_directory (void)
{
  return android_files_dir;
}

/* Return the name of the file behind a file descriptor FD by reading
   /proc/self/fd/.  Value is allocated memory holding the file name
   upon success, and 0 upon failure.  */

static char *
android_proc_name (int fd)
{
  char format[sizeof "/proc/self/fd/"
	      + INT_STRLEN_BOUND (int)];
  static struct allocator allocator = {
    /* Fill the allocator with C library malloc functions.  xmalloc
       and so aren't thread safe.  */
    malloc, realloc, free, NULL,
  };

  sprintf (format, "/proc/self/fd/%d", fd);
  return careadlinkat (AT_FDCWD, format, NULL, 0,
		       &allocator, readlinkat);
}

/* Try to guarantee the existence of the `lib' directory within the
   parent directory of the application files directory.

   If `/data/data/org.gnu.emacs/lib' (or
   `/data/user/N/org.gnu.emacs/lib') does not exist or is a dangling
   symbolic link, create a symlink from it to the library
   directory.

   Newer versions of Android don't create this link by default, making
   it difficult to locate the directory containing Emacs library
   files, particularly from scripts in other programs sharing the same
   user ID as Emacs that don't have access to `exec-path'.  */

static void
android_create_lib_link (void)
{
  char *filename;
  char lib_directory[PATH_MAX];
  int fd;

  /* Find the directory containing the files directory.  */
  filename = dirname (android_files_dir);
  if (!filename)
    goto failure;

  /* Now make `lib_directory' the name of the library directory
     within.  */
  snprintf (lib_directory, PATH_MAX, "%s/lib", filename);

  /* Try to open this directory.  */
  fd = open (lib_directory, O_DIRECTORY);

  /* If the directory can be opened normally, close it and return
     now.  */
  if (fd >= 0)
    goto success;

  /* Try to unlink the directory in case it's a dangling symbolic
     link.  */
  unlink (lib_directory);

  /* Otherwise, try to symlink lib_directory to the actual library
     directory.  */

  if (symlink (android_lib_dir, lib_directory))
    /* Print a warning message if creating the link fails.  */
    __android_log_print (ANDROID_LOG_WARN, __func__,
			 "Failed to create symbolic link from"
			 " application library directory `%s'"
			 " to its actual location at `%s'",
			 lib_directory, android_files_dir);

 success:
  close (fd);
 failure:
  return;
}



/* JNI functions called by Java.  */

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-prototypes"
#endif

JNIEXPORT jstring JNICALL
NATIVE_NAME (getFingerprint) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  char buffer[sizeof fingerprint * 2 + 1];

  memset (buffer, 0, sizeof buffer);
  hexbuf_digest (buffer, (char *) fingerprint,
		 sizeof fingerprint);

  return (*env)->NewStringUTF (env, buffer);
}

JNIEXPORT void JNICALL
NATIVE_NAME (setEmacsParams) (JNIEnv *env, jobject object,
			      jobject local_asset_manager,
			      jobject files_dir, jobject libs_dir,
			      jobject cache_dir,
			      jfloat pixel_density_x,
			      jfloat pixel_density_y,
			      jfloat scaled_density,
			      jint ui_mode,
			      jobject class_path,
			      jobject emacs_service_object,
			      jint api_level)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  int pipefd[2];
  pthread_t thread;
  const char *java_string;
  struct stat statb;

#ifdef THREADS_ENABLED
  /* Save the Java VM.  */
  if ((*env)->GetJavaVM (env, &android_jvm))
    emacs_abort ();
#endif /* THREADS_ENABLED */

  /* Set the Android API level early, as it is used by
     `android_vfs_init'.  */
  android_api_level = api_level;

  /* This function should only be called from the main thread.  */
  android_pixel_density_x = pixel_density_x;
  android_pixel_density_y = pixel_density_y;
  android_scaled_pixel_density = scaled_density;
  android_ui_mode = ui_mode;

  __android_log_print (ANDROID_LOG_INFO, __func__,
		       "Initializing "PACKAGE_STRING"...\nPlease report bugs to "
		       PACKAGE_BUGREPORT".  Thanks.\n");

  if (emacs_service_object)
    {
      /* Create a pipe and duplicate it to stdout and stderr.  Next,
	 make a thread that prints stderr to the system log.

         Notice that this function is called in one of two ways.  The
         first is when Emacs is being started as a GUI application by
         the system, and the second is when Emacs is being started by
         libandroid-emacs.so as an ordinary noninteractive Emacs.

         In the second case, stderr is usually connected to a PTY, so
         this is unnecessary.  */

      if (pipe2 (pipefd, O_CLOEXEC) < 0)
	emacs_abort ();

      if (dup2 (pipefd[1], 2) < 0)
	emacs_abort ();
      close (pipefd[1]);

      if (pthread_create (&thread, NULL, android_run_debug_thread,
			  (void *) (intptr_t) pipefd[0]))
	emacs_abort ();
    }

  /* Now set the path to the site load directory.  */

  java_string = (*env)->GetStringUTFChars (env, (jstring) files_dir,
					   NULL);

  if (!java_string)
    emacs_abort ();

  android_files_dir = strdup ((const char *) java_string);

  if (!android_files_dir)
    emacs_abort ();

  (*env)->ReleaseStringUTFChars (env, (jstring) files_dir,
				 java_string);

  java_string = (*env)->GetStringUTFChars (env, (jstring) libs_dir,
					   NULL);

  if (!java_string)
    emacs_abort ();

  android_lib_dir = strdup ((const char *) java_string);

  if (!android_files_dir)
    emacs_abort ();

  (*env)->ReleaseStringUTFChars (env, (jstring) libs_dir,
				 java_string);

  java_string = (*env)->GetStringUTFChars (env, (jstring) cache_dir,
					   NULL);

  if (!java_string)
    emacs_abort ();

  android_cache_dir = strdup ((const char *) java_string);

  if (!android_files_dir)
    emacs_abort ();

  (*env)->ReleaseStringUTFChars (env, (jstring) cache_dir,
				 java_string);

  if (class_path)
    {
      java_string = (*env)->GetStringUTFChars (env, (jstring) class_path,
					       NULL);

      if (!java_string)
	emacs_abort ();

      android_class_path = strdup ((const char *) java_string);

      if (!android_class_path)
	emacs_abort ();

      (*env)->ReleaseStringUTFChars (env, (jstring) class_path,
				     java_string);
    }

  /* Derive the installation date from the modification time of the
     file constitituing the class path.  */

  emacs_installation_time = invalid_timespec ();

  if (class_path)
    {
      if (!stat (android_class_path, &statb))
	emacs_installation_time = get_stat_mtime (&statb);
    }

  /* Calculate the site-lisp path.  */

  android_site_load_path = malloc (PATH_MAX + 1);

  if (!android_site_load_path)
    emacs_abort ();

  android_game_path = malloc (PATH_MAX + 1);

  if (!android_game_path)
    emacs_abort ();

  snprintf (android_site_load_path, PATH_MAX, "%s/site-lisp",
	    android_files_dir);
  snprintf (android_game_path, PATH_MAX, "%s/scores", android_files_dir);

  __android_log_print (ANDROID_LOG_INFO, __func__,
		       "Site-lisp directory: %s\n"
		       "Files directory: %s\n"
		       "Native code directory: %s\n"
		       "Game score path: %s\n"
		       "Class path: %s\n",
		       android_site_load_path,
		       android_files_dir,
		       android_lib_dir, android_game_path,
		       (android_class_path
			? android_class_path
			: "None"));

  if (android_class_path)
    /* Set EMACS_CLASS_PATH to the class path where
       EmacsNoninteractive can be found.  */
    setenv ("EMACS_CLASS_PATH", android_class_path, 1);

  /* If the system is Android 5.0 or later, set LANG to en_US.utf8,
     which is understood by the C library.  In other instances set it
     to C, a meaningless value, for good measure.  */

  if (emacs_service_object)
    {
      if (api_level >= 21)
	setenv ("LANG", "en_US.utf8", 1);
      else
	setenv ("LANG", "C", 1);
    }

  /* Make a reference to the Emacs service.  */

  if (emacs_service_object)
    {
      emacs_service = (*env)->NewGlobalRef (env, emacs_service_object);

      if (!emacs_service)
	emacs_abort ();

      /* If the service is set this Emacs is being initialized as part
	 of the Emacs application itself.

         Try to create a symlink from where scripts expect Emacs to
         place its library files to the directory that actually holds
         them; earlier versions of Android used to do this
         automatically, but that feature has been removed.  */

      android_create_lib_link ();
    }

  /* Set up events.  */
  android_init_events ();

  /* Set up the Android virtual filesystem layer.  */
  android_vfs_init (env, local_asset_manager);

  /* OK, setup is now complete.  The caller may call initEmacs
     now.  */
}

JNIEXPORT jobject JNICALL
NATIVE_NAME (getProcName) (JNIEnv *env, jobject object, jint fd)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  char *buffer;
  size_t length;
  jbyteArray array;

  buffer = android_proc_name (fd);
  if (!buffer)
    return NULL;

  /* Return a byte array, as Java strings cannot always encode file
     names.  */
  length = strlen (buffer);
  array = (*env)->NewByteArray (env, length);
  if (!array)
    goto finish;

  (*env)->SetByteArrayRegion (env, array, 0, length,
			      (jbyte *) buffer);

 finish:
  free (buffer);
  return array;
}

/* Initialize service_class, aborting if something goes wrong.  */

static void
android_init_emacs_service (void)
{
  jclass old;

  service_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsService");
  eassert (service_class.class);

  old = service_class.class;
  service_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!service_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  service_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					service_class.class,	\
					name, signature);	\
  eassert (service_class.c_name);

  FIND_METHOD (fill_rectangle, "fillRectangle",
	       "(Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsGC;IIII)V");
  FIND_METHOD (fill_polygon, "fillPolygon",
	       "(Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsGC;"
	       "[Landroid/graphics/Point;)V");
  FIND_METHOD (draw_rectangle, "drawRectangle",
	       "(Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsGC;IIII)V");
  FIND_METHOD (draw_line, "drawLine",
	       "(Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsGC;IIII)V");
  FIND_METHOD (draw_point, "drawPoint",
	       "(Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsGC;II)V");
  FIND_METHOD (ring_bell, "ringBell", "(I)V");
  FIND_METHOD (query_tree, "queryTree",
	       "(Lorg/gnu/emacs/EmacsWindow;)[J");
  FIND_METHOD (get_screen_width, "getScreenWidth", "(Z)I");
  FIND_METHOD (get_screen_height, "getScreenHeight", "(Z)I");
  FIND_METHOD (detect_mouse, "detectMouse", "()Z");
  FIND_METHOD (detect_keyboard, "detectKeyboard", "()Z");
  FIND_METHOD (name_keysym, "nameKeysym", "(I)Ljava/lang/String;");
  FIND_METHOD (browse_url, "browseUrl", "(Ljava/lang/String;Z)"
	       "Ljava/lang/String;");
  FIND_METHOD (restart_emacs, "restartEmacs", "()V");
  FIND_METHOD (update_ic, "updateIC",
	       "(Lorg/gnu/emacs/EmacsWindow;IIII)V");
  FIND_METHOD (reset_ic, "resetIC",
	       "(Lorg/gnu/emacs/EmacsWindow;I)V");
  FIND_METHOD (open_content_uri, "openContentUri",
	       "(Ljava/lang/String;ZZZ)I");
  FIND_METHOD (check_content_uri, "checkContentUri",
	       "(Ljava/lang/String;ZZ)Z");
  FIND_METHOD (query_battery, "queryBattery", "()[J");
  FIND_METHOD (update_extracted_text, "updateExtractedText",
	       "(Lorg/gnu/emacs/EmacsWindow;"
	       "Landroid/view/inputmethod/ExtractedText;I)V");
  FIND_METHOD (update_cursor_anchor_info, "updateCursorAnchorInfo",
	       "(Lorg/gnu/emacs/EmacsWindow;FFFF)V");
  FIND_METHOD (get_document_authorities, "getDocumentAuthorities",
	       "()[Ljava/lang/String;");
  FIND_METHOD (request_directory_access, "requestDirectoryAccess",
	       "()I");
  FIND_METHOD (get_document_trees, "getDocumentTrees",
	       "(Ljava/lang/String;)[Ljava/lang/String;");
  FIND_METHOD (document_id_from_name, "documentIdFromName",
	       "(Ljava/lang/String;Ljava/lang/String;"
	       "[Ljava/lang/String;)I");
  FIND_METHOD (get_tree_uri, "getTreeUri",
	       "(Ljava/lang/String;Ljava/lang/String;)"
	       "Ljava/lang/String;");
  FIND_METHOD (stat_document, "statDocument",
	       "(Ljava/lang/String;Ljava/lang/String;Z)[J");
  FIND_METHOD (access_document, "accessDocument",
	       "(Ljava/lang/String;Ljava/lang/String;Z)I");
  FIND_METHOD (open_document_directory, "openDocumentDirectory",
	       "(Ljava/lang/String;Ljava/lang/String;)"
	       "Landroid/database/Cursor;");
  FIND_METHOD (read_directory_entry, "readDirectoryEntry",
	       "(Landroid/database/Cursor;)Lorg/gnu/emacs/"
	       "EmacsDirectoryEntry;");
  FIND_METHOD (open_document, "openDocument",
	       "(Ljava/lang/String;Ljava/lang/String;ZZZ)"
	       "Landroid/os/ParcelFileDescriptor;");
  FIND_METHOD (create_document, "createDocument",
	       "(Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;)Ljava/lang/String;");
  FIND_METHOD (create_directory, "createDirectory",
	       "(Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;)Ljava/lang/String;");
  FIND_METHOD (delete_document, "deleteDocument",
	       "(Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;)I");
  FIND_METHOD (rename_document, "renameDocument",
	       "(Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;Ljava/lang/String;)I");
  FIND_METHOD (move_document, "moveDocument",
	       "(Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;)Ljava/lang/String;");
  FIND_METHOD (valid_authority, "validAuthority",
	       "(Ljava/lang/String;)Z");
  FIND_METHOD (external_storage_available,
	       "externalStorageAvailable", "()Z");
  FIND_METHOD (request_storage_access,
	       "requestStorageAccess", "()V");
  FIND_METHOD (cancel_notification,
	       "cancelNotification", "(Ljava/lang/String;)V");
  FIND_METHOD (relinquish_uri_rights,
	       "relinquishUriRights", "(Ljava/lang/String;)V");
#undef FIND_METHOD
}

static void
android_init_emacs_pixmap (void)
{
  jclass old;

  pixmap_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsPixmap");
  eassert (pixmap_class.class);

  old = pixmap_class.class;
  pixmap_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!pixmap_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  pixmap_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					pixmap_class.class,	\
					name, signature);	\
  eassert (pixmap_class.c_name);

  FIND_METHOD (constructor_mutable, "<init>", "(III)V");

#undef FIND_METHOD
}

static void
android_init_graphics_point (void)
{
  jclass old;

  point_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "android/graphics/Point");
  eassert (point_class.class);

  old = point_class.class;
  point_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!point_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  point_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					point_class.class,	\
					name, signature);	\
  eassert (point_class.c_name);

  FIND_METHOD (constructor, "<init>", "(II)V");
#undef FIND_METHOD
}

static void
android_init_emacs_drawable (void)
{
  jclass old;

  drawable_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsDrawable");
  eassert (drawable_class.class);

  old = drawable_class.class;
  drawable_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!drawable_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  drawable_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					drawable_class.class,	\
					name, signature);	\
  eassert (drawable_class.c_name);

  FIND_METHOD (get_bitmap, "getBitmap", "()Landroid/graphics/Bitmap;");
#undef FIND_METHOD
}

static void
android_init_emacs_window (void)
{
  jclass old;

  window_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsWindow");
  eassert (window_class.class);

  old = window_class.class;
  window_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!window_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  window_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					window_class.class,	\
					name, signature);	\
  eassert (window_class.c_name);

  FIND_METHOD (swap_buffers, "swapBuffers", "()V");
  FIND_METHOD (toggle_on_screen_keyboard,
	       "toggleOnScreenKeyboard", "(Z)V");
  FIND_METHOD (lookup_string, "lookupString", "(I)Ljava/lang/String;");
  FIND_METHOD (set_fullscreen, "setFullscreen", "(Z)V");
  FIND_METHOD (change_window_background, "changeWindowBackground",
	       "(I)V");
  FIND_METHOD (reparent_to, "reparentTo",
	       "(Lorg/gnu/emacs/EmacsWindow;II)V");
  FIND_METHOD (map_window, "mapWindow", "()V");
  FIND_METHOD (unmap_window, "unmapWindow", "()V");
  FIND_METHOD (resize_window, "resizeWindow", "(II)V");
  FIND_METHOD (move_window, "moveWindow", "(II)V");
  FIND_METHOD (move_resize_window, "moveResizeWindow", "(IIII)V");
  FIND_METHOD (make_input_focus, "makeInputFocus", "(J)V");
  FIND_METHOD (raise, "raise", "()V");
  FIND_METHOD (lower, "lower", "()V");
  FIND_METHOD (reconfigure, "reconfigure", "(Lorg/gnu/emacs/EmacsWindow;I)V");
  FIND_METHOD (get_window_geometry, "getWindowGeometry",
	       "()[I");
  FIND_METHOD (translate_coordinates, "translateCoordinates",
	       "(II)[I");
  FIND_METHOD (set_dont_focus_on_map, "setDontFocusOnMap", "(Z)V");
  FIND_METHOD (set_dont_accept_focus, "setDontAcceptFocus", "(Z)V");
  FIND_METHOD (define_cursor, "defineCursor",
	       "(Lorg/gnu/emacs/EmacsCursor;)V");
  /* In spite of the declaration of this function being located within
     EmacsDrawable, the ID of the `damage_rect' method is retrieved
     from EmacsWindow, which avoids virtual function dispatch within
     android_damage_window.  */
  FIND_METHOD (damage_rect, "damageRect", "(IIII)V");
  FIND_METHOD (recreate_activity, "recreateActivity", "()V");
  FIND_METHOD (clear_window, "clearWindow", "()V");
  FIND_METHOD (clear_area, "clearArea", "(IIII)V");
  FIND_METHOD (set_wm_name, "setWmName", "(Ljava/lang/String;)V");
#undef FIND_METHOD
}

static void
android_init_emacs_cursor (void)
{
  jclass old;

  cursor_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsCursor");
  eassert (cursor_class.class);

  old = cursor_class.class;
  cursor_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!cursor_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  cursor_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					cursor_class.class,	\
					name, signature);	\
  eassert (cursor_class.c_name);

  FIND_METHOD (constructor, "<init>", "(I)V");
#undef FIND_METHOD
}

static void
android_init_key_character_map (void)
{
  jclass old;

  key_character_map_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "android/view/KeyCharacterMap");
  eassert (key_character_map_class.class);

  old = key_character_map_class.class;
  key_character_map_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!key_character_map_class.class)
    emacs_abort ();

  key_character_map_class.get_dead_char
    = (*android_java_env)->GetStaticMethodID (android_java_env,
					      key_character_map_class.class,
					      "getDeadChar", "(II)I");
  eassert (key_character_map_class.get_dead_char);
}

static void
android_init_emacs_handle (void)
{
  jclass old;

  handle_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsHandleObject");
  eassert (handle_class.class);

  old = handle_class.class;
  handle_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!handle_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  handle_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					handle_class.class,	\
					name, signature);	\
  eassert (handle_class.c_name);

  FIND_METHOD (destroy_handle, "destroyHandle", "()V");
#undef FIND_METHOD

  handle_class.handle
    = (*android_java_env)->GetFieldID (android_java_env,
				       handle_class.class,
				       "handle", "J");
  eassert (handle_class.handle);
}

JNIEXPORT void JNICALL
NATIVE_NAME (initEmacs) (JNIEnv *env, jobject object, jarray argv,
			 jobject dump_file_object)
{
  /* android_emacs_init is not main, so GCC is not nice enough to add
     the stack alignment prologue.

     Unfortunately for us, dalvik on Android 4.0.x calls native code
     with a 4 byte aligned stack, so this prologue must be inserted
     before each function exported via JNI.  */

  JNI_STACK_ALIGNMENT_PROLOGUE;

  char **c_argv;
  jsize nelements, i;
  jobject argument;
  const char *c_argument;
  char *dump_file;

  android_java_env = env;

  nelements = (*env)->GetArrayLength (env, argv);
  c_argv = alloca (sizeof *c_argv * (nelements + 1));

  for (i = 0; i < nelements; ++i)
    {
      argument = (*env)->GetObjectArrayElement (env, argv, i);
      c_argument = (*env)->GetStringUTFChars (env, (jstring) argument,
					      NULL);

      if (!c_argument)
	emacs_abort ();

      /* Note that c_argument is in ``modified UTF-8 encoding'', but
	 we don't care as NUL bytes are not being specified inside.  */
      c_argv[i] = alloca (strlen (c_argument) + 1);
      strcpy (c_argv[i], c_argument);
      (*env)->ReleaseStringUTFChars (env, (jstring) argument, c_argument);
    }

  c_argv[nelements] = NULL;

  android_init_emacs_service ();
  android_init_emacs_pixmap ();
  android_init_graphics_point ();
  android_init_emacs_drawable ();
  android_init_emacs_window ();
  android_init_emacs_cursor ();
  android_init_key_character_map ();
  android_init_emacs_handle ();

  /* Set HOME to the app data directory.  */
  setenv ("HOME", android_files_dir, 1);

  /* Set TMPDIR to the temporary files directory.  */
  setenv ("TMPDIR", android_cache_dir, 1);

  /* And finally set "SHELL" to /system/bin/sh.  Otherwise, some
     programs will look for /bin/sh, which is problematic.  */
  setenv ("SHELL", "/system/bin/sh", 1);

  /* Set the cwd to that directory as well.  */
  if (chdir (android_files_dir))
    __android_log_print (ANDROID_LOG_WARN, __func__,
			 "chdir: %s", strerror (errno));

  /* Initialize the Android GUI as long as the service object was
     set.  */

  if (emacs_service)
    android_init_gui = true;

  /* Now see if a dump file has been specified and should be used.  */
  dump_file = NULL;

  if (dump_file_object)
    {
      c_argument
	= (*env)->GetStringUTFChars (env, (jstring) dump_file_object,
				     NULL);
      if (!c_argument)
	emacs_abort ();

      /* Copy the Java string data once.  */
      dump_file = strdup (c_argument);

      /* Release the Java string data.  */
      (*env)->ReleaseStringUTFChars (env, (jstring) dump_file_object,
				     c_argument);
    }

  /* Delete local references to objects that are no longer needed.  */
  ANDROID_DELETE_LOCAL_REF (argv);
  ANDROID_DELETE_LOCAL_REF (dump_file_object);

  /* Restore the signal mask at the time of startup if it was changed
     to block unwanted signals from reaching system threads.  */

  if (signal_mask_changed_p)
    pthread_sigmask (SIG_SETMASK, &startup_signal_mask, NULL);

  /* Now start Emacs proper.  */
  android_emacs_init (nelements, c_argv, dump_file);

  /* android_emacs_init should never return.  */
  emacs_abort ();
}

JNIEXPORT void JNICALL
NATIVE_NAME (emacsAbort) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  emacs_abort ();
}

JNIEXPORT void JNICALL
NATIVE_NAME (quit) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  __android_log_print (ANDROID_LOG_VERBOSE, __func__,
		       "Sending SIGIO and setting Vquit_flag");

  /* Raise sigio to interrupt anything that could be reading
     input.  */
  Vquit_flag = Qt;
  kill (getpid (), SIGIO);
}

/* Call shut_down_emacs subsequent to a call to the service's
   onDestroy callback.  CLOSURE is ignored.  */

static void
android_shut_down_emacs (void *closure)
{
  __android_log_print (ANDROID_LOG_INFO, __func__,
		       "The Emacs service is being shut down");
  shut_down_emacs (0, Qnil);
}

JNIEXPORT void JNICALL
NATIVE_NAME (shutDownEmacs) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  android_run_in_emacs_thread (android_shut_down_emacs, NULL);
}

/* Carry out garbage collection and clear all image caches on the
   Android terminal.  Called when the system has depleted most of its
   memory and desires that background processes release unused
   core.  */

static void
android_on_low_memory (void *closure)
{
  Fclear_image_cache (Qt, Qnil);
  garbage_collect ();
}

JNIEXPORT void JNICALL
NATIVE_NAME (onLowMemory) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  android_run_in_emacs_thread (android_on_low_memory, NULL);
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendConfigureNotify) (JNIEnv *env, jobject object,
				   jlong window, jlong time,
				   jint x, jint y, jint width,
				   jint height)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xconfigure.type = ANDROID_CONFIGURE_NOTIFY;
  event.xconfigure.serial = ++event_serial;
  event.xconfigure.window = window;
  event.xconfigure.time = time;
  event.xconfigure.x = x;
  event.xconfigure.y = y;
  event.xconfigure.width = width;
  event.xconfigure.height = height;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendKeyPress) (JNIEnv *env, jobject object,
			    jlong window, jlong time,
			    jint state, jint keycode,
			    jint unicode_char)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xkey.type = ANDROID_KEY_PRESS;
  event.xkey.serial = ++event_serial;
  event.xkey.window = window;
  event.xkey.time = time;
  event.xkey.state = state;
  event.xkey.keycode = keycode;
  event.xkey.unicode_char = unicode_char;
  event.xkey.counter = 0;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendKeyRelease) (JNIEnv *env, jobject object,
			      jlong window, jlong time,
			      jint state, jint keycode,
			      jint unicode_char)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xkey.type = ANDROID_KEY_RELEASE;
  event.xkey.serial = ++event_serial;
  event.xkey.window = window;
  event.xkey.time = time;
  event.xkey.state = state;
  event.xkey.keycode = keycode;
  event.xkey.unicode_char = unicode_char;
  event.xkey.counter = 0;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendFocusIn) (JNIEnv *env, jobject object,
			   jlong window, jlong time)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xfocus.type = ANDROID_FOCUS_IN;
  event.xfocus.serial = ++event_serial;
  event.xfocus.window = window;
  event.xfocus.time = time;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendFocusOut) (JNIEnv *env, jobject object,
			    jlong window, jlong time)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xfocus.type = ANDROID_FOCUS_OUT;
  event.xfocus.serial = ++event_serial;
  event.xfocus.window = window;
  event.xfocus.time = time;

  android_write_event (&event);
  return ++event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendWindowAction) (JNIEnv *env, jobject object,
				jlong window, jint action)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xaction.type = ANDROID_WINDOW_ACTION;
  event.xaction.serial = ++event_serial;
  event.xaction.window = window;
  event.xaction.action = action;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendEnterNotify) (JNIEnv *env, jobject object,
			       jlong window, jint x, jint y,
			       jlong time)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xcrossing.type = ANDROID_ENTER_NOTIFY;
  event.xcrossing.serial = ++event_serial;
  event.xcrossing.window = window;
  event.xcrossing.x = x;
  event.xcrossing.y = y;
  event.xcrossing.time = time;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendLeaveNotify) (JNIEnv *env, jobject object,
			       jlong window, jint x, jint y,
			       jlong time)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xcrossing.type = ANDROID_LEAVE_NOTIFY;
  event.xcrossing.serial = ++event_serial;
  event.xcrossing.window = window;
  event.xcrossing.x = x;
  event.xcrossing.y = y;
  event.xcrossing.time = time;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendMotionNotify) (JNIEnv *env, jobject object,
				jlong window, jint x, jint y,
				jlong time)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xmotion.type = ANDROID_MOTION_NOTIFY;
  event.xmotion.serial = ++event_serial;
  event.xmotion.window = window;
  event.xmotion.x = x;
  event.xmotion.y = y;
  event.xmotion.time = time;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendButtonPress) (JNIEnv *env, jobject object,
			       jlong window, jint x, jint y,
			       jlong time, jint state,
			       jint button)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xbutton.type = ANDROID_BUTTON_PRESS;
  event.xbutton.serial = ++event_serial;
  event.xbutton.window = window;
  event.xbutton.x = x;
  event.xbutton.y = y;
  event.xbutton.time = time;
  event.xbutton.state = state;
  event.xbutton.button = button;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendButtonRelease) (JNIEnv *env, jobject object,
				 jlong window, jint x, jint y,
				 jlong time, jint state,
				 jint button)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xbutton.type = ANDROID_BUTTON_RELEASE;
  event.xbutton.serial = ++event_serial;
  event.xbutton.window = window;
  event.xbutton.x = x;
  event.xbutton.y = y;
  event.xbutton.time = time;
  event.xbutton.state = state;
  event.xbutton.button = button;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendTouchDown) (JNIEnv *env, jobject object,
			     jlong window, jint x, jint y,
			     jlong time, jint pointer_id,
			     jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.touch.type = ANDROID_TOUCH_DOWN;
  event.touch.serial = ++event_serial;
  event.touch.window = window;
  event.touch.x = x;
  event.touch.y = y;
  event.touch.time = time;
  event.touch.pointer_id = pointer_id;
  event.touch.flags = flags;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendTouchUp) (JNIEnv *env, jobject object,
			   jlong window, jint x, jint y,
			   jlong time, jint pointer_id,
			   jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.touch.type = ANDROID_TOUCH_UP;
  event.touch.serial = ++event_serial;
  event.touch.window = window;
  event.touch.x = x;
  event.touch.y = y;
  event.touch.time = time;
  event.touch.pointer_id = pointer_id;
  event.touch.flags = flags;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendTouchMove) (JNIEnv *env, jobject object,
			     jlong window, jint x, jint y,
			     jlong time, jint pointer_id,
			     jint flags)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.touch.type = ANDROID_TOUCH_MOVE;
  event.touch.serial = ++event_serial;
  event.touch.window = window;
  event.touch.x = x;
  event.touch.y = y;
  event.touch.time = time;
  event.touch.pointer_id = pointer_id;
  event.touch.flags = flags;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendWheel) (JNIEnv *env, jobject object,
			 jlong window, jint x, jint y,
			 jlong time, jint state,
			 jfloat x_delta, jfloat y_delta)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.wheel.type = ANDROID_WHEEL;
  event.wheel.serial = ++event_serial;
  event.wheel.window = window;
  event.wheel.x = x;
  event.wheel.y = y;
  event.wheel.time = time;
  event.wheel.state = state;
  event.wheel.x_delta = x_delta;
  event.wheel.y_delta = y_delta;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendIconified) (JNIEnv *env, jobject object,
			     jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.iconified.type = ANDROID_ICONIFIED;
  event.iconified.serial = ++event_serial;
  event.iconified.window = window;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendDeiconified) (JNIEnv *env, jobject object,
			       jlong window)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.iconified.type = ANDROID_DEICONIFIED;
  event.iconified.serial = ++event_serial;
  event.iconified.window = window;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendContextMenu) (JNIEnv *env, jobject object,
			       jlong window, jint menu_event_id,
			       jint menu_event_serial)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.menu.type = ANDROID_CONTEXT_MENU;
  event.menu.serial = ++event_serial;
  event.menu.window = window;
  event.menu.menu_event_id = menu_event_id;
  event.menu.menu_event_serial = menu_event_serial;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendExpose) (JNIEnv *env, jobject object,
			  jlong window, jint x, jint y,
			  jint width, jint height)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.xexpose.type = ANDROID_EXPOSE;
  event.xexpose.serial = ++event_serial;
  event.xexpose.window = window;
  event.xexpose.x = x;
  event.xexpose.y = y;
  event.xexpose.width = width;
  event.xexpose.height = height;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendDndDrag) (JNIEnv *env, jobject object,
			   jlong window, jint x, jint y)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.dnd.type = ANDROID_DND_DRAG_EVENT;
  event.dnd.serial = ++event_serial;
  event.dnd.window = window;
  event.dnd.x = x;
  event.dnd.y = y;
  event.dnd.uri_or_string = NULL;
  event.dnd.length = 0;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendDndUri) (JNIEnv *env, jobject object,
			  jlong window, jint x, jint y,
			  jstring string)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  const jchar *characters;
  jsize length;
  uint16_t *buffer;

  event.dnd.type = ANDROID_DND_URI_EVENT;
  event.dnd.serial = ++event_serial;
  event.dnd.window = window;
  event.dnd.x = x;
  event.dnd.y = y;

  length = (*env)->GetStringLength (env, string);
  buffer = malloc (length * sizeof *buffer);

  /* Out of memory.  */
  if (!buffer)
    return 0;

  characters = (*env)->GetStringChars (env, string, NULL);

  if (!characters)
    /* The JVM has run out of memory; return and let the out of memory
       error take its course.  */
    return 0;

  memcpy (buffer, characters, length * sizeof *buffer);
  (*env)->ReleaseStringChars (env, string, characters);

  event.dnd.uri_or_string = buffer;
  event.dnd.length = length;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendDndText) (JNIEnv *env, jobject object,
			   jlong window, jint x, jint y,
			   jstring string)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  const jchar *characters;
  jsize length;
  uint16_t *buffer;

  event.dnd.type = ANDROID_DND_TEXT_EVENT;
  event.dnd.serial = ++event_serial;
  event.dnd.window = window;
  event.dnd.x = x;
  event.dnd.y = y;

  length = (*env)->GetStringLength (env, string);
  buffer = malloc (length * sizeof *buffer);

  /* Out of memory.  */
  if (!buffer)
    return 0;

  characters = (*env)->GetStringChars (env, string, NULL);

  if (!characters)
    /* The JVM has run out of memory; return and let the out of memory
       error take its course.  */
    return 0;

  memcpy (buffer, characters, length * sizeof *buffer);
  (*env)->ReleaseStringChars (env, string, characters);

  event.dnd.uri_or_string = buffer;
  event.dnd.length = length;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendNotificationDeleted) (JNIEnv *env, jobject object,
				       jstring tag)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  const char *characters;

  event.notification.type = ANDROID_NOTIFICATION_DELETED;
  event.notification.serial = ++event_serial;
  event.notification.window = ANDROID_NONE;

  /* TAG is guaranteed to be an ASCII string, of which the JNI character
     encoding is a superset.  */
  characters = (*env)->GetStringUTFChars (env, tag, NULL);
  if (!characters)
    return 0;

  event.notification.tag = strdup (characters);
  (*env)->ReleaseStringUTFChars (env, tag, characters);
  if (!event.notification.tag)
    return 0;

  event.notification.action = NULL;
  event.notification.length = 0;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendNotificationAction) (JNIEnv *env, jobject object,
				      jstring tag, jstring action)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;
  const void *characters;
  jsize length;
  uint16_t *buffer;

  event.notification.type = ANDROID_NOTIFICATION_ACTION;
  event.notification.serial = ++event_serial;
  event.notification.window = ANDROID_NONE;

  /* TAG is guaranteed to be an ASCII string, of which the JNI character
     encoding is a superset.  */
  characters = (*env)->GetStringUTFChars (env, tag, NULL);
  if (!characters)
    return 0;

  event.notification.tag = strdup (characters);
  (*env)->ReleaseStringUTFChars (env, tag, characters);
  if (!event.notification.tag)
    return 0;

  length = (*env)->GetStringLength (env, action);
  buffer = malloc (length * sizeof *buffer);
  characters = (*env)->GetStringChars (env, action, NULL);

  if (!characters)
    {
      /* The JVM has run out of memory; return and let the out of memory
	 error take its course.  */
      xfree (event.notification.tag);
      return 0;
    }

  memcpy (buffer, characters, length * sizeof *buffer);
  (*env)->ReleaseStringChars (env, action, characters);

  event.notification.action = buffer;
  event.notification.length = length;

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendConfigurationChanged) (JNIEnv *env, jobject object,
					int detail, jfloat dpi_x,
					jfloat dpi_y, jfloat dpi_scaled,
					int ui_mode)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  union android_event event;

  event.config.type = ANDROID_CONFIGURATION_CHANGED;
  event.config.serial = ++event_serial;
  event.config.window = ANDROID_NONE;
  event.config.detail = detail;

  switch (detail)
    {
    case ANDROID_PIXEL_DENSITY_CHANGED:
      event.config.u.pixel_density.dpi_x = dpi_x;
      event.config.u.pixel_density.dpi_y = dpi_y;
      event.config.u.pixel_density.dpi_scaled = dpi_scaled;
      break;

    case ANDROID_UI_MODE_CHANGED:
      event.config.u.ui_mode = ui_mode;
      break;

    default:
      emacs_abort ();
    }

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jboolean JNICALL
NATIVE_NAME (shouldForwardMultimediaButtons) (JNIEnv *env,
					      jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  /* Yes, android_pass_multimedia_buttons_to_system is being
     read from the UI thread.  */
  return !android_pass_multimedia_buttons_to_system;
}

JNIEXPORT jint JNICALL
NATIVE_NAME (getQuitKeycode) (JNIEnv *env, jobject object)
{
  /* Likewise.  */
  return (jint) android_quit_keycode;
}

JNIEXPORT jboolean JNICALL
NATIVE_NAME (shouldForwardCtrlSpace) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  return !android_intercept_control_space;
}

JNIEXPORT void JNICALL
NATIVE_NAME (blitRect) (JNIEnv *env, jobject object,
			jobject src, jobject dest,
			jint x1, jint y1, jint x2, jint y2)
{
  AndroidBitmapInfo src_info, dest_info;
  unsigned char *src_data_1, *dest_data_1;
  void *src_data, *dest_data;

  /* N.B. that X2 and Y2 represent the pixel past the edge of the
     rectangle; thus, the width is x2 - x1 and the height is y2 -
     y1.  */

  memset (&src_info, 0, sizeof src_info);
  memset (&dest_info, 0, sizeof dest_info);
  AndroidBitmap_getInfo (env, src, &src_info);
  AndroidBitmap_getInfo (env, dest, &dest_info);

  /* If the stride is 0 after a call to `getInfo', assume it
     failed.  */

  if (!src_info.stride || !dest_info.stride)
    return;

  /* If formats differ, abort.  */
  eassert (src_info.format == dest_info.format
	   && src_info.format == ANDROID_BITMAP_FORMAT_RGBA_8888);

  /* Lock the image data.  */
  src_data = NULL;
  AndroidBitmap_lockPixels (env, src, &src_data);

  if (!src_data)
    return;

  dest_data = NULL;
  AndroidBitmap_lockPixels (env, dest, &dest_data);

  if (!dest_data)
    goto fail1;

  /* Now clip the rectangle to the bounds of the source and
     destination bitmap.  */

  x1 = MAX (x1, 0);
  y1 = MAX (y1, 0);
  x2 = MAX (x2, 0);
  y2 = MAX (y2, 0);

  if (x1 >= src_info.width
      || x1 >= dest_info.width)
    x1 = MIN (dest_info.width - 1, src_info.width - 1);

  if (x2 > src_info.width
      || x2 > dest_info.width)
    x2 = MIN (src_info.width, dest_info.width);

  if (y1 >= src_info.height
      || y1 >= dest_info.height)
    y1 = MIN (dest_info.height - 1, src_info.height - 1);

  if (y2 > src_info.height
      || y2 > dest_info.height)
    y2 = MIN (src_info.height, dest_info.height);

  if (x1 >= x2 || y1 >= y2)
    goto fail2;

  /* Determine the address of the first line to copy.  */

  src_data_1 = src_data;
  dest_data_1 = dest_data;
  src_data_1 += x1 * 4;
  src_data_1 += y1 * src_info.stride;
  dest_data_1 += x1 * 4;
  dest_data_1 += y1 * dest_info.stride;

  /* Start copying each line.  */

  while (y1 != y2)
    {
      memcpy (dest_data_1, src_data_1, (x2 - x1) * 4);
      src_data_1 += src_info.stride;
      dest_data_1 += dest_info.stride;
      y1++;
    }

  /* Complete the copy and unlock the bitmap.  */

 fail2:
  AndroidBitmap_unlockPixels (env, dest);
 fail1:
  AndroidBitmap_unlockPixels (env, src);
}

JNIEXPORT void JNICALL
NATIVE_NAME (notifyPixelsChanged) (JNIEnv *env, jobject object,
				   jobject bitmap)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  void *data;

  /* Lock and unlock the bitmap.  This calls
     SkBitmap->notifyPixelsChanged.  */

  if (AndroidBitmap_lockPixels (env, bitmap, &data) < 0)
    /* The return value is less than 0 if an error occurs.
       Good luck finding this in the documentation.  */
    return;

  AndroidBitmap_unlockPixels (env, bitmap);
}

/* Forward declarations of deadlock prevention functions.  */

static void android_begin_query (void);
static void android_end_query (void);
static void android_answer_query_spin (void);

JNIEXPORT void JNICALL
NATIVE_NAME (beginSynchronous) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  android_begin_query ();
}

JNIEXPORT void JNICALL
NATIVE_NAME (endSynchronous) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  android_end_query ();
}

JNIEXPORT void JNICALL
NATIVE_NAME (answerQuerySpin) (JNIEnv *env, jobject object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  android_answer_query_spin ();
}



/* System thread setup.  Android doesn't always block signals Emacs is
   interested in from being received by the UI or render threads,
   which can lead to problems when those signals then interrupt one of
   those threads.  */

JNIEXPORT void JNICALL
NATIVE_NAME (setupSystemThread) (void)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  sigset_t sigset;

  /* Block everything except for SIGSEGV and SIGBUS; those two are
     used by the runtime.  */

  sigfillset (&sigset);
  sigdelset (&sigset, SIGSEGV);
  sigdelset (&sigset, SIGBUS);

  /* Save the signal mask that was previously used.  It will be
     restored in `initEmacs'.  */

  if (pthread_sigmask (SIG_BLOCK, &sigset, &startup_signal_mask))
    __android_log_print (ANDROID_LOG_WARN, __func__,
			 "pthread_sigmask: %s", strerror (errno));
  else
    signal_mask_changed_p = true;
}

#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma GCC diagnostic pop
#endif



/* Java functions called by C.

   Because all C code runs in the native function initEmacs, ALL LOCAL
   REFERENCES WILL PERSIST!

   This means that every local reference must be explicitly destroyed
   with DeleteLocalRef.  A helper macro is provided to do this.  */

/* Destroy the specified handle and mark it as free on the Java side
   as well.  */

static void
android_destroy_handle (android_handle handle)
{
  static jclass old, class;
  static jmethodID method;

  if (!class)
    {
      class
	= (*android_java_env)->FindClass (android_java_env,
					  "org/gnu/emacs/EmacsHandleObject");
      eassert (class != NULL);

      method
	= (*android_java_env)->GetMethodID (android_java_env, class,
					    "destroyHandle", "()V");
      eassert (method != NULL);

      old = class;
      class
	= (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						      (jobject) class);
      android_exception_check_1 (old);
      ANDROID_DELETE_LOCAL_REF (old);
    }

  (*android_java_env)->CallVoidMethod (android_java_env, (jobject) handle,
				       method);

  /* Just clear any exception thrown.  If destroying the handle
     fails from an out-of-memory error, then Emacs loses some
     resources, but that is not as big deal as signaling.  */
  (*android_java_env)->ExceptionClear (android_java_env);

  /* Delete the global reference regardless of any error.  */
  (*android_java_env)->DeleteGlobalRef (android_java_env, (jobject) handle);
}

void
android_change_window_attributes (android_window handle,
				  enum android_window_value_mask value_mask,
				  struct android_set_window_attributes *attrs)
{
  jmethodID method;
  jobject window;
  jint pixel;

  window = android_resolve_handle (handle);

  if (value_mask & ANDROID_CW_BACK_PIXEL)
    {
      method = window_class.change_window_background;
      pixel = (jint) attrs->background_pixel;
      (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						     window,
						     window_class.class,
						     method, pixel);
      android_exception_check ();
    }
}

/* Return a reference to the local reference HANDLE suitable for
   indefinite retention and save its value into HANDLE, deleting HANDLE,
   or signal an error if such a reference cannot be allocated.  */

static android_handle
android_globalize_reference (jobject handle)
{
  jobject global;

  /* Though Android 8.0 and later can support an unlimited number of
     active local references, they remain inappropriate in threading
     configurations for being local to the current thread.  */

  global = (*android_java_env)->NewGlobalRef (android_java_env,
					      handle);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (handle);

  if (__builtin_expect (global == NULL, 0))
    error ("JNI global reference reserves exhausted");

  /* Save the value of this handle into HANDLE.  */
  (*android_java_env)->SetLongField (android_java_env, global,
				     handle_class.handle,
				     (jlong) global);
  static_assert (sizeof (jlong) >= sizeof (intptr_t));
  return (intptr_t) global;
}

/* Create a new window with the given width, height and
   attributes.  */

android_window
android_create_window (android_window parent, int x, int y,
		       int width, int height,
		       enum android_window_value_mask value_mask,
		       struct android_set_window_attributes *attrs)
{
  static jclass class;
  static jmethodID constructor;
  jobject object, parent_object, old;
  android_window window;
  bool override_redirect;

  parent_object = android_resolve_handle (parent);


  if (!class)
    {
      class = (*android_java_env)->FindClass (android_java_env,
					      "org/gnu/emacs/EmacsWindow");
      eassert (class != NULL);

      constructor
	= (*android_java_env)->GetMethodID (android_java_env, class, "<init>",
					    "(Lorg/gnu/emacs/EmacsWindow;"
					    "IIIIZ)V");
      eassert (constructor != NULL);

      old = class;
      class = (*android_java_env)->NewGlobalRef (android_java_env, class);
      android_exception_check_1 (old);
      ANDROID_DELETE_LOCAL_REF (old);
    }

  /* N.B. that ANDROID_CW_OVERRIDE_REDIRECT can only be set at window
     creation time.  */
  override_redirect = ((value_mask
			& ANDROID_CW_OVERRIDE_REDIRECT)
		       && attrs->override_redirect);

  object = (*android_java_env)->NewObject (android_java_env, class,
					   constructor, parent_object,
					   (jint) x, (jint) y,
					   (jint) width, (jint) height,
					   (jboolean) override_redirect);
  android_exception_check ();
  window = android_globalize_reference (object);
  android_change_window_attributes (window, value_mask, attrs);
  return window;
}

void
android_set_window_background (android_window window, unsigned long pixel)
{
  struct android_set_window_attributes attrs;

  attrs.background_pixel = pixel;
  android_change_window_attributes (window, ANDROID_CW_BACK_PIXEL,
				    &attrs);
}

void
android_destroy_window (android_window window)
{
  android_destroy_handle (window);
}

static void
android_init_android_rect_class (void)
{
  jclass old;

  if (android_rect_class)
    /* Already initialized.  */
    return;

  android_rect_class
    = (*android_java_env)->FindClass (android_java_env,
				      "android/graphics/Rect");
  eassert (android_rect_class);

  android_rect_constructor
    = (*android_java_env)->GetMethodID (android_java_env, android_rect_class,
					"<init>", "(IIII)V");
  eassert (emacs_gc_constructor);

  old = android_rect_class;
  android_rect_class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) android_rect_class);
  android_exception_check_1 (old);
  ANDROID_DELETE_LOCAL_REF (old);
}

static void
android_init_emacs_gc_class (void)
{
  jclass old;

  if (emacs_gc_class)
    /* Already initialized.  */
    return;

  emacs_gc_class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsGC");
  eassert (emacs_gc_class);

  emacs_gc_constructor
    = (*android_java_env)->GetMethodID (android_java_env,
					emacs_gc_class,
					"<init>", "()V");
  eassert (emacs_gc_constructor);

  emacs_gc_mark_dirty
    = (*android_java_env)->GetMethodID (android_java_env,
					emacs_gc_class,
					"markDirty", "(Z)V");
  eassert (emacs_gc_mark_dirty);

  old = emacs_gc_class;
  emacs_gc_class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) emacs_gc_class);
  android_exception_check_1 (old);
  ANDROID_DELETE_LOCAL_REF (old);

  emacs_gc_foreground
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "foreground", "I");
  emacs_gc_background
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "background", "I");
  emacs_gc_function
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "function", "I");
  emacs_gc_clip_rects
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "clip_rects",
				       "[Landroid/graphics/Rect;");
  emacs_gc_clip_x_origin
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "clip_x_origin", "I");
  emacs_gc_clip_y_origin
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "clip_y_origin", "I");
  emacs_gc_stipple
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "stipple",
				       "Lorg/gnu/emacs/EmacsPixmap;");
  emacs_gc_clip_mask
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "clip_mask",
				       "Lorg/gnu/emacs/EmacsPixmap;");
  emacs_gc_fill_style
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "fill_style", "I");
  emacs_gc_ts_origin_x
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "ts_origin_x", "I");
  emacs_gc_ts_origin_y
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "ts_origin_y", "I");
  emacs_gc_line_style
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "line_style", "I");
  emacs_gc_line_width
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "line_width", "I");
  emacs_gc_dash_offset
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "dash_offset", "I");
  emacs_gc_dashes
    = (*android_java_env)->GetFieldID (android_java_env,
				       emacs_gc_class,
				       "dashes", "[I");
}

struct android_gc *
android_create_gc (enum android_gc_value_mask mask,
		   struct android_gc_values *values)
{
  struct android_gc *gc;
  jobject object;

  android_init_emacs_gc_class ();

  gc = xmalloc (sizeof *gc);
  gc->gcontext   = 0;
  gc->foreground = 0;
  gc->background = 0xffffff;
  gc->clip_rects = NULL;

  /* This means to not apply any clipping.  */
  gc->num_clip_rects = -1;

  /* Apply the other default values.  */
  gc->function = ANDROID_GC_COPY;
  gc->fill_style = ANDROID_FILL_SOLID;
  gc->clip_x_origin = 0;
  gc->clip_y_origin = 0;
  gc->clip_mask = ANDROID_NONE;
  gc->stipple = ANDROID_NONE;
  gc->ts_x_origin = 0;
  gc->ts_y_origin = 0;
  gc->line_style = ANDROID_LINE_SOLID;
  gc->line_width = 0;
  gc->dash_offset = 0;
  gc->dashes = NULL;
  gc->n_segments = 0;

  object = (*android_java_env)->NewObject (android_java_env,
					   emacs_gc_class,
					   emacs_gc_constructor);
  android_exception_check ();

  gc->gcontext = android_globalize_reference (object);
  android_change_gc (gc, mask, values);
  return gc;
}

void
android_free_gc (struct android_gc *gc)
{
  android_destroy_handle (gc->gcontext);

  xfree (gc->dashes);
  xfree (gc->clip_rects);
  xfree (gc);
}

void
android_change_gc (struct android_gc *gc,
		   enum android_gc_value_mask mask,
		   struct android_gc_values *values)
{
  jobject what, gcontext, array;
  jboolean clip_changed;

  clip_changed = false;

  android_init_emacs_gc_class ();
  gcontext = android_resolve_handle (gc->gcontext);

  if (mask & ANDROID_GC_FOREGROUND)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_foreground,
					values->foreground);
      gc->foreground = values->foreground;
    }

  if (mask & ANDROID_GC_BACKGROUND)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_background,
					values->background);
      gc->background = values->background;
    }

  if (mask & ANDROID_GC_FUNCTION)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_function,
					values->function);
      gc->function = values->function;
    }

  if (mask & ANDROID_GC_CLIP_X_ORIGIN)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_clip_x_origin,
					values->clip_x_origin);
      gc->clip_x_origin = values->clip_x_origin;
      clip_changed = true;
    }

  if (mask & ANDROID_GC_CLIP_Y_ORIGIN)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_clip_y_origin,
					values->clip_y_origin);
      gc->clip_y_origin = values->clip_y_origin;
      clip_changed = true;
    }

  if (mask & ANDROID_GC_CLIP_MASK)
    {
      what = android_resolve_handle (values->clip_mask);
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_clip_mask,
					   what);
      gc->clip_mask = values->clip_mask;

      /* Changing GCClipMask also clears the clip rectangles.  */
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_clip_rects,
					   NULL);

      xfree (gc->clip_rects);
      gc->clip_rects = NULL;
      gc->num_clip_rects = -1;
      clip_changed = true;
    }

  if (mask & ANDROID_GC_STIPPLE)
    {
      what = android_resolve_handle (values->stipple);
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_stipple,
					   what);
      gc->stipple = values->stipple;
    }

  if (mask & ANDROID_GC_FILL_STYLE)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_fill_style,
					values->fill_style);
      gc->fill_style = values->fill_style;
    }

  if (mask & ANDROID_GC_TILE_STIP_X_ORIGIN)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_ts_origin_x,
					values->ts_x_origin);
      gc->ts_x_origin = values->ts_x_origin;
    }

  if (mask & ANDROID_GC_TILE_STIP_Y_ORIGIN)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_ts_origin_y,
					values->ts_y_origin);
      gc->ts_y_origin = values->ts_y_origin;
    }

  if (mask & ANDROID_GC_LINE_STYLE)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_line_style,
					values->line_style);
      gc->line_style = values->line_style;
    }

  if (mask & ANDROID_GC_LINE_WIDTH)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_line_width,
					values->line_width);
      gc->line_width = values->line_width;
    }

  if (mask & ANDROID_GC_DASH_OFFSET)
    {
      (*android_java_env)->SetIntField (android_java_env,
					gcontext,
					emacs_gc_dash_offset,
					values->dash_offset);
      gc->dash_offset = values->dash_offset;
    }

  if (mask & ANDROID_GC_DASH_LIST)
    {
      /* Compare the new dash pattern with the old.  */
      if (gc->dashes && gc->n_segments == 1
	  && gc->dashes[0] == values->dash)
	/* If they be identical, nothing needs to change.  */
	mask &= ~ANDROID_GC_DASH_LIST;
      else
	{
	  if (gc->n_segments != 1)
	    gc->dashes = xrealloc (gc->dashes, sizeof *gc->dashes);
	  gc->n_segments = 1;
	  gc->dashes[0] = values->dash;
	  array = (*android_java_env)->NewIntArray (android_java_env, 1);
	  android_exception_check ();
	  (*android_java_env)->SetIntArrayRegion (android_java_env,
						  array, 0, 1,
						  (jint *) &values->dash);
	  (*android_java_env)->SetObjectField (android_java_env,
					       gcontext,
					       emacs_gc_dashes,
					       array);
	  ANDROID_DELETE_LOCAL_REF (array);
	}
    }

  if (mask)
    {
      (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						     gcontext,
						     emacs_gc_class,
						     emacs_gc_mark_dirty,
						     (jboolean) clip_changed);
      android_exception_check ();
    }
}

void
android_set_clip_rectangles (struct android_gc *gc, int clip_x_origin,
			     int clip_y_origin,
			     struct android_rectangle *clip_rects,
			     int n_clip_rects)
{
  jobjectArray array;
  jobject rect, gcontext;
  int i;

  android_init_android_rect_class ();
  android_init_emacs_gc_class ();

  gcontext = android_resolve_handle (gc->gcontext);

  array = (*android_java_env)->NewObjectArray (android_java_env,
					       n_clip_rects,
					       android_rect_class,
					       NULL);
  android_exception_check ();

  for (i = 0; i < n_clip_rects; ++i)
    {
      rect = (*android_java_env)->NewObject (android_java_env,
					     android_rect_class,
					     android_rect_constructor,
					     (jint) clip_rects[i].x,
					     (jint) clip_rects[i].y,
					     (jint) (clip_rects[i].x
						     + clip_rects[i].width),
					     (jint) (clip_rects[i].y
						     + clip_rects[i].height));

      /* The meaning of this call is to check whether or not an
	 allocation error happened, and to delete ARRAY and signal an
	 out-of-memory error if that is the case.  */
      android_exception_check_1 (array);

      (*android_java_env)->SetObjectArrayElement (android_java_env,
						  array, i, rect);
      ANDROID_DELETE_LOCAL_REF (rect);
    }

  (*android_java_env)->SetObjectField (android_java_env,
				       gcontext,
				       emacs_gc_clip_rects,
				       (jobject) array);
  ANDROID_DELETE_LOCAL_REF (array);

  (*android_java_env)->SetIntField (android_java_env,
				    gcontext,
				    emacs_gc_clip_x_origin,
				    clip_x_origin);
  (*android_java_env)->SetIntField (android_java_env,
				    gcontext,
				    emacs_gc_clip_y_origin,
				    clip_y_origin);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 gcontext,
						 emacs_gc_class,
						 emacs_gc_mark_dirty,
						 (jboolean) true);
  android_exception_check ();

  /* Cache the clip rectangles on the C side for
     sfntfont-android.c.  */
  if (gc->clip_rects)
    xfree (gc->clip_rects);

  /* If gc->num_clip_rects is 0, then no drawing will be performed at
     all.  */
  gc->clip_rects = xmalloc (sizeof *gc->clip_rects
			    * n_clip_rects);
  gc->num_clip_rects = n_clip_rects;
  memcpy (gc->clip_rects, clip_rects,
	  n_clip_rects * sizeof *gc->clip_rects);
}

void
android_set_dashes (struct android_gc *gc, int dash_offset,
		    int *dash_list, int n)
{
  int i;
  jobject array, gcontext;

  gcontext = android_resolve_handle (gc->gcontext);

  if (n == gc->n_segments
      && (!gc->dashes || !memcmp (gc->dashes, dash_list,
				  sizeof *dash_list * n)))
    /* No change in the dash list.  */
    goto set_offset;

  if (!n)
    {
      /* Reset the dash list to its initial empty state.  */
      xfree (gc->dashes);
      gc->dashes = NULL;
      array = NULL;
    }
  else
    {
      /* If the size of the array has not changed, it can be reused.  */

      if (n != gc->n_segments)
	{
	  gc->dashes = xrealloc (gc->dashes, sizeof *gc->dashes * n);
	  array = (*android_java_env)->NewIntArray (android_java_env, n);
	  android_exception_check ();
	}
      else
	array = (*android_java_env)->GetObjectField (android_java_env,
						     gcontext,
						     emacs_gc_dashes);

      /* Copy the list of segments into both arrays.  */
      for (i = 0; i < n; ++i)
	gc->dashes[i] = dash_list[i];
      static_assert (sizeof (int) == sizeof (jint));
      (*android_java_env)->SetIntArrayRegion (android_java_env,
					      array, 0, n,
					      (jint *) dash_list);
    }

  /* Replace the dash array in the GContext object if required.  */
  if (n != gc->n_segments)
    {
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_dashes,
					   array);
      gc->n_segments = n;
    }

  if (array)
    ANDROID_DELETE_LOCAL_REF (array);

 set_offset:
  /* And the offset.  */
  if (dash_offset != gc->dash_offset)
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_dash_offset,
				      dash_offset);
  gc->dash_offset = dash_offset;
}

void
android_reparent_window (android_window w, android_window parent_handle,
			 int x, int y)
{
  jobject window, parent;
  jmethodID method;

  window = android_resolve_handle (w);
  parent = android_resolve_handle (parent_handle);

  method = window_class.reparent_to;
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, window,
						 window_class.class, method,
						 parent, (jint) x, (jint) y);
  android_exception_check ();
}

void
android_clear_window (android_window handle)
{
  jobject window;

  window = android_resolve_handle (handle);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 window_class.clear_window);
  android_exception_check ();
}

void
android_map_window (android_window handle)
{
  jobject window;
  jmethodID map_window;

  window = android_resolve_handle (handle);
  map_window = window_class.map_window;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 map_window);
  android_exception_check ();
}

void
android_unmap_window (android_window handle)
{
  jobject window;
  jmethodID unmap_window;

  window = android_resolve_handle (handle);
  unmap_window = window_class.unmap_window;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 unmap_window);
  android_exception_check ();
}

void
android_resize_window (android_window handle, unsigned int width,
		       unsigned int height)
{
  jobject window;
  jmethodID resize_window;

  window = android_resolve_handle (handle);
  resize_window = window_class.resize_window;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 resize_window,
						 (jint) width,
						 (jint) height);
  android_exception_check ();
}

void
android_move_window (android_window handle, int x, int y)
{
  jobject window;
  jmethodID move_window;

  window = android_resolve_handle (handle);
  move_window = window_class.move_window;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 move_window,
						 (jint) x, (jint) y);
  android_exception_check ();
}

void
android_swap_buffers (struct android_swap_info *swap_info,
		      int num_windows)
{
  jobject window;
  int i;

  for (i = 0; i < num_windows; ++i)
    {
      window = android_resolve_handle (swap_info[i].swap_window);
      (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						     window,
						     window_class.class,
						     window_class.swap_buffers);
      android_exception_check ();
    }
}

void
android_get_gc_values (struct android_gc *gc,
		       enum android_gc_value_mask mask,
		       struct android_gc_values *values)
{
  if (mask & ANDROID_GC_FOREGROUND)
    /* GCs never have 32 bit colors, so we don't have to worry about
       sign extension here.  */
    values->foreground = gc->foreground;

  if (mask & ANDROID_GC_BACKGROUND)
    values->background = gc->background;

  if (mask & ANDROID_GC_FUNCTION)
    values->function = gc->function;

  if (mask & ANDROID_GC_CLIP_X_ORIGIN)
    values->clip_x_origin = gc->clip_x_origin;

  if (mask & ANDROID_GC_CLIP_Y_ORIGIN)
    values->clip_y_origin = gc->clip_y_origin;

  if (mask & ANDROID_GC_FILL_STYLE)
    values->fill_style = gc->fill_style;

  if (mask & ANDROID_GC_TILE_STIP_X_ORIGIN)
    values->ts_x_origin = gc->ts_x_origin;

  if (mask & ANDROID_GC_TILE_STIP_Y_ORIGIN)
    values->ts_y_origin = gc->ts_y_origin;

  /* Fields involving handles are not used by Emacs, and thus not
     implemented.  In addition, the size of GCClipMask and GCDashList is
     not static, precluding their retrieval.  */
}

void
android_set_foreground (struct android_gc *gc, unsigned long foreground)
{
  struct android_gc_values gcv;

  gcv.foreground = foreground;
  android_change_gc (gc, ANDROID_GC_FOREGROUND, &gcv);
}

void
android_fill_rectangle (android_drawable handle, struct android_gc *gc,
			int x, int y, unsigned int width,
			unsigned int height)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle (handle);
  gcontext = android_resolve_handle (gc->gcontext);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.fill_rectangle,
						 drawable,
						 gcontext,
						 (jint) x, (jint) y,
						 (jint) width,
						 (jint) height);
}

android_pixmap
android_create_pixmap_from_bitmap_data (char *data, unsigned int width,
					unsigned int height,
					unsigned long foreground,
					unsigned long background,
					unsigned int depth)
{
  android_pixmap pixmap;
  jobject object;
  AndroidBitmapInfo info;
  unsigned int *depth_24;
  unsigned char *depth_8;
  void *bitmap_data;
  unsigned int x, y;
  unsigned int r, g, b;

  /* Create a pixmap with the right dimensions and depth.  */
  pixmap = android_create_pixmap (width, height, depth);

  /* Lock the bitmap data.  */
  bitmap_data = android_lock_bitmap (pixmap, &info, &object);

  /* Merely return if locking the bitmap fails.  */
  if (!bitmap_data)
    return pixmap;

  eassert (info.format == ANDROID_BITMAP_FORMAT_RGBA_8888
	   || info.format == ANDROID_BITMAP_FORMAT_A_8);

  /* Begin copying each line.  */

  switch (info.format)
    {
    case ANDROID_BITMAP_FORMAT_RGBA_8888:

      /* Swizzle the pixels into ABGR format.  Android uses Skia's
	 ``native color type'', which is ABGR.  This is despite the
	 format being named ``ARGB'', and more confusingly
	 `ANDROID_BITMAP_FORMAT_RGBA_8888' in bitmap.h.  */

      r = background & 0x00ff0000;
      g = background & 0x0000ff00;
      b = background & 0x000000ff;
      background = (r >> 16) | g | (b << 16) | 0xff000000;
      r = foreground & 0x00ff0000;
      g = foreground & 0x0000ff00;
      b = foreground & 0x000000ff;
      foreground = (r >> 16) | g | (b << 16) | 0xff000000;

      for (y = 0; y < height; ++y)
	{
	  depth_24 = (void *) ((char *) bitmap_data + y * info.stride);

	  for (x = 0; x < width; ++x)
	    depth_24[x] = ((data[x / 8] & (1 << (x % 8)))
			   ? foreground : background);

	  data += (width + 7) / 8;
	}

      break;

    case ANDROID_BITMAP_FORMAT_A_8:

      /* 8-bit pixmaps are created, but in spite of that they are
	 employed only to represent bitmaps.  */

      foreground = (foreground ? 255 : 0);
      background = (background ? 255 : 0);

      for (y = 0; y < height; ++y)
	{
	  depth_8 = (void *) ((char *) bitmap_data + y * info.stride);

	  for (x = 0; x < width; ++x)
	    depth_8[x] = ((data[x / 8] & (1 << (x % 8)))
			  ? foreground : background);

	  data += (width + 7) / 8;
	}

      break;

    default:
      emacs_abort ();
    }

  /* Unlock the bitmap itself.  */
  AndroidBitmap_unlockPixels (android_java_env, object);
  ANDROID_DELETE_LOCAL_REF (object);

  /* Return the pixmap.  */
  return pixmap;
}

void
android_set_clip_mask (struct android_gc *gc, android_pixmap pixmap)
{
  struct android_gc_values gcv;

  gcv.clip_mask = pixmap;
  android_change_gc (gc, ANDROID_GC_CLIP_MASK, &gcv);
}

void
android_set_fill_style (struct android_gc *gc,
			enum android_fill_style fill_style)
{
  struct android_gc_values gcv;

  gcv.fill_style = fill_style;
  android_change_gc (gc, ANDROID_GC_FILL_STYLE, &gcv);
}



/* Pixmap bit blit implementation.  This exists as `Canvas.drawBitmap'
   seems to have trouble with copying bitmap data from one bitmap back
   to itself on Android 8.0.  */

/* Function called to actually perform the copy.  */

typedef void (*android_blit_func) (int, int, int, int, int, int,
				   struct android_gc *,
				   unsigned char *, AndroidBitmapInfo *,
				   unsigned char *, AndroidBitmapInfo *,
				   unsigned char *, AndroidBitmapInfo *);



#ifdef __aarch64__

/* Copy N pixels from SRC to DST, using MASK as a depth 1 clip
   mask.  */

static void
android_neon_mask_line (unsigned int *src, unsigned int *dst,
			unsigned char *mask, int n)
{
  uint32x4_t src_low, src_high, dst_low, dst_high;
  int16x8_t vmask;
  int32x4_t ext_mask_low, ext_mask_high, low, high;
  int rem, i;

  /* Calculate the remainder.  */
  rem = n & 7, n &= ~7;

  /* Process eight pixels at a time.  */

  if (n)
    {
    again:
      /* Load the low and high four pixels from the source.  */
      src_low = vld1q_u32 (src);
      src_high = vld1q_u32 (src + 4);

      /* Do the same with the destination.  */
      dst_low = vld1q_u32 (dst);
      dst_high = vld1q_u32 (dst + 4);

      /* Load and sign extend the mask.  */
      vmask = vmovl_s8 (vld1_u8 (mask));
      ext_mask_low = vmovl_s16 (vget_low_s16 (vmask));
      ext_mask_high = vmovl_s16 (vget_high_s16 (vmask));

      /* Reinterpret the mask.  */
      low = vreinterpretq_u32_s32 (ext_mask_low);
      high = vreinterpretq_u32_s32 (ext_mask_high);

      /* Apply the mask.  */
      dst_low = vbicq_u32 (dst_low, low);
      src_low = vandq_u32 (src_low, low);
      dst_high = vbicq_u32 (dst_high, high);
      src_high = vandq_u32 (src_high, high);

      /* Write the result after combining both masked vectors.  */
      vst1q_u32 (dst, vorrq_u32 (dst_low, src_low));
      vst1q_u32 (dst + 4, vorrq_u32 (dst_high, src_high));

      /* Adjust src, dst and mask.  */
      dst += 8;
      src += 8;
      mask += 8;

      /* See if this loop should continue.  */
      n -= 8;
      if (n > 0)
	goto again;
    }

  /* Process the remaining pixels.  */

  for (i = 0; i < rem; ++i)
    {
      /* Sign extend the mask.  */
      n = ((signed char *) mask)[i];

      /* Combine src and dst.  */
      dst[i] = ((src[i] & n) | (dst[i] & ~n));
    }
}

#endif /* __aarch64__ */



/* Copy a rectangle SRC_X, SRC_Y, WIDTH and HEIGHT from SRC, described
   by SRC_INFO, to DST_X and DST_Y in DST, as described by DST_INFO.

   If MASK is set, mask the source data using MASK_INFO, translating
   it by GC->clip_x_origin and GC->clip_y_origin.  MASK must be a
   pixmap of depth 1.

   N.B. that currently only copies between bitmaps of depth 24 are
   implemented.  */

static void
android_blit_copy (int src_x, int src_y, int width, int height,
		   int dst_x, int dst_y, struct android_gc *gc,
		   unsigned char *src, AndroidBitmapInfo *src_info,
		   unsigned char *dst, AndroidBitmapInfo *dst_info,
		   unsigned char *mask, AndroidBitmapInfo *mask_info)
{
  uintptr_t start, end;
  int mask_offset;
  size_t pixel, offset, offset1;
  unsigned char *src_current, *dst_current;
  unsigned char *mask_current;
  int overflow, temp, i;
#ifndef __aarch64__
  int j;
#endif /* __aarch64__ */
  bool backwards;
  unsigned int *long_src, *long_dst;

  /* Assert that the specified coordinates are within bounds.  */
  eassert (src_x >= 0 && src_y >= 0
	   && dst_x >= 0 && dst_y >= 0);
  eassert (src_x + width <= src_info->width);
  eassert (src_y + height <= src_info->height);
  eassert (dst_x + width <= dst_info->width);
  eassert (dst_y + height <= dst_info->height);

  /* Now check that each bitmap has the correct format.  */
  eassert (src_info->format == dst_info->format
	   && src_info->format == ANDROID_BITMAP_FORMAT_RGBA_8888);
  pixel = sizeof (unsigned int);

  /* Android doesn't have A1 bitmaps, so A8 is used to represent
     packed bitmaps of depth 1.  */
  eassert (!mask || mask_info->format == ANDROID_BITMAP_FORMAT_A_8);

  /* Calculate the address of the first pixel of the first row to be
     copied in both src and dst.  Compare them to determine the
     direction in which the copy is to take place.  */

  overflow  = ckd_mul (&start, src_y, src_info->stride);
  overflow |= ckd_mul (&end, src_x, pixel);
  overflow |= ckd_add (&start, end, start);
  overflow |= ckd_add (&start, (uintptr_t) src, start);

  if (overflow)
    return;

  src_current = (unsigned char *) start;

  overflow  = ckd_mul (&start, dst_y, dst_info->stride);
  overflow |= ckd_mul (&end, dst_x, pixel);
  overflow |= ckd_add (&start, end, start);
  overflow |= ckd_add (&start, (uintptr_t) dst, start);

  if (overflow)
    return;

  dst_current = (unsigned char *) start;
  backwards = false;

  /* Now see if copying should proceed from the bottom up.  */

  if (src == dst && dst_current >= src_current)
    {
      backwards = true;

      /* Walk src and dst from bottom to top, in order to avoid
	 overlap.  Calculate the coordinate of the last pixel of the
	 last row in both src and dst.  */

      overflow = ckd_mul (&start, src_y + height - 1,
			  src_info->stride);

      if (mask)
	/* If a mask is set, put the pointers before the end of the
	   row.  */
	overflow |= ckd_mul (&end, src_x + width - 1, pixel);
      else
	end = src_x * pixel;

      overflow |= ckd_add (&start, start, end);
      overflow |= ckd_add (&start, (uintptr_t) src, start);

      if (overflow)
	return;

      src_current = (unsigned char *) start;

      overflow = ckd_mul (&start, dst_y + height - 1,
			  dst_info->stride);

      if (mask)
	/* If a mask is set, put the pointers before the end of the
	   row.  */
	overflow |= ckd_mul (&end, dst_x + width - 1, pixel);
      else
	end = dst_x * pixel;

      overflow |= ckd_add (&start, start, end);
      overflow |= ckd_add (&start, (uintptr_t) dst, start);

      if (overflow)
	return;

      dst_current = (unsigned char *) start;
    }

  if (!mask)
    {
      /* Change the direction of the copy depending on how SRC and DST
	 overlap.  */

      for (i = 0; i < height; ++i)
	{
	  memmove (dst_current, src_current,
		   width * pixel);

	  if (backwards)
	    {
	      /* Proceed to the last row.  */
	      src_current -= src_info->stride;
	      dst_current -= dst_info->stride;
	    }
	  else
	    {
	      /* Proceed to the next row.  */
	      src_current += src_info->stride;
	      dst_current += dst_info->stride;
	    }
	}
    }
  else
    {
      /* Adjust the source and destination Y.  The start is MAX
         (dst_y, gc->clip_y_origin); the difference between that value
         and dst_y is the offset to apply to src_y. */

      temp    = dst_y;
      dst_y   = MAX (dst_y, gc->clip_y_origin);
      src_y  += dst_y - temp;
      height -= dst_y - temp;

      /* Verify that the bounds are correct.  */
      eassert (dst_y + height
	       <= gc->clip_y_origin + mask_info->height);
      eassert (dst_y >= gc->clip_y_origin);

      /* There is a mask.  For each scan line... */

      if (backwards)
	{
	  /* Calculate the number of pixels at the end of the
	     mask.  */

	  mask_offset  = dst_x + width;
	  mask_offset -= mask_info->width + gc->clip_x_origin;

	  if (mask_offset < 0)
	    mask_offset = 0;

	  /* Calculate the last column of the mask that will be
	     consulted.  */

	  temp = dst_x - gc->clip_x_origin;
	  temp += MIN (mask_info->width - temp,
		       width - mask_offset);

	  if (temp < 0)
	    return;

	  /* Now calculate the last row of the mask that will be
	     consulted.  */
	  i = dst_y - gc->clip_y_origin + height;

	  /* Turn both into offsets.  */

	  if (ckd_mul (&offset, temp, pixel)
	      || ckd_mul (&offset1, i, mask_info->stride)
	      || ckd_add (&offset, offset, offset1)
	      || ckd_add (&start, (uintptr_t) mask, offset))
	    return;

	  if (height <= 0)
	    return;

	  mask = mask_current = (unsigned char *) start;

	  while (height--)
	    {
	      /* Skip backwards past the end of the mask.  */

	      long_src = (unsigned int *) (src_current - mask_offset * pixel);
	      long_dst = (unsigned int *) (dst_current - mask_offset * pixel);
	      mask = mask_current;

	      /* For each pixel covered by the mask... */
	      temp = MIN (mask_info->width - temp, width - mask_offset);
	      while (temp--)
		{
		  /* Copy the destination it to the source, masked by
		     the mask.  */

		  /* Sign extend the mask.  */
		  i = *(signed char *) mask--;

		  /* Apply the mask.  */
		  *long_dst = ((*long_src & i) | (*long_dst & ~i));

		  long_dst--;
		  long_src--;
		}

	      /* Return to the last row.  */
	      src_current -= src_info->stride;
	      dst_current -= dst_info->stride;
	      mask_current -= mask_info->stride;
	    }
	}
      else
	{
	  /* Calculate the first column of the mask that will be
	     consulted.  */

	  mask_offset = dst_x - gc->clip_x_origin;

	  /* Adjust the mask by that much.  */

	  if (mask_offset > 0)
	    mask += mask_offset;
	  else
	    {
	      /* Offset src and dst by the mask offset.  */
	      src_current += -mask_offset * pixel;
	      dst_current += -mask_offset * pixel;
	      width += mask_offset;
	    }

	  /* Make sure it's not out of bounds.  */

	  eassert (dst_y - gc->clip_y_origin >= 0);
	  if ((dst_y - gc->clip_y_origin) + height > mask_info->height
	      || width <= 0)
	    return;

	  /* Now move mask to the position of the first row.  */

	  mask += ((dst_y - gc->clip_y_origin)
		   * mask_info->stride);

	  /* Determine how many bytes need to be copied.  */

	  if (mask_offset > 0)
	    temp = MIN (mask_info->width - mask_offset, width);
	  else
	    temp = MIN (mask_info->width, width);

	  if (temp <= 0 || height <= 0)
	    return;

	  /* Copy bytes according to the mask.  */

	  while (height--)
	    {
	      long_src = (unsigned int *) src_current;
	      long_dst = (unsigned int *) dst_current;
	      mask_current = mask;

#ifndef __aarch64__
	      for (j = 0; j < temp; ++j)
		{
		  /* Sign extend the mask.  */
		  i = *(signed char *) mask_current++;

		  /* Apply the mask.  */
		  *long_dst = ((*long_src & i) | (*long_dst & ~i));
		  long_dst++;
		  long_src++;
		}
#else /* __aarch64__ */
	      android_neon_mask_line (long_src, long_dst, mask, temp);
#endif /* __aarch64__ */

	      src_current += src_info->stride;
	      dst_current += dst_info->stride;
	      mask	  += mask_info->stride;
	    }
	}
    }
}


void
android_copy_area (android_drawable src, android_drawable dest,
		   struct android_gc *gc, int src_x, int src_y,
		   unsigned int width, unsigned int height,
		   int dest_x, int dest_y)
{
  jobject src_object, dest_object, mask;
  android_blit_func do_blit;
  AndroidBitmapInfo src_info, dest_info, mask_info;
  void *src_data, *dest_data, *mask_data;
  int n_clip_rects, i;
  bool flag;
  struct android_rectangle bounds, rect, temp, *clip_rectangles;

  /* Perform the copy.  Loop over each clip rectangle, unless none are
     set.  Also, obtain bitmaps for src and dst, and possibly the mask
     as well if it is present.  */

  src_data = android_lock_bitmap (src, &src_info, &src_object);
  if (!src_data)
    return;

  mask_data = mask = NULL;

  if (src != dest)
    {
      dest_data = android_lock_bitmap (dest, &dest_info, &dest_object);
      if (!dest_data)
	goto fail;
    }
  else
    {
      dest_data = src_data;
      dest_info = src_info;
    }

  /* Obtain the bitmap for the mask if necessary.  */

  if (gc->clip_mask)
    {
      mask_data = android_lock_bitmap (gc->clip_mask,
				       &mask_info, &mask);
      if (!mask_data)
	goto fail1;
    }

  /* Calculate the number of clip rectangles.  */
  n_clip_rects = gc->num_clip_rects;

  /* If n_clip_rects is -1, then no clipping is in effect.  Set rect
     to the bounds of the destination.  */

  flag = n_clip_rects == -1;
  if (flag)
    {
      n_clip_rects = 1;
      clip_rectangles = &rect;
    }
  else if (!n_clip_rects)
    goto fail2;
  else
    clip_rectangles = gc->clip_rects;

  /* Set rect to the bounds of the destination.  */

  rect.x = 0;
  rect.y = 0;
  rect.width = dest_info.width;
  rect.height = dest_info.height;

  if (mask_data)
    {
      /* Clip width and height to that of the mask.  */

      if (src_x + width > mask_info.width)
	width = mask_info.width - src_x;

      if (src_y + height > mask_info.height)
	height = mask_info.height - src_y;
    }

  /* Clip width and height to that of the source.  */

  if (src_x + width > src_info.width)
    width = src_info.width - src_x;

  if (src_y + height > src_info.height)
    height = src_info.height - src_y;

  /* Return if the copy is outside the source.  */

  if (width <= 0 || height <= 0)
    goto fail2;

  /* Look up the right function for the alu.  */

  switch (gc->function)
    {
    case ANDROID_GC_COPY:
      do_blit = android_blit_copy;
      break;

 /* case ANDROID_GC_INVERT: */
   /* do_blit = android_blit_invert; */
      /* A GC with its operation set to ANDROID_GC_INVERT is never given
	 to CopyArea.  */
    default:
      emacs_abort ();
    }

  /* Load the bounds of the destination rectangle.  */
  bounds.x = dest_x;
  bounds.y = dest_y;
  bounds.width = width;
  bounds.height = height;

  /* For each clip rectangle... */
  for (i = 0; i < n_clip_rects; ++i)
    {
      /* Calculate its intersection with the destination
	 rectangle.  */

      if (!gui_intersect_rectangles (&clip_rectangles[i], &bounds,
				     &temp))
	continue;

      /* And that of the destination itself.  */

      if (!flag && !gui_intersect_rectangles (&temp, &rect, &temp))
	continue;

      /* Now perform the copy.  */
      (*do_blit) (src_x + temp.x - dest_x,	/* temp.x relative to src_x */
		  src_y + temp.y - dest_y,	/* temp.y relative to src_y */
		  temp.width,			/* Width of area to copy.  */
		  temp.height,			/* Height of area to copy.  */
		  temp.x, temp.y,		/* Coordinates to copy to.  */
		  gc,				/* GC.  */
		  src_data, &src_info,		/* Source drawable.  */
		  dest_data, &dest_info,	/* Destination drawable.  */
		  mask_data, &mask_info);	/* Mask drawable.  */
    }

  /* Now damage the destination drawable accordingly, should it be a
     window.  */

  if ((*android_java_env)->IsInstanceOf (android_java_env,
					 (jobject) dest,
					 window_class.class))
    android_damage_window (dest, &bounds);

 fail2:
  if (mask)
    {
      AndroidBitmap_unlockPixels (android_java_env, mask);
      ANDROID_DELETE_LOCAL_REF (mask);
    }
 fail1:
  if (src != dest)
    {
      AndroidBitmap_unlockPixels (android_java_env, dest_object);
      ANDROID_DELETE_LOCAL_REF (dest_object);
    }
 fail:
  AndroidBitmap_unlockPixels (android_java_env, src_object);
  ANDROID_DELETE_LOCAL_REF (src_object);
}



void
android_free_pixmap (android_pixmap pixmap)
{
  android_destroy_handle (pixmap);
}

void
android_set_background (struct android_gc *gc, unsigned long background)
{
  struct android_gc_values gcv;

  gcv.background = background;
  android_change_gc (gc, ANDROID_GC_BACKGROUND, &gcv);
}

void
android_fill_polygon (android_drawable drawable, struct android_gc *gc,
		      struct android_point *points, int npoints,
		      enum android_shape shape, enum android_coord_mode mode)
{
  jobjectArray array;
  jobject point, drawable_object, gcontext;
  int i;

  drawable_object = android_resolve_handle (drawable);
  gcontext = android_resolve_handle (gc->gcontext);

  array = (*android_java_env)->NewObjectArray (android_java_env,
					       npoints,
					       point_class.class,
					       NULL);
  android_exception_check ();

  for (i = 0; i < npoints; ++i)
    {
      point = (*android_java_env)->NewObject (android_java_env,
					      point_class.class,
					      point_class.constructor,
					      (jint) points[i].x,
					      (jint) points[i].y);
      android_exception_check_1 (array);

      (*android_java_env)->SetObjectArrayElement (android_java_env,
						  array, i, point);
      ANDROID_DELETE_LOCAL_REF (point);
    }

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.fill_polygon,
						 drawable_object,
						 gcontext, array);
  android_exception_check_1 (array);
  ANDROID_DELETE_LOCAL_REF (array);
}

void
android_draw_rectangle (android_drawable handle, struct android_gc *gc,
			int x, int y, unsigned int width, unsigned int height)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle (handle);
  gcontext = android_resolve_handle (gc->gcontext);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.draw_rectangle,
						 drawable, gcontext,
						 (jint) x, (jint) y,
						 (jint) width, (jint) height);

  /* In lieu of android_exception_check, clear all exceptions after
     calling this frequently called graphics operation.  */
  (*android_java_env)->ExceptionClear (android_java_env);
}

void
android_draw_point (android_drawable handle, struct android_gc *gc,
		    int x, int y)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle (handle);
  gcontext = android_resolve_handle (gc->gcontext);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.draw_point,
						 drawable, gcontext,
						 (jint) x, (jint) y);

  /* In lieu of android_exception_check, clear all exceptions after
     calling this frequently called graphics operation.  */
  (*android_java_env)->ExceptionClear (android_java_env);
}

void
android_draw_line (android_drawable handle, struct android_gc *gc,
		   int x, int y, int x2, int y2)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle (handle);
  gcontext = android_resolve_handle (gc->gcontext);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.draw_line,
						 drawable, gcontext,
						 (jint) x, (jint) y,
						 (jint) x2, (jint) y2);

  /* In lieu of android_exception_check, clear all exceptions after
     calling this frequently called graphics operation.  */
  (*android_java_env)->ExceptionClear (android_java_env);
}

android_pixmap
android_create_pixmap (unsigned int width, unsigned int height,
		       int depth)
{
  jobject object;

  object = (*android_java_env)->NewObject (android_java_env,
					   pixmap_class.class,
					   pixmap_class.constructor_mutable,
					   (jint) width, (jint) height,
					   (jint) depth);
  android_exception_check ();
  return android_globalize_reference (object);
}

void
android_set_ts_origin (struct android_gc *gc, int x, int y)
{
  struct android_gc_values gcv;

  gcv.ts_x_origin = x;
  gcv.ts_y_origin = y;
  android_change_gc (gc, (ANDROID_GC_TILE_STIP_X_ORIGIN
			  | ANDROID_GC_TILE_STIP_Y_ORIGIN),
		     &gcv);
}

void
android_clear_area (android_window handle, int x, int y,
		    unsigned int width, unsigned int height)
{
  jobject window;

  window = android_resolve_handle (handle);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 window_class.clear_area,
						 (jint) x, (jint) y,
						 (jint) width, (jint) height);
}

android_pixmap
android_create_bitmap_from_data (char *bits, unsigned int width,
				 unsigned int height)
{
  return android_create_pixmap_from_bitmap_data (bits, width, height,
						 1, 0, 1);
}

struct android_image *
android_create_image (unsigned int depth, enum android_image_format format,
		      char *data, unsigned int width, unsigned int height)
{
  struct android_image *image;

  image = xmalloc (sizeof *image);

  /* Fill in the fields required by image.c.  N.B. that
     android_destroy_image ostensibly will free data, but image.c
     mostly sets and frees data itself.  */
  image->width = width;
  image->height = height;
  image->data = data;
  image->depth = depth;
  image->format = format;

  /* Now fill in the image dimensions.  There are only two depths
     supported by this function.  */

  if (depth == 1)
    {
      image->bytes_per_line = (width + 7) / 8;
      image->bits_per_pixel = 1;
    }
  else if (depth == 24)
    {
      image->bytes_per_line = width * 4;
      image->bits_per_pixel = 32;
    }
  else
    emacs_abort ();

  return image;
}

void
android_destroy_image (struct android_image *ximg)
{
  /* If XIMG->data is NULL, then it has already been freed by
     image.c.  */

  if (ximg->data)
    xfree (ximg->data);
  xfree (ximg);
}

void
android_put_pixel (struct android_image *ximg, int x, int y,
		   unsigned long pixel)
{
  char *byte, *word;
  unsigned int r, g, b;
  unsigned int pixel_int;

  /* Ignore out-of-bounds accesses.  */

  if (x >= ximg->width || y >= ximg->height || x < 0 || y < 0)
    return;

  switch (ximg->depth)
    {
    case 1:
      byte = ximg->data + y * ximg->bytes_per_line + x / 8;

      if (pixel)
	*byte |= (1 << x % 8);
      else
	*byte &= ~(1 << x % 8);
      break;

    case 24:
      /* Unaligned accesses are problematic on Android devices.  */
      word = ximg->data + y * ximg->bytes_per_line + x * 4;

      /* Swizzle the pixel into ABGR format.  Android uses Skia's
	 ``native color type'', which is ABGR.  This is despite the
	 format being named ``ARGB'', and more confusingly
	 `ANDROID_BITMAP_FORMAT_RGBA_8888' in bitmap.h.  */
      r = pixel & 0x00ff0000;
      g = pixel & 0x0000ff00;
      b = pixel & 0x000000ff;
      pixel = (r >> 16) | g | (b << 16) | 0xff000000;

      pixel_int = pixel;
      memcpy (word, &pixel_int, sizeof pixel_int);
      break;
    }
}

unsigned long
android_get_pixel (struct android_image *ximg, int x, int y)
{
  char *byte, *word;
  unsigned int pixel, r, g, b;

  if (x >= ximg->width || y >= ximg->height
      || x < 0 || y < 0)
    return 0;

  switch (ximg->depth)
    {
    case 1:
      byte = ximg->data + y * ximg->bytes_per_line + x / 8;
      return (*byte & (1 << x % 8)) ? 1 : 0;

    case 24:
      word = ximg->data + y * ximg->bytes_per_line + x * 4;
      memcpy (&pixel, word, sizeof pixel);

      /* Convert the pixel back to RGB.  */
      b = pixel & 0x00ff0000;
      g = pixel & 0x0000ff00;
      r = pixel & 0x000000ff;
      pixel = ((r << 16) | g | (b >> 16)) & ~0xff000000;

      return pixel;
    }

  emacs_abort ();
}

struct android_image *
android_get_image (android_drawable handle,
		   enum android_image_format format)
{
  jobject drawable, bitmap;
  AndroidBitmapInfo bitmap_info;
  size_t byte_size;
  void *data;
  struct android_image *image;
  unsigned char *data1, *data2;
  int i, x;

  drawable = android_resolve_handle (handle);

  /* Look up the drawable and get the bitmap corresponding to it.
     Then, lock the bitmap's bits.  */
  bitmap = (*android_java_env)->CallObjectMethod (android_java_env,
						  drawable,
						  drawable_class.get_bitmap);
  android_exception_check ();

  /* Clear the bitmap info structure.  */
  memset (&bitmap_info, 0, sizeof bitmap_info);

  /* The NDK doc seems to imply this function can fail but doesn't say
     what value it gives when it does! */
  AndroidBitmap_getInfo (android_java_env, bitmap, &bitmap_info);

  if (!bitmap_info.stride)
    {
      ANDROID_DELETE_LOCAL_REF (bitmap);
      memory_full (0);
    }

  /* Compute how big the image data will be.  Fail if it would be too
     big.  */

  if (bitmap_info.format != ANDROID_BITMAP_FORMAT_A_8)
    {
      if (ckd_mul (&byte_size,
		   (size_t) bitmap_info.stride,
		   (size_t) bitmap_info.height))
	{
	  ANDROID_DELETE_LOCAL_REF (bitmap);
	  memory_full (0);
	}
    }
  else
    /* This A8 image will be packed into A1 later on.  */
    byte_size = (bitmap_info.width + 7) / 8;

  /* Lock the image data.  Once again, the NDK documentation says the
     call can fail, but does not say how to determine whether or not
     it has failed, nor how the address is aligned.  */
  data = NULL;
  AndroidBitmap_lockPixels (android_java_env, bitmap, &data);

  if (!data)
    {
      /* Take a NULL pointer to mean that AndroidBitmap_lockPixels
	 failed.  */
      ANDROID_DELETE_LOCAL_REF (bitmap);
      memory_full (0);
    }

  /* Copy the data into a new struct android_image.  */
  image = xmalloc (sizeof *image);
  image->width = bitmap_info.width;
  image->height = bitmap_info.height;
  image->data = malloc (byte_size);

  if (!image->data)
    {
      ANDROID_DELETE_LOCAL_REF (bitmap);
      xfree (image);
      memory_full (byte_size);
    }

  /* Use the format of the bitmap to determine the image depth.  */
  switch (bitmap_info.format)
    {
    case ANDROID_BITMAP_FORMAT_RGBA_8888:
      image->depth = 24;
      image->bits_per_pixel = 32;
      break;

      /* A8 images are used by Emacs to represent bitmaps.  They have
	 to be packed manually.  */
    case ANDROID_BITMAP_FORMAT_A_8:
      image->depth = 1;
      image->bits_per_pixel = 1;
      break;

      /* Other formats are currently not supported.  */
    default:
      emacs_abort ();
    }

  image->format = format;

  if (image->depth == 24)
    {
      image->bytes_per_line = bitmap_info.stride;

      /* Copy the bitmap data over.  */
      memcpy (image->data, data, byte_size);
    }
  else
    {
      /* Pack the A8 image data into bits manually.  */
      image->bytes_per_line = (image->width + 7) / 8;

      data1 = (unsigned char *) image->data;
      data2 = data;

      for (i = 0; i < image->height; ++i)
	{
	  for (x = 0; x < image->width; ++x)
	    /* Some bits in data1 might be initialized at this point,
	       but they will all be set properly later.  */
	    data1[x / 8] = (data2[x]
			    ? (data1[x / 8] | (1 << (x % 8)))
			    : (data1[x / 8] & ~(1 << (x % 8))));

	  data1 += image->bytes_per_line;
	  data2 += bitmap_info.stride;
	}
    }

  /* Unlock the bitmap pixels.  */
  AndroidBitmap_unlockPixels (android_java_env, bitmap);

  /* Delete the bitmap reference.  */
  ANDROID_DELETE_LOCAL_REF (bitmap);
  return image;
}

void
android_put_image (android_pixmap handle, struct android_image *image)
{
  jobject drawable, bitmap;
  AndroidBitmapInfo bitmap_info;
  void *data;
  unsigned char *data_1, *data_2;
  int i, x;

  drawable = android_resolve_handle (handle);

  /* Look up the drawable and get the bitmap corresponding to it.
     Then, lock the bitmap's bits.  */
  bitmap = (*android_java_env)->CallObjectMethod (android_java_env,
						  drawable,
						  drawable_class.get_bitmap);
  android_exception_check ();

  /* Clear the bitmap info structure.  */
  memset (&bitmap_info, 0, sizeof bitmap_info);

  /* The NDK doc seems to imply this function can fail but doesn't say
     what value it gives when it does! */
  AndroidBitmap_getInfo (android_java_env, bitmap, &bitmap_info);

  if (!bitmap_info.stride)
    {
      ANDROID_DELETE_LOCAL_REF (bitmap);
      memory_full (0);
    }

  if (bitmap_info.width != image->width
      || bitmap_info.height != image->height)
    /* This is not yet supported.  */
    emacs_abort ();

  /* Make sure the bitmap formats are compatible with each other.  */

  if ((image->depth == 24
       && bitmap_info.format != ANDROID_BITMAP_FORMAT_RGBA_8888)
      || (image->depth == 1
	  && bitmap_info.format != ANDROID_BITMAP_FORMAT_A_8))
    emacs_abort ();

  /* Lock the image data.  Once again, the NDK documentation says the
     call can fail, but does not say how to determine whether or not
     it has failed, nor how the address is aligned.  */
  data = NULL;
  AndroidBitmap_lockPixels (android_java_env, bitmap, &data);

  if (!data)
    {
      /* Take a NULL pointer to mean that AndroidBitmap_lockPixels
	 failed.  */
      ANDROID_DELETE_LOCAL_REF (bitmap);
      memory_full (0);
    }

  data_1 = data;
  data_2 = (unsigned char *) image->data;

  /* Copy the bitmap data over scanline-by-scanline.  */
  for (i = 0; i < image->height; ++i)
    {
      if (image->depth != 1)
	memcpy (data_1, data_2,
		image->width * (image->bits_per_pixel / 8));
      else
	{
	  /* Android internally uses a 1 byte-per-pixel format for
	     ALPHA_8 images.  Expand the image from the 1
	     bit-per-pixel X format correctly.  */

	  for (x = 0; x < image->width; ++x)
	    data_1[x] = (data_2[x / 8] & (1 << x % 8)) ? 0xff : 0;
	}

      data_1 += bitmap_info.stride;
      data_2 += image->bytes_per_line;
    }

  /* Unlock the bitmap pixels.  */
  AndroidBitmap_unlockPixels (android_java_env, bitmap);

  /* Delete the bitmap reference.  */
  ANDROID_DELETE_LOCAL_REF (bitmap);
}

void
android_bell (void)
{
  jint duration;

  /* Restrict android_keyboard_bell_duration to values between 10 and
     1000.  */
  duration = MIN (1000, MAX (0, android_keyboard_bell_duration));

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.ring_bell,
						 duration);
  android_exception_check ();
}

void
android_set_input_focus (android_window handle, unsigned long time)
{
  jobject window;
  jmethodID make_input_focus;

  window = android_resolve_handle (handle);
  make_input_focus = window_class.make_input_focus;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 make_input_focus,
						 (jlong) time);
  android_exception_check ();
}

void
android_raise_window (android_window handle)
{
  jobject window;
  jmethodID raise;

  window = android_resolve_handle (handle);
  raise = window_class.raise;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 raise);
  android_exception_check ();
}

void
android_lower_window (android_window handle)
{
  jobject window;
  jmethodID lower;

  window = android_resolve_handle (handle);
  lower = window_class.lower;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 lower);
  android_exception_check ();
}

void
android_reconfigure_wm_window (android_window handle,
			       enum android_wc_value_mask value_mask,
			       struct android_window_changes *values)
{
  jobject sibling, window;

  window = android_resolve_handle (handle);

  if (!(value_mask & ANDROID_CW_STACK_MODE))
    return;

  /* If value_mask & ANDROID_CW_SIBLING, place HANDLE above or below
     values->sibling pursuant to values->stack_mode; else, reposition
     it at the top or the bottom of its parent.  */

  sibling = NULL;

  if (value_mask & ANDROID_CW_SIBLING)
    sibling = android_resolve_handle (values->sibling);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 window_class.reconfigure,
						 sibling,
						 (jint) values->stack_mode);
  android_exception_check ();
}

int
android_query_tree (android_window handle, android_window *root_return,
		    android_window *parent_return,
		    android_window **children_return,
		    unsigned int *nchildren_return)
{
  jobject window, array;
  jsize nelements, i;
  android_window *children;
  jlong *longs;
  jmethodID method;

  window = android_resolve_handle (handle);

  /* window can be NULL, so this is a service method.  */
  method = service_class.query_tree;
  array
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method, window);
  android_exception_check ();

  /* The first element of the array is the parent window.  The rest
     are the children.  */
  nelements = (*android_java_env)->GetArrayLength (android_java_env,
						   array);
  eassert (nelements);

  /* Now fill in the children.  */
  children = xnmalloc (nelements - 1, sizeof *children);

  longs
    = (*android_java_env)->GetLongArrayElements (android_java_env, array,
						 NULL);
  android_exception_check_nonnull (longs, array);

  for (i = 1; i < nelements; ++i)
    /* Subtract one from the index into children, since the parent is
       not included.  */
    children[i - 1] = longs[i];

  /* Finally, return the parent and other values.  */
  *root_return = 0;
  *parent_return = longs[0];
  *children_return = children;
  *nchildren_return = nelements - 1;

  /* Release the array contents.  */
  (*android_java_env)->ReleaseLongArrayElements (android_java_env, array,
						 longs, JNI_ABORT);

  ANDROID_DELETE_LOCAL_REF (array);
  return 1;
}

void
android_get_geometry (android_window handle,
		      android_window *root_return,
		      int *x_return, int *y_return,
		      unsigned int *width_return,
		      unsigned int *height_return,
		      unsigned int *border_width_return)
{
  jobject window;
  jarray window_geometry;
  jmethodID get_geometry;
  jint *ints;

  window = android_resolve_handle (handle);
  get_geometry = window_class.get_window_geometry;

  window_geometry
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       window,
						       window_class.class,
						       get_geometry);
  android_exception_check ();

  /* window_geometry is an array containing x, y, width and
     height.  border_width is always 0 on Android.  */
  eassert ((*android_java_env)->GetArrayLength (android_java_env,
						window_geometry)
	   == 4);

  *root_return = 0;
  *border_width_return = 0;

  ints
    = (*android_java_env)->GetIntArrayElements (android_java_env,
						window_geometry,
						NULL);
  android_exception_check_nonnull (ints, window_geometry);

  *x_return = ints[0];
  *y_return = ints[1];
  *width_return = ints[2];
  *height_return = ints[3];

  (*android_java_env)->ReleaseIntArrayElements (android_java_env,
						window_geometry,
						ints, JNI_ABORT);

  /* Now free the local reference.  */
  ANDROID_DELETE_LOCAL_REF (window_geometry);
}

void
android_move_resize_window (android_window handle, int x, int y,
			    unsigned int width, unsigned int height)
{
  jobject window;
  jmethodID move_resize_window;

  window = android_resolve_handle (handle);
  move_resize_window = window_class.move_resize_window;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 move_resize_window,
						 (jint) x, (jint) y,
						 (jint) width,
						 (jint) height);
  android_exception_check ();
}

void
android_map_raised (android_window window)
{
  android_raise_window (window);
  android_map_window (window);
}

void
android_translate_coordinates (android_window src, int x,
			       int y, int *root_x, int *root_y)
{
  jobject window;
  jarray coordinates;
  jmethodID method;
  jint *ints;

  window = android_resolve_handle (src);
  method = window_class.translate_coordinates;
  coordinates
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       window,
						       window_class.class,
						       method, (jint) x,
						       (jint) y);
  android_exception_check ();

  /* The array must contain two elements: X, Y translated to the root
     window.  */
  eassert ((*android_java_env)->GetArrayLength (android_java_env,
						coordinates)
	   == 2);

  /* Obtain the coordinates from the array.  */
  ints = (*android_java_env)->GetIntArrayElements (android_java_env,
						   coordinates, NULL);
  android_exception_check_nonnull (ints, coordinates);

  *root_x = ints[0];
  *root_y = ints[1];

  /* Release the coordinates.  */
  (*android_java_env)->ReleaseIntArrayElements (android_java_env,
						coordinates, ints,
						JNI_ABORT);

  /* And free the local reference.  */
  ANDROID_DELETE_LOCAL_REF (coordinates);
}

/* Return the character produced by combining the diacritic character
   DCHAR with the key-producing character C in *VALUE.  Value is 1 if
   there is no character for this combination, 0 otherwise.  */

static int
android_get_dead_char (unsigned int dchar, unsigned int c,
		       unsigned int *value)
{
  jmethodID method;
  jclass class;
  jint result;

  /* Call getDeadChar.  */
  class = key_character_map_class.class;
  method = key_character_map_class.get_dead_char;
  result = (*android_java_env)->CallStaticIntMethod (android_java_env,
						     class, method,
						     (jint) dchar,
						     (jint) c);

  if (result)
    {
      *value = result;
      return 0;
    }

  return 1;
}

/* Return a Unicode string in BUFFER_RETURN, a buffer of size
   WCHARS_BUFFER, from the key press event EVENT, much like
   XmbLookupString.  If EVENT represents a key press without a
   corresponding Unicode character, return its keysym in *KEYSYM_RETURN.
   Return the action taken in *STATUS_RETURN.

   COMPOSE_STATUS, if non-NULL, should point to a structure for
   temporary information to be stored in during dead key
   composition.  */

int
android_wc_lookup_string (android_key_pressed_event *event,
			  wchar_t *buffer_return, int wchars_buffer,
			  int *keysym_return,
			  enum android_lookup_status *status_return,
			  struct android_compose_status *compose_status)
{
  enum android_lookup_status status;
  int rc;
  jobject window, string;
  const jchar *characters;
  jsize size;
  size_t i;
  JNIEnv *env;
  unsigned int unicode_char;

  env = android_java_env;
  status = ANDROID_LOOKUP_NONE;
  rc = 0;

  /* See if an actual lookup has to be made.  Note that while
     BUFFER_RETURN is wchar_t, the returned characters are always in
     UCS.  */

  if (event->unicode_char != (uint32_t) -1)
    {
      if (event->unicode_char)
	{
	  /* KeyCharacterMap.COMBINING_ACCENT.  */
	  if ((event->unicode_char & 0x80000000) && compose_status)
	    goto dead_key;

	  /* Remove combining accent bits.  */
	  unicode_char = event->unicode_char & ~0x80000000;

	  if (wchars_buffer < 1)
	    {
	      *status_return = ANDROID_BUFFER_OVERFLOW;
	      return 0;
	    }
	  else
	    {
	      /* If COMPOSE_STATUS holds a diacritic mark unicode_char
		 ought to be combined with, and this combination is
		 valid, return the result alone with no keysym.  */

	      if (compose_status
		  && compose_status->chars_matched
		  && !android_get_dead_char (compose_status->accent,
					     unicode_char,
					     &unicode_char))
		{
		  buffer_return[0] = unicode_char;
		  *status_return = ANDROID_LOOKUP_CHARS;
		  compose_status->chars_matched = 0;
		  return 1;
		}
	      else if (compose_status && compose_status->chars_matched)
		{
		  /* If the combination is valid the compose status must
		     be reset and no character returned.  */
		  compose_status->chars_matched = 0;
		  status = ANDROID_LOOKUP_NONE;
		  return 0;
		}

	      buffer_return[0] = unicode_char;
	      status = ANDROID_LOOKUP_CHARS;
	      rc = 1;
	    }
	}

      *keysym_return = event->keycode;

      if (status == ANDROID_LOOKUP_CHARS)
	status = ANDROID_LOOKUP_BOTH;
      else
	{
	  status = ANDROID_LOOKUP_KEYSYM;
	  rc = 0;
	}

      /* Terminate any ongoing character composition after a key is
	 registered.  */
      if (compose_status
	  /* Provided that a modifier key is not the key being
	     depressed.  */
	  && !ANDROID_IS_MODIFIER_KEY (event->keycode))
	compose_status->chars_matched = 0;
      *status_return = status;
      return rc;
    }

  /* Now look up the window.  */
  rc = 0;

  window = android_resolve_handle (event->window);
  string
    = (*env)->CallNonvirtualObjectMethod (env, window,
					  window_class.class,
					  window_class.lookup_string,
					  (jint) event->serial);
  android_exception_check ();

  if (!string)
    status = ANDROID_LOOKUP_NONE;
  else
    {
      /* Now return this input method string.  */
      characters = (*env)->GetStringChars (env, string, NULL);
      android_exception_check_nonnull ((void *) characters, string);

      /* Establish the size of the string.  */
      size = (*env)->GetStringLength (env, string);

      /* Copy over the string data.  */
      for (i = 0; i < MIN ((unsigned int) wchars_buffer, size); ++i)
	buffer_return[i] = characters[i];

      if (i < size)
	status = ANDROID_BUFFER_OVERFLOW;
      else
	status = ANDROID_LOOKUP_CHARS;

      /* Return the number of characters that should have been
	 written.  */

      if (size > INT_MAX)
	rc = INT_MAX;
      else
	rc = size;

      (*env)->ReleaseStringChars (env, string, characters);
      ANDROID_DELETE_LOCAL_REF (string);
    }

  *status_return = status;
  return rc;

 dead_key:
  /* event->unicode_char is a dead key, which are diacritic marks that
     should not be directly inserted but instead be combined with a
     subsequent character before insertion.  */
  *status_return = ANDROID_LOOKUP_NONE;
  compose_status->chars_matched = 1;
  compose_status->accent = event->unicode_char & ~0x80000000;
  return 0;
}



/* Low level drawing primitives.  */

/* Lock the bitmap corresponding to the drawable DRAWABLE.  Return the
   bitmap data upon success, and store the bitmap object in
   BITMAP_RETURN.  Value is NULL upon failure.

   The caller must take care to unlock the bitmap data afterwards.  */

unsigned char *
android_lock_bitmap (android_drawable drawable,
		     AndroidBitmapInfo *bitmap_info,
		     jobject *bitmap_return)
{
  jobject object, bitmap;
  void *data;

  object = android_resolve_handle (drawable);

  /* Look up the drawable and get the bitmap corresponding to it.
     Then, lock the bitmap's bits.  */
  bitmap = (*android_java_env)->CallObjectMethod (android_java_env,
						  object,
						  drawable_class.get_bitmap);
  if (!bitmap)
    {
      /* Report any exception signaled.  */
      android_exception_check ();

      /* If no exception was signaled, then NULL was returned as the
	 bitmap does not presently exist due to window reconfiguration
	 on the main thread.  */
      return NULL;
    }

  memset (bitmap_info, 0, sizeof *bitmap_info);

  /* Get the bitmap info.  */
  AndroidBitmap_getInfo (android_java_env, bitmap, bitmap_info);

  if (!bitmap_info->stride)
    {
      ANDROID_DELETE_LOCAL_REF (bitmap);
      return NULL;
    }

  /* Now lock the image data.  */
  data = NULL;
  AndroidBitmap_lockPixels (android_java_env, bitmap, &data);

  if (!data)
    {
      ANDROID_DELETE_LOCAL_REF (bitmap);
      return NULL;
    }

  /* Give the bitmap to the caller.  */
  *bitmap_return = bitmap;

  /* The bitmap data is now locked.  */
  return data;
}

/* Damage the window HANDLE by the given damage rectangle.  */

void
android_damage_window (android_drawable handle,
		       struct android_rectangle *damage)
{
  jobject drawable;

  drawable = android_resolve_handle (handle);

  /* Post the damage to the drawable.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 drawable,
						 window_class.class,
						 window_class.damage_rect,
						 (jint) damage->x,
						 (jint) damage->y,
						 (jint) (damage->x
							 + damage->width),
						 (jint) (damage->y
							 + damage->height));
  android_exception_check ();
}



/* Other misc system routines.  */

int
android_get_screen_width (void)
{
  int rc;
  jmethodID method;

  method = service_class.get_screen_width;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method,
						     (jboolean) false);
  android_exception_check ();
  return rc;
}

int
android_get_screen_height (void)
{
  int rc;
  jmethodID method;

  method = service_class.get_screen_height;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method,
						     (jboolean) false);
  android_exception_check ();
  return rc;
}

int
android_get_mm_width (void)
{
  int rc;
  jmethodID method;

  method = service_class.get_screen_width;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method,
						     (jboolean) true);
  android_exception_check ();
  return rc;
}

int
android_get_mm_height (void)
{
  int rc;
  jmethodID method;

  method = service_class.get_screen_height;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method,
						     (jboolean) true);
  android_exception_check ();
  return rc;
}

bool
android_detect_mouse (void)
{
  bool rc;
  jmethodID method;

  method = service_class.detect_mouse;
  rc = (*android_java_env)->CallNonvirtualBooleanMethod (android_java_env,
							 emacs_service,
							 service_class.class,
							 method);
  android_exception_check ();
  return rc;
}

bool
android_detect_keyboard (void)
{
  bool rc;
  jmethodID method;

  method = service_class.detect_keyboard;
  rc = (*android_java_env)->CallNonvirtualBooleanMethod (android_java_env,
							 emacs_service,
							 service_class.class,
							 method);
  android_exception_check ();
  return rc;
}

void
android_set_dont_focus_on_map (android_window handle,
			       bool no_focus_on_map)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle);
  method = window_class.set_dont_focus_on_map;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, window,
						 window_class.class,
						 method,
						 (jboolean) no_focus_on_map);
  android_exception_check ();
}

void
android_set_dont_accept_focus (android_window handle,
			       bool no_accept_focus)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle);
  method = window_class.set_dont_accept_focus;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, window,
						 window_class.class,
						 method,
						 (jboolean) no_accept_focus);
  android_exception_check ();
}

/* Set the WM name of HANDLE to STRING, a Java string.  This name
   provides the task description of activities that receive HANDLE.  */

void
android_set_wm_name (android_window handle, jstring name)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle);
  method = window_class.set_wm_name;

  if (android_get_current_api_level () < 21)
    return;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, window,
						 window_class.class, method,
						 name);
  android_exception_check ();
}

void
android_get_keysym_name (int keysym, char *name_return, size_t size)
{
  jobject string;
  const char *buffer;
  jmethodID method;

  /* These keysyms are special editor actions sent by the input
     method.  */

  switch (keysym)
    {
    case 65536 + 1:
      strncpy (name_return, "select-all", size - 1);
      name_return[size] = '\0';
      return;

    case 65536 + 2:
      strncpy (name_return, "start-selecting-text", size - 1);
      name_return[size] = '\0';
      return;

    case 65536 + 3:
      strncpy (name_return, "stop-selecting-text", size - 1);
      name_return[size] = '\0';
      return;
    }

  method = service_class.name_keysym;
  string
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method,
						       (jint) keysym);
  android_exception_check ();

  if (!string)
    {
      strncpy (name_return, "stop-selecting-text", size - 1);
      name_return[size] = '\0';
      return;
    }

  buffer = (*android_java_env)->GetStringUTFChars (android_java_env,
						   (jstring) string,
						   NULL);
  android_exception_check_nonnull ((void *) buffer, string);
  strncpy (name_return, buffer, size - 1);
  name_return[size] = '\0';

  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) string,
					      buffer);
  ANDROID_DELETE_LOCAL_REF (string);
}

/* Display the on screen keyboard on window WINDOW, or hide it if SHOW
   is false.  Ask the system to bring up or hide the on-screen
   keyboard on behalf of WINDOW.  The request may be rejected by the
   system, especially when the window does not have the input
   focus.  */

void
android_toggle_on_screen_keyboard (android_window window, bool show)
{
  jobject object;
  jmethodID method;

  object = android_resolve_handle (window);
  method = window_class.toggle_on_screen_keyboard;

  /* Now display the on screen keyboard.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, object,
						 window_class.class,
						 method, (jboolean) show);

  /* Check for out of memory errors.  */
  android_exception_check ();
}



/* emacs_abort implementation for Android.  This logs a stack
   trace.  */

void
emacs_abort (void)
{
  volatile char *foo;

  __android_log_print (ANDROID_LOG_FATAL, __func__,
		       "emacs_abort called, please review the following"
		       " stack trace");

  /* Induce a NULL pointer dereference to make debuggerd generate a
     tombstone.  */
  foo = NULL;
  *foo = '\0';

  abort ();
}



/* Return whether or not TEXT, a string without multibyte
   characters, has no bytes with the 8th bit set.  */

static bool
android_check_string (Lisp_Object text)
{
  ptrdiff_t i;

  for (i = 0; i < SBYTES (text); ++i)
    {
      if (SREF (text, i) & 128)
	return false;
    }

  return true;
}

/* Verify that the specified NULL-terminated STRING is a valid JNI
   ``UTF-8'' string.  Return 0 if so, 1 otherwise.

   Do not perform GC, enabling NAME to be a direct reference to string
   data.

   The native coding system used by the JVM to store strings derives
   from UTF-8, but deviates from it in two aspects in an attempt to
   better represent the UCS-16 based Java String format, and to let
   strings contain NULL characters while remaining valid C strings:
   NULL bytes are encoded as two-byte sequences, and Unicode surrogate
   pairs encoded as two-byte sequences are preferred to four-byte
   sequences when encoding characters above the BMP.  */

int
android_verify_jni_string (const char *name)
{
  const unsigned char *chars;

  chars = (unsigned char *) name;
  while (*chars)
    {
      /* Switch on the high 4 bits.  */

      switch (*chars++ >> 4)
	{
	case 0 ... 7:
	  /* The 8th bit is clean, so this is a regular C
	     character.  */
	  break;

	case 8 ... 0xb:
	  /* Invalid starting byte! */
	  return 1;

	case 0xf:
	  /* The start of a four byte sequence.  These aren't allowed
	     in Java.  */
	  return 1;

	case 0xe:
	  /* The start of a three byte sequence.  Verify that its
	     continued.  */

	  if ((*chars++ & 0xc0) != 0x80)
	    return 1;

	  FALLTHROUGH;

	case 0xc ... 0xd:
	  /* The start of a two byte sequence.  Verify that the
	     next byte exists and has its high bit set.  */

	  if ((*chars++ & 0xc0) != 0x80)
	    return 1;

	  break;
	}
    }

  return 0;
}

/* Given a Lisp string TEXT, return a local reference to an equivalent
   Java string.  Each argument following TEXT should be NULL or a
   local reference that will be freed if creating the string fails,
   whereupon memory_full will also be signaled.  */

jstring
android_build_string (Lisp_Object text, ...)
{
  Lisp_Object encoded;
  jstring string;
  size_t nchars;
  jchar *characters;
  va_list ap;
  jobject object;

  USE_SAFE_ALLOCA;

  /* Directly encode TEXT if it contains no non-ASCII characters, or
     is multibyte and a valid Modified UTF-8 string.  This is okay
     because the Java extended UTF format is compatible with
     ASCII.  */

  if ((SBYTES (text) == SCHARS (text)
       && android_check_string (text))
      /* If TEXT is a multibyte string, then it's using Emacs's
	 internal UTF-8 coding system, a significant subset of which
	 is compatible with JNI.  */
      || (STRING_MULTIBYTE (text)
	  && !android_verify_jni_string (SSDATA (text))))
    {
      string = (*android_java_env)->NewStringUTF (android_java_env,
						  SSDATA (text));

      if ((*android_java_env)->ExceptionCheck (android_java_env))
	goto error;

      SAFE_FREE ();
      return string;
    }

  encoded = code_convert_string_norecord (text, Qutf_16le,
					  true);
  nchars = (SBYTES (encoded) / sizeof (jchar));

  /* Encode the string as UTF-16 prior to creating the string.
     Copy the string to a separate buffer in order to preserve
     alignment.  */

  characters = SAFE_ALLOCA (SBYTES (encoded));
  memcpy (characters, SDATA (encoded), SBYTES (encoded));

  /* Create the string.  */
  string
    = (*android_java_env)->NewString (android_java_env,
				      characters, nchars);

  if ((*android_java_env)->ExceptionCheck (android_java_env))
    goto error;

  SAFE_FREE ();
  return string;

 error:
  /* An exception arose while creating the string.  When this
     transpires, an assumption is made that the error was induced by
     running out of memory.  Delete each of the local references
     within AP.  */

  va_start (ap, text);

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  /* Now remove each and every local reference provided after
     OBJECT.  */

  while ((object = va_arg (ap, jobject)))
    ANDROID_DELETE_LOCAL_REF (object);

  va_end (ap);
  memory_full (0);
}

/* Do the same, except TEXT is constant string data in ASCII or
   UTF-8 containing no characters outside the Basic Multilingual
   Plane.  */

jstring
android_build_jstring (const char *text)
{
  jstring string;

  /* Note that Java expects this string to be in ``modified UTF
     encoding'', which is actually UTF-8, except with NUL
     encoded as a two-byte sequence, and surrogate pairs encoded
     in the three-byte extended encoding.  The only consequence
     of passing an actual UTF-8 string is that NUL bytes and
     characters requiring surrogate pairs cannot be represented,
     which is not really of consequence.  */

  string = (*android_java_env)->NewStringUTF (android_java_env,
					      text);
  android_exception_check ();

  return string;
}



/* Exception checking functions.  Most JNI functions which allocate
   memory return NULL upon failure; they also set the JNI
   environment's pending exception to an OutOfMemoryError.

   These functions check for such errors and call memory_full wherever
   appropriate.  Three variants are provided: one which releases no
   local references, one which releases a single local reference
   before calling memory_full, and one which releases two local
   references.

   Typically, you use these functions by calling them immediately
   after a JNI function which allocates memory, passing it any local
   references that are already valid but should be deleted after
   leaving the current scope.  For example, to allocate foo, make
   global_foo its global reference, and then release foo, you write:

     jobject foo, global_foo;

     foo = (*android_java_env)->New...;
     android_exception_check ();

     global_foo = (*android_java_env)->NewGlobalRef (..., foo);
     android_exception_check_1 (foo);
     ANDROID_DELETE_LOCAL_REF (foo);

   where the first android_exception_check ensures that foo has been
   allocated correctly, while the call to android_exception_check_1,
   and the call to ANDROID_DELETE_LOCAL_REF afterwards, together
   ensure the same of global_foo, and also that foo is released both
   if global_foo cannot be allocated, and after the global reference
   is created.  */

#define likely(cond)	__builtin_expect (cond, 1)

/* Check for JNI exceptions and call memory_full in that
   situation.  */

void
android_exception_check (void)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);
  memory_full (0);
}

/* Check for JNI exceptions.  If there is one such exception, clear
   it, then delete the local reference to OBJECT and call memory_full.
   OBJECT can be NULL, which is a valid local reference to the Java
   null object.  */

void
android_exception_check_1 (jobject object)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  if (object)
    ANDROID_DELETE_LOCAL_REF (object);

  memory_full (0);
}

/* Like android_exception_check_1, except it takes more than one local
   reference argument.  */

void
android_exception_check_2 (jobject object, jobject object1)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  if (object)
    ANDROID_DELETE_LOCAL_REF (object);

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  memory_full (0);
}

/* Like android_exception_check_2, except it takes more than two local
   reference arguments.  */

void
android_exception_check_3 (jobject object, jobject object1,
			   jobject object2)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  if (object)
    ANDROID_DELETE_LOCAL_REF (object);

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  if (object2)
    ANDROID_DELETE_LOCAL_REF (object2);

  memory_full (0);
}

/* Like android_exception_check_3, except it takes more than three
   local reference arguments.  */

void
android_exception_check_4 (jobject object, jobject object1,
			   jobject object2, jobject object3)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  if (object)
    ANDROID_DELETE_LOCAL_REF (object);

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  if (object2)
    ANDROID_DELETE_LOCAL_REF (object2);

  if (object3)
    ANDROID_DELETE_LOCAL_REF (object3);

  memory_full (0);
}

/* Like android_exception_check_4, except it takes more than four local
   reference arguments.  */

void
android_exception_check_5 (jobject object, jobject object1,
			   jobject object2, jobject object3,
			   jobject object4)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  if (object)
    ANDROID_DELETE_LOCAL_REF (object);

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  if (object2)
    ANDROID_DELETE_LOCAL_REF (object2);

  if (object3)
    ANDROID_DELETE_LOCAL_REF (object3);

  if (object4)
    ANDROID_DELETE_LOCAL_REF (object4);

  memory_full (0);
}


/* Like android_exception_check_5, except it takes more than five local
   reference arguments.  */

void
android_exception_check_6 (jobject object, jobject object1,
			   jobject object2, jobject object3,
			   jobject object4, jobject object5)
{
  if (likely (!(*android_java_env)->ExceptionCheck (android_java_env)))
    return;

  __android_log_print (ANDROID_LOG_WARN, __func__,
		       "Possible out of memory error. "
		       " The Java exception follows:  ");
  /* Describe exactly what went wrong.  */
  (*android_java_env)->ExceptionDescribe (android_java_env);
  (*android_java_env)->ExceptionClear (android_java_env);

  if (object)
    ANDROID_DELETE_LOCAL_REF (object);

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  if (object2)
    ANDROID_DELETE_LOCAL_REF (object2);

  if (object3)
    ANDROID_DELETE_LOCAL_REF (object3);

  if (object4)
    ANDROID_DELETE_LOCAL_REF (object4);

  if (object5)
    ANDROID_DELETE_LOCAL_REF (object5);

  memory_full (0);
}

/* Check for JNI problems based on the value of OBJECT.

   Signal out of memory if OBJECT is NULL.  OBJECT1 means the
   same as in `android_exception_check_1'.

   This function is useful when checking for errors from JNI
   functions that do not set exceptions on failure, such as
   `GetIntArrayElements'.  */

void
android_exception_check_nonnull (void *object, jobject object1)
{
  if (likely (object != NULL))
    return;

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  memory_full (0);
}

/* Check for JNI problems based on the value of OBJECT.

   Signal out of memory if OBJECT is NULL.  OBJECT1 and OBJECT2 mean
   the same as in `android_exception_check_2'.  */

void
android_exception_check_nonnull_1 (void *object, jobject object1,
				   jobject object2)
{
  if (likely (object != NULL))
    return;

  if (object1)
    ANDROID_DELETE_LOCAL_REF (object1);

  if (object2)
    ANDROID_DELETE_LOCAL_REF (object2);

  memory_full (0);
}



/* Native image transforms.  */

/* Transform the coordinates X and Y by the specified affine
   transformation MATRIX.  Place the result in *XOUT and *YOUT.  */

static void
android_transform_coordinates (int x, int y,
			       struct android_transform *transform,
			       float *xout, float *yout)
{
  /* Apply the specified affine transformation.
     A transform looks like:

       M1 M2 M3     X
       M4 M5 M6   * Y

       =

       M1*X + M2*Y + M3*1 = X1
       M4*X + M5*Y + M6*1 = Y1

     (In most transforms, there is another row at the bottom for
     mathematical reasons.  Since Z1 is always 1.0, the row is simply
     implied to be 0 0 1, because 0 * x + 0 * y + 1 * 1 = 1.0.  See
     the definition of matrix3x3 in image.c for some more explanations
     about this.) */

  *xout = transform->m1 * x + transform->m2 * y + transform->m3;
  *yout = transform->m4 * x + transform->m5 * y + transform->m6;
}

/* Return the interpolation of the four pixels TL, TR, BL, and BR,
   according to the weights DISTX and DISTY.  */

static unsigned int
android_four_corners_bilinear (unsigned int tl, unsigned int tr,
			       unsigned int bl, unsigned int br,
			       int distx, int disty)
{
  int distxy, distxiy, distixy, distixiy;
  uint32_t f, r;

  distxy = distx * disty;
  distxiy = (distx << 8) - distxy;
  distixy = (disty << 8) - distxy;
  distixiy = (256 * 256 - (disty << 8)
	      - (distx << 8) + distxy);

  /* Red */
  r = ((tl & 0x000000ff) * distixiy + (tr & 0x000000ff) * distxiy
       + (bl & 0x000000ff) * distixy  + (br & 0x000000ff) * distxy);

  /* Green */
  f = ((tl & 0x0000ff00) * distixiy + (tr & 0x0000ff00) * distxiy
       + (bl & 0x0000ff00) * distixy  + (br & 0x0000ff00) * distxy);
  r |= f & 0xff000000;

  /* Now do the upper two components.  */
  tl >>= 16;
  tr >>= 16;
  bl >>= 16;
  br >>= 16;
  r >>= 16;

  /* Blue */
  f = ((tl & 0x000000ff) * distixiy + (tr & 0x000000ff) * distxiy
       + (bl & 0x000000ff) * distixy  + (br & 0x000000ff) * distxy);
  r |= f & 0x00ff0000;

  /* Alpha */
  f = ((tl & 0x0000ff00) * distixiy + (tr & 0x0000ff00) * distxiy
       + (bl & 0x0000ff00) * distixy  + (br & 0x0000ff00) * distxy);
  r |= f & 0xff000000;

  return r;
}

/* Return the interpolation of the four pixels closest to at X, Y in
   IMAGE, according to weights in both axes computed from X and Y.
   IMAGE must be depth 24, or the behavior is undefined.  */

static unsigned int
android_fetch_pixel_bilinear (struct android_image *image,
			      float x, float y)
{
  int x1, y1, x2, y2;
  float distx, disty;
  unsigned int top_left, top_right;
  unsigned int bottom_left, bottom_right;
  char *word;

  /* Compute the four closest corners to X and Y.  */
  x1 = (int) x;
  x2 = x1 + 1;
  y1 = (int) y;
  y2 = y1 + 1;

  /* Make sure all four corners are within range.  */
  x1 = MAX (0, MIN (image->width - 1, x1));
  y1 = MAX (0, MIN (image->height - 1, y1));
  x2 = MAX (0, MIN (image->width - 1, x2));
  y2 = MAX (0, MIN (image->height - 1, y2));

  /* Compute the X and Y biases.  These are numbers between 0f and
     1f.  */
  distx = x - x1;
  disty = y - y1;

  /* Fetch the four closest pixels.  */
  word = image->data + y1 * image->bytes_per_line + x1 * 4;
  memcpy (&top_left, word, sizeof top_left);
  word = image->data + y1 * image->bytes_per_line + x2 * 4;
  memcpy (&top_right, word, sizeof top_right);
  word = image->data + y2 * image->bytes_per_line + x1 * 4;
  memcpy (&bottom_left, word, sizeof bottom_left);
  word = image->data + y2 * image->bytes_per_line + x2 * 4;
  memcpy (&bottom_right, word, sizeof bottom_right);

  /* Do the interpolation.  */
  return android_four_corners_bilinear (top_left, top_right, bottom_left,
					bottom_right, distx * 256,
					disty * 256);
}

/* Transform the depth 24 image IMAGE by the 3x2 affine transformation
   matrix MATRIX utilizing a bilinear filter.  Place the result in
   OUT.  The matrix maps from the coordinate space of OUT to
   IMAGE.  */

void
android_project_image_bilinear (struct android_image *image,
				struct android_image *out,
				struct android_transform *transform)
{
  int x, y;
  unsigned int pixel;
  float xout, yout;
  char *word;

  /* Loop through each pixel in OUT.  Transform it by TRANSFORM, then
     interpolate it to IMAGE, and place the result back in OUT.  */

  for (y = 0; y < out->height; ++y)
    {
      for (x = 0; x < out->width; ++x)
	{
	  /* Transform the coordinates by TRANSFORM.  */
	  android_transform_coordinates (x, y, transform,
					 &xout, &yout);

	  /* Interpolate back to IMAGE.  */
	  pixel = android_fetch_pixel_bilinear (image, xout, yout);

	  /* Put the pixel back in OUT.  */
	  word = out->data + y * out->bytes_per_line + x * 4;
	  memcpy (word, &pixel, sizeof pixel);
	}
    }
}

/* Return the interpolation of X, Y to IMAGE, a depth 24 image.  */

static unsigned int
android_fetch_pixel_nearest_24 (struct android_image *image, float x,
				float y)
{
  int x1, y1;
  char *word;
  unsigned int pixel;

  x1 = MAX (0, MIN (image->width - 1, (int) roundf (x)));
  y1 = MAX (0, MIN (image->height - 1, (int) roundf (y)));

  word = image->data + y1 * image->bytes_per_line + x1 * 4;
  memcpy (&pixel, word, sizeof pixel);

  return pixel;
}

/* Return the interpolation of X, Y to IMAGE, a depth 1 image.  */

static unsigned int
android_fetch_pixel_nearest_1 (struct android_image *image, float x,
			       float y)
{
  int x1, y1;
  char *byte;

  x1 = MAX (0, MIN (image->width - 1, (int) roundf (x)));
  y1 = MAX (0, MIN (image->height - 1, (int) roundf (y)));

  byte = image->data + y1 * image->bytes_per_line;
  return (byte[x1 / 8] & (1 << x1 % 8)) ? 1 : 0;
}

/* Transform the depth 24 or 1 image IMAGE by the 3x2 affine
   transformation matrix MATRIX.  Place the result in OUT.  The matrix
   maps from the coordinate space of OUT to IMAGE.  Use a
   nearest-neighbor filter.  */

void
android_project_image_nearest (struct android_image *image,
			       struct android_image *out,
			       struct android_transform *transform)
{
  int x, y;
  unsigned int pixel;
  float xout, yout;
  char *word, *byte;

  if (image->depth == 1)
    {
      for (y = 0; y < out->height; ++y)
	{
	  for (x = 0; x < out->width; ++x)
	    {
	      /* Transform the coordinates by TRANSFORM.  */
	      android_transform_coordinates (x, y, transform,
					     &xout, &yout);

	      /* Interpolate back to IMAGE.  */
	      pixel = android_fetch_pixel_nearest_1 (image, xout, yout);

	      /* Put the pixel back in OUT.  */
	      byte = out->data + y * out->bytes_per_line + x / 8;

	      if (pixel)
		*byte |= (1 << x % 8);
	      else
		*byte &= ~(1 << x % 8);
	    }
	}

      return;
    }

  for (y = 0; y < out->height; ++y)
    {
      for (x = 0; x < out->width; ++x)
	{
	  /* Transform the coordinates by TRANSFORM.  */
	  android_transform_coordinates (x, y, transform,
					 &xout, &yout);

	  /* Interpolate back to IMAGE.  */
	  pixel = android_fetch_pixel_nearest_24 (image, xout, yout);

	  /* Put the pixel back in OUT.  */
	  word = out->data + y * out->bytes_per_line + x * 4;
	  memcpy (word, &pixel, sizeof pixel);
	}
    }
}



/* Other miscellaneous functions.  */

/* Ask the system to start browsing the specified URL.  Upon failure,
   return a string describing the error.  Else, value is nil.  URL
   should be encoded unless SEND.

   If SEND, open the URL with applications that can ``send'' or
   ``share'' the URL (through mail, for example.)  */

Lisp_Object
android_browse_url (Lisp_Object url, Lisp_Object send)
{
  jobject value, string;
  Lisp_Object tem;
  const char *buffer;

  string = android_build_string (url, NULL);
  value
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       service_class.browse_url,
						       string,
						       (jboolean) !NILP (send));
  android_exception_check ();

  ANDROID_DELETE_LOCAL_REF (string);

  /* If no string was returned, return Qnil.  */
  if (!value)
    return Qnil;

  buffer = (*android_java_env)->GetStringUTFChars (android_java_env,
						   (jstring) value,
						   NULL);
  android_exception_check_nonnull ((void *) buffer, value);

  /* Otherwise, build the string describing the error.  */
  tem = build_unibyte_string (buffer);

  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) value,
					      buffer);

  /* And decode and return the same.  */
  ANDROID_DELETE_LOCAL_REF (value);
  return code_convert_string_norecord (tem, Qandroid_jni, false);
}

/* Tell the system to restart Emacs in a short amount of time, and
   then kill Emacs.  Never return.  This is used to implement
   `restart-emacs'.  */

_Noreturn void
android_restart_emacs (void)
{
  /* Try to call the Java side function.  Normally, this should call
     System.exit to terminate this process.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.restart_emacs);

  /* Exit anyway, in case EmacsService did not do so.  */
  exit (0);
}

/* Return a number from 1 to 34 describing the version of Android
   Emacs is running on.

   This is different from __ANDROID_API__, as that describes the
   minimum version of Android this build of Emacs will run on, and in
   turn which APIs Emacs can safely use.  */

int
(android_get_current_api_level) (void)
{
  return android_api_level;
}

/* Query the status of the battery, and place it in *STATUS.
   Value is 1 upon failure, else 0.  */

int
android_query_battery (struct android_battery_state *status)
{
  jlongArray array;
  jlong *longs;
  jmethodID method;

  method = service_class.query_battery;
  array
    = (*android_java_env)->CallNonvirtualObjectMethod (android_java_env,
						       emacs_service,
						       service_class.class,
						       method);
  android_exception_check ();

  /* A NULL return with no exception means that battery information
     could not be obtained.  */

  if (!array)
    return 1;

  longs = (*android_java_env)->GetLongArrayElements (android_java_env,
						     array, NULL);
  android_exception_check_nonnull (longs, array);

  status->capacity = longs[0];
  status->charge_counter = longs[1];
  status->current_average = longs[2];
  status->current_now = longs[3];
  status->status = longs[4];
  status->remaining = longs[5];
  status->plugged = longs[6];
  status->temperature = longs[7];

  (*android_java_env)->ReleaseLongArrayElements (android_java_env,
						 array, longs,
						 JNI_ABORT);
  ANDROID_DELETE_LOCAL_REF (array);

  return 0;
}

/* Display a file panel and grant Emacs access to the SAF directory
   within it.  Value is 1 upon failure and 0 upon success (which only
   indicates that the panel has been displayed successfully; the panel
   may still be dismissed without a file being selected.)  */

int
android_request_directory_access (void)
{
  jint rc;
  jmethodID method;

  method = service_class.request_directory_access;
  rc = (*android_java_env)->CallNonvirtualIntMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method);
  android_exception_check ();

  return rc;
}

/* Return whether Emacs is entitled to access external storage.

   On Android 5.1 and earlier, such permissions as are declared within
   an application's manifest are granted during installation and are
   irrevocable.

   On Android 6.0 through Android 10.0, the right to read external
   storage is a regular permission granted from the Permissions
   panel.

   On Android 11.0 and later, that right must be granted through an
   independent ``Special App Access'' settings panel.  */

bool
android_external_storage_available_p (void)
{
  jboolean rc;
  jmethodID method;

  if (android_api_level <= 22) /* LOLLIPOP_MR1 */
    return true;

  method = service_class.external_storage_available;
  rc = (*android_java_env)->CallNonvirtualBooleanMethod (android_java_env,
							 emacs_service,
							 service_class.class,
							 method);
  android_exception_check ();

  return rc;
}

/* Display a dialog from which the aforementioned rights can be
   granted.  */

void
android_request_storage_access (void)
{
  jmethodID method;

  if (android_api_level <= 22) /* LOLLIPOP_MR1 */
    return;

  method = service_class.request_storage_access;
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 method);
  android_exception_check ();
}

/* Recreate the activity to which WINDOW is attached to debug graphics
   code executed in response to window attachment.  */

void
android_recreate_activity (android_window window)
{
  jobject object;
  jmethodID method;

  object = android_resolve_handle (window);
  method = window_class.recreate_activity;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, object,
						 window_class.class,
						 method);
  android_exception_check ();
}



/* The thread from which a query against a thread is currently being
   made, if any.  Value is 0 if no query is in progress, 1 if a query
   is being made from the UI thread to the main thread, and 2 if a
   query is being made the other way around.  */
static char android_servicing_query;

/* Function that is waiting to be run in the Emacs thread.  */
static void (*android_query_function) (void *);

/* Context for that function.  */
static void *android_query_context;

/* Deadlock protection.  The UI thread and the Emacs thread must
   sometimes make synchronous queries to each other, which are
   normally answered inside each thread's respective event loop.
   Deadlocks can happen when both threads simultaneously make such
   synchronous queries and block waiting for each others responses.

   The Emacs thread can be interrupted to service any queries made by
   the UI thread, but is not possible the other way around.

   To avoid such deadlocks, an atomic counter is provided.  This
   counter is set to two every time a query starts from the main
   thread, and is set to zero every time one ends.  If the UI thread
   tries to make a query and sees that the counter is two, it simply
   returns so that its event loop can proceed to perform and respond
   to the query.  If the Emacs thread sees that the counter is one,
   then it stops to service all queries being made by the input
   method, then proceeds to make its query with the counter set to
   2.

   The memory synchronization is simple: all writes to
   `android_query_context' and `android_query_function' are depended
   on by writes to the atomic counter.  Loads of the new value from
   the counter are then guaranteed to make those writes visible.  The
   separate flag `android_urgent_query' does not depend on anything
   itself; however, the input signal handler executes a memory fence
   to ensure that all query related writes become visible.  */

/* Clear the ``urgent query'' flag and run any function that the UI
   thread has asked to run.  Must be invoked before `android_select'
   from the thread holding the global lock.  */

void
android_before_select (void)
{
  /* Since Emacs is reading keyboard input again, signify that queries
     from input methods are no longer ``urgent''.  */

  __atomic_clear (&android_urgent_query, __ATOMIC_RELEASE);

  /* Check for and run anything the UI thread wants to run on the main
     thread.  */
  android_check_query ();
}

/* Run any function that the UI thread has asked to run, and then
   signal its completion.  */

void
android_check_query (void)
{
  void (*proc) (void *);
  void *closure;

  if (!__atomic_load_n (&android_servicing_query, __ATOMIC_ACQUIRE))
    return;

  /* First, load the procedure and closure.  */
  closure = android_query_context;
  proc = android_query_function;

  if (!proc)
    return;

  proc (closure);

  /* Finish the query.  */
  android_query_context = NULL;
  android_query_function = NULL;
  __atomic_store_n (&android_servicing_query, 0, __ATOMIC_RELEASE);
  __atomic_clear (&android_urgent_query, __ATOMIC_RELEASE);

  /* Signal completion.  */
  sem_post (&android_query_sem);
}

/* Run any function that the UI thread has asked to run, if the UI
   thread has been waiting for more than two seconds.

   Call this from `process_pending_signals' to ensure that the UI
   thread always receives an answer within a reasonable amount of
   time.  */

void
android_check_query_urgent (void)
{
  void (*proc) (void *);
  void *closure;

  if (!__atomic_load_n (&android_urgent_query, __ATOMIC_ACQUIRE))
    return;

  __android_log_print (ANDROID_LOG_VERBOSE, __func__,
		       "Responding to urgent query...");

  if (!__atomic_load_n (&android_servicing_query, __ATOMIC_ACQUIRE))
    return;

  /* First, load the procedure and closure.  */
  closure = android_query_context;
  proc = android_query_function;

  if (!proc)
    return;

  (*proc) (closure);

  /* Finish the query.  Don't clear `android_urgent_query'; instead,
     do that the next time Emacs enters the keyboard loop.  */

  android_query_context = NULL;
  android_query_function = NULL;
  __atomic_store_n (&android_servicing_query, 0, __ATOMIC_RELEASE);

  /* Signal completion.  */
  sem_post (&android_query_sem);
}

/* Run the function that the UI thread has asked to run, and then
   signal its completion.  Do not change `android_servicing_query'
   after it completes.  */

static void
android_answer_query (void)
{
  void (*proc) (void *);
  void *closure;

  eassert (__atomic_load_n (&android_servicing_query,
			    __ATOMIC_ACQUIRE)
	   == 1);

  /* First, load the procedure and closure.  */
  closure = android_query_context;
  proc = android_query_function;

  if (!proc)
    return;

  proc (closure);

  /* Finish the query.  */
  android_query_context = NULL;
  android_query_function = NULL;
  __atomic_clear (&android_urgent_query, __ATOMIC_RELEASE);

  /* Signal completion.  */
  sem_post (&android_query_sem);
}

/* Like `android_answer_query'.  However, the query may not have
   begun; spin until it has.  */

static void
android_answer_query_spin (void)
{
  int n;

  while (!(n = __atomic_load_n (&android_servicing_query,
				__ATOMIC_ACQUIRE)))
    eassert (!n);

  /* Note that this function is supposed to be called before
     `android_begin_query' starts, so clear the service flag.  */
  android_check_query ();
}

/* Notice that the Emacs thread will start blocking waiting for a
   response from the UI thread.  Process any pending queries from the
   UI thread.

   This function may be called from Java.  */

static void
android_begin_query (void)
{
  char old;

  /* Load the previous value of `android_servicing_query' and then set
     it to 2.  */

  old = __atomic_exchange_n (&android_servicing_query,
			     2, __ATOMIC_ACQ_REL);

  /* See if a query was previously in progress.  */
  if (old == 1)
    {
      /* Answer the query that is currently being made.  */
      eassert (android_query_function != NULL);
      android_answer_query ();
    }

  /* `android_servicing_query' is now 2.  */
}

/* Notice that a query has stopped.  This function may be called from
   Java.  */

static void
android_end_query (void)
{
  __atomic_store_n (&android_servicing_query, 0, __ATOMIC_RELEASE);
  __atomic_clear (&android_urgent_query, __ATOMIC_RELEASE);
}

/* Synchronously ask the Emacs thread to run the specified PROC with
   the given CLOSURE.  Return if this fails, or once PROC is run.

   PROC may be run from inside maybe_quit.

   It is not okay to run Lisp code which signals or performs non
   trivial tasks inside PROC.

   Return 1 if the Emacs thread is currently waiting for the UI thread
   to respond and PROC could not be run, or 0 otherwise.  */

int
android_run_in_emacs_thread (void (*proc) (void *), void *closure)
{
  union android_event event;
  char old;
  int rc;
  struct timespec timeout;

  event.xaction.type = ANDROID_WINDOW_ACTION;
  event.xaction.serial = ++event_serial;
  event.xaction.window = 0;
  event.xaction.action = 0;

  /* Set android_query_function and android_query_context.  */
  android_query_context = closure;
  android_query_function = proc;

  /* Don't allow deadlocks to happen; make sure the Emacs thread is
     not waiting for something to be done (in that case,
     `android_query_context' is 2.)  */

  old = 0;
  if (!__atomic_compare_exchange_n (&android_servicing_query, &old,
				    1, false, __ATOMIC_ACQ_REL,
				    __ATOMIC_ACQUIRE))
    {
      android_query_context = NULL;
      android_query_function = NULL;

      /* The two variables above may still be non-NULL from the POV of
	 the main thread, as no happens-before constraint is placed on
         those stores wrt a future load from `android_servicing_query'.  */

      return 1;
    }

  /* Send a dummy event.  `android_check_query' will be called inside
     wait_reading_process_output after the event arrives.

     Otherwise, android_select will call `android_check_query' when next
     it is entered.  */
  android_write_event (&event);

  /* Start waiting for the function to be executed.  First, wait two
     seconds for the query to execute normally.  */

  timeout.tv_sec = 2;
  timeout.tv_nsec = 0;
  timeout = timespec_add (current_timespec (), timeout);

  /* See if an urgent query was recently answered without entering the
     keyboard loop in between.  When that happens, raise SIGIO to
     continue processing queries as soon as possible.  */

  if (__atomic_load_n (&android_urgent_query, __ATOMIC_ACQUIRE))
    kill (getpid (), SIGIO);

 again:
  rc = sem_timedwait (&android_query_sem, &timeout);

  if (rc < 0)
    {
      if (errno == EINTR)
	goto again;

      eassert (errno == ETIMEDOUT);

      __android_log_print (ANDROID_LOG_VERBOSE, __func__,
			   "Timed out waiting for response"
			   " from main thread...");

      /* The query timed out.  At this point, set
	 `android_urgent_query' to true.  */
      __atomic_store_n (&android_urgent_query, true,
			__ATOMIC_RELEASE);

    kill_again:

      /* And raise SIGIO.  Now that the query is considered urgent,
	 the main thread will reply while reading async input.

	 Normally, the main thread waits for the keyboard loop to be
	 entered before responding, in order to avoid responding with
	 inaccurate results taken during command executioon.  */
      kill (getpid (), SIGIO);

      /* Wait for the query to complete.  `android_urgent_query' is
	 only cleared by either `android_select' or
	 `android_check_query', so there's no need to worry about the
	 flag being cleared before the query is processed.

	 Send SIGIO again periodically until the query is answered, on
	 the off chance that SIGIO arrived too late to preempt a
	 system call, but too early for it to return EINTR.  */

      timeout.tv_sec = 4;
      timeout.tv_nsec = 0;
      timeout = timespec_add (current_timespec (), timeout);

      while (sem_timedwait (&android_query_sem, &timeout) < 0)
	{
	  /* If waiting timed out, send SIGIO to the main thread
	     again.  */

	  if (errno == ETIMEDOUT)
	    goto kill_again;

	  /* Otherwise, continue waiting.  */
	  eassert (errno == EINTR);
	}
    }

  /* At this point, `android_servicing_query' should either be zero if
     the query was answered or two if the main thread has started a
     query.  */

  eassert (!__atomic_load_n (&android_servicing_query,
			     __ATOMIC_ACQUIRE)
	   || (__atomic_load_n (&android_servicing_query,
				__ATOMIC_ACQUIRE) == 2));

  return 0;
}



/* Input method related functions.  */

/* Change WINDOW's active selection to the characters between
   SELECTION_START and SELECTION_END.

   Also, update the composing region to COMPOSING_REGION_START and
   COMPOSING_REGION_END.

   If any value cannot fit in jint, then the behavior of the input
   method is undefined.  */

void
android_update_ic (android_window window, ptrdiff_t selection_start,
		   ptrdiff_t selection_end, ptrdiff_t composing_region_start,
		   ptrdiff_t composing_region_end)
{
  jobject object;

  object = android_resolve_handle (window);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.update_ic,
						 object,
						 (jint) selection_start,
						 (jint) selection_end,
						 (jint) composing_region_start,
						 (jint) composing_region_end);
  android_exception_check ();
}

/* Reinitialize any ongoing input method connection on WINDOW.

   Any input method that is connected to WINDOW will invalidate its
   cache of the buffer contents.

   MODE controls certain aspects of the input method's behavior:

     - If MODE is ANDROID_IC_MODE_NULL, the input method will be
       deactivated, and an ASCII only keyboard will be displayed
       instead.

     - If MODE is ANDROID_IC_MODE_ACTION, the input method will
       edit text normally, but send ``return'' as a key event.
       This is useful inside the mini buffer.

     - If MODE is ANDROID_IC_MODE_TEXT, the input method is free
       to behave however it wants.  */

void
android_reset_ic (android_window window, enum android_ic_mode mode)
{
  jobject object;

  object = android_resolve_handle (window);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.reset_ic,
						 object, (jint) mode);
  android_exception_check ();
}

/* Make updates to extracted text known to the input method on
   WINDOW.  TEXT should be a local reference to the new
   extracted text.  TOKEN should be the token specified by the
   input method.  */

void
android_update_extracted_text (android_window window, void *text,
			       int token)
{
  jobject object;
  jmethodID method;

  object = android_resolve_handle (window);
  method = service_class.update_extracted_text;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 method, object,
						 /* N.B. that text is
						    not jobject,
						    because that type
						    is not available
						    in
						    androidgui.h.  */
						 (jobject) text,
						 (jint) token);
  android_exception_check_1 (text);
}

/* Report the position of the cursor to the input method connection on
   WINDOW.

   X is the horizontal position of the end of the insertion marker.  Y
   is the top of the insertion marker.  Y_BASELINE is the baseline of
   the row containing the insertion marker, and Y_BOTTOM is the bottom
   of the insertion marker.  */

void
android_update_cursor_anchor_info (android_window window, float x,
				   float y, float y_baseline,
				   float y_bottom)
{
  jobject object;
  jmethodID method;

  object = android_resolve_handle (window);
  method = service_class.update_cursor_anchor_info;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 method,
						 object,
						 (jfloat) x,
						 (jfloat) y,
						 (jfloat) y_baseline,
						 (jfloat) y_bottom);
  android_exception_check ();
}



/* Window decoration management functions.  */

/* Make the specified WINDOW fullscreen, i.e. obscure all of the
   system navigation and status bars.  If not FULLSCREEN, make it
   maximized instead.

   Value is 1 if the system does not support this, else 0.  */

int
android_set_fullscreen (android_window window, bool fullscreen)
{
  jobject object;

  /* Android 4.0 and earlier don't support fullscreen windows.  */

  if (android_api_level < 16)
    return 1;

  object = android_resolve_handle (window);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 object,
						 window_class.class,
						 window_class.set_fullscreen,
						 (jboolean) fullscreen);
  android_exception_check ();
  return 0;
}



/* Window cursor support.  */

android_cursor
android_create_font_cursor (enum android_cursor_shape shape)
{
  jobject object;

  /* Next, create the cursor.  */
  object = (*android_java_env)->NewObject (android_java_env,
					   cursor_class.class,
					   cursor_class.constructor,
					   (jint) shape);
  android_exception_check ();
  return android_globalize_reference (object);
}

void
android_define_cursor (android_window window, android_cursor cursor)
{
  jobject window1, cursor1;
  jmethodID method;

  window1 = android_resolve_handle (window);
  cursor1 = android_resolve_handle (cursor);
  method = window_class.define_cursor;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window1,
						 window_class.class,
						 method, cursor1);
  android_exception_check ();
}

void
android_free_cursor (android_cursor cursor)
{
  android_destroy_handle (cursor);
}



/* Process execution.

   Newer Android systems use SELinux to restrict user programs from
   executing programs installed in the application data directory for
   security reasons.  Emacs uses a `loader' binary installed in the
   application data directory to manually load executables and replace
   the `execve' system call.  */

/* Rewrite the command line given in *ARGV to utilize the `exec1'
   bootstrap binary if necessary.

   Value is 0 upon success, else 1.  Set errno upon failure.

   ARGV holds a pointer to a NULL-terminated array of arguments given
   to `emacs_spawn'.  */

int
android_rewrite_spawn_argv (const char ***argv)
{
  static const char **new_args;
  static size_t n_new_args;
  static char exec1_name[PATH_MAX + 1], loader_name[PATH_MAX + 1];
  size_t i, nargs;
  int n;

  /* This isn't required on Android 9 or earlier.  */

  if (android_api_level < 29 || !android_use_exec_loader)
    return 0;

  /* Get argv[0]; this should never be NULL.
     Then, verify that it exists and is executable.  */

  eassert (**argv);
  if (access (**argv, R_OK | X_OK))
    return 1;

  /* Count the number of arguments in *argv.  */

  nargs = 0;
  while ((*argv)[nargs])
    ++nargs;

  /* Allocate a buffer in which to save the rewritten argument
     array.  */
  if (n_new_args != nargs + 2)
    {
      new_args = xrealloc (new_args, sizeof *new_args * (nargs + 3));
      n_new_args = nargs + 2;
    }

  /* Fill in the name of `libexec1.so'.  */
  n = snprintf (exec1_name, PATH_MAX + 1, "%s/libexec1.so",
		android_lib_dir);
  if (n >= PATH_MAX)
    goto name_too_long;

  /* And libloader.so.  */
  n = snprintf (loader_name, PATH_MAX + 1, "%s/libloader.so",
		android_lib_dir);
  if (n >= PATH_MAX)
    goto name_too_long;

  /* Now fill in the first two arguments.  */
  new_args[0] = exec1_name;
  new_args[1] = loader_name;

  /* And insert the rest, including the trailing NULL.  */
  for (i = 0; i < nargs + 1; ++i)
    new_args[i + 2] = (*argv)[i];

  /* Replace argv.  */
  *argv = new_args;

  /* Return success.  */
  return 0;

 name_too_long:
  errno = ENAMETOOLONG;
  return 0;
}



#else /* ANDROID_STUBIFY */

/* X emulation functions for Android.  */

struct android_gc *
android_create_gc (enum android_gc_value_mask mask,
		   struct android_gc_values *values)
{
  /* This function should never be called when building stubs.  */
  emacs_abort ();
}

void
android_free_gc (struct android_gc *gc)
{
  /* This function should never be called when building stubs.  */
  emacs_abort ();
}

struct android_image *
android_create_image (unsigned int depth, enum android_image_format format,
		      char *data, unsigned int width, unsigned int height)
{
  emacs_abort ();
}

void
android_destroy_image (struct android_image *ximg)
{
  emacs_abort ();
}

void
android_put_pixel (struct android_image *ximg, int x, int y,
		   unsigned long pixel)
{
  emacs_abort ();
}

unsigned long
android_get_pixel (struct android_image *ximg, int x, int y)
{
  emacs_abort ();
}

struct android_image *
android_get_image (android_drawable drawable,
		   enum android_image_format format)
{
  emacs_abort ();
}

void
android_put_image (android_pixmap pixmap,
		   struct android_image *image)
{
  emacs_abort ();
}

void
android_project_image_bilinear (struct android_image *image,
				struct android_image *out,
				struct android_transform *transform)
{
  emacs_abort ();
}

void
android_project_image_nearest (struct android_image *image,
			       struct android_image *out,
			       struct android_transform *transform)
{
  emacs_abort ();
}

#endif /* !ANDROID_STUBIFY */
