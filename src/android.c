/* Android initialization for GNU Emacs.

Copyright (C) 2023 Free Software Foundation, Inc.

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
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <limits.h>
#include <signal.h>
#include <semaphore.h>
#include <dlfcn.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdckdint.h>

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/param.h>

/* Old NDK versions lack MIN and MAX.  */
#include <minmax.h>

#include <assert.h>
#include <fingerprint.h>

#include "android.h"
#include "androidgui.h"

#include "lisp.h"
#include "blockinput.h"
#include "coding.h"
#include "epaths.h"

/* Whether or not Emacs is running inside the application process and
   Android windowing should be enabled.  */
bool android_init_gui;

#ifndef ANDROID_STUBIFY

#if __ANDROID_API__ >= 9
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#else
#include "android-asset.h"
#endif

#include <android/bitmap.h>
#include <android/log.h>

#include <linux/ashmem.h>
#include <linux/unistd.h>

#include <sys/syscall.h>

#ifdef __aarch64__
#include <arm_neon.h>
#endif /* __aarch64__ */

#define ANDROID_THROW(env, class, msg)					\
  ((*(env))->ThrowNew ((env), (*(env))->FindClass ((env), class), msg))

#define ANDROID_MAX_ASSET_FD 65535

struct android_fd_table_entry
{
  /* Various flags associated with this table.  */
  short flags;

  /* The stat buffer associated with this entry.  */
  struct stat statb;
};

enum android_fd_table_entry_flags
  {
    ANDROID_FD_TABLE_ENTRY_IS_VALID = 1,
  };

struct android_emacs_service
{
  jclass class;
  jmethodID fill_rectangle;
  jmethodID fill_polygon;
  jmethodID draw_rectangle;
  jmethodID draw_line;
  jmethodID draw_point;
  jmethodID clear_window;
  jmethodID clear_area;
  jmethodID ring_bell;
  jmethodID query_tree;
  jmethodID get_screen_width;
  jmethodID get_screen_height;
  jmethodID detect_mouse;
  jmethodID name_keysym;
  jmethodID browse_url;
  jmethodID restart_emacs;
  jmethodID update_ic;
  jmethodID reset_ic;
  jmethodID open_content_uri;
  jmethodID check_content_uri;
  jmethodID query_battery;
  jmethodID display_toast;
  jmethodID update_extracted_text;
  jmethodID update_cursor_anchor_info;
};

struct android_emacs_pixmap
{
  jclass class;
  jmethodID constructor;
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
  jmethodID damage_rect;
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
  jmethodID make_input_focus;
  jmethodID raise;
  jmethodID lower;
  jmethodID get_window_geometry;
  jmethodID translate_coordinates;
  jmethodID set_dont_accept_focus;
  jmethodID set_dont_focus_on_map;
  jmethodID define_cursor;
};

struct android_emacs_cursor
{
  jclass class;
  jmethodID constructor;
};

/* The API level of the current device.  */
static int android_api_level;

/* The asset manager being used.  */
static AAssetManager *asset_manager;

/* Whether or not Emacs has been initialized.  */
static int emacs_initialized;

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
char *android_class_path;

/* The display's pixel densities.  */
double android_pixel_density_x, android_pixel_density_y;

/* The Android application data directory.  */
static char *android_files_dir;

/* Array of structures used to hold asset information corresponding to
   a file descriptor.  This assumes Emacs does not do funny things
   with dup.  It currently does not.  */
static struct android_fd_table_entry android_table[ANDROID_MAX_ASSET_FD];

/* The Java environment being used for the main thread.  */
JNIEnv *android_java_env;

/* The EmacsGC class.  */
static jclass emacs_gc_class;

/* Various fields.  */
static jfieldID emacs_gc_foreground, emacs_gc_background;
static jfieldID emacs_gc_function, emacs_gc_clip_rects;
static jfieldID emacs_gc_clip_x_origin, emacs_gc_clip_y_origin;
static jfieldID emacs_gc_stipple, emacs_gc_clip_mask;
static jfieldID emacs_gc_fill_style, emacs_gc_ts_origin_x;
static jfieldID emacs_gc_ts_origin_y;

/* The constructor and one function.  */
static jmethodID emacs_gc_constructor, emacs_gc_mark_dirty;

/* The Rect class.  */
static jclass android_rect_class;

/* Its constructor.  */
static jmethodID android_rect_constructor;

/* The EmacsService object.  */
static jobject emacs_service;

/* Various methods associated with the EmacsService.  */
static struct android_emacs_service service_class;

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

/* The last event serial used.  This is a 32 bit value, but it is
   stored in unsigned long to be consistent with X.  */
unsigned int event_serial;

#ifdef __i386__

/* Unused pointer used to control compiler optimizations.  */
void *unused_pointer;

#endif /* __i386__ */



/* Event handling functions.  Events are stored on a (circular) queue
   that is read synchronously.  The Android port replaces pselect with
   a function android_select, which runs pselect in a separate thread,
   but more importantly also waits for events to be available on the
   android event queue.  */

struct android_event_container
{
  /* The next and last events in this queue.  */
  struct android_event_container *volatile next, *last;

  /* The event itself.  */
  union android_event event;
};

struct android_event_queue
{
  /* Mutex protecting the event queue.  */
  pthread_mutex_t mutex;

  /* Mutex protecting the select data.  */
  pthread_mutex_t select_mutex;

  /* The thread used to run select.  */
  pthread_t select_thread;

  /* Condition variables for the reading side.  */
  pthread_cond_t read_var;

  /* The number of events in the queue.  If this is greater than 1024,
     writing will block.  */
  volatile int num_events;

  /* Circular queue of events.  */
  struct android_event_container events;
};

/* Arguments to pselect used by the select thread.  */
static volatile int android_pselect_nfds;
static fd_set *volatile android_pselect_readfds;
static fd_set *volatile android_pselect_writefds;
static fd_set *volatile android_pselect_exceptfds;
static struct timespec *volatile android_pselect_timeout;

/* Value of pselect.  */
static int android_pselect_rc;

/* The global event queue.  */
static struct android_event_queue event_queue;

/* Semaphores used to signal select completion and start.  */
static sem_t android_pselect_sem, android_pselect_start_sem;

#if __ANDROID_API__ < 16

/* Select self-pipe.  */
static int select_pipe[2];

#else

/* Whether or not pselect has been interrupted.  */
static volatile sig_atomic_t android_pselect_interrupted;

#endif

static void *
android_run_select_thread (void *data)
{
  /* Apparently this is required too.  */
  JNI_STACK_ALIGNMENT_PROLOGUE;

  int rc;
#if __ANDROID_API__ < 16
  int nfds;
  fd_set readfds, writefds;
  char byte;
#else
  sigset_t signals, waitset;
  int sig;
#endif

#if __ANDROID_API__ < 16
  /* A completely different implementation is used when building for
     Android versions earlier than 16, because pselect with a signal
     mask does not work there.  Instead of blocking SIGUSR1 and
     unblocking it inside pselect, a file descriptor is used instead.
     Something is written to the file descriptor every time select is
     supposed to return.  */

  while (true)
    {
      /* Wait for the thread to be released.  */
      while (sem_wait (&android_pselect_start_sem) < 0)
	;;

      /* Get the select lock and call pselect.  API 8 does not have
	 working pselect in any sense.  Instead, pselect wakes up on
	 select_pipe[0].  */

      pthread_mutex_lock (&event_queue.select_mutex);
      nfds = android_pselect_nfds;

      if (android_pselect_readfds)
	readfds = *android_pselect_readfds;
      else
	FD_ZERO (&readfds);

      if (nfds < select_pipe[0] + 1)
	nfds = select_pipe[0] + 1;
      FD_SET (select_pipe[0], &readfds);

      rc = pselect (nfds, &readfds, &writefds,
		    android_pselect_exceptfds,
		    android_pselect_timeout,
		    NULL);

      /* Subtract 1 from rc if writefds contains the select pipe.  */
      if (FD_ISSET (select_pipe[0], &writefds))
	rc -= 1;

      /* Save the writefds back again.  */
      if (android_pselect_writefds)
	*android_pselect_writefds = writefds;

      android_pselect_rc = rc;
      pthread_mutex_unlock (&event_queue.select_mutex);

      /* Signal the main thread that there is now data to read.  Hold
         the event queue lock during this process to make sure this
         does not happen before the main thread begins to wait for the
         condition variable.  */

      pthread_mutex_lock (&event_queue.mutex);
      pthread_cond_broadcast (&event_queue.read_var);
      pthread_mutex_unlock (&event_queue.mutex);

      /* Read a single byte from the select pipe.  */
      read (select_pipe[0], &byte, 1);

      /* Signal the Emacs thread that pselect is done.  If read_var
	 was signaled by android_write_event, event_queue.mutex could
	 still be locked, so this must come before.  */
      sem_post (&android_pselect_sem);
    }
#else
  if (pthread_sigmask (SIG_BLOCK, &signals, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_sigmask: %s",
			 strerror (errno));

  sigfillset (&signals);
  sigdelset (&signals, SIGUSR1);
  sigemptyset (&waitset);
  sigaddset (&waitset, SIGUSR1);

  while (true)
    {
      /* Wait for the thread to be released.  */
      while (sem_wait (&android_pselect_start_sem) < 0)
	;;

      /* Clear the ``pselect interrupted'' flag.  This is safe because
	 right now, SIGUSR1 is blocked.  */
      android_pselect_interrupted = 0;

      /* Get the select lock and call pselect.  */
      pthread_mutex_lock (&event_queue.select_mutex);
      rc = pselect (android_pselect_nfds,
		    android_pselect_readfds,
		    android_pselect_writefds,
		    android_pselect_exceptfds,
		    android_pselect_timeout,
		    &signals);
      android_pselect_rc = rc;
      pthread_mutex_unlock (&event_queue.select_mutex);

      /* Signal the main thread that there is now data to read.  Hold
         the event queue lock during this process to make sure this
         does not happen before the main thread begins to wait for the
         condition variable.  */

      pthread_mutex_lock (&event_queue.mutex);
      pthread_cond_broadcast (&event_queue.read_var);
      pthread_mutex_unlock (&event_queue.mutex);

      /* Check `android_pselect_interrupted' instead of rc and errno.

         This is because `pselect' does not return an rc of -1 upon
         being interrupted in some versions of Android, but does set
         signal masks correctly.  */

      if (!android_pselect_interrupted)
	/* Now, wait for SIGUSR1, unless pselect was interrupted and
	   the signal was already delivered.  The Emacs thread will
	   always send this signal after read_var is triggered or the
	   UI thread has sent an event.  */
	sigwait (&waitset, &sig);

      /* Signal the Emacs thread that pselect is done.  If read_var
	 was signaled by android_write_event, event_queue.mutex could
	 still be locked, so this must come before.  */
      sem_post (&android_pselect_sem);
    }
#endif

  return NULL;
}

#if __ANDROID_API__ >= 16

static void
android_handle_sigusr1 (int sig, siginfo_t *siginfo, void *arg)
{
  /* Notice that pselect has been interrupted.  */
  android_pselect_interrupted = 1;
}

#endif

/* Semaphore used to indicate completion of a query.
   This should ideally be defined further down.  */
static sem_t android_query_sem;

/* Set up the global event queue by initializing the mutex and two
   condition variables, and the linked list of events.  This must be
   called before starting the Emacs thread.  Also, initialize the
   thread used to run pselect.

   These functions must also use the C library malloc and free,
   because xmalloc is not thread safe.  */

static void
android_init_events (void)
{
  struct sigaction sa;

  if (pthread_mutex_init (&event_queue.mutex, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_mutex_init: %s",
			 strerror (errno));

  if (pthread_mutex_init (&event_queue.select_mutex, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_mutex_init: %s",
			 strerror (errno));

  if (pthread_cond_init (&event_queue.read_var, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_cond_init: %s",
			 strerror (errno));

  sem_init (&android_pselect_sem, 0, 0);
  sem_init (&android_pselect_start_sem, 0, 0);
  sem_init (&android_query_sem, 0, 0);

  event_queue.events.next = &event_queue.events;
  event_queue.events.last = &event_queue.events;

#if __ANDROID_API__ >= 16

  /* Before starting the select thread, make sure the disposition for
     SIGUSR1 is correct.  */
  sigfillset (&sa.sa_mask);
  sa.sa_sigaction = android_handle_sigusr1;
  sa.sa_flags = SA_SIGINFO;

#else

  /* Set up the file descriptor used to wake up pselect.  */
  if (pipe2 (select_pipe, O_CLOEXEC) < 0)
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pipe2: %s", strerror (errno));

  /* Make sure the read end will fit in fd_set.  */
  if (select_pipe[0] >= FD_SETSIZE)
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "read end of select pipe"
			 " lies outside FD_SETSIZE!");

#endif

  if (sigaction (SIGUSR1, &sa, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "sigaction: %s",
			 strerror (errno));

  /* Start the select thread.  */
  if (pthread_create (&event_queue.select_thread, NULL,
		      android_run_select_thread, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_create: %s",
			 strerror (errno));
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
   event arrives.  */

void
android_wait_event (void)
{
  pthread_mutex_lock (&event_queue.mutex);

  /* Wait for events to appear if there are none available to
     read.  */
  if (!event_queue.num_events)
    pthread_cond_wait (&event_queue.read_var,
		       &event_queue.mutex);

  pthread_mutex_unlock (&event_queue.mutex);
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
      raise (SIGIO);
      break;

    default:
      break;
    }
}

int
android_select (int nfds, fd_set *readfds, fd_set *writefds,
		fd_set *exceptfds, struct timespec *timeout)
{
  int nfds_return;
#if __ANDROID_API__ < 16
  static char byte;
#endif

  /* Check for and run anything the UI thread wants to run on the main
     thread.  */
  android_check_query ();

  pthread_mutex_lock (&event_queue.mutex);

  if (event_queue.num_events)
    {
      pthread_mutex_unlock (&event_queue.mutex);
      return 1;
    }

  nfds_return = 0;

  pthread_mutex_lock (&event_queue.select_mutex);
  android_pselect_nfds = nfds;
  android_pselect_readfds = readfds;
  android_pselect_writefds = writefds;
  android_pselect_exceptfds = exceptfds;
  android_pselect_timeout = timeout;
  pthread_mutex_unlock (&event_queue.select_mutex);

  /* Release the select thread.  */
  sem_post (&android_pselect_start_sem);

  /* Start waiting for the event queue condition to be set.  */
  pthread_cond_wait (&event_queue.read_var, &event_queue.mutex);

#if __ANDROID_API__ >= 16
  /* Interrupt the select thread now, in case it's still in
     pselect.  */
  pthread_kill (event_queue.select_thread, SIGUSR1);
#else
  /* Interrupt the select thread by writing to the select pipe.  */
  if (write (select_pipe[1], &byte, 1) != 1)
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "write: %s", strerror (errno));
#endif

  /* Unlock the event queue mutex.  */
  pthread_mutex_unlock (&event_queue.mutex);

  /* Wait for pselect to return in any case.  This must be done with
     the event queue mutex unlocked.  Otherwise, the pselect thread
     can hang if it tries to lock the event queue mutex to signal
     read_var after the UI thread has already done so.  */
  while (sem_wait (&android_pselect_sem) < 0)
    ;;

  /* If there are now events in the queue, return 1.  */

  pthread_mutex_lock (&event_queue.mutex);
  if (event_queue.num_events)
    nfds_return = 1;
  pthread_mutex_unlock (&event_queue.mutex);

  /* Add the return value of pselect.  */
  if (android_pselect_rc >= 0)
    nfds_return += android_pselect_rc;

  if (!nfds_return && android_pselect_rc < 0)
    nfds_return = android_pselect_rc;

  /* This is to shut up process.c when pselect gets EINTR.  */
  if (nfds_return < 0)
    errno = EINTR;

  /* Now check for and run anything the UI thread wants to run in the
     main thread.  */
  android_check_query ();

  return nfds_return;
}



static void *
android_run_debug_thread (void *data)
{
  FILE *file;
  int fd;
  char *line;
  size_t n;

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



/* Asset directory handling functions.  ``directory-tree'' is a file in
   the root of the assets directory describing its contents.

   See lib-src/asset-directory-tool for more details.  */

/* The Android directory tree.  */
static const char *directory_tree;

/* The size of the directory tree.  */
static size_t directory_tree_size;

/* Read an unaligned (32-bit) long from the address POINTER.  */

static unsigned int
android_extract_long (char *pointer)
{
  unsigned int number;

  memcpy (&number, pointer, sizeof number);
  return number;
}

/* Scan to the file FILE in the asset directory tree.  Return a
   pointer to the end of that file (immediately before any children)
   in the directory tree, or NULL if that file does not exist.

   If returning non-NULL, also return the offset to the end of the
   last subdirectory or file in *LIMIT_RETURN.  LIMIT_RETURN may be
   NULL.

   FILE must have less than 11 levels of nesting.  If it ends with a
   trailing slash, then NULL will be returned if it is not actually a
   directory.  */

static const char *
android_scan_directory_tree (char *file, size_t *limit_return)
{
  char *token, *saveptr, *copy, *copy1, *start, *max, *limit;
  size_t token_length, ntokens, i;
  char *tokens[10];

  USE_SAFE_ALLOCA;

  /* Skip past the 5 byte header.  */
  start = (char *) directory_tree + 5;

  /* Figure out the current limit.  */
  limit = (char *) directory_tree + directory_tree_size;

  /* Now, split `file' into tokens, with the delimiter being the file
     name separator.  Look for the file and seek past it.  */

  ntokens = 0;
  saveptr = NULL;
  copy = copy1 = xstrdup (file);
  memset (tokens, 0, sizeof tokens);

  while ((token = strtok_r (copy, "/", &saveptr)))
    {
      copy = NULL;

      /* Make sure ntokens is within bounds.  */
      if (ntokens == ARRAYELTS (tokens))
	{
	  xfree (copy1);
	  goto fail;
	}

      tokens[ntokens] = SAFE_ALLOCA (strlen (token) + 1);
      memcpy (tokens[ntokens], token, strlen (token) + 1);
      ntokens++;
    }

  /* Free the copy created for strtok_r.  */
  xfree (copy1);

  /* If there are no tokens, just return the start of the directory
     tree.  */

  if (!ntokens)
    {
      SAFE_FREE ();

      /* Return the size of the directory tree as the limit.
         Do not subtract the initial header bytes, as the limit
         is an offset from the start of the file.  */

      if (limit_return)
	*limit_return = directory_tree_size;

      return start;
    }

  /* Loop through tokens, indexing the directory tree each time.  */

  for (i = 0; i < ntokens; ++i)
    {
      token = tokens[i];

      /* Figure out how many bytes to compare.  */
      token_length = strlen (token);

    again:

      /* If this would be past the directory, return NULL.  */
      if (start + token_length > limit)
	goto fail;

      /* Now compare the file name.  */
      if (!memcmp (start, token, token_length))
	{
	  /* They probably match.  Find the NULL byte.  It must be
	     either one byte past start + token_length, with the last
	     byte a trailing slash (indicating that it is a
	     directory), or just start + token_length.  Return 4 bytes
	     past the next NULL byte.  */

	  max = memchr (start, 0, limit - start);

	  if (max != start + token_length
	      && !(max == start + token_length + 1
		   && *(max - 1) == '/'))
	    goto false_positive;

	  /* Return it if it exists and is in range, and this is the
	     last token.  Otherwise, set it as start and the limit as
	     start + the offset and continue the loop.  */

	  if (max && max + 5 <= limit)
	    {
	      if (i < ntokens - 1)
		{
		  start = max + 5;
		  limit = ((char *) directory_tree
			   + android_extract_long (max + 1));

		  /* Make sure limit is still in range.  */
		  if (limit > directory_tree + directory_tree_size
		      || start > directory_tree + directory_tree_size)
		    goto fail;

		  continue;
		}

	      /* Now see if max is not a directory and file is.  If
	         file is a directory, then return NULL.  */
	      if (*(max - 1) != '/' && file[strlen (file) - 1] == '/')
		max = NULL;
	      else
		{
		  /* Figure out the limit.  */
		  if (limit_return)
		    *limit_return = android_extract_long (max + 1);

		  /* Go to the end of this file.  */
		  max += 5;
		}

	      SAFE_FREE ();
	      return max;
	    }

	  /* Return NULL otherwise.  */
	  __android_log_print (ANDROID_LOG_WARN, __func__,
			       "could not scan to end of directory tree"
			       ": %s", file);
	  goto fail;
	}

    false_positive:

      /* No match was found.  Set start to the next sibling and try
	 again.  */

      start = memchr (start, 0, limit - start);

      if (!start || start + 5 > limit)
	goto fail;

      start = ((char *) directory_tree
	       + android_extract_long (start + 1));

      /* Make sure start is still in bounds.  */

      if (start > limit)
	goto fail;

      /* Continue the loop.  */
      goto again;
    }

 fail:
  SAFE_FREE ();
  return NULL;
}

/* Return whether or not the directory tree entry DIR is a
   directory.

   DIR should be a value returned by
   `android_scan_directory_tree'.  */

static bool
android_is_directory (const char *dir)
{
  /* If the directory is the directory tree, then it is a
     directory.  */
  if (dir == directory_tree + 5)
    return true;

  /* Otherwise, look 5 bytes behind.  If it is `/', then it is a
     directory.  */
  return (dir - 6 >= directory_tree
	  && *(dir - 6) == '/');
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
#else
  return "Android user";
#endif
}

/* Given a real file name, return the part that describes its asset
   path, or NULL if it is not an asset.  */

static const char *
android_get_asset_name (const char *filename)
{
  if (!strcmp (filename, "/assets") || !strcmp (filename, "/assets/"))
    return "/";

  if (!strncmp (filename, "/assets/", sizeof "/assets/" - 1))
    return filename + (sizeof "/assets/" - 1);

  return NULL;
}

/* Return whether or not the specified FILENAME actually resolves to a
   content resolver URI.  */

static bool
android_content_name_p (const char *filename)
{
  /* Content URIs aren't supported before Android 4.4, so return
     false.  */

  if (android_api_level < 19)
    return false;

  return (!strcmp (filename, "/content")
	  || !strncmp (filename, "/content/",
		       sizeof "/content/" - 1));
}

/* Return the content URI corresponding to a `/content' file name,
   or NULL if it is not a content URI.

   This function is not reentrant.  */

static const char *
android_get_content_name (const char *filename)
{
  static char buffer[PATH_MAX + 1];
  char *head, *token, *saveptr, *copy;
  size_t n;

  n = PATH_MAX;

  /* First handle content ``URIs'' without a provider.  */

  if (!strcmp (filename, "/content")
      || !strcmp (filename, "/content/"))
    return "content://";

  /* Next handle ordinary file names.  */

  if (strncmp (filename, "/content/", sizeof "/content/" - 1))
    return NULL;

  /* Forward past the first directory specifying the schema.  */

  copy = xstrdup (filename + sizeof "/content");
  token = saveptr = NULL;
  head = stpcpy (buffer, "content:/");

  /* Split FILENAME by slashes.  */

  while ((token = strtok_r (!token ? copy : NULL,
			    "/", &saveptr)))
    {
      head = stpncpy (head, "/", n--);
      head = stpncpy (head, token, n);

      /* Check that head has not overflown the buffer.  */
      eassert ((head - buffer) <= PATH_MAX);

      n = PATH_MAX - (head - buffer);
    }

  /* Make sure the given buffer ends up NULL terminated.  */
  buffer[PATH_MAX] = '\0';
  xfree (copy);

  return buffer;
}

/* Return whether or not the specified FILENAME is an accessible
   content URI.  MODE specifies what to check.  */

static bool
android_check_content_access (const char *filename, int mode)
{
  const char *name;
  jobject string;
  size_t length;
  jboolean rc;

  name = android_get_content_name (filename);
  length = strlen (name);

  string = (*android_java_env)->NewByteArray (android_java_env,
					      length);
  android_exception_check ();

  (*android_java_env)->SetByteArrayRegion (android_java_env,
					   string, 0, length,
					   (jbyte *) name);
  rc = (*android_java_env)->CallBooleanMethod (android_java_env,
					       emacs_service,
					       service_class.check_content_uri,
					       string,
					       (jboolean) ((mode & R_OK)
							   != 0),
					       (jboolean) ((mode & W_OK)
							   != 0));
  android_exception_check_1 (string);
  ANDROID_DELETE_LOCAL_REF (string);

  return rc;
}

/* Like fstat.  However, look up the asset corresponding to the file
   descriptor.  If it exists, return the right information.  */

int
android_fstat (int fd, struct stat *statb)
{
  if (fd < ANDROID_MAX_ASSET_FD
      && (android_table[fd].flags
	  & ANDROID_FD_TABLE_ENTRY_IS_VALID))
    {
      memcpy (statb, &android_table[fd].statb,
	      sizeof *statb);
      return 0;
    }

  return fstat (fd, statb);
}

static int android_lookup_asset_directory_fd (int,
					      const char *restrict *,
					      const char *restrict);

/* Like fstatat.  However, if dirfd is AT_FDCWD and PATHNAME is an
   asset, find the information for the corresponding asset, and if
   dirfd is an offset into directory_tree as returned by
   `android_dirfd', find the information within the corresponding
   directory tree entry.  */

int
android_fstatat (int dirfd, const char *restrict pathname,
		 struct stat *restrict statbuf, int flags)
{
  AAsset *asset_desc;
  const char *asset;
  const char *asset_dir;
  int fd, rc;

  /* Look up whether or not DIRFD belongs to an open struct
     android_dir.  */

  if (dirfd != AT_FDCWD)
    dirfd
      = android_lookup_asset_directory_fd (dirfd, &pathname,
					   pathname);

  if (dirfd == AT_FDCWD
      && asset_manager
      && (asset = android_get_asset_name (pathname)))
    {
      /* Look up whether or not PATHNAME happens to be a
	 directory.  */
      asset_dir = android_scan_directory_tree ((char *) asset,
					       NULL);

      if (!asset_dir)
	{
	  errno = ENOENT;
	  return -1;
	}

      if (android_is_directory (asset_dir))
	{
	  memset (statbuf, 0, sizeof *statbuf);

	  /* Fill in the stat buffer.  */
	  statbuf->st_mode = S_IFDIR | S_IRUSR | S_IRGRP | S_IROTH;
	  return 0;
	}

      /* AASSET_MODE_STREAMING is fastest here.  */
      asset_desc = AAssetManager_open (asset_manager, asset,
				       AASSET_MODE_STREAMING);

      if (!asset_desc)
	return ENOENT;

      memset (statbuf, 0, sizeof *statbuf);

      /* Fill in the stat buffer.  */
      statbuf->st_mode = S_IFREG | S_IRUSR | S_IRGRP | S_IROTH;
      statbuf->st_size = AAsset_getLength (asset_desc);

      /* Close the asset.  */
      AAsset_close (asset_desc);
      return 0;
    }

  if (dirfd == AT_FDCWD
      && android_init_gui
      && android_content_name_p (pathname))
    {
      /* This is actually a content:// URI.  Open that file and call
	 stat on it.  */

      fd = android_open (pathname, O_RDONLY, 0);

      if (fd < 0)
	return -1;

      rc = fstat (fd, statbuf);
      android_close (fd);
      return rc;
    }

  return fstatat (dirfd, pathname, statbuf, flags);
}

/* Return if NAME, a file name relative to the /assets directory, is
   accessible, as long as !(amode & W_OK).  */

static bool
android_file_access_p (const char *name, int amode)
{
  if (!asset_manager)
    return false;

  if (!(amode & W_OK))
    {
      if (!strcmp (name, "") || !strcmp (name, "/"))
	/* /assets always exists.  */
	return true;

      /* Check if the file exists by looking in the ``directory tree''
	 asset generated during the build process.  This is used
	 instead of the AAsset functions, because the latter are
	 buggy and treat directories inconsistently.  */
      return android_scan_directory_tree ((char *) name, NULL) != NULL;
    }

  return false;
}

/* Do the same as android_hack_asset_fd, but use an unlinked temporary
   file to cater to old Android kernels where ashmem files are not
   readable.  */

static int
android_hack_asset_fd_fallback (AAsset *asset)
{
  int fd;
  char filename[PATH_MAX];
  size_t size;
  void *mem;

  /* Assets must be small enough to fit in size_t, if off_t is
     larger.  */
  size = AAsset_getLength (asset);

  /* Get an unlinked file descriptor from a file in the cache
     directory, which is guaranteed to only be written to by Emacs.
     Creating an ashmem file descriptor and reading from it doesn't
     work on these old Android versions.  */

  snprintf (filename, PATH_MAX, "%s/temp~unlinked.%d",
	    android_cache_dir, getpid ());
  fd = open (filename, O_CREAT | O_RDWR | O_TRUNC,
	     S_IRUSR | S_IWUSR);

  if (fd < 0)
    return -1;

  if (unlink (filename))
    goto fail;

  if (ftruncate (fd, size))
    goto fail;

  mem = mmap (NULL, size, PROT_WRITE, MAP_SHARED, fd, 0);
  if (mem == MAP_FAILED)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "mmap: %s", strerror (errno));
      goto fail;
    }

  if (AAsset_read (asset, mem, size) != size)
    {
      /* Too little was read.  Close the file descriptor and
	 report an error.  */
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "AAsset_read: %s", strerror (errno));
      goto fail;
    }

  munmap (mem, size);
  return fd;

 fail:
  close (fd);
  return -1;
}

/* Pointer to the `ASharedMemory_create' function which is loaded
   dynamically.  */
static int (*asharedmemory_create) (const char *, size_t);

/* Return whether or not shared memory file descriptors can also be
   read from, and are thus suitable for creating asset files.

   This does not work on some ancient Android systems running old
   versions of the kernel.  */

static bool
android_detect_ashmem (void)
{
  int fd, rc;
  void *mem;
  char test_buffer[10];

  memcpy (test_buffer, "abcdefghi", 10);

  /* Create the file descriptor to be used for the test.  */

  /* Android 28 and earlier let Emacs access /dev/ashmem directly, so
     prefer that over using ASharedMemory.  */

  if (android_api_level <= 28)
    {
      fd = open ("/dev/ashmem", O_RDWR);

      if (fd < 0)
	return false;

      /* An empty name means the memory area will exist until the file
	 descriptor is closed, because no other process can
	 attach.  */
      rc = ioctl (fd, ASHMEM_SET_NAME, "");

      if (rc < 0)
	{
	  close (fd);
	  return false;
	}

      rc = ioctl (fd, ASHMEM_SET_SIZE, sizeof test_buffer);

      if (rc < 0)
	{
	  close (fd);
	  return false;
	}
    }
  else
    {
      /* On the other hand, SELinux restrictions on Android 29 and
	 later require that Emacs use a system service to obtain
	 shared memory.  Load this dynamically, as this service is not
	 available on all versions of the NDK.  */

      if (!asharedmemory_create)
	{
	  *(void **) (&asharedmemory_create)
	    = dlsym (RTLD_DEFAULT, "ASharedMemory_create");

	  if (!asharedmemory_create)
	    {
	      __android_log_print (ANDROID_LOG_FATAL, __func__,
				   "dlsym: %s\n",
				   strerror (errno));
	      emacs_abort ();
	    }
	}

      fd = asharedmemory_create ("", sizeof test_buffer);

      if (fd < 0)
	return false;
    }

  /* Now map the resource and write the test contents.  */

  mem = mmap (NULL, sizeof test_buffer, PROT_WRITE,
	      MAP_SHARED, fd, 0);
  if (mem == MAP_FAILED)
    {
      close (fd);
      return false;
    }

  /* Copy over the test contents.  */
  memcpy (mem, test_buffer, sizeof test_buffer);

  /* Return anyway even if munmap fails.  */
  munmap (mem, sizeof test_buffer);

  /* Try to read the content back into test_buffer.  If this does not
     compare equal to the original string, or the read fails, then
     ashmem descriptors are not readable on this system.  */

  if ((read (fd, test_buffer, sizeof test_buffer)
       != sizeof test_buffer)
      || memcmp (test_buffer, "abcdefghi", sizeof test_buffer))
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "/dev/ashmem does not produce real"
			   " temporary files on this system, so"
			   " Emacs will fall back to creating"
			   " unlinked temporary files.");
      close (fd);
      return false;
    }

  close (fd);
  return true;
}

/* Get a file descriptor backed by a temporary in-memory file for the
   given asset.  */

static int
android_hack_asset_fd (AAsset *asset)
{
  static bool ashmem_readable_p;
  static bool ashmem_initialized;
  int fd, rc;
  unsigned char *mem;
  size_t size;

  /* The first time this function is called, try to determine whether
     or not ashmem file descriptors can be read from.  */

  if (!ashmem_initialized)
    ashmem_readable_p
      = android_detect_ashmem ();
  ashmem_initialized = true;

  /* If it isn't, fall back.  */

  if (!ashmem_readable_p)
    return android_hack_asset_fd_fallback (asset);

  /* Assets must be small enough to fit in size_t, if off_t is
     larger.  */
  size = AAsset_getLength (asset);

  /* Android 28 and earlier let Emacs access /dev/ashmem directly, so
     prefer that over using ASharedMemory.  */

  if (android_api_level <= 28)
    {
      fd = open ("/dev/ashmem", O_RDWR);

      if (fd < 0)
	return -1;

      /* An empty name means the memory area will exist until the file
	 descriptor is closed, because no other process can
	 attach.  */
      rc = ioctl (fd, ASHMEM_SET_NAME, "");

      if (rc < 0)
	{
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "ioctl ASHMEM_SET_NAME: %s",
			       strerror (errno));
	  close (fd);
	  return -1;
	}

      rc = ioctl (fd, ASHMEM_SET_SIZE, size);

      if (rc < 0)
	{
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "ioctl ASHMEM_SET_SIZE: %s",
			       strerror (errno));
	  close (fd);
	  return -1;
	}

      if (!size)
	return fd;

      /* Now map the resource.  */
      mem = mmap (NULL, size, PROT_WRITE, MAP_SHARED, fd, 0);
      if (mem == MAP_FAILED)
	{
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "mmap: %s", strerror (errno));
	  close (fd);
	  return -1;
	}

      if (AAsset_read (asset, mem, size) != size)
	{
	  /* Too little was read.  Close the file descriptor and
	     report an error.  */
	  __android_log_print (ANDROID_LOG_ERROR, __func__,
			       "AAsset_read: %s", strerror (errno));
	  close (fd);
	  return -1;
	}

      /* Return anyway even if munmap fails.  */
      munmap (mem, size);
      return fd;
    }

  /* On the other hand, SELinux restrictions on Android 29 and later
     require that Emacs use a system service to obtain shared memory.
     Load this dynamically, as this service is not available on all
     versions of the NDK.  */

  if (!asharedmemory_create)
    {
      *(void **) (&asharedmemory_create)
	= dlsym (RTLD_DEFAULT, "ASharedMemory_create");

      if (!asharedmemory_create)
	{
	  __android_log_print (ANDROID_LOG_FATAL, __func__,
			       "dlsym: %s\n",
			       strerror (errno));
	  emacs_abort ();
	}
    }

  fd = asharedmemory_create ("", size);

  if (fd < 0)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "ASharedMemory_create: %s",
			   strerror (errno));
      return -1;
    }

  /* Now map the resource.  */
  mem = mmap (NULL, size, PROT_WRITE, MAP_SHARED, fd, 0);
  if (mem == MAP_FAILED)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "mmap: %s", strerror (errno));
      close (fd);
      return -1;
    }

  if (AAsset_read (asset, mem, size) != size)
    {
      /* Too little was read.  Close the file descriptor and
	 report an error.  */
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "AAsset_read: %s", strerror (errno));
      close (fd);
      return -1;
    }

  /* Return anyway even if munmap fails.  */
  munmap (mem, size);
  return fd;
}

/* Make FD close-on-exec.  If any system call fails, do not abort, but
   log a warning to the system log.  */

static void
android_close_on_exec (int fd)
{
  int flags, rc;

  flags = fcntl (fd, F_GETFD);

  if (flags < 0)
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "fcntl: %s", strerror (errno));
      return;
    }

  rc = fcntl (fd, F_SETFD, flags | O_CLOEXEC);

  if (rc < 0)
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "fcntl: %s", strerror (errno));
      return;
    }
}

/* `open' and such are modified even though they exist on Android,
   because Emacs treats "/assets/" as a special directory that must
   contain all assets in the application package.  */

int
android_open (const char *filename, int oflag, mode_t mode)
{
  const char *name;
  AAsset *asset;
  int fd;
  size_t length;
  jobject string;

  if (asset_manager && (name = android_get_asset_name (filename)))
    {
      /* If Emacs is trying to write to the file, return NULL.  */

      if (oflag & O_WRONLY || oflag & O_RDWR)
	{
	  errno = EROFS;
	  return -1;
	}

      if (oflag & O_DIRECTORY)
	{
	  errno = ENOTSUP;
	  return -1;
	}

      /* If given AASSET_MODE_BUFFER (which is what Emacs probably
	 does, given that a file descriptor is not always available),
	 the framework fails to uncompress the data before it returns
	 a file descriptor.  */
      asset = AAssetManager_open (asset_manager, name,
				  AASSET_MODE_STREAMING);

      if (!asset)
	{
	  errno = ENOENT;
	  return -1;
	}

      /* Create a shared memory file descriptor containing the asset
	 contents.

         The documentation misleads people into thinking that
         AAsset_openFileDescriptor does precisely this.  However, it
         instead returns an offset into any uncompressed assets in the
         ZIP archive.  This cannot be found in its documentation.  */

      fd = android_hack_asset_fd (asset);

      if (fd == -1)
	{
	  AAsset_close (asset);
	  errno = ENXIO;
	  return -1;
	}

      /* If O_CLOEXEC is specified, make the file descriptor close on
	 exec too.  */
      if (oflag & O_CLOEXEC)
	android_close_on_exec (fd);

      if (fd >= ANDROID_MAX_ASSET_FD || fd < 0)
	{
	  /* Too bad.  Pretend this is an out of memory error.  */
	  errno = ENOMEM;

	  if (fd >= 0)
	    close (fd);

	  fd = -1;
	}
      else
	{
	  assert (!(android_table[fd].flags
		    & ANDROID_FD_TABLE_ENTRY_IS_VALID));
	  android_table[fd].flags = ANDROID_FD_TABLE_ENTRY_IS_VALID;
	  memset (&android_table[fd].statb, 0,
		  sizeof android_table[fd].statb);

	  /* Fill in some information that will be reported to
	     callers of android_fstat, among others.  */
	  android_table[fd].statb.st_mode
	    = S_IFREG | S_IRUSR | S_IRGRP | S_IROTH;

	  /* Owned by root.  */
	  android_table[fd].statb.st_uid = 0;
	  android_table[fd].statb.st_gid = 0;

	  /* Size of the file.  */
	  android_table[fd].statb.st_size
	    = AAsset_getLength (asset);
	}

      AAsset_close (asset);
      return fd;
    }

  if (android_init_gui && android_content_name_p (filename))
    {
      /* This is a content:// URI.  Ask the system for a descriptor to
	 that file.  */

      name = android_get_content_name (filename);
      length = strlen (name);

      /* Check if the mode is valid.  */

      if (oflag & O_DIRECTORY)
	{
	  errno = ENOTSUP;
	  return -1;
	}

      /* Allocate a buffer to hold the file name.  */
      string = (*android_java_env)->NewByteArray (android_java_env,
						  length);
      if (!string)
	{
	  (*android_java_env)->ExceptionClear (android_java_env);
	  errno = ENOMEM;
	  return -1;
	}
      (*android_java_env)->SetByteArrayRegion (android_java_env,
					       string, 0, length,
					       (jbyte *) name);

      /* Try to open the file descriptor.  */

      fd
	= (*android_java_env)->CallIntMethod (android_java_env,
					      emacs_service,
					      service_class.open_content_uri,
					      string,
					      (jboolean) ((mode & O_WRONLY
							   || mode & O_RDWR)
							  != 0),
					      (jboolean) !(mode & O_WRONLY),
					      (jboolean) ((mode & O_TRUNC)
							  != 0));

      if ((*android_java_env)->ExceptionCheck (android_java_env))
	{
	  (*android_java_env)->ExceptionClear (android_java_env);
	  errno = ENOMEM;
	  ANDROID_DELETE_LOCAL_REF (string);
	  return -1;
	}

      /* If fd is -1, just assume that the file does not exist,
	 and return -1 with errno set to ENOENT.  */

      if (fd == -1)
	{
	  errno = ENOENT;
	  goto skip;
	}

      if (mode & O_CLOEXEC)
	android_close_on_exec (fd);

    skip:
      ANDROID_DELETE_LOCAL_REF (string);
      return fd;
    }

  return open (filename, oflag, mode);
}

/* Like close.  However, remove the file descriptor from the asset
   table as well.  */

int
android_close (int fd)
{
  if (fd < ANDROID_MAX_ASSET_FD)
    android_table[fd].flags = 0;

  return close (fd);
}

/* Like fclose.  However, remove any information associated with
   FILE's file descriptor from the asset table as well.  */

int
android_fclose (FILE *stream)
{
  int fd;

  fd = fileno (stream);

  if (fd != -1 && fd < ANDROID_MAX_ASSET_FD)
    android_table[fd].flags = 0;

  return fclose (stream);
}

/* Return the current user's ``home'' directory, which is actually the
   app data directory on Android.  */

const char *
android_get_home_directory (void)
{
  return android_files_dir;
}

/* Return the name of the file behind a file descriptor FD by reading
   /proc/self/fd/.  Place the name in BUFFER, which should be able to
   hold size bytes.  Value is 0 upon success, and 1 upon failure.  */

static int
android_proc_name (int fd, char *buffer, size_t size)
{
  char format[sizeof "/proc/self/fd/"
	      + INT_STRLEN_BOUND (int)];
  ssize_t read;

  sprintf (format, "/proc/self/fd/%d", fd);
  read = readlink (format, buffer, size - 1);

  if (read == -1)
    return 1;

  buffer[read] = '\0';
  return 0;
}



/* JNI functions called by Java.  */

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-prototypes"
#endif

JNIEXPORT jint JNICALL
NATIVE_NAME (dup) (JNIEnv *env, jobject object, jint fd)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  return dup (fd);
}

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
			      jobject class_path,
			      jobject emacs_service_object)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  int pipefd[2];
  pthread_t thread;
  const char *java_string;
  AAsset *asset;

  /* This may be called from multiple threads.  setEmacsParams should
     only ever be called once.  */
  if (__atomic_fetch_add (&emacs_initialized, -1, __ATOMIC_SEQ_CST))
    {
      ANDROID_THROW (env, "java/lang/IllegalArgumentException",
		     "Emacs was already initialized!");
      return;
    }

  android_pixel_density_x = pixel_density_x;
  android_pixel_density_y = pixel_density_y;

  __android_log_print (ANDROID_LOG_INFO, __func__,
		       "Initializing "PACKAGE_STRING"...\nPlease report bugs to "
		       PACKAGE_BUGREPORT".  Thanks.\n");

  /* Set the asset manager.  */
  asset_manager = AAssetManager_fromJava (env, local_asset_manager);

  /* Initialize the directory tree.  */
  asset = AAssetManager_open (asset_manager, "directory-tree",
			      AASSET_MODE_BUFFER);

  if (!asset)
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "Failed to open directory tree");
      emacs_abort ();
    }

  directory_tree = AAsset_getBuffer (asset);

  if (!directory_tree)
    emacs_abort ();

  /* Now figure out how big the directory tree is, and compare the
     first few bytes.  */
  directory_tree_size = AAsset_getLength (asset);
  if (directory_tree_size < 5
      || memcmp (directory_tree, "EMACS", 5))
    {
      __android_log_print (ANDROID_LOG_FATAL, __func__,
			   "Directory tree has bad magic");
      emacs_abort ();
    }

  /* Hold a VM reference to the asset manager to prevent the native
     object from being deleted.  */
  (*env)->NewGlobalRef (env, local_asset_manager);

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

      if (!android_files_dir)
	emacs_abort ();

      (*env)->ReleaseStringUTFChars (env, (jstring) class_path,
				     java_string);
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

  /* Set LD_LIBRARY_PATH to an appropriate value.  */
  setenv ("LD_LIBRARY_PATH", android_lib_dir, 1);

  /* Make a reference to the Emacs service.  */

  if (emacs_service_object)
    {
      emacs_service = (*env)->NewGlobalRef (env, emacs_service_object);

      if (!emacs_service)
	emacs_abort ();
    }

  /* Set up events.  */
  android_init_events ();

  /* OK, setup is now complete.  The caller may start the Emacs thread
     now.  */
}

JNIEXPORT jobject JNICALL
NATIVE_NAME (getProcName) (JNIEnv *env, jobject object, jint fd)
{
  JNI_STACK_ALIGNMENT_PROLOGUE;

  char buffer[PATH_MAX + 1];
  size_t length;
  jbyteArray array;

  if (android_proc_name (fd, buffer, PATH_MAX + 1))
    return NULL;

  /* Return a byte array, as Java strings cannot always encode file
     names.  */
  length = strlen (buffer);
  array = (*env)->NewByteArray (env, length);
  if (!array)
    return NULL;

  (*env)->SetByteArrayRegion (env, array, 0, length,
			      (jbyte *) buffer);

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
  assert (service_class.c_name);

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
  FIND_METHOD (clear_window, "clearWindow",
	       "(Lorg/gnu/emacs/EmacsWindow;)V");
  FIND_METHOD (clear_area, "clearArea",
	       "(Lorg/gnu/emacs/EmacsWindow;IIII)V");
  FIND_METHOD (ring_bell, "ringBell", "()V");
  FIND_METHOD (query_tree, "queryTree",
	       "(Lorg/gnu/emacs/EmacsWindow;)[S");
  FIND_METHOD (get_screen_width, "getScreenWidth", "(Z)I");
  FIND_METHOD (get_screen_height, "getScreenHeight", "(Z)I");
  FIND_METHOD (detect_mouse, "detectMouse", "()Z");
  FIND_METHOD (name_keysym, "nameKeysym", "(I)Ljava/lang/String;");
  FIND_METHOD (browse_url, "browseUrl", "(Ljava/lang/String;)"
	       "Ljava/lang/String;");
  FIND_METHOD (restart_emacs, "restartEmacs", "()V");
  FIND_METHOD (update_ic, "updateIC",
	       "(Lorg/gnu/emacs/EmacsWindow;IIII)V");
  FIND_METHOD (reset_ic, "resetIC",
	       "(Lorg/gnu/emacs/EmacsWindow;I)V");
  FIND_METHOD (open_content_uri, "openContentUri",
	       "([BZZZ)I");
  FIND_METHOD (check_content_uri, "checkContentUri",
	       "([BZZ)Z");
  FIND_METHOD (query_battery, "queryBattery", "()[J");
  FIND_METHOD (display_toast, "displayToast",
	       "(Ljava/lang/String;)V");
  FIND_METHOD (update_extracted_text, "updateExtractedText",
	       "(Lorg/gnu/emacs/EmacsWindow;"
	       "Landroid/view/inputmethod/ExtractedText;I)V");
  FIND_METHOD (update_cursor_anchor_info, "updateCursorAnchorInfo",
	       "(Lorg/gnu/emacs/EmacsWindow;FFFF)V");
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
  assert (pixmap_class.c_name);

  FIND_METHOD (constructor, "<init>", "(S[IIII)V");
  FIND_METHOD (constructor_mutable, "<init>", "(SIII)V");

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
  assert (point_class.c_name);

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
  assert (drawable_class.c_name);

  FIND_METHOD (get_bitmap, "getBitmap", "()Landroid/graphics/Bitmap;");
  FIND_METHOD (damage_rect, "damageRect", "(Landroid/graphics/Rect;)V");
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
  assert (window_class.c_name);

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
  FIND_METHOD (make_input_focus, "makeInputFocus", "(J)V");
  FIND_METHOD (raise, "raise", "()V");
  FIND_METHOD (lower, "lower", "()V");
  FIND_METHOD (get_window_geometry, "getWindowGeometry",
	       "()[I");
  FIND_METHOD (translate_coordinates, "translateCoordinates",
	       "(II)[I");
  FIND_METHOD (set_dont_focus_on_map, "setDontFocusOnMap", "(Z)V");
  FIND_METHOD (set_dont_accept_focus, "setDontAcceptFocus", "(Z)V");
  FIND_METHOD (define_cursor, "defineCursor",
	       "(Lorg/gnu/emacs/EmacsCursor;)V");
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
  assert (cursor_class.c_name);

  FIND_METHOD (constructor, "<init>", "(SI)V");
#undef FIND_METHOD
}

JNIEXPORT void JNICALL
NATIVE_NAME (initEmacs) (JNIEnv *env, jobject object, jarray argv,
			 jobject dump_file_object, jint api_level)
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

  /* Set the Android API level.  */
  android_api_level = api_level;

  android_java_env = env;

  nelements = (*env)->GetArrayLength (env, argv);
  c_argv = alloca (sizeof *c_argv * nelements);

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

  android_init_emacs_service ();
  android_init_emacs_pixmap ();
  android_init_graphics_point ();
  android_init_emacs_drawable ();
  android_init_emacs_window ();
  android_init_emacs_cursor ();

  /* Set HOME to the app data directory.  */
  setenv ("HOME", android_files_dir, 1);

  /* Set TMPDIR to the temporary files directory.  */
  setenv ("TMPDIR", android_cache_dir, 1);

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

      /* Copy the Java string data once.  */
      dump_file = strdup (c_argument);

      /* Release the Java string data.  */
      (*env)->ReleaseStringUTFChars (env, (jstring) dump_file_object,
				     c_argument);
    }

  /* Delete local references to objects that are no longer needed.  */
  ANDROID_DELETE_LOCAL_REF (argv);
  ANDROID_DELETE_LOCAL_REF (dump_file_object);

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

  /* Raise sigio to interrupt anything that could be reading
     input.  */
  Vquit_flag = Qt;
  raise (SIGIO);
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendConfigureNotify) (JNIEnv *env, jobject object,
				   jshort window, jlong time,
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
			    jshort window, jlong time,
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

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendKeyRelease) (JNIEnv *env, jobject object,
			      jshort window, jlong time,
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

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendFocusIn) (JNIEnv *env, jobject object,
			   jshort window, jlong time)
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
			    jshort window, jlong time)
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
				jshort window, jint action)
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
			       jshort window, jint x, jint y,
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
			       jshort window, jint x, jint y,
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
				jshort window, jint x, jint y,
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
			       jshort window, jint x, jint y,
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
				 jshort window, jint x, jint y,
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
			     jshort window, jint x, jint y,
			     jlong time, jint pointer_id)
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

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendTouchUp) (JNIEnv *env, jobject object,
			   jshort window, jint x, jint y,
			   jlong time, jint pointer_id)
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

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendTouchMove) (JNIEnv *env, jobject object,
			     jshort window, jint x, jint y,
			     jlong time, jint pointer_id)
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

  android_write_event (&event);
  return event_serial;
}

JNIEXPORT jlong JNICALL
NATIVE_NAME (sendWheel) (JNIEnv *env, jobject object,
			 jshort window, jint x, jint y,
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
			     jshort window)
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
			       jshort window)
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
			       jshort window, jint menu_event_id,
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
			  jshort window, jint x, jint y,
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

JNIEXPORT jboolean JNICALL
NATIVE_NAME (shouldForwardMultimediaButtons) (JNIEnv *env,
					      jobject object)
{
  /* Yes, android_pass_multimedia_buttons_to_system is being
     read from the UI thread.  */
  return !android_pass_multimedia_buttons_to_system;
}

/* Forward declarations of deadlock prevention functions.  */

static void android_begin_query (void);
static void android_end_query (void);

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

struct android_handle_entry
{
  /* The type.  */
  enum android_handle_type type;

  /* The handle.  */
  jobject handle;
};

/* Table of handles MAX_HANDLE long.  */
struct android_handle_entry android_handles[USHRT_MAX];

/* The largest handle ID currently known, but subject to
   wraparound.  */
static android_handle max_handle;

/* Allocate a new, unused, handle identifier.  If Emacs is out of
   identifiers, return 0.  */

static android_handle
android_alloc_id (void)
{
  android_handle handle;

  /* 0 is never a valid handle ID.  */

  if (!max_handle)
    max_handle++;

  /* See if the handle is already occupied.  */

  if (android_handles[max_handle].handle)
    {
      /* Look for a fresh unoccupied handle.  */

      handle = max_handle;
      max_handle++;

      while (handle != max_handle)
	{
	  ++max_handle;

	  /* Make sure the handle is valid.  */
	  if (!max_handle)
	    ++max_handle;

	  if (!android_handles[max_handle].handle)
	    return max_handle++;
	}

      return ANDROID_NONE;
    }

  return max_handle++;
}

/* Destroy the specified handle and mark it as free on the Java side
   as well.  */

static void
android_destroy_handle (android_handle handle)
{
  static jclass old, class;
  static jmethodID method;

  if (!android_handles[handle].handle)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Trying to destroy free handle!");
      emacs_abort ();
    }

  if (!class)
    {
      class
	= (*android_java_env)->FindClass (android_java_env,
					  "org/gnu/emacs/EmacsHandleObject");
      assert (class != NULL);

      method
	= (*android_java_env)->GetMethodID (android_java_env, class,
					    "destroyHandle", "()V");
      assert (method != NULL);

      old = class;
      class
	= (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						      (jobject) class);
      android_exception_check_1 (old);
      ANDROID_DELETE_LOCAL_REF (old);
    }

  (*android_java_env)->CallVoidMethod (android_java_env,
				       android_handles[handle].handle,
				       method);

  /* Just clear any exception thrown.  If destroying the handle
     fails from an out-of-memory error, then Emacs loses some
     resources, but that is not as big deal as signalling.  */
  (*android_java_env)->ExceptionClear (android_java_env);

  /* Delete the global reference regardless of any error.  */
  (*android_java_env)->DeleteGlobalRef (android_java_env,
					android_handles[handle].handle);
  android_handles[handle].handle = NULL;
}

jobject
android_resolve_handle (android_handle handle,
			enum android_handle_type type)
{
  if (!handle)
    /* ANDROID_NONE.  */
    return NULL;

  /* CheckJNI will normally ensure that the handle exists and is
     the right type, but with a less informative error message.
     Don't waste cycles doing our own checking here.  */

#ifdef ENABLE_CHECKING

  if (!android_handles[handle].handle)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Trying to resolve free handle!");
      emacs_abort ();
    }

  if (android_handles[handle].type != type)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Handle has wrong type!");
      emacs_abort ();
    }

#endif /* ENABLE_CHECKING */

  return android_handles[handle].handle;
}

static jobject
android_resolve_handle2 (android_handle handle,
			 enum android_handle_type type,
			 enum android_handle_type type2)
{
  if (!handle)
    return NULL;

  /* CheckJNI will normally ensure that the handle exists and is
     the right type, but with a less informative error message.
     Don't waste cycles doing our own checking here.  */

#ifdef ENABLE_CHECKING

  if (!android_handles[handle].handle)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Trying to resolve free handle!");
      emacs_abort ();
    }

  if (android_handles[handle].type != type
      && android_handles[handle].type != type2)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Handle has wrong type!");
      emacs_abort ();
    }

#endif /* ENABLE_CHECKING */

  return android_handles[handle].handle;
}

void
android_change_window_attributes (android_window handle,
				  enum android_window_value_mask value_mask,
				  struct android_set_window_attributes *attrs)
{
  jmethodID method;
  jobject window;
  jint pixel;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

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
  android_handle prev_max_handle;
  bool override_redirect;

  parent_object = android_resolve_handle (parent, ANDROID_HANDLE_WINDOW);

  prev_max_handle = max_handle;
  window = android_alloc_id ();

  if (!window)
    error ("Out of window handles!");

  if (!class)
    {
      class = (*android_java_env)->FindClass (android_java_env,
					      "org/gnu/emacs/EmacsWindow");
      assert (class != NULL);

      constructor
	= (*android_java_env)->GetMethodID (android_java_env, class, "<init>",
					    "(SLorg/gnu/emacs/EmacsWindow;"
					    "IIIIZ)V");
      assert (constructor != NULL);

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
					   constructor, (jshort) window,
					   parent_object, (jint) x, (jint) y,
					   (jint) width, (jint) height,
					   (jboolean) override_redirect);
  if (!object)
    {
      (*android_java_env)->ExceptionClear (android_java_env);

      max_handle = prev_max_handle;
      memory_full (0);
    }

  android_handles[window].type = ANDROID_HANDLE_WINDOW;
  android_handles[window].handle
    = (*android_java_env)->NewGlobalRef (android_java_env,
					 object);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (object);

  if (!android_handles[window].handle)
    memory_full (0);

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
  if (android_handles[window].type != ANDROID_HANDLE_WINDOW)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Trying to destroy something not a window!");
      emacs_abort ();
    }

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
  assert (android_rect_class);

  android_rect_constructor
    = (*android_java_env)->GetMethodID (android_java_env, android_rect_class,
					"<init>", "(IIII)V");
  assert (emacs_gc_constructor);

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
  assert (emacs_gc_class);

  emacs_gc_constructor
    = (*android_java_env)->GetMethodID (android_java_env,
					emacs_gc_class,
					"<init>", "(S)V");
  assert (emacs_gc_constructor);

  emacs_gc_mark_dirty
    = (*android_java_env)->GetMethodID (android_java_env,
					emacs_gc_class,
					"markDirty", "(Z)V");
  assert (emacs_gc_mark_dirty);

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
}

struct android_gc *
android_create_gc (enum android_gc_value_mask mask,
		   struct android_gc_values *values)
{
  struct android_gc *gc;
  android_handle prev_max_handle;
  jobject object;

  android_init_emacs_gc_class ();

  gc = xmalloc (sizeof *gc);
  prev_max_handle = max_handle;
  gc->gcontext = android_alloc_id ();
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

  if (!gc->gcontext)
    {
      xfree (gc);
      error ("Out of GContext handles!");
    }

  object = (*android_java_env)->NewObject (android_java_env,
					   emacs_gc_class,
					   emacs_gc_constructor,
					   (jshort) gc->gcontext);

  if (!object)
    {
      (*android_java_env)->ExceptionClear (android_java_env);

      max_handle = prev_max_handle;
      memory_full (0);
    }

  android_handles[gc->gcontext].type = ANDROID_HANDLE_GCONTEXT;
  android_handles[gc->gcontext].handle
    = (*android_java_env)->NewGlobalRef (android_java_env, object);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (object);

  if (!android_handles[gc->gcontext].handle)
    memory_full (0);

  android_change_gc (gc, mask, values);
  return gc;
}

void
android_free_gc (struct android_gc *gc)
{
  android_destroy_handle (gc->gcontext);

  xfree (gc->clip_rects);
  xfree (gc);
}

void
android_change_gc (struct android_gc *gc,
		   enum android_gc_value_mask mask,
		   struct android_gc_values *values)
{
  jobject what, gcontext;
  jboolean clip_changed;

  clip_changed = false;

  android_init_emacs_gc_class ();
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

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
      what = android_resolve_handle (values->clip_mask,
				     ANDROID_HANDLE_PIXMAP);
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
      what = android_resolve_handle (values->stipple,
				     ANDROID_HANDLE_PIXMAP);
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

  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

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
android_reparent_window (android_window w, android_window parent_handle,
			 int x, int y)
{
  jobject window, parent;
  jmethodID method;

  window = android_resolve_handle (w, ANDROID_HANDLE_WINDOW);
  parent = android_resolve_handle (parent_handle,
				   ANDROID_HANDLE_WINDOW);

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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.clear_window,
						 window);
  android_exception_check ();
}

void
android_map_window (android_window handle)
{
  jobject window;
  jmethodID map_window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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
      window = android_resolve_handle (swap_info[i].swap_window,
				       ANDROID_HANDLE_WINDOW);
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
  jobject gcontext;

  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

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
    values->fill_style
      = (*android_java_env)->GetIntField (android_java_env,
					  gcontext,
					  emacs_gc_fill_style);

  if (mask & ANDROID_GC_TILE_STIP_X_ORIGIN)
    values->ts_x_origin
      = (*android_java_env)->GetIntField (android_java_env,
					  gcontext,
					  emacs_gc_ts_origin_x);

  if (mask & ANDROID_GC_TILE_STIP_Y_ORIGIN)
    values->ts_y_origin
      = (*android_java_env)->GetIntField (android_java_env,
					  gcontext,
					  emacs_gc_ts_origin_y);

  /* Fields involving handles are not used by Emacs, and thus not
     implemented */
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

  drawable = android_resolve_handle2 (handle,
				      ANDROID_HANDLE_WINDOW,
				      ANDROID_HANDLE_PIXMAP);
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

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
  android_handle prev_max_handle;
  jobject object;
  jintArray colors;
  android_pixmap pixmap;
  unsigned int x, y;
  jint *region;

  USE_SAFE_ALLOCA;

  /* Create the color array holding the data.  */
  colors = (*android_java_env)->NewIntArray (android_java_env,
					     width * height);
  android_exception_check ();

  SAFE_NALLOCA (region, sizeof *region, width);

  for (y = 0; y < height; ++y)
    {
      for (x = 0; x < width; ++x)
	{
	  if (depth == 24)
	    {
	      /* The alpha channels must be set, or otherwise, the
		 pixmap will be created entirely transparent.  */

	      if (data[x / 8] & (1 << (x % 8)))
		region[x] = foreground | 0xff000000;
	      else
		region[x] = background | 0xff000000;
	    }
	  else
	    {
	      if (data[x / 8] & (1 << (x % 8)))
		region[x] = foreground;
	      else
		region[x] = background;
	    }
	}

      (*android_java_env)->SetIntArrayRegion (android_java_env,
					      colors,
					      width * y, width,
					      region);
      data += width / 8;
    }

  /* First, allocate the pixmap handle.  */
  prev_max_handle = max_handle;
  pixmap = android_alloc_id ();

  if (!pixmap)
    {
      ANDROID_DELETE_LOCAL_REF ((jobject) colors);
      error ("Out of pixmap handles!");
    }

  object = (*android_java_env)->NewObject (android_java_env,
					   pixmap_class.class,
					   pixmap_class.constructor,
					   (jshort) pixmap, colors,
					   (jint) width, (jint) height,
					   (jint) depth);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF ((jobject) colors);

  if (!object)
    {
      max_handle = prev_max_handle;
      memory_full (0);
    }

  android_handles[pixmap].type = ANDROID_HANDLE_PIXMAP;
  android_handles[pixmap].handle
    = (*android_java_env)->NewGlobalRef (android_java_env, object);
  ANDROID_DELETE_LOCAL_REF (object);

  if (!android_handles[pixmap].handle)
    memory_full (0);

  SAFE_FREE ();
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

void
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
  int overflow, temp, i, xdir;
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

      overflow  = ckd_mul (&start, src_y + height - 1,
			   src_info->stride);
      if (mask) /* If a mask is set, put the pointers before the end
		   of the row.  */
	overflow |= ckd_mul (&end, src_x + width - 1, pixel);
      else
	overflow |= ckd_mul (&end, src_x, pixel);
      overflow |= ckd_add (&start, start, end);
      overflow |= ckd_add (&start, (uintptr_t) src, start);

      if (overflow)
	return;

      src_current = (unsigned char *) start;

      overflow  = ckd_mul (&start, dst_y + height - 1,
			   dst_info->stride);
      if (mask) /* If a mask is set, put the pointers before the end
		   of the row.  */
	overflow |= ckd_mul (&end, dst_x + width - 1, pixel);
      else
	overflow |= ckd_mul (&end, dst_x, pixel);
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

	  if (INT_MULTIPLY_WRAPV (temp, pixel, &offset)
	      || INT_MULTIPLY_WRAPV (i, mask_info->stride, &offset1)
	      || INT_ADD_WRAPV (offset, offset1, &offset)
	      || INT_ADD_WRAPV ((uintptr_t) mask, offset, &start))
	    return;

	  mask = mask_current = (unsigned char *) start;

	  while (--height)
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

	  if (temp <= 0)
	    return;

	  /* Copy bytes according to the mask.  */

	  while (--height)
	    {
	      long_src = (unsigned int *) src_current;
	      long_dst = (unsigned int *) dst_current;
	      mask_current = mask;

#ifndef __aarch64__
	      while (temp--)
		{
		  /* Sign extend the mask.  */
		  height = *(signed char *) mask_current++;

		  /* Apply the mask.  */
		  *long_dst = ((*long_src & height)
			       | (*long_dst & ~height));
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


/* Xor a rectangle SRC_X, SRC_Y, WIDTH and HEIGHT from SRC, described
   by SRC_INFO, to DST_X and DST_Y in DST, as described by DST_INFO.

   Ignore the alpha channel when computing the exclusive-or of the
   destination pixel.

   If MASK is set, mask the source data using MASK_INFO, translating
   it by GC->clip_x_origin and GC->clip_y_origin.  MASK must be a
   pixmap of depth 1.

   N.B. that currently only copies between bitmaps of depth 24 are
   implemented.  */

void
android_blit_xor (int src_x, int src_y, int width, int height,
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
  int overflow, temp, i, xdir;
  bool backwards;
  unsigned int *long_src, *long_dst;

  /* Note that this alu hasn't been tested -- it probably does not
     work! */
  emacs_abort ();

#if 0
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
  overflow |= ckd_add (&start, (uintptr_t) src, start);

  if (overflow)
    return;

  src_current = (unsigned char *) start;

  overflow  = ckd_mul (&start, dst_y, src_info->stride);
  overflow |= ckd_mul (&end, dst_x, pixel);
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

      overflow  = ckd_mul (&start, src_y + height - 1,
			   src_info->stride);
      if (mask) /* If a mask is set, put the pointers before the end
		   of the row.  */
	overflow |= ckd_mul (&end, src_x + width - 1, pixel);
      else
	overflow |= ckd_mul (&end, src_x, pixel);
      overflow |= ckd_add (&start, start, end);
      overflow |= ckd_add (&start, (uintptr_t) src, start);

      if (overflow)
	return;

      src_current = (unsigned char *) start;

      overflow  = ckd_mul (&start, dst_y + height - 1,
			   dst_info->stride);
      if (mask) /* If a mask is set, put the pointers before the end
		   of the row.  */
	overflow |= ckd_mul (&end, dst_x + width - 1, pixel);
      else
	overflow |= ckd_mul (&end, dst_x, pixel);
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
	  if (backwards)
	    {
	      for (i = width - 1; i <= 0; --i)
		(((unsigned int *) dst_current)[i])
		  /* Keep the alpha channel intact.  */
		  ^= (((unsigned int *) src_current)[i]) & 0xffffff;

	      /* Proceed to the last row.  */
	      src_current -= src_info->stride;
	      dst_current -= dst_info->stride;
	    }
	  else
	    {
	      for (i = 0; i < width; ++i)
		(((unsigned int *) dst_current)[i])
		  /* Keep the alpha channel intact.  */
		  ^= (((unsigned int *) src_current)[i]) & 0xffffff;

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

	  if (mask_info < 0)
	    mask_info = 0;

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

	  if (INT_MULTIPLY_WRAPV (temp, pixel, &offset)
	      || INT_MULTIPLY_WRAPV (i, mask_info->stride, &offset1)
	      || INT_ADD_WRAPV (offset, offset1, &offset)
	      || INT_ADD_WRAPV ((uintptr_t) mask, offset, &start))
	    return;

	  mask = mask_current = (unsigned char *) start;

	  for (i = 0; i < height; ++i)
	    {
	      /* Skip backwards past the end of the mask.  */

	      long_src = (unsigned int *) (src_current - mask_offset * pixel);
	      long_dst = (unsigned int *) (dst_current - mask_offset * pixel);
	      mask = mask_current;

	      /* For each pixel covered by the mask... */
	      temp = MIN (mask_info->width - temp, width - mask_offset);
	      while (temp--)
		/* XOR the source to the destination, masked by the
		   mask.  */
		*long_dst-- ^= ((*(long_src--) & (0u - (*(mask--) & 1)))
				& 0xffffff);

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
	      width -= mask_offset;
	    }

	  /* Now move mask to the position of the first row.  */

	  mask += gc->clip_y_origin * mask_info->stride;

	  for (i = 0; i < height; ++i)
	    {
	      long_src = (unsigned int *) src_current;
	      long_dst = (unsigned int *) dst_current;
	      mask_current = mask;

	      if (mask_offset > 0)
		{
		  /* Copy bytes according to the mask.  */
		  temp = MIN (mask_info->width - mask_offset, width);
		  while (temp--)
		    *long_dst++ ^= ((*(long_src++)
				     & (0u - (*(mask_current++) & 1)))
				    & 0xffffff);
		}
	      else
		{
		  /* Copy bytes according to the mask.  */
		  temp = MIN (mask_info->width, width);
		  while (temp--)
		    *long_dst++ = ((*(long_src++)
				     & (0u - (*(mask_current++) & 1)))
				    & 0xffffff);
		}

	      src_current += src_info->stride;
	      dst_current += dst_info->stride;
	      mask	  += mask_info->stride;
	    }
	}
    }
#endif /* 0 */
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

    case ANDROID_GC_XOR:
      do_blit = android_blit_xor;
      break;
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

  if (android_handles[dest].type == ANDROID_HANDLE_WINDOW)
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

  drawable_object = android_resolve_handle2 (drawable,
					     ANDROID_HANDLE_WINDOW,
					     ANDROID_HANDLE_PIXMAP);
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

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
  ANDROID_DELETE_LOCAL_REF (array);
}

void
android_draw_rectangle (android_drawable handle, struct android_gc *gc,
			int x, int y, unsigned int width, unsigned int height)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle2 (handle,
				      ANDROID_HANDLE_WINDOW,
				      ANDROID_HANDLE_PIXMAP);
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.draw_rectangle,
						 drawable, gcontext,
						 (jint) x, (jint) y,
						 (jint) width, (jint) height);
}

void
android_draw_point (android_drawable handle, struct android_gc *gc,
		    int x, int y)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle2 (handle,
				      ANDROID_HANDLE_WINDOW,
				      ANDROID_HANDLE_PIXMAP);
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.draw_point,
						 drawable, gcontext,
						 (jint) x, (jint) y);
}

void
android_draw_line (android_drawable handle, struct android_gc *gc,
		   int x, int y, int x2, int y2)
{
  jobject drawable, gcontext;

  drawable = android_resolve_handle2 (handle,
				      ANDROID_HANDLE_WINDOW,
				      ANDROID_HANDLE_PIXMAP);
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.draw_line,
						 drawable, gcontext,
						 (jint) x, (jint) y,
						 (jint) x2, (jint) y2);
}

android_pixmap
android_create_pixmap (unsigned int width, unsigned int height,
		       int depth)
{
  android_handle prev_max_handle;
  jobject object;
  android_pixmap pixmap;

  /* First, allocate the pixmap handle.  */
  prev_max_handle = max_handle;
  pixmap = android_alloc_id ();

  if (!pixmap)
    error ("Out of pixmap handles!");

  object = (*android_java_env)->NewObject (android_java_env,
					   pixmap_class.class,
					   pixmap_class.constructor_mutable,
					   (jshort) pixmap,
					   (jint) width, (jint) height,
					   (jint) depth);

  if (!object)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      max_handle = prev_max_handle;
      memory_full (0);
    }

  android_handles[pixmap].type = ANDROID_HANDLE_PIXMAP;
  android_handles[pixmap].handle
    = (*android_java_env)->NewGlobalRef (android_java_env, object);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (object);

  if (!android_handles[pixmap].handle)
    memory_full (0);

  return pixmap;
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.clear_area,
						 window, (jint) x, (jint) y,
						 (jint) width, (jint) height);
}

android_pixmap
android_create_bitmap_from_data (char *bits, unsigned int width,
				 unsigned int height)
{
  return android_create_pixmap_from_bitmap_data (bits, 1, 0,
						 width, height, 1);
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

  drawable = android_resolve_handle2 (handle, ANDROID_HANDLE_WINDOW,
				      ANDROID_HANDLE_PIXMAP);

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
      if (INT_MULTIPLY_WRAPV ((size_t) bitmap_info.stride,
			      (size_t) bitmap_info.height,
			      &byte_size))
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

  drawable = android_resolve_handle (handle, ANDROID_HANDLE_PIXMAP);

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
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.ring_bell);
  android_exception_check ();
}

void
android_set_input_focus (android_window handle, unsigned long time)
{
  jobject window;
  jmethodID make_input_focus;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  lower = window_class.lower;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 window,
						 window_class.class,
						 lower);
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
  jshort *shorts;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

  /* window can be NULL, so this is a service method.  */
  array
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     emacs_service,
					     service_class.query_tree,
					     window);
  android_exception_check ();

  /* The first element of the array is the parent window.  The rest
     are the children.  */
  nelements = (*android_java_env)->GetArrayLength (android_java_env,
						   array);
  eassert (nelements);

  /* Now fill in the children.  */
  children = xnmalloc (nelements - 1, sizeof *children);

  shorts
    = (*android_java_env)->GetShortArrayElements (android_java_env, array,
						  NULL);
  android_exception_check_nonnull (shorts, array);

  for (i = 1; i < nelements; ++i)
    children[i] = shorts[i];

  /* Finally, return the parent and other values.  */
  *root_return = 0;
  *parent_return = shorts[0];
  *children_return = children;
  *nchildren_return = nelements - 1;

  /* Release the array contents.  */
  (*android_java_env)->ReleaseShortArrayElements (android_java_env, array,
						  shorts, JNI_ABORT);

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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  get_geometry = window_class.get_window_geometry;

  window_geometry
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     window,
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
android_move_resize_window (android_window window, int x, int y,
			    unsigned int width, unsigned int height)
{
  android_move_window (window, x, y);
  android_resize_window (window, width, height);
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

  window = android_resolve_handle (src, ANDROID_HANDLE_WINDOW);
  method = window_class.translate_coordinates;
  coordinates
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     window, method,
					     (jint) x, (jint) y);
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

int
android_wc_lookup_string (android_key_pressed_event *event,
			  wchar_t *buffer_return, int wchars_buffer,
			  int *keysym_return,
			  enum android_lookup_status *status_return)
{
  enum android_lookup_status status;
  int rc;
  jobject window, string;
  const jchar *characters;
  jsize size;
  size_t i;

  status = ANDROID_LOOKUP_NONE;
  rc = 0;

  /* See if an actual lookup has to be made.  Note that while
     BUFFER_RETURN is wchar_t, the returned characters are always in
     UCS.  */

  if (event->unicode_char != (uint32_t) -1)
    {
      if (event->unicode_char)
	{
	  if (wchars_buffer < 1)
	    {
	      *status_return = ANDROID_BUFFER_OVERFLOW;
	      return 0;
	    }
	  else
	    {
	      buffer_return[0] = event->unicode_char;
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

      *status_return = status;

      return rc;
    }

  /* Now look up the window.  */
  rc = 0;

  if (!android_handles[event->window].handle
      || (android_handles[event->window].type
	  != ANDROID_HANDLE_WINDOW))
    status = ANDROID_LOOKUP_NONE;
  else
    {
      window = android_handles[event->window].handle;
      string
	= (*android_java_env)->CallObjectMethod (android_java_env, window,
						 window_class.lookup_string,
						 (jint) event->serial);
      android_exception_check ();

      if (!string)
	status = ANDROID_LOOKUP_NONE;
      else
	{
	  /* Now return this input method string.  */
	  characters = (*android_java_env)->GetStringChars (android_java_env,
							    string, NULL);
	  android_exception_check_1 (string);

	  /* Figure out how big the string is.  */
	  size = (*android_java_env)->GetStringLength (android_java_env,
						       string);

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

	  (*android_java_env)->ReleaseStringChars (android_java_env, string,
						   characters);
	  ANDROID_DELETE_LOCAL_REF (string);
	}
    }

  *status_return = status;
  return rc;
}



/* Low level drawing primitives.  */

/* Lock the bitmap corresponding to the drawable DRAWABLE.  Return the
   bitmap data upon success, and store the bitmap object in
   BITMAP_RETURN.  Value is NULL upon failure.

   The caller must take care to unlock the bitmap data afterwards.  */

unsigned char *
android_lock_bitmap (android_window drawable,
		     AndroidBitmapInfo *bitmap_info,
		     jobject *bitmap_return)
{
  jobject object, bitmap;
  void *data;

  object = android_resolve_handle2 (drawable, ANDROID_HANDLE_WINDOW,
				    ANDROID_HANDLE_PIXMAP);

  /* Look up the drawable and get the bitmap corresponding to it.
     Then, lock the bitmap's bits.  */
  bitmap = (*android_java_env)->CallObjectMethod (android_java_env,
						  object,
						  drawable_class.get_bitmap);
  if (!bitmap)
    /* NULL is returned when the bitmap does not currently exist due
       to ongoing reconfiguration on the main thread.  */
    return NULL;

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
  jobject drawable, rect;

  drawable = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

  /* Now turn DAMAGE into a Java rectangle.  */
  rect = (*android_java_env)->NewObject (android_java_env,
					 android_rect_class,
					 android_rect_constructor,
					 (jint) damage->x,
					 (jint) damage->y,
					 (jint) (damage->x
						 + damage->width),
					 (jint) (damage->y
						 + damage->height));
  android_exception_check ();

  /* Post the damage to the drawable.  */
  (*android_java_env)->CallVoidMethod (android_java_env,
				       drawable,
				       drawable_class.damage_rect,
				       rect);
  android_exception_check_1 (rect);
  ANDROID_DELETE_LOCAL_REF (rect);
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

void
android_set_dont_focus_on_map (android_window handle,
			       bool no_focus_on_map)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
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

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  method = window_class.set_dont_accept_focus;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, window,
						 window_class.class,
						 method,
						 (jboolean) no_accept_focus);
  android_exception_check ();
}

void
android_get_keysym_name (int keysym, char *name_return, size_t size)
{
  jobject string;
  const char *buffer;

  string = (*android_java_env)->CallObjectMethod (android_java_env,
						  emacs_service,
						  service_class.name_keysym,
						  (jint) keysym);
  android_exception_check ();

  buffer = (*android_java_env)->GetStringUTFChars (android_java_env,
						   (jstring) string,
						   NULL);
  android_exception_check_nonnull ((void *) buffer, string);
  strncpy (name_return, buffer, size - 1);

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

  object = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);
  method = window_class.toggle_on_screen_keyboard;

  /* Now display the on screen keyboard.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env, object,
						 window_class.class,
						 method, (jboolean) show);

  /* Check for out of memory errors.  */
  android_exception_check ();
}



/* When calling the system's faccessat, make sure to clear the flag
   AT_EACCESS.

   Android's faccessat simply fails upon using AT_EACCESS, so replace
   it with zero here.  This isn't caught during configuration as Emacs
   is being cross compiled.

   This replacement is only done when building for Android 16 or
   later, because earlier versions use the gnulib replacement that
   lacks these issues.

   This is unnecessary on earlier API versions, as gnulib's
   rpl_faccessat will be used instead, which lacks these problems.  */

/* Like faccessat, except it also understands DIRFD opened using
   android_dirfd.  */

int
android_faccessat (int dirfd, const char *pathname, int mode, int flags)
{
  const char *asset;

  if (dirfd != AT_FDCWD)
    dirfd
      = android_lookup_asset_directory_fd (dirfd, &pathname,
					   pathname);

  /* Check if pathname is actually an asset.  If that is the case,
     simply fall back to android_file_access_p.  */

  if (dirfd == AT_FDCWD
      && asset_manager
      && (asset = android_get_asset_name (pathname)))
    {
      if (android_file_access_p (asset, mode))
	return 0;

      /* Set errno to an appropriate value.  */
      errno = ENOENT;
      return 1;
    }

  /* Check if pathname is actually a content resolver URI.  */

  if (dirfd == AT_FDCWD
      && android_init_gui
      && android_content_name_p (pathname))
    {
      if (android_check_content_access (pathname, mode))
	return 0;

      /* Set errno to an appropriate value.  */
      errno = ENOENT;
      return 1;
    }

#if __ANDROID_API__ >= 16
  return faccessat (dirfd, pathname, mode, flags & ~AT_EACCESS);
#else
  return faccessat (dirfd, pathname, mode, flags);
#endif
}



/* Directory listing emulation.  */

struct android_dir
{
  /* The real DIR *, if it exists.  */
  DIR *dir;

  /* Otherwise, the pointer to the directory in directory_tree.  */
  char *asset_dir;

  /* And the end of the files in asset_dir.  */
  char *asset_limit;

  /* The next struct android_dir.  */
  struct android_dir *next;

  /* Path to the directory relative to /.  */
  char *asset_file;

  /* File descriptor used when asset_dir is set.  */
  int fd;
};

/* List of all struct android_dir's corresponding to an asset
   directory that are currently open.  */
static struct android_dir *android_dirs;

/* Like opendir.  However, return an asset directory if NAME points to
   an asset.  */

struct android_dir *
android_opendir (const char *name)
{
  struct android_dir *dir;
  char *asset_dir;
  const char *asset_name;
  size_t limit, length;

  asset_name = android_get_asset_name (name);

  /* If the asset manager exists and NAME is an asset, return an asset
     directory.  */
  if (asset_manager && asset_name)
    {
      asset_dir
	= (char *) android_scan_directory_tree ((char *) asset_name,
						&limit);

      if (!asset_dir)
	{
	  errno = ENOENT;
	  return NULL;
	}

      length = strlen (name);

      dir = xmalloc (sizeof *dir);
      dir->dir = NULL;
      dir->asset_dir = asset_dir;
      dir->asset_limit = (char *) directory_tree + limit;
      dir->fd = -1;
      dir->asset_file = xzalloc (length + 2);

      /* Make sure dir->asset_file is terminated with /.  */
      strcpy (dir->asset_file, name);
      if (dir->asset_file[length - 1] != '/')
	dir->asset_file[length] = '/';

      /* Make sure dir->asset_limit is within bounds.  It is a limit,
	 and as such can be exactly one byte past directory_tree.  */
      if (dir->asset_limit > directory_tree + directory_tree_size)
	{
	  xfree (dir);
	  __android_log_print (ANDROID_LOG_VERBOSE, __func__,
			       "Invalid dir tree, limit %zu, size %zu\n",
			       limit, directory_tree_size);
	  dir = NULL;
	  errno = EACCES;
	}

      dir->next = android_dirs;
      android_dirs = dir;

      return dir;
    }

  /* Otherwise, open the directory normally.  */
  dir = xmalloc (sizeof *dir);
  dir->asset_dir = NULL;
  dir->dir = opendir (name);

  if (!dir->dir)
    {
      xfree (dir);
      return NULL;
    }

  return dir;
}

/* Like dirfd.  However, value is not a real directory file descriptor
   if DIR is an asset directory.  */

int
android_dirfd (struct android_dir *dirp)
{
  int fd;

  if (dirp->dir)
    return dirfd (dirp->dir);
  else if (dirp->fd != -1)
    return dirp->fd;

  fd = open ("/dev/null", O_RDONLY | O_CLOEXEC);

  /* Record this file descriptor in dirp.  */
  dirp->fd = fd;
  return fd;
}

/* Like readdir, except it understands asset directories.  */

struct dirent *
android_readdir (struct android_dir *dir)
{
  static struct dirent dirent;
  const char *last;

  if (dir->asset_dir)
    {
      /* There are no more files to read.  */
      if (dir->asset_dir >= dir->asset_limit)
	return NULL;

      /* Otherwise, scan forward looking for the next NULL byte.  */
      last = memchr (dir->asset_dir, 0,
		     dir->asset_limit - dir->asset_dir);

      /* No more NULL bytes remain.  */
      if (!last)
	return NULL;

      /* Forward last past the NULL byte.  */
      last++;

      /* Make sure it is still within the directory tree.  */
      if (last >= directory_tree + directory_tree_size)
	return NULL;

      /* Now, fill in the dirent with the name.  */
      memset (&dirent, 0, sizeof dirent);
      dirent.d_ino = 0;
      dirent.d_off = 0;
      dirent.d_reclen = sizeof dirent;

      /* If this is not a directory, return DT_UNKNOWN.  Otherwise,
	 return DT_DIR.  */

      if (android_is_directory (dir->asset_dir))
	dirent.d_type = DT_DIR;
      else
	dirent.d_type = DT_UNKNOWN;

      /* Note that dir->asset_dir is actually a NULL terminated
	 string.  */
      memcpy (dirent.d_name, dir->asset_dir,
	      MIN (sizeof dirent.d_name,
		   last - dir->asset_dir));
      dirent.d_name[sizeof dirent.d_name - 1] = '\0';

      /* Strip off the trailing slash, if any.  */
      if (dirent.d_name[MIN (sizeof dirent.d_name,
			     last - dir->asset_dir)
			- 2] == '/')
	dirent.d_name[MIN (sizeof dirent.d_name,
			   last - dir->asset_dir)
		      - 2] = '\0';

      /* Finally, forward dir->asset_dir to the file past last.  */
      dir->asset_dir = ((char *) directory_tree
			+ android_extract_long ((char *) last));

      return &dirent;
    }

  return readdir (dir->dir);
}

/* Like closedir, but it also closes asset manager directories.  */

void
android_closedir (struct android_dir *dir)
{
  struct android_dir **next, *tem;

  if (dir->dir)
    closedir (dir->dir);
  else
    {
      if (dir->fd != -1)
	close (dir->fd);

      /* Unlink this directory from the list of all asset manager
	 directories.  */

      for (next = &android_dirs; (tem = *next);)
	{
	  if (tem == dir)
	    *next = dir->next;
	  else
	    next = &(*next)->next;
	}

      /* Free the asset file name.  */
      xfree (dir->asset_file);
    }

  /* There is no need to close anything else, as the directory tree
     lies in statically allocated memory.  */

  xfree (dir);
}

/* Subroutine used by android_fstatat and android_faccessat.  If DIRFD
   belongs to an open asset directory and FILE is a relative file
   name, then return AT_FDCWD and the absolute file name of the
   directory prepended to FILE in *PATHNAME.  Else, return DIRFD.  */

int
android_lookup_asset_directory_fd (int dirfd,
				   const char *restrict *pathname,
				   const char *restrict file)
{
  struct android_dir *dir;
  static char *name;

  if (file[0] == '/')
    return dirfd;

  for (dir = android_dirs; dir; dir = dir->next)
    {
      if (dir->fd == dirfd && dirfd != -1)
	{
	  if (name)
	    xfree (name);

	  /* dir->asset_file is always separator terminated.  */
	  name = xzalloc (strlen (dir->asset_file)
			  + strlen (file) + 1);
	  strcpy (name, dir->asset_file);
	  strcpy (name + strlen (dir->asset_file),
		  file);
	  *pathname = name;
	  return AT_FDCWD;
	}
    }

  return dirfd;
}



/* emacs_abort implementation for Android.  This logs a stack
   trace.  */

void
emacs_abort (void)
{
  volatile char *foo;

  __android_log_print (ANDROID_LOG_FATAL, __func__,
		       "emacs_abort called, please review the ensuing"
		       " stack trace");

  /* Cause a NULL pointer dereference to make debuggerd generate a
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

/* Given a Lisp string TEXT, return a local reference to an equivalent
   Java string.  */

jstring
android_build_string (Lisp_Object text)
{
  Lisp_Object encoded;
  jstring string;
  size_t nchars;
  jchar *characters;
  USE_SAFE_ALLOCA;

  /* Directly encode TEXT if it contains no multibyte
     characters.  This is okay because the Java extended UTF
     format is compatible with ASCII.  */

  if (SBYTES (text) == SCHARS (text)
      && android_check_string (text))
    {
      string = (*android_java_env)->NewStringUTF (android_java_env,
						  SSDATA (text));
      android_exception_check ();
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
  android_exception_check ();

  SAFE_FREE ();
  return string;
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
   references that are already valid but are not used after leaving
   the current scope.  For example, to allocate foo and then make
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

/* Check for JNI exceptions and call memory_full in that
   situation.  */

void
android_exception_check (void)
{
  if ((*android_java_env)->ExceptionCheck (android_java_env))
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "Possible out of memory error. "
			   " The Java exception follows:  ");
      /* Describe exactly what went wrong.  */
      (*android_java_env)->ExceptionDescribe (android_java_env);
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }
}

/* Check for JNI exceptions.  If there is one such exception, clear
   it, then delete the local reference to OBJECT and call
   memory_full.  */

void
android_exception_check_1 (jobject object)
{
  if ((*android_java_env)->ExceptionCheck (android_java_env))
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "Possible out of memory error. "
			   " The Java exception follows:  ");
      /* Describe exactly what went wrong.  */
      (*android_java_env)->ExceptionDescribe (android_java_env);
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (object);
      memory_full (0);
    }
}

/* Like android_exception_check_one, except it takes more than one
   local reference argument.  */

void
android_exception_check_2 (jobject object, jobject object1)
{
  if ((*android_java_env)->ExceptionCheck (android_java_env))
    {
      __android_log_print (ANDROID_LOG_WARN, __func__,
			   "Possible out of memory error. "
			   " The Java exception follows:  ");
      /* Describe exactly what went wrong.  */
      (*android_java_env)->ExceptionDescribe (android_java_env);
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (object);
      ANDROID_DELETE_LOCAL_REF (object1);
      memory_full (0);
    }
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
  if (object)
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
  if (object)
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

/* Ask the system to start browsing the specified encoded URL.  Upon
   failure, return a string describing the error.  Else, value is
   nil.  */

Lisp_Object
android_browse_url (Lisp_Object url)
{
  jobject value, string;
  Lisp_Object tem;
  const char *buffer;

  string = android_build_string (url);
  value = (*android_java_env)->CallObjectMethod (android_java_env,
						 emacs_service,
						 service_class.browse_url,
						 string);
  android_exception_check ();

  ANDROID_DELETE_LOCAL_REF (string);

  /* If no string was returned, return Qnil.  */
  if (!value)
    return Qnil;

  buffer = (*android_java_env)->GetStringUTFChars (android_java_env,
						   (jstring) value,
						   NULL);
  android_exception_check_1 (string);

  /* Otherwise, build the string describing the error.  */
  tem = build_string_from_utf8 (buffer);

  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) value,
					      buffer);

  /* And return it.  */
  ANDROID_DELETE_LOCAL_REF (value);
  return tem;
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

/* Return a number from 1 to 33 describing the version of Android
   Emacs is running on.

   This is different from __ANDROID_API__, as that describes the
   minimum version of Android this build of Emacs will run on, and in
   turn which APIs Emacs can safely use.  */

int
android_get_current_api_level (void)
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

  array = (*android_java_env)->CallObjectMethod (android_java_env,
						 emacs_service,
						 service_class.query_battery);
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
  status->remaining = longs[4];
  status->status = longs[5];
  status->plugged = longs[6];
  status->temperature = longs[7];

  (*android_java_env)->ReleaseLongArrayElements (android_java_env,
						 array, longs,
						 JNI_ABORT);
  ANDROID_DELETE_LOCAL_REF (array);

  return 0;
}

/* Display a small momentary notification on screen containing
   TEXT, which must be in the modified UTF encoding used by the
   JVM.  */

void
android_display_toast (const char *text)
{
  jstring string;

  /* Make the string.  */
  string = (*android_java_env)->NewStringUTF (android_java_env,
					      text);
  android_exception_check ();

  /* Display the toast.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 service_class.display_toast,
						 string);
  android_exception_check_1 (string);

  /* Delete the local reference to the string.  */
  ANDROID_DELETE_LOCAL_REF (string);
}



/* Whether or not a query is currently being made.  */
static bool android_servicing_query;

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
   counter is incremented every time a query starts, and is set to
   zerp every time one ends.  If the UI thread tries to make a query
   and sees that the counter is non-zero, it simply returns so that
   its event loop can proceed to perform and respond to the query.  If
   the Emacs thread sees the same thing, then it stops to service all
   queries being made by the input method, then proceeds to make its
   query.  */

/* Run any function that the UI thread has asked to run, and then
   signal its completion.  */

void
android_check_query (void)
{
  void (*proc) (void *);
  void *closure;

  if (!__atomic_load_n (&android_servicing_query, __ATOMIC_SEQ_CST))
    return;

  /* First, load the procedure and closure.  */
  __atomic_load (&android_query_context, &closure, __ATOMIC_SEQ_CST);
  __atomic_load (&android_query_function, &proc, __ATOMIC_SEQ_CST);

  if (!proc)
    return;

  proc (closure);

  /* Finish the query.  */
  __atomic_store_n (&android_query_context, NULL, __ATOMIC_SEQ_CST);
  __atomic_store_n (&android_query_function, NULL, __ATOMIC_SEQ_CST);
  __atomic_clear (&android_servicing_query, __ATOMIC_SEQ_CST);

  /* Signal completion.  */
  sem_post (&android_query_sem);
}

/* Notice that the Emacs thread will start blocking waiting for a
   response from the UI thread.  Process any pending queries from the
   UI thread.

   This function may be called from Java.  */

static void
android_begin_query (void)
{
  if (__atomic_test_and_set (&android_servicing_query,
			     __ATOMIC_SEQ_CST))
    {
      /* Answer the query that is currently being made.  */
      assert (android_query_function != NULL);
      android_check_query ();

      /* Wait for that query to complete.  */
      while (__atomic_load_n (&android_servicing_query,
			      __ATOMIC_SEQ_CST))
	;;
    }
}

/* Notice that a query has stopped.  This function may be called from
   Java.  */

static void
android_end_query (void)
{
  __atomic_clear (&android_servicing_query, __ATOMIC_SEQ_CST);
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

  event.xaction.type = ANDROID_WINDOW_ACTION;
  event.xaction.serial = ++event_serial;
  event.xaction.window = 0;
  event.xaction.action = 0;

  /* Set android_query_function and android_query_context.  */
  __atomic_store_n (&android_query_context, closure, __ATOMIC_SEQ_CST);
  __atomic_store_n (&android_query_function, proc, __ATOMIC_SEQ_CST);

  /* Don't allow deadlocks to happen; make sure the Emacs thread is
     not waiting for something to be done.  */

  if (__atomic_test_and_set (&android_servicing_query,
			     __ATOMIC_SEQ_CST))
    {
      __atomic_store_n (&android_query_context, NULL,
			__ATOMIC_SEQ_CST);
      __atomic_store_n (&android_query_function, NULL,
			__ATOMIC_SEQ_CST);

      return 1;
    }

  /* Send a dummy event.  `android_check_query' will be called inside
     wait_reading_process_output after the event arrives.

     Otherwise, android_select will call android_check_thread the next
     time it is entered.  */
  android_write_event (&event);

  /* Start waiting for the function to be executed.  */
  while (sem_wait (&android_query_sem) < 0)
    ;;

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

  object = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);

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

  object = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);

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

  object = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);
  method = service_class.update_extracted_text;

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 emacs_service,
						 service_class.class,
						 method, object,
						 /* N.B. that
						   text is not
						   jobject,
						   because that
						   type is not
						   available in
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

  object = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);
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

  object = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);

  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 object,
						 window_class.class,
						 window_class.set_fullscreen,
						 (jboolean) fullscreen);
  android_exception_check ();
  return 0;
}



/* External asset management interface.  By using functions here
   to read and write from files, Emacs can avoid opening a
   shared memory file descriptor for each ``asset'' file.  */

/* Like android_open.  However, return a structure that can
   either directly hold an AAsset or a file descriptor.

   Value is the structure upon success.  Upon failure, value
   consists of an uninitialized file descriptor, but its asset
   field is set to -1, and errno is set accordingly.  */

struct android_fd_or_asset
android_open_asset (const char *filename, int oflag, mode_t mode)
{
  const char *name;
  struct android_fd_or_asset fd;
  AAsset *asset;

  /* Initialize FD by setting its asset to an invalid
     pointer.  */
  fd.asset = (void *) -1;

  /* See if this is an asset.  */

  if (asset_manager && (name = android_get_asset_name (filename)))
    {
      /* Return failure for unsupported flags.  */

      if (oflag & O_WRONLY || oflag & O_RDWR)
	{
	  errno = EROFS;
	  return fd;
	}

      if (oflag & O_DIRECTORY)
	{
	  errno = ENOTSUP;
	  return fd;
	}

      /* Now try to open the asset.  */
      asset = AAssetManager_open (asset_manager, name,
				  AASSET_MODE_STREAMING);

      if (!asset)
	{
	  errno = ENOENT;
	  return fd;
	}

      /* Return the asset.  */
      fd.asset = asset;
      return fd;
    }

  /* If the file is not an asset, fall back to android_open and
     get a regular file descriptor.  */

  fd.fd = android_open (filename, oflag, mode);
  if (fd.fd < 0)
    return fd;

  /* Set fd.asset to NULL, signifying that it is a file
     descriptor.  */
  fd.asset = NULL;
  return fd;
}

/* Like android_close.  However, it takes a ``file descriptor''
   opened using android_open_asset.  */

int
android_close_asset (struct android_fd_or_asset asset)
{
  if (!asset.asset)
    return android_close (asset.fd);

  AAsset_close (asset.asset);
  return 0;
}

/* Like `emacs_read_quit'.  However, it handles file descriptors
   opened using `android_open_asset' as well.  */

ssize_t
android_asset_read_quit (struct android_fd_or_asset asset,
			 void *buffer, size_t size)
{
  if (!asset.asset)
    return emacs_read_quit (asset.fd, buffer, size);

  /* It doesn't seem possible to quit from inside AAsset_read,
     sadly.  */
  return AAsset_read (asset.asset, buffer, size);
}

/* Like `read'.  However, it handles file descriptors opened
   using `android_open_asset' as well.  */

ssize_t
android_asset_read (struct android_fd_or_asset asset,
		    void *buffer, size_t size)
{
  if (!asset.asset)
    return read (asset.fd, buffer, size);

  /* It doesn't seem possible to quit from inside AAsset_read,
     sadly.  */
  return AAsset_read (asset.asset, buffer, size);
}

/* Like `lseek', but it handles ``file descriptors'' opened with
   android_open_asset.  */

off_t
android_asset_lseek (struct android_fd_or_asset asset, off_t off,
		     int whence)
{
  if (!asset.asset)
    return lseek (asset.fd, off, whence);

  return AAsset_seek (asset.asset, off, whence);
}

/* Like `fstat'.  */

int
android_asset_fstat (struct android_fd_or_asset asset,
		     struct stat *statb)
{
  if (!asset.asset)
    return fstat (asset.fd, statb);

  /* Clear statb.  */
  memset (statb, 0, sizeof *statb);

  /* Set the mode.  */
  statb->st_mode = S_IFREG | S_IRUSR | S_IRGRP | S_IROTH;

  /* Owned by root.  */
  statb->st_uid = 0;
  statb->st_gid = 0;

  /* Size of the file.  */
  statb->st_size = AAsset_getLength (asset.asset);
  return 0;
}



/* Window cursor support.  */

android_cursor
android_create_font_cursor (enum android_cursor_shape shape)
{
  android_cursor id;
  short prev_max_handle;
  jobject object;

  /* First, allocate the cursor handle.  */
  prev_max_handle = max_handle;
  id = android_alloc_id ();

  if (!id)
    error ("Out of cursor handles!");

  /* Next, create the cursor.  */
  object = (*android_java_env)->NewObject (android_java_env,
					   cursor_class.class,
					   cursor_class.constructor,
					   (jshort) id,
					   (jint) shape);
  if (!object)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      max_handle = prev_max_handle;
      memory_full (0);
    }

  android_handles[id].type = ANDROID_HANDLE_CURSOR;
  android_handles[id].handle
    = (*android_java_env)->NewGlobalRef (android_java_env, object);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (object);

  if (!android_handles[id].handle)
    memory_full (0);

  return id;
}

void
android_define_cursor (android_window window, android_cursor cursor)
{
  jobject window1, cursor1;
  jmethodID method;

  window1 = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);
  cursor1 = android_resolve_handle (cursor, ANDROID_HANDLE_CURSOR);
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
  if (android_handles[cursor].type != ANDROID_HANDLE_CURSOR)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Trying to destroy something not a CURSOR!");
      emacs_abort ();
    }

  android_destroy_handle (cursor);
}



/* Process execution.

   Newer Android systems use SELinux to restrict user programs from
   executing programs installed in the application data directory for
   security reasons.  Emacs uses a `loader' binary installed in the
   application data directory to manually load executables and replace
   the `execve' system call.  */

enum
  {
    /* Maximum number of arguments available.  */
    MAXARGS = 1024,
  };

/* Rewrite the command line given in *ARGV to utilize the `exec1'
   bootstrap binary if necessary.

   Value is 0 upon success, else 1.  Set errno upon failure.

   ARGV holds a pointer to a NULL-terminated array of arguments given
   to `emacs_spawn'.  */

int
android_rewrite_spawn_argv (const char ***argv)
{
  static const char *new_args[MAXARGS];
  static char exec1_name[PATH_MAX], loader_name[PATH_MAX];
  size_t i, nargs;

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

  /* nargs now holds the number of arguments in argv.  If it's larger
     than MAXARGS, return failure.  */

  if (nargs + 2 > MAXARGS)
    {
      errno = E2BIG;
      return 1;
    }

  /* Fill in the name of `libexec1.so'.  */
  snprintf (exec1_name, PATH_MAX, "%s/libexec1.so",
	    android_lib_dir);

  /* And libloader.so.  */
  snprintf (loader_name, PATH_MAX, "%s/libloader.so",
	    android_lib_dir);

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

#endif
