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

#include <sys/stat.h>
#include <sys/mman.h>

#include <assert.h>

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

#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>
#include <android/bitmap.h>
#include <android/log.h>

#include <linux/ashmem.h>

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
  jmethodID copy_area;
  jmethodID clear_window;
  jmethodID clear_area;
  jmethodID ring_bell;
  jmethodID query_tree;
  jmethodID get_screen_width;
  jmethodID get_screen_height;
  jmethodID detect_mouse;
  jmethodID name_keysym;
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

/* The asset manager being used.  */
static AAssetManager *asset_manager;

/* Whether or not Emacs has been initialized.  */
static int emacs_initialized;

/* The path used to store site-lisp.  */
char *android_site_load_path;

/* The path used to store native libraries.  */
char *android_lib_dir;

/* The path used to store game files.  */
char *android_game_path;

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
static const sigset_t *volatile android_pselect_sigset;

/* Value of pselect.  */
static int android_pselect_rc;

/* Whether or not pselect finished.  */
static volatile bool android_pselect_completed;

/* The global event queue.  */
static struct android_event_queue event_queue;

/* Semaphores used to signal select completion and start.  */
static sem_t android_pselect_sem, android_pselect_start_sem;

static void *
android_run_select_thread (void *data)
{
  sigset_t signals;
  int rc;

  sigfillset (&signals);

  if (pthread_sigmask (SIG_BLOCK, &signals, NULL))
    __android_log_print (ANDROID_LOG_FATAL, __func__,
			 "pthread_sigmask: %s",
			 strerror (errno));

  while (true)
    {
      /* Wait for the thread to be released.  */
      sem_wait (&android_pselect_start_sem);

      /* Get the select lock and call pselect.  */
      pthread_mutex_lock (&event_queue.select_mutex);
      rc = pselect (android_pselect_nfds,
		    android_pselect_readfds,
		    android_pselect_writefds,
		    android_pselect_exceptfds,
		    android_pselect_timeout,
		    android_pselect_sigset);
      android_pselect_rc = rc;
      pthread_mutex_unlock (&event_queue.select_mutex);

      /* Signal the Emacs thread that pselect is done.  If read_var
	 was signaled by android_write_event, event_queue.mutex could
	 still be locked, so this must come before.  */
      sem_post (&android_pselect_sem);

      pthread_mutex_lock (&event_queue.mutex);
      android_pselect_completed = true;
      pthread_cond_signal (&event_queue.read_var);
      pthread_mutex_unlock (&event_queue.mutex);
    }
}

static void
android_handle_sigusr1 (int sig, siginfo_t *siginfo, void *arg)
{
  /* Nothing to do here, this signal handler is only installed to make
     sure the disposition of SIGUSR1 is enough.  */
}

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

  event_queue.events.next = &event_queue.events;
  event_queue.events.last = &event_queue.events;

  /* Before starting the select thread, make sure the disposition for
     SIGUSR1 is correct.  */
  sigfillset (&sa.sa_mask);
  sa.sa_sigaction = android_handle_sigusr1;
  sa.sa_flags = SA_SIGINFO;

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

static void
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
  pthread_cond_signal (&event_queue.read_var);
  pthread_mutex_unlock (&event_queue.mutex);

  /* Now set pending_signals to true.  This allows C-g to be handled
     immediately even without SIGIO.  */
  pending_signals = true;
}

int
android_select (int nfds, fd_set *readfds, fd_set *writefds,
		fd_set *exceptfds, struct timespec *timeout,
		const sigset_t *sigset)
{
  int nfds_return;

  pthread_mutex_lock (&event_queue.mutex);

  if (event_queue.num_events)
    {
      pthread_mutex_unlock (&event_queue.mutex);
      return 1;
    }

  nfds_return = 0;
  android_pselect_completed = false;

  pthread_mutex_lock (&event_queue.select_mutex);
  android_pselect_nfds = nfds;
  android_pselect_readfds = readfds;
  android_pselect_writefds = writefds;
  android_pselect_exceptfds = exceptfds;
  android_pselect_timeout = timeout;
  android_pselect_sigset = sigset;
  pthread_mutex_unlock (&event_queue.select_mutex);

  /* Release the select thread.  */
  sem_post (&android_pselect_start_sem);

  pthread_cond_wait (&event_queue.read_var, &event_queue.mutex);

  /* Interrupt the select thread now, in case it's still in
     pselect.  */
  pthread_kill (event_queue.select_thread, SIGUSR1);

  /* Wait for pselect to return in any case.  */
  sem_wait (&android_pselect_sem);

  /* If there are now events in the queue, return 1.  */
  if (event_queue.num_events)
    nfds_return = 1;

  /* Add the return value of pselect.  */
  if (android_pselect_rc >= 0)
    nfds_return += android_pselect_rc;

  if (!nfds_return && android_pselect_rc < 0)
    nfds_return = android_pselect_rc;

  /* Unlock the event queue mutex.  */
  pthread_mutex_unlock (&event_queue.mutex);

  /* This is to shut up process.c when pselect gets EINTR.  */
  if (nfds_return < 0)
    errno = EINTR;

  return nfds_return;
}



static void *
android_run_debug_thread (void *data)
{
  FILE *file;
  int fd;
  char *line;
  size_t n;

  fd = (int) data;
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
  if (!pw->pw_gecos)
    return (char *) "Android user";

  return pw->pw_gecos;
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

/* Like fstatat.  However, if dirfd is AT_FDCWD and PATHNAME is an
   asset, find the information for the corresponding asset.  */

int
android_fstatat (int dirfd, const char *restrict pathname,
		 struct stat *restrict statbuf, int flags)
{
  AAsset *asset_desc;
  const char *asset;

  if (dirfd == AT_FDCWD
      && asset_manager
      && (asset = android_get_asset_name (pathname)))
    {
      /* AASSET_MODE_STREAMING is fastest here.  */
      asset_desc = AAssetManager_open (asset_manager, asset,
				       AASSET_MODE_STREAMING);

      if (!asset_desc)
	return ENOENT;

      memset (statbuf, 0, sizeof *statbuf);

      /* Fill in the stat buffer.  */
      statbuf->st_mode = S_IFREG;
      statbuf->st_size = AAsset_getLength (asset_desc);

      /* Close the asset.  */
      AAsset_close (asset_desc);
      return 0;
    }

  return fstatat (dirfd, pathname, statbuf, flags);
}

/* Return if NAME is a file that is actually an asset and is
   accessible, as long as !(amode & W_OK).  */

bool
android_file_access_p (const char *name, int amode)
{
  AAsset *asset;
  AAssetDir *directory;
  int length;

  if (!asset_manager)
    return false;

  if (!(amode & W_OK) && (name = android_get_asset_name (name)))
    {
      if (!strcmp (name, "") || !strcmp (name, "/"))
	/* /assets always exists.  */
	return true;

      /* Check if the asset exists by opening it.  Suboptimal! */
      asset = AAssetManager_open (asset_manager, name,
				  AASSET_MODE_UNKNOWN);

      if (!asset)
	{
	  /* See if it's a directory as well.  To open a directory
	     with the asset manager, the trailing slash (if specified)
	     must be removed.  */
	  directory = AAssetManager_openDir (asset_manager, name);

	  if (directory)
	    {
	      /* Make sure the directory actually has files in it.  */

	      if (!AAssetDir_getNextFileName (directory))
		{
		  AAssetDir_close (directory);
		  errno = ENOENT;
		  return false;
		}

	      AAssetDir_close (directory);
	      return true;
	    }

	  errno = ENOENT;
	  return false;
	}

      AAsset_close (asset);

      /* If NAME is a directory name, but it was a regular file, set
	 errno to ENOTDIR and return false.  This is to behave like
	 faccessat.  */

      length = strlen (name);
      if (name[length - 1] == '/')
	{
	  errno = ENOTDIR;
	  return false;
	}

      return true;
    }

  return false;
}

/* Get a file descriptor backed by a temporary in-memory file for the
   given asset.  */

static int
android_hack_asset_fd (AAsset *asset)
{
  int fd, rc;
  unsigned char *mem;
  size_t size;

  fd = open ("/dev/ashmem", O_RDWR);

  if (fd < 0)
    return -1;

  /* Assets must be small enough to fit in size_t, if off_t is
     larger.  */
  size = AAsset_getLength (asset);

  /* An empty name means the memory area will exist until the file
     descriptor is closed, because no other process can attach.  */
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
      /* Too little was read.  Close the file descriptor and report an
	 error.  */
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "AAsset_read: %s", strerror (errno));
      close (fd);
      return -1;
    }

  /* Return anyway even if munmap fails.  */
  munmap (mem, size);
  return fd;
}

/* `open' and such are modified even though they exist on Android,
   because Emacs treats "/assets/" as a special directory that must
   contain all assets in the application package.  */

int
android_open (const char *filename, int oflag, int mode)
{
  const char *name;
  AAsset *asset;
  int fd;
  off_t out_start, out_length;
  bool fd_hacked;

  /* This flag means whether or not fd should not be duplicated.  */
  fd_hacked = false;

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
	  errno = EINVAL;
	  return -1;
	}

      asset = AAssetManager_open (asset_manager, name,
				  AASSET_MODE_BUFFER);

      if (!asset)
	{
	  errno = ENOENT;
	  return -1;
	}

      /* Try to obtain the file descriptor corresponding to this
	 asset.  */
      fd = AAsset_openFileDescriptor (asset, &out_start,
				      &out_length);

      if (fd == -1)
	{
	  /* The asset can't be accessed for some reason.  Try to
	     create a shared memory file descriptor.  */
	  fd = android_hack_asset_fd (asset);

	  if (fd == -1)
	    {
	      AAsset_close (asset);
	      errno = ENXIO;
	      return -1;
	    }

	  fd_hacked = true;
	}

      /* Duplicate the file descriptor and then close the asset,
	 which will close the original file descriptor.  */

      if (!fd_hacked)
	fd = fcntl (fd, F_DUPFD_CLOEXEC);

      if (fd >= ANDROID_MAX_ASSET_FD || fd < 0)
	{
	  /* Too bad.  You lose.  */
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
	  android_table[fd].statb.st_mode = S_IFREG;

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

  return open (filename, oflag, mode);
}

/* Like close.  However, remove the file descriptor from the asset
   table as well.  */

int
android_close (int fd)
{
  if (fd < ANDROID_MAX_ASSET_FD
      && (android_table[fd].flags
	  & ANDROID_FD_TABLE_ENTRY_IS_VALID))
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

  if (fd != -1 && fd < ANDROID_MAX_ASSET_FD
      && (android_table[fd].flags
	  & ANDROID_FD_TABLE_ENTRY_IS_VALID))
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



/* JNI functions called by Java.  */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"

JNIEXPORT void JNICALL
NATIVE_NAME (setEmacsParams) (JNIEnv *env, jobject object,
			      jobject local_asset_manager,
			      jobject files_dir, jobject libs_dir,
			      jfloat pixel_density_x,
			      jfloat pixel_density_y,
			      jobject emacs_service_object)
{
  int pipefd[2];
  pthread_t thread;
  const char *java_string;

  /* This may be called from multiple threads.  setEmacsParams should
     only ever be called once.  */
  if (__atomic_fetch_add (&emacs_initialized, -1, __ATOMIC_RELAXED))
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

  /* Hold a VM reference to the asset manager to prevent the native
     object from being deleted.  */
  (*env)->NewGlobalRef (env, local_asset_manager);

  /* Create a pipe and duplicate it to stdout and stderr.  Next, make
     a thread that prints stderr to the system log.  */

  if (pipe2 (pipefd, O_CLOEXEC) < 0)
    emacs_abort ();

  if (dup2 (pipefd[1], 2) < 0)
    emacs_abort ();
  close (pipefd[1]);

  if (pthread_create (&thread, NULL, android_run_debug_thread,
		      (void *) pipefd[0]))
    emacs_abort ();

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
		       "Game score path: %s\n",
		       android_site_load_path,
		       android_files_dir,
		       android_lib_dir, android_game_path);

  /* Make a reference to the Emacs service.  */
  emacs_service = (*env)->NewGlobalRef (env, emacs_service_object);

  if (!emacs_service)
    emacs_abort ();

  /* Set up events.  */
  android_init_events ();

  /* OK, setup is now complete.  The caller may start the Emacs thread
     now.  */
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
  FIND_METHOD (copy_area, "copyArea",
	       "(Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsDrawable;"
	       "Lorg/gnu/emacs/EmacsGC;IIIIII)V");
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

extern JNIEXPORT void JNICALL
NATIVE_NAME (initEmacs) (JNIEnv *env, jobject object, jarray argv)
{
  char **c_argv;
  jsize nelements, i;
  jobject argument;
  const char *c_argument;

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

  /* Set HOME to the app data directory.  */
  setenv ("HOME", android_files_dir, 1);

  /* Set the cwd to that directory as well.  */
  if (chdir (android_files_dir))
    __android_log_print (ANDROID_LOG_WARN, __func__,
			 "chdir: %s", strerror (errno));

  /* Initialize the Android GUI.  */
  android_init_gui = true;
  android_emacs_init (nelements, c_argv);
  /* android_emacs_init should never return.  */
  emacs_abort ();
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (emacsAbort) (JNIEnv *env, jobject object)
{
  emacs_abort ();
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendConfigureNotify) (JNIEnv *env, jobject object,
				   jshort window, jlong time,
				   jint x, jint y, jint width,
				   jint height)
{
  union android_event event;

  event.xconfigure.type = ANDROID_CONFIGURE_NOTIFY;
  event.xconfigure.window = window;
  event.xconfigure.time = time;
  event.xconfigure.x = x;
  event.xconfigure.y = y;
  event.xconfigure.width = width;
  event.xconfigure.height = height;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendKeyPress) (JNIEnv *env, jobject object,
			    jshort window, jlong time,
			    jint state, jint keycode,
			    jint unicode_char)
{
  union android_event event;

  event.xkey.type = ANDROID_KEY_PRESS;
  event.xkey.window = window;
  event.xkey.time = time;
  event.xkey.state = state;
  event.xkey.keycode = keycode;
  event.xkey.unicode_char = unicode_char;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendKeyRelease) (JNIEnv *env, jobject object,
			      jshort window, jlong time,
			      jint state, jint keycode,
			      jint unicode_char)
{
  union android_event event;

  event.xkey.type = ANDROID_KEY_RELEASE;
  event.xkey.window = window;
  event.xkey.time = time;
  event.xkey.state = state;
  event.xkey.keycode = keycode;
  event.xkey.unicode_char = unicode_char;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendFocusIn) (JNIEnv *env, jobject object,
			   jshort window, jlong time)
{
  union android_event event;

  event.xkey.type = ANDROID_FOCUS_IN;
  event.xkey.window = window;
  event.xkey.time = time;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendFocusOut) (JNIEnv *env, jobject object,
			    jshort window, jlong time)
{
  union android_event event;

  event.xkey.type = ANDROID_FOCUS_OUT;
  event.xkey.window = window;
  event.xkey.time = time;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendWindowAction) (JNIEnv *env, jobject object,
				jshort window, jint action)
{
  union android_event event;

  event.xaction.type = ANDROID_WINDOW_ACTION;
  event.xaction.window = window;
  event.xaction.action = action;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendEnterNotify) (JNIEnv *env, jobject object,
			       jshort window, jint x, jint y,
			       jlong time)
{
  union android_event event;

  event.xcrossing.type = ANDROID_ENTER_NOTIFY;
  event.xcrossing.window = window;
  event.xcrossing.x = x;
  event.xcrossing.y = y;
  event.xcrossing.time = time;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendLeaveNotify) (JNIEnv *env, jobject object,
			       jshort window, jint x, jint y,
			       jlong time)
{
  union android_event event;

  event.xcrossing.type = ANDROID_LEAVE_NOTIFY;
  event.xcrossing.window = window;
  event.xcrossing.x = x;
  event.xcrossing.y = y;
  event.xcrossing.time = time;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendMotionNotify) (JNIEnv *env, jobject object,
				jshort window, jint x, jint y,
				jlong time)
{
  union android_event event;

  event.xmotion.type = ANDROID_MOTION_NOTIFY;
  event.xmotion.window = window;
  event.xmotion.x = x;
  event.xmotion.y = y;
  event.xmotion.time = time;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendButtonPress) (JNIEnv *env, jobject object,
			       jshort window, jint x, jint y,
			       jlong time, jint state,
			       jint button)
{
  union android_event event;

  event.xbutton.type = ANDROID_BUTTON_PRESS;
  event.xbutton.window = window;
  event.xbutton.x = x;
  event.xbutton.y = y;
  event.xbutton.time = time;
  event.xbutton.state = state;
  event.xbutton.button = button;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendButtonRelease) (JNIEnv *env, jobject object,
				 jshort window, jint x, jint y,
				 jlong time, jint state,
				 jint button)
{
  union android_event event;

  event.xbutton.type = ANDROID_BUTTON_RELEASE;
  event.xbutton.window = window;
  event.xbutton.x = x;
  event.xbutton.y = y;
  event.xbutton.time = time;
  event.xbutton.state = state;
  event.xbutton.button = button;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendTouchDown) (JNIEnv *env, jobject object,
			     jshort window, jint x, jint y,
			     jlong time, jint pointer_id)
{
  union android_event event;

  event.touch.type = ANDROID_TOUCH_DOWN;
  event.touch.window = window;
  event.touch.x = x;
  event.touch.y = y;
  event.touch.time = time;
  event.touch.pointer_id = pointer_id;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendTouchUp) (JNIEnv *env, jobject object,
			   jshort window, jint x, jint y,
			   jlong time, jint pointer_id)
{
  union android_event event;

  event.touch.type = ANDROID_TOUCH_UP;
  event.touch.window = window;
  event.touch.x = x;
  event.touch.y = y;
  event.touch.time = time;
  event.touch.pointer_id = pointer_id;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendTouchMove) (JNIEnv *env, jobject object,
			     jshort window, jint x, jint y,
			     jlong time, jint pointer_id)
{
  union android_event event;

  event.touch.type = ANDROID_TOUCH_MOVE;
  event.touch.window = window;
  event.touch.x = x;
  event.touch.y = y;
  event.touch.time = time;
  event.touch.pointer_id = pointer_id;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendWheel) (JNIEnv *env, jobject object,
			 jshort window, jint x, jint y,
			 jlong time, jint state,
			 jfloat x_delta, jfloat y_delta)
{
  union android_event event;

  event.wheel.type = ANDROID_WHEEL;
  event.wheel.window = window;
  event.wheel.x = x;
  event.wheel.y = y;
  event.wheel.time = time;
  event.wheel.state = state;
  event.wheel.x_delta = x_delta;
  event.wheel.y_delta = y_delta;

  android_write_event (&event);
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendIconified) (JNIEnv *env, jobject object,
			     jshort window)
{
  union android_event event;

  event.iconified.type = ANDROID_ICONIFIED;
  event.iconified.window = window;
}

extern JNIEXPORT void JNICALL
NATIVE_NAME (sendDeiconified) (JNIEnv *env, jobject object,
			       jshort window)
{
  union android_event event;

  event.iconified.type = ANDROID_DEICONIFIED;
  event.iconified.window = window;
}

#pragma clang diagnostic pop



/* Java functions called by C.

   Because all C code runs in the native function initEmacs, ALL LOCAL
   REFERENCES WILL PERSIST!

   This means that every local reference must be explicitly destroyed
   with DeleteLocalRef.  A helper macro is provided to do this.  */

#define MAX_HANDLE 65535

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

  if (android_handles[max_handle].handle)
    {
      handle = max_handle + 1;

      while (max_handle < handle)
	{
	  ++max_handle;

	  if (!max_handle)
	    ++max_handle;

	  if (!android_handles[max_handle].handle)
	    return 0;
	}

      return 0;
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
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (old);

      if (!class)
	memory_full (0);
    }

  (*android_java_env)->CallVoidMethod (android_java_env,
				       android_handles[handle].handle,
				       method);
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

  return android_handles[handle].handle;
}

static jobject
android_resolve_handle2 (android_handle handle,
			 enum android_handle_type type,
			 enum android_handle_type type2)
{
  if (!handle)
    return NULL;

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

  return android_handles[handle].handle;
}

static jmethodID android_lookup_method (const char *, const char *,
					const char *);

void
android_change_window_attributes (android_window handle,
				  enum android_window_value_mask value_mask,
				  struct android_set_window_attributes *attrs)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

  if (value_mask & ANDROID_CW_BACK_PIXEL)
    {
      method = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				      "changeWindowBackground", "(I)V");
      (*android_java_env)->CallVoidMethod (android_java_env,
					   window, method,
					   (jint) attrs->background_pixel);
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
      (*android_java_env)->ExceptionClear (android_java_env);
      ANDROID_DELETE_LOCAL_REF (old);

      if (!class)
	memory_full (0);
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
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!android_rect_class)
    memory_full (0);
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
					"markDirty", "()V");
  assert (emacs_gc_mark_dirty);

  old = emacs_gc_class;
  emacs_gc_class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) emacs_gc_class);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (old);
  if (!emacs_gc_class)
    memory_full (0);

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
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_function,
				      values->function);

  if (mask & ANDROID_GC_CLIP_X_ORIGIN)
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_clip_x_origin,
				      values->clip_x_origin);

  if (mask & ANDROID_GC_CLIP_Y_ORIGIN)
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_clip_y_origin,
				      values->clip_y_origin);

  if (mask & ANDROID_GC_CLIP_MASK)
    {
      what = android_resolve_handle (values->clip_mask,
				     ANDROID_HANDLE_PIXMAP);
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_clip_mask,
					   what);

      /* Changing GCClipMask also clears the clip rectangles.  */
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_clip_rects,
					   NULL);

      xfree (gc->clip_rects);
      gc->clip_rects = NULL;
      gc->num_clip_rects = -1;
    }

  if (mask & ANDROID_GC_STIPPLE)
    {
      what = android_resolve_handle (values->stipple,
				     ANDROID_HANDLE_PIXMAP);
      (*android_java_env)->SetObjectField (android_java_env,
					   gcontext,
					   emacs_gc_stipple,
					   what);
    }

  if (mask & ANDROID_GC_FILL_STYLE)
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_fill_style,
				      values->fill_style);

  if (mask & ANDROID_GC_TILE_STIP_X_ORIGIN)
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_ts_origin_x,
				      values->ts_x_origin);

  if (mask & ANDROID_GC_TILE_STIP_Y_ORIGIN)
    (*android_java_env)->SetIntField (android_java_env,
				      gcontext,
				      emacs_gc_ts_origin_y,
				      values->ts_y_origin);

  if (mask)
    (*android_java_env)->CallVoidMethod (android_java_env,
					 gcontext,
					 emacs_gc_mark_dirty);
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

  if (!array)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

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

      if (!rect)
	{
	  (*android_java_env)->ExceptionClear (android_java_env);
	  ANDROID_DELETE_LOCAL_REF (array);
	  memory_full (0);
	}

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

  (*android_java_env)->CallVoidMethod (android_java_env,
				       gcontext,
				       emacs_gc_mark_dirty);

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

  method = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				  "reparentTo",
				  "(Lorg/gnu/emacs/EmacsWindow;II)V");
  (*android_java_env)->CallVoidMethod (android_java_env, window,
				       method,
				       parent, (jint) x, (jint) y);
}

/* Look up the method with SIGNATURE by NAME in CLASS.  Abort if it
   could not be found.  This should be used for functions which are
   not called very often.

   CLASS must never be unloaded, or the behavior is undefined.  */

static jmethodID
android_lookup_method (const char *class, const char *name,
		       const char *signature)
{
  jclass java_class;
  jmethodID method;

  java_class
    = (*android_java_env)->FindClass (android_java_env, class);

  if (!java_class)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Failed to find class %s", class);
      emacs_abort ();
    }

  method
    = (*android_java_env)->GetMethodID (android_java_env,
					java_class, name,
					signature);

  if (!method)
    {
      __android_log_print (ANDROID_LOG_ERROR, __func__,
			   "Failed to find method %s in class %s"
			   " with signature %s",
			   name, class, signature);
      emacs_abort ();
    }

  ANDROID_DELETE_LOCAL_REF (java_class);
  return method;
}

void
android_clear_window (android_window handle)
{
  jobject window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
				       service_class.clear_window,
				       window);
}

void
android_map_window (android_window handle)
{
  jobject window;
  jmethodID map_window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  map_window = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				      "mapWindow", "()V");

  (*android_java_env)->CallVoidMethod (android_java_env,
				       window, map_window);
}

void
android_unmap_window (android_window handle)
{
  jobject window;
  jmethodID unmap_window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  unmap_window = android_lookup_method ("org/gnu/emacs/EmacsWindow",
					"unmapWindow", "()V");

  (*android_java_env)->CallVoidMethod (android_java_env,
				       window, unmap_window);
}

void
android_resize_window (android_window handle, unsigned int width,
		       unsigned int height)
{
  jobject window;
  jmethodID resize_window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  resize_window = android_lookup_method ("org/gnu/emacs/EmacsWindow",
					 "resizeWindow", "(II)V");

  (*android_java_env)->CallVoidMethod (android_java_env,
				       window, resize_window,
				       (jint) width, (jint) height);
}

void
android_move_window (android_window handle, int x, int y)
{
  jobject window;
  jmethodID move_window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  move_window = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				       "moveWindow", "(II)V");

  (*android_java_env)->CallVoidMethod (android_java_env,
				       window, move_window,
				       (jint) x, (jint) y);
}

void
android_swap_buffers (struct android_swap_info *swap_info,
		      int num_windows)
{
  jobject window;
  jmethodID swap_buffers;
  int i;

  swap_buffers = android_lookup_method ("org/gnu/emacs/EmacsWindow",
					"swapBuffers", "()V");

  for (i = 0; i < num_windows; ++i)
    {
      window = android_resolve_handle (swap_info[i].swap_window,
				       ANDROID_HANDLE_WINDOW);
      (*android_java_env)->CallVoidMethod (android_java_env,
					   window, swap_buffers);
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
    values->function
      = (*android_java_env)->GetIntField (android_java_env,
					  gcontext,
					  emacs_gc_function);

  if (mask & ANDROID_GC_CLIP_X_ORIGIN)
    values->clip_x_origin
      = (*android_java_env)->GetIntField (android_java_env,
					  gcontext,
					  emacs_gc_clip_x_origin);

  if (mask & ANDROID_GC_CLIP_Y_ORIGIN)
    values->clip_y_origin
      = (*android_java_env)->GetIntField (android_java_env,
					  gcontext,
					  emacs_gc_clip_y_origin);

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

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
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

  if (!colors)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

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

void
android_copy_area (android_drawable src, android_drawable dest,
		   struct android_gc *gc, int src_x, int src_y,
		   unsigned int width, unsigned int height,
		   int dest_x, int dest_y)
{
  jobject src_object, dest_object, gcontext;

  src_object = android_resolve_handle2 (src, ANDROID_HANDLE_WINDOW,
					ANDROID_HANDLE_PIXMAP);
  dest_object = android_resolve_handle2 (dest, ANDROID_HANDLE_WINDOW,
					 ANDROID_HANDLE_PIXMAP);
  gcontext = android_resolve_handle (gc->gcontext,
				     ANDROID_HANDLE_GCONTEXT);

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
				       service_class.copy_area,
				       src_object,
				       dest_object,
				       gcontext,
				       (jint) src_x, (jint) src_y,
				       (jint) width, (jint) height,
				       (jint) dest_x, (jint) dest_y);
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

  if (!array)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

  for (i = 0; i < npoints; ++i)
    {
      point = (*android_java_env)->NewObject (android_java_env,
					      point_class.class,
					      point_class.constructor,
					      (jint) points[i].x,
					      (jint) points[i].y);

      if (!point)
	{
	  (*android_java_env)->ExceptionClear (android_java_env);
	  ANDROID_DELETE_LOCAL_REF (array);
	  memory_full (0);
	}

      (*android_java_env)->SetObjectArrayElement (android_java_env,
						  array, i, point);
      ANDROID_DELETE_LOCAL_REF (point);
    }

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
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

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
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

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
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

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
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

  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
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

      memcpy (word, &pixel, sizeof pixel);
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
  if (!bitmap)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

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
  if (!bitmap)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

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
  (*android_java_env)->CallVoidMethod (android_java_env,
				       emacs_service,
				       service_class.ring_bell);
}

void
android_set_input_focus (android_window handle, unsigned long time)
{
  jobject window;
  jmethodID make_input_focus;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  make_input_focus = android_lookup_method ("org/gnu/emacs/EmacsWindow",
					    "makeInputFocus", "(J)V");

  (*android_java_env)->CallVoidMethod (android_java_env, window,
				       make_input_focus, (jlong) time);
}

void
android_raise_window (android_window handle)
{
  jobject window;
  jmethodID raise;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  raise = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				 "raise", "()V");

  (*android_java_env)->CallVoidMethod (android_java_env, window,
				       raise);
}

void
android_lower_window (android_window handle)
{
  jobject window;
  jmethodID lower;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  lower = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				 "lower", "()V");

  (*android_java_env)->CallVoidMethod (android_java_env, window,
				       lower);
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
  if (!array)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

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
  get_geometry = android_lookup_method ("org/gnu/emacs/EmacsWindow",
					"getWindowGeometry", "()[I");

  window_geometry
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     window,
					     get_geometry);
  if (!window_geometry)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

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
  method = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				  "translateCoordinates",
				  "(II)[I");
  coordinates
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     window, method,
					     (jint) x, (jint) y);

  if (!coordinates)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

  /* The array must contain two elements: X, Y translated to the root
     window.  */
  eassert ((*android_java_env)->GetArrayLength (android_java_env,
						coordinates)
	   == 2);

  /* Obtain the coordinates from the array.  */
  ints = (*android_java_env)->GetIntArrayElements (android_java_env,
						   coordinates, NULL);
  *root_x = ints[0];
  *root_y = ints[1];

  /* Release the coordinates.  */
  (*android_java_env)->ReleaseIntArrayElements (android_java_env,
						coordinates, ints,
						JNI_ABORT);

  /* And free the local reference.  */
  ANDROID_DELETE_LOCAL_REF (coordinates);
}



/* Low level drawing primitives.  */

/* Lock the bitmap corresponding to the window WINDOW.  Return the
   bitmap data upon success, and store the bitmap object in
   BITMAP_RETURN.  Value is NULL upon failure.

   The caller must take care to unlock the bitmap data afterwards.  */

unsigned char *
android_lock_bitmap (android_window window,
		     AndroidBitmapInfo *bitmap_info,
		     jobject *bitmap_return)
{
  jobject drawable, bitmap;
  void *data;

  drawable = android_resolve_handle (window, ANDROID_HANDLE_WINDOW);

  /* Look up the drawable and get the bitmap corresponding to it.
     Then, lock the bitmap's bits.  */
  bitmap = (*android_java_env)->CallObjectMethod (android_java_env,
						  drawable,
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
  if (!rect)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

  /* Post the damage to the drawable.  */
  (*android_java_env)->CallVoidMethod (android_java_env,
				       drawable,
				       drawable_class.damage_rect,
				       rect);
  ANDROID_DELETE_LOCAL_REF (rect);
}



/* Other misc system routines.  */

int
android_get_screen_width (void)
{
  return (*android_java_env)->CallIntMethod (android_java_env,
					     emacs_service,
					     service_class.get_screen_width,
					     (jboolean) false);
}

int
android_get_screen_height (void)
{
  return (*android_java_env)->CallIntMethod (android_java_env,
					     emacs_service,
					     service_class.get_screen_height,
					     (jboolean) false);
}

int
android_get_mm_width (void)
{
  return (*android_java_env)->CallIntMethod (android_java_env,
					     emacs_service,
					     service_class.get_screen_width,
					     (jboolean) true);
}

int
android_get_mm_height (void)
{
  return (*android_java_env)->CallIntMethod (android_java_env,
					     emacs_service,
					     service_class.get_screen_height,
					     (jboolean) true);
}

bool
android_detect_mouse (void)
{
  return (*android_java_env)->CallBooleanMethod (android_java_env,
						 emacs_service,
						 service_class.detect_mouse);
}

void
android_set_dont_focus_on_map (android_window handle,
			       bool no_focus_on_map)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  method = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				  "setDontFocusOnMap", "(Z)V");

  (*android_java_env)->CallVoidMethod (android_java_env, window,
				       method,
				       (jboolean) no_focus_on_map);
}

void
android_set_dont_accept_focus (android_window handle,
			       bool no_accept_focus)
{
  jmethodID method;
  jobject window;

  window = android_resolve_handle (handle, ANDROID_HANDLE_WINDOW);
  method = android_lookup_method ("org/gnu/emacs/EmacsWindow",
				  "setDontAcceptFocus", "(Z)V");

  (*android_java_env)->CallVoidMethod (android_java_env, window,
				       method,
				       (jboolean) no_accept_focus);
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
  android_exception_check ();
  strncpy (name_return, buffer, size - 1);

  (*android_java_env)->ReleaseStringUTFChars (android_java_env,
					      (jstring) string,
					      buffer);
  ANDROID_DELETE_LOCAL_REF (string);
}



#undef faccessat

/* Replace the system faccessat with one which understands AT_EACCESS.
   Android's faccessat simply fails upon using AT_EACCESS, so repalce
   it with zero here.  This isn't caught during configuration.  */

int
faccessat (int dirfd, const char *pathname, int mode, int flags)
{
  static int (*real_faccessat) (int, const char *, int, int);

  if (!real_faccessat)
    real_faccessat = dlsym (RTLD_NEXT, "faccessat");

  return real_faccessat (dirfd, pathname, mode, flags & ~AT_EACCESS);
}



/* Directory listing emulation.  */

struct android_dir
{
  /* The real DIR *, if it exists.  */
  DIR *dir;

  /* Otherwise, the AAssetDir.  */
  void *asset_dir;
};

/* Like opendir.  However, return an asset directory if NAME points to
   an asset.  */

struct android_dir *
android_opendir (const char *name)
{
  struct android_dir *dir;
  AAssetDir *asset_dir;
  const char *asset_name;

  asset_name = android_get_asset_name (name);

  /* If the asset manager exists and NAME is an asset, return an asset
     directory.  */
  if (asset_manager && asset_name)
    {
      asset_dir = AAssetManager_openDir (asset_manager,
					 asset_name);

      if (!asset_dir)
	{
	  errno = ENOENT;
	  return NULL;
	}

      dir = xmalloc (sizeof *dir);
      dir->dir = NULL;
      dir->asset_dir = asset_dir;
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

/* Like readdir, except it understands asset directories.  */

struct dirent *
android_readdir (struct android_dir *dir)
{
  static struct dirent dirent;
  const char *filename;

  if (dir->asset_dir)
    {
      filename = AAssetDir_getNextFileName (dir->asset_dir);
      errno = 0;

      if (!filename)
	return NULL;

      memset (&dirent, 0, sizeof dirent);
      dirent.d_ino = 0;
      dirent.d_off = 0;
      dirent.d_reclen = sizeof dirent;
      dirent.d_type = DT_UNKNOWN;
      strncpy (dirent.d_name, filename,
	       sizeof dirent.d_name - 1);
      return &dirent;
    }

  return readdir (dir->dir);
}

/* Like closedir, but it also closes asset manager directories.  */

void
android_closedir (struct android_dir *dir)
{
  if (dir->dir)
    closedir (dir->dir);
  else
    AAssetDir_close (dir->asset_dir);

  xfree (dir);
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



/* Given a Lisp string TEXT, return a local reference to an equivalent
   Java string.  */

jstring
android_build_string (Lisp_Object text)
{
  Lisp_Object encoded;
  jstring string;

  encoded = ENCODE_UTF_8 (text);

  /* Note that Java expects this string to be in ``modified UTF
     encoding'', which is actually UTF-8, except with NUL encoded as a
     two-byte sequence.  The only consequence of passing an actual
     UTF-8 string is that NUL bytes cannot be represented, which is
     not really of consequence.  */
  string = (*android_java_env)->NewStringUTF (android_java_env,
					      SSDATA (encoded));
  if (!string)
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }

  return string;
}

/* Check for JNI exceptions and call memory_full in that
   situation.  */

void
android_exception_check (void)
{
  if ((*android_java_env)->ExceptionCheck (android_java_env))
    {
      (*android_java_env)->ExceptionClear (android_java_env);
      memory_full (0);
    }
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

#endif
