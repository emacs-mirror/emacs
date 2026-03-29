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

/* On Android, Emacs is built as a shared library loaded from Java
   using the Java Native Interface.  Emacs's `main' function is
   renamed `android_emacs_init', and runs with some modifications
   inside a separate thread, communicating with the Java code through
   a table of function pointers.  */

#ifndef _ANDROID_H_
#define _ANDROID_H_

#ifndef ANDROID_STUBIFY
#include <jni.h>
#include <pwd.h>

#include <sys/stat.h>
#include <sys/select.h>

#include <dirent.h>
#include <stdio.h>

#include <android/bitmap.h>

#include "androidgui.h"
#include "lisp.h"
#endif /* ANDROID_STUBIFY */

extern bool android_init_gui;

#ifndef ANDROID_STUBIFY

extern char *android_cache_dir;

extern int android_emacs_init (int, char **, char *);
extern int android_select (int, fd_set *, fd_set *, fd_set *,
			   struct timespec *);
extern char *android_user_full_name (struct passwd *);



/* Structure describing the android.os.ParcelFileDescriptor class used
   to wrap file descriptors sent over IPC.  */

struct android_parcel_file_descriptor_class
{
  jclass class;
  jmethodID close;
  jmethodID get_fd;
  jmethodID detach_fd;
};

/* The ParcelFileDescriptor class.  */
extern struct android_parcel_file_descriptor_class fd_class;

extern void android_init_fd_class (JNIEnv *);

/* File I/O operations.  Many of these are defined in
   androidvfs.c.  */

extern bool android_is_special_directory (const char *, const char *);
extern const char *android_get_home_directory (void);

extern void android_vfs_init (JNIEnv *, jobject);

extern int android_open (const char *, int, mode_t);
extern int android_fstat (int, struct stat *);
extern int android_fstatat (int, const char *restrict,
			    struct stat *restrict, int);
extern int android_faccessat (int, const char *, int, int);
extern int android_close (int);
extern FILE *android_fdopen (int, const char *);
extern int android_fclose (FILE *);
extern int android_unlink (const char *);
extern int android_symlink (const char *, const char *);
extern int android_rmdir (const char *);
extern int android_mkdir (const char *, mode_t);
extern int android_renameat_noreplace (int, const char *,
				       int, const char *);
extern int android_rename (const char *, const char *);
extern int android_fchmodat (int, const char *, mode_t, int);
extern ssize_t android_readlinkat (int, const char *restrict, char *restrict,
				   size_t);



extern double android_pixel_density_x, android_pixel_density_y;
extern double android_scaled_pixel_density;
extern int android_ui_mode;

static_assert (sizeof (android_handle) == sizeof (jobject));
#define android_resolve_handle(handle) ((jobject) (handle))

extern unsigned char *android_lock_bitmap (android_drawable,
					   AndroidBitmapInfo *,
					   jobject *);
extern void android_damage_window (android_window,
				   struct android_rectangle *);
extern int android_get_screen_width (void);
extern int android_get_screen_height (void);
extern int android_get_mm_width (void);
extern int android_get_mm_height (void);
extern bool android_detect_mouse (void);
extern bool android_detect_keyboard (void);

extern void android_set_dont_focus_on_map (android_window, bool);
extern void android_set_dont_accept_focus (android_window, bool);
extern void android_set_wm_name (android_window, jstring);

extern int android_verify_jni_string (const char *);
extern jstring android_build_string (Lisp_Object, ...);
extern jstring android_build_jstring (const char *);
extern void android_exception_check (void);
extern void android_exception_check_1 (jobject);
extern void android_exception_check_2 (jobject, jobject);
extern void android_exception_check_3 (jobject, jobject, jobject);
extern void android_exception_check_4 (jobject, jobject, jobject, jobject);
extern void android_exception_check_5 (jobject, jobject, jobject, jobject,
				       jobject);
extern void android_exception_check_6 (jobject, jobject, jobject, jobject,
				       jobject, jobject);
extern void android_exception_check_nonnull (void *, jobject);
extern void android_exception_check_nonnull_1 (void *, jobject, jobject);

extern void android_get_keysym_name (int, char *, size_t);
extern void android_wait_event (void);
extern void android_toggle_on_screen_keyboard (android_window, bool);
extern _Noreturn void android_restart_emacs (void);
extern int android_request_directory_access (void);
extern bool android_external_storage_available_p (void);
extern void android_request_storage_access (void);
extern int android_get_current_api_level (void)
  __attribute__ ((pure));

/* Define `android_get_current_api_level' to a macro that the compiler
   knows will always return at least __ANDROID_API__.  */

#define android_get_current_api_level()				\
  ({ int value;							\
								\
     value = (android_get_current_api_level) ();		\
     eassume (value >= __ANDROID_API__); value; })



/* Directory listing emulation.  */

struct android_vdir;

extern struct android_vdir *android_opendir (const char *);
extern int android_dirfd (struct android_vdir *);
extern struct dirent *android_readdir (struct android_vdir *);
extern void android_closedir (struct android_vdir *);



/* External asset manager interface.  */

struct android_fd_or_asset
{
  /* The file descriptor.  */
  int fd;

  /* The asset.  If set, FD is not a real file descriptor.  */
  void *asset;
};

extern struct android_fd_or_asset android_open_asset (const char *,
						      int, mode_t);
extern int android_close_asset (struct android_fd_or_asset);
extern ssize_t android_asset_read_quit (struct android_fd_or_asset,
					void *, size_t);
extern ssize_t android_asset_read (struct android_fd_or_asset,
				   void *, size_t);
extern off_t android_asset_lseek (struct android_fd_or_asset, off_t, int);
extern int android_asset_fstat (struct android_fd_or_asset,
				struct stat *);



/* Very miscellaneous functions.  */

struct android_battery_state
{
  /* Battery charge level in integer percentage.  */
  intmax_t capacity;

  /* Battery charge level in microampere-hours.  */
  intmax_t charge_counter;

  /* Battery current in microampere-hours.  */
  intmax_t current_average;

  /* Instantaneous battery current in microampere-hours.  */
  intmax_t current_now;

  /* Estimate as to the amount of time remaining until the battery is
     charged, in milliseconds.  */
  intmax_t remaining;

  /* Battery status.  The value is either:

       2, if the battery is charging.
       3, if the battery is discharging.
       5, if the battery is full.
       4, if the battery is not full or discharging,
          but is not charging either.
       1, if the battery state is unknown.  */
  int status;

  /* The power source of the battery.  Value is:

       0, if on battery power.
       1, for line power.
       8, for dock power.
       2, for USB power.
       4, for wireless power.  */
  int plugged;

  /* The temperature of the battery in 10 * degrees centigrade.  */
  int temperature;
};

extern Lisp_Object android_browse_url (Lisp_Object, Lisp_Object);
extern int android_query_battery (struct android_battery_state *);
extern void android_display_toast (const char *);



/* Event loop functions.  */

extern void android_before_select (void);
extern void android_check_query (void);
extern void android_check_query_urgent (void);
extern int android_run_in_emacs_thread (void (*) (void *), void *);
extern void android_write_event (union android_event *);

extern unsigned int event_serial;



/* Process related functions.  */
extern int android_rewrite_spawn_argv (const char ***);

#else /* ANDROID_STUBIFY */

/* Define a substitute for use during Emacs compilation.  */

#define android_is_special_directory(name, dir) (false)

#endif /* !ANDROID_STUBIFY */

/* JNI functions should not be built when Emacs is stubbed out for the
   build.  These should be documented in EmacsNative.java.  */

#ifndef ANDROID_STUBIFY
#include <jni.h>

struct android_emacs_service
{
  jclass class;
  jmethodID fill_rectangle;
  jmethodID fill_polygon;
  jmethodID draw_rectangle;
  jmethodID draw_line;
  jmethodID draw_point;
  jmethodID ring_bell;
  jmethodID query_tree;
  jmethodID get_screen_width;
  jmethodID get_screen_height;
  jmethodID detect_mouse;
  jmethodID detect_keyboard;
  jmethodID name_keysym;
  jmethodID browse_url;
  jmethodID restart_emacs;
  jmethodID update_ic;
  jmethodID reset_ic;
  jmethodID open_content_uri;
  jmethodID check_content_uri;
  jmethodID query_battery;
  jmethodID update_extracted_text;
  jmethodID update_cursor_anchor_info;
  jmethodID get_document_authorities;
  jmethodID request_directory_access;
  jmethodID get_document_trees;
  jmethodID document_id_from_name;
  jmethodID get_tree_uri;
  jmethodID stat_document;
  jmethodID access_document;
  jmethodID open_document_directory;
  jmethodID read_directory_entry;
  jmethodID open_document;
  jmethodID create_document;
  jmethodID create_directory;
  jmethodID delete_document;
  jmethodID rename_document;
  jmethodID move_document;
  jmethodID valid_authority;
  jmethodID external_storage_available;
  jmethodID request_storage_access;
  jmethodID cancel_notification;
  jmethodID relinquish_uri_rights;
};

extern JNIEnv *android_java_env;

#ifdef THREADS_ENABLED
extern JavaVM *android_jvm;
#endif /* THREADS_ENABLED */

/* The Java String class.  */
extern jclass java_string_class;

/* The EmacsService object.  */
extern jobject emacs_service;

/* Various methods associated with the EmacsService.  */
extern struct android_emacs_service service_class;

/* The time at which Emacs was installed, which also supplies the
   mtime of asset files.  */
extern struct timespec emacs_installation_time;

#define ANDROID_DELETE_LOCAL_REF(ref)				\
  ((*android_java_env)->DeleteLocalRef (android_java_env,	\
					ref))

#define NATIVE_NAME(name) Java_org_gnu_emacs_EmacsNative_##name

/* Prologue which must be inserted before each JNI function.
   See initEmacs for why.  */

#if defined __i386__
extern void *unused_pointer;

#define JNI_STACK_ALIGNMENT_PROLOGUE				\
  __attribute__ ((aligned (32))) char stack_align_buffer[32];	\
								\
  /* Trick GCC into not optimizing this variable away.  */	\
  unused_pointer = stack_align_buffer;

#else /* !__i386__ */
#define JNI_STACK_ALIGNMENT_PROLOGUE ((void) 0)
#endif /* __i386__ */

#endif /* !ANDROID_STUBIFY */
#endif /* _ANDROID_H_ */
