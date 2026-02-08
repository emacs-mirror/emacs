/* Communication module for Android terminals.

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
#include <assert.h>
#include <minmax.h>
#include <unistd.h>
#include <dlfcn.h>

#include <boot-time.h>
#include <sys/types.h>

#include "lisp.h"
#include "blockinput.h"
#include "coding.h"
#include "android.h"
#include "androidterm.h"
#include "termhooks.h"

/* Selection support on Android is confined to copying and pasting of
   plain text and MIME data from the clipboard.  There is no primary
   selection.

   While newer versions of Android are supposed to have the necessary
   interfaces for transferring other kinds of selection data, doing so
   is too complicated, and involves registering ``content providers''
   and all kinds of other stuff; for this reason, Emacs does not
   support setting the clipboard contents to anything other than plain
   text.  */



/* Structure describing the EmacsClipboard class.  */

struct android_emacs_clipboard
{
  jclass class;
  jmethodID set_clipboard;
  jmethodID owns_clipboard;
  jmethodID clipboard_exists;
  jmethodID get_clipboard;
  jmethodID make_clipboard;
  jmethodID get_clipboard_targets;
  jmethodID get_clipboard_data;
};

/* Methods associated with the EmacsClipboard class.  */
static struct android_emacs_clipboard clipboard_class;

/* Reference to the EmacsClipboard object.  */
static jobject clipboard;



static void
android_init_emacs_clipboard (void)
{
  jclass old;

  clipboard_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsClipboard");
  eassert (clipboard_class.class);

  old = clipboard_class.class;
  clipboard_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!clipboard_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  clipboard_class.c_name					\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					clipboard_class.class,	\
					name, signature);	\
  eassert (clipboard_class.c_name);

  FIND_METHOD (set_clipboard, "setClipboard", "(Ljava/lang/String;)V");
  FIND_METHOD (owns_clipboard, "ownsClipboard", "()I");
  FIND_METHOD (clipboard_exists, "clipboardExists", "()Z");
  FIND_METHOD (get_clipboard, "getClipboard", "()Ljava/lang/String;");
  FIND_METHOD (get_clipboard_targets, "getClipboardTargets",
	       "()[Ljava/lang/String;");
  FIND_METHOD (get_clipboard_data, "getClipboardData",
	       "(Ljava/lang/String;)Landroid/content/res/"
	       "AssetFileDescriptor;");

  clipboard_class.make_clipboard
    = (*android_java_env)->GetStaticMethodID (android_java_env,
					      clipboard_class.class,
					      "makeClipboard",
					      "()Lorg/gnu/emacs/"
					      "EmacsClipboard;");
  eassert (clipboard_class.make_clipboard);

#undef FIND_METHOD
}




DEFUN ("android-clipboard-owner-p", Fandroid_clipboard_owner_p,
       Sandroid_clipboard_owner_p, 0, 0, 0,
       doc: /* Return whether or not Emacs owns the clipboard.
Alternatively, return the symbol `lambda' if that could not be
determined.  */)
  (void)
{
  jint rc;

  if (!android_init_gui)
    error ("Accessing clipboard without display connection");

  block_input ();
  rc = (*android_java_env)->CallIntMethod (android_java_env,
					   clipboard,
					   clipboard_class.owns_clipboard);
  android_exception_check ();
  unblock_input ();

  /* If rc is 0 or 1, then Emacs knows whether or not it owns the
     clipboard.  If rc is -1, then Emacs does not.  */

  if (rc < 0)
    return Qlambda;

  return rc ? Qt : Qnil;
}

DEFUN ("android-set-clipboard", Fandroid_set_clipboard,
       Sandroid_set_clipboard, 1, 1, 0,
       doc: /* Set the clipboard text to STRING.  */)
  (Lisp_Object string)
{
  jstring text;

  if (!android_init_gui)
    error ("Accessing clipboard without display connection");

  CHECK_STRING (string);
  string = code_convert_string_norecord (string, Qandroid_jni,
					 true);

  text = (*android_java_env)->NewStringUTF (android_java_env,
					    SSDATA (string));
  android_exception_check ();

  (*android_java_env)->CallVoidMethod (android_java_env,
				       clipboard,
				       clipboard_class.set_clipboard,
				       text);
  android_exception_check_1 (text);
  ANDROID_DELETE_LOCAL_REF (text);

  return Qnil;
}

DEFUN ("android-get-clipboard", Fandroid_get_clipboard,
       Sandroid_get_clipboard, 0, 0, 0,
       doc: /* Return the current contents of the clipboard.
Value is a multibyte string containing decoded clipboard
text.
Alternatively, return nil if the clipboard is empty.  */)
  (void)
{
  Lisp_Object string;
  jstring text;
  jmethodID method;
  jsize length;
  const char *data;

  if (!android_init_gui)
    error ("No Android display connection");

  method = clipboard_class.get_clipboard;
  text
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     clipboard,
					     method);
  android_exception_check ();

  if (!text)
    return Qnil;

  /* Retrieve a pointer to the raw JNI-encoded bytes of the string.  */
  length = (*android_java_env)->GetStringUTFLength (android_java_env,
						    text);
  data = (*android_java_env)->GetStringUTFChars (android_java_env, text,
						 NULL);
  android_exception_check_nonnull ((void *) data, text);

  /* Copy them into a unibyte string for decoding.  */
  string = make_unibyte_string (data, length);
  (*android_java_env)->ReleaseStringUTFChars (android_java_env, text,
					      data);
  ANDROID_DELETE_LOCAL_REF (text);

  /* Now decode the resulting string.  */
  return code_convert_string_norecord (string, Qandroid_jni, false);
}

DEFUN ("android-clipboard-exists-p", Fandroid_clipboard_exists_p,
       Sandroid_clipboard_exists_p, 0, 0, 0,
       doc: /* Return whether or not clipboard contents exist.  */)
  (void)
{
  jboolean rc;
  jmethodID method;

  if (!android_init_gui)
    error ("No Android display connection");

  method = clipboard_class.clipboard_exists;
  rc = (*android_java_env)->CallBooleanMethod (android_java_env,
					       clipboard,
					       method);
  android_exception_check ();

  return rc ? Qt : Qnil;
}

DEFUN ("android-browse-url-internal", Fandroid_browse_url_internal,
       Sandroid_browse_url_internal, 1, 2, 0,
       doc: /* Open URL in an external application.

URL should be a URL-encoded URL with a scheme specified unless SEND is
non-nil.  Signal an error upon failure.

If SEND is nil, start a program that is able to display the URL, such
as a web browser.  Otherwise, try to share URL using programs such as
email clients.

If URL is a file URI, convert it into a `content' address accessible to
other programs.  Files inside the /content or /assets directories cannot
be opened through such addresses, which this function does not provide
for.  Use `android-browse-url' instead.  */)
  (Lisp_Object url, Lisp_Object send)
{
  Lisp_Object value;

  if (!android_init_gui)
    error ("No Android display connection");

  CHECK_STRING (url);
  value = android_browse_url (url, send);

  /* Signal an error upon failure.  */
  if (!NILP (value))
    signal_error ("Error browsing URL", value);

  return Qnil;
}



/* MIME clipboard support.  This provides support for reading MIME
   data (but not text) from the clipboard.  */

DEFUN ("android-get-clipboard-targets", Fandroid_get_clipboard_targets,
       Sandroid_get_clipboard_targets, 0, 0, 0,
       doc: /* Return a list of data types in the clipboard.
Value is a list of MIME types as strings, each defining a single extra
data type available from the clipboard.  */)
  (void)
{
  jarray all_targets;
  jstring string;
  jmethodID method;
  size_t length, i;
  const char *data;
  Lisp_Object targets, tem;

  if (!android_init_gui)
    error ("No Android display connection");

  targets = Qnil;
  block_input ();
  method = clipboard_class.get_clipboard_targets;
  all_targets = (*android_java_env)->CallObjectMethod (android_java_env,
						       clipboard, method);
  android_exception_check ();

  if (!all_targets)
    goto fail;

  length = (*android_java_env)->GetArrayLength (android_java_env,
					        all_targets);
  for (i = 0; i < length; ++i)
    {
      /* Retrieve the MIME type.  */
      string
	= (*android_java_env)->GetObjectArrayElement (android_java_env,
						      all_targets, i);
      android_exception_check_nonnull (string, all_targets);

      /* Cons it onto the list of targets.  */
      data = (*android_java_env)->GetStringUTFChars (android_java_env,
						     string, NULL);
      android_exception_check_nonnull_1 ((void *) data, string,
					 all_targets);

      /* Decode the string.  */
      tem = build_unibyte_string ((char *) data);
      tem = code_convert_string_norecord (tem, Qandroid_jni, false);
      targets = Fcons (tem, targets);

      /* Delete the retrieved data.  */
      (*android_java_env)->ReleaseStringUTFChars (android_java_env,
						  string, data);
      ANDROID_DELETE_LOCAL_REF (string);
    }
  unblock_input ();

  ANDROID_DELETE_LOCAL_REF (all_targets);
  return Fnreverse (targets);

 fail:
  unblock_input ();
  return Qnil;
}



struct android_asset_file_descriptor
{
  jclass class;
  jmethodID close;
  jmethodID get_length;
  jmethodID get_start_offset;
  jmethodID get_file_descriptor;
  jmethodID get_parcel_file_descriptor;
  jmethodID get_fd;
};

/* Methods associated with the AssetFileDescriptor class.  */
static struct android_asset_file_descriptor asset_fd_class;

/* Initialize virtual function IDs and class pointers in connection with
   the AssetFileDescriptor class.  */

static void
android_init_asset_file_descriptor (void)
{
  jclass old;

  asset_fd_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "android/content/res/"
				      "AssetFileDescriptor");
  eassert (asset_fd_class.class);

  old = asset_fd_class.class;
  asset_fd_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!asset_fd_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  asset_fd_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
				        asset_fd_class.class,	\
					name, signature);	\
  eassert (asset_fd_class.c_name);

  FIND_METHOD (close, "close", "()V");
  FIND_METHOD (get_length, "getLength", "()J");
  FIND_METHOD (get_start_offset, "getStartOffset", "()J");
  FIND_METHOD (get_file_descriptor, "getFileDescriptor",
	       "()Ljava/io/FileDescriptor;");
  FIND_METHOD (get_parcel_file_descriptor, "getParcelFileDescriptor",
	       "()Landroid/os/ParcelFileDescriptor;");
#undef FIND_METHOD
}

/* Free the memory inside PTR, a pointer to a char pointer.  */

static void
android_xfree_inside (void *ptr)
{
  xfree (*(char **) ptr);
}

/* Close the referent of, then delete, the local reference to an asset
   file descriptor referenced by AFD.  */

static void
close_asset_fd (void *afd)
{
  jobject *afd_1;

  afd_1 = afd;
  (*android_java_env)->CallVoidMethod (android_java_env, *afd_1,
				       asset_fd_class.close);
  (*android_java_env)->ExceptionClear (android_java_env);
  ANDROID_DELETE_LOCAL_REF (*afd_1);
}

/* Return the offset, file descriptor and length of the data contained
   in the asset file descriptor AFD, in *FD, *OFFSET, and *LENGTH.  AFD
   will not be released if an exception is detected; it is the
   responsibility of the caller to arrange that it be.

   Value is 0 upon success, 1 otherwise.  */

static int
extract_fd_offsets (jobject afd, int *fd, jlong *offset, jlong *length)
{
  jobject java_fd;
  void *handle;
#if __ANDROID_API__ <= 11
  static int (*jniGetFDFromFileDescriptor) (JNIEnv *, jobject);
#endif /* __ANDROID_API__ <= 11 */
  static int (*AFileDescriptor_getFd) (JNIEnv *, jobject);
  jmethodID method;

  method  = asset_fd_class.get_start_offset;
  *offset = (*android_java_env)->CallLongMethod (android_java_env,
						 afd, method);
  android_exception_check ();
  method  = asset_fd_class.get_length;
  *length = (*android_java_env)->CallLongMethod (android_java_env,
						 afd, method);
  android_exception_check ();

#if __ANDROID_API__ <= 11
  if (android_get_current_api_level () <= 11)
    {
      /* Load libnativehelper and link to a private interface that is
	 the only means of retrieving the file descriptor from an asset
	 file descriptor on these systems.  */

      if (!jniGetFDFromFileDescriptor)
	{
	  handle = dlopen ("libnativehelper.so",
			   RTLD_LAZY | RTLD_GLOBAL);
	  if (!handle)
	    goto failure;
	  jniGetFDFromFileDescriptor = dlsym (handle,
					      "jniGetFDFromFileDescriptor");
	  if (!jniGetFDFromFileDescriptor)
	    goto failure;
	}

      method  = asset_fd_class.get_file_descriptor;
      java_fd = (*android_java_env)->CallObjectMethod (android_java_env,
						       afd, method);
      android_exception_check ();
      *fd = (*jniGetFDFromFileDescriptor) (android_java_env, java_fd);
      ANDROID_DELETE_LOCAL_REF (java_fd);

      if (*fd >= 0)
	return 0;
    }
  else
#endif /* __ANDROID_API__ <= 11 */
#if __ANDROID_API__ <= 30
  if (android_get_current_api_level () <= 30)
    {
      /* Convert this AssetFileDescriptor into a ParcelFileDescriptor,
	 whose getFd method will return its native file descriptor.  */
      method  = asset_fd_class.get_parcel_file_descriptor;
      java_fd = (*android_java_env)->CallObjectMethod (android_java_env,
						       afd, method);
      android_exception_check ();

      /* Initialize fd_class if not already complete.  */
      android_init_fd_class (android_java_env);
      *fd = (*android_java_env)->CallIntMethod (android_java_env,
						java_fd,
						fd_class.get_fd);
      android_exception_check_1 (java_fd);
      ANDROID_DELETE_LOCAL_REF (java_fd);

      if (*fd >= 0)
	return 0;
    }
  else
#endif /* __ANDROID_API__ <= 30 */
    {
      /* Load libnativehelper (now a public interface) and link to
	 AFileDescriptor_getFd.  */
      if (!AFileDescriptor_getFd)
	{
	  handle = dlopen ("libnativehelper.so",
			   RTLD_LAZY | RTLD_GLOBAL);
	  if (!handle)
	    goto failure;
	  AFileDescriptor_getFd = dlsym (handle, "AFileDescriptor_getFd");
	  if (!AFileDescriptor_getFd)
	    goto failure;
	}

      method  = asset_fd_class.get_file_descriptor;
      java_fd = (*android_java_env)->CallObjectMethod (android_java_env,
						       afd, method);
      android_exception_check ();
      *fd = (*AFileDescriptor_getFd) (android_java_env, java_fd);
      ANDROID_DELETE_LOCAL_REF (java_fd);

      if (*fd >= 0)
	return 0;
    }

 failure:
  return 1;
}

DEFUN ("android-get-clipboard-data", Fandroid_get_clipboard_data,
       Sandroid_get_clipboard_data, 1, 1, 0,
       doc: /* Return the clipboard data of the given MIME TYPE.
Value is a unibyte string containing the entire contents of the
clipboard, after its owner has converted the data to the given
MIME type.  Value is nil if the conversion fails, or if the data
is not present.

Value is also nil if the clipboard data consists of a single URL which
does not have any corresponding data.  In that case, use
`android-get-clipboard' instead.  */)
  (Lisp_Object type)
{
  jobject afd;
  jstring mime_type;
  jmethodID method;
  int fd;
  ptrdiff_t rc;
  jlong offset, length;
  specpdl_ref ref;
  char *buffer, *start;

  if (!android_init_gui)
    error ("No Android display connection");

  CHECK_STRING (type);

  /* Convert TYPE into a Java string.  */
  block_input ();
  mime_type = android_build_string (type, NULL);
  method = clipboard_class.get_clipboard_data;
  afd = (*android_java_env)->CallObjectMethod (android_java_env,
					       clipboard, method,
					       mime_type);
  android_exception_check_1 (mime_type);
  ANDROID_DELETE_LOCAL_REF (mime_type);

  if (!afd)
    goto fail;

  /* Extract the file descriptor from the AssetFileDescriptor
     object.  */
  ref = SPECPDL_INDEX ();
  record_unwind_protect_ptr (close_asset_fd, &afd);

  if (extract_fd_offsets (afd, &fd, &offset, &length))
    {
      unblock_input ();
      return unbind_to (ref, Qnil);
    }
  unblock_input ();

  /* Now begin reading from fd.  */

  if (length != -1)
    {
      buffer = xmalloc (MIN (length, PTRDIFF_MAX));
      record_unwind_protect_ptr (xfree, buffer);

      rc = emacs_read_quit (fd, buffer,
			    MIN (length, PTRDIFF_MAX));

      /* Return nil upon an IO problem.  */
      if (rc < 0)
	return unbind_to (ref, Qnil);

      /* Return the data as a unibyte string.  */
      return unbind_to (ref, make_unibyte_string (buffer, rc));
    }

  /* Otherwise, read BUFSIZ bytes at a time.  */
  buffer = xmalloc (BUFSIZ);
  length = 0;
  start = buffer;

  record_unwind_protect_ptr (android_xfree_inside, &buffer);

  /* Seek to the start of the data.  */

  if (offset)
    {
      if (lseek (fd, offset, SEEK_SET) < 0)
	return unbind_to (ref, Qnil);
    }

  while (true)
    {
      rc = emacs_read_quit (fd, start, BUFSIZ);

      if (ckd_add (&length, length, rc)
	  || PTRDIFF_MAX - length < BUFSIZ)
	memory_full (PTRDIFF_MAX);

      if (rc < 0)
	return unbind_to (ref, Qnil);

      if (rc < BUFSIZ)
	break;

      buffer = xrealloc (buffer, length + BUFSIZ);
      start = buffer + length;
    }

  return unbind_to (ref, make_unibyte_string (buffer, rc));

 fail:
  unblock_input ();
  return Qnil;
}



/* Desktop notifications.  `android-desktop-notify' implements a
   facsimile of `notifications-notify'.  */

/* Structure describing the EmacsDesktopNotification class.  */

struct android_emacs_desktop_notification
{
  jclass class;
  jmethodID init;
  jmethodID display;
};

/* Methods provided by the EmacsDesktopNotification class.  */
static struct android_emacs_desktop_notification notification_class;

/* Hash table pairing notification identifiers with callbacks.  */
static Lisp_Object notification_table;

/* Initialize virtual function IDs and class pointers tied to the
   EmacsDesktopNotification class.  */

static void
android_init_emacs_desktop_notification (void)
{
  jclass old;

  notification_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsDesktopNotification");
  eassert (notification_class.class);

  old = notification_class.class;
  notification_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!notification_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)				\
  notification_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,		\
				        notification_class.class,	\
					name, signature);		\
  eassert (notification_class.c_name);

  FIND_METHOD (init, "<init>", "(Ljava/lang/String;"
	       "Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;II[Ljava/lang/String;"
	       "[Ljava/lang/String;J)V");
  FIND_METHOD (display, "display", "()V");
#undef FIND_METHOD
}

/* Return the numeric resource ID designating the icon within the
   ``android.R.drawable'' package by the supplied NAME.

   If no icon is found, return that of
   ``android.R.drawable.ic_dialog_alert''.  */

static jint
android_locate_icon (const char *name)
{
  jclass drawable;
  jfieldID field;
  jint rc;

  if (android_verify_jni_string (name))
    /* If NAME isn't valid, return the default value.  */
    return 17301543; /* android.R.drawable.ic_dialog_alert.  */

  drawable = (*android_java_env)->FindClass (android_java_env,
					     "android/R$drawable");
  android_exception_check ();

  field = (*android_java_env)->GetStaticFieldID (android_java_env,
						 drawable, name, "I");
  (*android_java_env)->ExceptionClear (android_java_env);

  if (!field)
    rc = 17301543; /* android.R.drawable.ic_dialog_alert.  */
  else
    rc = (*android_java_env)->GetStaticIntField (android_java_env,
						 drawable, field);

  ANDROID_DELETE_LOCAL_REF (drawable);
  return rc;
}

/* Display a desktop notification with the provided TITLE, BODY,
   REPLACES_ID, GROUP, ICON, URGENCY, ACTIONS, TIMEOUT, RESIDENT,
   ACTION_CB and CLOSE_CB.  Return an identifier for the resulting
   notification.  */

static intmax_t
android_notifications_notify_1 (Lisp_Object title, Lisp_Object body,
				Lisp_Object replaces_id,
				Lisp_Object group, Lisp_Object icon,
				Lisp_Object urgency, Lisp_Object actions,
				Lisp_Object timeout, Lisp_Object resident,
				Lisp_Object action_cb, Lisp_Object close_cb)
{
  static intmax_t counter;
  intmax_t id;
  jstring title1, body1, group1, identifier1;
  jint type, icon1;
  jobject notification;
  jobjectArray action_keys, action_titles;
  char identifier[INT_STRLEN_BOUND (int)
		  + INT_STRLEN_BOUND (long int)
		  + INT_STRLEN_BOUND (intmax_t)
		  + sizeof "..."];
  struct timespec boot_time;
  Lisp_Object key, value, tem;
  jint nitems, i;
  jstring item;
  Lisp_Object length;
  jlong timeout_val;

  if (EQ (urgency, Qlow))
    type = 2; /* IMPORTANCE_LOW */
  else if (EQ (urgency, Qnormal))
    type = 3; /* IMPORTANCE_DEFAULT */
  else if (EQ (urgency, Qcritical))
    type = 4; /* IMPORTANCE_HIGH */
  else
    signal_error ("Invalid notification importance given", urgency);

  /* Decode the timeout.  */

  timeout_val = 0;

  if (!NILP (timeout))
    {
      CHECK_INTEGER (timeout);

      if (!integer_to_intmax (timeout, &id)
	  || id > TYPE_MAXIMUM (jlong)
	  || id < TYPE_MINIMUM (jlong))
	signal_error ("Invalid timeout", timeout);

      if (id > 0)
	timeout_val = id;
    }

  nitems = 0;

  /* If ACTIONS is provided, split it into two arrays of Java strings
     holding keys and titles.  */

  if (!NILP (actions))
    {
      /* Count the number of items to be inserted.  */

      length = Flength (actions);
      if (!TYPE_RANGED_FIXNUMP (jint, length))
	error ("Action list too long");
      nitems = XFIXNAT (length);
      if (nitems & 1)
	error ("Length of action list is invalid");
      nitems /= 2;

      /* Verify that the list consists exclusively of strings.  */
      tem = actions;
      FOR_EACH_TAIL (tem)
	CHECK_STRING (XCAR (tem));
    }

  if (NILP (replaces_id))
    {
      /* Generate a new identifier.  */
      ckd_add (&counter, counter, 1);
      id = counter;
    }
  else
    {
      CHECK_INTEGER (replaces_id);
      if (!integer_to_intmax (replaces_id, &id))
	id = -1; /* Overflow.  */
    }

  /* Locate the integer ID linked to ICON.  */
  icon1 = android_locate_icon (SSDATA (icon));

  /* Generate a unique identifier for this notification.  Because
     Android persists notifications past system shutdown, also include
     the boot time within IDENTIFIER.  Scale it down to avoid being
     perturbed by minor instabilities in the returned boot time,
     however.  */

  boot_time.tv_sec = 0;
  get_boot_time (&boot_time);
  sprintf (identifier, "%d.%ld.%jd", (int) getpid (),
	   (long int) (boot_time.tv_sec / 2), id);

  /* Encode all strings into their Java counterparts.  */
  title1 = android_build_string (title, NULL);
  body1  = android_build_string (body, title1, NULL);
  group1 = android_build_string (group, body1, title1, NULL);
  identifier1
    = (*android_java_env)->NewStringUTF (android_java_env, identifier);
  android_exception_check_3 (title1, body1, group1);

  /* Create the arrays for action identifiers and titles if
     provided.  */

  if (nitems)
    {
      action_keys = (*android_java_env)->NewObjectArray (android_java_env,
							 nitems,
							 java_string_class,
							 NULL);
      android_exception_check_4 (title, body1, group1, identifier1);
      action_titles = (*android_java_env)->NewObjectArray (android_java_env,
							   nitems,
							   java_string_class,
							   NULL);
      android_exception_check_5 (title, body1, group1, identifier1,
				 action_keys);

      for (i = 0; i < nitems; ++i)
	{
	  key = XCAR (actions);
	  value = XCAR (XCDR (actions));
	  actions = XCDR (XCDR (actions));

	  /* Create a string for this action.  */
	  item = android_build_string (key, body1, group1, identifier1,
				       action_keys, action_titles, NULL);
	  (*android_java_env)->SetObjectArrayElement (android_java_env,
						      action_keys, i,
						      item);
	  ANDROID_DELETE_LOCAL_REF (item);

	  /* Create a string for this title.  */
	  item = android_build_string (value, body1, group1, identifier1,
				       action_keys, action_titles, NULL);
	  (*android_java_env)->SetObjectArrayElement (android_java_env,
						      action_titles, i,
						      item);
	  ANDROID_DELETE_LOCAL_REF (item);
	}
    }
  else
    {
      action_keys = NULL;
      action_titles = NULL;
    }

  /* Create the notification.  */
  notification
    = (*android_java_env)->NewObject (android_java_env,
				      notification_class.class,
				      notification_class.init,
				      title1, body1, group1,
				      identifier1, icon1, type,
				      action_keys, action_titles,
				      timeout_val);
  android_exception_check_6 (title1, body1, group1, identifier1,
			     action_titles, action_keys);

  /* Delete unused local references.  */
  ANDROID_DELETE_LOCAL_REF (title1);
  ANDROID_DELETE_LOCAL_REF (body1);
  ANDROID_DELETE_LOCAL_REF (group1);
  ANDROID_DELETE_LOCAL_REF (identifier1);
  ANDROID_DELETE_LOCAL_REF (action_keys);
  ANDROID_DELETE_LOCAL_REF (action_titles);

  /* Display the notification.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 notification,
						 notification_class.class,
						 notification_class.display);
  android_exception_check_1 (notification);
  ANDROID_DELETE_LOCAL_REF (notification);

  /* If callbacks are provided, save them into notification_table. */

  if (!NILP (action_cb) || !NILP (close_cb) || !NILP (resident))
    Fputhash (build_string (identifier), list3 (action_cb, close_cb,
						resident),
	      notification_table);

  /* Return the ID.  */
  return id;
}

DEFUN ("android-notifications-notify", Fandroid_notifications_notify,
       Sandroid_notifications_notify, 0, MANY, 0, doc:
       /* Display a desktop notification.
ARGS must contain keywords followed by values.  Each of the following
keywords is understood:

  :title	The notification title.
  :body		The notification body.
  :replaces-id	The ID of a previous notification to supersede.
  :group	The notification group, or nil.
  :urgency	One of the symbols `low', `normal' or `critical',
		defining the importance of the notification group.
  :icon		The name of a drawable resource to display as the
		notification's icon.
  :actions	A list of actions of the form:
		  (KEY TITLE KEY TITLE ...)
		where KEY and TITLE are both strings.
		The action for which CALLBACK is called when the
		notification itself is selected is named "default",
		its existence is implied, and its TITLE is ignored.
		No more than three actions defined here will be
		displayed, not counting any with "default" as its
		key.
  :timeout	Number of milliseconds from the display of the
		notification at which it will be automatically
		dismissed, or a value of zero or smaller if it
		is to remain until user action is taken to dismiss
		it.
  :resident     When set the notification will not be automatically
		dismissed when it or an action is selected.
  :on-action	Function to call when an action is invoked.
		The notification id and the key of the action are
		provided as arguments to the function.
  :on-close	Function to call if the notification is dismissed,
		with the notification id and the symbol `undefined'
		for arguments.

The notification group and timeout are ignored on Android 7.1 and
earlier versions of Android.  On more recent versions, the group
identifies a category that will be displayed in the system Settings
menu, and the urgency provided always extends to affect all
notifications displayed within that category, though it may be ignored
if higher than any previously-specified urgency or if the user have
already configured a different urgency for this category from Settings.
If the group is not provided, it defaults to the string "Desktop
Notifications" with the urgency suffixed.

Each caller should strive to provide one unchanging combination of
notification group and urgency for each kind of notification it sends,
inasmuch as the system may, subject to user configuration, disregard
the urgency specified within a notification, should it not be the
first notification sent to its notification group.

The provided icon should be the name of a "drawable resource" present
within the "android.R.drawable" class designating an icon with a
transparent background.  Should no icon be provided (or the icon is
absent from this system), it defaults to "ic_dialog_alert".

Actions specified with :actions cannot be displayed on Android 4.0 and
earlier versions of the system.

When the system is running Android 13 or later, notifications sent
will be silently disregarded unless permission to display
notifications is expressly granted from the "App Info" settings panel
corresponding to Emacs.

A title and body must be supplied.  Value is an integer (fixnum or
bignum) uniquely designating the notification displayed, which may
subsequently be specified as the `:replaces-id' of another call to
this function.

usage: (android-notifications-notify &rest ARGS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object title, body, replaces_id, group, urgency, timeout, resident;
  Lisp_Object icon;
  Lisp_Object key, value, actions, action_cb, close_cb;
  ptrdiff_t i;
  AUTO_STRING (default_icon, "ic_dialog_alert");

  if (!android_init_gui)
    error ("No Android display connection");

  /* Clear each variable above.  */
  title = body = replaces_id = group = icon = urgency = actions = Qnil;
  timeout = resident = action_cb = close_cb = Qnil;

  /* If NARGS is odd, error.  */

  if (nargs & 1)
    error ("Odd number of arguments in call to `android-notifications-notify'");

  /* Next, iterate through ARGS, searching for arguments.  */

  for (i = 0; i < nargs; i += 2)
    {
      key = args[i];
      value = args[i + 1];

      if (EQ (key, QCtitle))
	title = value;
      else if (EQ (key, QCbody))
	body = value;
      else if (EQ (key, QCreplaces_id))
	replaces_id = value;
      else if (EQ (key, QCgroup))
	group = value;
      else if (EQ (key, QCurgency))
	urgency = value;
      else if (EQ (key, QCicon))
	icon = value;
      else if (EQ (key, QCactions))
	actions = value;
      else if (EQ (key, QCtimeout))
	timeout = value;
      else if (EQ (key, QCresident))
	resident = value;
      else if (EQ (key, QCon_action))
	action_cb = value;
      else if (EQ (key, QCon_close))
	close_cb = value;
    }

  /* Demand at least TITLE and BODY be present.  */

  if (NILP (title) || NILP (body))
    error ("Title or body not provided");

  /* Now check the type and possibly expand each non-nil argument.  */

  CHECK_STRING (title);
  CHECK_STRING (body);

  if (NILP (urgency))
    urgency = Qlow;

  if (NILP (group))
    {
      AUTO_STRING (format, "Desktop Notifications (%s importance)");
      group = CALLN (Fformat, format, urgency);
    }

  if (NILP (icon))
    icon = default_icon;
  else
    CHECK_STRING (icon);

  return make_int (android_notifications_notify_1 (title, body, replaces_id,
						   group, icon, urgency,
						   actions, timeout, resident,
						   action_cb, close_cb));
}

/* Run callbacks in response to a notification being deleted.
   Save any input generated for the keyboard within *IE.
   EVENT should be the notification deletion event.  */

void
android_notification_deleted (struct android_notification_event *event,
			      struct input_event *ie)
{
  Lisp_Object item, tag;
  intmax_t id;

  tag  = build_string (event->tag);
  item = Fgethash (tag, notification_table, Qnil);

  if (!NILP (item))
    Fremhash (tag, notification_table);

  if (CONSP (item) && FUNCTIONP (XCAR (XCDR (item)))
      && sscanf (event->tag, "%*d.%*ld.%jd", &id) > 0)
    {
      ie->kind = NOTIFICATION_EVENT;
      ie->arg  = list3 (XCAR (XCDR (item)), make_int (id),
			Qundefined);
    }
}

/* Run callbacks in response to one of a notification's actions being
   invoked, saving any input generated for the keyboard within *IE.
   EVENT should be the notification deletion event, and ACTION the
   action key.  */

void
android_notification_action (struct android_notification_event *event,
			     struct input_event *ie, Lisp_Object action)
{
  Lisp_Object item, tag;
  intmax_t id;
  jstring tag_object;
  jmethodID method;

  tag  = build_string (event->tag);
  item = Fgethash (tag, notification_table, Qnil);

  if (CONSP (item) && FUNCTIONP (XCAR (item))
      && sscanf (event->tag, "%*d.%*ld.%jd", &id) > 0)
    {
      ie->kind = NOTIFICATION_EVENT;
      ie->arg  = list3 (XCAR (item), make_int (id), action);
    }

  /* Test whether ITEM is resident.  Non-resident notifications must be
     removed when activated.  */

  if (!CONSP (item) || NILP (XCAR (XCDR (XCDR (item)))))
    {
      method = service_class.cancel_notification;
      tag_object
	= (*android_java_env)->NewStringUTF (android_java_env,
					     event->tag);
      android_exception_check ();

      (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						     emacs_service,
						     service_class.class,
						     method, tag_object);
      android_exception_check_1 (tag_object);
      ANDROID_DELETE_LOCAL_REF (tag_object);

      /* Remove the notification from the callback table.  */
      if (!NILP (item))
	Fremhash (tag, notification_table);
    }
}



void
init_androidselect (void)
{
  jobject tem;
  jmethodID make_clipboard;

  if (!android_init_gui)
    return;

  android_init_emacs_clipboard ();
  android_init_asset_file_descriptor ();
  android_init_emacs_desktop_notification ();

  make_clipboard = clipboard_class.make_clipboard;
  tem
    = (*android_java_env)->CallStaticObjectMethod (android_java_env,
						   clipboard_class.class,
						   make_clipboard);
  if (!tem)
    emacs_abort ();

  clipboard = (*android_java_env)->NewGlobalRef (android_java_env, tem);

  if (!clipboard)
    emacs_abort ();

  ANDROID_DELETE_LOCAL_REF (tem);
}

void
syms_of_androidselect (void)
{
  DEFSYM (QCtitle, ":title");
  DEFSYM (QCbody, ":body");
  DEFSYM (QCreplaces_id, ":replaces-id");
  DEFSYM (QCgroup, ":group");
  DEFSYM (QCurgency, ":urgency");
  DEFSYM (QCicon, ":icon");
  DEFSYM (QCactions, ":actions");
  DEFSYM (QCtimeout, ":timeout");
  DEFSYM (QCresident, ":resident");
  DEFSYM (QCon_action, ":on-action");
  DEFSYM (QCon_close, ":on-close");

  DEFSYM (Qlow, "low");
  DEFSYM (Qnormal, "normal");
  DEFSYM (Qcritical, "critical");

  defsubr (&Sandroid_clipboard_owner_p);
  defsubr (&Sandroid_set_clipboard);
  defsubr (&Sandroid_get_clipboard);
  defsubr (&Sandroid_clipboard_exists_p);
  defsubr (&Sandroid_browse_url_internal);
  defsubr (&Sandroid_get_clipboard_targets);
  defsubr (&Sandroid_get_clipboard_data);

  defsubr (&Sandroid_notifications_notify);

  notification_table = CALLN (Fmake_hash_table, QCtest, Qequal);
  staticpro (&notification_table);
}
