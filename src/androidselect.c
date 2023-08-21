/* Communication module for Android terminals.

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
#include <assert.h>
#include <minmax.h>
#include <unistd.h>

#include <boot-time.h>
#include <sys/types.h>

#include "lisp.h"
#include "blockinput.h"
#include "coding.h"
#include "android.h"
#include "androidterm.h"

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
  assert (clipboard_class.c_name);

  FIND_METHOD (set_clipboard, "setClipboard", "([B)V");
  FIND_METHOD (owns_clipboard, "ownsClipboard", "()I");
  FIND_METHOD (clipboard_exists, "clipboardExists", "()Z");
  FIND_METHOD (get_clipboard, "getClipboard", "()[B");
  FIND_METHOD (get_clipboard_targets, "getClipboardTargets",
	       "()[[B");
  FIND_METHOD (get_clipboard_data, "getClipboardData",
	       "([B)[J");

  clipboard_class.make_clipboard
    = (*android_java_env)->GetStaticMethodID (android_java_env,
					      clipboard_class.class,
					      "makeClipboard",
					      "()Lorg/gnu/emacs/"
					      "EmacsClipboard;");
  assert (clipboard_class.make_clipboard);

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
  jarray bytes;

  if (!android_init_gui)
    error ("Accessing clipboard without display connection");

  CHECK_STRING (string);
  string = ENCODE_UTF_8 (string);

  bytes = (*android_java_env)->NewByteArray (android_java_env,
					     SBYTES (string));
  android_exception_check ();

  (*android_java_env)->SetByteArrayRegion (android_java_env, bytes,
					   0, SBYTES (string),
					   (jbyte *) SDATA (string));
  (*android_java_env)->CallVoidMethod (android_java_env,
				       clipboard,
				       clipboard_class.set_clipboard,
				       bytes);
  android_exception_check_1 (bytes);

  ANDROID_DELETE_LOCAL_REF (bytes);
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
  jarray bytes;
  jmethodID method;
  size_t length;
  jbyte *data;

  if (!android_init_gui)
    error ("No Android display connection!");

  method = clipboard_class.get_clipboard;
  bytes
    = (*android_java_env)->CallObjectMethod (android_java_env,
					     clipboard,
					     method);
  android_exception_check ();

  if (!bytes)
    return Qnil;

  length = (*android_java_env)->GetArrayLength (android_java_env,
						bytes);
  data = (*android_java_env)->GetByteArrayElements (android_java_env,
						    bytes, NULL);
  android_exception_check_nonnull (data, bytes);

  string = make_unibyte_string ((char *) data, length);

  (*android_java_env)->ReleaseByteArrayElements (android_java_env,
						 bytes, data,
						 JNI_ABORT);
  ANDROID_DELETE_LOCAL_REF (bytes);

  /* Now decode the resulting string.  */
  return code_convert_string_norecord (string, Qutf_8, false);
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

DEFUN ("android-browse-url", Fandroid_browse_url,
       Sandroid_browse_url, 1, 2, 0,
       doc: /* Open URL in an external application.  URL should be a
URL-encoded URL with a scheme specified unless SEND is non-nil.
Signal an error upon failure.

If SEND is nil, start a program that is able to display the URL, such
as a web browser.  Otherwise, try to share URL using programs such as
email clients.  */)
  (Lisp_Object url, Lisp_Object send)
{
  Lisp_Object value;

  if (!android_init_gui)
    error ("No Android display connection!");

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
  jarray bytes_array;
  jbyteArray bytes;
  jmethodID method;
  size_t length, length1, i;
  jbyte *data;
  Lisp_Object targets, tem;

  if (!android_init_gui)
    error ("No Android display connection!");

  targets = Qnil;
  block_input ();
  method = clipboard_class.get_clipboard_targets;
  bytes_array = (*android_java_env)->CallObjectMethod (android_java_env,
						       clipboard, method);
  android_exception_check ();

  if (!bytes_array)
    goto fail;

  length = (*android_java_env)->GetArrayLength (android_java_env,
						bytes_array);
  for (i = 0; i < length; ++i)
    {
      /* Retireve the MIME type.  */
      bytes
	= (*android_java_env)->GetObjectArrayElement (android_java_env,
						      bytes_array, i);
      android_exception_check_nonnull (bytes, bytes_array);

      /* Cons it onto the list of targets.  */
      length1 = (*android_java_env)->GetArrayLength (android_java_env,
						     bytes);
      data = (*android_java_env)->GetByteArrayElements (android_java_env,
							bytes, NULL);
      android_exception_check_nonnull_1 (data, bytes, bytes_array);

      /* Decode the string.  */
      tem = make_unibyte_string ((char *) data, length1);
      tem = code_convert_string_norecord (tem, Qutf_8, false);
      targets = Fcons (tem, targets);

      /* Delete the retrieved data.  */
      (*android_java_env)->ReleaseByteArrayElements (android_java_env,
						     bytes, data,
						     JNI_ABORT);
      ANDROID_DELETE_LOCAL_REF (bytes);
    }
  unblock_input ();

  ANDROID_DELETE_LOCAL_REF (bytes_array);
  return Fnreverse (targets);

 fail:
  unblock_input ();
  return Qnil;
}

/* Free the memory inside PTR, a pointer to a char pointer.  */

static void
android_xfree_inside (void *ptr)
{
  xfree (*(char **) ptr);
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
  jlongArray array;
  jbyteArray bytes;
  jmethodID method;
  int fd;
  ptrdiff_t rc;
  jlong offset, length, *longs;
  specpdl_ref ref;
  char *buffer, *start;

  if (!android_init_gui)
    error ("No Android display connection!");

  /* Encode the string as UTF-8.  */
  CHECK_STRING (type);
  type = ENCODE_UTF_8 (type);

  /* Then give it to the selection code.  */
  block_input ();
  bytes = (*android_java_env)->NewByteArray (android_java_env,
					     SBYTES (type));
  (*android_java_env)->SetByteArrayRegion (android_java_env, bytes,
					   0, SBYTES (type),
					   (jbyte *) SDATA (type));
  android_exception_check ();

  method = clipboard_class.get_clipboard_data;
  array = (*android_java_env)->CallObjectMethod (android_java_env,
						 clipboard, method,
						 bytes);
  android_exception_check_1 (bytes);
  ANDROID_DELETE_LOCAL_REF (bytes);

  if (!array)
    goto fail;

  longs = (*android_java_env)->GetLongArrayElements (android_java_env,
						     array, NULL);
  android_exception_check_nonnull (longs, array);

  /* longs[0] is the file descriptor.
     longs[1] is an offset to apply to the file.
     longs[2] is either -1, or the number of bytes to read from the
     file.  */
  fd = longs[0];
  offset = longs[1];
  length = longs[2];

  (*android_java_env)->ReleaseLongArrayElements (android_java_env,
						 array, longs,
						 JNI_ABORT);
  ANDROID_DELETE_LOCAL_REF (array);
  unblock_input ();

  /* Now begin reading from longs[0].  */
  ref = SPECPDL_INDEX ();
  record_unwind_protect_int (close_file_unwind, fd);

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

      if (!INT_ADD_OK (rc, length, &length)
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
  assert (notification_class.c_name);

  FIND_METHOD (init, "<init>", "(Ljava/lang/String;"
	       "Ljava/lang/String;Ljava/lang/String;"
	       "Ljava/lang/String;II)V");
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
   REPLACES_ID, GROUP, ICON, and URGENCY.  Return an identifier for
   the resulting notification.  */

static intmax_t
android_notifications_notify_1 (Lisp_Object title, Lisp_Object body,
				Lisp_Object replaces_id,
				Lisp_Object group, Lisp_Object icon,
				Lisp_Object urgency)
{
  static intmax_t counter;
  intmax_t id;
  jstring title1, body1, group1, identifier1;
  jint type, icon1;
  jobject notification;
  char identifier[INT_STRLEN_BOUND (int)
		  + INT_STRLEN_BOUND (long int)
		  + INT_STRLEN_BOUND (intmax_t)
		  + sizeof "..."];
  struct timespec boot_time;

  if (EQ (urgency, Qlow))
    type = 2; /* IMPORTANCE_LOW */
  else if (EQ (urgency, Qnormal))
    type = 3; /* IMPORTANCE_DEFAULT */
  else if (EQ (urgency, Qcritical))
    type = 4; /* IMPORTANCE_HIGH */
  else
    signal_error ("Invalid notification importance given", urgency);

  if (NILP (replaces_id))
    {
      /* Generate a new identifier.  */
      INT_ADD_WRAPV (counter, 1, &counter);
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
  title1 = android_build_string (title);
  body1  = android_build_string (body);
  group1 = android_build_string (group);
  identifier1 = android_build_jstring (identifier);

  /* Create the notification.  */
  notification
    = (*android_java_env)->NewObject (android_java_env,
				      notification_class.class,
				      notification_class.init,
				      title1, body1, group1,
				      identifier1, icon1, type);
  android_exception_check_4 (title1, body1, group1, identifier1);

  /* Delete unused local references.  */
  ANDROID_DELETE_LOCAL_REF (title1);
  ANDROID_DELETE_LOCAL_REF (body1);
  ANDROID_DELETE_LOCAL_REF (group1);
  ANDROID_DELETE_LOCAL_REF (identifier1);

  /* Display the notification.  */
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 notification,
						 notification_class.class,
						 notification_class.display);
  android_exception_check_1 (notification);
  ANDROID_DELETE_LOCAL_REF (notification);

  /* Return the ID.  */
  return id;
}

DEFUN ("android-notifications-notify", Fandroid_notifications_notify,
       Sandroid_notifications_notify, 0, MANY, 0, doc:
       /* Display a desktop notification.
ARGS must contain keywords followed by values.  Each of the following
keywords is understood:

  :title        The notification title.
  :body         The notification body.
  :replaces-id  The ID of a previous notification to supersede.
  :group        The notification group, or nil.
  :urgency      One of the symbols `low', `normal' or `critical',
                defining the importance of the notification group.
  :icon         The name of a drawable resource to display as the
                notification's icon.

The notification group and urgency are ignored on Android 7.1 and
earlier versions of Android.  Outside such older systems, it
identifies a category that will be displayed in the system Settings
menu.  The urgency provided always extends to affect all notifications
displayed within that category.  If the group is not provided, it
defaults to the string "Desktop Notifications".

The provided icon should be the name of a "drawable resource" present
within the "android.R.drawable" class designating an icon with a
transparent background.  If no icon is provided (or the icon is absent
from this system), it defaults to "ic_dialog_alert".

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
  Lisp_Object title, body, replaces_id, group, urgency;
  Lisp_Object icon;
  Lisp_Object key, value;
  ptrdiff_t i;

  if (!android_init_gui)
    error ("No Android display connection!");

  /* Clear each variable above.  */
  title = body = replaces_id = group = icon = urgency = Qnil;

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
    group = build_string ("Desktop Notifications");

  if (NILP (icon))
    icon = build_string ("ic_dialog_alert");
  else
    CHECK_STRING (icon);

  return make_int (android_notifications_notify_1 (title, body, replaces_id,
						   group, icon, urgency));
}



void
init_androidselect (void)
{
  jobject tem;
  jmethodID make_clipboard;

  if (!android_init_gui)
    return;

  android_init_emacs_clipboard ();
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

  DEFSYM (Qlow, "low");
  DEFSYM (Qnormal, "normal");
  DEFSYM (Qcritical, "critical");

  defsubr (&Sandroid_clipboard_owner_p);
  defsubr (&Sandroid_set_clipboard);
  defsubr (&Sandroid_get_clipboard);
  defsubr (&Sandroid_clipboard_exists_p);
  defsubr (&Sandroid_browse_url);
  defsubr (&Sandroid_get_clipboard_targets);
  defsubr (&Sandroid_get_clipboard_data);

  defsubr (&Sandroid_notifications_notify);
}
