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

#include "lisp.h"
#include "blockinput.h"
#include "coding.h"
#include "android.h"
#include "androidterm.h"

/* Selection support on Android is confined to copying and pasting of
   plain text from the clipboard.  There is no primary selection.

   While newer versions of Android are supposed to have the necessary
   interfaces for transferring other kinds of selection data, doing so
   is too complicated, and involves registering ``content providers''
   and all kinds of other stuff.  */



/* Structure describing the EmacsClipboard class.  */

struct android_emacs_clipboard
{
  jclass class;
  jmethodID set_clipboard;
  jmethodID owns_clipboard;
  jmethodID clipboard_exists;
  jmethodID get_clipboard;
  jmethodID make_clipboard;
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
  return code_convert_string_norecord (string, Qutf_8, Qnil);
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
       Sandroid_browse_url, 1, 1, 0,
       doc: /* Start the system web browser.
Then, point the web browser to URL, which should be a URL-encoded
URL with a scheme specified.  Signal an error upon failure.  */)
  (Lisp_Object url)
{
  Lisp_Object value;

  if (!android_init_gui)
    error ("No Android display connection!");

  CHECK_STRING (url);
  value = android_browse_url (url);

  /* Signal an error upon failure.  */
  if (!NILP (value))
    signal_error ("Error browsing URL", value);

  return Qnil;
}



void
init_androidselect (void)
{
  jobject tem;
  jmethodID make_clipboard;

  if (!android_init_gui)
    return;

  android_init_emacs_clipboard ();

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
  defsubr (&Sandroid_clipboard_owner_p);
  defsubr (&Sandroid_set_clipboard);
  defsubr (&Sandroid_get_clipboard);
  defsubr (&Sandroid_clipboard_exists_p);
  defsubr (&Sandroid_browse_url);
}
