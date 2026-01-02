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

#include "lisp.h"
#include "androidterm.h"
#include "android.h"
#include "blockinput.h"
#include "keyboard.h"
#include "menu.h"

#ifndef ANDROID_STUBIFY

#include <android/log.h>

/* Flag indicating whether or not a popup menu has been posted and not
   yet popped down.  */

static int popup_activated_flag;

/* Serial number used to identify which context menu events are
   associated with the context menu currently being displayed.  */

unsigned int current_menu_serial;

int
popup_activated (void)
{
  return popup_activated_flag;
}



/* Toolkit menu implementation.  */

/* Structure describing the EmacsContextMenu class.  */

struct android_emacs_context_menu
{
  jclass class;
  jmethodID create_context_menu;
  jmethodID add_item;
  jmethodID add_submenu;
  jmethodID add_pane;
  jmethodID parent;
  jmethodID display;
  jmethodID dismiss;
};

/* Identifiers associated with the EmacsContextMenu class.  */
static struct android_emacs_context_menu menu_class;

static void
android_init_emacs_context_menu (void)
{
  jclass old;

  menu_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsContextMenu");
  eassert (menu_class.class);

  old = menu_class.class;
  menu_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!menu_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  menu_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					menu_class.class,	\
					name, signature);	\
  eassert (menu_class.c_name);

#define FIND_METHOD_STATIC(c_name, name, signature)		\
  menu_class.c_name						\
    = (*android_java_env)->GetStaticMethodID (android_java_env,	\
					      menu_class.class,	\
					      name, signature);	\
  eassert (menu_class.c_name);

  FIND_METHOD_STATIC (create_context_menu, "createContextMenu",
		      "(Ljava/lang/String;)"
		      "Lorg/gnu/emacs/EmacsContextMenu;");

  FIND_METHOD (add_item, "addItem", "(ILjava/lang/String;ZZZ"
	       "Ljava/lang/String;Z)V");
  FIND_METHOD (add_submenu, "addSubmenu", "(Ljava/lang/String;"
	       "Ljava/lang/String;)"
	       "Lorg/gnu/emacs/EmacsContextMenu;");
  FIND_METHOD (add_pane, "addPane", "(Ljava/lang/String;)V");
  FIND_METHOD (parent, "parent", "()Lorg/gnu/emacs/EmacsContextMenu;");
  FIND_METHOD (display, "display", "(Lorg/gnu/emacs/EmacsWindow;III)Z");
  FIND_METHOD (dismiss, "dismiss", "(Lorg/gnu/emacs/EmacsWindow;)V");

#undef FIND_METHOD
#undef FIND_METHOD_STATIC
}

static void
android_unwind_local_frame (void)
{
  (*android_java_env)->PopLocalFrame (android_java_env, NULL);
}

/* Push a local reference frame to the JVM stack and record it on the
   specpdl.  Release local references created within that frame when
   the specpdl is unwound past where it is after returning.  */

static void
android_push_local_frame (void)
{
  int rc;

  rc = (*android_java_env)->PushLocalFrame (android_java_env, 30);

  /* This means the JVM ran out of memory.  */
  if (rc < 1)
    android_exception_check ();

  record_unwind_protect_void (android_unwind_local_frame);
}

/* Data for android_dismiss_menu.  */

struct android_dismiss_menu_data
{
  /* The menu object.  */
  jobject menu;

  /* The window object.  */
  jobject window;
};

/* Cancel the context menu passed in POINTER.  Also, clear
   popup_activated_flag.  */

static void
android_dismiss_menu (void *pointer)
{
  struct android_dismiss_menu_data *data;

  data = pointer;
  (*android_java_env)->CallNonvirtualVoidMethod (android_java_env,
						 data->menu,
						 menu_class.class,
						 menu_class.dismiss,
						 data->window);
  popup_activated_flag = 0;
}

/* Recursively process events until a ANDROID_CONTEXT_MENU event
   arrives.  Then, return the item ID specified in the event in
   *ID.  */

static void
android_process_events_for_menu (int *id)
{
  int blocked;

  /* Set menu_event_id to -1; handle_one_android_event will set it to
     the event ID upon receiving a context menu event.  This can cause
     a non-local exit.  */
  x_display_list->menu_event_id = -1;

  /* Unblock input completely.  */
  blocked = interrupt_input_blocked;
  totally_unblock_input ();

  /* Now wait for the menu event ID to change.  */
  while (x_display_list->menu_event_id == -1)
    {
      /* Wait for events to become available.  */
      android_wait_event ();

      /* Process pending signals.  */
      process_pending_signals ();

      /* Maybe quit.  This is important because the framework (on
	 Android 4.0.3) can sometimes fail to deliver context menu
	 closed events if a submenu was opened, and the user still
	 needs to be able to quit.  */
      maybe_quit ();
    }

  /* Restore the input block.  */
  interrupt_input_blocked = blocked;

  /* Return the ID.  */
  *id = x_display_list->menu_event_id;
}

/* Structure describing a ``subprefix'' in the menu.  */

struct android_menu_subprefix
{
  /* The subprefix above.  */
  struct android_menu_subprefix *last;

  /* The subprefix itself.  */
  Lisp_Object subprefix;
};

/* Free the subprefixes starting from *DATA.  */

static void
android_free_subprefixes (void *data)
{
  struct android_menu_subprefix **head, *subprefix;

  head = data;

  while (*head)
    {
      subprefix = *head;
      *head = subprefix->last;

      xfree (subprefix);
    }
}

Lisp_Object
android_menu_show (struct frame *f, int x, int y, int menuflags,
		   Lisp_Object title, const char **error_name)
{
  jobject context_menu, current_context_menu;
  jobject title_string, help_string, temp;
  size_t i;
  Lisp_Object pane_name, prefix;
  specpdl_ref count, count1;
  Lisp_Object item_name, enable, def, tem, entry, type, selected;
  Lisp_Object help;
  jmethodID method;
  jobject store;
  bool rc;
  jobject window;
  int id, item_id, submenu_depth;
  struct android_dismiss_menu_data data;
  struct android_menu_subprefix *subprefix, *temp_subprefix;
  struct android_menu_subprefix *subprefix_1;
  bool checkmark;
  unsigned int serial;
  JNIEnv *env;

  count = SPECPDL_INDEX ();
  serial = ++current_menu_serial;
  env = android_java_env;

  block_input ();

  /* Push the first local frame.  */
  android_push_local_frame ();

  /* Set title_string to a Java string containing TITLE if non-nil.
     If the menu consists of more than one pane, replace the title
     with the pane header item so that the menu looks consistent.  */

  title_string = NULL;
  if (STRINGP (title) && menu_items_n_panes < 2)
    title_string = android_build_string (title, NULL);

  /* Push the first local frame for the context menu.  */
  method = menu_class.create_context_menu;
  current_context_menu = context_menu
    = (*android_java_env)->CallStaticObjectMethod (android_java_env,
						   menu_class.class,
						   method,
						   title_string);

  /* Delete the unused title reference.  */

  if (title_string)
    ANDROID_DELETE_LOCAL_REF (title_string);

  /* Push the second local frame for temporaries.  */
  count1 = SPECPDL_INDEX ();
  android_push_local_frame ();

  /* Iterate over the menu.  */
  i = 0, submenu_depth = 0;

  while (i < menu_items_used)
    {
      if (NILP (AREF (menu_items, i)))
	{
	  /* This is the start of a new submenu.  However, it can be
	     ignored here.  */
	  i += 1;
	  submenu_depth += 1;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  /* This is the end of a submenu.  Go back to the previous
	     context menu.  */
	  store = current_context_menu;
	  current_context_menu
	    = (*env)->CallNonvirtualObjectMethod (env,
						  current_context_menu,
						  menu_class.class,
						  menu_class.parent);
	  android_exception_check ();

	  if (store != context_menu)
	    ANDROID_DELETE_LOCAL_REF (store);
	  i += 1;
	  submenu_depth -= 1;

	  if (!current_context_menu || submenu_depth < 0)
	    {
	      __android_log_print (ANDROID_LOG_FATAL, __func__,
				   "unbalanced submenu pop in menu_items");
	      emacs_abort ();
	    }
	}
      else if (EQ (AREF (menu_items, i), Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  /* If the menu contains a single pane, then the pane is
	     actually TITLE.  Don't duplicate the text within the
	     context menu title.  */

	  if (menu_items_n_panes < 2)
	    goto next_item;

	  /* This is a new pane.  Switch back to the topmost context
	     menu.  */
	  if (current_context_menu != context_menu)
	    ANDROID_DELETE_LOCAL_REF (current_context_menu);
	  current_context_menu = context_menu;

	  /* Now figure out the title of this pane.  */
	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

	  /* PANE_NAME may be nil, in which case it must be set to an
	     empty string.  */

	  if (NILP (pane_name))
	    pane_name = empty_unibyte_string;

	  /* Remove the leading prefix character if need be.  */

	  if ((menuflags & MENU_KEYMAPS) && !NILP (prefix)
	      && SCHARS (prefix))
	    pane_name = Fsubstring (pane_name, make_fixnum (1), Qnil);

	  /* Add the pane.  */
	  temp = android_build_string (pane_name, NULL);
	  android_exception_check ();

	  (*env)->CallNonvirtualVoidMethod (env, current_context_menu,
					    menu_class.class,
					    menu_class.add_pane, temp);
	  android_exception_check ();
	  ANDROID_DELETE_LOCAL_REF (temp);

	next_item:
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

	  /* This is an actual menu item (or submenu).  Add it to the
	     menu.  */

	  if (i + MENU_ITEMS_ITEM_LENGTH < menu_items_used
	      && NILP (AREF (menu_items, i + MENU_ITEMS_ITEM_LENGTH)))
	    {
	      /* This is a submenu.  Add it.  */
	      title_string = (!NILP (item_name)
			      ? android_build_string (item_name, NULL)
			      : NULL);
	      help_string = NULL;

	      /* Menu items can have tool tips on Android 26 and
		 later.  In this case, set it to the help string.  */

	      if (android_get_current_api_level () >= 26
		  && STRINGP (help))
		help_string = android_build_string (help, NULL);

	      store = current_context_menu;
	      current_context_menu
		= (*env)->CallNonvirtualObjectMethod (env,
						      current_context_menu,
						      menu_class.class,
						      menu_class.add_submenu,
						      title_string,
						      help_string);
	      android_exception_check ();

	      if (store != context_menu)
		ANDROID_DELETE_LOCAL_REF (store);

	      if (title_string)
		ANDROID_DELETE_LOCAL_REF (title_string);

	      if (help_string)
		ANDROID_DELETE_LOCAL_REF (help_string);
	    }
	  else if (NILP (def) && menu_separator_name_p (SSDATA (item_name)))
	    /* Ignore this separator item.  */
	    ;
	  else
	    {
	      /* Compute the item ID.  This is the index of value.
		 Make sure it doesn't overflow.  */

	      if (ckd_add (&item_id, i + MENU_ITEMS_ITEM_VALUE, 0))
		memory_full (i + MENU_ITEMS_ITEM_VALUE * sizeof (Lisp_Object));

	      /* Add this menu item with the appropriate state.  */

	      title_string = (!NILP (item_name)
			      ? android_build_string (item_name, NULL)
			      : NULL);
	      help_string = NULL;

	      /* Menu items can have tool tips on Android 26 and
		 later.  In this case, set it to the help string.  */

	      if (android_get_current_api_level () >= 26
		  && STRINGP (help))
		help_string = android_build_string (help, NULL);

	      /* Determine whether or not to display a check box.  */

	      checkmark = (EQ (type, QCtoggle)
			   || EQ (type, QCradio));

	      (*env)->CallNonvirtualVoidMethod (env,
						current_context_menu,
						menu_class.class,
						menu_class.add_item,
						(jint) item_id,
						title_string,
						(jboolean) !NILP (enable),
						(jboolean) checkmark,
						(jboolean) !NILP (selected),
						help_string,
						(jboolean) (EQ (type,
								QCradio)));
	      android_exception_check ();

	      if (title_string)
		ANDROID_DELETE_LOCAL_REF (title_string);

	      if (help_string)
		ANDROID_DELETE_LOCAL_REF (help_string);
	    }

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* The menu has now been built.  Pop the second local frame.  */
  unbind_to (count1, Qnil);

  /* Now, display the context menu.  */
  window = android_resolve_handle (FRAME_ANDROID_WINDOW (f));
  rc = (*env)->CallNonvirtualBooleanMethod (env, context_menu,
					    menu_class.class,
					    menu_class.display,
					    window, (jint) x,
					    (jint) y,
					    (jint) serial);
  android_exception_check ();

  if (!rc)
    /* This means displaying the menu failed.  */
    goto finish;

  /* Make sure the context menu is always dismissed.  */
  data.menu = context_menu;
  data.window = window;
  record_unwind_protect_ptr (android_dismiss_menu, &data);

  /* Next, process events waiting for something to be selected.  */
  popup_activated_flag = 1;
  android_process_events_for_menu (&id);

  if (!id)
    /* This means no menu item was selected.  */
    goto finish;

  /* This means the id is invalid.  */
  if (id >= ASIZE (menu_items))
    goto finish;

  /* Now return the menu item at that location.  */
  tem = Qnil;
  subprefix = NULL;
  record_unwind_protect_ptr (android_free_subprefixes, &subprefix);

  /* Find the selected item, and its pane, to return
     the proper value.  */

  prefix = entry = Qnil;
  i = 0;
  while (i < menu_items_used)
    {
      if (NILP (AREF (menu_items, i)))
	{
	  temp_subprefix = xmalloc (sizeof *temp_subprefix);
	  temp_subprefix->last = subprefix;
	  subprefix = temp_subprefix;
	  subprefix->subprefix = prefix;

	  prefix = entry;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prefix = subprefix->subprefix;
	  temp_subprefix = subprefix->last;
	  xfree (subprefix);
	  subprefix = temp_subprefix;

	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  prefix
	    = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else
	{
	  entry = AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);

	  if (i + MENU_ITEMS_ITEM_VALUE == id)
	    {
	      if (menuflags & MENU_KEYMAPS)
		{
		  entry = list1 (entry);

		  if (!NILP (prefix))
		    entry = Fcons (prefix, entry);

		  for (subprefix_1 = subprefix; subprefix_1;
		       subprefix_1 = subprefix_1->last)
		    if (!NILP (subprefix_1->subprefix))
		      entry = Fcons (subprefix_1->subprefix, entry);
		}

	      tem = entry;
	    }
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  unblock_input ();
  return unbind_to (count, tem);

 finish:
  unblock_input ();
  return unbind_to (count, Qnil);
}



/* Toolkit dialog implementation.  */

/* Structure describing the EmacsDialog class.  */

struct android_emacs_dialog
{
  jclass class;
  jmethodID create_dialog;
  jmethodID add_button;
  jmethodID display;
};

/* Identifiers associated with the EmacsDialog class.  */
static struct android_emacs_dialog dialog_class;

static void
android_init_emacs_dialog (void)
{
  jclass old;

  dialog_class.class
    = (*android_java_env)->FindClass (android_java_env,
				      "org/gnu/emacs/EmacsDialog");
  eassert (dialog_class.class);

  old = dialog_class.class;
  dialog_class.class
    = (jclass) (*android_java_env)->NewGlobalRef (android_java_env,
						  (jobject) old);
  ANDROID_DELETE_LOCAL_REF (old);

  if (!dialog_class.class)
    emacs_abort ();

#define FIND_METHOD(c_name, name, signature)			\
  dialog_class.c_name						\
    = (*android_java_env)->GetMethodID (android_java_env,	\
					dialog_class.class,	\
					name, signature);	\
  eassert (dialog_class.c_name);

#define FIND_METHOD_STATIC(c_name, name, signature)			\
  dialog_class.c_name							\
    = (*android_java_env)->GetStaticMethodID (android_java_env,		\
					      dialog_class.class,	\
					      name, signature);		\

  FIND_METHOD_STATIC (create_dialog, "createDialog", "(Ljava/lang/String;"
		      "Ljava/lang/String;I)Lorg/gnu/emacs/EmacsDialog;");
  FIND_METHOD (add_button, "addButton", "(Ljava/lang/String;IZ)V");
  FIND_METHOD (display, "display", "()Z");

#undef FIND_METHOD
#undef FIND_METHOD_STATIC
}

static Lisp_Object
android_dialog_show (struct frame *f, Lisp_Object title,
		     Lisp_Object header, const char **error_name)
{
  specpdl_ref count;
  jobject dialog, java_header, java_title, temp;
  size_t i;
  Lisp_Object item_name, enable, entry;
  bool rc;
  int id;
  jmethodID method;
  unsigned int serial;
  JNIEnv *env;

  /* Generate a unique ID for events from this dialog box.  */
  serial = ++current_menu_serial;

  if (menu_items_n_panes > 1)
    {
      *error_name = "Multiple panes in dialog box";
      return Qnil;
    }

  /* Do the initial setup.  */
  count = SPECPDL_INDEX ();
  *error_name = NULL;

  android_push_local_frame ();

  /* Figure out what header to use.  */
  java_header = (!NILP (header)
		 ? android_build_jstring ("Information")
		 : android_build_jstring ("Question"));

  /* And the title.  */
  java_title = android_build_string (title, NULL);

  /* Now create the dialog.  */
  method = dialog_class.create_dialog;
  dialog = (*android_java_env)->CallStaticObjectMethod (android_java_env,
							dialog_class.class,
							method, java_header,
						        java_title,
							(jint) serial);
  android_exception_check ();

  /* Delete now unused local references.  */
  if (java_header)
    ANDROID_DELETE_LOCAL_REF (java_header);
  ANDROID_DELETE_LOCAL_REF (java_title);

  /* Save the JNI environment pointer prior to constructing the
     dialog, as typing (*android_java_env)->... gives rise to very
     long lines.  */
  env = android_java_env;

  /* Create the buttons.  */
  i = MENU_ITEMS_PANE_LENGTH;
  while (i < menu_items_used)
    {
      item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
      enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);

      /* Verify that there is no submenu here.  */

      if (NILP (item_name))
	{
	  *error_name = "Submenu in dialog items";
	  return unbind_to (count, Qnil);
	}

      /* Skip past boundaries between buttons on different sides.  The
	 Android toolkit is too silly to understand this
	 distinction.  */

      if (EQ (item_name, Qquote))
	++i;
      else
	{
	  /* Make sure i is within bounds.  */
	  if (i > TYPE_MAXIMUM (jint))
	    {
	      *error_name = "Dialog box too big";
	      return unbind_to (count, Qnil);
	    }

	  /* Add the button.  */
	  temp = android_build_string (item_name, NULL);
	  (*env)->CallNonvirtualVoidMethod (env, dialog,
					    dialog_class.class,
					    dialog_class.add_button,
					    temp, (jint) i,
					    (jboolean) NILP (enable));
	  android_exception_check ();
	  ANDROID_DELETE_LOCAL_REF (temp);
	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* The dialog is now built.  Run it.  */
  rc = (*env)->CallNonvirtualBooleanMethod (env, dialog,
					    dialog_class.class,
					    dialog_class.display);
  android_exception_check ();

  if (!rc)
    quit ();

  /* Wait for the menu ID to arrive.  */
  android_process_events_for_menu (&id);

  if (!id)
    quit ();

  /* Find the selected item, and its pane, to return
     the proper value.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (AREF (menu_items, i), Qt))
	i += MENU_ITEMS_PANE_LENGTH;
      else if (EQ (AREF (menu_items, i), Qquote))
	/* This is the boundary between left-side elts and right-side
	   elts.  */
	++i;
      else
	{
	  entry = AREF (menu_items, i + MENU_ITEMS_ITEM_VALUE);

	  if (id == i)
	    return entry;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  return Qnil;
}

Lisp_Object
android_popup_dialog (struct frame *f, Lisp_Object header,
		      Lisp_Object contents)
{
  Lisp_Object title;
  const char *error_name;
  Lisp_Object selection;
  specpdl_ref specpdl_count = SPECPDL_INDEX ();

  check_window_system (f);

  /* Decode the dialog items from what was specified.  */
  title = Fcar (contents);
  CHECK_STRING (title);
  record_unwind_protect_void (unuse_menu_items);

  if (NILP (Fcar (Fcdr (contents))))
    /* No buttons specified, add an "Ok" button so users can pop down
       the dialog.  */
    contents = list2 (title, Fcons (build_string ("Ok"), Qt));

  list_of_panes (list1 (contents));

  /* Display them in a dialog box.  */
  block_input ();
  selection = android_dialog_show (f, title, header, &error_name);
  unblock_input ();

  unbind_to (specpdl_count, Qnil);
  discard_menu_items ();

  if (error_name)
    error ("%s", error_name);

  return selection;
}

#else

int
popup_activated (void)
{
  return 0;
}

#endif

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p,
       Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* SKIP: real doc in xfns.c.  */)
  (void)
{
  return (popup_activated ()) ? Qt : Qnil;
}

void
init_androidmenu (void)
{
#ifndef ANDROID_STUBIFY
  android_init_emacs_context_menu ();
  android_init_emacs_dialog ();
#endif
}

void
syms_of_androidmenu (void)
{
  defsubr (&Smenu_or_popup_active_p);
}
