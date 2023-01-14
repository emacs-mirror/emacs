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

#include "lisp.h"
#include "androidterm.h"
#include "android.h"
#include "blockinput.h"
#include "keyboard.h"
#include "menu.h"

#ifndef ANDROID_STUBIFY

/* Flag indicating whether or not a popup menu has been posted and not
   yet popped down.  */

static int popup_activated_flag;

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
		      "(Ljava/lang/String;)Lorg/gnu/emacs/EmacsContextMenu;");

  FIND_METHOD (add_item, "addItem", "(ILjava/lang/String;Z)V");
  FIND_METHOD (add_submenu, "addSubmenu", "(Ljava/lang/String;"
	       "Ljava/lang/String;)Lorg/gnu/emacs/EmacsContextMenu;");
  FIND_METHOD (add_pane, "addPane", "(Ljava/lang/String;)V");
  FIND_METHOD (parent, "parent", "()Lorg/gnu/emacs/EmacsContextMenu;");
  FIND_METHOD (display, "display", "(Lorg/gnu/emacs/EmacsWindow;II)Z");

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

Lisp_Object
android_menu_show (struct frame *f, int x, int y, int menuflags,
		   Lisp_Object title, const char **error_name)
{
  jobject context_menu, current_context_menu;
  jobject title_string, temp;
  size_t i;
  Lisp_Object pane_name, prefix;
  const char *pane_string;
  specpdl_ref count, count1;
  Lisp_Object item_name, enable, def;
  jmethodID method;
  jobject store;
  bool rc;
  jobject window;

  count = SPECPDL_INDEX ();

  block_input ();

  /* Push the first local frame.  */
  android_push_local_frame ();

  /* Push the first local frame for the context menu.  */
  title_string = (!NILP (title)
		  ? (jobject) android_build_string (title)
		  : NULL);
  method = menu_class.create_context_menu;
  current_context_menu = context_menu
    = (*android_java_env)->CallStaticObjectMethod (android_java_env,
						   menu_class.class,
						   method,
						   title_string);

  if (title_string)
    ANDROID_DELETE_LOCAL_REF (title_string);

  /* Push the second local frame for temporaries.  */
  count1 = SPECPDL_INDEX ();
  android_push_local_frame ();

  /* Iterate over the menu.  */
  i = 0;

  while (i < menu_items_used)
    {
      if (NILP (AREF (menu_items, i)))
	{
	  /* This is the start of a new submenu.  However, it can be
	     ignored here.  */
	  i += 1;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  /* This is the end of a submenu.  Go back to the previous
	     context menu.  */
	  store = current_context_menu;
	  current_context_menu
	    = (*android_java_env)->CallObjectMethod (android_java_env,
						     current_context_menu,
						     menu_class.parent);
	  android_exception_check ();

	  if (store != context_menu)
	    ANDROID_DELETE_LOCAL_REF (store);
	  i += 1;

	  eassert (current_context_menu);
	}
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  /* This is a new pane.  Switch back to the topmost context
	     menu.  */
	  if (current_context_menu != context_menu)
	    ANDROID_DELETE_LOCAL_REF (current_context_menu);
	  current_context_menu = context_menu;

	  /* Now figure out the title of this pane.  */
	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);
	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  if ((menuflags & MENU_KEYMAPS) && !NILP (prefix))
	    pane_string++;

	  /* Add the pane.  */
	  temp = (*android_java_env)->NewStringUTF (android_java_env,
						    pane_string);
	  android_exception_check ();

	  (*android_java_env)->CallVoidMethod (android_java_env,
					       current_context_menu,
					       menu_class.add_pane,
					       temp);
	  android_exception_check ();
	  ANDROID_DELETE_LOCAL_REF (temp);

	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);

	  /* This is an actual menu item (or submenu).  Add it to the
	     menu.  */

	  if (i + MENU_ITEMS_ITEM_LENGTH < menu_items_used &&
	      NILP (AREF (menu_items, i + MENU_ITEMS_ITEM_LENGTH)))
	    {
	      /* This is a submenu.  Add it.  */
	      title_string = (!NILP (item_name)
			      ? android_build_string (item_name)
			      : NULL);
	      store = current_context_menu;
	      current_context_menu
		= (*android_java_env)->CallObjectMethod (android_java_env,
							 current_context_menu,
							 menu_class.add_submenu,
							 title_string);
	      android_exception_check ();

	      if (store != context_menu)
		ANDROID_DELETE_LOCAL_REF (store);

	      if (title_string)
		ANDROID_DELETE_LOCAL_REF (title_string);
	    }
	  else if (NILP (def) && menu_separator_name_p (SSDATA (item_name)))
	    /* Ignore this separator item.  */
	    ;
	  else
	    {
	      /* Add this menu item with the appropriate state.  */

	      title_string = (!NILP (item_name)
			      ? android_build_string (item_name)
			      : NULL);
	      (*android_java_env)->CallVoidMethod (android_java_env,
						   current_context_menu,
						   menu_class.add_item,
						   (jint) 1,
						   title_string,
						   (jboolean) !NILP (enable));
	      android_exception_check ();

	      if (title_string)
		ANDROID_DELETE_LOCAL_REF (title_string);
	    }

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }

  /* The menu has now been built.  Pop the second local frame.  */
  unbind_to (count1, Qnil);

  /* Now, display the context menu.  */
  window = android_resolve_handle (FRAME_ANDROID_WINDOW (f),
				   ANDROID_HANDLE_WINDOW);
  rc = (*android_java_env)->CallBooleanMethod (android_java_env,
					       context_menu,
					       window, (jint) x,
					       (jint) y);
  android_exception_check ();

  if (!rc)
    /* This means displaying the menu failed.  */
    goto finish;

#if 0
  record_unwind_protect_ptr (android_dismiss_menu, &context_menu);

  /* Otherwise, loop waiting for the menu event to arrive.  */
  android_process_events_for_menu (&id);

  if (!id)
    /* This means no menu item was selected.  */
    goto finish;

#endif

 finish:
  unblock_input ();
  return unbind_to (count, Qnil);
}

#endif

void
init_androidmenu (void)
{
#ifndef ANDROID_STUBIFY
  android_init_emacs_context_menu ();
#endif
}
