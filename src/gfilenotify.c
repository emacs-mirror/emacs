/* Filesystem notifications support with glib API.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.

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

#ifdef HAVE_GFILENOTIFY
#include <stdio.h>
#include <gio/gio.h>
#include "lisp.h"
#include "coding.h"
#include "termhooks.h"
#include "keyboard.h"


/* This is a list, elements are quadruples (DESCRIPTOR FILE FLAGS CALLBACK)  */
static Lisp_Object watch_list;

/* This is the callback function for arriving signals from
   g_file_monitor.  It shall create a Lisp event, and put it into
   Emacs input queue.  */
static gboolean
dir_monitor_callback (GFileMonitor *monitor,
		      GFile *file,
		      GFile *other_file,
		      GFileMonitorEvent event_type,
		      gpointer user_data)
{
  Lisp_Object symbol, monitor_object, watch_object, flags;
  char *name = g_file_get_parse_name (file);
  char *oname = other_file ? g_file_get_parse_name (other_file) : NULL;

  /* Determine event symbol.  */
  switch (event_type)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
      symbol = Qchanged;
      break;
    case G_FILE_MONITOR_EVENT_CHANGES_DONE_HINT:
      symbol = Qchanges_done_hint;
      break;
    case G_FILE_MONITOR_EVENT_DELETED:
      symbol = Qdeleted;
      break;
    case G_FILE_MONITOR_EVENT_CREATED:
      symbol = Qcreated;
      break;
    case G_FILE_MONITOR_EVENT_ATTRIBUTE_CHANGED:
      symbol = Qattribute_changed;
      break;
    case G_FILE_MONITOR_EVENT_PRE_UNMOUNT:
      symbol = Qpre_unmount;
      break;
    case G_FILE_MONITOR_EVENT_UNMOUNTED:
      symbol = Qunmounted;
      break;
    case G_FILE_MONITOR_EVENT_MOVED:
      symbol = Qmoved;
      break;
    default:
      goto cleanup;
    }

  /* Determine callback function.  */
  monitor_object = make_pointer_integer (monitor);
  eassert (INTEGERP (monitor_object));
  watch_object = assq_no_quit (monitor_object, watch_list);

  if (CONSP (watch_object))
    {
      struct input_event event;
      Lisp_Object otail = oname ? list1 (build_string (oname)) : Qnil;

      /* Check, whether event_type is expected.  */
      flags = XCAR (XCDR (XCDR (watch_object)));
      if ((!NILP (Fmember (Qchange, flags)) &&
	   !NILP (Fmember (symbol, list5 (Qchanged, Qchanges_done_hint,
					  Qdeleted, Qcreated, Qmoved)))) ||
	  (!NILP (Fmember (Qattribute_change, flags)) &&
	   ((EQ (symbol, Qattribute_changed)))))
	{
	  /* Construct an event.  */
	  EVENT_INIT (event);
	  event.kind = FILE_NOTIFY_EVENT;
	  event.frame_or_window = Qnil;
	  event.arg = list2 (Fcons (monitor_object,
				    Fcons (symbol,
					   Fcons (build_string (name),
						  otail))),
			     XCAR (XCDR (XCDR (XCDR (watch_object)))));

	  /* Store it into the input event queue.  */
	  kbd_buffer_store_event (&event);
	  /* XD_DEBUG_MESSAGE ("%s", XD_OBJECT_TO_STRING (event.arg));  */
	}

      /* Cancel monitor if file or directory is deleted.  */
      if (!NILP (Fmember (symbol, list2 (Qdeleted, Qmoved))) &&
	  (strcmp (name, SSDATA (XCAR (XCDR (watch_object)))) == 0) &&
	  !g_file_monitor_is_cancelled (monitor))
	g_file_monitor_cancel (monitor);
    }

  /* Cleanup.  */
 cleanup:
  g_free (name);
  g_free (oname);

  return TRUE;
}

DEFUN ("gfile-add-watch", Fgfile_add_watch, Sgfile_add_watch, 3, 3, 0,
       doc: /* Add a watch for filesystem events pertaining to FILE.

This arranges for filesystem events pertaining to FILE to be reported
to Emacs.  Use `gfile-rm-watch' to cancel the watch.

Value is a descriptor for the added watch.  If the file cannot be
watched for some reason, this function signals a `file-notify-error' error.

FLAGS is a list of conditions to set what will be watched for.  It can
include the following symbols:

  `change'           -- watch for file changes
  `attribute-change' -- watch for file attributes changes, like
                        permissions or modification time
  `watch-mounts'     -- watch for mount events
  `send-moved'       -- pair `deleted' and `created' events caused by
                        file renames and send a single `renamed' event
                        instead

When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTION FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTION is the description of the event.  It could be any one of the
following:

  `changed'           -- FILE has changed
  `changes-done-hint' -- a hint that this was probably the last change
                         in a set of changes
  `deleted'           -- FILE was deleted
  `created'           -- FILE was created
  `attribute-changed' -- a FILE attribute was changed
  `pre-unmount'       -- the FILE location will soon be unmounted
  `unmounted'         -- the FILE location was unmounted
  `moved'             -- FILE was moved to FILE1

FILE is the name of the file whose event is being reported.  FILE1
will be reported only in case of the `moved' event.  */)
  (Lisp_Object file, Lisp_Object flags, Lisp_Object callback)
{
  Lisp_Object watch_object;
  GFile *gfile;
  GFileMonitor *monitor;
  GFileMonitorFlags gflags = G_FILE_MONITOR_NONE;
  GError *gerror = NULL;

  /* Check parameters.  */
  CHECK_STRING (file);
  file = Fdirectory_file_name (Fexpand_file_name (file, Qnil));
  if (NILP (Ffile_exists_p (file)))
    report_file_error ("File does not exist", file);

  if (!FUNCTIONP (callback))
    wrong_type_argument (Qinvalid_function, callback);

  /* Assemble flags.  */
  if (!NILP (Fmember (Qwatch_mounts, flags)))
    gflags |= G_FILE_MONITOR_WATCH_MOUNTS;
  if (!NILP (Fmember (Qsend_moved, flags)))
    gflags |= G_FILE_MONITOR_SEND_MOVED;

  /* Create GFile name.  */
  gfile = g_file_new_for_path (SSDATA (ENCODE_FILE (file)));

  /* Enable watch.  */
  monitor = g_file_monitor (gfile, gflags, NULL, &gerror);
  g_object_unref (gfile);
  if (gerror)
    {
      char msg[1024];
      strcpy (msg, gerror->message);
      g_error_free (gerror);
      xsignal1 (Qfile_notify_error, build_string (msg));
    }
  if (! monitor)
    xsignal2 (Qfile_notify_error, build_string ("Cannot watch file"), file);

  Lisp_Object watch_descriptor = make_pointer_integer (monitor);

  /* Check the dicey assumption that make_pointer_integer is safe.  */
  if (! INTEGERP (watch_descriptor))
    {
      g_object_unref (monitor);
      xsignal2 (Qfile_notify_error, build_string ("Unsupported file watcher"),
		file);
    }

  /* The default rate limit is 800 msec.  We adapt this.  */
  g_file_monitor_set_rate_limit (monitor, 100);

  /* Subscribe to the "changed" signal.  */
  g_signal_connect (monitor, "changed",
		    (GCallback) dir_monitor_callback, NULL);

  /* Store watch object in watch list.  */
  watch_object = list4 (watch_descriptor, file, flags, callback);
  watch_list = Fcons (watch_object, watch_list);

  return watch_descriptor;
}

DEFUN ("gfile-rm-watch", Fgfile_rm_watch, Sgfile_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `gfile-add-watch'.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = assq_no_quit (watch_descriptor, watch_list);

  if (! CONSP (watch_object))
    xsignal2 (Qfile_notify_error, build_string ("Not a watch descriptor"),
	      watch_descriptor);

  eassert (INTEGERP (watch_descriptor));
  GFileMonitor *monitor = XINTPTR (watch_descriptor);
  if (!g_file_monitor_is_cancelled (monitor) &&
      !g_file_monitor_cancel (monitor))
      xsignal2 (Qfile_notify_error, build_string ("Could not rm watch"),
		watch_descriptor);

  /* Remove watch descriptor from watch list.  */
  watch_list = Fdelq (watch_object, watch_list);

  /* Cleanup.  */
  g_object_unref (monitor);

  return Qt;
}

DEFUN ("gfile-valid-p", Fgfile_valid_p, Sgfile_valid_p, 1, 1, 0,
       doc: /* Check a watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `gfile-add-watch'.

A watch can become invalid if the file or directory it watches is
deleted, or if the watcher thread exits abnormally for any other
reason.  Removing the watch by calling `gfile-rm-watch' also makes it
invalid.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = Fassoc (watch_descriptor, watch_list, Qnil);
  if (NILP (watch_object))
    return Qnil;
  else
    {
      GFileMonitor *monitor = XINTPTR (watch_descriptor);
      return g_file_monitor_is_cancelled (monitor) ? Qnil : Qt;
    }
}

DEFUN ("gfile-monitor-name", Fgfile_monitor_name, Sgfile_monitor_name, 1, 1, 0,
       doc: /* Return the internal monitor name for WATCH-DESCRIPTOR.

The result is a symbol, either `GInotifyFileMonitor',
`GKqueueFileMonitor', `GFamFileMonitor', or `GPollFileMonitor'.

WATCH-DESCRIPTOR should be an object returned by `gfile-add-watch'.
If WATCH-DESCRIPTOR is not valid, nil is returned.  */)
     (Lisp_Object watch_descriptor)
{
  if (NILP (Fgfile_valid_p (watch_descriptor)))
    return Qnil;
  else
    {
      GFileMonitor *monitor = XINTPTR (watch_descriptor);
      return intern (G_OBJECT_TYPE_NAME (monitor));
    }
}


void
globals_of_gfilenotify (void)
{
#if ! GLIB_CHECK_VERSION (2, 36, 0)
  g_type_init ();
#endif
  watch_list = Qnil;
}

void
syms_of_gfilenotify (void)
{
  defsubr (&Sgfile_add_watch);
  defsubr (&Sgfile_rm_watch);
  defsubr (&Sgfile_valid_p);
  defsubr (&Sgfile_monitor_name);

  /* Filter objects.  */
  DEFSYM (Qchange, "change");
  DEFSYM (Qattribute_change, "attribute-change");
  DEFSYM (Qwatch_mounts, "watch-mounts"); /* G_FILE_MONITOR_WATCH_MOUNTS  */
  DEFSYM (Qsend_moved, "send-moved");	/* G_FILE_MONITOR_SEND_MOVED  */

  /* Event types.  */
  DEFSYM (Qchanged, "changed");	/* G_FILE_MONITOR_EVENT_CHANGED  */
  DEFSYM (Qchanges_done_hint, "changes-done-hint");
				/* G_FILE_MONITOR_EVENT_CHANGES_DONE_HINT  */
  DEFSYM (Qdeleted, "deleted");	/* G_FILE_MONITOR_EVENT_DELETED  */
  DEFSYM (Qcreated, "created");	/* G_FILE_MONITOR_EVENT_CREATED  */
  DEFSYM (Qattribute_changed, "attribute-changed");
				/* G_FILE_MONITOR_EVENT_ATTRIBUTE_CHANGED  */
  DEFSYM (Qpre_unmount, "pre-unmount");	/* G_FILE_MONITOR_EVENT_PRE_UNMOUNT  */
  DEFSYM (Qunmounted, "unmounted");	/* G_FILE_MONITOR_EVENT_UNMOUNTED  */
  DEFSYM (Qmoved, "moved");	/* G_FILE_MONITOR_EVENT_MOVED  */

  staticpro (&watch_list);

  Fprovide (intern_c_string ("gfilenotify"), Qnil);

}

#endif /* HAVE_GFILENOTIFY  */
