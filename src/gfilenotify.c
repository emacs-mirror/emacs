/* Filesystem notifications support with glib API.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_GFILENOTIFY
#include <stdio.h>
#include <gio/gio.h>
#include "lisp.h"
#include "coding.h"
#include "frame.h"
#include "termhooks.h"
#include "keyboard.h"
#include "process.h"


/* Subroutines.  */
static Lisp_Object Qgfile_add_watch;
static Lisp_Object Qgfile_rm_watch;

/* Filter objects.  */
static Lisp_Object Qwatch_mounts;      /* G_FILE_MONITOR_WATCH_MOUNTS  */
static Lisp_Object Qsend_moved;        /* G_FILE_MONITOR_SEND_MOVED  */

/* Event types.  */
static Lisp_Object Qchanged;           /* G_FILE_MONITOR_EVENT_CHANGED  */
static Lisp_Object Qchanges_done_hint; /* G_FILE_MONITOR_EVENT_CHANGES_DONE_HINT  */
static Lisp_Object Qdeleted;           /* G_FILE_MONITOR_EVENT_DELETED  */
static Lisp_Object Qcreated;           /* G_FILE_MONITOR_EVENT_CREATED  */
static Lisp_Object Qattribute_changed; /* G_FILE_MONITOR_EVENT_ATTRIBUTE_CHANGED  */
static Lisp_Object Qpre_unmount;       /* G_FILE_MONITOR_EVENT_PRE_UNMOUNT  */
static Lisp_Object Qunmounted;         /* G_FILE_MONITOR_EVENT_UNMOUNTED  */
static Lisp_Object Qmoved;             /* G_FILE_MONITOR_EVENT_MOVED  */

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
  Lisp_Object symbol, monitor_object, watch_object;
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
  monitor_object = XIL ((intptr_t) monitor);
  eassert (INTEGERP (monitor_object));
  watch_object = assq_no_quit (monitor_object, watch_list);

  if (CONSP (watch_object))
    {
      /* Construct an event.  */
      struct input_event event;
      Lisp_Object otail = oname ? list1 (build_string (oname)) : Qnil;
      EVENT_INIT (event);
      event.kind = FILE_NOTIFY_EVENT;
      event.frame_or_window = Qnil;
      event.arg = list2 (Fcons (monitor_object,
				Fcons (symbol,
				       Fcons (build_string (name),
					      otail))),
			 XCDR (watch_object));

      /* Store it into the input event queue.  */
      kbd_buffer_store_event (&event);
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

  'watch-mounts' -- watch for mount events
  'send-moved'   -- pair 'deleted' and 'created' events caused by file
                    renames and send a single 'renamed' event instead

When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTION FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTION is the description of the event.  It could be any one of the
following:

  'changed'           -- FILE has changed
  'changes-done-hint' -- a hint that this was probably the last change
                         in a set of changes
  'deleted'           -- FILE was deleted
  'created'           -- FILE was created
  'attribute-changed' -- a FILE attribute was changed
  'pre-unmount'       -- the FILE location will soon be unmounted
  'unmounted'         -- the FILE location was unmounted
  'moved'             -- FILE was moved to FILE1

FILE is the name of the file whose event is being reported.  FILE1
will be reported only in case of the 'moved' event.  */)
  (Lisp_Object file, Lisp_Object flags, Lisp_Object callback)
{
  Lisp_Object watch_descriptor, watch_object;
  GFile *gfile;
  GFileMonitor *monitor;
  GFileMonitorFlags gflags = G_FILE_MONITOR_NONE;

  /* Check parameters.  */
  CHECK_STRING (file);
  file = Fdirectory_file_name (Fexpand_file_name (file, Qnil));
  if (NILP (Ffile_exists_p (file)))
    report_file_error ("File does not exist", file);

  CHECK_LIST (flags);

  if (!FUNCTIONP (callback))
    wrong_type_argument (Qinvalid_function, callback);

  /* Create GFile name.  */
  gfile = g_file_new_for_path (SSDATA (ENCODE_FILE (file)));

  /* Assemble flags.  */
  if (!NILP (Fmember (Qwatch_mounts, flags)))
    gflags |= G_FILE_MONITOR_WATCH_MOUNTS;
  if (!NILP (Fmember (Qsend_moved, flags)))
    gflags |= G_FILE_MONITOR_SEND_MOVED;

  /* Enable watch.  */
  monitor = g_file_monitor (gfile, gflags, NULL, NULL);
  if (! monitor)
    xsignal2 (Qfile_notify_error, build_string ("Cannot watch file"), file);

  /* On all known glib platforms, converting MONITOR directly to a
     Lisp_Object value results is a Lisp integer, which is safe.  This
     assumption is dicey, though, so check it now.  */
  watch_descriptor = XIL ((intptr_t) monitor);
  if (! INTEGERP (watch_descriptor))
    {
      g_object_unref (monitor);
      xsignal2 (Qfile_notify_error, build_string ("Unsupported file watcher"),
		file);
    }

  g_signal_connect (monitor, "changed",
		    (GCallback) dir_monitor_callback, NULL);

  /* Store watch object in watch list.  */
  watch_object = Fcons (watch_descriptor, callback);
  watch_list = Fcons (watch_object, watch_list);

  return watch_descriptor;
}

DEFUN ("gfile-rm-watch", Fgfile_rm_watch, Sgfile_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `gfile-add-watch'.  */)
     (Lisp_Object watch_descriptor)
{
  intptr_t int_monitor;
  GFileMonitor *monitor;
  Lisp_Object watch_object = assq_no_quit (watch_descriptor, watch_list);

  if (! CONSP (watch_object))
    xsignal2 (Qfile_notify_error, build_string ("Not a watch descriptor"),
	      watch_descriptor);

  eassert (INTEGERP (watch_descriptor));
  int_monitor = XLI (watch_descriptor);
  monitor = (GFileMonitor *) int_monitor;
  if (!g_file_monitor_cancel (monitor))
    xsignal2 (Qfile_notify_error, build_string ("Could not rm watch"),
	      watch_descriptor);

  /* Remove watch descriptor from watch list. */
  watch_list = Fdelq (watch_object, watch_list);

  /* Cleanup.  */
  g_object_unref (monitor);

  return Qt;
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

  DEFSYM (Qgfile_add_watch, "gfile-add-watch");
  defsubr (&Sgfile_add_watch);

  DEFSYM (Qgfile_rm_watch, "gfile-rm-watch");
  defsubr (&Sgfile_rm_watch);

  DEFSYM (Qwatch_mounts, "watch-mounts");
  DEFSYM (Qsend_moved, "send-moved");
  DEFSYM (Qchanged, "changed");
  DEFSYM (Qchanges_done_hint, "changes-done-hint");
  DEFSYM (Qdeleted, "deleted");
  DEFSYM (Qcreated, "created");
  DEFSYM (Qattribute_changed, "attribute-changed");
  DEFSYM (Qpre_unmount, "pre-unmount");
  DEFSYM (Qunmounted, "unmounted");
  DEFSYM (Qmoved, "moved");

  staticpro (&watch_list);

  Fprovide (intern_c_string ("gfilenotify"), Qnil);

}

#endif /* HAVE_GFILENOTIFY  */
