/* Inotify support for Emacs

Copyright (C) 2012-2017 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_INOTIFY

#include "lisp.h"
#include "coding.h"
#include "process.h"
#include "keyboard.h"
#include "termhooks.h"

#include <errno.h>
#include <sys/inotify.h>
#include <sys/ioctl.h>

/* Ignore bits that might be undefined on old GNU/Linux systems.  */
#ifndef IN_EXCL_UNLINK
# define IN_EXCL_UNLINK 0
#endif
#ifndef IN_DONT_FOLLOW
# define IN_DONT_FOLLOW 0
#endif
#ifndef IN_ONLYDIR
# define IN_ONLYDIR 0
#endif

/* File handle for inotify.  */
static int inotifyfd = -1;

/* Assoc list of files being watched.
   Format: (watch-descriptor name callback)
 */
static Lisp_Object watch_list;

static Lisp_Object
make_watch_descriptor (int wd)
{
  /* TODO replace this with a Misc Object! */
  return make_number (wd);
}

static Lisp_Object
mask_to_aspects (uint32_t mask) {
  Lisp_Object aspects = Qnil;
  if (mask & IN_ACCESS)
    aspects = Fcons (Qaccess, aspects);
  if (mask & IN_ATTRIB)
    aspects = Fcons (Qattrib, aspects);
  if (mask & IN_CLOSE_WRITE)
    aspects = Fcons (Qclose_write, aspects);
  if (mask & IN_CLOSE_NOWRITE)
    aspects = Fcons (Qclose_nowrite, aspects);
  if (mask & IN_CREATE)
    aspects = Fcons (Qcreate, aspects);
  if (mask & IN_DELETE)
    aspects = Fcons (Qdelete, aspects);
  if (mask & IN_DELETE_SELF)
    aspects = Fcons (Qdelete_self, aspects);
  if (mask & IN_MODIFY)
    aspects = Fcons (Qmodify, aspects);
  if (mask & IN_MOVE_SELF)
    aspects = Fcons (Qmove_self, aspects);
  if (mask & IN_MOVED_FROM)
    aspects = Fcons (Qmoved_from, aspects);
  if (mask & IN_MOVED_TO)
    aspects = Fcons (Qmoved_to, aspects);
  if (mask & IN_OPEN)
    aspects = Fcons (Qopen,  aspects);
  if (mask & IN_IGNORED)
    aspects = Fcons (Qignored, aspects);
  if (mask & IN_ISDIR)
    aspects = Fcons (Qisdir, aspects);
  if (mask & IN_Q_OVERFLOW)
    aspects = Fcons (Qq_overflow, aspects);
  if (mask & IN_UNMOUNT)
    aspects = Fcons (Qunmount, aspects);
  return aspects;
}

static Lisp_Object
inotifyevent_to_event (Lisp_Object watch_object, struct inotify_event const *ev)
{
  Lisp_Object name = Qnil;
  if (ev->len > 0)
    {
      size_t const len = strlen (ev->name);
      name = make_unibyte_string (ev->name, min (len, ev->len));
      name = DECODE_FILE (name);
    }
  else
    name = XCAR (XCDR (watch_object));

  return list2 (list4 (make_watch_descriptor (ev->wd),
                       mask_to_aspects (ev->mask),
                       name,
                       make_number (ev->cookie)),
		Fnth (make_number (2), watch_object));
}

/* This callback is called when the FD is available for read.  The inotify
   events are read from FD and converted into input_events.  */
static void
inotify_callback (int fd, void *_)
{
  struct input_event event;
  Lisp_Object watch_object;
  int to_read;
  char *buffer;
  ssize_t n;
  size_t i;

  to_read = 0;
  if (ioctl (fd, FIONREAD, &to_read) == -1)
    report_file_notify_error ("Error while retrieving file system events",
			      Qnil);
  buffer = xmalloc (to_read);
  n = read (fd, buffer, to_read);
  if (n < 0)
    {
      xfree (buffer);
      report_file_notify_error ("Error while reading file system events", Qnil);
    }

  EVENT_INIT (event);
  event.kind = FILE_NOTIFY_EVENT;

  i = 0;
  while (i < (size_t)n)
    {
      struct inotify_event *ev = (struct inotify_event*)&buffer[i];

      watch_object = Fassoc (make_watch_descriptor (ev->wd), watch_list);
      if (!NILP (watch_object))
        {
          event.arg = inotifyevent_to_event (watch_object, ev);

          /* If event was removed automatically: Drop it from watch list.  */
          if (ev->mask & IN_IGNORED)
            watch_list = Fdelete (watch_object, watch_list);

	  if (!NILP (event.arg))
	    kbd_buffer_store_event (&event);
        }

      i += sizeof (*ev) + ev->len;
    }

  xfree (buffer);
}

static uint32_t
symbol_to_inotifymask (Lisp_Object symb)
{
  if (EQ (symb, Qaccess))
    return IN_ACCESS;
  else if (EQ (symb, Qattrib))
    return IN_ATTRIB;
  else if (EQ (symb, Qclose_write))
    return IN_CLOSE_WRITE;
  else if (EQ (symb, Qclose_nowrite))
    return IN_CLOSE_NOWRITE;
  else if (EQ (symb, Qcreate))
    return IN_CREATE;
  else if (EQ (symb, Qdelete))
    return IN_DELETE;
  else if (EQ (symb, Qdelete_self))
    return IN_DELETE_SELF;
  else if (EQ (symb, Qmodify))
    return IN_MODIFY;
  else if (EQ (symb, Qmove_self))
    return IN_MOVE_SELF;
  else if (EQ (symb, Qmoved_from))
    return IN_MOVED_FROM;
  else if (EQ (symb, Qmoved_to))
    return IN_MOVED_TO;
  else if (EQ (symb, Qopen))
    return IN_OPEN;
  else if (EQ (symb, Qmove))
    return IN_MOVE;
  else if (EQ (symb, Qclose))
    return IN_CLOSE;

  else if (EQ (symb, Qdont_follow))
    return IN_DONT_FOLLOW;
  else if (EQ (symb, Qexcl_unlink))
    return IN_EXCL_UNLINK;
  else if (EQ (symb, Qmask_add))
    return IN_MASK_ADD;
  else if (EQ (symb, Qoneshot))
    return IN_ONESHOT;
  else if (EQ (symb, Qonlydir))
    return IN_ONLYDIR;

  else if (EQ (symb, Qt) || EQ (symb, Qall_events))
    return IN_ALL_EVENTS;
  else
    {
      errno = EINVAL;
      report_file_notify_error ("Unknown aspect", symb);
    }
}

static uint32_t
aspect_to_inotifymask (Lisp_Object aspect)
{
  if (CONSP (aspect))
    {
      Lisp_Object x = aspect;
      uint32_t mask = 0;
      while (CONSP (x))
        {
          mask |= symbol_to_inotifymask (XCAR (x));
          x = XCDR (x);
        }
      return mask;
    }
  else
    return symbol_to_inotifymask (aspect);
}

DEFUN ("inotify-add-watch", Finotify_add_watch, Sinotify_add_watch, 3, 3, 0,
       doc: /* Add a watch for FILE-NAME to inotify.

Return a watch descriptor.  The watch will look for ASPECT events and
invoke CALLBACK when an event occurs.

ASPECT might be one of the following symbols or a list of those symbols:

access
attrib
close-write
close-nowrite
create
delete
delete-self
modify
move-self
moved-from
moved-to
open

all-events or t
move
close

The following symbols can also be added to a list of aspects:

dont-follow
excl-unlink
mask-add
oneshot
onlydir

Watching a directory is not recursive.  CALLBACK is passed a single argument
EVENT which contains an event structure of the format

\(WATCH-DESCRIPTOR ASPECTS NAME COOKIE)

WATCH-DESCRIPTOR is the same object that was returned by this function.  It can
be tested for equality using `equal'.  ASPECTS describes the event.  It is a
list of ASPECT symbols described above and can also contain one of the following
symbols

ignored
isdir
q-overflow
unmount

If a directory is watched then NAME is the name of file that caused the event.

COOKIE is an object that can be compared using `equal' to identify two matching
renames (moved-from and moved-to).

See inotify(7) and inotify_add_watch(2) for further information.  The inotify fd
is managed internally and there is no corresponding inotify_init.  Use
`inotify-rm-watch' to remove a watch.
             */)
     (Lisp_Object file_name, Lisp_Object aspect, Lisp_Object callback)
{
  uint32_t mask;
  Lisp_Object watch_object;
  Lisp_Object encoded_file_name;
  Lisp_Object watch_descriptor;
  int watchdesc = -1;

  CHECK_STRING (file_name);

  if (inotifyfd < 0)
    {
      inotifyfd = inotify_init1 (IN_NONBLOCK|IN_CLOEXEC);
      if (inotifyfd < 0)
	report_file_notify_error ("File watching is not available", Qnil);
      watch_list = Qnil;
      add_read_fd (inotifyfd, &inotify_callback, NULL);
    }

  mask = aspect_to_inotifymask (aspect);
  encoded_file_name = ENCODE_FILE (file_name);
  watchdesc = inotify_add_watch (inotifyfd, SSDATA (encoded_file_name), mask);
  if (watchdesc == -1)
    report_file_notify_error ("Could not add watch for file", file_name);

  watch_descriptor = make_watch_descriptor (watchdesc);

  /* Delete existing watch object.  */
  watch_object = Fassoc (watch_descriptor, watch_list);
  if (!NILP (watch_object))
      watch_list = Fdelete (watch_object, watch_list);

  /* Store watch object in watch list.  */
  watch_object = list3 (watch_descriptor, encoded_file_name, callback);
  watch_list = Fcons (watch_object, watch_list);

  return watch_descriptor;
}

DEFUN ("inotify-rm-watch", Finotify_rm_watch, Sinotify_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `inotify-add-watch'.

See inotify_rm_watch(2) for more information.
             */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object;
  int wd = XINT (watch_descriptor);

  if (inotify_rm_watch (inotifyfd, wd) == -1)
    report_file_notify_error ("Could not rm watch", watch_descriptor);

  /* Remove watch descriptor from watch list.  */
  watch_object = Fassoc (watch_descriptor, watch_list);
  if (!NILP (watch_object))
    watch_list = Fdelete (watch_object, watch_list);

  /* Cleanup if no more files are watched.  */
  if (NILP (watch_list))
    {
      emacs_close (inotifyfd);
      delete_read_fd (inotifyfd);
      inotifyfd = -1;
    }

  return Qt;
}

DEFUN ("inotify-valid-p", Finotify_valid_p, Sinotify_valid_p, 1, 1, 0,
       doc: /* Check a watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `inotify-add-watch'.

A watch can become invalid if the file or directory it watches is
deleted, or if the watcher thread exits abnormally for any other
reason.  Removing the watch by calling `inotify-rm-watch' also makes
it invalid.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = Fassoc (watch_descriptor, watch_list);
  return NILP (watch_object) ? Qnil : Qt;
}

void
syms_of_inotify (void)
{
  DEFSYM (Qaccess, "access");		/* IN_ACCESS */
  DEFSYM (Qattrib, "attrib");		/* IN_ATTRIB */
  DEFSYM (Qclose_write, "close-write");	/* IN_CLOSE_WRITE */
  DEFSYM (Qclose_nowrite, "close-nowrite");
					/* IN_CLOSE_NOWRITE */
  DEFSYM (Qcreate, "create");		/* IN_CREATE */
  DEFSYM (Qdelete, "delete");		/* IN_DELETE */
  DEFSYM (Qdelete_self, "delete-self");	/* IN_DELETE_SELF */
  DEFSYM (Qmodify, "modify");		/* IN_MODIFY */
  DEFSYM (Qmove_self, "move-self");	/* IN_MOVE_SELF */
  DEFSYM (Qmoved_from, "moved-from");	/* IN_MOVED_FROM */
  DEFSYM (Qmoved_to, "moved-to");	/* IN_MOVED_TO */
  DEFSYM (Qopen, "open");		/* IN_OPEN */

  DEFSYM (Qall_events, "all-events");	/* IN_ALL_EVENTS */
  DEFSYM (Qmove, "move");		/* IN_MOVE */
  DEFSYM (Qclose, "close");		/* IN_CLOSE */

  DEFSYM (Qdont_follow, "dont-follow");	/* IN_DONT_FOLLOW */
  DEFSYM (Qexcl_unlink, "excl-unlink");	/* IN_EXCL_UNLINK */
  DEFSYM (Qmask_add, "mask-add");	/* IN_MASK_ADD */
  DEFSYM (Qoneshot, "oneshot");		/* IN_ONESHOT */
  DEFSYM (Qonlydir, "onlydir");		/* IN_ONLYDIR */

  DEFSYM (Qignored, "ignored");		/* IN_IGNORED */
  DEFSYM (Qisdir, "isdir");		/* IN_ISDIR */
  DEFSYM (Qq_overflow, "q-overflow");	/* IN_Q_OVERFLOW */
  DEFSYM (Qunmount, "unmount");		/* IN_UNMOUNT */

  defsubr (&Sinotify_add_watch);
  defsubr (&Sinotify_rm_watch);
  defsubr (&Sinotify_valid_p);

  staticpro (&watch_list);

  Fprovide (intern_c_string ("inotify"), Qnil);
}

#endif /* HAVE_INOTIFY */
