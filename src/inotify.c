/* Inotify support for Emacs

Copyright (C) 2012-2018 Free Software Foundation, Inc.

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

/* Alist of files being watched.  We want the returned descriptor to
   be unique for every watch, but inotify returns the same descriptor
   WD for multiple calls to inotify_add_watch with the same file.
   Supply a nonnegative integer ID, so that WD and ID together
   uniquely identify a watch/file combination.

   For the same reason, we also need to store the watch's mask and we
   can't allow the following flags to be used.

   IN_EXCL_UNLINK
   IN_MASK_ADD
   IN_ONESHOT

   Each element of this list is of the form (DESCRIPTOR . WATCHES)
   where no two DESCRIPTOR values are the same.  DESCRIPTOR represents
   the inotify watch descriptor and WATCHES is a list with elements of
   the form (ID FILENAME CALLBACK MASK), where ID is the integer
   described above, FILENAME names the file being watched, CALLBACK is
   invoked when the event occurs, and MASK represents the aspects
   being watched.  The WATCHES list is sorted by ID.  Although
   DESCRIPTOR and MASK are ordinarily integers, they are conses when
   representing integers outside of fixnum range.  */

static Lisp_Object watch_list;

static Lisp_Object
mask_to_aspects (uint32_t mask)
{
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
  if (CONSP (aspect) || NILP (aspect))
    {
      Lisp_Object x = aspect;
      uint32_t mask = 0;
      FOR_EACH_TAIL (x)
	mask |= symbol_to_inotifymask (XCAR (x));
      CHECK_LIST_END (x, aspect);
      return mask;
    }
  else
    return symbol_to_inotifymask (aspect);
}

static Lisp_Object
inotifyevent_to_event (Lisp_Object watch, struct inotify_event const *ev)
{
  Lisp_Object name;
  uint32_t mask;
  CONS_TO_INTEGER (Fnth (make_number (3), watch), uint32_t, mask);

  if (! (mask & ev->mask))
    return Qnil;

  if (ev->len > 0)
    {
      size_t const len = strlen (ev->name);
      name = make_unibyte_string (ev->name, min (len, ev->len));
      name = DECODE_FILE (name);
    }
  else
    name = XCAR (XCDR (watch));

  return list2 (list4 (Fcons (INTEGER_TO_CONS (ev->wd), XCAR (watch)),
                       mask_to_aspects (ev->mask),
                       name,
		       INTEGER_TO_CONS (ev->cookie)),
		Fnth (make_number (2), watch));
}

/* Add a new watch to watch-descriptor WD watching FILENAME and using
   IMASK and CALLBACK.  Return a cons (DESCRIPTOR . ID) uniquely
   identifying the new watch.  */
static Lisp_Object
add_watch (int wd, Lisp_Object filename,
	   uint32_t imask, Lisp_Object callback)
{
  Lisp_Object descriptor = INTEGER_TO_CONS (wd);
  Lisp_Object tail = assoc_no_quit (descriptor, watch_list);
  Lisp_Object watch, watch_id;
  Lisp_Object mask = INTEGER_TO_CONS (imask);

  EMACS_INT id = 0;
  if (NILP (tail))
    {
      tail = list1 (descriptor);
      watch_list = Fcons (tail, watch_list);
    }
  else
    {
      /* Assign a watch ID that is not already in use, by looking
	 for a gap in the existing sorted list.  */
      for (; ! NILP (XCDR (tail)); tail = XCDR (tail), id++)
	if (!EQ (XCAR (XCAR (XCDR (tail))), make_number (id)))
	  break;
      if (MOST_POSITIVE_FIXNUM < id)
	emacs_abort ();
    }

  /* Insert the newly-assigned ID into the previously-discovered gap,
     which is possibly at the end of the list.  Inserting it there
     keeps the list sorted.  */
  watch_id = make_number (id);
  watch = list4 (watch_id, filename, callback, mask);
  XSETCDR (tail, Fcons (watch, XCDR (tail)));

  return Fcons (descriptor, watch_id);
}

/* Find the watch list element (if any) matching DESCRIPTOR.  Return
   nil if not found.  If found, return t if the first element matches
   DESCRIPTOR; otherwise, return the cons whose cdr matches
   DESCRIPTOR.  This lets the caller easily remove the element
   matching DESCRIPTOR without having to search for it again, and
   without calling Fdelete (which might quit).  */

static Lisp_Object
find_descriptor (Lisp_Object descriptor)
{
  Lisp_Object tail, prevtail = Qt;
  for (tail = watch_list; !NILP (tail); prevtail = tail, tail = XCDR (tail))
    if (equal_no_quit (XCAR (XCAR (tail)), descriptor))
      return prevtail;
  return Qnil;
}

/*  Remove all watches associated with the watch list element after
    PREVTAIL, or after the first element if PREVTAIL is t.  If INVALID_P
    is true, the descriptor is already invalid, i.e., it received a
    IN_IGNORED event.  In this case skip calling inotify_rm_watch.  */
static void
remove_descriptor (Lisp_Object prevtail, bool invalid_p)
{
  Lisp_Object tail = CONSP (prevtail) ? XCDR (prevtail) : watch_list;

  int inotify_errno = 0;
  if (! invalid_p)
    {
      int wd;
      CONS_TO_INTEGER (XCAR (XCAR (tail)), int, wd);
      if (inotify_rm_watch (inotifyfd, wd) != 0)
	inotify_errno = errno;
    }

  if (CONSP (prevtail))
    XSETCDR (prevtail, XCDR (tail));
  else
    {
      watch_list = XCDR (tail);
      if (NILP (watch_list))
	{
	  delete_read_fd (inotifyfd);
	  emacs_close (inotifyfd);
	  inotifyfd = -1;
	}
    }

  if (inotify_errno != 0)
    {
      errno = inotify_errno;
      report_file_notify_error ("Could not rm watch", XCAR (tail));
    }
}

/*  Remove watch associated with (descriptor, id).  */
static void
remove_watch (Lisp_Object descriptor, Lisp_Object id)
{
  Lisp_Object prevtail = find_descriptor (descriptor);
  if (NILP (prevtail))
    return;

  Lisp_Object elt = XCAR (CONSP (prevtail) ? XCDR (prevtail) : watch_list);
  for (Lisp_Object prev = elt; !NILP (XCDR (prev)); prev = XCDR (prev))
    if (EQ (id, XCAR (XCAR (XCDR (prev)))))
      {
	XSETCDR (prev, XCDR (XCDR (prev)));
	if (NILP (XCDR (elt)))
	  remove_descriptor (prevtail, false);
	break;
      }
}

/* This callback is called when the FD is available for read.  The inotify
   events are read from FD and converted into input_events.  */
static void
inotify_callback (int fd, void *_)
{
  int to_read;
  if (ioctl (fd, FIONREAD, &to_read) < 0)
    report_file_notify_error ("Error while retrieving file system events",
			      Qnil);
  USE_SAFE_ALLOCA;
  char *buffer = SAFE_ALLOCA (to_read);
  ssize_t n = read (fd, buffer, to_read);
  if (n < 0)
    report_file_notify_error ("Error while reading file system events", Qnil);

  struct input_event event;
  EVENT_INIT (event);
  event.kind = FILE_NOTIFY_EVENT;

  for (ssize_t i = 0; i < n; )
    {
      struct inotify_event *ev = (struct inotify_event *) &buffer[i];
      Lisp_Object descriptor = INTEGER_TO_CONS (ev->wd);
      Lisp_Object prevtail = find_descriptor (descriptor);

      if (! NILP (prevtail))
        {
	  Lisp_Object tail = CONSP (prevtail) ? XCDR (prevtail) : watch_list;
	  for (Lisp_Object watches = XCDR (XCAR (tail)); ! NILP (watches);
	       watches = XCDR (watches))
            {
              event.arg = inotifyevent_to_event (XCAR (watches), ev);
              if (!NILP (event.arg))
                kbd_buffer_store_event (&event);
            }
          /* If event was removed automatically: Drop it from watch list.  */
          if (ev->mask & IN_IGNORED)
	    remove_descriptor (prevtail, true);
        }
      i += sizeof (*ev) + ev->len;
    }

  SAFE_FREE ();
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

ASPECT can also contain the following symbols, which control whether
the watch descriptor will be created:

dont-follow
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

See inotify(7) and inotify_add_watch(2) for further information.  The
inotify fd is managed internally and there is no corresponding
inotify_init.  Use `inotify-rm-watch' to remove a watch.

The following inotify bit-masks cannot be used because descriptors are
shared across different callers.

IN_EXCL_UNLINK
IN_MASK_ADD
IN_ONESHOT  */)
     (Lisp_Object filename, Lisp_Object aspect, Lisp_Object callback)
{
  Lisp_Object encoded_file_name;
  int wd = -1;
  uint32_t imask = aspect_to_inotifymask (aspect);
  uint32_t mask = imask | IN_MASK_ADD | IN_EXCL_UNLINK;

  CHECK_STRING (filename);

  if (inotifyfd < 0)
    {
      inotifyfd = inotify_init1 (IN_NONBLOCK | IN_CLOEXEC);
      if (inotifyfd < 0)
	report_file_notify_error ("File watching is not available", Qnil);
      watch_list = Qnil;
      add_read_fd (inotifyfd, &inotify_callback, NULL);
    }

  encoded_file_name = ENCODE_FILE (filename);
  wd = inotify_add_watch (inotifyfd, SSDATA (encoded_file_name), mask);
  if (wd < 0)
    report_file_notify_error ("Could not add watch for file", filename);

  return add_watch (wd, filename, imask, callback);
}

static bool
valid_watch_descriptor (Lisp_Object wd)
{
  return (CONSP (wd)
	  && (RANGED_INTEGERP (0, XCAR (wd), INT_MAX)
	      || (CONSP (XCAR (wd))
		  && RANGED_INTEGERP ((MOST_POSITIVE_FIXNUM >> 16) + 1,
				      XCAR (XCAR (wd)), INT_MAX >> 16)
		  && RANGED_INTEGERP (0, XCDR (XCAR (wd)), (1 << 16) - 1)))
	  && NATNUMP (XCDR (wd)));
}

DEFUN ("inotify-rm-watch", Finotify_rm_watch, Sinotify_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `inotify-add-watch'.

See inotify_rm_watch(2) for more information.  */)
     (Lisp_Object watch_descriptor)
{

  Lisp_Object descriptor, id;

  if (! valid_watch_descriptor (watch_descriptor))
    report_file_notify_error ("Invalid descriptor ", watch_descriptor);

  descriptor = XCAR (watch_descriptor);
  id = XCDR (watch_descriptor);
  remove_watch (descriptor, id);

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
  if (! valid_watch_descriptor (watch_descriptor))
    return Qnil;
  Lisp_Object tail = assoc_no_quit (XCAR (watch_descriptor), watch_list);
  if (NILP (tail))
    return Qnil;
  Lisp_Object watch = assq_no_quit (XCDR (watch_descriptor), XCDR (tail));
  return ! NILP (watch) ? Qt : Qnil;
}

#ifdef INOTIFY_DEBUG
DEFUN ("inotify-watch-list", Finotify_watch_list, Sinotify_watch_list, 0, 0, 0,
       doc: /* Return a copy of the internal watch_list.  */)
{
  return Fcopy_sequence (watch_list);
}

DEFUN ("inotify-allocated-p", Finotify_allocated_p, Sinotify_allocated_p, 0, 0, 0,
       doc: /* Return non-nil, if a inotify instance is allocated.  */)
{
  return inotifyfd < 0 ? Qnil : Qt;
}
#endif

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
  DEFSYM (Qonlydir, "onlydir");		/* IN_ONLYDIR */

  DEFSYM (Qignored, "ignored");		/* IN_IGNORED */
  DEFSYM (Qisdir, "isdir");		/* IN_ISDIR */
  DEFSYM (Qq_overflow, "q-overflow");	/* IN_Q_OVERFLOW */
  DEFSYM (Qunmount, "unmount");		/* IN_UNMOUNT */

  defsubr (&Sinotify_add_watch);
  defsubr (&Sinotify_rm_watch);
  defsubr (&Sinotify_valid_p);

#ifdef INOTIFY_DEBUG
  defsubr (&Sinotify_watch_list);
  defsubr (&Sinotify_allocated_p);
#endif
  staticpro (&watch_list);

  Fprovide (intern_c_string ("inotify"), Qnil);
}

#endif /* HAVE_INOTIFY */
