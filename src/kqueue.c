/* Filesystem notifications support with kqueue API.

Copyright (C) 2015-2020 Free Software Foundation, Inc.

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

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <fcntl.h>
#include "lisp.h"
#include "keyboard.h"
#include "process.h"

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif /* HAVE_SYS_RESOURCE_H  */


/* File handle for kqueue.  */
static int kqueuefd = -1;

/* This is a list, elements are (DESCRIPTOR FILE FLAGS CALLBACK [DIRLIST]).  */
static Lisp_Object watch_list;

/* Generate a list from the directory_files_internal output.
   Items are (INODE FILE-NAME LAST-MOD LAST-STATUS-MOD SIZE).  */
static Lisp_Object
kqueue_directory_listing (Lisp_Object directory_files)
{
  Lisp_Object dl, result = Qnil;

  for (dl = directory_files; ! NILP (dl); dl = XCDR (dl)) {
    /* We ignore "." and "..".  */
    if ((strcmp (".", SSDATA (XCAR (XCAR (dl)))) == 0) ||
	(strcmp ("..", SSDATA (XCAR (XCAR (dl)))) == 0))
      continue;

    result = Fcons
      (list5 (/* inode.  */
	      Fnth (make_fixnum (11), XCAR (dl)),
	      /* filename.  */
	      XCAR (XCAR (dl)),
	      /* last modification time.  */
	      Fnth (make_fixnum (6), XCAR (dl)),
	      /* last status change time.  */
	      Fnth (make_fixnum (7), XCAR (dl)),
	      /* size.  */
	      Fnth (make_fixnum (8), XCAR (dl))),
       result);
  }
  return result;
}

/* Generate a file notification event.  */
static void
kqueue_generate_event (Lisp_Object watch_object, Lisp_Object actions,
		       Lisp_Object file, Lisp_Object file1)
{
  Lisp_Object flags, action, entry;
  struct input_event event;

  /* Check, whether all actions shall be monitored.  */
  flags = Fnth (make_fixnum (2), watch_object);
  action = actions;
  do {
    if (NILP (action))
      break;
    entry = XCAR (action);
    if (NILP (Fmember (entry, flags))) {
      action = XCDR (action);
      actions = Fdelq (entry, actions);
    } else
      action = XCDR (action);
  } while (1);

  /* Store it into the input event queue.  */
  if (! NILP (actions)) {
    EVENT_INIT (event);
    event.kind = FILE_NOTIFY_EVENT;
    event.frame_or_window = Qnil;
    event.arg = list2 (Fcons (XCAR (watch_object),
			      Fcons (actions,
				     NILP (file1)
				     ? list1 (file)
				     : list2 (file, file1))),
		       Fnth (make_fixnum (3), watch_object));
    kbd_buffer_store_event (&event);
  }
}

/* This compares two directory listings in case of a `write' event for
   a directory.  Generate resulting file notification events.  The old
   directory listing is retrieved from watch_object, it will be
   replaced by the new directory listing at the end of this
   function.  */
static void
kqueue_compare_dir_list (Lisp_Object watch_object)
{
  Lisp_Object dir, pending_dl, deleted_dl;
  Lisp_Object old_directory_files, old_dl, new_directory_files, new_dl, dl;

  dir = XCAR (XCDR (watch_object));
  pending_dl = Qnil;
  deleted_dl = Qnil;

  old_directory_files = Fnth (make_fixnum (4), watch_object);
  old_dl = kqueue_directory_listing (old_directory_files);

  /* When the directory is not accessible anymore, it has been deleted.  */
  if (NILP (Ffile_directory_p (dir))) {
    kqueue_generate_event (watch_object, Fcons (Qdelete, Qnil), dir, Qnil);
    return;
  }
  new_directory_files =
    directory_files_internal (dir, Qnil, Qnil, Qnil, true, Qnil);
  new_dl = kqueue_directory_listing (new_directory_files);

  /* Parse through the old list.  */
  dl = old_dl;
  while (1) {
    Lisp_Object old_entry, new_entry, dl1;
    if (NILP (dl))
      break;

    /* Search for an entry with the same inode.  */
    old_entry = XCAR (dl);
    new_entry = assq_no_quit (XCAR (old_entry), new_dl);
    if (! NILP (Fequal (old_entry, new_entry))) {
      /* Both entries are identical.  Nothing to do.  */
      new_dl = Fdelq (new_entry, new_dl);
      goto the_end;
    }

    /* Both entries have the same inode.  */
    if (! NILP (new_entry)) {
      /* Both entries have the same file name.  */
      if (strcmp (SSDATA (XCAR (XCDR (old_entry))),
		  SSDATA (XCAR (XCDR (new_entry)))) == 0) {
	/* Modification time has been changed, the file has been written.  */
	if (NILP (Fequal (Fnth (make_fixnum (2), old_entry),
			  Fnth (make_fixnum (2), new_entry))))
	  kqueue_generate_event
	    (watch_object, Fcons (Qwrite, Qnil), XCAR (XCDR (old_entry)), Qnil);
	/* Status change time has been changed, the file attributes
	   have changed.  */
	  if (NILP (Fequal (Fnth (make_fixnum (3), old_entry),
			    Fnth (make_fixnum (3), new_entry))))
	  kqueue_generate_event
	    (watch_object, Fcons (Qattrib, Qnil),
	     XCAR (XCDR (old_entry)), Qnil);

      } else {
	/* The file has been renamed.  */
	kqueue_generate_event
	  (watch_object, Fcons (Qrename, Qnil),
	   XCAR (XCDR (old_entry)), XCAR (XCDR (new_entry)));
	deleted_dl = Fcons (new_entry, deleted_dl);
      }
      new_dl = Fdelq (new_entry, new_dl);
      goto the_end;
    }

    /* Search, whether there is a file with the same name but another
       inode.  */
    for (dl1 = new_dl; ! NILP (dl1); dl1 = XCDR (dl1)) {
      new_entry = XCAR (dl1);
      if (strcmp (SSDATA (XCAR (XCDR (old_entry))),
		  SSDATA (XCAR (XCDR (new_entry)))) == 0) {
	pending_dl = Fcons (new_entry, pending_dl);
	new_dl = Fdelq (new_entry, new_dl);
	goto the_end;
      }
    }

    /* Check, whether this a pending file.  */
    new_entry = assq_no_quit (XCAR (old_entry), pending_dl);

    if (NILP (new_entry)) {
      /* Check, whether this is an already deleted file (by rename).  */
      for (dl1 = deleted_dl; ! NILP (dl1); dl1 = XCDR (dl1)) {
	new_entry = XCAR (dl1);
	if (strcmp (SSDATA (XCAR (XCDR (old_entry))),
		    SSDATA (XCAR (XCDR (new_entry)))) == 0) {
	  deleted_dl = Fdelq (new_entry, deleted_dl);
	  goto the_end;
	}
      }
      /* The file has been deleted.  */
      kqueue_generate_event
	(watch_object, Fcons (Qdelete, Qnil), XCAR (XCDR (old_entry)), Qnil);

    } else {
      /* The file has been renamed.  */
      kqueue_generate_event
	(watch_object, Fcons (Qrename, Qnil),
	 XCAR (XCDR (old_entry)), XCAR (XCDR (new_entry)));
      pending_dl = Fdelq (new_entry, pending_dl);
    }

  the_end:
    dl = XCDR (dl);
    old_dl = Fdelq (old_entry, old_dl);
  }

  /* Parse through the resulting new list.  */
  dl = new_dl;
  while (1) {
    Lisp_Object entry;
    if (NILP (dl))
      break;

    /* A new file has appeared.  */
    entry = XCAR (dl);
    kqueue_generate_event
      (watch_object, Fcons (Qcreate, Qnil), XCAR (XCDR (entry)), Qnil);

    /* Check size of that file.  */
    Lisp_Object size = Fnth (make_fixnum (4), entry);
    if (FLOATP (size) || (XFIXNUM (size) > 0))
      kqueue_generate_event
	(watch_object, Fcons (Qwrite, Qnil), XCAR (XCDR (entry)), Qnil);

    dl = XCDR (dl);
    new_dl = Fdelq (entry, new_dl);
  }

  /* Parse through the resulting pending_dl list.  */
  dl = pending_dl;
  while (1) {
    Lisp_Object entry;
    if (NILP (dl))
      break;

    /* A file is still pending.  Assume it was a write.  */
    entry = XCAR (dl);
    kqueue_generate_event
      (watch_object, Fcons (Qwrite, Qnil), XCAR (XCDR (entry)), Qnil);

    dl = XCDR (dl);
    pending_dl = Fdelq (entry, pending_dl);
  }

  /* At this point, old_dl, new_dl and pending_dl shall be empty.
     deleted_dl might not be empty when there was a rename to a
     nonexistent file.  Let's make a check for this (might be removed
     once the code is stable).  */
  if (! NILP (old_dl))
    report_file_error ("Old list not empty", old_dl);
  if (! NILP (new_dl))
    report_file_error ("New list not empty", new_dl);
  if (! NILP (pending_dl))
    report_file_error ("Pending events list not empty", pending_dl);

  /* Replace old directory listing with the new one.  */
  XSETCDR (Fnthcdr (make_fixnum (3), watch_object),
	   Fcons (new_directory_files, Qnil));
  return;
}

/* This is the callback function for arriving input on kqueuefd.  It
   shall create a Lisp event, and put it into the Emacs input queue.  */
static void
kqueue_callback (int fd, void *data)
{
  for (;;) {
    struct kevent kev;
    static const struct timespec nullts = { 0, 0 };
    Lisp_Object descriptor, watch_object, file, actions;

    /* Read one event.  */
    int ret = kevent (kqueuefd, NULL, 0, &kev, 1, &nullts);
    if (ret < 1) {
      /* All events read.  */
      return;
    }

    /* Determine descriptor and file name.  */
    descriptor = make_fixnum (kev.ident);
    watch_object = assq_no_quit (descriptor, watch_list);
    if (CONSP (watch_object))
      file = XCAR (XCDR (watch_object));
    else
      continue;

    /* Determine event actions.  */
    actions = Qnil;
    if (kev.fflags & NOTE_DELETE)
      actions = Fcons (Qdelete, actions);
    if (kev.fflags & NOTE_WRITE) {
      /* Check, whether this is a directory event.  */
      if (NILP (Fnth (make_fixnum (4), watch_object)))
	actions = Fcons (Qwrite, actions);
      else
	kqueue_compare_dir_list (watch_object);
    }
    if (kev.fflags & NOTE_EXTEND)
      actions = Fcons (Qextend, actions);
    if (kev.fflags & NOTE_ATTRIB)
      actions = Fcons (Qattrib, actions);
    if (kev.fflags & NOTE_LINK)
      actions = Fcons (Qlink, actions);
    /* It would be useful to know the target of the rename operation.
       At this point, it is not possible.  Happens only when the upper
       directory is monitored.  */
    if (kev.fflags & NOTE_RENAME)
      actions = Fcons (Qrename, actions);

    /* Create the event.  */
    if (! NILP (actions))
      kqueue_generate_event (watch_object, actions, file, Qnil);

    /* Cancel monitor if file or directory is deleted or renamed.  */
    if (kev.fflags & (NOTE_DELETE | NOTE_RENAME))
      Fkqueue_rm_watch (descriptor);
  }
  return;
}

DEFUN ("kqueue-add-watch", Fkqueue_add_watch, Skqueue_add_watch, 3, 3, 0,
       doc: /* Add a watch for filesystem events pertaining to FILE.

This arranges for filesystem events pertaining to FILE to be reported
to Emacs.  Use `kqueue-rm-watch' to cancel the watch.

Returned value is a descriptor for the added watch.  If the file cannot be
watched for some reason, this function signals a `file-notify-error' error.

FLAGS is a list of events to be watched for.  It can include the
following symbols:

  `create' -- FILE was created
  `delete' -- FILE was deleted
  `write'  -- FILE has changed
  `extend' -- FILE was extended
  `attrib' -- a FILE attribute was changed
  `link'   -- a FILE's link count was changed
  `rename' -- FILE was moved to FILE1

When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTIONS FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTIONS is a list of events.

FILE is the name of the file whose event is being reported.  FILE1
will be reported only in case of the `rename' event.  This is possible
only when the upper directory of the renamed file is watched.  */)
  (Lisp_Object file, Lisp_Object flags, Lisp_Object callback)
{
  Lisp_Object watch_object, dir_list;
  int maxfd, fd, oflags;
  u_short fflags = 0;
  struct kevent kev;
#ifdef HAVE_GETRLIMIT
  struct rlimit rlim;
#endif /* HAVE_GETRLIMIT  */

  /* Check parameters.  */
  CHECK_STRING (file);
  file = Fdirectory_file_name (Fexpand_file_name (file, Qnil));
  if (NILP (Ffile_exists_p (file)))
    report_file_error ("File does not exist", file);

  CHECK_LIST (flags);

  if (! FUNCTIONP (callback))
    wrong_type_argument (Qinvalid_function, callback);

  /* Check available file descriptors.  */
#ifdef HAVE_GETRLIMIT
  if (! getrlimit (RLIMIT_NOFILE, &rlim))
    maxfd = rlim.rlim_cur;
  else
#endif /* HAVE_GETRLIMIT  */
    maxfd = 256;

  /* We assume 50 file descriptors are sufficient for the rest of Emacs.  */
  ptrdiff_t watch_list_len = list_length (watch_list);
  if (maxfd - 50 < watch_list_len)
    xsignal2
      (Qfile_notify_error,
       build_string ("File watching not possible, no file descriptor left"),
       make_fixnum (watch_list_len));

  if (kqueuefd < 0)
    {
      /* Create kqueue descriptor.  */
      kqueuefd = kqueue ();
      if (kqueuefd < 0)
	report_file_notify_error ("File watching is not available", Qnil);

      /* Start monitoring for possible I/O.  */
      add_read_fd (kqueuefd, kqueue_callback, NULL);

      watch_list = Qnil;
    }

  /* Open file.  */
  Lisp_Object encoded_file = ENCODE_FILE (file);
  oflags = O_NONBLOCK;
#if O_EVTONLY
  oflags |= O_EVTONLY;
#else
  oflags |= O_RDONLY;
#endif
#if O_SYMLINK
    oflags |= O_SYMLINK;
#else
    oflags |= O_NOFOLLOW;
#endif
  fd = emacs_open (SSDATA (encoded_file), oflags, 0);
  if (fd == -1)
    report_file_error ("File cannot be opened", file);

  /* Assemble filter flags  */
  if (! NILP (Fmember (Qdelete, flags))) fflags |= NOTE_DELETE;
  if (! NILP (Fmember (Qwrite, flags)))  fflags |= NOTE_WRITE;
  if (! NILP (Fmember (Qextend, flags))) fflags |= NOTE_EXTEND;
  if (! NILP (Fmember (Qattrib, flags))) fflags |= NOTE_ATTRIB;
  if (! NILP (Fmember (Qlink, flags)))   fflags |= NOTE_LINK;
  if (! NILP (Fmember (Qrename, flags))) fflags |= NOTE_RENAME;

  /* Register event.  */
  EV_SET (&kev, fd, EVFILT_VNODE, EV_ADD | EV_ENABLE | EV_CLEAR,
	  fflags, 0, NULL);

  if (kevent (kqueuefd, &kev, 1, NULL, 0, NULL) < 0) {
    emacs_close (fd);
    report_file_error ("Cannot watch file", file);
  }

  /* Store watch object in watch list.  */
  Lisp_Object watch_descriptor = make_fixnum (fd);
  if (NILP (Ffile_directory_p (file)))
    watch_object = list4 (watch_descriptor, file, flags, callback);
  else {
    dir_list = directory_files_internal (file, Qnil, Qnil, Qnil, true, Qnil);
    watch_object = list5 (watch_descriptor, file, flags, callback, dir_list);
  }
  watch_list = Fcons (watch_object, watch_list);

  return watch_descriptor;
}

DEFUN ("kqueue-rm-watch", Fkqueue_rm_watch, Skqueue_rm_watch, 1, 1, 0,
       doc: /* Remove an existing WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `kqueue-add-watch'.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = assq_no_quit (watch_descriptor, watch_list);

  if (! CONSP (watch_object))
    xsignal2 (Qfile_notify_error, build_string ("Not a watch descriptor"),
	      watch_descriptor);

  eassert (FIXNUMP (watch_descriptor));
  int fd = XFIXNUM (watch_descriptor);
  if ( fd >= 0)
    emacs_close (fd);

  /* Remove watch descriptor from watch list.  */
  watch_list = Fdelq (watch_object, watch_list);

  if (NILP (watch_list) && (kqueuefd >= 0)) {
    delete_read_fd (kqueuefd);
    emacs_close (kqueuefd);
    kqueuefd = -1;
  }

  return Qt;
}

DEFUN ("kqueue-valid-p", Fkqueue_valid_p, Skqueue_valid_p, 1, 1, 0,
       doc: /* Check a watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `kqueue-add-watch'.

A watch can become invalid if the file or directory it watches is
deleted, or if the watcher thread exits abnormally for any other
reason.  Removing the watch by calling `kqueue-rm-watch' also makes it
invalid.  */)
     (Lisp_Object watch_descriptor)
{
  return NILP (assq_no_quit (watch_descriptor, watch_list)) ? Qnil : Qt;
}


void
globals_of_kqueue (void)
{
  watch_list = Qnil;
}

void
syms_of_kqueue (void)
{
  defsubr (&Skqueue_add_watch);
  defsubr (&Skqueue_rm_watch);
  defsubr (&Skqueue_valid_p);

  /* Event types.  */
  DEFSYM (Qcreate, "create");
  DEFSYM (Qdelete, "delete");	/* NOTE_DELETE  */
  DEFSYM (Qwrite, "write");	/* NOTE_WRITE  */
  DEFSYM (Qextend, "extend");	/* NOTE_EXTEND  */
  DEFSYM (Qattrib, "attrib");	/* NOTE_ATTRIB  */
  DEFSYM (Qlink, "link");	/* NOTE_LINK  */
  DEFSYM (Qrename, "rename");	/* NOTE_RENAME  */

  staticpro (&watch_list);

  Fprovide (intern_c_string ("kqueue"), Qnil);
}

/* PROBLEMS
   * https://bugs.launchpad.net/ubuntu/+source/libkqueue/+bug/1514837
     prevents tests on Ubuntu.  */
