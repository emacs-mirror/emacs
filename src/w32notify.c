/* Filesystem notifications support for GNU Emacs on the Microsoft Windows API.
   Copyright (C) 2012-2016 Free Software Foundation, Inc.

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

/* Written by Eli Zaretskii <eliz@gnu.org>.

   Design overview:

   For each watch request, we launch a separate worker thread.  The
   worker thread runs the watch_worker function, which issues an
   asynchronous call to ReadDirectoryChangesW, and then waits in
   SleepEx for that call to complete.  Waiting in SleepEx puts the
   thread in an "alertable" state, so it wakes up when either (a) the
   call to ReadDirectoryChangesW completes, or (b) the main thread
   instructs the worker thread to terminate by sending it an APC, see
   below.

   When the ReadDirectoryChangesW call completes, its completion
   routine watch_completion is automatically called.  watch_completion
   stashes the received file events in a buffer used to communicate
   them to the main thread (using a critical section, so that several
   threads could use the same buffer), posts a special message,
   WM_EMACS_FILENOTIFY, to the Emacs's message queue, and returns.
   That causes the SleepEx function call inside watch_worker to
   return, and watch_worker then issues another call to
   ReadDirectoryChangesW.  (Except when it does not, see below.)

   In a GUI session, the WM_EMACS_FILENOTIFY message posted to the
   message queue gets dispatched to the main Emacs window procedure,
   which queues it for processing by w32_read_socket.  When
   w32_read_socket sees this message, it accesses the buffer with file
   notifications (using a critical section), extracts the information,
   converts it to a series of FILE_NOTIFY_EVENT events, and stuffs
   them into the input event queue to be processed by keyboard.c input
   machinery (read_char via a call to kbd_buffer_get_event).

   In a non-GUI session, we send the WM_EMACS_FILENOTIFY message to
   the main (a.k.a. "Lisp") thread instead, since there are no window
   procedures in console programs.  That message wakes up
   MsgWaitForMultipleObjects inside sys_select, which then signals to
   its caller that some keyboard input is available.  This causes
   w32_console_read_socket to be called, which accesses the buffer
   with file notifications and stuffs them into the input event queue
   for keyboard.c to process.

   When the FILE_NOTIFY_EVENT event is processed by keyboard.c's
   kbd_buffer_get_event, it is converted to a Lispy event that can be
   bound to a command.  The default binding is file-notify-handle-event,
   defined on subr.el.

   After w32_read_socket or w32_console_read_socket are done
   processing the notifications, they reset a flag signaling to all
   watch worker threads that the notifications buffer is available for
   more input.

   When the watch is removed by a call to w32notify-rm-watch, the main
   thread requests that the worker thread terminates by queuing an APC
   for the worker thread.  The APC specifies the watch_end function to
   be called.  watch_end calls CancelIo on the outstanding
   ReadDirectoryChangesW call and closes the handle on which the
   watched directory was open.  When watch_end returns, the
   watch_completion function is called one last time with the
   ERROR_OPERATION_ABORTED status, which causes it to clean up and set
   a flag telling watch_worker to exit without issuing another
   ReadDirectoryChangesW call.  Since watch_worker is the thread
   procedure of the worker thread, exiting it causes the thread to
   exit.  The main thread waits for some time for the worker thread to
   exit, and if it doesn't, terminates it forcibly.  */

#include <stddef.h>
#include <errno.h>

/* must include CRT headers *before* config.h */
#include <config.h>

#include <windows.h>

#include "lisp.h"
#include "w32term.h"	/* for enter_crit/leave_crit and WM_EMACS_FILENOTIFY */
#include "w32common.h"	/* for OS version data */
#include "w32.h"	/* for w32_strerror */
#include "coding.h"
#include "keyboard.h"
#include "frame.h"	/* needed by termhooks.h */
#include "termhooks.h"	/* for FILE_NOTIFY_EVENT */

#define DIRWATCH_SIGNATURE 0x01233210

struct notification {
  BYTE *buf;		/* buffer for ReadDirectoryChangesW */
  OVERLAPPED *io_info;	/* the OVERLAPPED structure for async I/O */
  BOOL subtree;		/* whether to watch subdirectories */
  DWORD filter;		/* bit mask for events to watch */
  char *watchee;	/* the file we are interested in, UTF-8 encoded */
  HANDLE dir;		/* handle to the watched directory */
  HANDLE thr;		/* handle to the thread that watches */
  volatile int terminate; /* if non-zero, request for the thread to terminate */
  unsigned signature;
};

/* Used for communicating notifications to the main thread.  */
volatile int notification_buffer_in_use;
BYTE file_notifications[16384];
DWORD notifications_size;
void *notifications_desc;

static Lisp_Object watch_list;

/* Signal to the main thread that we have file notifications for it to
   process.  */
static void
send_notifications (BYTE *info, DWORD info_size, void *desc,
		    volatile int *terminate)
{
  int done = 0;
  struct frame *f = SELECTED_FRAME ();

  /* A single buffer is used to communicate all notifications to the
     main thread.  Since both the main thread and several watcher
     threads could be active at the same time, we use a critical area
     and an "in-use" flag to synchronize them.  A watcher thread can
     only put its notifications in the buffer if it acquires the
     critical area and finds the "in-use" flag reset.  The main thread
     resets the flag after it is done processing notifications.

     FIXME: is there a better way of dealing with this?  */
  while (!done && !*terminate)
    {
      enter_crit ();
      if (!notification_buffer_in_use)
	{
	  if (info_size)
	    memcpy (file_notifications, info,
		    min (info_size, sizeof (file_notifications)));
	  notifications_size = min (info_size, sizeof (file_notifications));
	  notifications_desc = desc;
	  /* If PostMessage fails, the message queue is full.  If that
	     happens, the last thing they will worry about is file
	     notifications.  So we effectively discard the
	     notification in that case.  */
	  if ((FRAME_TERMCAP_P (f)
	       /* We send the message to the main (a.k.a. "Lisp")
		  thread, where it will wake up MsgWaitForMultipleObjects
		  inside sys_select, causing it to report that there's
		  some keyboard input available.  This will in turn cause
		  w32_console_read_socket to be called, which will pick
		  up the file notifications.  */
	       && PostThreadMessage (dwMainThreadId, WM_EMACS_FILENOTIFY, 0, 0))
	      || (FRAME_W32_P (f)
		  && PostMessage (FRAME_W32_WINDOW (f),
				  WM_EMACS_FILENOTIFY, 0, 0))
	      /* When we are running in batch mode, there's no one to
		 send a message, so we just signal the data is
		 available and hope sys_select will be called soon and
		 will read the data.  */
	      || (FRAME_INITIAL_P (f) && noninteractive))
	    notification_buffer_in_use = 1;
	  done = 1;
	}
      leave_crit ();
      if (!done)
	Sleep (5);
    }
}

/* An APC routine to cancel outstanding directory watch.  Invoked by
   the main thread via QueueUserAPC.  This is needed because only the
   thread that issued the ReadDirectoryChangesW call can call CancelIo
   to cancel that.  (CancelIoEx is only available since Vista, so we
   cannot use it on XP.)  */
VOID CALLBACK
watch_end (ULONG_PTR arg)
{
  HANDLE hdir = (HANDLE)arg;

  if (hdir && hdir != INVALID_HANDLE_VALUE)
    {
      CancelIo (hdir);
      CloseHandle (hdir);
    }
}

/* A completion routine (a.k.a. "APC function") for handling events
   read by ReadDirectoryChangesW.  Called by the OS when the thread
   which issued the asynchronous ReadDirectoryChangesW call is in the
   "alertable state", i.e. waiting inside SleepEx call.  */
VOID CALLBACK
watch_completion (DWORD status, DWORD bytes_ret, OVERLAPPED *io_info)
{
  struct notification *dirwatch;

  /* Who knows what happened?  Perhaps the OVERLAPPED structure was
     freed by someone already?  In any case, we cannot do anything
     with this request, so just punt and skip it.  FIXME: should we
     raise the 'terminate' flag in this case?  */
  if (!io_info)
    return;

  /* We have a pointer to our dirwatch structure conveniently stashed
     away in the hEvent member of the OVERLAPPED struct.  According to
     MSDN documentation of ReadDirectoryChangesW: "The hEvent member
     of the OVERLAPPED structure is not used by the system, so you can
     use it yourself."  */
  dirwatch = (struct notification *)io_info->hEvent;
  if (status == ERROR_OPERATION_ABORTED)
    {
      /* We've been called because the main thread told us to issue
	 CancelIo on the directory we watch, and watch_end did so.
	 The directory handle is already closed.  We should clean up
	 and exit, signaling to the thread worker routine not to
	 issue another call to ReadDirectoryChangesW.  Note that we
	 don't free the dirwatch object itself nor the memory consumed
	 by its buffers; this is done by the main thread in
	 remove_watch.  Calling malloc/free from a thread other than
	 the main thread is a no-no.  */
      dirwatch->dir = NULL;
      dirwatch->terminate = 1;
    }
  else
    {
      /* Tell the main thread we have notifications for it.  */
      send_notifications (dirwatch->buf, bytes_ret, dirwatch,
			  &dirwatch->terminate);
    }
}

/* Worker routine for the watch thread.  */
static DWORD WINAPI
watch_worker (LPVOID arg)
{
  struct notification *dirwatch = (struct notification *)arg;

  do {
    BOOL status;
    DWORD bytes_ret = 0;

    if (dirwatch->dir)
      {
	status = ReadDirectoryChangesW (dirwatch->dir, dirwatch->buf, 16384,
					dirwatch->subtree, dirwatch->filter,
					&bytes_ret,
					dirwatch->io_info, watch_completion);
	if (!status)
	  {
	    DebPrint (("watch_worker, abnormal exit: %lu\n", GetLastError ()));
	    /* We cannot remove the dirwatch object from watch_list,
	       because we are in a separate thread.  For the same
	       reason, we also cannot free memory consumed by the
	       buffers allocated for the dirwatch object.  So we close
	       the directory handle, but do not free the object itself
	       or its buffers.  We also don't touch the signature.
	       This way, remove_watch can still identify the object,
	       remove it, and free its memory.  */
	    CloseHandle (dirwatch->dir);
	    dirwatch->dir = NULL;
	    return 1;
	  }
      }
    /* Sleep indefinitely until awoken by the I/O completion, which
       could be either a change notification or a cancellation of the
       watch.  */
    SleepEx (INFINITE, TRUE);
  } while (!dirwatch->terminate);

  return 0;
}

/* Launch a thread to watch changes to FILE in a directory open on
   handle HDIR.  */
static struct notification *
start_watching (const char *file, HANDLE hdir, BOOL subdirs, DWORD flags)
{
  struct notification *dirwatch = xzalloc (sizeof (struct notification));

  dirwatch->signature = DIRWATCH_SIGNATURE;
  dirwatch->buf = xmalloc (16384);
  dirwatch->io_info = xzalloc (sizeof(OVERLAPPED));
  /* Stash a pointer to dirwatch structure for use by the completion
     routine.  According to MSDN documentation of ReadDirectoryChangesW:
     "The hEvent member of the OVERLAPPED structure is not used by the
     system, so you can use it yourself." */
  dirwatch->io_info->hEvent = dirwatch;
  dirwatch->subtree = subdirs;
  dirwatch->filter = flags;
  dirwatch->watchee = xstrdup (file);
  dirwatch->terminate = 0;
  dirwatch->dir = hdir;

  /* See w32proc.c where it calls CreateThread for the story behind
     the 2nd and 5th argument in the call to CreateThread.  */
  dirwatch->thr = CreateThread (NULL, 64 * 1024, watch_worker, (void *)dirwatch,
				0x00010000, NULL);

  if (!dirwatch->thr)
    {
      xfree (dirwatch->buf);
      xfree (dirwatch->io_info);
      xfree (dirwatch->watchee);
      xfree (dirwatch);
      dirwatch = NULL;
    }
  return dirwatch;
}

/* Called from the main thread to start watching FILE in PARENT_DIR,
   subject to FLAGS.  If SUBDIRS is TRUE, watch the subdirectories of
   PARENT_DIR as well.  Value is a pointer to 'struct notification'
   used by the thread that watches the changes.  */
static struct notification *
add_watch (const char *parent_dir, const char *file, BOOL subdirs, DWORD flags)
{
  HANDLE hdir;
  struct notification *dirwatch = NULL;

  if (!file)
    return NULL;

  if (w32_unicode_filenames)
    {
      wchar_t dir_w[MAX_PATH], file_w[MAX_PATH];

      filename_to_utf16 (parent_dir, dir_w);
      if (*file)
	filename_to_utf16 (file, file_w);
      else
	file_w[0] = 0;

      hdir = CreateFileW (dir_w,
			  FILE_LIST_DIRECTORY,
			  /* FILE_SHARE_DELETE doesn't preclude other
			     processes from deleting files inside
			     parent_dir.  */
			  FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,
			  NULL, OPEN_EXISTING,
			  FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
			  NULL);
    }
  else
    {
      char dir_a[MAX_PATH], file_a[MAX_PATH];

      filename_to_ansi (parent_dir, dir_a);
      if (*file)
	filename_to_ansi (file, file_a);
      else
	file_a[0] = '\0';

      hdir = CreateFileA (dir_a,
			  FILE_LIST_DIRECTORY,
			  FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,
			  NULL, OPEN_EXISTING,
			  FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
			  NULL);
    }
  if (hdir == INVALID_HANDLE_VALUE)
    return NULL;

  if ((dirwatch = start_watching (file, hdir, subdirs, flags)) == NULL)
    CloseHandle (hdir);

  return dirwatch;
}

/* Stop watching a directory specified by a pointer to its dirwatch object.  */
static int
remove_watch (struct notification *dirwatch)
{
  if (dirwatch && dirwatch->signature == DIRWATCH_SIGNATURE)
    {
      int i;
      BOOL status;
      DWORD exit_code, err;

      /* Only the thread that issued the outstanding I/O call can call
	 CancelIo on it.  (CancelIoEx is available only since Vista.)
	 So we need to queue an APC for the worker thread telling it
	 to terminate.  */
      if (!QueueUserAPC (watch_end, dirwatch->thr, (ULONG_PTR)dirwatch->dir))
	DebPrint (("QueueUserAPC failed (%lu)!\n", GetLastError ()));
      /* We also set the terminate flag, for when the thread is
	 waiting on the critical section that never gets acquired.
	 FIXME: is there a cleaner method?  Using SleepEx there is a
	 no-no, as that will lead to recursive APC invocations and
	 stack overflow.  */
      dirwatch->terminate = 1;
      /* Wait for the thread to exit.  FIXME: is there a better method
	 that is not overly complex?  */
      for (i = 0; i < 50; i++)
	{
	  if (!((status = GetExitCodeThread (dirwatch->thr, &exit_code))
		&& exit_code == STILL_ACTIVE))
	    break;
	  Sleep (10);
	}
      if ((status == FALSE && (err = GetLastError ()) == ERROR_INVALID_HANDLE)
	  || exit_code == STILL_ACTIVE)
	{
	  if (!(status == FALSE && err == ERROR_INVALID_HANDLE))
	    {
	      TerminateThread (dirwatch->thr, 0);
	      if (dirwatch->dir)
		CloseHandle (dirwatch->dir);
	    }
	}

      /* Clean up.  */
      if (dirwatch->thr)
	{
	  CloseHandle (dirwatch->thr);
	  dirwatch->thr = NULL;
	}
      xfree (dirwatch->buf);
      xfree (dirwatch->io_info);
      xfree (dirwatch->watchee);
      xfree (dirwatch);

      return 0;
    }
  else
    {
      DebPrint (("Unknown dirwatch object!\n"));
      return -1;
    }
}

static DWORD
filter_list_to_flags (Lisp_Object filter_list)
{
  DWORD flags = 0;

  if (NILP (filter_list))
    return flags;

  if (!NILP (Fmember (Qfile_name, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_FILE_NAME;
  if (!NILP (Fmember (Qdirectory_name, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_DIR_NAME;
  if (!NILP (Fmember (Qattributes, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if (!NILP (Fmember (Qsize, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_SIZE;
  if (!NILP (Fmember (Qlast_write_time, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_LAST_WRITE;
  if (!NILP (Fmember (Qlast_access_time, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if (!NILP (Fmember (Qcreation_time, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_CREATION;
  if (!NILP (Fmember (Qsecurity_desc, filter_list)))
    flags |= FILE_NOTIFY_CHANGE_SECURITY;

  return flags;
}

DEFUN ("w32notify-add-watch", Fw32notify_add_watch,
       Sw32notify_add_watch, 3, 3, 0,
       doc: /* Add a watch for filesystem events pertaining to FILE.

This arranges for filesystem events pertaining to FILE to be reported
to Emacs.  Use `w32notify-rm-watch' to cancel the watch.

Value is a descriptor for the added watch.  If the file cannot be
watched for some reason, this function signals a `file-error' error.

FILTER is a list of conditions for reporting an event.  It can include
the following symbols:

  'file-name'          -- report file creation, deletion, or renaming
  'directory-name'     -- report directory creation, deletion, or renaming
  'attributes'         -- report changes in attributes
  'size'               -- report changes in file-size
  'last-write-time'    -- report changes in last-write time
  'last-access-time'   -- report changes in last-access time
  'creation-time'      -- report changes in creation time
  'security-desc'      -- report changes in security descriptor

If FILE is a directory, and FILTER includes 'subtree', then all the
subdirectories will also be watched and changes in them reported.

When any event happens that satisfies the conditions specified by
FILTER, Emacs will call the CALLBACK function passing it a single
argument EVENT, which is of the form

  (DESCRIPTOR ACTION FILE)

DESCRIPTOR is the same object as the one returned by this function.
ACTION is the description of the event.  It could be any one of the
following:

  'added'        -- FILE was added
  'removed'      -- FILE was deleted
  'modified'     -- FILE's contents or its attributes were modified
  'renamed-from' -- a file was renamed whose old name was FILE
  'renamed-to'   -- a file was renamed and its new name is FILE

FILE is the name of the file whose event is being reported.

Note that some networked filesystems, such as Samba-mounted Unix
volumes, might not send notifications about file changes.  In these
cases, this function will return a valid descriptor, but notifications
will never come in.  Volumes shared from remote Windows machines do
generate notifications correctly, though.  */)
  (Lisp_Object file, Lisp_Object filter, Lisp_Object callback)
{
  Lisp_Object dirfn, basefn, watch_object, watch_descriptor;
  DWORD flags;
  BOOL subdirs = FALSE;
  struct notification *dirwatch = NULL;
  Lisp_Object lisp_errstr;
  char *errstr;

  CHECK_LIST (filter);

  /* The underlying features are available only since XP.  */
  if (os_subtype == OS_9X
      || (w32_major_version == 5 && w32_minor_version < 1))
    {
      errno = ENOSYS;
      report_file_notify_error ("Watching filesystem events is not supported",
				Qnil);
    }

  /* filenotify.el always passes us a directory, either the parent
     directory of a file to be watched, or the directory to be
     watched.  */
  file = Fdirectory_file_name (Fexpand_file_name (file, Qnil));
  if (NILP (Ffile_directory_p (file)))
    {
      /* This should only happen if we are called directly, not via
	 filenotify.el.  If BASEFN is empty, the argument was the root
	 directory on its drive.  */
      dirfn = ENCODE_FILE (Ffile_name_directory (file));
      basefn = ENCODE_FILE (Ffile_name_nondirectory (file));
      if (*SDATA (basefn) == '\0')
	subdirs = TRUE;
    }
  else
    {
      dirfn = ENCODE_FILE (file);
      basefn = Qnil;
    }

  if (!NILP (Fmember (Qsubtree, filter)))
    subdirs = TRUE;

  flags = filter_list_to_flags (filter);

  dirwatch = add_watch (SSDATA (dirfn), NILP (basefn) ? "" : SSDATA (basefn),
			subdirs, flags);
  if (!dirwatch)
    {
      DWORD err = GetLastError ();

      errno = EINVAL;
      if (err)
	{
	  errstr = w32_strerror (err);
	  if (!NILP (Vlocale_coding_system))
	    lisp_errstr
	      = code_convert_string_norecord (build_unibyte_string (errstr),
					      Vlocale_coding_system, 0);
	  else
	    lisp_errstr = build_string (errstr);
	  report_file_notify_error ("Cannot watch file",
				    Fcons (lisp_errstr, Fcons (file, Qnil)));
	}
      else
	report_file_notify_error ("Cannot watch file", Fcons (file, Qnil));
    }
  /* Store watch object in watch list. */
  watch_descriptor = make_pointer_integer (dirwatch);
  watch_object = Fcons (watch_descriptor, callback);
  watch_list = Fcons (watch_object, watch_list);

  return watch_descriptor;
}

DEFUN ("w32notify-rm-watch", Fw32notify_rm_watch,
       Sw32notify_rm_watch, 1, 1, 0,
       doc: /* Remove an existing watch specified by its WATCH-DESCRIPTOR.

WATCH-DESCRIPTOR should be an object returned by `w32notify-add-watch'.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object;
  struct notification *dirwatch;
  int status = -1;

  /* Remove the watch object from watch list.  Do this before freeing
     the object, do that even if we fail to free it, watch_list is
     kept free of junk.  */
  watch_object = Fassoc (watch_descriptor, watch_list);
  if (!NILP (watch_object))
    {
      watch_list = Fdelete (watch_object, watch_list);
      dirwatch = (struct notification *)XINTPTR (watch_descriptor);
      if (w32_valid_pointer_p (dirwatch, sizeof(struct notification)))
	status = remove_watch (dirwatch);
    }

  if (status == -1)
    report_file_notify_error ("Invalid watch descriptor",
			      Fcons (watch_descriptor, Qnil));

  return Qnil;
}

Lisp_Object
w32_get_watch_object (void *desc)
{
  Lisp_Object descriptor = make_pointer_integer (desc);

  /* This is called from the input queue handling code, inside a
     critical section, so we cannot possibly QUIT if watch_list is not
     in the right condition.  */
  return NILP (watch_list) ? Qnil : assoc_no_quit (descriptor, watch_list);
}

DEFUN ("w32notify-valid-p", Fw32notify_valid_p, Sw32notify_valid_p, 1, 1, 0,
       doc: /* "Check a watch specified by its WATCH-DESCRIPTOR for validity.

WATCH-DESCRIPTOR should be an object returned by `w32notify-add-watch'.

A watch can become invalid if the directory it watches is deleted, or if
the watcher thread exits abnormally for any other reason.  Removing the
watch by calling `w32notify-rm-watch' also makes it invalid.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = Fassoc (watch_descriptor, watch_list);

  if (!NILP (watch_object))
    {
      struct notification *dirwatch =
	(struct notification *)XINTPTR (watch_descriptor);
      if (w32_valid_pointer_p (dirwatch, sizeof(struct notification))
	  && dirwatch->dir != NULL)
	return Qt;
    }

  return Qnil;
}

void
globals_of_w32notify (void)
{
  watch_list = Qnil;
}

void
syms_of_w32notify (void)
{
  DEFSYM (Qfile_name, "file-name");
  DEFSYM (Qdirectory_name, "directory-name");
  DEFSYM (Qattributes, "attributes");
  DEFSYM (Qlast_write_time, "last-write-time");
  DEFSYM (Qlast_access_time, "last-access-time");
  DEFSYM (Qcreation_time, "creation-time");
  DEFSYM (Qsecurity_desc, "security-desc");
  DEFSYM (Qsubtree, "subtree");

  defsubr (&Sw32notify_add_watch);
  defsubr (&Sw32notify_rm_watch);
  defsubr (&Sw32notify_valid_p);

  staticpro (&watch_list);

  Fprovide (intern_c_string ("w32notify"), Qnil);
}
