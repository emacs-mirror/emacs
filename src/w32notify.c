/* Filesystem notifications support for GNU Emacs on the Microsoft Windows API.
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

/* Written by Eli Zaretskii <eliz@gnu.org>.

   Design overview:

   For each watch request, we launch a separate worker thread.  The
   worker thread runs the watch_worker function, which issues an
   asynchronous call to ReadDirectoryChangesW, and then calls
   WaitForSingleObjectEx to wait that an event be signaled
   to terminate the thread.
   Waiting with WaitForSingleObjectEx puts the thread in an
   "alertable" state, so it wakes up when either (a) the call to
   ReadDirectoryChangesW completes, or (b) the main thread instructs
   the worker thread to terminate by signaling an event, see below.

   When the ReadDirectoryChangesW call completes, its completion
   routine watch_completion is automatically called.  watch_completion
   stashes the received file events in a linked list used to
   communicate them to the main thread (using a critical section, so
   that several threads could alter the same linked list), posts a
   special message, WM_EMACS_FILENOTIFY, to the Emacs's message queue,
   and returns.  That causes the WaitForSingleObjectEx function call
   inside watch_worker to return, but the thread won't terminate until
   the event telling to do so will be signaled.  The completion
   routine issued another call to ReadDirectoryChangesW as quickly as
   possible.  (Except when it does not, see below.)

   In a GUI session, the WM_EMACS_FILENOTIFY message posted to the
   message queue gets dispatched to the main Emacs window procedure,
   which queues it for processing by w32_read_socket.  When
   w32_read_socket sees this message, it accesses the linked list with file
   notifications (using a critical section), extracts the information,
   converts it to a series of FILE_NOTIFY_EVENT events, and stuffs
   them into the input event queue to be processed by keyboard.c input
   machinery (read_char via a call to kbd_buffer_get_event).

   In a non-GUI session, we send the WM_EMACS_FILENOTIFY message to
   the main (a.k.a. "Lisp") thread instead, since there are no window
   procedures in console programs.  That message wakes up
   MsgWaitForMultipleObjects inside sys_select, which then signals to
   its caller that some keyboard input is available.  This causes
   w32_console_read_socket to be called, which accesses the linked list
   with file notifications and stuffs them into the input event queue
   for keyboard.c to process.

   When the FILE_NOTIFY_EVENT event is processed by keyboard.c's
   kbd_buffer_get_event, it is converted to a Lispy event that can be
   bound to a command.  The default binding is file-notify-handle-event,
   defined on subr.el.

   Routines w32_read_socket or w32_console_read_socket process notifications
   sets as long as some are available.

   When the watch is removed by a call to w32notify-rm-watch, the main
   thread requests that the worker thread terminates by signaling the
   appropriate event and queuing an APC for the worker thread.  The
   APC specifies the watch_end function to be called.  watch_end calls
   CancelIo on the outstanding ReadDirectoryChangesW call.  When
   watch_end returns, the watch_completion function is called one last
   time with the ERROR_OPERATION_ABORTED status, which causes it to
   clean up and set a flag telling watch_worker to exit without
   issuing another ReadDirectoryChangesW call.  Since watch_worker is
   the thread procedure of the worker thread, exiting it causes the
   thread to exit.  The main thread waits for some time for the worker
   thread to exit, and if it doesn't, terminates it forcibly.  */

#define DEFER_MS_W32_H
#include <config.h>

#include <stddef.h>
#include <errno.h>

/* Include CRT headers *before* ms-w32.h.  */
#include <ms-w32.h>

#include <windows.h>

#include "lisp.h"
#include "w32term.h"	/* for enter_crit/leave_crit and WM_EMACS_FILENOTIFY */
#include "w32common.h"	/* for OS version data */
#include "w32.h"	/* for w32_strerror */
#include "coding.h"
#include "keyboard.h"
#include "frame.h"	/* needed by termhooks.h */
#include "termhooks.h"	/* for FILE_NOTIFY_EVENT */

#define DIRWATCH_BUFFER_SIZE 16384
#define DIRWATCH_SIGNATURE 0x01233210

struct notification {
  BYTE *buf;		/* buffer for ReadDirectoryChangesW */
  OVERLAPPED *io_info;	/* the OVERLAPPED structure for async I/O */
  BOOL subtree;		/* whether to watch subdirectories */
  DWORD filter;		/* bit mask for events to watch */
  char *watchee;	/* the file we are interested in, UTF-8 encoded */
  HANDLE dir;		/* handle to the watched directory */
  HANDLE thr;		/* handle to the thread that watches */
  HANDLE terminate;     /* event signaling the thread to terminate */
  unsigned signature;
};

/* Used for communicating notifications to the main thread.  */
struct notifications_set *notifications_set_head;

static Lisp_Object watch_list;

/* Signal to the main thread that we have file notifications for it to
   process.  */
static void
send_notifications (struct notifications_set *ns)
{
  struct frame *f = SELECTED_FRAME ();

  /* We add the current notification set to the linked list.  Use the
     critical section to make sure only one thread will access the
     linked list. */
      enter_crit ();
  ns->next = notifications_set_head;
  ns->prev = notifications_set_head->prev;
  ns->prev->next = ns;
  notifications_set_head->prev = ns;
  leave_crit();

  /* If PostMessage fails, the message queue is full.  If that
     happens, the last thing they will worry about is file
     notifications.  So we effectively discard the notification in
     that case.  */
  if (FRAME_TERMCAP_P (f))
    /* We send the message to the main (a.k.a. "Lisp") thread, where
       it will wake up MsgWaitForMultipleObjects inside sys_select,
       causing it to report that there's some keyboard input
       available.  This will in turn cause w32_console_read_socket to
       be called, which will pick up the file notifications.  */
    PostThreadMessage (dwMainThreadId, WM_EMACS_FILENOTIFY, 0, 0);
  else if (FRAME_W32_P (f))
    PostMessage (FRAME_W32_WINDOW (f),
                 WM_EMACS_FILENOTIFY, 0, 0);
  /* When we are running in batch mode, there's no one to send a
     message, so we just signal the data is available and hope
     sys_select will be called soon and will read the data.  */
#if 0
  else if (FRAME_INITIAL_P (f) && noninteractive)
    ;
#endif
}

/* An APC routine to cancel outstanding directory watch.  Invoked by
   the main thread via QueueUserAPC.  This is needed because only the
   thread that issued the ReadDirectoryChangesW call can call CancelIo
   to cancel that.  (CancelIoEx is only available since Vista, so we
   cannot use it on XP.)  */
VOID CALLBACK watch_end (ULONG_PTR);

VOID CALLBACK
watch_end (ULONG_PTR arg)
{
  HANDLE hdir = (HANDLE)arg;

  if (hdir && hdir != INVALID_HANDLE_VALUE)
    CancelIo (hdir);
}

/* A completion routine (a.k.a. "APC function") for handling events
   read by ReadDirectoryChangesW.  Called by the OS when the thread
   which issued the asynchronous ReadDirectoryChangesW call is in the
   "alertable state", i.e. waiting inside SleepEx call.  */
VOID CALLBACK watch_completion (DWORD, DWORD, OVERLAPPED *);

VOID CALLBACK
watch_completion (DWORD status, DWORD bytes_ret, OVERLAPPED *io_info)
{
  struct notification *dirwatch;
  DWORD _bytes;
  struct notifications_set *ns = NULL;
  BOOL terminate = FALSE;

  /* Who knows what happened?  Perhaps the OVERLAPPED structure was
     freed by someone already?  In any case, we cannot do anything
     with this request, so just punt and skip it.  FIXME: should we
     raise the 'terminate' flag in this case?  */
  if (!io_info)
    {
      DebPrint(("watch_completion: io_info is null.\n"));
      return;
    }

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
         We must exit, without issuing another call to
         ReadDirectoryChangesW. */
      return;
    }

  /* We allocate a new set of notifications to be linked to the linked
     list of notifications set.  This will be processed by Emacs event
     loop in the main thread.  We need to duplicate the notifications
     buffer, but not the dirwatch structure.  */

  /* Implementation note: In general, allocating memory in non-main
     threads is a no-no in Emacs.  We certainly cannot call xmalloc
     and friends, because it can longjmp when allocation fails, which
     will crash Emacs because the jmp_buf is set up to a location on
     the main thread's stack.  However, we can call 'malloc' directly,
     since that is redirected to HeapAlloc that uses our private heap,
     see w32heap.c, and that is thread-safe.  */
  ns = malloc (sizeof(struct notifications_set));
  if (ns)
    {
      memset (ns, 0, sizeof(struct notifications_set));
      ns->notifications = malloc (bytes_ret);
      if (ns->notifications)
	{
	  memcpy (ns->notifications, dirwatch->buf, bytes_ret);
	  ns->size = bytes_ret;
	  ns->desc = dirwatch;
	}
      else
	{
	  free (ns);
	  ns = NULL;
	}
    }
  if (ns == NULL)
    DebPrint(("Out of memory.  Notifications lost."));

  /* Calling ReadDirectoryChangesW quickly to watch again for new
     notifications.  */
  if (!ReadDirectoryChangesW (dirwatch->dir, dirwatch->buf,
			      DIRWATCH_BUFFER_SIZE, dirwatch->subtree,
			      dirwatch->filter, &_bytes, dirwatch->io_info,
			      watch_completion))
    {
      DebPrint (("ReadDirectoryChangesW error: %lu\n", GetLastError ()));
      /* If this call fails, it means that the directory is not
         watchable any more.  We need to terminate the worker thread.
         Still, we will wait until the current notifications have been
         sent to the main thread.  */
      terminate = TRUE;
    }

  if (ns)
    send_notifications(ns);

  /* If we were asked to terminate the thread, then fire the event. */
  if (terminate)
    SetEvent(dirwatch->terminate);
}

/* Worker routine for the watch thread.  */
static DWORD WINAPI
watch_worker (LPVOID arg)
{
  struct notification *dirwatch = (struct notification *)arg;
  BOOL bErr;
  DWORD _bytes = 0;
  DWORD status;

  if (dirwatch->dir)
    {
      bErr = ReadDirectoryChangesW (dirwatch->dir, dirwatch->buf,
				    DIRWATCH_BUFFER_SIZE, dirwatch->subtree,
				    dirwatch->filter, &_bytes,
				    dirwatch->io_info, watch_completion);
      if (!bErr)
	{
	  DebPrint (("ReadDirectoryChangesW: %lu\n", GetLastError ()));
	  /* We cannot remove the dirwatch object from watch_list,
	     because we are in a separate thread.  For the same
	     reason, we also cannot free memory consumed by the
	     buffers allocated for the dirwatch object.  So we close
	     the directory handle, but do not free the object itself
	     or its buffers.  We also don't touch the signature.  This
	     way, remove_watch can still identify the object, remove
	     it, and free its memory.  */
	  CloseHandle (dirwatch->dir);
	  dirwatch->dir = NULL;
	  return 1;
	}
    }

  do {
    status = WaitForSingleObjectEx(dirwatch->terminate, INFINITE, TRUE);
  } while (status == WAIT_IO_COMPLETION);

  /* The thread is about to terminate, so we clean up the dir handle.  */
  CloseHandle (dirwatch->dir);
  dirwatch->dir = NULL;

  return 0;
}
/* Launch a thread to watch changes to FILE in a directory open on
   handle HDIR.  */
static struct notification *
start_watching (const char *file, HANDLE hdir, BOOL subdirs, DWORD flags)
{
  struct notification *dirwatch = xzalloc (sizeof (struct notification));

  dirwatch->signature = DIRWATCH_SIGNATURE;
  dirwatch->buf = xmalloc (DIRWATCH_BUFFER_SIZE);
  dirwatch->io_info = xzalloc (sizeof(OVERLAPPED));
  /* Stash a pointer to dirwatch structure for use by the completion
     routine.  According to MSDN documentation of ReadDirectoryChangesW:
     "The hEvent member of the OVERLAPPED structure is not used by the
     system, so you can use it yourself." */
  dirwatch->io_info->hEvent = dirwatch;
  dirwatch->subtree = subdirs;
  dirwatch->filter = flags;
  dirwatch->watchee = xstrdup (file);

  dirwatch->terminate = CreateEvent(NULL, FALSE, FALSE, NULL);

  dirwatch->dir = hdir;

  /* See w32proc.c where it calls CreateThread for the story behind
     the 2nd and 5th argument in the call to CreateThread.  */
  dirwatch->thr = CreateThread (NULL, 64 * 1024, watch_worker, (void *)dirwatch,
				0x00010000, NULL);

  if (!dirwatch->thr)
    {
      CloseHandle(dirwatch->terminate);
      xfree (dirwatch->buf);
      xfree (dirwatch->io_info);
      xfree (dirwatch->watchee);
      xfree (dirwatch);
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
    {
      CloseHandle (hdir);
      dirwatch->dir = NULL;
    }

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
      DWORD exit_code = 0, err = 0;

      /* Only the thread that issued the outstanding I/O call can call
	 CancelIo on it.  (CancelIoEx is available only since Vista.)
	 So we need to queue an APC for the worker thread telling it
	 to terminate.  */
      if (!QueueUserAPC (watch_end, dirwatch->thr, (ULONG_PTR)dirwatch->dir))
	DebPrint (("QueueUserAPC failed (%lu)!\n", GetLastError ()));

      /* We also signal the thread that it can terminate.  */
      SetEvent(dirwatch->terminate);

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
              DebPrint(("Forcing thread termination.\n"));
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
      CloseHandle(dirwatch->terminate);
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
  watch_object = Fassoc (watch_descriptor, watch_list, Qnil);
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
     critical section, so we cannot possibly quit if watch_list is not
     in the right condition.  */
  return NILP (watch_list) ? Qnil : assoc_no_quit (descriptor, watch_list);
}

DEFUN ("w32notify-valid-p", Fw32notify_valid_p, Sw32notify_valid_p, 1, 1, 0,
       doc: /* Check a watch specified by its WATCH-DESCRIPTOR for validity.

WATCH-DESCRIPTOR should be an object returned by `w32notify-add-watch'.

A watch can become invalid if the directory it watches is deleted, or if
the watcher thread exits abnormally for any other reason.  Removing the
watch by calling `w32notify-rm-watch' also makes it invalid.  */)
     (Lisp_Object watch_descriptor)
{
  Lisp_Object watch_object = Fassoc (watch_descriptor, watch_list, Qnil);

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
