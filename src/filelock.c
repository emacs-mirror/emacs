/* Lock files for editing.

Copyright (C) 1985-1987, 1993-1994, 1996, 1998-2026 Free Software
Foundation, Inc.

Author: Richard King
  (according to authors.el)

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
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <boot-time.h>
#include <c-ctype.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"
#ifdef WINDOWSNT
#include <share.h>
#include <sys/socket.h>	/* for fcntl */
#include "w32common.h"
#endif

#ifndef MSDOS

#ifdef HAVE_ANDROID
#include "android.h" /* For `android_is_special_directory'.  */
#endif /* HAVE_ANDROID */

/* Normally use a symbolic link to represent a lock.
   The strategy: to lock a file FN, create a symlink .#FN in FN's
   directory, with link data USER@HOST.PID:BOOT.  This avoids a single
   mount (== failure) point for lock files.  The :BOOT is omitted if
   the boot time is not available.

   When the host in the lock data is the current host, we can check if
   the pid is valid with kill.

   Otherwise, we could look at a separate file that maps hostnames to
   reboot times to see if the remote pid can possibly be valid, since we
   don't want Emacs to have to communicate via pipes or sockets or
   whatever to other processes, either locally or remotely; rms says
   that's too unreliable.  Hence the separate file, which could
   theoretically be updated by daemons running separately -- but this
   whole idea is unimplemented; in practice, at least in our
   environment, it seems such stale locks arise fairly infrequently, and
   Emacs's standard methods of dealing with clashes suffice.

   We use symlinks instead of normal files because (1) they can be
   stored more efficiently on the filesystem, since the kernel knows
   they will be small, and (2) all the info about the lock can be read
   in a single system call (readlink).  Although we could use regular
   files to be useful on old systems lacking symlinks, nowadays
   virtually all such systems are probably single-user anyway, so it
   didn't seem worth the complication.

   Similarly, we don't worry about a possible 14-character limit on
   file names, because those are all the same systems that don't have
   symlinks.

   This is compatible with the locking scheme used by Interleaf (which
   has contributed this implementation for Emacs), and was designed by
   Karl Berry, Ethan Jacobson, Kimbo Mundy, and others.

   On some file systems, notably those of MS-Windows, symbolic links
   do not work well, so instead of a symlink .#FN -> USER@HOST.PID:BOOT,
   the lock is a regular file .#FN with contents USER@HOST.PID:BOOT.  To
   establish a lock, a nonce file is created and then renamed to .#FN.
   On MS-Windows this renaming is atomic unless the lock is forcibly
   acquired.  On other systems the renaming is atomic if the lock is
   forcibly acquired; if not, the renaming is done via hard links,
   which is good enough for lock-file purposes.

   To summarize, race conditions can occur with either:

   * Forced locks on MS-Windows systems.

   * Non-forced locks on non-MS-Windows systems that support neither
     hard nor symbolic links.  */


/* Return the time of the last system boot, or 0 if that information
   is unavailable.  */

static time_t
get_boot_sec (void)
{
  /* get_boot_time maintains static state.  Don't touch that state
     if we are going to dump, since it might not survive dumping.  */
  if (will_dump_p ())
    return 0;

  struct timespec boot_time;
  boot_time.tv_sec = 0;
  get_boot_time (&boot_time);
  return boot_time.tv_sec;
}

/* An arbitrary limit on lock contents length.  8 K should be plenty
   big enough in practice.  */
enum { MAX_LFINFO = 8 * 1024 };

/* Here is the structure that stores information about a lock.  */

typedef struct
{
  /* Location of '@', '.', and ':' (or equivalent) in USER.  If there's
     no colon or equivalent, COLON points to the end of USER.  */
  char *at, *dot, *colon;

  /* Lock file contents USER@HOST.PID with an optional :BOOT_TIME
     appended.  This memory is used as a lock file contents buffer, so
     it needs room for MAX_LFINFO + 1 bytes.  A string " (pid NNNN)"
     may be appended to the USER@HOST while generating a diagnostic,
     so make room for its extra bytes (as opposed to ".NNNN") too.  */
  char user[MAX_LFINFO + 1 + sizeof " (pid )" - sizeof "."];
} lock_info_type;

/* For some reason Linux kernels return EPERM on file systems that do
   not support hard or symbolic links.  This symbol documents the quirk.
   There is no way to tell whether a symlink call fails due to
   permissions issues or because links are not supported, but luckily
   the lock file code should work either way.  */
enum { LINKS_MIGHT_NOT_WORK = EPERM };

/* Rename OLD to NEW.  If FORCE, replace any existing NEW.
   It is OK if there are temporarily two hard links to OLD.
   Return 0 if successful, -1 (setting errno) otherwise.  */
static int
rename_lock_file (char const *old, char const *new, bool force)
{
#ifdef WINDOWSNT
  return sys_rename_replace (old, new, force);
#else
  if (! force)
    {
      struct stat st;

      int r = emacs_renameat_noreplace (AT_FDCWD, old,
					AT_FDCWD, new);
      if (! (r < 0 && errno == ENOSYS))
	return r;
      if (link (old, new) == 0)
	return emacs_unlink (old) == 0 || errno == ENOENT ? 0 : -1;
      if (errno != ENOSYS && errno != LINKS_MIGHT_NOT_WORK)
	return -1;

      /* 'link' does not work on this file system.  This can occur on
	 a GNU/Linux host mounting a FAT32 file system.  Fall back on
	 'rename' after checking that NEW does not exist.  There is a
	 potential race condition since some other process may create
	 NEW immediately after the existence check, but it's the best
	 we can portably do here.  */
      if (emacs_fstatat (AT_FDCWD, new, &st, AT_SYMLINK_NOFOLLOW) == 0
	  || errno == EOVERFLOW)
	{
	  errno = EEXIST;
	  return -1;
	}
      if (errno != ENOENT)
	return -1;
    }

  return emacs_rename (old, new);
#endif
}

/* Create the lock file LFNAME with contents LOCK_INFO_STR.  Return 0 if
   successful, an errno value on failure.  If FORCE, remove any
   existing LFNAME if necessary.  */

static int
create_lock_file (char *lfname, char *lock_info_str, bool force)
{
#ifdef WINDOWSNT
  /* Symlinks are supported only by later versions of Windows, and
     creating them is a privileged operation that often triggers
     User Account Control elevation prompts.  Avoid the problem by
     pretending that 'symlink' does not work.  */
  int err = ENOSYS;
#else
  int err = emacs_symlink (lock_info_str, lfname) == 0 ? 0 : errno;
#endif

  if (err == EEXIST && force)
    {
      emacs_unlink (lfname);
      err = emacs_symlink (lock_info_str, lfname) == 0 ? 0 : errno;
    }

  if (err == ENOSYS || err == LINKS_MIGHT_NOT_WORK || err == ENAMETOOLONG)
    {
      static char const nonce_base[] = ".#-emacsXXXXXX";
      char *last_slash = strrchr (lfname, '/');
      ptrdiff_t lfdirlen = last_slash + 1 - lfname;
      USE_SAFE_ALLOCA;
      char *nonce = SAFE_ALLOCA (lfdirlen + sizeof nonce_base);
      int fd;
      memcpy (nonce, lfname, lfdirlen);
      strcpy (nonce + lfdirlen, nonce_base);

      fd = mkostemp (nonce, O_BINARY | O_CLOEXEC);
      if (fd < 0)
	err = errno;
      else
	{
	  ptrdiff_t lock_info_len;
	  lock_info_len = strlen (lock_info_str);
	  err = 0;

	  /* Make the lock file readable to others, so that others' sessions
	     can read it.  Even though nobody should write to the lock file,
	     keep it user-writable to work around problems on nonstandard file
	     systems that prohibit unlinking readonly files (Bug#37884).  */
	  if (emacs_write (fd, lock_info_str, lock_info_len) != lock_info_len
	      || fchmod (fd, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH) != 0)
	    err = errno;

	  /* There is no need to call fsync here, as the contents of
	     the lock file need not survive system crashes.  */
	  if (emacs_close (fd) != 0)
	    err = errno;
	  if (!err && rename_lock_file (nonce, lfname, force) != 0)
	    err = errno;
	  if (err)
	    emacs_unlink (nonce);
	}

      SAFE_FREE ();
    }

  return err;
}

/* Lock the lock file named LFNAME.
   If FORCE, do so even if it is already locked.
   Return 0 if successful, an error number on failure.  */

static int
lock_file_1 (Lisp_Object lfname, bool force)
{
  intmax_t boot = get_boot_sec ();
  Lisp_Object luser_name = Fuser_login_name (Qnil);
  Lisp_Object lhost_name = Fsystem_name ();
  char const *user_name = STRINGP (luser_name) ? SSDATA (luser_name) : "";
  char const *host_name = STRINGP (lhost_name) ? SSDATA (lhost_name) : "";
  char lock_info_str[MAX_LFINFO + 1];
  intmax_t pid = getpid ();

  int room = sizeof lock_info_str;
  int len = snprintf (lock_info_str, room, "%s@", user_name);
  if (! (0 <= len && len < sizeof lock_info_str))
    return ENAMETOOLONG;
  /* Protect against the extremely unlikely case of the host name
     containing an @ character.  */
  for (; *host_name; len++, host_name++)
    {
      if (! (len < sizeof lock_info_str - 1))
	return ENAMETOOLONG;
      lock_info_str[len] = *host_name == '@' ? '-' : *host_name;
    }
  char const *lock_info_fmt = boot ? ".%"PRIdMAX":%"PRIdMAX : ".%"PRIdMAX;
  room = sizeof lock_info_str - len;
  int suffixlen = snprintf (lock_info_str + len, room,
			    lock_info_fmt, pid, boot);
  if (! (0 <= suffixlen && suffixlen < room))
    return ENAMETOOLONG;
  return create_lock_file (SSDATA (lfname), lock_info_str, force);
}

/* Return true if times A and B are no more than one second apart.  */

static bool
within_one_second (intmax_t a, time_t b)
{
  intmax_t diff;
  return !ckd_sub (&diff, a, b) && -1 <= diff && diff <= 1;
}

/* On systems lacking ELOOP, test for an errno value that shouldn't occur.  */
#ifndef ELOOP
# define ELOOP (-1)
#endif

/* Read the data for the lock file LFNAME into LFINFO.  Read at most
   MAX_LFINFO + 1 bytes.  Return the number of bytes read, or -1
   (setting errno) on error.  */

static ptrdiff_t
read_lock_data (char *lfname, char lfinfo[MAX_LFINFO + 1])
{
  ptrdiff_t nbytes;

  while ((nbytes = readlinkat (AT_FDCWD, lfname, lfinfo, MAX_LFINFO + 1)) < 0
	 && errno == EINVAL)
    {
      int fd = emacs_open (lfname, O_RDONLY | O_NOFOLLOW, 0);
      if (0 <= fd)
	{
	  ptrdiff_t read_bytes = emacs_read (fd, lfinfo, MAX_LFINFO + 1);
	  int read_errno = errno;
	  if (emacs_close (fd) != 0)
	    return -1;
	  errno = read_errno;
	  return read_bytes;
	}

      if (errno != ELOOP)
	return -1;

      /* readlinkat saw a non-symlink, but emacs_open saw a symlink.
	 The former must have been removed and replaced by the latter.
	 Try again.  */
      maybe_quit ();
    }

  return nbytes;
}

/* Whether the string S starts with a decimal integer, optionally
   negative.  */
static bool
integer_prefixed (char const *s)
{
  /* Doing it this way avoids a conditional branch on most platforms.  */
  return c_isdigit (s[s[0] == '-']);
}

/* Whether the integer P could identify an individual process.  On most
   platforms this simply checks for positive pid_t, but on some
   MS-Windows ports our headers #define it to to some other test.  */
#ifndef VALID_PROCESS_ID
# define VALID_PROCESS_ID(p) (0 < (p) && (p) <= TYPE_MAXIMUM (pid_t))
#endif

/* True if errno values are negative.  Although the C standard
   requires them to be positive, they are negative in Haiku.  */
enum { NEGATIVE_ERRNO = EDOM < 0 };

/* Nonzero values that are not errno values.  */
enum
  {
    /* Another process on this machine owns it.  */
    ANOTHER_OWNS_IT = NEGATIVE_ERRNO ? 1 : -1,

    /* This Emacs process owns it.  */
    I_OWN_IT = 2 * ANOTHER_OWNS_IT
  };

/* Return 0 if nobody owns the lock file LFNAME or the lock is obsolete,
   ANOTHER_OWNS_IT if another process owns it
     (and set OWNER (if non-null) to info),
   I_OWN_IT if the current process owns it,
   or an errno value if something is wrong with the locking mechanism.  */

static int
current_lock_owner (lock_info_type *owner, Lisp_Object lfname)
{
  lock_info_type local_owner;

  /* Even if the caller doesn't want the owner info, we still have to
     read it to determine return value.  */
  if (!owner)
    owner = &local_owner;

  /* If nonexistent lock file, all is well; otherwise, got strange error. */
  ptrdiff_t lfinfolen = read_lock_data (SSDATA (lfname), owner->user);
  if (lfinfolen < 0)
    return errno == ENOENT || errno == ENOTDIR ? 0 : errno;

  /* If the lock file seems valid, return a value based on its contents.  */
  if (lfinfolen)
    {
      if (MAX_LFINFO < lfinfolen)
	return ENAMETOOLONG;
      owner->user[lfinfolen] = 0;

      /* Parse USER@HOST.PID:BOOT_TIME.  If can't parse, return EINVAL.  */
      /* The USER is everything before the last @.  */
      char *at = memrchr (owner->user, '@', lfinfolen);
      if (!at)
	return EINVAL;
      owner->at = at;
      char *dot = strrchr (at, '.');
      if (!dot)
	return EINVAL;
      owner->dot = dot;

      /* The PID is everything from the last '.' to the ':' or equivalent.  */
      if (! integer_prefixed (dot + 1))
	return EINVAL;
      errno = 0;
      intmax_t pid = strtoimax (dot + 1, &owner->colon, 10);
      if (errno == ERANGE)
	pid = -1;

      /* After the ':' or equivalent, if there is one, comes the boot time.  */
      intmax_t boot_time;
      char *boot = owner->colon + 1, *lfinfo_end;
      switch (owner->colon[0])
	{
	case 0:
	  boot_time = 0;
	  lfinfo_end = owner->colon;
	  break;

	case '\357':
	  /* Treat "\357\200\242" (U+F022 in UTF-8) like ":" (Bug#24656).
	     This works around a bug in the Linux CIFS kernel client, which can
	     mistakenly transliterate ':' to U+F022 in symlink contents.
	     See <https://bugzilla.redhat.com/show_bug.cgi?id=1384153>.  */
	  if (! (boot[0] == '\200' && boot[1] == '\242'))
	    return EINVAL;
	  boot += 2;
	  FALLTHROUGH;
	case ':':
	  if (! integer_prefixed (boot))
	    return EINVAL;
	  boot_time = strtoimax (boot, &lfinfo_end, 10);
	  break;

	default:
	  return EINVAL;
	}
      if (lfinfo_end != owner->user + lfinfolen)
	return EINVAL;

      char *linkhost = at + 1;
      ptrdiff_t linkhostlen = dot - linkhost;
      Lisp_Object system_name = Fsystem_name ();
      /* If `system-name' returns nil, that means we're in a
	 --no-build-details Emacs, and the name part of the link (e.g.,
	 .#test.txt -> larsi@.118961:1646577954) is an empty string.  */
      bool on_current_host;
      if (NILP (system_name))
	on_current_host = linkhostlen == 0;
      else
	{
	  on_current_host = linkhostlen == SBYTES (system_name);
	  if (on_current_host)
	    {
	      /* Protect against the extremely unlikely case of the host
		 name containing '@'.  */
	      char *sysname = SSDATA (system_name);
	      for (ptrdiff_t i = 0; i < linkhostlen; i++)
		if (linkhost[i] != (sysname[i] == '@' ? '-' : sysname[i]))
		  {
		    on_current_host = false;
		    break;
		  }
	    }
	}
      if (!on_current_host)
	{
	  /* Not on current host.  If we wanted to support the check for
	     stale locks on remote machines, here's where we'd do it.  */
	  return ANOTHER_OWNS_IT;
	}

      if (pid == getpid ())
	return I_OWN_IT;

      if (VALID_PROCESS_ID (pid)
	  && ! (kill (pid, 0) < 0 && errno != EPERM)
	  && (boot_time == 0
	      || within_one_second (boot_time, get_boot_sec ())))
	return ANOTHER_OWNS_IT;
    }

  /* The owner process is dead or has a strange pid, or the lock file is empty.
     Try to zap the lockfile.  If the lock file is empty, this assumes
     the file system is buggy, e.g., <https://bugs.gnu.org/72641>.
     Emacs never creates empty lock files even temporarily, so removing
     an empty lock file should be harmless.  */
  return emacs_unlink (SSDATA (lfname)) < 0 && errno != ENOENT ? errno : 0;
}


/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return ANOTHER_OWNS_IT if some other process owns the lock, and info about
     that process in CLASHER.
   Return errno value if cannot lock for any other reason.  */

static int
lock_if_free (lock_info_type *clasher, Lisp_Object lfname)
{
  int err;
  while ((err = lock_file_1 (lfname, 0)) == EEXIST)
    {
      err = current_lock_owner (clasher, lfname);

      /* Return if we locked it, or another process owns it, or it is
	 a strange error.  */
      if (err != 0)
	return err == I_OWN_IT ? 0 : err;

      /* We deleted a stale lock or some other process deleted the lock;
	 try again to lock the file.  */
    }

  return err;
}

/* Return the encoded name of the lock file for FN, or nil if none.  */

static Lisp_Object
make_lock_file_name (Lisp_Object fn)
{
  Lisp_Object lock_file_name;
#if defined HAVE_ANDROID && !defined ANDROID_STUBIFY
  char *name;
#endif

  fn = Fexpand_file_name (fn, Qnil);

#if defined HAVE_ANDROID && !defined ANDROID_STUBIFY
  /* Files in /assets and /contents can't have lock files on Android
     as these directories are fabrications of android.c, and backed by
     read only data.  */

  name = SSDATA (fn);

  if (android_is_special_directory (name, "/assets")
      || android_is_special_directory (name, "/content"))
  return Qnil;
#endif /* defined HAVE_ANDROID && !defined ANDROID_STUBIFY */

  lock_file_name = calln (Qmake_lock_file_name, fn);

  return !NILP (lock_file_name) ? ENCODE_FILE (lock_file_name) : Qnil;
}

/* lock_file locks file FN,
   meaning it serves notice on the world that you intend to edit that file.
   This should be done only when about to modify a file-visiting
   buffer previously unmodified.
   Do not (normally) call this for a buffer already modified,
   as either the file is already locked, or the user has already
   decided to go ahead without locking.

   When this returns, either the lock is locked for us,
   or lock creation failed,
   or the user has said to go ahead without locking.

   If the file is locked by someone else, this calls
   ask-user-about-lock (a Lisp function) with two arguments,
   the file name and info about the user who did the locking.
   This function can signal an error, or return t meaning
   take away the lock, or return nil meaning ignore the lock.  */

static Lisp_Object
lock_file (Lisp_Object fn)
{
  lock_info_type lock_info;

  /* Don't do locking while dumping Emacs.
     Uncompressing wtmp files uses call-process, which does not work
     in an uninitialized Emacs.  */
  if (will_dump_p ())
    return Qnil;

  Lisp_Object lfname = Qnil;
  if (create_lockfiles)
    {
      /* Create the name of the lock-file for file fn */
      lfname = make_lock_file_name (fn);
      if (NILP (lfname))
	return Qnil;
    }

  /* See if this file is visited and has changed on disk since it was
     visited.  */
  Lisp_Object subject_buf = Fget_truename_buffer (fn);
  if (!NILP (subject_buf)
      && NILP (Fverify_visited_file_modtime (subject_buf))
      && !NILP (Ffile_exists_p (fn))
      && !(!NILP (lfname) && current_lock_owner (NULL, lfname) == I_OWN_IT))
    calln (Quserlock__ask_user_about_supersession_threat, fn);

  /* Don't do locking if the user has opted out.  */
  if (!NILP (lfname))
    {
      /* Try to lock the lock.  FIXME: This ignores errors when
	 lock_if_free returns an errno value.  */
      if (lock_if_free (&lock_info, lfname) == ANOTHER_OWNS_IT)
	{
	  /* Someone else has the lock.  Consider breaking it.  */
	  Lisp_Object attack;
	  char *dot = lock_info.dot;
	  ptrdiff_t pidlen = lock_info.colon - (dot + 1);
	  static char const replacement[] = " (pid ";
	  int replacementlen = sizeof replacement - 1;
	  memmove (dot + replacementlen, dot + 1, pidlen);
	  strcpy (dot + replacementlen + pidlen, ")");
	  memcpy (dot, replacement, replacementlen);
	  attack = calln (Qask_user_about_lock, fn,
			  build_string (lock_info.user));
	  /* Take the lock if the user said so.  */
	  if (!NILP (attack))
	    lock_file_1 (lfname, 1);
	}
    }
  return Qnil;
}

static Lisp_Object
unlock_file (Lisp_Object fn)
{
  Lisp_Object lfname = make_lock_file_name (fn);
  if (NILP (lfname))
    return Qnil;

  int err = current_lock_owner (0, lfname);
  if (! (err == 0 || err == ANOTHER_OWNS_IT
	 || (err == I_OWN_IT
	     && (emacs_unlink (SSDATA (lfname)) == 0
		 || (err = errno) == ENOENT))))
    report_file_errno ("Unlocking file", fn, err);

  return Qnil;
}

static Lisp_Object
unlock_file_handle_error (Lisp_Object err)
{
  calln (Quserlock__handle_unlock_error, err);
  return Qnil;
}

#endif	/* MSDOS */

void
unlock_all_files (void)
{
  register Lisp_Object tail, buf;
  register struct buffer *b;

  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      b = XBUFFER (buf);
      if (STRINGP (BVAR (b, file_truename))
	  && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b))
	Funlock_file (BVAR (b, file_truename));
    }
}

DEFUN ("lock-file", Flock_file, Slock_file, 1, 1, 0,
       doc: /* Check whether FILE was modified since it was visited, and lock it.
If user option `create-lockfiles' is nil, this does not create
a lock file for FILE, but it still checks whether FILE was modified
outside of the current Emacs session, and if so, asks the user
whether to modify FILE.  */)
  (Lisp_Object file)
{
#ifndef MSDOS
  CHECK_STRING (file);

  /* If the file name has special constructs in it,
     call the corresponding file name handler.  */
  Lisp_Object handler;
  handler = Ffind_file_name_handler (file, Qlock_file);
  if (!NILP (handler))
    return calln (handler, Qlock_file, file);

  lock_file (file);
#endif	/* MSDOS */
  return Qnil;
}

DEFUN ("unlock-file", Funlock_file, Sunlock_file, 1, 1, 0,
       doc: /* Unlock FILE.  */)
  (Lisp_Object file)
{
#ifndef MSDOS
  CHECK_STRING (file);

  /* If the file name has special constructs in it,
     call the corresponding file name handler.  */
  Lisp_Object handler;
  handler = Ffind_file_name_handler (file, Qunlock_file);
  if (!NILP (handler))
    {
      calln (handler, Qunlock_file, file);
      return Qnil;
    }

  internal_condition_case_1 (unlock_file,
			     file,
			     list1 (Qfile_error),
			     unlock_file_handle_error);
#endif	/* MSDOS */
  return Qnil;
}

DEFUN ("lock-buffer", Flock_buffer, Slock_buffer,
       0, 1, 0,
       doc: /* Lock FILE, if current buffer is modified.
FILE defaults to current buffer's visited file,
or else nothing is done if current buffer isn't visiting a file.

If the option `create-lockfiles' is nil, this does nothing.  */)
  (Lisp_Object file)
{
  if (NILP (file))
    file = BVAR (current_buffer, file_truename);
  else
    CHECK_STRING (file);
  if (SAVE_MODIFF < MODIFF
      && !NILP (file))
    Flock_file (file);
  return Qnil;
}

DEFUN ("unlock-buffer", Funlock_buffer, Sunlock_buffer,
       0, 0, 0,
       doc: /* Unlock the file visited in the current buffer.
If the buffer is not modified, this does nothing because the file
should not be locked in that case.  It also does nothing if the
current buffer is not visiting a file, or is not locked.  Handles file
system errors by calling `display-warning' and continuing as if the
error did not occur.  */)
  (void)
{
  if (SAVE_MODIFF < MODIFF
      && STRINGP (BVAR (current_buffer, file_truename)))
    Funlock_file (BVAR (current_buffer, file_truename));
  return Qnil;
}

/* Unlock the file visited in buffer BUFFER.  */

void
unlock_buffer (struct buffer *buffer)
{
  if (BUF_SAVE_MODIFF (buffer) < BUF_MODIFF (buffer)
      && STRINGP (BVAR (buffer, file_truename)))
    Funlock_file (BVAR (buffer, file_truename));
}

DEFUN ("file-locked-p", Ffile_locked_p, Sfile_locked_p, 1, 1, 0,
       doc: /* Return a value indicating whether FILENAME is locked.
The value is nil if the FILENAME is not locked,
t if it is locked by you, else a string saying which user has locked it.  */)
  (Lisp_Object filename)
{
#ifdef MSDOS
  return Qnil;
#else
  Lisp_Object ret;
  int owner;
  lock_info_type locker;

  /* If the file name has special constructs in it,
     call the corresponding file name handler.  */
  Lisp_Object handler;
  handler = Ffind_file_name_handler (filename, Qfile_locked_p);
  if (!NILP (handler))
    {
      return calln (handler, Qfile_locked_p, filename);
    }

  Lisp_Object lfname = make_lock_file_name (filename);
  if (NILP (lfname))
    return Qnil;

  owner = current_lock_owner (&locker, lfname);
  switch (owner)
    {
    case I_OWN_IT: ret = Qt; break;
    case ANOTHER_OWNS_IT:
      ret = make_string (locker.user, locker.at - locker.user);
      break;
    case  0: ret = Qnil; break;
    default: report_file_errno ("Testing file lock", filename, owner);
    }

  return ret;
#endif
}

void
syms_of_filelock (void)
{
  DEFVAR_LISP ("temporary-file-directory", Vtemporary_file_directory,
	       doc: /* The directory for writing temporary files.  */);
  Vtemporary_file_directory = Qnil;

  DEFVAR_BOOL ("create-lockfiles", create_lockfiles,
	       doc: /* Non-nil means use lockfiles to avoid editing collisions.
The name of the (per-buffer) lockfile is constructed by prepending
".#" to the name of the file being locked.  See also `lock-buffer' and
Info node `(emacs)Interlocking'.  */);
  create_lockfiles = true;

  DEFSYM (Qlock_file, "lock-file");
  DEFSYM (Qunlock_file, "unlock-file");
  DEFSYM (Qfile_locked_p, "file-locked-p");
  DEFSYM (Qmake_lock_file_name, "make-lock-file-name");
  DEFSYM (Qstring_replace, "string-replace");
  DEFSYM (Qask_user_about_lock, "ask-user-about-lock");
  DEFSYM (Quserlock__handle_unlock_error, "userlock--handle-unlock-error");
  DEFSYM (Quserlock__ask_user_about_supersession_threat,
	  "userlock--ask-user-about-supersession-threat");

  defsubr (&Slock_file);
  defsubr (&Sunlock_file);
  defsubr (&Slock_buffer);
  defsubr (&Sunlock_buffer);
  defsubr (&Sfile_locked_p);
}
