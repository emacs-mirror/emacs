/* Lock files for editing.

Copyright (C) 1985-1987, 1993-1994, 1996, 1998-2020 Free Software
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

#ifdef __FreeBSD__
#include <sys/sysctl.h>
#endif /* __FreeBSD__ */

#include <errno.h>

#include <c-ctype.h>

#include "lisp.h"
#include "buffer.h"
#include "coding.h"
#ifdef WINDOWSNT
#include <share.h>
#include <sys/socket.h>	/* for fcntl */
#include "w32.h"	/* for dostounix_filename */
#endif

#ifndef MSDOS

#ifdef HAVE_UTMP_H
#include <utmp.h>
#endif

/* A file whose last-modified time is just after the most recent boot.
   Define this to be NULL to disable checking for this file.  */
#ifndef BOOT_TIME_FILE
#define BOOT_TIME_FILE "/var/run/random-seed"
#endif

#if !defined WTMP_FILE && !defined WINDOWSNT
#define WTMP_FILE "/var/log/wtmp"
#endif

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
   Emacs' standard methods of dealing with clashes suffice.

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


/* Return the time of the last system boot.  */

static time_t boot_time;
static bool boot_time_initialized;

#ifdef BOOT_TIME
static void get_boot_time_1 (const char *, bool);
#endif

static time_t
get_boot_time (void)
{
#if defined (BOOT_TIME)
  int counter;
#endif

  if (boot_time_initialized)
    return boot_time;
  boot_time_initialized = 1;

#if defined (CTL_KERN) && defined (KERN_BOOTTIME)
  {
    int mib[2];
    size_t size;
    struct timeval boottime_val;

    mib[0] = CTL_KERN;
    mib[1] = KERN_BOOTTIME;
    size = sizeof (boottime_val);

    if (sysctl (mib, 2, &boottime_val, &size, NULL, 0) >= 0 && size != 0)
      {
	boot_time = boottime_val.tv_sec;
	return boot_time;
      }
  }
#endif /* defined (CTL_KERN) && defined (KERN_BOOTTIME) */

  if (BOOT_TIME_FILE)
    {
      struct stat st;
      if (stat (BOOT_TIME_FILE, &st) == 0)
	{
	  boot_time = st.st_mtime;
	  return boot_time;
	}
    }

#if defined (BOOT_TIME)
  /* The utmp routines maintain static state.  Don't touch that state
     if we are going to dump, since it might not survive dumping.  */
  if (will_dump_p ())
    return boot_time;

  /* Try to get boot time from utmp before wtmp,
     since utmp is typically much smaller than wtmp.
     Passing a null pointer causes get_boot_time_1
     to inspect the default file, namely utmp.  */
  get_boot_time_1 (0, 0);
  if (boot_time)
    return boot_time;

  /* Try to get boot time from the current wtmp file.  */
  get_boot_time_1 (WTMP_FILE, 1);

  /* If we did not find a boot time in wtmp, look at wtmp, and so on.  */
  for (counter = 0; counter < 20 && ! boot_time; counter++)
    {
      Lisp_Object filename = Qnil;
      bool delete_flag = false;
      char cmd_string[sizeof WTMP_FILE ".19.gz"];
      AUTO_STRING_WITH_LEN (tempname, cmd_string,
			    sprintf (cmd_string, "%s.%d", WTMP_FILE, counter));
      if (! NILP (Ffile_exists_p (tempname)))
	filename = tempname;
      else
	{
	  tempname = make_formatted_string (cmd_string, "%s.%d.gz",
					    WTMP_FILE, counter);
	  if (! NILP (Ffile_exists_p (tempname)))
	    {
	      /* The utmp functions on older systems accept only file
		 names up to 8 bytes long.  Choose a 2 byte prefix, so
		 the 6-byte suffix does not make the name too long.  */
	      filename = Fmake_temp_file_internal (build_string ("wt"), Qnil,
						   empty_unibyte_string, Qnil);
	      CALLN (Fcall_process, build_string ("gzip"), Qnil,
		     list2 (QCfile, filename), Qnil,
		     build_string ("-cd"), tempname);
	      delete_flag = true;
	    }
	}

      if (! NILP (filename))
	{
	  get_boot_time_1 (SSDATA (filename), 1);
	  if (delete_flag)
	    unlink (SSDATA (filename));
	}
    }

  return boot_time;
#else
  return 0;
#endif
}

#ifdef BOOT_TIME
/* Try to get the boot time from wtmp file FILENAME.
   This succeeds if that file contains a reboot record.

   If FILENAME is zero, use the same file as before;
   if no FILENAME has ever been specified, this is the utmp file.
   Use the newest reboot record if NEWEST,
   the first reboot record otherwise.
   Ignore all reboot records on or before BOOT_TIME.
   Success is indicated by setting BOOT_TIME to a larger value.  */

void
get_boot_time_1 (const char *filename, bool newest)
{
  struct utmp ut, *utp;

  if (filename)
    utmpname (filename);

  setutent ();

  while (1)
    {
      /* Find the next reboot record.  */
      ut.ut_type = BOOT_TIME;
      utp = getutid (&ut);
      if (! utp)
	break;
      /* Compare reboot times and use the newest one.  */
      if (utp->ut_time > boot_time)
	{
	  boot_time = utp->ut_time;
	  if (! newest)
	    break;
	}
      /* Advance on element in the file
	 so that getutid won't repeat the same one.  */
      utp = getutent ();
      if (! utp)
	break;
    }
  endutent ();
}
#endif /* BOOT_TIME */

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

/* Write the name of the lock file for FNAME into LOCKNAME.  Length
   will be that of FNAME plus two more for the leading ".#", plus one
   for the NUL.  */
#define MAKE_LOCK_NAME(lockname, fname) \
  (lockname = SAFE_ALLOCA (SBYTES (fname) + 2 + 1), \
   fill_in_lock_file_name (lockname, fname))

static void
fill_in_lock_file_name (char *lockfile, Lisp_Object fn)
{
  char *last_slash = memrchr (SSDATA (fn), '/', SBYTES (fn));
  char *base = last_slash + 1;
  ptrdiff_t dirlen = base - SSDATA (fn);
  memcpy (lockfile, SSDATA (fn), dirlen);
  lockfile[dirlen] = '.';
  lockfile[dirlen + 1] = '#';
  strcpy (lockfile + dirlen + 2, base);
}

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

      int r = renameat_noreplace (AT_FDCWD, old, AT_FDCWD, new);
      if (! (r < 0 && errno == ENOSYS))
	return r;
      if (link (old, new) == 0)
	return unlink (old) == 0 || errno == ENOENT ? 0 : -1;
      if (errno != ENOSYS && errno != LINKS_MIGHT_NOT_WORK)
	return -1;

      /* 'link' does not work on this file system.  This can occur on
	 a GNU/Linux host mounting a FAT32 file system.  Fall back on
	 'rename' after checking that NEW does not exist.  There is a
	 potential race condition since some other process may create
	 NEW immediately after the existence check, but it's the best
	 we can portably do here.  */
      if (lstat (new, &st) == 0 || errno == EOVERFLOW)
	{
	  errno = EEXIST;
	  return -1;
	}
      if (errno != ENOENT)
	return -1;
    }

  return rename (old, new);
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
  int err = symlink (lock_info_str, lfname) == 0 ? 0 : errno;
#endif

  if (err == EEXIST && force)
    {
      unlink (lfname);
      err = symlink (lock_info_str, lfname) == 0 ? 0 : errno;
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
	    unlink (nonce);
	}

      SAFE_FREE ();
    }

  return err;
}

/* Lock the lock file named LFNAME.
   If FORCE, do so even if it is already locked.
   Return 0 if successful, an error number on failure.  */

static int
lock_file_1 (char *lfname, bool force)
{
  /* Call this first because it can GC.  */
  intmax_t boot = get_boot_time ();

  Lisp_Object luser_name = Fuser_login_name (Qnil);
  char const *user_name = STRINGP (luser_name) ? SSDATA (luser_name) : "";
  Lisp_Object lhost_name = Fsystem_name ();
  char const *host_name = STRINGP (lhost_name) ? SSDATA (lhost_name) : "";
  char lock_info_str[MAX_LFINFO + 1];
  intmax_t pid = getpid ();

  if (boot)
    {
      if (sizeof lock_info_str
          <= snprintf (lock_info_str, sizeof lock_info_str,
		       "%s@%s.%"PRIdMAX":%"PRIdMAX,
                       user_name, host_name, pid, boot))
        return ENAMETOOLONG;
    }
  else if (sizeof lock_info_str
           <= snprintf (lock_info_str, sizeof lock_info_str,
			"%s@%s.%"PRIdMAX,
                        user_name, host_name, pid))
    return ENAMETOOLONG;

  return create_lock_file (lfname, lock_info_str, force);
}

/* Return true if times A and B are no more than one second apart.  */

static bool
within_one_second (time_t a, time_t b)
{
  return (a - b >= -1 && a - b <= 1);
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

/* Return 0 if nobody owns the lock file LFNAME or the lock is obsolete,
   -1 if another process owns it (and set OWNER (if non-null) to info),
   -2 if the current process owns it,
   or an errno value if something is wrong with the locking mechanism.  */

static int
current_lock_owner (lock_info_type *owner, char *lfname)
{
  int ret;
  lock_info_type local_owner;
  ptrdiff_t lfinfolen;
  intmax_t pid, boot_time;
  char *at, *dot, *lfinfo_end;

  /* Even if the caller doesn't want the owner info, we still have to
     read it to determine return value.  */
  if (!owner)
    owner = &local_owner;

  /* If nonexistent lock file, all is well; otherwise, got strange error. */
  lfinfolen = read_lock_data (lfname, owner->user);
  if (lfinfolen < 0)
    return errno == ENOENT ? 0 : errno;
  if (MAX_LFINFO < lfinfolen)
    return ENAMETOOLONG;
  owner->user[lfinfolen] = 0;

  /* Parse USER@HOST.PID:BOOT_TIME.  If can't parse, return EINVAL.  */
  /* The USER is everything before the last @.  */
  owner->at = at = memrchr (owner->user, '@', lfinfolen);
  if (!at)
    return EINVAL;
  owner->dot = dot = strrchr (at, '.');
  if (!dot)
    return EINVAL;

  /* The PID is everything from the last '.' to the ':' or equivalent.  */
  if (! c_isdigit (dot[1]))
    return EINVAL;
  errno = 0;
  pid = strtoimax (dot + 1, &owner->colon, 10);
  if (errno == ERANGE)
    pid = -1;

  /* After the ':' or equivalent, if there is one, comes the boot time.  */
  char *boot = owner->colon + 1;
  switch (owner->colon[0])
    {
    case 0:
      boot_time = 0;
      lfinfo_end = owner->colon;
      break;

    case '\357':
      /* Treat "\357\200\242" (U+F022 in UTF-8) as if it were ":" (Bug#24656).
	 This works around a bug in the Linux CIFS kernel client, which can
	 mistakenly transliterate ':' to U+F022 in symlink contents.
	 See <https://bugzilla.redhat.com/show_bug.cgi?id=1384153>.  */
      if (! (boot[0] == '\200' && boot[1] == '\242'))
	return EINVAL;
      boot += 2;
      FALLTHROUGH;
    case ':':
      if (! c_isdigit (boot[0]))
	return EINVAL;
      boot_time = strtoimax (boot, &lfinfo_end, 10);
      break;

    default:
      return EINVAL;
    }
  if (lfinfo_end != owner->user + lfinfolen)
    return EINVAL;

  /* On current host?  */
  Lisp_Object system_name = Fsystem_name ();
  if (STRINGP (system_name)
      && dot - (at + 1) == SBYTES (system_name)
      && memcmp (at + 1, SSDATA (system_name), SBYTES (system_name)) == 0)
    {
      if (pid == getpid ())
        ret = -2; /* We own it.  */
      else if (0 < pid && pid <= TYPE_MAXIMUM (pid_t)
               && (kill (pid, 0) >= 0 || errno == EPERM)
	       && (boot_time == 0
		   || (boot_time <= TYPE_MAXIMUM (time_t)
		       && within_one_second (boot_time, get_boot_time ()))))
        ret = -1; /* An existing process on this machine owns it.  */
      /* The owner process is dead or has a strange pid, so try to
         zap the lockfile.  */
      else
        return unlink (lfname) < 0 ? errno : 0;
    }
  else
    { /* If we wanted to support the check for stale locks on remote machines,
         here's where we'd do it.  */
      ret = -1;
    }

  return ret;
}


/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return negative if some other process owns the lock, and info about
     that process in CLASHER.
   Return positive errno value if cannot lock for any other reason.  */

static int
lock_if_free (lock_info_type *clasher, char *lfname)
{
  int err;
  while ((err = lock_file_1 (lfname, 0)) == EEXIST)
    {
      err = current_lock_owner (clasher, lfname);
      if (err != 0)
	{
	  if (err < 0)
	    return -2 - err; /* We locked it, or someone else has it.  */
	  break; /* current_lock_owner returned strange error.  */
	}

      /* We deleted a stale lock; try again to lock the file.  */
    }

  return err;
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

void
lock_file (Lisp_Object fn)
{
  Lisp_Object orig_fn, encoded_fn;
  char *lfname;
  lock_info_type lock_info;
  USE_SAFE_ALLOCA;

  /* Don't do locking while dumping Emacs.
     Uncompressing wtmp files uses call-process, which does not work
     in an uninitialized Emacs.  */
  if (will_dump_p ())
    return;

  orig_fn = fn;
  fn = Fexpand_file_name (fn, Qnil);
#ifdef WINDOWSNT
  /* Ensure we have only '/' separators, to avoid problems with
     looking (inside fill_in_lock_file_name) for backslashes in file
     names encoded by some DBCS codepage.  */
  dostounix_filename (SSDATA (fn));
#endif
  encoded_fn = ENCODE_FILE (fn);

  /* See if this file is visited and has changed on disk since it was
     visited.  */
  {
    register Lisp_Object subject_buf;

    subject_buf = get_truename_buffer (orig_fn);

    if (!NILP (subject_buf)
	&& NILP (Fverify_visited_file_modtime (subject_buf))
	&& !NILP (Ffile_exists_p (fn)))
      call1 (intern ("userlock--ask-user-about-supersession-threat"), fn);

  }

  /* Don't do locking if the user has opted out.  */
  if (create_lockfiles)
    {

      /* Create the name of the lock-file for file fn */
      MAKE_LOCK_NAME (lfname, encoded_fn);

      /* Try to lock the lock.  FIXME: This ignores errors when
	 lock_if_free returns a positive errno value.  */
      if (lock_if_free (&lock_info, lfname) < 0)
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
	  attack = call2 (intern ("ask-user-about-lock"), fn,
			  build_string (lock_info.user));
	  /* Take the lock if the user said so.  */
	  if (!NILP (attack))
	    lock_file_1 (lfname, 1);
	}
      SAFE_FREE ();
    }
}

void
unlock_file (Lisp_Object fn)
{
  char *lfname;
  USE_SAFE_ALLOCA;

  Lisp_Object filename = Fexpand_file_name (fn, Qnil);
  fn = ENCODE_FILE (filename);

  MAKE_LOCK_NAME (lfname, fn);

  int err = current_lock_owner (0, lfname);
  if (err == -2 && unlink (lfname) != 0 && errno != ENOENT)
    err = errno;
  if (0 < err)
    report_file_errno ("Unlocking file", filename, err);

  SAFE_FREE ();
}

#else  /* MSDOS */
void
lock_file (Lisp_Object fn)
{
}

void
unlock_file (Lisp_Object fn)
{
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
	unlock_file (BVAR (b, file_truename));
    }
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
    lock_file (file);
  return Qnil;
}

DEFUN ("unlock-buffer", Funlock_buffer, Sunlock_buffer,
       0, 0, 0,
       doc: /* Unlock the file visited in the current buffer.
If the buffer is not modified, this does nothing because the file
should not be locked in that case.  */)
  (void)
{
  if (SAVE_MODIFF < MODIFF
      && STRINGP (BVAR (current_buffer, file_truename)))
    unlock_file (BVAR (current_buffer, file_truename));
  return Qnil;
}

/* Unlock the file visited in buffer BUFFER.  */

void
unlock_buffer (struct buffer *buffer)
{
  if (BUF_SAVE_MODIFF (buffer) < BUF_MODIFF (buffer)
      && STRINGP (BVAR (buffer, file_truename)))
    unlock_file (BVAR (buffer, file_truename));
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
  char *lfname;
  int owner;
  lock_info_type locker;
  USE_SAFE_ALLOCA;

  filename = Fexpand_file_name (filename, Qnil);
  Lisp_Object encoded_filename = ENCODE_FILE (filename);
  MAKE_LOCK_NAME (lfname, encoded_filename);

  owner = current_lock_owner (&locker, lfname);
  switch (owner)
    {
    case -2: ret = Qt; break;
    case -1: ret = make_string (locker.user, locker.at - locker.user); break;
    case  0: ret = Qnil; break;
    default: report_file_errno ("Testing file lock", filename, owner);
    }

  SAFE_FREE ();
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
The name of the (per-buffer) lockfile is constructed by prepending a
'.#' to the name of the file being locked.  See also `lock-buffer' and
Info node `(emacs)Interlocking'.  */);
  create_lockfiles = 1;

  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
  defsubr (&Sfile_locked_p);
}
