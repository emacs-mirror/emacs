/* Lock files for editing.
   Copyright (C) 1985, 1986, 1987, 1993, 1994, 1996, 1998, 1999, 2000, 2001,
                 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <sys/file.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __FreeBSD__
#include <sys/sysctl.h>
#endif /* __FreeBSD__ */

#include <errno.h>
#ifndef errno
extern int errno;
#endif

#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "systime.h"

/* The directory for writing temporary files.  */

Lisp_Object Vtemporary_file_directory;

#ifdef CLASH_DETECTION

#include <utmp.h>

#if !defined (S_ISLNK) && defined (S_IFLNK)
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif

/* A file whose last-modified time is just after the most recent boot.
   Define this to be NULL to disable checking for this file.  */
#ifndef BOOT_TIME_FILE
#define BOOT_TIME_FILE "/var/run/random-seed"
#endif

#ifndef WTMP_FILE
#define WTMP_FILE "/var/log/wtmp"
#endif

/* The strategy: to lock a file FN, create a symlink .#FN in FN's
   directory, with link data `user@host.pid'.  This avoids a single
   mount (== failure) point for lock files.

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
   Ethan Jacobson, Kimbo Mundy, and others.

   --karl@cs.umb.edu/karl@hq.ileaf.com.  */


/* Return the time of the last system boot.  */

static time_t boot_time;
static int boot_time_initialized;

extern Lisp_Object Vshell_file_name;

#ifdef BOOT_TIME
static void get_boot_time_1 P_ ((char *, int));
#endif

static time_t
get_boot_time ()
{
#if defined (BOOT_TIME) && ! defined (NO_WTMP_FILE)
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

    if (sysctl (mib, 2, &boottime_val, &size, NULL, 0) >= 0)
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

#if defined (BOOT_TIME) && ! defined (NO_WTMP_FILE)
#ifndef CANNOT_DUMP
  /* The utmp routines maintain static state.
     Don't touch that state unless we are initialized,
     since it might not survive dumping.  */
  if (! initialized)
    return boot_time;
#endif /* not CANNOT_DUMP */

  /* Try to get boot time from utmp before wtmp,
     since utmp is typically much smaller than wtmp.
     Passing a null pointer causes get_boot_time_1
     to inspect the default file, namely utmp.  */
  get_boot_time_1 ((char *) 0, 0);
  if (boot_time)
    return boot_time;

  /* Try to get boot time from the current wtmp file.  */
  get_boot_time_1 (WTMP_FILE, 1);

  /* If we did not find a boot time in wtmp, look at wtmp, and so on.  */
  for (counter = 0; counter < 20 && ! boot_time; counter++)
    {
      char cmd_string[100];
      Lisp_Object tempname, filename;
      int delete_flag = 0;

      filename = Qnil;

      sprintf (cmd_string, "%s.%d", WTMP_FILE, counter);
      tempname = build_string (cmd_string);
      if (! NILP (Ffile_exists_p (tempname)))
	filename = tempname;
      else
	{
	  sprintf (cmd_string, "%s.%d.gz", WTMP_FILE, counter);
	  tempname = build_string (cmd_string);
	  if (! NILP (Ffile_exists_p (tempname)))
	    {
	      Lisp_Object args[6];

	      /* The utmp functions on mescaline.gnu.org accept only
		 file names up to 8 characters long.  Choose a 2
		 character long prefix, and call make_temp_file with
		 second arg non-zero, so that it will add not more
		 than 6 characters to the prefix.  */
	      tempname = Fexpand_file_name (build_string ("wt"),
					    Vtemporary_file_directory);
	      tempname = make_temp_name (tempname, 1);
	      args[0] = Vshell_file_name;
	      args[1] = Qnil;
	      args[2] = Qnil;
	      args[3] = Qnil;
	      args[4] = build_string ("-c");
	      sprintf (cmd_string, "gunzip < %s.%d.gz > %s",
		       WTMP_FILE, counter, SDATA (tempname));
	      args[5] = build_string (cmd_string);
	      Fcall_process (6, args);
	      filename = tempname;
	      delete_flag = 1;
	    }
	}

      if (! NILP (filename))
	{
	  get_boot_time_1 (SDATA (filename), 1);
	  if (delete_flag)
	    unlink (SDATA (filename));
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
   Use the newest reboot record if NEWEST is nonzero,
   the first reboot record otherwise.
   Ignore all reboot records on or before BOOT_TIME.
   Success is indicated by setting BOOT_TIME to a larger value.  */

void
get_boot_time_1 (filename, newest)
     char *filename;
     int newest;
{
  struct utmp ut, *utp;
  int desc;

  if (filename)
    {
      /* On some versions of IRIX, opening a nonexistent file name
	 is likely to crash in the utmp routines.  */
      desc = emacs_open (filename, O_RDONLY, 0);
      if (desc < 0)
	return;

      emacs_close (desc);

      utmpname (filename);
    }

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

/* Here is the structure that stores information about a lock.  */

typedef struct
{
  char *user;
  char *host;
  unsigned long pid;
  time_t boot_time;
} lock_info_type;

/* When we read the info back, we might need this much more,
   enough for decimal representation plus null.  */
#define LOCK_PID_MAX (4 * sizeof (unsigned long))

/* Free the two dynamically-allocated pieces in PTR.  */
#define FREE_LOCK_INFO(i) do { xfree ((i).user); xfree ((i).host); } while (0)


/* Write the name of the lock file for FN into LFNAME.  Length will be
   that of FN plus two more for the leading `.#' plus 1 for the
   trailing period plus one for the digit after it plus one for the
   null.  */
#define MAKE_LOCK_NAME(lock, file) \
  (lock = (char *) alloca (SBYTES (file) + 2 + 1 + 1 + 1), \
   fill_in_lock_file_name (lock, (file)))

static void
fill_in_lock_file_name (lockfile, fn)
     register char *lockfile;
     register Lisp_Object fn;
{
  register char *p;
  struct stat st;
  int count = 0;

  strcpy (lockfile, SDATA (fn));

  /* Shift the nondirectory part of the file name (including the null)
     right two characters.  Here is one of the places where we'd have to
     do something to support 14-character-max file names.  */
  for (p = lockfile + strlen (lockfile); p != lockfile && *p != '/'; p--)
    p[2] = *p;

  /* Insert the `.#'.  */
  p[1] = '.';
  p[2] = '#';

  p = p + strlen (p);

  while (lstat (lockfile, &st) == 0 && !S_ISLNK (st.st_mode))
    {
      if (count > 9)
	{
	  *p = '\0';
	  return;
	}
      sprintf (p, ".%d", count++);
    }
}

/* Lock the lock file named LFNAME.
   If FORCE is nonzero, we do so even if it is already locked.
   Return 1 if successful, 0 if not.  */

static int
lock_file_1 (lfname, force)
     char *lfname;
     int force;
{
  register int err;
  time_t boot_time;
  char *user_name;
  char *host_name;
  char *lock_info_str;

  /* Call this first because it can GC.  */
  boot_time = get_boot_time ();

  if (STRINGP (Fuser_login_name (Qnil)))
    user_name = (char *)SDATA (Fuser_login_name (Qnil));
  else
    user_name = "";
  if (STRINGP (Fsystem_name ()))
    host_name = (char *)SDATA (Fsystem_name ());
  else
    host_name = "";
  lock_info_str = (char *)alloca (strlen (user_name) + strlen (host_name)
				  + LOCK_PID_MAX + 30);

  if (boot_time)
    sprintf (lock_info_str, "%s@%s.%lu:%lu", user_name, host_name,
	     (unsigned long) getpid (), (unsigned long) boot_time);
  else
    sprintf (lock_info_str, "%s@%s.%lu", user_name, host_name,
	     (unsigned long) getpid ());

  err = symlink (lock_info_str, lfname);
  if (errno == EEXIST && force)
    {
      unlink (lfname);
      err = symlink (lock_info_str, lfname);
    }

  return err == 0;
}

/* Return 1 if times A and B are no more than one second apart.  */

int
within_one_second (a, b)
     time_t a, b;
{
  return (a - b >= -1 && a - b <= 1);
}

/* Return 0 if nobody owns the lock file LFNAME or the lock is obsolete,
   1 if another process owns it (and set OWNER (if non-null) to info),
   2 if the current process owns it,
   or -1 if something is wrong with the locking mechanism.  */

static int
current_lock_owner (owner, lfname)
     lock_info_type *owner;
     char *lfname;
{
#ifndef index
  extern char *rindex (), *index ();
#endif
  int len, ret;
  int local_owner = 0;
  char *at, *dot, *colon;
  char *lfinfo = 0;
  int bufsize = 50;
  /* Read arbitrarily-long contents of symlink.  Similar code in
     file-symlink-p in fileio.c.  */
  do
    {
      bufsize *= 2;
      lfinfo = (char *) xrealloc (lfinfo, bufsize);
      errno = 0;
      len = readlink (lfname, lfinfo, bufsize);
#ifdef ERANGE
      /* HP-UX reports ERANGE if the buffer is too small.  */
      if (len == -1 && errno == ERANGE)
	len = bufsize;
#endif
    }
  while (len >= bufsize);

  /* If nonexistent lock file, all is well; otherwise, got strange error. */
  if (len == -1)
    {
      xfree (lfinfo);
      return errno == ENOENT ? 0 : -1;
    }

  /* Link info exists, so `len' is its length.  Null terminate.  */
  lfinfo[len] = 0;

  /* Even if the caller doesn't want the owner info, we still have to
     read it to determine return value, so allocate it.  */
  if (!owner)
    {
      owner = (lock_info_type *) alloca (sizeof (lock_info_type));
      local_owner = 1;
    }

  /* Parse USER@HOST.PID:BOOT_TIME.  If can't parse, return -1.  */
  /* The USER is everything before the first @.  */
  at = index (lfinfo, '@');
  dot = rindex (lfinfo, '.');
  if (!at || !dot)
    {
      xfree (lfinfo);
      return -1;
    }
  len = at - lfinfo;
  owner->user = (char *) xmalloc (len + 1);
  strncpy (owner->user, lfinfo, len);
  owner->user[len] = 0;

  /* The PID is everything from the last `.' to the `:'.  */
  owner->pid = atoi (dot + 1);
  colon = dot;
  while (*colon && *colon != ':')
    colon++;
  /* After the `:', if there is one, comes the boot time.  */
  if (*colon == ':')
    owner->boot_time = atoi (colon + 1);
  else
    owner->boot_time = 0;

  /* The host is everything in between.  */
  len = dot - at - 1;
  owner->host = (char *) xmalloc (len + 1);
  strncpy (owner->host, at + 1, len);
  owner->host[len] = 0;

  /* We're done looking at the link info.  */
  xfree (lfinfo);

  /* On current host?  */
  if (STRINGP (Fsystem_name ())
      && strcmp (owner->host, SDATA (Fsystem_name ())) == 0)
    {
      if (owner->pid == getpid ())
        ret = 2; /* We own it.  */
      else if (owner->pid > 0
               && (kill (owner->pid, 0) >= 0 || errno == EPERM)
	       && (owner->boot_time == 0
		   || within_one_second (owner->boot_time, get_boot_time ())))
        ret = 1; /* An existing process on this machine owns it.  */
      /* The owner process is dead or has a strange pid (<=0), so try to
         zap the lockfile.  */
      else if (unlink (lfname) < 0)
        ret = -1;
      else
	ret = 0;
    }
  else
    { /* If we wanted to support the check for stale locks on remote machines,
         here's where we'd do it.  */
      ret = 1;
    }

  /* Avoid garbage.  */
  if (local_owner || ret <= 0)
    {
      FREE_LOCK_INFO (*owner);
    }
  return ret;
}


/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return positive if some other process owns the lock, and info about
     that process in CLASHER.
   Return -1 if cannot lock for any other reason.  */

static int
lock_if_free (clasher, lfname)
     lock_info_type *clasher;
     register char *lfname;
{
  while (lock_file_1 (lfname, 0) == 0)
    {
      int locker;

      if (errno != EEXIST)
	return -1;

      locker = current_lock_owner (clasher, lfname);
      if (locker == 2)
        {
          FREE_LOCK_INFO (*clasher);
          return 0;   /* We ourselves locked it.  */
        }
      else if (locker == 1)
        return 1;  /* Someone else has it.  */
      else if (locker == -1)
	return -1;   /* current_lock_owner returned strange error.  */

      /* We deleted a stale lock; try again to lock the file.  */
    }
  return 0;
}

/* lock_file locks file FN,
   meaning it serves notice on the world that you intend to edit that file.
   This should be done only when about to modify a file-visiting
   buffer previously unmodified.
   Do not (normally) call this for a buffer already modified,
   as either the file is already locked, or the user has already
   decided to go ahead without locking.

   When this returns, either the lock is locked for us,
   or the user has said to go ahead without locking.

   If the file is locked by someone else, this calls
   ask-user-about-lock (a Lisp function) with two arguments,
   the file name and info about the user who did the locking.
   This function can signal an error, or return t meaning
   take away the lock, or return nil meaning ignore the lock.  */

void
lock_file (fn)
     Lisp_Object fn;
{
  register Lisp_Object attack, orig_fn, encoded_fn;
  register char *lfname, *locker;
  lock_info_type lock_info;
  struct gcpro gcpro1;

  /* Don't do locking while dumping Emacs.
     Uncompressing wtmp files uses call-process, which does not work
     in an uninitialized Emacs.  */
  if (! NILP (Vpurify_flag))
    return;

  orig_fn = fn;
  GCPRO1 (fn);
  fn = Fexpand_file_name (fn, Qnil);
  encoded_fn = ENCODE_FILE (fn);

  /* Create the name of the lock-file for file fn */
  MAKE_LOCK_NAME (lfname, encoded_fn);

  /* See if this file is visited and has changed on disk since it was
     visited.  */
  {
    register Lisp_Object subject_buf;

    subject_buf = get_truename_buffer (orig_fn);

    if (!NILP (subject_buf)
	&& NILP (Fverify_visited_file_modtime (subject_buf))
	&& !NILP (Ffile_exists_p (fn)))
      call1 (intern ("ask-user-about-supersession-threat"), fn);

  }
  UNGCPRO;

  /* Try to lock the lock. */
  if (lock_if_free (&lock_info, lfname) <= 0)
    /* Return now if we have locked it, or if lock creation failed */
    return;

  /* Else consider breaking the lock */
  locker = (char *) alloca (strlen (lock_info.user) + strlen (lock_info.host)
			    + LOCK_PID_MAX + 9);
  sprintf (locker, "%s@%s (pid %lu)", lock_info.user, lock_info.host,
           lock_info.pid);
  FREE_LOCK_INFO (lock_info);

  attack = call2 (intern ("ask-user-about-lock"), fn, build_string (locker));
  if (!NILP (attack))
    /* User says take the lock */
    {
      lock_file_1 (lfname, 1);
      return;
    }
  /* User says ignore the lock */
}

void
unlock_file (fn)
     register Lisp_Object fn;
{
  register char *lfname;

  fn = Fexpand_file_name (fn, Qnil);
  fn = ENCODE_FILE (fn);

  MAKE_LOCK_NAME (lfname, fn);

  if (current_lock_owner (0, lfname) == 2)
    unlink (lfname);
}

void
unlock_all_files ()
{
  register Lisp_Object tail;
  register struct buffer *b;

  for (tail = Vbuffer_alist; GC_CONSP (tail); tail = XCDR (tail))
    {
      b = XBUFFER (XCDR (XCAR (tail)));
      if (STRINGP (b->file_truename) && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b))
	{
	  unlock_file(b->file_truename);
	}
    }
}

DEFUN ("lock-buffer", Flock_buffer, Slock_buffer,
       0, 1, 0,
       doc: /* Lock FILE, if current buffer is modified.
FILE defaults to current buffer's visited file,
or else nothing is done if current buffer isn't visiting a file.  */)
     (file)
     Lisp_Object file;
{
  if (NILP (file))
    file = current_buffer->file_truename;
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
     ()
{
  if (SAVE_MODIFF < MODIFF
      && STRINGP (current_buffer->file_truename))
    unlock_file (current_buffer->file_truename);
  return Qnil;
}

/* Unlock the file visited in buffer BUFFER.  */

void
unlock_buffer (buffer)
     struct buffer *buffer;
{
  if (BUF_SAVE_MODIFF (buffer) < BUF_MODIFF (buffer)
      && STRINGP (buffer->file_truename))
    unlock_file (buffer->file_truename);
}

DEFUN ("file-locked-p", Ffile_locked_p, Sfile_locked_p, 1, 1, 0,
       doc: /* Return a value indicating whether FILENAME is locked.
The value is nil if the FILENAME is not locked,
t if it is locked by you, else a string saying which user has locked it.  */)
     (filename)
     Lisp_Object filename;
{
  Lisp_Object ret;
  register char *lfname;
  int owner;
  lock_info_type locker;

  filename = Fexpand_file_name (filename, Qnil);

  MAKE_LOCK_NAME (lfname, filename);

  owner = current_lock_owner (&locker, lfname);
  if (owner <= 0)
    ret = Qnil;
  else if (owner == 2)
    ret = Qt;
  else
    ret = build_string (locker.user);

  if (owner > 0)
    FREE_LOCK_INFO (locker);

  return ret;
}

/* Initialization functions.  */

void
init_filelock ()
{
  boot_time = 0;
  boot_time_initialized = 0;
}

void
syms_of_filelock ()
{
  DEFVAR_LISP ("temporary-file-directory", &Vtemporary_file_directory,
	       doc: /* The directory for writing temporary files.  */);
  Vtemporary_file_directory = Qnil;

  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
  defsubr (&Sfile_locked_p);
}

#endif /* CLASH_DETECTION */

/* arch-tag: e062676d-50b2-4be0-ab96-197c81b181a1
   (do not change this comment) */
