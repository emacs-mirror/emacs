/* Copyright (C) 1985, 86, 87, 93, 94, 96 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include <sys/types.h>
#include <sys/stat.h>
#include <config.h>

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif /* not VMS */

#include <sys/file.h>
#ifdef USG
#include <fcntl.h>
#include <string.h>
#endif /* USG */

#include "lisp.h"
#include "buffer.h"

#include <errno.h>
#ifndef errno
extern int errno;
#endif

#ifdef CLASH_DETECTION
  
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
   environment, it seems such stale locks arise fiarly infrequently, and
   Emacs' standard methods of dealing with clashes suffice.

   We use symlinks instead of normal files because (1) they can be
   stored more efficiently on the filesystem, since the kernel knows
   they will be small, and (2) all the info about the lock can be read
   in a single system call (readlink).  Although we could use regular
   files to be useful on old systems lacking symlinks, noawdays
   virtually all such systems are probably single-user anyway, so it
   didn't seem worth the complication.
   
   Similarly, we don't worry about a possible 14-character limit on
   file names, because those are all the same systems that don't have
   symlinks.
   
   This is compatible with the locking scheme used by Interleaf (which
   has contributed this implementation for Emacs), and was designed by
   Ethan Jacobson, Kimbo Mundy, and others.
   
   --karl@cs.umb.edu/karl@hq.ileaf.com.  */


/* Here is the structure that stores information about a lock.  */

typedef struct
{
  char *user;
  char *host;
  unsigned long pid;
} lock_info_type;

/* When we read the info back, we might need this much more,
   enough for decimal representation plus null.  */
#define LOCK_PID_MAX (4 * sizeof (unsigned long))

/* Free the two dynamically-allocated pieces in PTR.  */
#define FREE_LOCK_INFO(i) do { xfree ((i).user); xfree ((i).host); } while (0)


/* Write the name of the lock file for FN into LFNAME.  Length will be
   that of FN plus two more for the leading `.#' plus one for the null.  */
#define MAKE_LOCK_NAME(lock, file) \
  (lock = (char *) alloca (XSTRING (file)->size + 2 + 1), \
   fill_in_lock_file_name (lock, (file)))

static void
fill_in_lock_file_name (lockfile, fn)
     register char *lockfile;
     register Lisp_Object fn;
{
  register char *p;

  strcpy (lockfile, XSTRING (fn)->data);

  /* Shift the nondirectory part of the file name (including the null)
     right two characters.  Here is one of the places where we'd have to
     do something to support 14-character-max file names.  */
  for (p = lockfile + strlen (lockfile); p != lockfile && *p != '/'; p--)
    p[2] = *p;
  
  /* Insert the `.#'.  */
  p[1] = '.';
  p[2] = '#';
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
  char *user_name = XSTRING (Fuser_login_name (Qnil))->data;
  char *host_name = XSTRING (Fsystem_name ())->data;
  char *lock_info_str = alloca (strlen (user_name) + strlen (host_name)
                                + LOCK_PID_MAX + 5);

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
  int o, p, len, ret;
  int local_owner = 0;
  char *at, *dot;
  char *lfinfo = 0;
  int bufsize = 50;
  /* Read arbitrarily-long contents of symlink.  Similar code in
     file-symlink-p in fileio.c.  */
  do
    {
      bufsize *= 2;
      lfinfo = (char *) xrealloc (lfinfo, bufsize);
      len = readlink (lfname, lfinfo, bufsize);
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
      owner = alloca (sizeof (lock_info_type));
      local_owner = 1;
    }
  
  /* Parse USER@HOST.PID.  If can't parse, return -1.  */
  /* The USER is everything before the first @.  */
  at = index (lfinfo, '@');
  dot = rindex (lfinfo, '.');
  if (!at || !dot) {
    xfree (lfinfo);
    return -1;
  }
  len = at - lfinfo;
  owner->user = (char *) xmalloc (len + 1);
  strncpy (owner->user, lfinfo, len);
  owner->user[len] = 0;
  
  /* The PID is everything after the last `.'.  */
  owner->pid = atoi (dot + 1);

  /* The host is everything in between.  */
  len = dot - at - 1;
  owner->host = (char *) xmalloc (len + 1);
  strncpy (owner->host, at + 1, len);
  owner->host[len] = 0;

  /* We're done looking at the link info.  */
  xfree (lfinfo);
  
  /* On current host?  */
  if (strcmp (owner->host, XSTRING (Fsystem_name ())->data) == 0)
    {
      if (owner->pid == getpid ())
        ret = 2; /* We own it.  */
      
      if (owner->pid > 0
               && (kill (owner->pid, 0) >= 0 || errno == EPERM))
        ret = 1; /* An existing process on this machine owns it.  */
      
      /* The owner process is dead or has a strange pid (<=0), so try to
         zap the lockfile.  */
      if (unlink (lfname) < 0)
        ret = -1;
      
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
        return -1; /* Something's wrong.  */

       /* If some other error, or no such lock, try to lock again.  */
       /* Is there a case where we loop forever?  */
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
    register Lisp_Object fn;
{
  register Lisp_Object attack, orig_fn;
  register char *lfname, *locker;
  lock_info_type lock_info;

  orig_fn = fn;
  fn = Fexpand_file_name (fn, Qnil);

  /* Create the name of the lock-file for file fn */
  MAKE_LOCK_NAME (lfname, fn);

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

  /* Try to lock the lock. */
  if (lock_if_free (&lock_info, lfname) <= 0)
    /* Return now if we have locked it, or if lock creation failed */
    return;

  /* Else consider breaking the lock */
  locker = alloca (strlen (lock_info.user) + strlen (lock_info.host)
                   + LOCK_PID_MAX + 9);
  sprintf (locker, "%s@%s (pid %d)", lock_info.user, lock_info.host,
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

  MAKE_LOCK_NAME (lfname, fn);

  if (current_lock_owner (0, lfname) == 2)
    unlink (lfname);
}

void
unlock_all_files ()
{
  register Lisp_Object tail;
  register struct buffer *b;

  for (tail = Vbuffer_alist; GC_CONSP (tail); tail = XCONS (tail)->cdr)
    {
      b = XBUFFER (XCONS (XCONS (tail)->car)->cdr);
      if (STRINGP (b->file_truename) && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b))
	unlock_file (b->file_truename);
    }
}

DEFUN ("lock-buffer", Flock_buffer, Slock_buffer,
  0, 1, 0,
  "Lock FILE, if current buffer is modified.\n\
FILE defaults to current buffer's visited file,\n\
or else nothing is done if current buffer isn't visiting a file.")
  (file)
     Lisp_Object file;
{
  if (NILP (file))
    file = current_buffer->file_truename;
  else
    CHECK_STRING (file, 0);
  if (SAVE_MODIFF < MODIFF
      && !NILP (file))
    lock_file (file);
  return Qnil;    
}

DEFUN ("unlock-buffer", Funlock_buffer, Sunlock_buffer,
  0, 0, 0,
 "Unlock the file visited in the current buffer,\n\
if it should normally be locked.")
  ()
{
  if (SAVE_MODIFF < MODIFF
      && STRINGP (current_buffer->file_truename))
    unlock_file (current_buffer->file_truename);
  return Qnil;
}

/* Unlock the file visited in buffer BUFFER.  */

unlock_buffer (buffer)
     struct buffer *buffer;
{
  if (BUF_SAVE_MODIFF (buffer) < BUF_MODIFF (buffer)
      && STRINGP (buffer->file_truename))
    unlock_file (buffer->file_truename);
}

DEFUN ("file-locked-p", Ffile_locked_p, Sfile_locked_p, 0, 1, 0,
  "Return nil if the FILENAME is not locked,\n\
t if it is locked by you, else a string of the name of the locker.")
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

init_filelock ()
{
#if 0
  char *new_name;

  lock_dir = egetenv ("EMACSLOCKDIR");
  if (! lock_dir)
    lock_dir = PATH_LOCK;

  /* Copy the name in case egetenv got it from a Lisp string.  */
  new_name = (char *) xmalloc (strlen (lock_dir) + 2);
  strcpy (new_name, lock_dir);
  lock_dir = new_name;

  /* Make sure it ends with a slash.  */
  if (lock_dir[strlen (lock_dir) - 1] != '/')
    strcat (lock_dir, "/");

  superlock_file = (char *) xmalloc ((strlen (lock_dir)
				      + sizeof (SUPERLOCK_NAME)));
  strcpy (superlock_file, lock_dir);
  strcat (superlock_file, SUPERLOCK_NAME);
#endif
}

syms_of_filelock ()
{
  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
  defsubr (&Sfile_locked_p);
}

#endif /* CLASH_DETECTION */
