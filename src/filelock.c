/* Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <sys/types.h>
#include <sys/stat.h>
#include <config.h>

#ifdef VMS
#include "vms-pwd.h"
#else
#include <pwd.h>
#endif

#include <errno.h>
#include <sys/file.h>
#ifdef USG
#include <fcntl.h>
#endif /* USG */

#include "lisp.h"
#include <paths.h>
#include "buffer.h"

#ifdef SYSV_SYSTEM_DIR
#include <dirent.h>
#else /* not SYSV_SYSTEM_DIR */
#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#else /* not NONSYSTEM_DIR_LIBRARY */
#ifdef MSDOS
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#endif /* not NONSYSTEM_DIR_LIBRARY */
#ifndef MSDOS
extern DIR *opendir ();
#endif /* not MSDOS */
#endif /* not SYSV_SYSTEM_DIR */

extern int errno;

extern char *egetenv ();
extern char *strcpy ();

#ifdef DECLARE_GETPWUID_WITH_UID_T
extern struct passwd *getpwuid (uid_t);
#else
extern struct passwd *getpwuid ();
#endif

#ifdef CLASH_DETECTION
  
/* If system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif


/* The name of the directory in which we keep lock files, with a '/'
   appended.  */  
char *lock_dir;

/* The name of the file in the lock directory which is used to
   arbitrate access to the entire directory.  */
#define SUPERLOCK_NAME "!!!SuperLock!!!"

/* The name of the superlock file.  This is SUPERLOCK_NAME appended to
   lock_dir.  */
char *superlock_file;

/* Set LOCK to the name of the lock file for the filename FILE.
   char *LOCK; Lisp_Object FILE;  */

#ifndef HAVE_LONG_FILE_NAMES

#define MAKE_LOCK_NAME(lock, file) \
  (lock = (char *) alloca (14 + strlen (lock_dir) + 1), \
   fill_in_lock_short_file_name (lock, (file)))


fill_in_lock_short_file_name (lockfile, fn)
     register char *lockfile;
     register Lisp_Object fn;
{
  register union
    {
      unsigned int  word [2];
      unsigned char byte [8];
    } crc;
  register unsigned char *p, new;
  
  /* 7-bytes cyclic code for burst correction on byte-by-byte basis.
     the used polynomial is D^7 + D^6 + D^3 +1. pot@cnuce.cnr.it */

  crc.word[0] = crc.word[1] = 0;

  for (p = XSTRING (fn)->data; new = *p++; )
    {
      new += crc.byte[6];
      crc.byte[6] = crc.byte[5] + new;
      crc.byte[5] = crc.byte[4];
      crc.byte[4] = crc.byte[3];
      crc.byte[3] = crc.byte[2] + new;
      crc.byte[2] = crc.byte[1];
      crc.byte[1] = crc.byte[0];
      crc.byte[0] = new;
    }
  sprintf (lockfile, "%s%.2x%.2x%.2x%.2x%.2x%.2x%.2x", lock_dir,
	   crc.byte[0], crc.byte[1], crc.byte[2], crc.byte[3],
	   crc.byte[4], crc.byte[5], crc.byte[6]);
}

#else /* defined HAVE_LONG_FILE_NAMES */

#define MAKE_LOCK_NAME(lock, file) \
  (lock = (char *) alloca (XSTRING (file)->size + strlen (lock_dir) + 1), \
   fill_in_lock_file_name (lock, (file)))


fill_in_lock_file_name (lockfile, fn)
     register char *lockfile;
     register Lisp_Object fn;
{
  register char *p;

  strcpy (lockfile, lock_dir);

  p = lockfile + strlen (lockfile);

  strcpy (p, XSTRING (fn)->data);

  for (; *p; p++)
    {
      if (*p == '/')
	*p = '!';
    }
}
#endif /* !defined HAVE_LONG_FILE_NAMES */

static Lisp_Object
lock_file_owner_name (lfname)
     char *lfname;
{
  struct stat s;
  struct passwd *the_pw;

  if (lstat (lfname, &s) == 0)
    the_pw = getpwuid (s.st_uid);
  return (the_pw == 0 ? Qnil : build_string (the_pw->pw_name));
}


/* lock_file locks file fn,
   meaning it serves notice on the world that you intend to edit that file.
   This should be done only when about to modify a file-visiting
   buffer previously unmodified.
   Do not (normally) call lock_buffer for a buffer already modified,
   as either the file is already locked, or the user has already
   decided to go ahead without locking.

   When lock_buffer returns, either the lock is locked for us,
   or the user has said to go ahead without locking.

   If the file is locked by someone else, lock_buffer calls
   ask-user-about-lock (a Lisp function) with two arguments,
   the file name and the name of the user who did the locking.
   This function can signal an error, or return t meaning
   take away the lock, or return nil meaning ignore the lock.  */

/* The lock file name is the file name with "/" replaced by "!"
   and put in the Emacs lock directory.  */
/* (ie., /ka/king/junk.tex -> /!/!ka!king!junk.tex). */

/* If HAVE_LONG_FILE_NAMES is not defined, the lock file name is the hex
   representation of a 14-bytes CRC generated from the file name
   and put in the Emacs lock directory (not very nice, but it works).
   (ie., /ka/king/junk.tex -> /!/12a82c62f1c6da). */

void
lock_file (fn)
     register Lisp_Object fn;
{
  register Lisp_Object attack;
  register char *lfname;

  MAKE_LOCK_NAME (lfname, fn);

  /* See if this file is visited and has changed on disk since it was
     visited.  */
  {
    register Lisp_Object subject_buf;
    subject_buf = Fget_file_buffer (fn);
    if (!NILP (subject_buf)
	&& NILP (Fverify_visited_file_modtime (subject_buf))
	&& !NILP (Ffile_exists_p (fn)))
      call1 (intern ("ask-user-about-supersession-threat"), fn);
  }

  /* Try to lock the lock. */
  if (lock_if_free (lfname) <= 0)
    /* Return now if we have locked it, or if lock dir does not exist */
    return;

  /* Else consider breaking the lock */
  attack = call2 (intern ("ask-user-about-lock"), fn,
		  lock_file_owner_name (lfname));
  if (!NILP (attack))
    /* User says take the lock */
    {
      lock_superlock (lfname);
      lock_file_1 (lfname, O_WRONLY) ;
      unlink (superlock_file);
      return;
    }
  /* User says ignore the lock */
}

/* Lock the lock file named LFNAME.
   If MODE is O_WRONLY, we do so even if it is already locked.
   If MODE is O_WRONLY | O_EXCL | O_CREAT, we do so only if it is free.
   Return 1 if successful, 0 if not.  */

int
lock_file_1 (lfname, mode)
     int mode; char *lfname; 
{
  register int fd;
  char buf[20];

  if ((fd = open (lfname, mode, 0666)) >= 0)
    {
#ifdef USG
      chmod (lfname, 0666);
#else
      fchmod (fd, 0666);
#endif
      sprintf (buf, "%d ", getpid ());
      write (fd, buf, strlen (buf));
      close (fd);
      return 1;
    }
  else
    return 0;
}

/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return positive if lock is really locked by someone else.
   Return -1 if cannot lock for any other reason.  */

int
lock_if_free (lfname)
     register char *lfname; 
{
  register int clasher;

  while (lock_file_1 (lfname, O_WRONLY | O_EXCL | O_CREAT) == 0)
    {
      if (errno != EEXIST)
	return -1;
      clasher = current_lock_owner (lfname);
      if (clasher != 0)
	if (clasher != getpid ())
	  return (clasher);
	else return (0);
      /* Try again to lock it */
    }
  return 0;
}

/* Return the pid of the process that claims to own the lock file LFNAME,
   or 0 if nobody does or the lock is obsolete,
   or -1 if something is wrong with the locking mechanism.  */

int
current_lock_owner (lfname)
     char *lfname;
{
  int owner = current_lock_owner_1 (lfname);
  if (owner == 0 && errno == ENOENT)
    return (0);
  /* Is it locked by a process that exists?  */
  if (owner != 0 && (kill (owner, 0) >= 0 || errno == EPERM))
    return (owner);
  if (unlink (lfname) < 0)
    return (-1);
  return (0);
}

int
current_lock_owner_1 (lfname)
     char *lfname;
{
  register int fd;
  char buf[20];
  int tem;

  fd = open (lfname, O_RDONLY, 0666);
  if (fd < 0)
    return 0;
  tem = read (fd, buf, sizeof buf);
  close (fd);
  return (tem <= 0 ? 0 : atoi (buf));
}


void
unlock_file (fn)
     register Lisp_Object fn;
{
  register char *lfname;

  MAKE_LOCK_NAME (lfname, fn);

  lock_superlock (lfname);

  if (current_lock_owner_1 (lfname) == getpid ())
    unlink (lfname);

  unlink (superlock_file);
}

lock_superlock (lfname)
     char *lfname;
{
  register int i, fd;
  DIR *lockdir;

  for (i = -20; i < 0 && (fd = open (superlock_file,
				     O_WRONLY | O_EXCL | O_CREAT, 0666)) < 0;
       i++)
    {
      if (errno != EEXIST)
	return;

      /* This seems to be necessary to prevent Emacs from hanging when the
	 competing process has already deleted the superlock, but it's still
	 in the NFS cache.  So we force NFS to synchronize the cache.  */
      if (lockdir = opendir (lock_dir))
	closedir (lockdir);

      sleep (1);
    }
  if (fd >= 0)
    {
#ifdef USG
      chmod (superlock_file, 0666);
#else
      fchmod (fd, 0666);
#endif
      write (fd, lfname, strlen (lfname));
      close (fd);
    }
}

void
unlock_all_files ()
{
  register Lisp_Object tail;
  register struct buffer *b;

  for (tail = Vbuffer_alist; GC_CONSP (tail); tail = XCONS (tail)->cdr)
    {
      b = XBUFFER (XCONS (XCONS (tail)->car)->cdr);
      if (STRINGP (b->filename) && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b))
	unlock_file (b->filename);
    }
}


DEFUN ("lock-buffer", Flock_buffer, Slock_buffer,
  0, 1, 0,
  "Lock FILE, if current buffer is modified.\n\
FILE defaults to current buffer's visited file,\n\
or else nothing is done if current buffer isn't visiting a file.")
  (fn)
     Lisp_Object fn;
{
  if (NILP (fn))
    fn = current_buffer->filename;
  else
    CHECK_STRING (fn, 0);
  if (SAVE_MODIFF < MODIFF
      && !NILP (fn))
    lock_file (fn);
  return Qnil;    
}

DEFUN ("unlock-buffer", Funlock_buffer, Sunlock_buffer,
  0, 0, 0,
 "Unlock the file visited in the current buffer,\n\
if it should normally be locked.")
  ()
{
  if (SAVE_MODIFF < MODIFF
      && STRINGP (current_buffer->filename))
    unlock_file (current_buffer->filename);
  return Qnil;
}


/* Unlock the file visited in buffer BUFFER.  */

unlock_buffer (buffer)
     struct buffer *buffer;
{
  if (BUF_SAVE_MODIFF (buffer) < BUF_MODIFF (buffer)
      && STRINGP (buffer->filename))
    unlock_file (buffer->filename);
}

DEFUN ("file-locked-p", Ffile_locked_p, Sfile_locked_p, 0, 1, 0,
  "Return nil if the FILENAME is not locked,\n\
t if it is locked by you, else a string of the name of the locker.")
  (fn)
  Lisp_Object fn;
{
  register char *lfname;
  int owner;

  fn = Fexpand_file_name (fn, Qnil);

  MAKE_LOCK_NAME (lfname, fn);

  owner = current_lock_owner (lfname);
  if (owner <= 0)
    return (Qnil);
  else if (owner == getpid ())
    return (Qt);
  
  return (lock_file_owner_name (lfname));
}


/* Initialization functions.  */

init_filelock ()
{
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
}

syms_of_filelock ()
{
  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
  defsubr (&Sfile_locked_p);
}

#endif /* CLASH_DETECTION */
