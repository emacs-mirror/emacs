/* Unix emulation routines for GNU Emacs on the Mac OS.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <utime.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <pwd.h>
#include <sys/param.h>
#if __MWERKS__
#include <unistd.h>
#endif

#ifdef MAC_OSX
#undef mktime
#undef DEBUG
#undef free
#undef malloc
#undef realloc
#include <Carbon/Carbon.h>
#undef free
#define free unexec_free
#undef malloc
#define malloc unexec_malloc
#undef realloc
#define realloc unexec_realloc
#else /* not MAC_OSX */
#include <Files.h>
#include <MacTypes.h>
#include <TextUtils.h>
#include <Folders.h>
#include <Resources.h>
#include <Aliases.h>
#include <FixMath.h>
#include <Timer.h>
#include <OSA.h>
#include <AppleScript.h>
#include <Scrap.h>
#endif /* not MAC_OSX */

#include "lisp.h"
#include "process.h"
#include "sysselect.h"
#include "systime.h"

Lisp_Object QCLIPBOARD;

/* An instance of the AppleScript component.  */
static ComponentInstance as_scripting_component;
/* The single script context used for all script executions.  */
static OSAID as_script_context;


/* When converting from Mac to Unix pathnames, /'s in folder names are
   converted to :'s.  This function, used in copying folder names,
   performs a strncat and converts all character a to b in the copy of
   the string s2 appended to the end of s1.  */

void
string_cat_and_replace (char *s1, const char *s2, int n, char a, char b)
{
  int l1 = strlen (s1);
  int l2 = strlen (s2);
  char *p = s1 + l1;
  int i;
  
  strncat (s1, s2, n);
  for (i = 0; i < l2; i++)
    {
      if (*p == a)
        *p = b;
      p++;
    }
}


/* Convert a Mac pathname to Posix form.  A Mac full pathname is one
   that does not begin with a ':' and contains at least one ':'. A Mac
   full pathname causes an '/' to be prepended to the Posix pathname.
   The algorithm for the rest of the pathname is as follows:
     For each segment between two ':',
       if it is non-null, copy as is and then add a '/' at the end,
       otherwise, insert a "../" into the Posix pathname.
   Returns 1 if successful; 0 if fails.  */
   
int
mac_to_posix_pathname (const char *mfn, char *ufn, int ufnbuflen)
{
  const char *p, *q, *pe;
	
  strcpy (ufn, "");
	
  if (*mfn == '\0')
    return 1;
	
  p = strchr (mfn, ':');
  if (p != 0 && p != mfn)  /* full pathname */
    strcat (ufn, "/");
		
  p = mfn;
  if (*p == ':')
    p++;

  pe = mfn + strlen (mfn);
  while (p < pe)
    {
      q = strchr (p, ':');
      if (q)
	{
	  if (q == p)
	    {  /* two consecutive ':' */
	      if (strlen (ufn) + 3 >= ufnbuflen)
		return 0;
	      strcat (ufn, "../");
	    }
	  else
	    {
	      if (strlen (ufn) + (q - p) + 1 >= ufnbuflen)
		return 0;
	      string_cat_and_replace (ufn, p, q - p, '/', ':');
	      strcat (ufn, "/");
	    }
	  p = q + 1;
	}
      else
	{
	  if (strlen (ufn) + (pe - p) >= ufnbuflen)
	    return 0;
	  string_cat_and_replace (ufn, p, pe - p, '/', ':');
	    /* no separator for last one */
	  p = pe;
	}
    }
	
  return 1;
}


extern char *get_temp_dir_name ();


/* Convert a Posix pathname to Mac form.  Approximately reverse of the
   above in algorithm.  */
   
int
posix_to_mac_pathname (const char *ufn, char *mfn, int mfnbuflen)
{
  const char *p, *q, *pe;
  char expanded_pathname[MAXPATHLEN+1];
	
  strcpy (mfn, "");
	
  if (*ufn == '\0')
    return 1;

  p = ufn;
  
  /* Check for and handle volume names.  Last comparison: strangely
     somewhere "/.emacs" is passed.  A temporary fix for now.  */
  if (*p == '/' && strchr (p+1, '/') == NULL && strcmp (p, "/.emacs") != 0)
    {
      if (strlen (p) + 1 > mfnbuflen)
	return 0;
      strcpy (mfn, p+1);
      strcat (mfn, ":");
      return 1;
    }

  /* expand to emacs dir found by init_emacs_passwd_dir */
  if (strncmp (p, "~emacs/", 7) == 0)
    {
      struct passwd *pw = getpwnam ("emacs");
      p += 7;
      if (strlen (pw->pw_dir) + strlen (p) > MAXPATHLEN)
	return 0;
      strcpy (expanded_pathname, pw->pw_dir);
      strcat (expanded_pathname, p);
      p = expanded_pathname;
        /* now p points to the pathname with emacs dir prefix */
    }
  else if (strncmp (p, "/tmp/", 5) == 0)
    {
      char *t = get_temp_dir_name ();
      p += 5;
      if (strlen (t) + strlen (p) > MAXPATHLEN)
	return 0;
      strcpy (expanded_pathname, t);
      strcat (expanded_pathname, p);
      p = expanded_pathname;
        /* now p points to the pathname with emacs dir prefix */
    }    
  else if (*p != '/')  /* relative pathname */
    strcat (mfn, ":");
		
  if (*p == '/')
    p++;

  pe = p + strlen (p);
  while (p < pe)
    {
      q = strchr (p, '/');
      if (q)
	{
	  if (q - p == 2 && *p == '.' && *(p+1) == '.')
	    {
	      if (strlen (mfn) + 1 >= mfnbuflen)
		return 0;
	      strcat (mfn, ":");
	    }
	  else
	    {
	      if (strlen (mfn) + (q - p) + 1 >= mfnbuflen)
		return 0;
	      string_cat_and_replace (mfn, p, q - p, ':', '/');
	      strcat (mfn, ":");
	    }
	  p = q + 1;
	}
      else
	{
	  if (strlen (mfn) + (pe - p) >= mfnbuflen)
	    return 0;
	  string_cat_and_replace (mfn, p, pe - p, ':', '/');
	  p = pe;
	}
    }
	
  return 1;
}

#ifndef MAC_OSX

/* The following functions with "sys_" prefix are stubs to Unix
   functions that have already been implemented by CW or MPW.  The
   calls to them in Emacs source course are #define'd to call the sys_
   versions by the header files s-mac.h.  In these stubs pathnames are
   converted between their Unix and Mac forms.  */


/* Unix epoch is Jan 1, 1970 while Mac epoch is Jan 1, 1904: 66 years
   + 17 leap days.  These are for adjusting time values returned by
   MacOS Toolbox functions.  */

#define MAC_UNIX_EPOCH_DIFF  ((365L * 66 + 17) * 24 * 60 * 60)

#ifdef __MWERKS__
#if __MSL__ < 0x6000
/* CW Pro 5 epoch is Jan 1, 1900 (aaarghhhhh!); remember, 1900 is not
   a leap year!  This is for adjusting time_t values returned by MSL
   functions.  */
#define CW_OR_MPW_UNIX_EPOCH_DIFF ((365L * 70 + 17) * 24 * 60 * 60)
#else /* __MSL__ >= 0x6000 */
/* CW changes Pro 6 to follow Unix!  */
#define CW_OR_MPW_UNIX_EPOCH_DIFF ((365L * 66 + 17) * 24 * 60 * 60)
#endif /* __MSL__ >= 0x6000 */
#elif __MRC__
/* MPW library functions follow Unix (confused?).  */
#define CW_OR_MPW_UNIX_EPOCH_DIFF ((365L * 66 + 17) * 24 * 60 * 60)
#else /* not __MRC__ */
You lose!!!
#endif /* not __MRC__ */


/* Define our own stat function for both MrC and CW.  The reason for
   doing this: "stat" is both the name of a struct and function name:
   can't use the same trick like that for sys_open, sys_close, etc. to
   redirect Emacs's calls to our own version that converts Unix style
   filenames to Mac style filename because all sorts of compilation
   errors will be generated if stat is #define'd to be sys_stat.  */

int
stat_noalias (const char *path, struct stat *buf)
{
  char mac_pathname[MAXPATHLEN+1];
  CInfoPBRec cipb;

  if (posix_to_mac_pathname (path, mac_pathname, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */
  
  errno = PBGetCatInfo (&cipb, false);
  if (errno == -43) /* -43: fnfErr defined in Errors.h */
    errno = ENOENT;
  if (errno != noErr)
    return -1;

  if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* bit 4 = 1 for directories */
    {
      buf->st_mode = S_IFDIR | S_IREAD | S_IEXEC;
      
      if (!(cipb.hFileInfo.ioFlAttrib & 0x1))
	buf->st_mode |= S_IWRITE;  /* bit 1 = 1 for locked files/directories */
      buf->st_ino = cipb.dirInfo.ioDrDirID;
      buf->st_dev = cipb.dirInfo.ioVRefNum;
      buf->st_size = cipb.dirInfo.ioDrNmFls;
        /* size of dir = number of files and dirs */
      buf->st_atime
	= buf->st_mtime
	= cipb.dirInfo.ioDrMdDat - MAC_UNIX_EPOCH_DIFF;
      buf->st_ctime = cipb.dirInfo.ioDrCrDat - MAC_UNIX_EPOCH_DIFF;
    }
  else
    {
      buf->st_mode = S_IFREG | S_IREAD;
      if (!(cipb.hFileInfo.ioFlAttrib & 0x1))
	buf->st_mode |= S_IWRITE;  /* bit 1 = 1 for locked files/directories */
      if (cipb.hFileInfo.ioFlFndrInfo.fdType == 'APPL')
	buf->st_mode |= S_IEXEC;
      buf->st_ino = cipb.hFileInfo.ioDirID;
      buf->st_dev = cipb.hFileInfo.ioVRefNum;
      buf->st_size = cipb.hFileInfo.ioFlLgLen;
      buf->st_atime
	= buf->st_mtime
	= cipb.hFileInfo.ioFlMdDat - MAC_UNIX_EPOCH_DIFF;
      buf->st_ctime = cipb.hFileInfo.ioFlCrDat - MAC_UNIX_EPOCH_DIFF;
    }

  if (cipb.hFileInfo.ioFlFndrInfo.fdFlags & 0x8000)
    {
      /* identify alias files as symlinks */
      buf->st_mode &= ~S_IFREG;
      buf->st_mode |= S_IFLNK;
    }

  buf->st_nlink = 1;
  buf->st_uid = getuid ();
  buf->st_gid = getgid ();
  buf->st_rdev = 0;

  return 0;
}


int
lstat (const char *path, struct stat *buf)
{
  int result;
  char true_pathname[MAXPATHLEN+1];

  /* Try looking for the file without resolving aliases first.  */
  if ((result = stat_noalias (path, buf)) >= 0)
    return result;

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  return stat_noalias (true_pathname, buf);
}


int
stat (const char *path, struct stat *sb)
{
  int result;
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  int len;
  
  if ((result = stat_noalias (path, sb)) >= 0 &&
      ! (sb->st_mode & S_IFLNK))
    return result;

  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    {
      fully_resolved_name[len] = '\0';
        /* in fact our readlink terminates strings */
      return lstat (fully_resolved_name, sb);
    }
  else
    return lstat (true_pathname, sb);
}


#if __MRC__
/* CW defines fstat in stat.mac.c while MPW does not provide this
   function.  Without the information of how to get from a file
   descriptor in MPW StdCLib to a Mac OS file spec, it should be hard
   to implement this function.  Fortunately, there is only one place
   where this function is called in our configuration: in fileio.c,
   where only the st_dev and st_ino fields are used to determine
   whether two fildes point to different i-nodes to prevent copying
   a file onto itself equal.  What we have here probably needs
   improvement.  */

int
fstat (int fildes, struct stat *buf)
{
  buf->st_dev = 0;
  buf->st_ino = fildes;
  buf->st_mode = S_IFREG;  /* added by T.I. for the copy-file */
  return 0;  /* success */
}
#endif  /* __MRC__ */


int
mkdir (const char *dirname, int mode)
{
#pragma unused(mode)

  HFileParam hfpb;
  char true_pathname[MAXPATHLEN+1], mac_pathname[MAXPATHLEN+1];
  
  if (find_true_pathname (dirname, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
	
  if (posix_to_mac_pathname (true_pathname, mac_pathname, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_pathname);
  hfpb.ioNamePtr = mac_pathname;
  hfpb.ioVRefNum = 0;  /* ignored unless name is invalid */
  hfpb.ioDirID = 0;  /* parent is the root */
  
  errno = PBDirCreate ((HParmBlkPtr) &hfpb, false);
    /* just return the Mac OSErr code for now */
  return errno == noErr ? 0 : -1;
}


#undef rmdir
sys_rmdir (const char *dirname)
{
  HFileParam hfpb;
  char mac_pathname[MAXPATHLEN+1];
	
  if (posix_to_mac_pathname (dirname, mac_pathname, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_pathname);
  hfpb.ioNamePtr = mac_pathname;
  hfpb.ioVRefNum = 0;  /* ignored unless name is invalid */
  hfpb.ioDirID = 0;  /* parent is the root */
  
  errno = PBHDelete ((HParmBlkPtr) &hfpb, false);
  return errno == noErr ? 0 : -1;
}


#ifdef __MRC__
/* No implementation yet. */
int
execvp (const char *path, ...)
{
  return -1;
}
#endif /* __MRC__ */


int
utime (const char *path, const struct utimbuf *times)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  int len;
  char mac_pathname[MAXPATHLEN+1];
  CInfoPBRec cipb;
  
  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0; 
    /* set to 0 to get information about specific dir or file */
  
  errno = PBGetCatInfo (&cipb, false);
  if (errno != noErr)
    return -1;

  if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* bit 4 = 1 for directories */
    {
      if (times)
	cipb.dirInfo.ioDrMdDat = times->modtime + MAC_UNIX_EPOCH_DIFF;
      else
	GetDateTime (&cipb.dirInfo.ioDrMdDat);
    }
  else
    {
      if (times)
	cipb.hFileInfo.ioFlMdDat = times->modtime + MAC_UNIX_EPOCH_DIFF;
      else
	GetDateTime (&cipb.hFileInfo.ioFlMdDat);
    }

  errno = PBSetCatInfo (&cipb, false);
  return errno == noErr ? 0 : -1;
}


#ifndef F_OK
#define F_OK 0
#endif
#ifndef X_OK
#define X_OK 1
#endif
#ifndef W_OK
#define W_OK 2
#endif

/* Like stat, but test for access mode in hfpb.ioFlAttrib */
int
access (const char *path, int mode)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  int len;
  char mac_pathname[MAXPATHLEN+1];
  CInfoPBRec cipb;
  
  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */
  
  errno = PBGetCatInfo (&cipb, false);
  if (errno != noErr)
    return -1;

  if (mode == F_OK)  /* got this far, file exists */
    return 0;

  if (mode & X_OK)
    if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* path refers to a directory */
      return 0;
    else
      {
	if (cipb.hFileInfo.ioFlFndrInfo.fdType == 'APPL')
	  return 0;
	else
	  return -1;
      }

  if (mode & W_OK)
    return (cipb.hFileInfo.ioFlAttrib & 0x1) ? -1 : 0;
      /* don't allow if lock bit is on */

  return -1;
}


#define DEV_NULL_FD 0x10000

#undef open
int
sys_open (const char *path, int oflag)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  int len;
  char mac_pathname[MAXPATHLEN+1];
	
  if (strcmp (path, "/dev/null") == 0)
    return DEV_NULL_FD;  /* some bogus fd to be ignored in write */
  
  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;
  else
    {
#ifdef __MRC__
      int res = open (mac_pathname, oflag);
      /* if (oflag == O_WRONLY || oflag == O_RDWR) */
      if (oflag & O_CREAT)
        fsetfileinfo (mac_pathname, 'EMAx', 'TEXT');
      return res;
#else /* not __MRC__ */
      return open (mac_pathname, oflag);
#endif /* not __MRC__ */
    }
}


#undef creat
int
sys_creat (const char *path, mode_t mode)
{
  char true_pathname[MAXPATHLEN+1];  
  int len;
  char mac_pathname[MAXPATHLEN+1];
	
  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;

  if (!posix_to_mac_pathname (true_pathname, mac_pathname, MAXPATHLEN+1))
    return -1;
  else
    {
#ifdef __MRC__
      int result = creat (mac_pathname);
      fsetfileinfo (mac_pathname, 'EMAx', 'TEXT');
      return result;
#else /* not __MRC__ */
      return creat (mac_pathname, mode);
#endif /* not __MRC__ */
    }
}


#undef unlink
int
sys_unlink (const char *path)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  int len;
  char mac_pathname[MAXPATHLEN+1];
	
  if (find_true_pathname (path, true_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return -1;
  else
    return unlink (mac_pathname);
}


#undef read
int
sys_read (int fildes, char *buf, int count)
{
  if (fildes == 0)  /* this should not be used for console input */
    return -1;
  else
#if __MSL__ >= 0x6000
    return _read (fildes, buf, count);
#else
    return read (fildes, buf, count);
#endif
}


#undef write
int
sys_write (int fildes, const char *buf, int count)
{
  if (fildes == DEV_NULL_FD)
    return count;
  else
#if __MSL__ >= 0x6000
    return _write (fildes, buf, count);
#else
    return write (fildes, buf, count);
#endif
}


#undef rename
int
sys_rename (const char * old_name, const char * new_name)
{
  char true_old_pathname[MAXPATHLEN+1], true_new_pathname[MAXPATHLEN+1];
  char fully_resolved_old_name[MAXPATHLEN+1];  
  int len;
  char mac_old_name[MAXPATHLEN+1], mac_new_name[MAXPATHLEN+1];
	
  if (find_true_pathname (old_name, true_old_pathname, MAXPATHLEN+1) == -1)
    return -1;
  
  len = readlink (true_old_pathname, fully_resolved_old_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_old_name[len] = '\0';
  else
    strcpy (fully_resolved_old_name, true_old_pathname);

  if (find_true_pathname (new_name, true_new_pathname, MAXPATHLEN+1) == -1)
    return -1;
	
  if (strcmp (fully_resolved_old_name, true_new_pathname) == 0)
    return 0;

  if (!posix_to_mac_pathname (fully_resolved_old_name,
			     mac_old_name,
			     MAXPATHLEN+1))
    return -1;
		
  if (!posix_to_mac_pathname(true_new_pathname, mac_new_name, MAXPATHLEN+1))
    return -1;

  /* If a file with new_name already exists, rename deletes the old
     file in Unix.  CW version fails in these situation.  So we add a
     call to unlink here.  */
  (void) unlink (mac_new_name);
  
  return rename (mac_old_name, mac_new_name);
}


#undef fopen
extern FILE *fopen (const char *name, const char *mode);
FILE *
sys_fopen (const char *name, const char *mode)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  int len;
  char mac_pathname[MAXPATHLEN+1];
	
  if (find_true_pathname (name, true_pathname, MAXPATHLEN+1) == -1)
    return 0;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return 0;
  else
    {
#ifdef __MRC__
      if (mode[0] == 'w' || mode[0] == 'a')
        fsetfileinfo (mac_pathname, 'EMAx', 'TEXT');
#endif /* not __MRC__ */
      return fopen (mac_pathname, mode);
    }
}


#include <Events.h>

long target_ticks = 0;

#ifdef __MRC__
__sigfun alarm_signal_func = (__sigfun) 0;
#elif __MWERKS__
__signal_func_ptr alarm_signal_func = (__signal_func_ptr) 0;
#else /* not __MRC__ and not __MWERKS__ */
You lose!!!
#endif /* not __MRC__ and not __MWERKS__ */


/* These functions simulate SIG_ALRM.  The stub for function signal
   stores the signal handler function in alarm_signal_func if a
   SIG_ALRM is encountered.  check_alarm is called in XTread_socket,
   which emacs calls periodically.  A pending alarm is represented by
   a non-zero target_ticks value.  check_alarm calls the handler
   function pointed to by alarm_signal_func if one has been set up and
   an alarm is pending.  */

void
check_alarm ()
{
  if (target_ticks && TickCount () > target_ticks)
    {
      target_ticks = 0;
      if (alarm_signal_func)
	(*alarm_signal_func)(SIGALRM);
    }
}


int
select (n,  rfds, wfds, efds, timeout)
  int n;
  SELECT_TYPE *rfds;
  SELECT_TYPE *wfds;
  SELECT_TYPE *efds;
  struct timeval *timeout;
{
#ifdef TARGET_API_MAC_CARBON
  return 1;
#else /* not TARGET_API_MAC_CARBON */
  EMACS_TIME end_time, now;
  EventRecord e;

  /* Can only handle wait for keyboard input.  */
  if (n > 1 || wfds || efds)
    return -1;

  EMACS_GET_TIME (end_time);
  EMACS_ADD_TIME (end_time, end_time, *timeout);
  
  do
    {
      /* Also return true if an event other than a keyDown has
         occurred.  This causes kbd_buffer_get_event in keyboard.c to
         call read_avail_input which in turn calls XTread_socket to
         poll for these events.  Otherwise these never get processed
         except but a very slow poll timer.  */
      if (FD_ISSET (0, rfds) && EventAvail (everyEvent, &e))
        return 1;

      /* Also check movement of the mouse.  */
      {
        Point mouse_pos;
        static Point old_mouse_pos = {-1, -1};
        
        GetMouse (&mouse_pos);
        if (!EqualPt (mouse_pos, old_mouse_pos))
          {
            old_mouse_pos = mouse_pos;
            return 1;
          }
      }
      
      WaitNextEvent (0, &e, 1UL, NULL);	/* Accept no event; wait 1
					   tic. by T.I. */
      
      EMACS_GET_TIME (now);
      EMACS_SUB_TIME (now, end_time, now);
    }
  while (!EMACS_TIME_NEG_P (now));

  return 0;
#endif /* not TARGET_API_MAC_CARBON */
}


/* Called in sys_select to wait for an alarm signal to arrive.  */

int
pause ()
{
  EventRecord e;
  unsigned long tick;
  
  if (!target_ticks)  /* no alarm pending */
    return -1;

  if ((tick = TickCount ()) < target_ticks)
    WaitNextEvent (0, &e, target_ticks - tick, NULL); /* Accept no event;
							 just wait. by T.I. */
  
  target_ticks = 0;
  if (alarm_signal_func)
    (*alarm_signal_func)(SIGALRM);
  
  return 0;
}


int
alarm (int seconds)
{
  long remaining = target_ticks ? (TickCount () - target_ticks) / 60 : 0;
	
  target_ticks = seconds ? TickCount () + 60 * seconds : 0;
	
  return (remaining < 0) ? 0 : (unsigned int) remaining;
}


#undef signal
#ifdef __MRC__
extern __sigfun signal (int signal, __sigfun signal_func);
__sigfun
sys_signal (int signal_num, __sigfun signal_func)
#elif __MWERKS__
extern __signal_func_ptr signal (int signal, __signal_func_ptr signal_func);
__signal_func_ptr
sys_signal (int signal_num, __signal_func_ptr signal_func)
#else /* not __MRC__ and not __MWERKS__ */
     You lose!!!
#endif /* not __MRC__ and not __MWERKS__ */
{
  if (signal_num != SIGALRM)
    return signal (signal_num, signal_func);
  else
    {
#ifdef __MRC__
      __sigfun old_signal_func;		
#elif __MWERKS__
      __signal_func_ptr old_signal_func;		
#else
      You lose!!!
#endif
      old_signal_func = alarm_signal_func;
      alarm_signal_func = signal_func;
      return old_signal_func;
    }
}


/* gettimeofday should return the amount of time (in a timeval
   structure) since midnight today.  The toolbox function Microseconds
   returns the number of microseconds (in a UnsignedWide value) since
   the machine was booted.  Also making this complicated is WideAdd,
   WideSubtract, etc.  take wide values.  */

int
gettimeofday (tp)
     struct timeval *tp;
{
  static inited = 0;
  static wide wall_clock_at_epoch, clicks_at_epoch;
  UnsignedWide uw_microseconds;
  wide w_microseconds;
  time_t sys_time (time_t *);

  /* If this function is called for the first time, record the number
     of seconds since midnight and the number of microseconds since
     boot at the time of this first call.  */
  if (!inited)
    {
      time_t systime;
      inited = 1;
      systime = sys_time (NULL);
      /* Store microseconds since midnight in wall_clock_at_epoch.  */
      WideMultiply (systime, 1000000L, &wall_clock_at_epoch);
      Microseconds (&uw_microseconds);
      /* Store microseconds since boot in clicks_at_epoch.  */
      clicks_at_epoch.hi = uw_microseconds.hi;
      clicks_at_epoch.lo = uw_microseconds.lo;
    }

  /* Get time since boot */
  Microseconds (&uw_microseconds);
  
  /* Convert to time since midnight*/
  w_microseconds.hi = uw_microseconds.hi;
  w_microseconds.lo = uw_microseconds.lo;
  WideSubtract (&w_microseconds, &clicks_at_epoch);
  WideAdd (&w_microseconds, &wall_clock_at_epoch);
  tp->tv_sec = WideDivide (&w_microseconds, 1000000L, &tp->tv_usec);

  return 0;
}


#ifdef __MRC__
unsigned int
sleep (unsigned int seconds)
{
  unsigned long time_up;
  EventRecord e;

  time_up = TickCount () + seconds * 60;
  while (TickCount () < time_up)
    {
      /* Accept no event; just wait. by T.I.  */
      WaitNextEvent (0, &e, 30, NULL);
    }

  return (0);
}
#endif /* __MRC__ */


/* The time functions adjust time values according to the difference
   between the Unix and CW epoches. */

#undef gmtime
extern struct tm *gmtime (const time_t *);
struct tm *
sys_gmtime (const time_t *timer)
{
  time_t unix_time = *timer + CW_OR_MPW_UNIX_EPOCH_DIFF;
  
  return gmtime (&unix_time);
}


#undef localtime
extern struct tm *localtime (const time_t *);
struct tm *
sys_localtime (const time_t *timer)
{
#if __MSL__ >= 0x6000
  time_t unix_time = *timer;
#else
  time_t unix_time = *timer + CW_OR_MPW_UNIX_EPOCH_DIFF;
#endif
  
  return localtime (&unix_time);
}


#undef ctime
extern char *ctime (const time_t *);
char *
sys_ctime (const time_t *timer)
{
#if __MSL__ >= 0x6000
  time_t unix_time = *timer;
#else
  time_t unix_time = *timer + CW_OR_MPW_UNIX_EPOCH_DIFF;
#endif
  
  return ctime (&unix_time);
}


#undef time
extern time_t time (time_t *);
time_t
sys_time (time_t *timer)
{
#if __MSL__ >= 0x6000
  time_t mac_time = time (NULL);
#else
  time_t mac_time = time (NULL) - CW_OR_MPW_UNIX_EPOCH_DIFF;
#endif

  if (timer)
    *timer = mac_time;
    
  return mac_time;
}


/* MPW strftime broken for "%p" format */
#ifdef __MRC__
#undef strftime
#include <time.h>
size_t
sys_strftime (char * s, size_t maxsize, const char * format,
	      const struct tm * timeptr)
{
  if (strcmp (format, "%p") == 0)
    {
      if (maxsize < 3)
        return 0;
      if (timeptr->tm_hour < 12)
        {
          strcpy (s, "AM");
          return 2;
        }
      else
        {
          strcpy (s, "PM");
          return 2;
        }
    }
  else
    return strftime (s, maxsize, format, timeptr);
}
#endif  /* __MRC__ */


/* no subprocesses, empty wait */

int
wait (int pid)
{
  return 0;
}


void
croak (char *badfunc)
{
  printf ("%s not yet implemented\r\n", badfunc);
  exit (1);
}


char *
index (const char * str, int chr)
{
  return strchr (str, chr);
}


char *
mktemp (char *template)
{
  int len, k;
  static seqnum = 0;
  
  len = strlen (template);
  k = len - 1;
  while (k >= 0 && template[k] == 'X')
    k--;
  
  k++;  /* make k index of first 'X' */
  
  if (k < len)
    {
      /* Zero filled, number of digits equal to the number of X's.  */
      sprintf (&template[k], "%0*d", len-k, seqnum++);
  
      return template;
    }
  else
    return 0;  
}


/* Emulate getpwuid, getpwnam and others.  */

#define PASSWD_FIELD_SIZE 256

static char my_passwd_name[PASSWD_FIELD_SIZE];
static char my_passwd_dir[MAXPATHLEN+1];

static struct passwd my_passwd = 
{
  my_passwd_name,
  my_passwd_dir,
};


/* Initialized by main () in macterm.c to pathname of emacs directory.  */

char emacs_passwd_dir[MAXPATHLEN+1];

char *
getwd (char *);

void
init_emacs_passwd_dir ()
{
  int found = false;

  if (getwd (emacs_passwd_dir) && getwd (my_passwd_dir))
    {
      /* Need pathname of first ancestor that begins with "emacs"
	 since Mac emacs application is somewhere in the emacs-*
	 tree.  */
      int len = strlen (emacs_passwd_dir);
      int j = len - 1;
        /* j points to the "/" following the directory name being
	   compared.  */
      int i = j - 1;
      while (i >= 0 && !found)
	{
	  while (i >= 0 && emacs_passwd_dir[i] != '/')
	    i--;
	  if (emacs_passwd_dir[i] == '/' && i+5 < len)
	    found = (strncmp (&(emacs_passwd_dir[i+1]), "emacs", 5) == 0);
	  if (found)
	    emacs_passwd_dir[j+1] = '\0';
	  else
	    {
	      j = i;
	      i = j - 1;
	    }
	}
    }
  
  if (!found)
    {
      /* Setting to "/" probably won't work but set it to something
	 anyway.  */
      strcpy (emacs_passwd_dir, "/");
      strcpy (my_passwd_dir, "/");
    }
}


static struct passwd emacs_passwd = 
{
  "emacs",
  emacs_passwd_dir,
};

static int my_passwd_inited = 0;


static void
init_my_passwd ()
{
  char **owner_name;

  /* Note: my_passwd_dir initialized in int_emacs_passwd_dir to
     directory where Emacs was started.  */

  owner_name = (char **) GetResource ('STR ',-16096);
  if (owner_name)
    {
      HLock (owner_name);
      BlockMove ((unsigned char *) *owner_name,
		 (unsigned char *) my_passwd_name,
		 *owner_name[0]+1);
      HUnlock (owner_name);
      p2cstr ((unsigned char *) my_passwd_name);
    }
  else
    my_passwd_name[0] = 0;
}


struct passwd *
getpwuid (uid_t uid)
{
  if (!my_passwd_inited)
    {  
      init_my_passwd ();
      my_passwd_inited = 1;
    }
  
  return &my_passwd;
}


struct passwd *
getpwnam (const char *name)
{
  if (strcmp (name, "emacs") == 0)
  	return &emacs_passwd;

  if (!my_passwd_inited)
    {  
      init_my_passwd ();
      my_passwd_inited = 1;
    }
  
  return &my_passwd;
}


/* The functions fork, kill, sigsetmask, sigblock, request_sigio,
   setpgrp, setpriority, and unrequest_sigio are defined to be empty
   as in msdos.c.  */


int
fork ()
{
  return -1;
}


int
kill (int x, int y)
{
  return -1;
}


void
sys_subshell ()
{
  error ("Can't spawn subshell");
}


int
sigsetmask (int x)
{
  return 0;
}


int
sigblock (int mask)
{
  return 0;
} 


void
request_sigio (void)
{
}


void
unrequest_sigio (void)
{
}


int
setpgrp ()
{
  return 0;
}


/* No pipes yet.  */

int
pipe (int _fildes[2])
{
  errno = EACCES;
  return -1;
}


/* Hard and symbolic links.  */

int
symlink (const char *name1, const char *name2)
{
  errno = ENOENT;
  return -1;
}


int
link (const char *name1, const char *name2)
{
  errno = ENOENT;
  return -1;
}

#endif  /* ! MAC_OSX */

/* Determine the path name of the file specified by VREFNUM, DIRID,
   and NAME and place that in the buffer PATH of length
   MAXPATHLEN.  */
int
path_from_vol_dir_name (char *path, int man_path_len, short vol_ref_num,
			long dir_id, ConstStr255Param name)
{
  Str255 dir_name;
  CInfoPBRec cipb;
  OSErr err;

  if (strlen (name) > man_path_len)
    return 0;

  memcpy (dir_name, name, name[0]+1);
  memcpy (path, name, name[0]+1);
  p2cstr (path);

  cipb.dirInfo.ioDrParID = dir_id;
  cipb.dirInfo.ioNamePtr = dir_name;

  do
    {
      cipb.dirInfo.ioVRefNum = vol_ref_num;
      cipb.dirInfo.ioFDirIndex = -1;
      cipb.dirInfo.ioDrDirID = cipb.dirInfo.ioDrParID;
        /* go up to parent each time */

      err = PBGetCatInfo (&cipb, false);
      if (err != noErr)
        return 0;
      
      p2cstr (dir_name);
      if (strlen (dir_name) + strlen (path) + 1 >= man_path_len)
        return 0;

      strcat (dir_name, ":");
      strcat (dir_name, path);
        /* attach to front since we're going up directory tree */
      strcpy (path, dir_name);
    }
  while (cipb.dirInfo.ioDrDirID != fsRtDirID);
    /* stop when we see the volume's root directory */
  
  return 1;  /* success */
}

#ifndef MAC_OSX

int
readlink (const char *path, char *buf, int bufsiz)
{
  char mac_sym_link_name[MAXPATHLEN+1];
  OSErr err;
  FSSpec fsspec;
  Boolean target_is_folder, was_aliased;
  Str255 directory_name, mac_pathname;
  CInfoPBRec cipb;

  if (posix_to_mac_pathname (path, mac_sym_link_name, MAXPATHLEN+1) == 0)
    return -1;

  c2pstr (mac_sym_link_name);
  err = FSMakeFSSpec (0, 0, mac_sym_link_name, &fsspec);
  if (err != noErr)
    {
      errno = ENOENT;
      return -1;
    }

  err = ResolveAliasFile (&fsspec, true, &target_is_folder, &was_aliased);
  if (err != noErr || !was_aliased)
    {
      errno = ENOENT;
      return -1;
    }

  if (path_from_vol_dir_name (mac_pathname, 255, fsspec.vRefNum, fsspec.parID,
			      fsspec.name) == 0)
    {
      errno = ENOENT;
      return -1;
    }

  if (mac_to_posix_pathname (mac_pathname, buf, bufsiz) == 0)
    {
      errno = ENOENT;
      return -1;
    }

  return strlen (buf);
}


/* Convert a path to one with aliases fully expanded.  */

static int
find_true_pathname (const char *path, char *buf, int bufsiz)
{
  char *q, temp[MAXPATHLEN+1];
  const char *p;
  int len;

  if (bufsiz <= 0 || path == 0 || path[0] == '\0')
    return -1;

  buf[0] = '\0';
  
  p = path;
  if (*p == '/')
    q = strchr (p + 1, '/');
  else
    q = strchr (p, '/');
  len = 0;  /* loop may not be entered, e.g., for "/" */

  while (q)
    {
      strcpy (temp, buf);
      strncat (temp, p, q - p);
      len = readlink (temp, buf, bufsiz);
      if (len <= -1)
        {
          if (strlen (temp) + 1 > bufsiz)
            return -1;
          strcpy (buf, temp);
        }
      strcat (buf, "/");
      len++;
      p = q + 1;
      q = strchr(p, '/');
    }
  
  if (len + strlen (p) + 1 >= bufsiz)
    return -1;
  
  strcat (buf, p);
  return len + strlen (p);
}


mode_t
umask (mode_t numask)
{
  static mode_t mask = 022;
  mode_t oldmask = mask;
  mask = numask;
  return oldmask;
}


int
chmod (const char *path, mode_t mode)
{
  /* say it always succeed for now */
  return 0;
}


int
dup (int oldd)
{
#ifdef __MRC__
  return fcntl (oldd, F_DUPFD, 0);
#elif __MWERKS__
  /* current implementation of fcntl in fcntl.mac.c simply returns old
     descriptor */
  return fcntl (oldd, F_DUPFD);
#else
You lose!!!
#endif
}


/* This is from the original sysdep.c.  Emulate BSD dup2.  First close
   newd if it already exists.  Then, attempt to dup oldd.  If not
   successful, call dup2 recursively until we are, then close the
   unsuccessful ones.  */

int
dup2 (int oldd, int newd)
{
  int fd, ret;
  
  close (newd);

  fd = dup (oldd);
  if (fd == -1)
    return -1;
  if (fd == newd)
    return newd;
  ret = dup2 (oldd, newd);
  close (fd);
  return ret;
}


/* let it fail for now */

char *
sbrk (int incr)
{
  return (char *) -1;
}


int
fsync (int fd)
{
  return 0;
}


int
ioctl (int d, int request, void *argp)
{
  return -1;
}


#ifdef __MRC__
int
isatty (int fildes)
{
  if (fildes >=0 && fildes <= 2)
    return 1;
  else
    return 0;
}


int
getgid ()
{
  return 100;
}


int
getegid ()
{
  return 100;
}


int
getuid ()
{
  return 200;
}


int
geteuid ()
{
  return 200;
}
#endif /* __MRC__ */


#ifdef __MWERKS__
#if __MSL__ < 0x6000
#undef getpid
int
getpid ()
{
  return 9999;
}
#endif
#endif /* __MWERKS__ */

#endif /* ! MAC_OSX */


/* Return the path to the directory in which Emacs can create
   temporary files.  The MacOS "temporary items" directory cannot be
   used because it removes the file written by a process when it
   exits.  In that sense it's more like "/dev/null" than "/tmp" (but
   again not exactly).  And of course Emacs needs to read back the
   files written by its subprocesses.  So here we write the files to a
   directory "Emacs" in the Preferences Folder.  This directory is
   created if it does not exist.  */

char *
get_temp_dir_name ()
{
  static char *temp_dir_name = NULL;
  short vol_ref_num;
  long dir_id;
  OSErr err;
  Str255 dir_name, full_path;
  CInfoPBRec cpb;
  char unix_dir_name[MAXPATHLEN+1];
  DIR *dir;
  
  /* Cache directory name with pointer temp_dir_name.
     Look for it only the first time.  */
  if (!temp_dir_name)
    {
      err = FindFolder (kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
			&vol_ref_num, &dir_id);
      if (err != noErr)
	return NULL;
      
      if (!path_from_vol_dir_name (full_path, 255, vol_ref_num, dir_id, "\p"))
        return NULL;

      if (strlen (full_path) + 6 <= MAXPATHLEN)
	strcat (full_path, "Emacs:");
      else 
	return NULL;

      if (!mac_to_posix_pathname (full_path, unix_dir_name, MAXPATHLEN+1))
	return NULL;
    
      dir = opendir (unix_dir_name);  /* check whether temp directory exists */
      if (dir)
	closedir (dir);
      else if (mkdir (unix_dir_name, 0700) != 0)  /* create it if not */
	return NULL;

      temp_dir_name = (char *) malloc (strlen (unix_dir_name) + 1);
      strcpy (temp_dir_name, unix_dir_name);
    }

  return temp_dir_name;
}

#ifndef MAC_OSX

/* Allocate and construct an array of pointers to strings from a list
   of strings stored in a 'STR#' resource.  The returned pointer array
   is stored in the style of argv and environ: if the 'STR#' resource
   contains numString strings, an pointer array with numString+1
   elements is returned in which the last entry contains a null
   pointer.  The pointer to the pointer array is passed by pointer in
   parameter t.  The resource ID of the 'STR#' resource is passed in
   parameter StringListID.
   */

void
get_string_list (char ***t, short string_list_id)
{
  Handle h;
  Ptr p;
  int i, num_strings;

  h = GetResource ('STR#', string_list_id);
  if (h)
    {
      HLock (h);
      p = *h;
      num_strings = * (short *) p;
      p += sizeof(short);
      *t = (char **) malloc (sizeof (char *) * (num_strings + 1));
      for (i = 0; i < num_strings; i++)
        {
          short length = *p++;
          (*t)[i] = (char *) malloc (length + 1);
          strncpy ((*t)[i], p, length);
          (*t)[i][length] = '\0';
          p += length;
        }
      (*t)[num_strings] = 0;
      HUnlock (h);
    }
  else
    {
      /* Return no string in case GetResource fails.  Bug fixed by
         Ikegami Tsutomu.  Caused MPW build to crash without sym -on
         option (no sym -on implies -opt local). */
      *t = (char **) malloc (sizeof (char *));
      (*t)[0] = 0;
    }
}


static char *
get_path_to_system_folder ()
{
  short vol_ref_num;
  long dir_id;
  OSErr err;
  Str255 dir_name, full_path;
  CInfoPBRec cpb;
  static char system_folder_unix_name[MAXPATHLEN+1];
  DIR *dir;
  
  err = FindFolder (kOnSystemDisk, kSystemFolderType, kDontCreateFolder,
		    &vol_ref_num, &dir_id);
  if (err != noErr)
    return NULL;
      
  if (!path_from_vol_dir_name (full_path, 255, vol_ref_num, dir_id, "\p"))
    return NULL;

  if (!mac_to_posix_pathname (full_path, system_folder_unix_name,
			      MAXPATHLEN+1))
    return NULL;
    
  return system_folder_unix_name;
}


char **environ;

#define ENVIRON_STRING_LIST_ID 128

/* Get environment variable definitions from STR# resource.  */

void
init_environ ()
{
  int i;
  
  get_string_list (&environ, ENVIRON_STRING_LIST_ID);

  i = 0;
  while (environ[i])
    i++;

  /* Make HOME directory the one Emacs starts up in if not specified
     by resource.  */
  if (getenv ("HOME") == NULL)
    {
      environ = (char **) realloc (environ, sizeof (char *) * (i + 2));
      if (environ)
        {
          environ[i] = (char *) malloc (strlen (my_passwd_dir) + 6);
          if (environ[i])
            {
              strcpy (environ[i], "HOME=");
              strcat (environ[i], my_passwd_dir);
            }
          environ[i+1] = 0;
          i++;
        }
    }

  /* Make HOME directory the one Emacs starts up in if not specified
     by resource.  */
  if (getenv ("MAIL") == NULL)
    {
      environ = (char **) realloc (environ, sizeof (char *) * (i + 2));
      if (environ)
        {
          char * path_to_system_folder = get_path_to_system_folder ();
          environ[i] = (char *) malloc (strlen (path_to_system_folder) + 22);
          if (environ[i])
            {
              strcpy (environ[i], "MAIL=");
              strcat (environ[i], path_to_system_folder);
              strcat (environ[i], "Eudora Folder/In");
            }
          environ[i+1] = 0;
        }
    }
}


/* Return the value of the environment variable NAME.  */

char *
getenv (const char *name)
{
  int length = strlen(name);
  char **e;

  for (e = environ; *e != 0; e++)
    if (strncmp(*e, name, length) == 0 && (*e)[length] == '=')
      return &(*e)[length + 1];

  if (strcmp (name, "TMPDIR") == 0)
    return get_temp_dir_name ();

  return 0;
}


#ifdef __MRC__
/* see Interfaces&Libraries:Interfaces:CIncludes:signal.h */
char *sys_siglist[] =
{
  "Zero is not a signal!!!",
  "Abort", /* 1 */
  "Interactive user interrupt", /* 2 */ "?",
  "Floating point exception", /* 4 */ "?", "?", "?",
  "Illegal instruction", /* 8 */ "?", "?", "?", "?", "?", "?", "?",
  "Segment violation", /* 16 */ "?", "?", "?", "?", "?", "?", "?",
    "?", "?", "?", "?", "?", "?", "?", "?",
  "Terminal"  /* 32 */
};
#elif __MWERKS__
char *sys_siglist[] =
{
  "Zero is not a signal!!!",
  "Abort",
  "Floating point exception",
  "Illegal instruction",
  "Interactive user interrupt",
  "Segment violation",
  "Terminal"
};
#else /* not __MRC__ and not __MWERKS__ */
You lose!!!
#endif /* not __MRC__ and not __MWERKS__ */


#include <utsname.h>

int
uname (struct utsname *name)
{
  char **system_name;
  system_name = GetString (-16413);  /* IM - Resource Manager Reference */
  if (system_name)
    {
      BlockMove (*system_name, name->nodename, (*system_name)[0]+1);
      p2cstr (name->nodename);
      return 0;
    }
  else
    return -1;
}


#include <Processes.h>
#include <EPPC.h>

/* Event class of HLE sent to subprocess.  */
const OSType kEmacsSubprocessSend = 'ESND';

/* Event class of HLE sent back from subprocess.  */
const OSType kEmacsSubprocessReply = 'ERPY';


char *
mystrchr (char *s, char c)
{
  while (*s && *s != c)
    {
      if (*s == '\\')
	s++;
      s++;
    }

  if (*s)
    {
      *s = '\0';
      return s;
    }
  else
    return NULL;
}


char *
mystrtok (char *s)
{	
  while (*s)
    s++;

  return s + 1;
}


void
mystrcpy (char *to, char *from)
{
  while (*from)
    {
      if (*from == '\\')
	from++;
      *to++ = *from++;
    }
  *to = '\0';
}


/* Start a Mac subprocess.  Arguments for it is passed in argv (null
   terminated).  The process should run with the default directory
   "workdir", read input from "infn", and write output and error to
   "outfn" and "errfn", resp.  The Process Manager call
   LaunchApplication is used to start the subprocess.  We use high
   level events as the mechanism to pass arguments to the subprocess
   and to make Emacs wait for the subprocess to terminate and pass
   back a result code.  The bulk of the code here packs the arguments
   into one message to be passed together with the high level event.
   Emacs also sometimes starts a subprocess using a shell to perform
   wildcard filename expansion.  Since we don't really have a shell on
   the Mac, this case is detected and the starting of the shell is
   by-passed.  We really need to add code here to do filename
   expansion to support such functionality. */

int
run_mac_command (argv, workdir, infn, outfn, errfn)
     unsigned char **argv;
     const char *workdir;
     const char *infn, *outfn, *errfn;
{
#ifdef TARGET_API_MAC_CARBON
  return -1;
#else /* not TARGET_API_MAC_CARBON */
  char macappname[MAXPATHLEN+1], macworkdir[MAXPATHLEN+1];
  char macinfn[MAXPATHLEN+1], macoutfn[MAXPATHLEN+1], macerrfn[MAXPATHLEN+1];
  int paramlen, argc, newargc, j, retries;
  char **newargv, *param, *p;
  OSErr iErr;
  FSSpec spec;
  LaunchParamBlockRec lpbr;
  EventRecord send_event, reply_event;
  RgnHandle cursor_region_handle;
  TargetID targ;
  unsigned long ref_con, len;
 	
  if (posix_to_mac_pathname (workdir, macworkdir, MAXPATHLEN+1) == 0)
    return -1;
  if (posix_to_mac_pathname (infn, macinfn, MAXPATHLEN+1) == 0)
    return -1;
  if (posix_to_mac_pathname (outfn, macoutfn, MAXPATHLEN+1) == 0)
    return -1;
  if (posix_to_mac_pathname (errfn, macerrfn, MAXPATHLEN+1) == 0)
    return -1;
  
  paramlen = strlen (macworkdir) + strlen (macinfn) + strlen (macoutfn)
             + strlen (macerrfn) + 4;  /* count nulls at end of strings */

  argc = 0;
  while (argv[argc])
    argc++;

  if (argc == 0)
    return -1;

  /* If a subprocess is invoked with a shell, we receive 3 arguments
     of the form: "<path to emacs bins>/sh" "-c" "<path to emacs
     bins>/<command> <command args>" */
  j = strlen (argv[0]);
  if (j >= 3 && strcmp (argv[0]+j-3, "/sh") == 0
      && argc == 3 && strcmp (argv[1], "-c") == 0)
    {
      char *command, *t, tempmacpathname[MAXPATHLEN+1];
    
      /* The arguments for the command in argv[2] are separated by
	 spaces.  Count them and put the count in newargc.  */
      command = (char *) alloca (strlen (argv[2])+2);
      strcpy (command, argv[2]);
      if (command[strlen (command) - 1] != ' ')
	strcat (command, " ");
    
      t = command;
      newargc = 0;
      t = mystrchr (t, ' ');
      while (t)
	{
	  newargc++;
	  t = mystrchr (t+1, ' ');
	}
    
      newargv = (char **) alloca (sizeof (char *) * newargc);
    
      t = command;
      for (j = 0; j < newargc; j++)
	{
	  newargv[j] = (char *) alloca (strlen (t) + 1);
	  mystrcpy (newargv[j], t);

	  t = mystrtok (t);
	  paramlen += strlen (newargv[j]) + 1;
	}
    
      if (strncmp (newargv[0], "~emacs/", 7) == 0)
	{
	  if (posix_to_mac_pathname (newargv[0], tempmacpathname, MAXPATHLEN+1)
	      == 0)
	    return -1;
	}
      else
	{  /* sometimes Emacs call "sh" without a path for the command */
#if 0
	  char *t = (char *) alloca (strlen (newargv[0]) + 7 + 1);
	  strcpy (t, "~emacs/");
	  strcat (t, newargv[0]);
#endif /* 0 */
	  Lisp_Object path;
	  openp (Vexec_path, build_string (newargv[0]), EXEC_SUFFIXES, &path,
		 1);

	  if (NILP (path))
	    return -1;
	  if (posix_to_mac_pathname (XSTRING (path)->data, tempmacpathname,
				    MAXPATHLEN+1) == 0)
	    return -1;
	}
      strcpy (macappname, tempmacpathname);
    }
  else
    {      
      if (posix_to_mac_pathname (argv[0], macappname, MAXPATHLEN+1) == 0)
	return -1;

      newargv = (char **) alloca (sizeof (char *) * argc);
      newargc = argc;  
      for (j = 1; j < argc; j++)
	{
	  if (strncmp (argv[j], "~emacs/", 7) == 0)
	    {
	      char *t = strchr (argv[j], ' ');
	      if (t)
		{
		  char tempcmdname[MAXPATHLEN+1], tempmaccmdname[MAXPATHLEN+1];
		  strncpy (tempcmdname, argv[j], t-argv[j]);
		  tempcmdname[t-argv[j]] = '\0';
		  if (posix_to_mac_pathname (tempcmdname, tempmaccmdname,
					    MAXPATHLEN+1) == 0)
		    return -1;
		  newargv[j] = (char *) alloca (strlen (tempmaccmdname)
						+ strlen (t) + 1);
		  strcpy (newargv[j], tempmaccmdname);
		  strcat (newargv[j], t);
		}
	      else
		{
		  char tempmaccmdname[MAXPATHLEN+1];
		  if (posix_to_mac_pathname (argv[j], tempmaccmdname,
					    MAXPATHLEN+1) == 0)
		    return -1;
		  newargv[j] = (char *) alloca (strlen (tempmaccmdname)+1);
		  strcpy (newargv[j], tempmaccmdname);
		}
	    }
	  else
	    newargv[j] = argv[j];  
	  paramlen += strlen (newargv[j]) + 1;
	}
    }

  /* After expanding all the arguments, we now know the length of the
     parameter block to be sent to the subprocess as a message
     attached to the HLE.  */
  param = (char *) malloc (paramlen + 1);
  if (!param)
    return -1;

  p = param;
  *p++ = newargc;
    /* first byte of message contains number of arguments for command */
  strcpy (p, macworkdir);
  p += strlen (macworkdir);
  *p++ = '\0';
    /* null terminate strings sent so it's possible to use strcpy over there */
  strcpy (p, macinfn);
  p += strlen (macinfn);
  *p++ = '\0';  
  strcpy (p, macoutfn);
  p += strlen (macoutfn);
  *p++ = '\0';  
  strcpy (p, macerrfn);
  p += strlen (macerrfn);
  *p++ = '\0';  
  for (j = 1; j < newargc; j++)
    {
      strcpy (p, newargv[j]);
      p += strlen (newargv[j]);
      *p++ = '\0';  
    }
  
  c2pstr (macappname);
  
  iErr = FSMakeFSSpec (0, 0, macappname, &spec);
  
  if (iErr != noErr)
    {
      free (param);
      return -1;
    }

  lpbr.launchBlockID = extendedBlock;
  lpbr.launchEPBLength = extendedBlockLen;
  lpbr.launchControlFlags = launchContinue + launchNoFileFlags;
  lpbr.launchAppSpec = &spec;
  lpbr.launchAppParameters = NULL;

  iErr = LaunchApplication (&lpbr);  /* call the subprocess */
  if (iErr != noErr)
    {
      free (param);
      return -1;
    }

  send_event.what = kHighLevelEvent;
  send_event.message = kEmacsSubprocessSend;
    /* Event ID stored in "where" unused */

  retries = 3;
  /* OS may think current subprocess has terminated if previous one
     terminated recently.  */
  do
    {
      iErr = PostHighLevelEvent (&send_event, &lpbr.launchProcessSN, 0, param,
				 paramlen + 1, receiverIDisPSN);
    }
  while (iErr == sessClosedErr && retries-- > 0);

  if (iErr != noErr)
    {
      free (param);
      return -1;
    }

  cursor_region_handle = NewRgn ();
	
  /* Wait for the subprocess to finish, when it will send us a ERPY
     high level event.  */
  while (1)
    if (WaitNextEvent (highLevelEventMask, &reply_event, 180,
		       cursor_region_handle)
	&& reply_event.message == kEmacsSubprocessReply)
      break;
  
  /* The return code is sent through the refCon */
  iErr = AcceptHighLevelEvent (&targ, &ref_con, NULL, &len);
  if (iErr != noErr)
    {
      DisposeHandle ((Handle) cursor_region_handle);
      free (param);
      return -1;
    }
  
  DisposeHandle ((Handle) cursor_region_handle);
  free (param);

  return ref_con;
#endif /* not TARGET_API_MAC_CARBON */
}


DIR *
opendir (const char *dirname)
{
  char true_pathname[MAXPATHLEN+1], fully_resolved_name[MAXPATHLEN+1];  
  char mac_pathname[MAXPATHLEN+1], vol_name[MAXPATHLEN+1];
  DIR *dirp;
  CInfoPBRec cipb;
  HVolumeParam vpb;
  int len, vol_name_len;
  	
  if (find_true_pathname (dirname, true_pathname, MAXPATHLEN+1) == -1)
    return 0;
  
  len = readlink (true_pathname, fully_resolved_name, MAXPATHLEN);
  if (len > -1)
    fully_resolved_name[len] = '\0';
  else
    strcpy (fully_resolved_name, true_pathname);

  dirp = (DIR *) malloc (sizeof(DIR));
  if (!dirp)
    return 0;

  /* Handle special case when dirname is "/": sets up for readir to
     get all mount volumes.  */
  if (strcmp (fully_resolved_name, "/") == 0)
    {
      dirp->getting_volumes = 1;  /* special all mounted volumes DIR struct */
      dirp->current_index = 1;  /* index for first volume */
      return dirp;
    }

  /* Handle typical cases: not accessing all mounted volumes.  */
  if (!posix_to_mac_pathname (fully_resolved_name, mac_pathname, MAXPATHLEN+1))
    return 0;

  /* Emacs calls opendir without the trailing '/', Mac needs trailing ':' */
  len = strlen (mac_pathname);
  if (mac_pathname[len - 1] != ':' && len < MAXPATHLEN)
    strcat (mac_pathname, ":");
  
  /* Extract volume name */
  vol_name_len = strchr (mac_pathname, ':') - mac_pathname;
  strncpy (vol_name, mac_pathname, vol_name_len);
  vol_name[vol_name_len] = '\0';
  strcat (vol_name, ":");

  c2pstr (mac_pathname);
  cipb.hFileInfo.ioNamePtr = mac_pathname;
    /* using full pathname so vRefNum and DirID ignored */
  cipb.hFileInfo.ioVRefNum = 0;
  cipb.hFileInfo.ioDirID = 0;
  cipb.hFileInfo.ioFDirIndex = 0;
    /* set to 0 to get information about specific dir or file */
  
  errno = PBGetCatInfo (&cipb, false);
  if (errno != noErr)
    {
      errno = ENOENT;
      return 0;
    }

  if (!(cipb.hFileInfo.ioFlAttrib & 0x10))  /* bit 4 = 1 for directories */
    return 0;  /* not a directory */

  dirp->dir_id = cipb.dirInfo.ioDrDirID;  /* used later in readdir */
  dirp->getting_volumes = 0;
  dirp->current_index = 1;  /* index for first file/directory */

  c2pstr (vol_name);
  vpb.ioNamePtr = vol_name;
    /* using full pathname so vRefNum and DirID ignored */
  vpb.ioVRefNum = 0;
  vpb.ioVolIndex = -1;
  errno = PBHGetVInfo ((union HParamBlockRec *) &vpb, false);
  if (errno != noErr)
    {
      errno = ENOENT;
      return 0;
    }

  dirp->vol_ref_num = vpb.ioVRefNum;
  
  return dirp;
}

int
closedir (DIR *dp)
{
  free (dp);

  return 0;
}


struct dirent *
readdir (DIR *dp)
{
  HParamBlockRec hpblock;
  CInfoPBRec cipb;
  static struct dirent s_dirent;
  static Str255 s_name;
  int done;
  char *p;

  /* Handle the root directory containing the mounted volumes.  Call
     PBHGetVInfo specifying an index to obtain the info for a volume.
     PBHGetVInfo returns an error when it receives an index beyond the
     last volume, at which time we should return a nil dirent struct
     pointer.  */
  if (dp->getting_volumes)
    {
      hpblock.volumeParam.ioNamePtr = s_name;
      hpblock.volumeParam.ioVRefNum = 0;
      hpblock.volumeParam.ioVolIndex = dp->current_index;
                
      errno = PBHGetVInfo (&hpblock, false);
      if (errno != noErr)
	{
	  errno = ENOENT;
	  return 0;
	}
                        
      p2cstr (s_name);
      strcat (s_name, "/");  /* need "/" for stat to work correctly */

      dp->current_index++;

      s_dirent.d_ino = hpblock.volumeParam.ioVRefNum;
      s_dirent.d_name = s_name;
  
      return &s_dirent;
    }
  else
    {
      cipb.hFileInfo.ioVRefNum = dp->vol_ref_num;
      cipb.hFileInfo.ioNamePtr = s_name;
        /* location to receive filename returned */

      /* return only visible files */
      done = false;
      while (!done)
	{
	  cipb.hFileInfo.ioDirID = dp->dir_id;
	    /* directory ID found by opendir */
	  cipb.hFileInfo.ioFDirIndex = dp->current_index;
	  
	  errno = PBGetCatInfo (&cipb, false);
	  if (errno != noErr)
	    {
	      errno = ENOENT;
	      return 0;
	    }
	  
	  /* insist on an visibile entry */
	  if (cipb.hFileInfo.ioFlAttrib & 0x10)  /* directory? */
	    done = !(cipb.dirInfo.ioDrUsrWds.frFlags & fInvisible);
	  else
	    done = !(cipb.hFileInfo.ioFlFndrInfo.fdFlags & fInvisible);
	  
	  dp->current_index++;
	}

      p2cstr (s_name);
      
      p = s_name;
      while (*p)
        {
          if (*p == '/')
            *p = ':';
          p++;
        }

      s_dirent.d_ino = cipb.dirInfo.ioDrDirID;
        /* value unimportant: non-zero for valid file */
      s_dirent.d_name = s_name;
  
      return &s_dirent;
    }
}


char *
getwd (char *path)
{
  char mac_pathname[MAXPATHLEN+1];
  Str255 directory_name;
  OSErr errno;
  CInfoPBRec cipb;

  if (path_from_vol_dir_name (mac_pathname, 255, 0, 0, "\p") == 0)
    return NULL;

  if (mac_to_posix_pathname (mac_pathname, path, MAXPATHLEN+1) == 0)
    return 0;
  else
    return path;
}

#endif  /* ! MAC_OSX */


void
initialize_applescript ()
{
  AEDesc null_desc;
  OSAError osaerror;
  
  /* if open fails, as_scripting_component is set to NULL.  Its
     subsequent use in OSA calls will fail with badComponentInstance
     error.  */
  as_scripting_component = OpenDefaultComponent (kOSAComponentType,
						 kAppleScriptSubtype);

  null_desc.descriptorType = typeNull;
  null_desc.dataHandle = 0;
  osaerror = OSAMakeContext (as_scripting_component, &null_desc,
			     kOSANullScript, &as_script_context);
  if (osaerror)
    as_script_context = kOSANullScript;
      /* use default context if create fails */
}


void terminate_applescript()
{
  OSADispose (as_scripting_component, as_script_context);
  CloseComponent (as_scripting_component);
}


/* Compile and execute the AppleScript SCRIPT and return the error
   status as function value.  A zero is returned if compilation and
   execution is successful, in which case RESULT returns a pointer to
   a string containing the resulting script value.  Otherwise, the Mac
   error code is returned and RESULT returns a pointer to an error
   string.  In both cases the caller should deallocate the storage
   used by the string pointed to by RESULT if it is non-NULL.  For
   documentation on the MacOS scripting architecture, see Inside
   Macintosh - Interapplication Communications: Scripting Components.  */

static long
do_applescript (char *script, char **result)
{
  AEDesc script_desc, result_desc, error_desc;
  OSErr error;
  OSAError osaerror;
  long length;

  *result = 0;

  error = AECreateDesc (typeChar, script, strlen(script), &script_desc);
  if (error)
    return error;

  osaerror = OSADoScript (as_scripting_component, &script_desc, kOSANullScript,
			  typeChar, kOSAModeNull, &result_desc);

  if (osaerror == errOSAScriptError)
    {
      /* error executing AppleScript: retrieve error message */
      if (!OSAScriptError (as_scripting_component, kOSAErrorMessage, typeChar,
			   &error_desc))
        {
#if TARGET_API_MAC_CARBON
          length = AEGetDescDataSize (&error_desc);
          *result = (char *) xmalloc (length + 1);
          if (*result)
            {
              AEGetDescData (&error_desc, *result, length);
              *(*result + length) = '\0';
            }
#else /* not TARGET_API_MAC_CARBON */
          HLock (error_desc.dataHandle);
          length = GetHandleSize(error_desc.dataHandle);
          *result = (char *) xmalloc (length + 1);
          if (*result)
            {
              memcpy (*result, *(error_desc.dataHandle), length);
              *(*result + length) = '\0';
            }
          HUnlock (error_desc.dataHandle);
#endif /* not TARGET_API_MAC_CARBON */
          AEDisposeDesc (&error_desc);
        }
    }
  else if (osaerror == noErr)  /* success: retrieve resulting script value */
    {
#if TARGET_API_MAC_CARBON
      length = AEGetDescDataSize (&result_desc);
      *result = (char *) xmalloc (length + 1);
      if (*result)
        {
          AEGetDescData (&result_desc, *result, length);
          *(*result + length) = '\0';
        }
#else /* not TARGET_API_MAC_CARBON */
      HLock (result_desc.dataHandle);
      length = GetHandleSize(result_desc.dataHandle);
      *result = (char *) xmalloc (length + 1);
      if (*result)
        {
          memcpy (*result, *(result_desc.dataHandle), length);
          *(*result + length) = '\0';
        }
      HUnlock (result_desc.dataHandle);
#endif /* not TARGET_API_MAC_CARBON */
    }

  AEDisposeDesc (&script_desc);
  AEDisposeDesc (&result_desc);    

  return osaerror;
}


DEFUN ("do-applescript", Fdo_applescript, Sdo_applescript, 1, 1, 0,
       doc: /* Compile and execute AppleScript SCRIPT and retrieve and return the result.
If compilation and execution are successful, the resulting script
value is returned as a string.  Otherwise the function aborts and
displays the error message returned by the AppleScript scripting
component.  */)
  (script)
    Lisp_Object script;
{
  char *result, *temp;
  Lisp_Object lisp_result;
  long status;

  CHECK_STRING (script);
  
  status = do_applescript (XSTRING (script)->data, &result);
  if (status)
    {
      if (!result)
        error ("AppleScript error %ld", status);
      else
        {
          /* Unfortunately only OSADoScript in do_applescript knows how
             how large the resulting script value or error message is
             going to be and therefore as caller memory must be
             deallocated here.  It is necessary to free the error
             message before calling error to avoid a memory leak.  */
          temp = (char *) alloca (strlen (result) + 1);
          strcpy (temp, result);
          xfree (result);
          error (temp);
        }
    }
  else
    {
      lisp_result = build_string (result);
      xfree (result);
      return lisp_result;
    }
}


DEFUN ("mac-file-name-to-posix", Fmac_file_name_to_posix,
       Smac_file_name_to_posix, 1, 1, 0,
       doc: /* Convert Macintosh filename to Posix form.  */)
     (mac_filename)
     Lisp_Object mac_filename;
{
  char posix_filename[MAXPATHLEN+1];

  CHECK_STRING (mac_filename);
  
  if (mac_to_posix_pathname (XSTRING (mac_filename)->data, posix_filename,
			   MAXPATHLEN))
    return build_string (posix_filename);
  else
    return Qnil;
}


DEFUN ("posix-file-name-to-mac", Fposix_file_name_to_mac,
       Sposix_file_name_to_mac, 1, 1, 0,
       doc: /* Convert Posix filename to Mac form.  */)
     (posix_filename)
     Lisp_Object posix_filename;
{
  char mac_filename[MAXPATHLEN+1];

  CHECK_STRING (posix_filename);
  
  if (posix_to_mac_pathname (XSTRING (posix_filename)->data, mac_filename,
			   MAXPATHLEN))
    return build_string (mac_filename);
  else
    return Qnil;
}


/* set interprogram-paste-function to mac-paste-function in mac-win.el
   to enable Emacs to obtain the contents of the Mac clipboard. */
DEFUN ("mac-paste-function", Fmac_paste_function, Smac_paste_function, 0, 0, 0,
       doc: /* Return the contents of the Mac clipboard as a string.  */)
     ()
{
#if TARGET_API_MAC_CARBON
  ScrapRef scrap;
  ScrapFlavorFlags sff;
  Size s;
  int i;
  char *data;

  if (GetCurrentScrap (&scrap) != noErr)
    return Qnil;

  if (GetScrapFlavorFlags (scrap, kScrapFlavorTypeText, &sff) != noErr)
    return Qnil;

  if (GetScrapFlavorSize (scrap, kScrapFlavorTypeText, &s) != noErr)
    return Qnil;

  if ((data = (char*) alloca (s)) == NULL)
    return Qnil;

  if (GetScrapFlavorData (scrap, kScrapFlavorTypeText, &s, data) != noErr
      || s == 0)
    return Qnil;
  
  /* Emacs expects clipboard contents have Unix-style eol's */
  for (i = 0; i < s; i++)
    if (data[i] == '\r')
      data[i] = '\n';

  return make_string (data, s);
#else /* not TARGET_API_MAC_CARBON */
  Lisp_Object value;
  Handle my_handle;
  long scrap_offset, rc, i;

  my_handle = NewHandle (0);  /* allocate 0-length data area */

  rc = GetScrap (my_handle, 'TEXT', &scrap_offset);
  if (rc < 0)
    return Qnil;

  HLock (my_handle);

  /* Emacs expects clipboard contents have Unix-style eol's */
  for (i = 0; i < rc; i++)
    if ((*my_handle)[i] == '\r')
      (*my_handle)[i] = '\n';

  value = make_string (*my_handle, rc);

  HUnlock (my_handle);
  
  DisposeHandle (my_handle);

  return value;
#endif /* not TARGET_API_MAC_CARBON */
}


/* set interprogram-cut-function to mac-cut-function in mac-win.el
   to enable Emacs to write the top of the kill-ring to the Mac clipboard. */
DEFUN ("mac-cut-function", Fmac_cut_function, Smac_cut_function, 1, 2, 0,
       doc: /* Put the value of the string parameter to the Mac clipboard.  */)
  (value, push)
    Lisp_Object value, push;
{
  char *buf;
  int len, i;

  /* fixme: ignore the push flag for now */

  CHECK_STRING (value);
  
  len = XSTRING (value)->size;
  buf = (char *) alloca (len+1);
  bcopy (XSTRING (value)->data, buf, len);
  buf[len] = '\0';
  
  /* convert to Mac-style eol's before sending to clipboard */
  for (i = 0; i < len; i++)
    if (buf[i] == '\n')
      buf[i] = '\r';

#if TARGET_API_MAC_CARBON
  {
    ScrapRef scrap;
    ClearCurrentScrap ();
    if (GetCurrentScrap (&scrap) != noErr)
      error ("cannot get current scrap");

    if (PutScrapFlavor (scrap, kScrapFlavorTypeText, kScrapFlavorMaskNone, len,
			buf) != noErr)
      error ("cannot put to scrap");
  }
#else /* not TARGET_API_MAC_CARBON */
  ZeroScrap ();
  PutScrap (len, 'TEXT', buf);
#endif /* not TARGET_API_MAC_CARBON */
  
  return Qnil;
}


DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 1, 0,
       doc: /* Whether there is an owner for the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.  */)
  (selection)
     Lisp_Object selection;
{
  CHECK_SYMBOL (selection);

  /* Return nil for PRIMARY and SECONDARY selections; for CLIPBOARD, check
     if the clipboard currently has valid text format contents. */

  if (EQ (selection, QCLIPBOARD))
    {
      Lisp_Object val = Qnil;

#if TARGET_API_MAC_CARBON
      ScrapRef scrap;
      ScrapFlavorFlags sff;

      if (GetCurrentScrap (&scrap) == noErr)
        if (GetScrapFlavorFlags (scrap, kScrapFlavorTypeText, &sff) == noErr)
          val = Qt;
#else /* not TARGET_API_MAC_CARBON */
      Handle my_handle;
      long rc, scrap_offset;

      my_handle = NewHandle (0);

      rc = GetScrap (my_handle, 'TEXT', &scrap_offset);
      if (rc >= 0)
        val = Qt;

      DisposeHandle (my_handle);
#endif /* not TARGET_API_MAC_CARBON */

      return val;
    }
  return Qnil;
}


void
syms_of_mac ()
{
  QCLIPBOARD = intern ("CLIPBOARD");
  staticpro (&QCLIPBOARD);
  
  defsubr (&Smac_paste_function);
  defsubr (&Smac_cut_function);
#if 0
  defsubr (&Sx_selection_exists_p);
#endif /* 0 */

  defsubr (&Sdo_applescript);
  defsubr (&Smac_file_name_to_posix);
  defsubr (&Sposix_file_name_to_mac);
}
