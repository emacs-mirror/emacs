/* Utility and Unix shadow routines for GNU Emacs on Windows NT.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Geoff Voelker (voelker@cs.washington.edu)                         7-29-94
*/

/* Define stat before including config.h.  */
#include <string.h>
#include <sys/stat.h>
int
nt_stat (char *filename, struct stat *statbuf)
{
  int r, l = strlen (filename);
  char *str = NULL;
  extern long *xmalloc ();
  extern void xfree ();

  /* stat has a bug when passed a name of a directory with a trailing
     backslash (but a trailing forward slash works fine).  */
  if (filename[l - 1] == '\\') 
    {
      str = (char *) xmalloc (l + 1);
      strcpy (str, filename);
      str[l - 1] = '/';
      r = stat (str, statbuf);
      xfree (str);
      return r;
    }
  else
    return stat (filename, statbuf);
}

/* Place a wrapper around the NT version of ctime.  It returns NULL
   on network directories, so we handle that case here.  
   Define it before including config.h.  (Ulrich Leodolter, 1/11/95).  */
char *
nt_ctime (const time_t *t)
{
  char *str = (char *) ctime (t);
  return (str ? str : "Sun Jan 01 00:00:00 1970");
}

#include <config.h>
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <ctype.h>

#define getwd _getwd
#include "lisp.h"
#undef getwd

#include <pwd.h>

#include "ndir.h"
#include "ntheap.h"

extern int report_file_error (char *, Lisp_Object);

/* Get the current working directory.  */
int
getwd (char *dir)
{
  return GetCurrentDirectory (MAXPATHLEN, dir);
}

/* Emulate gethostname.  */
int
gethostname (char *buffer, int size)
{
  /* NT only allows small host names, so the buffer is 
     certainly large enough.  */
  return !GetComputerName (buffer, &size);
}

/* Emulate getloadavg.  */
int
getloadavg (double loadavg[], int nelem)
{
  int i;

  /* A faithful emulation is going to have to be saved for a rainy day.  */
  for (i = 0; i < nelem; i++) 
    {
      loadavg[i] = 0.0;
    }
  return i;
}

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */
void
nt_sleep (int seconds)
{
  Sleep (seconds * 1000);
}

/* Emulate the Unix directory procedures opendir, closedir, 
   and readdir.  We can't use the procedures supplied in sysdep.c,
   so we provide them here.  */

struct direct dir_static;       /* simulated directory contents */
static int    dir_finding;
static HANDLE dir_find_handle;

DIR *
opendir (char *filename)
{
  DIR *dirp;

  /* Opening is done by FindFirstFile.  However, a read is inherent to
     this operation, so we have a flag to handle the open at read
     time.  This flag essentially means "there is a find-handle open and
     it needs to be closed."  */

  if (!(dirp = (DIR *) malloc (sizeof (DIR)))) 
    {
      return 0;
    }

  dirp->dd_fd = 0;
  dirp->dd_loc = 0;
  dirp->dd_size = 0;

  /* This is tacky, but we need the directory name for our
     implementation of readdir.  */
  strncpy (dirp->dd_buf, filename, DIRBLKSIZ);
  return dirp;
}

void
closedir (DIR *dirp)
{
  /* If we have a find-handle open, close it.  */
  if (dir_finding) 
    {
      FindClose (dir_find_handle);
      dir_finding = 0;
    }
  xfree ((char *) dirp);
}

struct direct *
readdir (DIR *dirp)
{
  WIN32_FIND_DATA find_data;
  
  /* If we aren't dir_finding, do a find-first, otherwise do a find-next. */
  if (!dir_finding) 
    {
      char filename[MAXNAMLEN + 3];
      int ln;

      strncpy (filename, dirp->dd_buf, MAXNAMLEN);
      ln = strlen (filename)-1;
      if (filename[ln] != '\\' && filename[ln] != ':')
	strcat (filename, "\\");
      strcat (filename, "*.*");

      dir_find_handle = FindFirstFile (filename, &find_data);

      if (dir_find_handle == INVALID_HANDLE_VALUE) 
	return NULL;

      dir_finding = 1;
    } 
  else 
    {
      if (!FindNextFile (dir_find_handle, &find_data))
	return NULL;
    }
  
  /* NT's unique ID for a file is 64 bits, so we have to fake it here.  
     This should work as long as we never use 0.  */
  dir_static.d_ino = 1;
  
  dir_static.d_reclen = sizeof (struct direct) - MAXNAMLEN + 3 +
    dir_static.d_namlen - dir_static.d_namlen % 4;
  
  dir_static.d_namlen = strlen (find_data.cFileName);
  strncpy (dir_static.d_name, find_data.cFileName, MAXNAMLEN);
  
  return &dir_static;
}

/* Emulate getpwuid and getpwnam.  */

int getuid ();	/* forward declaration */

static char the_passwd_name[256];
static char the_passwd_passwd[256];
static char the_passwd_gecos[256];
static char the_passwd_dir[256];
static char the_passwd_shell[256];

static struct passwd the_passwd = 
{
  the_passwd_name,
  the_passwd_passwd,
  0,
  0,
  0,
  the_passwd_gecos,
  the_passwd_dir,
  the_passwd_shell,
};

struct passwd *
getpwuid (int uid)
{
  int size = 256;
  
  if (!GetUserName (the_passwd.pw_name, &size))
    return NULL;

  the_passwd.pw_passwd[0] = '\0';
  the_passwd.pw_uid = 0;
  the_passwd.pw_gid = 0;
  strcpy (the_passwd.pw_gecos, the_passwd.pw_name);
  the_passwd.pw_dir[0] = '\0';
  the_passwd.pw_shell[0] = '\0';

  return &the_passwd;
}

struct passwd *
getpwnam (char *name)
{
  struct passwd *pw;
  
  pw = getpwuid (getuid ());
  if (!pw)
    return pw;

  if (strcmp (name, pw->pw_name))
    return NULL;

  return pw;
}


/* We don't have scripts to automatically determine the system configuration
   for Emacs before it's compiled, and we don't want to have to make the
   user enter it, so we define EMACS_CONFIGURATION to invoke this runtime
   routine.  */

static char configuration_buffer[32];

char *
get_emacs_configuration (void)
{
    char *arch, *oem, *os;

    /* Determine the processor type.  */
    switch (get_processor_type ()) 
      {
      case PROCESSOR_INTEL_386:
      case PROCESSOR_INTEL_486:
      case PROCESSOR_INTEL_PENTIUM:
	arch = "i386";
	break;
      case PROCESSOR_INTEL_860:
	arch = "i860";
	break;
      case PROCESSOR_MIPS_R2000:
      case PROCESSOR_MIPS_R3000:
      case PROCESSOR_MIPS_R4000:
	arch = "mips";
	break;
      case PROCESSOR_ALPHA_21064:
	arch = "alpha";
	break;
      default:
	arch = "unknown";
	break;
      }

    /* Let oem be "*" until we figure out how to decode the OEM field.  */
    oem = "*";

#ifdef WINDOWS95
    os = "win";
#else
    os = "nt";
#endif

    sprintf (configuration_buffer, "%s-%s-%s%d.%d", arch, oem, os,
	     get_nt_major_version (), get_nt_minor_version ());
    return configuration_buffer;
}

/* Conjure up inode and device numbers that will serve the purpose
   of Emacs.  Return 1 upon success, 0 upon failure.  */
int
get_inode_and_device_vals (Lisp_Object filename, Lisp_Object *p_inode, 
			   Lisp_Object *p_device)
{
  /* File uids on NT are found using a handle to a file, which
     implies that it has been opened.  Since we want to be able
     to stat an arbitrary file, we must open it, get the info,
     and then close it.
     
     Also, NT file uids are 64-bits.  This is a problem.  */

  HANDLE handle;
  BOOL result;
  DWORD attrs;
  BY_HANDLE_FILE_INFORMATION info;

  /* We have to stat files and directories differently, so check
     to see what filename references.  */
  attrs = GetFileAttributes (XSTRING (filename)->data);
  if (attrs == 0xFFFFFFFF) {
    return 0;
  }
  if (attrs & FILE_ATTRIBUTE_DIRECTORY) {
    /* Conjure up bogus, but unique, values.  */
    attrs = GetTickCount ();
    *p_inode = make_number (attrs);
    *p_device = make_number (attrs);
    return 1;
  }

  /* FIXME:  It shouldn't be opened without READ access, but NT on x86
     doesn't allow GetFileInfo in that case (NT on mips does).  */
     
  handle = CreateFile (XSTRING (filename)->data,
		       GENERIC_READ,
		       FILE_SHARE_READ | FILE_SHARE_WRITE,
		       NULL,
		       OPEN_EXISTING,
		       FILE_ATTRIBUTE_NORMAL,
		       NULL);
  if (handle == INVALID_HANDLE_VALUE)
    return 0;

  result = GetFileInformationByHandle (handle, &info);
  CloseHandle (handle);
  if (!result)
    return 0;

  *p_inode = make_number (info.nFileIndexLow);	        /* use the low value */
  *p_device = make_number (info.dwVolumeSerialNumber);

  return 1;
}

/* The following pipe routines are used to support our fork emulation.
   Since NT's crt dup always creates inherited handles, we
   must be careful in setting up pipes.  First create 
   non-inherited pipe handles, then create an inherited handle
   to the write end by dup-ing it, and then close the non-inherited
   end that was just duped.  This gives us one non-inherited handle
   on the read end and one inherited handle to the write end.  As
   the parent, we close the inherited handle to the write end after
   spawning the child.  */

/* From callproc.c  */
extern Lisp_Object Vbinary_process_input;
extern Lisp_Object Vbinary_process_output;

void
pipe_with_inherited_out (int fds[2])
{
  int inherit_out;
  unsigned int flags = _O_NOINHERIT;

  if (!NILP (Vbinary_process_output))
    flags |= _O_BINARY;

  _pipe (fds, 0, flags);
  inherit_out = dup (fds[1]);
  close (fds[1]);
  fds[1] = inherit_out;
}

void
pipe_with_inherited_in (int fds[2])
{
  int inherit_in;
  unsigned int flags = _O_NOINHERIT;

  if (!NILP (Vbinary_process_input))
    flags |= _O_BINARY;

  _pipe (fds, 0, flags);
  inherit_in = dup (fds[0]);
  close (fds[0]);
  fds[0] = inherit_in;
}

/* The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are inherited, we make them stdin,
   stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the parent's standard handles to the handles being passed in.
     (Note that _get_osfhandle is an io.h procedure that 
     maps crt file descriptors to NT file handles.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

void
prepare_standard_handles (int in, int out, int err, HANDLE handles[4])
{
  HANDLE parent, stdin_save, stdout_save, stderr_save, err_handle;

#ifdef WINDOWS95
  /* The Win95 beta doesn't set the standard handles correctly.
     Handicap subprocesses until we get a version that works correctly.  
     Undefining the subprocesses macro reveals other incompatibilities,
     so, since we're expecting subprocs to work in the near future, 
     disable them here.  */
  report_file_error ("Subprocesses currently disabled on Win95", Qnil);
#endif

  parent = GetCurrentProcess ();
  stdin_save = GetStdHandle (STD_INPUT_HANDLE);
  stdout_save = GetStdHandle (STD_OUTPUT_HANDLE);
  stderr_save = GetStdHandle (STD_ERROR_HANDLE);

  if (!DuplicateHandle (parent, 
		       GetStdHandle (STD_INPUT_HANDLE), 
		       parent,
		       &stdin_save, 
		       0, 
		       FALSE, 
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating parent's input handle", Qnil);
  
  if (!DuplicateHandle (parent,
		       GetStdHandle (STD_OUTPUT_HANDLE),
		       parent,
		       &stdout_save,
		       0,
		       FALSE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating parent's output handle", Qnil);
  
  if (!DuplicateHandle (parent,
		       GetStdHandle (STD_ERROR_HANDLE),
		       parent,
		       &stderr_save,
		       0,
		       FALSE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating parent's error handle", Qnil);
  
  if (!SetStdHandle (STD_INPUT_HANDLE, (HANDLE) _get_osfhandle (in)))
    report_file_error ("Changing stdin handle", Qnil);
  
  if (!SetStdHandle (STD_OUTPUT_HANDLE, (HANDLE) _get_osfhandle (out)))
    report_file_error ("Changing stdout handle", Qnil);
  
  /* We lose data if we use the same handle to the pipe for stdout and
     stderr, so make a duplicate.  This took a while to find.  */
  if (out == err) 
    {
      if (!DuplicateHandle (parent,
			   (HANDLE) _get_osfhandle (err),
			   parent,
			   &err_handle,
			   0,
			   TRUE,
			   DUPLICATE_SAME_ACCESS))
	report_file_error ("Duplicating out handle to make err handle.",
			  Qnil);
    } 
  else 
    {
      err_handle = (HANDLE) _get_osfhandle (err);
    }

  if (!SetStdHandle (STD_ERROR_HANDLE, err_handle))
    report_file_error ("Changing stderr handle", Qnil);

  handles[0] = stdin_save;
  handles[1] = stdout_save;
  handles[2] = stderr_save;
  handles[3] = err_handle;
}

void
reset_standard_handles (int in, int out, int err, HANDLE handles[4])
{
  HANDLE stdin_save = handles[0];
  HANDLE stdout_save = handles[1];
  HANDLE stderr_save = handles[2];
  HANDLE err_handle = handles[3];
  int i;

  if (!SetStdHandle (STD_INPUT_HANDLE, stdin_save))
    report_file_error ("Resetting input handle", Qnil);
  
  if (!SetStdHandle (STD_OUTPUT_HANDLE, stdout_save))
    {
      i = GetLastError ();
      report_file_error ("Resetting output handle", Qnil);
    }
  
  if (!SetStdHandle (STD_ERROR_HANDLE, stderr_save))
    report_file_error ("Resetting error handle", Qnil);
  
  if (out == err) 
    {
      /* If out and err are the same handle, then we duplicated out
	 and stuck it in err_handle.  Close the duplicate to clean up.  */
      if (!CloseHandle (err_handle))
	report_file_error ("Closing error handle duplicated from out.", 
			  Qnil);
    }
}

int
random ()
{
  /* rand () on NT gives us 15 random bits...hack together 30 bits.  */
  return ((rand () << 15) | rand ());
}

void
srandom (int seed)
{
  srand (seed);
}

/* Destructively turn backslashes into slashes.  */
void
dostounix_filename (p)
     register char *p;
{
  while (*p)
    {
      if (*p == '\\')
	*p = '/';
      p++;
    }
}

/* Routines that are no-ops on NT but are defined to get Emacs to compile.  */


int 
sigsetmask (int signal_mask) 
{ 
  return 0;
}

int 
sigblock (int sig) 
{ 
  return 0;
}

int 
kill (int pid, int signal) 
{ 
  return 0;
}

int 
setpgrp (int pid, int gid) 
{ 
  return 0;
}

int 
alarm (int seconds) 
{ 
  return 0;
}

int 
unrequest_sigio (void) 
{ 
  return 0;
}

int 
request_sigio (void) 
{ 
  return 0;
}

int 
getuid () 
{ 
  return 0; 
}

int 
geteuid () 
{ 
  return 0; 
}

/* Remove all CR's that are followed by a LF.
   (From msdos.c...probably should figure out a way to share it,
   although this code isn't going to ever change.)  */
int
crlf_to_lf (n, buf)
     register int n;
     register unsigned char *buf;
{
  unsigned char *np = buf;
  unsigned char *startp = buf;
  unsigned char *endp = buf + n;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}

#ifdef HAVE_TIMEVAL
#include <sys/timeb.h>

/* Emulate gettimeofday (Ulrich Leodolter, 1/11/95).  */
void 
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  struct _timeb tb;
  _ftime (&tb);

  tv->tv_sec = tb.time;
  tv->tv_usec = tb.millitm * 1000L;
  if (tz) 
    {
      tz->tz_minuteswest = tb.timezone;	/* minutes west of Greenwich  */
      tz->tz_dsttime = tb.dstflag;	/* type of dst correction  */
    }
}
#endif /* HAVE_TIMEVAL */


#ifdef PIGSFLY
Keep this around...we might need it later.
#ifdef WINDOWSNT
{
  /*
   * Find the user's real name by opening the process token and looking
   * up the name associated with the user-sid in that token.
   */

  char            b[256], Name[256], RefD[256];
  DWORD           length = 256, rlength = 256, trash;
  HANDLE          Token;
  SID_NAME_USE    User;

  if (1)
    Vuser_real_login_name = build_string ("foo");
  else if (!OpenProcessToken (GetCurrentProcess (), TOKEN_QUERY, &Token))
    {
      Vuser_real_login_name = build_string ("unknown");
    }
  else if (!GetTokenInformation (Token, TokenUser, (PVOID)b, 256,
				 &trash))
    {
      CloseHandle (Token);
      Vuser_real_login_name = build_string ("unknown");
    }
  else if (!LookupAccountSid ((void *)0, (PSID)b, Name, &length, RefD,
			      &rlength, &User))
    {
      CloseHandle (Token);
      Vuser_real_login_name = build_string ("unknown");
    }
  else
    Vuser_real_login_name = build_string (Name);
}
#else   /* not WINDOWSNT */
#endif  /* not WINDOWSNT */
#endif  /* PIGSFLY */
