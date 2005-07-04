/* Utility and Unix shadow routines for GNU Emacs support programs on NT.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
Boston, MA 02110-1301, USA.

   Geoff Voelker (voelker@cs.washington.edu)                         10-8-94
*/

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <direct.h>

#include "ntlib.h"

#define MAXPATHLEN _MAX_PATH

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */
void
sleep(unsigned long seconds)
{
  Sleep (seconds * 1000);
}

/* Get the current working directory.  */
char *
getwd (char *dir)
{
  if (GetCurrentDirectory (MAXPATHLEN, dir) > 0)
    return dir;
  return NULL;
}

static HANDLE getppid_parent;
static int    getppid_ppid;

int
getppid(void)
{
  char *ppid;
  DWORD result;

  ppid = getenv ("EM_PARENT_PROCESS_ID");
  if (!ppid)
    {
      printf("no pid.\n");
      return 0;
    }
  else
    {
      getppid_ppid = atoi (ppid);
    }

  if (!getppid_parent)
    {
      getppid_parent = OpenProcess (SYNCHRONIZE, FALSE, atoi(ppid));
      if (!getppid_parent)
	{
	  printf ("Failed to open handle to parent process: %d\n",
		 GetLastError());
	  exit (1);
	}
    }

  result = WaitForSingleObject (getppid_parent, 0);
  switch (result)
    {
    case WAIT_TIMEOUT:
      /* The parent is still alive.  */
      return getppid_ppid;
    case WAIT_OBJECT_0:
      /* The parent is gone.  Return the pid of Unix init (1).  */
      return 1;
    case WAIT_FAILED:
    default:
      printf ("Checking parent status failed: %d\n", GetLastError());
      exit (1);
    }
}

char *
getlogin ()
{
  static char user_name[256];
  DWORD  length = sizeof (user_name);

  if (GetUserName (user_name, &length))
    return user_name;
  return NULL;
}

char *
cuserid (char * s)
{
  char * name = getlogin ();
  if (s)
    return strcpy (s, name ? name : "");
  return name;
}

int
getuid ()
{
  return 0;
}

int
setuid (int uid)
{
  return 0;
}

struct passwd *
getpwuid (int uid)
{
  return NULL;
}

char *
getpass (const char * prompt)
{
  static char input[256];
  HANDLE in;
  HANDLE err;
  DWORD  count;

  in = GetStdHandle (STD_INPUT_HANDLE);
  err = GetStdHandle (STD_ERROR_HANDLE);

  if (in == INVALID_HANDLE_VALUE || err == INVALID_HANDLE_VALUE)
    return NULL;

  if (WriteFile (err, prompt, strlen (prompt), &count, NULL))
    {
      int istty = (GetFileType (in) == FILE_TYPE_CHAR);
      DWORD old_flags;
      int rc;

      if (istty)
	{
	  if (GetConsoleMode (in, &old_flags))
	    SetConsoleMode (in, ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT);
	  else
	    istty = 0;
	}
      rc = ReadFile (in, input, sizeof (input), &count, NULL);
      if (count >= 2 && input[count - 2] == '\r')
	input[count - 2] = '\0';
      else
	{
	  char buf[256];
	  while (ReadFile (in, buf, sizeof (buf), &count, NULL) > 0)
	    if (count >= 2 && buf[count - 2] == '\r')
	      break;
	}
      WriteFile (err, "\r\n", 2, &count, NULL);
      if (istty)
	SetConsoleMode (in, old_flags);
      if (rc)
	return input;
    }

  return NULL;
}

int
fchown (int fd, int uid, int gid)
{
  return 0;
}

/* Place a wrapper around the MSVC version of ctime.  It returns NULL
   on network directories, so we handle that case here.
   (Ulrich Leodolter, 1/11/95).  */
char *
sys_ctime (const time_t *t)
{
  char *str = (char *) ctime (t);
  return (str ? str : "Sun Jan 01 00:00:00 1970");
}

FILE *
sys_fopen(const char * path, const char * mode)
{
  return fopen (path, mode);
}

int
sys_chdir (const char * path)
{
  return _chdir (path);
}

/* arch-tag: 7b63fb83-70ee-4124-8822-54e53e5d0773
   (do not change this comment) */
