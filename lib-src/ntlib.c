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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

   Geoff Voelker (voelker@cs.washington.edu)                         10-8-94
*/

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

#define MAXPATHLEN _MAX_PATH

/* Emulate sleep...we could have done this with a define, but that
   would necessitate including windows.h in the files that used it.
   This is much easier.  */
void
nt_sleep(int seconds)
{
  Sleep (seconds * 1000);
}

/* Get the current working directory.  */
int
getwd (char *dir)
{
  return GetCurrentDirectory (MAXPATHLEN, dir);
}

static HANDLE getppid_parent;
static int    getppid_ppid;

int
getppid(void)
{
  char *ppid;
  DWORD result;

  ppid = getenv ("__PARENT_PROCESS_ID");
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
