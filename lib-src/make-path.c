/* Make all the directories along a path.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

/* This program works like mkdir, except that it generates
   intermediate directories if they don't exist.  This is just like
   the `mkdir -p' command on most systems; unfortunately, the mkdir
   command on some of the purer BSD systems (like Mt. Xinu) don't have
   that option. */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>

extern int errno;

char *prog_name;

/* Create directory DIRNAME if it does not exist already.
   Then give permission for everyone to read and search it.
   Return 0 if successful, 1 if not.  */

int
touchy_mkdir (dirname)
     char *dirname;
{
  struct stat buf;

  /* If DIRNAME already exists and is a directory, don't create.  */
  if (! (stat (dirname, &buf) >= 0
	 && (buf.st_mode & S_IFMT) == S_IFDIR))
    {
      /* Otherwise, try to make it.  If DIRNAME exists but isn't a directory,
	 this will signal an error.  */
      if (mkdir (dirname, 0777) < 0)
	{
	  fprintf (stderr, "%s: ", prog_name);
	  perror (dirname);
	  return 1;
	}
    }

  /* Make sure everyone can look at this directory.  */
  if (stat (dirname, &buf) < 0)
    {
      fprintf (stderr, "%s: ", prog_name);
      perror (dirname);
      return 1;
    }
  if (chmod (dirname, 0555 | (buf.st_mode & 0777)) < 0)
    {
      fprintf (stderr, "%s: ", prog_name);
      perror (dirname);
    }

  return 0;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  prog_name = *argv;

  for (argc--, argv++; argc > 0; argc--, argv++)
    {
      char *dirname = *argv;
      int i;

      /* Stop at each slash in dirname and try to create the directory.
	 Skip any initial slash.  */
      for (i = (dirname[0] == '/') ? 1 : 0; dirname[i]; i++)
	if (dirname[i] == '/')
	  {
	    dirname[i] = '\0';
	    if (touchy_mkdir (dirname) < 0)
	      goto next_dirname;
	    dirname[i] = '/';
	  }

      touchy_mkdir (dirname);

    next_dirname:
      ;
    }

  return 0;
}
