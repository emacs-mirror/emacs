/* test-distrib.c --- testing distribution of nonprinting chars

   Copyright (C) 1987, 1993, 1994, 1995, 1999, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

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
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

/* Break string in two parts to avoid buggy C compilers that ignore characters
   after nulls in strings.  */

char string1[] = "Testing distribution of nonprinting chars:\n\
Should be 0177: \177 Should be 0377: \377 Should be 0212: \212.\n\
Should be 0000: ";

char string2[] = ".\n\
This file is read by the `test-distribution' program.\n\
If you change it, you will make that program fail.\n";

char buf[300];

/* Like `read' but keeps trying until it gets SIZE bytes or reaches eof.  */
int
cool_read (fd, buf, size)
     int fd;
     char *buf;
     int size;
{
  int num, sofar = 0;

  while (1)
    {
      if ((num = read (fd, buf + sofar, size - sofar)) == 0)
	return sofar;
      else if (num < 0)
	return num;
      sofar += num;
    }
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int fd;

  if (argc != 2)
    {
      fprintf (stderr, "Usage: %s testfile\n", argv[0]);
      exit (EXIT_FAILURE);
    }
  fd = open (argv[1], O_RDONLY);
  if (fd < 0)
    {
      perror (argv[1]);
      exit (EXIT_FAILURE);
    }
  if (cool_read (fd, buf, sizeof string1) != sizeof string1 ||
      strcmp (buf, string1) ||
      cool_read (fd, buf, sizeof string2) != sizeof string2 - 1 ||
      strncmp (buf, string2, sizeof string2 - 1))
    {
      fprintf (stderr, "Data in file `%s' has been damaged.\n\
Most likely this means that many nonprinting characters\n\
have been corrupted in the files of Emacs, and it will not work.\n",
	       argv[1]);
      exit (EXIT_FAILURE);
    }
  close (fd);
  return EXIT_SUCCESS;
}

/* arch-tag: 3a89005d-df98-4c32-aa9f-33570e16a26a
   (do not change this comment) */

/* test-distrib.c ends here */
