#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Cancel substitutions made by config.h for Emacs.  */
#undef open
#undef read
#undef write
#undef close

#include <stdio.h>

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
      exit (2);
    }
  fd = open (argv[1], O_RDONLY);
  if (fd < 0)
    {
      perror (argv[1]);
      exit (2);
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
      exit (2);
    }
  close (fd);
#ifdef VMS
  exit (1);			/* On VMS, success is 1.  */
#endif
  return (0);
}
