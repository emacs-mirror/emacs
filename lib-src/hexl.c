/* Convert files for Emacs Hexl mode.
   Copyright (C) 1989, 2001-2015 Free Software Foundation, Inc.

Author: Keith Gabryelski
(according to authors.el)

This file is not considered part of GNU Emacs.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.  */


#include <config.h>

#include <stdio.h>
#include <ctype.h>

#include <binary-io.h>

#define DEFAULT_GROUPING	0x01
#define DEFAULT_BASE		16

int base = DEFAULT_BASE;
bool un_flag = false, iso_flag = false, endian = true;
int group_by = DEFAULT_GROUPING;
char *progname;

_Noreturn void usage (void);

int
main (int argc, char **argv)
{
  register long address;
  char string[18];
  FILE *fp;

  progname = *argv++; --argc;

  /*
   ** -hex		hex dump
   ** -oct		Octal dump
   ** -group-by-8-bits
   ** -group-by-16-bits
   ** -group-by-32-bits
   ** -group-by-64-bits
   ** -iso		iso character set.
   ** -big-endian	Big Endian
   ** -little-endian	Little Endian
   ** -un || -de	from hexl format to binary.
   ** --		End switch list.
   ** <filename>	dump filename
   ** -		(as filename == stdin)
   */

  while (*argv && *argv[0] == '-' && (*argv)[1])
    {
      /* A switch! */
      if (!strcmp (*argv, "--"))
	{
	  --argc; argv++;
	  break;
	}
      else if (!strcmp (*argv, "-un") || !strcmp (*argv, "-de"))
	{
	  un_flag = true;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-hex"))
	{
	  base = 16;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-iso"))
	{
	  iso_flag = true;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-oct"))
	{
	  base = 8;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-big-endian"))
	{
	  endian = true;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-little-endian"))
	{
	  endian = false;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-group-by-8-bits"))
	{
	  group_by = 0x00;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-group-by-16-bits"))
	{
	  group_by = 0x01;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-group-by-32-bits"))
	{
	  group_by = 0x03;
	  --argc; argv++;
	}
      else if (!strcmp (*argv, "-group-by-64-bits"))
	{
	  group_by = 0x07;
	  endian = false;
	  --argc; argv++;
	}
      else
	{
	  fprintf (stderr, "%s: invalid switch: \"%s\".\n", progname,
		   *argv);
	  usage ();
	}
    }

  do
    {
      if (*argv == NULL)
	fp = stdin;
      else
	{
	  char *filename = *argv++;

	  if (!strcmp (filename, "-"))
	    fp = stdin;
	  else if ((fp = fopen (filename, "r")) == NULL)
	    {
	      perror (filename);
	      continue;
	    }
	}

      if (un_flag)
	{
	  SET_BINARY (fileno (stdout));

	  for (;;)
	    {
	      int i, c = 0, d;
	      char buf[18];

#define hexchar(x) (isdigit (x) ? x - '0' : x - 'a' + 10)

	      /* Skip 10 bytes.  */
	      if (fread (buf, 1, 10, fp) != 10)
		break;

	      for (i=0; i < 16; ++i)
		{
		  if ((c = getc (fp)) == ' ' || c == EOF)
		    break;

		  d = getc (fp);
		  c = hexchar (c) * 0x10 + hexchar (d);
		  putchar (c);

		  if ((i&group_by) == group_by)
		    getc (fp);
		}

	      if (c == ' ')
		{
		  while ((c = getc (fp)) != '\n' && c != EOF)
		    ;

		  if (c == EOF)
		    break;
		}
	      else
		{
		  if (i < 16)
		    break;

		  /* Skip 18 bytes.  */
		  if (fread (buf, 1, 18, fp) != 18)
		    break;
		}
	    }
	}
      else
	{
	  SET_BINARY (fileno (fp));
	  address = 0;
	  string[0] = ' ';
	  string[17] = '\0';
	  for (;;)
	    {
	      register int i, c = 0;

	      for (i=0; i < 16; ++i)
		{
		  if ((c = getc (fp)) == EOF)
		    {
		      if (!i)
			break;

		      fputs ("  ", stdout);
		      string[i+1] = '\0';
		    }
		  else
		    {
		      if (!i)
			printf ("%08lx: ", address + 0ul);

		      if (iso_flag)
			string[i+1] =
			  (c < 0x20 || (c >= 0x7F && c < 0xa0)) ? '.' :c;
		      else
			string[i+1] = (c < 0x20 || c >= 0x7F) ? '.' : c;

		      printf ("%02x", c + 0u);
		    }

		  if ((i&group_by) == group_by)
		    putchar (' ');
		}

	      if (i)
		puts (string);

	      if (c == EOF)
		break;

	      address += 0x10;

	    }
	}

      if (fp != stdin)
	fclose (fp);

    } while (*argv != NULL);
  return EXIT_SUCCESS;
}

void
usage (void)
{
  fprintf (stderr, "usage: %s [-de] [-iso]\n", progname);
  exit (EXIT_FAILURE);
}


/* hexl.c ends here */
