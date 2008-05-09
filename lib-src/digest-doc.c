/* Give this program DOC-mm.nn.oo as standard input and it outputs to
   standard output a file of nroff output containing the doc strings.

Copyright (C) 1987, 1994, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
  2008 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


See also sorted-doc.c, which produces similar output
but in texinfo format and sorted by function/variable name.  */

#include <stdio.h>

#ifdef DOS_NT
#include <fcntl.h>		/* for O_BINARY */
#include <io.h>			/* for setmode */
#endif

int
main ()
{
  register int ch;
  register int notfirst = 0;

#ifdef DOS_NT
  /* DOC is a binary file.  */
  if (!isatty (fileno (stdin)))
    setmode (fileno (stdin), O_BINARY);
#endif

  printf (".TL\n");
  printf ("Command Summary for GNU Emacs\n");
  printf (".AU\nRichard M. Stallman\n");
  while ((ch = getchar ()) != EOF)
    {
      if (ch == '\037')
	{
	  if (notfirst)
	    printf ("\n.DE");
	  else
	    notfirst = 1;

	  printf ("\n.SH\n");

	  ch = getchar ();
	  printf (ch == 'F' ? "Function " : "Variable ");

	  while ((ch = getchar ()) != '\n')  /* Changed this line */
	    {
	      if (ch != EOF)
		  putchar (ch);
	      else
		{
		  ungetc (ch, stdin);
		  break;
		}
	    }
	  printf ("\n.DS L\n");
	}
      else
	putchar (ch);
    }
  return 0;
}

/* arch-tag: 2ba2c9b0-4157-4eba-bd9f-967e3677e35f
   (do not change this comment) */
