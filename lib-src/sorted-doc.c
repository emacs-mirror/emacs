/* Give this program DOC-mm.nn.oo as standard input and it outputs to
   standard output a file of texinfo input containing the doc strings.

Copyright (C) 1989, 1992, 1994, 1996, 1999, 2000, 2001, 2002, 2003,
              2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* This version sorts the output by function name.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#ifdef DOS_NT
#include <fcntl.h>		/* for O_BINARY */
#include <io.h>			/* for setmode */
#endif
#ifndef HAVE_STDLIB_H		/* config.h includes stdlib.  */
#ifndef WINDOWSNT		/* src/s/ms-w32.h includes stdlib.h */
extern char *malloc ();
#endif
#endif

#define NUL	'\0'
#define MARKER '\037'

#define DEBUG 0

typedef struct line LINE;

struct line
{
  LINE *next;			/* ptr to next or NULL */
  char *line;			/* text of the line */
};

typedef struct docstr DOCSTR;

struct docstr			/* Allocated thing for an entry. */
{
  DOCSTR *next;			/* next in the chain */
  char *name;			/* name of the function or var */
  LINE *first;			/* first line of doc text. */
  char type;			/* 'F' for function, 'V' for variable */
};


/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

void
error (s1, s2)
     char *s1, *s2;
{
  fprintf (stderr, "sorted-doc: ");
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Print error message and exit.  */

void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (EXIT_FAILURE);
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (size)
     int size;
{
  char *result = malloc ((unsigned)size);
  if (result == NULL)
    fatal ("%s", "virtual memory exhausted");
  return result;
}

char *
xstrdup (str)
     char * str;
{
  char *buf = xmalloc (strlen (str) + 1);
  (void) strcpy (buf, str);
  return (buf);
}

/* Comparison function for qsort to call.  */

int
cmpdoc (a, b)
     DOCSTR **a;
     DOCSTR **b;
{
  register int val = strcmp ((*a)->name, (*b)->name);
  if (val) return val;
  return (*a)->type - (*b)->type;
}


enum state
{
  WAITING, BEG_NAME, NAME_GET, BEG_DESC, DESC_GET
};

char *states[] =
{
  "WAITING", "BEG_NAME", "NAME_GET", "BEG_DESC", "DESC_GET"
};

int
main ()
{
  register DOCSTR *dp = NULL;	/* allocated DOCSTR */
  register LINE *lp = NULL;	/* allocated line */
  register char *bp;		/* ptr inside line buffer */
  register enum state state = WAITING; /* state at start */
  int cnt = 0;			/* number of DOCSTRs read */

  DOCSTR *docs = NULL;          /* chain of allocated DOCSTRS */
  char buf[512];		/* line buffer */

#ifdef DOS_NT
  /* DOC is a binary file.  */
  if (!isatty (fileno (stdin)))
    setmode (fileno (stdin), O_BINARY);
#endif

  bp = buf;

  while (1)			/* process one char at a time */
    {
      /* this char from the DOCSTR file */
      register int ch = getchar ();

      /* Beginnings */

      if (state == WAITING)
	{
	  if (ch == MARKER)
	    state = BEG_NAME;
	}
      else if (state == BEG_NAME)
	{
	  cnt++;
	  if (dp == NULL)	/* first dp allocated */
	    {
	      docs = dp = (DOCSTR*) xmalloc (sizeof (DOCSTR));
	    }
	  else			/* all the rest */
	    {
	      dp->next = (DOCSTR*) xmalloc (sizeof (DOCSTR));
	      dp = dp->next;
	    }
	  lp = NULL;
	  dp->next = NULL;
	  bp = buf;
	  state = NAME_GET;
	  /* Record whether function or variable.  */
	  dp->type = ch;
	  ch = getchar ();
	}
      else if (state == BEG_DESC)
	{
	  if (lp == NULL)	/* first line for dp */
	    {
	      dp->first = lp = (LINE*)xmalloc (sizeof (LINE));
	    }
	  else			/* continuing lines */
	    {
	      lp->next = (LINE*)xmalloc (sizeof (LINE));
	      lp = lp->next;
	    }
	  lp->next = NULL;
	  bp = buf;
	  state = DESC_GET;
	}

      /* process gets */

      if (state == NAME_GET || state == DESC_GET)
	{
	  if (ch != MARKER && ch != '\n' && ch != EOF)
	    {
	      *bp++ = ch;
	    }
	  else			/* saving and changing state */
	    {
	      *bp = NUL;
	      bp = xstrdup (buf);

	      if (state == NAME_GET)
		dp->name = bp;
	      else
		lp->line = bp;

	      bp = buf;
	      state =  (ch == MARKER) ? BEG_NAME : BEG_DESC;
	    }
	}			/* NAME_GET || DESC_GET */
      if (ch == EOF)
	break;
    }

  {
    DOCSTR **array;
    register int i;		/* counter */

    /* build array of ptrs to DOCSTRs */

    array = (DOCSTR**)xmalloc (cnt * sizeof (*array));
    for (dp = docs, i = 0; dp != NULL ; dp = dp->next)
      array[i++] = dp;

    /* sort the array by name; within each name, by type */

    qsort ((char*)array, cnt, sizeof (DOCSTR*), cmpdoc);

    /* write the output header */

    printf ("\\input texinfo  @c -*-texinfo-*-\n");
    printf ("@setfilename ../info/summary\n");
    printf ("@settitle Command Summary for GNU Emacs\n");
    printf ("@finalout\n");
    printf ("@unnumbered Command Summary for GNU Emacs\n");
    printf ("@table @asis\n");
    printf ("\n");
    printf ("@iftex\n");
    printf ("@global@let@ITEM@item\n");
    printf ("@def@item{@filbreak@vskip5pt@ITEM}\n");
    printf ("@font@tensy cmsy10 scaled @magstephalf\n");
    printf ("@font@teni cmmi10 scaled @magstephalf\n");
    printf ("@def\\{{@tensy@char110}}\n"); /* this backslash goes with cmr10 */
    printf ("@def|{{@tensy@char106}}\n");
    printf ("@def@{{{@tensy@char102}}\n");
    printf ("@def@}{{@tensy@char103}}\n");
    printf ("@def<{{@teni@char62}}\n");
    printf ("@def>{{@teni@char60}}\n");
    printf ("@chardef@@64\n");
    printf ("@catcode43=12\n");
    printf ("@tableindent-0.2in\n");
    printf ("@end iftex\n");

    /* print each function from the array */

    for (i = 0; i < cnt; i++)
      {
	printf ("\n@item %s @code{%s}\n@display\n",
		array[i]->type == 'F' ? "Function" : "Variable",
		array[i]->name);

	for (lp = array[i]->first; lp != NULL ; lp = lp->next)
	  {
	    for (bp = lp->line; *bp; bp++)
	      {
		/* the characters "@{}" need special treatment */
		if (*bp == '@' || *bp == '{' || *bp == '}')
		  {
		    putchar('@');
		  }
		putchar(*bp);
	      }
	    putchar ('\n');
	  }
	printf("@end display\n");
	/* Try to avoid a save size overflow in the TeX output
           routine.  */
	if (i%100 == 0 && i > 0 && i != cnt)
	  printf("\n@end table\n@table @asis\n");
      }

    printf ("@end table\n");
    printf ("@bye\n");
  }

  return EXIT_SUCCESS;
}

/* arch-tag: ce28f204-1e70-4b34-8210-3d54a5662071
   (do not change this comment) */

/* sorted-doc.c ends here */
