/*
 * b2m - a filter for Babyl -> Unix mail files
 * The copyright on this file has been disclaimed.
 *
 * usage:	b2m < babyl > mailbox
 *
 * I find this useful whenever I have to use a
 * system which - shock horror! - doesn't run
 * GNU Emacs. At least now I can read all my
 * GNU Emacs Babyl format mail files!
 *
 * it's not much but it's free!
 *
 *   Ed Wilkinson
 *   E.Wilkinson@massey.ac.nz
 *   Mon Nov 7 15:54:06 PDT 1988
 */

/* Made conformant to the GNU coding standards January, 1995
   by Francesco Potorti` <pot@cnuce.cnr.it>. */

#ifdef HAVE_CONFIG_H
#include <config.h>
/* On some systems, Emacs defines static as nothing for the sake
   of unexec.  We don't want that here since we don't use unexec. */
#undef static
#endif

#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <getopt.h>
#ifdef MSDOS
#include <fcntl.h>
#endif

#undef TRUE
#define TRUE	1
#undef FALSE
#define FALSE	0

#define streq(s,t)	(strcmp (s, t) == 0)
#define strneq(s,t,n)	(strncmp (s, t, n) == 0)

typedef int logical;

/*
 * A `struct linebuffer' is a structure which holds a line of text.
 * `readline' reads a line from a stream into a linebuffer and works
 * regardless of the length of the line.
 */
struct linebuffer
{
  long size;
  char *buffer;
};

extern char *strtok();

long *xmalloc (), *xrealloc ();
char *concat ();
long readline ();
void fatal ();

/*
 * xnew -- allocate storage.  SYNOPSIS: Type *xnew (int n, Type);
 */
#define xnew(n, Type)	((Type *) xmalloc ((n) * sizeof (Type)))



char *progname;

struct option longopts[] =
{
  { "help",			no_argument,	   NULL,     'h'   },
  { "version",			no_argument,	   NULL,     'V'   },
  { 0 }
};

extern int optind;

int
main (argc, argv)
     int argc;
     char **argv;
{
  logical labels_saved, printing, header;
  time_t ltoday;
  char *labels, *p, *today;
  struct linebuffer data;

#ifdef MSDOS
  _fmode = O_BINARY;		/* all of files are treated as binary files */
#if __DJGPP__ > 1
  if (!isatty (fileno (stdout)))
    setmode (fileno (stdout), O_BINARY);
  if (!isatty (fileno (stdin)))
    setmode (fileno (stdin), O_BINARY);
#else /* not __DJGPP__ > 1 */
  (stdout)->_flag &= ~_IOTEXT;
  (stdin)->_flag &= ~_IOTEXT;
#endif /* not __DJGPP__ > 1 */
#endif
  progname = argv[0];

  while (1)
    {
      int opt = getopt_long (argc, argv, "hV", longopts, 0);
      if (opt == EOF)
	break;

      switch (opt)
	{
	case 'V':
	  printf ("%s (GNU Emacs %s)\n", "b2m", VERSION);
	  puts ("b2m is in the public domain.");
	  exit (EXIT_SUCCESS);

	case 'h':
	  fprintf (stderr, "Usage: %s <babylmailbox >unixmailbox\n", progname);
	  exit (EXIT_SUCCESS);
	}
    }

  if (optind != argc)
    {
      fprintf (stderr, "Usage: %s <babylmailbox >unixmailbox\n", progname);
      exit (EXIT_SUCCESS);
    }

  labels_saved = printing = header = FALSE;
  ltoday = time (0);
  today = ctime (&ltoday);
  data.size = 200;
  data.buffer = xnew (200, char);

  if (readline (&data, stdin) == 0
      || !strneq (data.buffer, "BABYL OPTIONS:", 14))
    fatal ("standard input is not a Babyl mailfile.");

  while (readline (&data, stdin) > 0)
    {
      if (streq (data.buffer, "*** EOOH ***") && !printing)
	{
	  printing = header = TRUE;
	  printf ("From \"Babyl to mail by %s\" %s", progname, today);
	  continue;
	}

      if (data.buffer[0] == '\037')
	{
	  if (data.buffer[1] == '\0')
	    continue;
	  else if (data.buffer[1] == '\f')
	    {
	      /* Save labels. */
	      readline (&data, stdin);
	      p = strtok (data.buffer, " ,\r\n\t");
	      labels = "X-Babyl-Labels: ";

	      while ((p = strtok (NULL, " ,\r\n\t")))
		labels = concat (labels, p, ", ");

	      p = &labels[strlen (labels) - 2];
	      if (*p == ',')
		*p = '\0';
	      printing = header = FALSE;
	      labels_saved = TRUE;
	      continue;
	    }
	}

      if ((data.buffer[0] == '\0') && header)
	{
	  header = FALSE;
	  if (labels_saved)
	    puts (labels);
	}

      if (printing)
	puts (data.buffer);
    }

  return EXIT_SUCCESS;
}



/*
 * Return a newly-allocated string whose contents
 * concatenate those of s1, s2, s3.
 */
char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = xnew (len1 + len2 + len3 + 1, char);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  result[len1 + len2 + len3] = '\0';

  return result;
}

/*
 * Read a line of text from `stream' into `linebuffer'.
 * Return the number of characters read from `stream',
 * which is the length of the line including the newline, if any.
 */
long
readline (linebuffer, stream)
     struct linebuffer *linebuffer;
     register FILE *stream;
{
  char *buffer = linebuffer->buffer;
  register char *p = linebuffer->buffer;
  register char *pend;
  int chars_deleted;

  pend = p + linebuffer->size;	/* Separate to avoid 386/IX compiler bug.  */

  while (1)
    {
      register int c = getc (stream);
      if (p == pend)
	{
	  linebuffer->size *= 2;
	  buffer = (char *) xrealloc (buffer, linebuffer->size);
	  p += buffer - linebuffer->buffer;
	  pend = buffer + linebuffer->size;
	  linebuffer->buffer = buffer;
	}
      if (c == EOF)
	{
	  *p = '\0';
	  chars_deleted = 0;
	  break;
	}
      if (c == '\n')
	{
	  if (p > buffer && p[-1] == '\r')
	    {
	      *--p = '\0';
	      chars_deleted = 2;
	    }
	  else
	    {
	      *p = '\0';
	      chars_deleted = 1;
	    }
	  break;
	}
      *p++ = c;
    }

  return (p - buffer + chars_deleted);
}

/*
 * Like malloc but get fatal error if memory is exhausted.
 */
long *
xmalloc (size)
     unsigned int size;
{
  long *result = (long *) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted");
  return result;
}

long *
xrealloc (ptr, size)
     char *ptr;
     unsigned int size;
{
  long *result = (long *) realloc (ptr, size);
  if (result == NULL)
    fatal ("virtual memory exhausted");
  return result;
}

void
fatal (message)
     char *message;
{
  fprintf (stderr, "%s: %s\n", progname, message);
  exit (EXIT_FAILURE);
}

/* b2m.c ends here */
