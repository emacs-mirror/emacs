/* Generate doc-string file for GNU Emacs from source files.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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

/* The arguments given to this program are all the C and Lisp source files
 of GNU Emacs.  .elc and .el and .c files are allowed.
 A .o file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.

 The results, which go to standard output or to a file
 specified with -a or -o (-a to append, -o to start from nothing),
 are entries containing function or variable names and their documentation.
 Each entry starts with a ^_ character.
 Then comes F for a function or V for a variable.
 Then comes the function or variable name, terminated with a newline.
 Then comes the documentation for that function or variable.
 */

#define NO_SHORTNAMES   /* Tell config not to load remap.h */
#include <../src/config.h>

#include <stdio.h>
#ifdef MSDOS
#include <fcntl.h>
#endif /* MSDOS */
#ifdef WINDOWSNT
#include <stdlib.h>
#include <fcntl.h>
#include <direct.h>
#endif /* WINDOWSNT */

#ifdef DOS_NT
#define READ_TEXT "rt"
#define READ_BINARY "rb"
#else  /* not DOS_NT */
#define READ_TEXT "r"
#define READ_BINARY "r"
#endif /* not DOS_NT */

int scan_file ();
int scan_lisp_file ();
int scan_c_file ();

#ifdef MSDOS
/* s/msdos.h defines this as sys_chdir, but we're not linking with the
   file where that function is defined.  */
#undef chdir
#endif

/* Stdio stream for output to the DOC file.  */
FILE *outfile;

/* Name this program was invoked with.  */
char *progname;

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

/* VARARGS1 */
void
error (s1, s2)
     char *s1, *s2;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Print error message and exit.  */

/* VARARGS1 */
void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (1);
}

/* Like malloc but get fatal error if memory is exhausted.  */

long *
xmalloc (size)
     unsigned int size;
{
  long *result = (long *) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", 0);
  return result;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  int i;
  int err_count = 0;
  int first_infile;

  progname = argv[0];

  outfile = stdout;

  /* Don't put CRs in the DOC file.  */
#ifdef MSDOS
#if 0  /* Suspicion is that this causes hanging.
	  So instead we require people to use -o on MSDOS.  */
  _fmode = O_BINARY;
  (stdout)->_flag &= ~_IOTEXT;
  _setmode (fileno (stdout), O_BINARY);
#endif
  outfile = 0;
#endif /* MSDOS */
#ifdef WINDOWSNT
  _fmode = O_BINARY;
  _setmode (fileno (stdout), O_BINARY);
#endif /* WINDOWSNT */

  /* If first two args are -o FILE, output to FILE.  */
  i = 1;
  if (argc > i + 1 && !strcmp (argv[i], "-o"))
    {
      outfile = fopen (argv[i + 1], "w");
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-a"))
    {
      outfile = fopen (argv[i + 1], "a");
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-d"))
    {
      chdir (argv[i + 1]);
      i += 2;
    }

  if (outfile == 0)
    fatal ("No output file specified", "");

  first_infile = i;
  for (; i < argc; i++)
    {
      int j;
      /* Don't process one file twice.  */
      for (j = first_infile; j < i; j++)
	if (! strcmp (argv[i], argv[j]))
	  break;
      if (j == i)
	err_count += scan_file (argv[i]);
    }
#ifndef VMS
  exit (err_count > 0);
#endif /* VMS */
  return err_count > 0;
}

/* Read file FILENAME and output its doc strings to outfile.  */
/* Return 1 if file is not found, 0 if it is found.  */

int
scan_file (filename)
     char *filename;
{
  int len = strlen (filename);
  if (!strcmp (filename + len - 4, ".elc"))
    return scan_lisp_file (filename, READ_BINARY);
  else if (!strcmp (filename + len - 3, ".el"))
    return scan_lisp_file (filename, READ_TEXT);
  else
    return scan_c_file (filename, READ_TEXT);
}

char buf[128];

/* Skip a C string from INFILE,
 and return the character that follows the closing ".
 If printflag is positive, output string contents to outfile.
 If it is negative, store contents in buf.
 Convert escape sequences \n and \t to newline and tab;
 discard \ followed by newline.  */

int
read_c_string (infile, printflag)
     FILE *infile;
     int printflag;
{
  register int c;
  char *p = buf;

  c = getc (infile);
  while (c != EOF)
    {
      while (c != '"' && c != EOF)
	{
	  if (c == '\\')
	    {
	      c = getc (infile);
	      if (c == '\n')
		{
		  c = getc (infile);
		  continue;
		}
	      if (c == 'n')
		c = '\n';
	      if (c == 't')
		c = '\t';
	    }
	  if (printflag > 0)
	    putc (c, outfile);
	  else if (printflag < 0)
	    *p++ = c;
	  c = getc (infile);
	}
      c = getc (infile);
      if (c != '"')
	break;
      /* If we had a "", concatenate the two strings.  */
      c = getc (infile);
    }

  if (printflag < 0)
    *p = 0;

  return c;
}

/* Write to file OUT the argument names of function FUNC, whose text is in BUF.
   MINARGS and MAXARGS are the minimum and maximum number of arguments.  */

void
write_c_args (out, func, buf, minargs, maxargs)
     FILE *out;
     char *func, *buf;
     int minargs, maxargs;
{
  register char *p;
  int in_ident = 0;
  int just_spaced = 0;
  int need_space = 1;

  fprintf (out, "(%s", func);

  if (*buf == '(')
    ++buf;

  for (p = buf; *p; p++)
    {
      char c = *p;
      int ident_start = 0;

      /* Notice when we start printing a new identifier.  */
      if ((('A' <= c && c <= 'Z')
	   || ('a' <= c && c <= 'z')
	   || ('0' <= c && c <= '9')
	   || c == '_')
	  != in_ident)
	{
	  if (!in_ident)
	    {
	      in_ident = 1;
	      ident_start = 1;

	      if (need_space)
		putc (' ', out);

	      if (minargs == 0 && maxargs > 0)
		fprintf (out, "&optional ");
	      just_spaced = 1;

	      minargs--;
	      maxargs--;
	    }
	  else
	    in_ident = 0;
	}

      /* Print the C argument list as it would appear in lisp:
	 print underscores as hyphens, and print commas as spaces.
	 Collapse adjacent spaces into one.  */
      if (c == '_') c = '-';
      if (c == ',') c = ' ';

      /* In C code, `default' is a reserved word, so we spell it
	 `defalt'; unmangle that here.  */
      if (ident_start
	  && strncmp (p, "defalt", 6) == 0
	  && ! (('A' <= p[6] && p[6] <= 'Z')
		|| ('a' <= p[6] && p[6] <= 'z')
		|| ('0' <= p[6] && p[6] <= '9')
		|| p[6] == '_'))
	{
	  fprintf (out, "DEFAULT");
	  p += 5;
	  in_ident = 0;
	  just_spaced = 0;
	}
      else if (c != ' ' || ! just_spaced)
	{
	  if (c >= 'a' && c <= 'z')
	    /* Upcase the letter.  */
	    c += 'A' - 'a';
	  putc (c, out);
	}

      just_spaced = (c == ' ');
      need_space = 0;
    }
}

/* Read through a c file.  If a .o file is named,
   the corresponding .c file is read instead.
   Looks for DEFUN constructs such as are defined in ../src/lisp.h.
   Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED.  */

int
scan_c_file (filename, mode)
     char *filename, *mode;
{
  FILE *infile;
  register int c;
  register int commas;
  register int defunflag;
  register int defvarperbufferflag;
  register int defvarflag;
  int minargs, maxargs;
  int extension = filename[strlen (filename) - 1];

  if (extension == 'o')
    filename[strlen (filename) - 1] = 'c';

  infile = fopen (filename, mode);

  /* No error if non-ex input file */
  if (infile == NULL)
    {
      perror (filename);
      return 0;
    }

  /* Reset extension to be able to detect duplicate files. */
  filename[strlen (filename) - 1] = extension;

  c = '\n';
  while (!feof (infile))
    {
      if (c != '\n')
	{
	  c = getc (infile);
	  continue;
	}
      c = getc (infile);
      if (c == ' ')
	{
	  while (c == ' ')
	    c = getc (infile);
	  if (c != 'D')
	    continue;
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  if (c != 'V')
	    continue;
	  c = getc (infile);
	  if (c != 'A')
	    continue;
	  c = getc (infile);
	  if (c != 'R')
	    continue;
	  c = getc (infile);
	  if (c != '_')
	    continue;

	  defvarflag = 1;
	  defunflag = 0;

	  c = getc (infile);
	  defvarperbufferflag = (c == 'P');

	  c = getc (infile);
	}
      else if (c == 'D')
	{
	  c = getc (infile);
	  if (c != 'E')
	    continue;
	  c = getc (infile);
	  if (c != 'F')
	    continue;
	  c = getc (infile);
	  defunflag = c == 'U';
	  defvarflag = 0;
	}
      else continue;

      while (c != '(')
	{
	  if (c < 0)
	    goto eof;
	  c = getc (infile);
	}

      c = getc (infile);
      if (c != '"')
	continue;
      c = read_c_string (infile, -1);

      if (defunflag)
	commas = 5;
      else if (defvarperbufferflag)
	commas = 2;
      else if (defvarflag)
	commas = 1;
      else  /* For DEFSIMPLE and DEFPRED */
	commas = 2;

      while (commas)
	{
	  if (c == ',')
	    {
	      commas--;
	      if (defunflag && (commas == 1 || commas == 2))
		{
		  do
		    c = getc (infile);
		  while (c == ' ' || c == '\n' || c == '\t');
		  if (c < 0)
		    goto eof;
		  ungetc (c, infile);
		  if (commas == 2) /* pick up minargs */
		    fscanf (infile, "%d", &minargs);
		  else /* pick up maxargs */
		    if (c == 'M' || c == 'U') /* MANY || UNEVALLED */
		      maxargs = -1;
		    else
		      fscanf (infile, "%d", &maxargs);
		}
	    }
	  if (c < 0)
	    goto eof;
	  c = getc (infile);
	}
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);
      if (c == '"')
	c = read_c_string (infile, 0);
      while (c != ',')
	c = getc (infile);
      c = getc (infile);
      while (c == ' ' || c == '\n' || c == '\t')
	c = getc (infile);

      if (c == '"')
	{
	  putc (037, outfile);
	  putc (defvarflag ? 'V' : 'F', outfile);
	  fprintf (outfile, "%s\n", buf);
	  c = read_c_string (infile, 1);

	  /* If this is a defun, find the arguments and print them.  If
	     this function takes MANY or UNEVALLED args, then the C source
	     won't give the names of the arguments, so we shouldn't bother
	     trying to find them.  */
	  if (defunflag && maxargs != -1)
	    {
	      char argbuf[1024], *p = argbuf;
	      while (c != ')')
		{
		  if (c < 0)
		    goto eof;
		  c = getc (infile);
		}
	      /* Skip into arguments.  */
	      while (c != '(')
		{
		  if (c < 0)
		    goto eof;
		  c = getc (infile);
		}
	      /* Copy arguments into ARGBUF.  */
	      *p++ = c;
	      do
		*p++ = c = getc (infile);
	      while (c != ')');
	      *p = '\0';
	      /* Output them.  */
	      fprintf (outfile, "\n\n");
	      write_c_args (outfile, buf, argbuf, minargs, maxargs);
	    }
	}
    }
 eof:
  fclose (infile);
  return 0;
}

/* Read a file of Lisp code, compiled or interpreted.
 Looks for
  (defun NAME ARGS DOCSTRING ...)
  (defmacro NAME ARGS DOCSTRING ...)
  (autoload (quote NAME) FILE DOCSTRING ...)
  (defvar NAME VALUE DOCSTRING)
  (defconst NAME VALUE DOCSTRING)
  (fset (quote NAME) (make-byte-code ... DOCSTRING ...))
  (fset (quote NAME) #[... DOCSTRING ...])
  (defalias (quote NAME) #[... DOCSTRING ...])
 starting in column zero.
 (quote NAME) may appear as 'NAME as well.

 We also look for #@LENGTH CONTENTS^_ at the beginning of the line.
 When we find that, we save it for the following defining-form,
 and we use that instead of reading a doc string within that defining-form.

 For defun, defmacro, and autoload, we know how to skip over the arglist.
 For defvar, defconst, and fset we skip to the docstring with a kludgy 
 formatting convention: all docstrings must appear on the same line as the
 initial open-paren (the one in column zero) and must contain a backslash 
 and a double-quote immediately after the initial double-quote.  No newlines
 must appear between the beginning of the form and the first double-quote.
 The only source file that must follow this convention is loaddefs.el; aside
 from that, it is always the .elc file that we look at, and they are no
 problem because byte-compiler output follows this convention.
 The NAME and DOCSTRING are output.
 NAME is preceded by `F' for a function or `V' for a variable.
 An entry is output only if DOCSTRING has \ newline just after the opening "
 */

void
skip_white (infile)
     FILE *infile;
{
  char c = ' ';
  while (c == ' ' || c == '\t' || c == '\n')
    c = getc (infile);
  ungetc (c, infile);
}

void
read_lisp_symbol (infile, buffer)
     FILE *infile;
     char *buffer;
{
  char c;
  char *fillp = buffer;

  skip_white (infile);
  while (1)
    {
      c = getc (infile);
      if (c == '\\')
	*(++fillp) = getc (infile);
      else if (c == ' ' || c == '\t' || c == '\n' || c == '(' || c == ')')
	{
	  ungetc (c, infile);
	  *fillp = 0;
	  break;
	}
      else
	*fillp++ = c;
    }

  if (! buffer[0])
    fprintf (stderr, "## expected a symbol, got '%c'\n", c);
  
  skip_white (infile);
}

int
scan_lisp_file (filename, mode)
     char *filename, *mode;
{
  FILE *infile;
  register int c;
  char *saved_string = 0;

  infile = fopen (filename, mode);
  if (infile == NULL)
    {
      perror (filename);
      return 0;				/* No error */
    }

  c = '\n';
  while (!feof (infile))
    {
      char buffer[BUFSIZ];
      char type;

      if (c != '\n')
	{
	  c = getc (infile);
	  continue;
	}
      c = getc (infile);
      /* Detect a dynamic doc string and save it for the next expression.  */
      if (c == '#')
	{
	  c = getc (infile);
	  if (c == '@')
	    {
	      int length = 0;
	      int i;

	      /* Read the length.  */
	      while ((c = getc (infile),
		      c >= '0' && c <= '9'))
		{
		  length *= 10;
		  length += c - '0';
		}

	      /* The next character is a space that is counted in the length
		 but not part of the doc string.
		 We already read it, so just ignore it.  */
	      length--;

	      /* Read in the contents.  */
	      if (saved_string != 0)
		free (saved_string);
	      saved_string = (char *) malloc (length);
	      for (i = 0; i < length; i++)
		saved_string[i] = getc (infile);
	      /* The last character is a ^_.
		 That is needed in the .elc file
		 but it is redundant in DOC.  So get rid of it here.  */
	      saved_string[length - 1] = 0;
	      /* Skip the newline.  */
	      c = getc (infile);
	      while (c != '\n')
		c = getc (infile);
	    }
	  continue;
	}

      if (c != '(')
	continue;

      read_lisp_symbol (infile, buffer);

      if (! strcmp (buffer, "defun") ||
	  ! strcmp (buffer, "defmacro"))
	{
	  type = 'F';
	  read_lisp_symbol (infile, buffer);

	  /* Skip the arguments: either "nil" or a list in parens */

	  c = getc (infile);
	  if (c == 'n') /* nil */
	    {
	      if ((c = getc (infile)) != 'i' ||
		  (c = getc (infile)) != 'l')
		{
		  fprintf (stderr, "## unparsable arglist in %s (%s)\n",
			   buffer, filename);
		  continue;
		}
	    }
	  else if (c != '(')
	    {
	      fprintf (stderr, "## unparsable arglist in %s (%s)\n",
		       buffer, filename);
	      continue;
	    }
	  else
	    while (c != ')')
	      c = getc (infile);
	  skip_white (infile);

	  /* If the next three characters aren't `dquote bslash newline'
	     then we're not reading a docstring.
	   */
	  if ((c = getc (infile)) != '"' ||
	      (c = getc (infile)) != '\\' ||
	      (c = getc (infile)) != '\n')
	    {
#ifdef DEBUG
	      fprintf (stderr, "## non-docstring in %s (%s)\n",
		       buffer, filename);
#endif
	      continue;
	    }
	}

      else if (! strcmp (buffer, "defvar") ||
	       ! strcmp (buffer, "defconst"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'V';
	  read_lisp_symbol (infile, buffer);

	  if (saved_string == 0)
	    {

	      /* Skip until the first newline; remember the two previous chars. */
	      while (c != '\n' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc (infile);
		}
	  
	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

      else if (! strcmp (buffer, "fset") || ! strcmp (buffer, "defalias"))
	{
	  char c1 = 0, c2 = 0;
	  type = 'F';

	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr, "## unparsable name in fset in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr, "## unparsable name in fset in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in fset in %s\n",
			   filename);
		  continue;
		}
	    }

	  if (saved_string == 0)
	    {
	      /* Skip until the first newline; remember the two previous chars. */
	      while (c != '\n' && c >= 0)
		{
		  c2 = c1;
		  c1 = c;
		  c = getc (infile);
		}
	  
	      /* If two previous characters were " and \,
		 this is a doc string.  Otherwise, there is none.  */
	      if (c2 != '"' || c1 != '\\')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

      else if (! strcmp (buffer, "autoload"))
	{
	  type = 'F';
	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr, "## unparsable name in autoload in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr, "## unparsable name in autoload in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in autoload in %s\n",
			   filename);
		  continue;
		}
	    }
	  skip_white (infile);
	  if ((c = getc (infile)) != '\"')
	    {
	      fprintf (stderr, "## autoload of %s unparsable (%s)\n",
		       buffer, filename);
	      continue;
	    }
	  read_c_string (infile, 0);
	  skip_white (infile);

	  if (saved_string == 0)
	    {
	      /* If the next three characters aren't `dquote bslash newline'
		 then we're not reading a docstring.  */
	      if ((c = getc (infile)) != '"' ||
		  (c = getc (infile)) != '\\' ||
		  (c = getc (infile)) != '\n')
		{
#ifdef DEBUG
		  fprintf (stderr, "## non-docstring in %s (%s)\n",
			   buffer, filename);
#endif
		  continue;
		}
	    }
	}

#ifdef DEBUG
      else if (! strcmp (buffer, "if") ||
	       ! strcmp (buffer, "byte-code"))
	;
#endif

      else
	{
#ifdef DEBUG
	  fprintf (stderr, "## unrecognised top-level form, %s (%s)\n",
		   buffer, filename);
#endif
	  continue;
	}

      /* At this point, we should either use the previous
	 dynamic doc string in saved_string
	 or gobble a doc string from the input file.

	 In the latter case, the opening quote (and leading
	 backslash-newline) have already been read.  */

      putc (037, outfile);
      putc (type, outfile);
      fprintf (outfile, "%s\n", buffer);
      if (saved_string)
	{
	  fputs (saved_string, outfile);
	  /* Don't use one dynamic doc string twice.  */
	  free (saved_string);
	  saved_string = 0;
	}
      else
	read_c_string (infile, 1);
    }
  fclose (infile);
  return 0;
}
