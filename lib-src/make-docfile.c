/* Generate doc-string file for GNU Emacs from source files.

Copyright (C) 1985-1986, 1992-1994, 1997, 1999-2015 Free Software
Foundation, Inc.

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


/* The arguments given to this program are all the C and Lisp source files
 of GNU Emacs.  .elc and .el and .c files are allowed.
 A .o file can also be specified; the .c file it was made from is used.
 This helps the makefile pass the correct list of files.
 Option -d DIR means change to DIR before looking for files.

 The results, which go to standard output or to a file
 specified with -a or -o (-a to append, -o to start from nothing),
 are entries containing function or variable names and their documentation.
 Each entry starts with a ^_ character.
 Then comes F for a function or V for a variable.
 Then comes the function or variable name, terminated with a newline.
 Then comes the documentation for that function or variable.
 */

#include <config.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>   /* config.h unconditionally includes this anyway */

#ifdef WINDOWSNT
/* Defined to be sys_fopen in ms-w32.h, but only #ifdef emacs, so this
   is really just insurance.  */
#undef fopen
#include <direct.h>
#endif /* WINDOWSNT */

#include <binary-io.h>

#ifdef DOS_NT
/* Defined to be sys_chdir in ms-w32.h, but only #ifdef emacs, so this
   is really just insurance.

   Similarly, msdos defines this as sys_chdir, but we're not linking with the
   file where that function is defined.  */
#undef chdir
#define IS_SLASH(c)  ((c) == '/' || (c) == '\\' || (c) == ':')
#else  /* not DOS_NT */
#define IS_SLASH(c)  ((c) == '/')
#endif /* not DOS_NT */

static int scan_file (char *filename);
static int scan_lisp_file (const char *filename, const char *mode);
static int scan_c_file (char *filename, const char *mode);
static int scan_c_stream (FILE *infile);
static void start_globals (void);
static void write_globals (void);

#include <unistd.h>

/* Name this program was invoked with.  */
char *progname;

/* Nonzero if this invocation is generating globals.h.  */
int generate_globals;

/* Print error message.  `s1' is printf control string, `s2' is arg for it.  */

/* VARARGS1 */
static void
error (const char *s1, const char *s2)
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Print error message and exit.  */

/* VARARGS1 */
static _Noreturn void
fatal (const char *s1, const char *s2)
{
  error (s1, s2);
  exit (EXIT_FAILURE);
}

/* Like malloc but get fatal error if memory is exhausted.  */

static void *
xmalloc (unsigned int size)
{
  void *result = (void *) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* Like strdup, but get fatal error if memory is exhausted.  */

static char *
xstrdup (char *s)
{
  char *result = strdup (s);
  if (! result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */

static void *
xrealloc (void *arg, unsigned int size)
{
  void *result = (void *) realloc (arg, size);
  if (result == NULL)
    fatal ("virtual memory exhausted", 0);
  return result;
}


int
main (int argc, char **argv)
{
  int i;
  int err_count = 0;

  progname = argv[0];

  /* If first two args are -o FILE, output to FILE.  */
  i = 1;
  if (argc > i + 1 && !strcmp (argv[i], "-o"))
    {
      if (! freopen (argv[i + 1], "w", stdout))
	{
	  perror (argv[i + 1]);
	  return EXIT_FAILURE;
	}
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-a"))
    {
      if (! freopen (argv[i + 1], "a", stdout))
	{
	  perror (argv[i + 1]);
	  return EXIT_FAILURE;
	}
      i += 2;
    }
  if (argc > i + 1 && !strcmp (argv[i], "-d"))
    {
      if (chdir (argv[i + 1]) != 0)
	{
	  perror (argv[i + 1]);
	  return EXIT_FAILURE;
	}
      i += 2;
    }
  if (argc > i && !strcmp (argv[i], "-g"))
    {
      generate_globals = 1;
      ++i;
    }

  set_binary_mode (fileno (stdout), O_BINARY);

  if (generate_globals)
    start_globals ();

  if (argc <= i)
    scan_c_stream (stdin);
  else
    {
      int first_infile = i;
      for (; i < argc; i++)
	{
	  int j;
	  /* Don't process one file twice.  */
	  for (j = first_infile; j < i; j++)
	    if (strcmp (argv[i], argv[j]) == 0)
	      break;
	  if (j == i)
	    err_count += scan_file (argv[i]);
	}
    }

  if (err_count == 0 && generate_globals)
    write_globals ();

  return (err_count > 0 ? EXIT_FAILURE : EXIT_SUCCESS);
}

/* Add a source file name boundary marker in the output file.  */
static void
put_filename (char *filename)
{
  char *tmp;

  for (tmp = filename; *tmp; tmp++)
    {
      if (IS_DIRECTORY_SEP (*tmp))
	filename = tmp + 1;
    }

  printf ("\037S%s\n", filename);
}

/* Read file FILENAME and output its doc strings to stdout.
   Return 1 if file is not found, 0 if it is found.  */

static int
scan_file (char *filename)
{

  size_t len = strlen (filename);

  if (!generate_globals)
    put_filename (filename);
  if (len > 4 && !strcmp (filename + len - 4, ".elc"))
    return scan_lisp_file (filename, "rb");
  else if (len > 3 && !strcmp (filename + len - 3, ".el"))
    return scan_lisp_file (filename, "r");
  else
    return scan_c_file (filename, "r");
}

static void
start_globals (void)
{
  puts ("/* This file was auto-generated by make-docfile.  */");
  puts ("/* DO NOT EDIT.  */");
  puts ("struct emacs_globals {");
}

static char input_buffer[128];

/* Some state during the execution of `read_c_string_or_comment'.  */
struct rcsoc_state
{
  /* A count of spaces and newlines that have been read, but not output.  */
  unsigned pending_spaces, pending_newlines;

  /* Where we're reading from.  */
  FILE *in_file;

  /* If non-zero, a buffer into which to copy characters.  */
  char *buf_ptr;
  /* If non-zero, a file into which to copy characters.  */
  FILE *out_file;

  /* A keyword we look for at the beginning of lines.  If found, it is
     not copied, and SAW_KEYWORD is set to true.  */
  const char *keyword;
  /* The current point we've reached in an occurrence of KEYWORD in
     the input stream.  */
  const char *cur_keyword_ptr;
  /* Set to true if we saw an occurrence of KEYWORD.  */
  int saw_keyword;
};

/* Output CH to the file or buffer in STATE.  Any pending newlines or
   spaces are output first.  */

static void
put_char (int ch, struct rcsoc_state *state)
{
  int out_ch;
  do
    {
      if (state->pending_newlines > 0)
	{
	  state->pending_newlines--;
	  out_ch = '\n';
	}
      else if (state->pending_spaces > 0)
	{
	  state->pending_spaces--;
	  out_ch = ' ';
	}
      else
	out_ch = ch;

      if (state->out_file)
	putc (out_ch, state->out_file);
      if (state->buf_ptr)
	*state->buf_ptr++ = out_ch;
    }
  while (out_ch != ch);
}

/* If in the middle of scanning a keyword, continue scanning with
   character CH, otherwise output CH to the file or buffer in STATE.
   Any pending newlines or spaces are output first, as well as any
   previously scanned characters that were thought to be part of a
   keyword, but were in fact not.  */

static void
scan_keyword_or_put_char (int ch, struct rcsoc_state *state)
{
  if (state->keyword
      && *state->cur_keyword_ptr == ch
      && (state->cur_keyword_ptr > state->keyword
	  || state->pending_newlines > 0))
    /* We might be looking at STATE->keyword at some point.
       Keep looking until we know for sure.  */
    {
      if (*++state->cur_keyword_ptr == '\0')
	/* Saw the whole keyword.  Set SAW_KEYWORD flag to true.  */
	{
	  state->saw_keyword = 1;

	  /* Reset the scanning pointer.  */
	  state->cur_keyword_ptr = state->keyword;

	  /* Canonicalize whitespace preceding a usage string.  */
	  state->pending_newlines = 2;
	  state->pending_spaces = 0;

	  /* Skip any whitespace between the keyword and the
	     usage string.  */
	  do
	    ch = getc (state->in_file);
	  while (ch == ' ' || ch == '\n');

	  /* Output the open-paren we just read.  */
	  put_char (ch, state);

	  /* Skip the function name and replace it with `fn'.  */
	  do
	    ch = getc (state->in_file);
	  while (ch != ' ' && ch != ')');
	  put_char ('f', state);
	  put_char ('n', state);

	  /* Put back the last character.  */
	  ungetc (ch, state->in_file);
	}
    }
  else
    {
      if (state->keyword && state->cur_keyword_ptr > state->keyword)
	/* We scanned the beginning of a potential usage
	   keyword, but it was a false alarm.  Output the
	   part we scanned.  */
	{
	  const char *p;

	  for (p = state->keyword; p < state->cur_keyword_ptr; p++)
	    put_char (*p, state);

	  state->cur_keyword_ptr = state->keyword;
	}

      put_char (ch, state);
    }
}


/* Skip a C string or C-style comment from INFILE, and return the
   character that follows.  COMMENT non-zero means skip a comment.  If
   PRINTFLAG is positive, output string contents to stdout.  If it is
   negative, store contents in buf.  Convert escape sequences \n and
   \t to newline and tab; discard \ followed by newline.
   If SAW_USAGE is non-zero, then any occurrences of the string `usage:'
   at the beginning of a line will be removed, and *SAW_USAGE set to
   true if any were encountered.  */

static int
read_c_string_or_comment (FILE *infile, int printflag, int comment, int *saw_usage)
{
  register int c;
  struct rcsoc_state state;

  state.in_file = infile;
  state.buf_ptr = (printflag < 0 ? input_buffer : 0);
  state.out_file = (printflag > 0 ? stdout : 0);
  state.pending_spaces = 0;
  state.pending_newlines = 0;
  state.keyword = (saw_usage ? "usage:" : 0);
  state.cur_keyword_ptr = state.keyword;
  state.saw_keyword = 0;

  c = getc (infile);
  if (comment)
    while (c == '\n' || c == '\r' || c == '\t' || c == ' ')
      c = getc (infile);

  while (c != EOF)
    {
      while (c != EOF && (comment ? c != '*' : c != '"'))
	{
	  if (c == '\\')
	    {
	      c = getc (infile);
	      if (c == '\n' || c == '\r')
		{
		  c = getc (infile);
		  continue;
		}
	      if (c == 'n')
		c = '\n';
	      if (c == 't')
		c = '\t';
	    }

	  if (c == ' ')
	    state.pending_spaces++;
	  else if (c == '\n')
	    {
	      state.pending_newlines++;
	      state.pending_spaces = 0;
	    }
	  else
	    scan_keyword_or_put_char (c, &state);

	  c = getc (infile);
	}

      if (c != EOF)
	c = getc (infile);

      if (comment)
	{
	  if (c == '/')
	    {
	      c = getc (infile);
	      break;
	    }

	  scan_keyword_or_put_char ('*', &state);
	}
      else
	{
	  if (c != '"')
	    break;

	  /* If we had a "", concatenate the two strings.  */
	  c = getc (infile);
	}
    }

  if (printflag < 0)
    *state.buf_ptr = 0;

  if (saw_usage)
    *saw_usage = state.saw_keyword;

  return c;
}



/* Write to stdout the argument names of function FUNC, whose text is in BUF.
   MINARGS and MAXARGS are the minimum and maximum number of arguments.  */

static void
write_c_args (char *func, char *buf, int minargs, int maxargs)
{
  register char *p;
  int in_ident = 0;
  char *ident_start IF_LINT (= NULL);
  size_t ident_length = 0;

  fputs ("(fn", stdout);

  if (*buf == '(')
    ++buf;

  for (p = buf; *p; p++)
    {
      char c = *p;

      /* Notice when a new identifier starts.  */
      if ((('A' <= c && c <= 'Z')
	   || ('a' <= c && c <= 'z')
	   || ('0' <= c && c <= '9')
	   || c == '_')
	  != in_ident)
	{
	  if (!in_ident)
	    {
	      in_ident = 1;
	      ident_start = p;
	    }
	  else
	    {
	      in_ident = 0;
	      ident_length = p - ident_start;
	    }
	}

      /* Found the end of an argument, write out the last seen
	 identifier.  */
      if (c == ',' || c == ')')
	{
	  if (ident_length == 0)
	    {
	      error ("empty arg list for '%s' should be (void), not ()", func);
	      continue;
	    }

	  if (strncmp (ident_start, "void", ident_length) == 0)
	    continue;

	  putchar (' ');

	  if (minargs == 0 && maxargs > 0)
	    fputs ("&optional ", stdout);

	  minargs--;
	  maxargs--;

	  /* In C code, `default' is a reserved word, so we spell it
	     `defalt'; demangle that here.  */
	  if (ident_length == 6 && memcmp (ident_start, "defalt", 6) == 0)
	    fputs ("DEFAULT", stdout);
	  else
	    while (ident_length-- > 0)
	      {
		c = *ident_start++;
		if (c >= 'a' && c <= 'z')
		  /* Upcase the letter.  */
		  c += 'A' - 'a';
		else if (c == '_')
		  /* Print underscore as hyphen.  */
		  c = '-';
		putchar (c);
	      }
	}
    }

  putchar (')');
}

/* The types of globals.  These are sorted roughly in decreasing alignment
   order to avoid allocation gaps, except that symbols and functions
   are last.  */
enum global_type
{
  INVALID,
  LISP_OBJECT,
  EMACS_INTEGER,
  BOOLEAN,
  SYMBOL,
  FUNCTION
};

/* A single global.  */
struct global
{
  enum global_type type;
  char *name;
  int flags;
  union
  {
    int value;
    char const *svalue;
  } v;
};

/* Bit values for FLAGS field from the above.  Applied for DEFUNs only.  */
enum { DEFUN_noreturn = 1, DEFUN_const = 2 };

/* All the variable names we saw while scanning C sources in `-g'
   mode.  */
int num_globals;
int num_globals_allocated;
struct global *globals;

static struct global *
add_global (enum global_type type, char *name, int value, char const *svalue)
{
  /* Ignore the one non-symbol that can occur.  */
  if (strcmp (name, "..."))
    {
      ++num_globals;

      if (num_globals_allocated == 0)
	{
	  num_globals_allocated = 100;
	  globals = xmalloc (num_globals_allocated * sizeof (struct global));
	}
      else if (num_globals == num_globals_allocated)
	{
	  num_globals_allocated *= 2;
	  globals = xrealloc (globals,
			      num_globals_allocated * sizeof (struct global));
	}

      globals[num_globals - 1].type = type;
      globals[num_globals - 1].name = name;
      if (svalue)
	globals[num_globals - 1].v.svalue = svalue;
      else
	globals[num_globals - 1].v.value = value;
      globals[num_globals - 1].flags = 0;
      return globals + num_globals - 1;
    }
  return NULL;
}

static int
compare_globals (const void *a, const void *b)
{
  const struct global *ga = a;
  const struct global *gb = b;

  if (ga->type != gb->type)
    return ga->type - gb->type;

  /* Consider "nil" to be the least, so that iQnil is zero.  That
     way, Qnil's internal representation is zero, which is a bit faster.  */
  if (ga->type == SYMBOL)
    {
      bool a_nil = strcmp (ga->name, "Qnil") == 0;
      bool b_nil = strcmp (gb->name, "Qnil") == 0;
      if (a_nil | b_nil)
	return b_nil - a_nil;
    }

  return strcmp (ga->name, gb->name);
}

static void
close_emacs_globals (int num_symbols)
{
  printf (("};\n"
	   "extern struct emacs_globals globals;\n"
	   "\n"
	   "#ifndef DEFINE_SYMBOLS\n"
	   "extern\n"
	   "#endif\n"
	   "struct Lisp_Symbol alignas (GCALIGNMENT) lispsym[%d];\n"),
	  num_symbols);
}

static void
write_globals (void)
{
  int i, j;
  bool seen_defun = false;
  int symnum = 0;
  int num_symbols = 0;
  qsort (globals, num_globals, sizeof (struct global), compare_globals);

  j = 0;
  for (i = 0; i < num_globals; i++)
    {
      while (i + 1 < num_globals
	     && strcmp (globals[i].name, globals[i + 1].name) == 0)
	{
	  if (globals[i].type == FUNCTION
	      && globals[i].v.value != globals[i + 1].v.value)
	    error ("function '%s' defined twice with differing signatures",
		   globals[i].name);
	  i++;
	}
      num_symbols += globals[i].type == SYMBOL;
      globals[j++] = globals[i];
    }
  num_globals = j;

  for (i = 0; i < num_globals; ++i)
    {
      char const *type = 0;

      switch (globals[i].type)
	{
	case EMACS_INTEGER:
	  type = "EMACS_INT";
	  break;
	case BOOLEAN:
	  type = "bool";
	  break;
	case LISP_OBJECT:
	  type = "Lisp_Object";
	  break;
	case SYMBOL:
	case FUNCTION:
	  if (!seen_defun)
	    {
	      close_emacs_globals (num_symbols);
	      putchar ('\n');
	      seen_defun = true;
	    }
	  break;
	default:
	  fatal ("not a recognized DEFVAR_", 0);
	}

      if (type)
	{
	  printf ("  %s f_%s;\n", type, globals[i].name);
	  printf ("#define %s globals.f_%s\n",
		  globals[i].name, globals[i].name);
	}
      else if (globals[i].type == SYMBOL)
	printf (("#define i%s %d\n"
		 "DEFINE_LISP_SYMBOL (%s)\n"),
		globals[i].name, symnum++, globals[i].name);
      else
	{
	  if (globals[i].flags & DEFUN_noreturn)
	    fputs ("_Noreturn ", stdout);

	  printf ("EXFUN (%s, ", globals[i].name);
	  if (globals[i].v.value == -1)
	    fputs ("MANY", stdout);
	  else if (globals[i].v.value == -2)
	    fputs ("UNEVALLED", stdout);
	  else
	    printf ("%d", globals[i].v.value);
	  putchar (')');

	  if (globals[i].flags & DEFUN_const)
	    fputs (" ATTRIBUTE_CONST", stdout);

	  puts (";");
	}
    }

  if (!seen_defun)
    close_emacs_globals (num_symbols);

  puts ("#ifdef DEFINE_SYMBOLS");
  puts ("static char const *const defsym_name[] = {");
  for (int i = 0; i < num_globals; i++)
    if (globals[i].type == SYMBOL)
      printf ("\t\"%s\",\n", globals[i].v.svalue);
  puts ("};");
  puts ("#endif");

  puts ("#define Qnil builtin_lisp_symbol (0)");
  puts ("#if DEFINE_NON_NIL_Q_SYMBOL_MACROS");
  num_symbols = 0;
  for (int i = 0; i < num_globals; i++)
    if (globals[i].type == SYMBOL && num_symbols++ != 0)
      printf ("# define %s builtin_lisp_symbol (%d)\n",
	      globals[i].name, num_symbols - 1);
  puts ("#endif");
}


/* Read through a c file.  If a .o file is named,
   the corresponding .c or .m file is read instead.
   Looks for DEFUN constructs such as are defined in ../src/lisp.h.
   Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED.  */

static int
scan_c_file (char *filename, const char *mode)
{
  FILE *infile;
  int extension = filename[strlen (filename) - 1];

  if (extension == 'o')
    filename[strlen (filename) - 1] = 'c';

  infile = fopen (filename, mode);

  if (infile == NULL && extension == 'o')
    {
      /* Try .m.  */
      filename[strlen (filename) - 1] = 'm';
      infile = fopen (filename, mode);
      if (infile == NULL)
        filename[strlen (filename) - 1] = 'c'; /* Don't confuse people.  */
    }

  /* No error if non-ex input file.  */
  if (infile == NULL)
    {
      perror (filename);
      return 0;
    }

  /* Reset extension to be able to detect duplicate files.  */
  filename[strlen (filename) - 1] = extension;
  return scan_c_stream (infile);
}

/* Return 1 if next input from INFILE is equal to P, -1 if EOF,
   0 if input doesn't match.  */

static int
stream_match (FILE *infile, const char *p)
{
  for (; *p; p++)
    {
      int c = getc (infile);
      if (c == EOF)
       return -1;
      if (c != *p)
       return 0;
    }
  return 1;
}

static int
scan_c_stream (FILE *infile)
{
  int commas, minargs, maxargs;
  int c = '\n';

  while (!feof (infile))
    {
      int doc_keyword = 0;
      int defunflag = 0;
      int defvarperbufferflag = 0;
      int defvarflag = 0;
      enum global_type type = INVALID;
      char *name IF_LINT (= 0);

      if (c != '\n' && c != '\r')
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
	  if (c == 'S')
	    {
	      c = getc (infile);
	      if (c != 'Y')
		continue;
	      c = getc (infile);
	      if (c != 'M')
		continue;
	      c = getc (infile);
	      if (c != ' ' && c != '\t' && c != '(')
		continue;
	      type = SYMBOL;
	    }
	  else if (c == 'V')
	    {
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

	      c = getc (infile);
	      defvarperbufferflag = (c == 'P');
	      if (generate_globals)
		{
		  if (c == 'I')
		    type = EMACS_INTEGER;
		  else if (c == 'L')
		    type = LISP_OBJECT;
		  else if (c == 'B')
		    type = BOOLEAN;
		}

	      c = getc (infile);
	      /* We need to distinguish between DEFVAR_BOOL and
		 DEFVAR_BUFFER_DEFAULTS.  */
	      if (generate_globals && type == BOOLEAN && c != 'O')
		type = INVALID;
	    }
	  else
	    continue;
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
	}
      else continue;

      if (generate_globals
	  && (!defvarflag || defvarperbufferflag || type == INVALID)
	  && !defunflag && type != SYMBOL)
	continue;

      while (c != '(')
	{
	  if (c < 0)
	    goto eof;
	  c = getc (infile);
	}

      if (type != SYMBOL)
	{
	  /* Lisp variable or function name.  */
	  c = getc (infile);
	  if (c != '"')
	    continue;
	  c = read_c_string_or_comment (infile, -1, 0, 0);
	}

      if (generate_globals)
	{
	  int i = 0;
	  char const *svalue = 0;

	  /* Skip "," and whitespace.  */
	  do
	    {
	      c = getc (infile);
	    }
	  while (c == ',' || c == ' ' || c == '\t' || c == '\n' || c == '\r');

	  /* Read in the identifier.  */
	  do
	    {
	      if (c < 0)
		goto eof;
	      input_buffer[i++] = c;
	      c = getc (infile);
	    }
	  while (! (c == ',' || c == ' ' || c == '\t'
		    || c == '\n' || c == '\r'));
	  input_buffer[i] = '\0';

	  name = xmalloc (i + 1);
	  memcpy (name, input_buffer, i + 1);

	  if (type == SYMBOL)
	    {
	      do
		c = getc (infile);
	      while (c == ' ' || c == '\t' || c == '\n' || c == '\r');
	      if (c != '"')
		continue;
	      c = read_c_string_or_comment (infile, -1, 0, 0);
	      svalue = xstrdup (input_buffer);
	    }

	  if (!defunflag)
	    {
	      add_global (type, name, 0, svalue);
	      continue;
	    }
	}

      if (type == SYMBOL)
	continue;

      /* DEFVAR_LISP ("name", addr, "doc")
	 DEFVAR_LISP ("name", addr /\* doc *\/)
	 DEFVAR_LISP ("name", addr, doc: /\* doc *\/)  */

      if (defunflag)
	commas = generate_globals ? 4 : 5;
      else if (defvarperbufferflag)
	commas = 3;
      else if (defvarflag)
	commas = 1;
      else  /* For DEFSIMPLE and DEFPRED.  */
	commas = 2;

      while (commas)
	{
	  if (c == ',')
	    {
	      commas--;

	      if (defunflag && (commas == 1 || commas == 2))
		{
		  int scanned = 0;
		  do
		    c = getc (infile);
		  while (c == ' ' || c == '\n' || c == '\r' || c == '\t');
		  if (c < 0)
		    goto eof;
		  ungetc (c, infile);
		  if (commas == 2) /* Pick up minargs.  */
		    scanned = fscanf (infile, "%d", &minargs);
		  else /* Pick up maxargs.  */
		    if (c == 'M' || c == 'U') /* MANY || UNEVALLED */
		      {
			if (generate_globals)
			  maxargs = (c == 'M') ? -1 : -2;
			else
			  maxargs = -1;
		      }
		    else
		      scanned = fscanf (infile, "%d", &maxargs);
		  if (scanned < 0)
		    goto eof;
		}
	    }

	  if (c == EOF)
	    goto eof;
	  c = getc (infile);
	}

      if (generate_globals)
	{
	  struct global *g = add_global (FUNCTION, name, maxargs, 0);

	  /* The following code tries to recognize function attributes
	     specified after the docstring, e.g.:

	     DEFUN ("foo", Ffoo, Sfoo, X, Y, Z,
		   doc: /\* doc *\/
		   attributes: attribute1 attribute2 ...)
	       (Lisp_Object arg...)

	     Now only 'noreturn' and 'const' attributes are used.  */

	  /* Advance to the end of docstring.  */
	  c = getc (infile);
	  if (c == EOF)
	    goto eof;
	  int d = getc (infile);
	  if (d == EOF)
	    goto eof;
	  while (1)
	    {
	      if (c == '*' && d == '/')
		break;
	      c = d, d = getc (infile);
	      if (d == EOF)
		goto eof;
	    }
	  /* Skip spaces, if any.  */
	  do
	    {
	      c = getc (infile);
	      if (c == EOF)
		goto eof;
	    }
	  while (c == ' ' || c == '\n' || c == '\r' || c == '\t');
	  /* Check for 'attributes:' token.  */
	  if (c == 'a' && stream_match (infile, "ttributes:"))
	    {
	      char *p = input_buffer;
	      /* Collect attributes up to ')'.  */
	      while (1)
		{
		  c = getc (infile);
		  if (c == EOF)
		    goto eof;
		  if (c == ')')
		    break;
		  if (p - input_buffer > sizeof (input_buffer))
		    abort ();
		  *p++ = c;
		}
	      *p = 0;
	      if (strstr (input_buffer, "noreturn"))
		g->flags |= DEFUN_noreturn;
	      if (strstr (input_buffer, "const"))
		g->flags |= DEFUN_const;
	    }
	  continue;
	}

      while (c == ' ' || c == '\n' || c == '\r' || c == '\t')
	c = getc (infile);

      if (c == '"')
	c = read_c_string_or_comment (infile, 0, 0, 0);

      while (c != EOF && c != ',' && c != '/')
	c = getc (infile);
      if (c == ',')
	{
	  c = getc (infile);
	  while (c == ' ' || c == '\n' || c == '\r' || c == '\t')
	    c = getc (infile);
	  while ((c >= 'a' && c <= 'z') || (c >= 'Z' && c <= 'Z'))
	    c = getc (infile);
	  if (c == ':')
	    {
	      doc_keyword = 1;
	      c = getc (infile);
	      while (c == ' ' || c == '\n' || c == '\r' || c == '\t')
		c = getc (infile);
	    }
	}

      if (c == '"'
	  || (c == '/'
	      && (c = getc (infile),
		  ungetc (c, infile),
		  c == '*')))
	{
	  int comment = c != '"';
	  int saw_usage;

	  printf ("\037%c%s\n", defvarflag ? 'V' : 'F', input_buffer);

	  if (comment)
	    getc (infile); 	/* Skip past `*'.  */
	  c = read_c_string_or_comment (infile, 1, comment, &saw_usage);

	  /* If this is a defun, find the arguments and print them.  If
	     this function takes MANY or UNEVALLED args, then the C source
	     won't give the names of the arguments, so we shouldn't bother
	     trying to find them.

	     Various doc-string styles:
	      0: DEFUN (..., "DOC") (args)            [!comment]
	      1: DEFUN (..., /\* DOC *\/ (args))      [comment && !doc_keyword]
	      2: DEFUN (..., doc: /\* DOC *\/) (args) [comment && doc_keyword]
	  */
	  if (defunflag && maxargs != -1 && !saw_usage)
	    {
	      char argbuf[1024], *p = argbuf;

	      if (!comment || doc_keyword)
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
	      fputs ("\n\n", stdout);
	      write_c_args (input_buffer, argbuf, minargs, maxargs);
	    }
	  else if (defunflag && maxargs == -1 && !saw_usage)
	    /* The DOC should provide the usage form.  */
	    fprintf (stderr, "Missing 'usage' for function '%s'.\n",
		     input_buffer);
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
  (defsubst NAME ARGS DOCSTRING ...)
  (autoload (quote NAME) FILE DOCSTRING ...)
  (defvar NAME VALUE DOCSTRING)
  (defconst NAME VALUE DOCSTRING)
  (fset (quote NAME) (make-byte-code ... DOCSTRING ...))
  (fset (quote NAME) #[... DOCSTRING ...])
  (defalias (quote NAME) #[... DOCSTRING ...])
  (custom-declare-variable (quote NAME) VALUE DOCSTRING ...)
 starting in column zero.
 (quote NAME) may appear as 'NAME as well.

 We also look for #@LENGTH CONTENTS^_ at the beginning of the line.
 When we find that, we save it for the following defining-form,
 and we use that instead of reading a doc string within that defining-form.

 For defvar, defconst, and fset we skip to the docstring with a kludgy
 formatting convention: all docstrings must appear on the same line as the
 initial open-paren (the one in column zero) and must contain a backslash
 and a newline immediately after the initial double-quote.  No newlines
 must appear between the beginning of the form and the first double-quote.
 For defun, defmacro, and autoload, we know how to skip over the
 arglist, but the doc string must still have a backslash and newline
 immediately after the double quote.
 The only source files that must follow this convention are preloaded
 uncompiled ones like loaddefs.el; aside from that, it is always the .elc
 file that we should look at, and they are no problem because byte-compiler
 output follows this convention.
 The NAME and DOCSTRING are output.
 NAME is preceded by `F' for a function or `V' for a variable.
 An entry is output only if DOCSTRING has \ newline just after the opening ".
 */

static void
skip_white (FILE *infile)
{
  char c = ' ';
  while (c == ' ' || c == '\t' || c == '\n' || c == '\r')
    c = getc (infile);
  ungetc (c, infile);
}

static void
read_lisp_symbol (FILE *infile, char *buffer)
{
  char c;
  char *fillp = buffer;

  skip_white (infile);
  while (1)
    {
      c = getc (infile);
      if (c == '\\')
	*(++fillp) = getc (infile);
      else if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '(' || c == ')')
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

static int
search_lisp_doc_at_eol (FILE *infile)
{
  int c = 0, c1 = 0, c2 = 0;

  /* Skip until the end of line; remember two previous chars.  */
  while (c != '\n' && c != '\r' && c != EOF)
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
      fprintf (stderr, "## non-docstring found\n");
#endif
      if (c != EOF)
	ungetc (c, infile);
      return 0;
    }
  return 1;
}

#define DEF_ELISP_FILE(fn)  { #fn, sizeof(#fn) - 1 }

static int
scan_lisp_file (const char *filename, const char *mode)
{
  FILE *infile;
  register int c;
  char *saved_string = 0;
  /* These are the only files that are loaded uncompiled, and must
     follow the conventions of the doc strings expected by this
     function.  These conventions are automatically followed by the
     byte compiler when it produces the .elc files.  */
  static struct {
    const char *fn;
    size_t fl;
  } const uncompiled[] = {
    DEF_ELISP_FILE (loaddefs.el),
    DEF_ELISP_FILE (loadup.el),
    DEF_ELISP_FILE (charprop.el),
    DEF_ELISP_FILE (cp51932.el),
    DEF_ELISP_FILE (eucjp-ms.el)
  };
  int i, match;
  size_t flen = strlen (filename);

  if (generate_globals)
    fatal ("scanning lisp file when -g specified", 0);
  if (flen > 3 && !strcmp (filename + flen - 3, ".el"))
    {
      for (i = 0, match = 0; i < sizeof (uncompiled) / sizeof (uncompiled[0]);
	   i++)
	{
	  if (uncompiled[i].fl <= flen
	      && !strcmp (filename + flen - uncompiled[i].fl, uncompiled[i].fn)
	      && (flen == uncompiled[i].fl
		  || IS_SLASH (filename[flen - uncompiled[i].fl - 1])))
	    {
	      match = 1;
	      break;
	    }
	}
      if (!match)
	fatal ("uncompiled lisp file %s is not supported", filename);
    }

  infile = fopen (filename, mode);
  if (infile == NULL)
    {
      perror (filename);
      return 0;				/* No error.  */
    }

  c = '\n';
  while (!feof (infile))
    {
      char buffer[BUFSIZ];
      char type;

      /* If not at end of line, skip till we get to one.  */
      if (c != '\n' && c != '\r')
	{
	  c = getc (infile);
	  continue;
	}
      /* Skip the line break.  */
      while (c == '\n' || c == '\r')
	c = getc (infile);
      /* Detect a dynamic doc string and save it for the next expression.  */
      if (c == '#')
	{
	  c = getc (infile);
	  if (c == '@')
	    {
	      size_t length = 0;
	      size_t i;

	      /* Read the length.  */
	      while ((c = getc (infile),
		      c >= '0' && c <= '9'))
		{
		  length *= 10;
		  length += c - '0';
		}

	      if (length <= 1)
		fatal ("invalid dynamic doc string length", "");

	      if (c != ' ')
		fatal ("space not found after dynamic doc string length", "");

	      /* The next character is a space that is counted in the length
		 but not part of the doc string.
		 We already read it, so just ignore it.  */
	      length--;

	      /* Read in the contents.  */
	      free (saved_string);
	      saved_string = (char *) xmalloc (length);
	      for (i = 0; i < length; i++)
		saved_string[i] = getc (infile);
	      /* The last character is a ^_.
		 That is needed in the .elc file
		 but it is redundant in DOC.  So get rid of it here.  */
	      saved_string[length - 1] = 0;
	      /* Skip the line break.  */
	      while (c == '\n' || c == '\r')
		c = getc (infile);
	      /* Skip the following line.  */
	      while (c != '\n' && c != '\r')
		c = getc (infile);
	    }
	  continue;
	}

      if (c != '(')
	continue;

      read_lisp_symbol (infile, buffer);

      if (! strcmp (buffer, "defun")
	  || ! strcmp (buffer, "defmacro")
	  || ! strcmp (buffer, "defsubst"))
	{
	  type = 'F';
	  read_lisp_symbol (infile, buffer);

	  /* Skip the arguments: either "nil" or a list in parens.  */

	  c = getc (infile);
	  if (c == 'n') /* nil */
	    {
	      if ((c = getc (infile)) != 'i'
		  || (c = getc (infile)) != 'l')
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
	  if ((c = getc (infile)) != '"'
	      || (c = getc (infile)) != '\\'
	      || ((c = getc (infile)) != '\n' && c != '\r'))
	    {
#ifdef DEBUG
	      fprintf (stderr, "## non-docstring in %s (%s)\n",
		       buffer, filename);
#endif
	      continue;
	    }
	}

      /* defcustom can only occur in uncompiled Lisp files.  */
      else if (! strcmp (buffer, "defvar")
	       || ! strcmp (buffer, "defconst")
	       || ! strcmp (buffer, "defcustom"))
	{
	  type = 'V';
	  read_lisp_symbol (infile, buffer);

	  if (saved_string == 0)
	    if (!search_lisp_doc_at_eol (infile))
	      continue;
	}

      else if (! strcmp (buffer, "custom-declare-variable")
	       || ! strcmp (buffer, "defvaralias")
	       )
	{
	  type = 'V';

	  c = getc (infile);
	  if (c == '\'')
	    read_lisp_symbol (infile, buffer);
	  else
	    {
	      if (c != '(')
		{
		  fprintf (stderr,
			   "## unparsable name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      if (strcmp (buffer, "quote"))
		{
		  fprintf (stderr,
			   "## unparsable name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	      read_lisp_symbol (infile, buffer);
	      c = getc (infile);
	      if (c != ')')
		{
		  fprintf (stderr,
			   "## unparsable quoted name in custom-declare-variable in %s\n",
			   filename);
		  continue;
		}
	    }

	  if (saved_string == 0)
	    if (!search_lisp_doc_at_eol (infile))
	      continue;
	}

      else if (! strcmp (buffer, "fset") || ! strcmp (buffer, "defalias"))
	{
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
	    if (!search_lisp_doc_at_eol (infile))
	      continue;
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
	  read_c_string_or_comment (infile, 0, 0, 0);

	  if (saved_string == 0)
	    if (!search_lisp_doc_at_eol (infile))
	      continue;
	}

#ifdef DEBUG
      else if (! strcmp (buffer, "if")
	       || ! strcmp (buffer, "byte-code"))
	continue;
#endif

      else
	{
#ifdef DEBUG
	  fprintf (stderr, "## unrecognized top-level form, %s (%s)\n",
		   buffer, filename);
#endif
	  continue;
	}

      /* At this point, we should either use the previous dynamic doc string in
	 saved_string or gobble a doc string from the input file.
	 In the latter case, the opening quote (and leading backslash-newline)
	 have already been read.  */

      printf ("\037%c%s\n", type, buffer);
      if (saved_string)
	{
	  fputs (saved_string, stdout);
	  /* Don't use one dynamic doc string twice.  */
	  free (saved_string);
	  saved_string = 0;
	}
      else
	read_c_string_or_comment (infile, 1, 0, 0);
    }
  fclose (infile);
  return 0;
}


/* make-docfile.c ends here */
