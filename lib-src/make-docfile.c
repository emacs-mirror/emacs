/* Generate doc-string file for GNU Emacs from source files.

Copyright (C) 1985-1986, 1992-1994, 1997, 1999-2023 Free Software
Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


/* The arguments given to this program are all the C files
 of GNU Emacs.  .c files are allowed.
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

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <attribute.h>
#include <binary-io.h>
#include <c-ctype.h>
#include <intprops.h>
#include <min-max.h>
#include <unlocked-io.h>

#ifdef WINDOWSNT
/* Defined to be sys_fopen in ms-w32.h, but only #ifdef emacs, so this
   is really just insurance.  */
#undef fopen
#include <direct.h>
#endif /* WINDOWSNT */

#ifdef DOS_NT
/* Defined to be sys_chdir in ms-w32.h, but only #ifdef emacs, so this
   is really just insurance.

   Similarly, msdos defines this as sys_chdir, but we're not linking with the
   file where that function is defined.  */
#undef chdir
#endif /* not DOS_NT */

static void scan_file (char *filename);
static void scan_c_file (char *filename, const char *mode);
static void scan_c_stream (FILE *infile);
static void start_globals (void);
static void write_globals (void);

#include <unistd.h>

/* Name this program was invoked with.  */
static char *progname;

/* True if this invocation is generating globals.h.  */
static bool generate_globals;

/* Print error message.  Args are like vprintf.  */

static void ATTRIBUTE_FORMAT_PRINTF (1, 0)
verror (char const *m, va_list ap)
{
  fprintf (stderr, "%s: ", progname);
  vfprintf (stderr, m, ap);
  fprintf (stderr, "\n");
}

/* Print error message.  Args are like printf.  */

static void ATTRIBUTE_FORMAT_PRINTF (1, 2)
error (char const *m, ...)
{
  va_list ap;
  va_start (ap, m);
  verror (m, ap);
  va_end (ap);
}

/* Print error message and exit.  Args are like printf.  */

static _Noreturn void ATTRIBUTE_FORMAT_PRINTF (1, 2)
fatal (char const *m, ...)
{
  va_list ap;
  va_start (ap, m);
  verror (m, ap);
  va_end (ap);
  exit (EXIT_FAILURE);
}

static _Noreturn void
memory_exhausted (void)
{
  fatal ("virtual memory exhausted");
}

/* Like malloc but get fatal error if memory is exhausted.  */

static void * ATTRIBUTE_MALLOC
xmalloc (ptrdiff_t size)
{
  void *result = malloc (size);
  if (result == NULL)
    memory_exhausted ();
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */

static void *
xrealloc (void *arg, ptrdiff_t size)
{
  void *result = realloc (arg, size);
  if (result == NULL)
    memory_exhausted ();
  return result;
}


int
main (int argc, char **argv)
{
  int i;

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
      generate_globals = true;
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
	    scan_file (argv[i]);
	}
    }

  if (generate_globals)
    write_globals ();

  if (ferror (stdout) || fclose (stdout) != 0)
    fatal ("write error");

  return EXIT_SUCCESS;
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
   Return true if file is found, false otherwise.  */

static void
scan_file (char *filename)
{
  if (!generate_globals)
    put_filename (filename);
  scan_c_file (filename, "r");
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
  intmax_t pending_spaces, pending_newlines;

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
  bool saw_keyword;
};

/* Output CH to the file or buffer in STATE.  Any pending newlines or
   spaces are output first.  */

static void
put_char (char ch, struct rcsoc_state *state)
{
  char out_ch;
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
scan_keyword_or_put_char (char ch, struct rcsoc_state *state)
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
	  state->saw_keyword = true;

	  /* Reset the scanning pointer.  */
	  state->cur_keyword_ptr = state->keyword;

	  /* Canonicalize whitespace preceding a usage string.  */
	  state->pending_newlines = 2;
	  state->pending_spaces = 0;

	  /* Skip any spaces and newlines between the keyword and the
	     usage string.  */
	  int c;
	  do
	    c = getc (state->in_file);
	  while (c == ' ' || c == '\n');

	  /* Output the open-paren we just read.  */
	  if (c != '(')
	    fatal ("Missing '(' after keyword");
	  put_char (c, state);

	  /* Skip the function name and replace it with `fn'.  */
	  do
	    {
	      c = getc (state->in_file);
	      if (c == EOF)
		fatal ("Unexpected EOF after keyword");
	    }
	  while (c != ' ' && c != ')');

	  put_char ('f', state);
	  put_char ('n', state);

	  /* Put back the last character.  */
	  ungetc (c, state->in_file);
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
   byte that follows, or EOF.  COMMENT means skip a comment.  If
   PRINTFLAG is positive, output string contents to stdout.  If it is
   negative, store contents in buf.  Convert escape sequences \n and
   \t to newline and tab; discard \ followed by newline.
   If SAW_USAGE is non-null, then any occurrences of the string "usage:"
   at the beginning of a line will be removed, and *SAW_USAGE set to
   true if any were encountered.  */

static int
read_c_string_or_comment (FILE *infile, int printflag, bool comment,
			  bool *saw_usage)
{
  int c;
  struct rcsoc_state state;

  state.in_file = infile;
  state.buf_ptr = (printflag < 0 ? input_buffer : 0);
  state.out_file = (printflag > 0 ? stdout : 0);
  state.pending_spaces = 0;
  state.pending_newlines = 0;
  state.keyword = (saw_usage ? "usage:" : 0);
  state.cur_keyword_ptr = state.keyword;
  state.saw_keyword = false;

  c = getc (infile);
  if (comment)
    while (c_isspace (c))
      c = getc (infile);

  while (c != EOF)
    {
      while (c != EOF && (comment ? c != '*' : c != '"'))
	{
	  if (c == '\\')
	    {
	      c = getc (infile);
	      switch (c)
		{
		case '\n': case '\r':
		  c = getc (infile);
		  continue;
		case 'n': c = '\n'; break;
		case 't': c = '\t'; break;
		}
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
  char *p;
  bool in_ident = false;
  char *ident_start UNINIT;
  ptrdiff_t ident_length = 0;

  fputs ("(fn", stdout);

  if (*buf == '(')
    ++buf;

  for (p = buf; *p; p++)
    {
      char c = *p;

      /* Notice when a new identifier starts.  */
      if ((c_isalnum (c) || c == '_')
	  != in_ident)
	{
	  if (!in_ident)
	    {
	      in_ident = true;
	      ident_start = p;
	    }
	  else
	    {
	      in_ident = false;
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
		c = c_toupper (*ident_start++);
		if (c == '_')
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
enum { DEFUN_noreturn = 1, DEFUN_const = 2, DEFUN_noinline = 4 };

/* All the variable names we saw while scanning C sources in `-g'
   mode.  */
static ptrdiff_t num_globals;
static ptrdiff_t num_globals_allocated;
static struct global *globals;

static struct global *
add_global (enum global_type type, char const *name, int value,
	    char const *svalue)
{
  /* Ignore the one non-symbol that can occur.  */
  if (strcmp (name, "..."))
    {
      if (num_globals == num_globals_allocated)
	{
	  ptrdiff_t num_globals_max = (min (PTRDIFF_MAX, SIZE_MAX)
				       / sizeof *globals);
	  if (num_globals_allocated == num_globals_max)
	    memory_exhausted ();
	  if (num_globals_allocated < num_globals_max / 2)
	    num_globals_allocated = 2 * num_globals_allocated + 1;
	  else
	    num_globals_allocated = num_globals_max;
	  globals = xrealloc (globals, num_globals_allocated * sizeof *globals);
	}

      ++num_globals;

      ptrdiff_t namesize = strlen (name) + 1;
      char *buf = xmalloc (namesize + (svalue ? strlen (svalue) + 1 : 0));
      globals[num_globals - 1].type = type;
      globals[num_globals - 1].name = strcpy (buf, name);
      if (svalue)
	globals[num_globals - 1].v.svalue = strcpy (buf + namesize, svalue);
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
     way, Qnil's internal representation is zero, which is a bit faster.
     Similarly, consider "t" to be the second-least, and so forth.  */
  if (ga->type == SYMBOL)
    {
      /* Common symbols in decreasing popularity order.  */
      static char const commonsym[][8]
	= { "nil", "t", "unbound", "error", "lambda" };
      int ncommonsym = sizeof commonsym / sizeof *commonsym;
      int ai = ncommonsym, bi = ncommonsym;
      for (int i = 0; i < ncommonsym; i++)
	{
	  if (ga->name[0] == 'Q' && strcmp (ga->name + 1, commonsym[i]) == 0)
	    ai = i;
	  if (gb->name[0] == 'Q' && strcmp (gb->name + 1, commonsym[i]) == 0)
	    bi = i;
	}
      if (! (ai == ncommonsym && bi == ncommonsym))
	return ai - bi;
    }

  return strcmp (ga->name, gb->name);
}

static void
close_emacs_globals (ptrdiff_t num_symbols)
{
  printf (("};\n"
	   "extern struct emacs_globals globals;\n"
	   "\n"
	   "#ifndef DEFINE_SYMBOLS\n"
	   "extern\n"
	   "#endif\n"
	   "struct Lisp_Symbol lispsym[%td];\n"),
	  num_symbols);
}

static void
write_globals (void)
{
  ptrdiff_t i, j;
  bool seen_defun = false;
  ptrdiff_t symnum = 0;
  ptrdiff_t num_symbols = 0;
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
	  free (globals[i].name);
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
	  type = "intmax_t";
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
	  fatal ("not a recognized DEFVAR_");
	}

      if (type)
	{
	  printf ("  %s f_%s;\n", type, globals[i].name);
	  printf ("#define %s globals.f_%s\n",
		  globals[i].name, globals[i].name);
	}
      else if (globals[i].type == SYMBOL)
	printf (("#define i%s %td\n"
		 "DEFINE_LISP_SYMBOL (%s)\n"),
		globals[i].name, symnum++, globals[i].name);
      else
	{
	  if (globals[i].flags & DEFUN_noreturn)
	    fputs ("_Noreturn ", stdout);
	  if (globals[i].flags & DEFUN_noinline)
	    fputs ("NO_INLINE ", stdout);

	  printf ("EXFUN (%s, ", globals[i].name);
	  if (globals[i].v.value == -1)
	    fputs ("MANY", stdout);
	  else if (globals[i].v.value == -2)
	    fputs ("UNEVALLED", stdout);
	  else
	    printf ("%d", globals[i].v.value);
	  putchar (')');

	  if (globals[i].flags & DEFUN_noreturn)
	    fputs (" ATTRIBUTE_COLD", stdout);
	  if (globals[i].flags & DEFUN_const)
	    fputs (" ATTRIBUTE_CONST", stdout);

	  puts (";");
	}
    }

  if (!seen_defun)
    close_emacs_globals (num_symbols);

  puts ("#ifdef DEFINE_SYMBOLS");
  puts ("static char const *const defsym_name[] = {");
  for (ptrdiff_t i = 0; i < num_globals; i++)
    if (globals[i].type == SYMBOL)
      printf ("\t\"%s\",\n", globals[i].v.svalue);
  puts ("};");
  puts ("#endif");

  puts ("#define Qnil builtin_lisp_symbol (0)");
  puts ("#if DEFINE_NON_NIL_Q_SYMBOL_MACROS");
  num_symbols = 0;
  for (ptrdiff_t i = 0; i < num_globals; i++)
    if (globals[i].type == SYMBOL && num_symbols++ != 0)
      printf ("# define %s builtin_lisp_symbol (%td)\n",
	      globals[i].name, num_symbols - 1);
  puts ("#endif");
}


/* Read through a c file.  If a .o file is named,
   the corresponding .c or .m file is read instead.
   Looks for DEFUN constructs such as are defined in ../src/lisp.h.
   Accepts any word starting DEF... so it finds DEFSIMPLE and DEFPRED.  */

static void
scan_c_file (char *filename, const char *mode)
{
  FILE *infile;
  char extension = filename[strlen (filename) - 1];

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

  if (infile == NULL)
    {
      perror (filename);
      exit (EXIT_FAILURE);
    }

  /* Reset extension to be able to detect duplicate files.  */
  filename[strlen (filename) - 1] = extension;
  scan_c_stream (infile);
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

static void
scan_c_stream (FILE *infile)
{
  int commas, minargs, maxargs;
  int c = '\n';

  while (!feof (infile))
    {
      bool doc_keyword = false;
      bool defunflag = false;
      bool defvarperbufferflag = false;
      bool defvarflag = false;
      enum global_type type = INVALID;
      static char name[sizeof input_buffer];

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

	      defvarflag = true;

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
	  c = read_c_string_or_comment (infile, -1, false, 0);
	}

      if (generate_globals)
	{
	  ptrdiff_t i = 0;
	  char const *svalue = 0;

	  /* Skip "," and whitespace.  */
	  do
	    {
	      c = getc (infile);
	    }
	  while (c == ',' || c_isspace (c));

	  /* Read in the identifier.  */
	  do
	    {
	      if (c < 0)
		goto eof;
	      input_buffer[i++] = c;
	      if (sizeof input_buffer <= i)
		fatal ("identifier too long");
	      c = getc (infile);
	    }
	  while (! (c == ',' || c_isspace (c)));

	  input_buffer[i] = '\0';
	  memcpy (name, input_buffer, i + 1);

	  if (type == SYMBOL)
	    {
	      do
		c = getc (infile);
	      while (c_isspace (c));

	      if (c != '"')
		continue;
	      c = read_c_string_or_comment (infile, -1, false, 0);
	      svalue = input_buffer;
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
		  while (c_isspace (c));

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
	  if (!g)
	    continue;

	  /* The following code tries to recognize function attributes
	     specified after the docstring, e.g.:

	     DEFUN ("foo", Ffoo, Sfoo, X, Y, Z,
		   doc: /\* doc *\/
		   attributes: attribute1 attribute2 ...)
	       (Lisp_Object arg...)

	     Now only `const', `noinline' and `noreturn' attributes
	     are used.  */

	  /* Advance to the end of docstring.  */
	  c = getc (infile);
	  if (c == EOF)
	    goto eof;
	  int d = getc (infile);
	  if (d == EOF)
	    goto eof;
	  while (true)
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
	  while (c_isspace (c));

	  /* Check for 'attributes:' token.  */
	  if (c == 'a' && stream_match (infile, "ttributes:"))
	    {
	      char *p = input_buffer;
	      /* Collect attributes up to ')'.  */
	      while (true)
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

	      /* Although the noinline attribute is no longer used,
		 leave its support in, in case it's needed later.  */
	      if (strstr (input_buffer, "noinline"))
		g->flags |= DEFUN_noinline;
	    }
	  continue;
	}

      while (c_isspace (c))
	c = getc (infile);

      if (c == '"')
	c = read_c_string_or_comment (infile, 0, false, 0);

      while (c != EOF && c != ',' && c != '/')
	c = getc (infile);
      if (c == ',')
	{
	  do
	    c = getc (infile);
	  while (c_isspace (c));

	  while (c_isalpha (c))
	    c = getc (infile);
	  if (c == ':')
	    {
	      doc_keyword = true;
	      do
		c = getc (infile);
	      while (c_isspace (c));
	    }
	}

      if (c == '"'
	  || (c == '/'
	      && (c = getc (infile),
		  ungetc (c, infile),
		  c == '*')))
	{
	  bool comment = c != '"';
	  bool saw_usage;

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
		{
		  c = getc (infile);
		  if (c < 0)
		    goto eof;
		  *p++ = c;
		}
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
  if (ferror (infile) || fclose (infile) != 0)
    fatal ("read error");
}

/* make-docfile.c ends here */
