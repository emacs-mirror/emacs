/* Tags file maker to go with GNU Emacs
   Copyright (C) 1984, 87, 88, 89, 93, 94, 95
   Free Software Foundation, Inc. and Ken Arnold
This file is not considered part of GNU Emacs.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * Authors:
 *	Ctags originally by Ken Arnold.
 *	Fortran added by Jim Kleckner.
 *	Ed Pelegri-Llopart added C typedefs.
 *	Gnu Emacs TAGS format and modifications by RMS?
 *	Sam Kendall added C++.
 *	Francesco Potorti` reorganised C and C++ based on work by Joe Wells.
#ifdef ETAGS_REGEXPS
 *	Regexp tags by Tom Tromey.
#endif
 *
 *	Francesco Potorti` (pot@cnuce.cnr.it) is the current maintainer.
 */

char pot_etags_version[] = "@(#) pot revision number is 11.26";

#define	TRUE	1
#define	FALSE	0
#ifndef DEBUG
# define DEBUG FALSE
#endif

#ifdef MSDOS
#include <fcntl.h>
#include <sys/param.h>
#endif /* MSDOS */

#ifdef WINDOWSNT
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#define MAXPATHLEN _MAX_PATH
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
/* On some systems, Emacs defines static as nothing for the sake
   of unexec.  We don't want that here since we don't use unexec. */
#undef static
#endif

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#ifndef errno
extern int errno;
#endif
#include <sys/types.h>
#include <sys/stat.h>

#if !defined (S_ISREG) && defined (S_IFREG)
# define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#endif

#include <getopt.h>

#ifdef ETAGS_REGEXPS
#include <regex.h>
#endif /* ETAGS_REGEXPS */

/* Define CTAGS to make the program "ctags" compatible with the usual one.
 Let it undefined to make the program "etags", which makes emacs-style
 tag tables and tags typedefs, #defines and struct/union/enum by default. */
#ifdef CTAGS
# undef  CTAGS
# define CTAGS TRUE
#else
# define CTAGS FALSE
#endif

/* Exit codes for success and failure.  */
#ifdef VMS
#define	GOOD	1
#define BAD	0
#else
#define	GOOD	0
#define	BAD	1
#endif

/* C extensions. */
#define C_PLPL	0x00001		/* C++ */
#define C_STAR	0x00003		/* C* */
#define YACC	0x10000		/* yacc file */

#define streq(s,t)	(strcmp (s, t) == 0)
#define strneq(s,t,n)	(strncmp (s, t, n) == 0)

#define	iswhite(arg)	(_wht[arg])	/* T if char is white		*/
#define	begtoken(arg)	(_btk[arg])	/* T if char can start token	*/
#define	intoken(arg)	(_itk[arg])	/* T if char can be in token	*/
#define	endtoken(arg)	(_etk[arg])	/* T if char ends tokens	*/

/*
 *	xnew -- allocate storage
 *
 * SYNOPSIS:	Type *xnew (int n, Type);
 */
#define xnew(n,Type)	((Type *) xmalloc ((n) * sizeof (Type)))

typedef int logical;

typedef struct nd_st
{				/* sorting structure			*/
  char *name;			/* function or type name	*/
  char *file;			/* file name			*/
  logical is_func;		/* use pattern or line no	*/
  logical named;		/* list name separately		*/
  logical been_warned;		/* set if noticed dup		*/
  int lno;			/* line number tag is on	*/
  long cno;			/* character number line starts on */
  char *pat;			/* search pattern		*/
  struct nd_st *left, *right;	/* left and right sons		*/
} NODE;

extern char *getenv ();

char *concat ();
char *savenstr (), *savestr ();
char *etags_strchr (), *etags_strrchr ();
char *etags_getcwd ();
char *relative_filename (), *absolute_filename (), *absolute_dirname ();
long *xmalloc (), *xrealloc ();

typedef void Lang_function ();
#if FALSE				/* many compilers barf on this */
Lang_function Asm_labels;
Lang_function default_C_entries;
Lang_function C_entries;
Lang_function Cplusplus_entries;
Lang_function Cstar_entries;
Lang_function Fortran_functions;
Lang_function Yacc_entries;
Lang_function Lisp_functions;
Lang_function Pascal_functions;
Lang_function Prolog_functions;
Lang_function Scheme_functions;
Lang_function TeX_functions;
Lang_function just_read_file;
#else				/* so let's write it this way */
void Asm_labels ();
void default_C_entries ();
void C_entries ();
void Cplusplus_entries ();
void Cstar_entries ();
void Fortran_functions ();
void Yacc_entries ();
void Lisp_functions ();
void Pascal_functions ();
void Prolog_functions ();
void Scheme_functions ();
void TeX_functions ();
void just_read_file ();
#endif

logical get_language ();
int total_size_of_entries ();
long readline ();
long readline_internal ();
#ifdef ETAGS_REGEXPS
void add_regex ();
#endif
void add_node ();
void error ();
void fatal (), pfatal ();
void find_entries ();
void free_tree ();
void getit ();
void init ();
void initbuffer ();
void pfnote ();
void process_file ();
void put_entries ();
void takeprec ();


char searchar = '/';		/* use /.../ searches */

int lineno;			/* line number of current line */
long charno;			/* current character number */

long linecharno;		/* charno of start of line; not used by C,
				   but by every other language. */

char *curfile;			/* current input file name */
char *tagfile;			/* output file */
char *progname;			/* name this program was invoked with */
char *cwd;			/* current working directory */
char *tagfiledir;		/* directory of tagfile */

FILE *tagf;			/* ioptr for tags file */
NODE *head;			/* the head of the binary tree of tags */

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

struct linebuffer lb;		/* the current line */
struct linebuffer token_name;	/* used by C_entries as temporary area */
struct
{
  long linepos;
  struct linebuffer lb;		/* used by C_entries instead of lb */
} lbs[2];

/* boolean "functions" (see init)	*/
logical _wht[0177], _etk[0177], _itk[0177], _btk[0177];
char
 *white = " \f\t\n\013",	/* white chars				*/
 *endtk = " \t\n\013\"'#()[]{}=-+%*/&|^~!<>;,.:?", /* token ending chars */
				/* token starting chars			*/
 *begtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$~",
				/* valid in-token chars			*/
 *intk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$0123456789";

logical append_to_tagfile;	/* -a: append to tags */
/* The following three default to TRUE for etags, but to FALSE for ctags.  */
logical typedefs;		/* -t: create tags for typedefs */
logical typedefs_and_cplusplus;	/* -T: create tags for typedefs, level */
				/* 0 struct/enum/union decls, and C++ */
				/* member functions. */
logical constantypedefs;	/* -d: create tags for C #define and enum */
				/* constants.  Enum consts not implemented. */
				/* -D: opposite of -d.  Default under ctags. */
logical update;			/* -u: update tags */
logical vgrind_style;		/* -v: create vgrind style index output */
logical no_warnings;		/* -w: suppress warnings */
logical cxref_style;		/* -x: create cxref style output */
logical cplusplus;		/* .[hc] means C++, not C */
logical noindentypedefs;	/* -I: ignore indentation in C */
#define permit_duplicates TRUE	/* allow duplicate tags */

struct option longopts[] =
{
  { "append",			no_argument,	   NULL, 'a' },
  { "backward-search",		no_argument,	   NULL, 'B' }, 
  { "c++",			no_argument,	   NULL, 'C' },
  { "cxref",			no_argument,	   NULL, 'x' },
  { "defines",			no_argument,	   NULL, 'd' },
  { "help",			no_argument,	   NULL, 'h' },
  { "help",			no_argument,	   NULL, 'H' },
  { "ignore-indentation",	no_argument,	   NULL, 'I' },
  { "include",			required_argument, NULL, 'i' },
  { "language",                 required_argument, NULL, 'l' },
  { "no-defines",		no_argument,	   NULL, 'D' },
  { "no-regex",			no_argument,	   NULL, 'R' },
  { "no-warn",			no_argument,	   NULL, 'w' },
  { "output",			required_argument, NULL, 'o' },
  { "regex",			required_argument, NULL, 'r' },
  { "typedefs",			no_argument,	   NULL, 't' },
  { "typedefs-and-c++",		no_argument,	   NULL, 'T' },
  { "update",			no_argument,	   NULL, 'u' }, 
  { "version",			no_argument,	   NULL, 'V' },
  { "vgrind",			no_argument,	   NULL, 'v' }, 
  { 0 }
};

#ifdef ETAGS_REGEXPS
/* Structure defining a regular expression.  Elements are 
   the compiled pattern, and the name string. */
struct pattern
{
  struct re_pattern_buffer *pattern;
  struct re_registers regs;
  char *name_pattern;
  logical error_signaled;
};

/* Number of regexps found. */
int num_patterns = 0;

/* Array of all regexps. */
struct pattern *patterns = NULL;
#endif /* ETAGS_REGEXPS */

/* Language stuff. */
struct lang_entry
{
  char *extension;
  Lang_function *function;
};

/* Table of language names and corresponding functions. */
/* It is ok for a given function to be listed under more than one
   name.  I just didn't. */
/* "auto" language reverts to default behavior. */
struct lang_entry lang_names[] =
{
  { "asm", Asm_labels },
  { "c", default_C_entries },
  { "c++", Cplusplus_entries },
  { "c*", Cstar_entries },
  { "fortran", Fortran_functions },
  { "lisp", Lisp_functions },
  { "none", just_read_file },
  { "pascal", Pascal_functions },
  { "scheme" , Scheme_functions },
  { "tex", TeX_functions },
  { "auto", NULL },
  { NULL, NULL }
};

/* Table of file extensions and corresponding language functions. */
struct lang_entry lang_extensions[] =
{
  /* Assume that ".s" or ".a" is assembly code. -wolfgang.
     Or even ".sa". */
  { "a", Asm_labels },		/* Unix assembler */
  { "asm", Asm_labels },	/* Microcontroller assembly */
  { "def", Asm_labels },	/* BSO/Tasking definition includes  */
  { "inc", Asm_labels },	/* Microcontroller include files */
  { "ins", Asm_labels },	/* Microcontroller include files */
  { "s", Asm_labels },
  { "sa", Asm_labels },		/* Unix assembler */
  { "src", Asm_labels },	/* BSO/Tasking C compiler output */

  /* .aux, .bbl, .clo, .cls, .dtx or .tex implies LaTeX source code. */
  { "aux", TeX_functions },
  { "bbl", TeX_functions },
  { "clo", TeX_functions },
  { "cls", TeX_functions },
  { "dtx", TeX_functions },
  { "sty", TeX_functions },
  { "tex", TeX_functions },

  /* .l or .el or .lisp (or .cl or .clisp or ...) implies lisp source code */
  { "cl", Lisp_functions },
  { "clisp", Lisp_functions },
  { "el", Lisp_functions },
  { "l", Lisp_functions },
  { "lisp", Lisp_functions },
  { "lsp", Lisp_functions },

  /* .scm or .sm or .scheme implies scheme source code */
  { "SCM", Scheme_functions },
  { "SM", Scheme_functions },
  { "oak", Scheme_functions },
  { "sch", Scheme_functions },
  { "scheme", Scheme_functions },
  { "scm", Scheme_functions },
  { "sm", Scheme_functions },
  { "t", Scheme_functions },
  /* FIXME Can't do the `SCM' or `scm' prefix with a version number */

  /* Note that ".c" and ".h" can be considered C++, if the --c++
     flag was given.  That is why default_C_entries is called here. */
  { "c", default_C_entries },
  { "h", default_C_entries },

  /* .C or .H or .c++ or .cc or .cpp or .cxx or .h++ or .hh or .hxx:
     a C++ file */
  { "C", Cplusplus_entries },
  { "H", Cplusplus_entries },
  { "c++", Cplusplus_entries },
  { "cc", Cplusplus_entries },
  { "cpp", Cplusplus_entries },
  { "cxx", Cplusplus_entries },
  { "h++", Cplusplus_entries },
  { "hh", Cplusplus_entries },
  { "hxx", Cplusplus_entries },

  /* .y: a yacc file */
  { "y", Yacc_entries },

  /* .cs or .hs: a C* file */
  { "cs", Cstar_entries },
  { "hs", Cstar_entries },

  /* .f and .for are FORTRAN. */
  { "F", Fortran_functions },
  { "f", Fortran_functions },
  { "for", Fortran_functions },

  /* .pl implies prolog source code */
  { "pl", Prolog_functions },

  /* .p or .pas: a Pascal file */
  { "p", Pascal_functions },
  { "pas", Pascal_functions },

  { NULL, NULL }
};

/* Non-NULL if language fixed. */
Lang_function *lang_func = NULL;


void
print_language_names ()
{
  struct lang_entry *name, *ext;

  puts ("\nThese are the currently supported languages, along with the\n\
default extensions for files:");
  for (name = lang_names; name->extension; ++name)
    {
      printf ("\t%s\t", name->extension);
      for (ext = lang_extensions; ext->extension; ++ext)
	if (name->function == ext->function)
	  printf (" .%s", ext->extension);
      puts ("");
    }
  puts ("Where `auto' means use default language for files based on filename\n\
extension, and `none' means only do regexp processing on files.\n\
If no language is specified and no extension is found for some file,\n\
Fortran is tried first; if no tags are found, C is tried next.");
}

void
print_version ()
{
#ifdef VERSION
  printf ("%s for Emacs version %s.\n", (CTAGS) ? "CTAGS" : "ETAGS", VERSION);
#else
  printf ("%s for Emacs version 19.\n", (CTAGS) ? "CTAGS" : "ETAGS");
#endif  

  exit (GOOD);
}

void
print_help ()
{
  printf ("These are the options accepted by %s.  You may use unambiguous\n\
abbreviations for the long option names.  A - as file name means read\n\
names from stdin.\n\n", progname);

  puts ("-a, --append\n\
        Append tag entries to existing tags file.");

  if (CTAGS)
    puts ("-B, --backward-search\n\
        Write the search commands for the tag entries using '?', the\n\
        backward-search command instead of '/', the forward-search command.");

  puts ("-C, --c++\n\
        Treat files whose extension defaults to C language as C++ files.");

  if (CTAGS)
    puts ("-d, --defines\n\
        Create tag entries for constant C #defines, too.");
  else
    puts ("-D, --no-defines\n\
        Don't create tag entries for constant C #defines.  This makes\n\
	the tags file smaller.");

  if (!CTAGS)
    {
      puts ("-i FILE, --include=FILE\n\
        Include a note in tag file indicating that, when searching for\n\
        a tag, one should also consult the tags file FILE after\n\
        checking the current file.");
      puts ("-l LANG, --language=LANG\n\
        Force the following files to be considered as written in the\n\
	named language up to the next --language=LANG option.");
    }

#ifdef ETAGS_REGEXPS
  puts ("-r /REGEXP/, --regex=/REGEXP/\n\
        Make a tag for each line matching pattern REGEXP in the\n\
 	following files.  REGEXP is anchored (as if preceded by ^).\n\
	The form /REGEXP/NAME/ creates a named tag.  For example Tcl\n\
	named tags can be created with:\n\
	--regex=/proc[ \\t]+\\([^ \\t]+\\)/\\1/.");
  puts ("-R, --no-regex\n\
        Don't create tags from regexps for the following files.");
#endif /* ETAGS_REGEXPS */
  puts ("-o FILE, --output=FILE\n\
        Write the tags to FILE.");
  puts ("-I, --ignore-indentation\n\
        Don't rely on indentation quite as much as normal.  Currently,\n\
        this means not to assume that a closing brace in the first\n\
        column is the final brace of a function or structure\n\
        definition in C and C++.");

  if (CTAGS)
    {
      puts ("-t, --typedefs\n\
        Generate tag entries for C typedefs.");
      puts ("-T, --typedefs-and-c++\n\
        Generate tag entries for C typedefs, C struct/enum/union tags,\n\
        and C++ member functions.");
      puts ("-u, --update\n\
        Update the tag entries for the given files, leaving tag\n\
        entries for other files in place.  Currently, this is\n\
        implemented by deleting the existing entries for the given\n\
        files and then rewriting the new entries at the end of the\n\
        tags file.  It is often faster to simply rebuild the entire\n\
        tag file than to use this.");
      puts ("-v, --vgrind\n\
        Generates an index of items intended for human consumption,\n\
        similar to the output of vgrind.  The index is sorted, and\n\
        gives the page number of each item.");
      puts ("-w, --no-warn\n\
        Suppress warning messages about entries defined in multiple\n\
        files.");
      puts ("-x, --cxref\n\
        Like --vgrind, but in the style of cxref, rather than vgrind.\n\
        The output uses line numbers instead of page numbers, but\n\
        beyond that the differences are cosmetic; try both to see\n\
        which you like.");
    }

  puts ("-V, --version\n\
        Print the version of the program.\n\
-h, --help\n\
        Print this help message.");

  print_language_names ();

  exit (GOOD);
}


enum argument_type
{
  at_language,
  at_regexp,
  at_filename
};

/* This structure helps us allow mixing of --lang and filenames. */
typedef struct
{
  enum argument_type arg_type;
  char *what;
  Lang_function *function;
} ARGUMENT;

#ifdef VMS			/* VMS specific functions */

#define	EOS	'\0'

/* This is a BUG!  ANY arbitrary limit is a BUG!
   Won't someone please fix this?  */
#define	MAX_FILE_SPEC_LEN	255
typedef struct	{
  short   curlen;
  char    body[MAX_FILE_SPEC_LEN + 1];
} vspec;

/*
 v1.05 nmm 26-Jun-86 fn_exp - expand specification of list of file names
 returning in each successive call the next filename matching the input
 spec. The function expects that each in_spec passed
 to it will be processed to completion; in particular, up to and
 including the call following that in which the last matching name
 is returned, the function ignores the value of in_spec, and will
 only start processing a new spec with the following call. 
 If an error occurs, on return out_spec contains the value
 of in_spec when the error occurred.

 With each successive filename returned in out_spec, the
 function's return value is one. When there are no more matching
 names the function returns zero. If on the first call no file
 matches in_spec, or there is any other error, -1 is returned. 
*/

#include	<rmsdef.h>
#include	<descrip.h>
#define		OUTSIZE	MAX_FILE_SPEC_LEN
short
fn_exp (out, in)
     vspec *out;
     char *in;
{
  static long context = 0;
  static struct dsc$descriptor_s o;
  static struct dsc$descriptor_s i;
  static logical pass1 = TRUE;
  long status;
  short retval;

  if (pass1)
    {
      pass1 = FALSE;
      o.dsc$a_pointer = (char *) out;
      o.dsc$w_length = (short)OUTSIZE;
      i.dsc$a_pointer = in;
      i.dsc$w_length = (short)strlen(in);
      i.dsc$b_dtype = DSC$K_DTYPE_T;
      i.dsc$b_class = DSC$K_CLASS_S;
      o.dsc$b_dtype = DSC$K_DTYPE_VT;
      o.dsc$b_class = DSC$K_CLASS_VS;
    }
  if ((status = lib$find_file(&i, &o, &context, 0, 0)) == RMS$_NORMAL)
    {
      out->body[out->curlen] = EOS;
      return 1;
    }
  else if (status == RMS$_NMF)
    retval = 0;
  else
    {
      strcpy(out->body, in);
      retval = -1;
    }
  lib$find_file_end(&context);
  pass1 = TRUE;
  return retval;
}	

/*
  v1.01 nmm 19-Aug-85 gfnames - return in successive calls the 
  name of each file specified by the provided arg expanding wildcards.
*/
char *
gfnames (arg, p_error)
     char *arg;
     logical *p_error;
{
  static vspec filename = {MAX_FILE_SPEC_LEN, "\0"};

  switch (fn_exp (&filename, arg))
    {
    case 1:
      *p_error = FALSE;
      return filename.body;
    case 0:
      *p_error = FALSE;
      return NULL;
    default:
      *p_error = TRUE;
      return filename.body;
    }
}

#ifndef OLD  /* Newer versions of VMS do provide `system'.  */
system (cmd)
     char *cmd;
{
  fprintf (stderr, "system() function not implemented under VMS\n");
}
#endif

#define	VERSION_DELIM	';'
char *massage_name (s)
     char *s;
{
  char *start = s;	

  for ( ; *s; s++)
    if (*s == VERSION_DELIM)
      {
	*s = EOS;
	break;
      }
    else
      *s = tolower(*s);
  return start;
}
#endif /* VMS */


void
main (argc, argv)
     int argc;
     char *argv[];
{
  int i;
  unsigned int nincluded_files = 0;
  char **included_files = xnew (argc, char *);
  char *this_file;
  ARGUMENT *argbuffer;
  int current_arg = 0, file_count = 0;
  struct linebuffer filename_lb;
#ifdef VMS
  logical got_err;
#endif
 
#ifdef DOS_NT
  _fmode = O_BINARY;   /* all of files are treated as binary files */
#endif /* DOS_NT */

  progname = argv[0];

  /* Allocate enough no matter what happens.  Overkill, but each one
     is small. */
  argbuffer = xnew (argc, ARGUMENT);

#ifdef ETAGS_REGEXPS
  /* Set syntax for regular expression routines. */
  re_set_syntax (RE_SYNTAX_EMACS);
#endif /* ETAGS_REGEXPS */

  /*
   * If etags, always find typedefs and structure tags.  Why not?
   * Also default is to find macro constants.
   */
  if (!CTAGS)
    typedefs = typedefs_and_cplusplus = constantypedefs = TRUE;

  while (1)
    {
      int opt = getopt_long (argc, argv,
			     "-aCdDf:Il:o:r:RStTi:BuvxwVhH", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  /* If getopt returns 0, then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	case 1:
	  /* This means that a filename has been seen.  Record it. */
	  argbuffer[current_arg].arg_type = at_filename;
	  argbuffer[current_arg].what = optarg;
	  ++current_arg;
	  ++file_count;
	  break;

	  /* Common options. */
	case 'a':
	  append_to_tagfile = TRUE;
	  break;
	case 'C':
	  cplusplus = TRUE;
	  break;
	case 'd':
	  constantypedefs = TRUE;
	  break;
	case 'D':
	  constantypedefs = FALSE;
	  break;
	case 'f':		/* for compatibility with old makefiles */
	case 'o':
	  if (tagfile)
	    {
	      fprintf (stderr, "%s: -%c option may only be given once.\n",
		       progname, opt);
	      goto usage;
	    }
	  tagfile = optarg;
	  break;
	case 'I':
	case 'S':		/* for backward compatibility */
	  noindentypedefs = TRUE;
	  break;
	case 'l':
	  if (!get_language (optarg, &argbuffer[current_arg].function))
	    {
	      fprintf (stderr, "%s: language \"%s\" not recognized.\n",
		       progname, optarg);
	      goto usage;
	    }
	  argbuffer[current_arg].arg_type = at_language;
	  ++current_arg;
	  break;
#ifdef ETAGS_REGEXPS
	case 'r':
	  argbuffer[current_arg].arg_type = at_regexp;
	  argbuffer[current_arg].what = optarg;
	  ++current_arg;
	  break;
	case 'R':
	  argbuffer[current_arg].arg_type = at_regexp;
	  argbuffer[current_arg].what = NULL;
	  ++current_arg;
	  break;
#endif /* ETAGS_REGEXPS */
	case 'V':
	  print_version ();
	  break;
	case 'h':
	case 'H':
	  print_help ();
	  break;
	case 't':
	  typedefs = TRUE;
	  break;
	case 'T':
	  typedefs = typedefs_and_cplusplus = TRUE;
	  break;
#if (!CTAGS)
	  /* Etags options */
	case 'i':
	  included_files[nincluded_files++] = optarg;
	  break;
#else /* CTAGS */
	  /* Ctags options. */
	case 'B':
	  searchar = '?';
	  break;
	case 'u':
	  update = TRUE;
	  break;
	case 'v':
	  vgrind_style = TRUE;
	  /*FALLTHRU*/
	case 'x':
	  cxref_style = TRUE;
	  break;
	case 'w':
	  no_warnings = TRUE;
	  break;
#endif /* CTAGS */
	default:
	  goto usage;
	}
    }

  for (; optind < argc; ++optind)
    {
      argbuffer[current_arg].arg_type = at_filename;
      argbuffer[current_arg].what = argv[optind];
      ++current_arg;
      ++file_count;
    }

  if (nincluded_files == 0 && file_count == 0)
    {
      fprintf (stderr, "%s: No input files specified.\n", progname);

    usage:
      fprintf (stderr, "\tTry `%s --help' for a complete list of options.\n",
	       progname);
      exit (BAD);
    }

  if (tagfile == NULL)
    {
      tagfile = CTAGS ? "tags" : "TAGS";
    }
  cwd = etags_getcwd ();	/* the current working directory */
  strcat (cwd, "/");
  if (streq (tagfile, "-"))
    {
      tagfiledir = cwd;
    }
  else
    {
      tagfiledir = absolute_dirname (tagfile, cwd);
    }

  init ();			/* set up boolean "functions" */

  initbuffer (&lb);
  initbuffer (&token_name);
  initbuffer (&lbs[0].lb);
  initbuffer (&lbs[1].lb);
  initbuffer (&filename_lb);

  if (!CTAGS)
    {
      if (streq (tagfile, "-"))
	tagf = stdout;
      else
	tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
      if (tagf == NULL)
	pfatal (tagfile);
    }

  /*
   * Loop through files finding functions.
   */
  for (i = 0; i < current_arg; ++i)
    {
      switch (argbuffer[i].arg_type)
	{
	case at_language:
	  lang_func = argbuffer[i].function;
	  break;
#ifdef ETAGS_REGEXPS
	case at_regexp:
	  add_regex (argbuffer[i].what);
	  break;
#endif
	case at_filename:
#ifdef VMS
	  while ((this_file = gfnames (argbuffer[i].what, &got_err)) != NULL)
	    {
	      if (got_err)
		{
		  error ("Can't find file %s\n", this_file);
		  argc--, argv++;
		}
	      else
		{
		  this_file = massage_name (this_file);
		}
#else
	      this_file = argbuffer[i].what;
#endif
	      /* Input file named "-" means read file names from stdin
		 and use them. */
	      if (streq (this_file, "-"))
		while (readline_internal (&filename_lb, stdin) > 0)
		  process_file (filename_lb.buffer);
	      else
		process_file (this_file);
#ifdef VMS
	    }
#endif
	  break;
	}
    }

  if (!CTAGS)
    {
      while (nincluded_files-- > 0)
	fprintf (tagf, "\f\n%s,include\n", *included_files++);

      fclose (tagf);
      exit (GOOD);
    }

  /* If CTAGS, we are here.  process_file did not write the tags yet,
     because we want them ordered.  Let's do it now. */
  if (cxref_style)
    {
      tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
      if (tagf == NULL)
	pfatal (tagfile);
      put_entries (head);
      exit (GOOD);
    }

  if (update)
    {
      char cmd[BUFSIZ];
      for (i = 0; i < current_arg; ++i)
	{
	  if (argbuffer[i].arg_type != at_filename)
	    continue;
	  sprintf (cmd,
		   "mv %s OTAGS;fgrep -v '\t%s\t' OTAGS >%s;rm OTAGS",
		   tagfile, argbuffer[i].what, tagfile);
	  if (system (cmd) != GOOD)
	    fatal ("failed to execute shell command");
	}
      append_to_tagfile = TRUE;
    }

  tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
  if (tagf == NULL)
    pfatal (tagfile);
  put_entries (head);
  fclose (tagf);

  if (update)
    {
      char cmd[BUFSIZ];
      sprintf (cmd, "sort %s -o %s", tagfile, tagfile);
      exit (system (cmd));
    }
  exit (GOOD);
}


/*
 * Set the language, given the name.
 */
logical
get_language (language, func)
     char *language;
     Lang_function **func;
{
  struct lang_entry *lang;

  for (lang = lang_names; lang->extension; ++lang)
    {
      if (streq (language, lang->extension))
	{
	  *func = lang->function;
	  return TRUE;
	}
    }

  return FALSE;
}


/*
 * This routine is called on each file argument.
 */
void
process_file (file)
     char *file;
{
  struct stat stat_buf;
  FILE *inf;

  if (stat (file, &stat_buf) == 0 && !S_ISREG (stat_buf.st_mode))
    {
      fprintf (stderr, "Skipping %s: it is not a regular file.\n", file);
      return;
    }
  if (streq (file, tagfile) && !streq (tagfile, "-"))
    {
      fprintf (stderr, "Skipping inclusion of %s in self.\n", file);
      return;
    }
  inf = fopen (file, "r");
  if (inf == NULL)
    {
      perror (file);
      return;
    }

  find_entries (file, inf);

  if (!CTAGS)
    {
      char *filename;

      if (file[0] == '/')
	{
	  /* file is an absolute filename.  Canonicalise it. */
	  filename = absolute_filename (file, cwd);
	}
      else
	{
	  /* file is a filename relative to cwd.  Make it relative
	     to the directory of the tags file. */
	  filename = relative_filename (file, tagfiledir);
	}
      fprintf (tagf, "\f\n%s,%d\n", filename, total_size_of_entries (head));
      free (filename);
      put_entries (head);
      free_tree (head);
      head = NULL;
    }
}

/*
 * This routine sets up the boolean pseudo-functions which work
 * by setting boolean flags dependent upon the corresponding character
 * Every char which is NOT in that string is not a white char.  Therefore,
 * all of the array "_wht" is set to FALSE, and then the elements
 * subscripted by the chars in "white" are set to TRUE.  Thus "_wht"
 * of a char is TRUE if it is the string "white", else FALSE.
 */
void
init ()
{
  register char *sp;
  register int i;

  for (i = 0; i < 0177; i++)
    _wht[i] = _etk[i] = _itk[i] = _btk[i] = FALSE;
  for (sp = white; *sp; sp++)
    _wht[*sp] = TRUE;
  for (sp = endtk; *sp; sp++)
    _etk[*sp] = TRUE;
  for (sp = intk; *sp; sp++)
    _itk[*sp] = TRUE;
  for (sp = begtk; *sp; sp++)
    _btk[*sp] = TRUE;
  _wht[0] = _wht['\n'];
  _etk[0] = _etk['\n'];
  _btk[0] = _btk['\n'];
  _itk[0] = _itk['\n'];
}

/*
 * This routine opens the specified file and calls the function
 * which finds the function and type definitions.
 */
void
find_entries (file, inf)
     char *file;
     FILE *inf;
{
  char *cp;
  struct lang_entry *lang;
  NODE *old_last_node;
  extern NODE *last_node;

  curfile = savestr (file);
  cp = etags_strrchr (file, '.');

  /* If user specified a language, use it. */
  if (lang_func != NULL)
    {
      lang_func (inf);
      fclose (inf);
      return;
    }

  if (cp)
    {
      ++cp;
      for (lang = lang_extensions; lang->extension; ++lang)
	{
	  if (streq (cp, lang->extension))
	    {
	      lang->function (inf);
	      fclose (inf);
	      return;
	    }
	}
    }

  /* Try Fortran. */
  old_last_node = last_node;
  Fortran_functions (inf);

  /* No Fortran entries found.  Try C. */
  if (old_last_node == last_node)
    default_C_entries (inf);
  fclose (inf);
}

/* Record a tag. */
void
pfnote (name, is_func, named, linestart, linelen, lno, cno)
     char *name;		/* tag name */
     logical is_func;		/* tag is a function */
     logical named;		/* tag different from text of definition */
     char *linestart;		/* start of the line where tag is */
     int linelen;		/* length of the line where tag is */
     int lno;			/* line number */
     long cno;			/* character number */
{
  register NODE *np = xnew (1, NODE);
  register char *fp;

  /* If ctags mode, change name "main" to M<thisfilename>. */
  if (CTAGS && !cxref_style && streq (name, "main"))
    {
      fp = etags_strrchr (curfile, '/');
      np->name = concat ("M", fp == 0 ? curfile : fp + 1, "");
      fp = etags_strrchr (np->name, '.');
      if (fp && fp[1] != '\0' && fp[2] == '\0')
	fp[0] = 0;
      np->named = TRUE;
    }
  else
    {
      np->name = name;
      np->named = named;
    }
  np->been_warned = FALSE;
  np->file = curfile;
  np->is_func = is_func;
  np->lno = lno;
  /* Our char numbers are 0-base, because of C language tradition?
     ctags compatibility?  old versions compatibility?   I don't know.
     Anyway, since emacs's are 1-base we espect etags.el to take care
     of the difference.  If we wanted to have 1-based numbers, we would
     uncomment the +1 below. */
  np->cno = cno /* + 1 */ ;
  np->left = np->right = NULL;
  np->pat = savenstr (linestart, ((CTAGS && !cxref_style) ? 50 : linelen));

  add_node (np, &head);
}

/*
 * free_tree ()
 *	recurse on left children, iterate on right children.
 */
void
free_tree (node)
     register NODE *node;
{
  while (node)
    {
      register NODE *node_right = node->right;
      free_tree (node->left);
      if (node->named)
	free (node->name);
      free (node->pat);
      free ((char *) node);
      node = node_right;
    }
}

/*
 * add_node ()
 *	Adds a node to the tree of nodes.  In etags mode, we don't keep
 *	it sorted; we just keep a linear list.  In ctags mode, maintain
 *	an ordered tree, with no attempt at balancing.
 *
 *	add_node is the only function allowed to add nodes, so it can
 *	maintain state.
 */
NODE *last_node = NULL;
void
add_node (node, cur_node_p)
     NODE *node, **cur_node_p;
{
  register int dif;
  register NODE *cur_node = *cur_node_p;

  if (cur_node == NULL)
    {
      *cur_node_p = node;
      last_node = node;
      return;
    }

  if (!CTAGS)
    {
      /* Etags Mode */
      if (last_node == NULL)
	fatal ("internal error in add_node", 0);
      last_node->right = node;
      last_node = node;
    }
  else
    {
      /* Ctags Mode */
      dif = strcmp (node->name, cur_node->name);

      /*
       * If this tag name matches an existing one, then
       * do not add the node, but maybe print a warning.
       */
      if (!dif)
	{
	  if (node->file == cur_node->file)
	    {
	      if (!no_warnings)
		{
		  fprintf (stderr, "Duplicate entry in file %s, line %d: %s\n",
			   node->file, lineno, node->name);
		  fprintf (stderr, "Second entry ignored\n");
		}
	      return;
	    }
	  if (!cur_node->been_warned && !no_warnings)
	    {
	      fprintf (stderr,
		  "Duplicate entry in files %s and %s: %s (Warning only)\n",
		       node->file, cur_node->file, node->name);
	    }
	  cur_node->been_warned = TRUE;
	  return;
	}

      /* Maybe refuse to add duplicate nodes.  */
      if (!permit_duplicates)
	{
	  if (streq (node->name, cur_node->name)
	      && streq (node->file, cur_node->file))
	    return;
	}

      /* Actually add the node */
      add_node (node, dif < 0 ? &cur_node->left : &cur_node->right);
    }
}

void
put_entries (node)
     register NODE *node;
{
  register char *sp;

  if (node == NULL)
    return;

  /* Output subentries that precede this one */
  put_entries (node->left);

  /* Output this entry */

  if (!CTAGS)
    {
      if (node->named)
	{
	  fprintf (tagf, "%s\177%s\001%d,%d\n",
		   node->pat, node->name,
		   node->lno, node->cno);
	}
      else
	{
	  fprintf (tagf, "%s\177%d,%d\n",
		   node->pat,
		   node->lno, node->cno);
	}
    }
  else if (!cxref_style)
    {
      fprintf (tagf, "%s\t%s\t",
	       node->name, node->file);

      if (node->is_func)
	{			/* a function */
	  putc (searchar, tagf);
	  putc ('^', tagf);

	  for (sp = node->pat; *sp; sp++)
	    {
	      if (*sp == '\\' || *sp == searchar)
		putc ('\\', tagf);
	      putc (*sp, tagf);
	    }
	  putc (searchar, tagf);
	}
      else
	{			/* a typedef; text pattern inadequate */
	  fprintf (tagf, "%d", node->lno);
	}
      putc ('\n', tagf);
    }
  else if (vgrind_style)
    fprintf (stdout, "%s %s %d\n",
	     node->name, node->file, (node->lno + 63) / 64);
  else
    fprintf (stdout, "%-16s %3d %-16s %s\n",
	     node->name, node->lno, node->file, node->pat);

  /* Output subentries that follow this one */
  put_entries (node->right);
}

/* Length of a number's decimal representation. */
int
number_len (num)
     long num;
{
  int len = 0;
  if (!num)
    return 1;
  for (; num; num /= 10)
    ++len;
  return len;
}

/*
 * Return total number of characters that put_entries will output for
 * the nodes in the subtree of the specified node.  Works only if
 * we are not ctags, but called only in that case.  This count
 * is irrelevant with the new tags.el, but is still supplied for
 * backward compatibility.
 */
int
total_size_of_entries (node)
     register NODE *node;
{
  register int total;

  if (node == NULL)
    return 0;

  total = 0;
  for (; node; node = node->right)
    {
      /* Count left subentries. */
      total += total_size_of_entries (node->left);

      /* Count this entry */
      total += strlen (node->pat) + 1;
      total += number_len ((long) node->lno) + 1 + number_len (node->cno) + 1;
      if (node->named)
	total += 1 + strlen (node->name);	/* \001name */
    }

  return total;
}

/*
 * The C symbol tables.
 */
enum sym_type
{
  st_none, st_C_struct, st_C_enum, st_C_define, st_C_typedef, st_C_typespec
};

/* Feed stuff between (but not including) %[ and %] lines to:
      gperf -c -k1,3 -o -p -r -t
%[
struct C_stab_entry { char *name; int c_ext; enum sym_type type; }
%%
class,  	C_PLPL,	st_C_struct
domain, 	C_STAR,	st_C_struct
union,  	0,	st_C_struct
struct, 	0,	st_C_struct
enum,    	0,	st_C_enum
typedef, 	0,	st_C_typedef
define,  	0,	st_C_define
long,    	0,	st_C_typespec
short,   	0,	st_C_typespec
int,     	0,	st_C_typespec
char,    	0,	st_C_typespec
float,   	0,	st_C_typespec
double,  	0,	st_C_typespec
signed,  	0,	st_C_typespec
unsigned,	0,	st_C_typespec
auto,    	0,	st_C_typespec
void,    	0,	st_C_typespec
extern,  	0,	st_C_typespec
static,  	0,	st_C_typespec
const,   	0,	st_C_typespec
volatile,	0,	st_C_typespec
%]
and replace lines between %< and %> with its output. */
/*%<*/
/* C code produced by gperf version 1.8.1 (K&R C version) */
/* Command-line: gperf -c -k1,3 -o -p -r -t  */


struct C_stab_entry { char *name; int c_ext; enum sym_type type; };

#define MIN_WORD_LENGTH 3
#define MAX_WORD_LENGTH 8
#define MIN_HASH_VALUE 10
#define MAX_HASH_VALUE 62
/*
   21 keywords
   53 is the maximum key range
*/

static int
hash (str, len)
     register char  *str;
     register int  len;
{
  static unsigned char hash_table[] =
    {
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
     62, 62, 62, 62, 62, 62, 62,  2, 62,  7,
      6,  9, 15, 30, 62, 24, 62, 62,  1, 24,
      7, 27, 13, 62, 19, 26, 18, 27,  1, 62,
     62, 62, 62, 62, 62, 62, 62, 62,
  };
  return len + hash_table[str[2]] + hash_table[str[0]];
}

struct C_stab_entry *
in_word_set  (str, len)
     register char *str;
     register int len;
{

  static struct C_stab_entry  wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, 
      {"volatile", 	0,	st_C_typespec},
      {"",}, 
      {"long",     	0,	st_C_typespec},
      {"char",     	0,	st_C_typespec},
      {"class",   	C_PLPL,	st_C_struct},
      {"",}, {"",}, {"",}, {"",}, 
      {"const",    	0,	st_C_typespec},
      {"",}, {"",}, {"",}, {"",}, 
      {"auto",     	0,	st_C_typespec},
      {"",}, {"",}, 
      {"define",   	0,	st_C_define},
      {"",}, 
      {"void",     	0,	st_C_typespec},
      {"",}, {"",}, {"",}, 
      {"extern",   	0,	st_C_typespec},
      {"static",   	0,	st_C_typespec},
      {"",}, 
      {"domain",  	C_STAR,	st_C_struct},
      {"",}, 
      {"typedef",  	0,	st_C_typedef},
      {"double",   	0,	st_C_typespec},
      {"enum",     	0,	st_C_enum},
      {"",}, {"",}, {"",}, {"",}, 
      {"int",      	0,	st_C_typespec},
      {"",}, 
      {"float",    	0,	st_C_typespec},
      {"",}, {"",}, {"",}, 
      {"struct",  	0,	st_C_struct},
      {"",}, {"",}, {"",}, {"",}, 
      {"union",   	0,	st_C_struct},
      {"",}, 
      {"short",    	0,	st_C_typespec},
      {"",}, {"",}, 
      {"unsigned", 	0,	st_C_typespec},
      {"signed",   	0,	st_C_typespec},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= MIN_HASH_VALUE)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && strneq (str + 1, s + 1, len - 1))
            return &wordlist[key];
        }
    }
  return 0;
}
/*%>*/

enum sym_type
C_symtype(str, len, c_ext)
     char *str;
     int len;
     int c_ext;
{
  register struct C_stab_entry *se = in_word_set(str, len);

  if (se == NULL || (se->c_ext && !(c_ext & se->c_ext)))
    return st_none;
  return se->type;
}

 /*
  * C functions are recognized using a simple finite automaton.
  * funcdef is its state variable.
  */
typedef enum
{
  fnone,			/* nothing seen */
  ftagseen,			/* function-like tag seen */
  fstartlist,			/* just after open parenthesis */
  finlist,			/* in parameter list */
  flistseen,			/* after parameter list */
  fignore			/* before open brace */
} FUNCST;
FUNCST funcdef;


 /*
  * typedefs are recognized using a simple finite automaton.
  * typeddef is its state variable.
  */
typedef enum
{
  tnone,			/* nothing seen */
  ttypedseen,			/* typedef keyword seen */
  tinbody,			/* inside typedef body */
  tend,				/* just before typedef tag */
  tignore			/* junk after typedef tag */
} TYPEDST;
TYPEDST typdef;


 /* 
  * struct-like structures (enum, struct and union) are recognized
  * using another simple finite automaton.  `structdef' is its state
  * variable.
  */
typedef enum
{
  snone,			/* nothing seen yet */
  skeyseen,			/* struct-like keyword seen */
  stagseen,			/* struct-like tag seen */
  scolonseen,			/* colon seen after struct-like tag */
  sinbody			/* in struct body: recognize member func defs*/
} STRUCTST;
STRUCTST structdef;

/*
 * When structdef is stagseen, scolonseen, or sinbody, structtag is the
 * struct tag, and structtype is the type of the preceding struct-like  
 * keyword.
 */
char *structtag = "<uninited>";
enum sym_type structtype;

/*
 * Yet another little state machine to deal with preprocessor lines.
 */
typedef enum
{
  dnone,			/* nothing seen */
  dsharpseen,			/* '#' seen as first char on line */
  ddefineseen,			/* '#' and 'define' seen */
  dignorerest			/* ignore rest of line */
} DEFINEST;
DEFINEST definedef;

/*
 * Set this to TRUE, and the next token considered is called a function.
 * Used only for GNU emacs's function-defining macros.
 */
logical next_token_is_func;

/*
 * TRUE in the rules part of a yacc file, FALSE outside (parse as C).
 */
logical yacc_rules;

/*
 * consider_token ()
 *	checks to see if the current token is at the start of a
 *	function, or corresponds to a typedef, or is a struct/union/enum
 *	tag.
 *
 *	*IS_FUNC gets TRUE iff the token is a function or macro with args.
 *	C_EXT is which language we are looking at.
 *
 *	In the future we will need some way to adjust where the end of
 *	the token is; for instance, implementing the C++ keyword
 *	`operator' properly will adjust the end of the token to be after
 *	whatever follows `operator'.
 *
 * Globals
 *	funcdef			IN OUT
 *	structdef		IN OUT
 *	definedef		IN OUT
 *	typdef			IN OUT
 *	next_token_is_func	IN OUT
 */

logical
consider_token (str, len, c, c_ext, cblev, is_func)
     register char *str;	/* IN: token pointer */
     register int len;		/* IN: token length */
     register char c;		/* IN: first char after the token */
     int c_ext;			/* IN: C extensions mask */
     int cblev;			/* IN: curly brace level */
     logical *is_func;		/* OUT: function found */
{
  enum sym_type toktype = C_symtype (str, len, c_ext);

  /*
   * Advance the definedef state machine.
   */
  switch (definedef)
    {
    case dnone:
      /* We're not on a preprocessor line. */
      break;
    case dsharpseen:
      if (toktype == st_C_define)
	{
	  definedef = ddefineseen;
	}
      else
	{
	  definedef = dignorerest;
	}
      return FALSE;
    case ddefineseen:
      /*
       * Make a tag for any macro, unless it is a constant
       * and constantypedefs is FALSE.
       */
      definedef = dignorerest;
      *is_func = (c == '(');
      if (!*is_func && !constantypedefs)
	return FALSE;
      else
	return TRUE;
    case dignorerest:
      return FALSE;
    default:
      error ("internal error: definedef value.", 0);
    }

  /*
   * Now typedefs
   */
  switch (typdef)
    {
    case tnone:
      if (toktype == st_C_typedef)
	{
	  if (typedefs)
	    typdef = ttypedseen;
	  funcdef = fnone;
	  return FALSE;
	}
      break;
    case ttypedseen:
      switch (toktype)
	{
	case st_none:
	case st_C_typespec:
	  typdef = tend;
	  break;
	case st_C_struct:
	case st_C_enum:
	  break;
	}
      /* Do not return here, so the structdef stuff has a chance. */
      break;
    case tend:
      switch (toktype)
	{
	case st_C_typespec:
	case st_C_struct:
	case st_C_enum:
	  return FALSE;
	}
      return TRUE;
    }

  /*
   * This structdef business is currently only invoked when cblev==0.
   * It should be recursively invoked whatever the curly brace level,
   * and a stack of states kept, to allow for definitions of structs
   * within structs.
   *
   * This structdef business is NOT invoked when we are ctags and the
   * file is plain C.  This is because a struct tag may have the same
   * name as another tag, and this loses with ctags.
   *
   * This if statement deals with the typdef state machine as 
   * follows: if typdef==ttypedseen and token is struct/union/class/enum,
   * return FALSE.  All the other code here is for the structdef 
   * state machine.
   */
  switch (toktype)
    {
    case st_C_struct:
    case st_C_enum:
      if (typdef == ttypedseen
	  || (typedefs_and_cplusplus && cblev == 0 && structdef == snone))
	{
	  structdef = skeyseen;
	  structtype = toktype;
	}
      return FALSE;
    }
  if (structdef == skeyseen)
    {
      /* Save the tag for struct/union/class, for functions that may be
         defined inside. */
      if (structtype == st_C_struct)
	structtag = savenstr (str, len);
      else
	structtag = "<enum>";
      structdef = stagseen;
      return TRUE;
    }

  /* Avoid entering funcdef stuff if typdef is going on. */
  if (typdef != tnone)
    {
      definedef = dnone;
      return FALSE;
    }

  /* Detect GNU macros. */
  if (definedef == dnone)
    if (strneq (str, "DEFUN", 5) /* Used in emacs */
#if FALSE	
	   These are defined inside C functions, so currently they
	   are not met anyway.
	|| strneq (str, "EXFUN", 5) /* Used in glibc */
	|| strneq (str, "DEFVAR_", 7) /* Used in emacs */
#endif
	|| strneq (str, "SYSCALL", 7) /* Used in glibc (mach) */
	|| strneq (str, "ENTRY", 5) /* Used in glibc */
	|| strneq (str, "PSEUDO", 6)) /* Used in glibc */

      {
	next_token_is_func = TRUE;
	return FALSE;
      }
  if (next_token_is_func)
    {
      next_token_is_func = FALSE;
      funcdef = fignore;
      *is_func = TRUE;
      return TRUE;
    }

  /* A function? */
  switch (toktype)
    {
    case st_C_typespec:
      if (funcdef != finlist && funcdef != fignore)
        funcdef = fnone;		/* should be useless */
      return FALSE;
    default:
      if (funcdef == fnone)
	{
	  funcdef = ftagseen;
	  *is_func = TRUE;
	  return TRUE;
	}
    }

  return FALSE;
}

/*
 * C_entries ()
 *	This routine finds functions, typedefs, #define's and
 * 	struct/union/enum definitions in C syntax and adds them
 *	to the list.
 */
typedef struct
{
  logical valid;
  char *str;
  logical named;
  int linelen;
  int lineno;
  long linepos;
  char *buffer;
} TOKEN;

#define current_lb_is_new (newndx == curndx)
#define switch_line_buffers() (curndx = 1 - curndx)

#define curlb (lbs[curndx].lb)
#define othlb (lbs[1-curndx].lb)
#define newlb (lbs[newndx].lb)
#define curlinepos (lbs[curndx].linepos)
#define othlinepos (lbs[1-curndx].linepos)
#define newlinepos (lbs[newndx].linepos)

#define CNL_SAVE_DEFINEDEF						\
do {									\
  curlinepos = charno;							\
  lineno++;								\
  charno += readline (&curlb, inf);					\
  lp = curlb.buffer;							\
  quotednl = FALSE;							\
  newndx = curndx;							\
} while (0)

#define CNL								\
do {									\
  CNL_SAVE_DEFINEDEF;							\
  if (savetok.valid)							\
    {									\
      tok = savetok;							\
      savetok.valid = FALSE;						\
    }									\
  definedef = dnone;							\
} while (0)

#define make_tag(isfun)  do \
{									\
  if (tok.valid)							\
    pfnote (savestr (token_name.buffer), isfun, tok.named,		\
	    tok.buffer, tok.linelen, tok.lineno, tok.linepos);		\
  else if (DEBUG) abort ();						\
  tok.valid = FALSE;							\
} while (0)

void
C_entries (c_ext, inf)
     int c_ext;			/* extension of C */
     FILE *inf;			/* input file */
{
  register char c;		/* latest char read; '\0' for end of line */
  register char *lp;		/* pointer one beyond the character `c' */
  int curndx, newndx;		/* indices for current and new lb */
  TOKEN tok;			/* latest token read */
  register int tokoff;		/* offset in line of start of current token */
  register int toklen;		/* length of current token */
  int cblev;			/* current curly brace level */
  int parlev;			/* current parenthesis level */
  logical incomm, inquote, inchar, quotednl, midtoken;
  logical cplpl;
  TOKEN savetok;		/* token saved during preprocessor handling */


  curndx = newndx = 0;
  lineno = 0;
  charno = 0;
  lp = curlb.buffer;
  *lp = 0;

  definedef = dnone; funcdef = fnone; typdef = tnone; structdef = snone;
  next_token_is_func = yacc_rules = FALSE;
  midtoken = inquote = inchar = incomm = quotednl = FALSE;
  tok.valid = savetok.valid = FALSE;
  cblev = 0;
  parlev = 0;
  cplpl = c_ext & C_PLPL;

  while (!feof (inf))
    {
      c = *lp++;
      if (c == '\\')
	{
	  /* If we're at the end of the line, the next character is a
	     '\0'; don't skip it, because it's the thing that tells us
	     to read the next line.  */
	  if (*lp == '\0')
	    {
	      quotednl = TRUE;
	      continue;
	    }
	  lp++;
	  c = ' ';
	}
      else if (incomm)
	{
	  switch (c)
	    {
	    case '*':
	      if (*lp == '/')
		{
		  c = *lp++;
		  incomm = FALSE;
		}
	      break;
	    case '\0':
	      /* Newlines inside comments do not end macro definitions in
		 traditional cpp. */
	      CNL_SAVE_DEFINEDEF;
	      break;
	    }
	  continue;
	}
      else if (inquote)
	{
	  switch (c)
	    {
	    case '"':
	      inquote = FALSE;
	      break;
	    case '\0':
	      /* Newlines inside strings do not end macro definitions
		 in traditional cpp, even though compilers don't
		 usually accept them. */
	      CNL_SAVE_DEFINEDEF;
	      break;
	    }
	  continue;
	}
      else if (inchar)
	{
	  switch (c)
	    {
	    case '\0':
	      /* Hmmm, something went wrong. */
	      CNL;
	      /* FALLTHRU */
	    case '\'':
	      inchar = FALSE;
	      break;
	    }
	  continue;
	}
      else 
	switch (c)
	  {
	  case '"':
	    inquote = TRUE;
	    if (funcdef != finlist && funcdef != fignore)
	      funcdef = fnone;
	    continue;
	  case '\'':
	    inchar = TRUE;
	    if (funcdef != finlist && funcdef != fignore)
	      funcdef = fnone;
	    continue;
	  case '/':
	    if (*lp == '*')
	      {
		lp++;
		incomm = TRUE;
		continue;
	      }
	    else if (cplpl && *lp == '/')
	      {
		c = 0;
		break;
	      }
	    else
	      break;
	  case '%':
	    if ((c_ext & YACC) && *lp == '%')
	      {
		/* entering or exiting rules section in yacc file */
		lp++;
		definedef = dnone; funcdef = fnone;
		typdef = tnone; structdef = snone;
		next_token_is_func = FALSE;
		midtoken = inquote = inchar = incomm = quotednl = FALSE;
		cblev = 0;
		yacc_rules = !yacc_rules;
		continue;
 	      }
	    else
	      break;
	  case '#':
	    if (definedef == dnone)
	      {
		char *cp;
		logical cpptoken = TRUE;

		/* Look back on this line.  If all blanks, or nonblanks
		   followed by an end of comment, this is a preprocessor
		   token. */
		for (cp = newlb.buffer; cp < lp-1; cp++)
		  if (!iswhite (*cp))
		    {
		      if (*cp == '*' && *(cp+1) == '/')
			{
			  cp++;
			  cpptoken = TRUE;
			}
		      else
			cpptoken = FALSE;
		    }
		if (cpptoken)
		  definedef = dsharpseen;
	      } /* if (definedef == dnone) */

	    continue;
	  } /* switch (c) */


      /* Consider token only if some complicated conditions are satisfied. */
      if ((definedef != dnone
	   || (cblev == 0 && structdef != scolonseen)
	   || (cblev == 1 && cplpl && structdef == sinbody))
	  && typdef != tignore
	  && definedef != dignorerest
	  && funcdef != finlist)
	{
	  if (midtoken)
	    {
	      if (endtoken (c))
		{
		  if (cplpl && c == ':' && *lp == ':' && begtoken(*(lp + 1)))
		    {
		      /*
		       * This handles :: in the middle, but not at the
		       * beginning of an identifier.
		       */
		      lp += 2;
		      toklen += 3;
		    }
		  else
		    {
		      logical is_func = FALSE;

		      if (yacc_rules
			  || consider_token (newlb.buffer + tokoff, toklen,
					     c, c_ext, cblev, &is_func))
			{
			  if (structdef == sinbody
			      && definedef == dnone
			      && is_func)
			    /* function defined in C++ class body */
			    {
			      int strsize = strlen(structtag) + 2 + toklen + 1;
			      while (token_name.size < strsize)
				{
				  token_name.size *= 2;
				  token_name.buffer
				    = (char *) xrealloc (token_name.buffer,
							 token_name.size);
				}
			      strcpy (token_name.buffer, structtag);
			      strcat (token_name.buffer, "::");
			      strncat (token_name.buffer,
				       newlb.buffer+tokoff, toklen);
			      tok.named = TRUE;
			    }
			  else
			    {
			      while (token_name.size < toklen + 1)
				{
				  token_name.size *= 2;
				  token_name.buffer
				    = (char *) xrealloc (token_name.buffer,
							 token_name.size);
				}
			      strncpy (token_name.buffer,
				       newlb.buffer+tokoff, toklen);
			      token_name.buffer[toklen] = '\0';
			      if (structdef == stagseen
				  || typdef == tend
				  || (is_func
				      && definedef == dignorerest)) /* macro */
				tok.named = TRUE;
			      else
				tok.named = FALSE;
			    }
			  tok.lineno = lineno;
			  tok.linelen = tokoff + toklen + 1;
			  tok.buffer = newlb.buffer;
			  tok.linepos = newlinepos;
			  tok.valid = TRUE;

			  if (definedef == dnone
			      && (funcdef == ftagseen
				  || structdef == stagseen
				  || typdef == tend))
			    {
			      if (current_lb_is_new)
				switch_line_buffers ();
			    }
			  else
			    make_tag (is_func);
			}
		      midtoken = FALSE;
		    }
		} /* if (endtoken (c)) */
	      else if (intoken (c))
		{
		  toklen++;
		  continue;
		}
	    } /* if (midtoken) */
	  else if (begtoken (c))
	    {
	      switch (definedef)
		{
		case dnone:
		  switch (funcdef)
		    {
		    case fstartlist:
		      funcdef = finlist;
		      continue;
		    case flistseen:
		      make_tag (TRUE);
		      funcdef = fignore;
		      break;
		    case ftagseen:
		      funcdef = fnone;
		      break;
		    }
		  if (structdef == stagseen)
		    structdef = snone;
		  break;
		case dsharpseen:
		  savetok = tok;
		}
	      if (!yacc_rules || lp == newlb.buffer + 1)
		{
		  tokoff = lp - 1 - newlb.buffer;
		  toklen = 1;
		  midtoken = TRUE;
		}
	      continue;
	    } /* if (begtoken) */
	} /* if must look at token */


      /* Detect end of line, colon, comma, semicolon and various braces
	 after having handled a token.*/
      switch (c)
	{
	case ':':
	  if (definedef != dnone)
	    break;
	  if (structdef == stagseen)
	    structdef = scolonseen;
	  else
	    switch (funcdef)
	      {
	      case ftagseen:
		if (yacc_rules)
		  {
		    make_tag (FALSE);
		    funcdef = fignore;
		  }
		break;
	      case fstartlist:
		funcdef = fnone;
		break;
	      }
	  break;
	case ';':
	  if (definedef != dnone)
	    break;
	  if (cblev == 0)
	    switch (typdef)
	      {
	      case tend:
		make_tag (FALSE);
		/* FALLTHRU */
	      default:
		typdef = tnone;
	      }
	  if (funcdef != fignore)
	    funcdef = fnone;
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case ',':
	  if (definedef != dnone)
	    break;
	  if (funcdef != finlist && funcdef != fignore)
	    funcdef = fnone;
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case '[':
	  if (definedef != dnone)
	    break;
	  if (cblev == 0 && typdef == tend)
	    {
	      typdef = tignore;
	      make_tag (FALSE);
	      break;
	    }
	  if (funcdef != finlist && funcdef != fignore)
	    funcdef = fnone;
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case '(':
	  if (definedef != dnone)
	    break;
	  switch (funcdef)
	    {
	    case fnone:
	      switch (typdef)
		{
		case ttypedseen:
		case tend:
		  /* Make sure that the next char is not a '*'.
		     This handles constructs like:
		     typedef void OperatorFun (int fun); */
		  if (*lp != '*')
		    {
		      typdef = tignore;
		      make_tag (FALSE);
		    }
		  break;
		} /* switch (typdef) */
	      break;
	    case ftagseen:
	      funcdef = fstartlist;
	      break;
	    case flistseen:
	      funcdef = finlist;
	      break;
	    }
	  parlev++;
	  break;
	case ')':
	  if (definedef != dnone)
	    break;
	  if (--parlev == 0)
	    {
	      switch (funcdef)
		{
		case fstartlist:
		case finlist:
		  funcdef = flistseen;
		  break;
		}
	      if (cblev == 0 && typdef == tend)
		{
		  typdef = tignore;
		  make_tag (FALSE);
		}
	    }
	  else if (parlev < 0)	/* can happen due to ill-conceived #if's. */
	    parlev = 0;
	  break;
	case '{':
	  if (definedef != dnone)
	    break;
	  if (typdef == ttypedseen)
	    typdef = tinbody;
	  switch (structdef)
	    {
	    case skeyseen:	/* unnamed struct */
	      structtag = "_anonymous_";
	      structdef = sinbody;
	      break;
	    case stagseen:
	    case scolonseen:	/* named struct */
	      structdef = sinbody;
	      make_tag (FALSE);
	      break;
	    }
	  switch (funcdef)
	    {
	    case flistseen:
	      make_tag (TRUE);
	      /* FALLTHRU */
	    case fignore:
	      funcdef = fnone;
	      break;
	    case fnone:
	      /* Neutralize `extern "C" {' grot and look inside structs. */
	      if (cblev == 0 && structdef == snone && typdef == tnone)
		cblev = -1;
	    }
	  cblev++;
	  break;
	case '*':
	  if (definedef != dnone)
	    break;
	  if (funcdef == fstartlist)
	    funcdef = fnone;	/* avoid tagging `foo' in `foo (*bar()) ()' */
	  break;
	case '}':
	  if (definedef != dnone)
	    break;
	  if (!noindentypedefs && lp == newlb.buffer + 1)
	    {
	      cblev = 0;	/* reset curly brace level if first column */
	      parlev = 0;	/* also reset paren level, just in case... */
	    }
	  else if (cblev > 0)
	    cblev--;
	  if (cblev == 0)
	    {
	      if (typdef == tinbody)
		typdef = tend;
#if FALSE			/* too risky */
	      if (structdef == sinbody)
		free (structtag);
#endif

	      structdef = snone;
	      structtag = "<error>";
	    }
	  break;
	case '=':
	case '#': case '+': case '-': case '~': case '&': case '%': case '/':
	case '|': case '^': case '!': case '<': case '>': case '.': case '?':
	  if (definedef != dnone)
	    break;
	  /* These surely cannot follow a function tag. */
	  if (funcdef != finlist && funcdef != fignore)
	    funcdef = fnone;
	  break;
	case '\0':
	  /* If a macro spans multiple lines don't reset its state. */
	  if (quotednl)
	    CNL_SAVE_DEFINEDEF;
	  else
	    CNL;
	  break;
	} /* switch (c) */

    } /* while not eof */
}

/*
 * Process either a C++ file or a C file depending on the setting
 * of a global flag.
 */
void
default_C_entries (inf)
     FILE *inf;
{
  C_entries (cplusplus ? C_PLPL : 0, inf);
}

/* Always do C++. */
void
Cplusplus_entries (inf)
     FILE *inf;
{
  C_entries (C_PLPL, inf);
}

/* Always do C*. */
void
Cstar_entries (inf)
     FILE *inf;
{
  C_entries (C_STAR, inf);
}

/* Always do Yacc. */
void
Yacc_entries (inf)
     FILE *inf;
{
  C_entries (YACC, inf);
}

/* Fortran parsing */

char *dbp;

logical
tail (cp)
     char *cp;
{
  register int len = 0;

  while (*cp && (*cp | ' ') == (dbp[len] | ' '))
    cp++, len++;
  if (*cp == 0)
    {
      dbp += len;
      return TRUE;
    }
  return FALSE;
}

void
takeprec ()
{
  while (isspace (*dbp))
    dbp++;
  if (*dbp != '*')
    return;
  dbp++;
  while (isspace (*dbp))
    dbp++;
  if (tail ("(*)"))
    return;
  if (!isdigit (*dbp))
    {
      --dbp;			/* force failure */
      return;
    }
  do
    dbp++;
  while (isdigit (*dbp));
}

void
getit (inf)
     FILE *inf;
{
  register char *cp;

  while (isspace (*dbp))
    dbp++;
  if (*dbp == '\0')
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      if (dbp[5] != '&')
	return;
      dbp += 6;
      while (isspace (*dbp))
	dbp++;
    }
  if (!isalpha (*dbp)
      && *dbp != '_'
      && *dbp != '$')
    return;
  for (cp = dbp + 1;
       (*cp
	&& (isalpha (*cp) || isdigit (*cp) || (*cp == '_') || (*cp == '$')));
       cp++)
    continue;
  pfnote (savenstr (dbp, cp-dbp), TRUE, FALSE, lb.buffer,
	  cp - lb.buffer + 1, lineno, linecharno);
}

void
Fortran_functions (inf)
     FILE *inf;
{
  lineno = 0;
  charno = 0;

  while (!feof (inf))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      if (*dbp == '%')
	dbp++;			/* Ratfor escape to fortran */
      while (isspace (*dbp))
	dbp++;
      if (*dbp == 0)
	continue;
      switch (*dbp | ' ')
	{
	case 'i':
	  if (tail ("integer"))
	    takeprec ();
	  break;
	case 'r':
	  if (tail ("real"))
	    takeprec ();
	  break;
	case 'l':
	  if (tail ("logical"))
	    takeprec ();
	  break;
	case 'c':
	  if (tail ("complex") || tail ("character"))
	    takeprec ();
	  break;
	case 'd':
	  if (tail ("double"))
	    {
	      while (isspace (*dbp))
		dbp++;
	      if (*dbp == 0)
		continue;
	      if (tail ("precision"))
		break;
	      continue;
	    }
	  break;
	}
      while (isspace (*dbp))
	dbp++;
      if (*dbp == 0)
	continue;
      switch (*dbp | ' ')
	{
	case 'f':
	  if (tail ("function"))
	    getit (inf);
	  continue;
	case 's':
	  if (tail ("subroutine"))
	    getit (inf);
	  continue;
	case 'e':
	  if (tail ("entry"))
	    getit (inf);
	  continue;
	case 'p':
	  if (tail ("program"))
	    {
	      getit (inf);
	      continue;
	    }
	  if (tail ("procedure"))
	    getit (inf);
	  continue;
	}
    }
}

/*
 * Bob Weiner, Motorola Inc., 4/3/94
 * Unix and microcontroller assembly tag handling
 * look for '^[a-zA-Z_.$][a-zA_Z0-9_.$]*[: ^I^J]'
 */
void
Asm_labels (inf)
     FILE *inf;
{
  register char *cp;

  lineno = 0;
  charno = 0;

  while (!feof (inf))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      cp = lb.buffer;

      /* If first char is alphabetic or one of [_.$], test for colon
	 following identifier. */
      if (isalpha (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	{
 	  /* Read past label. */
	  cp++;
 	  while (isalnum (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	    cp++;
 	  if (*cp == ':' || isspace (*cp))
 	    {
 	      /* Found end of label, so copy it and add it to the table. */
 	      pfnote (savenstr (lb.buffer, cp-lb.buffer), TRUE, FALSE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
 	    }
 	}
    }
}

/* Added by Mosur Mohan, 4/22/88 */
/* Pascal parsing                */

#define GET_NEW_LINE \
{ \
  linecharno = charno; lineno++; \
  charno += 1 + readline (&lb, inf); \
  dbp = lb.buffer; \
}

/*
 *  Locates tags for procedures & functions.  Doesn't do any type- or
 *  var-definitions.  It does look for the keyword "extern" or
 *  "forward" immediately following the procedure statement; if found,
 *  the tag is skipped.
 */
void
Pascal_functions (inf)
     FILE *inf;
{
  struct linebuffer tline;	/* mostly copied from C_entries */
  long save_lcno;
  int save_lineno;
  char c, *cp;
  char *nambuf;

  logical			/* each of these flags is TRUE iff: */
    incomment,			/* point is inside a comment */
    inquote,			/* point is inside '..' string */
    get_tagname,		/* point is after PROCEDURE/FUNCTION */
  /*   keyword, so next item = potential tag */
    found_tag,			/* point is after a potential tag */
    inparms,			/* point is within parameter-list */
    verify_tag;			/* point has passed the parm-list, so the */
  /*   next token will determine whether    */
  /*   this is a FORWARD/EXTERN to be       */
  /*   ignored, or whether it is a real tag */

  lineno = 0;
  charno = 0;
  dbp = lb.buffer;
  *dbp = 0;
  initbuffer (&tline);

  incomment = inquote = FALSE;
  found_tag = FALSE;		/* have a proc name; check if extern */
  get_tagname = FALSE;		/* have found "procedure" keyword    */
  inparms = FALSE;		/* found '(' after "proc"            */
  verify_tag = FALSE;		/* check if "extern" is ahead        */

  /* long main loop to get next char */
  while (!feof (inf))
    {
      c = *dbp++;
      if (c == '\0')		/* if end of line */
	{
	  GET_NEW_LINE;
	  if (*dbp == '\0')
	    continue;
	  if (!((found_tag && verify_tag) ||
		get_tagname))
	    c = *dbp++;		/* only if don't need *dbp pointing */
				/* to the beginning of the name of  */
				/* the procedure or function        */
	}
      if (incomment)
	{
	  if (c == '}')		/* within { - } comments */
	    incomment = FALSE;
	  else if (c == '*' && dbp[1] == ')') /* within (* - *) comments */
	    {
	      dbp++;
	      incomment = FALSE;
	    }
	  continue;
	}
      else if (inquote)
	{
	  if (c == '\'')
	    inquote = FALSE;
	  continue;
	}
      else
	switch (c)
	  {
	  case '\'':
	    inquote = TRUE;	/* found first quote */
	    continue;
	  case '{':		/* found open-{-comment */
	    incomment = TRUE;
	    continue;
	  case '(':
	    if (*dbp == '*')	/* found open-(*-comment */
	      {
		incomment = TRUE;
		dbp++;
	      }
	    else if (found_tag)	/* found '(' after tag, i.e., parm-list */
	      inparms = TRUE;
	    continue;
	  case ')':		/* end of parms list */
	    if (inparms)
	      inparms = FALSE;
	    continue;
	  case ';':
	    if ((found_tag) && (!inparms))	/* end of proc or fn stmt */
	      {
		verify_tag = TRUE;
		break;
	      }
	    continue;
	  }
      if ((found_tag) && (verify_tag) && (*dbp != ' '))
	{
	  /* check if this is an "extern" declaration */
	  if (*dbp == 0)
	    continue;
	  if ((*dbp == 'e') || (*dbp == 'E'))
	    {
	      if (tail ("extern"))	/* superfluous, really! */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  else if ((*dbp == 'f') || (*dbp == 'F'))
	    {
	      if (tail ("forward"))	/*  check for forward reference */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  if ((found_tag) && (verify_tag)) /* not external proc, so make tag */
	    {
	      found_tag = FALSE;
	      verify_tag = FALSE;
	      pfnote (nambuf, TRUE, FALSE, tline.buffer,
		      cp - tline.buffer + 1, save_lineno, save_lcno);
	      continue;
	    }
	}
      if (get_tagname)		/* grab name of proc or fn */
	{
	  if (*dbp == 0)
	    continue;

	  /* save all values for later tagging */
	  tline.size = lb.size;
	  strcpy (tline.buffer, lb.buffer);
	  save_lineno = lineno;
	  save_lcno = linecharno;

	  /* grab block name */
	  for (cp = dbp + 1; *cp && (!endtoken (*cp)); cp++)
	    continue;
	  nambuf = savenstr (dbp, cp-dbp);
	  dbp = cp;		/* restore dbp to e-o-token */
	  get_tagname = FALSE;
	  found_tag = TRUE;
	  continue;

	  /* and proceed to check for "extern" */
	}
      else if (!incomment && !inquote && !found_tag)
	{
	  /* check for proc/fn keywords */
	  switch (c | ' ')
	    {
	    case 'p':
	      if (tail ("rocedure"))	/* c = 'p', dbp has advanced */
		get_tagname = TRUE;
	      continue;
	    case 'f':
	      if (tail ("unction"))
		get_tagname = TRUE;
	      continue;
	    }
	}
    }				/* while not eof */
}

/*
 * lisp tag functions
 *  look for (def or (DEF, quote or QUOTE
 */
int
L_isdef (strp)
     register char *strp;
{
  return ((strp[1] == 'd' || strp[1] == 'D')
	  && (strp[2] == 'e' || strp[2] == 'E')
	  && (strp[3] == 'f' || strp[3] == 'F'));
}

int
L_isquote (strp)
     register char *strp;
{
  return ((*(++strp) == 'q' || *strp == 'Q')
	  && (*(++strp) == 'u' || *strp == 'U')
	  && (*(++strp) == 'o' || *strp == 'O')
	  && (*(++strp) == 't' || *strp == 'T')
	  && (*(++strp) == 'e' || *strp == 'E')
	  && isspace(*(++strp)));
}

void
L_getit ()
{
  register char *cp;

  if (*dbp == '\'')		/* Skip prefix quote */
    dbp++;
  else if (*dbp == '(' && L_isquote (dbp)) /* Skip "(quote " */
  {
    dbp += 7;
    while (isspace(*dbp))
      dbp++;
  }
  for (cp = dbp /*+1*/;
       *cp && *cp != '(' && *cp != ' ' && *cp != ')';
       cp++)
    continue;
  if (cp == dbp)
    return;
  
  pfnote (savenstr (dbp, cp-dbp), TRUE, FALSE, lb.buffer,
	  cp - lb.buffer + 1, lineno, linecharno);
}

void
Lisp_functions (inf)
     FILE *inf;
{
  lineno = 0;
  charno = 0;

  while (!feof (inf))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      if (dbp[0] == '(')
	{
	  if (L_isdef (dbp))
	    {
	      while (!isspace (*dbp))
		dbp++;
	      while (isspace (*dbp))
		dbp++;
	      L_getit ();
	    }
	  else
	    {
	      /* Check for (foo::defmumble name-defined ... */
	      do
		dbp++;
	      while (*dbp && !isspace (*dbp)
		     && *dbp != ':' && *dbp != '(' && *dbp != ')');
	      if (*dbp == ':')
		{
		  do
		    dbp++;
		  while (*dbp == ':');

		  if (L_isdef (dbp - 1))
		    {
		      while (!isspace (*dbp))
			dbp++;
		      while (isspace (*dbp))
			dbp++;
		      L_getit ();
		    }
		}
	    }
	}
    }
}

/*
 * Scheme tag functions
 * look for (def... xyzzy
 * look for (def... (xyzzy
 * look for (def ... ((...(xyzzy ....
 * look for (set! xyzzy
 */

void get_scheme ();

void
Scheme_functions (inf)
     FILE *inf;
{
  lineno = 0;
  charno = 0;

  while (!feof (inf))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      if (dbp[0] == '(' &&
	  (dbp[1] == 'D' || dbp[1] == 'd') &&
	  (dbp[2] == 'E' || dbp[2] == 'e') &&
	  (dbp[3] == 'F' || dbp[3] == 'f'))
	{
	  while (!isspace (*dbp))
	    dbp++;
	  /* Skip over open parens and white space */
	  while (*dbp && (isspace (*dbp) || *dbp == '('))
	    dbp++;
	  get_scheme ();
	}
      if (dbp[0] == '(' &&
	  (dbp[1] == 'S' || dbp[1] == 's') &&
	  (dbp[2] == 'E' || dbp[2] == 'e') &&
	  (dbp[3] == 'T' || dbp[3] == 't') &&
	  (dbp[4] == '!' || dbp[4] == '!') &&
	  (isspace (dbp[5])))
	{
	  while (!isspace (*dbp))
	    dbp++;
	  /* Skip over white space */
	  while (isspace (*dbp))
	    dbp++;
	  get_scheme ();
	}
    }
}

void
get_scheme ()
{
  register char *cp;

  if (*dbp == 0)
    return;
  /* Go till you get to white space or a syntactic break */
  for (cp = dbp + 1;
       *cp && *cp != '(' && *cp != ')' && !isspace (*cp);
       cp++)
    continue;
  pfnote (savenstr (dbp, cp-dbp), TRUE, FALSE,
	  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
}

/* Find tags in TeX and LaTeX input files.  */

/* TEX_toktab is a table of TeX control sequences that define tags.
   Each TEX_tabent records one such control sequence.
   CONVERT THIS TO USE THE Stab TYPE!! */
struct TEX_tabent
{
  char *name;
  int len;
};

struct TEX_tabent *TEX_toktab = NULL;	/* Table with tag tokens */

/* Default set of control sequences to put into TEX_toktab.
   The value of environment var TEXTAGS is prepended to this.  */

char *TEX_defenv = "\
:chapter:section:subsection:subsubsection:eqno:label:ref:cite:bibitem:typeout";

void TEX_mode ();
struct TEX_tabent *TEX_decode_env ();
void TEX_getit ();
int TEX_Token ();

char TEX_esc = '\\';
char TEX_opgrp = '{';
char TEX_clgrp = '}';

/*
 * TeX/LaTeX scanning loop.
 */
void
TeX_functions (inf)
     FILE *inf;
{
  char *lasthit;

  lineno = 0;
  charno = 0;

  /* Select either \ or ! as escape character.  */
  TEX_mode (inf);

  /* Initialize token table once from environment. */
  if (!TEX_toktab)
    TEX_toktab = TEX_decode_env ("TEXTAGS", TEX_defenv);

  while (!feof (inf))
    {				/* Scan each line in file */
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      lasthit = dbp;
      while (dbp = etags_strchr (dbp, TEX_esc)) /* Look at each esc in line */
	{
	  register int i;

	  if (!*(++dbp))
	    break;
	  linecharno += dbp - lasthit;
	  lasthit = dbp;
	  i = TEX_Token (lasthit);
	  if (0 <= i)
	    {
	      TEX_getit (lasthit, TEX_toktab[i].len);
	      break;		/* We only save a line once */
	    }
	}
    }
}

#define TEX_LESC '\\'
#define TEX_SESC '!'
#define TEX_cmt  '%'

/* Figure out whether TeX's escapechar is '\\' or '!' and set grouping
   chars accordingly. */
void
TEX_mode (inf)
     FILE *inf;
{
  int c;

  while ((c = getc (inf)) != EOF)
    {
      /* Skip to next line if we hit the TeX comment char. */
      if (c == TEX_cmt)
	while (c != '\n')
	  c = getc (inf);
      else if (c == TEX_LESC || c == TEX_SESC )
	break;
    }

  if (c == TEX_LESC)
    {
      TEX_esc = TEX_LESC;
      TEX_opgrp = '{';
      TEX_clgrp = '}';
    }
  else
    {
      TEX_esc = TEX_SESC;
      TEX_opgrp = '<';
      TEX_clgrp = '>';
    }
  rewind (inf);
}

/* Read environment and prepend it to the default string.
   Build token table. */
struct TEX_tabent *
TEX_decode_env (evarname, defenv)
     char *evarname;
     char *defenv;
{
  register char *env, *p;

  struct TEX_tabent *tab;
  int size, i;

  /* Append default string to environment. */
  env = getenv (evarname);
  if (!env)
    env = defenv;
  else
    env = concat (env, defenv, "");

  /* Allocate a token table */
  for (size = 1, p = env; p;)
    if ((p = etags_strchr (p, ':')) && *(++p))
      size++;
  /* Add 1 to leave room for null terminator.  */
  tab = xnew (size + 1, struct TEX_tabent);

  /* Unpack environment string into token table. Be careful about */
  /* zero-length strings (leading ':', "::" and trailing ':') */
  for (i = 0; *env;)
    {
      p = etags_strchr (env, ':');
      if (!p)			/* End of environment string. */
	p = env + strlen (env);
      if (p - env > 0)
	{			/* Only non-zero strings. */
	  tab[i].name = savenstr (env, p - env);
	  tab[i].len = strlen (tab[i].name);
	  i++;
	}
      if (*p)
	env = p + 1;
      else
	{
	  tab[i].name = NULL;	/* Mark end of table. */
	  tab[i].len = 0;
	  break;
	}
    }
  return tab;
}

/* Record a tag defined by a TeX command of length LEN and starting at NAME.
   The name being defined actually starts at (NAME + LEN + 1).
   But we seem to include the TeX command in the tag name.  */
void
TEX_getit (name, len)
     char *name;
     int len;
{
  char *p = name + len;

  if (*name == 0)
    return;

  /* Let tag name extend to next group close (or end of line) */
  while (*p && *p != TEX_clgrp)
    p++;
  pfnote (savenstr (name, p-name), TRUE, FALSE, lb.buffer,
	  strlen (lb.buffer), lineno, linecharno);
}

/* If the text at CP matches one of the tag-defining TeX command names,
   return the pointer to the first occurrence of that command in TEX_toktab.
   Otherwise return -1.
   Keep the capital `T' in `Token' for dumb truncating compilers
   (this distinguishes it from `TEX_toktab' */
int
TEX_Token (cp)
     char *cp;
{
  int i;

  for (i = 0; TEX_toktab[i].len > 0; i++)
    if (strneq (TEX_toktab[i].name, cp, TEX_toktab[i].len))
      return i;
  return -1;
}

/* Support for Prolog.  */

/* Whole head (not only functor, but also arguments)
   is gotten in compound term. */
void
prolog_getit (s)
     char *s;
{
  char *save_s;
  int insquote, npar;

  save_s = s;
  insquote = FALSE;
  npar = 0;
  while (1)
    {
      if (s[0] == '\0')		/* syntax error. */
	return;
      else if (insquote && s[0] == '\'' && s[1] == '\'')
	s += 2;
      else if (s[0] == '\'')
	{
	  insquote = !insquote;
	  s++;
	}
      else if (!insquote && s[0] == '(')
	{
	  npar++;
	  s++;
	}
      else if (!insquote && s[0] == ')')
	{
	  npar--;
	  s++;
	  if (npar == 0)
	    break;
	  else if (npar < 0)	/* syntax error. */
	    return;
	}
      else if (!insquote && s[0] == '.'
	       && (isspace (s[1]) || s[1] == '\0'))
	{			/* fullstop. */
	  if (npar != 0)	/* syntax error. */
	    return;
	  s++;
	  break;
	}
      else
	s++;
    }
  pfnote (savenstr (save_s, s-save_s), TRUE, FALSE,
	  save_s, s-save_s, lineno, linecharno);
}

/* It is assumed that prolog predicate starts from column 0. */
void
Prolog_functions (inf)
     FILE *inf;
{
  void skip_comment (), prolog_getit ();

  lineno = linecharno = charno = 0;
  while (!feof (inf))
    {
      lineno++;
      linecharno += charno;
      charno = readline (&lb, inf) + 1;	/* 1 for newline. */
      dbp = lb.buffer;
      if (isspace (dbp[0]))	/* not predicate header. */
	continue;
      else if (dbp[0] == '%')	/* comment. */
	continue;
      else if (dbp[0] == '/' && dbp[1] == '*')	/* comment. */
	skip_comment (&lb, inf, &lineno, &linecharno);
      else			/* found. */
	prolog_getit (dbp);
    }
}

void
skip_comment (plb, inf, plineno, plinecharno)
     struct linebuffer *plb;
     FILE *inf;
     int *plineno;		/* result */
     long *plinecharno;		/* result */
{
  char *cp;

  do
    {
      for (cp = plb->buffer; *cp != '\0'; cp++)
	if (cp[0] == '*' && cp[1] == '/')
	  return;
      (*plineno)++;
      *plinecharno += readline (plb, inf) + 1; /* 1 for newline. */
    }
  while (!feof(inf));
}

#ifdef ETAGS_REGEXPS
/* Take a string like "/blah/" and turn it into "blah", making sure
   that the first and last characters are the same, and handling
   quoted separator characters.  Actually, stops on the occurence of
   an unquoted separator.  Also turns "\t" into a Tab character.
   Returns pointer to terminating separator.  Works in place.  Null
   terminates name string. */
char *
scan_separators (name)
     char *name;
{
  char sep = name[0];
  char *copyto = name;
  logical quoted = FALSE;

  for (++name; *name != '\0'; ++name)
    {
      if (quoted)
	{
	  if (*name == 't')
	    *copyto++ = '\t';
	  else if (*name == sep)
	    *copyto++ = sep;
	  else
	    {
	      /* Something else is quoted, so preserve the quote. */
	      *copyto++ = '\\';
	      *copyto++ = *name;
	    }
	  quoted = FALSE;
	}
      else if (*name == '\\')
	quoted = TRUE;
      else if (*name == sep)
	break;
      else
	*copyto++ = *name;
    }

  /* Terminate copied string. */
  *copyto = '\0';
  return name;
}

/* Turn a name, which is an ed-style (but Emacs syntax) regular
   expression, into a real regular expression by compiling it. */
void
add_regex (regexp_pattern)
     char *regexp_pattern;
{
  char *name;
  const char *err;
  struct re_pattern_buffer *patbuf;

  if (regexp_pattern == NULL)
    {
      /* Remove existing regexps. */
      num_patterns = 0;
      patterns = NULL;
      return;
    }

  if (regexp_pattern[0] == '\0')
    {
      error ("missing regexp", 0);
      return;
    }
  if (regexp_pattern[strlen(regexp_pattern)-1] != regexp_pattern[0])
    {
      error ("%s: unterminated regexp", regexp_pattern);
      return;
    }
  name = scan_separators (regexp_pattern);
  if (regexp_pattern[0] == '\0')
    {
      error ("null regexp", 0);
      return;
    }
  (void) scan_separators (name);

  patbuf = xnew (1, struct re_pattern_buffer);
  patbuf->translate = NULL;
  patbuf->fastmap = NULL;
  patbuf->buffer = NULL;
  patbuf->allocated = 0;

  err = re_compile_pattern (regexp_pattern, strlen (regexp_pattern), patbuf);
  if (err != NULL)
    {
      error ("%s while compiling pattern", err);
      return;
    }

  num_patterns += 1;
  if (num_patterns == 1)
    patterns = xnew (1, struct pattern);
  else
    patterns = ((struct pattern *)
		xrealloc (patterns,
			  (num_patterns * sizeof (struct pattern))));
  patterns[num_patterns - 1].pattern = patbuf;
  patterns[num_patterns - 1].name_pattern = savestr (name);
  patterns[num_patterns - 1].error_signaled = FALSE;
}

/*
 * Do the subtitutions indicated by the regular expression and
 * arguments.
 */
char *
substitute (in, out, regs)
     char *in, *out;
     struct re_registers *regs;
{
  char *result = NULL, *t;
  int size = 0;

  /* Pass 1: figure out how much size to allocate. */
  for (t = out; *t; ++t)
    {
      if (*t == '\\')
	{
	  ++t;
	  if (!*t)
	    {
	      fprintf (stderr, "%s: pattern subtitution ends prematurely\n",
		       progname);
	      return NULL;
	    }
	  if (isdigit (*t))
	    {
	      int dig = *t - '0';
	      size += regs->end[dig] - regs->start[dig];
	    }
	}
    }

  /* Allocate space and do the substitutions. */
  result = xnew (size + 1, char);
  size = 0;
  for (; *out; ++out)
    {
      if (*out == '\\')
	{
	  ++out;
	  if (isdigit (*out))
	    {
	      /* Using "dig2" satisfies my debugger.  Bleah. */
	      int dig2 = *out - '0';
	      strncpy (result + size, in + regs->start[dig2],
		       regs->end[dig2] - regs->start[dig2]);
	      size += regs->end[dig2] - regs->start[dig2];
	    }
	  else
	    {
	      switch (*out)
		{
		case '\t':
		  result[size++] = '\t';
		  break;
		case '\\':
		  *out = '\\';
		  break;
		default:
		  result[size++] = *out;
		  break;
		}
	    }
	}
      else
	result[size++] = *out;
    }
  result[size] = '\0';

  return result;
}

#endif /* ETAGS_REGEXPS */
/* Initialize a linebuffer for use */
void
initbuffer (linebuffer)
     struct linebuffer *linebuffer;
{
  linebuffer->size = 200;
  linebuffer->buffer = xnew (200, char);
}

/*
 * Read a line of text from `stream' into `linebuffer'.
 * Return the number of characters read from `stream',
 * which is the length of the line including the newline, if any.
 */
long
readline_internal (linebuffer, stream)
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

  return p - buffer + chars_deleted;
}

/*
 * Like readline_internal, above, but try to match the input
 * line against any existing regular expressions.
 */
long
readline (linebuffer, stream)
     struct linebuffer *linebuffer;
     FILE *stream;
{
  /* Read new line. */
  int i;
  long result = readline_internal (linebuffer, stream);

#ifdef ETAGS_REGEXPS
  /* Match against all listed patterns. */
  for (i = 0; i < num_patterns; ++i)
    {
      int match = re_match (patterns[i].pattern, linebuffer->buffer,
			    (int)result, 0, &patterns[i].regs);
      switch (match)
	{
	case -2:
	  /* Some error. */
	  if (!patterns[i].error_signaled)
	    {
	      error ("error while matching pattern %d", i);
	      patterns[i].error_signaled = TRUE;
	    }
	  break;
	case -1:
	  /* No match. */
	  break;
	default:
	  /* Match occurred.  Construct a tag. */
	  if (patterns[i].name_pattern[0] != '\0')
	    {
	      /* Make a named tag. */
	      char *name = substitute (linebuffer->buffer,
				       patterns[i].name_pattern,
				       &patterns[i].regs);
	      if (name != NULL)
		pfnote (name, TRUE, TRUE, linebuffer->buffer,
			match, lineno, linecharno);
	    }
	  else
	    {
	      /* Make an unnamed tag. */
	      pfnote (NULL, TRUE, FALSE, linebuffer->buffer,
		      match, lineno, linecharno);
	    }
	  break;
	}
    }
#endif /* ETAGS_REGEXPS */

  return result;
}

/*
 * Read a file, but do no processing.  This is used to do regexp
 * matching on files that have no language defined.
 */
void
just_read_file (inf)
     FILE *inf;
{
  while (!feof (inf))
    {
      ++lineno;
      linecharno = charno;
      charno += readline (&lb, inf) + 1;
    }
}


/*
 * Return a pointer to a space of size strlen(cp)+1 allocated
 * with xnew where the string CP has been copied.
 */
char *
savestr (cp)
     char *cp;
{
  return savenstr (cp, strlen (cp));
}

/*
 * Return a pointer to a space of size LEN+1 allocated with xnew where
 * the string CP has been copied for at most the first LEN characters.
 */
char *
savenstr (cp, len)
     char *cp;
     int len;
{
  register char *dp;

  dp = xnew (len + 1, char);
  strncpy (dp, cp, len);
  dp[len] = '\0';
  return dp;
}

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 *
 * Identical to System V strrchr, included for portability.
 */
char *
etags_strrchr (sp, c)
     register char *sp, c;
{
  register char *r;

  r = NULL;
  do
    {
      if (*sp == c)
	r = sp;
  } while (*sp++);
  return r;
}


/*
 * Return the ptr in sp at which the character c first
 * appears; NULL if not found
 *
 * Identical to System V strchr, included for portability.
 */
char *
etags_strchr (sp, c)
     register char *sp, c;
{
  do
    {
      if (*sp == c)
	return sp;
    } while (*sp++);
  return NULL;
}

/* Print error message and exit.  */
void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (BAD);
}

void
pfatal (s1)
     char *s1;
{
  perror (s1);
  exit (BAD);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */
void
error (s1, s2)
     char *s1, *s2;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Return a newly-allocated string whose contents
   concatenate those of s1, s2, s3.  */
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

/* Does the same work as the system V getcwd, but does not need to
   guess buffer size in advance. */
char *
etags_getcwd ()
#ifdef DOS_NT
{
  char *p, path[MAXPATHLEN + 1]; /* Fixed size is safe on MSDOS.  */

  getwd (path);
  p = path;
  while (*p)
    if (*p == '\\')
      *p++ = '/';
    else
      *p++ = tolower (*p);

  return strdup (path);
}
#elif HAVE_GETCWD /* not DOS_NT */
{
  int bufsize = 200;
  char *path = xnew (bufsize, char);

  while (getcwd (path, bufsize) == NULL)
    {
      if (errno != ERANGE)
	pfatal ("pwd");
      bufsize *= 2;
      path = xnew (bufsize, char);
    }

  return path;
}
#else /* not DOS_NT and not HAVE_GETCWD */
{
  struct linebuffer path;
  FILE *pipe;

  initbuffer (&path);
  pipe = (FILE *) popen ("pwd 2>/dev/null", "r");
  if (pipe == NULL || readline_internal (&path, pipe) == 0)
    pfatal ("pwd");
  pclose (pipe);

  return path.buffer;
}
#endif /* not DOS_NT and not HAVE_GETCWD */

/* Return a newly allocated string containing the filename
   of FILE relative to the absolute directory DIR (which
   should end with a slash). */
char *
relative_filename (file, dir)
     char *file, *dir;
{
  char *fp, *dp, *res;

  /* Find the common root of file and dir. */
  fp = absolute_filename (file, cwd);
  dp = dir;
  while (*fp++ == *dp++)
    continue;
  do
    {
      fp--;
      dp--;
    }
  while (*fp != '/');

  /* Build a sequence of "../" strings for the resulting relative filename. */
  for (dp = etags_strchr (dp + 1, '/'), res = "";
       dp != NULL;
       dp = etags_strchr (dp + 1, '/'))
    {
      res = concat (res, "../", "");
    }

  /* Add the filename relative to the common root of file and dir. */
  res = concat (res, fp + 1, "");

  return res;			/* temporary stub */
}

/* Return a newly allocated string containing the
   absolute filename of FILE given CWD (which should
   end with a slash). */
char *
absolute_filename (file, cwd)
     char *file, *cwd;
{
  char *slashp, *cp, *res;

  if (file[0] == '/')
    res = concat (file, "", "");
  else
    res = concat (cwd, file, "");

  /* Delete the "/dirname/.." and "/." substrings. */
  slashp = etags_strchr (res, '/');
  while (slashp != NULL && slashp[0] != '\0')
    {
      if (slashp[1] == '.')
	{
	  if (slashp[2] == '.'
	      && (slashp[3] == '/' || slashp[3] == '\0'))
	    {
	      cp = slashp;
	      do
		cp--;
	      while (cp >= res && *cp != '/');
	      if (*cp == '/')
		{
		  strcpy (cp, slashp + 3);
		}
	      else		/* else (cp == res) */
		{
		  if (slashp[3] != '\0')
		    strcpy (cp, slashp + 4);
		  else
		    return ".";
		}
	      slashp = cp;
	      continue;
	    }
	  else if (slashp[2] == '/' || slashp[2] == '\0')
	    {
	      strcpy (slashp, slashp + 2);
	      continue;
	    }
	}

      slashp = etags_strchr (slashp + 1, '/');
    }

  return res;
}

/* Return a newly allocated string containing the absolute
   filename of dir where FILE resides given CWD (which should
   end with a slash). */
char *
absolute_dirname (file, cwd)
     char *file, *cwd;
{
  char *slashp, *res;
  char save;

  slashp = etags_strrchr (file, '/');
  if (slashp == NULL)
    return cwd;
  save = slashp[1];
  slashp[1] = '\0';
  res = absolute_filename (file, cwd);
  slashp[1] = save;

  return res;
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
