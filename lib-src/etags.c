/* Tags file maker to go with GNU Emacs           -*- coding: latin-1 -*-
   Copyright (C) 1984, 1987-1989, 1993-1995, 1998-2001, 2002
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
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*
 * Authors:
 *	Ctags originally by Ken Arnold.
 *	Fortran added by Jim Kleckner.
 *	Ed Pelegri-Llopart added C typedefs.
 *	Gnu Emacs TAGS format and modifications by RMS?
 * 1989	Sam Kendall added C++.
 * 1992 Joseph B. Wells improved C and C++ parsing.
 * 1993	Francesco Potortì reorganised C and C++.
 * 1994	Regexp tags by Tom Tromey.
 * 2001 Nested classes by Francesco Potortì (ideas by Mykola Dzyuba).
 *
 *	Francesco Potortì <pot@gnu.org> has maintained it since 1993.
 */

char pot_etags_version[] = "@(#) pot revision number is 15.10";

#define	TRUE	1
#define	FALSE	0

#ifdef DEBUG
#  undef DEBUG
#  define DEBUG TRUE
#else
#  define DEBUG  FALSE
#  define NDEBUG		/* disable assert */
#endif

#ifdef HAVE_CONFIG_H
# include <config.h>
  /* On some systems, Emacs defines static as nothing for the sake
     of unexec.  We don't want that here since we don't use unexec. */
# undef static
# define ETAGS_REGEXPS		/* use the regexp features */
# define LONG_OPTIONS		/* accept long options */
# ifndef PTR			/* for Xemacs */
#   define PTR void *
# endif
# ifndef __P			/* for Xemacs */
#   define __P(args) args
# endif
#else
# if defined(__STDC__) && (__STDC__ || defined(__SUNPRO_C))
#   define __P(args) args	/* use prototypes */
#   define PTR void *		/* for generic pointers */
# else
#   define __P(args) ()		/* no prototypes */
#   define const		/* remove const for old compilers' sake */
#   define PTR long *		/* don't use void* */
# endif
#endif /* !HAVE_CONFIG_H */

#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1		/* enables some compiler checks on GNU */
#endif

/* WIN32_NATIVE is for Xemacs.
   MSDOS, WINDOWSNT, DOS_NT are for Emacs. */
#ifdef WIN32_NATIVE
# undef MSDOS
# undef  WINDOWSNT
# define WINDOWSNT
#endif /* WIN32_NATIVE */

#ifdef MSDOS
# undef MSDOS
# define MSDOS TRUE
# include <fcntl.h>
# include <sys/param.h>
# include <io.h>
# ifndef HAVE_CONFIG_H
#   define DOS_NT
#   include <sys/config.h>
# endif
#else
# define MSDOS FALSE
#endif /* MSDOS */

#ifdef WINDOWSNT
# include <stdlib.h>
# include <fcntl.h>
# include <string.h>
# include <direct.h>
# include <io.h>
# define MAXPATHLEN _MAX_PATH
# undef HAVE_NTGUI
# undef  DOS_NT
# define DOS_NT
# ifndef HAVE_GETCWD
#   define HAVE_GETCWD
# endif /* undef HAVE_GETCWD */
#else /* !WINDOWSNT */
# ifdef STDC_HEADERS
#  include <stdlib.h>
#  include <string.h>
# else
    extern char *getenv ();
# endif
#endif /* !WINDOWSNT */

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# if defined (HAVE_GETCWD) && !defined (WINDOWSNT)
    extern char *getcwd (char *buf, size_t size);
# endif
#endif /* HAVE_UNISTD_H */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#ifndef errno
  extern int errno;
#endif
#include <sys/types.h>
#include <sys/stat.h>

#include <assert.h>
#ifdef NDEBUG
# undef  assert			/* some systems have a buggy assert.h */
# define assert(x) ((void) 0)
#endif

#if !defined (S_ISREG) && defined (S_IFREG)
# define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#endif

#ifdef LONG_OPTIONS
# include <getopt.h>
#else
# define getopt_long(argc,argv,optstr,lopts,lind) getopt (argc, argv, optstr)
  extern char *optarg;
  extern int optind, opterr;
#endif /* LONG_OPTIONS */

#ifdef ETAGS_REGEXPS
# ifndef HAVE_CONFIG_H		/* this is a standalone compilation */
#   ifdef __CYGWIN__         	/* compiling on Cygwin */
			     !!! NOTICE !!!
 the regex.h distributed with Cygwin is not compatible with etags, alas!
If you want regular expression support, you should delete this notice and
	      arrange to use the GNU regex.h and regex.c.
#   endif
# endif
# include <regex.h>
#endif /* ETAGS_REGEXPS */

/* Define CTAGS to make the program "ctags" compatible with the usual one.
 Leave it undefined to make the program "etags", which makes emacs-style
 tag tables and tags typedefs, #defines and struct/union/enum by default. */
#ifdef CTAGS
# undef  CTAGS
# define CTAGS TRUE
#else
# define CTAGS FALSE
#endif

/* Exit codes for success and failure.  */
#ifdef VMS
# define	GOOD	1
# define	BAD	0
#else
# define	GOOD	0
# define	BAD	1
#endif

#define streq(s,t)	(assert((s)!=NULL || (t)!=NULL), !strcmp (s, t))
#define strneq(s,t,n)	(assert((s)!=NULL || (t)!=NULL), !strncmp (s, t, n))

#define CHARS 256		/* 2^sizeof(char) */
#define CHAR(x)		((unsigned int)(x) & (CHARS - 1))
#define	iswhite(c)	(_wht[CHAR(c)]) /* c is white */
#define notinname(c)	(_nin[CHAR(c)]) /* c is not in a name */
#define	begtoken(c)	(_btk[CHAR(c)]) /* c can start token */
#define	intoken(c)	(_itk[CHAR(c)]) /* c can be in token */
#define	endtoken(c)	(_etk[CHAR(c)]) /* c ends tokens */

#define ISALNUM(c)	isalnum (CHAR(c))
#define ISALPHA(c)	isalpha (CHAR(c))
#define ISDIGIT(c)	isdigit (CHAR(c))
#define ISLOWER(c)	islower (CHAR(c))

#define lowcase(c)	tolower (CHAR(c))
#define upcase(c)	toupper (CHAR(c))


/*
 *	xnew, xrnew -- allocate, reallocate storage
 *
 * SYNOPSIS:	Type *xnew (int n, Type);
 *		void xrnew (OldPointer, int n, Type);
 */
#if DEBUG
# include "chkmalloc.h"
# define xnew(n,Type)	  ((Type *) trace_malloc (__FILE__, __LINE__, \
						  (n) * sizeof (Type)))
# define xrnew(op,n,Type) ((op) = (Type *) trace_realloc (__FILE__, __LINE__, \
					(char *) (op), (n) * sizeof (Type)))
#else
# define xnew(n,Type)	  ((Type *) xmalloc ((n) * sizeof (Type)))
# define xrnew(op,n,Type) ((op) = (Type *) xrealloc ( \
					(char *) (op), (n) * sizeof (Type)))
#endif

#define bool int

typedef void Lang_function __P((FILE *));

typedef struct
{
  char *suffix;			/* file name suffix for this compressor */
  char *command;		/* takes one arg and decompresses to stdout */
} compressor;

typedef struct
{
  char *name;			/* language name */
  bool metasource;		/* source used to generate other sources */
  Lang_function *function;	/* parse function */
  char **filenames;		/* names of this language's files */
  char **suffixes;		/* name suffixes of this language's files */
  char **interpreters;		/* interpreters for this language */
} language;

typedef struct fdesc
{
  struct fdesc *next;		/* for the linked list */
  char *infname;		/* uncompressed input file name */
  char *infabsname;		/* absolute uncompressed input file name */
  char *infabsdir;		/* absolute dir of input file */
  char *taggedfname;		/* file name to write in tagfile */
  language *lang;		/* language of file */
  char *prop;			/* file properties to write in tagfile */
  bool usecharno;		/* etags tags shall contain char number */
} fdesc;

typedef struct node_st
{				/* sorting structure */
  struct node_st *left, *right;	/* left and right sons */
  fdesc *fdp;			/* description of file to whom tag belongs */
  char *name;			/* tag name */
  char *pat;			/* search pattern */
  bool valid;			/* write this tag on the tag file */
  bool is_func;			/* function tag: use pattern in CTAGS mode */
  bool been_warned;		/* warning already given for duplicated tag */
  int lno;			/* line number tag is on */
  long cno;			/* character number line starts on */
} node;

/*
 * A `linebuffer' is a structure which holds a line of text.
 * `readline_internal' reads a line from a stream into a linebuffer
 * and works regardless of the length of the line.
 * SIZE is the size of BUFFER, LEN is the length of the string in
 * BUFFER after readline reads it.
 */
typedef struct
{
  long size;
  int len;
  char *buffer;
} linebuffer;

/* Used to support mixing of --lang and file names. */
typedef struct
{
  enum {
    at_language,		/* a language specification */
    at_regexp,			/* a regular expression */
    at_icregexp,		/* same, but with case ignored */
    at_filename			/* a file name */
  } arg_type;			/* argument type */
  language *lang;		/* language associated with the argument */
  char *what;			/* the argument itself */
} argument;

#ifdef ETAGS_REGEXPS
/* Structure defining a regular expression. */
typedef struct pattern
{
  struct pattern *p_next;
  language *lang;
  char *regex;
  struct re_pattern_buffer *pat;
  struct re_registers regs;
  char *name_pattern;
  bool error_signaled;
  bool ignore_case;
} pattern;
#endif /* ETAGS_REGEXPS */


/* Many compilers barf on this:
	Lang_function Ada_funcs;
   so let's write it this way */
static void Ada_funcs __P((FILE *));
static void Asm_labels __P((FILE *));
static void C_entries __P((int c_ext, FILE *));
static void default_C_entries __P((FILE *));
static void plain_C_entries __P((FILE *));
static void Cjava_entries __P((FILE *));
static void Cobol_paragraphs __P((FILE *));
static void Cplusplus_entries __P((FILE *));
static void Cstar_entries __P((FILE *));
static void Erlang_functions __P((FILE *));
static void Fortran_functions __P((FILE *));
static void Yacc_entries __P((FILE *));
static void Lisp_functions __P((FILE *));
static void Makefile_targets __P((FILE *));
static void Pascal_functions __P((FILE *));
static void Perl_functions __P((FILE *));
static void PHP_functions __P((FILE *));
static void Postscript_functions __P((FILE *));
static void Prolog_functions __P((FILE *));
static void Python_functions __P((FILE *));
static void Scheme_functions __P((FILE *));
static void TeX_commands __P((FILE *));
static void Texinfo_nodes __P((FILE *));
static void just_read_file __P((FILE *));

static void print_language_names __P((void));
static void print_version __P((void));
static void print_help __P((void));
int main __P((int, char **));

static compressor *get_compressor_from_suffix __P((char *, char **));
static language *get_language_from_langname __P((const char *));
static language *get_language_from_interpreter __P((char *));
static language *get_language_from_filename __P((char *));
static long readline __P((linebuffer *, FILE *));
static long readline_internal __P((linebuffer *, FILE *));
static bool nocase_tail __P((char *));
static char *get_tag __P((char *));

#ifdef ETAGS_REGEXPS
static void analyse_regex __P((char *, bool));
static void add_regex __P((char *, bool, language *));
static void free_patterns __P((void));
#endif /* ETAGS_REGEXPS */
static void error __P((const char *, const char *));
static void suggest_asking_for_help __P((void));
void fatal __P((char *, char *));
static void pfatal __P((char *));
static void add_node __P((node *, node **));

static void init __P((void));
static void initbuffer __P((linebuffer *));
static void process_file __P((char *, language *));
static void find_entries __P((FILE *));
static void free_tree __P((node *));
static void pfnote __P((char *, bool, char *, int, int, long));
static void new_pfnote __P((char *, int, bool, char *, int, int, long));
static void invalidate_nodes __P((fdesc *, node *));
static void put_entries __P((node *));

static char *concat __P((char *, char *, char *));
static char *skip_spaces __P((char *));
static char *skip_non_spaces __P((char *));
static char *savenstr __P((char *, int));
static char *savestr __P((char *));
static char *etags_strchr __P((const char *, int));
static char *etags_strrchr __P((const char *, int));
static char *etags_getcwd __P((void));
static char *relative_filename __P((char *, char *));
static char *absolute_filename __P((char *, char *));
static char *absolute_dirname __P((char *, char *));
static bool filename_is_absolute __P((char *f));
static void canonicalize_filename __P((char *));
static void linebuffer_setlen __P((linebuffer *, int));
static PTR xmalloc __P((unsigned int));
static PTR xrealloc __P((char *, unsigned int));


static char searchar = '/';	/* use /.../ searches */

static char *tagfile;		/* output file */
static char *progname;		/* name this program was invoked with */
static char *cwd;		/* current working directory */
static char *tagfiledir;	/* directory of tagfile */
static FILE *tagf;		/* ioptr for tags file */

static fdesc *fdhead;		/* head of file description list */
static fdesc *curfdp;		/* current file description */
static int lineno;		/* line number of current line */
static long charno;		/* current character number */
static long linecharno;		/* charno of start of current line */
static char *dbp;		/* pointer to start of current tag */

static const int invalidcharno = -1;

static node *nodehead;		/* the head of the binary tree of tags */

static linebuffer lb;		/* the current line */

/* boolean "functions" (see init)	*/
static bool _wht[CHARS], _nin[CHARS], _itk[CHARS], _btk[CHARS], _etk[CHARS];
static char
  /* white chars */
  *white = " \f\t\n\r\v",
  /* not in a name */
  *nonam = " \f\t\n\r()=,;",
  /* token ending chars */
  *endtk = " \t\n\r\"'#()[]{}=-+%*/&|^~!<>;,.:?",
  /* token starting chars */
  *begtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$~@",
  /* valid in-token chars */
  *midtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$0123456789";

static bool append_to_tagfile;	/* -a: append to tags */
/* The next four default to TRUE for etags, but to FALSE for ctags.  */
static bool typedefs;		/* -t: create tags for C and Ada typedefs */
static bool typedefs_or_cplusplus; /* -T: create tags for C typedefs, level */
				/* 0 struct/enum/union decls, and C++ */
				/* member functions. */
static bool constantypedefs;	/* -d: create tags for C #define, enum */
				/* constants and variables. */
				/* -D: opposite of -d.  Default under ctags. */
static bool globals;		/* create tags for global variables */
static bool declarations;	/* --declarations: tag them and extern in C&Co*/
static bool members;		/* create tags for C member variables */
static bool no_line_directive;	/* ignore #line directives */
static bool update;		/* -u: update tags */
static bool vgrind_style;	/* -v: create vgrind style index output */
static bool no_warnings;	/* -w: suppress warnings */
static bool cxref_style;	/* -x: create cxref style output */
static bool cplusplus;		/* .[hc] means C++, not C */
static bool noindentypedefs;	/* -I: ignore indentation in C */
static bool packages_only;	/* --packages-only: in Ada, only tag packages*/

#ifdef ETAGS_REGEXPS
/* List of all regexps. */
static pattern *p_head;

/* How many characters in the character set.  (From regex.c.)  */
#define CHAR_SET_SIZE 256
/* Translation table for case-insensitive matching. */
static char lc_trans[CHAR_SET_SIZE];
#endif /* ETAGS_REGEXPS */

#ifdef LONG_OPTIONS
static struct option longopts[] =
{
  { "packages-only",      no_argument,	     &packages_only, 	 TRUE  },
  { "c++",		  no_argument,	     NULL,	     	 'C'   },
  { "declarations",	  no_argument,	     &declarations,  	 TRUE  },
  { "no-line-directive",  no_argument,	     &no_line_directive, TRUE  },
  { "help",		  no_argument,	     NULL,     	     	 'h'   },
  { "help",		  no_argument,	     NULL,     	     	 'H'   },
  { "ignore-indentation", no_argument,	     NULL,     	     	 'I'   },
  { "language",           required_argument, NULL,     	     	 'l'   },
  { "members",		  no_argument,	     &members, 	     	 TRUE  },
  { "no-members",	  no_argument,	     &members, 	     	 FALSE },
  { "output",		  required_argument, NULL,	     	 'o'   },
#ifdef ETAGS_REGEXPS
  { "regex",		  required_argument, NULL,	     	 'r'   },
  { "no-regex",		  no_argument,	     NULL,	     	 'R'   },
  { "ignore-case-regex",  required_argument, NULL,	     	 'c'   },
#endif /* ETAGS_REGEXPS */
  { "version",		  no_argument,	     NULL,     	     	 'V'   },

#if CTAGS /* Etags options */
  { "backward-search",	  no_argument,	     NULL,	     	 'B'   },
  { "cxref",		  no_argument,	     NULL,	     	 'x'   },
  { "defines",		  no_argument,	     NULL,	     	 'd'   },
  { "globals",		  no_argument,	     &globals, 	     	 TRUE  },
  { "typedefs",		  no_argument,	     NULL,	     	 't'   },
  { "typedefs-and-c++",	  no_argument,	     NULL,     	     	 'T'   },
  { "update",		  no_argument,	     NULL,     	     	 'u'   },
  { "vgrind",		  no_argument,	     NULL,     	     	 'v'   },
  { "no-warn",		  no_argument,	     NULL,	     	 'w'   },

#else /* Ctags options */
  { "append",		  no_argument,	     NULL,	     	 'a'   },
  { "no-defines",	  no_argument,	     NULL,	     	 'D'   },
  { "no-globals",	  no_argument,	     &globals, 	     	 FALSE },
  { "include",		  required_argument, NULL,     	     	 'i'   },
#endif
  { NULL }
};
#endif /* LONG_OPTIONS */

static compressor compressors[] =
{
  { "z", "gzip -d -c"},
  { "Z", "gzip -d -c"},
  { "gz", "gzip -d -c"},
  { "GZ", "gzip -d -c"},
  { "bz2", "bzip2 -d -c" },
  { NULL }
};

/*
 * Language stuff.
 */

/* Ada code */
static char *Ada_suffixes [] =
  { "ads", "adb", "ada", NULL };

/* Assembly code */
static char *Asm_suffixes [] =
  { "a",	/* Unix assembler */
    "asm", /* Microcontroller assembly */
    "def", /* BSO/Tasking definition includes  */
    "inc", /* Microcontroller include files */
    "ins", /* Microcontroller include files */
    "s", "sa", /* Unix assembler */
    "S",   /* cpp-processed Unix assembler */
    "src", /* BSO/Tasking C compiler output */
    NULL
  };

/* Note that .c and .h can be considered C++, if the --c++ flag was
   given, or if the `class' keyowrd is met inside the file.
   That is why default_C_entries is called for these. */
static char *default_C_suffixes [] =
  { "c", "h", NULL };

static char *Cplusplus_suffixes [] =
  { "C", "c++", "cc", "cpp", "cxx", "H", "h++", "hh", "hpp", "hxx",
    "M",			/* Objective C++ */
    "pdb",			/* Postscript with C syntax */
    NULL };

static char *Cjava_suffixes [] =
  { "java", NULL };

static char *Cobol_suffixes [] =
  { "COB", "cob", NULL };

static char *Cstar_suffixes [] =
  { "cs", "hs", NULL };

static char *Erlang_suffixes [] =
  { "erl", "hrl", NULL };

static char *Fortran_suffixes [] =
  { "F", "f", "f90", "for", NULL };

static char *Lisp_suffixes [] =
  { "cl", "clisp", "el", "l", "lisp", "LSP", "lsp", "ml", NULL };

static char *Makefile_filenames [] =
  { "Makefile", "makefile", "GNUMakefile", "Makefile.in", "Makefile.am", NULL};

static char *Pascal_suffixes [] =
  { "p", "pas", NULL };

static char *Perl_suffixes [] =
  { "pl", "pm", NULL };

static char *Perl_interpreters [] =
  { "perl", "@PERL@", NULL };

static char *PHP_suffixes [] =
  { "php", "php3", "php4", NULL };

static char *plain_C_suffixes [] =
  { "lm",			/* Objective lex file */
    "m",			/* Objective C file */
    "pc",			/* Pro*C file */
     NULL };

static char *Postscript_suffixes [] =
  { "ps", "psw", NULL };	/* .psw is for PSWrap */

static char *Prolog_suffixes [] =
  { "prolog", NULL };

static char *Python_suffixes [] =
  { "py", NULL };

/* Can't do the `SCM' or `scm' prefix with a version number. */
static char *Scheme_suffixes [] =
  { "oak", "sch", "scheme", "SCM", "scm", "SM", "sm", "ss", "t", NULL };

static char *TeX_suffixes [] =
  { "bib", "clo", "cls", "ltx", "sty", "TeX", "tex", NULL };

static char *Texinfo_suffixes [] =
  { "texi", "texinfo", "txi", NULL };

static char *Yacc_suffixes [] =
  { "y", "y++", "ym", "yxx", "yy", NULL }; /* .ym is Objective yacc file */

/*
 * Table of languages.
 *
 * It is ok for a given function to be listed under more than one
 * name.  I just didn't.
 */

static language lang_names [] =
{
  { "ada",      FALSE, Ada_funcs,            NULL, Ada_suffixes,        NULL },
  { "asm",      FALSE, Asm_labels,           NULL, Asm_suffixes,        NULL },
  { "c",        FALSE, default_C_entries,    NULL, default_C_suffixes,  NULL },
  { "c++",      FALSE, Cplusplus_entries,    NULL, Cplusplus_suffixes,  NULL },
  { "c*",       FALSE, Cstar_entries,        NULL, Cstar_suffixes,      NULL },
  { "cobol",    FALSE, Cobol_paragraphs,     NULL, Cobol_suffixes,      NULL },
  { "erlang",   FALSE, Erlang_functions,     NULL, Erlang_suffixes,     NULL },
  { "fortran",  FALSE, Fortran_functions,    NULL, Fortran_suffixes,    NULL },
  { "java",     FALSE, Cjava_entries,        NULL, Cjava_suffixes,      NULL },
  { "lisp",     FALSE, Lisp_functions,       NULL, Lisp_suffixes,       NULL },
  { "makefile", FALSE, Makefile_targets,     Makefile_filenames, NULL,  NULL },
  { "pascal",   FALSE, Pascal_functions,     NULL, Pascal_suffixes,     NULL },
  { "perl",     FALSE, Perl_functions,NULL, Perl_suffixes, Perl_interpreters },
  { "php",      FALSE, PHP_functions,        NULL, PHP_suffixes,        NULL },
  { "postscript",FALSE, Postscript_functions,NULL, Postscript_suffixes, NULL },
  { "proc",     FALSE, plain_C_entries,      NULL, plain_C_suffixes,    NULL },
  { "prolog",   FALSE, Prolog_functions,     NULL, Prolog_suffixes,     NULL },
  { "python",   FALSE, Python_functions,     NULL, Python_suffixes,     NULL },
  { "scheme",   FALSE, Scheme_functions,     NULL, Scheme_suffixes,     NULL },
  { "tex",      FALSE, TeX_commands,         NULL, TeX_suffixes,        NULL },
  { "texinfo",  FALSE, Texinfo_nodes,        NULL, Texinfo_suffixes,    NULL },
  { "yacc",      TRUE, Yacc_entries,         NULL, Yacc_suffixes,       NULL },
  { "auto", FALSE, NULL },             /* default guessing scheme */
  { "none", FALSE, just_read_file },   /* regexp matching only */
  { NULL, FALSE, NULL }                /* end of list */
};


static void
print_language_names ()
{
  language *lang;
  char **name, **ext;

  puts ("\nThese are the currently supported languages, along with the\n\
default file names and dot suffixes:");
  for (lang = lang_names; lang->name != NULL; lang++)
    {
      printf ("  %-*s", 10, lang->name);
      if (lang->filenames != NULL)
	for (name = lang->filenames; *name != NULL; name++)
	  printf (" %s", *name);
      if (lang->suffixes != NULL)
	for (ext = lang->suffixes; *ext != NULL; ext++)
	  printf (" .%s", *ext);
      puts ("");
    }
  puts ("Where `auto' means use default language for files based on file\n\
name suffix, and `none' means only do regexp processing on files.\n\
If no language is specified and no matching suffix is found,\n\
the first line of the file is read for a sharp-bang (#!) sequence\n\
followed by the name of an interpreter.  If no such sequence is found,\n\
Fortran is tried first; if no tags are found, C is tried next.\n\
When parsing any C file, a \"class\" keyword switches to C++.\n\
Compressed files are supported using gzip and bzip2.");
}

#ifndef EMACS_NAME
# define EMACS_NAME "GNU Emacs"
#endif
#ifndef VERSION
# define VERSION "21"
#endif
static void
print_version ()
{
  printf ("%s (%s %s)\n", (CTAGS) ? "ctags" : "etags", EMACS_NAME, VERSION);
  puts ("Copyright (C) 2002 Free Software Foundation, Inc. and Ken Arnold");
  puts ("This program is distributed under the same terms as Emacs");

  exit (GOOD);
}

static void
print_help ()
{
  printf ("Usage: %s [options] [[regex-option ...] file-name] ...\n\
\n\
These are the options accepted by %s.\n", progname, progname);
#ifdef LONG_OPTIONS
  puts ("You may use unambiguous abbreviations for the long option names.");
#else
  puts ("Long option names do not work with this executable, as it is not\n\
linked with GNU getopt.");
#endif /* LONG_OPTIONS */
  puts ("  A - as file name means read names from stdin (one per line).\n\
Absolute names are stored in the output file as they are.\n\
Relative ones are stored relative to the output file's directory.\n");

  if (!CTAGS)
    puts ("-a, --append\n\
        Append tag entries to existing tags file.");

  puts ("--packages-only\n\
        For Ada files, only generate tags for packages.");

  if (CTAGS)
    puts ("-B, --backward-search\n\
        Write the search commands for the tag entries using '?', the\n\
        backward-search command instead of '/', the forward-search command.");

  /* This option is mostly obsolete, because etags can now automatically
     detect C++.  Retained for backward compatibility and for debugging and
     experimentation.  In principle, we could want to tag as C++ even
     before any "class" keyword.
  puts ("-C, --c++\n\
        Treat files whose name suffix defaults to C language as C++ files.");
  */

  puts ("--declarations\n\
	In C and derived languages, create tags for function declarations,");
  if (CTAGS)
    puts ("\tand create tags for extern variables if --globals is used.");
  else
    puts
      ("\tand create tags for extern variables unless --no-globals is used.");

  if (CTAGS)
    puts ("-d, --defines\n\
        Create tag entries for C #define constants and enum constants, too.");
  else
    puts ("-D, --no-defines\n\
        Don't create tag entries for C #define constants and enum constants.\n\
	This makes the tags file smaller.");

  if (!CTAGS)
    puts ("-i FILE, --include=FILE\n\
        Include a note in tag file indicating that, when searching for\n\
        a tag, one should also consult the tags file FILE after\n\
        checking the current file.");

  puts ("-l LANG, --language=LANG\n\
        Force the following files to be considered as written in the\n\
	named language up to the next --language=LANG option.");

  if (CTAGS)
    puts ("--globals\n\
	Create tag entries for global variables in some languages.");
  else
    puts ("--no-globals\n\
	Do not create tag entries for global variables in some\n\
	languages.  This makes the tags file smaller.");
  puts ("--members\n\
	Create tag entries for member variables in C and derived languages.");

#ifdef ETAGS_REGEXPS
  puts ("-r /REGEXP/, --regex=/REGEXP/ or --regex=@regexfile\n\
        Make a tag for each line matching pattern REGEXP in the following\n\
 	files.  {LANGUAGE}/REGEXP/ uses REGEXP for LANGUAGE files only.\n\
	regexfile is a file containing one REGEXP per line.\n\
	REGEXP is anchored (as if preceded by ^).\n\
	The form /REGEXP/NAME/ creates a named tag.\n\
	For example Tcl named tags can be created with:\n\
	--regex=\"/proc[ \\t]+\\([^ \\t]+\\)/\\1/.\"");
  puts ("-c /REGEXP/, --ignore-case-regex=/REGEXP/ or --ignore-case-regex=@regexfile\n\
        Like -r, --regex but ignore case when matching expressions.");
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
        Generate tag entries for C and Ada typedefs.");
      puts ("-T, --typedefs-and-c++\n\
        Generate tag entries for C typedefs, C struct/enum/union tags,\n\
        and C++ member functions.");
    }

  if (CTAGS)
    puts ("-u, --update\n\
        Update the tag entries for the given files, leaving tag\n\
        entries for other files in place.  Currently, this is\n\
        implemented by deleting the existing entries for the given\n\
        files and then rewriting the new entries at the end of the\n\
        tags file.  It is often faster to simply rebuild the entire\n\
        tag file than to use this.");

  if (CTAGS)
    {
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

  puts ("");
  puts ("Report bugs to bug-gnu-emacs@gnu.org");

  exit (GOOD);
}


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
 returning in each successive call the next file name matching the input
 spec. The function expects that each in_spec passed
 to it will be processed to completion; in particular, up to and
 including the call following that in which the last matching name
 is returned, the function ignores the value of in_spec, and will
 only start processing a new spec with the following call.
 If an error occurs, on return out_spec contains the value
 of in_spec when the error occurred.

 With each successive file name returned in out_spec, the
 function's return value is one. When there are no more matching
 names the function returns zero. If on the first call no file
 matches in_spec, or there is any other error, -1 is returned.
*/

#include	<rmsdef.h>
#include	<descrip.h>
#define		OUTSIZE	MAX_FILE_SPEC_LEN
static short
fn_exp (out, in)
     vspec *out;
     char *in;
{
  static long context = 0;
  static struct dsc$descriptor_s o;
  static struct dsc$descriptor_s i;
  static bool pass1 = TRUE;
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
static char *
gfnames (arg, p_error)
     char *arg;
     bool *p_error;
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
  error ("%s", "system() function not implemented under VMS");
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
      *s = lowcase (*s);
  return start;
}
#endif /* VMS */


int
main (argc, argv)
     int argc;
     char *argv[];
{
  int i;
  unsigned int nincluded_files;
  char **included_files;
  argument *argbuffer;
  int current_arg, file_count;
  linebuffer filename_lb;
#ifdef VMS
  bool got_err;
#endif

#ifdef DOS_NT
  _fmode = O_BINARY;   /* all of files are treated as binary files */
#endif /* DOS_NT */

  progname = argv[0];
  nincluded_files = 0;
  included_files = xnew (argc, char *);
  current_arg = 0;
  file_count = 0;

  /* Allocate enough no matter what happens.  Overkill, but each one
     is small. */
  argbuffer = xnew (argc, argument);

#ifdef ETAGS_REGEXPS
  /* Set syntax for regular expression routines. */
  re_set_syntax (RE_SYNTAX_EMACS | RE_INTERVALS);
  /* Translation table for case-insensitive search. */
  for (i = 0; i < CHAR_SET_SIZE; i++)
    lc_trans[i] = lowcase (i);
#endif /* ETAGS_REGEXPS */

  /*
   * If etags, always find typedefs and structure tags.  Why not?
   * Also default to find macro constants, enum constants and
   * global variables.
   */
  if (!CTAGS)
    {
      typedefs = typedefs_or_cplusplus = constantypedefs = TRUE;
      globals = TRUE;
    }

  while (1)
    {
      int opt;
      char *optstring = "-";

#ifdef ETAGS_REGEXPS
      optstring = "-r:Rc:";
#endif /* ETAGS_REGEXPS */

#ifndef LONG_OPTIONS
      optstring = optstring + 1;
#endif /* LONG_OPTIONS */

      optstring = concat (optstring,
			  "Cf:Il:o:SVhH",
			  (CTAGS) ? "BxdtTuvw" : "aDi:");

      opt = getopt_long (argc, argv, optstring, longopts, 0);
      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  /* If getopt returns 0, then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	case 1:
	  /* This means that a file name has been seen.  Record it. */
	  argbuffer[current_arg].arg_type = at_filename;
	  argbuffer[current_arg].what = optarg;
	  ++current_arg;
	  ++file_count;
	  break;

	  /* Common options. */
	case 'C': cplusplus = TRUE;		break;
	case 'f':		/* for compatibility with old makefiles */
	case 'o':
	  if (tagfile)
	    {
	      error ("-o option may only be given once.", (char *)NULL);
	      suggest_asking_for_help ();
	    }
	  tagfile = optarg;
	  break;
	case 'I':
	case 'S':		/* for backward compatibility */
	  noindentypedefs = TRUE;
	  break;
	case 'l':
	  {
	    language *lang = get_language_from_langname (optarg);
	    if (lang != NULL)
	      {
		argbuffer[current_arg].lang = lang;
		argbuffer[current_arg].arg_type = at_language;
		++current_arg;
	      }
	  }
	  break;
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
        case 'c':
	  argbuffer[current_arg].arg_type = at_icregexp;
	  argbuffer[current_arg].what = optarg;
	  ++current_arg;
	  break;
	case 'V':
	  print_version ();
	  break;
	case 'h':
	case 'H':
	  print_help ();
	  break;

	  /* Etags options */
	case 'a': append_to_tagfile = TRUE;			break;
	case 'D': constantypedefs = FALSE;			break;
	case 'i': included_files[nincluded_files++] = optarg;	break;

	  /* Ctags options. */
	case 'B': searchar = '?';				break;
	case 'd': constantypedefs = TRUE;			break;
	case 't': typedefs = TRUE;				break;
	case 'T': typedefs = typedefs_or_cplusplus = TRUE;	break;
	case 'u': update = TRUE;				break;
	case 'v': vgrind_style = TRUE;			  /*FALLTHRU*/
	case 'x': cxref_style = TRUE;				break;
	case 'w': no_warnings = TRUE;				break;
	default:
	  suggest_asking_for_help ();
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
      error ("no input files specified.", (char *)NULL);
      suggest_asking_for_help ();
    }

  if (tagfile == NULL)
    tagfile = CTAGS ? "tags" : "TAGS";
  cwd = etags_getcwd ();	/* the current working directory */
  if (cwd[strlen (cwd) - 1] != '/')
    {
      char *oldcwd = cwd;
      cwd = concat (oldcwd, "/", "");
      free (oldcwd);
    }
  if (streq (tagfile, "-"))
    tagfiledir = cwd;
  else
    tagfiledir = absolute_dirname (tagfile, cwd);

  init ();			/* set up boolean "functions" */

  initbuffer (&lb);
  initbuffer (&filename_lb);

  if (!CTAGS)
    {
      if (streq (tagfile, "-"))
	{
	  tagf = stdout;
#ifdef DOS_NT
	  /* Switch redirected `stdout' to binary mode (setting `_fmode'
	     doesn't take effect until after `stdout' is already open). */
	  if (!isatty (fileno (stdout)))
	    setmode (fileno (stdout), O_BINARY);
#endif /* DOS_NT */
	}
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
      static language *lang;	/* non-NULL if language is forced */
      char *this_file;

      switch (argbuffer[i].arg_type)
	{
	case at_language:
	  lang = argbuffer[i].lang;
	  break;
#ifdef ETAGS_REGEXPS
	case at_regexp:
	  analyse_regex (argbuffer[i].what, FALSE);
	  break;
	case at_icregexp:
	  analyse_regex (argbuffer[i].what, TRUE);
	  break;
#endif
	case at_filename:
#ifdef VMS
	  while ((this_file = gfnames (argbuffer[i].what, &got_err)) != NULL)
	    {
	      if (got_err)
		{
		  error ("can't find file %s\n", this_file);
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
		 (one per line) and use them. */
	      if (streq (this_file, "-"))
		while (readline_internal (&filename_lb, stdin) > 0)
		  process_file (filename_lb.buffer, lang);
	      else
		process_file (this_file, lang);
#ifdef VMS
	    }
#endif
	  break;
	}
    }

#ifdef ETAGS_REGEXPS
  free_patterns ();
#endif /* ETAGS_REGEXPS */

  if (!CTAGS || cxref_style)
    {
      put_entries (nodehead);
      free_tree (nodehead);
      nodehead = NULL;
      if (!CTAGS)
	while (nincluded_files-- > 0)
	  fprintf (tagf, "\f\n%s,include\n", *included_files++);

      if (fclose (tagf) == EOF)
	pfatal (tagfile);
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
	    fatal ("failed to execute shell command", (char *)NULL);
	}
      append_to_tagfile = TRUE;
    }

  tagf = fopen (tagfile, append_to_tagfile ? "a" : "w");
  if (tagf == NULL)
    pfatal (tagfile);
  put_entries (nodehead);
  free_tree (nodehead);
  nodehead = NULL;
  if (fclose (tagf) == EOF)
    pfatal (tagfile);

  if (update)
    {
      char cmd[BUFSIZ];
      sprintf (cmd, "sort %s -o %s", tagfile, tagfile);
      exit (system (cmd));
    }
  return GOOD;
}


/*
 * Return a compressor given the file name.  If EXTPTR is non-zero,
 * return a pointer into FILE where the compressor-specific
 * extension begins.  If no compressor is found, NULL is returned
 * and EXTPTR is not significant.
 * Idea by Vladimir Alexiev <vladimir@cs.ualberta.ca> (1998)
 */
static compressor *
get_compressor_from_suffix (file, extptr)
     char *file;
     char **extptr;
{
  compressor *compr;
  char *slash, *suffix;

  /* This relies on FN to be after canonicalize_filename,
     so we don't need to consider backslashes on DOS_NT.  */
  slash = etags_strrchr (file, '/');
  suffix = etags_strrchr (file, '.');
  if (suffix == NULL || suffix < slash)
    return NULL;
  if (extptr != NULL)
    *extptr = suffix;
  suffix += 1;
  /* Let those poor souls who live with DOS 8+3 file name limits get
     some solace by treating foo.cgz as if it were foo.c.gz, etc.
     Only the first do loop is run if not MSDOS */
  do
    {
      for (compr = compressors; compr->suffix != NULL; compr++)
	if (streq (compr->suffix, suffix))
	  return compr;
      if (!MSDOS)
	break;			/* do it only once: not really a loop */
      if (extptr != NULL)
	*extptr = ++suffix;
    } while (*suffix != '\0');
  return NULL;
}



/*
 * Return a language given the name.
 */
static language *
get_language_from_langname (name)
     const char *name;
{
  language *lang;

  if (name == NULL)
    error ("empty language name", (char *)NULL);
  else
    {
      for (lang = lang_names; lang->name != NULL; lang++)
	if (streq (name, lang->name))
	  return lang;
      error ("unknown language \"%s\"", name);
    }

  return NULL;
}


/*
 * Return a language given the interpreter name.
 */
static language *
get_language_from_interpreter (interpreter)
     char *interpreter;
{
  language *lang;
  char **iname;

  if (interpreter == NULL)
    return NULL;
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->interpreters != NULL)
      for (iname = lang->interpreters; *iname != NULL; iname++)
	if (streq (*iname, interpreter))
	    return lang;

  return NULL;
}



/*
 * Return a language given the file name.
 */
static language *
get_language_from_filename (file)
     char *file;
{
  language *lang;
  char **name, **ext, *suffix;

  /* Try whole file name first. */
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->filenames != NULL)
      for (name = lang->filenames; *name != NULL; name++)
	if (streq (*name, file))
	  return lang;

  /* If not found, try suffix after last dot. */
  suffix = etags_strrchr (file, '.');
  if (suffix == NULL)
    return NULL;
  suffix += 1;
  for (lang = lang_names; lang->name != NULL; lang++)
    if (lang->suffixes != NULL)
      for (ext = lang->suffixes; *ext != NULL; ext++)
	if (streq (*ext, suffix))
	  return lang;
  return NULL;
}


/*
 * This routine is called on each file argument.
 */
static void
process_file (file, lang)
     char *file;
     language *lang;
{
  struct stat stat_buf;
  FILE *inf;
  static const fdesc emptyfdesc;
  fdesc *fdp;
  compressor *compr;
  char *compressed_name, *uncompressed_name;
  char *ext, *real_name;
  int retval;


  canonicalize_filename (file);
  if (streq (file, tagfile) && !streq (tagfile, "-"))
    {
      error ("skipping inclusion of %s in self.", file);
      return;
    }
  if ((compr = get_compressor_from_suffix (file, &ext)) == NULL)
    {
      compressed_name = NULL;
      real_name = uncompressed_name = savestr (file);
    }
  else
    {
      real_name = compressed_name = savestr (file);
      uncompressed_name = savenstr (file, ext - file);
    }

  /* If the canonicalized uncompressed name
     has already been dealt with, skip it silently. */
  for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
    {
      assert (fdp->infname != NULL);
      if (streq (uncompressed_name, fdp->infname))
	goto cleanup;
    }

  /* Create a new input file description entry. */
  fdp = fdhead;
  fdhead = xnew (1, fdesc);
  *fdhead = emptyfdesc;
  fdhead->next = fdp;

  if (stat (real_name, &stat_buf) != 0)
    {
      /* Reset real_name and try with a different name. */
      real_name = NULL;
      if (compressed_name != NULL) /* try with the given suffix */
	{
	  if (stat (uncompressed_name, &stat_buf) == 0)
	    real_name = uncompressed_name;
	}
      else			/* try all possible suffixes */
	{
	  for (compr = compressors; compr->suffix != NULL; compr++)
	    {
	      compressed_name = concat (file, ".", compr->suffix);
	      if (stat (compressed_name, &stat_buf) != 0)
		{
		  if (MSDOS)
		    {
		      char *suf = compressed_name + strlen (file);
		      size_t suflen = strlen (compr->suffix) + 1;
		      for ( ; suf[1]; suf++, suflen--)
			{
			  memmove (suf, suf + 1, suflen);
			  if (stat (compressed_name, &stat_buf) == 0)
			    {
			      real_name = compressed_name;
			      break;
			    }
			}
		      if (real_name != NULL)
			break;
		    } /* MSDOS */
		  free (compressed_name);
		  compressed_name = NULL;
		}
	      else
		{
		  real_name = compressed_name;
		  break;
		}
	    }
	}
      if (real_name == NULL)
	{
	  perror (file);
	  goto cleanup;
	}
    } /* try with a different name */

  if (!S_ISREG (stat_buf.st_mode))
    {
      error ("skipping %s: it is not a regular file.", real_name);
      goto cleanup;
    }
  if (real_name == compressed_name)
    {
      char *cmd = concat (compr->command, " ", real_name);
      inf = (FILE *) popen (cmd, "r");
      free (cmd);
    }
  else
    inf = fopen (real_name, "r");
  if (inf == NULL)
    {
      perror (real_name);
      goto cleanup;
    }

  fdhead->infname = savestr (uncompressed_name);
  fdhead->lang = lang;
  fdhead->infabsname = absolute_filename (uncompressed_name, cwd);
  fdhead->infabsdir = absolute_dirname (uncompressed_name, cwd);
  if (filename_is_absolute (uncompressed_name))
    {
      /* file is an absolute file name.  Canonicalize it. */
      fdhead->taggedfname = absolute_filename (uncompressed_name, NULL);
    }
  else
    {
      /* file is a file name relative to cwd.  Make it relative
	 to the directory of the tags file. */
      fdhead->taggedfname = relative_filename (uncompressed_name, tagfiledir);
    }
  fdhead->usecharno = TRUE;	/* use char position when making tags */
  fdhead->prop = NULL;

  curfdp = fdhead;		/* the current file description */

  find_entries (inf);

  if (real_name == compressed_name)
    retval = pclose (inf);
  else
    retval = fclose (inf);
  if (retval < 0)
    pfatal (file);

 cleanup:
  /* XXX if no more useful, delete head of file description list */
  if (compressed_name) free (compressed_name);
  if (uncompressed_name) free (uncompressed_name);
  return;
}

/*
 * This routine sets up the boolean pseudo-functions which work
 * by setting boolean flags dependent upon the corresponding character.
 * Every char which is NOT in that string is not a white char.  Therefore,
 * all of the array "_wht" is set to FALSE, and then the elements
 * subscripted by the chars in "white" are set to TRUE.  Thus "_wht"
 * of a char is TRUE if it is the string "white", else FALSE.
 */
static void
init ()
{
  register char *sp;
  register int i;

  for (i = 0; i < CHARS; i++)
    iswhite(i) = notinname(i) = begtoken(i) = intoken(i) = endtoken(i) = FALSE;
  for (sp = white; *sp != '\0'; sp++) iswhite (*sp) = TRUE;
  for (sp = nonam; *sp != '\0'; sp++) notinname (*sp) = TRUE;
  notinname('\0') = notinname('\n');
  for (sp = begtk; *sp != '\0'; sp++) begtoken (*sp) = TRUE;
  begtoken('\0') = begtoken('\n');
  for (sp = midtk; *sp != '\0'; sp++) intoken (*sp) = TRUE;
  intoken('\0') = intoken('\n');
  for (sp = endtk; *sp != '\0'; sp++) endtoken (*sp) = TRUE;
  endtoken('\0') = endtoken('\n');
}

/*
 * This routine opens the specified file and calls the function
 * which finds the function and type definitions.
 */
static node *last_node = NULL;

static void
find_entries (inf)
     FILE *inf;
{
  char *cp;
  node *old_last_node;
  language *lang = curfdp->lang;
  Lang_function *parser = NULL;

  /* If user specified a language, use it. */
  if (lang != NULL && lang->function != NULL)
    {
      parser = lang->function;
    }

  /* Else try to guess the language given the file name. */
  if (parser == NULL)
    {
      lang = get_language_from_filename (curfdp->infname);
      if (lang != NULL && lang->function != NULL)
	{
	  curfdp->lang = lang;
	  parser = lang->function;
	}
    }

  /* Else look for sharp-bang as the first two characters. */
  if (parser == NULL
      && readline_internal (&lb, inf) > 0
      && lb.len >= 2
      && lb.buffer[0] == '#'
      && lb.buffer[1] == '!')
    {
      char *lp;

      /* Set lp to point at the first char after the last slash in the
         line or, if no slashes, at the first nonblank.  Then set cp to
	 the first successive blank and terminate the string. */
      lp = etags_strrchr (lb.buffer+2, '/');
      if (lp != NULL)
	lp += 1;
      else
	lp = skip_spaces (lb.buffer + 2);
      cp = skip_non_spaces (lp);
      *cp = '\0';

      if (strlen (lp) > 0)
	{
	  lang = get_language_from_interpreter (lp);
	  if (lang != NULL && lang->function != NULL)
	    {
	      curfdp->lang = lang;
	      parser = lang->function;
	    }
	}
    }

  if (!no_line_directive
      && curfdp->lang != NULL && curfdp->lang->metasource)
    /* It may be that this is an xxx.y file, and we already parsed an xxx.c
       file, or anyway we parsed a file that is automatically generated from
       this one.  If this is the case, the xxx.c file contained #line
       directives that generated tags pointing to this file.  Let's delete
       them all before parsing this file, which is the real source. */
    {
      fdesc **fdpp = &fdhead;
      while (*fdpp != NULL)
	if (*fdpp != curfdp
	    && streq ((*fdpp)->taggedfname, curfdp->taggedfname))
	  /* We found one of those!  We must delete both the file description
	     and all tags referring to it. */
	  {
	    fdesc *badfdp = *fdpp;

	    *fdpp = badfdp->next; /* remove the bad description from the list */
	    fdpp = &badfdp->next; /* advance the list pointer */

	    fprintf (stderr, "Removing references to \"%s\" obtained from \"%s\"\n",
		     badfdp->taggedfname, badfdp->infname);
	    /* Delete the tags referring to badfdp. */
	    invalidate_nodes (badfdp, nodehead);

	    /* Delete badfdp. */
	    if (badfdp->infname != NULL) free (badfdp->infname);
	    if (badfdp->infabsname != NULL) free (badfdp->infabsname);
	    if (badfdp->infabsdir != NULL) free (badfdp->infabsdir);
	    if (badfdp->taggedfname != NULL) free (badfdp->taggedfname);
	    if (badfdp->prop != NULL) free (badfdp->prop);
	    free (badfdp);
	  }
	else
	  fdpp = &(*fdpp)->next; /* advance the list pointer */
    }

  if (parser != NULL)
    {
      parser (inf);
      return;
    }

  /* We rewind here, even if inf may be a pipe.  We fail if the
     length of the first line is longer than the pipe block size,
     which is unlikely. */
  rewind (inf);

  /* Else try Fortran. */
  old_last_node = last_node;
  curfdp->lang = get_language_from_langname ("fortran");
  Fortran_functions (inf);

  if (old_last_node == last_node)
    /* No Fortran entries found.  Try C. */
    {
      /* We do not tag if rewind fails.
	 Only the file name will be recorded in the tags file. */
      rewind (inf);
      curfdp->lang = get_language_from_langname (cplusplus ? "c++" : "c");
      default_C_entries (inf);
    }
  return;
}


/* Record a tag. */
static void
pfnote (name, is_func, linestart, linelen, lno, cno)
     char *name;		/* tag name, or NULL if unnamed */
     bool is_func;		/* tag is a function */
     char *linestart;		/* start of the line where tag is */
     int linelen;		/* length of the line where tag is */
     int lno;			/* line number */
     long cno;			/* character number */
{
  register node *np;

  if (CTAGS && name == NULL)
    return;

  np = xnew (1, node);

  /* If ctags mode, change name "main" to M<thisfilename>. */
  if (CTAGS && !cxref_style && streq (name, "main"))
    {
      register char *fp = etags_strrchr (curfdp->taggedfname, '/');
      np->name = concat ("M", fp == NULL ? curfdp->taggedfname : fp + 1, "");
      fp = etags_strrchr (np->name, '.');
      if (fp != NULL && fp[1] != '\0' && fp[2] == '\0')
	fp[0] = '\0';
    }
  else
    np->name = name;
  np->valid = TRUE;
  np->been_warned = FALSE;
  np->fdp = curfdp;
  np->is_func = is_func;
  np->lno = lno;
  if (np->fdp->usecharno)
    /* Our char numbers are 0-base, because of C language tradition?
       ctags compatibility?  old versions compatibility?   I don't know.
       Anyway, since emacs's are 1-base we expect etags.el to take care
       of the difference.  If we wanted to have 1-based numbers, we would
       uncomment the +1 below. */
    np->cno = cno /* + 1 */ ;
  else
    np->cno = invalidcharno;
  np->left = np->right = NULL;
  if (CTAGS && !cxref_style)
    {
      if (strlen (linestart) < 50)
	np->pat = concat (linestart, "$", "");
      else
	np->pat = savenstr (linestart, 50);
    }
  else
    np->pat = savenstr (linestart, linelen);

  add_node (np, &nodehead);
}

/*
 * TAGS format specification
 * Idea by Sam Kendall <kendall@mv.mv.com> (1997)
 *
 * pfnote should emit the optimized form [unnamed tag] only if:
 *  1. name does not contain any of the characters " \t\r\n(),;";
 *  2. linestart contains name as either a rightmost, or rightmost but
 *     one character, substring;
 *  3. the character, if any, immediately before name in linestart must
 *     be one of the characters " \t(),;";
 *  4. the character, if any, immediately after name in linestart must
 *     also be one of the characters " \t(),;".
 *
 * The real implementation uses the notinname() macro, which recognises
 * characters slightly different from " \t\r\n(),;".  See the variable
 * `nonam'.
 */
#define traditional_tag_style TRUE
static void
new_pfnote (name, namelen, is_func, linestart, linelen, lno, cno)
     char *name;		/* tag name, or NULL if unnamed */
     int namelen;		/* tag length */
     bool is_func;		/* tag is a function */
     char *linestart;		/* start of the line where tag is */
     int linelen;		/* length of the line where tag is */
     int lno;			/* line number */
     long cno;			/* character number */
{
  register char *cp;
  bool named;

  named = TRUE;
  if (!CTAGS)
    {
      for (cp = name; !notinname (*cp); cp++)
	continue;
      if (*cp == '\0')				/* rule #1 */
	{
	  cp = linestart + linelen - namelen;
	  if (notinname (linestart[linelen-1]))
	    cp -= 1;				/* rule #4 */
	  if (cp >= linestart			/* rule #2 */
	      && (cp == linestart
		  || notinname (cp[-1]))	/* rule #3 */
	      && strneq (name, cp, namelen))	/* rule #2 */
	    named = FALSE;	/* use unnamed tag */
	}
    }

  if (named)
    name = savenstr (name, namelen);
  else
    name = NULL;
  pfnote (name, is_func, linestart, linelen, lno, cno);
}

/*
 * free_tree ()
 *	recurse on left children, iterate on right children.
 */
static void
free_tree (np)
     register node *np;
{
  while (np)
    {
      register node *node_right = np->right;
      free_tree (np->left);
      if (np->name != NULL)
	free (np->name);
      free (np->pat);
      free (np);
      np = node_right;
    }
}

/*
 * add_node ()
 *	Adds a node to the tree of nodes.  In etags mode, sort by file
 *  	name.  In ctags mode, sort by tag name.  Make no attempt at
 *    	balancing.
 *
 *	add_node is the only function allowed to add nodes, so it can
 *	maintain state.
 */
static void
add_node (np, cur_node_p)
     node *np, **cur_node_p;
{
  register int dif;
  register node *cur_node = *cur_node_p;

  if (cur_node == NULL)
    {
      *cur_node_p = np;
      last_node = np;
      return;
    }

  if (!CTAGS)
    {
      /* Etags Mode */
      assert (last_node != NULL);
      /* For each file name, tags are in a linked sublist on the right
	 pointer.  The first tags of different files are a linked list
	 on the left pointer.  last_node points to the end of the last
	 used sublist. */
      if (last_node->fdp == np->fdp)
	{
	  /* Let's use the same sublist as the last added node. */
	  last_node->right = np;
	  last_node = np;
	}
      else if (cur_node->fdp == np->fdp)
	{
	  /* Scanning the list we found the head of a sublist which is
	     good for us.  Let's scan this sublist. */
	  add_node (np, &cur_node->right);
	}
      else
	/* The head of this sublist is not good for us.  Let's try the
	   next one. */
	add_node (np, &cur_node->left);
    }
  else
    {
      /* Ctags Mode */
      dif = strcmp (np->name, cur_node->name);

      /*
       * If this tag name matches an existing one, then
       * do not add the node, but maybe print a warning.
       */
      if (!dif)
	{
	  if (np->fdp == cur_node->fdp)
	    {
	      if (!no_warnings)
		{
		  fprintf (stderr, "Duplicate entry in file %s, line %d: %s\n",
			   np->fdp->infname, lineno, np->name);
		  fprintf (stderr, "Second entry ignored\n");
		}
	    }
	  else if (!cur_node->been_warned && !no_warnings)
	    {
	      fprintf
		(stderr,
		 "Duplicate entry in files %s and %s: %s (Warning only)\n",
		 np->fdp->infname, cur_node->fdp->infname, np->name);
	      cur_node->been_warned = TRUE;
	    }
	  return;
	}

      /* Actually add the node */
      add_node (np, dif < 0 ? &cur_node->left : &cur_node->right);
    }
}

/*
 * invalidate_nodes ()
 *	Scan the node tree and invalidate all nodes pointing to the
 *	given file description.
 */
static void
invalidate_nodes (badfdp, np)
     fdesc *badfdp;
     node *np;
{
  if (np->left != NULL)
    invalidate_nodes (badfdp, np->left);
  if (np->fdp == badfdp)
    np-> valid = FALSE;
  if (np->right != NULL)
    invalidate_nodes (badfdp, np->right);
}


static int total_size_of_entries __P((node *));
static int number_len __P((long));

/* Length of a non-negative number's decimal representation. */
static int
number_len (num)
     long num;
{
  int len = 1;
  while ((num /= 10) > 0)
    len += 1;
  return len;
}

/*
 * Return total number of characters that put_entries will output for
 * the nodes in the linked list at the right of the specified node.
 * This count is irrelevant with etags.el since emacs 19.34 at least,
 * but is still supplied for backward compatibility.
 */
static int
total_size_of_entries (np)
     register node *np;
{
  register int total = 0;

  for (; np != NULL; np = np->right)
    {
      total += strlen (np->pat) + 1;		/* pat\177 */
      if (np->name != NULL)
	total += strlen (np->name) + 1;		/* name\001 */
      total += number_len ((long) np->lno) + 1;	/* lno, */
      if (np->cno != invalidcharno)		/* cno */
	total += number_len (np->cno);
      total += 1;				/* newline */
    }

  return total;
}

static void
put_entries (np)
     register node *np;
{
  register char *sp;
  static fdesc *fdp = NULL;

  if (np == NULL)
    return;

  /* Output subentries that precede this one */
  if (CTAGS)
    put_entries (np->left);

  /* Output this entry */
  if (np->valid)
    {
      if (!CTAGS)
	{
	  /* Etags mode */
	  if (fdp != np->fdp)
	    {
	      fdp = np->fdp;
	      fprintf (tagf, "\f\n%s,%d\n",
		       fdp->taggedfname, total_size_of_entries (np));
	    }
	  fputs (np->pat, tagf);
	  fputc ('\177', tagf);
	  if (np->name != NULL)
	    {
	      fputs (np->name, tagf);
	      fputc ('\001', tagf);
	    }
	  fprintf (tagf, "%d,", np->lno);
	  if (np->cno != invalidcharno)
	    fprintf (tagf, "%ld", np->cno);
	  fputs ("\n", tagf);
	}
      else
	{
	  /* Ctags mode */
	  if (np->name == NULL)
	    error ("internal error: NULL name in ctags mode.", (char *)NULL);

	  if (cxref_style)
	    {
	      if (vgrind_style)
		fprintf (stdout, "%s %s %d\n",
			 np->name, np->fdp->taggedfname, (np->lno + 63) / 64);
	      else
		fprintf (stdout, "%-16s %3d %-16s %s\n",
			 np->name, np->lno, np->fdp->taggedfname, np->pat);
	    }
	  else
	    {
	      fprintf (tagf, "%s\t%s\t", np->name, np->fdp->taggedfname);

	      if (np->is_func)
		{		/* function or #define macro with args */
		  putc (searchar, tagf);
		  putc ('^', tagf);

		  for (sp = np->pat; *sp; sp++)
		    {
		      if (*sp == '\\' || *sp == searchar)
			putc ('\\', tagf);
		      putc (*sp, tagf);
		    }
		  putc (searchar, tagf);
		}
	      else
		{		/* anything else; text pattern inadequate */
		  fprintf (tagf, "%d", np->lno);
		}
	      putc ('\n', tagf);
	    }
	}
    } /* if this node contains a valid tag */

  /* Output subentries that follow this one */
  put_entries (np->right);
  if (!CTAGS)
    put_entries (np->left);
}


/* C extensions. */
#define C_EXT	0x00fff		/* C extensions */
#define C_PLAIN 0x00000		/* C */
#define C_PLPL	0x00001		/* C++ */
#define C_STAR	0x00003		/* C* */
#define C_JAVA	0x00005		/* JAVA */
#define C_AUTO  0x01000		/* C, but switch to C++ if `class' is met */
#define YACC	0x10000		/* yacc file */

/*
 * The C symbol tables.
 */
enum sym_type
{
  st_none,
  st_C_objprot, st_C_objimpl, st_C_objend,
  st_C_gnumacro,
  st_C_ignore,
  st_C_javastruct,
  st_C_operator,
  st_C_class, st_C_template,
  st_C_struct, st_C_extern, st_C_enum, st_C_define, st_C_typedef, st_C_typespec
};

static unsigned int hash __P((const char *, unsigned int));
static struct C_stab_entry * in_word_set __P((const char *, unsigned int));
static enum sym_type C_symtype __P((char *, int, int));

/* Feed stuff between (but not including) %[ and %] lines to:
      gperf -c -k 1,3 -o -p -r -t
%[
struct C_stab_entry { char *name; int c_ext; enum sym_type type; }
%%
if,		0,	st_C_ignore
for,		0,	st_C_ignore
while,		0,	st_C_ignore
switch,		0,	st_C_ignore
return,		0,	st_C_ignore
@interface,	0,	st_C_objprot
@protocol,	0,	st_C_objprot
@implementation,0,	st_C_objimpl
@end,		0,	st_C_objend
import,		C_JAVA,	st_C_ignore
package,	C_JAVA,	st_C_ignore
friend,		C_PLPL,	st_C_ignore
extends,  	C_JAVA,	st_C_javastruct
implements,  	C_JAVA,	st_C_javastruct
interface,	C_JAVA, st_C_struct
class,  	0,	st_C_class
namespace,	C_PLPL,	st_C_struct
domain, 	C_STAR,	st_C_struct
union,  	0,	st_C_struct
struct, 	0,	st_C_struct
extern,  	0,	st_C_extern
enum,    	0,	st_C_enum
typedef, 	0,	st_C_typedef
define,  	0,	st_C_define
operator,	C_PLPL, st_C_operator
template,	0,	st_C_template
bool,		C_PLPL,	st_C_typespec
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
static,  	0,	st_C_typespec
const,   	0,	st_C_typespec
volatile,	0,	st_C_typespec
explicit,	C_PLPL,	st_C_typespec
mutable,	C_PLPL,	st_C_typespec
typename,	C_PLPL,	st_C_typespec
# DEFUN used in emacs, the next three used in glibc (SYSCALL only for mach).
DEFUN,		0,	st_C_gnumacro
SYSCALL,	0,	st_C_gnumacro
ENTRY,		0,	st_C_gnumacro
PSEUDO,		0,	st_C_gnumacro
# These are defined inside C functions, so currently they are not met.
# EXFUN used in glibc, DEFVAR_* in emacs.
#EXFUN,		0,	st_C_gnumacro
#DEFVAR_,	0,	st_C_gnumacro
%]
and replace lines between %< and %> with its output,
then make in_word_set and C_stab_entry static. */
/*%<*/
/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -c -k 1,3 -o -p -r -t  */
struct C_stab_entry { char *name; int c_ext; enum sym_type type; };

#define TOTAL_KEYWORDS 47
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 15
#define MIN_HASH_VALUE 18
#define MAX_HASH_VALUE 138
/* maximum key range = 121, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned char asso_values[] =
    {
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139,  63, 139, 139, 139,  33,  44,
       62, 139, 139, 139, 139, 139, 139, 139, 139, 139,
       42, 139, 139,  12,  32, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139,  34,  59,  37,
       24,  58,  33,   3, 139,  16, 139, 139,  42,  60,
       18,  11,  39, 139,  23,  57,   4,  63,   6,  20,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
      139, 139, 139, 139, 139, 139
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval;
}

#ifdef __GNUC__
__inline
#endif
static struct C_stab_entry *
in_word_set (str, len)
     register const char *str;
     register unsigned int len;
{
  static struct C_stab_entry wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"if",		0,	st_C_ignore},
      {""}, {""}, {""}, {""},
      {"int",     	0,	st_C_typespec},
      {""}, {""},
      {"void",    	0,	st_C_typespec},
      {""}, {""},
      {"interface",	C_JAVA, st_C_struct},
      {""},
      {"SYSCALL",	0,	st_C_gnumacro},
      {""},
      {"return",		0,	st_C_ignore},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"while",		0,	st_C_ignore},
      {"auto",    	0,	st_C_typespec},
      {""}, {""}, {""}, {""}, {""}, {""},
      {"float",   	0,	st_C_typespec},
      {"typedef", 	0,	st_C_typedef},
      {"typename",	C_PLPL,	st_C_typespec},
      {""}, {""}, {""},
      {"friend",		C_PLPL,	st_C_ignore},
      {"volatile",	0,	st_C_typespec},
      {""}, {""},
      {"for",		0,	st_C_ignore},
      {"const",   	0,	st_C_typespec},
      {"import",		C_JAVA,	st_C_ignore},
      {""},
      {"define",  	0,	st_C_define},
      {"long",    	0,	st_C_typespec},
      {"implements",  	C_JAVA,	st_C_javastruct},
      {"signed",  	0,	st_C_typespec},
      {""},
      {"extern",  	0,	st_C_extern},
      {"extends",  	C_JAVA,	st_C_javastruct},
      {""},
      {"mutable",	C_PLPL,	st_C_typespec},
      {"template",	0,	st_C_template},
      {"short",   	0,	st_C_typespec},
      {"bool",		C_PLPL,	st_C_typespec},
      {"char",    	0,	st_C_typespec},
      {"class",  	0,	st_C_class},
      {"operator",	C_PLPL, st_C_operator},
      {""},
      {"switch",		0,	st_C_ignore},
      {""},
      {"ENTRY",		0,	st_C_gnumacro},
      {""},
      {"package",	C_JAVA,	st_C_ignore},
      {"union",  	0,	st_C_struct},
      {"@end",		0,	st_C_objend},
      {"struct", 	0,	st_C_struct},
      {"namespace",	C_PLPL,	st_C_struct},
      {""}, {""},
      {"domain", 	C_STAR,	st_C_struct},
      {"@interface",	0,	st_C_objprot},
      {"PSEUDO",		0,	st_C_gnumacro},
      {"double",  	0,	st_C_typespec},
      {""},
      {"@protocol",	0,	st_C_objprot},
      {""},
      {"static",  	0,	st_C_typespec},
      {""}, {""},
      {"DEFUN",		0,	st_C_gnumacro},
      {""}, {""}, {""}, {""},
      {"explicit",	C_PLPL,	st_C_typespec},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
      {"enum",    	0,	st_C_enum},
      {""}, {""},
      {"unsigned",	0,	st_C_typespec},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"@implementation",0,	st_C_objimpl}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1))
            return &wordlist[key];
        }
    }
  return 0;
}
/*%>*/

static enum sym_type
C_symtype (str, len, c_ext)
     char *str;
     int len;
     int c_ext;
{
  register struct C_stab_entry *se = in_word_set (str, len);

  if (se == NULL || (se->c_ext && !(c_ext & se->c_ext)))
    return st_none;
  return se->type;
}


/*
 * C functions and variables are recognized using a simple
 * finite automaton.  fvdef is its state variable.
 */
static enum
{
  fvnone,			/* nothing seen */
  fdefunkey,			/* Emacs DEFUN keyword seen */
  fdefunname,			/* Emacs DEFUN name seen */
  foperator,			/* func: operator keyword seen (cplpl) */
  fvnameseen,			/* function or variable name seen */
  fstartlist,			/* func: just after open parenthesis */
  finlist,			/* func: in parameter list */
  flistseen,			/* func: after parameter list */
  fignore,			/* func: before open brace */
  vignore			/* var-like: ignore until ';' */
} fvdef;

static bool fvextern;		/* func or var: extern keyword seen; */

/*
 * typedefs are recognized using a simple finite automaton.
 * typdef is its state variable.
 */
static enum
{
  tnone,			/* nothing seen */
  tkeyseen,			/* typedef keyword seen */
  ttypeseen,			/* defined type seen */
  tinbody,			/* inside typedef body */
  tend,				/* just before typedef tag */
  tignore			/* junk after typedef tag */
} typdef;

/*
 * struct-like structures (enum, struct and union) are recognized
 * using another simple finite automaton.  `structdef' is its state
 * variable.
 */
static enum
{
  snone,			/* nothing seen yet,
				   or in struct body if cblev > 0 */
  skeyseen,			/* struct-like keyword seen */
  stagseen,			/* struct-like tag seen */
  sintemplate,			/* inside template (ignore) */
  scolonseen			/* colon seen after struct-like tag */
} structdef;

/*
 * When objdef is different from onone, objtag is the name of the class.
 */
static char *objtag = "<uninited>";

/*
 * Yet another little state machine to deal with preprocessor lines.
 */
static enum
{
  dnone,			/* nothing seen */
  dsharpseen,			/* '#' seen as first char on line */
  ddefineseen,			/* '#' and 'define' seen */
  dignorerest			/* ignore rest of line */
} definedef;

/*
 * State machine for Objective C protocols and implementations.
 * Idea by Tom R.Hageman <tom@basil.icce.rug.nl> (1995)
 */
static enum
{
  onone,			/* nothing seen */
  oprotocol,			/* @interface or @protocol seen */
  oimplementation,		/* @implementations seen */
  otagseen,			/* class name seen */
  oparenseen,			/* parenthesis before category seen */
  ocatseen,			/* category name seen */
  oinbody,			/* in @implementation body */
  omethodsign,			/* in @implementation body, after +/- */
  omethodtag,			/* after method name */
  omethodcolon,			/* after method colon */
  omethodparm,			/* after method parameter */
  oignore			/* wait for @end */
} objdef;


/*
 * Use this structure to keep info about the token read, and how it
 * should be tagged.  Used by the make_C_tag function to build a tag.
 */
static struct tok
{
  bool valid;
  bool named;
  int offset;
  int length;
  int lineno;
  long linepos;
  char *line;
} token;			/* latest token read */
static linebuffer token_name;	/* its name */

/*
 * Variables and functions for dealing with nested structures.
 * Idea by Mykola Dzyuba <mdzyuba@yahoo.com> (2001)
 */
static void pushclass_above __P((int, char *, int));
static void popclass_above __P((int));
static void write_classname __P((linebuffer *, char *qualifier));

static struct {
  char **cname;			/* nested class names */
  int *cblev;			/* nested class curly brace level */
  int nl;			/* class nesting level (elements used) */
  int size;			/* length of the array */
} cstack;			/* stack for nested declaration tags */
/* Current struct nesting depth (namespace, class, struct, union, enum). */
#define nestlev		(cstack.nl)
/* After struct keyword or in struct body, not inside an nested function. */
#define instruct	(structdef == snone && nestlev > 0			\
			 && cblev == cstack.cblev[nestlev-1] + 1)

static void
pushclass_above (cblev, str, len)
     int cblev;
     char *str;
     int len;
{
  int nl;

  popclass_above (cblev);
  nl = cstack.nl;
  if (nl >= cstack.size)
    {
      int size = cstack.size *= 2;
      xrnew (cstack.cname, size, char *);
      xrnew (cstack.cblev, size, int);
    }
  assert (nl == 0 || cstack.cblev[nl-1] < cblev);
  cstack.cname[nl] = (str == NULL) ? NULL : savenstr (str, len);
  cstack.cblev[nl] = cblev;
  cstack.nl = nl + 1;
}

static void
popclass_above (cblev)
     int cblev;
{
  int nl;

  for (nl = cstack.nl - 1;
       nl >= 0 && cstack.cblev[nl] >= cblev;
       nl--)
    {
      if (cstack.cname[nl] != NULL)
	free (cstack.cname[nl]);
      cstack.nl = nl;
    }
}

static void
write_classname (cn, qualifier)
     linebuffer *cn;
     char *qualifier;
{
  int i, len;
  int qlen = strlen (qualifier);

  if (cstack.nl == 0 || cstack.cname[0] == NULL)
    {
      len = 0;
      cn->len = 0;
      cn->buffer[0] = '\0';
    }
  else
    {
      len = strlen (cstack.cname[0]);
      linebuffer_setlen (cn, len);
      strcpy (cn->buffer, cstack.cname[0]);
    }
  for (i = 1; i < cstack.nl; i++)
    {
      char *s;
      int slen;

      s = cstack.cname[i];
      if (s == NULL)
	continue;
      slen = strlen (s);
      len += slen + qlen;
      linebuffer_setlen (cn, len);
      strncat (cn->buffer, qualifier, qlen);
      strncat (cn->buffer, s, slen);
    }
}


static bool consider_token __P((char *, int, int, int *, int, int, bool *));
static void make_C_tag __P((bool));

/*
 * consider_token ()
 *	checks to see if the current token is at the start of a
 *	function or variable, or corresponds to a typedef, or
 * 	is a struct/union/enum tag, or #define, or an enum constant.
 *
 *	*IS_FUNC gets TRUE iff the token is a function or #define macro
 *	with args.  C_EXTP points to which language we are looking at.
 *
 * Globals
 *	fvdef			IN OUT
 *	structdef		IN OUT
 *	definedef		IN OUT
 *	typdef			IN OUT
 *	objdef			IN OUT
 */

static bool
consider_token (str, len, c, c_extp, cblev, parlev, is_func_or_var)
     register char *str;	/* IN: token pointer */
     register int len;		/* IN: token length */
     register int c;		/* IN: first char after the token */
     int *c_extp;		/* IN, OUT: C extensions mask */
     int cblev;			/* IN: curly brace level */
     int parlev;		/* IN: parenthesis level */
     bool *is_func_or_var;	/* OUT: function or variable found */
{
  /* When structdef is stagseen, scolonseen, or snone with cblev > 0,
     structtype is the type of the preceding struct-like keyword, and
     structcblev is the curly brace level where it has been seen. */
  static enum sym_type structtype;
  static int structcblev;
  static enum sym_type toktype;


  toktype = C_symtype (str, len, *c_extp);

  /*
   * Advance the definedef state machine.
   */
  switch (definedef)
    {
    case dnone:
      /* We're not on a preprocessor line. */
      if (toktype == st_C_gnumacro)
	{
	  fvdef = fdefunkey;
	  return FALSE;
	}
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
      *is_func_or_var = (c == '(');
      if (!*is_func_or_var && !constantypedefs)
	return FALSE;
      else
	return TRUE;
    case dignorerest:
      return FALSE;
    default:
      error ("internal error: definedef value.", (char *)NULL);
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
	    typdef = tkeyseen;
	  fvextern = FALSE;
	  fvdef = fvnone;
	  return FALSE;
	}
      break;
    case tkeyseen:
      switch (toktype)
	{
	case st_none:
	case st_C_typespec:
	case st_C_class:
	case st_C_struct:
	case st_C_enum:
	  typdef = ttypeseen;
	  break;
	}
      break;
    case ttypeseen:
      if (structdef == snone && fvdef == fvnone)
	{
	  fvdef = fvnameseen;
	  return TRUE;
	}
      break;
    case tend:
      switch (toktype)
	{
	case st_C_typespec:
	case st_C_class:
	case st_C_struct:
	case st_C_enum:
	  return FALSE;
	}
      return TRUE;
    }

  /*
   * This structdef business is NOT invoked when we are ctags and the
   * file is plain C.  This is because a struct tag may have the same
   * name as another tag, and this loses with ctags.
   */
  switch (toktype)
    {
    case st_C_javastruct:
      if (structdef == stagseen)
        structdef = scolonseen;
      return FALSE;
    case st_C_template:
    case st_C_class:
      if (cblev == 0
	  && (*c_extp & C_AUTO)	/* automatic detection of C++ language */
	  && definedef == dnone && structdef == snone
	  && typdef == tnone && fvdef == fvnone)
	*c_extp = (*c_extp | C_PLPL) & ~C_AUTO;
      if (toktype == st_C_template)
	break;
      /* FALLTHRU */
    case st_C_struct:
    case st_C_enum:
      if (parlev == 0
	  && fvdef != vignore
	  && (typdef == tkeyseen
	      || (typedefs_or_cplusplus && structdef == snone)))
	{
	  structdef = skeyseen;
	  structtype = toktype;
	  structcblev = cblev;
	}
      return FALSE;
    }

  if (structdef == skeyseen)
    {
      structdef = stagseen;
      return TRUE;
    }

  if (typdef != tnone)
    definedef = dnone;

  /* Detect Objective C constructs. */
  switch (objdef)
    {
    case onone:
      switch (toktype)
	{
	case st_C_objprot:
	  objdef = oprotocol;
	  return FALSE;
	case st_C_objimpl:
	  objdef = oimplementation;
	  return FALSE;
	}
      break;
    case oimplementation:
      /* Save the class tag for functions or variables defined inside. */
      objtag = savenstr (str, len);
      objdef = oinbody;
      return FALSE;
    case oprotocol:
      /* Save the class tag for categories. */
      objtag = savenstr (str, len);
      objdef = otagseen;
      *is_func_or_var = TRUE;
      return TRUE;
    case oparenseen:
      objdef = ocatseen;
      *is_func_or_var = TRUE;
      return TRUE;
    case oinbody:
      break;
    case omethodsign:
      if (parlev == 0)
	{
	  objdef = omethodtag;
	  linebuffer_setlen (&token_name, len);
	  strncpy (token_name.buffer, str, len);
	  token_name.buffer[len] = '\0';
	  return TRUE;
	}
      return FALSE;
    case omethodcolon:
      if (parlev == 0)
	objdef = omethodparm;
      return FALSE;
    case omethodparm:
      if (parlev == 0)
	{
	  objdef = omethodtag;
	  linebuffer_setlen (&token_name, token_name.len + len);
	  strncat (token_name.buffer, str, len);
	  return TRUE;
	}
      return FALSE;
    case oignore:
      if (toktype == st_C_objend)
	{
	  /* Memory leakage here: the string pointed by objtag is
	     never released, because many tests would be needed to
	     avoid breaking on incorrect input code.  The amount of
	     memory leaked here is the sum of the lengths of the
	     class tags.
	  free (objtag); */
	  objdef = onone;
	}
      return FALSE;
    }

  /* A function, variable or enum constant? */
  switch (toktype)
    {
    case st_C_extern:
      fvextern = TRUE;
      /* FALLTHRU */
    case st_C_typespec:
      if (fvdef != finlist && fvdef != fignore && fvdef != vignore)
	fvdef = fvnone;		/* should be useless */
      return FALSE;
    case st_C_ignore:
      fvextern = FALSE;
      fvdef = vignore;
      return FALSE;
    case st_C_operator:
      fvdef = foperator;
      *is_func_or_var = TRUE;
      return TRUE;
    case st_none:
      if (constantypedefs
	  && structdef == snone
	  && structtype == st_C_enum && cblev > structcblev)
	return TRUE;		/* enum constant */
      switch (fvdef)
	{
	case fdefunkey:
	  if (cblev > 0)
	    break;
	  fvdef = fdefunname;	/* GNU macro */
	  *is_func_or_var = TRUE;
	  return TRUE;
	case fvnone:
	  if ((strneq (str, "asm", 3) && endtoken (str[3]))
	      || (strneq (str, "__asm__", 7) && endtoken (str[7])))
	    {
	      fvdef = vignore;
	      return FALSE;
	    }
	  if ((*c_extp & C_PLPL) && strneq (str+len-10, "::operator", 10))
	    {
	      fvdef = foperator;
	      *is_func_or_var = TRUE;
	      return TRUE;
	    }
	  if (cblev > 0 && !instruct)
	    break;
	  fvdef = fvnameseen;	/* function or variable */
	  *is_func_or_var = TRUE;
	  return TRUE;
	}
      break;
    }

  return FALSE;
}


/*
 * C_entries often keeps pointers to tokens or lines which are older than
 * the line currently read.  By keeping two line buffers, and switching
 * them at end of line, it is possible to use those pointers.
 */
static struct
{
  long linepos;
  linebuffer lb;
} lbs[2];

#define current_lb_is_new (newndx == curndx)
#define switch_line_buffers() (curndx = 1 - curndx)

#define curlb (lbs[curndx].lb)
#define newlb (lbs[newndx].lb)
#define curlinepos (lbs[curndx].linepos)
#define newlinepos (lbs[newndx].linepos)

#define CNL_SAVE_DEFINEDEF()						\
do {									\
  curlinepos = charno;							\
  lineno++;								\
  linecharno = charno;							\
  charno += readline (&curlb, inf);					\
  lp = curlb.buffer;							\
  quotednl = FALSE;							\
  newndx = curndx;							\
} while (0)

#define CNL()								\
do {									\
  CNL_SAVE_DEFINEDEF();							\
  if (savetoken.valid)							\
    {									\
      token = savetoken;						\
      savetoken.valid = FALSE;						\
    }									\
  definedef = dnone;							\
} while (0)


static void
make_C_tag (isfun)
     bool isfun;
{
  /* This function should never be called when token.valid is FALSE, but
     we must protect against invalid input or internal errors. */
  if (DEBUG || token.valid)
    {
      if (traditional_tag_style)
	{
	  /* This was the original code.  Now we call new_pfnote instead,
	     which uses the new method for naming tags (see new_pfnote). */
	  char *name = NULL;

	  if (CTAGS || token.named)
	    name = savestr (token_name.buffer);
	  if (DEBUG && !token.valid)
	    {
	      if (token.named)
		name = concat (name, "##invalid##", "");
	      else
		name = savestr ("##invalid##");
	    }
	  pfnote (name, isfun, token.line,
		  token.offset+token.length+1, token.lineno, token.linepos);
	}
      else
	new_pfnote (token_name.buffer, token_name.len, isfun, token.line,
		    token.offset+token.length+1, token.lineno, token.linepos);
      token.valid = FALSE;
    }
}


/*
 * C_entries ()
 *	This routine finds functions, variables, typedefs,
 * 	#define's, enum constants and struct/union/enum definitions in
 * 	C syntax and adds them to the list.
 */
static void
C_entries (c_ext, inf)
     int c_ext;			/* extension of C */
     FILE *inf;			/* input file */
{
  register char c;		/* latest char read; '\0' for end of line */
  register char *lp;		/* pointer one beyond the character `c' */
  int curndx, newndx;		/* indices for current and new lb */
  register int tokoff;		/* offset in line of start of current token */
  register int toklen;		/* length of current token */
  char *qualifier;		/* string used to qualify names */
  int qlen;			/* length of qualifier */
  int cblev;			/* current curly brace level */
  int parlev;			/* current parenthesis level */
  int typdefcblev;		/* cblev where a typedef struct body begun */
  bool incomm, inquote, inchar, quotednl, midtoken;
  bool cplpl, cjava;
  bool yacc_rules;		/* in the rules part of a yacc file */
  struct tok savetoken;	        /* token saved during preprocessor handling */


  initbuffer (&token_name);
  initbuffer (&lbs[0].lb);
  initbuffer (&lbs[1].lb);
  if (cstack.size == 0)
    {
      cstack.size = (DEBUG) ? 1 : 4;
      cstack.nl = 0;
      cstack.cname = xnew (cstack.size, char *);
      cstack.cblev = xnew (cstack.size, int);
    }

  tokoff = toklen = typdefcblev = 0; /* keep compiler quiet */
  curndx = newndx = 0;
  lineno = 0;
  charno = 0;
  lp = curlb.buffer;
  *lp = 0;

  fvdef = fvnone; fvextern = FALSE; typdef = tnone;
  structdef = snone; definedef = dnone; objdef = onone;
  yacc_rules = FALSE;
  midtoken = inquote = inchar = incomm = quotednl = FALSE;
  token.valid = savetoken.valid = FALSE;
  cblev = 0;
  parlev = 0;
  cplpl = (c_ext & C_PLPL) == C_PLPL;
  cjava = (c_ext & C_JAVA) == C_JAVA;
  if (cjava)
    { qualifier = "."; qlen = 1; }
  else
    { qualifier = "::"; qlen = 2; }


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
	      CNL_SAVE_DEFINEDEF ();
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
	      CNL_SAVE_DEFINEDEF ();
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
	      CNL ();
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
	    switch (fvdef)
	      {
	      case fdefunkey:
	      case fstartlist:
	      case finlist:
	      case fignore:
	      case vignore:
		break;
	      default:
		fvextern = FALSE;
		fvdef = fvnone;
	      }
	    continue;
	  case '\'':
	    inchar = TRUE;
	    if (fvdef != finlist && fvdef != fignore && fvdef !=vignore)
	      {
		fvextern = FALSE;
		fvdef = fvnone;
	      }
	    continue;
	  case '/':
	    if (*lp == '*')
	      {
		lp++;
		incomm = TRUE;
		continue;
	      }
	    else if (/* cplpl && */ *lp == '/')
	      {
		c = '\0';
		break;
	      }
	    else
	      break;
	  case '%':
	    if ((c_ext & YACC) && *lp == '%')
	      {
		/* Entering or exiting rules section in yacc file. */
		lp++;
		definedef = dnone; fvdef = fvnone; fvextern = FALSE;
		typdef = tnone; structdef = snone;
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
		bool cpptoken = TRUE;

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


      /* Consider token only if some involved conditions are satisfied. */
      if (typdef != tignore
	  && definedef != dignorerest
	  && fvdef != finlist
	  && structdef != sintemplate
	  && (definedef != dnone
	      || structdef != scolonseen))
	{
	  if (midtoken)
	    {
	      if (endtoken (c))
		{
		  if (c == ':' && cplpl && *lp == ':' && begtoken (lp[1]))
		    {
		      /*
		       * This handles :: in the middle, but not at the
		       * beginning of an identifier.  Also, space-separated
		       * :: is not recognised.
		       */
		      lp += 2;
		      toklen += 2;
		      c = lp[-1];
		      goto still_in_token;
		    }
		  else
		    {
		      bool funorvar = FALSE;

		      if (yacc_rules
			  || consider_token (newlb.buffer + tokoff, toklen, c,
					     &c_ext, cblev, parlev, &funorvar))
			{
			  if (fvdef == foperator)
			    {
			      char *oldlp = lp;
			      lp = skip_spaces (lp-1);
			      if (*lp != '\0')
				lp += 1;
			      while (*lp != '\0'
				     && !iswhite (*lp) && *lp != '(')
				lp += 1;
			      c = *lp++;
			      toklen += lp - oldlp;
			    }
			  token.named = FALSE;
			  if ((c_ext & C_EXT)	/* not pure C */
			      && nestlev > 0 && definedef == dnone)
			    /* in struct body */
			    {
                              write_classname (&token_name, qualifier);
			      linebuffer_setlen (&token_name,
						 token_name.len+qlen+toklen);
			      strcat (token_name.buffer, qualifier);
			      strncat (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      token.named = TRUE;
			    }
			  else if (objdef == ocatseen)
			    /* Objective C category */
			    {
			      int len = strlen (objtag) + 2 + toklen;
			      linebuffer_setlen (&token_name, len);
			      strcpy (token_name.buffer, objtag);
			      strcat (token_name.buffer, "(");
			      strncat (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      strcat (token_name.buffer, ")");
			      token.named = TRUE;
			    }
			  else if (objdef == omethodtag
				   || objdef == omethodparm)
			    /* Objective C method */
			    {
			      token.named = TRUE;
			    }
			  else if (fvdef == fdefunname)
			    /* GNU DEFUN and similar macros */
			    {
			      bool defun = (newlb.buffer[tokoff] == 'F');
			      int off = tokoff;
			      int len = toklen;

			      /* Rewrite the tag so that emacs lisp DEFUNs
				 can be found by their elisp name */
			      if (defun)
				{
				  off += 1;
				  len -= 1;
				}
			      len = toklen;
			      linebuffer_setlen (&token_name, len);
			      strncpy (token_name.buffer,
				       newlb.buffer + off, len);
			      token_name.buffer[len] = '\0';
			      if (defun)
				while (--len >= 0)
				  if (token_name.buffer[len] == '_')
				    token_name.buffer[len] = '-';
			      token.named = defun;
			    }
			  else
			    {
			      linebuffer_setlen (&token_name, toklen);
			      strncpy (token_name.buffer,
				       newlb.buffer + tokoff, toklen);
			      token_name.buffer[toklen] = '\0';
			      /* Name macros and members. */
			      token.named = (structdef == stagseen
					     || typdef == ttypeseen
					     || typdef == tend
					     || (funorvar
						 && definedef == dignorerest)
					     || (funorvar
						 && definedef == dnone
						 && structdef == snone
						 && cblev > 0));
			    }
			  token.lineno = lineno;
			  token.offset = tokoff;
			  token.length = toklen;
			  token.line = newlb.buffer;
			  token.linepos = newlinepos;
			  token.valid = TRUE;

			  if (definedef == dnone
			      && (fvdef == fvnameseen
				  || fvdef == foperator
				  || structdef == stagseen
				  || typdef == tend
				  || typdef == ttypeseen
				  || objdef != onone))
			    {
			      if (current_lb_is_new)
				switch_line_buffers ();
			    }
			  else if (definedef != dnone
				   || fvdef == fdefunname
				   || instruct)
			    make_C_tag (funorvar);
			}
		      midtoken = FALSE;
		    }
		} /* if (endtoken (c)) */
	      else if (intoken (c))
		still_in_token:
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
		  switch (fvdef)
		    {
		    case fstartlist:
		      fvdef = finlist;
		      continue;
		    case flistseen:
		      make_C_tag (TRUE); /* a function */
		      fvdef = fignore;
		      break;
		    case fvnameseen:
		      fvdef = fvnone;
		      break;
		    }
		  if (structdef == stagseen && !cjava)
		    {
		      popclass_above (cblev);
		      structdef = snone;
		    }
		  break;
		case dsharpseen:
		  savetoken = token;
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
	  if (yacc_rules && token.offset == 0 && token.valid)
	    {
	      make_C_tag (FALSE); /* a yacc function */
	      break;
	    }
	  if (definedef != dnone)
	    break;
	  switch (objdef)
	    {
	    case  otagseen:
	      objdef = oignore;
	      make_C_tag (TRUE); /* an Objective C class */
	      break;
	    case omethodtag:
	    case omethodparm:
	      objdef = omethodcolon;
	      linebuffer_setlen (&token_name, token_name.len + 1);
	      strcat (token_name.buffer, ":");
	      break;
	    }
	  if (structdef == stagseen)
	    structdef = scolonseen;
	  break;
	case ';':
	  if (definedef != dnone)
	    break;
	  switch (typdef)
	    {
	    case tend:
	    case ttypeseen:
	      make_C_tag (FALSE); /* a typedef */
	      typdef = tnone;
	      fvdef = fvnone;
	      break;
	    case tnone:
	    case tinbody:
	    case tignore:
	      switch (fvdef)
		{
		case fignore:
		  if (typdef == tignore)
		    fvdef = fvnone;
		  break;
		case fvnameseen:
		  if ((globals && cblev == 0 && (!fvextern || declarations))
		      || (members && instruct))
		    make_C_tag (FALSE); /* a variable */
		  fvextern = FALSE;
		  fvdef = fvnone;
		  token.valid = FALSE;
		  break;
		case flistseen:
		  if ((declarations && typdef == tnone && !instruct)
		      || (members && typdef != tignore && instruct))
		    make_C_tag (TRUE);  /* a function declaration */
		  /* FALLTHRU */
		default:
		  fvextern = FALSE;
		  fvdef = fvnone;
		  if (declarations
		      && structdef == stagseen && (c_ext & C_PLPL))
		    make_C_tag (FALSE);	/* forward declaration */
		  else
		    /* The following instruction invalidates the token.
		       Probably the token should be invalidated in all other
		       cases where some state machine is reset prematurely. */
		    token.valid = FALSE;
		} /* switch (fvdef) */
	      /* FALLTHRU */
	    default:
	      if (!instruct)
		typdef = tnone;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case ',':
	  if (definedef != dnone)
	    break;
	  switch (objdef)
	    {
	    case omethodtag:
	    case omethodparm:
	      make_C_tag (TRUE); /* an Objective C method */
	      objdef = oinbody;
	      break;
	    }
	  switch (fvdef)
	    {
	    case fdefunkey:
	    case foperator:
	    case fstartlist:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fdefunname:
	      fvdef = fignore;
	      break;
	    case fvnameseen:	/* a variable */
	      if ((globals && cblev == 0 && (!fvextern || declarations))
		  || (members && instruct))
		make_C_tag (FALSE);
	      break;
	    case flistseen:	/* a function */
	      if ((declarations && typdef == tnone && !instruct)
		  || (members && typdef != tignore && instruct))
		{
		  make_C_tag (TRUE); /* a function declaration */
		  fvdef = fvnameseen;
		}
	      else if (!declarations)
		fvdef = fvnone;
	      token.valid = FALSE;
	      break;
	    default:
	      fvdef = fvnone;
	    }
	  if (structdef == stagseen)
	    structdef = snone;
	  break;
	case '[':
	  if (definedef != dnone)
	    break;
	  if (structdef == stagseen)
	    structdef = snone;
	  switch (typdef)
	    {
	    case ttypeseen:
	    case tend:
	      typdef = tignore;
	      make_C_tag (FALSE);	/* a typedef */
	      break;
	    case tnone:
	    case tinbody:
	      switch (fvdef)
		{
		case foperator:
		case finlist:
		case fignore:
		case vignore:
		  break;
		case fvnameseen:
		  if ((members && cblev == 1)
		      || (globals && cblev == 0
			  && (!fvextern || declarations)))
		    make_C_tag (FALSE); /* a variable */
		  /* FALLTHRU */
		default:
		  fvdef = fvnone;
		}
	      break;
	    }
	  break;
	case '(':
	  if (definedef != dnone)
	    break;
	  if (objdef == otagseen && parlev == 0)
	    objdef = oparenseen;
	  switch (fvdef)
	    {
	    case fvnameseen:
	      if (typdef == ttypeseen
		  && *lp != '*'
		  && !instruct)
		{
		  /* This handles constructs like:
		     typedef void OperatorFun (int fun); */
		  make_C_tag (FALSE);
		  typdef = tignore;
		  fvdef = fignore;
		  break;
		}
	      /* FALLTHRU */
	    case foperator:
	      fvdef = fstartlist;
	      break;
	    case flistseen:
	      fvdef = finlist;
	      break;
	    }
	  parlev++;
	  break;
	case ')':
	  if (definedef != dnone)
	    break;
	  if (objdef == ocatseen && parlev == 1)
	    {
	      make_C_tag (TRUE); /* an Objective C category */
	      objdef = oignore;
	    }
	  if (--parlev == 0)
	    {
	      switch (fvdef)
		{
		case fstartlist:
		case finlist:
		  fvdef = flistseen;
		  break;
		}
	      if (!instruct
		  && (typdef == tend
		      || typdef == ttypeseen))
		{
		  typdef = tignore;
		  make_C_tag (FALSE); /* a typedef */
		}
	    }
	  else if (parlev < 0)	/* can happen due to ill-conceived #if's. */
	    parlev = 0;
	  break;
	case '{':
	  if (definedef != dnone)
	    break;
	  if (typdef == ttypeseen)
	    {
	      /* Whenever typdef is set to tinbody (currently only
		 here), typdefcblev should be set to cblev. */
	      typdef = tinbody;
	      typdefcblev = cblev;
	    }
	  switch (fvdef)
	    {
	    case flistseen:
	      make_C_tag (TRUE);    /* a function */
	      /* FALLTHRU */
	    case fignore:
	      fvdef = fvnone;
	      break;
	    case fvnone:
	      switch (objdef)
		{
		case otagseen:
		  make_C_tag (TRUE); /* an Objective C class */
		  objdef = oignore;
		  break;
		case omethodtag:
		case omethodparm:
		  make_C_tag (TRUE); /* an Objective C method */
		  objdef = oinbody;
		  break;
		default:
		  /* Neutralize `extern "C" {' grot. */
		  if (cblev == 0 && structdef == snone && nestlev == 0
		      && typdef == tnone)
		    cblev = -1;
		}
	    }
	  switch (structdef)
	    {
	    case skeyseen:	   /* unnamed struct */
	      pushclass_above (cblev, NULL, 0);
	      structdef = snone;
	      break;
	    case stagseen:	   /* named struct or enum */
	    case scolonseen:	   /* a class */
	      pushclass_above (cblev, token.line+token.offset, token.length);
	      structdef = snone;
	      make_C_tag (FALSE);  /* a struct or enum */
	      break;
	    }
	  cblev++;
	  break;
	case '*':
	  if (definedef != dnone)
	    break;
	  if (fvdef == fstartlist)
	    fvdef = fvnone;	/* avoid tagging `foo' in `foo (*bar()) ()' */
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
	  popclass_above (cblev);
	  structdef = snone;
	  /* Only if typdef == tinbody is typdefcblev significant. */
	  if (typdef == tinbody && cblev <= typdefcblev)
	    {
	      assert (cblev == typdefcblev);
	      typdef = tend;
	    }
	  break;
	case '=':
	  if (definedef != dnone)
	    break;
	  switch (fvdef)
	    {
	    case foperator:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    case fvnameseen:
	      if ((members && cblev == 1)
		  || (globals && cblev == 0 && (!fvextern || declarations)))
		make_C_tag (FALSE); /* a variable */
	      /* FALLTHRU */
	    default:
	      fvdef = vignore;
	    }
	  break;
	case '<':
	  if (cplpl && structdef == stagseen)
	    {
	      structdef = sintemplate;
	      break;
	    }
	  goto resetfvdef;
	case '>':
	  if (structdef == sintemplate)
	    {
	      structdef = stagseen;
	      break;
	    }
	  goto resetfvdef;
	case '+':
	case '-':
	  if (objdef == oinbody && cblev == 0)
	    {
	      objdef = omethodsign;
	      break;
	    }
	  /* FALLTHRU */
	resetfvdef:
	case '#': case '~': case '&': case '%': case '/': case '|':
	case '^': case '!': case '.': case '?': case ']':
	  if (definedef != dnone)
	    break;
	  /* These surely cannot follow a function tag in C. */
	  switch (fvdef)
	    {
	    case foperator:
	    case finlist:
	    case fignore:
	    case vignore:
	      break;
	    default:
	      fvdef = fvnone;
	    }
	  break;
	case '\0':
	  if (objdef == otagseen)
	    {
	      make_C_tag (TRUE); /* an Objective C class */
	      objdef = oignore;
	    }
	  /* If a macro spans multiple lines don't reset its state. */
	  if (quotednl)
	    CNL_SAVE_DEFINEDEF ();
	  else
	    CNL ();
	  break;
	} /* switch (c) */

    } /* while not eof */

  free (token_name.buffer);
  free (lbs[0].lb.buffer);
  free (lbs[1].lb.buffer);
}

/*
 * Process either a C++ file or a C file depending on the setting
 * of a global flag.
 */
static void
default_C_entries (inf)
     FILE *inf;
{
  C_entries (cplusplus ? C_PLPL : C_AUTO, inf);
}

/* Always do plain C. */
static void
plain_C_entries (inf)
     FILE *inf;
{
  C_entries (0, inf);
}

/* Always do C++. */
static void
Cplusplus_entries (inf)
     FILE *inf;
{
  C_entries (C_PLPL, inf);
}

/* Always do Java. */
static void
Cjava_entries (inf)
     FILE *inf;
{
  C_entries (C_JAVA, inf);
}

/* Always do C*. */
static void
Cstar_entries (inf)
     FILE *inf;
{
  C_entries (C_STAR, inf);
}

/* Always do Yacc. */
static void
Yacc_entries (inf)
     FILE *inf;
{
  C_entries (YACC, inf);
}


/* Useful macros. */
#define LOOP_ON_INPUT_LINES(file_pointer, line_buffer, char_pointer)	\
  for (lineno = charno = 0;	/* loop initialization */		\
       !feof (file_pointer)	/* loop test */				\
       && (lineno++,		/* instructions at start of loop */	\
	   linecharno = charno,						\
	   charno += readline (&line_buffer, file_pointer),		\
	   char_pointer = lb.buffer,					\
	   TRUE);							\
      )
#define LOOKING_AT(cp, keyword)	/* keyword is a constant string */	\
  (strneq ((cp), keyword, sizeof(keyword)-1) /* cp points at keyword */	\
   && notinname ((cp)[sizeof(keyword)-1])	/* end of keyword */	\
   && ((cp) = skip_spaces((cp)+sizeof(keyword)-1))) /* skip spaces */

/*
 * Read a file, but do no processing.  This is used to do regexp
 * matching on files that have no language defined.
 */
static void
just_read_file (inf)
     FILE *inf;
{
  register char *dummy;

  LOOP_ON_INPUT_LINES (inf, lb, dummy)
    continue;
}


/* Fortran parsing */

static void F_takeprec __P((void));
static void F_getit __P((FILE *));

static void
F_takeprec ()
{
  dbp = skip_spaces (dbp);
  if (*dbp != '*')
    return;
  dbp++;
  dbp = skip_spaces (dbp);
  if (strneq (dbp, "(*)", 3))
    {
      dbp += 3;
      return;
    }
  if (!ISDIGIT (*dbp))
    {
      --dbp;			/* force failure */
      return;
    }
  do
    dbp++;
  while (ISDIGIT (*dbp));
}

static void
F_getit (inf)
     FILE *inf;
{
  register char *cp;

  dbp = skip_spaces (dbp);
  if (*dbp == '\0')
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, inf);
      dbp = lb.buffer;
      if (dbp[5] != '&')
	return;
      dbp += 6;
      dbp = skip_spaces (dbp);
    }
  if (!ISALPHA (*dbp) && *dbp != '_' && *dbp != '$')
    return;
  for (cp = dbp + 1; *cp != '\0' && intoken (*cp); cp++)
    continue;
  pfnote (savenstr (dbp, cp-dbp), TRUE,
	  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
}


static void
Fortran_functions (inf)
     FILE *inf;
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (*dbp == '%')
	dbp++;			/* Ratfor escape to fortran */
      dbp = skip_spaces (dbp);
      if (*dbp == '\0')
	continue;
      switch (lowcase (*dbp))
	{
	case 'i':
	  if (nocase_tail ("integer"))
	    F_takeprec ();
	  break;
	case 'r':
	  if (nocase_tail ("real"))
	    F_takeprec ();
	  break;
	case 'l':
	  if (nocase_tail ("logical"))
	    F_takeprec ();
	  break;
	case 'c':
	  if (nocase_tail ("complex") || nocase_tail ("character"))
	    F_takeprec ();
	  break;
	case 'd':
	  if (nocase_tail ("double"))
	    {
	      dbp = skip_spaces (dbp);
	      if (*dbp == '\0')
		continue;
	      if (nocase_tail ("precision"))
		break;
	      continue;
	    }
	  break;
	}
      dbp = skip_spaces (dbp);
      if (*dbp == '\0')
	continue;
      switch (lowcase (*dbp))
	{
	case 'f':
	  if (nocase_tail ("function"))
	    F_getit (inf);
	  continue;
	case 's':
	  if (nocase_tail ("subroutine"))
	    F_getit (inf);
	  continue;
	case 'e':
	  if (nocase_tail ("entry"))
	    F_getit (inf);
	  continue;
	case 'b':
	  if (nocase_tail ("blockdata") || nocase_tail ("block data"))
	    {
	      dbp = skip_spaces (dbp);
	      if (*dbp == '\0')	/* assume un-named */
		pfnote (savestr ("blockdata"), TRUE,
			lb.buffer, dbp - lb.buffer, lineno, linecharno);
	      else
		F_getit (inf);	/* look for name */
	    }
	  continue;
	}
    }
}


/*
 * Ada parsing
 * Original code by
 * Philippe Waroquiers <philippe.waroquiers@eurocontrol.be> (1998)
 */

static void Ada_getit __P((FILE *, char *));

/* Once we are positioned after an "interesting" keyword, let's get
   the real tag value necessary. */
static void
Ada_getit (inf, name_qualifier)
     FILE *inf;
     char *name_qualifier;
{
  register char *cp;
  char *name;
  char c;

  while (!feof (inf))
    {
      dbp = skip_spaces (dbp);
      if (*dbp == '\0'
	  || (dbp[0] == '-' && dbp[1] == '-'))
	{
	  lineno++;
	  linecharno = charno;
	  charno += readline (&lb, inf);
	  dbp = lb.buffer;
	}
      switch (lowcase(*dbp))
        {
        case 'b':
          if (nocase_tail ("body"))
            {
              /* Skipping body of   procedure body   or   package body or ....
		 resetting qualifier to body instead of spec. */
              name_qualifier = "/b";
              continue;
            }
          break;
        case 't':
          /* Skipping type of   task type   or   protected type ... */
          if (nocase_tail ("type"))
            continue;
          break;
        }
      if (*dbp == '"')
	{
	  dbp += 1;
	  for (cp = dbp; *cp != '\0' && *cp != '"'; cp++)
	    continue;
	}
      else
	{
	  dbp = skip_spaces (dbp);
	  for (cp = dbp;
	       (*cp != '\0'
		&& (ISALPHA (*cp) || ISDIGIT (*cp) || *cp == '_' || *cp == '.'));
	       cp++)
	    continue;
	  if (cp == dbp)
	    return;
	}
      c = *cp;
      *cp = '\0';
      name = concat (dbp, name_qualifier, "");
      *cp = c;
      pfnote (name, TRUE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
      if (c == '"')
	dbp = cp + 1;
      return;
    }
}

static void
Ada_funcs (inf)
     FILE *inf;
{
  bool inquote = FALSE;

  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      while (*dbp != '\0')
	{
	  /* Skip a string i.e. "abcd". */
	  if (inquote || (*dbp == '"'))
	    {
	      dbp = etags_strchr ((inquote) ? dbp : dbp+1, '"');
	      if (dbp != NULL)
		{
		  inquote = FALSE;
		  dbp += 1;
		  continue;	/* advance char */
		}
	      else
		{
		  inquote = TRUE;
		  break;	/* advance line */
		}
	    }

	  /* Skip comments. */
	  if (dbp[0] == '-' && dbp[1] == '-')
	    break;		/* advance line */

	  /* Skip character enclosed in single quote i.e. 'a'
	     and skip single quote starting an attribute i.e. 'Image. */
	  if (*dbp == '\'')
	    {
	      dbp++ ;
	      if (*dbp != '\0')
		dbp++;
	      continue;
	    }

	  /* Search for beginning of a token.  */
	  if (!begtoken (*dbp))
	    {
	      dbp++;
	      continue;		/* advance char */
	    }

	  /* We are at the beginning of a token. */
	  switch (lowcase(*dbp))
	    {
	    case 'f':
	      if (!packages_only && nocase_tail ("function"))
		Ada_getit (inf, "/f");
	      else
		break;		/* from switch */
	      continue;		/* advance char */
	    case 'p':
	      if (!packages_only && nocase_tail ("procedure"))
		Ada_getit (inf, "/p");
	      else if (nocase_tail ("package"))
		Ada_getit (inf, "/s");
	      else if (nocase_tail ("protected")) /* protected type */
		Ada_getit (inf, "/t");
	      else
		break;		/* from switch */
	      continue;		/* advance char */
	    case 't':
	      if (!packages_only && nocase_tail ("task"))
		Ada_getit (inf, "/k");
	      else if (typedefs && !packages_only && nocase_tail ("type"))
		{
		  Ada_getit (inf, "/t");
		  while (*dbp != '\0')
		    dbp += 1;
		}
	      else
		break;		/* from switch */
	      continue;		/* advance char */
	    }

	  /* Look for the end of the token. */
	  while (!endtoken (*dbp))
	    dbp++;

	} /* advance char */
    } /* advance line */
}


/*
 * Unix and microcontroller assembly tag handling
 * Labels:  /^[a-zA-Z_.$][a-zA_Z0-9_.$]*[: ^I^J]/
 * Idea by Bob Weiner, Motorola Inc. (1994)
 */
static void
Asm_labels (inf)
     FILE *inf;
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      /* If first char is alphabetic or one of [_.$], test for colon
	 following identifier. */
      if (ISALPHA (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	{
 	  /* Read past label. */
	  cp++;
 	  while (ISALNUM (*cp) || *cp == '_' || *cp == '.' || *cp == '$')
 	    cp++;
 	  if (*cp == ':' || iswhite (*cp))
 	    {
 	      /* Found end of label, so copy it and add it to the table. */
 	      pfnote (savenstr(lb.buffer, cp-lb.buffer), TRUE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
 	    }
 	}
    }
}


/*
 * Perl support
 * Perl sub names: /^sub[ \t\n]+[^ \t\n{]+/
 * Perl variable names: /^(my|local).../
 * Original code by Bart Robinson <lomew@cs.utah.edu> (1995)
 * Additions by Michael Ernst <mernst@alum.mit.edu> (1997)
 * Ideas by Kai Großjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE> (2001)
 */
static void
Perl_functions (inf)
     FILE *inf;
{
  char *package = savestr ("main"); /* current package name */
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      skip_spaces(cp);

      if (LOOKING_AT (cp, "package"))
	{
	  free (package);
	  package = get_tag (cp);
	  if (package == NULL)	/* can't parse package name */
	    package = savestr ("");
	  else
	    package = savestr(package);	/* make a copy */
	}
      else if (LOOKING_AT (cp, "sub"))
	{
	  char *name, *fullname, *pos;
	  char *sp = cp;

	  while (!notinname (*cp))
	    cp++;
	  if (cp == sp)
	    continue;
	  name = savenstr (sp, cp-sp);
	  if ((pos = etags_strchr (name, ':')) != NULL && pos[1] == ':')
	    fullname = name;
	  else
	    fullname = concat (package, "::", name);
	  pfnote (fullname, TRUE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	  if (name != fullname)
	    free (name);
 	}
       else if (globals		/* only if tagging global vars is enabled */
		&& (LOOKING_AT (cp, "my") || LOOKING_AT (cp, "local")))
 	{
 	  /* After "my" or "local", but before any following paren or space. */
 	  char *varname = NULL;

 	  if (*cp == '$' || *cp == '@' || *cp == '%')
 	    {
 	      char* varstart = ++cp;
 	      while (ISALNUM (*cp) || *cp == '_')
 		cp++;
 	      varname = savenstr (varstart, cp-varstart);
 	    }
 	  else
 	    {
 	      /* Should be examining a variable list at this point;
 		 could insist on seeing an open parenthesis. */
 	      while (*cp != '\0' && *cp != ';' && *cp != '=' &&  *cp != ')')
 		cp++;
 	    }

 	  /* Perhaps I should back cp up one character, so the TAGS table
 	     doesn't mention (and so depend upon) the following char. */
 	  pfnote (varname, FALSE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}


/*
 * Python support
 * Look for /^[\t]*def[ \t\n]+[^ \t\n(:]+/ or /^class[ \t\n]+[^ \t\n(:]+/
 * Idea by Eric S. Raymond <esr@thyrsus.com> (1997)
 * More ideas by seb bacon <seb@jamkit.com> (2002)
 */
static void
Python_functions (inf)
     FILE *inf;
{
  register char *cp;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      cp = skip_spaces (cp);
      if (LOOKING_AT (cp, "def") || LOOKING_AT (cp, "class"))
	{
	  char *name = cp;
	  while (!notinname (*cp) && *cp != ':')
	    cp++;
	  pfnote (savenstr (name, cp-name), TRUE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}


/*
 * PHP support
 * Look for:
 *  - /^[ \t]*function[ \t\n]+[^ \t\n(]+/
 *  - /^[ \t]*class[ \t\n]+[^ \t\n]+/
 *  - /^[ \t]*define\(\"[^\"]+/
 * Only with --members:
 *  - /^[ \t]*var[ \t\n]+\$[^ \t\n=;]/
 * Idea by Diez B. Roggisch (2001)
 */
static void
PHP_functions (inf)
     FILE *inf;
{
  register char *cp, *name;
  bool search_identifier = FALSE;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      cp = skip_spaces (cp);
      name = cp;
      if (search_identifier
	  && *cp != '\0')
	{
	  while (!notinname (*cp))
	    cp++;
	  pfnote (savenstr (name, cp-name), TRUE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	  search_identifier = FALSE;
	}
      else if (LOOKING_AT (cp, "function"))
	{
	  if(*cp == '&')
	    cp = skip_spaces (cp+1);
	  if(*cp != '\0')
	    {
	      name = cp;
	      while (!notinname (*cp))
		cp++;
	      pfnote (savenstr (name, cp-name), TRUE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	    }
	  else
	    search_identifier = TRUE;
	}
      else if (LOOKING_AT (cp, "class"))
	{
	  if (*cp != '\0')
	    {
	      name = cp;
	      while (*cp != '\0' && !iswhite (*cp))
		cp++;
	      pfnote (savenstr (name, cp-name), FALSE,
		      lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	    }
	  else
	    search_identifier = TRUE;
	}
      else if (strneq (cp, "define", 6)
	       && (cp = skip_spaces (cp+6))
	       && *cp++ == '('
	       && (*cp == '"' || *cp == '\''))
	{
	  char quote = *cp++;
	  name = cp;
	  while (*cp != quote && *cp != '\0')
	    cp++;
	  pfnote (savenstr (name, cp-name), FALSE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
      else if (members
	       && LOOKING_AT (cp, "var")
	       && *cp == '$')
	{
	  name = cp;
	  while (!notinname(*cp))
	    cp++;
	  pfnote (savenstr (name, cp-name), FALSE,
		  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
	}
    }
}


/*
 * Cobol tag functions
 * We could look for anything that could be a paragraph name.
 * i.e. anything that starts in column 8 is one word and ends in a full stop.
 * Idea by Corny de Souza (1993)
 */
static void
Cobol_paragraphs (inf)
     FILE *inf;
{
  register char *bp, *ep;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (lb.len < 9)
	continue;
      bp += 8;

      /* If eoln, compiler option or comment ignore whole line. */
      if (bp[-1] != ' ' || !ISALNUM (bp[0]))
        continue;

      for (ep = bp; ISALNUM (*ep) || *ep == '-'; ep++)
	continue;
      if (*ep++ == '.')
	pfnote (savenstr (bp, ep-bp), TRUE,
		lb.buffer, ep - lb.buffer + 1, lineno, linecharno);
    }
}


/*
 * Makefile support
 * Idea by Assar Westerlund <assar@sics.se> (2001)
 */
static void
Makefile_targets (inf)
     FILE *inf;
{
  register char *bp;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (*bp == '\t' || *bp == '#')
	continue;
      while (*bp != '\0' && *bp != '=' && *bp != ':')
	bp++;
      if (*bp == ':')
	pfnote (savenstr (lb.buffer, bp - lb.buffer), TRUE,
		lb.buffer, bp - lb.buffer + 1, lineno, linecharno);
    }
}


/*
 * Pascal parsing
 * Original code by Mosur K. Mohan (1989)
 *
 *  Locates tags for procedures & functions.  Doesn't do any type- or
 *  var-definitions.  It does look for the keyword "extern" or
 *  "forward" immediately following the procedure statement; if found,
 *  the tag is skipped.
 */
static void
Pascal_functions (inf)
     FILE *inf;
{
  linebuffer tline;		/* mostly copied from C_entries */
  long save_lcno;
  int save_lineno, save_len;
  char c, *cp, *namebuf;

  bool				/* each of these flags is TRUE iff: */
    incomment,			/* point is inside a comment */
    inquote,			/* point is inside '..' string */
    get_tagname,		/* point is after PROCEDURE/FUNCTION
				   keyword, so next item = potential tag */
    found_tag,			/* point is after a potential tag */
    inparms,			/* point is within parameter-list */
    verify_tag;			/* point has passed the parm-list, so the
				   next token will determine whether this
				   is a FORWARD/EXTERN to be ignored, or
				   whether it is a real tag */

  save_lcno = save_lineno = save_len = 0; /* keep compiler quiet */
  namebuf = NULL;		/* keep compiler quiet */
  lineno = 0;
  charno = 0;
  dbp = lb.buffer;
  *dbp = '\0';
  initbuffer (&tline);

  incomment = inquote = FALSE;
  found_tag = FALSE;		/* have a proc name; check if extern */
  get_tagname = FALSE;		/* have found "procedure" keyword    */
  inparms = FALSE;		/* found '(' after "proc"            */
  verify_tag = FALSE;		/* check if "extern" is ahead        */


  while (!feof (inf))		/* long main loop to get next char */
    {
      c = *dbp++;
      if (c == '\0')		/* if end of line */
	{
	  lineno++;
	  linecharno = charno;
	  charno += readline (&lb, inf);
	  dbp = lb.buffer;
	  if (*dbp == '\0')
	    continue;
	  if (!((found_tag && verify_tag)
		|| get_tagname))
	    c = *dbp++;		/* only if don't need *dbp pointing
				   to the beginning of the name of
				   the procedure or function */
	}
      if (incomment)
	{
	  if (c == '}')		/* within { } comments */
	    incomment = FALSE;
	  else if (c == '*' && *dbp == ')') /* within (* *) comments */
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
	  case '{':		/* found open { comment */
	    incomment = TRUE;
	    continue;
	  case '(':
	    if (*dbp == '*')	/* found open (* comment */
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
	    if (found_tag && !inparms) /* end of proc or fn stmt */
	      {
		verify_tag = TRUE;
		break;
	      }
	    continue;
	  }
      if (found_tag && verify_tag && (*dbp != ' '))
	{
	  /* check if this is an "extern" declaration */
	  if (*dbp == '\0')
	    continue;
	  if (lowcase (*dbp == 'e'))
	    {
	      if (nocase_tail ("extern")) /* superfluous, really! */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  else if (lowcase (*dbp) == 'f')
	    {
	      if (nocase_tail ("forward")) /*  check for forward reference */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  if (found_tag && verify_tag) /* not external proc, so make tag */
	    {
	      found_tag = FALSE;
	      verify_tag = FALSE;
	      pfnote (namebuf, TRUE,
		      tline.buffer, save_len, save_lineno, save_lcno);
	      continue;
	    }
	}
      if (get_tagname)		/* grab name of proc or fn */
	{
	  if (*dbp == '\0')
	    continue;

	  /* save all values for later tagging */
	  linebuffer_setlen (&tline, lb.len);
	  strcpy (tline.buffer, lb.buffer);
	  save_lineno = lineno;
	  save_lcno = linecharno;

	  /* grab block name */
	  for (cp = dbp + 1; *cp != '\0' && !endtoken (*cp); cp++)
	    continue;
	  namebuf = savenstr (dbp, cp-dbp);
	  dbp = cp;		/* set dbp to e-o-token */
	  save_len = dbp - lb.buffer + 1;
	  get_tagname = FALSE;
	  found_tag = TRUE;
	  continue;

	  /* and proceed to check for "extern" */
	}
      else if (!incomment && !inquote && !found_tag)
	{
	  /* check for proc/fn keywords */
	  switch (lowcase (c))
	    {
	    case 'p':
	      if (nocase_tail ("rocedure")) /* c = 'p', dbp has advanced */
		get_tagname = TRUE;
	      continue;
	    case 'f':
	      if (nocase_tail ("unction"))
		get_tagname = TRUE;
	      continue;
	    }
	}
    }				/* while not eof */

  free (tline.buffer);
}


/*
 * Lisp tag functions
 *  look for (def or (DEF, quote or QUOTE
 */

static void L_getit __P((void));

static void
L_getit ()
{
  if (*dbp == '\'')		/* Skip prefix quote */
    dbp++;
  else if (*dbp == '(')
  {
    dbp++;
    /* Try to skip "(quote " */
    if (!LOOKING_AT (dbp, "quote") && !LOOKING_AT (dbp, "QUOTE"))
      /* Ok, then skip "(" before name in (defstruct (foo)) */
      dbp = skip_spaces (dbp);
  }
  get_tag (dbp);
}

static void
Lisp_functions (inf)
     FILE *inf;
{
  LOOP_ON_INPUT_LINES (inf, lb, dbp)
    {
      if (dbp[0] != '(')
	continue;

      if (strneq (dbp+1, "def", 3) || strneq (dbp+1, "DEF", 3))
	{
	  dbp = skip_non_spaces (dbp);
	  dbp = skip_spaces (dbp);
	  L_getit ();
	}
      else
	{
	  /* Check for (foo::defmumble name-defined ... */
	  do
	    dbp++;
	  while (!notinname (*dbp) && *dbp != ':');
	  if (*dbp == ':')
	    {
	      do
		dbp++;
	      while (*dbp == ':');

	      if (strneq (dbp, "def", 3) || strneq (dbp, "DEF", 3))
		{
		  dbp = skip_non_spaces (dbp);
		  dbp = skip_spaces (dbp);
		  L_getit ();
		}
	    }
	}
    }
}


/*
 * Postscript tag functions
 * Just look for lines where the first character is '/'
 * Also look at "defineps" for PSWrap
 * Ideas by:
 *   Richard Mlynarik <mly@adoc.xerox.com> (1997)
 *   Masatake Yamato <masata-y@is.aist-nara.ac.jp> (1999)
 */
static void
Postscript_functions (inf)
     FILE *inf;
{
  register char *bp, *ep;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (bp[0] == '/')
	{
	  for (ep = bp+1;
	       *ep != '\0' && *ep != ' ' && *ep != '{';
	       ep++)
	    continue;
	  pfnote (savenstr (bp, ep-bp), TRUE,
		  lb.buffer, ep - lb.buffer + 1, lineno, linecharno);
	}
      else if (LOOKING_AT (bp, "defineps"))
	get_tag (bp);
    }
}


/*
 * Scheme tag functions
 * look for (def... xyzzy
 *          (def... (xyzzy
 *          (def ... ((...(xyzzy ....
 *          (set! xyzzy
 * Original code by Ken Haase (1985?)
 */

static void
Scheme_functions (inf)
     FILE *inf;
{
  register char *bp;

  LOOP_ON_INPUT_LINES (inf, lb, bp)
    {
      if (strneq (bp, "(def", 4) || strneq (bp, "(DEF", 4))
	{
	  bp = skip_non_spaces (bp+4);
	  /* Skip over open parens and white space */
	  while (notinname (*bp))
	    bp++;
	  get_tag (bp);
	}
      if (LOOKING_AT (bp, "(SET!") || LOOKING_AT (bp, "(set!"))
	get_tag (bp);
    }
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

static struct TEX_tabent *TEX_toktab = NULL; /* Table with tag tokens */

/* Default set of control sequences to put into TEX_toktab.
   The value of environment var TEXTAGS is prepended to this.  */

static char *TEX_defenv = "\
:chapter:section:subsection:subsubsection:eqno:label:ref:cite:bibitem\
:part:appendix:entry:index";

static void TEX_mode __P((FILE *));
static struct TEX_tabent *TEX_decode_env __P((char *, char *));
static int TEX_Token __P((char *));

static char TEX_esc = '\\';
static char TEX_opgrp = '{';
static char TEX_clgrp = '}';

/*
 * TeX/LaTeX scanning loop.
 */
static void
TeX_commands (inf)
     FILE *inf;
{
  char *cp, *lasthit;
  register int i;

  /* Select either \ or ! as escape character.  */
  TEX_mode (inf);

  /* Initialize token table once from environment. */
  if (!TEX_toktab)
    TEX_toktab = TEX_decode_env ("TEXTAGS", TEX_defenv);

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      lasthit = cp;
      /* Look at each esc in line. */
      while ((cp = etags_strchr (cp, TEX_esc)) != NULL)
	{
	  if (*++cp == '\0')
	    break;
	  linecharno += cp - lasthit;
	  lasthit = cp;
	  i = TEX_Token (lasthit);
	  if (i >= 0)
	    {
	      register char *p;
	      for (lasthit += TEX_toktab[i].len;
		   *lasthit == TEX_esc || *lasthit == TEX_opgrp;
		   lasthit++)
		continue;
	      for (p = lasthit;
		   !iswhite (*p) && *p != TEX_opgrp && *p != TEX_clgrp;
		   p++)
		continue;
	      pfnote (savenstr (lasthit, p-lasthit), TRUE,
		      lb.buffer, lb.len, lineno, linecharno);
	      break;		/* We only tag a line once */
	    }
	}
    }
}

#define TEX_LESC '\\'
#define TEX_SESC '!'
#define TEX_cmt  '%'

/* Figure out whether TeX's escapechar is '\\' or '!' and set grouping
   chars accordingly. */
static void
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
  /* If the input file is compressed, inf is a pipe, and rewind may fail.
     No attempt is made to correct the situation. */
  rewind (inf);
}

/* Read environment and prepend it to the default string.
   Build token table. */
static struct TEX_tabent *
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
    {
      char *oldenv = env;
      env = concat (oldenv, defenv, "");
    }

  /* Allocate a token table */
  for (size = 1, p = env; p;)
    if ((p = etags_strchr (p, ':')) && *++p != '\0')
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

/* If the text at CP matches one of the tag-defining TeX command names,
   return the pointer to the first occurrence of that command in TEX_toktab.
   Otherwise return -1.
   Keep the capital `T' in `token' for dumb truncating compilers
   (this distinguishes it from `TEX_toktab' */
static int
TEX_Token (cp)
     char *cp;
{
  int i;

  for (i = 0; TEX_toktab[i].len > 0; i++)
    if (strneq (TEX_toktab[i].name, cp, TEX_toktab[i].len))
      return i;
  return -1;
}


/* Texinfo support.  Dave Love, Mar. 2000.  */
static void
Texinfo_nodes (inf)
     FILE * inf;
{
  char *cp, *start;
  LOOP_ON_INPUT_LINES (inf, lb, cp)
    if (LOOKING_AT (cp, "@node"))
      {
	start = cp;
	while (*cp != '\0' && *cp != ',')
	  cp++;
	pfnote (savenstr (start, cp - start), TRUE,
		lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
      }
}


/*
 * Prolog support
 *
 * Assumes that the predicate or rule starts at column 0.
 * Only the first clause of a predicate or rule is added.
 * Original code by Sunichirou Sugou (1989)
 * Rewritten by Anders Lindgren (1996)
 */
static int prolog_pr __P((char *, char *));
static void prolog_skip_comment __P((linebuffer *, FILE *));
static int prolog_atom __P((char *, int));

static void
Prolog_functions (inf)
     FILE *inf;
{
  char *cp, *last;
  int len;
  int allocated;

  allocated = 0;
  len = 0;
  last = NULL;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (cp[0] == '\0')	/* Empty line */
	continue;
      else if (iswhite (cp[0])) /* Not a predicate */
	continue;
      else if (cp[0] == '/' && cp[1] == '*')	/* comment. */
	prolog_skip_comment (&lb, inf);
      else if ((len = prolog_pr (cp, last)) > 0)
	{
	  /* Predicate or rule.  Store the function name so that we
	     only generate a tag for the first clause.  */
	  if (last == NULL)
	    last = xnew(len + 1, char);
	  else if (len + 1 > allocated)
	    xrnew (last, len + 1, char);
	  allocated = len + 1;
	  strncpy (last, cp, len);
	  last[len] = '\0';
	}
    }
}


static void
prolog_skip_comment (plb, inf)
     linebuffer *plb;
     FILE *inf;
{
  char *cp;

  do
    {
      for (cp = plb->buffer; *cp != '\0'; cp++)
	if (cp[0] == '*' && cp[1] == '/')
	  return;
      lineno++;
      linecharno += readline (plb, inf);
    }
  while (!feof(inf));
}

/*
 * A predicate or rule definition is added if it matches:
 *     <beginning of line><Prolog Atom><whitespace>(
 * or  <beginning of line><Prolog Atom><whitespace>:-
 *
 * It is added to the tags database if it doesn't match the
 * name of the previous clause header.
 *
 * Return the size of the name of the predicate or rule, or 0 if no
 * header was found.
 */
static int
prolog_pr (s, last)
     char *s;
     char *last;		/* Name of last clause. */
{
  int pos;
  int len;

  pos = prolog_atom (s, 0);
  if (pos < 1)
    return 0;

  len = pos;
  pos = skip_spaces (s + pos) - s;

  if ((s[pos] == '.'
       || (s[pos] == '(' && (pos += 1))
       || (s[pos] == ':' && s[pos + 1] == '-' && (pos += 2)))
      && (last == NULL		/* save only the first clause */
	  || len != strlen (last)
	  || !strneq (s, last, len)))
	{
	  pfnote (savenstr (s, len), TRUE, s, pos, lineno, linecharno);
	  return len;
	}
  else
    return 0;
}

/*
 * Consume a Prolog atom.
 * Return the number of bytes consumed, or -1 if there was an error.
 *
 * A prolog atom, in this context, could be one of:
 * - An alphanumeric sequence, starting with a lower case letter.
 * - A quoted arbitrary string. Single quotes can escape themselves.
 *   Backslash quotes everything.
 */
static int
prolog_atom (s, pos)
     char *s;
     int pos;
{
  int origpos;

  origpos = pos;

  if (ISLOWER(s[pos]) || (s[pos] == '_'))
    {
      /* The atom is unquoted. */
      pos++;
      while (ISALNUM(s[pos]) || (s[pos] == '_'))
	{
	  pos++;
	}
      return pos - origpos;
    }
  else if (s[pos] == '\'')
    {
      pos++;

      while (1)
	{
	  if (s[pos] == '\'')
	    {
	      pos++;
	      if (s[pos] != '\'')
		break;
	      pos++;		/* A double quote */
	    }
	  else if (s[pos] == '\0')
	    /* Multiline quoted atoms are ignored. */
	    return -1;
	  else if (s[pos] == '\\')
	    {
	      if (s[pos+1] == '\0')
		return -1;
	      pos += 2;
	    }
	  else
	    pos++;
	}
      return pos - origpos;
    }
  else
    return -1;
}


/*
 * Support for Erlang
 *
 * Generates tags for functions, defines, and records.
 * Assumes that Erlang functions start at column 0.
 * Original code by Anders Lindgren (1996)
 */
static int erlang_func __P((char *, char *));
static void erlang_attribute __P((char *));
static int erlang_atom __P((char *, int));

static void
Erlang_functions (inf)
     FILE *inf;
{
  char *cp, *last;
  int len;
  int allocated;

  allocated = 0;
  len = 0;
  last = NULL;

  LOOP_ON_INPUT_LINES (inf, lb, cp)
    {
      if (cp[0] == '\0')	/* Empty line */
	continue;
      else if (iswhite (cp[0])) /* Not function nor attribute */
	continue;
      else if (cp[0] == '%')	/* comment */
	continue;
      else if (cp[0] == '"')	/* Sometimes, strings start in column one */
	continue;
      else if (cp[0] == '-') 	/* attribute, e.g. "-define" */
	{
	  erlang_attribute (cp);
	  last = NULL;
	}
      else if ((len = erlang_func (cp, last)) > 0)
	{
	  /*
	   * Function.  Store the function name so that we only
	   * generates a tag for the first clause.
	   */
	  if (last == NULL)
	    last = xnew (len + 1, char);
	  else if (len + 1 > allocated)
	    xrnew (last, len + 1, char);
	  allocated = len + 1;
	  strncpy (last, cp, len);
	  last[len] = '\0';
	}
    }
}


/*
 * A function definition is added if it matches:
 *     <beginning of line><Erlang Atom><whitespace>(
 *
 * It is added to the tags database if it doesn't match the
 * name of the previous clause header.
 *
 * Return the size of the name of the function, or 0 if no function
 * was found.
 */
static int
erlang_func (s, last)
     char *s;
     char *last;		/* Name of last clause. */
{
  int pos;
  int len;

  pos = erlang_atom (s, 0);
  if (pos < 1)
    return 0;

  len = pos;
  pos = skip_spaces (s + pos) - s;

  /* Save only the first clause. */
  if (s[pos++] == '('
      && (last == NULL
	  || len != (int)strlen (last)
	  || !strneq (s, last, len)))
	{
	  pfnote (savenstr (s, len), TRUE, s, pos, lineno, linecharno);
	  return len;
	}

  return 0;
}


/*
 * Handle attributes.  Currently, tags are generated for defines
 * and records.
 *
 * They are on the form:
 * -define(foo, bar).
 * -define(Foo(M, N), M+N).
 * -record(graph, {vtab = notable, cyclic = true}).
 */
static void
erlang_attribute (s)
     char *s;
{
  int pos;
  int len;

  if (LOOKING_AT (s, "-define") || LOOKING_AT (s, "-record"))
    {
      if (s[pos++] == '(')
	{
	  pos = skip_spaces (s + pos) - s;
	  len = erlang_atom (s, pos);
	  if (len != 0)
	    pfnote (savenstr (& s[pos], len), TRUE,
		    s, pos + len, lineno, linecharno);
	}
    }
  return;
}


/*
 * Consume an Erlang atom (or variable).
 * Return the number of bytes consumed, or -1 if there was an error.
 */
static int
erlang_atom (s, pos)
     char *s;
     int pos;
{
  int origpos;

  origpos = pos;

  if (ISALPHA (s[pos]) || s[pos] == '_')
    {
      /* The atom is unquoted. */
      pos++;
      while (ISALNUM (s[pos]) || s[pos] == '_')
	pos++;
      return pos - origpos;
    }
  else if (s[pos] == '\'')
    {
      pos++;

      while (1)
	{
	  if (s[pos] == '\'')
	    {
	      pos++;
	      break;
	    }
	  else if (s[pos] == '\0')
	    /* Multiline quoted atoms are ignored. */
	    return -1;
	  else if (s[pos] == '\\')
	    {
	      if (s[pos+1] == '\0')
		return -1;
	      pos += 2;
	    }
	  else
	    pos++;
	}
      return pos - origpos;
    }
  else
    return -1;
}


#ifdef ETAGS_REGEXPS

static char *scan_separators __P((char *));
static void analyse_regex __P((char *, bool));
static void add_regex __P((char *, bool, language *));
static char *substitute __P((char *, char *, struct re_registers *));

/* Take a string like "/blah/" and turn it into "blah", making sure
   that the first and last characters are the same, and handling
   quoted separator characters.  Actually, stops on the occurrence of
   an unquoted separator.  Also turns "\t" into a Tab character.
   Returns pointer to terminating separator.  Works in place.  Null
   terminates name string. */
static char *
scan_separators (name)
     char *name;
{
  char sep = name[0];
  char *copyto = name;
  bool quoted = FALSE;

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

/* Look at the argument of --regex or --no-regex and do the right
   thing.  Same for each line of a regexp file. */
static void
analyse_regex (regex_arg, ignore_case)
     char *regex_arg;
     bool ignore_case;
{
  if (regex_arg == NULL)
    {
      free_patterns ();		/* --no-regex: remove existing regexps */
      return;
    }

  /* A real --regexp option or a line in a regexp file. */
  switch (regex_arg[0])
    {
      /* Comments in regexp file or null arg to --regex. */
    case '\0':
    case ' ':
    case '\t':
      break;

      /* Read a regex file.  This is recursive and may result in a
	 loop, which will stop when the file descriptors are exhausted. */
    case '@':
      {
	FILE *regexfp;
	linebuffer regexbuf;
	char *regexfile = regex_arg + 1;

	/* regexfile is a file containing regexps, one per line. */
	regexfp = fopen (regexfile, "r");
	if (regexfp == NULL)
	  {
	    pfatal (regexfile);
	    return;
	  }
	initbuffer (&regexbuf);
	while (readline_internal (&regexbuf, regexfp) > 0)
	  analyse_regex (regexbuf.buffer, ignore_case);
	free (regexbuf.buffer);
	fclose (regexfp);
      }
      break;

      /* Regexp to be used for a specific language only. */
    case '{':
      {
	language *lang;
	char *lang_name = regex_arg + 1;
	char *cp;

	for (cp = lang_name; *cp != '}'; cp++)
	  if (*cp == '\0')
	    {
	      error ("unterminated language name in regex: %s", regex_arg);
	      return;
	    }
	*cp = '\0';
	lang = get_language_from_langname (lang_name);
	if (lang == NULL)
	  return;
	add_regex (cp + 1, ignore_case, lang);
      }
      break;

      /* Regexp to be used for any language. */
    default:
      add_regex (regex_arg, ignore_case, NULL);
      break;
    }
}

/* Turn a name, which is an ed-style (but Emacs syntax) regular
   expression, into a real regular expression by compiling it. */
static void
add_regex (regexp_pattern, ignore_case, lang)
     char *regexp_pattern;
     bool ignore_case;
     language *lang;
{
  static struct re_pattern_buffer zeropattern;
  char *name;
  const char *err;
  struct re_pattern_buffer *patbuf;
  pattern *pp;


  if (regexp_pattern[strlen(regexp_pattern)-1] != regexp_pattern[0])
    {
      error ("%s: unterminated regexp", regexp_pattern);
      return;
    }
  name = scan_separators (regexp_pattern);
  if (regexp_pattern[0] == '\0')
    {
      error ("null regexp", (char *)NULL);
      return;
    }
  (void) scan_separators (name);

  patbuf = xnew (1, struct re_pattern_buffer);
  *patbuf = zeropattern;
  if (ignore_case)
    patbuf->translate = lc_trans;	/* translation table to fold case  */

  err = re_compile_pattern (regexp_pattern, strlen (regexp_pattern), patbuf);
  if (err != NULL)
    {
      error ("%s while compiling pattern", err);
      return;
    }

  pp = p_head;
  p_head = xnew (1, pattern);
  p_head->regex = savestr (regexp_pattern);
  p_head->p_next = pp;
  p_head->lang = lang;
  p_head->pat = patbuf;
  p_head->name_pattern = savestr (name);
  p_head->error_signaled = FALSE;
  p_head->ignore_case = ignore_case;
}

/*
 * Do the substitutions indicated by the regular expression and
 * arguments.
 */
static char *
substitute (in, out, regs)
     char *in, *out;
     struct re_registers *regs;
{
  char *result, *t;
  int size, dig, diglen;

  result = NULL;
  size = strlen (out);

  /* Pass 1: figure out how much to allocate by finding all \N strings. */
  if (out[size - 1] == '\\')
    fatal ("pattern error in \"%s\"", out);
  for (t = etags_strchr (out, '\\');
       t != NULL;
       t = etags_strchr (t + 2, '\\'))
    if (ISDIGIT (t[1]))
      {
	dig = t[1] - '0';
	diglen = regs->end[dig] - regs->start[dig];
	size += diglen - 2;
      }
    else
      size -= 1;

  /* Allocate space and do the substitutions. */
  result = xnew (size + 1, char);

  for (t = result; *out != '\0'; out++)
    if (*out == '\\' && ISDIGIT (*++out))
      {
	dig = *out - '0';
	diglen = regs->end[dig] - regs->start[dig];
	strncpy (t, in + regs->start[dig], diglen);
	t += diglen;
      }
    else
      *t++ = *out;
  *t = '\0';

  assert (t <= result + size && t - result == (int)strlen (result));

  return result;
}

/* Deallocate all patterns. */
static void
free_patterns ()
{
  pattern *pp;
  while (p_head != NULL)
    {
      pp = p_head->p_next;
      free (p_head->regex);
      free (p_head->name_pattern);
      free (p_head);
      p_head = pp;
    }
  return;
}
#endif /* ETAGS_REGEXPS */


static bool
nocase_tail (cp)
     char *cp;
{
  register int len = 0;

  while (*cp != '\0' && lowcase (*cp) == lowcase (dbp[len]))
    cp++, len++;
  if (*cp == '\0' && !intoken (dbp[len]))
    {
      dbp += len;
      return TRUE;
    }
  return FALSE;
}

static char *
get_tag (bp)
     register char *bp;
{
  register char *cp, *name;

  if (*bp == '\0')
    return NULL;
  /* Go till you get to white space or a syntactic break */
  for (cp = bp + 1; !notinname (*cp); cp++)
    continue;
  name = savenstr (bp, cp-bp);
  pfnote (name, TRUE,
	  lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
  return name;
}

/* Initialize a linebuffer for use */
static void
initbuffer (lbp)
     linebuffer *lbp;
{
  lbp->size = (DEBUG) ? 3 : 200;
  lbp->buffer = xnew (lbp->size, char);
  lbp->buffer[0] = '\0';
  lbp->len = 0;
}

/*
 * Read a line of text from `stream' into `lbp', excluding the
 * newline or CR-NL, if any.  Return the number of characters read from
 * `stream', which is the length of the line including the newline.
 *
 * On DOS or Windows we do not count the CR character, if any, before the
 * NL, in the returned length; this mirrors the behavior of emacs on those
 * platforms (for text files, it translates CR-NL to NL as it reads in the
 * file).
 */
static long
readline_internal (lbp, stream)
     linebuffer *lbp;
     register FILE *stream;
{
  char *buffer = lbp->buffer;
  register char *p = lbp->buffer;
  register char *pend;
  int chars_deleted;

  pend = p + lbp->size;		/* Separate to avoid 386/IX compiler bug.  */

  while (1)
    {
      register int c = getc (stream);
      if (p == pend)
	{
	  /* We're at the end of linebuffer: expand it. */
	  lbp->size *= 2;
	  xrnew (buffer, lbp->size, char);
	  p += buffer - lbp->buffer;
	  pend = buffer + lbp->size;
	  lbp->buffer = buffer;
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
	      p -= 1;
#ifdef DOS_NT
	     /* Assume CRLF->LF translation will be performed by Emacs
		when loading this file, so CRs won't appear in the buffer.
		It would be cleaner to compensate within Emacs;
		however, Emacs does not know how many CRs were deleted
		before any given point in the file.  */
	      chars_deleted = 1;
#else
	      chars_deleted = 2;
#endif
	    }
	  else
	    {
	      chars_deleted = 1;
	    }
	  *p = '\0';
	  break;
	}
      *p++ = c;
    }
  lbp->len = p - buffer;

  return lbp->len + chars_deleted;
}

/*
 * Like readline_internal, above, but in addition try to match the
 * input line against relevant regular expressions.
 */
static long
readline (lbp, stream)
     linebuffer *lbp;
     FILE *stream;
{
  /* Read new line. */
  long result = readline_internal (lbp, stream);

  /* Honour #line directives. */
  if (!no_line_directive)
    {
      static bool discard_until_line_directive;

      /* Check whether this is a #line directive. */
      if (result > 12 && strneq (lbp->buffer, "#line ", 6))
	{
	  int start, lno;

	  if (DEBUG) start = 0;	/* shut up the compiler */
	  if (sscanf (lbp->buffer, "#line %d \"%n", &lno, &start) == 1)
	    {
	      char *endp = lbp->buffer + start;

	      assert (start > 0);
	      while ((endp = etags_strchr (endp, '"')) != NULL
		     && endp[-1] == '\\')
		endp++;
	      if (endp != NULL)
		/* Ok, this is a real #line directive.  Let's deal with it. */
		{
		  char *taggedabsname;	/* absolute name of original file */
		  char *taggedfname;	/* name of original file as given */
		  char *name;		/* temp var */

		  discard_until_line_directive = FALSE; /* found it */
		  name = lbp->buffer + start;
		  *endp = '\0';
		  canonicalize_filename (name); /* for DOS */
		  taggedabsname = absolute_filename (name, curfdp->infabsdir);
		  if (filename_is_absolute (name)
		      || filename_is_absolute (curfdp->infname))
		    taggedfname = savestr (taggedabsname);
		  else
		    taggedfname = relative_filename (taggedabsname,tagfiledir);

		  if (streq (curfdp->taggedfname, taggedfname))
		    /* The #line directive is only a line number change.  We
		       deal with this afterwards. */
		    free (taggedfname);
		  else
		    /* The tags following this #line directive should be
		       attributed to taggedfname.  In order to do this, set
		       curfdp accordingly. */
		    {
		      fdesc *fdp; /* file description pointer */

		      /* Go look for a file description already set up for the
			 file indicated in the #line directive.  If there is
			 one, use it from now until the next #line
			 directive. */
		      for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
			if (streq (fdp->infname, curfdp->infname)
			    && streq (fdp->taggedfname, taggedfname))
			  /* If we remove the second test above (after the &&)
			     then all entries pertaining to the same file are
			     coalesced in the tags file.  If we use it, then
			     entries pertaining to the same file but generated
			     from different files (via #line directives) will
			     go into separate sections in the tags file.  These
			     alternatives look equivalent.  The first one
			     destroys some apparently useless information. */
			  {
			    curfdp = fdp;
			    free (taggedfname);
			    break;
			  }
		      /* Else, if we already tagged the real file, skip all
			 input lines until the next #line directive. */
		      if (fdp == NULL) /* not found */
			for (fdp = fdhead; fdp != NULL; fdp = fdp->next)
			  if (streq (fdp->infabsname, taggedabsname))
			    {
			      discard_until_line_directive = TRUE;
			      free (taggedfname);
			      break;
			    }
		      /* Else create a new file description and use that from
			 now on, until the next #line directive. */
		      if (fdp == NULL) /* not found */
			{
			  fdp = fdhead;
			  fdhead = xnew (1, fdesc);
			  *fdhead = *curfdp; /* copy curr. file description */
			  fdhead->next = fdp;
			  fdhead->infname = savestr (curfdp->infname);
			  fdhead->infabsname = savestr (curfdp->infabsname);
			  fdhead->infabsdir = savestr (curfdp->infabsdir);
			  fdhead->taggedfname = taggedfname;
			  fdhead->usecharno = FALSE;
			  curfdp = fdhead;
			}
		    }
		  free (taggedabsname);
		  lineno = lno;
		  return readline (lbp, stream);
		} /* if a real #line directive */
	    } /* if #line is followed by a a number */
	} /* if line begins with "#line " */

      /* If we are here, no #line directive was found. */
      if (discard_until_line_directive)
	{
	  if (result > 0)
	    /* Do a tail recursion on ourselves, thus discarding the contents
	       of the line buffer. */
	    return readline (lbp, stream);
	  /* End of file. */
	  discard_until_line_directive = FALSE;
	  return 0;
	}
    } /* if #line directives should be considered */

#ifdef ETAGS_REGEXPS
  {
    int match;
    pattern *pp;

    /* Match against relevant patterns. */
    if (lbp->len > 0)
      for (pp = p_head; pp != NULL; pp = pp->p_next)
	{
	  /* Only use generic regexps or those for the current language. */
	  if (pp->lang != NULL && pp->lang != fdhead->lang)
	    continue;

	  match = re_match (pp->pat, lbp->buffer, lbp->len, 0, &pp->regs);
	  switch (match)
	    {
	    case -2:
	      /* Some error. */
	      if (!pp->error_signaled)
		{
		  error ("error while matching \"%s\"", pp->regex);
		  pp->error_signaled = TRUE;
		}
	      break;
	    case -1:
	      /* No match. */
	      break;
	    default:
	      /* Match occurred.  Construct a tag. */
	      if (pp->name_pattern[0] != '\0')
		{
		  /* Make a named tag. */
		  char *name = substitute (lbp->buffer,
					   pp->name_pattern, &pp->regs);
		  if (name != NULL)
		    pfnote (name, TRUE, lbp->buffer, match, lineno, linecharno);
		}
	      else
		{
		  /* Make an unnamed tag. */
		  pfnote ((char *)NULL, TRUE,
			  lbp->buffer, match, lineno, linecharno);
		}
	      break;
	    }
	}
  }
#endif /* ETAGS_REGEXPS */

  return result;
}


/*
 * Return a pointer to a space of size strlen(cp)+1 allocated
 * with xnew where the string CP has been copied.
 */
static char *
savestr (cp)
     char *cp;
{
  return savenstr (cp, strlen (cp));
}

/*
 * Return a pointer to a space of size LEN+1 allocated with xnew where
 * the string CP has been copied for at most the first LEN characters.
 */
static char *
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
 * Identical to POSIX strrchr, included for portability.
 */
static char *
etags_strrchr (sp, c)
     register const char *sp;
     register int c;
{
  register const char *r;

  r = NULL;
  do
    {
      if (*sp == c)
	r = sp;
  } while (*sp++);
  return (char *)r;
}


/*
 * Return the ptr in sp at which the character c first
 * appears; NULL if not found
 *
 * Identical to POSIX strchr, included for portability.
 */
static char *
etags_strchr (sp, c)
     register const char *sp;
     register int c;
{
  do
    {
      if (*sp == c)
	return (char *)sp;
    } while (*sp++);
  return NULL;
}

/* Skip spaces, return new pointer. */
static char *
skip_spaces (cp)
     char *cp;
{
  while (iswhite (*cp))
    cp++;
  return cp;
}

/* Skip non spaces, return new pointer. */
static char *
skip_non_spaces (cp)
     char *cp;
{
  while (*cp != '\0' && !iswhite (*cp))
    cp++;
  return cp;
}

/* Print error message and exit.  */
void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (BAD);
}

static void
pfatal (s1)
     char *s1;
{
  perror (s1);
  exit (BAD);
}

static void
suggest_asking_for_help ()
{
  fprintf (stderr, "\tTry `%s %s' for a complete list of options.\n",
	   progname,
#ifdef LONG_OPTIONS
	   "--help"
#else
	   "-h"
#endif
	   );
  exit (BAD);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */
static void
error (s1, s2)
     const char *s1, *s2;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Return a newly-allocated string whose contents
   concatenate those of s1, s2, s3.  */
static char *
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
   guess the buffer size in advance. */
static char *
etags_getcwd ()
{
#ifdef HAVE_GETCWD
  int bufsize = 200;
  char *path = xnew (bufsize, char);

  while (getcwd (path, bufsize) == NULL)
    {
      if (errno != ERANGE)
	pfatal ("getcwd");
      bufsize *= 2;
      free (path);
      path = xnew (bufsize, char);
    }

  canonicalize_filename (path);
  return path;

#else /* not HAVE_GETCWD */
#if MSDOS

  char *p, path[MAXPATHLEN + 1]; /* Fixed size is safe on MSDOS.  */

  getwd (path);

  for (p = path; *p != '\0'; p++)
    if (*p == '\\')
      *p = '/';
    else
      *p = lowcase (*p);

  return strdup (path);
#else /* not MSDOS */
  linebuffer path;
  FILE *pipe;

  initbuffer (&path);
  pipe = (FILE *) popen ("pwd 2>/dev/null", "r");
  if (pipe == NULL || readline_internal (&path, pipe) == 0)
    pfatal ("pwd");
  pclose (pipe);

  return path.buffer;
#endif /* not MSDOS */
#endif /* not HAVE_GETCWD */
}

/* Return a newly allocated string containing the file name of FILE
   relative to the absolute directory DIR (which should end with a slash). */
static char *
relative_filename (file, dir)
     char *file, *dir;
{
  char *fp, *dp, *afn, *res;
  int i;

  /* Find the common root of file and dir (with a trailing slash). */
  afn = absolute_filename (file, cwd);
  fp = afn;
  dp = dir;
  while (*fp++ == *dp++)
    continue;
  fp--, dp--;			/* back to the first differing char */
#ifdef DOS_NT
  if (fp == afn && afn[0] != '/') /* cannot build a relative name */
    return afn;
#endif
  do				/* look at the equal chars until '/' */
    fp--, dp--;
  while (*fp != '/');

  /* Build a sequence of "../" strings for the resulting relative file name. */
  i = 0;
  while ((dp = etags_strchr (dp + 1, '/')) != NULL)
    i += 1;
  res = xnew (3*i + strlen (fp + 1) + 1, char);
  res[0] = '\0';
  while (i-- > 0)
    strcat (res, "../");

  /* Add the file name relative to the common root of file and dir. */
  strcat (res, fp + 1);
  free (afn);

  return res;
}

/* Return a newly allocated string containing the absolute file name
   of FILE given DIR (which should end with a slash). */
static char *
absolute_filename (file, dir)
     char *file, *dir;
{
  char *slashp, *cp, *res;

  if (filename_is_absolute (file))
    res = savestr (file);
#ifdef DOS_NT
  /* We don't support non-absolute file names with a drive
     letter, like `d:NAME' (it's too much hassle).  */
  else if (file[1] == ':')
    fatal ("%s: relative file names with drive letters not supported", file);
#endif
  else
    res = concat (dir, file, "");

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
	      while (cp >= res && !filename_is_absolute (cp));
	      if (cp < res)
		cp = slashp;	/* the absolute name begins with "/.." */
#ifdef DOS_NT
	      /* Under MSDOS and NT we get `d:/NAME' as absolute
		 file name, so the luser could say `d:/../NAME'.
		 We silently treat this as `d:/NAME'.  */
	      else if (cp[0] != '/')
		cp = slashp;
#endif
	      strcpy (cp, slashp + 3);
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

  if (res[0] == '\0')
    return savestr ("/");
  else
    return res;
}

/* Return a newly allocated string containing the absolute
   file name of dir where FILE resides given DIR (which should
   end with a slash). */
static char *
absolute_dirname (file, dir)
     char *file, *dir;
{
  char *slashp, *res;
  char save;

  canonicalize_filename (file);
  slashp = etags_strrchr (file, '/');
  if (slashp == NULL)
    return savestr (dir);
  save = slashp[1];
  slashp[1] = '\0';
  res = absolute_filename (file, dir);
  slashp[1] = save;

  return res;
}

/* Whether the argument string is an absolute file name.  The argument
   string must have been canonicalized with canonicalize_filename. */
static bool
filename_is_absolute (fn)
     char *fn;
{
  return (fn[0] == '/'
#ifdef DOS_NT
	  || (ISALPHA(fn[0]) && fn[1] == ':' && fn[2] == '/')
#endif
	  );
}

/* Translate backslashes into slashes.  Works in place. */
static void
canonicalize_filename (fn)
     register char *fn;
{
#ifdef DOS_NT
  /* Canonicalize drive letter case.  */
  if (fn[0] != '\0' && fn[1] == ':' && ISLOWER (fn[0]))
    fn[0] = upcase (fn[0]);
  /* Convert backslashes to slashes.  */
  for (; *fn != '\0'; fn++)
    if (*fn == '\\')
      *fn = '/';
#else
  /* No action. */
  fn = NULL;			/* shut up the compiler */
#endif
}

/* Set the minimum size of a string contained in a linebuffer. */
static void
linebuffer_setlen (lbp, toksize)
     linebuffer *lbp;
     int toksize;
{
  while (lbp->size <= toksize)
    {
      lbp->size *= 2;
      xrnew (lbp->buffer, lbp->size, char);
    }
  lbp->len = toksize;
}

/* Like malloc but get fatal error if memory is exhausted.  */
static PTR
xmalloc (size)
     unsigned int size;
{
  PTR result = (PTR) malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)NULL);
  return result;
}

static PTR
xrealloc (ptr, size)
     char *ptr;
     unsigned int size;
{
  PTR result = (PTR) realloc (ptr, size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)NULL);
  return result;
}

/*
 * Local Variables:
 * c-indentation-style: gnu
 * indent-tabs-mode: t
 * tab-width: 8
 * fill-column: 79
 * c-font-lock-extra-types: ("FILE" "bool" "language" "linebuffer" "fdesc")
 * End:
 */
