/* Tags file maker to go with GNU Emacs
   Copyright (C) 1984, 1987, 1988, 1989, 1993 Free Software Foundation, Inc. and Ken Arnold

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
 *	FORTRAN added by Jim Kleckner.
 *	Ed Pelegri-Llopart added C typedefs.
 *	Gnu Emacs TAGS format and modifications by RMS?
 *	Sam Kendall added C++.
 *
 *	Francesco Potorti` (pot@cnuce.cnr.it) is the current maintainer. 9.8
 */

#ifdef HAVE_CONFIG_H
#include <../src/config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#if !defined (S_ISREG) && defined (S_IFREG)
# define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#endif

#include "getopt.h"

extern char *malloc (), *realloc ();
extern char *getenv ();
extern char *strcpy (), *strncpy ();
extern int strcmp ();

char *etags_index (), *etags_rindex ();
char *savenstr ();

/* Define the symbol ETAGS to make the program "etags",
 which makes emacs-style tag tables by default.
 Define CTAGS to make the program "ctags" compatible with the usual one.
 Define neither one to get behavior that depends
 on the name with which the program is invoked
 (but we don't normally compile it that way).  */

#if !defined(ETAGS) && !defined(CTAGS)
/* If neither is defined, program can be run as either. */
#define ETAGS
#define CTAGS
#endif

/* On VMS, CTAGS is not useful, so always do ETAGS.  */
#ifdef VMS
#ifndef ETAGS
#define ETAGS
#endif
#endif

/* Exit codes for success and failure.  */
#ifdef VMS
#define	GOOD	(1)
#define BAD	(0)
#else
#define	GOOD	(0)
#define	BAD	(1)
#endif

/*
 * The FILEPOS abstract type, which represents a position in a file,
 * plus the following accessor functions:
 *
 *	long GET_CHARNO (pos)
 *				returns absolute char number.
 *	void SET_FILEPOS (pos, fp, charno)
 *	    FILE *fp; long charno;
 *				sets `pos' from the current file
 *				position of `fp' and from `charno',
 *				which must be the absolute character
 *				number corresponding to the current
 *				position of `fp'.
 *
 * The `pos' parameter is an lvalue expression of type FILEPOS.
 * Parameters to the accessor functions are evaluated 0 or more times,
 * and so must have no side effects.
 *
 * FILEPOS objects can also be assigned and passed to and from
 * functions in the normal C manner.
 *
 * Implementation notes: the `+ 0' is to enforce rvalue-ness.
 */

#ifndef DEBUG
 /* real implementation */
typedef long FILEPOS;
#define GET_CHARNO(pos)	((pos) + 0)
#define SET_FILEPOS(pos, fp, cno)	((void) ((pos) = (cno)))
#else
 /* debugging implementation */
typedef struct
{
  long charno;
} FILEPOS;

#define GET_CHARNO(pos)	((pos).charno + 0)
#define SET_FILEPOS(pos, fp, cno)					\
    ((void) ((pos).charno = (cno),					\
	     (cno) != ftell (fp) ? (error ("SET_FILEPOS inconsistency"), 0) \
	     			 : 0))
#endif

#define streq(s, t)	(strcmp (s, t) == 0)
#define strneq(s, t, n)	(strncmp (s, t, n) == 0)
#define	logical		int

#define	TRUE	1
#define	FALSE	0

#define	iswhite(arg)	(_wht[arg])	/* T if char is white		*/
#define	begtoken(arg)	(_btk[arg])	/* T if char can start token	*/
#define	intoken(arg)	(_itk[arg])	/* T if char can be in token	*/
#define	endtoken(arg)	(_etk[arg])	/* T if char ends tokens	*/

#define	max(I1,I2)	((I1) > (I2) ? (I1) : (I2))

struct nd_st
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
};

long ftell ();
typedef struct nd_st NODE;

logical header_file;		/* TRUE if .h file, FALSE o.w.  */
/* boolean "functions" (see init)	*/
logical _wht[0177], _etk[0177], _itk[0177], _btk[0177];


char *concat ();
char *savenstr ();
char *savestr ();
char *xmalloc ();
char *xrealloc ();
int L_isdef (), L_isquote ();
int PF_funcs ();
int total_size_of_entries ();
logical consider_token ();
logical tail ();
long readline ();
void Asm_funcs ();
void C_entries ();
void L_funcs ();
void L_getit ();
void PAS_funcs ();
void Scheme_funcs ();
void TEX_funcs ();
void add_node ();
void error ();
void fatal ();
void find_entries ();
void free_tree ();
void getit ();
void getline ();
void init ();
void initbuffer ();
void initbuffer ();
void pfnote ();
void process_file ();
void put_entries ();
void takeprec ();

/*
 * MACRO
 *	xnew -- allocate storage
 *
 * SYNOPSIS
 *	Type *xnew (int n, Type);
 */
#define xnew(n, Type)	((Type *) xmalloc ((n) * sizeof (Type)))



/*
 *	Symbol table stuff.
 *
 * Should probably be implemented with hash table; linked list for now.
 */

enum sym_type
{
  st_none, st_C_struct, st_C_enum, st_C_define, st_C_typedef, st_C_typespec
};

struct stab_entry
{
  char *sym;
  int symlen;
  enum sym_type type;
  struct stab_entry *next;
};

typedef struct stab_entry Stab_entry;
typedef Stab_entry *Stab;

/*
 * NAME
 *	Stab, Stab_entry, stab_create, stab_search, stab_find -- symbol table
 *
 * SYNOPSIS
 *	Types: Stab, Stab_entry, enum sym_type
 *
 *	Stab * stab_create ()
 *
 *	Stab_entry * stab_find (stab, sym)
 *	Stab *stab;
 *	char *sym;
 *
 *	Stab_entry * stab_search (stab, sym)
 *	Stab *stab;
 *	char *sym;
 *
 * DESCRIPTION
 *	stab_create creates a Stab, a symbol table object, and returns a
 *	pointer to it.  stab_find finds a symbol in a Stab; it returns a
 *	pointer to the Stab_entry if found, otherwise NULL.  stab_search
 *	is like stab_find, except that it creates a new Stab_entry,
 *	initialized with type = st_none, if one did not exist already
 *	(it never returns NULL).
 *
 *	A Stab_entry is a structure that contains at least the following
 *	members:
 *
 *		char *name;		// must not be modified
 *		enum sym_type type;	// should be set
 *
 *	The type field is initially set to st_none; it should be set to
 *	something else by the caller of stab_search.  Other possible values
 *	of an enum sym_type can be added.
 */

Stab *
stab_create ()
{
  Stab *sp;
  sp = xnew (1, Stab);
  *sp = NULL;			/* a Stab starts out as a null Stab_entry* */
  return sp;
}

Stab_entry *
stab_find (stab, sym, symlen)
     Stab *stab;
     register char *sym;
     register int symlen;
{
  register Stab_entry *se;
  for (se = *stab; se != NULL; se = se->next)
    {
      if (se->symlen == symlen && strneq (se->sym, sym, symlen))
	return se;
    }

  return NULL;
}

Stab_entry *
stab_search (stab, sym, symlen)
     register Stab *stab;
     char *sym;
     int symlen;
{
  register Stab_entry *se;
  se = stab_find (stab, sym, symlen);

  if (se == NULL)
    {
      /* make a new one */
      se = xnew (1, Stab_entry);
      se->sym = savenstr (sym, symlen);
      se->symlen = symlen;
      se->type = st_none;
      se->next = *stab;
      *stab = se;
    }

  return se;
}

/*
 * NAME
 *	stab_type -- type of a symbol table entry
 *
 * SYNOPSIS
 *	enum sym_type stab_type (Stab_entry *se);
 *
 * WARNING
 *	May evaluate its argument more than once.
 */

#define stab_type(se)	((se)==NULL ? st_none : (se)->type)



typedef int LINENO;

typedef struct
{
  char *p;
  int len;
  LINENO lineno;
  logical named;
} TOKEN;

/* C extensions.
 */
#define C_PLPL	0x00001		/* C++ */
#define C_STAR	0x00003		/* C* */
#define YACC	0x10000		/* yacc file */

char searchar = '/';		/* use /.../ searches 		*/

LINENO lineno;			/* line number of current line */
long charno;			/* current character number */

long linecharno;		/* charno of start of line; not used by C, but
				 * by every other language.
				 */

char *curfile,			/* current input file name		*/
 *outfile,			/* output file				*/
 *white = " \f\t\n",		/* white chars				*/
 *endtk = " \t\n\"'#()[]{}=-+%*/&|^~!<>;,.:?",	/* token ending chars	*/
				/* token starting chars			*/
 *begtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$~",
				  /* valid in-token chars		*/
 *intk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$0123456789";

int append_to_tagfile;		/* -a: append to tags */
int emacs_tags_format;		/* emacs style output (no -e option any more) */
/* The following three default to 1 for etags, but to 0 for ctags.  */
int typedefs;			/* -t: create tags for typedefs */
int typedefs_and_cplusplus;	/* -T: create tags for typedefs, level */
				/* 0 struct/enum/union decls, and C++ */
				/* member functions */
int constantypedefs;		/* -d: create tags for C #define and enum */
				/* constants. Default under etags.  Enum */
				/* constants not implemented. */
				/* -D: opposite of -d.  Default under ctags. */
int update;			/* -u: update tags */
int vgrind_style;		/* -v: create vgrind style index output */
int no_warnings;		/* -w: suppress warnings */
int cxref_style;		/* -x: create cxref style output */
int cplusplus;			/* .[hc] means C++, not C */
int noindentypedefs;		/* -S: ignore indentation in C */

/* Name this program was invoked with.  */
char *progname;

struct option longopts[] = {
  { "append",			no_argument,	   NULL, 'a' },
  { "backward-search",		no_argument,	   NULL, 'B' }, 
  { "c++",			no_argument,	   NULL, 'C' },
  { "cxref",			no_argument,	   NULL, 'x' },
  { "defines",			no_argument,	   NULL, 'd' },
  { "forward-search",		no_argument,	   NULL, 'F' }, 
  { "help",			no_argument,	   NULL, 'H' },
  { "ignore-indentation",	no_argument,	   NULL, 'S' },
  { "include",			required_argument, NULL, 'i' },
  { "no-defines",		no_argument,	   NULL, 'D' },
  { "no-warn",			no_argument,	   NULL, 'w' },
  { "output",			required_argument, NULL, 'o' },
  { "typedefs",			no_argument,	   NULL, 't' },
  { "typedefs-and-c++",		no_argument,	   NULL, 'T' },
  { "update",			no_argument,	   NULL, 'u' }, 
  { "version",			no_argument,	   NULL, 'V' },
  { "vgrind",			no_argument,	   NULL, 'v' }, 
  { 0 }
};

FILE *inf,			/* ioptr for current input file		*/
 *outf;				/* ioptr for tags file			*/

NODE *head;			/* the head of the binary tree of tags	*/

int permit_duplicates = 1;	/* Nonzero means allow duplicate tags.  */

/* A `struct linebuffer' is a structure which holds a line of text.
 `readline' reads a line from a stream into a linebuffer
 and works regardless of the length of the line.  */

struct linebuffer
{
  long size;
  char *buffer;
};

struct linebuffer lb;		/* the current line */
struct linebuffer filename_lb;	/* used to read in filenames */
struct
{
  FILEPOS linepos;
  struct linebuffer lb;		/* used by C_entries instead of lb */
} lbs[2];

void
print_version ()
{
#ifdef CTAGS
  printf ("CTAGS ");
#ifdef ETAGS
  printf ("and ");
#endif
#endif
#ifdef ETAGS
  printf ("ETAGS ");
#endif
  printf ("for Emacs version 19.\n");

  exit (0);
}

void
print_help ()
{
  printf ("These are the options accepted by %s.  You may use unambiguous\n\
abbreviations for the long option names.\n\n", progname);

  puts ("-a, --append\n\
        Append tag entries to existing tags file.");
  puts ("-C, --c++\n\
        Treat files with `.c' and `.h' extensions as C++ code, not C\n\
        code.  Files with `.C', `.H', `.cxx', `.hxx', or `.cc'\n\
        extensions are always assumed to be C++ code.");
  fputs ("-d, --defines\n\
        Create tag entries for #defines, too.", stdout);

#ifdef ETAGS
  fputs ("  This is the default\n\
        behavior.", stdout);
#endif

  fputs ("\n\
-D, --no-defines\n\
        Don't create tag entries for #defines.", stdout);

#ifdef CTAGS
  fputs ("  This is the default\n\
        behavior.", stdout);
#endif

  puts ("\n\
-o FILE, --output=FILE\n\
        Write the tags to FILE.\n\
-S, --ignore-indentation\n\
        Don't rely on indentation quite as much as normal.  Currently,\n\
        this means not to assume that a closing brace in the first\n\
        column is the final brace of a function or structure\n\
        definition.");
  puts ("-t, --typedefs\n\
        Generate tag entries for typedefs.  This is the default\n\
        behavior.");
  puts ("-T, --typedefs-and-c++\n\
        Generate tag entries for typedefs, struct/enum/union tags, and\n\
        C++ member functions.");

#ifdef ETAGS
  puts ("-i FILE, --include=FILE\n\
        Include a note in tag file indicating that, when searching for\n\
        a tag, one should also consult the tags file FILE after\n\
        checking the current file.");
#endif

#ifdef CTAGS
  puts ("-B, --backward-search\n\
        Write the search commands for the tag entries using '?', the\n\
        backward-search command.");
  puts ("-F, --forward-search\n\
        Write the search commands for the tag entries using '/', the\n\
        forward-search command.");
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
  puts ("-x, --cxref\n\
        Like --vgrind, but in the style of cxref, rather than vgrind.\n\
        The output uses line numbers instead of page numbers, but\n\
        beyond that the differences are cosmetic; try both to see\n\
        which you like.");
  puts ("-w, --no-warn\n\
        Suppress warning messages about entries defined in multiple\n\
        files.");
#endif

  puts ("-V, --version\n\
        Print the version of the program.\n\
-H, --help\n\
        Print this help message.");

  exit (0);
}


void
main (argc, argv)
     int argc;
     char *argv[];
{
  char cmd[100];
  int i;
  unsigned int nincluded_files = 0;
  char **included_files = xnew (argc, char *);
  char *this_file;
#ifdef VMS
  char got_err;

  extern char *gfnames ();
  extern char *massage_name ();
#endif

  progname = argv[0];

#ifndef CTAGS
  emacs_tags_format = 1;
#else
  emacs_tags_format = 0;
#endif

  /*
   * If etags, always find typedefs and structure tags.  Why not?
   * Also default is to find macro constants.
   */
  if (emacs_tags_format)
    typedefs = typedefs_and_cplusplus = constantypedefs = 1;

  for (;;)
    {
      int opt;
      opt = getopt_long (argc, argv, "aCdDo:f:StTi:BFuvxwVH", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case '\0':
	  /* If getopt returns '\0', then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	  /* Common options. */
	case 'a':
	  append_to_tagfile++;
	  break;
	case 'C':
	  cplusplus = 1;
	  break;
	case 'd':
	  constantypedefs = 1;
	  break;
	case 'D':
	  constantypedefs = 0;
	  break;
	case 'f':
	case 'o':
	  if (outfile)
	    {
	      fprintf (stderr,
		       "%s: -%c flag may only be given once\n", progname, opt);
	      goto usage;
	    }
	  outfile = optarg;
	  break;
	case 'S':
	  noindentypedefs++;
	  break;
	case 't':
	  typedefs++;
	  break;
	case 'T':
	  typedefs++;
	  typedefs_and_cplusplus++;
	  break;
	case 'V':
	  print_version ();
	  break;
	case 'H':
	  print_help ();
	  break;

	  /* Etags options */
	case 'i':
	  if (!emacs_tags_format)
	    goto usage;
	  included_files[nincluded_files++] = optarg;
	  break;

	  /* Ctags options. */
	case 'B':
	  searchar = '?';
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'F':
	  searchar = '/';
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'u':
	  update++;
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'v':
	  vgrind_style++;
	  /*FALLTHRU*/
	case 'x':
	  cxref_style++;
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'w':
	  no_warnings++;
	  if (emacs_tags_format)
	    goto usage;
	  break;

	default:
	  goto usage;
	}
    }

  if (optind == argc && nincluded_files == 0)
    {
      fprintf (stderr, "%s: No input files specified.\n", progname);

    usage:
      fprintf (stderr, "%s: Try `%s --help' for a complete list of options.\n",
	       progname, progname);
      exit (BAD);
    }

  if (outfile == 0)
    {
      outfile = emacs_tags_format ? "TAGS" : "tags";
    }

  init ();			/* set up boolean "functions"		*/

  initbuffer (&lb);
  initbuffer (&lbs[0].lb);
  initbuffer (&lbs[1].lb);
  initbuffer (&filename_lb);
  /*
   * loop through files finding functions
   */
  if (emacs_tags_format)
    {
      if (streq (outfile, "-"))
	outf = stdout;
      else
	outf = fopen (outfile, append_to_tagfile ? "a" : "w");
      if (!outf)
	{
	  perror (outfile);
	  exit (1);
	}
    }

#ifdef VMS
  argc -= optind;
  argv += optind;
  while (gfnames (&argc, &argv, &got_err) != NULL)
    {
      if (got_err)
	{
	  error ("Can't find file %s\n", this_file);
	  argc--, argv++;
	}
      else
	{
	  this_file = massage_name (this_file);
#if 0
	}
    }			/* solely to balance out the ifdef'd parens above */
#endif
#else
  for (; optind < argc; optind++)
    {
      this_file = argv[optind];
      if (1)
	{
#endif
	  /* Input file named "-" means read file names from stdin
	     and use them.  */
	  if (streq (this_file, "-"))
	    {
	      while (!feof (stdin))
		{
		  (void) readline (&filename_lb, stdin);
		  if (strlen (filename_lb.buffer) > 0)
		    process_file (filename_lb.buffer);
		}
	    }
	  else
	    process_file (this_file);
	}
    }

  if (emacs_tags_format)
    {
      while (nincluded_files-- > 0)
	fprintf (outf, "\f\n%s,include\n", *included_files++);

      (void) fclose (outf);
      exit (0);
    }

  if (cxref_style)
    {
      put_entries (head);
      exit (GOOD);
    }
  if (update)
    {
      /* update cannot be set under VMS, so we may assume that argc
	 and argv have not been munged.  */
      for (i = optind; i < argc; i++)
	{
	  sprintf (cmd,
		   "mv %s OTAGS;fgrep -v '\t%s\t' OTAGS >%s;rm OTAGS",
		   outfile, argv[i], outfile);
	  (void) system (cmd);
	}
      append_to_tagfile++;
    }
  outf = fopen (outfile, append_to_tagfile ? "a" : "w");
  if (outf == NULL)
    {
      perror (outfile);
      exit (GOOD);
    }
  put_entries (head);
  (void) fclose (outf);
  if (update)
    {
      sprintf (cmd, "sort %s -o %s", outfile, outfile);
      (void) system (cmd);
    }
  exit (GOOD);
}


/*
 * This routine is called on each file argument.
 */
void
process_file (file)
     char *file;
{
  struct stat stat_buf;

  if (stat (file, &stat_buf) || !S_ISREG (stat_buf.st_mode))
    {
      fprintf (stderr, "Skipping %s: it is not a regular file.\n", file);
      return;
    }

  if (streq (file, outfile) && !streq (outfile, "-"))
    {
      fprintf (stderr, "Skipping inclusion of %s in self.\n", file);
      return;
    }
  find_entries (file);
  if (emacs_tags_format)
    {
      fprintf (outf, "\f\n%s,%d\n", file, total_size_of_entries (head));
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
find_entries (file)
     char *file;
{
  char *cp;
  void prolog_funcs ();

  inf = fopen (file, "r");
  if (inf == NULL)
    {
      perror (file);
      return;
    }
  curfile = savestr (file);
  cp = etags_rindex (file, '.');

  header_file = (cp && (streq (cp + 1, "h")));

  /* .tex, .aux or .bbl implies LaTeX source code */
  if (cp && (streq (cp + 1, "tex") || streq (cp + 1, "aux")
	     || streq (cp + 1, "bbl")))
    {
      TEX_funcs (inf);
      goto close_and_return;
    }
  /* .l or .el or .lisp (or .cl or .clisp or ...) implies lisp source code */
  if (cp && (streq (cp + 1, "l")
	     || streq (cp + 1, "el")
	     || streq (cp + 1, "lsp")
	     || streq (cp + 1, "lisp")
	     || streq (cp + 1, "cl")
	     || streq (cp + 1, "clisp")))
    {
      L_funcs (inf);
      goto close_and_return;
    }
  /* .scm or .sm or .scheme or ... implies scheme source code */
  if (cp && (streq (cp + 1, "sm")
	     || streq (cp + 1, "scm")
	     || streq (cp + 1, "scheme")
	     || streq (cp + 1, "t")
	     || streq (cp + 1, "sch")
	     || streq (cp + 1, "SM")
	     || streq (cp + 1, "SCM")
	     /* The `SCM' or `scm' prefix with a version number */
             || (cp[-1] == 'm' && cp[-2] == 'c' && cp[-3] == 's'
		 && string_numeric_p (cp + 1))
             || (cp[-1] == 'M' && cp[-2] == 'C' && cp[-3] == 'S'
		 && string_numeric_p (cp + 1))))
    {
      Scheme_funcs (inf);
      fclose (inf);
      return;
    }
  /* Assume that ".s" or ".a" is assembly code. -wolfgang.
     Or even ".sa". */
  if (cp && (streq (cp + 1, "s")
	     || streq (cp + 1, "a")
	     || streq (cp + 1, "sa")))
    {
      Asm_funcs (inf);
      fclose (inf);
      return;
    }
  /* .C or .H or .cxx or .hxx or .cc: a C++ file */
  if (cp && (streq (cp + 1, "C")
	     || streq (cp + 1, "H")
	     || streq (cp + 1, "cxx")
	     || streq (cp + 1, "hxx")
	     || streq (cp + 1, "cc")))
    {
      C_entries (C_PLPL);	/* C++ */
      goto close_and_return;
    }
  /* .cs or .hs: a C* file */
  if (cp && (streq (cp + 1, "cs")
	     || streq (cp + 1, "hs")))
    {
      C_entries (C_STAR);
      goto close_and_return;
    }
  /* .y: a yacc file */
  if (cp && (streq (cp + 1, "y")))
    {
      C_entries (YACC);
      goto close_and_return;
    }
  /* .pl implies prolog source code */
  if (cp && streq (cp + 1, "pl"))
    {
      prolog_funcs (inf);
      goto close_and_return;
    }
  /* .p or .pas: a Pascal file */
  if (cp && (streq (cp + 1, "p")
	     || streq (cp + 1, "pas")))
    {
      PAS_funcs (inf);
      goto close_and_return;
    }
  /* If .f or .for, assume it is fortran or nothing.  */
  if (cp && (streq (cp + 1, "f")
	     || streq (cp + 1, "for")))
    {
      PF_funcs (inf);
      goto close_and_return;
    }
  /* if not a .c or .h or .y file, try fortran */
  if (cp && ((cp[1] != 'c'
	      && cp[1] != 'h'
	      && cp[1] != 'y')
	     || (cp[1] != 0 && cp[2] != 0)))
    {
      if (PF_funcs (inf) != 0)
	goto close_and_return;
      rewind (inf);		/* no fortran tags found, try C */
    }
  C_entries (cplusplus ? C_PLPL : 0);

close_and_return:
  (void) fclose (inf);
}

/* Nonzero if string STR is composed of digits.  */

int
string_numeric_p (str)
     char *str;
{
  while (*str)
    {
      if (*str < '0' || *str > '9')
	return 0;
    }
  return 1;
}

/* Record a tag. */
/* Should take a TOKEN* instead!! */
void
pfnote (name, is_func, named, linestart, linelen, lno, cno)
     char *name;		/* tag name */
     logical is_func;		/* function or type name? */
     logical named;		/* tag different from text of definition? */
     char *linestart;
     int linelen;
     int lno;
     long cno;
{
  register char *fp;
  register NODE *np;
  char tem[51];
  char c;

  np = (NODE *) malloc (sizeof (NODE));
  if (np == NULL)
    {
      if (!emacs_tags_format)
	{
	  /* It's okay to output early in etags -- it only disrupts the
	   * character count of the tag entries, which is no longer used
	   * by tags.el anyway.
	   */
	  error ("too many entries to sort");
	}
      put_entries (head);
      free_tree (head);
      head = NULL;
      np = xnew (1, NODE);
    }
  /* If ctags mode, change name "main" to M<thisfilename>. */
  if (!emacs_tags_format && !cxref_style && streq (name, "main"))
    {
      fp = etags_rindex (curfile, '/');
      name = concat ("M", fp == 0 ? curfile : fp + 1, "");
      fp = etags_rindex (name, '.');
      if (fp && fp[1] != '\0' && fp[2] == '\0')
	*fp = 0;
      named = TRUE;
    }
  np->name = savestr (name);
  np->file = curfile;
  np->is_func = is_func;
  np->named = named;
  np->lno = lno;
  /* UNCOMMENT THE +1 HERE: */
  np->cno = cno /* + 1 */ ;	/* our char numbers are 0-base; emacs's are 1-base */
  np->left = np->right = 0;
  if (emacs_tags_format)
    {
      c = linestart[linelen];
      linestart[linelen] = 0;
    }
  else if (cxref_style == 0)
    {
      sprintf (tem, strlen (linestart) < 50 ? "%s$" : "%.50s", linestart);
      linestart = tem;
    }
  np->pat = savestr (linestart);
  if (emacs_tags_format)
    {
      linestart[linelen] = c;
    }

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
/* Must avoid static vars within functions since some systems
   #define static as nothing.  */
static NODE *last_node = NULL;

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

  if (emacs_tags_format)
    {
      /* Etags Mode */
      if (!last_node)
	fatal ("internal error in add_node");
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
	  if (!strcmp (node->name, cur_node->name)
	      && !strcmp (node->file, cur_node->file))
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

  if (emacs_tags_format)
    {
      if (node->named)
	{
	  fprintf (outf, "%s\177%s\001%d,%d\n",
		   node->pat, node->name,
		   node->lno, node->cno);
	}
      else
	{
	  fprintf (outf, "%s\177%d,%d\n",
		   node->pat,
		   node->lno, node->cno);
	}
    }
  else if (!cxref_style)
    {
      fprintf (outf, "%s\t%s\t",
	       node->name, node->file);

      if (node->is_func)
	{			/* a function */
	  putc (searchar, outf);
	  putc ('^', outf);

	  for (sp = node->pat; *sp; sp++)
	    {
	      if (*sp == '\\' || *sp == searchar)
		putc ('\\', outf);
	      putc (*sp, outf);
	    }
	  putc (searchar, outf);
	}
      else
	{			/* a typedef; text pattern inadequate */
	  fprintf (outf, "%d", node->lno);
	}
      putc ('\n', outf);
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
 * the nodes in the subtree of the specified node.  Works only if emacs_tags_format
 * is set, but called only in that case.  This count is irrelevant with
 * the new tags.el, but is still supplied for backward compatibility.
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

Stab *C_stab, *C_PLPL_stab, *C_STAR_stab;

/*
 * SYNOPSIS
 *	Stab *get_C_stab (int c_ext);
 */
#define get_C_stab(c_ext) ((c_ext & C_STAR) ? C_STAR_stab :		\
			   (c_ext & C_PLPL) ? C_PLPL_stab :		\
			   C_stab)

void
add_keyword (stab, sym, type)
     Stab *stab;
     char *sym;
     enum sym_type type;
{
  stab_search (stab, sym, strlen (sym))->type = type;
}

Stab *
C_create_stab (c_ext)
     int c_ext;
{
  Stab *stab;

  stab = stab_create ();

  /* C, C++ and C* */
  if (c_ext & C_PLPL)
    add_keyword (stab, "class", st_C_struct);
  if (c_ext & C_STAR)
    add_keyword (stab, "domain", st_C_struct);
  add_keyword (stab, "union", st_C_struct);
  add_keyword (stab, "struct", st_C_struct);
  add_keyword (stab, "enum", st_C_enum);
  add_keyword (stab, "typedef", st_C_typedef);
  add_keyword (stab, "define", st_C_define);
  add_keyword (stab, "long", st_C_typespec);
  add_keyword (stab, "short", st_C_typespec);
  add_keyword (stab, "int", st_C_typespec);
  add_keyword (stab, "char", st_C_typespec);
  add_keyword (stab, "float", st_C_typespec);
  add_keyword (stab, "double", st_C_typespec);
  add_keyword (stab, "signed", st_C_typespec);
  add_keyword (stab, "unsigned", st_C_typespec);
  add_keyword (stab, "auto", st_C_typespec);
  add_keyword (stab, "void", st_C_typespec);
  add_keyword (stab, "extern", st_C_typespec);
  add_keyword (stab, "static", st_C_typespec);
  add_keyword (stab, "const", st_C_typespec);
  add_keyword (stab, "volatile", st_C_typespec);

  return stab;
}

void
C_create_stabs ()
{
  C_stab = C_create_stab (0);
  C_PLPL_stab = C_create_stab (C_PLPL);
  C_STAR_stab = C_create_stab (C_STAR | C_PLPL);
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


 /* typedefs are recognized using a simple finite automaton.
  * typeddef is its state variable.
  */
typedef enum
{
  tnone,			/* nothing seen */
  ttypedseen,			/* typedef keyword seen */
  tinbody,			/* inside typedef body */
  tend				/* just before typedef tag */
} TYPEDST;
TYPEDST typdef;


 /* struct tags for C++ are recognized using another simple
  * finite automaton.  `structdef' is its state variable.
  * This machinery is only invoked for C++; otherwise structdef
  * should remain snone.  However, this machinery can easily be
  * adapted to find structure tags in normal C code.
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
 * struct tag, and structkey is the preceding struct-like keyword.
 */
char structtag[BUFSIZ];
Stab_entry *structkey;

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
 * Used only for GNUmacs's function-defining macros.
 */
logical next_token_is_func;

/*
 * TRUE in the rules part of a yacc file, FALSE outside (parse as C).
 */
logical yacc_rules;

/*
 * C_entries ()
 *	This routine finds functions, typedefs, #define's and
 * 	struct/union/enum definitions in C syntax and adds them
 *	to the list.
 */

#define curlb (lbs[curndx].lb)
#define othlb (lbs[1-curndx].lb)
#define newlb (lbs[newndx].lb)
#define curlinepos (lbs[curndx].linepos)
#define othlinepos (lbs[1-curndx].linepos)
#define newlinepos (lbs[newndx].linepos)

/* Save and restore token state.  This is used when preprocessor defines
   are handled, to avoid disturbing active function/typedef/struct states.  */
#define TOKEN_SAVED_P	(savetok.lineno > 0)
#define SAVE_TOKEN	(savetok = tok, savetok.p = (char *) tokoff,	\
			 savetok.len = toklen, strcpy(savenameb, nameb))
#define RESTORE_TOKEN	(tok = savetok, tokoff = (int) tok.p,		\
			 toklen = tok.len, strcpy(nameb, savenameb),	\
			 savetok.lineno = 0)

#define CNL_SAVE_DEFINEDEF						\
do {									\
  SET_FILEPOS (curlinepos, inf, charno);				\
  lineno++;								\
  charno += readline (&curlb, inf);					\
  lp = curlb.buffer;							\
  quotednl = FALSE;							\
  newndx = curndx;							\
} while (FALSE)

#define CNL								\
do {									\
  CNL_SAVE_DEFINEDEF;							\
  if (TOKEN_SAVED_P)							\
    RESTORE_TOKEN;							\
  definedef = dnone;							\
} while (FALSE)

#define MAKE_TAG_FROM_NEW_LB(isfun)  pfnote (nameb, isfun, tok.named,	\
  newlb.buffer, tokoff + toklen + 1, tok.lineno, GET_CHARNO (newlinepos))
#define MAKE_TAG_FROM_OTH_LB(isfun)  pfnote (nameb, isfun, tok.named,	\
  othlb.buffer, tokoff + toklen + 1, tok.lineno, GET_CHARNO (othlinepos))

void
C_entries (c_ext)
     int c_ext;			/* extension of C? */
{
  register char c;		/* latest char read; '\0' for end of line */
  register char *lp;		/* pointer one beyond the character `c' */
  int curndx, newndx;		/* indices for current and new lb */
  TOKEN tok;			/* latest token read for funcdef & structdef */
  char nameb[BUFSIZ];		/* latest token name for funcdef & structdef */
  register int tokoff;		/* offset in line of start of latest token */
  register int toklen;		/* length of latest token */
  int cblev;			/* current curly brace level */
  int parlev;			/* current parenthesis level */
  logical incomm, inquote, inchar, quotednl, midtoken;
  logical cplpl;
  TOKEN savetok;		/* saved token during preprocessor handling */
  logical saveisfunc;
  char savenameb[BUFSIZ];	/* ouch! */

  savetok.lineno = 0;
  curndx = newndx = 0;
  lineno = 0;
  charno = 0;
  lp = curlb.buffer;
  *lp = 0;

  definedef = dnone; funcdef = fnone; typdef= tnone; structdef= snone;
  next_token_is_func = yacc_rules = FALSE;
  midtoken = inquote = inchar = incomm = quotednl = FALSE;
  cblev = 0;
  parlev = 0;
  cplpl = c_ext & C_PLPL;

  C_create_stabs ();

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
	      /* Newlines inside strings, do not end macro definitions
		 in traditional cpp, even though compilers don't
		 usually accept them. */
	      CNL_SAVE_DEFINEDEF;
	      break;
	    }
	  continue;
	}
      else if (inchar)
	{
	  if (c == '\'')
	    inchar = FALSE;
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
		typdef= tnone; structdef= snone;
		next_token_is_func = FALSE;
		midtoken = inquote = inchar = incomm = quotednl = FALSE;
		cblev = 0;
		yacc_rules = !yacc_rules;
		continue;
 	      }
	    else
	      break;
	  case '#':
	    if (lp == newlb.buffer + 1 && definedef == dnone)
	      definedef = dsharpseen;
	    continue;
	  } /* switch (c) */


      /* Consider token only if some complicated conditions are satisfied. */
      if (((cblev == 0 && structdef != scolonseen)
	   || (cblev == 1 && cplpl && structdef == sinbody))
	  && definedef != dignorerest
	  && (funcdef != finlist
	      || (definedef != dnone && definedef != dignorerest)))
	{
	  if (midtoken)
	    {
	      if (endtoken (c))
		{
		  if (cplpl && c == ':' && *lp == ':' && begtoken(*(lp + 1)))
		    {
		      /*
		       * This handles :: in the middle, but not at beginning
		       * of an identifier.
		       */
		      lp += 2;
		      toklen += 3;
		    }
		  else
		    {
		      logical is_func = FALSE;

		      tok.lineno = lineno;
		      tok.p = newlb.buffer + tokoff;
		      tok.len = toklen;
		      tok.named = FALSE;
		      if (yacc_rules
			  || consider_token (c, lp, &tok,
					     c_ext, cblev, &is_func))
			{
			  if (structdef == sinbody
			      && definedef == dnone
			      && is_func)
			    /* function defined in C++ class body */
			    {
			      tok.named = TRUE;
			      sprintf (nameb, "%s::%.*s",
				       ((structtag[0] == '\0')
					? "_anonymous_" : structtag),
				       tok.len, tok.p);
			    }
			  else
			    {
			      sprintf (nameb, "%.*s", tok.len, tok.p);
			    }

			  if (structdef == stagseen
			      || typdef == tend)
			    tok.named = TRUE;

			  if (definedef == dnone
			      && (funcdef == ftagseen
				  || structdef == stagseen
				  || typdef == tend))
			    {
			      if (newndx == curndx)
				curndx = 1 - curndx; /* switch line buffers */
			    }
			  else
			    MAKE_TAG_FROM_NEW_LB (is_func);
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
		      MAKE_TAG_FROM_OTH_LB (TRUE);
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
		  /* Take a quick peek ahead for define directive,
		     so we can avoid saving the token when not absolutely
		     necessary. [This is a speed hack.] */
		  if (c == 'd' && strneq(lp, "efine", 5)
		      && iswhite(*(lp + 5)))
		    {
		      SAVE_TOKEN;
		      definedef = ddefineseen;
		      lp += 6;
		    }
		  else
		    definedef = dignorerest;
		  continue;
		}
	      if (!yacc_rules || lp == newlb.buffer + 1)
		{
		  tokoff = lp - 1 - newlb.buffer;
		  toklen = 1;
		  midtoken = TRUE;
		}
	      continue;
	    }
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
		    MAKE_TAG_FROM_OTH_LB (FALSE);
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
	  if (cblev == 0 && typdef == tend)
	    {
	      typdef = tnone;
	      MAKE_TAG_FROM_OTH_LB (FALSE);
	    }
	  if (funcdef != fignore)
	    funcdef = fnone;
	  /* FALLTHRU */
	case ',':
	  /* FALLTHRU */
	case '[':
	  if (definedef != dnone)
	    break;
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
	      structtag[0] = '\0';
	      structdef = sinbody;
	      break;
	    case stagseen:
	    case scolonseen:	/* named struct */
	      structdef = sinbody;
	      MAKE_TAG_FROM_OTH_LB (FALSE);
	      break;
	    }
	  switch (funcdef)
	    {
	    case flistseen:
	      MAKE_TAG_FROM_OTH_LB (TRUE);
	      /* FALLTHRU */
	    case fignore:
	      funcdef = fnone;
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
	      structdef = snone;
	      (void) strcpy (structtag, "<error 2>");
	    }
	  break;
	case '=':
	case '#':
	case '+':
	case '-':
	case '~':
	case '&':
	case '%':
	case '/':
	case '|':
	case '^':
	case '!':
	case '<':
	case '>':
	case '.':
	case '?':
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
consider_token (c, lp, tokp, c_ext, cblev, is_func)
     register char c;		/* IN: first char after the token */
     register char *lp;		/* IN: lp points to 2nd char after the token */
     register TOKEN *tokp;	/* IN: token pointer */
     int c_ext;			/* IN: C extensions mask */
     int cblev;			/* IN: curly brace level */
     logical *is_func;		/* OUT */
{
  Stab_entry *tokse = stab_find (get_C_stab (c_ext), tokp->p, tokp->len);
  enum sym_type toktype = stab_type (tokse);

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
      return (FALSE);
    case ddefineseen:
      /*
       * Make a tag for any macro.
       */
      definedef = dignorerest;
      *is_func = (c == '(');
      if (!*is_func && !constantypedefs)
	return (FALSE);
      else
	return (TRUE);
    case dignorerest:
      return (FALSE);
    default:
      error ("internal error: definedef value");
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
	  return (FALSE);
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
	  return (FALSE);
	}
      return (TRUE);
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
   * return (FALSE).  All the other code here is for the structdef 
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
	  structkey = tokse;
	}
      return (FALSE);
    }
  if (structdef == skeyseen)
    {
      if (stab_type (structkey) == st_C_struct)
	{
	  (void) strncpy (structtag, tokp->p, tokp->len);
	  structtag[tokp->len] = '\0';	/* for struct/union/class */
	}
      else
	{
	  structtag[0] = '\0';	/* for enum (why is it treated differently?) */
	}
      structdef = stagseen;
      return (TRUE);
    }

  /* Avoid entering funcdef stuff if typdef is going on. */
  if (typdef != tnone)
    {
      definedef = dnone;
      return (FALSE);
    }

  /* Detect GNUmacs's function-defining macros. */
  if (definedef == dnone)
    {
      if (strneq (tokp->p, "DEF", 3)
	  || strneq (tokp->p, "ENTRY", 5)
	  || strneq (tokp->p, "SYSCALL", 7)
	  || strneq (tokp->p, "PSEUDO", 6))
	{
	  next_token_is_func = TRUE;
	  return (FALSE);
	}
      if (strneq (tokp->p, "EXFUN", 5))
	{
	  next_token_is_func = FALSE;
	  return (FALSE);
	}
    }
  if (next_token_is_func)
    {
      next_token_is_func = FALSE;
      funcdef = fnone;
      *is_func = TRUE;		/* to force search string in ctags */
      return (TRUE);
    }

  /* A function? */
  switch (toktype)
    {
    case st_C_typespec:
      funcdef = fnone;		/* should be useless */
      return (FALSE);
    default:
      if (funcdef == fnone)
	{
	  funcdef = ftagseen;
	  *is_func = TRUE;
	  return (TRUE);
	}
    }

  return (FALSE);
}

/* Fortran parsing */

char *dbp;
int pfcnt;

int
PF_funcs (fi)
     FILE *fi;
{
  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
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
	    getit ();
	  continue;
	case 's':
	  if (tail ("subroutine"))
	    getit ();
	  continue;
	case 'e':
	  if (tail ("entry"))
	    getit ();
	  continue;
	case 'p':
	  if (tail ("program"))
	    {
	      getit ();
	      continue;
	    }
	  if (tail ("procedure"))
	    getit ();
	  continue;
	}
    }
  return (pfcnt);
}

logical
tail (cp)
     char *cp;
{
  register int len = 0;

  while (*cp && (*cp & ~' ') == ((*(dbp + len)) & ~' '))
    cp++, len++;
  if (*cp == 0)
    {
      dbp += len;
      return (1);
    }
  return (0);
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
getit ()
{
  register char *cp;
  char c;
  char nambuf[BUFSIZ];

  while (isspace (*dbp))
    dbp++;
  if (*dbp == 0
      || (!isalpha (*dbp)
	  && *dbp != '_'
	  && *dbp != '$'))
    return;
  for (cp = dbp + 1; *cp && (isalpha (*cp) || isdigit (*cp)
			     || (*cp == '_') || (*cp == '$')); cp++)
    continue;
  c = cp[0];
  cp[0] = 0;
  (void) strcpy (nambuf, dbp);
  cp[0] = c;
  pfnote (nambuf, TRUE, FALSE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
  pfcnt++;
}

/* Handle a file of assembler code.  */

void
Asm_funcs (fi)
     FILE *fi;
{
  int i;
  register char c;

  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;

      for (i = 0; ((c = dbp[i]) && !isspace (c)) && (c != ':'); i++)
	;

      if ((i > 0) && (c == ':'))
	getit ();
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

/*  Locates tags for procedures & functions.
 *  Doesn't do any type- or var-definitions.
 *  It does look for the keyword "extern" or "forward"
 *  immediately following the procedure statement;
 *  if found, the tag is skipped.
 */

void
PAS_funcs (fi)
     FILE *fi;
{
  struct linebuffer tline;	/* mostly copied from C_entries */
  long save_lcno;
  int save_lineno;
  char c, *cp;
  char nambuf[BUFSIZ];

  logical			/* each of these flags is TRUE iff: */
    incomm1,			/* point is inside {..} comment */
    incomm2,			/* point is inside (*..*) comment */
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

  incomm1 = incomm2 = inquote = FALSE;
  found_tag = FALSE;		/* have a proc name; check if extern */
  get_tagname = FALSE;		/* have found "procedure" keyword    */
  inparms = FALSE;		/* found '(' after "proc"            */
  verify_tag = FALSE;		/* check if "extern" is ahead        */

  /* long main loop to get next char */
  while (!feof (fi))
    {
      c = *dbp++;
      if (c == 0)		/* if end of line */
	{
	  GET_NEW_LINE;
	  if (*dbp == 0)
	    continue;
	  if (!((found_tag && verify_tag) ||
		get_tagname))
	    c = *dbp++;		/* only if don't need *dbp pointing */
	  /* to the beginning of the name of  */
	  /* the procedure or function        */
	}
      if (incomm1)		/* within { - } comments */
	{
	  if (c == '}')
	    incomm1 = FALSE;
	  continue;
	}
      else if (incomm2)		/* within (* - *) comments */
	{
	  if (c == '*')
	    {
	      while ((c = *dbp++) == '*')
		continue;
	      if (c == 0)
		GET_NEW_LINE;
	      if (c == ')')
		incomm2 = FALSE;
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
	    incomm1 = TRUE;
	    continue;
	  case '(':
	    if (*dbp == '*')	/* found open-(*-comment */
	      {
		incomm2 = TRUE;
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
	  if ((found_tag) && (verify_tag))	/* not external proc, so make tag */
	    {
	      found_tag = FALSE;
	      verify_tag = FALSE;
	      pfnote (nambuf, TRUE, FALSE,
		      tline.buffer, cp - tline.buffer + 1,
		      save_lineno, save_lcno);
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
	  c = cp[0];
	  cp[0] = 0;
	  strcpy (nambuf, dbp);
	  cp[0] = c;
	  dbp = cp;		/* restore dbp to e-o-token */
	  get_tagname = FALSE;
	  found_tag = TRUE;
	  continue;

	  /* and proceed to check for "extern" */
	}
      if ((!incomm1) && (!incomm2) && (!inquote) &&
	  (!found_tag) && (!get_tagname))
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
    }				/* while not e-o-f */
}

/*
 * lisp tag functions
 * just look for (def or (DEF
 */

void
L_funcs (fi)
     FILE *fi;
{
  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
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

int
L_isdef (dbp)
     register char *dbp;
{
  return ((dbp[1] == 'd' || dbp[1] == 'D')
	  && (dbp[2] == 'e' || dbp[2] == 'E')
	  && (dbp[3] == 'f' || dbp[3] == 'F'));
}

int
L_isquote (dbp)
     register char *dbp;
{
  return ((*(++dbp) == 'q' || *dbp == 'Q')
	  && (*(++dbp) == 'u' || *dbp == 'U')
	  && (*(++dbp) == 'o' || *dbp == 'O')
	  && (*(++dbp) == 't' || *dbp == 'T')
	  && (*(++dbp) == 'e' || *dbp == 'E')
	  && isspace(*(++dbp)));
}

void
L_getit ()
{
  register char *cp;
  char c;
  char nambuf[BUFSIZ];

  if (*dbp == '\'')		/* Skip prefix quote */
    dbp++;
  else if (*dbp == '(' && L_isquote (dbp)) /* Skip "(quote " */
  {
    dbp += 7;
    while (isspace(*dbp))
      dbp++;
  }
  for (cp = dbp /*+1*/; *cp && *cp != '(' && *cp != ' ' && *cp != ')'; cp++)
    continue;
  if (cp == dbp)
    return;
  
  c = cp[0];
  cp[0] = 0;
  (void) strcpy (nambuf, dbp);
  cp[0] = c;
  pfnote (nambuf, TRUE, FALSE, lb.buffer,
	  cp - lb.buffer + 1, lineno, linecharno);
  pfcnt++;
}

/*
 * Scheme tag functions
 * look for (def... xyzzy
 * look for (def... (xyzzy
 * look for (def ... ((...(xyzzy ....
 * look for (set! xyzzy
 */

static void get_scheme ();

void
Scheme_funcs (fi)
     FILE *fi;
{
  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
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

static void
get_scheme ()
{
  register char *cp;
  char c;
  char nambuf[BUFSIZ];

  if (*dbp == 0)
    return;
  /* Go till you get to white space or a syntactic break */
  for (cp = dbp + 1; *cp && *cp != '(' && *cp != ')' && !isspace (*cp); cp++)
    continue;
  /* Null terminate the string there. */
  c = cp[0];
  cp[0] = 0;
  /* Copy the string */
  strcpy (nambuf, dbp);
  /* Unterminate the string */
  cp[0] = c;
  /* Announce the change */
  pfnote (nambuf, TRUE, FALSE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
  pfcnt++;
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

static char *TEX_defenv =
":chapter:section:subsection:subsubsection:eqno:label:ref:cite:bibitem:typeout";

void TEX_mode ();
struct TEX_tabent *TEX_decode_env ();
void TEX_getit ();
int TEX_Token ();

static char TEX_esc = '\\';
static char TEX_opgrp = '{';
static char TEX_clgrp = '}';

/*
 * TeX/LaTeX scanning loop.
 */

void
TEX_funcs (fi)
     FILE *fi;
{
  char *lasthit;

  lineno = 0;
  charno = 0;
  pfcnt = 0;

  /* Select either \ or ! as escape character.  */
  TEX_mode (fi);

  /* Initialize token table once from environment. */
  if (!TEX_toktab)
    TEX_toktab = TEX_decode_env ("TEXTAGS", TEX_defenv);

  while (!feof (fi))
    {				/* Scan each line in file */
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;
      lasthit = dbp;
      while (dbp = etags_index (dbp, TEX_esc)) /* Look at each escape in line */
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

/* Figure out whether TeX's escapechar is '\\' or '!' and set grouping */
/* chars accordingly. */

void
TEX_mode (f)
     FILE *f;
{
  int c;

  while ((c = getc (f)) != EOF)
    {
      /* Skip to next line if we hit the TeX comment char. */
      if (c == TEX_cmt)
	while (c != '\n')
	  c = getc (f);
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
  rewind (f);
}

/* Read environment and prepend it to the default string. */
/* Build token table. */

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
    if ((p = etags_index (p, ':')) && *(++p))
      size++;
  /* Add 1 to leave room for null terminator.  */
  tab = xnew (size + 1, struct TEX_tabent);

  /* Unpack environment string into token table. Be careful about */
  /* zero-length strings (leading ':', "::" and trailing ':') */
  for (i = 0; *env;)
    {
      p = etags_index (env, ':');
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
  char nambuf[BUFSIZ];

  if (*name == 0)
    return;

  /* Let tag name extend to next group close (or end of line) */
  while (*p && *p != TEX_clgrp)
    p++;
  (void) strncpy (nambuf, name, p - name);
  nambuf[p - name] = 0;

  pfnote (nambuf, TRUE, FALSE, lb.buffer, strlen (lb.buffer), lineno, linecharno);
  pfcnt++;
}

/* If the text at CP matches one of the tag-defining TeX command names,
   return the etags_index of that command in TEX_toktab.
   Otherwise return -1.  */

/* Keep the capital `T' in `Token' for dumb truncating compilers
   (this distinguishes it from `TEX_toktab' */
int
TEX_Token (cp)
     char *cp;
{
  int i;

  for (i = 0; TEX_toktab[i].len > 0; i++)
    if (strncmp (TEX_toktab[i].name, cp, TEX_toktab[i].len) == 0)
      return i;
  return -1;
}

/* Support for Prolog.  */

/* whole head (not only functor, but also arguments)
   is gotten in compound term. */

void
prolog_getit (s, lineno, linecharno)
     char *s;
     int lineno;
     long linecharno;
{
  char nambuf[BUFSIZ], *save_s, tmpc;
  int insquote, npar;

  save_s = s;
  insquote = FALSE;
  npar = 0;
  while (1)
    {
      if (*s == '\0')		/* syntax error. */
	return;
      else if (insquote && *s == '\'' && *(s + 1) == '\'')
	s += 2;
      else if (*s == '\'')
	{
	  insquote = !insquote;
	  s++;
	}
      else if (!insquote && *s == '(')
	{
	  npar++;
	  s++;
	}
      else if (!insquote && *s == ')')
	{
	  npar--;
	  s++;
	  if (npar == 0)
	    break;
	  else if (npar < 0)	/* syntax error. */
	    return;
	}
      else if (!insquote && *s == '.' && (isspace (*(s + 1)) || *(s + 1) == '\0'))
	{			/* fullstop. */
	  if (npar != 0)	/* syntax error. */
	    return;
	  s++;
	  break;
	}
      else
	s++;
    }
  tmpc = *s;
  *s = '\0';
  strcpy (nambuf, save_s);
  *s = tmpc;
  pfnote (nambuf, TRUE, FALSE, save_s, strlen (nambuf), lineno, linecharno);
}

/* It is assumed that prolog predicate starts from column 0. */

void
prolog_funcs (fi)
     FILE *fi;
{
  void skip_comment (), prolog_getit ();

  lineno = linecharno = charno = 0;
  while (!feof (fi))
    {
      lineno++;
      linecharno += charno;
      charno = readline (&lb, fi) + 1;	/* 1 for newline. */
      dbp = lb.buffer;
      if (isspace (dbp[0]))	/* not predicate header. */
	continue;
      else if (dbp[0] == '%')	/* comment. */
	continue;
      else if (dbp[0] == '/' && dbp[1] == '*')	/* comment. */
	skip_comment (&lb, fi, &lineno, &linecharno);
      else			/* found. */
	prolog_getit (dbp, lineno, linecharno);
    }
}

void
skip_comment (plb, fi, plineno, plinecharno)
     struct linebuffer *plb;
     FILE *fi;
     int *plineno;		/* result */
     long *plinecharno;		/* result */
{
  while (!substr ("*/", plb->buffer))
    {
      (*plineno)++;
      *plinecharno += readline (plb, fi) + 1;
    }				/* 1 for newline. */
}

/* Return TRUE if 'sub' exists somewhere in 's'. */

int
substr (sub, s)
     char *sub;
     char *s;
{
  while (*s && (s = etags_index (s, *sub)))
    if (prestr (sub, s))
      return (TRUE);
    else
      s++;
  return (FALSE);
}

/* Return TRUE if 'pre' is prefix of string 's'. */

int
prestr (pre, s)
     char *pre;
     char *s;
{
  if (*pre == '\0')
    return (TRUE);
  else if (*pre == *s)
    return (prestr (pre + 1, s + 1));
  else
    return (FALSE);
}

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
readline (linebuffer, stream)
     struct linebuffer *linebuffer;
     register FILE *stream;
{
  char *buffer = linebuffer->buffer;
  register char *p = linebuffer->buffer;
  register char *pend;
  int newline;			/* 1 if ended with newline, 0 if ended with EOF */

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
      if (c < 0 || c == '\n')
	{
	  *p = 0;
	  newline = (c == '\n' ? 1 : 0);
	  break;
	}
      *p++ = c;
    }

  return p - buffer + newline;
}

char *
savestr (cp)
     char *cp;
{
  return savenstr (cp, strlen (cp));
}

char *
savenstr (cp, len)
     char *cp;
     int len;
{
  register char *dp;

  dp = xnew (len + 1, char);
  (void) strncpy (dp, cp, len);
  dp[len] = '\0';
  return dp;
}

/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 *
 * Identical to v7 rindex, included for portability.
 */

char *
etags_rindex (sp, c)
     register char *sp, c;
{
  register char *r;

  r = NULL;
  do
    {
      if (*sp == c)
	r = sp;
  } while (*sp++);
  return (r);
}


/*
 * Return the ptr in sp at which the character c first
 * appears; NULL if not found
 *
 * Identical to v7 index, included for portability.
 */

char *
etags_index (sp, c)
     register char *sp, c;
{
  do
    {
      if (*sp == c)
	return (sp);
  } while (*sp++);
  return (NULL);
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

/* Return a newly-allocated string whose contents concatenate those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = xnew (len1 + len2 + len3 + 1, char);

  (void) strcpy (result, s1);
  (void) strcpy (result + len1, s2);
  (void) strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (size)
     int size;
{
  char *result = malloc (size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

char *
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  char *result = realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}
