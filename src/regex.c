/* Extended regular expression matching and search library, version
   0.12.  (Implements POSIX draft P1003.2/D11.2, except for some of the
   internationalization features.)

   Copyright (C) 1993,94,95,96,97,98,99,2000 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.	 */

/* TODO:
   - structure the opcode space into opcode+flag.
   - merge with glibc's regex.[ch].
   - replace (succeed_n + jump_n + set_number_at) with something that doesn't
     need to modify the compiled regexp so that re_match can be reentrant.
   - get rid of on_failure_jump_smart by doing the optimization in re_comp
     rather than at run-time, so that re_match can be reentrant.
*/

/* AIX requires this to be the first thing in the file. */
#if defined _AIX && !defined REGEX_MALLOC
  #pragma alloca
#endif

#undef	_GNU_SOURCE
#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if defined STDC_HEADERS && !defined emacs
# include <stddef.h>
#else
/* We need this for `regex.h', and perhaps for the Emacs include files.  */
# include <sys/types.h>
#endif

/* Whether to use ISO C Amendment 1 wide char functions.
   Those should not be used for Emacs since it uses its own.  */
#if defined _LIBC
#define WIDE_CHAR_SUPPORT 1
#else
#define WIDE_CHAR_SUPPORT \
	(HAVE_WCTYPE_H && HAVE_WCHAR_H && HAVE_BTOWC && !emacs)
#endif

/* For platform which support the ISO C amendement 1 functionality we
   support user defined character classes.  */
#if WIDE_CHAR_SUPPORT
/* Solaris 2.5 has a bug: <wchar.h> must be included before <wctype.h>.  */
# include <wchar.h>
# include <wctype.h>
#endif

#ifdef _LIBC
/* We have to keep the namespace clean.  */
# define regfree(preg) __regfree (preg)
# define regexec(pr, st, nm, pm, ef) __regexec (pr, st, nm, pm, ef)
# define regcomp(preg, pattern, cflags) __regcomp (preg, pattern, cflags)
# define regerror(errcode, preg, errbuf, errbuf_size) \
	__regerror(errcode, preg, errbuf, errbuf_size)
# define re_set_registers(bu, re, nu, st, en) \
	__re_set_registers (bu, re, nu, st, en)
# define re_match_2(bufp, string1, size1, string2, size2, pos, regs, stop) \
	__re_match_2 (bufp, string1, size1, string2, size2, pos, regs, stop)
# define re_match(bufp, string, size, pos, regs) \
	__re_match (bufp, string, size, pos, regs)
# define re_search(bufp, string, size, startpos, range, regs) \
	__re_search (bufp, string, size, startpos, range, regs)
# define re_compile_pattern(pattern, length, bufp) \
	__re_compile_pattern (pattern, length, bufp)
# define re_set_syntax(syntax) __re_set_syntax (syntax)
# define re_search_2(bufp, st1, s1, st2, s2, startpos, range, regs, stop) \
	__re_search_2 (bufp, st1, s1, st2, s2, startpos, range, regs, stop)
# define re_compile_fastmap(bufp) __re_compile_fastmap (bufp)

/* Make sure we call libc's function even if the user overrides them.  */
# define btowc __btowc
# define iswctype __iswctype
# define wctype __wctype

# define WEAK_ALIAS(a,b) weak_alias (a, b)

/* We are also using some library internals.  */
# include <locale/localeinfo.h>
# include <locale/elem-hash.h>
# include <langinfo.h>
#else
# define WEAK_ALIAS(a,b)
#endif

/* This is for other GNU distributions with internationalized messages.  */
#if HAVE_LIBINTL_H || defined _LIBC
# include <libintl.h>
#else
# define gettext(msgid) (msgid)
#endif

#ifndef gettext_noop
/* This define is so xgettext can find the internationalizable
   strings.  */
# define gettext_noop(String) String
#endif

/* The `emacs' switch turns on certain matching commands
   that make sense only in Emacs. */
#ifdef emacs

# include "lisp.h"
# include "buffer.h"

/* Make syntax table lookup grant data in gl_state.  */
# define SYNTAX_ENTRY_VIA_PROPERTY

# include "syntax.h"
# include "charset.h"
# include "category.h"

# ifdef malloc
#  undef malloc
# endif
# define malloc xmalloc
# ifdef realloc
#  undef realloc
# endif
# define realloc xrealloc
# ifdef free
#  undef free
# endif
# define free xfree

/* Converts the pointer to the char to BEG-based offset from the start.	 */
# define PTR_TO_OFFSET(d) POS_AS_IN_BUFFER (POINTER_TO_OFFSET (d))
# define POS_AS_IN_BUFFER(p) ((p) + (NILP (re_match_object) || BUFFERP (re_match_object)))

# define RE_MULTIBYTE_P(bufp) ((bufp)->multibyte)
# define RE_STRING_CHAR(p, s) \
  (multibyte ? (STRING_CHAR (p, s)) : (*(p)))
# define RE_STRING_CHAR_AND_LENGTH(p, s, len) \
  (multibyte ? (STRING_CHAR_AND_LENGTH (p, s, len)) : ((len) = 1, *(p)))

/* Set C a (possibly multibyte) character before P.  P points into a
   string which is the virtual concatenation of STR1 (which ends at
   END1) or STR2 (which ends at END2).  */
# define GET_CHAR_BEFORE_2(c, p, str1, end1, str2, end2)		\
  do {									\
    if (multibyte)							\
       {						    		\
	 re_char *dtemp = (p) == (str2) ? (end1) : (p);		    	\
	 re_char *dlimit = ((p) > (str2) && (p) <= (end2)) ? (str2) : (str1); \
	 while (dtemp-- > dlimit && !CHAR_HEAD_P (*dtemp));		\
	 c = STRING_CHAR (dtemp, (p) - dtemp);				\
       }						    		\
     else						    		\
       (c = ((p) == (str2) ? (end1) : (p))[-1]);			\
  } while (0)


#else  /* not emacs */

/* If we are not linking with Emacs proper,
   we can't use the relocating allocator
   even if config.h says that we can.  */
# undef REL_ALLOC

# if defined STDC_HEADERS || defined _LIBC
#  include <stdlib.h>
# else
char *malloc ();
char *realloc ();
# endif

/* When used in Emacs's lib-src, we need to get bzero and bcopy somehow.
   If nothing else has been done, use the method below.  */
# ifdef INHIBIT_STRING_HEADER
#  if !(defined HAVE_BZERO && defined HAVE_BCOPY)
#   if !defined bzero && !defined bcopy
#    undef INHIBIT_STRING_HEADER
#   endif
#  endif
# endif

/* This is the normal way of making sure we have memcpy, memcmp and bzero.
   This is used in most programs--a few other programs avoid this
   by defining INHIBIT_STRING_HEADER.  */
# ifndef INHIBIT_STRING_HEADER
#  if defined HAVE_STRING_H || defined STDC_HEADERS || defined _LIBC
#   include <string.h>
#   ifndef bzero
#    ifndef _LIBC
#     define bzero(s, n)	(memset (s, '\0', n), (s))
#    else
#     define bzero(s, n)	__bzero (s, n)
#    endif
#   endif
#  else
#   include <strings.h>
#   ifndef memcmp
#    define memcmp(s1, s2, n)	bcmp (s1, s2, n)
#   endif
#   ifndef memcpy
#    define memcpy(d, s, n)	(bcopy (s, d, n), (d))
#   endif
#  endif
# endif

/* Define the syntax stuff for \<, \>, etc.  */

/* Sword must be nonzero for the wordchar pattern commands in re_match_2.  */
enum syntaxcode { Swhitespace = 0, Sword = 1 };

# ifdef SWITCH_ENUM_BUG
#  define SWITCH_ENUM_CAST(x) ((int)(x))
# else
#  define SWITCH_ENUM_CAST(x) (x)
# endif

/* Dummy macros for non-Emacs environments.  */
# define BASE_LEADING_CODE_P(c) (0)
# define CHAR_CHARSET(c) 0
# define CHARSET_LEADING_CODE_BASE(c) 0
# define MAX_MULTIBYTE_LENGTH 1
# define RE_MULTIBYTE_P(x) 0
# define WORD_BOUNDARY_P(c1, c2) (0)
# define CHAR_HEAD_P(p) (1)
# define SINGLE_BYTE_CHAR_P(c) (1)
# define SAME_CHARSET_P(c1, c2) (1)
# define MULTIBYTE_FORM_LENGTH(p, s) (1)
# define STRING_CHAR(p, s) (*(p))
# define RE_STRING_CHAR STRING_CHAR
# define CHAR_STRING(c, s) (*(s) = (c), 1)
# define STRING_CHAR_AND_LENGTH(p, s, actual_len) ((actual_len) = 1, *(p))
# define RE_STRING_CHAR_AND_LENGTH STRING_CHAR_AND_LENGTH
# define GET_CHAR_BEFORE_2(c, p, str1, end1, str2, end2) \
  (c = ((p) == (str2) ? *((end1) - 1) : *((p) - 1)))
# define MAKE_CHAR(charset, c1, c2) (c1)
#endif /* not emacs */

#ifndef RE_TRANSLATE
# define RE_TRANSLATE(TBL, C) ((unsigned char)(TBL)[C])
# define RE_TRANSLATE_P(TBL) (TBL)
#endif

/* Get the interface, including the syntax bits.  */
#include "regex.h"

/* isalpha etc. are used for the character classes.  */
#include <ctype.h>

#ifdef emacs

/* 1 if C is an ASCII character.  */
# define IS_REAL_ASCII(c) ((c) < 0200)

/* 1 if C is a unibyte character.  */
# define ISUNIBYTE(c) (SINGLE_BYTE_CHAR_P ((c)))

/* The Emacs definitions should not be directly affected by locales.  */

/* In Emacs, these are only used for single-byte characters.  */
# define ISDIGIT(c) ((c) >= '0' && (c) <= '9')
# define ISCNTRL(c) ((c) < ' ')
# define ISXDIGIT(c) (((c) >= '0' && (c) <= '9')		\
		     || ((c) >= 'a' && (c) <= 'f')	\
		     || ((c) >= 'A' && (c) <= 'F'))

/* This is only used for single-byte characters.  */
# define ISBLANK(c) ((c) == ' ' || (c) == '\t')

/* The rest must handle multibyte characters.  */

# define ISGRAPH(c) (SINGLE_BYTE_CHAR_P (c)				\
		    ? (c) > ' ' && !((c) >= 0177 && (c) <= 0237)	\
		    : 1)

# define ISPRINT(c) (SINGLE_BYTE_CHAR_P (c)				\
		    ? (c) >= ' ' && !((c) >= 0177 && (c) <= 0237)	\
		    : 1)

# define ISALNUM(c) (IS_REAL_ASCII (c)			\
		    ? (((c) >= 'a' && (c) <= 'z')	\
		       || ((c) >= 'A' && (c) <= 'Z')	\
		       || ((c) >= '0' && (c) <= '9'))	\
		    : SYNTAX (c) == Sword)

# define ISALPHA(c) (IS_REAL_ASCII (c)			\
		    ? (((c) >= 'a' && (c) <= 'z')	\
		       || ((c) >= 'A' && (c) <= 'Z'))	\
		    : SYNTAX (c) == Sword)

# define ISLOWER(c) (LOWERCASEP (c))

# define ISPUNCT(c) (IS_REAL_ASCII (c)				\
		    ? ((c) > ' ' && (c) < 0177			\
		       && !(((c) >= 'a' && (c) <= 'z')		\
		            || ((c) >= 'A' && (c) <= 'Z')	\
		            || ((c) >= '0' && (c) <= '9')))	\
		    : SYNTAX (c) != Sword)

# define ISSPACE(c) (SYNTAX (c) == Swhitespace)

# define ISUPPER(c) (UPPERCASEP (c))

# define ISWORD(c) (SYNTAX (c) == Sword)

#else /* not emacs */

/* Jim Meyering writes:

   "... Some ctype macros are valid only for character codes that
   isascii says are ASCII (SGI's IRIX-4.0.5 is one such system --when
   using /bin/cc or gcc but without giving an ansi option).  So, all
   ctype uses should be through macros like ISPRINT...  If
   STDC_HEADERS is defined, then autoconf has verified that the ctype
   macros don't need to be guarded with references to isascii. ...
   Defining isascii to 1 should let any compiler worth its salt
   eliminate the && through constant folding."
   Solaris defines some of these symbols so we must undefine them first.  */

# undef ISASCII
# if defined STDC_HEADERS || (!defined isascii && !defined HAVE_ISASCII)
#  define ISASCII(c) 1
# else
#  define ISASCII(c) isascii(c)
# endif

/* 1 if C is an ASCII character.  */
# define IS_REAL_ASCII(c) ((c) < 0200)

/* This distinction is not meaningful, except in Emacs.  */
# define ISUNIBYTE(c) 1

# ifdef isblank
#  define ISBLANK(c) (ISASCII (c) && isblank (c))
# else
#  define ISBLANK(c) ((c) == ' ' || (c) == '\t')
# endif
# ifdef isgraph
#  define ISGRAPH(c) (ISASCII (c) && isgraph (c))
# else
#  define ISGRAPH(c) (ISASCII (c) && isprint (c) && !isspace (c))
# endif

# undef ISPRINT
# define ISPRINT(c) (ISASCII (c) && isprint (c))
# define ISDIGIT(c) (ISASCII (c) && isdigit (c))
# define ISALNUM(c) (ISASCII (c) && isalnum (c))
# define ISALPHA(c) (ISASCII (c) && isalpha (c))
# define ISCNTRL(c) (ISASCII (c) && iscntrl (c))
# define ISLOWER(c) (ISASCII (c) && islower (c))
# define ISPUNCT(c) (ISASCII (c) && ispunct (c))
# define ISSPACE(c) (ISASCII (c) && isspace (c))
# define ISUPPER(c) (ISASCII (c) && isupper (c))
# define ISXDIGIT(c) (ISASCII (c) && isxdigit (c))

# define ISWORD(c) ISALPHA(c)

# ifdef _tolower
#  define TOLOWER(c) _tolower(c)
# else
#  define TOLOWER(c) tolower(c)
# endif

/* How many characters in the character set.  */
# define CHAR_SET_SIZE 256

# ifdef SYNTAX_TABLE

extern char *re_syntax_table;

# else /* not SYNTAX_TABLE */

static char re_syntax_table[CHAR_SET_SIZE];

static void
init_syntax_once ()
{
   register int c;
   static int done = 0;

   if (done)
     return;

   bzero (re_syntax_table, sizeof re_syntax_table);

   for (c = 0; c < CHAR_SET_SIZE; ++c)
     if (ISALNUM (c))
	re_syntax_table[c] = Sword;

   re_syntax_table['_'] = Sword;

   done = 1;
}

# endif /* not SYNTAX_TABLE */

# define SYNTAX(c) re_syntax_table[(c)]

#endif /* not emacs */

#ifndef NULL
# define NULL (void *)0
#endif

/* We remove any previous definition of `SIGN_EXTEND_CHAR',
   since ours (we hope) works properly with all combinations of
   machines, compilers, `char' and `unsigned char' argument types.
   (Per Bothner suggested the basic approach.)  */
#undef SIGN_EXTEND_CHAR
#if __STDC__
# define SIGN_EXTEND_CHAR(c) ((signed char) (c))
#else  /* not __STDC__ */
/* As in Harbison and Steele.  */
# define SIGN_EXTEND_CHAR(c) ((((unsigned char) (c)) ^ 128) - 128)
#endif

/* Should we use malloc or alloca?  If REGEX_MALLOC is not defined, we
   use `alloca' instead of `malloc'.  This is because using malloc in
   re_search* or re_match* could cause memory leaks when C-g is used in
   Emacs; also, malloc is slower and causes storage fragmentation.  On
   the other hand, malloc is more portable, and easier to debug.

   Because we sometimes use alloca, some routines have to be macros,
   not functions -- `alloca'-allocated space disappears at the end of the
   function it is called in.  */

#ifdef REGEX_MALLOC

# define REGEX_ALLOCATE malloc
# define REGEX_REALLOCATE(source, osize, nsize) realloc (source, nsize)
# define REGEX_FREE free

#else /* not REGEX_MALLOC  */

/* Emacs already defines alloca, sometimes.  */
# ifndef alloca

/* Make alloca work the best possible way.  */
#  ifdef __GNUC__
#   define alloca __builtin_alloca
#  else /* not __GNUC__ */
#   if HAVE_ALLOCA_H
#    include <alloca.h>
#   endif /* HAVE_ALLOCA_H */
#  endif /* not __GNUC__ */

# endif /* not alloca */

# define REGEX_ALLOCATE alloca

/* Assumes a `char *destination' variable.  */
# define REGEX_REALLOCATE(source, osize, nsize)				\
  (destination = (char *) alloca (nsize),				\
   memcpy (destination, source, osize))

/* No need to do anything to free, after alloca.  */
# define REGEX_FREE(arg) ((void)0) /* Do nothing!  But inhibit gcc warning.  */

#endif /* not REGEX_MALLOC */

/* Define how to allocate the failure stack.  */

#if defined REL_ALLOC && defined REGEX_MALLOC

# define REGEX_ALLOCATE_STACK(size)				\
  r_alloc (&failure_stack_ptr, (size))
# define REGEX_REALLOCATE_STACK(source, osize, nsize)		\
  r_re_alloc (&failure_stack_ptr, (nsize))
# define REGEX_FREE_STACK(ptr)					\
  r_alloc_free (&failure_stack_ptr)

#else /* not using relocating allocator */

# ifdef REGEX_MALLOC

#  define REGEX_ALLOCATE_STACK malloc
#  define REGEX_REALLOCATE_STACK(source, osize, nsize) realloc (source, nsize)
#  define REGEX_FREE_STACK free

# else /* not REGEX_MALLOC */

#  define REGEX_ALLOCATE_STACK alloca

#  define REGEX_REALLOCATE_STACK(source, osize, nsize)			\
   REGEX_REALLOCATE (source, osize, nsize)
/* No need to explicitly free anything.	 */
#  define REGEX_FREE_STACK(arg) ((void)0)

# endif /* not REGEX_MALLOC */
#endif /* not using relocating allocator */


/* True if `size1' is non-NULL and PTR is pointing anywhere inside
   `string1' or just past its end.  This works if PTR is NULL, which is
   a good thing.  */
#define FIRST_STRING_P(ptr)					\
  (size1 && string1 <= (ptr) && (ptr) <= string1 + size1)

/* (Re)Allocate N items of type T using malloc, or fail.  */
#define TALLOC(n, t) ((t *) malloc ((n) * sizeof (t)))
#define RETALLOC(addr, n, t) ((addr) = (t *) realloc (addr, (n) * sizeof (t)))
#define RETALLOC_IF(addr, n, t) \
  if (addr) RETALLOC((addr), (n), t); else (addr) = TALLOC ((n), t)
#define REGEX_TALLOC(n, t) ((t *) REGEX_ALLOCATE ((n) * sizeof (t)))

#define BYTEWIDTH 8 /* In bits.  */

#define STREQ(s1, s2) ((strcmp (s1, s2) == 0))

#undef MAX
#undef MIN
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

/* Type of source-pattern and string chars.  */
typedef const unsigned char re_char;

typedef char boolean;
#define false 0
#define true 1

static int re_match_2_internal _RE_ARGS ((struct re_pattern_buffer *bufp,
					re_char *string1, int size1,
					re_char *string2, int size2,
					int pos,
					struct re_registers *regs,
					int stop));

/* These are the command codes that appear in compiled regular
   expressions.  Some opcodes are followed by argument bytes.  A
   command code can specify any interpretation whatsoever for its
   arguments.  Zero bytes may appear in the compiled regular expression.  */

typedef enum
{
  no_op = 0,

  /* Succeed right away--no more backtracking.  */
  succeed,

	/* Followed by one byte giving n, then by n literal bytes.  */
  exactn,

	/* Matches any (more or less) character.  */
  anychar,

	/* Matches any one char belonging to specified set.  First
	   following byte is number of bitmap bytes.  Then come bytes
	   for a bitmap saying which chars are in.  Bits in each byte
	   are ordered low-bit-first.  A character is in the set if its
	   bit is 1.  A character too large to have a bit in the map is
	   automatically not in the set.

	   If the length byte has the 0x80 bit set, then that stuff
	   is followed by a range table:
	       2 bytes of flags for character sets (low 8 bits, high 8 bits)
		   See RANGE_TABLE_WORK_BITS below.
	       2 bytes, the number of pairs that follow (upto 32767)
	       pairs, each 2 multibyte characters,
		   each multibyte character represented as 3 bytes.  */
  charset,

	/* Same parameters as charset, but match any character that is
	   not one of those specified.  */
  charset_not,

	/* Start remembering the text that is matched, for storing in a
	   register.  Followed by one byte with the register number, in
	   the range 0 to one less than the pattern buffer's re_nsub
	   field.  */
  start_memory,

	/* Stop remembering the text that is matched and store it in a
	   memory register.  Followed by one byte with the register
	   number, in the range 0 to one less than `re_nsub' in the
	   pattern buffer.  */
  stop_memory,

	/* Match a duplicate of something remembered. Followed by one
	   byte containing the register number.  */
  duplicate,

	/* Fail unless at beginning of line.  */
  begline,

	/* Fail unless at end of line.  */
  endline,

	/* Succeeds if at beginning of buffer (if emacs) or at beginning
	   of string to be matched (if not).  */
  begbuf,

	/* Analogously, for end of buffer/string.  */
  endbuf,

	/* Followed by two byte relative address to which to jump.  */
  jump,

	/* Followed by two-byte relative address of place to resume at
	   in case of failure.	*/
  on_failure_jump,

	/* Like on_failure_jump, but pushes a placeholder instead of the
	   current string position when executed.  */
  on_failure_keep_string_jump,

	/* Just like `on_failure_jump', except that it checks that we
	   don't get stuck in an infinite loop (matching an empty string
	   indefinitely).  */
  on_failure_jump_loop,

	/* Just like `on_failure_jump_loop', except that it checks for
	   a different kind of loop (the kind that shows up with non-greedy
	   operators).  This operation has to be immediately preceded
	   by a `no_op'.  */
  on_failure_jump_nastyloop,

	/* A smart `on_failure_jump' used for greedy * and + operators.
	   It analyses the loop before which it is put and if the
	   loop does not require backtracking, it changes itself to
	   `on_failure_keep_string_jump' and short-circuits the loop,
	   else it just defaults to changing itself into `on_failure_jump'.
	   It assumes that it is pointing to just past a `jump'.  */
  on_failure_jump_smart,

	/* Followed by two-byte relative address and two-byte number n.
	   After matching N times, jump to the address upon failure.
	   Does not work if N starts at 0: use on_failure_jump_loop
	   instead.  */
  succeed_n,

	/* Followed by two-byte relative address, and two-byte number n.
	   Jump to the address N times, then fail.  */
  jump_n,

	/* Set the following two-byte relative address to the
	   subsequent two-byte number.	The address *includes* the two
	   bytes of number.  */
  set_number_at,

  wordbeg,	/* Succeeds if at word beginning.  */
  wordend,	/* Succeeds if at word end.  */

  wordbound,	/* Succeeds if at a word boundary.  */
  notwordbound,	/* Succeeds if not at a word boundary.	*/

	/* Matches any character whose syntax is specified.  Followed by
	   a byte which contains a syntax code, e.g., Sword.  */
  syntaxspec,

	/* Matches any character whose syntax is not that specified.  */
  notsyntaxspec

#ifdef emacs
  ,before_dot,	/* Succeeds if before point.  */
  at_dot,	/* Succeeds if at point.  */
  after_dot,	/* Succeeds if after point.  */

  /* Matches any character whose category-set contains the specified
     category.	The operator is followed by a byte which contains a
     category code (mnemonic ASCII character).	*/
  categoryspec,

  /* Matches any character whose category-set does not contain the
     specified category.  The operator is followed by a byte which
     contains the category code (mnemonic ASCII character).  */
  notcategoryspec
#endif /* emacs */
} re_opcode_t;

/* Common operations on the compiled pattern.  */

/* Store NUMBER in two contiguous bytes starting at DESTINATION.  */

#define STORE_NUMBER(destination, number)				\
  do {									\
    (destination)[0] = (number) & 0377;					\
    (destination)[1] = (number) >> 8;					\
  } while (0)

/* Same as STORE_NUMBER, except increment DESTINATION to
   the byte after where the number is stored.  Therefore, DESTINATION
   must be an lvalue.  */

#define STORE_NUMBER_AND_INCR(destination, number)			\
  do {									\
    STORE_NUMBER (destination, number);					\
    (destination) += 2;							\
  } while (0)

/* Put into DESTINATION a number stored in two contiguous bytes starting
   at SOURCE.  */

#define EXTRACT_NUMBER(destination, source)				\
  do {									\
    (destination) = *(source) & 0377;					\
    (destination) += SIGN_EXTEND_CHAR (*((source) + 1)) << 8;		\
  } while (0)

#ifdef DEBUG
static void extract_number _RE_ARGS ((int *dest, re_char *source));
static void
extract_number (dest, source)
    int *dest;
    re_char *source;
{
  int temp = SIGN_EXTEND_CHAR (*(source + 1));
  *dest = *source & 0377;
  *dest += temp << 8;
}

# ifndef EXTRACT_MACROS /* To debug the macros.  */
#  undef EXTRACT_NUMBER
#  define EXTRACT_NUMBER(dest, src) extract_number (&dest, src)
# endif /* not EXTRACT_MACROS */

#endif /* DEBUG */

/* Same as EXTRACT_NUMBER, except increment SOURCE to after the number.
   SOURCE must be an lvalue.  */

#define EXTRACT_NUMBER_AND_INCR(destination, source)			\
  do {									\
    EXTRACT_NUMBER (destination, source);				\
    (source) += 2;							\
  } while (0)

#ifdef DEBUG
static void extract_number_and_incr _RE_ARGS ((int *destination,
					       re_char **source));
static void
extract_number_and_incr (destination, source)
    int *destination;
    re_char **source;
{
  extract_number (destination, *source);
  *source += 2;
}

# ifndef EXTRACT_MACROS
#  undef EXTRACT_NUMBER_AND_INCR
#  define EXTRACT_NUMBER_AND_INCR(dest, src) \
  extract_number_and_incr (&dest, &src)
# endif /* not EXTRACT_MACROS */

#endif /* DEBUG */

/* Store a multibyte character in three contiguous bytes starting
   DESTINATION, and increment DESTINATION to the byte after where the
   character is stored.	 Therefore, DESTINATION must be an lvalue.  */

#define STORE_CHARACTER_AND_INCR(destination, character)	\
  do {								\
    (destination)[0] = (character) & 0377;			\
    (destination)[1] = ((character) >> 8) & 0377;		\
    (destination)[2] = (character) >> 16;			\
    (destination) += 3;						\
  } while (0)

/* Put into DESTINATION a character stored in three contiguous bytes
   starting at SOURCE.	*/

#define EXTRACT_CHARACTER(destination, source)	\
  do {						\
    (destination) = ((source)[0]		\
		     | ((source)[1] << 8)	\
		     | ((source)[2] << 16));	\
  } while (0)


/* Macros for charset. */

/* Size of bitmap of charset P in bytes.  P is a start of charset,
   i.e. *P is (re_opcode_t) charset or (re_opcode_t) charset_not.  */
#define CHARSET_BITMAP_SIZE(p) ((p)[1] & 0x7F)

/* Nonzero if charset P has range table.  */
#define CHARSET_RANGE_TABLE_EXISTS_P(p)	 ((p)[1] & 0x80)

/* Return the address of range table of charset P.  But not the start
   of table itself, but the before where the number of ranges is
   stored.  `2 +' means to skip re_opcode_t and size of bitmap,
   and the 2 bytes of flags at the start of the range table.  */
#define CHARSET_RANGE_TABLE(p) (&(p)[4 + CHARSET_BITMAP_SIZE (p)])

/* Extract the bit flags that start a range table.  */
#define CHARSET_RANGE_TABLE_BITS(p)		\
  ((p)[2 + CHARSET_BITMAP_SIZE (p)]		\
   + (p)[3 + CHARSET_BITMAP_SIZE (p)] * 0x100)

/* Test if C is listed in the bitmap of charset P.  */
#define CHARSET_LOOKUP_BITMAP(p, c)				\
  ((c) < CHARSET_BITMAP_SIZE (p) * BYTEWIDTH			\
   && (p)[2 + (c) / BYTEWIDTH] & (1 << ((c) % BYTEWIDTH)))

/* Return the address of end of RANGE_TABLE.  COUNT is number of
   ranges (which is a pair of (start, end)) in the RANGE_TABLE.	 `* 2'
   is start of range and end of range.	`* 3' is size of each start
   and end.  */
#define CHARSET_RANGE_TABLE_END(range_table, count)	\
  ((range_table) + (count) * 2 * 3)

/* Test if C is in RANGE_TABLE.	 A flag NOT is negated if C is in.
   COUNT is number of ranges in RANGE_TABLE.  */
#define CHARSET_LOOKUP_RANGE_TABLE_RAW(not, c, range_table, count)	\
  do									\
    {									\
      re_wchar_t range_start, range_end;				\
      re_char *p;							\
      re_char *range_table_end						\
	= CHARSET_RANGE_TABLE_END ((range_table), (count));		\
									\
      for (p = (range_table); p < range_table_end; p += 2 * 3)		\
	{								\
	  EXTRACT_CHARACTER (range_start, p);				\
	  EXTRACT_CHARACTER (range_end, p + 3);				\
									\
	  if (range_start <= (c) && (c) <= range_end)			\
	    {								\
	      (not) = !(not);						\
	      break;							\
	    }								\
	}								\
    }									\
  while (0)

/* Test if C is in range table of CHARSET.  The flag NOT is negated if
   C is listed in it.  */
#define CHARSET_LOOKUP_RANGE_TABLE(not, c, charset)			\
  do									\
    {									\
      /* Number of ranges in range table. */				\
      int count;							\
      re_char *range_table = CHARSET_RANGE_TABLE (charset);		\
      									\
      EXTRACT_NUMBER_AND_INCR (count, range_table);			\
      CHARSET_LOOKUP_RANGE_TABLE_RAW ((not), (c), range_table, count);	\
    }									\
  while (0)

/* If DEBUG is defined, Regex prints many voluminous messages about what
   it is doing (if the variable `debug' is nonzero).  If linked with the
   main program in `iregex.c', you can enter patterns and strings
   interactively.  And if linked with the main program in `main.c' and
   the other test files, you can run the already-written tests.  */

#ifdef DEBUG

/* We use standard I/O for debugging.  */
# include <stdio.h>

/* It is useful to test things that ``must'' be true when debugging.  */
# include <assert.h>

static int debug = -100000;

# define DEBUG_STATEMENT(e) e
# define DEBUG_PRINT1(x) if (debug > 0) printf (x)
# define DEBUG_PRINT2(x1, x2) if (debug > 0) printf (x1, x2)
# define DEBUG_PRINT3(x1, x2, x3) if (debug > 0) printf (x1, x2, x3)
# define DEBUG_PRINT4(x1, x2, x3, x4) if (debug > 0) printf (x1, x2, x3, x4)
# define DEBUG_PRINT_COMPILED_PATTERN(p, s, e)				\
  if (debug > 0) print_partial_compiled_pattern (s, e)
# define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)			\
  if (debug > 0) print_double_string (w, s1, sz1, s2, sz2)


/* Print the fastmap in human-readable form.  */

void
print_fastmap (fastmap)
    char *fastmap;
{
  unsigned was_a_range = 0;
  unsigned i = 0;

  while (i < (1 << BYTEWIDTH))
    {
      if (fastmap[i++])
	{
	  was_a_range = 0;
	  putchar (i - 1);
	  while (i < (1 << BYTEWIDTH)  &&  fastmap[i])
	    {
	      was_a_range = 1;
	      i++;
	    }
	  if (was_a_range)
	    {
	      printf ("-");
	      putchar (i - 1);
	    }
	}
    }
  putchar ('\n');
}


/* Print a compiled pattern string in human-readable form, starting at
   the START pointer into it and ending just before the pointer END.  */

void
print_partial_compiled_pattern (start, end)
    re_char *start;
    re_char *end;
{
  int mcnt, mcnt2;
  re_char *p = start;
  re_char *pend = end;

  if (start == NULL)
    {
      printf ("(null)\n");
      return;
    }

  /* Loop over pattern commands.  */
  while (p < pend)
    {
      printf ("%d:\t", p - start);

      switch ((re_opcode_t) *p++)
	{
	case no_op:
	  printf ("/no_op");
	  break;

	case succeed:
	  printf ("/succeed");
	  break;

	case exactn:
	  mcnt = *p++;
	  printf ("/exactn/%d", mcnt);
	  do
	    {
	      putchar ('/');
	      putchar (*p++);
	    }
	  while (--mcnt);
	  break;

	case start_memory:
	  printf ("/start_memory/%d", *p++);
	  break;

	case stop_memory:
	  printf ("/stop_memory/%d", *p++);
	  break;

	case duplicate:
	  printf ("/duplicate/%d", *p++);
	  break;

	case anychar:
	  printf ("/anychar");
	  break;

	case charset:
	case charset_not:
	  {
	    register int c, last = -100;
	    register int in_range = 0;
	    int length = CHARSET_BITMAP_SIZE (p - 1);
	    int has_range_table = CHARSET_RANGE_TABLE_EXISTS_P (p - 1);

	    printf ("/charset [%s",
		    (re_opcode_t) *(p - 1) == charset_not ? "^" : "");

	    assert (p + *p < pend);

	    for (c = 0; c < 256; c++)
	      if (c / 8 < length
		  && (p[1 + (c/8)] & (1 << (c % 8))))
		{
		  /* Are we starting a range?  */
		  if (last + 1 == c && ! in_range)
		    {
		      putchar ('-');
		      in_range = 1;
		    }
		  /* Have we broken a range?  */
		  else if (last + 1 != c && in_range)
		    {
		      putchar (last);
		      in_range = 0;
		    }

		  if (! in_range)
		    putchar (c);

		  last = c;
	      }

	    if (in_range)
	      putchar (last);

	    putchar (']');

	    p += 1 + length;

	    if (has_range_table)
	      {
		int count;
		printf ("has-range-table");

		/* ??? Should print the range table; for now, just skip it.  */
		p += 2;		/* skip range table bits */
		EXTRACT_NUMBER_AND_INCR (count, p);
		p = CHARSET_RANGE_TABLE_END (p, count);
	      }
	  }
	  break;

	case begline:
	  printf ("/begline");
	  break;

	case endline:
	  printf ("/endline");
	  break;

	case on_failure_jump:
	  extract_number_and_incr (&mcnt, &p);
	  printf ("/on_failure_jump to %d", p + mcnt - start);
	  break;

	case on_failure_keep_string_jump:
	  extract_number_and_incr (&mcnt, &p);
	  printf ("/on_failure_keep_string_jump to %d", p + mcnt - start);
	  break;

	case on_failure_jump_nastyloop:
	  extract_number_and_incr (&mcnt, &p);
	  printf ("/on_failure_jump_nastyloop to %d", p + mcnt - start);
	  break;

	case on_failure_jump_loop:
	  extract_number_and_incr (&mcnt, &p);
	  printf ("/on_failure_jump_loop to %d", p + mcnt - start);
	  break;

	case on_failure_jump_smart:
	  extract_number_and_incr (&mcnt, &p);
	  printf ("/on_failure_jump_smart to %d", p + mcnt - start);
	  break;

	case jump:
	  extract_number_and_incr (&mcnt, &p);
	  printf ("/jump to %d", p + mcnt - start);
	  break;

	case succeed_n:
	  extract_number_and_incr (&mcnt, &p);
	  extract_number_and_incr (&mcnt2, &p);
	  printf ("/succeed_n to %d, %d times", p - 2 + mcnt - start, mcnt2);
	  break;

	case jump_n:
	  extract_number_and_incr (&mcnt, &p);
	  extract_number_and_incr (&mcnt2, &p);
	  printf ("/jump_n to %d, %d times", p - 2 + mcnt - start, mcnt2);
	  break;

	case set_number_at:
	  extract_number_and_incr (&mcnt, &p);
	  extract_number_and_incr (&mcnt2, &p);
	  printf ("/set_number_at location %d to %d", p - 2 + mcnt - start, mcnt2);
	  break;

	case wordbound:
	  printf ("/wordbound");
	  break;

	case notwordbound:
	  printf ("/notwordbound");
	  break;

	case wordbeg:
	  printf ("/wordbeg");
	  break;

	case wordend:
	  printf ("/wordend");

	case syntaxspec:
	  printf ("/syntaxspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;

	case notsyntaxspec:
	  printf ("/notsyntaxspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;

# ifdef emacs
	case before_dot:
	  printf ("/before_dot");
	  break;

	case at_dot:
	  printf ("/at_dot");
	  break;

	case after_dot:
	  printf ("/after_dot");
	  break;

	case categoryspec:
	  printf ("/categoryspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;

	case notcategoryspec:
	  printf ("/notcategoryspec");
	  mcnt = *p++;
	  printf ("/%d", mcnt);
	  break;
# endif /* emacs */

	case begbuf:
	  printf ("/begbuf");
	  break;

	case endbuf:
	  printf ("/endbuf");
	  break;

	default:
	  printf ("?%d", *(p-1));
	}

      putchar ('\n');
    }

  printf ("%d:\tend of pattern.\n", p - start);
}


void
print_compiled_pattern (bufp)
    struct re_pattern_buffer *bufp;
{
  re_char *buffer = bufp->buffer;

  print_partial_compiled_pattern (buffer, buffer + bufp->used);
  printf ("%ld bytes used/%ld bytes allocated.\n",
	  bufp->used, bufp->allocated);

  if (bufp->fastmap_accurate && bufp->fastmap)
    {
      printf ("fastmap: ");
      print_fastmap (bufp->fastmap);
    }

  printf ("re_nsub: %d\t", bufp->re_nsub);
  printf ("regs_alloc: %d\t", bufp->regs_allocated);
  printf ("can_be_null: %d\t", bufp->can_be_null);
  printf ("no_sub: %d\t", bufp->no_sub);
  printf ("not_bol: %d\t", bufp->not_bol);
  printf ("not_eol: %d\t", bufp->not_eol);
  printf ("syntax: %lx\n", bufp->syntax);
  fflush (stdout);
  /* Perhaps we should print the translate table?  */
}


void
print_double_string (where, string1, size1, string2, size2)
    re_char *where;
    re_char *string1;
    re_char *string2;
    int size1;
    int size2;
{
  int this_char;

  if (where == NULL)
    printf ("(null)");
  else
    {
      if (FIRST_STRING_P (where))
	{
	  for (this_char = where - string1; this_char < size1; this_char++)
	    putchar (string1[this_char]);

	  where = string2;
	}

      for (this_char = where - string2; this_char < size2; this_char++)
	putchar (string2[this_char]);
    }
}

#else /* not DEBUG */

# undef assert
# define assert(e)

# define DEBUG_STATEMENT(e)
# define DEBUG_PRINT1(x)
# define DEBUG_PRINT2(x1, x2)
# define DEBUG_PRINT3(x1, x2, x3)
# define DEBUG_PRINT4(x1, x2, x3, x4)
# define DEBUG_PRINT_COMPILED_PATTERN(p, s, e)
# define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)

#endif /* not DEBUG */

/* Set by `re_set_syntax' to the current regexp syntax to recognize.  Can
   also be assigned to arbitrarily: each pattern buffer stores its own
   syntax, so it can be changed between regex compilations.  */
/* This has no initializer because initialized variables in Emacs
   become read-only after dumping.  */
reg_syntax_t re_syntax_options;


/* Specify the precise syntax of regexps for compilation.  This provides
   for compatibility for various utilities which historically have
   different, incompatible syntaxes.

   The argument SYNTAX is a bit mask comprised of the various bits
   defined in regex.h.  We return the old syntax.  */

reg_syntax_t
re_set_syntax (syntax)
    reg_syntax_t syntax;
{
  reg_syntax_t ret = re_syntax_options;

  re_syntax_options = syntax;
  return ret;
}
WEAK_ALIAS (__re_set_syntax, re_set_syntax)

/* This table gives an error message for each of the error codes listed
   in regex.h.  Obviously the order here has to be same as there.
   POSIX doesn't require that we do anything for REG_NOERROR,
   but why not be nice?  */

static const char *re_error_msgid[] =
  {
    gettext_noop ("Success"),	/* REG_NOERROR */
    gettext_noop ("No match"),	/* REG_NOMATCH */
    gettext_noop ("Invalid regular expression"), /* REG_BADPAT */
    gettext_noop ("Invalid collation character"), /* REG_ECOLLATE */
    gettext_noop ("Invalid character class name"), /* REG_ECTYPE */
    gettext_noop ("Trailing backslash"), /* REG_EESCAPE */
    gettext_noop ("Invalid back reference"), /* REG_ESUBREG */
    gettext_noop ("Unmatched [ or [^"),	/* REG_EBRACK */
    gettext_noop ("Unmatched ( or \\("), /* REG_EPAREN */
    gettext_noop ("Unmatched \\{"), /* REG_EBRACE */
    gettext_noop ("Invalid content of \\{\\}"), /* REG_BADBR */
    gettext_noop ("Invalid range end"),	/* REG_ERANGE */
    gettext_noop ("Memory exhausted"), /* REG_ESPACE */
    gettext_noop ("Invalid preceding regular expression"), /* REG_BADRPT */
    gettext_noop ("Premature end of regular expression"), /* REG_EEND */
    gettext_noop ("Regular expression too big"), /* REG_ESIZE */
    gettext_noop ("Unmatched ) or \\)"), /* REG_ERPAREN */
  };

/* Avoiding alloca during matching, to placate r_alloc.  */

/* Define MATCH_MAY_ALLOCATE unless we need to make sure that the
   searching and matching functions should not call alloca.  On some
   systems, alloca is implemented in terms of malloc, and if we're
   using the relocating allocator routines, then malloc could cause a
   relocation, which might (if the strings being searched are in the
   ralloc heap) shift the data out from underneath the regexp
   routines.

   Here's another reason to avoid allocation: Emacs
   processes input from X in a signal handler; processing X input may
   call malloc; if input arrives while a matching routine is calling
   malloc, then we're scrod.  But Emacs can't just block input while
   calling matching routines; then we don't notice interrupts when
   they come in.  So, Emacs blocks input around all regexp calls
   except the matching calls, which it leaves unprotected, in the
   faith that they will not malloc.  */

/* Normally, this is fine.  */
#define MATCH_MAY_ALLOCATE

/* When using GNU C, we are not REALLY using the C alloca, no matter
   what config.h may say.  So don't take precautions for it.  */
#ifdef __GNUC__
# undef C_ALLOCA
#endif

/* The match routines may not allocate if (1) they would do it with malloc
   and (2) it's not safe for them to use malloc.
   Note that if REL_ALLOC is defined, matching would not use malloc for the
   failure stack, but we would still use it for the register vectors;
   so REL_ALLOC should not affect this.  */
#if (defined C_ALLOCA || defined REGEX_MALLOC) && defined emacs
# undef MATCH_MAY_ALLOCATE
#endif


/* Failure stack declarations and macros; both re_compile_fastmap and
   re_match_2 use a failure stack.  These have to be macros because of
   REGEX_ALLOCATE_STACK.  */


/* Approximate number of failure points for which to initially allocate space
   when matching.  If this number is exceeded, we allocate more
   space, so it is not a hard limit.  */
#ifndef INIT_FAILURE_ALLOC
# define INIT_FAILURE_ALLOC 20
#endif

/* Roughly the maximum number of failure points on the stack.  Would be
   exactly that if always used TYPICAL_FAILURE_SIZE items each time we failed.
   This is a variable only so users of regex can assign to it; we never
   change it ourselves.  We always multiply it by TYPICAL_FAILURE_SIZE
   before using it, so it should probably be a byte-count instead.  */
# if defined MATCH_MAY_ALLOCATE
/* Note that 4400 was enough to cause a crash on Alpha OSF/1,
   whose default stack limit is 2mb.  In order for a larger
   value to work reliably, you have to try to make it accord
   with the process stack limit.  */
size_t re_max_failures = 40000;
# else
size_t re_max_failures = 4000;
# endif

union fail_stack_elt
{
  re_char *pointer;
  /* This should be the biggest `int' that's no bigger than a pointer.  */
  long integer;
};

typedef union fail_stack_elt fail_stack_elt_t;

typedef struct
{
  fail_stack_elt_t *stack;
  size_t size;
  size_t avail;	/* Offset of next open position.  */
  size_t frame;	/* Offset of the cur constructed frame.  */
} fail_stack_type;

#define FAIL_STACK_EMPTY()     (fail_stack.frame == 0)
#define FAIL_STACK_FULL()      (fail_stack.avail == fail_stack.size)


/* Define macros to initialize and free the failure stack.
   Do `return -2' if the alloc fails.  */

#ifdef MATCH_MAY_ALLOCATE
# define INIT_FAIL_STACK()						\
  do {									\
    fail_stack.stack = (fail_stack_elt_t *)				\
      REGEX_ALLOCATE_STACK (INIT_FAILURE_ALLOC * TYPICAL_FAILURE_SIZE	\
			    * sizeof (fail_stack_elt_t));		\
									\
    if (fail_stack.stack == NULL)					\
      return -2;							\
									\
    fail_stack.size = INIT_FAILURE_ALLOC;				\
    fail_stack.avail = 0;						\
    fail_stack.frame = 0;						\
  } while (0)

# define RESET_FAIL_STACK()  REGEX_FREE_STACK (fail_stack.stack)
#else
# define INIT_FAIL_STACK()						\
  do {									\
    fail_stack.avail = 0;						\
    fail_stack.frame = 0;						\
  } while (0)

# define RESET_FAIL_STACK() ((void)0)
#endif


/* Double the size of FAIL_STACK, up to a limit
   which allows approximately `re_max_failures' items.

   Return 1 if succeeds, and 0 if either ran out of memory
   allocating space for it or it was already too large.

   REGEX_REALLOCATE_STACK requires `destination' be declared.   */

/* Factor to increase the failure stack size by
   when we increase it.
   This used to be 2, but 2 was too wasteful
   because the old discarded stacks added up to as much space
   were as ultimate, maximum-size stack.  */
#define FAIL_STACK_GROWTH_FACTOR 4

#define GROW_FAIL_STACK(fail_stack)					\
  (((fail_stack).size * sizeof (fail_stack_elt_t)			\
    >= re_max_failures * TYPICAL_FAILURE_SIZE)				\
   ? 0									\
   : ((fail_stack).stack						\
      = (fail_stack_elt_t *)						\
	REGEX_REALLOCATE_STACK ((fail_stack).stack,			\
	  (fail_stack).size * sizeof (fail_stack_elt_t),		\
	  MIN (re_max_failures * TYPICAL_FAILURE_SIZE,			\
	       ((fail_stack).size * sizeof (fail_stack_elt_t)		\
		* FAIL_STACK_GROWTH_FACTOR))),				\
									\
      (fail_stack).stack == NULL					\
      ? 0								\
      : ((fail_stack).size						\
	 = (MIN (re_max_failures * TYPICAL_FAILURE_SIZE,		\
		 ((fail_stack).size * sizeof (fail_stack_elt_t)		\
		  * FAIL_STACK_GROWTH_FACTOR))				\
	    / sizeof (fail_stack_elt_t)),				\
	 1)))


/* Push a pointer value onto the failure stack.
   Assumes the variable `fail_stack'.  Probably should only
   be called from within `PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_POINTER(item)					\
  fail_stack.stack[fail_stack.avail++].pointer = (item)

/* This pushes an integer-valued item onto the failure stack.
   Assumes the variable `fail_stack'.  Probably should only
   be called from within `PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_INT(item)					\
  fail_stack.stack[fail_stack.avail++].integer = (item)

/* Push a fail_stack_elt_t value onto the failure stack.
   Assumes the variable `fail_stack'.  Probably should only
   be called from within `PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_ELT(item)					\
  fail_stack.stack[fail_stack.avail++] =  (item)

/* These three POP... operations complement the three PUSH... operations.
   All assume that `fail_stack' is nonempty.  */
#define POP_FAILURE_POINTER() fail_stack.stack[--fail_stack.avail].pointer
#define POP_FAILURE_INT() fail_stack.stack[--fail_stack.avail].integer
#define POP_FAILURE_ELT() fail_stack.stack[--fail_stack.avail]

/* Individual items aside from the registers.  */
#define NUM_NONREG_ITEMS 3

/* Used to examine the stack (to detect infinite loops).  */
#define FAILURE_PAT(h) fail_stack.stack[(h) - 1].pointer
#define FAILURE_STR(h) (fail_stack.stack[(h) - 2].pointer)
#define NEXT_FAILURE_HANDLE(h) fail_stack.stack[(h) - 3].integer
#define TOP_FAILURE_HANDLE() fail_stack.frame


#define ENSURE_FAIL_STACK(space)					\
while (REMAINING_AVAIL_SLOTS <= space) {				\
  if (!GROW_FAIL_STACK (fail_stack))					\
    return -2;								\
  DEBUG_PRINT2 ("\n  Doubled stack; size now: %d\n", (fail_stack).size);\
  DEBUG_PRINT2 ("	 slots available: %d\n", REMAINING_AVAIL_SLOTS);\
}

/* Push register NUM onto the stack.  */
#define PUSH_FAILURE_REG(num)						\
do {									\
  char *destination;							\
  ENSURE_FAIL_STACK(3);							\
  DEBUG_PRINT4 ("    Push reg %d (spanning %p -> %p)\n",		\
		num, regstart[num], regend[num]);			\
  PUSH_FAILURE_POINTER (regstart[num]);					\
  PUSH_FAILURE_POINTER (regend[num]);					\
  PUSH_FAILURE_INT (num);						\
} while (0)

/* Change the counter's value to VAL, but make sure that it will
   be reset when backtracking.  */
#define PUSH_NUMBER(ptr,val)						\
do {									\
  char *destination;							\
  int c;								\
  ENSURE_FAIL_STACK(3);							\
  EXTRACT_NUMBER (c, ptr);						\
  DEBUG_PRINT4 ("    Push number %p = %d -> %d\n", ptr, c, val);	\
  PUSH_FAILURE_INT (c);							\
  PUSH_FAILURE_POINTER (ptr);						\
  PUSH_FAILURE_INT (-1);						\
  STORE_NUMBER (ptr, val);						\
} while (0)

/* Pop a saved register off the stack.  */
#define POP_FAILURE_REG_OR_COUNT()					\
do {									\
  int reg = POP_FAILURE_INT ();						\
  if (reg == -1)							\
    {									\
      /* It's a counter.  */						\
      /* Here, we discard `const', making re_match non-reentrant.  */	\
      unsigned char *ptr = (unsigned char*) POP_FAILURE_POINTER ();	\
      reg = POP_FAILURE_INT ();						\
      STORE_NUMBER (ptr, reg);						\
      DEBUG_PRINT3 ("     Pop counter %p = %d\n", ptr, reg);		\
    }									\
  else									\
    {									\
      regend[reg] = POP_FAILURE_POINTER ();				\
      regstart[reg] = POP_FAILURE_POINTER ();				\
      DEBUG_PRINT4 ("     Pop reg %d (spanning %p -> %p)\n",		\
		    reg, regstart[reg], regend[reg]);			\
    }									\
} while (0)

/* Discard a saved register off the stack.  */
#define DISCARD_FAILURE_REG_OR_COUNT()					\
do {									\
  int reg = POP_FAILURE_INT ();						\
  if (reg == -1)							\
    {									\
      /* It's a counter.  */						\
      POP_FAILURE_POINTER ();						\
      reg = POP_FAILURE_INT ();						\
      DEBUG_PRINT3 ("     Discard counter %p = %d\n", ptr, reg);	\
    }									\
  else									\
    {									\
      POP_FAILURE_POINTER ();						\
      POP_FAILURE_POINTER ();						\
      DEBUG_PRINT4 ("     Discard reg %d (spanning %p -> %p)\n",	\
		    reg, regstart[reg], regend[reg]);			\
    }									\
} while (0)

/* Check that we are not stuck in an infinite loop.  */
#define CHECK_INFINITE_LOOP(pat_cur, string_place)			\
do {									\
  int failure = TOP_FAILURE_HANDLE ();					\
  /* Check for infinite matching loops */				\
  while (failure > 0							\
	 && (FAILURE_STR (failure) == string_place			\
	     || FAILURE_STR (failure) == NULL))				\
    {									\
      assert (FAILURE_PAT (failure) >= bufp->buffer			\
	      && FAILURE_PAT (failure) <= bufp->buffer + bufp->used);	\
      if (FAILURE_PAT (failure) == pat_cur)				\
	{								\
	  while (fail_stack.frame < fail_stack.avail)			\
	    DISCARD_FAILURE_REG_OR_COUNT ();				\
	  goto fail;							\
	}								\
      DEBUG_PRINT2 ("  Other pattern: %p\n", FAILURE_PAT (failure));	\
      failure = NEXT_FAILURE_HANDLE(failure);				\
    }									\
  DEBUG_PRINT2 ("  Other string: %p\n", FAILURE_STR (failure));		\
} while (0)
    
/* Push the information about the state we will need
   if we ever fail back to it.

   Requires variables fail_stack, regstart, regend and
   num_regs be declared.  GROW_FAIL_STACK requires `destination' be
   declared.

   Does `return FAILURE_CODE' if runs out of memory.  */

#define PUSH_FAILURE_POINT(pattern, string_place)			\
do {									\
  char *destination;							\
  /* Must be int, so when we don't save any registers, the arithmetic	\
     of 0 + -1 isn't done as unsigned.  */				\
  									\
  DEBUG_STATEMENT (nfailure_points_pushed++);				\
  DEBUG_PRINT1 ("\nPUSH_FAILURE_POINT:\n");				\
  DEBUG_PRINT2 ("  Before push, next avail: %d\n", (fail_stack).avail);	\
  DEBUG_PRINT2 ("			size: %d\n", (fail_stack).size);\
  									\
  ENSURE_FAIL_STACK (NUM_NONREG_ITEMS);					\
  									\
  DEBUG_PRINT1 ("\n");							\
  									\
  DEBUG_PRINT2 ("  Push frame index: %d\n", fail_stack.frame);		\
  PUSH_FAILURE_INT (fail_stack.frame);					\
  									\
  DEBUG_PRINT2 ("  Push string %p: `", string_place);			\
  DEBUG_PRINT_DOUBLE_STRING (string_place, string1, size1, string2, size2);\
  DEBUG_PRINT1 ("'\n");							\
  PUSH_FAILURE_POINTER (string_place);					\
  									\
  DEBUG_PRINT2 ("  Push pattern %p: ", pattern);			\
  DEBUG_PRINT_COMPILED_PATTERN (bufp, pattern, pend);			\
  PUSH_FAILURE_POINTER (pattern);					\
  									\
  /* Close the frame by moving the frame pointer past it.  */		\
  fail_stack.frame = fail_stack.avail;					\
} while (0)

/* Estimate the size of data pushed by a typical failure stack entry.
   An estimate is all we need, because all we use this for
   is to choose a limit for how big to make the failure stack.  */
/* BEWARE, the value `20' is hard-coded in emacs.c:main().  */
#define TYPICAL_FAILURE_SIZE 20

/* How many items can still be added to the stack without overflowing it.  */
#define REMAINING_AVAIL_SLOTS ((fail_stack).size - (fail_stack).avail)


/* Pops what PUSH_FAIL_STACK pushes.

   We restore into the parameters, all of which should be lvalues:
     STR -- the saved data position.
     PAT -- the saved pattern position.
     REGSTART, REGEND -- arrays of string positions.

   Also assumes the variables `fail_stack' and (if debugging), `bufp',
   `pend', `string1', `size1', `string2', and `size2'.	*/

#define POP_FAILURE_POINT(str, pat)                                     \
do {									\
  assert (!FAIL_STACK_EMPTY ());					\
									\
  /* Remove failure points and point to how many regs pushed.  */	\
  DEBUG_PRINT1 ("POP_FAILURE_POINT:\n");				\
  DEBUG_PRINT2 ("  Before pop, next avail: %d\n", fail_stack.avail);	\
  DEBUG_PRINT2 ("		     size: %d\n", fail_stack.size);	\
									\
  /* Pop the saved registers.  */					\
  while (fail_stack.frame < fail_stack.avail)				\
    POP_FAILURE_REG_OR_COUNT ();					\
									\
  pat = POP_FAILURE_POINTER ();				\
  DEBUG_PRINT2 ("  Popping pattern %p: ", pat);				\
  DEBUG_PRINT_COMPILED_PATTERN (bufp, pat, pend);			\
									\
  /* If the saved string location is NULL, it came from an		\
     on_failure_keep_string_jump opcode, and we want to throw away the	\
     saved NULL, thus retaining our current position in the string.  */	\
  str = POP_FAILURE_POINTER ();						\
  DEBUG_PRINT2 ("  Popping string %p: `", str);				\
  DEBUG_PRINT_DOUBLE_STRING (str, string1, size1, string2, size2);	\
  DEBUG_PRINT1 ("'\n");							\
									\
  fail_stack.frame = POP_FAILURE_INT ();				\
  DEBUG_PRINT2 ("  Popping  frame index: %d\n", fail_stack.frame);	\
									\
  assert (fail_stack.avail >= 0);					\
  assert (fail_stack.frame <= fail_stack.avail);			\
									\
  DEBUG_STATEMENT (nfailure_points_popped++);				\
} while (0) /* POP_FAILURE_POINT */



/* Registers are set to a sentinel when they haven't yet matched.  */
#define REG_UNSET(e) ((e) == NULL)

/* Subroutine declarations and macros for regex_compile.  */

static reg_errcode_t regex_compile _RE_ARGS ((re_char *pattern, size_t size,
					      reg_syntax_t syntax,
					      struct re_pattern_buffer *bufp));
static void store_op1 _RE_ARGS ((re_opcode_t op, unsigned char *loc, int arg));
static void store_op2 _RE_ARGS ((re_opcode_t op, unsigned char *loc,
				 int arg1, int arg2));
static void insert_op1 _RE_ARGS ((re_opcode_t op, unsigned char *loc,
				  int arg, unsigned char *end));
static void insert_op2 _RE_ARGS ((re_opcode_t op, unsigned char *loc,
				  int arg1, int arg2, unsigned char *end));
static boolean at_begline_loc_p _RE_ARGS ((re_char *pattern,
					   re_char *p,
					   reg_syntax_t syntax));
static boolean at_endline_loc_p _RE_ARGS ((re_char *p,
					   re_char *pend,
					   reg_syntax_t syntax));
static re_char *skip_one_char _RE_ARGS ((re_char *p));
static int analyse_first _RE_ARGS ((re_char *p, re_char *pend,
				    char *fastmap, const int multibyte));

/* Fetch the next character in the uncompiled pattern---translating it
   if necessary.  */
#define PATFETCH(c)							\
  do {									\
    PATFETCH_RAW (c);							\
    c = TRANSLATE (c);							\
  } while (0)

/* Fetch the next character in the uncompiled pattern, with no
   translation.  */
#define PATFETCH_RAW(c)							\
  do {									\
    int len;								\
    if (p == pend) return REG_EEND;					\
    c = RE_STRING_CHAR_AND_LENGTH (p, pend - p, len);			\
    p += len;								\
  } while (0)


/* If `translate' is non-null, return translate[D], else just D.  We
   cast the subscript to translate because some data is declared as
   `char *', to avoid warnings when a string constant is passed.  But
   when we use a character as a subscript we must make it unsigned.  */
#ifndef TRANSLATE
# define TRANSLATE(d) \
  (RE_TRANSLATE_P (translate) ? RE_TRANSLATE (translate, (d)) : (d))
#endif


/* Macros for outputting the compiled pattern into `buffer'.  */

/* If the buffer isn't allocated when it comes in, use this.  */
#define INIT_BUF_SIZE  32

/* Make sure we have at least N more bytes of space in buffer.  */
#define GET_BUFFER_SPACE(n)						\
    while ((size_t) (b - bufp->buffer + (n)) > bufp->allocated)		\
      EXTEND_BUFFER ()

/* Make sure we have one more byte of buffer space and then add C to it.  */
#define BUF_PUSH(c)							\
  do {									\
    GET_BUFFER_SPACE (1);						\
    *b++ = (unsigned char) (c);						\
  } while (0)


/* Ensure we have two more bytes of buffer space and then append C1 and C2.  */
#define BUF_PUSH_2(c1, c2)						\
  do {									\
    GET_BUFFER_SPACE (2);						\
    *b++ = (unsigned char) (c1);					\
    *b++ = (unsigned char) (c2);					\
  } while (0)


/* As with BUF_PUSH_2, except for three bytes.  */
#define BUF_PUSH_3(c1, c2, c3)						\
  do {									\
    GET_BUFFER_SPACE (3);						\
    *b++ = (unsigned char) (c1);					\
    *b++ = (unsigned char) (c2);					\
    *b++ = (unsigned char) (c3);					\
  } while (0)


/* Store a jump with opcode OP at LOC to location TO.  We store a
   relative address offset by the three bytes the jump itself occupies.  */
#define STORE_JUMP(op, loc, to) \
  store_op1 (op, loc, (to) - (loc) - 3)

/* Likewise, for a two-argument jump.  */
#define STORE_JUMP2(op, loc, to, arg) \
  store_op2 (op, loc, (to) - (loc) - 3, arg)

/* Like `STORE_JUMP', but for inserting.  Assume `b' is the buffer end.  */
#define INSERT_JUMP(op, loc, to) \
  insert_op1 (op, loc, (to) - (loc) - 3, b)

/* Like `STORE_JUMP2', but for inserting.  Assume `b' is the buffer end.  */
#define INSERT_JUMP2(op, loc, to, arg) \
  insert_op2 (op, loc, (to) - (loc) - 3, arg, b)


/* This is not an arbitrary limit: the arguments which represent offsets
   into the pattern are two bytes long.  So if 2^16 bytes turns out to
   be too small, many things would have to change.  */
/* Any other compiler which, like MSC, has allocation limit below 2^16
   bytes will have to use approach similar to what was done below for
   MSC and drop MAX_BUF_SIZE a bit.  Otherwise you may end up
   reallocating to 0 bytes.  Such thing is not going to work too well.
   You have been warned!!  */
#if defined _MSC_VER  && !defined WIN32
/* Microsoft C 16-bit versions limit malloc to approx 65512 bytes.  */
# define MAX_BUF_SIZE  65500L
#else
# define MAX_BUF_SIZE (1L << 16)
#endif

/* Extend the buffer by twice its current size via realloc and
   reset the pointers that pointed into the old block to point to the
   correct places in the new one.  If extending the buffer results in it
   being larger than MAX_BUF_SIZE, then flag memory exhausted.  */
#if __BOUNDED_POINTERS__
# define SET_HIGH_BOUND(P) (__ptrhigh (P) = __ptrlow (P) + bufp->allocated)
# define MOVE_BUFFER_POINTER(P) \
  (__ptrlow (P) += incr, SET_HIGH_BOUND (P), __ptrvalue (P) += incr)
# define ELSE_EXTEND_BUFFER_HIGH_BOUND		\
  else						\
    {						\
      SET_HIGH_BOUND (b);			\
      SET_HIGH_BOUND (begalt);			\
      if (fixup_alt_jump)			\
	SET_HIGH_BOUND (fixup_alt_jump);	\
      if (laststart)				\
	SET_HIGH_BOUND (laststart);		\
      if (pending_exact)			\
	SET_HIGH_BOUND (pending_exact);		\
    }
#else
# define MOVE_BUFFER_POINTER(P) (P) += incr
# define ELSE_EXTEND_BUFFER_HIGH_BOUND
#endif
#define EXTEND_BUFFER()							\
  do {									\
    re_char *old_buffer = bufp->buffer;					\
    if (bufp->allocated == MAX_BUF_SIZE)				\
      return REG_ESIZE;							\
    bufp->allocated <<= 1;						\
    if (bufp->allocated > MAX_BUF_SIZE)					\
      bufp->allocated = MAX_BUF_SIZE;					\
    RETALLOC (bufp->buffer, bufp->allocated, unsigned char);		\
    if (bufp->buffer == NULL)						\
      return REG_ESPACE;						\
    /* If the buffer moved, move all the pointers into it.  */		\
    if (old_buffer != bufp->buffer)					\
      {									\
	int incr = bufp->buffer - old_buffer;				\
	MOVE_BUFFER_POINTER (b);					\
	MOVE_BUFFER_POINTER (begalt);					\
	if (fixup_alt_jump)						\
	  MOVE_BUFFER_POINTER (fixup_alt_jump);				\
	if (laststart)							\
	  MOVE_BUFFER_POINTER (laststart);				\
	if (pending_exact)						\
	  MOVE_BUFFER_POINTER (pending_exact);				\
      }									\
    ELSE_EXTEND_BUFFER_HIGH_BOUND					\
  } while (0)


/* Since we have one byte reserved for the register number argument to
   {start,stop}_memory, the maximum number of groups we can report
   things about is what fits in that byte.  */
#define MAX_REGNUM 255

/* But patterns can have more than `MAX_REGNUM' registers.  We just
   ignore the excess.  */
typedef unsigned regnum_t;


/* Macros for the compile stack.  */

/* Since offsets can go either forwards or backwards, this type needs to
   be able to hold values from -(MAX_BUF_SIZE - 1) to MAX_BUF_SIZE - 1.  */
/* int may be not enough when sizeof(int) == 2.  */
typedef long pattern_offset_t;

typedef struct
{
  pattern_offset_t begalt_offset;
  pattern_offset_t fixup_alt_jump;
  pattern_offset_t laststart_offset;
  regnum_t regnum;
} compile_stack_elt_t;


typedef struct
{
  compile_stack_elt_t *stack;
  unsigned size;
  unsigned avail;			/* Offset of next open position.  */
} compile_stack_type;


#define INIT_COMPILE_STACK_SIZE 32

#define COMPILE_STACK_EMPTY  (compile_stack.avail == 0)
#define COMPILE_STACK_FULL  (compile_stack.avail == compile_stack.size)

/* The next available element.  */
#define COMPILE_STACK_TOP (compile_stack.stack[compile_stack.avail])


/* Structure to manage work area for range table.  */
struct range_table_work_area
{
  int *table;			/* actual work area.  */
  int allocated;		/* allocated size for work area in bytes.  */
  int used;			/* actually used size in words.	 */
  int bits;			/* flag to record character classes */
};

/* Make sure that WORK_AREA can hold more N multibyte characters.  */
#define EXTEND_RANGE_TABLE_WORK_AREA(work_area, n)			  \
  do {									  \
    if (((work_area).used + (n)) * sizeof (int) > (work_area).allocated)  \
      {									  \
	(work_area).allocated += 16 * sizeof (int);			  \
	if ((work_area).table)						  \
	  (work_area).table						  \
	    = (int *) realloc ((work_area).table, (work_area).allocated); \
	else								  \
	  (work_area).table						  \
	    = (int *) malloc ((work_area).allocated);			  \
	if ((work_area).table == 0)					  \
	  FREE_STACK_RETURN (REG_ESPACE);				  \
      }									  \
  } while (0)

#define SET_RANGE_TABLE_WORK_AREA_BIT(work_area, bit)		\
  (work_area).bits |= (bit)

/* Bits used to implement the multibyte-part of the various character classes
   such as [:alnum:] in a charset's range table.  */
#define BIT_WORD	0x1
#define BIT_LOWER	0x2
#define BIT_PUNCT	0x4
#define BIT_SPACE	0x8
#define BIT_UPPER	0x10
#define BIT_MULTIBYTE	0x20

/* Set a range (RANGE_START, RANGE_END) to WORK_AREA.  */
#define SET_RANGE_TABLE_WORK_AREA(work_area, range_start, range_end)	\
  do {									\
    EXTEND_RANGE_TABLE_WORK_AREA ((work_area), 2);			\
    (work_area).table[(work_area).used++] = (range_start);		\
    (work_area).table[(work_area).used++] = (range_end);		\
  } while (0)

/* Free allocated memory for WORK_AREA.	 */
#define FREE_RANGE_TABLE_WORK_AREA(work_area)	\
  do {						\
    if ((work_area).table)			\
      free ((work_area).table);			\
  } while (0)

#define CLEAR_RANGE_TABLE_WORK_USED(work_area) ((work_area).used = 0, (work_area).bits = 0)
#define RANGE_TABLE_WORK_USED(work_area) ((work_area).used)
#define RANGE_TABLE_WORK_BITS(work_area) ((work_area).bits)
#define RANGE_TABLE_WORK_ELT(work_area, i) ((work_area).table[i])


/* Set the bit for character C in a list.  */
#define SET_LIST_BIT(c) (b[((c)) / BYTEWIDTH] |= 1 << ((c) % BYTEWIDTH))


/* Get the next unsigned number in the uncompiled pattern.  */
#define GET_UNSIGNED_NUMBER(num)					\
 do { if (p != pend)							\
     {									\
       PATFETCH (c);							\
       while (c == ' ') PATFETCH (c);					\
       while ('0' <= c && c <= '9')					\
	 {								\
           int prev;							\
	   if (num < 0)							\
	     num = 0;							\
	   prev = num;							\
	   num = num * 10 + c - '0';					\
	   if (num / 10 != prev)					\
	     FREE_STACK_RETURN (REG_BADBR);				\
	   if (p == pend)						\
	     break;							\
	   PATFETCH (c);						\
	 }								\
       while (c == ' ') PATFETCH (c);					\
       }								\
    } while (0)

#if WIDE_CHAR_SUPPORT
/* The GNU C library provides support for user-defined character classes
   and the functions from ISO C amendement 1.  */
# ifdef CHARCLASS_NAME_MAX
#  define CHAR_CLASS_MAX_LENGTH CHARCLASS_NAME_MAX
# else
/* This shouldn't happen but some implementation might still have this
   problem.  Use a reasonable default value.  */
#  define CHAR_CLASS_MAX_LENGTH 256
# endif
typedef wctype_t re_wctype_t;
typedef wchar_t re_wchar_t;
# define re_wctype wctype
# define re_iswctype iswctype
# define re_wctype_to_bit(cc) 0
#else
# define CHAR_CLASS_MAX_LENGTH  9 /* Namely, `multibyte'.  */
# define btowc(c) c

/* Character classes.  */
typedef enum { RECC_ERROR = 0,
	       RECC_ALNUM, RECC_ALPHA, RECC_WORD,
	       RECC_GRAPH, RECC_PRINT,
	       RECC_LOWER, RECC_UPPER,
	       RECC_PUNCT, RECC_CNTRL,
	       RECC_DIGIT, RECC_XDIGIT,
	       RECC_BLANK, RECC_SPACE,
	       RECC_MULTIBYTE, RECC_NONASCII,
	       RECC_ASCII, RECC_UNIBYTE
} re_wctype_t;

typedef int re_wchar_t;

/* Map a string to the char class it names (if any).  */
static re_wctype_t
re_wctype (str)
     re_char *str;
{
  const char *string = str;
  if      (STREQ (string, "alnum"))	return RECC_ALNUM;
  else if (STREQ (string, "alpha"))	return RECC_ALPHA;
  else if (STREQ (string, "word"))	return RECC_WORD;
  else if (STREQ (string, "ascii"))	return RECC_ASCII;
  else if (STREQ (string, "nonascii"))	return RECC_NONASCII;
  else if (STREQ (string, "graph"))	return RECC_GRAPH;
  else if (STREQ (string, "lower"))	return RECC_LOWER;
  else if (STREQ (string, "print"))	return RECC_PRINT;
  else if (STREQ (string, "punct"))	return RECC_PUNCT;
  else if (STREQ (string, "space"))	return RECC_SPACE;
  else if (STREQ (string, "upper"))	return RECC_UPPER;
  else if (STREQ (string, "unibyte"))	return RECC_UNIBYTE;
  else if (STREQ (string, "multibyte"))	return RECC_MULTIBYTE;
  else if (STREQ (string, "digit"))	return RECC_DIGIT;
  else if (STREQ (string, "xdigit"))	return RECC_XDIGIT;
  else if (STREQ (string, "cntrl"))	return RECC_CNTRL;
  else if (STREQ (string, "blank"))	return RECC_BLANK;
  else return 0;
}

/* True iff CH is in the char class CC.  */
static boolean
re_iswctype (ch, cc)
     int ch;
     re_wctype_t cc;
{
  switch (cc)
    {
    case RECC_ALNUM: return ISALNUM (ch);
    case RECC_ALPHA: return ISALPHA (ch);
    case RECC_BLANK: return ISBLANK (ch);
    case RECC_CNTRL: return ISCNTRL (ch);
    case RECC_DIGIT: return ISDIGIT (ch);
    case RECC_GRAPH: return ISGRAPH (ch);
    case RECC_LOWER: return ISLOWER (ch);
    case RECC_PRINT: return ISPRINT (ch);
    case RECC_PUNCT: return ISPUNCT (ch);
    case RECC_SPACE: return ISSPACE (ch);
    case RECC_UPPER: return ISUPPER (ch);
    case RECC_XDIGIT: return ISXDIGIT (ch);
    case RECC_ASCII: return IS_REAL_ASCII (ch);
    case RECC_NONASCII: return !IS_REAL_ASCII (ch);
    case RECC_UNIBYTE: return ISUNIBYTE (ch);
    case RECC_MULTIBYTE: return !ISUNIBYTE (ch);
    case RECC_WORD: return ISWORD (ch);
    case RECC_ERROR: return false;
    default:
      abort();
    }
}

/* Return a bit-pattern to use in the range-table bits to match multibyte
   chars of class CC.  */
static int
re_wctype_to_bit (cc)
     re_wctype_t cc;
{
  switch (cc)
    {
    case RECC_NONASCII: case RECC_PRINT: case RECC_GRAPH:
    case RECC_MULTIBYTE: return BIT_MULTIBYTE;
    case RECC_ALPHA: case RECC_ALNUM: case RECC_WORD: return BIT_WORD;
    case RECC_LOWER: return BIT_LOWER;
    case RECC_UPPER: return BIT_UPPER;
    case RECC_PUNCT: return BIT_PUNCT;
    case RECC_SPACE: return BIT_SPACE;
    case RECC_ASCII: case RECC_DIGIT: case RECC_XDIGIT: case RECC_CNTRL:
    case RECC_BLANK: case RECC_UNIBYTE: case RECC_ERROR: return 0;
    default:
      abort();
    }
}
#endif

/* Explicit quit checking is only used on NTemacs.  */
#if defined WINDOWSNT && defined emacs && defined QUIT
extern int immediate_quit;
# define IMMEDIATE_QUIT_CHECK			\
    do {					\
      if (immediate_quit) QUIT;			\
    } while (0)
#else
# define IMMEDIATE_QUIT_CHECK    ((void)0)
#endif

#ifndef MATCH_MAY_ALLOCATE

/* If we cannot allocate large objects within re_match_2_internal,
   we make the fail stack and register vectors global.
   The fail stack, we grow to the maximum size when a regexp
   is compiled.
   The register vectors, we adjust in size each time we
   compile a regexp, according to the number of registers it needs.  */

static fail_stack_type fail_stack;

/* Size with which the following vectors are currently allocated.
   That is so we can make them bigger as needed,
   but never make them smaller.  */
static int regs_allocated_size;

static re_char **     regstart, **     regend;
static re_char **best_regstart, **best_regend;

/* Make the register vectors big enough for NUM_REGS registers,
   but don't make them smaller.  */

static
regex_grow_registers (num_regs)
     int num_regs;
{
  if (num_regs > regs_allocated_size)
    {
      RETALLOC_IF (regstart,	 num_regs, re_char *);
      RETALLOC_IF (regend,	 num_regs, re_char *);
      RETALLOC_IF (best_regstart, num_regs, re_char *);
      RETALLOC_IF (best_regend,	 num_regs, re_char *);

      regs_allocated_size = num_regs;
    }
}

#endif /* not MATCH_MAY_ALLOCATE */

static boolean group_in_compile_stack _RE_ARGS ((compile_stack_type
						 compile_stack,
						 regnum_t regnum));

/* `regex_compile' compiles PATTERN (of length SIZE) according to SYNTAX.
   Returns one of error codes defined in `regex.h', or zero for success.

   Assumes the `allocated' (and perhaps `buffer') and `translate'
   fields are set in BUFP on entry.

   If it succeeds, results are put in BUFP (if it returns an error, the
   contents of BUFP are undefined):
     `buffer' is the compiled pattern;
     `syntax' is set to SYNTAX;
     `used' is set to the length of the compiled pattern;
     `fastmap_accurate' is zero;
     `re_nsub' is the number of subexpressions in PATTERN;
     `not_bol' and `not_eol' are zero;

   The `fastmap' field is neither examined nor set.  */

/* Insert the `jump' from the end of last alternative to "here".
   The space for the jump has already been allocated. */
#define FIXUP_ALT_JUMP()						\
do {									\
  if (fixup_alt_jump)							\
    STORE_JUMP (jump, fixup_alt_jump, b);				\
} while (0)


/* Return, freeing storage we allocated.  */
#define FREE_STACK_RETURN(value)		\
  do {							\
    FREE_RANGE_TABLE_WORK_AREA (range_table_work);	\
    free (compile_stack.stack);				\
    return value;					\
  } while (0)

static reg_errcode_t
regex_compile (pattern, size, syntax, bufp)
     re_char *pattern;
     size_t size;
     reg_syntax_t syntax;
     struct re_pattern_buffer *bufp;
{
  /* We fetch characters from PATTERN here.  */
  register re_wchar_t c, c1;

  /* A random temporary spot in PATTERN.  */
  re_char *p1;

  /* Points to the end of the buffer, where we should append.  */
  register unsigned char *b;

  /* Keeps track of unclosed groups.  */
  compile_stack_type compile_stack;

  /* Points to the current (ending) position in the pattern.  */
#ifdef AIX
  /* `const' makes AIX compiler fail.  */
  unsigned char *p = pattern;
#else
  re_char *p = pattern;
#endif
  re_char *pend = pattern + size;

  /* How to translate the characters in the pattern.  */
  RE_TRANSLATE_TYPE translate = bufp->translate;

  /* Address of the count-byte of the most recently inserted `exactn'
     command.  This makes it possible to tell if a new exact-match
     character can be added to that command or if the character requires
     a new `exactn' command.  */
  unsigned char *pending_exact = 0;

  /* Address of start of the most recently finished expression.
     This tells, e.g., postfix * where to find the start of its
     operand.  Reset at the beginning of groups and alternatives.  */
  unsigned char *laststart = 0;

  /* Address of beginning of regexp, or inside of last group.  */
  unsigned char *begalt;

  /* Place in the uncompiled pattern (i.e., the {) to
     which to go back if the interval is invalid.  */
  re_char *beg_interval;

  /* Address of the place where a forward jump should go to the end of
     the containing expression.	 Each alternative of an `or' -- except the
     last -- ends with a forward jump of this sort.  */
  unsigned char *fixup_alt_jump = 0;

  /* Counts open-groups as they are encountered.  Remembered for the
     matching close-group on the compile stack, so the same register
     number is put in the stop_memory as the start_memory.  */
  regnum_t regnum = 0;

  /* Work area for range table of charset.  */
  struct range_table_work_area range_table_work;

  /* If the object matched can contain multibyte characters.  */
  const boolean multibyte = RE_MULTIBYTE_P (bufp);

#ifdef DEBUG
  debug++;
  DEBUG_PRINT1 ("\nCompiling pattern: ");
  if (debug > 0)
    {
      unsigned debug_count;

      for (debug_count = 0; debug_count < size; debug_count++)
	putchar (pattern[debug_count]);
      putchar ('\n');
    }
#endif /* DEBUG */

  /* Initialize the compile stack.  */
  compile_stack.stack = TALLOC (INIT_COMPILE_STACK_SIZE, compile_stack_elt_t);
  if (compile_stack.stack == NULL)
    return REG_ESPACE;

  compile_stack.size = INIT_COMPILE_STACK_SIZE;
  compile_stack.avail = 0;

  range_table_work.table = 0;
  range_table_work.allocated = 0;

  /* Initialize the pattern buffer.  */
  bufp->syntax = syntax;
  bufp->fastmap_accurate = 0;
  bufp->not_bol = bufp->not_eol = 0;

  /* Set `used' to zero, so that if we return an error, the pattern
     printer (for debugging) will think there's no pattern.  We reset it
     at the end.  */
  bufp->used = 0;

  /* Always count groups, whether or not bufp->no_sub is set.  */
  bufp->re_nsub = 0;

#if !defined emacs && !defined SYNTAX_TABLE
  /* Initialize the syntax table.  */
   init_syntax_once ();
#endif

  if (bufp->allocated == 0)
    {
      if (bufp->buffer)
	{ /* If zero allocated, but buffer is non-null, try to realloc
	     enough space.  This loses if buffer's address is bogus, but
	     that is the user's responsibility.	 */
	  RETALLOC (bufp->buffer, INIT_BUF_SIZE, unsigned char);
	}
      else
	{ /* Caller did not allocate a buffer.	Do it for them.	 */
	  bufp->buffer = TALLOC (INIT_BUF_SIZE, unsigned char);
	}
      if (!bufp->buffer) FREE_STACK_RETURN (REG_ESPACE);

      bufp->allocated = INIT_BUF_SIZE;
    }

  begalt = b = bufp->buffer;

  /* Loop through the uncompiled pattern until we're at the end.  */
  while (p != pend)
    {
      PATFETCH (c);

      switch (c)
	{
	case '^':
	  {
	    if (   /* If at start of pattern, it's an operator.	 */
		   p == pattern + 1
		   /* If context independent, it's an operator.	 */
		|| syntax & RE_CONTEXT_INDEP_ANCHORS
		   /* Otherwise, depends on what's come before.	 */
		|| at_begline_loc_p (pattern, p, syntax))
	      BUF_PUSH ((syntax & RE_NO_NEWLINE_ANCHOR) ? begbuf : begline);
	    else
	      goto normal_char;
	  }
	  break;


	case '$':
	  {
	    if (   /* If at end of pattern, it's an operator.  */
		   p == pend
		   /* If context independent, it's an operator.	 */
		|| syntax & RE_CONTEXT_INDEP_ANCHORS
		   /* Otherwise, depends on what's next.  */
		|| at_endline_loc_p (p, pend, syntax))
	       BUF_PUSH ((syntax & RE_NO_NEWLINE_ANCHOR) ? endbuf : endline);
	     else
	       goto normal_char;
	   }
	   break;


	case '+':
	case '?':
	  if ((syntax & RE_BK_PLUS_QM)
	      || (syntax & RE_LIMITED_OPS))
	    goto normal_char;
	handle_plus:
	case '*':
	  /* If there is no previous pattern... */
	  if (!laststart)
	    {
	      if (syntax & RE_CONTEXT_INVALID_OPS)
		FREE_STACK_RETURN (REG_BADRPT);
	      else if (!(syntax & RE_CONTEXT_INDEP_OPS))
		goto normal_char;
	    }

	  {
	    /* 1 means zero (many) matches is allowed.	*/
	    boolean zero_times_ok = 0, many_times_ok = 0;
	    boolean greedy = 1;

	    /* If there is a sequence of repetition chars, collapse it
	       down to just one (the right one).  We can't combine
	       interval operators with these because of, e.g., `a{2}*',
	       which should only match an even number of `a's.	*/

	    for (;;)
	      {
		if ((syntax & RE_FRUGAL)
		    && c == '?' && (zero_times_ok || many_times_ok))
		  greedy = 0;
		else
		  {
		    zero_times_ok |= c != '+';
		    many_times_ok |= c != '?';
		  }

		if (p == pend)
		  break;
		else if (*p == '*'
			 || (!(syntax & RE_BK_PLUS_QM)
			     && (*p == '+' || *p == '?')))
		  ;
		else if (syntax & RE_BK_PLUS_QM	 && *p == '\\')
		  {
		    if (p+1 == pend)
		      FREE_STACK_RETURN (REG_EESCAPE);
		    if (p[1] == '+' || p[1] == '?')
		      PATFETCH (c); /* Gobble up the backslash.  */
		    else
		      break;
		  }
		else
		  break;
		/* If we get here, we found another repeat character.  */
		PATFETCH (c);
	       }

	    /* Star, etc. applied to an empty pattern is equivalent
	       to an empty pattern.  */
	    if (!laststart || laststart == b)
	      break;

	    /* Now we know whether or not zero matches is allowed
	       and also whether or not two or more matches is allowed.	*/
	    if (greedy)
	      {
		if (many_times_ok)
		  {
		    boolean simple = skip_one_char (laststart) == b;
		    unsigned int startoffset = 0;
		    re_opcode_t ofj =
		      /* Check if the loop can match the empty string.  */
		      (simple || !analyse_first (laststart, b, NULL, 0)) ?
		      on_failure_jump : on_failure_jump_loop;
		    assert (skip_one_char (laststart) <= b);
		    
		    if (!zero_times_ok && simple)
		      { /* Since simple * loops can be made faster by using
		    	   on_failure_keep_string_jump, we turn simple P+
		    	   into PP* if P is simple.  */
		    	unsigned char *p1, *p2;
		    	startoffset = b - laststart;
		    	GET_BUFFER_SPACE (startoffset);
		    	p1 = b; p2 = laststart;
		    	while (p2 < p1)
		    	  *b++ = *p2++;
		    	zero_times_ok = 1;
		      }

		    GET_BUFFER_SPACE (6);
		    if (!zero_times_ok)
		      /* A + loop.  */
		      STORE_JUMP (ofj, b, b + 6);
		    else
		      /* Simple * loops can use on_failure_keep_string_jump
			 depending on what follows.  But since we don't know
			 that yet, we leave the decision up to
			 on_failure_jump_smart.  */
		      INSERT_JUMP (simple ? on_failure_jump_smart : ofj,
				   laststart + startoffset, b + 6);
		    b += 3;
		    STORE_JUMP (jump, b, laststart + startoffset);
		    b += 3;
		  }
		else
		  {
		    /* A simple ? pattern.  */
		    assert (zero_times_ok);
		    GET_BUFFER_SPACE (3);
		    INSERT_JUMP (on_failure_jump, laststart, b + 3);
		    b += 3;
		  }
	      }
	    else		/* not greedy */
	      { /* I wish the greedy and non-greedy cases could be merged. */

		GET_BUFFER_SPACE (7); /* We might use less.  */
		if (many_times_ok)
		  {
		    boolean emptyp = analyse_first (laststart, b, NULL, 0);

		    /* The non-greedy multiple match looks like a repeat..until:
		       we only need a conditional jump at the end of the loop */
		    if (emptyp) BUF_PUSH (no_op);
		    STORE_JUMP (emptyp ? on_failure_jump_nastyloop
				: on_failure_jump, b, laststart);
		    b += 3;
		    if (zero_times_ok)
		      {
			/* The repeat...until naturally matches one or more.
			   To also match zero times, we need to first jump to
			   the end of the loop (its conditional jump). */
			INSERT_JUMP (jump, laststart, b);
			b += 3;
		      }
		  }
		else
		  {
		    /* non-greedy a?? */
		    INSERT_JUMP (jump, laststart, b + 3);
		    b += 3;
		    INSERT_JUMP (on_failure_jump, laststart, laststart + 6);
		    b += 3;
		  }
	      }
	  }
	  pending_exact = 0;
	  break;


	case '.':
	  laststart = b;
	  BUF_PUSH (anychar);
	  break;


	case '[':
	  {
	    CLEAR_RANGE_TABLE_WORK_USED (range_table_work);

	    if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

	    /* Ensure that we have enough space to push a charset: the
	       opcode, the length count, and the bitset; 34 bytes in all.  */
	    GET_BUFFER_SPACE (34);

	    laststart = b;

	    /* We test `*p == '^' twice, instead of using an if
	       statement, so we only need one BUF_PUSH.	 */
	    BUF_PUSH (*p == '^' ? charset_not : charset);
	    if (*p == '^')
	      p++;

	    /* Remember the first position in the bracket expression.  */
	    p1 = p;

	    /* Push the number of bytes in the bitmap.	*/
	    BUF_PUSH ((1 << BYTEWIDTH) / BYTEWIDTH);

	    /* Clear the whole map.  */
	    bzero (b, (1 << BYTEWIDTH) / BYTEWIDTH);

	    /* charset_not matches newline according to a syntax bit.  */
	    if ((re_opcode_t) b[-2] == charset_not
		&& (syntax & RE_HAT_LISTS_NOT_NEWLINE))
	      SET_LIST_BIT ('\n');

	    /* Read in characters and ranges, setting map bits.	 */
	    for (;;)
	      {
		boolean escaped_char = false;
		const unsigned char *p2 = p;

		if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

		PATFETCH (c);

		/* \ might escape characters inside [...] and [^...].  */
		if ((syntax & RE_BACKSLASH_ESCAPE_IN_LISTS) && c == '\\')
		  {
		    if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

		    PATFETCH (c);
		    escaped_char = true;
		  }
		else
		  {
		    /* Could be the end of the bracket expression.	If it's
		       not (i.e., when the bracket expression is `[]' so
		       far), the ']' character bit gets set way below.  */
		    if (c == ']' && p2 != p1)
		      break;
		  }

		/* What should we do for the character which is
		   greater than 0x7F, but not BASE_LEADING_CODE_P?
		   XXX */

		/* See if we're at the beginning of a possible character
		   class.  */

		if (!escaped_char &&
		    syntax & RE_CHAR_CLASSES && c == '[' && *p == ':')
		  {
		    /* Leave room for the null.	 */
		    unsigned char str[CHAR_CLASS_MAX_LENGTH + 1];
		    const unsigned char *class_beg;

		    PATFETCH (c);
		    c1 = 0;
		    class_beg = p;

		    /* If pattern is `[[:'.  */
		    if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

		    for (;;)
		      {
		        PATFETCH (c);
		        if ((c == ':' && *p == ']') || p == pend)
		          break;
			if (c1 < CHAR_CLASS_MAX_LENGTH)
			  str[c1++] = c;
			else
			  /* This is in any case an invalid class name.  */
			  str[0] = '\0';
		      }
		    str[c1] = '\0';

		    /* If isn't a word bracketed by `[:' and `:]':
		       undo the ending character, the letters, and
		       leave the leading `:' and `[' (but set bits for
		       them).  */
		    if (c == ':' && *p == ']')
		      {
			int ch;
			re_wctype_t cc;

			cc = re_wctype (str);

			if (cc == 0)
			  FREE_STACK_RETURN (REG_ECTYPE);

                        /* Throw away the ] at the end of the character
                           class.  */
                        PATFETCH (c);

                        if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

			/* Most character classes in a multibyte match
			   just set a flag.  Exceptions are is_blank,
			   is_digit, is_cntrl, and is_xdigit, since
			   they can only match ASCII characters.  We
			   don't need to handle them for multibyte.
			   They are distinguished by a negative wctype.  */

			if (multibyte)
			  SET_RANGE_TABLE_WORK_AREA_BIT (range_table_work,
							 re_wctype_to_bit (cc));

                        for (ch = 0; ch < 1 << BYTEWIDTH; ++ch)
			  {
			    int translated = TRANSLATE (ch);
			    if (re_iswctype (btowc (ch), cc))
			      SET_LIST_BIT (translated);
			  }

			/* Repeat the loop. */
			continue;
		      }
		    else
		      {
			/* Go back to right after the "[:".  */
			p = class_beg;
			SET_LIST_BIT ('[');

			/* Because the `:' may starts the range, we
			   can't simply set bit and repeat the loop.
			   Instead, just set it to C and handle below.	*/
			c = ':';
		      }
		  }

		if (p < pend && p[0] == '-' && p[1] != ']')
		  {

		    /* Discard the `-'. */
		    PATFETCH (c1);

		    /* Fetch the character which ends the range. */
		    PATFETCH (c1);

		    if (SINGLE_BYTE_CHAR_P (c))
		      {
			if (! SINGLE_BYTE_CHAR_P (c1))
			  {
			    /* Handle a range starting with a
			       character of less than 256, and ending
			       with a character of not less than 256.
			       Split that into two ranges, the low one
			       ending at 0377, and the high one
			       starting at the smallest character in
			       the charset of C1 and ending at C1.  */
			    int charset = CHAR_CHARSET (c1);
			    int c2 = MAKE_CHAR (charset, 0, 0);
			    
			    SET_RANGE_TABLE_WORK_AREA (range_table_work,
						       c2, c1);
			    c1 = 0377;
			  }
		      }
		    else if (!SAME_CHARSET_P (c, c1))
		      FREE_STACK_RETURN (REG_ERANGE);
		  }
		else
		  /* Range from C to C. */
		  c1 = c;

		/* Set the range ... */
		if (SINGLE_BYTE_CHAR_P (c))
		  /* ... into bitmap.  */
		  {
		    re_wchar_t this_char;
		    int range_start = c, range_end = c1;

		    /* If the start is after the end, the range is empty.  */
		    if (range_start > range_end)
		      {
			if (syntax & RE_NO_EMPTY_RANGES)
			  FREE_STACK_RETURN (REG_ERANGE);
			/* Else, repeat the loop.  */
		      }
		    else
		      {
			for (this_char = range_start; this_char <= range_end;
			     this_char++)
			  SET_LIST_BIT (TRANSLATE (this_char));
		      }
		  }
		else
		  /* ... into range table.  */
		  SET_RANGE_TABLE_WORK_AREA (range_table_work, c, c1);
	      }

	    /* Discard any (non)matching list bytes that are all 0 at the
	       end of the map.	Decrease the map-length byte too.  */
	    while ((int) b[-1] > 0 && b[b[-1] - 1] == 0)
	      b[-1]--;
	    b += b[-1];

	    /* Build real range table from work area.  */
	    if (RANGE_TABLE_WORK_USED (range_table_work)
		|| RANGE_TABLE_WORK_BITS (range_table_work))
	      {
		int i;
		int used = RANGE_TABLE_WORK_USED (range_table_work);

		/* Allocate space for COUNT + RANGE_TABLE.  Needs two
		   bytes for flags, two for COUNT, and three bytes for
		   each character. */
		GET_BUFFER_SPACE (4 + used * 3);

		/* Indicate the existence of range table.  */
		laststart[1] |= 0x80;

		/* Store the character class flag bits into the range table.
		   If not in emacs, these flag bits are always 0.  */
		*b++ = RANGE_TABLE_WORK_BITS (range_table_work) & 0xff;
		*b++ = RANGE_TABLE_WORK_BITS (range_table_work) >> 8;

		STORE_NUMBER_AND_INCR (b, used / 2);
		for (i = 0; i < used; i++)
		  STORE_CHARACTER_AND_INCR
		    (b, RANGE_TABLE_WORK_ELT (range_table_work, i));
	      }
	  }
	  break;


	case '(':
	  if (syntax & RE_NO_BK_PARENS)
	    goto handle_open;
	  else
	    goto normal_char;


	case ')':
	  if (syntax & RE_NO_BK_PARENS)
	    goto handle_close;
	  else
	    goto normal_char;


	case '\n':
	  if (syntax & RE_NEWLINE_ALT)
	    goto handle_alt;
	  else
	    goto normal_char;


	case '|':
	  if (syntax & RE_NO_BK_VBAR)
	    goto handle_alt;
	  else
	    goto normal_char;


	case '{':
	   if (syntax & RE_INTERVALS && syntax & RE_NO_BK_BRACES)
	     goto handle_interval;
	   else
	     goto normal_char;


	case '\\':
	  if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

	  /* Do not translate the character after the \, so that we can
	     distinguish, e.g., \B from \b, even if we normally would
	     translate, e.g., B to b.  */
	  PATFETCH_RAW (c);

	  switch (c)
	    {
	    case '(':
	      if (syntax & RE_NO_BK_PARENS)
		goto normal_backslash;

	    handle_open:
	      {
		int shy = 0;
		if (p+1 < pend)
		  {
		    /* Look for a special (?...) construct */
		    if ((syntax & RE_SHY_GROUPS) && *p == '?')
		      {
			PATFETCH (c); /* Gobble up the '?'.  */
			PATFETCH (c);
			switch (c)
			  {
			  case ':': shy = 1; break;
			  default:
			    /* Only (?:...) is supported right now. */
			    FREE_STACK_RETURN (REG_BADPAT);
			  }
		      }
		  }

		if (!shy)
		  {
		    bufp->re_nsub++;
		    regnum++;
		  }

		if (COMPILE_STACK_FULL)
		  {
		    RETALLOC (compile_stack.stack, compile_stack.size << 1,
			      compile_stack_elt_t);
		    if (compile_stack.stack == NULL) return REG_ESPACE;

		    compile_stack.size <<= 1;
		  }

		/* These are the values to restore when we hit end of this
		   group.	 They are all relative offsets, so that if the
		   whole pattern moves because of realloc, they will still
		   be valid.  */
		COMPILE_STACK_TOP.begalt_offset = begalt - bufp->buffer;
		COMPILE_STACK_TOP.fixup_alt_jump
		  = fixup_alt_jump ? fixup_alt_jump - bufp->buffer + 1 : 0;
		COMPILE_STACK_TOP.laststart_offset = b - bufp->buffer;
		COMPILE_STACK_TOP.regnum = shy ? -regnum : regnum;

		/* Do not push a
		   start_memory for groups beyond the last one we can
		   represent in the compiled pattern.  */
		if (regnum <= MAX_REGNUM && !shy)
		  BUF_PUSH_2 (start_memory, regnum);

		compile_stack.avail++;

		fixup_alt_jump = 0;
		laststart = 0;
		begalt = b;
		/* If we've reached MAX_REGNUM groups, then this open
		   won't actually generate any code, so we'll have to
		   clear pending_exact explicitly.  */
		pending_exact = 0;
		break;
	      }

	    case ')':
	      if (syntax & RE_NO_BK_PARENS) goto normal_backslash;

	      if (COMPILE_STACK_EMPTY)
		{
		  if (syntax & RE_UNMATCHED_RIGHT_PAREN_ORD)
		    goto normal_backslash;
		  else
		    FREE_STACK_RETURN (REG_ERPAREN);
		}

	    handle_close:
	      FIXUP_ALT_JUMP ();

	      /* See similar code for backslashed left paren above.  */
	      if (COMPILE_STACK_EMPTY)
		{
		  if (syntax & RE_UNMATCHED_RIGHT_PAREN_ORD)
		    goto normal_char;
		  else
		    FREE_STACK_RETURN (REG_ERPAREN);
		}

	      /* Since we just checked for an empty stack above, this
		 ``can't happen''.  */
	      assert (compile_stack.avail != 0);
	      {
		/* We don't just want to restore into `regnum', because
		   later groups should continue to be numbered higher,
		   as in `(ab)c(de)' -- the second group is #2.	 */
		regnum_t this_group_regnum;

		compile_stack.avail--;
		begalt = bufp->buffer + COMPILE_STACK_TOP.begalt_offset;
		fixup_alt_jump
		  = COMPILE_STACK_TOP.fixup_alt_jump
		    ? bufp->buffer + COMPILE_STACK_TOP.fixup_alt_jump - 1
		    : 0;
		laststart = bufp->buffer + COMPILE_STACK_TOP.laststart_offset;
		this_group_regnum = COMPILE_STACK_TOP.regnum;
		/* If we've reached MAX_REGNUM groups, then this open
		   won't actually generate any code, so we'll have to
		   clear pending_exact explicitly.  */
		pending_exact = 0;

		/* We're at the end of the group, so now we know how many
		   groups were inside this one.	 */
		if (this_group_regnum <= MAX_REGNUM && this_group_regnum > 0)
		  BUF_PUSH_2 (stop_memory, this_group_regnum);
	      }
	      break;


	    case '|':					/* `\|'.  */
	      if (syntax & RE_LIMITED_OPS || syntax & RE_NO_BK_VBAR)
		goto normal_backslash;
	    handle_alt:
	      if (syntax & RE_LIMITED_OPS)
		goto normal_char;

	      /* Insert before the previous alternative a jump which
		 jumps to this alternative if the former fails.	 */
	      GET_BUFFER_SPACE (3);
	      INSERT_JUMP (on_failure_jump, begalt, b + 6);
	      pending_exact = 0;
	      b += 3;

	      /* The alternative before this one has a jump after it
		 which gets executed if it gets matched.  Adjust that
		 jump so it will jump to this alternative's analogous
		 jump (put in below, which in turn will jump to the next
		 (if any) alternative's such jump, etc.).  The last such
		 jump jumps to the correct final destination.  A picture:
			  _____ _____
			  |   | |   |
			  |   v |   v
			 a | b	 | c

		 If we are at `b', then fixup_alt_jump right now points to a
		 three-byte space after `a'.  We'll put in the jump, set
		 fixup_alt_jump to right after `b', and leave behind three
		 bytes which we'll fill in when we get to after `c'.  */

	      FIXUP_ALT_JUMP ();

	      /* Mark and leave space for a jump after this alternative,
		 to be filled in later either by next alternative or
		 when know we're at the end of a series of alternatives.  */
	      fixup_alt_jump = b;
	      GET_BUFFER_SPACE (3);
	      b += 3;

	      laststart = 0;
	      begalt = b;
	      break;


	    case '{':
	      /* If \{ is a literal.  */
	      if (!(syntax & RE_INTERVALS)
		     /* If we're at `\{' and it's not the open-interval
			operator.  */
		  || (syntax & RE_NO_BK_BRACES))
		goto normal_backslash;

	    handle_interval:
	      {
		/* If got here, then the syntax allows intervals.  */

		/* At least (most) this many matches must be made.  */
		int lower_bound = 0, upper_bound = -1;

		beg_interval = p;

		if (p == pend)
		  FREE_STACK_RETURN (REG_EBRACE);

		GET_UNSIGNED_NUMBER (lower_bound);

		if (c == ',')
		  GET_UNSIGNED_NUMBER (upper_bound);
		else
		  /* Interval such as `{1}' => match exactly once. */
		  upper_bound = lower_bound;

		if (lower_bound < 0 || upper_bound > RE_DUP_MAX
		    || (upper_bound >= 0 && lower_bound > upper_bound))
		  FREE_STACK_RETURN (REG_BADBR);

		if (!(syntax & RE_NO_BK_BRACES))
		  {
		    if (c != '\\')
		      FREE_STACK_RETURN (REG_BADBR);

		    PATFETCH (c);
		  }

		if (c != '}')
		  FREE_STACK_RETURN (REG_BADBR);

		/* We just parsed a valid interval.  */

		/* If it's invalid to have no preceding re.  */
		if (!laststart)
		  {
		    if (syntax & RE_CONTEXT_INVALID_OPS)
		      FREE_STACK_RETURN (REG_BADRPT);
		    else if (syntax & RE_CONTEXT_INDEP_OPS)
		      laststart = b;
		    else
		      goto unfetch_interval;
		  }

		 if (upper_bound == 0)
		   /* If the upper bound is zero, just drop the sub pattern
		      altogether.  */
		   b = laststart;
		 else if (lower_bound == 1 && upper_bound == 1)
		   /* Just match it once: nothing to do here.  */
		   ;

		 /* Otherwise, we have a nontrivial interval.  When
		    we're all done, the pattern will look like:
		      set_number_at <jump count> <upper bound>
		      set_number_at <succeed_n count> <lower bound>
		      succeed_n <after jump addr> <succeed_n count>
		      <body of loop>
		      jump_n <succeed_n addr> <jump count>
		    (The upper bound and `jump_n' are omitted if
		    `upper_bound' is 1, though.)  */
		 else
		   { /* If the upper bound is > 1, we need to insert
			more at the end of the loop.  */
		     unsigned int nbytes = (upper_bound < 0 ? 3
					    : upper_bound > 1 ? 5 : 0);
		     unsigned int startoffset = 0;

		     GET_BUFFER_SPACE (20); /* We might use less.  */

		     if (lower_bound == 0)
		       {
			 /* A succeed_n that starts with 0 is really a
			    a simple on_failure_jump_loop.  */
			 INSERT_JUMP (on_failure_jump_loop, laststart,
				      b + 3 + nbytes);
			 b += 3;
		       }
		     else
		       {
			 /* Initialize lower bound of the `succeed_n', even
			    though it will be set during matching by its
			    attendant `set_number_at' (inserted next),
			    because `re_compile_fastmap' needs to know.
			    Jump to the `jump_n' we might insert below.  */
			 INSERT_JUMP2 (succeed_n, laststart,
				       b + 5 + nbytes,
				       lower_bound);
			 b += 5;

			 /* Code to initialize the lower bound.  Insert
			    before the `succeed_n'.	 The `5' is the last two
			    bytes of this `set_number_at', plus 3 bytes of
			    the following `succeed_n'.  */
			 insert_op2 (set_number_at, laststart, 5, lower_bound, b);
			 b += 5;
			 startoffset += 5;
		       }

		     if (upper_bound < 0)
		       {
			 /* A negative upper bound stands for infinity,
			    in which case it degenerates to a plain jump.  */
			 STORE_JUMP (jump, b, laststart + startoffset);
			 b += 3;
		       }
		     else if (upper_bound > 1)
		       { /* More than one repetition is allowed, so
			    append a backward jump to the `succeed_n'
			    that starts this interval.

			    When we've reached this during matching,
			    we'll have matched the interval once, so
			    jump back only `upper_bound - 1' times.  */
			 STORE_JUMP2 (jump_n, b, laststart + startoffset,
				      upper_bound - 1);
			 b += 5;

			 /* The location we want to set is the second
			    parameter of the `jump_n'; that is `b-2' as
			    an absolute address.  `laststart' will be
			    the `set_number_at' we're about to insert;
			    `laststart+3' the number to set, the source
			    for the relative address.  But we are
			    inserting into the middle of the pattern --
			    so everything is getting moved up by 5.
			    Conclusion: (b - 2) - (laststart + 3) + 5,
			    i.e., b - laststart.

			    We insert this at the beginning of the loop
			    so that if we fail during matching, we'll
			    reinitialize the bounds.  */
			 insert_op2 (set_number_at, laststart, b - laststart,
				     upper_bound - 1, b);
			 b += 5;
		       }
		   }
		pending_exact = 0;
		beg_interval = NULL;
	      }
	      break;

	    unfetch_interval:
	      /* If an invalid interval, match the characters as literals.  */
	       assert (beg_interval);
	       p = beg_interval;
	       beg_interval = NULL;

	       /* normal_char and normal_backslash need `c'.  */
	       c = '{';

	       if (!(syntax & RE_NO_BK_BRACES))
		 {
		   assert (p > pattern && p[-1] == '\\');
		   goto normal_backslash;
		 }
	       else
		 goto normal_char;

#ifdef emacs
	    /* There is no way to specify the before_dot and after_dot
	       operators.  rms says this is ok.	 --karl	 */
	    case '=':
	      BUF_PUSH (at_dot);
	      break;

	    case 's':
	      laststart = b;
	      PATFETCH (c);
	      BUF_PUSH_2 (syntaxspec, syntax_spec_code[c]);
	      break;

	    case 'S':
	      laststart = b;
	      PATFETCH (c);
	      BUF_PUSH_2 (notsyntaxspec, syntax_spec_code[c]);
	      break;

	    case 'c':
	      laststart = b;
	      PATFETCH_RAW (c);
	      BUF_PUSH_2 (categoryspec, c);
	      break;

	    case 'C':
	      laststart = b;
	      PATFETCH_RAW (c);
	      BUF_PUSH_2 (notcategoryspec, c);
	      break;
#endif /* emacs */


	    case 'w':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      laststart = b;
	      BUF_PUSH_2 (syntaxspec, Sword);
	      break;


	    case 'W':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      laststart = b;
	      BUF_PUSH_2 (notsyntaxspec, Sword);
	      break;


	    case '<':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      BUF_PUSH (wordbeg);
	      break;

	    case '>':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      BUF_PUSH (wordend);
	      break;

	    case 'b':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      BUF_PUSH (wordbound);
	      break;

	    case 'B':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      BUF_PUSH (notwordbound);
	      break;

	    case '`':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      BUF_PUSH (begbuf);
	      break;

	    case '\'':
	      if (syntax & RE_NO_GNU_OPS)
		goto normal_char;
	      BUF_PUSH (endbuf);
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      {
		regnum_t reg;

		if (syntax & RE_NO_BK_REFS)
		  goto normal_backslash;

		reg = c - '0';

		/* Can't back reference to a subexpression before its end.  */
		if (reg > regnum || group_in_compile_stack (compile_stack, reg))
		  FREE_STACK_RETURN (REG_ESUBREG);

		laststart = b;
		BUF_PUSH_2 (duplicate, reg);
	      }
	      break;


	    case '+':
	    case '?':
	      if (syntax & RE_BK_PLUS_QM)
		goto handle_plus;
	      else
		goto normal_backslash;

	    default:
	    normal_backslash:
	      /* You might think it would be useful for \ to mean
		 not to translate; but if we don't translate it
		 it will never match anything.  */
	      c = TRANSLATE (c);
	      goto normal_char;
	    }
	  break;


	default:
	/* Expects the character in `c'.  */
	normal_char:
	      /* If no exactn currently being built.  */
	  if (!pending_exact

	      /* If last exactn not at current position.  */
	      || pending_exact + *pending_exact + 1 != b

	      /* We have only one byte following the exactn for the count.  */
	      || *pending_exact >= (1 << BYTEWIDTH) - MAX_MULTIBYTE_LENGTH

	      /* If followed by a repetition operator.	*/
	      || (p != pend && (*p == '*' || *p == '^'))
	      || ((syntax & RE_BK_PLUS_QM)
		  ? p + 1 < pend && *p == '\\' && (p[1] == '+' || p[1] == '?')
		  : p != pend && (*p == '+' || *p == '?'))
	      || ((syntax & RE_INTERVALS)
		  && ((syntax & RE_NO_BK_BRACES)
		      ? p != pend && *p == '{'
		      : p + 1 < pend && p[0] == '\\' && p[1] == '{')))
	    {
	      /* Start building a new exactn.  */

	      laststart = b;

	      BUF_PUSH_2 (exactn, 0);
	      pending_exact = b - 1;
	    }

	  GET_BUFFER_SPACE (MAX_MULTIBYTE_LENGTH);
	  {
	    int len;

	    if (multibyte)
	      len = CHAR_STRING (c, b);
	    else
	      *b = c, len = 1;
	    b += len;
	    (*pending_exact) += len;
	  }

	  break;
	} /* switch (c) */
    } /* while p != pend */


  /* Through the pattern now.  */

  FIXUP_ALT_JUMP ();

  if (!COMPILE_STACK_EMPTY)
    FREE_STACK_RETURN (REG_EPAREN);

  /* If we don't want backtracking, force success
     the first time we reach the end of the compiled pattern.  */
  if (syntax & RE_NO_POSIX_BACKTRACKING)
    BUF_PUSH (succeed);

  free (compile_stack.stack);

  /* We have succeeded; set the length of the buffer.  */
  bufp->used = b - bufp->buffer;

#ifdef DEBUG
  if (debug > 0)
    {
      re_compile_fastmap (bufp);
      DEBUG_PRINT1 ("\nCompiled pattern: \n");
      print_compiled_pattern (bufp);
    }
  debug--;
#endif /* DEBUG */

#ifndef MATCH_MAY_ALLOCATE
  /* Initialize the failure stack to the largest possible stack.  This
     isn't necessary unless we're trying to avoid calling alloca in
     the search and match routines.  */
  {
    int num_regs = bufp->re_nsub + 1;

    if (fail_stack.size < re_max_failures * TYPICAL_FAILURE_SIZE)
      {
	fail_stack.size = re_max_failures * TYPICAL_FAILURE_SIZE;

	if (! fail_stack.stack)
	  fail_stack.stack
	    = (fail_stack_elt_t *) malloc (fail_stack.size
					   * sizeof (fail_stack_elt_t));
	else
	  fail_stack.stack
	    = (fail_stack_elt_t *) realloc (fail_stack.stack,
					    (fail_stack.size
					     * sizeof (fail_stack_elt_t)));
      }

    regex_grow_registers (num_regs);
  }
#endif /* not MATCH_MAY_ALLOCATE */

  return REG_NOERROR;
} /* regex_compile */

/* Subroutines for `regex_compile'.  */

/* Store OP at LOC followed by two-byte integer parameter ARG.	*/

static void
store_op1 (op, loc, arg)
    re_opcode_t op;
    unsigned char *loc;
    int arg;
{
  *loc = (unsigned char) op;
  STORE_NUMBER (loc + 1, arg);
}


/* Like `store_op1', but for two two-byte parameters ARG1 and ARG2.  */

static void
store_op2 (op, loc, arg1, arg2)
    re_opcode_t op;
    unsigned char *loc;
    int arg1, arg2;
{
  *loc = (unsigned char) op;
  STORE_NUMBER (loc + 1, arg1);
  STORE_NUMBER (loc + 3, arg2);
}


/* Copy the bytes from LOC to END to open up three bytes of space at LOC
   for OP followed by two-byte integer parameter ARG.  */

static void
insert_op1 (op, loc, arg, end)
    re_opcode_t op;
    unsigned char *loc;
    int arg;
    unsigned char *end;
{
  register unsigned char *pfrom = end;
  register unsigned char *pto = end + 3;

  while (pfrom != loc)
    *--pto = *--pfrom;

  store_op1 (op, loc, arg);
}


/* Like `insert_op1', but for two two-byte parameters ARG1 and ARG2.  */

static void
insert_op2 (op, loc, arg1, arg2, end)
    re_opcode_t op;
    unsigned char *loc;
    int arg1, arg2;
    unsigned char *end;
{
  register unsigned char *pfrom = end;
  register unsigned char *pto = end + 5;

  while (pfrom != loc)
    *--pto = *--pfrom;

  store_op2 (op, loc, arg1, arg2);
}


/* P points to just after a ^ in PATTERN.  Return true if that ^ comes
   after an alternative or a begin-subexpression.  We assume there is at
   least one character before the ^.  */

static boolean
at_begline_loc_p (pattern, p, syntax)
    re_char *pattern, *p;
    reg_syntax_t syntax;
{
  re_char *prev = p - 2;
  boolean prev_prev_backslash = prev > pattern && prev[-1] == '\\';

  return
       /* After a subexpression?  */
       (*prev == '(' && (syntax & RE_NO_BK_PARENS || prev_prev_backslash))
       /* After an alternative?	 */
    || (*prev == '|' && (syntax & RE_NO_BK_VBAR || prev_prev_backslash))
       /* After a shy subexpression?  */
    || ((syntax & RE_SHY_GROUPS) && prev - 2 >= pattern
	&& prev[-1] == '?' && prev[-2] == '('
	&& (syntax & RE_NO_BK_PARENS
	    || (prev - 3 >= pattern && prev[-3] == '\\')));
}


/* The dual of at_begline_loc_p.  This one is for $.  We assume there is
   at least one character after the $, i.e., `P < PEND'.  */

static boolean
at_endline_loc_p (p, pend, syntax)
    re_char *p, *pend;
    reg_syntax_t syntax;
{
  re_char *next = p;
  boolean next_backslash = *next == '\\';
  re_char *next_next = p + 1 < pend ? p + 1 : 0;

  return
       /* Before a subexpression?  */
       (syntax & RE_NO_BK_PARENS ? *next == ')'
	: next_backslash && next_next && *next_next == ')')
       /* Before an alternative?  */
    || (syntax & RE_NO_BK_VBAR ? *next == '|'
	: next_backslash && next_next && *next_next == '|');
}


/* Returns true if REGNUM is in one of COMPILE_STACK's elements and
   false if it's not.  */

static boolean
group_in_compile_stack (compile_stack, regnum)
    compile_stack_type compile_stack;
    regnum_t regnum;
{
  int this_element;

  for (this_element = compile_stack.avail - 1;
       this_element >= 0;
       this_element--)
    if (compile_stack.stack[this_element].regnum == regnum)
      return true;

  return false;
}

/* analyse_first.
   If fastmap is non-NULL, go through the pattern and fill fastmap
   with all the possible leading chars.  If fastmap is NULL, don't
   bother filling it up (obviously) and only return whether the
   pattern could potentially match the empty string.

   Return 1  if p..pend might match the empty string.
   Return 0  if p..pend matches at least one char.
   Return -1 if fastmap was not updated accurately.  */

static int
analyse_first (p, pend, fastmap, multibyte)
     re_char *p, *pend;
     char *fastmap;
     const int multibyte;
{
  int j, k;
  boolean not;

  /* If all elements for base leading-codes in fastmap is set, this
     flag is set true.	*/
  boolean match_any_multibyte_characters = false;

  assert (p);

  /* The loop below works as follows:
     - It has a working-list kept in the PATTERN_STACK and which basically
       starts by only containing a pointer to the first operation.
     - If the opcode we're looking at is a match against some set of
       chars, then we add those chars to the fastmap and go on to the
       next work element from the worklist (done via `break').
     - If the opcode is a control operator on the other hand, we either
       ignore it (if it's meaningless at this point, such as `start_memory')
       or execute it (if it's a jump).  If the jump has several destinations
       (i.e. `on_failure_jump'), then we push the other destination onto the
       worklist.
     We guarantee termination by ignoring backward jumps (more or less),
     so that `p' is monotonically increasing.  More to the point, we
     never set `p' (or push) anything `<= p1'.  */

  while (p < pend)
    {
      /* `p1' is used as a marker of how far back a `on_failure_jump'
	 can go without being ignored.  It is normally equal to `p'
	 (which prevents any backward `on_failure_jump') except right
	 after a plain `jump', to allow patterns such as:
	    0: jump 10
	    3..9: <body>
	    10: on_failure_jump 3
	 as used for the *? operator.  */
      re_char *p1 = p;

      switch (SWITCH_ENUM_CAST ((re_opcode_t) *p++))
	{
	case succeed:
	  return 1;
	  continue;

	case duplicate:
	  /* If the first character has to match a backreference, that means
	     that the group was empty (since it already matched).  Since this
	     is the only case that interests us here, we can assume that the
	     backreference must match the empty string.  */
	  p++;
	  continue;


      /* Following are the cases which match a character.  These end
	 with `break'.	*/

	case exactn:
	  if (fastmap)
	    {
	      int c = RE_STRING_CHAR (p + 1, pend - p);

	      if (SINGLE_BYTE_CHAR_P (c))
		fastmap[c] = 1;
	      else
		fastmap[p[1]] = 1;
	    }
	  break;


	case anychar:
	  /* We could put all the chars except for \n (and maybe \0)
	     but we don't bother since it is generally not worth it.  */
	  if (!fastmap) break;
	  return -1;


	case charset_not:
	  /* Chars beyond end of bitmap are possible matches.
	     All the single-byte codes can occur in multibyte buffers.
	     So any that are not listed in the charset
	     are possible matches, even in multibyte buffers.  */
	  if (!fastmap) break;
	  for (j = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH;
	       j < (1 << BYTEWIDTH); j++)
	    fastmap[j] = 1;
	  /* Fallthrough */
	case charset:
	  if (!fastmap) break;
	  not = (re_opcode_t) *(p - 1) == charset_not;
	  for (j = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH - 1, p++;
	       j >= 0; j--)
	    if (!!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))) ^ not)
	      fastmap[j] = 1;

	  if ((not && multibyte)
	      /* Any character set can possibly contain a character
		 which doesn't match the specified set of characters.  */
	      || (CHARSET_RANGE_TABLE_EXISTS_P (&p[-2])
		  && CHARSET_RANGE_TABLE_BITS (&p[-2]) != 0))
	    /* If we can match a character class, we can match
	       any character set.  */
	    {
	    set_fastmap_for_multibyte_characters:
	      if (match_any_multibyte_characters == false)
		{
		  for (j = 0x80; j < 0xA0; j++)	/* XXX */
		    if (BASE_LEADING_CODE_P (j))
		      fastmap[j] = 1;
		  match_any_multibyte_characters = true;
		}
	    }

	  else if (!not && CHARSET_RANGE_TABLE_EXISTS_P (&p[-2])
		   && match_any_multibyte_characters == false)
	    {
	      /* Set fastmap[I] 1 where I is a base leading code of each
		 multibyte character in the range table. */
	      int c, count;

	      /* Make P points the range table.  `+ 2' is to skip flag
		 bits for a character class.  */
	      p += CHARSET_BITMAP_SIZE (&p[-2]) + 2;

	      /* Extract the number of ranges in range table into COUNT.  */
	      EXTRACT_NUMBER_AND_INCR (count, p);
	      for (; count > 0; count--, p += 2 * 3) /* XXX */
		{
		  /* Extract the start of each range.  */
		  EXTRACT_CHARACTER (c, p);
		  j = CHAR_CHARSET (c);
		  fastmap[CHARSET_LEADING_CODE_BASE (j)] = 1;
		}
	    }
	  break;

	case syntaxspec:
	case notsyntaxspec:
	  if (!fastmap) break;
#ifndef emacs
	  not = (re_opcode_t)p[-1] == notsyntaxspec;
	  k = *p++;
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if ((SYNTAX (j) == (enum syntaxcode) k) ^ not)
	      fastmap[j] = 1;
	  break;
#else  /* emacs */
	  /* This match depends on text properties.  These end with
	     aborting optimizations.  */
	  return -1;

	case categoryspec:
	case notcategoryspec:
	  if (!fastmap) break;
	  not = (re_opcode_t)p[-1] == notcategoryspec;
	  k = *p++;
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if ((CHAR_HAS_CATEGORY (j, k)) ^ not)
	      fastmap[j] = 1;

	  if (multibyte)
	    /* Any character set can possibly contain a character
	       whose category is K (or not).  */
	    goto set_fastmap_for_multibyte_characters;
	  break;

      /* All cases after this match the empty string.  These end with
	 `continue'.  */

	case before_dot:
	case at_dot:
	case after_dot:
#endif /* !emacs */
	case no_op:
	case begline:
	case endline:
	case begbuf:
	case endbuf:
	case wordbound:
	case notwordbound:
	case wordbeg:
	case wordend:
	  continue;


	case jump:
	  EXTRACT_NUMBER_AND_INCR (j, p);
	  if (j < 0)
	    /* Backward jumps can only go back to code that we've already
	       visited.  `re_compile' should make sure this is true.  */
	    break;
	  p += j;
	  switch (SWITCH_ENUM_CAST ((re_opcode_t) *p))
	    {
	    case on_failure_jump:
	    case on_failure_keep_string_jump:
	    case on_failure_jump_loop:
	    case on_failure_jump_nastyloop:
	    case on_failure_jump_smart:
	      p++;
	      break;
	    default:
	      continue;
	    };
	  /* Keep `p1' to allow the `on_failure_jump' we are jumping to
	     to jump back to "just after here".  */
	  /* Fallthrough */

	case on_failure_jump:
	case on_failure_keep_string_jump:
	case on_failure_jump_nastyloop:
	case on_failure_jump_loop:
	case on_failure_jump_smart:
	  EXTRACT_NUMBER_AND_INCR (j, p);
	  if (p + j <= p1)
	    ; /* Backward jump to be ignored.  */
	  else
	    { /* We have to look down both arms.
		 We first go down the "straight" path so as to minimize
		 stack usage when going through alternatives.  */
	      int r = analyse_first (p, pend, fastmap, multibyte);
	      if (r) return r;
	      p += j;
	    }
	  continue;


	case jump_n:
	  /* This code simply does not properly handle forward jump_n.  */
	  DEBUG_STATEMENT (EXTRACT_NUMBER (j, p); assert (j < 0));
	  p += 4;
	  /* jump_n can either jump or fall through.  The (backward) jump
	     case has already been handled, so we only need to look at the
	     fallthrough case.  */
	  continue;
	  
	case succeed_n:
	  /* If N == 0, it should be an on_failure_jump_loop instead.  */
	  DEBUG_STATEMENT (EXTRACT_NUMBER (j, p + 2); assert (j > 0));
	  p += 4;
	  /* We only care about one iteration of the loop, so we don't
	     need to consider the case where this behaves like an
	     on_failure_jump.  */
	  continue;


	case set_number_at:
	  p += 4;
	  continue;


	case start_memory:
	case stop_memory:
	  p += 1;
	  continue;


	default:
	  abort (); /* We have listed all the cases.  */
	} /* switch *p++ */

      /* Getting here means we have found the possible starting
	 characters for one path of the pattern -- and that the empty
	 string does not match.	 We need not follow this path further.  */
      return 0;
    } /* while p */

  /* We reached the end without matching anything.  */
  return 1;

} /* analyse_first */

/* re_compile_fastmap computes a ``fastmap'' for the compiled pattern in
   BUFP.  A fastmap records which of the (1 << BYTEWIDTH) possible
   characters can start a string that matches the pattern.  This fastmap
   is used by re_search to skip quickly over impossible starting points.

   Character codes above (1 << BYTEWIDTH) are not represented in the
   fastmap, but the leading codes are represented.  Thus, the fastmap
   indicates which character sets could start a match.

   The caller must supply the address of a (1 << BYTEWIDTH)-byte data
   area as BUFP->fastmap.

   We set the `fastmap', `fastmap_accurate', and `can_be_null' fields in
   the pattern buffer.

   Returns 0 if we succeed, -2 if an internal error.   */

int
re_compile_fastmap (bufp)
     struct re_pattern_buffer *bufp;
{
  char *fastmap = bufp->fastmap;
  int analysis;

  assert (fastmap && bufp->buffer);

  bzero (fastmap, 1 << BYTEWIDTH);  /* Assume nothing's valid.	*/
  bufp->fastmap_accurate = 1;	    /* It will be when we're done.  */

  analysis = analyse_first (bufp->buffer, bufp->buffer + bufp->used,
			    fastmap, RE_MULTIBYTE_P (bufp));
  bufp->can_be_null = (analysis != 0);
  return 0;
} /* re_compile_fastmap */

/* Set REGS to hold NUM_REGS registers, storing them in STARTS and
   ENDS.  Subsequent matches using PATTERN_BUFFER and REGS will use
   this memory for recording register information.  STARTS and ENDS
   must be allocated using the malloc library routine, and must each
   be at least NUM_REGS * sizeof (regoff_t) bytes long.

   If NUM_REGS == 0, then subsequent matches should allocate their own
   register data.

   Unless this function is called, the first search or match using
   PATTERN_BUFFER will allocate its own register data, without
   freeing the old data.  */

void
re_set_registers (bufp, regs, num_regs, starts, ends)
    struct re_pattern_buffer *bufp;
    struct re_registers *regs;
    unsigned num_regs;
    regoff_t *starts, *ends;
{
  if (num_regs)
    {
      bufp->regs_allocated = REGS_REALLOCATE;
      regs->num_regs = num_regs;
      regs->start = starts;
      regs->end = ends;
    }
  else
    {
      bufp->regs_allocated = REGS_UNALLOCATED;
      regs->num_regs = 0;
      regs->start = regs->end = (regoff_t *) 0;
    }
}
WEAK_ALIAS (__re_set_registers, re_set_registers)

/* Searching routines.	*/

/* Like re_search_2, below, but only one string is specified, and
   doesn't let you say where to stop matching. */

int
re_search (bufp, string, size, startpos, range, regs)
     struct re_pattern_buffer *bufp;
     const char *string;
     int size, startpos, range;
     struct re_registers *regs;
{
  return re_search_2 (bufp, NULL, 0, string, size, startpos, range,
		      regs, size);
}
WEAK_ALIAS (__re_search, re_search)

/* End address of virtual concatenation of string.  */
#define STOP_ADDR_VSTRING(P)				\
  (((P) >= size1 ? string2 + size2 : string1 + size1))

/* Address of POS in the concatenation of virtual string. */
#define POS_ADDR_VSTRING(POS)					\
  (((POS) >= size1 ? string2 - size1 : string1) + (POS))

/* Using the compiled pattern in BUFP->buffer, first tries to match the
   virtual concatenation of STRING1 and STRING2, starting first at index
   STARTPOS, then at STARTPOS + 1, and so on.

   STRING1 and STRING2 have length SIZE1 and SIZE2, respectively.

   RANGE is how far to scan while trying to match.  RANGE = 0 means try
   only at STARTPOS; in general, the last start tried is STARTPOS +
   RANGE.

   In REGS, return the indices of the virtual concatenation of STRING1
   and STRING2 that matched the entire BUFP->buffer and its contained
   subexpressions.

   Do not consider matching one past the index STOP in the virtual
   concatenation of STRING1 and STRING2.

   We return either the position in the strings at which the match was
   found, -1 if no match, or -2 if error (such as failure
   stack overflow).  */

int
re_search_2 (bufp, str1, size1, str2, size2, startpos, range, regs, stop)
     struct re_pattern_buffer *bufp;
     const char *str1, *str2;
     int size1, size2;
     int startpos;
     int range;
     struct re_registers *regs;
     int stop;
{
  int val;
  re_char *string1 = (re_char*) str1;
  re_char *string2 = (re_char*) str2;
  register char *fastmap = bufp->fastmap;
  register RE_TRANSLATE_TYPE translate = bufp->translate;
  int total_size = size1 + size2;
  int endpos = startpos + range;
  boolean anchored_start;

  /* Nonzero if we have to concern multibyte character.	 */
  const boolean multibyte = RE_MULTIBYTE_P (bufp);

  /* Check for out-of-range STARTPOS.  */
  if (startpos < 0 || startpos > total_size)
    return -1;

  /* Fix up RANGE if it might eventually take us outside
     the virtual concatenation of STRING1 and STRING2.
     Make sure we won't move STARTPOS below 0 or above TOTAL_SIZE.  */
  if (endpos < 0)
    range = 0 - startpos;
  else if (endpos > total_size)
    range = total_size - startpos;

  /* If the search isn't to be a backwards one, don't waste time in a
     search for a pattern anchored at beginning of buffer.  */
  if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == begbuf && range > 0)
    {
      if (startpos > 0)
	return -1;
      else
	range = 0;
    }

#ifdef emacs
  /* In a forward search for something that starts with \=.
     don't keep searching past point.  */
  if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == at_dot && range > 0)
    {
      range = PT_BYTE - BEGV_BYTE - startpos;
      if (range < 0)
	return -1;
    }
#endif /* emacs */

  /* Update the fastmap now if not correct already.  */
  if (fastmap && !bufp->fastmap_accurate)
    re_compile_fastmap (bufp);

  /* See whether the pattern is anchored.  */
  anchored_start = (bufp->buffer[0] == begline);

#ifdef emacs
  gl_state.object = re_match_object;
  {
    int charpos = SYNTAX_TABLE_BYTE_TO_CHAR (POS_AS_IN_BUFFER (startpos));

    SETUP_SYNTAX_TABLE_FOR_OBJECT (re_match_object, charpos, 1);
  }
#endif

  /* Loop through the string, looking for a place to start matching.  */
  for (;;)
    {
      /* If the pattern is anchored,
	 skip quickly past places we cannot match.
	 We don't bother to treat startpos == 0 specially
	 because that case doesn't repeat.  */
      if (anchored_start && startpos > 0)
	{
	  if (! ((startpos <= size1 ? string1[startpos - 1]
		  : string2[startpos - size1 - 1])
		 == '\n'))
	    goto advance;
	}

      /* If a fastmap is supplied, skip quickly over characters that
	 cannot be the start of a match.  If the pattern can match the
	 null string, however, we don't need to skip characters; we want
	 the first null string.	 */
      if (fastmap && startpos < total_size && !bufp->can_be_null)
	{
	  register re_char *d;
	  register re_wchar_t buf_ch;

	  d = POS_ADDR_VSTRING (startpos);

	  if (range > 0)	/* Searching forwards.	*/
	    {
	      register int lim = 0;
	      int irange = range;

	      if (startpos < size1 && startpos + range >= size1)
		lim = range - (size1 - startpos);

	      /* Written out as an if-else to avoid testing `translate'
		 inside the loop.  */
	      if (RE_TRANSLATE_P (translate))
		{
		  if (multibyte)
		    while (range > lim)
		      {
			int buf_charlen;

			buf_ch = STRING_CHAR_AND_LENGTH (d, range - lim,
							 buf_charlen);

			buf_ch = RE_TRANSLATE (translate, buf_ch);
			if (buf_ch >= 0400
			    || fastmap[buf_ch])
			  break;

			range -= buf_charlen;
			d += buf_charlen;
		      }
		  else
		    while (range > lim
			   && !fastmap[RE_TRANSLATE (translate, *d)])
		      {
			d++;
			range--;
		      }
		}
	      else
		while (range > lim && !fastmap[*d])
		  {
		    d++;
		    range--;
		  }

	      startpos += irange - range;
	    }
	  else				/* Searching backwards.	 */
	    {
	      int room = (startpos >= size1
			  ? size2 + size1 - startpos
			  : size1 - startpos);
	      buf_ch = RE_STRING_CHAR (d, room);
	      buf_ch = TRANSLATE (buf_ch);

	      if (! (buf_ch >= 0400
		     || fastmap[buf_ch]))
		goto advance;
	    }
	}

      /* If can't match the null string, and that's all we have left, fail.  */
      if (range >= 0 && startpos == total_size && fastmap
	  && !bufp->can_be_null)
	return -1;

      val = re_match_2_internal (bufp, string1, size1, string2, size2,
				 startpos, regs, stop);
#ifndef REGEX_MALLOC
# ifdef C_ALLOCA
      alloca (0);
# endif
#endif

      if (val >= 0)
	return startpos;

      if (val == -2)
	return -2;

    advance:
      if (!range)
	break;
      else if (range > 0)
	{
	  /* Update STARTPOS to the next character boundary.  */
	  if (multibyte)
	    {
	      re_char *p = POS_ADDR_VSTRING (startpos);
	      re_char *pend = STOP_ADDR_VSTRING (startpos);
	      int len = MULTIBYTE_FORM_LENGTH (p, pend - p);

	      range -= len;
	      if (range < 0)
		break;
	      startpos += len;
	    }
	  else
	    {
	      range--;
	      startpos++;
	    }
	}
      else
	{
	  range++;
	  startpos--;

	  /* Update STARTPOS to the previous character boundary.  */
	  if (multibyte)
	    {
	      re_char *p = POS_ADDR_VSTRING (startpos);
	      int len = 0;

	      /* Find the head of multibyte form.  */
	      while (!CHAR_HEAD_P (*p))
		p--, len++;

	      /* Adjust it. */
#if 0				/* XXX */
	      if (MULTIBYTE_FORM_LENGTH (p, len + 1) != (len + 1))
		;
	      else
#endif
		{
		  range += len;
		  if (range > 0)
		    break;

		  startpos -= len;
		}
	    }
	}
    }
  return -1;
} /* re_search_2 */
WEAK_ALIAS (__re_search_2, re_search_2)

/* Declarations and macros for re_match_2.  */

static int bcmp_translate _RE_ARGS((re_char *s1, re_char *s2,
				    register int len,
				    RE_TRANSLATE_TYPE translate,
				    const int multibyte));

/* This converts PTR, a pointer into one of the search strings `string1'
   and `string2' into an offset from the beginning of that string.  */
#define POINTER_TO_OFFSET(ptr)			\
  (FIRST_STRING_P (ptr)				\
   ? ((regoff_t) ((ptr) - string1))		\
   : ((regoff_t) ((ptr) - string2 + size1)))

/* Call before fetching a character with *d.  This switches over to
   string2 if necessary.
   Check re_match_2_internal for a discussion of why end_match_2 might
   not be within string2 (but be equal to end_match_1 instead).  */
#define PREFETCH()							\
  while (d == dend)							\
    {									\
      /* End of string2 => fail.  */					\
      if (dend == end_match_2)						\
	goto fail;							\
      /* End of string1 => advance to string2.  */			\
      d = string2;							\
      dend = end_match_2;						\
    }

/* Call before fetching a char with *d if you already checked other limits.
   This is meant for use in lookahead operations like wordend, etc..
   where we might need to look at parts of the string that might be
   outside of the LIMITs (i.e past `stop').  */
#define PREFETCH_NOLIMIT()						\
  if (d == end1)							\
     {									\
       d = string2;							\
       dend = end_match_2;						\
     }									\

/* Test if at very beginning or at very end of the virtual concatenation
   of `string1' and `string2'.	If only one string, it's `string2'.  */
#define AT_STRINGS_BEG(d) ((d) == (size1 ? string1 : string2) || !size2)
#define AT_STRINGS_END(d) ((d) == end2)


/* Test if D points to a character which is word-constituent.  We have
   two special cases to check for: if past the end of string1, look at
   the first character in string2; and if before the beginning of
   string2, look at the last character in string1.  */
#define WORDCHAR_P(d)							\
  (SYNTAX ((d) == end1 ? *string2					\
	   : (d) == string2 - 1 ? *(end1 - 1) : *(d))			\
   == Sword)

/* Disabled due to a compiler bug -- see comment at case wordbound */

/* The comment at case wordbound is following one, but we don't use
   AT_WORD_BOUNDARY anymore to support multibyte form.

   The DEC Alpha C compiler 3.x generates incorrect code for the
   test	 WORDCHAR_P (d - 1) != WORDCHAR_P (d)  in the expansion of
   AT_WORD_BOUNDARY, so this code is disabled.	Expanding the
   macro and introducing temporary variables works around the bug.  */

#if 0
/* Test if the character before D and the one at D differ with respect
   to being word-constituent.  */
#define AT_WORD_BOUNDARY(d)						\
  (AT_STRINGS_BEG (d) || AT_STRINGS_END (d)				\
   || WORDCHAR_P (d - 1) != WORDCHAR_P (d))
#endif

/* Free everything we malloc.  */
#ifdef MATCH_MAY_ALLOCATE
# define FREE_VAR(var) if (var) { REGEX_FREE (var); var = NULL; } else
# define FREE_VARIABLES()						\
  do {									\
    REGEX_FREE_STACK (fail_stack.stack);				\
    FREE_VAR (regstart);						\
    FREE_VAR (regend);							\
    FREE_VAR (best_regstart);						\
    FREE_VAR (best_regend);						\
  } while (0)
#else
# define FREE_VARIABLES() ((void)0) /* Do nothing!  But inhibit gcc warning.  */
#endif /* not MATCH_MAY_ALLOCATE */


/* Optimization routines.  */

/* If the operation is a match against one or more chars,
   return a pointer to the next operation, else return NULL.  */
static re_char *
skip_one_char (p)
     re_char *p;
{
  switch (SWITCH_ENUM_CAST (*p++))
    {
    case anychar:
      break;
      
    case exactn:
      p += *p + 1;
      break;

    case charset_not:
    case charset:
      if (CHARSET_RANGE_TABLE_EXISTS_P (p - 1))
	{
	  int mcnt;
	  p = CHARSET_RANGE_TABLE (p - 1);
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  p = CHARSET_RANGE_TABLE_END (p, mcnt);
	}
      else
	p += 1 + CHARSET_BITMAP_SIZE (p - 1);
      break;
      
    case syntaxspec:
    case notsyntaxspec:
#ifdef emacs
    case categoryspec:
    case notcategoryspec:
#endif /* emacs */
      p++;
      break;

    default:
      p = NULL;
    }
  return p;
}


/* Jump over non-matching operations.  */
static unsigned char *
skip_noops (p, pend)
     unsigned char *p, *pend;
{
  int mcnt;
  while (p < pend)
    {
      switch (SWITCH_ENUM_CAST ((re_opcode_t) *p))
	{
	case start_memory:
	case stop_memory:
	  p += 2; break;
	case no_op:
	  p += 1; break;
	case jump:
	  p += 1;
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  p += mcnt;
	  break;
	default:
	  return p;
	}
    }
  assert (p == pend);
  return p;
}

/* Non-zero if "p1 matches something" implies "p2 fails".  */
static int
mutually_exclusive_p (bufp, p1, p2)
     struct re_pattern_buffer *bufp;
     unsigned char *p1, *p2;
{
  re_opcode_t op2;
  const boolean multibyte = RE_MULTIBYTE_P (bufp);
  unsigned char *pend = bufp->buffer + bufp->used;

  assert (p1 >= bufp->buffer && p1 < pend
	  && p2 >= bufp->buffer && p2 <= pend);

  /* Skip over open/close-group commands.
     If what follows this loop is a ...+ construct,
     look at what begins its body, since we will have to
     match at least one of that.  */
  p2 = skip_noops (p2, pend);
  /* The same skip can be done for p1, except that this function
     is only used in the case where p1 is a simple match operator.  */
  /* p1 = skip_noops (p1, pend); */

  assert (p1 >= bufp->buffer && p1 < pend
	  && p2 >= bufp->buffer && p2 <= pend);

  op2 = p2 == pend ? succeed : *p2;

  switch (SWITCH_ENUM_CAST (op2))
    {
    case succeed:
    case endbuf:
      /* If we're at the end of the pattern, we can change.  */
      if (skip_one_char (p1))
	{
	  DEBUG_PRINT1 ("  End of pattern: fast loop.\n");
	  return 1;
	}
      break;
      
    case endline:
    case exactn:
      {
	register re_wchar_t c
	  = (re_opcode_t) *p2 == endline ? '\n'
	  : RE_STRING_CHAR (p2 + 2, pend - p2 - 2);

	if ((re_opcode_t) *p1 == exactn)
	  {
	    if (c != RE_STRING_CHAR (p1 + 2, pend - p1 - 2))
	      {
		DEBUG_PRINT3 ("  '%c' != '%c' => fast loop.\n", c, p1[2]);
		return 1;
	      }
	  }

	else if ((re_opcode_t) *p1 == charset
		 || (re_opcode_t) *p1 == charset_not)
	  {
	    int not = (re_opcode_t) *p1 == charset_not;

	    /* Test if C is listed in charset (or charset_not)
	       at `p1'.  */
	    if (SINGLE_BYTE_CHAR_P (c))
	      {
		if (c < CHARSET_BITMAP_SIZE (p1) * BYTEWIDTH
		    && p1[2 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
		  not = !not;
	      }
	    else if (CHARSET_RANGE_TABLE_EXISTS_P (p1))
	      CHARSET_LOOKUP_RANGE_TABLE (not, c, p1);

	    /* `not' is equal to 1 if c would match, which means
	       that we can't change to pop_failure_jump.  */
	    if (!not)
	      {
		DEBUG_PRINT1 ("	 No match => fast loop.\n");
		return 1;
	      }
	  }
	else if ((re_opcode_t) *p1 == anychar
		 && c == '\n')
	  {
	    DEBUG_PRINT1 ("   . != \\n => fast loop.\n");
	    return 1;
	  }
      }
      break;

    case charset:
      {
	if ((re_opcode_t) *p1 == exactn)
	  /* Reuse the code above.  */
	  return mutually_exclusive_p (bufp, p2, p1);

      /* It is hard to list up all the character in charset
	 P2 if it includes multibyte character.  Give up in
	 such case.  */
      else if (!multibyte || !CHARSET_RANGE_TABLE_EXISTS_P (p2))
	{
	  /* Now, we are sure that P2 has no range table.
	     So, for the size of bitmap in P2, `p2[1]' is
	     enough.	But P1 may have range table, so the
	     size of bitmap table of P1 is extracted by
	     using macro `CHARSET_BITMAP_SIZE'.

	     Since we know that all the character listed in
	     P2 is ASCII, it is enough to test only bitmap
	     table of P1.  */

	  if ((re_opcode_t) *p1 == charset)
	    {
	      int idx;
	      /* We win if the charset inside the loop
		 has no overlap with the one after the loop.  */
	      for (idx = 0;
		   (idx < (int) p2[1]
		    && idx < CHARSET_BITMAP_SIZE (p1));
		   idx++)
		if ((p2[2 + idx] & p1[2 + idx]) != 0)
		  break;

	      if (idx == p2[1]
		  || idx == CHARSET_BITMAP_SIZE (p1))
		{
		  DEBUG_PRINT1 ("	 No match => fast loop.\n");
		  return 1;
		}
	    }
	  else if ((re_opcode_t) *p1 == charset_not)
	    {
	      int idx;
	      /* We win if the charset_not inside the loop lists
		 every character listed in the charset after.	 */
	      for (idx = 0; idx < (int) p2[1]; idx++)
		if (! (p2[2 + idx] == 0
		       || (idx < CHARSET_BITMAP_SIZE (p1)
			   && ((p2[2 + idx] & ~ p1[2 + idx]) == 0))))
		  break;

		if (idx == p2[1])
		  {
		    DEBUG_PRINT1 ("	 No match => fast loop.\n");
		    return 1;
		  }
	      }
	  }
      }
      break;
      
    case charset_not:
      switch (SWITCH_ENUM_CAST (*p1))
	{
	case exactn:
	case charset:
	  /* Reuse the code above.  */
	  return mutually_exclusive_p (bufp, p2, p1);
	case charset_not:
	  /* When we have two charset_not, it's very unlikely that
	     they don't overlap.  The union of the two sets of excluded
	     chars should cover all possible chars, which, as a matter of
	     fact, is virtually impossible in multibyte buffers.  */
	  ;
	}
      break;

    case wordend:
    case notsyntaxspec:
      return ((re_opcode_t) *p1 == syntaxspec
	      && p1[1] == (op2 == wordend ? Sword : p2[1]));

    case wordbeg:
    case syntaxspec:
      return ((re_opcode_t) *p1 == notsyntaxspec
	      && p1[1] == (op2 == wordend ? Sword : p2[1]));

    case wordbound:
      return (((re_opcode_t) *p1 == notsyntaxspec
	       || (re_opcode_t) *p1 == syntaxspec)
	      && p1[1] == Sword);

#ifdef emacs
    case categoryspec:
      return ((re_opcode_t) *p1 == notcategoryspec && p1[1] == p2[1]);
    case notcategoryspec:
      return ((re_opcode_t) *p1 == categoryspec && p1[1] == p2[1]);
#endif /* emacs */

    default:
      ;
    }

  /* Safe default.  */
  return 0;
}


/* Matching routines.  */

#ifndef emacs	/* Emacs never uses this.  */
/* re_match is like re_match_2 except it takes only a single string.  */

int
re_match (bufp, string, size, pos, regs)
     struct re_pattern_buffer *bufp;
     const char *string;
     int size, pos;
     struct re_registers *regs;
{
  int result = re_match_2_internal (bufp, NULL, 0, (re_char*) string, size,
				    pos, regs, size);
# if defined C_ALLOCA && !defined REGEX_MALLOC
  alloca (0);
# endif
  return result;
}
WEAK_ALIAS (__re_match, re_match)
#endif /* not emacs */

#ifdef emacs
/* In Emacs, this is the string or buffer in which we
   are matching.  It is used for looking up syntax properties.	*/
Lisp_Object re_match_object;
#endif

/* re_match_2 matches the compiled pattern in BUFP against the
   the (virtual) concatenation of STRING1 and STRING2 (of length SIZE1
   and SIZE2, respectively).  We start matching at POS, and stop
   matching at STOP.

   If REGS is non-null and the `no_sub' field of BUFP is nonzero, we
   store offsets for the substring each group matched in REGS.	See the
   documentation for exactly how many groups we fill.

   We return -1 if no match, -2 if an internal error (such as the
   failure stack overflowing).	Otherwise, we return the length of the
   matched substring.  */

int
re_match_2 (bufp, string1, size1, string2, size2, pos, regs, stop)
     struct re_pattern_buffer *bufp;
     const char *string1, *string2;
     int size1, size2;
     int pos;
     struct re_registers *regs;
     int stop;
{
  int result;

#ifdef emacs
  int charpos;
  gl_state.object = re_match_object;
  charpos = SYNTAX_TABLE_BYTE_TO_CHAR (POS_AS_IN_BUFFER (pos));
  SETUP_SYNTAX_TABLE_FOR_OBJECT (re_match_object, charpos, 1);
#endif

  result = re_match_2_internal (bufp, (re_char*) string1, size1,
				(re_char*) string2, size2,
				pos, regs, stop);
#if defined C_ALLOCA && !defined REGEX_MALLOC
  alloca (0);
#endif
  return result;
}
WEAK_ALIAS (__re_match_2, re_match_2)

/* This is a separate function so that we can force an alloca cleanup
   afterwards.	*/
static int
re_match_2_internal (bufp, string1, size1, string2, size2, pos, regs, stop)
     struct re_pattern_buffer *bufp;
     re_char *string1, *string2;
     int size1, size2;
     int pos;
     struct re_registers *regs;
     int stop;
{
  /* General temporaries.  */
  int mcnt;
  size_t reg;
  boolean not;

  /* Just past the end of the corresponding string.  */
  re_char *end1, *end2;

  /* Pointers into string1 and string2, just past the last characters in
     each to consider matching.	 */
  re_char *end_match_1, *end_match_2;

  /* Where we are in the data, and the end of the current string.  */
  re_char *d, *dend;

  /* Used sometimes to remember where we were before starting matching
     an operator so that we can go back in case of failure.  This "atomic"
     behavior of matching opcodes is indispensable to the correctness
     of the on_failure_keep_string_jump optimization.  */
  re_char *dfail;

  /* Where we are in the pattern, and the end of the pattern.  */
  re_char *p = bufp->buffer;
  re_char *pend = p + bufp->used;

  /* We use this to map every character in the string.	*/
  RE_TRANSLATE_TYPE translate = bufp->translate;

  /* Nonzero if we have to concern multibyte character.	 */
  const boolean multibyte = RE_MULTIBYTE_P (bufp);

  /* Failure point stack.  Each place that can handle a failure further
     down the line pushes a failure point on this stack.  It consists of
     regstart, and regend for all registers corresponding to
     the subexpressions we're currently inside, plus the number of such
     registers, and, finally, two char *'s.  The first char * is where
     to resume scanning the pattern; the second one is where to resume
     scanning the strings.	*/
#ifdef MATCH_MAY_ALLOCATE /* otherwise, this is global.	 */
  fail_stack_type fail_stack;
#endif
#ifdef DEBUG
  unsigned nfailure_points_pushed = 0, nfailure_points_popped = 0;
#endif

#if defined REL_ALLOC && defined REGEX_MALLOC
  /* This holds the pointer to the failure stack, when
     it is allocated relocatably.  */
  fail_stack_elt_t *failure_stack_ptr;
#endif

  /* We fill all the registers internally, independent of what we
     return, for use in backreferences.	 The number here includes
     an element for register zero.  */
  size_t num_regs = bufp->re_nsub + 1;

  /* Information on the contents of registers. These are pointers into
     the input strings; they record just what was matched (on this
     attempt) by a subexpression part of the pattern, that is, the
     regnum-th regstart pointer points to where in the pattern we began
     matching and the regnum-th regend points to right after where we
     stopped matching the regnum-th subexpression.  (The zeroth register
     keeps track of what the whole pattern matches.)  */
#ifdef MATCH_MAY_ALLOCATE /* otherwise, these are global.  */
  re_char **regstart, **regend;
#endif

  /* The following record the register info as found in the above
     variables when we find a match better than any we've seen before.
     This happens as we backtrack through the failure points, which in
     turn happens only if we have not yet matched the entire string. */
  unsigned best_regs_set = false;
#ifdef MATCH_MAY_ALLOCATE /* otherwise, these are global.  */
  re_char **best_regstart, **best_regend;
#endif

  /* Logically, this is `best_regend[0]'.  But we don't want to have to
     allocate space for that if we're not allocating space for anything
     else (see below).	Also, we never need info about register 0 for
     any of the other register vectors, and it seems rather a kludge to
     treat `best_regend' differently than the rest.  So we keep track of
     the end of the best match so far in a separate variable.  We
     initialize this to NULL so that when we backtrack the first time
     and need to test it, it's not garbage.  */
  re_char *match_end = NULL;

#ifdef DEBUG
  /* Counts the total number of registers pushed.  */
  unsigned num_regs_pushed = 0;
#endif

  DEBUG_PRINT1 ("\n\nEntering re_match_2.\n");

  INIT_FAIL_STACK ();

#ifdef MATCH_MAY_ALLOCATE
  /* Do not bother to initialize all the register variables if there are
     no groups in the pattern, as it takes a fair amount of time.  If
     there are groups, we include space for register 0 (the whole
     pattern), even though we never use it, since it simplifies the
     array indexing.  We should fix this.  */
  if (bufp->re_nsub)
    {
      regstart = REGEX_TALLOC (num_regs, re_char *);
      regend = REGEX_TALLOC (num_regs, re_char *);
      best_regstart = REGEX_TALLOC (num_regs, re_char *);
      best_regend = REGEX_TALLOC (num_regs, re_char *);

      if (!(regstart && regend && best_regstart && best_regend))
	{
	  FREE_VARIABLES ();
	  return -2;
	}
    }
  else
    {
      /* We must initialize all our variables to NULL, so that
	 `FREE_VARIABLES' doesn't try to free them.  */
      regstart = regend = best_regstart = best_regend = NULL;
    }
#endif /* MATCH_MAY_ALLOCATE */

  /* The starting position is bogus.  */
  if (pos < 0 || pos > size1 + size2)
    {
      FREE_VARIABLES ();
      return -1;
    }

  /* Initialize subexpression text positions to -1 to mark ones that no
     start_memory/stop_memory has been seen for. Also initialize the
     register information struct.  */
  for (reg = 1; reg < num_regs; reg++)
    regstart[reg] = regend[reg] = NULL;

  /* We move `string1' into `string2' if the latter's empty -- but not if
     `string1' is null.	 */
  if (size2 == 0 && string1 != NULL)
    {
      string2 = string1;
      size2 = size1;
      string1 = 0;
      size1 = 0;
    }
  end1 = string1 + size1;
  end2 = string2 + size2;

  /* `p' scans through the pattern as `d' scans through the data.
     `dend' is the end of the input string that `d' points within.  `d'
     is advanced into the following input string whenever necessary, but
     this happens before fetching; therefore, at the beginning of the
     loop, `d' can be pointing at the end of a string, but it cannot
     equal `string2'.  */
  if (pos >= size1)
    {
      /* Only match within string2.  */
      d = string2 + pos - size1;
      dend = end_match_2 = string2 + stop - size1;
      end_match_1 = end1;	/* Just to give it a value.  */
    }
  else
    {
      if (stop < size1)
	{
	  /* Only match within string1.  */
	  end_match_1 = string1 + stop;
	  /* BEWARE!
	     When we reach end_match_1, PREFETCH normally switches to string2.
	     But in the present case, this means that just doing a PREFETCH
	     makes us jump from `stop' to `gap' within the string.
	     What we really want here is for the search to stop as
	     soon as we hit end_match_1.  That's why we set end_match_2
	     to end_match_1 (since PREFETCH fails as soon as we hit
	     end_match_2).  */
	  end_match_2 = end_match_1;
	}
      else
	{ /* It's important to use this code when stop == size so that
	     moving `d' from end1 to string2 will not prevent the d == dend
	     check from catching the end of string.  */
	  end_match_1 = end1;
	  end_match_2 = string2 + stop - size1;
	}
      d = string1 + pos;
      dend = end_match_1;
    }

  DEBUG_PRINT1 ("The compiled pattern is: ");
  DEBUG_PRINT_COMPILED_PATTERN (bufp, p, pend);
  DEBUG_PRINT1 ("The string to match is: `");
  DEBUG_PRINT_DOUBLE_STRING (d, string1, size1, string2, size2);
  DEBUG_PRINT1 ("'\n");

  /* This loops over pattern commands.	It exits by returning from the
     function if the match is complete, or it drops through if the match
     fails at this starting point in the input data.  */
  for (;;)
    {
      DEBUG_PRINT2 ("\n%p: ", p);

      if (p == pend)
	{ /* End of pattern means we might have succeeded.  */
	  DEBUG_PRINT1 ("end of pattern ... ");

	  /* If we haven't matched the entire string, and we want the
	     longest match, try backtracking.  */
	  if (d != end_match_2)
	    {
	      /* 1 if this match ends in the same string (string1 or string2)
		 as the best previous match.  */
	      boolean same_str_p = (FIRST_STRING_P (match_end)
				    == FIRST_STRING_P (d));
	      /* 1 if this match is the best seen so far.  */
	      boolean best_match_p;

	      /* AIX compiler got confused when this was combined
		 with the previous declaration.	 */
	      if (same_str_p)
		best_match_p = d > match_end;
	      else
		best_match_p = !FIRST_STRING_P (d);

	      DEBUG_PRINT1 ("backtracking.\n");

	      if (!FAIL_STACK_EMPTY ())
		{ /* More failure points to try.  */

		  /* If exceeds best match so far, save it.  */
		  if (!best_regs_set || best_match_p)
		    {
		      best_regs_set = true;
		      match_end = d;

		      DEBUG_PRINT1 ("\nSAVING match as best so far.\n");

		      for (reg = 1; reg < num_regs; reg++)
			{
			  best_regstart[reg] = regstart[reg];
			  best_regend[reg] = regend[reg];
			}
		    }
		  goto fail;
		}

	      /* If no failure points, don't restore garbage.  And if
		 last match is real best match, don't restore second
		 best one. */
	      else if (best_regs_set && !best_match_p)
		{
		restore_best_regs:
		  /* Restore best match.  It may happen that `dend ==
		     end_match_1' while the restored d is in string2.
		     For example, the pattern `x.*y.*z' against the
		     strings `x-' and `y-z-', if the two strings are
		     not consecutive in memory.	 */
		  DEBUG_PRINT1 ("Restoring best registers.\n");

		  d = match_end;
		  dend = ((d >= string1 && d <= end1)
			   ? end_match_1 : end_match_2);

		  for (reg = 1; reg < num_regs; reg++)
		    {
		      regstart[reg] = best_regstart[reg];
		      regend[reg] = best_regend[reg];
		    }
		}
	    } /* d != end_match_2 */

	succeed_label:
	  DEBUG_PRINT1 ("Accepting match.\n");

	  /* If caller wants register contents data back, do it.  */
	  if (regs && !bufp->no_sub)
	    {
	      /* Have the register data arrays been allocated?	*/
	      if (bufp->regs_allocated == REGS_UNALLOCATED)
		{ /* No.  So allocate them with malloc.	 We need one
		     extra element beyond `num_regs' for the `-1' marker
		     GNU code uses.  */
		  regs->num_regs = MAX (RE_NREGS, num_regs + 1);
		  regs->start = TALLOC (regs->num_regs, regoff_t);
		  regs->end = TALLOC (regs->num_regs, regoff_t);
		  if (regs->start == NULL || regs->end == NULL)
		    {
		      FREE_VARIABLES ();
		      return -2;
		    }
		  bufp->regs_allocated = REGS_REALLOCATE;
		}
	      else if (bufp->regs_allocated == REGS_REALLOCATE)
		{ /* Yes.  If we need more elements than were already
		     allocated, reallocate them.  If we need fewer, just
		     leave it alone.  */
		  if (regs->num_regs < num_regs + 1)
		    {
		      regs->num_regs = num_regs + 1;
		      RETALLOC (regs->start, regs->num_regs, regoff_t);
		      RETALLOC (regs->end, regs->num_regs, regoff_t);
		      if (regs->start == NULL || regs->end == NULL)
			{
			  FREE_VARIABLES ();
			  return -2;
			}
		    }
		}
	      else
		{
		  /* These braces fend off a "empty body in an else-statement"
		     warning under GCC when assert expands to nothing.	*/
		  assert (bufp->regs_allocated == REGS_FIXED);
		}

	      /* Convert the pointer data in `regstart' and `regend' to
		 indices.  Register zero has to be set differently,
		 since we haven't kept track of any info for it.  */
	      if (regs->num_regs > 0)
		{
		  regs->start[0] = pos;
		  regs->end[0] = POINTER_TO_OFFSET (d);
		}

	      /* Go through the first `min (num_regs, regs->num_regs)'
		 registers, since that is all we initialized.  */
	      for (reg = 1; reg < MIN (num_regs, regs->num_regs); reg++)
		{
		  if (REG_UNSET (regstart[reg]) || REG_UNSET (regend[reg]))
		    regs->start[reg] = regs->end[reg] = -1;
		  else
		    {
		      regs->start[reg]
			= (regoff_t) POINTER_TO_OFFSET (regstart[reg]);
		      regs->end[reg]
			= (regoff_t) POINTER_TO_OFFSET (regend[reg]);
		    }
		}

	      /* If the regs structure we return has more elements than
		 were in the pattern, set the extra elements to -1.  If
		 we (re)allocated the registers, this is the case,
		 because we always allocate enough to have at least one
		 -1 at the end.	 */
	      for (reg = num_regs; reg < regs->num_regs; reg++)
		regs->start[reg] = regs->end[reg] = -1;
	    } /* regs && !bufp->no_sub */

	  DEBUG_PRINT4 ("%u failure points pushed, %u popped (%u remain).\n",
			nfailure_points_pushed, nfailure_points_popped,
			nfailure_points_pushed - nfailure_points_popped);
	  DEBUG_PRINT2 ("%u registers pushed.\n", num_regs_pushed);

	  mcnt = POINTER_TO_OFFSET (d) - pos;

	  DEBUG_PRINT2 ("Returning %d from re_match_2.\n", mcnt);

	  FREE_VARIABLES ();
	  return mcnt;
	}

      /* Otherwise match next pattern command.	*/
      switch (SWITCH_ENUM_CAST ((re_opcode_t) *p++))
	{
	/* Ignore these.  Used to ignore the n of succeed_n's which
	   currently have n == 0.  */
	case no_op:
	  DEBUG_PRINT1 ("EXECUTING no_op.\n");
	  break;

	case succeed:
	  DEBUG_PRINT1 ("EXECUTING succeed.\n");
	  goto succeed_label;

	/* Match the next n pattern characters exactly.	 The following
	   byte in the pattern defines n, and the n bytes after that
	   are the characters to match.	 */
	case exactn:
	  mcnt = *p++;
	  DEBUG_PRINT2 ("EXECUTING exactn %d.\n", mcnt);

	  /* Remember the start point to rollback upon failure.  */
	  dfail = d;

	  /* This is written out as an if-else so we don't waste time
	     testing `translate' inside the loop.  */
	  if (RE_TRANSLATE_P (translate))
	    {
	      if (multibyte)
		do
		  {
		    int pat_charlen, buf_charlen;
		    unsigned int pat_ch, buf_ch;

		    PREFETCH ();
		    pat_ch = STRING_CHAR_AND_LENGTH (p, pend - p, pat_charlen);
		    buf_ch = STRING_CHAR_AND_LENGTH (d, dend - d, buf_charlen);

		    if (RE_TRANSLATE (translate, buf_ch)
			!= pat_ch)
		      {
			d = dfail;
			goto fail;
		      }

		    p += pat_charlen;
		    d += buf_charlen;
		    mcnt -= pat_charlen;
		  }
		while (mcnt > 0);
	      else
		do
		  {
		    PREFETCH ();
		    if (RE_TRANSLATE (translate, *d) != *p++)
		      {
			d = dfail;
			goto fail;
		      }
		    d++;
		  }
		while (--mcnt);
	    }
	  else
	    {
	      do
		{
		  PREFETCH ();
		  if (*d++ != *p++)
		    {
		      d = dfail;
		      goto fail;
		    }
		}
	      while (--mcnt);
	    }
	  break;


	/* Match any character except possibly a newline or a null.  */
	case anychar:
	  {
	    int buf_charlen;
	    re_wchar_t buf_ch;

	    DEBUG_PRINT1 ("EXECUTING anychar.\n");

	    PREFETCH ();
	    buf_ch = RE_STRING_CHAR_AND_LENGTH (d, dend - d, buf_charlen);
	    buf_ch = TRANSLATE (buf_ch);

	    if ((!(bufp->syntax & RE_DOT_NEWLINE)
		 && buf_ch == '\n')
		|| ((bufp->syntax & RE_DOT_NOT_NULL)
		    && buf_ch == '\000'))
	      goto fail;

	    DEBUG_PRINT2 ("  Matched `%d'.\n", *d);
	    d += buf_charlen;
	  }
	  break;


	case charset:
	case charset_not:
	  {
	    register unsigned int c;
	    boolean not = (re_opcode_t) *(p - 1) == charset_not;
	    int len;

	    /* Start of actual range_table, or end of bitmap if there is no
	       range table.  */
	    re_char *range_table;

	    /* Nonzero if there is a range table.  */
	    int range_table_exists;

	    /* Number of ranges of range table.  This is not included
	       in the initial byte-length of the command.  */
	    int count = 0;

	    DEBUG_PRINT2 ("EXECUTING charset%s.\n", not ? "_not" : "");

	    range_table_exists = CHARSET_RANGE_TABLE_EXISTS_P (&p[-1]);

	    if (range_table_exists)
	      {
		range_table = CHARSET_RANGE_TABLE (&p[-1]); /* Past the bitmap.  */
		EXTRACT_NUMBER_AND_INCR (count, range_table);
	      }

	    PREFETCH ();
	    c = RE_STRING_CHAR_AND_LENGTH (d, dend - d, len);
	    c = TRANSLATE (c); /* The character to match.  */

	    if (SINGLE_BYTE_CHAR_P (c))
	      {			/* Lookup bitmap.  */
		/* Cast to `unsigned' instead of `unsigned char' in
		   case the bit list is a full 32 bytes long.  */
		if (c < (unsigned) (CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH)
		    && p[1 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
		  not = !not;
	      }
#ifdef emacs
	    else if (range_table_exists)
	      {
		int class_bits = CHARSET_RANGE_TABLE_BITS (&p[-1]);

		if (  (class_bits & BIT_LOWER && ISLOWER (c))
		    | (class_bits & BIT_MULTIBYTE)
		    | (class_bits & BIT_PUNCT && ISPUNCT (c))
		    | (class_bits & BIT_SPACE && ISSPACE (c))
		    | (class_bits & BIT_UPPER && ISUPPER (c))
		    | (class_bits & BIT_WORD  && ISWORD (c)))
		  not = !not;
		else
		  CHARSET_LOOKUP_RANGE_TABLE_RAW (not, c, range_table, count);
	      }
#endif /* emacs */

	    if (range_table_exists)
	      p = CHARSET_RANGE_TABLE_END (range_table, count);
	    else
	      p += CHARSET_BITMAP_SIZE (&p[-1]) + 1;

	    if (!not) goto fail;

	    d += len;
	    break;
	  }


	/* The beginning of a group is represented by start_memory.
	   The argument is the register number.  The text
	   matched within the group is recorded (in the internal
	   registers data structure) under the register number.	 */
	case start_memory:
	  DEBUG_PRINT2 ("EXECUTING start_memory %d:\n", *p);

	  /* In case we need to undo this operation (via backtracking).  */
	  PUSH_FAILURE_REG ((unsigned int)*p);

	  regstart[*p] = d;
	  regend[*p] = NULL;	/* probably unnecessary.  -sm  */
	  DEBUG_PRINT2 ("  regstart: %d\n", POINTER_TO_OFFSET (regstart[*p]));

	  /* Move past the register number and inner group count.  */
	  p += 1;
	  break;


	/* The stop_memory opcode represents the end of a group.  Its
	   argument is the same as start_memory's: the register number.  */
	case stop_memory:
	  DEBUG_PRINT2 ("EXECUTING stop_memory %d:\n", *p);

	  assert (!REG_UNSET (regstart[*p]));
	  /* Strictly speaking, there should be code such as:
	     
		assert (REG_UNSET (regend[*p]));
		PUSH_FAILURE_REGSTOP ((unsigned int)*p);

	     But the only info to be pushed is regend[*p] and it is known to
	     be UNSET, so there really isn't anything to push.
	     Not pushing anything, on the other hand deprives us from the
	     guarantee that regend[*p] is UNSET since undoing this operation
	     will not reset its value properly.  This is not important since
	     the value will only be read on the next start_memory or at
	     the very end and both events can only happen if this stop_memory
	     is *not* undone.  */

	  regend[*p] = d;
	  DEBUG_PRINT2 ("      regend: %d\n", POINTER_TO_OFFSET (regend[*p]));

	  /* Move past the register number and the inner group count.  */
	  p += 1;
	  break;


	/* \<digit> has been turned into a `duplicate' command which is
	   followed by the numeric value of <digit> as the register number.  */
	case duplicate:
	  {
	    register re_char *d2, *dend2;
	    int regno = *p++;	/* Get which register to match against.	 */
	    DEBUG_PRINT2 ("EXECUTING duplicate %d.\n", regno);

	    /* Can't back reference a group which we've never matched.	*/
	    if (REG_UNSET (regstart[regno]) || REG_UNSET (regend[regno]))
	      goto fail;

	    /* Where in input to try to start matching.	 */
	    d2 = regstart[regno];

	    /* Remember the start point to rollback upon failure.  */
	    dfail = d;

	    /* Where to stop matching; if both the place to start and
	       the place to stop matching are in the same string, then
	       set to the place to stop, otherwise, for now have to use
	       the end of the first string.  */

	    dend2 = ((FIRST_STRING_P (regstart[regno])
		      == FIRST_STRING_P (regend[regno]))
		     ? regend[regno] : end_match_1);
	    for (;;)
	      {
		/* If necessary, advance to next segment in register
		   contents.  */
		while (d2 == dend2)
		  {
		    if (dend2 == end_match_2) break;
		    if (dend2 == regend[regno]) break;

		    /* End of string1 => advance to string2. */
		    d2 = string2;
		    dend2 = regend[regno];
		  }
		/* At end of register contents => success */
		if (d2 == dend2) break;

		/* If necessary, advance to next segment in data.  */
		PREFETCH ();

		/* How many characters left in this segment to match.  */
		mcnt = dend - d;

		/* Want how many consecutive characters we can match in
		   one shot, so, if necessary, adjust the count.  */
		if (mcnt > dend2 - d2)
		  mcnt = dend2 - d2;

		/* Compare that many; failure if mismatch, else move
		   past them.  */
		if (RE_TRANSLATE_P (translate)
		    ? bcmp_translate (d, d2, mcnt, translate, multibyte)
		    : memcmp (d, d2, mcnt))
		  {
		    d = dfail;
		    goto fail;
		  }
		d += mcnt, d2 += mcnt;
	      }
	  }
	  break;


	/* begline matches the empty string at the beginning of the string
	   (unless `not_bol' is set in `bufp'), and after newlines.  */
	case begline:
	  DEBUG_PRINT1 ("EXECUTING begline.\n");

	  if (AT_STRINGS_BEG (d))
	    {
	      if (!bufp->not_bol) break;
	    }
	  else
	    {
	      unsigned char c;
	      GET_CHAR_BEFORE_2 (c, d, string1, end1, string2, end2);
	      if (c == '\n')
		break;
	    }
	  /* In all other cases, we fail.  */
	  goto fail;


	/* endline is the dual of begline.  */
	case endline:
	  DEBUG_PRINT1 ("EXECUTING endline.\n");

	  if (AT_STRINGS_END (d))
	    {
	      if (!bufp->not_eol) break;
	    }
	  else
	    {
	      PREFETCH_NOLIMIT ();
	      if (*d == '\n')
		break;
	    }
	  goto fail;


	/* Match at the very beginning of the data.  */
	case begbuf:
	  DEBUG_PRINT1 ("EXECUTING begbuf.\n");
	  if (AT_STRINGS_BEG (d))
	    break;
	  goto fail;


	/* Match at the very end of the data.  */
	case endbuf:
	  DEBUG_PRINT1 ("EXECUTING endbuf.\n");
	  if (AT_STRINGS_END (d))
	    break;
	  goto fail;


	/* on_failure_keep_string_jump is used to optimize `.*\n'.  It
	   pushes NULL as the value for the string on the stack.  Then
	   `POP_FAILURE_POINT' will keep the current value for the
	   string, instead of restoring it.  To see why, consider
	   matching `foo\nbar' against `.*\n'.	The .* matches the foo;
	   then the . fails against the \n.  But the next thing we want
	   to do is match the \n against the \n; if we restored the
	   string value, we would be back at the foo.

	   Because this is used only in specific cases, we don't need to
	   check all the things that `on_failure_jump' does, to make
	   sure the right things get saved on the stack.  Hence we don't
	   share its code.  The only reason to push anything on the
	   stack at all is that otherwise we would have to change
	   `anychar's code to do something besides goto fail in this
	   case; that seems worse than this.  */
	case on_failure_keep_string_jump:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT3 ("EXECUTING on_failure_keep_string_jump %d (to %p):\n",
			mcnt, p + mcnt);

	  PUSH_FAILURE_POINT (p - 3, NULL);
	  break;

	  /* A nasty loop is introduced by the non-greedy *? and +?.
	     With such loops, the stack only ever contains one failure point
	     at a time, so that a plain on_failure_jump_loop kind of
	     cycle detection cannot work.  Worse yet, such a detection
	     can not only fail to detect a cycle, but it can also wrongly
	     detect a cycle (between different instantiations of the same
	     loop.
	     So the method used for those nasty loops is a little different:
	     We use a special cycle-detection-stack-frame which is pushed
	     when the on_failure_jump_nastyloop failure-point is *popped*.
	     This special frame thus marks the beginning of one iteration
	     through the loop and we can hence easily check right here
	     whether something matched between the beginning and the end of
	     the loop.  */
	case on_failure_jump_nastyloop:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT3 ("EXECUTING on_failure_jump_nastyloop %d (to %p):\n",
			mcnt, p + mcnt);

	  assert ((re_opcode_t)p[-4] == no_op);
	  CHECK_INFINITE_LOOP (p - 4, d);
	  PUSH_FAILURE_POINT (p - 3, d);
	  break;


	  /* Simple loop detecting on_failure_jump:  just check on the
	     failure stack if the same spot was already hit earlier.  */
	case on_failure_jump_loop:
	on_failure:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT3 ("EXECUTING on_failure_jump_loop %d (to %p):\n",
			mcnt, p + mcnt);

	  CHECK_INFINITE_LOOP (p - 3, d);
	  PUSH_FAILURE_POINT (p - 3, d);
	  break;


	/* Uses of on_failure_jump:

	   Each alternative starts with an on_failure_jump that points
	   to the beginning of the next alternative.  Each alternative
	   except the last ends with a jump that in effect jumps past
	   the rest of the alternatives.  (They really jump to the
	   ending jump of the following alternative, because tensioning
	   these jumps is a hassle.)

	   Repeats start with an on_failure_jump that points past both
	   the repetition text and either the following jump or
	   pop_failure_jump back to this on_failure_jump.  */
	case on_failure_jump:
	  IMMEDIATE_QUIT_CHECK;
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT3 ("EXECUTING on_failure_jump %d (to %p):\n",
			mcnt, p + mcnt);

	  PUSH_FAILURE_POINT (p -3, d);
	  break;

	/* This operation is used for greedy *.
	   Compare the beginning of the repeat with what in the
	   pattern follows its end. If we can establish that there
	   is nothing that they would both match, i.e., that we
	   would have to backtrack because of (as in, e.g., `a*a')
	   then we can use a non-backtracking loop based on
	   on_failure_keep_string_jump instead of on_failure_jump.  */
	case on_failure_jump_smart:
	  IMMEDIATE_QUIT_CHECK;
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT3 ("EXECUTING on_failure_jump_smart %d (to %p).\n",
			mcnt, p + mcnt);
	  {
	    re_char *p1 = p; /* Next operation.  */
	    /* Here, we discard `const', making re_match non-reentrant.  */
	    unsigned char *p2 = (unsigned char*) p + mcnt; /* Jump dest.  */
	    unsigned char *p3 = (unsigned char*) p - 3; /* opcode location.  */

	    p -= 3;		/* Reset so that we will re-execute the
				   instruction once it's been changed. */

	    EXTRACT_NUMBER (mcnt, p2 - 2);

	    /* Ensure this is a indeed the trivial kind of loop
	       we are expecting.  */
	    assert (skip_one_char (p1) == p2 - 3);
	    assert ((re_opcode_t) p2[-3] == jump && p2 + mcnt == p);
	    DEBUG_STATEMENT (debug += 2);
	    if (mutually_exclusive_p (bufp, p1, p2))
	      {
		/* Use a fast `on_failure_keep_string_jump' loop.  */
		DEBUG_PRINT1 ("  smart exclusive => fast loop.\n");
		*p3 = (unsigned char) on_failure_keep_string_jump;
		STORE_NUMBER (p2 - 2, mcnt + 3);
	      }
	    else
	      {
		/* Default to a safe `on_failure_jump' loop.  */
		DEBUG_PRINT1 ("  smart default => slow loop.\n");
		*p3 = (unsigned char) on_failure_jump;
	      }
	    DEBUG_STATEMENT (debug -= 2);
	  }
	  break;

	/* Unconditionally jump (without popping any failure points).  */
	case jump:
	unconditional_jump:
	  IMMEDIATE_QUIT_CHECK;
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);	/* Get the amount to jump.  */
	  DEBUG_PRINT2 ("EXECUTING jump %d ", mcnt);
	  p += mcnt;				/* Do the jump.	 */
	  DEBUG_PRINT2 ("(to %p).\n", p);
	  break;


	/* Have to succeed matching what follows at least n times.
	   After that, handle like `on_failure_jump'.  */
	case succeed_n:
	  /* Signedness doesn't matter since we only compare MCNT to 0.  */
	  EXTRACT_NUMBER (mcnt, p + 2);
	  DEBUG_PRINT2 ("EXECUTING succeed_n %d.\n", mcnt);

	  /* Originally, mcnt is how many times we HAVE to succeed.  */
	  if (mcnt != 0)
	    {
	      /* Here, we discard `const', making re_match non-reentrant.  */
	      unsigned char *p2 = (unsigned char*) p + 2; /* counter loc.  */
	      mcnt--;
	      p += 4;
	      PUSH_NUMBER (p2, mcnt);
	    }
	  else
	    /* The two bytes encoding mcnt == 0 are two no_op opcodes.  */
	    goto on_failure;
	  break;

	case jump_n:
	  /* Signedness doesn't matter since we only compare MCNT to 0.  */
	  EXTRACT_NUMBER (mcnt, p + 2);
	  DEBUG_PRINT2 ("EXECUTING jump_n %d.\n", mcnt);

	  /* Originally, this is how many times we CAN jump.  */
	  if (mcnt != 0)
	    {
	       /* Here, we discard `const', making re_match non-reentrant.  */
	      unsigned char *p2 = (unsigned char*) p + 2; /* counter loc.  */
	      mcnt--;
	      PUSH_NUMBER (p2, mcnt);
	      goto unconditional_jump;
	    }
	  /* If don't have to jump any more, skip over the rest of command.  */
	  else
	    p += 4;
	  break;

	case set_number_at:
	  {
	    unsigned char *p2;	/* Location of the counter.  */
	    DEBUG_PRINT1 ("EXECUTING set_number_at.\n");

	    EXTRACT_NUMBER_AND_INCR (mcnt, p);
	    /* Here, we discard `const', making re_match non-reentrant.  */
	    p2 = (unsigned char*) p + mcnt;
	    /* Signedness doesn't matter since we only copy MCNT's bits .  */
	    EXTRACT_NUMBER_AND_INCR (mcnt, p);
	    DEBUG_PRINT3 ("  Setting %p to %d.\n", p2, mcnt);
	    PUSH_NUMBER (p2, mcnt);
	    break;
	  }

	case wordbound:
	case notwordbound:
	  not = (re_opcode_t) *(p - 1) == notwordbound;
	  DEBUG_PRINT2 ("EXECUTING %swordbound.\n", not?"not":"");

	  /* We SUCCEED (or FAIL) in one of the following cases: */

	  /* Case 1: D is at the beginning or the end of string.  */
	  if (AT_STRINGS_BEG (d) || AT_STRINGS_END (d))
	    not = !not;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      re_wchar_t c1, c2;
	      int s1, s2;
#ifdef emacs
	      int offset = PTR_TO_OFFSET (d - 1);
	      int charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	      UPDATE_SYNTAX_TABLE (charpos);
#endif
	      GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
	      s1 = SYNTAX (c1);
#ifdef emacs
	      UPDATE_SYNTAX_TABLE_FORWARD (charpos + 1);
#endif
	      PREFETCH_NOLIMIT ();
	      c2 = RE_STRING_CHAR (d, dend - d);
	      s2 = SYNTAX (c2);

	      if (/* Case 2: Only one of S1 and S2 is Sword.  */
		  ((s1 == Sword) != (s2 == Sword))
		  /* Case 3: Both of S1 and S2 are Sword, and macro
		     WORD_BOUNDARY_P (C1, C2) returns nonzero.	*/
		  || ((s1 == Sword) && WORD_BOUNDARY_P (c1, c2)))
		not = !not;
	    }
	  if (not)
	    break;
	  else
	    goto fail;

	case wordbeg:
	  DEBUG_PRINT1 ("EXECUTING wordbeg.\n");

	  /* We FAIL in one of the following cases: */

	  /* Case 1: D is at the end of string.	 */
	  if (AT_STRINGS_END (d))
	    goto fail;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      re_wchar_t c1, c2;
	      int s1, s2;
#ifdef emacs
	      int offset = PTR_TO_OFFSET (d);
	      int charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	      UPDATE_SYNTAX_TABLE (charpos);
#endif
	      PREFETCH ();
	      c2 = RE_STRING_CHAR (d, dend - d);
	      s2 = SYNTAX (c2);
	
	      /* Case 2: S2 is not Sword. */
	      if (s2 != Sword)
		goto fail;

	      /* Case 3: D is not at the beginning of string ... */
	      if (!AT_STRINGS_BEG (d))
		{
		  GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
#ifdef emacs
		  UPDATE_SYNTAX_TABLE_BACKWARD (charpos - 1);
#endif
		  s1 = SYNTAX (c1);

		  /* ... and S1 is Sword, and WORD_BOUNDARY_P (C1, C2)
		     returns 0.	 */
		  if ((s1 == Sword) && !WORD_BOUNDARY_P (c1, c2))
		    goto fail;
		}
	    }
	  break;

	case wordend:
	  DEBUG_PRINT1 ("EXECUTING wordend.\n");

	  /* We FAIL in one of the following cases: */

	  /* Case 1: D is at the beginning of string.  */
	  if (AT_STRINGS_BEG (d))
	    goto fail;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      re_wchar_t c1, c2;
	      int s1, s2;
#ifdef emacs
	      int offset = PTR_TO_OFFSET (d) - 1;
	      int charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	      UPDATE_SYNTAX_TABLE (charpos);
#endif
	      GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
	      s1 = SYNTAX (c1);

	      /* Case 2: S1 is not Sword.  */
	      if (s1 != Sword)
		goto fail;

	      /* Case 3: D is not at the end of string ... */
	      if (!AT_STRINGS_END (d))
		{
		  PREFETCH_NOLIMIT ();
		  c2 = RE_STRING_CHAR (d, dend - d);
#ifdef emacs
		  UPDATE_SYNTAX_TABLE_FORWARD (charpos);
#endif
		  s2 = SYNTAX (c2);

		  /* ... and S2 is Sword, and WORD_BOUNDARY_P (C1, C2)
		     returns 0.	 */
		  if ((s2 == Sword) && !WORD_BOUNDARY_P (c1, c2))
	  goto fail;
		}
	    }
	  break;

	case syntaxspec:
	case notsyntaxspec:
	  not = (re_opcode_t) *(p - 1) == notsyntaxspec;
	  mcnt = *p++;
	  DEBUG_PRINT3 ("EXECUTING %ssyntaxspec %d.\n", not?"not":"", mcnt);
	  PREFETCH ();
#ifdef emacs
	  {
	    int offset = PTR_TO_OFFSET (d);
	    int pos1 = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	    UPDATE_SYNTAX_TABLE (pos1);
	  }
#endif
	  {
	    int len;
	    re_wchar_t c;

	    c = RE_STRING_CHAR_AND_LENGTH (d, dend - d, len);

	    if ((SYNTAX (c) != (enum syntaxcode) mcnt) ^ not)
	      goto fail;
	    d += len;
	  }
	  break;

#ifdef emacs
	case before_dot:
	  DEBUG_PRINT1 ("EXECUTING before_dot.\n");
	  if (PTR_BYTE_POS (d) >= PT_BYTE)
	    goto fail;
	  break;

	case at_dot:
	  DEBUG_PRINT1 ("EXECUTING at_dot.\n");
	  if (PTR_BYTE_POS (d) != PT_BYTE)
	    goto fail;
	  break;

	case after_dot:
	  DEBUG_PRINT1 ("EXECUTING after_dot.\n");
	  if (PTR_BYTE_POS (d) <= PT_BYTE)
	    goto fail;
	  break;

	case categoryspec:
	case notcategoryspec:
	  not = (re_opcode_t) *(p - 1) == notcategoryspec;
	  mcnt = *p++;
	  DEBUG_PRINT3 ("EXECUTING %scategoryspec %d.\n", not?"not":"", mcnt);
	  PREFETCH ();
	  {
	    int len;
	    re_wchar_t c;

	    c = RE_STRING_CHAR_AND_LENGTH (d, dend - d, len);

	    if ((!CHAR_HAS_CATEGORY (c, mcnt)) ^ not)
	      goto fail;
	    d += len;
	  }
	  break;

#endif /* emacs */

	default:
	  abort ();
	}
      continue;  /* Successfully executed one pattern command; keep going.  */


    /* We goto here if a matching operation fails. */
    fail:
      IMMEDIATE_QUIT_CHECK;
      if (!FAIL_STACK_EMPTY ())
	{
	  re_char *str, *pat;
	  /* A restart point is known.  Restore to that state.  */
	  DEBUG_PRINT1 ("\nFAIL:\n");
	  POP_FAILURE_POINT (str, pat);
	  switch (SWITCH_ENUM_CAST ((re_opcode_t) *pat++))
	    {
	    case on_failure_keep_string_jump:
	      assert (str == NULL);
	      goto continue_failure_jump;

	    case on_failure_jump_nastyloop:
	      assert ((re_opcode_t)pat[-2] == no_op);
	      PUSH_FAILURE_POINT (pat - 2, str);
	      /* Fallthrough */

	    case on_failure_jump_loop:
	    case on_failure_jump:
	    case succeed_n:
	      d = str;
	    continue_failure_jump:
	      EXTRACT_NUMBER_AND_INCR (mcnt, pat);
	      p = pat + mcnt;
	      break;

	    case no_op:
	      /* A special frame used for nastyloops. */
	      goto fail;

	    default:
	      abort();
	    }

	  assert (p >= bufp->buffer && p <= pend);

	  if (d >= string1 && d <= end1)
	    dend = end_match_1;
	}
      else
	break;   /* Matching at this starting point really fails.  */
    } /* for (;;) */

  if (best_regs_set)
    goto restore_best_regs;

  FREE_VARIABLES ();

  return -1;         			/* Failure to match.  */
} /* re_match_2 */

/* Subroutine definitions for re_match_2.  */

/* Return zero if TRANSLATE[S1] and TRANSLATE[S2] are identical for LEN
   bytes; nonzero otherwise.  */

static int
bcmp_translate (s1, s2, len, translate, multibyte)
     re_char *s1, *s2;
     register int len;
     RE_TRANSLATE_TYPE translate;
     const int multibyte;
{
  register re_char *p1 = s1, *p2 = s2;
  re_char *p1_end = s1 + len;
  re_char *p2_end = s2 + len;

  /* FIXME: Checking both p1 and p2 presumes that the two strings might have
     different lengths, but relying on a single `len' would break this. -sm  */
  while (p1 < p1_end && p2 < p2_end)
    {
      int p1_charlen, p2_charlen;
      re_wchar_t p1_ch, p2_ch;

      p1_ch = RE_STRING_CHAR_AND_LENGTH (p1, p1_end - p1, p1_charlen);
      p2_ch = RE_STRING_CHAR_AND_LENGTH (p2, p2_end - p2, p2_charlen);

      if (RE_TRANSLATE (translate, p1_ch)
	  != RE_TRANSLATE (translate, p2_ch))
	return 1;

      p1 += p1_charlen, p2 += p2_charlen;
    }

  if (p1 != p1_end || p2 != p2_end)
    return 1;

  return 0;
}

/* Entry points for GNU code.  */

/* re_compile_pattern is the GNU regular expression compiler: it
   compiles PATTERN (of length SIZE) and puts the result in BUFP.
   Returns 0 if the pattern was valid, otherwise an error string.

   Assumes the `allocated' (and perhaps `buffer') and `translate' fields
   are set in BUFP on entry.

   We call regex_compile to do the actual compilation.  */

const char *
re_compile_pattern (pattern, length, bufp)
     const char *pattern;
     size_t length;
     struct re_pattern_buffer *bufp;
{
  reg_errcode_t ret;

  /* GNU code is written to assume at least RE_NREGS registers will be set
     (and at least one extra will be -1).  */
  bufp->regs_allocated = REGS_UNALLOCATED;

  /* And GNU code determines whether or not to get register information
     by passing null for the REGS argument to re_match, etc., not by
     setting no_sub.  */
  bufp->no_sub = 0;

  ret = regex_compile ((re_char*) pattern, length, re_syntax_options, bufp);

  if (!ret)
    return NULL;
  return gettext (re_error_msgid[(int) ret]);
}
WEAK_ALIAS (__re_compile_pattern, re_compile_pattern)

/* Entry points compatible with 4.2 BSD regex library.  We don't define
   them unless specifically requested.  */

#if defined _REGEX_RE_COMP || defined _LIBC

/* BSD has one and only one pattern buffer.  */
static struct re_pattern_buffer re_comp_buf;

char *
# ifdef _LIBC
/* Make these definitions weak in libc, so POSIX programs can redefine
   these names if they don't use our functions, and still use
   regcomp/regexec below without link errors.  */
weak_function
# endif
re_comp (s)
    const char *s;
{
  reg_errcode_t ret;

  if (!s)
    {
      if (!re_comp_buf.buffer)
	/* Yes, we're discarding `const' here if !HAVE_LIBINTL.  */
	return (char *) gettext ("No previous regular expression");
      return 0;
    }

  if (!re_comp_buf.buffer)
    {
      re_comp_buf.buffer = (unsigned char *) malloc (200);
      if (re_comp_buf.buffer == NULL)
	/* Yes, we're discarding `const' here if !HAVE_LIBINTL.  */
	return (char *) gettext (re_error_msgid[(int) REG_ESPACE]);
      re_comp_buf.allocated = 200;

      re_comp_buf.fastmap = (char *) malloc (1 << BYTEWIDTH);
      if (re_comp_buf.fastmap == NULL)
	/* Yes, we're discarding `const' here if !HAVE_LIBINTL.  */
	return (char *) gettext (re_error_msgid[(int) REG_ESPACE]);
    }

  /* Since `re_exec' always passes NULL for the `regs' argument, we
     don't need to initialize the pattern buffer fields which affect it.  */

  ret = regex_compile (s, strlen (s), re_syntax_options, &re_comp_buf);

  if (!ret)
    return NULL;

  /* Yes, we're discarding `const' here if !HAVE_LIBINTL.  */
  return (char *) gettext (re_error_msgid[(int) ret]);
}


int
# ifdef _LIBC
weak_function
# endif
re_exec (s)
    const char *s;
{
  const int len = strlen (s);
  return
    0 <= re_search (&re_comp_buf, s, len, 0, len, (struct re_registers *) 0);
}
#endif /* _REGEX_RE_COMP */

/* POSIX.2 functions.  Don't define these for Emacs.  */

#ifndef emacs

/* regcomp takes a regular expression as a string and compiles it.

   PREG is a regex_t *.  We do not expect any fields to be initialized,
   since POSIX says we shouldn't.  Thus, we set

     `buffer' to the compiled pattern;
     `used' to the length of the compiled pattern;
     `syntax' to RE_SYNTAX_POSIX_EXTENDED if the
       REG_EXTENDED bit in CFLAGS is set; otherwise, to
       RE_SYNTAX_POSIX_BASIC;
     `fastmap' to an allocated space for the fastmap;
     `fastmap_accurate' to zero;
     `re_nsub' to the number of subexpressions in PATTERN.

   PATTERN is the address of the pattern string.

   CFLAGS is a series of bits which affect compilation.

     If REG_EXTENDED is set, we use POSIX extended syntax; otherwise, we
     use POSIX basic syntax.

     If REG_NEWLINE is set, then . and [^...] don't match newline.
     Also, regexec will try a match beginning after every newline.

     If REG_ICASE is set, then we considers upper- and lowercase
     versions of letters to be equivalent when matching.

     If REG_NOSUB is set, then when PREG is passed to regexec, that
     routine will report only success or failure, and nothing about the
     registers.

   It returns 0 if it succeeds, nonzero if it doesn't.  (See regex.h for
   the return codes and their meanings.)  */

int
regcomp (preg, pattern, cflags)
    regex_t *__restrict preg;
    const char *__restrict pattern;
    int cflags;
{
  reg_errcode_t ret;
  reg_syntax_t syntax
    = (cflags & REG_EXTENDED) ?
      RE_SYNTAX_POSIX_EXTENDED : RE_SYNTAX_POSIX_BASIC;

  /* regex_compile will allocate the space for the compiled pattern.  */
  preg->buffer = 0;
  preg->allocated = 0;
  preg->used = 0;

  /* Try to allocate space for the fastmap.  */
  preg->fastmap = (char *) malloc (1 << BYTEWIDTH);

  if (cflags & REG_ICASE)
    {
      unsigned i;

      preg->translate
	= (RE_TRANSLATE_TYPE) malloc (CHAR_SET_SIZE
				      * sizeof (*(RE_TRANSLATE_TYPE)0));
      if (preg->translate == NULL)
	return (int) REG_ESPACE;

      /* Map uppercase characters to corresponding lowercase ones.  */
      for (i = 0; i < CHAR_SET_SIZE; i++)
	preg->translate[i] = ISUPPER (i) ? TOLOWER (i) : i;
    }
  else
    preg->translate = NULL;

  /* If REG_NEWLINE is set, newlines are treated differently.  */
  if (cflags & REG_NEWLINE)
    { /* REG_NEWLINE implies neither . nor [^...] match newline.  */
      syntax &= ~RE_DOT_NEWLINE;
      syntax |= RE_HAT_LISTS_NOT_NEWLINE;
    }
  else
    syntax |= RE_NO_NEWLINE_ANCHOR;

  preg->no_sub = !!(cflags & REG_NOSUB);

  /* POSIX says a null character in the pattern terminates it, so we
     can use strlen here in compiling the pattern.  */
  ret = regex_compile ((re_char*) pattern, strlen (pattern), syntax, preg);

  /* POSIX doesn't distinguish between an unmatched open-group and an
     unmatched close-group: both are REG_EPAREN.  */
  if (ret == REG_ERPAREN)
    ret = REG_EPAREN;

  if (ret == REG_NOERROR && preg->fastmap)
    { /* Compute the fastmap now, since regexec cannot modify the pattern
	 buffer.  */
      re_compile_fastmap (preg);
      if (preg->can_be_null)
	{ /* The fastmap can't be used anyway.  */
	  free (preg->fastmap);
	  preg->fastmap = NULL;
	}
    }
  return (int) ret;
}
WEAK_ALIAS (__regcomp, regcomp)


/* regexec searches for a given pattern, specified by PREG, in the
   string STRING.

   If NMATCH is zero or REG_NOSUB was set in the cflags argument to
   `regcomp', we ignore PMATCH.  Otherwise, we assume PMATCH has at
   least NMATCH elements, and we set them to the offsets of the
   corresponding matched substrings.

   EFLAGS specifies `execution flags' which affect matching: if
   REG_NOTBOL is set, then ^ does not match at the beginning of the
   string; if REG_NOTEOL is set, then $ does not match at the end.

   We return 0 if we find a match and REG_NOMATCH if not.  */

int
regexec (preg, string, nmatch, pmatch, eflags)
    const regex_t *__restrict preg;
    const char *__restrict string;
    size_t nmatch;
    regmatch_t pmatch[];
    int eflags;
{
  int ret;
  struct re_registers regs;
  regex_t private_preg;
  int len = strlen (string);
  boolean want_reg_info = !preg->no_sub && nmatch > 0 && pmatch;

  private_preg = *preg;

  private_preg.not_bol = !!(eflags & REG_NOTBOL);
  private_preg.not_eol = !!(eflags & REG_NOTEOL);

  /* The user has told us exactly how many registers to return
     information about, via `nmatch'.  We have to pass that on to the
     matching routines.  */
  private_preg.regs_allocated = REGS_FIXED;

  if (want_reg_info)
    {
      regs.num_regs = nmatch;
      regs.start = TALLOC (nmatch * 2, regoff_t);
      if (regs.start == NULL)
	return (int) REG_NOMATCH;
      regs.end = regs.start + nmatch;
    }

  /* Instead of using not_eol to implement REG_NOTEOL, we could simply
     pass (&private_preg, string, len + 1, 0, len, ...) pretending the string
     was a little bit longer but still only matching the real part.
     This works because the `endline' will check for a '\n' and will find a
     '\0', correctly deciding that this is not the end of a line.
     But it doesn't work out so nicely for REG_NOTBOL, since we don't have
     a convenient '\0' there.  For all we know, the string could be preceded
     by '\n' which would throw things off.  */

  /* Perform the searching operation.  */
  ret = re_search (&private_preg, string, len,
		   /* start: */ 0, /* range: */ len,
		   want_reg_info ? &regs : (struct re_registers *) 0);

  /* Copy the register information to the POSIX structure.  */
  if (want_reg_info)
    {
      if (ret >= 0)
	{
	  unsigned r;

	  for (r = 0; r < nmatch; r++)
	    {
	      pmatch[r].rm_so = regs.start[r];
	      pmatch[r].rm_eo = regs.end[r];
	    }
	}

      /* If we needed the temporary register info, free the space now.  */
      free (regs.start);
    }

  /* We want zero return to mean success, unlike `re_search'.  */
  return ret >= 0 ? (int) REG_NOERROR : (int) REG_NOMATCH;
}
WEAK_ALIAS (__regexec, regexec)


/* Returns a message corresponding to an error code, ERRCODE, returned
   from either regcomp or regexec.   We don't use PREG here.  */

size_t
regerror (errcode, preg, errbuf, errbuf_size)
    int errcode;
    const regex_t *preg;
    char *errbuf;
    size_t errbuf_size;
{
  const char *msg;
  size_t msg_size;

  if (errcode < 0
      || errcode >= (sizeof (re_error_msgid) / sizeof (re_error_msgid[0])))
    /* Only error codes returned by the rest of the code should be passed
       to this routine.  If we are given anything else, or if other regex
       code generates an invalid error code, then the program has a bug.
       Dump core so we can fix it.  */
    abort ();

  msg = gettext (re_error_msgid[errcode]);

  msg_size = strlen (msg) + 1; /* Includes the null.  */

  if (errbuf_size != 0)
    {
      if (msg_size > errbuf_size)
	{
	  strncpy (errbuf, msg, errbuf_size - 1);
	  errbuf[errbuf_size - 1] = 0;
	}
      else
	strcpy (errbuf, msg);
    }

  return msg_size;
}
WEAK_ALIAS (__regerror, regerror)


/* Free dynamically allocated space used by PREG.  */

void
regfree (preg)
    regex_t *preg;
{
  if (preg->buffer != NULL)
    free (preg->buffer);
  preg->buffer = NULL;

  preg->allocated = 0;
  preg->used = 0;

  if (preg->fastmap != NULL)
    free (preg->fastmap);
  preg->fastmap = NULL;
  preg->fastmap_accurate = 0;

  if (preg->translate != NULL)
    free (preg->translate);
  preg->translate = NULL;
}
WEAK_ALIAS (__regfree, regfree)

#endif /* not emacs  */
