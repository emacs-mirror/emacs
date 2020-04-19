/* Emacs regular expression matching and search

   Copyright (C) 1993-2020 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* TODO:
   - structure the opcode space into opcode+flag.
   - replace (succeed_n + jump_n + set_number_at) with something that doesn't
     need to modify the compiled regexp so that re_search can be reentrant.
   - get rid of on_failure_jump_smart by doing the optimization in re_comp
     rather than at run-time, so that re_search can be reentrant.
*/

#include <config.h>

#include "regex-emacs.h"

#include <stdlib.h>

#include "character.h"
#include "buffer.h"
#include "syntax.h"
#include "category.h"

/* Maximum number of duplicates an interval can allow.  Some systems
   define this in other header files, but we want our value, so remove
   any previous define.  Repeat counts are stored in opcodes as 2-byte
   unsigned integers.  */
#ifdef RE_DUP_MAX
# undef RE_DUP_MAX
#endif
#define RE_DUP_MAX (0xffff)

/* Make syntax table lookup grant data in gl_state.  */
#define SYNTAX(c) syntax_property (c, 1)

/* Convert the pointer to the char to BEG-based offset from the start.  */
#define PTR_TO_OFFSET(d) POS_AS_IN_BUFFER (POINTER_TO_OFFSET (d))
/* Strings are 0-indexed, buffers are 1-indexed; pun on the boolean
   result to get the right base index.  */
#define POS_AS_IN_BUFFER(p)                                    \
  ((p) + (NILP (gl_state.object) || BUFFERP (gl_state.object)))

#define RE_MULTIBYTE_P(bufp) ((bufp)->multibyte)
#define RE_TARGET_MULTIBYTE_P(bufp) ((bufp)->target_multibyte)
#define RE_STRING_CHAR(p, multibyte) \
  (multibyte ? STRING_CHAR (p) : *(p))
#define RE_STRING_CHAR_AND_LENGTH(p, len, multibyte) \
  (multibyte ? STRING_CHAR_AND_LENGTH (p, len) : ((len) = 1, *(p)))

#define RE_CHAR_TO_MULTIBYTE(c) UNIBYTE_TO_CHAR (c)

#define RE_CHAR_TO_UNIBYTE(c) CHAR_TO_BYTE_SAFE (c)

/* Set C a (possibly converted to multibyte) character before P.  P
   points into a string which is the virtual concatenation of STR1
   (which ends at END1) or STR2 (which ends at END2).  */
#define GET_CHAR_BEFORE_2(c, p, str1, end1, str2, end2)			     \
  do {									     \
    if (target_multibyte)						     \
      {									     \
	re_char *dtemp = (p) == (str2) ? (end1) : (p);			     \
	re_char *dlimit = (p) > (str2) && (p) <= (end2) ? (str2) : (str1);   \
	while (dtemp-- > dlimit && !CHAR_HEAD_P (*dtemp))		     \
	  continue;							     \
	c = STRING_CHAR (dtemp);					     \
      }									     \
    else								     \
      {									     \
	(c = ((p) == (str2) ? (end1) : (p))[-1]);			     \
	(c) = RE_CHAR_TO_MULTIBYTE (c);					     \
      }									     \
  } while (false)

/* Set C a (possibly converted to multibyte) character at P, and set
   LEN to the byte length of that character.  */
#define GET_CHAR_AFTER(c, p, len)		\
  do {						\
    if (target_multibyte)			\
      (c) = STRING_CHAR_AND_LENGTH (p, len);	\
    else					\
      {						\
	(c) = *p;				\
	len = 1;				\
	(c) = RE_CHAR_TO_MULTIBYTE (c);		\
      }						\
   } while (false)

/* 1 if C is an ASCII character.  */
#define IS_REAL_ASCII(c) ((c) < 0200)

/* 1 if C is a unibyte character.  */
#define ISUNIBYTE(c) (SINGLE_BYTE_CHAR_P ((c)))

/* The Emacs definitions should not be directly affected by locales.  */

/* In Emacs, these are only used for single-byte characters.  */
#define ISDIGIT(c) ((c) >= '0' && (c) <= '9')
#define ISCNTRL(c) ((c) < ' ')
#define ISXDIGIT(c) (0 <= char_hexdigit (c))

/* The rest must handle multibyte characters.  */

#define ISBLANK(c) (IS_REAL_ASCII (c)			\
                     ? ((c) == ' ' || (c) == '\t')      \
                     : blankp (c))

#define ISGRAPH(c) (SINGLE_BYTE_CHAR_P (c)				\
		     ? (c) > ' ' && !((c) >= 0177 && (c) <= 0240)	\
		     : graphicp (c))

#define ISPRINT(c) (SINGLE_BYTE_CHAR_P (c)				\
		    ? (c) >= ' ' && !((c) >= 0177 && (c) <= 0237)	\
		     : printablep (c))

#define ISALNUM(c) (IS_REAL_ASCII (c)			\
		    ? (((c) >= 'a' && (c) <= 'z')	\
		       || ((c) >= 'A' && (c) <= 'Z')	\
		       || ((c) >= '0' && (c) <= '9'))	\
		    : alphanumericp (c))

#define ISALPHA(c) (IS_REAL_ASCII (c)			\
		    ? (((c) >= 'a' && (c) <= 'z')	\
		       || ((c) >= 'A' && (c) <= 'Z'))	\
		    : alphabeticp (c))

#define ISLOWER(c) lowercasep (c)

#define ISPUNCT(c) (IS_REAL_ASCII (c)				\
		    ? ((c) > ' ' && (c) < 0177			\
		       && !(((c) >= 'a' && (c) <= 'z')		\
		            || ((c) >= 'A' && (c) <= 'Z')	\
		            || ((c) >= '0' && (c) <= '9')))	\
		    : SYNTAX (c) != Sword)

#define ISSPACE(c) (SYNTAX (c) == Swhitespace)

#define ISUPPER(c) uppercasep (c)

#define ISWORD(c) (SYNTAX (c) == Sword)

/* Use alloca instead of malloc.  This is because using malloc in
   re_search* or re_match* could cause memory leaks when C-g is used
   in Emacs (note that SAFE_ALLOCA could also call malloc, but does so
   via 'record_xmalloc' which uses 'unwind_protect' to ensure the
   memory is freed even in case of non-local exits); also, malloc is
   slower and causes storage fragmentation.  On the other hand, malloc
   is more portable, and easier to debug.

   Because we sometimes use alloca, some routines have to be macros,
   not functions -- 'alloca'-allocated space disappears at the end of the
   function it is called in.  */

/* This may be adjusted in main(), if the stack is successfully grown.  */
ptrdiff_t emacs_re_safe_alloca = MAX_ALLOCA;
/* Like USE_SAFE_ALLOCA, but use emacs_re_safe_alloca.  */
#define REGEX_USE_SAFE_ALLOCA					       \
  USE_SAFE_ALLOCA; sa_avail = emacs_re_safe_alloca

/* Assumes a 'char *destination' variable.  */
#define REGEX_REALLOCATE(source, osize, nsize)				\
  (destination = SAFE_ALLOCA (nsize),					\
   memcpy (destination, source, osize))

/* True if 'size1' is non-NULL and PTR is pointing anywhere inside
   'string1' or just past its end.  This works if PTR is NULL, which is
   a good thing.  */
#define FIRST_STRING_P(ptr)					\
  (size1 && string1 <= (ptr) && (ptr) <= string1 + size1)

#define BYTEWIDTH 8 /* In bits.  */

/* Type of source-pattern and string chars.  */
typedef const unsigned char re_char;

static void re_compile_fastmap (struct re_pattern_buffer *);
static ptrdiff_t re_match_2_internal (struct re_pattern_buffer *bufp,
				     re_char *string1, ptrdiff_t size1,
				     re_char *string2, ptrdiff_t size2,
				     ptrdiff_t pos,
				     struct re_registers *regs,
				     ptrdiff_t stop);

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
	   number, in the range 0 to one less than 're_nsub' in the
	   pattern buffer.  */
  stop_memory,

	/* Match a duplicate of something remembered. Followed by one
	   byte containing the register number.  */
  duplicate,

	/* Fail unless at beginning of line.  */
  begline,

	/* Fail unless at end of line.  */
  endline,

	/* Succeeds if at beginning of buffer.  */
  begbuf,

	/* Analogously, for end of buffer/string.  */
  endbuf,

	/* Followed by two byte relative address to which to jump.  */
  jump,

	/* Followed by two-byte relative address of place to resume at
	   in case of failure.  */
  on_failure_jump,

	/* Like on_failure_jump, but pushes a placeholder instead of the
	   current string position when executed.  */
  on_failure_keep_string_jump,

	/* Just like 'on_failure_jump', except that it checks that we
	   don't get stuck in an infinite loop (matching an empty string
	   indefinitely).  */
  on_failure_jump_loop,

	/* Just like 'on_failure_jump_loop', except that it checks for
	   a different kind of loop (the kind that shows up with non-greedy
	   operators).  This operation has to be immediately preceded
	   by a 'no_op'.  */
  on_failure_jump_nastyloop,

	/* A smart 'on_failure_jump' used for greedy * and + operators.
	   It analyzes the loop before which it is put and if the
	   loop does not require backtracking, it changes itself to
	   'on_failure_keep_string_jump' and short-circuits the loop,
	   else it just defaults to changing itself into 'on_failure_jump'.
	   It assumes that it is pointing to just past a 'jump'.  */
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
	   subsequent two-byte number.  The address *includes* the two
	   bytes of number.  */
  set_number_at,

  wordbeg,	/* Succeeds if at word beginning.  */
  wordend,	/* Succeeds if at word end.  */

  wordbound,	/* Succeeds if at a word boundary.  */
  notwordbound,	/* Succeeds if not at a word boundary.  */

  symbeg,       /* Succeeds if at symbol beginning.  */
  symend,       /* Succeeds if at symbol end.  */

	/* Matches any character whose syntax is specified.  Followed by
	   a byte which contains a syntax code, e.g., Sword.  */
  syntaxspec,

	/* Matches any character whose syntax is not that specified.  */
  notsyntaxspec,

  at_dot,	/* Succeeds if at point.  */

  /* Matches any character whose category-set contains the specified
     category.  The operator is followed by a byte which contains a
     category code (mnemonic ASCII character).  */
  categoryspec,

  /* Matches any character whose category-set does not contain the
     specified category.  The operator is followed by a byte which
     contains the category code (mnemonic ASCII character).  */
  notcategoryspec
} re_opcode_t;

/* Common operations on the compiled pattern.  */

/* Store NUMBER in two contiguous bytes starting at DESTINATION.  */

#define STORE_NUMBER(destination, number)				\
  do {									\
    (destination)[0] = (number) & 0377;					\
    (destination)[1] = (number) >> 8;					\
  } while (false)

/* Same as STORE_NUMBER, except increment DESTINATION to
   the byte after where the number is stored.  Therefore, DESTINATION
   must be an lvalue.  */

#define STORE_NUMBER_AND_INCR(destination, number)			\
  do {									\
    STORE_NUMBER (destination, number);					\
    (destination) += 2;							\
  } while (false)

/* Put into DESTINATION a number stored in two contiguous bytes starting
   at SOURCE.  */

#define EXTRACT_NUMBER(destination, source)				\
  ((destination) = extract_number (source))

static int
extract_number (re_char *source)
{
  signed char leading_byte = source[1];
  return leading_byte * 256 + source[0];
}

/* Same as EXTRACT_NUMBER, except increment SOURCE to after the number.
   SOURCE must be an lvalue.  */

#define EXTRACT_NUMBER_AND_INCR(destination, source)			\
  ((destination) = extract_number_and_incr (&source))

static int
extract_number_and_incr (re_char **source)
{
  int num = extract_number (*source);
  *source += 2;
  return num;
}

/* Store a multibyte character in three contiguous bytes starting
   DESTINATION, and increment DESTINATION to the byte after where the
   character is stored.  Therefore, DESTINATION must be an lvalue.  */

#define STORE_CHARACTER_AND_INCR(destination, character)	\
  do {								\
    (destination)[0] = (character) & 0377;			\
    (destination)[1] = ((character) >> 8) & 0377;		\
    (destination)[2] = (character) >> 16;			\
    (destination) += 3;						\
  } while (false)

/* Put into DESTINATION a character stored in three contiguous bytes
   starting at SOURCE.  */

#define EXTRACT_CHARACTER(destination, source)	\
  do {						\
    (destination) = ((source)[0]		\
		     | ((source)[1] << 8)	\
		     | ((source)[2] << 16));	\
  } while (false)


/* Macros for charset. */

/* Size of bitmap of charset P in bytes.  P is a start of charset,
   i.e. *P is (re_opcode_t) charset or (re_opcode_t) charset_not.  */
#define CHARSET_BITMAP_SIZE(p) ((p)[1] & 0x7F)

/* Nonzero if charset P has range table.  */
#define CHARSET_RANGE_TABLE_EXISTS_P(p)	 (((p)[1] & 0x80) != 0)

/* Return the address of range table of charset P.  But not the start
   of table itself, but the before where the number of ranges is
   stored.  '2 +' means to skip re_opcode_t and size of bitmap,
   and the 2 bytes of flags at the start of the range table.  */
#define CHARSET_RANGE_TABLE(p) (&(p)[4 + CHARSET_BITMAP_SIZE (p)])

/* Extract the bit flags that start a range table.  */
#define CHARSET_RANGE_TABLE_BITS(p)		\
  ((p)[2 + CHARSET_BITMAP_SIZE (p)]		\
   + (p)[3 + CHARSET_BITMAP_SIZE (p)] * 0x100)

/* Return the address of end of RANGE_TABLE.  COUNT is number of
   ranges (which is a pair of (start, end)) in the RANGE_TABLE.  '* 2'
   is start of range and end of range.  '* 3' is size of each start
   and end.  */
#define CHARSET_RANGE_TABLE_END(range_table, count)	\
  ((range_table) + (count) * 2 * 3)

/* If REGEX_EMACS_DEBUG is defined, print many voluminous messages
   (if the variable regex_emacs_debug is positive).  */

#ifdef REGEX_EMACS_DEBUG

/* Use standard I/O for debugging.  */
# include "sysstdio.h"

static int regex_emacs_debug = -100000;

# define DEBUG_STATEMENT(e) e
# define DEBUG_PRINT(...)                                       \
  if (regex_emacs_debug > 0) fprintf (stderr, __VA_ARGS__)
# define DEBUG_COMPILES_ARGUMENTS
# define DEBUG_PRINT_COMPILED_PATTERN(p, s, e)				\
  if (regex_emacs_debug > 0) print_partial_compiled_pattern (s, e)
# define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)			\
  if (regex_emacs_debug > 0) print_double_string (w, s1, sz1, s2, sz2)

static void
debug_putchar (int c)
{
  if (c >= 32 && c <= 126)
    putc (c, stderr);
  else
    {
      unsigned int uc = c;
      fprintf (stderr, "{%02x}", uc);
    }
}

/* Print the fastmap in human-readable form.  */

static void
print_fastmap (char *fastmap)
{
  bool was_a_range = false;
  int i = 0;

  while (i < (1 << BYTEWIDTH))
    {
      if (fastmap[i++])
	{
	  was_a_range = false;
	  debug_putchar (i - 1);
	  while (i < (1 << BYTEWIDTH)  &&  fastmap[i])
	    {
	      was_a_range = true;
	      i++;
	    }
	  if (was_a_range)
	    {
	      debug_putchar ('-');
	      debug_putchar (i - 1);
	    }
	}
    }
  putc ('\n', stderr);
}


/* Print a compiled pattern string in human-readable form, starting at
   the START pointer into it and ending just before the pointer END.  */

static void
print_partial_compiled_pattern (re_char *start, re_char *end)
{
  int mcnt, mcnt2;
  re_char *p = start;
  re_char *pend = end;

  if (start == NULL)
    {
      fputs ("(null)\n", stderr);
      return;
    }

  /* Loop over pattern commands.  */
  while (p < pend)
    {
      fprintf (stderr, "%td:\t", p - start);

      switch ((re_opcode_t) *p++)
	{
	case no_op:
	  fputs ("/no_op", stderr);
	  break;

	case succeed:
	  fputs ("/succeed", stderr);
	  break;

	case exactn:
	  mcnt = *p++;
	  fprintf (stderr, "/exactn/%d", mcnt);
	  do
	    {
	      debug_putchar ('/');
	      debug_putchar (*p++);
	    }
	  while (--mcnt);
	  break;

	case start_memory:
	  fprintf (stderr, "/start_memory/%d", *p++);
	  break;

	case stop_memory:
	  fprintf (stderr, "/stop_memory/%d", *p++);
	  break;

	case duplicate:
	  fprintf (stderr, "/duplicate/%d", *p++);
	  break;

	case anychar:
	  fputs ("/anychar", stderr);
	  break;

	case charset:
	case charset_not:
	  {
	    int c, last = -100;
	    bool in_range = false;
	    int length = CHARSET_BITMAP_SIZE (p - 1);
	    bool has_range_table = CHARSET_RANGE_TABLE_EXISTS_P (p - 1);

	    fprintf (stderr, "/charset [%s",
		     (re_opcode_t) *(p - 1) == charset_not ? "^" : "");

	    if (p + *p >= pend)
	      fputs (" !extends past end of pattern! ", stderr);

	    for (c = 0; c < 256; c++)
	      if (c / 8 < length
		  && (p[1 + (c/8)] & (1 << (c % 8))))
		{
		  /* Are we starting a range?  */
		  if (last + 1 == c && ! in_range)
		    {
		      debug_putchar ('-');
		      in_range = true;
		    }
		  /* Have we broken a range?  */
		  else if (last + 1 != c && in_range)
		    {
		      debug_putchar (last);
		      in_range = false;
		    }

		  if (! in_range)
		    debug_putchar (c);

		  last = c;
	      }

	    if (in_range)
	      debug_putchar (last);

	    debug_putchar (']');

	    p += 1 + length;

	    if (has_range_table)
	      {
		int count;
		fputs ("has-range-table", stderr);

		/* ??? Should print the range table; for now, just skip it.  */
		p += 2;		/* skip range table bits */
		EXTRACT_NUMBER_AND_INCR (count, p);
		p = CHARSET_RANGE_TABLE_END (p, count);
	      }
	  }
	  break;

	case begline:
	  fputs ("/begline", stderr);
	  break;

	case endline:
	  fputs ("/endline", stderr);
	  break;

	case on_failure_jump:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  fprintf (stderr, "/on_failure_jump to %td", p + mcnt - start);
	  break;

	case on_failure_keep_string_jump:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  fprintf (stderr, "/on_failure_keep_string_jump to %td",
		   p + mcnt - start);
	  break;

	case on_failure_jump_nastyloop:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  fprintf (stderr, "/on_failure_jump_nastyloop to %td",
		   p + mcnt - start);
	  break;

	case on_failure_jump_loop:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  fprintf (stderr, "/on_failure_jump_loop to %td",
		   p + mcnt - start);
	  break;

	case on_failure_jump_smart:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  fprintf (stderr, "/on_failure_jump_smart to %td",
		   p + mcnt - start);
	  break;

	case jump:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  fprintf (stderr, "/jump to %td", p + mcnt - start);
	  break;

	case succeed_n:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  EXTRACT_NUMBER_AND_INCR (mcnt2, p);
	  fprintf (stderr, "/succeed_n to %td, %d times",
		   p - 2 + mcnt - start, mcnt2);
	  break;

	case jump_n:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  EXTRACT_NUMBER_AND_INCR (mcnt2, p);
	  fprintf (stderr, "/jump_n to %td, %d times",
		   p - 2 + mcnt - start, mcnt2);
	  break;

	case set_number_at:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  EXTRACT_NUMBER_AND_INCR (mcnt2, p);
	  fprintf (stderr, "/set_number_at location %td to %d",
		   p - 2 + mcnt - start, mcnt2);
	  break;

	case wordbound:
	  fputs ("/wordbound", stderr);
	  break;

	case notwordbound:
	  fputs ("/notwordbound", stderr);
	  break;

	case wordbeg:
	  fputs ("/wordbeg", stderr);
	  break;

	case wordend:
	  fputs ("/wordend", stderr);
	  break;

	case symbeg:
	  fputs ("/symbeg", stderr);
	  break;

	case symend:
	  fputs ("/symend", stderr);
	  break;

	case syntaxspec:
	  fputs ("/syntaxspec", stderr);
	  mcnt = *p++;
	  fprintf (stderr, "/%d", mcnt);
	  break;

	case notsyntaxspec:
	  fputs ("/notsyntaxspec", stderr);
	  mcnt = *p++;
	  fprintf (stderr, "/%d", mcnt);
	  break;

	case at_dot:
	  fputs ("/at_dot", stderr);
	  break;

	case categoryspec:
	  fputs ("/categoryspec", stderr);
	  mcnt = *p++;
	  fprintf (stderr, "/%d", mcnt);
	  break;

	case notcategoryspec:
	  fputs ("/notcategoryspec", stderr);
	  mcnt = *p++;
	  fprintf (stderr, "/%d", mcnt);
	  break;

	case begbuf:
	  fputs ("/begbuf", stderr);
	  break;

	case endbuf:
	  fputs ("/endbuf", stderr);
	  break;

	default:
	  fprintf (stderr, "?%d", *(p-1));
	}

      putc ('\n', stderr);
    }

  fprintf (stderr, "%td:\tend of pattern.\n", p - start);
}


static void
print_compiled_pattern (struct re_pattern_buffer *bufp)
{
  re_char *buffer = bufp->buffer;

  print_partial_compiled_pattern (buffer, buffer + bufp->used);
  fprintf (stderr, "%td bytes used/%td bytes allocated.\n",
           bufp->used, bufp->allocated);

  if (bufp->fastmap_accurate && bufp->fastmap)
    {
      fputs ("fastmap: ", stderr);
      print_fastmap (bufp->fastmap);
    }

  fprintf (stderr, "re_nsub: %td\t", bufp->re_nsub);
  fprintf (stderr, "regs_alloc: %d\t", bufp->regs_allocated);
  fprintf (stderr, "can_be_null: %d\n", bufp->can_be_null);
  /* Perhaps we should print the translate table?  */
}


static void
print_double_string (re_char *where, re_char *string1, ptrdiff_t size1,
		     re_char *string2, ptrdiff_t size2)
{
  if (where == NULL)
    fputs ("(null)", stderr);
  else
    {
      int i;
      if (FIRST_STRING_P (where))
	{
	  for (i = 0; i < string1 + size1 - where; i++)
	    debug_putchar (where[i]);
	  where = string2;
	}

      for (i = 0; i < string2 + size2 - where; i++)
        debug_putchar (where[i]);
    }
}

#else /* not REGEX_EMACS_DEBUG */

# define DEBUG_STATEMENT(e)
# define DEBUG_PRINT(...)
# define DEBUG_PRINT_COMPILED_PATTERN(p, s, e)
# define DEBUG_PRINT_DOUBLE_STRING(w, s1, sz1, s2, sz2)

#endif /* not REGEX_EMACS_DEBUG */

typedef enum
{
  REG_NOERROR = 0,	/* Success.  */
  REG_NOMATCH,		/* Didn't find a match (for regexec).  */

  /* POSIX regcomp return error codes.  (In the order listed in the
     standard.)  An older version of this code supported the POSIX
     API; this version continues to use these names internally.  */
  REG_BADPAT,		/* Invalid pattern.  */
  REG_ECOLLATE,		/* Not implemented.  */
  REG_ECTYPE,		/* Invalid character class name.  */
  REG_EESCAPE,		/* Trailing backslash.  */
  REG_ESUBREG,		/* Invalid back reference.  */
  REG_EBRACK,		/* Unmatched left bracket.  */
  REG_EPAREN,		/* Parenthesis imbalance.  */
  REG_EBRACE,		/* Unmatched \{.  */
  REG_BADBR,		/* Invalid contents of \{\}.  */
  REG_ERANGE,		/* Invalid range end.  */
  REG_ESPACE,		/* Ran out of memory.  */
  REG_BADRPT,		/* No preceding re for repetition op.  */

  /* Error codes we've added.  */
  REG_EEND,		/* Premature end.  */
  REG_ESIZE,		/* Compiled pattern bigger than 2^16 bytes.  */
  REG_ERPAREN,		/* Unmatched ) or \); not returned from regcomp.  */
  REG_ERANGEX,		/* Range striding over charsets.  */
  REG_ESIZEBR           /* n or m too big in \{n,m\} */
} reg_errcode_t;

static const char *re_error_msgid[] =
  {
   [REG_NOERROR] = "Success",
   [REG_NOMATCH] = "No match",
   [REG_BADPAT] = "Invalid regular expression",
   [REG_ECOLLATE] = "Invalid collation character",
   [REG_ECTYPE] = "Invalid character class name",
   [REG_EESCAPE] = "Trailing backslash",
   [REG_ESUBREG] = "Invalid back reference",
   [REG_EBRACK] = "Unmatched [ or [^",
   [REG_EPAREN] = "Unmatched ( or \\(",
   [REG_EBRACE] = "Unmatched \\{",
   [REG_BADBR] = "Invalid content of \\{\\}",
   [REG_ERANGE] = "Invalid range end",
   [REG_ESPACE] = "Memory exhausted",
   [REG_BADRPT] = "Invalid preceding regular expression",
   [REG_EEND] = "Premature end of regular expression",
   [REG_ESIZE] = "Regular expression too big",
   [REG_ERPAREN] = "Unmatched ) or \\)",
   [REG_ERANGEX ] = "Range striding over charsets",
   [REG_ESIZEBR ] = "Invalid content of \\{\\}",
  };

/* For 'regs_allocated'.  */
enum { REGS_UNALLOCATED, REGS_REALLOCATE, REGS_FIXED };

/* If 'regs_allocated' is REGS_UNALLOCATED in the pattern buffer,
   're_match_2' returns information about at least this many registers
   the first time a 'regs' structure is passed.  */
enum { RE_NREGS = 30 };

/* The searching and matching functions allocate memory for the
   failure stack and registers.  Otherwise searching and matching
   routines would have much smaller memory resources at their
   disposal, and therefore might fail to handle complex regexps.  */

/* Failure stack declarations and macros; both re_compile_fastmap and
   re_match_2 use a failure stack.  These have to be macros because of
   SAFE_ALLOCA.  */


/* Approximate number of failure points for which to initially allocate space
   when matching.  If this number is exceeded, we allocate more
   space, so it is not a hard limit.  */
#define INIT_FAILURE_ALLOC 20

/* Roughly the maximum number of failure points on the stack.  Would be
   exactly that if failure always used TYPICAL_FAILURE_SIZE items.
   This is a variable only so users of regex can assign to it; we never
   change it ourselves.  We always multiply it by TYPICAL_FAILURE_SIZE
   before using it, so it should probably be a byte-count instead.  */
/* Note that 4400 was enough to cause a crash on Alpha OSF/1,
   whose default stack limit is 2mb.  In order for a larger
   value to work reliably, you have to try to make it accord
   with the process stack limit.  */
ptrdiff_t emacs_re_max_failures = 40000;

union fail_stack_elt
{
  re_char *pointer;
  intptr_t integer;
};

typedef union fail_stack_elt fail_stack_elt_t;

typedef struct
{
  fail_stack_elt_t *stack;
  ptrdiff_t size;
  ptrdiff_t avail;	/* Offset of next open position.  */
  ptrdiff_t frame;	/* Offset of the cur constructed frame.  */
} fail_stack_type;

#define FAIL_STACK_EMPTY()     (fail_stack.frame == 0)


/* Define macros to initialize and free the failure stack.  */

#define INIT_FAIL_STACK()						\
  do {									\
    fail_stack.stack =							\
      SAFE_ALLOCA (INIT_FAILURE_ALLOC * TYPICAL_FAILURE_SIZE		\
		   * sizeof (fail_stack_elt_t));			\
    fail_stack.size = INIT_FAILURE_ALLOC;				\
    fail_stack.avail = 0;						\
    fail_stack.frame = 0;						\
  } while (false)


/* Double the size of FAIL_STACK, up to a limit
   which allows approximately 'emacs_re_max_failures' items.

   Return 1 if succeeds, and 0 if either ran out of memory
   allocating space for it or it was already too large.

   REGEX_REALLOCATE requires 'destination' be declared.   */

/* Factor to increase the failure stack size by.
   This used to be 2, but 2 was too wasteful
   because the old discarded stacks added up to as much space
   were as ultimate, maximum-size stack.  */
#define FAIL_STACK_GROWTH_FACTOR 4

#define GROW_FAIL_STACK(fail_stack)					\
  (((fail_stack).size >= emacs_re_max_failures * TYPICAL_FAILURE_SIZE)        \
   ? 0									\
   : ((fail_stack).stack						\
      = REGEX_REALLOCATE ((fail_stack).stack,				\
	  (fail_stack).size * sizeof (fail_stack_elt_t),		\
          min (emacs_re_max_failures * TYPICAL_FAILURE_SIZE,                  \
               ((fail_stack).size * FAIL_STACK_GROWTH_FACTOR))          \
          * sizeof (fail_stack_elt_t)),                                 \
      ((fail_stack).size						\
       = (min (emacs_re_max_failures * TYPICAL_FAILURE_SIZE,		\
	       ((fail_stack).size * FAIL_STACK_GROWTH_FACTOR)))),	\
      1))


/* Push a pointer value onto the failure stack.
   Assumes the variable 'fail_stack'.  Probably should only
   be called from within 'PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_POINTER(item)					\
  fail_stack.stack[fail_stack.avail++].pointer = (item)

/* This pushes an integer-valued item onto the failure stack.
   Assumes the variable 'fail_stack'.  Probably should only
   be called from within 'PUSH_FAILURE_POINT'.  */
#define PUSH_FAILURE_INT(item)					\
  fail_stack.stack[fail_stack.avail++].integer = (item)

/* These POP... operations complement the PUSH... operations.
   All assume that 'fail_stack' is nonempty.  */
#define POP_FAILURE_POINTER() fail_stack.stack[--fail_stack.avail].pointer
#define POP_FAILURE_INT() fail_stack.stack[--fail_stack.avail].integer

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
  DEBUG_PRINT ("\n  Doubled stack; size now: %td\n", fail_stack.size);	\
  DEBUG_PRINT ("	 slots available: %td\n", REMAINING_AVAIL_SLOTS);\
}

/* Push register NUM onto the stack.  */
#define PUSH_FAILURE_REG(num)						\
do {									\
  char *destination;							\
  intptr_t n = num;							\
  ENSURE_FAIL_STACK(3);							\
  DEBUG_PRINT ("    Push reg %"PRIdPTR" (spanning %p -> %p)\n",		\
	       n, regstart[n], regend[n]);				\
  PUSH_FAILURE_POINTER (regstart[n]);					\
  PUSH_FAILURE_POINTER (regend[n]);					\
  PUSH_FAILURE_INT (n);							\
} while (false)

/* Change the counter's value to VAL, but make sure that it will
   be reset when backtracking.  */
#define PUSH_NUMBER(ptr,val)						\
do {									\
  char *destination;							\
  int c;								\
  ENSURE_FAIL_STACK(3);							\
  EXTRACT_NUMBER (c, ptr);						\
  DEBUG_PRINT ("    Push number %p = %d -> %d\n", ptr, c, val);		\
  PUSH_FAILURE_INT (c);							\
  PUSH_FAILURE_POINTER (ptr);						\
  PUSH_FAILURE_INT (-1);						\
  STORE_NUMBER (ptr, val);						\
} while (false)

/* Pop a saved register off the stack.  */
#define POP_FAILURE_REG_OR_COUNT()					\
do {									\
  intptr_t pfreg = POP_FAILURE_INT ();					\
  if (pfreg == -1)							\
    {									\
      /* It's a counter.  */						\
      /* Discard 'const', making re_search non-reentrant.  */		\
      unsigned char *ptr = (unsigned char *) POP_FAILURE_POINTER ();	\
      pfreg = POP_FAILURE_INT ();					\
      STORE_NUMBER (ptr, pfreg);					\
      DEBUG_PRINT ("     Pop counter %p = %"PRIdPTR"\n", ptr, pfreg);	\
    }									\
  else									\
    {									\
      regend[pfreg] = POP_FAILURE_POINTER ();				\
      regstart[pfreg] = POP_FAILURE_POINTER ();				\
      DEBUG_PRINT ("     Pop reg %ld (spanning %p -> %p)\n",		\
		   pfreg, regstart[pfreg], regend[pfreg]);		\
    }									\
} while (false)

/* Check that we are not stuck in an infinite loop.  */
#define CHECK_INFINITE_LOOP(pat_cur, string_place)			\
do {									\
  ptrdiff_t failure = TOP_FAILURE_HANDLE ();				\
  /* Check for infinite matching loops */				\
  while (failure > 0							\
	 && (FAILURE_STR (failure) == string_place			\
	     || FAILURE_STR (failure) == NULL))				\
    {									\
      eassert (FAILURE_PAT (failure) >= bufp->buffer			\
	       && FAILURE_PAT (failure) <= bufp->buffer + bufp->used);	\
      if (FAILURE_PAT (failure) == pat_cur)				\
	{								\
	  cycle = true;							\
	  break;							\
	}								\
      DEBUG_PRINT ("  Other pattern: %p\n", FAILURE_PAT (failure));	\
      failure = NEXT_FAILURE_HANDLE(failure);				\
    }									\
  DEBUG_PRINT ("  Other string: %p\n", FAILURE_STR (failure));		\
} while (false)

/* Push the information about the state we will need
   if we ever fail back to it.

   Requires variables fail_stack, regstart, regend and
   num_regs be declared.  GROW_FAIL_STACK requires 'destination' be
   declared.

   Does 'return FAILURE_CODE' if runs out of memory.  */

#define PUSH_FAILURE_POINT(pattern, string_place)			\
do {									\
  char *destination;							\
  DEBUG_STATEMENT (nfailure_points_pushed++);				\
  DEBUG_PRINT ("\nPUSH_FAILURE_POINT:\n");				\
  DEBUG_PRINT ("  Before push, next avail: %td\n", fail_stack.avail);	\
  DEBUG_PRINT ("			size: %td\n", fail_stack.size);	\
									\
  ENSURE_FAIL_STACK (NUM_NONREG_ITEMS);					\
									\
  DEBUG_PRINT ("\n");							\
									\
  DEBUG_PRINT ("  Push frame index: %td\n", fail_stack.frame);		\
  PUSH_FAILURE_INT (fail_stack.frame);					\
									\
  DEBUG_PRINT ("  Push string %p: \"", string_place);			\
  DEBUG_PRINT_DOUBLE_STRING (string_place, string1, size1, string2, size2);\
  DEBUG_PRINT ("\"\n");							\
  PUSH_FAILURE_POINTER (string_place);					\
									\
  DEBUG_PRINT ("  Push pattern %p: ", pattern);				\
  DEBUG_PRINT_COMPILED_PATTERN (bufp, pattern, pend);			\
  PUSH_FAILURE_POINTER (pattern);					\
									\
  /* Close the frame by moving the frame pointer past it.  */		\
  fail_stack.frame = fail_stack.avail;					\
} while (false)

/* Estimate the size of data pushed by a typical failure stack entry.
   An estimate is all we need, because all we use this for
   is to choose a limit for how big to make the failure stack.  */
/* BEWARE, the value `20' is hard-coded in emacs.c:main().  */
#define TYPICAL_FAILURE_SIZE 20

/* How many items can still be added to the stack without overflowing it.  */
#define REMAINING_AVAIL_SLOTS ((fail_stack).size - (fail_stack).avail)


/* Pop what PUSH_FAIL_STACK pushes.

   Restore into the parameters, all of which should be lvalues:
     STR -- the saved data position.
     PAT -- the saved pattern position.
     REGSTART, REGEND -- arrays of string positions.

   Also assume the variables FAIL_STACK and (if debugging) BUFP, PEND,
   STRING1, SIZE1, STRING2, and SIZE2.  */

#define POP_FAILURE_POINT(str, pat)                                     \
do {									\
  eassert (!FAIL_STACK_EMPTY ());					\
									\
  /* Remove failure points and point to how many regs pushed.  */	\
  DEBUG_PRINT ("POP_FAILURE_POINT:\n");					\
  DEBUG_PRINT ("  Before pop, next avail: %td\n", fail_stack.avail);	\
  DEBUG_PRINT ("		     size: %td\n", fail_stack.size);	\
									\
  /* Pop the saved registers.  */					\
  while (fail_stack.frame < fail_stack.avail)				\
    POP_FAILURE_REG_OR_COUNT ();					\
									\
  pat = POP_FAILURE_POINTER ();						\
  DEBUG_PRINT ("  Popping pattern %p: ", pat);				\
  DEBUG_PRINT_COMPILED_PATTERN (bufp, pat, pend);			\
									\
  /* If the saved string location is NULL, it came from an		\
     on_failure_keep_string_jump opcode, and we want to throw away the	\
     saved NULL, thus retaining our current position in the string.  */	\
  str = POP_FAILURE_POINTER ();						\
  DEBUG_PRINT ("  Popping string %p: \"", str);				\
  DEBUG_PRINT_DOUBLE_STRING (str, string1, size1, string2, size2);	\
  DEBUG_PRINT ("\"\n");							\
									\
  fail_stack.frame = POP_FAILURE_INT ();				\
  DEBUG_PRINT ("  Popping  frame index: %td\n", fail_stack.frame);	\
									\
  eassert (fail_stack.avail >= 0);					\
  eassert (fail_stack.frame <= fail_stack.avail);			\
									\
  DEBUG_STATEMENT (nfailure_points_popped++);				\
} while (false) /* POP_FAILURE_POINT */



/* Registers are set to a sentinel when they haven't yet matched.  */
#define REG_UNSET(e) ((e) == NULL)

/* Subroutine declarations and macros for regex_compile.  */

static reg_errcode_t regex_compile (re_char *pattern, ptrdiff_t size,
				    bool posix_backtracking,
				    const char *whitespace_regexp,
				    struct re_pattern_buffer *bufp);
static void store_op1 (re_opcode_t op, unsigned char *loc, int arg);
static void store_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2);
static void insert_op1 (re_opcode_t op, unsigned char *loc,
			int arg, unsigned char *end);
static void insert_op2 (re_opcode_t op, unsigned char *loc,
			int arg1, int arg2, unsigned char *end);
static bool at_begline_loc_p (re_char *pattern, re_char *p);
static bool at_endline_loc_p (re_char *p, re_char *pend);
static re_char *skip_one_char (re_char *p);
static int analyze_first (re_char *p, re_char *pend,
			  char *fastmap, bool multibyte);

/* Fetch the next character in the uncompiled pattern, with no
   translation.  */
#define PATFETCH(c)							\
  do {									\
    int len;								\
    if (p == pend) return REG_EEND;					\
    c = RE_STRING_CHAR_AND_LENGTH (p, len, multibyte);			\
    p += len;								\
  } while (false)


#define RE_TRANSLATE(TBL, C) char_table_translate (TBL, C)
#define TRANSLATE(d) (!NILP (translate) ? RE_TRANSLATE (translate, d) : (d))

/* Macros for outputting the compiled pattern into 'buffer'.  */

/* If the buffer isn't allocated when it comes in, use this.  */
#define INIT_BUF_SIZE  32

/* Ensure at least N more bytes of space in buffer.  */
#define GET_BUFFER_SPACE(n)						\
    if (bufp->buffer + bufp->allocated - b < (n))			\
      EXTEND_BUFFER ((n) - (bufp->buffer + bufp->allocated - b))

/* Ensure one more byte of buffer space and then add C to it.  */
#define BUF_PUSH(c)							\
  do {									\
    GET_BUFFER_SPACE (1);						\
    *b++ = (unsigned char) (c);						\
  } while (false)


/* Ensure we have two more bytes of buffer space and then append C1 and C2.  */
#define BUF_PUSH_2(c1, c2)						\
  do {									\
    GET_BUFFER_SPACE (2);						\
    *b++ = (unsigned char) (c1);					\
    *b++ = (unsigned char) (c2);					\
  } while (false)


/* Store a jump with opcode OP at LOC to location TO.  Store a
   relative address offset by the three bytes the jump itself occupies.  */
#define STORE_JUMP(op, loc, to) \
  store_op1 (op, loc, (to) - (loc) - 3)

/* Likewise, for a two-argument jump.  */
#define STORE_JUMP2(op, loc, to, arg) \
  store_op2 (op, loc, (to) - (loc) - 3, arg)

/* Like 'STORE_JUMP', but for inserting.  Assume B is the buffer end.  */
#define INSERT_JUMP(op, loc, to) \
  insert_op1 (op, loc, (to) - (loc) - 3, b)

/* Like 'STORE_JUMP2', but for inserting.  Assume B is the buffer end.  */
#define INSERT_JUMP2(op, loc, to, arg) \
  insert_op2 (op, loc, (to) - (loc) - 3, arg, b)


/* This is not an arbitrary limit: the arguments which represent offsets
   into the pattern are two bytes long.  So if 2^15 bytes turns out to
   be too small, many things would have to change.  */
# define MAX_BUF_SIZE (1 << 15)

/* Extend the buffer by at least N bytes via realloc and
   reset the pointers that pointed into the old block to point to the
   correct places in the new one.  If extending the buffer results in it
   being larger than MAX_BUF_SIZE, then flag memory exhausted.  */
#define EXTEND_BUFFER(n)						\
  do {									\
    ptrdiff_t requested_extension = n;					\
    unsigned char *old_buffer = bufp->buffer;				\
    if (MAX_BUF_SIZE - bufp->allocated < requested_extension)		\
      return REG_ESIZE;							\
    ptrdiff_t b_off = b - old_buffer;					\
    ptrdiff_t begalt_off = begalt - old_buffer;				\
    bool fixup_alt_jump_set = !!fixup_alt_jump;				\
    bool laststart_set = !!laststart;					\
    bool pending_exact_set = !!pending_exact;				\
    ptrdiff_t fixup_alt_jump_off, laststart_off, pending_exact_off;	\
    if (fixup_alt_jump_set) fixup_alt_jump_off = fixup_alt_jump - old_buffer; \
    if (laststart_set) laststart_off = laststart - old_buffer;		\
    if (pending_exact_set) pending_exact_off = pending_exact - old_buffer; \
    bufp->buffer = xpalloc (bufp->buffer, &bufp->allocated,		\
			    requested_extension, MAX_BUF_SIZE, 1);	\
    unsigned char *new_buffer = bufp->buffer;				\
    b = new_buffer + b_off;						\
    begalt = new_buffer + begalt_off;					\
    if (fixup_alt_jump_set) fixup_alt_jump = new_buffer + fixup_alt_jump_off; \
    if (laststart_set) laststart = new_buffer + laststart_off;		\
    if (pending_exact_set) pending_exact = new_buffer + pending_exact_off; \
  } while (false)


/* Since we have one byte reserved for the register number argument to
   {start,stop}_memory, the maximum number of groups we can report
   things about is what fits in that byte.  */
#define MAX_REGNUM 255

/* But patterns can have more than 'MAX_REGNUM' registers.  Just
   ignore the excess.  */
typedef int regnum_t;


/* Macros for the compile stack.  */

typedef long pattern_offset_t;
verify (LONG_MIN <= -(MAX_BUF_SIZE - 1) && MAX_BUF_SIZE - 1 <= LONG_MAX);

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
  ptrdiff_t size;
  ptrdiff_t avail;		/* Offset of next open position.  */
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
  int used;			/* actually used size in words.  */
  int bits;			/* flag to record character classes */
};

/* Make sure that WORK_AREA can hold N more multibyte characters.
   If it can't get the space, it returns from the surrounding function.  */

#define EXTEND_RANGE_TABLE(work_area, n)				\
  do {									\
    if (((work_area).used + (n)) * sizeof (int) > (work_area).allocated) \
      {									\
        extend_range_table_work_area (&work_area);			\
        if ((work_area).table == 0)					\
          return (REG_ESPACE);						\
      }									\
  } while (false)

#define SET_RANGE_TABLE_WORK_AREA_BIT(work_area, bit)		\
  (work_area).bits |= (bit)

/* Set a range (RANGE_START, RANGE_END) to WORK_AREA.  */
#define SET_RANGE_TABLE_WORK_AREA(work_area, range_start, range_end)	\
  do {									\
    EXTEND_RANGE_TABLE ((work_area), 2);				\
    (work_area).table[(work_area).used++] = (range_start);		\
    (work_area).table[(work_area).used++] = (range_end);		\
  } while (false)

/* Free allocated memory for WORK_AREA.  */
#define FREE_RANGE_TABLE_WORK_AREA(work_area)	\
  do {						\
    if ((work_area).table)			\
      xfree ((work_area).table);			\
  } while (false)

#define CLEAR_RANGE_TABLE_WORK_USED(work_area) \
  ((work_area).used = 0, (work_area).bits = 0)
#define RANGE_TABLE_WORK_USED(work_area) ((work_area).used)
#define RANGE_TABLE_WORK_BITS(work_area) ((work_area).bits)
#define RANGE_TABLE_WORK_ELT(work_area, i) ((work_area).table[i])

/* Bits used to implement the multibyte-part of the various character classes
   such as [:alnum:] in a charset's range table.  The code currently assumes
   that only the low 16 bits are used.  */
#define BIT_WORD	0x1
#define BIT_LOWER	0x2
#define BIT_PUNCT	0x4
#define BIT_SPACE	0x8
#define BIT_UPPER	0x10
#define BIT_MULTIBYTE	0x20
#define BIT_ALPHA	0x40
#define BIT_ALNUM	0x80
#define BIT_GRAPH	0x100
#define BIT_PRINT	0x200
#define BIT_BLANK       0x400


/* Set the bit for character C in a list.  */
#define SET_LIST_BIT(c) (b[((c)) / BYTEWIDTH] |= 1 << ((c) % BYTEWIDTH))


/* Store characters in the range FROM to TO in the bitmap at B (for
   ASCII and unibyte characters) and WORK_AREA (for multibyte
   characters) while translating them and paying attention to the
   continuity of translated characters.

   Implementation note: It is better to implement these fairly big
   macros by a function, but it's not that easy because macros called
   in this macro assume various local variables already declared.  */

/* Both FROM and TO are ASCII characters.  */

#define SETUP_ASCII_RANGE(work_area, FROM, TO)			\
  do {								\
    int C0, C1;							\
								\
    for (C0 = (FROM); C0 <= (TO); C0++)				\
      {								\
	C1 = TRANSLATE (C0);					\
	if (! ASCII_CHAR_P (C1))				\
	  {							\
	    SET_RANGE_TABLE_WORK_AREA ((work_area), C1, C1);	\
	    if ((C1 = RE_CHAR_TO_UNIBYTE (C1)) < 0)		\
	      C1 = C0;						\
	  }							\
	SET_LIST_BIT (C1);					\
      }								\
  } while (false)


/* Both FROM and TO are unibyte characters (0x80..0xFF).  */

#define SETUP_UNIBYTE_RANGE(work_area, FROM, TO)			       \
  do {									       \
    int C0, C1, C2, I;							       \
    int USED = RANGE_TABLE_WORK_USED (work_area);			       \
									       \
    for (C0 = (FROM); C0 <= (TO); C0++)					       \
      {									       \
	C1 = RE_CHAR_TO_MULTIBYTE (C0);					       \
	if (CHAR_BYTE8_P (C1))						       \
	  SET_LIST_BIT (C0);						       \
	else								       \
	  {								       \
	    C2 = TRANSLATE (C1);					       \
	    if (C2 == C1						       \
		|| (C1 = RE_CHAR_TO_UNIBYTE (C2)) < 0)			       \
	      C1 = C0;							       \
	    SET_LIST_BIT (C1);						       \
	    for (I = RANGE_TABLE_WORK_USED (work_area) - 2; I >= USED; I -= 2) \
	      {								       \
		int from = RANGE_TABLE_WORK_ELT (work_area, I);		       \
		int to = RANGE_TABLE_WORK_ELT (work_area, I + 1);	       \
									       \
		if (C2 >= from - 1 && C2 <= to + 1)			       \
		  {							       \
		    if (C2 == from - 1)					       \
		      RANGE_TABLE_WORK_ELT (work_area, I)--;		       \
		    else if (C2 == to + 1)				       \
		      RANGE_TABLE_WORK_ELT (work_area, I + 1)++;	       \
		    break;						       \
		  }							       \
	      }								       \
	    if (I < USED)						       \
	      SET_RANGE_TABLE_WORK_AREA ((work_area), C2, C2);		       \
	  }								       \
      }									       \
  } while (false)


/* Both FROM and TO are multibyte characters.  */

#define SETUP_MULTIBYTE_RANGE(work_area, FROM, TO)			   \
  do {									   \
    int C0, C1, C2, I, USED = RANGE_TABLE_WORK_USED (work_area);	   \
									   \
    SET_RANGE_TABLE_WORK_AREA ((work_area), (FROM), (TO));		   \
    for (C0 = (FROM); C0 <= (TO); C0++)					   \
      {									   \
	C1 = TRANSLATE (C0);						   \
	if ((C2 = RE_CHAR_TO_UNIBYTE (C1)) >= 0				   \
	    || (C1 != C0 && (C2 = RE_CHAR_TO_UNIBYTE (C0)) >= 0))	   \
	  SET_LIST_BIT (C2);						   \
	if (C1 >= (FROM) && C1 <= (TO))					   \
	  continue;							   \
	for (I = RANGE_TABLE_WORK_USED (work_area) - 2; I >= USED; I -= 2) \
	  {								   \
	    int from = RANGE_TABLE_WORK_ELT (work_area, I);		   \
	    int to = RANGE_TABLE_WORK_ELT (work_area, I + 1);		   \
									   \
	    if (C1 >= from - 1 && C1 <= to + 1)				   \
	      {								   \
		if (C1 == from - 1)					   \
		  RANGE_TABLE_WORK_ELT (work_area, I)--;		   \
		else if (C1 == to + 1)					   \
		  RANGE_TABLE_WORK_ELT (work_area, I + 1)++;		   \
		break;							   \
	      }								   \
	  }								   \
	if (I < USED)							   \
	  SET_RANGE_TABLE_WORK_AREA ((work_area), C1, C1);		   \
      }									   \
  } while (false)

/* Get the next unsigned number in the uncompiled pattern.  */
#define GET_INTERVAL_COUNT(num)					\
  do {									\
    if (p == pend)							\
      FREE_STACK_RETURN (REG_EBRACE);					\
    else								\
      {									\
	PATFETCH (c);							\
	while ('0' <= c && c <= '9')					\
	  {								\
	    if (num < 0)						\
	      num = 0;							\
	    if (RE_DUP_MAX / 10 - (RE_DUP_MAX % 10 < c - '0') < num)	\
	      FREE_STACK_RETURN (REG_ESIZEBR);				\
	    num = num * 10 + c - '0';					\
	    if (p == pend)						\
	      FREE_STACK_RETURN (REG_EBRACE);				\
	    PATFETCH (c);						\
	  }								\
      }									\
  } while (false)

/* Parse a character class, i.e. string such as "[:name:]".  *strp
   points to the string to be parsed and limit is length, in bytes, of
   that string.

   If *strp point to a string that begins with "[:name:]", where name is
   a non-empty sequence of lower case letters, *strp will be advanced past the
   closing square bracket and RECC_* constant which maps to the name will be
   returned.  If name is not a valid character class name zero, or RECC_ERROR,
   is returned.

   Otherwise, if *strp doesn't begin with "[:name:]", -1 is returned.

   The function can be used on ASCII and multibyte (UTF-8-encoded) strings.
 */
re_wctype_t
re_wctype_parse (const unsigned char **strp, ptrdiff_t limit)
{
  const char *beg = (const char *)*strp, *it;

  if (limit < 4 || beg[0] != '[' || beg[1] != ':')
    return -1;

  beg += 2;  /* skip opening "[:" */
  limit -= 3;  /* opening "[:" and half of closing ":]"; --limit handles rest */
  for (it = beg; it[0] != ':' || it[1] != ']'; ++it)
    if (!--limit)
      return -1;

  *strp = (const unsigned char *)(it + 2);

  /* Sort tests in the length=five case by frequency the classes to minimize
     number of times we fail the comparison.  The frequencies of character class
     names used in Emacs sources as of 2016-07-27:

     $ find \( -name \*.c -o -name \*.el \) -exec grep -h '\[:[a-z]*:]' {} + |
           sed 's/]/]\n/g' |grep -o '\[:[a-z]*:]' |sort |uniq -c |sort -nr
         213 [:alnum:]
         104 [:alpha:]
          62 [:space:]
          39 [:digit:]
          36 [:blank:]
          26 [:word:]
          26 [:upper:]
          21 [:lower:]
          10 [:xdigit:]
          10 [:punct:]
          10 [:ascii:]
           4 [:nonascii:]
           4 [:graph:]
           2 [:print:]
           2 [:cntrl:]
           1 [:ff:]

     If you update this list, consider also updating chain of or'ed conditions
     in execute_charset function.
   */

  switch (it - beg) {
  case 4:
    if (!memcmp (beg, "word", 4))      return RECC_WORD;
    break;
  case 5:
    if (!memcmp (beg, "alnum", 5))     return RECC_ALNUM;
    if (!memcmp (beg, "alpha", 5))     return RECC_ALPHA;
    if (!memcmp (beg, "space", 5))     return RECC_SPACE;
    if (!memcmp (beg, "digit", 5))     return RECC_DIGIT;
    if (!memcmp (beg, "blank", 5))     return RECC_BLANK;
    if (!memcmp (beg, "upper", 5))     return RECC_UPPER;
    if (!memcmp (beg, "lower", 5))     return RECC_LOWER;
    if (!memcmp (beg, "punct", 5))     return RECC_PUNCT;
    if (!memcmp (beg, "ascii", 5))     return RECC_ASCII;
    if (!memcmp (beg, "graph", 5))     return RECC_GRAPH;
    if (!memcmp (beg, "print", 5))     return RECC_PRINT;
    if (!memcmp (beg, "cntrl", 5))     return RECC_CNTRL;
    break;
  case 6:
    if (!memcmp (beg, "xdigit", 6))    return RECC_XDIGIT;
    break;
  case 7:
    if (!memcmp (beg, "unibyte", 7))   return RECC_UNIBYTE;
    break;
  case 8:
    if (!memcmp (beg, "nonascii", 8))  return RECC_NONASCII;
    break;
  case 9:
    if (!memcmp (beg, "multibyte", 9)) return RECC_MULTIBYTE;
    break;
  }

  return RECC_ERROR;
}

/* True if CH is in the char class CC.  */
bool
re_iswctype (int ch, re_wctype_t cc)
{
  switch (cc)
    {
    case RECC_ALNUM: return ISALNUM (ch) != 0;
    case RECC_ALPHA: return ISALPHA (ch) != 0;
    case RECC_BLANK: return ISBLANK (ch) != 0;
    case RECC_CNTRL: return ISCNTRL (ch) != 0;
    case RECC_DIGIT: return ISDIGIT (ch) != 0;
    case RECC_GRAPH: return ISGRAPH (ch) != 0;
    case RECC_LOWER: return ISLOWER (ch) != 0;
    case RECC_PRINT: return ISPRINT (ch) != 0;
    case RECC_PUNCT: return ISPUNCT (ch) != 0;
    case RECC_SPACE: return ISSPACE (ch) != 0;
    case RECC_UPPER: return ISUPPER (ch) != 0;
    case RECC_XDIGIT: return ISXDIGIT (ch) != 0;
    case RECC_ASCII: return IS_REAL_ASCII (ch) != 0;
    case RECC_NONASCII: return !IS_REAL_ASCII (ch);
    case RECC_UNIBYTE: return ISUNIBYTE (ch) != 0;
    case RECC_MULTIBYTE: return !ISUNIBYTE (ch);
    case RECC_WORD: return ISWORD (ch) != 0;
    case RECC_ERROR: return false;
    default:
      abort ();
    }
}

/* Return a bit-pattern to use in the range-table bits to match multibyte
   chars of class CC.  */
static int
re_wctype_to_bit (re_wctype_t cc)
{
  switch (cc)
    {
    case RECC_NONASCII:
    case RECC_MULTIBYTE: return BIT_MULTIBYTE;
    case RECC_ALPHA: return BIT_ALPHA;
    case RECC_ALNUM: return BIT_ALNUM;
    case RECC_WORD: return BIT_WORD;
    case RECC_LOWER: return BIT_LOWER;
    case RECC_UPPER: return BIT_UPPER;
    case RECC_PUNCT: return BIT_PUNCT;
    case RECC_SPACE: return BIT_SPACE;
    case RECC_GRAPH: return BIT_GRAPH;
    case RECC_PRINT: return BIT_PRINT;
    case RECC_BLANK: return BIT_BLANK;
    case RECC_ASCII: case RECC_DIGIT: case RECC_XDIGIT: case RECC_CNTRL:
    case RECC_UNIBYTE: case RECC_ERROR: return 0;
    default:
      abort ();
    }
}

/* Filling in the work area of a range.  */

/* Actually extend the space in WORK_AREA.  */

static void
extend_range_table_work_area (struct range_table_work_area *work_area)
{
  work_area->allocated += 16 * sizeof (int);
  work_area->table = xrealloc (work_area->table, work_area->allocated);
}

/* regex_compile and helpers.  */

static bool group_in_compile_stack (compile_stack_type, regnum_t);

/* Insert the 'jump' from the end of last alternative to "here".
   The space for the jump has already been allocated. */
#define FIXUP_ALT_JUMP()						\
do {									\
  if (fixup_alt_jump)							\
    STORE_JUMP (jump, fixup_alt_jump, b);				\
} while (false)


/* Return, freeing storage we allocated.  */
#define FREE_STACK_RETURN(value)		\
  do {							\
    FREE_RANGE_TABLE_WORK_AREA (range_table_work);	\
    xfree (compile_stack.stack);			\
    return value;					\
  } while (false)

/* Compile PATTERN (of length SIZE) according to SYNTAX.
   Return a nonzero error code on failure, or zero for success.

   If WHITESPACE_REGEXP is given, use it instead of a space
   character in PATTERN.

   Assume the 'allocated' (and perhaps 'buffer') and 'translate'
   fields are set in BUFP on entry.

   If successful, put results in *BUFP (otherwise the
   contents of *BUFP are undefined):
     'buffer' is the compiled pattern;
     'syntax' is set to SYNTAX;
     'used' is set to the length of the compiled pattern;
     'fastmap_accurate' is false;
     're_nsub' is the number of subexpressions in PATTERN;

   The 'fastmap' field is neither examined nor set.  */

static reg_errcode_t
regex_compile (re_char *pattern, ptrdiff_t size,
	       bool posix_backtracking,
	       const char *whitespace_regexp,
	       struct re_pattern_buffer *bufp)
{
  /* Fetch characters from PATTERN here.  */
  int c, c1;

  /* Points to the end of the buffer, where we should append.  */
  unsigned char *b;

  /* Keeps track of unclosed groups.  */
  compile_stack_type compile_stack;

  /* Points to the current (ending) position in the pattern.  */
  re_char *p = pattern;
  re_char *pend = pattern + size;

  /* How to translate the characters in the pattern.  */
  Lisp_Object translate = bufp->translate;

  /* Address of the count-byte of the most recently inserted 'exactn'
     command.  This makes it possible to tell if a new exact-match
     character can be added to that command or if the character requires
     a new 'exactn' command.  */
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
     the containing expression.  Each alternative of an 'or' -- except the
     last -- ends with a forward jump of this sort.  */
  unsigned char *fixup_alt_jump = 0;

  /* Work area for range table of charset.  */
  struct range_table_work_area range_table_work;

  /* If the regular expression is multibyte.  */
  bool multibyte = RE_MULTIBYTE_P (bufp);

  /* Nonzero if we have pushed down into a subpattern.  */
  int in_subpattern = 0;

  /* These hold the values of p, pattern, and pend from the main
     pattern when we have pushed into a subpattern.  */
  re_char *main_p;
  re_char *main_pattern;
  re_char *main_pend;

#ifdef REGEX_EMACS_DEBUG
  regex_emacs_debug++;
  DEBUG_PRINT ("\nCompiling pattern: ");
  if (regex_emacs_debug > 0)
    {
      for (ptrdiff_t debug_count = 0; debug_count < size; debug_count++)
	debug_putchar (pattern[debug_count]);
      putc ('\n', stderr);
    }
#endif

  /* Initialize the compile stack.  */
  compile_stack.stack = xmalloc (INIT_COMPILE_STACK_SIZE
				 * sizeof *compile_stack.stack);
  compile_stack.size = INIT_COMPILE_STACK_SIZE;
  compile_stack.avail = 0;

  range_table_work.table = 0;
  range_table_work.allocated = 0;

  /* Initialize the pattern buffer.  */
  bufp->fastmap_accurate = false;
  bufp->used_syntax = false;

  /* Set 'used' to zero, so that if we return an error, the pattern
     printer (for debugging) will think there's no pattern.  We reset it
     at the end.  */
  bufp->used = 0;

  bufp->re_nsub = 0;

  if (bufp->allocated == 0)
    {
      /* This loses if BUFP->buffer is bogus, but that is the user's
	 responsibility.  */
      bufp->buffer = xrealloc (bufp->buffer, INIT_BUF_SIZE);
      bufp->allocated = INIT_BUF_SIZE;
    }

  begalt = b = bufp->buffer;

  /* Loop through the uncompiled pattern until we're at the end.  */
  while (1)
    {
      if (p == pend)
	{
	  /* If this is the end of an included regexp,
	     pop back to the main regexp and try again.  */
	  if (in_subpattern)
	    {
	      in_subpattern = 0;
	      pattern = main_pattern;
	      p = main_p;
	      pend = main_pend;
	      continue;
	    }
	  /* If this is the end of the main regexp, we are done.  */
	  break;
	}

      PATFETCH (c);

      switch (c)
	{
	case ' ':
	  {
	    re_char *p1 = p;

	    /* If there's no special whitespace regexp, treat
	       spaces normally.  And don't try to do this recursively.  */
	    if (!whitespace_regexp || in_subpattern)
	      goto normal_char;

	    /* Peek past following spaces.  */
	    while (p1 != pend)
	      {
		if (*p1 != ' ')
		  break;
		p1++;
	      }
	    /* If the spaces are followed by a repetition op,
	       treat them normally.  */
	    if (p1 != pend
		&& (*p1 == '*' || *p1 == '+' || *p1 == '?'
		    || (*p1 == '\\' && p1 + 1 != pend && p1[1] == '{')))
	      goto normal_char;

	    /* Replace the spaces with the whitespace regexp.  */
	    in_subpattern = 1;
	    main_p = p1;
	    main_pend = pend;
	    main_pattern = pattern;
	    p = pattern = (re_char *) whitespace_regexp;
	    pend = p + strlen (whitespace_regexp);
	    break;
	  }

	case '^':
	  if (! (p == pattern + 1 || at_begline_loc_p (pattern, p)))
	    goto normal_char;
	  BUF_PUSH (begline);
	  break;

	case '$':
	  if (! (p == pend || at_endline_loc_p (p, pend)))
	    goto normal_char;
	  BUF_PUSH (endline);
	  break;


	case '+':
	case '?':
	case '*':
	  /* If there is no previous pattern...  */
	  if (!laststart)
	    goto normal_char;

	  {
	    /* 1 means zero (many) matches is allowed.  */
	    bool zero_times_ok = false, many_times_ok = false;
	    bool greedy = true;

	    /* If there is a sequence of repetition chars, collapse it
	       down to just one (the right one).  We can't combine
	       interval operators with these because of, e.g., 'a{2}*',
	       which should only match an even number of 'a's.  */

	    for (;;)
	      {
		if (c == '?' && (zero_times_ok || many_times_ok))
		  greedy = false;
		else
		  {
		    zero_times_ok |= c != '+';
		    many_times_ok |= c != '?';
		  }

		if (! (p < pend && (*p == '*' || *p == '+' || *p == '?')))
		  break;
		/* If we get here, we found another repeat character.  */
		c = *p++;
	       }

	    /* Star, etc. applied to an empty pattern is equivalent
	       to an empty pattern.  */
	    if (!laststart || laststart == b)
	      break;

	    /* Now we know whether or not zero matches is allowed
	       and also whether or not two or more matches is allowed.  */
	    if (greedy)
	      {
		if (many_times_ok)
		  {
		    bool simple = skip_one_char (laststart) == b;
		    ptrdiff_t startoffset = 0;
		    re_opcode_t ofj =
		      /* Check if the loop can match the empty string.  */
		      (simple || !analyze_first (laststart, b, NULL, false))
		      ? on_failure_jump : on_failure_jump_loop;
		    eassert (skip_one_char (laststart) <= b);

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
		    eassert (zero_times_ok);
		    GET_BUFFER_SPACE (3);
		    INSERT_JUMP (on_failure_jump, laststart, b + 3);
		    b += 3;
		  }
	      }
	    else		/* not greedy */
	      { /* I wish the greedy and non-greedy cases could be merged.  */

		GET_BUFFER_SPACE (7); /* We might use less.  */
		if (many_times_ok)
		  {
		    bool emptyp = !!analyze_first (laststart, b, NULL, false);

		    /* The non-greedy multiple match looks like
		       a repeat..until: we only need a conditional jump
		       at the end of the loop.  */
		    if (emptyp) BUF_PUSH (no_op);
		    STORE_JUMP (emptyp ? on_failure_jump_nastyloop
				: on_failure_jump, b, laststart);
		    b += 3;
		    if (zero_times_ok)
		      {
			/* The repeat...until naturally matches one or more.
			   To also match zero times, we need to first jump to
			   the end of the loop (its conditional jump).  */
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
	    re_char *p1;

	    CLEAR_RANGE_TABLE_WORK_USED (range_table_work);

	    if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

	    /* Ensure that we have enough space to push a charset: the
	       opcode, the length count, and the bitset; 34 bytes in all.  */
	    GET_BUFFER_SPACE (34);

	    laststart = b;

	    /* Test '*p == '^' twice, instead of using an if
	       statement, so we need only one BUF_PUSH.  */
	    BUF_PUSH (*p == '^' ? charset_not : charset);
	    if (*p == '^')
	      p++;

	    /* Remember the first position in the bracket expression.  */
	    p1 = p;

	    /* Push the number of bytes in the bitmap.  */
	    BUF_PUSH ((1 << BYTEWIDTH) / BYTEWIDTH);

	    /* Clear the whole map.  */
	    memset (b, 0, (1 << BYTEWIDTH) / BYTEWIDTH);

	    /* Read in characters and ranges, setting map bits.  */
	    for (;;)
	      {
		const unsigned char *p2 = p;
		int ch;

		if (p == pend) FREE_STACK_RETURN (REG_EBRACK);

		/* See if we're at the beginning of a possible character
		   class.  */
		re_wctype_t cc = re_wctype_parse (&p, pend - p);
		if (cc != -1)
		  {
		    if (cc == 0)
		      FREE_STACK_RETURN (REG_ECTYPE);

		    if (p == pend)
		      FREE_STACK_RETURN (REG_EBRACK);

		    /* Most character classes in a multibyte match just set
		       a flag.  Exceptions are is_blank, is_digit, is_cntrl, and
		       is_xdigit, since they can only match ASCII characters.
		       We don't need to handle them for multibyte.  */

		    /* Setup the gl_state object to its buffer-defined value.
		       This hardcodes the buffer-global syntax-table for ASCII
		       chars, while the other chars will obey syntax-table
		       properties.  It's not ideal, but it's the way it's been
		       done until now.  */
		    SETUP_BUFFER_SYNTAX_TABLE ();

		    for (c = 0; c < 0x80; ++c)
		      if (re_iswctype (c, cc))
			{
			  SET_LIST_BIT (c);
			  c1 = TRANSLATE (c);
			  if (c1 == c)
			    continue;
			  if (ASCII_CHAR_P (c1))
			    SET_LIST_BIT (c1);
			  else if ((c1 = RE_CHAR_TO_UNIBYTE (c1)) >= 0)
			    SET_LIST_BIT (c1);
			}
		    SET_RANGE_TABLE_WORK_AREA_BIT
		      (range_table_work, re_wctype_to_bit (cc));

		    /* In most cases the matching rule for char classes only
		       uses the syntax table for multibyte chars, so that the
		       content of the syntax-table is not hardcoded in the
		       range_table.  SPACE and WORD are the two exceptions.  */
		    if ((1 << cc) & ((1 << RECC_SPACE) | (1 << RECC_WORD)))
		      bufp->used_syntax = true;

		    /* Repeat the loop. */
		    continue;
		  }

		/* Don't translate yet.  The range TRANSLATE(X..Y) cannot
		   always be determined from TRANSLATE(X) and TRANSLATE(Y)
		   So the translation is done later in a loop.  Example:
		   (let ((case-fold-search t)) (string-match "[A-_]" "A"))  */
		PATFETCH (c);

		/* Could be the end of the bracket expression.  If it's
		   not (i.e., when the bracket expression is '[]' so
		   far), the ']' character bit gets set way below.  */
		if (c == ']' && p2 != p1)
		  break;

		if (p < pend && p[0] == '-' && p[1] != ']')
		  {

		    /* Discard the '-'. */
		    PATFETCH (c1);

		    /* Fetch the character which ends the range. */
		    PATFETCH (c1);

		    if (CHAR_BYTE8_P (c1)
			&& ! ASCII_CHAR_P (c) && ! CHAR_BYTE8_P (c))
		      /* Treat the range from a multibyte character to
			 raw-byte character as empty.  */
		      c = c1 + 1;
		  }
		else
		  /* Range from C to C. */
		  c1 = c;

		if (c <= c1)
		  {
		    if (c < 128)
		      {
			ch = min (127, c1);
			SETUP_ASCII_RANGE (range_table_work, c, ch);
			c = ch + 1;
			if (CHAR_BYTE8_P (c1))
			  c = BYTE8_TO_CHAR (128);
		      }
		    if (CHAR_BYTE8_P (c))
		      {
			c = CHAR_TO_BYTE8 (c);
			c1 = CHAR_TO_BYTE8 (c1);
			for (; c <= c1; c++)
			  SET_LIST_BIT (c);
		      }
		    else if (multibyte)
		      SETUP_MULTIBYTE_RANGE (range_table_work, c, c1);
		    else
		      SETUP_UNIBYTE_RANGE (range_table_work, c, c1);
		  }
	      }

	    /* Discard any (non)matching list bytes that are all 0 at the
	       end of the map.  Decrease the map-length byte too.  */
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
		   each character.  */
		GET_BUFFER_SPACE (4 + used * 3);

		/* Indicate the existence of range table.  */
		laststart[1] |= 0x80;

		/* Store the character class flag bits into the range table.  */
		*b++ = RANGE_TABLE_WORK_BITS (range_table_work) & 0xff;
		*b++ = RANGE_TABLE_WORK_BITS (range_table_work) >> 8;

		STORE_NUMBER_AND_INCR (b, used / 2);
		for (i = 0; i < used; i++)
		  STORE_CHARACTER_AND_INCR
		    (b, RANGE_TABLE_WORK_ELT (range_table_work, i));
	      }
	  }
	  break;


	case '\\':
	  if (p == pend) FREE_STACK_RETURN (REG_EESCAPE);

	  /* Do not translate the character after the \, so that we can
	     distinguish, e.g., \B from \b, even if we normally would
	     translate, e.g., B to b.  */
	  PATFETCH (c);

	  switch (c)
	    {
	    case '(':
	      {
		bool shy = false;
		regnum_t regnum = 0;
		if (p+1 < pend)
		  {
		    /* Look for a special (?...) construct */
		    if (*p == '?')
		      {
			PATFETCH (c); /* Gobble up the '?'.  */
			while (!shy)
			  {
			    PATFETCH (c);
			    switch (c)
			      {
			      case ':': shy = true; break;
			      case '0':
				/* An explicitly specified regnum must start
				   with non-0. */
				if (regnum == 0)
				  FREE_STACK_RETURN (REG_BADPAT);
				FALLTHROUGH;
			      case '1': case '2': case '3': case '4':
			      case '5': case '6': case '7': case '8': case '9':
				if (INT_MULTIPLY_WRAPV (regnum, 10, &regnum)
				    || INT_ADD_WRAPV (regnum, c - '0',
						      &regnum))
				  FREE_STACK_RETURN (REG_ESIZE);
				break;
			      default:
				/* Only (?:...) is supported right now. */
				FREE_STACK_RETURN (REG_BADPAT);
			      }
			  }
		      }
		  }

		if (!shy)
		  regnum = ++bufp->re_nsub;
		else if (regnum)
		  { /* It's actually not shy, but explicitly numbered.  */
		    shy = false;
		    if (regnum > bufp->re_nsub)
		      bufp->re_nsub = regnum;
		    else if (regnum > bufp->re_nsub
			     /* Ideally, we'd want to check that the specified
				group can't have matched (i.e. all subgroups
				using the same regnum are in other branches of
				OR patterns), but we don't currently keep track
				of enough info to do that easily.  */
			     || group_in_compile_stack (compile_stack, regnum))
		      FREE_STACK_RETURN (REG_BADPAT);
		  }
		else
		  /* It's really shy.  */
		  regnum = - bufp->re_nsub;

		if (COMPILE_STACK_FULL)
		  compile_stack.stack
		    = xpalloc (compile_stack.stack, &compile_stack.size,
			       1, -1, sizeof *compile_stack.stack);

		/* These are the values to restore when we hit end of this
		   group.  They are all relative offsets, so that if the
		   whole pattern moves because of realloc, they will still
		   be valid.  */
		COMPILE_STACK_TOP.begalt_offset = begalt - bufp->buffer;
		COMPILE_STACK_TOP.fixup_alt_jump
		  = fixup_alt_jump ? fixup_alt_jump - bufp->buffer + 1 : 0;
		COMPILE_STACK_TOP.laststart_offset = b - bufp->buffer;
		COMPILE_STACK_TOP.regnum = regnum;

		/* Do not push a start_memory for groups beyond the last one
		   we can represent in the compiled pattern.  */
		if (regnum <= MAX_REGNUM && regnum > 0)
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
	      if (COMPILE_STACK_EMPTY)
		FREE_STACK_RETURN (REG_ERPAREN);

	      FIXUP_ALT_JUMP ();

	      /* See similar code for backslashed left paren above.  */
	      if (COMPILE_STACK_EMPTY)
		FREE_STACK_RETURN (REG_ERPAREN);

	      /* Since we just checked for an empty stack above, this
		 "can't happen".  */
	      eassert (compile_stack.avail != 0);
	      {
		/* We don't just want to restore into 'regnum', because
		   later groups should continue to be numbered higher,
		   as in '(ab)c(de)' -- the second group is #2.  */
		regnum_t regnum;

		compile_stack.avail--;
		begalt = bufp->buffer + COMPILE_STACK_TOP.begalt_offset;
		fixup_alt_jump
		  = COMPILE_STACK_TOP.fixup_alt_jump
		    ? bufp->buffer + COMPILE_STACK_TOP.fixup_alt_jump - 1
		    : 0;
		laststart = bufp->buffer + COMPILE_STACK_TOP.laststart_offset;
		regnum = COMPILE_STACK_TOP.regnum;
		/* If we've reached MAX_REGNUM groups, then this open
		   won't actually generate any code, so we'll have to
		   clear pending_exact explicitly.  */
		pending_exact = 0;

		/* We're at the end of the group, so now we know how many
		   groups were inside this one.  */
		if (regnum <= MAX_REGNUM && regnum > 0)
		  BUF_PUSH_2 (stop_memory, regnum);
	      }
	      break;


	    case '|':					/* '\|'.  */
	      /* Insert before the previous alternative a jump which
		 jumps to this alternative if the former fails.  */
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
			A | B	| C

		 If we are at B, then fixup_alt_jump right now points to a
		 three-byte space after A.  We'll put in the jump, set
		 fixup_alt_jump to right after B, and leave behind three
		 bytes which we'll fill in when we get to after C.  */

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
	      {
		/* At least (most) this many matches must be made.  */
		int lower_bound = 0, upper_bound = -1;

		beg_interval = p;

		GET_INTERVAL_COUNT (lower_bound);

		if (c == ',')
		  GET_INTERVAL_COUNT (upper_bound);
		else
		  /* Interval such as '{1}' => match exactly once. */
		  upper_bound = lower_bound;

		if (lower_bound < 0
		    || (0 <= upper_bound && upper_bound < lower_bound)
		    || c != '\\')
		  FREE_STACK_RETURN (REG_BADBR);
		if (p == pend)
		  FREE_STACK_RETURN (REG_EESCAPE);
		if (*p++ != '}')
		  FREE_STACK_RETURN (REG_BADBR);

		/* We just parsed a valid interval.  */

		/* If it's invalid to have no preceding re.  */
		if (!laststart)
		  goto unfetch_interval;

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
		   (The upper bound and 'jump_n' are omitted if
		   'upper_bound' is 1, though.)  */
		else
		  { /* If the upper bound is > 1, we need to insert
		       more at the end of the loop.  */
		    int nbytes = upper_bound < 0 ? 3 : upper_bound > 1 ? 5 : 0;
		    int startoffset = 0;

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
			/* Initialize lower bound of the 'succeed_n', even
			   though it will be set during matching by its
			   attendant 'set_number_at' (inserted next),
			   because 're_compile_fastmap' needs to know.
			   Jump to the 'jump_n' we might insert below.  */
			INSERT_JUMP2 (succeed_n, laststart,
				      b + 5 + nbytes,
				      lower_bound);
			b += 5;

			/* Code to initialize the lower bound.  Insert
			   before the 'succeed_n'.  The '5' is the last two
			   bytes of this 'set_number_at', plus 3 bytes of
			   the following 'succeed_n'.  */
			insert_op2 (set_number_at, laststart, 5,
				    lower_bound, b);
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
			   append a backward jump to the 'succeed_n'
			   that starts this interval.

			   When we've reached this during matching,
			   we'll have matched the interval once, so
			   jump back only 'upper_bound - 1' times.  */
			STORE_JUMP2 (jump_n, b, laststart + startoffset,
				     upper_bound - 1);
			b += 5;

			/* The location we want to set is the second
			   parameter of the 'jump_n'; that is 'b-2' as
			   an absolute address.  'laststart' will be
			   the 'set_number_at' we're about to insert;
			   'laststart+3' the number to set, the source
			   for the relative address.  But we are
			   inserting into the middle of the pattern --
			   so everything is getting moved up by 5.
			   Conclusion: (b - 2) - (laststart + 3) + 5,
			   i.e., b - laststart.

			   Insert this at the beginning of the loop
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
	       eassert (beg_interval);
	       p = beg_interval;
	       beg_interval = NULL;
	       eassert (p > pattern && p[-1] == '\\');
	       c = '{';
	       goto normal_char;

	    case '=':
	      laststart = b;
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
	      PATFETCH (c);
	      BUF_PUSH_2 (categoryspec, c);
	      break;

	    case 'C':
	      laststart = b;
	      PATFETCH (c);
	      BUF_PUSH_2 (notcategoryspec, c);
	      break;

	    case 'w':
	      laststart = b;
	      BUF_PUSH_2 (syntaxspec, Sword);
	      break;


	    case 'W':
	      laststart = b;
	      BUF_PUSH_2 (notsyntaxspec, Sword);
	      break;


	    case '<':
	      laststart = b;
	      BUF_PUSH (wordbeg);
	      break;

	    case '>':
	      laststart = b;
	      BUF_PUSH (wordend);
	      break;

	    case '_':
              laststart = b;
              PATFETCH (c);
              if (c == '<')
                BUF_PUSH (symbeg);
              else if (c == '>')
                BUF_PUSH (symend);
              else
                FREE_STACK_RETURN (REG_BADPAT);
              break;

	    case 'b':
	      BUF_PUSH (wordbound);
	      break;

	    case 'B':
	      BUF_PUSH (notwordbound);
	      break;

	    case '`':
	      BUF_PUSH (begbuf);
	      break;

	    case '\'':
	      BUF_PUSH (endbuf);
	      break;

	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      {
		regnum_t reg = c - '0';

		if (reg > bufp->re_nsub || reg < 1
		    /* Can't back reference to a subexp before its end.  */
		    || group_in_compile_stack (compile_stack, reg))
		  FREE_STACK_RETURN (REG_ESUBREG);

		laststart = b;
		BUF_PUSH_2 (duplicate, reg);
	      }
	      break;

	    default:
	      /* You might think it would be useful for \ to mean
		 not to translate; but if we don't translate it
		 it will never match anything.  */
	      goto normal_char;
	    }
	  break;


	default:
	/* Expects the character in C.  */
	normal_char:
	  /* If no exactn currently being built.  */
	  if (!pending_exact

	      /* If last exactn not at current position.  */
	      || pending_exact + *pending_exact + 1 != b

	      /* Only one byte follows the exactn for the count.  */
	      || *pending_exact >= (1 << BYTEWIDTH) - MAX_MULTIBYTE_LENGTH

	      /* If followed by a repetition operator.  */
	      || (p != pend
		  && (*p == '*' || *p == '+' || *p == '?' || *p == '^'))
	      || (p + 1 < pend && p[0] == '\\' && p[1] == '{'))
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
	      {
		c = TRANSLATE (c);
		len = CHAR_STRING (c, b);
		b += len;
	      }
	    else
	      {
		c1 = RE_CHAR_TO_MULTIBYTE (c);
		if (! CHAR_BYTE8_P (c1))
		  {
		    int c2 = TRANSLATE (c1);

		    if (c1 != c2 && (c1 = RE_CHAR_TO_UNIBYTE (c2)) >= 0)
		      c = c1;
		  }
		*b++ = c;
		len = 1;
	      }
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
  if (!posix_backtracking)
    BUF_PUSH (succeed);

  /* Success; set the length of the buffer.  */
  bufp->used = b - bufp->buffer;

#ifdef REGEX_EMACS_DEBUG
  if (regex_emacs_debug > 0)
    {
      re_compile_fastmap (bufp);
      DEBUG_PRINT ("\nCompiled pattern:\n");
      print_compiled_pattern (bufp);
    }
  regex_emacs_debug--;
#endif

  FREE_STACK_RETURN (REG_NOERROR);

} /* regex_compile */

/* Subroutines for 'regex_compile'.  */

/* Store OP at LOC followed by two-byte integer parameter ARG.  */

static void
store_op1 (re_opcode_t op, unsigned char *loc, int arg)
{
  *loc = (unsigned char) op;
  STORE_NUMBER (loc + 1, arg);
}


/* Like 'store_op1', but for two two-byte parameters ARG1 and ARG2.  */

static void
store_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2)
{
  *loc = (unsigned char) op;
  STORE_NUMBER (loc + 1, arg1);
  STORE_NUMBER (loc + 3, arg2);
}


/* Copy the bytes from LOC to END to open up three bytes of space at LOC
   for OP followed by two-byte integer parameter ARG.  */

static void
insert_op1 (re_opcode_t op, unsigned char *loc, int arg, unsigned char *end)
{
  register unsigned char *pfrom = end;
  register unsigned char *pto = end + 3;

  while (pfrom != loc)
    *--pto = *--pfrom;

  store_op1 (op, loc, arg);
}


/* Like 'insert_op1', but for two two-byte parameters ARG1 and ARG2.  */

static void
insert_op2 (re_opcode_t op, unsigned char *loc, int arg1, int arg2,
	    unsigned char *end)
{
  register unsigned char *pfrom = end;
  register unsigned char *pto = end + 5;

  while (pfrom != loc)
    *--pto = *--pfrom;

  store_op2 (op, loc, arg1, arg2);
}


/* P points to just after a ^ in PATTERN.  Return true if that ^ comes
   after an alternative or a begin-subexpression.  Assume there is at
   least one character before the ^.  */

static bool
at_begline_loc_p (re_char *pattern, re_char *p)
{
  re_char *prev = p - 2;

  switch (*prev)
    {
    case '(': /* After a subexpression.  */
    case '|': /* After an alternative.  */
      break;

    case ':': /* After a shy subexpression.  */
      /* Skip over optional regnum.  */
      while (prev > pattern && '0' <= prev[-1] && prev[-1] <= '9')
	--prev;

      if (! (prev > pattern + 1 && prev[-1] == '?' && prev[-2] == '('))
	return false;
      prev -= 2;
      break;

    default:
      return false;
    }

  /* Count the number of preceding backslashes.  */
  p = prev;
  while (prev > pattern && prev[-1] == '\\')
    --prev;
  return (p - prev) & 1;
}


/* The dual of at_begline_loc_p.  This one is for $.  Assume there is
   at least one character after the $, i.e., 'P < PEND'.  */

static bool
at_endline_loc_p (re_char *p, re_char *pend)
{
  /* Before a subexpression or an alternative?  */
  return *p == '\\' && p + 1 < pend && (p[1] == ')' || p[1] == '|');
}


/* Returns true if REGNUM is in one of COMPILE_STACK's elements and
   false if it's not.  */

static bool
group_in_compile_stack (compile_stack_type compile_stack, regnum_t regnum)
{
  ptrdiff_t this_element;

  for (this_element = compile_stack.avail - 1;
       this_element >= 0;
       this_element--)
    if (compile_stack.stack[this_element].regnum == regnum)
      return true;

  return false;
}

/* analyze_first.
   If fastmap is non-NULL, go through the pattern and fill fastmap
   with all the possible leading chars.  If fastmap is NULL, don't
   bother filling it up (obviously) and only return whether the
   pattern could potentially match the empty string.

   Return 1  if p..pend might match the empty string.
   Return 0  if p..pend matches at least one char.
   Return -1 if fastmap was not updated accurately.  */

static int
analyze_first (re_char *p, re_char *pend, char *fastmap, bool multibyte)
{
  int j, k;
  int nbits;
  bool not;

  /* If all elements for base leading-codes in fastmap is set, this
     flag is set true.  */
  bool match_any_multibyte_characters = false;

  eassert (p);

  /* The loop below works as follows:
     - It has a working-list kept in the PATTERN_STACK and which basically
       starts by only containing a pointer to the first operation.
     - If the opcode we're looking at is a match against some set of
       chars, then we add those chars to the fastmap and go on to the
       next work element from the worklist (done via 'break').
     - If the opcode is a control operator on the other hand, we either
       ignore it (if it's meaningless at this point, such as 'start_memory')
       or execute it (if it's a jump).  If the jump has several destinations
       (i.e. 'on_failure_jump'), then we push the other destination onto the
       worklist.
     We guarantee termination by ignoring backward jumps (more or less),
     so that P is monotonically increasing.  More to the point, we
     never set P (or push) anything '<= p1'.  */

  while (p < pend)
    {
      /* P1 is used as a marker of how far back a 'on_failure_jump'
	 can go without being ignored.  It is normally equal to P
	 (which prevents any backward 'on_failure_jump') except right
	 after a plain 'jump', to allow patterns such as:
	    0: jump 10
	    3..9: <body>
	    10: on_failure_jump 3
	 as used for the *? operator.  */
      re_char *p1 = p;

      switch (*p++)
	{
	case succeed:
	  return 1;

	case duplicate:
	  /* If the first character has to match a backreference, that means
	     that the group was empty (since it already matched).  Since this
	     is the only case that interests us here, we can assume that the
	     backreference must match the empty string.  */
	  p++;
	  continue;


      /* Following are the cases which match a character.  These end
	 with 'break'.  */

	case exactn:
	  if (fastmap)
	    {
	      /* If multibyte is nonzero, the first byte of each
		 character is an ASCII or a leading code.  Otherwise,
		 each byte is a character.  Thus, this works in both
		 cases. */
	      fastmap[p[1]] = 1;
	      if (multibyte)
		{
		  /* Cover the case of matching a raw char in a
		     multibyte regexp against unibyte.	*/
		  if (CHAR_BYTE8_HEAD_P (p[1]))
		    fastmap[CHAR_TO_BYTE8 (STRING_CHAR (p + 1))] = 1;
		}
	      else
		{
		  /* For the case of matching this unibyte regex
		     against multibyte, we must set a leading code of
		     the corresponding multibyte character.  */
		  int c = RE_CHAR_TO_MULTIBYTE (p[1]);

		  fastmap[CHAR_LEADING_CODE (c)] = 1;
		}
	    }
	  break;


	case anychar:
	  /* We could put all the chars except for \n (and maybe \0)
	     but we don't bother since it is generally not worth it.  */
	  if (!fastmap) break;
	  return -1;


	case charset_not:
	  if (!fastmap) break;
	  {
	    /* Chars beyond end of bitmap are possible matches.  */
	    for (j = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH;
		 j < (1 << BYTEWIDTH); j++)
	      fastmap[j] = 1;
	  }
	  FALLTHROUGH;
	case charset:
	  if (!fastmap) break;
	  not = (re_opcode_t) *(p - 1) == charset_not;
	  nbits = CHARSET_BITMAP_SIZE (&p[-1]) * BYTEWIDTH;
	  p++;
	  for (j = 0; j < nbits; j++)
	    if (!!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))) ^ not)
	      fastmap[j] = 1;

	  /* To match raw bytes (in the 80..ff range) against multibyte
	     strings, add their leading bytes to the fastmap.  */
	  for (j = 0x80; j < nbits; j++)
	    if (!!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))) ^ not)
	      fastmap[CHAR_LEADING_CODE (BYTE8_TO_CHAR (j))] = 1;

	  if (/* Any leading code can possibly start a character
		 which doesn't match the specified set of characters.  */
	      not
	      ||
	      /* If we can match a character class, we can match any
		 multibyte characters.  */
	      (CHARSET_RANGE_TABLE_EXISTS_P (&p[-2])
	       && CHARSET_RANGE_TABLE_BITS (&p[-2]) != 0))

	    {
	      if (match_any_multibyte_characters == false)
		{
		  for (j = MIN_MULTIBYTE_LEADING_CODE;
		       j <= MAX_MULTIBYTE_LEADING_CODE; j++)
		    fastmap[j] = 1;
		  match_any_multibyte_characters = true;
		}
	    }

	  else if (!not && CHARSET_RANGE_TABLE_EXISTS_P (&p[-2])
		   && match_any_multibyte_characters == false)
	    {
	      /* Set fastmap[I] to 1 where I is a leading code of each
		 multibyte character in the range table. */
	      int c, count;
	      unsigned char lc1, lc2;

	      /* Make P points the range table.  '+ 2' is to skip flag
		 bits for a character class.  */
	      p += CHARSET_BITMAP_SIZE (&p[-2]) + 2;

	      /* Extract the number of ranges in range table into COUNT.  */
	      EXTRACT_NUMBER_AND_INCR (count, p);
	      for (; count > 0; count--, p += 3)
		{
		  /* Extract the start and end of each range.  */
		  EXTRACT_CHARACTER (c, p);
		  lc1 = CHAR_LEADING_CODE (c);
		  p += 3;
		  EXTRACT_CHARACTER (c, p);
		  lc2 = CHAR_LEADING_CODE (c);
		  for (j = lc1; j <= lc2; j++)
		    fastmap[j] = 1;
		}
	    }
	  break;

	case syntaxspec:
	case notsyntaxspec:
	  if (!fastmap) break;
	  /* This match depends on text properties.  These end with
	     aborting optimizations.  */
	  return -1;

	case categoryspec:
	case notcategoryspec:
	  if (!fastmap) break;
	  not = (re_opcode_t)p[-1] == notcategoryspec;
	  k = *p++;
	  for (j = (1 << BYTEWIDTH); j >= 0; j--)
	    if ((CHAR_HAS_CATEGORY (j, k)) ^ not)
	      fastmap[j] = 1;

	  /* Any leading code can possibly start a character which
	     has or doesn't has the specified category.  */
	  if (match_any_multibyte_characters == false)
	    {
	      for (j = MIN_MULTIBYTE_LEADING_CODE;
		   j <= MAX_MULTIBYTE_LEADING_CODE; j++)
		fastmap[j] = 1;
	      match_any_multibyte_characters = true;
	    }
	  break;

      /* All cases after this match the empty string.  These end with
	 'continue'.  */

	case at_dot:
	case no_op:
	case begline:
	case endline:
	case begbuf:
	case endbuf:
	case wordbound:
	case notwordbound:
	case wordbeg:
	case wordend:
	case symbeg:
	case symend:
	  continue;


	case jump:
	  EXTRACT_NUMBER_AND_INCR (j, p);
	  if (j < 0)
	    /* Backward jumps can only go back to code that we've already
	       visited.  're_compile' should make sure this is true.  */
	    break;
	  p += j;
	  switch (*p)
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
	  /* Keep P1 to allow the 'on_failure_jump' we are jumping to
	     to jump back to "just after here".  */
	  FALLTHROUGH;
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
	      int r = analyze_first (p, pend, fastmap, multibyte);
	      if (r) return r;
	      p += j;
	    }
	  continue;


	case jump_n:
	  /* This code simply does not properly handle forward jump_n.  */
	  DEBUG_STATEMENT (EXTRACT_NUMBER (j, p); eassert (j < 0));
	  p += 4;
	  /* jump_n can either jump or fall through.  The (backward) jump
	     case has already been handled, so we only need to look at the
	     fallthrough case.  */
	  continue;

	case succeed_n:
	  /* If N == 0, it should be an on_failure_jump_loop instead.  */
	  DEBUG_STATEMENT (EXTRACT_NUMBER (j, p + 2); eassert (j > 0));
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
	 string does not match.  We need not follow this path further.  */
      return 0;
    } /* while p */

  /* We reached the end without matching anything.  */
  return 1;

} /* analyze_first */

/* Compute a fastmap for the compiled pattern in BUFP.
   A fastmap records which of the (1 << BYTEWIDTH) possible
   characters can start a string that matches the pattern.  This fastmap
   is used by re_search to skip quickly over impossible starting points.

   Character codes above (1 << BYTEWIDTH) are not represented in the
   fastmap, but the leading codes are represented.  Thus, the fastmap
   indicates which character sets could start a match.

   The caller must supply the address of a (1 << BYTEWIDTH)-byte data
   area as BUFP->fastmap.

   Set the 'fastmap', 'fastmap_accurate', and 'can_be_null' fields in
   the pattern buffer.  */

static void
re_compile_fastmap (struct re_pattern_buffer *bufp)
{
  char *fastmap = bufp->fastmap;
  int analysis;

  eassert (fastmap && bufp->buffer);

  memset (fastmap, 0, 1 << BYTEWIDTH);  /* Assume nothing's valid.  */

  /* FIXME: Is the following assignment correct even when ANALYSIS < 0?  */
  bufp->fastmap_accurate = 1;	    /* It will be when we're done.  */

  analysis = analyze_first (bufp->buffer, bufp->buffer + bufp->used,
			    fastmap, RE_MULTIBYTE_P (bufp));
  bufp->can_be_null = (analysis != 0);
} /* re_compile_fastmap */

/* Set REGS to hold NUM_REGS registers, storing them in STARTS and
   ENDS.  Subsequent matches using PATTERN_BUFFER and REGS will use
   this memory for recording register information.  STARTS and ENDS
   must be allocated using the malloc library routine, and must each
   be at least NUM_REGS * sizeof (ptrdiff_t) bytes long.

   If NUM_REGS == 0, then subsequent matches should allocate their own
   register data.

   Unless this function is called, the first search or match using
   PATTERN_BUFFER will allocate its own register data, without
   freeing the old data.  */

void
re_set_registers (struct re_pattern_buffer *bufp, struct re_registers *regs,
		  ptrdiff_t num_regs, ptrdiff_t *starts, ptrdiff_t *ends)
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
      regs->start = regs->end = 0;
    }
}

/* Searching routines.  */

/* Like re_search_2, below, but only one string is specified, and
   doesn't let you say where to stop matching. */

ptrdiff_t
re_search (struct re_pattern_buffer *bufp, const char *string, ptrdiff_t size,
	   ptrdiff_t startpos, ptrdiff_t range, struct re_registers *regs)
{
  return re_search_2 (bufp, NULL, 0, string, size, startpos, range,
		      regs, size);
}

/* Head address of virtual concatenation of string.  */
#define HEAD_ADDR_VSTRING(P)		\
  (((P) >= size1 ? string2 : string1))

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

   Return either the position in the strings at which the match was
   found, -1 if no match, or -2 if error (such as failure
   stack overflow).  */

ptrdiff_t
re_search_2 (struct re_pattern_buffer *bufp, const char *str1, ptrdiff_t size1,
	     const char *str2, ptrdiff_t size2,
	     ptrdiff_t startpos, ptrdiff_t range,
	     struct re_registers *regs, ptrdiff_t stop)
{
  ptrdiff_t val;
  re_char *string1 = (re_char *) str1;
  re_char *string2 = (re_char *) str2;
  char *fastmap = bufp->fastmap;
  Lisp_Object translate = bufp->translate;
  ptrdiff_t total_size = size1 + size2;
  ptrdiff_t endpos = startpos + range;
  bool anchored_start;
  /* Nonzero if we are searching multibyte string.  */
  bool multibyte = RE_TARGET_MULTIBYTE_P (bufp);

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

  /* In a forward search for something that starts with \=.
     don't keep searching past point.  */
  if (bufp->used > 0 && (re_opcode_t) bufp->buffer[0] == at_dot && range > 0)
    {
      range = PT_BYTE - BEGV_BYTE - startpos;
      if (range < 0)
	return -1;
    }

  /* Update the fastmap now if not correct already.  */
  if (fastmap && !bufp->fastmap_accurate)
    re_compile_fastmap (bufp);

  /* See whether the pattern is anchored.  */
  anchored_start = (bufp->buffer[0] == begline);

  gl_state.object = re_match_object; /* Used by SYNTAX_TABLE_BYTE_TO_CHAR. */
  {
    ptrdiff_t charpos = SYNTAX_TABLE_BYTE_TO_CHAR (POS_AS_IN_BUFFER (startpos));

    SETUP_SYNTAX_TABLE_FOR_OBJECT (re_match_object, charpos, 1);
  }

  /* Loop through the string, looking for a place to start matching.  */
  for (;;)
    {
      /* If the pattern is anchored,
	 skip quickly past places we cannot match.
	 Don't bother to treat startpos == 0 specially
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
	 the first null string.  */
      if (fastmap && startpos < total_size && !bufp->can_be_null)
	{
	  re_char *d;
	  int buf_ch;

	  d = POS_ADDR_VSTRING (startpos);

	  if (range > 0)	/* Searching forwards.  */
	    {
	      ptrdiff_t irange = range, lim = 0;

	      if (startpos < size1 && startpos + range >= size1)
		lim = range - (size1 - startpos);

	      /* Written out as an if-else to avoid testing 'translate'
		 inside the loop.  */
	      if (!NILP (translate))
		{
		  if (multibyte)
		    while (range > lim)
		      {
			int buf_charlen;

			buf_ch = STRING_CHAR_AND_LENGTH (d, buf_charlen);
			buf_ch = RE_TRANSLATE (translate, buf_ch);
			if (fastmap[CHAR_LEADING_CODE (buf_ch)])
			  break;

			range -= buf_charlen;
			d += buf_charlen;
		      }
		  else
		    while (range > lim)
		      {
			buf_ch = *d;
			int ch = RE_CHAR_TO_MULTIBYTE (buf_ch);
			int translated = RE_TRANSLATE (translate, ch);
			if (translated != ch
			    && (ch = RE_CHAR_TO_UNIBYTE (translated)) >= 0)
			  buf_ch = ch;
			if (fastmap[buf_ch])
			  break;
			d++;
			range--;
		      }
		}
	      else
		{
		  if (multibyte)
		    while (range > lim)
		      {
			int buf_charlen;

			buf_ch = STRING_CHAR_AND_LENGTH (d, buf_charlen);
			if (fastmap[CHAR_LEADING_CODE (buf_ch)])
			  break;
			range -= buf_charlen;
			d += buf_charlen;
		      }
		  else
		    while (range > lim && !fastmap[*d])
		      {
			d++;
			range--;
		      }
		}
	      startpos += irange - range;
	    }
	  else				/* Searching backwards.  */
	    {
	      if (multibyte)
		{
		  buf_ch = STRING_CHAR (d);
		  buf_ch = TRANSLATE (buf_ch);
		  if (! fastmap[CHAR_LEADING_CODE (buf_ch)])
		    goto advance;
		}
	      else
		{
		  buf_ch = *d;
		  int ch = RE_CHAR_TO_MULTIBYTE (buf_ch);
		  int translated = TRANSLATE (ch);
		  if (translated != ch
		      && (ch = RE_CHAR_TO_UNIBYTE (translated)) >= 0)
		    buf_ch = ch;
		  if (! fastmap[TRANSLATE (buf_ch)])
		    goto advance;
		}
	    }
	}

      /* If can't match the null string, and that's all we have left, fail.  */
      if (range >= 0 && startpos == total_size && fastmap
	  && !bufp->can_be_null)
	return -1;

      val = re_match_2_internal (bufp, string1, size1, string2, size2,
				 startpos, regs, stop);

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
	      int len = BYTES_BY_CHAR_HEAD (*p);

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
	      re_char *p = POS_ADDR_VSTRING (startpos) + 1;
	      re_char *p0 = p;
	      re_char *phead = HEAD_ADDR_VSTRING (startpos);

	      /* Find the head of multibyte form.  */
	      PREV_CHAR_BOUNDARY (p, phead);
	      range += p0 - 1 - p;
	      if (range > 0)
		break;

	      startpos -= p0 - 1 - p;
	    }
	}
    }
  return -1;
} /* re_search_2 */

/* Declarations and macros for re_match_2.  */

static bool bcmp_translate (re_char *, re_char *, ptrdiff_t,
			    Lisp_Object, bool);

/* This converts PTR, a pointer into one of the search strings 'string1'
   and 'string2' into an offset from the beginning of that string.  */
#define POINTER_TO_OFFSET(ptr)			\
  (FIRST_STRING_P (ptr)				\
   ? (ptr) - string1				\
   : (ptr) - string2 + (ptrdiff_t) size1)

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
   outside of the LIMITs (i.e past 'stop').  */
#define PREFETCH_NOLIMIT()						\
  if (d == end1)							\
     {									\
       d = string2;							\
       dend = end_match_2;						\
     }

/* Test if at very beginning or at very end of the virtual concatenation
   of STRING1 and STRING2.  If only one string, it's STRING2.  */
#define AT_STRINGS_BEG(d) ((d) == (size1 ? string1 : string2) || !size2)
#define AT_STRINGS_END(d) ((d) == end2)

/* Disabled due to a compiler bug -- see comment at case wordbound */

/* The comment at case wordbound is following one, but we don't use
   AT_WORD_BOUNDARY anymore to support multibyte form.

   The DEC Alpha C compiler 3.x generates incorrect code for the
   test	 WORDCHAR_P (d - 1) != WORDCHAR_P (d)  in the expansion of
   AT_WORD_BOUNDARY, so this code is disabled.  Expanding the
   macro and introducing temporary variables works around the bug.  */

#if 0
/* Test if D points to a character which is word-constituent.  We have
   two special cases to check for: if past the end of string1, look at
   the first character in string2; and if before the beginning of
   string2, look at the last character in string1.  */
#define WORDCHAR_P(d)							\
  (SYNTAX ((d) == end1 ? *string2					\
	   : (d) == string2 - 1 ? *(end1 - 1) : *(d))			\
   == Sword)

/* Test if the character before D and the one at D differ with respect
   to being word-constituent.  */
#define AT_WORD_BOUNDARY(d)						\
  (AT_STRINGS_BEG (d) || AT_STRINGS_END (d)				\
   || WORDCHAR_P (d - 1) != WORDCHAR_P (d))
#endif


/* Optimization routines.  */

/* If the operation is a match against one or more chars,
   return a pointer to the next operation, else return NULL.  */
static re_char *
skip_one_char (re_char *p)
{
  switch (*p++)
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
    case categoryspec:
    case notcategoryspec:
      p++;
      break;

    default:
      p = NULL;
    }
  return p;
}


/* Jump over non-matching operations.  */
static re_char *
skip_noops (re_char *p, re_char *pend)
{
  int mcnt;
  while (p < pend)
    {
      switch (*p)
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
  eassert (p == pend);
  return p;
}

/* Test if C matches charset op.  *PP points to the charset or charset_not
   opcode.  When the function finishes, *PP will be advanced past that opcode.
   C is character to test (possibly after translations) and CORIG is original
   character (i.e. without any translations).  UNIBYTE denotes whether c is
   unibyte or multibyte character. */
static bool
execute_charset (re_char **pp, int c, int corig, bool unibyte)
{
  eassume (0 <= c && 0 <= corig);
  re_char *p = *pp, *rtp = NULL;
  bool not = (re_opcode_t) *p == charset_not;

  if (CHARSET_RANGE_TABLE_EXISTS_P (p))
    {
      int count;
      rtp = CHARSET_RANGE_TABLE (p);
      EXTRACT_NUMBER_AND_INCR (count, rtp);
      *pp = CHARSET_RANGE_TABLE_END ((rtp), (count));
    }
  else
    *pp += 2 + CHARSET_BITMAP_SIZE (p);

  if (unibyte && c < (1 << BYTEWIDTH))
    {			/* Lookup bitmap.  */
      /* Cast to 'unsigned' instead of 'unsigned char' in
	 case the bit list is a full 32 bytes long.  */
      if (c < (unsigned) (CHARSET_BITMAP_SIZE (p) * BYTEWIDTH)
	  && p[2 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
	return !not;
    }
  else if (rtp)
    {
      int class_bits = CHARSET_RANGE_TABLE_BITS (p);
      int range_start, range_end;

  /* Sort tests by the most commonly used classes with some adjustment to which
     tests are easiest to perform.  Take a look at comment in re_wctype_parse
     for table with frequencies of character class names. */

      if ((class_bits & BIT_MULTIBYTE) ||
	  (class_bits & BIT_ALNUM && ISALNUM (c)) ||
	  (class_bits & BIT_ALPHA && ISALPHA (c)) ||
	  (class_bits & BIT_SPACE && ISSPACE (c)) ||
          (class_bits & BIT_BLANK && ISBLANK (c)) ||
	  (class_bits & BIT_WORD  && ISWORD  (c)) ||
	  ((class_bits & BIT_UPPER) &&
	   (ISUPPER (c) || (corig != c &&
			    c == downcase (corig) && ISLOWER (c)))) ||
	  ((class_bits & BIT_LOWER) &&
	   (ISLOWER (c) || (corig != c &&
			    c == upcase (corig) && ISUPPER(c)))) ||
	  (class_bits & BIT_PUNCT && ISPUNCT (c)) ||
	  (class_bits & BIT_GRAPH && ISGRAPH (c)) ||
	  (class_bits & BIT_PRINT && ISPRINT (c)))
	return !not;

      for (p = *pp; rtp < p; rtp += 2 * 3)
	{
	  EXTRACT_CHARACTER (range_start, rtp);
	  EXTRACT_CHARACTER (range_end, rtp + 3);
	  if (range_start <= c && c <= range_end)
	    return !not;
	}
    }

  return not;
}

/* True if "p1 matches something" implies "p2 fails".  */
static bool
mutually_exclusive_p (struct re_pattern_buffer *bufp, re_char *p1,
		      re_char *p2)
{
  re_opcode_t op2;
  bool multibyte = RE_MULTIBYTE_P (bufp);
  unsigned char *pend = bufp->buffer + bufp->used;

  eassert (p1 >= bufp->buffer && p1 < pend
	   && p2 >= bufp->buffer && p2 <= pend);

  /* Skip over open/close-group commands.
     If what follows this loop is a ...+ construct,
     look at what begins its body, since we will have to
     match at least one of that.  */
  p2 = skip_noops (p2, pend);
  /* The same skip can be done for p1, except that this function
     is only used in the case where p1 is a simple match operator.  */
  /* p1 = skip_noops (p1, pend); */

  eassert (p1 >= bufp->buffer && p1 < pend
	   && p2 >= bufp->buffer && p2 <= pend);

  op2 = p2 == pend ? succeed : *p2;

  switch (op2)
    {
    case succeed:
    case endbuf:
      /* If we're at the end of the pattern, we can change.  */
      if (skip_one_char (p1))
	{
	  DEBUG_PRINT ("  End of pattern: fast loop.\n");
	  return true;
	}
      break;

    case endline:
    case exactn:
      {
	int c
	  = (re_opcode_t) *p2 == endline ? '\n'
	  : RE_STRING_CHAR (p2 + 2, multibyte);

	if ((re_opcode_t) *p1 == exactn)
	  {
	    if (c != RE_STRING_CHAR (p1 + 2, multibyte))
	      {
		DEBUG_PRINT ("  '%c' != '%c' => fast loop.\n", c, p1[2]);
		return true;
	      }
	  }

	else if ((re_opcode_t) *p1 == charset
		 || (re_opcode_t) *p1 == charset_not)
	  {
	    if (!execute_charset (&p1, c, c, !multibyte || ASCII_CHAR_P (c)))
	      {
		DEBUG_PRINT ("	 No match => fast loop.\n");
		return true;
	      }
	  }
	else if ((re_opcode_t) *p1 == anychar
		 && c == '\n')
	  {
	    DEBUG_PRINT ("   . != \\n => fast loop.\n");
	    return true;
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
	     So, for the size of bitmap in P2, 'p2[1]' is
	     enough.  But P1 may have range table, so the
	     size of bitmap table of P1 is extracted by
	     using macro 'CHARSET_BITMAP_SIZE'.

	     In a multibyte case, we know that all the character
	     listed in P2 is ASCII.  In a unibyte case, P1 has only a
	     bitmap table.  So, in both cases, it is enough to test
	     only the bitmap table of P1.  */

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
		  DEBUG_PRINT ("	 No match => fast loop.\n");
		  return true;
		}
	    }
	  else if ((re_opcode_t) *p1 == charset_not)
	    {
	      int idx;
	      /* We win if the charset_not inside the loop lists
		 every character listed in the charset after.  */
	      for (idx = 0; idx < (int) p2[1]; idx++)
		if (! (p2[2 + idx] == 0
		       || (idx < CHARSET_BITMAP_SIZE (p1)
			   && ((p2[2 + idx] & ~ p1[2 + idx]) == 0))))
		  break;

	      if (idx == p2[1])
		{
		  DEBUG_PRINT ("	 No match => fast loop.\n");
		  return true;
		}
	      }
	  }
      }
      break;

    case charset_not:
      switch (*p1)
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
	  break;
	}
      break;

    case wordend:
      return ((re_opcode_t) *p1 == syntaxspec && p1[1] == Sword);
    case symend:
      return ((re_opcode_t) *p1 == syntaxspec
              && (p1[1] == Ssymbol || p1[1] == Sword));
    case notsyntaxspec:
      return ((re_opcode_t) *p1 == syntaxspec && p1[1] == p2[1]);

    case wordbeg:
      return ((re_opcode_t) *p1 == notsyntaxspec && p1[1] == Sword);
    case symbeg:
      return ((re_opcode_t) *p1 == notsyntaxspec
              && (p1[1] == Ssymbol || p1[1] == Sword));
    case syntaxspec:
      return ((re_opcode_t) *p1 == notsyntaxspec && p1[1] == p2[1]);

    case wordbound:
      return (((re_opcode_t) *p1 == notsyntaxspec
	       || (re_opcode_t) *p1 == syntaxspec)
	      && p1[1] == Sword);

    case categoryspec:
      return ((re_opcode_t) *p1 == notcategoryspec && p1[1] == p2[1]);
    case notcategoryspec:
      return ((re_opcode_t) *p1 == categoryspec && p1[1] == p2[1]);

    default:
      ;
    }

  /* Safe default.  */
  return false;
}


/* Matching routines.  */

/* re_match_2 matches the compiled pattern in BUFP against the
   the (virtual) concatenation of STRING1 and STRING2 (of length SIZE1
   and SIZE2, respectively).  We start matching at POS, and stop
   matching at STOP.

   If REGS is non-null, store offsets for the substring each group
   matched in REGS.

   We return -1 if no match, -2 if an internal error (such as the
   failure stack overflowing).  Otherwise, we return the length of the
   matched substring.  */

ptrdiff_t
re_match_2 (struct re_pattern_buffer *bufp,
	    char const *string1, ptrdiff_t size1,
	    char const *string2, ptrdiff_t size2,
	    ptrdiff_t pos, struct re_registers *regs, ptrdiff_t stop)
{
  ptrdiff_t result;

  ptrdiff_t charpos;
  gl_state.object = re_match_object; /* Used by SYNTAX_TABLE_BYTE_TO_CHAR. */
  charpos = SYNTAX_TABLE_BYTE_TO_CHAR (POS_AS_IN_BUFFER (pos));
  SETUP_SYNTAX_TABLE_FOR_OBJECT (re_match_object, charpos, 1);

  result = re_match_2_internal (bufp, (re_char *) string1, size1,
				(re_char *) string2, size2,
				pos, regs, stop);
  return result;
}

static void
unwind_re_match (void *ptr)
{
  struct buffer *b = (struct buffer *) ptr;
  b->text->inhibit_shrinking = 0;
}

/* This is a separate function so that we can force an alloca cleanup
   afterwards.  */
static ptrdiff_t
re_match_2_internal (struct re_pattern_buffer *bufp,
		     re_char *string1, ptrdiff_t size1,
		     re_char *string2, ptrdiff_t size2,
		     ptrdiff_t pos, struct re_registers *regs, ptrdiff_t stop)
{
  /* General temporaries.  */
  int mcnt;

  /* Just past the end of the corresponding string.  */
  re_char *end1, *end2;

  /* Pointers into string1 and string2, just past the last characters in
     each to consider matching.  */
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
  Lisp_Object translate = bufp->translate;

  /* True if BUFP is setup from a multibyte regex.  */
  bool multibyte = RE_MULTIBYTE_P (bufp);

  /* True if STRING1/STRING2 are multibyte.  */
  bool target_multibyte = RE_TARGET_MULTIBYTE_P (bufp);

  /* Failure point stack.  Each place that can handle a failure further
     down the line pushes a failure point on this stack.  It consists of
     regstart, and regend for all registers corresponding to
     the subexpressions we're currently inside, plus the number of such
     registers, and, finally, two char *'s.  The first char * is where
     to resume scanning the pattern; the second one is where to resume
     scanning the strings.  */
  fail_stack_type fail_stack;
#ifdef DEBUG_COMPILES_ARGUMENTS
  ptrdiff_t nfailure_points_pushed = 0, nfailure_points_popped = 0;
#endif

  /* We fill all the registers internally, independent of what we
     return, for use in backreferences.  The number here includes
     an element for register zero.  */
  ptrdiff_t num_regs = bufp->re_nsub + 1;
  eassume (0 < num_regs);

  /* Information on the contents of registers. These are pointers into
     the input strings; they record just what was matched (on this
     attempt) by a subexpression part of the pattern, that is, the
     regnum-th regstart pointer points to where in the pattern we began
     matching and the regnum-th regend points to right after where we
     stopped matching the regnum-th subexpression.  (The zeroth register
     keeps track of what the whole pattern matches.)  */
  re_char **regstart UNINIT, **regend UNINIT;

  /* The following record the register info as found in the above
     variables when we find a match better than any we've seen before.
     This happens as we backtrack through the failure points, which in
     turn happens only if we have not yet matched the entire string. */
  bool best_regs_set = false;
  re_char **best_regstart UNINIT, **best_regend UNINIT;

  /* Logically, this is 'best_regend[0]'.  But we don't want to have to
     allocate space for that if we're not allocating space for anything
     else (see below).  Also, we never need info about register 0 for
     any of the other register vectors, and it seems rather a kludge to
     treat 'best_regend' differently from the rest.  So we keep track of
     the end of the best match so far in a separate variable.  We
     initialize this to NULL so that when we backtrack the first time
     and need to test it, it's not garbage.  */
  re_char *match_end = NULL;

#ifdef DEBUG_COMPILES_ARGUMENTS
  /* Counts the total number of registers pushed.  */
  ptrdiff_t num_regs_pushed = 0;
#endif

  DEBUG_PRINT ("\nEntering re_match_2.\n");

  REGEX_USE_SAFE_ALLOCA;

  INIT_FAIL_STACK ();

  ptrdiff_t count = SPECPDL_INDEX ();

  /* Prevent shrinking and relocation of buffer text if GC happens
     while we are inside this function.  The calls to
     UPDATE_SYNTAX_TABLE_* macros can call Lisp (via
     `internal--syntax-propertize`); these calls are careful to defend against
     buffer modifications, but even with no modifications, the buffer text may
     be relocated during GC by `compact_buffer` which would invalidate
     our C pointers to buffer text.  */
  if (!current_buffer->text->inhibit_shrinking)
    {
      record_unwind_protect_ptr (unwind_re_match, current_buffer);
      current_buffer->text->inhibit_shrinking = 1;
    }

  /* Do not bother to initialize all the register variables if there are
     no groups in the pattern, as it takes a fair amount of time.  If
     there are groups, we include space for register 0 (the whole
     pattern), even though we never use it, since it simplifies the
     array indexing.  We should fix this.  */
  if (bufp->re_nsub)
    {
      regstart = SAFE_ALLOCA (num_regs * 4 * sizeof *regstart);
      regend = regstart + num_regs;
      best_regstart = regend + num_regs;
      best_regend = best_regstart + num_regs;
    }

  /* The starting position is bogus.  */
  if (pos < 0 || pos > size1 + size2)
    {
      unbind_to (count, Qnil);
      SAFE_FREE ();
      return -1;
    }

  /* Initialize subexpression text positions to -1 to mark ones that no
     start_memory/stop_memory has been seen for.  */
  for (ptrdiff_t reg = 1; reg < num_regs; reg++)
    regstart[reg] = regend[reg] = NULL;

  /* We move 'string1' into 'string2' if the latter's empty -- but not if
     'string1' is null.  */
  if (size2 == 0 && string1 != NULL)
    {
      string2 = string1;
      size2 = size1;
      string1 = 0;
      size1 = 0;
    }
  end1 = string1 + size1;
  end2 = string2 + size2;

  /* P scans through the pattern as D scans through the data.
     DEND is the end of the input string that D points within.
     Advance D into the following input string whenever necessary, but
     this happens before fetching; therefore, at the beginning of the
     loop, D can be pointing at the end of a string, but it cannot
     equal STRING2.  */
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
	     makes us jump from 'stop' to 'gap' within the string.
	     What we really want here is for the search to stop as
	     soon as we hit end_match_1.  That's why we set end_match_2
	     to end_match_1 (since PREFETCH fails as soon as we hit
	     end_match_2).  */
	  end_match_2 = end_match_1;
	}
      else
	{ /* It's important to use this code when STOP == SIZE so that
	     moving D from end1 to string2 will not prevent the D == DEND
	     check from catching the end of string.  */
	  end_match_1 = end1;
	  end_match_2 = string2 + stop - size1;
	}
      d = string1 + pos;
      dend = end_match_1;
    }

  DEBUG_PRINT ("The compiled pattern is:\n");
  DEBUG_PRINT_COMPILED_PATTERN (bufp, p, pend);
  DEBUG_PRINT ("The string to match is: \"");
  DEBUG_PRINT_DOUBLE_STRING (d, string1, size1, string2, size2);
  DEBUG_PRINT ("\"\n");

  /* This loops over pattern commands.  It exits by returning from the
     function if the match is complete, or it drops through if the match
     fails at this starting point in the input data.  */
  for (;;)
    {
      DEBUG_PRINT ("\n%p: ", p);

      if (p == pend)
	{
	  /* End of pattern means we might have succeeded.  */
	  DEBUG_PRINT ("end of pattern ... ");

	  /* If we haven't matched the entire string, and we want the
	     longest match, try backtracking.  */
	  if (d != end_match_2)
	    {
	      /* True if this match is the best seen so far.  */
	      bool best_match_p;

	      {
		/* True if this match ends in the same string (string1
		   or string2) as the best previous match.  */
		bool same_str_p = (FIRST_STRING_P (match_end)
				   == FIRST_STRING_P (d));

		/* AIX compiler got confused when this was combined
		   with the previous declaration.  */
		if (same_str_p)
		  best_match_p = d > match_end;
		else
		  best_match_p = !FIRST_STRING_P (d);
	      }

	      DEBUG_PRINT ("backtracking.\n");

	      if (!FAIL_STACK_EMPTY ())
		{ /* More failure points to try.  */

		  /* If exceeds best match so far, save it.  */
		  if (!best_regs_set || best_match_p)
		    {
		      best_regs_set = true;
		      match_end = d;

		      DEBUG_PRINT ("\nSAVING match as best so far.\n");

		      for (ptrdiff_t reg = 1; reg < num_regs; reg++)
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
		  /* Restore best match.  It may happen that 'dend ==
		     end_match_1' while the restored d is in string2.
		     For example, the pattern 'x.*y.*z' against the
		     strings 'x-' and 'y-z-', if the two strings are
		     not consecutive in memory.  */
		  DEBUG_PRINT ("Restoring best registers.\n");

		  d = match_end;
		  dend = ((d >= string1 && d <= end1)
			   ? end_match_1 : end_match_2);

		  for (ptrdiff_t reg = 1; reg < num_regs; reg++)
		    {
		      regstart[reg] = best_regstart[reg];
		      regend[reg] = best_regend[reg];
		    }
		}
	    } /* d != end_match_2 */

	succeed_label:
	  DEBUG_PRINT ("Accepting match.\n");

	  /* If caller wants register contents data back, do it.  */
	  if (regs)
	    {
	      /* Have the register data arrays been allocated?	*/
	      if (bufp->regs_allocated == REGS_UNALLOCATED)
		{ /* No.  So allocate them with malloc.  */
		  ptrdiff_t n = max (RE_NREGS, num_regs);
		  regs->start = xnmalloc (n, sizeof *regs->start);
		  regs->end = xnmalloc (n, sizeof *regs->end);
		  regs->num_regs = n;
		  bufp->regs_allocated = REGS_REALLOCATE;
		}
	      else if (bufp->regs_allocated == REGS_REALLOCATE)
		{ /* Yes.  If we need more elements than were already
		     allocated, reallocate them.  If we need fewer, just
		     leave it alone.  */
		  ptrdiff_t n = regs->num_regs;
		  if (n < num_regs)
		    {
		      n = max (n + (n >> 1), num_regs);
		      regs->start
			= xnrealloc (regs->start, n, sizeof *regs->start);
		      regs->end = xnrealloc (regs->end, n, sizeof *regs->end);
		      regs->num_regs = n;
		    }
		}
	      else
		eassert (bufp->regs_allocated == REGS_FIXED);

	      /* Convert the pointer data in 'regstart' and 'regend' to
		 indices.  Register zero has to be set differently,
		 since we haven't kept track of any info for it.  */
	      if (regs->num_regs > 0)
		{
		  regs->start[0] = pos;
		  regs->end[0] = POINTER_TO_OFFSET (d);
		}

	      for (ptrdiff_t reg = 1; reg < num_regs; reg++)
		{
		  if (REG_UNSET (regstart[reg]) || REG_UNSET (regend[reg]))
		    regs->start[reg] = regs->end[reg] = -1;
		  else
		    {
		      regs->start[reg] = POINTER_TO_OFFSET (regstart[reg]);
		      regs->end[reg] = POINTER_TO_OFFSET (regend[reg]);
		    }
		}

	      /* If the regs structure we return has more elements than
		 were in the pattern, set the extra elements to -1.  */
	      for (ptrdiff_t reg = num_regs; reg < regs->num_regs; reg++)
		regs->start[reg] = regs->end[reg] = -1;
	    }

	  DEBUG_PRINT ("%td failure points pushed, %td popped (%td remain).\n",
		       nfailure_points_pushed, nfailure_points_popped,
		       nfailure_points_pushed - nfailure_points_popped);
	  DEBUG_PRINT ("%td registers pushed.\n", num_regs_pushed);

	  ptrdiff_t dcnt = POINTER_TO_OFFSET (d) - pos;

	  DEBUG_PRINT ("Returning %td from re_match_2.\n", dcnt);

	  unbind_to (count, Qnil);
	  SAFE_FREE ();
	  return dcnt;
	}

      /* Otherwise match next pattern command.  */
      switch (*p++)
	{
	/* Ignore these.  Used to ignore the n of succeed_n's which
	   currently have n == 0.  */
	case no_op:
	  DEBUG_PRINT ("EXECUTING no_op.\n");
	  break;

	case succeed:
	  DEBUG_PRINT ("EXECUTING succeed.\n");
	  goto succeed_label;

	/* Match the next n pattern characters exactly.  The following
	   byte in the pattern defines n, and the n bytes after that
	   are the characters to match.  */
	case exactn:
	  mcnt = *p++;
	  DEBUG_PRINT ("EXECUTING exactn %d.\n", mcnt);

	  /* Remember the start point to rollback upon failure.  */
	  dfail = d;

	  /* The cost of testing 'translate' is comparatively small.  */
	  if (target_multibyte)
	    do
	      {
		int pat_charlen, buf_charlen;
		int pat_ch, buf_ch;

		PREFETCH ();
		if (multibyte)
		  pat_ch = STRING_CHAR_AND_LENGTH (p, pat_charlen);
		else
		  {
		    pat_ch = RE_CHAR_TO_MULTIBYTE (*p);
		    pat_charlen = 1;
		  }
		buf_ch = STRING_CHAR_AND_LENGTH (d, buf_charlen);

		if (TRANSLATE (buf_ch) != pat_ch)
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
		int pat_charlen;
		int pat_ch, buf_ch;

		PREFETCH ();
		if (multibyte)
		  {
		    pat_ch = STRING_CHAR_AND_LENGTH (p, pat_charlen);
		    pat_ch = RE_CHAR_TO_UNIBYTE (pat_ch);
		  }
		else
		  {
		    pat_ch = *p;
		    pat_charlen = 1;
		  }
		buf_ch = RE_CHAR_TO_MULTIBYTE (*d);
		if (! CHAR_BYTE8_P (buf_ch))
		  {
		    buf_ch = TRANSLATE (buf_ch);
		    buf_ch = RE_CHAR_TO_UNIBYTE (buf_ch);
		    if (buf_ch < 0)
		      buf_ch = *d;
		  }
		else
		  buf_ch = *d;
		if (buf_ch != pat_ch)
		  {
		    d = dfail;
		    goto fail;
		  }
		p += pat_charlen;
		d++;
		mcnt -= pat_charlen;
	      }
	    while (mcnt > 0);

	  break;


	/* Match any character except newline.  */
	case anychar:
	  {
	    int buf_charlen;
	    int buf_ch;

	    DEBUG_PRINT ("EXECUTING anychar.\n");

	    PREFETCH ();
	    buf_ch = RE_STRING_CHAR_AND_LENGTH (d, buf_charlen,
						target_multibyte);
	    buf_ch = TRANSLATE (buf_ch);
	    if (buf_ch == '\n')
	      goto fail;

	    DEBUG_PRINT ("  Matched \"%d\".\n", *d);
	    d += buf_charlen;
	  }
	  break;


	case charset:
	case charset_not:
	  {
	    /* Whether matching against a unibyte character.  */
	    bool unibyte_char = false;

	    DEBUG_PRINT ("EXECUTING charset%s.\n",
			 (re_opcode_t) *(p - 1) == charset_not ? "_not" : "");

	    PREFETCH ();
	    int len;
	    int corig = RE_STRING_CHAR_AND_LENGTH (d, len, target_multibyte);
	    int c = corig;
	    if (target_multibyte)
	      {
		int c1;

		c = TRANSLATE (c);
		c1 = RE_CHAR_TO_UNIBYTE (c);
		if (c1 >= 0)
		  {
		    unibyte_char = true;
		    c = c1;
		  }
	      }
	    else
	      {
		int c1 = RE_CHAR_TO_MULTIBYTE (c);

		if (! CHAR_BYTE8_P (c1))
		  {
		    c1 = TRANSLATE (c1);
		    c1 = RE_CHAR_TO_UNIBYTE (c1);
		    if (c1 >= 0)
		      {
			unibyte_char = true;
			c = c1;
		      }
		  }
		else
		  unibyte_char = true;
	      }

	    p -= 1;
	    if (!execute_charset (&p, c, corig, unibyte_char))
	      goto fail;

	    d += len;
	  }
	  break;


	/* The beginning of a group is represented by start_memory.
	   The argument is the register number.  The text
	   matched within the group is recorded (in the internal
	   registers data structure) under the register number.  */
	case start_memory:
	  DEBUG_PRINT ("EXECUTING start_memory %d:\n", *p);

	  /* In case we need to undo this operation (via backtracking).  */
	  PUSH_FAILURE_REG (*p);

	  regstart[*p] = d;
	  regend[*p] = NULL;	/* probably unnecessary.  -sm  */
	  DEBUG_PRINT ("  regstart: %td\n", POINTER_TO_OFFSET (regstart[*p]));

	  /* Move past the register number and inner group count.  */
	  p += 1;
	  break;


	/* The stop_memory opcode represents the end of a group.  Its
	   argument is the same as start_memory's: the register number.  */
	case stop_memory:
	  DEBUG_PRINT ("EXECUTING stop_memory %d:\n", *p);

	  eassert (!REG_UNSET (regstart[*p]));
	  /* Strictly speaking, there should be code such as:

		eassert (REG_UNSET (regend[*p]));
		PUSH_FAILURE_REGSTOP (*p);

	     But the only info to be pushed is regend[*p] and it is known to
	     be UNSET, so there really isn't anything to push.
	     Not pushing anything, on the other hand deprives us from the
	     guarantee that regend[*p] is UNSET since undoing this operation
	     will not reset its value properly.  This is not important since
	     the value will only be read on the next start_memory or at
	     the very end and both events can only happen if this stop_memory
	     is *not* undone.  */

	  regend[*p] = d;
	  DEBUG_PRINT ("      regend: %td\n", POINTER_TO_OFFSET (regend[*p]));

	  /* Move past the register number and the inner group count.  */
	  p += 1;
	  break;


	/* \<digit> has been turned into a 'duplicate' command which is
	   followed by the numeric value of <digit> as the register number.  */
	case duplicate:
	  {
	    re_char *d2, *dend2;
	    int regno = *p++;	/* Get which register to match against.  */
	    DEBUG_PRINT ("EXECUTING duplicate %d.\n", regno);

	    /* Can't back reference a group which we've never matched.  */
	    if (REG_UNSET (regstart[regno]) || REG_UNSET (regend[regno]))
	      goto fail;

	    /* Where in input to try to start matching.  */
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
		ptrdiff_t dcnt;

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
		dcnt = dend - d;

		/* Want how many consecutive characters we can match in
		   one shot, so, if necessary, adjust the count.  */
		if (dcnt > dend2 - d2)
		  dcnt = dend2 - d2;

		/* Compare that many; failure if mismatch, else move
		   past them.  */
		if (!NILP (translate)
		    ? bcmp_translate (d, d2, dcnt, translate, target_multibyte)
		    : memcmp (d, d2, dcnt))
		  {
		    d = dfail;
		    goto fail;
		  }
		d += dcnt, d2 += dcnt;
	      }
	  }
	  break;


	/* begline matches the empty string at the beginning of the string,
	   and after newlines.  */
	case begline:
	  DEBUG_PRINT ("EXECUTING begline.\n");

	  if (AT_STRINGS_BEG (d))
	    break;
	  else
	    {
	      unsigned c;
	      GET_CHAR_BEFORE_2 (c, d, string1, end1, string2, end2);
	      if (c == '\n')
		break;
	    }
	  goto fail;


	/* endline is the dual of begline.  */
	case endline:
	  DEBUG_PRINT ("EXECUTING endline.\n");

	  if (AT_STRINGS_END (d))
	    break;
	  PREFETCH_NOLIMIT ();
	  if (*d == '\n')
	    break;
	  goto fail;


	/* Match at the very beginning of the data.  */
	case begbuf:
	  DEBUG_PRINT ("EXECUTING begbuf.\n");
	  if (AT_STRINGS_BEG (d))
	    break;
	  goto fail;


	/* Match at the very end of the data.  */
	case endbuf:
	  DEBUG_PRINT ("EXECUTING endbuf.\n");
	  if (AT_STRINGS_END (d))
	    break;
	  goto fail;


	/* on_failure_keep_string_jump is used to optimize '.*\n'.  It
	   pushes NULL as the value for the string on the stack.  Then
	   'POP_FAILURE_POINT' will keep the current value for the
	   string, instead of restoring it.  To see why, consider
	   matching 'foo\nbar' against '.*\n'.  The .* matches the foo;
	   then the . fails against the \n.  But the next thing we want
	   to do is match the \n against the \n; if we restored the
	   string value, we would be back at the foo.

	   Because this is used only in specific cases, we don't need to
	   check all the things that 'on_failure_jump' does, to make
	   sure the right things get saved on the stack.  Hence we don't
	   share its code.  The only reason to push anything on the
	   stack at all is that otherwise we would have to change
	   'anychar's code to do something besides goto fail in this
	   case; that seems worse than this.  */
	case on_failure_keep_string_jump:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT ("EXECUTING on_failure_keep_string_jump %d (to %p):\n",
		       mcnt, p + mcnt);

	  PUSH_FAILURE_POINT (p - 3, NULL);
	  break;

	  /* A nasty loop is introduced by the non-greedy *? and +?.
	     With such loops, the stack only ever contains one failure point
	     at a time, so that a plain on_failure_jump_loop kind of
	     cycle detection cannot work.  Worse yet, such a detection
	     can not only fail to detect a cycle, but it can also wrongly
	     detect a cycle (between different instantiations of the same
	     loop).
	     So the method used for those nasty loops is a little different:
	     We use a special cycle-detection-stack-frame which is pushed
	     when the on_failure_jump_nastyloop failure-point is *popped*.
	     This special frame thus marks the beginning of one iteration
	     through the loop and we can hence easily check right here
	     whether something matched between the beginning and the end of
	     the loop.  */
	case on_failure_jump_nastyloop:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT ("EXECUTING on_failure_jump_nastyloop %d (to %p):\n",
		       mcnt, p + mcnt);

	  eassert ((re_opcode_t)p[-4] == no_op);
	  {
	    bool cycle = false;
	    CHECK_INFINITE_LOOP (p - 4, d);
	    if (!cycle)
	      /* If there's a cycle, just continue without pushing
		 this failure point.  The failure point is the "try again"
		 option, which shouldn't be tried.
		 We want (x?)*?y\1z to match both xxyz and xxyxz.  */
	      PUSH_FAILURE_POINT (p - 3, d);
	  }
	  break;

	  /* Simple loop detecting on_failure_jump:  just check on the
	     failure stack if the same spot was already hit earlier.  */
	case on_failure_jump_loop:
	on_failure:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT ("EXECUTING on_failure_jump_loop %d (to %p):\n",
		       mcnt, p + mcnt);
	  {
	    bool cycle = false;
	    CHECK_INFINITE_LOOP (p - 3, d);
	    if (cycle)
	      /* If there's a cycle, get out of the loop, as if the matching
		 had failed.  We used to just 'goto fail' here, but that was
		 aborting the search a bit too early: we want to keep the
		 empty-loop-match and keep matching after the loop.
		 We want (x?)*y\1z to match both xxyz and xxyxz.  */
	      p += mcnt;
	    else
	      PUSH_FAILURE_POINT (p - 3, d);
	  }
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
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT ("EXECUTING on_failure_jump %d (to %p):\n",
		       mcnt, p + mcnt);

	  PUSH_FAILURE_POINT (p -3, d);
	  break;

	/* This operation is used for greedy *.
	   Compare the beginning of the repeat with what in the
	   pattern follows its end. If we can establish that there
	   is nothing that they would both match, i.e., that we
	   would have to backtrack because of (as in, e.g., 'a*a')
	   then we can use a non-backtracking loop based on
	   on_failure_keep_string_jump instead of on_failure_jump.  */
	case on_failure_jump_smart:
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);
	  DEBUG_PRINT ("EXECUTING on_failure_jump_smart %d (to %p).\n",
		       mcnt, p + mcnt);
	  {
	    re_char *p1 = p; /* Next operation.  */
	    /* Discard 'const', making re_search non-reentrant.  */
	    unsigned char *p2 = (unsigned char *) p + mcnt; /* Jump dest.  */
	    unsigned char *p3 = (unsigned char *) p - 3; /* opcode location.  */

	    p -= 3;		/* Reset so that we will re-execute the
				   instruction once it's been changed. */

	    EXTRACT_NUMBER (mcnt, p2 - 2);

	    /* Ensure this is indeed the trivial kind of loop
	       we are expecting.  */
	    eassert (skip_one_char (p1) == p2 - 3);
	    eassert ((re_opcode_t) p2[-3] == jump && p2 + mcnt == p);
	    DEBUG_STATEMENT (regex_emacs_debug += 2);
	    if (mutually_exclusive_p (bufp, p1, p2))
	      {
		/* Use a fast 'on_failure_keep_string_jump' loop.  */
		DEBUG_PRINT ("  smart exclusive => fast loop.\n");
		*p3 = (unsigned char) on_failure_keep_string_jump;
		STORE_NUMBER (p2 - 2, mcnt + 3);
	      }
	    else
	      {
		/* Default to a safe 'on_failure_jump' loop.  */
		DEBUG_PRINT ("  smart default => slow loop.\n");
		*p3 = (unsigned char) on_failure_jump;
	      }
	    DEBUG_STATEMENT (regex_emacs_debug -= 2);
	  }
	  break;

	/* Unconditionally jump (without popping any failure points).  */
	case jump:
	unconditional_jump:
	  maybe_quit ();
	  EXTRACT_NUMBER_AND_INCR (mcnt, p);	/* Get the amount to jump.  */
	  DEBUG_PRINT ("EXECUTING jump %d ", mcnt);
	  p += mcnt;				/* Do the jump.  */
	  DEBUG_PRINT ("(to %p).\n", p);
	  break;


	/* Have to succeed matching what follows at least n times.
	   After that, handle like 'on_failure_jump'.  */
	case succeed_n:
	  /* Signedness doesn't matter since we only compare MCNT to 0.  */
	  EXTRACT_NUMBER (mcnt, p + 2);
	  DEBUG_PRINT ("EXECUTING succeed_n %d.\n", mcnt);

	  /* Originally, mcnt is how many times we HAVE to succeed.  */
	  if (mcnt != 0)
	    {
	      /* Discard 'const', making re_search non-reentrant.  */
	      unsigned char *p2 = (unsigned char *) p + 2; /* counter loc.  */
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
	  DEBUG_PRINT ("EXECUTING jump_n %d.\n", mcnt);

	  /* Originally, this is how many times we CAN jump.  */
	  if (mcnt != 0)
	    {
	      /* Discard 'const', making re_search non-reentrant.  */
	      unsigned char *p2 = (unsigned char *) p + 2; /* counter loc.  */
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
	    DEBUG_PRINT ("EXECUTING set_number_at.\n");

	    EXTRACT_NUMBER_AND_INCR (mcnt, p);
	    /* Discard 'const', making re_search non-reentrant.  */
	    p2 = (unsigned char *) p + mcnt;
	    /* Signedness doesn't matter since we only copy MCNT's bits.  */
	    EXTRACT_NUMBER_AND_INCR (mcnt, p);
	    DEBUG_PRINT ("  Setting %p to %d.\n", p2, mcnt);
	    PUSH_NUMBER (p2, mcnt);
	    break;
	  }

	case wordbound:
	case notwordbound:
	  {
	    bool not = (re_opcode_t) *(p - 1) == notwordbound;
	    DEBUG_PRINT ("EXECUTING %swordbound.\n", not ? "not" : "");

	    /* We SUCCEED (or FAIL) in one of the following cases: */

	    /* Case 1: D is at the beginning or the end of string.  */
	    if (AT_STRINGS_BEG (d) || AT_STRINGS_END (d))
	      not = !not;
	    else
	      {
		/* C1 is the character before D, S1 is the syntax of C1, C2
		   is the character at D, and S2 is the syntax of C2.  */
		int c1, c2;
		int s1, s2;
		int dummy;
                ptrdiff_t offset = PTR_TO_OFFSET (d);
                ptrdiff_t charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset) - 1;
		UPDATE_SYNTAX_TABLE (charpos);
		GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
		s1 = SYNTAX (c1);
		UPDATE_SYNTAX_TABLE_FORWARD (charpos + 1);
		PREFETCH_NOLIMIT ();
		GET_CHAR_AFTER (c2, d, dummy);
		s2 = SYNTAX (c2);

		if (/* Case 2: Only one of S1 and S2 is Sword.  */
		    ((s1 == Sword) != (s2 == Sword))
		    /* Case 3: Both of S1 and S2 are Sword, and macro
		       WORD_BOUNDARY_P (C1, C2) returns nonzero.  */
		    || ((s1 == Sword) && WORD_BOUNDARY_P (c1, c2)))
		  not = !not;
	      }
	    if (not)
	      break;
	    else
	      goto fail;
	  }

	case wordbeg:
	  DEBUG_PRINT ("EXECUTING wordbeg.\n");

	  /* We FAIL in one of the following cases: */

	  /* Case 1: D is at the end of string.  */
	  if (AT_STRINGS_END (d))
	    goto fail;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      int c1, c2;
	      int s1, s2;
	      int dummy;
	      ptrdiff_t offset = PTR_TO_OFFSET (d);
	      ptrdiff_t charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	      UPDATE_SYNTAX_TABLE (charpos);
	      PREFETCH ();
	      GET_CHAR_AFTER (c2, d, dummy);
	      s2 = SYNTAX (c2);

	      /* Case 2: S2 is not Sword. */
	      if (s2 != Sword)
		goto fail;

	      /* Case 3: D is not at the beginning of string ... */
	      if (!AT_STRINGS_BEG (d))
		{
		  GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
		  UPDATE_SYNTAX_TABLE_BACKWARD (charpos - 1);
		  s1 = SYNTAX (c1);

		  /* ... and S1 is Sword, and WORD_BOUNDARY_P (C1, C2)
		     returns 0.  */
		  if ((s1 == Sword) && !WORD_BOUNDARY_P (c1, c2))
		    goto fail;
		}
	    }
	  break;

	case wordend:
	  DEBUG_PRINT ("EXECUTING wordend.\n");

	  /* We FAIL in one of the following cases: */

	  /* Case 1: D is at the beginning of string.  */
	  if (AT_STRINGS_BEG (d))
	    goto fail;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      int c1, c2;
	      int s1, s2;
	      int dummy;
              ptrdiff_t offset = PTR_TO_OFFSET (d);
              ptrdiff_t charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset) - 1;
	      UPDATE_SYNTAX_TABLE (charpos);
	      GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
	      s1 = SYNTAX (c1);

	      /* Case 2: S1 is not Sword.  */
	      if (s1 != Sword)
		goto fail;

	      /* Case 3: D is not at the end of string ... */
	      if (!AT_STRINGS_END (d))
		{
		  PREFETCH_NOLIMIT ();
		  GET_CHAR_AFTER (c2, d, dummy);
                  UPDATE_SYNTAX_TABLE_FORWARD (charpos + 1);
		  s2 = SYNTAX (c2);

		  /* ... and S2 is Sword, and WORD_BOUNDARY_P (C1, C2)
		     returns 0.  */
		  if ((s2 == Sword) && !WORD_BOUNDARY_P (c1, c2))
	  goto fail;
		}
	    }
	  break;

	case symbeg:
	  DEBUG_PRINT ("EXECUTING symbeg.\n");

	  /* We FAIL in one of the following cases: */

	  /* Case 1: D is at the end of string.  */
	  if (AT_STRINGS_END (d))
	    goto fail;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      int c1, c2;
	      int s1, s2;
	      ptrdiff_t offset = PTR_TO_OFFSET (d);
	      ptrdiff_t charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	      UPDATE_SYNTAX_TABLE (charpos);
	      PREFETCH ();
	      c2 = RE_STRING_CHAR (d, target_multibyte);
	      s2 = SYNTAX (c2);

	      /* Case 2: S2 is neither Sword nor Ssymbol. */
	      if (s2 != Sword && s2 != Ssymbol)
		goto fail;

	      /* Case 3: D is not at the beginning of string ... */
	      if (!AT_STRINGS_BEG (d))
		{
		  GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
		  UPDATE_SYNTAX_TABLE_BACKWARD (charpos - 1);
		  s1 = SYNTAX (c1);

		  /* ... and S1 is Sword or Ssymbol.  */
		  if (s1 == Sword || s1 == Ssymbol)
		    goto fail;
		}
	    }
	  break;

	case symend:
	  DEBUG_PRINT ("EXECUTING symend.\n");

	  /* We FAIL in one of the following cases: */

	  /* Case 1: D is at the beginning of string.  */
	  if (AT_STRINGS_BEG (d))
	    goto fail;
	  else
	    {
	      /* C1 is the character before D, S1 is the syntax of C1, C2
		 is the character at D, and S2 is the syntax of C2.  */
	      int c1, c2;
	      int s1, s2;
              ptrdiff_t offset = PTR_TO_OFFSET (d);
              ptrdiff_t charpos = SYNTAX_TABLE_BYTE_TO_CHAR (offset) - 1;
	      UPDATE_SYNTAX_TABLE (charpos);
	      GET_CHAR_BEFORE_2 (c1, d, string1, end1, string2, end2);
	      s1 = SYNTAX (c1);

	      /* Case 2: S1 is neither Ssymbol nor Sword.  */
	      if (s1 != Sword && s1 != Ssymbol)
		goto fail;

	      /* Case 3: D is not at the end of string ... */
	      if (!AT_STRINGS_END (d))
		{
		  PREFETCH_NOLIMIT ();
		  c2 = RE_STRING_CHAR (d, target_multibyte);
		  UPDATE_SYNTAX_TABLE_FORWARD (charpos + 1);
		  s2 = SYNTAX (c2);

		  /* ... and S2 is Sword or Ssymbol.  */
		  if (s2 == Sword || s2 == Ssymbol)
                    goto fail;
		}
	    }
	  break;

	case syntaxspec:
	case notsyntaxspec:
	  {
	    bool not = (re_opcode_t) *(p - 1) == notsyntaxspec;
	    mcnt = *p++;
	    DEBUG_PRINT ("EXECUTING %ssyntaxspec %d.\n", not ? "not" : "",
			 mcnt);
	    PREFETCH ();
	    {
	      ptrdiff_t offset = PTR_TO_OFFSET (d);
	      ptrdiff_t pos1 = SYNTAX_TABLE_BYTE_TO_CHAR (offset);
	      UPDATE_SYNTAX_TABLE (pos1);
	    }
	    {
	      int len;
	      int c;

	      GET_CHAR_AFTER (c, d, len);
	      if ((SYNTAX (c) != (enum syntaxcode) mcnt) ^ not)
		goto fail;
	      d += len;
	    }
	  }
	  break;

	case at_dot:
	  DEBUG_PRINT ("EXECUTING at_dot.\n");
	  if (PTR_BYTE_POS (d) != PT_BYTE)
	    goto fail;
	  break;

	case categoryspec:
	case notcategoryspec:
	  {
	    bool not = (re_opcode_t) *(p - 1) == notcategoryspec;
	    mcnt = *p++;
	    DEBUG_PRINT ("EXECUTING %scategoryspec %d.\n",
			 not ? "not" : "", mcnt);
	    PREFETCH ();

	    {
	      int len;
	      int c;
	      GET_CHAR_AFTER (c, d, len);
	      if ((!CHAR_HAS_CATEGORY (c, mcnt)) ^ not)
		goto fail;
	      d += len;
	    }
	  }
	  break;

	default:
	  abort ();
	}
      continue;  /* Successfully executed one pattern command; keep going.  */


    /* We goto here if a matching operation fails. */
    fail:
      maybe_quit ();
      if (!FAIL_STACK_EMPTY ())
	{
	  re_char *str, *pat;
	  /* A restart point is known.  Restore to that state.  */
	  DEBUG_PRINT ("\nFAIL:\n");
	  POP_FAILURE_POINT (str, pat);
	  switch (*pat++)
	    {
	    case on_failure_keep_string_jump:
	      eassert (str == NULL);
	      goto continue_failure_jump;

	    case on_failure_jump_nastyloop:
	      eassert ((re_opcode_t)pat[-2] == no_op);
	      PUSH_FAILURE_POINT (pat - 2, str);
	      FALLTHROUGH;
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
	      abort ();
	    }

	  eassert (p >= bufp->buffer && p <= pend);

	  if (d >= string1 && d <= end1)
	    dend = end_match_1;
	}
      else
	break;   /* Matching at this starting point really fails.  */
    } /* for (;;) */

  if (best_regs_set)
    goto restore_best_regs;

  unbind_to (count, Qnil);
  SAFE_FREE ();

  return -1;				/* Failure to match.  */
}

/* Subroutine definitions for re_match_2.  */

/* Return true if TRANSLATE[S1] and TRANSLATE[S2] are not identical
   for LEN bytes.  */

static bool
bcmp_translate (re_char *s1, re_char *s2, ptrdiff_t len,
		Lisp_Object translate, bool target_multibyte)
{
  re_char *p1 = s1, *p2 = s2;
  re_char *p1_end = s1 + len;
  re_char *p2_end = s2 + len;

  /* FIXME: Checking both p1 and p2 presumes that the two strings might have
     different lengths, but relying on a single LEN would break this. -sm  */
  while (p1 < p1_end && p2 < p2_end)
    {
      int p1_charlen, p2_charlen;
      int p1_ch, p2_ch;

      GET_CHAR_AFTER (p1_ch, p1, p1_charlen);
      GET_CHAR_AFTER (p2_ch, p2, p2_charlen);

      if (RE_TRANSLATE (translate, p1_ch)
	  != RE_TRANSLATE (translate, p2_ch))
	return true;

      p1 += p1_charlen, p2 += p2_charlen;
    }

  return p1 != p1_end || p2 != p2_end;
}

/* Entry points for GNU code.  */

/* re_compile_pattern is the GNU regular expression compiler: it
   compiles PATTERN (of length SIZE) and puts the result in BUFP.
   Returns 0 if the pattern was valid, otherwise an error string.

   Assumes the 'allocated' (and perhaps 'buffer') and 'translate' fields
   are set in BUFP on entry.

   We call regex_compile to do the actual compilation.  */

const char *
re_compile_pattern (const char *pattern, ptrdiff_t length,
		    bool posix_backtracking, const char *whitespace_regexp,
		    struct re_pattern_buffer *bufp)
{
  bufp->regs_allocated = REGS_UNALLOCATED;

  reg_errcode_t ret
      = regex_compile ((re_char *) pattern, length,
		       posix_backtracking,
		       whitespace_regexp,
		       bufp);

  if (!ret)
    return NULL;
  return re_error_msgid[ret];
}
