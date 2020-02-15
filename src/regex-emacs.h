/* Emacs regular expression API

   Copyright (C) 1985, 1989-1993, 1995, 2000-2020 Free Software
   Foundation, Inc.

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

#ifndef EMACS_REGEX_H
#define EMACS_REGEX_H 1

#include <stddef.h>

/* This is the structure we store register match data in.
   Declare this before including lisp.h, since lisp.h (via thread.h)
   uses struct re_registers.  */
struct re_registers
{
  ptrdiff_t num_regs;
  ptrdiff_t *start;
  ptrdiff_t *end;
};

#include "lisp.h"

/* The string or buffer being matched.
   It is used for looking up syntax properties.

   If the value is a Lisp string object, match text in that string; if
   it's nil, match text in the current buffer; if it's t, match text
   in a C string.

   This value is effectively another parameter to re_search_2 and
   re_match_2.  No calls into Lisp or thread switches are allowed
   before setting re_match_object and calling into the regex search
   and match functions.  These functions capture the current value of
   re_match_object into gl_state on entry.

   TODO: turn into an actual function parameter.  */
extern Lisp_Object re_match_object;

/* Roughly the maximum number of failure points on the stack.  */
extern ptrdiff_t emacs_re_max_failures;

/* Amount of memory that we can safely stack allocate.  */
extern ptrdiff_t emacs_re_safe_alloca;

/* This data structure represents a compiled pattern.  Before calling
   the pattern compiler, the fields 'buffer', 'allocated', 'fastmap',
   and 'translate' can be set.  After the pattern has been
   compiled, the 're_nsub' field is available.  All other fields are
   private to the regex routines.  */

struct re_pattern_buffer
{
	/* Space that holds the compiled pattern.  It is declared as
          'unsigned char *' because its elements are
           sometimes used as array indexes.  */
  unsigned char *buffer;

	/* Number of bytes to which 'buffer' points.  */
  ptrdiff_t allocated;

	/* Number of bytes actually used in 'buffer'.  */
  ptrdiff_t used;

        /* Charset of unibyte characters at compiling time.  */
  int charset_unibyte;

        /* Pointer to a fastmap, if any, otherwise zero.  re_search uses
           the fastmap, if there is one, to skip over impossible
           starting points for matches.  */
  char *fastmap;

        /* Either a translate table to apply to all characters before
           comparing them, or zero for no translation.  The translation
           applies to a pattern when it is compiled and to a string
           when it is matched.  */
  Lisp_Object translate;

	/* Number of subexpressions found by the compiler.  */
  ptrdiff_t re_nsub;

        /* True if and only if this pattern can match the empty string.
           Well, in truth it's used only in 're_search_2', to see
           whether or not we should use the fastmap, so we don't set
           this absolutely perfectly; see 're_compile_fastmap'.  */
  bool_bf can_be_null : 1;

        /* If REGS_UNALLOCATED, allocate space in the 'regs' structure
             for at least (re_nsub + 1) groups.
           If REGS_REALLOCATE, reallocate space if necessary.
           If REGS_FIXED, use what's there.  */
  unsigned regs_allocated : 2;

        /* Set to false when 'regex_compile' compiles a pattern; set to true
           by 're_compile_fastmap' if it updates the fastmap.  */
  bool_bf fastmap_accurate : 1;

  /* If true, the compilation of the pattern had to look up the syntax table,
     so the compiled pattern is valid for the current syntax table only.  */
  bool_bf used_syntax : 1;

  /* If true, multi-byte form in the regexp pattern should be
     recognized as a multibyte character.  */
  bool_bf multibyte : 1;

  /* If true, multi-byte form in the target of match should be
     recognized as a multibyte character.  */
  bool_bf target_multibyte : 1;
};

/* Declarations for routines.  */

/* Compile the regular expression PATTERN, with length LENGTH
   and syntax given by the global 're_syntax_options', into the buffer
   BUFFER.  Return NULL if successful, and an error string if not.  */
extern const char *re_compile_pattern (const char *pattern, ptrdiff_t length,
				       bool posix_backtracking,
				       const char *whitespace_regexp,
				       struct re_pattern_buffer *buffer);


/* Search in the string STRING (with length LENGTH) for the pattern
   compiled into BUFFER.  Start searching at position START, for RANGE
   characters.  Return the starting position of the match, -1 for no
   match, or -2 for an internal error.  Also return register
   information in REGS (if REGS is non-null).  */
extern ptrdiff_t re_search (struct re_pattern_buffer *buffer,
			   const char *string, ptrdiff_t length,
			   ptrdiff_t start, ptrdiff_t range,
			   struct re_registers *regs);


/* Like 're_search', but search in the concatenation of STRING1 and
   STRING2.  Also, stop searching at index START + STOP.  */
extern ptrdiff_t re_search_2 (struct re_pattern_buffer *buffer,
			     const char *string1, ptrdiff_t length1,
			     const char *string2, ptrdiff_t length2,
			     ptrdiff_t start, ptrdiff_t range,
			     struct re_registers *regs,
			     ptrdiff_t stop);


/* Like 're_search_2', but return how many characters in STRING the regexp
   in BUFFER matched, starting at position START.  */
extern ptrdiff_t re_match_2 (struct re_pattern_buffer *buffer,
			    const char *string1, ptrdiff_t length1,
			    const char *string2, ptrdiff_t length2,
			    ptrdiff_t start, struct re_registers *regs,
			    ptrdiff_t stop);


/* Set REGS to hold NUM_REGS registers, storing them in STARTS and
   ENDS.  Subsequent matches using BUFFER and REGS will use this memory
   for recording register information.  STARTS and ENDS must be
   allocated with malloc, and must each be at least 'NUM_REGS * sizeof
   (ptrdiff_t)' bytes long.

   If NUM_REGS == 0, then subsequent matches should allocate their own
   register data.

   Unless this function is called, the first search or match using
   PATTERN_BUFFER will allocate its own register data, without
   freeing the old data.  */
extern void re_set_registers (struct re_pattern_buffer *buffer,
			      struct re_registers *regs,
			      ptrdiff_t num_regs,
			      ptrdiff_t *starts, ptrdiff_t *ends);

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

extern bool re_iswctype (int ch, re_wctype_t cc);
extern re_wctype_t re_wctype_parse (const unsigned char **strp,
				    ptrdiff_t limit);

#endif /* EMACS_REGEX_H */
