/* Marker vectors.
   Copyright (C) 2025 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

#ifndef EMACS_MARKER_VECTOR_H
#define EMACS_MARKER_VECTOR_H

#include <config.h>
#include "lisp.h"

/* A marker vector is a Lisp vector starting with a header of
   MARKER_VECTOR_HEADER_SIZE Lisp_Objects, followed by entries of
   MARKER_VECTOR_ENTRY_SIZE Lisp_Objects.  */

enum
{
  /* Header. */
  MARKER_VECTOR_FREE = 0,
  MARKER_VECTOR_MAX_ENTRY = 1,
  MARKER_VECTOR_HEADER_SIZE = 2,

  /* Entries.  */
  MARKER_VECTOR_OFFSET_MARKER = 0,
  MARKER_VECTOR_OFFSET_CHARPOS = 1,
  MARKER_VECTOR_ENTRY_SIZE = 2,
};

/* Iterate over markers in marker vector MV, binding a variable with
   name M to a pointer to Lisp_Marker.  The loop must be ended
   with an END_DO_MARKERS.  */

# define DO_MARKERS_OF_VECTOR(mv, m)					\
  for (ptrdiff_t e_ = MARKER_VECTOR_HEADER_SIZE,			\
	 end_ = XFIXNUM (AREF (mv, MARKER_VECTOR_MAX_ENTRY));		\
       e_ <= end_;							\
       e_ += MARKER_VECTOR_ENTRY_SIZE)					\
    {									\
       Lisp_Object m_ = AREF (mv, e_ + MARKER_VECTOR_OFFSET_MARKER);	\
       if (MARKERP (m_))						\
	 {								\
            struct Lisp_Marker *m = XMARKER (m_);

/* Iterate over markers of buffer B, binding a variable with name M to a
   pointer to Lisp_Marker.  The loop must be ended with an
   END_DO_MARKERS.  */

# define DO_MARKERS(b, m) DO_MARKERS_OF_VECTOR (BUF_MARKERS (b), m)
# define END_DO_MARKERS }}

Lisp_Object make_marker_vector (void);
Lisp_Object alloc_marker_vector (ptrdiff_t len);
void marker_vector_add (struct buffer *b, struct Lisp_Marker *m);
void marker_vector_remove (struct Lisp_Vector *v, struct Lisp_Marker *m);
void marker_vector_reset (struct buffer *b);
void marker_vector_set_charpos (struct Lisp_Marker *m, ptrdiff_t charpos);
ptrdiff_t marker_vector_charpos (const struct Lisp_Marker *m);
ptrdiff_t marker_vector_bytepos (const struct Lisp_Marker *m);
void marker_vector_adjust_for_delete (struct buffer *b, ptrdiff_t from, ptrdiff_t to);
void marker_vector_adjust_for_insert (struct buffer *b, const ptrdiff_t from,
				      ptrdiff_t to, bool before_markers);
void marker_vector_adjust_for_replace (struct buffer *b, ptrdiff_t from,
				       ptrdiff_t old_chars, ptrdiff_t new_chars);

#endif /* EMACS_MARKER_VECTOR_H */
