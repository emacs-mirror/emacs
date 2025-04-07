/* Text index for character positions.
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

#ifndef EMACS_TEXT_INDEX_H
# define EMACS_TEXT_INDEX_H

#include "config.h"
# include "lisp.h"

struct text_index;

void syms_of_text_index (void);
void text_index_free (struct text_index *ti);
ptrdiff_t buf_bytepos_to_charpos (struct buffer *b, ptrdiff_t bytepos);
ptrdiff_t buf_charpos_to_bytepos (struct buffer *b, ptrdiff_t charpos);
void text_index_invalidate (struct buffer *b, ptrdiff_t from_byte);

#endif /* EMACS_TEXT_INDEX_H */
