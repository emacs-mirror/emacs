/* String conversion support for graphics terminals.

Copyright (C) 2023 Free Software Foundation, Inc.

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

#ifndef _TEXTCONV_H_

#include "lisp.h"
#include "frame.h"

/* The function pointers in this structure should be filled out by
   each GUI backend interested in supporting text conversion.

   Finally, register_texconv_interface must be called at some point
   during terminal initialization.  */

struct textconv_interface
{
  /* Notice that the text conversion context has changed (which can
     happen if the window is deleted or switches buffers, or an
     unexpected buffer change occurs.) */
  void (*reset) (struct frame *);
};



enum textconv_caret_direction
  {
    TEXTCONV_FORWARD_CHAR,
    TEXTCONV_BACKWARD_CHAR,
    TEXTCONV_FORWARD_WORD,
    TEXTCONV_BACKWARD_WORD,
    TEXTCONV_CARET_UP,
    TEXTCONV_CARET_DOWN,
    TEXTCONV_NEXT_LINE,
    TEXTCONV_PREVIOUS_LINE,
    TEXTCONV_LINE_START,
    TEXTCONV_LINE_END,
    TEXTCONV_ABSOLUTE_POSITION,
  };

enum textconv_operation
  {
    TEXTCONV_SUBSTITUTION,
    TEXTCONV_RETRIEVAL,
  };

/* Structure describing text in a buffer corresponding to a ``struct
   textconv_callback_struct''.  */

struct textconv_conversion_text
{
  /* Length of the text in characters and bytes.  */
  size_t length, bytes;

  /* Pointer to the text data.  This must be deallocated by the
     caller.  */
  char *text;
};

/* Structure describing a single query submitted by the input
   method.  */

struct textconv_callback_struct
{
  /* Character position, relative to the current spot location, from
     where on text should be returned.  */
  EMACS_INT position;

  /* The type of scanning to perform to determine either the start or
     the end of the conversion.  */
  enum textconv_caret_direction direction;

  /* The the number of times for which to repeat the scanning in order
     to determine the starting position of the text to return.  */
  unsigned short factor;

  /* The operation to perform upon the current buffer contents.

     If this is TEXTCONV_SUBSTITUTION, then the text that is returned
     will be deleted from the buffer itself.

     Otherwise, the text is simply returned without modifying the
     buffer contents.  */
  enum textconv_operation operation;

  /* Structure that will be filled with a description of the resulting
     text.  */
  struct textconv_conversion_text text;
};

extern int textconv_query (struct frame *, struct textconv_callback_struct *);
extern void register_texconv_interface (struct textconv_interface *);

#endif /* _TEXTCONV_H_ */
