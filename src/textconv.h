/* String conversion support for graphics terminals.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

  /* Notice that point or mark has moved in the specified frame's
     selected window's selected buffer.  The second argument is the
     window whose point changed, and the third argument is the
     buffer.  */
  void (*point_changed) (struct frame *, struct window *,
			 struct buffer *);

  /* Notice that the preconversion region has changed without point
     being moved.  */
  void (*compose_region_changed) (struct frame *);

  /* Notice that an asynch conversion identified by COUNTER has
     completed.  */
  void (*notify_conversion) (unsigned long);
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

  /* The number of times for which to repeat the scanning in order
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



#define TEXTCONV_SKIP_CONVERSION_REGION (1 << 0)

extern int textconv_query (struct frame *, struct textconv_callback_struct *,
			   int);
extern bool detect_conversion_events (void);
extern void handle_pending_conversion_events (void);
#ifdef HAVE_ANDROID
extern void start_batch_edit (struct frame *, unsigned long);
extern void end_batch_edit (struct frame *, unsigned long);
extern void commit_text (struct frame *, Lisp_Object, ptrdiff_t,
			 unsigned long);
extern void finish_composing_text (struct frame *, unsigned long,
				   bool);
extern void set_composing_text (struct frame *, Lisp_Object,
				ptrdiff_t, unsigned long);
extern void set_composing_region (struct frame *, ptrdiff_t, ptrdiff_t,
				  unsigned long);
extern void textconv_set_point_and_mark (struct frame *, ptrdiff_t,
					 ptrdiff_t, unsigned long);
extern void delete_surrounding_text (struct frame *, ptrdiff_t,
				     ptrdiff_t, unsigned long);
extern void request_point_update (struct frame *, unsigned long);
extern void textconv_barrier (struct frame *, unsigned long);
extern void replace_text (struct frame *, ptrdiff_t, ptrdiff_t,
			  Lisp_Object, ptrdiff_t, unsigned long);

extern char *get_extracted_text (struct frame *, ptrdiff_t, ptrdiff_t *,
				 ptrdiff_t *, ptrdiff_t *, ptrdiff_t *,
				 ptrdiff_t *, bool *);
extern char *get_surrounding_text (struct frame *, ptrdiff_t,
				   ptrdiff_t, ptrdiff_t *,
				   ptrdiff_t *, ptrdiff_t *,
				   ptrdiff_t *, ptrdiff_t *);
extern void get_conversion_field (struct frame *, ptrdiff_t *, ptrdiff_t *);
#endif
extern bool conversion_disabled_p (void);
extern void check_postponed_buffers (void);

extern void register_textconv_interface (struct textconv_interface *);

#endif /* _TEXTCONV_H_ */
