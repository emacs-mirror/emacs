/* Header file for the buffer manipulation primitives.

Copyright (C) 1985-1986, 1993-1995, 1997-2013 Free Software Foundation,
Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <sys/types.h> /* for off_t, time_t */
#include "systime.h" /* for EMACS_TIME */

INLINE_HEADER_BEGIN
#ifndef BUFFER_INLINE
# define BUFFER_INLINE INLINE
#endif

/* Accessing the parameters of the current buffer.  */

/* These macros come in pairs, one for the char position
   and one for the byte position.  */

/* Position of beginning of buffer.  */
#define BEG (1)
#define BEG_BYTE (BEG)

/* Position of beginning of accessible range of buffer.  */
#define BEGV (current_buffer->begv)
#define BEGV_BYTE (current_buffer->begv_byte)

/* Position of point in buffer.  The "+ 0" makes this
   not an l-value, so you can't assign to it.  Use SET_PT instead.  */
#define PT (current_buffer->pt + 0)
#define PT_BYTE (current_buffer->pt_byte + 0)

/* Position of gap in buffer.  */
#define GPT (current_buffer->text->gpt)
#define GPT_BYTE (current_buffer->text->gpt_byte)

/* Position of end of accessible range of buffer.  */
#define ZV (current_buffer->zv)
#define ZV_BYTE (current_buffer->zv_byte)

/* Position of end of buffer.  */
#define Z (current_buffer->text->z)
#define Z_BYTE (current_buffer->text->z_byte)

/* Macros for the addresses of places in the buffer.  */

/* Address of beginning of buffer.  */
#define BEG_ADDR (current_buffer->text->beg)

/* Address of beginning of accessible range of buffer.  */
#define BEGV_ADDR (BYTE_POS_ADDR (current_buffer->begv_byte))

/* Address of point in buffer.  */
#define PT_ADDR (BYTE_POS_ADDR (current_buffer->pt_byte))

/* Address of beginning of gap in buffer.  */
#define GPT_ADDR (current_buffer->text->beg + current_buffer->text->gpt_byte - BEG_BYTE)

/* Address of end of gap in buffer.  */
#define GAP_END_ADDR (current_buffer->text->beg + current_buffer->text->gpt_byte + current_buffer->text->gap_size - BEG_BYTE)

/* Address of end of accessible range of buffer.  */
#define ZV_ADDR (BYTE_POS_ADDR (current_buffer->zv_byte))

/* Address of end of buffer.  */
#define Z_ADDR (current_buffer->text->beg + current_buffer->text->gap_size + current_buffer->text->z_byte - BEG_BYTE)

/* Size of gap.  */
#define GAP_SIZE (current_buffer->text->gap_size)

/* Is the current buffer narrowed?  */
#define NARROWED	((BEGV != BEG) || (ZV != Z))

/* Modification count.  */
#define MODIFF (current_buffer->text->modiff)

/* Character modification count.  */
#define CHARS_MODIFF (current_buffer->text->chars_modiff)

/* Overlay modification count.  */
#define OVERLAY_MODIFF (current_buffer->text->overlay_modiff)

/* Modification count as of last visit or save.  */
#define SAVE_MODIFF (current_buffer->text->save_modiff)

/* BUFFER_CEILING_OF (resp. BUFFER_FLOOR_OF), when applied to n, return
   the max (resp. min) p such that

   BYTE_POS_ADDR (p) - BYTE_POS_ADDR (n) == p - n       */

#define BUFFER_CEILING_OF(BYTEPOS) \
  (((BYTEPOS) < GPT_BYTE && GPT < ZV ? GPT_BYTE : ZV_BYTE) - 1)
#define BUFFER_FLOOR_OF(BYTEPOS) \
  (BEGV <= GPT && GPT_BYTE <= (BYTEPOS) ? GPT_BYTE : BEGV_BYTE)

/* Similar macros to operate on a specified buffer.
   Note that many of these evaluate the buffer argument more than once.  */

/* Position of beginning of buffer.  */
#define BUF_BEG(buf) (BEG)
#define BUF_BEG_BYTE(buf) (BEG_BYTE)

/* The BUF_BEGV[_BYTE], BUF_ZV[_BYTE], and BUF_PT[_BYTE] macros cannot
   be used for assignment; use SET_BUF_* macros below for that.  */

/* Position of beginning of accessible range of buffer.  */
#define BUF_BEGV(buf)					\
   (buf == current_buffer ? BEGV			\
    : NILP (BVAR (buf, begv_marker)) ? buf->begv	\
    : marker_position (BVAR (buf, begv_marker)))

#define BUF_BEGV_BYTE(buf)				\
   (buf == current_buffer ? BEGV_BYTE			\
    : NILP (BVAR (buf, begv_marker)) ? buf->begv_byte	\
    : marker_byte_position (BVAR (buf, begv_marker)))

/* Position of point in buffer.  */
#define BUF_PT(buf)					\
   (buf == current_buffer ? PT				\
    : NILP (BVAR (buf, pt_marker)) ? buf->pt		\
    : marker_position (BVAR (buf, pt_marker)))

#define BUF_PT_BYTE(buf)				\
   (buf == current_buffer ? PT_BYTE			\
    : NILP (BVAR (buf, pt_marker)) ? buf->pt_byte	\
    : marker_byte_position (BVAR (buf, pt_marker)))

/* Position of end of accessible range of buffer.  */
#define BUF_ZV(buf)					\
   (buf == current_buffer ? ZV				\
    : NILP (BVAR (buf, zv_marker)) ? buf->zv		\
    : marker_position (BVAR (buf, zv_marker)))

#define BUF_ZV_BYTE(buf)				\
   (buf == current_buffer ? ZV_BYTE			\
    : NILP (BVAR (buf, zv_marker)) ? buf->zv_byte	\
    : marker_byte_position (BVAR (buf, zv_marker)))

/* Position of gap in buffer.  */
#define BUF_GPT(buf) ((buf)->text->gpt)
#define BUF_GPT_BYTE(buf) ((buf)->text->gpt_byte)

/* Position of end of buffer.  */
#define BUF_Z(buf) ((buf)->text->z)
#define BUF_Z_BYTE(buf) ((buf)->text->z_byte)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text->beg)

/* Address of beginning of gap of buffer.  */
#define BUF_GPT_ADDR(buf) ((buf)->text->beg + (buf)->text->gpt_byte - BEG_BYTE)

/* Address of end of buffer.  */
#define BUF_Z_ADDR(buf) ((buf)->text->beg + (buf)->text->gap_size + (buf)->text->z_byte - BEG_BYTE)

/* Address of end of gap in buffer.  */
#define BUF_GAP_END_ADDR(buf) ((buf)->text->beg + (buf)->text->gpt_byte + (buf)->text->gap_size - BEG_BYTE)

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text->gap_size)

/* Is this buffer narrowed?  */
#define BUF_NARROWED(buf) ((BUF_BEGV (buf) != BUF_BEG (buf)) \
			   || (BUF_ZV (buf) != BUF_Z (buf)))

/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text->modiff)

/* Character modification count.  */
#define BUF_CHARS_MODIFF(buf) ((buf)->text->chars_modiff)

/* Modification count as of last visit or save.  */
#define BUF_SAVE_MODIFF(buf) ((buf)->text->save_modiff)

/* Overlay modification count.  */
#define BUF_OVERLAY_MODIFF(buf) ((buf)->text->overlay_modiff)

/* Modification count as of last auto-save.  */
/* FIXME: should we move this into ->text->auto_save_modiff?  */
#define BUF_AUTOSAVE_MODIFF(buf) ((buf)->auto_save_modified)

/* Marker chain of buffer.  */
#define BUF_MARKERS(buf) ((buf)->text->markers)

#define BUF_UNCHANGED_MODIFIED(buf) \
  ((buf)->text->unchanged_modified)

#define BUF_OVERLAY_UNCHANGED_MODIFIED(buf) \
  ((buf)->text->overlay_unchanged_modified)
#define BUF_BEG_UNCHANGED(buf) ((buf)->text->beg_unchanged)
#define BUF_END_UNCHANGED(buf) ((buf)->text->end_unchanged)

#define UNCHANGED_MODIFIED \
  BUF_UNCHANGED_MODIFIED (current_buffer)
#define OVERLAY_UNCHANGED_MODIFIED \
  BUF_OVERLAY_UNCHANGED_MODIFIED (current_buffer)
#define BEG_UNCHANGED BUF_BEG_UNCHANGED (current_buffer)
#define END_UNCHANGED BUF_END_UNCHANGED (current_buffer)

/* Compute how many characters at the top and bottom of BUF are
   unchanged when the range START..END is modified.  This computation
   must be done each time BUF is modified.  */

#define BUF_COMPUTE_UNCHANGED(buf, start, end)				\
  do									\
    {									\
      if (BUF_UNCHANGED_MODIFIED (buf) == BUF_MODIFF (buf)		\
	  && (BUF_OVERLAY_UNCHANGED_MODIFIED (buf)			\
	      == BUF_OVERLAY_MODIFF (buf)))				\
	{								\
	  BUF_BEG_UNCHANGED (buf) = (start) - BUF_BEG (buf);		\
	  BUF_END_UNCHANGED (buf) = BUF_Z (buf) - (end);		\
	}								\
      else								\
	{								\
	  if (BUF_Z (buf) - (end) < BUF_END_UNCHANGED (buf))		\
	    BUF_END_UNCHANGED (buf) = BUF_Z (buf) - (end);		\
	  if ((start) - BUF_BEG (buf) < BUF_BEG_UNCHANGED (buf))	\
	    BUF_BEG_UNCHANGED (buf) = (start) - BUF_BEG (buf);		\
	}								\
    }									\
  while (0)


/* Macros to set PT in the current buffer, or another buffer.  */

#define SET_PT(position) (set_point (position))
#define TEMP_SET_PT(position) (temp_set_point (current_buffer, (position)))

#define SET_PT_BOTH(position, byte) (set_point_both (position, byte))
#define TEMP_SET_PT_BOTH(position, byte) \
  (temp_set_point_both (current_buffer, (position), (byte)))

#define BUF_TEMP_SET_PT(buffer, position) \
  (temp_set_point ((buffer), (position)))

extern void set_point (ptrdiff_t);
extern void temp_set_point (struct buffer *, ptrdiff_t);
extern void set_point_both (ptrdiff_t, ptrdiff_t);
extern void temp_set_point_both (struct buffer *,
				 ptrdiff_t, ptrdiff_t);
extern void enlarge_buffer_text (struct buffer *, ptrdiff_t);


/* Macros for setting the BEGV, ZV or PT of a given buffer.

   The ..._BOTH macros take both a charpos and a bytepos,
   which must correspond to each other.

   The macros without ..._BOTH take just a charpos,
   and compute the bytepos from it.  */

#define SET_BUF_BEGV(buf, charpos)				 \
  ((buf)->begv_byte = buf_charpos_to_bytepos ((buf), (charpos)), \
   (buf)->begv = (charpos))

#define SET_BUF_ZV(buf, charpos)				\
  ((buf)->zv_byte = buf_charpos_to_bytepos ((buf), (charpos)),	\
   (buf)->zv = (charpos))

#define SET_BUF_BEGV_BOTH(buf, charpos, byte)		\
  ((buf)->begv = (charpos),				\
   (buf)->begv_byte = (byte))

#define SET_BUF_ZV_BOTH(buf, charpos, byte)		\
  ((buf)->zv = (charpos),				\
   (buf)->zv_byte = (byte))

#define SET_BUF_PT_BOTH(buf, charpos, byte)		\
  ((buf)->pt = (charpos),				\
   (buf)->pt_byte = (byte))

/* Macros to access a character or byte in the current buffer,
   or convert between a byte position and an address.
   These macros do not check that the position is in range.  */

/* Access a Lisp position value in POS,
   and store the charpos in CHARPOS and the bytepos in BYTEPOS.  */

#define DECODE_POSITION(charpos, bytepos, pos)			\
do								\
  {								\
    Lisp_Object __pos = (pos);					\
    if (NUMBERP (__pos))					\
      {								\
	charpos = __pos;					\
	bytepos = buf_charpos_to_bytepos (current_buffer, __pos);  \
      }								\
    else if (MARKERP (__pos))					\
      {								\
	charpos = marker_position (__pos);			\
	bytepos = marker_byte_position (__pos);			\
      }								\
    else							\
      wrong_type_argument (Qinteger_or_marker_p, __pos);	\
  }								\
while (0)

/* Maximum number of bytes in a buffer.
   A buffer cannot contain more bytes than a 1-origin fixnum can represent,
   nor can it be so large that C pointer arithmetic stops working.
   The ptrdiff_t cast ensures that this is signed, not unsigned.  */
#define BUF_BYTES_MAX \
  (ptrdiff_t) min (MOST_POSITIVE_FIXNUM - 1, min (SIZE_MAX, PTRDIFF_MAX))

/* Return the address of byte position N in current buffer.  */

#define BYTE_POS_ADDR(n) \
  (((n) >= GPT_BYTE ? GAP_SIZE : 0) + (n) + BEG_ADDR - BEG_BYTE)

/* Return the address of char position N.  */

#define CHAR_POS_ADDR(n)			\
  (((n) >= GPT ? GAP_SIZE : 0)			\
   + buf_charpos_to_bytepos (current_buffer, n)	\
   + BEG_ADDR - BEG_BYTE)

/* Convert a character position to a byte position.  */

#define CHAR_TO_BYTE(charpos)			\
  (buf_charpos_to_bytepos (current_buffer, charpos))

/* Convert a byte position to a character position.  */

#define BYTE_TO_CHAR(bytepos)			\
  (buf_bytepos_to_charpos (current_buffer, bytepos))

/* Convert PTR, the address of a byte in the buffer, into a byte position.  */

#define PTR_BYTE_POS(ptr) \
((ptr) - (current_buffer)->text->beg					    \
 - (ptr - (current_buffer)->text->beg <= GPT_BYTE - BEG_BYTE ? 0 : GAP_SIZE) \
 + BEG_BYTE)

/* Return character at byte position POS.  See the caveat WARNING for
   FETCH_MULTIBYTE_CHAR below.  */

#define FETCH_CHAR(pos)				      	\
  (!NILP (BVAR (current_buffer, enable_multibyte_characters))	\
   ? FETCH_MULTIBYTE_CHAR ((pos))		      	\
   : FETCH_BYTE ((pos)))

/* Return the byte at byte position N.  */

#define FETCH_BYTE(n) *(BYTE_POS_ADDR ((n)))

/* Return character at byte position POS.  If the current buffer is unibyte
   and the character is not ASCII, make the returning character
   multibyte.  */

#define FETCH_CHAR_AS_MULTIBYTE(pos)			\
  (!NILP (BVAR (current_buffer, enable_multibyte_characters))	\
   ? FETCH_MULTIBYTE_CHAR ((pos))			\
   : UNIBYTE_TO_CHAR (FETCH_BYTE ((pos))))


/* Macros for accessing a character or byte,
   or converting between byte positions and addresses,
   in a specified buffer.  */

/* Return the address of character at byte position POS in buffer BUF.
   Note that both arguments can be computed more than once.  */

#define BUF_BYTE_ADDRESS(buf, pos) \
((buf)->text->beg + (pos) - BEG_BYTE		\
 + ((pos) >= (buf)->text->gpt_byte ? (buf)->text->gap_size : 0))

/* Return the address of character at char position POS in buffer BUF.
   Note that both arguments can be computed more than once.  */

#define BUF_CHAR_ADDRESS(buf, pos) \
((buf)->text->beg + buf_charpos_to_bytepos ((buf), (pos)) - BEG_BYTE	\
 + ((pos) >= (buf)->text->gpt ? (buf)->text->gap_size : 0))

/* Convert PTR, the address of a char in buffer BUF,
   into a character position.  */

#define BUF_PTR_BYTE_POS(buf, ptr)				\
((ptr) - (buf)->text->beg					\
 - (ptr - (buf)->text->beg <= BUF_GPT_BYTE (buf) - BEG_BYTE	\
    ? 0 : BUF_GAP_SIZE ((buf)))					\
 + BEG_BYTE)

/* Return the character at byte position POS in buffer BUF.   */

#define BUF_FETCH_CHAR(buf, pos)	      	\
  (!NILP (buf->enable_multibyte_characters)	\
   ? BUF_FETCH_MULTIBYTE_CHAR ((buf), (pos))    \
   : BUF_FETCH_BYTE ((buf), (pos)))

/* Return the byte at byte position N in buffer BUF.   */

#define BUF_FETCH_BYTE(buf, n) \
  *(BUF_BYTE_ADDRESS ((buf), (n)))

/* Define the actual buffer data structures.  */

/* This data structure describes the actual text contents of a buffer.
   It is shared between indirect buffers and their base buffer.  */

struct buffer_text
  {
    /* Actual address of buffer contents.  If REL_ALLOC is defined,
       this address might change when blocks are relocated which can
       e.g. happen when malloc is called.  So, don't pass a pointer
       into a buffer's text to functions that malloc.  */
    unsigned char *beg;

    ptrdiff_t gpt;		/* Char pos of gap in buffer.  */
    ptrdiff_t z;		/* Char pos of end of buffer.  */
    ptrdiff_t gpt_byte;		/* Byte pos of gap in buffer.  */
    ptrdiff_t z_byte;		/* Byte pos of end of buffer.  */
    ptrdiff_t gap_size;		/* Size of buffer's gap.  */
    EMACS_INT modiff;		/* This counts buffer-modification events
				   for this buffer.  It is incremented for
				   each such event, and never otherwise
				   changed.  */
    EMACS_INT chars_modiff;	/* This is modified with character change
				   events for this buffer.  It is set to
				   modiff for each such event, and never
				   otherwise changed.  */
    EMACS_INT save_modiff;	/* Previous value of modiff, as of last
				   time buffer visited or saved a file.  */

    EMACS_INT overlay_modiff;	/* Counts modifications to overlays.  */

    EMACS_INT compact;		/* Set to modiff each time when compact_buffer
				   is called for this buffer.  */

    /* Minimum value of GPT - BEG since last redisplay that finished.  */
    ptrdiff_t beg_unchanged;

    /* Minimum value of Z - GPT since last redisplay that finished.  */
    ptrdiff_t end_unchanged;

    /* MODIFF as of last redisplay that finished; if it matches MODIFF,
       beg_unchanged and end_unchanged contain no useful information.  */
    EMACS_INT unchanged_modified;

    /* BUF_OVERLAY_MODIFF of current buffer, as of last redisplay that
       finished; if it matches BUF_OVERLAY_MODIFF, beg_unchanged and
       end_unchanged contain no useful information.  */
    EMACS_INT overlay_unchanged_modified;

    /* Properties of this buffer's text.  */
    INTERVAL intervals;

    /* The markers that refer to this buffer.
       This is actually a single marker ---
       successive elements in its marker `chain'
       are the other markers referring to this buffer.
       This is a singly linked unordered list, which means that it's
       very cheap to add a marker to the list and it's also very cheap
       to move a marker within a buffer.  */
    struct Lisp_Marker *markers;

    /* Usually 0.  Temporarily set to 1 in decode_coding_gap to
       prevent Fgarbage_collect from shrinking the gap and losing
       not-yet-decoded bytes.  */
    bool inhibit_shrinking;
  };

/* Most code should use this macro to access Lisp fields in struct buffer.  */

#define BVAR(buf, field) ((buf)->INTERNAL_FIELD (field))

/* This is the structure that the buffer Lisp object points to.  */

struct buffer
{
  /* HEADER.NEXT is the next buffer, in chain of all buffers, including killed
     buffers.  This chain, starting from all_buffers, is used only for garbage
     collection, in order to collect killed buffers properly.  Note that large
     vectors and large pseudo-vector objects are all on another chain starting
     from large_vectors.  */
  struct vectorlike_header header;

  /* The name of this buffer.  */
  Lisp_Object INTERNAL_FIELD (name);

  /* The name of the file visited in this buffer, or nil.  */
  Lisp_Object INTERNAL_FIELD (filename);

  /* Directory for expanding relative file names.  */
  Lisp_Object INTERNAL_FIELD (directory);

  /* True if this buffer has been backed up (if you write to the visited
     file and it hasn't been backed up, then a backup will be made).  */
  Lisp_Object INTERNAL_FIELD (backed_up);

  /* Length of file when last read or saved.
     -1 means auto saving turned off because buffer shrank a lot.
     -2 means don't turn off auto saving if buffer shrinks.
       (That value is used with buffer-swap-text.)
     This is not in the  struct buffer_text
     because it's not used in indirect buffers at all.  */
  Lisp_Object INTERNAL_FIELD (save_length);

  /* File name used for auto-saving this buffer.
     This is not in the  struct buffer_text
     because it's not used in indirect buffers at all.  */
  Lisp_Object INTERNAL_FIELD (auto_save_file_name);

  /* Non-nil if buffer read-only.  */
  Lisp_Object INTERNAL_FIELD (read_only);

  /* "The mark".  This is a marker which may
     point into this buffer or may point nowhere.  */
  Lisp_Object INTERNAL_FIELD (mark);

  /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER) for all
     per-buffer variables of this buffer.  For locally unbound
     symbols, just the symbol appears as the element.  */
  Lisp_Object INTERNAL_FIELD (local_var_alist);

  /* Symbol naming major mode (e.g., lisp-mode).  */
  Lisp_Object INTERNAL_FIELD (major_mode);

  /* Pretty name of major mode (e.g., "Lisp"). */
  Lisp_Object INTERNAL_FIELD (mode_name);

  /* Mode line element that controls format of mode line.  */
  Lisp_Object INTERNAL_FIELD (mode_line_format);

  /* Analogous to mode_line_format for the line displayed at the top
     of windows.  Nil means don't display that line.  */
  Lisp_Object INTERNAL_FIELD (header_line_format);

  /* Keys that are bound local to this buffer.  */
  Lisp_Object INTERNAL_FIELD (keymap);

  /* This buffer's local abbrev table.  */
  Lisp_Object INTERNAL_FIELD (abbrev_table);

  /* This buffer's syntax table.  */
  Lisp_Object INTERNAL_FIELD (syntax_table);

  /* This buffer's category table.  */
  Lisp_Object INTERNAL_FIELD (category_table);

  /* Values of several buffer-local variables.  */
  /* tab-width is buffer-local so that redisplay can find it
     in buffers that are not current.  */
  Lisp_Object INTERNAL_FIELD (case_fold_search);
  Lisp_Object INTERNAL_FIELD (tab_width);
  Lisp_Object INTERNAL_FIELD (fill_column);
  Lisp_Object INTERNAL_FIELD (left_margin);

  /* Function to call when insert space past fill column.  */
  Lisp_Object INTERNAL_FIELD (auto_fill_function);

  /* Case table for case-conversion in this buffer.
     This char-table maps each char into its lower-case version.  */
  Lisp_Object INTERNAL_FIELD (downcase_table);

  /* Char-table mapping each char to its upper-case version.  */
  Lisp_Object INTERNAL_FIELD (upcase_table);

  /* Char-table for conversion for case-folding search.  */
  Lisp_Object INTERNAL_FIELD (case_canon_table);

  /* Char-table of equivalences for case-folding search.  */
  Lisp_Object INTERNAL_FIELD (case_eqv_table);

  /* Non-nil means do not display continuation lines.  */
  Lisp_Object INTERNAL_FIELD (truncate_lines);

  /* Non-nil means to use word wrapping when displaying continuation lines.  */
  Lisp_Object INTERNAL_FIELD (word_wrap);

  /* Non-nil means display ctl chars with uparrow.  */
  Lisp_Object INTERNAL_FIELD (ctl_arrow);

  /* Non-nil means reorder bidirectional text for display in the
     visual order.  */
  Lisp_Object INTERNAL_FIELD (bidi_display_reordering);

  /* If non-nil, specifies which direction of text to force in all the
     paragraphs of the buffer.  Nil means determine paragraph
     direction dynamically for each paragraph.  */
  Lisp_Object INTERNAL_FIELD (bidi_paragraph_direction);

  /* Non-nil means do selective display;
     see doc string in syms_of_buffer (buffer.c) for details.  */
  Lisp_Object INTERNAL_FIELD (selective_display);

  /* Non-nil means show ... at end of line followed by invisible lines.  */
  Lisp_Object INTERNAL_FIELD (selective_display_ellipses);

  /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer.  */
  Lisp_Object INTERNAL_FIELD (minor_modes);

  /* t if "self-insertion" should overwrite; `binary' if it should also
     overwrite newlines and tabs - for editing executables and the like.  */
  Lisp_Object INTERNAL_FIELD (overwrite_mode);

  /* Non-nil means abbrev mode is on.  Expand abbrevs automatically.  */
  Lisp_Object INTERNAL_FIELD (abbrev_mode);

  /* Display table to use for text in this buffer.  */
  Lisp_Object INTERNAL_FIELD (display_table);

  /* t means the mark and region are currently active.  */
  Lisp_Object INTERNAL_FIELD (mark_active);

  /* Non-nil means the buffer contents are regarded as multi-byte
     form of characters, not a binary code.  */
  Lisp_Object INTERNAL_FIELD (enable_multibyte_characters);

  /* Coding system to be used for encoding the buffer contents on
     saving.  */
  Lisp_Object INTERNAL_FIELD (buffer_file_coding_system);

  /* List of symbols naming the file format used for visited file.  */
  Lisp_Object INTERNAL_FIELD (file_format);

  /* List of symbols naming the file format used for auto-save file.  */
  Lisp_Object INTERNAL_FIELD (auto_save_file_format);

  /* True if the newline position cache and width run cache are
     enabled.  See search.c and indent.c.  */
  Lisp_Object INTERNAL_FIELD (cache_long_line_scans);

  /* If the width run cache is enabled, this table contains the
     character widths width_run_cache (see above) assumes.  When we
     do a thorough redisplay, we compare this against the buffer's
     current display table to see whether the display table has
     affected the widths of any characters.  If it has, we
     invalidate the width run cache, and re-initialize width_table.  */
  Lisp_Object INTERNAL_FIELD (width_table);

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     PT for this buffer when the buffer is not current.  */
  Lisp_Object INTERNAL_FIELD (pt_marker);

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     BEGV for this buffer when the buffer is not current.  */
  Lisp_Object INTERNAL_FIELD (begv_marker);

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     ZV for this buffer when the buffer is not current.  */
  Lisp_Object INTERNAL_FIELD (zv_marker);

  /* This holds the point value before the last scroll operation.
     Explicitly setting point sets this to nil.  */
  Lisp_Object INTERNAL_FIELD (point_before_scroll);

  /* Truename of the visited file, or nil.  */
  Lisp_Object INTERNAL_FIELD (file_truename);

  /* Invisibility spec of this buffer.
     t => any non-nil `invisible' property means invisible.
     A list => `invisible' property means invisible
     if it is memq in that list.  */
  Lisp_Object INTERNAL_FIELD (invisibility_spec);

  /* This is the last window that was selected with this buffer in it,
     or nil if that window no longer displays this buffer.  */
  Lisp_Object INTERNAL_FIELD (last_selected_window);

  /* Incremented each time the buffer is displayed in a window.  */
  Lisp_Object INTERNAL_FIELD (display_count);

  /* Widths of left and right marginal areas for windows displaying
     this buffer.  */
  Lisp_Object INTERNAL_FIELD (left_margin_cols);
  Lisp_Object INTERNAL_FIELD (right_margin_cols);

  /* Widths of left and right fringe areas for windows displaying
     this buffer.  */
  Lisp_Object INTERNAL_FIELD (left_fringe_width);
  Lisp_Object INTERNAL_FIELD (right_fringe_width);

  /* Non-nil means fringes are drawn outside display margins;
     othersize draw them between margin areas and text.  */
  Lisp_Object INTERNAL_FIELD (fringes_outside_margins);

  /* Width and type of scroll bar areas for windows displaying
     this buffer.  */
  Lisp_Object INTERNAL_FIELD (scroll_bar_width);
  Lisp_Object INTERNAL_FIELD (vertical_scroll_bar_type);

  /* Non-nil means indicate lines not displaying text (in a style
     like vi).  */
  Lisp_Object INTERNAL_FIELD (indicate_empty_lines);

  /* Non-nil means indicate buffer boundaries and scrolling.  */
  Lisp_Object INTERNAL_FIELD (indicate_buffer_boundaries);

  /* Logical to physical fringe bitmap mappings.  */
  Lisp_Object INTERNAL_FIELD (fringe_indicator_alist);

  /* Logical to physical cursor bitmap mappings.  */
  Lisp_Object INTERNAL_FIELD (fringe_cursor_alist);

  /* Time stamp updated each time this buffer is displayed in a window.  */
  Lisp_Object INTERNAL_FIELD (display_time);

  /* If scrolling the display because point is below the bottom of a
     window showing this buffer, try to choose a window start so
     that point ends up this number of lines from the top of the
     window.  Nil means that scrolling method isn't used.  */
  Lisp_Object INTERNAL_FIELD (scroll_up_aggressively);

  /* If scrolling the display because point is above the top of a
     window showing this buffer, try to choose a window start so
     that point ends up this number of lines from the bottom of the
     window.  Nil means that scrolling method isn't used.  */
  Lisp_Object INTERNAL_FIELD (scroll_down_aggressively);

  /* Desired cursor type in this buffer.  See the doc string of
     per-buffer variable `cursor-type'.  */
  Lisp_Object INTERNAL_FIELD (cursor_type);

  /* An integer > 0 means put that number of pixels below text lines
     in the display of this buffer.  */
  Lisp_Object INTERNAL_FIELD (extra_line_spacing);

  /* Cursor type to display in non-selected windows.
     t means to use hollow box cursor.
     See `cursor-type' for other values.  */
  Lisp_Object INTERNAL_FIELD (cursor_in_non_selected_windows);

  /* No more Lisp_Object beyond this point.  Except undo_list,
     which is handled specially in Fgarbage_collect .  */

  /* This structure holds the coordinates of the buffer contents
     in ordinary buffers.  In indirect buffers, this is not used.  */
  struct buffer_text own_text;

  /* This points to the `struct buffer_text' that used for this buffer.
     In an ordinary buffer, this is the own_text field above.
     In an indirect buffer, this is the own_text field of another buffer.  */
  struct buffer_text *text;

  /* Char position of point in buffer.  */
  ptrdiff_t pt;

  /* Byte position of point in buffer.  */
  ptrdiff_t pt_byte;

  /* Char position of beginning of accessible range.  */
  ptrdiff_t begv;

  /* Byte position of beginning of accessible range.  */
  ptrdiff_t begv_byte;

  /* Char position of end of accessible range.  */
  ptrdiff_t zv;

  /* Byte position of end of accessible range.  */
  ptrdiff_t zv_byte;

  /* In an indirect buffer, this points to the base buffer.
     In an ordinary buffer, it is 0.  */
  struct buffer *base_buffer;

  /* In an indirect buffer, this is -1. In an ordinary buffer,
     it's the number of indirect buffers that share our text;
     zero means that we're the only owner of this text.  */
  int indirections;

  /* A non-zero value in slot IDX means that per-buffer variable
     with index IDX has a local value in this buffer.  The index IDX
     for a buffer-local variable is stored in that variable's slot
     in buffer_local_flags as a Lisp integer.  If the index is -1,
     this means the variable is always local in all buffers.  */
#define MAX_PER_BUFFER_VARS 50
  char local_flags[MAX_PER_BUFFER_VARS];

  /* Set to the modtime of the visited file when read or written.
     EMACS_NSECS (modtime) == NONEXISTENT_MODTIME_NSECS means
     visited file was nonexistent.  EMACS_NSECS (modtime) ==
     UNKNOWN_MODTIME_NSECS means visited file modtime unknown;
     in no case complain about any mismatch on next save attempt.  */
#define NONEXISTENT_MODTIME_NSECS (-1)
#define UNKNOWN_MODTIME_NSECS (-2)
  EMACS_TIME modtime;

  /* Size of the file when modtime was set.  This is used to detect the
     case where the file grew while we were reading it, so the modtime
     is still the same (since it's rounded up to seconds) but we're actually
     not up-to-date.  -1 means the size is unknown.  Only meaningful if
     modtime is actually set.  */
  off_t modtime_size;

  /* The value of text->modiff at the last auto-save.  */
  EMACS_INT auto_save_modified;

  /* The value of text->modiff at the last display error.
     Redisplay of this buffer is inhibited until it changes again.  */
  EMACS_INT display_error_modiff;

  /* The time at which we detected a failure to auto-save,
     Or 0 if we didn't have a failure.  */
  time_t auto_save_failure_time;

  /* Position in buffer at which display started
     the last time this buffer was displayed.  */
  ptrdiff_t last_window_start;

  /* If the long line scan cache is enabled (i.e. the buffer-local
     variable cache-long-line-scans is non-nil), newline_cache
     points to the newline cache, and width_run_cache points to the
     width run cache.

     The newline cache records which stretches of the buffer are
     known *not* to contain newlines, so that they can be skipped
     quickly when we search for newlines.

     The width run cache records which stretches of the buffer are
     known to contain characters whose widths are all the same.  If
     the width run cache maps a character to a value > 0, that value is
     the character's width; if it maps a character to zero, we don't
     know what its width is.  This allows compute_motion to process
     such regions very quickly, using algebra instead of inspecting
     each character.   See also width_table, below.  */
  struct region_cache *newline_cache;
  struct region_cache *width_run_cache;

  /* Non-zero means don't use redisplay optimizations for
     displaying this buffer.  */
  unsigned prevent_redisplay_optimizations_p : 1;

  /* Non-zero whenever the narrowing is changed in this buffer.  */
  unsigned clip_changed : 1;

  /* List of overlays that end at or before the current center,
     in order of end-position.  */
  struct Lisp_Overlay *overlays_before;

  /* List of overlays that end after  the current center,
     in order of start-position.  */
  struct Lisp_Overlay *overlays_after;

  /* Position where the overlay lists are centered.  */
  ptrdiff_t overlay_center;

  /* Changes in the buffer are recorded here for undo, and t means
     don't record anything.  This information belongs to the base
     buffer of an indirect buffer.  But we can't store it in the
     struct buffer_text because local variables have to be right in
     the struct buffer. So we copy it around in set_buffer_internal.  */
  Lisp_Object INTERNAL_FIELD (undo_list);
};

/* Most code should use these functions to set Lisp fields in struct
   buffer.  */
BUFFER_INLINE void
bset_bidi_paragraph_direction (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (bidi_paragraph_direction) = val;
}
BUFFER_INLINE void
bset_case_canon_table (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (case_canon_table) = val;
}
BUFFER_INLINE void
bset_case_eqv_table (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (case_eqv_table) = val;
}
BUFFER_INLINE void
bset_directory (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (directory) = val;
}
BUFFER_INLINE void
bset_display_count (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (display_count) = val;
}
BUFFER_INLINE void
bset_display_time (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (display_time) = val;
}
BUFFER_INLINE void
bset_downcase_table (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (downcase_table) = val;
}
BUFFER_INLINE void
bset_enable_multibyte_characters (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (enable_multibyte_characters) = val;
}
BUFFER_INLINE void
bset_filename (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (filename) = val;
}
BUFFER_INLINE void
bset_keymap (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (keymap) = val;
}
BUFFER_INLINE void
bset_last_selected_window (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (last_selected_window) = val;
}
BUFFER_INLINE void
bset_local_var_alist (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (local_var_alist) = val;
}
BUFFER_INLINE void
bset_mark_active (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (mark_active) = val;
}
BUFFER_INLINE void
bset_point_before_scroll (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (point_before_scroll) = val;
}
BUFFER_INLINE void
bset_read_only (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (read_only) = val;
}
BUFFER_INLINE void
bset_truncate_lines (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (truncate_lines) = val;
}
BUFFER_INLINE void
bset_undo_list (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (undo_list) = val;
}
BUFFER_INLINE void
bset_upcase_table (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (upcase_table) = val;
}
BUFFER_INLINE void
bset_width_table (struct buffer *b, Lisp_Object val)
{
  b->INTERNAL_FIELD (width_table) = val;
}

/* Convenient check whether buffer B is live.  */

#define BUFFER_LIVE_P(b) (!NILP (BVAR (b, name)))

/* Verify indirection counters.  */

#define BUFFER_CHECK_INDIRECTION(b)			\
  do {							\
    if (BUFFER_LIVE_P (b))				\
    {							\
      if (b->base_buffer)				\
	{						\
	  eassert (b->indirections == -1);		\
	  eassert (b->base_buffer->indirections > 0);	\
	}						\
      else						\
	eassert (b->indirections >= 0);			\
    }							\
  } while (0)

/* Chain of all buffers, including killed ones.  */

extern struct buffer *all_buffers;

/* Used to iterate over the chain above.  */

#define FOR_EACH_BUFFER(b) \
  for ((b) = all_buffers; (b); (b) = (b)->header.next.buffer)

/* This points to the current buffer.  */

extern struct buffer *current_buffer;

/* This structure holds the default values of the buffer-local variables
   that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

extern struct buffer buffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the entry for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_flags array.

   If a slot in this structure is zero, then even though there may
   be a Lisp-level local variable for the slot, it has no default value,
   and the corresponding slot in buffer_defaults is not used.  */


extern struct buffer buffer_local_flags;

/* For each buffer slot, this points to the Lisp symbol name
   for that slot in the current buffer.  It is 0 for slots
   that don't have such names.  */

extern struct buffer buffer_local_symbols;

extern void delete_all_overlays (struct buffer *);
extern void reset_buffer (struct buffer *);
extern void compact_buffer (struct buffer *);
extern void evaporate_overlays (ptrdiff_t);
extern ptrdiff_t overlays_at (EMACS_INT, bool, Lisp_Object **,
			      ptrdiff_t *, ptrdiff_t *, ptrdiff_t *, bool);
extern ptrdiff_t sort_overlays (Lisp_Object *, ptrdiff_t, struct window *);
extern void recenter_overlay_lists (struct buffer *, ptrdiff_t);
extern ptrdiff_t overlay_strings (ptrdiff_t, struct window *, unsigned char **);
extern void validate_region (Lisp_Object *, Lisp_Object *);
extern void set_buffer_internal_1 (struct buffer *);
extern void set_buffer_temp (struct buffer *);
extern Lisp_Object buffer_local_value_1 (Lisp_Object, Lisp_Object);
extern void record_buffer (Lisp_Object);
extern _Noreturn void buffer_slot_type_mismatch (Lisp_Object, int);
extern void fix_overlays_before (struct buffer *, ptrdiff_t, ptrdiff_t);
extern void mmap_set_vars (bool);

/* Set the current buffer to B.

   We previously set windows_or_buffers_changed here to invalidate
   global unchanged information in beg_unchanged and end_unchanged.
   This is no longer necessary because we now compute unchanged
   information on a buffer-basis.  Every action affecting other
   windows than the selected one requires a select_window at some
   time, and that increments windows_or_buffers_changed.  */

BUFFER_INLINE void
set_buffer_internal (struct buffer *b)
{
  if (current_buffer != b)
    set_buffer_internal_1 (b);
}

/* Arrange to go back to the original buffer after the next
   call to unbind_to if the original buffer is still alive.  */

BUFFER_INLINE void
record_unwind_current_buffer (void)
{
  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());
}

/* Get overlays at POSN into array OVERLAYS with NOVERLAYS elements.
   If NEXTP is non-NULL, return next overlay there.
   See overlay_at arg CHANGE_REQ for meaning of CHRQ arg.  */

#define GET_OVERLAYS_AT(posn, overlays, noverlays, nextp, chrq)		\
  do {									\
    ptrdiff_t maxlen = 40;						\
    overlays = alloca (maxlen * sizeof *overlays);			\
    noverlays = overlays_at (posn, 0, &overlays, &maxlen,		\
			     nextp, NULL, chrq);			\
    if (noverlays > maxlen)						\
      {									\
	maxlen = noverlays;						\
	overlays = alloca (maxlen * sizeof *overlays);			\
	noverlays = overlays_at (posn, 0, &overlays, &maxlen,		\
				 nextp, NULL, chrq);			\
      }									\
  } while (0)

extern Lisp_Object Qbefore_change_functions;
extern Lisp_Object Qafter_change_functions;
extern Lisp_Object Qfirst_change_hook;

/* Get text properties of B.  */

BUFFER_INLINE INTERVAL
buffer_intervals (struct buffer *b)
{
  eassert (b->text != NULL);
  return b->text->intervals;
}

/* Set text properties of B to I.  */

BUFFER_INLINE void
set_buffer_intervals (struct buffer *b, INTERVAL i)
{
  eassert (b->text != NULL);
  b->text->intervals = i;
}

/* Non-zero if current buffer has overlays.  */

BUFFER_INLINE bool
buffer_has_overlays (void)
{
  return current_buffer->overlays_before || current_buffer->overlays_after;
}

/* Return character code of multi-byte form at byte position POS.  If POS
   doesn't point the head of valid multi-byte form, only the byte at
   POS is returned.  No range checking.

   WARNING: The character returned by this macro could be "unified"
   inside STRING_CHAR, if the original character in the buffer belongs
   to one of the Private Use Areas (PUAs) of codepoints that Emacs
   uses to support non-unified CJK characters.  If that happens,
   CHAR_BYTES will return a value that is different from the length of
   the original multibyte sequence stored in the buffer.  Therefore,
   do _not_ use FETCH_MULTIBYTE_CHAR if you need to advance through
   the buffer to the next character after fetching this one.  Instead,
   use either FETCH_CHAR_ADVANCE or STRING_CHAR_AND_LENGTH.  */

BUFFER_INLINE int
FETCH_MULTIBYTE_CHAR (ptrdiff_t pos)
{
  unsigned char *p = ((pos >= GPT_BYTE ? GAP_SIZE : 0)
		      + pos + BEG_ADDR - BEG_BYTE);
  return STRING_CHAR (p);
}

/* Return character code of multi-byte form at byte position POS in BUF.
   If POS doesn't point the head of valid multi-byte form, only the byte at
   POS is returned.  No range checking.  */

BUFFER_INLINE int
BUF_FETCH_MULTIBYTE_CHAR (struct buffer *buf, ptrdiff_t pos)
{
  unsigned char *p
    = ((pos >= BUF_GPT_BYTE (buf) ? BUF_GAP_SIZE (buf) : 0)
       + pos + BUF_BEG_ADDR (buf) - BEG_BYTE);
  return STRING_CHAR (p);
}

/* Overlays */

/* Return the marker that stands for where OV starts in the buffer.  */

#define OVERLAY_START(OV) XOVERLAY (OV)->start

/* Return the marker that stands for where OV ends in the buffer.  */

#define OVERLAY_END(OV) XOVERLAY (OV)->end

/* Return the plist of overlay OV.  */

#define OVERLAY_PLIST(OV) XOVERLAY (OV)->plist

/* Return the actual buffer position for the marker P.
   We assume you know which buffer it's pointing into.  */

#define OVERLAY_POSITION(P) \
 (MARKERP (P) ? marker_position (P) : (emacs_abort (), 0))


/***********************************************************************
			Buffer-local Variables
 ***********************************************************************/

/* Number of per-buffer variables used.  */

extern int last_per_buffer_idx;

/* Return the offset in bytes of member VAR of struct buffer
   from the start of a buffer structure.  */

#define PER_BUFFER_VAR_OFFSET(VAR) \
  offsetof (struct buffer, INTERNAL_FIELD (VAR))

/* Used to iterate over normal Lisp_Object fields of struct buffer (all
   Lisp_Objects except undo_list).  If you add, remove, or reorder
   Lisp_Objects in a struct buffer, make sure that this is still correct.  */

#define FOR_EACH_PER_BUFFER_OBJECT_AT(offset)				 \
  for (offset = PER_BUFFER_VAR_OFFSET (name);				 \
       offset <= PER_BUFFER_VAR_OFFSET (cursor_in_non_selected_windows); \
       offset += word_size)

/* Return the index of buffer-local variable VAR.  Each per-buffer
   variable has an index > 0 associated with it, except when it always
   has buffer-local values, in which case the index is -1.  If this is
   0, this is a bug and means that the slot of VAR in
   buffer_local_flags wasn't initialized.  */

#define PER_BUFFER_VAR_IDX(VAR) \
    PER_BUFFER_IDX (PER_BUFFER_VAR_OFFSET (VAR))

/* Value is non-zero if the variable with index IDX has a local value
   in buffer B.  */

#define PER_BUFFER_VALUE_P(B, IDX)		\
    (((IDX) < 0 || IDX >= last_per_buffer_idx)	\
     ? (emacs_abort (), 0)			\
     : ((B)->local_flags[IDX] != 0))

/* Set whether per-buffer variable with index IDX has a buffer-local
   value in buffer B.  VAL zero means it hasn't.  */

#define SET_PER_BUFFER_VALUE_P(B, IDX, VAL)	\
     do {						\
       if ((IDX) < 0 || (IDX) >= last_per_buffer_idx)	\
	 emacs_abort ();				\
       (B)->local_flags[IDX] = (VAL);			\
     } while (0)

/* Return the index value of the per-buffer variable at offset OFFSET
   in the buffer structure.

   If the slot OFFSET has a corresponding default value in
   buffer_defaults, the index value is positive and has only one
   nonzero bit.  When a buffer has its own local value for a slot, the
   bit for that slot (found in the same slot in this structure) is
   turned on in the buffer's local_flags array.

   If the index value is -1, even though there may be a
   DEFVAR_PER_BUFFER for the slot, there is no default value for it;
   and the corresponding slot in buffer_defaults is not used.

   If the index value is -2, then there is no DEFVAR_PER_BUFFER for
   the slot, but there is a default value which is copied into each
   new buffer.

   If a slot in this structure corresponding to a DEFVAR_PER_BUFFER is
   zero, that is a bug */


#define PER_BUFFER_IDX(OFFSET) \
      XINT (*(Lisp_Object *)((OFFSET) + (char *) &buffer_local_flags))

/* Functions to get and set default value of the per-buffer
   variable at offset OFFSET in the buffer structure.  */

BUFFER_INLINE Lisp_Object
per_buffer_default (int offset)
{
  return *(Lisp_Object *)(offset + (char *) &buffer_defaults);
}

BUFFER_INLINE void
set_per_buffer_default (int offset, Lisp_Object value)
{
  *(Lisp_Object *)(offset + (char *) &buffer_defaults) = value;
}

/* Functions to get and set buffer-local value of the per-buffer
   variable at offset OFFSET in the buffer structure.  */

BUFFER_INLINE Lisp_Object
per_buffer_value (struct buffer *b, int offset)
{
  return *(Lisp_Object *)(offset + (char *) b);
}

BUFFER_INLINE void
set_per_buffer_value (struct buffer *b, int offset, Lisp_Object value)
{
  *(Lisp_Object *)(offset + (char *) b) = value;
}

/* Downcase a character C, or make no change if that cannot be done.  */
BUFFER_INLINE int
downcase (int c)
{
  Lisp_Object downcase_table = BVAR (current_buffer, downcase_table);
  Lisp_Object down = CHAR_TABLE_REF (downcase_table, c);
  return NATNUMP (down) ? XFASTINT (down) : c;
}

/* 1 if C is upper case.  */
BUFFER_INLINE bool uppercasep (int c) { return downcase (c) != c; }

/* Upcase a character C known to be not upper case.  */
BUFFER_INLINE int
upcase1 (int c)
{
  Lisp_Object upcase_table = BVAR (current_buffer, upcase_table);
  Lisp_Object up = CHAR_TABLE_REF (upcase_table, c);
  return NATNUMP (up) ? XFASTINT (up) : c;
}

/* 1 if C is lower case.  */
BUFFER_INLINE bool
lowercasep (int c)
{
  return !uppercasep (c) && upcase1 (c) != c;
}

/* Upcase a character C, or make no change if that cannot be done.  */
BUFFER_INLINE int upcase (int c) { return uppercasep (c) ? c : upcase1 (c); }

INLINE_HEADER_END
