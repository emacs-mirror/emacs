/* Header file for the buffer manipulation primitives.

Copyright (C) 1985-1986, 1993-1995, 1997-2020 Free Software Foundation,
Inc.

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

#ifndef EMACS_BUFFER_H
#define EMACS_BUFFER_H

#include <sys/types.h>
#include <time.h>

#include "character.h"
#include "lisp.h"

INLINE_HEADER_BEGIN

/* Accessing the parameters of the current buffer.  */

/* These constants and macros come in pairs, one for the char position
   and one for the byte position.  */

/* Position of beginning of buffer.  */
enum { BEG = 1, BEG_BYTE = BEG };

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

/* Modification count.  */
#define MODIFF (current_buffer->text->modiff)

/* Character modification count.  */
#define CHARS_MODIFF (current_buffer->text->chars_modiff)

/* Overlay modification count.  */
#define OVERLAY_MODIFF (current_buffer->text->overlay_modiff)

/* Modification count as of last visit or save.  */
#define SAVE_MODIFF (current_buffer->text->save_modiff)


/* Position of gap in buffer.  */
#define BUF_GPT(buf) ((buf)->text->gpt)
#define BUF_GPT_BYTE(buf) ((buf)->text->gpt_byte)

/* Position of end of buffer.  */
#define BUF_Z(buf) ((buf)->text->z)
#define BUF_Z_BYTE(buf) ((buf)->text->z_byte)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text->beg)

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text->gap_size)

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

/* Compaction count.  */
#define BUF_COMPACT(buf) ((buf)->text->compact)

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

/* Functions to set PT in the current buffer, or another buffer.  */

extern void set_point (ptrdiff_t);
extern void temp_set_point (struct buffer *, ptrdiff_t);
extern void set_point_both (ptrdiff_t, ptrdiff_t);
extern void temp_set_point_both (struct buffer *,
				 ptrdiff_t, ptrdiff_t);
extern void set_point_from_marker (Lisp_Object);
extern void enlarge_buffer_text (struct buffer *, ptrdiff_t);

INLINE void
SET_PT (ptrdiff_t position)
{
  set_point (position);
}
INLINE void
TEMP_SET_PT (ptrdiff_t position)
{
  temp_set_point (current_buffer, position);
}
INLINE void
SET_PT_BOTH (ptrdiff_t position, ptrdiff_t byte)
{
  set_point_both (position, byte);
}
INLINE void
TEMP_SET_PT_BOTH (ptrdiff_t position, ptrdiff_t byte)
{
  temp_set_point_both (current_buffer, position, byte);
}
INLINE void
BUF_TEMP_SET_PT (struct buffer *buffer, ptrdiff_t position)
{
  temp_set_point (buffer, position);
}

/* Maximum number of bytes in a buffer.
   A buffer cannot contain more bytes than a 1-origin fixnum can represent,
   nor can it be so large that C pointer arithmetic stops working.
   The ptrdiff_t cast ensures that this is signed, not unsigned.  */
#define BUF_BYTES_MAX \
  (ptrdiff_t) min (MOST_POSITIVE_FIXNUM - 1, min (SIZE_MAX, PTRDIFF_MAX))

/* Maximum gap size after compact_buffer, in bytes.  Also
   used in make_gap_larger to get some extra reserved space.  */

enum { GAP_BYTES_DFL = 2000 };

/* Minimum gap size after compact_buffer, in bytes.  Also
   used in make_gap_smaller to avoid too small gap size.  */

enum { GAP_BYTES_MIN = 20 };

/* For those very rare cases where you may have a "random" pointer into
   the middle of a multibyte char, this moves to the next boundary.  */
extern ptrdiff_t advance_to_char_boundary (ptrdiff_t byte_pos);

/* Return the byte at byte position N.
   Do not check that the position is in range.  */

#define FETCH_BYTE(n) *(BYTE_POS_ADDR ((n)))

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
    modiff_count modiff;	/* This counts buffer-modification events
				   for this buffer.  It is incremented for
				   each such event, and never otherwise
				   changed.  */
    modiff_count chars_modiff;	/* This is modified with character change
				   events for this buffer.  It is set to
				   modiff for each such event, and never
				   otherwise changed.  */
    modiff_count save_modiff;	/* Previous value of modiff, as of last
				   time buffer visited or saved a file.  */

    modiff_count overlay_modiff; /* Counts modifications to overlays.  */

    modiff_count compact;	/* Set to modiff each time when compact_buffer
				   is called for this buffer.  */

    /* Minimum value of GPT - BEG since last redisplay that finished.  */
    ptrdiff_t beg_unchanged;

    /* Minimum value of Z - GPT since last redisplay that finished.  */
    ptrdiff_t end_unchanged;

    /* MODIFF as of last redisplay that finished; if it matches MODIFF,
       beg_unchanged and end_unchanged contain no useful information.  */
    modiff_count unchanged_modified;

    /* BUF_OVERLAY_MODIFF of current buffer, as of last redisplay that
       finished; if it matches BUF_OVERLAY_MODIFF, beg_unchanged and
       end_unchanged contain no useful information.  */
    modiff_count overlay_unchanged_modified;

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

    /* Usually false.  Temporarily true in decode_coding_gap to
       prevent Fgarbage_collect from shrinking the gap and losing
       not-yet-decoded bytes.  */
    bool_bf inhibit_shrinking : 1;

    /* True if it needs to be redisplayed.  */
    bool_bf redisplay : 1;
  };

/* Most code should use this macro to access Lisp fields in struct buffer.  */

#define BVAR(buf, field) ((buf)->field ## _)

/* Max number of builtin per-buffer variables.  */
enum { MAX_PER_BUFFER_VARS = 50 };

/* Special values for struct buffer.modtime.  */
enum { NONEXISTENT_MODTIME_NSECS = -1 };
enum { UNKNOWN_MODTIME_NSECS = -2 };

/* This is the structure that the buffer Lisp object points to.  */

struct buffer
{
  union vectorlike_header header;

  /* The name of this buffer.  */
  Lisp_Object name_;

  /* The name of the file visited in this buffer, or nil.  */
  Lisp_Object filename_;

  /* Directory for expanding relative file names.  */
  Lisp_Object directory_;

  /* True if this buffer has been backed up (if you write to the visited
     file and it hasn't been backed up, then a backup will be made).  */
  Lisp_Object backed_up_;

  /* Length of file when last read or saved.
     -1 means auto saving turned off because buffer shrank a lot.
     -2 means don't turn off auto saving if buffer shrinks.
       (That value is used with buffer-swap-text.)
     This is not in the  struct buffer_text
     because it's not used in indirect buffers at all.  */
  Lisp_Object save_length_;

  /* File name used for auto-saving this buffer.
     This is not in the  struct buffer_text
     because it's not used in indirect buffers at all.  */
  Lisp_Object auto_save_file_name_;

  /* Non-nil if buffer read-only.  */
  Lisp_Object read_only_;

  /* "The mark".  This is a marker which may
     point into this buffer or may point nowhere.  */
  Lisp_Object mark_;

  /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER) for all
     per-buffer variables of this buffer.  For locally unbound
     symbols, just the symbol appears as the element.  */
  Lisp_Object local_var_alist_;

  /* Symbol naming major mode (e.g., lisp-mode).  */
  Lisp_Object major_mode_;

  /* Pretty name of major mode (e.g., "Lisp"). */
  Lisp_Object mode_name_;

  /* Mode line element that controls format of mode line.  */
  Lisp_Object mode_line_format_;

  /* Analogous to mode_line_format for the line displayed at the top
     of windows.  Nil means don't display that line.  */
  Lisp_Object header_line_format_;

  /* Analogous to mode_line_format for the line displayed at the top
     of windows.  Nil means don't display that line.  */
  Lisp_Object tab_line_format_;

  /* Keys that are bound local to this buffer.  */
  Lisp_Object keymap_;

  /* This buffer's local abbrev table.  */
  Lisp_Object abbrev_table_;

  /* This buffer's syntax table.  */
  Lisp_Object syntax_table_;

  /* This buffer's category table.  */
  Lisp_Object category_table_;

  /* Values of several buffer-local variables.  */
  /* tab-width is buffer-local so that redisplay can find it
     in buffers that are not current.  */
  Lisp_Object case_fold_search_;
  Lisp_Object tab_width_;
  Lisp_Object fill_column_;
  Lisp_Object left_margin_;

  /* Function to call when insert space past fill column.  */
  Lisp_Object auto_fill_function_;

  /* Case table for case-conversion in this buffer.
     This char-table maps each char into its lower-case version.  */
  Lisp_Object downcase_table_;

  /* Char-table mapping each char to its upper-case version.  */
  Lisp_Object upcase_table_;

  /* Char-table for conversion for case-folding search.  */
  Lisp_Object case_canon_table_;

  /* Char-table of equivalences for case-folding search.  */
  Lisp_Object case_eqv_table_;

  /* Non-nil means do not display continuation lines.  */
  Lisp_Object truncate_lines_;

  /* Non-nil means to use word wrapping when displaying continuation lines.  */
  Lisp_Object word_wrap_;

  /* Non-nil means display ctl chars with uparrow.  */
  Lisp_Object ctl_arrow_;

  /* Non-nil means reorder bidirectional text for display in the
     visual order.  */
  Lisp_Object bidi_display_reordering_;

  /* If non-nil, specifies which direction of text to force in all the
     paragraphs of the buffer.  Nil means determine paragraph
     direction dynamically for each paragraph.  */
  Lisp_Object bidi_paragraph_direction_;

  /* If non-nil, a regular expression for bidi paragraph separator.  */
  Lisp_Object bidi_paragraph_separate_re_;

  /* If non-nil, a regular expression for bidi paragraph start.  */
  Lisp_Object bidi_paragraph_start_re_;

  /* Non-nil means do selective display;
     see doc string in syms_of_buffer (buffer.c) for details.  */
  Lisp_Object selective_display_;

  /* Non-nil means show ... at end of line followed by invisible lines.  */
  Lisp_Object selective_display_ellipses_;

  /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer.  */
  Lisp_Object minor_modes_;

  /* t if "self-insertion" should overwrite; `binary' if it should also
     overwrite newlines and tabs - for editing executables and the like.  */
  Lisp_Object overwrite_mode_;

  /* Non-nil means abbrev mode is on.  Expand abbrevs automatically.  */
  Lisp_Object abbrev_mode_;

  /* Display table to use for text in this buffer.  */
  Lisp_Object display_table_;

  /* t means the mark and region are currently active.  */
  Lisp_Object mark_active_;

  /* Non-nil means the buffer contents are regarded as multi-byte
     form of characters, not a binary code.  */
  Lisp_Object enable_multibyte_characters_;

  /* Coding system to be used for encoding the buffer contents on
     saving.  */
  Lisp_Object buffer_file_coding_system_;

  /* List of symbols naming the file format used for visited file.  */
  Lisp_Object file_format_;

  /* List of symbols naming the file format used for auto-save file.  */
  Lisp_Object auto_save_file_format_;

  /* True if the newline position cache, width run cache and BIDI paragraph
     cache are enabled.  See search.c, indent.c and bidi.c for details.  */
  Lisp_Object cache_long_scans_;

  /* If the width run cache is enabled, this table contains the
     character widths width_run_cache (see above) assumes.  When we
     do a thorough redisplay, we compare this against the buffer's
     current display table to see whether the display table has
     affected the widths of any characters.  If it has, we
     invalidate the width run cache, and re-initialize width_table.  */
  Lisp_Object width_table_;

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     PT for this buffer when the buffer is not current.  */
  Lisp_Object pt_marker_;

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     BEGV for this buffer when the buffer is not current.  */
  Lisp_Object begv_marker_;

  /* In an indirect buffer, or a buffer that is the base of an
     indirect buffer, this holds a marker that records
     ZV for this buffer when the buffer is not current.  */
  Lisp_Object zv_marker_;

  /* This holds the point value before the last scroll operation.
     Explicitly setting point sets this to nil.  */
  Lisp_Object point_before_scroll_;

  /* Truename of the visited file, or nil.  */
  Lisp_Object file_truename_;

  /* Invisibility spec of this buffer.
     t => any non-nil `invisible' property means invisible.
     A list => `invisible' property means invisible
     if it is memq in that list.  */
  Lisp_Object invisibility_spec_;

  /* This is the last window that was selected with this buffer in it,
     or nil if that window no longer displays this buffer.  */
  Lisp_Object last_selected_window_;

  /* Incremented each time the buffer is displayed in a window.  */
  Lisp_Object display_count_;

  /* Widths of left and right marginal areas for windows displaying
     this buffer.  */
  Lisp_Object left_margin_cols_;
  Lisp_Object right_margin_cols_;

  /* Widths of left and right fringe areas for windows displaying
     this buffer.  */
  Lisp_Object left_fringe_width_;
  Lisp_Object right_fringe_width_;

  /* Non-nil means fringes are drawn outside display margins;
     othersize draw them between margin areas and text.  */
  Lisp_Object fringes_outside_margins_;

  /* Width, height and types of scroll bar areas for windows displaying
     this buffer.  */
  Lisp_Object scroll_bar_width_;
  Lisp_Object scroll_bar_height_;
  Lisp_Object vertical_scroll_bar_type_;
  Lisp_Object horizontal_scroll_bar_type_;

  /* Non-nil means indicate lines not displaying text (in a style
     like vi).  */
  Lisp_Object indicate_empty_lines_;

  /* Non-nil means indicate buffer boundaries and scrolling.  */
  Lisp_Object indicate_buffer_boundaries_;

  /* Logical to physical fringe bitmap mappings.  */
  Lisp_Object fringe_indicator_alist_;

  /* Logical to physical cursor bitmap mappings.  */
  Lisp_Object fringe_cursor_alist_;

  /* Time stamp updated each time this buffer is displayed in a window.  */
  Lisp_Object display_time_;

  /* If scrolling the display because point is below the bottom of a
     window showing this buffer, try to choose a window start so
     that point ends up this number of lines from the top of the
     window.  Nil means that scrolling method isn't used.  */
  Lisp_Object scroll_up_aggressively_;

  /* If scrolling the display because point is above the top of a
     window showing this buffer, try to choose a window start so
     that point ends up this number of lines from the bottom of the
     window.  Nil means that scrolling method isn't used.  */
  Lisp_Object scroll_down_aggressively_;

  /* Desired cursor type in this buffer.  See the doc string of
     per-buffer variable `cursor-type'.  */
  Lisp_Object cursor_type_;

  /* An integer > 0 means put that number of pixels below text lines
     in the display of this buffer.  */
  Lisp_Object extra_line_spacing_;

  /* Cursor type to display in non-selected windows.
     t means to use hollow box cursor.
     See `cursor-type' for other values.  */
  Lisp_Object cursor_in_non_selected_windows_;

  /* No more Lisp_Object beyond cursor_in_non_selected_windows_.
     Except undo_list, which is handled specially in Fgarbage_collect.  */

  /* This structure holds the coordinates of the buffer contents
     in ordinary buffers.  In indirect buffers, this is not used.  */
  struct buffer_text own_text;

  /* This points to the `struct buffer_text' that used for this buffer.
     In an ordinary buffer, this is the own_text field above.
     In an indirect buffer, this is the own_text field of another buffer.  */
  struct buffer_text *text;

  /* Next buffer, in chain of all buffers, including killed ones.  */
  struct buffer *next;

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

  /* In an indirect buffer, this is -1.  In an ordinary buffer,
     it's the number of indirect buffers that share our text;
     zero means that we're the only owner of this text.  */
  int indirections;

  /* Number of windows showing this buffer.  Always -1 for
     an indirect buffer since it counts as its base buffer.  */
  int window_count;

  /* A non-zero value in slot IDX means that per-buffer variable
     with index IDX has a local value in this buffer.  The index IDX
     for a buffer-local variable is stored in that variable's slot
     in buffer_local_flags as a Lisp integer.  If the index is -1,
     this means the variable is always local in all buffers.  */
  char local_flags[MAX_PER_BUFFER_VARS];

  /* Set to the modtime of the visited file when read or written.
     modtime.tv_nsec == NONEXISTENT_MODTIME_NSECS means
     visited file was nonexistent.  modtime.tv_nsec ==
     UNKNOWN_MODTIME_NSECS means visited file modtime unknown;
     in no case complain about any mismatch on next save attempt.  */
  struct timespec modtime;

  /* Size of the file when modtime was set.  This is used to detect the
     case where the file grew while we were reading it, so the modtime
     is still the same (since it's rounded up to seconds) but we're actually
     not up-to-date.  -1 means the size is unknown.  Only meaningful if
     modtime is actually set.  */
  off_t modtime_size;

  /* The value of text->modiff at the last auto-save.  */
  modiff_count auto_save_modified;

  /* The value of text->modiff at the last display error.
     Redisplay of this buffer is inhibited until it changes again.  */
  modiff_count display_error_modiff;

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
     each character.   See also width_table, below.

     The latter cache is used to speedup bidi_find_paragraph_start.  */
  struct region_cache *newline_cache;
  struct region_cache *width_run_cache;
  struct region_cache *bidi_paragraph_cache;

  /* Non-zero means disable redisplay optimizations when rebuilding the glyph
     matrices (but not when redrawing).  */
  bool_bf prevent_redisplay_optimizations_p : 1;

  /* Non-zero whenever the narrowing is changed in this buffer.  */
  bool_bf clip_changed : 1;

  /* Non-zero for internally used temporary buffers that don't need to
     run hooks kill-buffer-hook, buffer-list-update-hook, and
     kill-buffer-query-functions.  This is used in coding.c to avoid
     slowing down en/decoding when there are a lot of these hooks
     defined.  */
  bool_bf inhibit_buffer_hooks : 1;

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
  Lisp_Object undo_list_;
};

INLINE bool
BUFFERP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_BUFFER);
}

INLINE void
CHECK_BUFFER (Lisp_Object x)
{
  CHECK_TYPE (BUFFERP (x), Qbufferp, x);
}

INLINE struct buffer *
XBUFFER (Lisp_Object a)
{
  eassert (BUFFERP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct buffer);
}

/* Most code should use these functions to set Lisp fields in struct
   buffer.  (Some setters that are private to a single .c file are
   defined as static in those files.)  */
INLINE void
bset_bidi_paragraph_direction (struct buffer *b, Lisp_Object val)
{
  b->bidi_paragraph_direction_ = val;
}
INLINE void
bset_cache_long_scans (struct buffer *b, Lisp_Object val)
{
  b->cache_long_scans_ = val;
}
INLINE void
bset_case_canon_table (struct buffer *b, Lisp_Object val)
{
  b->case_canon_table_ = val;
}
INLINE void
bset_case_eqv_table (struct buffer *b, Lisp_Object val)
{
  b->case_eqv_table_ = val;
}
INLINE void
bset_directory (struct buffer *b, Lisp_Object val)
{
  b->directory_ = val;
}
INLINE void
bset_display_count (struct buffer *b, Lisp_Object val)
{
  b->display_count_ = val;
}
INLINE void
bset_left_margin_cols (struct buffer *b, Lisp_Object val)
{
  b->left_margin_cols_ = val;
}
INLINE void
bset_right_margin_cols (struct buffer *b, Lisp_Object val)
{
  b->right_margin_cols_ = val;
}
INLINE void
bset_display_time (struct buffer *b, Lisp_Object val)
{
  b->display_time_ = val;
}
INLINE void
bset_downcase_table (struct buffer *b, Lisp_Object val)
{
  b->downcase_table_ = val;
}
INLINE void
bset_enable_multibyte_characters (struct buffer *b, Lisp_Object val)
{
  b->enable_multibyte_characters_ = val;
}
INLINE void
bset_filename (struct buffer *b, Lisp_Object val)
{
  b->filename_ = val;
}
INLINE void
bset_keymap (struct buffer *b, Lisp_Object val)
{
  b->keymap_ = val;
}
INLINE void
bset_last_selected_window (struct buffer *b, Lisp_Object val)
{
  b->last_selected_window_ = val;
}
INLINE void
bset_local_var_alist (struct buffer *b, Lisp_Object val)
{
  b->local_var_alist_ = val;
}
INLINE void
bset_mark_active (struct buffer *b, Lisp_Object val)
{
  b->mark_active_ = val;
}
INLINE void
bset_point_before_scroll (struct buffer *b, Lisp_Object val)
{
  b->point_before_scroll_ = val;
}
INLINE void
bset_read_only (struct buffer *b, Lisp_Object val)
{
  b->read_only_ = val;
}
INLINE void
bset_truncate_lines (struct buffer *b, Lisp_Object val)
{
  b->truncate_lines_ = val;
}
INLINE void
bset_undo_list (struct buffer *b, Lisp_Object val)
{
  b->undo_list_ = val;
}
INLINE void
bset_upcase_table (struct buffer *b, Lisp_Object val)
{
  b->upcase_table_ = val;
}
INLINE void
bset_width_table (struct buffer *b, Lisp_Object val)
{
  b->width_table_ = val;
}

/* BUFFER_CEILING_OF (resp. BUFFER_FLOOR_OF), when applied to n, return
   the max (resp. min) p such that

   BYTE_POS_ADDR (p) - BYTE_POS_ADDR (n) == p - n       */

INLINE ptrdiff_t
BUFFER_CEILING_OF (ptrdiff_t bytepos)
{
  return (bytepos < GPT_BYTE && GPT < ZV ? GPT_BYTE : ZV_BYTE) - 1;
}

INLINE ptrdiff_t
BUFFER_FLOOR_OF (ptrdiff_t bytepos)
{
  return BEGV <= GPT && GPT_BYTE <= bytepos ? GPT_BYTE : BEGV_BYTE;
}

/* The BUF_BEGV[_BYTE], BUF_ZV[_BYTE], and BUF_PT[_BYTE] functions cannot
   be used for assignment; use SET_BUF_* functions below for that.  */

/* Position of beginning of accessible range of buffer.  */
INLINE ptrdiff_t
BUF_BEGV (struct buffer *buf)
{
  return (buf == current_buffer ? BEGV
	  : NILP (BVAR (buf, begv_marker)) ? buf->begv
	  : marker_position (BVAR (buf, begv_marker)));
}

INLINE ptrdiff_t
BUF_BEGV_BYTE (struct buffer *buf)
{
  return (buf == current_buffer ? BEGV_BYTE
	  : NILP (BVAR (buf, begv_marker)) ? buf->begv_byte
	  : marker_byte_position (BVAR (buf, begv_marker)));
}

/* Position of point in buffer.  */
INLINE ptrdiff_t
BUF_PT (struct buffer *buf)
{
  return (buf == current_buffer ? PT
	  : NILP (BVAR (buf, pt_marker)) ? buf->pt
	  : marker_position (BVAR (buf, pt_marker)));
}

INLINE ptrdiff_t
BUF_PT_BYTE (struct buffer *buf)
{
  return (buf == current_buffer ? PT_BYTE
	  : NILP (BVAR (buf, pt_marker)) ? buf->pt_byte
	  : marker_byte_position (BVAR (buf, pt_marker)));
}

/* Position of end of accessible range of buffer.  */
INLINE ptrdiff_t
BUF_ZV (struct buffer *buf)
{
  return (buf == current_buffer ? ZV
	  : NILP (BVAR (buf, zv_marker)) ? buf->zv
	  : marker_position (BVAR (buf, zv_marker)));
}

INLINE ptrdiff_t
BUF_ZV_BYTE (struct buffer *buf)
{
  return (buf == current_buffer ? ZV_BYTE
	  : NILP (BVAR (buf, zv_marker)) ? buf->zv_byte
	  : marker_byte_position (BVAR (buf, zv_marker)));
}

/* Similar functions to operate on a specified buffer.  */

/* Position of beginning of buffer.  */
INLINE ptrdiff_t
BUF_BEG (struct buffer *buf)
{
  return BEG;
}

INLINE ptrdiff_t
BUF_BEG_BYTE (struct buffer *buf)
{
  return BEG_BYTE;
}

/* Address of beginning of gap of buffer.  */
INLINE unsigned char *
BUF_GPT_ADDR (struct buffer *buf)
{
  return buf->text->beg + buf->text->gpt_byte - BEG_BYTE;
}

/* Address of end of buffer.  */
INLINE unsigned char *
BUF_Z_ADDR (struct buffer *buf)
{
  return buf->text->beg + buf->text->gap_size + buf->text->z_byte - BEG_BYTE;
}

/* Address of end of gap in buffer.  */
INLINE unsigned char *
BUF_GAP_END_ADDR (struct buffer *buf)
{
  return buf->text->beg + buf->text->gpt_byte + buf->text->gap_size - BEG_BYTE;
}

/* Compute how many characters at the top and bottom of BUF are
   unchanged when the range START..END is modified.  This computation
   must be done each time BUF is modified.  */

INLINE void
BUF_COMPUTE_UNCHANGED (struct buffer *buf, ptrdiff_t start, ptrdiff_t end)
{
  if (BUF_UNCHANGED_MODIFIED (buf) == BUF_MODIFF (buf)
      && (BUF_OVERLAY_UNCHANGED_MODIFIED (buf)
	  == BUF_OVERLAY_MODIFF (buf)))
    {
      buf->text->beg_unchanged = start - BUF_BEG (buf);
      buf->text->end_unchanged = BUF_Z (buf) - (end);
    }
  else
    {
      if (BUF_Z (buf) - end < BUF_END_UNCHANGED (buf))
	buf->text->end_unchanged = BUF_Z (buf) - end;
      if (start - BUF_BEG (buf) < BUF_BEG_UNCHANGED (buf))
	buf->text->beg_unchanged = start - BUF_BEG (buf);
    }
}

/* Functions for setting the BEGV, ZV or PT of a given buffer.

   The ..._BOTH functions take both a charpos and a bytepos,
   which must correspond to each other.

   The functions without ..._BOTH take just a charpos,
   and compute the bytepos from it.  */

INLINE void
SET_BUF_BEGV (struct buffer *buf, ptrdiff_t charpos)
{
  buf->begv_byte = buf_charpos_to_bytepos (buf, charpos);
  buf->begv = charpos;
}

INLINE void
SET_BUF_ZV (struct buffer *buf, ptrdiff_t charpos)
{
  buf->zv_byte = buf_charpos_to_bytepos (buf, charpos);
  buf->zv = charpos;
}

INLINE void
SET_BUF_BEGV_BOTH (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t byte)
{
  buf->begv = charpos;
  buf->begv_byte = byte;
}

INLINE void
SET_BUF_ZV_BOTH (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t byte)
{
  buf->zv = charpos;
  buf->zv_byte = byte;
}

INLINE void
SET_BUF_PT_BOTH (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t byte)
{
  buf->pt = charpos;
  buf->pt_byte = byte;
}

/* Functions to access a character or byte in the current buffer,
   or convert between a byte position and an address.
   These functions do not check that the position is in range.  */

/* Return the address of byte position N in current buffer.  */

INLINE unsigned char *
BYTE_POS_ADDR (ptrdiff_t n)
{
  return (n < GPT_BYTE ? 0 : GAP_SIZE) + n + BEG_ADDR - BEG_BYTE;
}

/* Return the address of char position N.  */

INLINE unsigned char *
CHAR_POS_ADDR (ptrdiff_t n)
{
  return ((n < GPT ? 0 : GAP_SIZE)
	  + buf_charpos_to_bytepos (current_buffer, n)
	  + BEG_ADDR - BEG_BYTE);
}

/* Convert a character position to a byte position.  */

INLINE ptrdiff_t
CHAR_TO_BYTE (ptrdiff_t charpos)
{
  return buf_charpos_to_bytepos (current_buffer, charpos);
}

/* Convert a byte position to a character position.  */

INLINE ptrdiff_t
BYTE_TO_CHAR (ptrdiff_t bytepos)
{
  return buf_bytepos_to_charpos (current_buffer, bytepos);
}

/* Convert PTR, the address of a byte in the buffer, into a byte position.  */

INLINE ptrdiff_t
PTR_BYTE_POS (unsigned char const *ptr)
{
  ptrdiff_t byte = ptr - current_buffer->text->beg;
  return byte - (byte <= GPT_BYTE - BEG_BYTE ? 0 : GAP_SIZE) + BEG_BYTE;
}

/* Number of Lisp_Objects at the beginning of struct buffer.
   If you add, remove, or reorder Lisp_Objects within buffer
   structure, make sure that this is still correct.  */

enum { BUFFER_LISP_SIZE = PSEUDOVECSIZE (struct buffer,
					 cursor_in_non_selected_windows_) };

/* Allocated size of the struct buffer part beyond leading
   Lisp_Objects, in word_size units.  */

enum { BUFFER_REST_SIZE = VECSIZE (struct buffer) - BUFFER_LISP_SIZE };

/* Initialize the pseudovector header of buffer object.  BUFFER_LISP_SIZE
   is required for GC, but BUFFER_REST_SIZE is set up just to be consistent
   with other pseudovectors.  */

INLINE void
BUFFER_PVEC_INIT (struct buffer *b)
{
  XSETPVECTYPESIZE (b, PVEC_BUFFER, BUFFER_LISP_SIZE, BUFFER_REST_SIZE);
}

/* Convenient check whether buffer B is live.  */

INLINE bool
BUFFER_LIVE_P (struct buffer *b)
{
  return !NILP (BVAR (b, name));
}

/* Convenient check whether buffer B is hidden (i.e. its name
   starts with a space).  Caller must ensure that B is live.  */

INLINE bool
BUFFER_HIDDEN_P (struct buffer *b)
{
  return SREF (BVAR (b, name), 0) == ' ';
}

/* Verify indirection counters.  */

INLINE void
BUFFER_CHECK_INDIRECTION (struct buffer *b)
{
  if (BUFFER_LIVE_P (b))
    {
      if (b->base_buffer)
	{
	  eassert (b->indirections == -1);
	  eassert (b->base_buffer->indirections > 0);
	}
      else
	eassert (b->indirections >= 0);
    }
}

/* Chain of all buffers, including killed ones.  */

extern struct buffer *all_buffers;

/* Used to iterate over the chain above.  */

#define FOR_EACH_BUFFER(b) \
  for ((b) = all_buffers; (b); (b) = (b)->next)

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

/* verify_interval_modification saves insertion hooks here
   to be run later by report_interval_modification.  */
extern Lisp_Object interval_insert_behind_hooks;
extern Lisp_Object interval_insert_in_front_hooks;


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
extern void set_buffer_internal_2 (struct buffer *);
extern void set_buffer_temp (struct buffer *);
extern Lisp_Object buffer_local_value (Lisp_Object, Lisp_Object);
extern void record_buffer (Lisp_Object);
extern void fix_overlays_before (struct buffer *, ptrdiff_t, ptrdiff_t);
extern void mmap_set_vars (bool);
extern void restore_buffer (Lisp_Object);
extern void set_buffer_if_live (Lisp_Object);

/* Return B as a struct buffer pointer, defaulting to the current buffer.  */

INLINE struct buffer *
decode_buffer (Lisp_Object b)
{
  return NILP (b) ? current_buffer : (CHECK_BUFFER (b), XBUFFER (b));
}

/* Set the current buffer to B.

   We previously set windows_or_buffers_changed here to invalidate
   global unchanged information in beg_unchanged and end_unchanged.
   This is no longer necessary because we now compute unchanged
   information on a buffer-basis.  Every action affecting other
   windows than the selected one requires a select_window at some
   time, and that increments windows_or_buffers_changed.  */

INLINE void
set_buffer_internal (struct buffer *b)
{
  if (current_buffer != b)
    set_buffer_internal_1 (b);
}

/* Arrange to go back to the original buffer after the next
   call to unbind_to if the original buffer is still alive.  */

INLINE void
record_unwind_current_buffer (void)
{
  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());
}

/* Get overlays at POSN into array OVERLAYS with NOVERLAYS elements.
   If NEXTP is non-NULL, return next overlay there.
   See overlay_at arg CHANGE_REQ for meaning of CHRQ arg.
   This macro might evaluate its args multiple times,
   and it treat some args as lvalues.  */

#define GET_OVERLAYS_AT(posn, overlays, noverlays, nextp, chrq)		\
  do {									\
    ptrdiff_t maxlen = 40;						\
    SAFE_NALLOCA (overlays, 1, maxlen);					\
    (noverlays) = overlays_at (posn, false, &(overlays), &maxlen,	\
			       nextp, NULL, chrq);			\
    if ((noverlays) > maxlen)						\
      {									\
	maxlen = noverlays;						\
	SAFE_NALLOCA (overlays, 1, maxlen);				\
	(noverlays) = overlays_at (posn, false, &(overlays), &maxlen,	\
				   nextp, NULL, chrq);			\
      }									\
  } while (false)

extern Lisp_Object Vbuffer_alist;

/* FOR_EACH_LIVE_BUFFER (LIST_VAR, BUF_VAR) followed by a statement is
   a `for' loop which iterates over the buffers from Vbuffer_alist.  */

#define FOR_EACH_LIVE_BUFFER(list_var, buf_var)			\
  FOR_EACH_ALIST_VALUE (Vbuffer_alist, list_var, buf_var)

/* Get text properties of B.  */

INLINE INTERVAL
buffer_intervals (struct buffer *b)
{
  eassert (b->text != NULL);
  return b->text->intervals;
}

/* Set text properties of B to I.  */

INLINE void
set_buffer_intervals (struct buffer *b, INTERVAL i)
{
  eassert (b->text != NULL);
  b->text->intervals = i;
}

/* Non-zero if current buffer has overlays.  */

INLINE bool
buffer_has_overlays (void)
{
  return current_buffer->overlays_before || current_buffer->overlays_after;
}

/* Functions for accessing a character or byte,
   or converting between byte positions and addresses,
   in a specified buffer.  */

/* Return character code of multi-byte form at byte position POS.  If POS
   doesn't point the head of valid multi-byte form, only the byte at
   POS is returned.  No range checking.  */

INLINE int
FETCH_MULTIBYTE_CHAR (ptrdiff_t pos)
{
  unsigned char *p = BYTE_POS_ADDR (pos);
  return STRING_CHAR (p);
}

/* Return character code of multi-byte form at byte position POS in BUF.
   If POS doesn't point the head of valid multi-byte form, only the byte at
   POS is returned.  No range checking.  */

INLINE int
BUF_FETCH_MULTIBYTE_CHAR (struct buffer *buf, ptrdiff_t pos)
{
  unsigned char *p
    = ((pos >= BUF_GPT_BYTE (buf) ? BUF_GAP_SIZE (buf) : 0)
       + pos + BUF_BEG_ADDR (buf) - BEG_BYTE);
  return STRING_CHAR (p);
}

/* Return character at byte position POS.
   If the current buffer is unibyte and the character is not ASCII,
   make the returning character multibyte.  */

INLINE int
FETCH_CHAR_AS_MULTIBYTE (ptrdiff_t pos)
{
  return (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	  ? FETCH_MULTIBYTE_CHAR (pos)
	  : UNIBYTE_TO_CHAR (FETCH_BYTE (pos)));
}

/* Return character at byte position POS.
   See the caveat WARNING for FETCH_MULTIBYTE_CHAR above.  */

INLINE int
FETCH_CHAR (ptrdiff_t pos)
{
  return (!NILP (BVAR (current_buffer, enable_multibyte_characters))
	  ? FETCH_MULTIBYTE_CHAR (pos)
	  : FETCH_BYTE (pos));
}

/* Return the address of character at byte position POS in buffer BUF.
   Note that both arguments can be computed more than once.  */

INLINE unsigned char *
BUF_BYTE_ADDRESS (struct buffer *buf, ptrdiff_t pos)
{
  return (buf->text->beg + pos - BEG_BYTE
	  + (pos < buf->text->gpt_byte ? 0 : buf->text->gap_size));
}

/* Return the address of character at char position POS in buffer BUF.
   Note that both arguments can be computed more than once.  */

INLINE unsigned char *
BUF_CHAR_ADDRESS (struct buffer *buf, ptrdiff_t pos)
{
  return (buf->text->beg + buf_charpos_to_bytepos (buf, pos) - BEG_BYTE
	  + (pos < buf->text->gpt ? 0 : buf->text->gap_size));
}

/* Convert PTR, the address of a char in buffer BUF,
   into a character position.  */

INLINE ptrdiff_t
BUF_PTR_BYTE_POS (struct buffer *buf, unsigned char *ptr)
{
  ptrdiff_t byte = ptr - buf->text->beg;
  return (byte - (byte <= BUF_GPT_BYTE (buf) - BEG_BYTE ? 0 : BUF_GAP_SIZE (buf))
	  + BEG_BYTE);
}

/* Return the byte at byte position N in buffer BUF.   */

INLINE unsigned char
BUF_FETCH_BYTE (struct buffer *buf, ptrdiff_t n)
{
  return *BUF_BYTE_ADDRESS (buf, n);
}

/* Return character at byte position POS in buffer BUF.  If BUF is
   unibyte and the character is not ASCII, make the returning
   character multibyte.  */

INLINE int
BUF_FETCH_CHAR_AS_MULTIBYTE (struct buffer *buf, ptrdiff_t pos)
{
  return (! NILP (BVAR (buf, enable_multibyte_characters))
	  ? BUF_FETCH_MULTIBYTE_CHAR (buf, pos)
	  : UNIBYTE_TO_CHAR (BUF_FETCH_BYTE (buf, pos)));
}

/* Return number of windows showing B.  */

INLINE int
buffer_window_count (struct buffer *b)
{
  if (b->base_buffer)
    b = b->base_buffer;
  eassert (b->window_count >= 0);
  return b->window_count;
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

INLINE ptrdiff_t
OVERLAY_POSITION (Lisp_Object p)
{
  return marker_position (p);
}


/***********************************************************************
			Buffer-local Variables
 ***********************************************************************/

/* Return the offset in bytes of member VAR of struct buffer
   from the start of a buffer structure.  */

#define PER_BUFFER_VAR_OFFSET(VAR) \
  offsetof (struct buffer, VAR ## _)

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

extern bool valid_per_buffer_idx (int);

/* Value is true if the variable with index IDX has a local value
   in buffer B.  */

INLINE bool
PER_BUFFER_VALUE_P (struct buffer *b, int idx)
{
  eassert (valid_per_buffer_idx (idx));
  return b->local_flags[idx];
}

/* Set whether per-buffer variable with index IDX has a buffer-local
   value in buffer B.  VAL zero means it hasn't.  */

INLINE void
SET_PER_BUFFER_VALUE_P (struct buffer *b, int idx, bool val)
{
  eassert (valid_per_buffer_idx (idx));
  b->local_flags[idx] = val;
}

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
   zero, that is a bug.  */

INLINE int
PER_BUFFER_IDX (ptrdiff_t offset)
{
  return XFIXNUM (*(Lisp_Object *) (offset + (char *) &buffer_local_flags));
}

/* Functions to get and set default value of the per-buffer
   variable at offset OFFSET in the buffer structure.  */

INLINE Lisp_Object
per_buffer_default (int offset)
{
  return *(Lisp_Object *)(offset + (char *) &buffer_defaults);
}

INLINE void
set_per_buffer_default (int offset, Lisp_Object value)
{
  *(Lisp_Object *)(offset + (char *) &buffer_defaults) = value;
}

/* Functions to get and set buffer-local value of the per-buffer
   variable at offset OFFSET in the buffer structure.  */

INLINE Lisp_Object
per_buffer_value (struct buffer *b, int offset)
{
  return *(Lisp_Object *)(offset + (char *) b);
}

INLINE void
set_per_buffer_value (struct buffer *b, int offset, Lisp_Object value)
{
  *(Lisp_Object *)(offset + (char *) b) = value;
}

/* Downcase a character C, or make no change if that cannot be done.  */
INLINE int
downcase (int c)
{
  Lisp_Object downcase_table = BVAR (current_buffer, downcase_table);
  Lisp_Object down = CHAR_TABLE_REF (downcase_table, c);
  return FIXNATP (down) ? XFIXNAT (down) : c;
}

/* Upcase a character C, or make no change if that cannot be done. */
INLINE int
upcase (int c)
{
  Lisp_Object upcase_table = BVAR (current_buffer, upcase_table);
  Lisp_Object up = CHAR_TABLE_REF (upcase_table, c);
  return FIXNATP (up) ? XFIXNAT (up) : c;
}

/* True if C is upper case.  */
INLINE bool
uppercasep (int c)
{
  return downcase (c) != c;
}

/* True if C is lower case.  */
INLINE bool
lowercasep (int c)
{
  return !uppercasep (c) && upcase (c) != c;
}

INLINE_HEADER_END

#endif /* EMACS_BUFFER_H */
