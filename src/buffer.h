/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985, 1986, 1993, 1994, 1995 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifdef USE_TEXT_PROPERTIES
#define SET_PT(position) (set_point ((position), current_buffer))
#define TEMP_SET_PT(position) (temp_set_point ((position), current_buffer))

#define BUF_SET_PT(buffer, position) (set_point ((position), (buffer)))
#define BUF_TEMP_SET_PT(buffer, position) (temp_set_point ((position), (buffer)))

#else  /* don't support text properties */

#define SET_PT(position) (current_buffer->pt = (position))
#define TEMP_SET_PT(position) (current_buffer->pt = (position))

#define BUF_SET_PT(buffer, position) (buffer->pt = (position))
#define BUF_TEMP_SET_PT(buffer, position) (buffer->pt = (position))
#endif /* don't support text properties */

/* Character position of beginning of buffer.  */ 
#define BEG (1)

/* Character position of beginning of accessible range of buffer.  */ 
#define BEGV (current_buffer->begv)

/* Character position of point in buffer.  The "+ 0" makes this
   not an l-value, so you can't assign to it.  Use SET_PT instead.  */ 
#define PT (current_buffer->pt + 0)

/* Character position of gap in buffer.  */ 
#define GPT (current_buffer->text->gpt)

/* Character position of end of accessible range of buffer.  */ 
#define ZV (current_buffer->zv)

/* Character position of end of buffer.  */ 
#define Z (current_buffer->text->z)

/* Is the current buffer narrowed? */
#define NARROWED	((BEGV != BEG) || (ZV != Z))

/* Modification count.  */
#define MODIFF (current_buffer->text->modiff)

/* Modification count as of last visit or save.  */
#define SAVE_MODIFF (current_buffer->text->save_modiff)

/* Address of beginning of buffer.  */ 
#define BEG_ADDR (current_buffer->text->beg)

/* Address of beginning of accessible range of buffer.  */ 
#define BEGV_ADDR (&FETCH_CHAR (current_buffer->begv))

/* Address of point in buffer.  */ 
#define PT_ADDR (&FETCH_CHAR (current_buffer->pt))

/* Address of beginning of gap in buffer.  */ 
#define GPT_ADDR (current_buffer->text->beg + current_buffer->text->gpt - 1)

/* Address of end of gap in buffer.  */
#define GAP_END_ADDR (current_buffer->text->beg + current_buffer->text->gpt + current_buffer->text->gap_size - 1)

/* Address of end of accessible range of buffer.  */ 
#define ZV_ADDR (&FETCH_CHAR (current_buffer->zv))

/* Size of gap.  */
#define GAP_SIZE (current_buffer->text->gap_size)

/* Now similar macros for a specified buffer.
   Note that many of these evaluate the buffer argument more than once.  */

/* Character position of beginning of buffer.  */ 
#define BUF_BEG(buf) (1)

/* Character position of beginning of accessible range of buffer.  */ 
#define BUF_BEGV(buf) ((buf)->begv)

/* Character position of point in buffer.  */ 
#define BUF_PT(buf) ((buf)->pt)

/* Character position of gap in buffer.  */ 
#define BUF_GPT(buf) ((buf)->text->gpt)

/* Character position of end of accessible range of buffer.  */ 
#define BUF_ZV(buf) ((buf)->zv)

/* Character position of end of buffer.  */ 
#define BUF_Z(buf) ((buf)->text->z)

/* Is this buffer narrowed? */
#define BUF_NARROWED(buf) ((BUF_BEGV (buf) != BUF_BEG (buf)) \
			   || (BUF_ZV (buf) != BUF_Z (buf)))

/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text->modiff)

/* Modification count as of last visit or save.  */
#define BUF_SAVE_MODIFF(buf) ((buf)->text->save_modiff)

/* Interval tree of buffer.  */
#define BUF_INTERVALS(buf) ((buf)->text->intervals)

/* Marker chain of buffer.  */
#define BUF_MARKERS(buf) ((buf)->text->markers)

/* Address of beginning of buffer.  */
#define BUF_BEG_ADDR(buf) ((buf)->text->beg)

/* Macro for setting the value of BUF_ZV (BUF) to VALUE,
   by varying the end of the accessible region.  */
#define SET_BUF_ZV(buf, value) ((buf)->zv = (value))
#define SET_BUF_PT(buf, value) ((buf)->pt = (value))

/* Size of gap.  */
#define BUF_GAP_SIZE(buf) ((buf)->text->gap_size)

/* Return the address of character at position POS in buffer BUF. 
   Note that both arguments can be computed more than once.  */
#define BUF_CHAR_ADDRESS(buf, pos) \
((buf)->text->beg + (pos) - 1		\
 + ((pos) >= (buf)->text->gpt ? (buf)->text->gap_size : 0))

/* Convert the address of a char in the buffer into a character position.  */
#define PTR_CHAR_POS(ptr) \
((ptr) - (current_buffer)->text->beg					\
 - (ptr - (current_buffer)->text->beg < (unsigned) GPT ? 0 : GAP_SIZE)	\
 + 1)

/* Convert the address of a char in the buffer into a character position.  */
#define BUF_PTR_CHAR_POS(buf, ptr)			\
((ptr) - (buf)->text->beg				\
 - (ptr - (buf)->text->beg < (unsigned) BUF_GPT ((buf))	\
    ? 0 : BUF_GAP_SIZE ((buf)))				\
 + 1)

struct buffer_text
  {
    unsigned char *beg;		/* Actual address of buffer contents.  */
    int gpt;			/* Index of gap in buffer.  */
    int z;			/* Index of end of buffer.  */
    int gap_size;		/* Size of buffer's gap.  */
    int modiff;			/* This counts buffer-modification events
				   for this buffer.  It is incremented for
				   each such event, and never otherwise
				   changed.  */
    int save_modiff;		/* Previous value of modiff, as of last
				   time buffer visited or saved a file.  */

    /* Properties of this buffer's text -- conditionally compiled.  */
    DECLARE_INTERVALS

    /* The markers that refer to this buffer.
       This is actually a single marker ---
       successive elements in its marker `chain'
       are the other markers referring to this buffer.  */
    Lisp_Object markers;
  };

struct buffer
  {
    /* Everything before the `name' slot must be of a non-Lisp_Object type,
       and every slot after `name' must be a Lisp_Object.

       Check out mark_buffer (alloc.c) to see why.  */

    EMACS_INT size;

    /* Next buffer, in chain of all buffers including killed buffers.
       This chain is used only for garbage collection, in order to
       collect killed buffers properly.
       Note that vectors and most pseudovectors are all on one chain,
       but buffers are on a separate chain of their own.  */
    struct buffer *next;

    /* This structure holds the coordinates of the buffer contents
       in ordinary buffers.  In indirect buffers, this is not used.  */
    struct buffer_text own_text;

    /* This points to the `struct buffer_text' that used for this buffer.
       In an ordinary buffer, this is the own_text field above.
       In an indirect buffer, this is the own_text field of another buffer.  */
    struct buffer_text *text;

    /* Position of point in buffer.  */
    int pt;
    /* Index of beginning of accessible range.  */
    int begv;
    /* Index of end of accessible range.  */
    int zv;

    /* In an indirect buffer, this points to the base buffer.
       In an ordinary buffer, it is 0.  */
    struct buffer *base_buffer;

    /* Flags saying which DEFVAR_PER_BUFFER variables
       are local to this buffer.  */
    int local_var_flags;
    /* Set to the modtime of the visited file when read or written.
       -1 means visited file was nonexistent.
       0 means visited file modtime unknown; in no case complain
       about any mismatch on next save attempt.  */
    int modtime;
    /* the value of text->modiff at the last auto-save.  */
    int auto_save_modified;
    /* The time at which we detected a failure to auto-save,
       Or -1 if we didn't have a failure.  */
    int auto_save_failure_time;
    /* Position in buffer at which display started
       the last time this buffer was displayed.  */
    int last_window_start;

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

    /* Everything from here down must be a Lisp_Object */


    /* The name of this buffer.  */
    Lisp_Object name;
    /* The name of the file visited in this buffer, or nil.  */
    Lisp_Object filename;
    /* Dir for expanding relative file names.  */
    Lisp_Object directory;
    /* True iff this buffer has been backed up (if you write to the
       visited file and it hasn't been backed up, then a backup will
       be made).  */
    /* This isn't really used by the C code, so could be deleted.  */
    Lisp_Object backed_up;
    /* Length of file when last read or saved.
       This is not in the  struct buffer_text
       because it's not used in indirect buffers at all.  */
    Lisp_Object save_length;
    /* File name used for auto-saving this buffer.
       This is not in the  struct buffer_text
       because it's not used in indirect buffers at all.  */
    Lisp_Object auto_save_file_name;

    /* Non-nil if buffer read-only.  */
    Lisp_Object read_only;
    /* "The mark".  This is a marker which may
       point into this buffer or may point nowhere.  */
    Lisp_Object mark;

    /* Alist of elements (SYMBOL . VALUE-IN-THIS-BUFFER)
       for all per-buffer variables of this buffer.  */
    Lisp_Object local_var_alist;

    /* Symbol naming major mode (eg, lisp-mode).  */
    Lisp_Object major_mode;
    /* Pretty name of major mode (eg, "Lisp"). */
    Lisp_Object mode_name;
    /* Mode line element that controls format of mode line.  */
    Lisp_Object mode_line_format;

    /* Keys that are bound local to this buffer.  */
    Lisp_Object keymap;
    /* This buffer's local abbrev table.  */
    Lisp_Object abbrev_table;
    /* This buffer's syntax table.  */
    Lisp_Object syntax_table;

    /* Values of several buffer-local variables */
    /* tab-width is buffer-local so that redisplay can find it
       in buffers that are not current */
    Lisp_Object case_fold_search;
    Lisp_Object tab_width;
    Lisp_Object fill_column;
    Lisp_Object left_margin;
    /* Function to call when insert space past fill column.  */
    Lisp_Object auto_fill_function;
    /* nil: text, t: binary.
       This value is meaningful only on certain operating systems.  */
    Lisp_Object buffer_file_type;

    /* String of length 256 mapping each char to its lower-case version.  */
    Lisp_Object downcase_table;
    /* String of length 256 mapping each char to its upper-case version.  */
    Lisp_Object upcase_table;
    /* Translate table for case-folding search.  */
    Lisp_Object case_canon_table;
    /* Inverse translate (equivalence class) table for case-folding search.  */
    Lisp_Object case_eqv_table;

    /* Non-nil means do not display continuation lines.  */
    Lisp_Object truncate_lines;
    /* Non-nil means display ctl chars with uparrow.  */
    Lisp_Object ctl_arrow;
    /* Non-nil means do selective display;
       see doc string in syms_of_buffer (buffer.c) for details.  */
    Lisp_Object selective_display;
#ifndef old
    /* Non-nil means show ... at end of line followed by invisible lines.  */
    Lisp_Object selective_display_ellipses;
#endif
    /* Alist of (FUNCTION . STRING) for each minor mode enabled in buffer.  */
    Lisp_Object minor_modes;
    /* t if "self-insertion" should overwrite; `binary' if it should also
       overwrite newlines and tabs - for editing executables and the like.  */
    Lisp_Object overwrite_mode;
    /* non-nil means abbrev mode is on.  Expand abbrevs automatically.  */
    Lisp_Object abbrev_mode;
    /* Display table to use for text in this buffer.  */
    Lisp_Object display_table;
    /* t means the mark and region are currently active.  */
    Lisp_Object mark_active;

    /* Changes in the buffer are recorded here for undo.
       t means don't record anything.
       This information belongs to the base buffer of an indirect buffer,
       But we can't store it in the  struct buffer_text
       because local variables have to be right in the  struct buffer.
       So we copy it around in set_buffer_internal.  */
    Lisp_Object undo_list;

    /* List of overlays that end at or before the current center,
       in order of end-position.  */
    Lisp_Object overlays_before;

    /* List of overlays that end after  the current center,
       in order of start-position.  */
    Lisp_Object overlays_after;

    /* Position where the overlay lists are centered.  */
    Lisp_Object overlay_center;

    /* Lisp of symbols naming the file format used for visited file. */
    Lisp_Object file_format;

    /* True if the newline position cache and width run cache are
       enabled.  See search.c and indent.c.  */
    Lisp_Object cache_long_line_scans;

    /* If the width run cache is enabled, this table contains the
       character widths width_run_cache (see above) assumes.  When we
       do a thorough redisplay, we compare this against the buffer's
       current display table to see whether the display table has
       affected the widths of any characters.  If it has, we
       invalidate the width run cache, and re-initialize width_table.  */
    Lisp_Object width_table;

    /* In an indirect buffer, or a buffer that is the base of an
       indirect buffer, this holds a marker that records
       PT for this buffer when the buffer is not current.  */
    Lisp_Object pt_marker;

    /* In an indirect buffer, or a buffer that is the base of an
       indirect buffer, this holds a marker that records
       BEGV for this buffer when the buffer is not current.  */
    Lisp_Object begv_marker;

    /* In an indirect buffer, or a buffer that is the base of an
       indirect buffer, this holds a marker that records
       ZV for this buffer when the buffer is not current.  */
    Lisp_Object zv_marker;

    /* This holds the point value before the last scroll operation.
       Explicitly setting point sets this to nil.  */
    Lisp_Object point_before_scroll;

    /* Truename of the visited file, or nil.  */
    Lisp_Object file_truename;

    /* Invisibility spec of this buffer.
       t => any non-nil `invisible' property means invisible.
       A list => `invisible' property means invisible
                 if it is memq in that list.  */
    Lisp_Object invisibility_spec;
  };

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
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is zero, then even though there may
   be a Lisp-level local variable for the slot, it has no default value,
   and the corresponding slot in buffer_defaults is not used.  */

extern struct buffer buffer_local_flags;

/* For each buffer slot, this points to the Lisp symbol name
   for that slot in the current buffer.  It is 0 for slots
   that don't have such names.  */

extern struct buffer buffer_local_symbols;

/* This structure holds the required types for the values in the
   buffer-local slots.  If a slot contains Qnil, then the
   corresponding buffer slot may contain a value of any type.  If a
   slot contains an integer, then prospective values' tags must be
   equal to that integer.  When a tag does not match, the function
   buffer_slot_type_mismatch will signal an error.  The value Qnil may
   always be safely stored in any slot.  */
extern struct buffer buffer_local_types;

/* Point in the current buffer.  This is an obsolete alias
   and should be eliminated.  */
#define point (current_buffer->pt + 0)

/* Return character at position n.  No range checking.  */
#define FETCH_CHAR(n) *(((n)>= GPT ? GAP_SIZE : 0) + (n) + BEG_ADDR - 1)

/* BUFFER_CEILING_OF (resp. BUFFER_FLOOR_OF), when applied to n, return
   the max (resp. min) p such that

   &FETCH_CHAR (p) - &FETCH_CHAR (n) == p - n       */

#define BUFFER_CEILING_OF(n) (((n) < GPT && GPT < ZV ? GPT : ZV) - 1)
#define BUFFER_FLOOR_OF(n) (BEGV <= GPT && GPT <= (n) ? GPT : BEGV)

extern void reset_buffer ();
extern void evaporate_overlays ();

extern Lisp_Object Fbuffer_name ();
extern Lisp_Object Fget_file_buffer ();
extern Lisp_Object Fnext_overlay_change ();
extern Lisp_Object Fdelete_overlay ();

/* Functions to call before and after each text change.  */
extern Lisp_Object Vbefore_change_function;
extern Lisp_Object Vafter_change_function;
extern Lisp_Object Vbefore_change_functions;
extern Lisp_Object Vafter_change_functions;
extern Lisp_Object Vfirst_change_hook;
extern Lisp_Object Qfirst_change_hook;

extern Lisp_Object Vdeactivate_mark;
extern Lisp_Object Vtransient_mark_mode;

/* Overlays */

/* 1 if the OV is an overlay object.  */
#define OVERLAY_VALID(OV) (OVERLAYP (OV))

/* Return the marker that stands for where OV starts in the buffer.  */
#define OVERLAY_START(OV) (XOVERLAY (OV)->start)

/* Return the marker that stands for where OV ends in the buffer.  */
#define OVERLAY_END(OV) (XOVERLAY (OV)->end)

/* Return the actual buffer position for the marker P.
   We assume you know which buffer it's pointing into.  */

#define OVERLAY_POSITION(P) \
 (GC_MARKERP (P) ? marker_position (P) : (abort (), 0))

/* Allocation of buffer text.  */

#ifdef REL_ALLOC
#define BUFFER_ALLOC(data,size) ((unsigned char *) r_alloc (&data, (size)))
#define BUFFER_REALLOC(data,size) ((unsigned char *) r_re_alloc (&data, (size)))
#define BUFFER_FREE(data) (r_alloc_free (&data))
#define R_ALLOC_DECLARE(var,data) (r_alloc_declare (&var, (data)))
#else
#define BUFFER_ALLOC(data,size) (data = (unsigned char *) malloc ((size)))
#define BUFFER_REALLOC(data,size) ((unsigned char *) realloc ((data), (size)))
#define BUFFER_FREE(data) (free ((data)))
#define R_ALLOC_DECLARE(var,data)
#endif
