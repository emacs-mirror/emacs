/* Buffer manipulation primitives for GNU Emacs.
   Copyright (C) 1985,86,87,88,89,93,94,95,97,98, 1999, 2000, 2001, 2002
	Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <config.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <stdio.h>

#ifndef USE_CRT_DLL
extern int errno;
#endif

#ifndef MAXPATHLEN
/* in 4.1, param.h fails to define this. */
#define MAXPATHLEN 1024
#endif /* not MAXPATHLEN */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "lisp.h"
#include "intervals.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "charset.h"
#include "region-cache.h"
#include "indent.h"
#include "blockinput.h"
#include "keyboard.h"
#include "frame.h"

struct buffer *current_buffer;		/* the current buffer */

/* First buffer in chain of all buffers (in reverse order of creation).
   Threaded through ->next.  */

struct buffer *all_buffers;

/* This structure holds the default values of the buffer-local variables
   defined with DEFVAR_PER_BUFFER, that have special slots in each buffer.
   The default value occupies the same slot in this structure
   as an individual buffer's value occupies in that buffer.
   Setting the default value also goes through the alist of buffers
   and stores into each buffer that does not say it has a local value.  */

struct buffer buffer_defaults;

/* A Lisp_Object pointer to the above, used for staticpro */

static Lisp_Object Vbuffer_defaults;

/* This structure marks which slots in a buffer have corresponding
   default values in buffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the entry for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_flags array.

   If a slot in this structure is -1, then even though there may
   be a DEFVAR_PER_BUFFER for the slot, there is no default value for it;
   and the corresponding slot in buffer_defaults is not used.

   If a slot is -2, then there is no DEFVAR_PER_BUFFER for it,
   but there is a default value which is copied into each buffer.

   If a slot in this structure is negative, then even though there may
   be a DEFVAR_PER_BUFFER for the slot, there is no default value for it;
   and the corresponding slot in buffer_defaults is not used.

   If a slot in this structure corresponding to a DEFVAR_PER_BUFFER is
   zero, that is a bug */

struct buffer buffer_local_flags;

/* This structure holds the names of symbols whose values may be
   buffer-local.  It is indexed and accessed in the same way as the above. */

struct buffer buffer_local_symbols;
/* A Lisp_Object pointer to the above, used for staticpro */
static Lisp_Object Vbuffer_local_symbols;

/* This structure holds the required types for the values in the
   buffer-local slots.  If a slot contains Qnil, then the
   corresponding buffer slot may contain a value of any type.  If a
   slot contains an integer, then prospective values' tags must be
   equal to that integer (except nil is always allowed).
   When a tag does not match, the function
   buffer_slot_type_mismatch will signal an error.

   If a slot here contains -1, the corresponding variable is read-only.  */
struct buffer buffer_local_types;

/* Flags indicating which built-in buffer-local variables
   are permanent locals.  */
static char buffer_permanent_local_flags[MAX_PER_BUFFER_VARS];

/* Number of per-buffer variables used.  */

int last_per_buffer_idx;

Lisp_Object Fset_buffer ();
void set_buffer_internal ();
void set_buffer_internal_1 ();
static void call_overlay_mod_hooks ();
static void swap_out_buffer_local_variables ();
static void reset_buffer_local_variables ();

/* Alist of all buffer names vs the buffers. */
/* This used to be a variable, but is no longer,
 to prevent lossage due to user rplac'ing this alist or its elements.  */
Lisp_Object Vbuffer_alist;

/* Functions to call before and after each text change. */
Lisp_Object Vbefore_change_functions;
Lisp_Object Vafter_change_functions;

Lisp_Object Vtransient_mark_mode;

/* t means ignore all read-only text properties.
   A list means ignore such a property if its value is a member of the list.
   Any non-nil value means ignore buffer-read-only.  */
Lisp_Object Vinhibit_read_only;

/* List of functions to call that can query about killing a buffer.
   If any of these functions returns nil, we don't kill it.  */
Lisp_Object Vkill_buffer_query_functions;

/* List of functions to call before changing an unmodified buffer.  */
Lisp_Object Vfirst_change_hook;

Lisp_Object Qfirst_change_hook;
Lisp_Object Qbefore_change_functions;
Lisp_Object Qafter_change_functions;

/* If nonzero, all modification hooks are suppressed.  */
int inhibit_modification_hooks;

Lisp_Object Qfundamental_mode, Qmode_class, Qpermanent_local;

Lisp_Object Qprotected_field;

Lisp_Object QSFundamental;	/* A string "Fundamental" */

Lisp_Object Qkill_buffer_hook;

Lisp_Object Qget_file_buffer;

Lisp_Object Qoverlayp;

Lisp_Object Qpriority, Qwindow, Qevaporate, Qbefore_string, Qafter_string;

Lisp_Object Qmodification_hooks;
Lisp_Object Qinsert_in_front_hooks;
Lisp_Object Qinsert_behind_hooks;

static void alloc_buffer_text P_ ((struct buffer *, size_t));
static void free_buffer_text P_ ((struct buffer *b));
static Lisp_Object copy_overlays P_ ((struct buffer *, Lisp_Object));
static void modify_overlay P_ ((struct buffer *, int, int));


/* For debugging; temporary.  See set_buffer_internal.  */
/* Lisp_Object Qlisp_mode, Vcheck_symbol; */

void
nsberror (spec)
     Lisp_Object spec;
{
  if (STRINGP (spec))
    error ("No buffer named %s", XSTRING (spec)->data);
  error ("Invalid buffer argument");
}

DEFUN ("buffer-live-p", Fbuffer_live_p, Sbuffer_live_p, 1, 1, 0,
  "Return non-nil if OBJECT is a buffer which has not been killed.\n\
Value is nil if OBJECT is not a buffer or if it has been killed.")
  (object)
     Lisp_Object object;
{
  return ((BUFFERP (object) && ! NILP (XBUFFER (object)->name))
	  ? Qt : Qnil);
}

DEFUN ("buffer-list", Fbuffer_list, Sbuffer_list, 0, 1, 0,
  "Return a list of all existing live buffers.\n\
If the optional arg FRAME is a frame, we return that frame's buffer list.")
  (frame)
     Lisp_Object frame;
{
  Lisp_Object framelist, general;
  general = Fmapcar (Qcdr, Vbuffer_alist);

  if (FRAMEP (frame))
    {
      Lisp_Object tail;

      CHECK_FRAME (frame, 1);

      framelist = Fcopy_sequence (XFRAME (frame)->buffer_list);

      /* Remove from GENERAL any buffer that duplicates one in FRAMELIST.  */
      tail = framelist;
      while (! NILP (tail))
	{
	  general = Fdelq (XCAR (tail), general);
	  tail = XCDR (tail);
	}
      return nconc2 (framelist, general);
    }

  return general;
}

/* Like Fassoc, but use Fstring_equal to compare
   (which ignores text properties),
   and don't ever QUIT.  */

static Lisp_Object
assoc_ignore_text_properties (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      tem = Fstring_equal (Fcar (elt), key);
      if (!NILP (tem))
	return elt;
    }
  return Qnil;
}

DEFUN ("get-buffer", Fget_buffer, Sget_buffer, 1, 1, 0,
  "Return the buffer named NAME (a string).\n\
If there is no live buffer named NAME, return nil.\n\
NAME may also be a buffer; if so, the value is that buffer.")
  (name)
     register Lisp_Object name;
{
  if (BUFFERP (name))
    return name;
  CHECK_STRING (name, 0);

  return Fcdr (assoc_ignore_text_properties (name, Vbuffer_alist));
}

DEFUN ("get-file-buffer", Fget_file_buffer, Sget_file_buffer, 1, 1, 0,
  "Return the buffer visiting file FILENAME (a string).\n\
The buffer's `buffer-file-name' must match exactly the expansion of FILENAME.\n\
If there is no such live buffer, return nil.\n\
See also `find-buffer-visiting'.")
  (filename)
     register Lisp_Object filename;
{
  register Lisp_Object tail, buf, tem;
  Lisp_Object handler;

  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qget_file_buffer);
  if (!NILP (handler))
    return call2 (handler, Qget_file_buffer, filename);

  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    {
      buf = Fcdr (XCAR (tail));
      if (!BUFFERP (buf)) continue;
      if (!STRINGP (XBUFFER (buf)->filename)) continue;
      tem = Fstring_equal (XBUFFER (buf)->filename, filename);
      if (!NILP (tem))
	return buf;
    }
  return Qnil;
}

Lisp_Object
get_truename_buffer (filename)
     register Lisp_Object filename;
{
  register Lisp_Object tail, buf, tem;

  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
    {
      buf = Fcdr (XCAR (tail));
      if (!BUFFERP (buf)) continue;
      if (!STRINGP (XBUFFER (buf)->file_truename)) continue;
      tem = Fstring_equal (XBUFFER (buf)->file_truename, filename);
      if (!NILP (tem))
	return buf;
    }
  return Qnil;
}

/* Incremented for each buffer created, to assign the buffer number. */
int buffer_count;

DEFUN ("get-buffer-create", Fget_buffer_create, Sget_buffer_create, 1, 1, 0,
  "Return the buffer named NAME, or create such a buffer and return it.\n\
A new buffer is created if there is no live buffer named NAME.\n\
If NAME starts with a space, the new buffer does not keep undo information.\n\
If NAME is a buffer instead of a string, then it is the value returned.\n\
The value is never nil.")  
  (name)
     register Lisp_Object name;
{
  register Lisp_Object buf;
  register struct buffer *b;

  buf = Fget_buffer (name);
  if (!NILP (buf))
    return buf;

  if (XSTRING (name)->size == 0)
    error ("Empty string for buffer name is not allowed");

  b = (struct buffer *) allocate_buffer ();

  b->size = sizeof (struct buffer) / sizeof (EMACS_INT);

  /* An ordinary buffer uses its own struct buffer_text.  */
  b->text = &b->own_text;
  b->base_buffer = 0;

  BUF_GAP_SIZE (b) = 20;
  BLOCK_INPUT;
  /* We allocate extra 1-byte at the tail and keep it always '\0' for
     anchoring a search.  */
  alloc_buffer_text (b, BUF_GAP_SIZE (b) + 1);
  UNBLOCK_INPUT;
  if (! BUF_BEG_ADDR (b))
    buffer_memory_full ();

  BUF_PT (b) = 1;
  BUF_GPT (b) = 1;
  BUF_BEGV (b) = 1;
  BUF_ZV (b) = 1;
  BUF_Z (b) = 1;
  BUF_PT_BYTE (b) = 1;
  BUF_GPT_BYTE (b) = 1;
  BUF_BEGV_BYTE (b) = 1;
  BUF_ZV_BYTE (b) = 1;
  BUF_Z_BYTE (b) = 1;
  BUF_MODIFF (b) = 1;
  BUF_OVERLAY_MODIFF (b) = 1;
  BUF_SAVE_MODIFF (b) = 1;
  BUF_INTERVALS (b) = 0;
  BUF_UNCHANGED_MODIFIED (b) = 1;
  BUF_OVERLAY_UNCHANGED_MODIFIED (b) = 1;
  BUF_END_UNCHANGED (b) = 0;
  BUF_BEG_UNCHANGED (b) = 0;
  *(BUF_GPT_ADDR (b)) = *(BUF_Z_ADDR (b)) = 0; /* Put an anchor '\0'.  */

  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->width_table = Qnil;
  b->prevent_redisplay_optimizations_p = 1;

  /* Put this on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;

  /* An ordinary buffer normally doesn't need markers
     to handle BEGV and ZV.  */
  b->pt_marker = Qnil;
  b->begv_marker = Qnil;
  b->zv_marker = Qnil;

  name = Fcopy_sequence (name);
  XSTRING (name)->intervals = NULL_INTERVAL;
  b->name = name;

  if (XSTRING (name)->data[0] != ' ')
    b->undo_list = Qnil;
  else
    b->undo_list = Qt;

  reset_buffer (b);
  reset_buffer_local_variables (b, 1);

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buf, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buf), Qnil));

  b->mark = Fmake_marker ();
  BUF_MARKERS (b) = Qnil;
  b->name = name;
  return buf;
}


/* Return a list of overlays which is a copy of the overlay list
   LIST, but for buffer B.  */

static Lisp_Object
copy_overlays (b, list)
     struct buffer *b;
     Lisp_Object list;
{
  Lisp_Object result, buffer;

  XSETBUFFER (buffer, b);

  for (result = Qnil; CONSP (list); list = XCDR (list))
    {
      Lisp_Object overlay, start, end, old_overlay;
      int charpos;

      old_overlay = XCAR (list);
      charpos = marker_position (OVERLAY_START (old_overlay));
      start = Fmake_marker ();
      Fset_marker (start, make_number (charpos), buffer);
      XMARKER (start)->insertion_type
	= XMARKER (OVERLAY_START (old_overlay))->insertion_type;

      charpos = marker_position (OVERLAY_END (old_overlay));
      end = Fmake_marker ();
      Fset_marker (end, make_number (charpos), buffer);
      XMARKER (end)->insertion_type
	= XMARKER (OVERLAY_END (old_overlay))->insertion_type;

      overlay = allocate_misc ();
      XMISCTYPE (overlay) = Lisp_Misc_Overlay;
      OVERLAY_START (overlay) = start;
      OVERLAY_END (overlay) = end;
      OVERLAY_PLIST (overlay) = Fcopy_sequence (OVERLAY_PLIST (old_overlay));
      
      result = Fcons (overlay, result);
    }

  return Fnreverse (result);
}
     

/* Clone per-buffer values of buffer FROM.

   Buffer TO gets the same per-buffer values as FROM, with the
   following exceptions: (1) TO's name is left untouched, (2) markers
   are copied and made to refer to TO, and (3) overlay lists are
   copied.  */

static void
clone_per_buffer_values (from, to)
     struct buffer *from, *to;
{
  Lisp_Object to_buffer;
  int offset;

  XSETBUFFER (to_buffer, to);
  
  for (offset = PER_BUFFER_VAR_OFFSET (name) + sizeof (Lisp_Object);
       offset < sizeof *to;
       offset += sizeof (Lisp_Object))
    {
      Lisp_Object obj;

      obj = PER_BUFFER_VALUE (from, offset);
      if (MARKERP (obj))
	{
	  struct Lisp_Marker *m = XMARKER (obj);
	  obj = Fmake_marker ();
	  XMARKER (obj)->insertion_type = m->insertion_type;
	  set_marker_both (obj, to_buffer, m->charpos, m->bytepos);
	}

      PER_BUFFER_VALUE (to, offset) = obj;
    }

  bcopy (from->local_flags, to->local_flags, sizeof to->local_flags);
  
  to->overlays_before = copy_overlays (to, from->overlays_before);
  to->overlays_after = copy_overlays (to, from->overlays_after);
}


DEFUN ("make-indirect-buffer", Fmake_indirect_buffer, Smake_indirect_buffer,
       2, 3,
       "bMake indirect buffer (to buffer): \nBName of indirect buffer: ",
  "Create and return an indirect buffer for buffer BASE-BUFFER, named NAME.\n\
BASE-BUFFER should be an existing buffer (or buffer name).\n\
NAME should be a string which is not the name of an existing buffer.\n\
Optional argument CLONE non-nil means preserve BASE-BUFFER's state,\n\
such as major and minor modes, in the indirect buffer.\n\
CLONE nil means the indirect buffer's state is reset to default values.")
  (base_buffer, name, clone)
     Lisp_Object base_buffer, name, clone;
{
  Lisp_Object buf;
  struct buffer *b;

  buf = Fget_buffer (name);
  if (!NILP (buf))
    error ("Buffer name `%s' is in use", XSTRING (name)->data);

  base_buffer = Fget_buffer (base_buffer);
  if (NILP (base_buffer))
    error ("No such buffer: `%s'", XSTRING (name)->data);

  if (XSTRING (name)->size == 0)
    error ("Empty string for buffer name is not allowed");

  b = (struct buffer *) allocate_buffer ();
  b->size = sizeof (struct buffer) / sizeof (EMACS_INT);

  if (XBUFFER (base_buffer)->base_buffer)
    b->base_buffer = XBUFFER (base_buffer)->base_buffer;
  else
    b->base_buffer = XBUFFER (base_buffer);

  /* Use the base buffer's text object.  */
  b->text = b->base_buffer->text;

  BUF_BEGV (b) = BUF_BEGV (b->base_buffer);
  BUF_ZV (b) = BUF_ZV (b->base_buffer);
  BUF_PT (b) = BUF_PT (b->base_buffer);
  BUF_BEGV_BYTE (b) = BUF_BEGV_BYTE (b->base_buffer);
  BUF_ZV_BYTE (b) = BUF_ZV_BYTE (b->base_buffer);
  BUF_PT_BYTE (b) = BUF_PT_BYTE (b->base_buffer);

  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->width_table = Qnil;

  /* Put this on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;

  name = Fcopy_sequence (name);
  XSTRING (name)->intervals = NULL_INTERVAL;
  b->name = name;

  reset_buffer (b);
  reset_buffer_local_variables (b, 1);

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buf, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buf), Qnil));

  b->mark = Fmake_marker ();
  b->name = name;

  /* The multibyte status belongs to the base buffer.  */
  b->enable_multibyte_characters = b->base_buffer->enable_multibyte_characters;

  /* Make sure the base buffer has markers for its narrowing.  */
  if (NILP (b->base_buffer->pt_marker))
    {
      b->base_buffer->pt_marker = Fmake_marker ();
      set_marker_both (b->base_buffer->pt_marker, base_buffer,
		       BUF_PT (b->base_buffer),
		       BUF_PT_BYTE (b->base_buffer));
    }
  if (NILP (b->base_buffer->begv_marker))
    {
      b->base_buffer->begv_marker = Fmake_marker ();
      set_marker_both (b->base_buffer->begv_marker, base_buffer,
		       BUF_BEGV (b->base_buffer),
		       BUF_BEGV_BYTE (b->base_buffer));
    }
  if (NILP (b->base_buffer->zv_marker))
    {
      b->base_buffer->zv_marker = Fmake_marker ();
      set_marker_both (b->base_buffer->zv_marker, base_buffer,
		       BUF_ZV (b->base_buffer),
		       BUF_ZV_BYTE (b->base_buffer));
      XMARKER (b->base_buffer->zv_marker)->insertion_type = 1;
    }

  if (NILP (clone))
    {
      /* Give the indirect buffer markers for its narrowing.  */
      b->pt_marker = Fmake_marker ();
      set_marker_both (b->pt_marker, buf, BUF_PT (b), BUF_PT_BYTE (b));
      b->begv_marker = Fmake_marker ();
      set_marker_both (b->begv_marker, buf, BUF_BEGV (b), BUF_BEGV_BYTE (b));
      b->zv_marker = Fmake_marker ();
      set_marker_both (b->zv_marker, buf, BUF_ZV (b), BUF_ZV_BYTE (b));
      XMARKER (b->zv_marker)->insertion_type = 1;
    }
  else
    clone_per_buffer_values (b->base_buffer, b);

  return buf;
}

/* Reinitialize everything about a buffer except its name and contents
   and local variables.  */

void
reset_buffer (b)
     register struct buffer *b;
{
  b->filename = Qnil;
  b->file_truename = Qnil;
  b->directory = (current_buffer) ? current_buffer->directory : Qnil;
  b->modtime = 0;
  XSETFASTINT (b->save_length, 0);
  b->last_window_start = 1;
  /* It is more conservative to start out "changed" than "unchanged".  */
  b->clip_changed = 0;
  b->prevent_redisplay_optimizations_p = 1;
  b->backed_up = Qnil;
  b->auto_save_modified = 0;
  b->auto_save_failure_time = -1;
  b->auto_save_file_name = Qnil;
  b->read_only = Qnil;
  b->overlays_before = Qnil;
  b->overlays_after = Qnil;
  XSETFASTINT (b->overlay_center, 1);
  b->mark_active = Qnil;
  b->point_before_scroll = Qnil;
  b->file_format = Qnil;
  b->last_selected_window = Qnil;
  XSETINT (b->display_count, 0);
  b->display_time = Qnil;
  b->enable_multibyte_characters = buffer_defaults.enable_multibyte_characters;
  b->cursor_type = buffer_defaults.cursor_type;
  b->extra_line_spacing = buffer_defaults.extra_line_spacing;
}

/* Reset buffer B's local variables info.
   Don't use this on a buffer that has already been in use;
   it does not treat permanent locals consistently.
   Instead, use Fkill_all_local_variables.

   If PERMANENT_TOO is 1, then we reset permanent built-in
   buffer-local variables.  If PERMANENT_TOO is 0,
   we preserve those.  */

static void
reset_buffer_local_variables (b, permanent_too)
     register struct buffer *b;
     int permanent_too;
{
  register int offset;
  int i;

  /* Reset the major mode to Fundamental, together with all the
     things that depend on the major mode.
     default-major-mode is handled at a higher level.
     We ignore it here.  */
  b->major_mode = Qfundamental_mode;
  b->keymap = Qnil;
  b->abbrev_table = Vfundamental_mode_abbrev_table;
  b->mode_name = QSFundamental;
  b->minor_modes = Qnil;

  /* If the standard case table has been altered and invalidated,
     fix up its insides first.  */
  if (! (CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[0])
	 && CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[1])
	 && CHAR_TABLE_P (XCHAR_TABLE (Vascii_downcase_table)->extras[2])))
    Fset_standard_case_table (Vascii_downcase_table);

  b->downcase_table = Vascii_downcase_table;
  b->upcase_table = XCHAR_TABLE (Vascii_downcase_table)->extras[0];
  b->case_canon_table = XCHAR_TABLE (Vascii_downcase_table)->extras[1];
  b->case_eqv_table = XCHAR_TABLE (Vascii_downcase_table)->extras[2];
  b->invisibility_spec = Qt;
#ifndef DOS_NT
  b->buffer_file_type = Qnil;
#endif

#if 0
  b->sort_table = XSTRING (Vascii_sort_table);
  b->folding_sort_table = XSTRING (Vascii_folding_sort_table);
#endif /* 0 */

  /* Reset all (or most) per-buffer variables to their defaults.  */
  b->local_var_alist = Qnil;
  for (i = 0; i < last_per_buffer_idx; ++i)
    if (permanent_too || buffer_permanent_local_flags[i] == 0)
      SET_PER_BUFFER_VALUE_P (b, i, 0);

  /* For each slot that has a default value,
     copy that into the slot.  */

  for (offset = PER_BUFFER_VAR_OFFSET (name);
       offset < sizeof *b;
       offset += sizeof (Lisp_Object))
    {
      int idx = PER_BUFFER_IDX (offset);
      if ((idx > 0
	   && (permanent_too
	       || buffer_permanent_local_flags[idx] == 0))
	  /* Is -2 used anywhere?  */
	  || idx == -2)
	PER_BUFFER_VALUE (b, offset) = PER_BUFFER_DEFAULT (offset);
    }
}

/* We split this away from generate-new-buffer, because rename-buffer
   and set-visited-file-name ought to be able to use this to really
   rename the buffer properly.  */

DEFUN ("generate-new-buffer-name", Fgenerate_new_buffer_name, Sgenerate_new_buffer_name,
  1, 2, 0,
  "Return a string that is the name of no existing buffer based on NAME.\n\
If there is no live buffer named NAME, then return NAME.\n\
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER\n\
until an unused name is found, and then return that name.\n\
Optional second argument IGNORE specifies a name that is okay to use\n\
\(if it is in the sequence to be tried)\n\
even if a buffer with that name exists.")
 (name, ignore)
     register Lisp_Object name, ignore;
{
  register Lisp_Object gentemp, tem;
  int count;
  char number[10];

  CHECK_STRING (name, 0);

  tem = Fget_buffer (name);
  if (NILP (tem))
    return name;

  count = 1;
  while (1)
    {
      sprintf (number, "<%d>", ++count);
      gentemp = concat2 (name, build_string (number));
      tem = Fstring_equal (gentemp, ignore);
      if (!NILP (tem))
	return gentemp;
      tem = Fget_buffer (gentemp);
      if (NILP (tem))
	return gentemp;
    }
}


DEFUN ("buffer-name", Fbuffer_name, Sbuffer_name, 0, 1, 0,
  "Return the name of BUFFER, as a string.\n\
With no argument or nil as argument, return the name of the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  if (NILP (buffer))
    return current_buffer->name;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->name;
}

DEFUN ("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, 0, 1, 0,
  "Return name of file BUFFER is visiting, or nil if none.\n\
No argument or nil as argument means use the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  if (NILP (buffer))
    return current_buffer->filename;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->filename;
}

DEFUN ("buffer-base-buffer", Fbuffer_base_buffer, Sbuffer_base_buffer,
       0, 1, 0,
  "Return the base buffer of indirect buffer BUFFER.\n\
If BUFFER is not indirect, return nil.")
  (buffer)
     register Lisp_Object buffer;
{
  struct buffer *base;
  Lisp_Object base_buffer;

  if (NILP (buffer))
    base = current_buffer->base_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      base = XBUFFER (buffer)->base_buffer;
    }

  if (! base)
    return Qnil;
  XSETBUFFER (base_buffer, base);
  return base_buffer;
}

DEFUN ("buffer-local-variables", Fbuffer_local_variables,
  Sbuffer_local_variables, 0, 1, 0,
  "Return an alist of variables that are buffer-local in BUFFER.\n\
Most elements look like (SYMBOL . VALUE), describing one variable.\n\
For a symbol that is locally unbound, just the symbol appears in the value.\n\
Note that storing new VALUEs in these elements doesn't change the variables.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  register Lisp_Object result;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  result = Qnil;

  {
    register Lisp_Object tail;
    for (tail = buf->local_var_alist; CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object val, elt;

	elt = XCAR (tail);

	/* Reference each variable in the alist in buf.
	   If inquiring about the current buffer, this gets the current values,
	   so store them into the alist so the alist is up to date.
	   If inquiring about some other buffer, this swaps out any values
	   for that buffer, making the alist up to date automatically.  */
	val = find_symbol_value (XCAR (elt));
	/* Use the current buffer value only if buf is the current buffer.  */
	if (buf != current_buffer)
	  val = XCDR (elt);

	/* If symbol is unbound, put just the symbol in the list.  */
	if (EQ (val, Qunbound))
	  result = Fcons (XCAR (elt), result);
	/* Otherwise, put (symbol . value) in the list.  */
	else
	  result = Fcons (Fcons (XCAR (elt), val), result);
      }
  }

  /* Add on all the variables stored in special slots.  */
  {
    int offset, idx;

    for (offset = PER_BUFFER_VAR_OFFSET (name);
	 offset < sizeof (struct buffer);
	 /* sizeof EMACS_INT == sizeof Lisp_Object */
	 offset += (sizeof (EMACS_INT)))
      {
	idx = PER_BUFFER_IDX (offset);
	if ((idx == -1 || PER_BUFFER_VALUE_P (buf, idx))
	    && SYMBOLP (PER_BUFFER_SYMBOL (offset)))
	  result = Fcons (Fcons (PER_BUFFER_SYMBOL (offset),
				 PER_BUFFER_VALUE (buf, offset)),
			  result);
      }
  }

  return result;
}


DEFUN ("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p,
  0, 1, 0,
  "Return t if BUFFER was modified since its file was last read or saved.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  return BUF_SAVE_MODIFF (buf) < BUF_MODIFF (buf) ? Qt : Qnil;
}

DEFUN ("set-buffer-modified-p", Fset_buffer_modified_p, Sset_buffer_modified_p,
  1, 1, 0,
  "Mark current buffer as modified or unmodified according to FLAG.\n\
A non-nil FLAG means mark the buffer modified.")
  (flag)
     register Lisp_Object flag;
{
  register int already;
  register Lisp_Object fn;
  Lisp_Object buffer, window;

#ifdef CLASH_DETECTION
  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = current_buffer->file_truename;
  /* Test buffer-file-name so that binding it to nil is effective.  */
  if (!NILP (fn) && ! NILP (current_buffer->filename))
    {
      already = SAVE_MODIFF < MODIFF;
      if (!already && !NILP (flag))
	lock_file (fn);
      else if (already && NILP (flag))
	unlock_file (fn);
    }
#endif /* CLASH_DETECTION */

  SAVE_MODIFF = NILP (flag) ? MODIFF : 0;
  
  /* Set update_mode_lines only if buffer is displayed in some window.
     Packages like jit-lock or lazy-lock preserve a buffer's modified
     state by recording/restoring the state around blocks of code.
     Setting update_mode_lines makes redisplay consider all windows
     (on all frames).  Stealth fontification of buffers not displayed
     would incur additional redisplay costs if we'd set
     update_modes_lines unconditionally.

     Ideally, I think there should be another mechanism for fontifying
     buffers without "modifying" buffers, or redisplay should be
     smarter about updating the `*' in mode lines.  --gerd  */
  XSETBUFFER (buffer, current_buffer);
  window = Fget_buffer_window (buffer, Qt);
  if (WINDOWP (window))
    {
      ++update_mode_lines;
      current_buffer->prevent_redisplay_optimizations_p = 1;
    }
  
  return flag;
}

DEFUN ("restore-buffer-modified-p", Frestore_buffer_modified_p,
       Srestore_buffer_modified_p, 1, 1, 0,
   "Like `set-buffer-modified-p', with a differences concerning redisplay.\n\
It is not ensured that mode lines will be updated to show the modified\n\
state of the current buffer.  Use with care.")
  (flag)
     Lisp_Object flag;
{
#ifdef CLASH_DETECTION
  Lisp_Object fn;
  
  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = current_buffer->file_truename;
  /* Test buffer-file-name so that binding it to nil is effective.  */
  if (!NILP (fn) && ! NILP (current_buffer->filename))
    {
      int already = SAVE_MODIFF < MODIFF;
      if (!already && !NILP (flag))
	lock_file (fn);
      else if (already && NILP (flag))
	unlock_file (fn);
    }
#endif /* CLASH_DETECTION */
  
  SAVE_MODIFF = NILP (flag) ? MODIFF : 0;
  return flag;
}

DEFUN ("buffer-modified-tick", Fbuffer_modified_tick, Sbuffer_modified_tick,
  0, 1, 0,
  "Return BUFFER's tick counter, incremented for each change in text.\n\
Each buffer has a tick counter which is incremented each time the text in\n\
that buffer is changed.  It wraps around occasionally.\n\
No argument or nil as argument means use current buffer as BUFFER.")
  (buffer)
     register Lisp_Object buffer;
{
  register struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  return make_number (BUF_MODIFF (buf));
}

DEFUN ("rename-buffer", Frename_buffer, Srename_buffer, 1, 2,
       "sRename buffer (to new name): \nP",
  "Change current buffer's name to NEWNAME (a string).\n\
If second arg UNIQUE is nil or omitted, it is an error if a\n\
buffer named NEWNAME already exists.\n\
If UNIQUE is non-nil, come up with a new name using\n\
`generate-new-buffer-name'.\n\
Interactively, you can set UNIQUE with a prefix argument.\n\
We return the name we actually gave the buffer.\n\
This does not change the name of the visited file (if any).")
  (newname, unique)
     register Lisp_Object newname, unique;
{
  register Lisp_Object tem, buf;

  CHECK_STRING (newname, 0);

  if (XSTRING (newname)->size == 0)
    error ("Empty string is invalid as a buffer name");

  tem = Fget_buffer (newname);
  if (!NILP (tem))
    {
      /* Don't short-circuit if UNIQUE is t.  That is a useful way to
	 rename the buffer automatically so you can create another
	 with the original name.  It makes UNIQUE equivalent to
	 (rename-buffer (generate-new-buffer-name NEWNAME)).  */
      if (NILP (unique) && XBUFFER (tem) == current_buffer)
	return current_buffer->name;
      if (!NILP (unique))
	newname = Fgenerate_new_buffer_name (newname, current_buffer->name);
      else
	error ("Buffer name `%s' is in use", XSTRING (newname)->data);
    }

  current_buffer->name = newname;

  /* Catch redisplay's attention.  Unless we do this, the mode lines for
     any windows displaying current_buffer will stay unchanged.  */
  update_mode_lines++;

  XSETBUFFER (buf, current_buffer);
  Fsetcar (Frassq (buf, Vbuffer_alist), newname);
  if (NILP (current_buffer->filename)
      && !NILP (current_buffer->auto_save_file_name))
    call0 (intern ("rename-auto-save-file"));
  /* Refetch since that last call may have done GC.  */
  return current_buffer->name;
}

DEFUN ("other-buffer", Fother_buffer, Sother_buffer, 0, 3, 0,
  "Return most recently selected buffer other than BUFFER.\n\
Buffers not visible in windows are preferred to visible buffers,\n\
unless optional second argument VISIBLE-OK is non-nil.\n\
If the optional third argument FRAME is non-nil, use that frame's\n\
buffer list instead of the selected frame's buffer list.\n\
If no other buffer exists, the buffer `*scratch*' is returned.\n\
If BUFFER is omitted or nil, some interesting buffer is returned.")
  (buffer, visible_ok, frame)
     register Lisp_Object buffer, visible_ok, frame;
{
  Lisp_Object Fset_buffer_major_mode ();
  register Lisp_Object tail, buf, notsogood, tem, pred, add_ons;
  notsogood = Qnil;

  if (NILP (frame))
    frame = selected_frame;

  tail = Vbuffer_alist;
  pred = frame_buffer_predicate (frame);

  /* Consider buffers that have been seen in the selected frame
     before other buffers.  */
    
  tem = frame_buffer_list (frame);
  add_ons = Qnil;
  while (CONSP (tem))
    {
      if (BUFFERP (XCAR (tem)))
	add_ons = Fcons (Fcons (Qnil, XCAR (tem)), add_ons);
      tem = XCDR (tem);
    }
  tail = nconc2 (Fnreverse (add_ons), tail);

  for (; !NILP (tail); tail = Fcdr (tail))
    {
      buf = Fcdr (Fcar (tail));
      if (EQ (buf, buffer))
	continue;
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
      /* If the selected frame has a buffer_predicate,
	 disregard buffers that don't fit the predicate.  */
      if (!NILP (pred))
	{
	  tem = call1 (pred, buf);
	  if (NILP (tem))
	    continue;
	}

      if (NILP (visible_ok))
	tem = Fget_buffer_window (buf, Qvisible);
      else
	tem = Qnil;
      if (NILP (tem))
	return buf;
      if (NILP (notsogood))
	notsogood = buf;
    }
  if (!NILP (notsogood))
    return notsogood;
  buf = Fget_buffer (build_string ("*scratch*"));
  if (NILP (buf))
    {
      buf = Fget_buffer_create (build_string ("*scratch*"));
      Fset_buffer_major_mode (buf);
    }
  return buf;
}

DEFUN ("buffer-disable-undo", Fbuffer_disable_undo, Sbuffer_disable_undo,
       0, 1, "",
  "Make BUFFER stop keeping undo information.\n\
No argument or nil as argument means do this for the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  Lisp_Object real_buffer;

  if (NILP (buffer))
    XSETBUFFER (real_buffer, current_buffer);
  else
    {
      real_buffer = Fget_buffer (buffer);
      if (NILP (real_buffer))
	nsberror (buffer);
    }

  XBUFFER (real_buffer)->undo_list = Qt;

  return Qnil;
}

DEFUN ("buffer-enable-undo", Fbuffer_enable_undo, Sbuffer_enable_undo,
       0, 1, "",
  "Start keeping undo information for buffer BUFFER.\n\
No argument or nil as argument means do this for the current buffer.")
  (buffer)
     register Lisp_Object buffer;
{
  Lisp_Object real_buffer;

  if (NILP (buffer))
    XSETBUFFER (real_buffer, current_buffer);
  else
    {
      real_buffer = Fget_buffer (buffer);
      if (NILP (real_buffer))
	nsberror (buffer);
    }

  if (EQ (XBUFFER (real_buffer)->undo_list, Qt))
    XBUFFER (real_buffer)->undo_list = Qnil;

  return Qnil;
}

/*
  DEFVAR_LISP ("kill-buffer-hook", no_cell, "\
Hook to be run (by `run-hooks', which see) when a buffer is killed.\n\
The buffer being killed will be current while the hook is running.\n\
See `kill-buffer'."
 */
DEFUN ("kill-buffer", Fkill_buffer, Skill_buffer, 1, 1, "bKill buffer: ",
  "Kill the buffer BUFFER.\n\
The argument may be a buffer or may be the name of a buffer.\n\
An argument of nil means kill the current buffer.\n\n\
Value is t if the buffer is actually killed, nil if user says no.\n\n\
The value of `kill-buffer-hook' (which may be local to that buffer),\n\
if not void, is a list of functions to be called, with no arguments,\n\
before the buffer is actually killed.  The buffer to be killed is current\n\
when the hook functions are called.\n\n\
Any processes that have this buffer as the `process-buffer' are killed\n\
with SIGHUP.")
  (buffer)
     Lisp_Object buffer;
{
  Lisp_Object buf;
  register struct buffer *b;
  register Lisp_Object tem;
  register struct Lisp_Marker *m;
  struct gcpro gcpro1;

  if (NILP (buffer))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (buffer);
  if (NILP (buf))
    nsberror (buffer);

  b = XBUFFER (buf);

  /* Avoid trouble for buffer already dead.  */
  if (NILP (b->name))
    return Qnil;

  /* Query if the buffer is still modified.  */
  if (INTERACTIVE && !NILP (b->filename)
      && BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
    {
      GCPRO1 (buf);
      tem = do_yes_or_no_p (format1 ("Buffer %s modified; kill anyway? ",
				     XSTRING (b->name)->data));
      UNGCPRO;
      if (NILP (tem))
	return Qnil;
    }

  /* Run hooks with the buffer to be killed the current buffer.  */
  {
    int count = specpdl_ptr - specpdl;
    Lisp_Object list;

    record_unwind_protect (save_excursion_restore, save_excursion_save ());
    set_buffer_internal (b);

    /* First run the query functions; if any query is answered no,
       don't kill the buffer.  */
    for (list = Vkill_buffer_query_functions; !NILP (list); list = Fcdr (list))
      {
	tem = call0 (Fcar (list));
	if (NILP (tem))
	  return unbind_to (count, Qnil);
      }

    /* Then run the hooks.  */
    if (!NILP (Vrun_hooks))
      call1 (Vrun_hooks, Qkill_buffer_hook);
    unbind_to (count, Qnil);
  }

  /* We have no more questions to ask.  Verify that it is valid
     to kill the buffer.  This must be done after the questions
     since anything can happen within do_yes_or_no_p.  */

  /* Don't kill the minibuffer now current.  */
  if (EQ (buf, XWINDOW (minibuf_window)->buffer))
    return Qnil;

  if (NILP (b->name))
    return Qnil;

  /* When we kill a base buffer, kill all its indirect buffers.
     We do it at this stage so nothing terrible happens if they
     ask questions or their hooks get errors.  */
  if (! b->base_buffer)
    {
      struct buffer *other;

      GCPRO1 (buf);

      for (other = all_buffers; other; other = other->next)
	/* all_buffers contains dead buffers too;
	   don't re-kill them.  */
	if (other->base_buffer == b && !NILP (other->name))
	  {
	    Lisp_Object buf;
	    XSETBUFFER (buf, other);
	    Fkill_buffer (buf);
	  }

      UNGCPRO;
    }
  
  /* Make this buffer not be current.
     In the process, notice if this is the sole visible buffer
     and give up if so.  */
  if (b == current_buffer)
    {
      tem = Fother_buffer (buf, Qnil, Qnil);
      Fset_buffer (tem);
      if (b == current_buffer)
	return Qnil;
    }

  /* Notice if the buffer to kill is the sole visible buffer
     when we're currently in the mini-buffer, and give up if so.  */
  XSETBUFFER (tem, current_buffer);
  if (EQ (tem, XWINDOW (minibuf_window)->buffer))
    {
      tem = Fother_buffer (buf, Qnil, Qnil);
      if (EQ (buf, tem))
	return Qnil;
    }

  /* Now there is no question: we can kill the buffer.  */

#ifdef CLASH_DETECTION
  /* Unlock this buffer's file, if it is locked.  */
  unlock_buffer (b);
#endif /* CLASH_DETECTION */

  kill_buffer_processes (buf);

  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;
  replace_buffer_in_all_windows (buf);
  Vbuffer_alist = Fdelq (Frassq (buf, Vbuffer_alist), Vbuffer_alist);
  frames_discard_buffer (buf);
  Vinhibit_quit = tem;

  /* Delete any auto-save file, if we saved it in this session.
     But not if the buffer is modified.  */
  if (STRINGP (b->auto_save_file_name)
      && b->auto_save_modified != 0
      && BUF_SAVE_MODIFF (b) < b->auto_save_modified
      && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b))
    {
      Lisp_Object tem;
      tem = Fsymbol_value (intern ("delete-auto-save-files"));
      if (! NILP (tem))
	internal_delete_file (b->auto_save_file_name);
    }

  if (b->base_buffer)
    {
      /* Unchain all markers that belong to this indirect buffer.
	 Don't unchain the markers that belong to the base buffer
	 or its other indirect buffers.  */
      for (tem = BUF_MARKERS (b); !NILP (tem); )
	{
	  Lisp_Object next;
	  m = XMARKER (tem);
	  next = m->chain;
	  if (m->buffer == b)
	    unchain_marker (tem);
	  tem = next;
	}
    }
  else
    {
      /* Unchain all markers of this buffer and its indirect buffers.
	 and leave them pointing nowhere.  */
      for (tem = BUF_MARKERS (b); !NILP (tem); )
	{
	  m = XMARKER (tem);
	  m->buffer = 0;
	  tem = m->chain;
	  m->chain = Qnil;
	}
      BUF_MARKERS (b) = Qnil;
      BUF_INTERVALS (b) = NULL_INTERVAL;

      /* Perhaps we should explicitly free the interval tree here... */
    }

  /* Reset the local variables, so that this buffer's local values
     won't be protected from GC.  They would be protected
     if they happened to remain encached in their symbols.
     This gets rid of them for certain.  */
  swap_out_buffer_local_variables (b);
  reset_buffer_local_variables (b, 1);

  b->name = Qnil;

  BLOCK_INPUT;
  if (! b->base_buffer)
    free_buffer_text (b);

  if (b->newline_cache)
    {
      free_region_cache (b->newline_cache);
      b->newline_cache = 0;
    }
  if (b->width_run_cache)
    {
      free_region_cache (b->width_run_cache);
      b->width_run_cache = 0;
    }
  b->width_table = Qnil;
  UNBLOCK_INPUT;
  b->undo_list = Qnil;

  return Qt;
}

/* Move the assoc for buffer BUF to the front of buffer-alist.  Since
   we do this each time BUF is selected visibly, the more recently
   selected buffers are always closer to the front of the list.  This
   means that other_buffer is more likely to choose a relevant buffer.  */

void
record_buffer (buf)
     Lisp_Object buf;
{
  register Lisp_Object link, prev;
  Lisp_Object frame;
  frame = selected_frame;

  prev = Qnil;
  for (link = Vbuffer_alist; CONSP (link); link = XCDR (link))
    {
      if (EQ (XCDR (XCAR (link)), buf))
	break;
      prev = link;
    }

  /* Effectively do Vbuffer_alist = Fdelq (link, Vbuffer_alist);
     we cannot use Fdelq itself here because it allows quitting.  */

  if (NILP (prev))
    Vbuffer_alist = XCDR (Vbuffer_alist);
  else
    XCDR (prev) = XCDR (XCDR (prev));
	
  XCDR (link) = Vbuffer_alist;
  Vbuffer_alist = link;

  /* Now move this buffer to the front of frame_buffer_list also.  */

  prev = Qnil;
  for (link = frame_buffer_list (frame); CONSP (link);
       link = XCDR (link))
    {
      if (EQ (XCAR (link), buf))
	break;
      prev = link;
    }

  /* Effectively do delq.  */

  if (CONSP (link))
    {
      if (NILP (prev))
	set_frame_buffer_list (frame,
			       XCDR (frame_buffer_list (frame)));
      else
	XCDR (prev) = XCDR (XCDR (prev));
	
      XCDR (link) = frame_buffer_list (frame);
      set_frame_buffer_list (frame, link);
    }
  else
    set_frame_buffer_list (frame, Fcons (buf, frame_buffer_list (frame)));
}

DEFUN ("set-buffer-major-mode", Fset_buffer_major_mode, Sset_buffer_major_mode, 1, 1, 0,
  "Set an appropriate major mode for BUFFER.\n\
For the *scratch* buffer use `initial-major-mode', otherwise choose a\n\
mode according to `default-major-mode'.\n\
Use this function before selecting the buffer, since it may need to inspect\n\
the current buffer's major mode.")
  (buffer)
     Lisp_Object buffer;
{
  int count;
  Lisp_Object function;

  if (STRINGP (XBUFFER (buffer)->name)
      && strcmp (XSTRING (XBUFFER (buffer)->name)->data, "*scratch*") == 0)
    function = find_symbol_value (intern ("initial-major-mode"));
  else
    {
      function = buffer_defaults.major_mode;
      if (NILP (function)
	  && NILP (Fget (current_buffer->major_mode, Qmode_class)))
	function = current_buffer->major_mode;
    }
  
  if (NILP (function) || EQ (function, Qfundamental_mode))
    return Qnil;

  count = specpdl_ptr - specpdl;

  /* To select a nonfundamental mode,
     select the buffer temporarily and then call the mode function. */

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  Fset_buffer (buffer);
  call0 (function);

  return unbind_to (count, Qnil);
}

/* If switching buffers in WINDOW would be an error, return
   a C string saying what the error would be.  */

char *
no_switch_window (window)
     Lisp_Object window;
{
  Lisp_Object tem;
  if (EQ (minibuf_window, window))
    return "Cannot switch buffers in minibuffer window";
  tem = Fwindow_dedicated_p (window);
  if (!NILP (tem))
    return "Cannot switch buffers in a dedicated window";
  return NULL;
}

/* Switch to buffer BUFFER in the selected window.
   If NORECORD is non-nil, don't call record_buffer.  */

Lisp_Object
switch_to_buffer_1 (buffer, norecord)
     Lisp_Object buffer, norecord;
{
  register Lisp_Object buf;

  if (NILP (buffer))
    buf = Fother_buffer (Fcurrent_buffer (), Qnil, Qnil);
  else
    {
      buf = Fget_buffer (buffer);
      if (NILP (buf))
	{
	  buf = Fget_buffer_create (buffer);
	  Fset_buffer_major_mode (buf);
	}
    }
  Fset_buffer (buf);
  if (NILP (norecord))
    record_buffer (buf);

  Fset_window_buffer (EQ (selected_window, minibuf_window)
		      ? Fnext_window (minibuf_window, Qnil, Qnil)
		      : selected_window,
		      buf);

  return buf;
}

DEFUN ("switch-to-buffer", Fswitch_to_buffer, Sswitch_to_buffer, 1, 2, "BSwitch to buffer: ",
  "Select buffer BUFFER in the current window.\n\
BUFFER may be a buffer or a buffer name.\n\
Optional second arg NORECORD non-nil means\n\
do not put this buffer at the front of the list of recently selected ones.\n\
\n\
WARNING: This is NOT the way to work on another buffer temporarily\n\
within a Lisp program!  Use `set-buffer' instead.  That avoids messing with\n\
the window-buffer correspondences.")
  (buffer, norecord)
     Lisp_Object buffer, norecord;
{
  char *err;

  err = no_switch_window (selected_window);
  if (err) error (err);

  return switch_to_buffer_1 (buffer, norecord);
}

DEFUN ("pop-to-buffer", Fpop_to_buffer, Spop_to_buffer, 1, 3, 0,
  "Select buffer BUFFER in some window, preferably a different one.\n\
If BUFFER is nil, then some other buffer is chosen.\n\
If `pop-up-windows' is non-nil, windows can be split to do this.\n\
If optional second arg OTHER-WINDOW is non-nil, insist on finding another\n\
window even if BUFFER is already visible in the selected window.\n\
This uses the function `display-buffer' as a subroutine; see the documentation\n\
of `display-buffer' for additional customization information.\n\
\n\
Optional third arg NORECORD non-nil means\n\
do not put this buffer at the front of the list of recently selected ones.")
  (buffer, other_window, norecord)
     Lisp_Object buffer, other_window, norecord;
{
  register Lisp_Object buf;
  if (NILP (buffer))
    buf = Fother_buffer (Fcurrent_buffer (), Qnil, Qnil);
  else
    {
      buf = Fget_buffer (buffer);
      if (NILP (buf))
	{
	  buf = Fget_buffer_create (buffer);
	  Fset_buffer_major_mode (buf);
	}
    }
  Fset_buffer (buf);
  if (NILP (norecord))
    record_buffer (buf);
  Fselect_window (Fdisplay_buffer (buf, other_window, Qnil));
  return buf;
}

DEFUN ("current-buffer", Fcurrent_buffer, Scurrent_buffer, 0, 0, 0,
  "Return the current buffer as a Lisp object.")
  ()
{
  register Lisp_Object buf;
  XSETBUFFER (buf, current_buffer);
  return buf;
}

/* Set the current buffer to B.

   We previously set windows_or_buffers_changed here to invalidate
   global unchanged information in beg_unchanged and end_unchanged.
   This is no longer necessary because we now compute unchanged
   information on a buffer-basis.  Every action affecting other
   windows than the selected one requires a select_window at some
   time, and that increments windows_or_buffers_changed.  */

void
set_buffer_internal (b)
     register struct buffer *b;
{
  if (current_buffer != b)
    set_buffer_internal_1 (b);
}

/* Set the current buffer to B, and do not set windows_or_buffers_changed.
   This is used by redisplay.  */

void
set_buffer_internal_1 (b)
     register struct buffer *b;
{
  register struct buffer *old_buf;
  register Lisp_Object tail, valcontents;
  Lisp_Object tem;

#ifdef USE_MMAP_FOR_BUFFERS
  if (b->text->beg == NULL)
    enlarge_buffer_text (b, 0);
#endif /* USE_MMAP_FOR_BUFFERS */
  
  if (current_buffer == b)
    return;

  old_buf = current_buffer;
  current_buffer = b;
  last_known_column_point = -1;   /* invalidate indentation cache */

  if (old_buf)
    {
      /* Put the undo list back in the base buffer, so that it appears
	 that an indirect buffer shares the undo list of its base.  */
      if (old_buf->base_buffer)
	old_buf->base_buffer->undo_list = old_buf->undo_list;

      /* If the old current buffer has markers to record PT, BEGV and ZV
	 when it is not current, update them now.  */
      if (! NILP (old_buf->pt_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  set_marker_both (old_buf->pt_marker, obuf,
			   BUF_PT (old_buf), BUF_PT_BYTE (old_buf));
	}
      if (! NILP (old_buf->begv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  set_marker_both (old_buf->begv_marker, obuf,
			   BUF_BEGV (old_buf), BUF_BEGV_BYTE (old_buf));
	}
      if (! NILP (old_buf->zv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  set_marker_both (old_buf->zv_marker, obuf,
			   BUF_ZV (old_buf), BUF_ZV_BYTE (old_buf));
	}
    }

  /* Get the undo list from the base buffer, so that it appears
     that an indirect buffer shares the undo list of its base.  */
  if (b->base_buffer)
    b->undo_list = b->base_buffer->undo_list;

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  if (! NILP (b->pt_marker))
    {
      BUF_PT (b) = marker_position (b->pt_marker);
      BUF_PT_BYTE (b) = marker_byte_position (b->pt_marker);
    }
  if (! NILP (b->begv_marker))
    {
      BUF_BEGV (b) = marker_position (b->begv_marker);
      BUF_BEGV_BYTE (b) = marker_byte_position (b->begv_marker);
    }
  if (! NILP (b->zv_marker))
    {
      BUF_ZV (b) = marker_position (b->zv_marker);
      BUF_ZV_BYTE (b) = marker_byte_position (b->zv_marker);
    }

  /* Look down buffer's list of local Lisp variables
     to find and update any that forward into C variables. */

  for (tail = b->local_var_alist; !NILP (tail); tail = XCDR (tail))
    {
      valcontents = XSYMBOL (XCAR (XCAR (tail)))->value;
      if ((BUFFER_LOCAL_VALUEP (valcontents)
	   || SOME_BUFFER_LOCAL_VALUEP (valcontents))
	  && (tem = XBUFFER_LOCAL_VALUE (valcontents)->realvalue,
	      (BOOLFWDP (tem) || INTFWDP (tem) || OBJFWDP (tem))))
	/* Just reference the variable
	     to cause it to become set for this buffer.  */
	Fsymbol_value (XCAR (XCAR (tail)));
    }

  /* Do the same with any others that were local to the previous buffer */

  if (old_buf)
    for (tail = old_buf->local_var_alist; !NILP (tail); tail = XCDR (tail))
      {
	valcontents = XSYMBOL (XCAR (XCAR (tail)))->value;
	if ((BUFFER_LOCAL_VALUEP (valcontents)
	     || SOME_BUFFER_LOCAL_VALUEP (valcontents))
	    && (tem = XBUFFER_LOCAL_VALUE (valcontents)->realvalue,
		(BOOLFWDP (tem) || INTFWDP (tem) || OBJFWDP (tem))))
	  /* Just reference the variable
               to cause it to become set for this buffer.  */
	  Fsymbol_value (XCAR (XCAR (tail)));
      }
}

/* Switch to buffer B temporarily for redisplay purposes.
   This avoids certain things that don't need to be done within redisplay.  */

void
set_buffer_temp (b)
     struct buffer *b;
{
  register struct buffer *old_buf;

  if (current_buffer == b)
    return;

  old_buf = current_buffer;
  current_buffer = b;

  if (old_buf)
    {
      /* If the old current buffer has markers to record PT, BEGV and ZV
	 when it is not current, update them now.  */
      if (! NILP (old_buf->pt_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  set_marker_both (old_buf->pt_marker, obuf,
			   BUF_PT (old_buf), BUF_PT_BYTE (old_buf));
	}
      if (! NILP (old_buf->begv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  set_marker_both (old_buf->begv_marker, obuf,
			   BUF_BEGV (old_buf), BUF_BEGV_BYTE (old_buf));
	}
      if (! NILP (old_buf->zv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  set_marker_both (old_buf->zv_marker, obuf,
			   BUF_ZV (old_buf), BUF_ZV_BYTE (old_buf));
	}
    }

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  if (! NILP (b->pt_marker))
    {
      BUF_PT (b) = marker_position (b->pt_marker);
      BUF_PT_BYTE (b) = marker_byte_position (b->pt_marker);
    }
  if (! NILP (b->begv_marker))
    {
      BUF_BEGV (b) = marker_position (b->begv_marker);
      BUF_BEGV_BYTE (b) = marker_byte_position (b->begv_marker);
    }
  if (! NILP (b->zv_marker))
    {
      BUF_ZV (b) = marker_position (b->zv_marker);
      BUF_ZV_BYTE (b) = marker_byte_position (b->zv_marker);
    }
}

DEFUN ("set-buffer", Fset_buffer, Sset_buffer, 1, 1, 0,
  "Make the buffer BUFFER current for editing operations.\n\
BUFFER may be a buffer or the name of an existing buffer.\n\
See also `save-excursion' when you want to make a buffer current temporarily.\n\
This function does not display the buffer, so its effect ends\n\
when the current command terminates.\n\
Use `switch-to-buffer' or `pop-to-buffer' to switch buffers permanently.")
  (buffer)
     register Lisp_Object buffer;
{
  register Lisp_Object buf;
  buf = Fget_buffer (buffer);
  if (NILP (buf))
    nsberror (buffer);
  if (NILP (XBUFFER (buf)->name))
    error ("Selecting deleted buffer");
  set_buffer_internal (XBUFFER (buf));
  return buf;
}

/* Set the current buffer to BUFFER provided it is alive.  */

Lisp_Object
set_buffer_if_live (buffer)
     Lisp_Object buffer;
{
  if (! NILP (XBUFFER (buffer)->name))
    Fset_buffer (buffer);
  return Qnil;
}

DEFUN ("barf-if-buffer-read-only", Fbarf_if_buffer_read_only,
				   Sbarf_if_buffer_read_only, 0, 0, 0,
  "Signal a `buffer-read-only' error if the current buffer is read-only.")
  ()
{
  if (!NILP (current_buffer->read_only)
      && NILP (Vinhibit_read_only))
    Fsignal (Qbuffer_read_only, (Fcons (Fcurrent_buffer (), Qnil)));
  return Qnil;
}

DEFUN ("bury-buffer", Fbury_buffer, Sbury_buffer, 0, 1, "",
  "Put BUFFER at the end of the list of all buffers.\n\
There it is the least likely candidate for `other-buffer' to return;\n\
thus, the least likely buffer for \\[switch-to-buffer] to select by default.\n\
If BUFFER is nil or omitted, bury the current buffer.\n\
Also, if BUFFER is nil or omitted, remove the current buffer from the\n\
selected window if it is displayed there.")
  (buffer)
     register Lisp_Object buffer;
{
  /* Figure out what buffer we're going to bury.  */
  if (NILP (buffer))
    {
      XSETBUFFER (buffer, current_buffer);

      /* If we're burying the current buffer, unshow it.  */
      Fswitch_to_buffer (Fother_buffer (buffer, Qnil, Qnil), Qnil);
    }
  else
    {
      Lisp_Object buf1;
      
      buf1 = Fget_buffer (buffer);
      if (NILP (buf1))
	nsberror (buffer);
      buffer = buf1;
    }

  /* Move buffer to the end of the buffer list.  Do nothing if the
     buffer is killed.  */
  if (!NILP (XBUFFER (buffer)->name))
    {
      Lisp_Object aelt, link;

      aelt = Frassq (buffer, Vbuffer_alist);
      link = Fmemq (aelt, Vbuffer_alist);
      Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
      XCDR (link) = Qnil;
      Vbuffer_alist = nconc2 (Vbuffer_alist, link);

      frames_bury_buffer (buffer);
    }

  return Qnil;
}

DEFUN ("erase-buffer", Ferase_buffer, Serase_buffer, 0, 0, "*",
  "Delete the entire contents of the current buffer.\n\
Any narrowing restriction in effect (see `narrow-to-region') is removed,\n\
so the buffer is truly empty after this.")
  ()
{
  Fwiden ();

  del_range (BEG, Z);

  current_buffer->last_window_start = 1;
  /* Prevent warnings, or suspension of auto saving, that would happen
     if future size is less than past size.  Use of erase-buffer
     implies that the future text is not really related to the past text.  */
  XSETFASTINT (current_buffer->save_length, 0);
  return Qnil;
}

void
validate_region (b, e)
     register Lisp_Object *b, *e;
{
  CHECK_NUMBER_COERCE_MARKER (*b, 0);
  CHECK_NUMBER_COERCE_MARKER (*e, 1);

  if (XINT (*b) > XINT (*e))
    {
      Lisp_Object tem;
      tem = *b;  *b = *e;  *e = tem;
    }

  if (!(BEGV <= XINT (*b) && XINT (*b) <= XINT (*e)
        && XINT (*e) <= ZV))
    args_out_of_range (*b, *e);
}

/* Advance BYTE_POS up to a character boundary
   and return the adjusted position.  */

static int
advance_to_char_boundary (byte_pos)
     int byte_pos;
{
  int c;

  if (byte_pos == BEG)
    /* Beginning of buffer is always a character boundary.  */
    return 1;

  c = FETCH_BYTE (byte_pos);
  if (! CHAR_HEAD_P (c))
    {
      /* We should advance BYTE_POS only when C is a constituent of a
         multibyte sequence.  */
      int orig_byte_pos = byte_pos;

      do
	{
	  byte_pos--;
	  c = FETCH_BYTE (byte_pos);
	}
      while (! CHAR_HEAD_P (c) && byte_pos > BEG);
      INC_POS (byte_pos);
      if (byte_pos < orig_byte_pos)
	byte_pos = orig_byte_pos;
      /* If C is a constituent of a multibyte sequence, BYTE_POS was
         surely advance to the correct character boundary.  If C is
         not, BYTE_POS was unchanged.  */
    }

  return byte_pos;
}

DEFUN ("set-buffer-multibyte", Fset_buffer_multibyte, Sset_buffer_multibyte,
       1, 1, 0,
  "Set the multibyte flag of the current buffer to FLAG.\n\
If FLAG is t, this makes the buffer a multibyte buffer.\n\
If FLAG is nil, this makes the buffer a single-byte buffer.\n\
The buffer contents remain unchanged as a sequence of bytes\n\
but the contents viewed as characters do change.")
  (flag)
     Lisp_Object flag;
{
  Lisp_Object tail, markers;
  struct buffer *other;
  int undo_enabled_p = !EQ (current_buffer->undo_list, Qt);
  int begv = BEGV, zv = ZV;
  int narrowed = (BEG != begv || Z != zv);
  int modified_p = !NILP (Fbuffer_modified_p (Qnil));

  if (current_buffer->base_buffer)
    error ("Cannot do `set-buffer-multibyte' on an indirect buffer");

  /* Do nothing if nothing actually changes.  */
  if (NILP (flag) == NILP (current_buffer->enable_multibyte_characters))
    return flag;

  /* It would be better to update the list,
     but this is good enough for now.  */
  if (undo_enabled_p)
    current_buffer->undo_list = Qt;

  /* If the cached position is for this buffer, clear it out.  */
  clear_charpos_cache (current_buffer);

  if (narrowed)
    Fwiden ();

  if (NILP (flag))
    {
      int pos, stop;
      unsigned char *p;

      /* Do this first, so it can use CHAR_TO_BYTE
	 to calculate the old correspondences.  */
      set_intervals_multibyte (0);

      current_buffer->enable_multibyte_characters = Qnil;

      Z = Z_BYTE;
      BEGV = BEGV_BYTE;
      ZV = ZV_BYTE;
      GPT = GPT_BYTE;
      TEMP_SET_PT_BOTH (PT_BYTE, PT_BYTE);

      tail = BUF_MARKERS (current_buffer);
      while (! NILP (tail))
	{
	  XMARKER (tail)->charpos = XMARKER (tail)->bytepos;
	  tail = XMARKER (tail)->chain;
	}

      /* Convert multibyte form of 8-bit characters to unibyte.  */
      pos = BEG;
      stop = GPT;
      p = BEG_ADDR;
      while (1)
	{
	  int c, bytes;

	  if (pos == stop)
	    {
	      if (pos == Z)
		break;
	      p = GAP_END_ADDR;
	      stop = Z;
	    }
	  if (MULTIBYTE_STR_AS_UNIBYTE_P (p, bytes))
	    p += bytes, pos += bytes;
	  else
	    {
	      c = STRING_CHAR (p, stop - pos);
	      /* Delete all bytes for this 8-bit character but the
		 last one, and change the last one to the charcter
		 code.  */
	      bytes--;
	      del_range_2 (pos, pos, pos + bytes, pos + bytes, 0);
	      p = GAP_END_ADDR;
	      *p++ = c;
	      pos++;
	      if (begv > pos)
		begv -= bytes;
	      if (zv > pos)
		zv -= bytes;
	      stop = Z;
	    }
	}
      if (narrowed)
	Fnarrow_to_region (make_number (begv), make_number (zv));
    }
  else
    {
      int pt = PT;
      int pos, stop;
      unsigned char *p;

      /* Be sure not to have a multibyte sequence striding over the GAP.
	 Ex: We change this: "...abc\201 _GAP_ \241def..."
	     to: "...abc _GAP_ \201\241def..."  */

      if (GPT_BYTE > 1 && GPT_BYTE < Z_BYTE
	  && ! CHAR_HEAD_P (*(GAP_END_ADDR)))
	{
	  unsigned char *p = GPT_ADDR - 1;

	  while (! CHAR_HEAD_P (*p) && p > BEG_ADDR) p--;
	  if (BASE_LEADING_CODE_P (*p))
	    {
	      int new_gpt = GPT_BYTE - (GPT_ADDR - p);

	      move_gap_both (new_gpt, new_gpt);
	    }
	}

      /* Make the buffer contents valid as multibyte by converting
	 8-bit characters to multibyte form.  */
      pos = BEG;
      stop = GPT;
      p = BEG_ADDR;
      while (1)
	{
	  int bytes;

	  if (pos == stop)
	    {
	      if (pos == Z)
		break;
	      p = GAP_END_ADDR;
	      stop = Z;
	    }
	      
	  if (UNIBYTE_STR_AS_MULTIBYTE_P (p, stop - pos, bytes))
	    p += bytes, pos += bytes;
	  else
	    {
	      unsigned char tmp[MAX_MULTIBYTE_LENGTH];

	      bytes = CHAR_STRING (*p, tmp);
	      *p = tmp[0];
	      TEMP_SET_PT_BOTH (pos + 1, pos + 1);
	      bytes--;
	      insert_1_both (tmp + 1, bytes, bytes, 1, 0, 0);
	      /* Now the gap is after the just inserted data.  */
	      pos = GPT;
	      p = GAP_END_ADDR;
	      if (pos <= begv)
		begv += bytes;
	      if (pos <= zv)
		zv += bytes;
	      if (pos <= pt)
		pt += bytes;
	      stop = Z;
	    }
	}

      if (pt != PT)
	TEMP_SET_PT (pt);

      if (narrowed)
	Fnarrow_to_region (make_number (begv), make_number (zv));

      /* Do this first, so that chars_in_text asks the right question.
	 set_intervals_multibyte needs it too.  */
      current_buffer->enable_multibyte_characters = Qt;

      GPT_BYTE = advance_to_char_boundary (GPT_BYTE);
      GPT = chars_in_text (BEG_ADDR, GPT_BYTE - BEG_BYTE) + BEG;

      Z = chars_in_text (GAP_END_ADDR, Z_BYTE - GPT_BYTE) + GPT;

      BEGV_BYTE = advance_to_char_boundary (BEGV_BYTE);
      if (BEGV_BYTE > GPT_BYTE)
	BEGV = chars_in_text (GAP_END_ADDR, BEGV_BYTE - GPT_BYTE) + GPT;
      else
	BEGV = chars_in_text (BEG_ADDR, BEGV_BYTE - BEG_BYTE) + BEG;

      ZV_BYTE = advance_to_char_boundary (ZV_BYTE);
      if (ZV_BYTE > GPT_BYTE)
	ZV = chars_in_text (GAP_END_ADDR, ZV_BYTE - GPT_BYTE) + GPT;
      else
	ZV = chars_in_text (BEG_ADDR, ZV_BYTE - BEG_BYTE) + BEG;

      {
	int pt_byte = advance_to_char_boundary (PT_BYTE);
	int pt;

	if (pt_byte > GPT_BYTE)
	  pt = chars_in_text (GAP_END_ADDR, pt_byte - GPT_BYTE) + GPT;
	else
	  pt = chars_in_text (BEG_ADDR, pt_byte - BEG_BYTE) + BEG;
	TEMP_SET_PT_BOTH (pt, pt_byte);
      }

      tail = markers = BUF_MARKERS (current_buffer);

      /* This prevents BYTE_TO_CHAR (that is, buf_bytepos_to_charpos) from
	 getting confused by the markers that have not yet been updated.
	 It is also a signal that it should never create a marker.  */
      BUF_MARKERS (current_buffer) = Qnil;

      while (! NILP (tail))
	{
	  XMARKER (tail)->bytepos
	    = advance_to_char_boundary (XMARKER (tail)->bytepos);
	  XMARKER (tail)->charpos = BYTE_TO_CHAR (XMARKER (tail)->bytepos);

	  tail = XMARKER (tail)->chain;
	}

      /* Make sure no markers were put on the chain
	 while the chain value was incorrect.  */
      if (! EQ (BUF_MARKERS (current_buffer), Qnil))
	abort ();

      BUF_MARKERS (current_buffer) = markers;

      /* Do this last, so it can calculate the new correspondences
	 between chars and bytes.  */
      set_intervals_multibyte (1);
    }

  if (undo_enabled_p)
    current_buffer->undo_list = Qnil;

  /* Changing the multibyteness of a buffer means that all windows
     showing that buffer must be updated thoroughly.  */
  current_buffer->prevent_redisplay_optimizations_p = 1;
  ++windows_or_buffers_changed;

  /* Copy this buffer's new multibyte status
     into all of its indirect buffers.  */
  for (other = all_buffers; other; other = other->next)
    if (other->base_buffer == current_buffer && !NILP (other->name))
      {
	other->enable_multibyte_characters
	  = current_buffer->enable_multibyte_characters;
	other->prevent_redisplay_optimizations_p = 1;
      }

  /* Restore the modifiedness of the buffer.  */
  if (!modified_p && !NILP (Fbuffer_modified_p (Qnil)))
    Fset_buffer_modified_p (Qnil);

  return flag;
}

DEFUN ("kill-all-local-variables", Fkill_all_local_variables, Skill_all_local_variables,
  0, 0, 0,
  "Switch to Fundamental mode by killing current buffer's local variables.\n\
Most local variable bindings are eliminated so that the default values\n\
become effective once more.  Also, the syntax table is set from\n\
`standard-syntax-table', the local keymap is set to nil,\n\
and the abbrev table from `fundamental-mode-abbrev-table'.\n\
This function also forces redisplay of the mode line.\n\
\n\
Every function to select a new major mode starts by\n\
calling this function.\n\n\
As a special exception, local variables whose names have\n\
a non-nil `permanent-local' property are not eliminated by this function.\n\
\n\
The first thing this function does is run\n\
the normal hook `change-major-mode-hook'.")
  ()
{
  register Lisp_Object alist, sym, tem;
  Lisp_Object oalist;

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("change-major-mode-hook"));
  oalist = current_buffer->local_var_alist;

  /* Make sure none of the bindings in oalist
     remain swapped in, in their symbols.  */

  swap_out_buffer_local_variables (current_buffer);

  /* Actually eliminate all local bindings of this buffer.  */

  reset_buffer_local_variables (current_buffer, 0);

  /* Redisplay mode lines; we are changing major mode.  */

  update_mode_lines++;

  /* Any which are supposed to be permanent,
     make local again, with the same values they had.  */
     
  for (alist = oalist; !NILP (alist); alist = XCDR (alist))
    {
      sym = XCAR (XCAR (alist));
      tem = Fget (sym, Qpermanent_local);
      if (! NILP (tem))
	{
	  Fmake_local_variable (sym);
	  Fset (sym, XCDR (XCAR (alist)));
	}
    }

  /* Force mode-line redisplay.  Useful here because all major mode
     commands call this function.  */
  update_mode_lines++;

  return Qnil;
}

/* Make sure no local variables remain set up with buffer B
   for their current values.  */

static void
swap_out_buffer_local_variables (b)
     struct buffer *b;
{
  Lisp_Object oalist, alist, sym, tem, buffer;

  XSETBUFFER (buffer, b);
  oalist = b->local_var_alist;

  for (alist = oalist; !NILP (alist); alist = XCDR (alist))
    {
      sym = XCAR (XCAR (alist));

      /* Need not do anything if some other buffer's binding is now encached.  */
      tem = XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->buffer;
      if (BUFFERP (tem) && XBUFFER (tem) == current_buffer)
	{
	  /* Symbol is set up for this buffer's old local value.
	     Set it up for the current buffer with the default value.  */

	  tem = XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->cdr;
	  /* Store the symbol's current value into the alist entry
	     it is currently set up for.  This is so that, if the
	     local is marked permanent, and we make it local again
	     later in Fkill_all_local_variables, we don't lose the value.  */
	  XCDR (XCAR (tem))
	    = do_symval_forwarding (XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->realvalue);
	  /* Switch to the symbol's default-value alist entry.  */
	  XCAR (tem) = tem;
	  /* Mark it as current for buffer B.  */
	  XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->buffer = buffer;
	  /* Store the current value into any forwarding in the symbol.  */
	  store_symval_forwarding (sym,
				   XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->realvalue,
				   XCDR (tem), NULL);
	}
    }
}

/* Find all the overlays in the current buffer that contain position POS.
   Return the number found, and store them in a vector in *VEC_PTR.  
   Store in *LEN_PTR the size allocated for the vector.
   Store in *NEXT_PTR the next position after POS where an overlay starts,
     or ZV if there are no more overlays.
   Store in *PREV_PTR the previous position before POS where an overlay ends,
     or where an overlay starts which ends at or after POS;
     or BEGV if there are no such overlays.
   NEXT_PTR and/or PREV_PTR may be 0, meaning don't store that info.

   *VEC_PTR and *LEN_PTR should contain a valid vector and size
   when this function is called.

   If EXTEND is non-zero, we make the vector bigger if necessary.
   If EXTEND is zero, we never extend the vector,
   and we store only as many overlays as will fit.
   But we still return the total number of overlays.

   If CHANGE_REQ is true, then any position written into *PREV_PTR or
   *NEXT_PTR is guaranteed to be not equal to POS, unless it is the
   default (BEGV or ZV).  */

int
overlays_at (pos, extend, vec_ptr, len_ptr, next_ptr, prev_ptr, change_req)
     int pos;
     int extend;
     Lisp_Object **vec_ptr;
     int *len_ptr;
     int *next_ptr;
     int *prev_ptr;
     int change_req;
{
  Lisp_Object tail, overlay, start, end;
  int idx = 0;
  int len = *len_ptr;
  Lisp_Object *vec = *vec_ptr;
  int next = ZV;
  int prev = BEGV;
  int inhibit_storing = 0;

  for (tail = current_buffer->overlays_before;
       GC_CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos, endpos;

      overlay = XCAR (tail);

      start = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (endpos < pos)
	{
	  if (prev < endpos)
	    prev = endpos;
	  break;
	}
      startpos = OVERLAY_POSITION (start);
      /* This one ends at or after POS
	 so its start counts for PREV_PTR if it's before POS.  */
      if (prev < startpos && startpos < pos)
	prev = startpos;
      if (endpos == pos)
	continue;
      if (startpos <= pos)
	{
	  if (idx == len)
	    {
	      /* The supplied vector is full.
		 Either make it bigger, or don't store any more in it.  */
	      if (extend)
		{
		  /* Make it work with an initial len == 0.  */
		  len *= 2;
		  if (len == 0)
		    len = 4;
		  *len_ptr = len;
		  vec = (Lisp_Object *) xrealloc (vec, len * sizeof (Lisp_Object));
		  *vec_ptr = vec;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  /* Keep counting overlays even if we can't return them all.  */
	  idx++;
	}
      else if (startpos < next)
	next = startpos;
    }

  for (tail = current_buffer->overlays_after;
       GC_CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos, endpos;

      overlay = XCAR (tail);

      start = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);
      startpos = OVERLAY_POSITION (start);
      if (pos < startpos)
	{
	  if (startpos < next)
	    next = startpos;
	  break;
	}
      endpos = OVERLAY_POSITION (end);
      if (pos < endpos)
	{
	  if (idx == len)
	    {
	      if (extend)
		{
		  *len_ptr = len *= 2;
		  if (len == 0)
		    len = *len_ptr = 4;
		  vec = (Lisp_Object *) xrealloc (vec, len * sizeof (Lisp_Object));
		  *vec_ptr = vec;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  idx++;

	  if (startpos < pos && startpos > prev)
	    prev = startpos;
	}
      else if (endpos < pos && endpos > prev)
	prev = endpos;
      else if (endpos == pos && startpos > prev
	       && (!change_req || startpos < pos))
	prev = startpos;
    }

  if (next_ptr)
    *next_ptr = next;
  if (prev_ptr)
    *prev_ptr = prev;
  return idx;
}

/* Find all the overlays in the current buffer that overlap the range BEG-END
   or are empty at BEG.

   Return the number found, and store them in a vector in *VEC_PTR.  
   Store in *LEN_PTR the size allocated for the vector.
   Store in *NEXT_PTR the next position after POS where an overlay starts,
     or ZV if there are no more overlays.
   Store in *PREV_PTR the previous position before POS where an overlay ends,
     or BEGV if there are no previous overlays.
   NEXT_PTR and/or PREV_PTR may be 0, meaning don't store that info.

   *VEC_PTR and *LEN_PTR should contain a valid vector and size
   when this function is called.

   If EXTEND is non-zero, we make the vector bigger if necessary.
   If EXTEND is zero, we never extend the vector,
   and we store only as many overlays as will fit.
   But we still return the total number of overlays.  */

int
overlays_in (beg, end, extend, vec_ptr, len_ptr, next_ptr, prev_ptr)
     int beg, end;
     int extend;
     Lisp_Object **vec_ptr;
     int *len_ptr;
     int *next_ptr;
     int *prev_ptr;
{
  Lisp_Object tail, overlay, ostart, oend;
  int idx = 0;
  int len = *len_ptr;
  Lisp_Object *vec = *vec_ptr;
  int next = ZV;
  int prev = BEGV;
  int inhibit_storing = 0;

  for (tail = current_buffer->overlays_before;
       GC_CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos, endpos;

      overlay = XCAR (tail);

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (oend);
      if (endpos < beg)
	{
	  if (prev < endpos)
	    prev = endpos;
	  break;
	}
      startpos = OVERLAY_POSITION (ostart);
      /* Count an interval if it either overlaps the range
	 or is empty at the start of the range.  */
      if ((beg < endpos && startpos < end)
	  || (startpos == endpos && beg == endpos))
	{
	  if (idx == len)
	    {
	      /* The supplied vector is full.
		 Either make it bigger, or don't store any more in it.  */
	      if (extend)
		{
		  *len_ptr = len *= 2;
		  vec = (Lisp_Object *) xrealloc (vec, len * sizeof (Lisp_Object));
		  *vec_ptr = vec;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  /* Keep counting overlays even if we can't return them all.  */
	  idx++;
	}
      else if (startpos < next)
	next = startpos;
    }

  for (tail = current_buffer->overlays_after;
       GC_CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos, endpos;

      overlay = XCAR (tail);

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      startpos = OVERLAY_POSITION (ostart);
      if (end < startpos)
	{
	  if (startpos < next)
	    next = startpos;
	  break;
	}
      endpos = OVERLAY_POSITION (oend);
      /* Count an interval if it either overlaps the range
	 or is empty at the start of the range.  */
      if ((beg < endpos && startpos < end)
	  || (startpos == endpos && beg == endpos))
	{
	  if (idx == len)
	    {
	      if (extend)
		{
		  *len_ptr = len *= 2;
		  vec = (Lisp_Object *) xrealloc (vec, len * sizeof (Lisp_Object));
		  *vec_ptr = vec;
		}
	      else
		inhibit_storing = 1;
	    }

	  if (!inhibit_storing)
	    vec[idx] = overlay;
	  idx++;
	}
      else if (endpos < beg && endpos > prev)
	prev = endpos;
    }

  if (next_ptr)
    *next_ptr = next;
  if (prev_ptr)
    *prev_ptr = prev;
  return idx;
}


/* Return non-zero if there exists an overlay with a non-nil
   `mouse-face' property overlapping OVERLAY.  */

int
mouse_face_overlay_overlaps (overlay)
     Lisp_Object overlay;
{
  int start = OVERLAY_POSITION (OVERLAY_START (overlay));
  int end = OVERLAY_POSITION (OVERLAY_END (overlay));
  int n, i, size;
  Lisp_Object *v, tem;
  
  size = 10;
  v = (Lisp_Object *) alloca (size * sizeof *v);
  n = overlays_in (start, end, 0, &v, &size, NULL, NULL);
  if (n > size)
    {
      v = (Lisp_Object *) alloca (n * sizeof *v);
      overlays_in (start, end, 0, &v, &n, NULL, NULL);
    }

  for (i = 0; i < n; ++i)
    if (!EQ (v[i], overlay)
	&& (tem = Foverlay_get (overlay, Qmouse_face),
	    !NILP (tem)))
      break;

  return i < n;
}



/* Fast function to just test if we're at an overlay boundary.  */
int
overlay_touches_p (pos)
     int pos;
{
  Lisp_Object tail, overlay;

  for (tail = current_buffer->overlays_before; GC_CONSP (tail);
       tail = XCDR (tail))
    {
      int endpos;

      overlay = XCAR (tail);
      if (!GC_OVERLAYP (overlay))
	abort ();

      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (endpos < pos)
	break;
      if (endpos == pos || OVERLAY_POSITION (OVERLAY_START (overlay)) == pos)
	return 1;
    }

  for (tail = current_buffer->overlays_after; GC_CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos;

      overlay = XCAR (tail);
      if (!GC_OVERLAYP (overlay))
	abort ();

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      if (pos < startpos)
	break;
      if (startpos == pos || OVERLAY_POSITION (OVERLAY_END (overlay)) == pos)
	return 1;
    }
  return 0;
}

struct sortvec
{
  Lisp_Object overlay;
  int beg, end;
  int priority;
};

static int
compare_overlays (v1, v2)
     const void *v1, *v2;
{
  const struct sortvec *s1 = (const struct sortvec *) v1;
  const struct sortvec *s2 = (const struct sortvec *) v2;
  if (s1->priority != s2->priority)
    return s1->priority - s2->priority;
  if (s1->beg != s2->beg)
    return s1->beg - s2->beg;
  if (s1->end != s2->end)
    return s2->end - s1->end;
  return 0;
}

/* Sort an array of overlays by priority.  The array is modified in place.
   The return value is the new size; this may be smaller than the original
   size if some of the overlays were invalid or were window-specific.  */
int
sort_overlays (overlay_vec, noverlays, w)
     Lisp_Object *overlay_vec;
     int noverlays;
     struct window *w;
{
  int i, j;
  struct sortvec *sortvec;
  sortvec = (struct sortvec *) alloca (noverlays * sizeof (struct sortvec));

  /* Put the valid and relevant overlays into sortvec.  */

  for (i = 0, j = 0; i < noverlays; i++)
    {
      Lisp_Object tem;
      Lisp_Object overlay;

      overlay = overlay_vec[i];
      if (OVERLAY_VALID (overlay)
	  && OVERLAY_POSITION (OVERLAY_START (overlay)) > 0
	  && OVERLAY_POSITION (OVERLAY_END (overlay)) > 0)
	{
	  /* If we're interested in a specific window, then ignore
	     overlays that are limited to some other window.  */
	  if (w)
	    {
	      Lisp_Object window;

	      window = Foverlay_get (overlay, Qwindow);
	      if (WINDOWP (window) && XWINDOW (window) != w)
		continue;
	    }

	  /* This overlay is good and counts: put it into sortvec.  */
	  sortvec[j].overlay = overlay;
	  sortvec[j].beg = OVERLAY_POSITION (OVERLAY_START (overlay));
	  sortvec[j].end = OVERLAY_POSITION (OVERLAY_END (overlay));
	  tem = Foverlay_get (overlay, Qpriority);
	  if (INTEGERP (tem))
	    sortvec[j].priority = XINT (tem);
	  else
	    sortvec[j].priority = 0;
	  j++;
	}
    }
  noverlays = j;

  /* Sort the overlays into the proper order: increasing priority.  */

  if (noverlays > 1)
    qsort (sortvec, noverlays, sizeof (struct sortvec), compare_overlays);

  for (i = 0; i < noverlays; i++)
    overlay_vec[i] = sortvec[i].overlay;
  return (noverlays);
}

struct sortstr
{
  Lisp_Object string, string2;
  int size;
  int priority;
};

struct sortstrlist
{
  struct sortstr *buf;	/* An array that expands as needed; never freed.  */
  int size;		/* Allocated length of that array.  */
  int used;		/* How much of the array is currently in use.  */
  int bytes;		/* Total length of the strings in buf.  */
};

/* Buffers for storing information about the overlays touching a given
   position.  These could be automatic variables in overlay_strings, but
   it's more efficient to hold onto the memory instead of repeatedly
   allocating and freeing it.  */
static struct sortstrlist overlay_heads, overlay_tails;
static unsigned char *overlay_str_buf;

/* Allocated length of overlay_str_buf.  */
static int overlay_str_len;

/* A comparison function suitable for passing to qsort.  */
static int
cmp_for_strings (as1, as2)
     char *as1, *as2;
{
  struct sortstr *s1 = (struct sortstr *)as1;
  struct sortstr *s2 = (struct sortstr *)as2;
  if (s1->size != s2->size)
    return s2->size - s1->size;
  if (s1->priority != s2->priority)
    return s1->priority - s2->priority;
  return 0;
}

static void
record_overlay_string (ssl, str, str2, pri, size)
     struct sortstrlist *ssl;
     Lisp_Object str, str2, pri;
     int size;
{
  int nbytes;

  if (ssl->used == ssl->size)
    {
      if (ssl->buf)
	ssl->size *= 2;
      else
	ssl->size = 5;
      ssl->buf = ((struct sortstr *)
		  xrealloc (ssl->buf, ssl->size * sizeof (struct sortstr)));
    }
  ssl->buf[ssl->used].string = str;
  ssl->buf[ssl->used].string2 = str2;
  ssl->buf[ssl->used].size = size;
  ssl->buf[ssl->used].priority = (INTEGERP (pri) ? XINT (pri) : 0);
  ssl->used++;

  if (NILP (current_buffer->enable_multibyte_characters))
    nbytes = XSTRING (str)->size;
  else if (! STRING_MULTIBYTE (str))
    nbytes = count_size_as_multibyte (XSTRING (str)->data,
				      STRING_BYTES (XSTRING (str)));
  else
    nbytes = STRING_BYTES (XSTRING (str));

  ssl->bytes += nbytes;

  if (STRINGP (str2))
    {
      if (NILP (current_buffer->enable_multibyte_characters))
	nbytes = XSTRING (str2)->size;
      else if (! STRING_MULTIBYTE (str2))
	nbytes = count_size_as_multibyte (XSTRING (str2)->data,
					  STRING_BYTES (XSTRING (str2)));
      else
	nbytes = STRING_BYTES (XSTRING (str2));

      ssl->bytes += nbytes;
    }
}

/* Return the concatenation of the strings associated with overlays that
   begin or end at POS, ignoring overlays that are specific to a window
   other than W.  The strings are concatenated in the appropriate order:
   shorter overlays nest inside longer ones, and higher priority inside
   lower.  Normally all of the after-strings come first, but zero-sized
   overlays have their after-strings ride along with the before-strings
   because it would look strange to print them inside-out.

   Returns the string length, and stores the contents indirectly through
   PSTR, if that variable is non-null.  The string may be overwritten by
   subsequent calls.  */

int
overlay_strings (pos, w, pstr)
     int pos;
     struct window *w;
     unsigned char **pstr;
{
  Lisp_Object ov, overlay, window, str;
  int startpos, endpos;
  int multibyte = ! NILP (current_buffer->enable_multibyte_characters);

  overlay_heads.used = overlay_heads.bytes = 0;
  overlay_tails.used = overlay_tails.bytes = 0;
  for (ov = current_buffer->overlays_before; CONSP (ov); ov = XCDR (ov))
    {
      overlay = XCAR (ov);
      if (!OVERLAYP (overlay))
	abort ();

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (endpos < pos)
	break;
      if (endpos != pos && startpos != pos)
	continue;
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != w)
	continue;
      if (startpos == pos
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str)))
	record_overlay_string (&overlay_heads, str,
			       (startpos == endpos
				? Foverlay_get (overlay, Qafter_string)
				: Qnil),
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
      else if (endpos == pos
	  && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str)))
	record_overlay_string (&overlay_tails, str, Qnil,
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
    }
  for (ov = current_buffer->overlays_after; CONSP (ov); ov = XCDR (ov))
    {
      overlay = XCAR (ov);
      if (!OVERLAYP (overlay))
	abort ();

      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (startpos > pos)
	break;
      if (endpos != pos && startpos != pos)
	continue;
      window = Foverlay_get (overlay, Qwindow);
      if (WINDOWP (window) && XWINDOW (window) != w)
	continue;
      if (startpos == pos
	  && (str = Foverlay_get (overlay, Qbefore_string), STRINGP (str)))
	record_overlay_string (&overlay_heads, str,
			       (startpos == endpos
				? Foverlay_get (overlay, Qafter_string)
				: Qnil),
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
      else if (endpos == pos
	       && (str = Foverlay_get (overlay, Qafter_string), STRINGP (str)))
	record_overlay_string (&overlay_tails, str, Qnil,
			       Foverlay_get (overlay, Qpriority),
			       endpos - startpos);
    }
  if (overlay_tails.used > 1)
    qsort (overlay_tails.buf, overlay_tails.used, sizeof (struct sortstr),
	   cmp_for_strings);
  if (overlay_heads.used > 1)
    qsort (overlay_heads.buf, overlay_heads.used, sizeof (struct sortstr),
	   cmp_for_strings);
  if (overlay_heads.bytes || overlay_tails.bytes)
    {
      Lisp_Object tem;
      int i;
      unsigned char *p;
      int total = overlay_heads.bytes + overlay_tails.bytes;

      if (total > overlay_str_len)
	{
	  overlay_str_len = total;
	  overlay_str_buf = (unsigned char *)xrealloc (overlay_str_buf,
						       total);
	}
      p = overlay_str_buf;
      for (i = overlay_tails.used; --i >= 0;)
	{
	  int nbytes;
	  tem = overlay_tails.buf[i].string;
	  nbytes = copy_text (XSTRING (tem)->data, p,
			      STRING_BYTES (XSTRING (tem)),
			      STRING_MULTIBYTE (tem), multibyte);
	  p += nbytes;
	}
      for (i = 0; i < overlay_heads.used; ++i)
	{
	  int nbytes;
	  tem = overlay_heads.buf[i].string;
	  nbytes = copy_text (XSTRING (tem)->data, p,
			      STRING_BYTES (XSTRING (tem)),
			      STRING_MULTIBYTE (tem), multibyte);
	  p += nbytes;
	  tem = overlay_heads.buf[i].string2;
	  if (STRINGP (tem))
	    {
	      nbytes = copy_text (XSTRING (tem)->data, p,
				  STRING_BYTES (XSTRING (tem)),
				  STRING_MULTIBYTE (tem), multibyte);
	      p += nbytes;
	    }
	}
      if (p != overlay_str_buf + total)
	abort ();
      if (pstr)
	*pstr = overlay_str_buf;
      return total;
    }
  return 0;
}

/* Shift overlays in BUF's overlay lists, to center the lists at POS.  */

void
recenter_overlay_lists (buf, pos)
     struct buffer *buf;
     int pos;
{
  Lisp_Object overlay, tail, next, prev, beg, end;

  /* See if anything in overlays_before should move to overlays_after.  */

  /* We don't strictly need prev in this loop; it should always be nil.
     But we use it for symmetry and in case that should cease to be true
     with some future change.  */
  prev = Qnil;
  for (tail = buf->overlays_before;
       CONSP (tail);
       prev = tail, tail = next)
    {
      next = XCDR (tail);
      overlay = XCAR (tail);

      /* If the overlay is not valid, get rid of it.  */
      if (!OVERLAY_VALID (overlay))
#if 1
	abort ();
#else
	{
	  /* Splice the cons cell TAIL out of overlays_before.  */
	  if (!NILP (prev))
	    XCDR (prev) = next;
	  else
	    buf->overlays_before = next;
	  tail = prev;
	  continue;
	}
#endif

      beg = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);

      if (OVERLAY_POSITION (end) > pos)
	{
	  /* OVERLAY needs to be moved.  */
	  int where = OVERLAY_POSITION (beg);
	  Lisp_Object other, other_prev;

	  /* Splice the cons cell TAIL out of overlays_before.  */
	  if (!NILP (prev))
	    XCDR (prev) = next;
	  else
	    buf->overlays_before = next;

	  /* Search thru overlays_after for where to put it.  */
	  other_prev = Qnil;
	  for (other = buf->overlays_after;
	       CONSP (other);
	       other_prev = other, other = XCDR (other))
	    {
	      Lisp_Object otherbeg, otheroverlay;

	      otheroverlay = XCAR (other);
	      if (! OVERLAY_VALID (otheroverlay))
		abort ();

	      otherbeg = OVERLAY_START (otheroverlay);
	      if (OVERLAY_POSITION (otherbeg) >= where)
		break;
	    }

	  /* Add TAIL to overlays_after before OTHER.  */
	  XCDR (tail) = other;
	  if (!NILP (other_prev))
	    XCDR (other_prev) = tail;
	  else
	    buf->overlays_after = tail;
	  tail = prev;
	}
      else
	/* We've reached the things that should stay in overlays_before.
	   All the rest of overlays_before must end even earlier,
	   so stop now.  */
	break;
    }

  /* See if anything in overlays_after should be in overlays_before.  */
  prev = Qnil;
  for (tail = buf->overlays_after;
       CONSP (tail);
       prev = tail, tail = next)
    {
      next = XCDR (tail);
      overlay = XCAR (tail);

      /* If the overlay is not valid, get rid of it.  */
      if (!OVERLAY_VALID (overlay))
#if 1
	abort ();
#else
	{
	  /* Splice the cons cell TAIL out of overlays_after.  */
	  if (!NILP (prev))
	    XCDR (prev) = next;
	  else
	    buf->overlays_after = next;
	  tail = prev;
	  continue;
	}
#endif

      beg = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);

      /* Stop looking, when we know that nothing further
	 can possibly end before POS.  */
      if (OVERLAY_POSITION (beg) > pos)
	break;

      if (OVERLAY_POSITION (end) <= pos)
	{
	  /* OVERLAY needs to be moved.  */
	  int where = OVERLAY_POSITION (end);
	  Lisp_Object other, other_prev;

	  /* Splice the cons cell TAIL out of overlays_after.  */
	  if (!NILP (prev))
	    XCDR (prev) = next;
	  else
	    buf->overlays_after = next;

	  /* Search thru overlays_before for where to put it.  */
	  other_prev = Qnil;
	  for (other = buf->overlays_before;
	       CONSP (other);
	       other_prev = other, other = XCDR (other))
	    {
	      Lisp_Object otherend, otheroverlay;

	      otheroverlay = XCAR (other);
	      if (! OVERLAY_VALID (otheroverlay))
		abort ();

	      otherend = OVERLAY_END (otheroverlay);
	      if (OVERLAY_POSITION (otherend) <= where)
		break;
	    }

	  /* Add TAIL to overlays_before before OTHER.  */
	  XCDR (tail) = other;
	  if (!NILP (other_prev))
	    XCDR (other_prev) = tail;
	  else
	    buf->overlays_before = tail;
	  tail = prev;
	}
    }

  XSETFASTINT (buf->overlay_center, pos);
}

void
adjust_overlays_for_insert (pos, length)
     int pos;
     int length;
{
  /* After an insertion, the lists are still sorted properly,
     but we may need to update the value of the overlay center.  */
  if (XFASTINT (current_buffer->overlay_center) >= pos)
    XSETFASTINT (current_buffer->overlay_center,
		 XFASTINT (current_buffer->overlay_center) + length);
}

void
adjust_overlays_for_delete (pos, length)
     int pos;
     int length;
{
  if (XFASTINT (current_buffer->overlay_center) < pos)
    /* The deletion was to our right.  No change needed; the before- and
       after-lists are still consistent.  */
    ;
  else if (XFASTINT (current_buffer->overlay_center) > pos + length)
    /* The deletion was to our left.  We need to adjust the center value
       to account for the change in position, but the lists are consistent
       given the new value.  */
    XSETFASTINT (current_buffer->overlay_center,
		 XFASTINT (current_buffer->overlay_center) - length);
  else
    /* We're right in the middle.  There might be things on the after-list
       that now belong on the before-list.  Recentering will move them,
       and also update the center point.  */
    recenter_overlay_lists (current_buffer, pos);
}

/* Fix up overlays that were garbled as a result of permuting markers
   in the range START through END.  Any overlay with at least one
   endpoint in this range will need to be unlinked from the overlay
   list and reinserted in its proper place.
   Such an overlay might even have negative size at this point.
   If so, we'll reverse the endpoints.  Can you think of anything
   better to do in this situation?  */
void
fix_overlays_in_range (start, end)
     register int start, end;
{
  Lisp_Object overlay;
  Lisp_Object before_list, after_list;
  Lisp_Object *ptail, *pbefore = &before_list, *pafter = &after_list;
  int startpos, endpos;

  /* This algorithm shifts links around instead of consing and GCing.
     The loop invariant is that before_list (resp. after_list) is a
     well-formed list except that its last element, the one that
     *pbefore (resp. *pafter) points to, is still uninitialized.
     So it's not a bug that before_list isn't initialized, although
     it may look strange.  */
  for (ptail = &current_buffer->overlays_before; CONSP (*ptail);)
    {
      overlay = XCAR (*ptail);
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (endpos < start)
	break;
      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      if (endpos < end
	  || (startpos >= start && startpos < end))
	{
	  /* If the overlay is backwards, fix that now.  */
	  if (startpos > endpos)
	    {
	      int tem;
	      Fset_marker (OVERLAY_START (overlay), make_number (endpos),
			   Qnil);
	      Fset_marker (OVERLAY_END (overlay), make_number (startpos),
			   Qnil);
	      tem = startpos; startpos = endpos; endpos = tem;
	    }
	  /* Add it to the end of the wrong list.  Later on,
	     recenter_overlay_lists will move it to the right place.  */
	  if (endpos < XINT (current_buffer->overlay_center))
	    {
	      *pafter = *ptail;
	      pafter = &XCDR (*ptail);
	    }
	  else
	    {
	      *pbefore = *ptail;
	      pbefore = &XCDR (*ptail);
	    }
	  *ptail = XCDR (*ptail);
	}
      else
	ptail = &XCDR (*ptail);
    }
  for (ptail = &current_buffer->overlays_after; CONSP (*ptail);)
    {
      overlay = XCAR (*ptail);
      startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
      if (startpos >= end)
	break;
      endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
      if (startpos >= start
	  || (endpos >= start && endpos < end))
	{
	  if (startpos > endpos)
	    {
	      int tem;
	      Fset_marker (OVERLAY_START (overlay), make_number (endpos),
			   Qnil);
	      Fset_marker (OVERLAY_END (overlay), make_number (startpos),
			   Qnil);
	      tem = startpos; startpos = endpos; endpos = tem;
	    }
	  if (endpos < XINT (current_buffer->overlay_center))
	    {
	      *pafter = *ptail;
	      pafter = &XCDR (*ptail);
	    }
	  else
	    {
	      *pbefore = *ptail;
	      pbefore = &XCDR (*ptail);
	    }
	  *ptail = XCDR (*ptail);
	}
      else
	ptail = &XCDR (*ptail);
    }

  /* Splice the constructed (wrong) lists into the buffer's lists,
     and let the recenter function make it sane again.  */
  *pbefore = current_buffer->overlays_before;
  current_buffer->overlays_before = before_list;
  recenter_overlay_lists (current_buffer,
			  XINT (current_buffer->overlay_center));

  *pafter = current_buffer->overlays_after;
  current_buffer->overlays_after = after_list;
  recenter_overlay_lists (current_buffer,
			  XINT (current_buffer->overlay_center));
}

/* We have two types of overlay: the one whose ending marker is
   after-insertion-marker (this is the usual case) and the one whose
   ending marker is before-insertion-marker.  When `overlays_before'
   contains overlays of the latter type and the former type in this
   order and both overlays end at inserting position, inserting a text
   increases only the ending marker of the latter type, which results
   in incorrect ordering of `overlays_before'.

   This function fixes ordering of overlays in the slot
   `overlays_before' of the buffer *BP.  Before the insertion, `point'
   was at PREV, and now is at POS.  */

void
fix_overlays_before (bp, prev, pos)
     struct buffer *bp;
     int prev, pos;
{
  Lisp_Object *tailp = &bp->overlays_before;
  Lisp_Object *right_place;
  int end;

  /* After the insertion, the several overlays may be in incorrect
     order.  The possibility is that, in the list `overlays_before',
     an overlay which ends at POS appears after an overlay which ends
     at PREV.  Since POS is greater than PREV, we must fix the
     ordering of these overlays, by moving overlays ends at POS before
     the overlays ends at PREV.  */

  /* At first, find a place where disordered overlays should be linked
     in.  It is where an overlay which end before POS exists. (i.e. an
     overlay whose ending marker is after-insertion-marker if disorder
     exists).  */
  while (!NILP (*tailp)
	 && ((end = OVERLAY_POSITION (OVERLAY_END (XCAR (*tailp))))
	     >= pos))
    tailp = &XCDR (*tailp);

  /* If we don't find such an overlay,
     or the found one ends before PREV,
     or the found one is the last one in the list,
     we don't have to fix anything.  */
  if (NILP (*tailp)
      || end < prev
      || NILP (XCDR (*tailp)))
    return;

  right_place = tailp;
  tailp = &XCDR (*tailp);

  /* Now, end position of overlays in the list *TAILP should be before
     or equal to PREV.  In the loop, an overlay which ends at POS is
     moved ahead to the place pointed by RIGHT_PLACE.  If we found an
     overlay which ends before PREV, the remaining overlays are in
     correct order.  */
  while (!NILP (*tailp))
    {
      end = OVERLAY_POSITION (OVERLAY_END (XCAR (*tailp)));

      if (end == pos)
	{			/* This overlay is disordered. */
	  Lisp_Object found = *tailp;

	  /* Unlink the found overlay.  */
	  *tailp = XCDR (found);
	  /* Move an overlay at RIGHT_PLACE to the next of the found one.  */
	  XCDR (found) = *right_place;
	  /* Link it into the right place.  */
	  *right_place = found;
	}
      else if (end == prev)
	tailp = &XCDR (*tailp);
      else			/* No more disordered overlay. */
	break;
    }
}

DEFUN ("overlayp", Foverlayp, Soverlayp, 1, 1, 0,
  "Return t if OBJECT is an overlay.")
  (object)
     Lisp_Object object;
{
  return (OVERLAYP (object) ? Qt : Qnil);
}

DEFUN ("make-overlay", Fmake_overlay, Smake_overlay, 2, 5, 0,
  "Create a new overlay with range BEG to END in BUFFER.\n\
If omitted, BUFFER defaults to the current buffer.\n\
BEG and END may be integers or markers.\n\
The fourth arg FRONT-ADVANCE, if non-nil, makes the\n\
front delimiter advance when text is inserted there.\n\
The fifth arg REAR-ADVANCE, if non-nil, makes the\n\
rear delimiter advance when text is inserted there.")
  (beg, end, buffer, front_advance, rear_advance)
     Lisp_Object beg, end, buffer;
     Lisp_Object front_advance, rear_advance;
{
  Lisp_Object overlay;
  struct buffer *b;

  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);
  else
    CHECK_BUFFER (buffer, 2);
  if (MARKERP (beg)
      && ! EQ (Fmarker_buffer (beg), buffer))
    error ("Marker points into wrong buffer");
  if (MARKERP (end)
      && ! EQ (Fmarker_buffer (end), buffer))
    error ("Marker points into wrong buffer");

  CHECK_NUMBER_COERCE_MARKER (beg, 1);
  CHECK_NUMBER_COERCE_MARKER (end, 1);

  if (XINT (beg) > XINT (end))
    {
      Lisp_Object temp;
      temp = beg; beg = end; end = temp;
    }

  b = XBUFFER (buffer);

  beg = Fset_marker (Fmake_marker (), beg, buffer);
  end = Fset_marker (Fmake_marker (), end, buffer);

  if (!NILP (front_advance))
    XMARKER (beg)->insertion_type = 1;
  if (!NILP (rear_advance))
    XMARKER (end)->insertion_type = 1;

  overlay = allocate_misc ();
  XMISCTYPE (overlay) = Lisp_Misc_Overlay;
  XOVERLAY (overlay)->start = beg;
  XOVERLAY (overlay)->end = end;
  XOVERLAY (overlay)->plist = Qnil;

  /* Put the new overlay on the wrong list.  */ 
  end = OVERLAY_END (overlay);
  if (OVERLAY_POSITION (end) < XINT (b->overlay_center))
    b->overlays_after = Fcons (overlay, b->overlays_after);
  else
    b->overlays_before = Fcons (overlay, b->overlays_before);

  /* This puts it in the right list, and in the right order.  */
  recenter_overlay_lists (b, XINT (b->overlay_center));

  /* We don't need to redisplay the region covered by the overlay, because
     the overlay has no properties at the moment.  */

  return overlay;
}

/* Mark a section of BUF as needing redisplay because of overlays changes.  */

static void
modify_overlay (buf, start, end)
     struct buffer *buf;
     int start, end;
{
  if (start > end)
    {
      int temp = start;
      start = end;
      end = temp;
    }

  BUF_COMPUTE_UNCHANGED (buf, start, end);
  
  /* If this is a buffer not in the selected window,
     we must do other windows.  */
  if (buf != XBUFFER (XWINDOW (selected_window)->buffer))
    windows_or_buffers_changed = 1;
  /* If multiple windows show this buffer, we must do other windows.  */
  else if (buffer_shared > 1)
    windows_or_buffers_changed = 1;

  ++BUF_OVERLAY_MODIFF (buf);
}


Lisp_Object Fdelete_overlay ();

DEFUN ("move-overlay", Fmove_overlay, Smove_overlay, 3, 4, 0,
  "Set the endpoints of OVERLAY to BEG and END in BUFFER.\n\
If BUFFER is omitted, leave OVERLAY in the same buffer it inhabits now.\n\
If BUFFER is omitted, and OVERLAY is in no buffer, put it in the current\n\
buffer.")
  (overlay, beg, end, buffer)
     Lisp_Object overlay, beg, end, buffer;
{
  struct buffer *b, *ob;
  Lisp_Object obuffer;
  int count = specpdl_ptr - specpdl;

  CHECK_OVERLAY (overlay, 0);
  if (NILP (buffer))
    buffer = Fmarker_buffer (OVERLAY_START (overlay));
  if (NILP (buffer))
    XSETBUFFER (buffer, current_buffer);
  CHECK_BUFFER (buffer, 3);

  if (MARKERP (beg)
      && ! EQ (Fmarker_buffer (beg), buffer))
    error ("Marker points into wrong buffer");
  if (MARKERP (end)
      && ! EQ (Fmarker_buffer (end), buffer))
    error ("Marker points into wrong buffer");

  CHECK_NUMBER_COERCE_MARKER (beg, 1);
  CHECK_NUMBER_COERCE_MARKER (end, 1);

  if (XINT (beg) == XINT (end) && ! NILP (Foverlay_get (overlay, Qevaporate)))
    return Fdelete_overlay (overlay);

  if (XINT (beg) > XINT (end))
    {
      Lisp_Object temp;
      temp = beg; beg = end; end = temp;
    }

  specbind (Qinhibit_quit, Qt);

  obuffer = Fmarker_buffer (OVERLAY_START (overlay));
  b = XBUFFER (buffer);
  ob = BUFFERP (obuffer) ? XBUFFER (obuffer) : (struct buffer *) 0;

  /* If the overlay has changed buffers, do a thorough redisplay.  */
  if (!EQ (buffer, obuffer))
    {
      /* Redisplay where the overlay was.  */
      if (!NILP (obuffer))
	{
	  int o_beg;
	  int o_end;

	  o_beg = OVERLAY_POSITION (OVERLAY_START (overlay));
	  o_end = OVERLAY_POSITION (OVERLAY_END (overlay));

	  modify_overlay (ob, o_beg, o_end);
	}

      /* Redisplay where the overlay is going to be.  */
      modify_overlay (b, XINT (beg), XINT (end));
    }
  else
    /* Redisplay the area the overlay has just left, or just enclosed.  */
    {
      int o_beg, o_end;

      o_beg = OVERLAY_POSITION (OVERLAY_START (overlay));
      o_end = OVERLAY_POSITION (OVERLAY_END (overlay));

      if (o_beg == XINT (beg))
	modify_overlay (b, o_end, XINT (end));
      else if (o_end == XINT (end))
	modify_overlay (b, o_beg, XINT (beg));
      else
	{
	  if (XINT (beg) < o_beg) o_beg = XINT (beg);
	  if (XINT (end) > o_end) o_end = XINT (end);
	  modify_overlay (b, o_beg, o_end);
	}
    }

  if (!NILP (obuffer))
    {
      ob->overlays_before = Fdelq (overlay, ob->overlays_before);
      ob->overlays_after  = Fdelq (overlay, ob->overlays_after);
    }

  Fset_marker (OVERLAY_START (overlay), beg, buffer);
  Fset_marker (OVERLAY_END   (overlay), end, buffer);

  /* Put the overlay on the wrong list.  */ 
  end = OVERLAY_END (overlay);
  if (OVERLAY_POSITION (end) < XINT (b->overlay_center))
    b->overlays_after = Fcons (overlay, b->overlays_after);
  else
    b->overlays_before = Fcons (overlay, b->overlays_before);

  /* This puts it in the right list, and in the right order.  */
  recenter_overlay_lists (b, XINT (b->overlay_center));

  return unbind_to (count, overlay);
}

DEFUN ("delete-overlay", Fdelete_overlay, Sdelete_overlay, 1, 1, 0,
  "Delete the overlay OVERLAY from its buffer.")
  (overlay)
     Lisp_Object overlay;
{
  Lisp_Object buffer;
  struct buffer *b;
  int count = specpdl_ptr - specpdl;

  CHECK_OVERLAY (overlay, 0);

  buffer = Fmarker_buffer (OVERLAY_START (overlay));
  if (NILP (buffer))
    return Qnil;

  b = XBUFFER (buffer);
  specbind (Qinhibit_quit, Qt);
  
  b->overlays_before = Fdelq (overlay, b->overlays_before);
  b->overlays_after = Fdelq (overlay, b->overlays_after);
  modify_overlay (b,
		  marker_position (OVERLAY_START (overlay)),
		  marker_position (OVERLAY_END   (overlay)));
  Fset_marker (OVERLAY_START (overlay), Qnil, Qnil);
  Fset_marker (OVERLAY_END   (overlay), Qnil, Qnil);

  /* When deleting an overlay with before or after strings, turn off
     display optimizations for the affected buffer, on the basis that
     these strings may contain newlines.  This is easier to do than to
     check for that situation during redisplay.  */
  if (!windows_or_buffers_changed
      && (!NILP (Foverlay_get (overlay, Qbefore_string))
	  || !NILP (Foverlay_get (overlay, Qafter_string))))
    b->prevent_redisplay_optimizations_p = 1;

  return unbind_to (count, Qnil);
}

/* Overlay dissection functions.  */

DEFUN ("overlay-start", Foverlay_start, Soverlay_start, 1, 1, 0,
  "Return the position at which OVERLAY starts.")
     (overlay)
     Lisp_Object overlay;
{
  CHECK_OVERLAY (overlay, 0);

  return (Fmarker_position (OVERLAY_START (overlay)));
}

DEFUN ("overlay-end", Foverlay_end, Soverlay_end, 1, 1, 0,
  "Return the position at which OVERLAY ends.")
     (overlay)
     Lisp_Object overlay;
{
  CHECK_OVERLAY (overlay, 0);

  return (Fmarker_position (OVERLAY_END (overlay)));
}

DEFUN ("overlay-buffer", Foverlay_buffer, Soverlay_buffer, 1, 1, 0,
  "Return the buffer OVERLAY belongs to.")
     (overlay)
       Lisp_Object overlay;
{
  CHECK_OVERLAY (overlay, 0);

  return Fmarker_buffer (OVERLAY_START (overlay));
}

DEFUN ("overlay-properties", Foverlay_properties, Soverlay_properties, 1, 1, 0,
  "Return a list of the properties on OVERLAY.\n\
This is a copy of OVERLAY's plist; modifying its conses has no effect on\n\
OVERLAY.")
  (overlay)
    Lisp_Object overlay;
{
  CHECK_OVERLAY (overlay, 0);

  return Fcopy_sequence (XOVERLAY (overlay)->plist);
}


DEFUN ("overlays-at", Foverlays_at, Soverlays_at, 1, 1, 0,
  "Return a list of the overlays that contain position POS.")
  (pos)
     Lisp_Object pos;
{
  int noverlays;
  Lisp_Object *overlay_vec;
  int len;
  Lisp_Object result;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  len = 10;
  /* We can't use alloca here because overlays_at can call xrealloc.  */
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
			   (int *) 0, (int *) 0, 0);

  /* Make a list of them all.  */
  result = Flist (noverlays, overlay_vec);

  xfree (overlay_vec);
  return result;
}

DEFUN ("overlays-in", Foverlays_in, Soverlays_in, 2, 2, 0,
  "Return a list of the overlays that overlap the region BEG ... END.\n\
Overlap means that at least one character is contained within the overlay\n\
and also contained within the specified region.\n\
Empty overlays are included in the result if they are located at BEG\n\
or between BEG and END.")
  (beg, end)
     Lisp_Object beg, end;
{
  int noverlays;
  Lisp_Object *overlay_vec;
  int len;
  Lisp_Object result;

  CHECK_NUMBER_COERCE_MARKER (beg, 0);
  CHECK_NUMBER_COERCE_MARKER (end, 0);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_in (XINT (beg), XINT (end), 1, &overlay_vec, &len,
			   (int *) 0, (int *) 0);

  /* Make a list of them all.  */
  result = Flist (noverlays, overlay_vec);

  xfree (overlay_vec);
  return result;
}

DEFUN ("next-overlay-change", Fnext_overlay_change, Snext_overlay_change,
  1, 1, 0,
  "Return the next position after POS where an overlay starts or ends.\n\
If there are no more overlay boundaries after POS, return (point-max).")
  (pos)
     Lisp_Object pos;
{
  int noverlays;
  int endpos;
  Lisp_Object *overlay_vec;
  int len;
  int i;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.
     endpos gets the position where the next overlay starts.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
			   &endpos, (int *) 0, 1);

  /* If any of these overlays ends before endpos,
     use its ending point instead.  */
  for (i = 0; i < noverlays; i++)
    {
      Lisp_Object oend;
      int oendpos;

      oend = OVERLAY_END (overlay_vec[i]);
      oendpos = OVERLAY_POSITION (oend);
      if (oendpos < endpos)
	endpos = oendpos;
    }

  xfree (overlay_vec);
  return make_number (endpos);
}

DEFUN ("previous-overlay-change", Fprevious_overlay_change,
       Sprevious_overlay_change, 1, 1, 0,
  "Return the previous position before POS where an overlay starts or ends.\n\
If there are no more overlay boundaries before POS, return (point-min).")
  (pos)
     Lisp_Object pos;
{
  int noverlays;
  int prevpos;
  Lisp_Object *overlay_vec;
  int len;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  /* At beginning of buffer, we know the answer;
     avoid bug subtracting 1 below.  */
  if (XINT (pos) == BEGV)
    return pos;

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.
     prevpos gets the position of the previous change.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
			   (int *) 0, &prevpos, 1);

  xfree (overlay_vec);
  return make_number (prevpos);
}

/* These functions are for debugging overlays.  */

DEFUN ("overlay-lists", Foverlay_lists, Soverlay_lists, 0, 0, 0,
  "Return a pair of lists giving all the overlays of the current buffer.\n\
The car has all the overlays before the overlay center;\n\
the cdr has all the overlays after the overlay center.\n\
Recentering overlays moves overlays between these lists.\n\
The lists you get are copies, so that changing them has no effect.\n\
However, the overlays you get are the real objects that the buffer uses.")
  ()
{
  Lisp_Object before, after;
  before = current_buffer->overlays_before;
  if (CONSP (before))
    before = Fcopy_sequence (before);
  after = current_buffer->overlays_after;
  if (CONSP (after))
    after = Fcopy_sequence (after);

  return Fcons (before, after);
}

DEFUN ("overlay-recenter", Foverlay_recenter, Soverlay_recenter, 1, 1, 0,
  "Recenter the overlays of the current buffer around position POS.")
  (pos)
     Lisp_Object pos;
{
  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  recenter_overlay_lists (current_buffer, XINT (pos));
  return Qnil;
}

DEFUN ("overlay-get", Foverlay_get, Soverlay_get, 2, 2, 0,
  "Get the property of overlay OVERLAY with property name PROP.")
  (overlay, prop)
     Lisp_Object overlay, prop;
{
  Lisp_Object plist, fallback;

  CHECK_OVERLAY (overlay, 0);

  fallback = Qnil;

  for (plist = XOVERLAY (overlay)->plist;
       CONSP (plist) && CONSP (XCDR (plist));
       plist = XCDR (XCDR (plist)))
    {
      if (EQ (XCAR (plist), prop))
	return XCAR (XCDR (plist));
      else if (EQ (XCAR (plist), Qcategory))
	{
	  Lisp_Object tem;
	  tem = Fcar (Fcdr (plist));
	  if (SYMBOLP (tem))
	    fallback = Fget (tem, prop);
	}
    }

  return fallback;
}

DEFUN ("overlay-put", Foverlay_put, Soverlay_put, 3, 3, 0,
  "Set one property of overlay OVERLAY: give property PROP value VALUE.")
  (overlay, prop, value)
     Lisp_Object overlay, prop, value;
{
  Lisp_Object tail, buffer;
  int changed;

  CHECK_OVERLAY (overlay, 0);

  buffer = Fmarker_buffer (OVERLAY_START (overlay));

  for (tail = XOVERLAY (overlay)->plist;
       CONSP (tail) && CONSP (XCDR (tail));
       tail = XCDR (XCDR (tail)))
    if (EQ (XCAR (tail), prop))
      {
	changed = !EQ (XCAR (XCDR (tail)), value);
	XCAR (XCDR (tail)) = value;
	goto found;
      }
  /* It wasn't in the list, so add it to the front.  */
  changed = !NILP (value);
  XOVERLAY (overlay)->plist
    = Fcons (prop, Fcons (value, XOVERLAY (overlay)->plist));
 found:
  if (! NILP (buffer))
    {
      if (changed)
	modify_overlay (XBUFFER (buffer),
			marker_position (OVERLAY_START (overlay)),
			marker_position (OVERLAY_END   (overlay)));
      if (EQ (prop, Qevaporate) && ! NILP (value)
	  && (OVERLAY_POSITION (OVERLAY_START (overlay))
	      == OVERLAY_POSITION (OVERLAY_END (overlay))))
	Fdelete_overlay (overlay);
    }
  return value;
}

/* Subroutine of report_overlay_modification.  */

/* Lisp vector holding overlay hook functions to call.
   Vector elements come in pairs.
   Each even-index element is a list of hook functions.
   The following odd-index element is the overlay they came from.

   Before the buffer change, we fill in this vector
   as we call overlay hook functions.
   After the buffer change, we get the functions to call from this vector.
   This way we always call the same functions before and after the change.  */
static Lisp_Object last_overlay_modification_hooks;

/* Number of elements actually used in last_overlay_modification_hooks.  */
static int last_overlay_modification_hooks_used;

/* Add one functionlist/overlay pair
   to the end of last_overlay_modification_hooks.  */

static void
add_overlay_mod_hooklist (functionlist, overlay)
     Lisp_Object functionlist, overlay;
{
  int oldsize = XVECTOR (last_overlay_modification_hooks)->size;

  if (last_overlay_modification_hooks_used == oldsize)
    {
      Lisp_Object old;
      old = last_overlay_modification_hooks;
      last_overlay_modification_hooks
	= Fmake_vector (make_number (oldsize * 2), Qnil);
      bcopy (XVECTOR (old)->contents,
	     XVECTOR (last_overlay_modification_hooks)->contents,
	     sizeof (Lisp_Object) * oldsize);
    }
  XVECTOR (last_overlay_modification_hooks)->contents[last_overlay_modification_hooks_used++] = functionlist;
  XVECTOR (last_overlay_modification_hooks)->contents[last_overlay_modification_hooks_used++] = overlay;
}

/* Run the modification-hooks of overlays that include
   any part of the text in START to END.
   If this change is an insertion, also
   run the insert-before-hooks of overlay starting at END,
   and the insert-after-hooks of overlay ending at START.

   This is called both before and after the modification.
   AFTER is nonzero when we call after the modification.

   ARG1, ARG2, ARG3 are arguments to pass to the hook functions.
   When AFTER is nonzero, they are the start position,
   the position after the inserted new text,
   and the length of deleted or replaced old text.  */

void
report_overlay_modification (start, end, after, arg1, arg2, arg3)
     Lisp_Object start, end;
     int after;
     Lisp_Object arg1, arg2, arg3;
{
  Lisp_Object prop, overlay, tail;
  /* 1 if this change is an insertion.  */
  int insertion = (after ? XFASTINT (arg3) == 0 : EQ (start, end));
  int tail_copied;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  overlay = Qnil;
  tail = Qnil;
  GCPRO5 (overlay, tail, arg1, arg2, arg3);

  if (after)
    {
      /* Call the functions recorded in last_overlay_modification_hooks
	 rather than scanning the overlays again.
	 First copy the vector contents, in case some of these hooks
	 do subsequent modification of the buffer.  */
      int size = last_overlay_modification_hooks_used;
      Lisp_Object *copy = (Lisp_Object *) alloca (size * sizeof (Lisp_Object));
      int i;

      bcopy (XVECTOR (last_overlay_modification_hooks)->contents,
	     copy, size * sizeof (Lisp_Object));
      gcpro1.var = copy;
      gcpro1.nvars = size;

      for (i = 0; i < size;)
	{
	  Lisp_Object prop, overlay;
	  prop = copy[i++];
	  overlay = copy[i++];
	  call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	}
      UNGCPRO;
      return;
    }

  /* We are being called before a change.
     Scan the overlays to find the functions to call.  */
  last_overlay_modification_hooks_used = 0;
  tail_copied = 0;
  for (tail = current_buffer->overlays_before;
       CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos, endpos;
      Lisp_Object ostart, oend;

      overlay = XCAR (tail);

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (oend);
      if (XFASTINT (start) > endpos)
	break;
      startpos = OVERLAY_POSITION (ostart);
      if (insertion && (XFASTINT (start) == startpos
			|| XFASTINT (end) == startpos))
	{
	  prop = Foverlay_get (overlay, Qinsert_in_front_hooks);
	  if (!NILP (prop))
	    {
	      /* Copy TAIL in case the hook recenters the overlay lists.  */
	      if (!tail_copied)
		tail = Fcopy_sequence (tail);
	      tail_copied = 1;
	      call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	    }
	}
      if (insertion && (XFASTINT (start) == endpos
			|| XFASTINT (end) == endpos))
	{
	  prop = Foverlay_get (overlay, Qinsert_behind_hooks);
	  if (!NILP (prop))
	    {
	      if (!tail_copied)
		tail = Fcopy_sequence (tail);
	      tail_copied = 1;
	      call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	    }
	}
      /* Test for intersecting intervals.  This does the right thing
	 for both insertion and deletion.  */
      if (XFASTINT (end) > startpos && XFASTINT (start) < endpos)
	{
	  prop = Foverlay_get (overlay, Qmodification_hooks);
	  if (!NILP (prop))
	    {
	      if (!tail_copied)
		tail = Fcopy_sequence (tail);
	      tail_copied = 1;
	      call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	    }
	}
    }

  tail_copied = 0;
  for (tail = current_buffer->overlays_after;
       CONSP (tail);
       tail = XCDR (tail))
    {
      int startpos, endpos;
      Lisp_Object ostart, oend;

      overlay = XCAR (tail);

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      startpos = OVERLAY_POSITION (ostart);
      endpos = OVERLAY_POSITION (oend);
      if (XFASTINT (end) < startpos)
	break;
      if (insertion && (XFASTINT (start) == startpos
			|| XFASTINT (end) == startpos))
	{
	  prop = Foverlay_get (overlay, Qinsert_in_front_hooks);
	  if (!NILP (prop))
	    {
	      if (!tail_copied)
		tail = Fcopy_sequence (tail);
	      tail_copied = 1;
	      call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	    }
	}
      if (insertion && (XFASTINT (start) == endpos
			|| XFASTINT (end) == endpos))
	{
	  prop = Foverlay_get (overlay, Qinsert_behind_hooks);
	  if (!NILP (prop))
	    {
	      if (!tail_copied)
		tail = Fcopy_sequence (tail);
	      tail_copied = 1;
	      call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	    }
	}
      /* Test for intersecting intervals.  This does the right thing
	 for both insertion and deletion.  */
      if (XFASTINT (end) > startpos && XFASTINT (start) < endpos)
	{
	  prop = Foverlay_get (overlay, Qmodification_hooks);
	  if (!NILP (prop))
	    {
	      if (!tail_copied)
		tail = Fcopy_sequence (tail);
	      tail_copied = 1;
	      call_overlay_mod_hooks (prop, overlay, after, arg1, arg2, arg3);
	    }
	}
    }

  UNGCPRO;
}

static void
call_overlay_mod_hooks (list, overlay, after, arg1, arg2, arg3)
     Lisp_Object list, overlay;
     int after;
     Lisp_Object arg1, arg2, arg3;
{
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (list, arg1, arg2, arg3);
  if (! after)
    add_overlay_mod_hooklist (list, overlay);

  while (!NILP (list))
    {
      if (NILP (arg3))
	call4 (Fcar (list), overlay, after ? Qt : Qnil, arg1, arg2);
      else
	call5 (Fcar (list), overlay, after ? Qt : Qnil, arg1, arg2, arg3);
      list = Fcdr (list);
    }
  UNGCPRO;
}

/* Delete any zero-sized overlays at position POS, if the `evaporate'
   property is set.  */
void
evaporate_overlays (pos)
     int pos;
{
  Lisp_Object tail, overlay, hit_list;

  hit_list = Qnil;
  if (pos <= XFASTINT (current_buffer->overlay_center))
    for (tail = current_buffer->overlays_before; CONSP (tail);
	 tail = XCDR (tail))
      {
	int endpos;
	overlay = XCAR (tail);
	endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
	if (endpos < pos)
	  break;
	if (endpos == pos && OVERLAY_POSITION (OVERLAY_START (overlay)) == pos
	    && ! NILP (Foverlay_get (overlay, Qevaporate)))
	  hit_list = Fcons (overlay, hit_list);
      }
  else
    for (tail = current_buffer->overlays_after; CONSP (tail);
	 tail = XCDR (tail))
      {
	int startpos;
	overlay = XCAR (tail);
	startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
	if (startpos > pos)
	  break;
	if (startpos == pos && OVERLAY_POSITION (OVERLAY_END (overlay)) == pos
	    && ! NILP (Foverlay_get (overlay, Qevaporate)))
	  hit_list = Fcons (overlay, hit_list);
      }
  for (; CONSP (hit_list); hit_list = XCDR (hit_list))
    Fdelete_overlay (XCAR (hit_list));
}

/* Somebody has tried to store a value with an unacceptable type
   in the slot with offset OFFSET.  */

void
buffer_slot_type_mismatch (offset)
     int offset;
{
  Lisp_Object sym;
  char *type_name;
  
  switch (XINT (PER_BUFFER_TYPE (offset)))
    {
    case Lisp_Int:
      type_name = "integers";
      break;
      
    case Lisp_String:
      type_name = "strings";
      break;
      
    case Lisp_Symbol:
      type_name = "symbols";
      break;
      
    default:
      abort ();
    }

  sym = PER_BUFFER_SYMBOL (offset);
  error ("Only %s should be stored in the buffer-local variable %s",
	 type_name, XSYMBOL (sym)->name->data);
}


/***********************************************************************
			 Allocation with mmap
 ***********************************************************************/

#ifdef USE_MMAP_FOR_BUFFERS

#include <sys/types.h>
#include <sys/mman.h>

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON 0
#endif
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#include <stdio.h>
#include <errno.h>

#if MAP_ANON == 0
#include <fcntl.h>
#endif

#include "coding.h"


/* Memory is allocated in regions which are mapped using mmap(2).
   The current implementation lets the system select mapped
   addresses;  we're not using MAP_FIXED in general, except when
   trying to enlarge regions.

   Each mapped region starts with a mmap_region structure, the user
   area starts after that structure, aligned to MEM_ALIGN.

	+-----------------------+
	| struct mmap_info +	|
	| padding		|
	+-----------------------+
	| user data		|
	|			|
	|			|
	+-----------------------+  */

struct mmap_region
{
  /* User-specified size.  */
  size_t nbytes_specified;
  
  /* Number of bytes mapped */
  size_t nbytes_mapped;

  /* Pointer to the location holding the address of the memory
     allocated with the mmap'd block.  The variable actually points
     after this structure.  */
  POINTER_TYPE **var;

  /* Next and previous in list of all mmap'd regions.  */
  struct mmap_region *next, *prev;
};

/* Doubly-linked list of mmap'd regions.  */

static struct mmap_region *mmap_regions;

/* File descriptor for mmap.  If we don't have anonymous mapping,
   /dev/zero will be opened on it.  */

static int mmap_fd;

/* Temporary storage for mmap_set_vars, see there.  */

static struct mmap_region *mmap_regions_1;
static int mmap_fd_1;

/* Page size on this system.  */

static int mmap_page_size;

/* 1 means mmap has been intialized.  */

static int mmap_initialized_p;

/* Value is X rounded up to the next multiple of N.  */

#define ROUND(X, N)	(((X) + (N) - 1) / (N) * (N))

/* Size of mmap_region structure plus padding.  */

#define MMAP_REGION_STRUCT_SIZE	\
     ROUND (sizeof (struct mmap_region), MEM_ALIGN)

/* Given a pointer P to the start of the user-visible part of a mapped
   region, return a pointer to the start of the region.  */

#define MMAP_REGION(P) \
     ((struct mmap_region *) ((char *) (P) - MMAP_REGION_STRUCT_SIZE))

/* Given a pointer P to the start of a mapped region, return a pointer
   to the start of the user-visible part of the region.  */

#define MMAP_USER_AREA(P) \
     ((POINTER_TYPE *) ((char *) (P) + MMAP_REGION_STRUCT_SIZE))

#define MEM_ALIGN	sizeof (double)

/* Predicate returning true if part of the address range [START ..
   END[ is currently mapped.  Used to prevent overwriting an existing
   memory mapping.

   Default is to conservativly assume the address range is occupied by
   something else.  This can be overridden by system configuration
   files if system-specific means to determine this exists.  */

#ifndef MMAP_ALLOCATED_P
#define MMAP_ALLOCATED_P(start, end) 1
#endif

/* Function prototypes.  */

static int mmap_free_1 P_ ((struct mmap_region *));
static int mmap_enlarge P_ ((struct mmap_region *, int));
static struct mmap_region *mmap_find P_ ((POINTER_TYPE *, POINTER_TYPE *));
static POINTER_TYPE *mmap_alloc P_ ((POINTER_TYPE **, size_t));
static POINTER_TYPE *mmap_realloc P_ ((POINTER_TYPE **, size_t));
static void mmap_free P_ ((POINTER_TYPE **ptr));
static void mmap_init P_ ((void));


/* Return a region overlapping address range START...END, or null if
   none.  END is not including, i.e. the last byte in the range
   is at END - 1.  */

static struct mmap_region *
mmap_find (start, end)
     POINTER_TYPE *start, *end;
{
  struct mmap_region *r;
  char *s = (char *) start, *e = (char *) end;
  
  for (r = mmap_regions; r; r = r->next)
    {
      char *rstart = (char *) r;
      char *rend   = rstart + r->nbytes_mapped;

      if (/* First byte of range, i.e. START, in this region?  */
	  (s >= rstart && s < rend)
	  /* Last byte of range, i.e. END - 1, in this region?  */
	  || (e > rstart && e <= rend)
	  /* First byte of this region in the range?  */
	  || (rstart >= s && rstart < e)
	  /* Last byte of this region in the range?  */
	  || (rend > s && rend <= e))
	break;
    }

  return r;
}


/* Unmap a region.  P is a pointer to the start of the user-araa of
   the region.  Value is non-zero if successful.  */

static int
mmap_free_1 (r)
     struct mmap_region *r;
{
  if (r->next)
    r->next->prev = r->prev;
  if (r->prev)
    r->prev->next = r->next;
  else
    mmap_regions = r->next;
  
  if (munmap ((POINTER_TYPE *) r, r->nbytes_mapped) == -1)
    {
      fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
      return 0;
    }

  return 1;
}


/* Enlarge region R by NPAGES pages.  NPAGES < 0 means shrink R.
   Value is non-zero if successful.  */

static int
mmap_enlarge (r, npages)
     struct mmap_region *r;
     int npages;
{
  char *region_end = (char *) r + r->nbytes_mapped;
  size_t nbytes;
  int success = 0;

  if (npages < 0)
    {
      /* Unmap pages at the end of the region.  */
      nbytes = - npages * mmap_page_size;
      if (munmap (region_end - nbytes, nbytes) == -1)
	fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
      else
	{
	  r->nbytes_mapped -= nbytes;
	  success = 1;
	}
    }
  else if (npages > 0)
    {
      nbytes = npages * mmap_page_size;
      
      /* Try to map additional pages at the end of the region.  We
	 cannot do this if the address range is already occupied by
	 something else because mmap deletes any previous mapping.
	 I'm not sure this is worth doing, let's see.  */
      if (!MMAP_ALLOCATED_P (region_end, region_end + nbytes))
	{
	  POINTER_TYPE *p;
      
	  p = mmap (region_end, nbytes, PROT_READ | PROT_WRITE,
		    MAP_ANON | MAP_PRIVATE | MAP_FIXED, mmap_fd, 0);
	  if (p == MAP_FAILED)
	    ; /* fprintf (stderr, "mmap: %s\n", emacs_strerror (errno)); */
	  else if (p != (POINTER_TYPE *) region_end)
	    {
	      /* Kernels are free to choose a different address.  In
		 that case, unmap what we've mapped above; we have
		 no use for it.  */
	      if (munmap (p, nbytes) == -1)
		fprintf (stderr, "munmap: %s\n", emacs_strerror (errno));
	    }
	  else
	    {
	      r->nbytes_mapped += nbytes;
	      success = 1;
	    }
	}
    }

  return success;
}


/* Set or reset variables holding references to mapped regions.  If
   RESTORE_P is zero, set all variables to null.  If RESTORE_P is
   non-zero, set all variables to the start of the user-areas
   of mapped regions.

   This function is called from Fdump_emacs to ensure that the dumped
   Emacs doesn't contain references to memory that won't be mapped
   when Emacs starts.  */

void
mmap_set_vars (restore_p)
     int restore_p;
{
  struct mmap_region *r;

  if (restore_p)
    {
      mmap_regions = mmap_regions_1;
      mmap_fd = mmap_fd_1;
      for (r = mmap_regions; r; r = r->next)
	*r->var = MMAP_USER_AREA (r);
    }
  else
    {
      for (r = mmap_regions; r; r = r->next)
	*r->var = NULL;
      mmap_regions_1 = mmap_regions;
      mmap_regions = NULL;
      mmap_fd_1 = mmap_fd;
      mmap_fd = -1;
    }
}


/* Allocate a block of storage large enough to hold NBYTES bytes of
   data.  A pointer to the data is returned in *VAR.  VAR is thus the
   address of some variable which will use the data area.

   The allocation of 0 bytes is valid.

   If we can't allocate the necessary memory, set *VAR to null, and
   return null.  */

static POINTER_TYPE *
mmap_alloc (var, nbytes)
     POINTER_TYPE **var;
     size_t nbytes;
{
  void *p;
  size_t map;

  mmap_init ();

  map = ROUND (nbytes + MMAP_REGION_STRUCT_SIZE, mmap_page_size);
  p = mmap (NULL, map, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE,
	    mmap_fd, 0);
  
  if (p == MAP_FAILED)
    {
      if (errno != ENOMEM)
	fprintf (stderr, "mmap: %s\n", emacs_strerror (errno));
      p = NULL;
    }
  else
    {
      struct mmap_region *r = (struct mmap_region *) p;
      
      r->nbytes_specified = nbytes;
      r->nbytes_mapped = map;
      r->var = var;
      r->prev = NULL;
      r->next = mmap_regions;
      if (r->next)
	r->next->prev = r;
      mmap_regions = r;
      
      p = MMAP_USER_AREA (p);
    }
  
  return *var = p;
}


/* Given a pointer at address VAR to data allocated with mmap_alloc,
   resize it to size NBYTES.  Change *VAR to reflect the new block,
   and return this value.  If more memory cannot be allocated, then
   leave *VAR unchanged, and return null.  */

static POINTER_TYPE *
mmap_realloc (var, nbytes)
     POINTER_TYPE **var;
     size_t nbytes;
{
  POINTER_TYPE *result;
  
  mmap_init ();

  if (*var == NULL)
    result = mmap_alloc (var, nbytes);
  else if (nbytes == 0) 
    {
      mmap_free (var);
      result = mmap_alloc (var, nbytes);
    }
  else
    {
      struct mmap_region *r = MMAP_REGION (*var);
      size_t room = r->nbytes_mapped - MMAP_REGION_STRUCT_SIZE;
      
      if (room < nbytes)
	{
	  /* Must enlarge.  */
	  POINTER_TYPE *old_ptr = *var;

	  /* Try to map additional pages at the end of the region.
	     If that fails, allocate a new region,  copy data
	     from the old region, then free it.  */
	  if (mmap_enlarge (r, (ROUND (nbytes - room, mmap_page_size)
				/ mmap_page_size)))
	    {
	      r->nbytes_specified = nbytes;
	      *var = result = old_ptr;
	    }
	  else if (mmap_alloc (var, nbytes))
	    {
	      bcopy (old_ptr, *var, r->nbytes_specified);
	      mmap_free_1 (MMAP_REGION (old_ptr));
	      result = *var;
	      r = MMAP_REGION (result);
	      r->nbytes_specified = nbytes;
	    }
	  else
	    {
	      *var = old_ptr;
	      result = NULL;
	    }
	}
      else if (room - nbytes >= mmap_page_size)
	{
	  /* Shrinking by at least a page.  Let's give some
	     memory back to the system.  */
	  mmap_enlarge (r, - (room - nbytes) / mmap_page_size);
	  result = *var;
	  r->nbytes_specified = nbytes;
	}
      else
	{
	  /* Leave it alone.  */
	  result = *var;
	  r->nbytes_specified = nbytes;
	}
    }

  return result;
}


/* Free a block of relocatable storage whose data is pointed to by
   PTR.  Store 0 in *PTR to show there's no block allocated.  */

static void
mmap_free (var)
     POINTER_TYPE **var;
{
  mmap_init ();
  
  if (*var)
    {
      mmap_free_1 (MMAP_REGION (*var));
      *var = NULL;
    }
}


/* Perform necessary intializations for the use of mmap.  */

static void
mmap_init ()
{
#if MAP_ANON == 0
  /* The value of mmap_fd is initially 0 in temacs, and -1
     in a dumped Emacs.  */
  if (mmap_fd <= 0)
    {
      /* No anonymous mmap -- we need the file descriptor.  */
      mmap_fd = open ("/dev/zero", O_RDONLY);
      if (mmap_fd == -1)
	fatal ("Cannot open /dev/zero: %s", emacs_strerror (errno));
    }
#endif /* MAP_ANON == 0 */

  if (mmap_initialized_p)
    return;
  mmap_initialized_p = 1;
  
#if MAP_ANON != 0
  mmap_fd = -1;
#endif
  
  mmap_page_size = getpagesize ();
}

#endif /* USE_MMAP_FOR_BUFFERS */



/***********************************************************************
			    Buffer-text Allocation
 ***********************************************************************/

#ifdef REL_ALLOC
extern POINTER_TYPE *r_alloc P_ ((POINTER_TYPE **, size_t));
extern POINTER_TYPE *r_re_alloc P_ ((POINTER_TYPE **, size_t));
extern void r_alloc_free P_ ((POINTER_TYPE **ptr));
#endif /* REL_ALLOC */


/* Allocate NBYTES bytes for buffer B's text buffer.  */

static void
alloc_buffer_text (b, nbytes)
     struct buffer *b;
     size_t nbytes;
{
  POINTER_TYPE *p;
  
  BLOCK_INPUT;
#if defined USE_MMAP_FOR_BUFFERS
  p = mmap_alloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#elif defined REL_ALLOC
  p = r_alloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#else
  p = xmalloc (nbytes);
#endif
  
  if (p == NULL)
    {
      UNBLOCK_INPUT;
      memory_full ();
    }

  b->text->beg = (unsigned char *) p;
  UNBLOCK_INPUT;
}

/* Enlarge buffer B's text buffer by DELTA bytes.  DELTA < 0 means
   shrink it.  */

void
enlarge_buffer_text (b, delta)
     struct buffer *b;
     int delta;
{
  POINTER_TYPE *p;
  size_t nbytes = (BUF_Z_BYTE (b) - BUF_BEG_BYTE (b) + BUF_GAP_SIZE (b) + 1
		   + delta);
  BLOCK_INPUT;
#if defined USE_MMAP_FOR_BUFFERS
  p = mmap_realloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#elif defined REL_ALLOC
  p = r_re_alloc ((POINTER_TYPE **) &b->text->beg, nbytes);
#else
  p = xrealloc (b->text->beg, nbytes);
#endif
  
  if (p == NULL)
    {
      UNBLOCK_INPUT;
      memory_full ();
    }

  BUF_BEG_ADDR (b) = (unsigned char *) p;
  UNBLOCK_INPUT;
}


/* Free buffer B's text buffer.  */

static void
free_buffer_text (b)
     struct buffer *b;
{
  BLOCK_INPUT;

#if defined USE_MMAP_FOR_BUFFERS
  mmap_free ((POINTER_TYPE **) &b->text->beg);
#elif defined REL_ALLOC
  r_alloc_free ((POINTER_TYPE **) &b->text->beg);
#else
  xfree (b->text->beg);
#endif
  
  BUF_BEG_ADDR (b) = NULL;
  UNBLOCK_INPUT;
}



/***********************************************************************
			    Initialization
 ***********************************************************************/

void
init_buffer_once ()
{
  int idx;

  bzero (buffer_permanent_local_flags, sizeof buffer_permanent_local_flags);

  /* Make sure all markable slots in buffer_defaults
     are initialized reasonably, so mark_buffer won't choke.  */
  reset_buffer (&buffer_defaults);
  reset_buffer_local_variables (&buffer_defaults, 1);
  reset_buffer (&buffer_local_symbols);
  reset_buffer_local_variables (&buffer_local_symbols, 1);
  /* Prevent GC from getting confused.  */
  buffer_defaults.text = &buffer_defaults.own_text;
  buffer_local_symbols.text = &buffer_local_symbols.own_text;
  BUF_INTERVALS (&buffer_defaults) = 0;
  BUF_INTERVALS (&buffer_local_symbols) = 0;
  XSETBUFFER (Vbuffer_defaults, &buffer_defaults);
  XSETBUFFER (Vbuffer_local_symbols, &buffer_local_symbols);

  /* Set up the default values of various buffer slots.  */
  /* Must do these before making the first buffer! */

  /* real setup is done in bindings.el */
  buffer_defaults.mode_line_format = build_string ("%-");
  buffer_defaults.header_line_format = Qnil;
  buffer_defaults.abbrev_mode = Qnil;
  buffer_defaults.overwrite_mode = Qnil;
  buffer_defaults.case_fold_search = Qt;
  buffer_defaults.auto_fill_function = Qnil;
  buffer_defaults.selective_display = Qnil;
#ifndef old
  buffer_defaults.selective_display_ellipses = Qt;
#endif
  buffer_defaults.abbrev_table = Qnil;
  buffer_defaults.display_table = Qnil;
  buffer_defaults.undo_list = Qnil;
  buffer_defaults.mark_active = Qnil;
  buffer_defaults.file_format = Qnil;
  buffer_defaults.overlays_before = Qnil;
  buffer_defaults.overlays_after = Qnil;
  XSETFASTINT (buffer_defaults.overlay_center, BEG);

  XSETFASTINT (buffer_defaults.tab_width, 8);
  buffer_defaults.truncate_lines = Qnil;
  buffer_defaults.ctl_arrow = Qt;
  buffer_defaults.direction_reversed = Qnil;
  buffer_defaults.cursor_type = Qt;
  buffer_defaults.extra_line_spacing = Qnil;

#ifdef DOS_NT
  buffer_defaults.buffer_file_type = Qnil; /* TEXT */
#endif
  buffer_defaults.enable_multibyte_characters = Qt;
  buffer_defaults.buffer_file_coding_system = Qnil;
  XSETFASTINT (buffer_defaults.fill_column, 70);
  XSETFASTINT (buffer_defaults.left_margin, 0);
  buffer_defaults.cache_long_line_scans = Qnil;
  buffer_defaults.file_truename = Qnil;
  XSETFASTINT (buffer_defaults.display_count, 0);
  buffer_defaults.indicate_empty_lines = Qnil;
  buffer_defaults.scroll_up_aggressively = Qnil;
  buffer_defaults.scroll_down_aggressively = Qnil;
  buffer_defaults.display_time = Qnil;

  /* Assign the local-flags to the slots that have default values.
     The local flag is a bit that is used in the buffer
     to say that it has its own local value for the slot.
     The local flag bits are in the local_var_flags slot of the buffer.  */

  /* Nothing can work if this isn't true */
  if (sizeof (EMACS_INT) != sizeof (Lisp_Object)) abort ();

  /* 0 means not a lisp var, -1 means always local, else mask */
  bzero (&buffer_local_flags, sizeof buffer_local_flags);
  XSETINT (buffer_local_flags.filename, -1);
  XSETINT (buffer_local_flags.directory, -1);
  XSETINT (buffer_local_flags.backed_up, -1);
  XSETINT (buffer_local_flags.save_length, -1);
  XSETINT (buffer_local_flags.auto_save_file_name, -1);
  XSETINT (buffer_local_flags.read_only, -1);
  XSETINT (buffer_local_flags.major_mode, -1);
  XSETINT (buffer_local_flags.mode_name, -1);
  XSETINT (buffer_local_flags.undo_list, -1);
  XSETINT (buffer_local_flags.mark_active, -1);
  XSETINT (buffer_local_flags.point_before_scroll, -1);
  XSETINT (buffer_local_flags.file_truename, -1);
  XSETINT (buffer_local_flags.invisibility_spec, -1);
  XSETINT (buffer_local_flags.file_format, -1);
  XSETINT (buffer_local_flags.display_count, -1);
  XSETINT (buffer_local_flags.display_time, -1);
  XSETINT (buffer_local_flags.enable_multibyte_characters, -1);

  idx = 1;
  XSETFASTINT (buffer_local_flags.mode_line_format, idx); ++idx;
  XSETFASTINT (buffer_local_flags.abbrev_mode, idx); ++idx;
  XSETFASTINT (buffer_local_flags.overwrite_mode, idx); ++idx;
  XSETFASTINT (buffer_local_flags.case_fold_search, idx); ++idx;
  XSETFASTINT (buffer_local_flags.auto_fill_function, idx); ++idx;
  XSETFASTINT (buffer_local_flags.selective_display, idx); ++idx;
#ifndef old
  XSETFASTINT (buffer_local_flags.selective_display_ellipses, idx); ++idx;
#endif
  XSETFASTINT (buffer_local_flags.tab_width, idx); ++idx;
  XSETFASTINT (buffer_local_flags.truncate_lines, idx); ++idx;
  XSETFASTINT (buffer_local_flags.ctl_arrow, idx); ++idx;
  XSETFASTINT (buffer_local_flags.fill_column, idx); ++idx;
  XSETFASTINT (buffer_local_flags.left_margin, idx); ++idx;
  XSETFASTINT (buffer_local_flags.abbrev_table, idx); ++idx;
  XSETFASTINT (buffer_local_flags.display_table, idx); ++idx;
#ifdef DOS_NT
  XSETFASTINT (buffer_local_flags.buffer_file_type, idx);
  /* Make this one a permanent local.  */
  buffer_permanent_local_flags[idx++] = 1;
#endif
  XSETFASTINT (buffer_local_flags.syntax_table, idx); ++idx;
  XSETFASTINT (buffer_local_flags.cache_long_line_scans, idx); ++idx;
  XSETFASTINT (buffer_local_flags.category_table, idx); ++idx;
  XSETFASTINT (buffer_local_flags.direction_reversed, idx); ++idx;
  XSETFASTINT (buffer_local_flags.buffer_file_coding_system, idx); 
  /* Make this one a permanent local.  */
  buffer_permanent_local_flags[idx++] = 1;
  XSETFASTINT (buffer_local_flags.left_margin_width, idx); ++idx;
  XSETFASTINT (buffer_local_flags.right_margin_width, idx); ++idx;
  XSETFASTINT (buffer_local_flags.indicate_empty_lines, idx); ++idx;
  XSETFASTINT (buffer_local_flags.scroll_up_aggressively, idx); ++idx;
  XSETFASTINT (buffer_local_flags.scroll_down_aggressively, idx); ++idx;
  XSETFASTINT (buffer_local_flags.header_line_format, idx); ++idx;
  XSETFASTINT (buffer_local_flags.cursor_type, idx); ++idx;
  XSETFASTINT (buffer_local_flags.extra_line_spacing, idx); ++idx;

  /* Need more room? */
  if (idx >= MAX_PER_BUFFER_VARS)
    abort ();
  last_per_buffer_idx = idx;
  
  Vbuffer_alist = Qnil;
  current_buffer = 0;
  all_buffers = 0;

  QSFundamental = build_string ("Fundamental");

  Qfundamental_mode = intern ("fundamental-mode");
  buffer_defaults.major_mode = Qfundamental_mode;

  Qmode_class = intern ("mode-class");

  Qprotected_field = intern ("protected-field");

  Qpermanent_local = intern ("permanent-local");

  Qkill_buffer_hook = intern ("kill-buffer-hook");

  Vprin1_to_string_buffer = Fget_buffer_create (build_string (" prin1"));

  /* super-magic invisible buffer */
  Vbuffer_alist = Qnil;

  Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));

  inhibit_modification_hooks = 0;
}

void
init_buffer ()
{
  char buf[MAXPATHLEN + 1];
  char *pwd;
  struct stat dotstat, pwdstat;
  Lisp_Object temp;
  int rc;

#ifdef USE_MMAP_FOR_BUFFERS
 {
   /* When using the ralloc implementation based on mmap(2), buffer
      text pointers will have been set to null in the dumped Emacs.
      Map new memory.  */
   struct buffer *b;
   
   for (b = all_buffers; b; b = b->next)
     if (b->text->beg == NULL)
       enlarge_buffer_text (b, 0);
 }
#endif /* USE_MMAP_FOR_BUFFERS */
  
  Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));
  if (NILP (buffer_defaults.enable_multibyte_characters))
    Fset_buffer_multibyte (Qnil);

  /* If PWD is accurate, use it instead of calling getwd.  This is faster
     when PWD is right, and may avoid a fatal error.  */
  if ((pwd = getenv ("PWD")) != 0
      && (IS_DIRECTORY_SEP (*pwd) || (*pwd && IS_DEVICE_SEP (pwd[1])))
      && stat (pwd, &pwdstat) == 0
      && stat (".", &dotstat) == 0
      && dotstat.st_ino == pwdstat.st_ino
      && dotstat.st_dev == pwdstat.st_dev
      && strlen (pwd) < MAXPATHLEN)
    strcpy (buf, pwd);
#ifdef HAVE_GETCWD
  else if (getcwd (buf, MAXPATHLEN+1) == 0)
    fatal ("`getcwd' failed: %s\n", strerror (errno));
#else
  else if (getwd (buf) == 0)
    fatal ("`getwd' failed: %s\n", buf);
#endif

#ifndef VMS
  /* Maybe this should really use some standard subroutine
     whose definition is filename syntax dependent.  */
  rc = strlen (buf);
  if (!(IS_DIRECTORY_SEP (buf[rc - 1])))
    {
      buf[rc] = DIRECTORY_SEP;
      buf[rc + 1] = '\0';
    }
#endif /* not VMS */

  current_buffer->directory = build_string (buf);

  /* Add /: to the front of the name
     if it would otherwise be treated as magic.  */
  temp = Ffind_file_name_handler (current_buffer->directory, Qt);
  if (! NILP (temp)
      /* If the default dir is just /, TEMP is non-nil
	 because of the ange-ftp completion handler.
	 However, it is not necessary to turn / into /:/.
	 So avoid doing that.  */
      && strcmp ("/", XSTRING (current_buffer->directory)->data))
    current_buffer->directory
      = concat2 (build_string ("/:"), current_buffer->directory);

  temp = get_minibuffer (0);
  XBUFFER (temp)->directory = current_buffer->directory;
}

/* initialize the buffer routines */
void
syms_of_buffer ()
{
  staticpro (&last_overlay_modification_hooks);
  last_overlay_modification_hooks
    = Fmake_vector (make_number (10), Qnil);

  staticpro (&Vbuffer_defaults);
  staticpro (&Vbuffer_local_symbols);
  staticpro (&Qfundamental_mode);
  staticpro (&Qmode_class);
  staticpro (&QSFundamental);
  staticpro (&Vbuffer_alist);
  staticpro (&Qprotected_field);
  staticpro (&Qpermanent_local);
  staticpro (&Qkill_buffer_hook);
  Qoverlayp = intern ("overlayp");
  staticpro (&Qoverlayp);
  Qevaporate = intern ("evaporate");
  staticpro (&Qevaporate);
  Qmodification_hooks = intern ("modification-hooks");
  staticpro (&Qmodification_hooks);
  Qinsert_in_front_hooks = intern ("insert-in-front-hooks");
  staticpro (&Qinsert_in_front_hooks);
  Qinsert_behind_hooks = intern ("insert-behind-hooks");
  staticpro (&Qinsert_behind_hooks);
  Qget_file_buffer = intern ("get-file-buffer");
  staticpro (&Qget_file_buffer);
  Qpriority = intern ("priority");
  staticpro (&Qpriority);
  Qwindow = intern ("window");
  staticpro (&Qwindow);
  Qbefore_string = intern ("before-string");
  staticpro (&Qbefore_string);
  Qafter_string = intern ("after-string");
  staticpro (&Qafter_string);
  Qfirst_change_hook = intern ("first-change-hook");
  staticpro (&Qfirst_change_hook);
  Qbefore_change_functions = intern ("before-change-functions");
  staticpro (&Qbefore_change_functions);
  Qafter_change_functions = intern ("after-change-functions");
  staticpro (&Qafter_change_functions);

  Fput (Qprotected_field, Qerror_conditions,
	Fcons (Qprotected_field, Fcons (Qerror, Qnil)));
  Fput (Qprotected_field, Qerror_message,
	build_string ("Attempt to modify a protected field"));

  /* All these use DEFVAR_LISP_NOPRO because the slots in
     buffer_defaults will all be marked via Vbuffer_defaults.  */

  DEFVAR_LISP_NOPRO ("default-mode-line-format",
		     &buffer_defaults.mode_line_format,
    "Default value of `mode-line-format' for buffers that don't override it.\n\
This is the same as (default-value 'mode-line-format).");

  DEFVAR_LISP_NOPRO ("default-header-line-format",
		     &buffer_defaults.header_line_format,
    "Default value of `header-line-format' for buffers that don't override it.\n\
This is the same as (default-value 'header-line-format).");

  DEFVAR_LISP_NOPRO ("default-cursor-type", &buffer_defaults.cursor_type,
    "Default value of `cursor-type' for buffers that don't override it.\n\
This is the same as (default-value 'cursor-type).");

  DEFVAR_LISP_NOPRO ("default-line-spacing",
		     &buffer_defaults.extra_line_spacing,
    "Default value of `line-spacing' for buffers that don't override it.\n\
This is the same as (default-value 'line-spacing).");

  DEFVAR_LISP_NOPRO ("default-abbrev-mode",
	      &buffer_defaults.abbrev_mode,
    "Default value of `abbrev-mode' for buffers that do not override it.\n\
This is the same as (default-value 'abbrev-mode).");

  DEFVAR_LISP_NOPRO ("default-ctl-arrow",
	      &buffer_defaults.ctl_arrow,
    "Default value of `ctl-arrow' for buffers that do not override it.\n\
This is the same as (default-value 'ctl-arrow).");

   DEFVAR_LISP_NOPRO ("default-direction-reversed",
 	      &buffer_defaults.direction_reversed,
     "Default value of `direction_reversed' for buffers that do not override it.\n\
 This is the same as (default-value 'direction-reversed).");
 
   DEFVAR_LISP_NOPRO ("default-enable-multibyte-characters",
 	      &buffer_defaults.enable_multibyte_characters,
     "*Default value of `enable-multibyte-characters' for buffers not overriding it.\n\
This is the same as (default-value 'enable-multibyte-characters).");
 
   DEFVAR_LISP_NOPRO ("default-buffer-file-coding-system",
 	      &buffer_defaults.buffer_file_coding_system,
     "Default value of `buffer-file-coding-system' for buffers not overriding it.\n\
This is the same as (default-value 'buffer-file-coding-system).");
 
  DEFVAR_LISP_NOPRO ("default-truncate-lines",
	      &buffer_defaults.truncate_lines,
    "Default value of `truncate-lines' for buffers that do not override it.\n\
This is the same as (default-value 'truncate-lines).");

  DEFVAR_LISP_NOPRO ("default-fill-column",
	      &buffer_defaults.fill_column,
    "Default value of `fill-column' for buffers that do not override it.\n\
This is the same as (default-value 'fill-column).");

  DEFVAR_LISP_NOPRO ("default-left-margin",
	      &buffer_defaults.left_margin,
    "Default value of `left-margin' for buffers that do not override it.\n\
This is the same as (default-value 'left-margin).");

  DEFVAR_LISP_NOPRO ("default-tab-width",
	      &buffer_defaults.tab_width,
    "Default value of `tab-width' for buffers that do not override it.\n\
This is the same as (default-value 'tab-width).");

  DEFVAR_LISP_NOPRO ("default-case-fold-search",
	      &buffer_defaults.case_fold_search,
    "Default value of `case-fold-search' for buffers that don't override it.\n\
This is the same as (default-value 'case-fold-search).");

#ifdef DOS_NT
  DEFVAR_LISP_NOPRO ("default-buffer-file-type", 
		     &buffer_defaults.buffer_file_type,
    "Default file type for buffers that do not override it.\n\
This is the same as (default-value 'buffer-file-type).\n\
The file type is nil for text, t for binary.");
#endif

  DEFVAR_LISP_NOPRO ("default-left-margin-width",
	      &buffer_defaults.left_margin_width,
    "Default value of `left-margin-width' for buffers that don't override it.\n\
This is the same as (default-value 'left-margin-width).");

  DEFVAR_LISP_NOPRO ("default-right-margin-width",
	      &buffer_defaults.right_margin_width,
    "Default value of `right_margin_width' for buffers that don't override it.\n\
This is the same as (default-value 'right-margin-width).");
  
  DEFVAR_LISP_NOPRO ("default-indicate-empty-lines",
	      &buffer_defaults.indicate_empty_lines,
    "Default value of `indicate-empty-lines' for buffers that don't override it.\n\
This is the same as (default-value 'indicate-empty-lines).");
  
  DEFVAR_LISP_NOPRO ("default-scroll-up-aggressively",
	      &buffer_defaults.scroll_up_aggressively,
    "Default value of `scroll-up-aggressively' for buffers that\n\
don't override it.  This is the same as (default-value\n\
'scroll-up-aggressively).");
  
  DEFVAR_LISP_NOPRO ("default-scroll-down-aggressively",
	      &buffer_defaults.scroll_down_aggressively,
    "Default value of `scroll-down-aggressively' for buffers that\n\
don't override it.  This is the same as (default-value\n\
'scroll-down-aggressively).");
  
  DEFVAR_PER_BUFFER ("header-line-format",
		     &current_buffer->header_line_format, 
		     Qnil,
   "Analogous to `mode-line-format', but for the mode line that can be\n\
displayed at the top of a window.");
  
  DEFVAR_PER_BUFFER ("mode-line-format", &current_buffer->mode_line_format, 
		     Qnil, 0);

/* This doc string is too long for cpp; cpp dies if it isn't in a comment.
   But make-docfile finds it!
  DEFVAR_PER_BUFFER ("mode-line-format", &current_buffer->mode_line_format,
    Qnil,
    "Template for displaying mode line for current buffer.\n\
Each buffer has its own value of this variable.\n\
Value may be nil, a string, a symbol or a list or cons cell.\n\
A value of nil means don't display a mode line.\n\
For a symbol, its value is used (but it is ignored if t or nil).\n\
 A string appearing directly as the value of a symbol is processed verbatim\n\
 in that the %-constructs below are not recognized.\n\
For a list of the form `(:eval FORM)', FORM is evaluated and the result\n\
 is used as a mode line element.\n\
For a list whose car is a symbol, the symbol's value is taken,\n\
 and if that is non-nil, the cadr of the list is processed recursively.\n\
 Otherwise, the caddr of the list (if there is one) is processed.\n\
For a list whose car is a string or list, each element is processed\n\
 recursively and the results are effectively concatenated.\n\
For a list whose car is an integer, the cdr of the list is processed\n\
  and padded (if the number is positive) or truncated (if negative)\n\
  to the width specified by that number.\n\
A string is printed verbatim in the mode line except for %-constructs:\n\
  (%-constructs are allowed when the string is the entire mode-line-format\n\
   or when it is found in a cons-cell or a list)\n\
  %b -- print buffer name.      %f -- print visited file name.\n\
  %F -- print frame name.\n\
  %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.\n\
	%& is like %*, but ignore read-only-ness.\n\
	% means buffer is read-only and * means it is modified.\n\
	For a modified read-only buffer, %* gives % and %+ gives *.\n\
  %s -- print process status.   %l -- print the current line number.\n\
  %c -- print the current column number (this makes editing slower).\n\
        To make the column number update correctly in all cases,\n\
	`column-number-mode' must be non-nil.\n\
  %p -- print percent of buffer above top of window, or Top, Bot or All.\n\
  %P -- print percent of buffer above bottom of window, perhaps plus Top,\n\
        or print Bottom or All.\n\
  %m -- print the mode name.\n\
  %n -- print Narrow if appropriate.\n\
  %z -- print mnemonics of buffer, terminal, and keyboard coding systems.\n\
  %Z -- like %z, but including the end-of-line format.\n\
  %[ -- print one [ for each recursive editing level.  %] similar.\n\
  %% -- print %.   %- -- print infinitely many dashes.\n\
Decimal digits after the % specify field width to which to pad.");
*/

  DEFVAR_LISP_NOPRO ("default-major-mode", &buffer_defaults.major_mode,
    "*Major mode for new buffers.  Defaults to `fundamental-mode'.\n\
nil here means use current buffer's major mode.");

  DEFVAR_PER_BUFFER ("major-mode", &current_buffer->major_mode,
		     make_number (Lisp_Symbol),
    "Symbol for current buffer's major mode.");

  DEFVAR_PER_BUFFER ("mode-name", &current_buffer->mode_name,
                     make_number (Lisp_String),
    "Pretty name of current buffer's major mode (a string).");

  DEFVAR_PER_BUFFER ("abbrev-mode", &current_buffer->abbrev_mode, Qnil, 
    "Non-nil turns on automatic expansion of abbrevs as they are inserted.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("case-fold-search", &current_buffer->case_fold_search,
		     Qnil,
    "*Non-nil if searches and matches should ignore case.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("fill-column", &current_buffer->fill_column,
		     make_number (Lisp_Int),
    "*Column beyond which automatic line-wrapping should happen.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("left-margin", &current_buffer->left_margin,
		     make_number (Lisp_Int),
    "*Column for the default indent-line-function to indent to.\n\
Linefeed indents to this column in Fundamental mode.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("tab-width", &current_buffer->tab_width,
		     make_number (Lisp_Int),
    "*Distance between tab stops (for display of tab characters), in columns.\n\
Automatically becomes buffer-local when set in any fashion.");

  DEFVAR_PER_BUFFER ("ctl-arrow", &current_buffer->ctl_arrow, Qnil,
    "*Non-nil means display control chars with uparrow.\n\
A value of nil means use backslash and octal digits.\n\
Automatically becomes buffer-local when set in any fashion.\n\
This variable does not apply to characters whose display is specified\n\
in the current display table (if there is one).");

  DEFVAR_PER_BUFFER ("enable-multibyte-characters",
		     &current_buffer->enable_multibyte_characters,
		     make_number (-1),
    "Non-nil means the buffer contents are regarded as multi-byte characters.\n\
Otherwise they are regarded as unibyte.  This affects the display,\n\
file I/O and the behavior of various editing commands.\n\
\n\
This variable is buffer-local but you cannot set it directly;\n\
use the function `set-buffer-multibyte' to change a buffer's representation.\n\
Changing its default value with `setq-default' is supported.\n\
See also variable `default-enable-multibyte-characters' and Info node\n\
`(elisp)Text Representations'.");

  DEFVAR_PER_BUFFER ("buffer-file-coding-system",
		     &current_buffer->buffer_file_coding_system, Qnil,
    "Coding system to be used for encoding the buffer contents on saving.\n\
This variable applies to saving the buffer, and also to `write-region'\n\
and other functions that use `write-region'.\n\
It does not apply to sending output to subprocesses, however.\n\
\n\
If this is nil, the buffer is saved without any code conversion\n\
unless some coding system is specified in `file-coding-system-alist'\n\
for the buffer file.\n\
\n\
The variable `coding-system-for-write', if non-nil, overrides this variable.\n\
\n\
This variable is never applied to a way of decoding\n\
a file while reading it.");

  DEFVAR_PER_BUFFER ("direction-reversed", &current_buffer->direction_reversed,
		     Qnil,
    "*Non-nil means lines in the buffer are displayed right to left.");

  DEFVAR_PER_BUFFER ("truncate-lines", &current_buffer->truncate_lines, Qnil,
    "*Non-nil means do not display continuation lines;\n\
give each line of text one screen line.\n\
Automatically becomes buffer-local when set in any fashion.\n\
\n\
Note that this is overridden by the variable\n\
`truncate-partial-width-windows' if that variable is non-nil\n\
and this buffer is not full-frame width.");

#ifdef DOS_NT
  DEFVAR_PER_BUFFER ("buffer-file-type", &current_buffer->buffer_file_type,
		     Qnil,
    "Non-nil if the visited file is a binary file.\n\
This variable is meaningful on MS-DOG and Windows NT.\n\
On those systems, it is automatically local in every buffer.\n\
On other systems, this variable is normally always nil.");
#endif

  DEFVAR_PER_BUFFER ("default-directory", &current_buffer->directory,
		     make_number (Lisp_String),
    "Name of default directory of current buffer.  Should end with slash.\n\
Each buffer has its own value of this variable.  To change the\n\
default directory, use function `cd'.");

  DEFVAR_PER_BUFFER ("auto-fill-function", &current_buffer->auto_fill_function,
		     Qnil,
    "Function called (if non-nil) to perform auto-fill.\n\
It is called after self-inserting any character specified in\n\
the `auto-fill-chars' table.\n\
Each buffer has its own value of this variable.\n\
NOTE: This variable is not a hook;\n\
its value may not be a list of functions.");

  DEFVAR_PER_BUFFER ("buffer-file-name", &current_buffer->filename,
		     make_number (Lisp_String),
    "Name of file visited in current buffer, or nil if not visiting a file.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-file-truename", &current_buffer->file_truename,
		     make_number (Lisp_String),
    "Abbreviated truename of file visited in current buffer, or nil if none.\n\
The truename of a file is calculated by `file-truename'\n\
and then abbreviated with `abbreviate-file-name'.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-auto-save-file-name",
		     &current_buffer->auto_save_file_name,
		     make_number (Lisp_String),
    "Name of file for auto-saving current buffer,\n\
or nil if buffer should not be auto-saved.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-read-only", &current_buffer->read_only, Qnil,
    "Non-nil if this buffer is read-only.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-backed-up", &current_buffer->backed_up, Qnil,
    "Non-nil if this buffer's file has been backed up.\n\
Backing up is done before the first time the file is saved.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("buffer-saved-size", &current_buffer->save_length,
		     make_number (Lisp_Int),
    "Length of current buffer when last read in, saved or auto-saved.\n\
0 initially.\n\
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("selective-display", &current_buffer->selective_display,
		     Qnil,
    "Non-nil enables selective display:\n\
Integer N as value means display only lines\n\
 that start with less than n columns of space.\n\
A value of t means, after a ^M, all the rest of the line is invisible.\n\
 Then ^M's in the file are written into files as newlines.\n\n\
Automatically becomes buffer-local when set in any fashion.");

#ifndef old
  DEFVAR_PER_BUFFER ("selective-display-ellipses",
		    &current_buffer->selective_display_ellipses,
		     Qnil,
    "t means display ... on previous line when a line is invisible.\n\
Automatically becomes buffer-local when set in any fashion.");
#endif

  DEFVAR_PER_BUFFER ("overwrite-mode", &current_buffer->overwrite_mode, Qnil,
    "Non-nil if self-insertion should replace existing text.\n\
The value should be one of `overwrite-mode-textual',\n\
`overwrite-mode-binary', or nil.\n\
If it is `overwrite-mode-textual', self-insertion still\n\
inserts at the end of a line, and inserts when point is before a tab,\n\
until the tab is filled in.\n\
If `overwrite-mode-binary', self-insertion replaces newlines and tabs too.\n\
Automatically becomes buffer-local when set in any fashion.");

#if 0 /* The doc string is too long for some compilers,
	 but make-docfile can find it in this comment.  */
  DEFVAR_PER_BUFFER ("buffer-display-table", &current_buffer->display_table,
		     Qnil,
    "Display table that controls display of the contents of current buffer.\n\
Automatically becomes buffer-local when set in any fashion.\n\
\n\
If this variable is nil, the value of `standard-display-table' is used.\n\
Each window can have its own, overriding display table, see\n\
`set-window-display-table' and `window-display-table'.\n\
\n\
The display table is a char-table created with `make-display-table'.\n\
A char-table is an array indexed by character codes.  Normal array\n\
primitives `aref' and `aset' can be used to access elements of a char-table.\n\
\n\
Each of the char-table elements control how to display the corresponding\n\
text character: the element at index C in the table says how to display\n\
the character whose code is C.  Each element should be a vector of\n\
characters or nil.  nil means display the character in the default fashion;\n\
otherwise, the characters from the vector are delivered to the screen\n\
instead of the original character.\n\
\n\
For example, (aset buffer-display-table ?X ?Y) will cause Emacs to display\n\
a capital Y instead of each X character.\n\
\n\
In addition, a char-table has six extra slots to control the display of:\n\
\n\
  the end of a truncated screen line (extra-slot 0, a single character);\n\
  the end of a continued line (extra-slot 1, a single character);\n\
  the escape character used to display character codes in octal\n\
    (extra-slot 2, a single character);\n\
  the character used as an arrow for control characters (extra-slot 3,\n\
    a single character);\n\
  the decoration indicating the presence of invisible lines (extra-slot 4,\n\
    a vector of characters);\n\
  the character used to draw the border between side-by-side windows\n\
    (extra-slot 5, a single character).\n\
\n\
See also the functions `display-table-slot' and `set-display-table-slot'.");
#endif
  DEFVAR_PER_BUFFER ("buffer-display-table", &current_buffer->display_table,
		     Qnil, 0);

  DEFVAR_PER_BUFFER ("left-margin-width", &current_buffer->left_margin_width,
		     Qnil,
    "*Width of left marginal area for display of a buffer.\n\
Automatically becomes buffer-local when set in any fashion.\n\
A value of nil means no marginal area.");
  
  DEFVAR_PER_BUFFER ("right-margin-width", &current_buffer->right_margin_width,
		     Qnil,
    "*Width of right marginal area for display of a buffer.\n\
Automatically becomes buffer-local when set in any fashion.\n\
A value of nil means no marginal area.");
  
  DEFVAR_PER_BUFFER ("indicate-empty-lines",
		     &current_buffer->indicate_empty_lines, Qnil,
    "*Visually indicate empty lines after the buffer end.\n\
If non-nil, a bitmap is displayed in the left fringe of a window on\n\
window-systems.\n\
Automatically becomes buffer-local when set in any fashion.\n");
  
  DEFVAR_PER_BUFFER ("scroll-up-aggressively",
		     &current_buffer->scroll_up_aggressively, Qnil,
    "*If a number, scroll display up aggressively.\n\
If scrolling a window because point is below the window end, choose\n\
a new window start so that point ends up that fraction of the window's\n\
height from the bottom of the window.\n\
Automatically becomes buffer-local when set in any fashion.");
  
  DEFVAR_PER_BUFFER ("scroll-down-aggressively",
		     &current_buffer->scroll_down_aggressively, Qnil,
    "*If a number, scroll display down aggressively.\n\
If scrolling a window because point is above the window start, choose\n\
a new window start so that point ends up that fraction of the window's\n\
height from the top of the window.\n\
Automatically becomes buffer-local when set in any fashion.");
  
/*DEFVAR_LISP ("debug-check-symbol", &Vcheck_symbol,
    "Don't ask.");
*/

  DEFVAR_LISP ("before-change-functions", &Vbefore_change_functions,
	       "List of functions to call before each text change.\n\
Two arguments are passed to each function: the positions of\n\
the beginning and end of the range of old text to be changed.\n\
\(For an insertion, the beginning and end are at the same place.)\n\
No information is given about the length of the text after the change.\n\
\n\
Buffer changes made while executing the `before-change-functions'\n\
don't call any before-change or after-change functions.\n\
That's because these variables are temporarily set to nil.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
these variables.  See the Emacs Lisp manual for a way of\n\
accomplishing an equivalent result by using other variables.\n\
\n\
If an unhandled error happens in running these functions,\n\
the variable's value remains nil.  That prevents the error\n\
from happening repeatedly and making Emacs nonfunctional.");
  Vbefore_change_functions = Qnil;

  DEFVAR_LISP ("after-change-functions", &Vafter_change_functions,
	       "List of function to call after each text change.\n\
Three arguments are passed to each function: the positions of\n\
the beginning and end of the range of changed text,\n\
and the length in bytes of the pre-change text replaced by that range.\n\
\(For an insertion, the pre-change length is zero;\n\
for a deletion, that length is the number of bytes deleted,\n\
and the post-change beginning and end are at the same place.)\n\
\n\
Buffer changes made while executing the `after-change-functions'\n\
don't call any before-change or after-change functions.\n\
That's because these variables are temporarily set to nil.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
these variables.  See the Emacs Lisp manual for a way of\n\
accomplishing an equivalent result by using other variables.\n\
\n\
If an unhandled error happens in running these functions,\n\
the variable's value remains nil.  That prevents the error\n\
from happening repeatedly and making Emacs nonfunctional.");
  Vafter_change_functions = Qnil;

  DEFVAR_LISP ("first-change-hook", &Vfirst_change_hook,
  "A list of functions to call before changing a buffer which is unmodified.\n\
The functions are run using the `run-hooks' function.");
  Vfirst_change_hook = Qnil;

#if 0 /* The doc string is too long for some compilers,
	 but make-docfile can find it in this comment.  */
  DEFVAR_PER_BUFFER ("buffer-undo-list", &current_buffer->undo_list, Qnil,
    "List of undo entries in current buffer.\n\
This variable is always local in all buffers.\n\
Recent changes come first; older changes follow newer.\n\
\n\
An entry (BEG . END) represents an insertion which begins at\n\
position BEG and ends at position END.\n\
\n\
An entry (TEXT . POSITION) represents the deletion of the string TEXT\n\
from (abs POSITION).  If POSITION is positive, point was at the front\n\
of the text being deleted; if negative, point was at the end.\n\
\n\
An entry (t HIGH . LOW) indicates that the buffer previously had\n\
\"unmodified\" status.  HIGH and LOW are the high and low 16-bit portions\n\
of the visited file's modification time, as of that time.  If the\n\
modification time of the most recent save is different, this entry is\n\
obsolete.\n\
\n\
An entry (nil PROPERTY VALUE BEG . END) indicates that a text property\n\
was modified between BEG and END.  PROPERTY is the property name,\n\
and VALUE is the old value.\n\
\n\
An entry (MARKER . DISTANCE) indicates that the marker MARKER\n\
was adjusted in position by the offset DISTANCE (an integer).\n\
\n\
An entry of the form POSITION indicates that point was at the buffer\n\
location given by the integer.  Undoing an entry of this form places\n\
point at POSITION.\n\
\n\
nil marks undo boundaries.  The undo command treats the changes\n\
between two undo boundaries as a single step to be undone.\n\
\n\
If the value of the variable is t, undo information is not recorded.");
#endif
  DEFVAR_PER_BUFFER ("buffer-undo-list", &current_buffer->undo_list, Qnil,
    0);

  DEFVAR_PER_BUFFER ("mark-active", &current_buffer->mark_active, Qnil, 
    "Non-nil means the mark and region are currently active in this buffer.\n\
Automatically local in all buffers.");

  DEFVAR_PER_BUFFER ("cache-long-line-scans", &current_buffer->cache_long_line_scans, Qnil, 
    "Non-nil means that Emacs should use caches to handle long lines more quickly.\n\
This variable is buffer-local, in all buffers.\n\
\n\
Normally, the line-motion functions work by scanning the buffer for\n\
newlines.  Columnar operations (like move-to-column and\n\
compute-motion) also work by scanning the buffer, summing character\n\
widths as they go.  This works well for ordinary text, but if the\n\
buffer's lines are very long (say, more than 500 characters), these\n\
motion functions will take longer to execute.  Emacs may also take\n\
longer to update the display.\n\
\n\
If cache-long-line-scans is non-nil, these motion functions cache the\n\
results of their scans, and consult the cache to avoid rescanning\n\
regions of the buffer until the text is modified.  The caches are most\n\
beneficial when they prevent the most searching---that is, when the\n\
buffer contains long lines and large regions of characters with the\n\
same, fixed screen width.\n\
\n\
When cache-long-line-scans is non-nil, processing short lines will\n\
become slightly slower (because of the overhead of consulting the\n\
cache), and the caches will use memory roughly proportional to the\n\
number of newlines and characters whose screen width varies.\n\
\n\
The caches require no explicit maintenance; their accuracy is\n\
maintained internally by the Emacs primitives.  Enabling or disabling\n\
the cache should not affect the behavior of any of the motion\n\
functions; it should only affect their performance.");

  DEFVAR_PER_BUFFER ("point-before-scroll", &current_buffer->point_before_scroll, Qnil,
  "Value of point before the last series of scroll operations, or nil.\n\
This variable is always local in all buffers.");

  DEFVAR_PER_BUFFER ("buffer-file-format", &current_buffer->file_format, Qnil,
    "List of formats to use when saving this buffer.\n\
This variable is always local in all buffers.\n\
Formats are defined by `format-alist'.  This variable is\n\
set when a file is visited.  Automatically local in all buffers.");

  DEFVAR_PER_BUFFER ("buffer-invisibility-spec",
		     &current_buffer->invisibility_spec, Qnil,
  "Invisibility spec of this buffer.\n\
This variable is always local in all buffers.\n\
The default is t, which means that text is invisible\n\
if it has a non-nil `invisible' property.\n\
If the value is a list, a text character is invisible if its `invisible'\n\
property is an element in that list.\n\
If an element is a cons cell of the form (PROP . ELLIPSIS),\n\
then characters with property value PROP are invisible,\n\
and they have an ellipsis as well if ELLIPSIS is non-nil.");

  DEFVAR_PER_BUFFER ("buffer-display-count",
		     &current_buffer->display_count, Qnil,
  "A number incremented each time this buffer is displayed in a window.\n\
This variable is always local in all buffers.\n\
The function `set-window-buffer increments it.");

  DEFVAR_PER_BUFFER ("buffer-display-time",
		     &current_buffer->display_time, Qnil,
  "Time stamp updated each time this buffer is displayed in a window.\n\
This variable is always local in all buffers.\n\
The function `set-window-buffer' updates this variable\n\
to the value obtained by calling `current-time'.\n\
If the buffer has never been shown in a window, the value is nil.");

  DEFVAR_LISP ("transient-mark-mode", &Vtransient_mark_mode,
    "*Non-nil means deactivate the mark when the buffer contents change.\n\
Non-nil also enables highlighting of the region whenever the mark is active.\n\
The variable `highlight-nonselected-windows' controls whether to highlight\n\
all windows or just the selected window.");
  Vtransient_mark_mode = Qnil;

  DEFVAR_LISP ("inhibit-read-only", &Vinhibit_read_only,
    "*Non-nil means disregard read-only status of buffers or characters.\n\
If the value is t, disregard `buffer-read-only' and all `read-only'\n\
text properties.  If the value is a list, disregard `buffer-read-only'\n\
and disregard a `read-only' text property if the property value\n\
is a member of the list.");
  Vinhibit_read_only = Qnil;

  DEFVAR_PER_BUFFER ("cursor-type", &current_buffer->cursor_type, Qnil,
    "Cursor to use in window displaying this buffer.\n\
Values are interpreted as follows:\n\
\n\
  t 		use the cursor specified for the frame\n\
  nil		don't display a cursor\n\
  `bar'		display a bar cursor with default width\n\
  (bar . WIDTH)	display a bar cursor with width WIDTH\n\
  others	display a box cursor.");

  DEFVAR_PER_BUFFER ("line-spacing",
		     &current_buffer->extra_line_spacing, Qnil,
    "Additional space to put between lines when displaying a buffer.\n\
The space is measured in pixels, and put below lines on window systems.");

  DEFVAR_LISP ("kill-buffer-query-functions", &Vkill_buffer_query_functions,
    "List of functions called with no args to query before killing a buffer.");
  Vkill_buffer_query_functions = Qnil;

  defsubr (&Sbuffer_live_p);
  defsubr (&Sbuffer_list);
  defsubr (&Sget_buffer);
  defsubr (&Sget_file_buffer);
  defsubr (&Sget_buffer_create);
  defsubr (&Smake_indirect_buffer);
  defsubr (&Sgenerate_new_buffer_name);
  defsubr (&Sbuffer_name);
/*defsubr (&Sbuffer_number);*/
  defsubr (&Sbuffer_file_name);
  defsubr (&Sbuffer_base_buffer);
  defsubr (&Sbuffer_local_variables);
  defsubr (&Sbuffer_modified_p);
  defsubr (&Sset_buffer_modified_p);
  defsubr (&Sbuffer_modified_tick);
  defsubr (&Srename_buffer);
  defsubr (&Sother_buffer);
  defsubr (&Sbuffer_disable_undo);
  defsubr (&Sbuffer_enable_undo);
  defsubr (&Skill_buffer);
  defsubr (&Sset_buffer_major_mode);
  defsubr (&Sswitch_to_buffer);
  defsubr (&Spop_to_buffer);
  defsubr (&Scurrent_buffer);
  defsubr (&Sset_buffer);
  defsubr (&Sbarf_if_buffer_read_only);
  defsubr (&Sbury_buffer);
  defsubr (&Serase_buffer);
  defsubr (&Sset_buffer_multibyte);
  defsubr (&Skill_all_local_variables);

  defsubr (&Soverlayp);
  defsubr (&Smake_overlay);
  defsubr (&Sdelete_overlay);
  defsubr (&Smove_overlay);
  defsubr (&Soverlay_start);
  defsubr (&Soverlay_end);
  defsubr (&Soverlay_buffer);
  defsubr (&Soverlay_properties);
  defsubr (&Soverlays_at);
  defsubr (&Soverlays_in);
  defsubr (&Snext_overlay_change);
  defsubr (&Sprevious_overlay_change);
  defsubr (&Soverlay_recenter);
  defsubr (&Soverlay_lists);
  defsubr (&Soverlay_get);
  defsubr (&Soverlay_put);
  defsubr (&Srestore_buffer_modified_p);
}

void
keys_of_buffer ()
{
  initial_define_key (control_x_map, 'b', "switch-to-buffer");
  initial_define_key (control_x_map, 'k', "kill-buffer");

  /* This must not be in syms_of_buffer, because Qdisabled is not
     initialized when that function gets called.  */
  Fput (intern ("erase-buffer"), Qdisabled, Qt);
}
