/* Buffer manipulation primitives for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1989, 1993, 1994
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#ifndef MAXPATHLEN
/* in 4.1, param.h fails to define this. */
#define MAXPATHLEN 1024
#endif /* not MAXPATHLEN */

#include <config.h>
#include "lisp.h"
#include "intervals.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "region-cache.h"
#include "indent.h"
#include "blockinput.h"

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
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

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
   equal to that integer.  When a tag does not match, the function
   buffer_slot_type_mismatch will signal an error.  */
struct buffer buffer_local_types;

Lisp_Object Fset_buffer ();
void set_buffer_internal ();
static void call_overlay_mod_hooks ();

/* Alist of all buffer names vs the buffers. */
/* This used to be a variable, but is no longer,
 to prevent lossage due to user rplac'ing this alist or its elements.  */
Lisp_Object Vbuffer_alist;

/* Functions to call before and after each text change. */
Lisp_Object Vbefore_change_function;
Lisp_Object Vafter_change_function;
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

Lisp_Object Qfundamental_mode, Qmode_class, Qpermanent_local;

Lisp_Object Qprotected_field;

Lisp_Object QSFundamental;	/* A string "Fundamental" */

Lisp_Object Qkill_buffer_hook;

Lisp_Object Qget_file_buffer;

Lisp_Object Qoverlayp;

Lisp_Object Qpriority, Qwindow, Qevaporate;

Lisp_Object Qmodification_hooks;
Lisp_Object Qinsert_in_front_hooks;
Lisp_Object Qinsert_behind_hooks;

/* For debugging; temporary.  See set_buffer_internal.  */
/* Lisp_Object Qlisp_mode, Vcheck_symbol; */

nsberror (spec)
     Lisp_Object spec;
{
  if (STRINGP (spec))
    error ("No buffer named %s", XSTRING (spec)->data);
  error ("Invalid buffer argument");
}

DEFUN ("buffer-list", Fbuffer_list, Sbuffer_list, 0, 0, 0,
  "Return a list of all existing live buffers.")
  ()
{
  return Fmapcar (Qcdr, Vbuffer_alist);
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

  for (tail = Vbuffer_alist; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      buf = Fcdr (XCONS (tail)->car);
      if (!BUFFERP (buf)) continue;
      if (!STRINGP (XBUFFER (buf)->filename)) continue;
      tem = Fstring_equal (XBUFFER (buf)->filename, filename);
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

  b = (struct buffer *) xmalloc (sizeof (struct buffer));

  b->size = sizeof (struct buffer) / sizeof (EMACS_INT);

  /* An ordinary buffer uses its own struct buffer_text.  */
  b->text = &b->own_text;
  b->base_buffer = 0;

  BUF_GAP_SIZE (b) = 20;
  BLOCK_INPUT;
  BUFFER_ALLOC (BUF_BEG_ADDR (b), BUF_GAP_SIZE (b));
  UNBLOCK_INPUT;
  if (! BUF_BEG_ADDR (b))
    buffer_memory_full ();

  BUF_PT (b) = 1;
  BUF_GPT (b) = 1;
  BUF_BEGV (b) = 1;
  BUF_ZV (b) = 1;
  BUF_Z (b) = 1;
  BUF_MODIFF (b) = 1;
  BUF_SAVE_MODIFF (b) = 1;
  BUF_INTERVALS (b) = 0;

  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->width_table = Qnil;

  /* Put this on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;

  /* An ordinary buffer normally doesn't need markers
     to handle BEGV and ZV.  */
  b->pt_marker = Qnil;
  b->begv_marker = Qnil;
  b->zv_marker = Qnil;

  name = Fcopy_sequence (name);
  INITIALIZE_INTERVAL (XSTRING (name), NULL_INTERVAL);
  b->name = name;

  if (XSTRING (name)->data[0] != ' ')
    b->undo_list = Qnil;
  else
    b->undo_list = Qt;

  reset_buffer (b);
  reset_buffer_local_variables (b);

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buf, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buf), Qnil));

  b->mark = Fmake_marker ();
  BUF_MARKERS (b) = Qnil;
  b->name = name;
  return buf;
}

DEFUN ("make-indirect-buffer",
       Fmake_indirect_buffer, Smake_indirect_buffer, 2, 2,
       "BMake indirect buffer: \nbIndirect to base buffer: ",
  "Create and return an indirect buffer named NAME, with base buffer BASE.\n\
BASE should be an existing buffer (or buffer name).")
  (name, base_buffer)
     register Lisp_Object name, base_buffer;
{
  register Lisp_Object buf;
  register struct buffer *b;

  buf = Fget_buffer (name);
  if (!NILP (buf))
    error ("Buffer name `%s' is in use", XSTRING (name)->data);

  base_buffer = Fget_buffer (base_buffer);
  if (NILP (base_buffer))
    error ("No such buffer: `%s'",
	   XSTRING (XBUFFER (base_buffer)->name)->data);

  if (XSTRING (name)->size == 0)
    error ("Empty string for buffer name is not allowed");

  b = (struct buffer *) xmalloc (sizeof (struct buffer));

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

  b->newline_cache = 0;
  b->width_run_cache = 0;
  b->width_table = Qnil;

  /* Put this on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;

  name = Fcopy_sequence (name);
  INITIALIZE_INTERVAL (XSTRING (name), NULL_INTERVAL);
  b->name = name;

  reset_buffer (b);
  reset_buffer_local_variables (b);

  /* Put this in the alist of all live buffers.  */
  XSETBUFFER (buf, b);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buf), Qnil));

  b->mark = Fmake_marker ();
  b->name = name;

  /* Make sure the base buffer has markers for its narrowing.  */
  if (NILP (b->base_buffer->pt_marker))
    {
      b->base_buffer->pt_marker = Fmake_marker ();
      Fset_marker (b->base_buffer->pt_marker,
		   make_number (BUF_PT (b->base_buffer)), base_buffer);
    }
  if (NILP (b->base_buffer->begv_marker))
    {
      b->base_buffer->begv_marker = Fmake_marker ();
      Fset_marker (b->base_buffer->begv_marker,
		   make_number (BUF_BEGV (b->base_buffer)), base_buffer);
    }
  if (NILP (b->base_buffer->zv_marker))
    {
      b->base_buffer->zv_marker = Fmake_marker ();
      Fset_marker (b->base_buffer->zv_marker,
		   make_number (BUF_ZV (b->base_buffer)), base_buffer);
    }

  /* Give the indirect buffer markers for its narrowing.  */
  b->pt_marker = Fpoint_marker ();
  b->begv_marker = Fpoint_min_marker ();
  b->zv_marker = Fpoint_max_marker ();

  return buf;
}

/* Reinitialize everything about a buffer except its name and contents
   and local variables.  */

void
reset_buffer (b)
     register struct buffer *b;
{
  b->filename = Qnil;
  b->directory = (current_buffer) ? current_buffer->directory : Qnil;
  b->modtime = 0;
  XSETFASTINT (b->save_length, 0);
  b->last_window_start = 1;
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
}

/* Reset buffer B's local variables info.
   Don't use this on a buffer that has already been in use;
   it does not treat permanent locals consistently.
   Instead, use Fkill_all_local_variables.  */

reset_buffer_local_variables (b)
     register struct buffer *b;
{
  register int offset;

  /* Reset the major mode to Fundamental, together with all the
     things that depend on the major mode.
     default-major-mode is handled at a higher level.
     We ignore it here.  */
  b->major_mode = Qfundamental_mode;
  b->keymap = Qnil;
  b->abbrev_table = Vfundamental_mode_abbrev_table;
  b->mode_name = QSFundamental;
  b->minor_modes = Qnil;
  b->downcase_table = Vascii_downcase_table;
  b->upcase_table = Vascii_upcase_table;
  b->case_canon_table = Vascii_canon_table;
  b->case_eqv_table = Vascii_eqv_table;
  b->buffer_file_type = Qnil;
#if 0
  b->sort_table = XSTRING (Vascii_sort_table);
  b->folding_sort_table = XSTRING (Vascii_folding_sort_table);
#endif /* 0 */

  /* Reset all per-buffer variables to their defaults.  */
  b->local_var_alist = Qnil;
  b->local_var_flags = 0;

  /* For each slot that has a default value,
     copy that into the slot.  */

  for (offset = (char *)&buffer_local_flags.name - (char *)&buffer_local_flags;
       offset < sizeof (struct buffer);
       offset += sizeof (Lisp_Object)) /* sizeof EMACS_INT == sizeof Lisp_Object */
    {
      int flag = XINT (*(Lisp_Object *)(offset + (char *)&buffer_local_flags));
      if (flag > 0 || flag == -2)
	*(Lisp_Object *)(offset + (char *)b) =
	  *(Lisp_Object *)(offset + (char *)&buffer_defaults);
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
    for (tail = buf->local_var_alist; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	Lisp_Object val, elt;

	elt = XCONS (tail)->car;

	/* Reference each variable in the alist in buf.
	   If inquiring about the current buffer, this gets the current values,
	   so store them into the alist so the alist is up to date.
	   If inquiring about some other buffer, this swaps out any values
	   for that buffer, making the alist up to date automatically.  */
	val = find_symbol_value (XCONS (elt)->car);
	/* Use the current buffer value only if buf is the current buffer.  */
	if (buf != current_buffer)
	  val = XCONS (elt)->cdr;

	/* If symbol is unbound, put just the symbol in the list.  */
	if (EQ (val, Qunbound))
	  result = Fcons (XCONS (elt)->car, result);
	/* Otherwise, put (symbol . value) in the list.  */
	else
	  result = Fcons (Fcons (XCONS (elt)->car, val), result);
      }
  }

  /* Add on all the variables stored in special slots.  */
  {
    register int offset, mask;

    for (offset = (char *)&buffer_local_symbols.name - (char *)&buffer_local_symbols;
	 offset < sizeof (struct buffer);
	 offset += (sizeof (EMACS_INT))) /* sizeof EMACS_INT == sizeof Lisp_Object */
      {
	mask = XINT (*(Lisp_Object *)(offset + (char *)&buffer_local_flags));
	if (mask == -1 || (buf->local_var_flags & mask))
	  if (SYMBOLP (*(Lisp_Object *)(offset
					+ (char *)&buffer_local_symbols)))
	    result = Fcons (Fcons (*((Lisp_Object *)
				     (offset + (char *)&buffer_local_symbols)),
				   *(Lisp_Object *)(offset + (char *)buf)),
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

#ifdef CLASH_DETECTION
  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = current_buffer->filename;
  if (!NILP (fn))
    {
      already = SAVE_MODIFF < MODIFF;
      if (!already && !NILP (flag))
	lock_file (fn);
      else if (already && NILP (flag))
	unlock_file (fn);
    }
#endif /* CLASH_DETECTION */

  SAVE_MODIFF = NILP (flag) ? MODIFF : 0;
  update_mode_lines++;
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
  /* Don't short-circuit if UNIQUE is t.  That is a useful way to rename
     the buffer automatically so you can create another with the original name.
     It makes UNIQUE equivalent to
     (rename-buffer (generate-new-buffer-name NEWNAME)).  */
  if (NILP (unique) && XBUFFER (tem) == current_buffer)
    return current_buffer->name;
  if (!NILP (tem))
    {
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

DEFUN ("other-buffer", Fother_buffer, Sother_buffer, 0, 2, 0,
  "Return most recently selected buffer other than BUFFER.\n\
Buffers not visible in windows are preferred to visible buffers,\n\
unless optional second argument VISIBLE-OK is non-nil.\n\
If no other buffer exists, the buffer `*scratch*' is returned.\n\
If BUFFER is omitted or nil, some interesting buffer is returned.")
  (buffer, visible_ok)
     register Lisp_Object buffer, visible_ok;
{
  register Lisp_Object tail, buf, notsogood, tem;
  notsogood = Qnil;

  for (tail = Vbuffer_alist; !NILP (tail); tail = Fcdr (tail))
    {
      buf = Fcdr (Fcar (tail));
      if (EQ (buf, buffer))
	continue;
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
#ifdef MULTI_FRAME
      /* If the selected frame has a buffer_predicate,
	 disregard buffers that don't fit the predicate.  */
      tem = frame_buffer_predicate ();
      if (!NILP (tem))
	{
	  tem = call1 (tem, buf);
	  if (NILP (tem))
	    continue;
	}
#endif

      if (NILP (visible_ok))
	tem = Fget_buffer_window (buf, Qt);
      else
	tem = Qnil;
      if (NILP (tem))
	return buf;
      if (NILP (notsogood))
	notsogood = buf;
    }
  if (!NILP (notsogood))
    return notsogood;
  return Fget_buffer_create (build_string ("*scratch*"));
}

DEFUN ("buffer-disable-undo", Fbuffer_disable_undo, Sbuffer_disable_undo, 0, 1,
0,
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
with `delete-process'.")
  (bufname)
     Lisp_Object bufname;
{
  Lisp_Object buf;
  register struct buffer *b;
  register Lisp_Object tem;
  register struct Lisp_Marker *m;
  struct gcpro gcpro1, gcpro2;

  if (NILP (bufname))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (bufname);
  if (NILP (buf))
    nsberror (bufname);

  b = XBUFFER (buf);

  /* Query if the buffer is still modified.  */
  if (INTERACTIVE && !NILP (b->filename)
      && BUF_MODIFF (b) > BUF_SAVE_MODIFF (b))
    {
      GCPRO2 (buf, bufname);
      tem = do_yes_or_no_p (format1 ("Buffer %s modified; kill anyway? ",
				     XSTRING (b->name)->data));
      UNGCPRO;
      if (NILP (tem))
	return Qnil;
    }

  /* Run hooks with the buffer to be killed the current buffer.  */
  {
    register Lisp_Object val;
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
	if (other->base_buffer == b)
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
      tem = Fother_buffer (buf, Qnil);
      Fset_buffer (tem);
      if (b == current_buffer)
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
  Vbuffer_alist = Fdelq (Frassq (buf, Vbuffer_alist), Vbuffer_alist);
  Freplace_buffer_in_windows (buf);
  Vinhibit_quit = tem;

  /* Delete any auto-save file, if we saved it in this session.  */
  if (STRINGP (b->auto_save_file_name)
      && b->auto_save_modified != 0)
    {
      Lisp_Object tem;
      tem = Fsymbol_value (intern ("delete-auto-save-files"));
      if (! NILP (tem))
	internal_delete_file (b->auto_save_file_name);
    }

  if (! b->base_buffer)
    {
      /* Unchain all markers of this buffer
	 and leave them pointing nowhere.  */
      for (tem = BUF_MARKERS (b); !EQ (tem, Qnil); )
	{
	  m = XMARKER (tem);
	  m->buffer = 0;
	  tem = m->chain;
	  m->chain = Qnil;
	}
      BUF_MARKERS (b) = Qnil;

#ifdef USE_TEXT_PROPERTIES
      BUF_INTERVALS (b) = NULL_INTERVAL;
#endif

      /* Perhaps we should explicitly free the interval tree here... */
    }

  b->name = Qnil;

  BLOCK_INPUT;
  if (! b->base_buffer)
    BUFFER_FREE (BUF_BEG_ADDR (b));

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

record_buffer (buf)
     Lisp_Object buf;
{
  register Lisp_Object link, prev;

  prev = Qnil;
  for (link = Vbuffer_alist; CONSP (link); link = XCONS (link)->cdr)
    {
      if (EQ (XCONS (XCONS (link)->car)->cdr, buf))
	break;
      prev = link;
    }

  /* Effectively do Vbuffer_alist = Fdelq (link, Vbuffer_alist);
     we cannot use Fdelq itself here because it allows quitting.  */

  if (NILP (prev))
    Vbuffer_alist = XCONS (Vbuffer_alist)->cdr;
  else
    XCONS (prev)->cdr = XCONS (XCONS (prev)->cdr)->cdr;
	
  XCONS(link)->cdr = Vbuffer_alist;
  Vbuffer_alist = link;
}

DEFUN ("set-buffer-major-mode", Fset_buffer_major_mode, Sset_buffer_major_mode, 1, 1, 0,
  "Set an appropriate major mode for BUFFER, according to `default-major-mode'.\n\
Use this function before selecting the buffer, since it may need to inspect\n\
the current buffer's major mode.")
  (buf)
     Lisp_Object buf;
{
  int count;
  Lisp_Object function;

  function = buffer_defaults.major_mode;
  if (NILP (function) && NILP (Fget (current_buffer->major_mode, Qmode_class)))
    function = current_buffer->major_mode;

  if (NILP (function) || EQ (function, Qfundamental_mode))
    return Qnil;

  count = specpdl_ptr - specpdl;

  /* To select a nonfundamental mode,
     select the buffer temporarily and then call the mode function. */

  record_unwind_protect (save_excursion_restore, save_excursion_save ());

  Fset_buffer (buf);
  call0 (function);

  return unbind_to (count, Qnil);
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
  (bufname, norecord)
     Lisp_Object bufname, norecord;
{
  register Lisp_Object buf;
  Lisp_Object tem;

  if (EQ (minibuf_window, selected_window))
    error ("Cannot switch buffers in minibuffer window");
  tem = Fwindow_dedicated_p (selected_window);
  if (!NILP (tem))
    error ("Cannot switch buffers in a dedicated window");

  if (NILP (bufname))
    buf = Fother_buffer (Fcurrent_buffer (), Qnil);
  else
    {
      buf = Fget_buffer (bufname);
      if (NILP (buf))
	{
	  buf = Fget_buffer_create (bufname);
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

DEFUN ("pop-to-buffer", Fpop_to_buffer, Spop_to_buffer, 1, 2, 0,
  "Select buffer BUFFER in some window, preferably a different one.\n\
If BUFFER is nil, then some other buffer is chosen.\n\
If `pop-up-windows' is non-nil, windows can be split to do this.\n\
If optional second arg OTHER-WINDOW is non-nil, insist on finding another\n\
window even if BUFFER is already visible in the selected window.")
  (bufname, other)
     Lisp_Object bufname, other;
{
  register Lisp_Object buf;
  if (NILP (bufname))
    buf = Fother_buffer (Fcurrent_buffer (), Qnil);
  else
    buf = Fget_buffer_create (bufname);
  Fset_buffer (buf);
  record_buffer (buf);
  Fselect_window (Fdisplay_buffer (buf, other));
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

/* Set the current buffer to b */

void
set_buffer_internal (b)
     register struct buffer *b;
{
  register struct buffer *old_buf;
  register Lisp_Object tail, valcontents;
  Lisp_Object tem;

  if (current_buffer == b)
    return;

  windows_or_buffers_changed = 1;
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
	  Fset_marker (old_buf->pt_marker, BUF_PT (old_buf), obuf);
	}
      if (! NILP (old_buf->begv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  Fset_marker (old_buf->begv_marker, BUF_BEGV (old_buf), obuf);
	}
      if (! NILP (old_buf->zv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  Fset_marker (old_buf->zv_marker, BUF_ZV (old_buf), obuf);
	}
    }

  /* Get the undo list from the base buffer, so that it appears
     that an indirect buffer shares the undo list of its base.  */
  if (b->base_buffer)
    b->undo_list = b->base_buffer->undo_list;

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  if (! NILP (b->pt_marker))
    BUF_PT (b) = marker_position (b->pt_marker);
  if (! NILP (b->begv_marker))
    BUF_BEGV (b) = marker_position (b->begv_marker);
  if (! NILP (b->zv_marker))
    BUF_ZV (b) = marker_position (b->zv_marker);

  /* Look down buffer's list of local Lisp variables
     to find and update any that forward into C variables. */

  for (tail = b->local_var_alist; !NILP (tail); tail = XCONS (tail)->cdr)
    {
      valcontents = XSYMBOL (XCONS (XCONS (tail)->car)->car)->value;
      if ((BUFFER_LOCAL_VALUEP (valcontents)
	   || SOME_BUFFER_LOCAL_VALUEP (valcontents))
	  && (tem = XBUFFER_LOCAL_VALUE (valcontents)->car,
	      (BOOLFWDP (tem) || INTFWDP (tem) || OBJFWDP (tem))))
	/* Just reference the variable
	     to cause it to become set for this buffer.  */
	Fsymbol_value (XCONS (XCONS (tail)->car)->car);
    }

  /* Do the same with any others that were local to the previous buffer */

  if (old_buf)
    for (tail = old_buf->local_var_alist; !NILP (tail); tail = XCONS (tail)->cdr)
      {
	valcontents = XSYMBOL (XCONS (XCONS (tail)->car)->car)->value;
	if ((BUFFER_LOCAL_VALUEP (valcontents)
	     || SOME_BUFFER_LOCAL_VALUEP (valcontents))
	    && (tem = XBUFFER_LOCAL_VALUE (valcontents)->car,
		(BOOLFWDP (tem) || INTFWDP (tem) || OBJFWDP (tem))))
	  /* Just reference the variable
               to cause it to become set for this buffer.  */
	  Fsymbol_value (XCONS (XCONS (tail)->car)->car);
      }
}

/* Switch to buffer B temporarily for redisplay purposes.
   This avoids certain things thatdon't need to be done within redisplay.  */

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
	  Fset_marker (old_buf->pt_marker, BUF_PT (old_buf), obuf);
	}
      if (! NILP (old_buf->begv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  Fset_marker (old_buf->begv_marker, BUF_BEGV (old_buf), obuf);
	}
      if (! NILP (old_buf->zv_marker))
	{
	  Lisp_Object obuf;
	  XSETBUFFER (obuf, old_buf);
	  Fset_marker (old_buf->zv_marker, BUF_ZV (old_buf), obuf);
	}
    }

  /* If the new current buffer has markers to record PT, BEGV and ZV
     when it is not current, fetch them now.  */
  if (! NILP (b->pt_marker))
    BUF_PT (b) = marker_position (b->pt_marker);
  if (! NILP (b->begv_marker))
    BUF_BEGV (b) = marker_position (b->begv_marker);
  if (! NILP (b->zv_marker))
    BUF_ZV (b) = marker_position (b->zv_marker);
}

DEFUN ("set-buffer", Fset_buffer, Sset_buffer, 1, 1, 0,
  "Make the buffer BUFFER current for editing operations.\n\
BUFFER may be a buffer or the name of an existing buffer.\n\
See also `save-excursion' when you want to make a buffer current temporarily.\n\
This function does not display the buffer, so its effect ends\n\
when the current command terminates.\n\
Use `switch-to-buffer' or `pop-to-buffer' to switch buffers permanently.")
  (bufname)
     register Lisp_Object bufname;
{
  register Lisp_Object buffer;
  buffer = Fget_buffer (bufname);
  if (NILP (buffer))
    nsberror (bufname);
  if (NILP (XBUFFER (buffer)->name))
    error ("Selecting deleted buffer");
  set_buffer_internal (XBUFFER (buffer));
  return buffer;
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
  (buf)
     register Lisp_Object buf;
{
  /* Figure out what buffer we're going to bury.  */
  if (NILP (buf))
    {
      XSETBUFFER (buf, current_buffer);

      /* If we're burying the current buffer, unshow it.  */
      Fswitch_to_buffer (Fother_buffer (buf, Qnil), Qnil);
    }
  else
    {
      Lisp_Object buf1;
      
      buf1 = Fget_buffer (buf);
      if (NILP (buf1))
	nsberror (buf);
      buf = buf1;
    }

  /* Move buf to the end of the buffer list.  */
  {
    register Lisp_Object aelt, link;

    aelt = Frassq (buf, Vbuffer_alist);
    link = Fmemq (aelt, Vbuffer_alist);
    Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
    XCONS (link)->cdr = Qnil;
    Vbuffer_alist = nconc2 (Vbuffer_alist, link);
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

  /* Make sure no local variables remain set up with this buffer
     for their current values.  */

  for (alist = oalist; !NILP (alist); alist = XCONS (alist)->cdr)
    {
      sym = XCONS (XCONS (alist)->car)->car;

      /* Need not do anything if some other buffer's binding is now encached.  */
      tem = XCONS (XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->cdr)->car;
      if (XBUFFER (tem) == current_buffer)
	{
	  /* Symbol is set up for this buffer's old local value.
	     Set it up for the current buffer with the default value.  */

	  tem = XCONS (XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->cdr)->cdr;
	  /* Store the symbol's current value into the alist entry
	     it is currently set up for.  This is so that, if the
	     local is marked permanent, and we make it local again below,
	     we don't lose the value.  */
	  XCONS (XCONS (tem)->car)->cdr
	    = do_symval_forwarding (XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->car);
	  /* Switch to the symbol's default-value alist entry.  */
	  XCONS (tem)->car = tem;
	  /* Mark it as current for the current buffer.  */
	  XCONS (XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->cdr)->car
	    = Fcurrent_buffer ();
	  /* Store the current value into any forwarding in the symbol.  */
	  store_symval_forwarding (sym, XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->car,
				   XCONS (tem)->cdr);
	}
    }

  /* Actually eliminate all local bindings of this buffer.  */

  reset_buffer_local_variables (current_buffer);

  /* Redisplay mode lines; we are changing major mode.  */

  update_mode_lines++;

  /* Any which are supposed to be permanent,
     make local again, with the same values they had.  */
     
  for (alist = oalist; !NILP (alist); alist = XCONS (alist)->cdr)
    {
      sym = XCONS (XCONS (alist)->car)->car;
      tem = Fget (sym, Qpermanent_local);
      if (! NILP (tem))
	{
	  Fmake_local_variable (sym);
	  Fset (sym, XCONS (XCONS (alist)->car)->cdr);
	}
    }

  /* Force mode-line redisplay.  Useful here because all major mode
     commands call this function.  */
  update_mode_lines++;

  return Qnil;
}

/* Find all the overlays in the current buffer that contain position POS.
   Return the number found, and store them in a vector in *VEC_PTR.  
   Store in *LEN_PTR the size allocated for the vector.
   Store in *NEXT_PTR the next position after POS where an overlay starts,
     or ZV if there are no more overlays.
   Store in *PREV_PTR the previous position after POS where an overlay ends,
     or BEGV if there are no previous overlays.
   NEXT_PTR and/or PREV_PTR may be 0, meaning don't store that info.

   *VEC_PTR and *LEN_PTR should contain a valid vector and size
   when this function is called.

   If EXTEND is non-zero, we make the vector bigger if necessary.
   If EXTEND is zero, we never extend the vector,
   and we store only as many overlays as will fit.
   But we still return the total number of overlays.  */

int
overlays_at (pos, extend, vec_ptr, len_ptr, next_ptr, prev_ptr)
     int pos;
     int extend;
     Lisp_Object **vec_ptr;
     int *len_ptr;
     int *next_ptr;
     int *prev_ptr;
{
  Lisp_Object tail, overlay, start, end, result;
  int idx = 0;
  int len = *len_ptr;
  Lisp_Object *vec = *vec_ptr;
  int next = ZV;
  int prev = BEGV;
  int inhibit_storing = 0;

  for (tail = current_buffer->overlays_before;
       GC_CONSP (tail);
       tail = XCONS (tail)->cdr)
    {
      int startpos, endpos;

      overlay = XCONS (tail)->car;

      start = OVERLAY_START (overlay);
      end = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (end);
      if (endpos < pos)
	{
	  if (prev < endpos)
	    prev = endpos;
	  break;
	}
      if (endpos == pos)
	continue;
      startpos = OVERLAY_POSITION (start);
      if (startpos <= pos)
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
       tail = XCONS (tail)->cdr)
    {
      int startpos, endpos;

      overlay = XCONS (tail)->car;

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
      else if (endpos < pos && endpos > prev)
	prev = endpos;
    }

  if (next_ptr)
    *next_ptr = next;
  if (prev_ptr)
    *prev_ptr = prev;
  return idx;
}

struct sortvec
{
  Lisp_Object overlay;
  int beg, end;
  int priority;
};

static int
compare_overlays (s1, s2)
     struct sortvec *s1, *s2;
{
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
      next = XCONS (tail)->cdr;
      overlay = XCONS (tail)->car;

      /* If the overlay is not valid, get rid of it.  */
      if (!OVERLAY_VALID (overlay))
#if 1
	abort ();
#else
	{
	  /* Splice the cons cell TAIL out of overlays_before.  */
	  if (!NILP (prev))
	    XCONS (prev)->cdr = next;
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
	    XCONS (prev)->cdr = next;
	  else
	    buf->overlays_before = next;

	  /* Search thru overlays_after for where to put it.  */
	  other_prev = Qnil;
	  for (other = buf->overlays_after;
	       CONSP (other);
	       other_prev = other, other = XCONS (other)->cdr)
	    {
	      Lisp_Object otherbeg, otheroverlay, follower;
	      int win;

	      otheroverlay = XCONS (other)->car;
	      if (! OVERLAY_VALID (otheroverlay))
		abort ();

	      otherbeg = OVERLAY_START (otheroverlay);
	      if (OVERLAY_POSITION (otherbeg) >= where)
		break;
	    }

	  /* Add TAIL to overlays_after before OTHER.  */
	  XCONS (tail)->cdr = other;
	  if (!NILP (other_prev))
	    XCONS (other_prev)->cdr = tail;
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
      next = XCONS (tail)->cdr;
      overlay = XCONS (tail)->car;

      /* If the overlay is not valid, get rid of it.  */
      if (!OVERLAY_VALID (overlay))
#if 1
	abort ();
#else
	{
	  /* Splice the cons cell TAIL out of overlays_after.  */
	  if (!NILP (prev))
	    XCONS (prev)->cdr = next;
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
	    XCONS (prev)->cdr = next;
	  else
	    buf->overlays_after = next;

	  /* Search thru overlays_before for where to put it.  */
	  other_prev = Qnil;
	  for (other = buf->overlays_before;
	       CONSP (other);
	       other_prev = other, other = XCONS (other)->cdr)
	    {
	      Lisp_Object otherend, otheroverlay;
	      int win;

	      otheroverlay = XCONS (other)->car;
	      if (! OVERLAY_VALID (otheroverlay))
		abort ();

	      otherend = OVERLAY_END (otheroverlay);
	      if (OVERLAY_POSITION (otherend) <= where)
		break;
	    }

	  /* Add TAIL to overlays_before before OTHER.  */
	  XCONS (tail)->cdr = other;
	  if (!NILP (other_prev))
	    XCONS (other_prev)->cdr = tail;
	  else
	    buf->overlays_before = tail;
	  tail = prev;
	}
    }

  XSETFASTINT (buf->overlay_center, pos);
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
  Lisp_Object tem, overlay;
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
      overlay = XCONS (*ptail)->car;
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
	      Fset_marker (OVERLAY_START (overlay), endpos, Qnil);
	      Fset_marker (OVERLAY_END (overlay), startpos, Qnil);
	      tem = startpos; startpos = endpos; endpos = tem;
	    }
	  /* Add it to the end of the wrong list.  Later on,
	     recenter_overlay_lists will move it to the right place.  */
	  if (endpos < XINT (current_buffer->overlay_center))
	    {
	      *pafter = *ptail;
	      pafter = &XCONS (*ptail)->cdr;
	    }
	  else
	    {
	      *pbefore = *ptail;
	      pbefore = &XCONS (*ptail)->cdr;
	    }
	  *ptail = XCONS (*ptail)->cdr;
	}
      else
	ptail = &XCONS (*ptail)->cdr;
    }
  for (ptail = &current_buffer->overlays_after; CONSP (*ptail);)
    {
      overlay = XCONS (*ptail)->car;
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
	      Fset_marker (OVERLAY_START (overlay), endpos, Qnil);
	      Fset_marker (OVERLAY_END (overlay), startpos, Qnil);
	      tem = startpos; startpos = endpos; endpos = tem;
	    }
	  if (endpos < XINT (current_buffer->overlay_center))
	    {
	      *pafter = *ptail;
	      pafter = &XCONS (*ptail)->cdr;
	    }
	  else
	    {
	      *pbefore = *ptail;
	      pbefore = &XCONS (*ptail)->cdr;
	    }
	  *ptail = XCONS (*ptail)->cdr;
	}
      else
	ptail = &XCONS (*ptail)->cdr;
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

DEFUN ("overlayp", Foverlayp, Soverlayp, 1, 1, 0,
  "Return t if OBJECT is an overlay.")
  (object)
     Lisp_Object object;
{
  return (OVERLAYP (object) ? Qt : Qnil);
}

DEFUN ("make-overlay", Fmake_overlay, Smake_overlay, 2, 3, 0,
  "Create a new overlay with range BEG to END in BUFFER.\n\
If omitted, BUFFER defaults to the current buffer.\n\
BEG and END may be integers or markers.")
  (beg, end, buffer)
     Lisp_Object beg, end, buffer;
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

  overlay = allocate_misc ();
  XMISC (overlay)->type = Lisp_Misc_Overlay;
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
  ob = XBUFFER (obuffer);

  /* If the overlay has changed buffers, do a thorough redisplay.  */
  if (!EQ (buffer, obuffer))
    {
      /* Redisplay where the overlay was.  */
      if (!NILP (obuffer))
	{
	  Lisp_Object o_beg;
	  Lisp_Object o_end;

	  o_beg = OVERLAY_START (overlay);
	  o_end = OVERLAY_END   (overlay);
	  o_beg = OVERLAY_POSITION (o_beg);
	  o_end = OVERLAY_POSITION (o_end);

	  redisplay_region (ob, XINT (o_beg), XINT (o_end));
	}

      /* Redisplay where the overlay is going to be.  */
      redisplay_region (b, XINT (beg), XINT (end));

      /* Don't limit redisplay to the selected window.  */
      windows_or_buffers_changed = 1;
    }
  else
    /* Redisplay the area the overlay has just left, or just enclosed.  */
    {
      Lisp_Object o_beg;
      Lisp_Object o_end;
      int change_beg, change_end;

      o_beg = OVERLAY_START (overlay);
      o_end = OVERLAY_END   (overlay);
      o_beg = OVERLAY_POSITION (o_beg);
      o_end = OVERLAY_POSITION (o_end);

      if (XINT (o_beg) == XINT (beg))
	redisplay_region (b, XINT (o_end), XINT (end));
      else if (XINT (o_end) == XINT (end))
	redisplay_region (b, XINT (o_beg), XINT (beg));
      else
	{
	  if (XINT (beg) < XINT (o_beg)) o_beg = beg;
	  if (XINT (end) > XINT (o_end)) o_end = end;
	  redisplay_region (b, XINT (o_beg), XINT (o_end));
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
  b->overlays_after  = Fdelq (overlay, b->overlays_after);

  redisplay_region (b,
		    marker_position (OVERLAY_START (overlay)),
		    marker_position (OVERLAY_END   (overlay)));

  Fset_marker (OVERLAY_START (overlay), Qnil, Qnil);
  Fset_marker (OVERLAY_END   (overlay), Qnil, Qnil);

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
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len, NULL, NULL);

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
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len, &endpos, NULL);

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
If there are no more overlay boundaries after POS, return (point-min).")
  (pos)
     Lisp_Object pos;
{
  int noverlays;
  int prevpos;
  Lisp_Object *overlay_vec;
  int len;
  int i;

  CHECK_NUMBER_COERCE_MARKER (pos, 0);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

  /* Put all the overlays we want in a vector in overlay_vec.
     Store the length in len.
     prevpos gets the position of an overlay end.  */
  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len, NULL, &prevpos);

  /* If any of these overlays starts before endpos,
     maybe use its starting point instead.  */
  for (i = 0; i < noverlays; i++)
    {
      Lisp_Object ostart;
      int ostartpos;

      ostart = OVERLAY_START (overlay_vec[i]);
      ostartpos = OVERLAY_POSITION (ostart);
      if (ostartpos > prevpos && ostartpos < XINT (pos))
	prevpos = ostartpos;
    }

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
  "Get the property of overlay OVERLAY with property name NAME.")
  (overlay, prop)
     Lisp_Object overlay, prop;
{
  Lisp_Object plist, fallback;

  CHECK_OVERLAY (overlay, 0);

  fallback = Qnil;

  for (plist = XOVERLAY (overlay)->plist;
       CONSP (plist) && CONSP (XCONS (plist)->cdr);
       plist = XCONS (XCONS (plist)->cdr)->cdr)
    {
      if (EQ (XCONS (plist)->car, prop))
	return XCONS (XCONS (plist)->cdr)->car;
      else if (EQ (XCONS (plist)->car, Qcategory))
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
       CONSP (tail) && CONSP (XCONS (tail)->cdr);
       tail = XCONS (XCONS (tail)->cdr)->cdr)
    if (EQ (XCONS (tail)->car, prop))
      {
	changed = !EQ (XCONS (XCONS (tail)->cdr)->car, value);
	XCONS (XCONS (tail)->cdr)->car = value;
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
	redisplay_region (XBUFFER (buffer),
			  marker_position (OVERLAY_START (overlay)),
			  marker_position (OVERLAY_END   (overlay)));
      if (EQ (prop, Qevaporate) && ! NILP (value)
	  && (OVERLAY_POSITION (OVERLAY_START (overlay))
	      == OVERLAY_POSITION (OVERLAY_END (overlay))))
	Fdelete_overlay (overlay);
    }
  return value;
}

/* Run the modification-hooks of overlays that include
   any part of the text in START to END.
   Run the insert-before-hooks of overlay starting at END,
   and the insert-after-hooks of overlay ending at START.

   This is called both before and after the modification.
   AFTER is nonzero when we call after the modification.

   ARG1, ARG2, ARG3 are arguments to pass to the hook functions.  */

void
report_overlay_modification (start, end, after, arg1, arg2, arg3)
     Lisp_Object start, end;
     int after;
     Lisp_Object arg1, arg2, arg3;
{
  Lisp_Object prop, overlay, tail;
  int insertion = EQ (start, end);
  int tail_copied;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

  overlay = Qnil;
  tail = Qnil;
  GCPRO5 (overlay, tail, arg1, arg2, arg3);

  tail_copied = 0;
  for (tail = current_buffer->overlays_before;
       CONSP (tail);
       tail = XCONS (tail)->cdr)
    {
      int startpos, endpos;
      Lisp_Object ostart, oend;

      overlay = XCONS (tail)->car;

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      endpos = OVERLAY_POSITION (oend);
      if (XFASTINT (start) > endpos)
	break;
      startpos = OVERLAY_POSITION (ostart);
      if (XFASTINT (end) == startpos && insertion)
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
      if (XFASTINT (start) == endpos && insertion)
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
       tail = XCONS (tail)->cdr)
    {
      int startpos, endpos;
      Lisp_Object ostart, oend;

      overlay = XCONS (tail)->car;

      ostart = OVERLAY_START (overlay);
      oend = OVERLAY_END (overlay);
      startpos = OVERLAY_POSITION (ostart);
      endpos = OVERLAY_POSITION (oend);
      if (XFASTINT (end) < startpos)
	break;
      if (XFASTINT (end) == startpos && insertion)
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
      if (XFASTINT (start) == endpos && insertion)
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
	 tail = XCONS (tail)->cdr)
      {
	int endpos;
	overlay = XCONS (tail)->car;
	endpos = OVERLAY_POSITION (OVERLAY_END (overlay));
	if (endpos < pos)
	  break;
	if (endpos == pos && OVERLAY_POSITION (OVERLAY_START (overlay)) == pos
	    && Foverlay_get (overlay, Qevaporate))
	  hit_list = Fcons (overlay, hit_list);
      }
  else
    for (tail = current_buffer->overlays_after; CONSP (tail);
	 tail = XCONS (tail)->cdr)
      {
	int startpos;
	overlay = XCONS (tail)->car;
	startpos = OVERLAY_POSITION (OVERLAY_START (overlay));
	if (startpos > pos)
	  break;
	if (startpos == pos && OVERLAY_POSITION (OVERLAY_END (overlay)) == pos
	    && Foverlay_get (overlay, Qevaporate))
	  hit_list = Fcons (overlay, hit_list);
      }
  for (; CONSP (hit_list); hit_list = XCONS (hit_list)->cdr)
    Fdelete_overlay (XCONS (hit_list)->car);
}

/* Somebody has tried to store a value with an unacceptable type
   into the buffer-local slot with offset OFFSET.  */
void
buffer_slot_type_mismatch (offset)
     int offset;
{
  Lisp_Object sym;
  char *type_name;
  sym = *(Lisp_Object *)(offset + (char *)&buffer_local_symbols);
  switch (XINT (*(Lisp_Object *)(offset + (char *)&buffer_local_types)))
    {
    case Lisp_Int:	type_name = "integers";  break;
    case Lisp_String:	type_name = "strings";   break;
    case Lisp_Symbol:	type_name = "symbols";   break;
    default:
      abort ();
    }

  error ("only %s should be stored in the buffer-local variable %s",
	 type_name, XSYMBOL (sym)->name->data);
}

init_buffer_once ()
{
  register Lisp_Object tem;

  /* Make sure all markable slots in buffer_defaults
     are initialized reasonably, so mark_buffer won't choke.  */
  reset_buffer (&buffer_defaults);
  reset_buffer_local_variables (&buffer_defaults);
  reset_buffer (&buffer_local_symbols);
  reset_buffer_local_variables (&buffer_local_symbols);
  /* Prevent GC from getting confused.  */
  buffer_defaults.text = &buffer_defaults.own_text;
  buffer_local_symbols.text = &buffer_local_symbols.own_text;
#ifdef USE_TEXT_PROPERTIES
  BUF_INTERVALS (&buffer_defaults) = 0;
  BUF_INTERVALS (&buffer_local_symbols) = 0;
#endif
  XSETBUFFER (Vbuffer_defaults, &buffer_defaults);
  XSETBUFFER (Vbuffer_local_symbols, &buffer_local_symbols);

  /* Set up the default values of various buffer slots.  */
  /* Must do these before making the first buffer! */

  /* real setup is done in loaddefs.el */
  buffer_defaults.mode_line_format = build_string ("%-");
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
  buffer_defaults.overlays_before = Qnil;
  buffer_defaults.overlays_after = Qnil;
  XSETFASTINT (buffer_defaults.overlay_center, 1);

  XSETFASTINT (buffer_defaults.tab_width, 8);
  buffer_defaults.truncate_lines = Qnil;
  buffer_defaults.ctl_arrow = Qt;

#ifdef DOS_NT
  buffer_defaults.buffer_file_type = Qnil; /* TEXT */
#endif
  XSETFASTINT (buffer_defaults.fill_column, 70);
  XSETFASTINT (buffer_defaults.left_margin, 0);
  buffer_defaults.cache_long_line_scans = Qnil;

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

  XSETFASTINT (buffer_local_flags.mode_line_format, 1);
  XSETFASTINT (buffer_local_flags.abbrev_mode, 2);
  XSETFASTINT (buffer_local_flags.overwrite_mode, 4);
  XSETFASTINT (buffer_local_flags.case_fold_search, 8);
  XSETFASTINT (buffer_local_flags.auto_fill_function, 0x10);
  XSETFASTINT (buffer_local_flags.selective_display, 0x20);
#ifndef old
  XSETFASTINT (buffer_local_flags.selective_display_ellipses, 0x40);
#endif
  XSETFASTINT (buffer_local_flags.tab_width, 0x80);
  XSETFASTINT (buffer_local_flags.truncate_lines, 0x100);
  XSETFASTINT (buffer_local_flags.ctl_arrow, 0x200);
  XSETFASTINT (buffer_local_flags.fill_column, 0x400);
  XSETFASTINT (buffer_local_flags.left_margin, 0x800);
  XSETFASTINT (buffer_local_flags.abbrev_table, 0x1000);
  XSETFASTINT (buffer_local_flags.display_table, 0x2000);
  XSETFASTINT (buffer_local_flags.syntax_table, 0x8000);
  XSETFASTINT (buffer_local_flags.cache_long_line_scans, 0x10000);
#ifdef DOS_NT
  XSETFASTINT (buffer_local_flags.buffer_file_type, 0x4000);
#endif

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
}

init_buffer ()
{
  char buf[MAXPATHLEN+1];
  char *pwd;
  struct stat dotstat, pwdstat;
  Lisp_Object temp;
  int rc;

  Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));

  /* If PWD is accurate, use it instead of calling getwd.  This is faster
     when PWD is right, and may avoid a fatal error.  */
  if ((pwd = getenv ("PWD")) != 0 && IS_DIRECTORY_SEP (*pwd)
      && stat (pwd, &pwdstat) == 0
      && stat (".", &dotstat) == 0
      && dotstat.st_ino == pwdstat.st_ino
      && dotstat.st_dev == pwdstat.st_dev
      && strlen (pwd) < MAXPATHLEN)
    strcpy (buf, pwd);
  else if (getwd (buf) == 0)
    fatal ("`getwd' failed: %s\n", buf);

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

  temp = get_minibuffer (0);
  XBUFFER (temp)->directory = current_buffer->directory;
}

/* initialize the buffer routines */
syms_of_buffer ()
{
  extern Lisp_Object Qdisabled;

  staticpro (&Vbuffer_defaults);
  staticpro (&Vbuffer_local_symbols);
  staticpro (&Qfundamental_mode);
  staticpro (&Qmode_class);
  staticpro (&QSFundamental);
  staticpro (&Vbuffer_alist);
  staticpro (&Qprotected_field);
  staticpro (&Qpermanent_local);
  staticpro (&Qkill_buffer_hook);
  staticpro (&Qoverlayp);
  Qevaporate = intern ("evaporate");
  staticpro (&Qevaporate);
  staticpro (&Qmodification_hooks);
  Qmodification_hooks = intern ("modification-hooks");
  staticpro (&Qinsert_in_front_hooks);
  Qinsert_in_front_hooks = intern ("insert-in-front-hooks");
  staticpro (&Qinsert_behind_hooks);
  Qinsert_behind_hooks = intern ("insert-behind-hooks");
  staticpro (&Qget_file_buffer);
  Qget_file_buffer = intern ("get-file-buffer");
  Qpriority = intern ("priority");
  staticpro (&Qpriority);
  Qwindow = intern ("window");
  staticpro (&Qwindow);

  Qoverlayp = intern ("overlayp");

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

  DEFVAR_LISP_NOPRO ("default-abbrev-mode",
	      &buffer_defaults.abbrev_mode,
    "Default value of `abbrev-mode' for buffers that do not override it.\n\
This is the same as (default-value 'abbrev-mode).");

  DEFVAR_LISP_NOPRO ("default-ctl-arrow",
	      &buffer_defaults.ctl_arrow,
    "Default value of `ctl-arrow' for buffers that do not override it.\n\
This is the same as (default-value 'ctl-arrow).");

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

  DEFVAR_PER_BUFFER ("mode-line-format", &current_buffer->mode_line_format, 
		     Qnil, 0);

/* This doc string is too long for cpp; cpp dies if it isn't in a comment.
   But make-docfile finds it!
  DEFVAR_PER_BUFFER ("mode-line-format", &current_buffer->mode_line_format,
    Qnil,
    "Template for displaying mode line for current buffer.\n\
Each buffer has its own value of this variable.\n\
Value may be a string, a symbol or a list or cons cell.\n\
For a symbol, its value is used (but it is ignored if t or nil).\n\
 A string appearing directly as the value of a symbol is processed verbatim\n\
 in that the %-constructs below are not recognized.\n\
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
  %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.\n\
	% means buffer is read-only and * means it is modified.\n\
	For a modified read-only buffer, %* gives % and %+ gives *.\n\
  %s -- print process status.   %l -- print the current line number.\n\
  %p -- print percent of buffer above top of window, or Top, Bot or All.\n\
  %P -- print percent of buffer above bottom of window, perhaps plus Top,\n\
        or print Bottom or All.\n\
  %n -- print Narrow if appropriate.\n\
  %t -- print T if files is text, B if binary.\n\
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
    "*Non-nil if searches should ignore case.\n\
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
Nil means use backslash and octal digits.\n\
Automatically becomes buffer-local when set in any fashion.\n\
This variable does not apply to characters whose display is specified\n\
in the current display table (if there is one).");

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
Each buffer has its own value of this variable.");

  DEFVAR_PER_BUFFER ("auto-fill-function", &current_buffer->auto_fill_function,
		     Qnil,
    "Function called (if non-nil) to perform auto-fill.\n\
It is called after self-inserting a space at a column beyond `fill-column'.\n\
Each buffer has its own value of this variable.\n\
NOTE: This variable is not an ordinary hook;\n\
It may not be a list of functions.");

  DEFVAR_PER_BUFFER ("buffer-file-name", &current_buffer->filename,
		     make_number (Lisp_String),
    "Name of file visited in current buffer, or nil if not visiting a file.\n\
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
The display table is a vector created with `make-display-table'.\n\
The first 256 elements control how to display each possible text character.\n\
Each value should be a vector of characters or nil;\n\
nil means display the character in the default fashion.\n\
The remaining six elements control the display of\n\
  the end of a truncated screen line (element 256, a single character);\n\
  the end of a continued line (element 257, a single character);\n\
  the escape character used to display character codes in octal\n\
    (element 258, a single character);\n\
  the character used as an arrow for control characters (element 259,\n\
    a single character);\n\
  the decoration indicating the presence of invisible lines (element 260,\n\
    a vector of characters);\n\
  the character used to draw the border between side-by-side windows\n\
    (element 261, a single character).\n\
If this variable is nil, the value of `standard-display-table' is used.\n\
Each window can have its own, overriding display table.");
#endif
  DEFVAR_PER_BUFFER ("buffer-display-table", &current_buffer->display_table,
		     Qnil, 0);

/*DEFVAR_LISP ("debug-check-symbol", &Vcheck_symbol,
    "Don't ask.");
*/
  DEFVAR_LISP ("before-change-function", &Vbefore_change_function,
	       "Function to call before each text change.\n\
Two arguments are passed to the function: the positions of\n\
the beginning and end of the range of old text to be changed.\n\
\(For an insertion, the beginning and end are at the same place.)\n\
No information is given about the length of the text after the change.\n\
\n\
Buffer changes made while executing the `before-change-function'\n\
don't call any before-change or after-change functions.\n\
That's because these variables are temporarily set to nil.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
these variables.  See the Emacs Lisp manual for a way of\n\
accomplishing an equivalent result by using other variables.");
  Vbefore_change_function = Qnil;

  DEFVAR_LISP ("after-change-function", &Vafter_change_function,
	       "Function to call after each text change.\n\
Three arguments are passed to the function: the positions of\n\
the beginning and end of the range of changed text,\n\
and the length of the pre-change text replaced by that range.\n\
\(For an insertion, the pre-change length is zero;\n\
for a deletion, that length is the number of characters deleted,\n\
and the post-change beginning and end are at the same place.)\n\
\n\
Buffer changes made while executing the `after-change-function'\n\
don't call any before-change or after-change functions.\n\
That's because these variables are temporarily set to nil.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
these variables.  See the Emacs Lisp manual for a way of\n\
accomplishing an equivalent result by using other variables.");
  Vafter_change_function = Qnil;

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
accomplishing an equivalent result by using other variables.");
  Vbefore_change_functions = Qnil;

  DEFVAR_LISP ("after-change-functions", &Vafter_change_functions,
	       "List of function to call after each text change.\n\
Three arguments are passed to each function: the positions of\n\
the beginning and end of the range of changed text,\n\
and the length of the pre-change text replaced by that range.\n\
\(For an insertion, the pre-change length is zero;\n\
for a deletion, that length is the number of characters deleted,\n\
and the post-change beginning and end are at the same place.)\n\
\n\
Buffer changes made while executing the `after-change-functions'\n\
don't call any before-change or after-change functions.\n\
That's because these variables are temporarily set to nil.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
these variables.  See the Emacs Lisp manual for a way of\n\
accomplishing an equivalent result by using other variables.");

  Vafter_change_functions = Qnil;

  DEFVAR_LISP ("first-change-hook", &Vfirst_change_hook,
  "A list of functions to call before changing a buffer which is unmodified.\n\
The functions are run using the `run-hooks' function.");
  Vfirst_change_hook = Qnil;
  Qfirst_change_hook = intern ("first-change-hook");
  staticpro (&Qfirst_change_hook);

#if 0 /* The doc string is too long for some compilers,
	 but make-docfile can find it in this comment.  */
  DEFVAR_PER_BUFFER ("buffer-undo-list", &current_buffer->undo_list, Qnil,
    "List of undo entries in current buffer.\n\
Recent changes come first; older changes follow newer.\n\
\n\
An entry (START . END) represents an insertion which begins at\n\
position START and ends at position END.\n\
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
  "Value of point before the last series of scroll operations, or nil.");

  DEFVAR_LISP ("transient-mark-mode", &Vtransient_mark_mode,
    "*Non-nil means deactivate the mark when the buffer contents change.");
  Vtransient_mark_mode = Qnil;

  DEFVAR_LISP ("inhibit-read-only", &Vinhibit_read_only,
    "*Non-nil means disregard read-only status of buffers or characters.\n\
If the value is t, disregard `buffer-read-only' and all `read-only'\n\
text properties.  If the value is a list, disregard `buffer-read-only'\n\
and disregard a `read-only' text property if the property value\n\
is a member of the list.");
  Vinhibit_read_only = Qnil;

  DEFVAR_LISP ("kill-buffer-query-functions", &Vkill_buffer_query_functions,
    "List of functions called with no args to query before killing a buffer.");
  Vkill_buffer_query_functions = Qnil;

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
  defsubr (&Serase_buffer);
  defsubr (&Sset_buffer_major_mode);
  defsubr (&Sswitch_to_buffer);
  defsubr (&Spop_to_buffer);
  defsubr (&Scurrent_buffer);
  defsubr (&Sset_buffer);
  defsubr (&Sbarf_if_buffer_read_only);
  defsubr (&Sbury_buffer);
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
  defsubr (&Snext_overlay_change);
  defsubr (&Sprevious_overlay_change);
  defsubr (&Soverlay_recenter);
  defsubr (&Soverlay_lists);
  defsubr (&Soverlay_get);
  defsubr (&Soverlay_put);
}

keys_of_buffer ()
{
  initial_define_key (control_x_map, 'b', "switch-to-buffer");
  initial_define_key (control_x_map, 'k', "kill-buffer");

  /* This must not be in syms_of_buffer, because Qdisabled is not
     initialized when that function gets called.  */
  Fput (intern ("erase-buffer"), Qdisabled, Qt);
}
