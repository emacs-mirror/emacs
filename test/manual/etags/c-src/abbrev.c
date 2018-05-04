/* Primitives for word-abbrev mode.
   Copyright (C) 1985-1986, 1993, 1996, 1998, 2016-2018 Free Software
   Foundation, Inc.

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
#include <stdio.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"
#include "charset.h"
#include "syntax.h"

/* An abbrev table is an obarray.
 Each defined abbrev is represented by a symbol in that obarray
 whose print name is the abbreviation.
 The symbol's value is a string which is the expansion.
 If its function definition is non-nil, it is called
  after the expansion is done.
 The plist slot of the abbrev symbol is its usage count. */

/* List of all abbrev-table name symbols:
 symbols whose values are abbrev tables.  */

Lisp_Object Vabbrev_table_name_list;

/* The table of global abbrevs.  These are in effect
 in any buffer in which abbrev mode is turned on. */

Lisp_Object Vglobal_abbrev_table;

/* The local abbrev table used by default (in Fundamental Mode buffers) */

Lisp_Object Vfundamental_mode_abbrev_table;

/* Set nonzero when an abbrev definition is changed */

int abbrevs_changed;

int abbrev_all_caps;

/* Non-nil => use this location as the start of abbrev to expand
 (rather than taking the word before point as the abbrev) */

Lisp_Object Vabbrev_start_location;

/* Buffer that Vabbrev_start_location applies to */
Lisp_Object Vabbrev_start_location_buffer;

/* The symbol representing the abbrev most recently expanded */

Lisp_Object Vlast_abbrev;

/* A string for the actual text of the abbrev most recently expanded.
   This has more info than Vlast_abbrev since case is significant.  */

Lisp_Object Vlast_abbrev_text;

/* Character address of start of last abbrev expanded */

int last_abbrev_point;

/* Hook to run before expanding any abbrev.  */

Lisp_Object Vpre_abbrev_expand_hook, Qpre_abbrev_expand_hook;

DEFUN ("make-abbrev-table", Fmake_abbrev_table, Smake_abbrev_table, 0, 0, 0,
  "Create a new, empty abbrev table object.")
  ()
{
  return Fmake_vector (make_number (59), make_number (0));
}

DEFUN ("clear-abbrev-table", Fclear_abbrev_table, Sclear_abbrev_table, 1, 1, 0,
  "Undefine all abbrevs in abbrev table TABLE, leaving it empty.")
  (table)
     Lisp_Object table;
{
  int i, size;

  CHECK_VECTOR (table, 0);
  size = XVECTOR (table)->size;
  abbrevs_changed = 1;
  for (i = 0; i < size; i++)
    XVECTOR (table)->contents[i] = make_number (0);
  return Qnil;
}

DEFUN ("define-abbrev", Fdefine_abbrev, Sdefine_abbrev, 3, 5, 0,
  "Define an abbrev in TABLE named NAME, to expand to EXPANSION and call HOOK.\n\
NAME must be a string.\n\
EXPANSION should usually be a string.\n\
To undefine an abbrev, define it with EXPANSION = nil.\n\
If HOOK is non-nil, it should be a function of no arguments;\n\
it is called after EXPANSION is inserted.\n\
If EXPANSION is not a string, the abbrev is a special one,\n\
 which does not expand in the usual way but only runs HOOK.\n\
COUNT, if specified, initializes the abbrev's usage-count\n\
which is incremented each time the abbrev is used.")
  (table, name, expansion, hook, count)
     Lisp_Object table, name, expansion, hook, count;
{
  Lisp_Object sym, oexp, ohook, tem;
  CHECK_VECTOR (table, 0);
  CHECK_STRING (name, 1);

  if (NILP (count))
    count = make_number (0);
  else
    CHECK_NUMBER (count, 0);

  sym = Fintern (name, table);

  oexp = XSYMBOL (sym)->value;
  ohook = XSYMBOL (sym)->function;
  if (!((EQ (oexp, expansion)
	 || (STRINGP (oexp) && STRINGP (expansion)
	     && (tem = Fstring_equal (oexp, expansion), !NILP (tem))))
	&&
	(EQ (ohook, hook)
	 || (tem = Fequal (ohook, hook), !NILP (tem)))))
    abbrevs_changed = 1;

  Fset (sym, expansion);
  Ffset (sym, hook);
  Fsetplist (sym, count);

  return name;
}

DEFUN ("define-global-abbrev", Fdefine_global_abbrev, Sdefine_global_abbrev, 2, 2,
  "sDefine global abbrev: \nsExpansion for %s: ",
  "Define ABBREV as a global abbreviation for EXPANSION.")
  (abbrev, expansion)
     Lisp_Object abbrev, expansion;
{
  Fdefine_abbrev (Vglobal_abbrev_table, Fdowncase (abbrev),
		  expansion, Qnil, make_number (0));
  return abbrev;
}

DEFUN ("define-mode-abbrev", Fdefine_mode_abbrev, Sdefine_mode_abbrev, 2, 2,
  "sDefine mode abbrev: \nsExpansion for %s: ",
  "Define ABBREV as a mode-specific abbreviation for EXPANSION.")
  (abbrev, expansion)
     Lisp_Object abbrev, expansion;
{
  if (NILP (current_buffer->abbrev_table))
    error ("Major mode has no abbrev table");

  Fdefine_abbrev (current_buffer->abbrev_table, Fdowncase (abbrev),
		  expansion, Qnil, make_number (0));
  return abbrev;
}

DEFUN ("abbrev-symbol", Fabbrev_symbol, Sabbrev_symbol, 1, 2, 0,
  "Return the symbol representing abbrev named ABBREV.\n\
This symbol's name is ABBREV, but it is not the canonical symbol of that name;\n\
it is interned in an abbrev-table rather than the normal obarray.\n\
The value is nil if that abbrev is not defined.\n\
Optional second arg TABLE is abbrev table to look it up in.\n\
The default is to try buffer's mode-specific abbrev table, then global table.")
  (abbrev, table)
     Lisp_Object abbrev, table;
{
  Lisp_Object sym;
  CHECK_STRING (abbrev, 0);
  if (!NILP (table))
    sym = Fintern_soft (abbrev, table);
  else
    {
      sym = Qnil;
      if (!NILP (current_buffer->abbrev_table))
	sym = Fintern_soft (abbrev, current_buffer->abbrev_table);
      if (NILP (XSYMBOL (sym)->value))
	sym = Qnil;
      if (NILP (sym))
	sym = Fintern_soft (abbrev, Vglobal_abbrev_table);
    }
  if (NILP (XSYMBOL (sym)->value)) return Qnil;
  return sym;
}

DEFUN ("abbrev-expansion", Fabbrev_expansion, Sabbrev_expansion, 1, 2, 0,
  "Return the string that ABBREV expands into in the current buffer.\n\
Optionally specify an abbrev table as second arg;\n\
then ABBREV is looked up in that table only.")
  (abbrev, table)
     Lisp_Object abbrev, table;
{
  Lisp_Object sym;
  sym = Fabbrev_symbol (abbrev, table);
  if (NILP (sym)) return sym;
  return Fsymbol_value (sym);
}

/* Expand the word before point, if it is an abbrev.
  Returns 1 if an expansion is done. */

DEFUN ("expand-abbrev", Fexpand_abbrev, Sexpand_abbrev, 0, 0, "",
  "Expand the abbrev before point, if there is an abbrev there.\n\
Effective when explicitly called even when `abbrev-mode' is nil.\n\
Returns the abbrev symbol, if expansion took place.")
  ()
{
  register char *buffer, *p;
  int wordstart, wordend;
  register int wordstart_byte, wordend_byte, idx;
  int whitecnt;
  int uccount = 0, lccount = 0;
  register Lisp_Object sym;
  Lisp_Object expansion, hook, tem;
  Lisp_Object value;

  value = Qnil;

  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, Qpre_abbrev_expand_hook);

  wordstart = 0;
  if (!(BUFFERP (Vabbrev_start_location_buffer)
	&& XBUFFER (Vabbrev_start_location_buffer) == current_buffer))
    Vabbrev_start_location = Qnil;
  if (!NILP (Vabbrev_start_location))
    {
      tem = Vabbrev_start_location;
      CHECK_NUMBER_COERCE_MARKER (tem, 0);
      wordstart = XINT (tem);
      Vabbrev_start_location = Qnil;
      if (wordstart < BEGV || wordstart > ZV)
	wordstart = 0;
      if (wordstart && wordstart != ZV)
	{
	  wordstart_byte = CHAR_TO_BYTE (wordstart);
	  if (FETCH_BYTE (wordstart_byte) == '-')
	    del_range (wordstart, wordstart + 1);
	}
    }
  if (!wordstart)
    wordstart = scan_words (PT, -1);

  if (!wordstart)
    return value;

  wordstart_byte = CHAR_TO_BYTE (wordstart);
  wordend = scan_words (wordstart, 1);
  if (!wordend)
    return value;

  if (wordend > PT)
    wordend = PT;

  wordend_byte = CHAR_TO_BYTE (wordend);
  whitecnt = PT - wordend;
  if (wordend <= wordstart)
    return value;

  p = buffer = (char *) alloca (wordend_byte - wordstart_byte);

  for (idx = wordstart_byte; idx < wordend_byte; idx++)
    {
      /* ??? This loop needs to go by characters!  */
      register int c = FETCH_BYTE (idx);
      if (UPPERCASEP (c))
	c = DOWNCASE (c), uccount++;
      else if (! NOCASEP (c))
	lccount++;
      *p++ = c;
    }

  if (VECTORP (current_buffer->abbrev_table))
    sym = oblookup (current_buffer->abbrev_table, buffer,
		    wordend - wordstart, wordend_byte - wordstart_byte);
  else
    XSETFASTINT (sym, 0);
  if (INTEGERP (sym) || NILP (XSYMBOL (sym)->value))
    sym = oblookup (Vglobal_abbrev_table, buffer,
		    wordend - wordstart, wordend_byte - wordstart_byte);
  if (INTEGERP (sym) || NILP (XSYMBOL (sym)->value))
    return value;

  if (INTERACTIVE && !EQ (minibuf_window, selected_window))
    {
      /* Add an undo boundary, in case we are doing this for
	 a self-inserting command which has avoided making one so far.  */
      SET_PT (wordend);
      Fundo_boundary ();
    }

  Vlast_abbrev_text
    = Fbuffer_substring (make_number (wordstart), make_number (wordend));

  /* Now sym is the abbrev symbol.  */
  Vlast_abbrev = sym;
  value = sym;
  last_abbrev_point = wordstart;

  if (INTEGERP (XSYMBOL (sym)->plist))
    XSETINT (XSYMBOL (sym)->plist,
	     XINT (XSYMBOL (sym)->plist) + 1);	/* Increment use count */

  /* If this abbrev has an expansion, delete the abbrev
     and insert the expansion.  */
  expansion = XSYMBOL (sym)->value;
  if (STRINGP (expansion))
    {
      SET_PT (wordstart);

      del_range_both (wordstart, wordstart_byte, wordend, wordend_byte, 1);

      insert_from_string (expansion, 0, 0, XSTRING (expansion)->size,
			  STRING_BYTES (XSTRING (expansion)), 1);
      SET_PT (PT + whitecnt);

      if (uccount && !lccount)
	{
	  /* Abbrev was all caps */
	  /* If expansion is multiple words, normally capitalize each word */
	  /* This used to be if (!... && ... >= ...) Fcapitalize; else Fupcase
	     but Megatest 68000 compiler can't handle that */
	  if (!abbrev_all_caps)
	    if (scan_words (PT, -1) > scan_words (wordstart, 1))
	      {
		Fupcase_initials_region (make_number (wordstart),
					 make_number (PT));
		goto caped;
	      }
	  /* If expansion is one word, or if user says so, upcase it all. */
	  Fupcase_region (make_number (wordstart), make_number (PT));
	caped: ;
	}
      else if (uccount)
	{
	  /* Abbrev included some caps.  Cap first initial of expansion */
	  int pos = wordstart_byte;

	  /* Find the initial.  */
	  while (pos < PT_BYTE
		 && SYNTAX (*BUF_BYTE_ADDRESS (current_buffer, pos)) != Sword)
	    pos++;

	  /* Change just that.  */
	  pos = BYTE_TO_CHAR (pos);
	  Fupcase_initials_region (make_number (pos), make_number (pos + 1));
	}
    }

  hook = XSYMBOL (sym)->function;
  if (!NILP (hook))
    {
      Lisp_Object expanded, prop;

      /* If the abbrev has a hook function, run it.  */
      expanded = call0 (hook);

      /* In addition, if the hook function is a symbol with a a
	 non-nil `no-self-insert' property, let the value it returned
	 specify whether we consider that an expansion took place.  If
	 it returns nil, no expansion has been done.  */

      if (SYMBOLP (hook)
	  && NILP (expanded)
	  && (prop = Fget (hook, intern ("no-self-insert")),
	      !NILP (prop)))
	value = Qnil;
    }

  return value;
}

DEFUN ("unexpand-abbrev", Funexpand_abbrev, Sunexpand_abbrev, 0, 0, "",
  "Undo the expansion of the last abbrev that expanded.\n\
This differs from ordinary undo in that other editing done since then\n\
is not undone.")
  ()
{
  int opoint = PT;
  int adjust = 0;
  if (last_abbrev_point < BEGV
      || last_abbrev_point > ZV)
    return Qnil;
  SET_PT (last_abbrev_point);
  if (STRINGP (Vlast_abbrev_text))
    {
      /* This isn't correct if Vlast_abbrev->function was used
         to do the expansion */
      Lisp_Object val;
      int zv_before;

      val = XSYMBOL (Vlast_abbrev)->value;
      if (!STRINGP (val))
	error ("value of abbrev-symbol must be a string");
      zv_before = ZV;
      del_range_byte (PT_BYTE, PT_BYTE + STRING_BYTES (XSTRING (val)), 1);
      /* Don't inherit properties here; just copy from old contents.  */
      insert_from_string (Vlast_abbrev_text, 0, 0,
			  XSTRING (Vlast_abbrev_text)->size,
			  STRING_BYTES (XSTRING (Vlast_abbrev_text)), 0);
      Vlast_abbrev_text = Qnil;
      /* Total number of characters deleted.  */
      adjust = ZV - zv_before;
    }
  SET_PT (last_abbrev_point < opoint ? opoint + adjust : opoint);
  return Qnil;
}

static void
write_abbrev (sym, stream)
     Lisp_Object sym, stream;
{
  Lisp_Object name;
  if (NILP (XSYMBOL (sym)->value))
    return;
  insert ("    (", 5);
  XSETSTRING (name, XSYMBOL (sym)->name);
  Fprin1 (name, stream);
  insert (" ", 1);
  Fprin1 (XSYMBOL (sym)->value, stream);
  insert (" ", 1);
  Fprin1 (XSYMBOL (sym)->function, stream);
  insert (" ", 1);
  Fprin1 (XSYMBOL (sym)->plist, stream);
  insert (")\n", 2);
}

static void
describe_abbrev (sym, stream)
     Lisp_Object sym, stream;
{
  Lisp_Object one;

  if (NILP (XSYMBOL (sym)->value))
    return;
  one = make_number (1);
  Fprin1 (Fsymbol_name (sym), stream);
  Findent_to (make_number (15), one);
  Fprin1 (XSYMBOL (sym)->plist, stream);
  Findent_to (make_number (20), one);
  Fprin1 (XSYMBOL (sym)->value, stream);
  if (!NILP (XSYMBOL (sym)->function))
    {
      Findent_to (make_number (45), one);
      Fprin1 (XSYMBOL (sym)->function, stream);
    }
  Fterpri (stream);
}

DEFUN ("insert-abbrev-table-description", Finsert_abbrev_table_description,
  Sinsert_abbrev_table_description, 1, 2, 0,
  "Insert before point a full description of abbrev table named NAME.\n\
NAME is a symbol whose value is an abbrev table.\n\
If optional 2nd arg READABLE is non-nil, a human-readable description\n\
is inserted.  Otherwise the description is an expression,\n\
a call to `define-abbrev-table', which would\n\
define the abbrev table NAME exactly as it is currently defined.")
  (name, readable)
     Lisp_Object name, readable;
{
  Lisp_Object table;
  Lisp_Object stream;

  CHECK_SYMBOL (name, 0);
  table = Fsymbol_value (name);
  CHECK_VECTOR (table, 0);

  XSETBUFFER (stream, current_buffer);

  if (!NILP (readable))
    {
      insert_string ("(");
      Fprin1 (name, stream);
      insert_string (")\n\n");
      map_obarray (table, describe_abbrev, stream);
      insert_string ("\n\n");
    }
  else
    {
      insert_string ("(define-abbrev-table '");
      Fprin1 (name, stream);
      insert_string (" '(\n");
      map_obarray (table, write_abbrev, stream);
      insert_string ("    ))\n\n");
    }

  return Qnil;
}

DEFUN ("define-abbrev-table", Fdefine_abbrev_table, Sdefine_abbrev_table,
       2, 2, 0,
  "Define TABLENAME (a symbol) as an abbrev table name.\n\
Define abbrevs in it according to DEFINITIONS, which is a list of elements\n\
of the form (ABBREVNAME EXPANSION HOOK USECOUNT).")
  (tablename, definitions)
     Lisp_Object tablename, definitions;
{
  Lisp_Object name, exp, hook, count;
  Lisp_Object table, elt;

  CHECK_SYMBOL (tablename, 0);
  table = Fboundp (tablename);
  if (NILP (table) || (table = Fsymbol_value (tablename), NILP (table)))
    {
      table = Fmake_abbrev_table ();
      Fset (tablename, table);
      Vabbrev_table_name_list = Fcons (tablename, Vabbrev_table_name_list);
    }
  CHECK_VECTOR (table, 0);

  for (; !NILP (definitions); definitions = Fcdr (definitions))
    {
      elt = Fcar (definitions);
      name  = Fcar (elt);	elt = Fcdr (elt);
      exp   = Fcar (elt);	elt = Fcdr (elt);
      hook  = Fcar (elt);	elt = Fcdr (elt);
      count = Fcar (elt);
      Fdefine_abbrev (table, name, exp, hook, count);
    }
  return Qnil;
}

void
syms_of_abbrev ()
{
  DEFVAR_LISP ("abbrev-table-name-list", &Vabbrev_table_name_list,
    "List of symbols whose values are abbrev tables.");
  Vabbrev_table_name_list = Fcons (intern ("fundamental-mode-abbrev-table"),
				   Fcons (intern ("global-abbrev-table"),
					  Qnil));

  DEFVAR_LISP ("global-abbrev-table", &Vglobal_abbrev_table,
    "The abbrev table whose abbrevs affect all buffers.\n\
Each buffer may also have a local abbrev table.\n\
If it does, the local table overrides the global one\n\
for any particular abbrev defined in both.");
  Vglobal_abbrev_table = Fmake_abbrev_table ();

  DEFVAR_LISP ("fundamental-mode-abbrev-table", &Vfundamental_mode_abbrev_table,
    "The abbrev table of mode-specific abbrevs for Fundamental Mode.");
  Vfundamental_mode_abbrev_table = Fmake_abbrev_table ();
  current_buffer->abbrev_table = Vfundamental_mode_abbrev_table;
  buffer_defaults.abbrev_table = Vfundamental_mode_abbrev_table;

  DEFVAR_LISP ("last-abbrev", &Vlast_abbrev,
    "The abbrev-symbol of the last abbrev expanded.  See `abbrev-symbol'.");

  DEFVAR_LISP ("last-abbrev-text", &Vlast_abbrev_text,
    "The exact text of the last abbrev expanded.\n\
nil if the abbrev has already been unexpanded.");

  DEFVAR_INT ("last-abbrev-location", &last_abbrev_point,
    "The location of the start of the last abbrev expanded.");

  Vlast_abbrev = Qnil;
  Vlast_abbrev_text = Qnil;
  last_abbrev_point = 0;

  DEFVAR_LISP ("abbrev-start-location", &Vabbrev_start_location,
    "Buffer position for `expand-abbrev' to use as the start of the abbrev.\n\
nil means use the word before point as the abbrev.\n\
Calling `expand-abbrev' sets this to nil.");
  Vabbrev_start_location = Qnil;

  DEFVAR_LISP ("abbrev-start-location-buffer", &Vabbrev_start_location_buffer,
    "Buffer that `abbrev-start-location' has been set for.\n\
Trying to expand an abbrev in any other buffer clears `abbrev-start-location'.");
  Vabbrev_start_location_buffer = Qnil;

  DEFVAR_PER_BUFFER ("local-abbrev-table", &current_buffer->abbrev_table, Qnil,
    "Local (mode-specific) abbrev table of current buffer.");

  DEFVAR_BOOL ("abbrevs-changed", &abbrevs_changed,
    "Set non-nil by defining or altering any word abbrevs.\n\
This causes `save-some-buffers' to offer to save the abbrevs.");
  abbrevs_changed = 0;

  DEFVAR_BOOL ("abbrev-all-caps", &abbrev_all_caps,
    "*Set non-nil means expand multi-word abbrevs all caps if abbrev was so.");
  abbrev_all_caps = 0;

  DEFVAR_LISP ("pre-abbrev-expand-hook", &Vpre_abbrev_expand_hook,
    "Function or functions to be called before abbrev expansion is done.\n\
This is the first thing that `expand-abbrev' does, and so this may change\n\
the current abbrev table before abbrev lookup happens.");
  Vpre_abbrev_expand_hook = Qnil;
  Qpre_abbrev_expand_hook = intern ("pre-abbrev-expand-hook");
  staticpro (&Qpre_abbrev_expand_hook);

  defsubr (&Smake_abbrev_table);
  defsubr (&Sclear_abbrev_table);
  defsubr (&Sdefine_abbrev);
  defsubr (&Sdefine_global_abbrev);
  defsubr (&Sdefine_mode_abbrev);
  defsubr (&Sabbrev_expansion);
  defsubr (&Sabbrev_symbol);
  defsubr (&Sexpand_abbrev);
  defsubr (&Sunexpand_abbrev);
  defsubr (&Sinsert_abbrev_table_description);
  defsubr (&Sdefine_abbrev_table);
}
