/* Manipulation of keymaps
   Copyright (C) 1985, 86,87,88,93,94,95,98,99, 2000, 2001
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
#include <stdio.h>
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "charset.h"
#include "keyboard.h"
#include "termhooks.h"
#include "blockinput.h"
#include "puresize.h"
#include "intervals.h"
#include "keymap.h"

/* The number of elements in keymap vectors.  */
#define DENSE_TABLE_SIZE (0200)

/* Actually allocate storage for these variables */

Lisp_Object current_global_map;	/* Current global keymap */

Lisp_Object global_map;		/* default global key bindings */

Lisp_Object meta_map;		/* The keymap used for globally bound
				   ESC-prefixed default commands */

Lisp_Object control_x_map;	/* The keymap used for globally bound
				   C-x-prefixed default commands */

/* was MinibufLocalMap */
Lisp_Object Vminibuffer_local_map;
				/* The keymap used by the minibuf for local
				   bindings when spaces are allowed in the
				   minibuf */

/* was MinibufLocalNSMap */
Lisp_Object Vminibuffer_local_ns_map;
				/* The keymap used by the minibuf for local
				   bindings when spaces are not encouraged
				   in the minibuf */

/* keymap used for minibuffers when doing completion */
/* was MinibufLocalCompletionMap */
Lisp_Object Vminibuffer_local_completion_map;

/* keymap used for minibuffers when doing completion and require a match */
/* was MinibufLocalMustMatchMap */
Lisp_Object Vminibuffer_local_must_match_map;

/* Alist of minor mode variables and keymaps.  */
Lisp_Object Vminor_mode_map_alist;

/* Alist of major-mode-specific overrides for
   minor mode variables and keymaps.  */
Lisp_Object Vminor_mode_overriding_map_alist;

/* List of emulation mode keymap alists.  */
Lisp_Object Vemulation_mode_map_alists;

/* Keymap mapping ASCII function key sequences onto their preferred forms.
   Initialized by the terminal-specific lisp files.  See DEFVAR for more
   documentation.  */
Lisp_Object Vfunction_key_map;

/* Keymap mapping ASCII function key sequences onto their preferred forms.  */
Lisp_Object Vkey_translation_map;

/* A list of all commands given new bindings since a certain time
   when nil was stored here.
   This is used to speed up recomputation of menu key equivalents
   when Emacs starts up.   t means don't record anything here.  */
Lisp_Object Vdefine_key_rebound_commands;

Lisp_Object Qkeymapp, Qkeymap, Qnon_ascii, Qmenu_item, Qremap;

/* Alist of elements like (DEL . "\d").  */
static Lisp_Object exclude_keys;

/* Pre-allocated 2-element vector for Fremap_command to use.  */
static Lisp_Object remap_command_vector;

/* A char with the CHAR_META bit set in a vector or the 0200 bit set
   in a string key sequence is equivalent to prefixing with this
   character.  */
extern Lisp_Object meta_prefix_char;

extern Lisp_Object Voverriding_local_map;

/* Hash table used to cache a reverse-map to speed up calls to where-is.  */
static Lisp_Object where_is_cache;
/* Which keymaps are reverse-stored in the cache.  */
static Lisp_Object where_is_cache_keymaps;

static Lisp_Object store_in_keymap P_ ((Lisp_Object, Lisp_Object, Lisp_Object));
static void fix_submap_inheritance P_ ((Lisp_Object, Lisp_Object, Lisp_Object));

static Lisp_Object define_as_prefix P_ ((Lisp_Object, Lisp_Object));
static void describe_command P_ ((Lisp_Object, Lisp_Object));
static void describe_translation P_ ((Lisp_Object, Lisp_Object));
static void describe_map P_ ((Lisp_Object, Lisp_Object,
			      void (*) P_ ((Lisp_Object, Lisp_Object)),
			      int, Lisp_Object, Lisp_Object*, int));
static void silly_event_symbol_error P_ ((Lisp_Object));

/* Keymap object support - constructors and predicates.			*/

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 1, 0,
       doc: /* Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
CHARTABLE is a char-table that holds the bindings for the ASCII
characters.  ALIST is an assoc-list which holds bindings for function keys,
mouse events, and any other things that appear in the input stream.
All entries in it are initially nil, meaning "command undefined".

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with `x-popup-menu'.  */)
     (string)
     Lisp_Object string;
{
  Lisp_Object tail;
  if (!NILP (string))
    tail = Fcons (string, Qnil);
  else
    tail = Qnil;
  return Fcons (Qkeymap,
		Fcons (Fmake_char_table (Qkeymap, Qnil), tail));
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap, 0, 1, 0,
       doc: /* Construct and return a new sparse keymap.
Its car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),
which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),
which binds the function key or mouse event SYMBOL to DEFINITION.
Initially the alist is nil.

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with `x-popup-menu'.  */)
     (string)
     Lisp_Object string;
{
  if (!NILP (string))
    return Fcons (Qkeymap, Fcons (string, Qnil));
  return Fcons (Qkeymap, Qnil);
}

/* This function is used for installing the standard key bindings
   at initialization time.

   For example:

   initial_define_key (control_x_map, Ctl('X'), "exchange-point-and-mark");  */

void
initial_define_key (keymap, key, defname)
     Lisp_Object keymap;
     int key;
     char *defname;
{
  store_in_keymap (keymap, make_number (key), intern (defname));
}

void
initial_define_lispy_key (keymap, keyname, defname)
     Lisp_Object keymap;
     char *keyname;
     char *defname;
{
  store_in_keymap (keymap, intern (keyname), intern (defname));
}

DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
       doc: /* Return t if OBJECT is a keymap.

A keymap is a list (keymap . ALIST),
or a symbol whose function definition is itself a keymap.
ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);
a vector of densely packed bindings for small character codes
is also allowed as an element.  */)
     (object)
     Lisp_Object object;
{
  return (KEYMAPP (object) ? Qt : Qnil);
}

DEFUN ("keymap-prompt", Fkeymap_prompt, Skeymap_prompt, 1, 1, 0,
       doc: /* Return the prompt-string of a keymap MAP.
If non-nil, the prompt is shown in the echo-area
when reading a key-sequence to be looked-up in this keymap.  */)
     (map)
     Lisp_Object map;
{
  while (CONSP (map))
    {
      register Lisp_Object tem;
      tem = Fcar (map);
      if (STRINGP (tem))
	return tem;
      map = Fcdr (map);
    }
  return Qnil;
}

/* Check that OBJECT is a keymap (after dereferencing through any
   symbols).  If it is, return it.

   If AUTOLOAD is non-zero and OBJECT is a symbol whose function value
   is an autoload form, do the autoload and try again.
   If AUTOLOAD is nonzero, callers must assume GC is possible.

   If the map needs to be autoloaded, but AUTOLOAD is zero (and ERROR
   is zero as well), return Qt.

   ERROR controls how we respond if OBJECT isn't a keymap.
   If ERROR is non-zero, signal an error; otherwise, just return Qnil.

   Note that most of the time, we don't want to pursue autoloads.
   Functions like Faccessible_keymaps which scan entire keymap trees
   shouldn't load every autoloaded keymap.  I'm not sure about this,
   but it seems to me that only read_key_sequence, Flookup_key, and
   Fdefine_key should cause keymaps to be autoloaded.

   This function can GC when AUTOLOAD is non-zero, because it calls
   do_autoload which can GC.  */

Lisp_Object
get_keymap (object, error, autoload)
     Lisp_Object object;
     int error, autoload;
{
  Lisp_Object tem;

 autoload_retry:
  if (NILP (object))
    goto end;
  if (CONSP (object) && EQ (XCAR (object), Qkeymap))
    return object;

  tem = indirect_function (object);
  if (CONSP (tem))
    {
      if (EQ (XCAR (tem), Qkeymap))
	return tem;

      /* Should we do an autoload?  Autoload forms for keymaps have
	 Qkeymap as their fifth element.  */
      if ((autoload || !error) && EQ (XCAR (tem), Qautoload))
	{
	  Lisp_Object tail;

	  tail = Fnth (make_number (4), tem);
	  if (EQ (tail, Qkeymap))
	    {
	      if (autoload)
		{
		  struct gcpro gcpro1, gcpro2;

		  GCPRO2 (tem, object);
		  do_autoload (tem, object);
		  UNGCPRO;

		  goto autoload_retry;
		}
	      else
	      	return Qt;
	    }
	}
    }

 end:
  if (error)
    wrong_type_argument (Qkeymapp, object);
  return Qnil;
}

/* Return the parent map of KEYMAP, or nil if it has none.
   We assume that KEYMAP is a valid keymap.  */

Lisp_Object
keymap_parent (keymap, autoload)
     Lisp_Object keymap;
     int autoload;
{
  Lisp_Object list;

  keymap = get_keymap (keymap, 1, autoload);

  /* Skip past the initial element `keymap'.  */
  list = XCDR (keymap);
  for (; CONSP (list); list = XCDR (list))
    {
      /* See if there is another `keymap'.  */
      if (KEYMAPP (list))
	return list;
    }

  return get_keymap (list, 0, autoload);
}

DEFUN ("keymap-parent", Fkeymap_parent, Skeymap_parent, 1, 1, 0,
       doc: /* Return the parent keymap of KEYMAP.  */)
     (keymap)
     Lisp_Object keymap;
{
  return keymap_parent (keymap, 1);
}

/* Check whether MAP is one of MAPS parents.  */
int
keymap_memberp (map, maps)
     Lisp_Object map, maps;
{
  if (NILP (map)) return 0;
  while (KEYMAPP (maps) && !EQ (map, maps))
    maps = keymap_parent (maps, 0);
  return (EQ (map, maps));
}

/* Set the parent keymap of MAP to PARENT.  */

DEFUN ("set-keymap-parent", Fset_keymap_parent, Sset_keymap_parent, 2, 2, 0,
       doc: /* Modify KEYMAP to set its parent map to PARENT.
PARENT should be nil or another keymap.  */)
     (keymap, parent)
     Lisp_Object keymap, parent;
{
  Lisp_Object list, prev;
  struct gcpro gcpro1, gcpro2;
  int i;

  /* Force a keymap flush for the next call to where-is.
     Since this can be called from within where-is, we don't set where_is_cache
     directly but only where_is_cache_keymaps, since where_is_cache shouldn't
     be changed during where-is, while where_is_cache_keymaps is only used at
     the very beginning of where-is and can thus be changed here without any
     adverse effect.
     This is a very minor correctness (rather than safety) issue.  */
  where_is_cache_keymaps = Qt;

  GCPRO2 (keymap, parent);
  keymap = get_keymap (keymap, 1, 1);

  if (!NILP (parent))
    {
      parent = get_keymap (parent, 1, 1);

      /* Check for cycles.  */
      if (keymap_memberp (keymap, parent))
	error ("Cyclic keymap inheritance");
    }

  /* Skip past the initial element `keymap'.  */
  prev = keymap;
  while (1)
    {
      list = XCDR (prev);
      /* If there is a parent keymap here, replace it.
	 If we came to the end, add the parent in PREV.  */
      if (!CONSP (list) || KEYMAPP (list))
	{
	  /* If we already have the right parent, return now
	     so that we avoid the loops below.  */
	  if (EQ (XCDR (prev), parent))
	    RETURN_UNGCPRO (parent);

	  XSETCDR (prev, parent);
	  break;
	}
      prev = list;
    }

  /* Scan through for submaps, and set their parents too.  */

  for (list = XCDR (keymap); CONSP (list); list = XCDR (list))
    {
      /* Stop the scan when we come to the parent.  */
      if (EQ (XCAR (list), Qkeymap))
	break;

      /* If this element holds a prefix map, deal with it.  */
      if (CONSP (XCAR (list))
	  && CONSP (XCDR (XCAR (list))))
	fix_submap_inheritance (keymap, XCAR (XCAR (list)),
				XCDR (XCAR (list)));

      if (VECTORP (XCAR (list)))
	for (i = 0; i < XVECTOR (XCAR (list))->size; i++)
	  if (CONSP (XVECTOR (XCAR (list))->contents[i]))
	    fix_submap_inheritance (keymap, make_number (i),
				    XVECTOR (XCAR (list))->contents[i]);

      if (CHAR_TABLE_P (XCAR (list)))
	{
	  Lisp_Object indices[3];

	  map_char_table (fix_submap_inheritance, Qnil, XCAR (list),
			  keymap, 0, indices);
	}
    }

  RETURN_UNGCPRO (parent);
}

/* EVENT is defined in MAP as a prefix, and SUBMAP is its definition.
   if EVENT is also a prefix in MAP's parent,
   make sure that SUBMAP inherits that definition as its own parent.  */

static void
fix_submap_inheritance (map, event, submap)
     Lisp_Object map, event, submap;
{
  Lisp_Object map_parent, parent_entry;

  /* SUBMAP is a cons that we found as a key binding.
     Discard the other things found in a menu key binding.  */

  submap = get_keymap (get_keyelt (submap, 0), 0, 0);

  /* If it isn't a keymap now, there's no work to do.  */
  if (!CONSP (submap))
    return;

  map_parent = keymap_parent (map, 0);
  if (!NILP (map_parent))
    parent_entry =
      get_keymap (access_keymap (map_parent, event, 0, 0, 0), 0, 0);
  else
    parent_entry = Qnil;

  /* If MAP's parent has something other than a keymap,
     our own submap shadows it completely.  */
  if (!CONSP (parent_entry))
    return;

  if (! EQ (parent_entry, submap))
    {
      Lisp_Object submap_parent;
      submap_parent = submap;
      while (1)
	{
	  Lisp_Object tem;

	  tem = keymap_parent (submap_parent, 0);

	  if (KEYMAPP (tem))
	    {
	      if (keymap_memberp (tem, parent_entry))
		/* Fset_keymap_parent could create a cycle.  */
		return;
	      submap_parent = tem;
	    }
	  else
	    break;
	}
      Fset_keymap_parent (submap_parent, parent_entry);
    }
}

/* Look up IDX in MAP.  IDX may be any sort of event.
   Note that this does only one level of lookup; IDX must be a single
   event, not a sequence.

   If T_OK is non-zero, bindings for Qt are treated as default
   bindings; any key left unmentioned by other tables and bindings is
   given the binding of Qt.

   If T_OK is zero, bindings for Qt are not treated specially.

   If NOINHERIT, don't accept a subkeymap found in an inherited keymap.  */

Lisp_Object
access_keymap (map, idx, t_ok, noinherit, autoload)
     Lisp_Object map;
     Lisp_Object idx;
     int t_ok;
     int noinherit;
     int autoload;
{
  Lisp_Object val;

  /* Qunbound in VAL means we have found no binding yet.  */
  val = Qunbound;

  /* If idx is a list (some sort of mouse click, perhaps?),
     the index we want to use is the car of the list, which
     ought to be a symbol.  */
  idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (SYMBOLP (idx))
    idx = reorder_modifiers (idx);
  else if (INTEGERP (idx))
    /* Clobber the high bits that can be present on a machine
       with more than 24 bits of integer.  */
    XSETFASTINT (idx, XINT (idx) & (CHAR_META | (CHAR_META - 1)));

  /* Handle the special meta -> esc mapping. */
  if (INTEGERP (idx) && XUINT (idx) & meta_modifier)
    {
      /* See if there is a meta-map.  If there's none, there is
         no binding for IDX, unless a default binding exists in MAP.  */
      struct gcpro gcpro1;
      Lisp_Object meta_map;
      GCPRO1 (map);
      meta_map = get_keymap (access_keymap (map, meta_prefix_char,
					    t_ok, noinherit, autoload),
			     0, autoload);
      UNGCPRO;
      if (CONSP (meta_map))
	{
	  map = meta_map;
	  idx = make_number (XUINT (idx) & ~meta_modifier);
	}
      else if (t_ok)
	/* Set IDX to t, so that we only find a default binding.  */
	idx = Qt;
      else
	/* We know there is no binding.  */
	return Qnil;
    }

  /* t_binding is where we put a default binding that applies,
     to use in case we do not find a binding specifically
     for this key sequence.  */
  {
    Lisp_Object tail;
    Lisp_Object t_binding = Qnil;
    struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

    GCPRO4 (map, tail, idx, t_binding);

    /* If `t_ok' is 2, both `t' and generic-char bindings are accepted.
       If it is 1, only generic-char bindings are accepted.
       Otherwise, neither are.  */
    t_ok = t_ok ? 2 : 0;

    for (tail = XCDR (map);
	 (CONSP (tail)
	  || (tail = get_keymap (tail, 0, autoload), CONSP (tail)));
	 tail = XCDR (tail))
      {
	Lisp_Object binding;

	binding = XCAR (tail);
	if (SYMBOLP (binding))
	  {
	    /* If NOINHERIT, stop finding prefix definitions
	       after we pass a second occurrence of the `keymap' symbol.  */
	    if (noinherit && EQ (binding, Qkeymap))
	      RETURN_UNGCPRO (Qnil);
	  }
	else if (CONSP (binding))
	  {
	    Lisp_Object key = XCAR (binding);

	    if (EQ (key, idx))
	      val = XCDR (binding);
	    else if (t_ok
		     && INTEGERP (idx)
		     && (XINT (idx) & CHAR_MODIFIER_MASK) == 0
		     && INTEGERP (key)
		     && (XINT (key) & CHAR_MODIFIER_MASK) == 0
		     && !SINGLE_BYTE_CHAR_P (XINT (idx))
		     && !SINGLE_BYTE_CHAR_P (XINT (key))
		     && CHAR_VALID_P (XINT (key), 1)
		     && !CHAR_VALID_P (XINT (key), 0)
		     && (CHAR_CHARSET (XINT (key))
			 == CHAR_CHARSET (XINT (idx))))
	      {
		/* KEY is the generic character of the charset of IDX.
		   Use KEY's binding if there isn't a binding for IDX
		   itself.  */
		t_binding = XCDR (binding);
		t_ok = 0;
	      }
	    else if (t_ok > 1 && EQ (key, Qt))
	      {
		t_binding = XCDR (binding);
		t_ok = 1;
	      }
	  }
	else if (VECTORP (binding))
	  {
	    if (NATNUMP (idx) && XFASTINT (idx) < ASIZE (binding))
	      val = AREF (binding, XFASTINT (idx));
	  }
	else if (CHAR_TABLE_P (binding))
	  {
	    /* Character codes with modifiers
	       are not included in a char-table.
	       All character codes without modifiers are included.  */
	    if (NATNUMP (idx) && (XFASTINT (idx) & CHAR_MODIFIER_MASK) == 0)
	      {
		val = Faref (binding, idx);
		/* `nil' has a special meaning for char-tables, so
		   we use something else to record an explicitly
		   unbound entry.  */
		if (NILP (val))
		  val = Qunbound;
	      }
	  }

	/* If we found a binding, clean it up and return it.  */
	if (!EQ (val, Qunbound))
	  {
	    if (EQ (val, Qt))
	      /* A Qt binding is just like an explicit nil binding
		 (i.e. it shadows any parent binding but not bindings in
		 keymaps of lower precedence).  */
	      val = Qnil;
	    val = get_keyelt (val, autoload);
	    if (KEYMAPP (val))
	      fix_submap_inheritance (map, idx, val);
	    RETURN_UNGCPRO (val);
	  }
	QUIT;
      }
    UNGCPRO;
    return get_keyelt (t_binding, autoload);
  }
}

/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is the object to look up in KEYMAP to yield the definition.

   Also if OBJECT has a menu string as the first element,
   remove that.  Also remove a menu help string as second element.

   If AUTOLOAD is nonzero, load autoloadable keymaps
   that are referred to with indirection.  */

Lisp_Object
get_keyelt (object, autoload)
     Lisp_Object object;
     int autoload;
{
  while (1)
    {
      if (!(CONSP (object)))
	/* This is really the value.  */
	return object;

      /* If the keymap contents looks like (keymap ...) or (lambda ...)
	 then use itself. */
      else if (EQ (XCAR (object), Qkeymap) || EQ (XCAR (object), Qlambda))
	return object;

      /* If the keymap contents looks like (menu-item name . DEFN)
	 or (menu-item name DEFN ...) then use DEFN.
	 This is a new format menu item.  */
      else if (EQ (XCAR (object), Qmenu_item))
	{
	  if (CONSP (XCDR (object)))
	    {
	      Lisp_Object tem;

	      object = XCDR (XCDR (object));
	      tem = object;
	      if (CONSP (object))
		object = XCAR (object);

	      /* If there's a `:filter FILTER', apply FILTER to the
		 menu-item's definition to get the real definition to
		 use.  */
	      for (; CONSP (tem) && CONSP (XCDR (tem)); tem = XCDR (tem))
		if (EQ (XCAR (tem), QCfilter) && autoload)
		  {
		    Lisp_Object filter;
		    filter = XCAR (XCDR (tem));
		    filter = list2 (filter, list2 (Qquote, object));
		    object = menu_item_eval_property (filter);
		    break;
		  }
	    }
	  else
	    /* Invalid keymap.  */
	    return object;
	}

      /* If the keymap contents looks like (STRING . DEFN), use DEFN.
	 Keymap alist elements like (CHAR MENUSTRING . DEFN)
	 will be used by HierarKey menus.  */
      else if (STRINGP (XCAR (object)))
	{
	  object = XCDR (object);
	  /* Also remove a menu help string, if any,
	     following the menu item name.  */
	  if (CONSP (object) && STRINGP (XCAR (object)))
	    object = XCDR (object);
	  /* Also remove the sublist that caches key equivalences, if any.  */
	  if (CONSP (object) && CONSP (XCAR (object)))
	    {
	      Lisp_Object carcar;
	      carcar = XCAR (XCAR (object));
	      if (NILP (carcar) || VECTORP (carcar))
		object = XCDR (object);
	    }
	}

      /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
      else
	{
	  struct gcpro gcpro1;
	  Lisp_Object map;
	  GCPRO1 (object);
	  map = get_keymap (Fcar_safe (object), 0, autoload);
	  UNGCPRO;
	  return (!CONSP (map) ? object /* Invalid keymap */
		  : access_keymap (map, Fcdr (object), 0, 0, autoload));
	}
    }
}

static Lisp_Object
store_in_keymap (keymap, idx, def)
     Lisp_Object keymap;
     register Lisp_Object idx;
     register Lisp_Object def;
{
  /* Flush any reverse-map cache.  */
  where_is_cache = Qnil;
  where_is_cache_keymaps = Qt;

  /* If we are preparing to dump, and DEF is a menu element
     with a menu item indicator, copy it to ensure it is not pure.  */
  if (CONSP (def) && PURE_P (def)
      && (EQ (XCAR (def), Qmenu_item) || STRINGP (XCAR (def))))
    def = Fcons (XCAR (def), XCDR (def));

  if (!CONSP (keymap) || !EQ (XCAR (keymap), Qkeymap))
    error ("attempt to define a key in a non-keymap");

  /* If idx is a list (some sort of mouse click, perhaps?),
     the index we want to use is the car of the list, which
     ought to be a symbol.  */
  idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (SYMBOLP (idx))
    idx = reorder_modifiers (idx);
  else if (INTEGERP (idx))
    /* Clobber the high bits that can be present on a machine
       with more than 24 bits of integer.  */
    XSETFASTINT (idx, XINT (idx) & (CHAR_META | (CHAR_META - 1)));

  /* Scan the keymap for a binding of idx.  */
  {
    Lisp_Object tail;

    /* The cons after which we should insert new bindings.  If the
       keymap has a table element, we record its position here, so new
       bindings will go after it; this way, the table will stay
       towards the front of the alist and character lookups in dense
       keymaps will remain fast.  Otherwise, this just points at the
       front of the keymap.  */
    Lisp_Object insertion_point;

    insertion_point = keymap;
    for (tail = XCDR (keymap); CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object elt;

	elt = XCAR (tail);
	if (VECTORP (elt))
	  {
	    if (NATNUMP (idx) && XFASTINT (idx) < ASIZE (elt))
	      {
		ASET (elt, XFASTINT (idx), def);
		return def;
	      }
	    insertion_point = tail;
	  }
	else if (CHAR_TABLE_P (elt))
	  {
	    /* Character codes with modifiers
	       are not included in a char-table.
	       All character codes without modifiers are included.  */
	    if (NATNUMP (idx) && !(XFASTINT (idx) & CHAR_MODIFIER_MASK))
	      {
		Faset (elt, idx,
		       /* `nil' has a special meaning for char-tables, so
			  we use something else to record an explicitly
			  unbound entry.  */
		       NILP (def) ? Qt : def);
		return def;
	      }
	    insertion_point = tail;
	  }
	else if (CONSP (elt))
	  {
	    if (EQ (idx, XCAR (elt)))
	      {
		XSETCDR (elt, def);
		return def;
	      }
	  }
	else if (EQ (elt, Qkeymap))
	  /* If we find a 'keymap' symbol in the spine of KEYMAP,
	     then we must have found the start of a second keymap
	     being used as the tail of KEYMAP, and a binding for IDX
	     should be inserted before it.  */
	  goto keymap_end;

	QUIT;
      }

  keymap_end:
    /* We have scanned the entire keymap, and not found a binding for
       IDX.  Let's add one.  */
    XSETCDR (insertion_point,
	     Fcons (Fcons (idx, def), XCDR (insertion_point)));
  }

  return def;
}

EXFUN (Fcopy_keymap, 1);

Lisp_Object
copy_keymap_item (elt)
     Lisp_Object elt;
{
  Lisp_Object res, tem;

  if (!CONSP (elt))
    return elt;

  res = tem = elt;

  /* Is this a new format menu item.  */
  if (EQ (XCAR (tem), Qmenu_item))
    {
      /* Copy cell with menu-item marker.  */
      res = elt = Fcons (XCAR (tem), XCDR (tem));
      tem = XCDR (elt);
      if (CONSP (tem))
	{
	  /* Copy cell with menu-item name.  */
	  XSETCDR (elt, Fcons (XCAR (tem), XCDR (tem)));
	  elt = XCDR (elt);
	  tem = XCDR (elt);
	}
      if (CONSP (tem))
	{
	  /* Copy cell with binding and if the binding is a keymap,
	     copy that.  */
	  XSETCDR (elt, Fcons (XCAR (tem), XCDR (tem)));
	  elt = XCDR (elt);
	  tem = XCAR (elt);
	  if (CONSP (tem) && EQ (XCAR (tem), Qkeymap))
	    XSETCAR (elt, Fcopy_keymap (tem));
	  tem = XCDR (elt);
	  if (CONSP (tem) && CONSP (XCAR (tem)))
	    /* Delete cache for key equivalences.  */
	    XSETCDR (elt, XCDR (tem));
	}
    }
  else
    {
      /* It may be an old fomat menu item.
	 Skip the optional menu string.  */
      if (STRINGP (XCAR (tem)))
	{
	  /* Copy the cell, since copy-alist didn't go this deep.  */
	  res = elt = Fcons (XCAR (tem), XCDR (tem));
	  tem = XCDR (elt);
	  /* Also skip the optional menu help string.  */
	  if (CONSP (tem) && STRINGP (XCAR (tem)))
	    {
	      XSETCDR (elt, Fcons (XCAR (tem), XCDR (tem)));
	      elt = XCDR (elt);
	      tem = XCDR (elt);
	    }
	  /* There may also be a list that caches key equivalences.
	     Just delete it for the new keymap.  */
	  if (CONSP (tem)
	      && CONSP (XCAR (tem))
	      && (NILP (XCAR (XCAR (tem)))
		  || VECTORP (XCAR (XCAR (tem)))))
	    {
	      XSETCDR (elt, XCDR (tem));
	      tem = XCDR (tem);
	    }
	  if (CONSP (tem) && EQ (XCAR (tem), Qkeymap))
	    XSETCDR (elt, Fcopy_keymap (tem));
	}
      else if (EQ (XCAR (tem), Qkeymap))
	res = Fcopy_keymap (elt);
    }
  return res;
}

void
copy_keymap_1 (chartable, idx, elt)
     Lisp_Object chartable, idx, elt;
{
  Faset (chartable, idx, copy_keymap_item (elt));
}

DEFUN ("copy-keymap", Fcopy_keymap, Scopy_keymap, 1, 1, 0,
       doc: /* Return a copy of the keymap KEYMAP.
The copy starts out with the same definitions of KEYMAP,
but changing either the copy or KEYMAP does not affect the other.
Any key definitions that are subkeymaps are recursively copied.
However, a key definition which is a symbol whose definition is a keymap
is not copied.  */)
     (keymap)
     Lisp_Object keymap;
{
  register Lisp_Object copy, tail;
  keymap = get_keymap (keymap, 1, 0);
  copy = tail = Fcons (Qkeymap, Qnil);
  keymap = XCDR (keymap);		/* Skip the `keymap' symbol.  */

  while (CONSP (keymap) && !EQ (XCAR (keymap), Qkeymap))
    {
      Lisp_Object elt = XCAR (keymap);
      if (CHAR_TABLE_P (elt))
	{
	  Lisp_Object indices[3];
	  elt = Fcopy_sequence (elt);
	  map_char_table (copy_keymap_1, Qnil, elt, elt, 0, indices);
	}
      else if (VECTORP (elt))
	{
	  int i;
	  elt = Fcopy_sequence (elt);
	  for (i = 0; i < ASIZE (elt); i++)
	    ASET (elt, i, copy_keymap_item (AREF (elt, i)));
	}
      else if (CONSP (elt))
	elt = Fcons (XCAR (elt), copy_keymap_item (XCDR (elt)));
      XSETCDR (tail, Fcons (elt, Qnil));
      tail = XCDR (tail);
      keymap = XCDR (keymap);
    }
  XSETCDR (tail, keymap);
  return copy;
}

/* Simple Keymap mutators and accessors.				*/

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 3, 0,
       doc: /* Args KEYMAP, KEY, DEF.  Define key sequence KEY, in KEYMAP, as DEF.
KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters meaning a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling)
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol.  When the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right),
 or a cons (KEYMAP . CHAR), meaning use definition of CHAR in map KEYMAP.

If KEYMAP is a sparse keymap, the pair binding KEY to DEF is added at
the front of KEYMAP.  */)
     (keymap, key, def)
     Lisp_Object keymap;
     Lisp_Object key;
     Lisp_Object def;
{
  register int idx;
  register Lisp_Object c;
  register Lisp_Object cmd;
  int metized = 0;
  int meta_bit;
  int length;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (keymap, key, def);
  keymap = get_keymap (keymap, 1, 1);

  if (!VECTORP (key) && !STRINGP (key))
    key = wrong_type_argument (Qarrayp, key);

  length = XFASTINT (Flength (key));
  if (length == 0)
    RETURN_UNGCPRO (Qnil);

  if (SYMBOLP (def) && !EQ (Vdefine_key_rebound_commands, Qt))
    Vdefine_key_rebound_commands = Fcons (def, Vdefine_key_rebound_commands);

  meta_bit = VECTORP (key) ? meta_modifier : 0x80;

  idx = 0;
  while (1)
    {
      c = Faref (key, make_number (idx));

      if (CONSP (c) && lucid_event_type_list_p (c))
	c = Fevent_convert_list (c);

      if (SYMBOLP (c))
	silly_event_symbol_error (c);

      if (INTEGERP (c)
	  && (XINT (c) & meta_bit)
	  && !metized)
	{
	  c = meta_prefix_char;
	  metized = 1;
	}
      else
	{
	  if (INTEGERP (c))
	    XSETINT (c, XINT (c) & ~meta_bit);

	  metized = 0;
	  idx++;
	}

      if (!INTEGERP (c) && !SYMBOLP (c) && !CONSP (c))
	error ("Key sequence contains invalid event");

      if (idx == length)
	RETURN_UNGCPRO (store_in_keymap (keymap, c, def));

      cmd = access_keymap (keymap, c, 0, 1, 1);

      /* If this key is undefined, make it a prefix.  */
      if (NILP (cmd))
	cmd = define_as_prefix (keymap, c);

      keymap = get_keymap (cmd, 0, 1);
      if (!CONSP (keymap))
	/* We must use Fkey_description rather than just passing key to
	   error; key might be a vector, not a string.  */
	error ("Key sequence %s uses invalid prefix characters",
	       XSTRING (Fkey_description (key))->data);
    }
}

/* This function may GC (it calls Fkey_binding).  */

DEFUN ("remap-command", Fremap_command, Sremap_command, 1, 1, 0,
       doc: /* Return the remapping for command COMMAND in current keymaps.
Returns nil if COMMAND is not remapped.  */)
     (command)
     Lisp_Object command;
{
  ASET (remap_command_vector, 1, command);
  return Fkey_binding (remap_command_vector, Qnil, Qt);
}

/* Value is number if KEY is too long; nil if valid but has no definition. */
/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 3, 0,
       doc: /* In keymap KEYMAP, look up key sequence KEY.  Return the definition.
nil means undefined.  See doc of `define-key' for kinds of definitions.

A number as value means KEY is "too long";
that is, characters or symbols in it except for the last one
fail to be a valid sequence of prefix characters in KEYMAP.
The number is how many characters at the front of KEY
it takes to reach a non-prefix command.

Normally, `lookup-key' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
recognize the default bindings, just as `read-key-sequence' does.  */)
     (keymap, key, accept_default)
     Lisp_Object keymap;
     Lisp_Object key;
     Lisp_Object accept_default;
{
  register int idx;
  register Lisp_Object cmd;
  register Lisp_Object c;
  int length;
  int t_ok = !NILP (accept_default);
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (keymap, key);
  keymap = get_keymap (keymap, 1, 1);

  if (!VECTORP (key) && !STRINGP (key))
    key = wrong_type_argument (Qarrayp, key);

  length = XFASTINT (Flength (key));
  if (length == 0)
    RETURN_UNGCPRO (keymap);

  idx = 0;
  while (1)
    {
      c = Faref (key, make_number (idx++));

      if (CONSP (c) && lucid_event_type_list_p (c))
	c = Fevent_convert_list (c);

      /* Turn the 8th bit of string chars into a meta modifier.  */
      if (XINT (c) & 0x80 && STRINGP (key))
	XSETINT (c, (XINT (c) | meta_modifier) & ~0x80);

      /* Allow string since binding for `menu-bar-select-buffer'
	 includes the buffer name in the key sequence.  */
      if (!INTEGERP (c) && !SYMBOLP (c) && !CONSP (c) && !STRINGP (c))
	error ("Key sequence contains invalid event");

      cmd = access_keymap (keymap, c, t_ok, 0, 1);
      if (idx == length)
	RETURN_UNGCPRO (cmd);

      keymap = get_keymap (cmd, 0, 1);
      if (!CONSP (keymap))
	RETURN_UNGCPRO (make_number (idx));

      QUIT;
    }
}

/* Make KEYMAP define event C as a keymap (i.e., as a prefix).
   Assume that currently it does not define C at all.
   Return the keymap.  */

static Lisp_Object
define_as_prefix (keymap, c)
     Lisp_Object keymap, c;
{
  Lisp_Object cmd;

  cmd = Fmake_sparse_keymap (Qnil);
  /* If this key is defined as a prefix in an inherited keymap,
     make it a prefix in this map, and make its definition
     inherit the other prefix definition.  */
  cmd = nconc2 (cmd, access_keymap (keymap, c, 0, 0, 0));
  store_in_keymap (keymap, c, cmd);

  return cmd;
}

/* Append a key to the end of a key sequence.  We always make a vector.  */

Lisp_Object
append_key (key_sequence, key)
     Lisp_Object key_sequence, key;
{
  Lisp_Object args[2];

  args[0] = key_sequence;

  args[1] = Fcons (key, Qnil);
  return Fvconcat (2, args);
}

/* Given a event type C which is a symbol,
   signal an error if is a mistake such as RET or M-RET or C-DEL, etc.  */

static void
silly_event_symbol_error (c)
     Lisp_Object c;
{
  Lisp_Object parsed, base, name, assoc;
  int modifiers;

  parsed = parse_modifiers (c);
  modifiers = (int) XUINT (XCAR (XCDR (parsed)));
  base = XCAR (parsed);
  name = Fsymbol_name (base);
  /* This alist includes elements such as ("RET" . "\\r").  */
  assoc = Fassoc (name, exclude_keys);

  if (! NILP (assoc))
    {
      char new_mods[sizeof ("\\A-\\C-\\H-\\M-\\S-\\s-")];
      char *p = new_mods;
      Lisp_Object keystring;
      if (modifiers & alt_modifier)
	{ *p++ = '\\'; *p++ = 'A'; *p++ = '-'; }
      if (modifiers & ctrl_modifier)
	{ *p++ = '\\'; *p++ = 'C'; *p++ = '-'; }
      if (modifiers & hyper_modifier)
	{ *p++ = '\\'; *p++ = 'H'; *p++ = '-'; }
      if (modifiers & meta_modifier)
	{ *p++ = '\\'; *p++ = 'M'; *p++ = '-'; }
      if (modifiers & shift_modifier)
	{ *p++ = '\\'; *p++ = 'S'; *p++ = '-'; }
      if (modifiers & super_modifier)
	{ *p++ = '\\'; *p++ = 's'; *p++ = '-'; }
      *p = 0;

      c = reorder_modifiers (c);
      keystring = concat2 (build_string (new_mods), XCDR (assoc));

      error ((modifiers & ~meta_modifier
	      ? "To bind the key %s, use [?%s], not [%s]"
	      : "To bind the key %s, use \"%s\", not [%s]"),
	     XSTRING (SYMBOL_NAME (c))->data, XSTRING (keystring)->data,
	     XSTRING (SYMBOL_NAME (c))->data);
    }
}

/* Global, local, and minor mode keymap stuff.				*/

/* We can't put these variables inside current_minor_maps, since under
   some systems, static gets macro-defined to be the empty string.
   Ickypoo.  */
static Lisp_Object *cmm_modes = NULL, *cmm_maps = NULL;
static int cmm_size = 0;

/* Error handler used in current_minor_maps.  */
static Lisp_Object
current_minor_maps_error ()
{
  return Qnil;
}

/* Store a pointer to an array of the keymaps of the currently active
   minor modes in *buf, and return the number of maps it contains.

   This function always returns a pointer to the same buffer, and may
   free or reallocate it, so if you want to keep it for a long time or
   hand it out to lisp code, copy it.  This procedure will be called
   for every key sequence read, so the nice lispy approach (return a
   new assoclist, list, what have you) for each invocation would
   result in a lot of consing over time.

   If we used xrealloc/xmalloc and ran out of memory, they would throw
   back to the command loop, which would try to read a key sequence,
   which would call this function again, resulting in an infinite
   loop.  Instead, we'll use realloc/malloc and silently truncate the
   list, let the key sequence be read, and hope some other piece of
   code signals the error.  */
int
current_minor_maps (modeptr, mapptr)
     Lisp_Object **modeptr, **mapptr;
{
  int i = 0;
  int list_number = 0;
  Lisp_Object alist, assoc, var, val;
  Lisp_Object emulation_alists;
  Lisp_Object lists[2];

  emulation_alists = Vemulation_mode_map_alists;
  lists[0] = Vminor_mode_overriding_map_alist;
  lists[1] = Vminor_mode_map_alist;

  for (list_number = 0; list_number < 2; list_number++)
    {
      if (CONSP (emulation_alists))
	{
	  alist = XCAR (emulation_alists);
	  emulation_alists = XCDR (emulation_alists);
	  if (SYMBOLP (alist))
	    alist = find_symbol_value (alist);
	  list_number = -1;
	}
      else
	alist = lists[list_number];

      for ( ; CONSP (alist); alist = XCDR (alist))
	if ((assoc = XCAR (alist), CONSP (assoc))
	    && (var = XCAR (assoc), SYMBOLP (var))
	    && (val = find_symbol_value (var), !EQ (val, Qunbound))
	    && !NILP (val))
	  {
	    Lisp_Object temp;

	    /* If a variable has an entry in Vminor_mode_overriding_map_alist,
	       and also an entry in Vminor_mode_map_alist,
	       ignore the latter.  */
	    if (list_number == 1)
	      {
		val = assq_no_quit (var, lists[0]);
		if (!NILP (val))
		  continue;
	      }

	    if (i >= cmm_size)
	      {
		int newsize, allocsize;
		Lisp_Object *newmodes, *newmaps;

		newsize = cmm_size == 0 ? 30 : cmm_size * 2;
		allocsize = newsize * sizeof *newmodes;

		/* Use malloc here.  See the comment above this function.
		   Avoid realloc here; it causes spurious traps on GNU/Linux [KFS] */
		BLOCK_INPUT;
		newmodes = (Lisp_Object *) malloc (allocsize);
		if (newmodes)
		  {
		    if (cmm_modes)
		      {
			bcopy (cmm_modes, newmodes, cmm_size * sizeof cmm_modes[0]);
			free (cmm_modes);
		      }
		    cmm_modes = newmodes;
		  }

		newmaps = (Lisp_Object *) malloc (allocsize);
		if (newmaps)
		  {
		    if (cmm_maps)
		      {
			bcopy (cmm_maps, newmaps, cmm_size * sizeof cmm_maps[0]);
			free (cmm_maps);
		      }
		    cmm_maps = newmaps;
		  }
		UNBLOCK_INPUT;

		if (newmodes == NULL || newmaps == NULL)
		  break;
		cmm_size = newsize;
	      }

	    /* Get the keymap definition--or nil if it is not defined.  */
	    temp = internal_condition_case_1 (Findirect_function,
					      XCDR (assoc),
					      Qerror, current_minor_maps_error);
	    if (!NILP (temp))
	      {
		cmm_modes[i] = var;
		cmm_maps [i] = temp;
		i++;
	      }
	  }
    }

  if (modeptr) *modeptr = cmm_modes;
  if (mapptr)  *mapptr  = cmm_maps;
  return i;
}

DEFUN ("current-active-maps", Fcurrent_active_maps, Scurrent_active_maps,
       0, 1, 0,
       doc: /* Return a list of the currently active keymaps.
OLP if non-nil indicates that we should obey `overriding-local-map' and
`overriding-terminal-local-map'.  */)
     (olp)
     Lisp_Object olp;
{
  Lisp_Object keymaps = Fcons (current_global_map, Qnil);

  if (!NILP (olp))
    {
      if (!NILP (Voverriding_local_map))
	keymaps = Fcons (Voverriding_local_map, keymaps);
      if (!NILP (current_kboard->Voverriding_terminal_local_map))
	keymaps = Fcons (current_kboard->Voverriding_terminal_local_map, keymaps);
    }
  if (NILP (XCDR (keymaps)))
    {
      Lisp_Object local;
      Lisp_Object *maps;
      int nmaps, i;

      local = get_local_map (PT, current_buffer, Qlocal_map);
      if (!NILP (local))
	keymaps = Fcons (local, keymaps);

      nmaps = current_minor_maps (0, &maps);

      for (i = --nmaps; i >= 0; i--)
	if (!NILP (maps[i]))
	  keymaps = Fcons (maps[i], keymaps);

      local = get_local_map (PT, current_buffer, Qkeymap);
      if (!NILP (local))
	keymaps = Fcons (local, keymaps);
    }

  return keymaps;
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 3, 0,
       doc: /* Return the binding for command KEY in current keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.

Normally, `key-binding' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
optional second argument ACCEPT-DEFAULT is non-nil, `key-binding' does
recognize the default bindings, just as `read-key-sequence' does.

Like the normal command loop, `key-binding' will remap the command
resulting from looking up KEY by looking up the command in the
currrent keymaps.  However, if the optional third argument NO-REMAP
is non-nil, `key-binding' returns the unmapped command.  */)
     (key, accept_default, no_remap)
     Lisp_Object key, accept_default, no_remap;
{
  Lisp_Object *maps, value;
  int nmaps, i;
  struct gcpro gcpro1;

  GCPRO1 (key);

  if (!NILP (current_kboard->Voverriding_terminal_local_map))
    {
      value = Flookup_key (current_kboard->Voverriding_terminal_local_map,
			   key, accept_default);
      if (! NILP (value) && !INTEGERP (value))
	goto done;
    }
  else if (!NILP (Voverriding_local_map))
    {
      value = Flookup_key (Voverriding_local_map, key, accept_default);
      if (! NILP (value) && !INTEGERP (value))
	goto done;
    }
  else
    {
      Lisp_Object local;

      local = get_local_map (PT, current_buffer, Qkeymap);
      if (! NILP (local))
	{
	  value = Flookup_key (local, key, accept_default);
	  if (! NILP (value) && !INTEGERP (value))
	    goto done;
	}

      nmaps = current_minor_maps (0, &maps);
      /* Note that all these maps are GCPRO'd
	 in the places where we found them.  */

      for (i = 0; i < nmaps; i++)
	if (! NILP (maps[i]))
	  {
	    value = Flookup_key (maps[i], key, accept_default);
	    if (! NILP (value) && !INTEGERP (value))
	      goto done;
	  }

      local = get_local_map (PT, current_buffer, Qlocal_map);
      if (! NILP (local))
	{
	  value = Flookup_key (local, key, accept_default);
	  if (! NILP (value) && !INTEGERP (value))
	    goto done;
	}
    }

  value = Flookup_key (current_global_map, key, accept_default);

 done:
  UNGCPRO;
  if (NILP (value) || INTEGERP (value))
    return Qnil;

  /* If the result of the ordinary keymap lookup is an interactive
     command, look for a key binding (ie. remapping) for that command.  */

  if (NILP (no_remap) && SYMBOLP (value))
    {
      Lisp_Object value1;
      if (value1 = Fremap_command (value), !NILP (value1))
	value = value1;
    }

  return value;
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("local-key-binding", Flocal_key_binding, Slocal_key_binding, 1, 2, 0,
       doc: /* Return the binding for command KEYS in current local keymap only.
KEYS is a string, a sequence of keystrokes.
The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
     (keys, accept_default)
     Lisp_Object keys, accept_default;
{
  register Lisp_Object map;
  map = current_buffer->keymap;
  if (NILP (map))
    return Qnil;
  return Flookup_key (map, keys, accept_default);
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("global-key-binding", Fglobal_key_binding, Sglobal_key_binding, 1, 2, 0,
       doc: /* Return the binding for command KEYS in current global keymap only.
KEYS is a string, a sequence of keystrokes.
The binding is probably a symbol with a function definition.
This function's return values are the same as those of lookup-key
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
     (keys, accept_default)
     Lisp_Object keys, accept_default;
{
  return Flookup_key (current_global_map, keys, accept_default);
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("minor-mode-key-binding", Fminor_mode_key_binding, Sminor_mode_key_binding, 1, 2, 0,
       doc: /* Find the visible minor mode bindings of KEY.
Return an alist of pairs (MODENAME . BINDING), where MODENAME is the
the symbol which names the minor mode binding KEY, and BINDING is
KEY's definition in that mode.  In particular, if KEY has no
minor-mode bindings, return nil.  If the first binding is a
non-prefix, all subsequent bindings will be omitted, since they would
be ignored.  Similarly, the list doesn't include non-prefix bindings
that come after prefix bindings.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
     (key, accept_default)
     Lisp_Object key, accept_default;
{
  Lisp_Object *modes, *maps;
  int nmaps;
  Lisp_Object binding;
  int i, j;
  struct gcpro gcpro1, gcpro2;

  nmaps = current_minor_maps (&modes, &maps);
  /* Note that all these maps are GCPRO'd
     in the places where we found them.  */

  binding = Qnil;
  GCPRO2 (key, binding);

  for (i = j = 0; i < nmaps; i++)
    if (!NILP (maps[i])
	&& !NILP (binding = Flookup_key (maps[i], key, accept_default))
	&& !INTEGERP (binding))
      {
	if (KEYMAPP (binding))
	  maps[j++] = Fcons (modes[i], binding);
	else if (j == 0)
	  RETURN_UNGCPRO (Fcons (Fcons (modes[i], binding), Qnil));
      }

  UNGCPRO;
  return Flist (j, maps);
}

DEFUN ("define-prefix-command", Fdefine_prefix_command, Sdefine_prefix_command, 1, 3, 0,
       doc: /* Define COMMAND as a prefix command.  COMMAND should be a symbol.
A new sparse keymap is stored as COMMAND's function definition and its value.
If a second optional argument MAPVAR is given, the map is stored as
its value instead of as COMMAND's value; but COMMAND is still defined
as a function.
The third optional argument NAME, if given, supplies a menu name
string for the map.  This is required to use the keymap as a menu.  */)
     (command, mapvar, name)
     Lisp_Object command, mapvar, name;
{
  Lisp_Object map;
  map = Fmake_sparse_keymap (name);
  Ffset (command, map);
  if (!NILP (mapvar))
    Fset (mapvar, map);
  else
    Fset (command, map);
  return command;
}

DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
       doc: /* Select KEYMAP as the global keymap.  */)
     (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap, 1, 1);
  current_global_map = keymap;

  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
       doc: /* Select KEYMAP as the local keymap.
If KEYMAP is nil, that means no local keymap.  */)
     (keymap)
     Lisp_Object keymap;
{
  if (!NILP (keymap))
    keymap = get_keymap (keymap, 1, 1);

  current_buffer->keymap = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
       doc: /* Return current buffer's local keymap, or nil if it has none.  */)
     ()
{
  return current_buffer->keymap;
}

DEFUN ("current-global-map", Fcurrent_global_map, Scurrent_global_map, 0, 0, 0,
       doc: /* Return the current global keymap.  */)
     ()
{
  return current_global_map;
}

DEFUN ("current-minor-mode-maps", Fcurrent_minor_mode_maps, Scurrent_minor_mode_maps, 0, 0, 0,
       doc: /* Return a list of keymaps for the minor modes of the current buffer.  */)
     ()
{
  Lisp_Object *maps;
  int nmaps = current_minor_maps (0, &maps);

  return Flist (nmaps, maps);
}

/* Help functions for describing and documenting keymaps.		*/


static void
accessible_keymaps_1 (key, cmd, maps, tail, thisseq, is_metized)
     Lisp_Object maps, tail, thisseq, key, cmd;
     int is_metized;		/* If 1, `key' is assumed to be INTEGERP.  */
{
  Lisp_Object tem;

  cmd = get_keyelt (cmd, 0);
  if (NILP (cmd))
    return;

  tem = get_keymap (cmd, 0, 0);
  if (CONSP (tem))
    {
      cmd = tem;
      /* Ignore keymaps that are already added to maps.  */
      tem = Frassq (cmd, maps);
      if (NILP (tem))
	{
	  /* If the last key in thisseq is meta-prefix-char,
	     turn it into a meta-ized keystroke.  We know
	     that the event we're about to append is an
	     ascii keystroke since we're processing a
	     keymap table.  */
	  if (is_metized)
	    {
	      int meta_bit = meta_modifier;
	      Lisp_Object last = make_number (XINT (Flength (thisseq)) - 1);
	      tem = Fcopy_sequence (thisseq);

	      Faset (tem, last, make_number (XINT (key) | meta_bit));

	      /* This new sequence is the same length as
		 thisseq, so stick it in the list right
		 after this one.  */
	      XSETCDR (tail,
		       Fcons (Fcons (tem, cmd), XCDR (tail)));
	    }
	  else
	    {
	      tem = append_key (thisseq, key);
	      nconc2 (tail, Fcons (Fcons (tem, cmd), Qnil));
	    }
	}
    }
}

static void
accessible_keymaps_char_table (args, index, cmd)
     Lisp_Object args, index, cmd;
{
  accessible_keymaps_1 (index, cmd,
			XCAR (XCAR (args)),
			XCAR (XCDR (args)),
			XCDR (XCDR (args)),
			XINT (XCDR (XCAR (args))));
}

/* This function cannot GC.  */

DEFUN ("accessible-keymaps", Faccessible_keymaps, Saccessible_keymaps,
       1, 2, 0,
       doc: /* Find all keymaps accessible via prefix characters from KEYMAP.
Returns a list of elements of the form (KEYS . MAP), where the sequence
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered
so that the KEYS increase in length.  The first element is ([] . KEYMAP).
An optional argument PREFIX, if non-nil, should be a key sequence;
then the value includes only maps for prefixes that start with PREFIX.  */)
     (keymap, prefix)
     Lisp_Object keymap, prefix;
{
  Lisp_Object maps, good_maps, tail;
  int prefixlen = 0;

  /* no need for gcpro because we don't autoload any keymaps.  */

  if (!NILP (prefix))
    prefixlen = XINT (Flength (prefix));

  if (!NILP (prefix))
    {
      /* If a prefix was specified, start with the keymap (if any) for
	 that prefix, so we don't waste time considering other prefixes.  */
      Lisp_Object tem;
      tem = Flookup_key (keymap, prefix, Qt);
      /* Flookup_key may give us nil, or a number,
	 if the prefix is not defined in this particular map.
	 It might even give us a list that isn't a keymap.  */
      tem = get_keymap (tem, 0, 0);
      if (CONSP (tem))
	{
	  /* Convert PREFIX to a vector now, so that later on
	     we don't have to deal with the possibility of a string.  */
	  if (STRINGP (prefix))
	    {
	      int i, i_byte, c;
	      Lisp_Object copy;

	      copy = Fmake_vector (make_number (XSTRING (prefix)->size), Qnil);
	      for (i = 0, i_byte = 0; i < XSTRING (prefix)->size;)
		{
		  int i_before = i;

		  FETCH_STRING_CHAR_ADVANCE (c, prefix, i, i_byte);
		  if (SINGLE_BYTE_CHAR_P (c) && (c & 0200))
		    c ^= 0200 | meta_modifier;
		  ASET (copy, i_before, make_number (c));
		}
	      prefix = copy;
	    }
	  maps = Fcons (Fcons (prefix, tem), Qnil);
	}
      else
	return Qnil;
    }
  else
    maps = Fcons (Fcons (Fmake_vector (make_number (0), Qnil),
			 get_keymap (keymap, 1, 0)),
		  Qnil);

  /* For each map in the list maps,
     look at any other maps it points to,
     and stick them at the end if they are not already in the list.

     This is a breadth-first traversal, where tail is the queue of
     nodes, and maps accumulates a list of all nodes visited.  */

  for (tail = maps; CONSP (tail); tail = XCDR (tail))
    {
      register Lisp_Object thisseq, thismap;
      Lisp_Object last;
      /* Does the current sequence end in the meta-prefix-char?  */
      int is_metized;

      thisseq = Fcar (Fcar (tail));
      thismap = Fcdr (Fcar (tail));
      last = make_number (XINT (Flength (thisseq)) - 1);
      is_metized = (XINT (last) >= 0
		    /* Don't metize the last char of PREFIX.  */
		    && XINT (last) >= prefixlen
		    && EQ (Faref (thisseq, last), meta_prefix_char));

      for (; CONSP (thismap); thismap = XCDR (thismap))
	{
	  Lisp_Object elt;

	  elt = XCAR (thismap);

	  QUIT;

	  if (CHAR_TABLE_P (elt))
	    {
	      Lisp_Object indices[3];

	      map_char_table (accessible_keymaps_char_table, Qnil,
			      elt, Fcons (Fcons (maps, make_number (is_metized)),
					  Fcons (tail, thisseq)),
			      0, indices);
	    }
	  else if (VECTORP (elt))
	    {
	      register int i;

	      /* Vector keymap.  Scan all the elements.  */
	      for (i = 0; i < ASIZE (elt); i++)
		accessible_keymaps_1 (make_number (i), AREF (elt, i),
				      maps, tail, thisseq, is_metized);

	    }
	  else if (CONSP (elt))
	    accessible_keymaps_1 (XCAR (elt), XCDR (elt),
				  maps, tail, thisseq,
				  is_metized && INTEGERP (XCAR (elt)));

	}
    }

  if (NILP (prefix))
    return maps;

  /* Now find just the maps whose access prefixes start with PREFIX.  */

  good_maps = Qnil;
  for (; CONSP (maps); maps = XCDR (maps))
    {
      Lisp_Object elt, thisseq;
      elt = XCAR (maps);
      thisseq = XCAR (elt);
      /* The access prefix must be at least as long as PREFIX,
	 and the first elements must match those of PREFIX.  */
      if (XINT (Flength (thisseq)) >= prefixlen)
	{
	  int i;
	  for (i = 0; i < prefixlen; i++)
	    {
	      Lisp_Object i1;
	      XSETFASTINT (i1, i);
	      if (!EQ (Faref (thisseq, i1), Faref (prefix, i1)))
		break;
	    }
	  if (i == prefixlen)
	    good_maps = Fcons (elt, good_maps);
	}
    }

  return Fnreverse (good_maps);
}

Lisp_Object Qsingle_key_description, Qkey_description;

/* This function cannot GC.  */

DEFUN ("key-description", Fkey_description, Skey_description, 1, 1, 0,
       doc: /* Return a pretty description of key-sequence KEYS.
Control characters turn into "C-foo" sequences, meta into "M-foo"
spaces are put between sequence elements, etc.  */)
     (keys)
     Lisp_Object keys;
{
  int len = 0;
  int i, i_byte;
  Lisp_Object sep;
  Lisp_Object *args = NULL;

  if (STRINGP (keys))
    {
      Lisp_Object vector;
      vector = Fmake_vector (Flength (keys), Qnil);
      for (i = 0, i_byte = 0; i < XSTRING (keys)->size; )
	{
	  int c;
	  int i_before = i;

	  FETCH_STRING_CHAR_ADVANCE (c, keys, i, i_byte);
	  if (SINGLE_BYTE_CHAR_P (c) && (c & 0200))
	    c ^= 0200 | meta_modifier;
	  XSETFASTINT (AREF (vector, i_before), c);
	}
      keys = vector;
    }

  if (VECTORP (keys))
    {
      /* In effect, this computes
	 (mapconcat 'single-key-description keys " ")
	 but we shouldn't use mapconcat because it can do GC.  */

      len = XVECTOR (keys)->size;
      sep = build_string (" ");
      /* This has one extra element at the end that we don't pass to Fconcat.  */
      args = (Lisp_Object *) alloca (len * 2 * sizeof (Lisp_Object));

      for (i = 0; i < len; i++)
	{
	  args[i * 2] = Fsingle_key_description (AREF (keys, i), Qnil);
	  args[i * 2 + 1] = sep;
	}
    }
  else if (CONSP (keys))
    {
      /* In effect, this computes
	 (mapconcat 'single-key-description keys " ")
	 but we shouldn't use mapconcat because it can do GC.  */

      len = XFASTINT (Flength (keys));
      sep = build_string (" ");
      /* This has one extra element at the end that we don't pass to Fconcat.  */
      args = (Lisp_Object *) alloca (len * 2 * sizeof (Lisp_Object));

      for (i = 0; i < len; i++)
	{
	  args[i * 2] = Fsingle_key_description (XCAR (keys), Qnil);
	  args[i * 2 + 1] = sep;
	  keys = XCDR (keys);
	}
    }
  else
    keys = wrong_type_argument (Qarrayp, keys);

  if (len == 0)
    return empty_string;
  return Fconcat (len * 2 - 1, args);
}

char *
push_key_description (c, p, force_multibyte)
     register unsigned int c;
     register char *p;
     int force_multibyte;
{
  unsigned c2;

  /* Clear all the meaningless bits above the meta bit.  */
  c &= meta_modifier | ~ - meta_modifier;
  c2 = c & ~(alt_modifier | ctrl_modifier | hyper_modifier
	     | meta_modifier | shift_modifier | super_modifier);

  if (c & alt_modifier)
    {
      *p++ = 'A';
      *p++ = '-';
      c -= alt_modifier;
    }
  if ((c & ctrl_modifier) != 0
      || (c2 < ' ' && c2 != 27 && c2 != '\t' && c2 != Ctl ('M')))
    {
      *p++ = 'C';
      *p++ = '-';
      c &= ~ctrl_modifier;
    }
  if (c & hyper_modifier)
    {
      *p++ = 'H';
      *p++ = '-';
      c -= hyper_modifier;
    }
  if (c & meta_modifier)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= meta_modifier;
    }
  if (c & shift_modifier)
    {
      *p++ = 'S';
      *p++ = '-';
      c -= shift_modifier;
    }
  if (c & super_modifier)
    {
      *p++ = 's';
      *p++ = '-';
      c -= super_modifier;
    }
  if (c < 040)
    {
      if (c == 033)
	{
	  *p++ = 'E';
	  *p++ = 'S';
	  *p++ = 'C';
	}
      else if (c == '\t')
	{
	  *p++ = 'T';
	  *p++ = 'A';
	  *p++ = 'B';
	}
      else if (c == Ctl ('M'))
	{
	  *p++ = 'R';
	  *p++ = 'E';
	  *p++ = 'T';
	}
      else
	{
	  /* `C-' already added above.  */
	  if (c > 0 && c <= Ctl ('Z'))
	    *p++ = c + 0140;
	  else
	    *p++ = c + 0100;
	}
    }
  else if (c == 0177)
    {
      *p++ = 'D';
      *p++ = 'E';
      *p++ = 'L';
    }
  else if (c == ' ')
   {
      *p++ = 'S';
      *p++ = 'P';
      *p++ = 'C';
    }
  else if (c < 128
	   || (NILP (current_buffer->enable_multibyte_characters)
	       && SINGLE_BYTE_CHAR_P (c)
	       && !force_multibyte))
    {
      *p++ = c;
    }
  else
    {
      int valid_p = SINGLE_BYTE_CHAR_P (c) || char_valid_p (c, 0);

      if (force_multibyte && valid_p)
	{
	  if (SINGLE_BYTE_CHAR_P (c))
	    c = unibyte_char_to_multibyte (c);
	  p += CHAR_STRING (c, p);
	}
      else if (NILP (current_buffer->enable_multibyte_characters)
	       || valid_p)
	{
	  int bit_offset;
	  *p++ = '\\';
	  /* The biggest character code uses 19 bits.  */
	  for (bit_offset = 18; bit_offset >= 0; bit_offset -= 3)
	    {
	      if (c >= (1 << bit_offset))
		*p++ = ((c & (7 << bit_offset)) >> bit_offset) + '0';
	    }
	}
      else
	p += CHAR_STRING (c, p);
    }

  return p;
}

/* This function cannot GC.  */

DEFUN ("single-key-description", Fsingle_key_description,
       Ssingle_key_description, 1, 2, 0,
       doc: /* Return a pretty description of command character KEY.
Control characters turn into C-whatever, etc.
Optional argument NO-ANGLES non-nil means don't put angle brackets
around function keys and event symbols.  */)
     (key, no_angles)
     Lisp_Object key, no_angles;
{
  if (CONSP (key) && lucid_event_type_list_p (key))
    key = Fevent_convert_list (key);

  key = EVENT_HEAD (key);

  if (INTEGERP (key))		/* Normal character */
    {
      unsigned int charset, c1, c2;
      int without_bits = XINT (key) & ~((-1) << CHARACTERBITS);

      if (SINGLE_BYTE_CHAR_P (without_bits))
	charset = 0;
      else
	SPLIT_CHAR (without_bits, charset, c1, c2);

      if (charset
	  && CHARSET_DEFINED_P (charset)
	  && ((c1 >= 0 && c1 < 32)
	      || (c2 >= 0 && c2 < 32)))
	{
	  /* Handle a generic character.  */
	  Lisp_Object name;
	  name = CHARSET_TABLE_INFO (charset, CHARSET_LONG_NAME_IDX);
	  CHECK_STRING (name);
	  return concat2 (build_string ("Character set "), name);
	}
      else
	{
	  char tem[KEY_DESCRIPTION_SIZE], *end;
	  int nbytes, nchars;
	  Lisp_Object string;

	  end = push_key_description (XUINT (key), tem, 1);
	  nbytes = end - tem;
	  nchars = multibyte_chars_in_text (tem, nbytes);
	  if (nchars == nbytes)
	    {
	      *end = '\0';
	      string = build_string (tem);
	    }
	  else
	    string = make_multibyte_string (tem, nchars, nbytes);
	  return string;
	}
    }
  else if (SYMBOLP (key))	/* Function key or event-symbol */
    {
      if (NILP (no_angles))
	{
	  char *buffer
	    = (char *) alloca (STRING_BYTES (XSTRING (SYMBOL_NAME (key))) + 5);
	  sprintf (buffer, "<%s>", XSTRING (SYMBOL_NAME (key))->data);
	  return build_string (buffer);
	}
      else
	return Fsymbol_name (key);
    }
  else if (STRINGP (key))	/* Buffer names in the menubar.  */
    return Fcopy_sequence (key);
  else
    error ("KEY must be an integer, cons, symbol, or string");
  return Qnil;
}

char *
push_text_char_description (c, p)
     register unsigned int c;
     register char *p;
{
  if (c >= 0200)
    {
      *p++ = 'M';
      *p++ = '-';
      c -= 0200;
    }
  if (c < 040)
    {
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  else if (c == 0177)
    {
      *p++ = '^';
      *p++ = '?';
    }
  else
    *p++ = c;
  return p;
}

/* This function cannot GC.  */

DEFUN ("text-char-description", Ftext_char_description, Stext_char_description, 1, 1, 0,
       doc: /* Return a pretty description of file-character CHARACTER.
Control characters turn into "^char", etc.  */)
     (character)
     Lisp_Object character;
{
  /* Currently MAX_MULTIBYTE_LENGTH is 4 (< 6).  */
  unsigned char str[6];
  int c;

  CHECK_NUMBER (character);

  c = XINT (character);
  if (!SINGLE_BYTE_CHAR_P (c))
    {
      int len = CHAR_STRING (c, str);

      return make_multibyte_string (str, 1, len);
    }

  *push_text_char_description (c & 0377, str) = 0;

  return build_string (str);
}

/* Return non-zero if SEQ contains only ASCII characters, perhaps with
   a meta bit.  */
static int
ascii_sequence_p (seq)
     Lisp_Object seq;
{
  int i;
  int len = XINT (Flength (seq));

  for (i = 0; i < len; i++)
    {
      Lisp_Object ii, elt;

      XSETFASTINT (ii, i);
      elt = Faref (seq, ii);

      if (!INTEGERP (elt)
	  || (XUINT (elt) & ~CHAR_META) >= 0x80)
	return 0;
    }

  return 1;
}


/* where-is - finding a command in a set of keymaps.			*/

static Lisp_Object where_is_internal ();
static Lisp_Object where_is_internal_1 ();
static void where_is_internal_2 ();

/* Like Flookup_key, but uses a list of keymaps SHADOW instead of a single map.
   Returns the first non-nil binding found in any of those maps.  */

static Lisp_Object
shadow_lookup (shadow, key, flag)
     Lisp_Object shadow, key, flag;
{
  Lisp_Object tail, value;

  for (tail = shadow; CONSP (tail); tail = XCDR (tail))
    {
      value = Flookup_key (XCAR (tail), key, flag);
      if (!NILP (value) && !NATNUMP (value))
	return value;
    }
  return Qnil;
}

/* This function can GC if Flookup_key autoloads any keymaps.  */

static Lisp_Object
where_is_internal (definition, keymaps, firstonly, noindirect, no_remap)
     Lisp_Object definition, keymaps;
     Lisp_Object firstonly, noindirect, no_remap;
{
  Lisp_Object maps = Qnil;
  Lisp_Object found, sequences;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  /* 1 means ignore all menu bindings entirely.  */
  int nomenus = !NILP (firstonly) && !EQ (firstonly, Qnon_ascii);

  /* If this command is remapped, then it has no key bindings
     of its own.  */
  if (NILP (no_remap) && SYMBOLP (definition))
    {
      Lisp_Object tem;
      if (tem = Fremap_command (definition), !NILP (tem))
	return Qnil;
    }

  found = keymaps;
  while (CONSP (found))
    {
      maps =
	nconc2 (maps,
		Faccessible_keymaps (get_keymap (XCAR (found), 1, 0), Qnil));
      found = XCDR (found);
    }

  GCPRO5 (definition, keymaps, maps, found, sequences);
  found = Qnil;
  sequences = Qnil;

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      /* Key sequence to reach map, and the map that it reaches */
      register Lisp_Object this, map;

      /* In order to fold [META-PREFIX-CHAR CHAR] sequences into
	 [M-CHAR] sequences, check if last character of the sequence
	 is the meta-prefix char.  */
      Lisp_Object last;
      int last_is_meta;

      this = Fcar (Fcar (maps));
      map  = Fcdr (Fcar (maps));
      last = make_number (XINT (Flength (this)) - 1);
      last_is_meta = (XINT (last) >= 0
		      && EQ (Faref (this, last), meta_prefix_char));

      /* if (nomenus && !ascii_sequence_p (this)) */
      if (nomenus && XINT (last) >= 0
	  && !INTEGERP (Faref (this, make_number (0))))
	/* If no menu entries should be returned, skip over the
	   keymaps bound to `menu-bar' and `tool-bar' and other
	   non-ascii prefixes like `C-down-mouse-2'.  */
	continue;

      QUIT;

      while (CONSP (map))
	{
	  /* Because the code we want to run on each binding is rather
	     large, we don't want to have two separate loop bodies for
	     sparse keymap bindings and tables; we want to iterate one
	     loop body over both keymap and vector bindings.

	     For this reason, if Fcar (map) is a vector, we don't
	     advance map to the next element until i indicates that we
	     have finished off the vector.  */
	  Lisp_Object elt, key, binding;
	  elt = XCAR (map);
	  map = XCDR (map);

	  sequences = Qnil;

	  QUIT;

	  /* Set key and binding to the current key and binding, and
	     advance map and i to the next binding.  */
	  if (VECTORP (elt))
	    {
	      Lisp_Object sequence;
	      int i;
	      /* In a vector, look at each element.  */
	      for (i = 0; i < XVECTOR (elt)->size; i++)
		{
		  binding = AREF (elt, i);
		  XSETFASTINT (key, i);
		  sequence = where_is_internal_1 (binding, key, definition,
						  noindirect, this,
						  last, nomenus, last_is_meta);
		  if (!NILP (sequence))
		    sequences = Fcons (sequence, sequences);
		}
	    }
	  else if (CHAR_TABLE_P (elt))
	    {
	      Lisp_Object indices[3];
	      Lisp_Object args;

	      args = Fcons (Fcons (Fcons (definition, noindirect),
				   Qnil), /* Result accumulator.  */
			    Fcons (Fcons (this, last),
				   Fcons (make_number (nomenus),
					  make_number (last_is_meta))));
	      map_char_table (where_is_internal_2, Qnil, elt, args,
			      0, indices);
	      sequences = XCDR (XCAR (args));
	    }
	  else if (CONSP (elt))
	    {
	      Lisp_Object sequence;

	      key = XCAR (elt);
	      binding = XCDR (elt);

	      sequence = where_is_internal_1 (binding, key, definition,
					      noindirect, this,
					      last, nomenus, last_is_meta);
	      if (!NILP (sequence))
		sequences = Fcons (sequence, sequences);
	    }


	  while (!NILP (sequences))
	    {
	      Lisp_Object sequence, remapped, function;

	      sequence = XCAR (sequences);
	      sequences = XCDR (sequences);

	      /* If the current sequence is a command remapping with
		 format [remap COMMAND], find the key sequences
		 which run COMMAND, and use those sequences instead.  */
	      remapped = Qnil;
	      if (NILP (no_remap)
		  && VECTORP (sequence) && XVECTOR (sequence)->size == 2
		  && EQ (AREF (sequence, 0), Qremap)
		  && (function = AREF (sequence, 1), SYMBOLP (function)))
		{
		  Lisp_Object remapped1;

		  remapped1 = where_is_internal (function, keymaps, firstonly, noindirect, Qt);
		  if (CONSP (remapped1))
		    {
		      /* Verify that this key binding actually maps to the
			 remapped command (see below).  */
		      if (!EQ (shadow_lookup (keymaps, XCAR (remapped1), Qnil), function))
			continue;
		      sequence = XCAR (remapped1);
		      remapped = XCDR (remapped1);
		      goto record_sequence;
		    }
		}

	      /* Verify that this key binding is not shadowed by another
		 binding for the same key, before we say it exists.

		 Mechanism: look for local definition of this key and if
		 it is defined and does not match what we found then
		 ignore this key.

		 Either nil or number as value from Flookup_key
		 means undefined.  */
	      if (!EQ (shadow_lookup (keymaps, sequence, Qnil), definition))
		continue;

	    record_sequence:
	      /* It is a true unshadowed match.  Record it, unless it's already
		 been seen (as could happen when inheriting keymaps).  */
	      if (NILP (Fmember (sequence, found)))
		found = Fcons (sequence, found);

	      /* If firstonly is Qnon_ascii, then we can return the first
		 binding we find.  If firstonly is not Qnon_ascii but not
		 nil, then we should return the first ascii-only binding
		 we find.  */
	      if (EQ (firstonly, Qnon_ascii))
		RETURN_UNGCPRO (sequence);
	      else if (!NILP (firstonly) && ascii_sequence_p (sequence))
		RETURN_UNGCPRO (sequence);

	      if (CONSP (remapped))
		{
		  sequence = XCAR (remapped);
		  remapped = XCDR (remapped);
		  goto record_sequence;
		}
	    }
	}
    }

  UNGCPRO;

  found = Fnreverse (found);

  /* firstonly may have been t, but we may have gone all the way through
     the keymaps without finding an all-ASCII key sequence.  So just
     return the best we could find.  */
  if (!NILP (firstonly))
    return Fcar (found);

  return found;
}

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 5, 0,
       doc: /* Return list of keys that invoke DEFINITION.
If KEYMAP is non-nil, search only KEYMAP and the global keymap.
If KEYMAP is nil, search all the currently active keymaps.
If KEYMAP is a list of keymaps, search only those keymaps.

If optional 3rd arg FIRSTONLY is non-nil, return the first key sequence found,
rather than a list of all possible key sequences.
If FIRSTONLY is the symbol `non-ascii', return the first binding found,
no matter what it is.
If FIRSTONLY has another non-nil value, prefer sequences of ASCII characters,
and entirely reject menu bindings.

If optional 4th arg NOINDIRECT is non-nil, don't follow indirections
to other keymaps or slots.  This makes it possible to search for an
indirect definition itself.

If optional 5th arg NO-REMAP is non-nil, don't search for key sequences
that invoke a command which is remapped to DEFINITION, but include the
remapped command in the returned list.  */)
     (definition, keymap, firstonly, noindirect, no_remap)
     Lisp_Object definition, keymap;
     Lisp_Object firstonly, noindirect, no_remap;
{
  Lisp_Object sequences, keymaps;
  /* 1 means ignore all menu bindings entirely.  */
  int nomenus = !NILP (firstonly) && !EQ (firstonly, Qnon_ascii);
  Lisp_Object result;

  /* Find the relevant keymaps.  */
  if (CONSP (keymap) && KEYMAPP (XCAR (keymap)))
    keymaps = keymap;
  else if (!NILP (keymap))
    keymaps = Fcons (keymap, Fcons (current_global_map, Qnil));
  else
    keymaps = Fcurrent_active_maps (Qnil);

  /* Only use caching for the menubar (i.e. called with (def nil t nil).
     We don't really need to check `keymap'.  */
  if (nomenus && NILP (noindirect) && NILP (keymap))
    {
      Lisp_Object *defns;
      int i, j, n;
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

      /* Check heuristic-consistency of the cache.  */
      if (NILP (Fequal (keymaps, where_is_cache_keymaps)))
	where_is_cache = Qnil;

      if (NILP (where_is_cache))
	{
	  /* We need to create the cache.  */
	  Lisp_Object args[2];
	  where_is_cache = Fmake_hash_table (0, args);
	  where_is_cache_keymaps = Qt;

	  /* Fill in the cache.  */
	  GCPRO5 (definition, keymaps, firstonly, noindirect, no_remap);
	  where_is_internal (definition, keymaps, firstonly, noindirect, no_remap);
	  UNGCPRO;

	  where_is_cache_keymaps = keymaps;
	}

      /* We want to process definitions from the last to the first.
	 Instead of consing, copy definitions to a vector and step
	 over that vector.  */
      sequences = Fgethash (definition, where_is_cache, Qnil);
      n = XINT (Flength (sequences));
      defns = (Lisp_Object *) alloca (n * sizeof *defns);
      for (i = 0; CONSP (sequences); sequences = XCDR (sequences))
	defns[i++] = XCAR (sequences);

      /* Verify that the key bindings are not shadowed.  Note that
	 the following can GC.  */
      GCPRO2 (definition, keymaps);
      result = Qnil;
      j = -1;
      for (i = n - 1; i >= 0; --i)
	if (EQ (shadow_lookup (keymaps, defns[i], Qnil), definition))
	  {
	    if (ascii_sequence_p (defns[i]))
	      break;
	    else if (j < 0)
	      j = i;
	  }

      result = i >= 0 ? defns[i] : (j >= 0 ? defns[j] : Qnil);
      UNGCPRO;
    }
  else
    {
      /* Kill the cache so that where_is_internal_1 doesn't think
	 we're filling it up.  */
      where_is_cache = Qnil;
      result = where_is_internal (definition, keymaps, firstonly, noindirect, no_remap);
    }

  return result;
}

/* This is the function that Fwhere_is_internal calls using map_char_table.
   ARGS has the form
   (((DEFINITION . NOINDIRECT) . (KEYMAP . RESULT))
    .
    ((THIS . LAST) . (NOMENUS . LAST_IS_META)))
   Since map_char_table doesn't really use the return value from this function,
   we the result append to RESULT, the slot in ARGS.

   This function can GC because it calls where_is_internal_1 which can
   GC.  */

static void
where_is_internal_2 (args, key, binding)
     Lisp_Object args, key, binding;
{
  Lisp_Object definition, noindirect, this, last;
  Lisp_Object result, sequence;
  int nomenus, last_is_meta;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (args, key, binding);
  result = XCDR (XCAR (args));
  definition = XCAR (XCAR (XCAR (args)));
  noindirect = XCDR (XCAR (XCAR (args)));
  this = XCAR (XCAR (XCDR (args)));
  last = XCDR (XCAR (XCDR (args)));
  nomenus = XFASTINT (XCAR (XCDR (XCDR (args))));
  last_is_meta = XFASTINT (XCDR (XCDR (XCDR (args))));

  sequence = where_is_internal_1 (binding, key, definition, noindirect,
				  this, last, nomenus, last_is_meta);

  if (!NILP (sequence))
    XSETCDR (XCAR (args), Fcons (sequence, result));

  UNGCPRO;
}


/* This function cannot GC.  */

static Lisp_Object
where_is_internal_1 (binding, key, definition, noindirect, this, last,
		     nomenus, last_is_meta)
     Lisp_Object binding, key, definition, noindirect, this, last;
     int nomenus, last_is_meta;
{
  Lisp_Object sequence;

  /* Search through indirections unless that's not wanted.  */
  if (NILP (noindirect))
    binding = get_keyelt (binding, 0);

  /* End this iteration if this element does not match
     the target.  */

  if (!(!NILP (where_is_cache)	/* everything "matches" during cache-fill.  */
	|| EQ (binding, definition)
	|| (CONSP (definition) && !NILP (Fequal (binding, definition)))))
    /* Doesn't match.  */
    return Qnil;

  /* We have found a match.  Construct the key sequence where we found it.  */
  if (INTEGERP (key) && last_is_meta)
    {
      sequence = Fcopy_sequence (this);
      Faset (sequence, last, make_number (XINT (key) | meta_modifier));
    }
  else
    sequence = append_key (this, key);

  if (!NILP (where_is_cache))
    {
      Lisp_Object sequences = Fgethash (binding, where_is_cache, Qnil);
      Fputhash (binding, Fcons (sequence, sequences), where_is_cache);
      return Qnil;
    }
  else
    return sequence;
}

/* describe-bindings - summarizing all the bindings in a set of keymaps.  */

DEFUN ("describe-buffer-bindings", Fdescribe_buffer_bindings, Sdescribe_buffer_bindings, 1, 3, 0,
       doc: /* Insert the list of all defined keys and their definitions.
The list is inserted in the current buffer, while the bindings are
looked up in BUFFER.
The optional argument PREFIX, if non-nil, should be a key sequence;
then we display only bindings that start with that prefix.
The optional argument MENUS, if non-nil, says to mention menu bindings.
\(Ordinarily these are omitted from the output.)  */)
     (buffer, prefix, menus)
     Lisp_Object buffer, prefix, menus;
{
  Lisp_Object outbuf, shadow;
  int nomenu = NILP (menus);
  register Lisp_Object start1;
  struct gcpro gcpro1;

  char *alternate_heading
    = "\
Keyboard translations:\n\n\
You type        Translation\n\
--------        -----------\n";

  shadow = Qnil;
  GCPRO1 (shadow);

  outbuf = Fcurrent_buffer ();

  /* Report on alternates for keys.  */
  if (STRINGP (Vkeyboard_translate_table) && !NILP (prefix))
    {
      int c;
      unsigned char *translate = XSTRING (Vkeyboard_translate_table)->data;
      int translate_len = XSTRING (Vkeyboard_translate_table)->size;

      for (c = 0; c < translate_len; c++)
	if (translate[c] != c)
	  {
	    char buf[KEY_DESCRIPTION_SIZE];
	    char *bufend;

	    if (alternate_heading)
	      {
		insert_string (alternate_heading);
		alternate_heading = 0;
	      }

	    bufend = push_key_description (translate[c], buf, 1);
	    insert (buf, bufend - buf);
	    Findent_to (make_number (16), make_number (1));
	    bufend = push_key_description (c, buf, 1);
	    insert (buf, bufend - buf);

	    insert ("\n", 1);
	  }

      insert ("\n", 1);
    }

  if (!NILP (Vkey_translation_map))
    describe_map_tree (Vkey_translation_map, 0, Qnil, prefix,
		       "Key translations", nomenu, 1, 0);


  /* Print the (major mode) local map.  */
  start1 = Qnil;
  if (!NILP (current_kboard->Voverriding_terminal_local_map))
    start1 = current_kboard->Voverriding_terminal_local_map;
  else if (!NILP (Voverriding_local_map))
    start1 = Voverriding_local_map;

  if (!NILP (start1))
    {
      describe_map_tree (start1, 1, shadow, prefix,
			 "\f\nOverriding Bindings", nomenu, 0, 0);
      shadow = Fcons (start1, shadow);
    }
  else
    {
      /* Print the minor mode and major mode keymaps.  */
      int i, nmaps;
      Lisp_Object *modes, *maps;

      /* Temporarily switch to `buffer', so that we can get that buffer's
	 minor modes correctly.  */
      Fset_buffer (buffer);

      nmaps = current_minor_maps (&modes, &maps);
      Fset_buffer (outbuf);

      start1 = get_local_map (BUF_PT (XBUFFER (buffer)),
			      XBUFFER (buffer), Qkeymap);
      if (!NILP (start1))
	{
	  describe_map_tree (start1, 1, shadow, prefix,
			     "\f\n`keymap' Property Bindings", nomenu, 0, 0);
	  shadow = Fcons (start1, shadow);
	}

      /* Print the minor mode maps.  */
      for (i = 0; i < nmaps; i++)
	{
	  /* The title for a minor mode keymap
	     is constructed at run time.
	     We let describe_map_tree do the actual insertion
	     because it takes care of other features when doing so.  */
	  char *title, *p;

	  if (!SYMBOLP (modes[i]))
	    abort();

	  p = title = (char *) alloca (42 + XSTRING (SYMBOL_NAME (modes[i]))->size);
	  *p++ = '\f';
	  *p++ = '\n';
	  *p++ = '`';
	  bcopy (XSTRING (SYMBOL_NAME (modes[i]))->data, p,
		 XSTRING (SYMBOL_NAME (modes[i]))->size);
	  p += XSTRING (SYMBOL_NAME (modes[i]))->size;
	  *p++ = '\'';
	  bcopy (" Minor Mode Bindings", p, sizeof (" Minor Mode Bindings") - 1);
	  p += sizeof (" Minor Mode Bindings") - 1;
	  *p = 0;

	  describe_map_tree (maps[i], 1, shadow, prefix, title, nomenu, 0, 0);
	  shadow = Fcons (maps[i], shadow);
	}

      start1 = get_local_map (BUF_PT (XBUFFER (buffer)),
			      XBUFFER (buffer), Qlocal_map);
      if (!NILP (start1))
	{
	  if (EQ (start1, XBUFFER (buffer)->keymap))
	    describe_map_tree (start1, 1, shadow, prefix,
			       "\f\nMajor Mode Bindings", nomenu, 0, 0);
	  else
	    describe_map_tree (start1, 1, shadow, prefix,
			       "\f\n`local-map' Property Bindings",
			       nomenu, 0, 0);

	  shadow = Fcons (start1, shadow);
	}
    }

  describe_map_tree (current_global_map, 1, shadow, prefix,
		     "\f\nGlobal Bindings", nomenu, 0, 1);

  /* Print the function-key-map translations under this prefix.  */
  if (!NILP (Vfunction_key_map))
    describe_map_tree (Vfunction_key_map, 0, Qnil, prefix,
		       "\f\nFunction key map translations", nomenu, 1, 0);

  UNGCPRO;
  return Qnil;
}

/* Insert a description of the key bindings in STARTMAP,
    followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
    (such as `undefined').
   If SHADOW is non-nil, it is a list of maps;
    don't mention keys which would be shadowed by any of them.
   PREFIX, if non-nil, says mention only keys that start with PREFIX.
   TITLE, if not 0, is a string to insert at the beginning.
   TITLE should not end with a colon or a newline; we supply that.
   If NOMENU is not 0, then omit menu-bar commands.

   If TRANSL is nonzero, the definitions are actually key translations
   so print strings and vectors differently.

   If ALWAYS_TITLE is nonzero, print the title even if there are no maps
   to look through.  */

void
describe_map_tree (startmap, partial, shadow, prefix, title, nomenu, transl,
		   always_title)
     Lisp_Object startmap, shadow, prefix;
     int partial;
     char *title;
     int nomenu;
     int transl;
     int always_title;
{
  Lisp_Object maps, orig_maps, seen, sub_shadows;
  struct gcpro gcpro1, gcpro2, gcpro3;
  int something = 0;
  char *key_heading
    = "\
key             binding\n\
---             -------\n";

  orig_maps = maps = Faccessible_keymaps (startmap, prefix);
  seen = Qnil;
  sub_shadows = Qnil;
  GCPRO3 (maps, seen, sub_shadows);

  if (nomenu)
    {
      Lisp_Object list;

      /* Delete from MAPS each element that is for the menu bar.  */
      for (list = maps; !NILP (list); list = XCDR (list))
	{
	  Lisp_Object elt, prefix, tem;

	  elt = Fcar (list);
	  prefix = Fcar (elt);
	  if (XVECTOR (prefix)->size >= 1)
	    {
	      tem = Faref (prefix, make_number (0));
	      if (EQ (tem, Qmenu_bar))
		maps = Fdelq (elt, maps);
	    }
	}
    }

  if (!NILP (maps) || always_title)
    {
      if (title)
	{
	  insert_string (title);
	  if (!NILP (prefix))
	    {
	      insert_string (" Starting With ");
	      insert1 (Fkey_description (prefix));
	    }
	  insert_string (":\n");
	}
      insert_string (key_heading);
      something = 1;
    }

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      register Lisp_Object elt, prefix, tail;

      elt = Fcar (maps);
      prefix = Fcar (elt);

      sub_shadows = Qnil;

      for (tail = shadow; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object shmap;

	  shmap = XCAR (tail);

	  /* If the sequence by which we reach this keymap is zero-length,
	     then the shadow map for this keymap is just SHADOW.  */
	  if ((STRINGP (prefix) && XSTRING (prefix)->size == 0)
	      || (VECTORP (prefix) && XVECTOR (prefix)->size == 0))
	    ;
	  /* If the sequence by which we reach this keymap actually has
	     some elements, then the sequence's definition in SHADOW is
	     what we should use.  */
	  else
	    {
	      shmap = Flookup_key (shmap, Fcar (elt), Qt);
	      if (INTEGERP (shmap))
		shmap = Qnil;
	    }

	  /* If shmap is not nil and not a keymap,
	     it completely shadows this map, so don't
	     describe this map at all.  */
	  if (!NILP (shmap) && !KEYMAPP (shmap))
	    goto skip;

	  if (!NILP (shmap))
	    sub_shadows = Fcons (shmap, sub_shadows);
	}

      /* Maps we have already listed in this loop shadow this map.  */
      for (tail = orig_maps; !EQ (tail, maps); tail = XCDR (tail))
	{
	  Lisp_Object tem;
	  tem = Fequal (Fcar (XCAR (tail)), prefix);
	  if (!NILP (tem))
	    sub_shadows = Fcons (XCDR (XCAR (tail)), sub_shadows);
	}

      describe_map (Fcdr (elt), prefix,
		    transl ? describe_translation : describe_command,
		    partial, sub_shadows, &seen, nomenu);

    skip: ;
    }

  if (something)
    insert_string ("\n");

  UNGCPRO;
}

static int previous_description_column;

static void
describe_command (definition, args)
     Lisp_Object definition, args;
{
  register Lisp_Object tem1;
  int column = (int) current_column (); /* iftc */
  int description_column;

  /* If column 16 is no good, go to col 32;
     but don't push beyond that--go to next line instead.  */
  if (column > 30)
    {
      insert_char ('\n');
      description_column = 32;
    }
  else if (column > 14 || (column > 10 && previous_description_column == 32))
    description_column = 32;
  else
    description_column = 16;

  Findent_to (make_number (description_column), make_number (1));
  previous_description_column = description_column;

  if (SYMBOLP (definition))
    {
      tem1 = SYMBOL_NAME (definition);
      insert1 (tem1);
      insert_string ("\n");
    }
  else if (STRINGP (definition) || VECTORP (definition))
    insert_string ("Keyboard Macro\n");
  else if (KEYMAPP (definition))
    insert_string ("Prefix Command\n");
  else
    insert_string ("??\n");
}

static void
describe_translation (definition, args)
     Lisp_Object definition, args;
{
  register Lisp_Object tem1;

  Findent_to (make_number (16), make_number (1));

  if (SYMBOLP (definition))
    {
      tem1 = SYMBOL_NAME (definition);
      insert1 (tem1);
      insert_string ("\n");
    }
  else if (STRINGP (definition) || VECTORP (definition))
    {
      insert1 (Fkey_description (definition));
      insert_string ("\n");
    }
  else if (KEYMAPP (definition))
    insert_string ("Prefix Command\n");
  else
    insert_string ("??\n");
}

/* Describe the contents of map MAP, assuming that this map itself is
   reached by the sequence of prefix keys KEYS (a string or vector).
   PARTIAL, SHADOW, NOMENU are as in `describe_map_tree' above.  */

static void
describe_map (map, keys, elt_describer, partial, shadow, seen, nomenu)
     register Lisp_Object map;
     Lisp_Object keys;
     void (*elt_describer) P_ ((Lisp_Object, Lisp_Object));
     int partial;
     Lisp_Object shadow;
     Lisp_Object *seen;
     int nomenu;
{
  Lisp_Object elt_prefix;
  Lisp_Object tail, definition, event;
  Lisp_Object tem;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2, gcpro3;

  suppress = Qnil;

  if (!NILP (keys) && XFASTINT (Flength (keys)) > 0)
    {
      /* Call Fkey_description first, to avoid GC bug for the other string.  */
      tem = Fkey_description (keys);
      elt_prefix = concat2 (tem, build_string (" "));
    }
  else
    elt_prefix = Qnil;

  if (partial)
    suppress = intern ("suppress-keymap");

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per keymap element, we don't want to cons up a
     fresh vector every time.  */
  kludge = Fmake_vector (make_number (1), Qnil);
  definition = Qnil;

  GCPRO3 (elt_prefix, definition, kludge);

  for (tail = map; CONSP (tail); tail = XCDR (tail))
    {
      QUIT;

      if (VECTORP (XCAR (tail))
	  || CHAR_TABLE_P (XCAR (tail)))
	describe_vector (XCAR (tail),
			 elt_prefix, Qnil, elt_describer, partial, shadow, map,
			 (int *)0, 0);
      else if (CONSP (XCAR (tail)))
	{
	  event = XCAR (XCAR (tail));

	  /* Ignore bindings whose "keys" are not really valid events.
	     (We get these in the frames and buffers menu.)  */
	  if (!(SYMBOLP (event) || INTEGERP (event)))
	    continue;

	  if (nomenu && EQ (event, Qmenu_bar))
	    continue;

	  definition = get_keyelt (XCDR (XCAR (tail)), 0);

	  /* Don't show undefined commands or suppressed commands.  */
	  if (NILP (definition)) continue;
	  if (SYMBOLP (definition) && partial)
	    {
	      tem = Fget (definition, suppress);
	      if (!NILP (tem))
		continue;
	    }

	  /* Don't show a command that isn't really visible
	     because a local definition of the same key shadows it.  */

	  ASET (kludge, 0, event);
	  if (!NILP (shadow))
	    {
	      tem = shadow_lookup (shadow, kludge, Qt);
	      if (!NILP (tem)) continue;
	    }

	  tem = Flookup_key (map, kludge, Qt);
	  if (!EQ (tem, definition)) continue;

	  if (first)
	    {
	      previous_description_column = 0;
	      insert ("\n", 1);
	      first = 0;
	    }

	  if (!NILP (elt_prefix))
	    insert1 (elt_prefix);

	  /* THIS gets the string to describe the character EVENT.  */
	  insert1 (Fsingle_key_description (event, Qnil));

	  /* Print a description of the definition of this character.
	     elt_describer will take care of spacing out far enough
	     for alignment purposes.  */
	  (*elt_describer) (definition, Qnil);
	}
      else if (EQ (XCAR (tail), Qkeymap))
	{
	  /* The same keymap might be in the structure twice, if we're
	     using an inherited keymap.  So skip anything we've already
	     encountered.  */
	  tem = Fassq (tail, *seen);
	  if (CONSP (tem) && !NILP (Fequal (XCAR (tem), keys)))
	    break;
	  *seen = Fcons (Fcons (tail, keys), *seen);
	}
    }

  UNGCPRO;
}

static void
describe_vector_princ (elt, fun)
     Lisp_Object elt, fun;
{
  Findent_to (make_number (16), make_number (1));
  call1 (fun, elt);
  Fterpri (Qnil);
}

DEFUN ("describe-vector", Fdescribe_vector, Sdescribe_vector, 1, 2, 0,
       doc: /* Insert a description of contents of VECTOR.
This is text showing the elements of vector matched against indices.  */)
     (vector, describer)
     Lisp_Object vector, describer;
{
  int count = specpdl_ptr - specpdl;
  if (NILP (describer))
    describer = intern ("princ");
  specbind (Qstandard_output, Fcurrent_buffer ());
  CHECK_VECTOR_OR_CHAR_TABLE (vector);
  describe_vector (vector, Qnil, describer, describe_vector_princ, 0,
		   Qnil, Qnil, (int *)0, 0);

  return unbind_to (count, Qnil);
}

/* Insert in the current buffer a description of the contents of VECTOR.
   We call ELT_DESCRIBER to insert the description of one value found
   in VECTOR.

   ELT_PREFIX describes what "comes before" the keys or indices defined
   by this vector.  This is a human-readable string whose size
   is not necessarily related to the situation.

   If the vector is in a keymap, ELT_PREFIX is a prefix key which
   leads to this keymap.

   If the vector is a chartable, ELT_PREFIX is the vector
   of bytes that lead to the character set or portion of a character
   set described by this chartable.

   If PARTIAL is nonzero, it means do not mention suppressed commands
   (that assumes the vector is in a keymap).

   SHADOW is a list of keymaps that shadow this map.
   If it is non-nil, then we look up the key in those maps
   and we don't mention it now if it is defined by any of them.

   ENTIRE_MAP is the keymap in which this vector appears.
   If the definition in effect in the whole map does not match
   the one in this vector, we ignore this one.

   When describing a sub-char-table, INDICES is a list of
   indices at higher levels in this char-table,
   and CHAR_TABLE_DEPTH says how many levels down we have gone.

   ARGS is simply passed as the second argument to ELT_DESCRIBER.  */

void
describe_vector (vector, elt_prefix, args, elt_describer,
		 partial, shadow, entire_map,
		 indices, char_table_depth)
     register Lisp_Object vector;
     Lisp_Object elt_prefix, args;
     void (*elt_describer) P_ ((Lisp_Object, Lisp_Object));
     int partial;
     Lisp_Object shadow;
     Lisp_Object entire_map;
     int *indices;
     int char_table_depth;
{
  Lisp_Object definition;
  Lisp_Object tem2;
  register int i;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2, gcpro3;
  /* Range of elements to be handled.  */
  int from, to;
  /* A flag to tell if a leaf in this level of char-table is not a
     generic character (i.e. a complete multibyte character).  */
  int complete_char;
  int character;
  int starting_i;

  suppress = Qnil;

  if (indices == 0)
    indices = (int *) alloca (3 * sizeof (int));

  definition = Qnil;

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per vector element, we don't want to cons up a
     fresh vector every time.  */
  kludge = Fmake_vector (make_number (1), Qnil);
  GCPRO3 (elt_prefix, definition, kludge);

  if (partial)
    suppress = intern ("suppress-keymap");

  if (CHAR_TABLE_P (vector))
    {
      if (char_table_depth == 0)
	{
	  /* VECTOR is a top level char-table.  */
	  complete_char = 1;
	  from = 0;
	  to = CHAR_TABLE_ORDINARY_SLOTS;
	}
      else
	{
	  /* VECTOR is a sub char-table.  */
	  if (char_table_depth >= 3)
	    /* A char-table is never that deep.  */
	    error ("Too deep char table");

	  complete_char
	    = (CHARSET_VALID_P (indices[0])
	       && ((CHARSET_DIMENSION (indices[0]) == 1
		    && char_table_depth == 1)
		   || char_table_depth == 2));

	  /* Meaningful elements are from 32th to 127th.  */
	  from = 32;
	  to = SUB_CHAR_TABLE_ORDINARY_SLOTS;
	}
    }
  else
    {
      /* This does the right thing for ordinary vectors.  */

      complete_char = 1;
      from = 0;
      to = XVECTOR (vector)->size;
    }

  for (i = from; i < to; i++)
    {
      QUIT;

      if (CHAR_TABLE_P (vector))
	{
	  if (char_table_depth == 0 && i >= CHAR_TABLE_SINGLE_BYTE_SLOTS)
	    complete_char = 0;

	  if (i >= CHAR_TABLE_SINGLE_BYTE_SLOTS
	      && !CHARSET_DEFINED_P (i - 128))
	    continue;

	  definition
	    = get_keyelt (XCHAR_TABLE (vector)->contents[i], 0);
	}
      else
	definition = get_keyelt (AREF (vector, i), 0);

      if (NILP (definition)) continue;

      /* Don't mention suppressed commands.  */
      if (SYMBOLP (definition) && partial)
	{
	  Lisp_Object tem;

	  tem = Fget (definition, suppress);

	  if (!NILP (tem)) continue;
	}

      /* Set CHARACTER to the character this entry describes, if any.
	 Also update *INDICES.  */
      if (CHAR_TABLE_P (vector))
	{
	  indices[char_table_depth] = i;

	  if (char_table_depth == 0)
	    {
	      character = i;
	      indices[0] = i - 128;
	    }
	  else if (complete_char)
	    {
	      character	= MAKE_CHAR (indices[0], indices[1], indices[2]);
	    }
	  else
	    character = 0;
	}
      else
	character = i;

      /* If this binding is shadowed by some other map, ignore it.  */
      if (!NILP (shadow) && complete_char)
	{
	  Lisp_Object tem;

	  ASET (kludge, 0, make_number (character));
	  tem = shadow_lookup (shadow, kludge, Qt);

	  if (!NILP (tem)) continue;
	}

      /* Ignore this definition if it is shadowed by an earlier
	 one in the same keymap.  */
      if (!NILP (entire_map) && complete_char)
	{
	  Lisp_Object tem;

	  ASET (kludge, 0, make_number (character));
	  tem = Flookup_key (entire_map, kludge, Qt);

	  if (!EQ (tem, definition))
	    continue;
	}

      if (first)
	{
	  if (char_table_depth == 0)
	    insert ("\n", 1);
	  first = 0;
	}

      /* For a sub char-table, show the depth by indentation.
	 CHAR_TABLE_DEPTH can be greater than 0 only for a char-table.  */
      if (char_table_depth > 0)
	insert ("    ", char_table_depth * 2); /* depth is 1 or 2.  */

      /* Output the prefix that applies to every entry in this map.  */
      if (!NILP (elt_prefix))
	insert1 (elt_prefix);

      /* Insert or describe the character this slot is for,
	 or a description of what it is for.  */
      if (SUB_CHAR_TABLE_P (vector))
	{
	  if (complete_char)
	    insert_char (character);
	  else
	    {
	      /* We need an octal representation for this block of
                 characters.  */
	      char work[16];
	      sprintf (work, "(row %d)", i);
	      insert (work, strlen (work));
	    }
	}
      else if (CHAR_TABLE_P (vector))
	{
	  if (complete_char)
	    insert1 (Fsingle_key_description (make_number (character), Qnil));
	  else
	    {
	      /* Print the information for this character set.  */
	      insert_string ("<");
	      tem2 = CHARSET_TABLE_INFO (i - 128, CHARSET_SHORT_NAME_IDX);
	      if (STRINGP (tem2))
		insert_from_string (tem2, 0, 0, XSTRING (tem2)->size,
				    STRING_BYTES (XSTRING (tem2)), 0);
	      else
		insert ("?", 1);
	      insert (">", 1);
	    }
	}
      else
	{
	  insert1 (Fsingle_key_description (make_number (character), Qnil));
	}

      /* If we find a sub char-table within a char-table,
	 scan it recursively; it defines the details for
	 a character set or a portion of a character set.  */
      if (CHAR_TABLE_P (vector) && SUB_CHAR_TABLE_P (definition))
	{
	  insert ("\n", 1);
	  describe_vector (definition, elt_prefix, args, elt_describer,
			   partial, shadow, entire_map,
			   indices, char_table_depth + 1);
	  continue;
	}

      starting_i = i;

      /* Find all consecutive characters or rows that have the same
         definition.  But, for elements of a top level char table, if
         they are for charsets, we had better describe one by one even
         if they have the same definition.  */
      if (CHAR_TABLE_P (vector))
	{
	  int limit = to;

	  if (char_table_depth == 0)
	    limit = CHAR_TABLE_SINGLE_BYTE_SLOTS;

	  while (i + 1 < limit
		 && (tem2 = get_keyelt (XCHAR_TABLE (vector)->contents[i + 1], 0),
		     !NILP (tem2))
		 && !NILP (Fequal (tem2, definition)))
	    i++;
	}
      else
	while (i + 1 < to
	       && (tem2 = get_keyelt (AREF (vector, i + 1), 0),
		   !NILP (tem2))
	       && !NILP (Fequal (tem2, definition)))
	  i++;


      /* If we have a range of more than one character,
	 print where the range reaches to.  */

      if (i != starting_i)
	{
	  insert (" .. ", 4);

	  if (!NILP (elt_prefix))
	    insert1 (elt_prefix);

	  if (CHAR_TABLE_P (vector))
	    {
	      if (char_table_depth == 0)
		{
		  insert1 (Fsingle_key_description (make_number (i), Qnil));
		}
	      else if (complete_char)
		{
		  indices[char_table_depth] = i;
		  character = MAKE_CHAR (indices[0], indices[1], indices[2]);
		  insert_char (character);
		}
	      else
		{
		  /* We need an octal representation for this block of
		     characters.  */
		  char work[16];
		  sprintf (work, "(row %d)", i);
		  insert (work, strlen (work));
		}
	    }
	  else
	    {
	      insert1 (Fsingle_key_description (make_number (i), Qnil));
	    }
	}

      /* Print a description of the definition of this character.
	 elt_describer will take care of spacing out far enough
	 for alignment purposes.  */
      (*elt_describer) (definition, args);
    }

  /* For (sub) char-table, print `defalt' slot at last.  */
  if (CHAR_TABLE_P (vector) && !NILP (XCHAR_TABLE (vector)->defalt))
    {
      insert ("    ", char_table_depth * 2);
      insert_string ("<<default>>");
      (*elt_describer) (XCHAR_TABLE (vector)->defalt, args);
    }

  UNGCPRO;
}

/* Apropos - finding all symbols whose names match a regexp.		*/
Lisp_Object apropos_predicate;
Lisp_Object apropos_accumulate;

static void
apropos_accum (symbol, string)
     Lisp_Object symbol, string;
{
  register Lisp_Object tem;

  tem = Fstring_match (string, Fsymbol_name (symbol), Qnil);
  if (!NILP (tem) && !NILP (apropos_predicate))
    tem = call1 (apropos_predicate, symbol);
  if (!NILP (tem))
    apropos_accumulate = Fcons (symbol, apropos_accumulate);
}

DEFUN ("apropos-internal", Fapropos_internal, Sapropos_internal, 1, 2, 0,
       doc: /* Show all symbols whose names contain match for REGEXP.
If optional 2nd arg PREDICATE is non-nil, (funcall PREDICATE SYMBOL) is done
for each symbol and a symbol is mentioned only if that returns non-nil.
Return list of symbols found.  */)
     (regexp, predicate)
     Lisp_Object regexp, predicate;
{
  struct gcpro gcpro1, gcpro2;
  CHECK_STRING (regexp);
  apropos_predicate = predicate;
  GCPRO2 (apropos_predicate, apropos_accumulate);
  apropos_accumulate = Qnil;
  map_obarray (Vobarray, apropos_accum, regexp);
  apropos_accumulate = Fsort (apropos_accumulate, Qstring_lessp);
  UNGCPRO;
  return apropos_accumulate;
}

void
syms_of_keymap ()
{
  Qkeymap = intern ("keymap");
  staticpro (&Qkeymap);

  /* Now we are ready to set up this property, so we can
     create char tables.  */
  Fput (Qkeymap, Qchar_table_extra_slots, make_number (0));

  /* Initialize the keymaps standardly used.
     Each one is the value of a Lisp variable, and is also
     pointed to by a C variable */

  global_map = Fmake_keymap (Qnil);
  Fset (intern ("global-map"), global_map);

  current_global_map = global_map;
  staticpro (&global_map);
  staticpro (&current_global_map);

  meta_map = Fmake_keymap (Qnil);
  Fset (intern ("esc-map"), meta_map);
  Ffset (intern ("ESC-prefix"), meta_map);

  control_x_map = Fmake_keymap (Qnil);
  Fset (intern ("ctl-x-map"), control_x_map);
  Ffset (intern ("Control-X-prefix"), control_x_map);

  exclude_keys
    = Fcons (Fcons (build_string ("DEL"), build_string ("\\d")),
	     Fcons (Fcons (build_string ("TAB"), build_string ("\\t")),
		    Fcons (Fcons (build_string ("RET"), build_string ("\\r")),
			   Fcons (Fcons (build_string ("ESC"), build_string ("\\e")),
				  Fcons (Fcons (build_string ("SPC"), build_string (" ")),
					 Qnil)))));
  staticpro (&exclude_keys);

  DEFVAR_LISP ("define-key-rebound-commands", &Vdefine_key_rebound_commands,
	       doc: /* List of commands given new key bindings recently.
This is used for internal purposes during Emacs startup;
don't alter it yourself.  */);
  Vdefine_key_rebound_commands = Qt;

  DEFVAR_LISP ("minibuffer-local-map", &Vminibuffer_local_map,
	       doc: /* Default keymap to use when reading from the minibuffer.  */);
  Vminibuffer_local_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("minibuffer-local-ns-map", &Vminibuffer_local_ns_map,
	       doc: /* Local keymap for the minibuffer when spaces are not allowed.  */);
  Vminibuffer_local_ns_map = Fmake_sparse_keymap (Qnil);
  Fset_keymap_parent (Vminibuffer_local_ns_map, Vminibuffer_local_map);

  DEFVAR_LISP ("minibuffer-local-completion-map", &Vminibuffer_local_completion_map,
	       doc: /* Local keymap for minibuffer input with completion.  */);
  Vminibuffer_local_completion_map = Fmake_sparse_keymap (Qnil);
  Fset_keymap_parent (Vminibuffer_local_completion_map, Vminibuffer_local_map);

  DEFVAR_LISP ("minibuffer-local-must-match-map", &Vminibuffer_local_must_match_map,
	       doc: /* Local keymap for minibuffer input with completion, for exact match.  */);
  Vminibuffer_local_must_match_map = Fmake_sparse_keymap (Qnil);
  Fset_keymap_parent (Vminibuffer_local_must_match_map,
		      Vminibuffer_local_completion_map);

  DEFVAR_LISP ("minor-mode-map-alist", &Vminor_mode_map_alist,
	       doc: /* Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings iff VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.  */);
  Vminor_mode_map_alist = Qnil;

  DEFVAR_LISP ("minor-mode-overriding-map-alist", &Vminor_mode_overriding_map_alist,
	       doc: /* Alist of keymaps to use for minor modes, in current major mode.
This variable is a alist just like `minor-mode-map-alist', and it is
used the same way (and before `minor-mode-map-alist'); however,
it is provided for major modes to bind locally.  */);
  Vminor_mode_overriding_map_alist = Qnil;

  DEFVAR_LISP ("emulation-mode-map-alists", &Vemulation_mode_map_alists,
	       doc: /* List of keymap alists to use for emulations modes.
It is intended for modes or packages using multiple minor-mode keymaps.
Each element is a keymap alist just like `minor-mode-map-alist', or a
symbol with a variable binding which is a keymap alist, and it is used
the same way.  The "active" keymaps in each alist are used before
`minor-mode-map-alist' and `minor-mode-overriding-map-alist'.  */);
  Vemulation_mode_map_alists = Qnil;


  DEFVAR_LISP ("function-key-map", &Vfunction_key_map,
	       doc: /* Keymap mapping ASCII function key sequences onto their preferred forms.
This allows Emacs to recognize function keys sent from ASCII
terminals at any point in a key sequence.

The `read-key-sequence' function replaces any subsequence bound by
`function-key-map' with its binding.  More precisely, when the active
keymaps have no binding for the current key sequence but
`function-key-map' binds a suffix of the sequence to a vector or string,
`read-key-sequence' replaces the matching suffix with its binding, and
continues with the new sequence.

The events that come from bindings in `function-key-map' are not
themselves looked up in `function-key-map'.

For example, suppose `function-key-map' binds `ESC O P' to [f1].
Typing `ESC O P' to `read-key-sequence' would return [f1].  Typing
`C-x ESC O P' would return [?\\C-x f1].  If [f1] were a prefix
key, typing `ESC O P x' would return [f1 x].  */);
  Vfunction_key_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("key-translation-map", &Vkey_translation_map,
	       doc: /* Keymap of key translations that can override keymaps.
This keymap works like `function-key-map', but comes after that,
and applies even for keys that have ordinary bindings.  */);
  Vkey_translation_map = Qnil;

  Qsingle_key_description = intern ("single-key-description");
  staticpro (&Qsingle_key_description);

  Qkey_description = intern ("key-description");
  staticpro (&Qkey_description);

  Qkeymapp = intern ("keymapp");
  staticpro (&Qkeymapp);

  Qnon_ascii = intern ("non-ascii");
  staticpro (&Qnon_ascii);

  Qmenu_item = intern ("menu-item");
  staticpro (&Qmenu_item);

  Qremap = intern ("remap");
  staticpro (&Qremap);

  remap_command_vector = Fmake_vector (make_number (2), Qremap);
  staticpro (&remap_command_vector);

  where_is_cache_keymaps = Qt;
  where_is_cache = Qnil;
  staticpro (&where_is_cache);
  staticpro (&where_is_cache_keymaps);

  defsubr (&Skeymapp);
  defsubr (&Skeymap_parent);
  defsubr (&Skeymap_prompt);
  defsubr (&Sset_keymap_parent);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Scopy_keymap);
  defsubr (&Sremap_command);
  defsubr (&Skey_binding);
  defsubr (&Slocal_key_binding);
  defsubr (&Sglobal_key_binding);
  defsubr (&Sminor_mode_key_binding);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Sdefine_prefix_command);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Scurrent_global_map);
  defsubr (&Scurrent_minor_mode_maps);
  defsubr (&Scurrent_active_maps);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Sdescribe_vector);
  defsubr (&Ssingle_key_description);
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Sdescribe_buffer_bindings);
  defsubr (&Sapropos_internal);
}

void
keys_of_keymap ()
{
  initial_define_key (global_map, 033, "ESC-prefix");
  initial_define_key (global_map, Ctl('X'), "Control-X-prefix");
}
