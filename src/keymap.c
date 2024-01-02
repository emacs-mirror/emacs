/* Manipulation of keymaps
   Copyright (C) 1985-1988, 1993-1995, 1998-2024 Free Software
   Foundation, Inc.

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

/* Old BUGS:
   - [M-C-a] != [?\M-\C-a]
   - [M-f2] != [?\e f2].
   - (define-key map [menu-bar foo] <bla>) does not always place <bla>
     at the head of the menu (if `foo' was already bound earlier and
     then unbound, for example).
   TODO:
   - allow many more Meta -> ESC mappings (like Hyper -> C-e for Emacspeak)
   - Think about the various defaulting that's currently hard-coded in
     keyboard.c (uppercase->lowercase, char->charset, button-events, ...)
     and make it more generic.  Maybe we should allow mappings of the
     form (PREDICATE . BINDING) as generalization of the default binding,
     tho probably a cleaner way to attack this is to allow functional
     keymaps (i.e. keymaps that are implemented as functions that implement
     a few different methods like `lookup', `map', ...).
   - Make [a] equivalent to [?a].
   BEWARE:
   - map-keymap should work meaningfully even if entries are added/removed
     to the keymap while iterating through it:
       start - removed <= visited <= start + added
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>

#include "lisp.h"
#include "commands.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "termhooks.h"
#include "blockinput.h"
#include "puresize.h"
#include "intervals.h"
#include "keymap.h"
#include "window.h"

/* Actually allocate storage for these variables.  */

Lisp_Object current_global_map;	/* Current global keymap.  */

/* Alist of elements like (DEL . "\d").  */
static Lisp_Object exclude_keys;

/* Pre-allocated 2-element vector for Fcommand_remapping to use.  */
static Lisp_Object command_remapping_vector;

/* Char table for the backwards-compatibility part in Flookup_key.  */
static Lisp_Object unicode_case_table;

/* Hash table used to cache a reverse-map to speed up calls to where-is.  */
static Lisp_Object where_is_cache;
/* Which keymaps are reverse-stored in the cache.  */
static Lisp_Object where_is_cache_keymaps;

static Lisp_Object store_in_keymap (Lisp_Object, Lisp_Object, Lisp_Object,
				    bool);

static Lisp_Object define_as_prefix (Lisp_Object, Lisp_Object);
static void describe_vector (Lisp_Object, Lisp_Object, Lisp_Object,
                             void (*) (Lisp_Object, Lisp_Object), bool,
                             Lisp_Object, Lisp_Object, bool, bool);
static void silly_event_symbol_error (Lisp_Object);
static Lisp_Object get_keyelt (Lisp_Object, bool);

static void
CHECK_VECTOR_OR_CHAR_TABLE (Lisp_Object x)
{
  CHECK_TYPE (VECTORP (x) || CHAR_TABLE_P (x), Qvector_or_char_table_p, x);
}

/* Keymap object support - constructors and predicates.			*/

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 1, 0,
       doc: /* Construct and return a new keymap, of the form (keymap CHARTABLE . ALIST).
CHARTABLE is a char-table that holds the bindings for all characters
without modifiers.  All entries in it are initially nil, meaning
"command undefined".  ALIST is an assoc-list which holds bindings for
function keys, mouse events, and any other things that appear in the
input stream.  Initially, ALIST is nil.

The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with `x-popup-menu'.  */)
  (Lisp_Object string)
{
  Lisp_Object tail = !NILP (string) ? list1 (string) : Qnil;
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
  (Lisp_Object string)
{
  if (!NILP (string))
    {
      if (!NILP (Vpurify_flag))
	string = Fpurecopy (string);
      return list2 (Qkeymap, string);
    }
  return list1 (Qkeymap);
}

void
initial_define_lispy_key (Lisp_Object keymap, const char *keyname, const char *defname)
{
  store_in_keymap (keymap, intern_c_string (keyname),
		   intern_c_string (defname), false);
}

DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
       doc: /* Return t if OBJECT is a keymap.

A keymap is a list (keymap . ALIST),
or a symbol whose function definition is itself a keymap.
ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);
a vector of densely packed bindings for small character codes
is also allowed as an element.  */)
  (Lisp_Object object)
{
  return (KEYMAPP (object) ? Qt : Qnil);
}

DEFUN ("keymap-prompt", Fkeymap_prompt, Skeymap_prompt, 1, 1, 0,
       doc: /* Return the prompt-string of a keymap MAP.
If non-nil, the prompt is shown in the echo-area
when reading a key-sequence to be looked-up in this keymap.  */)
  (Lisp_Object map)
{
  map = get_keymap (map, 0, 0);
  while (CONSP (map))
    {
      Lisp_Object tem = XCAR (map);
      if (STRINGP (tem))
	return tem;
      else if (KEYMAPP (tem))
	{
	  tem = Fkeymap_prompt (tem);
	  if (!NILP (tem))
	    return tem;
	}
      map = XCDR (map);
    }
  return Qnil;
}

/* Check that OBJECT is a keymap (after dereferencing through any
   symbols).  If it is, return it.

   If AUTOLOAD and if OBJECT is a symbol whose function value
   is an autoload form, do the autoload and try again.
   If AUTOLOAD, callers must assume GC is possible.

   ERROR_IF_NOT_KEYMAP controls how we respond if OBJECT isn't a keymap.
   If ERROR_IF_NOT_KEYMAP, signal an error; otherwise,
   just return Qnil.

   Note that most of the time, we don't want to pursue autoloads.
   Functions like Faccessible_keymaps which scan entire keymap trees
   shouldn't load every autoloaded keymap.  I'm not sure about this,
   but it seems to me that only read_key_sequence, Flookup_key, and
   Fdefine_key should cause keymaps to be autoloaded.

   This function can GC when AUTOLOAD is true, because it calls
   Fautoload_do_load which can GC.  */

Lisp_Object
get_keymap (Lisp_Object object, bool error_if_not_keymap, bool autoload)
{
 autoload_retry:
  if (NILP (object))
    goto end;
  if (CONSP (object) && EQ (XCAR (object), Qkeymap))
    return object;

  Lisp_Object tem = indirect_function (object);
  if (CONSP (tem))
    {
      if (EQ (XCAR (tem), Qkeymap))
	return tem;

      /* Should we do an autoload?  Autoload forms for keymaps have
	 Qkeymap as their fifth element.  */
      if ((autoload || !error_if_not_keymap) && EQ (XCAR (tem), Qautoload)
	  && SYMBOLP (object))
	{
	  Lisp_Object tail;

	  tail = Fnth (make_fixnum (4), tem);
	  if (EQ (tail, Qkeymap))
	    {
	      if (autoload)
		{
		  Fautoload_do_load (tem, object, Qnil);
		  goto autoload_retry;
		}
	      else
	      	return object;
	    }
	}
    }

 end:
  if (error_if_not_keymap)
    wrong_type_argument (Qkeymapp, object);
  return Qnil;
}

/* Return the parent map of KEYMAP, or nil if it has none.
   We assume that KEYMAP is a valid keymap.  */

static Lisp_Object
keymap_parent (Lisp_Object keymap, bool autoload)
{
  keymap = get_keymap (keymap, 1, autoload);

  /* Skip past the initial element `keymap'.  */
  Lisp_Object list = XCDR (keymap);
  for (; CONSP (list); list = XCDR (list))
    {
      /* See if there is another `keymap'.  */
      if (KEYMAPP (list))
	return list;
    }

  return get_keymap (list, 0, autoload);
}

DEFUN ("keymap-parent", Fkeymap_parent, Skeymap_parent, 1, 1, 0,
       doc: /* Return the parent keymap of KEYMAP.
If KEYMAP has no parent, return nil.  */)
  (Lisp_Object keymap)
{
  return keymap_parent (keymap, 1);
}

/* Check whether MAP is one of MAPS parents.  */
static bool
keymap_memberp (Lisp_Object map, Lisp_Object maps)
{
  if (NILP (map)) return 0;
  while (KEYMAPP (maps) && !EQ (map, maps))
    maps = keymap_parent (maps, 0);
  return (EQ (map, maps));
}

/* Set the parent keymap of MAP to PARENT.  */

DEFUN ("set-keymap-parent", Fset_keymap_parent, Sset_keymap_parent, 2, 2, 0,
       doc: /* Modify KEYMAP to set its parent map to PARENT.
Return PARENT.  PARENT should be nil or another keymap.  */)
  (Lisp_Object keymap, Lisp_Object parent)
{
  /* Flush any reverse-map cache.  */
  where_is_cache = Qnil; where_is_cache_keymaps = Qt;

  keymap = get_keymap (keymap, 1, 1);

  if (!NILP (parent))
    {
      parent = get_keymap (parent, 1, 0);

      /* Check for cycles.  */
      if (keymap_memberp (keymap, parent))
	error ("Cyclic keymap inheritance");
    }

  /* Skip past the initial element `keymap'.  */
  Lisp_Object prev = keymap;
  while (1)
    {
      Lisp_Object list = XCDR (prev);
      /* If there is a parent keymap here, replace it.
	 If we came to the end, add the parent in PREV.  */
      if (!CONSP (list) || KEYMAPP (list))
	{
	  CHECK_IMPURE (prev, XCONS (prev));
	  XSETCDR (prev, parent);
	  return parent;
	}
      prev = list;
    }
}


/* Look up IDX in MAP.  IDX may be any sort of event.
   Note that this does only one level of lookup; IDX must be a single
   event, not a sequence.

   MAP must be a keymap or a list of keymaps.

   If T_OK, bindings for Qt are treated as default
   bindings; any key left unmentioned by other tables and bindings is
   given the binding of Qt.

   If not T_OK, bindings for Qt are not treated specially.

   If NOINHERIT, don't accept a subkeymap found in an inherited keymap.

   Return Qunbound if no binding was found (and return Qnil if a nil
   binding was found).  */

static Lisp_Object
access_keymap_1 (Lisp_Object map, Lisp_Object idx,
		 bool t_ok, bool noinherit, bool autoload)
{
  /* If idx is a list (some sort of mouse click, perhaps?),
     the index we want to use is the car of the list, which
     ought to be a symbol.  */
  idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (SYMBOLP (idx))
    idx = reorder_modifiers (idx);
  else if (FIXNUMP (idx))
    /* Clobber the high bits that can be present on a machine
       with more than 24 bits of integer.  */
    XSETFASTINT (idx, XFIXNUM (idx) & (CHAR_META | (CHAR_META - 1)));

  /* Handle the special meta -> esc mapping.  */
  if (FIXNUMP (idx) && XFIXNAT (idx) & meta_modifier)
    {
      /* See if there is a meta-map.  If there's none, there is
         no binding for IDX, unless a default binding exists in MAP.  */
      Lisp_Object event_meta_binding, event_meta_map;
      /* A strange value in which Meta is set would cause
	 infinite recursion.  Protect against that.  */
      if (XFIXNUM (meta_prefix_char) & CHAR_META)
	meta_prefix_char = make_fixnum (27);
      event_meta_binding = access_keymap_1 (map, meta_prefix_char, t_ok,
					    noinherit, autoload);
      event_meta_map = get_keymap (event_meta_binding, 0, autoload);
      if (CONSP (event_meta_map))
	{
	  map = event_meta_map;
	  idx = make_fixnum (XFIXNAT (idx) & ~meta_modifier);
	}
      else if (t_ok)
	/* Set IDX to t, so that we only find a default binding.  */
	idx = Qt;
      else
	/* An explicit nil binding, or no binding at all.  */
	return NILP (event_meta_binding) ? Qnil : Qunbound;
    }

  /* t_binding is where we put a default binding that applies,
     to use in case we do not find a binding specifically
     for this key sequence.  */
  {
    Lisp_Object tail;
    Lisp_Object t_binding = Qunbound;
    Lisp_Object retval = Qunbound;
    Lisp_Object retval_tail = Qnil;

    for (tail = (CONSP (map) && EQ (Qkeymap, XCAR (map))) ? XCDR (map) : map;
	 (CONSP (tail)
	  || (tail = get_keymap (tail, 0, autoload), CONSP (tail)));
	 tail = XCDR (tail))
      {
	/* Qunbound in VAL means we have found no binding.  */
	Lisp_Object val = Qunbound;
	Lisp_Object binding = XCAR (tail);
	Lisp_Object submap = get_keymap (binding, 0, autoload);

	if (EQ (binding, Qkeymap))
	  {
	    if (noinherit || NILP (retval))
	      /* If NOINHERIT, stop here, the rest is inherited.  */
	      break;
	    else if (!BASE_EQ (retval, Qunbound))
	      {
		Lisp_Object parent_entry;
		eassert (KEYMAPP (retval));
		parent_entry
		  = get_keymap (access_keymap_1 (tail, idx,
						 t_ok, 0, autoload),
				0, autoload);
		if (KEYMAPP (parent_entry))
		  {
		    if (CONSP (retval_tail))
		      XSETCDR (retval_tail, parent_entry);
		    else
		      {
			retval_tail = Fcons (retval, parent_entry);
			retval = Fcons (Qkeymap, retval_tail);
		      }
		  }
		break;
	      }
	  }
	else if (CONSP (submap))
	  {
	    val = access_keymap_1 (submap, idx, t_ok, noinherit, autoload);
	  }
	else if (CONSP (binding))
	  {
	    Lisp_Object key = XCAR (binding);

	    if (EQ (key, idx))
	      val = XCDR (binding);
	    else if (t_ok && EQ (key, Qt))
	      {
		t_binding = XCDR (binding);
		t_ok = 0;
	      }
	  }
	else if (VECTORP (binding))
	  {
	    if (FIXNUMP (idx) && XFIXNAT (idx) < ASIZE (binding))
	      val = AREF (binding, XFIXNAT (idx));
	  }
	else if (CHAR_TABLE_P (binding))
	  {
	    /* Character codes with modifiers
	       are not included in a char-table.
	       All character codes without modifiers are included.  */
	    if (FIXNUMP (idx) && (XFIXNAT (idx) & CHAR_MODIFIER_MASK) == 0)
	      {
		val = Faref (binding, idx);
		/* nil has a special meaning for char-tables, so
		   we use something else to record an explicitly
		   unbound entry.  */
		if (NILP (val))
		  val = Qunbound;
	      }
	  }

	/* If we found a binding, clean it up and return it.  */
	if (!BASE_EQ (val, Qunbound))
	  {
	    if (EQ (val, Qt))
	      /* A Qt binding is just like an explicit nil binding
		 (i.e. it shadows any parent binding but not bindings in
		 keymaps of lower precedence).  */
	      val = Qnil;

	    val = get_keyelt (val, autoload);

	    if (!KEYMAPP (val))
	      {
		if (NILP (retval) || BASE_EQ (retval, Qunbound))
		  retval = val;
		if (!NILP (val))
		  break;  /* Shadows everything that follows.  */
	      }
	    else if (NILP (retval) || BASE_EQ (retval, Qunbound))
	      retval = val;
	    else if (CONSP (retval_tail))
	      {
		XSETCDR (retval_tail, list1 (val));
		retval_tail = XCDR (retval_tail);
	      }
	    else
	      {
		retval_tail = list1 (val);
		retval = Fcons (Qkeymap, Fcons (retval, retval_tail));
	      }
	  }
	maybe_quit ();
      }

    return BASE_EQ (Qunbound, retval)
           ? get_keyelt (t_binding, autoload) : retval;
  }
}

Lisp_Object
access_keymap (Lisp_Object map, Lisp_Object idx,
	       bool t_ok, bool noinherit, bool autoload)
{
  Lisp_Object val = access_keymap_1 (map, idx, t_ok, noinherit, autoload);
  return BASE_EQ (val, Qunbound) ? Qnil : val;
}

static void
map_keymap_item (map_keymap_function_t fun, Lisp_Object args, Lisp_Object key, Lisp_Object val, void *data)
{
  if (EQ (val, Qt))
    val = Qnil;
  (*fun) (key, val, args, data);
}

union map_keymap
{
  struct
  {
    map_keymap_function_t fun;
    Lisp_Object args;
    void *data;
  } s;
  GCALIGNED_UNION_MEMBER
};
verify (GCALIGNED (union map_keymap));

static void
map_keymap_char_table_item (Lisp_Object args, Lisp_Object key, Lisp_Object val)
{
  if (!NILP (val))
    {
      /* If the key is a range, make a copy since map_char_table modifies
	 it in place.  */
      if (CONSP (key))
	key = Fcons (XCAR (key), XCDR (key));
      union map_keymap *md = XFIXNUMPTR (args);
      map_keymap_item (md->s.fun, md->s.args, key, val, md->s.data);
    }
}

/* Call FUN for every binding in MAP and stop at (and return) the parent.
   FUN is called with 4 arguments: FUN (KEY, BINDING, ARGS, DATA).  */
static Lisp_Object
map_keymap_internal (Lisp_Object map,
		     map_keymap_function_t fun,
		     Lisp_Object args,
		     void *data)
{
  Lisp_Object tail
    = (CONSP (map) && EQ (Qkeymap, XCAR (map))) ? XCDR (map) : map;

  for (; CONSP (tail) && !EQ (Qkeymap, XCAR (tail)); tail = XCDR (tail))
    {
      Lisp_Object binding = XCAR (tail);

      if (KEYMAPP (binding))	/* An embedded parent.  */
	break;
      else if (CONSP (binding))
	map_keymap_item (fun, args, XCAR (binding), XCDR (binding), data);
      else if (VECTORP (binding))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = ASIZE (binding);
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      map_keymap_item (fun, args, character, AREF (binding, c), data);
	    }
	}
      else if (CHAR_TABLE_P (binding))
	{
	  union map_keymap mapdata = {{fun, args, data}};
	  map_char_table (map_keymap_char_table_item, Qnil, binding,
			  make_pointer_integer (&mapdata));
	}
    }

  return tail;
}

static void
map_keymap_call (Lisp_Object key, Lisp_Object val, Lisp_Object fun, void *dummy)
{
  call2 (fun, key, val);
}

/* Same as map_keymap_internal, but traverses parent keymaps as well.
   AUTOLOAD indicates that autoloaded keymaps should be loaded.  */
void
map_keymap (Lisp_Object map, map_keymap_function_t fun, Lisp_Object args,
	    void *data, bool autoload)
{
  map = get_keymap (map, 1, autoload);
  while (CONSP (map))
    {
      if (KEYMAPP (XCAR (map)))
	{
	  map_keymap (XCAR (map), fun, args, data, autoload);
	  map = XCDR (map);
	}
      else
	map = map_keymap_internal (map, fun, args, data);
      if (!CONSP (map))
	map = get_keymap (map, 0, autoload);
    }
}

/* Same as map_keymap, but does it right, properly eliminating duplicate
   bindings due to inheritance.   */
void
map_keymap_canonical (Lisp_Object map, map_keymap_function_t fun, Lisp_Object args, void *data)
{
  /* map_keymap_canonical may be used from redisplay (e.g. when building menus)
     so be careful to ignore errors and to inhibit redisplay.  */
  map = safe_call1 (Qkeymap_canonicalize, map);
  /* No need to use `map_keymap' here because canonical map has no parent.  */
  map_keymap_internal (map, fun, args, data);
}

DEFUN ("map-keymap-internal", Fmap_keymap_internal, Smap_keymap_internal, 2, 2, 0,
       doc: /* Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.
If KEYMAP has a parent, this function returns it without processing it.  */)
  (Lisp_Object function, Lisp_Object keymap)
{
  keymap = get_keymap (keymap, 1, 1);
  keymap = map_keymap_internal (keymap, map_keymap_call, function, NULL);
  return keymap;
}

DEFUN ("map-keymap", Fmap_keymap, Smap_keymap, 2, 3, 0,
       doc: /* Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.

If KEYMAP has a parent, the parent's bindings are included as well.
This works recursively: if the parent has itself a parent, then the
grandparent's bindings are also included and so on.

For more information, see Info node `(elisp) Keymaps'.

usage: (map-keymap FUNCTION KEYMAP)  */)
  (Lisp_Object function, Lisp_Object keymap, Lisp_Object sort_first)
{
  if (! NILP (sort_first))
    return call2 (intern ("map-keymap-sorted"), function, keymap);

  map_keymap (keymap, map_keymap_call, function, NULL, 1);
  return Qnil;
}

DEFUN ("keymap--get-keyelt", Fkeymap__get_keyelt, Skeymap__get_keyelt, 2, 2, 0,
       doc: /* Given OBJECT which was found in a slot in a keymap,
trace indirect definitions to get the actual definition of that slot.
An indirect definition is a list of the form
(KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
and INDEX is the object to look up in KEYMAP to yield the definition.

Also if OBJECT has a menu string as the first element,
remove that.  Also remove a menu help string as second element.

If AUTOLOAD, load autoloadable keymaps
that are referred to with indirection.  */)
  (Lisp_Object object, Lisp_Object autoload)
{
  return get_keyelt (object, NILP (autoload) ? false : true);
}

/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is the object to look up in KEYMAP to yield the definition.

   Also if OBJECT has a menu string as the first element,
   remove that.  Also remove a menu help string as second element.

   If AUTOLOAD, load autoloadable keymaps
   that are referred to with indirection.

   This can GC because menu_item_eval_property calls Feval.  */

static Lisp_Object
get_keyelt (Lisp_Object object, bool autoload)
{
  while (1)
    {
      if (!(CONSP (object)))
	/* This is really the value.  */
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
	object = XCDR (object);

      else
	return object;
    }
}

static Lisp_Object
store_in_keymap (Lisp_Object keymap, register Lisp_Object idx,
		 Lisp_Object def, bool remove)
{
  /* Flush any reverse-map cache.  */
  where_is_cache = Qnil;
  where_is_cache_keymaps = Qt;

  if (EQ (idx, Qkeymap))
    error ("`keymap' is reserved for embedded parent maps");

  /* If we are preparing to dump, and DEF is a menu element
     with a menu item indicator, copy it to ensure it is not pure.  */
  if (CONSP (def) && PURE_P (XCONS (def))
      && (EQ (XCAR (def), Qmenu_item) || STRINGP (XCAR (def))))
    def = Fcons (XCAR (def), XCDR (def));

  if (!CONSP (keymap) || !EQ (XCAR (keymap), Qkeymap))
    error ("attempt to define a key in a non-keymap");

  /* If idx is a cons, and the car part is a character, idx must be of
     the form (FROM-CHAR . TO-CHAR).  */
  if (CONSP (idx) && CHARACTERP (XCAR (idx)))
    CHECK_CHARACTER_CDR (idx);
  else
    /* If idx is a list (some sort of mouse click, perhaps?),
       the index we want to use is the car of the list, which
       ought to be a symbol.  */
    idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (SYMBOLP (idx))
    idx = reorder_modifiers (idx);
  else if (FIXNUMP (idx))
    /* Clobber the high bits that can be present on a machine
       with more than 24 bits of integer.  */
    XSETFASTINT (idx, XFIXNUM (idx) & (CHAR_META | (CHAR_META - 1)));

  /* Scan the keymap for a binding of idx.  */
  {
    Lisp_Object tail;

    /* The cons after which we should insert new bindings.  If the
       keymap has a table element, we record its position here, so new
       bindings will go after it; this way, the table will stay
       towards the front of the alist and character lookups in dense
       keymaps will remain fast.  Otherwise, this just points at the
       front of the keymap.  */
    Lisp_Object insertion_point = keymap;
    for (tail = XCDR (keymap); CONSP (tail); tail = XCDR (tail))
      {
	Lisp_Object elt = XCAR (tail);
	if (VECTORP (elt))
	  {
	    if (FIXNATP (idx) && XFIXNAT (idx) < ASIZE (elt))
	      {
		CHECK_IMPURE (elt, XVECTOR (elt));
		ASET (elt, XFIXNAT (idx), def);
		return def;
	      }
	    else if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	      {
		int from = XFIXNAT (XCAR (idx));
		int to = XFIXNAT (XCDR (idx));

		if (to >= ASIZE (elt))
		  to = ASIZE (elt) - 1;
		for (; from <= to; from++)
		  ASET (elt, from, def);
		if (to == XFIXNAT (XCDR (idx)))
		  /* We have defined all keys in IDX.  */
		  return def;
	      }
	    insertion_point = tail;
	  }
	else if (CHAR_TABLE_P (elt))
	  {
	    Lisp_Object sdef = def;
	    if (remove)
	      sdef = Qnil;
	    /* nil has a special meaning for char-tables, so
	       we use something else to record an explicitly
	       unbound entry.  */
	    else if (NILP (sdef))
	      sdef = Qt;

	    /* Character codes with modifiers
	       are not included in a char-table.
	       All character codes without modifiers are included.  */
	    if (FIXNATP (idx) && !(XFIXNAT (idx) & CHAR_MODIFIER_MASK))
	      {
		Faset (elt, idx, sdef);
		return def;
	      }
	    else if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	      {
		Fset_char_table_range (elt, idx, sdef);
		return def;
	      }
	    insertion_point = tail;
	  }
	else if (CONSP (elt))
	  {
	    if (EQ (Qkeymap, XCAR (elt)))
	      { /* A sub keymap.  This might be due to a lookup that found
		   two matching bindings (maybe because of a sub keymap).
		   It almost never happens (since the second binding normally
		   only happens in the inherited part of the keymap), but
		   if it does, we want to update the sub-keymap since the
		   main one might be temporary (built by access_keymap).  */
		tail = insertion_point = elt;
	      }
	    else if (EQ (idx, XCAR (elt)))
	      {
		CHECK_IMPURE (elt, XCONS (elt));
		if (remove)
		  /* Remove the element. */
		  insertion_point = Fdelq (elt, insertion_point);
		else
		  /* Just set the definition. */
		  XSETCDR (elt, def);
		return def;
	      }
	    else if (CONSP (idx)
		     && CHARACTERP (XCAR (idx))
		     && CHARACTERP (XCAR (elt)))
	      {
		int from = XFIXNAT (XCAR (idx));
		int to = XFIXNAT (XCDR (idx));

		if (from <= XFIXNAT (XCAR (elt))
		    && to >= XFIXNAT (XCAR (elt)))
		  {
		    if (remove)
		      insertion_point = Fdelq (elt, insertion_point);
		    else
		      XSETCDR (elt, def);
		    if (from == to)
		      return def;
		  }
	      }
	  }
	else if (EQ (elt, Qkeymap))
	  /* If we find a 'keymap' symbol in the spine of KEYMAP,
	     then we must have found the start of a second keymap
	     being used as the tail of KEYMAP, and a binding for IDX
	     should be inserted before it.  */
	  goto keymap_end;

	maybe_quit ();
      }

  keymap_end:
    /* We have scanned the entire keymap, and not found a binding for
       IDX.  Let's add one.  */
    if (!remove)
      {
	Lisp_Object elt;

	if (CONSP (idx) && CHARACTERP (XCAR (idx)))
	  {
	    /* IDX specifies a range of characters, and not all of them
	       were handled yet, which means this keymap doesn't have a
	       char-table.  So, we insert a char-table now.  */
	    elt = Fmake_char_table (Qkeymap, Qnil);
	    Fset_char_table_range (elt, idx, NILP (def) ? Qt : def);
	  }
	else
	  elt = Fcons (idx, def);
	CHECK_IMPURE (insertion_point, XCONS (insertion_point));
	XSETCDR (insertion_point, Fcons (elt, XCDR (insertion_point)));
      }
  }

  return def;
}

static Lisp_Object copy_keymap_1 (Lisp_Object keymap, int depth);

static Lisp_Object
copy_keymap_item (Lisp_Object elt, int depth)
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
	    XSETCAR (elt, copy_keymap_1 (tem, depth));
	  tem = XCDR (elt);
	}
    }
  else
    {
      /* It may be an old format menu item.
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
	  if (CONSP (tem) && EQ (XCAR (tem), Qkeymap))
	    XSETCDR (elt, copy_keymap_1 (tem, depth));
	}
      else if (EQ (XCAR (tem), Qkeymap))
	res = copy_keymap_1 (elt, depth);
    }
  return res;
}

static void
copy_keymap_set_char_table (Lisp_Object chartable_and_depth, Lisp_Object idx,
			    Lisp_Object elt)
{
  Fset_char_table_range
    (XCAR (chartable_and_depth), idx,
     copy_keymap_item (elt, XFIXNUM (XCDR (chartable_and_depth))));
}

static Lisp_Object
copy_keymap_1 (Lisp_Object keymap, int depth)
{
  Lisp_Object copy, tail;

  if (depth > 100)
    error ("Possible infinite recursion when copying keymap");

  keymap = get_keymap (keymap, 1, 0);
  copy = tail = list1 (Qkeymap);
  keymap = XCDR (keymap);		/* Skip the `keymap' symbol.  */

  while (CONSP (keymap) && !EQ (XCAR (keymap), Qkeymap))
    {
      Lisp_Object elt = XCAR (keymap);
      if (CHAR_TABLE_P (elt))
	{
	  elt = Fcopy_sequence (elt);
	  map_char_table (copy_keymap_set_char_table, Qnil, elt,
			  Fcons (elt, make_fixnum (depth + 1)));
	}
      else if (VECTORP (elt))
	{
	  elt = Fcopy_sequence (elt);
	  for (int i = 0; i < ASIZE (elt); i++)
	    ASET (elt, i, copy_keymap_item (AREF (elt, i), depth + 1));
	}
      else if (CONSP (elt))
	{
	  if (EQ (XCAR (elt), Qkeymap))
	    /* This is a sub keymap.  */
	    elt = copy_keymap_1 (elt, depth + 1);
	  else
	    elt = Fcons (XCAR (elt), copy_keymap_item (XCDR (elt), depth + 1));
	}
      XSETCDR (tail, list1 (elt));
      tail = XCDR (tail);
      keymap = XCDR (keymap);
    }
  XSETCDR (tail, keymap);
  return copy;
}

DEFUN ("copy-keymap", Fcopy_keymap, Scopy_keymap, 1, 1, 0,
       doc: /* Return a copy of the keymap KEYMAP.

Note that this is almost never needed.  If you want a keymap that's like
another yet with a few changes, you should use keymap inheritance rather
than copying.  That is, something like:

    (defvar-keymap foo-map
      :parent <theirmap>
      ...)

Or, if you need to support Emacs versions older than 29:

    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map <theirmap>)
      (define-key map ...)
      ...)

After performing `copy-keymap', the copy starts out with the same definitions
of KEYMAP, but changing either the copy or KEYMAP does not affect the other.
Any key definitions that are subkeymaps are recursively copied.
However, a key definition which is a symbol whose definition is a keymap
is not copied.  */)
  (Lisp_Object keymap)
{
  return copy_keymap_1 (keymap, 0);
}


/* Simple Keymap mutators and accessors.				*/

static Lisp_Object
possibly_translate_key_sequence (Lisp_Object key, ptrdiff_t *length)
{
  if (VECTORP (key) && ASIZE (key) == 1 && STRINGP (AREF (key, 0)))
    {
      /* KEY is on the ["C-c"] format, so translate to internal
	 format.  */
      if (NILP (Ffboundp (Qkey_valid_p)))
	xsignal2 (Qerror,
		  build_string ("`key-valid-p' is not defined, so this syntax can't be used: %s"),
		  key);
      /* If key-valid-p is unhappy about KEY, we return it as-is.
         This happens when menu items define as bindings strings that
         should be inserted into the buffer, not commands.  See
         bug#64927, for example.  */
      if (NILP (call1 (Qkey_valid_p, AREF (key, 0))))
	return key;
      key = call1 (Qkey_parse, AREF (key, 0));
      *length = CHECK_VECTOR_OR_STRING (key);
      if (*length == 0)
	xsignal2 (Qerror, build_string ("Invalid `key-parse' syntax: %S"), key);
    }

  return key;
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 4, 0,
       doc: /* In KEYMAP, define key sequence KEY as DEF.
This is a legacy function; see `keymap-set' for the recommended
function to use instead.

KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right) and
    STRING is the menu item name (which is used only if the containing
    keymap has been created with a menu name, see `make-keymap'),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)

If REMOVE is non-nil, the definition will be removed.  This is almost
the same as setting the definition to nil, but makes a difference if
the KEYMAP has a parent, and KEY is shadowing the same binding in the
parent.  With REMOVE, subsequent lookups will return the binding in
the parent, and with a nil DEF, the lookups will return nil.

If KEYMAP is a sparse keymap with a binding for KEY, the existing
binding is altered.  If there is no binding for KEY, the new pair
binding KEY to DEF is added at the front of KEYMAP.  */)
  (Lisp_Object keymap, Lisp_Object key, Lisp_Object def, Lisp_Object remove)
{
  bool metized = false;

  keymap = get_keymap (keymap, 1, 1);

  ptrdiff_t length = CHECK_VECTOR_OR_STRING (key);
  if (length == 0)
    return Qnil;

  int meta_bit = (VECTORP (key) || (STRINGP (key) && STRING_MULTIBYTE (key))
		  ? meta_modifier : 0x80);

  if (VECTORP (def) && ASIZE (def) > 0 && CONSP (AREF (def, 0)))
    { /* DEF is apparently an XEmacs-style keyboard macro.  */
      Lisp_Object tmp = make_nil_vector (ASIZE (def));
      ptrdiff_t i = ASIZE (def);
      while (--i >= 0)
	{
	  Lisp_Object defi = AREF (def, i);
	  if (CONSP (defi) && lucid_event_type_list_p (defi))
	    defi = Fevent_convert_list (defi);
	  ASET (tmp, i, defi);
	}
      def = tmp;
    }

  key = possibly_translate_key_sequence (key, &length);

  ptrdiff_t idx = 0;
  while (1)
    {
      Lisp_Object c = Faref (key, make_fixnum (idx));

      if (CONSP (c))
	{
	  /* C may be a Lucid style event type list or a cons (FROM .
	     TO) specifying a range of characters.  */
	  if (lucid_event_type_list_p (c))
	    c = Fevent_convert_list (c);
	  else if (CHARACTERP (XCAR (c)))
	    CHECK_CHARACTER_CDR (c);
	}

      if (SYMBOLP (c))
	silly_event_symbol_error (c);

      if (FIXNUMP (c)
	  && (XFIXNUM (c) & meta_bit)
	  && !metized)
	{
	  c = meta_prefix_char;
	  metized = true;
	}
      else
	{
	  if (FIXNUMP (c))
	    XSETINT (c, XFIXNUM (c) & ~meta_bit);

	  metized = false;
	  idx++;
	}

      if (!FIXNUMP (c) && !SYMBOLP (c)
	  && (!CONSP (c)
	      /* If C is a range, it must be a leaf.  */
	      || (FIXNUMP (XCAR (c)) && idx != length)))
	message_with_string ("Key sequence contains invalid event %s", c, 1);

      if (idx == length)
	return store_in_keymap (keymap, c, def, !NILP (remove));

      Lisp_Object cmd = access_keymap (keymap, c, 0, 1, 1);

      /* If this key is undefined, make it a prefix.  */
      if (NILP (cmd))
	cmd = define_as_prefix (keymap, c);

      keymap = get_keymap (cmd, 0, 1);
      if (!CONSP (keymap))
	{
	  const char *trailing_esc = ((EQ (c, meta_prefix_char) && metized)
				      ? (idx == 0 ? "ESC" : " ESC")
				      : "");

	  /* We must use Fkey_description rather than just passing key to
	     error; key might be a vector, not a string.  */
	  error ("Key sequence %s starts with non-prefix key %s%s",
		 SDATA (Fkey_description (key, Qnil)),
		 SDATA (Fkey_description (Fsubstring (key, make_fixnum (0),
						      make_fixnum (idx)),
					  Qnil)),
		 trailing_esc);
	}
    }
}

/* This function may GC (it calls Fkey_binding).  */

DEFUN ("command-remapping", Fcommand_remapping, Scommand_remapping, 1, 3, 0,
       doc: /* Return the remapping for command COMMAND.
Returns nil if COMMAND is not remapped (or not a symbol).

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the
remapping occurs in the keymaps associated with it.  It can also be a
number or marker, in which case the keymap properties at the specified
buffer position instead of point are used.  The KEYMAPS argument is
ignored if POSITION is non-nil.

If the optional argument KEYMAPS is non-nil, it should be a keymap or list of
keymaps to search for command remapping.  Otherwise, search for the
remapping in all currently active keymaps.  */)
  (Lisp_Object command, Lisp_Object position, Lisp_Object keymaps)
{
  if (!SYMBOLP (command))
    return Qnil;

  ASET (command_remapping_vector, 1, command);

  if (NILP (keymaps))
    command = Fkey_binding (command_remapping_vector, Qnil, Qt, position);
  else
    command = Flookup_key (keymaps, command_remapping_vector, Qnil);
  return FIXNUMP (command) ? Qnil : command;
}

static Lisp_Object
lookup_key_1 (Lisp_Object keymap, Lisp_Object key, Lisp_Object accept_default)
{
  bool t_ok = !NILP (accept_default);

  if (!CONSP (keymap) && !NILP (keymap))
    keymap = get_keymap (keymap, true, true);

  ptrdiff_t length = CHECK_VECTOR_OR_STRING (key);
  if (length == 0)
    return keymap;

  key = possibly_translate_key_sequence (key, &length);

  ptrdiff_t idx = 0;
  while (1)
    {
      Lisp_Object c = Faref (key, make_fixnum (idx++));

      if (CONSP (c) && lucid_event_type_list_p (c))
	c = Fevent_convert_list (c);

      /* Turn the 8th bit of string chars into a meta modifier.  */
      if (STRINGP (key) && XFIXNUM (c) & 0x80 && !STRING_MULTIBYTE (key))
	XSETINT (c, (XFIXNUM (c) | meta_modifier) & ~0x80);

      /* Allow string since binding for `menu-bar-select-buffer'
	 includes the buffer name in the key sequence.  */
      if (!FIXNUMP (c) && !SYMBOLP (c) && !CONSP (c) && !STRINGP (c))
	message_with_string ("Key sequence contains invalid event %s", c, 1);

      Lisp_Object cmd = access_keymap (keymap, c, t_ok, 0, 1);
      if (idx == length)
	return cmd;

      keymap = get_keymap (cmd, 0, 1);
      if (!CONSP (keymap))
	return make_fixnum (idx);

      maybe_quit ();
    }
}

/* Value is number if KEY is too long; nil if valid but has no definition.  */
/* GC is possible in this function.  */

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 3, 0,
       doc: /* Look up key sequence KEY in KEYMAP.  Return the definition.
This is a legacy function; see `keymap-lookup' for the recommended
function to use instead.

A value of nil means undefined.  See doc of `define-key'
for kinds of definitions.

A number as value means KEY is "too long";
that is, characters or symbols in it except for the last one
fail to be a valid sequence of prefix characters in KEYMAP.
The number is how many characters at the front of KEY
it takes to reach a non-prefix key.
KEYMAP can also be a list of keymaps.

Normally, `lookup-key' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
recognize the default bindings, just as `read-key-sequence' does.  */)
  (Lisp_Object keymap, Lisp_Object key, Lisp_Object accept_default)
{
  Lisp_Object found = lookup_key_1 (keymap, key, accept_default);
  if (!NILP (found) && !NUMBERP (found))
    return found;

  /* Menu definitions might use mixed case symbols (notably in old
     versions of `easy-menu-define'), or use " " instead of "-".
     The rest of this function is about accepting these variations for
     backwards-compatibility.  (Bug#50752) */

  /* Just skip everything below unless this is a menu item.  */
  if (!VECTORP (key) || !(ASIZE (key) > 0)
      || !EQ (AREF (key, 0), Qmenu_bar))
    return found;

  /* Initialize the unicode case table, if it wasn't already.  */
  if (NILP (unicode_case_table))
    {
      unicode_case_table = uniprop_table (intern ("lowercase"));
      /* uni-lowercase.el might be unavailable during bootstrap.  */
      if (NILP (unicode_case_table))
	return found;
      staticpro (&unicode_case_table);
    }

  ptrdiff_t key_len = ASIZE (key);
  Lisp_Object new_key = make_vector (key_len, Qnil);

  /* Try both the Unicode case table, and the buffer local one.
     Otherwise, we will fail for e.g. the "Turkish" language
     environment where 'I' does not downcase to 'i'.  */
  Lisp_Object tables[2] = {unicode_case_table, Fcurrent_case_table ()};
  for (int tbl_num = 0; tbl_num < 2; tbl_num++)
    {
      /* First, let's try converting all symbols like "Foo-Bar-Baz" to
	 "foo-bar-baz".  */
      for (int i = 0; i < key_len; i++)
	{
	  Lisp_Object item = AREF (key, i);
	  if (!SYMBOLP (item))
	    ASET (new_key, i, item);
	  else
	    {
	      Lisp_Object key_item = Fsymbol_name (item);
	      Lisp_Object new_item;
	      if (!STRING_MULTIBYTE (key_item))
		new_item = Fdowncase (key_item);
	      else
		{
		  USE_SAFE_ALLOCA;
		  ptrdiff_t size = SCHARS (key_item), n;
		  if (INT_MULTIPLY_WRAPV (size, MAX_MULTIBYTE_LENGTH, &n))
		    n = PTRDIFF_MAX;
		  unsigned char *dst = SAFE_ALLOCA (n);
		  unsigned char *p = dst;
		  ptrdiff_t j_char = 0, j_byte = 0;

		  while (j_char < size)
		    {
		      int ch = fetch_string_char_advance (key_item,
							  &j_char, &j_byte);
		      Lisp_Object ch_conv = CHAR_TABLE_REF (tables[tbl_num],
							    ch);
		      if (!NILP (ch_conv))
			CHAR_STRING (XFIXNUM (ch_conv), p);
		      else
			CHAR_STRING (ch, p);
		      p = dst + j_byte;
		    }
		  new_item = make_multibyte_string ((char *) dst,
						    SCHARS (key_item),
						    SBYTES (key_item));
		  SAFE_FREE ();
		}
	      ASET (new_key, i, Fintern (new_item, Qnil));
	    }
	}

      /* Check for match.  */
      found = lookup_key_1 (keymap, new_key, accept_default);
      if (!NILP (found) && !NUMBERP (found))
	break;

      /* If we still don't have a match, let's convert any spaces in
	 our lowercased string into dashes, e.g. "foo bar baz" to
	 "foo-bar-baz".  */
      for (int i = 0; i < key_len; i++)
	{
	  if (!SYMBOLP (AREF (new_key, i)))
	    continue;

	  Lisp_Object lc_key = Fsymbol_name (AREF (new_key, i));

	  /* If there are no spaces in this symbol, just skip it.  */
	  if (!strstr (SSDATA (lc_key), " "))
	    continue;

	  USE_SAFE_ALLOCA;
	  ptrdiff_t size = SCHARS (lc_key), n;
	  if (INT_MULTIPLY_WRAPV (size, MAX_MULTIBYTE_LENGTH, &n))
	    n = PTRDIFF_MAX;
	  unsigned char *dst = SAFE_ALLOCA (n);

	  /* We can walk the string data byte by byte, because UTF-8
	     encoding ensures that no other byte of any multibyte
	     sequence will ever include a 7-bit byte equal to an ASCII
	     single-byte character.  */
	  memcpy (dst, SSDATA (lc_key), SBYTES (lc_key));
	  for (int i = 0; i < SBYTES (lc_key); ++i)
	    {
	      if (dst[i] == ' ')
		dst[i] = '-';
	    }
	  Lisp_Object new_it =
	    make_multibyte_string ((char *) dst,
				   SCHARS (lc_key), SBYTES (lc_key));
	  ASET (new_key, i, Fintern (new_it, Qnil));
	  SAFE_FREE ();
	}

      /* Check for match.  */
      found = lookup_key_1 (keymap, new_key, accept_default);
      if (!NILP (found) && !NUMBERP (found))
	break;
    }

  return found;
}

/* Make KEYMAP define event C as a keymap (i.e., as a prefix).
   Assume that currently it does not define C at all.
   Return the keymap.  */

static Lisp_Object
define_as_prefix (Lisp_Object keymap, Lisp_Object c)
{
  Lisp_Object cmd = Fmake_sparse_keymap (Qnil);
  store_in_keymap (keymap, c, cmd, false);

  return cmd;
}

/* Append a key to the end of a key sequence.  We always make a vector.  */

static Lisp_Object
append_key (Lisp_Object key_sequence, Lisp_Object key)
{
  AUTO_LIST1 (key_list, key);
  return CALLN (Fvconcat, key_sequence, key_list);
}

/* Given an event type C which is a symbol,
   signal an error if is a mistake such as RET or M-RET or C-DEL, etc.  */

static void
silly_event_symbol_error (Lisp_Object c)
{
  Lisp_Object parsed = parse_modifiers (c);
  int modifiers = XFIXNAT (XCAR (XCDR (parsed)));
  Lisp_Object base = XCAR (parsed);
  Lisp_Object name = Fsymbol_name (base);
  /* This alist includes elements such as ("RET" . "\\r").  */
  Lisp_Object assoc = Fassoc (name, exclude_keys, Qnil);

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
      AUTO_STRING_WITH_LEN (new_mods_string, new_mods, p - new_mods);
      keystring = concat2 (new_mods_string, XCDR (assoc));

      error ("To bind the key %s, use [?%s], not [%s]",
	     SDATA (SYMBOL_NAME (c)), SDATA (keystring),
	     SDATA (SYMBOL_NAME (c)));
    }
}

/* Global, local, and minor mode keymap stuff.				*/

/* We can't put these variables inside current_minor_maps, since under
   some systems, static gets macro-defined to be the empty string.
   Ickypoo.  */
static Lisp_Object *cmm_modes = NULL, *cmm_maps = NULL;
static ptrdiff_t cmm_size = 0;

/* Store a pointer to an array of the currently active minor modes in
   *modeptr, a pointer to an array of the keymaps of the currently
   active minor modes in *mapptr, and return the number of maps
   *mapptr contains.

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
ptrdiff_t
current_minor_maps (Lisp_Object **modeptr, Lisp_Object **mapptr)
{
  ptrdiff_t i = 0;
  Lisp_Object alist, assoc, var, val;
  Lisp_Object emulation_alists = Vemulation_mode_map_alists;
  Lisp_Object lists[2];

  lists[0] = Vminor_mode_overriding_map_alist;
  lists[1] = Vminor_mode_map_alist;

  for (int list_number = 0; list_number < 2; list_number++)
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
	    && (val = find_symbol_value (var), !BASE_EQ (val, Qunbound))
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
		ptrdiff_t newsize, allocsize;
		Lisp_Object *newmodes, *newmaps;

		/* Check for size calculation overflow.  Other code
		   (e.g., read_key_sequence) adds 3 to the count
		   later, so subtract 3 from the limit here.  */
		if (min (PTRDIFF_MAX, SIZE_MAX) / (2 * sizeof *newmodes) - 3
		    < cmm_size)
		  break;

		newsize = cmm_size == 0 ? 30 : cmm_size * 2;
		allocsize = newsize * sizeof *newmodes;

		/* Use malloc here.  See the comment above this function.
		   Avoid realloc here; it causes spurious traps on GNU/Linux [KFS] */
		block_input ();
		newmodes = malloc (allocsize);
		if (newmodes)
		  {
		    if (cmm_modes)
		      {
			memcpy (newmodes, cmm_modes,
				cmm_size * sizeof cmm_modes[0]);
			free (cmm_modes);
		      }
		    cmm_modes = newmodes;
		  }

		newmaps = malloc (allocsize);
		if (newmaps)
		  {
		    if (cmm_maps)
		      {
			memcpy (newmaps, cmm_maps,
				cmm_size * sizeof cmm_maps[0]);
			free (cmm_maps);
		      }
		    cmm_maps = newmaps;
		  }
		unblock_input ();

		if (newmodes == NULL || newmaps == NULL)
		  break;
		cmm_size = newsize;
	      }

	    /* Get the keymap definition--or nil if it is not defined.  */
	    temp = Findirect_function (XCDR (assoc), Qt);
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

/* Return the offset of POSITION, a click position, in the style of
   the respective argument of Fkey_binding.  */
static ptrdiff_t
click_position (Lisp_Object position)
{
  EMACS_INT pos = (FIXNUMP (position) ? XFIXNUM (position)
		   : MARKERP (position) ? marker_position (position)
		   : PT);
  if (! (BEGV <= pos && pos <= ZV))
    args_out_of_range (Fcurrent_buffer (), position);
  return pos;
}

DEFUN ("current-active-maps", Fcurrent_active_maps, Scurrent_active_maps,
       0, 2, 0,
       doc: /* Return a list of the currently active keymaps.
OLP if non-nil indicates that we should obey `overriding-local-map' and
`overriding-terminal-local-map'.  POSITION can specify a click position
like in the respective argument of `key-binding'.  */)
  (Lisp_Object olp, Lisp_Object position)
{
  specpdl_ref count = SPECPDL_INDEX ();

  Lisp_Object keymaps = list1 (current_global_map);

  /* If a mouse click position is given, our variables are based on
     the buffer clicked on, not the current buffer.  So we may have to
     switch the buffer here.  */

  if (CONSP (position))
    {
      Lisp_Object window = POSN_WINDOW (position);

      if (WINDOWP (window)
	  && BUFFERP (XWINDOW (window)->contents)
	  && XBUFFER (XWINDOW (window)->contents) != current_buffer)
	{
	  /* Arrange to go back to the original buffer once we're done
	     processing the key sequence.  We don't use
	     save_excursion_{save,restore} here, in analogy to
	     `read-key-sequence' to avoid saving point.  Maybe this
	     would not be a problem here, but it is easier to keep
	     things the same.
	  */
	  record_unwind_current_buffer ();
	  set_buffer_internal (XBUFFER (XWINDOW (window)->contents));
	}
    }

  if (!NILP (olp)
      /* The doc said that overriding-terminal-local-map should
	 override overriding-local-map.  The code used them both,
	 but it seems clearer to use just one.  rms, jan 2005.  */
      && NILP (KVAR (current_kboard, Voverriding_terminal_local_map))
      && !NILP (Voverriding_local_map))
    keymaps = Fcons (Voverriding_local_map, keymaps);

  if (NILP (XCDR (keymaps)))
    {
      Lisp_Object *maps;
      int nmaps;
      ptrdiff_t pt = click_position (position);
      /* This usually returns the buffer's local map,
	 but that can be overridden by a `local-map' property.  */
      Lisp_Object local_map = get_local_map (pt, current_buffer, Qlocal_map);
      /* This returns nil unless there is a `keymap' property.  */
      Lisp_Object keymap = get_local_map (pt, current_buffer, Qkeymap);
      Lisp_Object otlp = KVAR (current_kboard, Voverriding_terminal_local_map);

      if (CONSP (position))
	{
	  Lisp_Object string = POSN_STRING (position);

	  /* For a mouse click, get the local text-property keymap
	     of the place clicked on, rather than point.  */

	  if (POSN_INBUFFER_P (position))
	    {
	      Lisp_Object pos = POSN_BUFFER_POSN (position);
	      if (FIXNUMP (pos)
		  && XFIXNUM (pos) >= BEG && XFIXNUM (pos) <= Z)
		{
		  local_map = get_local_map (XFIXNUM (pos),
					     current_buffer, Qlocal_map);

		  keymap = get_local_map (XFIXNUM (pos),
					  current_buffer, Qkeymap);
		}
	    }

	  /* If on a mode line string with a local keymap,
	     or for a click on a string, i.e. overlay string or a
	     string displayed via the `display' property,
	     consider `local-map' and `keymap' properties of
	     that string.  */

	  if (CONSP (string) && STRINGP (XCAR (string)))
	    {
	      Lisp_Object pos = XCDR (string);
	      string = XCAR (string);
	      if (FIXNUMP (pos)
		  && XFIXNUM (pos) >= 0
		  && XFIXNUM (pos) < SCHARS (string))
		{
		  Lisp_Object map = Fget_text_property (pos, Qlocal_map, string);
		  if (!NILP (map))
		    local_map = map;

		  map = Fget_text_property (pos, Qkeymap, string);
		  if (!NILP (map))
		    keymap = map;
		}
	    }

	}

      if (!NILP (local_map))
	keymaps = Fcons (local_map, keymaps);

      /* Now put all the minor mode keymaps on the list.  */
      nmaps = current_minor_maps (0, &maps);

      for (int i = --nmaps; i >= 0; i--)
	if (!NILP (maps[i]))
	  keymaps = Fcons (maps[i], keymaps);

      if (!NILP (keymap))
	keymaps = Fcons (keymap, keymaps);

      if (!NILP (olp) && !NILP (otlp))
	keymaps = Fcons (otlp, keymaps);
    }

  return unbind_to (count, keymaps);
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 4, 0,
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
current keymaps.  However, if the optional third argument NO-REMAP
is non-nil, `key-binding' returns the unmapped command.

If KEY is a key sequence initiated with the mouse, the used keymaps
will depend on the clicked mouse position with regard to the buffer
and possible local keymaps on strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position instead of point are used.
  */)
  (Lisp_Object key, Lisp_Object accept_default, Lisp_Object no_remap, Lisp_Object position)
{
  if (NILP (position) && VECTORP (key))
    {
      if (ASIZE (key) == 0)
	return Qnil;

      /* mouse events may have a symbolic prefix indicating the
	 scrollbar or mode line */
      Lisp_Object event
	= AREF (key, SYMBOLP (AREF (key, 0)) && ASIZE (key) > 1 ? 1 : 0);

      /* We are not interested in locations without event data */

      if (EVENT_HAS_PARAMETERS (event) && CONSP (XCDR (event)))
	{
	  Lisp_Object kind = EVENT_HEAD_KIND (EVENT_HEAD (event));
	  if (EQ (kind, Qmouse_click))
	    position = EVENT_START (event);
	}
    }

  Lisp_Object value = Flookup_key (Fcurrent_active_maps (Qt, position),
				   key, accept_default);

  if (NILP (value) || FIXNUMP (value))
    return Qnil;

  /* If the result of the ordinary keymap lookup is an interactive
     command, look for a key binding (ie. remapping) for that command.  */

  if (NILP (no_remap) && SYMBOLP (value))
    {
      Lisp_Object value1;
      if (value1 = Fcommand_remapping (value, position, Qnil), !NILP (value1))
	value = value1;
    }

  return value;
}

/* GC is possible in this function if it autoloads a keymap.  */

DEFUN ("minor-mode-key-binding", Fminor_mode_key_binding, Sminor_mode_key_binding, 1, 2, 0,
       doc: /* Find the visible minor mode bindings of KEY.
Return an alist of pairs (MODENAME . BINDING), where MODENAME is
the symbol which names the minor mode binding KEY, and BINDING is
KEY's definition in that mode.  In particular, if KEY has no
minor-mode bindings, return nil.  If the first binding is a
non-prefix, all subsequent bindings will be omitted, since they would
be ignored.  Similarly, the list doesn't include non-prefix bindings
that come after prefix bindings.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this.  */)
  (Lisp_Object key, Lisp_Object accept_default)
{
  Lisp_Object *modes, *maps;
  int nmaps = current_minor_maps (&modes, &maps);
  Lisp_Object binding = Qnil;

  int j;
  for (int i = j = 0; i < nmaps; i++)
    if (!NILP (maps[i])
	&& !NILP (binding = Flookup_key (maps[i], key, accept_default))
	&& !FIXNUMP (binding))
      {
	if (KEYMAPP (binding))
	  maps[j++] = Fcons (modes[i], binding);
	else if (j == 0)
	  return list1 (Fcons (modes[i], binding));
      }

  return Flist (j, maps);
}

DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
       doc: /* Select KEYMAP as the global keymap.  */)
  (Lisp_Object keymap)
{
  keymap = get_keymap (keymap, 1, 1);
  current_global_map = keymap;

  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
       doc: /* Select KEYMAP as the local keymap.
If KEYMAP is nil, that means no local keymap.  */)
  (Lisp_Object keymap)
{
  if (!NILP (keymap))
    keymap = get_keymap (keymap, 1, 1);

  bset_keymap (current_buffer, keymap);

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
       doc: /* Return current buffer's local keymap, or nil if it has none.
Normally the local keymap is set by the major mode with `use-local-map'.  */)
  (void)
{
  return BVAR (current_buffer, keymap);
}

DEFUN ("current-global-map", Fcurrent_global_map, Scurrent_global_map, 0, 0, 0,
       doc: /* Return the current global keymap.  */)
  (void)
{
  return current_global_map;
}

DEFUN ("current-minor-mode-maps", Fcurrent_minor_mode_maps, Scurrent_minor_mode_maps, 0, 0, 0,
       doc: /* Return a list of keymaps for the minor modes of the current buffer.  */)
  (void)
{
  Lisp_Object *maps;
  int nmaps = current_minor_maps (0, &maps);

  return Flist (nmaps, maps);
}

/* Help functions for describing and documenting keymaps.		*/

struct accessible_keymaps_data {
  Lisp_Object maps, tail, thisseq;
  /* Does the current sequence end in the meta-prefix-char?  */
  bool is_metized;
};

static void
accessible_keymaps_1 (Lisp_Object key, Lisp_Object cmd, Lisp_Object args, void *data)
/* Use void * data to be compatible with map_keymap_function_t.  */
{
  struct accessible_keymaps_data *d = data; /* Cast! */
  Lisp_Object maps = d->maps;
  Lisp_Object tail = d->tail;
  Lisp_Object thisseq = d->thisseq;
  bool is_metized = d->is_metized && FIXNUMP (key);
  Lisp_Object tem;

  cmd = get_keymap (get_keyelt (cmd, 0), 0, 0);
  if (NILP (cmd))
    return;

  /* Look for and break cycles.  */
  while (!NILP (tem = Frassq (cmd, maps)))
    {
      Lisp_Object prefix = XCAR (tem);
      ptrdiff_t lim = XFIXNUM (Flength (XCAR (tem)));
      if (lim <= XFIXNUM (Flength (thisseq)))
	{ /* This keymap was already seen with a smaller prefix.  */
	  ptrdiff_t i = 0;
	  while (i < lim && EQ (Faref (prefix, make_fixnum (i)),
				Faref (thisseq, make_fixnum (i))))
	    i++;
	  if (i >= lim)
	    /* `prefix' is a prefix of `thisseq' => there's a cycle.  */
	    return;
	}
      /* This occurrence of `cmd' in `maps' does not correspond to a cycle,
	 but maybe `cmd' occurs again further down in `maps', so keep
	 looking.  */
      maps = XCDR (Fmemq (tem, maps));
    }

  /* If the last key in thisseq is meta-prefix-char,
     turn it into a meta-ized keystroke.  We know
     that the event we're about to append is an
     ascii keystroke since we're processing a
     keymap table.  */
  if (is_metized)
    {
      int meta_bit = meta_modifier;
      Lisp_Object last = make_fixnum (XFIXNUM (Flength (thisseq)) - 1);
      tem = Fcopy_sequence (thisseq);

      Faset (tem, last, make_fixnum (XFIXNUM (key) | meta_bit));

      /* This new sequence is the same length as
	 thisseq, so stick it in the list right
	 after this one.  */
      XSETCDR (tail,
	       Fcons (Fcons (tem, cmd), XCDR (tail)));
    }
  else
    {
      tem = append_key (thisseq, key);
      nconc2 (tail, list1 (Fcons (tem, cmd)));
    }
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
  (Lisp_Object keymap, Lisp_Object prefix)
{
  Lisp_Object maps, tail;
  EMACS_INT prefixlen = XFIXNAT (Flength (prefix));

  if (!NILP (prefix))
    {
      /* If a prefix was specified, start with the keymap (if any) for
	 that prefix, so we don't waste time considering other prefixes.  */
      Lisp_Object tem = Flookup_key (keymap, prefix, Qt);
      /* Flookup_key may give us nil, or a number,
	 if the prefix is not defined in this particular map.
	 It might even give us a list that isn't a keymap.  */
      tem = get_keymap (tem, 0, 0);
      /* If the keymap is autoloaded `tem' is not a cons-cell, but we still
	 want to return it.  */
      if (!NILP (tem))
	{
	  /* Convert PREFIX to a vector now, so that later on
	     we don't have to deal with the possibility of a string.  */
	  if (STRINGP (prefix))
	    {
	      ptrdiff_t i_byte = 0;
	      Lisp_Object copy = make_nil_vector (SCHARS (prefix));
	      for (ptrdiff_t i = 0; i < SCHARS (prefix); )
		{
		  ptrdiff_t i_before = i;
		  int c = fetch_string_char_advance (prefix, &i, &i_byte);
		  if (SINGLE_BYTE_CHAR_P (c) && (c & 0200))
		    c ^= 0200 | meta_modifier;
		  ASET (copy, i_before, make_fixnum (c));
		}
	      prefix = copy;
	    }
	  maps = list1 (Fcons (prefix, tem));
	}
      else
	return Qnil;
    }
  else
    maps = list1 (Fcons (zero_vector, get_keymap (keymap, 1, 0)));

  /* For each map in the list maps,
     look at any other maps it points to,
     and stick them at the end if they are not already in the list.

     This is a breadth-first traversal, where tail is the queue of
     nodes, and maps accumulates a list of all nodes visited.  */

  for (tail = maps; CONSP (tail); tail = XCDR (tail))
    {
      struct accessible_keymaps_data data;
      register Lisp_Object thismap = Fcdr (XCAR (tail));
      Lisp_Object last;

      data.thisseq = Fcar (XCAR (tail));
      data.maps = maps;
      data.tail = tail;
      last = make_fixnum (XFIXNUM (Flength (data.thisseq)) - 1);
      /* Does the current sequence end in the meta-prefix-char?  */
      data.is_metized = (XFIXNUM (last) >= 0
		    /* Don't metize the last char of PREFIX.  */
		    && XFIXNUM (last) >= prefixlen
		    && EQ (Faref (data.thisseq, last), meta_prefix_char));

      /* Since we can't run lisp code, we can't scan autoloaded maps.  */
      if (CONSP (thismap))
	map_keymap (thismap, accessible_keymaps_1, Qnil, &data, 0);
    }
  return maps;
}

/* This function cannot GC.  */

DEFUN ("key-description", Fkey_description, Skey_description, 1, 2, 0,
       doc: /* Return a pretty description of key-sequence KEYS.
Optional arg PREFIX is the sequence of keys leading up to KEYS.
For example, [?\\C-x ?l] is converted into the string \"C-x l\".

For an approximate inverse of this, see `kbd'.  */)
  (Lisp_Object keys, Lisp_Object prefix)
{
  ptrdiff_t len = 0;
  Lisp_Object *args;
  EMACS_INT nkeys = XFIXNUM (Flength (keys));
  EMACS_INT nprefix = XFIXNUM (Flength (prefix));
  Lisp_Object sep = build_string (" ");
  bool add_meta = false;
  USE_SAFE_ALLOCA;

  /* This has one extra element at the end that we don't pass to Fconcat.  */
  ptrdiff_t size4;
  if (INT_MULTIPLY_WRAPV (nkeys + nprefix, 4, &size4))
    memory_full (SIZE_MAX);
  SAFE_ALLOCA_LISP (args, size4);

  /* In effect, this computes
     (mapconcat 'single-key-description keys " ")
     but we shouldn't use mapconcat because it can do GC.  */

  Lisp_Object lists[2] = { prefix, keys };
  ptrdiff_t listlens[2] = { nprefix, nkeys };
  for (int li = 0; li < ARRAYELTS (lists); li++)
    {
      Lisp_Object list = lists[li];
      ptrdiff_t listlen = listlens[li], i_byte = 0;

      if (! (NILP (list) || STRINGP (list) || VECTORP (list) || CONSP (list)))
	wrong_type_argument (Qarrayp, list);

      for (ptrdiff_t i = 0; i < listlen; )
	{
	  Lisp_Object key;
	  if (STRINGP (list))
	    {
	      int c = fetch_string_char_advance (list, &i, &i_byte);
	      if (SINGLE_BYTE_CHAR_P (c) && (c & 0200))
		c ^= 0200 | meta_modifier;
	      key = make_fixnum (c);
	    }
	  else if (VECTORP (list))
	    {
	      key = AREF (list, i);
	      i++;
	    }
	  else
	    {
	      key = XCAR (list);
	      list = XCDR (list);
	      i++;
	    }

	  if (add_meta)
	    {
	      if (!FIXNUMP (key)
		  || EQ (key, meta_prefix_char)
		  || (XFIXNUM (key) & meta_modifier))
		{
		  args[len++] = Fsingle_key_description (meta_prefix_char,
							 Qnil);
		  args[len++] = sep;
		  if (EQ (key, meta_prefix_char))
		    continue;
		}
	      else
		key = make_fixnum (XFIXNUM (key) | meta_modifier);
	      add_meta = false;
	    }
	  else if (EQ (key, meta_prefix_char))
	    {
	      add_meta = true;
	      continue;
	    }
	  args[len++] = Fsingle_key_description (key, Qnil);
	  args[len++] = sep;
	}
    }

  Lisp_Object result;
  if (add_meta)
    {
      args[len] = Fsingle_key_description (meta_prefix_char, Qnil);
      result = Fconcat (len + 1, args);
    }
  else if (len == 0)
    result = empty_unibyte_string;
  else
    result = Fconcat (len - 1, args);
  SAFE_FREE ();
  return result;
}


char *
push_key_description (EMACS_INT ch, char *p)
{
  int c, c2;
  bool tab_as_ci;

  /* Clear all the meaningless bits above the meta bit.  */
  c = ch & (meta_modifier | ~ - meta_modifier);
  c2 = c & ~(alt_modifier | ctrl_modifier | hyper_modifier
	     | meta_modifier | shift_modifier | super_modifier);

  if (! CHARACTERP (make_fixnum (c2)))
    {
      /* KEY_DESCRIPTION_SIZE is large enough for this.  */
      p += sprintf (p, "[%d]", c);
      return p;
    }

  tab_as_ci = (c2 == '\t' && (c & meta_modifier));

  if (c & alt_modifier)
    {
      *p++ = 'A';
      *p++ = '-';
      c -= alt_modifier;
    }
  if ((c & ctrl_modifier) != 0
      || (c2 < ' ' && c2 != 27 && c2 != '\t' && c2 != Ctl ('M'))
      || tab_as_ci)
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
      else if (tab_as_ci)
	{
	  *p++ = 'i';
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
  else if (c < 128)
    *p++ = c;
  else
    {
      /* Now we are sure that C is a valid character code.  */
      p += CHAR_STRING (c, (unsigned char *) p);
    }

  return p;
}

/* This function cannot GC.  */

DEFUN ("single-key-description", Fsingle_key_description,
       Ssingle_key_description, 1, 2, 0,
       doc: /* Return a pretty description of a character event KEY.
Control characters turn into C-whatever, etc.
Optional argument NO-ANGLES non-nil means don't put angle brackets
around function keys and event symbols.

See `text-char-description' for describing character codes.  */)
  (Lisp_Object key, Lisp_Object no_angles)
{
  USE_SAFE_ALLOCA;

  if (CONSP (key) && lucid_event_type_list_p (key))
    key = Fevent_convert_list (key);

  if (CONSP (key) && FIXNUMP (XCAR (key)) && FIXNUMP (XCDR (key)))
    /* An interval from a map-char-table.  */
    {
      AUTO_STRING (dot_dot, "..");
      return concat3 (Fsingle_key_description (XCAR (key), no_angles),
		      dot_dot,
		      Fsingle_key_description (XCDR (key), no_angles));
    }

  key = EVENT_HEAD (key);

  if (FIXNUMP (key))		/* Normal character.  */
    {
      char tem[KEY_DESCRIPTION_SIZE];
      char *p = push_key_description (XFIXNUM (key), tem);
      *p = 0;
      return make_specified_string (tem, -1, p - tem, 1);
    }
  else if (SYMBOLP (key))	/* Function key or event-symbol.  */
    {
      if (NILP (no_angles))
	{
	  Lisp_Object namestr = SYMBOL_NAME (key);
	  const char *sym = SSDATA (namestr);
	  ptrdiff_t len = SBYTES (namestr);
	  /* Find the extent of the modifier prefix, like "C-M-". */
	  int i = 0;
	  while (i < len - 3 && sym[i + 1] == '-' && strchr ("CMSsHA", sym[i]))
	    i += 2;
	  /* First I bytes of SYM are modifiers; put <> around the rest. */
	  char *buffer = SAFE_ALLOCA (len + 3);
	  memcpy (buffer, sym, i);
	  buffer[i] = '<';
	  memcpy (buffer + i + 1, sym + i, len - i);
	  buffer [len + 1] = '>';
	  buffer [len + 2] = '\0';
	  Lisp_Object result = build_string (buffer);
	  SAFE_FREE ();
	  return result;
	}
      else
	return Fsymbol_name (key);
    }
  else if (STRINGP (key))	/* Buffer names in the menubar.  */
    return Fcopy_sequence (key);
  else
    error ("KEY must be an integer, cons, symbol, or string");
}

static char *
push_text_char_description (register unsigned int c, register char *p)
{
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
       doc: /* Return the description of CHARACTER in standard Emacs notation.
CHARACTER must be a valid character code that passes the `characterp' test.
Control characters turn into "^char", and characters with Meta and other
modifiers signal an error, as they are not valid character codes.
This differs from `single-key-description' which accepts character events,
and thus doesn't enforce the `characterp' condition, turns control
characters into "C-char", and uses the 2**27 bit for Meta.
See Info node `(elisp)Describing Characters' for examples.  */)
  (Lisp_Object character)
{
  CHECK_CHARACTER (character);

  int c = XFIXNUM (character);
  if (!ASCII_CHAR_P (c))
    {
      char str[MAX_MULTIBYTE_LENGTH];
      int len = CHAR_STRING (c, (unsigned char *) str);

      return make_multibyte_string (str, 1, len);
    }
  else
    {
      char desc[4];
      int len = push_text_char_description (c, desc) - desc;
      return make_string (desc, len);
    }
}

static int where_is_preferred_modifier;

/* Return 0 if SEQ uses non-preferred modifiers or non-char events.
   Else, return 2 if SEQ uses the where_is_preferred_modifier,
   and 1 otherwise.  */
static int
preferred_sequence_p (Lisp_Object seq)
{
  EMACS_INT i;
  EMACS_INT len = XFIXNAT (Flength (seq));
  int result = 1;

  for (i = 0; i < len; i++)
    {
      Lisp_Object ii, elt;

      XSETFASTINT (ii, i);
      elt = Faref (seq, ii);

      if (!FIXNUMP (elt))
	return 0;
      else
	{
	  int modifiers = XFIXNUM (elt) & (CHAR_MODIFIER_MASK & ~CHAR_META);
	  if (modifiers == where_is_preferred_modifier)
	    result = 2;
	  else if (modifiers)
	    return 0;
	}
    }

  return result;
}


/* where-is - finding a command in a set of keymaps.			*/

static void where_is_internal_1 (Lisp_Object key, Lisp_Object binding,
                                 Lisp_Object args, void *data);

/* Like Flookup_key, but with command remapping; just returns nil
   if the key sequence is too long.  */

static Lisp_Object
shadow_lookup (Lisp_Object keymap, Lisp_Object key, Lisp_Object accept_default,
	       bool remap)
{
  Lisp_Object value = Flookup_key (keymap, key, accept_default);

  if (FIXNATP (value))          /* `key' is too long!  */
    return Qnil;
  else if (!NILP (value) && remap && SYMBOLP (value))
    {
      Lisp_Object remapping = Fcommand_remapping (value, Qnil, keymap);
      return (!NILP (remapping) ? remapping : value);
    }
  else
    return value;
}

static Lisp_Object Vmouse_events;

struct where_is_internal_data {
  Lisp_Object definition, this, last;
  bool last_is_meta, noindirect;
  Lisp_Object sequences;
};

/* This function can't GC, AFAIK.  */
/* Return the list of bindings found.  This list is ordered "longest
   to shortest".  It may include bindings that are actually shadowed
   by others, as well as duplicate bindings and remapping bindings.
   The list returned is potentially shared with where_is_cache, so
   be careful not to modify it via side-effects.  */

static Lisp_Object
where_is_internal (Lisp_Object definition, Lisp_Object keymaps,
		   bool noindirect, bool nomenus)
{
  Lisp_Object maps = Qnil;
  struct where_is_internal_data data;

  /* Only important use of caching is for the menubar
     (i.e. where-is-internal called with (def nil t nil nil)).  */
  if (nomenus && !noindirect)
    {
      /* Check heuristic-consistency of the cache.  */
      if (NILP (Fequal (keymaps, where_is_cache_keymaps)))
	where_is_cache = Qnil;

      if (NILP (where_is_cache))
	{
	  /* We need to create the cache.  */
	  where_is_cache = Fmake_hash_table (0, NULL);
	  where_is_cache_keymaps = Qt;
	}
      else
	/* We can reuse the cache.  */
	return Fgethash (definition, where_is_cache, Qnil);
    }
  else
    /* Kill the cache so that where_is_internal_1 doesn't think
       we're filling it up.  */
    where_is_cache = Qnil;

  Lisp_Object found = keymaps;
  while (CONSP (found))
    {
      maps =
	nconc2 (maps,
		Faccessible_keymaps (get_keymap (XCAR (found), 1, 0), Qnil));
      found = XCDR (found);
    }

  data.sequences = Qnil;
  for (; CONSP (maps); maps = XCDR (maps))
    {
      /* Key sequence to reach map, and the map that it reaches */
      register Lisp_Object this, map, tem;

      /* In order to fold [META-PREFIX-CHAR CHAR] sequences into
	 [M-CHAR] sequences, check if last character of the sequence
	 is the meta-prefix char.  */
      Lisp_Object last;
      bool last_is_meta;

      this = Fcar (XCAR (maps));
      map  = Fcdr (XCAR (maps));
      last = make_fixnum (XFIXNUM (Flength (this)) - 1);
      last_is_meta = (XFIXNUM (last) >= 0
		      && EQ (Faref (this, last), meta_prefix_char));

      /* if (nomenus && !preferred_sequence_p (this)) */
      if (nomenus && XFIXNUM (last) >= 0
	  && SYMBOLP (tem = Faref (this, make_fixnum (0)))
	  && !NILP (Fmemq (XCAR (parse_modifiers (tem)), Vmouse_events)))
	/* If no menu entries should be returned, skip over the
	   keymaps bound to `menu-bar' and `tool-bar' and other
	   non-ascii prefixes like `C-down-mouse-2'.  */
	continue;

      maybe_quit ();

      data.definition = definition;
      data.noindirect = noindirect;
      data.this = this;
      data.last = last;
      data.last_is_meta = last_is_meta;

      if (CONSP (map))
	map_keymap (map, where_is_internal_1, Qnil, &data, 0);
    }

  if (nomenus && !noindirect)
    { /* Remember for which keymaps this cache was built.
	 We do it here (late) because we want to keep where_is_cache_keymaps
	 set to t while the cache isn't fully filled.  */
      where_is_cache_keymaps = keymaps;
      /* During cache-filling, data.sequences is not filled by
	 where_is_internal_1.  */
      return Fgethash (definition, where_is_cache, Qnil);
    }
  else
    return data.sequences;
}

/* This function can GC if Flookup_key autoloads any keymaps.  */

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 5, 0,
       doc: /* Return list of keys that invoke DEFINITION.
If KEYMAP is a keymap, search only KEYMAP and the global keymap.
If KEYMAP is nil, search all the currently active keymaps, except
 for `overriding-local-map' (which is ignored).
If KEYMAP is a list of keymaps, search only those keymaps.

If optional 3rd arg FIRSTONLY is non-nil, return the first key sequence found,
rather than a list of all possible key sequences.
If FIRSTONLY is the symbol `non-ascii', return the first binding found,
no matter what it is.
If FIRSTONLY has another non-nil value, prefer bindings
that use the modifier key specified in `where-is-preferred-modifier'
\(or their meta variants) and entirely reject menu bindings.

If optional 4th arg NOINDIRECT is non-nil, don't extract the commands inside
menu-items.  This makes it possible to search for a menu-item itself.

The optional 5th arg NO-REMAP alters how command remapping is handled:

- If another command OTHER-COMMAND is remapped to DEFINITION, normally
  search for the bindings of OTHER-COMMAND and include them in the
  returned list.  But if NO-REMAP is non-nil, include the vector
  [remap OTHER-COMMAND] in the returned list instead, without
  searching for those other bindings.

- If DEFINITION is remapped to OTHER-COMMAND, normally return the
  bindings for OTHER-COMMAND.  But if NO-REMAP is non-nil, return the
  bindings for DEFINITION instead, ignoring its remapping.

Keys that are represented as events that have a `non-key-event' non-nil
symbol property are ignored.  */)
  (Lisp_Object definition, Lisp_Object keymap, Lisp_Object firstonly, Lisp_Object noindirect, Lisp_Object no_remap)
{
  /* The keymaps in which to search.  */
  Lisp_Object keymaps;
  /* Potentially relevant bindings in "shortest to longest" order.  */
  Lisp_Object sequences = Qnil;
    /* Actually relevant bindings.  */
  Lisp_Object found = Qnil;
  /* 1 means ignore all menu bindings entirely.  */
  bool nomenus = !NILP (firstonly) && !EQ (firstonly, Qnon_ascii);
  /* List of sequences found via remapping.  Keep them in a separate
     variable, so as to push them later, since we prefer
     non-remapped binding.  */
  Lisp_Object remapped_sequences = Qnil;
  /* Whether or not we're handling remapped sequences.  This is needed
     because remapping is not done recursively by Fcommand_remapping: you
     can't remap a remapped command.  */
  bool remapped = false;

  /* Refresh the C version of the modifier preference.  */
  where_is_preferred_modifier
    = parse_solitary_modifier (Vwhere_is_preferred_modifier);

  /* Find the relevant keymaps.  */
  if (CONSP (keymap) && KEYMAPP (XCAR (keymap)))
    keymaps = keymap;
  else if (!NILP (keymap))
    keymaps = list2 (keymap, current_global_map);
  else
    keymaps = Fcurrent_active_maps (Qnil, Qnil);

  Lisp_Object tem = Fcommand_remapping (definition, Qnil, keymaps);
  /* If `definition' is remapped to `tem', then OT1H no key will run
     that command (since they will run `tem' instead), so we should
     return nil; but OTOH all keys bound to `definition' (or to `tem')
     will run the same command.
     So for menu-shortcut purposes, we want to find all the keys bound (maybe
     via remapping) to `tem'.  But for the purpose of finding the keys that
     run `definition', then we'd want to just return nil.
     We choose to make it work right for menu-shortcuts, since it's the most
     common use.
     Known bugs: if you remap switch-to-buffer to toto, C-h f switch-to-buffer
     will tell you that switch-to-buffer is bound to C-x b even though C-x b
     will run toto instead.  And if `toto' is itself remapped to forward-char,
     then C-h f toto will tell you that it's bound to C-f even though C-f does
     not run toto and it won't tell you that C-x b does run toto.  */
  if (NILP (no_remap) && !NILP (tem))
    definition = tem;

  if (SYMBOLP (definition)
      && !NILP (firstonly)
      && !NILP (tem = Fget (definition, QCadvertised_binding)))
    {
      /* We have a list of advertised bindings.  */
      /* FIXME: Not sure why we use false for shadow_lookup's remapping,
         nor why we use `EQ' here but `Fequal' in the call further down.  */
      while (CONSP (tem))
	if (EQ (shadow_lookup (keymaps, XCAR (tem), Qnil, 0), definition))
	  return XCAR (tem);
	else
	  tem = XCDR (tem);
      if (EQ (shadow_lookup (keymaps, tem, Qnil, 0), definition))
	return tem;
    }

  sequences = Freverse (where_is_internal (definition, keymaps,
					   !NILP (noindirect), nomenus));

  while (CONSP (sequences)
	 /* If we're at the end of the `sequences' list and we haven't
	    considered remapped sequences yet, copy them over and
	    process them.  */
	 || (!remapped && (sequences = remapped_sequences,
			   remapped = true,
			   CONSP (sequences))))
    {
      Lisp_Object sequence, function;

      sequence = XCAR (sequences);
      sequences = XCDR (sequences);

      /* Verify that this key binding is not shadowed by another
	 binding for the same key, before we say it exists.

	 Mechanism: look for local definition of this key and if
	 it is defined and does not match what we found then
	 ignore this key.

	 Either nil or number as value from Flookup_key
	 means undefined.  */
      if (NILP (Fequal (shadow_lookup (keymaps, sequence, Qnil, remapped),
			definition)))
	continue;

      /* If the current sequence is a command remapping with
	 format [remap COMMAND], find the key sequences
	 which run COMMAND, and use those sequences instead.  */
      if (NILP (no_remap) && !remapped
	  && VECTORP (sequence) && ASIZE (sequence) == 2
	  && EQ (AREF (sequence, 0), Qremap)
	  && (function = AREF (sequence, 1), SYMBOLP (function)))
	{
	  Lisp_Object seqs = where_is_internal (function, keymaps,
						!NILP (noindirect), nomenus);
	  remapped_sequences = nconc2 (Freverse (seqs), remapped_sequences);
	  continue;
	}

      /* Don't annoy user with strings from a menu such as the
	 entries from the "Edit => Paste from Kill Menu".
	 Change them all to "(any string)", so that there
	 seems to be only one menu item to report.  */
      if (! NILP (sequence))
	{
	  Lisp_Object tem1;
	  tem1 = Faref (sequence, make_fixnum (ASIZE (sequence) - 1));
	  if (STRINGP (tem1))
	    Faset (sequence, make_fixnum (ASIZE (sequence) - 1),
		   build_string ("(any string)"));
	}

      /* It is a true unshadowed match.  Record it, unless it's already
	 been seen (as could happen when inheriting keymaps).  */
      if (NILP (Fmember (sequence, found))
	  /* Filter out non key events.  */
	  && !(VECTORP (sequence)
	       && ASIZE (sequence) == 1
	       && SYMBOLP (AREF (sequence, 0))
	       && !NILP (Fget (AREF (sequence, 0), Qnon_key_event))))
	found = Fcons (sequence, found);

      /* If firstonly is Qnon_ascii, then we can return the first
	 binding we find.  If firstonly is not Qnon_ascii but not
	 nil, then we should return the first ascii-only binding
	 we find.  */
      if (EQ (firstonly, Qnon_ascii))
	return sequence;
      else if (!NILP (firstonly)
	       && 2 == preferred_sequence_p (sequence))
	return sequence;
    }

  found = Fnreverse (found);

  /* firstonly may have been t, but we may have gone all the way through
     the keymaps without finding an all-ASCII key sequence.  So just
     return the best we could find.  */
  if (NILP (firstonly))
    return found;
  else if (where_is_preferred_modifier == 0)
    return Fcar (found);
  else
    { /* Maybe we did not find a preferred_modifier binding, but we did find
	 some ASCII binding.  */
      Lisp_Object bindings = found;
      while (CONSP (bindings))
	if (preferred_sequence_p (XCAR (bindings)))
	  return XCAR (bindings);
	else
	  bindings = XCDR (bindings);
      return Fcar (found);
    }
}

/* This function can GC because get_keyelt can.  */

static void
where_is_internal_1 (Lisp_Object key, Lisp_Object binding, Lisp_Object args, void *data)
{
  struct where_is_internal_data *d = data; /* Cast! */
  Lisp_Object definition = d->definition;
  bool noindirect = d->noindirect;
  Lisp_Object this = d->this;
  Lisp_Object last = d->last;
  bool last_is_meta = d->last_is_meta;
  Lisp_Object sequence;

  /* Search through indirections unless that's not wanted.  */
  if (!noindirect)
    binding = get_keyelt (binding, 0);

  /* End this iteration if this element does not match
     the target.  */

  if (!(!NILP (where_is_cache)	/* everything "matches" during cache-fill.  */
	|| EQ (binding, definition)
	|| (CONSP (definition) && !NILP (Fequal (binding, definition)))))
    /* Doesn't match.  */
    return;

  /* We have found a match.  Construct the key sequence where we found it.  */
  if (FIXNUMP (key) && last_is_meta)
    {
      sequence = Fcopy_sequence (this);
      Faset (sequence, last, make_fixnum (XFIXNUM (key) | meta_modifier));
    }
  else
    {
      if (CONSP (key))
	key = Fcons (XCAR (key), XCDR (key));
      sequence = append_key (this, key);
    }

  if (!NILP (where_is_cache))
    {
      Lisp_Object sequences = Fgethash (binding, where_is_cache, Qnil);
      Fputhash (binding, Fcons (sequence, sequences), where_is_cache);
    }
  else
    d->sequences = Fcons (sequence, d->sequences);
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
  (Lisp_Object buffer, Lisp_Object prefix, Lisp_Object menus)
{
  Lisp_Object nomenu = NILP (menus) ? Qt : Qnil;

  const char *alternate_heading
    = "\
Keyboard translations:\n\n\
You type        Translation\n\
--------        -----------\n";

  CHECK_BUFFER (buffer);

  Lisp_Object shadow = Qnil;
  Lisp_Object outbuf = Fcurrent_buffer ();

  /* Report on alternates for keys.  */
  if (STRINGP (KVAR (current_kboard, Vkeyboard_translate_table)) && !NILP (prefix))
    {
      const unsigned char *translate = SDATA (KVAR (current_kboard, Vkeyboard_translate_table));
      int translate_len = SCHARS (KVAR (current_kboard, Vkeyboard_translate_table));

      for (int c = 0; c < translate_len; c++)
	if (translate[c] != c)
	  {
	    char buf[KEY_DESCRIPTION_SIZE];
	    char *bufend;

	    if (alternate_heading)
	      {
		insert_string (alternate_heading);
		alternate_heading = 0;
	      }

	    bufend = push_key_description (translate[c], buf);
	    insert (buf, bufend - buf);
	    Findent_to (make_fixnum (16), make_fixnum (1));
	    bufend = push_key_description (c, buf);
	    insert (buf, bufend - buf);

	    insert ("\n", 1);

	    /* Insert calls signal_after_change which may GC.  */
	    translate = SDATA (KVAR (current_kboard, Vkeyboard_translate_table));
	  }

      insert ("\n", 1);
    }

  if (!NILP (Vkey_translation_map))
    {
      Lisp_Object msg = build_unibyte_string ("Key translations");
      CALLN (Ffuncall,
	     Qdescribe_map_tree,
	     Vkey_translation_map, Qnil, Qnil, prefix,
	     msg, nomenu, Qt, Qnil, Qnil, buffer);
    }

  /* Print the (major mode) local map.  */
  Lisp_Object start1 = Qnil;
  if (!NILP (KVAR (current_kboard, Voverriding_terminal_local_map)))
    start1 = KVAR (current_kboard, Voverriding_terminal_local_map);

  if (!NILP (start1))
    {
      Lisp_Object msg = build_unibyte_string ("\f\nOverriding Bindings");
      CALLN (Ffuncall,
	     Qdescribe_map_tree,
	     start1, Qt, shadow, prefix,
	     msg, nomenu, Qnil, Qnil, Qnil, buffer);
      shadow = Fcons (start1, shadow);
      start1 = Qnil;
    }
  else if (!NILP (Voverriding_local_map))
    start1 = Voverriding_local_map;

  if (!NILP (start1))
    {
      Lisp_Object msg = build_unibyte_string ("\f\nOverriding Bindings");
      CALLN (Ffuncall,
	     Qdescribe_map_tree,
	     start1, Qt, shadow, prefix,
	     msg, nomenu, Qnil, Qnil, Qnil, buffer);
      shadow = Fcons (start1, shadow);
    }
  else
    {
      /* Print the minor mode and major mode keymaps.  */
      Lisp_Object *modes, *maps;

      /* Temporarily switch to `buffer', so that we can get that buffer's
	 minor modes correctly.  */
      Fset_buffer (buffer);

      int nmaps = current_minor_maps (&modes, &maps);
      Fset_buffer (outbuf);

      start1 = get_local_map (BUF_PT (XBUFFER (buffer)),
			      XBUFFER (buffer), Qkeymap);
      if (!NILP (start1))
	{
	  Lisp_Object msg = build_unibyte_string ("\f\n`keymap' Property Bindings");
	  CALLN (Ffuncall,
		 Qdescribe_map_tree,
		 start1, Qt, shadow, prefix,
		 msg, nomenu, Qnil, Qnil, Qnil, buffer);
	  shadow = Fcons (start1, shadow);
	}

      /* Print the minor mode maps.  */
      for (int i = 0; i < nmaps; i++)
	{
	  /* The title for a minor mode keymap
	     is constructed at run time.
	     We let describe-map-tree do the actual insertion
	     because it takes care of other features when doing so.  */
	  char *title, *p;

	  if (!SYMBOLP (modes[i]))
	    emacs_abort ();

	  USE_SAFE_ALLOCA;
	  p = title = SAFE_ALLOCA (42 + SBYTES (SYMBOL_NAME (modes[i])));
	  *p++ = '\f';
	  *p++ = '\n';
	  *p++ = '`';
	  memcpy (p, SDATA (SYMBOL_NAME (modes[i])),
		  SBYTES (SYMBOL_NAME (modes[i])));
	  p += SBYTES (SYMBOL_NAME (modes[i]));
	  *p++ = '\'';
	  memcpy (p, " Minor Mode Bindings", strlen (" Minor Mode Bindings"));
	  p += strlen (" Minor Mode Bindings");
	  *p = 0;

	  Lisp_Object msg = build_unibyte_string (title);
	  CALLN (Ffuncall,
		 Qdescribe_map_tree,
		 maps[i], Qt, shadow, prefix,
		 msg, nomenu, Qnil, Qnil, Qnil, buffer);
	  shadow = Fcons (maps[i], shadow);
	  SAFE_FREE ();
	}

      start1 = get_local_map (BUF_PT (XBUFFER (buffer)),
			      XBUFFER (buffer), Qlocal_map);
      if (!NILP (start1))
	{
	  if (EQ (start1, BVAR (XBUFFER (buffer), keymap)))
	    {
	      Lisp_Object msg =
		CALLN (Fformat,
		       build_unibyte_string ("\f\n`%s' Major Mode Bindings"),
		       XBUFFER (buffer)->major_mode_);
	      CALLN (Ffuncall,
		     Qdescribe_map_tree,
		     start1, Qt, shadow, prefix,
		     msg, nomenu, Qnil, Qnil, Qnil, buffer);
	    }
	  else
	    {
	      Lisp_Object msg = build_unibyte_string ("\f\n`local-map' Property Bindings");
	      CALLN (Ffuncall,
		     Qdescribe_map_tree,
		     start1, Qt, shadow, prefix,
		     msg, nomenu, Qnil, Qnil, Qnil, buffer);
	    }

	  shadow = Fcons (start1, shadow);
	}
    }

  Lisp_Object msg = build_unibyte_string ("\f\nGlobal Bindings");
  CALLN (Ffuncall,
	 Qdescribe_map_tree,
	 current_global_map, Qt, shadow, prefix,
	 msg, nomenu, Qnil, Qt, Qnil, buffer);

  /* Print the function-key-map translations under this prefix.  */
  if (!NILP (KVAR (current_kboard, Vlocal_function_key_map)))
    {
      Lisp_Object msg = build_unibyte_string ("\f\nFunction key map translations");
      CALLN (Ffuncall,
	     Qdescribe_map_tree,
	     KVAR (current_kboard, Vlocal_function_key_map), Qnil, Qnil, prefix,
	     msg, nomenu, Qt, Qnil, Qnil, buffer);
    }

  /* Print the input-decode-map translations under this prefix.  */
  if (!NILP (KVAR (current_kboard, Vinput_decode_map)))
    {
      Lisp_Object msg = build_unibyte_string ("\f\nInput decoding map translations");
      CALLN (Ffuncall,
	     Qdescribe_map_tree,
	     KVAR (current_kboard, Vinput_decode_map), Qnil, Qnil, prefix,
	     msg, nomenu, Qt, Qnil, Qnil, buffer);
    }
  return Qnil;
}

static void
describe_vector_princ (Lisp_Object elt, Lisp_Object fun)
{
  Findent_to (make_fixnum (16), make_fixnum (1));
  call1 (fun, elt);
  Fterpri (Qnil, Qnil);
}

static void
describe_vector_basic (Lisp_Object elt, Lisp_Object fun)
{
  call1 (fun, elt);
}

DEFUN ("describe-vector", Fdescribe_vector, Sdescribe_vector, 1, 2, 0,
       doc: /* Insert a description of contents of VECTOR.
This is text showing the elements of vector matched against indices.
DESCRIBER is the output function used; nil means use `princ'.  */)
  (Lisp_Object vector, Lisp_Object describer)
{
  specpdl_ref count = SPECPDL_INDEX ();
  if (NILP (describer))
    describer = intern ("princ");
  specbind (Qstandard_output, Fcurrent_buffer ());
  CHECK_VECTOR_OR_CHAR_TABLE (vector);
  describe_vector (vector, Qnil, describer, describe_vector_princ, 0,
		   Qnil, Qnil, 0, 0);

  return unbind_to (count, Qnil);
}

static Lisp_Object fontify_key_properties;

static Lisp_Object
describe_key_maybe_fontify (Lisp_Object str, Lisp_Object prefix,
				   bool keymap_p)
{
  Lisp_Object key_desc = Fkey_description (str, prefix);
  if (keymap_p)
    Fadd_text_properties (make_fixnum (0),
			  make_fixnum (SCHARS (key_desc)),
			  fontify_key_properties,
			  key_desc);
  return key_desc;
}

DEFUN ("help--describe-vector", Fhelp__describe_vector, Shelp__describe_vector, 7, 7, 0,
       doc: /* Insert in the current buffer a description of the contents of VECTOR.
Call DESCRIBER to insert the description of one value found in VECTOR.

PREFIX is a string describing the key which leads to the keymap that
this vector is in.

If PARTIAL, it means do not mention suppressed commands.

SHADOW is a list of keymaps that shadow this map.
If it is non-nil, look up the key in those maps and don't mention it
if it is defined by any of them.

ENTIRE-MAP is the keymap in which this vector appears.
If the definition in effect in the whole map does not match
the one in this keymap, we ignore this one.  */)
  (Lisp_Object vector, Lisp_Object prefix, Lisp_Object describer,
   Lisp_Object partial, Lisp_Object shadow, Lisp_Object entire_map,
   Lisp_Object mention_shadow)
{
  specpdl_ref count = SPECPDL_INDEX ();
  specbind (Qstandard_output, Fcurrent_buffer ());
  CHECK_VECTOR_OR_CHAR_TABLE (vector);

  bool b_partial = NILP (partial) ? false : true;
  bool b_mention_shadow = NILP (mention_shadow) ? false : true;

  describe_vector (vector, prefix, describer, describe_vector_basic, b_partial,
		   shadow, entire_map, true, b_mention_shadow);
  return unbind_to (count, Qnil);
}

/* Insert in the current buffer a description of the contents of VECTOR.
   Call ELT_DESCRIBER to insert the description of one value found
   in VECTOR.

   ELT_PREFIX describes what "comes before" the keys or indices defined
   by this vector.  This is a human-readable string whose size
   is not necessarily related to the situation.

   If the vector is in a keymap, ELT_PREFIX is a prefix key which
   leads to this keymap.

   If the vector is a chartable, ELT_PREFIX is the vector
   of bytes that lead to the character set or portion of a character
   set described by this chartable.

   If PARTIAL, it means do not mention suppressed commands
   (that assumes the vector is in a keymap).

   SHADOW is a list of keymaps that shadow this map.
   If it is non-nil, then we look up the key in those maps
   and we don't mention it now if it is defined by any of them.

   ENTIRE_MAP is the keymap in which this vector appears.
   If the definition in effect in the whole map does not match
   the one in this vector, we ignore this one.

   ARGS is simply passed as the second argument to ELT_DESCRIBER.

   KEYMAP_P is 1 if vector is known to be a keymap, so map ESC to M-.

   ARGS is simply passed as the second argument to ELT_DESCRIBER.  */

static void
describe_vector (Lisp_Object vector, Lisp_Object prefix, Lisp_Object args,
		 void (*elt_describer) (Lisp_Object, Lisp_Object),
		 bool partial, Lisp_Object shadow, Lisp_Object entire_map,
		 bool keymap_p, bool mention_shadow)
{
  Lisp_Object elt_prefix = Qnil;
  Lisp_Object suppress = Qnil;
  bool first = true;
  /* Range of elements to be handled.  */
  int to, stop;

  if (!keymap_p)
    {
      if (!NILP (prefix) && XFIXNAT (Flength (prefix)) > 0)
	{
	  AUTO_STRING (space, " ");
	  elt_prefix = concat2 (Fkey_description (prefix, Qnil), space);
	}
      prefix = Qnil;
    }

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per vector element, we don't want to cons up a
     fresh vector every time.  */
  Lisp_Object kludge = make_nil_vector (1);

  if (partial)
    suppress = intern ("suppress-keymap");

  /* STOP is a boundary between normal characters (-#x3FFF7F) and
     8-bit characters (#x3FFF80-), used below when VECTOR is a
     char-table.  */
  if (CHAR_TABLE_P (vector))
    stop = MAX_5_BYTE_CHAR + 1, to = MAX_CHAR + 1;
  else
    stop = to = ASIZE (vector);

  for (int i = 0; ; i++)
    {
      bool this_shadowed = false;
      Lisp_Object shadowed_by = Qnil;
      int range_beg;
      Lisp_Object val, tem2;

      maybe_quit ();

      if (i == stop)
	{
	  if (i == to)
	    break;
	  stop = to;
	}

      int starting_i = i;

      if (CHAR_TABLE_P (vector))
	{
	  /* Find the value in VECTOR for the first character in the
	     range [RANGE_BEG..STOP), and update the range to include
	     only the characters whose value is the same as that of
	     the first in the range.  */
	  range_beg = i;
	  i = stop - 1;
	  val = char_table_ref_and_range (vector, range_beg, &range_beg, &i);
	}
      else
	val = AREF (vector, i);
      Lisp_Object definition = get_keyelt (val, 0);

      if (NILP (definition)) continue;

      /* Don't mention suppressed commands.  */
      if (SYMBOLP (definition) && partial)
	{
	  Lisp_Object tem = Fget (definition, suppress);

	  if (!NILP (tem)) continue;
	}

      Lisp_Object character = make_fixnum (starting_i);
      ASET (kludge, 0, character);

      /* If this binding is shadowed by some other map, ignore it.  */
      if (!NILP (shadow))
	{
	  shadowed_by = shadow_lookup (shadow, kludge, Qt, 0);

	  if (!NILP (shadowed_by) && !EQ (shadowed_by, definition))
	    {
	      if (mention_shadow)
		this_shadowed = true;
	      else
		continue;
	    }
	}

      /* Ignore this definition if it is shadowed by an earlier
	 one in the same keymap.  */
      if (!NILP (entire_map))
	{
	  Lisp_Object tem = Flookup_key (entire_map, kludge, Qt);

	  if (!EQ (tem, definition))
	    continue;
	}

      if (first)
	{
	  insert ("\n", 1);
	  first = false;
	}

      /* Output the prefix that applies to every entry in this map.  */
      if (!NILP (elt_prefix))
	insert1 (elt_prefix);

      insert1 (describe_key_maybe_fontify (kludge, prefix, keymap_p));

      /* Find all consecutive characters or rows that have the same
	 definition.  */
      if (!CHAR_TABLE_P (vector))
	{
	  while (i + 1 < stop
		 && (tem2 = get_keyelt (AREF (vector, i + 1), 0),
		     !NILP (tem2))
		 && !NILP (Fequal (tem2, definition)))
	    i++;
	}

      /* Make sure found consecutive keys are either not shadowed or,
	 if they are, that they are shadowed by the same command.  */
      if (!NILP (Vdescribe_bindings_check_shadowing_in_ranges)
	  && CHAR_TABLE_P (vector) && i != starting_i
	  && (!EQ (Vdescribe_bindings_check_shadowing_in_ranges,
		   Qignore_self_insert)
	      || !EQ (definition, Qself_insert_command)))
	{
	  Lisp_Object key = make_nil_vector (1);
	  for (int j = range_beg + 1; j <= i; j++)
	    {
	      ASET (key, 0, make_fixnum (j));
	      Lisp_Object tem = shadow_lookup (shadow, key, Qt, 0);
	      if (NILP (Fequal (tem, shadowed_by)))
		i = j - 1;
	    }
	}

      /* If we have a range of more than one character,
	 print where the range reaches to.  */

      if (i != starting_i)
	{
	  insert (" .. ", 4);

	  ASET (kludge, 0, make_fixnum (i));

	  if (!NILP (elt_prefix))
	    insert1 (elt_prefix);

	  insert1 (describe_key_maybe_fontify (kludge, prefix, keymap_p));
	}

      /* Print a description of the definition of this character.
	 elt_describer will take care of spacing out far enough
	 for alignment purposes.  */
      (*elt_describer) (definition, args);

      if (this_shadowed)
	{
	  SET_PT (PT - 1);
	  if (SYMBOLP (shadowed_by))
	    {
	      static char const fmt[] = "  (currently shadowed by `%s')";
	      USE_SAFE_ALLOCA;
	      char *buffer =
		SAFE_ALLOCA (sizeof fmt + SBYTES (SYMBOL_NAME (shadowed_by)));
	      esprintf (buffer, fmt, SDATA (SYMBOL_NAME (shadowed_by)));
	      insert_string (buffer);
	      SAFE_FREE();
	    }
	  else	/* Could be a keymap, a lambda, or a keyboard macro.  */
	    insert_string ("  (currently shadowed)");
	  SET_PT (PT + 1);
	}
    }

  if (CHAR_TABLE_P (vector) && ! NILP (XCHAR_TABLE (vector)->defalt))
    {
      if (!NILP (elt_prefix))
	insert1 (elt_prefix);
      insert ("default", 7);
      (*elt_describer) (XCHAR_TABLE (vector)->defalt, args);
    }
}

void
syms_of_keymap (void)
{
  DEFSYM (Qkeymap, "keymap");
  DEFSYM (Qdescribe_map_tree, "describe-map-tree");

  DEFSYM (Qkeymap_canonicalize, "keymap-canonicalize");

  /* Now we are ready to set up this property, so we can
     create char tables.  */
  Fput (Qkeymap, Qchar_table_extra_slots, make_fixnum (0));

  /* Initialize the keymaps standardly used.
     Each one is the value of a Lisp variable, and is also
     pointed to by a C variable */

  current_global_map = Qnil;
  staticpro (&current_global_map);

  exclude_keys = pure_list
    (pure_cons (build_pure_c_string ("DEL"), build_pure_c_string ("\\d")),
     pure_cons (build_pure_c_string ("TAB"), build_pure_c_string ("\\t")),
     pure_cons (build_pure_c_string ("RET"), build_pure_c_string ("\\r")),
     pure_cons (build_pure_c_string ("ESC"), build_pure_c_string ("\\e")),
     pure_cons (build_pure_c_string ("SPC"), build_pure_c_string (" ")));
  staticpro (&exclude_keys);

  DEFVAR_LISP ("minibuffer-local-map", Vminibuffer_local_map,
	       doc: /* Default keymap to use when reading from the minibuffer.  */);
  Vminibuffer_local_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("minor-mode-map-alist", Vminor_mode_map_alist,
	       doc: /* Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings if VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.  */);
  Vminor_mode_map_alist = Qnil;

  DEFVAR_LISP ("minor-mode-overriding-map-alist", Vminor_mode_overriding_map_alist,
	       doc: /* Alist of keymaps to use for minor modes, in current major mode.
This variable is an alist just like `minor-mode-map-alist', and it is
used the same way (and before `minor-mode-map-alist'); however,
it is provided for major modes to bind locally.  */);
  Vminor_mode_overriding_map_alist = Qnil;

  DEFVAR_LISP ("emulation-mode-map-alists", Vemulation_mode_map_alists,
	       doc: /* List of keymap alists to use for emulation modes.
It is intended for modes or packages using multiple minor-mode keymaps.
Each element is a keymap alist just like `minor-mode-map-alist', or a
symbol with a variable binding which is a keymap alist, and it is used
the same way.  The "active" keymaps in each alist are used before
`minor-mode-map-alist' and `minor-mode-overriding-map-alist'.  */);
  Vemulation_mode_map_alists = Qnil;

  DEFVAR_LISP ("where-is-preferred-modifier", Vwhere_is_preferred_modifier,
	       doc: /* Preferred modifier key to use for `where-is'.
When a single binding is requested, `where-is' will return one that
uses this modifier key if possible.  If nil, or if no such binding
exists, bindings using keys without modifiers (or only with meta) will
be preferred.  */);
  Vwhere_is_preferred_modifier = Qnil;
  where_is_preferred_modifier = 0;

  DEFVAR_LISP ("describe-bindings-check-shadowing-in-ranges",
	       Vdescribe_bindings_check_shadowing_in_ranges,
	       doc: /* If non-nil, consider command shadowing when describing ranges of keys.
If the value is t, describing bindings of consecutive keys will not
report them as a single range if they are shadowed by different
minor-mode commands.
If the value is `ignore-self-insert', assume that consecutive keys
bound to `self-insert-command' are not all shadowed; this speeds up
commands such as \\[describe-bindings] and \\[describe-mode], but could miss some shadowing.
Any other non-nil value is treated is t.

Beware: setting this non-nil could potentially slow down commands
that describe key bindings.  That is why the default is nil.  */);
  Vdescribe_bindings_check_shadowing_in_ranges = Qnil;

  DEFSYM (Qself_insert_command, "self-insert-command");
  DEFSYM (Qignore_self_insert, "ignore-self-insert");

  DEFSYM (Qmenu_bar, "menu-bar");
  DEFSYM (Qmode_line, "mode-line");

  staticpro (&Vmouse_events);
  Vmouse_events = pure_list (Qmenu_bar, Qtab_bar, Qtool_bar,
			     Qtab_line, Qheader_line, Qmode_line,
			     intern_c_string ("mouse-1"),
			     intern_c_string ("mouse-2"),
			     intern_c_string ("mouse-3"),
			     intern_c_string ("mouse-4"),
			     intern_c_string ("mouse-5"));

  /* Keymap used for minibuffers when doing completion.  */
  /* Keymap used for minibuffers when doing completion and require a match.  */
  DEFSYM (Qkeymapp, "keymapp");
  DEFSYM (Qnon_ascii, "non-ascii");
  DEFSYM (Qmenu_item, "menu-item");
  DEFSYM (Qremap, "remap");
  DEFSYM (QCadvertised_binding, ":advertised-binding");

  command_remapping_vector = make_vector (2, Qremap);
  staticpro (&command_remapping_vector);

  where_is_cache_keymaps = Qt;
  where_is_cache = Qnil;
  staticpro (&where_is_cache);
  staticpro (&where_is_cache_keymaps);

  DEFSYM (Qfont_lock_face, "font-lock-face");
  DEFSYM (Qhelp_key_binding, "help-key-binding");
  staticpro (&fontify_key_properties);
  fontify_key_properties = Fcons (Qfont_lock_face,
				  Fcons (Qhelp_key_binding, Qnil));

  defsubr (&Skeymapp);
  defsubr (&Skeymap_parent);
  defsubr (&Skeymap_prompt);
  defsubr (&Sset_keymap_parent);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Smap_keymap_internal);
  defsubr (&Smap_keymap);
  defsubr (&Scopy_keymap);
  defsubr (&Scommand_remapping);
  defsubr (&Skey_binding);
  defsubr (&Sminor_mode_key_binding);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Scurrent_global_map);
  defsubr (&Scurrent_minor_mode_maps);
  defsubr (&Scurrent_active_maps);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Skeymap__get_keyelt);
  defsubr (&Shelp__describe_vector);
  defsubr (&Sdescribe_vector);
  defsubr (&Ssingle_key_description);
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Sdescribe_buffer_bindings);

  DEFSYM (Qkey_parse, "key-parse");
  DEFSYM (Qkey_valid_p, "key-valid-p");

  DEFSYM (Qnon_key_event, "non-key-event");
}
