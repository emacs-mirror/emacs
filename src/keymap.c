/* Manipulation of keymaps
   Copyright (C) 1985, 1986, 1987, 1988, 1992 Free Software Foundation, Inc.

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


#include "config.h"
#include <stdio.h>
#undef NULL
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "keyboard.h"
#include "termhooks.h"
#include "blockinput.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

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

/* Keymap mapping ASCII function key sequences onto their preferred forms.
   Initialized by the terminal-specific lisp files.  See DEFVAR for more
   documentation.  */
Lisp_Object Vfunction_key_map;

Lisp_Object Qkeymapp, Qkeymap, Qnon_ascii;

/* A char over 0200 in a key sequence
   is equivalent to prefixing with this character.  */

extern Lisp_Object meta_prefix_char;

void describe_map_tree ();
static Lisp_Object describe_buffer_bindings ();
static void describe_command ();
static void describe_map ();
static void describe_map_2 ();

/* Keymap object support - constructors and predicates.			*/

DEFUN ("make-keymap", Fmake_keymap, Smake_keymap, 0, 1, 0,
  "Construct and return a new keymap, of the form (keymap VECTOR . ALIST).\n\
VECTOR is a 128-element vector which holds the bindings for the ASCII\n\
characters.  ALIST is an assoc-list which holds bindings for function keys,\n\
mouse events, and any other things that appear in the input stream.\n\
All entries in it are initially nil, meaning \"command undefined\".\n\n\
The optional arg STRING supplies a menu name for the keymap\n\
in case you use it as a menu with `x-popup-menu'.")
  (string)
     Lisp_Object string;
{
  Lisp_Object tail;
  if (!NILP (string))
    tail = Fcons (string, Qnil);
  else
    tail = Qnil;
  return Fcons (Qkeymap,
		Fcons (Fmake_vector (make_number (DENSE_TABLE_SIZE), Qnil),
		       tail));
}

DEFUN ("make-sparse-keymap", Fmake_sparse_keymap, Smake_sparse_keymap, 0, 1, 0,
  "Construct and return a new sparse-keymap list.\n\
Its car is `keymap' and its cdr is an alist of (CHAR . DEFINITION),\n\
which binds the character CHAR to DEFINITION, or (SYMBOL . DEFINITION),\n\
which binds the function key or mouse event SYMBOL to DEFINITION.\n\
Initially the alist is nil.\n\n\
The optional arg STRING supplies a menu name for the keymap\n\
in case you use it as a menu with `x-popup-menu'.")
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

/* Define character fromchar in map frommap as an alias for character
   tochar in map tomap.  Subsequent redefinitions of the latter WILL
   affect the former. */

#if 0
void
synkey (frommap, fromchar, tomap, tochar)
     struct Lisp_Vector *frommap, *tomap;
     int fromchar, tochar;
{
  Lisp_Object v, c;
  XSET (v, Lisp_Vector, tomap);
  XFASTINT (c) = tochar;
  frommap->contents[fromchar] = Fcons (v, c);
}
#endif /* 0 */

DEFUN ("keymapp", Fkeymapp, Skeymapp, 1, 1, 0,
  "Return t if ARG is a keymap.\n\
\n\
A keymap is a list (keymap . ALIST), a list (keymap VECTOR . ALIST),\n\
or a symbol whose function definition is a keymap is itself a keymap.\n\
ALIST elements look like (CHAR . DEFN) or (SYMBOL . DEFN);\n\
VECTOR is a 128-element vector of bindings for ASCII characters.")
  (object)
     Lisp_Object object;
{
  return (NILP (get_keymap_1 (object, 0, 0)) ? Qnil : Qt);
}

/* Check that OBJECT is a keymap (after dereferencing through any
   symbols).  If it is, return it.

   If AUTOLOAD is non-zero and OBJECT is a symbol whose function value
   is an autoload form, do the autoload and try again.

   ERROR controls how we respond if OBJECT isn't a keymap.
   If ERROR is non-zero, signal an error; otherwise, just return Qnil.

   Note that most of the time, we don't want to pursue autoloads.
   Functions like Faccessible_keymaps which scan entire keymap trees
   shouldn't load every autoloaded keymap.  I'm not sure about this,
   but it seems to me that only read_key_sequence, Flookup_key, and
   Fdefine_key should cause keymaps to be autoloaded.  */

Lisp_Object
get_keymap_1 (object, error, autoload)
     Lisp_Object object;
     int error, autoload;
{
  Lisp_Object tem;

 autoload_retry:
  tem = indirect_function (object);
  if (CONSP (tem) && EQ (XCONS (tem)->car, Qkeymap))
    return tem;

  /* Should we do an autoload?  Autoload forms for keymaps have
     Qkeymap as their fifth element.  */
  if (autoload
      && XTYPE (object) == Lisp_Symbol
      && CONSP (tem)
      && EQ (XCONS (tem)->car, Qautoload))
    {
      Lisp_Object tail;

      tail = Fnth (make_number (4), tem);
      if (EQ (tail, Qkeymap))
	{
	  struct gcpro gcpro1, gcpro2;

	  GCPRO2 (tem, object)
	    do_autoload (tem, object);
	  UNGCPRO;

	  goto autoload_retry;
	}
    }

  if (error)
    wrong_type_argument (Qkeymapp, object);
  else
    return Qnil;
}


/* Follow any symbol chaining, and return the keymap denoted by OBJECT.
   If OBJECT doesn't denote a keymap at all, signal an error.  */
Lisp_Object
get_keymap (object)
     Lisp_Object object;
{
  return get_keymap_1 (object, 0, 0);
}


/* Look up IDX in MAP.  IDX may be any sort of event.
   Note that this does only one level of lookup; IDX must be a single
   event, not a sequence. 

   If T_OK is non-zero, bindings for Qt are treated as default
   bindings; any key left unmentioned by other tables and bindings is
   given the binding of Qt.  

   If T_OK is zero, bindings for Qt are not treated specially.  */

Lisp_Object
access_keymap (map, idx, t_ok)
     Lisp_Object map;
     Lisp_Object idx;
     int t_ok;
{
  /* If idx is a list (some sort of mouse click, perhaps?),
     the index we want to use is the car of the list, which
     ought to be a symbol.  */
  idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (XTYPE (idx) == Lisp_Symbol)
    idx = reorder_modifiers (idx);

  {
    Lisp_Object tail;
    Lisp_Object t_binding = Qnil;

    for (tail = map; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	Lisp_Object binding = XCONS (tail)->car;

	switch (XTYPE (binding))
	  {
	  case Lisp_Cons:
	    if (EQ (XCONS (binding)->car, idx))
	      return XCONS (binding)->cdr;
	    if (t_ok && EQ (XCONS (binding)->car, Qt))
	      t_binding = XCONS (binding)->cdr;
	    break;

	  case Lisp_Vector:
	    if (XVECTOR (binding)->size == DENSE_TABLE_SIZE
		&& XTYPE (idx) == Lisp_Int
		&& XINT (idx) >= 0
		&& XINT (idx) < DENSE_TABLE_SIZE)
	      return XVECTOR (binding)->contents[XINT (idx)];
	    break;
	  }

	QUIT;
      }

    return t_binding;
  }
}

/* Given OBJECT which was found in a slot in a keymap,
   trace indirect definitions to get the actual definition of that slot.
   An indirect definition is a list of the form
   (KEYMAP . INDEX), where KEYMAP is a keymap or a symbol defined as one
   and INDEX is the object to look up in KEYMAP to yield the definition.

   Also if OBJECT has a menu string as the first element,
   remove that.  Also remove a menu help string as second element.  */

Lisp_Object
get_keyelt (object)
     register Lisp_Object object;
{
  while (1)
    {
      register Lisp_Object map, tem;

      /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
      map = get_keymap_1 (Fcar_safe (object), 0, 0);
      tem = Fkeymapp (map);
      if (!NILP (tem))
	object = access_keymap (map, Fcdr (object), 0);
      
      /* If the keymap contents looks like (STRING . DEFN),
	 use DEFN.
	 Keymap alist elements like (CHAR MENUSTRING . DEFN)
	 will be used by HierarKey menus.  */
      else if (XTYPE (object) == Lisp_Cons
	       && XTYPE (XCONS (object)->car) == Lisp_String)
	{
	  object = XCONS (object)->cdr;
	  /* Also remove a menu help string, if any,
	     following the menu item name.  */
	  if (XTYPE (object) == Lisp_Cons
	      && XTYPE (XCONS (object)->car) == Lisp_String)
	    object = XCONS (object)->cdr;
	}

      else
	/* Anything else is really the value.  */
	return object;
    }
}

Lisp_Object
store_in_keymap (keymap, idx, def)
     Lisp_Object keymap;
     register Lisp_Object idx;
     register Lisp_Object def;
{
  if (XTYPE (keymap) != Lisp_Cons
      || ! EQ (XCONS (keymap)->car, Qkeymap))
    error ("attempt to define a key in a non-keymap");

  /* If idx is a list (some sort of mouse click, perhaps?),
     the index we want to use is the car of the list, which
     ought to be a symbol.  */
  idx = EVENT_HEAD (idx);

  /* If idx is a symbol, it might have modifiers, which need to
     be put in the canonical order.  */
  if (XTYPE (idx) == Lisp_Symbol)
    idx = reorder_modifiers (idx);


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

    for (tail = XCONS (keymap)->cdr; CONSP (tail); tail = XCONS (tail)->cdr)
      {
	Lisp_Object elt = XCONS (tail)->car;

	switch (XTYPE (elt))
	  {
	  case Lisp_Vector:
	    if (XVECTOR (elt)->size != DENSE_TABLE_SIZE)
	      break;
	    if (XTYPE (idx) == Lisp_Int
		&& XINT (idx) >= 0 && XINT (idx) < DENSE_TABLE_SIZE)
	      {
		XVECTOR (elt)->contents[XFASTINT (idx)] = def;
		return def;
	      }
	    insertion_point = tail;
	    break;

	  case Lisp_Cons:
	    if (EQ (idx, XCONS (elt)->car))
	      {
		XCONS (elt)->cdr = def;
		return def;
	      }
	    break;

	  case Lisp_Symbol:
	    /* If we find a 'keymap' symbol in the spine of KEYMAP,
               then we must have found the start of a second keymap
               being used as the tail of KEYMAP, and a binding for IDX
               should be inserted before it.  */
	    if (EQ (elt, Qkeymap))
	      goto keymap_end;
	    break;
	  }

	QUIT;
      }

  keymap_end:
    /* We have scanned the entire keymap, and not found a binding for
       IDX.  Let's add one.  */
    XCONS (insertion_point)->cdr =
      Fcons (Fcons (idx, def), XCONS (insertion_point)->cdr);
  }
	  
  return def;
}


DEFUN ("copy-keymap", Fcopy_keymap, Scopy_keymap, 1, 1, 0,
  "Return a copy of the keymap KEYMAP.\n\
The copy starts out with the same definitions of KEYMAP,\n\
but changing either the copy or KEYMAP does not affect the other.\n\
Any key definitions that are subkeymaps are recursively copied.\n\
However, a key definition which is a symbol whose definition is a keymap\n\
is not copied.")
  (keymap)
     Lisp_Object keymap;
{
  register Lisp_Object copy, tail;

  copy = Fcopy_alist (get_keymap (keymap));

  for (tail = copy; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      Lisp_Object elt = XCONS (tail)->car;

      if (XTYPE (elt) == Lisp_Vector
	  && XVECTOR (elt)->size == DENSE_TABLE_SIZE)
	{
	  int i;

	  elt = Fcopy_sequence (elt);
	  XCONS (tail)->car = elt;

	  for (i = 0; i < DENSE_TABLE_SIZE; i++)
	    if (XTYPE (XVECTOR (elt)->contents[i]) != Lisp_Symbol
		&& Fkeymapp (XVECTOR (elt)->contents[i]))
	      XVECTOR (elt)->contents[i] =
		Fcopy_keymap (XVECTOR (elt)->contents[i]);
	}
      else if (CONSP (elt)
	       && XTYPE (XCONS (elt)->cdr) != Lisp_Symbol
	       && ! NILP (Fkeymapp (XCONS (elt)->cdr)))
	XCONS (elt)->cdr = Fcopy_keymap (XCONS (elt)->cdr);
    }

  return copy;
}

/* Simple Keymap mutators and accessors.				*/

DEFUN ("define-key", Fdefine_key, Sdefine_key, 3, 3, 0,
  "Args KEYMAP, KEY, DEF.  Define key sequence KEY, in KEYMAP, as DEF.\n\
KEYMAP is a keymap.  KEY is a string or a vector of symbols and characters\n\
meaning a sequence of keystrokes and events.\n\
DEF is anything that can be a key's definition:\n\
 nil (means key is undefined in this keymap),\n\
 a command (a Lisp function suitable for interactive calling)\n\
 a string (treated as a keyboard macro),\n\
 a keymap (to define a prefix key),\n\
 a symbol.  When the key is looked up, the symbol will stand for its\n\
    function definition, which should at that time be one of the above,\n\
    or another symbol whose function definition is used, etc.\n\
 a cons (STRING . DEFN), meaning that DEFN is the definition\n\
    (DEFN should be a valid definition in its own right),\n\
 or a cons (KEYMAP . CHAR), meaning use definition of CHAR in map KEYMAP.\n\
\n\
If KEYMAP is a sparse keymap, the pair binding KEY to DEF is added at\n\
the front of KEYMAP.")
  (keymap, key, def)
     Lisp_Object keymap;
     Lisp_Object key;
     Lisp_Object def;
{
  register int idx;
  register Lisp_Object c;
  register Lisp_Object tem;
  register Lisp_Object cmd;
  int metized = 0;
  int meta_bit;
  int length;
  struct gcpro gcpro1, gcpro2, gcpro3;

  keymap = get_keymap (keymap);

  if (XTYPE (key) != Lisp_Vector
      && XTYPE (key) != Lisp_String)
    key = wrong_type_argument (Qarrayp, key);

  length = XFASTINT (Flength (key));
  if (length == 0)
    return Qnil;

  GCPRO3 (keymap, key, def);

  if (XTYPE (key) == Lisp_Vector)
    meta_bit = meta_modifier;
  else
    meta_bit = 0x80;

  idx = 0;
  while (1)
    {
      c = Faref (key, make_number (idx));

      if (XTYPE (c) == Lisp_Int
	  && (XINT (c) & meta_bit)
	  && !metized)
	{
	  c = meta_prefix_char;
	  metized = 1;
	}
      else
	{
	  if (XTYPE (c) == Lisp_Int)
	    XSETINT (c, XINT (c) & ~meta_bit);

	  metized = 0;
	  idx++;
	}

      if (idx == length)
	RETURN_UNGCPRO (store_in_keymap (keymap, c, def));

      cmd = get_keyelt (access_keymap (keymap, c, 0));

      if (NILP (cmd))
	{
	  cmd = Fmake_sparse_keymap (Qnil);
	  store_in_keymap (keymap, c, cmd);
	}

      keymap = get_keymap_1 (cmd, 0, 1);
      if (NILP (keymap))
	{
	  /* We must use Fkey_description rather than just passing key to
	     error; key might be a vector, not a string.  */
	  Lisp_Object description = Fkey_description (key);

	  error ("Key sequence %s uses invalid prefix characters",
		 XSTRING (description)->data);
	}
    }
}

/* Value is number if KEY is too long; NIL if valid but has no definition. */

DEFUN ("lookup-key", Flookup_key, Slookup_key, 2, 3, 0,
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.\n\
nil means undefined.  See doc of `define-key' for kinds of definitions.\n\
\n\
A number as value means KEY is \"too long\";\n\
that is, characters or symbols in it except for the last one\n\
fail to be a valid sequence of prefix characters in KEYMAP.\n\
The number is how many characters at the front of KEY\n\
it takes to reach a non-prefix command.\n\
\n\
Normally, `lookup-key' ignores bindings for t, which act as default\n\
bindings, used when nothing else in the keymap applies; this makes it\n\
useable as a general function for probing keymaps.  However, if the\n\
third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will\n\
recognize the default bindings, just as `read-key-sequence' does.")
  (keymap, key, accept_default)
     register Lisp_Object keymap;
     Lisp_Object key;
     Lisp_Object accept_default;
{
  register int idx;
  register Lisp_Object tem;
  register Lisp_Object cmd;
  register Lisp_Object c;
  int metized = 0;
  int length;
  int t_ok = ! NILP (accept_default);
  int meta_bit;

  keymap = get_keymap (keymap);

  if (XTYPE (key) != Lisp_Vector
      && XTYPE (key) != Lisp_String)
    key = wrong_type_argument (Qarrayp, key);

  length = XFASTINT (Flength (key));
  if (length == 0)
    return keymap;

  if (XTYPE (key) == Lisp_Vector)
    meta_bit = meta_modifier;
  else
    meta_bit = 0x80;

  idx = 0;
  while (1)
    {
      c = Faref (key, make_number (idx));

      if (XTYPE (c) == Lisp_Int
	  && (XINT (c) & meta_bit)
	  && !metized)
	{
	  c = meta_prefix_char;
	  metized = 1;
	}
      else
	{
	  if (XTYPE (c) == Lisp_Int)
	    XSETINT (c, XINT (c) & ~meta_bit);

	  metized = 0;
	  idx++;
	}

      cmd = get_keyelt (access_keymap (keymap, c, t_ok));
      if (idx == length)
	return cmd;

      keymap = get_keymap_1 (cmd, 0, 0);
      if (NILP (keymap))
	return make_number (idx);

      QUIT;
    }
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


/* Global, local, and minor mode keymap stuff.				*/

/* We can't put these variables inside current_minor_maps, since under
   some systems, static gets macro-defined to be the empty string.
   Ickypoo.  */
static Lisp_Object *cmm_modes, *cmm_maps;
static int cmm_size;

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
  Lisp_Object alist, assoc, var, val;

  for (alist = Vminor_mode_map_alist;
       CONSP (alist);
       alist = XCONS (alist)->cdr)
    if (CONSP (assoc = XCONS (alist)->car)
	&& XTYPE (var = XCONS (assoc)->car) == Lisp_Symbol
	&& ! EQ ((val = find_symbol_value (var)), Qunbound)
	&& ! NILP (val))
      {
	if (i >= cmm_size)
	  {
	    Lisp_Object *newmodes, *newmaps;

	    if (cmm_maps)
	      {
		BLOCK_INPUT;
		newmodes = (Lisp_Object *) realloc (cmm_modes, cmm_size *= 2);
		newmaps  = (Lisp_Object *) realloc (cmm_maps,  cmm_size);
		UNBLOCK_INPUT;
	      }
	    else
	      {
		BLOCK_INPUT;
		newmodes = (Lisp_Object *) malloc (cmm_size = 30);
		newmaps  = (Lisp_Object *) malloc (cmm_size);
		UNBLOCK_INPUT;
	      }

	    if (newmaps && newmodes)
	      {
		cmm_modes = newmodes;
		cmm_maps = newmaps;
	      }
	    else
	      break;
	  }
	cmm_modes[i] = var;
	cmm_maps [i] = XCONS (assoc)->cdr;
	i++;
      }

  if (modeptr) *modeptr = cmm_modes;
  if (mapptr)  *mapptr  = cmm_maps;
  return i;
}

DEFUN ("key-binding", Fkey_binding, Skey_binding, 1, 2, 0,
  "Return the binding for command KEY in current keymaps.\n\
KEY is a string or vector, a sequence of keystrokes.\n\
The binding is probably a symbol with a function definition.\n\
\n\
Normally, `key-binding' ignores bindings for t, which act as default\n\
bindings, used when nothing else in the keymap applies; this makes it\n\
useable as a general function for probing keymaps.  However, if the\n\
third optional argument ACCEPT-DEFAULT is non-nil, `key-binding' will\n\
recognize the default bindings, just as `read-key-sequence' does.")
  (key, accept_default)
     Lisp_Object key;
{
  Lisp_Object *maps, value;
  int nmaps, i;

  nmaps = current_minor_maps (0, &maps);
  for (i = 0; i < nmaps; i++)
    if (! NILP (maps[i]))
      {
	value = Flookup_key (maps[i], key, accept_default);
	if (! NILP (value) && XTYPE (value) != Lisp_Int)
	  return value;
      }

  if (! NILP (current_buffer->keymap))
    {
      value = Flookup_key (current_buffer->keymap, key, accept_default);
      if (! NILP (value) && XTYPE (value) != Lisp_Int)
	return value;
    }

  value = Flookup_key (current_global_map, key, accept_default);
  if (! NILP (value) && XTYPE (value) != Lisp_Int)
    return value;
  
  return Qnil;
}

DEFUN ("local-key-binding", Flocal_key_binding, Slocal_key_binding, 1, 2, 0,
  "Return the binding for command KEYS in current local keymap only.\n\
KEYS is a string, a sequence of keystrokes.\n\
The binding is probably a symbol with a function definition.\n\
\n\
If optional argument ACCEPT-DEFAULT is non-nil, recognize default\n\
bindings; see the description of `lookup-key' for more details about this.")
  (keys, accept_default)
     Lisp_Object keys, accept_default;
{
  register Lisp_Object map;
  map = current_buffer->keymap;
  if (NILP (map))
    return Qnil;
  return Flookup_key (map, keys, accept_default);
}

DEFUN ("global-key-binding", Fglobal_key_binding, Sglobal_key_binding, 1, 2, 0,
  "Return the binding for command KEYS in current global keymap only.\n\
KEYS is a string, a sequence of keystrokes.\n\
The binding is probably a symbol with a function definition.\n\
This function's return values are the same as those of lookup-key\n\
(which see).\n\
\n\
If optional argument ACCEPT-DEFAULT is non-nil, recognize default\n\
bindings; see the description of `lookup-key' for more details about this.")
  (keys, accept_default)
     Lisp_Object keys, accept_default;
{
  return Flookup_key (current_global_map, keys, accept_default);
}

DEFUN ("minor-mode-key-binding", Fminor_mode_key_binding, Sminor_mode_key_binding, 1, 2, 0,
  "Find the visible minor mode bindings of KEY.\n\
Return an alist of pairs (MODENAME . BINDING), where MODENAME is the\n\
the symbol which names the minor mode binding KEY, and BINDING is\n\
KEY's definition in that mode.  In particular, if KEY has no\n\
minor-mode bindings, return nil.  If the first binding is a\n\
non-prefix, all subsequent bindings will be omitted, since they would\n\
be ignored.  Similarly, the list doesn't include non-prefix bindings\n\
that come after prefix bindings.\n\
\n\
If optional argument ACCEPT-DEFAULT is non-nil, recognize default\n\
bindings; see the description of `lookup-key' for more details about this.")
  (key, accept_default)
     Lisp_Object key, accept_default;
{
  Lisp_Object *modes, *maps;
  int nmaps;
  Lisp_Object binding;
  int i, j;

  nmaps = current_minor_maps (&modes, &maps);

  for (i = j = 0; i < nmaps; i++)
    if (! NILP (maps[i])
	&& ! NILP (binding = Flookup_key (maps[i], key, accept_default))
	&& XTYPE (binding) != Lisp_Int)
      {
	if (! NILP (get_keymap (binding)))
	  maps[j++] = Fcons (modes[i], binding);
	else if (j == 0)
	  return Fcons (Fcons (modes[i], binding), Qnil);
      }

  return Flist (j, maps);
}

DEFUN ("global-set-key", Fglobal_set_key, Sglobal_set_key, 2, 2,
  "kSet key globally: \nCSet key %s to command: ",
  "Give KEY a global binding as COMMAND.\n\
COMMAND is a symbol naming an interactively-callable function.\n\
KEY is a string representing a sequence of keystrokes.\n\
Note that if KEY has a local binding in the current buffer\n\
that local binding will continue to shadow any global binding.")
  (keys, function)
     Lisp_Object keys, function;
{
  if (XTYPE (keys) != Lisp_Vector
      && XTYPE (keys) != Lisp_String)
    keys = wrong_type_argument (Qarrayp, keys);

  Fdefine_key (current_global_map, keys, function);
  return Qnil;
}

DEFUN ("local-set-key", Flocal_set_key, Slocal_set_key, 2, 2,
  "kSet key locally: \nCSet key %s locally to command: ",
  "Give KEY a local binding as COMMAND.\n\
COMMAND is a symbol naming an interactively-callable function.\n\
KEY is a string representing a sequence of keystrokes.\n\
The binding goes in the current buffer's local map,\n\
which is shared with other buffers in the same major mode.")
  (keys, function)
     Lisp_Object keys, function;
{
  register Lisp_Object map;
  map = current_buffer->keymap;
  if (NILP (map))
    {
      map = Fmake_sparse_keymap (Qnil);
      current_buffer->keymap = map;
    }

  if (XTYPE (keys) != Lisp_Vector
      && XTYPE (keys) != Lisp_String)
    keys = wrong_type_argument (Qarrayp, keys);

  Fdefine_key (map, keys, function);
  return Qnil;
}

DEFUN ("global-unset-key", Fglobal_unset_key, Sglobal_unset_key,
  1, 1, "kUnset key globally: ",
  "Remove global binding of KEY.\n\
KEY is a string representing a sequence of keystrokes.")
  (keys)
     Lisp_Object keys;
{
  return Fglobal_set_key (keys, Qnil);
}

DEFUN ("local-unset-key", Flocal_unset_key, Slocal_unset_key, 1, 1,
  "kUnset key locally: ",
  "Remove local binding of KEY.\n\
KEY is a string representing a sequence of keystrokes.")
  (keys)
     Lisp_Object keys;
{
  if (!NILP (current_buffer->keymap))
    Flocal_set_key (keys, Qnil);
  return Qnil;
}

DEFUN ("define-prefix-command", Fdefine_prefix_command, Sdefine_prefix_command, 1, 2, 0,
  "Define COMMAND as a prefix command.  COMMAND should be a symbol.\n\
A new sparse keymap is stored as COMMAND's function definition and its value.\n\
If a second optional argument MAPVAR is given, the map is stored as\n\
its value instead of as COMMAND's value; but COMMAND is still defined\n\
as a function.")
  (name, mapvar)
     Lisp_Object name, mapvar;
{
  Lisp_Object map;
  map = Fmake_sparse_keymap (Qnil);
  Ffset (name, map);
  if (!NILP (mapvar))
    Fset (mapvar, map);
  else
    Fset (name, map);
  return name;
}

DEFUN ("use-global-map", Fuse_global_map, Suse_global_map, 1, 1, 0,
  "Select KEYMAP as the global keymap.")
  (keymap)
     Lisp_Object keymap;
{
  keymap = get_keymap (keymap);
  current_global_map = keymap;
  return Qnil;
}

DEFUN ("use-local-map", Fuse_local_map, Suse_local_map, 1, 1, 0,
  "Select KEYMAP as the local keymap.\n\
If KEYMAP is nil, that means no local keymap.")
  (keymap)
     Lisp_Object keymap;
{
  if (!NILP (keymap))
    keymap = get_keymap (keymap);

  current_buffer->keymap = keymap;

  return Qnil;
}

DEFUN ("current-local-map", Fcurrent_local_map, Scurrent_local_map, 0, 0, 0,
  "Return current buffer's local keymap, or nil if it has none.")
  ()
{
  return current_buffer->keymap;
}

DEFUN ("current-global-map", Fcurrent_global_map, Scurrent_global_map, 0, 0, 0,
  "Return the current global keymap.")
  ()
{
  return current_global_map;
}

DEFUN ("current-minor-mode-maps", Fcurrent_minor_mode_maps, Scurrent_minor_mode_maps, 0, 0, 0,
  "Return a list of keymaps for the minor modes of the current buffer.")
  ()
{
  Lisp_Object *maps;
  int nmaps = current_minor_maps (0, &maps);

  return Flist (nmaps, maps);
}

/* Help functions for describing and documenting keymaps.		*/

DEFUN ("accessible-keymaps", Faccessible_keymaps, Saccessible_keymaps,
  1, 1, 0,
  "Find all keymaps accessible via prefix characters from KEYMAP.\n\
Returns a list of elements of the form (KEYS . MAP), where the sequence\n\
KEYS starting from KEYMAP gets you to MAP.  These elements are ordered\n\
so that the KEYS increase in length.  The first element is (\"\" . KEYMAP).")
  (startmap)
     Lisp_Object startmap;
{
  Lisp_Object maps, tail;

  maps = Fcons (Fcons (Fmake_vector (make_number (0), Qnil),
		       get_keymap (startmap)),
		Qnil);

  /* For each map in the list maps,
     look at any other maps it points to,
     and stick them at the end if they are not already in the list.

     This is a breadth-first traversal, where tail is the queue of
     nodes, and maps accumulates a list of all nodes visited.  */

  for (tail = maps; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      register Lisp_Object thisseq = Fcar (Fcar (tail));
      register Lisp_Object thismap = Fcdr (Fcar (tail));
      Lisp_Object last = make_number (XINT (Flength (thisseq)) - 1);

      /* Does the current sequence end in the meta-prefix-char?  */
      int is_metized = (XINT (last) >= 0
			&& EQ (Faref (thisseq, last), meta_prefix_char));

      for (; CONSP (thismap); thismap = XCONS (thismap)->cdr)
	{
	  Lisp_Object elt = XCONS (thismap)->car;

	  QUIT;

	  if (XTYPE (elt) == Lisp_Vector)
	    {
	      register int i;

	      /* Vector keymap.  Scan all the elements.  */
	      for (i = 0; i < XVECTOR (elt)->size; i++)
		{
		  register Lisp_Object tem;
		  register Lisp_Object cmd;

		  cmd = get_keyelt (XVECTOR (elt)->contents[i]);
		  if (NILP (cmd)) continue;
		  tem = Fkeymapp (cmd);
		  if (!NILP (tem))
		    {
		      cmd = get_keymap (cmd);
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
			      tem = Fcopy_sequence (thisseq);
			      
			      Faset (tem, last, make_number (i | meta_bit));
			      
			      /* This new sequence is the same length as
				 thisseq, so stick it in the list right
				 after this one.  */
			      XCONS (tail)->cdr
				= Fcons (Fcons (tem, cmd), XCONS (tail)->cdr);
			    }
			  else
			    {
			      tem = append_key (thisseq, make_number (i));
			      nconc2 (tail, Fcons (Fcons (tem, cmd), Qnil));
			    }
			}
		    }
		}
	    }	    
	  else if (CONSP (elt))
	    {
	      register Lisp_Object cmd = get_keyelt (XCONS (elt)->cdr);
	      register Lisp_Object tem;

	      /* Ignore definitions that aren't keymaps themselves.  */
	      tem = Fkeymapp (cmd);
	      if (!NILP (tem))
		{
		  /* Ignore keymaps that have been seen already.  */
		  cmd = get_keymap (cmd);
		  tem = Frassq (cmd, maps);
		  if (NILP (tem))
		    {
		      /* let elt be the event defined by this map entry.  */
		      elt = XCONS (elt)->car;

		      /* If the last key in thisseq is meta-prefix-char, and
			 this entry is a binding for an ascii keystroke,
			 turn it into a meta-ized keystroke.  */
		      if (is_metized && XTYPE (elt) == Lisp_Int)
			{
			  tem = Fcopy_sequence (thisseq);
			  Faset (tem, last,
				 make_number (XINT (elt) | meta_modifier));

			  /* This new sequence is the same length as
			     thisseq, so stick it in the list right
			     after this one.  */
			  XCONS (tail)->cdr =
			    Fcons (Fcons (tem, cmd), XCONS (tail)->cdr);
			}
		      else
			nconc2 (tail,
				Fcons (Fcons (append_key (thisseq, elt), cmd),
				       Qnil));
		    }
		}
	    }
	}
    }

  return maps;
}

Lisp_Object Qsingle_key_description, Qkey_description;

DEFUN ("key-description", Fkey_description, Skey_description, 1, 1, 0,
  "Return a pretty description of key-sequence KEYS.\n\
Control characters turn into \"C-foo\" sequences, meta into \"M-foo\"\n\
spaces are put between sequence elements, etc.")
  (keys)
     Lisp_Object keys;
{
  if (XTYPE (keys) == Lisp_String)
    {
      Lisp_Object vector;
      int i;
      vector = Fmake_vector (Flength (keys), Qnil);
      for (i = 0; i < XSTRING (keys)->size; i++)
	{
	  if (XSTRING (keys)->data[i] & 0x80)
	    XFASTINT (XVECTOR (vector)->contents[i])
	      = meta_modifier | (XSTRING (keys)->data[i] & ~0x80);
	  else
	    XFASTINT (XVECTOR (vector)->contents[i])
	      = XSTRING (keys)->data[i];
	}
      keys = vector;
    }
  return Fmapconcat (Qsingle_key_description, keys, build_string (" "));
}

char *
push_key_description (c, p)
     register unsigned int c;
     register char *p;
{
  /* Clear all the meaningless bits above the meta bit.  */
  c &= meta_modifier | ~ - meta_modifier;

  if (c & alt_modifier)
    {
      *p++ = 'A';
      *p++ = '-';
      c -= alt_modifier;
    }
  if (c & ctrl_modifier)
    {
      *p++ = 'C';
      *p++ = '-';
      c -= ctrl_modifier;
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
      else if (c == Ctl('J'))
	{
	  *p++ = 'L';
	  *p++ = 'F';
	  *p++ = 'D';
	}
      else if (c == Ctl('M'))
	{
	  *p++ = 'R';
	  *p++ = 'E';
	  *p++ = 'T';
	}
      else
	{
	  *p++ = 'C';
	  *p++ = '-';
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
  else if (c < 256)
    *p++ = c;
  else
    {
      *p++ = '\\';
      *p++ = (7 & (c >> 15)) + '0';
      *p++ = (7 & (c >> 12)) + '0';
      *p++ = (7 & (c >> 9)) + '0';
      *p++ = (7 & (c >> 6)) + '0';
      *p++ = (7 & (c >> 3)) + '0';
      *p++ = (7 & (c >> 0)) + '0';
    }

  return p;  
}

DEFUN ("single-key-description", Fsingle_key_description, Ssingle_key_description, 1, 1, 0,
  "Return a pretty description of command character KEY.\n\
Control characters turn into C-whatever, etc.")
  (key)
     Lisp_Object key;
{
  char tem[20];

  key = EVENT_HEAD (key);

  switch (XTYPE (key))
    {
    case Lisp_Int:		/* Normal character */
      *push_key_description (XUINT (key), tem) = 0;
      return build_string (tem);

    case Lisp_Symbol:		/* Function key or event-symbol */
      return Fsymbol_name (key);

    default:
      error ("KEY must be an integer, cons, or symbol.");
    }
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

DEFUN ("text-char-description", Ftext_char_description, Stext_char_description, 1, 1, 0,
  "Return a pretty description of file-character CHAR.\n\
Control characters turn into \"^char\", etc.")
  (chr)
     Lisp_Object chr;
{
  char tem[6];

  CHECK_NUMBER (chr, 0);

  *push_text_char_description (XINT (chr) & 0377, tem) = 0;

  return build_string (tem);
}

/* Return non-zero if SEQ contains only ASCII characters, perhaps with
   a meta bit.  */
static int
ascii_sequence_p (seq)
     Lisp_Object seq;
{
  Lisp_Object i;
  int len = XINT (Flength (seq));
  
  for (XFASTINT (i) = 0; XFASTINT (i) < len; XFASTINT (i)++)
    {
      Lisp_Object elt = Faref (seq, i);

      if (XTYPE (elt) != Lisp_Int
	  || (XUINT (elt) & ~CHAR_META) >= 0x80)
	return 0;
    }

  return 1;
}


/* where-is - finding a command in a set of keymaps.			*/

DEFUN ("where-is-internal", Fwhere_is_internal, Swhere_is_internal, 1, 5, 0,
  "Return list of keys that invoke DEFINITION in KEYMAP or KEYMAP1.\n\
If KEYMAP is nil, search only KEYMAP1.\n\
If KEYMAP1 is nil, use the current global map.\n\
\n\
If optional 4th arg FIRSTONLY is non-nil, return a string representing\n\
the first key sequence found, rather than a list of all possible key\n\
sequences.  If FIRSTONLY is t, avoid key sequences which use non-ASCII\n\
keys and therefore may not be usable on ASCII terminals.  If FIRSTONLY\n\
is the symbol `non-ascii', return the first binding found, no matter\n\
what its components.\n\
\n\
If optional 5th arg NOINDIRECT is non-nil, don't follow indirections\n\
to other keymaps or slots.  This makes it possible to search for an\n\
indirect definition itself.")
  (definition, local_keymap, global_keymap, firstonly, noindirect)
     Lisp_Object definition, local_keymap, global_keymap;
     Lisp_Object firstonly, noindirect;
{
  register Lisp_Object maps;
  Lisp_Object found;

  if (NILP (global_keymap))
    global_keymap = current_global_map;

  if (!NILP (local_keymap))
    maps = nconc2 (Faccessible_keymaps (get_keymap (local_keymap)),
		   Faccessible_keymaps (get_keymap (global_keymap)));
  else
    maps = Faccessible_keymaps (get_keymap (global_keymap));

  found = Qnil;

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      /* Key sequence to reach map */
      register Lisp_Object this = Fcar (Fcar (maps));

      /* The map that it reaches */
      register Lisp_Object map  = Fcdr (Fcar (maps));

      /* If Fcar (map) is a VECTOR, the current element within that vector.  */
      int i = 0;

      /* In order to fold [META-PREFIX-CHAR CHAR] sequences into
	 [M-CHAR] sequences, check if last character of the sequence
	 is the meta-prefix char.  */
      Lisp_Object last = make_number (XINT (Flength (this)) - 1);
      int last_is_meta = (XINT (last) >= 0
			  && EQ (Faref (this, last), meta_prefix_char));

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
	  
	  Lisp_Object elt = XCONS (map)->car;
	  Lisp_Object key, binding, sequence;

	  QUIT;

	  /* Set key and binding to the current key and binding, and
	     advance map and i to the next binding.  */
	  if (XTYPE (elt) == Lisp_Vector)
	    {
	      /* In a vector, look at each element.  */
	      binding = XVECTOR (elt)->contents[i];
	      XFASTINT (key) = i;
	      i++;

	      /* If we've just finished scanning a vector, advance map
		 to the next element, and reset i in anticipation of the
		 next vector we may find.  */
	      if (i >= XVECTOR (elt)->size)
		{
		  map = XCONS (map)->cdr;
		  i = 0;
		}
	    }
	  else if (CONSP (elt))
	    {
	      key = Fcar (Fcar (map));
	      binding = Fcdr (Fcar (map));

	      map = XCONS (map)->cdr;
	    }
	  else
	    /* We want to ignore keymap elements that are neither
	       vectors nor conses.  */
	    {
	      map = XCONS (map)->cdr;
	      continue;
	    }

	  /* Search through indirections unless that's not wanted.  */
	  if (NILP (noindirect))
	    binding = get_keyelt (binding);

	  /* End this iteration if this element does not match
	     the target.  */

	  if (XTYPE (definition) == Lisp_Cons)
	    {
	      Lisp_Object tem;
	      tem = Fequal (binding, definition);
	      if (NILP (tem))
		continue;
	    }
	  else
	    if (!EQ (binding, definition))
	      continue;

	  /* We have found a match.
	     Construct the key sequence where we found it.  */
	  if (XTYPE (key) == Lisp_Int && last_is_meta)
	    {
	      sequence = Fcopy_sequence (this);
	      Faset (sequence, last, make_number (XINT (key) | meta_modifier));
	    }
	  else
	    sequence = append_key (this, key);

	  /* Verify that this key binding is not shadowed by another
	     binding for the same key, before we say it exists.

	     Mechanism: look for local definition of this key and if
	     it is defined and does not match what we found then
	     ignore this key.

	     Either nil or number as value from Flookup_key
	     means undefined.  */
	  if (!NILP (local_keymap))
	    {
	      binding = Flookup_key (local_keymap, sequence, Qnil);
	      if (!NILP (binding) && XTYPE (binding) != Lisp_Int)
		{
		  if (XTYPE (definition) == Lisp_Cons)
		    {
		      Lisp_Object tem;
		      tem = Fequal (binding, definition);
		      if (NILP (tem))
			continue;
		    }
		  else
		    if (!EQ (binding, definition))
		      continue;
		}
	    }

	  /* It is a true unshadowed match.  Record it.  */
	  found = Fcons (sequence, found);

	  /* If firstonly is Qnon_ascii, then we can return the first
	     binding we find.  If firstonly is not Qnon_ascii but not
	     nil, then we should return the first ascii-only binding
	     we find.  */
	  if (EQ (firstonly, Qnon_ascii))
	    return sequence;
	  else if (! NILP (firstonly) && ascii_sequence_p (sequence))
	    return sequence;
	}
    }

  found = Fnreverse (found);

  /* firstonly may have been t, but we may have gone all the way through
     the keymaps without finding an all-ASCII key sequence.  So just
     return the best we could find.  */
  if (! NILP (firstonly))
    return Fcar (found);
    
  return found;
}

/* Return a string listing the keys and buttons that run DEFINITION.  */

static Lisp_Object
where_is_string (definition)
     Lisp_Object definition;
{
  register Lisp_Object keys, keys1;

  keys = Fwhere_is_internal (definition,
			     current_buffer->keymap, Qnil, Qnil, Qnil);
  keys1 = Fmapconcat (Qkey_description, keys, build_string (", "));

  return keys1;
}

DEFUN ("where-is", Fwhere_is, Swhere_is, 1, 1, "CWhere is command: ",
  "Print message listing key sequences that invoke specified command.\n\
Argument is a command definition, usually a symbol with a function definition.")
  (definition)
     Lisp_Object definition;
{
  register Lisp_Object string;

  CHECK_SYMBOL (definition, 0);
  string = where_is_string (definition);
 
  if (XSTRING (string)->size)
    message ("%s is on %s", XSYMBOL (definition)->name->data,
	     XSTRING (string)->data);
  else
    message ("%s is not on any key", XSYMBOL (definition)->name->data);
  return Qnil;
}

/* describe-bindings - summarizing all the bindings in a set of keymaps.  */

DEFUN ("describe-bindings", Fdescribe_bindings, Sdescribe_bindings, 0, 0, "",
  "Show a list of all defined keys, and their definitions.\n\
The list is put in a buffer, which is displayed.")
  ()
{
  register Lisp_Object thisbuf;
  XSET (thisbuf, Lisp_Buffer, current_buffer);
  internal_with_output_to_temp_buffer ("*Help*",
				       describe_buffer_bindings,
				       thisbuf);
  return Qnil;
}

static Lisp_Object
describe_buffer_bindings (descbuf)
     Lisp_Object descbuf;
{
  register Lisp_Object start1, start2;

  char *key_heading
    = "\
key             binding\n\
---             -------\n";
  char *alternate_heading
    = "\
Alternate Characters (use anywhere the nominal character is listed):\n\
nominal         alternate\n\
-------         ---------\n";

  Fset_buffer (Vstandard_output);

  /* Report on alternates for keys.  */
  if (XTYPE (Vkeyboard_translate_table) == Lisp_String)
    {
      int c;
      unsigned char *translate = XSTRING (Vkeyboard_translate_table)->data;
      int translate_len = XSTRING (Vkeyboard_translate_table)->size;

      for (c = 0; c < translate_len; c++)
	if (translate[c] != c)
	  {
	    char buf[20];
	    char *bufend;

	    if (alternate_heading)
	      {
		insert_string (alternate_heading);
		alternate_heading = 0;
	      }

	    bufend = push_key_description (translate[c], buf);
	    insert (buf, bufend - buf);
	    Findent_to (make_number (16), make_number (1));
	    bufend = push_key_description (c, buf);
	    insert (buf, bufend - buf);

	    insert ("\n", 1);
	  }

      insert ("\n", 1);
    }

  {
    int i, nmaps;
    Lisp_Object *modes, *maps;

    /* Temporarily switch to descbuf, so that we can get that buffer's
       minor modes correctly.  */
    Fset_buffer (descbuf);
    nmaps = current_minor_maps (&modes, &maps);
    Fset_buffer (Vstandard_output);

    for (i = 0; i < nmaps; i++)
      {
	if (XTYPE (modes[i]) == Lisp_Symbol)
	  {
	    insert_char ('`');
	    insert_string (XSYMBOL (modes[i])->name->data);
	    insert_char ('\'');
	  }
	else
	  insert_string ("Strangely Named");
	insert_string (" Minor Mode Bindings:\n");
	insert_string (key_heading);
	describe_map_tree (maps[i], 0, Qnil);
	insert_char ('\n');
      }
  }

  start1 = XBUFFER (descbuf)->keymap;
  if (!NILP (start1))
    {
      insert_string ("Local Bindings:\n");
      insert_string (key_heading);
      describe_map_tree (start1, 0, Qnil);
      insert_string ("\n");
    }

  insert_string ("Global Bindings:\n");
  if (NILP (start1))
    insert_string (key_heading);

  describe_map_tree (current_global_map, 0, XBUFFER (descbuf)->keymap);

  Fset_buffer (descbuf);
  return Qnil;
}

/* Insert a desription of the key bindings in STARTMAP,
    followed by those of all maps reachable through STARTMAP.
   If PARTIAL is nonzero, omit certain "uninteresting" commands
    (such as `undefined').
   If SHADOW is non-nil, it is another map;
    don't mention keys which would be shadowed by it.  */

void
describe_map_tree (startmap, partial, shadow)
     Lisp_Object startmap, shadow;
     int partial;
{
  register Lisp_Object elt, sh;
  Lisp_Object maps;
  struct gcpro gcpro1;

  maps = Faccessible_keymaps (startmap);
  GCPRO1 (maps);

  for (; !NILP (maps); maps = Fcdr (maps))
    {
      elt = Fcar (maps);
      sh = Fcar (elt);

      /* If there is no shadow keymap given, don't shadow.  */
      if (NILP (shadow))
	sh = Qnil;

      /* If the sequence by which we reach this keymap is zero-length,
	 then the shadow map for this keymap is just SHADOW.  */
      else if ((XTYPE (sh) == Lisp_String
		&& XSTRING (sh)->size == 0)
	       || (XTYPE (sh) == Lisp_Vector
		   && XVECTOR (sh)->size == 0))
	sh = shadow;

      /* If the sequence by which we reach this keymap actually has
	 some elements, then the sequence's definition in SHADOW is
	 what we should use.  */
      else
	{
	  sh = Flookup_key (shadow, Fcar (elt), Qt);
	  if (XTYPE (sh) == Lisp_Int)
	    sh = Qnil;
	}

      /* If sh is null (meaning that the current map is not shadowed),
	 or a keymap (meaning that bindings from the current map might
	 show through), describe the map.  Otherwise, sh is a command
	 that completely shadows the current map, and we shouldn't
	 bother.  */
      if (NILP (sh) || !NILP (Fkeymapp (sh)))
	describe_map (Fcdr (elt), Fcar (elt), partial, sh);
    }

  UNGCPRO;
}

static void
describe_command (definition)
     Lisp_Object definition;
{
  register Lisp_Object tem1;

  Findent_to (make_number (16), make_number (1));

  if (XTYPE (definition) == Lisp_Symbol)
    {
      XSET (tem1, Lisp_String, XSYMBOL (definition)->name);
      insert1 (tem1);
      insert_string ("\n");
    }
  else
    {
      tem1 = Fkeymapp (definition);
      if (!NILP (tem1))
	insert_string ("Prefix Command\n");
      else
	insert_string ("??\n");
    }
}

/* Describe the contents of map MAP, assuming that this map itself is
   reached by the sequence of prefix keys KEYS (a string or vector).
   PARTIAL, SHADOW is as in `describe_map_tree' above.  */

static void
describe_map (map, keys, partial, shadow)
     Lisp_Object map, keys;
     int partial;
     Lisp_Object shadow;
{
  register Lisp_Object keysdesc;

  if (!NILP (keys) && XFASTINT (Flength (keys)) > 0)
    {
      Lisp_Object tem;
      /* Call Fkey_description first, to avoid GC bug for the other string.  */
      tem = Fkey_description (keys);
      keysdesc = concat2 (tem, build_string (" "));
    }
  else
    keysdesc = Qnil;

  describe_map_2 (map, keysdesc, describe_command, partial, shadow);
}

/* Insert a description of KEYMAP into the current buffer.  */

static void
describe_map_2 (keymap, elt_prefix, elt_describer, partial, shadow)
     register Lisp_Object keymap;
     Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
     Lisp_Object shadow;
{
  Lisp_Object this;
  Lisp_Object tem1, tem2 = Qnil;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (partial)
    suppress = intern ("suppress-keymap");

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per keymap element, we don't want to cons up a
     fresh vector every time.  */
  kludge = Fmake_vector (make_number (1), Qnil);

  GCPRO3 (elt_prefix, tem2, kludge);

  for (; CONSP (keymap); keymap = Fcdr (keymap))
    {
      QUIT;

      if (XTYPE (XCONS (keymap)->car) == Lisp_Vector)
	describe_vector (XCONS (keymap)->car,
			 elt_prefix, elt_describer, partial, shadow);
      else
	{
	  tem1 =             Fcar_safe (Fcar (keymap));
	  tem2 = get_keyelt (Fcdr_safe (Fcar (keymap)));

	  /* Don't show undefined commands or suppressed commands.  */
	  if (NILP (tem2)) continue;
	  if (XTYPE (tem2) == Lisp_Symbol && partial)
	    {
	      this = Fget (tem2, suppress);
	      if (!NILP (this))
		continue;
	    }

	  /* Don't show a command that isn't really visible
	     because a local definition of the same key shadows it.  */

	  if (!NILP (shadow))
	    {
	      Lisp_Object tem;

	      XVECTOR (kludge)->contents[0] = tem1;
	      tem = Flookup_key (shadow, kludge, Qt);
	      if (!NILP (tem)) continue;
	    }

	  if (first)
	    {
	      insert ("\n", 1);
	      first = 0;
	    }

	  if (!NILP (elt_prefix))
	    insert1 (elt_prefix);

	  /* THIS gets the string to describe the character TEM1.  */
	  this = Fsingle_key_description (tem1);
	  insert1 (this);

	  /* Print a description of the definition of this character.
	     elt_describer will take care of spacing out far enough
	     for alignment purposes.  */
	  (*elt_describer) (tem2);
	}
    }

  UNGCPRO;
}

static int
describe_vector_princ (elt)
     Lisp_Object elt;
{
  Fprinc (elt, Qnil);
}

DEFUN ("describe-vector", Fdescribe_vector, Sdescribe_vector, 1, 1, 0,
  "Print on `standard-output' a description of contents of VECTOR.\n\
This is text showing the elements of vector matched against indices.")
  (vector)
     Lisp_Object vector;
{
  CHECK_VECTOR (vector, 0);
  describe_vector (vector, Qnil, describe_vector_princ, 0, Qnil);
}

describe_vector (vector, elt_prefix, elt_describer, partial, shadow)
     register Lisp_Object vector;
     Lisp_Object elt_prefix;
     int (*elt_describer) ();
     int partial;
     Lisp_Object shadow;
{
  Lisp_Object this;
  Lisp_Object dummy;
  Lisp_Object tem1, tem2;
  register int i;
  Lisp_Object suppress;
  Lisp_Object kludge;
  int first = 1;
  struct gcpro gcpro1, gcpro2, gcpro3;

  tem1 = Qnil;

  /* This vector gets used to present single keys to Flookup_key.  Since
     that is done once per vector element, we don't want to cons up a
     fresh vector every time.  */
  kludge = Fmake_vector (make_number (1), Qnil);
  GCPRO3 (elt_prefix, tem1, kludge);

  if (partial)
    suppress = intern ("suppress-keymap");

  for (i = 0; i < XVECTOR (vector)->size; i++)
    {
      QUIT;
      tem1 = get_keyelt (XVECTOR (vector)->contents[i]);

      if (NILP (tem1)) continue;      

      /* Don't mention suppressed commands.  */
      if (XTYPE (tem1) == Lisp_Symbol && partial)
	{
	  this = Fget (tem1, suppress);
	  if (!NILP (this))
	    continue;
	}

      /* If this command in this map is shadowed by some other map,
	 ignore it.  */
      if (!NILP (shadow))
	{
	  Lisp_Object tem;
	  
	  XVECTOR (kludge)->contents[0] = make_number (i);
	  tem = Flookup_key (shadow, kludge, Qt);

	  if (!NILP (tem)) continue;
	}

      if (first)
	{
	  insert ("\n", 1);
	  first = 0;
	}

      /* Output the prefix that applies to every entry in this map.  */
      if (!NILP (elt_prefix))
	insert1 (elt_prefix);

      /* Get the string to describe the character I, and print it.  */
      XFASTINT (dummy) = i;

      /* THIS gets the string to describe the character DUMMY.  */
      this = Fsingle_key_description (dummy);
      insert1 (this);

      /* Find all consecutive characters that have the same definition.  */
      while (i + 1 < XVECTOR (vector)->size
	     && (tem2 = get_keyelt (XVECTOR (vector)->contents[i+1]),
		 EQ (tem2, tem1)))
	i++;

      /* If we have a range of more than one character,
	 print where the range reaches to.  */

      if (i != XINT (dummy))
	{
	  insert (" .. ", 4);
	  if (!NILP (elt_prefix))
	    insert1 (elt_prefix);

	  XFASTINT (dummy) = i;
	  insert1 (Fsingle_key_description (dummy));
	}

      /* Print a description of the definition of this character.
	 elt_describer will take care of spacing out far enough
	 for alignment purposes.  */
      (*elt_describer) (tem1);
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
  "Show all symbols whose names contain match for REGEXP.\n\
If optional 2nd arg PRED is non-nil, (funcall PRED SYM) is done\n\
for each symbol and a symbol is mentioned only if that returns non-nil.\n\
Return list of symbols found.")
  (string, pred)
     Lisp_Object string, pred;
{
  struct gcpro gcpro1, gcpro2;
  CHECK_STRING (string, 0);
  apropos_predicate = pred;
  GCPRO2 (apropos_predicate, apropos_accumulate);
  apropos_accumulate = Qnil;
  map_obarray (Vobarray, apropos_accum, string);
  apropos_accumulate = Fsort (apropos_accumulate, Qstring_lessp);
  UNGCPRO;
  return apropos_accumulate;
}

syms_of_keymap ()
{
  Lisp_Object tem;

  Qkeymap = intern ("keymap");
  staticpro (&Qkeymap);

/* Initialize the keymaps standardly used.
   Each one is the value of a Lisp variable, and is also
   pointed to by a C variable */

  global_map = Fmake_keymap (Qnil);
  Fset (intern ("global-map"), global_map);

  meta_map = Fmake_keymap (Qnil);
  Fset (intern ("esc-map"), meta_map);
  Ffset (intern ("ESC-prefix"), meta_map);

  control_x_map = Fmake_keymap (Qnil);
  Fset (intern ("ctl-x-map"), control_x_map);
  Ffset (intern ("Control-X-prefix"), control_x_map);

  DEFVAR_LISP ("minibuffer-local-map", &Vminibuffer_local_map,
    "Default keymap to use when reading from the minibuffer.");
  Vminibuffer_local_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("minibuffer-local-ns-map", &Vminibuffer_local_ns_map,
    "Local keymap for the minibuffer when spaces are not allowed.");
  Vminibuffer_local_ns_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("minibuffer-local-completion-map", &Vminibuffer_local_completion_map,
    "Local keymap for minibuffer input with completion.");
  Vminibuffer_local_completion_map = Fmake_sparse_keymap (Qnil);

  DEFVAR_LISP ("minibuffer-local-must-match-map", &Vminibuffer_local_must_match_map,
    "Local keymap for minibuffer input with completion, for exact match.");
  Vminibuffer_local_must_match_map = Fmake_sparse_keymap (Qnil);

  current_global_map = global_map;

  DEFVAR_LISP ("minor-mode-map-alist", &Vminor_mode_map_alist,
    "Alist of keymaps to use for minor modes.\n\
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read\n\
key sequences and look up bindings iff VARIABLE's value is non-nil.\n\
If two active keymaps bind the same key, the keymap appearing earlier\n\
in the list takes precedence.");
  Vminor_mode_map_alist = Qnil;

  DEFVAR_LISP ("function-key-map", &Vfunction_key_map,
  "Keymap mapping ASCII function key sequences onto their preferred forms.\n\
This allows Emacs to recognize function keys sent from ASCII\n\
terminals at any point in a key sequence.\n\
\n\
The read-key-sequence function replaces subsequences bound by\n\
function-key-map with their bindings.  When the current local and global\n\
keymaps have no binding for the current key sequence but\n\
function-key-map binds a suffix of the sequence to a vector,\n\
read-key-sequence replaces the matching suffix with its binding, and\n\
continues with the new sequence.\n\
\n\
For example, suppose function-key-map binds `ESC O P' to [pf1].\n\
Typing `ESC O P' to read-key-sequence would return [pf1].  Typing\n\
`C-x ESC O P' would return [?\C-x pf1].  If [pf1] were a prefix\n\
key, typing `ESC O P x' would return [pf1 x].");
  Vfunction_key_map = Fmake_sparse_keymap (Qnil);

  Qsingle_key_description = intern ("single-key-description");
  staticpro (&Qsingle_key_description);

  Qkey_description = intern ("key-description");
  staticpro (&Qkey_description);

  Qkeymapp = intern ("keymapp");
  staticpro (&Qkeymapp);

  Qnon_ascii = intern ("non-ascii");
  staticpro (&Qnon_ascii);

  defsubr (&Skeymapp);
  defsubr (&Smake_keymap);
  defsubr (&Smake_sparse_keymap);
  defsubr (&Scopy_keymap);
  defsubr (&Skey_binding);
  defsubr (&Slocal_key_binding);
  defsubr (&Sglobal_key_binding);
  defsubr (&Sminor_mode_key_binding);
  defsubr (&Sglobal_set_key);
  defsubr (&Slocal_set_key);
  defsubr (&Sdefine_key);
  defsubr (&Slookup_key);
  defsubr (&Sglobal_unset_key);
  defsubr (&Slocal_unset_key);
  defsubr (&Sdefine_prefix_command);
  defsubr (&Suse_global_map);
  defsubr (&Suse_local_map);
  defsubr (&Scurrent_local_map);
  defsubr (&Scurrent_global_map);
  defsubr (&Scurrent_minor_mode_maps);
  defsubr (&Saccessible_keymaps);
  defsubr (&Skey_description);
  defsubr (&Sdescribe_vector);
  defsubr (&Ssingle_key_description);
  defsubr (&Stext_char_description);
  defsubr (&Swhere_is_internal);
  defsubr (&Swhere_is);
  defsubr (&Sdescribe_bindings);
  defsubr (&Sapropos_internal);
}

keys_of_keymap ()
{
  Lisp_Object tem;

  initial_define_key (global_map, 033, "ESC-prefix");
  initial_define_key (global_map, Ctl('X'), "Control-X-prefix");
}
