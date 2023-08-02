/* Common Lisp style packages.
   Copyright (C) 2022 Free Software Foundation, Inc.

Author: Gerd Möllmann <gerd@gnu.org>

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

/* Common Lisp style packages.

   Useful features that could be added:
   package locks
   hierarchical packages
   package-local nicknames  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"

/***********************************************************************
			    Useless tools
 ***********************************************************************/

/* Signal an error with arguments like printf.  */

void
pkg_error (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  verror (fmt, ap);
}

/* Iterator for hash tables.  */

struct h_iter
{
  /* Hash table being iterated over.  */
  struct Lisp_Hash_Table *h;

  /* Current index in key/value vector of H.  */
  ptrdiff_t i;

  /* Key and value at I, or nil.  */
  Lisp_Object key, value;
};

/* Return a freshly initialized iterator for iterating over hash table
   TABLE.  */

static struct h_iter
h_init (Lisp_Object table)
{
  struct Lisp_Hash_Table *h = check_hash_table (table);
  struct h_iter it = {.h = h, .i = 0, .key = Qnil, .value = Qnil};
  return it;
}

/* Value is true if iterator IT is on a valid poisition.  If it is,
   IT->key and IT->value are set to key and value at that
   position.  */

static bool
h_valid (struct h_iter *it)
{
  for (; it->i < HASH_TABLE_SIZE (it->h); ++it->i)
    if (!EQ (HASH_KEY (it->h, it->i), Qunbound))
      {
	it->key = HASH_KEY (it->h, it->i);
	it->value = HASH_VALUE (it->h, it->i);
	return true;
      }
  return false;
}

/* Advance to next element.  */

static void
h_next (struct h_iter *it)
{
  ++it->i;
}

/* Macrology.  IT is a variable name that is bound to an iterator over
   hash table TABLE for the duration of the loop.  */

#define FOR_EACH_KEY_VALUE(it, table) \
  for (struct h_iter it = h_init (table); h_valid (&it); h_next (&it))

/* Sometimes useful for setting a breakpoint, after inserting it
   somewhere in the code.  */

void
pkg_break (void)
{
}


/***********************************************************************
			   Package registry
 ***********************************************************************/

/* Create and return a new Lisp package object for a package with name
   NAME, a string.  NSYMBOLS is the sieo of the symbol-table to
   allocate.  */

static Lisp_Object
pkg_make_package (Lisp_Object name, Lisp_Object nsymbols)
{
  struct Lisp_Package *pkg
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Package, symbols,
				    PVEC_PACKAGE);
  pkg->name = name;
  pkg->symbols = CALLN (Fmake_hash_table, QCtest, Qstring_equal,
			QCsize, nsymbols);
  Lisp_Object package;
  XSETPACKAGE (package, pkg);
  return package;
}

/* Find a package named NAME in the package registry.  Value is the
   package found, or nil if nothing was found.  */

Lisp_Object
pkg_find_package (Lisp_Object name)
{
  CHECK_STRING (name);
  return Fgethash (name, Vpackage_registry, Qnil);
}

/* Register package PACKAGE in the package registry, that is, make it
   known under its name and all its nicknames.  */

static void
pkg_register_package (Lisp_Object package)
{
  const struct Lisp_Package *pkg = XPACKAGE (package);
  Fputhash (pkg->name, package, Vpackage_registry);
  Lisp_Object tail = pkg->nicknames;
  FOR_EACH_TAIL (tail)
    Fputhash (XCAR (tail), package, Vpackage_registry);
}


/***********************************************************************
		    String and package designators
 ***********************************************************************/

/* Return a string for DESIGNATOR.  If DESIGNATOR is a symbol, return
   the symbol's name.  If DESIGNATOR is a string, return that string.
   If DESIGNATOR is a character, return a string that contains only
   that character.  If it is neither, signal an error.  */

static Lisp_Object
pkg_string_from_designator (Lisp_Object designator)
{
  if (SYMBOLP (designator))
    return SYMBOL_NAME (designator);
  if (STRINGP (designator))
    return designator;
  if (CHARACTERP (designator))
    return Fchar_to_string (designator);
  signal_error ("Not a string designator", designator);
}

/* Value is PACKAGE if it is a package, otherwise signal an
   error.  */

static Lisp_Object
pkg_package_or_lose (Lisp_Object package)
{
  if (PACKAGEP (package))
    return package;
  CHECK_PACKAGE (package);
  return Qnil;
}

/* Return a package for a package designator DESIGNATOR.  If
   DESIGNATOR is a package, return that package.  Otherwise,
   DESIGNATOR must a string designator for a registered package.
   Signal an error in the designator case if the package is not
   registered.  */

static Lisp_Object
pkg_package_from_designator (Lisp_Object designator)
{
  /* Not signaling here if DESIGNATOR is not registered is
     odd, but I think that's what CLHS says.  */
  if (PACKAGEP (designator))
    return designator;
  const Lisp_Object name = pkg_string_from_designator (designator);
  const Lisp_Object package = pkg_find_package (name);
  return pkg_package_or_lose (package);
}

/* Value is the package designated by DESIGNATOR, or the value of
   "*package*" if DESIGNATOR is nil.  */

static Lisp_Object
pkg_package_or_default (Lisp_Object designator)
{
  if (NILP (designator))
    return pkg_package_or_lose (Vearmuffs_package);
  return pkg_package_from_designator (designator);
}


/***********************************************************************
                             Symbol table
 ***********************************************************************/

/* Find a symbol with name NAME in PACKAGE or one of the packages it
   inherits from (use-package).  Value is the symbol found, or
   Qunbound if no symbol is found.  If STATUS is not null, return in
   it the status of the symbol, one of :internal, :external,
   :inhertied, or nil if the symbol was not found.  */

Lisp_Object
pkg_find_symbol (Lisp_Object name, Lisp_Object package, Lisp_Object *status)
{
  eassert (STRINGP (name));
  eassert (PACKAGEP (package));

  struct Lisp_Hash_Table *h = XHASH_TABLE (PACKAGE_SYMBOLS (package));
  ptrdiff_t i = hash_lookup (h, name, NULL);
  if (i >= 0)
    {
      if (status)
	*status = HASH_VALUE (h, i);
      return HASH_KEY (h, i);
    }

  Lisp_Object tail = PACKAGE_USE_LIST (package);
  FOR_EACH_TAIL (tail)
    {
      const Lisp_Object used_package = XCAR (tail);
      h = XHASH_TABLE (PACKAGE_SYMBOLS (used_package));
      i = hash_lookup (h, name, NULL);
      if (i >= 0 && EQ (HASH_VALUE (h, i), QCexternal))
	{
	  if (status)
	    *status = QCinherited;
	  return HASH_KEY (h, i);
	}
    }

  if (status)
    *status = Qnil;
  return Qunbound;
}

/* Add SYMBOL to package PACKAGE.  Value is SYMBOL.  The symbol gets status STATUS
   in PACKAGE (one of :external or :internal).  */

static Lisp_Object
pkg_add_symbol (Lisp_Object symbol, Lisp_Object status, Lisp_Object package)
{
  eassert (SYMBOLP (symbol));
  eassert (SYMBOLP (status));
  eassert (PACKAGEP (package));
  Fputhash (symbol, status, PACKAGE_SYMBOLS (package));
  return symbol;
}

/* Remove SYMBOL from PACKAGE.  */

static void
pkg_remove_symbol (Lisp_Object symbol, Lisp_Object package)
{
  eassert (SYMBOLP (symbol));
  eassert (PACKAGEP (package));
  XPACKAGE (package)->shadowing_symbols
    = Fdelq (symbol, XPACKAGE (package)->shadowing_symbols);
  Fremhash (symbol, PACKAGE_SYMBOLS (package));
}

/* Intern a symbol with name NAME to PACKAGE.  If a symbol with name
   NAME is already accessible in PACKAGE, return that symbol.

   Otherwise, add a new symbol to PACKAGE. If EXISTING_SYMBOL is not
   Qunbound, use that symbol instead of making a new one.  This is
   used for built-in symbols.

   Value is the symbol found or newly inserted.  Return in *STATUS the
   status of the SYMBOL in PACKAGE. */

static Lisp_Object
pkg_intern_symbol1 (const Lisp_Object name, Lisp_Object package,
		    Lisp_Object *status, Lisp_Object existing_symbol)
{
  /* PKG-FIXME this symbol_or_name is shit.  */
  CHECK_STRING (name);
  eassert (PACKAGEP (package));

  /* If already accessible in package, return that.  */
  Lisp_Object symbol = pkg_find_symbol (name, package, status);
  if (!EQ (symbol, Qunbound))
    return symbol;

  /* Not found.  If we have an existing symbol (which should be a
     built-in symbol), use that, otherwise make a new one.  */
  if (!EQ (existing_symbol, Qunbound))
    symbol = existing_symbol;
  else
    symbol = Fmake_symbol (name);

  /* PACKAGE becomes the home package of the symbol created.  */
  XSYMBOL (symbol)->u.s.package = package;

  if (EQ (package, Vkeyword_package))
    {
      if (status)
	*status = QCexternal;
      /* Symbol-value of a keyword is itself, and cannot be set.  */
      XSYMBOL (symbol)->u.s.redirect = SYMBOL_PLAINVAL;
      XSYMBOL (symbol)->u.s.val.value = symbol;
      make_symbol_constant (symbol);
      /* Mark keywords as special.  This makes (let ((:key 'foo)) ...)
	 in lexically bound elisp signal an error, as documented.  */
      XSYMBOL (symbol)->u.s.declared_special = true;
      pkg_add_symbol (symbol, QCexternal, Vkeyword_package);
    }
  else if (EQ (package, Vemacs_package))
    {
      /* Special-case package "emacs" because nothing exists yet
	 exporting symbols from that package.  */
      if (status)
	*status = QCexternal;
      pkg_add_symbol (symbol, QCexternal, package);
    }
  else
    {
      if (status)
	*status = QCinternal;
      pkg_add_symbol (symbol, QCinternal, package);
    }

  return symbol;
}

/* Intern a symbol that is not a built-in symbol.  */

Lisp_Object
pkg_intern_symbol (const Lisp_Object name, Lisp_Object package,
		   Lisp_Object *status)
{
  return pkg_intern_symbol1 (name, package, status, Qunbound);
}

/* Define SYMBOL in package.  This is called from define_symbol for
   built-in symbols.  */

Lisp_Object
pkg_define_symbol (Lisp_Object symbol, Lisp_Object package)
{
  return pkg_intern_symbol1 (SYMBOL_NAME (symbol), package, NULL, symbol);
}

/* Intern NAME, which may or may not have a ':' in its name, that is
   might be intended to be a keyword.  */

Lisp_Object
pkg_intern_maybe_keyword (Lisp_Object name)
{
  CHECK_STRING (name);
  if (SREF (name, 0) == ':')
    {
      name = Fsubstring (name, make_fixnum (1), Qnil);
      return pkg_intern_symbol (name, Vkeyword_package, NULL);
    }
  return pkg_intern_symbol (name, Vearmuffs_package, NULL);
}

/* Find a symbol in *package* that has a name given by PTR, NCHARS,
   and NBYTES.  */

Lisp_Object
pkg_lookup_non_keyword_c_string (const char *ptr, ptrdiff_t nchars, ptrdiff_t nbytes)
{
  eassert (*ptr != ':');
  const Lisp_Object name = make_string_from_bytes (ptr, nchars, nbytes);
  return pkg_find_symbol (name, Vearmuffs_package, NULL);
}

/* Unintern SYMBOL from PACKAGE.  Value is Qt if removed.  */

static Lisp_Object
pkg_unintern_symbol (Lisp_Object symbol, Lisp_Object package)
{
  CHECK_SYMBOL (symbol);
  package = pkg_package_or_default (package);

  Lisp_Object status;
  Lisp_Object found = pkg_find_symbol (SYMBOL_NAME (symbol), package, &status);
  Lisp_Object removedp = Qnil;

  if (!EQ (found, Qunbound) && !EQ (status, QCinherited))
    {
      /* Symbol is present in the package. Remove it from the symbol
	 table and shadowing list.  */
      removedp = Qt;
      pkg_remove_symbol (symbol, package);
    }

  if (EQ (package, SYMBOL_PACKAGE (symbol)))
    XSYMBOL (symbol)->u.s.package = Qnil;

  return removedp;
}

/* Map function FN over symbols in PACKAGE.  */

static void
pkg_map_package_symbols (Lisp_Object fn, Lisp_Object package)
{
  package = pkg_package_or_lose (package);
  FOR_EACH_KEY_VALUE (it, PACKAGE_SYMBOLS (package))
    call1 (fn, it.key);
}

/* Return a list of all registered packages.  */

static Lisp_Object
pkg_list_all_packages (void)
{
  Lisp_Object all = Qnil;
  FOR_EACH_KEY_VALUE (it, Vpackage_registry)
    if (NILP (Fmemq (it.value, all)))
      all = Fcons (it.value, all);
  return all;
}

/* Map FUNCTION over all symbols in PACKAGE.  */

static void
pkg_map_symbols (Lisp_Object function)
{
  Lisp_Object tail = pkg_list_all_packages ();
  FOR_EACH_TAIL (tail)
    pkg_map_package_symbols (function, XCAR (tail));
}

/* Map a C funtion FN over all symbols in all registered packages.
   The function is called with first argument being the symbol, and
   second argument ARG.  */

void
pkg_map_symbols_c_fn (void (*fn) (Lisp_Object, Lisp_Object), Lisp_Object arg)
{
  Lisp_Object tail = pkg_list_all_packages ();
  FOR_EACH_TAIL (tail)
    FOR_EACH_KEY_VALUE (it, PACKAGE_SYMBOLS (XCAR (tail)))
      fn (it.key, arg);
}

/* Value is true if obj is a keyword symbol.  */

bool
pkg_keywordp (Lisp_Object obj)
{
  return SYMBOLP (obj) && EQ (SYMBOL_PACKAGE (obj), Vkeyword_package);
}

static Lisp_Object
pkg_set_status (Lisp_Object symbol, Lisp_Object package, Lisp_Object status)
{
  CHECK_SYMBOL (symbol);
  CHECK_PACKAGE (package);
  if (!EQ (status, QCinternal) && !EQ (status, QCexternal))
    pkg_error ("Invalid symbol status %s", status);

  struct Lisp_Hash_Table *h = XHASH_TABLE (PACKAGE_SYMBOLS (package));
  ptrdiff_t i = hash_lookup (h, SYMBOL_NAME (symbol), NULL);
  eassert (i >= 0);
  ASET (h->key_and_value, 2 * i + 1, status);
  return Qnil;
}



/***********************************************************************
                        Traditional Emacs intern stuff
 ***********************************************************************/

/* The idea behinf this is as follows:

   We want to get rid of Lisp_Symbol::next.  But legcacy code may
   still obarrays.  We accept these in some place (they are just
   vectors, which no indication that they are obarrays).

   When we come across such a vector, create a package and store it in
   its slot 0.  Then we use that package behind the scenes.  */

static Lisp_Object
pkg_fake_me_an_obarray (Lisp_Object vector)
{
  eassert (VECTORP (vector));
  Lisp_Object package = Faref (vector, make_fixnum (0));
  if (!PACKAGEP (package))
    {
      package = pkg_make_package (build_string ("obarray"),
				  Flength (vector));
      Faset (vector, make_fixnum (0), package);
    }
  return package;
}

/* Implements Emacs' traditional Fintern function.  */

Lisp_Object
pkg_emacs_intern (Lisp_Object name, Lisp_Object package, Lisp_Object cl)
{
  CHECK_STRING (name);

  /* PKG-FIXME: We are assuming that this is intended to be a keyword
     like it was before.  */
  if (SREF (name, 0) == ':' && NILP (package))
    {
      name = Fsubstring (name, make_fixnum (1), Qnil);
      package = Vkeyword_package;
    }

  /* The following assertion would be invalid because we might want to
     intern '::' in the keyword package, and a test does that.  */
  //eassert (SREF (name, 0) != ':');

  if (VECTORP (package))
    package = pkg_fake_me_an_obarray (package);
  package = pkg_package_or_default (package);

  return pkg_intern_symbol (name, package, NULL);
}

/* Implements Emacs' traditional Fintern_soft function.  */

Lisp_Object
pkg_emacs_intern_soft (Lisp_Object name, Lisp_Object package, Lisp_Object cl)
{
  /* intern-soft allows symbols.  */
  Lisp_Object orig = name;
  if (SYMBOLP (name))
    name = SYMBOL_NAME (name);
  CHECK_STRING (name);

  /* PKG-FIXME? We are assuming that this is intended to be a keyword
     like it was before.  */
  if (SREF (name, 0) == ':' && NILP (package))
    {
      name = Fsubstring (name, make_fixnum (1), Qnil);
      package = Vkeyword_package;
    }

  if (VECTORP (package))
    package = pkg_fake_me_an_obarray (package);
  package = pkg_package_or_default (package);

  Lisp_Object found = pkg_find_symbol (name, package, NULL);
  if (EQ (found, Qunbound))
    return Qnil;

  if (SYMBOLP (orig) && !EQ (found, orig))
    return Qnil;

  /* We should never find an uninterned symbol in a package.  */
  eassert (!NILP (SYMBOL_PACKAGE (found)));
  return found;
}

/* Implements Emacs' traditional Funintern function.  */

Lisp_Object
pkg_emacs_unintern (Lisp_Object name, Lisp_Object package)
{
  /* unintern allows symbols.  */
  Lisp_Object orig = name;
  if (SYMBOLP (name))
    name = SYMBOL_NAME (name);
  CHECK_STRING (name);

  /* PKG-FIXME? We are assuming that this is intended to be a keyword
     like it was before.  */
  if (SREF (name, 0) == ':' && NILP (package))
    {
      name = Fsubstring (name, make_fixnum (1), Qnil);
      package = Vkeyword_package;
    }

  if (VECTORP (package))
    package = pkg_fake_me_an_obarray (package);
  package = pkg_package_or_default (package);

  Lisp_Object found = pkg_find_symbol (name, package, NULL);
  if (EQ (found, Qunbound))
    return Qnil;

  if (SYMBOLP (orig) && !EQ (found, orig))
    return Qnil;

  /* We should never find an uninterned symbol in a package.  */
  eassert (!NILP (SYMBOL_PACKAGE (found)));
  if (VECTORP (package))
    package = pkg_fake_me_an_obarray (package);
  package = pkg_package_or_default (package);
  return pkg_unintern_symbol (found, package);
}

/* Implements Emacs mapatoms.  */

Lisp_Object
pkg_emacs_mapatoms (Lisp_Object function, Lisp_Object package)
{
  if (VECTORP (package))
    package = pkg_fake_me_an_obarray (package);
  if (NILP (package))
    pkg_map_symbols (function);
  else
    pkg_map_package_symbols (function, package);
  return Qnil;
}


/***********************************************************************
				Reader
 ***********************************************************************/

/* We have read a symbol with NAME, and a package prefix for PACKAGE.
   EXTERNAL means that we have seen ':' and not '::'.  Value is the
   symbol for that case.  */

Lisp_Object
pkg_qualified_symbol (Lisp_Object name, Lisp_Object package, bool external)
{
  /* If we want a symbol for a given package, check the
     package has that symbol and its accessibily.  */
  Lisp_Object status;
  Lisp_Object found = pkg_find_symbol (name, package, &status);

  if (EQ (package, Vkeyword_package))
    {
      /* If found, use that symbol, else make a new one.
	 PKG-FIXME: there might already be a symbol named
	 'test' in the obarray, and we'd like to use that
	 name for ':test'.  That's a problem.  */
      if (EQ (found, Qunbound))
	return pkg_intern_symbol (name, package, NULL);
      return found;
    }

  if (EQ (found, Qunbound))
    {
      if (external)
	pkg_error ("Symbol '%s' is not accessible in package '%s'",
		   SDATA (name), SDATA (PACKAGE_NAMEX (package)));
      /* Access with x::y. intern y into x.  */
      return pkg_intern_symbol (name, package, NULL);
    }

  /* Check if the symbol is accesible in the package as external
     symbol.  PKG-FIXME: Check what to do for inherited symbols.  */
  if (external && EQ (status, QCinternal))
    pkg_error ("Symbol '%s' is internal in package '%s'",
	       SDATA (name), SDATA (PACKAGE_NAMEX (package)));

  return found;
}

/* Return symbol with name NAME when accessed without qualification in
   the current package.  */

Lisp_Object
pkg_unqualified_symbol (Lisp_Object name)
{
  const Lisp_Object package = pkg_package_or_lose (Vearmuffs_package);

  if (EQ (package, Vkeyword_package))
    return pkg_qualified_symbol (name, package, true);

  /* If we want a symbol for a given package, check the
     package has that symboland its accessibily.  */
  Lisp_Object status;
  const Lisp_Object found = pkg_find_symbol (name, package, &status);
  if (!EQ (found, Qunbound))
    return found;
  return pkg_intern_symbol (name, package, NULL);
}


/***********************************************************************
			    Lisp functions
 ***********************************************************************/

DEFUN ("make-%package", Fmake_percent_package, Smake_percent_package,
       2, 2, 0, doc: /**/)
  (Lisp_Object name, Lisp_Object size)
{
  CHECK_STRING (name);
  CHECK_FIXNAT (size);
  return pkg_make_package (name, size);
}

DEFUN ("packagep", Fpackagep, Spackagep, 1, 1, 0, doc:
       /* Value is non-nil if PACKAGE is a package object. */)
  (Lisp_Object package)
{
  return PACKAGEP (package) ? Qt : Qnil;
}

DEFUN ("find-symbol", Ffind_symbol, Sfind_symbol, 1, 2, 0, doc:
       /* Find symbol with name NAME in PACKAGE.
If PACKAGE is omitted, use the current package.

Value is nil if no symbol is found.

Otherwise, value is a list (SYMBOL STATUS), where SYMBOL is the
symbol that was found, and STATUS is one of the following:

`internal' if SYMBOL is present in PACKAGE as an internal symbol.

`external' if SYMBOL is present in PACKAGE as an external symbol.

`inherited' if SYMBOL is inherited via `use-package'. */)
  (Lisp_Object name, Lisp_Object package)
{
  CHECK_STRING (name);
  package = pkg_package_or_default (package);
  Lisp_Object status;
  const Lisp_Object symbol = pkg_find_symbol (name, package, &status);
  if (EQ (symbol, Qunbound))
    return Qnil;
  return list2 (symbol, status);
}

/* PKG-FIXME: Make this somehow compatible with Emacs' intern?  */

DEFUN ("cl-intern", Fcl_intern, Scl_intern, 1, 2, 0, doc:
       /* Enter a symbol with name NAME into PACKAGE.

If PACKAGE is omitted, use the current package.

Value is a list (SYMBOL STATUS).

If a symbol with name NAME is already accessible, SYMBOL is that
symbol, and STATUS is it's status in the package.

Otherwise, a new SYMBOL is created, whose status 'external' if
package is the keyword package, or 'internal' if not.  */)
  (Lisp_Object name, Lisp_Object package)
{
  CHECK_STRING (name);
  package = pkg_package_or_default (package);
  Lisp_Object status;
  const Lisp_Object symbol = pkg_intern_symbol (name, package, &status);
  return list2 (symbol, status);
}

DEFUN ("cl-unintern", Fcl_unintern, Scl_unintern, 1, 2, 0, doc:
       /* tbd */)
  (Lisp_Object symbol, Lisp_Object package)
{
  return pkg_unintern_symbol (symbol, package);
}

DEFUN ("pkg-break", Fpkg_read, Spkg_read, 1, 1, 0,
       doc: /* tbd  */)
  (Lisp_Object stream)
{
  pkg_break ();
  return Qnil;
}


/***********************************************************************
                     Internal access to packages
 ***********************************************************************/

DEFUN ("package-%name", Fpackage_percent_name, Spackage_percent_name, 1, 1, 0,
       doc:  /* Internal use only.  */)
  (Lisp_Object package)
{
  CHECK_PACKAGE (package);
  return PACKAGE_NAMEX (package);
}

DEFUN ("package-%set-name", Fpackage_percent_set_name, Spackage_percent_set_name,
       2, 2, 0, doc: /* Internal use only.  */)
  (Lisp_Object package, Lisp_Object name)
{
  CHECK_PACKAGE (package);
  if (!NILP (name))
    CHECK_STRING (name);
  if (EQ (package, Vemacs_package) || EQ (package, Vkeyword_package))
    error ("Cannot change name of standard package");
  return XPACKAGE (package)->name = name;
}

DEFUN ("package-%nicknames", Fpackage_percent_nicknames,
       Spackage_percent_nicknames, 1, 1, 0, doc:  /* Internal use only.  */)
  (Lisp_Object package)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->nicknames;
}

DEFUN ("package-%set-nicknames", Fpackage_percent_set_nicknames,
       Spackage_percent_set_nicknames, 2, 2, 0, doc: /* Internal use only.  */)
  (Lisp_Object package, Lisp_Object nicknames)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->nicknames = nicknames;
}

DEFUN ("package-%use-list", Fpackage_percent_use_list,
       Spackage_percent_use_list, 1, 1, 0, doc:  /* Internal use only.  */)
  (Lisp_Object package)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->use_list;
}

DEFUN ("package-%set-use-list", Fpackage_percent_set_use_list,
       Spackage_percent_set_use_list, 2, 2, 0, doc: /* Internal use only.  */)
  (Lisp_Object package, Lisp_Object use_list)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->use_list = use_list;
}

DEFUN ("package-%shadowing-symbols", Fpackage_percent_shadowing_symbols,
       Spackage_percent_shadowing_symbols, 1, 1, 0, doc:  /* Internal use only.  */)
  (Lisp_Object package)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->shadowing_symbols;
}

DEFUN ("package-%set-shadowing-symbols", Fpackage_percent_set_shadowing_symbols,
       Spackage_percent_set_shadowing_symbols, 2, 2, 0, doc: /* Internal use only.  */)
  (Lisp_Object package, Lisp_Object shadowing_symbols)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->shadowing_symbols = shadowing_symbols;
}

DEFUN ("package-%symbols", Fpackage_percent_symbols,
       Spackage_percent_symbols, 1, 1, 0, doc:  /* Internal use only.  */)
  (Lisp_Object package)
{
  CHECK_PACKAGE (package);
  return XPACKAGE (package)->symbols;
}

DEFUN ("package-%set-status", Fpackage_percent_set_status,
       Spackage_percent_set_status, 3, 3, 0, doc:  /* Internal use only.  */)
  (Lisp_Object symbol, Lisp_Object package, Lisp_Object status)
{
  return pkg_set_status (symbol, package, status);
}

DEFUN ("package-%register", Fpackage_percent_register,
       Spackage_percent_register, 1, 1, 0, doc:  /* Internal use only.  */)
  (Lisp_Object package)
{
  pkg_register_package (package);
  return package;
}

DEFUN ("package-%set-symbol-package", Fpackage_percent_set_symbol_package,
       Spackage_percent_set_symbol_package, 2, 2, 0, doc:  /* Internal use only.  */)
  (Lisp_Object symbol, Lisp_Object package)
{
  CHECK_SYMBOL (symbol);
  if (!NILP (package))
    CHECK_PACKAGE (package);
  XSYMBOL (symbol)->u.s.package = package;
  return symbol;
}

DEFUN ("watch-*package*", Fwatch_earmuffs_package, Swatch_earmuffs_package,
       4, 4, 0, doc:  /* Internal use only.  */)
  (Lisp_Object symbol, Lisp_Object newval, Lisp_Object operation,
   Lisp_Object where)
{
  if (EQ (operation, Qmakunbound))
    {
      if (!BUFFERP (where))
	error ("Cannot makunbound %s", SDATA (SYMBOL_NAME (symbol)));
    }
  else if (!PACKAGEP (newval))
    error ("%s must alwasy be bound to a package object (operation %s)",
	   SDATA (SYMBOL_NAME (symbol)),
	   SDATA (SYMBOL_NAME (operation)));
  return Qnil;
}




/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Called very early, after init_alloc_once and init_obarray_once.
   Not called when starting a dumped Emacs.  */

void
init_pkg_once (void)
{
  DEFSYM (QCexternal, ":external");
  DEFSYM (QCinherited, ":inherited");
  DEFSYM (QCinternal, ":internal");
  DEFSYM (QCnicknames, ":nicknames");
  DEFSYM (QCuse, ":use");

  DEFSYM (Qearmuffs_package, "*package*");
  DEFSYM (Qemacs_package, "emacs-package");
  DEFSYM (Qkeyword, "keyword");
  DEFSYM (Qkeyword_package, "keyword-package");
  DEFSYM (Qpackage, "package");
  DEFSYM (Qpackage_prefixes, "package-prefixes");
  DEFSYM (Qpackage_registry, "package-registry");
  DEFSYM (Qpackagep, "packagep");
  DEFSYM (Qsymbol_packages, "symbol-packages");
  DEFSYM (Qsymbol_packages, "symbol-packages");
  DEFSYM (Qwatch_earmuffs_package, "watch-*package*");

  staticpro (&Vpackage_registry);
  Vpackage_registry = make_hash_table (hashtest_equal, DEFAULT_HASH_SIZE,
				       DEFAULT_REHASH_SIZE,
				       DEFAULT_REHASH_THRESHOLD,
				       Qnil, false);

  staticpro (&Vemacs_package);
  Vemacs_package = pkg_make_package (build_string ("emacs"),
				     make_fixnum (100000));
  pkg_register_package (Vemacs_package);

  staticpro (&Vkeyword_package);
  Vkeyword_package = pkg_make_package (build_string ("keyword"),
				       make_fixnum (5000));
  XPACKAGE (Vkeyword_package)->nicknames = Fcons (build_string (""), Qnil);
  pkg_register_package (Vkeyword_package);

  staticpro (&Vemacs_user_package);
  Vemacs_user_package = pkg_make_package (build_string ("emacs-user"),
					  make_fixnum (1000));
  XPACKAGE (Vemacs_user_package)->use_list = Fcons (Vemacs_package, Qnil);
  pkg_register_package (Vemacs_user_package);

  staticpro (&Vearmuffs_package);
  Vearmuffs_package = Vemacs_package;
  XSYMBOL (Qearmuffs_package)->u.s.declared_special = true;

  DEFSYM (Qpackage_prefixes, "package-prefixes");
  staticpro (&Vpackage_prefixes);
  Vpackage_prefixes = Qnil;

  pkg_define_builtin_symbols ();
}

/* Not called when starting a dumped Emacs.  */

void
syms_of_pkg (void)
{
  defsubr (&Scl_intern);
  defsubr (&Scl_unintern);
  defsubr (&Sfind_symbol);
  defsubr (&Smake_percent_package);
  defsubr (&Spackage_percent_name);
  defsubr (&Spackage_percent_nicknames);
  defsubr (&Spackage_percent_register);
  defsubr (&Spackage_percent_set_name);
  defsubr (&Spackage_percent_set_nicknames);
  defsubr (&Spackage_percent_set_shadowing_symbols);
  defsubr (&Spackage_percent_set_status);
  defsubr (&Spackage_percent_set_symbol_package);
  defsubr (&Spackage_percent_set_use_list);
  defsubr (&Spackage_percent_shadowing_symbols);
  defsubr (&Spackage_percent_symbols);
  defsubr (&Spackage_percent_use_list);
  defsubr (&Spackagep);
  defsubr (&Spkg_read);
  defsubr (&Swatch_earmuffs_package);

  DEFVAR_LISP_NOPRO ("*package-registry*", Vpackage_registry,
		     doc: /* The package registry.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*emacs-package*", Vemacs_package,
		     doc: /* The Emacs package.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*emacs-user-package*", Vemacs_user_package,
		     doc: /* The Emacs user package.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*keyword-package*", Vkeyword_package,
		     doc: /* The keyword package.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*package*", Vearmuffs_package,
		     doc: /* The current package.  */);
  Fmake_variable_buffer_local (Qearmuffs_package);
  Fadd_variable_watcher (Qearmuffs_package, Fsymbol_function (Qwatch_earmuffs_package));
  DEFVAR_LISP_NOPRO ("package-prefixes", Vpackage_prefixes,
		     doc: /* */);
  Fmake_variable_buffer_local (Qpackage_prefixes);

  Fprovide (Qsymbol_packages, Qnil);
}

/* Called when starting a dumped Emacs.  */

void
init_pkg (void)
{
}
