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

/* Lisp packages patterned after CMUCL, which implements CLHS plus
   extensions.  The extensions are currently not implemented.

   Useful features that could be added:
   package locks
   hierarchical packages
   package-local nicknames  */

#include <config.h>
#include "lisp.h"
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

/***********************************************************************
			       Helpers
 ***********************************************************************/

Lisp_Object
pkg_find_package (Lisp_Object name)
{
  CHECK_STRING (name);
  return Fgethash (name, Vpackage_registry, Qnil);
}

/* Create and return a new Lisp package object for a package with name
   NAME, a string.  NSYMBOLS is the sieo of the symbol-table to allocate.  */

static Lisp_Object
make_package (Lisp_Object name, Lisp_Object nsymbols)
{
  struct Lisp_Package *pkg
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Package, symbols,
				    PVEC_PACKAGE);
  pkg->name = name;
  pkg->symbols = CALLN (Fmake_hash_table, QCtest, Qequal,
			QCsize, nsymbols);
  Lisp_Object package;
  XSETPACKAGE (package, pkg);
  return package;
}

/* Return a string for DESIGNATOR.  If DESIGNATOR is a symbol, return
   the symbol's name.  If DESIGNATOR is a string, return that string.
   If DESIGNATOR is a character, return a string that contains only
   that character.  If it is neither, signal an error.  */

static Lisp_Object
string_from_designator (Lisp_Object designator)
{
  if (SYMBOLP (designator))
    return Fsymbol_name (designator);
  if (STRINGP (designator))
    return designator;
  if (CHARACTERP (designator))
    return Fchar_to_string (designator);
  signal_error ("Not a string designator", designator);
}

/* Valiue is PACKAGE, if it is a package, otherwise signal an
   error.  */

static Lisp_Object
check_package (Lisp_Object package)
{
  if (PACKAGEP (package))
    return package;
  signal_error ("Not a package", package);
}

/* Return a package for a package designator DESIGNATOR.  If
   DESIGNATOR is a package, return that package.  Otherwise,
   DESIGNATOR must a string designator for a registered package.
   Signal an error in the designator case if the package is not
   registered.  */

static Lisp_Object
package_from_designator (Lisp_Object designator)
{
  /* PKG-FIXME? Not signaling here if DESIGNATOR is not registered is
     odd, but I think that's what CLHS says.  */
  if (PACKAGEP (designator))
    return designator;
  const Lisp_Object name = string_from_designator (designator);
  const Lisp_Object package = pkg_find_package (name);
  return check_package (package);
}

/* Value is the package designated by DESIGNATOR, or the value of
   "*package*" if DESIGNATOR is nil.  Signal an error if DESIGNATOR is
   not a registered package, or *package* is not.  */

static Lisp_Object
package_or_default (Lisp_Object designator)
{
  if (NILP (designator))
    return check_package (Vearmuffs_package);
  return package_from_designator (designator);
}

/* Check for conflicts of NAME and NICKNAMES with registered packages.
   Value is the conflicting package or nil.  */

static Lisp_Object
conflicting_package (Lisp_Object name, Lisp_Object nicknames)
{
  const Lisp_Object conflict = pkg_find_package (name);
  if (!NILP (conflict))
    return conflict;

  Lisp_Object tail = nicknames;
  FOR_EACH_TAIL (tail)
    {
      const Lisp_Object conflict = pkg_find_package (XCAR (tail));
      if (!NILP (conflict))
	return conflict;
    }

  return Qnil;
}

/* Register package PACKAGE in the package registry, that is, make it
   known under its name and all its nicknames.  */

static void
register_package (Lisp_Object package)
{
  const struct Lisp_Package *pkg = XPACKAGE (package);
  Fputhash (pkg->name, package, Vpackage_registry);
  Lisp_Object tail = pkg->nicknames;
  FOR_EACH_TAIL (tail)
    Fputhash (XCAR (tail), package, Vpackage_registry);
}

/***********************************************************************
                             Symbol table
 ***********************************************************************/

/* This is a bit fiddly because nil is a "normal" symbol that has
   a package and so on.  */

/* Find a symbol with name NAME in PACKAGE or one of the packages it
   inherits from.  Value is Qunbound if no symbol is found.  SEEN is a
   list of packages that have already been checked, to prevent infinte
   recursion.  */

static Lisp_Object
lookup_symbol1 (Lisp_Object name, Lisp_Object package, Lisp_Object seen)
{
  eassert (STRINGP (name));
  eassert (PACKAGEP (package));
  eassert (CONSP (seen) || NILP (seen));

  const struct Lisp_Package *pkg = XPACKAGE (package);
  Lisp_Object symbol = Fgethash (name, pkg->symbols, Qunbound);
  if (EQ (symbol, Qunbound))
    {
      Lisp_Object tail = pkg->use_list;
      FOR_EACH_TAIL (tail)
	{
	  const Lisp_Object used_package = XCAR (tail);
	  if (NILP (Fmemq (used_package, seen)))
	    {
	      seen = Fcons (used_package, seen);
	      symbol = lookup_symbol1 (name, used_package, seen);
	      if (!EQ (symbol, Qunbound))
		return symbol;
	    }
	}
    }

  return symbol;
}

static Lisp_Object
lookup_symbol (Lisp_Object name, Lisp_Object package)
{
  return lookup_symbol1(name, package, Qnil);
}

static Lisp_Object
add_to_package_symbols (Lisp_Object symbol, Lisp_Object package)
{
  eassert (SYMBOLP (symbol));
  eassert (PACKAGEP (package));
  Fputhash (SYMBOL_NAME (symbol), symbol, XPACKAGE (package)->symbols);
  return symbol;
}

/* Remove NAME as a name for PACKAGE from the package registry.  */

static void
remove_from_package_symbols (Lisp_Object symbol, Lisp_Object package)
{
  eassert (SYMBOLP (symbol));
  eassert (PACKAGEP (package));
  eassert (EQ (SYMBOL_PACKAGE (symbol), package));
  Fremhash (SYMBOL_NAME (symbol), XPACKAGE (package)->symbols);
}

/* Add a new SYMBOL to package PACKAGE.  Value is SYMBOL.  The symbol
   is made external if PACKAGE is the keyword package.  Otherwise it
   is internal.  */

static Lisp_Object
pkg_add_symbol (Lisp_Object symbol, Lisp_Object package)
{
  eassert (SYMBOLP (symbol));
  eassert (PACKAGEP (package));
  eassert (NILP (SYMBOL_PACKAGE (symbol)));

  XSYMBOL (symbol)->u.s.package = package;
  XSYMBOL (symbol)->u.s.external = EQ (package, Vkeyword_package);

  /* There should be no symbol with the name in the package.  */
#ifdef ENABLE_CHECKING
  const Lisp_Object existing
    = Fgethash (SYMBOL_NAME (symbol), XPACKAGE (package)->symbols, Qunbound);
  eassert (EQ (existing, Qunbound));
#endif

  return add_to_package_symbols (symbol, package);
}

/* Remvoe SYMBOL from the shadowing list of PACKAGE.  */

static void
remove_shadowing_symbol (Lisp_Object symbol, Lisp_Object package)
{
  struct Lisp_Package *pkg = XPACKAGE (package);
  pkg->shadowing_symbols = Fdelq (symbol, pkg->shadowing_symbols);
}

/* Return a list (SYMBOL STATUS) where STATUS is a symbol describing
   the status of SYMBOL relative to PACKAGE (internal, external,
   inherted).  This is kind of a poor man's substitude for multiple
   values.  */

static Lisp_Object
symbol_and_status (Lisp_Object symbol, Lisp_Object package)
{
  if (EQ (symbol, Qunbound))
    return Qnil;
  if (EQ (SYMBOL_PACKAGE (symbol), package))
    return list2 (symbol, SYMBOL_EXTERNAL_P (symbol) ? QCexternal : QCinternal);
  return list2 (symbol, QCinherited);
}

/* Add a new symbol with name NAME to PACKAGE.  If a symbol with name
   NAME is already accessible in PACKAGE, return that symbol.
   Otherwise, add a new symbol to PACKAGE.  Value is the symbol found
   or newly inserted.  */

Lisp_Object
pkg_intern_symbol (const Lisp_Object symbol_or_name, Lisp_Object package)
{
  /* PKG-FIXME this symbol_or_name is shit.  */
  eassert (PACKAGEP (package));

  const Lisp_Object name
    = SYMBOLP (symbol_or_name) ? SYMBOL_NAME (symbol_or_name) : symbol_or_name;
  CHECK_STRING (name);

  /* If already present in package, return that.  */
  Lisp_Object found = lookup_symbol (name, package);
  if (!EQ (found, Qunbound))
    {
      /* We should never find an uninterned symbol in a package.  */
      eassert (!NILP (SYMBOL_PACKAGE (found)));
      return found;
    }

  /* Not found.  If intended as a keyword, add it there. */
  if (EQ (package, Vkeyword_package))
    return pkg_intern_keyword (name);

  /* Not found, and we have already a symbol, use that symbol.  */
  if (SYMBOLP (symbol_or_name))
    return pkg_add_symbol (symbol_or_name, package);

  /* Make a new symbol and add it.  */
  return pkg_add_symbol (Fmake_symbol (name), package);
}

/* Lookup or create a new keyword with name NAME.  */

Lisp_Object
pkg_intern_keyword (Lisp_Object name)
{
  eassert (STRINGP (name));
  eassert (SREF (name, 0) != ':');
  Lisp_Object keyword = lookup_symbol (name, Vkeyword_package);
  if (EQ (keyword, Qunbound))
    {
      keyword = Fmake_symbol (name);
      /* Symbol-value of a keyword is itself, and cannot be set.  */
      XSYMBOL (keyword)->u.s.redirect = SYMBOL_PLAINVAL;
      XSYMBOL (keyword)->u.s.val.value = keyword;
      make_symbol_constant (keyword);
      /* Mark keywords as special.  This makes (let ((:key 'foo)) ...)
	 in lexically bound elisp signal an error, as documented.  */
      XSYMBOL (keyword)->u.s.declared_special = true;
      pkg_add_symbol (keyword, Vkeyword_package);
    }
  else
    eassert (SYMBOL_KEYWORD_P (keyword));

  return keyword;
}

/* Define KEYWORD as keyword symbol.  */

Lisp_Object
pkg_define_keyword (Lisp_Object keyword)
{
  eassert (SYMBOLP (keyword));
  eassert (!EQ (keyword, Qunbound));
  eassert (SREF (SYMBOL_NAME (keyword), 0) != ':');

  /* Symbol-value of a keyword is itself, and cannot be set.  */
  XSYMBOL (keyword)->u.s.redirect = SYMBOL_PLAINVAL;
  XSYMBOL (keyword)->u.s.val.value = keyword;
  make_symbol_constant (keyword);
  /* Mark keywords as special.  This makes (let ((:key 'foo)) ...)
     in lexically bound elisp signal an error, as documented.  */
  XSYMBOL (keyword)->u.s.declared_special = true;
  return pkg_add_symbol (keyword, Vkeyword_package);
}

Lisp_Object
pkg_define_non_keyword (Lisp_Object symbol)
{
  eassert (!EQ (symbol, Qunbound));
  return pkg_add_symbol (symbol, Vemacs_package);
}

Lisp_Object
pkg_intern_non_keyword (Lisp_Object name)
{
  return pkg_intern_symbol (name, Vearmuffs_package);
}

Lisp_Object
pkg_intern_non_keyword_c_string (const char *p, ptrdiff_t len)
{
  const Lisp_Object name = make_unibyte_string (p, len);
  return pkg_intern_non_keyword (name);
}

Lisp_Object
pkg_intern_maybe_keyword (Lisp_Object name)
{
  CHECK_STRING (name);
  if (SREF (name, 0) == ':')
    {
      const Lisp_Object keyword_name = Fsubstring (name, make_fixnum (1), Qnil);
      return pkg_intern_keyword (keyword_name);
    }
  return pkg_intern_non_keyword (name);
}

Lisp_Object
pkg_lookup_non_keyword_c_string (const char *ptr, ptrdiff_t nchars, ptrdiff_t nbytes)
{
  eassert (*ptr != ':');
  const Lisp_Object name = make_string_from_bytes (ptr, nchars, nbytes);
  return lookup_symbol (name, Vearmuffs_package);
}

static Lisp_Object
pkg_unintern_symbol (Lisp_Object symbol, Lisp_Object package)
{
  CHECK_SYMBOL (symbol);
  remove_shadowing_symbol (symbol, package);
  package = package_or_default (package);
  remove_shadowing_symbol (symbol, package);
  if (EQ (package, SYMBOL_PACKAGE (symbol)))
    {
      remove_from_package_symbols (symbol, package);
      return Qt;
    }

  /* PKG-FIXME: What to do if PACKAGE is not the home package?  */
  return Qnil;
}

void pkg_break (void)
{
}


static void
pkg_map_package_symbols (Lisp_Object fn, Lisp_Object package)
{
  package = check_package (package);
  FOR_EACH_KEY_VALUE (it_symbol, PACKAGE_SYMBOLS (package))
    call1 (fn, it_symbol.value);

}

/* Map FUNCTION over all symbols in PACKAGE.  */

static void
pkg_map_symbols (Lisp_Object function)
{
  FOR_EACH_KEY_VALUE (it_package, Vpackage_registry)
    pkg_map_package_symbols (function, it_package.value);
}

void
pkg_map_symbols_c_fn (void (*fn) (Lisp_Object, Lisp_Object), Lisp_Object arg)
{
  FOR_EACH_KEY_VALUE (it_package, Vpackage_registry)
    FOR_EACH_KEY_VALUE (it_symbol, PACKAGE_SYMBOLS (it_package.value))
      fn (it_symbol.value, arg);
}

/***********************************************************************
                        Old Emacs intern stuff
 ***********************************************************************/

/* The idea begind this is as follows:

   We want to het rid of Lisp_Symbol::next.  But legcaly code may
   still contain code for intended for obarrays.  These are the
   possibilities:

   1. The code uses the obarray variable.  In this case, he doesn't
   get a vector, but the Emacs package.

   2. The code makes an obarray with obarray-make, in which case he
   got a package.

   3. The code uses make-vector, in which case we make a package for
   him. */

static Lisp_Object
fake_me_an_obarray (Lisp_Object vector)
{
  eassert (VECTORP (vector));
  Lisp_Object package = Faref (vector, make_fixnum (0));
  if (!PACKAGEP (package))
    {
      package = make_package (build_string ("fake obarray"), Qnil);
      Faset (vector, make_fixnum (0), package);
    }
  return package;
}

/* Implements Emacs' old Fintern function.  */

Lisp_Object
pkg_emacs_intern (Lisp_Object name, Lisp_Object package)
{
  CHECK_STRING (name);

  /* PKG-FIXME: We are assuming that this is intended to be a keyword
     like it was before.  */
  if (SREF (name, 0) == ':' && NILP (package))
    {
      name = Fsubstring (name, make_fixnum (1), Qnil);
      package = Vkeyword_package;
    }

  eassert (SREF (name, 0) != ':');

  if (VECTORP (package))
    package = fake_me_an_obarray (package);
  package = package_or_default (package);

  return pkg_intern_symbol (name, package);
}

/* Implements Emacs' old Fintern_soft function.  */

Lisp_Object
pkg_emacs_intern_soft (Lisp_Object name, Lisp_Object package)
{
  /* intern-soft allows symbols.  */
  if (SYMBOLP (name))
    name = SYMBOL_NAME (name);
  CHECK_STRING (name);

  /* PKG-FIXME: We are assuming that this is intended to be a keyword
     like it was before.  */
  if (SREF (name, 0) == ':' && NILP (package))
    {
      name = Fsubstring (name, make_fixnum (1), Qnil);
      package = Vkeyword_package;
    }

  if (VECTORP (package))
    package = fake_me_an_obarray (package);
  package = package_or_default (package);

  Lisp_Object found = lookup_symbol (name, package);
  if (!EQ (found, Qunbound))
    {
      /* We should never find an uninterned symbol in a package.  */
      eassert (!NILP (SYMBOL_PACKAGE (found)));
      return found;
    }

  return Qnil;
}

/* Implements Emacs' old Funintern function.  */

Lisp_Object
pkg_emacs_unintern (Lisp_Object name, Lisp_Object package)
{
  if (VECTORP (package))
    package = fake_me_an_obarray (package);
  package = package_or_default (package);
  return pkg_unintern_symbol (name, package);
}

Lisp_Object
pkg_emacs_mapatoms (Lisp_Object function, Lisp_Object package)
{
  if (VECTORP (package))
    package = fake_me_an_obarray (package);
  if (NILP (package))
    pkg_map_symbols (function);
  else
    pkg_map_package_symbols (function, package);
  return Qnil;
}


/***********************************************************************
				Reader
 ***********************************************************************/

Lisp_Object
pkg_qualified_symbol (Lisp_Object name, Lisp_Object package, bool external)
{
  /* If we want a symbol for a given package, check the
     package has that symboland its accessibily.  */
  Lisp_Object found = Ffind_symbol (name, package);

  if (EQ (package, Vkeyword_package))
    {
      /* If found, use that symbol, else make a new one.
	 PKG-FIXME: there might already be a symbol named
	 'test' in the obarray, and we'd like to use that
	 name for ':test'.  That's a problem.  */

      /* PKG-FIXME: Make keywords constants.  */
      if (NILP (found))
	return pkg_intern_symbol (name, package);
      return XCAR (found);
    }

  if (NILP (found))
    pkg_error ("Symbol '%s' is not present in package", SDATA (name));

  /* Check if the symbol is accesible in the package as external
     symbol.  PKG-FIXME: Check what to do for inherited symbols.  */
  const Lisp_Object status = XCAR (XCDR (found));
  if (external && EQ (status, QCinternal))
    pkg_error ("Symbol '%s' is internal in package '%s'",
	       SDATA (name), SDATA (XPACKAGE (package)->name));

  return XCAR (found);
}

/* Return symbol with name NAME when accessed without qualification in
   the current package.  */

Lisp_Object
pkg_unqualified_symbol (Lisp_Object name)
{
  const Lisp_Object package = check_package (Vearmuffs_package);

  if (EQ (package, Vkeyword_package))
    return pkg_qualified_symbol (name, package, true);

  /* If we want a symbol for a given package, check the
     package has that symboland its accessibily.  */
  const Lisp_Object found = Ffind_symbol (name, package);
  if (!NILP (found))
    return XCAR (found);
  return pkg_intern_symbol (name, package);
}

bool
pkg_keywordp (Lisp_Object obj)
{
  if (!SYMBOLP (obj))
    return false;
  return EQ (SYMBOL_PACKAGE (obj), Vkeyword_package);
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
  return make_package (name, size);
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
  package = package_or_default (package);
  const Lisp_Object symbol = lookup_symbol (name, package);
  return symbol_and_status (symbol, package);
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
  package = package_or_default (package);
  const Lisp_Object symbol = pkg_intern_symbol (name, package);
  return symbol_and_status (symbol, package);
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
  return XPACKAGE (package)->name;
}

DEFUN ("package-%set-name", Fpackage_percent_set_name, Spackage_percent_set_name,
       2, 2, 0, doc: /* Internal use only.  */)
  (Lisp_Object package, Lisp_Object name)
{
  CHECK_PACKAGE (package);
  CHECK_STRING (name);
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

  staticpro (&Vpackage_registry);
  Vpackage_registry = make_hash_table (hashtest_equal, DEFAULT_HASH_SIZE,
				       DEFAULT_REHASH_SIZE,
				       DEFAULT_REHASH_THRESHOLD,
				       Qnil, false);

  staticpro (&Vemacs_package);
  Vemacs_package = make_package (build_string ("emacs"), make_fixnum (100000));
  register_package (Vemacs_package);

  staticpro (&Vkeyword_package);
  Vkeyword_package = make_package (build_string ("keyword"), make_fixnum (5000));
  XPACKAGE (Vkeyword_package)->nicknames = Fcons (build_string (""), Qnil);
  register_package (Vkeyword_package);

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
  DEFVAR_LISP_NOPRO ("*package-registry*", Vpackage_registry,
		     doc: /* The package registry.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*emacs-package*", Vemacs_package,
		     doc: /* The Emacs package.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*keyword-package*", Vkeyword_package,
		     doc: /* The keyword package.  For internal use only.  */);
  DEFVAR_LISP_NOPRO ("*package*", Vearmuffs_package,
		     doc: /* The current package.  */);

  DEFVAR_LISP_NOPRO ("package-prefixes", Vpackage_prefixes,
		     doc: /* */);
  Fmake_variable_buffer_local (Qpackage_prefixes);

  defsubr (&Spackage_percent_name);
  defsubr (&Spackage_percent_nicknames);
  defsubr (&Spackage_percent_set_name);
  defsubr (&Spackage_percent_set_nicknames);
  defsubr (&Spackage_percent_set_shadowing_symbols);
  defsubr (&Spackage_percent_set_use_list);
  defsubr (&Spackage_percent_shadowing_symbols);
  defsubr (&Spackage_percent_symbols);
  defsubr (&Spackage_percent_use_list);

  defsubr (&Smake_percent_package);
  defsubr (&Scl_intern);
  defsubr (&Scl_unintern);
  defsubr (&Sfind_symbol);
  defsubr (&Spackagep);
  defsubr (&Spkg_read);

  Fmake_variable_buffer_local (Qpackage_prefixes);
}

/* Called when starting a dumped Emacs.  */

void
init_pkg (void)
{
}
