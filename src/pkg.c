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

bool package_system_ready = false;

/* Lists of keywords and other symbols that are defined before
   packages are ready to use.  These are fixed up and the lists set
   to nil when the package system is ready.  */

static Lisp_Object early_keywords, early_symbols;

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

/* Cons ELT onto *LIST, and return *LIST.  */

static void
add_to_list (Lisp_Object elt, Lisp_Object *list)
{
  *list = Fcons (elt, *list);
}

/* Cons ELT onto *LIST, if not already present.  Return *LIST.  */

static void
add_new_to_list (Lisp_Object elt, Lisp_Object *list)
{
  if (NILP (Fmemq (elt, *list)))
    add_to_list (elt, list);
}

/***********************************************************************
			       Helpers
 ***********************************************************************/

/* If THING is nil, return nil.  If THING is symbol, return a list of
   length 1 containing THING: Otherwise, THING must be a list.  Check
   that each element of the list is a symbol, and return a new list
   containing all elements of THING, with duplicates removed.  */

static Lisp_Object
symbols_to_list (Lisp_Object thing)
{
  if (NILP (thing))
    return Qnil;
  if (SYMBOLP (thing))
    return list1 (thing);
  if (CONSP (thing))
    {
      Lisp_Object result = Qnil;
      Lisp_Object tail = thing;
      FOR_EACH_TAIL (tail)
	{
	  Lisp_Object symbol = XCAR (tail);
	  CHECK_SYMBOL (symbol);
	  add_new_to_list (symbol, &result);
	  return result;
	}
    }
  signal_error ("Not a list of symbols", thing);
}

/* Create and return a new Lisp package object for a package with name
   NAME, a string.

   What are the contents of the symbol hash table?  Mapping symbol
   names to entries of which form?  Can there be more than one
   symbol-name for different symbols */

static Lisp_Object
make_package (Lisp_Object name)
{
  struct Lisp_Package *pkg
    = ALLOCATE_ZEROED_PSEUDOVECTOR (struct Lisp_Package, symbols, PVEC_PACKAGE);
  eassert (STRINGP (name));
  pkg->name = name;
  pkg->symbols = CALLN (Fmake_hash_table, QCtest, Qequal, QCsize, make_fixnum (1024));

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

/* Return a list of strings for a list of string designators
   DESIGNATORS.  If DESIGNATORS is nil, return nil.  if DESIGNATORS is
   a list, return a new list of strings for the designators with order
   being preserved, and duplicates removed.  Signal an error if
   DESIGNATORS is neither nil nor a cons.  */

static Lisp_Object
string_list_from_designators (Lisp_Object designators)
{
  if (CONSP (designators))
    {
      Lisp_Object result = Qnil;
      Lisp_Object tail = designators;
      FOR_EACH_TAIL (tail)
	{
	  const Lisp_Object name = string_from_designator (XCAR (tail));
	  if (NILP (Fmember (name, result)))
	    result = Fcons (name, result);
	}
      return Fnreverse (result);
    }
  else if (NILP (designators))
    return Qnil;
  signal_error ("Not a list of strings designators", designators);
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
  const Lisp_Object package = Ffind_package (name);
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

/* Convert a list of package designators to a list of packages.
   Order is preserved, and duplicates are removed.  */

static Lisp_Object
package_list_from_designators (Lisp_Object designators)
{
  if (NILP (designators))
    return Qnil;
  if (CONSP (designators))
    {
      Lisp_Object result = Qnil;
      Lisp_Object tail = designators;
      FOR_EACH_TAIL (tail)
	{
	  Lisp_Object package = package_from_designator (XCAR (tail));
	  add_new_to_list (package, &result);
	}
      return Fnreverse (result);
    }
  signal_error ("Not a package designator list", designators);
}

/* Check for conflicts of NAME and NICKNAMES with registered packages.
   Value is the conflicting package or nil.  */

static Lisp_Object
conflicting_package (Lisp_Object name, Lisp_Object nicknames)
{
  const Lisp_Object conflict = Ffind_package (name);
  if (!NILP (conflict))
    return conflict;

  Lisp_Object tail = nicknames;
  FOR_EACH_TAIL (tail)
    {
      const Lisp_Object conflict = Ffind_package (XCAR (tail));
      if (!NILP (conflict))
	return conflict;
    }

  return Qnil;
}

/* Register NAME as a name for PACKAGE in the package registry.  */

static void
add_to_package_registry (Lisp_Object name, Lisp_Object package)
{
  eassert (STRINGP (name));
  eassert (PACKAGEP (package));
  Fputhash (name, package, Vpackage_registry);
}

/* Remove NAME as a name for PACKAGE from the package registry.  */

static void
remove_from_package_registry (Lisp_Object name)
{
  eassert (STRINGP (name));
  Fremhash (name, Vpackage_registry);
}

/* Register package PACKAGE in the package registry, that is, make it
   known under its name and all its nicknames.  */

static void
register_package (Lisp_Object package)
{
  const struct Lisp_Package *pkg = XPACKAGE (package);

  const Lisp_Object conflict = conflicting_package (pkg->name, pkg->nicknames);
  if (!NILP (conflict))
    signal_error ("Package name conflict", conflict);

  add_to_package_registry (pkg->name, package);
  Lisp_Object tail = pkg->nicknames;
  FOR_EACH_TAIL (tail)
    add_to_package_registry (XCAR (tail), package);
}

/* Remove PACKAGE fromt the package registry, that is, remove its name
   all its nicknames. Note that we intentionally don't remove the
   package from used_packages of other packages.  */

static void
unregister_package (Lisp_Object package)
{
  remove_from_package_registry (XPACKAGE (package)->name);
  Lisp_Object tail = XPACKAGE (package)->nicknames;
  FOR_EACH_TAIL (tail)
    remove_from_package_registry (XCAR (tail));
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
      Lisp_Object tail = pkg->used_packages;
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

static Lisp_Object
lookup_symbol (Lisp_Object name, Lisp_Object package)
{
  return lookup_symbol1(name, package, Qnil);
}

/* Add a new SYMBOL to package PACKAGE.  Value is SYMBOL.  The symbol
   is made external if PACKAGE is the keyword package.  Otherwise it
   is internal.  */

Lisp_Object
pkg_add_symbol (Lisp_Object symbol, Lisp_Object package)
{
  eassert (SYMBOLP (symbol));
  eassert (!package_system_ready || PACKAGEP (package));
  eassert (NILP (SYMBOL_PACKAGE (symbol)));

  /* IF we are not ready yet to do the right thing, remember
     the symbol for later.  There is only one candidate package
     to add it to later: the emacs package.  */
  if (!package_system_ready)
    {
      add_new_to_list (symbol, &early_symbols);
      return symbol;
    }

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

/* Add a new keyword by adding SYMBOL to the keyword package.  */

Lisp_Object
pkg_add_keyword (Lisp_Object symbol)
{
  /* Symbol-value of a keyword is itself, and cannot be set.  */
  XSYMBOL (symbol)->u.s.redirect = SYMBOL_PLAINVAL;
  XSYMBOL (symbol)->u.s.val.value = symbol;
  make_symbol_constant (symbol);

  /* Mark keywords as special.  This makes (let ((:key 'foo)) ...)
     in lexically bound elisp signal an error, as documented.  */
  XSYMBOL (symbol)->u.s.declared_special = true;

  if (package_system_ready)
    pkg_add_symbol (symbol, Vkeyword_package);
  else
    early_keywords = Fcons (symbol, early_keywords);
  return symbol;
}

/* Add SYMBOL to PACKAGE's shadowing symbols, if not already
   present.  */

static void
add_shadowing_symbol (Lisp_Object symbol, Lisp_Object package)
{
  struct Lisp_Package *pkg = XPACKAGE (package);
  add_new_to_list (symbol, &pkg->shadowing_symbols);
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

static Lisp_Object
pkg_intern_symbol (const Lisp_Object symbol_or_name, Lisp_Object package)
{
  eassert (package_system_ready);
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
    return pkg_add_keyword (Fmake_symbol (name));

  /* Not found, and we have already a symbol, use that symbol.  */
  if (SYMBOLP (symbol_or_name))
    return pkg_add_symbol (symbol_or_name, package);

  /* Make a new symbol and add it.  */
  return pkg_add_symbol (Fmake_symbol (name), package);
}

bool
pkg_intern_name_c_string (const char *p, ptrdiff_t len, Lisp_Object *symbol)
{
  if (!package_system_ready)
    return false;
  Lisp_Object name = make_unibyte_string (p, len);
  *symbol = pkg_intern_symbol (name, Vearmuffs_package);
  return true;
}

void
pkg_early_intern_symbol (Lisp_Object symbol)
{
  if (package_system_ready)
    pkg_intern_symbol (symbol, Vemacs_package);
  else
    pkg_add_symbol (symbol, Qnil);
}

static Lisp_Object
pkg_unintern_symbol (Lisp_Object symbol, Lisp_Object package)
{
  eassert (package_system_ready);
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


/***********************************************************************
			Old Emacs intern stuff
 ***********************************************************************/

/* Implements Emacs' old Fintern function.  */

Lisp_Object
pkg_emacs_intern (Lisp_Object name, Lisp_Object package)
{
  eassert (package_system_ready);
  CHECK_STRING (name);
  return pkg_intern_symbol (name, Vearmuffs_package);
}

/* Implements Emacs' old Fintern_soft function.  */

Lisp_Object
pkg_emacs_intern_soft (Lisp_Object symbol, Lisp_Object package)
{
  eassert (package_system_ready);

  const Lisp_Object name = SYMBOLP (symbol) ? SYMBOL_NAME (symbol) : symbol;
  CHECK_STRING (name);
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
  eassert (package_system_ready);
  package = package_or_default (package);
  return pkg_unintern_symbol (name, package);
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
  if (package_system_ready)
    return EQ (SYMBOL_PACKAGE (obj), Vkeyword_package);
  return !NILP (Fmemq (obj, early_keywords));
}


/***********************************************************************
			       Printer
 ***********************************************************************/


/***********************************************************************
			    Lisp functions
 ***********************************************************************/

DEFUN ("packagep", Fpackagep, Spackagep, 1, 1, 0, doc:
       /* Value is non-nil if PACKAGE is a package object. */)
  (Lisp_Object package)
{
  return PACKAGEP (package) ? Qt : Qnil;
}

DEFUN ("package-name", Fpackage_name, Spackage_name, 1, 1, 0, doc:
       /* Value is the name of package PACKAGE.  */)
  (Lisp_Object package)
{
  package = package_from_designator (package);
  return XPACKAGE (package)->name;
}

DEFUN ("package-nicknames", Fpackage_nicknames,
       Spackage_nicknames, 1, 1, 0, doc:
       /* Valus is the package nicknames of package PACKAGE.  */)
  (Lisp_Object package)
{
  package = package_from_designator (package);
  return Fcopy_sequence (XPACKAGE (package)->nicknames);
}

DEFUN ("package-shadowing-symbols", Fpackage_shadowing_symbols,
       Spackage_shadowing_symbols, 1, 1, 0, doc:
       /* tbd.  */)
  (Lisp_Object package)
{
  package = package_from_designator (package);
  return Fcopy_sequence (XPACKAGE (package)->shadowing_symbols);
}

DEFUN ("package-use-list", Fpackage_use_list, Spackage_use_list, 1, 1, 0, doc:
       /* tbd.  */)
  (Lisp_Object package)
{
  package = package_from_designator (package);
  return Fcopy_sequence (XPACKAGE (package)->used_packages);
}

DEFUN ("package-used-by-list", Fpackage_used_by_list, Spackage_used_by_list,
       1, 1, 0, doc:
       /* tbd.  */)
  (Lisp_Object package)
{
  package = package_from_designator (package);
  Lisp_Object result = Qnil;
  FOR_EACH_KEY_VALUE (it, Vpackage_registry)
    if (!NILP (Fmemq (package, XPACKAGE (it.value)->used_packages)))
      add_to_list (it.value, &result);
  return result;
}

DEFUN ("make-package", Fmake_package, Smake_package, 0, MANY, 0,
       doc: /* Value is a new package with name NAME.

NAME must be a string designator.

Additional arguments are specified as keyword/argument pairs.  The
following keyword arguments are defined:

:nicknames NICKNAMES is a list of additional names which may be used
to refer to the new package.

:use USE specifies a list of zero or more packages the external
symbols of which are to be inherited by the new package. See the
function 'use-package'.

usage: (make-package NAME &rest KEYWORD-ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (nargs <= 0)
    signal_error ("make-package: no package name", Qnil);

  /* Determine the package's name as a string.  A package with the
     same name or nickname must not be known yet.  */
  const Lisp_Object name = string_from_designator (args[0]);
  ++args;
  --nargs;

  /* The vector `used' is used to keep track of arguments that have
     been consumed below.  */
  USE_SAFE_ALLOCA;
  char *used_args = SAFE_ALLOCA (nargs * sizeof *used_args);
  memset (used_args, 0, nargs * sizeof *used_args);

  /* Check for :USE.  Argument must be a list of package designators
     for known packages.  */
  const ptrdiff_t use_index = get_key_arg (QCuse, nargs, args, used_args);
  const Lisp_Object use_designators = use_index ? args[use_index] : Qnil;
  const Lisp_Object used_packages = package_list_from_designators (use_designators);

  /* Check for :NICKNAMES.  Argument must be a list of string
     designators.  Note that we don't check if the package name
     appears also as a nickname, because SBCL also doesn't.  */
  const ptrdiff_t nicknames_index = get_key_arg (QCnicknames, nargs, args, used_args);
  const Lisp_Object nickname_designators = nicknames_index ? args[nicknames_index] : Qnil;
  const Lisp_Object nicknames = string_list_from_designators (nickname_designators);

  /* Now, all args should have been used up, or there's a problem.  */
  for (ptrdiff_t i = 0; i < nargs; ++i)
    if (!used_args[i])
      signal_error ("make-package: invalid argument", args[i]);

  const Lisp_Object package = make_package (name);
  XPACKAGE (package)->nicknames = nicknames;
  XPACKAGE (package)->used_packages = used_packages;

  /* PKG-FIXME:  Don't register, it's done by defpackage. */
  register_package (package);

  SAFE_FREE ();
  return package;
}

DEFUN ("list-all-packages", Flist_all_packages, Slist_all_packages, 0, 0, 0, doc:
       /* Return a list of all registered packages.  */)
  (void)
{
  Lisp_Object result = Qnil;
  FOR_EACH_KEY_VALUE (it, Vpackage_registry)
    result = Fcons (it.value, result);
  return result;
}

DEFUN ("find-package", Ffind_package, Sfind_package, 1, 1, 0, doc:
       /* Find the package with name or nickname NAME.

If NAME is a package object, return that.  Otherwise, NAME must be a
string designator.

Value is nil if no such package exists.  */)
  (Lisp_Object name)
{
  if (PACKAGEP (name))
    return name;
  name = string_from_designator (name);
  return Fgethash (name, Vpackage_registry, Qnil);
}

DEFUN ("delete-package", Fdelete_package, Sdelete_package, 1, 1, 0, doc:
       /* Delete package PACKAGE.

If the operation is successful, delete-package returns t, otherwise
nil.  The effect of delete-package is that the name and nicknames of
PACKAGE cease to be recognized package names.  The package object is
still a package (i.e., packagep is true of it) but package-name
returns nil.

The consequences of deleting the EMACS package or the KEYWORD package
are undefined.  The consequences of invoking any other package
operation on package once it has been deleted are unspecified. In
particular, the consequences of invoking find-symbol, intern and other
functions that look for a symbol name in a package are unspecified if
they are called with *package* bound to the deleted package or with
the deleted package as an argument.

If package is a package object that has already been deleted,
delete-package immediately returns nil.

After this operation completes, the home package of any symbol whose
home package had previously been package is
implementation-dependent. Except for this, symbols accessible in
package are not modified in any other way; symbols whose home package
is not package remain unchanged.  */)
  (Lisp_Object package)
{
  /* Deleting an already deleted package.  */
  if (NILP (XPACKAGE (package)->name))
    return Qnil;

  package = package_from_designator (package);

  /* Don't allow deleting the standard packages.  */
  if (EQ (package, Vemacs_package) || EQ (package, Vkeyword_package))
    signal_error ("Cannot delete standard package", package);

  unregister_package (package);
  XPACKAGE (package)->name = Qnil;
  return Qt;
}

DEFUN ("rename-package", Frename_package, Srename_package, 2, 3, 0, doc:
       /* Replace the name and nicknames of package.

PACKAGE must be a package designator.

NEW-NAME is the new name for the package.

Optional NEW-NICKNAMES replaces the nicknames of the package.  Note
that omitting NEW-NICKNAMES removes all nicknames.

The consequences are undefined if NEW-NAME or any NEW-NICKNAMES
conflicts with any existing package names.

Value is the package object after renaming.  */)
  (Lisp_Object package, Lisp_Object new_name, Lisp_Object new_nicknames)
{
  package = package_from_designator (package);

  /* Don't rename deleted package, which is what CLHS says, and SBCL
     does.  */
  if (NILP (XPACKAGE (package)->name))
    signal_error ("Cannot rename deleted package", package);

  /* Don't change anything if register would fail.  */
  new_name = string_from_designator (new_name);
  new_nicknames = string_list_from_designators (new_nicknames);
  const Lisp_Object conflict = conflicting_package (new_name, new_nicknames);
  if (!NILP (conflict))
    signal_error("Package name conflict", conflict);

  unregister_package (package);
  XPACKAGE (package)->name = new_name;
  XPACKAGE (package)->nicknames = new_nicknames;
  register_package (package);
  return package;
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

DEFUN ("export", Fexport, Sexport, 1, 2, 0, doc: /* tbd  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  return Qt;
}

DEFUN ("unexport", Funexport, Sunexport, 1, 2, 0, doc: /* tbd  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  return Qt;
}

DEFUN ("import", Fimport, Simport, 1, 2, 0, doc: /* tbd  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  return Qt;
}

DEFUN ("shadow", Fshadow, Sshadow, 1, 2, 0, doc:
       /* Make an internal symbol in PACKAGE with the same name as
  each of the specified SYMBOLS, adding the new symbols to the
  package-shadowing-symbols.  If a symbol with the given name is
  already present in PACKAGE, then the existing symbol is placed in
  the shadowing symbols list if it is not already present.  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  package = package_or_default (package);
  Lisp_Object tail = symbols_to_list (symbols);
  FOR_EACH_TAIL (tail)
    {
      const Lisp_Object name = string_from_designator (XCAR (tail));
      const Lisp_Object found = Ffind_symbol (name, package);
      Lisp_Object symbol = NILP (found) ? Qnil : XCAR (found);
      if (NILP (symbol) || EQ (XCAR (XCDR (found)), QCinherited))
	{
	  symbol = Fmake_symbol (name);
	  pkg_add_symbol (symbol, package);
	}
      add_shadowing_symbol (symbol, package);
    }
  return Qt;
}

DEFUN ("shadowing-import", Fshadowing_import, Sshadowing_import, 1, 2, 0,
       doc: /* Import SYMBOLS into PACKAGE, disregarding any name conflict.
  If a symbol of the same name is present, then it is uninterned.  The
  symbols are added to the 'package-shadowing-symbols'.  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  package = package_or_default (package);
  Lisp_Object tail = symbols_to_list (symbols);
  FOR_EACH_TAIL (tail)
    {
      const Lisp_Object import = XCAR (tail);
      const Lisp_Object found = Ffind_symbol (SYMBOL_NAME (import), package);
      const Lisp_Object symbol = NILP (found) ? Qnil : XCAR (found);
      const Lisp_Object status = NILP (found) ? Qnil : XCAR (XCDR (found));

      if (!EQ (import, symbol))
	{
	  /* Inintern if symbol with the same name is found.  */
	  if (EQ (status, QCinternal) || EQ (status, QCexternal))
	    {
	      remove_shadowing_symbol (symbol, package);
	      Fcl_unintern (symbol, package);
	    }
	}
      add_shadowing_symbol (import, package);
    }
  return Qt;
}

DEFUN ("use-package", Fuse_package, Suse_package, 1, 2, 0,
       doc: /* tbd  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  return Qt;
}

DEFUN ("unuse-package", Funuse_package, Sunuse_package, 1, 2, 0,
       doc: /* tbd  */)
  (Lisp_Object symbols, Lisp_Object package)
{
  return Qt;
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

/* Loop over early-defined symbols and fix their packages.  */

static void
fix_symbol_packages (void)
{
  int len_keywords = 0, len_symbols = 0;

  Lisp_Object tail = early_keywords;
  FOR_EACH_TAIL (tail)
    {

      /* Fix symbol names of keywords by removing the leading colon.  */
      Lisp_Object symbol = XCAR (tail);
      Lisp_Object name = SYMBOL_NAME (symbol);
      struct Lisp_String *s = XSTRING (name);
      if (s->u.s.size > 0 && *s->u.s.data == ':')
	{
	  eassume (s->u.s.size_byte == -2);
	  ++s->u.s.data;
	  --s->u.s.size;
	}
      pkg_add_symbol (symbol, Vkeyword_package);
      ++len_keywords;
    }

  tail = early_symbols;
  FOR_EACH_TAIL (tail)
    {
      ++len_symbols;
      pkg_add_symbol (XCAR (tail), Vemacs_package);
    }

  fprintf (stderr, "Early keywords = %d, symbols = %d\n", len_keywords, len_symbols);

  early_keywords = early_symbols = Qnil;

#ifdef ENABLE_CHECKING
  const Lisp_Object nil = lookup_symbol (SYMBOL_NAME (Qnil), Vemacs_package);
  eassert (EQ (nil, Qnil));
  eassert (NILP (nil));
  eassert (NILP (XSYMBOL (nil)->u.s.val.value));

  const Lisp_Object t = lookup_symbol (SYMBOL_NAME (Qt), Vemacs_package);
  eassert (EQ (t, Qt));
  eassert (EQ (XSYMBOL (t)->u.s.val.value, Qt));
#endif
}

/* Called very early, after init_alloc_once and init_obarray_once.
   Not called when starting a dumped Emacs.  */

void
init_pkg_once (void)
{
  staticpro (&early_symbols);
  early_keywords = Qnil;
  staticpro (&early_keywords);
  early_keywords = Qnil;
}

/* Not called when starting a dumped Emacs.  */

void
syms_of_pkg (void)
{
  defsubr (&Scl_intern);
  defsubr (&Scl_unintern);
  defsubr (&Sdelete_package);
  defsubr (&Sexport);
  defsubr (&Sfind_package);
  defsubr (&Sfind_symbol);
  defsubr (&Simport);
  defsubr (&Slist_all_packages);
  defsubr (&Smake_package);
  defsubr (&Spackage_name);
  defsubr (&Spackage_nicknames);
  defsubr (&Spackage_shadowing_symbols);
  defsubr (&Spackage_use_list);
  defsubr (&Spackage_used_by_list);
  defsubr (&Spackagep);
  defsubr (&Srename_package);
  defsubr (&Sshadow);
  defsubr (&Sshadowing_import);
  defsubr (&Sunexport);
  defsubr (&Sunuse_package);
  defsubr (&Suse_package);

  DEFSYM (QCexternal, ":external");
  DEFSYM (QCinherited, ":inherited");
  DEFSYM (QCinternal, ":internal");
  DEFSYM (QCnicknames, ":nicknames");
  DEFSYM (QCuse, ":use");

  DEFSYM (Qearmuffs_package, "*package*");
  DEFSYM (Qemacs_package, "emacs-package");
  DEFSYM (Qkeyword_package, "keyword-package");
  DEFSYM (Qpackage_registry, "package-registry");

  DEFSYM (Qkeyword, "keyword");
  DEFSYM (Qpackage, "package");

  DEFVAR_LISP ("package-registry", Vpackage_registry,
	       doc: "A map of names to packages.");
  Vpackage_registry = CALLN (Fmake_hash_table, QCtest, Qequal);

  DEFVAR_LISP ("emacs-package", Vemacs_package, doc: "The emacs package.");
  Vemacs_package = CALLN (Fmake_package, Qemacs);
  make_symbol_constant (Qemacs_package);

  DEFVAR_LISP ("keyword-package", Vkeyword_package, doc: "The keyword package.");
  Vkeyword_package = CALLN (Fmake_package, Qkeyword,
			    QCnicknames, list1 (make_string ("", 0)));
  make_symbol_constant (Qkeyword_package);

  DEFVAR_LISP ("*package*", Vearmuffs_package, doc: "The current package.");
  Vearmuffs_package = Vemacs_package;
  XSYMBOL (Qearmuffs_package)->u.s.declared_special = true;

  package_system_ready = true;
  fix_symbol_packages ();
}

/* Called when starting a dumped Emacs.  */

void
init_pkg (void)
{
}
