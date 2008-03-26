/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1988, 1993, 1994, 1995, 1997, 1998, 1999, 2000,
                 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
                 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include <config.h>
#include <signal.h>
#include <stdio.h>
#include "lisp.h"
#include "puresize.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "frame.h"
#include "syssignal.h"
#include "termhooks.h"  /* For FRAME_KBOARD reference in y-or-n-p. */

#ifdef STDC_HEADERS
#include <float.h>
#endif

/* If IEEE_FLOATING_POINT isn't defined, default it from FLT_*. */
#ifndef IEEE_FLOATING_POINT
#if (FLT_RADIX == 2 && FLT_MANT_DIG == 24 \
     && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128)
#define IEEE_FLOATING_POINT 1
#else
#define IEEE_FLOATING_POINT 0
#endif
#endif

/* Work around a problem that happens because math.h on hpux 7
   defines two static variables--which, in Emacs, are not really static,
   because `static' is defined as nothing.  The problem is that they are
   here, in floatfns.c, and in lread.c.
   These macros prevent the name conflict.  */
#if defined (HPUX) && !defined (HPUX8)
#define _MAXLDBL data_c_maxldbl
#define _NMAXLDBL data_c_nmaxldbl
#endif

#include <math.h>

#if !defined (atof)
extern double atof ();
#endif /* !atof */

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qsubr, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
Lisp_Object Qvoid_variable, Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qcyclic_variable_indirection, Qcircular_list;
Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
Lisp_Object Qend_of_file, Qarith_error, Qmark_inactive;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qtext_read_only;

Lisp_Object Qintegerp, Qnatnump, Qwholenump, Qsymbolp, Qlistp, Qconsp;
Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qvectorp;
Lisp_Object Qbuffer_or_string_p, Qkeywordp;
Lisp_Object Qboundp, Qfboundp;
Lisp_Object Qchar_table_p, Qvector_or_char_table_p;

Lisp_Object Qcdr;
Lisp_Object Qad_advice_info, Qad_activate_internal;

Lisp_Object Qrange_error, Qdomain_error, Qsingularity_error;
Lisp_Object Qoverflow_error, Qunderflow_error;

Lisp_Object Qfloatp;
Lisp_Object Qnumberp, Qnumber_or_marker_p;

Lisp_Object Qinteger;
static Lisp_Object Qsymbol, Qstring, Qcons, Qmarker, Qoverlay;
static Lisp_Object Qfloat, Qwindow_configuration, Qwindow;
Lisp_Object Qprocess;
static Lisp_Object Qcompiled_function, Qbuffer, Qframe, Qvector;
static Lisp_Object Qchar_table, Qbool_vector, Qhash_table;
static Lisp_Object Qsubrp, Qmany, Qunevalled;

static Lisp_Object swap_in_symval_forwarding P_ ((Lisp_Object, Lisp_Object));

Lisp_Object Vmost_positive_fixnum, Vmost_negative_fixnum;


void
circular_list_error (list)
     Lisp_Object list;
{
  xsignal (Qcircular_list, list);
}


Lisp_Object
wrong_type_argument (predicate, value)
     register Lisp_Object predicate, value;
{
  /* If VALUE is not even a valid Lisp object, abort here
     where we can get a backtrace showing where it came from.  */
  if ((unsigned int) XTYPE (value) >= Lisp_Type_Limit)
    abort ();

  xsignal2 (Qwrong_type_argument, predicate, value);
}

void
pure_write_error ()
{
  error ("Attempt to modify read-only object");
}

void
args_out_of_range (a1, a2)
     Lisp_Object a1, a2;
{
  xsignal2 (Qargs_out_of_range, a1, a2);
}

void
args_out_of_range_3 (a1, a2, a3)
     Lisp_Object a1, a2, a3;
{
  xsignal3 (Qargs_out_of_range, a1, a2, a3);
}

/* On some machines, XINT needs a temporary location.
   Here it is, in case it is needed.  */

int sign_extend_temp;

/* On a few machines, XINT can only be done by calling this.  */

int
sign_extend_lisp_int (num)
     EMACS_INT num;
{
  if (num & (((EMACS_INT) 1) << (VALBITS - 1)))
    return num | (((EMACS_INT) (-1)) << VALBITS);
  else
    return num & ((((EMACS_INT) 1) << VALBITS) - 1);
}

/* Data type predicates */

DEFUN ("eq", Feq, Seq, 2, 2, 0,
       doc: /* Return t if the two args are the same Lisp object.  */)
     (obj1, obj2)
     Lisp_Object obj1, obj2;
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}

DEFUN ("null", Fnull, Snull, 1, 1, 0,
       doc: /* Return t if OBJECT is nil.  */)
     (object)
     Lisp_Object object;
{
  if (NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("type-of", Ftype_of, Stype_of, 1, 1, 0,
       doc: /* Return a symbol representing the type of OBJECT.
The symbol returned names the object's basic type;
for example, (type-of 1) returns `integer'.  */)
     (object)
     Lisp_Object object;
{
  switch (XTYPE (object))
    {
    case Lisp_Int:
      return Qinteger;

    case Lisp_Symbol:
      return Qsymbol;

    case Lisp_String:
      return Qstring;

    case Lisp_Cons:
      return Qcons;

    case Lisp_Misc:
      switch (XMISCTYPE (object))
	{
	case Lisp_Misc_Marker:
	  return Qmarker;
	case Lisp_Misc_Overlay:
	  return Qoverlay;
	case Lisp_Misc_Float:
	  return Qfloat;
	}
      abort ();

    case Lisp_Vectorlike:
      if (WINDOW_CONFIGURATIONP (object))
	return Qwindow_configuration;
      if (PROCESSP (object))
	return Qprocess;
      if (WINDOWP (object))
	return Qwindow;
      if (SUBRP (object))
	return Qsubr;
      if (COMPILEDP (object))
	return Qcompiled_function;
      if (BUFFERP (object))
	return Qbuffer;
      if (CHAR_TABLE_P (object))
	return Qchar_table;
      if (BOOL_VECTOR_P (object))
	return Qbool_vector;
      if (FRAMEP (object))
	return Qframe;
      if (HASH_TABLE_P (object))
	return Qhash_table;
      return Qvector;

    case Lisp_Float:
      return Qfloat;

    default:
      abort ();
    }
}

DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0,
       doc: /* Return t if OBJECT is a cons cell.  */)
     (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}

DEFUN ("atom", Fatom, Satom, 1, 1, 0,
       doc: /* Return t if OBJECT is not a cons cell.  This includes nil.  */)
     (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return Qnil;
  return Qt;
}

DEFUN ("listp", Flistp, Slistp, 1, 1, 0,
       doc: /* Return t if OBJECT is a list, that is, a cons cell or nil.
Otherwise, return nil.  */)
     (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("nlistp", Fnlistp, Snlistp, 1, 1, 0,
       doc: /* Return t if OBJECT is not a list.  Lists include nil.  */)
     (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object))
    return Qnil;
  return Qt;
}

DEFUN ("symbolp", Fsymbolp, Ssymbolp, 1, 1, 0,
       doc: /* Return t if OBJECT is a symbol.  */)
     (object)
     Lisp_Object object;
{
  if (SYMBOLP (object))
    return Qt;
  return Qnil;
}

/* Define this in C to avoid unnecessarily consing up the symbol
   name.  */
DEFUN ("keywordp", Fkeywordp, Skeywordp, 1, 1, 0,
       doc: /* Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with `:'
interned in the initial obarray.  */)
     (object)
     Lisp_Object object;
{
  if (SYMBOLP (object)
      && SREF (SYMBOL_NAME (object), 0) == ':'
      && SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("vectorp", Fvectorp, Svectorp, 1, 1, 0,
       doc: /* Return t if OBJECT is a vector.  */)
     (object)
     Lisp_Object object;
{
  if (VECTORP (object))
    return Qt;
  return Qnil;
}

DEFUN ("stringp", Fstringp, Sstringp, 1, 1, 0,
       doc: /* Return t if OBJECT is a string.  */)
     (object)
     Lisp_Object object;
{
  if (STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("multibyte-string-p", Fmultibyte_string_p, Smultibyte_string_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a multibyte string.  */)
     (object)
     Lisp_Object object;
{
  if (STRINGP (object) && STRING_MULTIBYTE (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-table-p", Fchar_table_p, Schar_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a char-table.  */)
     (object)
     Lisp_Object object;
{
  if (CHAR_TABLE_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("vector-or-char-table-p", Fvector_or_char_table_p,
       Svector_or_char_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a char-table or vector.  */)
     (object)
     Lisp_Object object;
{
  if (VECTORP (object) || CHAR_TABLE_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("bool-vector-p", Fbool_vector_p, Sbool_vector_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a bool-vector.  */)
     (object)
     Lisp_Object object;
{
  if (BOOL_VECTOR_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("arrayp", Farrayp, Sarrayp, 1, 1, 0,
       doc: /* Return t if OBJECT is an array (string or vector).  */)
     (object)
     Lisp_Object object;
{
  if (ARRAYP (object))
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
       doc: /* Return t if OBJECT is a sequence (list or array).  */)
     (object)
     register Lisp_Object object;
{
  if (CONSP (object) || NILP (object) || ARRAYP (object))
    return Qt;
  return Qnil;
}

DEFUN ("bufferp", Fbufferp, Sbufferp, 1, 1, 0,
       doc: /* Return t if OBJECT is an editor buffer.  */)
     (object)
     Lisp_Object object;
{
  if (BUFFERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("markerp", Fmarkerp, Smarkerp, 1, 1, 0,
       doc: /* Return t if OBJECT is a marker (editor pointer).  */)
     (object)
     Lisp_Object object;
{
  if (MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("subrp", Fsubrp, Ssubrp, 1, 1, 0,
       doc: /* Return t if OBJECT is a built-in function.  */)
     (object)
     Lisp_Object object;
{
  if (SUBRP (object))
    return Qt;
  return Qnil;
}

DEFUN ("byte-code-function-p", Fbyte_code_function_p, Sbyte_code_function_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a byte-compiled function object.  */)
     (object)
     Lisp_Object object;
{
  if (COMPILEDP (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, Schar_or_string_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a character or a string.  */)
     (object)
     register Lisp_Object object;
{
  if (CHARACTERP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integerp", Fintegerp, Sintegerp, 1, 1, 0,
       doc: /* Return t if OBJECT is an integer.  */)
     (object)
     Lisp_Object object;
{
  if (INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, Sinteger_or_marker_p, 1, 1, 0,
       doc: /* Return t if OBJECT is an integer or a marker (editor pointer).  */)
     (object)
     register Lisp_Object object;
{
  if (MARKERP (object) || INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("natnump", Fnatnump, Snatnump, 1, 1, 0,
       doc: /* Return t if OBJECT is a nonnegative integer.  */)
     (object)
     Lisp_Object object;
{
  if (NATNUMP (object))
    return Qt;
  return Qnil;
}

DEFUN ("numberp", Fnumberp, Snumberp, 1, 1, 0,
       doc: /* Return t if OBJECT is a number (floating point or integer).  */)
     (object)
     Lisp_Object object;
{
  if (NUMBERP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("number-or-marker-p", Fnumber_or_marker_p,
       Snumber_or_marker_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a number or a marker.  */)
     (object)
     Lisp_Object object;
{
  if (NUMBERP (object) || MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("floatp", Ffloatp, Sfloatp, 1, 1, 0,
       doc: /* Return t if OBJECT is a floating point number.  */)
     (object)
     Lisp_Object object;
{
  if (FLOATP (object))
    return Qt;
  return Qnil;
}


/* Extract and set components of lists */

DEFUN ("car", Fcar, Scar, 1, 1, 0,
       doc: /* Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as car, cdr, cons cell and list.  */)
     (list)
     register Lisp_Object list;
{
  return CAR (list);
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
       doc: /* Return the car of OBJECT if it is a cons cell, or else nil.  */)
     (object)
     Lisp_Object object;
{
  return CAR_SAFE (object);
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
       doc: /* Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as cdr, car, cons cell and list.  */)
     (list)
     register Lisp_Object list;
{
  return CDR (list);
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
       doc: /* Return the cdr of OBJECT if it is a cons cell, or else nil.  */)
     (object)
     Lisp_Object object;
{
  return CDR_SAFE (object);
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
       doc: /* Set the car of CELL to be NEWCAR.  Returns NEWCAR.  */)
     (cell, newcar)
     register Lisp_Object cell, newcar;
{
  CHECK_CONS (cell);
  CHECK_IMPURE (cell);
  XSETCAR (cell, newcar);
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
       doc: /* Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.  */)
     (cell, newcdr)
     register Lisp_Object cell, newcdr;
{
  CHECK_CONS (cell);
  CHECK_IMPURE (cell);
  XSETCDR (cell, newcdr);
  return newcdr;
}

/* Extract and set components of symbols */

DEFUN ("boundp", Fboundp, Sboundp, 1, 1, 0,
       doc: /* Return t if SYMBOL's value is not void.  */)
     (symbol)
     register Lisp_Object symbol;
{
  Lisp_Object valcontents;
  CHECK_SYMBOL (symbol);

  valcontents = SYMBOL_VALUE (symbol);

  if (BUFFER_LOCAL_VALUEP (valcontents))
    valcontents = swap_in_symval_forwarding (symbol, valcontents);

  return (EQ (valcontents, Qunbound) ? Qnil : Qt);
}

DEFUN ("fboundp", Ffboundp, Sfboundp, 1, 1, 0,
       doc: /* Return t if SYMBOL's function definition is not void.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  return (EQ (XSYMBOL (symbol)->function, Qunbound) ? Qnil : Qt);
}

DEFUN ("makunbound", Fmakunbound, Smakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's value be void.
Return SYMBOL.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  if (SYMBOL_CONSTANT_P (symbol))
    xsignal1 (Qsetting_constant, symbol);
  Fset (symbol, Qunbound);
  return symbol;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's function definition be void.
Return SYMBOL.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    xsignal1 (Qsetting_constant, symbol);
  XSYMBOL (symbol)->function = Qunbound;
  return symbol;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
       doc: /* Return SYMBOL's function definition.  Error if that is void.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  if (!EQ (XSYMBOL (symbol)->function, Qunbound))
    return XSYMBOL (symbol)->function;
  xsignal1 (Qvoid_function, symbol);
}

DEFUN ("symbol-plist", Fsymbol_plist, Ssymbol_plist, 1, 1, 0,
       doc: /* Return SYMBOL's property list.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  return XSYMBOL (symbol)->plist;
}

DEFUN ("symbol-name", Fsymbol_name, Ssymbol_name, 1, 1, 0,
       doc: /* Return SYMBOL's name, a string.  */)
     (symbol)
     register Lisp_Object symbol;
{
  register Lisp_Object name;

  CHECK_SYMBOL (symbol);
  name = SYMBOL_NAME (symbol);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.  */)
     (symbol, definition)
     register Lisp_Object symbol, definition;
{
  register Lisp_Object function;

  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    xsignal1 (Qsetting_constant, symbol);

  function = XSYMBOL (symbol)->function;

  if (!NILP (Vautoload_queue) && !EQ (function, Qunbound))
    Vautoload_queue = Fcons (Fcons (symbol, function), Vautoload_queue);

  if (CONSP (function) && EQ (XCAR (function), Qautoload))
    Fput (symbol, Qautoload, XCDR (function));

  XSYMBOL (symbol)->function = definition;
  /* Handle automatic advice activation */
  if (CONSP (XSYMBOL (symbol)->plist) && !NILP (Fget (symbol, Qad_advice_info)))
    {
      call2 (Qad_activate_internal, symbol, Qnil);
      definition = XSYMBOL (symbol)->function;
    }
  return definition;
}

extern Lisp_Object Qfunction_documentation;

DEFUN ("defalias", Fdefalias, Sdefalias, 2, 3, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
Associates the function with the current load file, if any.
The optional third argument DOCSTRING specifies the documentation string
for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
determined by DEFINITION.  */)
     (symbol, definition, docstring)
     register Lisp_Object symbol, definition, docstring;
{
  CHECK_SYMBOL (symbol);
  if (CONSP (XSYMBOL (symbol)->function)
      && EQ (XCAR (XSYMBOL (symbol)->function), Qautoload))
    LOADHIST_ATTACH (Fcons (Qt, symbol));
  definition = Ffset (symbol, definition);
  LOADHIST_ATTACH (Fcons (Qdefun, symbol));
  if (!NILP (docstring))
    Fput (symbol, Qfunction_documentation, docstring);
  return definition;
}

DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
       doc: /* Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.  */)
     (symbol, newplist)
     register Lisp_Object symbol, newplist;
{
  CHECK_SYMBOL (symbol);
  XSYMBOL (symbol)->plist = newplist;
  return newplist;
}

DEFUN ("subr-arity", Fsubr_arity, Ssubr_arity, 1, 1, 0,
       doc: /* Return minimum and maximum number of args allowed for SUBR.
SUBR must be a built-in function.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form.  */)
     (subr)
     Lisp_Object subr;
{
  short minargs, maxargs;
  CHECK_SUBR (subr);
  minargs = XSUBR (subr)->min_args;
  maxargs = XSUBR (subr)->max_args;
  if (maxargs == MANY)
    return Fcons (make_number (minargs), Qmany);
  else if (maxargs == UNEVALLED)
    return Fcons (make_number (minargs), Qunevalled);
  else
    return Fcons (make_number (minargs), make_number (maxargs));
}

DEFUN ("subr-name", Fsubr_name, Ssubr_name, 1, 1, 0,
       doc: /* Return name of subroutine SUBR.
SUBR must be a built-in function.  */)
     (subr)
     Lisp_Object subr;
{
  const char *name;
  CHECK_SUBR (subr);
  name = XSUBR (subr)->symbol_name;
  return make_string (name, strlen (name));
}

DEFUN ("interactive-form", Finteractive_form, Sinteractive_form, 1, 1, 0,
       doc: /* Return the interactive form of CMD or nil if none.
If CMD is not a command, the return value is nil.
Value, if non-nil, is a list \(interactive SPEC).  */)
     (cmd)
     Lisp_Object cmd;
{
  Lisp_Object fun = indirect_function (cmd); /* Check cycles.  */

  if (NILP (fun) || EQ (fun, Qunbound))
    return Qnil;

  /* Use an `interactive-form' property if present, analogous to the
     function-documentation property. */
  fun = cmd;
  while (SYMBOLP (fun))
    {
      Lisp_Object tmp = Fget (fun, intern ("interactive-form"));
      if (!NILP (tmp))
	return tmp;
      else
	fun = Fsymbol_function (fun);
    }

  if (SUBRP (fun))
    {
      char *spec = XSUBR (fun)->intspec;
      if (spec)
	return list2 (Qinteractive,
		      (*spec != '(') ? build_string (spec) :
		      Fcar (Fread_from_string (build_string (spec), Qnil, Qnil)));
    }
  else if (COMPILEDP (fun))
    {
      if ((ASIZE (fun) & PSEUDOVECTOR_SIZE_MASK) > COMPILED_INTERACTIVE)
	return list2 (Qinteractive, AREF (fun, COMPILED_INTERACTIVE));
    }
  else if (CONSP (fun))
    {
      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qlambda))
	return Fassq (Qinteractive, Fcdr (XCDR (fun)));
      else if (EQ (funcar, Qautoload))
	{
	  struct gcpro gcpro1;
	  GCPRO1 (cmd);
	  do_autoload (fun, cmd);
	  UNGCPRO;
	  return Finteractive_form (cmd);
	}
    }
  return Qnil;
}


/***********************************************************************
		Getting and Setting Values of Symbols
 ***********************************************************************/

/* Return the symbol holding SYMBOL's value.  Signal
   `cyclic-variable-indirection' if SYMBOL's chain of variable
   indirections contains a loop.  */

Lisp_Object
indirect_variable (symbol)
     Lisp_Object symbol;
{
  Lisp_Object tortoise, hare;

  hare = tortoise = symbol;

  while (XSYMBOL (hare)->indirect_variable)
    {
      hare = XSYMBOL (hare)->value;
      if (!XSYMBOL (hare)->indirect_variable)
	break;

      hare = XSYMBOL (hare)->value;
      tortoise = XSYMBOL (tortoise)->value;

      if (EQ (hare, tortoise))
	xsignal1 (Qcyclic_variable_indirection, symbol);
    }

  return hare;
}


DEFUN ("indirect-variable", Findirect_variable, Sindirect_variable, 1, 1, 0,
       doc: /* Return the variable at the end of OBJECT's variable chain.
If OBJECT is a symbol, follow all variable indirections and return the final
variable.  If OBJECT is not a symbol, just return it.
Signal a cyclic-variable-indirection error if there is a loop in the
variable chain of symbols.  */)
     (object)
     Lisp_Object object;
{
  if (SYMBOLP (object))
    object = indirect_variable (object);
  return object;
}


/* Given the raw contents of a symbol value cell,
   return the Lisp value of the symbol.
   This does not handle buffer-local variables; use
   swap_in_symval_forwarding for that.  */

Lisp_Object
do_symval_forwarding (valcontents)
     register Lisp_Object valcontents;
{
  register Lisp_Object val;
  if (MISCP (valcontents))
    switch (XMISCTYPE (valcontents))
      {
      case Lisp_Misc_Intfwd:
	XSETINT (val, *XINTFWD (valcontents)->intvar);
	return val;

      case Lisp_Misc_Boolfwd:
	return (*XBOOLFWD (valcontents)->boolvar ? Qt : Qnil);

      case Lisp_Misc_Objfwd:
	return *XOBJFWD (valcontents)->objvar;

      case Lisp_Misc_Buffer_Objfwd:
	return PER_BUFFER_VALUE (current_buffer,
				 XBUFFER_OBJFWD (valcontents)->offset);

      case Lisp_Misc_Kboard_Objfwd:
        /* We used to simply use current_kboard here, but from Lisp
           code, it's value is often unexpected.  It seems nicer to
           allow constructions like this to work as intuitively expected:

           	(with-selected-frame frame
                   (define-key local-function-map "\eOP" [f1]))

           On the other hand, this affects the semantics of
           last-command and real-last-command, and people may rely on
           that.  I took a quick look at the Lisp codebase, and I
           don't think anything will break.  --lorentey  */
	return *(Lisp_Object *)(XKBOARD_OBJFWD (valcontents)->offset
				+ (char *)FRAME_KBOARD (SELECTED_FRAME ()));
      }
  return valcontents;
}

/* Store NEWVAL into SYMBOL, where VALCONTENTS is found in the value cell
   of SYMBOL.  If SYMBOL is buffer-local, VALCONTENTS should be the
   buffer-independent contents of the value cell: forwarded just one
   step past the buffer-localness.

   BUF non-zero means set the value in buffer BUF instead of the
   current buffer.  This only plays a role for per-buffer variables.  */

void
store_symval_forwarding (symbol, valcontents, newval, buf)
     Lisp_Object symbol;
     register Lisp_Object valcontents, newval;
     struct buffer *buf;
{
  switch (SWITCH_ENUM_CAST (XTYPE (valcontents)))
    {
    case Lisp_Misc:
      switch (XMISCTYPE (valcontents))
	{
	case Lisp_Misc_Intfwd:
	  CHECK_NUMBER (newval);
	  *XINTFWD (valcontents)->intvar = XINT (newval);
	  /* This can never happen since intvar points to an EMACS_INT
	     which is at least large enough to hold a Lisp_Object.
             if (*XINTFWD (valcontents)->intvar != XINT (newval))
	       error ("Value out of range for variable `%s'",
	   	   SDATA (SYMBOL_NAME (symbol))); */
	  break;

	case Lisp_Misc_Boolfwd:
	  *XBOOLFWD (valcontents)->boolvar = !NILP (newval);
	  break;

	case Lisp_Misc_Objfwd:
	  *XOBJFWD (valcontents)->objvar = newval;

	  /* If this variable is a default for something stored
	     in the buffer itself, such as default-fill-column,
	     find the buffers that don't have local values for it
	     and update them.  */
	  if (XOBJFWD (valcontents)->objvar > (Lisp_Object *) &buffer_defaults
	      && XOBJFWD (valcontents)->objvar < (Lisp_Object *) (&buffer_defaults + 1))
	    {
	      int offset = ((char *) XOBJFWD (valcontents)->objvar
			    - (char *) &buffer_defaults);
	      int idx = PER_BUFFER_IDX (offset);

	      Lisp_Object tail;

	      if (idx <= 0)
		break;

	      for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
		{
		  Lisp_Object buf;
		  struct buffer *b;

		  buf = Fcdr (XCAR (tail));
		  if (!BUFFERP (buf)) continue;
		  b = XBUFFER (buf);

		  if (! PER_BUFFER_VALUE_P (b, idx))
		    PER_BUFFER_VALUE (b, offset) = newval;
		}
	    }
	  break;

	case Lisp_Misc_Buffer_Objfwd:
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    Lisp_Object type = XBUFFER_OBJFWD (valcontents)->slottype;

	    if (! NILP (type) && ! NILP (newval)
		&& XTYPE (newval) != XINT (type))
	      buffer_slot_type_mismatch (symbol, XINT (type));

	    if (buf == NULL)
	      buf = current_buffer;
	    PER_BUFFER_VALUE (buf, offset) = newval;
	  }
	  break;

	case Lisp_Misc_Kboard_Objfwd:
	  {
	    char *base = (char *) FRAME_KBOARD (SELECTED_FRAME ());
	    char *p = base + XKBOARD_OBJFWD (valcontents)->offset;
	    *(Lisp_Object *) p = newval;
	  }
	  break;

	default:
	  goto def;
	}
      break;

    default:
    def:
      valcontents = SYMBOL_VALUE (symbol);
      if (BUFFER_LOCAL_VALUEP (valcontents))
	XBUFFER_LOCAL_VALUE (valcontents)->realvalue = newval;
      else
	SET_SYMBOL_VALUE (symbol, newval);
    }
}

/* Set up SYMBOL to refer to its global binding.
   This makes it safe to alter the status of other bindings.  */

void
swap_in_global_binding (symbol)
     Lisp_Object symbol;
{
  Lisp_Object valcontents = SYMBOL_VALUE (symbol);
  struct Lisp_Buffer_Local_Value *blv = XBUFFER_LOCAL_VALUE (valcontents);
  Lisp_Object cdr = blv->cdr;

  /* Unload the previously loaded binding.  */
  Fsetcdr (XCAR (cdr),
	   do_symval_forwarding (blv->realvalue));

  /* Select the global binding in the symbol.  */
  XSETCAR (cdr, cdr);
  store_symval_forwarding (symbol, blv->realvalue, XCDR (cdr), NULL);

  /* Indicate that the global binding is set up now.  */
  blv->frame = Qnil;
  blv->buffer = Qnil;
  blv->found_for_frame = 0;
  blv->found_for_buffer = 0;
}

/* Set up the buffer-local symbol SYMBOL for validity in the current buffer.
   VALCONTENTS is the contents of its value cell,
   which points to a struct Lisp_Buffer_Local_Value.

   Return the value forwarded one step past the buffer-local stage.
   This could be another forwarding pointer.  */

static Lisp_Object
swap_in_symval_forwarding (symbol, valcontents)
     Lisp_Object symbol, valcontents;
{
  register Lisp_Object tem1;

  tem1 = XBUFFER_LOCAL_VALUE (valcontents)->buffer;

  if (NILP (tem1)
      || current_buffer != XBUFFER (tem1)
      || (XBUFFER_LOCAL_VALUE (valcontents)->check_frame
	  && ! EQ (selected_frame, XBUFFER_LOCAL_VALUE (valcontents)->frame)))
    {
      if (XSYMBOL (symbol)->indirect_variable)
	symbol = indirect_variable (symbol);

      /* Unload the previously loaded binding.  */
      tem1 = XCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr);
      Fsetcdr (tem1,
	       do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->realvalue));
      /* Choose the new binding.  */
      tem1 = assq_no_quit (symbol, current_buffer->local_var_alist);
      XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame = 0;
      XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 0;
      if (NILP (tem1))
	{
	  if (XBUFFER_LOCAL_VALUE (valcontents)->check_frame)
	    tem1 = assq_no_quit (symbol, XFRAME (selected_frame)->param_alist);
	  if (! NILP (tem1))
	    XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame = 1;
	  else
	    tem1 = XBUFFER_LOCAL_VALUE (valcontents)->cdr;
	}
      else
	XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 1;

      /* Load the new binding.  */
      XSETCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr, tem1);
      XSETBUFFER (XBUFFER_LOCAL_VALUE (valcontents)->buffer, current_buffer);
      XBUFFER_LOCAL_VALUE (valcontents)->frame = selected_frame;
      store_symval_forwarding (symbol,
			       XBUFFER_LOCAL_VALUE (valcontents)->realvalue,
			       Fcdr (tem1), NULL);
    }
  return XBUFFER_LOCAL_VALUE (valcontents)->realvalue;
}

/* Find the value of a symbol, returning Qunbound if it's not bound.
   This is helpful for code which just wants to get a variable's value
   if it has one, without signaling an error.
   Note that it must not be possible to quit
   within this function.  Great care is required for this.  */

Lisp_Object
find_symbol_value (symbol)
     Lisp_Object symbol;
{
  register Lisp_Object valcontents;
  register Lisp_Object val;

  CHECK_SYMBOL (symbol);
  valcontents = SYMBOL_VALUE (symbol);

  if (BUFFER_LOCAL_VALUEP (valcontents))
    valcontents = swap_in_symval_forwarding (symbol, valcontents);

  return do_symval_forwarding (valcontents);
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0,
       doc: /* Return SYMBOL's value.  Error if that is void.  */)
     (symbol)
     Lisp_Object symbol;
{
  Lisp_Object val;

  val = find_symbol_value (symbol);
  if (!EQ (val, Qunbound))
    return val;

  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set", Fset, Sset, 2, 2, 0,
       doc: /* Set SYMBOL's value to NEWVAL, and return NEWVAL.  */)
     (symbol, newval)
     register Lisp_Object symbol, newval;
{
  return set_internal (symbol, newval, current_buffer, 0);
}

/* Return 1 if SYMBOL currently has a let-binding
   which was made in the buffer that is now current.  */

static int
let_shadows_buffer_binding_p (symbol)
     Lisp_Object symbol;
{
  volatile struct specbinding *p;

  for (p = specpdl_ptr - 1; p >= specpdl; p--)
    if (p->func == NULL
	&& CONSP (p->symbol))
      {
	Lisp_Object let_bound_symbol = XCAR (p->symbol);
	if ((EQ (symbol, let_bound_symbol)
	     || (XSYMBOL (let_bound_symbol)->indirect_variable
		 && EQ (symbol, indirect_variable (let_bound_symbol))))
	    && XBUFFER (XCDR (XCDR (p->symbol))) == current_buffer)
	  break;
      }

  return p >= specpdl;
}

/* Store the value NEWVAL into SYMBOL.
   If buffer-locality is an issue, BUF specifies which buffer to use.
   (0 stands for the current buffer.)

   If BINDFLAG is zero, then if this symbol is supposed to become
   local in every buffer where it is set, then we make it local.
   If BINDFLAG is nonzero, we don't do that.  */

Lisp_Object
set_internal (symbol, newval, buf, bindflag)
     register Lisp_Object symbol, newval;
     struct buffer *buf;
     int bindflag;
{
  int voide = EQ (newval, Qunbound);

  register Lisp_Object valcontents, innercontents, tem1, current_alist_element;

  if (buf == 0)
    buf = current_buffer;

  /* If restoring in a dead buffer, do nothing.  */
  if (NILP (buf->name))
    return newval;

  CHECK_SYMBOL (symbol);
  if (SYMBOL_CONSTANT_P (symbol)
      && (NILP (Fkeywordp (symbol))
	  || !EQ (newval, SYMBOL_VALUE (symbol))))
    xsignal1 (Qsetting_constant, symbol);

  innercontents = valcontents = SYMBOL_VALUE (symbol);

  if (BUFFER_OBJFWDP (valcontents))
    {
      int offset = XBUFFER_OBJFWD (valcontents)->offset;
      int idx = PER_BUFFER_IDX (offset);
      if (idx > 0
	  && !bindflag
	  && !let_shadows_buffer_binding_p (symbol))
	SET_PER_BUFFER_VALUE_P (buf, idx, 1);
    }
  else if (BUFFER_LOCAL_VALUEP (valcontents))
    {
      /* valcontents is a struct Lisp_Buffer_Local_Value.   */
      if (XSYMBOL (symbol)->indirect_variable)
	symbol = indirect_variable (symbol);

      /* What binding is loaded right now?  */
      current_alist_element
	= XCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr);

      /* If the current buffer is not the buffer whose binding is
	 loaded, or if there may be frame-local bindings and the frame
	 isn't the right one, or if it's a Lisp_Buffer_Local_Value and
	 the default binding is loaded, the loaded binding may be the
	 wrong one.  */
      if (!BUFFERP (XBUFFER_LOCAL_VALUE (valcontents)->buffer)
	  || buf != XBUFFER (XBUFFER_LOCAL_VALUE (valcontents)->buffer)
	  || (XBUFFER_LOCAL_VALUE (valcontents)->check_frame
	      && !EQ (selected_frame, XBUFFER_LOCAL_VALUE (valcontents)->frame))
	  /* Also unload a global binding (if the var is local_if_set). */
	  || (EQ (XCAR (current_alist_element),
		  current_alist_element)))
	{
	  /* The currently loaded binding is not necessarily valid.
	     We need to unload it, and choose a new binding.  */

	  /* Write out `realvalue' to the old loaded binding.  */
          Fsetcdr (current_alist_element,
		   do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->realvalue));

	  /* Find the new binding.  */
	  tem1 = Fassq (symbol, buf->local_var_alist);
	  XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 1;
	  XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame = 0;

	  if (NILP (tem1))
	    {
	      /* This buffer still sees the default value.  */

	      /* If the variable is not local_if_set,
		 or if this is `let' rather than `set',
		 make CURRENT-ALIST-ELEMENT point to itself,
		 indicating that we're seeing the default value.
		 Likewise if the variable has been let-bound
		 in the current buffer.  */
	      if (bindflag || !XBUFFER_LOCAL_VALUE (valcontents)->local_if_set
		  || let_shadows_buffer_binding_p (symbol))
		{
		  XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 0;

		  if (XBUFFER_LOCAL_VALUE (valcontents)->check_frame)
		    tem1 = Fassq (symbol,
				  XFRAME (selected_frame)->param_alist);

		  if (! NILP (tem1))
		    XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame = 1;
		  else
		    tem1 = XBUFFER_LOCAL_VALUE (valcontents)->cdr;
		}
	      /* If it's a Lisp_Buffer_Local_Value, being set not bound,
		 and we're not within a let that was made for this buffer,
		 create a new buffer-local binding for the variable.
		 That means, give this buffer a new assoc for a local value
		 and load that binding.  */
	      else
		{
		  tem1 = Fcons (symbol, XCDR (current_alist_element));
		  buf->local_var_alist
		    = Fcons (tem1, buf->local_var_alist);
		}
	    }

	  /* Record which binding is now loaded.  */
	  XSETCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr, tem1);

	  /* Set `buffer' and `frame' slots for the binding now loaded.  */
	  XSETBUFFER (XBUFFER_LOCAL_VALUE (valcontents)->buffer, buf);
	  XBUFFER_LOCAL_VALUE (valcontents)->frame = selected_frame;
	}
      innercontents = XBUFFER_LOCAL_VALUE (valcontents)->realvalue;

      /* Store the new value in the cons-cell.  */
      XSETCDR (XCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr), newval);
    }

  /* If storing void (making the symbol void), forward only through
     buffer-local indicator, not through Lisp_Objfwd, etc.  */
  if (voide)
    store_symval_forwarding (symbol, Qnil, newval, buf);
  else
    store_symval_forwarding (symbol, innercontents, newval, buf);

  return newval;
}

/* Access or set a buffer-local symbol's default value.  */

/* Return the default value of SYMBOL, but don't check for voidness.
   Return Qunbound if it is void.  */

Lisp_Object
default_value (symbol)
     Lisp_Object symbol;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (symbol);
  valcontents = SYMBOL_VALUE (symbol);

  /* For a built-in buffer-local variable, get the default value
     rather than letting do_symval_forwarding get the current value.  */
  if (BUFFER_OBJFWDP (valcontents))
    {
      int offset = XBUFFER_OBJFWD (valcontents)->offset;
      if (PER_BUFFER_IDX (offset) != 0)
	return PER_BUFFER_DEFAULT (offset);
    }

  /* Handle user-created local variables.  */
  if (BUFFER_LOCAL_VALUEP (valcontents))
    {
      /* If var is set up for a buffer that lacks a local value for it,
	 the current value is nominally the default value.
	 But the `realvalue' slot may be more up to date, since
	 ordinary setq stores just that slot.  So use that.  */
      Lisp_Object current_alist_element, alist_element_car;
      current_alist_element
	= XCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr);
      alist_element_car = XCAR (current_alist_element);
      if (EQ (alist_element_car, current_alist_element))
	return do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->realvalue);
      else
	return XCDR (XBUFFER_LOCAL_VALUE (valcontents)->cdr);
    }
  /* For other variables, get the current value.  */
  return do_symval_forwarding (valcontents);
}

DEFUN ("default-boundp", Fdefault_boundp, Sdefault_boundp, 1, 1, 0,
       doc: /* Return t if SYMBOL has a non-void default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  */)
     (symbol)
     Lisp_Object symbol;
{
  register Lisp_Object value;

  value = default_value (symbol);
  return (EQ (value, Qunbound) ? Qnil : Qt);
}

DEFUN ("default-value", Fdefault_value, Sdefault_value, 1, 1, 0,
       doc: /* Return SYMBOL's default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  The default value is meaningful for variables with
local bindings in certain buffers.  */)
     (symbol)
     Lisp_Object symbol;
{
  register Lisp_Object value;

  value = default_value (symbol);
  if (!EQ (value, Qunbound))
    return value;

  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
       doc: /* Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
The default value is seen in buffers that do not have their own values
for this variable.  */)
     (symbol, value)
     Lisp_Object symbol, value;
{
  register Lisp_Object valcontents, current_alist_element, alist_element_buffer;

  CHECK_SYMBOL (symbol);
  valcontents = SYMBOL_VALUE (symbol);

  /* Handle variables like case-fold-search that have special slots
     in the buffer.  Make them work apparently like Lisp_Buffer_Local_Value
     variables.  */
  if (BUFFER_OBJFWDP (valcontents))
    {
      int offset = XBUFFER_OBJFWD (valcontents)->offset;
      int idx = PER_BUFFER_IDX (offset);

      PER_BUFFER_DEFAULT (offset) = value;

      /* If this variable is not always local in all buffers,
	 set it in the buffers that don't nominally have a local value.  */
      if (idx > 0)
	{
	  struct buffer *b;

	  for (b = all_buffers; b; b = b->next)
	    if (!PER_BUFFER_VALUE_P (b, idx))
	      PER_BUFFER_VALUE (b, offset) = value;
	}
      return value;
    }

  if (!BUFFER_LOCAL_VALUEP (valcontents))
    return Fset (symbol, value);

  /* Store new value into the DEFAULT-VALUE slot.  */
  XSETCDR (XBUFFER_LOCAL_VALUE (valcontents)->cdr, value);

  /* If the default binding is now loaded, set the REALVALUE slot too.  */
  current_alist_element
    = XCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr);
  alist_element_buffer = Fcar (current_alist_element);
  if (EQ (alist_element_buffer, current_alist_element))
    store_symval_forwarding (symbol,
			     XBUFFER_LOCAL_VALUE (valcontents)->realvalue,
			     value, NULL);

  return value;
}

DEFUN ("setq-default", Fsetq_default, Ssetq_default, 0, UNEVALLED, 0,
       doc: /* Set the default value of variable VAR to VALUE.
VAR, the variable name, is literal (not evaluated);
VALUE is an expression: it is evaluated and its value returned.
The default value of a variable is seen in buffers
that do not have their own values for the variable.

More generally, you can use multiple variables and values, as in
  (setq-default VAR VALUE VAR VALUE...)
This sets each VAR's default value to the corresponding VALUE.
The VALUE for the Nth VAR can refer to the new default values
of previous VARs.
usage: (setq-default [VAR VALUE]...)  */)
     (args)
     Lisp_Object args;
{
  register Lisp_Object args_left;
  register Lisp_Object val, symbol;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = Feval (Fcar (Fcdr (args_left)));
      symbol = XCAR (args_left);
      Fset_default (symbol, val);
      args_left = Fcdr (XCDR (args_left));
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

/* Lisp functions for creating and removing buffer-local variables.  */

DEFUN ("make-variable-buffer-local", Fmake_variable_buffer_local, Smake_variable_buffer_local,
       1, 1, "vMake Variable Buffer Local: ",
       doc: /* Make VARIABLE become buffer-local whenever it is set.
At any time, the value for the current buffer is in effect,
unless the variable has never been set in this buffer,
in which case the default value is in effect.
Note that binding the variable with `let', or setting it while
a `let'-style binding made in this buffer is in effect,
does not make the variable buffer-local.  Return VARIABLE.

In most cases it is better to use `make-local-variable',
which makes a variable local in just one buffer.

The function `default-value' gets the default value and `set-default' sets it.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents, newval;

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (XSYMBOL (variable)->constant || KBOARD_OBJFWDP (valcontents))
    error ("Symbol %s may not be buffer-local", SDATA (SYMBOL_NAME (variable)));

  if (BUFFER_OBJFWDP (valcontents))
    return variable;
  else if (BUFFER_LOCAL_VALUEP (valcontents))
    newval = valcontents;
  else
    {
      if (EQ (valcontents, Qunbound))
	SET_SYMBOL_VALUE (variable, Qnil);
      tem = Fcons (Qnil, Fsymbol_value (variable));
      XSETCAR (tem, tem);
      newval = allocate_misc ();
      XMISCTYPE (newval) = Lisp_Misc_Buffer_Local_Value;
      XBUFFER_LOCAL_VALUE (newval)->realvalue = SYMBOL_VALUE (variable);
      XBUFFER_LOCAL_VALUE (newval)->buffer = Fcurrent_buffer ();
      XBUFFER_LOCAL_VALUE (newval)->frame = Qnil;
      XBUFFER_LOCAL_VALUE (newval)->found_for_buffer = 0;
      XBUFFER_LOCAL_VALUE (newval)->found_for_frame = 0;
      XBUFFER_LOCAL_VALUE (newval)->check_frame = 0;
      XBUFFER_LOCAL_VALUE (newval)->cdr = tem;
      SET_SYMBOL_VALUE (variable, newval);
    }
  XBUFFER_LOCAL_VALUE (newval)->local_if_set = 1;
  return variable;
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
       1, 1, "vMake Local Variable: ",
       doc: /* Make VARIABLE have a separate value in the current buffer.
Other buffers will continue to share a common default value.
\(The buffer-local value of VARIABLE starts out as the same value
VARIABLE previously had.  If VARIABLE was void, it remains void.\)
Return VARIABLE.

If the variable is already arranged to become local when set,
this function causes a local value to exist for this buffer,
just as setting the variable would do.

This function returns VARIABLE, and therefore
  (set (make-local-variable 'VARIABLE) VALUE-EXP)
works.

See also `make-variable-buffer-local'.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (XSYMBOL (variable)->constant || KBOARD_OBJFWDP (valcontents))
    error ("Symbol %s may not be buffer-local", SDATA (SYMBOL_NAME (variable)));

  if ((BUFFER_LOCAL_VALUEP (valcontents)
       && XBUFFER_LOCAL_VALUE (valcontents)->local_if_set)
      || BUFFER_OBJFWDP (valcontents))
    {
      tem = Fboundp (variable);

      /* Make sure the symbol has a local value in this particular buffer,
	 by setting it to the same value it already has.  */
      Fset (variable, (EQ (tem, Qt) ? Fsymbol_value (variable) : Qunbound));
      return variable;
    }
  /* Make sure symbol is set up to hold per-buffer values.  */
  if (!BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object newval;
      tem = Fcons (Qnil, do_symval_forwarding (valcontents));
      XSETCAR (tem, tem);
      newval = allocate_misc ();
      XMISCTYPE (newval) = Lisp_Misc_Buffer_Local_Value;
      XBUFFER_LOCAL_VALUE (newval)->realvalue = SYMBOL_VALUE (variable);
      XBUFFER_LOCAL_VALUE (newval)->buffer = Qnil;
      XBUFFER_LOCAL_VALUE (newval)->frame = Qnil;
      XBUFFER_LOCAL_VALUE (newval)->local_if_set = 0;
      XBUFFER_LOCAL_VALUE (newval)->found_for_buffer = 0;
      XBUFFER_LOCAL_VALUE (newval)->found_for_frame = 0;
      XBUFFER_LOCAL_VALUE (newval)->check_frame = 0;
      XBUFFER_LOCAL_VALUE (newval)->cdr = tem;
      SET_SYMBOL_VALUE (variable, newval);
    }
  /* Make sure this buffer has its own value of symbol.  */
  tem = Fassq (variable, current_buffer->local_var_alist);
  if (NILP (tem))
    {
      /* Swap out any local binding for some other buffer, and make
	 sure the current value is permanently recorded, if it's the
	 default value.  */
      find_symbol_value (variable);

      current_buffer->local_var_alist
        = Fcons (Fcons (variable, XCDR (XBUFFER_LOCAL_VALUE (SYMBOL_VALUE (variable))->cdr)),
		 current_buffer->local_var_alist);

      /* Make sure symbol does not think it is set up for this buffer;
	 force it to look once again for this buffer's value.  */
      {
	Lisp_Object *pvalbuf;

	valcontents = SYMBOL_VALUE (variable);

	pvalbuf = &XBUFFER_LOCAL_VALUE (valcontents)->buffer;
	if (current_buffer == XBUFFER (*pvalbuf))
	  *pvalbuf = Qnil;
	XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 0;
      }
    }

  /* If the symbol forwards into a C variable, then load the binding
     for this buffer now.  If C code modifies the variable before we
     load the binding in, then that new value will clobber the default
     binding the next time we unload it.  */
  valcontents = XBUFFER_LOCAL_VALUE (SYMBOL_VALUE (variable))->realvalue;
  if (INTFWDP (valcontents) || BOOLFWDP (valcontents) || OBJFWDP (valcontents))
    swap_in_symval_forwarding (variable, SYMBOL_VALUE (variable));

  return variable;
}

DEFUN ("kill-local-variable", Fkill_local_variable, Skill_local_variable,
       1, 1, "vKill Local Variable: ",
       doc: /* Make VARIABLE no longer have a separate value in the current buffer.
From now on the default value will apply in this buffer.  Return VARIABLE.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  valcontents = SYMBOL_VALUE (variable);

  if (BUFFER_OBJFWDP (valcontents))
    {
      int offset = XBUFFER_OBJFWD (valcontents)->offset;
      int idx = PER_BUFFER_IDX (offset);

      if (idx > 0)
	{
	  SET_PER_BUFFER_VALUE_P (current_buffer, idx, 0);
	  PER_BUFFER_VALUE (current_buffer, offset)
	    = PER_BUFFER_DEFAULT (offset);
	}
      return variable;
    }

  if (!BUFFER_LOCAL_VALUEP (valcontents))
    return variable;

  /* Get rid of this buffer's alist element, if any.  */

  tem = Fassq (variable, current_buffer->local_var_alist);
  if (!NILP (tem))
    current_buffer->local_var_alist
      = Fdelq (tem, current_buffer->local_var_alist);

  /* If the symbol is set up with the current buffer's binding
     loaded, recompute its value.  We have to do it now, or else
     forwarded objects won't work right.  */
  {
    Lisp_Object *pvalbuf, buf;
    valcontents = SYMBOL_VALUE (variable);
    pvalbuf = &XBUFFER_LOCAL_VALUE (valcontents)->buffer;
    XSETBUFFER (buf, current_buffer);
    if (EQ (buf, *pvalbuf))
      {
	*pvalbuf = Qnil;
	XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 0;
	find_symbol_value (variable);
      }
  }

  return variable;
}

/* Lisp functions for creating and removing buffer-local variables.  */

DEFUN ("make-variable-frame-local", Fmake_variable_frame_local, Smake_variable_frame_local,
       1, 1, "vMake Variable Frame Local: ",
       doc: /* Enable VARIABLE to have frame-local bindings.
This does not create any frame-local bindings for VARIABLE,
it just makes them possible.

A frame-local binding is actually a frame parameter value.
If a frame F has a value for the frame parameter named VARIABLE,
that also acts as a frame-local binding for VARIABLE in F--
provided this function has been called to enable VARIABLE
to have frame-local bindings at all.

The only way to create a frame-local binding for VARIABLE in a frame
is to set the VARIABLE frame parameter of that frame.  See
`modify-frame-parameters' for how to set frame parameters.

Buffer-local bindings take precedence over frame-local bindings.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents, newval;

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (XSYMBOL (variable)->constant || KBOARD_OBJFWDP (valcontents)
      || BUFFER_OBJFWDP (valcontents))
    error ("Symbol %s may not be frame-local", SDATA (SYMBOL_NAME (variable)));

  if (BUFFER_LOCAL_VALUEP (valcontents))
    {
      XBUFFER_LOCAL_VALUE (valcontents)->check_frame = 1;
      return variable;
    }

  if (EQ (valcontents, Qunbound))
    SET_SYMBOL_VALUE (variable, Qnil);
  tem = Fcons (Qnil, Fsymbol_value (variable));
  XSETCAR (tem, tem);
  newval = allocate_misc ();
  XMISCTYPE (newval) = Lisp_Misc_Buffer_Local_Value;
  XBUFFER_LOCAL_VALUE (newval)->realvalue = SYMBOL_VALUE (variable);
  XBUFFER_LOCAL_VALUE (newval)->buffer = Qnil;
  XBUFFER_LOCAL_VALUE (newval)->frame = Qnil;
  XBUFFER_LOCAL_VALUE (newval)->local_if_set = 0;
  XBUFFER_LOCAL_VALUE (newval)->found_for_buffer = 0;
  XBUFFER_LOCAL_VALUE (newval)->found_for_frame = 0;
  XBUFFER_LOCAL_VALUE (newval)->check_frame = 1;
  XBUFFER_LOCAL_VALUE (newval)->cdr = tem;
  SET_SYMBOL_VALUE (variable, newval);
  return variable;
}

DEFUN ("local-variable-p", Flocal_variable_p, Slocal_variable_p,
       1, 2, 0,
       doc: /* Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer.  */)
     (variable, buffer)
     register Lisp_Object variable, buffer;
{
  Lisp_Object valcontents;
  register struct buffer *buf;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object tail, elt;

      for (tail = buf->local_var_alist; CONSP (tail); tail = XCDR (tail))
	{
	  elt = XCAR (tail);
	  if (EQ (variable, XCAR (elt)))
	    return Qt;
	}
    }
  if (BUFFER_OBJFWDP (valcontents))
    {
      int offset = XBUFFER_OBJFWD (valcontents)->offset;
      int idx = PER_BUFFER_IDX (offset);
      if (idx == -1 || PER_BUFFER_VALUE_P (buf, idx))
	return Qt;
    }
  return Qnil;
}

DEFUN ("local-variable-if-set-p", Flocal_variable_if_set_p, Slocal_variable_if_set_p,
       1, 2, 0,
       doc: /* Non-nil if VARIABLE will be local in buffer BUFFER when set there.
More precisely, this means that setting the variable \(with `set' or`setq'),
while it does not have a `let'-style binding that was made in BUFFER,
will produce a buffer local binding.  See Info node
`(elisp)Creating Buffer-Local'.
BUFFER defaults to the current buffer.  */)
     (variable, buffer)
     register Lisp_Object variable, buffer;
{
  Lisp_Object valcontents;
  register struct buffer *buf;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  valcontents = SYMBOL_VALUE (variable);

  if (BUFFER_OBJFWDP (valcontents))
    /* All these slots become local if they are set.  */
    return Qt;
  else if (BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object tail, elt;
      if (XBUFFER_LOCAL_VALUE (valcontents)->local_if_set)
	return Qt;
      for (tail = buf->local_var_alist; CONSP (tail); tail = XCDR (tail))
	{
	  elt = XCAR (tail);
	  if (EQ (variable, XCAR (elt)))
	    return Qt;
	}
    }
  return Qnil;
}

DEFUN ("variable-binding-locus", Fvariable_binding_locus, Svariable_binding_locus,
       1, 1, 0,
       doc: /* Return a value indicating where VARIABLE's current binding comes from.
If the current binding is buffer-local, the value is the current buffer.
If the current binding is frame-local, the value is the selected frame.
If the current binding is global (the default), the value is nil.  */)
     (variable)
     register Lisp_Object variable;
{
  Lisp_Object valcontents;

  CHECK_SYMBOL (variable);
  variable = indirect_variable (variable);

  /* Make sure the current binding is actually swapped in.  */
  find_symbol_value (variable);

  valcontents = XSYMBOL (variable)->value;

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || BUFFER_OBJFWDP (valcontents))
    {
      /* For a local variable, record both the symbol and which
	 buffer's or frame's value we are saving.  */
      if (!NILP (Flocal_variable_p (variable, Qnil)))
	return Fcurrent_buffer ();
      else if (BUFFER_LOCAL_VALUEP (valcontents)
	       && XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame)
	return XBUFFER_LOCAL_VALUE (valcontents)->frame;
    }

  return Qnil;
}

/* This code is disabled now that we use the selected frame to return
   keyboard-local-values. */
#if 0
extern struct terminal *get_terminal P_ ((Lisp_Object display, int));

DEFUN ("terminal-local-value", Fterminal_local_value, Sterminal_local_value, 2, 2, 0,
       doc: /* Return the terminal-local value of SYMBOL on TERMINAL.
If SYMBOL is not a terminal-local variable, then return its normal
value, like `symbol-value'.

TERMINAL may be a terminal id, a frame, or nil (meaning the
selected frame's terminal device).  */)
  (symbol, terminal)
     Lisp_Object symbol;
     Lisp_Object terminal;
{
  Lisp_Object result;
  struct terminal *t = get_terminal (terminal, 1);
  push_kboard (t->kboard);
  result = Fsymbol_value (symbol);
  pop_kboard ();
  return result;
}

DEFUN ("set-terminal-local-value", Fset_terminal_local_value, Sset_terminal_local_value, 3, 3, 0,
       doc: /* Set the terminal-local binding of SYMBOL on TERMINAL to VALUE.
If VARIABLE is not a terminal-local variable, then set its normal
binding, like `set'.

TERMINAL may be a terminal id, a frame, or nil (meaning the
selected frame's terminal device).  */)
  (symbol, terminal, value)
     Lisp_Object symbol;
     Lisp_Object terminal;
     Lisp_Object value;
{
  Lisp_Object result;
  struct terminal *t = get_terminal (terminal, 1);
  push_kboard (d->kboard);
  result = Fset (symbol, value);
  pop_kboard ();
  return result;
}
#endif

/* Find the function at the end of a chain of symbol function indirections.  */

/* If OBJECT is a symbol, find the end of its function chain and
   return the value found there.  If OBJECT is not a symbol, just
   return it.  If there is a cycle in the function chain, signal a
   cyclic-function-indirection error.

   This is like Findirect_function, except that it doesn't signal an
   error if the chain ends up unbound.  */
Lisp_Object
indirect_function (object)
     register Lisp_Object object;
{
  Lisp_Object tortoise, hare;

  hare = tortoise = object;

  for (;;)
    {
      if (!SYMBOLP (hare) || EQ (hare, Qunbound))
	break;
      hare = XSYMBOL (hare)->function;
      if (!SYMBOLP (hare) || EQ (hare, Qunbound))
	break;
      hare = XSYMBOL (hare)->function;

      tortoise = XSYMBOL (tortoise)->function;

      if (EQ (hare, tortoise))
	xsignal1 (Qcyclic_function_indirection, object);
    }

  return hare;
}

DEFUN ("indirect-function", Findirect_function, Sindirect_function, 1, 2, 0,
       doc: /* Return the function at the end of OBJECT's function chain.
If OBJECT is not a symbol, just return it.  Otherwise, follow all
function indirections to find the final function binding and return it.
If the final symbol in the chain is unbound, signal a void-function error.
Optional arg NOERROR non-nil means to return nil instead of signalling.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.  */)
     (object, noerror)
     register Lisp_Object object;
     Lisp_Object noerror;
{
  Lisp_Object result;

  /* Optimize for no indirection.  */
  result = object;
  if (SYMBOLP (result) && !EQ (result, Qunbound)
      && (result = XSYMBOL (result)->function, SYMBOLP (result)))
    result = indirect_function (result);
  if (!EQ (result, Qunbound))
    return result;

  if (NILP (noerror))
    xsignal1 (Qvoid_function, object);

  return Qnil;
}

/* Extract and set vector and string elements */

DEFUN ("aref", Faref, Saref, 2, 2, 0,
       doc: /* Return the element of ARRAY at index IDX.
ARRAY may be a vector, a string, a char-table, a bool-vector,
or a byte-code object.  IDX starts at 0.  */)
     (array, idx)
     register Lisp_Object array;
     Lisp_Object idx;
{
  register int idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  if (STRINGP (array))
    {
      int c, idxval_byte;

      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      if (! STRING_MULTIBYTE (array))
	return make_number ((unsigned char) SREF (array, idxval));
      idxval_byte = string_char_to_byte (array, idxval);

      c = STRING_CHAR (SDATA (array) + idxval_byte,
		       SBYTES (array) - idxval_byte);
      return make_number (c);
    }
  else if (BOOL_VECTOR_P (array))
    {
      int val;

      if (idxval < 0 || idxval >= XBOOL_VECTOR (array)->size)
	args_out_of_range (array, idx);

      val = (unsigned char) XBOOL_VECTOR (array)->data[idxval / BOOL_VECTOR_BITS_PER_CHAR];
      return (val & (1 << (idxval % BOOL_VECTOR_BITS_PER_CHAR)) ? Qt : Qnil);
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      return CHAR_TABLE_REF (array, idxval);
    }
  else
    {
      int size = 0;
      if (VECTORP (array))
	size = XVECTOR (array)->size;
      else if (COMPILEDP (array))
	size = XVECTOR (array)->size & PSEUDOVECTOR_SIZE_MASK;
      else
	wrong_type_argument (Qarrayp, array);

      if (idxval < 0 || idxval >= size)
	args_out_of_range (array, idx);
      return XVECTOR (array)->contents[idxval];
    }
}

DEFUN ("aset", Faset, Saset, 3, 3, 0,
       doc: /* Store into the element of ARRAY at index IDX the value NEWELT.
Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
bool-vector.  IDX starts at 0.  */)
     (array, idx, newelt)
     register Lisp_Object array;
     Lisp_Object idx, newelt;
{
  register int idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  CHECK_ARRAY (array, Qarrayp);
  CHECK_IMPURE (array);

  if (VECTORP (array))
    {
      if (idxval < 0 || idxval >= XVECTOR (array)->size)
	args_out_of_range (array, idx);
      XVECTOR (array)->contents[idxval] = newelt;
    }
  else if (BOOL_VECTOR_P (array))
    {
      int val;

      if (idxval < 0 || idxval >= XBOOL_VECTOR (array)->size)
	args_out_of_range (array, idx);

      val = (unsigned char) XBOOL_VECTOR (array)->data[idxval / BOOL_VECTOR_BITS_PER_CHAR];

      if (! NILP (newelt))
	val |= 1 << (idxval % BOOL_VECTOR_BITS_PER_CHAR);
      else
	val &= ~(1 << (idxval % BOOL_VECTOR_BITS_PER_CHAR));
      XBOOL_VECTOR (array)->data[idxval / BOOL_VECTOR_BITS_PER_CHAR] = val;
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      CHAR_TABLE_SET (array, idxval, newelt);
    }
  else if (STRING_MULTIBYTE (array))
    {
      int idxval_byte, prev_bytes, new_bytes, nbytes;
      unsigned char workbuf[MAX_MULTIBYTE_LENGTH], *p0 = workbuf, *p1;

      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      CHECK_CHARACTER (newelt);

      nbytes = SBYTES (array);

      idxval_byte = string_char_to_byte (array, idxval);
      p1 = SDATA (array) + idxval_byte;
      PARSE_MULTIBYTE_SEQ (p1, nbytes - idxval_byte, prev_bytes);
      new_bytes = CHAR_STRING (XINT (newelt), p0);
      if (prev_bytes != new_bytes)
	{
	  /* We must relocate the string data.  */
	  int nchars = SCHARS (array);
	  unsigned char *str;
	  USE_SAFE_ALLOCA;

	  SAFE_ALLOCA (str, unsigned char *, nbytes);
	  bcopy (SDATA (array), str, nbytes);
	  allocate_string_data (XSTRING (array), nchars,
				nbytes + new_bytes - prev_bytes);
	  bcopy (str, SDATA (array), idxval_byte);
	  p1 = SDATA (array) + idxval_byte;
	  bcopy (str + idxval_byte + prev_bytes, p1 + new_bytes,
		 nbytes - (idxval_byte + prev_bytes));
	  SAFE_FREE ();
	  clear_string_char_byte_cache ();
	}
      while (new_bytes--)
	*p1++ = *p0++;
    }
  else
    {
      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      CHECK_NUMBER (newelt);

      if (XINT (newelt) >= 0 && ! SINGLE_BYTE_CHAR_P (XINT (newelt)))
	args_out_of_range (array, newelt);
      SSET (array, idxval, XINT (newelt));
    }

  return newelt;
}

/* Arithmetic functions */

enum comparison { equal, notequal, less, grtr, less_or_equal, grtr_or_equal };

Lisp_Object
arithcompare (num1, num2, comparison)
     Lisp_Object num1, num2;
     enum comparison comparison;
{
  double f1 = 0, f2 = 0;
  int floatp = 0;

  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num1);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num2);

  if (FLOATP (num1) || FLOATP (num2))
    {
      floatp = 1;
      f1 = (FLOATP (num1)) ? XFLOAT_DATA (num1) : XINT (num1);
      f2 = (FLOATP (num2)) ? XFLOAT_DATA (num2) : XINT (num2);
    }

  switch (comparison)
    {
    case equal:
      if (floatp ? f1 == f2 : XINT (num1) == XINT (num2))
	return Qt;
      return Qnil;

    case notequal:
      if (floatp ? f1 != f2 : XINT (num1) != XINT (num2))
	return Qt;
      return Qnil;

    case less:
      if (floatp ? f1 < f2 : XINT (num1) < XINT (num2))
	return Qt;
      return Qnil;

    case less_or_equal:
      if (floatp ? f1 <= f2 : XINT (num1) <= XINT (num2))
	return Qt;
      return Qnil;

    case grtr:
      if (floatp ? f1 > f2 : XINT (num1) > XINT (num2))
	return Qt;
      return Qnil;

    case grtr_or_equal:
      if (floatp ? f1 >= f2 : XINT (num1) >= XINT (num2))
	return Qt;
      return Qnil;

    default:
      abort ();
    }
}

DEFUN ("=", Feqlsign, Seqlsign, 2, 2, 0,
       doc: /* Return t if two args, both numbers or markers, are equal.  */)
     (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, equal);
}

DEFUN ("<", Flss, Slss, 2, 2, 0,
       doc: /* Return t if first arg is less than second arg.  Both must be numbers or markers.  */)
     (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, less);
}

DEFUN (">", Fgtr, Sgtr, 2, 2, 0,
       doc: /* Return t if first arg is greater than second arg.  Both must be numbers or markers.  */)
     (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, grtr);
}

DEFUN ("<=", Fleq, Sleq, 2, 2, 0,
       doc: /* Return t if first arg is less than or equal to second arg.
Both must be numbers or markers.  */)
     (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, less_or_equal);
}

DEFUN (">=", Fgeq, Sgeq, 2, 2, 0,
       doc: /* Return t if first arg is greater than or equal to second arg.
Both must be numbers or markers.  */)
     (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, grtr_or_equal);
}

DEFUN ("/=", Fneq, Sneq, 2, 2, 0,
       doc: /* Return t if first arg is not equal to second arg.  Both must be numbers or markers.  */)
     (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, notequal);
}

DEFUN ("zerop", Fzerop, Szerop, 1, 1, 0,
       doc: /* Return t if NUMBER is zero.  */)
     (number)
     register Lisp_Object number;
{
  CHECK_NUMBER_OR_FLOAT (number);

  if (FLOATP (number))
    {
      if (XFLOAT_DATA (number) == 0.0)
	return Qt;
      return Qnil;
    }

  if (!XINT (number))
    return Qt;
  return Qnil;
}

/* Convert between long values and pairs of Lisp integers.
   Note that long_to_cons returns a single Lisp integer
   when the value fits in one.  */

Lisp_Object
long_to_cons (i)
     unsigned long i;
{
  unsigned long top = i >> 16;
  unsigned int bot = i & 0xFFFF;
  if (top == 0)
    return make_number (bot);
  if (top == (unsigned long)-1 >> 16)
    return Fcons (make_number (-1), make_number (bot));
  return Fcons (make_number (top), make_number (bot));
}

unsigned long
cons_to_long (c)
     Lisp_Object c;
{
  Lisp_Object top, bot;
  if (INTEGERP (c))
    return XINT (c);
  top = XCAR (c);
  bot = XCDR (c);
  if (CONSP (bot))
    bot = XCAR (bot);
  return ((XINT (top) << 16) | XINT (bot));
}

DEFUN ("number-to-string", Fnumber_to_string, Snumber_to_string, 1, 1, 0,
       doc: /* Return the decimal representation of NUMBER as a string.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.  */)
     (number)
     Lisp_Object number;
{
  char buffer[VALBITS];

  CHECK_NUMBER_OR_FLOAT (number);

  if (FLOATP (number))
    {
      char pigbuf[350];	/* see comments in float_to_string */

      float_to_string (pigbuf, XFLOAT_DATA (number));
      return build_string (pigbuf);
    }

  if (sizeof (int) == sizeof (EMACS_INT))
    sprintf (buffer, "%d", (int) XINT (number));
  else if (sizeof (long) == sizeof (EMACS_INT))
    sprintf (buffer, "%ld", (long) XINT (number));
  else
    abort ();
  return build_string (buffer);
}

INLINE static int
digit_to_number (character, base)
     int character, base;
{
  int digit;

  if (character >= '0' && character <= '9')
    digit = character - '0';
  else if (character >= 'a' && character <= 'z')
    digit = character - 'a' + 10;
  else if (character >= 'A' && character <= 'Z')
    digit = character - 'A' + 10;
  else
    return -1;

  if (digit >= base)
    return -1;
  else
    return digit;
}

DEFUN ("string-to-number", Fstring_to_number, Sstring_to_number, 1, 2, 0,
       doc: /* Parse STRING as a decimal number and return the number.
This parses both integers and floating point numbers.
It ignores leading spaces and tabs.

If BASE, interpret STRING as a number in that base.  If BASE isn't
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, floating point is not recognized.  */)
     (string, base)
     register Lisp_Object string, base;
{
  register unsigned char *p;
  register int b;
  int sign = 1;
  Lisp_Object val;

  CHECK_STRING (string);

  if (NILP (base))
    b = 10;
  else
    {
      CHECK_NUMBER (base);
      b = XINT (base);
      if (b < 2 || b > 16)
	xsignal1 (Qargs_out_of_range, base);
    }

  /* Skip any whitespace at the front of the number.  Some versions of
     atoi do this anyway, so we might as well make Emacs lisp consistent.  */
  p = SDATA (string);
  while (*p == ' ' || *p == '\t')
    p++;

  if (*p == '-')
    {
      sign = -1;
      p++;
    }
  else if (*p == '+')
    p++;

  if (isfloat_string (p) && b == 10)
    val = make_float (sign * atof (p));
  else
    {
      double v = 0;

      while (1)
	{
	  int digit = digit_to_number (*p++, b);
	  if (digit < 0)
	    break;
	  v = v * b + digit;
	}

      val = make_fixnum_or_float (sign * v);
    }

  return val;
}


enum arithop
  {
    Aadd,
    Asub,
    Amult,
    Adiv,
    Alogand,
    Alogior,
    Alogxor,
    Amax,
    Amin
  };

static Lisp_Object float_arith_driver P_ ((double, int, enum arithop,
					   int, Lisp_Object *));
extern Lisp_Object fmod_float ();

Lisp_Object
arith_driver (code, nargs, args)
     enum arithop code;
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object val;
  register int argnum;
  register EMACS_INT accum = 0;
  register EMACS_INT next;

  switch (SWITCH_ENUM_CAST (code))
    {
    case Alogior:
    case Alogxor:
    case Aadd:
    case Asub:
      accum = 0;
      break;
    case Amult:
      accum = 1;
      break;
    case Alogand:
      accum = -1;
      break;
    default:
      break;
    }

  for (argnum = 0; argnum < nargs; argnum++)
    {
      /* Using args[argnum] as argument to CHECK_NUMBER_... */
      val = args[argnum];
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val);

      if (FLOATP (val))
	return float_arith_driver ((double) accum, argnum, code,
				   nargs, args);
      args[argnum] = val;
      next = XINT (args[argnum]);
      switch (SWITCH_ENUM_CAST (code))
	{
	case Aadd:
	  accum += next;
	  break;
	case Asub:
	  accum = argnum ? accum - next : nargs == 1 ? - next : next;
	  break;
	case Amult:
	  accum *= next;
	  break;
	case Adiv:
	  if (!argnum)
	    accum = next;
	  else
	    {
	      if (next == 0)
		xsignal0 (Qarith_error);
	      accum /= next;
	    }
	  break;
	case Alogand:
	  accum &= next;
	  break;
	case Alogior:
	  accum |= next;
	  break;
	case Alogxor:
	  accum ^= next;
	  break;
	case Amax:
	  if (!argnum || next > accum)
	    accum = next;
	  break;
	case Amin:
	  if (!argnum || next < accum)
	    accum = next;
	  break;
	}
    }

  XSETINT (val, accum);
  return val;
}

#undef isnan
#define isnan(x) ((x) != (x))

static Lisp_Object
float_arith_driver (accum, argnum, code, nargs, args)
     double accum;
     register int argnum;
     enum arithop code;
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object val;
  double next;

  for (; argnum < nargs; argnum++)
    {
      val = args[argnum];    /* using args[argnum] as argument to CHECK_NUMBER_... */
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val);

      if (FLOATP (val))
	{
	  next = XFLOAT_DATA (val);
	}
      else
	{
	  args[argnum] = val;    /* runs into a compiler bug. */
	  next = XINT (args[argnum]);
	}
      switch (SWITCH_ENUM_CAST (code))
	{
	case Aadd:
	  accum += next;
	  break;
	case Asub:
	  accum = argnum ? accum - next : nargs == 1 ? - next : next;
	  break;
	case Amult:
	  accum *= next;
	  break;
	case Adiv:
	  if (!argnum)
	    accum = next;
	  else
	    {
	      if (! IEEE_FLOATING_POINT && next == 0)
		xsignal0 (Qarith_error);
	      accum /= next;
	    }
	  break;
	case Alogand:
	case Alogior:
	case Alogxor:
	  return wrong_type_argument (Qinteger_or_marker_p, val);
	case Amax:
	  if (!argnum || isnan (next) || next > accum)
	    accum = next;
	  break;
	case Amin:
	  if (!argnum || isnan (next) || next < accum)
	    accum = next;
	  break;
	}
    }

  return make_float (accum);
}


DEFUN ("+", Fplus, Splus, 0, MANY, 0,
       doc: /* Return sum of any number of arguments, which are numbers or markers.
usage: (+ &rest NUMBERS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Aadd, nargs, args);
}

DEFUN ("-", Fminus, Sminus, 0, MANY, 0,
       doc: /* Negate number or subtract numbers or markers and return the result.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.
usage: (- &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Asub, nargs, args);
}

DEFUN ("*", Ftimes, Stimes, 0, MANY, 0,
       doc: /* Return product of any number of arguments, which are numbers or markers.
usage: (* &rest NUMBERS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amult, nargs, args);
}

DEFUN ("/", Fquo, Squo, 2, MANY, 0,
       doc: /* Return first argument divided by all the remaining arguments.
The arguments must be numbers or markers.
usage: (/ DIVIDEND DIVISOR &rest DIVISORS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  int argnum;
  for (argnum = 2; argnum < nargs; argnum++)
    if (FLOATP (args[argnum]))
      return float_arith_driver (0, 0, Adiv, nargs, args);
  return arith_driver (Adiv, nargs, args);
}

DEFUN ("%", Frem, Srem, 2, 2, 0,
       doc: /* Return remainder of X divided by Y.
Both must be integers or markers.  */)
     (x, y)
     register Lisp_Object x, y;
{
  Lisp_Object val;

  CHECK_NUMBER_COERCE_MARKER (x);
  CHECK_NUMBER_COERCE_MARKER (y);

  if (XFASTINT (y) == 0)
    xsignal0 (Qarith_error);

  XSETINT (val, XINT (x) % XINT (y));
  return val;
}

#ifndef HAVE_FMOD
double
fmod (f1, f2)
     double f1, f2;
{
  double r = f1;

  if (f2 < 0.0)
    f2 = -f2;

  /* If the magnitude of the result exceeds that of the divisor, or
     the sign of the result does not agree with that of the dividend,
     iterate with the reduced value.  This does not yield a
     particularly accurate result, but at least it will be in the
     range promised by fmod.  */
  do
    r -= f2 * floor (r / f2);
  while (f2 <= (r < 0 ? -r : r) || ((r < 0) != (f1 < 0) && ! isnan (r)));

  return r;
}
#endif /* ! HAVE_FMOD */

DEFUN ("mod", Fmod, Smod, 2, 2, 0,
       doc: /* Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers or markers.  */)
     (x, y)
     register Lisp_Object x, y;
{
  Lisp_Object val;
  EMACS_INT i1, i2;

  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (x);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (y);

  if (FLOATP (x) || FLOATP (y))
    return fmod_float (x, y);

  i1 = XINT (x);
  i2 = XINT (y);

  if (i2 == 0)
    xsignal0 (Qarith_error);

  i1 %= i2;

  /* If the "remainder" comes out with the wrong sign, fix it.  */
  if (i2 < 0 ? i1 > 0 : i1 < 0)
    i1 += i2;

  XSETINT (val, i1);
  return val;
}

DEFUN ("max", Fmax, Smax, 1, MANY, 0,
       doc: /* Return largest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.
usage: (max NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amax, nargs, args);
}

DEFUN ("min", Fmin, Smin, 1, MANY, 0,
       doc: /* Return smallest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.
usage: (min NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amin, nargs, args);
}

DEFUN ("logand", Flogand, Slogand, 0, MANY, 0,
       doc: /* Return bitwise-and of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logand &rest INTS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogand, nargs, args);
}

DEFUN ("logior", Flogior, Slogior, 0, MANY, 0,
       doc: /* Return bitwise-or of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logior &rest INTS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogior, nargs, args);
}

DEFUN ("logxor", Flogxor, Slogxor, 0, MANY, 0,
       doc: /* Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logxor &rest INTS-OR-MARKERS)  */)
     (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogxor, nargs, args);
}

DEFUN ("ash", Fash, Sash, 2, 2, 0,
       doc: /* Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, the sign bit is duplicated.  */)
     (value, count)
     register Lisp_Object value, count;
{
  register Lisp_Object val;

  CHECK_NUMBER (value);
  CHECK_NUMBER (count);

  if (XINT (count) >= BITS_PER_EMACS_INT)
    XSETINT (val, 0);
  else if (XINT (count) > 0)
    XSETINT (val, XINT (value) << XFASTINT (count));
  else if (XINT (count) <= -BITS_PER_EMACS_INT)
    XSETINT (val, XINT (value) < 0 ? -1 : 0);
  else
    XSETINT (val, XINT (value) >> -XINT (count));
  return val;
}

DEFUN ("lsh", Flsh, Slsh, 2, 2, 0,
       doc: /* Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, zeros are shifted in on the left.  */)
     (value, count)
     register Lisp_Object value, count;
{
  register Lisp_Object val;

  CHECK_NUMBER (value);
  CHECK_NUMBER (count);

  if (XINT (count) >= BITS_PER_EMACS_INT)
    XSETINT (val, 0);
  else if (XINT (count) > 0)
    XSETINT (val, (EMACS_UINT) XUINT (value) << XFASTINT (count));
  else if (XINT (count) <= -BITS_PER_EMACS_INT)
    XSETINT (val, 0);
  else
    XSETINT (val, (EMACS_UINT) XUINT (value) >> -XINT (count));
  return val;
}

DEFUN ("1+", Fadd1, Sadd1, 1, 1, 0,
       doc: /* Return NUMBER plus one.  NUMBER may be a number or a marker.
Markers are converted to integers.  */)
     (number)
     register Lisp_Object number;
{
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (number);

  if (FLOATP (number))
    return (make_float (1.0 + XFLOAT_DATA (number)));

  XSETINT (number, XINT (number) + 1);
  return number;
}

DEFUN ("1-", Fsub1, Ssub1, 1, 1, 0,
       doc: /* Return NUMBER minus one.  NUMBER may be a number or a marker.
Markers are converted to integers.  */)
     (number)
     register Lisp_Object number;
{
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (number);

  if (FLOATP (number))
    return (make_float (-1.0 + XFLOAT_DATA (number)));

  XSETINT (number, XINT (number) - 1);
  return number;
}

DEFUN ("lognot", Flognot, Slognot, 1, 1, 0,
       doc: /* Return the bitwise complement of NUMBER.  NUMBER must be an integer.  */)
     (number)
     register Lisp_Object number;
{
  CHECK_NUMBER (number);
  XSETINT (number, ~XINT (number));
  return number;
}

DEFUN ("byteorder", Fbyteorder, Sbyteorder, 0, 0, 0,
       doc: /* Return the byteorder for the machine.
Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII
lowercase l) for small endian machines.  */)
     ()
{
  unsigned i = 0x04030201;
  int order = *(char *)&i == 1 ? 108 : 66;

  return make_number (order);
}



void
syms_of_data ()
{
  Lisp_Object error_tail, arith_tail;

  Qquote = intern ("quote");
  Qlambda = intern ("lambda");
  Qsubr = intern ("subr");
  Qerror_conditions = intern ("error-conditions");
  Qerror_message = intern ("error-message");
  Qtop_level = intern ("top-level");

  Qerror = intern ("error");
  Qquit = intern ("quit");
  Qwrong_type_argument = intern ("wrong-type-argument");
  Qargs_out_of_range = intern ("args-out-of-range");
  Qvoid_function = intern ("void-function");
  Qcyclic_function_indirection = intern ("cyclic-function-indirection");
  Qcyclic_variable_indirection = intern ("cyclic-variable-indirection");
  Qvoid_variable = intern ("void-variable");
  Qsetting_constant = intern ("setting-constant");
  Qinvalid_read_syntax = intern ("invalid-read-syntax");

  Qinvalid_function = intern ("invalid-function");
  Qwrong_number_of_arguments = intern ("wrong-number-of-arguments");
  Qno_catch = intern ("no-catch");
  Qend_of_file = intern ("end-of-file");
  Qarith_error = intern ("arith-error");
  Qbeginning_of_buffer = intern ("beginning-of-buffer");
  Qend_of_buffer = intern ("end-of-buffer");
  Qbuffer_read_only = intern ("buffer-read-only");
  Qtext_read_only = intern ("text-read-only");
  Qmark_inactive = intern ("mark-inactive");

  Qlistp = intern ("listp");
  Qconsp = intern ("consp");
  Qsymbolp = intern ("symbolp");
  Qkeywordp = intern ("keywordp");
  Qintegerp = intern ("integerp");
  Qnatnump = intern ("natnump");
  Qwholenump = intern ("wholenump");
  Qstringp = intern ("stringp");
  Qarrayp = intern ("arrayp");
  Qsequencep = intern ("sequencep");
  Qbufferp = intern ("bufferp");
  Qvectorp = intern ("vectorp");
  Qchar_or_string_p = intern ("char-or-string-p");
  Qmarkerp = intern ("markerp");
  Qbuffer_or_string_p = intern ("buffer-or-string-p");
  Qinteger_or_marker_p = intern ("integer-or-marker-p");
  Qboundp = intern ("boundp");
  Qfboundp = intern ("fboundp");

  Qfloatp = intern ("floatp");
  Qnumberp = intern ("numberp");
  Qnumber_or_marker_p = intern ("number-or-marker-p");

  Qchar_table_p = intern ("char-table-p");
  Qvector_or_char_table_p = intern ("vector-or-char-table-p");

  Qsubrp = intern ("subrp");
  Qunevalled = intern ("unevalled");
  Qmany = intern ("many");

  Qcdr = intern ("cdr");

  /* Handle automatic advice activation */
  Qad_advice_info = intern ("ad-advice-info");
  Qad_activate_internal = intern ("ad-activate-internal");

  error_tail = Fcons (Qerror, Qnil);

  /* ERROR is used as a signaler for random errors for which nothing else is right */

  Fput (Qerror, Qerror_conditions,
	error_tail);
  Fput (Qerror, Qerror_message,
	build_string ("error"));

  Fput (Qquit, Qerror_conditions,
	Fcons (Qquit, Qnil));
  Fput (Qquit, Qerror_message,
	build_string ("Quit"));

  Fput (Qwrong_type_argument, Qerror_conditions,
	Fcons (Qwrong_type_argument, error_tail));
  Fput (Qwrong_type_argument, Qerror_message,
	build_string ("Wrong type argument"));

  Fput (Qargs_out_of_range, Qerror_conditions,
	Fcons (Qargs_out_of_range, error_tail));
  Fput (Qargs_out_of_range, Qerror_message,
	build_string ("Args out of range"));

  Fput (Qvoid_function, Qerror_conditions,
	Fcons (Qvoid_function, error_tail));
  Fput (Qvoid_function, Qerror_message,
	build_string ("Symbol's function definition is void"));

  Fput (Qcyclic_function_indirection, Qerror_conditions,
	Fcons (Qcyclic_function_indirection, error_tail));
  Fput (Qcyclic_function_indirection, Qerror_message,
	build_string ("Symbol's chain of function indirections contains a loop"));

  Fput (Qcyclic_variable_indirection, Qerror_conditions,
	Fcons (Qcyclic_variable_indirection, error_tail));
  Fput (Qcyclic_variable_indirection, Qerror_message,
	build_string ("Symbol's chain of variable indirections contains a loop"));

  Qcircular_list = intern ("circular-list");
  staticpro (&Qcircular_list);
  Fput (Qcircular_list, Qerror_conditions,
	Fcons (Qcircular_list, error_tail));
  Fput (Qcircular_list, Qerror_message,
	build_string ("List contains a loop"));

  Fput (Qvoid_variable, Qerror_conditions,
	Fcons (Qvoid_variable, error_tail));
  Fput (Qvoid_variable, Qerror_message,
	build_string ("Symbol's value as variable is void"));

  Fput (Qsetting_constant, Qerror_conditions,
	Fcons (Qsetting_constant, error_tail));
  Fput (Qsetting_constant, Qerror_message,
	build_string ("Attempt to set a constant symbol"));

  Fput (Qinvalid_read_syntax, Qerror_conditions,
	Fcons (Qinvalid_read_syntax, error_tail));
  Fput (Qinvalid_read_syntax, Qerror_message,
	build_string ("Invalid read syntax"));

  Fput (Qinvalid_function, Qerror_conditions,
	Fcons (Qinvalid_function, error_tail));
  Fput (Qinvalid_function, Qerror_message,
	build_string ("Invalid function"));

  Fput (Qwrong_number_of_arguments, Qerror_conditions,
	Fcons (Qwrong_number_of_arguments, error_tail));
  Fput (Qwrong_number_of_arguments, Qerror_message,
	build_string ("Wrong number of arguments"));

  Fput (Qno_catch, Qerror_conditions,
	Fcons (Qno_catch, error_tail));
  Fput (Qno_catch, Qerror_message,
	build_string ("No catch for tag"));

  Fput (Qend_of_file, Qerror_conditions,
	Fcons (Qend_of_file, error_tail));
  Fput (Qend_of_file, Qerror_message,
	build_string ("End of file during parsing"));

  arith_tail = Fcons (Qarith_error, error_tail);
  Fput (Qarith_error, Qerror_conditions,
	arith_tail);
  Fput (Qarith_error, Qerror_message,
	build_string ("Arithmetic error"));

  Fput (Qbeginning_of_buffer, Qerror_conditions,
	Fcons (Qbeginning_of_buffer, error_tail));
  Fput (Qbeginning_of_buffer, Qerror_message,
	build_string ("Beginning of buffer"));

  Fput (Qend_of_buffer, Qerror_conditions,
	Fcons (Qend_of_buffer, error_tail));
  Fput (Qend_of_buffer, Qerror_message,
	build_string ("End of buffer"));

  Fput (Qbuffer_read_only, Qerror_conditions,
	Fcons (Qbuffer_read_only, error_tail));
  Fput (Qbuffer_read_only, Qerror_message,
	build_string ("Buffer is read-only"));

  Fput (Qtext_read_only, Qerror_conditions,
	Fcons (Qtext_read_only, error_tail));
  Fput (Qtext_read_only, Qerror_message,
	build_string ("Text is read-only"));

  Qrange_error = intern ("range-error");
  Qdomain_error = intern ("domain-error");
  Qsingularity_error = intern ("singularity-error");
  Qoverflow_error = intern ("overflow-error");
  Qunderflow_error = intern ("underflow-error");

  Fput (Qdomain_error, Qerror_conditions,
	Fcons (Qdomain_error, arith_tail));
  Fput (Qdomain_error, Qerror_message,
	build_string ("Arithmetic domain error"));

  Fput (Qrange_error, Qerror_conditions,
	Fcons (Qrange_error, arith_tail));
  Fput (Qrange_error, Qerror_message,
	build_string ("Arithmetic range error"));

  Fput (Qsingularity_error, Qerror_conditions,
	Fcons (Qsingularity_error, Fcons (Qdomain_error, arith_tail)));
  Fput (Qsingularity_error, Qerror_message,
	build_string ("Arithmetic singularity error"));

  Fput (Qoverflow_error, Qerror_conditions,
	Fcons (Qoverflow_error, Fcons (Qdomain_error, arith_tail)));
  Fput (Qoverflow_error, Qerror_message,
	build_string ("Arithmetic overflow error"));

  Fput (Qunderflow_error, Qerror_conditions,
	Fcons (Qunderflow_error, Fcons (Qdomain_error, arith_tail)));
  Fput (Qunderflow_error, Qerror_message,
	build_string ("Arithmetic underflow error"));

  staticpro (&Qrange_error);
  staticpro (&Qdomain_error);
  staticpro (&Qsingularity_error);
  staticpro (&Qoverflow_error);
  staticpro (&Qunderflow_error);

  staticpro (&Qnil);
  staticpro (&Qt);
  staticpro (&Qquote);
  staticpro (&Qlambda);
  staticpro (&Qsubr);
  staticpro (&Qunbound);
  staticpro (&Qerror_conditions);
  staticpro (&Qerror_message);
  staticpro (&Qtop_level);

  staticpro (&Qerror);
  staticpro (&Qquit);
  staticpro (&Qwrong_type_argument);
  staticpro (&Qargs_out_of_range);
  staticpro (&Qvoid_function);
  staticpro (&Qcyclic_function_indirection);
  staticpro (&Qcyclic_variable_indirection);
  staticpro (&Qvoid_variable);
  staticpro (&Qsetting_constant);
  staticpro (&Qinvalid_read_syntax);
  staticpro (&Qwrong_number_of_arguments);
  staticpro (&Qinvalid_function);
  staticpro (&Qno_catch);
  staticpro (&Qend_of_file);
  staticpro (&Qarith_error);
  staticpro (&Qbeginning_of_buffer);
  staticpro (&Qend_of_buffer);
  staticpro (&Qbuffer_read_only);
  staticpro (&Qtext_read_only);
  staticpro (&Qmark_inactive);

  staticpro (&Qlistp);
  staticpro (&Qconsp);
  staticpro (&Qsymbolp);
  staticpro (&Qkeywordp);
  staticpro (&Qintegerp);
  staticpro (&Qnatnump);
  staticpro (&Qwholenump);
  staticpro (&Qstringp);
  staticpro (&Qarrayp);
  staticpro (&Qsequencep);
  staticpro (&Qbufferp);
  staticpro (&Qvectorp);
  staticpro (&Qchar_or_string_p);
  staticpro (&Qmarkerp);
  staticpro (&Qbuffer_or_string_p);
  staticpro (&Qinteger_or_marker_p);
  staticpro (&Qfloatp);
  staticpro (&Qnumberp);
  staticpro (&Qnumber_or_marker_p);
  staticpro (&Qchar_table_p);
  staticpro (&Qvector_or_char_table_p);
  staticpro (&Qsubrp);
  staticpro (&Qmany);
  staticpro (&Qunevalled);

  staticpro (&Qboundp);
  staticpro (&Qfboundp);
  staticpro (&Qcdr);
  staticpro (&Qad_advice_info);
  staticpro (&Qad_activate_internal);

  /* Types that type-of returns.  */
  Qinteger = intern ("integer");
  Qsymbol = intern ("symbol");
  Qstring = intern ("string");
  Qcons = intern ("cons");
  Qmarker = intern ("marker");
  Qoverlay = intern ("overlay");
  Qfloat = intern ("float");
  Qwindow_configuration = intern ("window-configuration");
  Qprocess = intern ("process");
  Qwindow = intern ("window");
  /* Qsubr = intern ("subr"); */
  Qcompiled_function = intern ("compiled-function");
  Qbuffer = intern ("buffer");
  Qframe = intern ("frame");
  Qvector = intern ("vector");
  Qchar_table = intern ("char-table");
  Qbool_vector = intern ("bool-vector");
  Qhash_table = intern ("hash-table");

  staticpro (&Qinteger);
  staticpro (&Qsymbol);
  staticpro (&Qstring);
  staticpro (&Qcons);
  staticpro (&Qmarker);
  staticpro (&Qoverlay);
  staticpro (&Qfloat);
  staticpro (&Qwindow_configuration);
  staticpro (&Qprocess);
  staticpro (&Qwindow);
  /* staticpro (&Qsubr); */
  staticpro (&Qcompiled_function);
  staticpro (&Qbuffer);
  staticpro (&Qframe);
  staticpro (&Qvector);
  staticpro (&Qchar_table);
  staticpro (&Qbool_vector);
  staticpro (&Qhash_table);

  defsubr (&Sindirect_variable);
  defsubr (&Sinteractive_form);
  defsubr (&Seq);
  defsubr (&Snull);
  defsubr (&Stype_of);
  defsubr (&Slistp);
  defsubr (&Snlistp);
  defsubr (&Sconsp);
  defsubr (&Satom);
  defsubr (&Sintegerp);
  defsubr (&Sinteger_or_marker_p);
  defsubr (&Snumberp);
  defsubr (&Snumber_or_marker_p);
  defsubr (&Sfloatp);
  defsubr (&Snatnump);
  defsubr (&Ssymbolp);
  defsubr (&Skeywordp);
  defsubr (&Sstringp);
  defsubr (&Smultibyte_string_p);
  defsubr (&Svectorp);
  defsubr (&Schar_table_p);
  defsubr (&Svector_or_char_table_p);
  defsubr (&Sbool_vector_p);
  defsubr (&Sarrayp);
  defsubr (&Ssequencep);
  defsubr (&Sbufferp);
  defsubr (&Smarkerp);
  defsubr (&Ssubrp);
  defsubr (&Sbyte_code_function_p);
  defsubr (&Schar_or_string_p);
  defsubr (&Scar);
  defsubr (&Scdr);
  defsubr (&Scar_safe);
  defsubr (&Scdr_safe);
  defsubr (&Ssetcar);
  defsubr (&Ssetcdr);
  defsubr (&Ssymbol_function);
  defsubr (&Sindirect_function);
  defsubr (&Ssymbol_plist);
  defsubr (&Ssymbol_name);
  defsubr (&Smakunbound);
  defsubr (&Sfmakunbound);
  defsubr (&Sboundp);
  defsubr (&Sfboundp);
  defsubr (&Sfset);
  defsubr (&Sdefalias);
  defsubr (&Ssetplist);
  defsubr (&Ssymbol_value);
  defsubr (&Sset);
  defsubr (&Sdefault_boundp);
  defsubr (&Sdefault_value);
  defsubr (&Sset_default);
  defsubr (&Ssetq_default);
  defsubr (&Smake_variable_buffer_local);
  defsubr (&Smake_local_variable);
  defsubr (&Skill_local_variable);
  defsubr (&Smake_variable_frame_local);
  defsubr (&Slocal_variable_p);
  defsubr (&Slocal_variable_if_set_p);
  defsubr (&Svariable_binding_locus);
#if 0                           /* XXX Remove this. --lorentey */
  defsubr (&Sterminal_local_value);
  defsubr (&Sset_terminal_local_value);
#endif
  defsubr (&Saref);
  defsubr (&Saset);
  defsubr (&Snumber_to_string);
  defsubr (&Sstring_to_number);
  defsubr (&Seqlsign);
  defsubr (&Slss);
  defsubr (&Sgtr);
  defsubr (&Sleq);
  defsubr (&Sgeq);
  defsubr (&Sneq);
  defsubr (&Szerop);
  defsubr (&Splus);
  defsubr (&Sminus);
  defsubr (&Stimes);
  defsubr (&Squo);
  defsubr (&Srem);
  defsubr (&Smod);
  defsubr (&Smax);
  defsubr (&Smin);
  defsubr (&Slogand);
  defsubr (&Slogior);
  defsubr (&Slogxor);
  defsubr (&Slsh);
  defsubr (&Sash);
  defsubr (&Sadd1);
  defsubr (&Ssub1);
  defsubr (&Slognot);
  defsubr (&Sbyteorder);
  defsubr (&Ssubr_arity);
  defsubr (&Ssubr_name);

  XSYMBOL (Qwholenump)->function = XSYMBOL (Qnatnump)->function;

  DEFVAR_LISP ("most-positive-fixnum", &Vmost_positive_fixnum,
	       doc: /* The largest value that is representable in a Lisp integer.  */);
  Vmost_positive_fixnum = make_number (MOST_POSITIVE_FIXNUM);
  XSYMBOL (intern ("most-positive-fixnum"))->constant = 1;

  DEFVAR_LISP ("most-negative-fixnum", &Vmost_negative_fixnum,
	       doc: /* The smallest value that is representable in a Lisp integer.  */);
  Vmost_negative_fixnum = make_number (MOST_NEGATIVE_FIXNUM);
  XSYMBOL (intern ("most-negative-fixnum"))->constant = 1;
}

SIGTYPE
arith_error (signo)
     int signo;
{
#if defined(USG) && !defined(POSIX_SIGNALS)
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, arith_error);
#endif /* USG */
#ifdef VMS
  /* VMS systems are like USG.  */
  signal (signo, arith_error);
#endif /* VMS */
#ifdef BSD4_1
  sigrelse (SIGFPE);
#else /* not BSD4_1 */
  sigsetmask (SIGEMPTYMASK);
#endif /* not BSD4_1 */

  SIGNAL_THREAD_CHECK (signo);
  xsignal0 (Qarith_error);
}

void
init_data ()
{
  /* Don't do this if just dumping out.
     We don't want to call `signal' in this case
     so that we don't have trouble with dumping
     signal-delivering routines in an inconsistent state.  */
#ifndef CANNOT_DUMP
  if (!initialized)
    return;
#endif /* CANNOT_DUMP */
  signal (SIGFPE, arith_error);

#ifdef uts
  signal (SIGEMT, arith_error);
#endif /* uts */
}

/* arch-tag: 25879798-b84d-479a-9c89-7d148e2109f7
   (do not change this comment) */
