/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
   Copyright (C) 1985,86,88,93,94,95,97,98,99, 2000, 2001
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
#include <signal.h>
#include <stdio.h>
#include "lisp.h"
#include "puresize.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "frame.h"
#include "syssignal.h"

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

static Lisp_Object Qinteger, Qsymbol, Qstring, Qcons, Qmarker, Qoverlay;
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
  Fsignal (Qcircular_list, list);
}


Lisp_Object
wrong_type_argument (predicate, value)
     register Lisp_Object predicate, value;
{
  register Lisp_Object tem;
  do
    {
      /* If VALUE is not even a valid Lisp object, abort here
	 where we can get a backtrace showing where it came from.  */
      if ((unsigned int) XGCTYPE (value) >= Lisp_Type_Limit)
	abort ();

      value = Fsignal (Qwrong_type_argument, Fcons (predicate, Fcons (value, Qnil)));
      tem = call1 (predicate, value);
    }
  while (NILP (tem));
  return value;
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
  while (1)
    Fsignal (Qargs_out_of_range, Fcons (a1, Fcons (a2, Qnil)));
}

void
args_out_of_range_3 (a1, a2, a3)
     Lisp_Object a1, a2, a3;
{
  while (1)
    Fsignal (Qargs_out_of_range, Fcons (a1, Fcons (a2, Fcons (a3, Qnil))));
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
  switch (XGCTYPE (object))
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
      if (GC_WINDOW_CONFIGURATIONP (object))
	return Qwindow_configuration;
      if (GC_PROCESSP (object))
	return Qprocess;
      if (GC_WINDOWP (object))
	return Qwindow;
      if (GC_SUBRP (object))
	return Qsubr;
      if (GC_COMPILEDP (object))
	return Qcompiled_function;
      if (GC_BUFFERP (object))
	return Qbuffer;
      if (GC_CHAR_TABLE_P (object))
	return Qchar_table;
      if (GC_BOOL_VECTOR_P (object))
	return Qbool_vector;
      if (GC_FRAMEP (object))
	return Qframe;
      if (GC_HASH_TABLE_P (object))
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
       doc: /* Return t if OBJECT is a list.  This includes nil.  */)
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
      && XSYMBOL (object)->name->data[0] == ':'
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
  if (VECTORP (object) || STRINGP (object)
      || CHAR_TABLE_P (object) || BOOL_VECTOR_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
       doc: /* Return t if OBJECT is a sequence (list or array).  */)
     (object)
     register Lisp_Object object;
{
  if (CONSP (object) || NILP (object) || VECTORP (object) || STRINGP (object)
      || CHAR_TABLE_P (object) || BOOL_VECTOR_P (object))
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
Error if arg is not nil and not a cons cell.  See also `car-safe'.  */)
     (list)
     register Lisp_Object list;
{
  while (1)
    {
      if (CONSP (list))
	return XCAR (list);
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
       doc: /* Return the car of OBJECT if it is a cons cell, or else nil.  */)
     (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return XCAR (object);
  else
    return Qnil;
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
       doc: /* Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.  */)
     (list)
     register Lisp_Object list;
{
  while (1)
    {
      if (CONSP (list))
	return XCDR (list);
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
       doc: /* Return the cdr of OBJECT if it is a cons cell, or else nil.  */)
     (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return XCDR (object);
  else
    return Qnil;
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
       doc: /* Set the car of CELL to be NEWCAR.  Returns NEWCAR.  */)
     (cell, newcar)
     register Lisp_Object cell, newcar;
{
  if (!CONSP (cell))
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XSETCAR (cell, newcar);
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
       doc: /* Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.  */)
     (cell, newcdr)
     register Lisp_Object cell, newcdr;
{
  if (!CONSP (cell))
    cell = wrong_type_argument (Qconsp, cell);

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

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
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
       doc: /* Make SYMBOL's value be void.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  if (XSYMBOL (symbol)->constant)
    return Fsignal (Qsetting_constant, Fcons (symbol, Qnil));
  Fset (symbol, Qunbound);
  return symbol;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's function definition be void.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    return Fsignal (Qsetting_constant, Fcons (symbol, Qnil));
  XSYMBOL (symbol)->function = Qunbound;
  return symbol;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
       doc: /* Return SYMBOL's function definition.  Error if that is void.  */)
     (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol);
  if (EQ (XSYMBOL (symbol)->function, Qunbound))
    return Fsignal (Qvoid_function, Fcons (symbol, Qnil));
  return XSYMBOL (symbol)->function;
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
  XSETSTRING (name, XSYMBOL (symbol)->name);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.  */)
     (symbol, definition)
     register Lisp_Object symbol, definition;
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    return Fsignal (Qsetting_constant, Fcons (symbol, Qnil));
  if (!NILP (Vautoload_queue) && !EQ (XSYMBOL (symbol)->function, Qunbound))
    Vautoload_queue = Fcons (Fcons (symbol, XSYMBOL (symbol)->function),
			     Vautoload_queue);
  XSYMBOL (symbol)->function = definition;
  /* Handle automatic advice activation */
  if (CONSP (XSYMBOL (symbol)->plist) && !NILP (Fget (symbol, Qad_advice_info)))
    {
      call2 (Qad_activate_internal, symbol, Qnil);
      definition = XSYMBOL (symbol)->function;
    }
  return definition;
}

DEFUN ("defalias", Fdefalias, Sdefalias, 2, 2, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
Associates the function with the current load file, if any.  */)
     (symbol, definition)
     register Lisp_Object symbol, definition;
{
  definition = Ffset (symbol, definition);
  LOADHIST_ATTACH (symbol);
  return definition;
}

DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
       doc: /* Set SYMBOL's property list to NEWVAL, and return NEWVAL.  */)
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
  if (!SUBRP (subr))
    wrong_type_argument (Qsubrp, subr);
  minargs = XSUBR (subr)->min_args;
  maxargs = XSUBR (subr)->max_args;
  if (maxargs == MANY)
    return Fcons (make_number (minargs), Qmany);
  else if (maxargs == UNEVALLED)
    return Fcons (make_number (minargs), Qunevalled);
  else
    return Fcons (make_number (minargs), make_number (maxargs));
}

DEFUN ("subr-interactive-form", Fsubr_interactive_form, Ssubr_interactive_form, 1, 1, 0,
       doc: /* Return the interactive form of SUBR or nil if none.
SUBR must be a built-in function.  Value, if non-nil, is a list
\(interactive SPEC).  */)
     (subr)
     Lisp_Object subr;
{
  if (!SUBRP (subr))
    wrong_type_argument (Qsubrp, subr);
  if (XSUBR (subr)->prompt)
    return list2 (Qinteractive, build_string (XSUBR (subr)->prompt));
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
	Fsignal (Qcyclic_variable_indirection, Fcons (symbol, Qnil));
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
  int offset;
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
	offset = XBUFFER_OBJFWD (valcontents)->offset;
	return PER_BUFFER_VALUE (current_buffer, offset);

      case Lisp_Misc_Kboard_Objfwd:
	offset = XKBOARD_OBJFWD (valcontents)->offset;
	return *(Lisp_Object *)(offset + (char *)current_kboard);
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
	  if (*XINTFWD (valcontents)->intvar != XINT (newval))
	    error ("Value out of range for variable `%s'",
		   XSYMBOL (symbol)->name->data);
	  break;

	case Lisp_Misc_Boolfwd:
	  *XBOOLFWD (valcontents)->boolvar = NILP (newval) ? 0 : 1;
	  break;

	case Lisp_Misc_Objfwd:
	  *XOBJFWD (valcontents)->objvar = newval;
	  break;

	case Lisp_Misc_Buffer_Objfwd:
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    Lisp_Object type;

	    type = PER_BUFFER_TYPE (offset);
	    if (XINT (type) == -1)
	      error ("Variable %s is read-only", XSYMBOL (symbol)->name->data);

	    if (! NILP (type) && ! NILP (newval)
		&& XTYPE (newval) != XINT (type))
	      buffer_slot_type_mismatch (offset);

	    if (buf == NULL)
	      buf = current_buffer;
	    PER_BUFFER_VALUE (buf, offset) = newval;
	  }
	  break;

	case Lisp_Misc_Kboard_Objfwd:
	  {
	    char *base = (char *) current_kboard;
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
      if (BUFFER_LOCAL_VALUEP (valcontents)
	  || SOME_BUFFER_LOCAL_VALUEP (valcontents))
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
  Lisp_Object valcontents, cdr;
  
  valcontents = SYMBOL_VALUE (symbol);
  if (!BUFFER_LOCAL_VALUEP (valcontents)
      && !SOME_BUFFER_LOCAL_VALUEP (valcontents))
    abort ();
  cdr = XBUFFER_LOCAL_VALUE (valcontents)->cdr;

  /* Unload the previously loaded binding.  */
  Fsetcdr (XCAR (cdr),
	   do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->realvalue));
  
  /* Select the global binding in the symbol.  */
  XSETCAR (cdr, cdr);
  store_symval_forwarding (symbol, valcontents, XCDR (cdr), NULL);

  /* Indicate that the global binding is set up now.  */
  XBUFFER_LOCAL_VALUE (valcontents)->frame = Qnil;
  XBUFFER_LOCAL_VALUE (valcontents)->buffer = Qnil;
  XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame = 0;
  XBUFFER_LOCAL_VALUE (valcontents)->found_for_buffer = 0;
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

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    valcontents = swap_in_symval_forwarding (symbol, valcontents);

  if (MISCP (valcontents))
    {
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
	  return *(Lisp_Object *)(XKBOARD_OBJFWD (valcontents)->offset
				  + (char *)current_kboard);
	}
    }

  return valcontents;
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0,
       doc: /* Return SYMBOL's value.  Error if that is void.  */)
     (symbol)
     Lisp_Object symbol;
{
  Lisp_Object val;

  val = find_symbol_value (symbol);
  if (EQ (val, Qunbound))
    return Fsignal (Qvoid_variable, Fcons (symbol, Qnil));
  else
    return val;
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
  struct specbinding *p;

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
    return Fsignal (Qsetting_constant, Fcons (symbol, Qnil));

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
  else if (BUFFER_LOCAL_VALUEP (valcontents)
	   || SOME_BUFFER_LOCAL_VALUEP (valcontents))
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
	  || (BUFFER_LOCAL_VALUEP (valcontents)
	      && EQ (XCAR (current_alist_element),
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

	      /* If the variable is a Lisp_Some_Buffer_Local_Value,
		 or if this is `let' rather than `set',
		 make CURRENT-ALIST-ELEMENT point to itself,
		 indicating that we're seeing the default value.
		 Likewise if the variable has been let-bound
		 in the current buffer.  */
	      if (bindflag || SOME_BUFFER_LOCAL_VALUEP (valcontents)
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
		  tem1 = Fcons (symbol, Fcdr (current_alist_element));
		  buf->local_var_alist
		    = Fcons (tem1, buf->local_var_alist);
		}
	    }

	  /* Record which binding is now loaded.  */
	  XSETCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr,
		   tem1);

	  /* Set `buffer' and `frame' slots for thebinding now loaded.  */
	  XSETBUFFER (XBUFFER_LOCAL_VALUE (valcontents)->buffer, buf);
	  XBUFFER_LOCAL_VALUE (valcontents)->frame = selected_frame;
	}
      innercontents = XBUFFER_LOCAL_VALUE (valcontents)->realvalue;
    }

  /* If storing void (making the symbol void), forward only through
     buffer-local indicator, not through Lisp_Objfwd, etc.  */
  if (voide)
    store_symval_forwarding (symbol, Qnil, newval, buf);
  else
    store_symval_forwarding (symbol, innercontents, newval, buf);

  /* If we just set a variable whose current binding is frame-local,
     store the new value in the frame parameter too.  */

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      /* What binding is loaded right now?  */
      current_alist_element
	= XCAR (XBUFFER_LOCAL_VALUE (valcontents)->cdr);

      /* If the current buffer is not the buffer whose binding is
	 loaded, or if there may be frame-local bindings and the frame
	 isn't the right one, or if it's a Lisp_Buffer_Local_Value and
	 the default binding is loaded, the loaded binding may be the
	 wrong one.  */
      if (XBUFFER_LOCAL_VALUE (valcontents)->found_for_frame)
	XSETCDR (current_alist_element, newval);
    }

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
  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
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
  if (EQ (value, Qunbound))
    return Fsignal (Qvoid_variable, Fcons (symbol, Qnil));
  return value;
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
       doc: /* Set SYMBOL's default value to VAL.  SYMBOL and VAL are evaluated.
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

  if (!BUFFER_LOCAL_VALUEP (valcontents)
      && !SOME_BUFFER_LOCAL_VALUEP (valcontents))
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

DEFUN ("setq-default", Fsetq_default, Ssetq_default, 2, UNEVALLED, 0,
       doc: /* Set the default value of variable VAR to VALUE.
VAR, the variable name, is literal (not evaluated);
VALUE is an expression and it is evaluated.
The default value of a variable is seen in buffers
that do not have their own values for the variable.

More generally, you can use multiple variables and values, as in
  (setq-default SYMBOL VALUE SYMBOL VALUE...)
This sets each SYMBOL's default value to the corresponding VALUE.
The VALUE for the Nth SYMBOL can refer to the new default values
of previous SYMs.
usage: (setq-default SYMBOL VALUE [SYMBOL VALUE...])  */)
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
      symbol = Fcar (args_left);
      Fset_default (symbol, val);
      args_left = Fcdr (Fcdr (args_left));
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
does not make the variable buffer-local.

The function `default-value' gets the default value and `set-default' sets it.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents, newval;

  CHECK_SYMBOL (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (EQ (variable, Qnil) || EQ (variable, Qt) || KBOARD_OBJFWDP (valcontents))
    error ("Symbol %s may not be buffer-local", XSYMBOL (variable)->name->data);

  if (BUFFER_LOCAL_VALUEP (valcontents) || BUFFER_OBJFWDP (valcontents))
    return variable;
  if (SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      XMISCTYPE (SYMBOL_VALUE (variable)) = Lisp_Misc_Buffer_Local_Value;
      return variable;
    }
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
  return variable;
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
       1, 1, "vMake Local Variable: ",
       doc: /* Make VARIABLE have a separate value in the current buffer.
Other buffers will continue to share a common default value.
\(The buffer-local value of VARIABLE starts out as the same value
VARIABLE previously had.  If VARIABLE was void, it remains void.\)
See also `make-variable-buffer-local'.

If the variable is already arranged to become local when set,
this function causes a local value to exist for this buffer,
just as setting the variable would do.

This function returns VARIABLE, and therefore
  (set (make-local-variable 'VARIABLE) VALUE-EXP)
works.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (EQ (variable, Qnil) || EQ (variable, Qt) || KBOARD_OBJFWDP (valcontents))
    error ("Symbol %s may not be buffer-local", XSYMBOL (variable)->name->data);

  if (BUFFER_LOCAL_VALUEP (valcontents) || BUFFER_OBJFWDP (valcontents))
    {
      tem = Fboundp (variable);

      /* Make sure the symbol has a local value in this particular buffer,
	 by setting it to the same value it already has.  */
      Fset (variable, (EQ (tem, Qt) ? Fsymbol_value (variable) : Qunbound));
      return variable;
    }
  /* Make sure symbol is set up to hold per-buffer values.  */
  if (!SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object newval;
      tem = Fcons (Qnil, do_symval_forwarding (valcontents));
      XSETCAR (tem, tem);
      newval = allocate_misc ();
      XMISCTYPE (newval) = Lisp_Misc_Some_Buffer_Local_Value;
      XBUFFER_LOCAL_VALUE (newval)->realvalue = SYMBOL_VALUE (variable);
      XBUFFER_LOCAL_VALUE (newval)->buffer = Qnil;
      XBUFFER_LOCAL_VALUE (newval)->frame = Qnil;
      XBUFFER_LOCAL_VALUE (newval)->found_for_buffer = 0;
      XBUFFER_LOCAL_VALUE (newval)->found_for_frame = 0;
      XBUFFER_LOCAL_VALUE (newval)->check_frame = 0;
      XBUFFER_LOCAL_VALUE (newval)->cdr = tem;
      SET_SYMBOL_VALUE (variable, newval);;
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
From now on the default value will apply in this buffer.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (variable);

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

  if (!BUFFER_LOCAL_VALUEP (valcontents)
      && !SOME_BUFFER_LOCAL_VALUEP (valcontents))
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
    Lisp_Object *pvalbuf;
    valcontents = SYMBOL_VALUE (variable);
    pvalbuf = &XBUFFER_LOCAL_VALUE (valcontents)->buffer;
    if (current_buffer == XBUFFER (*pvalbuf))
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
When a frame-local binding exists in the current frame,
it is in effect whenever the current buffer has no buffer-local binding.
A frame-local binding is actual a frame parameter value;
thus, any given frame has a local binding for VARIABLE
if it has a value for the frame parameter named VARIABLE.
See `modify-frame-parameters'.  */)
     (variable)
     register Lisp_Object variable;
{
  register Lisp_Object tem, valcontents, newval;

  CHECK_SYMBOL (variable);

  valcontents = SYMBOL_VALUE (variable);
  if (EQ (variable, Qnil) || EQ (variable, Qt) || KBOARD_OBJFWDP (valcontents)
      || BUFFER_OBJFWDP (valcontents))
    error ("Symbol %s may not be frame-local", XSYMBOL (variable)->name->data);

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      XBUFFER_LOCAL_VALUE (valcontents)->check_frame = 1;
      return variable;
    }

  if (EQ (valcontents, Qunbound))
    SET_SYMBOL_VALUE (variable, Qnil);
  tem = Fcons (Qnil, Fsymbol_value (variable));
  XSETCAR (tem, tem);
  newval = allocate_misc ();
  XMISCTYPE (newval) = Lisp_Misc_Some_Buffer_Local_Value;
  XBUFFER_LOCAL_VALUE (newval)->realvalue = SYMBOL_VALUE (variable);
  XBUFFER_LOCAL_VALUE (newval)->buffer = Qnil;
  XBUFFER_LOCAL_VALUE (newval)->frame = Qnil;
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

  valcontents = SYMBOL_VALUE (variable);
  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object tail, elt;

      variable = indirect_variable (variable);
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
       doc: /* Non-nil if VARIABLE will be local in buffer BUFFER if it is set there.
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

  valcontents = SYMBOL_VALUE (variable);

  /* This means that make-variable-buffer-local was done.  */
  if (BUFFER_LOCAL_VALUEP (valcontents))
    return Qt;
  /* All these slots become local if they are set.  */
  if (BUFFER_OBJFWDP (valcontents))
    return Qt;
  if (SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object tail, elt;
      for (tail = buf->local_var_alist; CONSP (tail); tail = XCDR (tail))
	{
	  elt = XCAR (tail);
	  if (EQ (variable, XCAR (elt)))
	    return Qt;
	}
    }
  return Qnil;
}

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
	Fsignal (Qcyclic_function_indirection, Fcons (object, Qnil));
    }

  return hare;
}

DEFUN ("indirect-function", Findirect_function, Sindirect_function, 1, 1, 0,
       doc: /* Return the function at the end of OBJECT's function chain.
If OBJECT is a symbol, follow all function indirections and return the final
function binding.
If OBJECT is not a symbol, just return it.
Signal a void-function error if the final symbol is unbound.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.  */)
     (object)
     register Lisp_Object object;
{
  Lisp_Object result;

  result = indirect_function (object);

  if (EQ (result, Qunbound))
    return Fsignal (Qvoid_function, Fcons (object, Qnil));
  return result;
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

      if (idxval < 0 || idxval >= XSTRING (array)->size)
	args_out_of_range (array, idx);
      if (! STRING_MULTIBYTE (array))
	return make_number ((unsigned char) XSTRING (array)->data[idxval]);
      idxval_byte = string_char_to_byte (array, idxval);

      c = STRING_CHAR (&XSTRING (array)->data[idxval_byte],
		       STRING_BYTES (XSTRING (array)) - idxval_byte);
      return make_number (c);
    }
  else if (BOOL_VECTOR_P (array))
    {
      int val;

      if (idxval < 0 || idxval >= XBOOL_VECTOR (array)->size)
	args_out_of_range (array, idx);

      val = (unsigned char) XBOOL_VECTOR (array)->data[idxval / BITS_PER_CHAR];
      return (val & (1 << (idxval % BITS_PER_CHAR)) ? Qt : Qnil);
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

/* Don't use alloca for relocating string data larger than this, lest
   we overflow their stack.  The value is the same as what used in
   fns.c for base64 handling.  */
#define MAX_ALLOCA 16*1024

DEFUN ("aset", Faset, Saset, 3, 3, 0,
       doc: /* Store into the element of ARRAY at index IDX the value NEWELT.
ARRAY may be a vector, a string, a char-table or a bool-vector.
IDX starts at 0.  */)
     (array, idx, newelt)
     register Lisp_Object array;
     Lisp_Object idx, newelt;
{
  register int idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  if (!VECTORP (array) && !STRINGP (array) && !BOOL_VECTOR_P (array)
      && ! CHAR_TABLE_P (array))
    array = wrong_type_argument (Qarrayp, array);
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

      val = (unsigned char) XBOOL_VECTOR (array)->data[idxval / BITS_PER_CHAR];

      if (! NILP (newelt))
	val |= 1 << (idxval % BITS_PER_CHAR);
      else
	val &= ~(1 << (idxval % BITS_PER_CHAR));
      XBOOL_VECTOR (array)->data[idxval / BITS_PER_CHAR] = val;
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      CHAR_TABLE_SET (array, idxval, newelt);
    }
  else if (STRING_MULTIBYTE (array))
    {
      int idxval_byte, prev_bytes, new_bytes;
      unsigned char workbuf[MAX_MULTIBYTE_LENGTH], *p0 = workbuf, *p1;

      if (idxval < 0 || idxval >= XSTRING (array)->size)
	args_out_of_range (array, idx);
      CHECK_NUMBER (newelt);

      idxval_byte = string_char_to_byte (array, idxval);
      p1 = &XSTRING (array)->data[idxval_byte];
      PARSE_MULTIBYTE_SEQ (p1, nbytes - idxval_byte, prev_bytes);
      new_bytes = CHAR_STRING (XINT (newelt), p0);
      if (prev_bytes != new_bytes)
	{
	  /* We must relocate the string data.  */
	  int nchars = XSTRING (array)->size;
	  int nbytes = STRING_BYTES (XSTRING (array));
	  unsigned char *str;

	  str = (nbytes <= MAX_ALLOCA
		 ? (unsigned char *) alloca (nbytes)
		 : (unsigned char *) xmalloc (nbytes));
	  bcopy (XSTRING (array)->data, str, nbytes);
	  allocate_string_data (XSTRING (array), nchars,
				nbytes + new_bytes - prev_bytes);
	  bcopy (str, XSTRING (array)->data, idxval_byte);
	  p1 = XSTRING (array)->data + idxval_byte;
	  bcopy (str + idxval_byte + prev_bytes, p1 + new_bytes,
		 nbytes - (idxval_byte + prev_bytes));
	  if (nbytes > MAX_ALLOCA)
	    xfree (str);
	  clear_string_char_byte_cache ();
	}
      while (new_bytes--)
	*p1++ = *p0++;
    }
  else
    {
      if (idxval < 0 || idxval >= XSTRING (array)->size)
	args_out_of_range (array, idx);
      CHECK_NUMBER (newelt);

      if (XINT (newelt) < 0 || SINGLE_BYTE_CHAR_P (XINT (newelt)))
	XSTRING (array)->data[idxval] = XINT (newelt);
      else
	{
	  /* We must relocate the string data while converting it to
	     multibyte.  */
	  int idxval_byte, prev_bytes, new_bytes;
	  unsigned char workbuf[MAX_MULTIBYTE_LENGTH], *p0 = workbuf, *p1;
	  unsigned char *origstr = XSTRING (array)->data, *str;
	  int nchars, nbytes;

	  nchars = XSTRING (array)->size;
	  nbytes = idxval_byte = count_size_as_multibyte (origstr, idxval);
	  nbytes += count_size_as_multibyte (origstr + idxval,
					     nchars - idxval);
	  str = (nbytes <= MAX_ALLOCA
		 ? (unsigned char *) alloca (nbytes)
		 : (unsigned char *) xmalloc (nbytes));
	  copy_text (XSTRING (array)->data, str, nchars, 0, 1);
	  PARSE_MULTIBYTE_SEQ (str + idxval_byte, nbytes - idxval_byte,
			       prev_bytes);
	  new_bytes = CHAR_STRING (XINT (newelt), p0);
	  allocate_string_data (XSTRING (array), nchars,
				nbytes + new_bytes - prev_bytes);
	  bcopy (str, XSTRING (array)->data, idxval_byte);
	  p1 = XSTRING (array)->data + idxval_byte;
	  while (new_bytes--)
	    *p1++ = *p0++;
	  bcopy (str + idxval_byte + prev_bytes, p1,
		 nbytes - (idxval_byte + prev_bytes));
	  if (nbytes > MAX_ALLOCA)
	    xfree (str);
	  clear_string_char_byte_cache ();
	}
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

/* Convert between long values and pairs of Lisp integers.  */

Lisp_Object
long_to_cons (i)
     unsigned long i;
{
  unsigned int top = i >> 16;
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
       doc: /* Convert NUMBER to a string by printing it in decimal.
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
    sprintf (buffer, "%d", XINT (number));
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
       doc: /* Convert STRING to a number by parsing it as a decimal number.
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
	Fsignal (Qargs_out_of_range, Fcons (base, Qnil));
    }

  /* Skip any whitespace at the front of the number.  Some versions of
     atoi do this anyway, so we might as well make Emacs lisp consistent.  */
  p = XSTRING (string)->data;
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
		Fsignal (Qarith_error, Qnil);
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
		Fsignal (Qarith_error, Qnil);
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
       doc: /* Negate number or subtract numbers or markers.
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
    Fsignal (Qarith_error, Qnil);

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
    Fsignal (Qarith_error, Qnil);

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
usage: (logxor &rest INTS-OR-MARKERS) */)
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
In this case,  zeros are shifted in on the left.  */)
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
  defsubr (&Ssubr_interactive_form);
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
  defsubr (&Ssubr_arity);

  XSYMBOL (Qwholenump)->function = XSYMBOL (Qnatnump)->function;

  DEFVAR_LISP ("most-positive-fixnum", &Vmost_positive_fixnum,
	       doc: /* The largest value that is representable in a Lisp integer.  */);
  Vmost_positive_fixnum = make_number (MOST_POSITIVE_FIXNUM);
  
  DEFVAR_LISP ("most-negative-fixnum", &Vmost_negative_fixnum,
	       doc: /* The smallest value that is representable in a Lisp integer.  */);
  Vmost_negative_fixnum = make_number (MOST_NEGATIVE_FIXNUM);
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

  Fsignal (Qarith_error, Qnil);
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
