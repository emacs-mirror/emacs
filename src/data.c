/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 86, 88, 93, 94, 95 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <signal.h>

#include <config.h>
#include "lisp.h"
#include "puresize.h"

#ifndef standalone
#include "buffer.h"
#endif

#include "syssignal.h"

#ifdef MSDOS
/* These are redefined (correctly, but differently) in values.h.  */
#undef INTBITS
#undef LONGBITS
#undef SHORTBITS
#endif

#ifdef LISP_FLOAT_TYPE

#ifdef STDC_HEADERS
#include <stdlib.h>
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
#endif /* LISP_FLOAT_TYPE */

#if !defined (atof)
extern double atof ();
#endif /* !atof */

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qsubr, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
Lisp_Object Qvoid_variable, Qvoid_function, Qcyclic_function_indirection;
Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
Lisp_Object Qend_of_file, Qarith_error, Qmark_inactive;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qintegerp, Qnatnump, Qwholenump, Qsymbolp, Qlistp, Qconsp;
Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qvectorp;
Lisp_Object Qbuffer_or_string_p;
Lisp_Object Qboundp, Qfboundp;

Lisp_Object Qcdr;
Lisp_Object Qad_advice_info, Qad_activate;

Lisp_Object Qrange_error, Qdomain_error, Qsingularity_error;
Lisp_Object Qoverflow_error, Qunderflow_error;

#ifdef LISP_FLOAT_TYPE
Lisp_Object Qfloatp;
Lisp_Object Qnumberp, Qnumber_or_marker_p;
#endif

static Lisp_Object Qinteger, Qsymbol, Qstring, Qcons, Qmarker, Qoverlay;
static Lisp_Object Qfloat, Qwindow_configuration, Qprocess, Qwindow;
static Lisp_Object Qcompiled_function, Qbuffer, Qframe, Qvector;

static Lisp_Object swap_in_symval_forwarding ();

Lisp_Object
wrong_type_argument (predicate, value)
     register Lisp_Object predicate, value;
{
  register Lisp_Object tem;
  do
    {
      if (!EQ (Vmocklisp_arguments, Qt))
	{
	 if (STRINGP (value) &&
	     (EQ (predicate, Qintegerp) || EQ (predicate, Qinteger_or_marker_p)))
	   return Fstring_to_number (value);
	 if (INTEGERP (value) && EQ (predicate, Qstringp))
	   return Fnumber_to_string (value);
	}

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

Lisp_Object
make_number (num)
     int num;
{
  register Lisp_Object val;
  XSETINT (val, num);
  return val;
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
  "T if the two args are the same Lisp object.")
  (obj1, obj2)
     Lisp_Object obj1, obj2;
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}

DEFUN ("null", Fnull, Snull, 1, 1, 0, "T if OBJECT is nil.")
  (object)
     Lisp_Object object;
{
  if (NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("type-of", Ftype_of, Stype_of, 1, 1, 0,
  "Return a symbol representing the type of OBJECT.\n\
The symbol returned names the object's basic type;\n\
for example, (type-of 1) returns `integer'.")
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
      switch (XMISC (object)->type)
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

#ifdef MULTI_FRAME
      if (GC_FRAMEP (object))
	return Qframe;
#endif
      return Qvector;

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      return Qfloat;
#endif

    default:
      abort ();
    }
}

DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0, "T if OBJECT is a cons cell.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}

DEFUN ("atom", Fatom, Satom, 1, 1, 0, "T if OBJECT is not a cons cell.  This includes nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return Qnil;
  return Qt;
}

DEFUN ("listp", Flistp, Slistp, 1, 1, 0, "T if OBJECT is a list.  This includes nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("nlistp", Fnlistp, Snlistp, 1, 1, 0, "T if OBJECT is not a list.  Lists include nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object) || NILP (object))
    return Qnil;
  return Qt;
}

DEFUN ("symbolp", Fsymbolp, Ssymbolp, 1, 1, 0, "T if OBJECT is a symbol.")
  (object)
     Lisp_Object object;
{
  if (SYMBOLP (object))
    return Qt;
  return Qnil;
}

DEFUN ("vectorp", Fvectorp, Svectorp, 1, 1, 0, "T if OBJECT is a vector.")
  (object)
     Lisp_Object object;
{
  if (VECTORP (object))
    return Qt;
  return Qnil;
}

DEFUN ("stringp", Fstringp, Sstringp, 1, 1, 0, "T if OBJECT is a string.")
  (object)
     Lisp_Object object;
{
  if (STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("arrayp", Farrayp, Sarrayp, 1, 1, 0, "T if OBJECT is an array (string or vector).")
  (object)
     Lisp_Object object;
{
  if (VECTORP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
  "T if OBJECT is a sequence (list or array).")
  (object)
     register Lisp_Object object;
{
  if (CONSP (object) || NILP (object) || VECTORP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("bufferp", Fbufferp, Sbufferp, 1, 1, 0, "T if OBJECT is an editor buffer.")
  (object)
     Lisp_Object object;
{
  if (BUFFERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("markerp", Fmarkerp, Smarkerp, 1, 1, 0, "T if OBJECT is a marker (editor pointer).")
  (object)
     Lisp_Object object;
{
  if (MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("subrp", Fsubrp, Ssubrp, 1, 1, 0, "T if OBJECT is a built-in function.")
  (object)
     Lisp_Object object;
{
  if (SUBRP (object))
    return Qt;
  return Qnil;
}

DEFUN ("byte-code-function-p", Fbyte_code_function_p, Sbyte_code_function_p,
       1, 1, 0, "T if OBJECT is a byte-compiled function object.")
  (object)
     Lisp_Object object;
{
  if (COMPILEDP (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, Schar_or_string_p, 1, 1, 0,
  "T if OBJECT is a character (an integer) or a string.")
  (object)
     register Lisp_Object object;
{
  if (INTEGERP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integerp", Fintegerp, Sintegerp, 1, 1, 0, "T if OBJECT is an integer.")
  (object)
     Lisp_Object object;
{
  if (INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, Sinteger_or_marker_p, 1, 1, 0,
  "T if OBJECT is an integer or a marker (editor pointer).")
  (object)
     register Lisp_Object object;
{
  if (MARKERP (object) || INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("natnump", Fnatnump, Snatnump, 1, 1, 0,
  "T if OBJECT is a nonnegative integer.")
  (object)
     Lisp_Object object;
{
  if (NATNUMP (object))
    return Qt;
  return Qnil;
}

DEFUN ("numberp", Fnumberp, Snumberp, 1, 1, 0,
       "T if OBJECT is a number (floating point or integer).")
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
       "T if OBJECT is a number or a marker.")
  (object)
     Lisp_Object object;
{
  if (NUMBERP (object) || MARKERP (object))
    return Qt;
  return Qnil;
}

#ifdef LISP_FLOAT_TYPE
DEFUN ("floatp", Ffloatp, Sfloatp, 1, 1, 0,
       "T if OBJECT is a floating point number.")
  (object)
     Lisp_Object object;
{
  if (FLOATP (object))
    return Qt;
  return Qnil;
}
#endif /* LISP_FLOAT_TYPE */

/* Extract and set components of lists */

DEFUN ("car", Fcar, Scar, 1, 1, 0,
  "Return the car of CONSCELL.  If arg is nil, return nil.\n\
Error if arg is not nil and not a cons cell.  See also `car-safe'.")
  (list)
     register Lisp_Object list;
{
  while (1)
    {
      if (CONSP (list))
	return XCONS (list)->car;
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
  "Return the car of OBJECT if it is a cons cell, or else nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return XCONS (object)->car;
  else
    return Qnil;
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
  "Return the cdr of CONSCELL.  If arg is nil, return nil.\n\
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.")

  (list)
     register Lisp_Object list;
{
  while (1)
    {
      if (CONSP (list))
	return XCONS (list)->cdr;
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
  "Return the cdr of OBJECT if it is a cons cell, or else nil.")
  (object)
     Lisp_Object object;
{
  if (CONSP (object))
    return XCONS (object)->cdr;
  else
    return Qnil;
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
  "Set the car of CONSCELL to be NEWCAR.  Returns NEWCAR.")
  (cell, newcar)
     register Lisp_Object cell, newcar;
{
  if (!CONSP (cell))
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XCONS (cell)->car = newcar;
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
  "Set the cdr of CONSCELL to be NEWCDR.  Returns NEWCDR.")
  (cell, newcdr)
     register Lisp_Object cell, newcdr;
{
  if (!CONSP (cell))
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XCONS (cell)->cdr = newcdr;
  return newcdr;
}

/* Extract and set components of symbols */

DEFUN ("boundp", Fboundp, Sboundp, 1, 1, 0, "T if SYMBOL's value is not void.")
  (sym)
     register Lisp_Object sym;
{
  Lisp_Object valcontents;
  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    valcontents = swap_in_symval_forwarding (sym, valcontents);

  return (EQ (valcontents, Qunbound) ? Qnil : Qt);
}

DEFUN ("fboundp", Ffboundp, Sfboundp, 1, 1, 0, "T if SYMBOL's function definition is not void.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return (EQ (XSYMBOL (sym)->function, Qunbound) ? Qnil : Qt);
}

DEFUN ("makunbound", Fmakunbound, Smakunbound, 1, 1, 0, "Make SYMBOL's value be void.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  if (NILP (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, Fcons (sym, Qnil));
  Fset (sym, Qunbound);
  return sym;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0, "Make SYMBOL's function definition be void.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  if (NILP (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, Fcons (sym, Qnil));
  XSYMBOL (sym)->function = Qunbound;
  return sym;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
  "Return SYMBOL's function definition.  Error if that is void.")
  (symbol)
     register Lisp_Object symbol;
{
  CHECK_SYMBOL (symbol, 0);
  if (EQ (XSYMBOL (symbol)->function, Qunbound))
    return Fsignal (Qvoid_function, Fcons (symbol, Qnil));
  return XSYMBOL (symbol)->function;
}

DEFUN ("symbol-plist", Fsymbol_plist, Ssymbol_plist, 1, 1, 0, "Return SYMBOL's property list.")
  (sym)
     register Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return XSYMBOL (sym)->plist;
}

DEFUN ("symbol-name", Fsymbol_name, Ssymbol_name, 1, 1, 0, "Return SYMBOL's name, a string.")
  (sym)
     register Lisp_Object sym;
{
  register Lisp_Object name;

  CHECK_SYMBOL (sym, 0);
  XSETSTRING (name, XSYMBOL (sym)->name);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.")
  (sym, newdef)
     register Lisp_Object sym, newdef;
{
  CHECK_SYMBOL (sym, 0);
  if (NILP (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, Fcons (sym, Qnil));
  if (!NILP (Vautoload_queue) && !EQ (XSYMBOL (sym)->function, Qunbound))
    Vautoload_queue = Fcons (Fcons (sym, XSYMBOL (sym)->function),
			     Vautoload_queue);
  XSYMBOL (sym)->function = newdef;
  /* Handle automatic advice activation */
  if (CONSP (XSYMBOL (sym)->plist) && !NILP (Fget (sym, Qad_advice_info)))
    {
      call2 (Qad_activate, sym, Qnil);
      newdef = XSYMBOL (sym)->function;
    }
  return newdef;
}

/* This name should be removed once it is eliminated from elsewhere.  */

DEFUN ("defalias", Fdefalias, Sdefalias, 2, 2, 0,
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.\n\
Associates the function with the current load file, if any.")
  (sym, newdef)
     register Lisp_Object sym, newdef;
{
  CHECK_SYMBOL (sym, 0);
  if (!NILP (Vautoload_queue) && !EQ (XSYMBOL (sym)->function, Qunbound))
    Vautoload_queue = Fcons (Fcons (sym, XSYMBOL (sym)->function),
			     Vautoload_queue);
  XSYMBOL (sym)->function = newdef;
  /* Handle automatic advice activation */
  if (CONSP (XSYMBOL (sym)->plist) && !NILP (Fget (sym, Qad_advice_info)))
    {
      call2 (Qad_activate, sym, Qnil);
      newdef = XSYMBOL (sym)->function;
    }
  LOADHIST_ATTACH (sym);
  return newdef;
}

DEFUN ("define-function", Fdefine_function, Sdefine_function, 2, 2, 0,
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.\n\
Associates the function with the current load file, if any.")
  (sym, newdef)
     register Lisp_Object sym, newdef;
{
  CHECK_SYMBOL (sym, 0);
  if (!NILP (Vautoload_queue) && !EQ (XSYMBOL (sym)->function, Qunbound))
    Vautoload_queue = Fcons (Fcons (sym, XSYMBOL (sym)->function),
			     Vautoload_queue);
  XSYMBOL (sym)->function = newdef;
  /* Handle automatic advice activation */
  if (CONSP (XSYMBOL (sym)->plist) && !NILP (Fget (sym, Qad_advice_info)))
    {
      call2 (Qad_activate, sym, Qnil);
      newdef = XSYMBOL (sym)->function;
    }
  LOADHIST_ATTACH (sym);
  return newdef;
}

DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
  "Set SYMBOL's property list to NEWVAL, and return NEWVAL.")
  (sym, newplist)
     register Lisp_Object sym, newplist;
{
  CHECK_SYMBOL (sym, 0);
  XSYMBOL (sym)->plist = newplist;
  return newplist;
}


/* Getting and setting values of symbols */

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
    switch (XMISC (valcontents)->type)
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
	return *(Lisp_Object *)(offset + (char *)current_buffer);

      case Lisp_Misc_Display_Objfwd:
	if (!current_perdisplay)
	  abort ();
	offset = XDISPLAY_OBJFWD (valcontents)->offset;
	return *(Lisp_Object *)(offset + (char *)current_perdisplay);
      }
  return valcontents;
}

/* Store NEWVAL into SYM, where VALCONTENTS is found in the value cell
   of SYM.  If SYM is buffer-local, VALCONTENTS should be the
   buffer-independent contents of the value cell: forwarded just one
   step past the buffer-localness.  */

void
store_symval_forwarding (sym, valcontents, newval)
     Lisp_Object sym;
     register Lisp_Object valcontents, newval;
{
  switch (SWITCH_ENUM_CAST (XTYPE (valcontents)))
    {
    case Lisp_Misc:
      switch (XMISC (valcontents)->type)
	{
	case Lisp_Misc_Intfwd:
	  CHECK_NUMBER (newval, 1);
	  *XINTFWD (valcontents)->intvar = XINT (newval);
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

	    type = *(Lisp_Object *)(offset + (char *)&buffer_local_types);
	    if (! NILP (type) && ! NILP (newval)
		&& XTYPE (newval) != XINT (type))
	      buffer_slot_type_mismatch (offset);

	    *(Lisp_Object *)(offset + (char *)current_buffer) = newval;
	  }
	  break;

	case Lisp_Misc_Display_Objfwd:
	  if (!current_perdisplay)
	    abort ();
	  (*(Lisp_Object *)((char *)current_perdisplay
			    + XDISPLAY_OBJFWD (valcontents)->offset))
	    = newval;
	  break;

	default:
	  goto def;
	}
      break;

    default:
    def:
      valcontents = XSYMBOL (sym)->value;
      if (BUFFER_LOCAL_VALUEP (valcontents)
	  || SOME_BUFFER_LOCAL_VALUEP (valcontents))
	XBUFFER_LOCAL_VALUE (valcontents)->car = newval;
      else
	XSYMBOL (sym)->value = newval;
    }
}

/* Set up the buffer-local symbol SYM for validity in the current
   buffer.  VALCONTENTS is the contents of its value cell.
   Return the value forwarded one step past the buffer-local indicator.  */

static Lisp_Object
swap_in_symval_forwarding (sym, valcontents)
     Lisp_Object sym, valcontents;
{
  /* valcontents is a pointer to a struct resembling the cons
     (REALVALUE BUFFER CURRENT-ALIST-ELEMENT . DEFAULT-VALUE)).

     CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
     local_var_alist, that being the element whose car is this
     variable.  Or it can be a pointer to the
     (CURRENT-ALIST-ELEMENT . DEFAULT-VALUE), if BUFFER does not have
     an element in its alist for this variable.

     If the current buffer is not BUFFER, we store the current
     REALVALUE value into CURRENT-ALIST-ELEMENT, then find the
     appropriate alist element for the buffer now current and set up
     CURRENT-ALIST-ELEMENT.  Then we set REALVALUE out of that
     element, and store into BUFFER.

     Note that REALVALUE can be a forwarding pointer. */

  register Lisp_Object tem1;
  tem1 = XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->car;

  if (NILP (tem1) || current_buffer != XBUFFER (tem1))
    {
      tem1 = XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->car;
      Fsetcdr (tem1,
	       do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->car));
      tem1 = assq_no_quit (sym, current_buffer->local_var_alist);
      if (NILP (tem1))
	tem1 = XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr;
      XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->car = tem1;
      XSETBUFFER (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->car,
		  current_buffer);
      store_symval_forwarding (sym, XBUFFER_LOCAL_VALUE (valcontents)->car,
			       Fcdr (tem1));
    }
  return XBUFFER_LOCAL_VALUE (valcontents)->car;
}

/* Find the value of a symbol, returning Qunbound if it's not bound.
   This is helpful for code which just wants to get a variable's value
   if it has one, without signalling an error.
   Note that it must not be possible to quit
   within this function.  Great care is required for this.  */

Lisp_Object
find_symbol_value (sym)
     Lisp_Object sym;
{
  register Lisp_Object valcontents, tem1;
  register Lisp_Object val;
  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    valcontents = swap_in_symval_forwarding (sym, valcontents);

  if (MISCP (valcontents))
    {
      switch (XMISC (valcontents)->type)
	{
	case Lisp_Misc_Intfwd:
	  XSETINT (val, *XINTFWD (valcontents)->intvar);
	  return val;

	case Lisp_Misc_Boolfwd:
	  return (*XBOOLFWD (valcontents)->boolvar ? Qt : Qnil);

	case Lisp_Misc_Objfwd:
	  return *XOBJFWD (valcontents)->objvar;

	case Lisp_Misc_Buffer_Objfwd:
	  return *(Lisp_Object *)(XBUFFER_OBJFWD (valcontents)->offset
				  + (char *)current_buffer);

	case Lisp_Misc_Display_Objfwd:
	  if (!current_perdisplay)
	    abort ();
	  return *(Lisp_Object *)(XDISPLAY_OBJFWD (valcontents)->offset
				  + (char *)current_perdisplay);
	}
    }

  return valcontents;
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0,
  "Return SYMBOL's value.  Error if that is void.")
  (sym)
     Lisp_Object sym;
{
  Lisp_Object val;

  val = find_symbol_value (sym);
  if (EQ (val, Qunbound))
    return Fsignal (Qvoid_variable, Fcons (sym, Qnil));
  else
    return val;
}

DEFUN ("set", Fset, Sset, 2, 2, 0,
  "Set SYMBOL's value to NEWVAL, and return NEWVAL.")
  (sym, newval)
     register Lisp_Object sym, newval;
{
  int voide = EQ (newval, Qunbound);

  register Lisp_Object valcontents, tem1, current_alist_element;

  CHECK_SYMBOL (sym, 0);
  if (NILP (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, Fcons (sym, Qnil));
  valcontents = XSYMBOL (sym)->value;

  if (BUFFER_OBJFWDP (valcontents))
    {
      register int idx = XBUFFER_OBJFWD (valcontents)->offset;
      register int mask = XINT (*((Lisp_Object *)
				  (idx + (char *)&buffer_local_flags)));
      if (mask > 0)
	current_buffer->local_var_flags |= mask;
    }

  else if (BUFFER_LOCAL_VALUEP (valcontents)
	   || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      /* valcontents is actually a pointer to a struct resembling a cons,
	 with contents something like:
	 (REALVALUE BUFFER CURRENT-ALIST-ELEMENT . DEFAULT-VALUE).

	 BUFFER is the last buffer for which this symbol's value was
	 made up to date.

	 CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
	 local_var_alist, that being the element whose car is this
	 variable.  Or it can be a pointer to the
	 (CURRENT-ALIST-ELEMENT . DEFAULT-VALUE), if BUFFER does not
	 have an element in its alist for this variable (that is, if
	 BUFFER sees the default value of this variable).

	 If we want to examine or set the value and BUFFER is current,
	 we just examine or set REALVALUE. If BUFFER is not current, we
	 store the current REALVALUE value into CURRENT-ALIST-ELEMENT,
	 then find the appropriate alist element for the buffer now
	 current and set up CURRENT-ALIST-ELEMENT.  Then we set
	 REALVALUE out of that element, and store into BUFFER.

	 If we are setting the variable and the current buffer does
	 not have an alist entry for this variable, an alist entry is
	 created.

	 Note that REALVALUE can be a forwarding pointer.  Each time
	 it is examined or set, forwarding must be done.  */

      /* What value are we caching right now?  */
      current_alist_element =
	XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->car;

      /* If the current buffer is not the buffer whose binding is
	 currently cached, or if it's a Lisp_Buffer_Local_Value and
	 we're looking at the default value, the cache is invalid; we
	 need to write it out, and find the new CURRENT-ALIST-ELEMENT.  */
      if ((current_buffer
	   != XBUFFER (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->car))
	  || (BUFFER_LOCAL_VALUEP (valcontents)
	      && EQ (XCONS (current_alist_element)->car,
		     current_alist_element)))
	{
	  /* Write out the cached value for the old buffer; copy it
	     back to its alist element.  This works if the current
	     buffer only sees the default value, too.  */
          Fsetcdr (current_alist_element,
		   do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->car));

	  /* Find the new value for CURRENT-ALIST-ELEMENT.  */
	  tem1 = Fassq (sym, current_buffer->local_var_alist);
	  if (NILP (tem1))
	    {
	      /* This buffer still sees the default value.  */

	      /* If the variable is a Lisp_Some_Buffer_Local_Value,
		 make CURRENT-ALIST-ELEMENT point to itself,
		 indicating that we're seeing the default value.  */
	      if (SOME_BUFFER_LOCAL_VALUEP (valcontents))
		tem1 = XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr;

	      /* If it's a Lisp_Buffer_Local_Value, give this buffer a
		 new assoc for a local value and set
		 CURRENT-ALIST-ELEMENT to point to that.  */
	      else
		{
		  tem1 = Fcons (sym, Fcdr (current_alist_element));
		  current_buffer->local_var_alist =
		    Fcons (tem1, current_buffer->local_var_alist);
		}
	    }
	  /* Cache the new buffer's assoc in CURRENT-ALIST-ELEMENT.  */
	  XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->car
	    = tem1;

	  /* Set BUFFER, now that CURRENT-ALIST-ELEMENT is accurate.  */
	  XSETBUFFER (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->car,
		      current_buffer);
	}
      valcontents = XBUFFER_LOCAL_VALUE (valcontents)->car;
    }

  /* If storing void (making the symbol void), forward only through
     buffer-local indicator, not through Lisp_Objfwd, etc.  */
  if (voide)
    store_symval_forwarding (sym, Qnil, newval);
  else
    store_symval_forwarding (sym, valcontents, newval);

  return newval;
}

/* Access or set a buffer-local symbol's default value.  */

/* Return the default value of SYM, but don't check for voidness.
   Return Qunbound if it is void.  */

Lisp_Object
default_value (sym)
     Lisp_Object sym;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

  /* For a built-in buffer-local variable, get the default value
     rather than letting do_symval_forwarding get the current value.  */
  if (BUFFER_OBJFWDP (valcontents))
    {
      register int idx = XBUFFER_OBJFWD (valcontents)->offset;

      if (XINT (*(Lisp_Object *) (idx + (char *) &buffer_local_flags)) != 0)
	return *(Lisp_Object *)(idx + (char *) &buffer_defaults);
    }

  /* Handle user-created local variables.  */
  if (BUFFER_LOCAL_VALUEP (valcontents)
      || SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      /* If var is set up for a buffer that lacks a local value for it,
	 the current value is nominally the default value.
	 But the current value slot may be more up to date, since
	 ordinary setq stores just that slot.  So use that.  */
      Lisp_Object current_alist_element, alist_element_car;
      current_alist_element
	= XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->car;
      alist_element_car = XCONS (current_alist_element)->car;
      if (EQ (alist_element_car, current_alist_element))
	return do_symval_forwarding (XBUFFER_LOCAL_VALUE (valcontents)->car);
      else
	return XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->cdr;
    }
  /* For other variables, get the current value.  */
  return do_symval_forwarding (valcontents);
}

DEFUN ("default-boundp", Fdefault_boundp, Sdefault_boundp, 1, 1, 0,
  "Return T if SYMBOL has a non-void default value.\n\
This is the value that is seen in buffers that do not have their own values\n\
for this variable.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object value;

  value = default_value (sym);
  return (EQ (value, Qunbound) ? Qnil : Qt);
}

DEFUN ("default-value", Fdefault_value, Sdefault_value, 1, 1, 0,
  "Return SYMBOL's default value.\n\
This is the value that is seen in buffers that do not have their own values\n\
for this variable.  The default value is meaningful for variables with\n\
local bindings in certain buffers.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object value;

  value = default_value (sym);
  if (EQ (value, Qunbound))
    return Fsignal (Qvoid_variable, Fcons (sym, Qnil));
  return value;
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
  "Set SYMBOL's default value to VAL.  SYMBOL and VAL are evaluated.\n\
The default value is seen in buffers that do not have their own values\n\
for this variable.")
  (sym, value)
     Lisp_Object sym, value;
{
  register Lisp_Object valcontents, current_alist_element, alist_element_buffer;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

  /* Handle variables like case-fold-search that have special slots
     in the buffer.  Make them work apparently like Lisp_Buffer_Local_Value
     variables.  */
  if (BUFFER_OBJFWDP (valcontents))
    {
      register int idx = XBUFFER_OBJFWD (valcontents)->offset;
      register struct buffer *b;
      register int mask = XINT (*((Lisp_Object *)
				  (idx + (char *)&buffer_local_flags)));

      if (mask > 0)
	{
	  *(Lisp_Object *)(idx + (char *) &buffer_defaults) = value;
	  for (b = all_buffers; b; b = b->next)
	    if (!(b->local_var_flags & mask))
	      *(Lisp_Object *)(idx + (char *) b) = value;
	}
      return value;
    }

  if (!BUFFER_LOCAL_VALUEP (valcontents)
      && !SOME_BUFFER_LOCAL_VALUEP (valcontents))
    return Fset (sym, value);

  /* Store new value into the DEFAULT-VALUE slot */
  XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->cdr = value;

  /* If that slot is current, we must set the REALVALUE slot too */
  current_alist_element
    = XCONS (XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->cdr)->car;
  alist_element_buffer = Fcar (current_alist_element);
  if (EQ (alist_element_buffer, current_alist_element))
    store_symval_forwarding (sym, XBUFFER_LOCAL_VALUE (valcontents)->car,
			     value);

  return value;
}

DEFUN ("setq-default", Fsetq_default, Ssetq_default, 2, UNEVALLED, 0,
       "Set the default value of variable VAR to VALUE.\n\
VAR, the variable name, is literal (not evaluated);\n\
VALUE is an expression and it is evaluated.\n\
The default value of a variable is seen in buffers\n\
that do not have their own values for the variable.\n\
\n\
More generally, you can use multiple variables and values, as in\n\
  (setq-default SYM VALUE SYM VALUE...)\n\
This sets each SYM's default value to the corresponding VALUE.\n\
The VALUE for the Nth SYM can refer to the new default values\n\
of previous SYMs.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object args_left;
  register Lisp_Object val, sym;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = Feval (Fcar (Fcdr (args_left)));
      sym = Fcar (args_left);
      Fset_default (sym, val);
      args_left = Fcdr (Fcdr (args_left));
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

/* Lisp functions for creating and removing buffer-local variables.  */

DEFUN ("make-variable-buffer-local", Fmake_variable_buffer_local, Smake_variable_buffer_local,
  1, 1, "vMake Variable Buffer Local: ",
  "Make VARIABLE have a separate value for each buffer.\n\
At any time, the value for the current buffer is in effect.\n\
There is also a default value which is seen in any buffer which has not yet\n\
set its own value.\n\
Using `set' or `setq' to set the variable causes it to have a separate value\n\
for the current buffer if it was previously using the default value.\n\
The function `default-value' gets the default value and `set-default' sets it.")
  (sym)
     register Lisp_Object sym;
{
  register Lisp_Object tem, valcontents, newval;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;
  if (EQ (sym, Qnil) || EQ (sym, Qt) || DISPLAY_OBJFWDP (valcontents))
    error ("Symbol %s may not be buffer-local", XSYMBOL (sym)->name->data);

  if (BUFFER_LOCAL_VALUEP (valcontents) || BUFFER_OBJFWDP (valcontents))
    return sym;
  if (SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      XMISC (XSYMBOL (sym)->value)->type = Lisp_Misc_Buffer_Local_Value;
      return sym;
    }
  if (EQ (valcontents, Qunbound))
    XSYMBOL (sym)->value = Qnil;
  tem = Fcons (Qnil, Fsymbol_value (sym));
  XCONS (tem)->car = tem;
  newval = allocate_misc ();
  XMISC (newval)->type = Lisp_Misc_Buffer_Local_Value;
  XBUFFER_LOCAL_VALUE (newval)->car = XSYMBOL (sym)->value;
  XBUFFER_LOCAL_VALUE (newval)->cdr = Fcons (Fcurrent_buffer (), tem);
  XSYMBOL (sym)->value = newval;
  return sym;
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
  1, 1, "vMake Local Variable: ",
  "Make VARIABLE have a separate value in the current buffer.\n\
Other buffers will continue to share a common default value.\n\
\(The buffer-local value of VARIABLE starts out as the same value\n\
VARIABLE previously had.  If VARIABLE was void, it remains void.\)\n\
See also `make-variable-buffer-local'.\n\n\
If the variable is already arranged to become local when set,\n\
this function causes a local value to exist for this buffer,\n\
just as setting the variable would do.\n\
\n\
Do not use `make-local-variable' to make a hook variable buffer-local.\n\
Use `make-local-hook' instead.")
  (sym)
     register Lisp_Object sym;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;
  if (EQ (sym, Qnil) || EQ (sym, Qt) || DISPLAY_OBJFWDP (valcontents))
    error ("Symbol %s may not be buffer-local", XSYMBOL (sym)->name->data);

  if (BUFFER_LOCAL_VALUEP (valcontents) || BUFFER_OBJFWDP (valcontents))
    {
      tem = Fboundp (sym);

      /* Make sure the symbol has a local value in this particular buffer,
	 by setting it to the same value it already has.  */
      Fset (sym, (EQ (tem, Qt) ? Fsymbol_value (sym) : Qunbound));
      return sym;
    }
  /* Make sure sym is set up to hold per-buffer values */
  if (!SOME_BUFFER_LOCAL_VALUEP (valcontents))
    {
      Lisp_Object newval;
      tem = Fcons (Qnil, do_symval_forwarding (valcontents));
      XCONS (tem)->car = tem;
      newval = allocate_misc ();
      XMISC (newval)->type = Lisp_Misc_Some_Buffer_Local_Value;
      XBUFFER_LOCAL_VALUE (newval)->car = XSYMBOL (sym)->value;
      XBUFFER_LOCAL_VALUE (newval)->cdr = Fcons (Qnil, tem);
      XSYMBOL (sym)->value = newval;
    }
  /* Make sure this buffer has its own value of sym */
  tem = Fassq (sym, current_buffer->local_var_alist);
  if (NILP (tem))
    {
      current_buffer->local_var_alist
        = Fcons (Fcons (sym, XCONS (XCONS (XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->cdr)->cdr)->cdr),
		 current_buffer->local_var_alist);

      /* Make sure symbol does not think it is set up for this buffer;
	 force it to look once again for this buffer's value */
      {
	Lisp_Object *pvalbuf;
	valcontents = XSYMBOL (sym)->value;
	pvalbuf = &XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->car;
	if (current_buffer == XBUFFER (*pvalbuf))
	  *pvalbuf = Qnil;
      }
    }

  /* If the symbol forwards into a C variable, then swap in the
     variable for this buffer immediately.  If C code modifies the
     variable before we swap in, then that new value will clobber the
     default value the next time we swap.  */
  valcontents = XBUFFER_LOCAL_VALUE (XSYMBOL (sym)->value)->car;
  if (INTFWDP (valcontents) || BOOLFWDP (valcontents) || OBJFWDP (valcontents))
    swap_in_symval_forwarding (sym, XSYMBOL (sym)->value);

  return sym;
}

DEFUN ("kill-local-variable", Fkill_local_variable, Skill_local_variable,
  1, 1, "vKill Local Variable: ",
  "Make VARIABLE no longer have a separate value in the current buffer.\n\
From now on the default value will apply in this buffer.")
  (sym)
     register Lisp_Object sym;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;

  if (BUFFER_OBJFWDP (valcontents))
    {
      register int idx = XBUFFER_OBJFWD (valcontents)->offset;
      register int mask = XINT (*((Lisp_Object*)
				  (idx + (char *)&buffer_local_flags)));

      if (mask > 0)
	{
	  *(Lisp_Object *)(idx + (char *) current_buffer)
	    = *(Lisp_Object *)(idx + (char *) &buffer_defaults);
	  current_buffer->local_var_flags &= ~mask;
	}
      return sym;
    }

  if (!BUFFER_LOCAL_VALUEP (valcontents)
      && !SOME_BUFFER_LOCAL_VALUEP (valcontents))
    return sym;

  /* Get rid of this buffer's alist element, if any */

  tem = Fassq (sym, current_buffer->local_var_alist);
  if (!NILP (tem))
    current_buffer->local_var_alist
      = Fdelq (tem, current_buffer->local_var_alist);

  /* Make sure symbol does not think it is set up for this buffer;
     force it to look once again for this buffer's value */
  {
    Lisp_Object *pvalbuf;
    valcontents = XSYMBOL (sym)->value;
    pvalbuf = &XCONS (XBUFFER_LOCAL_VALUE (valcontents)->cdr)->car;
    if (current_buffer == XBUFFER (*pvalbuf))
      *pvalbuf = Qnil;
  }

  return sym;
}

DEFUN ("local-variable-p", Flocal_variable_p, Slocal_variable_p,
  1, 1, 0,
  "Non-nil if VARIABLE has a local binding in the current buffer.")
  (sym)
     register Lisp_Object sym;
{
  Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;
  return ((BUFFER_LOCAL_VALUEP (valcontents)
	   || SOME_BUFFER_LOCAL_VALUEP (valcontents)
	   || BUFFER_OBJFWDP (valcontents))
	  ? Qt : Qnil);
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
  "Return the function at the end of OBJECT's function chain.\n\
If OBJECT is a symbol, follow all function indirections and return the final\n\
function binding.\n\
If OBJECT is not a symbol, just return it.\n\
Signal a void-function error if the final symbol is unbound.\n\
Signal a cyclic-function-indirection error if there is a loop in the\n\
function chain of symbols.")
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
  "Return the element of ARRAY at index INDEX.\n\
ARRAY may be a vector or a string, or a byte-code object.  INDEX starts at 0.")
  (array, idx)
     register Lisp_Object array;
     Lisp_Object idx;
{
  register int idxval;

  CHECK_NUMBER (idx, 1);
  idxval = XINT (idx);
  if (STRINGP (array))
    {
      Lisp_Object val;
      if (idxval < 0 || idxval >= XSTRING (array)->size)
	args_out_of_range (array, idx);
      XSETFASTINT (val, (unsigned char) XSTRING (array)->data[idxval]);
      return val;
    }
  else
    {
      int size;
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
  "Store into the element of ARRAY at index IDX the value NEWELT.\n\
ARRAY may be a vector or a string.  IDX starts at 0.")
  (array, idx, newelt)
     register Lisp_Object array;
     Lisp_Object idx, newelt;
{
  register int idxval;

  CHECK_NUMBER (idx, 1);
  idxval = XINT (idx);
  if (!VECTORP (array) && !STRINGP (array))
    array = wrong_type_argument (Qarrayp, array);
  CHECK_IMPURE (array);

  if (VECTORP (array))
    {
      if (idxval < 0 || idxval >= XVECTOR (array)->size)
	args_out_of_range (array, idx);
      XVECTOR (array)->contents[idxval] = newelt;
    }
  else
    {
      if (idxval < 0 || idxval >= XSTRING (array)->size)
	args_out_of_range (array, idx);
      CHECK_NUMBER (newelt, 2);
      XSTRING (array)->data[idxval] = XINT (newelt);
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
  double f1, f2;
  int floatp = 0;

#ifdef LISP_FLOAT_TYPE
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num2, 0);

  if (FLOATP (num1) || FLOATP (num2))
    {
      floatp = 1;
      f1 = (FLOATP (num1)) ? XFLOAT (num1)->data : XINT (num1);
      f2 = (FLOATP (num2)) ? XFLOAT (num2)->data : XINT (num2);
    }
#else
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);
#endif /* LISP_FLOAT_TYPE */

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
  "T if two args, both numbers or markers, are equal.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, equal);
}

DEFUN ("<", Flss, Slss, 2, 2, 0,
  "T if first arg is less than second arg.  Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, less);
}

DEFUN (">", Fgtr, Sgtr, 2, 2, 0,
  "T if first arg is greater than second arg.  Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, grtr);
}

DEFUN ("<=", Fleq, Sleq, 2, 2, 0,
  "T if first arg is less than or equal to second arg.\n\
Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, less_or_equal);
}

DEFUN (">=", Fgeq, Sgeq, 2, 2, 0,
  "T if first arg is greater than or equal to second arg.\n\
Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, grtr_or_equal);
}

DEFUN ("/=", Fneq, Sneq, 2, 2, 0,
  "T if first arg is not equal to second arg.  Both must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  return arithcompare (num1, num2, notequal);
}

DEFUN ("zerop", Fzerop, Szerop, 1, 1, 0, "T if NUMBER is zero.")
  (num)
     register Lisp_Object num;
{
#ifdef LISP_FLOAT_TYPE
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (FLOATP (num))
    {
      if (XFLOAT(num)->data == 0.0)
	return Qt;
      return Qnil;
    }
#else
  CHECK_NUMBER (num, 0);
#endif /* LISP_FLOAT_TYPE */

  if (!XINT (num))
    return Qt;
  return Qnil;
}

/* Convert between 32-bit values and pairs of lispy 24-bit values.  */

Lisp_Object
long_to_cons (i)
     unsigned long i;
{
  unsigned int top = i >> 16;
  unsigned int bot = i & 0xFFFF;
  if (top == 0)
    return make_number (bot);
  if (top == 0xFFFF)
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
  top = XCONS (c)->car;
  bot = XCONS (c)->cdr;
  if (CONSP (bot))
    bot = XCONS (bot)->car;
  return ((XINT (top) << 16) | XINT (bot));
}

DEFUN ("number-to-string", Fnumber_to_string, Snumber_to_string, 1, 1, 0,
  "Convert NUM to a string by printing it in decimal.\n\
Uses a minus sign if negative.\n\
NUM may be an integer or a floating point number.")
  (num)
     Lisp_Object num;
{
  char buffer[20];

#ifndef LISP_FLOAT_TYPE
  CHECK_NUMBER (num, 0);
#else
  CHECK_NUMBER_OR_FLOAT (num, 0);

  if (FLOATP (num))
    {
      char pigbuf[350];	/* see comments in float_to_string */

      float_to_string (pigbuf, XFLOAT(num)->data);
      return build_string (pigbuf);
    }
#endif /* LISP_FLOAT_TYPE */

  sprintf (buffer, "%d", XINT (num));
  return build_string (buffer);
}

DEFUN ("string-to-number", Fstring_to_number, Sstring_to_number, 1, 1, 0,
  "Convert STRING to a number by parsing it as a decimal number.\n\
This parses both integers and floating point numbers.\n\
It ignores leading spaces and tabs.")
  (str)
     register Lisp_Object str;
{
  unsigned char *p;

  CHECK_STRING (str, 0);

  p = XSTRING (str)->data;

  /* Skip any whitespace at the front of the number.  Some versions of
     atoi do this anyway, so we might as well make Emacs lisp consistent.  */
  while (*p == ' ' || *p == '\t')
    p++;

#ifdef LISP_FLOAT_TYPE
  if (isfloat_string (p))
    return make_float (atof (p));
#endif /* LISP_FLOAT_TYPE */

  return make_number (atoi (p));
}

enum arithop
  { Aadd, Asub, Amult, Adiv, Alogand, Alogior, Alogxor, Amax, Amin };

extern Lisp_Object float_arith_driver ();

Lisp_Object
arith_driver (code, nargs, args)
     enum arithop code;
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object val;
  register int argnum;
  register int accum;
  register int next;

  switch (SWITCH_ENUM_CAST (code))
    {
    case Alogior:
    case Alogxor:
    case Aadd:
    case Asub:
      accum = 0; break;
    case Amult:
      accum = 1; break;
    case Alogand:
      accum = -1; break;
    }

  for (argnum = 0; argnum < nargs; argnum++)
    {
      val = args[argnum];    /* using args[argnum] as argument to CHECK_NUMBER_... */
#ifdef LISP_FLOAT_TYPE
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val, argnum);

      if (FLOATP (val)) /* time to do serious math */
	return (float_arith_driver ((double) accum, argnum, code,
				    nargs, args));
#else
      CHECK_NUMBER_COERCE_MARKER (val, argnum);
#endif /* LISP_FLOAT_TYPE */
      args[argnum] = val;    /* runs into a compiler bug. */
      next = XINT (args[argnum]);
      switch (SWITCH_ENUM_CAST (code))
	{
	case Aadd: accum += next; break;
	case Asub:
	  if (!argnum && nargs != 1)
	    next = - next;
	  accum -= next;
	  break;
	case Amult: accum *= next; break;
	case Adiv:
	  if (!argnum) accum = next;
	  else
	    {
	      if (next == 0)
		Fsignal (Qarith_error, Qnil);
	      accum /= next;
	    }
	  break;
	case Alogand: accum &= next; break;
	case Alogior: accum |= next; break;
	case Alogxor: accum ^= next; break;
	case Amax: if (!argnum || next > accum) accum = next; break;
	case Amin: if (!argnum || next < accum) accum = next; break;
	}
    }

  XSETINT (val, accum);
  return val;
}

#ifdef LISP_FLOAT_TYPE

#undef isnan
#define isnan(x) ((x) != (x))

Lisp_Object
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
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val, argnum);

      if (FLOATP (val))
	{
	  next = XFLOAT (val)->data;
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
	  if (!argnum && nargs != 1)
	    next = - next;
	  accum -= next;
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
#endif /* LISP_FLOAT_TYPE */

DEFUN ("+", Fplus, Splus, 0, MANY, 0,
  "Return sum of any number of arguments, which are numbers or markers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Aadd, nargs, args);
}

DEFUN ("-", Fminus, Sminus, 0, MANY, 0,
  "Negate number or subtract numbers or markers.\n\
With one arg, negates it.  With more than one arg,\n\
subtracts all but the first from the first.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Asub, nargs, args);
}

DEFUN ("*", Ftimes, Stimes, 0, MANY, 0,
  "Returns product of any number of arguments, which are numbers or markers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amult, nargs, args);
}

DEFUN ("/", Fquo, Squo, 2, MANY, 0,
  "Returns first argument divided by all the remaining arguments.\n\
The arguments must be numbers or markers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Adiv, nargs, args);
}

DEFUN ("%", Frem, Srem, 2, 2, 0,
  "Returns remainder of first arg divided by second.\n\
Both must be integers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  Lisp_Object val;

  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 1);

  if (XFASTINT (num2) == 0)
    Fsignal (Qarith_error, Qnil);

  XSETINT (val, XINT (num1) % XINT (num2));
  return val;
}

#ifndef HAVE_FMOD
double
fmod (f1, f2)
     double f1, f2;
{
#ifdef HAVE_DREM  /* Some systems use this non-standard name.  */
  return (drem (f1, f2));
#else  /* Other systems don't seem to have it at all.  */
  return (f1 - f2 * floor (f1/f2));
#endif
}
#endif /* ! HAVE_FMOD */

DEFUN ("mod", Fmod, Smod, 2, 2, 0,
  "Returns X modulo Y.\n\
The result falls between zero (inclusive) and Y (exclusive).\n\
Both X and Y must be numbers or markers.")
  (num1, num2)
     register Lisp_Object num1, num2;
{
  Lisp_Object val;
  int i1, i2;

#ifdef LISP_FLOAT_TYPE
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num2, 1);

  if (FLOATP (num1) || FLOATP (num2))
    {
      double f1, f2;

      f1 = FLOATP (num1) ? XFLOAT (num1)->data : XINT (num1);
      f2 = FLOATP (num2) ? XFLOAT (num2)->data : XINT (num2);
      if (f2 == 0)
	Fsignal (Qarith_error, Qnil);

      f1 = fmod (f1, f2);
      /* If the "remainder" comes out with the wrong sign, fix it.  */
      if ((f1 < 0) != (f2 < 0))
	f1 += f2;
      return (make_float (f1));
    }
#else /* not LISP_FLOAT_TYPE */
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 1);
#endif /* not LISP_FLOAT_TYPE */

  i1 = XINT (num1);
  i2 = XINT (num2);

  if (i2 == 0)
    Fsignal (Qarith_error, Qnil);

  i1 %= i2;

  /* If the "remainder" comes out with the wrong sign, fix it.  */
  if ((i1 < 0) != (i2 < 0))
    i1 += i2;

  XSETINT (val, i1);
  return val;
}

DEFUN ("max", Fmax, Smax, 1, MANY, 0,
  "Return largest of all the arguments (which must be numbers or markers).\n\
The value is always a number; markers are converted to numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amax, nargs, args);
}

DEFUN ("min", Fmin, Smin, 1, MANY, 0,
  "Return smallest of all the arguments (which must be numbers or markers).\n\
The value is always a number; markers are converted to numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amin, nargs, args);
}

DEFUN ("logand", Flogand, Slogand, 0, MANY, 0,
  "Return bitwise-and of all the arguments.\n\
Arguments may be integers, or markers converted to integers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogand, nargs, args);
}

DEFUN ("logior", Flogior, Slogior, 0, MANY, 0,
  "Return bitwise-or of all the arguments.\n\
Arguments may be integers, or markers converted to integers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogior, nargs, args);
}

DEFUN ("logxor", Flogxor, Slogxor, 0, MANY, 0,
  "Return bitwise-exclusive-or of all the arguments.\n\
Arguments may be integers, or markers converted to integers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogxor, nargs, args);
}

DEFUN ("ash", Fash, Sash, 2, 2, 0,
  "Return VALUE with its bits shifted left by COUNT.\n\
If COUNT is negative, shifting is actually to the right.\n\
In this case, the sign bit is duplicated.")
  (value, num2)
     register Lisp_Object value, num2;
{
  register Lisp_Object val;

  CHECK_NUMBER (value, 0);
  CHECK_NUMBER (count, 1);

  if (XINT (count) > 0)
    XSETINT (val, XINT (value) << XFASTINT (count));
  else
    XSETINT (val, XINT (value) >> -XINT (count));
  return val;
}

DEFUN ("lsh", Flsh, Slsh, 2, 2, 0,
  "Return VALUE with its bits shifted left by COUNT.\n\
If COUNT is negative, shifting is actually to the right.\n\
In this case,  zeros are shifted in on the left.")
  (value, count)
     register Lisp_Object value, count;
{
  register Lisp_Object val;

  CHECK_NUMBER (value, 0);
  CHECK_NUMBER (count, 1);

  if (XINT (count) > 0)
    XSETINT (val, (EMACS_UINT) XUINT (value) << XFASTINT (count));
  else
    XSETINT (val, (EMACS_UINT) XUINT (value) >> -XINT (count));
  return val;
}

DEFUN ("1+", Fadd1, Sadd1, 1, 1, 0,
  "Return NUMBER plus one.  NUMBER may be a number or a marker.\n\
Markers are converted to integers.")
  (num)
     register Lisp_Object num;
{
#ifdef LISP_FLOAT_TYPE
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num, 0);

  if (FLOATP (num))
    return (make_float (1.0 + XFLOAT (num)->data));
#else
  CHECK_NUMBER_COERCE_MARKER (num, 0);
#endif /* LISP_FLOAT_TYPE */

  XSETINT (num, XINT (num) + 1);
  return num;
}

DEFUN ("1-", Fsub1, Ssub1, 1, 1, 0,
  "Return NUMBER minus one.  NUMBER may be a number or a marker.\n\
Markers are converted to integers.")
  (num)
     register Lisp_Object num;
{
#ifdef LISP_FLOAT_TYPE
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num, 0);

  if (FLOATP (num))
    return (make_float (-1.0 + XFLOAT (num)->data));
#else
  CHECK_NUMBER_COERCE_MARKER (num, 0);
#endif /* LISP_FLOAT_TYPE */

  XSETINT (num, XINT (num) - 1);
  return num;
}

DEFUN ("lognot", Flognot, Slognot, 1, 1, 0,
  "Return the bitwise complement of ARG.  ARG must be an integer.")
  (num)
     register Lisp_Object num;
{
  CHECK_NUMBER (num, 0);
  XSETINT (num, ~XINT (num));
  return num;
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
  Qmark_inactive = intern ("mark-inactive");

  Qlistp = intern ("listp");
  Qconsp = intern ("consp");
  Qsymbolp = intern ("symbolp");
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

#ifdef LISP_FLOAT_TYPE
  Qfloatp = intern ("floatp");
  Qnumberp = intern ("numberp");
  Qnumber_or_marker_p = intern ("number-or-marker-p");
#endif /* LISP_FLOAT_TYPE */

  Qcdr = intern ("cdr");

  /* Handle automatic advice activation */
  Qad_advice_info = intern ("ad-advice-info");
  Qad_activate = intern ("ad-activate");

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

#ifdef LISP_FLOAT_TYPE
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
#endif /* LISP_FLOAT_TYPE */

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
  staticpro (&Qmark_inactive);

  staticpro (&Qlistp);
  staticpro (&Qconsp);
  staticpro (&Qsymbolp);
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
#ifdef LISP_FLOAT_TYPE
  staticpro (&Qfloatp);
  staticpro (&Qnumberp);
  staticpro (&Qnumber_or_marker_p);
#endif /* LISP_FLOAT_TYPE */

  staticpro (&Qboundp);
  staticpro (&Qfboundp);
  staticpro (&Qcdr);
  staticpro (&Qad_advice_info);
  staticpro (&Qad_activate);

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
#ifdef LISP_FLOAT_TYPE
  defsubr (&Sfloatp);
#endif /* LISP_FLOAT_TYPE */
  defsubr (&Snatnump);
  defsubr (&Ssymbolp);
  defsubr (&Sstringp);
  defsubr (&Svectorp);
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
  defsubr (&Sdefine_function);
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
  defsubr (&Slocal_variable_p);
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

  XSYMBOL (Qwholenump)->function = XSYMBOL (Qnatnump)->function;
}

SIGTYPE
arith_error (signo)
     int signo;
{
#ifdef USG
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
