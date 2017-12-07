/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
   Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2017 Free Software
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


#include <config.h>

#include <math.h>
#include <stdio.h>

#include <byteswap.h>
#include <count-one-bits.h>
#include <count-trailing-zeros.h>
#include <intprops.h>

#include "lisp.h"
#include "puresize.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "process.h"
#include "frame.h"
#include "keymap.h"

static void swap_in_symval_forwarding (struct Lisp_Symbol *,
				       struct Lisp_Buffer_Local_Value *);

static bool
BOOLFWDP (union Lisp_Fwd *a)
{
  return XFWDTYPE (a) == Lisp_Fwd_Bool;
}
static bool
INTFWDP (union Lisp_Fwd *a)
{
  return XFWDTYPE (a) == Lisp_Fwd_Int;
}
static bool
KBOARD_OBJFWDP (union Lisp_Fwd *a)
{
  return XFWDTYPE (a) == Lisp_Fwd_Kboard_Obj;
}
static bool
OBJFWDP (union Lisp_Fwd *a)
{
  return XFWDTYPE (a) == Lisp_Fwd_Obj;
}

static struct Lisp_Boolfwd *
XBOOLFWD (union Lisp_Fwd *a)
{
  eassert (BOOLFWDP (a));
  return &a->u_boolfwd;
}
static struct Lisp_Kboard_Objfwd *
XKBOARD_OBJFWD (union Lisp_Fwd *a)
{
  eassert (KBOARD_OBJFWDP (a));
  return &a->u_kboard_objfwd;
}
static struct Lisp_Intfwd *
XINTFWD (union Lisp_Fwd *a)
{
  eassert (INTFWDP (a));
  return &a->u_intfwd;
}
static struct Lisp_Objfwd *
XOBJFWD (union Lisp_Fwd *a)
{
  eassert (OBJFWDP (a));
  return &a->u_objfwd;
}

static void
CHECK_SUBR (Lisp_Object x)
{
  CHECK_TYPE (SUBRP (x), Qsubrp, x);
}

static void
set_blv_found (struct Lisp_Buffer_Local_Value *blv, int found)
{
  eassert (found == !EQ (blv->defcell, blv->valcell));
  blv->found = found;
}

static Lisp_Object
blv_value (struct Lisp_Buffer_Local_Value *blv)
{
  return XCDR (blv->valcell);
}

static void
set_blv_value (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  XSETCDR (blv->valcell, val);
}

static void
set_blv_where (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  blv->where = val;
}

static void
set_blv_defcell (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  blv->defcell = val;
}

static void
set_blv_valcell (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  blv->valcell = val;
}

static _Noreturn void
wrong_length_argument (Lisp_Object a1, Lisp_Object a2, Lisp_Object a3)
{
  Lisp_Object size1 = make_number (bool_vector_size (a1));
  Lisp_Object size2 = make_number (bool_vector_size (a2));
  if (NILP (a3))
    xsignal2 (Qwrong_length_argument, size1, size2);
  else
    xsignal3 (Qwrong_length_argument, size1, size2,
	      make_number (bool_vector_size (a3)));
}

_Noreturn void
wrong_type_argument (register Lisp_Object predicate, register Lisp_Object value)
{
  /* If VALUE is not even a valid Lisp object, we'd want to abort here
     where we can get a backtrace showing where it came from.  We used
     to try and do that by checking the tagbits, but nowadays all
     tagbits are potentially valid.  */
  /* if ((unsigned int) XTYPE (value) >= Lisp_Type_Limit)
   *   emacs_abort (); */

  xsignal2 (Qwrong_type_argument, predicate, value);
}

void
pure_write_error (Lisp_Object obj)
{
  xsignal2 (Qerror, build_string ("Attempt to modify read-only object"), obj);
}

void
args_out_of_range (Lisp_Object a1, Lisp_Object a2)
{
  xsignal2 (Qargs_out_of_range, a1, a2);
}

void
args_out_of_range_3 (Lisp_Object a1, Lisp_Object a2, Lisp_Object a3)
{
  xsignal3 (Qargs_out_of_range, a1, a2, a3);
}

void
circular_list (Lisp_Object list)
{
  xsignal1 (Qcircular_list, list);
}


/* Data type predicates.  */

DEFUN ("eq", Feq, Seq, 2, 2, 0,
       doc: /* Return t if the two args are the same Lisp object.  */
       attributes: const)
  (Lisp_Object obj1, Lisp_Object obj2)
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}

DEFUN ("null", Fnull, Snull, 1, 1, 0,
       doc: /* Return t if OBJECT is nil, and return nil otherwise.  */
       attributes: const)
  (Lisp_Object object)
{
  if (NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("type-of", Ftype_of, Stype_of, 1, 1, 0,
       doc: /* Return a symbol representing the type of OBJECT.
The symbol returned names the object's basic type;
for example, (type-of 1) returns `integer'.  */)
  (Lisp_Object object)
{
  switch (XTYPE (object))
    {
    case_Lisp_Int:
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
        case Lisp_Misc_Finalizer:
          return Qfinalizer;
#ifdef HAVE_MODULES
	case Lisp_Misc_User_Ptr:
	  return Quser_ptr;
#endif
	default:
	  emacs_abort ();
	}

    case Lisp_Vectorlike:
      switch (PSEUDOVECTOR_TYPE (XVECTOR (object)))
        {
        case PVEC_NORMAL_VECTOR: return Qvector;
        case PVEC_WINDOW_CONFIGURATION: return Qwindow_configuration;
        case PVEC_PROCESS: return Qprocess;
        case PVEC_WINDOW: return Qwindow;
        case PVEC_SUBR: return Qsubr;
        case PVEC_COMPILED: return Qcompiled_function;
        case PVEC_BUFFER: return Qbuffer;
        case PVEC_CHAR_TABLE: return Qchar_table;
        case PVEC_BOOL_VECTOR: return Qbool_vector;
        case PVEC_FRAME: return Qframe;
        case PVEC_HASH_TABLE: return Qhash_table;
        case PVEC_FONT:
          if (FONT_SPEC_P (object))
	    return Qfont_spec;
          if (FONT_ENTITY_P (object))
	    return Qfont_entity;
          if (FONT_OBJECT_P (object))
	    return Qfont_object;
          else
            emacs_abort (); /* return Qfont?  */
        case PVEC_THREAD: return Qthread;
        case PVEC_MUTEX: return Qmutex;
        case PVEC_CONDVAR: return Qcondition_variable;
        case PVEC_TERMINAL: return Qterminal;
        case PVEC_RECORD:
          {
            Lisp_Object t = AREF (object, 0);
            if (RECORDP (t) && 1 < PVSIZE (t))
              /* Return the type name field of the class!  */
              return AREF (t, 1);
            else
              return t;
          }
        case PVEC_MODULE_FUNCTION:
          return Qmodule_function;
        /* "Impossible" cases.  */
        case PVEC_XWIDGET:
        case PVEC_OTHER:
        case PVEC_XWIDGET_VIEW:
        case PVEC_SUB_CHAR_TABLE:
        case PVEC_FREE: ;
        }
      emacs_abort ();

    case Lisp_Float:
      return Qfloat;

    default:
      emacs_abort ();
    }
}

DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0,
       doc: /* Return t if OBJECT is a cons cell.  */
       attributes: const)
  (Lisp_Object object)
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}

DEFUN ("atom", Fatom, Satom, 1, 1, 0,
       doc: /* Return t if OBJECT is not a cons cell.  This includes nil.  */
       attributes: const)
  (Lisp_Object object)
{
  if (CONSP (object))
    return Qnil;
  return Qt;
}

DEFUN ("listp", Flistp, Slistp, 1, 1, 0,
       doc: /* Return t if OBJECT is a list, that is, a cons cell or nil.
Otherwise, return nil.  */
       attributes: const)
  (Lisp_Object object)
{
  if (CONSP (object) || NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("nlistp", Fnlistp, Snlistp, 1, 1, 0,
       doc: /* Return t if OBJECT is not a list.  Lists include nil.  */
       attributes: const)
  (Lisp_Object object)
{
  if (CONSP (object) || NILP (object))
    return Qnil;
  return Qt;
}

DEFUN ("symbolp", Fsymbolp, Ssymbolp, 1, 1, 0,
       doc: /* Return t if OBJECT is a symbol.  */
       attributes: const)
  (Lisp_Object object)
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
  (Lisp_Object object)
{
  if (SYMBOLP (object)
      && SREF (SYMBOL_NAME (object), 0) == ':'
      && SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("vectorp", Fvectorp, Svectorp, 1, 1, 0,
       doc: /* Return t if OBJECT is a vector.  */)
  (Lisp_Object object)
{
  if (VECTORP (object))
    return Qt;
  return Qnil;
}

DEFUN ("recordp", Frecordp, Srecordp, 1, 1, 0,
       doc: /* Return t if OBJECT is a record.  */)
  (Lisp_Object object)
{
  if (RECORDP (object))
    return Qt;
  return Qnil;
}

DEFUN ("stringp", Fstringp, Sstringp, 1, 1, 0,
       doc: /* Return t if OBJECT is a string.  */
       attributes: const)
  (Lisp_Object object)
{
  if (STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("multibyte-string-p", Fmultibyte_string_p, Smultibyte_string_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a multibyte string.
Return nil if OBJECT is either a unibyte string, or not a string.  */)
  (Lisp_Object object)
{
  if (STRINGP (object) && STRING_MULTIBYTE (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-table-p", Fchar_table_p, Schar_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a char-table.  */)
  (Lisp_Object object)
{
  if (CHAR_TABLE_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("vector-or-char-table-p", Fvector_or_char_table_p,
       Svector_or_char_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a char-table or vector.  */)
  (Lisp_Object object)
{
  if (VECTORP (object) || CHAR_TABLE_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("bool-vector-p", Fbool_vector_p, Sbool_vector_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a bool-vector.  */)
  (Lisp_Object object)
{
  if (BOOL_VECTOR_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("arrayp", Farrayp, Sarrayp, 1, 1, 0,
       doc: /* Return t if OBJECT is an array (string or vector).  */)
  (Lisp_Object object)
{
  if (ARRAYP (object))
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
       doc: /* Return t if OBJECT is a sequence (list or array).  */)
  (register Lisp_Object object)
{
  if (CONSP (object) || NILP (object) || ARRAYP (object))
    return Qt;
  return Qnil;
}

DEFUN ("bufferp", Fbufferp, Sbufferp, 1, 1, 0,
       doc: /* Return t if OBJECT is an editor buffer.  */)
  (Lisp_Object object)
{
  if (BUFFERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("markerp", Fmarkerp, Smarkerp, 1, 1, 0,
       doc: /* Return t if OBJECT is a marker (editor pointer).  */)
  (Lisp_Object object)
{
  if (MARKERP (object))
    return Qt;
  return Qnil;
}

#ifdef HAVE_MODULES
DEFUN ("user-ptrp", Fuser_ptrp, Suser_ptrp, 1, 1, 0,
       doc: /* Return t if OBJECT is a module user pointer.  */)
     (Lisp_Object object)
{
  if (USER_PTRP (object))
    return Qt;
  return Qnil;
}
#endif

DEFUN ("subrp", Fsubrp, Ssubrp, 1, 1, 0,
       doc: /* Return t if OBJECT is a built-in function.  */)
  (Lisp_Object object)
{
  if (SUBRP (object))
    return Qt;
  return Qnil;
}

DEFUN ("byte-code-function-p", Fbyte_code_function_p, Sbyte_code_function_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a byte-compiled function object.  */)
  (Lisp_Object object)
{
  if (COMPILEDP (object))
    return Qt;
  return Qnil;
}

DEFUN ("module-function-p", Fmodule_function_p, Smodule_function_p, 1, 1, NULL,
       doc: /* Return t if OBJECT is a function loaded from a dynamic module.  */
       attributes: const)
  (Lisp_Object object)
{
  return MODULE_FUNCTIONP (object) ? Qt : Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, Schar_or_string_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a character or a string.  */
       attributes: const)
  (register Lisp_Object object)
{
  if (CHARACTERP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integerp", Fintegerp, Sintegerp, 1, 1, 0,
       doc: /* Return t if OBJECT is an integer.  */
       attributes: const)
  (Lisp_Object object)
{
  if (INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, Sinteger_or_marker_p, 1, 1, 0,
       doc: /* Return t if OBJECT is an integer or a marker (editor pointer).  */)
  (register Lisp_Object object)
{
  if (MARKERP (object) || INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("natnump", Fnatnump, Snatnump, 1, 1, 0,
       doc: /* Return t if OBJECT is a nonnegative integer.  */
       attributes: const)
  (Lisp_Object object)
{
  if (NATNUMP (object))
    return Qt;
  return Qnil;
}

DEFUN ("numberp", Fnumberp, Snumberp, 1, 1, 0,
       doc: /* Return t if OBJECT is a number (floating point or integer).  */
       attributes: const)
  (Lisp_Object object)
{
  if (NUMBERP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("number-or-marker-p", Fnumber_or_marker_p,
       Snumber_or_marker_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a number or a marker.  */)
  (Lisp_Object object)
{
  if (NUMBERP (object) || MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("floatp", Ffloatp, Sfloatp, 1, 1, 0,
       doc: /* Return t if OBJECT is a floating point number.  */
       attributes: const)
  (Lisp_Object object)
{
  if (FLOATP (object))
    return Qt;
  return Qnil;
}

DEFUN ("threadp", Fthreadp, Sthreadp, 1, 1, 0,
       doc: /* Return t if OBJECT is a thread.  */)
  (Lisp_Object object)
{
  if (THREADP (object))
    return Qt;
  return Qnil;
}

DEFUN ("mutexp", Fmutexp, Smutexp, 1, 1, 0,
       doc: /* Return t if OBJECT is a mutex.  */)
  (Lisp_Object object)
{
  if (MUTEXP (object))
    return Qt;
  return Qnil;
}

DEFUN ("condition-variable-p", Fcondition_variable_p, Scondition_variable_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a condition variable.  */)
  (Lisp_Object object)
{
  if (CONDVARP (object))
    return Qt;
  return Qnil;
}

/* Extract and set components of lists.  */

DEFUN ("car", Fcar, Scar, 1, 1, 0,
       doc: /* Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as car, cdr, cons cell and list.  */)
  (register Lisp_Object list)
{
  return CAR (list);
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
       doc: /* Return the car of OBJECT if it is a cons cell, or else nil.  */)
  (Lisp_Object object)
{
  return CAR_SAFE (object);
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
       doc: /* Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as cdr, car, cons cell and list.  */)
  (register Lisp_Object list)
{
  return CDR (list);
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
       doc: /* Return the cdr of OBJECT if it is a cons cell, or else nil.  */)
  (Lisp_Object object)
{
  return CDR_SAFE (object);
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
       doc: /* Set the car of CELL to be NEWCAR.  Returns NEWCAR.  */)
  (register Lisp_Object cell, Lisp_Object newcar)
{
  CHECK_CONS (cell);
  CHECK_IMPURE (cell, XCONS (cell));
  XSETCAR (cell, newcar);
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
       doc: /* Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.  */)
  (register Lisp_Object cell, Lisp_Object newcdr)
{
  CHECK_CONS (cell);
  CHECK_IMPURE (cell, XCONS (cell));
  XSETCDR (cell, newcdr);
  return newcdr;
}

/* Extract and set components of symbols.  */

DEFUN ("boundp", Fboundp, Sboundp, 1, 1, 0,
       doc: /* Return t if SYMBOL's value is not void.
Note that if `lexical-binding' is in effect, this refers to the
global value outside of any lexical scope.  */)
  (register Lisp_Object symbol)
{
  Lisp_Object valcontents;
  struct Lisp_Symbol *sym;
  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_PLAINVAL: valcontents = SYMBOL_VAL (sym); break;
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (blv->fwd)
	  /* In set_internal, we un-forward vars when their value is
	     set to Qunbound.  */
    	  return Qt;
	else
	  {
	    swap_in_symval_forwarding (sym, blv);
	    valcontents = blv_value (blv);
	  }
	break;
      }
    case SYMBOL_FORWARDED:
      /* In set_internal, we un-forward vars when their value is
	 set to Qunbound.  */
      return Qt;
    default: emacs_abort ();
    }

  return (EQ (valcontents, Qunbound) ? Qnil : Qt);
}

/* It has been previously suggested to make this function an alias for
   symbol-function, but upon discussion at Bug#23957, there is a risk
   breaking backward compatibility, as some users of fboundp may
   expect `t' in particular, rather than any true value.  */
DEFUN ("fboundp", Ffboundp, Sfboundp, 1, 1, 0,
       doc: /* Return t if SYMBOL's function definition is not void.  */)
  (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  return NILP (XSYMBOL (symbol)->u.s.function) ? Qnil : Qt;
}

DEFUN ("makunbound", Fmakunbound, Smakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's value be void.
Return SYMBOL.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (SYMBOL_CONSTANT_P (symbol))
    xsignal1 (Qsetting_constant, symbol);
  Fset (symbol, Qunbound);
  return symbol;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's function definition be nil.
Return SYMBOL.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    xsignal1 (Qsetting_constant, symbol);
  set_symbol_function (symbol, Qnil);
  return symbol;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
       doc: /* Return SYMBOL's function definition, or nil if that is void.  */)
  (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  return XSYMBOL (symbol)->u.s.function;
}

DEFUN ("symbol-plist", Fsymbol_plist, Ssymbol_plist, 1, 1, 0,
       doc: /* Return SYMBOL's property list.  */)
  (Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  return XSYMBOL (symbol)->u.s.plist;
}

DEFUN ("symbol-name", Fsymbol_name, Ssymbol_name, 1, 1, 0,
       doc: /* Return SYMBOL's name, a string.  */)
  (register Lisp_Object symbol)
{
  register Lisp_Object name;

  CHECK_SYMBOL (symbol);
  name = SYMBOL_NAME (symbol);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.  */)
  (register Lisp_Object symbol, Lisp_Object definition)
{
  register Lisp_Object function;
  CHECK_SYMBOL (symbol);
  /* Perhaps not quite the right error signal, but seems good enough.  */
  if (NILP (symbol))
    xsignal1 (Qsetting_constant, symbol);

  function = XSYMBOL (symbol)->u.s.function;

  if (!NILP (Vautoload_queue) && !NILP (function))
    Vautoload_queue = Fcons (Fcons (symbol, function), Vautoload_queue);

  if (AUTOLOADP (function))
    Fput (symbol, Qautoload, XCDR (function));

  /* Convert to eassert or remove after GC bug is found.  In the
     meantime, check unconditionally, at a slight perf hit.  */
  if (! valid_lisp_object_p (definition))
    emacs_abort ();

  set_symbol_function (symbol, definition);

  return definition;
}

DEFUN ("defalias", Fdefalias, Sdefalias, 2, 3, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION.
Associates the function with the current load file, if any.
The optional third argument DOCSTRING specifies the documentation string
for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
determined by DEFINITION.

Internally, this normally uses `fset', but if SYMBOL has a
`defalias-fset-function' property, the associated value is used instead.

The return value is undefined.  */)
  (register Lisp_Object symbol, Lisp_Object definition, Lisp_Object docstring)
{
  CHECK_SYMBOL (symbol);
  if (!NILP (Vpurify_flag)
      /* If `definition' is a keymap, immutable (and copying) is wrong.  */
      && !KEYMAPP (definition))
    definition = Fpurecopy (definition);

  {
    bool autoload = AUTOLOADP (definition);
    if (NILP (Vpurify_flag) || !autoload)
      { /* Only add autoload entries after dumping, because the ones before are
	   not useful and else we get loads of them from the loaddefs.el.  */

	if (AUTOLOADP (XSYMBOL (symbol)->u.s.function))
	  /* Remember that the function was already an autoload.  */
	  LOADHIST_ATTACH (Fcons (Qt, symbol));
	LOADHIST_ATTACH (Fcons (autoload ? Qautoload : Qdefun, symbol));
      }
  }

  { /* Handle automatic advice activation.  */
    Lisp_Object hook = Fget (symbol, Qdefalias_fset_function);
    if (!NILP (hook))
      call2 (hook, symbol, definition);
    else
      Ffset (symbol, definition);
  }

  if (!NILP (docstring))
    Fput (symbol, Qfunction_documentation, docstring);
  /* We used to return `definition', but now that `defun' and `defmacro' expand
     to a call to `defalias', we return `symbol' for backward compatibility
     (bug#11686).  */
  return symbol;
}

DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
       doc: /* Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.  */)
  (register Lisp_Object symbol, Lisp_Object newplist)
{
  CHECK_SYMBOL (symbol);
  set_symbol_plist (symbol, newplist);
  return newplist;
}

DEFUN ("subr-arity", Fsubr_arity, Ssubr_arity, 1, 1, 0,
       doc: /* Return minimum and maximum number of args allowed for SUBR.
SUBR must be a built-in function.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form.  */)
  (Lisp_Object subr)
{
  short minargs, maxargs;
  CHECK_SUBR (subr);
  minargs = XSUBR (subr)->min_args;
  maxargs = XSUBR (subr)->max_args;
  return Fcons (make_number (minargs),
		maxargs == MANY ?        Qmany
		: maxargs == UNEVALLED ? Qunevalled
		:                        make_number (maxargs));
}

DEFUN ("subr-name", Fsubr_name, Ssubr_name, 1, 1, 0,
       doc: /* Return name of subroutine SUBR.
SUBR must be a built-in function.  */)
  (Lisp_Object subr)
{
  const char *name;
  CHECK_SUBR (subr);
  name = XSUBR (subr)->symbol_name;
  return build_string (name);
}

DEFUN ("interactive-form", Finteractive_form, Sinteractive_form, 1, 1, 0,
       doc: /* Return the interactive form of CMD or nil if none.
If CMD is not a command, the return value is nil.
Value, if non-nil, is a list (interactive SPEC).  */)
  (Lisp_Object cmd)
{
  Lisp_Object fun = indirect_function (cmd); /* Check cycles.  */

  if (NILP (fun))
    return Qnil;

  /* Use an `interactive-form' property if present, analogous to the
     function-documentation property.  */
  fun = cmd;
  while (SYMBOLP (fun))
    {
      Lisp_Object tmp = Fget (fun, Qinteractive_form);
      if (!NILP (tmp))
	return tmp;
      else
	fun = Fsymbol_function (fun);
    }

  if (SUBRP (fun))
    {
      const char *spec = XSUBR (fun)->intspec;
      if (spec)
	return list2 (Qinteractive,
		      (*spec != '(') ? build_string (spec) :
		      Fcar (Fread_from_string (build_string (spec), Qnil, Qnil)));
    }
  else if (COMPILEDP (fun))
    {
      if (PVSIZE (fun) > COMPILED_INTERACTIVE)
	return list2 (Qinteractive, AREF (fun, COMPILED_INTERACTIVE));
    }
  else if (AUTOLOADP (fun))
    return Finteractive_form (Fautoload_do_load (fun, cmd, Qnil));
  else if (CONSP (fun))
    {
      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qclosure))
	return Fassq (Qinteractive, Fcdr (Fcdr (XCDR (fun))));
      else if (EQ (funcar, Qlambda))
	return Fassq (Qinteractive, Fcdr (XCDR (fun)));
    }
  return Qnil;
}


/***********************************************************************
		Getting and Setting Values of Symbols
 ***********************************************************************/

/* Return the symbol holding SYMBOL's value.  Signal
   `cyclic-variable-indirection' if SYMBOL's chain of variable
   indirections contains a loop.  */

struct Lisp_Symbol *
indirect_variable (struct Lisp_Symbol *symbol)
{
  struct Lisp_Symbol *tortoise, *hare;

  hare = tortoise = symbol;

  while (hare->u.s.redirect == SYMBOL_VARALIAS)
    {
      hare = SYMBOL_ALIAS (hare);
      if (hare->u.s.redirect != SYMBOL_VARALIAS)
	break;

      hare = SYMBOL_ALIAS (hare);
      tortoise = SYMBOL_ALIAS (tortoise);

      if (hare == tortoise)
	{
	  Lisp_Object tem;
	  XSETSYMBOL (tem, symbol);
	  xsignal1 (Qcyclic_variable_indirection, tem);
	}
    }

  return hare;
}


DEFUN ("indirect-variable", Findirect_variable, Sindirect_variable, 1, 1, 0,
       doc: /* Return the variable at the end of OBJECT's variable chain.
If OBJECT is a symbol, follow its variable indirections (if any), and
return the variable at the end of the chain of aliases.  See Info node
`(elisp)Variable Aliases'.

If OBJECT is not a symbol, just return it.  If there is a loop in the
chain of aliases, signal a `cyclic-variable-indirection' error.  */)
  (Lisp_Object object)
{
  if (SYMBOLP (object))
    {
      struct Lisp_Symbol *sym = indirect_variable (XSYMBOL (object));
      XSETSYMBOL (object, sym);
    }
  return object;
}


/* Given the raw contents of a symbol value cell,
   return the Lisp value of the symbol.
   This does not handle buffer-local variables; use
   swap_in_symval_forwarding for that.  */

Lisp_Object
do_symval_forwarding (register union Lisp_Fwd *valcontents)
{
  register Lisp_Object val;
  switch (XFWDTYPE (valcontents))
    {
    case Lisp_Fwd_Int:
      XSETINT (val, *XINTFWD (valcontents)->intvar);
      return val;

    case Lisp_Fwd_Bool:
      return (*XBOOLFWD (valcontents)->boolvar ? Qt : Qnil);

    case Lisp_Fwd_Obj:
      return *XOBJFWD (valcontents)->objvar;

    case Lisp_Fwd_Buffer_Obj:
      return per_buffer_value (current_buffer,
			       XBUFFER_OBJFWD (valcontents)->offset);

    case Lisp_Fwd_Kboard_Obj:
      /* We used to simply use current_kboard here, but from Lisp
	 code, its value is often unexpected.  It seems nicer to
	 allow constructions like this to work as intuitively expected:

	 (with-selected-frame frame
	 (define-key local-function-map "\eOP" [f1]))

	 On the other hand, this affects the semantics of
	 last-command and real-last-command, and people may rely on
	 that.  I took a quick look at the Lisp codebase, and I
	 don't think anything will break.  --lorentey  */
      return *(Lisp_Object *)(XKBOARD_OBJFWD (valcontents)->offset
			      + (char *)FRAME_KBOARD (SELECTED_FRAME ()));
    default: emacs_abort ();
    }
}

/* Used to signal a user-friendly error when symbol WRONG is
   not a member of CHOICE, which should be a list of symbols.  */

void
wrong_choice (Lisp_Object choice, Lisp_Object wrong)
{
  ptrdiff_t i = 0, len = XINT (Flength (choice));
  Lisp_Object obj, *args;
  AUTO_STRING (one_of, "One of ");
  AUTO_STRING (comma, ", ");
  AUTO_STRING (or, " or ");
  AUTO_STRING (should_be_specified, " should be specified");

  USE_SAFE_ALLOCA;
  SAFE_ALLOCA_LISP (args, len * 2 + 1);

  args[i++] = one_of;

  for (obj = choice; !NILP (obj); obj = XCDR (obj))
    {
      args[i++] = SYMBOL_NAME (XCAR (obj));
      args[i++] = (NILP (XCDR (obj)) ? should_be_specified
		   : NILP (XCDR (XCDR (obj))) ? or : comma);
    }

  obj = Fconcat (i, args);
  SAFE_FREE ();
  xsignal2 (Qerror, obj, wrong);
}

/* Used to signal a user-friendly error if WRONG is not a number or
   integer/floating-point number outsize of inclusive MIN..MAX range.  */

static void
wrong_range (Lisp_Object min, Lisp_Object max, Lisp_Object wrong)
{
  AUTO_STRING (value_should_be_from, "Value should be from ");
  AUTO_STRING (to, " to ");
  xsignal2 (Qerror,
	    CALLN (Fconcat, value_should_be_from, Fnumber_to_string (min),
		   to, Fnumber_to_string (max)),
	    wrong);
}

/* Store NEWVAL into SYMBOL, where VALCONTENTS is found in the value cell
   of SYMBOL.  If SYMBOL is buffer-local, VALCONTENTS should be the
   buffer-independent contents of the value cell: forwarded just one
   step past the buffer-localness.

   BUF non-zero means set the value in buffer BUF instead of the
   current buffer.  This only plays a role for per-buffer variables.  */

static void
store_symval_forwarding (union Lisp_Fwd *valcontents, register Lisp_Object newval, struct buffer *buf)
{
  switch (XFWDTYPE (valcontents))
    {
    case Lisp_Fwd_Int:
      CHECK_NUMBER (newval);
      *XINTFWD (valcontents)->intvar = XINT (newval);
      break;

    case Lisp_Fwd_Bool:
      *XBOOLFWD (valcontents)->boolvar = !NILP (newval);
      break;

    case Lisp_Fwd_Obj:
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

	  Lisp_Object tail, buf;

	  if (idx <= 0)
	    break;

	  FOR_EACH_LIVE_BUFFER (tail, buf)
	    {
	      struct buffer *b = XBUFFER (buf);

	      if (! PER_BUFFER_VALUE_P (b, idx))
		set_per_buffer_value (b, offset, newval);
	    }
	}
      break;

    case Lisp_Fwd_Buffer_Obj:
      {
	int offset = XBUFFER_OBJFWD (valcontents)->offset;
	Lisp_Object predicate = XBUFFER_OBJFWD (valcontents)->predicate;

	if (!NILP (newval))
	  {
	    if (SYMBOLP (predicate))
	      {
		Lisp_Object prop;

		if ((prop = Fget (predicate, Qchoice), !NILP (prop)))
		  {
		    if (NILP (Fmemq (newval, prop)))
		      wrong_choice (prop, newval);
		  }
		else if ((prop = Fget (predicate, Qrange), !NILP (prop)))
		  {
		    Lisp_Object min = XCAR (prop), max = XCDR (prop);
		    if (! NUMBERP (newval)
			|| NILP (CALLN (Fleq, min, newval, max)))
		      wrong_range (min, max, newval);
		  }
		else if (FUNCTIONP (predicate))
		  {
		    if (NILP (call1 (predicate, newval)))
		      wrong_type_argument (predicate, newval);
		  }
	      }
	  }
	if (buf == NULL)
	  buf = current_buffer;
	set_per_buffer_value (buf, offset, newval);
      }
      break;

    case Lisp_Fwd_Kboard_Obj:
      {
	char *base = (char *) FRAME_KBOARD (SELECTED_FRAME ());
	char *p = base + XKBOARD_OBJFWD (valcontents)->offset;
	*(Lisp_Object *) p = newval;
      }
      break;

    default:
      emacs_abort (); /* goto def; */
    }
}

/* Set up SYMBOL to refer to its global binding.  This makes it safe
   to alter the status of other bindings.  BEWARE: this may be called
   during the mark phase of GC, where we assume that Lisp_Object slots
   of BLV are marked after this function has changed them.  */

void
swap_in_global_binding (struct Lisp_Symbol *symbol)
{
  struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (symbol);

  /* Unload the previously loaded binding.  */
  if (blv->fwd)
    set_blv_value (blv, do_symval_forwarding (blv->fwd));

  /* Select the global binding in the symbol.  */
  set_blv_valcell (blv, blv->defcell);
  if (blv->fwd)
    store_symval_forwarding (blv->fwd, XCDR (blv->defcell), NULL);

  /* Indicate that the global binding is set up now.  */
  set_blv_where (blv, Qnil);
  set_blv_found (blv, 0);
}

/* Set up the buffer-local symbol SYMBOL for validity in the current buffer.
   VALCONTENTS is the contents of its value cell,
   which points to a struct Lisp_Buffer_Local_Value.

   Return the value forwarded one step past the buffer-local stage.
   This could be another forwarding pointer.  */

static void
swap_in_symval_forwarding (struct Lisp_Symbol *symbol, struct Lisp_Buffer_Local_Value *blv)
{
  register Lisp_Object tem1;

  eassert (blv == SYMBOL_BLV (symbol));

  tem1 = blv->where;

  if (NILP (tem1)
      || current_buffer != XBUFFER (tem1))
    {

      /* Unload the previously loaded binding.  */
      tem1 = blv->valcell;
      if (blv->fwd)
	set_blv_value (blv, do_symval_forwarding (blv->fwd));
      /* Choose the new binding.  */
      {
	Lisp_Object var;
	XSETSYMBOL (var, symbol);
	tem1 = assq_no_quit (var, BVAR (current_buffer, local_var_alist));
	set_blv_where (blv, Fcurrent_buffer ());
      }
      if (!(blv->found = !NILP (tem1)))
	tem1 = blv->defcell;

      /* Load the new binding.  */
      set_blv_valcell (blv, tem1);
      if (blv->fwd)
	store_symval_forwarding (blv->fwd, blv_value (blv), NULL);
    }
}

/* Find the value of a symbol, returning Qunbound if it's not bound.
   This is helpful for code which just wants to get a variable's value
   if it has one, without signaling an error.
   Note that it must not be possible to quit
   within this function.  Great care is required for this.  */

Lisp_Object
find_symbol_value (Lisp_Object symbol)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return SYMBOL_VAL (sym);
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	swap_in_symval_forwarding (sym, blv);
	return blv->fwd ? do_symval_forwarding (blv->fwd) : blv_value (blv);
      }
      /* FALLTHROUGH */
    case SYMBOL_FORWARDED:
      return do_symval_forwarding (SYMBOL_FWD (sym));
    default: emacs_abort ();
    }
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0,
       doc: /* Return SYMBOL's value.  Error if that is void.
Note that if `lexical-binding' is in effect, this returns the
global value outside of any lexical scope.  */)
  (Lisp_Object symbol)
{
  Lisp_Object val;

  val = find_symbol_value (symbol);
  if (!EQ (val, Qunbound))
    return val;

  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set", Fset, Sset, 2, 2, 0,
       doc: /* Set SYMBOL's value to NEWVAL, and return NEWVAL.  */)
  (register Lisp_Object symbol, Lisp_Object newval)
{
  set_internal (symbol, newval, Qnil, SET_INTERNAL_SET);
  return newval;
}

/* Store the value NEWVAL into SYMBOL.
   If buffer-locality is an issue, WHERE specifies which context to use.
   (nil stands for the current buffer/frame).

   If BINDFLAG is SET_INTERNAL_SET, then if this symbol is supposed to
   become local in every buffer where it is set, then we make it
   local.  If BINDFLAG is SET_INTERNAL_BIND or SET_INTERNAL_UNBIND, we
   don't do that.  */

void
set_internal (Lisp_Object symbol, Lisp_Object newval, Lisp_Object where,
              enum Set_Internal_Bind bindflag)
{
  bool voide = EQ (newval, Qunbound);
  struct Lisp_Symbol *sym;
  Lisp_Object tem1;

  /* If restoring in a dead buffer, do nothing.  */
  /* if (BUFFERP (where) && NILP (XBUFFER (where)->name))
      return; */

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);
  switch (sym->u.s.trapped_write)
    {
    case SYMBOL_NOWRITE:
      if (NILP (Fkeywordp (symbol))
          || !EQ (newval, Fsymbol_value (symbol)))
        xsignal1 (Qsetting_constant, symbol);
      else
        /* Allow setting keywords to their own value.  */
        return;

    case SYMBOL_TRAPPED_WRITE:
      /* Setting due to thread-switching doesn't count.  */
      if (bindflag != SET_INTERNAL_THREAD_SWITCH)
        notify_variable_watchers (symbol, voide? Qnil : newval,
                                  (bindflag == SET_INTERNAL_BIND? Qlet :
                                   bindflag == SET_INTERNAL_UNBIND? Qunlet :
                                   voide? Qmakunbound : Qset),
                                  where);
      /* FALLTHROUGH!  */
    case SYMBOL_UNTRAPPED_WRITE:
        break;

    default: emacs_abort ();
    }

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: SET_SYMBOL_VAL (sym , newval); return;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (NILP (where))
	  XSETBUFFER (where, current_buffer);

	/* If the current buffer is not the buffer whose binding is
	   loaded, or if it's a Lisp_Buffer_Local_Value and
	   the default binding is loaded, the loaded binding may be the
	   wrong one.  */
	if (!EQ (blv->where, where)
	    /* Also unload a global binding (if the var is local_if_set).  */
	    || (EQ (blv->valcell, blv->defcell)))
	  {
	    /* The currently loaded binding is not necessarily valid.
	       We need to unload it, and choose a new binding.  */

	    /* Write out `realvalue' to the old loaded binding.  */
	    if (blv->fwd)
	      set_blv_value (blv, do_symval_forwarding (blv->fwd));

	    /* Find the new binding.  */
	    XSETSYMBOL (symbol, sym); /* May have changed via aliasing.  */
	    tem1 = assq_no_quit (symbol,
				 BVAR (XBUFFER (where), local_var_alist));
	    set_blv_where (blv, where);
	    blv->found = 1;

	    if (NILP (tem1))
	      {
		/* This buffer still sees the default value.  */

		/* If the variable is a Lisp_Some_Buffer_Local_Value,
		   or if this is `let' rather than `set',
		   make CURRENT-ALIST-ELEMENT point to itself,
		   indicating that we're seeing the default value.
		   Likewise if the variable has been let-bound
		   in the current buffer.  */
		if (bindflag || !blv->local_if_set
		    || let_shadows_buffer_binding_p (sym))
		  {
		    blv->found = 0;
		    tem1 = blv->defcell;
		  }
		/* If it's a local_if_set, being set not bound,
		   and we're not within a let that was made for this buffer,
		   create a new buffer-local binding for the variable.
		   That means, give this buffer a new assoc for a local value
		   and load that binding.  */
		else
		  {
		    tem1 = Fcons (symbol, XCDR (blv->defcell));
		    bset_local_var_alist
		      (XBUFFER (where),
		       Fcons (tem1, BVAR (XBUFFER (where), local_var_alist)));
		  }
	      }

	    /* Record which binding is now loaded.  */
	    set_blv_valcell (blv, tem1);
	  }

	/* Store the new value in the cons cell.  */
	set_blv_value (blv, newval);

	if (blv->fwd)
	  {
	    if (voide)
	      /* If storing void (making the symbol void), forward only through
		 buffer-local indicator, not through Lisp_Objfwd, etc.  */
	      blv->fwd = NULL;
	    else
	      store_symval_forwarding (blv->fwd, newval,
				       BUFFERP (where)
				       ? XBUFFER (where) : current_buffer);
	  }
	break;
      }
    case SYMBOL_FORWARDED:
      {
	struct buffer *buf
	  = BUFFERP (where) ? XBUFFER (where) : current_buffer;
	union Lisp_Fwd *innercontents = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (innercontents))
	  {
	    int offset = XBUFFER_OBJFWD (innercontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);
	    if (idx > 0
                && bindflag == SET_INTERNAL_SET
		&& !let_shadows_buffer_binding_p (sym))
	      SET_PER_BUFFER_VALUE_P (buf, idx, 1);
	  }

	if (voide)
	  { /* If storing void (making the symbol void), forward only through
	       buffer-local indicator, not through Lisp_Objfwd, etc.  */
	    sym->u.s.redirect = SYMBOL_PLAINVAL;
	    SET_SYMBOL_VAL (sym, newval);
	  }
	else
	  store_symval_forwarding (/* sym, */ innercontents, newval, buf);
	break;
      }
    default: emacs_abort ();
    }
  return;
}

static void
set_symbol_trapped_write (Lisp_Object symbol, enum symbol_trapped_write trap)
{
  struct Lisp_Symbol *sym = XSYMBOL (symbol);
  if (sym->u.s.trapped_write == SYMBOL_NOWRITE)
    xsignal1 (Qtrapping_constant, symbol);
  sym->u.s.trapped_write = trap;
}

static void
restore_symbol_trapped_write (Lisp_Object symbol)
{
  set_symbol_trapped_write (symbol, SYMBOL_TRAPPED_WRITE);
}

static void
harmonize_variable_watchers (Lisp_Object alias, Lisp_Object base_variable)
{
  if (!EQ (base_variable, alias)
      && EQ (base_variable, Findirect_variable (alias)))
    set_symbol_trapped_write
      (alias, XSYMBOL (base_variable)->u.s.trapped_write);
}

DEFUN ("add-variable-watcher", Fadd_variable_watcher, Sadd_variable_watcher,
       2, 2, 0,
       doc: /* Cause WATCH-FUNCTION to be called when SYMBOL is set.

It will be called with 4 arguments: (SYMBOL NEWVAL OPERATION WHERE).
SYMBOL is the variable being changed.
NEWVAL is the value it will be changed to.
OPERATION is a symbol representing the kind of change, one of: `set',
`let', `unlet', `makunbound', and `defvaralias'.
WHERE is a buffer if the buffer-local value of the variable being
changed, nil otherwise.

All writes to aliases of SYMBOL will call WATCH-FUNCTION too.  */)
  (Lisp_Object symbol, Lisp_Object watch_function)
{
  symbol = Findirect_variable (symbol);
  set_symbol_trapped_write (symbol, SYMBOL_TRAPPED_WRITE);
  map_obarray (Vobarray, harmonize_variable_watchers, symbol);

  Lisp_Object watchers = Fget (symbol, Qwatchers);
  Lisp_Object member = Fmember (watch_function, watchers);
  if (NILP (member))
    Fput (symbol, Qwatchers, Fcons (watch_function, watchers));
  return Qnil;
}

DEFUN ("remove-variable-watcher", Fremove_variable_watcher, Sremove_variable_watcher,
       2, 2, 0,
       doc: /* Undo the effect of `add-variable-watcher'.
Remove WATCH-FUNCTION from the list of functions to be called when
SYMBOL (or its aliases) are set.  */)
  (Lisp_Object symbol, Lisp_Object watch_function)
{
  symbol = Findirect_variable (symbol);
  Lisp_Object watchers = Fget (symbol, Qwatchers);
  watchers = Fdelete (watch_function, watchers);
  if (NILP (watchers))
    {
      set_symbol_trapped_write (symbol, SYMBOL_UNTRAPPED_WRITE);
      map_obarray (Vobarray, harmonize_variable_watchers, symbol);
    }
  Fput (symbol, Qwatchers, watchers);
  return Qnil;
}

DEFUN ("get-variable-watchers", Fget_variable_watchers, Sget_variable_watchers,
       1, 1, 0,
       doc: /* Return a list of SYMBOL's active watchers.  */)
  (Lisp_Object symbol)
{
  return (SYMBOL_TRAPPED_WRITE_P (symbol) == SYMBOL_TRAPPED_WRITE)
    ? Fget (Findirect_variable (symbol), Qwatchers)
    : Qnil;
}

void
notify_variable_watchers (Lisp_Object symbol,
                          Lisp_Object newval,
                          Lisp_Object operation,
                          Lisp_Object where)
{
  symbol = Findirect_variable (symbol);

  ptrdiff_t count = SPECPDL_INDEX ();
  record_unwind_protect (restore_symbol_trapped_write, symbol);
  /* Avoid recursion.  */
  set_symbol_trapped_write (symbol, SYMBOL_UNTRAPPED_WRITE);

  if (NILP (where)
      && !EQ (operation, Qset_default) && !EQ (operation, Qmakunbound)
      && !NILP (Flocal_variable_if_set_p (symbol, Fcurrent_buffer ())))
    {
      XSETBUFFER (where, current_buffer);
    }

  if (EQ (operation, Qset_default))
    operation = Qset;

  for (Lisp_Object watchers = Fget (symbol, Qwatchers);
       CONSP (watchers);
       watchers = XCDR (watchers))
    {
      Lisp_Object watcher = XCAR (watchers);
      /* Call subr directly to avoid gc.  */
      if (SUBRP (watcher))
        {
          Lisp_Object args[] = { symbol, newval, operation, where };
          funcall_subr (XSUBR (watcher), ARRAYELTS (args), args);
        }
      else
        CALLN (Ffuncall, watcher, symbol, newval, operation, where);
    }

  unbind_to (count, Qnil);
}


/* Access or set a buffer-local symbol's default value.  */

/* Return the default value of SYMBOL, but don't check for voidness.
   Return Qunbound if it is void.  */

static Lisp_Object
default_value (Lisp_Object symbol)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return SYMBOL_VAL (sym);
    case SYMBOL_LOCALIZED:
      {
	/* If var is set up for a buffer that lacks a local value for it,
	   the current value is nominally the default value.
	   But the `realvalue' slot may be more up to date, since
	   ordinary setq stores just that slot.  So use that.  */
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (blv->fwd && EQ (blv->valcell, blv->defcell))
	  return do_symval_forwarding (blv->fwd);
	else
	  return XCDR (blv->defcell);
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);

	/* For a built-in buffer-local variable, get the default value
	   rather than letting do_symval_forwarding get the current value.  */
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    if (PER_BUFFER_IDX (offset) != 0)
	      return per_buffer_default (offset);
	  }

	/* For other variables, get the current value.  */
	return do_symval_forwarding (valcontents);
      }
    default: emacs_abort ();
    }
}

DEFUN ("default-boundp", Fdefault_boundp, Sdefault_boundp, 1, 1, 0,
       doc: /* Return t if SYMBOL has a non-void default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  */)
  (Lisp_Object symbol)
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
  (Lisp_Object symbol)
{
  Lisp_Object value = default_value (symbol);
  if (!EQ (value, Qunbound))
    return value;

  xsignal1 (Qvoid_variable, symbol);
}

void
set_default_internal (Lisp_Object symbol, Lisp_Object value,
                      enum Set_Internal_Bind bindflag)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);
  switch (sym->u.s.trapped_write)
    {
    case SYMBOL_NOWRITE:
      if (NILP (Fkeywordp (symbol))
          || !EQ (value, Fsymbol_value (symbol)))
        xsignal1 (Qsetting_constant, symbol);
      else
        /* Allow setting keywords to their own value.  */
        return;

    case SYMBOL_TRAPPED_WRITE:
      /* Don't notify here if we're going to call Fset anyway.  */
      if (sym->u.s.redirect != SYMBOL_PLAINVAL
          /* Setting due to thread switching doesn't count.  */
          && bindflag != SET_INTERNAL_THREAD_SWITCH)
        notify_variable_watchers (symbol, value, Qset_default, Qnil);
      /* FALLTHROUGH!  */
    case SYMBOL_UNTRAPPED_WRITE:
        break;

    default: emacs_abort ();
    }

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: set_internal (symbol, value, Qnil, bindflag); return;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);

	/* Store new value into the DEFAULT-VALUE slot.  */
	XSETCDR (blv->defcell, value);

	/* If the default binding is now loaded, set the REALVALUE slot too.  */
	if (blv->fwd && EQ (blv->defcell, blv->valcell))
	  store_symval_forwarding (blv->fwd, value, NULL);
        return;
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);

	/* Handle variables like case-fold-search that have special slots
	   in the buffer.
	   Make them work apparently like Lisp_Buffer_Local_Value variables.  */
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);

	    set_per_buffer_default (offset, value);

	    /* If this variable is not always local in all buffers,
	       set it in the buffers that don't nominally have a local value.  */
	    if (idx > 0)
	      {
		struct buffer *b;

		FOR_EACH_BUFFER (b)
		  if (!PER_BUFFER_VALUE_P (b, idx))
		    set_per_buffer_value (b, offset, value);
	      }
	  }
	else
          set_internal (symbol, value, Qnil, bindflag);
        return;
      }
    default: emacs_abort ();
    }
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
       doc: /* Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
The default value is seen in buffers that do not have their own values
for this variable.  */)
  (Lisp_Object symbol, Lisp_Object value)
{
  set_default_internal (symbol, value, SET_INTERNAL_SET);
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
  (Lisp_Object args)
{
  Lisp_Object args_left, symbol, val;

  args_left = val = args;

  while (CONSP (args_left))
    {
      val = eval_sub (Fcar (XCDR (args_left)));
      symbol = XCAR (args_left);
      Fset_default (symbol, val);
      args_left = Fcdr (XCDR (args_left));
    }

  return val;
}

/* Lisp functions for creating and removing buffer-local variables.  */

union Lisp_Val_Fwd
  {
    Lisp_Object value;
    union Lisp_Fwd *fwd;
  };

static struct Lisp_Buffer_Local_Value *
make_blv (struct Lisp_Symbol *sym, bool forwarded,
	  union Lisp_Val_Fwd valcontents)
{
  struct Lisp_Buffer_Local_Value *blv = xmalloc (sizeof *blv);
  Lisp_Object symbol;
  Lisp_Object tem;

 XSETSYMBOL (symbol, sym);
 tem = Fcons (symbol, (forwarded
                       ? do_symval_forwarding (valcontents.fwd)
                       : valcontents.value));

  /* Buffer_Local_Values cannot have as realval a buffer-local
     or keyboard-local forwarding.  */
  eassert (!(forwarded && BUFFER_OBJFWDP (valcontents.fwd)));
  eassert (!(forwarded && KBOARD_OBJFWDP (valcontents.fwd)));
  blv->fwd = forwarded ? valcontents.fwd : NULL;
  set_blv_where (blv, Qnil);
  blv->local_if_set = 0;
  set_blv_defcell (blv, tem);
  set_blv_valcell (blv, tem);
  set_blv_found (blv, 0);
  return blv;
}

DEFUN ("make-variable-buffer-local", Fmake_variable_buffer_local,
       Smake_variable_buffer_local, 1, 1, "vMake Variable Buffer Local: ",
       doc: /* Make VARIABLE become buffer-local whenever it is set.
At any time, the value for the current buffer is in effect,
unless the variable has never been set in this buffer,
in which case the default value is in effect.
Note that binding the variable with `let', or setting it while
a `let'-style binding made in this buffer is in effect,
does not make the variable buffer-local.  Return VARIABLE.

This globally affects all uses of this variable, so it belongs together with
the variable declaration, rather than with its uses (if you just want to make
a variable local to the current buffer for one particular use, use
`make-local-variable').  Buffer-local bindings are normally cleared
while setting up a new major mode, unless they have a `permanent-local'
property.

The function `default-value' gets the default value and `set-default' sets it.  */)
  (register Lisp_Object variable)
{
  struct Lisp_Symbol *sym;
  struct Lisp_Buffer_Local_Value *blv = NULL;
  union Lisp_Val_Fwd valcontents;
  bool forwarded UNINIT;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL:
      forwarded = 0; valcontents.value = SYMBOL_VAL (sym);
      if (EQ (valcontents.value, Qunbound))
	valcontents.value = Qnil;
      break;
    case SYMBOL_LOCALIZED:
      blv = SYMBOL_BLV (sym);
      break;
    case SYMBOL_FORWARDED:
      forwarded = 1; valcontents.fwd = SYMBOL_FWD (sym);
      if (KBOARD_OBJFWDP (valcontents.fwd))
	error ("Symbol %s may not be buffer-local",
	       SDATA (SYMBOL_NAME (variable)));
      else if (BUFFER_OBJFWDP (valcontents.fwd))
	return variable;
      break;
    default: emacs_abort ();
    }

  if (SYMBOL_CONSTANT_P (variable))
    xsignal1 (Qsetting_constant, variable);

  if (!blv)
    {
      blv = make_blv (sym, forwarded, valcontents);
      sym->u.s.redirect = SYMBOL_LOCALIZED;
      SET_SYMBOL_BLV (sym, blv);
    }

  blv->local_if_set = 1;
  return variable;
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
       1, 1, "vMake Local Variable: ",
       doc: /* Make VARIABLE have a separate value in the current buffer.
Other buffers will continue to share a common default value.
\(The buffer-local value of VARIABLE starts out as the same value
VARIABLE previously had.  If VARIABLE was void, it remains void.)
Return VARIABLE.

If the variable is already arranged to become local when set,
this function causes a local value to exist for this buffer,
just as setting the variable would do.

This function returns VARIABLE, and therefore
  (set (make-local-variable \\='VARIABLE) VALUE-EXP)
works.

See also `make-variable-buffer-local'.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.  */)
  (Lisp_Object variable)
{
  Lisp_Object tem;
  bool forwarded UNINIT;
  union Lisp_Val_Fwd valcontents;
  struct Lisp_Symbol *sym;
  struct Lisp_Buffer_Local_Value *blv = NULL;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL:
      forwarded = 0; valcontents.value = SYMBOL_VAL (sym); break;
    case SYMBOL_LOCALIZED:
      blv = SYMBOL_BLV (sym);
      break;
    case SYMBOL_FORWARDED:
      forwarded = 1; valcontents.fwd = SYMBOL_FWD (sym);
      if (KBOARD_OBJFWDP (valcontents.fwd))
	error ("Symbol %s may not be buffer-local",
	       SDATA (SYMBOL_NAME (variable)));
      break;
    default: emacs_abort ();
    }

  if (sym->u.s.trapped_write == SYMBOL_NOWRITE)
    xsignal1 (Qsetting_constant, variable);

  if (blv ? blv->local_if_set
      : (forwarded && BUFFER_OBJFWDP (valcontents.fwd)))
    {
      tem = Fboundp (variable);
      /* Make sure the symbol has a local value in this particular buffer,
	 by setting it to the same value it already has.  */
      Fset (variable, (EQ (tem, Qt) ? Fsymbol_value (variable) : Qunbound));
      return variable;
    }
  if (!blv)
    {
      blv = make_blv (sym, forwarded, valcontents);
      sym->u.s.redirect = SYMBOL_LOCALIZED;
      SET_SYMBOL_BLV (sym, blv);
    }

  /* Make sure this buffer has its own value of symbol.  */
  XSETSYMBOL (variable, sym);	/* Update in case of aliasing.  */
  tem = Fassq (variable, BVAR (current_buffer, local_var_alist));
  if (NILP (tem))
    {
      if (let_shadows_buffer_binding_p (sym))
	{
	  AUTO_STRING (format,
		       "Making %s buffer-local while locally let-bound!");
	  CALLN (Fmessage, format, SYMBOL_NAME (variable));
	}

      /* Swap out any local binding for some other buffer, and make
	 sure the current value is permanently recorded, if it's the
	 default value.  */
      find_symbol_value (variable);

      bset_local_var_alist
	(current_buffer,
	 Fcons (Fcons (variable, XCDR (blv->defcell)),
		BVAR (current_buffer, local_var_alist)));

      /* Make sure symbol does not think it is set up for this buffer;
	 force it to look once again for this buffer's value.  */
      if (current_buffer == XBUFFER (blv->where))
	set_blv_where (blv, Qnil);
      set_blv_found (blv, 0);
    }

  /* If the symbol forwards into a C variable, then load the binding
     for this buffer now.  If C code modifies the variable before we
     load the binding in, then that new value will clobber the default
     binding the next time we unload it.  */
  if (blv->fwd)
    swap_in_symval_forwarding (sym, blv);

  return variable;
}

DEFUN ("kill-local-variable", Fkill_local_variable, Skill_local_variable,
       1, 1, "vKill Local Variable: ",
       doc: /* Make VARIABLE no longer have a separate value in the current buffer.
From now on the default value will apply in this buffer.  Return VARIABLE.  */)
  (register Lisp_Object variable)
{
  register Lisp_Object tem;
  struct Lisp_Buffer_Local_Value *blv;
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return variable;
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);

	    if (idx > 0)
	      {
		SET_PER_BUFFER_VALUE_P (current_buffer, idx, 0);
		set_per_buffer_value (current_buffer, offset,
				      per_buffer_default (offset));
	      }
	  }
	return variable;
      }
    case SYMBOL_LOCALIZED:
      blv = SYMBOL_BLV (sym);
      break;
    default: emacs_abort ();
    }

  if (sym->u.s.trapped_write == SYMBOL_TRAPPED_WRITE)
    notify_variable_watchers (variable, Qnil, Qmakunbound, Fcurrent_buffer ());

  /* Get rid of this buffer's alist element, if any.  */
  XSETSYMBOL (variable, sym);	/* Propagate variable indirection.  */
  tem = Fassq (variable, BVAR (current_buffer, local_var_alist));
  if (!NILP (tem))
    bset_local_var_alist
      (current_buffer,
       Fdelq (tem, BVAR (current_buffer, local_var_alist)));

  /* If the symbol is set up with the current buffer's binding
     loaded, recompute its value.  We have to do it now, or else
     forwarded objects won't work right.  */
  {
    Lisp_Object buf; XSETBUFFER (buf, current_buffer);
    if (EQ (buf, blv->where))
      {
	set_blv_where (blv, Qnil);
	blv->found = 0;
	find_symbol_value (variable);
      }
  }

  return variable;
}

/* Lisp functions for creating and removing buffer-local variables.  */

DEFUN ("local-variable-p", Flocal_variable_p, Slocal_variable_p,
       1, 2, 0,
       doc: /* Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer.  */)
  (Lisp_Object variable, Lisp_Object buffer)
{
  struct buffer *buf = decode_buffer (buffer);
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Qnil;
    case SYMBOL_LOCALIZED:
      {
	Lisp_Object tail, elt, tmp;
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	XSETBUFFER (tmp, buf);
	XSETSYMBOL (variable, sym); /* Update in case of aliasing.  */

	if (EQ (blv->where, tmp)) /* The binding is already loaded.  */
	  return blv_found (blv) ? Qt : Qnil;
	else
	  for (tail = BVAR (buf, local_var_alist); CONSP (tail); tail = XCDR (tail))
	    {
	      elt = XCAR (tail);
	      if (EQ (variable, XCAR (elt)))
		return Qt;
	    }
	return Qnil;
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);
	    if (idx == -1 || PER_BUFFER_VALUE_P (buf, idx))
	      return Qt;
	  }
	return Qnil;
      }
    default: emacs_abort ();
    }
}

DEFUN ("local-variable-if-set-p", Flocal_variable_if_set_p, Slocal_variable_if_set_p,
       1, 2, 0,
       doc: /* Non-nil if VARIABLE is local in buffer BUFFER when set there.
BUFFER defaults to the current buffer.

More precisely, return non-nil if either VARIABLE already has a local
value in BUFFER, or if VARIABLE is automatically buffer-local (see
`make-variable-buffer-local').  */)
  (register Lisp_Object variable, Lisp_Object buffer)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Qnil;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (blv->local_if_set)
	  return Qt;
	XSETSYMBOL (variable, sym); /* Update in case of aliasing.  */
	return Flocal_variable_p (variable, buffer);
      }
    case SYMBOL_FORWARDED:
      /* All BUFFER_OBJFWD slots become local if they are set.  */
      return (BUFFER_OBJFWDP (SYMBOL_FWD (sym)) ? Qt : Qnil);
    default: emacs_abort ();
    }
}

DEFUN ("variable-binding-locus", Fvariable_binding_locus, Svariable_binding_locus,
       1, 1, 0,
       doc: /* Return a value indicating where VARIABLE's current binding comes from.
If the current binding is buffer-local, the value is the current buffer.
If the current binding is global (the default), the value is nil.  */)
  (register Lisp_Object variable)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

  /* Make sure the current binding is actually swapped in.  */
  find_symbol_value (variable);

 start:
  switch (sym->u.s.redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Qnil;
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);
	if (KBOARD_OBJFWDP (valcontents))
	  return Fframe_terminal (selected_frame);
	else if (!BUFFER_OBJFWDP (valcontents))
	  return Qnil;
      }
      FALLTHROUGH;
    case SYMBOL_LOCALIZED:
      /* For a local variable, record both the symbol and which
	 buffer's or frame's value we are saving.  */
      if (!NILP (Flocal_variable_p (variable, Qnil)))
	return Fcurrent_buffer ();
      else if (sym->u.s.redirect == SYMBOL_LOCALIZED
	       && blv_found (SYMBOL_BLV (sym)))
	return SYMBOL_BLV (sym)->where;
      else
	return Qnil;
    default: emacs_abort ();
    }
}

/* This code is disabled now that we use the selected frame to return
   keyboard-local-values.  */
#if 0
extern struct terminal *get_terminal (Lisp_Object display, int);

DEFUN ("terminal-local-value", Fterminal_local_value,
       Sterminal_local_value, 2, 2, 0,
       doc: /* Return the terminal-local value of SYMBOL on TERMINAL.
If SYMBOL is not a terminal-local variable, then return its normal
value, like `symbol-value'.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal device).  */)
  (Lisp_Object symbol, Lisp_Object terminal)
{
  Lisp_Object result;
  struct terminal *t = get_terminal (terminal, 1);
  push_kboard (t->kboard);
  result = Fsymbol_value (symbol);
  pop_kboard ();
  return result;
}

DEFUN ("set-terminal-local-value", Fset_terminal_local_value,
       Sset_terminal_local_value, 3, 3, 0,
       doc: /* Set the terminal-local binding of SYMBOL on TERMINAL to VALUE.
If VARIABLE is not a terminal-local variable, then set its normal
binding, like `set'.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal device).  */)
  (Lisp_Object symbol, Lisp_Object terminal, Lisp_Object value)
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
indirect_function (register Lisp_Object object)
{
  Lisp_Object tortoise, hare;

  hare = tortoise = object;

  for (;;)
    {
      if (!SYMBOLP (hare) || NILP (hare))
	break;
      hare = XSYMBOL (hare)->u.s.function;
      if (!SYMBOLP (hare) || NILP (hare))
	break;
      hare = XSYMBOL (hare)->u.s.function;

      tortoise = XSYMBOL (tortoise)->u.s.function;

      if (EQ (hare, tortoise))
	xsignal1 (Qcyclic_function_indirection, object);
    }

  return hare;
}

DEFUN ("indirect-function", Findirect_function, Sindirect_function, 1, 2, 0,
       doc: /* Return the function at the end of OBJECT's function chain.
If OBJECT is not a symbol, just return it.  Otherwise, follow all
function indirections to find the final function binding and return it.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.  */)
  (register Lisp_Object object, Lisp_Object noerror)
{
  Lisp_Object result;

  /* Optimize for no indirection.  */
  result = object;
  if (SYMBOLP (result) && !NILP (result)
      && (result = XSYMBOL (result)->u.s.function, SYMBOLP (result)))
    result = indirect_function (result);
  if (!NILP (result))
    return result;

  return Qnil;
}

/* Extract and set vector and string elements.  */

DEFUN ("aref", Faref, Saref, 2, 2, 0,
       doc: /* Return the element of ARG at index IDX.
ARG may be a vector, a string, a char-table, a bool-vector, a record,
or a byte-code object.  IDX starts at 0.  */)
  (register Lisp_Object array, Lisp_Object idx)
{
  register EMACS_INT idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  if (STRINGP (array))
    {
      int c;
      ptrdiff_t idxval_byte;

      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      if (! STRING_MULTIBYTE (array))
	return make_number ((unsigned char) SREF (array, idxval));
      idxval_byte = string_char_to_byte (array, idxval);

      c = STRING_CHAR (SDATA (array) + idxval_byte);
      return make_number (c);
    }
  else if (BOOL_VECTOR_P (array))
    {
      if (idxval < 0 || idxval >= bool_vector_size (array))
	args_out_of_range (array, idx);
      return bool_vector_ref (array, idxval);
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      return CHAR_TABLE_REF (array, idxval);
    }
  else
    {
      ptrdiff_t size = 0;
      if (VECTORP (array))
	size = ASIZE (array);
      else if (COMPILEDP (array) || RECORDP (array))
	size = PVSIZE (array);
      else
	wrong_type_argument (Qarrayp, array);

      if (idxval < 0 || idxval >= size)
	args_out_of_range (array, idx);
      return AREF (array, idxval);
    }
}

DEFUN ("aset", Faset, Saset, 3, 3, 0,
       doc: /* Store into the element of ARRAY at index IDX the value NEWELT.
Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
bool-vector.  IDX starts at 0.  */)
  (register Lisp_Object array, Lisp_Object idx, Lisp_Object newelt)
{
  register EMACS_INT idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  if (! RECORDP (array))
    CHECK_ARRAY (array, Qarrayp);

  if (VECTORP (array))
    {
      CHECK_IMPURE (array, XVECTOR (array));
      if (idxval < 0 || idxval >= ASIZE (array))
	args_out_of_range (array, idx);
      ASET (array, idxval, newelt);
    }
  else if (BOOL_VECTOR_P (array))
    {
      if (idxval < 0 || idxval >= bool_vector_size (array))
	args_out_of_range (array, idx);
      bool_vector_set (array, idxval, !NILP (newelt));
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      CHAR_TABLE_SET (array, idxval, newelt);
    }
  else if (RECORDP (array))
    {
      if (idxval < 0 || idxval >= PVSIZE (array))
	args_out_of_range (array, idx);
      ASET (array, idxval, newelt);
    }
  else /* STRINGP */
    {
      int c;

      CHECK_IMPURE (array, XSTRING (array));
      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      CHECK_CHARACTER (newelt);
      c = XFASTINT (newelt);

      if (STRING_MULTIBYTE (array))
	{
	  ptrdiff_t idxval_byte, nbytes;
	  int prev_bytes, new_bytes;
	  unsigned char workbuf[MAX_MULTIBYTE_LENGTH], *p0 = workbuf, *p1;

	  nbytes = SBYTES (array);
	  idxval_byte = string_char_to_byte (array, idxval);
	  p1 = SDATA (array) + idxval_byte;
	  prev_bytes = BYTES_BY_CHAR_HEAD (*p1);
	  new_bytes = CHAR_STRING (c, p0);
	  if (prev_bytes != new_bytes)
	    {
	      /* We must relocate the string data.  */
	      ptrdiff_t nchars = SCHARS (array);
	      USE_SAFE_ALLOCA;
	      unsigned char *str = SAFE_ALLOCA (nbytes);

	      memcpy (str, SDATA (array), nbytes);
	      allocate_string_data (XSTRING (array), nchars,
				    nbytes + new_bytes - prev_bytes);
	      memcpy (SDATA (array), str, idxval_byte);
	      p1 = SDATA (array) + idxval_byte;
	      memcpy (p1 + new_bytes, str + idxval_byte + prev_bytes,
		      nbytes - (idxval_byte + prev_bytes));
	      SAFE_FREE ();
	      clear_string_char_byte_cache ();
	    }
	  while (new_bytes--)
	    *p1++ = *p0++;
	}
      else
	{
	  if (! SINGLE_BYTE_CHAR_P (c))
	    {
	      ptrdiff_t i;

	      for (i = SBYTES (array) - 1; i >= 0; i--)
		if (SREF (array, i) >= 0x80)
		  args_out_of_range (array, newelt);
	      /* ARRAY is an ASCII string.  Convert it to a multibyte
		 string, and try `aset' again.  */
	      STRING_SET_MULTIBYTE (array);
	      return Faset (array, idx, newelt);
	    }
	  SSET (array, idxval, c);
	}
    }

  return newelt;
}

/* Arithmetic functions */

Lisp_Object
arithcompare (Lisp_Object num1, Lisp_Object num2,
	      enum Arith_Comparison comparison)
{
  double f1, f2;
  EMACS_INT i1, i2;
  bool fneq;
  bool test;

  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num1);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num2);

  /* If either arg is floating point, set F1 and F2 to the 'double'
     approximations of the two arguments, and set FNEQ if floating-point
     comparison reports that F1 is not equal to F2, possibly because F1
     or F2 is a NaN.  Regardless, set I1 and I2 to integers that break
     ties if the floating-point comparison is either not done or reports
     equality.  */

  if (FLOATP (num1))
    {
      f1 = XFLOAT_DATA (num1);
      if (FLOATP (num2))
	{
	  i1 = i2 = 0;
	  f2 = XFLOAT_DATA (num2);
	}
      else
	{
	  /* Compare a float NUM1 to an integer NUM2 by converting the
	     integer I2 (i.e., NUM2) to the double F2 (a conversion that
	     can round on some platforms, if I2 is large enough), and then
	     converting F2 back to the integer I1 (a conversion that is
	     always exact), so that I1 exactly equals ((double) NUM2).  If
	     floating-point comparison reports a tie, NUM1 = F1 = F2 = I1
	     (exactly) so I1 - I2 = NUM1 - NUM2 (exactly), so comparing I1
	     to I2 will break the tie correctly.  */
	  i1 = f2 = i2 = XINT (num2);
	}
      fneq = f1 != f2;
    }
  else
    {
      i1 = XINT (num1);
      if (FLOATP (num2))
	{
	  /* Compare an integer NUM1 to a float NUM2.  This is the
	     converse of comparing float to integer (see above).  */
	  i2 = f1 = i1;
	  f2 = XFLOAT_DATA (num2);
	  fneq = f1 != f2;
	}
      else
	{
	  i2 = XINT (num2);
	  fneq = false;
	}
    }

  switch (comparison)
    {
    case ARITH_EQUAL:
      test = !fneq && i1 == i2;
      break;

    case ARITH_NOTEQUAL:
      test = fneq || i1 != i2;
      break;

    case ARITH_LESS:
      test = fneq ? f1 < f2 : i1 < i2;
      break;

    case ARITH_LESS_OR_EQUAL:
      test = fneq ? f1 <= f2 : i1 <= i2;
      break;

    case ARITH_GRTR:
      test = fneq ? f1 > f2 : i1 > i2;
      break;

    case ARITH_GRTR_OR_EQUAL:
      test = fneq ? f1 >= f2 : i1 >= i2;
      break;

    default:
      eassume (false);
    }

  return test ? Qt : Qnil;
}

static Lisp_Object
arithcompare_driver (ptrdiff_t nargs, Lisp_Object *args,
                     enum Arith_Comparison comparison)
{
  for (ptrdiff_t i = 1; i < nargs; i++)
    if (NILP (arithcompare (args[i - 1], args[i], comparison)))
      return Qnil;
  return Qt;
}

DEFUN ("=", Feqlsign, Seqlsign, 1, MANY, 0,
       doc: /* Return t if args, all numbers or markers, are equal.
usage: (= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arithcompare_driver (nargs, args, ARITH_EQUAL);
}

DEFUN ("<", Flss, Slss, 1, MANY, 0,
       doc: /* Return t if each arg (a number or marker), is less than the next arg.
usage: (< NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arithcompare_driver (nargs, args, ARITH_LESS);
}

DEFUN (">", Fgtr, Sgtr, 1, MANY, 0,
       doc: /* Return t if each arg (a number or marker) is greater than the next arg.
usage: (> NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arithcompare_driver (nargs, args, ARITH_GRTR);
}

DEFUN ("<=", Fleq, Sleq, 1, MANY, 0,
       doc: /* Return t if each arg (a number or marker) is less than or equal to the next.
usage: (<= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arithcompare_driver (nargs, args, ARITH_LESS_OR_EQUAL);
}

DEFUN (">=", Fgeq, Sgeq, 1, MANY, 0,
       doc: /* Return t if each arg (a number or marker) is greater than or equal to the next.
usage: (>= NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arithcompare_driver (nargs, args, ARITH_GRTR_OR_EQUAL);
}

DEFUN ("/=", Fneq, Sneq, 2, 2, 0,
       doc: /* Return t if first arg is not equal to second arg.  Both must be numbers or markers.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, ARITH_NOTEQUAL);
}

/* Convert the integer I to a cons-of-integers, where I is not in
   fixnum range.  */

#define INTBIG_TO_LISP(i, extremum)				    \
  (eassert (FIXNUM_OVERFLOW_P (i)),				    \
   (! (FIXNUM_OVERFLOW_P ((extremum) >> 16)			    \
       && FIXNUM_OVERFLOW_P ((i) >> 16))			    \
    ? Fcons (make_number ((i) >> 16), make_number ((i) & 0xffff))   \
    : ! (FIXNUM_OVERFLOW_P ((extremum) >> 16 >> 24)		    \
	 && FIXNUM_OVERFLOW_P ((i) >> 16 >> 24))		    \
    ? Fcons (make_number ((i) >> 16 >> 24),			    \
	     Fcons (make_number ((i) >> 16 & 0xffffff),		    \
		    make_number ((i) & 0xffff)))		    \
    : make_float (i)))

Lisp_Object
intbig_to_lisp (intmax_t i)
{
  return INTBIG_TO_LISP (i, INTMAX_MIN);
}

Lisp_Object
uintbig_to_lisp (uintmax_t i)
{
  return INTBIG_TO_LISP (i, UINTMAX_MAX);
}

/* Convert the cons-of-integers, integer, or float value C to an
   unsigned value with maximum value MAX, where MAX is one less than a
   power of 2.  Signal an error if C does not have a valid format or
   is out of range.  */
uintmax_t
cons_to_unsigned (Lisp_Object c, uintmax_t max)
{
  bool valid = false;
  uintmax_t val UNINIT;
  if (INTEGERP (c))
    {
      valid = XINT (c) >= 0;
      val = XINT (c);
    }
  else if (FLOATP (c))
    {
      double d = XFLOAT_DATA (c);
      if (d >= 0 && d < 1.0 + max)
	{
	  val = d;
	  valid = val == d;
	}
    }
  else if (CONSP (c) && NATNUMP (XCAR (c)))
    {
      uintmax_t top = XFASTINT (XCAR (c));
      Lisp_Object rest = XCDR (c);
      if (top <= UINTMAX_MAX >> 24 >> 16
	  && CONSP (rest)
	  && NATNUMP (XCAR (rest)) && XFASTINT (XCAR (rest)) < 1 << 24
	  && NATNUMP (XCDR (rest)) && XFASTINT (XCDR (rest)) < 1 << 16)
	{
	  uintmax_t mid = XFASTINT (XCAR (rest));
	  val = top << 24 << 16 | mid << 16 | XFASTINT (XCDR (rest));
	  valid = true;
	}
      else if (top <= UINTMAX_MAX >> 16)
	{
	  if (CONSP (rest))
	    rest = XCAR (rest);
	  if (NATNUMP (rest) && XFASTINT (rest) < 1 << 16)
	    {
	      val = top << 16 | XFASTINT (rest);
	      valid = true;
	    }
	}
    }

  if (! (valid && val <= max))
    error ("Not an in-range integer, integral float, or cons of integers");
  return val;
}

/* Convert the cons-of-integers, integer, or float value C to a signed
   value with extrema MIN and MAX.  MAX should be one less than a
   power of 2, and MIN should be zero or the negative of a power of 2.
   Signal an error if C does not have a valid format or is out of
   range.  */
intmax_t
cons_to_signed (Lisp_Object c, intmax_t min, intmax_t max)
{
  bool valid = false;
  intmax_t val UNINIT;
  if (INTEGERP (c))
    {
      val = XINT (c);
      valid = true;
    }
  else if (FLOATP (c))
    {
      double d = XFLOAT_DATA (c);
      if (d >= min && d < 1.0 + max)
	{
	  val = d;
	  valid = val == d;
	}
    }
  else if (CONSP (c) && INTEGERP (XCAR (c)))
    {
      intmax_t top = XINT (XCAR (c));
      Lisp_Object rest = XCDR (c);
      if (top >= INTMAX_MIN >> 24 >> 16 && top <= INTMAX_MAX >> 24 >> 16
	  && CONSP (rest)
	  && NATNUMP (XCAR (rest)) && XFASTINT (XCAR (rest)) < 1 << 24
	  && NATNUMP (XCDR (rest)) && XFASTINT (XCDR (rest)) < 1 << 16)
	{
	  intmax_t mid = XFASTINT (XCAR (rest));
	  val = top << 24 << 16 | mid << 16 | XFASTINT (XCDR (rest));
	  valid = true;
	}
      else if (top >= INTMAX_MIN >> 16 && top <= INTMAX_MAX >> 16)
	{
	  if (CONSP (rest))
	    rest = XCAR (rest);
	  if (NATNUMP (rest) && XFASTINT (rest) < 1 << 16)
	    {
	      val = top << 16 | XFASTINT (rest);
	      valid = true;
	    }
	}
    }

  if (! (valid && min <= val && val <= max))
    error ("Not an in-range integer, integral float, or cons of integers");
  return val;
}

DEFUN ("number-to-string", Fnumber_to_string, Snumber_to_string, 1, 1, 0,
       doc: /* Return the decimal representation of NUMBER as a string.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.  */)
  (Lisp_Object number)
{
  char buffer[max (FLOAT_TO_STRING_BUFSIZE, INT_BUFSIZE_BOUND (EMACS_INT))];
  int len;

  CHECK_NUMBER_OR_FLOAT (number);

  if (FLOATP (number))
    len = float_to_string (buffer, XFLOAT_DATA (number));
  else
    len = sprintf (buffer, "%"pI"d", XINT (number));

  return make_unibyte_string (buffer, len);
}

DEFUN ("string-to-number", Fstring_to_number, Sstring_to_number, 1, 2, 0,
       doc: /* Parse STRING as a decimal number and return the number.
Ignore leading spaces and tabs, and all trailing chars.  Return 0 if
STRING cannot be parsed as an integer or floating point number.

If BASE, interpret STRING as a number in that base.  If BASE isn't
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, STRING is always parsed as an integer.  */)
  (register Lisp_Object string, Lisp_Object base)
{
  register char *p;
  register int b;
  Lisp_Object val;

  CHECK_STRING (string);

  if (NILP (base))
    b = 10;
  else
    {
      CHECK_NUMBER (base);
      if (! (XINT (base) >= 2 && XINT (base) <= 16))
	xsignal1 (Qargs_out_of_range, base);
      b = XINT (base);
    }

  p = SSDATA (string);
  while (*p == ' ' || *p == '\t')
    p++;

  val = string_to_number (p, b, 1);
  return NILP (val) ? make_number (0) : val;
}

enum arithop
  {
    Aadd,
    Asub,
    Amult,
    Adiv,
    Alogand,
    Alogior,
    Alogxor
  };

static Lisp_Object float_arith_driver (double, ptrdiff_t, enum arithop,
                                       ptrdiff_t, Lisp_Object *);
static Lisp_Object
arith_driver (enum arithop code, ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val;
  ptrdiff_t argnum, ok_args;
  EMACS_INT accum = 0;
  EMACS_INT next, ok_accum;
  bool overflow = 0;

  switch (code)
    {
    case Alogior:
    case Alogxor:
    case Aadd:
    case Asub:
      accum = 0;
      break;
    case Amult:
    case Adiv:
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
      if (! overflow)
	{
	  ok_args = argnum;
	  ok_accum = accum;
	}

      /* Using args[argnum] as argument to CHECK_NUMBER_... */
      val = args[argnum];
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val);

      if (FLOATP (val))
	return float_arith_driver (ok_accum, ok_args, code,
				   nargs, args);
      args[argnum] = val;
      next = XINT (args[argnum]);
      switch (code)
	{
	case Aadd:
	  overflow |= INT_ADD_WRAPV (accum, next, &accum);
	  break;
	case Asub:
	  if (! argnum)
	    accum = nargs == 1 ? - next : next;
	  else
	    overflow |= INT_SUBTRACT_WRAPV (accum, next, &accum);
	  break;
	case Amult:
	  overflow |= INT_MULTIPLY_WRAPV (accum, next, &accum);
	  break;
	case Adiv:
	  if (! (argnum || nargs == 1))
	    accum = next;
	  else
	    {
	      if (next == 0)
		xsignal0 (Qarith_error);
	      if (INT_DIVIDE_OVERFLOW (accum, next))
		overflow = true;
	      else
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
	}
    }

  XSETINT (val, accum);
  return val;
}

#ifndef isnan
# define isnan(x) ((x) != (x))
#endif

static Lisp_Object
float_arith_driver (double accum, ptrdiff_t argnum, enum arithop code,
		    ptrdiff_t nargs, Lisp_Object *args)
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
      switch (code)
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
	  if (! (argnum || nargs == 1))
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
	  wrong_type_argument (Qinteger_or_marker_p, val);
	}
    }

  return make_float (accum);
}


DEFUN ("+", Fplus, Splus, 0, MANY, 0,
       doc: /* Return sum of any number of arguments, which are numbers or markers.
usage: (+ &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Aadd, nargs, args);
}

DEFUN ("-", Fminus, Sminus, 0, MANY, 0,
       doc: /* Negate number or subtract numbers or markers and return the result.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.
usage: (- &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Asub, nargs, args);
}

DEFUN ("*", Ftimes, Stimes, 0, MANY, 0,
       doc: /* Return product of any number of arguments, which are numbers or markers.
usage: (* &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Amult, nargs, args);
}

DEFUN ("/", Fquo, Squo, 1, MANY, 0,
       doc: /* Divide number by divisors and return the result.
With two or more arguments, return first argument divided by the rest.
With one argument, return 1 divided by the argument.
The arguments must be numbers or markers.
usage: (/ NUMBER &rest DIVISORS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t argnum;
  for (argnum = 2; argnum < nargs; argnum++)
    if (FLOATP (args[argnum]))
      return float_arith_driver (0, 0, Adiv, nargs, args);
  return arith_driver (Adiv, nargs, args);
}

DEFUN ("%", Frem, Srem, 2, 2, 0,
       doc: /* Return remainder of X divided by Y.
Both must be integers or markers.  */)
  (register Lisp_Object x, Lisp_Object y)
{
  Lisp_Object val;

  CHECK_NUMBER_COERCE_MARKER (x);
  CHECK_NUMBER_COERCE_MARKER (y);

  if (XINT (y) == 0)
    xsignal0 (Qarith_error);

  XSETINT (val, XINT (x) % XINT (y));
  return val;
}

DEFUN ("mod", Fmod, Smod, 2, 2, 0,
       doc: /* Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers or markers.  */)
  (register Lisp_Object x, Lisp_Object y)
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

static Lisp_Object
minmax_driver (ptrdiff_t nargs, Lisp_Object *args,
	       enum Arith_Comparison comparison)
{
  Lisp_Object accum = args[0];
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (accum);
  for (ptrdiff_t argnum = 1; argnum < nargs; argnum++)
    {
      Lisp_Object val = args[argnum];
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val);
      if (!NILP (arithcompare (val, accum, comparison)))
	accum = val;
      else if (FLOATP (val) && isnan (XFLOAT_DATA (val)))
	return val;
    }
  return accum;
}

DEFUN ("max", Fmax, Smax, 1, MANY, 0,
       doc: /* Return largest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.
usage: (max NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return minmax_driver (nargs, args, ARITH_GRTR);
}

DEFUN ("min", Fmin, Smin, 1, MANY, 0,
       doc: /* Return smallest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.
usage: (min NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return minmax_driver (nargs, args, ARITH_LESS);
}

DEFUN ("logand", Flogand, Slogand, 0, MANY, 0,
       doc: /* Return bitwise-and of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logand &rest INTS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Alogand, nargs, args);
}

DEFUN ("logior", Flogior, Slogior, 0, MANY, 0,
       doc: /* Return bitwise-or of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logior &rest INTS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Alogior, nargs, args);
}

DEFUN ("logxor", Flogxor, Slogxor, 0, MANY, 0,
       doc: /* Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logxor &rest INTS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Alogxor, nargs, args);
}

DEFUN ("logcount", Flogcount, Slogcount, 1, 1, 0,
       doc: /* Return population count of VALUE.
This is the number of one bits in the two's complement representation
of VALUE.  If VALUE is negative, return the number of zero bits in the
representation.  */)
  (Lisp_Object value)
{
  CHECK_NUMBER (value);
  EMACS_INT v = XINT (value) < 0 ? -1 - XINT (value) : XINT (value);
  return make_number (EMACS_UINT_WIDTH <= UINT_WIDTH
		      ? count_one_bits (v)
		      : EMACS_UINT_WIDTH <= ULONG_WIDTH
		      ? count_one_bits_l (v)
		      : count_one_bits_ll (v));
}

static Lisp_Object
ash_lsh_impl (Lisp_Object value, Lisp_Object count, bool lsh)
{
  /* This code assumes that signed right shifts are arithmetic.  */
  verify ((EMACS_INT) -1 >> 1 == -1);

  Lisp_Object val;

  CHECK_NUMBER (value);
  CHECK_NUMBER (count);

  if (XINT (count) >= EMACS_INT_WIDTH)
    XSETINT (val, 0);
  else if (XINT (count) > 0)
    XSETINT (val, XUINT (value) << XINT (count));
  else if (XINT (count) <= -EMACS_INT_WIDTH)
    XSETINT (val, lsh ? 0 : XINT (value) < 0 ? -1 : 0);
  else
    XSETINT (val, (lsh ? XUINT (value) >> -XINT (count)
		   : XINT (value) >> -XINT (count)));
  return val;
}

DEFUN ("ash", Fash, Sash, 2, 2, 0,
       doc: /* Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, the sign bit is duplicated.  */)
  (register Lisp_Object value, Lisp_Object count)
{
  return ash_lsh_impl (value, count, false);
}

DEFUN ("lsh", Flsh, Slsh, 2, 2, 0,
       doc: /* Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, zeros are shifted in on the left.  */)
  (register Lisp_Object value, Lisp_Object count)
{
  return ash_lsh_impl (value, count, true);
}

DEFUN ("1+", Fadd1, Sadd1, 1, 1, 0,
       doc: /* Return NUMBER plus one.  NUMBER may be a number or a marker.
Markers are converted to integers.  */)
  (register Lisp_Object number)
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
  (register Lisp_Object number)
{
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (number);

  if (FLOATP (number))
    return (make_float (-1.0 + XFLOAT_DATA (number)));

  XSETINT (number, XINT (number) - 1);
  return number;
}

DEFUN ("lognot", Flognot, Slognot, 1, 1, 0,
       doc: /* Return the bitwise complement of NUMBER.  NUMBER must be an integer.  */)
  (register Lisp_Object number)
{
  CHECK_NUMBER (number);
  XSETINT (number, ~XINT (number));
  return number;
}

DEFUN ("byteorder", Fbyteorder, Sbyteorder, 0, 0, 0,
       doc: /* Return the byteorder for the machine.
Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII
lowercase l) for small endian machines.  */
       attributes: const)
  (void)
{
  unsigned i = 0x04030201;
  int order = *(char *)&i == 1 ? 108 : 66;

  return make_number (order);
}

/* Because we round up the bool vector allocate size to word_size
   units, we can safely read past the "end" of the vector in the
   operations below.  These extra bits are always zero.  */

static bits_word
bool_vector_spare_mask (EMACS_INT nr_bits)
{
  return (((bits_word) 1) << (nr_bits % BITS_PER_BITS_WORD)) - 1;
}

/* Info about unsigned long long, falling back on unsigned long
   if unsigned long long is not available.  */

#if HAVE_UNSIGNED_LONG_LONG_INT && defined ULLONG_WIDTH
enum { ULL_WIDTH = ULLONG_WIDTH };
# define ULL_MAX ULLONG_MAX
#else
enum { ULL_WIDTH = ULONG_WIDTH };
# define ULL_MAX ULONG_MAX
# define count_one_bits_ll count_one_bits_l
# define count_trailing_zeros_ll count_trailing_zeros_l
#endif

/* Shift VAL right by the width of an unsigned long long.
   ULL_WIDTH must be less than BITS_PER_BITS_WORD.  */

static bits_word
shift_right_ull (bits_word w)
{
  /* Pacify bogus GCC warning about shift count exceeding type width.  */
  int shift = ULL_WIDTH - BITS_PER_BITS_WORD < 0 ? ULL_WIDTH : 0;
  return w >> shift;
}

/* Return the number of 1 bits in W.  */

static int
count_one_bits_word (bits_word w)
{
  if (BITS_WORD_MAX <= UINT_MAX)
    return count_one_bits (w);
  else if (BITS_WORD_MAX <= ULONG_MAX)
    return count_one_bits_l (w);
  else
    {
      int i = 0, count = 0;
      while (count += count_one_bits_ll (w),
	     (i += ULL_WIDTH) < BITS_PER_BITS_WORD)
	w = shift_right_ull (w);
      return count;
    }
}

enum bool_vector_op { bool_vector_exclusive_or,
                      bool_vector_union,
                      bool_vector_intersection,
                      bool_vector_set_difference,
                      bool_vector_subsetp };

static Lisp_Object
bool_vector_binop_driver (Lisp_Object a,
                          Lisp_Object b,
                          Lisp_Object dest,
                          enum bool_vector_op op)
{
  EMACS_INT nr_bits;
  bits_word *adata, *bdata, *destdata;
  ptrdiff_t i = 0;
  ptrdiff_t nr_words;

  CHECK_BOOL_VECTOR (a);
  CHECK_BOOL_VECTOR (b);

  nr_bits = bool_vector_size (a);
  if (bool_vector_size (b) != nr_bits)
    wrong_length_argument (a, b, dest);

  nr_words = bool_vector_words (nr_bits);
  adata = bool_vector_data (a);
  bdata = bool_vector_data (b);

  if (NILP (dest))
    {
      dest = make_uninit_bool_vector (nr_bits);
      destdata = bool_vector_data (dest);
    }
  else
    {
      CHECK_BOOL_VECTOR (dest);
      destdata = bool_vector_data (dest);
      if (bool_vector_size (dest) != nr_bits)
	wrong_length_argument (a, b, dest);

      switch (op)
	{
	case bool_vector_exclusive_or:
	  for (; i < nr_words; i++)
	    if (destdata[i] != (adata[i] ^ bdata[i]))
	      goto set_dest;
	  break;

	case bool_vector_subsetp:
	  for (; i < nr_words; i++)
	    if (adata[i] &~ bdata[i])
	      return Qnil;
	  return Qt;

	case bool_vector_union:
	  for (; i < nr_words; i++)
	    if (destdata[i] != (adata[i] | bdata[i]))
	      goto set_dest;
	  break;

	case bool_vector_intersection:
	  for (; i < nr_words; i++)
	    if (destdata[i] != (adata[i] & bdata[i]))
	      goto set_dest;
	  break;

	case bool_vector_set_difference:
	  for (; i < nr_words; i++)
	    if (destdata[i] != (adata[i] &~ bdata[i]))
	      goto set_dest;
	  break;
	}

      return Qnil;
    }

 set_dest:
  switch (op)
    {
    case bool_vector_exclusive_or:
      for (; i < nr_words; i++)
	destdata[i] = adata[i] ^ bdata[i];
      break;

    case bool_vector_union:
      for (; i < nr_words; i++)
	destdata[i] = adata[i] | bdata[i];
      break;

    case bool_vector_intersection:
      for (; i < nr_words; i++)
	destdata[i] = adata[i] & bdata[i];
      break;

    case bool_vector_set_difference:
      for (; i < nr_words; i++)
	destdata[i] = adata[i] &~ bdata[i];
      break;

    default:
      eassume (0);
    }

  return dest;
}

/* PRECONDITION must be true.  Return VALUE.  This odd construction
   works around a bogus GCC diagnostic "shift count >= width of type".  */

static int
pre_value (bool precondition, int value)
{
  eassume (precondition);
  return precondition ? value : 0;
}

/* Compute the number of trailing zero bits in val.  If val is zero,
   return the number of bits in val.  */
static int
count_trailing_zero_bits (bits_word val)
{
  if (BITS_WORD_MAX == UINT_MAX)
    return count_trailing_zeros (val);
  if (BITS_WORD_MAX == ULONG_MAX)
    return count_trailing_zeros_l (val);
  if (BITS_WORD_MAX == ULL_MAX)
    return count_trailing_zeros_ll (val);

  /* The rest of this code is for the unlikely platform where bits_word differs
     in width from unsigned int, unsigned long, and unsigned long long.  */
  val |= ~ BITS_WORD_MAX;
  if (BITS_WORD_MAX <= UINT_MAX)
    return count_trailing_zeros (val);
  if (BITS_WORD_MAX <= ULONG_MAX)
    return count_trailing_zeros_l (val);
  else
    {
      int count;
      for (count = 0;
	   count < BITS_PER_BITS_WORD - ULL_WIDTH;
	   count += ULL_WIDTH)
	{
	  if (val & ULL_MAX)
	    return count + count_trailing_zeros_ll (val);
	  val = shift_right_ull (val);
	}

      if (BITS_PER_BITS_WORD % ULL_WIDTH != 0
	  && BITS_WORD_MAX == (bits_word) -1)
	val |= (bits_word) 1 << pre_value (ULONG_MAX < BITS_WORD_MAX,
					   BITS_PER_BITS_WORD % ULL_WIDTH);
      return count + count_trailing_zeros_ll (val);
    }
}

static bits_word
bits_word_to_host_endian (bits_word val)
{
#ifndef WORDS_BIGENDIAN
  return val;
#else
  if (BITS_WORD_MAX >> 31 == 1)
    return bswap_32 (val);
# if HAVE_UNSIGNED_LONG_LONG
  if (BITS_WORD_MAX >> 31 >> 31 >> 1 == 1)
    return bswap_64 (val);
# endif
  {
    int i;
    bits_word r = 0;
    for (i = 0; i < sizeof val; i++)
      {
	r = ((r << 1 << (CHAR_BIT - 1))
	     | (val & ((1u << 1 << (CHAR_BIT - 1)) - 1)));
	val = val >> 1 >> (CHAR_BIT - 1);
      }
    return r;
  }
#endif
}

DEFUN ("bool-vector-exclusive-or", Fbool_vector_exclusive_or,
       Sbool_vector_exclusive_or, 2, 3, 0,
       doc: /* Return A ^ B, bitwise exclusive or.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.  */)
  (Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
  return bool_vector_binop_driver (a, b, c, bool_vector_exclusive_or);
}

DEFUN ("bool-vector-union", Fbool_vector_union,
       Sbool_vector_union, 2, 3, 0,
       doc: /* Return A | B, bitwise or.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.  */)
  (Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
  return bool_vector_binop_driver (a, b, c, bool_vector_union);
}

DEFUN ("bool-vector-intersection", Fbool_vector_intersection,
       Sbool_vector_intersection, 2, 3, 0,
       doc: /* Return A & B, bitwise and.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.  */)
  (Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
  return bool_vector_binop_driver (a, b, c, bool_vector_intersection);
}

DEFUN ("bool-vector-set-difference", Fbool_vector_set_difference,
       Sbool_vector_set_difference, 2, 3, 0,
       doc: /* Return A &~ B, set difference.
If optional third argument C is given, store result into C.
A, B, and C must be bool vectors of the same length.
Return the destination vector if it changed or nil otherwise.  */)
  (Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
  return bool_vector_binop_driver (a, b, c, bool_vector_set_difference);
}

DEFUN ("bool-vector-subsetp", Fbool_vector_subsetp,
       Sbool_vector_subsetp, 2, 2, 0,
       doc: /* Return t if every t value in A is also t in B, nil otherwise.
A and B must be bool vectors of the same length.  */)
  (Lisp_Object a, Lisp_Object b)
{
  return bool_vector_binop_driver (a, b, b, bool_vector_subsetp);
}

DEFUN ("bool-vector-not", Fbool_vector_not,
       Sbool_vector_not, 1, 2, 0,
       doc: /* Compute ~A, set complement.
If optional second argument B is given, store result into B.
A and B must be bool vectors of the same length.
Return the destination vector.  */)
  (Lisp_Object a, Lisp_Object b)
{
  EMACS_INT nr_bits;
  bits_word *bdata, *adata;
  ptrdiff_t i;

  CHECK_BOOL_VECTOR (a);
  nr_bits = bool_vector_size (a);

  if (NILP (b))
    b = make_uninit_bool_vector (nr_bits);
  else
    {
      CHECK_BOOL_VECTOR (b);
      if (bool_vector_size (b) != nr_bits)
	wrong_length_argument (a, b, Qnil);
    }

  bdata = bool_vector_data (b);
  adata = bool_vector_data (a);

  for (i = 0; i < nr_bits / BITS_PER_BITS_WORD; i++)
    bdata[i] = BITS_WORD_MAX & ~adata[i];

  if (nr_bits % BITS_PER_BITS_WORD)
    {
      bits_word mword = bits_word_to_host_endian (adata[i]);
      mword = ~mword;
      mword &= bool_vector_spare_mask (nr_bits);
      bdata[i] = bits_word_to_host_endian (mword);
    }

  return b;
}

DEFUN ("bool-vector-count-population", Fbool_vector_count_population,
       Sbool_vector_count_population, 1, 1, 0,
       doc: /* Count how many elements in A are t.
A is a bool vector.  To count A's nil elements, subtract the return
value from A's length.  */)
  (Lisp_Object a)
{
  EMACS_INT count;
  EMACS_INT nr_bits;
  bits_word *adata;
  ptrdiff_t i, nwords;

  CHECK_BOOL_VECTOR (a);

  nr_bits = bool_vector_size (a);
  nwords = bool_vector_words (nr_bits);
  count = 0;
  adata = bool_vector_data (a);

  for (i = 0; i < nwords; i++)
    count += count_one_bits_word (adata[i]);

  return make_number (count);
}

DEFUN ("bool-vector-count-consecutive", Fbool_vector_count_consecutive,
       Sbool_vector_count_consecutive, 3, 3, 0,
       doc: /* Count how many consecutive elements in A equal B starting at I.
A is a bool vector, B is t or nil, and I is an index into A.  */)
  (Lisp_Object a, Lisp_Object b, Lisp_Object i)
{
  EMACS_INT count;
  EMACS_INT nr_bits;
  int offset;
  bits_word *adata;
  bits_word twiddle;
  bits_word mword; /* Machine word.  */
  ptrdiff_t pos, pos0;
  ptrdiff_t nr_words;

  CHECK_BOOL_VECTOR (a);
  CHECK_NATNUM (i);

  nr_bits = bool_vector_size (a);
  if (XFASTINT (i) > nr_bits) /* Allow one past the end for convenience */
    args_out_of_range (a, i);

  adata = bool_vector_data (a);
  nr_words = bool_vector_words (nr_bits);
  pos = XFASTINT (i) / BITS_PER_BITS_WORD;
  offset = XFASTINT (i) % BITS_PER_BITS_WORD;
  count = 0;

  /* By XORing with twiddle, we transform the problem of "count
     consecutive equal values" into "count the zero bits".  The latter
     operation usually has hardware support.  */
  twiddle = NILP (b) ? 0 : BITS_WORD_MAX;

  /* Scan the remainder of the mword at the current offset.  */
  if (pos < nr_words && offset != 0)
    {
      mword = bits_word_to_host_endian (adata[pos]);
      mword ^= twiddle;
      mword >>= offset;

      /* Do not count the pad bits.  */
      mword |= (bits_word) 1 << (BITS_PER_BITS_WORD - offset);

      count = count_trailing_zero_bits (mword);
      pos++;
      if (count + offset < BITS_PER_BITS_WORD)
        return make_number (count);
    }

  /* Scan whole words until we either reach the end of the vector or
     find an mword that doesn't completely match.  twiddle is
     endian-independent.  */
  pos0 = pos;
  while (pos < nr_words && adata[pos] == twiddle)
    pos++;
  count += (pos - pos0) * BITS_PER_BITS_WORD;

  if (pos < nr_words)
    {
      /* If we stopped because of a mismatch, see how many bits match
         in the current mword.  */
      mword = bits_word_to_host_endian (adata[pos]);
      mword ^= twiddle;
      count += count_trailing_zero_bits (mword);
    }
  else if (nr_bits % BITS_PER_BITS_WORD != 0)
    {
      /* If we hit the end, we might have overshot our count.  Reduce
         the total by the number of spare bits at the end of the
         vector.  */
      count -= BITS_PER_BITS_WORD - nr_bits % BITS_PER_BITS_WORD;
    }

  return make_number (count);
}


void
syms_of_data (void)
{
  Lisp_Object error_tail, arith_tail;

  DEFSYM (Qquote, "quote");
  DEFSYM (Qlambda, "lambda");
  DEFSYM (Qerror_conditions, "error-conditions");
  DEFSYM (Qerror_message, "error-message");
  DEFSYM (Qtop_level, "top-level");

  DEFSYM (Qerror, "error");
  DEFSYM (Quser_error, "user-error");
  DEFSYM (Qquit, "quit");
  DEFSYM (Qwrong_length_argument, "wrong-length-argument");
  DEFSYM (Qwrong_type_argument, "wrong-type-argument");
  DEFSYM (Qargs_out_of_range, "args-out-of-range");
  DEFSYM (Qvoid_function, "void-function");
  DEFSYM (Qcyclic_function_indirection, "cyclic-function-indirection");
  DEFSYM (Qcyclic_variable_indirection, "cyclic-variable-indirection");
  DEFSYM (Qvoid_variable, "void-variable");
  DEFSYM (Qsetting_constant, "setting-constant");
  DEFSYM (Qtrapping_constant, "trapping-constant");
  DEFSYM (Qinvalid_read_syntax, "invalid-read-syntax");

  DEFSYM (Qinvalid_function, "invalid-function");
  DEFSYM (Qwrong_number_of_arguments, "wrong-number-of-arguments");
  DEFSYM (Qno_catch, "no-catch");
  DEFSYM (Qend_of_file, "end-of-file");
  DEFSYM (Qarith_error, "arith-error");
  DEFSYM (Qbeginning_of_buffer, "beginning-of-buffer");
  DEFSYM (Qend_of_buffer, "end-of-buffer");
  DEFSYM (Qbuffer_read_only, "buffer-read-only");
  DEFSYM (Qtext_read_only, "text-read-only");
  DEFSYM (Qmark_inactive, "mark-inactive");

  DEFSYM (Qlistp, "listp");
  DEFSYM (Qconsp, "consp");
  DEFSYM (Qsymbolp, "symbolp");
  DEFSYM (Qintegerp, "integerp");
  DEFSYM (Qnatnump, "natnump");
  DEFSYM (Qwholenump, "wholenump");
  DEFSYM (Qstringp, "stringp");
  DEFSYM (Qarrayp, "arrayp");
  DEFSYM (Qsequencep, "sequencep");
  DEFSYM (Qbufferp, "bufferp");
  DEFSYM (Qvectorp, "vectorp");
  DEFSYM (Qrecordp, "recordp");
  DEFSYM (Qbool_vector_p, "bool-vector-p");
  DEFSYM (Qchar_or_string_p, "char-or-string-p");
  DEFSYM (Qmarkerp, "markerp");
#ifdef HAVE_MODULES
  DEFSYM (Quser_ptrp, "user-ptrp");
#endif
  DEFSYM (Qbuffer_or_string_p, "buffer-or-string-p");
  DEFSYM (Qinteger_or_marker_p, "integer-or-marker-p");
  DEFSYM (Qfboundp, "fboundp");

  DEFSYM (Qfloatp, "floatp");
  DEFSYM (Qnumberp, "numberp");
  DEFSYM (Qnumber_or_marker_p, "number-or-marker-p");

  DEFSYM (Qchar_table_p, "char-table-p");
  DEFSYM (Qvector_or_char_table_p, "vector-or-char-table-p");

  DEFSYM (Qsubrp, "subrp");
  DEFSYM (Qunevalled, "unevalled");
  DEFSYM (Qmany, "many");

  DEFSYM (Qcdr, "cdr");

  error_tail = pure_cons (Qerror, Qnil);

  /* ERROR is used as a signaler for random errors for which nothing else is
     right.  */

  Fput (Qerror, Qerror_conditions,
	error_tail);
  Fput (Qerror, Qerror_message,
	build_pure_c_string ("error"));

#define PUT_ERROR(sym, tail, msg)			\
  Fput (sym, Qerror_conditions, pure_cons (sym, tail)); \
  Fput (sym, Qerror_message, build_pure_c_string (msg))

  PUT_ERROR (Qquit, Qnil, "Quit");

  PUT_ERROR (Quser_error, error_tail, "");
  PUT_ERROR (Qwrong_length_argument, error_tail, "Wrong length argument");
  PUT_ERROR (Qwrong_type_argument, error_tail, "Wrong type argument");
  PUT_ERROR (Qargs_out_of_range, error_tail, "Args out of range");
  PUT_ERROR (Qvoid_function, error_tail,
	     "Symbol's function definition is void");
  PUT_ERROR (Qcyclic_function_indirection, error_tail,
	     "Symbol's chain of function indirections contains a loop");
  PUT_ERROR (Qcyclic_variable_indirection, error_tail,
	     "Symbol's chain of variable indirections contains a loop");
  DEFSYM (Qcircular_list, "circular-list");
  PUT_ERROR (Qcircular_list, error_tail, "List contains a loop");
  PUT_ERROR (Qvoid_variable, error_tail, "Symbol's value as variable is void");
  PUT_ERROR (Qsetting_constant, error_tail,
	     "Attempt to set a constant symbol");
  PUT_ERROR (Qtrapping_constant, error_tail,
             "Attempt to trap writes to a constant symbol");
  PUT_ERROR (Qinvalid_read_syntax, error_tail, "Invalid read syntax");
  PUT_ERROR (Qinvalid_function, error_tail, "Invalid function");
  PUT_ERROR (Qwrong_number_of_arguments, error_tail,
	     "Wrong number of arguments");
  PUT_ERROR (Qno_catch, error_tail, "No catch for tag");
  PUT_ERROR (Qend_of_file, error_tail, "End of file during parsing");

  arith_tail = pure_cons (Qarith_error, error_tail);
  Fput (Qarith_error, Qerror_conditions, arith_tail);
  Fput (Qarith_error, Qerror_message, build_pure_c_string ("Arithmetic error"));

  PUT_ERROR (Qbeginning_of_buffer, error_tail, "Beginning of buffer");
  PUT_ERROR (Qend_of_buffer, error_tail, "End of buffer");
  PUT_ERROR (Qbuffer_read_only, error_tail, "Buffer is read-only");
  PUT_ERROR (Qtext_read_only, pure_cons (Qbuffer_read_only, error_tail),
	     "Text is read-only");

  DEFSYM (Qrange_error, "range-error");
  DEFSYM (Qdomain_error, "domain-error");
  DEFSYM (Qsingularity_error, "singularity-error");
  DEFSYM (Qoverflow_error, "overflow-error");
  DEFSYM (Qunderflow_error, "underflow-error");

  PUT_ERROR (Qdomain_error, arith_tail, "Arithmetic domain error");

  PUT_ERROR (Qrange_error, arith_tail, "Arithmetic range error");

  PUT_ERROR (Qsingularity_error, Fcons (Qdomain_error, arith_tail),
	     "Arithmetic singularity error");

  PUT_ERROR (Qoverflow_error, Fcons (Qdomain_error, arith_tail),
	     "Arithmetic overflow error");
  PUT_ERROR (Qunderflow_error, Fcons (Qdomain_error, arith_tail),
	     "Arithmetic underflow error");

  /* Types that type-of returns.  */
  DEFSYM (Qinteger, "integer");
  DEFSYM (Qsymbol, "symbol");
  DEFSYM (Qstring, "string");
  DEFSYM (Qcons, "cons");
  DEFSYM (Qmarker, "marker");
  DEFSYM (Qoverlay, "overlay");
  DEFSYM (Qfinalizer, "finalizer");
#ifdef HAVE_MODULES
  DEFSYM (Qmodule_function, "module-function");
  DEFSYM (Quser_ptr, "user-ptr");
#endif
  DEFSYM (Qfloat, "float");
  DEFSYM (Qwindow_configuration, "window-configuration");
  DEFSYM (Qprocess, "process");
  DEFSYM (Qwindow, "window");
  DEFSYM (Qsubr, "subr");
  DEFSYM (Qcompiled_function, "compiled-function");
  DEFSYM (Qbuffer, "buffer");
  DEFSYM (Qframe, "frame");
  DEFSYM (Qvector, "vector");
  DEFSYM (Qrecord, "record");
  DEFSYM (Qchar_table, "char-table");
  DEFSYM (Qbool_vector, "bool-vector");
  DEFSYM (Qhash_table, "hash-table");
  DEFSYM (Qthread, "thread");
  DEFSYM (Qmutex, "mutex");
  DEFSYM (Qcondition_variable, "condition-variable");
  DEFSYM (Qfont_spec, "font-spec");
  DEFSYM (Qfont_entity, "font-entity");
  DEFSYM (Qfont_object, "font-object");
  DEFSYM (Qterminal, "terminal");

  DEFSYM (Qdefun, "defun");

  DEFSYM (Qinteractive_form, "interactive-form");
  DEFSYM (Qdefalias_fset_function, "defalias-fset-function");

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
  defsubr (&Srecordp);
  defsubr (&Schar_table_p);
  defsubr (&Svector_or_char_table_p);
  defsubr (&Sbool_vector_p);
  defsubr (&Sarrayp);
  defsubr (&Ssequencep);
  defsubr (&Sbufferp);
  defsubr (&Smarkerp);
  defsubr (&Ssubrp);
  defsubr (&Sbyte_code_function_p);
  defsubr (&Smodule_function_p);
  defsubr (&Schar_or_string_p);
  defsubr (&Sthreadp);
  defsubr (&Smutexp);
  defsubr (&Scondition_variable_p);
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
  defsubr (&Slogcount);
  defsubr (&Slsh);
  defsubr (&Sash);
  defsubr (&Sadd1);
  defsubr (&Ssub1);
  defsubr (&Slognot);
  defsubr (&Sbyteorder);
  defsubr (&Ssubr_arity);
  defsubr (&Ssubr_name);
#ifdef HAVE_MODULES
  defsubr (&Suser_ptrp);
#endif

  defsubr (&Sbool_vector_exclusive_or);
  defsubr (&Sbool_vector_union);
  defsubr (&Sbool_vector_intersection);
  defsubr (&Sbool_vector_set_difference);
  defsubr (&Sbool_vector_not);
  defsubr (&Sbool_vector_subsetp);
  defsubr (&Sbool_vector_count_consecutive);
  defsubr (&Sbool_vector_count_population);

  set_symbol_function (Qwholenump, XSYMBOL (Qnatnump)->u.s.function);

  DEFVAR_LISP ("most-positive-fixnum", Vmost_positive_fixnum,
	       doc: /* The largest value that is representable in a Lisp integer.
This variable cannot be set; trying to do so will signal an error.  */);
  Vmost_positive_fixnum = make_number (MOST_POSITIVE_FIXNUM);
  make_symbol_constant (intern_c_string ("most-positive-fixnum"));

  DEFVAR_LISP ("most-negative-fixnum", Vmost_negative_fixnum,
	       doc: /* The smallest value that is representable in a Lisp integer.
This variable cannot be set; trying to do so will signal an error.  */);
  Vmost_negative_fixnum = make_number (MOST_NEGATIVE_FIXNUM);
  make_symbol_constant (intern_c_string ("most-negative-fixnum"));

  DEFSYM (Qwatchers, "watchers");
  DEFSYM (Qmakunbound, "makunbound");
  DEFSYM (Qunlet, "unlet");
  DEFSYM (Qset, "set");
  DEFSYM (Qset_default, "set-default");
  defsubr (&Sadd_variable_watcher);
  defsubr (&Sremove_variable_watcher);
  defsubr (&Sget_variable_watchers);
}
