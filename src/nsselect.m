/* NeXT/Open/GNUstep / MacOSX Cocoa selection processing for emacs.
   Copyright (C) 1993, 1994, 2005, 2006, 2008, 2009, 2010
     Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
MacOSX/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>
#include <setjmp.h>

#include "lisp.h"
#include "nsterm.h"
#include "termhooks.h"
#include "keyboard.h"

#define CUT_BUFFER_SUPPORT

Lisp_Object QCLIPBOARD, QSECONDARY, QTEXT, QFILE_NAME;

static Lisp_Object Vns_sent_selection_hooks;
static Lisp_Object Vns_lost_selection_hooks;
static Lisp_Object Vselection_alist;
static Lisp_Object Vselection_converter_alist;

static Lisp_Object Qforeign_selection;

/* NSGeneralPboard is pretty much analogous to X11 CLIPBOARD */
NSString *NXPrimaryPboard;
NSString *NXSecondaryPboard;



/* ==========================================================================

    Internal utility functions

   ========================================================================== */


static NSString *
symbol_to_nsstring (Lisp_Object sym)
{
  CHECK_SYMBOL (sym);
  if (EQ (sym, QCLIPBOARD))     return NSGeneralPboard;
  if (EQ (sym, QPRIMARY))     return NXPrimaryPboard;
  if (EQ (sym, QSECONDARY))   return NXSecondaryPboard;
  if (EQ (sym, QTEXT))        return NSStringPboardType;
  return [NSString stringWithUTF8String: SDATA (XSYMBOL (sym)->xname)];
}


static Lisp_Object
ns_string_to_symbol (NSString *t)
{
  if ([t isEqualToString: NSGeneralPboard])
    return QCLIPBOARD;
  if ([t isEqualToString: NXPrimaryPboard])
    return QPRIMARY;
  if ([t isEqualToString: NXSecondaryPboard])
    return QSECONDARY;
  if ([t isEqualToString: NSStringPboardType])
    return QTEXT;
  if ([t isEqualToString: NSFilenamesPboardType])
    return QFILE_NAME;
  if ([t isEqualToString: NSTabularTextPboardType])
    return QTEXT;
  return intern ([t UTF8String]);
}


static Lisp_Object
clean_local_selection_data (Lisp_Object obj)
{
  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && CONSP (XCDR (obj))
      && INTEGERP (XCAR (XCDR (obj)))
      && NILP (XCDR (XCDR (obj))))
    obj = Fcons (XCAR (obj), XCDR (obj));

  if (CONSP (obj)
      && INTEGERP (XCAR (obj))
      && INTEGERP (XCDR (obj)))
    {
      if (XINT (XCAR (obj)) == 0)
        return XCDR (obj);
      if (XINT (XCAR (obj)) == -1)
        return make_number (- XINT (XCDR (obj)));
    }

  if (VECTORP (obj))
    {
      int i;
      int size = ASIZE (obj);
      Lisp_Object copy;

      if (size == 1)
        return clean_local_selection_data (AREF (obj, 0));
      copy = Fmake_vector (make_number (size), Qnil);
      for (i = 0; i < size; i++)
        ASET (copy, i, clean_local_selection_data (AREF (obj, i)));
      return copy;
    }

  return obj;
}


static void
ns_declare_pasteboard (id pb)
{
  [pb declareTypes: ns_send_types owner: NSApp];
}


static void
ns_undeclare_pasteboard (id pb)
{
  [pb declareTypes: [NSArray array] owner: nil];
}


static void
ns_string_to_pasteboard_internal (id pb, Lisp_Object str, NSString *gtype)
{
  if (EQ (str, Qnil))
    {
      [pb declareTypes: [NSArray array] owner: nil];
    }
  else
    {
      char *utfStr;
      NSString *type, *nsStr;
      NSEnumerator *tenum;

      CHECK_STRING (str);

      utfStr = SDATA (str);
      nsStr = [NSString stringWithUTF8String: utfStr];

      if (gtype == nil)
        {
          [pb declareTypes: ns_send_types owner: nil];
          tenum = [ns_send_types objectEnumerator];
          while ( (type = [tenum nextObject]) )
            [pb setString: nsStr forType: type];
        }
      else
        {
          [pb setString: nsStr forType: gtype];
        }
    }
}


static Lisp_Object
ns_get_local_selection (Lisp_Object selection_name,
                       Lisp_Object target_type)
{
  Lisp_Object local_value;
  Lisp_Object handler_fn, value, type, check;
  int count;

  local_value = assq_no_quit (selection_name, Vselection_alist);

  if (NILP (local_value)) return Qnil;

  count = specpdl_ptr - specpdl;
  specbind (Qinhibit_quit, Qt);
  CHECK_SYMBOL (target_type);
  handler_fn = Fcdr (Fassq (target_type, Vselection_converter_alist));
  if (!NILP (handler_fn))
    value = call3 (handler_fn, selection_name, target_type,
                XCAR (XCDR (local_value)));
  else
    value = Qnil;
  unbind_to (count, Qnil);

  check = value;
  if (CONSP (value) && SYMBOLP (XCAR (value)))
    {
      type = XCAR (value);
      check = XCDR (value);
    }

  if (STRINGP (check) || VECTORP (check) || SYMBOLP (check)
      || INTEGERP (check) || NILP (value))
    return value;

  if (CONSP (check)
      && INTEGERP (XCAR (check))
      && (INTEGERP (XCDR (check))||
          (CONSP (XCDR (check))
           && INTEGERP (XCAR (XCDR (check)))
           && NILP (XCDR (XCDR (check))))))
    return value;

  // FIXME: Why `quit' rather than `error'?
  Fsignal (Qquit, Fcons (build_string (
      "invalid data returned by selection-conversion function"),
                        Fcons (handler_fn, Fcons (value, Qnil))));
  // FIXME: Beware, `quit' can return!!
  return Qnil;
}


static Lisp_Object
ns_get_foreign_selection (Lisp_Object symbol, Lisp_Object target)
{
  id pb;
  pb =[NSPasteboard pasteboardWithName: symbol_to_nsstring (symbol)];
  return ns_string_from_pasteboard (pb);
}


static void
ns_handle_selection_request (struct input_event *event)
{
  // FIXME: BIG UGLY HACK!!!
  id pb = (id)*(EMACS_INT*)&(event->x);
  NSString *type = (NSString *)*(EMACS_INT*)&(event->y);
  Lisp_Object selection_name, selection_data, target_symbol, data;
  Lisp_Object successful_p, rest;

  selection_name = ns_string_to_symbol ([(NSPasteboard *)pb name]);
  target_symbol = ns_string_to_symbol (type);
  selection_data = assq_no_quit (selection_name, Vselection_alist);
  successful_p = Qnil;

  if (!NILP (selection_data))
    {
      data = ns_get_local_selection (selection_name, target_symbol);
      if (!NILP (data))
        {
          if (STRINGP (data))
            ns_string_to_pasteboard_internal (pb, data, type);
          successful_p = Qt;
        }
    }

  if (!EQ (Vns_sent_selection_hooks, Qunbound))
    {
      for (rest = Vns_sent_selection_hooks; CONSP (rest); rest = Fcdr (rest))
        call3 (Fcar (rest), selection_name, target_symbol, successful_p);
    }
}


static void
ns_handle_selection_clear (struct input_event *event)
{
  id pb = (id)*(EMACS_INT*)&(event->x);
  Lisp_Object selection_name, selection_data, rest;

  selection_name = ns_string_to_symbol ([(NSPasteboard *)pb name]);
  selection_data = assq_no_quit (selection_name, Vselection_alist);
  if (NILP (selection_data)) return;

  if (EQ (selection_data, Fcar (Vselection_alist)))
    Vselection_alist = Fcdr (Vselection_alist);
  else
    {
      for (rest = Vselection_alist; !NILP (rest); rest = Fcdr (rest))
        if (EQ (selection_data, Fcar (Fcdr (rest))))
          Fsetcdr (rest, Fcdr (Fcdr (rest)));
    }

  if (!EQ (Vns_lost_selection_hooks, Qunbound))
    {
      for (rest = Vns_lost_selection_hooks;CONSP (rest); rest = Fcdr (rest))
        call1 (Fcar (rest), selection_name);
    }
}



/* ==========================================================================

    Functions used externally

   ========================================================================== */


Lisp_Object
ns_string_from_pasteboard (id pb)
{
  NSString *type, *str;
  const char *utfStr;

  type = [pb availableTypeFromArray: ns_return_types];
  if (type == nil)
    {
      Fsignal (Qquit,
              Fcons (build_string ("empty or unsupported pasteboard type"),
                    Qnil));
    return Qnil;
    }

  /* get the string */
  if (! (str = [pb stringForType: type]))
    {
      NSData *data = [pb dataForType: type];
      if (data != nil)
        str = [[NSString alloc] initWithData: data
                                    encoding: NSUTF8StringEncoding];
      if (str != nil)
        {
          [str autorelease];
        }
      else
        {
          Fsignal (Qquit,
                  Fcons (build_string ("pasteboard doesn't contain valid data"),
                        Qnil));
          return Qnil;
        }
    }

  /* assume UTF8 */
  NS_DURING
    {
      /* EOL conversion: PENDING- is this too simple? */
      NSMutableString *mstr = [[str mutableCopy] autorelease];
      [mstr replaceOccurrencesOfString: @"\r\n" withString: @"\n"
            options: NSLiteralSearch range: NSMakeRange (0, [mstr length])];
      [mstr replaceOccurrencesOfString: @"\r" withString: @"\n"
            options: NSLiteralSearch range: NSMakeRange (0, [mstr length])];

      utfStr = [mstr UTF8String];
      if (!utfStr)
        utfStr = [mstr cString];
    }
  NS_HANDLER
    {
      message1 ("ns_string_from_pasteboard: UTF8String failed\n");
      utfStr = [str lossyCString];
    }
  NS_ENDHANDLER

  return build_string (utfStr);
}


void
ns_string_to_pasteboard (id pb, Lisp_Object str)
{
  ns_string_to_pasteboard_internal (pb, str, nil);
}



/* ==========================================================================

    Lisp Defuns

   ========================================================================== */


DEFUN ("x-own-selection-internal", Fx_own_selection_internal,
       Sx_own_selection_internal, 2, 2, 0,
       doc: /* Assert a selection.
SELECTION-NAME is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.  */)
     (Lisp_Object selection_name, Lisp_Object selection_value)
{
  id pb;
  Lisp_Object old_value, new_value;

  check_ns ();
  CHECK_SYMBOL (selection_name);
  if (NILP (selection_value))
      error ("selection-value may not be nil.");
  pb =[NSPasteboard pasteboardWithName: symbol_to_nsstring (selection_name)];
  ns_declare_pasteboard (pb);
  old_value = assq_no_quit (selection_name, Vselection_alist);
  new_value = Fcons (selection_name, Fcons (selection_value, Qnil));
  if (NILP (old_value))
    Vselection_alist = Fcons (new_value, Vselection_alist);
  else
    Fsetcdr (old_value, Fcdr (new_value));
  /* XXX An evil hack, but a necessary one I fear XXX */
  {
    struct input_event ev;
    ev.kind = SELECTION_REQUEST_EVENT;
    ev.modifiers = 0;
    ev.code = 0;
    *(EMACS_INT*)(&(ev.x)) = (EMACS_INT)pb; // FIXME: BIG UGLY HACK!!
    *(EMACS_INT*)(&(ev.y)) = (EMACS_INT)NSStringPboardType;
    ns_handle_selection_request (&ev);
  }
  return selection_value;
}


DEFUN ("x-disown-selection-internal", Fx_disown_selection_internal,
       Sx_disown_selection_internal, 1, 2, 0,
       doc: /* If we own the selection SELECTION, disown it.  */)
     (Lisp_Object selection_name, Lisp_Object time)
{
  id pb;
  check_ns ();
  CHECK_SYMBOL (selection_name);
  if (NILP (assq_no_quit (selection_name, Vselection_alist))) return Qnil;

  pb =[NSPasteboard pasteboardWithName: symbol_to_nsstring (selection_name)];
  ns_undeclare_pasteboard (pb);
  return Qt;
}


DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
       0, 1, 0, doc: /* Whether there is an owner for the given selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.)  */)
     (Lisp_Object selection)
{
  id pb;
  NSArray *types;

  check_ns ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  pb =[NSPasteboard pasteboardWithName: symbol_to_nsstring (selection)];
  types =[pb types];
  return ([types count] == 0) ? Qnil : Qt;
}


DEFUN ("x-selection-owner-p", Fx_selection_owner_p, Sx_selection_owner_p,
       0, 1, 0,
       doc: /* Whether the current Emacs process owns the given selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.)  */)
     (Lisp_Object selection)
{
  check_ns ();
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  return (NILP (Fassq (selection, Vselection_alist))) ? Qnil : Qt;
}


DEFUN ("x-get-selection-internal", Fx_get_selection_internal,
       Sx_get_selection_internal, 2, 2, 0,
       doc: /* Return text selected from some pasteboard.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names.)
TYPE is the type of data desired, typically `STRING'.  */)
     (Lisp_Object selection_name, Lisp_Object target_type)
{
  Lisp_Object val;

  check_ns ();
  CHECK_SYMBOL (selection_name);
  CHECK_SYMBOL (target_type);
  val = ns_get_local_selection (selection_name, target_type);
  if (NILP (val))
    val = ns_get_foreign_selection (selection_name, target_type);
  if (CONSP (val) && SYMBOLP (Fcar (val)))
    {
      val = Fcdr (val);
      if (CONSP (val) && NILP (Fcdr (val)))
        val = Fcar (val);
    }
  val = clean_local_selection_data (val);
  return val;
}


#ifdef CUT_BUFFER_SUPPORT
DEFUN ("ns-get-cut-buffer-internal", Fns_get_cut_buffer_internal,
       Sns_get_cut_buffer_internal, 1, 1, 0,
       doc: /* Returns the value of the named cut buffer.  */)
     (Lisp_Object buffer)
{
  id pb;
  check_ns ();
  pb =[NSPasteboard pasteboardWithName: symbol_to_nsstring (buffer)];
  return ns_string_from_pasteboard (pb);
}


DEFUN ("ns-rotate-cut-buffers-internal", Fns_rotate_cut_buffers_internal,
       Sns_rotate_cut_buffers_internal, 1, 1, 0,
       doc: /* Rotate the values of the cut buffers by N steps.
Positive N means move values forward, negative means
backward. CURRENTLY NOT IMPLEMENTED UNDER NEXTSTEP. */ )
     (Lisp_Object n)
{
  /* XXX This function is unimplemented under NeXTstep XXX */
  Fsignal (Qquit, Fcons (build_string (
      "Warning: ns-rotate-cut-buffers-internal not implemented\n"), Qnil));
  return Qnil;
}


DEFUN ("ns-store-cut-buffer-internal", Fns_store_cut_buffer_internal,
       Sns_store_cut_buffer_internal, 2, 2, 0,
       doc: /* Sets the value of the named cut buffer (typically CUT_BUFFER0).  */)
     (Lisp_Object buffer, Lisp_Object string)
{
  id pb;
  check_ns ();
  pb =[NSPasteboard pasteboardWithName: symbol_to_nsstring (buffer)];
  ns_string_to_pasteboard (pb, string);
  return Qnil;
}
#endif


void
nxatoms_of_nsselect (void)
{
  NXPrimaryPboard = @"Selection";
  NXSecondaryPboard = @"Secondary";
}

void
syms_of_nsselect (void)
{
  QCLIPBOARD = intern_c_string ("CLIPBOARD");	staticpro (&QCLIPBOARD);
  QSECONDARY = intern_c_string ("SECONDARY");	staticpro (&QSECONDARY);
  QTEXT      = intern_c_string ("TEXT"); 	staticpro (&QTEXT);
  QFILE_NAME = intern_c_string ("FILE_NAME"); 	staticpro (&QFILE_NAME);

  defsubr (&Sx_disown_selection_internal);
  defsubr (&Sx_get_selection_internal);
  defsubr (&Sx_own_selection_internal);
  defsubr (&Sx_selection_exists_p);
  defsubr (&Sx_selection_owner_p);
#ifdef CUT_BUFFER_SUPPORT
  defsubr (&Sns_get_cut_buffer_internal);
  defsubr (&Sns_rotate_cut_buffers_internal);
  defsubr (&Sns_store_cut_buffer_internal);
#endif

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("ns-sent-selection-hooks", &Vns_sent_selection_hooks,
               "A list of functions to be called when Emacs answers a selection request.\n\
The functions are called with four arguments:\n\
  - the selection name (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');\n\
  - the selection-type which Emacs was asked to convert the\n\
    selection into before sending (for example, `STRING' or `LENGTH');\n\
  - a flag indicating success or failure for responding to the request.\n\
We might have failed (and declined the request) for any number of reasons,\n\
including being asked for a selection that we no longer own, or being asked\n\
to convert into a type that we don't know about or that is inappropriate.\n\
This hook doesn't let you change the behavior of Emacs's selection replies,\n\
it merely informs you that they have happened.");
  Vns_sent_selection_hooks = Qnil;

  DEFVAR_LISP ("selection-converter-alist", &Vselection_converter_alist,
               "An alist associating X Windows selection-types with functions.\n\
These functions are called to convert the selection, with three args:\n\
the name of the selection (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');\n\
a desired type to which the selection should be converted;\n\
and the local selection value (whatever was given to `x-own-selection').\n\
\n\
The function should return the value to send to the X server\n\
\(typically a string).  A return value of nil\n\
means that the conversion could not be done.\n\
A return value which is the symbol `NULL'\n\
means that a side-effect was executed,\n\
and there is no meaningful selection value.");
  Vselection_converter_alist = Qnil;

  DEFVAR_LISP ("ns-lost-selection-hooks", &Vns_lost_selection_hooks,
               "A list of functions to be called when Emacs loses an X selection.\n\
\(This happens when some other X client makes its own selection\n\
or when a Lisp program explicitly clears the selection.)\n\
The functions are called with one argument, the selection type\n\
\(a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD').");
  Vns_lost_selection_hooks = Qnil;

  Qforeign_selection = intern_c_string ("foreign-selection");
  staticpro (&Qforeign_selection);
}

// arch-tag: 39d1dde7-06a6-49ff-95a7-0e7af12d2218
