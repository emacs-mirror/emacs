/* NeXT/Open/GNUstep / macOS Cocoa selection processing for emacs.
   Copyright (C) 1993-1994, 2005-2006, 2008-2018 Free Software
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

/*
Originally by Carl Edman
Updated by Christian Limpach (chris@nice.ch)
OpenStep/Rhapsody port by Scott Bender (sbender@harmony-ds.com)
macOS/Aqua port by Christophe de Dinechin (descubes@earthlink.net)
GNUstep port and post-20 update by Adrian Robert (arobert@cogsci.ucsd.edu)
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes.  */
#include <config.h>

#include "lisp.h"
#include "nsterm.h"
#include "termhooks.h"
#include "keyboard.h"

static Lisp_Object Vselection_alist;

/* NSPasteboardNameGeneral is pretty much analogous to X11 CLIPBOARD */
static NSString *NXPrimaryPboard;
static NSString *NXSecondaryPboard;


static NSMutableDictionary *pasteboard_changecount;

/* ==========================================================================

    Internal utility functions

   ========================================================================== */


static NSString *
symbol_to_nsstring (Lisp_Object sym)
{
  CHECK_SYMBOL (sym);
  if (EQ (sym, QCLIPBOARD))   return NSPasteboardNameGeneral;
  if (EQ (sym, QPRIMARY))     return NXPrimaryPboard;
  if (EQ (sym, QSECONDARY))   return NXSecondaryPboard;
  if (EQ (sym, QTEXT))        return NSStringPboardType;
  return [NSString stringWithUTF8String: SSDATA (SYMBOL_NAME (sym))];
}

static NSPasteboard *
ns_symbol_to_pb (Lisp_Object symbol)
{
  return [NSPasteboard pasteboardWithName: symbol_to_nsstring (symbol)];
}

static Lisp_Object
ns_string_to_symbol (NSString *t)
{
  if ([t isEqualToString: NSPasteboardNameGeneral])
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
      ptrdiff_t i;
      ptrdiff_t size = ASIZE (obj);
      Lisp_Object copy;

      if (size == 1)
        return clean_local_selection_data (AREF (obj, 0));
      copy = make_uninit_vector (size);
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
ns_store_pb_change_count (id pb)
{
  [pasteboard_changecount
        setObject: [NSNumber numberWithLong: [pb changeCount]]
           forKey: [pb name]];
}

static NSInteger
ns_get_pb_change_count (Lisp_Object selection)
{
  id pb = ns_symbol_to_pb (selection);
  return pb != nil ? [pb changeCount] : -1;
}

static NSInteger
ns_get_our_change_count_for (Lisp_Object selection)
{
  NSNumber *num = [pasteboard_changecount
                    objectForKey: symbol_to_nsstring (selection)];
  return num != nil ? (NSInteger)[num longValue] : -1;
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

      utfStr = SSDATA (str);
      nsStr = [[NSString alloc] initWithBytesNoCopy: utfStr
                                             length: SBYTES (str)
                                           encoding: NSUTF8StringEncoding
                                       freeWhenDone: NO];
      // FIXME: Why those 2 different code paths?
      if (gtype == nil)
        {
	  // Used for ns_string_to_pasteboard
          [pb declareTypes: ns_send_types owner: nil];
          tenum = [ns_send_types objectEnumerator];
          while ( (type = [tenum nextObject]) )
            [pb setString: nsStr forType: type];
        }
      else
        {
	  // Used for ns-own-selection-internal.
	  eassert (gtype == NSStringPboardType);
          [pb setString: nsStr forType: gtype];
        }
      [nsStr release];
      ns_store_pb_change_count (pb);
    }
}


Lisp_Object
ns_get_local_selection (Lisp_Object selection_name,
                        Lisp_Object target_type)
{
  Lisp_Object local_value;
  local_value = assq_no_quit (selection_name, Vselection_alist);
  return local_value;
}


static Lisp_Object
ns_get_foreign_selection (Lisp_Object symbol, Lisp_Object target)
{
  id pb;
  pb = ns_symbol_to_pb (symbol);
  return pb != nil ? ns_string_from_pasteboard (pb) : Qnil;
}




/* ==========================================================================

    Functions used externally

   ========================================================================== */


Lisp_Object
ns_string_from_pasteboard (id pb)
{
  NSString *type, *str;
  const char *utfStr;
  int length;

  type = [pb availableTypeFromArray: ns_return_types];
  if (type == nil)
    {
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
      length = [mstr lengthOfBytesUsingEncoding: NSUTF8StringEncoding];

#if ! defined (NS_IMPL_COCOA)
      if (!utfStr)
        {
          utfStr = [mstr cString];
          length = strlen (utfStr);
        }
#endif
    }
  NS_HANDLER
    {
      message1 ("ns_string_from_pasteboard: UTF8String failed\n");
#if defined (NS_IMPL_COCOA)
      utfStr = "Conversion failed";
#else
      utfStr = [str lossyCString];
#endif
      length = strlen (utfStr);
    }
  NS_ENDHANDLER

    return make_string (utfStr, length);
}


void
ns_string_to_pasteboard (id pb, Lisp_Object str)
{
  ns_string_to_pasteboard_internal (pb, str, nil);
}



/* ==========================================================================

    Lisp Defuns

   ========================================================================== */


DEFUN ("ns-own-selection-internal", Fns_own_selection_internal,
       Sns_own_selection_internal, 2, 2, 0,
       doc: /* Assert an X selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.  */)
     (Lisp_Object selection, Lisp_Object value)
{
  id pb;
  NSString *type;
  Lisp_Object successful_p = Qnil, rest;
  Lisp_Object target_symbol;

  check_window_system (NULL);
  CHECK_SYMBOL (selection);
  if (NILP (value))
    error ("Selection value may not be nil");
  pb = ns_symbol_to_pb (selection);
  if (pb == nil) return Qnil;

  ns_declare_pasteboard (pb);
  {
    Lisp_Object old_value = assq_no_quit (selection, Vselection_alist);
    Lisp_Object new_value = list2 (selection, value);

    if (NILP (old_value))
      Vselection_alist = Fcons (new_value, Vselection_alist);
    else
      Fsetcdr (old_value, Fcdr (new_value));
  }

  /* We only support copy of text.  */
  type = NSStringPboardType;
  target_symbol = ns_string_to_symbol (type);
  if (STRINGP (value))
    {
      ns_string_to_pasteboard_internal (pb, value, type);
      successful_p = Qt;
    }

  if (!EQ (Vns_sent_selection_hooks, Qunbound))
    {
      /* FIXME: Use run-hook-with-args!  */
      for (rest = Vns_sent_selection_hooks; CONSP (rest); rest = Fcdr (rest))
        call3 (Fcar (rest), selection, target_symbol, successful_p);
    }

  return value;
}


DEFUN ("ns-disown-selection-internal", Fns_disown_selection_internal,
       Sns_disown_selection_internal, 1, 1, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.  */)
  (Lisp_Object selection)
{
  id pb;
  check_window_system (NULL);
  CHECK_SYMBOL (selection);

  if (ns_get_pb_change_count (selection)
      != ns_get_our_change_count_for (selection))
      return Qnil;

  pb = ns_symbol_to_pb (selection);
  if (pb != nil) ns_undeclare_pasteboard (pb);
  return Qt;
}


DEFUN ("ns-selection-exists-p", Fns_selection_exists_p, Sns_selection_exists_p,
       0, 1, 0, doc: /* Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  (X expects
these literal upper-case names.)  The symbol nil is the same as
`PRIMARY', and t is the same as `SECONDARY'.  */)
     (Lisp_Object selection)
{
  id pb;
  NSArray *types;

  if (!window_system_available (NULL))
    return Qnil;

  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  pb = ns_symbol_to_pb (selection);
  if (pb == nil) return Qnil;

  types = [pb types];
  return ([types count] == 0) ? Qnil : Qt;
}


DEFUN ("ns-selection-owner-p", Fns_selection_owner_p, Sns_selection_owner_p,
       0, 1, 0,
       doc: /* Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.  */)
     (Lisp_Object selection)
{
  check_window_system (NULL);
  CHECK_SYMBOL (selection);
  if (EQ (selection, Qnil)) selection = QPRIMARY;
  if (EQ (selection, Qt)) selection = QSECONDARY;
  return ns_get_pb_change_count (selection)
    == ns_get_our_change_count_for (selection)
    ? Qt : Qnil;
}


DEFUN ("ns-get-selection", Fns_get_selection,
       Sns_get_selection, 2, 2, 0,
       doc: /* Return text selected from some X window.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'.  */)
     (Lisp_Object selection_name, Lisp_Object target_type)
{
  Lisp_Object val = Qnil;

  check_window_system (NULL);
  CHECK_SYMBOL (selection_name);
  CHECK_SYMBOL (target_type);

  if (ns_get_pb_change_count (selection_name)
      == ns_get_our_change_count_for (selection_name))
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


void
nxatoms_of_nsselect (void)
{
  NXPrimaryPboard = @"Selection";
  NXSecondaryPboard = @"Secondary";

  // This is a memory loss, never released.
  pasteboard_changecount
    = [[NSMutableDictionary
	 dictionaryWithObjectsAndKeys:
	     [NSNumber numberWithLong:0], NSPasteboardNameGeneral,
	     [NSNumber numberWithLong:0], NXPrimaryPboard,
	     [NSNumber numberWithLong:0], NXSecondaryPboard,
	     [NSNumber numberWithLong:0], NSStringPboardType,
	     [NSNumber numberWithLong:0], NSFilenamesPboardType,
	     [NSNumber numberWithLong:0], NSTabularTextPboardType,
	 nil] retain];
}

void
syms_of_nsselect (void)
{
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QTEXT, "TEXT");
  DEFSYM (QFILE_NAME, "FILE_NAME");

  defsubr (&Sns_disown_selection_internal);
  defsubr (&Sns_get_selection);
  defsubr (&Sns_own_selection_internal);
  defsubr (&Sns_selection_exists_p);
  defsubr (&Sns_selection_owner_p);

  Vselection_alist = Qnil;
  staticpro (&Vselection_alist);

  DEFVAR_LISP ("ns-sent-selection-hooks", Vns_sent_selection_hooks,
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
}
