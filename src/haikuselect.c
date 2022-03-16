/* Haiku window system selection support.
   Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

#include "lisp.h"
#include "blockinput.h"
#include "coding.h"
#include "haikuselect.h"
#include "haikuterm.h"

#include <stdlib.h>

static Lisp_Object
haiku_selection_data_1 (Lisp_Object clipboard)
{
  Lisp_Object result = Qnil;
  char *targets[256];

  block_input ();
  if (EQ (clipboard, QPRIMARY))
    BClipboard_primary_targets ((char **) &targets, 256);
  else if (EQ (clipboard, QSECONDARY))
    BClipboard_secondary_targets ((char **) &targets, 256);
  else if (EQ (clipboard, QCLIPBOARD))
    BClipboard_system_targets ((char **) &targets, 256);
  else
    {
      unblock_input ();
      signal_error ("Bad clipboard", clipboard);
    }

  for (int i = 0; targets[i]; ++i)
    {
      result = Fcons (build_unibyte_string (targets[i]),
		      result);
      free (targets[i]);
    }
  unblock_input ();

  return result;
}

DEFUN ("haiku-selection-targets", Fhaiku_selection_targets,
       Shaiku_selection_targets, 1, 1, 0,
       doc: /* Find the types of data available from CLIPBOARD.
CLIPBOARD should be the symbol `PRIMARY', `SECONDARY' or `CLIPBOARD'.
Return the available types as a list of strings.  */)
  (Lisp_Object clipboard)
{
  return haiku_selection_data_1 (clipboard);
}

DEFUN ("haiku-selection-data", Fhaiku_selection_data, Shaiku_selection_data,
       2, 2, 0,
       doc: /* Retrieve content typed as NAME from the clipboard
CLIPBOARD.  CLIPBOARD is the symbol `PRIMARY', `SECONDARY' or
`CLIPBOARD'.  NAME is a MIME type denoting the type of the data to
fetch.  */)
  (Lisp_Object clipboard, Lisp_Object name)
{
  CHECK_SYMBOL (clipboard);
  CHECK_STRING (name);
  char *dat;
  ssize_t len;

  block_input ();
  if (EQ (clipboard, QPRIMARY))
    dat = BClipboard_find_primary_selection_data (SSDATA (name), &len);
  else if (EQ (clipboard, QSECONDARY))
    dat = BClipboard_find_secondary_selection_data (SSDATA (name), &len);
  else if (EQ (clipboard, QCLIPBOARD))
    dat = BClipboard_find_system_data (SSDATA (name), &len);
  else
    {
      unblock_input ();
      signal_error ("Bad clipboard", clipboard);
    }
  unblock_input ();

  if (!dat)
    return Qnil;

  Lisp_Object str = make_unibyte_string (dat, len);

  /* `foreign-selection' just means that the selection has to be
     decoded by `gui-get-selection'.  It has no other meaning,
     AFAICT.  */
  Fput_text_property (make_fixnum (0), make_fixnum (len),
		      Qforeign_selection, Qt, str);

  block_input ();
  BClipboard_free_data (dat);
  unblock_input ();

  return str;
}

DEFUN ("haiku-selection-put", Fhaiku_selection_put, Shaiku_selection_put,
       3, 4, 0,
       doc: /* Add or remove content from the clipboard CLIPBOARD.
CLIPBOARD is the symbol `PRIMARY', `SECONDARY' or `CLIPBOARD'.  NAME
is a MIME type denoting the type of the data to add.  DATA is the
string that will be placed in the clipboard, or nil if the content is
to be removed.  CLEAR, if non-nil, means to erase all the previous
contents of the clipboard.  */)
  (Lisp_Object clipboard, Lisp_Object name, Lisp_Object data,
   Lisp_Object clear)
{
  CHECK_SYMBOL (clipboard);
  CHECK_STRING (name);
  if (!NILP (data))
    CHECK_STRING (data);

  block_input ();
  char *dat = !NILP (data) ? SSDATA (data) : NULL;
  ptrdiff_t len = !NILP (data) ? SBYTES (data) : 0;

  if (EQ (clipboard, QPRIMARY))
    BClipboard_set_primary_selection_data (SSDATA (name), dat, len,
					   !NILP (clear));
  else if (EQ (clipboard, QSECONDARY))
    BClipboard_set_secondary_selection_data (SSDATA (name), dat, len,
					     !NILP (clear));
  else if (EQ (clipboard, QCLIPBOARD))
    BClipboard_set_system_data (SSDATA (name), dat, len, !NILP (clear));
  else
    {
      unblock_input ();
      signal_error ("Bad clipboard", clipboard);
    }
  unblock_input ();

  return Qnil;
}

DEFUN ("haiku-selection-owner-p", Fhaiku_selection_owner_p, Shaiku_selection_owner_p,
       0, 1, 0,
       doc: /* Whether the current Emacs process owns the given SELECTION.
The arg should be the name of the selection in question, typically one
of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  For
convenience, the symbol nil is the same as `PRIMARY', and t is the
same as `SECONDARY'.  */)
  (Lisp_Object selection)
{
  bool value;

  if (NILP (selection))
    selection = QPRIMARY;
  else if (EQ (selection, Qt))
    selection = QSECONDARY;

  block_input ();
  if (EQ (selection, QPRIMARY))
    value = BClipboard_owns_primary ();
  else if (EQ (selection, QSECONDARY))
    value = BClipboard_owns_secondary ();
  else if (EQ (selection, QCLIPBOARD))
    value = BClipboard_owns_clipboard ();
  else
    value = false;
  unblock_input ();

  return value ? Qt : Qnil;
}

/* Return the Lisp representation of MESSAGE.

   It is an alist of strings, denoting message parameter names, to a
   list the form (TYPE . (DATA ...)), where TYPE is an integer
   denoting the system data type of DATA, and DATA is in the general
   case a unibyte string.

   If TYPE is a symbol instead of an integer, then DATA was specially
   decoded.  If TYPE is `ref', then DATA is the absolute file name of
   a file, or nil if decoding the file name failed.  If TYPE is
   `string', then DATA is a unibyte string.  If TYPE is `short', then
   DATA is a 16-bit signed integer.  If TYPE is `long', then DATA is a
   32-bit signed integer.  If TYPE is `llong', then DATA is a 64-bit
   signed integer. If TYPE is `byte' or `char', then DATA is an 8-bit
   signed integer.  If TYPE is `bool', then DATA is a boolean.  */
Lisp_Object
haiku_message_to_lisp (void *message)
{
  Lisp_Object list = Qnil, tem, t1, t2;
  const char *name;
  char *pbuf;
  const void *buf;
  ssize_t buf_size;
  int32 i, j, count, type_code;
  int rc;

  for (i = 0; !be_enum_message (message, &type_code, i,
				&count, &name); ++i)
    {
      tem = Qnil;

      for (j = 0; j < count; ++j)
	{
	  rc = be_get_message_data (message, name,
				    type_code, j,
				    &buf, &buf_size);
	  if (rc)
	    emacs_abort ();

	  switch (type_code)
	    {
	    case 'BOOL':
	      t1 = (*(bool *) buf) ? Qt : Qnil;
	      break;

	    case 'RREF':
	      rc = be_get_refs_data (message, name,
				     j, &pbuf);

	      if (rc)
		{
		  t1 = Qnil;
		  break;
		}

	      if (!pbuf)
		memory_full (SIZE_MAX);

	      t1 = build_string (pbuf);
	      free (pbuf);
	      break;

	    case 'SHRT':
	      t1 = make_fixnum (*(int16 *) buf);
	      break;

	    case 'LONG':
	      t1 = make_int (*(int32 *) buf);
	      break;

	    case 'LLNG':
	      t1 = make_int ((intmax_t) *(int64 *) buf);
	      break;

	    case 'BYTE':
	    case 'CHAR':
	      t1 = make_fixnum (*(int8 *) buf);
	      break;

	    default:
	      t1 = make_uninit_string (buf_size);
	      memcpy (SDATA (t1), buf, buf_size);
	    }

	  tem = Fcons (t1, tem);
	}

      switch (type_code)
	{
	case 'CSTR':
	  t2 = Qstring;
	  break;

	case 'SHRT':
	  t2 = Qshort;
	  break;

	case 'LONG':
	  t2 = Qlong;
	  break;

	case 'LLNG':
	  t2 = Qllong;
	  break;

	case 'BYTE':
	  t2 = Qbyte;
	  break;

	case 'RREF':
	  t2 = Qref;
	  break;

	case 'CHAR':
	  t2 = Qchar;
	  break;

	case 'BOOL':
	  t2 = Qbool;
	  break;

	default:
	  t2 = make_int (type_code);
	}

      tem = Fcons (t2, tem);
      list = Fcons (Fcons (build_string_from_utf8 (name), tem), list);
    }

  return list;
}

void
syms_of_haikuselect (void)
{
  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QSTRING, "STRING");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");
  DEFSYM (Qforeign_selection, "foreign-selection");
  DEFSYM (QTARGETS, "TARGETS");
  DEFSYM (Qstring, "string");
  DEFSYM (Qref, "ref");
  DEFSYM (Qshort, "short");
  DEFSYM (Qlong, "long");
  DEFSYM (Qllong, "llong");
  DEFSYM (Qbyte, "byte");
  DEFSYM (Qchar, "char");
  DEFSYM (Qbool, "bool");

  defsubr (&Shaiku_selection_data);
  defsubr (&Shaiku_selection_put);
  defsubr (&Shaiku_selection_targets);
  defsubr (&Shaiku_selection_owner_p);
}
