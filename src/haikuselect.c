/* Haiku window system selection support.
   Copyright (C) 2021 Free Software Foundation, Inc.

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
  Lisp_Object lispy_type = Qnil;

  if (!strcmp (SSDATA (name), "text/utf-8") ||
      !strcmp (SSDATA (name), "text/plain"))
    {
      if (string_ascii_p (str))
	lispy_type = QSTRING;
      else
	lispy_type = QUTF8_STRING;
    }

  if (!NILP (lispy_type))
    Fput_text_property (make_fixnum (0), make_fixnum (len),
			Qforeign_selection, lispy_type, str);

  block_input ();
  BClipboard_free_data (dat);
  unblock_input ();

  return str;
}

DEFUN ("haiku-selection-put", Fhaiku_selection_put, Shaiku_selection_put,
       3, 3, 0,
       doc: /* Add or remove content from the clipboard CLIPBOARD.
CLIPBOARD is the symbol `PRIMARY', `SECONDARY' or `CLIPBOARD'.  NAME
is a MIME type denoting the type of the data to add.  DATA is the
string that will be placed in the clipboard, or nil if the content is
to be removed.  If NAME is the string `text/utf-8' or the string
`text/plain', encode it as UTF-8 before storing it into the
clipboard.  */)
  (Lisp_Object clipboard, Lisp_Object name, Lisp_Object data)
{
  CHECK_SYMBOL (clipboard);
  CHECK_STRING (name);
  if (!NILP (data))
    CHECK_STRING (data);

  block_input ();
  /* It seems that Haiku applications counter-intuitively expect
     UTF-8 data in both text/utf-8 and text/plain.  */
  if (!NILP (data) && STRING_MULTIBYTE (data) &&
      (!strcmp (SSDATA (name), "text/utf-8") ||
       !strcmp (SSDATA (name), "text/plain")))
    data = ENCODE_UTF_8 (data);

  char *dat = !NILP (data) ? SSDATA (data) : NULL;
  ptrdiff_t len = !NILP (data) ? SBYTES (data) : 0;

  if (EQ (clipboard, QPRIMARY))
    BClipboard_set_primary_selection_data (SSDATA (name), dat, len);
  else if (EQ (clipboard, QSECONDARY))
    BClipboard_set_secondary_selection_data (SSDATA (name), dat, len);
  else if (EQ (clipboard, QCLIPBOARD))
    BClipboard_set_system_data (SSDATA (name), dat, len);
  else
    {
      unblock_input ();
      signal_error ("Bad clipboard", clipboard);
    }
  unblock_input ();

  return Qnil;
}

void
syms_of_haikuselect (void)
{
  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QSTRING, "STRING");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");
  DEFSYM (Qforeign_selection, "foreign-selection");

  defsubr (&Shaiku_selection_data);
  defsubr (&Shaiku_selection_put);
}
