/* Haiku window system selection support.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
#include "haiku_support.h"
#include "keyboard.h"

#include <stdlib.h>

/* The frame that is currently the source of a drag-and-drop
   operation, or NULL if none is in progress.  The reason for this
   variable is to prevent it from being deleted, which really breaks
   the nested event loop inside be_drag_message.  */
struct frame *haiku_dnd_frame;

/* Whether or not to move the tip frame during drag-and-drop.  */
bool haiku_dnd_follow_tooltip;

/* Whether or not the current DND frame is able to receive drops from
   the current drag-and-drop operation.  */
bool haiku_dnd_allow_same_frame;

static void haiku_lisp_to_message (Lisp_Object, void *);

static enum haiku_clipboard
haiku_get_clipboard_name (Lisp_Object clipboard)
{
  if (EQ (clipboard, QPRIMARY))
    return CLIPBOARD_PRIMARY;

  if (EQ (clipboard, QSECONDARY))
    return CLIPBOARD_SECONDARY;

  if (EQ (clipboard, QCLIPBOARD))
    return CLIPBOARD_CLIPBOARD;

  signal_error ("Invalid clipboard", clipboard);
}

DEFUN ("haiku-selection-timestamp", Fhaiku_selection_timestamp,
       Shaiku_selection_timestamp, 1, 1, 0,
       doc: /* Retrieve the "timestamp" of the clipboard CLIPBOARD.
CLIPBOARD can either be the symbol `PRIMARY', `SECONDARY' or
`CLIPBOARD'.  The timestamp is returned as a number describing the
number of times programs have put data into CLIPBOARD.  */)
  (Lisp_Object clipboard)
{
  enum haiku_clipboard clipboard_name;
  int64 timestamp;

  clipboard_name = haiku_get_clipboard_name (clipboard);
  timestamp = be_get_clipboard_count (clipboard_name);

  return INT_TO_INTEGER (timestamp);
}

DEFUN ("haiku-selection-data", Fhaiku_selection_data, Shaiku_selection_data,
       2, 2, 0,
       doc: /* Retrieve content typed as NAME from the clipboard
CLIPBOARD.  CLIPBOARD is the symbol `PRIMARY', `SECONDARY' or
`CLIPBOARD'.  NAME is a string describing the MIME type denoting the
type of the data to fetch.  If NAME is nil, then the entire contents
of the clipboard will be returned instead, as a serialized system
message in the format accepted by `haiku-drag-message', which see.  */)
  (Lisp_Object clipboard, Lisp_Object name)
{
  char *dat;
  ssize_t len;
  Lisp_Object str;
  void *message;
  enum haiku_clipboard clipboard_name;
  int rc;

  CHECK_SYMBOL (clipboard);
  clipboard_name = haiku_get_clipboard_name (clipboard);

  if (!NILP (name))
    {
      CHECK_STRING (name);

      block_input ();
      dat = be_find_clipboard_data (clipboard_name,
				    SSDATA (name), &len);
      unblock_input ();

      if (!dat)
	return Qnil;

      str = make_unibyte_string (dat, len);

      /* `foreign-selection' just means that the selection has to be
	 decoded by `gui-get-selection'.  It has no other meaning,
	 AFAICT.  */
      Fput_text_property (make_fixnum (0), make_fixnum (len),
			  Qforeign_selection, Qt, str);

      block_input ();
      free (dat);
      unblock_input ();
    }
  else
    {
      block_input ();
      rc = be_lock_clipboard_message (clipboard_name, &message, false);
      unblock_input ();

      if (rc)
	signal_error ("Couldn't open clipboard", clipboard);

      block_input ();
      str = haiku_message_to_lisp (message);
      be_unlock_clipboard (clipboard_name, true);
      unblock_input ();
    }

  return str;
}

static void
haiku_unwind_clipboard_lock (int clipboard)
{
  be_unlock_clipboard (clipboard, false);
}

DEFUN ("haiku-selection-put", Fhaiku_selection_put, Shaiku_selection_put,
       2, 4, 0,
       doc: /* Add or remove content from the clipboard CLIPBOARD.
CLIPBOARD is the symbol `PRIMARY', `SECONDARY' or `CLIPBOARD'.  NAME
is a MIME type denoting the type of the data to add.  DATA is the
string that will be placed in the clipboard, or nil if the content is
to be removed.  CLEAR, if non-nil, means to erase all the previous
contents of the clipboard.

Alternatively, NAME can be a system message in the format accepted by
`haiku-drag-message', which will replace the contents of CLIPBOARD.
In that case, the arguments after NAME are ignored.  */)
  (Lisp_Object clipboard, Lisp_Object name, Lisp_Object data,
   Lisp_Object clear)
{
  enum haiku_clipboard clipboard_name;
  specpdl_ref ref;
  char *dat;
  ptrdiff_t len;
  int rc;
  void *message;

  CHECK_SYMBOL (clipboard);
  clipboard_name = haiku_get_clipboard_name (clipboard);

  if (CONSP (name) || NILP (name))
    {
      be_update_clipboard_count (clipboard_name);

      rc = be_lock_clipboard_message (clipboard_name,
				      &message, true);

      if (rc)
	signal_error ("Couldn't open clipboard", clipboard);

      ref = SPECPDL_INDEX ();
      record_unwind_protect_int (haiku_unwind_clipboard_lock,
				 clipboard_name);
      haiku_lisp_to_message (name, message);

      return unbind_to (ref, Qnil);
    }

  CHECK_STRING (name);
  if (!NILP (data))
    CHECK_STRING (data);

  dat = !NILP (data) ? SSDATA (data) : NULL;
  len = !NILP (data) ? SBYTES (data) : 0;

  be_set_clipboard_data (clipboard_name, SSDATA (name), dat, len,
			 !NILP (clear));
  return Qnil;
}

DEFUN ("haiku-selection-owner-p", Fhaiku_selection_owner_p, Shaiku_selection_owner_p,
       0, 1, 0,
       doc: /* Whether the current Emacs process owns the given SELECTION.
The arg should be the name of the selection in question, typically one
of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  */)
  (Lisp_Object selection)
{
  bool value;
  enum haiku_clipboard name;

  block_input ();
  name = haiku_get_clipboard_name (selection);
  value = be_clipboard_owner_p (name);
  unblock_input ();

  return value ? Qt : Qnil;
}

/* Return the Lisp representation of MESSAGE.  See Fhaiku_drag_message
   for the format of the object returned.  */
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
  void *msg;
  float point_x, point_y;

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
	    case 'MSGG':
	      msg = be_get_message_message (message, name, j);
	      if (!msg)
		memory_full (SIZE_MAX);
	      t1 = haiku_message_to_lisp (msg);
	      BMessage_delete (msg);

	      break;

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

	      t1 = DECODE_FILE (build_string (pbuf));

	      free (pbuf);
	      break;

	    case 'BPNT':
	      rc = be_get_point_data (message, name,
				      j, &point_x,
				      &point_y);

	      if (rc)
		{
		  t1 = Qnil;
		  break;
		}

	      t1 = Fcons (make_float (point_x),
			  make_float (point_y));
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

	    case 'SIZT':
	      t1 = make_uint ((uintmax_t) *(size_t *) buf);
	      break;

	    case 'SSZT':
	      t1 = make_int ((intmax_t) *(ssize_t *) buf);
	      break;

	    case 'DBLE':
	      t1 = make_float (*(double *) buf);
	      break;

	    case 'FLOT':
	      t1 = make_float (*(float *) buf);
	      break;

	    case 'CSTR':
	      /* Is this even possible? */
	      if (!buf_size)
		buf_size = 1;

	      t1 = make_uninit_string (buf_size - 1);
	      memcpy (SDATA (t1), buf, buf_size - 1);
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

	case 'MSGG':
	  t2 = Qmessage;
	  break;

	case 'SIZT':
	  t2 = Qsize_t;
	  break;

	case 'SSZT':
	  t2 = Qssize_t;
	  break;

	case 'BPNT':
	  t2 = Qpoint;
	  break;

	case 'DBLE':
	  t2 = Qdouble;
	  break;

	case 'FLOT':
	  t2 = Qfloat;
	  break;

	default:
	  t2 = make_int (type_code);
	}

      tem = Fcons (t2, tem);
      list = Fcons (Fcons (build_string_from_utf8 (name), tem), list);
    }

  tem = Fcons (Qtype, make_uint (be_get_message_type (message)));
  return Fcons (tem, list);
}

static int32
lisp_to_type_code (Lisp_Object obj)
{
  if (BIGNUMP (obj))
    return (int32) bignum_to_intmax (obj);

  if (FIXNUMP (obj))
    return XFIXNUM (obj);

  if (EQ (obj, Qstring))
    return 'CSTR';
  else if (EQ (obj, Qshort))
    return 'SHRT';
  else if (EQ (obj, Qlong))
    return 'LONG';
  else if (EQ (obj, Qllong))
    return 'LLNG';
  else if (EQ (obj, Qbyte))
    return 'BYTE';
  else if (EQ (obj, Qref))
    return 'RREF';
  else if (EQ (obj, Qchar))
    return 'CHAR';
  else if (EQ (obj, Qbool))
    return 'BOOL';
  else if (EQ (obj, Qmessage))
    return 'MSGG';
  else if (EQ (obj, Qsize_t))
    return 'SIZT';
  else if (EQ (obj, Qssize_t))
    return 'SSZT';
  else if (EQ (obj, Qpoint))
    return 'BPNT';
  else if (EQ (obj, Qfloat))
    return 'FLOT';
  else if (EQ (obj, Qdouble))
    return 'DBLE';
  else
    return -1;
}

static void
haiku_lisp_to_message (Lisp_Object obj, void *message)
{
  Lisp_Object tem, t1, name, type_sym, t2, data;
  int32 type_code, long_data;
  int16 short_data;
  int64 llong_data;
  int8 char_data;
  bool bool_data;
  void *msg_data;
  size_t sizet_data;
  ssize_t ssizet_data;
  intmax_t t4;
  uintmax_t t5;
  float t6, t7, float_data;
  double double_data;
  int rc;
  specpdl_ref ref;

  tem = obj;

  FOR_EACH_TAIL (tem)
    {
      t1 = XCAR (tem);
      CHECK_CONS (t1);

      name = XCAR (t1);

      if (EQ (name, Qtype))
	{
	  t2 = XCDR (t1);

	  if (BIGNUMP (t2))
	    {
	      t5 = bignum_to_uintmax (t2);

	      if (!t5 || t5 > TYPE_MAXIMUM (uint32))
		signal_error ("Value too large", t2);

	      block_input ();
	      be_set_message_type (message, t5);
	      unblock_input ();
	    }
	  else
	    {
	      if (!TYPE_RANGED_FIXNUMP (uint32, t2))
		signal_error ("Invalid data type", t2);

	      block_input ();
	      be_set_message_type (message, XFIXNAT (t2));
	      unblock_input ();
	    }

	  continue;
	}

      CHECK_STRING (name);

      t1 = XCDR (t1);
      CHECK_CONS (t1);

      type_sym = XCAR (t1);
      type_code = lisp_to_type_code (type_sym);

      if (type_code == -1)
	signal_error ("Unknown data type", type_sym);

      CHECK_LIST (t1);
      t2 = XCDR (t1);
      FOR_EACH_TAIL (t2)
	{
	  data = XCAR (t2);

	  if (FIXNUMP (type_sym) || BIGNUMP (type_sym))
	    goto decode_normally;

	  switch (type_code)
	    {
	    case 'MSGG':
	      ref = SPECPDL_INDEX ();

	      block_input ();
	      msg_data = be_create_simple_message ();
	      unblock_input ();

	      record_unwind_protect_ptr (BMessage_delete, msg_data);
	      haiku_lisp_to_message (data, msg_data);

	      block_input ();
	      rc = be_add_message_message (message, SSDATA (name), msg_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Invalid message", data);
	      unbind_to (ref, Qnil);
	      break;

	    case 'RREF':
	      CHECK_STRING (data);

	      if (be_add_refs_data (message, SSDATA (name),
				    SSDATA (ENCODE_FILE (data)))
		  && haiku_signal_invalid_refs)
		signal_error ("Invalid file name", data);
	      break;

	    case 'BPNT':
	      CHECK_CONS (data);
	      CHECK_NUMBER (XCAR (data));
	      CHECK_NUMBER (XCDR (data));

	      t6 = XFLOATINT (XCAR (data));
	      t7 = XFLOATINT (XCDR (data));

	      if (be_add_point_data (message, SSDATA (name),
				     t6, t7))
		signal_error ("Invalid point", data);
	      break;

	    case 'FLOT':
	      CHECK_NUMBER (data);
	      float_data = XFLOATINT (data);

	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &float_data,
					sizeof float_data);

	      if (rc)
		signal_error ("Failed to add float", data);
	      break;

	    case 'DBLE':
	      CHECK_NUMBER (data);
	      double_data = XFLOATINT (data);

	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &double_data,
					sizeof double_data);

	      if (rc)
		signal_error ("Failed to add double", data);
	      break;

	    case 'SHRT':
	      if (!TYPE_RANGED_FIXNUMP (int16, data))
		signal_error ("Invalid value", data);
	      short_data = XFIXNUM (data);

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &short_data,
					sizeof short_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add short", data);
	      break;

	    case 'LONG':
	      if (BIGNUMP (data))
		{
		  t4 = bignum_to_intmax (data);

		  /* We know that int32 is signed.  */
		  if (!t4 || t4 > TYPE_MINIMUM (int32)
		      || t4 < TYPE_MAXIMUM (int32))
		    signal_error ("Value too large", data);

		  long_data = (int32) t4;
		}
	      else
		{
		  if (!TYPE_RANGED_FIXNUMP (int32, data))
		    signal_error ("Invalid value", data);

		  long_data = (int32) XFIXNUM (data);
		}

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &long_data,
					sizeof long_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add long", data);
	      break;

	    case 'LLNG':
	      if (BIGNUMP (data))
		{
		  t4 = bignum_to_intmax (data);

		  if (!t4 || t4 > TYPE_MINIMUM (int64)
		      || t4 < TYPE_MAXIMUM (int64))
		    signal_error ("Value too large", data);

		  llong_data = (int64) t4;
		}
	      else
		{
		  if (!TYPE_RANGED_FIXNUMP (int64, data))
		    signal_error ("Invalid value", data);

		  llong_data = (int64) XFIXNUM (data);
		}

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &llong_data,
					sizeof llong_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add llong", data);
	      break;

	    case 'SIZT':
	      if (BIGNUMP (data))
		{
		  t4 = bignum_to_intmax (data);

		  if (!t4 || t4 > TYPE_MAXIMUM (size_t))
		    signal_error ("Value too large", data);

		  sizet_data = (size_t) t4;
		}
	      else
		{
		  if (!TYPE_RANGED_FIXNUMP (size_t, data))
		    signal_error ("Invalid value", data);

		  sizet_data = (int64) XFIXNUM (data);
		}

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &sizet_data,
					sizeof sizet_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add sizet", data);
	      break;

	    case 'SSZT':
	      if (BIGNUMP (data))
		{
		  t4 = bignum_to_intmax (data);

		  if (!t4 || t4 > TYPE_MINIMUM (ssize_t)
		      || t4 < TYPE_MAXIMUM (ssize_t))
		    signal_error ("Value too large", data);

		  ssizet_data = (ssize_t) t4;
		}
	      else
		{
		  if (!TYPE_RANGED_FIXNUMP (ssize_t, data))
		    signal_error ("Invalid value", data);

		  ssizet_data = (int64) XFIXNUM (data);
		}

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &ssizet_data,
					sizeof ssizet_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add ssizet", data);
	      break;

	    case 'CHAR':
	    case 'BYTE':
	      if (!TYPE_RANGED_FIXNUMP (int8, data))
		signal_error ("Invalid value", data);
	      char_data = XFIXNUM (data);

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &char_data,
					sizeof char_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add char", data);
	      break;

	    case 'BOOL':
	      bool_data = !NILP (data);

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, &bool_data,
					sizeof bool_data);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add bool", data);
	      break;

	    case 'CSTR':
	      /* C strings must be handled specially, since they
		 include a trailing NULL byte.  */
	      CHECK_STRING (data);

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, SDATA (data),
					SBYTES (data) + 1);
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add", data);
	      break;

	    default:
	    decode_normally:
	      CHECK_STRING (data);

	      block_input ();
	      rc = be_add_message_data (message, SSDATA (name),
					type_code, SDATA (data),
					SBYTES (data));
	      unblock_input ();

	      if (rc)
		signal_error ("Failed to add", data);
	    }
	}
      CHECK_LIST_END (t2, t1);
    }
  CHECK_LIST_END (tem, obj);
}

static bool
haiku_should_quit_drag (void)
{
  return !NILP (Vquit_flag);
}

static void
haiku_unwind_drag_message (void *message)
{
  haiku_dnd_frame = NULL;
  BMessage_delete (message);
}

static void
haiku_report_system_error (status_t code, const char *format)
{
  switch (code)
    {
    case B_BAD_VALUE:
      error (format, "Bad value");
      break;

    case B_ENTRY_NOT_FOUND:
      error (format, "File not found");
      break;

    case B_PERMISSION_DENIED:
      error (format, "Permission denied");
      break;

    case B_LINK_LIMIT:
      error (format, "Link limit reached");
      break;

    case B_BUSY:
      error (format, "Device busy");
      break;

    case B_NO_MORE_FDS:
      error (format, "No more file descriptors");
      break;

    case B_FILE_ERROR:
      error (format, "File error");
      break;

    case B_NO_MEMORY:
      memory_full (SIZE_MAX);
      break;

    default:
      error (format, "Unknown error");
      break;
    }
}

DEFUN ("haiku-drag-message", Fhaiku_drag_message, Shaiku_drag_message,
       2, 4, 0,
       doc: /* Begin dragging MESSAGE from FRAME.

MESSAGE an alist of strings, denoting message field names, to a list
the form (TYPE DATA ...), where TYPE is an integer denoting the system
data type of DATA, and DATA is in the general case a unibyte string.

If TYPE is a symbol instead of an integer, then DATA was specially
decoded.  If TYPE is `ref', then DATA is the absolute file name of a
file, or nil if decoding the file name failed.  If TYPE is `string',
then DATA is a unibyte string.  If TYPE is `short', then DATA is a
16-bit signed integer.  If TYPE is `long', then DATA is a 32-bit
signed integer.  If TYPE is `llong', then DATA is a 64-bit signed
integer. If TYPE is `byte' or `char', then DATA is an 8-bit signed
integer.  If TYPE is `bool', then DATA is a boolean.  If TYPE is
`size_t', then DATA is an integer that can hold between 0 and the
maximum value returned by the `sizeof' C operator on the current
system.  If TYPE is `ssize_t', then DATA is an integer that can hold
values from -1 to the maximum value of the C data type `ssize_t' on
the current system.  If TYPE is `point', then DATA is a cons of float
values describing the X and Y coordinates of an on-screen location.
If TYPE is `float', then DATA is a low-precision floating point
number, whose exact precision is not guaranteed.  If TYPE is `double',
then DATA is a floating point number that can represent any value a
Lisp float can represent.

If the field name is not a string but the symbol `type', then it
associates to a 32-bit unsigned integer describing the type of the
system message.

FRAME is a window system frame that must be visible, from which the
drag will originate.

ALLOW-SAME-FRAME, if nil or not specified, means that MESSAGE will be
ignored if it is dropped on top of FRAME.

FOLLOW-TOOLTIP, if non-nil, will cause any non-system tooltip
currently being displayed to move along with the mouse pointer.  */)
  (Lisp_Object frame, Lisp_Object message, Lisp_Object allow_same_frame,
   Lisp_Object follow_tooltip)
{
  specpdl_ref idx;
  void *be_message;
  struct frame *f;
  bool rc;

  idx = SPECPDL_INDEX ();
  f = decode_window_system_frame (frame);

  if (!FRAME_VISIBLE_P (f))
    error ("Frame is invisible");

  haiku_dnd_frame = f;
  haiku_dnd_follow_tooltip = !NILP (follow_tooltip);
  haiku_dnd_allow_same_frame = !NILP (allow_same_frame);

  be_message = be_create_simple_message ();

  record_unwind_protect_ptr (haiku_unwind_drag_message, be_message);
  haiku_lisp_to_message (message, be_message);

  rc = be_drag_message (FRAME_HAIKU_VIEW (f), be_message,
			!NILP (allow_same_frame),
			block_input, unblock_input,
			process_pending_signals,
			haiku_should_quit_drag);

  /* Don't clear the mouse grab if the user decided to quit instead
     of the drop finishing.  */
  if (rc)
    quit ();

  /* Now dismiss the tooltip, since the drop presumably succeeded.  */
  if (!NILP (follow_tooltip))
    Fx_hide_tip ();

  FRAME_DISPLAY_INFO (f)->grabbed = 0;

  return unbind_to (idx, Qnil);
}

DEFUN ("haiku-roster-launch", Fhaiku_roster_launch, Shaiku_roster_launch,
       2, 2, 0,
       doc: /* Launch an application associated with FILE-OR-TYPE.
Return the process ID of any process created, the symbol
`already-running' if ARGS was sent to a program that's already
running, or nil if launching the application failed because no
application was found for FILE-OR-TYPE.

Signal an error if FILE-OR-TYPE is invalid, or if ARGS is a message
but the application doesn't accept messages.

FILE-OR-TYPE can either be a string denoting a MIME type, or a list
with one argument FILE, denoting a file whose associated application
will be launched.

ARGS can either be a vector of strings containing the arguments that
will be passed to the application, or a system message in the form
accepted by `haiku-drag-message' that will be sent to the application
after it starts.  */)
  (Lisp_Object file_or_type, Lisp_Object args)
{
  char **cargs;
  char *type, *file;
  team_id team_id;
  status_t rc;
  ptrdiff_t i, nargs;
  Lisp_Object tem, canonical;
  void *message;
  specpdl_ref depth;

  type = NULL;
  file = NULL;
  cargs = NULL;
  message = NULL;
  nargs = 0;
  depth = SPECPDL_INDEX ();

  USE_SAFE_ALLOCA;

  if (STRINGP (file_or_type))
    SAFE_ALLOCA_STRING (type, file_or_type);
  else
    {
      CHECK_LIST (file_or_type);
      tem = XCAR (file_or_type);
      canonical = Fexpand_file_name (tem, Qnil);

      CHECK_STRING (tem);
      SAFE_ALLOCA_STRING (file, ENCODE_FILE (canonical));
      CHECK_LIST_END (XCDR (file_or_type), file_or_type);
    }

  if (VECTORP (args))
    {
      nargs = ASIZE (args);
      cargs = SAFE_ALLOCA (nargs * sizeof *cargs);

      for (i = 0; i < nargs; ++i)
	{
	  tem = AREF (args, i);
	  CHECK_STRING (tem);
	  maybe_quit ();

	  cargs[i] = SAFE_ALLOCA (SBYTES (tem) + 1);
	  memcpy (cargs[i], SDATA (tem), SBYTES (tem) + 1);
	}
    }
  else
    {
      message = be_create_simple_message ();

      record_unwind_protect_ptr (BMessage_delete, message);
      haiku_lisp_to_message (args, message);
    }

  block_input ();
  rc = be_roster_launch (type, file, cargs, nargs, message,
			 &team_id);
  unblock_input ();

  /* `be_roster_launch' can potentially take a while in IO, but
     signals from async input will interrupt that operation.  If the
     user wanted to quit, act like it.  */
  maybe_quit ();

  if (rc == B_OK)
    return SAFE_FREE_UNBIND_TO (depth,
				make_uint (team_id));
  else if (rc == B_ALREADY_RUNNING)
    return Qalready_running;
  else if (rc == B_BAD_VALUE)
    signal_error ("Invalid type or bad arguments",
		  list2 (file_or_type, args));

  return SAFE_FREE_UNBIND_TO (depth, Qnil);
}

DEFUN ("haiku-write-node-attribute", Fhaiku_write_node_attribute,
       Shaiku_write_node_attribute, 3, 3, 0,
       doc: /* Write a message as a file-system attribute of NODE.
FILE should be a file name of a file on a Be File System volume, NAME
should be a string describing the name of the attribute that will be
written, and MESSAGE will be the attribute written to FILE, as a
system message in the format accepted by `haiku-drag-message', which
see.  */)
  (Lisp_Object file, Lisp_Object name, Lisp_Object message)
{
  void *be_message;
  status_t rc;
  specpdl_ref count;

  CHECK_STRING (file);
  CHECK_STRING (name);

  file = ENCODE_FILE (file);
  name = ENCODE_SYSTEM (name);

  be_message = be_create_simple_message ();
  count = SPECPDL_INDEX ();

  record_unwind_protect_ptr (BMessage_delete, be_message);
  haiku_lisp_to_message (message, be_message);
  rc = be_write_node_message (SSDATA (file), SSDATA (name),
			      be_message);

  if (rc < B_OK)
    haiku_report_system_error (rc, "Failed to set attribute: %s");

  return unbind_to (count, Qnil);
}

DEFUN ("haiku-send-message", Fhaiku_send_message, Shaiku_send_message,
       2, 2, 0,
       doc: /* Send a system message to PROGRAM.
PROGRAM must be the name of the application to which the message will
be sent.  MESSAGE is the system message, serialized in the format
accepted by `haiku-drag-message', that will be sent to the application
specified by PROGRAM.  There is no guarantee that the message will
arrive after this function is called.  */)
  (Lisp_Object program, Lisp_Object message)
{
  specpdl_ref count;
  void *be_message;

  CHECK_STRING (program);
  program = ENCODE_SYSTEM (program);

  be_message = be_create_simple_message ();
  count = SPECPDL_INDEX ();

  record_unwind_protect_ptr (BMessage_delete, be_message);
  haiku_lisp_to_message (message, be_message);
  be_send_message (SSDATA (program), be_message);

  return unbind_to (count, Qnil);
}

static void
haiku_dnd_compute_tip_xy (int *root_x, int *root_y)
{
  int min_x, min_y, max_x, max_y;
  int width, height;

  width = FRAME_PIXEL_WIDTH (XFRAME (tip_frame));
  height = FRAME_PIXEL_HEIGHT (XFRAME (tip_frame));

  min_x = 0;
  min_y = 0;
  be_get_screen_dimensions (&max_x, &max_y);

  if (*root_y + XFIXNUM (tip_dy) <= min_y)
    *root_y = min_y; /* Can happen for negative dy */
  else if (*root_y + XFIXNUM (tip_dy) + height <= max_y)
    /* It fits below the pointer */
    *root_y += XFIXNUM (tip_dy);
  else if (height + XFIXNUM (tip_dy) + min_y <= *root_y)
    /* It fits above the pointer.  */
    *root_y -= height + XFIXNUM (tip_dy);
  else
    /* Put it on the top.  */
    *root_y = min_y;

  if (*root_x + XFIXNUM (tip_dx) <= min_x)
    *root_x = 0; /* Can happen for negative dx */
  else if (*root_x + XFIXNUM (tip_dx) + width <= max_x)
    /* It fits to the right of the pointer.  */
    *root_x += XFIXNUM (tip_dx);
  else if (width + XFIXNUM (tip_dx) + min_x <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XFIXNUM (tip_dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = min_x;
}

static Lisp_Object
haiku_note_drag_motion_1 (void *data)
{
  if (!NILP (Vhaiku_drag_track_function))
    return call0 (Vhaiku_drag_track_function);

  return Qnil;
}

static Lisp_Object
haiku_note_drag_motion_2 (enum nonlocal_exit exit, Lisp_Object error)
{
  return Qnil;
}

void
haiku_note_drag_motion (void)
{
  struct frame *tip_f;
  int x, y;

  if (FRAMEP (tip_frame) && haiku_dnd_follow_tooltip
      && FIXNUMP (tip_dx) && FIXNUMP (tip_dy))
    {
      tip_f = XFRAME (tip_frame);

      if (FRAME_LIVE_P (tip_f) && FRAME_VISIBLE_P (tip_f))
	{
	  BView_get_mouse (FRAME_HAIKU_VIEW (haiku_dnd_frame),
			   &x, &y);
	  BView_convert_to_screen (FRAME_HAIKU_VIEW (haiku_dnd_frame),
				   &x, &y);

	  haiku_dnd_compute_tip_xy (&x, &y);
	  BWindow_set_offset (FRAME_HAIKU_WINDOW (tip_f), x, y);
	}
    }

  internal_catch_all (haiku_note_drag_motion_1, NULL,
		      haiku_note_drag_motion_2);

  /* Redisplay this way to preserve the echo area.  Otherwise, the
     contents will abruptly disappear when the mouse moves over a
     frame.  */
  redisplay_preserve_echo_area (34);
}

void
haiku_note_drag_wheel (struct input_event *ie)
{
  bool horizontal, up;

  up = false;
  horizontal = false;

  if (ie->modifiers & up_modifier)
    up = true;

  if (ie->kind == HORIZ_WHEEL_EVENT)
    horizontal = true;

  ie->kind = NO_EVENT;

  if (!NILP (Vhaiku_drag_wheel_function)
      && (haiku_dnd_allow_same_frame
	  || XFRAME (ie->frame_or_window) != haiku_dnd_frame))
    safe_call (7, Vhaiku_drag_wheel_function, ie->frame_or_window,
	       ie->x, ie->y, horizontal ? Qt : Qnil, up ? Qt : Qnil,
	       make_int (ie->modifiers));

  redisplay_preserve_echo_area (35);
}

void
init_haiku_select (void)
{
  be_clipboard_init ();
}

void
haiku_handle_selection_clear (struct input_event *ie)
{
  enum haiku_clipboard id;

  id = haiku_get_clipboard_name (ie->arg);

  if (be_selection_outdated_p (id, ie->timestamp))
    return;

  CALLN (Frun_hook_with_args,
	 Qhaiku_lost_selection_functions, ie->arg);

  /* This is required for redisplay to happen if something changed the
     display inside the selection loss functions.  */
  redisplay_preserve_echo_area (20);
}

void
haiku_selection_disowned (enum haiku_clipboard id, int64 count)
{
  struct input_event ie;

  EVENT_INIT (ie);
  ie.kind = SELECTION_CLEAR_EVENT;

  switch (id)
    {
    case CLIPBOARD_CLIPBOARD:
      ie.arg = QCLIPBOARD;
      break;

    case CLIPBOARD_PRIMARY:
      ie.arg = QPRIMARY;
      break;

    case CLIPBOARD_SECONDARY:
      ie.arg = QSECONDARY;
      break;
    }

  ie.timestamp = count;
  kbd_buffer_store_event (&ie);
}

void
haiku_start_watching_selections (void)
{
  be_start_watching_selection (CLIPBOARD_CLIPBOARD);
  be_start_watching_selection (CLIPBOARD_PRIMARY);
  be_start_watching_selection (CLIPBOARD_SECONDARY);
}

void
syms_of_haikuselect (void)
{
  DEFVAR_BOOL ("haiku-signal-invalid-refs", haiku_signal_invalid_refs,
    doc: /* If nil, silently ignore invalid file names in system messages.
Otherwise, an error will be signaled if adding a file reference to a
system message failed.  */);
  haiku_signal_invalid_refs = true;

  DEFVAR_LISP ("haiku-drag-track-function", Vhaiku_drag_track_function,
    doc: /* If non-nil, a function to call upon mouse movement while dragging a message.
The function is called without any arguments.  `mouse-position' can be
used to retrieve the current position of the mouse.  */);
  Vhaiku_drag_track_function = Qnil;

  DEFVAR_LISP ("haiku-lost-selection-functions", Vhaiku_lost_selection_functions,
    doc: /* A list of functions to be called when Emacs loses an X selection.
These are only called if a connection to the Haiku display was opened.  */);
  Vhaiku_lost_selection_functions = Qnil;

  DEFVAR_LISP ("haiku-drag-wheel-function", Vhaiku_drag_wheel_function,
    doc: /* Function called upon wheel movement while dragging a message.
If non-nil, it is called with 6 arguments when the mouse wheel moves
while a drag-and-drop operation is in progress: the frame where the
mouse moved, the frame-relative X and Y positions where the mouse
moved, whether or not the wheel movement was horizontal, whether or
not the wheel moved up (or left, if the movement was horizontal), and
keyboard modifiers currently held down.  */);
  Vhaiku_drag_wheel_function = Qnil;

  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QSTRING, "STRING");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");
  DEFSYM (Qforeign_selection, "foreign-selection");
  DEFSYM (QTARGETS, "TARGETS");

  DEFSYM (Qhaiku_lost_selection_functions,
	  "haiku-lost-selection-functions");

  DEFSYM (Qmessage, "message");
  DEFSYM (Qstring, "string");
  DEFSYM (Qref, "ref");
  DEFSYM (Qshort, "short");
  DEFSYM (Qlong, "long");
  DEFSYM (Qllong, "llong");
  DEFSYM (Qbyte, "byte");
  DEFSYM (Qchar, "char");
  DEFSYM (Qbool, "bool");
  DEFSYM (Qtype, "type");
  DEFSYM (Qsize_t, "size_t");
  DEFSYM (Qssize_t, "ssize_t");
  DEFSYM (Qpoint, "point");
  DEFSYM (Qfloat, "float");
  DEFSYM (Qdouble, "double");
  DEFSYM (Qalready_running, "already-running");

  defsubr (&Shaiku_selection_data);
  defsubr (&Shaiku_selection_timestamp);
  defsubr (&Shaiku_selection_put);
  defsubr (&Shaiku_selection_owner_p);
  defsubr (&Shaiku_drag_message);
  defsubr (&Shaiku_roster_launch);
  defsubr (&Shaiku_write_node_attribute);
  defsubr (&Shaiku_send_message);

  haiku_dnd_frame = NULL;
}
