/* Gtk selection processing for emacs.
   Copyright (C) 1993-1994, 2005-2006, 2008-2022 Free Software
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

/* FIXME: this file needs a major rewrite to replace the use of GTK's
   own high-level GtkClipboard API with the GDK selection API:

   https://developer-old.gnome.org/gdk3/stable/gdk3-Selections.html

   That way, most of the code can be shared with X, and non-text
   targets along with drag-and-drop can be supported.  GDK implements
   selections according to the ICCCM, as on X, but its selection API
   will work on any supported window system.  */

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes.  */
#include <config.h>

#include "lisp.h"
#include "pgtkterm.h"
#include "termhooks.h"
#include "keyboard.h"
#include "pgtkselect.h"
#include <gdk/gdk.h>

static GQuark quark_primary_data = 0;
static GQuark quark_primary_size = 0;
static GQuark quark_secondary_data = 0;
static GQuark quark_secondary_size = 0;
static GQuark quark_clipboard_data = 0;
static GQuark quark_clipboard_size = 0;

/* ==========================================================================

    Internal utility functions

   ========================================================================== */

/* From a Lisp_Object, return a suitable frame for selection
   operations.  OBJECT may be a frame, a terminal object, or nil
   (which stands for the selected frame--or, if that is not an pgtk
   frame, the first pgtk display on the list).  If no suitable frame can
   be found, return NULL.  */

static struct frame *
frame_for_pgtk_selection (Lisp_Object object)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (NILP (object))
    {
      f = XFRAME (selected_frame);
      if (FRAME_PGTK_P (f) && FRAME_LIVE_P (f))
	return f;

      FOR_EACH_FRAME (tail, frame)
      {
	f = XFRAME (frame);
	if (FRAME_PGTK_P (f) && FRAME_LIVE_P (f))
	  return f;
      }
    }
  else if (TERMINALP (object))
    {
      struct terminal *t = decode_live_terminal (object);

      if (t->type == output_pgtk)
	FOR_EACH_FRAME (tail, frame)
	{
	  f = XFRAME (frame);
	  if (FRAME_LIVE_P (f) && f->terminal == t)
	    return f;
	}
    }
  else if (FRAMEP (object))
    {
      f = XFRAME (object);
      if (FRAME_PGTK_P (f) && FRAME_LIVE_P (f))
	return f;
    }

  return NULL;
}

static GtkClipboard *
symbol_to_gtk_clipboard (GtkWidget * widget, Lisp_Object symbol)
{
  GdkAtom atom;

  CHECK_SYMBOL (symbol);
  if (NILP (symbol))
    {
      atom = GDK_SELECTION_PRIMARY;
    }
  else if (EQ (symbol, QCLIPBOARD))
    {
      atom = GDK_SELECTION_CLIPBOARD;
    }
  else if (EQ (symbol, QPRIMARY))
    {
      atom = GDK_SELECTION_PRIMARY;
    }
  else if (EQ (symbol, QSECONDARY))
    {
      atom = GDK_SELECTION_SECONDARY;
    }
  else if (EQ (symbol, Qt))
    {
      atom = GDK_SELECTION_SECONDARY;
    }
  else
    {
      atom = 0;
      error ("Bad selection");
    }

  return gtk_widget_get_clipboard (widget, atom);
}

static void
selection_type_to_quarks (GdkAtom type, GQuark * quark_data,
			  GQuark * quark_size)
{
  if (type == GDK_SELECTION_PRIMARY)
    {
      *quark_data = quark_primary_data;
      *quark_size = quark_primary_size;
    }
  else if (type == GDK_SELECTION_SECONDARY)
    {
      *quark_data = quark_secondary_data;
      *quark_size = quark_secondary_size;
    }
  else if (type == GDK_SELECTION_CLIPBOARD)
    {
      *quark_data = quark_clipboard_data;
      *quark_size = quark_clipboard_size;
    }
  else
    /* FIXME: Is it safe to use 'error' here? */
    error ("Unknown selection type.");
}

static void
get_func (GtkClipboard * cb, GtkSelectionData * data, guint info,
	  gpointer user_data_or_owner)
{
  GObject *obj = G_OBJECT (user_data_or_owner);
  const char *str;
  int size;
  GQuark quark_data, quark_size;

  selection_type_to_quarks (gtk_clipboard_get_selection (cb), &quark_data,
			    &quark_size);

  str = g_object_get_qdata (obj, quark_data);
  size = GPOINTER_TO_SIZE (g_object_get_qdata (obj, quark_size));
  gtk_selection_data_set_text (data, str, size);
}

static void
clear_func (GtkClipboard * cb, gpointer user_data_or_owner)
{
  GObject *obj = G_OBJECT (user_data_or_owner);
  GQuark quark_data, quark_size;

  selection_type_to_quarks (gtk_clipboard_get_selection (cb), &quark_data,
			    &quark_size);

  g_object_set_qdata (obj, quark_data, NULL);
  g_object_set_qdata (obj, quark_size, 0);
}


/* ==========================================================================

    Functions used externally

   ========================================================================== */

void
pgtk_selection_init (void)
{
  if (quark_primary_data == 0)
    {
      quark_primary_data = g_quark_from_static_string ("pgtk-primary-data");
      quark_primary_size = g_quark_from_static_string ("pgtk-primary-size");
      quark_secondary_data =
	g_quark_from_static_string ("pgtk-secondary-data");
      quark_secondary_size =
	g_quark_from_static_string ("pgtk-secondary-size");
      quark_clipboard_data =
	g_quark_from_static_string ("pgtk-clipboard-data");
      quark_clipboard_size =
	g_quark_from_static_string ("pgtk-clipboard-size");
    }
}

void
pgtk_selection_lost (GtkWidget * widget, GdkEventSelection * event,
		     gpointer user_data)
{
  GQuark quark_data, quark_size;

  selection_type_to_quarks (event->selection, &quark_data, &quark_size);

  g_object_set_qdata (G_OBJECT (widget), quark_data, NULL);
  g_object_set_qdata (G_OBJECT (widget), quark_size, 0);
}

static bool
pgtk_selection_usable (void)
{
  if (pgtk_enable_selection_on_multi_display)
    return true;

  /* Gdk uses `gdk_display_get_default' when handling selections, so
     selections don't work properly when Emacs is connected to
     multiple displays.  */

  GdkDisplayManager *dpyman = gdk_display_manager_get ();
  GSList *list = gdk_display_manager_list_displays (dpyman);
  int len = g_slist_length (list);
  g_slist_free (list);
  return len < 2;
}

/* ==========================================================================

    Lisp Defuns

   ========================================================================== */


DEFUN ("pgtk-own-selection-internal", Fpgtk_own_selection_internal, Spgtk_own_selection_internal, 2, 3, 0,
       doc: /* Assert an X selection of type SELECTION and value VALUE.
SELECTION is a symbol, typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
VALUE is typically a string, or a cons of two markers, but may be
anything that the functions on `selection-converter-alist' know about.

FRAME should be a frame that should own the selection.  If omitted or
nil, it defaults to the selected frame. */)
  (Lisp_Object selection, Lisp_Object value, Lisp_Object frame)
{
  Lisp_Object successful_p = Qnil;
  Lisp_Object target_symbol, rest;
  GtkClipboard *cb;
  struct frame *f;
  GQuark quark_data, quark_size;

  check_window_system (NULL);

  if (!pgtk_selection_usable ())
    return Qnil;

  if (NILP (frame))
    frame = selected_frame;
  if (!FRAME_LIVE_P (XFRAME (frame)) || !FRAME_PGTK_P (XFRAME (frame)))
    error ("pgtk selection unavailable for this frame");
  f = XFRAME (frame);

  cb = symbol_to_gtk_clipboard (FRAME_GTK_WIDGET (f), selection);
  selection_type_to_quarks (gtk_clipboard_get_selection (cb), &quark_data,
			    &quark_size);

  /* We only support copy of text.  */
  target_symbol = QTEXT;
  if (STRINGP (value))
    {
      GtkTargetList *list;
      GtkTargetEntry *targets;
      gint n_targets;
      GtkWidget *widget;

      list = gtk_target_list_new (NULL, 0);
      gtk_target_list_add_text_targets (list, 0);

      {
	/* text/plain: Strings encoded by Gtk are not correctly decoded by Chromium(Wayland). */
	GdkAtom atom_text_plain = gdk_atom_intern ("text/plain", false);
	gtk_target_list_remove (list, atom_text_plain);
      }

      targets = gtk_target_table_new_from_list (list, &n_targets);

      int size = SBYTES (value);
      gchar *str = xmalloc (size + 1);
      memcpy (str, SSDATA (value), size);
      str[size] = '\0';

      widget = FRAME_GTK_WIDGET (f);
      g_object_set_qdata_full (G_OBJECT (widget), quark_data, str, xfree);
      g_object_set_qdata_full (G_OBJECT (widget), quark_size,
			       GSIZE_TO_POINTER (size), NULL);

      if (gtk_clipboard_set_with_owner (cb,
					targets, n_targets,
					get_func, clear_func,
					G_OBJECT (FRAME_GTK_WIDGET (f))))
	{
	  successful_p = Qt;
	}
      gtk_clipboard_set_can_store (cb, NULL, 0);

      gtk_target_table_free (targets, n_targets);
      gtk_target_list_unref (list);
    }

  if (!BASE_EQ (Vpgtk_sent_selection_hooks, Qunbound))
    {
      /* FIXME: Use run-hook-with-args!  */
      for (rest = Vpgtk_sent_selection_hooks; CONSP (rest);
	   rest = Fcdr (rest))
	call3 (Fcar (rest), selection, target_symbol, successful_p);
    }

  return value;
}


DEFUN ("pgtk-disown-selection-internal", Fpgtk_disown_selection_internal,
       Spgtk_disown_selection_internal, 1, 2, 0,
       doc: /* If we own the selection SELECTION, disown it.
Disowning it means there is no such selection.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  struct frame *f = frame_for_pgtk_selection (terminal);
  GtkClipboard *cb;

  if (!pgtk_selection_usable ())
    return Qnil;

  if (!f)
    return Qnil;

  cb = symbol_to_gtk_clipboard (FRAME_GTK_WIDGET (f), selection);

  gtk_clipboard_clear (cb);

  return Qt;
}


DEFUN ("pgtk-selection-exists-p", Fpgtk_selection_exists_p, Spgtk_selection_exists_p, 0, 2, 0,
       doc: /* Whether there is an owner for the given X selection.
SELECTION should be the name of the selection in question, typically
one of the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.  (X expects
these literal upper-case names.)  The symbol nil is the same as
`PRIMARY', and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  struct frame *f = frame_for_pgtk_selection (terminal);
  GtkClipboard *cb;

  if (!pgtk_selection_usable ())
    return Qnil;

  if (!f)
    return Qnil;

  cb = symbol_to_gtk_clipboard (FRAME_GTK_WIDGET (f), selection);

  return gtk_clipboard_wait_is_text_available (cb) ? Qt : Qnil;
}


DEFUN ("pgtk-selection-owner-p", Fpgtk_selection_owner_p, Spgtk_selection_owner_p, 0, 2, 0,
       doc: /* Whether the current Emacs process owns the given X Selection.
The arg should be the name of the selection in question, typically one of
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
For convenience, the symbol nil is the same as `PRIMARY',
and t is the same as `SECONDARY'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available X display.

On Nextstep, TERMINAL is unused.  */)
  (Lisp_Object selection, Lisp_Object terminal)
{
  struct frame *f = frame_for_pgtk_selection (terminal);
  GtkClipboard *cb;
  GObject *obj;
  GQuark quark_data, quark_size;

  if (!pgtk_selection_usable ())
    return Qnil;

  cb = symbol_to_gtk_clipboard (FRAME_GTK_WIDGET (f), selection);
  selection_type_to_quarks (gtk_clipboard_get_selection (cb), &quark_data,
			    &quark_size);

  obj = gtk_clipboard_get_owner (cb);

  return obj && g_object_get_qdata (obj, quark_data) != NULL ? Qt : Qnil;
}


DEFUN ("pgtk-get-selection-internal", Fpgtk_get_selection_internal,
       Spgtk_get_selection_internal, 2, 3, 0,
       doc: /* Return text selected from some program.
SELECTION-SYMBOL is typically `PRIMARY', `SECONDARY', or `CLIPBOARD'.
\(Those are literal upper-case symbol names, since that's what X expects.)
TARGET-TYPE is the type of data desired, typically `STRING'.

TERMINAL should be a terminal object or a frame specifying the X
server to query.  If omitted or nil, that stands for the selected
frame's display, or the first available display.  */)
  (Lisp_Object selection_symbol, Lisp_Object target_type,
   Lisp_Object terminal)
{
  struct frame *f = frame_for_pgtk_selection (terminal);
  GtkClipboard *cb;

  CHECK_SYMBOL (selection_symbol);
  CHECK_SYMBOL (target_type);

  if (EQ (target_type, QMULTIPLE))
    error ("Retrieving MULTIPLE selections is currently unimplemented");
  if (!f)
    error ("PGTK selection unavailable for this frame");

  if (!pgtk_selection_usable ())
    return Qnil;

  cb = symbol_to_gtk_clipboard (FRAME_GTK_WIDGET (f), selection_symbol);

  GdkAtom target_atom = gdk_atom_intern (SSDATA (SYMBOL_NAME (target_type)), false);
  GtkSelectionData *seldata = gtk_clipboard_wait_for_contents (cb, target_atom);

  if (seldata == NULL)
    return Qnil;

  const guchar *sd_data = gtk_selection_data_get_data (seldata);
  int sd_len = gtk_selection_data_get_length (seldata);
  int sd_format = gtk_selection_data_get_format (seldata);
  GdkAtom sd_type = gtk_selection_data_get_data_type (seldata);

  if (sd_format == 8)
    {
      Lisp_Object str, lispy_type;

      str = make_unibyte_string ((char *) sd_data, sd_len);
      /* Indicate that this string is from foreign selection by a text
	 property `foreign-selection' so that the caller of
	 x-get-selection-internal (usually x-get-selection) can know
	 that the string must be decode.  */
      if (sd_type == gdk_atom_intern ("COMPOUND_TEXT", false))
	lispy_type = QCOMPOUND_TEXT;
      else if (sd_type == gdk_atom_intern ("UTF8_STRING", false))
	lispy_type = QUTF8_STRING;
      else if (sd_type == gdk_atom_intern ("text/plain;charset=utf-8", false))
	lispy_type = Qtext_plain_charset_utf_8;
      else
	lispy_type = QSTRING;
      Fput_text_property (make_fixnum (0), make_fixnum (sd_len),
			  Qforeign_selection, lispy_type, str);

      gtk_selection_data_free (seldata);
      return str;
    }

  gtk_selection_data_free (seldata);
  return Qnil;
}

void
syms_of_pgtkselect (void)
{
  DEFSYM (QCLIPBOARD, "CLIPBOARD");
  DEFSYM (QSECONDARY, "SECONDARY");
  DEFSYM (QTEXT, "TEXT");
  DEFSYM (QFILE_NAME, "FILE_NAME");
  DEFSYM (QMULTIPLE, "MULTIPLE");

  DEFSYM (Qforeign_selection, "foreign-selection");
  DEFSYM (QUTF8_STRING, "UTF8_STRING");
  DEFSYM (QSTRING, "STRING");
  DEFSYM (QCOMPOUND_TEXT, "COMPOUND_TEXT");
  DEFSYM (Qtext_plain_charset_utf_8, "text/plain;charset=utf-8");

  defsubr (&Spgtk_disown_selection_internal);
  defsubr (&Spgtk_get_selection_internal);
  defsubr (&Spgtk_own_selection_internal);
  defsubr (&Spgtk_selection_exists_p);
  defsubr (&Spgtk_selection_owner_p);

  DEFVAR_LISP ("pgtk-sent-selection-hooks", Vpgtk_sent_selection_hooks,
	       doc: /* A list of functions to be called when Emacs answers a selection request
The functions are called with four arguments:
  - the selection name (typically `PRIMARY', `SECONDARY', or `CLIPBOARD');
  - the selection-type which Emacs was asked to convert the
    selection into before sending (for example, `STRING' or `LENGTH');
  - a flag indicating success or failure for responding to the request.
We might have failed (and declined the request) for any number of reasons,
including being asked for a selection that we no longer own, or being asked
to convert into a type that we don't know about or that is inappropriate.
This hook doesn't let you change the behavior of Emacs's selection replies,
it merely informs you that they have happened.  */);
  Vpgtk_sent_selection_hooks = Qnil;

  DEFVAR_BOOL ("pgtk-enable-selection-on-multi-display", pgtk_enable_selection_on_multi_display,
	       doc: /* Enable selections when connected to multiple displays.
This may cause crashes due to a GTK bug, which assumes that clients
will connect to a single display.  It might also cause selections to
not arrive at the correct display.  */);
  pgtk_enable_selection_on_multi_display = false;
}
