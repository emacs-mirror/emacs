/* Pure Gtk+-3 communication module.

Copyright (C) 1989, 1993-1994, 2005-2006, 2008-2024 Free Software
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

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>

#include "pgtkterm.h"

static void
im_context_commit_cb (GtkIMContext *imc,
		      gchar *str,
		      gpointer user_data)
{
  struct pgtk_display_info *dpyinfo = user_data;
  struct frame *f = dpyinfo->im.focused_frame;

  if (dpyinfo->im.context == NULL)
    return;
  if (f == NULL)
    return;

  pgtk_enqueue_string (f, str);
}

static gboolean
im_context_retrieve_surrounding_cb (GtkIMContext *imc, gpointer user_data)
{
  gtk_im_context_set_surrounding (imc, "", -1, 0);
  return TRUE;
}

static gboolean
im_context_delete_surrounding_cb (GtkIMContext *imc, int offset, int n_chars,
				  gpointer user_data)
{
  return TRUE;
}

static Lisp_Object
make_color_string (PangoAttrColor *pac)
{
  char buf[256];
  sprintf (buf, "#%02x%02x%02x",
	   pac->color.red >> 8, pac->color.green >> 8, pac->color.blue >> 8);
  return build_string (buf);
}

static void
im_context_preedit_changed_cb (GtkIMContext *imc, gpointer user_data)
{
  struct pgtk_display_info *dpyinfo = user_data;
  struct frame *f = dpyinfo->im.focused_frame;
  char *str;
  PangoAttrList *attrs;
  int pos;

  if (dpyinfo->im.context == NULL)
    return;
  if (f == NULL)
    return;

  gtk_im_context_get_preedit_string (imc, &str, &attrs, &pos);


  /*
   * (
   *   (TEXT (ul . COLOR) (bg . COLOR) (fg . COLOR))
   *   ...
   * )
   */
  Lisp_Object list = Qnil;

  PangoAttrIterator *iter;
  iter = pango_attr_list_get_iterator (attrs);
  do
    {
      int st, ed;
      int has_underline = 0;
      Lisp_Object part = Qnil;

      pango_attr_iterator_range (iter, &st, &ed);

      if (ed > strlen (str))
	ed = strlen (str);
      if (st >= ed)
	continue;

      Lisp_Object text = make_string (str + st, ed - st);
      part = Fcons (text, part);

      PangoAttrInt *ul =
	(PangoAttrInt *) pango_attr_iterator_get (iter, PANGO_ATTR_UNDERLINE);
      if (ul != NULL)
	{
	  if (ul->value != PANGO_UNDERLINE_NONE)
	    has_underline = 1;
	}

      PangoAttrColor *pac;
      if (has_underline)
	{
	  pac =
	    (PangoAttrColor *) pango_attr_iterator_get (iter,
							PANGO_ATTR_UNDERLINE_COLOR);
	  if (pac != NULL)
	    part = Fcons (Fcons (Qul, make_color_string (pac)), part);
	  else
	    part = Fcons (Fcons (Qul, Qt), part);
	}

      pac =
	(PangoAttrColor *) pango_attr_iterator_get (iter,
						    PANGO_ATTR_FOREGROUND);
      if (pac != NULL)
	part = Fcons (Fcons (Qfg, make_color_string (pac)), part);

      pac =
	(PangoAttrColor *) pango_attr_iterator_get (iter,
						    PANGO_ATTR_BACKGROUND);
      if (pac != NULL)
	part = Fcons (Fcons (Qbg, make_color_string (pac)), part);

      part = Fnreverse (part);
      list = Fcons (part, list);
    }
  while (pango_attr_iterator_next (iter));

  list = Fnreverse (list);
  pgtk_enqueue_preedit (f, list);

  g_free (str);
  pango_attr_list_unref (attrs);
}

static void
im_context_preedit_end_cb (GtkIMContext *imc, gpointer user_data)
{
  struct pgtk_display_info *dpyinfo = user_data;
  struct frame *f = dpyinfo->im.focused_frame;

  if (dpyinfo->im.context == NULL)
    return;
  if (f == NULL)
    return;

  pgtk_enqueue_preedit (f, Qnil);
}

static void
im_context_preedit_start_cb (GtkIMContext *imc, gpointer user_data)
{
}

void
pgtk_im_focus_in (struct frame *f)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  if (dpyinfo->im.context != NULL)
    {
      gtk_im_context_reset (dpyinfo->im.context);
      gtk_im_context_set_client_window (dpyinfo->im.context,
					gtk_widget_get_window
					(FRAME_GTK_WIDGET (f)));
      gtk_im_context_focus_in (dpyinfo->im.context);
    }
  dpyinfo->im.focused_frame = f;
}

void
pgtk_im_focus_out (struct frame *f)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  if (dpyinfo->im.focused_frame == f)
    {
      if (dpyinfo->im.context != NULL)
	{
	  gtk_im_context_reset (dpyinfo->im.context);
	  gtk_im_context_focus_out (dpyinfo->im.context);
	  gtk_im_context_set_client_window (dpyinfo->im.context, NULL);
	}
      dpyinfo->im.focused_frame = NULL;
    }
}

bool
pgtk_im_filter_keypress (struct frame *f, GdkEventKey * ev)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  if (dpyinfo->im.context != NULL)
    {
      if (gtk_im_context_filter_keypress (dpyinfo->im.context, ev))
	return true;
    }
  return false;
}

void
pgtk_im_set_cursor_location (struct frame *f, int x, int y, int width,
			     int height)
{
  struct pgtk_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  if (dpyinfo->im.context != NULL && dpyinfo->im.focused_frame == f)
    {
      GdkRectangle area = { x, y, width, height };
      gtk_im_context_set_cursor_location (dpyinfo->im.context, &area);
    }
}

static void
pgtk_im_use_context (struct pgtk_display_info *dpyinfo, bool use_p)
{
  if (!use_p)
    {
      if (dpyinfo->im.context != NULL)
	{
	  gtk_im_context_reset (dpyinfo->im.context);
	  gtk_im_context_focus_out (dpyinfo->im.context);
	  gtk_im_context_set_client_window (dpyinfo->im.context, NULL);

	  g_object_unref (dpyinfo->im.context);
	  dpyinfo->im.context = NULL;
	}
    }
  else
    {
      if (dpyinfo->im.context == NULL)
	{
	  dpyinfo->im.context = gtk_im_multicontext_new ();
	  g_signal_connect (dpyinfo->im.context, "commit",
			    G_CALLBACK (im_context_commit_cb), dpyinfo);
	  g_signal_connect (dpyinfo->im.context, "retrieve-surrounding",
			    G_CALLBACK (im_context_retrieve_surrounding_cb),
			    dpyinfo);
	  g_signal_connect (dpyinfo->im.context, "delete-surrounding",
			    G_CALLBACK (im_context_delete_surrounding_cb),
			    dpyinfo);
	  g_signal_connect (dpyinfo->im.context, "preedit-changed",
			    G_CALLBACK (im_context_preedit_changed_cb),
			    dpyinfo);
	  g_signal_connect (dpyinfo->im.context, "preedit-end",
			    G_CALLBACK (im_context_preedit_end_cb), dpyinfo);
	  g_signal_connect (dpyinfo->im.context, "preedit-start",
			    G_CALLBACK (im_context_preedit_start_cb),
			    dpyinfo);
	  gtk_im_context_set_use_preedit (dpyinfo->im.context, TRUE);

	  if (dpyinfo->im.focused_frame)
	    pgtk_im_focus_in (dpyinfo->im.focused_frame);
	}
    }
}

void
pgtk_im_init (struct pgtk_display_info *dpyinfo)
{
  dpyinfo->im.context = NULL;

  pgtk_im_use_context (dpyinfo, !NILP (Vpgtk_use_im_context_on_new_connection));
}

void
pgtk_im_finish (struct pgtk_display_info *dpyinfo)
{
  if (dpyinfo->im.context != NULL)
    g_object_unref (dpyinfo->im.context);
  dpyinfo->im.context = NULL;
}

DEFUN ("pgtk-use-im-context", Fpgtk_use_im_context, Spgtk_use_im_context, 1, 2, 0,
       doc: /* Set whether to use GtkIMContext. */)
  (Lisp_Object use_p, Lisp_Object terminal)
{
  struct pgtk_display_info *dpyinfo = check_pgtk_display_info (terminal);

  pgtk_im_use_context (dpyinfo, !NILP (use_p));

  return Qnil;
}

void
syms_of_pgtkim (void)
{
  defsubr (&Spgtk_use_im_context);

  DEFSYM (Qpgtk_refresh_preedit, "pgtk-refresh-preedit");
  DEFSYM (Qul, "ul");
  DEFSYM (Qfg, "fg");
  DEFSYM (Qbg, "bg");

  DEFVAR_LISP ("pgtk-use-im-context-on-new-connection", Vpgtk_use_im_context_on_new_connection,
	       doc: /* Whether to use GtkIMContext on a new connection.
If you want to change it after connection, use the `pgtk-use-im-context'
function.  */ );
  Vpgtk_use_im_context_on_new_connection = Qt;
}
