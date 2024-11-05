/* Functions for handling font and other changes dynamically.

Copyright (C) 2009-2024 Free Software Foundation, Inc.

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

#include <float.h>
#include <limits.h>
#include <fcntl.h>

#include <byteswap.h>

#include "lisp.h"
#ifndef HAVE_PGTK
#include "xterm.h"
#else
#include "gtkutil.h"
#endif
#include "xsettings.h"
#include "frame.h"
#include "keyboard.h"
#include "blockinput.h"
#include "termhooks.h"
#include "pdumper.h"

#ifndef HAVE_PGTK
#include <X11/Xproto.h>
#else
typedef unsigned short CARD16;
typedef unsigned int CARD32;
#endif

#ifdef HAVE_GSETTINGS
#include <glib-object.h>
#include <gio/gio.h>
#endif

#ifdef HAVE_GCONF
#include <gconf/gconf-client.h>
#endif

#ifdef USE_CAIRO
#include <fontconfig/fontconfig.h>
#include "ftfont.h"
#elif defined HAVE_XFT
#include <X11/Xft/Xft.h>
#endif

#if defined USE_CAIRO && defined CAIRO_HAS_FT_FONT
#include <cairo/cairo-ft.h>
#endif

static char *current_mono_font;
static char *current_font;
static Display_Info *first_dpyinfo;
static Lisp_Object current_tool_bar_style;

/* Store a config changed event in to the event queue.  */

static void
store_config_changed_event (Lisp_Object arg, Lisp_Object display_name)
{
  struct input_event event;
  EVENT_INIT (event);
  event.kind = CONFIG_CHANGED_EVENT;
  event.frame_or_window = display_name;
  event.arg = arg;
  kbd_buffer_store_event (&event);
}

/* Return true if DPYINFO is still valid.  */
static bool
dpyinfo_valid (Display_Info *dpyinfo)
{
  bool found = false;
  if (dpyinfo != NULL)
    {
      Display_Info *d;
      for (d = x_display_list; !found && d; d = d->next)
#ifndef HAVE_PGTK
        found = d == dpyinfo && d->display == dpyinfo->display;
#else
        found = d == dpyinfo && d->gdpy == dpyinfo->gdpy;
#endif
    }
  return found;
}

/* Store a monospace font change event if the monospaced font changed.  */

#if (defined USE_CAIRO || defined HAVE_XFT) && (defined HAVE_GSETTINGS || defined HAVE_GCONF)
static void
store_monospaced_changed (const char *newfont)
{
  if (current_mono_font != NULL && strcmp (newfont, current_mono_font) == 0)
    return; /* No change. */

  dupstring (&current_mono_font, newfont);

  if (dpyinfo_valid (first_dpyinfo) && use_system_font)
    {
      store_config_changed_event (Qmonospace_font_name,
                                  XCAR (first_dpyinfo->name_list_element));
    }
}
#endif

/* Store a font name change event if the font name changed.  */

#if defined USE_CAIRO || defined HAVE_XFT
static void
store_font_name_changed (const char *newfont)
{
  if (current_font != NULL && strcmp (newfont, current_font) == 0)
    return; /* No change. */

  dupstring (&current_font, newfont);

  if (dpyinfo_valid (first_dpyinfo))
    {
      store_config_changed_event (Qfont_name,
                                  XCAR (first_dpyinfo->name_list_element));
    }
}
#endif /* USE_CAIRO || HAVE_XFT */

/* Map TOOL_BAR_STYLE from a string to its corresponding Lisp value.
   Return Qnil if TOOL_BAR_STYLE is not known.  */

static Lisp_Object
map_tool_bar_style (const char *tool_bar_style)
{
  Lisp_Object style = Qnil;
  if (tool_bar_style)
    {
      if (strcmp (tool_bar_style, "both") == 0)
        style = Qboth;
      else if (strcmp (tool_bar_style, "both-horiz") == 0)
        style = Qboth_horiz;
      else if (strcmp (tool_bar_style, "icons") == 0)
        style = Qimage;
      else if (strcmp (tool_bar_style, "text") == 0)
        style = Qtext;
    }

  return style;
}

/* Store a tool bar style change event if the tool bar style changed.  */

static void
store_tool_bar_style_changed (const char *newstyle,
                              Display_Info *dpyinfo)
{
  Lisp_Object style = map_tool_bar_style (newstyle);
  if (EQ (current_tool_bar_style, style))
    return; /* No change. */

  current_tool_bar_style = style;
  if (dpyinfo_valid (dpyinfo))
    store_config_changed_event (Qtool_bar_style,
                                XCAR (dpyinfo->name_list_element));
}

#ifndef HAVE_PGTK
#if defined USE_CAIRO || defined HAVE_XFT
#define XSETTINGS_FONT_NAME       "Gtk/FontName"
#endif
#define XSETTINGS_TOOL_BAR_STYLE  "Gtk/ToolbarStyle"
#endif

enum {
  SEEN_AA         = 0x01,
  SEEN_HINTING    = 0x02,
  SEEN_RGBA       = 0x04,
  SEEN_LCDFILTER  = 0x08,
  SEEN_HINTSTYLE  = 0x10,
  SEEN_DPI        = 0x20,
  SEEN_FONT       = 0x40,
  SEEN_TB_STYLE   = 0x80
};
struct xsettings
{
#if defined USE_CAIRO || defined HAVE_XFT
  FcBool aa, hinting;
  int rgba, lcdfilter, hintstyle;
  double dpi;

  char *font;
#endif

  char *tb_style;

  unsigned seen;
};

#ifdef HAVE_PGTK
/* The cairo font_options as obtained using gsettings.  */
static cairo_font_options_t *font_options;
#endif

#ifdef HAVE_GSETTINGS
#define GSETTINGS_SCHEMA         "org.gnome.desktop.interface"
#define GSETTINGS_TOOL_BAR_STYLE "toolbar-style"

#if defined USE_CAIRO || defined HAVE_XFT
#define GSETTINGS_MONO_FONT  "monospace-font-name"
#define GSETTINGS_FONT_NAME  "font-name"
#endif

#ifdef HAVE_PGTK
#define GSETTINGS_FONT_ANTIALIASING  "font-antialiasing"
#define GSETTINGS_FONT_RGBA_ORDER    "font-rgba-order"
#define GSETTINGS_FONT_HINTING       "font-hinting"
#endif

/* The single GSettings instance, or NULL if not connected to GSettings.  */

static GSettings *gsettings_client;

#if defined HAVE_PGTK && defined HAVE_GSETTINGS

static bool
xg_settings_key_valid_p (GSettings *settings, const char *key)
{
#ifdef GLIB_VERSION_2_32
  GSettingsSchema *schema;
  bool rc;

  g_object_get (G_OBJECT (settings),
		"settings-schema", &schema,
		NULL);

  if (!schema)
    return false;

  rc = g_settings_schema_has_key (schema, key);
  g_settings_schema_unref (schema);

  return rc;
#else
  return false;
#endif
}

#endif

#ifdef HAVE_PGTK
/* Store an event for re-rendering of the fonts.  */
static void
store_font_options_changed (void)
{
  if (dpyinfo_valid (first_dpyinfo))
    store_config_changed_event (Qfont_render,
				XCAR (first_dpyinfo->name_list_element));
}

/* Apply changes in the hinting system setting.  */
static void
apply_gsettings_font_hinting (GSettings *settings)
{
  GVariant *val;
  const char *hinting;

  if (!xg_settings_key_valid_p (settings, GSETTINGS_FONT_HINTING))
    return;

  val = g_settings_get_value (settings, GSETTINGS_FONT_HINTING);

  if (val)
    {
      g_variant_ref_sink (val);

      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
	{
	  hinting = g_variant_get_string (val, NULL);

	  if (!strcmp (hinting, "full"))
	    cairo_font_options_set_hint_style (font_options,
					       CAIRO_HINT_STYLE_FULL);
	  else if (!strcmp (hinting, "medium"))
	    cairo_font_options_set_hint_style (font_options,
					       CAIRO_HINT_STYLE_MEDIUM);
	  else if (!strcmp (hinting, "slight"))
	    cairo_font_options_set_hint_style (font_options,
					       CAIRO_HINT_STYLE_SLIGHT);
	  else if (!strcmp (hinting, "none"))
	    cairo_font_options_set_hint_style (font_options,
					       CAIRO_HINT_STYLE_NONE);
	}
      g_variant_unref (val);
    }
}

/* Apply changes in the antialiasing system setting.  */
static void
apply_gsettings_font_antialias (GSettings *settings)
{
  GVariant *val;
  const char *antialias;

  if (!xg_settings_key_valid_p (settings, GSETTINGS_FONT_ANTIALIASING))
    return;

  val = g_settings_get_value (settings, GSETTINGS_FONT_ANTIALIASING);

  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
	{
	  antialias = g_variant_get_string (val, NULL);

	  if (!strcmp (antialias, "none"))
	    cairo_font_options_set_antialias (font_options,
					      CAIRO_ANTIALIAS_NONE);
	  else if (!strcmp (antialias, "grayscale"))
	    cairo_font_options_set_antialias (font_options,
					      CAIRO_ANTIALIAS_GRAY);
	  else if (!strcmp (antialias, "rgba"))
	    cairo_font_options_set_antialias (font_options,
					      CAIRO_ANTIALIAS_SUBPIXEL);
	}
      g_variant_unref (val);
    }
}

/* Apply the settings for the rgb element ordering.  */
static void
apply_gsettings_font_rgba_order (GSettings *settings)
{
  GVariant *val;
  const char *rgba_order;

  if (!xg_settings_key_valid_p (settings, GSETTINGS_FONT_RGBA_ORDER))
    return;

  val = g_settings_get_value (settings,
			      GSETTINGS_FONT_RGBA_ORDER);

  if (val)
    {
      g_variant_ref_sink (val);

      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
	{
	  rgba_order = g_variant_get_string (val, NULL);

	  if (!strcmp (rgba_order, "rgb"))
	    cairo_font_options_set_subpixel_order (font_options,
						   CAIRO_SUBPIXEL_ORDER_RGB);
	  else if (!strcmp (rgba_order, "bgr"))
	    cairo_font_options_set_subpixel_order (font_options,
						   CAIRO_SUBPIXEL_ORDER_BGR);
	  else if (!strcmp (rgba_order, "vrgb"))
	    cairo_font_options_set_subpixel_order (font_options,
						   CAIRO_SUBPIXEL_ORDER_VRGB);
	  else if (!strcmp (rgba_order, "vbgr"))
	    cairo_font_options_set_subpixel_order (font_options,
						   CAIRO_SUBPIXEL_ORDER_VBGR);
	}
      g_variant_unref (val);
    }
}
#endif /* HAVE_PGTK */

/* Callback called when something changed in GSettings.  */

static void
something_changed_gsettingsCB (GSettings *settings,
                               gchar *key,
                               gpointer user_data)
{
  GVariant *val;

  if (strcmp (key, GSETTINGS_TOOL_BAR_STYLE) == 0)
    {
      val = g_settings_get_value (settings, GSETTINGS_TOOL_BAR_STYLE);
      if (val)
        {
          g_variant_ref_sink (val);
          if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
            {
              const gchar *newstyle = g_variant_get_string (val, NULL);
              store_tool_bar_style_changed (newstyle, first_dpyinfo);
            }
          g_variant_unref (val);
        }
    }
#if defined USE_CAIRO || defined HAVE_XFT
  else if (strcmp (key, GSETTINGS_MONO_FONT) == 0)
    {
      val = g_settings_get_value (settings, GSETTINGS_MONO_FONT);
      if (val)
        {
          g_variant_ref_sink (val);
          if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
            {
              const gchar *newfont = g_variant_get_string (val, NULL);
              store_monospaced_changed (newfont);
            }
          g_variant_unref (val);
        }
    }
  else if (strcmp (key, GSETTINGS_FONT_NAME) == 0)
    {
      val = g_settings_get_value (settings, GSETTINGS_FONT_NAME);
      if (val)
        {
          g_variant_ref_sink (val);
          if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
            {
              const gchar *newfont = g_variant_get_string (val, NULL);
              store_font_name_changed (newfont);
            }
          g_variant_unref (val);
        }
    }
#endif /* USE_CAIRO || HAVE_XFT */
#ifdef HAVE_PGTK
  else if (!strcmp (key, GSETTINGS_FONT_ANTIALIASING))
    {
      apply_gsettings_font_antialias (settings);
      store_font_options_changed ();
    }
  else if (!strcmp (key, GSETTINGS_FONT_HINTING))
    {
      apply_gsettings_font_hinting (settings);
      store_font_options_changed ();
    }
  else if (!strcmp (key, GSETTINGS_FONT_RGBA_ORDER))
    {
      apply_gsettings_font_rgba_order (settings);
      store_font_options_changed ();
    }
#endif /* HAVE_PGTK */
}

#endif /* HAVE_GSETTINGS */

#ifdef HAVE_GCONF
#define GCONF_TOOL_BAR_STYLE "/desktop/gnome/interface/toolbar_style"
#if defined USE_CAIRO || defined HAVE_XFT
#define GCONF_MONO_FONT  "/desktop/gnome/interface/monospace_font_name"
#define GCONF_FONT_NAME  "/desktop/gnome/interface/font_name"
#endif

/* The single GConf instance, or NULL if not connected to GConf.  */

static GConfClient *gconf_client;

/* Callback called when something changed in GConf that we care about.  */

static void
something_changed_gconfCB (GConfClient *client,
                           guint cnxn_id,
                           GConfEntry *entry,
                           gpointer user_data)
{
  GConfValue *v = gconf_entry_get_value (entry);
  const char *key = gconf_entry_get_key (entry);

  if (!v || v->type != GCONF_VALUE_STRING || ! key) return;
  if (strcmp (key, GCONF_TOOL_BAR_STYLE) == 0)
    {
      const char *value = gconf_value_get_string (v);
      store_tool_bar_style_changed (value, first_dpyinfo);
    }
#if defined USE_CAIRO || defined HAVE_XFT
  else if (strcmp (key, GCONF_MONO_FONT) == 0)
    {
      const char *value = gconf_value_get_string (v);
      store_monospaced_changed (value);
    }
  else if (strcmp (key, GCONF_FONT_NAME) == 0)
    {
      const char *value = gconf_value_get_string (v);
      store_font_name_changed (value);
    }
#endif /* USE_CAIRO || HAVE_XFT */
}

#endif /* HAVE_GCONF */

#if defined USE_CAIRO || defined HAVE_XFT

/* Older fontconfig versions don't have FC_LCD_*.  */
#ifndef FC_LCD_NONE
#define FC_LCD_NONE 0
#endif
#ifndef FC_LCD_DEFAULT
#define FC_LCD_DEFAULT 1
#endif
#ifndef FC_LCD_FILTER
#define FC_LCD_FILTER "lcdfilter"
#endif

#endif /* USE_CAIRO || HAVE_XFT */

#ifndef HAVE_PGTK
/* Find the window that contains the XSETTINGS property values.  */

static void
get_prop_window (Display_Info *dpyinfo)
{
  Display *dpy = dpyinfo->display;

  XGrabServer (dpy);
  dpyinfo->xsettings_window = XGetSelectionOwner (dpy,
                                                  dpyinfo->Xatom_xsettings_sel);
  if (dpyinfo->xsettings_window != None)
    /* Select events so we can detect if window is deleted or if settings
       are changed.  */
    XSelectInput (dpy, dpyinfo->xsettings_window,
                  PropertyChangeMask|StructureNotifyMask);

  XUngrabServer (dpy);
}
#endif

#ifndef HAVE_PGTK

#define PAD(nr)    (((nr) + 3) & ~3)

/* Parse xsettings and extract those that deal with Xft.
   See https://freedesktop.org/wiki/Specifications/XSettingsRegistry/
   and https://specifications.freedesktop.org/xsettings-spec/xsettings-spec-0.5.html.

   Layout of prop.  First is a header:

   bytes   type     what
   ------------------------------------
   1      CARD8    byte-order
   3               unused
   4      CARD32   SERIAL
   4      CARD32   N_SETTINGS

   Then N_SETTINGS records, with header:

   bytes   type          what
   ------------------------------------
   1      SETTING_TYPE  type (0 = integer, 1 = string, 2 RGB color).
   1                    unused
   2      CARD16        n == name-length
   n      STRING8       name
   p                    unused, p=pad_to_even_4(n)
   4      CARD32        last-change-serial

   and then the value, For string:

   bytes   type          what
   ------------------------------------
   4      CARD32        n = value-length
   n      STRING8       value
   p                    unused, p=pad_to_even_4(n)

   For integer:

   bytes   type          what
   ------------------------------------
   4      INT32         value

   For RGB color:

   bytes   type          what
   ------------------------------------
   2      CARD16        red
   2      CARD16        blue
   2      CARD16        green
   2      CARD16        alpha

   Returns non-zero if some Xft settings was seen, zero otherwise.
*/

static int
parse_settings (unsigned char *prop,
                unsigned long bytes,
                struct xsettings *settings)
{
  int int1 = 1;
  int my_bo = *(char *) &int1 == 1 ? LSBFirst : MSBFirst;
  int that_bo = prop[0];
  CARD32 n_settings;
  int bytes_parsed = 0;
  int settings_seen = 0;
  int i = 0;

  /* First 4 bytes is a serial number, skip that.  */

  if (bytes < 12) return settings_seen;
  memcpy (&n_settings, prop+8, 4);
  if (my_bo != that_bo) n_settings = bswap_32 (n_settings);
  bytes_parsed = 12;

  memset (settings, 0, sizeof (*settings));

  while (bytes_parsed+4 < bytes && settings_seen < 7
         && i < n_settings)
    {
      int type = prop[bytes_parsed++];
      CARD16 nlen;
      CARD32 vlen, ival = 0;
      char name[128]; /* The names we are looking for are not this long.  */
      char sval[128]; /* The values we are looking for are not this long.  */
      bool want_this;
      int to_cpy;

      sval[0] = '\0';
      ++i;
      ++bytes_parsed; /* Padding */

      memcpy (&nlen, prop+bytes_parsed, 2);
      bytes_parsed += 2;
      if (my_bo != that_bo) nlen = bswap_16 (nlen);
      if (bytes_parsed + nlen > bytes) return settings_seen;
      to_cpy = min (nlen, sizeof name - 1);
      memcpy (name, prop+bytes_parsed, to_cpy);
      name[to_cpy] = '\0';

      bytes_parsed += nlen;
      bytes_parsed = PAD (bytes_parsed);

      bytes_parsed += 4; /* Skip serial for this value */
      if (bytes_parsed > bytes) return settings_seen;

      want_this = strcmp (XSETTINGS_TOOL_BAR_STYLE, name) == 0;
#if defined USE_CAIRO || defined HAVE_XFT
      if ((nlen > 6 && memcmp (name, "Xft/", 4) == 0)
	  || strcmp (XSETTINGS_FONT_NAME, name) == 0)
	want_this = true;
#endif

      switch (type)
        {
        case 0: /* Integer */
          if (bytes_parsed + 4 > bytes) return settings_seen;
          if (want_this)
            {
              memcpy (&ival, prop+bytes_parsed, 4);
              if (my_bo != that_bo) ival = bswap_32 (ival);
            }
          bytes_parsed += 4;
          break;

        case 1: /* String */
          if (bytes_parsed + 4 > bytes) return settings_seen;
          memcpy (&vlen, prop+bytes_parsed, 4);
          bytes_parsed += 4;
          if (my_bo != that_bo) vlen = bswap_32 (vlen);
          if (want_this)
            {
              to_cpy = min (vlen, sizeof sval - 1);
              memcpy (sval, prop+bytes_parsed, to_cpy);
              sval[to_cpy] = '\0';
            }
          bytes_parsed += vlen;
          bytes_parsed = PAD (bytes_parsed);
          break;

        case 2: /* RGB value */
          /* No need to parse this */
          if (bytes_parsed + 8 > bytes) return settings_seen;
          bytes_parsed += 8; /* 4 values (r, b, g, alpha), 2 bytes each.  */
          break;

        default: /* Parse Error */
          return settings_seen;
        }

      if (want_this)
        {
          if (strcmp (name, XSETTINGS_TOOL_BAR_STYLE) == 0)
            {
              dupstring (&settings->tb_style, sval);
              settings->seen |= SEEN_TB_STYLE;
            }
#if defined USE_CAIRO || defined HAVE_XFT
          else if (strcmp (name, XSETTINGS_FONT_NAME) == 0)
            {
              dupstring (&settings->font, sval);
              settings->seen |= SEEN_FONT;
            }
          else if (strcmp (name, "Xft/Antialias") == 0)
            {
              settings->seen |= SEEN_AA;
              settings->aa = ival != 0;
            }
          else if (strcmp (name, "Xft/Hinting") == 0)
            {
              settings->seen |= SEEN_HINTING;
              settings->hinting = ival != 0;
            }
# ifdef FC_HINT_STYLE
          else if (strcmp (name, "Xft/HintStyle") == 0)
            {
              settings->seen |= SEEN_HINTSTYLE;
              if (strcmp (sval, "hintnone") == 0)
                settings->hintstyle = FC_HINT_NONE;
              else if (strcmp (sval, "hintslight") == 0)
                settings->hintstyle = FC_HINT_SLIGHT;
              else if (strcmp (sval, "hintmedium") == 0)
                settings->hintstyle = FC_HINT_MEDIUM;
              else if (strcmp (sval, "hintfull") == 0)
                settings->hintstyle = FC_HINT_FULL;
              else
                settings->seen &= ~SEEN_HINTSTYLE;
            }
# endif
          else if (strcmp (name, "Xft/RGBA") == 0)
            {
              settings->seen |= SEEN_RGBA;
              if (strcmp (sval, "none") == 0)
                settings->rgba = FC_RGBA_NONE;
              else if (strcmp (sval, "rgb") == 0)
                settings->rgba = FC_RGBA_RGB;
              else if (strcmp (sval, "bgr") == 0)
                settings->rgba = FC_RGBA_BGR;
              else if (strcmp (sval, "vrgb") == 0)
                settings->rgba = FC_RGBA_VRGB;
              else if (strcmp (sval, "vbgr") == 0)
                settings->rgba = FC_RGBA_VBGR;
              else
                settings->seen &= ~SEEN_RGBA;
            }
          else if (strcmp (name, "Xft/DPI") == 0 && ival != (CARD32) -1)
            {
              settings->seen |= SEEN_DPI;
              settings->dpi = ival / 1024.0;
            }
          else if (strcmp (name, "Xft/lcdfilter") == 0)
            {
              settings->seen |= SEEN_LCDFILTER;
              if (strcmp (sval, "none") == 0)
                settings->lcdfilter = FC_LCD_NONE;
              else if (strcmp (sval, "lcddefault") == 0)
                settings->lcdfilter = FC_LCD_DEFAULT;
              else
                settings->seen &= ~SEEN_LCDFILTER;
            }
#endif /* USE_CAIRO || HAVE_XFT */
	  else
	    want_this = false;
	  settings_seen += want_this;
        }
    }

  return settings_seen;
}
#endif

#ifndef HAVE_PGTK
/* Read settings from the XSettings property window on display for DPYINFO.
   Store settings read in SETTINGS.
   Return true if successful.  */

static bool
read_settings (Display_Info *dpyinfo, struct xsettings *settings)
{
  Atom act_type;
  int act_form;
  unsigned long nitems, bytes_after;
  unsigned char *prop = NULL;
  Display *dpy = dpyinfo->display;
  int rc;
  bool got_settings = false;

  x_catch_errors (dpy);
  rc = XGetWindowProperty (dpy,
                           dpyinfo->xsettings_window,
                           dpyinfo->Xatom_xsettings_prop,
                           0, LONG_MAX, False, AnyPropertyType,
                           &act_type, &act_form, &nitems, &bytes_after,
                           &prop);

  if (rc == Success && prop != NULL && act_form == 8 && nitems > 0
      && act_type == dpyinfo->Xatom_xsettings_prop)
    got_settings = parse_settings (prop, nitems, settings) != 0;

  XFree (prop);

  x_uncatch_errors ();

  return got_settings;
}
#endif

#ifndef HAVE_PGTK
/* Apply Xft settings in SETTINGS to the Xft library.
   Store a Lisp event that Xft settings changed.  */

static void
apply_xft_settings (Display_Info *dpyinfo,
                    struct xsettings *settings)
{
#if defined HAVE_XFT					\
  || (defined USE_CAIRO && defined CAIRO_HAS_FC_FONT	\
      && defined CAIRO_HAS_FT_FONT)
  FcPattern *pat;
  struct xsettings oldsettings;
  bool changed = false;
#ifndef HAVE_XFT
  cairo_font_options_t *options;
#endif


  memset (&oldsettings, 0, sizeof (oldsettings));
  pat = FcPatternCreate ();
#ifdef HAVE_XFT
  XftDefaultSubstitute (dpyinfo->display,
                        XScreenNumberOfScreen (dpyinfo->screen),
                        pat);
#else
  FcConfigSubstitute (NULL, pat, FcMatchPattern);
  options = cairo_font_options_create ();
  ftcrfont_get_default_font_options (dpyinfo, options);
  cairo_ft_font_options_substitute (options, pat);
  cairo_font_options_destroy (options);
  FcDefaultSubstitute (pat);
#endif
  FcPatternGetBool (pat, FC_ANTIALIAS, 0, &oldsettings.aa);
  FcPatternGetBool (pat, FC_HINTING, 0, &oldsettings.hinting);
#ifdef FC_HINT_STYLE
  FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &oldsettings.hintstyle);
#endif
  FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &oldsettings.lcdfilter);
  FcPatternGetInteger (pat, FC_RGBA, 0, &oldsettings.rgba);
  FcPatternGetDouble (pat, FC_DPI, 0, &oldsettings.dpi);

  if ((settings->seen & SEEN_AA) != 0 && oldsettings.aa != settings->aa)
    {
      FcPatternDel (pat, FC_ANTIALIAS);
      FcPatternAddBool (pat, FC_ANTIALIAS, settings->aa);
      changed = true;
      oldsettings.aa = settings->aa;
    }

  if ((settings->seen & SEEN_HINTING) != 0
      && oldsettings.hinting != settings->hinting)
    {
      FcPatternDel (pat, FC_HINTING);
      FcPatternAddBool (pat, FC_HINTING, settings->hinting);
      changed = true;
      oldsettings.hinting = settings->hinting;
    }
  if ((settings->seen & SEEN_RGBA) != 0 && oldsettings.rgba != settings->rgba)
    {
      FcPatternDel (pat, FC_RGBA);
      FcPatternAddInteger (pat, FC_RGBA, settings->rgba);
      oldsettings.rgba = settings->rgba;
      changed = true;
    }

  /* Older fontconfig versions don't have FC_LCD_FILTER. */
  if ((settings->seen & SEEN_LCDFILTER) != 0
      && oldsettings.lcdfilter != settings->lcdfilter)
    {
      FcPatternDel (pat, FC_LCD_FILTER);
      FcPatternAddInteger (pat, FC_LCD_FILTER, settings->lcdfilter);
      changed = true;
      oldsettings.lcdfilter = settings->lcdfilter;
    }

#ifdef FC_HINT_STYLE
  if ((settings->seen & SEEN_HINTSTYLE) != 0
      && oldsettings.hintstyle != settings->hintstyle)
    {
      FcPatternDel (pat, FC_HINT_STYLE);
      FcPatternAddInteger (pat, FC_HINT_STYLE, settings->hintstyle);
      changed = true;
      oldsettings.hintstyle = settings->hintstyle;
    }
#endif

#ifdef USE_CAIRO
  /* When Cairo is being used, set oldsettings.dpi to dpyinfo->resx.
     This is a gross hack, but seeing as Cairo fails to report
     anything reasonable, just use it to avoid config-changed events
     being sent at startup.  */
  oldsettings.dpi = dpyinfo->resx;
#endif

  if ((settings->seen & SEEN_DPI) != 0
      && settings->dpi > 0
      /* The following conjunct avoids setting `changed' to true when
	 old and new dpi settings do not differ "substantially".
	 Otherwise, the dynamic-setting Elisp code may process all sorts
	 of unrelated settings that override users' font customizations,
	 among others.  Compare:

	 https://lists.gnu.org/r/emacs-devel/2016-05/msg00557.html
	 https://lists.gnu.org/r/bug-gnu-emacs/2016-12/msg00820.html

	 As soon as the dynamic-settings code has been tested and
	 verified, this Emacs 25.2 workaround should be removed.  */
      && ((oldsettings.dpi >= settings->dpi
	   && (oldsettings.dpi - settings->dpi) > 2)
	  || ((settings->dpi > oldsettings.dpi)
	      && (settings->dpi - oldsettings.dpi) > 2)))
    {
      FcPatternDel (pat, FC_DPI);
      FcPatternAddDouble (pat, FC_DPI, settings->dpi);
      changed = true;
      oldsettings.dpi = settings->dpi;

      /* Changing the DPI on this display affects all frames on it.
	 Check FRAME_RES_X and FRAME_RES_Y in frame.h to see how.  */
      dpyinfo->resy = dpyinfo->resx = settings->dpi;
    }

  if (changed)
    {
      static char const format[] =
	"Antialias: %d, Hinting: %d, RGBA: %d, LCDFilter: %d, "
	"Hintstyle: %d, DPI: %f";
      enum
      {
	d_formats = 5,
	d_growth = INT_BUFSIZE_BOUND (int) - sizeof "%d",
	lf_formats = 1,
	max_f_integer_digits = DBL_MAX_10_EXP + 1,
	f_precision = 6,
	lf_growth = (sizeof "-." + max_f_integer_digits + f_precision
		     - sizeof "%f")
      };
      char buf[sizeof format + d_formats * d_growth + lf_formats * lf_growth];
#ifdef HAVE_XFT
      XftDefaultSet (dpyinfo->display, pat);
#else
      FcPatternDestroy (pat);
#endif
      store_config_changed_event (Qfont_render,
				  XCAR (dpyinfo->name_list_element));
      Vxft_settings
	= make_formatted_string (buf, format,
				 oldsettings.aa, oldsettings.hinting,
				 oldsettings.rgba, oldsettings.lcdfilter,
				 oldsettings.hintstyle, oldsettings.dpi);

    }
  else
    FcPatternDestroy (pat);
#endif /* HAVE_XFT || (USE_CAIRO && CAIRO_HAS_FC_FONT && CAIRO_HAS_FT_FONT) */
}
#endif

#ifndef HAVE_PGTK
/* Read XSettings from the display for DPYINFO.
   If SEND_EVENT_P store a Lisp event settings that changed.  */

static void
read_and_apply_settings (Display_Info *dpyinfo, bool send_event_p)
{
  struct xsettings settings;

  if (!read_settings (dpyinfo, &settings))
    return;

  apply_xft_settings (dpyinfo, &settings);
  if (settings.seen & SEEN_TB_STYLE)
    {
      if (send_event_p)
        store_tool_bar_style_changed (settings.tb_style, dpyinfo);
      else
        current_tool_bar_style = map_tool_bar_style (settings.tb_style);
      xfree (settings.tb_style);
    }
#if defined USE_CAIRO || defined HAVE_XFT
  if (settings.seen & SEEN_FONT)
    {
      if (send_event_p)
        store_font_name_changed (settings.font);
      else
	dupstring (&current_font, settings.font);
      xfree (settings.font);
    }
#endif
}
#endif

#ifndef HAVE_PGTK
/* Check if EVENT for the display in DPYINFO is XSettings related.
   Return true if it is, after performing associated side effects.  */

bool
xft_settings_event (Display_Info *dpyinfo, const XEvent *event)
{
  bool check_window_p = false, apply_settings_p = false;

  switch (event->type)
    {
    case DestroyNotify:
      if (dpyinfo->xsettings_window == event->xany.window)
        check_window_p = true;
      break;

    case ClientMessage:
      if (event->xclient.message_type == dpyinfo->Xatom_xsettings_mgr
          && event->xclient.data.l[1] == dpyinfo->Xatom_xsettings_sel
          && event->xclient.window == dpyinfo->root_window)
        check_window_p = true;
      break;

    case PropertyNotify:
      if (event->xproperty.window == dpyinfo->xsettings_window
          && event->xproperty.state == PropertyNewValue
          && event->xproperty.atom == dpyinfo->Xatom_xsettings_prop)
        apply_settings_p = true;
      break;
    }


  if (check_window_p)
    {
      dpyinfo->xsettings_window = None;
      get_prop_window (dpyinfo);
      if (dpyinfo->xsettings_window != None)
        apply_settings_p = true;
    }

  if (apply_settings_p)
    read_and_apply_settings (dpyinfo, true);

  return check_window_p || apply_settings_p;
}
#endif

/* Initialize GSettings and read startup values.  */

static void
init_gsettings (void)
{
#ifdef HAVE_GSETTINGS
  GVariant *val;
  bool schema_found = false;

#if ! GLIB_CHECK_VERSION (2, 36, 0)
  g_type_init ();
#endif

#if GLIB_CHECK_VERSION (2, 32, 0)
  {
    GSettingsSchema *sc = g_settings_schema_source_lookup
      (g_settings_schema_source_get_default (),
       GSETTINGS_SCHEMA,
       true);
    schema_found = sc != NULL;
    if (sc) g_settings_schema_unref (sc);
  }
#else
  {
    const gchar *const *schemas = g_settings_list_schemas ();
    if (schemas == NULL) return;
    while (! schema_found && *schemas != NULL)
      schema_found = strcmp (*schemas++, GSETTINGS_SCHEMA) == 0;
  }
#endif
  if (!schema_found) return;

  gsettings_client = g_settings_new (GSETTINGS_SCHEMA);
  if (!gsettings_client) return;
  g_object_ref_sink (G_OBJECT (gsettings_client));
  g_signal_connect (G_OBJECT (gsettings_client), "changed",
                    G_CALLBACK (something_changed_gsettingsCB), NULL);

  val = g_settings_get_value (gsettings_client, GSETTINGS_TOOL_BAR_STYLE);
  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
        current_tool_bar_style
          = map_tool_bar_style (g_variant_get_string (val, NULL));
      g_variant_unref (val);
    }

#if defined USE_CAIRO || defined HAVE_XFT
  val = g_settings_get_value (gsettings_client, GSETTINGS_MONO_FONT);
  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
	dupstring (&current_mono_font, g_variant_get_string (val, NULL));
      g_variant_unref (val);
    }

  val = g_settings_get_value (gsettings_client, GSETTINGS_FONT_NAME);
  if (val)
    {
      g_variant_ref_sink (val);
      if (g_variant_is_of_type (val, G_VARIANT_TYPE_STRING))
        dupstring (&current_font, g_variant_get_string (val, NULL));
      g_variant_unref (val);
    }

  /* Only use the gsettings font entries for the Cairo backend
     running on PGTK.  */
#ifdef HAVE_PGTK
  font_options = cairo_font_options_create ();
  apply_gsettings_font_antialias (gsettings_client);
  apply_gsettings_font_hinting (gsettings_client);
  apply_gsettings_font_rgba_order (gsettings_client);
#endif /* HAVE_PGTK */

#endif /* USE_CAIRO || HAVE_XFT */

#endif /* HAVE_GSETTINGS */
}

/* Init GConf and read startup values.  */

static void
init_gconf (void)
{
#if defined (HAVE_GCONF)
  char *s;

#if ! GLIB_CHECK_VERSION (2, 36, 0)
  g_type_init ();
#endif

  gconf_client = gconf_client_get_default ();
  gconf_client_set_error_handling (gconf_client, GCONF_CLIENT_HANDLE_NONE);
  gconf_client_add_dir (gconf_client,
                        GCONF_TOOL_BAR_STYLE,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           GCONF_TOOL_BAR_STYLE,
                           something_changed_gconfCB,
                           NULL, NULL, NULL);

  s = gconf_client_get_string (gconf_client, GCONF_TOOL_BAR_STYLE, NULL);
  if (s)
    {
      current_tool_bar_style = map_tool_bar_style (s);
      g_free (s);
    }

#if defined USE_CAIRO || defined HAVE_XFT
  s = gconf_client_get_string (gconf_client, GCONF_MONO_FONT, NULL);
  if (s)
    {
      dupstring (&current_mono_font, s);
      g_free (s);
    }
  s = gconf_client_get_string (gconf_client, GCONF_FONT_NAME, NULL);
  if (s)
    {
      dupstring (&current_font, s);
      g_free (s);
    }
  gconf_client_add_dir (gconf_client,
                        GCONF_MONO_FONT,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           GCONF_MONO_FONT,
                           something_changed_gconfCB,
                           NULL, NULL, NULL);
  gconf_client_add_dir (gconf_client,
                        GCONF_FONT_NAME,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           GCONF_FONT_NAME,
                           something_changed_gconfCB,
                           NULL, NULL, NULL);
#endif /* USE_CAIRO || HAVE_XFT */
#endif /* HAVE_GCONF */
}

#ifndef HAVE_PGTK
/* Init Xsettings and read startup values.  */

static void
init_xsettings (Display_Info *dpyinfo)
{
  Display *dpy = dpyinfo->display;

  block_input ();

  /* Select events so we can detect client messages sent when selection
     owner changes.  */
  XSelectInput (dpy, dpyinfo->root_window, StructureNotifyMask);

  get_prop_window (dpyinfo);
  if (dpyinfo->xsettings_window != None)
    read_and_apply_settings (dpyinfo, false);

  unblock_input ();
}
#endif

void
xsettings_initialize (Display_Info *dpyinfo)
{
  if (first_dpyinfo == NULL) first_dpyinfo = dpyinfo;
  init_gconf ();
#ifndef HAVE_PGTK
  init_xsettings (dpyinfo);
#endif
  init_gsettings ();
}

/* Return the system monospaced font.
   May be NULL if not known.  */

const char *
xsettings_get_system_font (void)
{
  return current_mono_font;
}

#ifdef USE_LUCID
/* Return the system font.
   May be NULL if not known.  */

const char *
xsettings_get_system_normal_font (void)
{
  return current_font;
}
#endif

#ifdef HAVE_PGTK
/* Return the cairo font options, updated from the gsettings font
   config entries.  The caller should call cairo_font_options_destroy
   on the result.  */
cairo_font_options_t *
xsettings_get_font_options (void)
{
  if (font_options != NULL)
    return cairo_font_options_copy (font_options);
  else
    /* GSettings is not configured.  */
    return cairo_font_options_create ();
}
#endif

DEFUN ("font-get-system-normal-font", Ffont_get_system_normal_font,
       Sfont_get_system_normal_font,
       0, 0, 0,
       doc: /* Get the system default application font.
The font is returned as either a font-spec or font name.  */)
  (void)
{
  return current_font ? build_string (current_font) : Qnil;
}

DEFUN ("font-get-system-font", Ffont_get_system_font, Sfont_get_system_font,
       0, 0, 0,
       doc: /* Get the system default fixed width font.
The font is returned as either a font-spec or font name.  */)
  (void)
{
  return current_mono_font ? build_string (current_mono_font) : Qnil;
}

DEFUN ("tool-bar-get-system-style", Ftool_bar_get_system_style,
       Stool_bar_get_system_style, 0, 0, 0,
       doc: /* Get the system tool bar style.
If no system tool bar style is known, return `tool-bar-style' if set to a
known style.  Otherwise return image.  */)
  (void)
{
  if (EQ (Vtool_bar_style, Qimage)
      || EQ (Vtool_bar_style, Qtext)
      || EQ (Vtool_bar_style, Qboth)
      || EQ (Vtool_bar_style, Qboth_horiz)
      || EQ (Vtool_bar_style, Qtext_image_horiz))
    return Vtool_bar_style;
  if (!NILP (current_tool_bar_style))
    return current_tool_bar_style;
  return Qimage;
}

void
syms_of_xsettings (void)
{
  current_mono_font = NULL;
  PDUMPER_IGNORE (current_mono_font);
  current_font = NULL;
  PDUMPER_IGNORE (current_font);
  first_dpyinfo = NULL;
  PDUMPER_IGNORE (first_dpyinfo);
#ifdef HAVE_GSETTINGS
  gsettings_client = NULL;
  PDUMPER_IGNORE (gsettings_client);
#endif
#ifdef HAVE_GCONF
  gconf_client = NULL;
  PDUMPER_IGNORE (gconf_client);
#endif
#ifdef HAVE_PGTK
  font_options = NULL;
  PDUMPER_IGNORE (font_options);
#endif

  DEFSYM (Qmonospace_font_name, "monospace-font-name");
  DEFSYM (Qfont_name, "font-name");
  DEFSYM (Qfont_render, "font-render");
  DEFSYM (Qdynamic_setting, "dynamic-setting");
  DEFSYM (Qfont_render_setting, "font-render-setting");
  DEFSYM (Qsystem_font_setting, "system-font-setting");

  defsubr (&Sfont_get_system_font);
  defsubr (&Sfont_get_system_normal_font);

  DEFVAR_BOOL ("font-use-system-font", use_system_font,
    doc: /* Non-nil means to apply the system defined font dynamically.
When this is non-nil and the system defined fixed width font changes, we
update frames dynamically.
If this variable is nil, Emacs ignores system font changes.  */);
  use_system_font = false;

  DEFVAR_LISP ("xft-settings", Vxft_settings,
               doc: /* Font settings applied to Xft.  */);
  Vxft_settings = empty_unibyte_string;

#if defined USE_CAIRO || defined HAVE_XFT
  Fprovide (Qfont_render_setting, Qnil);
#if defined (HAVE_GCONF) || defined (HAVE_GSETTINGS)
  Fprovide (Qsystem_font_setting, Qnil);
#endif
#endif

  current_tool_bar_style = Qnil;
  DEFSYM (Qtool_bar_style, "tool-bar-style");
  defsubr (&Stool_bar_get_system_style);

  Fprovide (Qdynamic_setting, Qnil);
}
