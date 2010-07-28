/* Functions for handle font and other changes dynamically.
   Copyright (C) 2009, 2010
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

#include <config.h>
#include <limits.h>
#include <setjmp.h>
#include <fcntl.h>
#include "lisp.h"
#include "xterm.h"
#include "xsettings.h"
#include "frame.h"
#include "keyboard.h"
#include "blockinput.h"
#include "termhooks.h"
#include "termopts.h"

#include <X11/Xproto.h>

#ifdef HAVE_GCONF
#include <gconf/gconf-client.h>
#endif
#ifdef HAVE_XFT
#include <X11/Xft/Xft.h>
#endif

static char *current_mono_font;
static char *current_font;
static struct x_display_info *first_dpyinfo;
static Lisp_Object Qmonospace_font_name, Qfont_name, Qfont_render,
  Qtool_bar_style;
static int use_system_font;
static Lisp_Object Vxft_settings;
static Lisp_Object current_tool_bar_style;

#ifdef HAVE_GCONF
static GConfClient *gconf_client;
#endif


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

#define XSETTINGS_FONT_NAME       "Gtk/FontName"
#define XSETTINGS_TOOL_BAR_STYLE  "Gtk/ToolbarStyle"

enum {
  SEEN_AA         = 0x01,
  SEEN_HINTING    = 0x02,
  SEEN_RGBA       = 0x04,
  SEEN_LCDFILTER  = 0x08,
  SEEN_HINTSTYLE  = 0x10,
  SEEN_DPI        = 0x20,
  SEEN_FONT       = 0x40,
  SEEN_TB_STYLE   = 0x80,
};
struct xsettings 
{
#ifdef HAVE_XFT
  FcBool aa, hinting;
  int rgba, lcdfilter, hintstyle;
  double dpi;
#endif

  char *font;
  char *tb_style;

  unsigned seen;
};

#ifdef HAVE_GCONF

#define SYSTEM_MONO_FONT     "/desktop/gnome/interface/monospace_font_name"
#define SYSTEM_FONT          "/desktop/gnome/interface/font_name"

/* Callback called when something changed in GConf that we care about,
   that is SYSTEM_MONO_FONT.  */

static void
something_changedCB (GConfClient *client,
                     guint cnxn_id,
                     GConfEntry *entry,
                     gpointer user_data)
{
  GConfValue *v = gconf_entry_get_value (entry);
  
  if (!v) return;
  if (v->type == GCONF_VALUE_STRING)
    {
      const char *value = gconf_value_get_string (v);
      if (current_mono_font != NULL && strcmp (value, current_mono_font) == 0)
        return; /* No change. */

      xfree (current_mono_font);
      current_mono_font = xstrdup (value);
    }


  if (first_dpyinfo != NULL)
    {
      /* Check if display still open */
      struct x_display_info *dpyinfo;
      int found = 0;
      for (dpyinfo = x_display_list; !found && dpyinfo; dpyinfo = dpyinfo->next)
        found = dpyinfo == first_dpyinfo;

      if (found && use_system_font)
        store_config_changed_event (Qmonospace_font_name,
                                    XCAR (first_dpyinfo->name_list_element));
    }
}
#endif /* HAVE_GCONF */

#ifdef HAVE_XFT

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

#endif /* HAVE_XFT */

/* Find the window that contains the XSETTINGS property values.  */

static void
get_prop_window (struct x_display_info *dpyinfo)
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

#define SWAP32(nr) (((nr) << 24) | (((nr) << 8) & 0xff0000)     \
                    | (((nr) >> 8) & 0xff00) | ((nr) >> 24))
#define SWAP16(nr) (((nr) << 8) | ((nr) >> 8))
#define PAD(nr)    (((nr) + 3) & ~3)

/* Parse xsettings and extract those that deal with Xft.
   See http://freedesktop.org/wiki/Specifications/XSettingsRegistry
   and http://standards.freedesktop.org/xsettings-spec/xsettings-spec-0.5.html.

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
                long unsigned int bytes,
                struct xsettings *settings)
{
  Lisp_Object byteorder = Fbyteorder ();
  int my_bo = XFASTINT (byteorder) == 'B' ? MSBFirst : LSBFirst;
  int that_bo = prop[0];
  CARD32 n_settings;
  int bytes_parsed = 0;
  int settings_seen = 0;
  int i = 0;

  /* First 4 bytes is a serial number, skip that.  */

  if (bytes < 12) return BadLength;
  memcpy (&n_settings, prop+8, 4);
  if (my_bo != that_bo) n_settings = SWAP32 (n_settings);
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
      int want_this;
      int to_cpy;

      sval[0] = '\0';
      ++i;
      ++bytes_parsed; /* Padding */

      memcpy (&nlen, prop+bytes_parsed, 2);
      bytes_parsed += 2;
      if (my_bo != that_bo) nlen = SWAP16 (nlen);
      if (bytes_parsed+nlen > bytes) return BadLength;
      to_cpy = nlen > 127 ? 127 : nlen;
      memcpy (name, prop+bytes_parsed, to_cpy);
      name[to_cpy] = '\0';

      bytes_parsed += nlen;
      bytes_parsed = PAD (bytes_parsed);

      bytes_parsed += 4; /* Skip serial for this value */
      if (bytes_parsed > bytes) return BadLength;

      want_this =
#ifdef HAVE_XFT
        (nlen > 6 && strncmp (name, "Xft/", 4) == 0)
        ||
#endif
        (strcmp (XSETTINGS_FONT_NAME, name) == 0)
        || (strcmp (XSETTINGS_TOOL_BAR_STYLE, name) == 0);

      switch (type) 
        {
        case 0: /* Integer */
          if (bytes_parsed+4 > bytes) return BadLength;
          if (want_this)
            {
              memcpy (&ival, prop+bytes_parsed, 4);
              if (my_bo != that_bo) ival = SWAP32 (ival);
            }
          bytes_parsed += 4;
          break;

        case 1: /* String */
          if (bytes_parsed+4 > bytes) return BadLength;
          memcpy (&vlen, prop+bytes_parsed, 4);
          bytes_parsed += 4;
          if (my_bo != that_bo) vlen = SWAP32 (vlen);
          if (want_this)
            {
              to_cpy = vlen > 127 ? 127 : vlen;
              memcpy (sval, prop+bytes_parsed, to_cpy);
              sval[to_cpy] = '\0';
            }
          bytes_parsed += vlen;
          bytes_parsed = PAD (bytes_parsed);
          break;

        case 2: /* RGB value */
          /* No need to parse this */
          if (bytes_parsed+8 > bytes) return BadLength;
          bytes_parsed += 8; /* 4 values (r, b, g, alpha), 2 bytes each.  */ 
          break;

        default: /* Parse Error */
          return BadValue;
        }

      if (want_this) 
        {
          ++settings_seen;
          if (strcmp (name, XSETTINGS_FONT_NAME) == 0)
            {
              settings->font = xstrdup (sval);
              settings->seen |= SEEN_FONT;
            }
          else if (strcmp (name, XSETTINGS_TOOL_BAR_STYLE) == 0)
            {
              settings->tb_style = xstrdup (sval);
              settings->seen |= SEEN_TB_STYLE;
            }
#ifdef HAVE_XFT
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
          else if (strcmp (name, "Xft/DPI") == 0)
            {
              settings->seen |= SEEN_DPI;
              settings->dpi = (double)ival/1024.0;
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
#endif /* HAVE_XFT */
        }
    }

  return settings_seen;
}

static int
read_settings (struct x_display_info *dpyinfo, struct xsettings *settings)
{
  Atom act_type;
  int act_form;
  unsigned long nitems, bytes_after;
  unsigned char *prop = NULL;
  Display *dpy = dpyinfo->display;
  int rc;

  x_catch_errors (dpy);
  rc = XGetWindowProperty (dpy,
                           dpyinfo->xsettings_window,
                           dpyinfo->Xatom_xsettings_prop,
                           0, LONG_MAX, False, AnyPropertyType,
                           &act_type, &act_form, &nitems, &bytes_after,
                           &prop);

  if (rc == Success && prop != NULL && act_form == 8 && nitems > 0
      && act_type == dpyinfo->Xatom_xsettings_prop)
    rc = parse_settings (prop, nitems, settings);

  XFree (prop);

  x_uncatch_errors ();

  return rc != 0;
}


static void
apply_xft_settings (struct x_display_info *dpyinfo,
                    int send_event_p,
                    struct xsettings *settings)
{
#ifdef HAVE_XFT
  FcPattern *pat;
  struct xsettings oldsettings;
  int changed = 0;
  char buf[256];

  memset (&oldsettings, 0, sizeof (oldsettings));
  buf[0] = '\0';
  pat = FcPatternCreate ();
  XftDefaultSubstitute (dpyinfo->display,
                        XScreenNumberOfScreen (dpyinfo->screen),
                        pat);
  FcPatternGetBool (pat, FC_ANTIALIAS, 0, &oldsettings.aa);
  FcPatternGetBool (pat, FC_HINTING, 0, &oldsettings.hinting);
  FcPatternGetInteger (pat, FC_HINT_STYLE, 0, &oldsettings.hintstyle);
  FcPatternGetInteger (pat, FC_LCD_FILTER, 0, &oldsettings.lcdfilter);
  FcPatternGetInteger (pat, FC_RGBA, 0, &oldsettings.rgba);
  FcPatternGetDouble (pat, FC_DPI, 0, &oldsettings.dpi);

  if ((settings->seen & SEEN_AA) != 0 && oldsettings.aa != settings->aa)
    {
      FcPatternDel (pat, FC_ANTIALIAS);
      FcPatternAddBool (pat, FC_ANTIALIAS, settings->aa);
      ++changed;
      oldsettings.aa = settings->aa;
    }
  sprintf (buf, "Antialias: %d", oldsettings.aa);

  if ((settings->seen & SEEN_HINTING) != 0
      && oldsettings.hinting != settings->hinting)
    {
      FcPatternDel (pat, FC_HINTING);
      FcPatternAddBool (pat, FC_HINTING, settings->hinting);
      ++changed;
      oldsettings.hinting = settings->hinting;
    }
  if (strlen (buf) > 0) strcat (buf, ", ");
  sprintf (buf+strlen (buf), "Hinting: %d", oldsettings.hinting);
  if ((settings->seen & SEEN_RGBA) != 0 && oldsettings.rgba != settings->rgba)
    {
      FcPatternDel (pat, FC_RGBA);
      FcPatternAddInteger (pat, FC_RGBA, settings->rgba);
      oldsettings.rgba = settings->rgba;
      ++changed;
    }
  if (strlen (buf) > 0) strcat (buf, ", ");
  sprintf (buf+strlen (buf), "RGBA: %d", oldsettings.rgba);

  /* Older fontconfig versions don't have FC_LCD_FILTER. */
  if ((settings->seen & SEEN_LCDFILTER) != 0
      && oldsettings.lcdfilter != settings->lcdfilter)
    {
      FcPatternDel (pat, FC_LCD_FILTER);
      FcPatternAddInteger (pat, FC_LCD_FILTER, settings->lcdfilter);
      ++changed;
      oldsettings.lcdfilter = settings->lcdfilter;
    }
  if (strlen (buf) > 0) strcat (buf, ", ");
  sprintf (buf+strlen (buf), "LCDFilter: %d", oldsettings.lcdfilter);

  if ((settings->seen & SEEN_HINTSTYLE) != 0
      && oldsettings.hintstyle != settings->hintstyle)
    {
      FcPatternDel (pat, FC_HINT_STYLE);
      FcPatternAddInteger (pat, FC_HINT_STYLE, settings->hintstyle);
      ++changed;
      oldsettings.hintstyle = settings->hintstyle;
    }
  if (strlen (buf) > 0) strcat (buf, ", ");
  sprintf (buf+strlen (buf), "Hintstyle: %d", oldsettings.hintstyle);

  if ((settings->seen & SEEN_DPI) != 0 && oldsettings.dpi != settings->dpi
      && settings->dpi > 0)
    {
      Lisp_Object frame, tail;

      FcPatternDel (pat, FC_DPI);
      FcPatternAddDouble (pat, FC_DPI, settings->dpi);
      ++changed;
      oldsettings.dpi = settings->dpi;
      
      /* Change the DPI on this display and all frames on the display.  */
      dpyinfo->resy = dpyinfo->resx = settings->dpi;
      FOR_EACH_FRAME (tail, frame)
        if (FRAME_X_P (XFRAME (frame))
            && FRAME_X_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
          XFRAME (frame)->resy = XFRAME (frame)->resx = settings->dpi;
    }

  if (strlen (buf) > 0) strcat (buf, ", ");
  sprintf (buf+strlen (buf), "DPI: %lf", oldsettings.dpi);

  if (changed)
    {
      XftDefaultSet (dpyinfo->display, pat);
      if (send_event_p)
        store_config_changed_event (Qfont_render,
                                    XCAR (dpyinfo->name_list_element));
      Vxft_settings = make_string (buf, strlen (buf));
    }
  else
    FcPatternDestroy (pat);
#endif /* HAVE_XFT */
}

static void
read_and_apply_settings (struct x_display_info *dpyinfo, int send_event_p)
{
  struct xsettings settings;
  Lisp_Object dpyname = XCAR (dpyinfo->name_list_element);

  if (!read_settings (dpyinfo, &settings))
    return;

  apply_xft_settings (dpyinfo, True, &settings);
  if (settings.seen & SEEN_TB_STYLE)
    {
      Lisp_Object style = Qnil;
      if (strcmp (settings.tb_style, "both") == 0)
        style = Qboth;
      else if (strcmp (settings.tb_style, "both-horiz") == 0)
        style = Qboth_horiz;
      else if (strcmp (settings.tb_style, "icons") == 0)
        style = Qimage;
      else if (strcmp (settings.tb_style, "text") == 0)
        style = Qtext;
      if (!NILP (style) && !EQ (style, current_tool_bar_style))
        {
          current_tool_bar_style = style;
          if (send_event_p)
            store_config_changed_event (Qtool_bar_style, dpyname);
        }
      free (settings.tb_style);
    }

  if (settings.seen & SEEN_FONT)
    {
      if (!current_font || strcmp (current_font, settings.font) != 0) 
        {
          free (current_font);
          current_font = settings.font;
          if (send_event_p)
            store_config_changed_event (Qfont_name, dpyname);
        }
      else
        free (settings.font);
    }
}

void
xft_settings_event (struct x_display_info *dpyinfo, XEvent *event)
{
  int check_window_p = 0;
  int apply_settings = 0;

  switch (event->type)
    {
    case DestroyNotify:
      if (dpyinfo->xsettings_window == event->xany.window)
        check_window_p = 1;
      break;

    case ClientMessage:
      if (event->xclient.message_type == dpyinfo->Xatom_xsettings_mgr
          && event->xclient.data.l[1] == dpyinfo->Xatom_xsettings_sel
          && event->xclient.window == dpyinfo->root_window)
        check_window_p = 1;
      break;

    case PropertyNotify:
      if (event->xproperty.window == dpyinfo->xsettings_window
          && event->xproperty.state == PropertyNewValue
          && event->xproperty.atom == dpyinfo->Xatom_xsettings_prop)
        apply_settings = 1;
      break;
    }


  if (check_window_p)
    {
      dpyinfo->xsettings_window = None;
      get_prop_window (dpyinfo);
      if (dpyinfo->xsettings_window != None)
        apply_settings = 1;
    }

  if (apply_settings)
    read_and_apply_settings (dpyinfo, True);
}


static void
init_gconf (void)
{
#if defined (HAVE_GCONF) && defined (HAVE_XFT)
  char *s;

  g_type_init ();
  gconf_client = gconf_client_get_default ();
  s = gconf_client_get_string (gconf_client, SYSTEM_MONO_FONT, NULL);
  if (s)
    {
      current_mono_font = xstrdup (s);
      g_free (s);
    }
  s = gconf_client_get_string (gconf_client, SYSTEM_FONT, NULL);
  if (s)
    {
      current_font = xstrdup (s);
      g_free (s);
    }
  gconf_client_set_error_handling (gconf_client, GCONF_CLIENT_HANDLE_NONE);
  gconf_client_add_dir (gconf_client,
                        SYSTEM_MONO_FONT,
                        GCONF_CLIENT_PRELOAD_ONELEVEL,
                        NULL);
  gconf_client_notify_add (gconf_client,
                           SYSTEM_MONO_FONT,
                           something_changedCB,
                           NULL, NULL, NULL);
#endif /* HAVE_GCONF && HAVE_XFT */
}

static void
init_xsettings (struct x_display_info *dpyinfo)
{
  char sel[64];
  Display *dpy = dpyinfo->display;

  BLOCK_INPUT;

  sprintf (sel, "_XSETTINGS_S%d", XScreenNumberOfScreen (dpyinfo->screen));
  dpyinfo->Xatom_xsettings_sel = XInternAtom (dpy, sel, False);
  dpyinfo->Xatom_xsettings_prop = XInternAtom (dpy,
                                               "_XSETTINGS_SETTINGS",
                                               False);
  dpyinfo->Xatom_xsettings_mgr = XInternAtom (dpy, "MANAGER", False);

  /* Select events so we can detect client messages sent when selection
     owner changes.  */
  XSelectInput (dpy, dpyinfo->root_window, StructureNotifyMask);

  get_prop_window (dpyinfo);
  if (dpyinfo->xsettings_window != None)
    read_and_apply_settings (dpyinfo, False);

  UNBLOCK_INPUT;
}

void
xsettings_initialize (struct x_display_info *dpyinfo)
{
  if (first_dpyinfo == NULL) first_dpyinfo = dpyinfo;
  init_gconf ();
  init_xsettings (dpyinfo);
}

const char *
xsettings_get_system_font (void)
{
  return current_mono_font;
}

const char *
xsettings_get_system_normal_font (void)
{
  return current_font;
}

DEFUN ("font-get-system-normal-font", Ffont_get_system_normal_font,
       Sfont_get_system_normal_font,
       0, 0, 0,
       doc: /* Get the system default application font. */)
  (void)
{
  return current_font
    ? make_string (current_font, strlen (current_font))
    : Qnil;
}

DEFUN ("font-get-system-font", Ffont_get_system_font, Sfont_get_system_font,
       0, 0, 0,
       doc: /* Get the system default fixed width font. */)
  (void)
{
  return current_mono_font
    ? make_string (current_mono_font, strlen (current_mono_font))
    : Qnil;
}

DEFUN ("tool-bar-get-system-style", Ftool_bar_get_system_style, Stool_bar_get_system_style,
       0, 0, 0,
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
  current_font = NULL;
  first_dpyinfo = NULL;
#ifdef HAVE_GCONF
  gconf_client = NULL;
#endif

  Qmonospace_font_name = intern_c_string ("monospace-font-name");
  staticpro (&Qmonospace_font_name);
  Qfont_name = intern_c_string ("font-name");
  staticpro (&Qfont_name);
  Qfont_render = intern_c_string ("font-render");
  staticpro (&Qfont_render);
  defsubr (&Sfont_get_system_font);
  defsubr (&Sfont_get_system_normal_font);

  DEFVAR_BOOL ("font-use-system-font", &use_system_font,
    doc: /* *Non-nil means to apply the system defined font dynamically.
When this is non-nil and the system defined fixed width font changes, we
update frames dynamically.
If this variable is nil, Emacs ignores system font changes.  */);
  use_system_font = 0;

  DEFVAR_LISP ("xft-settings", &Vxft_settings,
               doc: /* Font settings applied to Xft.  */);
  Vxft_settings = make_string ("", 0);

#ifdef HAVE_XFT
  Fprovide (intern_c_string ("font-render-setting"), Qnil);
#ifdef HAVE_GCONF
  Fprovide (intern_c_string ("system-font-setting"), Qnil);
#endif
#endif

  current_tool_bar_style = Qnil;
  Qtool_bar_style = intern_c_string ("tool-bar-style");
  staticpro (&Qtool_bar_style);
  defsubr (&Stool_bar_get_system_style);

  Fprovide (intern_c_string ("dynamic-setting"), Qnil);
}

/* arch-tag: 541716ed-2e6b-42e1-8212-3197e01ea61d
   (do not change this comment) */
