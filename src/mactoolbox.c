/* Functions for GUI implemented with (HI)Toolbox on the Mac OS.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008  Free Software Foundation, Inc.

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

#include <stdio.h>

#include "lisp.h"
#include "blockinput.h"

#include "macterm.h"

#if !TARGET_API_MAC_CARBON
#include <Quickdraw.h>
#include <ToolUtils.h>
#include <Sound.h>
#include <Events.h>
#include <Script.h>
#include <Resources.h>
#include <Fonts.h>
#include <TextUtils.h>
#include <LowMem.h>
#include <Controls.h>
#include <Windows.h>
#include <Displays.h>
#if defined (__MRC__) || (__MSL__ >= 0x6000)
#include <ControlDefinitions.h>
#endif

#if __profile__
#include <profiler.h>
#endif
#endif /* not TARGET_API_MAC_CARBON */

#include "charset.h"
#include "coding.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"

#include <sys/param.h>

#ifndef MAC_OSX
#include <alloca.h>
#endif


/************************************************************************
			       General
 ************************************************************************/

/* The difference in pixels between the top left corner of the
   Emacs window (including possible window manager decorations)
   and FRAME_MAC_WINDOW (f).  */
#define FRAME_OUTER_TO_INNER_DIFF_X(f) ((f)->x_pixels_diff)
#define FRAME_OUTER_TO_INNER_DIFF_Y(f) ((f)->y_pixels_diff)

#define mac_window_to_frame(wp) (((mac_output *) GetWRefCon (wp))->mFP)

void
mac_alert_sound_play ()
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
  AlertSoundPlay ();
#else
  SysBeep (1);
#endif
}


/************************************************************************
			     Application
 ************************************************************************/

extern struct frame *mac_focus_frame P_ ((struct mac_display_info *));
extern void do_keystroke P_ ((EventKind, unsigned char, UInt32, UInt32,
			      unsigned long, struct input_event *));
extern UInt32 mac_mapped_modifiers P_ ((UInt32, UInt32));
#if TARGET_API_MAC_CARBON
extern int mac_to_emacs_modifiers P_ ((UInt32, UInt32));
#else
extern int mac_to_emacs_modifiers P_ ((EventModifiers, EventModifiers));
#endif

#if TARGET_API_MAC_CARBON
/* Points to the variable `inev' in the function XTread_socket.  It is
   used for passing an input event to the function back from
   Carbon/Apple event handlers.  */
static struct input_event *read_socket_inev = NULL;

extern const unsigned char keycode_to_xkeysym_table[];
extern EMACS_INT extra_keyboard_modifiers;

extern Lisp_Object Qhi_command;
#if USE_MAC_TSM
static TSMDocumentID tsm_document_id;
extern Lisp_Object Qtext_input;
extern Lisp_Object Qupdate_active_input_area, Qunicode_for_key_event;
extern Lisp_Object Vmac_ts_active_input_overlay, Vmac_ts_active_input_buf;
extern Lisp_Object Qbefore_string;
#endif

static int mac_event_to_emacs_modifiers P_ ((EventRef));
static OSStatus install_menu_target_item_handler P_ ((void));
#ifdef MAC_OSX
static OSStatus install_service_handler P_ ((void));
#endif

extern OSStatus mac_store_event_ref_as_apple_event P_ ((AEEventClass, AEEventID,
							Lisp_Object,
							Lisp_Object,
							EventRef, UInt32,
							const EventParamName *,
							const EventParamType *));
extern int fast_find_position P_ ((struct window *, int, int *, int *,
				   int *, int *, Lisp_Object));
extern struct glyph *x_y_to_hpos_vpos P_ ((struct window *, int, int,
					   int *, int *, int *, int *, int *));
extern void mac_ax_selected_text_range P_ ((struct frame *, CFRange *));
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
extern unsigned int mac_ax_number_of_characters P_ ((struct frame *));
#endif

#if USE_MAC_TSM
extern OSStatus mac_restore_keyboard_input_source P_ ((void));
extern void mac_save_keyboard_input_source P_ ((void));

static OSStatus
mac_tsm_resume ()
{
  OSStatus err;

  err = ActivateTSMDocument (tsm_document_id);
  if (err == noErr)
    err = mac_restore_keyboard_input_source ();

  return err;
}

static OSStatus
mac_tsm_suspend ()
{
  OSStatus err;

  mac_save_keyboard_input_source ();
  err = DeactivateTSMDocument (tsm_document_id);

  return err;
}

static void
init_tsm ()
{
#ifdef MAC_OSX
  static InterfaceTypeList types = {kUnicodeDocument};
#else
  static InterfaceTypeList types = {kTextService};
#endif

  NewTSMDocument (sizeof (types) / sizeof (types[0]), types,
		  &tsm_document_id, 0);
}
#endif	/* USE_MAC_TSM */

static pascal OSStatus
mac_handle_keyboard_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result = eventNotHandledErr;
  UInt32 event_kind, key_code, modifiers;
  unsigned char char_code;

  event_kind = GetEventKind (event);
  switch (event_kind)
    {
    case kEventRawKeyDown:
    case kEventRawKeyRepeat:
    case kEventRawKeyUp:
      /* When using Carbon Events, we need to pass raw keyboard events
	 to the TSM ourselves.  If TSM handles it, it will pass back
	 noErr, otherwise it will pass back "eventNotHandledErr" and
	 we can process it normally.  */
      result = CallNextEventHandler (next_handler, event);
      if (result != eventNotHandledErr)
	break;

      if (read_socket_inev == NULL)
	break;

#if USE_MAC_TSM
      if (read_socket_inev->kind != NO_EVENT)
	{
	  result = noErr;
	  break;
	}
#endif

      if (event_kind == kEventRawKeyUp)
	break;

      err = GetEventParameter (event, kEventParamKeyMacCharCodes,
			       typeChar, NULL,
			       sizeof (char), NULL, &char_code);
      if (err != noErr)
	break;

      err = GetEventParameter (event, kEventParamKeyCode,
			       typeUInt32, NULL,
			       sizeof (UInt32), NULL, &key_code);
      if (err != noErr)
	break;

      err = GetEventParameter (event, kEventParamKeyModifiers,
			       typeUInt32, NULL,
			       sizeof (UInt32), NULL, &modifiers);
      if (err != noErr)
	break;

      do_keystroke ((event_kind == kEventRawKeyDown ? keyDown : autoKey),
		    char_code, key_code, modifiers,
		    ((unsigned long)
		     (GetEventTime (event) / kEventDurationMillisecond)),
		    read_socket_inev);
      result = noErr;
      break;

    default:
      abort ();
    }

  return result;
}

static pascal OSStatus
mac_handle_command_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result = eventNotHandledErr;
  HICommand command;
  static const EventParamName names[] =
    {kEventParamDirectObject, kEventParamKeyModifiers};
  static const EventParamType types[] =
    {typeHICommand, typeUInt32};
  int num_params = sizeof (names) / sizeof (names[0]);

  err = GetEventParameter (event, kEventParamDirectObject, typeHICommand,
			   NULL, sizeof (HICommand), NULL, &command);
  if (err != noErr)
    return eventNotHandledErr;

  switch (GetEventKind (event))
    {
    case kEventCommandProcess:
      result = CallNextEventHandler (next_handler, event);
      if (result != eventNotHandledErr)
	break;

      err = GetEventParameter (event, kEventParamDirectObject,
			       typeHICommand, NULL,
			       sizeof (HICommand), NULL, &command);

      if (err != noErr || command.commandID == 0)
	break;

      /* A HI command event is mapped to an Apple event whose event
	 class symbol is `hi-command' and event ID is its command
	 ID.  */
      err = mac_store_event_ref_as_apple_event (0, command.commandID,
						Qhi_command, Qnil,
						event, num_params,
						names, types);
      if (err == noErr)
	result = noErr;
      break;

    default:
      abort ();
    }

  return result;
}

static pascal OSStatus
mac_handle_mouse_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result = eventNotHandledErr;

  switch (GetEventKind (event))
    {
    case kEventMouseWheelMoved:
      {
	WindowRef wp;
	struct frame *f;
	EventMouseWheelAxis axis;
	SInt32 delta;
	Point point;

	result = CallNextEventHandler (next_handler, event);
	if (result != eventNotHandledErr || read_socket_inev == NULL)
	  break;

	f = mac_focus_frame (&one_mac_display_info);

	err = GetEventParameter (event, kEventParamWindowRef, typeWindowRef,
				 NULL, sizeof (WindowRef), NULL, &wp);
	if (err != noErr
	    || wp != FRAME_MAC_WINDOW (f))
	  break;

	err = GetEventParameter (event, kEventParamMouseWheelAxis,
				 typeMouseWheelAxis, NULL,
				 sizeof (EventMouseWheelAxis), NULL, &axis);
	if (err != noErr || axis != kEventMouseWheelAxisY)
	  break;

	err = GetEventParameter (event, kEventParamMouseLocation,
				 typeQDPoint, NULL, sizeof (Point),
				 NULL, &point);
	if (err != noErr)
	  break;

	point.h -= f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
	point.v -= f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);
	if (point.h < 0 || point.v < 0
	    || EQ (window_from_coordinates (f, point.h, point.v, 0, 0, 0, 1),
		   f->tool_bar_window))
	  break;

	err = GetEventParameter (event, kEventParamMouseWheelDelta,
				 typeSInt32, NULL, sizeof (SInt32),
				 NULL, &delta);
	if (err != noErr)
	  break;

	read_socket_inev->kind = WHEEL_EVENT;
	read_socket_inev->code = 0;
	read_socket_inev->modifiers =
	  (mac_event_to_emacs_modifiers (event)
	   | ((delta < 0) ? down_modifier : up_modifier));
	XSETINT (read_socket_inev->x, point.h);
	XSETINT (read_socket_inev->y, point.v);
	XSETFRAME (read_socket_inev->frame_or_window, f);

	result = noErr;
      }
      break;

    default:
      abort ();
    }

  return result;
}

#if USE_MAC_TSM
extern void mac_get_selected_range P_ ((struct window *, CFRange *));
extern int mac_store_buffer_text_to_unicode_chars P_ ((struct buffer *,
						       int, int, UniChar *));

static pascal OSStatus
mac_handle_text_input_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result;
  Lisp_Object id_key = Qnil;
  int num_params;
  const EventParamName *names;
  const EventParamType *types;
  static UInt32 seqno_uaia = 0;
  static const EventParamName names_uaia[] =
    {kEventParamTextInputSendComponentInstance,
     kEventParamTextInputSendRefCon,
     kEventParamTextInputSendSLRec,
     kEventParamTextInputSendFixLen,
     kEventParamTextInputSendText,
     kEventParamTextInputSendUpdateRng,
     kEventParamTextInputSendHiliteRng,
     kEventParamTextInputSendClauseRng,
     kEventParamTextInputSendPinRng,
     kEventParamTextInputSendTextServiceEncoding,
     kEventParamTextInputSendTextServiceMacEncoding,
     EVENT_PARAM_TEXT_INPUT_SEQUENCE_NUMBER};
  static const EventParamType types_uaia[] =
    {typeComponentInstance,
     typeLongInteger,
     typeIntlWritingCode,
     typeLongInteger,
#ifdef MAC_OSX
     typeUnicodeText,
#else
     typeChar,
#endif
     typeTextRangeArray,
     typeTextRangeArray,
     typeOffsetArray,
     typeTextRange,
     typeUInt32,
     typeUInt32,
     typeUInt32};
  static const EventParamName names_ufke[] =
    {kEventParamTextInputSendComponentInstance,
     kEventParamTextInputSendRefCon,
     kEventParamTextInputSendSLRec,
     kEventParamTextInputSendText};
  static const EventParamType types_ufke[] =
    {typeComponentInstance,
     typeLongInteger,
     typeIntlWritingCode,
     typeUnicodeText};

  result = CallNextEventHandler (next_handler, event);
  if (result != eventNotHandledErr)
    return result;

  switch (GetEventKind (event))
    {
    case kEventTextInputUpdateActiveInputArea:
      id_key = Qupdate_active_input_area;
      num_params = sizeof (names_uaia) / sizeof (names_uaia[0]);
      names = names_uaia;
      types = types_uaia;
      SetEventParameter (event, EVENT_PARAM_TEXT_INPUT_SEQUENCE_NUMBER,
			 typeUInt32, sizeof (UInt32), &seqno_uaia);
      seqno_uaia++;
      result = noErr;
      break;

    case kEventTextInputUnicodeForKeyEvent:
      {
	EventRef kbd_event;
	UInt32 actual_size, modifiers, key_code;

	err = GetEventParameter (event, kEventParamTextInputSendKeyboardEvent,
				 typeEventRef, NULL, sizeof (EventRef), NULL,
				 &kbd_event);
	if (err == noErr)
	  err = GetEventParameter (kbd_event, kEventParamKeyModifiers,
				   typeUInt32, NULL,
				   sizeof (UInt32), NULL, &modifiers);
	if (err == noErr)
	  err = GetEventParameter (kbd_event, kEventParamKeyCode,
				   typeUInt32, NULL, sizeof (UInt32),
				   NULL, &key_code);
	if (err == noErr && mac_mapped_modifiers (modifiers, key_code))
	  /* There're mapped modifier keys.  Process it in
	     do_keystroke.  */
	  break;
	if (err == noErr)
	  err = GetEventParameter (kbd_event, kEventParamKeyUnicodes,
				   typeUnicodeText, NULL, 0, &actual_size,
				   NULL);
	if (err == noErr && actual_size == sizeof (UniChar))
	  {
	    UniChar code;

	    err = GetEventParameter (kbd_event, kEventParamKeyUnicodes,
				     typeUnicodeText, NULL,
				     sizeof (UniChar), NULL, &code);
	    if (err == noErr && code < 0x80)
	      {
		/* ASCII character.  Process it in do_keystroke.  */
		if (read_socket_inev && code >= 0x20 && code <= 0x7e
		    && !(key_code <= 0x7f
			 && keycode_to_xkeysym_table [key_code]))
		  {
		    struct frame *f = mac_focus_frame (&one_mac_display_info);

		    read_socket_inev->kind = ASCII_KEYSTROKE_EVENT;
		    read_socket_inev->code = code;
		    read_socket_inev->modifiers =
		      mac_to_emacs_modifiers (modifiers, 0);
		    read_socket_inev->modifiers |=
		      (extra_keyboard_modifiers
		       & (meta_modifier | alt_modifier
			  | hyper_modifier | super_modifier));
		    XSETFRAME (read_socket_inev->frame_or_window, f);
		  }
		break;
	      }
	  }
	if (err == noErr)
	  {
	    /* Non-ASCII keystrokes without mapped modifiers are
	       processed at the Lisp level.  */
	    id_key = Qunicode_for_key_event;
	    num_params = sizeof (names_ufke) / sizeof (names_ufke[0]);
	    names = names_ufke;
	    types = types_ufke;
	    result = noErr;
	  }
      }
      break;

    case kEventTextInputOffsetToPos:
      {
	long byte_offset;
	struct frame *f;
	struct window *w;
	Point p;

	err = GetEventParameter (event, kEventParamTextInputSendTextOffset,
				 typeLongInteger, NULL, sizeof (long), NULL,
				 &byte_offset);
	if (err != noErr)
	  break;

	if (STRINGP (Vmac_ts_active_input_buf)
	    && SBYTES (Vmac_ts_active_input_buf) != 0)
	  {
	    if (!OVERLAYP (Vmac_ts_active_input_overlay))
	      break;

	    /* Strictly speaking, this is not always correct because
	       previous events may change some states about display.  */
	    if (!NILP (Foverlay_get (Vmac_ts_active_input_overlay, Qbefore_string)))
	      {
		/* Active input area is displayed around the current point.  */
		f = SELECTED_FRAME ();
		w = XWINDOW (f->selected_window);
	      }
	    else if (WINDOWP (echo_area_window))
	      {
		/* Active input area is displayed in the echo area.  */
		w = XWINDOW (echo_area_window);
		f = WINDOW_XFRAME (w);
	      }
	    else
	      break;

	    p.h = (WINDOW_TO_FRAME_PIXEL_X (w, w->cursor.x)
		   + WINDOW_LEFT_FRINGE_WIDTH (w)
		   + f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f));
	    p.v = (WINDOW_TO_FRAME_PIXEL_Y (w, w->cursor.y)
		   + FONT_BASE (FRAME_FONT (f))
		   + f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f));
	  }
	else
	  {
#ifndef MAC_OSX
	    break;
#else  /* MAC_OSX */
	    CFRange sel_range;
	    int charpos;
	    int hpos, vpos, x, y;
	    struct glyph_row *row;
	    struct glyph *glyph;
	    XFontStruct *font;

	    f = mac_focus_frame (&one_mac_display_info);
	    w = XWINDOW (f->selected_window);
	    mac_get_selected_range (w, &sel_range);
	    charpos = (BUF_BEGV (XBUFFER (w->buffer)) + sel_range.location
		       + byte_offset / (long) sizeof (UniChar));

	    if (!fast_find_position (w, charpos, &hpos, &vpos, &x, &y, Qnil))
	      {
		result = errOffsetInvalid;
		break;
	      }

	    row = MATRIX_ROW (w->current_matrix, vpos);
	    glyph = row->glyphs[TEXT_AREA] + hpos;
	    if (glyph->type != CHAR_GLYPH || glyph->glyph_not_available_p)
	      break;

	    p.h = (WINDOW_TEXT_TO_FRAME_PIXEL_X (w, x)
		   + f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f));
	    p.v = (WINDOW_TO_FRAME_PIXEL_Y (w, y)
		   + row->visible_height
		   + f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f));

	    font = FACE_FROM_ID (f, glyph->face_id)->font;
	    if (font)
	      {
		Fixed point_size = Long2Fix (font->mac_fontsize);
		short height = row->visible_height;
		short ascent = row->ascent;

		SetEventParameter (event,
				   kEventParamTextInputReplyPointSize,
				   typeFixed, sizeof (Fixed), &point_size);
		SetEventParameter (event,
				   kEventParamTextInputReplyLineHeight,
				   typeShortInteger, sizeof (short), &height);
		SetEventParameter (event,
				   kEventParamTextInputReplyLineAscent,
				   typeShortInteger, sizeof (short), &ascent);
		if (font->mac_fontnum != -1)
		  {
		    OSStatus err1;
		    FMFont fm_font;
		    FMFontStyle style;

		    err1 = FMGetFontFromFontFamilyInstance (font->mac_fontnum,
							    font->mac_fontface,
							    &fm_font, &style);
		    if (err1 == noErr)
		      SetEventParameter (event, kEventParamTextInputReplyFMFont,
					 typeUInt32, sizeof (UInt32), &fm_font);
		    else
		      {
			long qd_font = font->mac_fontnum;

			SetEventParameter (event, kEventParamTextInputReplyFont,
					   typeLongInteger, sizeof (long),
					   &qd_font);
		      }
		  }
		else if (font->mac_style)
		  {
		    OSStatus err1;
		    ATSUFontID font_id;

		    err1 = ATSUGetAttribute (font->mac_style, kATSUFontTag,
					     sizeof (ATSUFontID), &font_id,
					     NULL);
		    if (err1 == noErr)
		      SetEventParameter (event, kEventParamTextInputReplyFMFont,
					 typeUInt32, sizeof (UInt32), &font_id);
		  }
		else
		  abort ();
	      }
#endif	/* MAC_OSX */
	  }

	err = SetEventParameter (event, kEventParamTextInputReplyPoint,
				 typeQDPoint, sizeof (Point), &p);
	if (err == noErr)
	  result = noErr;
      }
      break;

#ifdef MAC_OSX
    case kEventTextInputPosToOffset:
      {
	Point point;
	Boolean leading_edge_p = true;
	struct frame *f;
	int x, y;
	Lisp_Object window;
	enum window_part part;
	long region_class = kTSMOutsideOfBody, byte_offset = 0;

	err = GetEventParameter (event, kEventParamTextInputSendCurrentPoint,
				 typeQDPoint, NULL, sizeof (Point), NULL,
				 &point);
	if (err != noErr)
	  break;

	GetEventParameter (event, kEventParamTextInputReplyLeadingEdge,
			   typeBoolean, NULL, sizeof (Boolean), NULL,
			   &leading_edge_p);

	f = mac_focus_frame (&one_mac_display_info);
	x = point.h - (f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f));
	y = point.v - (f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f));
	window = window_from_coordinates (f, x, y, &part, 0, 0, 1);
	if (WINDOWP (window) && EQ (window, f->selected_window))
	  {
	    struct window *w;
	    struct buffer *b;

	    /* Convert to window-relative pixel coordinates.  */
	    w = XWINDOW (window);
	    frame_to_window_pixel_xy (w, &x, &y);

	    /* Are we in a window whose display is up to date?
	       And verify the buffer's text has not changed.  */
	    b = XBUFFER (w->buffer);
	    if (part == ON_TEXT
		&& EQ (w->window_end_valid, w->buffer)
		&& XINT (w->last_modified) == BUF_MODIFF (b)
		&& XINT (w->last_overlay_modified) == BUF_OVERLAY_MODIFF (b))
	      {
		int hpos, vpos, area;
		struct glyph *glyph;

		/* Find the glyph under X/Y.  */
		glyph = x_y_to_hpos_vpos (w, x, y, &hpos, &vpos, 0, 0, &area);

		if (glyph != NULL && area == TEXT_AREA)
		  {
		    byte_offset = ((glyph->charpos - BUF_BEGV (b))
				   * sizeof (UniChar));
		    region_class = kTSMInsideOfBody;
		  }
	      }
	  }

	err = SetEventParameter (event, kEventParamTextInputReplyRegionClass,
				 typeLongInteger, sizeof (long),
				 &region_class);
	if (err == noErr)
	  err = SetEventParameter (event, kEventParamTextInputReplyTextOffset,
				   typeLongInteger, sizeof (long),
				   &byte_offset);
	if (err == noErr)
	  result = noErr;
      }
      break;

    case kEventTextInputGetSelectedText:
      {
	struct frame *f = mac_focus_frame (&one_mac_display_info);
	struct window *w = XWINDOW (f->selected_window);
	struct buffer *b = XBUFFER (w->buffer);
	CFRange sel_range;
	int start, end;
	UniChar *characters, c;

	if (poll_suppress_count == 0 && !NILP (Vinhibit_quit))
	  /* Don't try to get buffer contents as the gap might be
	     being altered. */
	  break;

	mac_get_selected_range (w, &sel_range);
	if (sel_range.length == 0)
	  {
	    Boolean leading_edge_p;

	    err = GetEventParameter (event,
				     kEventParamTextInputReplyLeadingEdge,
				     typeBoolean, NULL, sizeof (Boolean), NULL,
				     &leading_edge_p);
	    if (err != noErr)
	      break;

	    start = BUF_BEGV (b) + sel_range.location;
	    if (!leading_edge_p)
	      start--;
	    end = start + 1;
	    characters = &c;

	    if (start < BUF_BEGV (b) || end > BUF_ZV (b))
	      break;
	  }
	else
	  {
	    start = BUF_BEGV (b) + sel_range.location;
	    end = start + sel_range.length;
	    characters = xmalloc (sel_range.length * sizeof (UniChar));
	  }

	if (mac_store_buffer_text_to_unicode_chars (b, start, end, characters))
	  err = SetEventParameter (event, kEventParamTextInputReplyText,
				   typeUnicodeText,
				   sel_range.length * sizeof (UniChar),
				   characters);
	if (characters != &c)
	  xfree (characters);

	if (err == noErr)
	  result = noErr;
      }
      break;
#endif	/* MAC_OSX */

    default:
      abort ();
    }

  if (!NILP (id_key))
    err = mac_store_event_ref_as_apple_event (0, 0, Qtext_input, id_key,
					      event, num_params,
					      names, types);
  return result;
}

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
static pascal OSStatus
mac_handle_document_access_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result;
  struct frame *f = mac_focus_frame (&one_mac_display_info);

  result = CallNextEventHandler (next_handler, event);
  if (result != eventNotHandledErr)
    return result;

  switch (GetEventKind (event))
    {
    case kEventTSMDocumentAccessGetLength:
      {
	CFIndex count = mac_ax_number_of_characters (f);

	err = SetEventParameter (event, kEventParamTSMDocAccessCharacterCount,
				 typeCFIndex, sizeof (CFIndex), &count);
	if (err == noErr)
	  result = noErr;
      }
      break;

    case kEventTSMDocumentAccessGetSelectedRange:
      {
	CFRange sel_range;

	mac_ax_selected_text_range (f, &sel_range);
	err = SetEventParameter (event,
				 kEventParamTSMDocAccessReplyCharacterRange,
				 typeCFRange, sizeof (CFRange), &sel_range);
	if (err == noErr)
	  result = noErr;
      }
      break;

    case kEventTSMDocumentAccessGetCharacters:
      {
	struct buffer *b = XBUFFER (XWINDOW (f->selected_window)->buffer);
	CFRange range;
	Ptr characters;
	int start, end;

	if (poll_suppress_count == 0 && !NILP (Vinhibit_quit))
	  /* Don't try to get buffer contents as the gap might be
	     being altered. */
	  break;

	err = GetEventParameter (event,
				 kEventParamTSMDocAccessSendCharacterRange,
				 typeCFRange, NULL, sizeof (CFRange), NULL,
				 &range);
	if (err == noErr)
	  err = GetEventParameter (event,
				   kEventParamTSMDocAccessSendCharactersPtr,
				   typePtr, NULL, sizeof (Ptr), NULL,
				   &characters);
	if (err != noErr)
	  break;

	start = BUF_BEGV (b) + range.location;
	end = start + range.length;
	if (mac_store_buffer_text_to_unicode_chars (b, start, end,
						    (UniChar *) characters))
	  result = noErr;
      }
      break;

    default:
      abort ();
    }

  return result;
}
#endif
#endif

OSStatus
install_application_handler ()
{
  OSStatus err = noErr;

  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassKeyboard, kEventRawKeyDown},
	 {kEventClassKeyboard, kEventRawKeyRepeat},
	 {kEventClassKeyboard, kEventRawKeyUp}};

      err = InstallApplicationEventHandler (NewEventHandlerUPP
					    (mac_handle_keyboard_event),
					    GetEventTypeCount (specs),
					    specs, NULL, NULL);
    }

  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassCommand, kEventCommandProcess}};

      err = InstallApplicationEventHandler (NewEventHandlerUPP
					    (mac_handle_command_event),
					    GetEventTypeCount (specs),
					    specs, NULL, NULL);
    }

  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassMouse, kEventMouseWheelMoved}};

      err = InstallApplicationEventHandler (NewEventHandlerUPP
					    (mac_handle_mouse_event),
					    GetEventTypeCount (specs),
					    specs, NULL, NULL);
    }

#if USE_MAC_TSM
  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassTextInput, kEventTextInputUpdateActiveInputArea},
	 {kEventClassTextInput, kEventTextInputUnicodeForKeyEvent},
	 {kEventClassTextInput, kEventTextInputOffsetToPos},
#ifdef MAC_OSX
	 {kEventClassTextInput, kEventTextInputPosToOffset},
	 {kEventClassTextInput, kEventTextInputGetSelectedText}
#endif
	};

      err = InstallApplicationEventHandler (NewEventHandlerUPP
					    (mac_handle_text_input_event),
					    GetEventTypeCount (specs),
					    specs, NULL, NULL);
    }

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassTSMDocumentAccess, kEventTSMDocumentAccessGetLength},
	 {kEventClassTSMDocumentAccess, kEventTSMDocumentAccessGetSelectedRange},
	 {kEventClassTSMDocumentAccess, kEventTSMDocumentAccessGetCharacters}};

      err = InstallApplicationEventHandler (mac_handle_document_access_event,
					    GetEventTypeCount (specs),
					    specs, NULL, NULL);
    }
#endif
#endif

  if (err == noErr)
    err = install_menu_target_item_handler ();

#ifdef MAC_OSX
  if (err == noErr)
    err = install_service_handler ();
#endif

  return err;
}
#endif	/* TARGET_API_MAC_CARBON */


/************************************************************************
			       Windows
 ************************************************************************/

#define DEFAULT_NUM_COLS 80

#define MIN_DOC_SIZE 64
#define MAX_DOC_SIZE 32767

/* Drag and Drop */
static OSErr install_drag_handler P_ ((WindowRef));
static void remove_drag_handler P_ ((WindowRef));

#if USE_CG_DRAWING
static void mac_prepare_for_quickdraw P_ ((struct frame *));
#endif

extern void mac_handle_visibility_change P_ ((struct frame *));
extern void mac_handle_origin_change P_ ((struct frame *));
extern void mac_handle_size_change P_ ((struct frame *, int, int));

#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
extern Lisp_Object Qwindow;
extern Lisp_Object Qtoolbar_switch_mode;
#endif
#endif

static void
do_window_update (WindowRef win)
{
  struct frame *f = mac_window_to_frame (win);

  BeginUpdate (win);

  /* The tooltip has been drawn already.  Avoid the SET_FRAME_GARBAGED
     below.  */
  if (win != tip_window)
    {
      if (f->async_visible == 0)
        {
	  /* Update events may occur when a frame gets iconified.  */
#if 0
          f->async_visible = 1;
          f->async_iconified = 0;
          SET_FRAME_GARBAGED (f);
#endif
        }
      else
	{
	  Rect r;
#if TARGET_API_MAC_CARBON
	  RgnHandle region = NewRgn ();

	  GetPortVisibleRegion (GetWindowPort (win), region);
	  GetRegionBounds (region, &r);
	  expose_frame (f, r.left, r.top, r.right - r.left, r.bottom - r.top);
#if USE_CG_DRAWING
	  mac_prepare_for_quickdraw (f);
#endif
	  UpdateControls (win, region);
	  DisposeRgn (region);
#else
	  r = (*win->visRgn)->rgnBBox;
	  expose_frame (f, r.left, r.top, r.right - r.left, r.bottom - r.top);
	  UpdateControls (win, win->visRgn);
#endif
	}
    }

  EndUpdate (win);
}

static int
is_emacs_window (WindowRef win)
{
  Lisp_Object tail, frame;

  if (!win)
    return 0;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_MAC_P (XFRAME (frame)))
      if (FRAME_MAC_WINDOW (XFRAME (frame)) == win)
	return 1;

  return 0;
}

/* Handle drags in size box.  Based on code contributed by Ben
   Mesander and IM - Window Manager A.  */

static void
do_grow_window (w, e)
     WindowRef w;
     const EventRecord *e;
{
  Rect limit_rect;
  int rows, columns, width, height;
  struct frame *f = mac_window_to_frame (w);
  XSizeHints *size_hints = FRAME_SIZE_HINTS (f);
  int min_width = MIN_DOC_SIZE, min_height = MIN_DOC_SIZE;
#if TARGET_API_MAC_CARBON
  Rect new_rect;
#else
  long grow_size;
#endif

  if (size_hints->flags & PMinSize)
    {
      min_width  = size_hints->min_width;
      min_height = size_hints->min_height;
    }
  SetRect (&limit_rect, min_width, min_height, MAX_DOC_SIZE, MAX_DOC_SIZE);

#if TARGET_API_MAC_CARBON
  if (!ResizeWindow (w, e->where, &limit_rect, &new_rect))
    return;
  height = new_rect.bottom - new_rect.top;
  width = new_rect.right - new_rect.left;
#else
  grow_size = GrowWindow (w, e->where, &limit_rect);
  /* see if it really changed size */
  if (grow_size == 0)
    return;
  height = HiWord (grow_size);
  width = LoWord (grow_size);
#endif

  if (width != FRAME_PIXEL_WIDTH (f)
      || height != FRAME_PIXEL_HEIGHT (f))
    {
      rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, height);
      columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, width);

      x_set_window_size (f, 0, columns, rows);
    }
}

#if TARGET_API_MAC_CARBON
static Point
mac_get_ideal_size (f)
     struct frame *f;
{
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  WindowRef w = FRAME_MAC_WINDOW (f);
  Point ideal_size;
  Rect standard_rect;
  int height, width, columns, rows;

  ideal_size.h = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, DEFAULT_NUM_COLS);
  ideal_size.v = dpyinfo->height;
  IsWindowInStandardState (w, &ideal_size, &standard_rect);
  /* Adjust the standard size according to character boundaries.  */
  width = standard_rect.right - standard_rect.left;
  height = standard_rect.bottom - standard_rect.top;
  columns = FRAME_PIXEL_WIDTH_TO_TEXT_COLS (f, width);
  rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, height);
  ideal_size.h = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, columns);
  ideal_size.v = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);

  return ideal_size;
}

static pascal OSStatus
mac_handle_window_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  WindowRef wp;
  OSStatus err, result = eventNotHandledErr;
  struct frame *f;
  UInt32 attributes;
  XSizeHints *size_hints;

  err = GetEventParameter (event, kEventParamDirectObject, typeWindowRef,
			   NULL, sizeof (WindowRef), NULL, &wp);
  if (err != noErr)
    return eventNotHandledErr;

  f = mac_window_to_frame (wp);
  switch (GetEventKind (event))
    {
      /* -- window refresh events -- */

    case kEventWindowUpdate:
      result = CallNextEventHandler (next_handler, event);
      if (result != eventNotHandledErr)
	break;

      do_window_update (wp);
      result = noErr;
      break;

      /* -- window state change events -- */

    case kEventWindowShowing:
      size_hints = FRAME_SIZE_HINTS (f);
      if (!(size_hints->flags & (USPosition | PPosition)))
	{
	  struct frame *sf = SELECTED_FRAME ();

	  if (!(FRAME_MAC_P (sf) && sf->async_visible))
	    RepositionWindow (wp, NULL, kWindowCenterOnMainScreen);
	  else
	    {
	      RepositionWindow (wp, FRAME_MAC_WINDOW (sf),
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
				kWindowCascadeStartAtParentWindowScreen
#else
				kWindowCascadeOnParentWindowScreen
#endif
				);
#if USE_MAC_TOOLBAR
	      /* This is a workaround.  RepositionWindow fails to put
		 a window at the cascading position when its parent
		 window has a Carbon HIToolbar.  */
	      if ((f->left_pos == sf->left_pos
		   && f->top_pos == sf->top_pos)
		  || (f->left_pos == sf->left_pos + 10 * 2
		      && f->top_pos == sf->top_pos + 32 * 2))
		MoveWindowStructure (wp, sf->left_pos + 10, sf->top_pos + 32);
#endif
	    }
	  result = noErr;
	}
      break;

    case kEventWindowHiding:
      /* Before unmapping the window, update the WM_SIZE_HINTS
	 property to claim that the current position of the window is
	 user-specified, rather than program-specified, so that when
	 the window is mapped again, it will be placed at the same
	 location, without forcing the user to position it by hand
	 again (they have already done that once for this window.)  */
      x_wm_set_size_hint (f, (long) 0, 1);
      result = noErr;
      break;

    case kEventWindowShown:
    case kEventWindowHidden:
    case kEventWindowCollapsed:
    case kEventWindowExpanded:
      mac_handle_visibility_change (f);
      result = noErr;
      break;

    case kEventWindowBoundsChanging:
      result = CallNextEventHandler (next_handler, event);
      if (result != eventNotHandledErr)
	break;

      err = GetEventParameter (event, kEventParamAttributes, typeUInt32,
			       NULL, sizeof (UInt32), NULL, &attributes);
      if (err != noErr)
	break;

      size_hints = FRAME_SIZE_HINTS (f);
      if ((attributes & kWindowBoundsChangeUserResize)
	  && ((size_hints->flags & (PResizeInc | PBaseSize | PMinSize))
	      == (PResizeInc | PBaseSize | PMinSize)))
	{
	  Rect bounds;
	  int width, height;

	  err = GetEventParameter (event, kEventParamCurrentBounds,
				   typeQDRectangle, NULL, sizeof (Rect),
				   NULL, &bounds);
	  if (err != noErr)
	    break;

	  width = bounds.right - bounds.left;
	  height = bounds.bottom - bounds.top;

	  if (width < size_hints->min_width)
	    width = size_hints->min_width;
	  else
	    width = size_hints->base_width
	      + (int) ((width - size_hints->base_width)
		       / (float) size_hints->width_inc + .5)
	      * size_hints->width_inc;

	  if (height < size_hints->min_height)
	    height = size_hints->min_height;
	  else
	    height = size_hints->base_height
	      + (int) ((height - size_hints->base_height)
		       / (float) size_hints->height_inc + .5)
	      * size_hints->height_inc;

	  bounds.right = bounds.left + width;
	  bounds.bottom = bounds.top + height;
	  SetEventParameter (event, kEventParamCurrentBounds,
			     typeQDRectangle, sizeof (Rect), &bounds);
	  result = noErr;
	}
      break;

    case kEventWindowBoundsChanged:
      err = GetEventParameter (event, kEventParamAttributes, typeUInt32,
			       NULL, sizeof (UInt32), NULL, &attributes);
      if (err != noErr)
	break;

      if (attributes & kWindowBoundsChangeSizeChanged)
	{
	  Rect bounds;

	  err = GetEventParameter (event, kEventParamCurrentBounds,
				   typeQDRectangle, NULL, sizeof (Rect),
				   NULL, &bounds);
	  if (err == noErr)
	    {
	      int width, height;

	      width = bounds.right - bounds.left;
	      height = bounds.bottom - bounds.top;
	      mac_handle_size_change (f, width, height);
	      mac_wakeup_from_rne ();
	    }
	}

      if (attributes & kWindowBoundsChangeOriginChanged)
	mac_handle_origin_change (f);

      result = noErr;
      break;

      /* -- window action events -- */

    case kEventWindowClose:
      {
	struct input_event buf;

	EVENT_INIT (buf);
	buf.kind = DELETE_WINDOW_EVENT;
	XSETFRAME (buf.frame_or_window, f);
	buf.arg = Qnil;
	kbd_buffer_store_event (&buf);
      }
      result = noErr;
      break;

    case kEventWindowGetIdealSize:
      result = CallNextEventHandler (next_handler, event);
      if (result != eventNotHandledErr)
	break;

      {
	Point ideal_size = mac_get_ideal_size (f);

	err = SetEventParameter (event, kEventParamDimensions,
				 typeQDPoint, sizeof (Point), &ideal_size);
	if (err == noErr)
	  result = noErr;
      }
      break;

#ifdef MAC_OSX
    case kEventWindowToolbarSwitchMode:
      {
	static const EventParamName names[] = {kEventParamDirectObject,
					       kEventParamWindowMouseLocation,
					       kEventParamKeyModifiers,
					       kEventParamMouseButton,
					       kEventParamClickCount,
					       kEventParamMouseChord};
	static const EventParamType types[] = {typeWindowRef,
					       typeQDPoint,
					       typeUInt32,
					       typeMouseButton,
					       typeUInt32,
					       typeUInt32};
	int num_params = sizeof (names) / sizeof (names[0]);

	err = mac_store_event_ref_as_apple_event (0, 0,
						  Qwindow,
						  Qtoolbar_switch_mode,
						  event, num_params,
						  names, types);
      }
      if (err == noErr)
	result = noErr;
      break;
#endif

#if USE_MAC_TSM
      /* -- window focus events -- */

    case kEventWindowFocusAcquired:
      err = mac_tsm_resume ();
      if (err == noErr)
	result = noErr;
      break;

    case kEventWindowFocusRelinquish:
      err = mac_tsm_suspend ();
      if (err == noErr)
	result = noErr;
      break;
#endif

    default:
      abort ();
    }

  return result;
}
#endif

/* Handle clicks in zoom box.  Calculation of "standard state" based
   on code in IM - Window Manager A and code contributed by Ben
   Mesander.  The standard state of an Emacs window is 80-characters
   wide (DEFAULT_NUM_COLS) and as tall as will fit on the screen.  */

static void
do_zoom_window (WindowRef w, int zoom_in_or_out)
{
  Rect zoom_rect, port_rect;
  int width, height;
  struct frame *f = mac_window_to_frame (w);
#if TARGET_API_MAC_CARBON
  Point ideal_size = mac_get_ideal_size (f);

  GetWindowBounds (w, kWindowContentRgn, &port_rect);
  if (IsWindowInStandardState (w, &ideal_size, &zoom_rect)
      && port_rect.left == zoom_rect.left
      && port_rect.top == zoom_rect.top)
    zoom_in_or_out = inZoomIn;
  else
    zoom_in_or_out = inZoomOut;

#ifdef MAC_OS8
  mac_clear_area (f, 0, 0, port_rect.right - port_rect.left,
		  port_rect.bottom - port_rect.top);
#endif
  ZoomWindowIdeal (w, zoom_in_or_out, &ideal_size);
#else /* not TARGET_API_MAC_CARBON */
  GrafPtr save_port;
  Point top_left;
  int w_title_height, rows;
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

  GetPort (&save_port);

  SetPortWindowPort (w);

  /* Clear window to avoid flicker.  */
  EraseRect (&(w->portRect));
  if (zoom_in_or_out == inZoomOut)
    {
      SetPt (&top_left, w->portRect.left, w->portRect.top);
      LocalToGlobal (&top_left);

      /* calculate height of window's title bar */
      w_title_height = top_left.v - 1
	- (**((WindowPeek) w)->strucRgn).rgnBBox.top + GetMBarHeight ();

      /* get maximum height of window into zoom_rect.bottom - zoom_rect.top */
      zoom_rect = qd.screenBits.bounds;
      zoom_rect.top += w_title_height;
      InsetRect (&zoom_rect, 8, 4);  /* not too tight */

      zoom_rect.right = zoom_rect.left
	+ FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, DEFAULT_NUM_COLS);

      /* Adjust the standard size according to character boundaries.  */
      rows = FRAME_PIXEL_HEIGHT_TO_TEXT_LINES (f, zoom_rect.bottom - zoom_rect.top);
      zoom_rect.bottom =
	zoom_rect.top + FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, rows);

      (**((WStateDataHandle) ((WindowPeek) w)->dataHandle)).stdState
	= zoom_rect;
    }

  ZoomWindow (w, zoom_in_or_out, f == mac_focus_frame (dpyinfo));

  SetPort (save_port);
#endif /* not TARGET_API_MAC_CARBON */

#if !TARGET_API_MAC_CARBON
  /* retrieve window size and update application values */
  port_rect = w->portRect;
  height = port_rect.bottom - port_rect.top;
  width = port_rect.right - port_rect.left;

  mac_handle_size_change (f, width, height);
  mac_handle_origin_change (f);
#endif
}

static OSStatus
install_window_handler (window)
     WindowRef window;
{
  OSStatus err = noErr;

#if TARGET_API_MAC_CARBON
  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{
	  /* -- window refresh events -- */
	  {kEventClassWindow, kEventWindowUpdate},
	  /* -- window state change events -- */
	  {kEventClassWindow, kEventWindowShowing},
	  {kEventClassWindow, kEventWindowHiding},
	  {kEventClassWindow, kEventWindowShown},
	  {kEventClassWindow, kEventWindowHidden},
	  {kEventClassWindow, kEventWindowCollapsed},
	  {kEventClassWindow, kEventWindowExpanded},
	  {kEventClassWindow, kEventWindowBoundsChanging},
	  {kEventClassWindow, kEventWindowBoundsChanged},
	  /* -- window action events -- */
	  {kEventClassWindow, kEventWindowClose},
	  {kEventClassWindow, kEventWindowGetIdealSize},
#ifdef MAC_OSX
	  {kEventClassWindow, kEventWindowToolbarSwitchMode},
#endif
#if USE_MAC_TSM
	  /* -- window focus events -- */
	  {kEventClassWindow, kEventWindowFocusAcquired},
	  {kEventClassWindow, kEventWindowFocusRelinquish},
#endif
	};
      static EventHandlerUPP handle_window_eventUPP = NULL;

      if (handle_window_eventUPP == NULL)
	handle_window_eventUPP = NewEventHandlerUPP (mac_handle_window_event);

      err = InstallWindowEventHandler (window, handle_window_eventUPP,
				       GetEventTypeCount (specs),
				       specs, NULL, NULL);
    }
#endif

  if (err == noErr)
    err = install_drag_handler (window);

  return err;
}

static void
remove_window_handler (window)
     WindowRef window;
{
  remove_drag_handler (window);
}

void
mac_get_window_bounds (f, inner, outer)
     struct frame *f;
     Rect *inner, *outer;
{
#if TARGET_API_MAC_CARBON
  GetWindowBounds (FRAME_MAC_WINDOW (f), kWindowContentRgn, inner);
  GetWindowBounds (FRAME_MAC_WINDOW (f), kWindowStructureRgn, outer);
#else /* not TARGET_API_MAC_CARBON */
  RgnHandle region = NewRgn ();

  GetWindowRegion (FRAME_MAC_WINDOW (f), kWindowContentRgn, region);
  *inner = (*region)->rgnBBox;
  GetWindowRegion (FRAME_MAC_WINDOW (f), kWindowStructureRgn, region);
  *outer = (*region)->rgnBBox;
  DisposeRgn (region);
#endif /* not TARGET_API_MAC_CARBON */
}

Rect *
mac_get_frame_bounds (f, r)
     struct frame *f;
     Rect *r;
{
#if TARGET_API_MAC_CARBON
  return GetWindowPortBounds (FRAME_MAC_WINDOW (f), r);
#else
  *r = FRAME_MAC_WINDOW (f)->portRect;

  return r;
#endif
}

void
mac_get_frame_mouse (f, point)
     struct frame *f;
     Point *point;
{
#if TARGET_API_MAC_CARBON
  GetGlobalMouse (point);
  point->h -= f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  point->v -= f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);
#else
  SetPortWindowPort (FRAME_MAC_WINDOW (f));
  GetMouse (point);
#endif
}

void
mac_convert_frame_point_to_global (f, x, y)
     struct frame *f;
     int *x, *y;
{
  *x += f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  *y += f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);
}

#if TARGET_API_MAC_CARBON
void
mac_update_proxy_icon (f)
     struct frame *f;
{
  OSStatus err;
  Lisp_Object file_name =
    XBUFFER (XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer)->filename;
  Window w = FRAME_MAC_WINDOW (f);
  AliasHandle alias = NULL;

  err = GetWindowProxyAlias (w, &alias);
  if (err == errWindowDoesNotHaveProxy && !STRINGP (file_name))
    return;

  if (STRINGP (file_name))
    {
      AEDesc desc;
#ifdef MAC_OSX
      FSRef fref, fref_proxy;
#else
      FSSpec fss, fss_proxy;
#endif
      Boolean changed;
      Lisp_Object encoded_file_name = ENCODE_FILE (file_name);

#ifdef MAC_OSX
      err = AECoercePtr (TYPE_FILE_NAME, SDATA (encoded_file_name),
			 SBYTES (encoded_file_name), typeFSRef, &desc);
#else
      SetPortWindowPort (w);
      err = AECoercePtr (TYPE_FILE_NAME, SDATA (encoded_file_name),
			 SBYTES (encoded_file_name), typeFSS, &desc);
#endif
      if (err == noErr)
	{
#ifdef MAC_OSX
	  err = AEGetDescData (&desc, &fref, sizeof (FSRef));
#else
	  err = AEGetDescData (&desc, &fss, sizeof (FSSpec));
#endif
	  AEDisposeDesc (&desc);
	}
      if (err == noErr)
	{
	  if (alias)
	    {
	      /* (FS)ResolveAlias never sets `changed' to true if
		 `alias' is minimal.  */
#ifdef MAC_OSX
	      err = FSResolveAlias (NULL, alias, &fref_proxy, &changed);
	      if (err == noErr)
		err = FSCompareFSRefs (&fref, &fref_proxy);
#else
	      err = ResolveAlias (NULL, alias, &fss_proxy, &changed);
	      if (err == noErr)
		err = !(fss.vRefNum == fss_proxy.vRefNum
			&& fss.parID == fss_proxy.parID
			&& EqualString (fss.name, fss_proxy.name,
					false, true));
#endif
	    }
	  if (err != noErr || alias == NULL)
	    {
	      if (alias)
		DisposeHandle ((Handle) alias);
#ifdef MAC_OSX
	      err = FSNewAliasMinimal (&fref, &alias);
#else
	      err = NewAliasMinimal (&fss, &alias);
#endif
	      changed = true;
	    }
	}
      if (err == noErr)
	if (changed)
	  err = SetWindowProxyAlias (w, alias);
    }

  if (alias)
    DisposeHandle ((Handle) alias);

  if (err != noErr || !STRINGP (file_name))
    RemoveWindowProxy (w);
}
#endif

/* Mac replacement for XSetWindowBackground.  */

void
mac_set_frame_window_background (f, color)
     struct frame *f;
     unsigned long color;
{
  WindowRef w = FRAME_MAC_WINDOW (f);
#if !TARGET_API_MAC_CARBON
  AuxWinHandle aw_handle;
  CTabHandle ctab_handle;
  ColorSpecPtr ct_table;
  short ct_size;
#endif
  RGBColor bg_color;

  bg_color.red = RED16_FROM_ULONG (color);
  bg_color.green = GREEN16_FROM_ULONG (color);
  bg_color.blue = BLUE16_FROM_ULONG (color);

#if TARGET_API_MAC_CARBON
  SetWindowContentColor (w, &bg_color);
#else
  if (GetAuxWin (w, &aw_handle))
    {
      ctab_handle = (*aw_handle)->awCTable;
      HandToHand ((Handle *) &ctab_handle);
      ct_table = (*ctab_handle)->ctTable;
      ct_size = (*ctab_handle)->ctSize;
      while (ct_size > -1)
	{
	  if (ct_table->value == 0)
	    {
	      ct_table->rgb = bg_color;
	      CTabChanged (ctab_handle);
	      SetWinColor (w, (WCTabHandle) ctab_handle);
	    }
	  ct_size--;
	}
    }
#endif
}

/* Flush display of frame F, or of all frames if F is null.  */

void
x_flush (f)
     struct frame *f;
{
#if TARGET_API_MAC_CARBON
  BLOCK_INPUT;
#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (f);
#endif
  if (f)
    QDFlushPortBuffer (GetWindowPort (FRAME_MAC_WINDOW (f)), NULL);
  else
    QDFlushPortBuffer (GetQDGlobalsThePort (), NULL);
  UNBLOCK_INPUT;
#endif
}

#if USE_CG_DRAWING
void
mac_flush_display_optional (f)
     struct frame *f;
{
  BLOCK_INPUT;
  mac_prepare_for_quickdraw (f);
  UNBLOCK_INPUT;
}
#endif

void
mac_update_begin (f)
     struct frame *f;
{
#if TARGET_API_MAC_CARBON
  /* During update of a frame, availability of input events is
     periodically checked with ReceiveNextEvent if
     redisplay-dont-pause is nil.  That normally flushes window buffer
     changes for every check, and thus screen update looks waving even
     if no input is available.  So we disable screen updates during
     update of a frame.  */
  DisableScreenUpdates ();
#endif
}

void
mac_update_end (f)
     struct frame *f;
{
#if TARGET_API_MAC_CARBON
  EnableScreenUpdates ();
#endif
}

void
mac_frame_up_to_date (f)
     struct frame *f;
{
  /* Nothing to do.  */
}

void
mac_create_frame_window (f, tooltip_p)
     struct frame *f;
     int tooltip_p;
{
  Rect r;
#if TARGET_API_MAC_CARBON
  WindowClass window_class;
  WindowAttributes attributes;
#else
  short proc_id;
  WindowRef behind;
  Boolean go_away_flag;
#endif

  if (!tooltip_p)
    {
      SetRect (&r, f->left_pos, f->top_pos,
	       f->left_pos + FRAME_PIXEL_WIDTH (f),
	       f->top_pos + FRAME_PIXEL_HEIGHT (f));
#if TARGET_API_MAC_CARBON
      window_class = kDocumentWindowClass;
      attributes =  (kWindowStandardDocumentAttributes
#ifdef MAC_OSX
		     | kWindowToolbarButtonAttribute
#endif
		     );
#else
      proc_id = zoomDocProc;
      behind = (WindowRef) -1;
      go_away_flag = true;
#endif
    }
  else
    {
      SetRect (&r, 0, 0, 1, 1);
#if TARGET_API_MAC_CARBON
      window_class = kHelpWindowClass;
      attributes = (kWindowNoUpdatesAttribute
		    | kWindowNoActivatesAttribute
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1020
		    | kWindowIgnoreClicksAttribute
#endif
		    );
#else
      proc_id = plainDBox;
      behind = NULL;
      go_away_flag = false;
#endif
    }

#if TARGET_API_MAC_CARBON
  CreateNewWindow (window_class, attributes, &r, &FRAME_MAC_WINDOW (f));
  if (FRAME_MAC_WINDOW (f))
    {
      SetWRefCon (FRAME_MAC_WINDOW (f), (long) f->output_data.mac);
      if (!tooltip_p)
	if (install_window_handler (FRAME_MAC_WINDOW (f)) != noErr)
	  {
	    DisposeWindow (FRAME_MAC_WINDOW (f));
	    FRAME_MAC_WINDOW (f) = NULL;
	  }
    }
#else  /* !TARGET_API_MAC_CARBON */
  FRAME_MAC_WINDOW (f)
    = NewCWindow (NULL, &r, "\p", false, proc_id, behind, go_away_flag,
		  (long) f->output_data.mac);
#endif  /* !TARGET_API_MAC_CARBON */
  /* so that update events can find this mac_output struct */
  f->output_data.mac->mFP = f;  /* point back to emacs frame */

#ifndef MAC_OSX
  if (!tooltip_p)
    if (FRAME_MAC_WINDOW (f))
      {
	ControlRef root_control;

	if (CreateRootControl (FRAME_MAC_WINDOW (f), &root_control) != noErr)
	  {
	    DisposeWindow (FRAME_MAC_WINDOW (f));
	    FRAME_MAC_WINDOW (f) = NULL;
	  }
      }
#endif
}

/* Dispose of the Mac window of the frame F.  */

void
mac_dispose_frame_window (f)
     struct frame *f;
{
  WindowRef window = FRAME_MAC_WINDOW (f);

  if (window != tip_window)
    remove_window_handler (window);

#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (f);
#endif
  DisposeWindow (window);
}


/************************************************************************
			   View and Drawing
 ************************************************************************/

#if USE_CG_DRAWING
#define FRAME_CG_CONTEXT(f)	((f)->output_data.mac->cg_context)

CGContextRef
mac_begin_cg_clip (f, gc)
     struct frame *f;
     GC gc;
{
  CGContextRef context = FRAME_CG_CONTEXT (f);

  if (!context)
    {
      QDBeginCGContext (GetWindowPort (FRAME_MAC_WINDOW (f)), &context);
      FRAME_CG_CONTEXT (f) = context;
    }

  CGContextSaveGState (context);
  CGContextTranslateCTM (context, 0, FRAME_PIXEL_HEIGHT (f));
  CGContextScaleCTM (context, 1, -1);
  if (gc && gc->n_clip_rects)
    CGContextClipToRects (context, gc->clip_rects, gc->n_clip_rects);

  return context;
}

void
mac_end_cg_clip (f)
     struct frame *f;
{
  CGContextRestoreGState (FRAME_CG_CONTEXT (f));
}

static void
mac_prepare_for_quickdraw (f)
     struct frame *f;
{
  if (f == NULL)
    {
      Lisp_Object rest, frame;
      FOR_EACH_FRAME (rest, frame)
	if (FRAME_MAC_P (XFRAME (frame)))
	  mac_prepare_for_quickdraw (XFRAME (frame));
    }
  else
    {
      CGContextRef context = FRAME_CG_CONTEXT (f);

      if (context)
	{
	  CGContextSynchronize (context);
	  QDEndCGContext (GetWindowPort (FRAME_MAC_WINDOW (f)),
			  &FRAME_CG_CONTEXT (f));
	}
    }
}
#endif

static RgnHandle saved_port_clip_region = NULL;

void
mac_begin_clip (f, gc)
     struct frame *f;
     GC gc;
{
  static RgnHandle new_region = NULL;

  if (saved_port_clip_region == NULL)
    saved_port_clip_region = NewRgn ();
  if (new_region == NULL)
    new_region = NewRgn ();

#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (f);
#endif
  SetPortWindowPort (FRAME_MAC_WINDOW (f));

  if (gc && gc->n_clip_rects)
    {
      GetClip (saved_port_clip_region);
      SectRgn (saved_port_clip_region, gc->clip_region, new_region);
      SetClip (new_region);
    }
}

void
mac_end_clip (f, gc)
     struct frame *f;
     GC gc;
{
  if (gc && gc->n_clip_rects)
    SetClip (saved_port_clip_region);
}

#if TARGET_API_MAC_CARBON
/* Mac replacement for XCopyArea: used only for scrolling.  */

void
mac_scroll_area (f, gc, src_x, src_y, width, height, dest_x, dest_y)
     struct frame *f;
     GC gc;
     int src_x, src_y;
     unsigned int width, height;
     int dest_x, dest_y;
{
  Rect src_r;
  RgnHandle dummy = NewRgn ();	/* For avoiding update events.  */

  SetRect (&src_r, src_x, src_y, src_x + width, src_y + height);
#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (f);
#endif
  ScrollWindowRect (FRAME_MAC_WINDOW (f),
		    &src_r, dest_x - src_x, dest_y - src_y,
		    kScrollWindowNoOptions, dummy);
  DisposeRgn (dummy);
}
#endif


/************************************************************************
			     Scroll bars
 ************************************************************************/

extern struct scroll_bar *tracked_scroll_bar;
extern Lisp_Object last_mouse_scroll_bar;
extern Time last_mouse_movement_time;

static void x_scroll_bar_handle_click P_ ((struct scroll_bar *,
					   ControlPartCode,
					   const EventRecord *,
					   struct input_event *));
#ifndef USE_TOOLKIT_SCROLL_BARS
static void x_scroll_bar_note_movement P_ ((struct scroll_bar *, int, Time));
#else /* USE_TOOLKIT_SCROLL_BARS */
static void x_scroll_bar_handle_press P_ ((struct scroll_bar *,
					   ControlPartCode, Point,
					   struct input_event *));
static void x_scroll_bar_handle_release P_ ((struct scroll_bar *,
					     struct input_event *));
static void x_scroll_bar_handle_drag P_ ((WindowRef, struct scroll_bar *,
					  Point, struct input_event *));
static pascal void scroll_bar_timer_callback P_ ((EventLoopTimerRef, void *));
static OSStatus install_scroll_bar_timer P_ ((void));
static OSStatus set_scroll_bar_timer P_ ((EventTimerInterval));
static int control_part_code_to_scroll_bar_part P_ ((ControlPartCode));
static void construct_scroll_bar_click P_ ((struct scroll_bar *, int,
					    struct input_event *));
static OSStatus get_control_part_bounds P_ ((ControlRef, ControlPartCode,
					     Rect *));
static void update_scroll_bar_track_info P_ ((struct scroll_bar *));

/* Last scroll bar part sent in x_scroll_bar_handle_*.  */

static int last_scroll_bar_part;

static EventLoopTimerRef scroll_bar_timer;

static int scroll_bar_timer_event_posted_p;

#define SCROLL_BAR_FIRST_DELAY 0.5
#define SCROLL_BAR_CONTINUOUS_DELAY (1.0 / 15)

static pascal void
scroll_bar_timer_callback (timer, data)
     EventLoopTimerRef timer;
     void *data;
{
  OSStatus err;

  err = mac_post_mouse_moved_event ();
  if (err == noErr)
    scroll_bar_timer_event_posted_p = 1;
}

static OSStatus
install_scroll_bar_timer ()
{
  static EventLoopTimerUPP scroll_bar_timer_callbackUPP = NULL;

  if (scroll_bar_timer_callbackUPP == NULL)
    scroll_bar_timer_callbackUPP =
      NewEventLoopTimerUPP (scroll_bar_timer_callback);

  if (scroll_bar_timer == NULL)
    /* Mac OS X and CarbonLib 1.5 and later allow us to specify
       kEventDurationForever as delays.  */
    return
      InstallEventLoopTimer (GetCurrentEventLoop (),
			     kEventDurationForever, kEventDurationForever,
			     scroll_bar_timer_callbackUPP, NULL,
			     &scroll_bar_timer);
}

static OSStatus
set_scroll_bar_timer (delay)
     EventTimerInterval delay;
{
  if (scroll_bar_timer == NULL)
    install_scroll_bar_timer ();

  scroll_bar_timer_event_posted_p = 0;

  return SetEventLoopTimerNextFireTime (scroll_bar_timer, delay);
}

static int
control_part_code_to_scroll_bar_part (part_code)
     ControlPartCode part_code;
{
  switch (part_code)
    {
    case kControlUpButtonPart:		return scroll_bar_up_arrow;
    case kControlDownButtonPart:	return scroll_bar_down_arrow;
    case kControlPageUpPart:		return scroll_bar_above_handle;
    case kControlPageDownPart:		return scroll_bar_below_handle;
    case kControlIndicatorPart:		return scroll_bar_handle;
    }

  return -1;
}

static void
construct_scroll_bar_click (bar, part, bufp)
     struct scroll_bar *bar;
     int part;
     struct input_event *bufp;
{
  bufp->kind = SCROLL_BAR_CLICK_EVENT;
  bufp->frame_or_window = bar->window;
  bufp->arg = Qnil;
  bufp->part = part;
  bufp->code = 0;
  XSETINT (bufp->x, 0);
  XSETINT (bufp->y, 0);
  bufp->modifiers = 0;
}

static OSStatus
get_control_part_bounds (ch, part_code, rect)
     ControlRef ch;
     ControlPartCode part_code;
     Rect *rect;
{
  RgnHandle region = NewRgn ();
  OSStatus err;

  err = GetControlRegion (ch, part_code, region);
  if (err == noErr)
    GetRegionBounds (region, rect);
  DisposeRgn (region);

  return err;
}

static void
x_scroll_bar_handle_press (bar, part_code, mouse_pos, bufp)
     struct scroll_bar *bar;
     ControlPartCode part_code;
     Point mouse_pos;
     struct input_event *bufp;
{
  int part = control_part_code_to_scroll_bar_part (part_code);

  if (part < 0)
    return;

  if (part != scroll_bar_handle)
    {
      construct_scroll_bar_click (bar, part, bufp);
      HiliteControl (SCROLL_BAR_CONTROL_REF (bar), part_code);
      set_scroll_bar_timer (SCROLL_BAR_FIRST_DELAY);
      bar->dragging = Qnil;
    }
  else
    {
      Rect r;

      get_control_part_bounds (SCROLL_BAR_CONTROL_REF (bar),
			       kControlIndicatorPart, &r);
      XSETINT (bar->dragging, - (mouse_pos.v - r.top) - 1);
    }

  last_scroll_bar_part = part;
  tracked_scroll_bar = bar;
}

static void
x_scroll_bar_handle_release (bar, bufp)
     struct scroll_bar *bar;
     struct input_event *bufp;
{
  if (last_scroll_bar_part != scroll_bar_handle
      || (INTEGERP (bar->dragging) && XINT (bar->dragging) >= 0))
    construct_scroll_bar_click (bar, scroll_bar_end_scroll, bufp);

  HiliteControl (SCROLL_BAR_CONTROL_REF (bar), 0);
  set_scroll_bar_timer (kEventDurationForever);

  last_scroll_bar_part = -1;
  bar->dragging = Qnil;
  tracked_scroll_bar = NULL;
}

static void
x_scroll_bar_handle_drag (win, bar, mouse_pos, bufp)
     WindowRef win;
     struct scroll_bar *bar;
     Point mouse_pos;
     struct input_event *bufp;
{
  ControlRef ch = SCROLL_BAR_CONTROL_REF (bar);

  if (last_scroll_bar_part == scroll_bar_handle)
    {
      int top, top_range;
      Rect r;

      get_control_part_bounds (SCROLL_BAR_CONTROL_REF (bar),
			       kControlIndicatorPart, &r);

      if (INTEGERP (bar->dragging) && XINT (bar->dragging) < 0)
	XSETINT (bar->dragging, - (XINT (bar->dragging) + 1));

      top = mouse_pos.v - XINT (bar->dragging) - XINT (bar->track_top);
      top_range = XINT (bar->track_height) - XINT (bar->min_handle);

      if (top < 0)
	top = 0;
      if (top > top_range)
	top = top_range;

      construct_scroll_bar_click (bar, scroll_bar_handle, bufp);
      XSETINT (bufp->x, top);
      XSETINT (bufp->y, top_range);
    }
  else
    {
      ControlPartCode part_code;
      int unhilite_p = 0, part;

      if (ch != FindControlUnderMouse (mouse_pos, win, &part_code))
	unhilite_p = 1;
      else
	{
	  part = control_part_code_to_scroll_bar_part (part_code);

	  switch (last_scroll_bar_part)
	    {
	    case scroll_bar_above_handle:
	    case scroll_bar_below_handle:
	      if (part != scroll_bar_above_handle
		  && part != scroll_bar_below_handle)
		unhilite_p = 1;
	      break;

	    case scroll_bar_up_arrow:
	    case scroll_bar_down_arrow:
	      if (part != scroll_bar_up_arrow
		  && part != scroll_bar_down_arrow)
		unhilite_p = 1;
	      break;
	    }
	}

      if (unhilite_p)
	HiliteControl (SCROLL_BAR_CONTROL_REF (bar), 0);
      else if (part != last_scroll_bar_part
	       || scroll_bar_timer_event_posted_p)
	{
	  construct_scroll_bar_click (bar, part, bufp);
	  last_scroll_bar_part = part;
	  HiliteControl (SCROLL_BAR_CONTROL_REF (bar), part_code);
	  set_scroll_bar_timer (SCROLL_BAR_CONTINUOUS_DELAY);
	}
    }
}

/* Update BAR->track_top, BAR->track_height, and BAR->min_handle for
   the scroll bar BAR.  This function should be called when the bounds
   of the scroll bar is changed.  */

static void
update_scroll_bar_track_info (bar)
     struct scroll_bar *bar;
{
  ControlRef ch = SCROLL_BAR_CONTROL_REF (bar);
  Rect r0, r1;

  GetControlBounds (ch, &r0);

  if (r0.right - r0.left >= r0.bottom - r0.top
#ifdef MAC_OSX
      || r0.right - r0.left < MAC_AQUA_SMALL_VERTICAL_SCROLL_BAR_WIDTH
#endif
      )
    {
      XSETINT (bar->track_top, 0);
      XSETINT (bar->track_height, 0);
      XSETINT (bar->min_handle, 0);
    }
  else
    {
      BLOCK_INPUT;

      SetControl32BitMinimum (ch, 0);
      SetControl32BitMaximum (ch, 1 << 30);
      SetControlViewSize (ch, 1);

      /* Move the scroll bar thumb to the top.  */
      SetControl32BitValue (ch, 0);
      get_control_part_bounds (ch, kControlIndicatorPart, &r0);

      /* Move the scroll bar thumb to the bottom.  */
      SetControl32BitValue (ch, 1 << 30);
      get_control_part_bounds (ch, kControlIndicatorPart, &r1);

      UnionRect (&r0, &r1, &r0);
      XSETINT (bar->track_top, r0.top);
      XSETINT (bar->track_height, r0.bottom - r0.top);
      XSETINT (bar->min_handle, r1.bottom - r1.top);

      /* Don't show the scroll bar if its height is not enough to
	 display the scroll bar thumb.  */
      if (r0.bottom - r0.top > 0)
	ShowControl (ch);

      UNBLOCK_INPUT;
    }
}

/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

void
x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole)
     struct scroll_bar *bar;
     int portion, position, whole;
{
  ControlRef ch = SCROLL_BAR_CONTROL_REF (bar);
  int value, viewsize, maximum;

  if (XINT (bar->track_height) == 0)
    return;

  if (whole <= portion)
    value = 0, viewsize = 1, maximum = 0;
  else
    {
      float scale;

      maximum = XINT (bar->track_height) - XINT (bar->min_handle);
      scale = (float) maximum / (whole - portion);
      value = position * scale + 0.5f;
      viewsize = (int) (portion * scale + 0.5f) + XINT (bar->min_handle);
    }

  BLOCK_INPUT;

  if (GetControlViewSize (ch) != viewsize
      || GetControl32BitValue (ch) != value
      || GetControl32BitMaximum (ch) != maximum)
    {
      /* Temporarily hide the scroll bar to avoid multiple redraws.  */
      SetControlVisibility (ch, false, false);

      SetControl32BitMaximum (ch, maximum);
      SetControl32BitValue (ch, value);
      SetControlViewSize (ch, viewsize);

      SetControlVisibility (ch, true, true);
    }

  UNBLOCK_INPUT;
}

#endif /* USE_TOOLKIT_SCROLL_BARS */

/* Create a scroll bar control for BAR.  BOUNDS and VISIBLE specifies
   the initial geometry and visibility, respectively.  The created
   control is stored in some members of BAR.  */

void
mac_create_scroll_bar (bar, bounds, visible)
     struct scroll_bar *bar;
     const Rect *bounds;
     Boolean visible;
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  ControlRef ch;

#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (f);
#endif
  ch = NewControl (FRAME_MAC_WINDOW (f), bounds, "\p", visible, 0, 0, 0,
#if TARGET_API_MAC_CARBON
		   kControlScrollBarProc,
#else
		   scrollBarProc,
#endif
		   (SInt32) bar);
  SET_SCROLL_BAR_CONTROL_REF (bar, ch);

  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

#ifdef USE_TOOLKIT_SCROLL_BARS
  update_scroll_bar_track_info (bar);
#endif
}

/* Dispose of the scroll bar control stored in some members of
   BAR.  */

void
mac_dispose_scroll_bar (bar)
     struct scroll_bar *bar;
{
#if USE_CG_DRAWING
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  mac_prepare_for_quickdraw (f);
#endif
  DisposeControl (SCROLL_BAR_CONTROL_REF (bar));
}

/* Set bounds of the scroll bar BAR to BOUNDS.  */

void
mac_set_scroll_bar_bounds (bar, bounds)
     struct scroll_bar *bar;
     const Rect *bounds;
{
  ControlRef ch = SCROLL_BAR_CONTROL_REF (bar);
  SInt16 width, height;
#if USE_CG_DRAWING
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  mac_prepare_for_quickdraw (f);
#endif

  width = bounds->right - bounds->left;
  height = bounds->bottom - bounds->top;
  HideControl (ch);
  MoveControl (ch, bounds->left, bounds->top);
  SizeControl (ch, width, height);
#ifdef USE_TOOLKIT_SCROLL_BARS
  update_scroll_bar_track_info (bar);
#else
  if (width < height)
    ShowControl (ch);
#endif
}

/* Draw the scroll bar BAR.  */

void
mac_redraw_scroll_bar (bar)
     struct scroll_bar *bar;
{
#if USE_CG_DRAWING
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  mac_prepare_for_quickdraw (f);
#endif
  Draw1Control (SCROLL_BAR_CONTROL_REF (bar));
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than NO_EVENT, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_handle_click (bar, part_code, er, bufp)
     struct scroll_bar *bar;
     ControlPartCode part_code;
     const EventRecord *er;
     struct input_event *bufp;
{
  int win_y, top_range;

  if (! GC_WINDOWP (bar->window))
    abort ();

  bufp->kind = SCROLL_BAR_CLICK_EVENT;
  bufp->frame_or_window = bar->window;
  bufp->arg = Qnil;

  bar->dragging = Qnil;

  switch (part_code)
    {
    case kControlUpButtonPart:
      bufp->part = scroll_bar_up_arrow;
      break;
    case kControlDownButtonPart:
      bufp->part = scroll_bar_down_arrow;
      break;
    case kControlPageUpPart:
      bufp->part = scroll_bar_above_handle;
      break;
    case kControlPageDownPart:
      bufp->part = scroll_bar_below_handle;
      break;
#if TARGET_API_MAC_CARBON
    default:
#else
    case kControlIndicatorPart:
#endif
      if (er->what == mouseDown)
        bar->dragging = make_number (0);
      XSETVECTOR (last_mouse_scroll_bar, bar);
      bufp->part = scroll_bar_handle;
      break;
    }

  win_y = XINT (bufp->y) - XINT (bar->top);
  top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (0/*dummy*/, XINT (bar->height));

  win_y -= VERTICAL_SCROLL_BAR_TOP_BORDER;

  win_y -= 24;

  if (! NILP (bar->dragging))
    win_y -= XINT (bar->dragging);

  if (win_y < 0)
    win_y = 0;
  if (win_y > top_range)
    win_y = top_range;

  XSETINT (bufp->x, win_y);
  XSETINT (bufp->y, top_range);
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */

void
x_scroll_bar_report_motion (fp, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  ControlRef ch = SCROLL_BAR_CONTROL_REF (bar);
#if TARGET_API_MAC_CARBON
  WindowRef wp = GetControlOwner (ch);
#else
  WindowRef wp = (*ch)->contrlOwner;
#endif
  Point mouse_pos;
  struct frame *f = mac_window_to_frame (wp);
  int win_y, top_range;

#if TARGET_API_MAC_CARBON
  GetGlobalMouse (&mouse_pos);
  mouse_pos.h -= f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  mouse_pos.v -= f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);
#else
  SetPortWindowPort (wp);
  GetMouse (&mouse_pos);
#endif

  win_y = mouse_pos.v - XINT (bar->top);
  top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));

  win_y -= VERTICAL_SCROLL_BAR_TOP_BORDER;

  win_y -= 24;

  if (! NILP (bar->dragging))
    win_y -= XINT (bar->dragging);

  if (win_y < 0)
    win_y = 0;
  if (win_y > top_range)
    win_y = top_range;

  *fp = f;
  *bar_window = bar->window;

  if (! NILP (bar->dragging))
    *part = scroll_bar_handle;
  else if (win_y < XINT (bar->start))
    *part = scroll_bar_above_handle;
  else if (win_y < XINT (bar->end) + VERTICAL_SCROLL_BAR_MIN_HANDLE)
    *part = scroll_bar_handle;
  else
    *part = scroll_bar_below_handle;

  XSETINT (*x, win_y);
  XSETINT (*y, top_range);

  f->mouse_moved = 0;
  last_mouse_scroll_bar = Qnil;

  *time = last_mouse_movement_time;
}

#ifndef USE_TOOLKIT_SCROLL_BARS
/* Draw BAR's handle in the proper position.

   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD is non-zero; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll
   bar handle, we want to let them drag it down all the way, so that
   the bar's top is as far down as it goes; otherwise, there's no way
   to move to the very end of the buffer.  */

void
x_scroll_bar_set_handle (bar, start, end, rebuild)
     struct scroll_bar *bar;
     int start, end;
     int rebuild;
{
  int dragging = ! NILP (bar->dragging);
  ControlRef ch = SCROLL_BAR_CONTROL_REF (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, XINT (bar->height));
  int length = end - start;

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  BLOCK_INPUT;

  /* Make sure the values are reasonable, and try to preserve the
     distance between start and end.  */
  if (start < 0)
    start = 0;
  else if (start > top_range)
    start = top_range;
  end = start + length;

  if (end < start)
    end = start;
  else if (end > top_range && ! dragging)
    end = top_range;

  /* Store the adjusted setting in the scroll bar.  */
  XSETINT (bar->start, start);
  XSETINT (bar->end, end);

  /* Clip the end position, just for display.  */
  if (end > top_range)
    end = top_range;

  /* Draw bottom positions VERTICAL_SCROLL_BAR_MIN_HANDLE pixels below
     top positions, to make sure the handle is always at least that
     many pixels tall.  */
  end += VERTICAL_SCROLL_BAR_MIN_HANDLE;

  SetControlMinimum (ch, 0);
  /* Don't inadvertently activate deactivated scroll bars */
  if (GetControlMaximum (ch) != -1)
    SetControlMaximum (ch, top_range + VERTICAL_SCROLL_BAR_MIN_HANDLE
		       - (end - start));
  SetControlValue (ch, start);
#if TARGET_API_MAC_CARBON
  SetControlViewSize (ch, end - start);
#endif

  UNBLOCK_INPUT;
}

/* Handle some mouse motion while someone is dragging the scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_note_movement (bar, y_pos, t)
     struct scroll_bar *bar;
     int y_pos;
     Time t;
{
  FRAME_PTR f = XFRAME (XWINDOW (bar->window)->frame);

  last_mouse_movement_time = t;

  f->mouse_moved = 1;
  XSETVECTOR (last_mouse_scroll_bar, bar);

  /* If we're dragging the bar, display it.  */
  if (! GC_NILP (bar->dragging))
    {
      /* Where should the handle be now?  */
      int new_start = y_pos - 24;

      if (new_start != XINT (bar->start))
	{
	  int new_end = new_start + (XINT (bar->end) - XINT (bar->start));

	  x_scroll_bar_set_handle (bar, new_start, new_end, 0);
	}
    }
}
#endif /* !USE_TOOLKIT_SCROLL_BARS */


/***********************************************************************
			       Tool-bars
 ***********************************************************************/

#if USE_MAC_TOOLBAR
/* In identifiers such as function/variable names, Emacs tool bar is
   referred to as `tool_bar', and Carbon HIToolbar as `toolbar'.  */

#define TOOLBAR_IDENTIFIER (CFSTR ("org.gnu.Emacs.toolbar"))
#define TOOLBAR_ICON_ITEM_IDENTIFIER (CFSTR ("org.gnu.Emacs.toolbar.icon"))

#define TOOLBAR_ITEM_COMMAND_ID_OFFSET 'Tb\0\0'
#define TOOLBAR_ITEM_COMMAND_ID_P(id)			\
  (((id) & ~0xffff) == TOOLBAR_ITEM_COMMAND_ID_OFFSET)
#define TOOLBAR_ITEM_COMMAND_ID_VALUE(id)	\
  ((id) - TOOLBAR_ITEM_COMMAND_ID_OFFSET)
#define TOOLBAR_ITEM_MAKE_COMMAND_ID(value)	\
  ((value) + TOOLBAR_ITEM_COMMAND_ID_OFFSET)

static OSStatus mac_handle_toolbar_command_event P_ ((EventHandlerCallRef,
						      EventRef, void *));

extern Rect last_mouse_glyph;

extern void mac_move_window_with_gravity P_ ((struct frame *, int,
					      short, short));
extern void mac_get_window_origin_with_gravity P_ ((struct frame *, int,
						    short *, short *));
extern CGImageRef mac_image_spec_to_cg_image P_ ((struct frame *,
						  Lisp_Object));

static OSStatus
mac_handle_toolbar_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus result = eventNotHandledErr;

  switch (GetEventKind (event))
    {
    case kEventToolbarGetDefaultIdentifiers:
      result = noErr;
      break;

    case kEventToolbarGetAllowedIdentifiers:
      {
	CFMutableArrayRef array;

	GetEventParameter (event, kEventParamMutableArray,
			   typeCFMutableArrayRef, NULL,
			   sizeof (CFMutableArrayRef), NULL, &array);
	CFArrayAppendValue (array, TOOLBAR_ICON_ITEM_IDENTIFIER);
	result = noErr;
      }
      break;

    case kEventToolbarCreateItemWithIdentifier:
      {
	CFStringRef identifier;
	HIToolbarItemRef item = NULL;

	GetEventParameter (event, kEventParamToolbarItemIdentifier,
			   typeCFStringRef, NULL,
			   sizeof (CFStringRef), NULL, &identifier);

	if (CFStringCompare (identifier, TOOLBAR_ICON_ITEM_IDENTIFIER, 0)
	    == kCFCompareEqualTo)
	  HIToolbarItemCreate (identifier,
			       kHIToolbarItemAllowDuplicates
			       | kHIToolbarItemCantBeRemoved, &item);

	if (item)
	  {
	    SetEventParameter (event, kEventParamToolbarItem,
			       typeHIToolbarItemRef,
			       sizeof (HIToolbarItemRef), &item);
	    result = noErr;
	  }
      }
      break;

    default:
      abort ();
    }

  return result;
}

/* Create a tool bar for frame F.  */

static OSStatus
mac_create_frame_tool_bar (f)
     FRAME_PTR f;
{
  OSStatus err;
  HIToolbarRef toolbar;

  err = HIToolbarCreate (TOOLBAR_IDENTIFIER, kHIToolbarNoAttributes,
			 &toolbar);
  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassToolbar, kEventToolbarGetDefaultIdentifiers},
	 {kEventClassToolbar, kEventToolbarGetAllowedIdentifiers},
	 {kEventClassToolbar, kEventToolbarCreateItemWithIdentifier}};

      err = InstallEventHandler (HIObjectGetEventTarget (toolbar),
				 mac_handle_toolbar_event,
				 GetEventTypeCount (specs), specs,
				 f, NULL);
    }

  if (err == noErr)
    err = HIToolbarSetDisplayMode (toolbar, kHIToolbarDisplayModeIconOnly);
  if (err == noErr)
    {
      static const EventTypeSpec specs[] =
	{{kEventClassCommand, kEventCommandProcess}};

      err = InstallWindowEventHandler (FRAME_MAC_WINDOW (f),
				       mac_handle_toolbar_command_event,
				       GetEventTypeCount (specs),
				       specs, f, NULL);
    }
  if (err == noErr)
    err = SetWindowToolbar (FRAME_MAC_WINDOW (f), toolbar);

  if (toolbar)
    CFRelease (toolbar);

  return err;
}

/* Update the tool bar for frame F.  Add new buttons and remove old.  */

void
update_frame_tool_bar (f)
     FRAME_PTR f;
{
  HIToolbarRef toolbar = NULL;
  short left, top;
  CFArrayRef old_items = NULL;
  CFIndex old_count;
  int i, pos, win_gravity = f->output_data.mac->toolbar_win_gravity;
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

  BLOCK_INPUT;

  GetWindowToolbar (FRAME_MAC_WINDOW (f), &toolbar);
  if (toolbar == NULL)
    {
      mac_create_frame_tool_bar (f);
      GetWindowToolbar (FRAME_MAC_WINDOW (f), &toolbar);
      if (toolbar == NULL)
	goto out;
      if (win_gravity >= NorthWestGravity && win_gravity <= SouthEastGravity)
	mac_get_window_origin_with_gravity (f, win_gravity, &left, &top);
    }

  HIToolbarCopyItems (toolbar, &old_items);
  if (old_items == NULL)
    goto out;

  old_count = CFArrayGetCount (old_items);
  pos = 0;
  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define PROP(IDX) AREF (f->tool_bar_items, i * TOOL_BAR_ITEM_NSLOTS + (IDX))

      int enabled_p = !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P));
      int selected_p = !NILP (PROP (TOOL_BAR_ITEM_SELECTED_P));
      int idx;
      Lisp_Object image;
      CGImageRef cg_image;
      CFStringRef label;
      HIToolbarItemRef item;

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = PROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
	  if (enabled_p)
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_ENABLED_SELECTED
		   : TOOL_BAR_IMAGE_ENABLED_DESELECTED);
	  else
	    idx = (selected_p
		   ? TOOL_BAR_IMAGE_DISABLED_SELECTED
		   : TOOL_BAR_IMAGE_DISABLED_DESELECTED);

	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
	idx = -1;

      cg_image = mac_image_spec_to_cg_image (f, image);
      /* Ignore invalid image specifications.  */
      if (cg_image == NULL)
	continue;

      label = cfstring_create_with_string (PROP (TOOL_BAR_ITEM_CAPTION));
      if (label == NULL)
	label = CFSTR ("");

      if (pos < old_count)
	{
	  CGImageRef old_cg_image = NULL;
	  CFStringRef old_label = NULL;
	  Boolean old_enabled_p;

	  item = (HIToolbarItemRef) CFArrayGetValueAtIndex (old_items, pos);

	  HIToolbarItemCopyImage (item, &old_cg_image);
	  if (cg_image != old_cg_image)
	    HIToolbarItemSetImage (item, cg_image);
	  CGImageRelease (old_cg_image);

	  HIToolbarItemCopyLabel (item, &old_label);
	  if (CFStringCompare (label, old_label, 0) != kCFCompareEqualTo)
	    HIToolbarItemSetLabel (item, label);
	  CFRelease (old_label);

	  old_enabled_p = HIToolbarItemIsEnabled (item);
	  if ((enabled_p || idx >= 0) != old_enabled_p)
	    HIToolbarItemSetEnabled (item, (enabled_p || idx >= 0));
	}
      else
	{
	  item = NULL;
	  HIToolbarCreateItemWithIdentifier (toolbar,
					     TOOLBAR_ICON_ITEM_IDENTIFIER,
					     NULL, &item);
	  if (item)
	    {
	      HIToolbarItemSetImage (item, cg_image);
	      HIToolbarItemSetLabel (item, label);
	      HIToolbarItemSetEnabled (item, (enabled_p || idx >= 0));
	      HIToolbarAppendItem (toolbar, item);
	      CFRelease (item);
	    }
	}

      CFRelease (label);
      if (item)
	{
	  HIToolbarItemSetCommandID (item, TOOLBAR_ITEM_MAKE_COMMAND_ID (i));
	  pos++;
	}
    }

  CFRelease (old_items);

  while (pos < old_count)
    HIToolbarRemoveItemAtIndex (toolbar, --old_count);

  ShowHideWindowToolbar (FRAME_MAC_WINDOW (f), true,
			 !win_gravity && f == mac_focus_frame (dpyinfo));
  /* Mac OS X 10.3 does not issue kEventWindowBoundsChanged events on
     toolbar visibility change.  */
  mac_handle_origin_change (f);
  if (win_gravity >= NorthWestGravity && win_gravity <= SouthEastGravity)
    {
      mac_move_window_with_gravity (f, win_gravity, left, top);
      /* If the title bar is completely outside the screen, adjust the
	 position. */
      ConstrainWindowToScreen (FRAME_MAC_WINDOW (f), kWindowTitleBarRgn,
			       kWindowConstrainMoveRegardlessOfFit
			       | kWindowConstrainAllowPartial, NULL, NULL);
      f->output_data.mac->toolbar_win_gravity = 0;
    }

 out:
  UNBLOCK_INPUT;
}

/* Hide the tool bar on frame F.  Unlike the counterpart on GTK+, it
   doesn't deallocate the resources.  */

void
free_frame_tool_bar (f)
     FRAME_PTR f;
{
  if (IsWindowToolbarVisible (FRAME_MAC_WINDOW (f)))
    {
      struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

      BLOCK_INPUT;
      ShowHideWindowToolbar (FRAME_MAC_WINDOW (f), false,
			     (NILP (find_symbol_value
				    (intern ("frame-notice-user-settings")))
			      && f == mac_focus_frame (dpyinfo)));
      /* Mac OS X 10.3 does not issue kEventWindowBoundsChanged events
	 on toolbar visibility change.  */
      mac_handle_origin_change (f);
      UNBLOCK_INPUT;
    }
}

/* Report a mouse movement over toolbar to the mainstream Emacs
   code.  */

static void
mac_tool_bar_note_mouse_movement (f, event)
     struct frame *f;
     EventRef event;
{
  OSStatus err;
  struct mac_display_info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);
  int mouse_down_p;
  WindowRef window;
  WindowPartCode part_code;
  HIViewRef item_view;
  UInt32 command_id;

  mouse_down_p = (dpyinfo->grabbed
		  && f == last_mouse_frame
		  && FRAME_LIVE_P (f));
  if (mouse_down_p)
    return;

  err = GetEventParameter (event, kEventParamWindowRef, typeWindowRef, NULL,
			   sizeof (WindowRef), NULL, &window);
  if (err != noErr || window != FRAME_MAC_WINDOW (f))
    return;

  err = GetEventParameter (event, kEventParamWindowPartCode,
			   typeWindowPartCode, NULL,
			   sizeof (WindowPartCode), NULL, &part_code);
  if (err != noErr || part_code != inStructure)
    return;

  err = HIViewGetViewForMouseEvent (HIViewGetRoot (window), event, &item_view);
  /* This doesn't work on Mac OS X 10.2.  On Mac OS X 10.3 and 10.4, a
     toolbar item view seems to have the same command ID with that of
     the toolbar item.  */
  if (err == noErr)
    err = GetControlCommandID (item_view, &command_id);
  if (err == noErr && TOOLBAR_ITEM_COMMAND_ID_P (command_id))
    {
      int i = TOOLBAR_ITEM_COMMAND_ID_VALUE (command_id);

      if (i < f->n_tool_bar_items)
	{
	  HIRect bounds;
	  HIViewRef content_view;

	  err = HIViewGetBounds (item_view, &bounds);
	  if (err == noErr)
	    err = HIViewFindByID (HIViewGetRoot (window),
				  kHIViewWindowContentID, &content_view);
	  if (err == noErr)
	    err = HIViewConvertRect (&bounds, item_view, content_view);
	  if (err == noErr)
	    SetRect (&last_mouse_glyph,
		     CGRectGetMinX (bounds), CGRectGetMinY (bounds),
		     CGRectGetMaxX (bounds), CGRectGetMaxY (bounds));

	  help_echo_object = help_echo_window = Qnil;
	  help_echo_pos = -1;
	  help_echo_string = PROP (TOOL_BAR_ITEM_HELP);
	  if (NILP (help_echo_string))
	    help_echo_string = PROP (TOOL_BAR_ITEM_CAPTION);
	}
    }
}

static OSStatus
mac_handle_toolbar_command_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result = eventNotHandledErr;
  struct frame *f = (struct frame *) data;
  HICommand command;

  err = GetEventParameter (event, kEventParamDirectObject,
			   typeHICommand, NULL,
			   sizeof (HICommand), NULL, &command);
  if (err != noErr)
    return result;

  switch (GetEventKind (event))
    {
    case kEventCommandProcess:
      if (!TOOLBAR_ITEM_COMMAND_ID_P (command.commandID))
	result = CallNextEventHandler (next_handler, event);
      else
	{
	  int i = TOOLBAR_ITEM_COMMAND_ID_VALUE (command.commandID);

	  if (i < f->n_tool_bar_items
	      && !NILP (PROP (TOOL_BAR_ITEM_ENABLED_P)))
	    {
	      Lisp_Object frame;
	      struct input_event buf;

	      EVENT_INIT (buf);

	      XSETFRAME (frame, f);
	      buf.kind = TOOL_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = frame;
	      kbd_buffer_store_event (&buf);

	      buf.kind = TOOL_BAR_EVENT;
	      buf.frame_or_window = frame;
	      buf.arg = PROP (TOOL_BAR_ITEM_KEY);
	      buf.modifiers = mac_event_to_emacs_modifiers (event);
	      kbd_buffer_store_event (&buf);

	      result = noErr;
	    }
	}
      break;

    default:
      abort ();
    }
#undef PROP

  return result;
}
#endif	/* USE_MAC_TOOLBAR */


/***********************************************************************
			      Font Panel
 ***********************************************************************/

#if USE_MAC_FONT_PANEL
/* Whether Font Panel has been shown before.  The first call to font
   panel functions (FPIsFontPanelVisible, SetFontInfoForSelection) is
   slow.  This variable is used for deferring such a call as much as
   possible.  */
static int font_panel_shown_p = 0;

extern Lisp_Object Qpanel_closed, Qselection;
extern Lisp_Object Qfont;

/* Whether the font panel is currently visible.  */

int
mac_font_panel_visible_p ()
{
  return font_panel_shown_p && FPIsFontPanelVisible ();
}

static pascal OSStatus
mac_handle_font_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus result, err;
  Lisp_Object id_key;
  int num_params;
  const EventParamName *names;
  const EventParamType *types;
  static const EventParamName names_sel[] = {kEventParamATSUFontID,
					     kEventParamATSUFontSize,
					     kEventParamFMFontFamily,
					     kEventParamFMFontStyle,
					     kEventParamFMFontSize,
					     kEventParamFontColor};
  static const EventParamType types_sel[] = {typeATSUFontID,
					     typeATSUSize,
					     typeFMFontFamily,
					     typeFMFontStyle,
					     typeFMFontSize,
					     typeFontColor};

  result = CallNextEventHandler (next_handler, event);
  if (result != eventNotHandledErr)
    return result;

  switch (GetEventKind (event))
    {
    case kEventFontPanelClosed:
      id_key = Qpanel_closed;
      num_params = 0;
      names = NULL;
      types = NULL;
      break;

    case kEventFontSelection:
      id_key = Qselection;
      num_params = sizeof (names_sel) / sizeof (names_sel[0]);
      names = names_sel;
      types = types_sel;
      break;
    }

  err = mac_store_event_ref_as_apple_event (0, 0, Qfont, id_key,
					    event, num_params,
					    names, types);
  if (err == noErr)
    result = noErr;

  return result;
}

/* Toggle visiblity of the font panel.  */

OSStatus
mac_show_hide_font_panel ()
{
  if (!font_panel_shown_p)
    {
      OSStatus err;

      static const EventTypeSpec specs[] =
	{{kEventClassFont, kEventFontPanelClosed},
	 {kEventClassFont, kEventFontSelection}};

      err = InstallApplicationEventHandler (mac_handle_font_event,
					    GetEventTypeCount (specs),
					    specs, NULL, NULL);
      if (err != noErr)
	return err;

      font_panel_shown_p = 1;
    }

  return FPShowHideFontPanel ();
}

/* Set the font selected in the font panel to the one corresponding to
   the face FACE_ID and the charcacter C in the frame F.  */

OSStatus
mac_set_font_info_for_selection (f, face_id, c)
     struct frame *f;
     int face_id, c;
{
  OSStatus err;
  EventTargetRef target = NULL;
  XFontStruct *font = NULL;

  if (!mac_font_panel_visible_p ())
    return noErr;

  if (f)
    {
      target = GetWindowEventTarget (FRAME_MAC_WINDOW (f));

      if (FRAME_FACE_CACHE (f) && CHAR_VALID_P (c, 0))
	{
	  struct face *face;

	  face_id = FACE_FOR_CHAR (f, FACE_FROM_ID (f, face_id), c);
	  face = FACE_FROM_ID (f, face_id);
	  font = face->font;
	}
    }

  if (font == NULL)
    err = SetFontInfoForSelection (kFontSelectionATSUIType, 0, NULL, target);
  else
    {
      if (font->mac_fontnum != -1)
	{
	  FontSelectionQDStyle qd_style;

	  qd_style.version = kFontSelectionQDStyleVersionZero;
	  qd_style.instance.fontFamily = font->mac_fontnum;
	  qd_style.instance.fontStyle = font->mac_fontface;
	  qd_style.size = font->mac_fontsize;
	  qd_style.hasColor = false;

	  err = SetFontInfoForSelection (kFontSelectionQDType,
					 1, &qd_style, target);
	}
      else
	err = SetFontInfoForSelection (kFontSelectionATSUIType,
				       1, &font->mac_style, target);
    }

  return err;
}
#endif	/* USE_MAC_FONT_PANEL */


/************************************************************************
			    Event Handling
 ************************************************************************/

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static int any_help_event_p;

/* Last window where we saw the mouse.  Used by mouse-autoselect-window.  */
static Lisp_Object last_window;

static Point saved_menu_event_location;

extern struct frame *pending_autoraise_frame;

extern FRAME_PTR last_mouse_glyph_frame;

#ifdef __STDC__
extern int volatile input_signal_count;
#else
extern int input_signal_count;
#endif

extern int mac_screen_config_changed;

extern Lisp_Object Vmac_emulate_three_button_mouse;
#if TARGET_API_MAC_CARBON
extern int mac_wheel_button_is_mouse_2;
extern int mac_pass_command_to_system;
extern int mac_pass_control_to_system;
#endif	/* TARGET_API_MAC_CARBON */
extern int mac_ready_for_apple_events;

extern void mac_focus_changed P_ ((int, struct mac_display_info *,
				   struct frame *, struct input_event *));
extern int mac_get_emulated_btn P_ ((UInt32));
extern int note_mouse_movement P_ ((FRAME_PTR, Point *));
extern void mac_get_screen_info P_ ((struct mac_display_info *));

/* The focus may have changed.  Figure out if it is a real focus change,
   by checking both FocusIn/Out and Enter/LeaveNotify events.

   Returns FOCUS_IN_EVENT event in *BUFP. */

static void
x_detect_focus_change (dpyinfo, event, bufp)
     struct mac_display_info *dpyinfo;
     const EventRecord *event;
     struct input_event *bufp;
{
  struct frame *frame;

  frame = mac_window_to_frame ((WindowRef) event->message);
  if (! frame)
    return;

  /* On Mac, this is only called from focus events, so no switch needed.  */
  mac_focus_changed ((event->modifiers & activeFlag),
		     dpyinfo, frame, bufp);
}

#if TARGET_API_MAC_CARBON
/* Obtains the event modifiers from the event EVENTREF and then calls
   mac_to_emacs_modifiers.  */

static int
mac_event_to_emacs_modifiers (EventRef eventRef)
{
  UInt32 mods = 0, class;

  GetEventParameter (eventRef, kEventParamKeyModifiers, typeUInt32, NULL,
		    sizeof (UInt32), NULL, &mods);
  class = GetEventClass (eventRef);
  if (!NILP (Vmac_emulate_three_button_mouse)
      && (class == kEventClassMouse || class == kEventClassCommand))
    {
      mods &= ~(optionKey | cmdKey);
    }
  return mac_to_emacs_modifiers (mods, 0);
}

/* Given an event REF, return the code to use for the mouse button
   code in the emacs input_event.  */

static int
mac_get_mouse_btn (EventRef ref)
{
  EventMouseButton result = kEventMouseButtonPrimary;
  GetEventParameter (ref, kEventParamMouseButton, typeMouseButton, NULL,
		    sizeof (EventMouseButton), NULL, &result);
  switch (result)
    {
    case kEventMouseButtonPrimary:
      if (NILP (Vmac_emulate_three_button_mouse))
	return 0;
      else {
	UInt32 mods = 0;
	GetEventParameter (ref, kEventParamKeyModifiers, typeUInt32, NULL,
			   sizeof (UInt32), NULL, &mods);
	return mac_get_emulated_btn(mods);
      }
    case kEventMouseButtonSecondary:
      return mac_wheel_button_is_mouse_2 ? 2 : 1;
    case kEventMouseButtonTertiary:
    case 4:  /* 4 is the number for the mouse wheel button */
      return mac_wheel_button_is_mouse_2 ? 1 : 2;
    default:
      return 0;
    }
}

/* Normally, ConvertEventRefToEventRecord will correctly handle all
   events.  However the click of the mouse wheel is not converted to a
   mouseDown or mouseUp event.  Likewise for dead key events.  This
   calls ConvertEventRefToEventRecord, but then checks to see if it is
   a mouse up/down, or a dead key Carbon event that has not been
   converted, and if so, converts it by hand (to be picked up in the
   XTread_socket loop).  */
static Boolean mac_convert_event_ref (EventRef eventRef, EventRecord *eventRec)
{
  OSStatus err;
  Boolean result = ConvertEventRefToEventRecord (eventRef, eventRec);
  EventKind action;

  if (result)
    return result;

  switch (GetEventClass (eventRef))
    {
    case kEventClassMouse:
      switch (GetEventKind (eventRef))
	{
	case kEventMouseDown:
	  eventRec->what = mouseDown;
	  result = 1;
	  break;

	case kEventMouseUp:
	  eventRec->what = mouseUp;
	  result = 1;
	  break;

	default:
	  break;
	}
      break;

    case kEventClassKeyboard:
      switch (GetEventKind (eventRef))
	{
	case kEventRawKeyDown:
	  action = keyDown;
	  goto keystroke_common;
	case kEventRawKeyRepeat:
	  action = autoKey;
	  goto keystroke_common;
	case kEventRawKeyUp:
	  action = keyUp;
	keystroke_common:
	  {
	    unsigned char char_codes;
	    UInt32 key_code;

	    err = GetEventParameter (eventRef, kEventParamKeyMacCharCodes,
				     typeChar, NULL, sizeof (char),
				     NULL, &char_codes);
	    if (err == noErr)
	      err = GetEventParameter (eventRef, kEventParamKeyCode,
				       typeUInt32, NULL, sizeof (UInt32),
				       NULL, &key_code);
	    if (err == noErr)
	      {
		eventRec->what = action;
		eventRec->message = char_codes | ((key_code & 0xff) << 8);
		result = 1;
	      }
	  }
	  break;

	default:
	  break;
	}
      break;

    default:
      break;
    }

  if (result)
    {
      /* Need where and when.  */
      UInt32 mods = 0;

      GetEventParameter (eventRef, kEventParamMouseLocation, typeQDPoint,
			 NULL, sizeof (Point), NULL, &eventRec->where);
      /* Use two step process because new event modifiers are 32-bit
	 and old are 16-bit.  Currently, only loss is NumLock & Fn. */
      GetEventParameter (eventRef, kEventParamKeyModifiers, typeUInt32,
			 NULL, sizeof (UInt32), NULL, &mods);
      eventRec->modifiers = mods;

      eventRec->when = EventTimeToTicks (GetEventTime (eventRef));
    }

  return result;
}
#endif	/* TARGET_API_MAC_CARBON */

#if !TARGET_API_MAC_CARBON
static RgnHandle mouse_region = NULL;

Boolean
mac_wait_next_event (er, sleep_time, dequeue)
     EventRecord *er;
     UInt32 sleep_time;
     Boolean dequeue;
{
  static EventRecord er_buf = {nullEvent};
  UInt32 target_tick, current_tick;
  EventMask event_mask;

  if (mouse_region == NULL)
    mouse_region = NewRgn ();

  event_mask = everyEvent;
  if (!mac_ready_for_apple_events)
    event_mask -= highLevelEventMask;

  current_tick = TickCount ();
  target_tick = current_tick + sleep_time;

  if (er_buf.what == nullEvent)
    while (!WaitNextEvent (event_mask, &er_buf,
			   target_tick - current_tick, mouse_region))
      {
	current_tick = TickCount ();
	if (target_tick <= current_tick)
	  return false;
      }

  *er = er_buf;
  if (dequeue)
    er_buf.what = nullEvent;
  return true;
}
#endif /* not TARGET_API_MAC_CARBON */

#if TARGET_API_MAC_CARBON
OSStatus
mac_post_mouse_moved_event ()
{
  EventRef event = NULL;
  OSStatus err;

  err = CreateEvent (NULL, kEventClassMouse, kEventMouseMoved, 0,
		     kEventAttributeNone, &event);
  if (err == noErr)
    {
      Point mouse_pos;

      GetGlobalMouse (&mouse_pos);
      err = SetEventParameter (event, kEventParamMouseLocation, typeQDPoint,
			       sizeof (Point), &mouse_pos);
    }
  if (err == noErr)
    {
      UInt32 modifiers = GetCurrentKeyModifiers ();

      err = SetEventParameter (event, kEventParamKeyModifiers, typeUInt32,
			       sizeof (UInt32), &modifiers);
    }
  if (err == noErr)
    err = PostEventToQueue (GetCurrentEventQueue (), event,
			    kEventPriorityStandard);
  if (event)
    ReleaseEvent (event);

  return err;
}
#endif

#ifdef MAC_OSX
/* Run the current run loop in the default mode until some input
   happens or TIMEOUT seconds passes unless it is negative.  Return
   true if timeout occurs first.  */

Boolean
mac_run_loop_run_once (timeout)
     EventTimeout timeout;
{
#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (NULL);
#endif
  return (CFRunLoopRunInMode (kCFRunLoopDefaultMode,
			      timeout >= 0 ? timeout : 100000, true)
	  == kCFRunLoopRunTimedOut);
}
#endif

/* Emacs calls this whenever it wants to read an input event from the
   user. */

int
XTread_socket (sd, expected, hold_quit)
     int sd, expected;
     struct input_event *hold_quit;
{
  struct input_event inev;
  int count = 0;
#if TARGET_API_MAC_CARBON
  EventRef eventRef;
  EventTargetRef toolbox_dispatcher;
#endif
  EventRecord er;
  struct mac_display_info *dpyinfo = &one_mac_display_info;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  ++handling_signal;

#if TARGET_API_MAC_CARBON
  toolbox_dispatcher = GetEventDispatcherTarget ();

  while (
#if USE_CG_DRAWING
	 mac_prepare_for_quickdraw (NULL),
#endif
	 !ReceiveNextEvent (0, NULL, kEventDurationNoWait,
			    kEventRemoveFromQueue, &eventRef))
#else /* !TARGET_API_MAC_CARBON */
  while (mac_wait_next_event (&er, 0, true))
#endif /* !TARGET_API_MAC_CARBON */
    {
      int do_help = 0;
      struct frame *f;
      unsigned long timestamp;

      EVENT_INIT (inev);
      inev.kind = NO_EVENT;
      inev.arg = Qnil;

#if TARGET_API_MAC_CARBON
      timestamp = GetEventTime (eventRef) / kEventDurationMillisecond;

      if (!mac_convert_event_ref (eventRef, &er))
	goto OTHER;
#else  /* !TARGET_API_MAC_CARBON */
      timestamp = er.when * (1000 / 60); /* ticks to milliseconds */
#endif  /* !TARGET_API_MAC_CARBON */

      switch (er.what)
	{
	case mouseDown:
	case mouseUp:
	  {
	    WindowRef window_ptr;
	    ControlPartCode part_code;
	    int tool_bar_p = 0;

#if TARGET_API_MAC_CARBON
	    OSStatus err;

	    /* This is needed to send mouse events like aqua window
	       buttons to the correct handler.  */
	    read_socket_inev = &inev;
	    err = SendEventToEventTarget (eventRef, toolbox_dispatcher);
	    read_socket_inev = NULL;
	    if (err != eventNotHandledErr)
	      break;
#endif
	    last_mouse_glyph_frame = 0;

	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      {
		window_ptr = FRAME_MAC_WINDOW (last_mouse_frame);
		part_code = inContent;
	      }
	    else
	      {
		part_code = FindWindow (er.where, &window_ptr);
		if (tip_window && window_ptr == tip_window)
		  {
		    HideWindow (tip_window);
		    part_code = FindWindow (er.where, &window_ptr);
		  }
	      }

	    if (er.what != mouseDown
		&& (part_code != inContent || dpyinfo->grabbed == 0))
	      break;

	    switch (part_code)
	      {
	      case inMenuBar:
		f = mac_focus_frame (dpyinfo);
		saved_menu_event_location = er.where;
		inev.kind = MENU_BAR_ACTIVATE_EVENT;
		XSETFRAME (inev.frame_or_window, f);
		break;

	      case inContent:
		if (
#if TARGET_API_MAC_CARBON
		    FrontNonFloatingWindow ()
#else
		    FrontWindow ()
#endif
		    != window_ptr
		    || (mac_window_to_frame (window_ptr)
			!= dpyinfo->x_focus_frame))
		  SelectWindow (window_ptr);
		else
		  {
		    ControlPartCode control_part_code;
		    ControlRef ch;
		    Point mouse_loc;
#ifdef MAC_OSX
		    ControlKind control_kind;
#endif

		    f = mac_window_to_frame (window_ptr);
		    /* convert to local coordinates of new window */
		    mouse_loc.h = (er.where.h
				   - (f->left_pos
				      + FRAME_OUTER_TO_INNER_DIFF_X (f)));
		    mouse_loc.v = (er.where.v
				   - (f->top_pos
				      + FRAME_OUTER_TO_INNER_DIFF_Y (f)));
#if TARGET_API_MAC_CARBON
		    ch = FindControlUnderMouse (mouse_loc, window_ptr,
						&control_part_code);
#ifdef MAC_OSX
		    if (ch)
		      GetControlKind (ch, &control_kind);
#endif
#else
		    control_part_code = FindControl (mouse_loc, window_ptr,
						     &ch);
#endif

#if TARGET_API_MAC_CARBON
		    inev.code = mac_get_mouse_btn (eventRef);
		    inev.modifiers = mac_event_to_emacs_modifiers (eventRef);
#else
		    inev.code = mac_get_emulated_btn (er.modifiers);
		    inev.modifiers = mac_to_emacs_modifiers (er.modifiers, 0);
#endif
		    XSETINT (inev.x, mouse_loc.h);
		    XSETINT (inev.y, mouse_loc.v);

		    if ((dpyinfo->grabbed && tracked_scroll_bar)
			|| (ch != 0
#ifndef USE_TOOLKIT_SCROLL_BARS
			    /* control_part_code becomes kControlNoPart if
			       a progress indicator is clicked.  */
			    && control_part_code != kControlNoPart
#else  /* USE_TOOLKIT_SCROLL_BARS */
#ifdef MAC_OSX
			    && control_kind.kind == kControlKindScrollBar
#endif	/* MAC_OSX */
#endif	/* USE_TOOLKIT_SCROLL_BARS */
			    ))
		      {
			struct scroll_bar *bar;

			if (dpyinfo->grabbed && tracked_scroll_bar)
			  {
			    bar = tracked_scroll_bar;
#ifndef USE_TOOLKIT_SCROLL_BARS
			    control_part_code = kControlIndicatorPart;
#endif
			  }
			else
			  bar = (struct scroll_bar *) GetControlReference (ch);
#ifdef USE_TOOLKIT_SCROLL_BARS
			/* Make the "Ctrl-Mouse-2 splits window" work
			   for toolkit scroll bars.  */
			if (inev.modifiers & ctrl_modifier)
			  x_scroll_bar_handle_click (bar, control_part_code,
						     &er, &inev);
			else if (er.what == mouseDown)
			  x_scroll_bar_handle_press (bar, control_part_code,
						     mouse_loc, &inev);
			else
			  x_scroll_bar_handle_release (bar, &inev);
#else  /* not USE_TOOLKIT_SCROLL_BARS */
			x_scroll_bar_handle_click (bar, control_part_code,
						   &er, &inev);
			if (er.what == mouseDown
			    && control_part_code == kControlIndicatorPart)
			  tracked_scroll_bar = bar;
			else
			  tracked_scroll_bar = NULL;
#endif  /* not USE_TOOLKIT_SCROLL_BARS */
		      }
		    else
		      {
			Lisp_Object window;
			int x = mouse_loc.h;
			int y = mouse_loc.v;

			window = window_from_coordinates (f, x, y, 0, 0, 0, 1);
			if (EQ (window, f->tool_bar_window))
			  {
			    if (er.what == mouseDown)
			      handle_tool_bar_click (f, x, y, 1, 0);
			    else
			      handle_tool_bar_click (f, x, y, 0,
						     inev.modifiers);
			    tool_bar_p = 1;
			  }
			else
			  {
			    XSETFRAME (inev.frame_or_window, f);
			    inev.kind = MOUSE_CLICK_EVENT;
			  }
		      }

		    if (er.what == mouseDown)
		      {
			dpyinfo->grabbed |= (1 << inev.code);
			last_mouse_frame = f;

			if (!tool_bar_p)
			  last_tool_bar_item = -1;
		      }
		    else
		      {
			if ((dpyinfo->grabbed & (1 << inev.code)) == 0)
			  /* If a button is released though it was not
			     previously pressed, that would be because
			     of multi-button emulation.  */
			  dpyinfo->grabbed = 0;
			else
			  dpyinfo->grabbed &= ~(1 << inev.code);
		      }

		    /* Ignore any mouse motion that happened before
		       this event; any subsequent mouse-movement Emacs
		       events should reflect only motion after the
		       ButtonPress.  */
		    if (f != 0)
		      f->mouse_moved = 0;

#ifdef USE_TOOLKIT_SCROLL_BARS
		    if (inev.kind == MOUSE_CLICK_EVENT
			|| (inev.kind == SCROLL_BAR_CLICK_EVENT
			    && (inev.modifiers & ctrl_modifier)))
#endif
		      switch (er.what)
			{
			case mouseDown:
			  inev.modifiers |= down_modifier;
			  break;
			case mouseUp:
			  inev.modifiers |= up_modifier;
			  break;
			}
		  }
		break;

	      case inDrag:
#if TARGET_API_MAC_CARBON
	      case inProxyIcon:
		if (IsWindowPathSelectClick (window_ptr, &er))
		  {
		    WindowPathSelect (window_ptr, NULL, NULL);
		    break;
		  }
		if (part_code == inProxyIcon
		    && (TrackWindowProxyDrag (window_ptr, er.where)
			!= errUserWantsToDragWindow))
		  break;
		DragWindow (window_ptr, er.where, NULL);
#else /* not TARGET_API_MAC_CARBON */
		DragWindow (window_ptr, er.where, &qd.screenBits.bounds);
		/* Update the frame parameters.  */
		{
		  struct frame *f = mac_window_to_frame (window_ptr);

		  if (f && !f->async_iconified)
		    mac_handle_origin_change (f);
		}
#endif /* not TARGET_API_MAC_CARBON */
		break;

	      case inGoAway:
		if (TrackGoAway (window_ptr, er.where))
		  {
		    inev.kind = DELETE_WINDOW_EVENT;
		    XSETFRAME (inev.frame_or_window,
			       mac_window_to_frame (window_ptr));
		  }
		break;

		/* window resize handling added --ben */
	      case inGrow:
		do_grow_window (window_ptr, &er);
		break;

		/* window zoom handling added --ben */
	      case inZoomIn:
	      case inZoomOut:
		if (TrackBox (window_ptr, er.where, part_code))
		  do_zoom_window (window_ptr, part_code);
		break;

#if USE_MAC_TOOLBAR
	      case inStructure:
		{
		  OSStatus err;
		  HIViewRef ch;

		  if (FrontNonFloatingWindow () != window_ptr)
		    SelectWindow (window_ptr);

		  err = HIViewGetViewForMouseEvent (HIViewGetRoot (window_ptr),
						    eventRef, &ch);
		  /* This doesn't work on Mac OS X 10.2.  */
		  if (err == noErr)
		    HIViewClick (ch, eventRef);
		}
		break;
#endif	/* USE_MAC_TOOLBAR */

	      default:
		break;
	      }
	  }
	  break;

#if !TARGET_API_MAC_CARBON
	case updateEvt:
	  do_window_update ((WindowRef) er.message);
	  break;
#endif

	case osEvt:
#if TARGET_API_MAC_CARBON
	  if (SendEventToEventTarget (eventRef, toolbox_dispatcher)
	      != eventNotHandledErr)
	    break;
#endif
	  switch ((er.message >> 24) & 0x000000FF)
	    {
#if USE_MAC_TSM
	    case suspendResumeMessage:
	      if (er.message & resumeFlag)
		mac_tsm_resume ();
	      else
		mac_tsm_suspend ();
	      break;
#endif

	    case mouseMovedMessage:
#if !TARGET_API_MAC_CARBON
	      SetRectRgn (mouse_region, er.where.h, er.where.v,
			  er.where.h + 1, er.where.v + 1);
#endif
	      previous_help_echo_string = help_echo_string;
	      help_echo_string = Qnil;

	      if (dpyinfo->grabbed && last_mouse_frame
		  && FRAME_LIVE_P (last_mouse_frame))
		f = last_mouse_frame;
	      else
		f = dpyinfo->x_focus_frame;

	      if (dpyinfo->mouse_face_hidden)
		{
		  dpyinfo->mouse_face_hidden = 0;
		  clear_mouse_face (dpyinfo);
		}

	      if (f)
		{
		  WindowRef wp = FRAME_MAC_WINDOW (f);
		  Point mouse_pos;

		  mouse_pos.h = (er.where.h
				 - (f->left_pos
				    + FRAME_OUTER_TO_INNER_DIFF_X (f)));
		  mouse_pos.v = (er.where.v
				 - (f->top_pos
				    + FRAME_OUTER_TO_INNER_DIFF_Y (f)));
		  if (dpyinfo->grabbed && tracked_scroll_bar)
#ifdef USE_TOOLKIT_SCROLL_BARS
		    x_scroll_bar_handle_drag (wp, tracked_scroll_bar,
					      mouse_pos, &inev);
#else /* not USE_TOOLKIT_SCROLL_BARS */
		    x_scroll_bar_note_movement (tracked_scroll_bar,
						mouse_pos.v
						- XINT (tracked_scroll_bar->top),
						er.when * (1000 / 60));
#endif /* not USE_TOOLKIT_SCROLL_BARS */
		  else
		    {
		      /* Generate SELECT_WINDOW_EVENTs when needed.  */
		      if (!NILP (Vmouse_autoselect_window))
			{
			  Lisp_Object window;

			  window = window_from_coordinates (f,
							    mouse_pos.h,
							    mouse_pos.v,
							    0, 0, 0, 0);

			  /* Window will be selected only when it is
			     not selected now and last mouse movement
			     event was not in it.  Minibuffer window
			     will be selected only when it is active.  */
			  if (WINDOWP (window)
			      && !EQ (window, last_window)
			      && !EQ (window, selected_window)
			      /* For click-to-focus window managers
				 create event iff we don't leave the
				 selected frame.  */
			      && (focus_follows_mouse
				  || (EQ (XWINDOW (window)->frame,
					  XWINDOW (selected_window)->frame))))
			    {
			      inev.kind = SELECT_WINDOW_EVENT;
			      inev.frame_or_window = window;
			    }

			  last_window=window;
			}
		      if (!note_mouse_movement (f, &mouse_pos))
			help_echo_string = previous_help_echo_string;
#if USE_MAC_TOOLBAR
		      else
			mac_tool_bar_note_mouse_movement (f, eventRef);
#endif
		    }
		}

	      /* If the contents of the global variable
		 help_echo_string has changed, generate a
		 HELP_EVENT.  */
	      if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
		do_help = 1;
	      break;
	    }
	  break;

	case activateEvt:
	  {
	    WindowRef window_ptr = (WindowRef) er.message;
	    OSErr err;
	    ControlRef root_control;

	    if (window_ptr == tip_window)
	      {
		HideWindow (tip_window);
		break;
	      }

	    if (!is_emacs_window (window_ptr))
	      goto OTHER;

	    f = mac_window_to_frame (window_ptr);

	    if ((er.modifiers & activeFlag) != 0)
	      {
		/* A window has been activated */
		Point mouse_loc;

		err = GetRootControl (FRAME_MAC_WINDOW (f), &root_control);
		if (err == noErr)
		  ActivateControl (root_control);

		x_detect_focus_change (dpyinfo, &er, &inev);

		mouse_loc.h = (er.where.h
			       - (f->left_pos
				  + FRAME_OUTER_TO_INNER_DIFF_X (f)));
		mouse_loc.v = (er.where.v
			       - (f->top_pos
				  + FRAME_OUTER_TO_INNER_DIFF_Y (f)));
		/* Window-activated event counts as mouse movement,
		   so update things that depend on mouse position.  */
		note_mouse_movement (f, &mouse_loc);
	      }
	    else
	      {
		/* A window has been deactivated */
		err = GetRootControl (FRAME_MAC_WINDOW (f), &root_control);
		if (err == noErr)
		  DeactivateControl (root_control);

#ifdef USE_TOOLKIT_SCROLL_BARS
		if (dpyinfo->grabbed && tracked_scroll_bar)
		  {
		    struct input_event event;

		    EVENT_INIT (event);
		    event.kind = NO_EVENT;
		    x_scroll_bar_handle_release (tracked_scroll_bar, &event);
		    if (event.kind != NO_EVENT)
		      {
			event.timestamp = timestamp;
			kbd_buffer_store_event_hold (&event, hold_quit);
			count++;
		      }
		  }
#endif
		dpyinfo->grabbed = 0;

		x_detect_focus_change (dpyinfo, &er, &inev);

		if (f == dpyinfo->mouse_face_mouse_frame)
		  {
		    /* If we move outside the frame, then we're
		       certainly no longer on any text in the
		       frame.  */
		    clear_mouse_face (dpyinfo);
		    dpyinfo->mouse_face_mouse_frame = 0;
		  }

		/* Generate a nil HELP_EVENT to cancel a help-echo.
		   Do it only if there's something to cancel.
		   Otherwise, the startup message is cleared when the
		   mouse leaves the frame.  */
		if (any_help_event_p)
		  do_help = -1;
	      }
	  }
	  break;

	case keyDown:
	case keyUp:
	case autoKey:
	  ObscureCursor ();

	  f = mac_focus_frame (dpyinfo);
	  XSETFRAME (inev.frame_or_window, f);

	  /* If mouse-highlight is an integer, input clears out mouse
	     highlighting.  */
	  if (!dpyinfo->mouse_face_hidden && INTEGERP (Vmouse_highlight)
	      && !EQ (f->tool_bar_window, dpyinfo->mouse_face_window))
	    {
	      clear_mouse_face (dpyinfo);
	      dpyinfo->mouse_face_hidden = 1;
	    }

	  {
	    UInt32 modifiers = er.modifiers, mapped_modifiers;
	    UInt32 key_code = (er.message & keyCodeMask) >> 8;

#ifdef MAC_OSX
	    GetEventParameter (eventRef, kEventParamKeyModifiers,
			       typeUInt32, NULL,
			       sizeof (UInt32), NULL, &modifiers);
#endif
	    mapped_modifiers = mac_mapped_modifiers (modifiers, key_code);

#if TARGET_API_MAC_CARBON
	    if (!(mapped_modifiers
		  & ~(mac_pass_command_to_system ? cmdKey : 0)
		  & ~(mac_pass_control_to_system ? controlKey : 0)))
	      goto OTHER;
	    else
#endif
	      if (er.what != keyUp)
		do_keystroke (er.what, er.message & charCodeMask,
			      key_code, modifiers, timestamp, &inev);
	  }
	  break;

	case kHighLevelEvent:
	  AEProcessAppleEvent (&er);
	  break;

	default:
	OTHER:
#if TARGET_API_MAC_CARBON
	  {
	    OSStatus err;

	    read_socket_inev = &inev;
	    err = SendEventToEventTarget (eventRef, toolbox_dispatcher);
	    read_socket_inev = NULL;
	  }
#endif
	  break;
	}
#if TARGET_API_MAC_CARBON
      ReleaseEvent (eventRef);
#endif

      if (inev.kind != NO_EVENT)
	{
	  inev.timestamp = timestamp;
	  kbd_buffer_store_event_hold (&inev, hold_quit);
	  count++;
	}

      if (do_help
	  && !(hold_quit && hold_quit->kind != NO_EVENT))
	{
	  Lisp_Object frame;

	  if (f)
	    XSETFRAME (frame, f);
	  else
	    frame = Qnil;

	  if (do_help > 0)
	    {
	      any_help_event_p = 1;
	      gen_help_event (help_echo_string, frame, help_echo_window,
			      help_echo_object, help_echo_pos);
	    }
	  else
	    {
	      help_echo_string = Qnil;
	      gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	    }
	  count++;
	}
    }

  /* If the focus was just given to an autoraising frame,
     raise it now.  */
  /* ??? This ought to be able to handle more than one such frame.  */
  if (pending_autoraise_frame)
    {
      x_raise_frame (pending_autoraise_frame);
      pending_autoraise_frame = 0;
    }

  if (mac_screen_config_changed)
    {
      mac_get_screen_info (dpyinfo);
      mac_screen_config_changed = 0;
    }

#if !TARGET_API_MAC_CARBON
  /* Check which frames are still visible.  We do this here because
     there doesn't seem to be any direct notification from the Window
     Manager that the visibility of a window has changed (at least,
     not in all cases).  */
  {
    Lisp_Object tail, frame;

    FOR_EACH_FRAME (tail, frame)
      {
	struct frame *f = XFRAME (frame);

	/* The tooltip has been drawn already.  Avoid the
	   SET_FRAME_GARBAGED in mac_handle_visibility_change.  */
	if (EQ (frame, tip_frame))
	  continue;

	if (FRAME_MAC_P (f))
	  mac_handle_visibility_change (f);
      }
  }
#endif

  --handling_signal;
  UNBLOCK_INPUT;
  return count;
}


/***********************************************************************
				Busy cursor
 ***********************************************************************/

#if TARGET_API_MAC_CARBON
/* Show the spinning progress indicator for the frame F.  Create it if
   it doesn't exist yet. */

void
mac_show_hourglass (f)
     struct frame *f;
{
#if USE_CG_DRAWING
  mac_prepare_for_quickdraw (f);
#endif
  if (!f->output_data.mac->hourglass_control)
    {
      Window w = FRAME_MAC_WINDOW (f);
      Rect r;
      ControlRef c;

      GetWindowPortBounds (w, &r);
      r.left = r.right - HOURGLASS_WIDTH;
      r.bottom = r.top + HOURGLASS_HEIGHT;
      if (CreateChasingArrowsControl (w, &r, &c) == noErr)
	f->output_data.mac->hourglass_control = c;
    }

  if (f->output_data.mac->hourglass_control)
    ShowControl (f->output_data.mac->hourglass_control);
}

/* Hide the spinning progress indicator for the frame F.  Do nothing
   it doesn't exist yet. */

void
mac_hide_hourglass (f)
     struct frame *f;
{
  if (f->output_data.mac->hourglass_control)
    {
#if USE_CG_DRAWING
      mac_prepare_for_quickdraw (f);
#endif
      HideControl (f->output_data.mac->hourglass_control);
    }
}

/* Reposition the spinning progress indicator for the frame F.  Do
   nothing it doesn't exist yet. */

void
mac_reposition_hourglass (f)
     struct frame *f;
{
  if (f->output_data.mac->hourglass_control)
    {
#if USE_CG_DRAWING
      mac_prepare_for_quickdraw (f);
#endif
      MoveControl (f->output_data.mac->hourglass_control,
		   FRAME_PIXEL_WIDTH (f) - HOURGLASS_WIDTH, 0);
    }
}
#endif	/* TARGET_API_MAC_CARBON */


/***********************************************************************
			File selection dialog
 ***********************************************************************/

#if TARGET_API_MAC_CARBON
extern Lisp_Object Qfile_name_history;

static pascal void mac_nav_event_callback P_ ((NavEventCallbackMessage,
					       NavCBRecPtr, void *));

/* The actual implementation of Fx_file_dialog.  */

Lisp_Object
mac_file_dialog (prompt, dir, default_filename, mustmatch, only_dir_p)
     Lisp_Object prompt, dir, default_filename, mustmatch, only_dir_p;
{
  Lisp_Object file = Qnil;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;
  char filename[MAXPATHLEN];
  static NavEventUPP mac_nav_event_callbackUPP = NULL;

  check_mac ();

  GCPRO6 (prompt, dir, default_filename, mustmatch, file, only_dir_p);
  CHECK_STRING (prompt);
  CHECK_STRING (dir);

  /* Create the dialog with PROMPT as title, using DIR as initial
     directory and using "*" as pattern.  */
  dir = Fexpand_file_name (dir, Qnil);

  {
    OSStatus status;
    NavDialogCreationOptions options;
    NavDialogRef dialogRef;
    NavTypeListHandle fileTypes = NULL;
    NavUserAction userAction;
    CFStringRef message=NULL, saveName = NULL;

    BLOCK_INPUT;
    /* No need for a callback function because we are modal */
    NavGetDefaultDialogCreationOptions(&options);
    options.modality = kWindowModalityAppModal;
    options.location.h = options.location.v = -1;
    options.optionFlags = kNavDefaultNavDlogOptions;
    options.optionFlags |= kNavAllFilesInPopup;  /* All files allowed */
    options.optionFlags |= kNavSelectAllReadableItem;
    options.optionFlags &= ~kNavAllowMultipleFiles;
    if (!NILP(prompt))
      {
	message = cfstring_create_with_string (prompt);
	options.message = message;
      }
    /* Don't set the application, let it use default.
    options.clientName = CFSTR ("Emacs");
    */

    if (mac_nav_event_callbackUPP == NULL)
      mac_nav_event_callbackUPP = NewNavEventUPP (mac_nav_event_callback);

    if (!NILP (only_dir_p))
      status = NavCreateChooseFolderDialog(&options, mac_nav_event_callbackUPP,
					   NULL, NULL, &dialogRef);
    else if (NILP (mustmatch))
      {
	/* This is a save dialog */
	options.optionFlags |= kNavDontConfirmReplacement;
	options.actionButtonLabel = CFSTR ("Ok");
	options.windowTitle = CFSTR ("Enter name");

	if (STRINGP (default_filename))
	  {
	    Lisp_Object utf8 = ENCODE_UTF_8 (default_filename);
	    char *begPtr = SDATA(utf8);
	    char *filePtr = begPtr + SBYTES(utf8);
	    while (filePtr != begPtr && !IS_DIRECTORY_SEP(filePtr[-1]))
	      filePtr--;
	    saveName = cfstring_create_with_utf8_cstring (filePtr);
	    options.saveFileName = saveName;
	    options.optionFlags |= kNavSelectDefaultLocation;
	  }
	  status = NavCreatePutFileDialog(&options,
					  'TEXT', kNavGenericSignature,
					  mac_nav_event_callbackUPP, NULL,
					  &dialogRef);
	}
    else
      {
	/* This is an open dialog*/
	status = NavCreateChooseFileDialog(&options, fileTypes,
					   mac_nav_event_callbackUPP, NULL,
					   NULL, NULL, &dialogRef);
      }

    /* Set the default location and continue*/
    if (status == noErr)
      {
	Lisp_Object encoded_dir = ENCODE_FILE (dir);
	AEDesc defLocAed;

	status = AECreateDesc (TYPE_FILE_NAME, SDATA (encoded_dir),
			       SBYTES (encoded_dir), &defLocAed);
	if (status == noErr)
	  {
	    NavCustomControl(dialogRef, kNavCtlSetLocation, (void*) &defLocAed);
	    AEDisposeDesc(&defLocAed);
	  }
	status = NavDialogRun(dialogRef);
      }

    if (saveName) CFRelease(saveName);
    if (message) CFRelease(message);

    if (status == noErr) {
      userAction = NavDialogGetUserAction(dialogRef);
      switch (userAction)
	{
	case kNavUserActionNone:
	case kNavUserActionCancel:
	  break;		/* Treat cancel like C-g */
	case kNavUserActionOpen:
	case kNavUserActionChoose:
	case kNavUserActionSaveAs:
	  {
	    NavReplyRecord reply;
	    Size len;

	    status = NavDialogGetReply(dialogRef, &reply);
	    if (status != noErr)
	      break;
	    status = AEGetNthPtr (&reply.selection, 1, TYPE_FILE_NAME,
				  NULL, NULL, filename,
				  sizeof (filename) - 1, &len);
	    if (status == noErr)
	      {
		len = min (len, sizeof (filename) - 1);
		filename[len] = '\0';
		if (reply.saveFileName)
		  {
		    /* If it was a saved file, we need to add the file name */
		    if (len && len < sizeof (filename) - 1
			&& filename[len-1] != '/')
		      filename[len++] = '/';
		    CFStringGetCString(reply.saveFileName, filename+len,
				       sizeof (filename) - len,
#ifdef MAC_OSX
				       kCFStringEncodingUTF8
#else
				       CFStringGetSystemEncoding ()
#endif
				       );
		  }
		file = DECODE_FILE (make_unibyte_string (filename,
							 strlen (filename)));
	      }
	    NavDisposeReply(&reply);
	  }
	  break;
	}
      NavDialogDispose(dialogRef);
      UNBLOCK_INPUT;
    }
    else {
      UNBLOCK_INPUT;
      /* Fall back on minibuffer if there was a problem */
      file = Fcompleting_read (prompt, intern ("read-file-name-internal"),
			       dir, mustmatch, dir, Qfile_name_history,
			       default_filename, Qnil);
    }
  }

  UNGCPRO;

  /* Make "Cancel" equivalent to C-g.  */
  if (NILP (file))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, file);
}

/* Need to register some event callback function for enabling drag and
   drop in Navigation Service dialogs.  */
static pascal void
mac_nav_event_callback (selector, parms, data)
     NavEventCallbackMessage selector;
     NavCBRecPtr parms;
     void *data;
{
}
#endif


/************************************************************************
				 Menu
 ************************************************************************/

#if !TARGET_API_MAC_CARBON
#include <MacTypes.h>
#include <Menus.h>
#include <Quickdraw.h>
#include <ToolUtils.h>
#include <Fonts.h>
#include <Controls.h>
#include <Windows.h>
#include <Events.h>
#if defined (__MRC__) || (__MSL__ >= 0x6000)
#include <ControlDefinitions.h>
#endif
#endif /* not TARGET_API_MAC_CARBON */

extern int menu_item_selection;
extern int popup_activated_flag;
extern int name_is_separator P_ ((const char *));
extern void find_and_call_menu_selection P_ ((FRAME_PTR, int, Lisp_Object,
					      void *));
extern void set_frame_menubar P_ ((FRAME_PTR, int, int));

enum mac_menu_kind {		/* Menu ID range  */
  MAC_MENU_APPLE,		/* 0 (Reserved by Apple) */
  MAC_MENU_MENU_BAR,		/* 1 .. 233       */
  MAC_MENU_M_APPLE,		/* 234      (== M_APPLE) */
  MAC_MENU_POPUP,		/* 235            */
  MAC_MENU_DRIVER,		/* 236 .. 255 (Reserved) */
  MAC_MENU_MENU_BAR_SUB,	/* 256 .. 16383   */
  MAC_MENU_POPUP_SUB,		/* 16384 .. 32767 */
  MAC_MENU_END			/* 32768          */
};

static const int min_menu_id[] = {0, 1, 234, 235, 236, 256, 16384, 32768};

static int fill_menu P_ ((MenuRef, widget_value *, enum mac_menu_kind, int));
static void dispose_menus P_ ((enum mac_menu_kind, int));

#if !TARGET_API_MAC_CARBON
static void
do_apple_menu (SInt16 menu_item)
{
  Str255 item_name;
  SInt16 da_driver_refnum;

  if (menu_item == I_ABOUT)
    NoteAlert (ABOUT_ALERT_ID, NULL);
  else
    {
      GetMenuItemText (GetMenuRef (M_APPLE), menu_item, item_name);
      da_driver_refnum = OpenDeskAcc (item_name);
    }
}
#endif /* !TARGET_API_MAC_CARBON */

/* Activate the menu bar of frame F.
   This is called from keyboard.c when it gets the
   MENU_BAR_ACTIVATE_EVENT out of the Emacs event queue.

   To activate the menu bar, we use the button-press event location
   that was saved in saved_menu_event_location.

   But first we recompute the menu bar contents (the whole tree).

   The reason for saving the button event until here, instead of
   passing it to the toolkit right away, is that we can safely
   execute Lisp code.  */

void
x_activate_menubar (f)
     FRAME_PTR f;
{
  SInt32 menu_choice;
  SInt16 menu_id, menu_item;

  set_frame_menubar (f, 0, 1);
  BLOCK_INPUT;

  popup_activated_flag = 1;
  menu_choice = MenuSelect (saved_menu_event_location);
  popup_activated_flag = 0;
  menu_id = HiWord (menu_choice);
  menu_item = LoWord (menu_choice);

#if !TARGET_API_MAC_CARBON
  if (menu_id == min_menu_id[MAC_MENU_M_APPLE])
    do_apple_menu (menu_item);
  else
#endif
    if (menu_id)
      {
        MenuRef menu = GetMenuRef (menu_id);

        if (menu)
          {
            UInt32 refcon;

            GetMenuItemRefCon (menu, menu_item, &refcon);
            find_and_call_menu_selection (f, f->menu_bar_items_used,
					  f->menu_bar_vector, (void *) refcon);
          }
      }

  HiliteMenu (0);

  UNBLOCK_INPUT;
}

#if TARGET_API_MAC_CARBON
extern Lisp_Object Vshow_help_function;

static Lisp_Object
restore_show_help_function (old_show_help_function)
     Lisp_Object old_show_help_function;
{
  Vshow_help_function = old_show_help_function;

  return Qnil;
}

static pascal OSStatus
menu_target_item_handler (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err;
  MenuRef menu;
  MenuItemIndex menu_item;
  Lisp_Object help;
  GrafPtr port;
  int specpdl_count = SPECPDL_INDEX ();

  /* Don't be bothered with the overflowed toolbar items menu.  */
  if (!popup_activated ())
    return eventNotHandledErr;

  err = GetEventParameter (event, kEventParamDirectObject, typeMenuRef,
			   NULL, sizeof (MenuRef), NULL, &menu);
  if (err == noErr)
    err = GetEventParameter (event, kEventParamMenuItemIndex,
			     typeMenuItemIndex, NULL,
			     sizeof (MenuItemIndex), NULL, &menu_item);
  if (err == noErr)
    err = GetMenuItemProperty (menu, menu_item,
			       MAC_EMACS_CREATOR_CODE, 'help',
			       sizeof (Lisp_Object), NULL, &help);
  if (err != noErr)
    help = Qnil;

  /* Temporarily bind Vshow_help_function to Qnil because we don't
     want tooltips during menu tracking.  */
  record_unwind_protect (restore_show_help_function, Vshow_help_function);
  Vshow_help_function = Qnil;
  GetPort (&port);
  show_help_echo (help, Qnil, Qnil, Qnil, 1);
  SetPort (port);
  unbind_to (specpdl_count, Qnil);

  return err == noErr ? noErr : eventNotHandledErr;
}

/* Showing help echo string during menu tracking.  */

static OSStatus
install_menu_target_item_handler ()
{
  static const EventTypeSpec specs[] =
    {{kEventClassMenu, kEventMenuTargetItem}};

  return InstallApplicationEventHandler (NewEventHandlerUPP
					 (menu_target_item_handler),
					 GetEventTypeCount (specs),
					 specs, NULL, NULL);
}
#endif  /* TARGET_API_MAC_CARBON */

/* Event handler function that pops down a menu on C-g.  We can only pop
   down menus if CancelMenuTracking is present (OSX 10.3 or later).  */

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
static pascal OSStatus
menu_quit_handler (nextHandler, theEvent, userData)
     EventHandlerCallRef nextHandler;
     EventRef theEvent;
     void* userData;
{
  OSStatus err;
  UInt32 keyCode;
  UInt32 keyModifiers;

  err = GetEventParameter (theEvent, kEventParamKeyCode,
			   typeUInt32, NULL, sizeof(UInt32), NULL, &keyCode);

  if (err == noErr)
    err = GetEventParameter (theEvent, kEventParamKeyModifiers,
			     typeUInt32, NULL, sizeof(UInt32),
			     NULL, &keyModifiers);

  if (err == noErr && mac_quit_char_key_p (keyModifiers, keyCode))
    {
      MenuRef menu = userData != 0
        ? (MenuRef)userData : AcquireRootMenu ();

      CancelMenuTracking (menu, true, 0);
      if (!userData) ReleaseMenu (menu);
      return noErr;
    }

  return CallNextEventHandler (nextHandler, theEvent);
}
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1030 */

/* Add event handler to all menus that belong to KIND so we can detect
   C-g.  ROOT_MENU is the root menu of the tracking session to dismiss
   when C-g is detected.  NULL means the menu bar.  If
   CancelMenuTracking isn't available, do nothing.  */

static void
install_menu_quit_handler (kind, root_menu)
     enum mac_menu_kind kind;
     MenuRef root_menu;
{
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
  static const EventTypeSpec typesList[] =
    {{kEventClassKeyboard, kEventRawKeyDown}};
  int id;

#if MAC_OS_X_VERSION_MIN_REQUIRED == 1020
  if (CancelMenuTracking == NULL)
    return;
#endif
  for (id = min_menu_id[kind]; id < min_menu_id[kind + 1]; id++)
    {
      MenuRef menu = GetMenuRef (id);

      if (menu == NULL)
	break;
      InstallMenuEventHandler (menu, menu_quit_handler,
			       GetEventTypeCount (typesList),
			       typesList, root_menu, NULL);
    }
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1030 */
}

static Lisp_Object
pop_down_menu (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  FRAME_PTR f = p->pointer;
  MenuRef menu = GetMenuRef (min_menu_id[MAC_MENU_POPUP]);

  BLOCK_INPUT;

  /* Must reset this manually because the button release event is not
     passed to Emacs event loop. */
  FRAME_MAC_DISPLAY_INFO (f)->grabbed = 0;

  /* delete all menus */
  dispose_menus (MAC_MENU_POPUP_SUB, 0);
  DeleteMenu (min_menu_id[MAC_MENU_POPUP]);
  DisposeMenu (menu);

  UNBLOCK_INPUT;

  return Qnil;
}

/* Pop up the menu for frame F defined by FIRST_WV at X/Y and loop
   until the menu pops down.  Return the selection.  */

void
create_and_show_popup_menu (f, first_wv, x, y, for_click)
     FRAME_PTR f;
     widget_value *first_wv;
     int x;
     int y;
     int for_click;
{
  int result = 0;
  MenuRef menu = NewMenu (min_menu_id[MAC_MENU_POPUP], "\p");
  int menu_item_choice;
  int specpdl_count = SPECPDL_INDEX ();

  InsertMenu (menu, -1);
  fill_menu (menu, first_wv->contents, MAC_MENU_POPUP_SUB,
	     min_menu_id[MAC_MENU_POPUP_SUB]);

  /* Add event handler so we can detect C-g. */
  install_menu_quit_handler (MAC_MENU_POPUP, menu);
  install_menu_quit_handler (MAC_MENU_POPUP_SUB, menu);

  record_unwind_protect (pop_down_menu, make_save_value (f, 0));

  /* Adjust coordinates to be root-window-relative.  */
  x += f->left_pos + FRAME_OUTER_TO_INNER_DIFF_X (f);
  y += f->top_pos + FRAME_OUTER_TO_INNER_DIFF_Y (f);

  /* Display the menu.  */
  popup_activated_flag = 1;
  menu_item_choice = PopUpMenuSelect (menu, y, x, 0);
  popup_activated_flag = 0;

  /* Get the refcon to find the correct item */
  if (menu_item_choice)
    {
      MenuRef sel_menu = GetMenuRef (HiWord (menu_item_choice));

      if (sel_menu)
	GetMenuItemRefCon (sel_menu, LoWord (menu_item_choice),
			   (UInt32 *) &result);
    }

  unbind_to (specpdl_count, Qnil);

  menu_item_selection = result;
}

static void
add_menu_item (menu, pos, wv)
     MenuRef menu;
     int pos;
     widget_value *wv;
{
#if TARGET_API_MAC_CARBON
  CFStringRef item_name;
#else
  Str255 item_name;
#endif

  if (name_is_separator (wv->name))
    AppendMenu (menu, "\p-");
  else
    {
      AppendMenu (menu, "\pX");

#if TARGET_API_MAC_CARBON
      item_name = cfstring_create_with_utf8_cstring (wv->name);

      if (wv->key != NULL)
	{
	  CFStringRef name, key;

	  name = item_name;
	  key = cfstring_create_with_utf8_cstring (wv->key);
	  item_name = CFStringCreateWithFormat (NULL, NULL, CFSTR ("%@ %@"),
						name, key);
	  CFRelease (name);
	  CFRelease (key);
	}

      SetMenuItemTextWithCFString (menu, pos, item_name);
      CFRelease (item_name);

      if (wv->enabled)
        EnableMenuItem (menu, pos);
      else
        DisableMenuItem (menu, pos);

      if (STRINGP (wv->help))
	SetMenuItemProperty (menu, pos, MAC_EMACS_CREATOR_CODE, 'help',
			     sizeof (Lisp_Object), &wv->help);
#else  /* ! TARGET_API_MAC_CARBON */
      item_name[sizeof (item_name) - 1] = '\0';
      strncpy (item_name, wv->name, sizeof (item_name) - 1);
      if (wv->key != NULL)
	{
	  int len = strlen (item_name);

	  strncpy (item_name + len, " ", sizeof (item_name) - 1 - len);
	  len = strlen (item_name);
	  strncpy (item_name + len, wv->key, sizeof (item_name) - 1 - len);
	}
      c2pstr (item_name);
      SetMenuItemText (menu, pos, item_name);

      if (wv->enabled)
        EnableItem (menu, pos);
      else
        DisableItem (menu, pos);
#endif  /* ! TARGET_API_MAC_CARBON */

      /* Draw radio buttons and tickboxes. */
      if (wv->selected && (wv->button_type == BUTTON_TYPE_TOGGLE
			   || wv->button_type == BUTTON_TYPE_RADIO))
	SetItemMark (menu, pos, checkMark);
      else
	SetItemMark (menu, pos, noMark);

      SetMenuItemRefCon (menu, pos, (UInt32) wv->call_data);
    }
}

/* Construct native Mac OS menu based on widget_value tree.  */

static int
fill_menu (menu, wv, kind, submenu_id)
     MenuRef menu;
     widget_value *wv;
     enum mac_menu_kind kind;
     int submenu_id;
{
  int pos;

  for (pos = 1; wv != NULL; wv = wv->next, pos++)
    {
      add_menu_item (menu, pos, wv);
      if (wv->contents && submenu_id < min_menu_id[kind + 1])
	{
	  MenuRef submenu = NewMenu (submenu_id, "\pX");

	  InsertMenu (submenu, -1);
#if TARGET_API_MAC_CARBON
	  SetMenuItemHierarchicalMenu (menu, pos, submenu);
#else
	  SetMenuItemHierarchicalID (menu, pos, submenu_id);
#endif
	  submenu_id = fill_menu (submenu, wv->contents, kind, submenu_id + 1);
	}
    }

  return submenu_id;
}

/* Fill menu bar with the items defined by WV.  If DEEP_P, consider
   the entire menu trees we supply, rather than just the menu bar item
   names.  */

void
mac_fill_menubar (wv, deep_p)
     widget_value *wv;
     int deep_p;
{
  int id, submenu_id;
#if !TARGET_API_MAC_CARBON
  int title_changed_p = 0;
#endif

  /* Clean up the menu bar when filled by the entire menu trees.  */
  if (deep_p)
    {
      dispose_menus (MAC_MENU_MENU_BAR, 0);
      dispose_menus (MAC_MENU_MENU_BAR_SUB, 0);
#if !TARGET_API_MAC_CARBON
      title_changed_p = 1;
#endif
    }

  /* Fill menu bar titles and submenus.  Reuse the existing menu bar
     titles as much as possible to minimize redraw (if !deep_p).  */
  submenu_id = min_menu_id[MAC_MENU_MENU_BAR_SUB];
  for (id = min_menu_id[MAC_MENU_MENU_BAR];
       wv != NULL && id < min_menu_id[MAC_MENU_MENU_BAR + 1];
       wv = wv->next, id++)
    {
      OSStatus err = noErr;
      MenuRef menu;
#if TARGET_API_MAC_CARBON
      CFStringRef title;

      title = CFStringCreateWithCString (NULL, wv->name,
					 kCFStringEncodingMacRoman);
#else
      Str255 title;

      strncpy (title, wv->name, 255);
      title[255] = '\0';
      c2pstr (title);
#endif

      menu = GetMenuRef (id);
      if (menu)
	{
#if TARGET_API_MAC_CARBON
	  CFStringRef old_title;

	  err = CopyMenuTitleAsCFString (menu, &old_title);
	  if (err == noErr)
	    {
	      if (CFStringCompare (title, old_title, 0) != kCFCompareEqualTo)
		{
#ifdef MAC_OSX
		  if (id + 1 == min_menu_id[MAC_MENU_MENU_BAR + 1]
		      || GetMenuRef (id + 1) == NULL)
		    {
		      /* This is a workaround for Mac OS X 10.5 where
			 just calling SetMenuTitleWithCFString fails
			 to change the title of the last (Help) menu
			 in the menu bar.  */
		      DeleteMenu (id);
		      DisposeMenu (menu);
		      menu = NULL;
		    }
		  else
#endif	/* MAC_OSX */
		    err = SetMenuTitleWithCFString (menu, title);
		}
	      CFRelease (old_title);
	    }
	  else
	    err = SetMenuTitleWithCFString (menu, title);
#else  /* !TARGET_API_MAC_CARBON */
	  if (!EqualString (title, (*menu)->menuData, false, false))
	    {
	      DeleteMenu (id);
	      DisposeMenu (menu);
	      menu = NewMenu (id, title);
	      InsertMenu (menu, GetMenuRef (id + 1) ? id + 1 : 0);
	      title_changed_p = 1;
	    }
#endif  /* !TARGET_API_MAC_CARBON */
	}

      if (!menu)
	{
#if TARGET_API_MAC_CARBON
	  err = CreateNewMenu (id, 0, &menu);
	  if (err == noErr)
	    err = SetMenuTitleWithCFString (menu, title);
#else
	  menu = NewMenu (id, title);
#endif
	  if (err == noErr)
	    {
	      InsertMenu (menu, 0);
#if !TARGET_API_MAC_CARBON
	      title_changed_p = 1;
#endif
	    }
	}
#if TARGET_API_MAC_CARBON
      CFRelease (title);
#endif

      if (err == noErr)
	if (wv->contents)
	  submenu_id = fill_menu (menu, wv->contents, MAC_MENU_MENU_BAR_SUB,
				  submenu_id);
    }

  if (id < min_menu_id[MAC_MENU_MENU_BAR + 1] && GetMenuRef (id))
    {
      dispose_menus (MAC_MENU_MENU_BAR, id);
#if !TARGET_API_MAC_CARBON
      title_changed_p = 1;
#endif
    }

#if !TARGET_API_MAC_CARBON
  if (title_changed_p)
    InvalMenuBar ();
#endif

  /* Add event handler so we can detect C-g. */
  install_menu_quit_handler (MAC_MENU_MENU_BAR, NULL);
  install_menu_quit_handler (MAC_MENU_MENU_BAR_SUB, NULL);
}

/* Dispose of menus that belong to KIND, and remove them from the menu
   list.  ID is the lower bound of menu IDs that will be processed.  */

static void
dispose_menus (kind, id)
     enum mac_menu_kind kind;
     int id;
{
  for (id = max (id, min_menu_id[kind]); id < min_menu_id[kind + 1]; id++)
    {
      MenuRef menu = GetMenuRef (id);

      if (menu == NULL)
	break;
      DeleteMenu (id);
      DisposeMenu (menu);
    }
}

static void
init_menu_bar ()
{
#ifdef MAC_OSX
  OSStatus err;
  MenuRef menu;
  MenuItemIndex menu_index;

  err = GetIndMenuItemWithCommandID (NULL, kHICommandQuit, 1,
				     &menu, &menu_index);
  if (err == noErr)
    SetMenuItemCommandKey (menu, menu_index, false, 0);
  EnableMenuCommand (NULL, kHICommandPreferences);
  err = GetIndMenuItemWithCommandID (NULL, kHICommandPreferences, 1,
				     &menu, &menu_index);
  if (err == noErr)
    {
      SetMenuItemCommandKey (menu, menu_index, false, 0);
      InsertMenuItemTextWithCFString (menu, NULL,
				      0, kMenuItemAttrSeparator, 0);
      InsertMenuItemTextWithCFString (menu, CFSTR ("About Emacs"),
				      0, 0, kHICommandAbout);
    }
#else	/* !MAC_OSX */
#if TARGET_API_MAC_CARBON
  SetMenuItemCommandID (GetMenuRef (M_APPLE), I_ABOUT, kHICommandAbout);
#endif
#endif
}


/***********************************************************************
			     Popup Dialog
 ***********************************************************************/

#if TARGET_API_MAC_CARBON
#define DIALOG_BUTTON_COMMAND_ID_OFFSET 'Bt\0\0'
#define DIALOG_BUTTON_COMMAND_ID_P(id)			\
  (((id) & ~0xffff) == DIALOG_BUTTON_COMMAND_ID_OFFSET)
#define DIALOG_BUTTON_COMMAND_ID_VALUE(id)	\
  ((id) - DIALOG_BUTTON_COMMAND_ID_OFFSET)
#define DIALOG_BUTTON_MAKE_COMMAND_ID(value)	\
  ((value) + DIALOG_BUTTON_COMMAND_ID_OFFSET)

extern EMACS_TIME timer_check P_ ((int));
static int quit_dialog_event_loop;

static pascal OSStatus
mac_handle_dialog_event (next_handler, event, data)
     EventHandlerCallRef next_handler;
     EventRef event;
     void *data;
{
  OSStatus err, result = eventNotHandledErr;
  WindowRef window = (WindowRef) data;

  switch (GetEventClass (event))
    {
    case kEventClassCommand:
      {
	HICommand command;

	err = GetEventParameter (event, kEventParamDirectObject,
				 typeHICommand, NULL, sizeof (HICommand),
				 NULL, &command);
	if (err == noErr)
	  if (DIALOG_BUTTON_COMMAND_ID_P (command.commandID))
	    {
	      SetWRefCon (window, command.commandID);
	      quit_dialog_event_loop = 1;
	      break;
	    }

	result = CallNextEventHandler (next_handler, event);
      }
      break;

    case kEventClassKeyboard:
      {
	OSStatus result;
	char char_code;

	result = CallNextEventHandler (next_handler, event);
	if (result != eventNotHandledErr)
	  break;

	err = GetEventParameter (event, kEventParamKeyMacCharCodes,
				 typeChar, NULL, sizeof (char),
				 NULL, &char_code);
	if (err == noErr)
	  switch (char_code)
	    {
	    case kEscapeCharCode:
	      quit_dialog_event_loop = 1;
	      break;

	    default:
	      {
		UInt32 modifiers, key_code;

		err = GetEventParameter (event, kEventParamKeyModifiers,
					 typeUInt32, NULL, sizeof (UInt32),
					 NULL, &modifiers);
		if (err == noErr)
		  err = GetEventParameter (event, kEventParamKeyCode,
					   typeUInt32, NULL, sizeof (UInt32),
					   NULL, &key_code);
		if (err == noErr)
		  if (mac_quit_char_key_p (modifiers, key_code))
		    quit_dialog_event_loop = 1;
	      }
	      break;
	    }
      }
      break;

    default:
      abort ();
    }

  if (quit_dialog_event_loop)
    {
      err = QuitEventLoop (GetCurrentEventLoop ());
      if (err == noErr)
	result = noErr;
    }

  return result;
}

static OSStatus
install_dialog_event_handler (window)
     WindowRef window;
{
  static const EventTypeSpec specs[] =
    {{kEventClassCommand, kEventCommandProcess},
     {kEventClassKeyboard, kEventRawKeyDown}};
  static EventHandlerUPP handle_dialog_eventUPP = NULL;

  if (handle_dialog_eventUPP == NULL)
    handle_dialog_eventUPP = NewEventHandlerUPP (mac_handle_dialog_event);
  return InstallWindowEventHandler (window, handle_dialog_eventUPP,
				    GetEventTypeCount (specs), specs,
				    window, NULL);
}

static Lisp_Object
pop_down_dialog (arg)
     Lisp_Object arg;
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  WindowRef window = p->pointer;

  BLOCK_INPUT;

  if (popup_activated_flag)
    EndAppModalStateForWindow (window);
  DisposeWindow (window);
  popup_activated_flag = 0;

  UNBLOCK_INPUT;

  return Qnil;
}

/* Pop up the dialog for frame F defined by FIRST_WV and loop until the
   dialog pops down.
   menu_item_selection will be set to the selection.  */

void
create_and_show_dialog (f, first_wv)
     FRAME_PTR f;
     widget_value *first_wv;
{
  OSStatus err;
  char *dialog_name, *message;
  int nb_buttons, first_group_count, i, result = 0;
  widget_value *wv;
  short buttons_height, text_height, inner_width, inner_height;
  Rect empty_rect, *rects;
  WindowRef window = NULL;
  ControlRef *buttons, default_button = NULL, text;
  int specpdl_count = SPECPDL_INDEX ();

  dialog_name = first_wv->name;
  nb_buttons = dialog_name[1] - '0';
  first_group_count = nb_buttons - (dialog_name[4] - '0');

  wv = first_wv->contents;
  message = wv->value;

  wv = wv->next;
  SetRect (&empty_rect, 0, 0, 0, 0);

  /* Create dialog window.  */
  err = CreateNewWindow (kMovableModalWindowClass,
			 kWindowStandardHandlerAttribute,
			 &empty_rect, &window);
  if (err == noErr)
    {
      record_unwind_protect (pop_down_dialog, make_save_value (window, 0));
      err = SetThemeWindowBackground (window, kThemeBrushMovableModalBackground,
				      true);
    }
  if (err == noErr)
    err = SetWindowTitleWithCFString (window, (dialog_name[0] == 'Q'
					       ? CFSTR ("Question")
					       : CFSTR ("Information")));

  /* Create button controls and measure their optimal bounds.  */
  if (err == noErr)
    {
      buttons = alloca (sizeof (ControlRef) * nb_buttons);
      rects = alloca (sizeof (Rect) * nb_buttons);
      for (i = 0; i < nb_buttons; i++)
	{
	  CFStringRef label = cfstring_create_with_utf8_cstring (wv->value);

	  if (label == NULL)
	    err = memFullErr;
	  else
	    {
	      err = CreatePushButtonControl (window, &empty_rect,
					     label, &buttons[i]);
	      CFRelease (label);
	    }
	  if (err == noErr)
	    {
	      if (!wv->enabled)
		{
#ifdef MAC_OSX
		  err = DisableControl (buttons[i]);
#else
		  err = DeactivateControl (buttons[i]);
#endif
		}
	      else if (default_button == NULL)
		default_button = buttons[i];
	    }
	  if (err == noErr)
	    {
	      SInt16 unused;

	      rects[i] = empty_rect;
	      err = GetBestControlRect (buttons[i], &rects[i], &unused);
	    }
	  if (err == noErr)
	    {
	      UInt32 command_id;

	      OffsetRect (&rects[i], -rects[i].left, -rects[i].top);
	      if (rects[i].right < DIALOG_BUTTON_MIN_WIDTH)
		rects[i].right = DIALOG_BUTTON_MIN_WIDTH;
	      else if (rects[i].right > DIALOG_MAX_INNER_WIDTH)
		rects[i].right = DIALOG_MAX_INNER_WIDTH;

	      command_id = DIALOG_BUTTON_MAKE_COMMAND_ID ((int) wv->call_data);
	      err = SetControlCommandID (buttons[i], command_id);
	    }
	  if (err != noErr)
	    break;
	  wv = wv->next;
	}
    }

  /* Layout buttons.  rects[i] is set relative to the bottom-right
     corner of the inner box.  */
  if (err == noErr)
    {
      short bottom, right, max_height, left_align_shift;

      inner_width = DIALOG_MIN_INNER_WIDTH;
      bottom = right = max_height = 0;
      for (i = 0; i < nb_buttons; i++)
	{
	  if (right - rects[i].right < - inner_width)
	    {
	      if (i != first_group_count
		  && right - rects[i].right >= - DIALOG_MAX_INNER_WIDTH)
		inner_width = - (right - rects[i].right);
	      else
		{
		  bottom -= max_height + DIALOG_BUTTON_BUTTON_VERTICAL_SPACE;
		  right = max_height = 0;
		}
	    }
	  if (max_height < rects[i].bottom)
	    max_height = rects[i].bottom;
	  OffsetRect (&rects[i], right - rects[i].right,
		      bottom - rects[i].bottom);
	  right = rects[i].left - DIALOG_BUTTON_BUTTON_HORIZONTAL_SPACE;
	  if (i == first_group_count - 1)
	    right -= DIALOG_BUTTON_BUTTON_HORIZONTAL_SPACE;
	}
      buttons_height = - (bottom - max_height);

      left_align_shift = - (inner_width + rects[nb_buttons - 1].left);
      for (i = nb_buttons - 1; i >= first_group_count; i--)
	{
	  if (bottom != rects[i].bottom)
	    {
	      left_align_shift = - (inner_width + rects[i].left);
	      bottom = rects[i].bottom;
	    }
	  OffsetRect (&rects[i], left_align_shift, 0);
	}
    }

  /* Create a static text control and measure its bounds.  */
  if (err == noErr)
    {
      CFStringRef message_string;
      Rect bounds;

      message_string = cfstring_create_with_utf8_cstring (message);
      if (message_string == NULL)
	err = memFullErr;
      else
	{
	  ControlFontStyleRec text_style;

	  text_style.flags = 0;
	  SetRect (&bounds, 0, 0, inner_width, 0);
	  err = CreateStaticTextControl (window, &bounds, message_string,
					 &text_style, &text);
	  CFRelease (message_string);
	}
      if (err == noErr)
	{
	  SInt16 unused;

	  bounds = empty_rect;
	  err = GetBestControlRect (text, &bounds, &unused);
	}
      if (err == noErr)
	{
	  text_height = bounds.bottom - bounds.top;
	  if (text_height < DIALOG_TEXT_MIN_HEIGHT)
	    text_height = DIALOG_TEXT_MIN_HEIGHT;
	}
    }

  /* Place buttons. */
  if (err == noErr)
    {
      inner_height = (text_height + DIALOG_TEXT_BUTTONS_VERTICAL_SPACE
		      + buttons_height);

      for (i = 0; i < nb_buttons; i++)
	{
	  OffsetRect (&rects[i], DIALOG_LEFT_MARGIN + inner_width,
		      DIALOG_TOP_MARGIN + inner_height);
	  SetControlBounds (buttons[i], &rects[i]);
	}
    }

  /* Place text.  */
  if (err == noErr)
    {
      Rect bounds;

      SetRect (&bounds, DIALOG_LEFT_MARGIN, DIALOG_TOP_MARGIN,
	       DIALOG_LEFT_MARGIN + inner_width,
	       DIALOG_TOP_MARGIN + text_height);
      SetControlBounds (text, &bounds);
    }

  /* Create the application icon at the upper-left corner.  */
  if (err == noErr)
    {
      ControlButtonContentInfo content;
      ControlRef icon;
      static const ProcessSerialNumber psn = {0, kCurrentProcess};
#ifdef MAC_OSX
      FSRef app_location;
#else
      ProcessInfoRec pinfo;
      FSSpec app_spec;
#endif
      SInt16 unused;

      content.contentType = kControlContentIconRef;
#ifdef MAC_OSX
      err = GetProcessBundleLocation (&psn, &app_location);
      if (err == noErr)
	err = GetIconRefFromFileInfo (&app_location, 0, NULL, 0, NULL,
				      kIconServicesNormalUsageFlag,
				      &content.u.iconRef, &unused);
#else
      bzero (&pinfo, sizeof (ProcessInfoRec));
      pinfo.processInfoLength = sizeof (ProcessInfoRec);
      pinfo.processAppSpec = &app_spec;
      err = GetProcessInformation (&psn, &pinfo);
      if (err == noErr)
	err = GetIconRefFromFile (&app_spec, &content.u.iconRef, &unused);
#endif
      if (err == noErr)
	{
	  Rect bounds;

	  SetRect (&bounds, DIALOG_ICON_LEFT_MARGIN, DIALOG_ICON_TOP_MARGIN,
		   DIALOG_ICON_LEFT_MARGIN + DIALOG_ICON_WIDTH,
		   DIALOG_ICON_TOP_MARGIN + DIALOG_ICON_HEIGHT);
	  err = CreateIconControl (window, &bounds, &content, true, &icon);
	  ReleaseIconRef (content.u.iconRef);
	}
    }

  /* Show the dialog window and run event loop.  */
  if (err == noErr)
    if (default_button)
      err = SetWindowDefaultButton (window, default_button);
  if (err == noErr)
    err = install_dialog_event_handler (window);
  if (err == noErr)
    {
      SizeWindow (window,
		  DIALOG_LEFT_MARGIN + inner_width + DIALOG_RIGHT_MARGIN,
		  DIALOG_TOP_MARGIN + inner_height + DIALOG_BOTTOM_MARGIN,
		  true);
      err = RepositionWindow (window, FRAME_MAC_WINDOW (f),
			      kWindowAlertPositionOnParentWindow);
    }
  if (err == noErr)
    {
      SetWRefCon (window, 0);
      ShowWindow (window);
      BringToFront (window);
      popup_activated_flag = 1;
      err = BeginAppModalStateForWindow (window);
    }
  if (err == noErr)
    {
      EventTargetRef toolbox_dispatcher = GetEventDispatcherTarget ();

      quit_dialog_event_loop = 0;
      while (1)
	{
	  EMACS_TIME next_time = timer_check (1);
	  long secs = EMACS_SECS (next_time);
	  long usecs = EMACS_USECS (next_time);
	  EventTimeout timeout;
	  EventRef event;

	  if (secs < 0 || (secs == 0 && usecs == 0))
	    {
	      /* Sometimes timer_check returns -1 (no timers) even if
		 there are timers.  So do a timeout anyway.  */
	      secs = 1;
	      usecs = 0;
	    }

	  timeout = (secs * kEventDurationSecond
		     + usecs * kEventDurationMicrosecond);
	  err = ReceiveNextEvent (0, NULL, timeout, kEventRemoveFromQueue,
				  &event);
	  if (err == noErr)
	    {
	      SendEventToEventTarget (event, toolbox_dispatcher);
	      ReleaseEvent (event);
	    }
#if 0 /* defined (MAC_OSX) */
	  else if (err != eventLoopTimedOutErr)
	    {
	      if (err == eventLoopQuitErr)
		err = noErr;
	      break;
	    }
#else
	  /* The return value of ReceiveNextEvent seems to be
	     unreliable.  Use our own global variable instead.  */
	  if (quit_dialog_event_loop)
	    {
	      err = noErr;
	      break;
	    }
#endif
	}
    }
  if (err == noErr)
    {
      UInt32 command_id = GetWRefCon (window);

      if (DIALOG_BUTTON_COMMAND_ID_P (command_id))
	result = DIALOG_BUTTON_COMMAND_ID_VALUE (command_id);
    }

  unbind_to (specpdl_count, Qnil);

  menu_item_selection = result;
}
#else  /* not TARGET_API_MAC_CARBON */
#define DIALOG_WINDOW_RESOURCE 130

int
mac_dialog (widget_value *wv)
{
  char *dialog_name;
  char *prompt;
  char **button_labels;
  UInt32 *ref_cons;
  int nb_buttons;
  int left_count;
  int i;
  int dialog_width;
  Rect rect;
  WindowRef window_ptr;
  ControlRef ch;
  int left;
  EventRecord event_record;
  SInt16 part_code;
  int control_part_code;
  Point mouse;

  dialog_name = wv->name;
  nb_buttons = dialog_name[1] - '0';
  left_count = nb_buttons - (dialog_name[4] - '0');
  button_labels = (char **) alloca (sizeof (char *) * nb_buttons);
  ref_cons = (UInt32 *) alloca (sizeof (UInt32) * nb_buttons);

  wv = wv->contents;
  prompt = (char *) alloca (strlen (wv->value) + 1);
  strcpy (prompt, wv->value);
  c2pstr (prompt);

  wv = wv->next;
  for (i = 0; i < nb_buttons; i++)
    {
      button_labels[i] = wv->value;
      button_labels[i] = (char *) alloca (strlen (wv->value) + 1);
      strcpy (button_labels[i], wv->value);
      c2pstr (button_labels[i]);
      ref_cons[i] = (UInt32) wv->call_data;
      wv = wv->next;
    }

  window_ptr = GetNewCWindow (DIALOG_WINDOW_RESOURCE, NULL, (WindowRef) -1);

  SetPortWindowPort (window_ptr);

  TextFont (0);
  /* Left and right margins in the dialog are 13 pixels each.*/
  dialog_width = 14;
  /* Calculate width of dialog box: 8 pixels on each side of the text
     label in each button, 12 pixels between buttons.  */
  for (i = 0; i < nb_buttons; i++)
    dialog_width +=  StringWidth (button_labels[i]) + 16 + 12;

  if (left_count != 0 && nb_buttons - left_count != 0)
    dialog_width += 12;

  dialog_width = max (dialog_width, StringWidth (prompt) + 26);

  SizeWindow (window_ptr, dialog_width, 78, 0);
  ShowWindow (window_ptr);

  SetPortWindowPort (window_ptr);

  TextFont (0);

  MoveTo (13, 29);
  DrawString (prompt);

  left = 13;
  for (i = 0; i < nb_buttons; i++)
    {
      int button_width = StringWidth (button_labels[i]) + 16;
      SetRect (&rect, left, 45, left + button_width, 65);
      ch = NewControl (window_ptr, &rect, button_labels[i], 1, 0, 0, 0,
                       kControlPushButtonProc, ref_cons[i]);
      left += button_width + 12;
      if (i == left_count - 1)
        left += 12;
    }

  i = 0;
  while (!i)
    {
      if (WaitNextEvent (mDownMask, &event_record, 10, NULL))
        if (event_record.what == mouseDown)
          {
            part_code = FindWindow (event_record.where, &window_ptr);
            if (part_code == inContent)
              {
                mouse = event_record.where;
                GlobalToLocal (&mouse);
                control_part_code = FindControl (mouse, window_ptr, &ch);
                if (control_part_code == kControlButtonPart)
                  if (TrackControl (ch, mouse, NULL))
                    i = GetControlReference (ch);
              }
          }
    }

  DisposeWindow (window_ptr);

  return i;
}
#endif  /* not TARGET_API_MAC_CARBON */


/***********************************************************************
			  Selection support
***********************************************************************/

#if !TARGET_API_MAC_CARBON
#include <Scrap.h>
#include <Endian.h>
#endif

extern Lisp_Object Vselection_converter_alist;
extern Lisp_Object Qmac_scrap_name, Qmac_ostype;

static ScrapFlavorType get_flavor_type_from_symbol P_ ((Lisp_Object,
							Selection));

/* Get a reference to the selection corresponding to the symbol SYM.
   The reference is set to *SEL, and it becomes NULL if there's no
   corresponding selection.  Clear the selection if CLEAR_P is
   non-zero.  */

OSStatus
mac_get_selection_from_symbol (sym, clear_p, sel)
     Lisp_Object sym;
     int clear_p;
     Selection *sel;
{
  OSStatus err = noErr;
  Lisp_Object str = Fget (sym, Qmac_scrap_name);

  if (!STRINGP (str))
    *sel = NULL;
  else
    {
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
      CFStringRef scrap_name = cfstring_create_with_string (str);
      OptionBits options = (clear_p ? kScrapClearNamedScrap
			    : kScrapGetNamedScrap);

      err = GetScrapByName (scrap_name, options, sel);
      CFRelease (scrap_name);
#else	/* !MAC_OSX */
      if (clear_p)
	err = ClearCurrentScrap ();
      if (err == noErr)
	err = GetCurrentScrap (sel);
#endif	/* !MAC_OSX */
#else	/* !TARGET_API_MAC_CARBON */
      if (clear_p)
	err = ZeroScrap ();
      if (err == noErr)
	*sel = 1;
#endif	/* !TARGET_API_MAC_CARBON */
    }

  return err;
}

/* Get a scrap flavor type from the symbol SYM.  Return 0 if no
   corresponding flavor type.  If SEL is non-zero, the return value is
   non-zero only when the SEL has the flavor type.  */

static ScrapFlavorType
get_flavor_type_from_symbol (sym, sel)
     Lisp_Object sym;
     Selection sel;
{
  Lisp_Object str = Fget (sym, Qmac_ostype);
  ScrapFlavorType flavor_type;

  if (STRINGP (str) && SBYTES (str) == 4)
    flavor_type = EndianU32_BtoN (*((UInt32 *) SDATA (str)));
  else
    flavor_type = 0;

  if (flavor_type && sel)
    {
#if TARGET_API_MAC_CARBON
      OSStatus err;
      ScrapFlavorFlags flags;

      err = GetScrapFlavorFlags (sel, flavor_type, &flags);
      if (err != noErr)
	flavor_type = 0;
#else  /* !TARGET_API_MAC_CARBON */
      SInt32 size, offset;

      size = GetScrap (NULL, flavor_type, &offset);
      if (size < 0)
	flavor_type = 0;
#endif	/* !TARGET_API_MAC_CARBON */
    }

  return flavor_type;
}

/* Check if the symbol SYM has a corresponding selection target type.  */

int
mac_valid_selection_target_p (sym)
     Lisp_Object sym;
{
  return get_flavor_type_from_symbol (sym, 0) != 0;
}

/* Clear the selection whose reference is *SEL.  */

OSStatus
mac_clear_selection (sel)
     Selection *sel;
{
#if TARGET_API_MAC_CARBON
#ifdef MAC_OSX
  return ClearScrap (sel);
#else
  OSStatus err;

  err = ClearCurrentScrap ();
  if (err == noErr)
    err = GetCurrentScrap (sel);
  return err;
#endif
#else  /* !TARGET_API_MAC_CARBON */
  return ZeroScrap ();
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Get ownership information for SEL.  Emacs can detect a change of
   the ownership by comparing saved and current values of the
   ownership information.  */

Lisp_Object
mac_get_selection_ownership_info (sel)
     Selection sel;
{
#if TARGET_API_MAC_CARBON
  return long_to_cons ((unsigned long) sel);
#else  /* !TARGET_API_MAC_CARBON */
  ScrapStuffPtr scrap_info = InfoScrap ();

  return make_number (scrap_info->scrapCount);
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Return non-zero if VALUE is a valid selection value for TARGET.  */

int
mac_valid_selection_value_p (value, target)
     Lisp_Object value, target;
{
  return STRINGP (value);
}

/* Put Lisp object VALUE to the selection SEL.  The target type is
   specified by TARGET. */

OSStatus
mac_put_selection_value (sel, target, value)
     Selection sel;
     Lisp_Object target, value;
{
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (target, 0);

  if (flavor_type == 0 || !STRINGP (value))
    return noTypeErr;

#if TARGET_API_MAC_CARBON
  return PutScrapFlavor (sel, flavor_type, kScrapFlavorMaskNone,
			 SBYTES (value), SDATA (value));
#else  /* !TARGET_API_MAC_CARBON */
  return PutScrap (SBYTES (value), flavor_type, SDATA (value));
#endif	/* !TARGET_API_MAC_CARBON */
}

/* Check if data for the target type TARGET is available in SEL.  */

int
mac_selection_has_target_p (sel, target)
     Selection sel;
     Lisp_Object target;
{
  return get_flavor_type_from_symbol (target, sel) != 0;
}

/* Get data for the target type TARGET from SEL and create a Lisp
   string.  Return nil if failed to get data.  */

Lisp_Object
mac_get_selection_value (sel, target)
     Selection sel;
     Lisp_Object target;
{
  OSStatus err;
  Lisp_Object result = Qnil;
  ScrapFlavorType flavor_type = get_flavor_type_from_symbol (target, sel);
#if TARGET_API_MAC_CARBON
  Size size;

  if (flavor_type)
    {
      err = GetScrapFlavorSize (sel, flavor_type, &size);
      if (err == noErr)
	{
	  do
	    {
	      result = make_uninit_string (size);
	      err = GetScrapFlavorData (sel, flavor_type,
					&size, SDATA (result));
	      if (err != noErr)
		result = Qnil;
	      else if (size < SBYTES (result))
		result = make_unibyte_string (SDATA (result), size);
	    }
	  while (STRINGP (result) && size > SBYTES (result));
	}
    }
#else
  Handle handle;
  SInt32 size, offset;

  if (flavor_type)
    size = GetScrap (NULL, flavor_type, &offset);
  if (size >= 0)
    {
      handle = NewHandle (size);
      HLock (handle);
      size = GetScrap (handle, flavor_type, &offset);
      if (size >= 0)
	result = make_unibyte_string (*handle, size);
      DisposeHandle (handle);
    }
#endif

  return result;
}

/* Get the list of target types in SEL.  The return value is a list of
   target type symbols possibly followed by scrap flavor type
   strings.  */

Lisp_Object
mac_get_selection_target_list (sel)
     Selection sel;
{
  Lisp_Object result = Qnil, rest, target;
#if TARGET_API_MAC_CARBON
  OSStatus err;
  UInt32 count, i, type;
  ScrapFlavorInfo *flavor_info = NULL;
  Lisp_Object strings = Qnil;

  err = GetScrapFlavorCount (sel, &count);
  if (err == noErr)
    flavor_info = xmalloc (sizeof (ScrapFlavorInfo) * count);
  err = GetScrapFlavorInfoList (sel, &count, flavor_info);
  if (err != noErr)
    {
      xfree (flavor_info);
      flavor_info = NULL;
    }
  if (flavor_info == NULL)
    count = 0;
#endif
  for (rest = Vselection_converter_alist; CONSP (rest); rest = XCDR (rest))
    {
      ScrapFlavorType flavor_type = 0;

      if (CONSP (XCAR (rest))
	  && (target = XCAR (XCAR (rest)),
	      SYMBOLP (target))
	  && (flavor_type = get_flavor_type_from_symbol (target, sel)))
	{
	  result = Fcons (target, result);
#if TARGET_API_MAC_CARBON
	  for (i = 0; i < count; i++)
	    if (flavor_info[i].flavorType == flavor_type)
	      {
		flavor_info[i].flavorType = 0;
		break;
	      }
#endif
	}
    }
#if TARGET_API_MAC_CARBON
  if (flavor_info)
    {
      for (i = 0; i < count; i++)
	if (flavor_info[i].flavorType)
	  {
	    type = EndianU32_NtoB (flavor_info[i].flavorType);
	    strings = Fcons (make_unibyte_string ((char *) &type, 4), strings);
	  }
      result = nconc2 (result, strings);
      xfree (flavor_info);
    }
#endif

  return result;
}


/***********************************************************************
			 Apple event support
***********************************************************************/

extern pascal OSErr mac_handle_apple_event P_ ((const AppleEvent *,
						AppleEvent *, SInt32));
extern void cleanup_all_suspended_apple_events P_ ((void));

void
init_apple_event_handler ()
{
  OSErr err;
  long result;

  /* Make sure we have Apple events before starting.  */
  err = Gestalt (gestaltAppleEventsAttr, &result);
  if (err != noErr)
    abort ();

  if (!(result & (1 << gestaltAppleEventsPresent)))
    abort ();

  err = AEInstallEventHandler (typeWildCard, typeWildCard,
#if TARGET_API_MAC_CARBON
			       NewAEEventHandlerUPP (mac_handle_apple_event),
#else
			       NewAEEventHandlerProc (mac_handle_apple_event),
#endif
			       0L, false);
  if (err != noErr)
    abort ();

  atexit (cleanup_all_suspended_apple_events);
}


/***********************************************************************
                      Drag and drop support
***********************************************************************/

#if TARGET_API_MAC_CARBON
extern Lisp_Object Vmac_dnd_known_types;

static pascal OSErr mac_do_track_drag P_ ((DragTrackingMessage, WindowRef,
					   void *, DragRef));
static pascal OSErr mac_do_receive_drag P_ ((WindowRef, void *, DragRef));
static DragTrackingHandlerUPP mac_do_track_dragUPP = NULL;
static DragReceiveHandlerUPP mac_do_receive_dragUPP = NULL;

static OSErr
create_apple_event_from_drag_ref (drag, num_types, types, result)
     DragRef drag;
     UInt32 num_types;
     const FlavorType *types;
     AppleEvent *result;
{
  OSErr err;
  UInt16 num_items;
  AppleEvent items;
  long index;
  char *buf = NULL;

  err = CountDragItems (drag, &num_items);
  if (err != noErr)
    return err;
  err = AECreateList (NULL, 0, false, &items);
  if (err != noErr)
    return err;

  for (index = 1; index <= num_items; index++)
    {
      ItemReference item;
      DescType desc_type = typeNull;
      Size size;

      err = GetDragItemReferenceNumber (drag, index, &item);
      if (err == noErr)
	{
	  int i;

	  for (i = 0; i < num_types; i++)
	    {
	      err = GetFlavorDataSize (drag, item, types[i], &size);
	      if (err == noErr)
		{
		  buf = xrealloc (buf, size);
		  err = GetFlavorData (drag, item, types[i], buf, &size, 0);
		}
	      if (err == noErr)
		{
		  desc_type = types[i];
		  break;
		}
	    }
	}
      err = AEPutPtr (&items, index, desc_type,
		      desc_type != typeNull ? buf : NULL,
		      desc_type != typeNull ? size : 0);
      if (err != noErr)
	break;
    }
  if (buf)
    xfree (buf);

  if (err == noErr)
    {
      err = create_apple_event (0, 0, result); /* Dummy class and ID.  */
      if (err == noErr)
	err = AEPutParamDesc (result, keyDirectObject, &items);
      if (err != noErr)
	AEDisposeDesc (result);
    }

  AEDisposeDesc (&items);

  return err;
}

static void
mac_store_drag_event (window, mouse_pos, modifiers, desc)
     WindowRef window;
     Point mouse_pos;
     SInt16 modifiers;
     const AEDesc *desc;
{
  struct input_event buf;

  EVENT_INIT (buf);

  buf.kind = DRAG_N_DROP_EVENT;
  buf.modifiers = mac_to_emacs_modifiers (modifiers, 0);
  buf.timestamp = TickCount () * (1000 / 60);
  XSETINT (buf.x, mouse_pos.h);
  XSETINT (buf.y, mouse_pos.v);
  XSETFRAME (buf.frame_or_window, mac_window_to_frame (window));
  buf.arg = mac_aedesc_to_lisp (desc);
  kbd_buffer_store_event (&buf);
}

static pascal OSErr
mac_do_track_drag (message, window, refcon, drag)
     DragTrackingMessage message;
     WindowRef window;
     void *refcon;
     DragRef drag;
{
  OSErr err = noErr;
  static int can_accept;
  UInt16 num_items, index;

  if (GetFrontWindowOfClass (kMovableModalWindowClass, false))
    return dragNotAcceptedErr;

  switch (message)
    {
    case kDragTrackingEnterHandler:
      err = CountDragItems (drag, &num_items);
      if (err != noErr)
	break;
      can_accept = 0;
      for (index = 1; index <= num_items; index++)
	{
	  ItemReference item;
	  FlavorFlags flags;
	  Lisp_Object rest;

	  err = GetDragItemReferenceNumber (drag, index, &item);
	  if (err != noErr)
	    continue;
	  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
	    {
	      Lisp_Object str;
	      FlavorType type;

	      str = XCAR (rest);
	      if (!(STRINGP (str) && SBYTES (str) == 4))
		continue;
	      type = EndianU32_BtoN (*((UInt32 *) SDATA (str)));

	      err = GetFlavorFlags (drag, item, type, &flags);
	      if (err == noErr)
		{
		  can_accept = 1;
		  break;
		}
	    }
	}
      break;

    case kDragTrackingEnterWindow:
      if (can_accept)
	{
	  RgnHandle hilite_rgn = NewRgn ();

	  if (hilite_rgn)
	    {
	      Rect r;

	      GetWindowPortBounds (window, &r);
	      OffsetRect (&r, -r.left, -r.top);
	      RectRgn (hilite_rgn, &r);
	      ShowDragHilite (drag, hilite_rgn, true);
	      DisposeRgn (hilite_rgn);
	    }
	  SetThemeCursor (kThemeCopyArrowCursor);
	}
      break;

    case kDragTrackingInWindow:
      break;

    case kDragTrackingLeaveWindow:
      if (can_accept)
	{
	  HideDragHilite (drag);
	  SetThemeCursor (kThemeArrowCursor);
	}
      break;

    case kDragTrackingLeaveHandler:
      break;
    }

  if (err != noErr)
    return dragNotAcceptedErr;
  return noErr;
}

static pascal OSErr
mac_do_receive_drag (window, refcon, drag)
     WindowRef window;
     void *refcon;
     DragRef drag;
{
  OSErr err;
  int num_types, i;
  Lisp_Object rest, str;
  FlavorType *types;
  AppleEvent apple_event;
  Point mouse_pos;
  SInt16 modifiers;

  if (GetFrontWindowOfClass (kMovableModalWindowClass, false))
    return dragNotAcceptedErr;

  num_types = 0;
  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
    {
      str = XCAR (rest);
      if (STRINGP (str) && SBYTES (str) == 4)
	num_types++;
    }

  types = xmalloc (sizeof (FlavorType) * num_types);
  i = 0;
  for (rest = Vmac_dnd_known_types; CONSP (rest); rest = XCDR (rest))
    {
      str = XCAR (rest);
      if (STRINGP (str) && SBYTES (str) == 4)
	types[i++] = EndianU32_BtoN (*((UInt32 *) SDATA (str)));
    }

  err = create_apple_event_from_drag_ref (drag, num_types, types,
					  &apple_event);
  xfree (types);

  if (err == noErr)
    err = GetDragMouse (drag, &mouse_pos, NULL);
  if (err == noErr)
    {
      GlobalToLocal (&mouse_pos);
      err = GetDragModifiers (drag, NULL, NULL, &modifiers);
    }
  if (err == noErr)
    {
      UInt32 key_modifiers = modifiers;

      err = AEPutParamPtr (&apple_event, kEventParamKeyModifiers,
			   typeUInt32, &key_modifiers, sizeof (UInt32));
    }

  if (err == noErr)
    {
      mac_store_drag_event (window, mouse_pos, 0, &apple_event);
      AEDisposeDesc (&apple_event);
      mac_wakeup_from_rne ();
      return noErr;
    }
  else
    return dragNotAcceptedErr;
}
#endif	/* TARGET_API_MAC_CARBON */

static OSErr
install_drag_handler (window)
     WindowRef window;
{
  OSErr err = noErr;

#if TARGET_API_MAC_CARBON
  if (mac_do_track_dragUPP == NULL)
    mac_do_track_dragUPP = NewDragTrackingHandlerUPP (mac_do_track_drag);
  if (mac_do_receive_dragUPP == NULL)
    mac_do_receive_dragUPP = NewDragReceiveHandlerUPP (mac_do_receive_drag);

  err = InstallTrackingHandler (mac_do_track_dragUPP, window, NULL);
  if (err == noErr)
    err = InstallReceiveHandler (mac_do_receive_dragUPP, window, NULL);
#endif

  return err;
}

static void
remove_drag_handler (window)
     WindowRef window;
{
#if TARGET_API_MAC_CARBON
  if (mac_do_track_dragUPP)
    RemoveTrackingHandler (mac_do_track_dragUPP, window);
  if (mac_do_receive_dragUPP)
    RemoveReceiveHandler (mac_do_receive_dragUPP, window);
#endif
}

#if TARGET_API_MAC_CARBON
/* Return default value for mac-dnd-known-types.  */

Lisp_Object
mac_dnd_default_known_types ()
{
  Lisp_Object result = list4 (build_string ("hfs "), build_string ("utxt"),
			      build_string ("TEXT"), build_string ("TIFF"));

#ifdef MAC_OSX
  result = Fcons (build_string ("furl"), result);
#endif

  return result;
}
#endif


/***********************************************************************
			Services menu support
***********************************************************************/

#ifdef MAC_OSX
extern Lisp_Object Qservice, Qpaste, Qperform;
extern Lisp_Object Vmac_service_selection;

static OSStatus
mac_store_service_event (event)
     EventRef event;
{
  OSStatus err;
  Lisp_Object id_key;
  int num_params;
  const EventParamName *names;
  const EventParamType *types;
  static const EventParamName names_pfm[] =
    {kEventParamServiceMessageName, kEventParamServiceUserData};
  static const EventParamType types_pfm[] =
    {typeCFStringRef, typeCFStringRef};

  switch (GetEventKind (event))
    {
    case kEventServicePaste:
      id_key = Qpaste;
      num_params = 0;
      names = NULL;
      types = NULL;
      break;

    case kEventServicePerform:
      id_key = Qperform;
      num_params = sizeof (names_pfm) / sizeof (names_pfm[0]);
      names = names_pfm;
      types = types_pfm;
      break;

    default:
      abort ();
    }

  err = mac_store_event_ref_as_apple_event (0, 0, Qservice, id_key,
					    event, num_params,
					    names, types);

  return err;
}

static OSStatus
copy_scrap_flavor_data (from_scrap, to_scrap, flavor_type)
     ScrapRef from_scrap, to_scrap;
     ScrapFlavorType flavor_type;
{
  OSStatus err;
  Size size, size_allocated;
  char *buf = NULL;

  err = GetScrapFlavorSize (from_scrap, flavor_type, &size);
  if (err == noErr)
    buf = xmalloc (size);
  while (buf)
    {
      size_allocated = size;
      err = GetScrapFlavorData (from_scrap, flavor_type, &size, buf);
      if (err != noErr)
	{
	  xfree (buf);
	  buf = NULL;
	}
      else if (size_allocated < size)
	buf = xrealloc (buf, size);
      else
	break;
    }
  if (err == noErr)
    {
      if (buf == NULL)
	err = memFullErr;
      else
	{
	  err = PutScrapFlavor (to_scrap, flavor_type, kScrapFlavorMaskNone,
				size, buf);
	  xfree (buf);
	}
    }

  return err;
}

static OSStatus
mac_handle_service_event (call_ref, event, data)
     EventHandlerCallRef call_ref;
     EventRef event;
     void *data;
{
  OSStatus err = noErr;
  ScrapRef cur_scrap, specific_scrap;
  UInt32 event_kind = GetEventKind (event);
  CFMutableArrayRef copy_types, paste_types;
  CFStringRef type;
  Lisp_Object rest;
  ScrapFlavorType flavor_type;

  /* Check if Vmac_service_selection is a valid selection that has a
     corresponding scrap.  */
  if (!SYMBOLP (Vmac_service_selection))
    err = eventNotHandledErr;
  else
    err = mac_get_selection_from_symbol (Vmac_service_selection, 0, &cur_scrap);
  if (!(err == noErr && cur_scrap))
    return eventNotHandledErr;

  switch (event_kind)
    {
    case kEventServiceGetTypes:
      /* Set paste types. */
      err = GetEventParameter (event, kEventParamServicePasteTypes,
			       typeCFMutableArrayRef, NULL,
			       sizeof (CFMutableArrayRef), NULL,
			       &paste_types);
      if (err != noErr)
	break;

      for (rest = Vselection_converter_alist; CONSP (rest);
	   rest = XCDR (rest))
	if (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))
	    && (flavor_type =
		get_flavor_type_from_symbol (XCAR (XCAR (rest)), 0)))
	  {
	    type = CreateTypeStringWithOSType (flavor_type);
	    if (type)
	      {
		CFArrayAppendValue (paste_types, type);
		CFRelease (type);
	      }
	  }

      /* Set copy types.  */
      err = GetEventParameter (event, kEventParamServiceCopyTypes,
			       typeCFMutableArrayRef, NULL,
			       sizeof (CFMutableArrayRef), NULL,
			       &copy_types);
      if (err != noErr)
	break;

      if (NILP (Fx_selection_owner_p (Vmac_service_selection)))
	break;
      else
	goto copy_all_flavors;

    case kEventServiceCopy:
      err = GetEventParameter (event, kEventParamScrapRef,
			       typeScrapRef, NULL,
			       sizeof (ScrapRef), NULL, &specific_scrap);
      if (err != noErr
	  || NILP (Fx_selection_owner_p (Vmac_service_selection)))
	{
	  err = eventNotHandledErr;
	  break;
	}

    copy_all_flavors:
      {
	UInt32 count, i;
	ScrapFlavorInfo *flavor_info = NULL;
	ScrapFlavorFlags flags;

	err = GetScrapFlavorCount (cur_scrap, &count);
	if (err == noErr)
	  flavor_info = xmalloc (sizeof (ScrapFlavorInfo) * count);
	err = GetScrapFlavorInfoList (cur_scrap, &count, flavor_info);
	if (err != noErr)
	  {
	    xfree (flavor_info);
	    flavor_info = NULL;
	  }
	if (flavor_info == NULL)
	  break;

	for (i = 0; i < count; i++)
	  {
	    flavor_type = flavor_info[i].flavorType;
	    err = GetScrapFlavorFlags (cur_scrap, flavor_type, &flags);
	    if (err == noErr && !(flags & kScrapFlavorMaskSenderOnly))
	      {
		if (event_kind == kEventServiceCopy)
		  err = copy_scrap_flavor_data (cur_scrap, specific_scrap,
						flavor_type);
		else	     /* event_kind == kEventServiceGetTypes */
		  {
		    type = CreateTypeStringWithOSType (flavor_type);
		    if (type)
		      {
			CFArrayAppendValue (copy_types, type);
			CFRelease (type);
		      }
		  }
	      }
	  }
	xfree (flavor_info);
      }
      break;

    case kEventServicePaste:
    case kEventServicePerform:
      {
	int data_exists_p = 0;

        err = GetEventParameter (event, kEventParamScrapRef, typeScrapRef,
				 NULL, sizeof (ScrapRef), NULL,
				 &specific_scrap);
	if (err == noErr)
	  err = mac_clear_selection (&cur_scrap);
	if (err == noErr)
	  for (rest = Vselection_converter_alist; CONSP (rest);
	       rest = XCDR (rest))
	    {
	      if (! (CONSP (XCAR (rest)) && SYMBOLP (XCAR (XCAR (rest)))))
		continue;
	      flavor_type = get_flavor_type_from_symbol (XCAR (XCAR (rest)),
							 specific_scrap);
	      if (flavor_type == 0)
		continue;
	      err = copy_scrap_flavor_data (specific_scrap, cur_scrap,
					    flavor_type);
	      if (err == noErr)
		data_exists_p = 1;
	    }
	if (!data_exists_p)
	  err = eventNotHandledErr;
	else
	  err = mac_store_service_event (event);
      }
      break;
    }

  if (err != noErr)
    err = eventNotHandledErr;
  return err;
}

static OSStatus
install_service_handler ()
{
  static const EventTypeSpec specs[] =
    {{kEventClassService, kEventServiceGetTypes},
     {kEventClassService, kEventServiceCopy},
     {kEventClassService, kEventServicePaste},
     {kEventClassService, kEventServicePerform}};

  return InstallApplicationEventHandler (NewEventHandlerUPP
					 (mac_handle_service_event),
					 GetEventTypeCount (specs),
					 specs, NULL, NULL);
}
#endif	/* MAC_OSX */


/***********************************************************************
			    Initialization
 ***********************************************************************/

void
mac_toolbox_initialize ()
{
  any_help_event_p = 0;

  init_menu_bar ();

#ifdef MAC_OSX
  init_apple_event_handler ();
#endif
#if USE_MAC_TSM
  init_tsm ();
#endif
}

/* arch-tag: 71a597a8-6e9f-47b0-8b89-5a5ae3e16516
   (do not change this comment) */
