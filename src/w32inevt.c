/* Input event support for Emacs on the Microsoft Windows API.
   Copyright (C) 1992-1993, 1995, 2001-2017 Free Software Foundation,
   Inc.

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
   Drew Bliss                   01-Oct-93
     Adapted from ntkbd.c by Tim Fleehart
*/


#include <config.h>
#include <stdio.h>
#include <windows.h>

#ifndef MOUSE_MOVED
#define MOUSE_MOVED   1
#endif
#ifndef MOUSE_HWHEELED
#define MOUSE_HWHEELED 8
#endif

#include "lisp.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "termchar.h"	/* for Mouse_HLInfo, tty_display_info */
#include "w32term.h"
#include "w32inevt.h"
#include "w32common.h"

/* stdin, from w32console.c */
extern HANDLE keyboard_handle;

/* Info for last mouse motion */
static COORD movement_pos;
static Time movement_time;

/* from w32fns.c */
extern unsigned int map_keypad_keys (unsigned int, unsigned int);
extern unsigned int w32_key_to_modifier (int key);

/* Event queue */
#define EVENT_QUEUE_SIZE 50
static INPUT_RECORD event_queue[EVENT_QUEUE_SIZE];
static INPUT_RECORD *queue_ptr = event_queue, *queue_end = event_queue;

/* Temporarily store lead byte of DBCS input sequences.  */
static char dbcs_lead = 0;

static inline BOOL
w32_read_console_input (HANDLE h, INPUT_RECORD *rec, DWORD recsize,
			DWORD *waiting)
{
  return (w32_console_unicode_input
	  ? ReadConsoleInputW (h, rec, recsize, waiting)
	  : ReadConsoleInputA (h, rec, recsize, waiting));
}

/* Set by w32_console_toggle_lock_key.  */
int faked_key;

static int
fill_queue (BOOL block)
{
  BOOL rc;
  DWORD events_waiting;

  if (queue_ptr < queue_end)
    return queue_end-queue_ptr;

  if (!block)
    {
      /* Check to see if there are some events to read before we try
	 because we can't block.  */
      if (!GetNumberOfConsoleInputEvents (keyboard_handle, &events_waiting))
	return -1;
      if (events_waiting == 0)
	return 0;
    }

  rc = w32_read_console_input (keyboard_handle, event_queue, EVENT_QUEUE_SIZE,
			       &events_waiting);
  if (!rc)
    return -1;
  queue_ptr = event_queue;
  queue_end = event_queue + events_waiting;
  return (int) events_waiting;
}

/* In a generic, multi-frame world this should take a console handle
   and return the frame for it.

   Right now, there's only one frame so return it.  */
static struct frame *
get_frame (void)
{
  return SELECTED_FRAME ();
}

/* Translate console modifiers to emacs modifiers.
   German keyboard support (Kai Morgan Zeise 2/18/95).  */


#if 0
/* Return nonzero if the virtual key is a dead key.  */
static int
is_dead_key (int wparam)
{
  unsigned int code = MapVirtualKey (wparam, 2);

  /* Windows 95 returns 0x8000, NT returns 0x80000000.  */
  return (code & 0x80008000) ? 1 : 0;
}
#endif

/* The return code indicates key code size.  cpID is the codepage to
   use for translation to Unicode; -1 means use the current console
   input codepage.  */


/* return code -1 means that event_queue_ptr won't be incremented.
   In other word, this event makes two key codes.   (by himi)       */
static int
key_event (KEY_EVENT_RECORD *event, struct input_event *emacs_ev, int *isdead)
{
  static int mod_key_state = 0;
  int wParam;

  *isdead = 0;

  /* Skip key-up events.  */
  if (!event->bKeyDown)
    {
      switch (event->wVirtualKeyCode)
	{
	case VK_LWIN:
          if (!w32_kbdhook_active)
            mod_key_state &= ~LEFT_WIN_PRESSED;
	  break;
	case VK_RWIN:
          if (!w32_kbdhook_active)
            mod_key_state &= ~RIGHT_WIN_PRESSED;
	  break;
	case VK_APPS:
	  mod_key_state &= ~APPS_PRESSED;
	  break;
	}
      return 0;
    }

  /* Ignore keystrokes we fake ourself; see below.  */
  if (faked_key == event->wVirtualKeyCode)
    {
      faked_key = 0;
      return 0;
    }

  /* To make it easier to debug this code, ignore modifier keys!  */
  switch (event->wVirtualKeyCode)
    {
    case VK_LWIN:
      if (NILP (Vw32_pass_lwindow_to_system))
	{
	  /* Prevent system from acting on keyup (which opens the Start
	     menu if no other key was pressed) by simulating a press of
	     Space which we will ignore.  */
	  if ((mod_key_state & LEFT_WIN_PRESSED) == 0)
	    {
	      if (NUMBERP (Vw32_phantom_key_code))
		faked_key = XUINT (Vw32_phantom_key_code) & 255;
	      else
		faked_key = VK_SPACE;
	      keybd_event (faked_key, (BYTE) MapVirtualKey (faked_key, 0), 0, 0);
	    }
	}
      if (!w32_kbdhook_active)
        mod_key_state |= LEFT_WIN_PRESSED;
      if (!NILP (Vw32_lwindow_modifier))
	return 0;
      break;
    case VK_RWIN:
      if (NILP (Vw32_pass_rwindow_to_system))
	{
	  if ((mod_key_state & RIGHT_WIN_PRESSED) == 0)
	    {
	      if (NUMBERP (Vw32_phantom_key_code))
		faked_key = XUINT (Vw32_phantom_key_code) & 255;
	      else
		faked_key = VK_SPACE;
	      keybd_event (faked_key, (BYTE) MapVirtualKey (faked_key, 0), 0, 0);
	    }
	}
      if (!w32_kbdhook_active)
        mod_key_state |= RIGHT_WIN_PRESSED;
      if (!NILP (Vw32_rwindow_modifier))
	return 0;
      break;
    case VK_APPS:
      mod_key_state |= APPS_PRESSED;
      if (!NILP (Vw32_apps_modifier))
	return 0;
      break;
    case VK_CAPITAL:
      /* Decide whether to treat as modifier or function key.  */
      if (NILP (Vw32_enable_caps_lock))
	goto disable_lock_key;
      return 0;
    case VK_NUMLOCK:
      /* Decide whether to treat as modifier or function key.  */
      if (NILP (Vw32_enable_num_lock))
	goto disable_lock_key;
      return 0;
    case VK_SCROLL:
      /* Decide whether to treat as modifier or function key.  */
      if (NILP (Vw32_scroll_lock_modifier))
	goto disable_lock_key;
      return 0;
    disable_lock_key:
      /* Ensure the appropriate lock key state is off (and the
	 indicator light as well).  */
      wParam = event->wVirtualKeyCode;
      if (GetAsyncKeyState (wParam) & 0x8000)
	{
	  /* Fake another press of the relevant key.  Apparently, this
	     really is the only way to turn off the indicator.  */
	  faked_key = wParam;
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | 0, 0);
	  keybd_event ((BYTE) wParam, (BYTE) MapVirtualKey (wParam, 0),
		       KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0);
	}
      break;
    case VK_MENU:
    case VK_CONTROL:
    case VK_SHIFT:
      return 0;
    case VK_CANCEL:
      /* Windows maps Ctrl-Pause (aka Ctrl-Break) into VK_CANCEL,
	 which is confusing for purposes of key binding; convert
	 VK_CANCEL events into VK_PAUSE events.  */
      event->wVirtualKeyCode = VK_PAUSE;
      break;
    case VK_PAUSE:
      /* Windows maps Ctrl-NumLock into VK_PAUSE, which is confusing
	 for purposes of key binding; convert these back into
	 VK_NUMLOCK events, at least when we want to see NumLock key
	 presses.  (Note that there is never any possibility that
	 VK_PAUSE with Ctrl really is C-Pause as per above.)  */
      if (NILP (Vw32_enable_num_lock)
	  && (event->dwControlKeyState
	      & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) != 0)
	event->wVirtualKeyCode = VK_NUMLOCK;
      break;
    }

  /* Recognize state of Windows and Apps keys.  */
  event->dwControlKeyState |= mod_key_state;
  if (w32_kbdhook_active)
    {
      if (check_w32_winkey_state (VK_LWIN))
        event->dwControlKeyState |= LEFT_WIN_PRESSED;
      if (check_w32_winkey_state (VK_RWIN))
        event->dwControlKeyState |= RIGHT_WIN_PRESSED;
    }

  /* Distinguish numeric keypad keys from extended keys.  */
  event->wVirtualKeyCode =
    map_keypad_keys (event->wVirtualKeyCode,
		     (event->dwControlKeyState & ENHANCED_KEY));

  if (lispy_function_keys[event->wVirtualKeyCode] == 0)
    {
      if (!NILP (Vw32_recognize_altgr)
	  && (event->dwControlKeyState & LEFT_CTRL_PRESSED)
	  && (event->dwControlKeyState & RIGHT_ALT_PRESSED))
	{
	  /* Don't try to interpret AltGr key chords; ToAscii seems not
	     to process them correctly.  */
	}
      /* Handle key chords including any modifiers other than shift
         directly, in order to preserve as much modifier information as
         possible.  */
      else if (event->dwControlKeyState
	       & (  RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED
		  | RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED
		  | (!NILP (Vw32_lwindow_modifier) ? LEFT_WIN_PRESSED : 0)
		  | (!NILP (Vw32_rwindow_modifier) ? RIGHT_WIN_PRESSED : 0)
		  | (!NILP (Vw32_apps_modifier) ? APPS_PRESSED : 0)
		  | (!NILP (Vw32_scroll_lock_modifier) ? SCROLLLOCK_ON : 0)))
	{
	  /* Don't translate modified alphabetic keystrokes, so the user
	     doesn't need to constantly switch layout to type control or
	     meta keystrokes when the normal layout translates
	     alphabetic characters to non-ascii characters.  */
	  if ('A' <= event->wVirtualKeyCode && event->wVirtualKeyCode <= 'Z')
	    {
	      event->uChar.AsciiChar = event->wVirtualKeyCode;
	      if ((event->dwControlKeyState & SHIFT_PRESSED) == 0)
		event->uChar.AsciiChar += ('a' - 'A');
	    }
	  /* Try to handle unrecognized keystrokes by determining the
             base character (ie. translating the base key plus shift
             modifier).  */
	  else if (event->uChar.AsciiChar == 0)
	    w32_kbd_patch_key (event, -1);
	}

      if (event->uChar.AsciiChar == 0)
	{
	  emacs_ev->kind = NO_EVENT;
	  return 0;
	}
      else if (event->uChar.AsciiChar > 0)
	{
	  /* Pure ASCII characters < 128.  */
	  emacs_ev->kind = ASCII_KEYSTROKE_EVENT;
	  emacs_ev->code = event->uChar.AsciiChar;
	}
      else if (event->uChar.UnicodeChar > 0
	       && w32_console_unicode_input)
	{
	  /* Unicode codepoint; only valid if we are using Unicode
	     console input mode.  */
	  emacs_ev->kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	  emacs_ev->code = event->uChar.UnicodeChar;
	}
      else
	{
	  /* Fallback handling of non-ASCII characters for non-Unicode
	     versions of Windows, and for non-Unicode input on NT
	     family of Windows.  Only characters in the current
	     console codepage are supported by this fallback.  */
	  wchar_t code;
	  char dbcs[2];
          int cpId;

	  /* Get the current console input codepage to interpret this
	     key with.  Note that the system defaults for the OEM
	     codepage could have been changed by calling SetConsoleCP
	     or w32-set-console-codepage, so using GetLocaleInfo to
	     get LOCALE_IDEFAULTCODEPAGE is not TRT here.  */
          cpId = GetConsoleCP ();

	  dbcs[0] = dbcs_lead;
	  dbcs[1] = event->uChar.AsciiChar;
	  if (dbcs_lead)
	    {
	      dbcs_lead = 0;
	      if (!MultiByteToWideChar (cpId, 0, dbcs, 2, &code, 1))
		{
		  /* Garbage  */
		  DebPrint (("Invalid DBCS sequence: %d %d\n",
			     dbcs[0], dbcs[1]));
		  emacs_ev->kind = NO_EVENT;
		}
	    }
	  else if (IsDBCSLeadByteEx (cpId, dbcs[1]))
	    {
	      dbcs_lead = dbcs[1];
	      emacs_ev->kind = NO_EVENT;
	    }
	  else
	    {
	      if (!MultiByteToWideChar (cpId, 0, &dbcs[1], 1, &code, 1))
		{
		  /* Garbage  */
		  DebPrint (("Invalid character: %d\n", dbcs[1]));
		  emacs_ev->kind = NO_EVENT;
		}
	    }
	  emacs_ev->kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	  emacs_ev->code = code;
	}
    }
  else
    {
      /* Function keys and other non-character keys.  */
      emacs_ev->kind = NON_ASCII_KEYSTROKE_EVENT;
      emacs_ev->code = event->wVirtualKeyCode;
    }

  XSETFRAME (emacs_ev->frame_or_window, get_frame ());
  emacs_ev->modifiers = w32_kbd_mods_to_emacs (event->dwControlKeyState,
					       event->wVirtualKeyCode);
  emacs_ev->timestamp = GetTickCount ();
  return 1;
}

/* Mouse position hook.  */
void
w32_console_mouse_position (struct frame **f,
			    int insist,
			    Lisp_Object *bar_window,
			    enum scroll_bar_part *part,
			    Lisp_Object *x,
			    Lisp_Object *y,
			    Time *time)
{
  block_input ();

  insist = insist;

  *f = get_frame ();
  *bar_window = Qnil;
  *part = scroll_bar_above_handle;
  SELECTED_FRAME ()->mouse_moved = 0;

  XSETINT (*x, movement_pos.X);
  XSETINT (*y, movement_pos.Y);
  *time = movement_time;

  unblock_input ();
}

/* Remember mouse motion and notify emacs.  */
static void
mouse_moved_to (int x, int y)
{
  /* If we're in the same place, ignore it.  */
  if (x != movement_pos.X || y != movement_pos.Y)
    {
      SELECTED_FRAME ()->mouse_moved = 1;
      movement_pos.X = x;
      movement_pos.Y = y;
      movement_time = GetTickCount ();
    }
}

/* Consoles return button bits in a strange order:
     least significant - Leftmost button
     next - Rightmost button
     next - Leftmost+1
     next - Leftmost+2...

   For the 3 standard buttons, we have:
     Left == 0
     Middle == 1
     Right == 2
   Others increase from there.  */

#define NUM_TRANSLATED_MOUSE_BUTTONS 5
static int emacs_button_translation[NUM_TRANSLATED_MOUSE_BUTTONS] =
{
  0, 2, 1, 3, 4
};

static int
do_mouse_event (MOUSE_EVENT_RECORD *event,
		struct input_event *emacs_ev)
{
  static DWORD button_state = 0;
  static Lisp_Object last_mouse_window;
  DWORD but_change, mask, flags = event->dwEventFlags;
  int i;

  switch (flags)
    {
    case MOUSE_MOVED:
      {
	struct frame *f = get_frame ();
	Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
	int mx = event->dwMousePosition.X, my = event->dwMousePosition.Y;

	mouse_moved_to (mx, my);

	if (f->mouse_moved)
	  {
	    if (hlinfo->mouse_face_hidden)
	      {
		hlinfo->mouse_face_hidden = 0;
		clear_mouse_face (hlinfo);
	      }

	    /* Generate SELECT_WINDOW_EVENTs when needed.  */
	    if (!NILP (Vmouse_autoselect_window))
	      {
		Lisp_Object mouse_window = window_from_coordinates (f, mx, my,
								    0, 0);
		/* A window will be selected only when it is not
		   selected now, and the last mouse movement event was
		   not in it.  A minibuffer window will be selected iff
		   it is active.  */
		if (WINDOWP (mouse_window)
		    && !EQ (mouse_window, last_mouse_window)
		    && !EQ (mouse_window, selected_window))
		  {
		    struct input_event event;

		    EVENT_INIT (event);
		    event.kind = SELECT_WINDOW_EVENT;
		    event.frame_or_window = mouse_window;
		    event.arg = Qnil;
		    event.timestamp = movement_time;
		    kbd_buffer_store_event (&event);
		  }
		last_mouse_window = mouse_window;
	      }
	    else
	      last_mouse_window = Qnil;

	    previous_help_echo_string = help_echo_string;
	    help_echo_string = help_echo_object = help_echo_window = Qnil;
	    help_echo_pos = -1;
	    note_mouse_highlight (f, mx, my);
	    /* If the contents of the global variable help_echo has
	       changed (inside note_mouse_highlight), generate a HELP_EVENT.  */
	    if (!NILP (help_echo_string) || !NILP (previous_help_echo_string))
	      gen_help_event (help_echo_string, selected_frame,
			      help_echo_window, help_echo_object,
			      help_echo_pos);
	  }
	/* We already called kbd_buffer_store_event, so indicate to
	   the caller it shouldn't.  */
	return 0;
      }
    case MOUSE_WHEELED:
    case MOUSE_HWHEELED:
      {
	struct frame *f = get_frame ();
	int mx = event->dwMousePosition.X, my = event->dwMousePosition.Y;
	bool down_p = (event->dwButtonState & 0x10000000) != 0;

	emacs_ev->kind =
	  flags == MOUSE_HWHEELED ? HORIZ_WHEEL_EVENT : WHEEL_EVENT;
	emacs_ev->code = 0;
	emacs_ev->modifiers = down_p ? down_modifier : up_modifier;
	emacs_ev->modifiers |=
	  w32_kbd_mods_to_emacs (event->dwControlKeyState, 0);
	XSETINT (emacs_ev->x, mx);
	XSETINT (emacs_ev->y, my);
	XSETFRAME (emacs_ev->frame_or_window, f);
	emacs_ev->arg = Qnil;
	emacs_ev->timestamp = GetTickCount ();
	return 1;
      }
    case DOUBLE_CLICK:
    default:	/* mouse pressed or released */
      /* It looks like the console code sends us a button-release
	 mouse event with dwButtonState == 0 when a window is
	 activated and when the mouse is first clicked.  Ignore this
	 case.  */
      if (event->dwButtonState == button_state)
	return 0;

      emacs_ev->kind = MOUSE_CLICK_EVENT;

      /* Find out what button has changed state since the last button
	 event.  */
      but_change = button_state ^ event->dwButtonState;
      mask = 1;
      for (i = 0; mask; i++, mask <<= 1)
	if (but_change & mask)
	  {
	    if (i < NUM_TRANSLATED_MOUSE_BUTTONS)
	      emacs_ev->code = emacs_button_translation[i];
	    else
	      emacs_ev->code = i;
	    break;
	  }

      button_state = event->dwButtonState;
      emacs_ev->modifiers =
	w32_kbd_mods_to_emacs (event->dwControlKeyState, 0)
	| ((event->dwButtonState & mask) ? down_modifier : up_modifier);

      XSETFASTINT (emacs_ev->x, event->dwMousePosition.X);
      XSETFASTINT (emacs_ev->y, event->dwMousePosition.Y);
      XSETFRAME (emacs_ev->frame_or_window, get_frame ());
      emacs_ev->arg = Qnil;
      emacs_ev->timestamp = GetTickCount ();

      return 1;
    }
}

static void
resize_event (WINDOW_BUFFER_SIZE_RECORD *event)
{
  struct frame *f = get_frame ();

  change_frame_size (f, event->dwSize.X, event->dwSize.Y
		     - FRAME_MENU_BAR_LINES (f), 0, 1, 0, 0);
  SET_FRAME_GARBAGED (f);
}

static void
maybe_generate_resize_event (void)
{
  CONSOLE_SCREEN_BUFFER_INFO info;
  struct frame *f = get_frame ();

  GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info);

  /* It is okay to call this unconditionally, since it will do nothing
     if the size hasn't actually changed.  */
  change_frame_size (f,
		     1 + info.srWindow.Right - info.srWindow.Left,
		     1 + info.srWindow.Bottom - info.srWindow.Top
		     - FRAME_MENU_BAR_LINES (f), 0, 1, 0, 0);
}

#if HAVE_W32NOTIFY
int
handle_file_notifications (struct input_event *hold_quit)
{
  struct notifications_set *ns = NULL;
  int nevents = 0;
  int done = 0;

  /* We cannot process notification before Emacs is fully initialized,
     since we need the UTF-16LE coding-system to be set up.  */
  if (!initialized)
    {
      return nevents;
    }

  while (!done)
    {
      ns = NULL;

      /* Find out if there is a record available in the linked list of
	 notifications sets.  If so, unlink te set from the linked list.
	 Use the critical section.  */
      enter_crit ();
      if (notifications_set_head->next != notifications_set_head)
	{
	  ns = notifications_set_head->next;
	  ns->prev->next = ns->next;
	  ns->next->prev = ns->prev;
	}
      else
	done = 1;
      leave_crit();

      if (ns)
	{
	  BYTE *p = ns->notifications;
	  FILE_NOTIFY_INFORMATION *fni = (PFILE_NOTIFY_INFORMATION)p;
	  const DWORD min_size
	    = offsetof (FILE_NOTIFY_INFORMATION, FileName) + sizeof(wchar_t);
	  struct input_event inev;
	  DWORD info_size = ns->size;
	  Lisp_Object cs = Qutf_16le;
	  Lisp_Object obj = w32_get_watch_object (ns->desc);

	  /* notifications size could be zero when the buffer of
	     notifications overflowed on the OS level, or when the
	     directory being watched was itself deleted.  Do nothing in
	     that case.  */
	  if (info_size
	      && !NILP (obj) && CONSP (obj))
	    {
	      Lisp_Object callback = XCDR (obj);

	      EVENT_INIT (inev);

	      while (info_size >= min_size)
		{
		  Lisp_Object utf_16_fn
		    = make_unibyte_string ((char *)fni->FileName,
					   fni->FileNameLength);
		  /* Note: mule-conf is preloaded, so utf-16le must
		     already be defined at this point.  */
		  Lisp_Object fname
		    = code_convert_string_norecord (utf_16_fn, cs, 0);
		  Lisp_Object action = lispy_file_action (fni->Action);

		  inev.kind = FILE_NOTIFY_EVENT;
		  inev.timestamp = GetTickCount ();
		  inev.modifiers = 0;
		  inev.frame_or_window = callback;
		  inev.arg = Fcons (action, fname);
		  inev.arg = list3 (make_pointer_integer (ns->desc),
				    action, fname);
		  kbd_buffer_store_event_hold (&inev, hold_quit);
		  nevents++;
		  if (!fni->NextEntryOffset)
		    break;
		  p += fni->NextEntryOffset;
		  fni = (PFILE_NOTIFY_INFORMATION)p;
		  info_size -= fni->NextEntryOffset;
		}
	    }
	  /* Free this notification set.  */
	  free (ns->notifications);
	  free (ns);
	}
    }
  return nevents;
}
#else  /* !HAVE_W32NOTIFY */
int
handle_file_notifications (struct input_event *hold_quit)
{
  return 0;
}
#endif	/* !HAVE_W32NOTIFY */

/* Here's an overview of how Emacs input works in non-GUI sessions on
   MS-Windows.  (For description of the GUI input, see the commentary
   before w32_msg_pump in w32fns.c.)

   When Emacs is idle, it loops inside wait_reading_process_output,
   calling pselect periodically to check whether any input is
   available.  On Windows, pselect is redirected to sys_select, which
   uses MsgWaitForMultipleObjects to wait for input, either from the
   keyboard or from any of the Emacs subprocesses.  In addition,
   MsgWaitForMultipleObjects wakes up when some Windows message is
   posted to the input queue of the Emacs's main thread (which is the
   thread in which sys_select runs).

   When the Emacs's console window has focus, Windows sends input
   events that originate from the keyboard or the mouse; these events
   wake up MsgWaitForMultipleObjects, which reports that input is
   available.  Emacs then calls w32_console_read_socket, below, to
   read the input.  w32_console_read_socket uses
   GetNumberOfConsoleInputEvents and ReadConsoleInput to peek at and
   read the console input events.

   One type of non-keyboard input event that gets reported as input
   available is due to the Emacs's console window receiving focus.
   When that happens, Emacs gets the FOCUS_EVENT event and sys_select
   reports some input; however, w32_console_read_socket ignores such
   events when called to read them.

   Note that any other Windows message sent to the main thread will
   also wake up MsgWaitForMultipleObjects.  These messages get
   immediately dispatched to their destinations by calling
   drain_message_queue.  */

int
w32_console_read_socket (struct terminal *terminal,
                         struct input_event *hold_quit)
{
  int nev, add;
  int isdead;

  block_input ();

  for (;;)
    {
      int nfnotify = handle_file_notifications (hold_quit);

      nev = fill_queue (0);
      if (nev <= 0)
        {
	  /* If nev == -1, there was some kind of error
	     If nev == 0 then no events were available
	     so return.  */
	  if (nfnotify)
	    nev = 0;
	  break;
        }

      while (nev > 0)
        {
	  struct input_event inev;
	  /* Having a separate variable with this value makes
	     debugging easier, as otherwise the compiler might
	     rearrange the switch below in a way that makes it hard to
	     track the event type.  */
	  unsigned evtype = queue_ptr->EventType;

	  EVENT_INIT (inev);
	  inev.kind = NO_EVENT;
	  inev.arg = Qnil;

	  switch (evtype)
            {
            case KEY_EVENT:
	      add = key_event (&queue_ptr->Event.KeyEvent, &inev, &isdead);
	      if (add == -1) /* 95.7.25 by himi */
		{
		  queue_ptr--;
		  add = 1;
		}
	      if (add)
		kbd_buffer_store_event_hold (&inev, hold_quit);
	      break;

            case MOUSE_EVENT:
	      add = do_mouse_event (&queue_ptr->Event.MouseEvent, &inev);
	      if (add)
		kbd_buffer_store_event_hold (&inev, hold_quit);
	      break;

            case WINDOW_BUFFER_SIZE_EVENT:
	      if (w32_use_full_screen_buffer)
		resize_event (&queue_ptr->Event.WindowBufferSizeEvent);
	      break;

            case MENU_EVENT:
            case FOCUS_EVENT:
	      /* Internal event types, ignored. */
	      break;
            }

	  queue_ptr++;
	  nev--;
        }
    }

  /* We don't get told about changes in the window size (only the buffer
     size, which we no longer care about), so we have to check it
     periodically.  */
  if (!w32_use_full_screen_buffer)
    maybe_generate_resize_event ();

  unblock_input ();
  return nev;
}
