/* Input event support for Windows NT port of GNU Emacs.
   Copyright (C) 1992, 1993, 1995 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Drew Bliss                   01-Oct-93
     Adapted from ntkbd.c by Tim Fleehart
*/


#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <windows.h>

#include "lisp.h"
#include "frame.h"
#include "blockinput.h"
#include "termhooks.h"

/* stdin, from ntterm */
extern HANDLE keyboard_handle;

/* Indicate mouse motion, from keyboard.c */
extern int mouse_moved;

/* Info for last mouse motion */
static COORD movement_pos;
static DWORD movement_time;

/* from keyboard.c */
extern void reinvoke_input_signal (void);

/* from dispnew.c */
extern int change_frame_size (FRAME_PTR, int, int, int, int);

/* Event queue */
#define EVENT_QUEUE_SIZE 50
static INPUT_RECORD event_queue[EVENT_QUEUE_SIZE];
static INPUT_RECORD *queue_ptr = event_queue, *queue_end = event_queue;

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
  
  rc = ReadConsoleInput (keyboard_handle, event_queue, EVENT_QUEUE_SIZE,
			 &events_waiting);
  if (!rc)
    return -1;
  queue_ptr = event_queue;
  queue_end = event_queue + events_waiting;
  return (int) events_waiting;
}

/* In a generic, multi-frame world this should take a console handle
   and return the frame for it

   Right now, there's only one frame so return it.  */
static FRAME_PTR 
get_frame (void)
{
  return selected_frame;
}

/* Translate console modifiers to emacs modifiers.  
   German keyboard support (Kai Morgan Zeise 2/18/95).  */
static int 
win32_kbd_mods_to_emacs (DWORD mods)
{
  int retval = 0;

  /* If AltGr has been pressed, remove it.  */
  if ((mods & (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED)) 
      == (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED))
    mods &= ~ (RIGHT_ALT_PRESSED | LEFT_CTRL_PRESSED);

  if (mods & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED))
    retval = meta_modifier;
  
  if (mods & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
    {
      retval |= ctrl_modifier;
      if ((mods & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED)) 
	  == (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
	retval |= meta_modifier;
    }

  if (((mods & (SHIFT_PRESSED | CAPSLOCK_ON)) == SHIFT_PRESSED)
      || ((mods & (SHIFT_PRESSED | CAPSLOCK_ON)) == CAPSLOCK_ON))
    retval |= shift_modifier;

  return retval;
}

/* Patch up NT keyboard events when info is missing that should be there,
   assuming that map_virt_key says that the key is a valid ASCII char. */
static char win32_number_shift_map[] = {
  ')', '!', '@', '#', '$', '%', '^', '&', '*', '('
};

#define WIN32_KEY_SHIFTED(mods, no, yes) \
  ((mods & (SHIFT_PRESSED | CAPSLOCK_ON)) ? yes : no)

static void
win32_kbd_patch_key (KEY_EVENT_RECORD *event)
{
  unsigned int key_code = event->wVirtualKeyCode;
  unsigned int mods = event->dwControlKeyState;
  int mapped_punct = 0;

  /* map_virt_key says its a valid key, but the uChar.AsciiChar field
     is empty.  patch up the uChar.AsciiChar field using wVirtualKeyCode.  */
  if (event->uChar.AsciiChar == 0
      && ((key_code >= '0' && key_code <= '9')
	  || (key_code >= 'A' && key_code <= 'Z')
	  || (key_code >= 0xBA && key_code <= 0xC0)
	  || (key_code >= 0xDB && key_code <= 0xDE)
	  )) {
    if (key_code >= '0' && key_code <= '9') {
      event->uChar.AsciiChar = 
	WIN32_KEY_SHIFTED (mods, key_code,
			win32_number_shift_map[key_code - '0']);
      return;
    }
    switch (key_code) {
    case 0xBA: mapped_punct = WIN32_KEY_SHIFTED (mods, ';', ':'); break;
    case 0xBB: mapped_punct = WIN32_KEY_SHIFTED (mods, '=', '+'); break;
    case 0xBC: mapped_punct = WIN32_KEY_SHIFTED (mods, ',', '<'); break;
    case 0xBD: mapped_punct = WIN32_KEY_SHIFTED (mods, '-', '_'); break;
    case 0xBE: mapped_punct = WIN32_KEY_SHIFTED (mods, '.', '>'); break;
    case 0xBF: mapped_punct = WIN32_KEY_SHIFTED (mods, '/', '?'); break;
    case 0xC0: mapped_punct = WIN32_KEY_SHIFTED (mods, '`', '~'); break;
    case 0xDB: mapped_punct = WIN32_KEY_SHIFTED (mods, '[', '{'); break;
    case 0xDC: mapped_punct = WIN32_KEY_SHIFTED (mods, '\\', '|'); break;
    case 0xDD: mapped_punct = WIN32_KEY_SHIFTED (mods, ']', '}'); break;
    case 0xDE: mapped_punct = WIN32_KEY_SHIFTED (mods, '\'', '"'); break;
    default:
      mapped_punct = 0;
      break;
    }
    if (mapped_punct) {
      event->uChar.AsciiChar = mapped_punct;
      return;
    }
    /* otherwise, it's a letter.  */
    event->uChar.AsciiChar = WIN32_KEY_SHIFTED (mods, key_code - 'A' + 'a',
						key_code);
  }
}

/* Map virtual key codes into:
   -1 - Ignore this key
   -2 - ASCII char
   Other - Map non-ASCII keys into X keysyms so that they are looked up
   correctly in keyboard.c

   Return, escape and tab are mapped to ASCII rather than coming back
   as non-ASCII to be more compatible with old-style keyboard support.  */

static int map_virt_key[256] =
{
  -1,
  -1,                 /* VK_LBUTTON */
  -1,                 /* VK_RBUTTON */
  0x69,               /* VK_CANCEL */
  -1,                 /* VK_MBUTTON */
  -1, -1, -1,
  8,                  /* VK_BACK */
  -2,                 /* VK_TAB */
  -1, -1,
  11,                 /* VK_CLEAR */
  -2,                 /* VK_RETURN */
  -1, -1,
  -1,                 /* VK_SHIFT */
  -1,                 /* VK_CONTROL */
  -1,                 /* VK_MENU */
  0x13,               /* VK_PAUSE */
  -1,                 /* VK_CAPITAL */
  -1, -1, -1, -1, -1, -1,
  -2,                 /* VK_ESCAPE */
  -1, -1, -1, -1,
  -2,                 /* VK_SPACE */
  0x55,               /* VK_PRIOR */
  0x56,               /* VK_NEXT */
  0x57,               /* VK_END */
  0x50,               /* VK_HOME */
  0x51,               /* VK_LEFT */
  0x52,               /* VK_UP */
  0x53,               /* VK_RIGHT */
  0x54,               /* VK_DOWN */
  0x60,               /* VK_SELECT */
  0x61,               /* VK_PRINT */
  0x62,               /* VK_EXECUTE */
  -1,                 /* VK_SNAPSHOT */
  0x63,               /* VK_INSERT */
  0xff,               /* VK_DELETE */
  0x6a,               /* VK_HELP */
  -2, -2, -2, -2, -2, -2, -2, -2, -2, -2,     /* 0 - 9 */
  -1, -1, -1, -1, -1, -1, -1,
  -2, -2, -2, -2, -2, -2, -2, -2,             /* A - Z */
  -2, -2, -2, -2, -2, -2, -2, -2,
  -2, -2, -2, -2, -2, -2, -2, -2,
  -2, -2,
  -1, -1, -1, -1, -1,
  0xb0,               /* VK_NUMPAD0 */
  0xb1,               /* VK_NUMPAD1 */
  0xb2,               /* VK_NUMPAD2 */
  0xb3,               /* VK_NUMPAD3 */
  0xb4,               /* VK_NUMPAD4 */
  0xb5,               /* VK_NUMPAD5 */
  0xb6,               /* VK_NUMPAD6 */
  0xb7,               /* VK_NUMPAD7 */
  0xb8,               /* VK_NUMPAD8 */
  0xb9,               /* VK_NUMPAD9 */
  0xaa,               /* VK_MULTIPLY */
  0xab,               /* VK_ADD */
  0xac,               /* VK_SEPARATOR */
  0xad,               /* VK_SUBTRACT */
  0xae,               /* VK_DECIMAL */
  0xaf,               /* VK_DIVIDE */
  0xbe,               /* VK_F1 */
  0xbf,               /* VK_F2 */
  0xc0,               /* VK_F3 */
  0xc1,               /* VK_F4 */
  0xc2,               /* VK_F5 */
  0xc3,               /* VK_F6 */
  0xc4,               /* VK_F7 */
  0xc5,               /* VK_F8 */
  0xc6,               /* VK_F9 */
  0xc7,               /* VK_F10 */
  0xc8,               /* VK_F11 */
  0xc9,               /* VK_F12 */
  0xca,               /* VK_F13 */
  0xcb,               /* VK_F14 */
  0xcc,               /* VK_F15 */
  0xcd,               /* VK_F16 */
  0xce,               /* VK_F17 */
  0xcf,               /* VK_F18 */
  0xd0,               /* VK_F19 */
  0xd1,               /* VK_F20 */
  0xd2,               /* VK_F21 */
  0xd3,               /* VK_F22 */
  0xd4,               /* VK_F23 */
  0xd5,               /* VK_F24 */
  -1, -1, -1, -1, -1, -1, -1, -1,
  0x7f,               /* VK_NUMLOCK */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 0x9f */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 0xaf */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 0xb9 */
  -2,                 /* ; */
  -2,                 /* = */
  -2,                 /* , */
  -2,                 /* \ */
  -2,                 /* . */
  -2,                 /* / */
  -2,                 /* ` */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 0xcf */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 0xda */
  -2,                 /* [ */
  -2,                 /* - */
  -2,                 /* ] */
  -2,                 /* ' */
  -1, /* 0xdf */
  -1, -1, -2,         /* VK_OEM_102 */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 0xef */
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 /* 0xff */
};

static int 
key_event (KEY_EVENT_RECORD *event, struct input_event *emacs_ev)
{
  int map;
  static BOOL map_virt_key_init_done;
  
  /* Skip key-up events.  */
  if (event->bKeyDown == FALSE)
    return 0;
  
  if (event->wVirtualKeyCode > 0xff)
    {
      printf ("Unknown key code %d\n", event->wVirtualKeyCode);
      return 0;
    }

  /* Patch needed for German keyboard. Ulrich Leodolter (1/11/95).  */
  if (! map_virt_key_init_done) 
    {
      short vk;

      if ((vk = VkKeyScan (0x3c)) >= 0 && vk < 256) map_virt_key[vk] = -2; /* less */
      if ((vk = VkKeyScan (0x3e)) >= 0 && vk < 256) map_virt_key[vk] = -2; /* greater */

      map_virt_key_init_done = TRUE;
    }
  
  /* BUGBUG - Ignores the repeat count
     It's questionable whether we want to obey the repeat count anyway
     since keys usually aren't repeated unless key events back up in
     the queue.  If they're backing up then we don't generally want
     to honor them later since that leads to significant slop in
     cursor motion when the system is under heavy load.  */
  
  map = map_virt_key[event->wVirtualKeyCode];
  if (map == -1)
    {
      return 0;
    }
  else if (map == -2)
    {
      /* ASCII */
      emacs_ev->kind = ascii_keystroke;
      win32_kbd_patch_key (event);
      XSETINT (emacs_ev->code, event->uChar.AsciiChar);
    }
  else
    {
      /* non-ASCII */
      emacs_ev->kind = non_ascii_keystroke;
      /*
       * make_lispy_event () now requires non-ascii codes to have
       * the full X keysym values (2nd byte is 0xff).  add it on.
       */
      map |= 0xff00;
      XSETINT (emacs_ev->code, map);
    }
  XSETFRAME (emacs_ev->frame_or_window, get_frame ());
  emacs_ev->modifiers = win32_kbd_mods_to_emacs (event->dwControlKeyState);
  emacs_ev->timestamp = GetTickCount ();
  return 1;
}

/* Mouse position hook.  */
void 
win32_mouse_position (FRAME_PTR *f,
		      int insist,
		      Lisp_Object *bar_window,
		      enum scroll_bar_part *part,
		      Lisp_Object *x,
		      Lisp_Object *y,
		      unsigned long *time)
{
  BLOCK_INPUT;
  
  insist = insist;

  *f = get_frame ();
  *bar_window = Qnil;
  *part = 0;
  mouse_moved = 0;
  
  *x = movement_pos.X;
  *y = movement_pos.Y;
  *time = movement_time;
  
  UNBLOCK_INPUT;
}

/* Remember mouse motion and notify emacs.  */
static void 
mouse_moved_to (int x, int y)
{
  /* If we're in the same place, ignore it */
  if (x != movement_pos.X || y != movement_pos.Y)
    {
      mouse_moved = 1;
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

   Assume emacs likes three button mice, so
     Left == 0
     Middle == 1
     Right == 2
   Others increase from there.  */

static int emacs_button_translation[NUM_MOUSE_BUTTONS] =
{
  0, 2, 1, 3, 4,
};

static int 
do_mouse_event (MOUSE_EVENT_RECORD *event,
		struct input_event *emacs_ev)
{
  static DWORD button_state = 0;
  DWORD but_change, mask;
  int i;
  
  if (event->dwEventFlags == MOUSE_MOVED)
    {
      /* For movement events we just note that the mouse has moved
	 so that emacs will generate drag events.  */
      mouse_moved_to (event->dwMousePosition.X, event->dwMousePosition.Y);
      return 0;
    }
  
  /* It looks like the console code sends us a mouse event with
     dwButtonState == 0 when a window is activated.  Ignore this case.  */
  if (event->dwButtonState == button_state)
    return 0;
  
  emacs_ev->kind = mouse_click;
  
  /* Find out what button has changed state since the last button event.  */
  but_change = button_state ^ event->dwButtonState;
  mask = 1;
  for (i = 0; i < NUM_MOUSE_BUTTONS; i++, mask <<= 1)
    if (but_change & mask)
      {
	XSETINT (emacs_ev->code, emacs_button_translation[i]);
	break;
      }

  /* If the changed button is out of emacs' range (highly unlikely)
     ignore this event.  */
  if (i == NUM_MOUSE_BUTTONS)
    return 0;
  
  button_state = event->dwButtonState;
  emacs_ev->timestamp = GetTickCount ();
  emacs_ev->modifiers = win32_kbd_mods_to_emacs (event->dwControlKeyState) |
    ((event->dwButtonState & mask) ? down_modifier : up_modifier);
  
  XSETFASTINT (emacs_ev->x, event->dwMousePosition.X);
  XSETFASTINT (emacs_ev->y, event->dwMousePosition.Y);
  XSETFRAME (emacs_ev->frame_or_window, get_frame ());
  
  return 1;
}

static void 
resize_event (WINDOW_BUFFER_SIZE_RECORD *event)
{
  FRAME_PTR f = get_frame ();
  
  change_frame_size (f, event->dwSize.Y, event->dwSize.X, 0, 1);
  SET_FRAME_GARBAGED (f);
}

int 
win32_read_socket (int sd, struct input_event *bufp, int numchars,
		   int waitp, int expected)
{
  BOOL no_events = TRUE;
  int nev, ret = 0, add;
  
  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }
  
  interrupt_input_pending = 0;
  BLOCK_INPUT;
  
  for (;;)
    {
      nev = fill_queue (waitp != 0);
      if (nev <= 0)
        {
	  /* If nev == -1, there was some kind of error
	     If nev == 0 then waitp must be zero and no events were available
	     so return.  */
	  UNBLOCK_INPUT;
	  return nev;
        }

      while (nev > 0 && numchars > 0)
        {
	  switch (queue_ptr->EventType)
            {
            case KEY_EVENT:
	      add = key_event (&queue_ptr->Event.KeyEvent, bufp);
	      bufp += add;
	      ret += add;
	      numchars -= add;
	      break;

            case MOUSE_EVENT:
	      add = do_mouse_event (&queue_ptr->Event.MouseEvent, bufp);
	      bufp += add;
	      ret += add;
	      numchars -= add;
	      break;

            case WINDOW_BUFFER_SIZE_EVENT:
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

      if (ret > 0 || expected == 0)
	break;
    }
  
  UNBLOCK_INPUT;
  return ret;
}
