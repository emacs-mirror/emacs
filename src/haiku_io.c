/* Haiku window system support.
   Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

#include <signal.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

#include <OS.h>

#include "haiku_support.h"
#include "lisp.h"
#include "haikuterm.h"
#include "blockinput.h"

#define PORT_CAP 1200

/* The port used to send messages from the application thread to
   Emacs.  */
port_id port_application_to_emacs;

/* The port used to send popup menu messages from the application
   thread to Emacs.  */
port_id port_popup_menu_to_emacs;

/* The port used to send replies to the application after a session
   management event.  */
port_id port_emacs_to_session_manager;

void
haiku_io_init (void)
{
  port_application_to_emacs = create_port (PORT_CAP, "application emacs port");
  port_emacs_to_session_manager = create_port (1, "session manager port");
}

static ssize_t
haiku_len (enum haiku_event_type type)
{
  switch (type)
    {
    case QUIT_REQUESTED:
      return sizeof (struct haiku_quit_requested_event);
    case FRAME_RESIZED:
      return sizeof (struct haiku_resize_event);
    case FRAME_EXPOSED:
      return sizeof (struct haiku_expose_event);
    case KEY_DOWN:
    case KEY_UP:
      return sizeof (struct haiku_key_event);
    case ACTIVATION:
      return sizeof (struct haiku_activation_event);
    case MOUSE_MOTION:
      return sizeof (struct haiku_mouse_motion_event);
    case BUTTON_DOWN:
    case BUTTON_UP:
      return sizeof (struct haiku_button_event);
    case ICONIFICATION:
      return sizeof (struct haiku_iconification_event);
    case MOVE_EVENT:
      return sizeof (struct haiku_move_event);
    case SCROLL_BAR_VALUE_EVENT:
      return sizeof (struct haiku_scroll_bar_value_event);
    case SCROLL_BAR_DRAG_EVENT:
      return sizeof (struct haiku_scroll_bar_drag_event);
    case WHEEL_MOVE_EVENT:
      return sizeof (struct haiku_wheel_move_event);
    case MENU_BAR_RESIZE:
      return sizeof (struct haiku_menu_bar_resize_event);
    case MENU_BAR_CLICK:
      return sizeof (struct haiku_menu_bar_click_event);
    case MENU_BAR_OPEN:
    case MENU_BAR_CLOSE:
      return sizeof (struct haiku_menu_bar_state_event);
    case MENU_BAR_SELECT_EVENT:
      return sizeof (struct haiku_menu_bar_select_event);
    case MENU_BAR_HELP_EVENT:
      return sizeof (struct haiku_menu_bar_help_event);
    case ZOOM_EVENT:
      return sizeof (struct haiku_zoom_event);
    case DRAG_AND_DROP_EVENT:
      return sizeof (struct haiku_drag_and_drop_event);
    case APP_QUIT_REQUESTED_EVENT:
      return sizeof (struct haiku_app_quit_requested_event);
    case DUMMY_EVENT:
      return sizeof (struct haiku_dummy_event);
    case MENU_BAR_LEFT:
      return sizeof (struct haiku_menu_bar_left_event);
    case SCROLL_BAR_PART_EVENT:
      return sizeof (struct haiku_scroll_bar_part_event);
    case SCREEN_CHANGED_EVENT:
      return sizeof (struct haiku_screen_changed_event);
    case CLIPBOARD_CHANGED_EVENT:
      return sizeof (struct haiku_clipboard_changed_event);
    case FONT_CHANGE_EVENT:
      return sizeof (struct haiku_font_change_event);
    case NOTIFICATION_CLICK_EVENT:
      return sizeof (struct haiku_notification_click_event);
    }

  emacs_abort ();
}

/* Read the size of the next message into len, returning -1 if the
   query fails or there is no next message.  */
void
haiku_read_size (ssize_t *len, bool popup_menu_p)
{
  port_id from = (popup_menu_p
		  ? port_popup_menu_to_emacs
		  : port_application_to_emacs);
  ssize_t size;

  size = port_buffer_size_etc (from, B_TIMEOUT, 0);

  if (size < B_OK)
    *len = -1;
  else
    *len = size;
}

/* Read the next message into BUF, putting its type into TYPE,
   assuming the message is at most LEN long.  Return 0 if successful
   and -1 if the read fails.  */
int
haiku_read (enum haiku_event_type *type, void *buf, ssize_t len)
{
  int32 typ;
  port_id from = port_application_to_emacs;

  if (read_port (from, &typ, buf, len) < B_OK)
    return -1;

  *type = (enum haiku_event_type) typ;
  eassert (len >= haiku_len (typ));
  return 0;
}

/* The same as haiku_read, but time out after TIMEOUT microseconds.
   POPUP_MENU_P means to read from the popup menu port instead.
   Input is blocked when an attempt to read is in progress.  */
int
haiku_read_with_timeout (enum haiku_event_type *type, void *buf, ssize_t len,
			 bigtime_t timeout, bool popup_menu_p)
{
  int32 typ;
  port_id from = (popup_menu_p
		  ? port_popup_menu_to_emacs
		  : port_application_to_emacs);

  block_input ();
  if (read_port_etc (from, &typ, buf, len,
		     B_TIMEOUT, (bigtime_t) timeout) < B_OK)
    {
      unblock_input ();
      return -1;
    }
  unblock_input ();
  *type = (enum haiku_event_type) typ;
  eassert (len >= haiku_len (typ));
  return 0;
}

/* Write a message with type TYPE into BUF.  */
int
haiku_write (enum haiku_event_type type, void *buf)
{
  port_id to = port_application_to_emacs;

  if (write_port (to, (int32_t) type, buf, haiku_len (type)) < B_OK)
    return -1;

  kill (getpid (), SIGPOLL);

  return 0;
}

int
haiku_write_without_signal (enum haiku_event_type type, void *buf,
			    bool popup_menu_p)
{
  port_id to = (popup_menu_p
		? port_popup_menu_to_emacs
		: port_application_to_emacs);

  if (write_port (to, (int32_t) type, buf, haiku_len (type)) < B_OK)
    return -1;

  return 0;
}

void
haiku_io_init_in_app_thread (void)
{
  sigset_t set;
  sigfillset (&set);

  if (pthread_sigmask (SIG_BLOCK, &set, NULL))
    perror ("pthread_sigmask");
}
