/* Haiku window system support.  Hey, Emacs, this is -*- C++ -*-
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
#include <attribute.h>

#include <app/Application.h>
#include <app/Cursor.h>
#include <app/Clipboard.h>
#include <app/Messenger.h>
#include <app/Roster.h>

#include <interface/GraphicsDefs.h>
#include <interface/InterfaceDefs.h>
#include <interface/Bitmap.h>
#include <interface/Window.h>
#include <interface/View.h>
#include <interface/Screen.h>
#include <interface/ScrollBar.h>
#include <interface/Region.h>
#include <interface/Menu.h>
#include <interface/MenuItem.h>
#include <interface/PopUpMenu.h>
#include <interface/MenuBar.h>
#include <interface/Alert.h>
#include <interface/Button.h>
#include <interface/ControlLook.h>
#include <interface/Deskbar.h>
#include <interface/ListView.h>
#include <interface/StringItem.h>
#include <interface/SplitView.h>
#include <interface/ScrollView.h>
#include <interface/StringView.h>
#include <interface/TextControl.h>
#include <interface/CheckBox.h>

#include <locale/UnicodeChar.h>

#include <game/WindowScreen.h>
#include <game/DirectWindow.h>

#include <storage/FindDirectory.h>
#include <storage/Entry.h>
#include <storage/Path.h>
#include <storage/FilePanel.h>
#include <storage/AppFileInfo.h>
#include <storage/Path.h>
#include <storage/PathFinder.h>
#include <storage/Node.h>

#include <support/Beep.h>
#include <support/DataIO.h>
#include <support/Locker.h>
#include <support/ObjectList.h>

#include <translation/TranslatorRoster.h>
#include <translation/TranslationDefs.h>
#include <translation/TranslationUtils.h>

#include <kernel/OS.h>
#include <kernel/fs_attr.h>
#include <kernel/scheduler.h>

#include <private/interface/ToolTip.h>
#include <private/interface/WindowPrivate.h>

#include <cmath>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <csignal>
#include <cfloat>

#ifdef USE_BE_CAIRO
#include <cairo.h>
#endif

#include "haiku_support.h"

/* Some messages that Emacs sends to itself.  */
enum
  {
    SCROLL_BAR_UPDATE	     = 3000,
    WAIT_FOR_RELEASE	     = 3001,
    RELEASE_NOW		     = 3002,
    CANCEL_DROP		     = 3003,
    SHOW_MENU_BAR	     = 3004,
    BE_MENU_BAR_OPEN	     = 3005,
    QUIT_APPLICATION	     = 3006,
    REPLAY_MENU_BAR	     = 3007,
    FONT_FAMILY_SELECTED     = 3008,
    FONT_STYLE_SELECTED	     = 3009,
    FILE_PANEL_SELECTION     = 3010,
    QUIT_PREVIEW_DIALOG	     = 3011,
    SET_FONT_INDICES	     = 3012,
    SET_PREVIEW_DIALOG	     = 3013,
    UPDATE_PREVIEW_DIALOG    = 3014,
    SEND_MOVE_FRAME_EVENT    = 3015,
    SET_DISABLE_ANTIALIASING = 3016,
  };

/* X11 keysyms that we use.  */
enum
  {
    KEY_BACKSPACE	  = 0xff08,
    KEY_TAB		  = 0xff09,
    KEY_RETURN		  = 0xff0d,
    KEY_PAUSE		  = 0xff13,
    KEY_ESCAPE		  = 0xff1b,
    KEY_DELETE		  = 0xffff,
    KEY_HOME		  = 0xff50,
    KEY_LEFT_ARROW	  = 0xff51,
    KEY_UP_ARROW	  = 0xff52,
    KEY_RIGHT_ARROW	  = 0xff53,
    KEY_DOWN_ARROW	  = 0xff54,
    KEY_PAGE_UP		  = 0xff55,
    KEY_PAGE_DOWN	  = 0xff56,
    KEY_END		  = 0xff57,
    KEY_PRINT		  = 0xff61,
    KEY_INSERT		  = 0xff63,
    /* This is used to indicate the first function key.  */
    KEY_F1		  = 0xffbe,
    /* These are found on some multilingual keyboards.  */
    KEY_HANGUL		  = 0xff31,
    KEY_HANGUL_HANJA	  = 0xff34,
    KEY_HIRIGANA_KATAGANA = 0xff27,
    KEY_ZENKAKU_HANKAKU	  = 0xff2a,
  };

struct font_selection_dialog_message
{
  /* Whether or not font selection was canceled.  */
  bool_bf cancel : 1;

  /* Whether or not a size was explicitly specified.  */
  bool_bf size_specified : 1;

  /* Whether or not antialiasing should be disabled.  */
  bool_bf disable_antialias : 1;

  /* The index of the selected font family.  */
  int family_idx;

  /* The index of the selected font style.  */
  int style_idx;

  /* The selected font size.  */
  int size;
};

/* The color space of the main screen.  B_NO_COLOR_SPACE means it has
   not yet been computed.  */
static color_space dpy_color_space = B_NO_COLOR_SPACE;

/* The keymap, or NULL if it has not been initialized.  */
static key_map *key_map;

/* Indices of characters into the keymap.  */
static char *key_chars;

/* Lock around keymap data, since it's touched from different
   threads.  */
static BLocker key_map_lock;

/* The locking semantics of BWindows running in multiple threads are
   so complex that child frame state (which is the only state that is
   shared between different BWindows at runtime) does best with a
   single global lock.  */
static BLocker child_frame_lock;

/* Variable where the popup menu thread returns the chosen menu
   item.  */
static BMessage volatile *popup_track_message;

/* Variable in which alert dialog threads return the selected button
   number.  */
static int32 volatile alert_popup_value;

/* The view that has the passive grab.  */
static void *grab_view;

/* The locker for that variable.  */
static BLocker grab_view_locker;

/* Whether or not a drag-and-drop operation is in progress.  */
static bool drag_and_drop_in_progress;

/* Many places require us to lock the child frame data, and then lock
   the locker of some random window.  Unfortunately, locking such a
   window might be delayed due to an arriving message, which then
   calls a callback inside that window that tries to lock the child
   frame data but doesn't finish since the child frame lock is already
   held, not letting the code that held the child frame lock proceed,
   thereby causing a deadlock.

   Rectifying that problem is simple: all code in a looper callback
   must lock the child frame data with this macro instead.

   IOW, if some other code is already running with the child frame
   lock held, don't interfere: wait until it's finished before
   continuing.  */
#define CHILD_FRAME_LOCK_INSIDE_LOOPER_CALLBACK		\
  if (child_frame_lock.LockWithTimeout (200) != B_OK)	\
    {							\
      /* The Haiku equivalent of XPutBackEvent.  */	\
      if (CurrentMessage ())				\
	PostMessage (CurrentMessage ());		\
    }							\
  else

/* This could be a private API, but it's used by (at least) the Qt
   port, so it's probably here to stay.  */
extern status_t get_subpixel_antialiasing (bool *);

/* The ID of the thread the BApplication is running in.  */
static thread_id app_thread;

_Noreturn void
gui_abort (const char *msg)
{
  fprintf (stderr, "Abort in GUI code: %s\n", msg);
  fprintf (stderr, "Under Haiku, Emacs cannot recover from errors in GUI code\n");
  fprintf (stderr, "App Server disconnects usually manifest as bitmap "
	   "initialization failures or lock failures.");
  abort ();
}

struct be_popup_menu_data
{
  int x, y;
  BPopUpMenu *menu;
};

static int32
be_popup_menu_thread_entry (void *thread_data)
{
  struct be_popup_menu_data *data;
  struct haiku_dummy_event dummy;
  BMenuItem *it;

  data = (struct be_popup_menu_data *) thread_data;

  it = data->menu->Go (BPoint (data->x, data->y));

  if (it)
    popup_track_message = it->Message ();
  else
    popup_track_message = NULL;

  haiku_write (DUMMY_EVENT, &dummy);
  return 0;
}

/* Convert a raw character RAW produced by the keycode KEY into a key
   symbol and place it in KEYSYM.

   If RAW cannot be converted into a keysym, value is 0.  If RAW can
   be converted into a keysym, but it should be ignored, value is -1.

   Any other value means success, and that the keysym should be used
   instead of mapping the keycode into a character.  */

static int
keysym_from_raw_char (int32 raw, int32 key, unsigned *code)
{
  switch (raw)
    {
    case B_BACKSPACE:
      *code = KEY_BACKSPACE;
      break;
    case B_RETURN:
      *code = KEY_RETURN;
      break;
    case B_TAB:
      *code = KEY_TAB;
      break;
    case B_ESCAPE:
      *code = KEY_ESCAPE;
      break;
    case B_LEFT_ARROW:
      *code = KEY_LEFT_ARROW;
      break;
    case B_RIGHT_ARROW:
      *code = KEY_RIGHT_ARROW;
      break;
    case B_UP_ARROW:
      *code = KEY_UP_ARROW;
      break;
    case B_DOWN_ARROW:
      *code = KEY_DOWN_ARROW;
      break;
    case B_INSERT:
      *code = KEY_INSERT;
      break;
    case B_DELETE:
      *code = KEY_DELETE;
      break;
    case B_HOME:
      *code = KEY_HOME;
      break;
    case B_END:
      *code = KEY_END;
      break;
    case B_PAGE_UP:
      *code = KEY_PAGE_UP;
      break;
    case B_PAGE_DOWN:
      *code = KEY_PAGE_DOWN;
      break;

    case B_FUNCTION_KEY:
      *code = KEY_F1 + key - 2;

      if (*code - KEY_F1 == 12)
	*code = KEY_PRINT;
      else if (*code - KEY_F1 == 13)
	/* Okay, Scroll Lock is a bit too much: keyboard.c doesn't
	   know about it yet, and it shouldn't, since that's a
	   modifier key.

	   *code = KEY_SCROLL_LOCK; */
	return -1;
      else if (*code - KEY_F1 == 14)
	*code = KEY_PAUSE;

      break;

    case B_HANGUL:
      *code = KEY_HANGUL;
      break;
    case B_HANGUL_HANJA:
      *code = KEY_HANGUL_HANJA;
      break;
    case B_KATAKANA_HIRAGANA:
      *code = KEY_HIRIGANA_KATAGANA;
      break;
    case B_HANKAKU_ZENKAKU:
      *code = KEY_ZENKAKU_HANKAKU;
      break;

    default:
      return 0;
    }

  return 1;
}

static void
map_key (char *chars, int32 offset, uint32_t *c)
{
  int size = chars[offset++];
  switch (size)
    {
    case 0:
      break;

    case 1:
      *c = chars[offset];
      break;

    default:
      {
        char str[5];
        int i = (size <= 4) ? size : 4;
        strncpy (str, &(chars[offset]), i);
        str[i] = '0';
	*c = BUnicodeChar::FromUTF8 ((char *) &str);
	break;
      }
    }
}

static void
map_shift (uint32_t kc, uint32_t *ch)
{
  if (!key_map_lock.Lock ())
    gui_abort ("Failed to lock keymap");
  if (!key_map)
    get_key_map (&key_map, &key_chars);
  if (!key_map)
    return;
  if (kc >= 128)
    return;

  int32_t m = key_map->shift_map[kc];
  map_key (key_chars, m, ch);
  key_map_lock.Unlock ();
}

static void
map_caps (uint32_t kc, uint32_t *ch)
{
  if (!key_map_lock.Lock ())
    gui_abort ("Failed to lock keymap");
  if (!key_map)
    get_key_map (&key_map, &key_chars);
  if (!key_map)
    return;
  if (kc >= 128)
    return;

  int32_t m = key_map->caps_map[kc];
  map_key (key_chars, m, ch);
  key_map_lock.Unlock ();
}

static void
map_caps_shift (uint32_t kc, uint32_t *ch)
{
  if (!key_map_lock.Lock ())
    gui_abort ("Failed to lock keymap");
  if (!key_map)
    get_key_map (&key_map, &key_chars);
  if (!key_map)
    return;
  if (kc >= 128)
    return;

  int32_t m = key_map->caps_shift_map[kc];
  map_key (key_chars, m, ch);
  key_map_lock.Unlock ();
}

static void
map_normal (uint32_t kc, uint32_t *ch)
{
  if (!key_map_lock.Lock ())
    gui_abort ("Failed to lock keymap");
  if (!key_map)
    get_key_map (&key_map, &key_chars);
  if (!key_map)
    return;
  if (kc >= 128)
    return;

  int32_t m = key_map->normal_map[kc];
  map_key (key_chars, m, ch);
  key_map_lock.Unlock ();
}

static BRect
get_zoom_rect (BWindow *window)
{
  BScreen screen;
  BDeskbar deskbar;
  BRect screen_frame;
  BRect frame;
  BRect deskbar_frame;
  BRect window_frame;
  BRect decorator_frame;

  if (!screen.IsValid ())
    gui_abort ("Failed to calculate screen rect");

  screen_frame = frame = screen.Frame ();
  deskbar_frame = deskbar.Frame ();

  if (!(modifiers () & B_SHIFT_KEY) && !deskbar.IsAutoHide ())
    {
      switch (deskbar.Location ())
	{
	case B_DESKBAR_TOP:
	  frame.top = deskbar_frame.bottom + 2;
	  break;

	case B_DESKBAR_BOTTOM:
	case B_DESKBAR_LEFT_BOTTOM:
	case B_DESKBAR_RIGHT_BOTTOM:
	  frame.bottom = deskbar_frame.top - 2;
	  break;

	case B_DESKBAR_LEFT_TOP:
	  if (!deskbar.IsExpanded ())
	    frame.top = deskbar_frame.bottom + 2;
	  else if (!deskbar.IsAlwaysOnTop ()
		   && !deskbar.IsAutoRaise ())
	    frame.left = deskbar_frame.right + 2;
	  break;

	default:
	  if (deskbar.IsExpanded ()
	      && !deskbar.IsAlwaysOnTop ()
	      && !deskbar.IsAutoRaise ())
	    frame.right = deskbar_frame.left - 2;
	}
    }

  if (window)
    {
      window_frame = window->Frame ();
      decorator_frame = window->DecoratorFrame ();

      frame.top += (window_frame.top
		    - decorator_frame.top);
      frame.bottom -= (decorator_frame.bottom
		       - window_frame.bottom);
      frame.left += (window_frame.left
		     - decorator_frame.left);
      frame.right -= (decorator_frame.right
		      - window_frame.right);

      if (frame.top > deskbar_frame.bottom
	  || frame.bottom < deskbar_frame.top)
	{
	  frame.left = screen_frame.left + (window_frame.left
					    - decorator_frame.left);
	  frame.right = screen_frame.right - (decorator_frame.right
					      - window_frame.right);
	}
    }

  return frame;
}

/* Invisible window used to get B_SCREEN_CHANGED events.  */
class EmacsScreenChangeMonitor : public BWindow
{
  BRect previous_screen_frame;

public:
  EmacsScreenChangeMonitor (void) : BWindow (BRect (-100, -100, 0, 0), "",
					     B_NO_BORDER_WINDOW_LOOK,
					     B_FLOATING_ALL_WINDOW_FEEL,
					     B_AVOID_FRONT | B_AVOID_FOCUS)
  {
    BScreen screen (this);

    if (!screen.IsValid ())
      return;

    previous_screen_frame = screen.Frame ();

    /* Immediately show this window upon creation.  It will not steal
       the focus or become visible.  */
    Show ();

    if (!LockLooper ())
      return;

    Hide ();
    UnlockLooper ();
  }

  void
  DispatchMessage (BMessage *msg, BHandler *handler)
  {
    struct haiku_screen_changed_event rq;
    BRect frame;

    if (msg->what == B_SCREEN_CHANGED)
      {
	if (msg->FindInt64 ("when", &rq.when) != B_OK)
	  rq.when = 0;

	if (msg->FindRect ("frame", &frame) != B_OK
	    || frame != previous_screen_frame)
	  {
	    haiku_write (SCREEN_CHANGED_EVENT, &rq);

	    if (frame.IsValid ())
	      previous_screen_frame = frame;
	  }
      }

    BWindow::DispatchMessage (msg, handler);
  }
};

class Emacs : public BApplication
{
public:
  BMessage settings;
  bool settings_valid_p;
  EmacsScreenChangeMonitor *monitor;

  Emacs (void) : BApplication ("application/x-vnd.GNU-emacs"),
		 settings_valid_p (false)
  {
    BPath settings_path;

    if (find_directory (B_USER_SETTINGS_DIRECTORY, &settings_path) != B_OK)
      return;

    settings_path.Append (PACKAGE_NAME);

    BEntry entry (settings_path.Path ());
    BFile settings_file (&entry, B_READ_ONLY | B_CREATE_FILE);

    if (settings.Unflatten (&settings_file) != B_OK)
      return;

    settings_valid_p = true;
    monitor = new EmacsScreenChangeMonitor;
  }

  ~Emacs (void)
  {
    if (monitor->LockLooper ())
      monitor->Quit ();
    else
      delete monitor;
  }

  void
  AboutRequested (void)
  {
    BAlert *about = new BAlert (PACKAGE_NAME,
				PACKAGE_STRING
				"\nThe extensible, self-documenting, real-time display editor.",
				"Close");
    about->Go ();
  }

  bool
  QuitRequested (void)
  {
    struct haiku_app_quit_requested_event rq;
    struct haiku_session_manager_reply reply;
    int32 reply_type;

    haiku_write (APP_QUIT_REQUESTED_EVENT, &rq);

    if (read_port (port_emacs_to_session_manager,
		   &reply_type, &reply, sizeof reply) < B_OK)
      /* Return true so the system kills us, since there's no real
	 alternative if this read fails.  */
      return true;

    return reply.quit_reply;
  }

  void
  MessageReceived (BMessage *msg)
  {
    struct haiku_clipboard_changed_event rq;

    if (msg->what == QUIT_APPLICATION)
      Quit ();
    else if (msg->what == B_CLIPBOARD_CHANGED)
      haiku_write (CLIPBOARD_CHANGED_EVENT, &rq);
    else if (msg->what == B_KEY_MAP_LOADED)
      {
	/* Install the new keymap.  Or rather, clear key_map -- Emacs
	   will fetch it again from the main thread the next time it
	   is needed.  */
	if (key_map_lock.Lock ())
	  {
	    if (key_map)
	      free (key_map);

	    if (key_chars)
	      free (key_chars);

	    key_map = NULL;
	    key_chars = NULL;
	    key_map_lock.Unlock ();
	  }
      }
    else
      BApplication::MessageReceived (msg);
  }
};

class EmacsWindow : public BWindow
{
public:
  struct child_frame
  {
    struct child_frame *next;
    int xoff, yoff;
    EmacsWindow *window;
  } *subset_windows;

  EmacsWindow *parent;
  BRect pre_fullscreen_rect;
  BRect pre_zoom_rect;
  int x_before_zoom;
  int y_before_zoom;
  bool shown_flag;
  volatile bool was_shown_p;
  bool menu_bar_active_p;
  bool override_redirect_p;
  window_look pre_override_redirect_look;
  window_feel pre_override_redirect_feel;
  uint32 pre_override_redirect_workspaces;
  int window_id;
  bool *menus_begun;
  enum haiku_z_group z_group;
  bool tooltip_p;
  enum haiku_fullscreen_mode fullscreen_mode;

  EmacsWindow () : BWindow (BRect (0, 0, 0, 0), "", B_TITLED_WINDOW_LOOK,
			    B_NORMAL_WINDOW_FEEL, B_NO_SERVER_SIDE_WINDOW_MODIFIERS),
		   subset_windows (NULL),
		   parent (NULL),
		   x_before_zoom (INT_MIN),
		   y_before_zoom (INT_MIN),
		   shown_flag (false),
		   was_shown_p (false),
		   menu_bar_active_p (false),
		   override_redirect_p (false),
		   menus_begun (NULL),
		   z_group (Z_GROUP_NONE),
		   tooltip_p (false),
		   fullscreen_mode (FULLSCREEN_MODE_NONE)
  {
    /* This pulse rate is used by scroll bars for repeating a button
       action while a button is held down.  */
    SetPulseRate (30000);
  }

  ~EmacsWindow ()
  {
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");
    struct child_frame *next;
    for (struct child_frame *f = subset_windows; f; f = next)
      {
	if (f->window->LockLooper ())
	  gui_abort ("Failed to lock looper for unparent");
	f->window->Unparent ();
	f->window->UnlockLooper ();
	next = f->next;
	delete f;
      }

    if (this->parent)
      UnparentAndUnlink ();
    child_frame_lock.Unlock ();
  }

  void
  RecomputeFeel (void)
  {
    if (override_redirect_p || tooltip_p)
      SetFeel (kMenuWindowFeel);
    else if (parent)
      SetFeel (B_FLOATING_SUBSET_WINDOW_FEEL);
    else if (z_group == Z_GROUP_ABOVE)
      SetFeel (B_FLOATING_ALL_WINDOW_FEEL);
    else
      SetFeel (B_NORMAL_WINDOW_FEEL);
  }

  void
  UpwardsSubset (EmacsWindow *w)
  {
    for (; w; w = w->parent)
      AddToSubset (w);
  }

  void
  UpwardsSubsetChildren (EmacsWindow *w)
  {
    if (!LockLooper ())
      gui_abort ("Failed to lock looper for subset");
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");
    UpwardsSubset (w);
    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      f->window->UpwardsSubsetChildren (w);
    child_frame_lock.Unlock ();
    UnlockLooper ();
  }

  void
  UpwardsUnSubset (EmacsWindow *w)
  {
    for (; w; w = w->parent)
      RemoveFromSubset (w);
  }

  void
  UpwardsUnSubsetChildren (EmacsWindow *w)
  {
    if (!LockLooper ())
      gui_abort ("Failed to lock looper for unsubset");
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");
    UpwardsUnSubset (w);
    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      f->window->UpwardsUnSubsetChildren (w);
    child_frame_lock.Unlock ();
    UnlockLooper ();
  }

  void
  Unparent (void)
  {
    EmacsWindow *parent;

    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    parent = this->parent;
    this->parent = NULL;
    RecomputeFeel ();
    UpwardsUnSubsetChildren (parent);
    this->RemoveFromSubset (this);
    child_frame_lock.Unlock ();
  }

  void
  UnparentAndUnlink (void)
  {
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");
    this->parent->UnlinkChild (this);
    this->Unparent ();
    child_frame_lock.Unlock ();
  }

  void
  UnlinkChild (EmacsWindow *window)
  {
    struct child_frame *last = NULL;
    struct child_frame *tem = subset_windows;

    for (; tem; last = tem, tem = tem->next)
      {
	if (tem->window == window)
	  {
	    if (last)
	      last->next = tem->next;
	    else
	      subset_windows = tem->next;
	    delete tem;
	    return;
	  }
      }

    gui_abort ("Failed to unlink child frame");
  }

  void
  ParentTo (EmacsWindow *window)
  {
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    if (this->parent)
      UnparentAndUnlink ();

    this->parent = window;
    RecomputeFeel ();
    this->AddToSubset (this);
    if (!IsHidden () && this->parent)
      UpwardsSubsetChildren (parent);
    window->LinkChild (this);

    child_frame_lock.Unlock ();
  }

  void
  LinkChild (EmacsWindow *window)
  {
    struct child_frame *f = new struct child_frame;

    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      {
	if (window == f->window)
	  gui_abort ("Trying to link a child frame that is already present");
      }

    f->window = window;
    f->next = subset_windows;
    f->xoff = -1;
    f->yoff = -1;

    subset_windows = f;
  }

  void
  MoveToIncludingFrame (int x, int y)
  {
    BRect decorator, frame;

    decorator = DecoratorFrame ();
    frame = Frame ();

    MoveTo (x + frame.left - decorator.left,
	    y + frame.top - decorator.top);
  }

  void
  DoMove (struct child_frame *f)
  {
    BRect frame = this->Frame ();
    f->window->MoveToIncludingFrame (frame.left + f->xoff,
				     frame.top + f->yoff);
  }

  void
  DoUpdateWorkspace (struct child_frame *f)
  {
    f->window->SetWorkspaces (this->Workspaces ());
  }

  void
  MoveChild (EmacsWindow *window, int xoff, int yoff,
	     int weak_p)
  {
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      {
	if (window == f->window)
	  {
	    f->xoff = xoff;
	    f->yoff = yoff;
	    if (!weak_p)
	      DoMove (f);

	    child_frame_lock.Unlock ();
	    return;
	  }
      }

    child_frame_lock.Unlock ();
    gui_abort ("Trying to move a child frame that doesn't exist");
  }

  void
  WindowActivated (bool activated)
  {
    struct haiku_activation_event rq;
    rq.window = this;
    rq.activated_p = activated;

    haiku_write (ACTIVATION, &rq);
  }

  void
  MessageReceived (BMessage *msg)
  {
    if (msg->WasDropped ())
      {
	BPoint whereto;
	int64 threadid;
	struct haiku_drag_and_drop_event rq;

	if (msg->FindInt64 ("emacs:thread_id", &threadid) == B_OK
	    && threadid == find_thread (NULL))
	  return;

	whereto = msg->DropPoint ();

	this->ConvertFromScreen (&whereto);

	rq.window = this;
	rq.message = DetachCurrentMessage ();
	rq.x = whereto.x;
	rq.y = whereto.y;

	haiku_write (DRAG_AND_DROP_EVENT, &rq);
      }
    else if (msg->GetPointer ("menuptr"))
      {
	struct haiku_menu_bar_select_event rq;

	rq.window = this;
	rq.ptr = (void *) msg->GetPointer ("menuptr");

	haiku_write (MENU_BAR_SELECT_EVENT, &rq);
      }
    else
      BWindow::MessageReceived (msg);
  }

  void
  DispatchMessage (BMessage *msg, BHandler *handler)
  {
    if (msg->what == B_KEY_DOWN || msg->what == B_KEY_UP)
      {
	struct haiku_key_event rq;

	/* Pass through key events to the regular dispatch mechanism
	   if the menu bar active, so that key navigation can work.  */
	if (menu_bar_active_p)
	  {
	    BWindow::DispatchMessage (msg, handler);
	    return;
	  }

	rq.window = this;

	int32 raw, key;
	int ret;
	msg->FindInt32 ("raw_char", &raw);
	msg->FindInt32 ("key", &key);
	msg->FindInt64 ("when", &rq.time);

	rq.modifiers = 0;
	uint32_t mods = modifiers ();

	if (mods & B_SHIFT_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_SHIFT;

	if (mods & B_CONTROL_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_CTRL;

	if (mods & B_COMMAND_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_ALT;

	if (mods & B_OPTION_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_SUPER;

	ret = keysym_from_raw_char (raw, key, &rq.keysym);

	if (!ret)
	  rq.keysym = 0;

	if (ret < 0)
	  return;

	rq.multibyte_char = 0;

	if (!rq.keysym)
	  {
	    if (mods & B_SHIFT_KEY)
	      {
		if (mods & B_CAPS_LOCK)
		  map_caps_shift (key, &rq.multibyte_char);
		else
		  map_shift (key, &rq.multibyte_char);
	      }
	    else
	      {
		if (mods & B_CAPS_LOCK)
		  map_caps (key, &rq.multibyte_char);
		else
		  map_normal (key, &rq.multibyte_char);
	      }
	  }

	haiku_write (msg->what == B_KEY_DOWN ? KEY_DOWN : KEY_UP, &rq);
      }
    else if (msg->what == B_MOUSE_WHEEL_CHANGED)
      {
	struct haiku_wheel_move_event rq;
	rq.window = this;
	rq.modifiers = 0;

	uint32_t mods = modifiers ();

	if (mods & B_SHIFT_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_SHIFT;

	if (mods & B_CONTROL_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_CTRL;

	if (mods & B_COMMAND_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_ALT;

	if (mods & B_OPTION_KEY)
	  rq.modifiers |= HAIKU_MODIFIER_SUPER;

	float dx, dy;
	if (msg->FindFloat ("be:wheel_delta_x", &dx) == B_OK &&
	    msg->FindFloat ("be:wheel_delta_y", &dy) == B_OK)
	  {
	    rq.delta_x = dx;
	    rq.delta_y = dy;

	    haiku_write (WHEEL_MOVE_EVENT, &rq);
	  };
      }
    else if (msg->what == SEND_MOVE_FRAME_EVENT)
      FrameMoved (Frame ().LeftTop ());
    else if (msg->what == B_SCREEN_CHANGED)
      {
	if (fullscreen_mode != FULLSCREEN_MODE_NONE)
	  SetFullscreen (fullscreen_mode);

	BWindow::DispatchMessage (msg, handler);
      }
    else
      BWindow::DispatchMessage (msg, handler);
  }

  void
  MenusBeginning (void)
  {
    struct haiku_menu_bar_state_event rq;

    rq.window = this;
    if (!menus_begun)
      haiku_write (MENU_BAR_OPEN, &rq);
    else
      *menus_begun = true;

    menu_bar_active_p = true;
  }

  void
  MenusEnded ()
  {
    struct haiku_menu_bar_state_event rq;
    rq.window = this;

    haiku_write (MENU_BAR_CLOSE, &rq);
    menu_bar_active_p = false;
  }

  void
  FrameResized (float newWidth, float newHeight)
  {
    struct haiku_resize_event rq;
    rq.window = this;
    rq.width = newWidth + 1.0f;
    rq.height = newHeight + 1.0f;

    haiku_write (FRAME_RESIZED, &rq);
    BWindow::FrameResized (newWidth, newHeight);
  }

  void
  FrameMoved (BPoint new_position)
  {
    struct haiku_move_event rq;
    BRect frame, decorator_frame;
    struct child_frame *f;

    if (fullscreen_mode == FULLSCREEN_MODE_WIDTH
	&& new_position.x != 0)
      {
	MoveTo (0, new_position.y);
	return;
      }

    if (fullscreen_mode == FULLSCREEN_MODE_HEIGHT
	&& new_position.y != 0)
      {
	MoveTo (new_position.x, 0);
	return;
      }

    rq.window = this;
    rq.x = std::lrint (new_position.x);
    rq.y = std::lrint (new_position.y);

    frame = Frame ();
    decorator_frame = DecoratorFrame ();

    rq.decorator_width
      = std::lrint (frame.left - decorator_frame.left);
    rq.decorator_height
      = std::lrint (frame.top - decorator_frame.top);

    haiku_write (MOVE_EVENT, &rq);

    CHILD_FRAME_LOCK_INSIDE_LOOPER_CALLBACK
      {
	for (f = subset_windows; f; f = f->next)
	  DoMove (f);
	child_frame_lock.Unlock ();

	BWindow::FrameMoved (new_position);
      }
  }

  void
  WorkspacesChanged (uint32_t old, uint32_t n)
  {
    struct child_frame *f;

    CHILD_FRAME_LOCK_INSIDE_LOOPER_CALLBACK
      {
	for (f = subset_windows; f; f = f->next)
	  DoUpdateWorkspace (f);

	child_frame_lock.Unlock ();
      }
  }

  void
  EmacsMoveTo (int x, int y)
  {
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    if (!this->parent)
      this->MoveToIncludingFrame (x, y);
    else
      this->parent->MoveChild (this, x, y, 0);
    child_frame_lock.Unlock ();
  }

  bool
  QuitRequested ()
  {
    struct haiku_quit_requested_event rq;
    rq.window = this;
    haiku_write (QUIT_REQUESTED, &rq);
    return false;
  }

  void
  Minimize (bool minimized_p)
  {
    struct haiku_iconification_event rq;

    rq.window = this;
    rq.iconified_p = !parent && minimized_p;
    haiku_write (ICONIFICATION, &rq);

    BWindow::Minimize (minimized_p);
  }

  void
  EmacsHide (void)
  {
    if (this->IsHidden ())
      return;
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    Hide ();
    if (this->parent)
      UpwardsUnSubsetChildren (this->parent);

    child_frame_lock.Unlock ();
  }

  void
  EmacsShow (void)
  {
    if (!this->IsHidden ())
      return;

    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    if (!was_shown_p)
      {
	/* This window is being shown for the first time, which means
	   Show will unlock the looper.  In this case, it should be
	   locked again, since the looper is unlocked when the window
	   is first created.  */

	if (!LockLooper ())
	  gui_abort ("Failed to lock looper during first window show");
	was_shown_p = true;
      }

    if (this->parent)
      shown_flag = 1;
    Show ();
    if (this->parent)
      UpwardsSubsetChildren (this->parent);

    child_frame_lock.Unlock ();
  }

  BRect
  ClearFullscreen (enum haiku_fullscreen_mode target_mode)
  {
    BRect original_frame;

    switch (fullscreen_mode)
      {
      case FULLSCREEN_MODE_MAXIMIZED:
	original_frame = pre_zoom_rect;

	if (target_mode == FULLSCREEN_MODE_NONE)
	  BWindow::Zoom (pre_zoom_rect.LeftTop (),
			 BE_RECT_WIDTH (pre_zoom_rect) - 1,
			 BE_RECT_HEIGHT (pre_zoom_rect) - 1);
	break;

      case FULLSCREEN_MODE_BOTH:
      case FULLSCREEN_MODE_HEIGHT:
      case FULLSCREEN_MODE_WIDTH:
	original_frame = pre_fullscreen_rect;
	SetFlags (Flags () & ~(B_NOT_MOVABLE
			       | B_NOT_ZOOMABLE
			       | B_NOT_RESIZABLE));

	if (target_mode != FULLSCREEN_MODE_NONE)
	  goto out;

	MoveTo (pre_fullscreen_rect.LeftTop ());
	ResizeTo (BE_RECT_WIDTH (pre_fullscreen_rect) - 1,
		  BE_RECT_HEIGHT (pre_fullscreen_rect) - 1);
	break;

      case FULLSCREEN_MODE_NONE:
	original_frame = Frame ();
	break;
      }

  out:
    fullscreen_mode = FULLSCREEN_MODE_NONE;
    return original_frame;
  }

  BRect
  FullscreenRectForMode (enum haiku_fullscreen_mode mode)
  {
    BScreen screen (this);
    BRect frame;

    if (!screen.IsValid ())
      return BRect (0, 0, 0, 0);

    frame = screen.Frame ();

    if (mode == FULLSCREEN_MODE_HEIGHT)
      frame.right -= BE_RECT_WIDTH (frame) / 2;
    else if (mode == FULLSCREEN_MODE_WIDTH)
      frame.bottom -= BE_RECT_HEIGHT (frame) / 2;

    return frame;
  }

  void
  SetFullscreen (enum haiku_fullscreen_mode mode)
  {
    BRect zoom_rect, frame;

    frame = ClearFullscreen (mode);

    switch (mode)
      {
      case FULLSCREEN_MODE_MAXIMIZED:
	pre_zoom_rect = frame;
	zoom_rect = get_zoom_rect (this);
	BWindow::Zoom (zoom_rect.LeftTop (),
		       BE_RECT_WIDTH (zoom_rect) - 1,
		       BE_RECT_HEIGHT (zoom_rect) - 1);
	break;

      case FULLSCREEN_MODE_BOTH:
	SetFlags (Flags () | B_NOT_MOVABLE);
	FALLTHROUGH;

      case FULLSCREEN_MODE_HEIGHT:
      case FULLSCREEN_MODE_WIDTH:
	SetFlags (Flags () | B_NOT_ZOOMABLE | B_NOT_RESIZABLE);
	pre_fullscreen_rect = frame;
	zoom_rect = FullscreenRectForMode (mode);
	ResizeTo (BE_RECT_WIDTH (zoom_rect) - 1,
		  BE_RECT_HEIGHT (zoom_rect) - 1);
	MoveTo (zoom_rect.left, zoom_rect.top);
	break;

      case FULLSCREEN_MODE_NONE:
	break;
      }

    fullscreen_mode = mode;
  }

  void
  Zoom (BPoint origin, float width, float height)
  {
    struct haiku_zoom_event rq;

    rq.window = this;
    rq.fullscreen_mode = fullscreen_mode;
    haiku_write (ZOOM_EVENT, &rq);
  }

  void
  OffsetChildRect (BRect *r, EmacsWindow *c)
  {
    if (!child_frame_lock.Lock ())
      gui_abort ("Failed to lock child frame state lock");

    for (struct child_frame *f; f; f = f->next)
      if (f->window == c)
	{
	  r->top -= f->yoff;
	  r->bottom -= f->yoff;
	  r->left -= f->xoff;
	  r->right -= f->xoff;
	  child_frame_lock.Unlock ();
	  return;
	}

    child_frame_lock.Lock ();
    gui_abort ("Trying to calculate offsets for a child frame that doesn't exist");
  }
};

class EmacsMenuBar : public BMenuBar
{
  bool tracking_p;

public:
  EmacsMenuBar () : BMenuBar (BRect (0, 0, 0, 0), NULL)
  {
  }

  void
  AttachedToWindow (void)
  {
    BWindow *window = Window ();

    window->SetKeyMenuBar (this);
  }

  void
  FrameResized (float newWidth, float newHeight)
  {
    struct haiku_menu_bar_resize_event rq;
    rq.window = this->Window ();
    rq.height = std::lrint (newHeight + 1);
    rq.width = std::lrint (newWidth + 1);

    haiku_write (MENU_BAR_RESIZE, &rq);
    BMenuBar::FrameResized (newWidth, newHeight);
  }

  void
  MouseDown (BPoint point)
  {
    struct haiku_menu_bar_click_event rq;
    EmacsWindow *ew = (EmacsWindow *) Window ();

    rq.window = ew;
    rq.x = std::lrint (point.x);
    rq.y = std::lrint (point.y);

    if (!ew->menu_bar_active_p)
      haiku_write (MENU_BAR_CLICK, &rq);
    else
      BMenuBar::MouseDown (point);
  }

  void
  MouseMoved (BPoint point, uint32 transit, const BMessage *msg)
  {
    struct haiku_menu_bar_left_event rq;

    if (transit == B_EXITED_VIEW)
      {
	rq.x = std::lrint (point.x);
	rq.y = std::lrint (point.y);
	rq.window = this->Window ();

	haiku_write (MENU_BAR_LEFT, &rq);
      }

    BMenuBar::MouseMoved (point, transit, msg);
  }

  void
  MessageReceived (BMessage *msg)
  {
    BRect frame;
    BPoint pt, l;
    EmacsWindow *window;
    bool menus_begun;

    if (msg->what == SHOW_MENU_BAR)
      {
	window = (EmacsWindow *) Window ();
	frame = Frame ();
	pt = frame.LeftTop ();
	l = pt;
	menus_begun = false;
	Parent ()->ConvertToScreen (&pt);

	window->menus_begun = &menus_begun;
	set_mouse_position (pt.x, pt.y);
	BMenuBar::MouseDown (l);
	window->menus_begun = NULL;

	if (!menus_begun)
	  msg->SendReply (msg);
	else
	  msg->SendReply (BE_MENU_BAR_OPEN);
      }
    else if (msg->what == REPLAY_MENU_BAR)
      {
	window = (EmacsWindow *) Window ();
	menus_begun = false;
	window->menus_begun = &menus_begun;

	if (msg->FindPoint ("emacs:point", &pt) == B_OK)
	  BMenuBar::MouseDown (pt);

	window->menus_begun = NULL;

	if (!menus_begun)
	  msg->SendReply (msg);
	else
	  msg->SendReply (BE_MENU_BAR_OPEN);
      }
    else
      BMenuBar::MessageReceived (msg);
  }
};

class EmacsView : public BView
{
public:
  int looper_locked_count;
  BRegion sb_region;
  BRegion invalid_region;

  BView *offscreen_draw_view;
  BBitmap *offscreen_draw_bitmap_1;
  BBitmap *copy_bitmap;

#ifdef USE_BE_CAIRO
  cairo_surface_t *cr_surface;
  cairo_t *cr_context;
  BLocker cr_surface_lock;
#endif

  BMessage *wait_for_release_message;
  int64 grabbed_buttons;
  BScreen screen;
  bool use_frame_synchronization;

  EmacsView () : BView (BRect (0, 0, 0, 0), "Emacs",
			B_FOLLOW_NONE, B_WILL_DRAW),
		 looper_locked_count (0),
		 offscreen_draw_view (NULL),
		 offscreen_draw_bitmap_1 (NULL),
		 copy_bitmap (NULL),
#ifdef USE_BE_CAIRO
		 cr_surface (NULL),
		 cr_context (NULL),
#endif
		 wait_for_release_message (NULL),
		 grabbed_buttons (0),
		 use_frame_synchronization (false)
  {

  }

  ~EmacsView ()
  {
    if (wait_for_release_message)
      {
	wait_for_release_message->SendReply (wait_for_release_message);
	delete wait_for_release_message;
      }

    TearDownDoubleBuffering ();

    if (!grab_view_locker.Lock ())
      gui_abort ("Couldn't lock grab view locker");
    if (grab_view == this)
      grab_view = NULL;
    grab_view_locker.Unlock ();
  }

  void
  SetFrameSynchronization (bool sync)
  {
    if (LockLooper ())
      {
	use_frame_synchronization = sync;
	UnlockLooper ();
      }
  }

  void
  MessageReceived (BMessage *msg)
  {
    uint32 buttons;
    BLooper *looper = Looper ();

    if (msg->what == WAIT_FOR_RELEASE)
      {
	if (wait_for_release_message)
	  gui_abort ("Wait for release message already exists");

	GetMouse (NULL, &buttons, false);

	if (!buttons)
	  msg->SendReply (msg);
	else
	  wait_for_release_message = looper->DetachCurrentMessage ();
      }
    else if (msg->what == RELEASE_NOW)
      {
	if (wait_for_release_message)
	  wait_for_release_message->SendReply (msg);

	delete wait_for_release_message;
	wait_for_release_message = NULL;
      }
    else
      BView::MessageReceived (msg);
  }

#ifdef USE_BE_CAIRO
  void
  DetachCairoSurface (void)
  {
    if (!cr_surface_lock.Lock ())
      gui_abort ("Could not lock cr surface during detachment");
    if (!cr_surface)
      gui_abort ("Trying to detach window cr surface when none exists");
    cairo_destroy (cr_context);
    cairo_surface_destroy (cr_surface);
    cr_surface = NULL;
    cr_context = NULL;
    cr_surface_lock.Unlock ();
  }

  void
  AttachCairoSurface (void)
  {
    if (!cr_surface_lock.Lock ())
      gui_abort ("Could not lock cr surface during attachment");
    if (cr_surface)
      gui_abort ("Trying to attach cr surface when one already exists");
    BRect bounds = offscreen_draw_bitmap_1->Bounds ();

    cr_surface = cairo_image_surface_create_for_data
      ((unsigned char *) offscreen_draw_bitmap_1->Bits (),
       CAIRO_FORMAT_ARGB32, BE_RECT_WIDTH (bounds),
       BE_RECT_HEIGHT (bounds),
       offscreen_draw_bitmap_1->BytesPerRow ());
    if (!cr_surface)
      gui_abort ("Cr surface allocation failed for double-buffered view");

    cr_context = cairo_create (cr_surface);
    if (!cr_context)
      gui_abort ("cairo_t allocation failed for double-buffered view");
    cr_surface_lock.Unlock ();
  }
#endif

  void
  TearDownDoubleBuffering (void)
  {
    if (offscreen_draw_view)
      {
	if (Window ())
	  ClearViewBitmap ();
	if (copy_bitmap)
	  {
	    delete copy_bitmap;
	    copy_bitmap = NULL;
	  }
	if (!looper_locked_count)
	  if (!offscreen_draw_view->LockLooper ())
	    gui_abort ("Failed to lock offscreen draw view");
#ifdef USE_BE_CAIRO
	if (cr_surface)
	  DetachCairoSurface ();
#endif
	offscreen_draw_view->RemoveSelf ();
	delete offscreen_draw_view;
	offscreen_draw_view = NULL;
	delete offscreen_draw_bitmap_1;
	offscreen_draw_bitmap_1 = NULL;
      }
   }

  void
  AfterResize (void)
  {
    if (offscreen_draw_view)
      {
	if (!LockLooper ())
	  gui_abort ("Failed to lock looper after resize");

	if (!offscreen_draw_view->LockLooper ())
	  gui_abort ("Failed to lock offscreen draw view after resize");
#ifdef USE_BE_CAIRO
	DetachCairoSurface ();
#endif
	offscreen_draw_view->RemoveSelf ();
	delete offscreen_draw_bitmap_1;
	offscreen_draw_bitmap_1 = new BBitmap (Frame (), B_RGBA32, 1);
	if (offscreen_draw_bitmap_1->InitCheck () != B_OK)
	  gui_abort ("Offscreen draw bitmap initialization failed");

	BRect frame = Frame ();

	offscreen_draw_view->MoveTo (frame.left, frame.top);
	offscreen_draw_view->ResizeTo (BE_RECT_WIDTH (frame),
				       BE_RECT_HEIGHT (frame));
	offscreen_draw_bitmap_1->AddChild (offscreen_draw_view);
#ifdef USE_BE_CAIRO
	AttachCairoSurface ();
#endif

	if (looper_locked_count)
	  offscreen_draw_bitmap_1->Lock ();

	UnlockLooper ();
      }
  }

  void
  Draw (BRect expose_bounds)
  {
    struct haiku_expose_event rq;
    EmacsWindow *w = (EmacsWindow *) Window ();

    if (w->shown_flag && offscreen_draw_view)
      {
	PushState ();
	SetDrawingMode (B_OP_ERASE);
	FillRect (Frame ());
	PopState ();
	return;
      }

    if (!offscreen_draw_view)
      {
	if (sb_region.Contains (std::lrint (expose_bounds.left),
				std::lrint (expose_bounds.top)) &&
	    sb_region.Contains (std::lrint (expose_bounds.right),
				std::lrint (expose_bounds.top)) &&
	    sb_region.Contains (std::lrint (expose_bounds.left),
				std::lrint (expose_bounds.bottom)) &&
	    sb_region.Contains (std::lrint (expose_bounds.right),
				std::lrint (expose_bounds.bottom)))
	  return;

	rq.x = std::floor (expose_bounds.left);
	rq.y = std::floor (expose_bounds.top);
	rq.width = std::ceil (expose_bounds.right - expose_bounds.left + 1);
	rq.height = std::ceil (expose_bounds.bottom - expose_bounds.top + 1);
	if (!rq.width)
	  rq.width = 1;
	if (!rq.height)
	  rq.height = 1;
	rq.window = this->Window ();

	haiku_write (FRAME_EXPOSED, &rq);
      }
  }

  void
  FlipBuffers (void)
  {
    EmacsWindow *w;
    if (!LockLooper ())
      gui_abort ("Failed to lock looper during buffer flip");
    if (!offscreen_draw_view)
      gui_abort ("Failed to lock offscreen view during buffer flip");

    offscreen_draw_view->Sync ();
    w = (EmacsWindow *) Window ();
    w->shown_flag = 0;

    if (copy_bitmap &&
	copy_bitmap->Bounds () != offscreen_draw_bitmap_1->Bounds ())
      {
	delete copy_bitmap;
	copy_bitmap = NULL;
      }
    if (!copy_bitmap)
      {
	copy_bitmap = new BBitmap (offscreen_draw_bitmap_1);
	SetViewBitmap (copy_bitmap, Frame (),
		       Frame (), B_FOLLOW_NONE, 0);
      }
    else
      copy_bitmap->ImportBits (offscreen_draw_bitmap_1);

    if (copy_bitmap->InitCheck () != B_OK)
      gui_abort ("Failed to init copy bitmap during buffer flip");

    /* Wait for VBLANK.  If responding to the invalidation or buffer
       flipping takes longer than the blanking period, we lose.  */
    if (use_frame_synchronization)
      screen.WaitForRetrace ();

    Invalidate (&invalid_region);
    invalid_region.MakeEmpty ();
    UnlockLooper ();
    return;
  }

  void
  SetUpDoubleBuffering (void)
  {
    if (!LockLooper ())
      gui_abort ("Failed to lock self setting up double buffering");
    if (offscreen_draw_view)
      gui_abort ("Failed to lock offscreen view setting up double buffering");

    offscreen_draw_bitmap_1 = new BBitmap (Frame (), B_RGBA32, 1);
    if (offscreen_draw_bitmap_1->InitCheck () != B_OK)
      gui_abort ("Failed to init offscreen bitmap");
#ifdef USE_BE_CAIRO
    AttachCairoSurface ();
#endif
    offscreen_draw_view = new BView (Frame (), NULL, B_FOLLOW_NONE, B_WILL_DRAW);
    offscreen_draw_bitmap_1->AddChild (offscreen_draw_view);

    if (looper_locked_count)
      {
	if (!offscreen_draw_bitmap_1->Lock ())
	  gui_abort ("Failed to lock bitmap after double buffering was set up");
      }

    invalid_region.MakeEmpty ();
    UnlockLooper ();
    Invalidate ();
  }

  void
  MouseMoved (BPoint point, uint32 transit, const BMessage *drag_msg)
  {
    struct haiku_mouse_motion_event rq;
    int64 threadid;
    EmacsWindow *window;

    window = (EmacsWindow *) Window ();

    if (transit == B_EXITED_VIEW)
      rq.just_exited_p = true;
    else
      rq.just_exited_p = false;

    rq.x = point.x;
    rq.y = point.y;
    rq.window = window;
    rq.time = system_time ();

    if (drag_msg && (drag_msg->IsSourceRemote ()
		     || drag_msg->FindInt64 ("emacs:thread_id",
					     &threadid) != B_OK
		     || threadid != find_thread (NULL)))
      rq.dnd_message = true;
    else
      rq.dnd_message = false;

    if (!grab_view_locker.Lock ())
      gui_abort ("Couldn't lock grab view locker");

    if (grab_view && this != grab_view)
      {
	grab_view_locker.Unlock ();
	return;
      }

    grab_view_locker.Unlock ();

    haiku_write (MOUSE_MOTION, &rq);
  }

  void
  BasicMouseDown (BPoint point, BView *scroll_bar, BMessage *message)
  {
    struct haiku_button_event rq;
    int64 when;
    int32 mods, buttons, button;

    if (message->FindInt64 ("when", &when) != B_OK
	|| message->FindInt32 ("modifiers", &mods) != B_OK
	|| message->FindInt32 ("buttons", &buttons) != B_OK)
      return;

    /* Find which button was pressed by comparing the previous button
       mask to the current one.  This assumes that B_MOUSE_DOWN will
       be sent for each button press.  */
    button = buttons & ~grabbed_buttons;
    grabbed_buttons = buttons;

    if (!scroll_bar)
      {
	if (!grab_view_locker.Lock ())
	  gui_abort ("Couldn't lock grab view locker");
	grab_view = this;
	grab_view_locker.Unlock ();
      }

    rq.window = this->Window ();
    rq.scroll_bar = scroll_bar;

    if (button == B_PRIMARY_MOUSE_BUTTON)
      rq.btn_no = 0;
    else if (button == B_SECONDARY_MOUSE_BUTTON)
      rq.btn_no = 2;
    else if (button == B_TERTIARY_MOUSE_BUTTON)
      rq.btn_no = 1;
    else
      /* We don't know which button was pressed.  This usually happens
	 when a B_MOUSE_UP is sent to a view that didn't receive a
	 corresponding B_MOUSE_DOWN event, so simply ignore the
	 message.  */
      return;

    rq.x = point.x;
    rq.y = point.y;
    rq.modifiers = 0;

    if (mods & B_SHIFT_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SHIFT;

    if (mods & B_CONTROL_KEY)
      rq.modifiers |= HAIKU_MODIFIER_CTRL;

    if (mods & B_COMMAND_KEY)
      rq.modifiers |= HAIKU_MODIFIER_ALT;

    if (mods & B_OPTION_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SUPER;

    if (!scroll_bar)
      SetMouseEventMask (B_POINTER_EVENTS, (B_LOCK_WINDOW_FOCUS
					    | B_NO_POINTER_HISTORY));

    rq.time = when;
    haiku_write (BUTTON_DOWN, &rq);
  }

  void
  MouseDown (BPoint point)
  {
    BMessage *msg;
    BLooper *looper;

    looper = Looper ();
    msg = (looper
	   ? looper->CurrentMessage ()
	   : NULL);

    if (msg)
      BasicMouseDown (point, NULL, msg);
  }

  void
  BasicMouseUp (BPoint point, BView *scroll_bar, BMessage *message)
  {
    struct haiku_button_event rq;
    int64 when;
    int32 mods, button, buttons;

    if (message->FindInt64 ("when", &when) != B_OK
	|| message->FindInt32 ("modifiers", &mods) != B_OK
	|| message->FindInt32 ("buttons", &buttons) != B_OK)
      return;

    if (!scroll_bar)
      {
	if (!grab_view_locker.Lock ())
	  gui_abort ("Couldn't lock grab view locker");
	if (!buttons)
	  grab_view = NULL;
	grab_view_locker.Unlock ();
      }

    button = (grabbed_buttons & ~buttons);
    grabbed_buttons = buttons;

    if (wait_for_release_message)
      {
	if (!grabbed_buttons)
	  {
	    wait_for_release_message->SendReply (wait_for_release_message);
	    delete wait_for_release_message;
	    wait_for_release_message = NULL;
	  }

	return;
      }

    rq.window = this->Window ();
    rq.scroll_bar = scroll_bar;

    if (button == B_PRIMARY_MOUSE_BUTTON)
      rq.btn_no = 0;
    else if (button == B_SECONDARY_MOUSE_BUTTON)
      rq.btn_no = 2;
    else if (button == B_TERTIARY_MOUSE_BUTTON)
      rq.btn_no = 1;
    else
      return;

    rq.x = point.x;
    rq.y = point.y;

    rq.modifiers = 0;
    if (mods & B_SHIFT_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SHIFT;

    if (mods & B_CONTROL_KEY)
      rq.modifiers |= HAIKU_MODIFIER_CTRL;

    if (mods & B_COMMAND_KEY)
      rq.modifiers |= HAIKU_MODIFIER_ALT;

    if (mods & B_OPTION_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SUPER;

    rq.time = when;
    haiku_write (BUTTON_UP, &rq);
  }

  void
  MouseUp (BPoint point)
  {
    BMessage *msg;
    BLooper *looper;

    looper = Looper ();
    msg = (looper
	   ? looper->CurrentMessage ()
	   : NULL);

    if (msg)
      BasicMouseUp (point, NULL, msg);
  }
};

class EmacsScrollBar : public BScrollBar
{
public:
  int dragging;
  bool horizontal;
  enum haiku_scroll_bar_part current_part;
  float old_value;
  scroll_bar_info info;

  /* How many button events were passed to the parent without
     release.  */
  int handle_button_count;
  bool in_overscroll;
  bool can_overscroll;
  bool maybe_overscroll;
  BPoint last_overscroll;
  int last_reported_overscroll_value;
  int max_value, real_max_value;
  int overscroll_start_value;
  bigtime_t repeater_start;
  EmacsView *parent;

  EmacsScrollBar (int x, int y, int x1, int y1, bool horizontal_p,
		  EmacsView *parent)
    : BScrollBar (BRect (x, y, x1, y1), NULL, NULL, 0, 0, horizontal_p ?
		  B_HORIZONTAL : B_VERTICAL),
      dragging (0),
      handle_button_count (0),
      in_overscroll (false),
      can_overscroll (false),
      maybe_overscroll (false),
      parent (parent)
  {
    BView *vw = (BView *) this;
    vw->SetResizingMode (B_FOLLOW_NONE);
    horizontal = horizontal_p;
    get_scroll_bar_info (&info);
    SetSteps (5000, 10000);
  }

  void
  MessageReceived (BMessage *msg)
  {
    int32 portion, range, dragging, value;
    float proportion;

    if (msg->what == SCROLL_BAR_UPDATE)
      {
	portion = msg->GetInt32 ("emacs:portion", 0);
	range = msg->GetInt32 ("emacs:range", 0);
	dragging = msg->GetInt32 ("emacs:dragging", 0);
	proportion = ((range <= 0 || portion <= 0)
		      ? 1.0f : (float) portion / range);
	value = msg->GetInt32 ("emacs:units", 0);
	can_overscroll = msg->GetBool ("emacs:overscroll", false);

	if (value < 0)
	  value = 0;

	if (dragging != 1)
	  {
	    if (in_overscroll || dragging != -1)
	      {
		/* Set the value to the smallest possible one.
		   Otherwise, the call to SetRange could lead to
		   spurious updates.  */
		old_value = 0;
		SetValue (0);

		/* Unlike on Motif, PORTION isn't included in the total
		   range of the scroll bar.  */

		SetRange (0, range - portion);
		SetProportion (proportion);
		max_value = range - portion;
		real_max_value = range;

		if (in_overscroll || value > max_value)
		  value = max_value;

		old_value = roundf (value);
		SetValue (old_value);
	      }
	    else
	      {
		value = Value ();

		old_value = 0;
		SetValue (0);
		SetRange (0, range - portion);
		SetProportion (proportion);
		old_value = value;
		SetValue (value);
		max_value = range - portion;
		real_max_value = range;
	      }
	  }
      }

    BScrollBar::MessageReceived (msg);
  }

  void
  Pulse (void)
  {
    struct haiku_scroll_bar_part_event rq;
    BPoint point;
    uint32 buttons;

    if (!dragging)
      {
	SetFlags (Flags () & ~B_PULSE_NEEDED);
	return;
      }

    if (repeater_start < system_time ())
      {
	GetMouse (&point, &buttons, false);

	if (ButtonRegionFor (current_part).Contains (point))
	  {
	    rq.scroll_bar = this;
	    rq.window = Window ();
	    rq.part = current_part;
	    haiku_write (SCROLL_BAR_PART_EVENT, &rq);
	  }
      }

    BScrollBar::Pulse ();
  }

  void
  ValueChanged (float new_value)
  {
    struct haiku_scroll_bar_value_event rq;

    new_value = Value ();

    if (dragging)
      {
	if (new_value != old_value)
	  {
	    if (dragging > 1)
	      {
		SetValue (old_value);
		SetFlags (Flags () | B_PULSE_NEEDED);
	      }
	    else
	      dragging++;
	  }

	return;
      }

    if (new_value != old_value)
      {
	rq.scroll_bar = this;
	rq.window = Window ();
	rq.position = new_value;
	old_value = new_value;

	haiku_write (SCROLL_BAR_VALUE_EVENT, &rq);
      }
  }

  BRegion
  ButtonRegionFor (enum haiku_scroll_bar_part button)
  {
    BRegion region;
    BRect bounds;
    BRect rect;
    float button_size;

    bounds = Bounds ();
    bounds.InsetBy (0.0, 0.0);

    if (horizontal)
      button_size = bounds.Height () + 1.0f;
    else
      button_size = bounds.Width () + 1.0f;

    rect = BRect (bounds.left, bounds.top,
		  bounds.left + button_size - 1.0f,
		  bounds.top + button_size - 1.0f);

    if (button == HAIKU_SCROLL_BAR_UP_BUTTON)
      {
	if (!horizontal)
	  {
	    region.Include (rect);
	    if (info.double_arrows)
	      region.Include (rect.OffsetToCopy (bounds.left,
						 bounds.bottom - 2 * button_size + 1));
	  }
	else
	  {
	    region.Include (rect);
	    if (info.double_arrows)
	      region.Include (rect.OffsetToCopy (bounds.right - 2 * button_size,
						 bounds.top));
	  }
      }
    else
      {
	if (!horizontal)
	  {
	    region.Include (rect.OffsetToCopy (bounds.left, bounds.bottom - button_size));

	    if (info.double_arrows)
	      region.Include (rect.OffsetByCopy (0.0, button_size));
	  }
	else
	  {
	    region.Include (rect.OffsetToCopy (bounds.right - button_size, bounds.top));

	    if (info.double_arrows)
	      region.Include (rect.OffsetByCopy (button_size, 0.0));
	  }
      }

    return region;
  }

  void
  MouseDown (BPoint pt)
  {
    struct haiku_scroll_bar_drag_event rq;
    struct haiku_scroll_bar_part_event part;
    BRegion r;
    BLooper *looper;
    BMessage *message;
    int32 buttons, mods;

    looper = Looper ();
    message = NULL;

    if (!looper)
      GetMouse (&pt, (uint32 *) &buttons, false);
    else
      {
	message = looper->CurrentMessage ();

	if (!message || message->FindInt32 ("buttons", &buttons) != B_OK)
	  GetMouse (&pt, (uint32 *) &buttons, false);
      }

    if (message && (message->FindInt32 ("modifiers", &mods)
		    == B_OK)
	&& mods & B_CONTROL_KEY)
      {
	/* Allow C-mouse-3 to split the window on a scroll bar.   */
	handle_button_count += 1;
	SetMouseEventMask (B_POINTER_EVENTS, (B_SUSPEND_VIEW_FOCUS
					      | B_LOCK_WINDOW_FOCUS));
	parent->BasicMouseDown (ConvertToParent (pt), this, message);

	return;
      }

    repeater_start = system_time () + 300000;

    if (buttons == B_PRIMARY_MOUSE_BUTTON)
      {
	r = ButtonRegionFor (HAIKU_SCROLL_BAR_UP_BUTTON);

	if (r.Contains (pt))
	  {
	    part.scroll_bar = this;
	    part.window = Window ();
	    part.part = HAIKU_SCROLL_BAR_UP_BUTTON;
	    dragging = 1;
	    current_part = HAIKU_SCROLL_BAR_UP_BUTTON;

	    haiku_write (SCROLL_BAR_PART_EVENT, &part);
	    goto out;
	  }

	r = ButtonRegionFor (HAIKU_SCROLL_BAR_DOWN_BUTTON);

	if (r.Contains (pt))
	  {
	    part.scroll_bar = this;
	    part.window = Window ();
	    part.part = HAIKU_SCROLL_BAR_DOWN_BUTTON;
	    dragging = 1;
	    current_part = HAIKU_SCROLL_BAR_DOWN_BUTTON;

	    if (Value () == max_value)
	      {
		SetFlags (Flags () | B_PULSE_NEEDED);
		dragging = 2;
	      }

	    haiku_write (SCROLL_BAR_PART_EVENT, &part);
	    goto out;
	  }

	maybe_overscroll = true;
      }

    rq.dragging_p = 1;
    rq.window = Window ();
    rq.scroll_bar = this;

    SetMouseEventMask (B_POINTER_EVENTS, (B_SUSPEND_VIEW_FOCUS
					  | B_LOCK_WINDOW_FOCUS));

    haiku_write (SCROLL_BAR_DRAG_EVENT, &rq);

  out:
    BScrollBar::MouseDown (pt);
  }

  void
  MouseUp (BPoint pt)
  {
    struct haiku_scroll_bar_drag_event rq;
    BMessage *msg;
    BLooper *looper;

    in_overscroll = false;
    maybe_overscroll = false;

    if (handle_button_count)
      {
	handle_button_count--;
	looper = Looper ();
	msg = (looper
	       ? looper->CurrentMessage ()
	       : NULL);

	if (msg)
	  parent->BasicMouseUp (ConvertToParent (pt),
				this, msg);

	return;
      }

    rq.dragging_p = 0;
    rq.scroll_bar = this;
    rq.window = Window ();

    haiku_write (SCROLL_BAR_DRAG_EVENT, &rq);
    dragging = 0;

    BScrollBar::MouseUp (pt);
  }

  void
  MouseMoved (BPoint point, uint32 transit, const BMessage *msg)
  {
    struct haiku_menu_bar_left_event rq;
    struct haiku_scroll_bar_value_event value_event;
    int range, diff, value, trough_size;
    BRect bounds;
    BPoint conv;
    uint32 buttons;

    GetMouse (NULL, &buttons, false);

    if (transit == B_EXITED_VIEW)
      {
	conv = ConvertToParent (point);

	rq.x = std::lrint (conv.x);
	rq.y = std::lrint (conv.y);
	rq.window = this->Window ();

	haiku_write (MENU_BAR_LEFT, &rq);
      }

    if (in_overscroll)
      {
	if (horizontal)
	  diff = point.x - last_overscroll.x;
	else
	  diff = point.y - last_overscroll.y;

	if (diff < 0)
	  {
	    in_overscroll = false;
	    goto allow;
	  }

	range = real_max_value;
	bounds = Bounds ();
	bounds.InsetBy (1.0, 1.0);
	value = overscroll_start_value;
	trough_size = (horizontal
		       ? BE_RECT_WIDTH (bounds)
		       : BE_RECT_HEIGHT (bounds));
	trough_size -= (horizontal
			? BE_RECT_HEIGHT (bounds)
			: BE_RECT_WIDTH (bounds)) / 2;
	if (info.double_arrows)
	  trough_size -= (horizontal
			  ? BE_RECT_HEIGHT (bounds)
			  : BE_RECT_WIDTH (bounds)) / 2;

	value += ((double) range / trough_size) * diff;

	if (value != last_reported_overscroll_value)
	  {
	    last_reported_overscroll_value = value;

	    value_event.scroll_bar = this;
	    value_event.window = Window ();
	    value_event.position = value;

	    haiku_write (SCROLL_BAR_VALUE_EVENT, &value_event);
	    return;
	  }
      }
    else if (can_overscroll
	     && (buttons == B_PRIMARY_MOUSE_BUTTON)
	     && maybe_overscroll)
      {
	value = Value ();

	if (value >= max_value)
	  {
	    BScrollBar::MouseMoved (point, transit, msg);

	    if (value == Value ())
	      {
		overscroll_start_value = value;
		in_overscroll = true;
		last_overscroll = point;
		last_reported_overscroll_value = value;

		MouseMoved (point, transit, msg);
		return;
	      }
	  }
      }

  allow:
    BScrollBar::MouseMoved (point, transit, msg);
  }
};

class EmacsTitleMenuItem : public BMenuItem
{
public:
  EmacsTitleMenuItem (const char *str) : BMenuItem (str, NULL)
  {
    SetEnabled (0);
  }

  void
  DrawContent (void)
  {
    BMenu *menu = Menu ();

    menu->PushState ();
    menu->SetFont (be_bold_font);
    menu->SetHighColor (ui_color (B_MENU_ITEM_TEXT_COLOR));
    BMenuItem::DrawContent ();
    menu->PopState ();
  }
};

class EmacsMenuItem : public BMenuItem
{
public:
  int menu_bar_id;
  void *menu_ptr;
  void *wind_ptr;
  char *key;
  char *help;

  EmacsMenuItem (const char *key_label, const char *label,
		 const char *help, BMessage *message = NULL)
    : BMenuItem (label, message),
      menu_bar_id (-1),
      menu_ptr (NULL),
      wind_ptr (NULL),
      key (NULL),
      help (NULL)
  {
    if (key_label)
      key = strdup (key_label);

    if (help)
      this->help = strdup (help);
  }

  ~EmacsMenuItem ()
  {
    if (key)
      free (key);
    if (help)
      free (help);
  }

  void
  DrawContent (void)
  {
    BMenu *menu = Menu ();

    BMenuItem::DrawContent ();

    if (key)
      {
	BRect r = Frame ();
	int w;

	menu->PushState ();
	menu->ClipToRect (r);
	menu->SetFont (be_plain_font);
	w = menu->StringWidth (key);
	menu->MovePenTo (BPoint (BE_RECT_WIDTH (r) - w - 4,
				 menu->PenLocation ().y));
	menu->DrawString (key);
	menu->PopState ();
      }
  }

  void
  GetContentSize (float *w, float *h)
  {
    BMenuItem::GetContentSize (w, h);
    if (Menu () && key)
      *w += 4 + Menu ()->StringWidth (key);
  }

  void
  Highlight (bool highlight_p)
  {
    struct haiku_menu_bar_help_event rq;
    struct haiku_dummy_event dummy;
    BMenu *menu = Menu ();
    BRect r;
    BPoint pt;
    uint32 buttons;

    if (help)
      menu->SetToolTip (highlight_p ? help : NULL);
    else
      {
	rq.window = wind_ptr;
	rq.mb_idx = highlight_p ? menu_bar_id : -1;
	rq.highlight_p = highlight_p;
	rq.data = menu_ptr;

	r = Frame ();
	menu->GetMouse (&pt, &buttons);

	if (!highlight_p || r.Contains (pt))
	  {
	    if (menu_bar_id > 0)
	      haiku_write (MENU_BAR_HELP_EVENT, &rq);
	    else
	      {
		haiku_write_without_signal (MENU_BAR_HELP_EVENT, &rq, true);
		haiku_write (DUMMY_EVENT, &dummy);
	      }
	  }
      }

    BMenuItem::Highlight (highlight_p);
  }
};

class EmacsFontPreviewDialog : public BWindow
{
  BStringView text_view;
  BMessenger preview_source;
  BFont *current_font;
  bool is_visible;

  void
  DoLayout (void)
  {
    float width, height;

    text_view.GetPreferredSize (&width, &height);
    text_view.ResizeTo (width - 1, height - 1);

    SetSizeLimits (width, width, height, height);
    ResizeTo (width - 1, height - 1);
  }

  bool
  QuitRequested (void)
  {
    preview_source.SendMessage (QUIT_PREVIEW_DIALOG);

    return false;
  }

  void
  MessageReceived (BMessage *message)
  {
    int32 family, style;
    uint32 flags;
    font_family name;
    font_style sname;
    status_t rc;
    const char *size_name;
    int size;

    if (message->what == SET_FONT_INDICES)
      {
	size_name = message->FindString ("emacs:size");

	if (message->FindInt32 ("emacs:family", &family) != B_OK
	    || message->FindInt32 ("emacs:style", &style) != B_OK)
	  return;

	rc = get_font_family (family, &name, &flags);

	if (rc != B_OK)
	  return;

	rc = get_font_style (name, style, &sname, &flags);

	if (rc != B_OK)
	  return;

	if (current_font)
	  delete current_font;

	current_font = new BFont;
	current_font->SetFamilyAndStyle (name, sname);

	if (message->GetBool ("emacs:disable_antialiasing", false))
	  current_font->SetFlags (B_DISABLE_ANTIALIASING);

	if (size_name && strlen (size_name))
	  {
	    size = atoi (size_name);
	    current_font->SetSize (size);
	  }

	text_view.SetFont (current_font);
	DoLayout ();
	return;
      }

    BWindow::MessageReceived (message);
  }

public:

  EmacsFontPreviewDialog (BWindow *target)
    : BWindow (BRect (45, 45, 500, 300),
	       "Preview font",
	       B_FLOATING_WINDOW_LOOK,
	       B_MODAL_APP_WINDOW_FEEL,
	       B_NOT_ZOOMABLE | B_NOT_RESIZABLE),
      text_view (BRect (0, 0, 0, 0),
		 NULL, "The quick brown fox "
		 "jumped over the lazy dog"),
      preview_source (target),
      current_font (NULL)
  {
    AddChild (&text_view);
    DoLayout ();
  }

  ~EmacsFontPreviewDialog (void)
  {
    text_view.RemoveSelf ();

    if (current_font)
      delete current_font;
  }
};

class TripleLayoutView : public BView
{
  BScrollView *view_1;
  BView *view_2, *view_3;

  void
  FrameResized (float new_width, float new_height)
  {
    BRect frame;
    float width, height, height_1, width_1;
    float basic_height;

    frame = Frame ();

    view_2->GetPreferredSize (&width, &height);
    view_3->GetPreferredSize (&width_1, &height_1);

    basic_height = height + height_1;

    view_1->MoveTo (0, 0);
    view_1->ResizeTo (BE_RECT_WIDTH (frame),
		      BE_RECT_HEIGHT (frame) - basic_height);
    view_2->MoveTo (2, BE_RECT_HEIGHT (frame) - basic_height);
    view_2->ResizeTo (BE_RECT_WIDTH (frame) - 4, height);
    view_3->MoveTo (2, BE_RECT_HEIGHT (frame) - height_1);
    view_3->ResizeTo (BE_RECT_WIDTH (frame) - 4, height_1);

    BView::FrameResized (new_width, new_height);
  }

  /* This is called by the BSplitView.  */
  BSize
  MinSize (void)
  {
    float width, height;
    float width_1, height_1;
    BSize size_1;

    size_1 = view_1->MinSize ();
    view_2->GetPreferredSize (&width, &height);
    view_3->GetPreferredSize (&width_1, &height_1);

    return BSize (std::max (size_1.width,
			    std::max (width_1, width)),
		  std::max (size_1.height, height + height_1));
  }

public:
  TripleLayoutView (BScrollView *first, BView *second,
		    BView *third) : BView (NULL, B_FRAME_EVENTS),
				    view_1 (first),
				    view_2 (second),
				    view_3 (third)
  {
    FrameResized (801, 801);
  }
};

class EmacsFontSelectionDialog : public BWindow
{
  BView basic_view;
  BCheckBox antialias_checkbox;
  BCheckBox preview_checkbox;
  BSplitView split_view;
  BListView font_family_pane;
  BListView font_style_pane;
  BScrollView font_family_scroller;
  BScrollView font_style_scroller;
  TripleLayoutView style_view;
  BObjectList<BStringItem> all_families;
  BObjectList<BStringItem> all_styles;
  BButton cancel_button, ok_button;
  BTextControl size_entry;
  port_id comm_port;
  bool allow_monospace_only;
  int pending_selection_idx;
  EmacsFontPreviewDialog *preview;

  void
  ShowPreview (void)
  {
    if (!preview)
      {
	preview = new EmacsFontPreviewDialog (this);
	preview->Show ();

	UpdatePreview ();
      }
  }

  void
  UpdatePreview (void)
  {
    int family, style;
    BMessage message;
    BMessenger messenger (preview);

    family = font_family_pane.CurrentSelection ();
    style = font_style_pane.CurrentSelection ();

    message.what = SET_FONT_INDICES;
    message.AddInt32 ("emacs:family", family);
    message.AddInt32 ("emacs:style", style);

    if (antialias_checkbox.Value () == B_CONTROL_ON)
      message.AddBool ("emacs:disable_antialiasing", true);

    message.AddString ("emacs:size",
		       size_entry.Text ());

    messenger.SendMessage (&message);
  }

  void
  HidePreview (void)
  {
    if (preview)
      {
	if (preview->LockLooper ())
	  preview->Quit ();
	/* I hope this works.  */
	else
	  delete preview;

	preview = NULL;
      }
  }

  void
  UpdateStylesForIndex (int idx)
  {
    int n, i, previous_selection;
    uint32 flags;
    font_family family;
    font_style style;
    BStringItem *item;
    char *current_style;

    n = all_styles.CountItems ();
    current_style = NULL;
    previous_selection = font_style_pane.CurrentSelection ();

    if (previous_selection >= 0)
      {
	item = all_styles.ItemAt (previous_selection);
	current_style = strdup (item->Text ());
      }

    font_style_pane.MakeEmpty ();
    all_styles.MakeEmpty ();

    if (get_font_family (idx, &family, &flags) == B_OK)
      {
	n = count_font_styles (family);

	for (i = 0; i < n; ++i)
	  {
	    if (get_font_style (family, i, &style, &flags) == B_OK)
	      item = new BStringItem (style);
	    else
	      item = new BStringItem ("<error>");

	    if (current_style && pending_selection_idx < 0
		&& !strcmp (current_style, style))
	      pending_selection_idx = i;

	    font_style_pane.AddItem (item);
	    all_styles.AddItem (item);
	  }
      }

    if (pending_selection_idx >= 0)
      {
	font_style_pane.Select (pending_selection_idx);
	font_style_pane.ScrollToSelection ();
      }

    pending_selection_idx = -1;
    UpdateForSelectedStyle ();

    if (current_style)
      free (current_style);
  }

  bool
  QuitRequested (void)
  {
    struct font_selection_dialog_message rq;

    rq.cancel = true;
    write_port (comm_port, 0, &rq, sizeof rq);

    return false;
  }

  void
  UpdateForSelectedStyle (void)
  {
    int style = font_style_pane.CurrentSelection ();

    if (style < 0)
      ok_button.SetEnabled (false);
    else
      ok_button.SetEnabled (true);

    if (style >= 0 && preview)
      UpdatePreview ();
  }

  void
  MessageReceived (BMessage *msg)
  {
    const char *text;
    int idx;
    struct font_selection_dialog_message rq;

    if (msg->what == FONT_FAMILY_SELECTED)
      {
	idx = font_family_pane.CurrentSelection ();
	UpdateStylesForIndex (idx);
      }
    else if (msg->what == FONT_STYLE_SELECTED)
      UpdateForSelectedStyle ();
    else if (msg->what == B_OK
	     && font_style_pane.CurrentSelection () >= 0)
      {
	text = size_entry.Text ();

	rq.cancel = false;
	rq.family_idx = font_family_pane.CurrentSelection ();
	rq.style_idx = font_style_pane.CurrentSelection ();
	rq.size = atoi (text);
	rq.size_specified = rq.size > 0 || strlen (text);

	if (antialias_checkbox.Value () == B_CONTROL_ON)
	  rq.disable_antialias = true;
	else
	  rq.disable_antialias = false;

	write_port (comm_port, 0, &rq, sizeof rq);
      }
    else if (msg->what == B_CANCEL)
      {
	rq.cancel = true;

	write_port (comm_port, 0, &rq, sizeof rq);
      }
    else if (msg->what == SET_PREVIEW_DIALOG)
      {
	if (preview_checkbox.Value () == B_CONTROL_OFF)
	  HidePreview ();
	else
	  ShowPreview ();
      }
    else if (msg->what == QUIT_PREVIEW_DIALOG)
      {
	preview_checkbox.SetValue (B_CONTROL_OFF);
	HidePreview ();
      }
    else if (msg->what == UPDATE_PREVIEW_DIALOG)
      {
	if (preview)
	  UpdatePreview ();
      }
    else if (msg->what == SET_DISABLE_ANTIALIASING)
      {
	if (preview)
	  UpdatePreview ();
      }

    BWindow::MessageReceived (msg);
  }

public:

  ~EmacsFontSelectionDialog (void)
  {
    if (preview)
      {
	if (preview->LockLooper ())
	  preview->Quit ();
	/* I hope this works.  */
	else
	  delete preview;
      }

    font_family_pane.MakeEmpty ();
    font_style_pane.MakeEmpty ();

    font_family_pane.RemoveSelf ();
    font_style_pane.RemoveSelf ();
    antialias_checkbox.RemoveSelf ();
    preview_checkbox.RemoveSelf ();
    style_view.RemoveSelf ();
    font_family_scroller.RemoveSelf ();
    font_style_scroller.RemoveSelf ();
    cancel_button.RemoveSelf ();
    ok_button.RemoveSelf ();
    size_entry.RemoveSelf ();
    basic_view.RemoveSelf ();

    if (comm_port >= B_OK)
      delete_port (comm_port);
  }

  EmacsFontSelectionDialog (bool monospace_only,
			    int initial_family_idx,
			    int initial_style_idx,
			    int initial_size,
			    bool initial_antialias)
    : BWindow (BRect (0, 0, 500, 500),
	       "Select font from list",
	       B_TITLED_WINDOW_LOOK,
	       B_MODAL_APP_WINDOW_FEEL, 0),
      basic_view (NULL, 0),
      antialias_checkbox ("Disable antialiasing", "Disable antialiasing",
			  new BMessage (SET_DISABLE_ANTIALIASING)),
      preview_checkbox ("Show preview", "Show preview",
			new BMessage (SET_PREVIEW_DIALOG)),
      font_family_pane (BRect (0, 0, 0, 0), NULL,
			B_SINGLE_SELECTION_LIST,
			B_FOLLOW_ALL_SIDES),
      font_style_pane (BRect (0, 0, 0, 0), NULL,
		       B_SINGLE_SELECTION_LIST,
		       B_FOLLOW_ALL_SIDES),
      font_family_scroller (NULL, &font_family_pane,
			    B_FOLLOW_LEFT | B_FOLLOW_TOP,
			    0, false, true),
      font_style_scroller (NULL, &font_style_pane,
			   B_FOLLOW_ALL_SIDES,
			   B_SUPPORTS_LAYOUT, false, true),
      style_view (&font_style_scroller, &antialias_checkbox,
		  &preview_checkbox),
      all_families (20, true),
      all_styles (20, true),
      cancel_button ("Cancel", "Cancel",
		     new BMessage (B_CANCEL)),
      ok_button ("OK", "OK", new BMessage (B_OK)),
      size_entry (NULL, "Size:", NULL,
		  new BMessage (UPDATE_PREVIEW_DIALOG)),
      allow_monospace_only (monospace_only),
      pending_selection_idx (initial_style_idx),
      preview (NULL)
  {
    BStringItem *family_item;
    int i, n_families;
    font_family name;
    uint32 flags, c;
    BMessage *selection;
    BTextView *size_text;
    char format_buffer[4];

    AddChild (&basic_view);

    basic_view.AddChild (&split_view);
    basic_view.AddChild (&cancel_button);
    basic_view.AddChild (&ok_button);
    basic_view.AddChild (&size_entry);
    split_view.AddChild (&font_family_scroller, 0.7);
    split_view.AddChild (&style_view, 0.3);
    style_view.AddChild (&font_style_scroller);
    style_view.AddChild (&antialias_checkbox);
    style_view.AddChild (&preview_checkbox);

    basic_view.SetViewUIColor (B_PANEL_BACKGROUND_COLOR);
    style_view.SetViewUIColor (B_PANEL_BACKGROUND_COLOR);

    FrameResized (801, 801);
    UpdateForSelectedStyle ();

    selection = new BMessage (FONT_FAMILY_SELECTED);
    font_family_pane.SetSelectionMessage (selection);
    selection = new BMessage (FONT_STYLE_SELECTED);
    font_style_pane.SetSelectionMessage (selection);
    selection = new BMessage (B_OK);
    font_style_pane.SetInvocationMessage (selection);
    selection = new BMessage (UPDATE_PREVIEW_DIALOG);
    size_entry.SetModificationMessage (selection);

    comm_port = create_port (1, "font dialog port");

    n_families = count_font_families ();

    for (i = 0; i < n_families; ++i)
      {
	if (get_font_family (i, &name, &flags) == B_OK)
	  {
	    family_item = new BStringItem (name);

	    all_families.AddItem (family_item);
	    font_family_pane.AddItem (family_item);

	    family_item->SetEnabled (!allow_monospace_only
				     || flags & B_IS_FIXED);
	  }
	else
	  {
	    family_item = new BStringItem ("<error>");

	    all_families.AddItem (family_item);
	    font_family_pane.AddItem (family_item);
	  }
      }

    if (initial_family_idx >= 0)
      {
	font_family_pane.Select (initial_family_idx);
	font_family_pane.ScrollToSelection ();
      }

    size_text = size_entry.TextView ();

    for (c = 0; c <= 47; ++c)
      size_text->DisallowChar (c);

    for (c = 58; c <= 127; ++c)
      size_text->DisallowChar (c);

    if (initial_size > 0 && initial_size < 1000)
      {
	sprintf (format_buffer, "%d", initial_size);
	size_entry.SetText (format_buffer);
      }

    if (!initial_antialias)
      antialias_checkbox.SetValue (B_CONTROL_ON);
  }

  void
  FrameResized (float new_width, float new_height)
  {
    BRect frame;
    float ok_height, ok_width;
    float cancel_height, cancel_width;
    float size_width, size_height;
    float bone;
    int max_height;

    ok_button.GetPreferredSize (&ok_width, &ok_height);
    cancel_button.GetPreferredSize (&cancel_width,
				    &cancel_height);
    size_entry.GetPreferredSize (&size_width, &size_height);

    max_height = std::max (std::max (ok_height, cancel_height),
			   size_height);

    SetSizeLimits (cancel_width + ok_width + size_width + 6,
		   65535, max_height + 64, 65535);
    frame = Frame ();

    basic_view.ResizeTo (BE_RECT_WIDTH (frame), BE_RECT_HEIGHT (frame));
    split_view.ResizeTo (BE_RECT_WIDTH (frame) - 1,
			 BE_RECT_HEIGHT (frame) - 4 - max_height);

    bone = BE_RECT_HEIGHT (frame) - 2 - max_height / 2;

    ok_button.MoveTo ((BE_RECT_WIDTH (frame)
		       - 4 - cancel_width - ok_width),
		      bone - ok_height / 2);
    cancel_button.MoveTo (BE_RECT_WIDTH (frame) - 2 - cancel_width,
			  bone - cancel_height / 2);
    size_entry.MoveTo (2, bone - size_height / 2);

    ok_button.ResizeTo (ok_width, ok_height);
    cancel_button.ResizeTo (cancel_width, cancel_height);
    size_entry.ResizeTo (std::max (size_width,
				   BE_RECT_WIDTH (frame) / 4),
			 size_height);
  }

  void
  WaitForChoice (struct font_selection_dialog_message *msg,
		 void (*process_pending_signals_function) (void),
		 bool (*should_quit_function) (void))
  {
    int32 reply_type;
    struct object_wait_info infos[2];
    ssize_t status;

    infos[0].object = port_application_to_emacs;
    infos[0].type = B_OBJECT_TYPE_PORT;
    infos[0].events = B_EVENT_READ;

    infos[1].object = comm_port;
    infos[1].type = B_OBJECT_TYPE_PORT;
    infos[1].events = B_EVENT_READ;

    while (true)
      {
	status = wait_for_objects (infos, 2);

	if (status < B_OK)
	  continue;

	if (infos[1].events & B_EVENT_READ)
	  {
	    if (read_port (comm_port, &reply_type,
			   msg, sizeof *msg) >= B_OK)
	      return;

	    goto cancel;
	  }

	if (infos[0].events & B_EVENT_READ)
	  process_pending_signals_function ();

	if (should_quit_function ())
	  goto cancel;

	infos[0].events = B_EVENT_READ;
	infos[1].events = B_EVENT_READ;
      }

  cancel:
    msg->cancel = true;
    return;
  }

  status_t
  InitCheck (void)
  {
    return comm_port >= B_OK ? B_OK : comm_port;
  }
};

class EmacsFilePanelCallbackLooper : public BLooper
{
  port_id comm_port;

  void
  MessageReceived (BMessage *msg)
  {
    const char *str_path, *name;
    char *file_name, *str_buf;
    BEntry entry;
    BPath path;
    entry_ref ref;
    int32 old_what;

    if (msg->what == FILE_PANEL_SELECTION
	|| ((msg->FindInt32 ("old_what", &old_what) == B_OK
	     && old_what == FILE_PANEL_SELECTION)))
      {
	file_name = NULL;

	if (msg->FindRef ("refs", &ref) == B_OK
	    && entry.SetTo (&ref, 0) == B_OK
	    && entry.GetPath (&path) == B_OK)
	  {
	    str_path = path.Path ();

	    if (str_path)
	      file_name = strdup (str_path);
	  }
	else if (msg->FindRef ("directory", &ref) == B_OK
		 && entry.SetTo (&ref, 0) == B_OK
		 && entry.GetPath (&path) == B_OK)
	  {
	    name = msg->GetString ("name");
	    str_path = path.Path ();

	    if (name)
	      {
		str_buf = (char *) alloca (std::strlen (str_path)
					   + std::strlen (name) + 2);
		snprintf (str_buf, std::strlen (str_path)
			  + std::strlen (name) + 2, "%s/%s",
			  str_path, name);
		file_name = strdup (str_buf);
	      }
	  }

	write_port (comm_port, 0, &file_name, sizeof file_name);
      }

    BLooper::MessageReceived (msg);
  }

public:
  EmacsFilePanelCallbackLooper (void) : BLooper ()
  {
    comm_port = create_port (1, "file panel port");
  }

  ~EmacsFilePanelCallbackLooper (void)
  {
    delete_port (comm_port);
  }

  char *
  ReadFileName (void (*process_pending_signals_function) (void))
  {
    object_wait_info infos[2];
    ssize_t status;
    int32 reply_type;
    char *file_name;

    file_name = NULL;

    infos[0].object = port_application_to_emacs;
    infos[0].type = B_OBJECT_TYPE_PORT;
    infos[0].events = B_EVENT_READ;

    infos[1].object = comm_port;
    infos[1].type = B_OBJECT_TYPE_PORT;
    infos[1].events = B_EVENT_READ;

    while (true)
      {
	status = wait_for_objects (infos, 2);

	if (status == B_INTERRUPTED || status == B_WOULD_BLOCK)
	  continue;

	if (infos[0].events & B_EVENT_READ)
	  process_pending_signals_function ();

	if (infos[1].events & B_EVENT_READ)
	  {
	    status = read_port (comm_port,
				&reply_type, &file_name,
				sizeof file_name);

	    if (status < B_OK)
	      file_name = NULL;

	    goto out;
	  }

	infos[0].events = B_EVENT_READ;
	infos[1].events = B_EVENT_READ;
      }

  out:
    return file_name;
  }

  status_t
  InitCheck (void)
  {
    return comm_port >= B_OK ? B_OK : comm_port;
  }
};

/* A view that is added as a child of a tooltip's text view, and
   prevents motion events from reaching it (thereby moving the
   tooltip).  */
class EmacsMotionSuppressionView : public BView
{
  void
  AttachedToWindow (void)
  {
    BView *text_view, *tooltip_view;

    /* We know that this view is a child of the text view, whose
       parent is the tooltip view, and that the tooltip view has
       already set its mouse event mask.  */

    text_view = Parent ();

    if (!text_view)
      return;

    tooltip_view = text_view->Parent ();

    if (!tooltip_view)
      return;

    tooltip_view->SetEventMask (B_KEYBOARD_EVENTS, 0);
  }

public:
  EmacsMotionSuppressionView (void) : BView (BRect (-1, -1, 1, 1),
					     NULL, 0, 0)
  {
    return;
  }
};

static int32
start_running_application (void *data)
{
  Emacs *app = (Emacs *) data;

  haiku_io_init_in_app_thread ();

  if (!app->Lock ())
    gui_abort ("Failed to lock application");

  app->Run ();
  app->Unlock ();
  return 0;
}

/* Take BITMAP, a reference to a BBitmap, and return a pointer to its
   data.  */
void *
BBitmap_data (void *bitmap)
{
  return ((BBitmap *) bitmap)->Bits ();
}

/* Convert bitmap if required, placing the new bitmap in NEW_BITMAP,
   and return non-null if bitmap was successfully converted.  Bitmaps
   should be freed with `BBitmap_free'.  */
int
BBitmap_convert (void *_bitmap, void **new_bitmap)
{
  BBitmap *bitmap = (BBitmap *) _bitmap;
  if (bitmap->ColorSpace () == B_RGBA32)
    return -1;
  BRect bounds = bitmap->Bounds ();
  BBitmap *bmp = new (std::nothrow) BBitmap (bounds, B_RGBA32);
  if (!bmp || bmp->InitCheck () != B_OK)
    {
      if (bmp)
	delete bmp;
      return 0;
    }
  if (bmp->ImportBits (bitmap) != B_OK)
    {
      delete bmp;
      return 0;
    }
  *(BBitmap **) new_bitmap = bmp;
  return 1;
}

void
BBitmap_free (void *bitmap)
{
  delete (BBitmap *) bitmap;
}

/* Create new bitmap in RGB32 format, or in GRAY1 if MONO_P is
   non-zero.  */
void *
BBitmap_new (int width, int height, int mono_p)
{
  BBitmap *bn = new (std::nothrow) BBitmap (BRect (0, 0, width - 1, height - 1),
					    mono_p ? B_GRAY1 : B_RGB32);

  return bn->InitCheck () == B_OK ? (void *) bn : (void *) (delete bn, NULL);
}

void
BBitmap_dimensions (void *bitmap, int *left, int *top,
		    int *right, int *bottom,
		    int32_t *bytes_per_row, int *mono_p)
{
  BRect rect = ((BBitmap *) bitmap)->Bounds ();
  *left = rect.left;
  *top = rect.top;
  *right = rect.right;
  *bottom = rect.bottom;

  *bytes_per_row = ((BBitmap *) bitmap)->BytesPerRow ();
  *mono_p = (((BBitmap *) bitmap)->ColorSpace () == B_GRAY1);
}

static void
wait_for_exit_of_app_thread (void)
{
  status_t ret;

  be_app->PostMessage (QUIT_APPLICATION);
  wait_for_thread (app_thread, &ret);
}

/* Set up an application and return it.  If starting the application
   thread fails, abort Emacs.  */
void *
BApplication_setup (void)
{
  thread_id id;
  Emacs *app;

  if (be_app)
    return be_app;

  app = new Emacs;
  app->Unlock ();

  if ((id = spawn_thread (start_running_application, "Emacs app thread",
			  B_DEFAULT_MEDIA_PRIORITY, app)) < 0)
    gui_abort ("spawn_thread failed");

  resume_thread (id);
  app_thread = id;

  atexit (wait_for_exit_of_app_thread);
  return app;
}

/* Set up and return a window with its view put in VIEW.  */
void *
BWindow_new (void **view)
{
  BWindow *window;
  BView *vw;

  window = new (std::nothrow) EmacsWindow;
  if (!window)
    {
      *view = NULL;
      return window;
    }

  vw = new (std::nothrow) EmacsView;
  if (!vw)
    {
      *view = NULL;
      window->LockLooper ();
      window->Quit ();
      return NULL;
    }

  /* Windows are created locked by the current thread, but calling
     Show for the first time causes them to be unlocked.  To avoid a
     deadlock when a frame is created invisible in one thread, and
     another thread later tries to lock it, the window is unlocked
     here, and EmacsShow will lock it manually if it's being shown for
     the first time.  */
  window->UnlockLooper ();
  window->AddChild (vw);
  *view = vw;
  return window;
}

void
BWindow_quit (void *window)
{
  BWindow *w = (BWindow *) window;

  w->LockLooper ();
  w->Quit ();
}

/* Set WINDOW's offset to X, Y.  */
void
BWindow_set_offset (void *window, int x, int y)
{
  BWindow *wn = (BWindow *) window;
  EmacsWindow *w = dynamic_cast<EmacsWindow *> (wn);
  if (w)
    {
      if (!w->LockLooper ())
	gui_abort ("Failed to lock window looper setting offset");
      w->EmacsMoveTo (x, y);
      w->UnlockLooper ();
    }
  else
    wn->MoveTo (x, y);
}

void
BWindow_dimensions (void *window, int *width, int *height)
{
  BWindow *w = (BWindow *) window;
  BRect frame = w->Frame ();

  *width = BE_RECT_WIDTH (frame);
  *height = BE_RECT_HEIGHT (frame);
}

/* Iconify WINDOW.  */
void
BWindow_iconify (void *window)
{
  if (((BWindow *) window)->IsHidden ())
    BWindow_set_visible (window, true);
  ((BWindow *) window)->Minimize (true);
}

/* Show or hide WINDOW.  */
void
BWindow_set_visible (void *window, int visible_p)
{
  EmacsWindow *win = (EmacsWindow *) window;
  if (visible_p)
    {
      if (win->IsMinimized ())
	win->Minimize (false);
      win->EmacsShow ();
    }
  else if (!win->IsHidden ())
    {
      if (win->IsMinimized ())
	win->Minimize (false);
      win->EmacsHide ();
    }
}

/* Change the title of WINDOW to the multibyte string TITLE.  */
void
BWindow_retitle (void *window, const char *title)
{
  ((BWindow *) window)->SetTitle (title);
}

/* Resize WINDOW to WIDTH by HEIGHT.  */
void
BWindow_resize (void *window, int width, int height)
{
  ((BWindow *) window)->ResizeTo (width - 1, height - 1);
}

/* Activate WINDOW, making it the subject of keyboard focus and
   bringing it to the front of the screen.  */
void
BWindow_activate (void *window)
{
  ((BWindow *) window)->Activate ();
}

/* Return the pixel dimensions of the main screen in WIDTH and
   HEIGHT.  */
void
be_get_screen_dimensions (int *width, int *height)
{
  BScreen screen;
  BRect frame;

  if (!screen.IsValid ())
    gui_abort ("Invalid screen");

  frame = screen.Frame ();

  *width = BE_RECT_WIDTH (frame);
  *height = BE_RECT_HEIGHT (frame);
}

/* Resize VIEW to WIDTH, HEIGHT.  */
void
BView_resize_to (void *view, int width, int height)
{
  EmacsView *vw = (EmacsView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view for resize");
  vw->ResizeTo (width, height);
  vw->AfterResize ();
  vw->UnlockLooper ();
}

void
be_delete_cursor (void *cursor)
{
  if (cursor)
    delete (BCursor *) cursor;
}

void *
be_create_cursor_from_id (int id)
{
  return new BCursor ((enum BCursorID) id);
}

void
BView_set_view_cursor (void *view, void *cursor)
{
  BView *v = (BView *) view;

  if (!v->LockLooper ())
    gui_abort ("Failed to lock view setting cursor");
  v->SetViewCursor ((BCursor *) cursor);
  v->UnlockLooper ();
}

void
BWindow_Flush (void *window)
{
  ((BWindow *) window)->Flush ();
}

/* Make a scrollbar, attach it to VIEW's window, and return it.  */
void *
be_make_scroll_bar_for_view (void *view, int horizontal_p,
			     int x, int y, int x1, int y1)
{
  EmacsScrollBar *scroll_bar;
  BView *vw = (BView *) view;

  if (!vw->LockLooper ())
    gui_abort ("Failed to lock scrollbar owner");

  scroll_bar = new EmacsScrollBar (x, y, x1, y1, horizontal_p,
				   (EmacsView *) vw);

  vw->AddChild (scroll_bar);
  vw->UnlockLooper ();

  return scroll_bar;
}

void
BScrollBar_delete (void *sb)
{
  BView *view = (BView *) sb;
  BView *pr = view->Parent ();

  if (!pr->LockLooper ())
    gui_abort ("Failed to lock scrollbar parent");
  pr->RemoveChild (view);
  pr->UnlockLooper ();

  delete (EmacsScrollBar *) sb;
}

void
BView_move_frame (void *view, int x, int y, int x1, int y1)
{
  BView *vw = (BView *) view;

  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view moving frame");
  vw->MoveTo (x, y);
  vw->ResizeTo (x1 - x, y1 - y);
  vw->UnlockLooper ();
}

/* DRAGGING can either be 0 (which means to update everything), 1
   (which means to update nothing), or -1 (which means to update only
   the thumb size and range).  */

void
BView_scroll_bar_update (void *sb, int portion, int whole, int position,
			 int dragging, bool can_overscroll)
{
  BScrollBar *bar = (BScrollBar *) sb;
  BMessage msg = BMessage (SCROLL_BAR_UPDATE);
  BMessenger mr = BMessenger (bar);
  msg.AddInt32 ("emacs:range", whole);
  msg.AddInt32 ("emacs:units", position);
  msg.AddInt32 ("emacs:portion", portion);
  msg.AddInt32 ("emacs:dragging", dragging);
  msg.AddBool ("emacs:overscroll", can_overscroll);

  mr.SendMessage (&msg);
}

/* Return the default scrollbar size.  */
int
BScrollBar_default_size (int horizontal_p)
{
  return be_control_look->GetScrollBarWidth (horizontal_p
					     ? B_HORIZONTAL
					     : B_VERTICAL);
}

/* Invalidate VIEW, causing it to be drawn again.  */
void
BView_invalidate (void *view)
{
  BView *vw = (BView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Couldn't lock view while invalidating it");
  vw->Invalidate ();
  vw->UnlockLooper ();
}

/* Lock VIEW in preparation for drawing operations.  This should be
   called before any attempt to draw onto VIEW or to lock it for Cairo
   drawing.  `BView_draw_unlock' should be called afterwards.

   If any drawing is going to take place, INVALID_REGION should be
   true, and X, Y, WIDTH, HEIGHT should specify a rectangle in which
   the drawing will take place.  */
void
BView_draw_lock (void *view, bool invalidate_region,
		 int x, int y, int width, int height)
{
  EmacsView *vw = (EmacsView *) view;
  if (vw->looper_locked_count)
    {
      vw->looper_locked_count++;

      if (invalidate_region && vw->offscreen_draw_view)
	vw->invalid_region.Include (BRect (x, y, x + width - 1,
					   y + height - 1));
      return;
    }
  BView *v = (BView *) find_appropriate_view_for_draw (vw);
  if (v != vw)
    {
      if (!vw->offscreen_draw_bitmap_1->Lock ())
	gui_abort ("Failed to lock offscreen bitmap while acquiring draw lock");
    }
  else if (!v->LockLooper ())
    gui_abort ("Failed to lock draw view while acquiring draw lock");

  if (v != vw && !vw->LockLooper ())
    gui_abort ("Failed to lock view while acquiring draw lock");

  if (invalidate_region && vw->offscreen_draw_view)
    vw->invalid_region.Include (BRect (x, y, x + width - 1,
				       y + height - 1));
  vw->looper_locked_count++;
}

void
BView_invalidate_region (void *view, int x, int y, int width, int height)
{
  EmacsView *vw = (EmacsView *) view;

  if (vw->offscreen_draw_view)
    vw->invalid_region.Include (BRect (x, y, x + width - 1,
				       y + height - 1));
}

void
BView_draw_unlock (void *view)
{
  EmacsView *vw = (EmacsView *) view;
  if (--vw->looper_locked_count)
    return;

  BView *v = (BView *) find_appropriate_view_for_draw (view);
  if (v == vw)
    vw->UnlockLooper ();
  else
    {
      vw->offscreen_draw_bitmap_1->Unlock ();
      vw->UnlockLooper ();
    }
}

void
BWindow_center_on_screen (void *window)
{
  BWindow *w = (BWindow *) window;
  w->CenterOnScreen ();
}

/* Import fringe bitmap (short array, low bit rightmost) BITS into
   BITMAP using the B_GRAY1 colorspace.  */
void
BBitmap_import_fringe_bitmap (void *bitmap, unsigned short *bits, int wd, int h)
{
  BBitmap *bmp = (BBitmap *) bitmap;
  unsigned char *data = (unsigned char *) bmp->Bits ();
  int i;

  for (i = 0; i < h; i++)
    {
      if (wd <= 8)
	data[0] = bits[i] & 0xff;
      else
	{
	  data[1] = bits[i] & 0xff;
	  data[0] = bits[i] >> 8;
	}

      data += bmp->BytesPerRow ();
    }
}

/* Make a scrollbar at X, Y known to the view VIEW.  */
void
BView_publish_scroll_bar (void *view, int x, int y, int width, int height)
{
  EmacsView *vw = (EmacsView *) view;
  if (vw->LockLooper ())
    {
      vw->sb_region.Include (BRect (x, y, x - 1 + width,
				    y - 1 + height));
      vw->UnlockLooper ();
    }
}

void
BView_forget_scroll_bar (void *view, int x, int y, int width, int height)
{
  EmacsView *vw = (EmacsView *) view;
  if (vw->LockLooper ())
    {
      vw->sb_region.Exclude (BRect (x, y, x - 1 + width,
				    y - 1 + height));
      vw->UnlockLooper ();
    }
}

bool
BView_inside_scroll_bar (void *view, int x, int y)
{
  EmacsView *vw = (EmacsView *) view;
  bool val;

  if (vw->LockLooper ())
    {
      val = vw->sb_region.Contains (BPoint (x, y));
      vw->UnlockLooper ();
    }
  else
    val = false;

  return val;
}

void
BView_get_mouse (void *view, int *x, int *y)
{
  BPoint l;
  BView *vw = (BView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view in BView_get_mouse");
  vw->GetMouse (&l, NULL, 1);
  vw->UnlockLooper ();

  *x = std::lrint (l.x);
  *y = std::lrint (l.y);
}

/* Perform an in-place conversion of X and Y from VIEW's coordinate
   system to its screen's coordinate system.  */
void
BView_convert_to_screen (void *view, int *x, int *y)
{
  BPoint l = BPoint (*x, *y);
  BView *vw = (BView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view in convert_to_screen");
  vw->ConvertToScreen (&l);
  vw->UnlockLooper ();

  *x = std::lrint (l.x);
  *y = std::lrint (l.y);
}

void
BView_convert_from_screen (void *view, int *x, int *y)
{
  BPoint l = BPoint (*x, *y);
  BView *vw = (BView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view in convert_from_screen");
  vw->ConvertFromScreen (&l);
  vw->UnlockLooper ();

  *x = std::lrint (l.x);
  *y = std::lrint (l.y);
}

/* Decorate or undecorate WINDOW depending on DECORATE_P.  */
void
BWindow_change_decoration (void *window, int decorate_p)
{
  EmacsWindow *w = (EmacsWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while changing its decorations");

  if (!w->override_redirect_p)
    {
      if (decorate_p)
	w->SetLook (B_TITLED_WINDOW_LOOK);
      else
	w->SetLook (B_NO_BORDER_WINDOW_LOOK);
    }
  else
    {
      if (decorate_p)
	w->pre_override_redirect_look = B_TITLED_WINDOW_LOOK;
      else
	w->pre_override_redirect_look = B_NO_BORDER_WINDOW_LOOK;
    }
  w->UnlockLooper ();
}

/* Decorate WINDOW appropriately for use as a tooltip.  */
void
BWindow_set_tooltip_decoration (void *window)
{
  EmacsWindow *w = (EmacsWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while setting ttip decoration");
  w->tooltip_p = true;
  w->RecomputeFeel ();
  w->SetLook (B_BORDERED_WINDOW_LOOK);
  w->SetFlags (B_NOT_ZOOMABLE
	       | B_NOT_MINIMIZABLE
	       | B_AVOID_FRONT
	       | B_AVOID_FOCUS);
  w->UnlockLooper ();
}

/* Set B_AVOID_FOCUS on WINDOW if AVOID_FOCUS_P is non-nil, or clear
   it otherwise.  */
void
BWindow_set_avoid_focus (void *window, int avoid_focus_p)
{
  BWindow *w = (BWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while setting avoid focus");

  if (!avoid_focus_p)
    w->SetFlags (w->Flags () & ~B_AVOID_FOCUS);
  else
    w->SetFlags (w->Flags () | B_AVOID_FOCUS);
  w->UnlockLooper ();
}

void
BView_emacs_delete (void *view)
{
  EmacsView *vw = (EmacsView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view while deleting it");
  vw->RemoveSelf ();
  delete vw;
}

/* Create a popup menu.  */
void *
BPopUpMenu_new (const char *name)
{
  BPopUpMenu *menu = new BPopUpMenu (name);

  menu->SetRadioMode (0);
  return menu;
}

/* Add a title item to MENU.  These items cannot be highlighted or
   triggered, and their labels will display as bold text.  */
void
BMenu_add_title (void *menu, const char *text)
{
  BMenu *be_menu = (BMenu *) menu;
  EmacsTitleMenuItem *it;

  it = new EmacsTitleMenuItem (text);
  be_menu->AddItem (it);
}

/* Add an item to the menu MENU.  */
void
BMenu_add_item (void *menu, const char *label, void *ptr, bool enabled_p,
		bool marked_p, bool mbar_p, void *mbw_ptr, const char *key,
		const char *help)
{
  BMenu *m = (BMenu *) menu;
  BMessage *msg;
  if (ptr)
    msg = new BMessage ();
  EmacsMenuItem *it = new EmacsMenuItem (key, label, help, ptr ? msg : NULL);
  it->SetTarget (m->Window ());
  it->SetEnabled (enabled_p);
  it->SetMarked (marked_p);
  if (mbar_p)
    {
      it->menu_bar_id = (intptr_t) ptr;
      it->wind_ptr = mbw_ptr;
    }
  it->menu_ptr = ptr;
  if (ptr)
    msg->AddPointer ("menuptr", ptr);
  m->AddItem (it);
}

/* Add a separator to the menu MENU.  */
void
BMenu_add_separator (void *menu)
{
  BMenu *m = (BMenu *) menu;

  m->AddSeparatorItem ();
}

/* Create a submenu and attach it to MENU.  */
void *
BMenu_new_submenu (void *menu, const char *label, bool enabled_p)
{
  BMenu *m = (BMenu *) menu;
  BMenu *mn = new BMenu (label, B_ITEMS_IN_COLUMN);
  mn->SetRadioMode (0);
  BMenuItem *i = new BMenuItem (mn);
  i->SetEnabled (enabled_p);
  m->AddItem (i);
  return mn;
}

/* Create a submenu that notifies Emacs upon opening.  */
void *
BMenu_new_menu_bar_submenu (void *menu, const char *label)
{
  BMenu *m = (BMenu *) menu;
  BMenu *mn = new BMenu (label, B_ITEMS_IN_COLUMN);
  mn->SetRadioMode (0);
  BMenuItem *i = new BMenuItem (mn);
  i->SetEnabled (1);
  m->AddItem (i);
  return mn;
}

/* Run MENU, waiting for it to close, and return a pointer to the
   data of the selected item (if one exists), or NULL.  X, Y should
   be in the screen coordinate system.  */
void *
BMenu_run (void *menu, int x, int y,
	   void (*run_help_callback) (void *, void *),
	   void (*block_input_function) (void),
	   void (*unblock_input_function) (void),
	   struct timespec (*process_pending_signals_function) (void),
	   void *run_help_callback_data)
{
  BPopUpMenu *mn = (BPopUpMenu *) menu;
  enum haiku_event_type type;
  void *buf;
  void *ptr = NULL;
  struct be_popup_menu_data data;
  struct object_wait_info infos[3];
  struct haiku_menu_bar_help_event *event;
  BMessage *msg;
  ssize_t stat;
  struct timespec next_time;
  bigtime_t timeout;

  block_input_function ();
  port_popup_menu_to_emacs = create_port (1800, "popup menu port");
  data.x = x;
  data.y = y;
  data.menu = mn;
  unblock_input_function ();

  if (port_popup_menu_to_emacs < B_OK)
    return NULL;

  block_input_function ();
  mn->SetRadioMode (0);
  buf = alloca (200);

  infos[0].object = port_popup_menu_to_emacs;
  infos[0].type = B_OBJECT_TYPE_PORT;
  infos[0].events = B_EVENT_READ;

  infos[1].object = spawn_thread (be_popup_menu_thread_entry,
				  "Menu tracker", B_DEFAULT_MEDIA_PRIORITY,
				  (void *) &data);
  infos[1].type = B_OBJECT_TYPE_THREAD;
  infos[1].events = B_EVENT_INVALID;

  infos[2].object = port_application_to_emacs;
  infos[2].type = B_OBJECT_TYPE_PORT;
  infos[2].events = B_EVENT_READ;
  unblock_input_function ();

  if (infos[1].object < B_OK)
    {
      block_input_function ();
      delete_port (port_popup_menu_to_emacs);
      unblock_input_function ();
      return NULL;
    }

  block_input_function ();
  resume_thread (infos[1].object);
  unblock_input_function ();

  while (true)
    {
      next_time = process_pending_signals_function ();

      if (next_time.tv_nsec < 0)
	timeout = 10000000000;
      else
	timeout = (next_time.tv_sec * 1000000
		   + next_time.tv_nsec / 1000);

      if ((stat = wait_for_objects_etc ((object_wait_info *) &infos, 3,
					B_RELATIVE_TIMEOUT, timeout)) < B_OK)
	{
	  if (stat == B_INTERRUPTED || stat == B_TIMED_OUT
	      || stat == B_WOULD_BLOCK)
	    continue;
	  else
	    gui_abort ("Failed to wait for popup");
	}

      if (infos[0].events & B_EVENT_READ)
	{
	  while (!haiku_read_with_timeout (&type, buf, 200, 0, true))
	    {
	      switch (type)
		{
		case MENU_BAR_HELP_EVENT:
		  event = (struct haiku_menu_bar_help_event *) buf;
		  run_help_callback (event->highlight_p
				     ? event->data
				     : NULL, run_help_callback_data);
		  break;
		default:
		  gui_abort ("Unknown popup menu event");
		}
	    }
	}

      if (infos[1].events & B_EVENT_INVALID)
	{
	  block_input_function ();
	  msg = (BMessage *) popup_track_message;
	  if (popup_track_message)
	    ptr = (void *) msg->GetPointer ("menuptr");

	  delete_port (port_popup_menu_to_emacs);
	  unblock_input_function ();
	  return ptr;
	}

      infos[0].events = B_EVENT_READ;
      infos[1].events = B_EVENT_INVALID;
      infos[2].events = B_EVENT_READ;
    }
}

/* Delete the entire menu hierarchy of MENU, and then delete MENU
   itself.  */
void
BPopUpMenu_delete (void *menu)
{
  delete (BPopUpMenu *) menu;
}

/* Create a menubar, attach it to VIEW, and return it.  */
void *
BMenuBar_new (void *view)
{
  BView *vw = (BView *) view;
  EmacsMenuBar *bar = new EmacsMenuBar ();

  if (!vw->LockLooper ())
    gui_abort ("Failed to lock menu bar parent");
  vw->AddChild ((BView *) bar);
  vw->UnlockLooper ();

  return bar;
}

/* Delete MENUBAR along with all subitems. */
void
BMenuBar_delete (void *menubar)
{
  BView *vw = (BView *) menubar;
  BView *p = vw->Parent ();
  EmacsWindow *window = (EmacsWindow *) p->Window ();

  if (!p->LockLooper ())
    gui_abort ("Failed to lock menu bar parent while removing menubar");
  window->SetKeyMenuBar (NULL);
  /* MenusEnded isn't called if the menu bar is destroyed
     before it closes.  */
  window->menu_bar_active_p = false;
  vw->RemoveSelf ();
  p->UnlockLooper ();
  delete vw;
}

/* Delete all items from MENU.  */
void
BMenu_delete_all (void *menu)
{
  BMenu *mn = (BMenu *) menu;
  mn->RemoveItems (0, mn->CountItems (), true);
}

/* Delete COUNT items from MENU starting from START.  */
void
BMenu_delete_from (void *menu, int start, int count)
{
  BMenu *mn = (BMenu *) menu;
  mn->RemoveItems (start, count, true);
}

/* Count items in menu MENU.  */
int
BMenu_count_items (void *menu)
{
  return ((BMenu *) menu)->CountItems ();
}

/* Find the item in MENU at IDX.  */
void *
BMenu_item_at (void *menu, int idx)
{
  return ((BMenu *) menu)->ItemAt (idx);
}

/* Set ITEM's label to LABEL.  */
void
BMenu_item_set_label (void *item, const char *label)
{
  ((BMenuItem *) item)->SetLabel (label);
}

/* Get ITEM's menu.  */
void *
BMenu_item_get_menu (void *item)
{
  return ((BMenuItem *) item)->Submenu ();
}

/* Emit a beep noise.  */
void
haiku_ring_bell (void)
{
  beep ();
}

/* Create a BAlert with TEXT.  */
void *
BAlert_new (const char *text, enum haiku_alert_type type)
{
  return new BAlert (NULL, text, NULL, NULL, NULL, B_WIDTH_AS_USUAL,
		     (enum alert_type) type);
}

/* Add a button to ALERT and return the button.  */
void *
BAlert_add_button (void *alert, const char *text)
{
  BAlert *al = (BAlert *) alert;
  al->AddButton (text);
  return al->ButtonAt (al->CountButtons () - 1);
}

/* Make sure the leftmost button is grouped to the left hand side of
   the alert.  */
void
BAlert_set_offset_spacing (void *alert)
{
  BAlert *al = (BAlert *) alert;

  al->SetButtonSpacing (B_OFFSET_SPACING);
}

static int32
be_alert_thread_entry (void *thread_data)
{
  BAlert *alert = (BAlert *) thread_data;
  int32 value;

  if (alert->LockLooper ())
    value = alert->Go ();
  else
    value = -1;

  alert_popup_value = value;
  return 0;
}

/* Run ALERT, returning the number of the button that was selected,
   or -1 if no button was selected before the alert was closed.  */
int32
BAlert_go (void *alert,
	   void (*block_input_function) (void),
	   void (*unblock_input_function) (void),
	   void (*process_pending_signals_function) (void))
{
  struct object_wait_info infos[2];
  ssize_t stat;
  BAlert *alert_object = (BAlert *) alert;

  infos[0].object = port_application_to_emacs;
  infos[0].type = B_OBJECT_TYPE_PORT;
  infos[0].events = B_EVENT_READ;

  block_input_function ();
  /* Alerts are created locked, just like other windows.  */
  alert_object->UnlockLooper ();
  infos[1].object = spawn_thread (be_alert_thread_entry,
				  "Popup tracker",
				  B_DEFAULT_MEDIA_PRIORITY,
				  alert);
  infos[1].type = B_OBJECT_TYPE_THREAD;
  infos[1].events = B_EVENT_INVALID;
  unblock_input_function ();

  if (infos[1].object < B_OK)
    return -1;

  block_input_function ();
  resume_thread (infos[1].object);
  unblock_input_function ();

  while (true)
    {
      stat = wait_for_objects ((object_wait_info *) &infos, 2);

      if (stat == B_INTERRUPTED)
	continue;
      else if (stat < B_OK)
	gui_abort ("Failed to wait for popup dialog");

      if (infos[1].events & B_EVENT_INVALID)
	return alert_popup_value;

      if (infos[0].events & B_EVENT_READ)
	process_pending_signals_function ();

      infos[0].events = B_EVENT_READ;
      infos[1].events = B_EVENT_INVALID;
    }
}

/* Enable or disable BUTTON depending on ENABLED_P.  */
void
BButton_set_enabled (void *button, int enabled_p)
{
  ((BButton *) button)->SetEnabled (enabled_p);
}

/* Set VIEW's tooltip to TOOLTIP.  */
void
BView_set_tooltip (void *view, const char *tooltip)
{
  ((BView *) view)->SetToolTip (tooltip);
}

/* Set VIEW's tooltip to a sticky tooltip at X by Y.  */
void
be_show_sticky_tooltip (void *view, const char *tooltip_text,
			int x, int y)
{
  BToolTip *tooltip;
  BView *vw, *tooltip_view;
  BPoint point;

  vw = (BView *) view;

  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view while showing sticky tooltip");

  vw->SetToolTip ((const char *) NULL);

  /* If the tooltip text is empty, then a tooltip object won't be
     created by SetToolTip.  */
  if (tooltip_text[0] == '\0')
    tooltip_text = " ";

  vw->SetToolTip (tooltip_text);

  tooltip = vw->ToolTip ();

  vw->GetMouse (&point, NULL, 1);
  point.x -= x;
  point.y -= y;

  point.x = -point.x;
  point.y = -point.y;

  /* We don't have to make the tooltip sticky since not receiving
     mouse movement is enough to prevent it from being hidden.  */
  tooltip->SetMouseRelativeLocation (point);

  /* Prevent the tooltip from moving in response to mouse
     movement.  */
  tooltip_view = tooltip->View ();

  if (tooltip_view)
    tooltip_view->AddChild (new EmacsMotionSuppressionView);

  vw->ShowToolTip (tooltip);
  vw->UnlockLooper ();
}

/* Delete ALERT.  */
void
BAlert_delete (void *alert)
{
  delete (BAlert *) alert;
}

/* Place the resolution of the monitor in DPI in X_OUT and Y_OUT.  */
void
be_get_display_resolution (double *x_out, double *y_out)
{
  BScreen s (B_MAIN_SCREEN_ID);
  monitor_info i;
  double x_inches, y_inches;
  BRect frame;

  if (!s.IsValid ())
    gui_abort ("Invalid screen for resolution checks");

  if (s.GetMonitorInfo (&i) == B_OK)
    {
      frame = s.Frame ();

      x_inches = (double) i.width * 25.4;
      y_inches = (double) i.height * 25.4;

      *x_out = (double) BE_RECT_WIDTH (frame) / x_inches;
      *y_out = (double) BE_RECT_HEIGHT (frame) / y_inches;
      return;
    }

  *x_out = 72.0;
  *y_out = 72.0;
}

/* Add WINDOW to OTHER_WINDOW's subset and parent it to
   OTHER_WINDOW.  */
void
EmacsWindow_parent_to (void *window, void *other_window)
{
  EmacsWindow *w = (EmacsWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while parenting");
  w->ParentTo ((EmacsWindow *) other_window);
  w->UnlockLooper ();
}

void
EmacsWindow_unparent (void *window)
{
  EmacsWindow *w = (EmacsWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while unparenting");
  w->UnparentAndUnlink ();
  w->UnlockLooper ();
}

/* Place text describing the current version of Haiku in VERSION,
   which should be a buffer LEN bytes wide.  */
void
be_get_version_string (char *version, int len)
{
  std::strncpy (version, "Unknown Haiku release", len - 1);
  version[len - 1] = '\0';

  BPath path;
  if (find_directory (B_BEOS_LIB_DIRECTORY, &path) == B_OK)
    {
      path.Append ("libbe.so");

      BAppFileInfo appFileInfo;
      version_info versionInfo;
      BFile file;
      if (file.SetTo (path.Path (), B_READ_ONLY) == B_OK
          && appFileInfo.SetTo (&file) == B_OK
          && appFileInfo.GetVersionInfo (&versionInfo,
                                         B_APP_VERSION_KIND) == B_OK
          && versionInfo.short_info[0] != '\0')
	{
	  std::strncpy (version, versionInfo.short_info, len - 1);
	  version[len - 1] = '\0';
	}
    }
}

/* Return the amount of color planes in the current display.  */
int
be_get_display_planes (void)
{
  color_space space = dpy_color_space;
  BScreen screen;

  if (space == B_NO_COLOR_SPACE)
    {
      if (!screen.IsValid ())
	gui_abort ("Invalid screen");

      space = dpy_color_space = screen.ColorSpace ();
    }

  switch (space)
    {
    case B_RGB32:
    case B_RGB24:
      return 24;
    case B_RGB16:
      return 16;
    case B_RGB15:
      return 15;
    case B_CMAP8:
    case B_GRAY8:
      return 8;
    case B_GRAY1:
      return 1;

    default:
      gui_abort ("Bad colorspace for screen");
    }

  /* https://www.haiku-os.org/docs/api/classBScreen.html
     says a valid screen can't be anything else.  */
  return -1;
}

/* Return the amount of colors the display can handle.  */
int
be_get_display_color_cells (void)
{
  BScreen screen;
  color_space space = dpy_color_space;

  if (space == B_NO_COLOR_SPACE)
    {
      if (!screen.IsValid ())
	gui_abort ("Invalid screen");

      space = dpy_color_space = screen.ColorSpace ();
    }

  switch (space)
    {
    case B_RGB32:
    case B_RGB24:
      return 16777216;
    case B_RGB16:
      return 65536;
    case B_RGB15:
      return 32768;
    case B_CMAP8:
    case B_GRAY8:
      return 256;
    case B_GRAY1:
      return 2;

    default:
      gui_abort ("Bad colorspace for screen");
    }

  return -1;
}

/* Return whether or not the current display is only capable of
   producing grayscale colors.  */
bool
be_is_display_grayscale (void)
{
  BScreen screen;
  color_space space = dpy_color_space;

  if (space == B_NO_COLOR_SPACE)
    {
      if (!screen.IsValid ())
	gui_abort ("Invalid screen");

      space = dpy_color_space = screen.ColorSpace ();
    }

  return space == B_GRAY8 || space == B_GRAY1;
}

/* Warp the pointer to X by Y.  */
void
be_warp_pointer (int x, int y)
{
  /* We're not supposed to use the following function without a
     BWindowScreen object, but in Haiku nothing actually prevents us
     from doing so.  */

  set_mouse_position (x, y);
}

/* Update the position of CHILD in WINDOW without actually moving
   it.  */
void
EmacsWindow_move_weak_child (void *window, void *child, int xoff, int yoff)
{
  EmacsWindow *w = (EmacsWindow *) window;
  EmacsWindow *c = (EmacsWindow *) child;

  if (!w->LockLooper ())
    gui_abort ("Couldn't lock window for weak move");
  w->MoveChild (c, xoff, yoff, 1);
  w->UnlockLooper ();
}

/* Find an appropriate view to draw onto.  If VW is double-buffered,
   this will be the view used for double buffering instead of VW
   itself.  */
void *
find_appropriate_view_for_draw (void *vw)
{
  BView *v = (BView *) vw;
  EmacsView *ev = dynamic_cast<EmacsView *>(v);
  if (!ev)
    return v;

  return ev->offscreen_draw_view ? ev->offscreen_draw_view : vw;
}

/* Set up double buffering for VW.  */
void
EmacsView_set_up_double_buffering (void *vw)
{
  EmacsView *view = (EmacsView *) vw;
  if (!view->LockLooper ())
    gui_abort ("Couldn't lock view while setting up double buffering");
  if (view->offscreen_draw_view)
    {
      view->UnlockLooper ();
      return;
    }
  view->SetUpDoubleBuffering ();
  view->UnlockLooper ();
}

/* Flip and invalidate the view VW.  */
void
EmacsView_flip_and_blit (void *vw)
{
  EmacsView *view = (EmacsView *) vw;
  if (!view->offscreen_draw_view)
    return;
  if (!view->LockLooper ())
    gui_abort ("Couldn't lock view in flip_and_blit");
  view->FlipBuffers ();
  view->UnlockLooper ();
}

/* Disable double buffering for VW.  */
void
EmacsView_disable_double_buffering (void *vw)
{
  EmacsView *view = (EmacsView *) vw;
  if (!view->LockLooper ())
    gui_abort ("Couldn't lock view tearing down double buffering");
  view->TearDownDoubleBuffering ();
  view->UnlockLooper ();
}

/* Return non-0 if VW is double-buffered.  */
int
EmacsView_double_buffered_p (void *vw)
{
  EmacsView *view = (EmacsView *) vw;
  if (!view->LockLooper ())
    gui_abort ("Couldn't lock view testing double buffering status");
  int db_p = !!view->offscreen_draw_view;
  view->UnlockLooper ();
  return db_p;
}

/* Popup a file dialog.  */
char *
be_popup_file_dialog (int open_p, const char *default_dir, int must_match_p,
		      int dir_only_p, void *window, const char *save_text,
		      const char *prompt,
		      void (*process_pending_signals_function) (void))
{
  BWindow *panel_window;
  BEntry path;
  BMessage msg (FILE_PANEL_SELECTION);
  BFilePanel panel (open_p ? B_OPEN_PANEL : B_SAVE_PANEL,
		    NULL, NULL, (dir_only_p
				 ? B_DIRECTORY_NODE
				 : B_FILE_NODE | B_DIRECTORY_NODE));
  char *file_name;
  EmacsFilePanelCallbackLooper *looper;

  looper = new EmacsFilePanelCallbackLooper;

  if (looper->InitCheck () < B_OK)
    {
      delete looper;
      return NULL;
    }

  if (default_dir)
    {
      if (path.SetTo (default_dir, 0) != B_OK)
	default_dir = NULL;
    }

  panel_window = panel.Window ();

  if (default_dir)
    panel.SetPanelDirectory (&path);

  if (save_text)
    panel.SetSaveText (save_text);

  panel_window->SetTitle (prompt);
  panel_window->SetFeel (B_MODAL_APP_WINDOW_FEEL);

  panel.SetHideWhenDone (false);
  panel.SetTarget (BMessenger (looper));
  panel.SetMessage (&msg);
  panel.Show ();

  looper->Run ();
  file_name = looper->ReadFileName (process_pending_signals_function);

  if (looper->Lock ())
    looper->Quit ();

  return file_name;
}

/* Move the pointer into MBAR and start tracking.  Return whether the
   menu bar was opened correctly.  */
bool
BMenuBar_start_tracking (void *mbar)
{
  EmacsMenuBar *mb = (EmacsMenuBar *) mbar;
  BMessenger messenger (mb);
  BMessage reply;

  messenger.SendMessage (SHOW_MENU_BAR, &reply);

  return reply.what == BE_MENU_BAR_OPEN;
}

#ifdef HAVE_NATIVE_IMAGE_API
int
be_can_translate_type_to_bitmap_p (const char *mime)
{
  BTranslatorRoster *r = BTranslatorRoster::Default ();
  translator_id *ids;
  int32 id_len;

  if (r->GetAllTranslators (&ids, &id_len) != B_OK)
    return 0;

  int found_in = 0;
  int found_out = 0;

  for (int i = 0; i < id_len; ++i)
    {
      found_in = 0;
      found_out = 0;
      const translation_format *i_fmts;
      const translation_format *o_fmts;

      int32 i_count, o_count;

      if (r->GetInputFormats (ids[i], &i_fmts, &i_count) != B_OK)
	continue;

      if (r->GetOutputFormats (ids[i], &o_fmts, &o_count) != B_OK)
	continue;

      for (int x = 0; x < i_count; ++x)
	{
	  if (!strcmp (i_fmts[x].MIME, mime))
	    {
	      found_in = 1;
	      break;
	    }
	}

      for (int x = 0; x < i_count; ++x)
	{
	  if (!strcmp (o_fmts[x].MIME, "image/x-be-bitmap") ||
	      !strcmp (o_fmts[x].MIME, "image/x-vnd.Be-bitmap"))
	    {
	      found_out = 1;
	      break;
	    }
	}

      if (found_in && found_out)
	break;
    }

  delete [] ids;

  return found_in && found_out;
}

void *
be_translate_bitmap_from_file_name (const char *filename)
{
  BBitmap *bm = BTranslationUtils::GetBitmap (filename);
  return bm;
}

void *
be_translate_bitmap_from_memory (const void *buf, size_t bytes)
{
  BMemoryIO io (buf, bytes);
  BBitmap *bm = BTranslationUtils::GetBitmap (&io);
  return bm;
}
#endif

/* Return the size of BITMAP's data, in bytes.  */
size_t
BBitmap_bytes_length (void *bitmap)
{
  BBitmap *bm = (BBitmap *) bitmap;
  return bm->BitsLength ();
}

/* Show VIEW's tooltip.  */
void
BView_show_tooltip (void *view)
{
  BView *vw = (BView *) view;
  if (vw->LockLooper ())
    {
      vw->ShowToolTip (vw->ToolTip ());
      vw->UnlockLooper ();
    }
}


#ifdef USE_BE_CAIRO
/* Return VIEW's cairo context.  */
cairo_t *
EmacsView_cairo_context (void *view)
{
  EmacsView *vw = (EmacsView *) view;
  return vw->cr_context;
}

/* Transfer each clip rectangle in VIEW to the cairo context
   CTX.  */
void
BView_cr_dump_clipping (void *view, cairo_t *ctx)
{
  BView *vw = (BView *) find_appropriate_view_for_draw (view);
  BRegion cr;
  vw->GetClippingRegion (&cr);

  for (int i = 0; i < cr.CountRects (); ++i)
    {
      BRect r = cr.RectAt (i);
      cairo_rectangle (ctx, r.left, r.top,
		       BE_RECT_WIDTH (r),
		       BE_RECT_HEIGHT (r));
    }

  cairo_clip (ctx);
}

/* Lock WINDOW in preparation for drawing using Cairo.  */
void
EmacsWindow_begin_cr_critical_section (void *window)
{
  BWindow *w = (BWindow *) window;
  BView *vw = (BView *) w->FindView ("Emacs");
  EmacsView *ev = dynamic_cast <EmacsView *> (vw);
  if (ev && !ev->cr_surface_lock.Lock ())
    gui_abort ("Couldn't lock view cairo surface");
}

/* Unlock WINDOW in preparation for drawing using Cairo.  */
void
EmacsWindow_end_cr_critical_section (void *window)
{
  BWindow *w = (BWindow *) window;
  BView *vw = (BView *) w->FindView ("Emacs");
  EmacsView *ev = dynamic_cast <EmacsView *> (vw);
  if (ev)
    ev->cr_surface_lock.Unlock ();
}
#endif

/* Get the width of STR in the plain font.  */
int
be_string_width_with_plain_font (const char *str)
{
  return be_plain_font->StringWidth (str);
}

/* Get the ascent + descent of the plain font.  */
int
be_plain_font_height (void)
{
  struct font_height fheight;
  be_plain_font->GetHeight (&fheight);

  return fheight.ascent + fheight.descent;
}

/* Return the number of physical displays connected.  */
int
be_get_display_screens (void)
{
  int count = 1;
  BScreen scr;

  if (!scr.IsValid ())
    gui_abort ("Main screen vanished!");
  while (scr.SetToNext () == B_OK && scr.IsValid ())
    ++count;

  return count;
}

/* Set the minimum width the user can resize WINDOW to.  */
/* Synchronize WINDOW's connection to the App Server.  */
void
BWindow_sync (void *window)
{
  BWindow *w = (BWindow *) window;

  if (!w->LockLooper ())
    gui_abort ("Failed to lock window looper for sync");
  w->Sync ();
  w->UnlockLooper ();
}

/* Set the alignment of WINDOW's dimensions.  */
void
BWindow_set_size_alignment (void *window, int align_width, int align_height)
{
  BWindow *w = (BWindow *) window;

  if (!w->LockLooper ())
    gui_abort ("Failed to lock window looper setting alignment");
#if 0 /* Haiku does not currently implement SetWindowAlignment.  */
  if (w->SetWindowAlignment (B_PIXEL_ALIGNMENT, -1, -1, align_width,
			     align_width, -1, -1, align_height,
			     align_height) != B_NO_ERROR)
    gui_abort ("Invalid pixel alignment");
#endif
  w->UnlockLooper ();
}

void
BWindow_send_behind (void *window, void *other_window)
{
  BWindow *w = (BWindow *) window;
  BWindow *other = (BWindow *) other_window;

  if (!w->LockLooper ())
    gui_abort ("Failed to lock window in order to send it behind another");
  w->SendBehind (other);
  w->UnlockLooper ();
}

bool
BWindow_is_active (void *window)
{
  BWindow *w = (BWindow *) window;
  return w->IsActive ();
}

bool
be_use_subpixel_antialiasing (void)
{
  bool current_subpixel_antialiasing;

  if (get_subpixel_antialiasing (&current_subpixel_antialiasing) != B_OK)
    return false;

  return current_subpixel_antialiasing;
}

void
BWindow_set_override_redirect (void *window, bool override_redirect_p)
{
  EmacsWindow *w = (EmacsWindow *) window;

  if (w->LockLooper ())
    {
      if (override_redirect_p && !w->override_redirect_p)
	{
	  w->override_redirect_p = true;
	  w->pre_override_redirect_look = w->Look ();
	  w->RecomputeFeel ();
	  w->SetLook (B_NO_BORDER_WINDOW_LOOK);
	  w->pre_override_redirect_workspaces = w->Workspaces ();
	  w->SetWorkspaces (B_ALL_WORKSPACES);
	}
      else if (w->override_redirect_p)
	{
	  w->override_redirect_p = false;
	  w->SetLook (w->pre_override_redirect_look);
	  w->RecomputeFeel ();
	  w->SetWorkspaces (w->pre_override_redirect_workspaces);
	}

      w->UnlockLooper ();
    }
}

/* Find a resource by the name NAME inside the settings file.  The
   string returned is in UTF-8 encoding, and will stay allocated as
   long as the BApplication (a.k.a display) is alive.  */
const char *
be_find_setting (const char *name)
{
  Emacs *app = (Emacs *) be_app;
  const char *value;

  /* Note that this is thread-safe since the constructor of `Emacs'
     runs in the main thread.  */
  if (!app->settings_valid_p)
    return NULL;

  if (app->settings.FindString (name, 0, &value) != B_OK)
    return NULL;

  return value;
}

void
BMessage_delete (void *message)
{
  delete (BMessage *) message;
}

static int32
be_drag_message_thread_entry (void *thread_data)
{
  BMessenger *messenger;
  BMessage reply;

  messenger = (BMessenger *) thread_data;
  messenger->SendMessage (WAIT_FOR_RELEASE, &reply);

  return 0;
}

bool
be_drag_message (void *view, void *message, bool allow_same_view,
		 void (*block_input_function) (void),
		 void (*unblock_input_function) (void),
		 void (*process_pending_signals_function) (void),
		 bool (*should_quit_function) (void))
{
  EmacsView *vw = (EmacsView *) view;
  EmacsWindow *window = (EmacsWindow *) vw->Window ();
  BMessage *msg = (BMessage *) message;
  BMessage wait_for_release;
  BMessenger messenger (vw);
  BMessage cancel_message (CANCEL_DROP);
  struct object_wait_info infos[2];
  ssize_t stat;
  thread_id window_thread;

  block_input_function ();

  if (!allow_same_view)
    window_thread = window->Looper ()->Thread ();

  if (!allow_same_view
      && (msg->ReplaceInt64 ("emacs:thread_id", window_thread)
	  == B_NAME_NOT_FOUND))
    msg->AddInt64 ("emacs:thread_id", window_thread);

  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view looper for drag");

  vw->DragMessage (msg, BRect (0, 0, 0, 0));
  vw->UnlockLooper ();

  infos[0].object = port_application_to_emacs;
  infos[0].type = B_OBJECT_TYPE_PORT;
  infos[0].events = B_EVENT_READ;

  infos[1].object = spawn_thread (be_drag_message_thread_entry,
				  "Drag waiter thread",
				  B_DEFAULT_MEDIA_PRIORITY,
				  (void *) &messenger);
  infos[1].type = B_OBJECT_TYPE_THREAD;
  infos[1].events = B_EVENT_INVALID;
  unblock_input_function ();

  if (infos[1].object < B_OK)
    return false;

  block_input_function ();
  resume_thread (infos[1].object);
  unblock_input_function ();

  drag_and_drop_in_progress = true;

  while (true)
    {
      block_input_function ();
      stat = wait_for_objects ((struct object_wait_info *) &infos, 2);
      unblock_input_function ();

      if (stat == B_INTERRUPTED || stat == B_TIMED_OUT
	  || stat == B_WOULD_BLOCK)
	continue;

      if (stat < B_OK)
	gui_abort ("Failed to wait for drag");

      if (infos[0].events & B_EVENT_READ)
	process_pending_signals_function ();

      if (should_quit_function ())
	{
	  /* Do the best we can to prevent something from being
	     dropped, since Haiku doesn't provide a way to actually
	     cancel drag-and-drop.  */
	  if (vw->LockLooper ())
	    {
	      vw->DragMessage (&cancel_message, BRect (0, 0, 0, 0));
	      vw->UnlockLooper ();
	    }

	  messenger.SendMessage (CANCEL_DROP);
	  drag_and_drop_in_progress = false;
	  return true;
	}

      if (infos[1].events & B_EVENT_INVALID)
	{
	  drag_and_drop_in_progress = false;
	  return false;
	}

      infos[0].events = B_EVENT_READ;
      infos[1].events = B_EVENT_INVALID;
    }
}

bool
be_drag_and_drop_in_progress (void)
{
  return drag_and_drop_in_progress;
}

/* Replay the menu bar click event EVENT.  Return whether or not the
   menu bar actually opened.  */
bool
be_replay_menu_bar_event (void *menu_bar,
			  struct haiku_menu_bar_click_event *event)
{
  BMenuBar *m = (BMenuBar *) menu_bar;
  BMessenger messenger (m);
  BMessage reply, msg (REPLAY_MENU_BAR);

  msg.AddPoint ("emacs:point", BPoint (event->x, event->y));
  messenger.SendMessage (&msg, &reply);
  return reply.what == BE_MENU_BAR_OPEN;
}

void
BWindow_set_z_group (void *window, enum haiku_z_group z_group)
{
  EmacsWindow *w = (EmacsWindow *) window;

  if (w->LockLooper ())
    {
      if (w->z_group != z_group)
	{
	  w->z_group = z_group;
	  w->RecomputeFeel ();

	  if (w->z_group == Z_GROUP_BELOW)
	    w->SetFlags (w->Flags () | B_AVOID_FRONT);
	  else
	    w->SetFlags (w->Flags () & ~B_AVOID_FRONT);
	}

      w->UnlockLooper ();
    }
}

int
be_get_ui_color (const char *name, uint32_t *color)
{
  color_which which;
  rgb_color rgb;

  which = which_ui_color (name);

  if (which == B_NO_COLOR)
    return 1;

  rgb = ui_color (which);
  *color = (rgb.blue | rgb.green << 8
	    | rgb.red << 16 | 255 << 24);

  return 0;
}

bool
be_select_font (void (*process_pending_signals_function) (void),
		bool (*should_quit_function) (void),
		haiku_font_family_or_style *family,
		haiku_font_family_or_style *style,
		int *size, bool allow_monospace_only,
		int initial_family, int initial_style,
		int initial_size, bool initial_antialias,
		bool *disable_antialias)
{
  EmacsFontSelectionDialog *dialog;
  struct font_selection_dialog_message msg;
  uint32 flags;
  font_family family_buffer;
  font_style style_buffer;

  dialog = new EmacsFontSelectionDialog (allow_monospace_only,
					 initial_family, initial_style,
					 initial_size, initial_antialias);
  dialog->CenterOnScreen ();

  if (dialog->InitCheck () < B_OK)
    {
      dialog->Quit ();
      return false;
    }

  dialog->Show ();
  dialog->WaitForChoice (&msg, process_pending_signals_function,
			 should_quit_function);

  if (!dialog->LockLooper ())
    gui_abort ("Failed to lock font selection dialog looper");
  dialog->Quit ();

  if (msg.cancel)
    return false;

  if (get_font_family (msg.family_idx,
		       &family_buffer, &flags) != B_OK
      || get_font_style (family_buffer, msg.style_idx,
			 &style_buffer, &flags) != B_OK)
    return false;

  memcpy (family, family_buffer, sizeof *family);
  memcpy (style, style_buffer, sizeof *style);
  *size = msg.size_specified ? msg.size : -1;
  *disable_antialias = msg.disable_antialias;

  return true;
}

void
BWindow_set_sticky (void *window, bool sticky)
{
  BWindow *w = (BWindow *) window;

  if (w->LockLooper ())
    {
      w->SetFlags (sticky ? (w->Flags ()
			     | B_SAME_POSITION_IN_ALL_WORKSPACES)
		   : w->Flags () & ~B_SAME_POSITION_IN_ALL_WORKSPACES);

      w->UnlockLooper ();
    }
}

status_t
be_roster_launch (const char *type, const char *file, char **cargs,
		  ptrdiff_t nargs, void *message, team_id *team_id)
{
  BEntry entry;
  entry_ref ref;

  if (type)
    {
      if (message)
	return be_roster->Launch (type, (BMessage *) message,
				  team_id);

      return be_roster->Launch (type, (nargs > INT_MAX
				       ? INT_MAX : nargs),
				cargs, team_id);
    }

  if (entry.SetTo (file) != B_OK)
    return B_ERROR;

  if (entry.GetRef (&ref) != B_OK)
    return B_ERROR;

  if (message)
    return be_roster->Launch (&ref, (BMessage *) message,
			      team_id);

  return be_roster->Launch (&ref, (nargs > INT_MAX
				   ? INT_MAX : nargs),
			    cargs, team_id);
}

void *
be_create_pixmap_cursor (void *bitmap, int x, int y)
{
  BBitmap *bm;
  BCursor *cursor;

  bm = (BBitmap *) bitmap;
  cursor = new BCursor (bm, BPoint (x, y));

  if (cursor->InitCheck () != B_OK)
    {
      delete cursor;
      return NULL;
    }

  return cursor;
}

void
be_get_window_decorator_dimensions (void *window, int *left, int *top,
				    int *right, int *bottom)
{
  BWindow *wnd;
  BRect frame, window_frame;

  wnd = (BWindow *) window;

  if (!wnd->LockLooper ())
    gui_abort ("Failed to lock window looper frame");

  frame = wnd->DecoratorFrame ();
  window_frame = wnd->Frame ();

  if (left)
    *left = window_frame.left - frame.left;

  if (top)
    *top = window_frame.top - frame.top;

  if (right)
    *right = frame.right - window_frame.right;

  if (bottom)
    *bottom = frame.bottom - window_frame.bottom;

  wnd->UnlockLooper ();
}

void
be_get_window_decorator_frame (void *window, int *left, int *top,
			       int *width, int *height)
{
  BWindow *wnd;
  BRect frame;

  wnd = (BWindow *) window;

  if (!wnd->LockLooper ())
    gui_abort ("Failed to lock window looper frame");

  frame = wnd->DecoratorFrame ();

  *left = frame.left;
  *top = frame.top;
  *width = BE_RECT_WIDTH (frame);
  *height = BE_RECT_HEIGHT (frame);

  wnd->UnlockLooper ();
}

/* Request that a MOVE_EVENT be sent for WINDOW.  This is so that
   frame offsets can be updated after a frame parameter affecting
   decorators changes.  Sending an event instead of updating the
   offsets directly avoids race conditions where events with older
   information are received after the update happens.  */
void
be_send_move_frame_event (void *window)
{
  BWindow *wnd = (BWindow *) window;
  BMessenger msg (wnd);

  msg.SendMessage (SEND_MOVE_FRAME_EVENT);
}

void
be_lock_window (void *window)
{
  BWindow *wnd = (BWindow *) window;

  if (!wnd->LockLooper ())
    gui_abort ("Failed to lock window looper");
}

void
be_unlock_window (void *window)
{
  BWindow *wnd = (BWindow *) window;

  wnd->UnlockLooper ();
}

void
be_set_window_fullscreen_mode (void *window, enum haiku_fullscreen_mode mode)
{
  EmacsWindow *w = (EmacsWindow *) window;

  if (!w->LockLooper ())
    gui_abort ("Failed to lock window to set fullscreen mode");

  w->SetFullscreen (mode);
  w->UnlockLooper ();
}

bool
be_get_explicit_workarea (int *x, int *y, int *width, int *height)
{
  BDeskbar deskbar;
  BRect zoom;
  deskbar_location location;

  location = deskbar.Location ();

  if (location != B_DESKBAR_TOP
      && location != B_DESKBAR_BOTTOM)
    return false;

  zoom = get_zoom_rect (NULL);

  *x = zoom.left;
  *y = zoom.top;
  *width = BE_RECT_WIDTH (zoom);
  *height = BE_RECT_HEIGHT (zoom);

  return true;
}

/* Clear the grab view.  This has to be called manually from some
   places, since we don't get B_MOUSE_UP messages after a popup menu
   is run.  */

void
be_clear_grab_view (void)
{
  if (grab_view_locker.Lock ())
    {
      grab_view = NULL;
      grab_view_locker.Unlock ();
    }
}

void
be_set_use_frame_synchronization (void *view, bool sync)
{
  EmacsView *vw;

  vw = (EmacsView *) view;
  vw->SetFrameSynchronization (sync);
}

status_t
be_write_node_message (const char *path, const char *name, void *message)
{
  BNode node (path);
  status_t rc;
  ssize_t flat, result;
  char *buffer;
  BMessage *msg;

  rc = node.InitCheck ();
  msg = (BMessage *) message;

  if (rc < B_OK)
    return rc;

  flat = msg->FlattenedSize ();
  if (flat < B_OK)
    return flat;

  buffer = new (std::nothrow) char[flat];
  if (!buffer)
    return B_NO_MEMORY;

  rc = msg->Flatten (buffer, flat);
  if (rc < B_OK)
    {
      delete[] buffer;
      return rc;
    }

  result = node.WriteAttr (name, B_MIME_TYPE, 0,
			   buffer, flat);
  delete[] buffer;

  if (result < B_OK)
    return result;

  if (result != flat)
    return B_ERROR;

  return B_OK;
}

void
be_send_message (const char *app_id, void *message)
{
  BMessenger messenger (app_id);

  messenger.SendMessage ((BMessage *) message);
}
