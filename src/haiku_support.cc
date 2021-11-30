/* Haiku window system support.  Hey, Emacs, this is -*- C++ -*-
   Copyright (C) 2021 Free Software Foundation, Inc.

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

#include <app/Application.h>
#include <app/Cursor.h>
#include <app/Messenger.h>

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

#include <locale/UnicodeChar.h>

#include <game/WindowScreen.h>
#include <game/DirectWindow.h>

#include <storage/Entry.h>
#include <storage/Path.h>
#include <storage/FilePanel.h>
#include <storage/AppFileInfo.h>
#include <storage/Path.h>
#include <storage/PathFinder.h>

#include <support/Beep.h>
#include <support/DataIO.h>
#include <support/Locker.h>

#include <translation/TranslatorRoster.h>
#include <translation/TranslationDefs.h>
#include <translation/TranslationUtils.h>

#include <kernel/OS.h>
#include <kernel/fs_attr.h>
#include <kernel/scheduler.h>

#include <private/interface/ToolTip.h>

#include <cmath>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <csignal>
#include <cfloat>

#include <pthread.h>

#ifdef USE_BE_CAIRO
#include <cairo.h>
#endif

#include "haiku_support.h"

#define SCROLL_BAR_UPDATE 3000

static color_space dpy_color_space = B_NO_COLOR_SPACE;
static key_map *key_map = NULL;
static char *key_chars = NULL;
static BLocker key_map_lock;

extern "C"
{
  extern _Noreturn void emacs_abort (void);
  /* Also defined in haikuterm.h.  */
  extern void be_app_quit (void);
}

static thread_id app_thread;

_Noreturn void
gui_abort (const char *msg)
{
  fprintf (stderr, "Abort in GUI code: %s\n", msg);
  fprintf (stderr, "Under Haiku, Emacs cannot recover from errors in GUI code\n");
  fprintf (stderr, "App Server disconnects usually manifest as bitmap "
	   "initialization failures or lock failures.");
  emacs_abort ();
}

#ifdef USE_BE_CAIRO
static cairo_format_t
cairo_format_from_color_space (color_space space)
{
  switch (space)
    {
    case B_RGBA32:
      return CAIRO_FORMAT_ARGB32;
    case B_RGB32:
      return CAIRO_FORMAT_RGB24;
    case B_RGB16:
      return CAIRO_FORMAT_RGB16_565;
    case B_GRAY8:
      return CAIRO_FORMAT_A8;
    case B_GRAY1:
      return CAIRO_FORMAT_A1;
    default:
      gui_abort ("Unsupported color space");
    }
}
#endif

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

class Emacs : public BApplication
{
public:
  Emacs () : BApplication ("application/x-vnd.GNU-emacs")
  {
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
    haiku_write (APP_QUIT_REQUESTED_EVENT, &rq);
    return 0;
  }

  void
  RefsReceived (BMessage *msg)
  {
    struct haiku_refs_event rq;
    entry_ref ref;
    BEntry entry;
    BPath path;
    int32 cookie = 0;
    int32 x, y;
    void *window;

    if ((msg->FindPointer ("window", 0, &window) != B_OK)
	|| (msg->FindInt32 ("x", 0, &x) != B_OK)
	|| (msg->FindInt32 ("y", 0, &y) != B_OK))
      return;

    rq.window = window;
    rq.x = x;
    rq.y = y;

    while (msg->FindRef ("refs", cookie++, &ref) == B_OK)
      {
        if (entry.SetTo (&ref, 0) == B_OK
            && entry.GetPath (&path) == B_OK)
          {
            rq.ref = strdup (path.Path ());
            haiku_write (REFS_EVENT, &rq);
          }
      }
  }
};

class EmacsWindow : public BDirectWindow
{
public:
  struct child_frame
  {
    struct child_frame *next;
    int xoff, yoff;
    EmacsWindow *window;
  } *subset_windows = NULL;

  EmacsWindow *parent = NULL;
  BRect pre_fullscreen_rect;
  BRect pre_zoom_rect;
  int x_before_zoom = INT_MIN;
  int y_before_zoom = INT_MIN;
  int fullscreen_p = 0;
  int zoomed_p = 0;
  int shown_flag = 0;

#ifdef USE_BE_CAIRO
  BLocker surface_lock;
  cairo_surface_t *cr_surface = NULL;
#endif

  EmacsWindow () : BDirectWindow (BRect (0, 0, 0, 0), "", B_TITLED_WINDOW_LOOK,
				  B_NORMAL_WINDOW_FEEL, B_NO_SERVER_SIDE_WINDOW_MODIFIERS)
  {

  }

  ~EmacsWindow ()
  {
    struct child_frame *next;
    for (struct child_frame *f = subset_windows; f; f = next)
      {
	f->window->Unparent ();
	next = f->next;
	delete f;
      }

    if (this->parent)
      UnparentAndUnlink ();

#ifdef USE_BE_CAIRO
    if (!surface_lock.Lock ())
      gui_abort ("Failed to lock cairo surface");
    if (cr_surface)
      {
	cairo_surface_destroy (cr_surface);
	cr_surface = NULL;
      }
    surface_lock.Unlock ();
#endif
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
    UpwardsSubset (w);
    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      f->window->UpwardsSubsetChildren (w);
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
    UpwardsUnSubset (w);
    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      f->window->UpwardsUnSubsetChildren (w);
  }

  void
  Unparent (void)
  {
    this->SetFeel (B_NORMAL_WINDOW_FEEL);
    UpwardsUnSubsetChildren (parent);
    this->RemoveFromSubset (this);
    this->parent = NULL;
    if (fullscreen_p)
      {
	fullscreen_p = 0;
	MakeFullscreen (1);
      }
  }

  void
  UnparentAndUnlink (void)
  {
    this->parent->UnlinkChild (this);
    this->Unparent ();
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
	    if (tem == subset_windows)
	      subset_windows = NULL;
	    delete tem;
	    return;
	  }
      }

    gui_abort ("Failed to unlink child frame");
  }

  void
  ParentTo (EmacsWindow *window)
  {
    if (this->parent)
      UnparentAndUnlink ();

    this->parent = window;
    this->SetFeel (B_FLOATING_SUBSET_WINDOW_FEEL);
    this->AddToSubset (this);
    if (!IsHidden () && this->parent)
      UpwardsSubsetChildren (parent);
    if (fullscreen_p)
      {
	fullscreen_p = 0;
	MakeFullscreen (1);
      }
    this->Sync ();
    window->LinkChild (this);
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
  DoMove (struct child_frame *f)
  {
    BRect frame = this->Frame ();
    f->window->MoveTo (frame.left + f->xoff,
		       frame.top + f->yoff);
    this->Sync ();
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
    for (struct child_frame *f = subset_windows; f;
	 f = f->next)
      {
	if (window == f->window)
	  {
	    f->xoff = xoff;
	    f->yoff = yoff;
	    if (!weak_p)
	      DoMove (f);
	    return;
	  }
      }

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
  DirectConnected (direct_buffer_info *info)
  {
#ifdef USE_BE_CAIRO
    if (!surface_lock.Lock ())
      gui_abort ("Failed to lock window direct cr surface");
    if (cr_surface)
      {
	cairo_surface_destroy (cr_surface);
	cr_surface = NULL;
      }

    if (info->buffer_state != B_DIRECT_STOP)
      {
	int left, top, right, bottom;
	left = info->clip_bounds.left;
	top = info->clip_bounds.top;
	right = info->clip_bounds.right;
	bottom = info->clip_bounds.bottom;

	unsigned char *bits = (unsigned char *) info->bits;
	if ((info->bits_per_pixel % 8) == 0)
	  {
	    bits += info->bytes_per_row * top;
	    bits += (left * info->bits_per_pixel / 8);
	    cr_surface = cairo_image_surface_create_for_data
	      (bits,
	       cairo_format_from_color_space (info->pixel_format),
	       right - left + 1,
	       bottom - top + 1,
	       info->bytes_per_row);
	  }
      }
    surface_lock.Unlock ();
#endif
  }

  void
  MessageReceived (BMessage *msg)
  {
    int32 old_what = 0;

    if (msg->WasDropped ())
      {
	entry_ref ref;
	BPoint whereto;

        if (msg->FindRef ("refs", &ref) == B_OK)
	  {
	    msg->what = B_REFS_RECEIVED;
	    msg->AddPointer ("window", this);
	    if (msg->FindPoint ("_drop_point_", &whereto) == B_OK)
	      {
		this->ConvertFromScreen (&whereto);
		msg->AddInt32 ("x", whereto.x);
		msg->AddInt32 ("y", whereto.y);
	      }
	    be_app->PostMessage (msg);
	    msg->SendReply (B_OK);
	  }
      }
    else if (msg->GetPointer ("menuptr"))
      {
	struct haiku_menu_bar_select_event rq;
	rq.window = this;
	rq.ptr = (void *) msg->GetPointer ("menuptr");
	haiku_write (MENU_BAR_SELECT_EVENT, &rq);
      }
    else if (msg->what == 'FPSE'
	     || ((msg->FindInt32 ("old_what", &old_what) == B_OK
		  && old_what == 'FPSE')))
      {
	struct haiku_file_panel_event rq;
	BEntry entry;
	BPath path;
	entry_ref ref;

	rq.ptr = NULL;

	if (msg->FindRef ("refs", &ref) == B_OK &&
	    entry.SetTo (&ref, 0) == B_OK &&
	    entry.GetPath (&path) == B_OK)
	  {
	    const char *str_path = path.Path ();
	    if (str_path)
	      rq.ptr = strdup (str_path);
	  }

	if (msg->FindRef ("directory", &ref),
	    entry.SetTo (&ref, 0) == B_OK &&
	    entry.GetPath (&path) == B_OK)
	  {
	    const char *name = msg->GetString ("name");
	    const char *str_path = path.Path ();

	    if (name)
	      {
		char str_buf[std::strlen (str_path)
			     + std::strlen (name) + 2];
		snprintf ((char *) &str_buf,
			  std::strlen (str_path)
			  + std::strlen (name) + 2, "%s/%s",
			  str_path, name);
		rq.ptr = strdup (str_buf);
	      }
	  }

	haiku_write (FILE_PANEL_EVENT, &rq);
      }
    else
      BDirectWindow::MessageReceived (msg);
  }

  void
  DispatchMessage (BMessage *msg, BHandler *handler)
  {
    if (msg->what == B_KEY_DOWN || msg->what == B_KEY_UP)
      {
	struct haiku_key_event rq;
	rq.window = this;

	int32_t code = msg->GetInt32 ("raw_char", 0);

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

	rq.mb_char = code;
	rq.kc = msg->GetInt32 ("key", -1);
	rq.unraw_mb_char =
	  BUnicodeChar::FromUTF8 (msg->GetString ("bytes"));

	if ((mods & B_SHIFT_KEY) && rq.kc >= 0)
	  map_shift (rq.kc, &rq.unraw_mb_char);
	else if (rq.kc >= 0)
	  map_normal (rq.kc, &rq.unraw_mb_char);

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
    else
      BDirectWindow::DispatchMessage (msg, handler);
  }

  void
  MenusBeginning ()
  {
    struct haiku_menu_bar_state_event rq;
    rq.window = this;

    haiku_write (MENU_BAR_OPEN, &rq);
  }

  void
  MenusEnded ()
  {
    struct haiku_menu_bar_state_event rq;
    rq.window = this;

    haiku_write (MENU_BAR_CLOSE, &rq);
  }

  void
  FrameResized (float newWidth, float newHeight)
  {
    struct haiku_resize_event rq;
    rq.window = this;
    rq.px_heightf = newHeight + 1.0f;
    rq.px_widthf = newWidth + 1.0f;

    haiku_write (FRAME_RESIZED, &rq);
    BDirectWindow::FrameResized (newWidth, newHeight);
  }

  void
  FrameMoved (BPoint newPosition)
  {
    struct haiku_move_event rq;
    rq.window = this;
    rq.x = std::lrint (newPosition.x);
    rq.y = std::lrint (newPosition.y);

    haiku_write (MOVE_EVENT, &rq);

    for (struct child_frame *f = subset_windows;
	 f; f = f->next)
      DoMove (f);
    BDirectWindow::FrameMoved (newPosition);
  }

  void
  WorkspacesChanged (uint32_t old, uint32_t n)
  {
    for (struct child_frame *f = subset_windows;
	 f; f = f->next)
      DoUpdateWorkspace (f);
  }

  void
  EmacsMoveTo (int x, int y)
  {
    if (!this->parent)
      this->MoveTo (x, y);
    else
      this->parent->MoveChild (this, x, y, 0);
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
    BDirectWindow::Minimize (minimized_p);
    struct haiku_iconification_event rq;
    rq.window = this;
    rq.iconified_p = !parent && minimized_p;

    haiku_write (ICONIFICATION, &rq);
  }

  void
  EmacsHide (void)
  {
    if (this->IsHidden ())
      return;
    Hide ();
    if (this->parent)
      UpwardsUnSubsetChildren (this->parent);
  }

  void
  EmacsShow (void)
  {
    if (!this->IsHidden ())
      return;
    if (this->parent)
      shown_flag = 1;
    Show ();
    if (this->parent)
      UpwardsSubsetChildren (this->parent);
  }

  void
  Zoom (BPoint o, float w, float h)
  {
    struct haiku_zoom_event rq;
    rq.window = this;

    rq.x = o.x;
    rq.y = o.y;

    rq.width = w + 1;
    rq.height = h + 1;

    if (fullscreen_p)
      MakeFullscreen (0);

    if (o.x != x_before_zoom ||
	o.y != y_before_zoom)
      {
	x_before_zoom = Frame ().left;
	y_before_zoom = Frame ().top;
	pre_zoom_rect = Frame ();
	zoomed_p = 1;
	haiku_write (ZOOM_EVENT, &rq);
      }
    else
      {
	zoomed_p = 0;
	x_before_zoom = y_before_zoom = INT_MIN;
      }

    BDirectWindow::Zoom (o, w, h);
  }

  void
  UnZoom (void)
  {
    if (!zoomed_p)
      return;
    zoomed_p = 0;

    EmacsMoveTo (pre_zoom_rect.left, pre_zoom_rect.top);
    ResizeTo (pre_zoom_rect.Width (),
	      pre_zoom_rect.Height ());
  }

  void
  GetParentWidthHeight (int *width, int *height)
  {
    if (parent)
      {
	*width = parent->Frame ().Width ();
	*height = parent->Frame ().Height ();
      }
    else
      {
	BScreen s (this);
	*width = s.Frame ().Width ();
	*height = s.Frame ().Height ();
      }
  }

  void
  OffsetChildRect (BRect *r, EmacsWindow *c)
  {
    for (struct child_frame *f; f; f = f->next)
      if (f->window == c)
	{
	  r->top -= f->yoff;
	  r->bottom -= f->yoff;
	  r->left -= f->xoff;
	  r->right -= f->xoff;
	  return;
	}

    gui_abort ("Trying to calculate offsets for a child frame that doesn't exist");
  }

  void
  MakeFullscreen (int make_fullscreen_p)
  {
    BScreen screen (this);

      if (!screen.IsValid ())
	gui_abort ("Trying to make a window fullscreen without a screen");

    if (make_fullscreen_p == fullscreen_p)
      return;

    fullscreen_p = make_fullscreen_p;
    uint32 flags = Flags ();
    if (fullscreen_p)
      {
	if (zoomed_p)
	  UnZoom ();

	flags |= B_NOT_MOVABLE | B_NOT_ZOOMABLE;
	pre_fullscreen_rect = Frame ();
	if (parent)
	  parent->OffsetChildRect (&pre_fullscreen_rect, this);

	int w, h;
	EmacsMoveTo (0, 0);
	GetParentWidthHeight (&w, &h);
	ResizeTo (w, h);
      }
    else
      {
	flags &= ~(B_NOT_MOVABLE | B_NOT_ZOOMABLE);
	EmacsMoveTo (pre_fullscreen_rect.left,
		     pre_fullscreen_rect.top);
	ResizeTo (pre_fullscreen_rect.Width (),
		  pre_fullscreen_rect.Height ());
      }
    SetFlags (flags);
  }
};

class EmacsMenuBar : public BMenuBar
{
public:
  EmacsMenuBar () : BMenuBar (BRect (0, 0, 0, 0), NULL)
  {
  }

  void
  FrameResized (float newWidth, float newHeight)
  {
    struct haiku_menu_bar_resize_event rq;
    rq.window = this->Window ();
    rq.height = std::lrint (newHeight);
    rq.width = std::lrint (newWidth);

    haiku_write (MENU_BAR_RESIZE, &rq);
    BMenuBar::FrameResized (newWidth, newHeight);
  }
};

class EmacsView : public BView
{
public:
  uint32_t visible_bell_color = 0;
  uint32_t previous_buttons = 0;
  int looper_locked_count = 0;
  BRegion sb_region;

  BView *offscreen_draw_view = NULL;
  BBitmap *offscreen_draw_bitmap_1 = NULL;
  BBitmap *copy_bitmap = NULL;

#ifdef USE_BE_CAIRO
  cairo_surface_t *cr_surface = NULL;
  BLocker cr_surface_lock;
#endif

  BPoint tt_absl_pos;

  color_space cspace;

  EmacsView () : BView (BRect (0, 0, 0, 0), "Emacs", B_FOLLOW_NONE, B_WILL_DRAW)
  {

  }

  ~EmacsView ()
  {
    TearDownDoubleBuffering ();
  }

  void
  AttachedToWindow (void)
  {
    cspace = B_RGBA32;
  }

#ifdef USE_BE_CAIRO
  void
  DetachCairoSurface (void)
  {
    if (!cr_surface_lock.Lock ())
      gui_abort ("Could not lock cr surface during detachment");
    if (!cr_surface)
      gui_abort ("Trying to detach window cr surface when none exists");
    cairo_surface_destroy (cr_surface);
    cr_surface = NULL;
    cr_surface_lock.Unlock ();
  }

  void
  AttachCairoSurface (void)
  {
    if (!cr_surface_lock.Lock ())
      gui_abort ("Could not lock cr surface during attachment");
    if (cr_surface)
      gui_abort ("Trying to attach cr surface when one already exists");
    cr_surface = cairo_image_surface_create_for_data
      ((unsigned char *) offscreen_draw_bitmap_1->Bits (),
       CAIRO_FORMAT_ARGB32, offscreen_draw_bitmap_1->Bounds ().Width (),
       offscreen_draw_bitmap_1->Bounds ().Height (),
       offscreen_draw_bitmap_1->BytesPerRow ());
    if (!cr_surface)
      gui_abort ("Cr surface allocation failed for double-buffered view");
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
	offscreen_draw_bitmap_1 = new BBitmap (Frame (), cspace, 1);
	if (offscreen_draw_bitmap_1->InitCheck () != B_OK)
	  gui_abort ("Offscreen draw bitmap initialization failed");

	offscreen_draw_view->MoveTo (Frame ().left, Frame ().top);
	offscreen_draw_view->ResizeTo (Frame ().Width (), Frame ().Height ());
	offscreen_draw_bitmap_1->AddChild (offscreen_draw_view);
#ifdef USE_BE_CAIRO
	AttachCairoSurface ();
#endif

	if (looper_locked_count)
	  {
	    offscreen_draw_bitmap_1->Lock ();
	  }

	UnlockLooper ();
      }
  }

  void
  Pulse (void)
  {
    visible_bell_color = 0;
    SetFlags (Flags () & ~B_PULSE_NEEDED);
    Window ()->SetPulseRate (0);
    Invalidate ();
  }

  void
  Draw (BRect expose_bounds)
  {
    struct haiku_expose_event rq;
    EmacsWindow *w = (EmacsWindow *) Window ();

    if (visible_bell_color > 0)
      {
	PushState ();
	BView_SetHighColorForVisibleBell (this, visible_bell_color);
	FillRect (Frame ());
	PopState ();
	return;
      }

    if (w->shown_flag)
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
  DoVisibleBell (uint32_t color)
  {
    if (!LockLooper ())
      gui_abort ("Failed to lock looper during visible bell");
    visible_bell_color = color | (255 << 24);
    SetFlags (Flags () | B_PULSE_NEEDED);
    Window ()->SetPulseRate (100 * 1000);
    Invalidate ();
    UnlockLooper ();
  }

  void
  FlipBuffers (void)
  {
    if (!LockLooper ())
      gui_abort ("Failed to lock looper during buffer flip");
    if (!offscreen_draw_view)
      gui_abort ("Failed to lock offscreen view during buffer flip");

    offscreen_draw_view->Flush ();
    offscreen_draw_view->Sync ();

    EmacsWindow *w = (EmacsWindow *) Window ();
    w->shown_flag = 0;

    if (copy_bitmap &&
	copy_bitmap->Bounds () != offscreen_draw_bitmap_1->Bounds ())
      {
	delete copy_bitmap;
	copy_bitmap = NULL;
      }
    if (!copy_bitmap)
      copy_bitmap = new BBitmap (offscreen_draw_bitmap_1);
    else
      copy_bitmap->ImportBits (offscreen_draw_bitmap_1);

    if (copy_bitmap->InitCheck () != B_OK)
      gui_abort ("Failed to init copy bitmap during buffer flip");

    SetViewBitmap (copy_bitmap,
		   Frame (), Frame (), B_FOLLOW_NONE, 0);

    Invalidate ();
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

    offscreen_draw_bitmap_1 = new BBitmap (Frame (), cspace, 1);
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
	  gui_abort ("Failed to lock bitmap after double buffering was set up.");
      }

    UnlockLooper ();
    Invalidate ();
  }

  void
  MouseMoved (BPoint point, uint32 transit, const BMessage *msg)
  {
    struct haiku_mouse_motion_event rq;

    rq.just_exited_p = transit == B_EXITED_VIEW;
    rq.x = point.x;
    rq.y = point.y;
    rq.be_code = transit;
    rq.window = this->Window ();

    if (ToolTip ())
      ToolTip ()->SetMouseRelativeLocation (BPoint (-(point.x - tt_absl_pos.x),
						    -(point.y - tt_absl_pos.y)));

    haiku_write (MOUSE_MOTION, &rq);
  }

  void
  MouseDown (BPoint point)
  {
    struct haiku_button_event rq;
    uint32 buttons;

    this->GetMouse (&point, &buttons, false);

    rq.window = this->Window ();
    rq.btn_no = 0;

    if (!(previous_buttons & B_PRIMARY_MOUSE_BUTTON) &&
	(buttons & B_PRIMARY_MOUSE_BUTTON))
      rq.btn_no = 0;
    else if (!(previous_buttons & B_SECONDARY_MOUSE_BUTTON) &&
	     (buttons & B_SECONDARY_MOUSE_BUTTON))
      rq.btn_no = 2;
    else if (!(previous_buttons & B_TERTIARY_MOUSE_BUTTON) &&
	     (buttons & B_TERTIARY_MOUSE_BUTTON))
      rq.btn_no = 1;
    previous_buttons = buttons;

    rq.x = point.x;
    rq.y = point.y;

    uint32_t mods = modifiers ();

    rq.modifiers = 0;
    if (mods & B_SHIFT_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SHIFT;

    if (mods & B_CONTROL_KEY)
      rq.modifiers |= HAIKU_MODIFIER_CTRL;

    if (mods & B_COMMAND_KEY)
      rq.modifiers |= HAIKU_MODIFIER_ALT;

    if (mods & B_OPTION_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SUPER;

    SetMouseEventMask (B_POINTER_EVENTS, B_LOCK_WINDOW_FOCUS);

    haiku_write (BUTTON_DOWN, &rq);
  }

  void
  MouseUp (BPoint point)
  {
    struct haiku_button_event rq;
    uint32 buttons;

    this->GetMouse (&point, &buttons, false);

    rq.window = this->Window ();
    rq.btn_no = 0;

    if ((previous_buttons & B_PRIMARY_MOUSE_BUTTON)
	&& !(buttons & B_PRIMARY_MOUSE_BUTTON))
      rq.btn_no = 0;
    else if ((previous_buttons & B_SECONDARY_MOUSE_BUTTON)
	     && !(buttons & B_SECONDARY_MOUSE_BUTTON))
      rq.btn_no = 2;
    else if ((previous_buttons & B_TERTIARY_MOUSE_BUTTON)
	     && !(buttons & B_TERTIARY_MOUSE_BUTTON))
      rq.btn_no = 1;
    previous_buttons = buttons;

    rq.x = point.x;
    rq.y = point.y;

    uint32_t mods = modifiers ();

    rq.modifiers = 0;
    if (mods & B_SHIFT_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SHIFT;

    if (mods & B_CONTROL_KEY)
      rq.modifiers |= HAIKU_MODIFIER_CTRL;

    if (mods & B_COMMAND_KEY)
      rq.modifiers |= HAIKU_MODIFIER_ALT;

    if (mods & B_OPTION_KEY)
      rq.modifiers |= HAIKU_MODIFIER_SUPER;

    if (!buttons)
      SetMouseEventMask (0, 0);

    haiku_write (BUTTON_UP, &rq);
  }
};

class EmacsScrollBar : public BScrollBar
{
public:
  void *scroll_bar;

  EmacsScrollBar (int x, int y, int x1, int y1, bool horizontal_p) :
    BScrollBar (BRect (x, y, x1, y1), NULL, NULL, 0, 0, horizontal_p ?
		B_HORIZONTAL : B_VERTICAL)
  {
    BView *vw = (BView *) this;
    vw->SetResizingMode (B_FOLLOW_NONE);
  }

  void
  MessageReceived (BMessage *msg)
  {
    if (msg->what == SCROLL_BAR_UPDATE)
      {
	this->SetRange (0, msg->GetInt32 ("emacs:range", 0));
	this->SetValue (msg->GetInt32 ("emacs:units", 0));
      }

    BScrollBar::MessageReceived (msg);
  }

  void
  ValueChanged (float new_value)
  {
    struct haiku_scroll_bar_value_event rq;
    rq.scroll_bar = scroll_bar;
    rq.position = new_value;

    haiku_write (SCROLL_BAR_VALUE_EVENT, &rq);
  }

  void
  MouseDown (BPoint pt)
  {
    struct haiku_scroll_bar_drag_event rq;
    rq.dragging_p = 1;
    rq.scroll_bar = scroll_bar;

    haiku_write (SCROLL_BAR_DRAG_EVENT, &rq);
    BScrollBar::MouseDown (pt);
  }

  void
  MouseUp (BPoint pt)
  {
    struct haiku_scroll_bar_drag_event rq;
    rq.dragging_p = 0;
    rq.scroll_bar = scroll_bar;

    haiku_write (SCROLL_BAR_DRAG_EVENT, &rq);
    BScrollBar::MouseUp (pt);
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
    BView_SetHighColorForVisibleBell (menu, 0);
    BMenuItem::DrawContent ();
    menu->PopState ();
  }
};

class EmacsMenuItem : public BMenuItem
{
public:
  int menu_bar_id = -1;
  void *wind_ptr = NULL;
  char *key = NULL;
  char *help = NULL;

  EmacsMenuItem (const char *ky,
		 const char *str,
		 const char *help,
		 BMessage *message = NULL) : BMenuItem (str, message)
  {
    if (ky)
      {
	key = strdup (ky);
	if (!key)
	  gui_abort ("strdup failed");
      }

    if (help)
      {
	this->help = strdup (help);
	if (!this->help)
	  gui_abort ("strdup failed");
      }
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
	BRect r = menu->Frame ();
	int w = menu->StringWidth (key);
	menu->MovePenTo (BPoint (r.Width () - w - 4,
				 menu->PenLocation ().y));
	menu->DrawString (key);
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

    if (menu_bar_id >= 0)
      {
	rq.window = wind_ptr;
	rq.mb_idx = highlight_p ? menu_bar_id : -1;

	haiku_write (MENU_BAR_HELP_EVENT, &rq);
      }
    else if (help)
      {
	Menu ()->SetToolTip (highlight_p ? help : NULL);
      }

    BMenuItem::Highlight (highlight_p);
  }
};

class EmacsPopUpMenu : public BPopUpMenu
{
public:
  EmacsPopUpMenu (const char *name) : BPopUpMenu (name, 0)
  {

  }

  void
  FrameResized (float w, float h)
  {
    Invalidate ();
    BPopUpMenu::FrameResized (w, h);
  }
};

static int32
start_running_application (void *data)
{
  haiku_io_init_in_app_thread ();

  if (!((Emacs *) data)->Lock ())
    gui_abort ("Failed to lock application");

  ((Emacs *) data)->Run ();
  ((Emacs *) data)->Unlock ();
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

/* Set up an application and return it.  If starting the application
   thread fails, abort Emacs.  */
void *
BApplication_setup (void)
{
  if (be_app)
    return be_app;
  thread_id id;
  Emacs *app;

  app = new Emacs;
  app->Unlock ();
  if ((id = spawn_thread (start_running_application, "Emacs app thread",
			  B_DEFAULT_MEDIA_PRIORITY, app)) < 0)
    gui_abort ("spawn_thread failed");

  resume_thread (id);

  app_thread = id;
  return app;
}

/* Set up and return a window with its view put in VIEW.  */
void *
BWindow_new (void *_view)
{
  BWindow *window = new (std::nothrow) EmacsWindow;
  BView **v = (BView **) _view;
  if (!window)
    {
      *v = NULL;
      return window;
    }

  BView *vw = new (std::nothrow) EmacsView;
  if (!vw)
    {
      *v = NULL;
      window->Lock ();
      window->Quit ();
      return NULL;
    }
  window->AddChild (vw);
  *v = vw;
  return window;
}

void
BWindow_quit (void *window)
{
  ((BWindow *) window)->Lock ();
  ((BWindow *) window)->Quit ();
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
  win->Sync ();
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
  ((BWindow *) window)->ResizeTo (width, height);
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
BScreen_px_dim (int *width, int *height)
{
  BScreen screen;
  if (!screen.IsValid ())
    gui_abort ("Invalid screen");
  BRect frame = screen.Frame ();

  *width = frame.right - frame.left;
  *height = frame.bottom - frame.top;
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

void *
BCursor_create_default (void)
{
  return new BCursor (B_CURSOR_ID_SYSTEM_DEFAULT);
}

void *
BCursor_create_modeline (void)
{
  return new BCursor (B_CURSOR_ID_CONTEXT_MENU);
}

void *
BCursor_from_id (enum haiku_cursor cursor)
{
  return new BCursor ((enum BCursorID) cursor);
}

void *
BCursor_create_i_beam (void)
{
  return new BCursor (B_CURSOR_ID_I_BEAM);
}

void *
BCursor_create_progress_cursor (void)
{
  return new BCursor (B_CURSOR_ID_PROGRESS);
}

void *
BCursor_create_grab (void)
{
  return new BCursor (B_CURSOR_ID_GRAB);
}

void
BCursor_delete (void *cursor)
{
  delete (BCursor *) cursor;
}

void
BView_set_view_cursor (void *view, void *cursor)
{
  if (!((BView *) view)->LockLooper ())
    gui_abort ("Failed to lock view setting cursor");
  ((BView *) view)->SetViewCursor ((BCursor *) cursor);
  ((BView *) view)->UnlockLooper ();
}

void
BWindow_Flush (void *window)
{
  ((BWindow *) window)->Flush ();
}

/* Map the keycode KC, storing the result in CODE and 1 in
   NON_ASCII_P if it should be used.  */
void
BMapKey (uint32_t kc, int *non_ascii_p, unsigned *code)
{
  if (*code == 10 && kc != 0x42)
    {
      *code = XK_Return;
      *non_ascii_p = 1;
      return;
    }

  switch (kc)
    {
    default:
      *non_ascii_p = 0;
      if (kc < 0xe && kc > 0x1)
	{
	  *code = XK_F1 + kc - 2;
	  *non_ascii_p = 1;
	}
      return;
    case 0x1e:
      *code = XK_BackSpace;
      break;
    case 0x61:
      *code = XK_Left;
      break;
    case 0x63:
      *code = XK_Right;
      break;
    case 0x57:
      *code = XK_Up;
      break;
    case 0x62:
      *code = XK_Down;
      break;
    case 0x64:
      *code = XK_Insert;
      break;
    case 0x65:
      *code = XK_Delete;
      break;
    case 0x37:
      *code = XK_Home;
      break;
    case 0x58:
      *code = XK_End;
      break;
    case 0x39:
      *code = XK_Page_Up;
      break;
    case 0x5a:
      *code = XK_Page_Down;
      break;
    case 0x1:
      *code = XK_Escape;
      break;
    case 0x68:
      *code = XK_Menu;
      break;
    }
  *non_ascii_p = 1;
}

/* Make a scrollbar, attach it to VIEW's window, and return it.  */
void *
BScrollBar_make_for_view (void *view, int horizontal_p,
			  int x, int y, int x1, int y1,
			  void *scroll_bar_ptr)
{
  EmacsScrollBar *sb = new EmacsScrollBar (x, y, x1, y1, horizontal_p);
  sb->scroll_bar = scroll_bar_ptr;

  BView *vw = (BView *) view;
  BView *sv = (BView *) sb;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock scrollbar owner");
  vw->AddChild ((BView *) sb);
  sv->WindowActivated (vw->Window ()->IsActive ());
  vw->UnlockLooper ();
  return sb;
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
  vw->Flush ();
  vw->Sync ();
  vw->UnlockLooper ();
}

void
BView_scroll_bar_update (void *sb, int portion, int whole, int position)
{
  BScrollBar *bar = (BScrollBar *) sb;
  BMessage msg = BMessage (SCROLL_BAR_UPDATE);
  BMessenger mr = BMessenger (bar);
  msg.AddInt32 ("emacs:range", whole);
  msg.AddInt32 ("emacs:units", position);

  mr.SendMessage (&msg);
}

/* Return the default scrollbar size.  */
int
BScrollBar_default_size (int horizontal_p)
{
  return horizontal_p ? B_H_SCROLL_BAR_HEIGHT : B_V_SCROLL_BAR_WIDTH;
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
   drawing.  `BView_draw_unlock' should be called afterwards.  */
void
BView_draw_lock (void *view)
{
  EmacsView *vw = (EmacsView *) view;
  if (vw->looper_locked_count)
    {
      vw->looper_locked_count++;
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
  vw->looper_locked_count++;
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

/* Tell VIEW it has been clicked at X by Y.  */
void
BView_mouse_down (void *view, int x, int y)
{
  BView *vw = (BView *) view;
  if (vw->LockLooper ())
    {
      vw->MouseDown (BPoint (x, y));
      vw->UnlockLooper ();
    }
}

/* Tell VIEW the mouse has been released at X by Y.  */
void
BView_mouse_up (void *view, int x, int y)
{
  BView *vw = (BView *) view;
  if (vw->LockLooper ())
    {
      vw->MouseUp (BPoint (x, y));
      vw->UnlockLooper ();
    }
}

/* Tell VIEW that the mouse has moved to Y by Y.  */
void
BView_mouse_moved (void *view, int x, int y, uint32_t transit)
{
  BView *vw = (BView *) view;
  if (vw->LockLooper ())
    {
      vw->MouseMoved (BPoint (x, y), transit, NULL);
      vw->UnlockLooper ();
    }
}

/* Import BITS into BITMAP using the B_GRAY1 colorspace.  */
void
BBitmap_import_mono_bits (void *bitmap, void *bits, int wd, int h)
{
  BBitmap *bmp = (BBitmap *) bitmap;
  unsigned char *data = (unsigned char *) bmp->Bits ();
  unsigned short *bts = (unsigned short *) bits;

  for (int i = 0; i < (h * (wd / 8)); i++)
    {
      *((unsigned short *) data) = bts[i];
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
  BWindow *w = (BWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while changing its decorations");
  if (decorate_p)
    w->SetLook (B_TITLED_WINDOW_LOOK);
  else
    w->SetLook (B_NO_BORDER_WINDOW_LOOK);
  w->UnlockLooper ();
}

/* Decorate WINDOW appropriately for use as a tooltip.  */
void
BWindow_set_tooltip_decoration (void *window)
{
  BWindow *w = (BWindow *) window;
  if (!w->LockLooper ())
    gui_abort ("Failed to lock window while setting ttip decoration");
  w->SetLook (B_BORDERED_WINDOW_LOOK);
  w->SetFeel (B_FLOATING_APP_WINDOW_FEEL);
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
  w->Sync ();
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

/* Return the current workspace.  */
uint32_t
haiku_current_workspace (void)
{
  return current_workspace ();
}

/* Return a bitmask consisting of workspaces WINDOW is on.  */
uint32_t
BWindow_workspaces (void *window)
{
  return ((BWindow *) window)->Workspaces ();
}

/* Create a popup menu.  */
void *
BPopUpMenu_new (const char *name)
{
  BPopUpMenu *menu = new EmacsPopUpMenu (name);
  menu->SetRadioMode (0);
  return menu;
}

/* Add a title item to MENU.  These items cannot be highlighted or
   triggered, and their labels will display as bold text.  */
void
BMenu_add_title (void *menu, const char *text)
{
  EmacsTitleMenuItem *it = new EmacsTitleMenuItem (text);
  BMenu *mn = (BMenu *) menu;
  mn->AddItem (it);
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
BMenu_run (void *menu, int x, int y)
{
  BPopUpMenu *mn = (BPopUpMenu *) menu;
  mn->SetRadioMode (0);
  BMenuItem *it = mn->Go (BPoint (x, y));
  if (it)
    {
      BMessage *mg = it->Message ();
      if (mg)
	return (void *) mg->GetPointer ("menuptr");
      else
	return NULL;
    }
  return NULL;
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
  if (!p->LockLooper ())
    gui_abort ("Failed to lock menu bar parent while removing menubar");
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

/* Run ALERT, returning the number of the button that was selected,
   or -1 if no button was selected before the alert was closed.  */
int32_t
BAlert_go (void *alert)
{
  return ((BAlert *) alert)->Go ();
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
BView_set_and_show_sticky_tooltip (void *view, const char *tooltip,
				   int x, int y)
{
  BToolTip *tip;
  BView *vw = (BView *) view;
  if (!vw->LockLooper ())
    gui_abort ("Failed to lock view while showing sticky tooltip");
  vw->SetToolTip (tooltip);
  tip = vw->ToolTip ();
  BPoint pt;
  EmacsView *ev = dynamic_cast<EmacsView *> (vw);
  if (ev)
    ev->tt_absl_pos = BPoint (x, y);

  vw->GetMouse (&pt, NULL, 1);
  pt.x -= x;
  pt.y -= y;

  pt.x = -pt.x;
  pt.y = -pt.y;

  tip->SetMouseRelativeLocation (pt);
  tip->SetSticky (1);
  vw->ShowToolTip (tip);
  vw->UnlockLooper ();
}

/* Delete ALERT.  */
void
BAlert_delete (void *alert)
{
  delete (BAlert *) alert;
}

/* Place the resolution of the monitor in DPI in RSSX and RSSY.  */
void
BScreen_res (double *rrsx, double *rrsy)
{
  BScreen s (B_MAIN_SCREEN_ID);
  if (!s.IsValid ())
    gui_abort ("Invalid screen for resolution checks");
  monitor_info i;

  if (s.GetMonitorInfo (&i) == B_OK)
    {
      *rrsx = (double) i.width / (double) 2.54;
      *rrsy = (double) i.height / (double) 2.54;
    }
  else
    {
      *rrsx = 72.27;
      *rrsy = 72.27;
    }
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
	std::strncpy (version, versionInfo.short_info, len - 1);
    }
}

/* Return the amount of color planes in the current display.  */
int
be_get_display_planes (void)
{
  color_space space = dpy_color_space;
  if (space == B_NO_COLOR_SPACE)
    {
      BScreen screen; /* This is actually a very slow operation.  */
      if (!screen.IsValid ())
	gui_abort ("Invalid screen");
      space = dpy_color_space = screen.ColorSpace ();
    }

  if (space == B_RGB32 || space == B_RGB24)
    return 24;
  if (space == B_RGB16)
    return 16;
  if (space == B_RGB15)
    return 15;
  if (space == B_CMAP8)
    return 8;

  gui_abort ("Bad colorspace for screen");
  /* https://www.haiku-os.org/docs/api/classBScreen.html
     says a valid screen can't be anything else.  */
  return -1;
}

/* Return the amount of colors the display can handle.  */
int
be_get_display_color_cells (void)
{
  color_space space = dpy_color_space;
  if (space == B_NO_COLOR_SPACE)
    {
      BScreen screen;
      if (!screen.IsValid ())
	gui_abort ("Invalid screen");
      space = dpy_color_space = screen.ColorSpace ();
    }

  if (space == B_RGB32 || space == B_RGB24)
    return 1677216;
  if (space == B_RGB16)
    return 65536;
  if (space == B_RGB15)
    return 32768;
  if (space == B_CMAP8)
    return 256;

  gui_abort ("Bad colorspace for screen");
  return -1;
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

struct popup_file_dialog_data
{
  BMessage *msg;
  BFilePanel *panel;
  BEntry *entry;
};

static void
unwind_popup_file_dialog (void *ptr)
{
  struct popup_file_dialog_data *data =
    (struct popup_file_dialog_data *) ptr;
  BFilePanel *panel = data->panel;
  delete panel;
  delete data->entry;
  delete data->msg;
}

static void
be_popup_file_dialog_safe_set_target (BFilePanel *dialog, BWindow *window)
{
  dialog->SetTarget (BMessenger (window));
}

/* Popup a file dialog.  */
char *
be_popup_file_dialog (int open_p, const char *default_dir, int must_match_p, int dir_only_p,
		      void *window, const char *save_text, const char *prompt,
		      void (*block_input_function) (void),
		      void (*unblock_input_function) (void))
{
  ptrdiff_t idx = c_specpdl_idx_from_cxx ();
  /* setjmp/longjmp is UB with automatic objects. */
  block_input_function ();
  BWindow *w = (BWindow *) window;
  uint32_t mode = dir_only_p ? B_DIRECTORY_NODE : B_FILE_NODE | B_DIRECTORY_NODE;
  BEntry *path = new BEntry;
  BMessage *msg = new BMessage ('FPSE');
  BFilePanel *panel = new BFilePanel (open_p ? B_OPEN_PANEL : B_SAVE_PANEL,
				      NULL, NULL, mode);
  unblock_input_function ();

  struct popup_file_dialog_data dat;
  dat.entry = path;
  dat.msg = msg;
  dat.panel = panel;

  record_c_unwind_protect_from_cxx (unwind_popup_file_dialog, &dat);
  if (default_dir)
    {
      if (path->SetTo (default_dir, 0) != B_OK)
	default_dir = NULL;
    }

  panel->SetMessage (msg);
  if (default_dir)
    panel->SetPanelDirectory (path);
  if (save_text)
    panel->SetSaveText (save_text);
  panel->SetHideWhenDone (0);
  panel->Window ()->SetTitle (prompt);
  be_popup_file_dialog_safe_set_target (panel, w);

  panel->Show ();
  panel->Window ()->Show ();

  void *buf = alloca (200);
  while (1)
    {
      enum haiku_event_type type;
      char *ptr = NULL;

      if (!haiku_read_with_timeout (&type, buf, 200, 100000))
	{
	  if (type != FILE_PANEL_EVENT)
	    haiku_write (type, buf);
	  else if (!ptr)
	    ptr = (char *) ((struct haiku_file_panel_event *) buf)->ptr;
	}

      ssize_t b_s;
      haiku_read_size (&b_s);
      if (!b_s || b_s == -1 || ptr || panel->Window ()->IsHidden ())
	{
	  c_unbind_to_nil_from_cxx (idx);
	  return ptr;
	}
    }
}

void
be_app_quit (void)
{
  if (be_app)
    {
      status_t e;
      while (!be_app->Lock ());
      be_app->Quit ();
      wait_for_thread (app_thread, &e);
    }
}

/* Temporarily fill VIEW with COLOR.  */
void
EmacsView_do_visible_bell (void *view, uint32_t color)
{
  EmacsView *vw = (EmacsView *) view;
  vw->DoVisibleBell (color);
}

/* Zoom WINDOW.  */
void
BWindow_zoom (void *window)
{
  BWindow *w = (BWindow *) window;
  w->Zoom ();
}

/* Make WINDOW fullscreen if FULLSCREEN_P.  */
void
EmacsWindow_make_fullscreen (void *window, int fullscreen_p)
{
  EmacsWindow *w = (EmacsWindow *) window;
  w->MakeFullscreen (fullscreen_p);
}

/* Unzoom (maximize) WINDOW.  */
void
EmacsWindow_unzoom (void *window)
{
  EmacsWindow *w = (EmacsWindow *) window;
  w->UnZoom ();
}

/* Move the pointer into MBAR and start tracking.  */
void
BMenuBar_start_tracking (void *mbar)
{
  EmacsMenuBar *mb = (EmacsMenuBar *) mbar;
  if (!mb->LockLooper ())
    gui_abort ("Couldn't lock menubar");
  BRect frame = mb->Frame ();
  BPoint pt = frame.LeftTop ();
  BPoint l = pt;
  mb->Parent ()->ConvertToScreen (&pt);
  set_mouse_position (pt.x, pt.y);
  mb->MouseDown (l);
  mb->UnlockLooper ();
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
/* Return VIEW's cairo surface.  */
cairo_surface_t *
EmacsView_cairo_surface (void *view)
{
  EmacsView *vw = (EmacsView *) view;
  EmacsWindow *wn = (EmacsWindow *) vw->Window ();
  return vw->cr_surface ? vw->cr_surface : wn->cr_surface;
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
      cairo_rectangle (ctx, r.left, r.top, r.Width () + 1,
		       r.Height () + 1);
    }

  cairo_clip (ctx);
}

/* Lock WINDOW in preparation for drawing using Cairo.  */
void
EmacsWindow_begin_cr_critical_section (void *window)
{
  EmacsWindow *w = (EmacsWindow *) window;
  if (!w->surface_lock.Lock ())
    gui_abort ("Couldn't lock cairo surface");

  BView *vw = (BView *) w->FindView ("Emacs");
  EmacsView *ev = dynamic_cast <EmacsView *> (vw);
  if (ev && !ev->cr_surface_lock.Lock ())
    gui_abort ("Couldn't lock view cairo surface");
}

/* Unlock WINDOW in preparation for drawing using Cairo.  */
void
EmacsWindow_end_cr_critical_section (void *window)
{
  EmacsWindow *w = (EmacsWindow *) window;
  w->surface_lock.Unlock ();
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
void
BWindow_set_min_size (void *window, int width, int height)
{
  BWindow *w = (BWindow *) window;

  if (!w->LockLooper ())
    gui_abort ("Failed to lock window looper setting min size");
  w->SetSizeLimits (width, -1, height, -1);
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
