/* Implementation of GUI terminal on the Microsoft W32 API.
   Copyright (C) 1989, 93, 94, 95, 96, 97, 98 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <signal.h>
#include <config.h>
#include <stdio.h>
#include "lisp.h"
#include "charset.h"
#include "fontset.h"
#include "blockinput.h"

#include "w32heap.h"
#include "w32term.h"
#include <shellapi.h>

#include "systty.h"
#include "systime.h"

#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#include "frame.h"
#include "dispextern.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "gnu.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "coding.h"

#undef min
#undef max
#define min(x, y) (((x) < (y)) ? (x) : (y))
#define max(x, y) (((x) > (y)) ? (x) : (y))

#define CP_DEFAULT 1004

extern unsigned int msh_mousewheel;

extern void free_frame_menubar ();

extern Lisp_Object Vwindow_system;

#define x_any_window_to_frame x_window_to_frame
#define x_top_window_to_frame x_window_to_frame


/* This is display since w32 does not support multiple ones.  */
struct w32_display_info one_w32_display_info;

/* This is a list of cons cells, each of the form (NAME . FONT-LIST-CACHE),
   one for each element of w32_display_list and in the same order.
   NAME is the name of the frame.
   FONT-LIST-CACHE records previous values returned by x-list-fonts.  */
Lisp_Object w32_display_name_list;

/* Frame being updated by update_frame.  This is declared in term.c.
   This is set by update_begin and looked at by all the
   w32 functions.  It is zero while not inside an update.
   In that case, the w32 functions assume that `selected_frame'
   is the frame to apply to.  */
extern struct frame *updating_frame;

/* This is a frame waiting to be autoraised, within w32_read_socket.  */
struct frame *pending_autoraise_frame;

/* During an update, maximum vpos for ins/del line operations to affect.  */

static int flexlines;

/* During an update, nonzero if chars output now should be highlighted.  */

static int highlight;

/* Nominal cursor position -- where to draw output.
   During an update, these are different from the cursor-box position.  */

static int curs_x;
static int curs_y;

/* Flag to enable Unicode output in case users wish to use programs
   like Twinbridge on '95 rather than installed system level support
   for Far East languages.  */
int w32_enable_unicode_output;

DWORD dwWindowsThreadId = 0;
HANDLE hWindowsThread = NULL;
DWORD dwMainThreadId = 0;
HANDLE hMainThread = NULL;

#ifndef SIF_ALL
/* These definitions are new with Windows 95. */
#define SIF_RANGE           0x0001
#define SIF_PAGE            0x0002
#define SIF_POS             0x0004
#define SIF_DISABLENOSCROLL 0x0008
#define SIF_TRACKPOS        0x0010
#define SIF_ALL             (SIF_RANGE | SIF_PAGE | SIF_POS | SIF_TRACKPOS)

typedef struct tagSCROLLINFO
{
    UINT    cbSize;
    UINT    fMask;
    int     nMin;
    int     nMax;
    UINT    nPage;
    int     nPos;
    int     nTrackPos;
}   SCROLLINFO, FAR *LPSCROLLINFO;
typedef SCROLLINFO CONST FAR *LPCSCROLLINFO;
#endif /* SIF_ALL */

/* Dynamic linking to new proportional scroll bar functions. */
int (PASCAL *pfnSetScrollInfo) (HWND hwnd, int fnBar, LPSCROLLINFO lpsi, BOOL fRedraw);
BOOL (PASCAL *pfnGetScrollInfo) (HWND hwnd, int fnBar, LPSCROLLINFO lpsi);

int vertical_scroll_bar_min_handle;
int vertical_scroll_bar_top_border;
int vertical_scroll_bar_bottom_border;

int last_scroll_bar_drag_pos;

/* Mouse movement. */

/* Where the mouse was last time we reported a mouse event.  */
static FRAME_PTR last_mouse_frame;
static RECT last_mouse_glyph;

Lisp_Object Vw32_num_mouse_buttons;

Lisp_Object Vw32_swap_mouse_buttons;

/* Control whether x_raise_frame also sets input focus.  */
Lisp_Object Vw32_grab_focus_on_raise;

/* Control whether Caps Lock affects non-ascii characters.  */
Lisp_Object Vw32_capslock_is_shiftlock;

/* Control whether right-alt and left-ctrl should be recognized as AltGr.  */
Lisp_Object Vw32_recognize_altgr;

/* The scroll bar in which the last motion event occurred.

   If the last motion event occurred in a scroll bar, we set this
   so w32_mouse_position can know whether to report a scroll bar motion or
   an ordinary motion.

   If the last motion event didn't occur in a scroll bar, we set this
   to Qnil, to tell w32_mouse_position to return an ordinary motion event.  */
Lisp_Object last_mouse_scroll_bar;
int last_mouse_scroll_bar_pos;

/* This is a hack.  We would really prefer that w32_mouse_position would
   return the time associated with the position it returns, but there
   doesn't seem to be any way to wrest the timestamp from the server
   along with the position query.  So, we just keep track of the time
   of the last movement we received, and return that in hopes that
   it's somewhat accurate.  */
Time last_mouse_movement_time;

/* Associative list linking character set strings to Windows codepages. */
Lisp_Object Vw32_charset_to_codepage_alist;

/* Incremented by w32_read_socket whenever it really tries to read events.  */
#ifdef __STDC__
static int volatile input_signal_count;
#else
static int input_signal_count;
#endif

extern Lisp_Object Vcommand_line_args, Vsystem_name;

extern Lisp_Object Qface, Qmouse_face;

extern int errno;

/* A mask of extra modifier bits to put into every keyboard char.  */
extern int extra_keyboard_modifiers;

static Lisp_Object Qvendor_specific_keysyms;

void w32_delete_display ();

static void redraw_previous_char ();
static void redraw_following_char ();
static unsigned int w32_get_modifiers ();

static int fast_find_position ();
static void note_mouse_highlight ();
static void clear_mouse_face ();
static void show_mouse_face ();
static void do_line_dance ();

/* Forward declarations for term hooks.  Consistency with the rest of Emacs
   requires the use of K&R functions prototypes.  However, MSVC does not
   pick up the function prototypes correctly with K&R function definitions,
   and so we declare them first to give a little help to MSVC.  */
static void w32_clear_frame ();
static void w32_clear_end_of_line (int);
static void w32_ins_del_lines (int, int);
static void w32_change_line_highlight (int, int, int);
static void w32_insert_glyphs (GLYPH *, int);
static void w32_write_glyphs (GLYPH *, int);
static void w32_delete_glyphs (int);
static void w32_ring_bell ();
static void w32_reset_terminal_modes ();
static void w32_set_terminal_modes ();
static void w32_update_begin (FRAME_PTR);
static void w32_update_end (FRAME_PTR);
static void w32_set_terminal_window (int);
extern int  w32_read_socket (int, struct input_event *, int, int);
static void w32_frame_up_to_date (FRAME_PTR);
static void w32_cursor_to (int, int);
static void w32_reassert_line_highlight (int, int);
static void w32_mouse_position (FRAME_PTR *, int, Lisp_Object *,
		enum scroll_bar_part *, Lisp_Object *,
		Lisp_Object *, unsigned long *);
static void w32_frame_rehighlight (FRAME_PTR);
static void w32_frame_raise_lower (FRAME_PTR, int);
static void w32_set_vertical_scroll_bar (struct window *, int, int, int);
static void w32_condemn_scroll_bars (FRAME_PTR);
static void w32_redeem_scroll_bar (struct window *);
static void w32_judge_scroll_bars (FRAME_PTR);

#if 0
/* This is a function useful for recording debugging information
   about the sequence of occurrences in this file.  */

struct record 
{
  char *locus;
  int type;
};

struct record event_record[100];

int event_record_index;

record_event (locus, type)
     char *locus;
     int type;
{
  if (event_record_index == sizeof (event_record) / sizeof (struct record))
    event_record_index = 0;

  event_record[event_record_index].locus = locus;
  event_record[event_record_index].type = type;
  event_record_index++;
}

#endif /* 0 */

/* Return the struct w32_display_info.  */

struct w32_display_info *
w32_display_info_for_display ()
{
  return (&one_w32_display_info);
}

void 
w32_fill_rect (f, _hdc, pix, lprect)
     FRAME_PTR f;
     HDC _hdc;
     COLORREF pix;
     RECT * lprect;
{
  HDC hdc;
  HBRUSH hb;
  RECT rect;
  
  if (_hdc)
    hdc = _hdc;
  else 
    {
      if (!f) return;
      hdc = get_frame_dc (f);
    }
  
  hb = CreateSolidBrush (pix);
  FillRect (hdc, lprect, hb);
  DeleteObject (hb);
  
  if (!_hdc)
    release_frame_dc (f, hdc);
}

void 
w32_clear_window (f)
     FRAME_PTR f;
{
  RECT rect;

  GetClientRect (FRAME_W32_WINDOW (f), &rect);
  w32_clear_rect (f, NULL, &rect);
}


/* Starting and ending updates.

   These hooks are called by update_frame at the beginning and end
   of a frame update.  We record in `updating_frame' the identity
   of the frame being updated, so that the w32_... functions do not
   need to take a frame as argument.  Most of the w32_... functions
   should never be called except during an update, the only exceptions
   being w32_cursor_to, w32_write_glyphs and w32_reassert_line_highlight.  */

static void
w32_update_begin (f)
     struct frame *f;
{
  if (f == 0)
    abort ();

  flexlines = f->height;
  highlight = 0;

  BLOCK_INPUT;

  /* Regenerate display palette before drawing if list of requested
     colors has changed. */
  if (FRAME_W32_DISPLAY_INFO (f)->regen_palette)
  {
    w32_regenerate_palette (f);
    FRAME_W32_DISPLAY_INFO (f)->regen_palette = FALSE;
  }

  if (f == FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    {
      /* Don't do highlighting for mouse motion during the update.  */
      FRAME_W32_DISPLAY_INFO (f)->mouse_face_defer = 1;

      /* If the frame needs to be redrawn,
	 simply forget about any prior mouse highlighting.  */
      if (FRAME_GARBAGED_P (f))
	FRAME_W32_DISPLAY_INFO (f)->mouse_face_window = Qnil;

      if (!NILP (FRAME_W32_DISPLAY_INFO (f)->mouse_face_window))
	{
	  int firstline, lastline, i;
	  struct window *w = XWINDOW (FRAME_W32_DISPLAY_INFO (f)->mouse_face_window);

	  /* Find the first, and the last+1, lines affected by redisplay.  */
	  for (firstline = 0; firstline < f->height; firstline++)
	    if (FRAME_DESIRED_GLYPHS (f)->enable[firstline])
	      break;

	  lastline = f->height;
	  for (i = f->height - 1; i >= 0; i--)
	    {
	      if (FRAME_DESIRED_GLYPHS (f)->enable[i])
		break;
	      else
		lastline = i;
	    }

	  /* Can we tell that this update does not affect the window
	     where the mouse highlight is?  If so, no need to turn off.
	     Likewise, don't do anything if the frame is garbaged;
	     in that case, the FRAME_CURRENT_GLYPHS that we would use
	     are all wrong, and we will redisplay that line anyway.  */
	  if (! (firstline > (XFASTINT (w->top) + window_internal_height (w))
		 || lastline < XFASTINT (w->top)))
	    clear_mouse_face (FRAME_W32_DISPLAY_INFO (f));
	}
    }

  UNBLOCK_INPUT;
}

static void
w32_update_end (f)
     struct frame *f;
{
  BLOCK_INPUT;

  do_line_dance ();
  x_display_cursor (f, 1);

  if (f == FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    FRAME_W32_DISPLAY_INFO (f)->mouse_face_defer = 0;

  UNBLOCK_INPUT;
}

/* This is called after a redisplay on frame F.  */

static void
w32_frame_up_to_date (f)
     FRAME_PTR f;
{
  BLOCK_INPUT;
  if (FRAME_W32_DISPLAY_INFO (f)->mouse_face_deferred_gc
      || f == FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_frame)
    {
      note_mouse_highlight (FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_frame,
			    FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_x,
			    FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_y);
      FRAME_W32_DISPLAY_INFO (f)->mouse_face_deferred_gc = 0;
    }
  UNBLOCK_INPUT;
}

/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

static void
w32_reassert_line_highlight (new, vpos)
     int new, vpos;
{
  highlight = new;
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

static void
w32_change_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  highlight = new_highlight;
  w32_cursor_to (vpos, 0);
  w32_clear_end_of_line (updating_frame->width);
}

/* This is used when starting Emacs and when restarting after suspend.
   When starting Emacs, no window is mapped.  And nothing must be done
   to Emacs's own window if it is suspended (though that rarely happens).  */

static void
w32_set_terminal_modes (void)
{
}

/* This is called when exiting or suspending Emacs.
   Exiting will make the W32 windows go away, and suspending
   requires no action.  */

static void
w32_reset_terminal_modes (void)
{
}

/* Set the nominal cursor position of the frame.
   This is where display update commands will take effect.
   This does not affect the place where the cursor-box is displayed.  */

static void
w32_cursor_to (row, col)
     register int row, col;
{
  int orow = row;

  curs_x = col;
  curs_y = row;

  if (updating_frame == 0)
    {
      BLOCK_INPUT;
      x_display_cursor (selected_frame, 1);
      UNBLOCK_INPUT;
    }
}

/* Get the Windows codepage corresponding to the specified font.  The
   charset info in the font name is used to look up
   w32-charset-to-codepage-alist.  */
int 
w32_codepage_for_font (char *fontname)
{
  Lisp_Object codepage;
  char charset_str[20], *charset, *end;

  /* Extract charset part of font string.  */
  if (sscanf (fontname,
              "-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%*[^-]-%19s",
              charset_str) == EOF)
    return CP_DEFAULT;

  /* Remove leading "*-".  */
  if (strncmp ("*-", charset_str, 2) == 0)
    charset = charset_str + 2;
  else
    charset = charset_str;

  /* Stop match at wildcard (including preceding '-'). */
  if (end = strchr (charset, '*'))
      {
        if (end > charset && *(end-1) == '-')
          end--;
        *end = '\0';
      }

  codepage = Fcdr (Fassoc (build_string(charset),
                           Vw32_charset_to_codepage_alist));

  if (INTEGERP (codepage))
    return XINT (codepage);
  else
    return CP_DEFAULT;
}

BOOL 
w32_use_unicode_for_codepage (codepage)
{
  /* If the current codepage is supported, use Unicode for output. */
  return (w32_enable_unicode_output
          && codepage != CP_DEFAULT && IsValidCodePage (codepage));
}

/* Dealing with bits of wchar_t as if they were an XChar2B.  */
#define BUILD_WCHAR_T(byte1, byte2) \
 ((wchar_t)(((byte1 & 0x00ff) << 8) | (byte2 & 0x00ff)))


#define BYTE1(ch) \
 ((ch & 0xff00) >> 8)

#define BYTE2(ch) \
 (ch & 0x00ff)

#define W32_TEXTOUT(start_offset,nchars)                             \
{                                                                    \
  int charset_dim = CHARSET_DIMENSION(charset);                      \
  if (font->bdf)                                                     \
    w32_BDF_TextOut (font->bdf, hdc, left + xoffset,                 \
                     top + yoffset,                                  \
                     x_1byte_buffer + start_offset,                  \
                     charset_dim, nchars, 0);                        \
  else if (print_via_unicode)                                        \
    ExtTextOutW (hdc, left + xoffset, top + yoffset,                 \
                 fuOptions, clip_region,                             \
                 x_2byte_buffer + start_offset, nchars, NULL);       \
  else                                                               \
    ExtTextOut (hdc, left + xoffset, top + yoffset,                  \
                fuOptions, clip_region,                              \
                x_1byte_buffer + start_offset,                       \
                nchars * charset_dim, NULL);                         \
    start_offset += nchars * (print_via_unicode ? 1 : charset_dim ); \
    xoffset += nchars * glyph_width;                                 \
}

/* Display a sequence of N glyphs found at GP.
   WINDOW is the window to output to.  LEFT and TOP are starting coords.
   HL is 1 if this text is highlighted, 2 if the cursor is on it,
   3 if should appear in its mouse-face.
   JUST_FOREGROUND if 1 means draw only the foreground;
   don't alter the background.

   FONT is the default font to use (for glyphs whose font-code is 0).

   Since the display generation code is responsible for calling
   compute_char_face and compute_glyph_face on everything it puts in
   the display structure, we can assume that the face code on each
   glyph is a valid index into FRAME_COMPUTED_FACES (f), and the one
   to which we can actually apply intern_face.
   Call this function with input blocked.  */

static int
dumpglyphs (f, left, top, gp, n, hl, just_foreground, cmpcharp)
     struct frame *f;
     int left, top;
     register GLYPH *gp; /* Points to first GLYPH. */
     register int n;  /* Number of glyphs to display. */
     int hl;
     int just_foreground;
     struct cmpchar_info *cmpcharp;
{
  wchar_t *x_2byte_buffer
    = (wchar_t *) alloca (FRAME_WINDOW_WIDTH (f) * sizeof (*x_2byte_buffer));
  register wchar_t *cp;         /* Steps through x_2byte_buffer[].  */

  /* Allocate double the window width, as this buffer may contain MBCS
     characters under w32.  Unsigned to let GetCharABCWidths work.  */
  unsigned char *x_1byte_buffer
    = (unsigned char *) alloca (2 * FRAME_WINDOW_WIDTH (f)
                                * sizeof (*x_1byte_buffer));
  register unsigned char *bp;       /* Steps through x_1byte_buffer[]. */ 
  register int tlen = GLYPH_TABLE_LENGTH;
  register Lisp_Object *tbase = GLYPH_TABLE_BASE;
  Window window = FRAME_W32_WINDOW (f);
  HDC hdc = get_frame_dc (f);
  int orig_left = left;
  int gidx = 0;
  int i;

  while (n > 0)
    {
      /* Get the face-code of the next GLYPH.  */
      int cf, len, n_chars;
      GLYPH g = *gp;
      int ch, charset;
      Lisp_Object first_ch;
      /* HIGHEST and LOWEST are used while drawing a composite
         character.  The meanings are described later.  */
      int highest, lowest;

      GLYPH_FOLLOW_ALIASES (tbase, tlen, g);
      cf = (cmpcharp ? cmpcharp->face_work : FAST_GLYPH_FACE (g));
      ch = FAST_GLYPH_CHAR (g);
      if (unibyte_display_via_language_environment
	  && SINGLE_BYTE_CHAR_P (ch)
	  && ch >= 160)
	ch = unibyte_char_to_multibyte (ch);
      if (gidx == 0) XSETFASTINT (first_ch, ch);
      charset = CHAR_CHARSET (ch);
      if (charset == CHARSET_COMPOSITION)
	{
	  /* We must draw components of the composite character on the
             same column.  */
	  cmpcharp = cmpchar_table[COMPOSITE_CHAR_ID (ch)];

	  /* Set the face in the slot for work. */
	  cmpcharp->face_work = cf;

	  /* We don't need the return value ... */
	  dumpglyphs (f, left, top, cmpcharp->glyph, cmpcharp->glyph_len,
		      hl, just_foreground, cmpcharp);
	  /* ... because the width of just drawn text can be
             calculated as follows.  */
	  left += FONT_WIDTH (FRAME_FONT (f)) * cmpcharp->width;

	  ++gp, --n;
	  while (gp && (*gp & GLYPH_MASK_PADDING)) ++gp, --n;
	  cmpcharp = NULL;
	  continue;
	}

      /* Find the run of consecutive glyphs which can be drawn with
	 the same DC (i.e. the same charset and the same face-code).
	 Extract their character codes into X_2BYTE_BUFFER.
	 If CMPCHARP is not NULL, face-code is not checked because we
	 use only the face specified in `cmpcharp->face_work'.  */
      cp = x_2byte_buffer;
      while (n > 0)
	{
	  int this_charset, c1, c2;

	  g = *gp;
	  GLYPH_FOLLOW_ALIASES (tbase, tlen, g);
	  ch = FAST_GLYPH_CHAR (g);
	  if (unibyte_display_via_language_environment
	      && SINGLE_BYTE_CHAR_P (ch)
	      && ch >= 160)
	    ch = unibyte_char_to_multibyte (ch);
	  SPLIT_CHAR (ch, this_charset, c1, c2);
	  if (this_charset != charset
	      || (cmpcharp == NULL && FAST_GLYPH_FACE (g) != cf))
	    break;

	  if (c2 > 0)
	    *cp = BUILD_WCHAR_T (c1, c2);
	  else
	    *cp = BUILD_WCHAR_T (0, c1);
	  ++cp;
	      ++gp, --n;
	  while (gp && (*gp & GLYPH_MASK_PADDING))
	    ++gp, --n;
	}

      /* LEN gets the length of the run.  */
      len = cp - x_2byte_buffer;
      /* Now output this run of chars, with the font and pixel values
	 determined by the face code CF.  */
      {
	struct face *face = FRAME_DEFAULT_FACE (f);
	XFontStruct *font = NULL;
        int fontset;
        struct font_info *fontp;
        int font_id;
	COLORREF fg;
	COLORREF bg;
	int stippled = 0;
	int line_height = FRAME_LINE_HEIGHT (f);
	/* Pixel width of each glyph in this run.  */
	int glyph_width
	  = (FONT_WIDTH (FRAME_FONT (f))
	     * (cmpcharp ? cmpcharp->width : CHARSET_WIDTH (charset)));
	/* Overall pixel width of this run.  */
	int run_width
	  = (FONT_WIDTH (FRAME_FONT (f))
	     * (cmpcharp ? cmpcharp->width : len * CHARSET_WIDTH (charset)));
	/* A flag to tell if we have already filled background.  We
	   fill background in advance in the following cases:
	   1) A face has stipple.
	   2) A height of font is shorter than LINE_HEIGHT.
	   3) Drawing a composite character.
	   4) Font has non-zero _MULE_BASELINE_OFFSET property.
           5) Font is a bdf font.
	   6) Font is italic (italic fonts falsely report their height).
	   After filling background, we draw glyphs by XDrawString16.  */
	int background_filled;
	/* Baseline position of a character, offset from TOP.  */
	int baseline;
	/* The property value of `_MULE_RELATIVE_COMPOSE' and
           `_MULE_DEFAULT_ASCENT'.  */
	int relative_compose = 0, default_ascent = 0;
	/* 1 if we find no font or a font of inappropriate size.  */
	int require_clipping;
        RECT clip_rectangle;
        LPRECT clip_region = NULL;
        UINT fuOptions = 0;

        int codepage = CP_DEFAULT;
        BOOL print_via_unicode = FALSE;

	/* HL = 3 means use a mouse face previously chosen.  */
	if (hl == 3)
	  cf = FRAME_W32_DISPLAY_INFO (f)->mouse_face_face_id;

	/* First look at the face of the text itself.  */
	if (cf != 0)
	  {
	    /* It's possible for the display table to specify
	       a face code that is out of range.  Use 0 in that case.  */
	    if (cf < 0 || cf >= FRAME_N_COMPUTED_FACES (f)
		|| FRAME_COMPUTED_FACES (f) [cf] == 0)
	      cf = 0;

	    if (cf == 1)
	      face = FRAME_MODE_LINE_FACE (f);
	    else
	      face = intern_face (f, FRAME_COMPUTED_FACES (f) [cf]);
	    if (FACE_STIPPLE (face))
	      stippled = 1;
	  }

	/* Then comes the distinction between modeline and normal text.  */
	else if (hl == 0)
	 ;
	else if (hl == 1)
	  {
	    face = FRAME_MODE_LINE_FACE (f);
	    if (FACE_STIPPLE (face))
	      stippled = 1;
	  }

        /* Setting appropriate font and codepage for this charset.  */
        if (charset != CHARSET_ASCII)
          {
            fontset = FACE_FONTSET (face);

	    if ((fontset < 0 && (fontset = FRAME_FONTSET (f)) < 0)
		|| !(fontp = FS_LOAD_FONT (f, FRAME_W32_FONT_TABLE (f),
					   charset, NULL, fontset)))
	      goto font_not_found;

	    font = (XFontStruct *) (fontp->font);
            codepage = w32_codepage_for_font (fontp->name);

            if ( font && !font->bdf )
              print_via_unicode = w32_use_unicode_for_codepage (codepage);

            baseline = FONT_BASE (font) + fontp->baseline_offset;

	    if (cmpcharp && cmpcharp->cmp_rule == NULL)
	      {
		relative_compose = fontp->relative_compose;
		default_ascent = fontp->default_ascent;
	      }

	    /* We have to change code points in the following cases. */
            if (fontp->font_encoder)
	      {
		/* This font requires CCL program to calculate code
                   point of characters.  */
		struct ccl_program *ccl = fontp->font_encoder;

		if (CHARSET_DIMENSION (charset) == 1)
		  for (cp = x_2byte_buffer; cp < x_2byte_buffer + len; cp++)
		    {
		      ccl->reg[0] = charset;
		      ccl->reg[1] = BYTE2 (*cp);
		      ccl_driver (ccl, NULL, NULL, 0, 0, NULL);
		      /* We assume that MSBs are appropriately
                         set/reset by CCL program.  */
#if 0 /* this probably works under NT, but not under 95.  */
		      if (font->tm.tmLastChar < 256)	/* 1-byte font */
			*cp = BUILD_WCHAR_T (0, ccl->reg[1]);
		      else
			*cp = BUILD_WCHAR_T (ccl->reg[1], ccl->reg[2]);
#else /* Assume single dimensional charsets stay so.  */
                      *cp = BUILD_WCHAR_T (0, ccl->reg[1]);
#endif
                    }
		else
		  for (cp = x_2byte_buffer; cp < x_2byte_buffer + len; cp++)
		    {
		      ccl->reg[0] = charset;
		      ccl->reg[1] = BYTE1 (*cp) , ccl->reg[2] = BYTE2 (*cp);
		      ccl_driver (ccl, NULL, NULL, 0, 0, NULL);
		      /* We assume that MSBs are appropriately
                         set/reset by CCL program.  */
#if 0 /* this probably works under NT, but not under 95.  */
		      if (font->tm.tmLastChar < 256)	/* 1-byte font */
			*cp = BUILD_WCHAR_T (0, ccl->reg[1]);
		      else
			*cp = BUILD_WCHAR_T (ccl->reg[1],ccl->reg[2]);
#else /* Assume multidimensional charsets stay so.  */
                      *cp = BUILD_WCHAR_T (ccl->reg[1],ccl->reg[2]);
#endif
		    }
	      }
	    else if (fontp->encoding[charset])
	      {
		int enc = fontp->encoding[charset];

		if ((enc == 1 || enc == 2) && CHARSET_DIMENSION (charset) == 2)
		  for (cp = x_2byte_buffer; cp < x_2byte_buffer + len; cp++)
		    *cp = BUILD_WCHAR_T (BYTE1 (*cp) | 0x80, BYTE2 (*cp));
		if (enc == 1 || enc == 3)
		  for (cp = x_2byte_buffer; cp < x_2byte_buffer + len; cp++)
		    *cp = BUILD_WCHAR_T (BYTE1 (*cp), BYTE2 (*cp) | 0x80);
                /* Special encoding for SJIS Kanji.  */
                if (enc == 4)
                  {
                    if (CHARSET_DIMENSION (charset) == 2)
                      {
                        int sjis1, sjis2;
                        for (cp = x_2byte_buffer;
                             cp < x_2byte_buffer + len; cp++)
                          {
                            ENCODE_SJIS (BYTE1 (*cp), BYTE2 (*cp),
                                         sjis1, sjis2);
                            *cp = BUILD_WCHAR_T (sjis1, sjis2);
                          }
                      }
                    else
                      for (cp = x_2byte_buffer;
                           cp < x_2byte_buffer + len; cp++)
                        *cp = BUILD_WCHAR_T (BYTE1 (*cp),
                                             BYTE2 (*cp) | 0x80);
                  }
	      }
	  }
	else
	  {
	  font_not_found:
	    if (charset == CHARSET_ASCII || charset == charset_latin_iso8859_1)
	      {
		font = FACE_FONT (face);
		if (!font || font == (XFontStruct *) FACE_DEFAULT)
		  font = FRAME_FONT (f);
		baseline = FONT_BASE (FRAME_FONT (f));
		if (charset == charset_latin_iso8859_1)
		  {
		    if (!font->bdf && font->tm.tmLastChar < 0x80)
		      /* This font can't display Latin1 characters.  */
		      font = NULL;
		    else
		      {
			for (cp = x_2byte_buffer;
                             cp < x_2byte_buffer + len; cp++)
			  *cp = BUILD_WCHAR_T (BYTE1 (*cp),
                                               BYTE2 (*cp) | 0x80);
		      }
		  }
	      }
	  }

	fg = face->foreground;
	bg = face->background;

	/* Now override that if the cursor's on this character.  */
	if (hl == 2)
	  {
	    /* The cursor overrides stippling.  */
	    stippled = 0;

	    if (font == FRAME_FONT (f)
		&& face->background == FRAME_BACKGROUND_PIXEL (f)
		&& face->foreground == FRAME_FOREGROUND_PIXEL (f)
		&& !cmpcharp)
	      {
		bg = f->output_data.w32->cursor_pixel;
		fg = face->background;
	      }
	    /* Cursor on non-default face: must merge.  */
	    else
	      {
		bg = f->output_data.w32->cursor_pixel;
		fg = face->background;
		/* If the glyph would be invisible,
		   try a different foreground.  */
		if (fg == bg)
		  fg = face->foreground;
		if (fg == bg)
		  fg = f->output_data.w32->cursor_foreground_pixel;
		if (fg == bg)
		  fg = face->foreground;
		/* Make sure the cursor is distinct from text in this face.  */
		if (bg == face->background
		    && fg == face->foreground)
		  {
		    bg = face->foreground;
		    fg = face->background;
		  }
	      }
	  }

	if (font)
	  require_clipping = (!NILP (Vclip_large_size_font)
	     && ((font->bdf
                  ? (font->bdf->ury > baseline
                     || font->bdf->lly > line_height - baseline)
                  : (font->tm.tmAscent > baseline
                     || font->tm.tmDescent > line_height - baseline))
                 || (!cmpcharp && FONT_MAX_WIDTH (font) > glyph_width)));

        if (font && (just_foreground || (cmpcharp && gidx > 0)))
          background_filled = 1;

        /* Stippling not supported under w32.  */

        else if (!font
                 || font->bdf
                 || FONT_HEIGHT (font) < line_height
                 || FONT_WIDTH (font) < glyph_width
                 || FONT_MAX_WIDTH (font) != FONT_WIDTH (font)
		 || font->tm.tmItalic
                 || cmpcharp)
          {
	    /* Fill in the background for the current run.  */
            w32_fill_area (f, hdc, bg,
                           left,
                           top,
                           run_width,
                           line_height);
            background_filled = 1;
            if (cmpcharp)
              /* To assure not to fill background while drawing
                 remaining components.  */
              just_foreground = 1;
	  }
        else
          background_filled = 0;

	SetBkMode (hdc, background_filled ? TRANSPARENT : OPAQUE);
	SetTextColor (hdc, fg);
	SetBkColor (hdc, bg);
        SetTextAlign (hdc, TA_BASELINE | TA_LEFT);

	/* On NT, where conversion to Unicode has to happen sometime
           when using the normal ExtTextOut facility, we might as well
           take advantage of x_2byte_buffer which is already allocated,
           to avoid the allocation overhead for implicit conversion.  */

	if (!print_via_unicode
	    && codepage == CP_DEFAULT
	    && w32_enable_unicode_output
	    && os_subtype == OS_NT
	    && font && !font->bdf)
	  {
	    print_via_unicode = TRUE;
	  }

	/* Note that we can special-case the conversion to Unicode when
	   the charset is CHARSET_ASCII (an important case) or Latin-1,
	   because x_2byte_buffer in fact already contains the unicode
	   characters.  So avoid setting up x_1byte_buffer in that case.  */
        if (!print_via_unicode
	    || (charset != CHARSET_ASCII && charset != charset_latin_iso8859_1))
	  {
	    /* Convert x_2byte_buffer into a buffer of single byte
	       characters - possibly containing MBCS runs.  */
	    bp = x_1byte_buffer;
	    for (i = 0; i < len; i++)
	      {
		if (BYTE1 (*(x_2byte_buffer + i)))
		  *bp++ = BYTE1 (*(x_2byte_buffer + i));
		*bp++ = BYTE2 (*(x_2byte_buffer + i));
	      }
	    n_chars = bp - x_1byte_buffer;
	  }
	else
	  n_chars = len;

        if (print_via_unicode
	    && charset != CHARSET_ASCII && charset != charset_latin_iso8859_1)
	  {
	    i = MultiByteToWideChar
	      (codepage, 0, x_1byte_buffer, n_chars,
	       x_2byte_buffer, FRAME_WINDOW_WIDTH (f));

	    /* Make sure we don't display nothing if conversion fails.  */
	    if (i == 0)
	      print_via_unicode = FALSE;
	    else
	      n_chars = i;
	  }

	if (font)
	  {
            if (font->hfont)
	      SelectObject (hdc, font->hfont);

            if (!cmpcharp)
	      {
                int xoffset = 0, yoffset = baseline;
                if (require_clipping || FONT_WIDTH (font) != glyph_width
                    || FONT_MAX_WIDTH (font) != FONT_WIDTH (font))
                  {
                    /* The incrementing of i in this loop is done
                       inside the W32_CHAROUT macro.  */
                    for (i = 0; i < n_chars; )
                      {
                        if (require_clipping)
                          {
                            /* Set up a clipping rectangle for ExtTextOut */
                            fuOptions |= ETO_CLIPPED;
                            clip_rectangle.left = left + i * glyph_width;
                            clip_rectangle.right
                              = left + (i + 1) * glyph_width;
                            clip_rectangle.top = top;
                            clip_rectangle.bottom = top + line_height;
                            clip_region = &clip_rectangle;
			  }
                        W32_TEXTOUT (i, 1);
                      }
		  }
                else
                  {
                    i = 0;
                    W32_TEXTOUT (i, n_chars);
                  }
              }
            else
              {
                /* Handle composite characters.  */
                RECT clip_rectangle;
                LPRECT clip_region = NULL;
                UINT fuOptions = 0;
                ABC char_placement;
                int char_width = 0;

                if (require_clipping)
		  {
                    /* Set up a clipping rectangle for ExtTextOut */
                    fuOptions |= ETO_CLIPPED;
                    clip_rectangle.left = left;
                    clip_rectangle.right = left + glyph_width;
                    clip_rectangle.top = top;
                    clip_rectangle.bottom = top + line_height;
                    clip_region = &clip_rectangle;
		  }
                if ((cmpcharp->cmp_rule || relative_compose)
                    && gidx == 0)
		  {
		    /* This is the first character.  Initialize variables.
		       HIGHEST is the highest position of glyphs ever
		       written, LOWEST the lowest position.  */
		    int xoffset = 0;
                    int yoffset = baseline;
                    int start = 0;

		    if (default_ascent
			&& CHAR_TABLE_P (Vuse_default_ascent)
			&& !NILP (Faref (Vuse_default_ascent, first_ch)))
		      {
			highest = default_ascent;
			lowest = 0;
		      }
                    /* TODO: per char metrics for Truetype and BDF
                       fonts.  */
		      {
                        highest = FONT_BASE (font) + 1;
                        lowest = - (FONT_HEIGHT (font) - FONT_BASE (font));
		      }

		    if (cmpcharp->cmp_rule)
		      xoffset = (int)(cmpcharp->col_offset[0]
				  * FONT_WIDTH (FRAME_FONT (f)));

                    i = 1;

                    /* Truetype fonts often contain underhangs to
                       handle composition characters. This works
                       against our attempts to position the characters
                       manually, so we need to compensate for this.
                    */
                    if (print_via_unicode ?
                        GetCharABCWidthsW (hdc, *x_2byte_buffer,
                                           *x_2byte_buffer,
                                           &char_placement)
                        : GetCharABCWidths (hdc, *x_1byte_buffer,
                                            *x_1byte_buffer,
                                            &char_placement))
                      {
                        char_width = char_placement.abcA
                          + char_placement.abcB + char_placement.abcC;
                        xoffset += FONT_WIDTH (font) - char_width;
                      }
                    /* Don't let characters go beyond the glyph
                       boundary whatever their over/underhangs. */
                    if (xoffset > glyph_width - char_width)
                      xoffset = glyph_width - char_width;

                    if (xoffset < 0)
                      xoffset = 0;

		    /* Draw the first character at the normal
                       position.  */
                    W32_TEXTOUT (start, 1);
                    gidx++;
                  }
                else
                  i = 0;

                for (; i < n_chars; gidx++)
		  {
		    int xoffset = 0, yoffset = FONT_BASE (font);

		    if (relative_compose)
		      {
			/* No per char metrics on w32.  */
			if (NILP (Vignore_relative_composition)
			    || NILP (Faref (Vignore_relative_composition,
					    make_number (cmpcharp->glyph[gidx]))))
			  {
			    if (- (FONT_HEIGHT (font) - FONT_BASE (font))
                                >= relative_compose)
			      {
				/* Draw above the current glyphs.  */
				yoffset = highest + FONT_HEIGHT (font);
				highest += FONT_HEIGHT (font);
			      }
			    else if (FONT_BASE (font) <= 0)
			      {
				/* Draw beneath the current glyphs.  */
				yoffset = lowest;
				lowest -= FONT_HEIGHT (font);
			      }
			  }
			else
			  {
			    /* Draw the glyph at normal position.  If
                               it sticks out of HIGHEST or LOWEST,
                               update them appropriately.  */
			    if (FONT_BASE (font) > highest)
			      highest = FONT_BASE (font);
			    else if (- (FONT_HEIGHT (font) - FONT_BASE (font))
                                     < lowest)
			      lowest = - (FONT_HEIGHT (font) -
                                          FONT_BASE (font));
			  }
		      }
		    else if (cmpcharp->cmp_rule)
		      {
			int gref = (cmpcharp->cmp_rule[gidx] - 0xA0) / 9;
			int nref = (cmpcharp->cmp_rule[gidx] - 0xA0) % 9;
			int bottom, top;

			/* Re-encode GREF and NREF so that they specify
			   only Y-axis information:
			   0:top, 1:base, 2:bottom, 3:center  */
			gref = gref / 3 + (gref == 4) * 2;
			nref = nref / 3 + (nref == 4) * 2;

                        /* No per char metrics on w32.  */
			bottom = ((gref == 0 ? highest : gref == 1 ? 0
				   : gref == 2 ? lowest
				   : (highest + lowest) / 2)
			  - (nref == 0 ? FONT_HEIGHT (font)
			   : nref == 1 ? (FONT_HEIGHT (font) -
                                          FONT_BASE (font))
                                     : nref == 2 ? 0
                           : (FONT_HEIGHT (font) / 2)));
			top = bottom + FONT_HEIGHT (font);

			if (top > highest)
			  highest = top;
			if (bottom < lowest)
			  lowest = bottom;
			yoffset = bottom + FONT_HEIGHT (font);
			xoffset = (int)(cmpcharp->col_offset[gidx]
				    * FONT_WIDTH (FRAME_FONT(f)));
		      }

                    /* Truetype fonts often contain underhangs to
                       handle composition characters. This works
                       against our attempts to position the characters
                       manually, so we need to compensate for this.
                    */
                    if (print_via_unicode ?
                        GetCharABCWidthsW (hdc, *(x_2byte_buffer + i),
                                           *(x_2byte_buffer + i),
                                           &char_placement)
                        : GetCharABCWidths (hdc, *(x_1byte_buffer + i),
                                            *(x_1byte_buffer + i),
                                            &char_placement))
                      {
                        char_width = char_placement.abcA
                          + char_placement.abcB + char_placement.abcC;
                        xoffset += FONT_WIDTH (font) - char_width;
                      }
                    /* Don't let characters go beyond the glyph
                       boundary whatever their over/underhangs. */
                    if (xoffset > glyph_width - char_width)
                      xoffset = glyph_width - char_width;

                    if (xoffset < 0)
                      xoffset = 0;

                    W32_TEXTOUT (i, 1);
		  }
	      }
	  }
	if (!font)
	  {
	    /* Show rectangles to indicate that we found no font.  */
	    int limit = cmpcharp ? 1 : len;
	    HBRUSH hb, oldhb;
	    HPEN hp, oldhp;
	    hb = CreateSolidBrush (bg);
	    hp = CreatePen (PS_SOLID, 0, fg);
	    oldhb = SelectObject(hdc, hb);
	    oldhp = SelectObject(hdc, hp);

	    for (i = 0; i < limit; i++)
	      Rectangle (hdc, left + glyph_width * i, top,
			 left + glyph_width * (i + 1),
			 top + line_height);

	    SelectObject(hdc, oldhb);
	    SelectObject(hdc, oldhp);
	    DeleteObject (hb);
	    DeleteObject (hp);
	  }
        else if (require_clipping && !NILP (Vhighlight_wrong_size_font))
          {
            /* Indicate that we found a font of inappropriate size.  */
            int limit = cmpcharp ? 1 : len;

            for (i = 0; i < limit; i++)
              {
                w32_fill_area (f, hdc, fg, left + glyph_width * i,
                               top + line_height - 1, glyph_width, 1);
                w32_fill_area (f, hdc, fg, left + glyph_width * i,
                               top + line_height - 3, 1, 2);
              }
          }

	{
	  /* Setting underline position based on the metric of the
	     current font results in shaky underline if it strides
	     over different fonts.  So, we set the position based only
	     on the default font of this frame.  */
	  int underline_position = 1;

	  if (FONT_HEIGHT (FRAME_FONT (f)) - FONT_BASE(FRAME_FONT (f))
              <= underline_position)
	    underline_position = (FONT_HEIGHT (FRAME_FONT (f)) -
                                  FONT_BASE(FRAME_FONT (f))) - 1;

	  if (face->underline)
	    w32_fill_area (f, hdc, fg, left,
                           top + FONT_BASE (FRAME_FONT (f))
                           + underline_position,
                           run_width, 1);
	}

	if (!cmpcharp)
	  left += run_width;
      }
    }
  release_frame_dc (f, hdc);

  return (left - orig_left);
}


/* Output some text at the nominal frame cursor position.
   Advance the cursor over the text.
   Output LEN glyphs at START.

   `highlight', set up by w32_reassert_line_highlight or w32_change_line_highlight,
   controls the pixel values used for foreground and background.  */

static void
w32_write_glyphs (start, len)
     register GLYPH *start;
     int len;
{
  register int temp_length;
  struct frame *f;

  BLOCK_INPUT;

  do_line_dance ();
  f = updating_frame;
  if (f == 0)
    {
      f = selected_frame;
      /* If not within an update,
	 output at the frame's visible cursor.  */
      curs_x = f->cursor_x;
      curs_y = f->cursor_y;
    }

  dumpglyphs (f,
	      CHAR_TO_PIXEL_COL (f, curs_x),
	      CHAR_TO_PIXEL_ROW (f, curs_y),
	      start, len, highlight, 0, NULL);

  /* If we drew on top of the cursor, note that it is turned off.  */
  if (curs_y == f->phys_cursor_y
      && curs_x <= f->phys_cursor_x
      && curs_x + len > f->phys_cursor_x)
    f->phys_cursor_on = 0;

  if (updating_frame == 0)
    {
      f->cursor_x += len;
      x_display_cursor (f, 1);
      f->cursor_x -= len;
    }
  else
    curs_x += len;

  UNBLOCK_INPUT;
}

/* Clear to the end of the line.
   Erase the current text line from the nominal cursor position (inclusive)
   to column FIRST_UNUSED (exclusive).  The idea is that everything
   from FIRST_UNUSED onward is already erased.  */

static void
w32_clear_end_of_line (first_unused)
     register int first_unused;
{
  struct frame *f = updating_frame;

  if (f == 0)
    abort ();

  if (curs_y < 0 || curs_y >= f->height)
    return;
  if (first_unused <= 0)
    return;

  if (first_unused >= f->width)
    first_unused = f->width;

  first_unused += FRAME_LEFT_SCROLL_BAR_WIDTH (f);

  BLOCK_INPUT;

  do_line_dance ();

  /* Notice if the cursor will be cleared by this operation.  */
  if (curs_y == f->phys_cursor_y
      && curs_x <= f->phys_cursor_x
      && f->phys_cursor_x < first_unused)
    f->phys_cursor_on = 0;

  w32_clear_area (f, NULL,
		    CHAR_TO_PIXEL_COL (f, curs_x),
		    CHAR_TO_PIXEL_ROW (f, curs_y),
		    FONT_WIDTH (f->output_data.w32->font) * (first_unused - curs_x),
		    f->output_data.w32->line_height);

  UNBLOCK_INPUT;
}

static void
w32_clear_frame ()
{
  struct frame *f = updating_frame;

  if (f == 0)
    f = selected_frame;

  f->phys_cursor_on = 0;        /* Cursor not visible.  */
  curs_x = 0;                   /* Nominal cursor position is top left.  */
  curs_y = 0;

  BLOCK_INPUT;

  w32_clear_window (f);

  /* We have to clear the scroll bars, too.  If we have changed
     colors or something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  UNBLOCK_INPUT;
}

/* Make audible bell.  */

static void
w32_ring_bell (void)
{
  BLOCK_INPUT;

  if (visible_bell)
    {
      int i;
      HWND hwnd = FRAME_W32_WINDOW (selected_frame);

      for (i = 0; i < 5; i++) 
	{
	  FlashWindow (hwnd, TRUE);
	  Sleep (10);
	}
      FlashWindow (hwnd, FALSE);
    }
  else
      w32_sys_ring_bell ();

  UNBLOCK_INPUT;
}

/* Insert and delete character.
   These are not supposed to be used because we are supposed to turn
   off the feature of using them.  */

static void
w32_insert_glyphs (start, len)
     register GLYPH *start;
     register int len;
{
  abort ();
}

static void
w32_delete_glyphs (n)
     register int n;
{
  abort ();
}

/* Specify how many text lines, from the top of the window,
   should be affected by insert-lines and delete-lines operations.
   This, and those operations, are used only within an update
   that is bounded by calls to w32_update_begin and w32_update_end.  */

static void
w32_set_terminal_window (n)
     register int n;
{
  if (updating_frame == 0)
    abort ();

  if ((n <= 0) || (n > updating_frame->height))
    flexlines = updating_frame->height;
  else
    flexlines = n;
}

/* These variables need not be per frame
   because redisplay is done on a frame-by-frame basis
   and the line dance for one frame is finished before
   anything is done for another frame.  */

/* Array of line numbers from cached insert/delete operations.
   line_dance[i] is the old position of the line that we want
   to move to line i, or -1 if we want a blank line there.  */
static int *line_dance;

/* Allocated length of that array.  */
static int line_dance_len;

/* Flag indicating whether we've done any work.  */
static int line_dance_in_progress;

/* Perform an insert-lines or delete-lines operation,
   inserting N lines or deleting -N lines at vertical position VPOS.  */

static void
w32_ins_del_lines (vpos, n)
     int vpos, n;
{
  register int fence, i;

  if (vpos >= flexlines)
    return;

  if (!line_dance_in_progress)
    {
      int ht = updating_frame->height;
      if (ht > line_dance_len)
	{
	  line_dance = (int *)xrealloc (line_dance, ht * sizeof (int));
	  line_dance_len = ht;
	}
      for (i = 0; i < ht; ++i) line_dance[i] = i;
      line_dance_in_progress = 1;
    }
  if (n >= 0)
    {
      if (n > flexlines - vpos)
	n = flexlines - vpos;
      fence = vpos + n;
      for (i = flexlines; --i >= fence;)
	line_dance[i] = line_dance[i-n];
      for (i = fence; --i >= vpos;)
	line_dance[i] = -1;
    }
  else
    {
      n = -n;
      if (n > flexlines - vpos)
	n = flexlines - vpos;
      fence = flexlines - n;
      for (i = vpos; i < fence; ++i)
	line_dance[i] = line_dance[i + n];
      for (i = fence; i < flexlines; ++i)
	line_dance[i] = -1;
    }
}

/* Here's where we actually move the pixels around.
   Must be called with input blocked.  */
static void
do_line_dance ()
{
  register int i, j, distance;
  register struct frame *f;
  int ht;
  int intborder;
  HDC hdc;

  /* Must check this flag first.  If it's not set, then not only is the
     array uninitialized, but we might not even have a frame.  */
  if (!line_dance_in_progress)
    return;

  f = updating_frame;
  if (f == 0)
    abort ();

  ht = f->height;
  intborder = CHAR_TO_PIXEL_COL (f, FRAME_LEFT_SCROLL_BAR_WIDTH (f));

  x_display_cursor (updating_frame, 0);

  hdc = get_frame_dc (f);

  for (i = 0; i < ht; ++i)
    if (line_dance[i] != -1 && (distance = line_dance[i]-i) > 0)
      {
	for (j = i; (j < ht && line_dance[j] != -1
		     && line_dance[j]-j == distance); ++j);
	/* Copy [i,j) upward from [i+distance, j+distance) */
	BitBlt (hdc, 
		intborder, CHAR_TO_PIXEL_ROW (f, i+distance),
		FRAME_WINDOW_WIDTH (f) * FONT_WIDTH (FRAME_FONT (f)),
		(j-i) * FRAME_LINE_HEIGHT (f), 
		hdc,
		intborder, CHAR_TO_PIXEL_ROW (f, i),
		SRCCOPY);
	i = j-1;
      }

  for (i = ht; --i >=0; )
    if (line_dance[i] != -1 && (distance = line_dance[i]-i) < 0)
      {
	for (j = i; (--j >= 0 && line_dance[j] != -1
		     && line_dance[j]-j == distance););
	/* Copy (j, i] downward from (j+distance, i+distance] */
	BitBlt (hdc,
		intborder, CHAR_TO_PIXEL_ROW (f, j+1+distance),
		FRAME_WINDOW_WIDTH (f) * FONT_WIDTH (FRAME_FONT (f)),
		(i-j) * FRAME_LINE_HEIGHT (f), 
		hdc,
		intborder, CHAR_TO_PIXEL_ROW (f, j+1),
		SRCCOPY);
	i = j+1;
      }

  for (i = 0; i < ht; ++i)
    if (line_dance[i] == -1)
      {
	for (j = i; j < ht && line_dance[j] == -1; ++j);
	/* Clear [i,j) */
	w32_clear_area (f, hdc,
			  intborder, 
			  CHAR_TO_PIXEL_ROW (f, i),
			  FRAME_WINDOW_WIDTH (f) * FONT_WIDTH (FRAME_FONT (f)),
			  (j-i) * FRAME_LINE_HEIGHT (f));
	i = j-1;
      }
  line_dance_in_progress = 0;

  release_frame_dc (f, hdc);
}

/* Support routines for exposure events.  */
static void clear_cursor ();

/* Output into a rectangle of a window (for frame F)
   the characters in f->phys_lines that overlap that rectangle.
   TOP and LEFT are the position of the upper left corner of the rectangle.
   ROWS and COLS are the size of the rectangle.
   Call this function with input blocked.  */

void
dumprectangle (f, left, top, cols, rows)
     struct frame *f;
     register int left, top, cols, rows;
{
  register struct frame_glyphs *active_frame = FRAME_CURRENT_GLYPHS (f);
  int cursor_cleared = 0;
  int bottom, right;
  register int y;

  if (FRAME_GARBAGED_P (f))
    return;

  /* Express rectangle as four edges, instead of position-and-size.  */
  bottom = top + rows;
  right = left + cols;

  /* Convert rectangle edges in pixels to edges in chars.
     Round down for left and top, up for right and bottom.  */
  top  = PIXEL_TO_CHAR_ROW (f, top);
  left = PIXEL_TO_CHAR_COL (f, left);
  bottom += (f->output_data.w32->line_height - 1);
  right += (FONT_WIDTH (f->output_data.w32->font) - 1);
  bottom = PIXEL_TO_CHAR_ROW (f, bottom);
  right = PIXEL_TO_CHAR_COL (f, right);

  /* Clip the rectangle to what can be visible.  */
  if (left < FRAME_LEFT_SCROLL_BAR_WIDTH (f))
    left = FRAME_LEFT_SCROLL_BAR_WIDTH (f);
  if (top < 0)
    top = 0;
  if (right > f->width + FRAME_LEFT_SCROLL_BAR_WIDTH (f))
    right = f->width + FRAME_LEFT_SCROLL_BAR_WIDTH (f);
  if (bottom > f->height)
    bottom = f->height;

  /* Get size in chars of the rectangle.  */
  cols = right - left;
  rows = bottom - top;

  /* If rectangle has zero area, return.  */
  if (rows <= 0) return;
  if (cols <= 0) return;

  /* Turn off the cursor if it is in the rectangle.
     We will turn it back on afterward.  */
  if ((f->phys_cursor_x >= left) && (f->phys_cursor_x < right)
      && (f->phys_cursor_y >= top) && (f->phys_cursor_y < bottom))
    {
      clear_cursor (f);
      cursor_cleared = 1;
    }

  /* Display the text in the rectangle, one text line at a time.  */

  for (y = top; y < bottom; y++)
    {
      GLYPH *line = &active_frame->glyphs[y][left];

      if (! active_frame->enable[y] || left > active_frame->used[y])
	continue;

      while (*line & GLYPH_MASK_PADDING)
	{
	  /* We must display the whole glyph of a wide-column
	     character.  */
	  left--;
	  line--;
	  cols++;
	}
      dumpglyphs (f,
		  CHAR_TO_PIXEL_COL (f, left),
		  CHAR_TO_PIXEL_ROW (f, y),
		  line, min (cols, active_frame->used[y] - left),
		  active_frame->highlight[y], 0, NULL);
    }

  /* Turn the cursor on if we turned it off.  */

  if (cursor_cleared)
    x_display_cursor (f, 1);
}

static void
frame_highlight (f)
     struct frame *f;
{
  x_display_cursor (f, 1);
}

static void
frame_unhighlight (f)
     struct frame *f;
{
  x_display_cursor (f, 1);
}

static void x_frame_rehighlight ();

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

void
x_new_focus_frame (dpyinfo, frame)
     struct w32_display_info *dpyinfo;
     struct frame *frame;
{
  struct frame *old_focus = dpyinfo->w32_focus_frame;
  int events_enqueued = 0;

  if (frame != dpyinfo->w32_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of w32_focus_frame.  */
      dpyinfo->w32_focus_frame = frame;

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

      if (dpyinfo->w32_focus_frame && dpyinfo->w32_focus_frame->auto_raise)
	pending_autoraise_frame = dpyinfo->w32_focus_frame;
      else
	pending_autoraise_frame = 0;
    }

  x_frame_rehighlight (dpyinfo);
}

/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  x_new_focus_frame (dpyinfo, dpyinfo->w32_focus_event_frame);
}

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   minibuffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or unhighlighted; we only use it to find
   the appropriate display info.  */
static void
w32_frame_rehighlight (frame)
     struct frame *frame;
{
  x_frame_rehighlight (FRAME_W32_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  struct frame *old_highlight = dpyinfo->w32_highlight_frame;

  if (dpyinfo->w32_focus_frame)
    {
      dpyinfo->w32_highlight_frame
	= ((GC_FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame))
	   : dpyinfo->w32_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->w32_highlight_frame))
	{
	  FRAME_FOCUS_FRAME (dpyinfo->w32_focus_frame) = Qnil;
	  dpyinfo->w32_highlight_frame = dpyinfo->w32_focus_frame;
	}
    }
  else
    dpyinfo->w32_highlight_frame = 0;

  if (dpyinfo->w32_highlight_frame != old_highlight)
    {
      if (old_highlight)
	frame_unhighlight (old_highlight);
      if (dpyinfo->w32_highlight_frame)
	frame_highlight (dpyinfo->w32_highlight_frame);
    }
}

/* Keyboard processing - modifier keys, etc. */

/* Convert a keysym to its name.  */

char *
x_get_keysym_name (keysym)
    int keysym;
{
  /* Make static so we can always return it */
  static char value[100];

  BLOCK_INPUT;
  GetKeyNameText(keysym, value, 100);
  UNBLOCK_INPUT;

  return value;
}

/* Mouse clicks and mouse movement.  Rah.  */

/* Given a pixel position (PIX_X, PIX_Y) on the frame F, return
   glyph co-ordinates in (*X, *Y).  Set *BOUNDS to the rectangle
   that the glyph at X, Y occupies, if BOUNDS != 0.
   If NOCLIP is nonzero, do not force the value into range.  */

void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds, noclip)
     FRAME_PTR f;
     register int pix_x, pix_y;
     register int *x, *y;
     RECT *bounds;
     int noclip;
{
  /* Support tty mode: if Vwindow_system is nil, behave correctly. */
  if (NILP (Vwindow_system))
    {
      *x = pix_x;
      *y = pix_y;
      return;
    }

  /* Arrange for the division in PIXEL_TO_CHAR_COL etc. to round down
     even for negative values.  */
  if (pix_x < 0)
    pix_x -= FONT_WIDTH ((f)->output_data.w32->font) - 1;
  if (pix_y < 0)
    pix_y -= (f)->output_data.w32->line_height - 1;

  pix_x = PIXEL_TO_CHAR_COL (f, pix_x);
  pix_y = PIXEL_TO_CHAR_ROW (f, pix_y);

  if (bounds)
    {
      bounds->left = CHAR_TO_PIXEL_COL (f, pix_x);
      bounds->top = CHAR_TO_PIXEL_ROW (f, pix_y);
      bounds->right  = bounds->left + FONT_WIDTH  (f->output_data.w32->font) - 1;
      bounds->bottom = bounds->top + f->output_data.w32->line_height - 1;
    }

  if (!noclip)
    {
      if (pix_x < 0)
	pix_x = 0;
      else if (pix_x > f->width)
	pix_x = f->width;

      if (pix_y < 0)
	pix_y = 0;
      else if (pix_y > f->height)
	pix_y = f->height;
    }

  *x = pix_x;
  *y = pix_y;
}

void
glyph_to_pixel_coords (f, x, y, pix_x, pix_y)
     FRAME_PTR f;
     register int x, y;
     register int *pix_x, *pix_y;
{
  /* Support tty mode: if Vwindow_system is nil, behave correctly. */
  if (NILP (Vwindow_system))
    {
      *pix_x = x;
      *pix_y = y;
      return;
    }

  *pix_x = CHAR_TO_PIXEL_COL (f, x);
  *pix_y = CHAR_TO_PIXEL_ROW (f, y);
}

BOOL 
parse_button (message, pbutton, pup)
     int message;
     int * pbutton;
     int * pup;
{
  int button = 0;
  int up = 0;
  
  switch (message)
    {
    case WM_LBUTTONDOWN:
      button = 0;
      up = 0;
      break;
    case WM_LBUTTONUP:
      button = 0;
      up = 1;
      break;
    case WM_MBUTTONDOWN:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 0;
      break;
    case WM_MBUTTONUP:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 1;
      else
	button = 2;
      up = 1;
      break;
    case WM_RBUTTONDOWN:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 0;
      break;
    case WM_RBUTTONUP:
      if (NILP (Vw32_swap_mouse_buttons))
	button = 2;
      else
	button = 1;
      up = 1;
      break;
    default:
      return (FALSE);
    }
  
  if (pup) *pup = up;
  if (pbutton) *pbutton = button;
  
  return (TRUE);
}


/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.  */

static void
construct_mouse_click (result, msg, f)
     struct input_event *result;
     W32Msg *msg;
     struct frame *f;
{
  int button;
  int up;

  parse_button (msg->msg.message, &button, &up);

  /* Make the event type no_event; we'll change that when we decide
     otherwise.  */
  result->kind = mouse_click;
  result->code = button;
  result->timestamp = msg->msg.time;
  result->modifiers = (msg->dwModifiers
		       | (up
			  ? up_modifier
			  : down_modifier));

  {
    int row, column;

    XSETINT (result->x, LOWORD (msg->msg.lParam));
    XSETINT (result->y, HIWORD (msg->msg.lParam));
    XSETFRAME (result->frame_or_window, f);
  }
}

static void
construct_mouse_wheel (result, msg, f)
     struct input_event *result;
     W32Msg *msg;
     struct frame *f;
{
  POINT p;
  result->kind = mouse_wheel;
  result->code = (short) HIWORD (msg->msg.wParam);
  result->timestamp = msg->msg.time;
  result->modifiers = msg->dwModifiers;
  p.x = LOWORD (msg->msg.lParam);
  p.y = HIWORD (msg->msg.lParam);
  ScreenToClient(msg->msg.hwnd, &p);
  XSETINT (result->x, p.x);
  XSETINT (result->y, p.y);
  XSETFRAME (result->frame_or_window, f);
}

static void
construct_drag_n_drop (result, msg, f)
     struct input_event *result;
     W32Msg *msg;
     struct frame *f;
{
  Lisp_Object files;
  Lisp_Object frame;
  HDROP hdrop;
  POINT p;
  WORD num_files;
  char *name;
  int i, len;

  result->kind = drag_n_drop;
  result->code = 0;
  result->timestamp = msg->msg.time;
  result->modifiers = msg->dwModifiers;

  hdrop = (HDROP) msg->msg.wParam;
  DragQueryPoint (hdrop, &p);

#if 0
  p.x = LOWORD (msg->msg.lParam);
  p.y = HIWORD (msg->msg.lParam);
  ScreenToClient (msg->msg.hwnd, &p);
#endif

  XSETINT (result->x, p.x);
  XSETINT (result->y, p.y);

  num_files = DragQueryFile (hdrop, 0xFFFFFFFF, NULL, 0);
  files = Qnil;

  for (i = 0; i < num_files; i++)
    {
      len = DragQueryFile (hdrop, i, NULL, 0);
      if (len <= 0)
	continue;
      name = alloca (len + 1);
      DragQueryFile (hdrop, i, name, len + 1);
      files = Fcons (build_string (name), files);
    }

  DragFinish (hdrop);

  XSETFRAME (frame, f);
  result->frame_or_window = Fcons (frame, files);
}


/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.  */

static void
note_mouse_movement (frame, msg)
     FRAME_PTR frame;
     MSG *msg;
{
  last_mouse_movement_time = msg->time;

  if (msg->hwnd != FRAME_W32_WINDOW (frame))
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, -1, -1);
    }

  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  else if (LOWORD (msg->lParam) < last_mouse_glyph.left
	   || LOWORD (msg->lParam) > last_mouse_glyph.right
	   || HIWORD (msg->lParam) < last_mouse_glyph.top
	   || HIWORD (msg->lParam) > last_mouse_glyph.bottom)
    {
      frame->mouse_moved = 1;
      last_mouse_scroll_bar = Qnil;

      note_mouse_highlight (frame, LOWORD (msg->lParam), HIWORD (msg->lParam));
    }
}

/* This is used for debugging, to turn off note_mouse_highlight.  */
static int disable_mouse_highlight;

/* Take proper action when the mouse has moved to position X, Y on frame F
   as regards highlighting characters that have mouse-face properties.
   Also dehighlighting chars where the mouse was before.
   X and Y can be negative or out of range.  */

static void
note_mouse_highlight (f, x, y)
     FRAME_PTR f;
     int x, y;
{
  int row, column, portion;
  RECT new_glyph;
  Lisp_Object window;
  struct window *w;

  if (disable_mouse_highlight)
    return;

  FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_x = x;
  FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_y = y;
  FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_frame = f;

  if (FRAME_W32_DISPLAY_INFO (f)->mouse_face_defer)
    return;

  if (gc_in_progress)
    {
      FRAME_W32_DISPLAY_INFO (f)->mouse_face_deferred_gc = 1;
      return;
    }

  /* Find out which glyph the mouse is on.  */
  pixel_to_glyph_coords (f, x, y, &column, &row,
			 &new_glyph, FRAME_W32_DISPLAY_INFO (f)->grabbed);

  /* Which window is that in?  */
  window = window_from_coordinates (f, column, row, &portion);
  w = XWINDOW (window);

  /* If we were displaying active text in another window, clear that.  */
  if (! EQ (window, FRAME_W32_DISPLAY_INFO (f)->mouse_face_window))
    clear_mouse_face (FRAME_W32_DISPLAY_INFO (f));

  /* Are we in a window whose display is up to date?
     And verify the buffer's text has not changed.  */
  if (WINDOWP (window) && portion == 0 && row >= 0 && column >= 0
      && row < FRAME_HEIGHT (f) && column < FRAME_WIDTH (f)
      && EQ (w->window_end_valid, w->buffer)
      && w->last_modified == BUF_MODIFF (XBUFFER (w->buffer))
      && w->last_overlay_modified == BUF_OVERLAY_MODIFF (XBUFFER (w->buffer)))
    {
      int *ptr = FRAME_CURRENT_GLYPHS (f)->charstarts[row];
      int i, pos;

      /* Find which buffer position the mouse corresponds to.  */
      for (i = column; i >= 0; i--)
	if (ptr[i] > 0)
	  break;
      pos = ptr[i];
      /* Is it outside the displayed active region (if any)?  */
      if (pos <= 0)
	clear_mouse_face (FRAME_W32_DISPLAY_INFO (f));
      else if (! (EQ (window, FRAME_W32_DISPLAY_INFO (f)->mouse_face_window)
		  && row >= FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row
		  && row <= FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row
		  && (row > FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row
		      || column >= FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_col)
		  && (row < FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row
		      || column < FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_col
		      || FRAME_W32_DISPLAY_INFO (f)->mouse_face_past_end)))
	{
	  Lisp_Object mouse_face, overlay, position;
	  Lisp_Object *overlay_vec;
	  int len, noverlays, ignor1;
	  struct buffer *obuf;
	  int obegv, ozv;

	  /* If we get an out-of-range value, return now; avoid an error.  */
	  if (pos > BUF_Z (XBUFFER (w->buffer)))
	    return;

	  /* Make the window's buffer temporarily current for
	     overlays_at and compute_char_face.  */
	  obuf = current_buffer;
	  current_buffer = XBUFFER (w->buffer);
	  obegv = BEGV;
	  ozv = ZV;
	  BEGV = BEG;
	  ZV = Z;

	  /* Yes.  Clear the display of the old active region, if any.  */
	  clear_mouse_face (FRAME_W32_DISPLAY_INFO (f));

	  /* Is this char mouse-active?  */
	  XSETINT (position, pos);

	  len = 10;
	  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));

	  /* Put all the overlays we want in a vector in overlay_vec.
	     Store the length in len.  */
	  noverlays = overlays_at (XINT (pos), 1, &overlay_vec, &len,
				   NULL, NULL);
	  noverlays = sort_overlays (overlay_vec, noverlays, w);

	  /* Find the highest priority overlay that has a mouse-face prop.  */
	  overlay = Qnil;
	  for (i = 0; i < noverlays; i++)
	    {
	      mouse_face = Foverlay_get (overlay_vec[i], Qmouse_face);
	      if (!NILP (mouse_face))
		{
		  overlay = overlay_vec[i];
		  break;
		}
	    }
	  free (overlay_vec);
	  /* If no overlay applies, get a text property.  */
	  if (NILP (overlay))
	    mouse_face = Fget_text_property (position, Qmouse_face, w->buffer);

	  /* Handle the overlay case.  */
	  if (! NILP (overlay))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after;
	      int ignore;

	      before = Foverlay_start (overlay);
	      after = Foverlay_end (overlay);
	      /* Record this as the current active region.  */
	      fast_find_position (window, before,
				  &FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_col,
				  &FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row);
	      FRAME_W32_DISPLAY_INFO (f)->mouse_face_past_end
		= !fast_find_position (window, after,
				       &FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_col,
				       &FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row);
	      FRAME_W32_DISPLAY_INFO (f)->mouse_face_window = window;
	      FRAME_W32_DISPLAY_INFO (f)->mouse_face_face_id
		= compute_char_face (f, w, pos, 0, 0,
				     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (FRAME_W32_DISPLAY_INFO (f), 1);
	    }
	  /* Handle the text property case.  */
	  else if (! NILP (mouse_face))
	    {
	      /* Find the range of text around this char that
		 should be active.  */
	      Lisp_Object before, after, beginning, end;
	      int ignore;

	      beginning = Fmarker_position (w->start);
	      XSETINT (end, (BUF_Z (XBUFFER (w->buffer))
			     - XFASTINT (w->window_end_pos)));
	      before
		= Fprevious_single_property_change (make_number (pos + 1),
						    Qmouse_face,
						    w->buffer, beginning);
	      after
		= Fnext_single_property_change (position, Qmouse_face,
						w->buffer, end);
	      /* Record this as the current active region.  */
	      fast_find_position (window, before,
				  &FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_col,
				  &FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row);
	      FRAME_W32_DISPLAY_INFO (f)->mouse_face_past_end
		= !fast_find_position (window, after,
				       &FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_col,
				       &FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row);
	      FRAME_W32_DISPLAY_INFO (f)->mouse_face_window = window;
	      FRAME_W32_DISPLAY_INFO (f)->mouse_face_face_id
		= compute_char_face (f, w, pos, 0, 0,
				     &ignore, pos + 1, 1);

	      /* Display it as active.  */
	      show_mouse_face (FRAME_W32_DISPLAY_INFO (f), 1);
	    }
	  BEGV = obegv;
	  ZV = ozv;
	  current_buffer = obuf;
	}
    }
}

/* Find the row and column of position POS in window WINDOW.
   Store them in *COLUMNP and *ROWP.
   This assumes display in WINDOW is up to date.
   If POS is above start of WINDOW, return coords
   of start of first screen line.
   If POS is after end of WINDOW, return coords of end of last screen line.

   Value is 1 if POS is in range, 0 if it was off screen.  */

static int
fast_find_position (window, pos, columnp, rowp)
     Lisp_Object window;
     int pos;
     int *columnp, *rowp;
{
  struct window *w = XWINDOW (window);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int i;
  int row = 0;
  int left = WINDOW_LEFT_MARGIN (w);
  int top = w->top;
  int height = XFASTINT (w->height) - ! MINI_WINDOW_P (w);
  int width = window_internal_width (w);
  int *charstarts;
  int lastcol;
  int maybe_next_line = 0;

  /* Find the right row.  */
  for (i = 0;
       i < height;
       i++)
    {
      int linestart = FRAME_CURRENT_GLYPHS (f)->charstarts[top + i][left];
      if (linestart > pos)
	break;
      /* If the position sought is the end of the buffer,
	 don't include the blank lines at the bottom of the window.  */
      if (linestart == pos && pos == BUF_ZV (XBUFFER (w->buffer)))
	{
	  maybe_next_line = 1;
	  break;
	}
      if (linestart > 0)
	row = i;
    }

  /* Find the right column with in it.  */
  charstarts = FRAME_CURRENT_GLYPHS (f)->charstarts[top + row];
  lastcol = left;
  for (i = 0; i < width; i++)
    {
      if (charstarts[left + i] == pos)
	{
	  *rowp = row + top;
	  *columnp = i + left;
	  return 1;
	}
      else if (charstarts[left + i] > pos)
	break;
      else if (charstarts[left + i] > 0)
	lastcol = left + i;
    }

  /* If we're looking for the end of the buffer,
     and we didn't find it in the line we scanned,
     use the start of the following line.  */
  if (maybe_next_line)
    {
      row++;
      i = 0;
    }

  *rowp = row + top;
  *columnp = lastcol;
  return 0;
}

/* Display the active region described by mouse_face_*
   in its mouse-face if HL > 0, in its normal face if HL = 0.  */

static void
show_mouse_face (dpyinfo, hl)
     struct w32_display_info *dpyinfo;
     int hl;
{
  struct window *w = XWINDOW (dpyinfo->mouse_face_window);
  int width = window_internal_width (w);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (w));
  int i;
  int cursor_off = 0;
  int old_curs_x = curs_x;
  int old_curs_y = curs_y;

  /* Set these variables temporarily
     so that if we have to turn the cursor off and on again
     we will put it back at the same place.  */
  curs_x = f->phys_cursor_x;
  curs_y = f->phys_cursor_y;

  for (i = FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row;
       i <= FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row; i++)
    {
      int column = (i == FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row
		    ? FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_col
		    : WINDOW_LEFT_MARGIN (w));
      int endcolumn = (i == FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row
		       ? FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_col
		       : WINDOW_LEFT_MARGIN (w) + width);
      endcolumn = min (endcolumn, FRAME_CURRENT_GLYPHS (f)->used[i]);

      /* If the cursor's in the text we are about to rewrite,
	 turn the cursor off.  */
      if (i == curs_y
	  && curs_x >= column - 1
	  && curs_x <= endcolumn)
	{
	  x_display_cursor (f, 0);
	  cursor_off = 1;
	}

      dumpglyphs (f,
		  CHAR_TO_PIXEL_COL (f, column),
		  CHAR_TO_PIXEL_ROW (f, i),
		  FRAME_CURRENT_GLYPHS (f)->glyphs[i] + column,
		  endcolumn - column,
		  /* Highlight with mouse face if hl > 0.  */
		  hl > 0 ? 3 : 0, 0, NULL);
    }

  /* If we turned the cursor off, turn it back on.  */
  if (cursor_off)
    x_display_cursor (f, 1);

  curs_x = old_curs_x;
  curs_y = old_curs_y;

  /* Change the mouse cursor according to the value of HL.  */
  if (hl > 0)
    SetCursor (f->output_data.w32->cross_cursor);
  else
    SetCursor (f->output_data.w32->text_cursor);
}

/* Clear out the mouse-highlighted active region.
   Redraw it unhighlighted first.  */

static void
clear_mouse_face (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  if (! NILP (dpyinfo->mouse_face_window))
    show_mouse_face (dpyinfo, 0);

  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_window = Qnil;
}

/* Just discard the mouse face information for frame F, if any.
   This is used when the size of F is changed.  */

void
cancel_mouse_face (f)
     FRAME_PTR f;
{
  Lisp_Object window;
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  window = dpyinfo->mouse_face_window;
  if (! NILP (window) && XFRAME (XWINDOW (window)->frame) == f)
    {
      dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }
}

struct scroll_bar *x_window_to_scroll_bar ();
static void x_scroll_bar_report_motion ();

/* Return the current position of the mouse.
   *fp should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *fp, *bar_window,
   and *part to the frame, window, and scroll bar part that the mouse
   is over.  Set *x and *y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *fp to the frame the
   mouse is on, *bar_window to nil, and *x and *y to the character cell
   the mouse is over.

   Set *time to the server timestamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  This also calls XQueryPointer, which will cause the
   server to give us another MotionNotify when the mouse moves
   again. */

static void
w32_mouse_position (fp, insist, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     int insist;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  FRAME_PTR f1;

  BLOCK_INPUT;

  if (! NILP (last_mouse_scroll_bar) && insist == 0)
    /* This is never called at the moment.  */
    x_scroll_bar_report_motion (fp, bar_window, part, x, y, time);
  else
    {
      POINT pt;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	XFRAME (frame)->mouse_moved = 0;

      last_mouse_scroll_bar = Qnil;
      
      GetCursorPos (&pt);

      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	if (FRAME_W32_DISPLAY_INFO (*fp)->grabbed && last_mouse_frame
	    && FRAME_LIVE_P (last_mouse_frame))
	  {
	    /* If mouse was grabbed on a frame, give coords for that frame
	       even if the mouse is now outside it.  */
	    f1 = last_mouse_frame;
	  }
	else
	  {
	    /* Is window under mouse one of our frames?  */
	    f1 = x_window_to_frame (FRAME_W32_DISPLAY_INFO (*fp), WindowFromPoint(pt));
	  }

	/* If not, is it one of our scroll bars?  */
	if (! f1)
	  {
	    struct scroll_bar *bar = x_window_to_scroll_bar (WindowFromPoint(pt));

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
	      }
	  }

	if (f1 == 0 && insist > 0)
	  f1 = selected_frame;

	if (f1)
	  {
	    int ignore1, ignore2;

	    ScreenToClient (FRAME_W32_WINDOW (f1), &pt);

	    /* Ok, we found a frame.  Store all the values.  */

	    pixel_to_glyph_coords (f1, pt.x, pt.y, &ignore1, &ignore2,
				   &last_mouse_glyph,
				   FRAME_W32_DISPLAY_INFO (f1)->grabbed
				   || insist);

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, pt.x);
	    XSETINT (*y, pt.y);
	    *time = last_mouse_movement_time;
	  }
      }
    }

  UNBLOCK_INPUT;
}

/* Scroll bar support.  */

/* Given an window ID, find the struct scroll_bar which manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */
struct scroll_bar *
x_window_to_scroll_bar (window_id)
     Window window_id;
{
  Lisp_Object tail, frame;

  for (tail = Vframe_list;
       XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      Lisp_Object frame, bar, condemned;

      frame = XCONS (tail)->car;
      /* All elements of Vframe_list should be frames.  */
      if (! GC_FRAMEP (frame))
	abort ();

      /* Scan this frame's scroll bar list for a scroll bar with the
	 right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
	      condemned scroll bar lists with one loop.  */
	   ! GC_NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! GC_NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar)) == window_id)
	  return XSCROLL_BAR (bar);
    }

  return 0;
}

HWND 
my_create_scrollbar (f, bar)
     struct frame * f;
     struct scroll_bar * bar;
{
  return (HWND) SendMessage (FRAME_W32_WINDOW (f),
			     WM_EMACS_CREATESCROLLBAR, (WPARAM) f, 
			     (LPARAM) bar);
}

//#define ATTACH_THREADS

BOOL
my_show_window (FRAME_PTR f, HWND hwnd, int how)
{
#ifndef ATTACH_THREADS
  return SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_SHOWWINDOW,
		      (WPARAM) hwnd, (LPARAM) how);
#else
  return ShowWindow (hwnd, how);
#endif
}

void
my_set_window_pos (HWND hwnd, HWND hwndAfter,
		   int x, int y, int cx, int cy, UINT flags)
{
#ifndef ATTACH_THREADS
  WINDOWPOS pos;
  pos.hwndInsertAfter = hwndAfter;
  pos.x = x;
  pos.y = y;
  pos.cx = cx;
  pos.cy = cy;
  pos.flags = flags;
  SendMessage (hwnd, WM_EMACS_SETWINDOWPOS, (WPARAM) &pos, 0);
#else
  SetWindowPos (hwnd, hwndAfter, x, y, cx, cy, flags);
#endif
}

BOOL
my_set_focus (f, hwnd)
     struct frame * f;
     HWND hwnd;
{
  SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_SETFOCUS, 
	       (WPARAM) hwnd, 0);
}

BOOL
my_set_foreground_window (hwnd)
     HWND hwnd;
{
  SendMessage (hwnd, WM_EMACS_SETFOREGROUND, (WPARAM) hwnd, 0);
}

void
my_destroy_window (f, hwnd)
     struct frame * f;
     HWND hwnd;
{
  SendMessage (FRAME_W32_WINDOW (f), WM_EMACS_DESTROYWINDOW, 
	       (WPARAM) hwnd, 0);
}

/* Open a new window to serve as a scroll bar, and return the
   scroll bar vector for it.  */
static struct scroll_bar *
x_scroll_bar_create (window, top, left, width, height)
     struct window *window;
     int top, left, width, height;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (window));
  struct scroll_bar *bar
    = XSCROLL_BAR (Fmake_vector (make_number (SCROLL_BAR_VEC_SIZE), Qnil));
  HWND hwnd;

  BLOCK_INPUT;

  XSETWINDOW (bar->window, window);
  XSETINT (bar->top, top);
  XSETINT (bar->left, left);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);
  XSETINT (bar->start, 0);
  XSETINT (bar->end, 0);
  bar->dragging = Qnil;

  /* Requires geometry to be set before call to create the real window */

  hwnd = my_create_scrollbar (f, bar);

  if (pfnSetScrollInfo)
    {
      SCROLLINFO si;

      si.cbSize = sizeof (si);
      si.fMask = SIF_ALL;
      si.nMin = 0;
      si.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (height)
	+ VERTICAL_SCROLL_BAR_MIN_HANDLE;
      si.nPage = si.nMax;
      si.nPos = 0;

      pfnSetScrollInfo (hwnd, SB_CTL, &si, FALSE);
    }
  else
    {
      SetScrollRange (hwnd, SB_CTL, 0, VERTICAL_SCROLL_BAR_TOP_RANGE (height), FALSE);
      SetScrollPos (hwnd, SB_CTL, 0, FALSE);
    }

  SET_SCROLL_BAR_W32_WINDOW (bar, hwnd);

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
  if (! NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  UNBLOCK_INPUT;

  return bar;
}

/* Draw BAR's handle in the proper position.
   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD is non-zero; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll bar
   handle, we want to let them drag it down all the way, so that the
   bar's top is as far down as it goes; otherwise, there's no way to
   move to the very end of the buffer.  */
static void
x_scroll_bar_set_handle (bar, start, end, rebuild)
     struct scroll_bar *bar;
     int start, end;
     int rebuild;
{
  int dragging = ! NILP (bar->dragging);
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == XINT (bar->start)
      && end == XINT (bar->end))
    return;

  BLOCK_INPUT;

  {
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height));

    /* Make sure the values are reasonable, and try to preserve
       the distance between start and end.  */
    {
      int length = end - start;

      if (start < 0)
	start = 0;
      else if (start > top_range)
	start = top_range;
      end = start + length;

      if (end < start)
	end = start;
      else if (end > top_range && ! dragging)
	end = top_range;
    }
  }

  /* Store the adjusted setting in the scroll bar.  */
  XSETINT (bar->start, start);
  XSETINT (bar->end, end);

  /* If being dragged, let scroll bar update itself.  */
  if (!dragging)
    {
      if (pfnSetScrollInfo)
	{
	  SCROLLINFO si;

	  si.cbSize = sizeof (si);
	  si.fMask = SIF_PAGE | SIF_POS;
	  si.nPage = end - start + VERTICAL_SCROLL_BAR_MIN_HANDLE;
	  si.nPos = start;

	  pfnSetScrollInfo (w, SB_CTL, &si, TRUE);
	}
      else
	SetScrollPos (w, SB_CTL, start, TRUE);
    }

  UNBLOCK_INPUT;
}

/* Move a scroll bar around on the screen, to accommodate changing
   window configurations.  */
static void
x_scroll_bar_move (bar, top, left, width, height)
     struct scroll_bar *bar;
     int top, left, width, height;
{
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  /* If already correctly positioned, do nothing.  */
  if ( XINT (bar->left) == left
       && XINT (bar->top) == top
       && XINT (bar->width) ==  width
       && XINT (bar->height) == height )
    {
      /* Redraw after clear_frame. */
      if (!my_show_window (f, w, SW_NORMAL))
	InvalidateRect (w, NULL, FALSE);
      return;
    }

  BLOCK_INPUT;

  /* Make sure scroll bar is "visible" before moving, to ensure the
     area of the parent window now exposed will be refreshed.  */
  my_show_window (f, w, SW_HIDE);
  MoveWindow (w, left, top, width, height, TRUE);
  if (pfnSetScrollInfo)
    {
      SCROLLINFO si;

      si.cbSize = sizeof (si);
      si.fMask = SIF_RANGE;
      si.nMin = 0;
      si.nMax = VERTICAL_SCROLL_BAR_TOP_RANGE (height)
	+ VERTICAL_SCROLL_BAR_MIN_HANDLE;

      pfnSetScrollInfo (w, SB_CTL, &si, FALSE);
    }
  else
    SetScrollRange (w, SB_CTL, 0, VERTICAL_SCROLL_BAR_TOP_RANGE (height), FALSE);
  my_show_window (f, w, SW_NORMAL);
//  InvalidateRect (w, NULL, FALSE);

  XSETINT (bar->left, left);
  XSETINT (bar->top, top);
  XSETINT (bar->width, width);
  XSETINT (bar->height, height);

  UNBLOCK_INPUT;
}

/* Destroy the window for BAR, and set its Emacs window's scroll bar
   to nil.  */
static void
x_scroll_bar_remove (bar)
     struct scroll_bar *bar;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));

  BLOCK_INPUT;

  /* Destroy the window.  */
  my_destroy_window (f, SCROLL_BAR_W32_WINDOW (bar));

  /* Disassociate this scroll bar from its window.  */
  XWINDOW (bar->window)->vertical_scroll_bar = Qnil;

  UNBLOCK_INPUT;
}

/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */
static void
w32_set_vertical_scroll_bar (window, portion, whole, position)
     struct window *window;
     int portion, whole, position;
{
  FRAME_PTR f = XFRAME (WINDOW_FRAME (window));
  int top = XINT (window->top);
  int left = WINDOW_VERTICAL_SCROLL_BAR_COLUMN (window);
  int height = WINDOW_VERTICAL_SCROLL_BAR_HEIGHT (window);

  /* Where should this scroll bar be, pixelwise?  */
  int pixel_top  = CHAR_TO_PIXEL_ROW (f, top);
  int pixel_left = CHAR_TO_PIXEL_COL (f, left);
  int pixel_width
    = (FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.w32->font)));
  int pixel_height = VERTICAL_SCROLL_BAR_PIXEL_HEIGHT (f, height);

  struct scroll_bar *bar;

  /* Does the scroll bar exist yet?  */
  if (NILP (window->vertical_scroll_bar))
    bar = x_scroll_bar_create (window,
			      pixel_top, pixel_left,
			      pixel_width, pixel_height);
  else
    {
      /* It may just need to be moved and resized.  */
      bar = XSCROLL_BAR (window->vertical_scroll_bar);
      x_scroll_bar_move (bar, pixel_top, pixel_left, pixel_width, pixel_height);
    }

  /* Set the scroll bar's current state.  */
  {
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (pixel_height);

    if (whole == 0)
      x_scroll_bar_set_handle (bar, 0, top_range, 0);
    else
      {
	int start = (int) (((double) position * top_range) / whole);
	int end = (int) (((double) (position + portion) * top_range) / whole);

	x_scroll_bar_set_handle (bar, start, end, 0);
      }
  }

  XSETVECTOR (window->vertical_scroll_bar, bar);
}


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - "Can you say set-window-configuration, boys
   and girls?"  Instead, we just assert at the beginning of redisplay
   that *all* scroll bars are to be removed, and then save a scroll bar
   from the fiery pit when we actually redisplay its window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgement.  */
static void
w32_condemn_scroll_bars (frame)
     FRAME_PTR frame;
{
  /* Transfer all the scroll bars to FRAME_CONDEMNED_SCROLL_BARS.  */
  while (! NILP (FRAME_SCROLL_BARS (frame)))
    {
      Lisp_Object bar;
      bar = FRAME_SCROLL_BARS (frame);
      FRAME_SCROLL_BARS (frame) = XSCROLL_BAR (bar)->next;
      XSCROLL_BAR (bar)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
      XSCROLL_BAR (bar)->prev = Qnil;
      if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = bar;
      FRAME_CONDEMNED_SCROLL_BARS (frame) = bar;
    }
#ifdef PIGSFLY
  /* The condemned list should be empty at this point; if it's not,
     then the rest of Emacs isn't using the condemn/redeem/judge
     protocol correctly.  */
  if (! NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
    abort ();

  /* Move them all to the "condemned" list.  */
  FRAME_CONDEMNED_SCROLL_BARS (frame) = FRAME_SCROLL_BARS (frame);
  FRAME_SCROLL_BARS (frame) = Qnil;
#endif
}

/* Unmark WINDOW's scroll bar for deletion in this judgement cycle.
   Note that WINDOW isn't necessarily condemned at all.  */
static void
w32_redeem_scroll_bar (window)
     struct window *window;
{
  struct scroll_bar *bar;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  {
    FRAME_PTR f = XFRAME (WINDOW_FRAME (window));

    if (NILP (bar->prev))
      {
	/* If the prev pointer is nil, it must be the first in one of
           the lists.  */
	if (EQ (FRAME_SCROLL_BARS (f), window->vertical_scroll_bar))
	  /* It's not condemned.  Everything's fine.  */
	  return;
	else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		     window->vertical_scroll_bar))
	  FRAME_CONDEMNED_SCROLL_BARS (f) = bar->next;
	else
	  /* If its prev pointer is nil, it must be at the front of
             one or the other!  */
	  abort ();
      }
    else
      XSCROLL_BAR (bar->prev)->next = bar->next;

    if (! NILP (bar->next))
      XSCROLL_BAR (bar->next)->prev = bar->prev;

    bar->next = FRAME_SCROLL_BARS (f);
    bar->prev = Qnil;
    XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
    if (! NILP (bar->next))
      XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
  }
#ifdef PIGSFLY
  struct scroll_bar *bar;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (window->vertical_scroll_bar))
    abort ();

  bar = XSCROLL_BAR (window->vertical_scroll_bar);

  /* Unlink it from the condemned list.  */
  {
    FRAME_PTR f = XFRAME (WINDOW_FRAME (window));

    if (NILP (bar->prev))
      {
	/* If the prev pointer is nil, it must be the first in one of
	   the lists.  */
	if (EQ (FRAME_SCROLL_BARS (f), window->vertical_scroll_bar))
	  /* It's not condemned.  Everything's fine.  */
	  return;
	else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		     window->vertical_scroll_bar))
	  FRAME_CONDEMNED_SCROLL_BARS (f) = bar->next;
	else
	  /* If its prev pointer is nil, it must be at the front of
	     one or the other!  */
	  abort ();
      }
    else
      XSCROLL_BAR (bar->prev)->next = bar->next;

    if (! NILP (bar->next))
      XSCROLL_BAR (bar->next)->prev = bar->prev;

    bar->next = FRAME_SCROLL_BARS (f);
    bar->prev = Qnil;
    XSETVECTOR (FRAME_SCROLL_BARS (f), bar);
    if (! NILP (bar->next))
      XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
  }
#endif
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */
static void
w32_judge_scroll_bars (f)
     FRAME_PTR f;
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  FRAME_CONDEMNED_SCROLL_BARS (f) = Qnil;

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      x_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
#ifdef PIGSFLY
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  FRAME_CONDEMNED_SCROLL_BARS (f) = Qnil;

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      x_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
#endif
}

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than no_event, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static int
x_scroll_bar_handle_click (bar, msg, emacs_event)
     struct scroll_bar *bar;
     W32Msg *msg;
     struct input_event *emacs_event;
{
  if (! GC_WINDOWP (bar->window))
    abort ();

  emacs_event->kind = w32_scroll_bar_click;
  emacs_event->code = 0;
  /* not really meaningful to distinguish up/down */
  emacs_event->modifiers = msg->dwModifiers;
  emacs_event->frame_or_window = bar->window;
  emacs_event->timestamp = msg->msg.time;

  {
    int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height));
    int y;
    int dragging = !NILP (bar->dragging);

    if (pfnGetScrollInfo)
      {
	SCROLLINFO si;

	si.cbSize = sizeof (si);
	si.fMask = SIF_POS;

	pfnGetScrollInfo ((HWND) msg->msg.lParam, SB_CTL, &si);
	y = si.nPos;
      }
    else
      y = GetScrollPos ((HWND) msg->msg.lParam, SB_CTL);

    bar->dragging = Qnil;

    switch (LOWORD (msg->msg.wParam))
      {
      case SB_LINEDOWN:
	emacs_event->part = scroll_bar_down_arrow;
	break;
      case SB_LINEUP:
	emacs_event->part = scroll_bar_up_arrow;
	break;
      case SB_PAGEUP:
	emacs_event->part = scroll_bar_above_handle;
	break;
      case SB_PAGEDOWN:
	emacs_event->part = scroll_bar_below_handle;
	break;
      case SB_TOP:
	emacs_event->part = scroll_bar_handle;
	y = 0;
	break;
      case SB_BOTTOM:
	emacs_event->part = scroll_bar_handle;
	y = top_range;
	break;
      case SB_THUMBTRACK:
      case SB_THUMBPOSITION:
	if (VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height)) <= 0xffff)
	    y = HIWORD (msg->msg.wParam);
	bar->dragging = Qt;
	emacs_event->part = scroll_bar_handle;

	/* "Silently" update current position.  */
	if (pfnSetScrollInfo)
	  {
	    SCROLLINFO si;

	    si.cbSize = sizeof (si);
	    si.fMask = SIF_POS;

#if 0
	    /* Shrink handle if necessary to allow full range for position.  */
	    {
	      int start = XINT (bar->start);
	      int end = XINT (bar->end);
	      int len = end - start;

	      /* If new end is nearly hitting bottom, we must shrink
	         handle.  How much we shrink it depends on the relative
	         sizes of len and top_range.  */
	      if (y + len > top_range - 2)
		{
		  len -= min (top_range / 10, (len / 3) + 2);
		  if (len < 0)
		    len = 0;
		}
	      si.nPage = len + VERTICAL_SCROLL_BAR_MIN_HANDLE;
	      si.fMask |= SIF_PAGE;
	    }
#endif
	    si.nPos = y;
	    /* Remember apparent position (we actually lag behind the real
	       position, so don't set that directly.  */
	    last_scroll_bar_drag_pos = y;

	    pfnSetScrollInfo (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, &si, FALSE);
	  }
	else
	  SetScrollPos (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, y, FALSE);
	break;
      case SB_ENDSCROLL:
	/* If this is the end of a drag sequence, then reset the scroll
	   handle size to normal and do a final redraw.  Otherwise do
	   nothing.  */
	if (dragging)
	  {
	    if (pfnSetScrollInfo)
	      {
		SCROLLINFO si;
		int start = XINT (bar->start);
		int end = XINT (bar->end);

		si.cbSize = sizeof (si);
		si.fMask = SIF_PAGE | SIF_POS;
		si.nPage = end - start + VERTICAL_SCROLL_BAR_MIN_HANDLE;
		si.nPos = last_scroll_bar_drag_pos;

		pfnSetScrollInfo (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, &si, TRUE);
	      }
	    else
	      SetScrollPos (SCROLL_BAR_W32_WINDOW (bar), SB_CTL, y, TRUE);
	  }
	/* fall through */
      default:
	emacs_event->kind = no_event;
	return FALSE;
      }

    XSETINT (emacs_event->x, y);
    XSETINT (emacs_event->y, top_range);

    return TRUE;
  }
}

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */
static void
x_scroll_bar_report_motion (fp, bar_window, part, x, y, time)
     FRAME_PTR *fp;
     Lisp_Object *bar_window;
     enum scroll_bar_part *part;
     Lisp_Object *x, *y;
     unsigned long *time;
{
  struct scroll_bar *bar = XSCROLL_BAR (last_mouse_scroll_bar);
  Window w = SCROLL_BAR_W32_WINDOW (bar);
  FRAME_PTR f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int pos;
  int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height));

  BLOCK_INPUT;

  *fp = f;
  *bar_window = bar->window;

  if (pfnGetScrollInfo)
    {
      SCROLLINFO si;

      si.cbSize = sizeof (si);
      si.fMask = SIF_POS | SIF_PAGE | SIF_RANGE;

      pfnGetScrollInfo (w, SB_CTL, &si);
      pos = si.nPos;
      top_range = si.nMax - si.nPage + 1;
    }
  else
    pos = GetScrollPos (w, SB_CTL);

  switch (LOWORD (last_mouse_scroll_bar_pos))
  {
  case SB_THUMBPOSITION:
  case SB_THUMBTRACK:
      *part = scroll_bar_handle;
      if (VERTICAL_SCROLL_BAR_TOP_RANGE (XINT (bar->height)) <= 0xffff)
	  pos = HIWORD (last_mouse_scroll_bar_pos);
      break;
  case SB_LINEDOWN:
      *part = scroll_bar_handle;
      pos++;
      break;
  default:
      *part = scroll_bar_handle;
      break;
  }

  XSETINT(*x, pos);
  XSETINT(*y, top_range);

  f->mouse_moved = 0;
  last_mouse_scroll_bar = Qnil;

  *time = last_mouse_movement_time;

  UNBLOCK_INPUT;
}

/* The screen has been cleared so we may have changed foreground or
   background colors, and the scroll bars may need to be redrawn.
   Clear out the scroll bars, and ask for expose events, so we can
   redraw them.  */

x_scroll_bar_clear (f)
     FRAME_PTR f;
{
  Lisp_Object bar;

  for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
       bar = XSCROLL_BAR (bar)->next)
    {
      HWND window = SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar));
      HDC hdc = GetDC (window);
      RECT rect;

      /* Hide scroll bar until ready to repaint.  x_scroll_bar_move
         arranges to refresh the scroll bar if hidden.  */
      my_show_window (f, window, SW_HIDE);

      GetClientRect (window, &rect);
      select_palette (f, hdc);
      w32_clear_rect (f, hdc, &rect);
      deselect_palette (f, hdc);

      ReleaseDC (window, hdc);
    }
}

show_scroll_bars (f, how)
     FRAME_PTR f;
     int how;
{
  Lisp_Object bar;

  for (bar = FRAME_SCROLL_BARS (f); VECTORP (bar);
       bar = XSCROLL_BAR (bar)->next)
    {
      HWND window = SCROLL_BAR_W32_WINDOW (XSCROLL_BAR (bar));
      my_show_window (f, window, how);
    }
}


/* The main W32 event-reading loop - w32_read_socket.  */

/* Timestamp of enter window event.  This is only used by w32_read_socket,
   but we have to put it out here, since static variables within functions
   sometimes don't work.  */
static Time enter_timestamp;

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */
int temp_index;
short temp_buffer[100];


/* Read events coming from the W32 shell.
   This routine is called by the SIGIO handler.
   We return as soon as there are no more events to be read.

   Events representing keys are stored in buffer BUFP,
   which can hold up to NUMCHARS characters.
   We return the number of characters stored into the buffer,
   thus pretending to be `read'.

   EXPECTED is nonzero if the caller knows input is available.  

   Some of these messages are reposted back to the message queue since the
   system calls the windows proc directly in a context where we cannot return 
   the data nor can we guarantee the state we are in.  So if we dispatch  them
   we will get into an infinite loop.  To prevent this from ever happening we
   will set a variable to indicate we are in the read_socket call and indicate
   which message we are processing since the windows proc gets called 
   recursively with different messages by the system.
*/

int
w32_read_socket (sd, bufp, numchars, expected)
     register int sd;
     register struct input_event *bufp;
     register int numchars;
     int expected;
{
  int count = 0;
  int check_visibility = 0;
  W32Msg msg;
  struct frame *f;
  Lisp_Object part;
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  if (interrupt_input_blocked)
    {
      interrupt_input_pending = 1;
      return -1;
    }

  interrupt_input_pending = 0;
  BLOCK_INPUT;

  /* So people can tell when we have read the available input.  */
  input_signal_count++;

  if (numchars <= 0)
    abort ();                   /* Don't think this happens. */

  while (get_next_msg (&msg, FALSE))
    {
      switch (msg.msg.message)
	{
	case WM_PAINT:
	    f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	    if (f) 
	      {
		if (msg.rect.right == msg.rect.left ||
		    msg.rect.bottom == msg.rect.top)
		  {
		    /* We may get paint messages even though the client
		       area is clipped - these are not expose events. */
		    DebPrint (("clipped frame %04x (%s) got WM_PAINT\n", f,
			       XSTRING (f->name)->data));
		  }
		else if (f->async_visible != 1)
		  {
		    /* Definitely not obscured, so mark as visible.  */
		    f->async_visible = 1;
		    f->async_iconified = 0;
		    SET_FRAME_GARBAGED (f);
		    DebPrint (("frame %04x (%s) reexposed\n", f,
			       XSTRING (f->name)->data));

		    /* WM_PAINT serves as MapNotify as well, so report
                       visibility changes properly.  */
		    if (f->iconified)
		      {
			bufp->kind = deiconify_event;
			XSETFRAME (bufp->frame_or_window, f);
			bufp++;
			count++;
			numchars--;
		      }
		    else if (! NILP(Vframe_list)
			     && ! NILP (XCONS (Vframe_list)->cdr))
		      /* Force a redisplay sooner or later to update the
			 frame titles in case this is the second frame.  */
		      record_asynch_buffer_change ();
		  }
		else
		  {
		    /* Erase background again for safety.  */
		    w32_clear_rect (f, NULL, &msg.rect);
		    dumprectangle (f,
				   msg.rect.left,
				   msg.rect.top,
				   msg.rect.right - msg.rect.left,
				   msg.rect.bottom - msg.rect.top);
		  }
	      }
	  break;

	case WM_INPUTLANGCHANGE:
	  /* Generate a language change event.  */
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	  
	      bufp->kind = language_change_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = msg.msg.lParam & 0xffff;
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_KEYDOWN:
	case WM_SYSKEYDOWN:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->iconified)
	    {
	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      bufp->kind = non_ascii_keystroke;
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = msg.dwModifiers;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->timestamp = msg.msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;

	case WM_SYSCHAR:
	case WM_CHAR:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->iconified)
	    {
	      if (temp_index == sizeof temp_buffer / sizeof (short))
		temp_index = 0;
	      temp_buffer[temp_index++] = msg.msg.wParam;
	      bufp->kind = ascii_keystroke;
	      bufp->code = msg.msg.wParam;
	      bufp->modifiers = msg.dwModifiers;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp->timestamp = msg.msg.time;
	      bufp++;
	      numchars--;
	      count++;
	    }
	  break;

	case WM_MOUSEMOVE:
	  if (dpyinfo->grabbed && last_mouse_frame
	      && FRAME_LIVE_P (last_mouse_frame))
	    f = last_mouse_frame;
	  else
	    f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    note_mouse_movement (f, &msg.msg);
	  else
	    clear_mouse_face (FRAME_W32_DISPLAY_INFO (f));
	  
	  break;

	case WM_LBUTTONDOWN:
	case WM_LBUTTONUP:
	case WM_MBUTTONDOWN:
	case WM_MBUTTONUP:
	case WM_RBUTTONDOWN:
	case WM_RBUTTONUP:
	  {
	    int button;
	    int up;
	    
	    if (dpyinfo->grabbed && last_mouse_frame
		&& FRAME_LIVE_P (last_mouse_frame))
	      f = last_mouse_frame;
	    else
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	    
	    if (f)
	      {
		if ((!dpyinfo->w32_focus_frame || f == dpyinfo->w32_focus_frame) 
		    && (numchars >= 1))
		  {
		    construct_mouse_click (bufp, &msg, f);
		    bufp++;
		    count++;
		    numchars--;
		  }
	      }
	    
	    parse_button (msg.msg.message, &button, &up);
	    
	    if (up)
	      {
		dpyinfo->grabbed &= ~ (1 << button);
	      }
	    else
	      {
		dpyinfo->grabbed |= (1 << button);
		last_mouse_frame = f;
	      }
	    break;
	  }
	  
      case WM_MOUSEWHEEL:
          if (dpyinfo->grabbed && last_mouse_frame
              && FRAME_LIVE_P (last_mouse_frame))
            f = last_mouse_frame;
          else
            f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

          if (f)
            {
              if ((!dpyinfo->w32_focus_frame 
                   || f == dpyinfo->w32_focus_frame)
                  && (numchars >= 1))
                {
                  construct_mouse_wheel (bufp, &msg, f);
                  bufp++;
                  count++;
                  numchars--;
                }
            }
	  break;

	case WM_DROPFILES:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      construct_drag_n_drop (bufp, &msg, f);
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_VSCROLL:
	  {
	    struct scroll_bar *bar =
	      x_window_to_scroll_bar ((HWND)msg.msg.lParam);
	      
	    if (bar && numchars >= 1)
	      {
		if (x_scroll_bar_handle_click (bar, &msg, bufp))
		  {
		    bufp++;
		    count++;
		    numchars--;
		  }
	      }
	    break;
	  }
	  
	case WM_WINDOWPOSCHANGED:
	case WM_ACTIVATE:
	case WM_ACTIVATEAPP:
	  check_visibility = 1;
	  break;

	case WM_MOVE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f && !f->async_iconified)
	    {
	      int x, y;

	      x_real_positions (f, &x, &y);
	      f->output_data.w32->left_pos = x;
	      f->output_data.w32->top_pos = y;
	    }

	  check_visibility = 1;
	  break;

	case WM_SHOWWINDOW:
	  /* If window has been obscured or exposed by another window
	     being maximised or minimised/restored, then recheck
	     visibility of all frames.  Direct changes to our own
	     windows get handled by WM_SIZE.  */
#if 0
	  if (msg.msg.lParam != 0)
	    check_visibility = 1;
	  else
	    {
	      f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	      f->async_visible = msg.msg.wParam;
	    }
#endif

	  check_visibility = 1;
	  break;

	case WM_SIZE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  /* Inform lisp of whether frame has been iconified etc. */
	  if (f)
	    {
	      switch (msg.msg.wParam)
		{
		case SIZE_MINIMIZED:
		  f->async_visible = 0;
		  f->async_iconified = 1;
		  
		  bufp->kind = iconify_event;
		  XSETFRAME (bufp->frame_or_window, f);
		  bufp++;
		  count++;
		  numchars--;
		  break;

		case SIZE_MAXIMIZED:
		case SIZE_RESTORED:
		  f->async_visible = 1;
		  f->async_iconified = 0;
		  
		  /* wait_reading_process_input will notice this and update
		     the frame's display structures.  */
		  SET_FRAME_GARBAGED (f);
		  
		  if (f->iconified)
		    {
                      int x, y;

                      /* Reset top and left positions of the Window
                         here since Windows sends a WM_MOVE message
                         BEFORE telling us the Window is minimized
                         when the Window is iconified, with 3000,3000
                         as the co-ords. */
                      x_real_positions (f, &x, &y);
                      f->output_data.w32->left_pos = x;
                      f->output_data.w32->top_pos = y;

		      bufp->kind = deiconify_event;
		      XSETFRAME (bufp->frame_or_window, f);
		      bufp++;
		      count++;
		      numchars--;
		    }
		  else
		    /* Force a redisplay sooner or later
		       to update the frame titles
		       in case this is the second frame.  */
		    record_asynch_buffer_change ();
		  break;
		}
	    }

	  if (f && !f->async_iconified && msg.msg.wParam != SIZE_MINIMIZED)
	    {
	      RECT rect;
	      int rows;
	      int columns;
	      int width;
	      int height;
	      
	      GetClientRect(msg.msg.hwnd, &rect);
	      
	      height = rect.bottom - rect.top;
	      width = rect.right - rect.left;
	      
	      rows = PIXEL_TO_CHAR_HEIGHT (f, height);
	      columns = PIXEL_TO_CHAR_WIDTH (f, width);
	      
	      /* TODO: Clip size to the screen dimensions.  */
	      
	      /* Even if the number of character rows and columns has
		 not changed, the font size may have changed, so we need
		 to check the pixel dimensions as well.  */
	      
	      if (columns != f->width
		  || rows != f->height
		  || width != f->output_data.w32->pixel_width
		  || height != f->output_data.w32->pixel_height)
		{
		  /* I had set this to 0, 0 - I am not sure why?? */
		  
		  change_frame_size (f, rows, columns, 0, 1);
		  SET_FRAME_GARBAGED (f);
		  cancel_mouse_face (f);
		  f->output_data.w32->pixel_width = width;
		  f->output_data.w32->pixel_height = height;
		  f->output_data.w32->win_gravity = NorthWestGravity;
		}
	    }

	  check_visibility = 1;
	  break;

	case WM_SETFOCUS:
	case WM_KILLFOCUS:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (msg.msg.message == WM_SETFOCUS)
	    {
	      x_new_focus_frame (dpyinfo, f);
	    }
	  else if (f == dpyinfo->w32_focus_frame)
	    {
	      x_new_focus_frame (dpyinfo, 0);

	      if (f == dpyinfo->mouse_face_mouse_frame)
		clear_mouse_face (dpyinfo);
	    }

	  dpyinfo->grabbed = 0;
	  check_visibility = 1;
	  break;

	case WM_CLOSE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	      
	      bufp->kind = delete_window_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_INITMENU:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
	  
	  if (f)
	    {
	      if (numchars == 0)
		abort ();
	  
	      bufp->kind = menu_bar_activate_event;
	      XSETFRAME (bufp->frame_or_window, f);
	      bufp++;
	      count++;
	      numchars--;
	    }
	  break;

	case WM_COMMAND:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f)
	    {
	      extern void menubar_selection_callback
		(FRAME_PTR f, void * client_data);
	      menubar_selection_callback (f, (void *)msg.msg.wParam);
	    }

	  check_visibility = 1;
	  break;

	case WM_DISPLAYCHANGE:
	  f = x_window_to_frame (dpyinfo, msg.msg.hwnd);

	  if (f) 
	    {
	      dpyinfo->width = (short) LOWORD (msg.msg.lParam);
	      dpyinfo->height = (short) HIWORD (msg.msg.lParam);
	      dpyinfo->n_cbits = msg.msg.wParam;
	      DebPrint (("display change: %d %d\n", dpyinfo->width,
			 dpyinfo->height));
	    }
	  
	  check_visibility = 1;
	  break;

	default:
	  /* Check for messages registered at runtime. */
	  if (msg.msg.message == msh_mousewheel)
	    {
	      if (dpyinfo->grabbed && last_mouse_frame 
		  && FRAME_LIVE_P (last_mouse_frame))
		f = last_mouse_frame;
	      else
		f = x_window_to_frame (dpyinfo, msg.msg.hwnd);
          
	      if (f)
		{
		  if ((!dpyinfo->w32_focus_frame 
		       || f == dpyinfo->w32_focus_frame)
		      && (numchars >= 1))
		    {
		      construct_mouse_wheel (bufp, &msg, f);
		      bufp++;
		      count++;
		      numchars--;
		    }
		}
	    }
	  break;
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

  /* Check which frames are still visisble, if we have enqueued any user
     events or been notified of events that may affect visibility.  We
     do this here because there doesn't seem to be any direct
     notification from Windows that the visibility of a window has
     changed (at least, not in all cases).  */
  if (count > 0 || check_visibility)
    {
      Lisp_Object tail, frame;

      FOR_EACH_FRAME (tail, frame)
	{
	  FRAME_PTR f = XFRAME (frame);
	  /* Check "visible" frames and mark each as obscured or not.
	     Note that async_visible is nonzero for unobscured and
	     obscured frames, but zero for hidden and iconified frames.  */
	  if (FRAME_W32_P (f) && f->async_visible)
	    {
	      RECT clipbox;
	      HDC  hdc = get_frame_dc (f);
	      GetClipBox (hdc, &clipbox);
	      release_frame_dc (f, hdc);

	      if (clipbox.right == clipbox.left
		  || clipbox.bottom == clipbox.top)
		{
		  /* Frame has become completely obscured so mark as
		     such (we do this by setting async_visible to 2 so
		     that FRAME_VISIBLE_P is still true, but redisplay
		     will skip it).  */
		  f->async_visible = 2;

		  if (!FRAME_OBSCURED_P (f))
		    {
		      DebPrint (("frame %04x (%s) obscured\n", f,
				 XSTRING (f->name)->data));
		    }
		}
	      else
		{
		  /* Frame is not obscured, so mark it as such.  */
		  f->async_visible = 1;

		  if (FRAME_OBSCURED_P (f))
		    {
		      SET_FRAME_GARBAGED (f);
		      DebPrint (("frame %04x (%s) reexposed\n", f,
				 XSTRING (f->name)->data));

		      /* Force a redisplay sooner or later.  */
		      record_asynch_buffer_change ();
		    }
		}
	    }
	}
    }

  UNBLOCK_INPUT;
  return count;
}

/* Drawing the cursor.  */


/* Draw a hollow box cursor.  Don't change the inside of the box.  */

static void
x_draw_box (f)
     struct frame *f;
{
  RECT rect;
  HBRUSH hb;
  HDC hdc;
  
  hdc = get_frame_dc (f);
  
  hb = CreateSolidBrush (f->output_data.w32->cursor_pixel);
  
  rect.left = CHAR_TO_PIXEL_COL (f, curs_x);
  rect.top  = CHAR_TO_PIXEL_ROW (f, curs_y);
  rect.right = rect.left + FONT_WIDTH (f->output_data.w32->font);
  rect.bottom = rect.top + f->output_data.w32->line_height;

  FrameRect (hdc, &rect, hb);
  DeleteObject (hb);

  release_frame_dc (f, hdc);
}

/* Clear the cursor of frame F to background color,
   and mark the cursor as not shown.
   This is used when the text where the cursor is
   is about to be rewritten.  */

static void
clear_cursor (f)
     struct frame *f;
{
  if (! FRAME_VISIBLE_P (f)
      || !f->phys_cursor_on)
    return;

  x_display_cursor (f, 0);
  f->phys_cursor_on = 0;
}

/* Redraw the glyph at ROW, COLUMN on frame F, in the style
   HIGHLIGHT.  HIGHLIGHT is as defined for dumpglyphs.  Return the
   glyph drawn.  */

static void
x_draw_single_glyph (f, row, column, glyph, highlight)
     struct frame *f;
     int row, column;
     GLYPH glyph;
     int highlight;
{
  dumpglyphs (f,
	      CHAR_TO_PIXEL_COL (f, column),
	      CHAR_TO_PIXEL_ROW (f, row),
	      &glyph, 1, highlight, 0, NULL);
}

static void
x_display_bar_cursor (f, on)
     struct frame *f;
     int on;
{
  struct frame_glyphs *current_glyphs = FRAME_CURRENT_GLYPHS (f);

  /* This is pointless on invisible frames, and dangerous on garbaged
     frames; in the latter case, the frame may be in the midst of
     changing its size, and curs_x and curs_y may be off the frame.  */
  if (! FRAME_VISIBLE_P (f) || FRAME_GARBAGED_P (f))
    return;

  if (! on && ! f->phys_cursor_on)
    return;

  /* If there is anything wrong with the current cursor state, remove it.  */
  if (f->phys_cursor_on
      && (!on
	  || f->phys_cursor_x != curs_x
	  || f->phys_cursor_y != curs_y
	  || f->output_data.w32->current_cursor != bar_cursor))
    {
      /* Erase the cursor by redrawing the character underneath it.  */
      x_draw_single_glyph (f, f->phys_cursor_y, f->phys_cursor_x,
			   f->phys_cursor_glyph,
			   current_glyphs->highlight[f->phys_cursor_y]);
      f->phys_cursor_on = 0;
    }

  /* If we now need a cursor in the new place or in the new form, do it so.  */
  if (on
      && (! f->phys_cursor_on
	  || (f->output_data.w32->current_cursor != bar_cursor)))
    {
      f->phys_cursor_glyph
	= ((current_glyphs->enable[curs_y]
	    && curs_x < current_glyphs->used[curs_y])
	   ? current_glyphs->glyphs[curs_y][curs_x]
	   : SPACEGLYPH);
      w32_fill_area (f, NULL, f->output_data.w32->cursor_pixel,
		       CHAR_TO_PIXEL_COL (f, curs_x),
		       CHAR_TO_PIXEL_ROW (f, curs_y),
		       max (f->output_data.w32->cursor_width, 1),
		       f->output_data.w32->line_height);

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;
      f->phys_cursor_on = 1;

      f->output_data.w32->current_cursor = bar_cursor;
    }
}


/* Turn the displayed cursor of frame F on or off according to ON.
   If ON is nonzero, where to put the cursor is specified
   by F->cursor_x and F->cursor_y.  */

static void
x_display_box_cursor (f, on)
     struct frame *f;
     int on;
{
  struct frame_glyphs *current_glyphs = FRAME_CURRENT_GLYPHS (f);

  /* This is pointless on invisible frames, and dangerous on garbaged
     frames; in the latter case, the frame may be in the midst of
     changing its size, and curs_x and curs_y may be off the frame.  */
  if (! FRAME_VISIBLE_P (f) || FRAME_GARBAGED_P (f))
    return;

  /* If cursor is off and we want it off, return quickly.  */
  if (!on && ! f->phys_cursor_on)
    return;

  /* If cursor is currently being shown and we don't want it to be
     or it is in the wrong place,
     or we want a hollow box and it's not so, (pout!)
     erase it.  */
  if (f->phys_cursor_on
      && (!on
	  || f->phys_cursor_x != curs_x
	  || f->phys_cursor_y != curs_y
	  || (f->output_data.w32->current_cursor != hollow_box_cursor
	      && (f != FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame))))
    {
      int mouse_face_here = 0;
      struct frame_glyphs *active_glyphs = FRAME_CURRENT_GLYPHS (f);

      /* If the cursor is in the mouse face area, redisplay that when
	 we clear the cursor.  */
      if (f == FRAME_W32_DISPLAY_INFO (f)->mouse_face_mouse_frame
	  && (f->phys_cursor_y > FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row
	      || (f->phys_cursor_y == FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_row
		  && f->phys_cursor_x >= FRAME_W32_DISPLAY_INFO (f)->mouse_face_beg_col))
	  && (f->phys_cursor_y < FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row
	      || (f->phys_cursor_y == FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_row
		  && f->phys_cursor_x < FRAME_W32_DISPLAY_INFO (f)->mouse_face_end_col))
	  /* Don't redraw the cursor's spot in mouse face
	     if it is at the end of a line (on a newline).
	     The cursor appears there, but mouse highlighting does not.  */
	  && active_glyphs->used[f->phys_cursor_y] > f->phys_cursor_x)
	mouse_face_here = 1;

      /* If the font is not as tall as a whole line,
	 we must explicitly clear the line's whole height.  */
      if (FONT_HEIGHT (f->output_data.w32->font) != f->output_data.w32->line_height)
	w32_clear_area (f, NULL,
			  CHAR_TO_PIXEL_COL (f, f->phys_cursor_x),
			  CHAR_TO_PIXEL_ROW (f, f->phys_cursor_y),
			  FONT_WIDTH (f->output_data.w32->font),
			  f->output_data.w32->line_height);
      /* Erase the cursor by redrawing the character underneath it.  */
      x_draw_single_glyph (f, f->phys_cursor_y, f->phys_cursor_x,
			   f->phys_cursor_glyph,
			   (mouse_face_here
			    ? 3
			    : current_glyphs->highlight[f->phys_cursor_y]));
      f->phys_cursor_on = 0;
    }

  /* If we want to show a cursor,
     or we want a box cursor and it's not so,
     write it in the right place.  */
  if (on
      && (! f->phys_cursor_on
	  || (f->output_data.w32->current_cursor != filled_box_cursor
	      && f == FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame)))
    {
      f->phys_cursor_glyph
	= ((current_glyphs->enable[curs_y]
	    && curs_x < current_glyphs->used[curs_y])
	   ? current_glyphs->glyphs[curs_y][curs_x]
	   : SPACEGLYPH);
      if (f != FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame)
	{
	  x_draw_box (f);
	  f->output_data.w32->current_cursor = hollow_box_cursor;
	}
      else
	{
	  x_draw_single_glyph (f, curs_y, curs_x,
			       f->phys_cursor_glyph, 2);
	  f->output_data.w32->current_cursor = filled_box_cursor;
	}

      f->phys_cursor_x = curs_x;
      f->phys_cursor_y = curs_y;
      f->phys_cursor_on = 1;
    }
}

/* Display the cursor on frame F, or clear it, according to ON.
   Use the position specified by curs_x and curs_y
   if we are doing an update of frame F now.
   Otherwise use the position in the FRAME_CURSOR_X and FRAME_CURSOR_Y fields
   of F.  */

x_display_cursor (f, on)
     struct frame *f;
     int on;
{
  BLOCK_INPUT;

  /* If we're not updating, then we want to use the current frame's
     cursor position, not our local idea of where the cursor ought to be.  */
  if (f != updating_frame)
    {
      curs_x = FRAME_CURSOR_X (f);
      curs_y = FRAME_CURSOR_Y (f);
    }

  if (FRAME_DESIRED_CURSOR (f) == filled_box_cursor)
    x_display_box_cursor (f, on);
  else if (FRAME_DESIRED_CURSOR (f) == bar_cursor)
    x_display_bar_cursor (f, on);
  else
    /* Those are the only two we have implemented!  */
    abort ();

  UNBLOCK_INPUT;
}

/* Icons.  */

int
x_bitmap_icon (f, icon)
     struct frame *f;
     Lisp_Object icon;
{
  int mask, bitmap_id;
  Window icon_window;
  HANDLE hicon;

  if (FRAME_W32_WINDOW (f) == 0)
    return 1;

  if (NILP (icon))
    hicon = LoadIcon (hinst, EMACS_CLASS);
  else if (STRINGP (icon))
    hicon = LoadImage (NULL, (LPCTSTR) XSTRING (icon)->data, IMAGE_ICON, 0, 0,
		       LR_DEFAULTSIZE | LR_LOADFROMFILE);
  else if (SYMBOLP (icon))
    {
      LPCTSTR name;

      if (EQ (icon, intern ("application")))
	name = (LPCTSTR) IDI_APPLICATION;
      else if (EQ (icon, intern ("hand")))
	name = (LPCTSTR) IDI_HAND;
      else if (EQ (icon, intern ("question")))
	name = (LPCTSTR) IDI_QUESTION;
      else if (EQ (icon, intern ("exclamation")))
	name = (LPCTSTR) IDI_EXCLAMATION;
      else if (EQ (icon, intern ("asterisk")))
	name = (LPCTSTR) IDI_ASTERISK;
      else if (EQ (icon, intern ("winlogo")))
	name = (LPCTSTR) IDI_WINLOGO;
      else
	return 1;

      hicon = LoadIcon (NULL, name);
    }
  else
    return 1;

  if (hicon == NULL)
    return 1;

  PostMessage (FRAME_W32_WINDOW (f), WM_SETICON, (WPARAM) ICON_BIG, (LPARAM) hicon);

  return 0;
}


/* Changing the font of the frame.  */

/* Give frame F the font named FONTNAME as its default font, and
   return the full name of that font.  FONTNAME may be a wildcard
   pattern; in that case, we choose some font that fits the pattern.
   The return value shows which font we chose.  */

Lisp_Object
x_new_font (f, fontname)
     struct frame *f;
     register char *fontname;
{
  struct font_info *fontp
    = fs_load_font (f, FRAME_W32_FONT_TABLE (f), CHARSET_ASCII,
                    fontname, -1);

  if (!fontp)
	  return Qnil;

  FRAME_FONT (f) = (XFontStruct *) (fontp->font);
  f->output_data.w32->font_baseline
    = FONT_BASE (FRAME_FONT (f)) + fontp->baseline_offset;
  FRAME_FONTSET (f) = -1;

  /* Compute the scroll bar width in character columns.  */
  if (f->scroll_bar_pixel_width > 0)
    {
      int wid = FONT_WIDTH (f->output_data.w32->font);
      f->scroll_bar_cols = (f->scroll_bar_pixel_width + wid-1) / wid;
    }
  else
    f->scroll_bar_cols = 2;

  /* Now make the frame display the given font.  */
  if (FRAME_W32_WINDOW (f) != 0)
    {
      frame_update_line_height (f);
      x_set_window_size (f, 0, f->width, f->height);
    }
  else
    /* If we are setting a new frame's font for the first time,
       there are no faces yet, so this font's height is the line height.  */
    f->output_data.w32->line_height = FONT_HEIGHT (f->output_data.w32->font);

  {
    Lisp_Object lispy_name;

    lispy_name = build_string (fontname);

    return lispy_name;
  }
}

/* Give frame F the fontset named FONTSETNAME as its default font, and
   return the full name of that fontset.  FONTSETNAME may be a wildcard
   pattern; in that case, we choose some fontset that fits the pattern.
   The return value shows which fontset we chose.  */

Lisp_Object
x_new_fontset (f, fontsetname)
     struct frame *f;
     char *fontsetname;
{
  int fontset = fs_query_fontset (f, fontsetname);
  struct fontset_info *fontsetp;
  Lisp_Object result;

  if (fontset < 0)
    return Qnil;

  if (FRAME_FONTSET (f) == fontset)
    /* This fontset is already set in frame F.  There's nothing more
       to do.  */
    return build_string (fontsetname);

  fontsetp = FRAME_FONTSET_DATA (f)->fontset_table[fontset];

  if (!fontsetp->fontname[CHARSET_ASCII])
    /* This fontset doesn't contain ASCII font.  */
    return Qnil;

  result = x_new_font (f, fontsetp->fontname[CHARSET_ASCII]);

  if (!STRINGP (result))
    /* Can't load ASCII font.  */
    return Qnil;

  /* Since x_new_font doesn't update any fontset information, do it now.  */
  FRAME_FONTSET(f) = fontset;
  FS_LOAD_FONT (f, FRAME_W32_FONT_TABLE (f),
		CHARSET_ASCII, XSTRING (result)->data, fontset);

  return build_string (fontsetname);
}

/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

x_calc_absolute_position (f)
     struct frame *f;
{
  Window win, child;
  POINT pt;
  int flags = f->output_data.w32->size_hint_flags;

  pt.x = pt.y = 0;

  /* Find the position of the outside upper-left corner of
     the inner window, with respect to the outer window.  */
  if (f->output_data.w32->parent_desc != FRAME_W32_DISPLAY_INFO (f)->root_window)
    {
      BLOCK_INPUT;
      MapWindowPoints (FRAME_W32_WINDOW (f),
		       f->output_data.w32->parent_desc,
		       &pt, 1);
      UNBLOCK_INPUT;
    }

  {
      RECT rt;
      rt.left = rt.right = rt.top = rt.bottom = 0;
      
      BLOCK_INPUT;
      AdjustWindowRect(&rt, f->output_data.w32->dwStyle,
		       FRAME_EXTERNAL_MENU_BAR (f));
      UNBLOCK_INPUT;

      pt.x += (rt.right - rt.left);
      pt.y += (rt.bottom - rt.top);
  }

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    f->output_data.w32->left_pos = (FRAME_W32_DISPLAY_INFO (f)->width
			      - 2 * f->output_data.w32->border_width - pt.x
			      - PIXEL_WIDTH (f)
			      + f->output_data.w32->left_pos);

  if (flags & YNegative)
    f->output_data.w32->top_pos = (FRAME_W32_DISPLAY_INFO (f)->height
			     - 2 * f->output_data.w32->border_width - pt.y
			     - PIXEL_HEIGHT (f)
			     + f->output_data.w32->top_pos);
  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->output_data.w32->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from x_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

x_set_offset (f, xoff, yoff, change_gravity)
     struct frame *f;
     register int xoff, yoff;
     int change_gravity;
{
  int modified_top, modified_left;

  if (change_gravity > 0)
    {
      f->output_data.w32->top_pos = yoff;
      f->output_data.w32->left_pos = xoff;
      f->output_data.w32->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->output_data.w32->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->output_data.w32->size_hint_flags |= YNegative;
      f->output_data.w32->win_gravity = NorthWestGravity;
    }
  x_calc_absolute_position (f);

  BLOCK_INPUT;
  x_wm_set_size_hint (f, (long) 0, 0);

  /* It is a mystery why we need to add the border_width here
     when the frame is already visible, but experiment says we do.  */
  modified_left = f->output_data.w32->left_pos;
  modified_top = f->output_data.w32->top_pos;
#ifndef HAVE_NTGUI
  /* Do not add in border widths under W32.  */
  if (change_gravity != 0)
    {
      modified_left += f->output_data.w32->border_width;
      modified_top += f->output_data.w32->border_width;
    }
#endif

  my_set_window_pos (FRAME_W32_WINDOW (f),
		     NULL,
		     modified_left, modified_top,
		     0, 0,
		     SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE);
  UNBLOCK_INPUT;
}

/* Call this to change the size of frame F's x-window.
   If CHANGE_GRAVITY is 1, we change to top-left-corner window gravity
   for this size change and subsequent size changes.
   Otherwise we leave the window gravity unchanged.  */

x_set_window_size (f, change_gravity, cols, rows)
     struct frame *f;
     int change_gravity;
     int cols, rows;
{
  int pixelwidth, pixelheight;
  struct w32_display_info *dpyinfo = &one_w32_display_info;
  
  BLOCK_INPUT;
  
  check_frame_size (f, &rows, &cols);
  f->output_data.w32->vertical_scroll_bar_extra
    = (!FRAME_HAS_VERTICAL_SCROLL_BARS (f)
       ? 0
       : FRAME_SCROLL_BAR_PIXEL_WIDTH (f) > 0
       ? FRAME_SCROLL_BAR_PIXEL_WIDTH (f)
       : (FRAME_SCROLL_BAR_COLS (f) * FONT_WIDTH (f->output_data.w32->font)));
  pixelwidth = CHAR_TO_PIXEL_WIDTH (f, cols);
  pixelheight = CHAR_TO_PIXEL_HEIGHT (f, rows);
  
  f->output_data.w32->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, (long) 0, 0);
  
  {
    RECT rect;

    rect.left = rect.top = 0;
    rect.right = pixelwidth;
    rect.bottom = pixelheight;
      
    AdjustWindowRect(&rect, f->output_data.w32->dwStyle,
		     FRAME_EXTERNAL_MENU_BAR (f));
      
    my_set_window_pos (FRAME_W32_WINDOW (f),
		       NULL, 
		       0, 0,
		       rect.right - rect.left,
		       rect.bottom - rect.top,
		       SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);
  }
  
  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.
     
     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.  */
  change_frame_size (f, rows, cols, 0, 0);
  PIXEL_WIDTH (f) = pixelwidth;
  PIXEL_HEIGHT (f) = pixelheight;

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  /* If cursor was outside the new size, mark it as off.  */
  if (f->phys_cursor_y >= rows
      || f->phys_cursor_x >= cols)
    {
      f->phys_cursor_x = 0;
      f->phys_cursor_y = 0;
      f->phys_cursor_on = 0;
    }

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size. 
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  UNBLOCK_INPUT;
}

/* Mouse warping.  */

void
x_set_mouse_pixel_position (f, pix_x, pix_y)
     struct frame *f;
     int pix_x, pix_y;
{
  RECT rect;
  POINT pt;

  BLOCK_INPUT;

  GetClientRect (FRAME_W32_WINDOW (f), &rect);
  pt.x = rect.left + pix_x;
  pt.y = rect.top + pix_y;
  ClientToScreen (FRAME_W32_WINDOW (f), &pt);

  SetCursorPos (pt.x, pt.y);

  UNBLOCK_INPUT;
}

void
x_set_mouse_position (f, x, y)
     struct frame *f;
     int x, y;
{
  int pix_x, pix_y;

  pix_x = CHAR_TO_PIXEL_COL (f, x) + FONT_WIDTH  (f->output_data.w32->font) / 2;
  pix_y = CHAR_TO_PIXEL_ROW (f, y) + f->output_data.w32->line_height / 2;

  if (pix_x < 0) pix_x = 0;
  if (pix_x > PIXEL_WIDTH (f)) pix_x = PIXEL_WIDTH (f);

  if (pix_y < 0) pix_y = 0;
  if (pix_y > PIXEL_HEIGHT (f)) pix_y = PIXEL_HEIGHT (f);

  x_set_mouse_pixel_position (f, pix_x, pix_y);
}

/* focus shifting, raising and lowering.  */

x_focus_on_frame (f)
     struct frame *f;
{
  struct w32_display_info *dpyinfo = &one_w32_display_info;

  /* Give input focus to frame.  */
  BLOCK_INPUT;
#if 0
  /* Try not to change its Z-order if possible.  */
  if (x_window_to_frame (dpyinfo, GetForegroundWindow ()))
    my_set_focus (f, FRAME_W32_WINDOW (f));
  else
#endif
    my_set_foreground_window (FRAME_W32_WINDOW (f));
  UNBLOCK_INPUT;
}

x_unfocus_frame (f)
     struct frame *f;
{
}

/* Raise frame F.  */

x_raise_frame (f)
     struct frame *f;
{
  BLOCK_INPUT;

  /* Strictly speaking, raise-frame should only change the frame's Z
     order, leaving input focus unchanged.  This is reasonable behaviour
     on X where the usual policy is point-to-focus.  However, this
     behaviour would be very odd on Windows where the usual policy is
     click-to-focus.

     On X, if the mouse happens to be over the raised frame, it gets
     input focus anyway (so the window with focus will never be
     completely obscured) - if not, then just moving the mouse over it
     is sufficient to give it focus.  On Windows, the user must actually
     click on the frame (preferrably the title bar so as not to move
     point), which is more awkward.  Also, no other Windows program
     raises a window to the top but leaves another window (possibly now
     completely obscured) with input focus.

     Because there is a system setting on Windows that allows the user
     to choose the point to focus policy, we make the strict semantics
     optional, but by default we grab focus when raising.  */

  if (NILP (Vw32_grab_focus_on_raise))
    {
      /* The obvious call to my_set_window_pos doesn't work if Emacs is
	 not already the foreground application: the frame is raised
	 above all other frames belonging to us, but not above the
	 current top window.  To achieve that, we have to resort to this
	 more cumbersome method.  */

      HDWP handle = BeginDeferWindowPos (2);
      if (handle)
	{
	  DeferWindowPos (handle,
			  FRAME_W32_WINDOW (f),
  			  HWND_TOP,
  			  0, 0, 0, 0,
  			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	  DeferWindowPos (handle,
			  GetForegroundWindow (),
			  FRAME_W32_WINDOW (f),
			  0, 0, 0, 0,
			  SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	  EndDeferWindowPos (handle);
	}
    }
  else
    {
      my_set_foreground_window (FRAME_W32_WINDOW (f));
    }

  UNBLOCK_INPUT;
}

/* Lower frame F.  */

x_lower_frame (f)
     struct frame *f;
{
  BLOCK_INPUT;
  my_set_window_pos (FRAME_W32_WINDOW (f),
		     HWND_BOTTOM,
		     0, 0, 0, 0,
		     SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
  UNBLOCK_INPUT;
}

static void
w32_frame_raise_lower (f, raise)
     FRAME_PTR f;
     int raise;
{
  if (raise)
    x_raise_frame (f);
  else
    x_lower_frame (f);
}

/* Change of visibility.  */

/* This tries to wait until the frame is really visible.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

x_make_frame_visible (f)
     struct frame *f;
{
  Lisp_Object type;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test FRAME_GARBAGED_P here to make sure we don't
	 call x_set_offset a second time
	 if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! f->output_data.w32->asked_for_visible)
	x_set_offset (f, f->output_data.w32->left_pos, f->output_data.w32->top_pos, 0);

      f->output_data.w32->asked_for_visible = 1;

//      my_show_window (f, FRAME_W32_WINDOW (f), f->async_iconified ? SW_RESTORE : SW_SHOW);
      my_show_window (f, FRAME_W32_WINDOW (f), SW_SHOWNORMAL);
    }

  /* Synchronize to ensure Emacs knows the frame is visible
     before we do anything else.  We do this loop with input not blocked
     so that incoming events are handled.  */
  {
    Lisp_Object frame;
    int count = input_signal_count;

    /* This must come after we set COUNT.  */
    UNBLOCK_INPUT;

    XSETFRAME (frame, f);

    while (1)
      {
	/* Once we have handled input events,
	   we should have received the MapNotify if one is coming.
	   So if we have not got it yet, stop looping.
	   Some window managers make their own decisions
	   about visibility.  */
	if (input_signal_count != count)
	  break;
	/* Machines that do polling rather than SIGIO have been observed
	   to go into a busy-wait here.  So we'll fake an alarm signal
	   to let the handler know that there's something to be read.
	   We used to raise a real alarm, but it seems that the handler
	   isn't always enabled here.  This is probably a bug.  */
	if (input_polling_used ())
	  {
	    /* It could be confusing if a real alarm arrives while processing
	       the fake one.  Turn it off and let the handler reset it.  */
	    alarm (0);
	    input_poll_signal (0);
	  }
	/* Once we have handled input events,
	   we should have received the MapNotify if one is coming.
	   So if we have not got it yet, stop looping.
	   Some window managers make their own decisions
	   about visibility.  */
	if (input_signal_count != count)
	  break;
      }
    FRAME_SAMPLE_VISIBILITY (f);
  }
}

/* Change from mapped state to withdrawn state. */

/* Make the frame visible (mapped and not iconified).  */

x_make_frame_invisible (f)
     struct frame *f;
{
  Window window;
  
  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame = 0;
  
  BLOCK_INPUT;
  
  my_show_window (f, FRAME_W32_WINDOW (f), SW_HIDE);
  
  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  f->visible = 0;
  FRAME_ICONIFIED_P (f) = 0;
  f->async_visible = 0;
  f->async_iconified = 0;
  
  UNBLOCK_INPUT;
}

/* Change window state from mapped to iconified. */

void
x_iconify_frame (f)
     struct frame *f;
{
  int result;
  Lisp_Object type;

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame == f)
    FRAME_W32_DISPLAY_INFO (f)->w32_highlight_frame = 0;

  if (f->async_iconified)
    return;

  BLOCK_INPUT;

  type = x_icon_type (f);
  if (!NILP (type))
    x_bitmap_icon (f, type);

  /* Simulate the user minimizing the frame.  */
  SendMessage (FRAME_W32_WINDOW (f), WM_SYSCOMMAND, SC_MINIMIZE, 0);

  UNBLOCK_INPUT;
}

/* Destroy the window of frame F.  */

x_destroy_window (f)
     struct frame *f;
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);

  BLOCK_INPUT;

  my_destroy_window (f, FRAME_W32_WINDOW (f));
  free_frame_menubar (f);
  free_frame_faces (f);

  xfree (f->output_data.w32);
  f->output_data.w32 = 0;
  if (f == dpyinfo->w32_focus_frame)
    dpyinfo->w32_focus_frame = 0;
  if (f == dpyinfo->w32_focus_event_frame)
    dpyinfo->w32_focus_event_frame = 0;
  if (f == dpyinfo->w32_highlight_frame)
    dpyinfo->w32_highlight_frame = 0;

  dpyinfo->reference_count--;

  if (f == dpyinfo->mouse_face_mouse_frame)
    {
      dpyinfo->mouse_face_beg_row
	= dpyinfo->mouse_face_beg_col = -1;
      dpyinfo->mouse_face_end_row
	= dpyinfo->mouse_face_end_col = -1;
      dpyinfo->mouse_face_window = Qnil;
    }

  UNBLOCK_INPUT;
}

/* Setting window manager hints.  */

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION is nonzero, we set the USPosition
   flag (this is useful when FLAGS is 0).  */

x_wm_set_size_hint (f, flags, user_position)
     struct frame *f;
     long flags;
     int user_position;
{
  Window window = FRAME_W32_WINDOW (f);

  flexlines = f->height;

  enter_crit ();

  SetWindowLong (window, WND_FONTWIDTH_INDEX, FONT_WIDTH (f->output_data.w32->font));
  SetWindowLong (window, WND_LINEHEIGHT_INDEX, f->output_data.w32->line_height);
  SetWindowLong (window, WND_BORDER_INDEX, f->output_data.w32->internal_border_width);
  SetWindowLong (window, WND_SCROLLBAR_INDEX, f->output_data.w32->vertical_scroll_bar_extra);

  leave_crit ();
}

/* Window manager things */
x_wm_set_icon_position (f, icon_x, icon_y)
     struct frame *f;
     int icon_x, icon_y;
{
#if 0
  Window window = FRAME_W32_WINDOW (f);

  f->display.x->wm_hints.flags |= IconPositionHint;
  f->display.x->wm_hints.icon_x = icon_x;
  f->display.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->display.x->wm_hints);
#endif
}


/* Initialization.  */

#ifdef USE_X_TOOLKIT
static XrmOptionDescRec emacs_options[] = {
  {"-geometry", ".geometry", XrmoptionSepArg, NULL},
  {"-iconic",   ".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {"-internal-border-width", "*EmacsScreen.internalBorderWidth",
     XrmoptionSepArg, NULL},
  {"-ib",       "*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},

  {"-T",        "*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-wn",       "*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-title",    "*EmacsShell.title", XrmoptionSepArg, (XtPointer) NULL},
  {"-iconname", "*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-in",       "*EmacsShell.iconName", XrmoptionSepArg, (XtPointer) NULL},
  {"-mc",       "*pointerColor", XrmoptionSepArg, (XtPointer) NULL},
  {"-cr",       "*cursorColor", XrmoptionSepArg, (XtPointer) NULL}
};
#endif /* USE_X_TOOLKIT */

static int w32_initialized = 0;

struct w32_display_info *
w32_term_init (display_name, xrm_option, resource_name)
     Lisp_Object display_name;
     char *xrm_option;
     char *resource_name;
{
  Lisp_Object frame;
  char *defaultvalue;
  struct w32_display_info *dpyinfo;
  HDC hdc;
  
  BLOCK_INPUT;
  
  if (!w32_initialized)
    {
      w32_initialize ();
      w32_initialized = 1;
    }
  
  {
    int argc = 0;
    char *argv[3];

    argv[0] = "";
    argc = 1;
    if (xrm_option)
      {
	argv[argc++] = "-xrm";
	argv[argc++] = xrm_option;
      }
  }
  
  dpyinfo = &one_w32_display_info;
  
  /* Put this display on the chain.  */
  dpyinfo->next = NULL;
  
  /* Put it on w32_display_name_list as well, to keep them parallel.  */ 
  w32_display_name_list = Fcons (Fcons (display_name, Qnil),
				   w32_display_name_list);
  dpyinfo->name_list_element = XCONS (w32_display_name_list)->car;
  
  dpyinfo->w32_id_name
    = (char *) xmalloc (XSTRING (Vinvocation_name)->size
			+ XSTRING (Vsystem_name)->size
			+ 2);
  sprintf (dpyinfo->w32_id_name, "%s@%s",
	   XSTRING (Vinvocation_name)->data, XSTRING (Vsystem_name)->data);

#if 0
  xrdb = x_load_resources (dpyinfo->display, xrm_option,
			   resource_name, EMACS_CLASS);
  
  /* Put the rdb where we can find it in a way that works on
     all versions.  */
  dpyinfo->xrdb = xrdb;
#endif
  hdc = GetDC (GetDesktopWindow ());
  
  dpyinfo->height = GetDeviceCaps (hdc, VERTRES);
  dpyinfo->width = GetDeviceCaps (hdc, HORZRES);
  dpyinfo->root_window = GetDesktopWindow ();
  dpyinfo->n_planes = GetDeviceCaps (hdc, PLANES);
  dpyinfo->n_cbits = GetDeviceCaps (hdc, BITSPIXEL);
  dpyinfo->height_in = GetDeviceCaps (hdc, LOGPIXELSX);
  dpyinfo->width_in = GetDeviceCaps (hdc, LOGPIXELSY);
  dpyinfo->has_palette = GetDeviceCaps (hdc, RASTERCAPS) & RC_PALETTE;
  dpyinfo->grabbed = 0;
  dpyinfo->reference_count = 0;
  dpyinfo->n_fonts = 0;
  dpyinfo->font_table_size = 0;
  dpyinfo->bitmaps = 0;
  dpyinfo->bitmaps_size = 0;
  dpyinfo->bitmaps_last = 0;
  dpyinfo->mouse_face_mouse_frame = 0;
  dpyinfo->mouse_face_deferred_gc = 0;
  dpyinfo->mouse_face_beg_row = dpyinfo->mouse_face_beg_col = -1;
  dpyinfo->mouse_face_end_row = dpyinfo->mouse_face_end_col = -1;
  dpyinfo->mouse_face_face_id = 0;
  dpyinfo->mouse_face_window = Qnil;
  dpyinfo->mouse_face_mouse_x = dpyinfo->mouse_face_mouse_y = 0;
  dpyinfo->mouse_face_defer = 0;
  dpyinfo->w32_focus_frame = 0;
  dpyinfo->w32_focus_event_frame = 0;
  dpyinfo->w32_highlight_frame = 0;
  
  ReleaseDC (GetDesktopWindow (), hdc);

  /* Determine if there is a middle mouse button, to allow parse_button
     to decide whether right mouse events should be mouse-2 or
     mouse-3. */
  XSETINT (Vw32_num_mouse_buttons, GetSystemMetrics (SM_CMOUSEBUTTONS));

  /* initialise palette with white and black */
  {
    COLORREF color;
    defined_color (0, "white", &color, 1);
    defined_color (0, "black", &color, 1);
  }

#ifndef F_SETOWN_BUG
#ifdef F_SETOWN
#ifdef F_SETOWN_SOCK_NEG
  /* stdin is a socket here */
  fcntl (connection, F_SETOWN, -getpid ());
#else /* ! defined (F_SETOWN_SOCK_NEG) */
  fcntl (connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN_SOCK_NEG) */
#endif /* ! defined (F_SETOWN) */
#endif /* F_SETOWN_BUG */

#ifdef SIGIO
  if (interrupt_input)
    init_sigio (connection);
#endif /* ! defined (SIGIO) */

  UNBLOCK_INPUT;

  return dpyinfo;
}

/* Get rid of display DPYINFO, assuming all frames are already gone.  */

void
x_delete_display (dpyinfo)
     struct w32_display_info *dpyinfo;
{
  /* Discard this display from w32_display_name_list and w32_display_list.
     We can't use Fdelq because that can quit.  */
  if (! NILP (w32_display_name_list)
      && EQ (XCONS (w32_display_name_list)->car, dpyinfo->name_list_element))
    w32_display_name_list = XCONS (w32_display_name_list)->cdr;
  else
    {
      Lisp_Object tail;

      tail = w32_display_name_list;
      while (CONSP (tail) && CONSP (XCONS (tail)->cdr))
	{
	  if (EQ (XCONS (XCONS (tail)->cdr)->car,
		  dpyinfo->name_list_element))
	    {
	      XCONS (tail)->cdr = XCONS (XCONS (tail)->cdr)->cdr;
	      break;
	    }
	  tail = XCONS (tail)->cdr;
	}
    }

  /* free palette table */
  {
    struct w32_palette_entry * plist;

    plist = dpyinfo->color_list;
    while (plist)
    {
      struct w32_palette_entry * pentry = plist;
      plist = plist->next;
      xfree(pentry);
    }
    dpyinfo->color_list = NULL;
    if (dpyinfo->palette)
      DeleteObject(dpyinfo->palette);
  }
  xfree (dpyinfo->font_table);
  xfree (dpyinfo->w32_id_name);
}

/* Set up use of W32.  */

DWORD w32_msg_worker ();

w32_initialize ()
{
  /* MSVC does not type K&R functions with no arguments correctly, and
     so we must explicitly cast them.  */
  clear_frame_hook = (void (*)(void)) w32_clear_frame;
  clear_end_of_line_hook = w32_clear_end_of_line;
  ins_del_lines_hook = w32_ins_del_lines;
  change_line_highlight_hook = w32_change_line_highlight;
  insert_glyphs_hook = w32_insert_glyphs;
  write_glyphs_hook = w32_write_glyphs;
  delete_glyphs_hook = w32_delete_glyphs;
  ring_bell_hook = (void (*)(void)) w32_ring_bell;
  reset_terminal_modes_hook = (void (*)(void)) w32_reset_terminal_modes;
  set_terminal_modes_hook = (void (*)(void)) w32_set_terminal_modes;
  update_begin_hook = w32_update_begin;
  update_end_hook = w32_update_end;
  set_terminal_window_hook = w32_set_terminal_window;
  read_socket_hook = w32_read_socket;
  frame_up_to_date_hook = w32_frame_up_to_date;
  cursor_to_hook = w32_cursor_to;
  reassert_line_highlight_hook = w32_reassert_line_highlight;
  mouse_position_hook = w32_mouse_position;
  frame_rehighlight_hook = w32_frame_rehighlight;
  frame_raise_lower_hook = w32_frame_raise_lower;
  set_vertical_scroll_bar_hook = w32_set_vertical_scroll_bar;
  condemn_scroll_bars_hook = w32_condemn_scroll_bars;
  redeem_scroll_bar_hook = w32_redeem_scroll_bar;
  judge_scroll_bars_hook = w32_judge_scroll_bars;

  scroll_region_ok = 1;         /* we'll scroll partial frames */
  char_ins_del_ok = 0;          /* just as fast to write the line */
  line_ins_del_ok = 1;          /* we'll just blt 'em */
  fast_clear_end_of_line = 1;   /* X does this well */
  memory_below_frame = 0;       /* we don't remember what scrolls
				   off the bottom */
  baud_rate = 19200;

  /* Initialize input mode: interrupt_input off, no flow control, allow
     8 bit character input, standard quit char.  */
  Fset_input_mode (Qnil, Qnil, make_number (2), Qnil);

  /* Create the window thread - it will terminate itself or when the app terminates */

  init_crit ();

  dwMainThreadId = GetCurrentThreadId ();
  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (), 
		   GetCurrentProcess (), &hMainThread, 0, TRUE, DUPLICATE_SAME_ACCESS);

  /* Wait for thread to start */

  {
    MSG msg;

    PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE);

    hWindowsThread = CreateThread (NULL, 0, 
			       (LPTHREAD_START_ROUTINE) w32_msg_worker, 
			       0, 0, &dwWindowsThreadId);

    GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);
  }
  
  /* It is desirable that mainThread should have the same notion of
     focus window and active window as windowsThread.  Unfortunately, the
     following call to AttachThreadInput, which should do precisely what
     we need, causes major problems when Emacs is linked as a console
     program.  Unfortunately, we have good reasons for doing that, so
     instead we need to send messages to windowsThread to make some API
     calls for us (ones that affect, or depend on, the active/focus
     window state.  */
#ifdef ATTACH_THREADS
  AttachThreadInput (dwMainThreadId, dwWindowsThreadId, TRUE);
#endif

  /* Dynamically link to optional system components. */
  {
    HANDLE user_lib = LoadLibrary ("user32.dll");

#define LOAD_PROC(fn) pfn##fn = (void *) GetProcAddress (user_lib, #fn)

    /* New proportional scroll bar functions. */
    LOAD_PROC( SetScrollInfo );
    LOAD_PROC( GetScrollInfo );

#undef LOAD_PROC

    FreeLibrary (user_lib);

    /* If using proportional scroll bars, ensure handle is at least 5 pixels;
       otherwise use the fixed height.  */
    vertical_scroll_bar_min_handle = (pfnSetScrollInfo != NULL) ? 5 :
      GetSystemMetrics (SM_CYVTHUMB);

    /* For either kind of scroll bar, take account of the arrows; these
       effectively form the border of the main scroll bar range.  */
    vertical_scroll_bar_top_border = vertical_scroll_bar_bottom_border
      = GetSystemMetrics (SM_CYVSCROLL);
  }
}

void
syms_of_w32term ()
{
  Lisp_Object codepage;

  staticpro (&w32_display_name_list);
  w32_display_name_list = Qnil;

  staticpro (&last_mouse_scroll_bar);
  last_mouse_scroll_bar = Qnil;

  staticpro (&Qvendor_specific_keysyms);
  Qvendor_specific_keysyms = intern ("vendor-specific-keysyms");

  DEFVAR_INT ("w32-num-mouse-buttons",
	      &Vw32_num_mouse_buttons,
	      "Number of physical mouse buttons.");
  Vw32_num_mouse_buttons = Qnil;

  DEFVAR_LISP ("w32-swap-mouse-buttons",
	      &Vw32_swap_mouse_buttons,
	      "Swap the mapping of middle and right mouse buttons.\n\
When nil, middle button is mouse-2 and right button is mouse-3.");
  Vw32_swap_mouse_buttons = Qnil;

  DEFVAR_LISP ("w32-grab-focus-on-raise",
	       &Vw32_grab_focus_on_raise,
	       "Raised frame grabs input focus.\n\
When t, `raise-frame' grabs input focus as well.  This fits well\n\
with the normal Windows click-to-focus policy, but might not be\n\
desirable when using a point-to-focus policy.");
  Vw32_grab_focus_on_raise = Qt;

  DEFVAR_LISP ("w32-capslock-is-shiftlock",
	       &Vw32_capslock_is_shiftlock,
	       "Apply CapsLock state to non character input keys.\n\
When nil, CapsLock only affects normal character input keys.");
  Vw32_capslock_is_shiftlock = Qnil;

  DEFVAR_LISP ("w32-recognize-altgr",
	       &Vw32_recognize_altgr,
	       "Recognize right-alt and left-ctrl as AltGr.\n\
When nil, the right-alt and left-ctrl key combination is\n\
interpreted normally."); 
  Vw32_recognize_altgr = Qt;

  DEFVAR_BOOL ("w32-enable-unicode-output",
               &w32_enable_unicode_output,
               "Enable the use of Unicode for text output if non-nil.\n\
Unicode output may prevent some third party applications for displaying\n\
Far-East Languages on Windows 95/98 from working properly.\n\
NT uses Unicode internally anyway, so this flag will probably have no\n\
affect on NT machines.");
  w32_enable_unicode_output = 1;

  DEFVAR_LISP ("w32-charset-to-codepage-alist",
               &Vw32_charset_to_codepage_alist,
               "Alist linking character sets to Windows Codepages.");
  Vw32_charset_to_codepage_alist = Qnil;
  /* Initialise the alist with some defaults.  */
  XSETFASTINT (codepage, 936);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("gb2312"), codepage);
  XSETFASTINT (codepage, 950);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("big5"), codepage);
  XSETFASTINT (codepage, 949);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("ksc5601.1987"), codepage);
  XSETFASTINT (codepage, 1361);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("ksc5601.1992"), codepage);
  XSETFASTINT (codepage, 932);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("jisx0208-sjis"), codepage);
  XSETFASTINT (codepage, 874);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("tis620"), codepage);
  XSETFASTINT (codepage, 20866);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("koi8-r"), codepage);
  /* iso8859-13 is not yet officially adopted, but it is conveniently
     covered by CP 1257.  */
  XSETFASTINT (codepage, 1257);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-13"), codepage);
  XSETFASTINT (codepage, 1254);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-9"), codepage);
  XSETFASTINT (codepage, 1255);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-8"), codepage);
  XSETFASTINT (codepage, 28597);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-7"), codepage);
  XSETFASTINT (codepage, 28596);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-6"), codepage);
  XSETFASTINT (codepage, 28595);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-5"), codepage);
  XSETFASTINT (codepage, 28594);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-4"), codepage);
  XSETFASTINT (codepage, 28593);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-3"), codepage);
  XSETFASTINT (codepage, 28592);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-2"), codepage);
  XSETFASTINT (codepage, 1252);
  store_in_alist (&Vw32_charset_to_codepage_alist,
                  build_string ("iso8859-1"), codepage);
}
