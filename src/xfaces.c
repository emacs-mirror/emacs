/* "Face" primitives.
   Copyright (C) 1992, 1993 Free Software Foundation.

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
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This is derived from work by Lucid (some parts very loosely so).  */

#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"
#include "lisp.h"

#ifdef HAVE_X_WINDOWS

#include "xterm.h"
#include "buffer.h"
#include "dispextern.h"
#include "frame.h"
#include "blockinput.h"
#include "window.h"

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
/* #include <X11/Xmu/Drawing.h> */  /* Appears not to be used */
#include <X11/Xos.h>


/* An explanation of the face data structures.  */

/* ========================= Face Data Structures =========================

   All lisp code uses symbols as face names.

   Each frame has a face_alist member (with the frame-face-alist and
   set-frame-face-alist accessors), associating the face names with
   vectors of the form 
       [face NAME ID FONT FOREGROUND BACKGROUND BACKGROUND-PIXMAP UNDERLINE-P]
   where
       face is the symbol `face',
       NAME is the symbol with which this vector is associated (a backpointer),
       ID is the face ID, an integer used internally by the C code to identify
           the face,
       FONT, FOREGROUND, and BACKGROUND are strings naming the fonts and colors
           to use with the face,
       BACKGROUND-PIXMAP is the name of an x bitmap filename, which we don't
           use right now, and
       UNDERLINE-P is non-nil if the face should be underlined.
   (lisp/faces.el maintains these association lists.)

   The frames' private alists hold the frame-local definitions for the
   faces.  The lisp variable global-face-data contains the global
   defaults for faces.  (See lisp/faces.el for this too.)

   In the C code, we also have a `struct face' with the elements
      `foreground', `background', `font', and `underline',
   which specify its visual appearance, and elements
      `gc' and `cached_index';
   `gc' may be an X GC which has been built for the given display
   parameters.  Faces with GC's are called `display faces'.  Whether
   or not a face has a GC depends on what data structure the face is
   in; we explain these more below.  (See src/dispextern.h.)

   Each frame also has members called `faces' and `n_faces' (with the
   accessors FRAME_FACES and FRAME_N_FACES), which define an array of
   struct face pointers, indexed by face ID (element 2 of the
   vector).  These are called "frame faces".
      Element 0 is the default face --- the one used for normal text.
      Element 1 is the modeline face.
   These faces have their GC's set; the rest do not.  (See src/xterm.h.)

   The global variables `face_vector' and `nfaces' define another
   array of struct face pointers, with their GC's set.  This array
   acts as a cache of GC's to be used by all frames.  The function
   `intern_face', passed a struct face *, searches face_vector for a
   struct face with the same parameters, adds a new one with a GC if
   it doesn't find one, and returns it.  If you have a `struct face',
   and you want a GC for it, call intern_face on that struct, and it
   will return a `struct face *' with its GC set.  The faces in
   face_vector are called `cached faces.' (See src/xfaces.c.)

   The `GLYPH' data type is an unsigned integer type; the bottom byte
   is a character code, and the byte above that is a face id.  The
   `struct frame_glyphs' structure, used to describe frames' current
   or desired contents, is essentially a matrix of GLYPHs; the face
   ID's in a struct frame_glyphs are indices into FRAME_FACES.  (See
   src/dispextern.h.)

   Some subtleties:
   
   Since face_vector is just a cache --- there are no pointers into it
   from the rest of the code, and everyone accesses it through
   intern_face --- we could just free its GC's and throw the whole
   thing away without breaking anything.  This gives us a simple way
   to garbage-collect old GC's nobody's using any more - we can just
   purge face_vector, and then let subsequent calls to intern_face
   refill it as needed.  The function clear_face_vector performs this
   purge.

   We're often applying intern_face to faces in frames' local arrays -
   for example, we do this while sending GLYPHs from a struct
   frame_glyphs to X during redisplay.  It would be nice to avoid
   searching all of face_vector every time we intern a frame's face.
   So, when intern_face finds a match for FACE in face_vector, it
   stores the index of the match in FACE's cached_index member, and
   checks there first next time.  */
   

/* Definitions and declarations.  */

/* A table of display faces.  */
struct face **face_vector;
/* The length in use of the table.  */
int nfaces;
/* The allocated length of the table.   */
int nfaces_allocated;

/* The number of face-id's in use (same for all frames).  */
int next_face_id;

/* The number of the face to use to indicate the region.  */
int region_face;

/* This is what appears in a slot in a face to signify that the face
   does not specify that display aspect.  */
#define FACE_DEFAULT (~0)

Lisp_Object Qface, Qwindow, Qpriority;

static void build_face ();
static Lisp_Object face_name_id_number ();

struct face *intern_face ();
static void ensure_face_ready ();

/* Allocating, copying, and comparing struct faces.  */

/* Allocate a new face */
static struct face *
allocate_face ()
{
  struct face *result = (struct face *) xmalloc (sizeof (struct face));
  bzero (result, sizeof (struct face));
  result->font = (XFontStruct *) FACE_DEFAULT;
  result->foreground = FACE_DEFAULT;
  result->background = FACE_DEFAULT;
  result->stipple = FACE_DEFAULT;
  return result;
}

/* Make a new face that's a copy of an existing one.  */
static struct face *
copy_face (face)
     struct face *face;
{
  struct face *result = allocate_face ();

  result->font = face->font;
  result->foreground = face->foreground;
  result->background = face->background;
  result->stipple = face->stipple;
  result->underline = face->underline;

  return result;
}

static int
face_eql (face1, face2)
     struct face *face1, *face2;
{
  return (   face1->font       == face2->font
	  && face1->foreground == face2->foreground
	  && face1->background == face2->background
	  && face1->stipple    == face2->stipple
	  && face1->underline  == face2->underline);
}

/* Interning faces in the `face_vector' cache, and clearing that cache.  */

/* Return the unique display face corresponding to the user-level face FACE.
   If there isn't one, make one, and find a slot in the face_vector to
   put it in.  */
static struct face *
get_cached_face (f, face)
     struct frame *f;
     struct face *face;
{
  int i, empty = -1;
  struct face *result;

  /* Perhaps FACE->cached_index is valid; this could happen if FACE is
     in a frame's face list.  */
  if (face->cached_index >= 0
      && face->cached_index < nfaces
      && face_eql (face_vector[face->cached_index], face))
    return face_vector[face->cached_index];

  /* Look for an existing display face that does the job.
     Also find an empty slot if any.   */
  for (i = 0; i < nfaces; i++)
    {
      if (face_eql (face_vector[i], face))
	return face_vector[i];
      if (face_vector[i] == 0)
	empty = i;
    }

  /* If no empty slots, make one.  */
  if (empty < 0 && nfaces == nfaces_allocated)
    {
      int newsize = nfaces + 20;
      face_vector
	= (struct face **) xrealloc (face_vector,
				     newsize * sizeof (struct face *));
      nfaces_allocated = newsize;
    }

  if (empty < 0)
    empty = nfaces++;

  /* Put a new display face in the empty slot.  */
  result = copy_face (face);
  face_vector[empty] = result;
  
  /* Make a graphics context for it.  */
  build_face (f, result);

  return result;
}

/* Given a frame face, return an equivalent display face
   (one which has a graphics context).  */

struct face *
intern_face (f, face)
     struct frame *f;
     struct face *face;
{
  /* If it's equivalent to the default face, use that.  */
  if (face_eql (face, FRAME_DEFAULT_FACE (f)))
    {
      if (!FRAME_DEFAULT_FACE (f)->gc)
	build_face (f, FRAME_DEFAULT_FACE (f));
      return FRAME_DEFAULT_FACE (f);
    }
  
  /* If it's equivalent to the mode line face, use that.  */
  if (face_eql (face, FRAME_MODE_LINE_FACE (f)))
    {
      if (!FRAME_MODE_LINE_FACE (f)->gc)
	build_face (f, FRAME_MODE_LINE_FACE (f));
      return FRAME_MODE_LINE_FACE (f);
    }

  /* If it's not one of the frame's default faces, it shouldn't have a GC.  */
  if (face->gc)
    abort ();
  
  /* Get a specialized display face.  */
  return get_cached_face (f, face);
}

/* Clear out face_vector and start anew.
   This should be done from time to time just to avoid
   keeping too many graphics contexts in face_vector
   that are no longer needed.  */

void
clear_face_vector ()
{
  Lisp_Object rest;
  Display *dpy = x_current_display;
  int i;

  BLOCK_INPUT;
  /* Free the display faces in the face_vector.  */
  for (i = 0; i < nfaces; i++)
    {
      struct face *face = face_vector[i];
      if (face->gc)
	XFreeGC (dpy, face->gc);
      xfree (face);
    }
  nfaces = 0;

  UNBLOCK_INPUT;
}

/* Allocating and freeing X resources for display faces.  */

/* Make a graphics context for face FACE, which is on frame F,
   if that can be done.  */
static void
build_face (f, face)
     struct frame *f;
     struct face *face;
{
  GC gc;
  XGCValues xgcv;
  unsigned long mask;

  if (face->foreground != FACE_DEFAULT)
    xgcv.foreground = face->foreground;
  else
    xgcv. foreground = f->display.x->foreground_pixel;
  if (face->background != FACE_DEFAULT)
    xgcv.background = face->background;
  else
    xgcv. background = f->display.x->background_pixel;
  if (face->font && (int) face->font != FACE_DEFAULT)
    xgcv.font = face->font->fid;
  else
    xgcv.font = f->display.x->font->fid;
  xgcv.graphics_exposures = 0;
  mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
  gc = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
		  mask, &xgcv);
#if 0
  if (face->stipple && face->stipple != FACE_DEFAULT)
    XSetStipple (x_current_display, gc, face->stipple);
#endif
  face->gc = gc;
}

/* Allocating, freeing, and duplicating fonts, colors, and pixmaps.  */

static XFontStruct *
load_font (f, name)
     struct frame *f;
     Lisp_Object name;
{
  XFontStruct *font;

  if (NILP (name))
    return (XFontStruct *) FACE_DEFAULT;

  CHECK_STRING (name, 0);
  BLOCK_INPUT;
  font = XLoadQueryFont (x_current_display, (char *) XSTRING (name)->data);
  UNBLOCK_INPUT;

  if (! font)
    Fsignal (Qerror, Fcons (build_string ("undefined font"),
			    Fcons (name, Qnil)));
  return font;
}

static void
unload_font (f, font)
     struct frame *f;
     XFontStruct *font;
{
  if (!font || font == ((XFontStruct *) FACE_DEFAULT))
    return;
  XFreeFont (x_current_display, font);
}

static unsigned long
load_color (f, name)
     struct frame *f;
     Lisp_Object name;
{
  Display *dpy = x_current_display;
  Colormap cmap;
  XColor color;
  int result;

  if (NILP (name))
    return FACE_DEFAULT;

  cmap = DefaultColormapOfScreen (DefaultScreenOfDisplay (x_current_display));

  CHECK_STRING (name, 0);
  BLOCK_INPUT;
  result = XParseColor (dpy, cmap, (char *) XSTRING (name)->data, &color);
  UNBLOCK_INPUT;
  if (! result)
    Fsignal (Qerror, Fcons (build_string ("undefined color"),
			    Fcons (name, Qnil)));
  BLOCK_INPUT;
  result = XAllocColor (dpy, cmap, &color);
  UNBLOCK_INPUT;
  if (! result)
    Fsignal (Qerror, Fcons (build_string ("X server cannot allocate color"),
			    Fcons (name, Qnil)));
  return (unsigned long) color.pixel;
}

static void
unload_color (f, pixel)
     struct frame *f;
     Pixel pixel;
{
  Colormap cmap;
  Display *dpy = x_current_display;
  if (pixel == FACE_DEFAULT
      || pixel == BLACK_PIX_DEFAULT
      || pixel == WHITE_PIX_DEFAULT)
    return;
  cmap = DefaultColormapOfScreen (DefaultScreenOfDisplay (x_current_display));
  BLOCK_INPUT;
  XFreeColors (dpy, cmap, &pixel, 1, 0);
  UNBLOCK_INPUT;
}

/* Initializing face arrays for frames. */

/* Set up faces 0 and 1 based on the normal text and modeline GC's.  */
void
init_frame_faces (f)
     struct frame *f;
{
  ensure_face_ready (f, 0);
  {
    XGCValues gcv;
    struct face *face = FRAME_FACES (f) [0];

    XGetGCValues (x_current_display, f->display.x->normal_gc,
		  GCForeground | GCBackground | GCFont, &gcv);
    face->gc         = f->display.x->normal_gc;
    face->foreground = gcv.foreground;
    face->background = gcv.background;
    face->font       = f->display.x->font;
    face->stipple = 0;
    face->underline = 0;
  }

  ensure_face_ready (f, 1);
  {
    XGCValues gcv;
    struct face *face = FRAME_FACES (f) [1];

    XGetGCValues (x_current_display, f->display.x->reverse_gc,
		  GCForeground | GCBackground | GCFont, &gcv);
    face->gc         = f->display.x->reverse_gc;
    face->foreground = gcv.foreground;
    face->background = gcv.background;
    face->font       = f->display.x->font;
    face->stipple = 0;
    face->underline = 0;
  }
}

/* Called from Fdelete_frame.  */
void
free_frame_faces (f)
     struct frame *f;
{
  Display *dpy = x_current_display;
  int i;

  /* The first two faces on the frame are just made of resources which 
     we borrowed from the frame's GC's, so don't free them.  Let
     them get freed by the x_destroy_window code.  */
  for (i = 2; i < FRAME_N_FACES (f); i++)
    {
      struct face *face = FRAME_FACES (f) [i];
      if (! face)
        continue;
      if (face->gc)
	XFreeGC (dpy, face->gc);
      unload_font (f, face->font);
      unload_color (f, face->foreground);
      unload_color (f, face->background);
#if 0
      unload_pixmap (f, face->stipple);
#endif
      xfree (face);
    }
  xfree (FRAME_FACES (f));
  FRAME_FACES (f) = 0;
  FRAME_N_FACES (f) = 0;
}

/* Interning faces in a frame's face array.  */

/* Find a match for NEW_FACE in a FRAME's face array, and add it if we don't
   find one.  */
int
intern_frame_face (frame, new_face)
     struct frame *frame;
     struct face *new_face;
{
  int len = FRAME_N_FACES (frame);
  int i;

  /* Search for a face already on FRAME equivalent to FACE.  */
  for (i = 0; i < len; i++)
    {
      struct face *frame_face = FRAME_FACES (frame)[i];
      
      if (frame_face && face_eql (new_face, frame_face))
	return i;
    }

  /* We didn't find one; add a new one.  */
  i = next_face_id++;

  ensure_face_ready (frame, i);
  bcopy (new_face, FRAME_FACES (frame)[i], sizeof (*new_face));

  return i;
}

/* Make face id ID valid on frame F.  */

static void
ensure_face_ready (f, id)
     struct frame *f;
     int id;
{
  if (FRAME_N_FACES (f) <= id)
    {
      int n = id + 10;
      int i;
      if (!FRAME_N_FACES (f))
	FRAME_FACES (f)
	  = (struct face **) xmalloc (sizeof (struct face *) * n);
      else
	FRAME_FACES (f)
	  = (struct face **) xrealloc (FRAME_FACES (f),
				       sizeof (struct face *) * n);

      bzero (FRAME_FACES (f) + FRAME_N_FACES (f),
	     (n - FRAME_N_FACES (f)) * sizeof (struct face *));
      FRAME_N_FACES (f) = n;
    }

  if (FRAME_FACES (f) [id] == 0)
    FRAME_FACES (f) [id] = allocate_face ();
}

/* Computing faces appropriate for a given piece of text in a buffer.  */

/* Modify face TO by copying from FROM all properties which have
   nondefault settings.  */
static void 
merge_faces (from, to)
     struct face *from, *to;
{
  if (from->font != (XFontStruct *)FACE_DEFAULT)
    {
      to->font = from->font;
    }
  if (from->foreground != FACE_DEFAULT)
    to->foreground = from->foreground;
  if (from->background != FACE_DEFAULT)
    to->background = from->background;
  if (from->stipple != FACE_DEFAULT)
    to->stipple = from->stipple;
  if (from->underline)
    to->underline = from->underline;
}

struct sortvec
{
  Lisp_Object overlay;
  int beg, end;
  int priority;
};

static int
sort_overlays (s1, s2)
     struct sortvec *s1, *s2;
{
  if (s1->priority != s2->priority)
    return s1->priority - s2->priority;
  if (s1->beg != s2->beg)
    return s1->beg - s2->beg;
  if (s1->end != s2->end)
    return s2->end - s1->end;
  return 0;
}

/* Return the face ID associated with a buffer position POS.
   Store into *ENDPTR the position at which a different face is needed.
   This does not take account of glyphs that specify their own face codes.
   F is the frame in use for display, and W is a window displaying
   the current buffer.

   REGION_BEG, REGION_END delimit the region, so it can be highlighted.  */

int
compute_char_face (f, w, pos, region_beg, region_end, endptr)
     struct frame *f;
     struct window *w;
     int pos;
     int region_beg, region_end;
     int *endptr;
{
  struct face face;
  Lisp_Object prop, position;
  int i, j, noverlays;
  int facecode;
  Lisp_Object *overlay_vec;
  struct sortvec *sortvec;
  Lisp_Object frame;
  int endpos;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  if (XBUFFER (w->buffer) != current_buffer)
    abort ();

  XSET (frame, Lisp_Frame, f);

  endpos = ZV;
  if (pos < region_beg && region_beg < endpos)
    endpos = region_beg;

  XFASTINT (position) = pos;
  prop = Fget_text_property (position, Qface, w->buffer);
  {
    Lisp_Object end;

    end = Fnext_single_property_change (position, Qface, w->buffer);
    if (INTEGERP (end))
      endpos = XINT (end);
  }

  {
    int next_overlay;
    int len;

    /* First try with room for 40 overlays.  */
    len = 40;
    overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
    
    noverlays = overlays_at (pos, 0, &overlay_vec, &len, &next_overlay);

    /* If there are more than 40,
       make enough space for all, and try again.  */
    if (noverlays > len)
      {
	len = noverlays;
	overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	noverlays = overlays_at (pos, 0, &overlay_vec, &len, &next_overlay);
      }

    if (next_overlay < endpos)
      endpos = next_overlay;
  }

  *endptr = endpos;

  /* Optimize the default case.  */
  if (noverlays == 0 && NILP (prop)
      && !(pos >= region_beg && pos < region_end))
    return 0;

  bcopy (FRAME_DEFAULT_FACE (f), &face, sizeof (struct face));
  face.gc = 0;

  if (!NILP (prop))
    {
      facecode = face_name_id_number (frame, prop);
      if (facecode >= 0 && facecode < FRAME_N_FACES (f)
	  && FRAME_FACES (f) [facecode] != 0)
	merge_faces (FRAME_FACES (f) [facecode], &face);
    }

  /* Put the valid and relevant overlays into sortvec.  */
  sortvec = (struct sortvec *) alloca (noverlays * sizeof (struct sortvec));

  for (i = 0, j = 0; i < noverlays; i++)
    {
      Lisp_Object overlay = overlay_vec[i];

      if (OVERLAY_VALID (overlay)
	  && OVERLAY_POSITION (OVERLAY_START (overlay)) > 0
	  && OVERLAY_POSITION (OVERLAY_END (overlay)) > 0)
	{
	  Lisp_Object window;
	  window = Foverlay_get (overlay, Qwindow);

	  /* Also ignore overlays limited to one window
	     if it's not the window we are using.  */
	  if (XTYPE (window) != Lisp_Window
	      || XWINDOW (window) == w)
	    {
	      Lisp_Object tem;

	      /* This overlay is good and counts:
		 put it in sortvec.  */
	      sortvec[j].overlay = overlay;
	      sortvec[j].beg = OVERLAY_POSITION (OVERLAY_START (overlay));
	      sortvec[j].end = OVERLAY_POSITION (OVERLAY_END (overlay));
	      tem = Foverlay_get (overlay, Qpriority);
	      if (INTEGERP (tem))
		sortvec[j].priority = XINT (tem);
	      else
		sortvec[j].priority = 0;
	      j++;
	    }
	}
    }
  noverlays = j;

  /* Sort the overlays into the proper order: increasing priority.  */

  if (noverlays > 1)
    qsort (sortvec, noverlays, sizeof (struct sortvec), sort_overlays);

  /* Now merge the overlay data in that order.  */
  for (i = 0; i < noverlays; i++)
    {
      prop = Foverlay_get (sortvec[i].overlay, Qface);
      if (!NILP (prop))
	{
	  Lisp_Object oend;
	  int oendpos;

	  facecode = face_name_id_number (frame, prop);
	  if (facecode >= 0 && facecode < FRAME_N_FACES (f)
	      && FRAME_FACES (f) [facecode] != 0)
	    merge_faces (FRAME_FACES (f) [facecode], &face);

	  oend = OVERLAY_END (sortvec[i].overlay);
	  oendpos = OVERLAY_POSITION (oend);
	  if (oendpos < endpos)
	    endpos = oendpos;
	}
    }

  if (pos >= region_beg && pos < region_end)
    {
      if (region_end < endpos)
	endpos = region_end;
      if (region_face >= 0 && region_face < next_face_id)
	merge_faces (FRAME_FACES (f) [region_face], &face);
    }

  *endptr = endpos;

  return intern_frame_face (f, &face);
}

/* Return the face ID to use to display a special glyph which selects
   FACE_CODE as the face ID, assuming that ordinarily the face would
   be BASIC_FACE.  F is the frame.  */
int
compute_glyph_face (f, face_code)
     struct frame *f;
     int face_code;
{
  struct face face;

  bcopy (FRAME_DEFAULT_FACE (f), &face, sizeof (face));
  face.gc = 0;

  if (face_code >= 0 && face_code < FRAME_N_FACES (f)
      && FRAME_FACES (f) [face_code] != 0)
    merge_faces (FRAME_FACES (f) [face_code], &face);

  return intern_frame_face (f, &face);
}

/* Lisp interface. */

DEFUN ("frame-face-alist", Fframe_face_alist, Sframe_face_alist, 1, 1, 0,
       "")
     (frame)
     Lisp_Object frame;
{
  CHECK_FRAME (frame, 0);
  return XFRAME (frame)->face_alist;
}

DEFUN ("set-frame-face-alist", Fset_frame_face_alist, Sset_frame_face_alist,
       2, 2, 0, "")
     (frame, value)
     Lisp_Object frame, value;
{
  CHECK_FRAME (frame, 0);
  XFRAME (frame)->face_alist = value;
  return value;
}


DEFUN ("make-face-internal", Fmake_face_internal, Smake_face_internal, 1, 1, 0,
  "Create face number FACE-ID on all frames.")
  (face_id)
     Lisp_Object face_id;
{
  Lisp_Object rest;
  int id = XINT (face_id);

  CHECK_NUMBER (face_id, 0);
  if (id < 0 || id >= next_face_id)
    error ("Face id out of range");

  for (rest = Vframe_list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      struct frame *f = XFRAME (XCONS (rest)->car);
      if (FRAME_X_P (f))
	ensure_face_ready (f, id);
    }
  return Qnil;
}


DEFUN ("set-face-attribute-internal", Fset_face_attribute_internal,
       Sset_face_attribute_internal, 4, 4, 0, "")
     (face_id, attr_name, attr_value, frame)
     Lisp_Object face_id, attr_name, attr_value, frame;
{
  struct face *face;
  struct frame *f;
  int magic_p;
  int id;

  CHECK_FRAME (frame, 0);
  CHECK_NUMBER (face_id, 0);
  CHECK_SYMBOL (attr_name, 0);

  f = XFRAME (frame);
  id = XINT (face_id);
  if (id < 0 || id >= next_face_id)
    error ("Face id out of range");

  if (! FRAME_X_P (f))
    return;

  ensure_face_ready (f, id);
  face = FRAME_FACES (f) [XFASTINT (face_id)];

  if (EQ (attr_name, intern ("font")))
    {
      XFontStruct *font = load_font (f, attr_value);
      unload_font (f, face->font);
      face->font = font;
    }
  else if (EQ (attr_name, intern ("foreground")))
    {
      unsigned long new_color = load_color (f, attr_value);
      unload_color (f, face->foreground);
      face->foreground = new_color;
    }
  else if (EQ (attr_name, intern ("background")))
    {
      unsigned long new_color = load_color (f, attr_value);
      unload_color (f, face->background);
      face->background = new_color;
    }
#if 0
  else if (EQ (attr_name, intern ("background-pixmap")))
    {
      unsigned int w, h, d;
      unsigned long new_pixmap = load_pixmap (f, attr_value, &w, &h, &d, 0);
      unload_pixmap (f, face->stipple);
      if (NILP (attr_value))
	new_pixmap = 0;
      face->stipple = new_pixmap;
      face->pixmap_w = w;
      face->pixmap_h = h;
/*      face->pixmap_depth = d; */
    }
#endif /* 0 */
  else if (EQ (attr_name, intern ("underline")))
    {
      int new = !NILP (attr_value);
      face->underline = new;
    }
  else
    error ("unknown face attribute");

  if (id == 0)
    {
      BLOCK_INPUT;
      if (FRAME_DEFAULT_FACE (f)->gc != 0)
	XFreeGC (x_current_display, FRAME_DEFAULT_FACE (f)->gc);
      build_face (f, FRAME_DEFAULT_FACE (f));
      UNBLOCK_INPUT;
    }

  if (id == 1)
    {
      BLOCK_INPUT;
      if (FRAME_MODE_LINE_FACE (f)->gc != 0)
	XFreeGC (x_current_display, FRAME_MODE_LINE_FACE (f)->gc);
      build_face (f, FRAME_MODE_LINE_FACE (f));
      UNBLOCK_INPUT;
    }

  /* If we're modifying either of the frame's display faces, that
     means that we're changing the parameters of a fixed face code;
     since the color/font/whatever is changed but the face ID hasn't,
     redisplay won't know to redraw the affected sections.  Give it a
     kick.  */
  if (id == 0 || id == 1)
    SET_FRAME_GARBAGED (f);
  else
    /* Otherwise, it's enough to tell it to redisplay the text.  */
    windows_or_buffers_changed = 1;

  return Qnil;
}

DEFUN ("internal-next-face-id", Finternal_next_face_id, Sinternal_next_face_id,
  0, 0, 0, "")
  ()
{
  return make_number (next_face_id++);
}

/* Return the face id for name NAME on frame FRAME.
   (It should be the same for all frames,
   but it's as easy to use the "right" frame to look it up
   as to use any other one.)  */

static Lisp_Object
face_name_id_number (frame, name)
     Lisp_Object frame, name;
{
  Lisp_Object tem;

  CHECK_FRAME (frame, 0);
  tem = Fcdr (Fassq (name, XFRAME (frame)->face_alist));
  if (NILP (tem))
    return 0;
  CHECK_VECTOR (tem, 0);
  tem = XVECTOR (tem)->contents[2];
  CHECK_NUMBER (tem, 0);
  return XINT (tem);
}

/* Emacs initialization.  */

void
syms_of_xfaces ()
{
  Qwindow = intern ("window");
  staticpro (&Qwindow);
  Qface = intern ("face");
  staticpro (&Qface);
  Qpriority = intern ("priority");
  staticpro (&Qpriority);

  DEFVAR_INT ("region-face", &region_face,
    "Face number to use to highlight the region\n\
The region is highlighted with this face\n\
when Transient Mark mode is enabled and the mark is active.");

  defsubr (&Sframe_face_alist);
  defsubr (&Sset_frame_face_alist);
  defsubr (&Smake_face_internal);
  defsubr (&Sset_face_attribute_internal);
  defsubr (&Sinternal_next_face_id);
}

#endif /* HAVE_X_WINDOWS */

