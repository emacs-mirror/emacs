/* "Face" primitives
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

/* This derived from work by Lucid (some parts very loosely so).  */

#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"
#include "lisp.h"

#include "xterm.h"
#include "buffer.h"
#include "dispextern.h"
#include "frame.h"
/* #include "window.h" */

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xos.h>

/* We use face structures in two ways:
   At the frame level, each frame has a vector of faces (FRAME_FACES).
   Face number 0 is the default face (for normal text).
   Face number 1 is the mode line face.
   Higher face numbers have no built-in meaning.
   The faces in these vectors are called "frame faces".

   Faces number 0 and 1 have graphics contexts.
   They can be user in the redisplay code directly.
   Higher numbered frame faces do not have graphics contexts.

   There are also "cached faces".  They have graphics contexts.
   They are kept in a C vector called face_vector.

   A "display face" is a face with a graphics context.
   It is either a frame face number 0 or 1,
   or a cached face.  */
   
/* A table of display faces.  */
struct face **face_vector;
/* The length in use of the table.  */
int nfaces;
/* The allocated length of the table.   */
int nfaces_allocated;

/* The number of face-id's in use (same for all frames).  */
int next_face_id;

#define FACE_DEFAULT (~0)

#define xfree free

Lisp_Object Qface, Qwindow, Qpriority;

static struct face *allocate_face ();
static void build_face ();
static int sort_overlays ();
static struct face *get_display_face ();
static Lisp_Object face_name_id_number ();

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
  return (face1->font == face2->font
	  && face1->foreground == face2->foreground
	  && face1->background == face2->background
	  && face1->stipple == face2->stipple
	  && face1->underline == face2->underline);
}

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

/* Make a graphics context for face FACE, which is on frame F,
   if that can be done.  */

static void
build_face (f, face)
     struct frame* f;
     struct face* face;
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

/* Return the display face associated with a buffer position POS.
   Store into *ENDPTR the position at which a different face is needed.
   This does not take account of glyphs that specify their own face codes.
   F is the frame in use for display, and W is the window.  */

struct face *
compute_char_face (f, w, pos, endptr)
     struct frame *f;
     struct window *w;
     int pos;
     int *endptr;
{
  struct face face;
  Lisp_Object prop, position, length;
  Lisp_Object overlay, start, end;
  int i, j, noverlays;
  int facecode;
  int endpos;
  Lisp_Object *overlay_vec;
  int len;
  struct sortvec *sortvec;
  Lisp_Object frame;

  XSET (frame, Lisp_Frame, f);

  XFASTINT (position) = pos;
  prop = Fget_text_property (position, Qface);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));
  noverlays = overlays_at (pos, &overlay_vec, &len, &endpos);

  /* Optimize the default case.  */
  if (noverlays == 0 && NILP (prop))
    return FRAME_DEFAULT_FACE (f);

  bcopy (FRAME_DEFAULT_FACE (f), &face, sizeof (struct face));

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
      overlay = overlay_vec[i];

      if (OVERLAY_VALID (overlay)
	  && OVERLAY_POSITION (OVERLAY_START (overlay)) > 0
	  && OVERLAY_POSITION (OVERLAY_END (overlay)) > 0)
	{
	  Lisp_Object window;
	  window = Foverlay_get (overlay, Qwindow);

	  /* Also ignore overlays limited to one window
	     if it's not the window we are using.  */
	  if (NILP (window) || XWINDOW (window) == w)
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

  qsort (sortvec, noverlays, sizeof (struct sortvec), sort_overlays);

  /* Now merge the overlay data in that order.  */

  for (i = 0; i < noverlays; i++)
    {
      prop = Foverlay_get (overlay_vec[i], Qface);
      if (!NILP (prop))
	{
	  Lisp_Object oend;
	  int oendpos;

	  facecode = face_name_id_number (frame, prop);
	  if (facecode >= 0 && facecode < FRAME_N_FACES (f)
	      && FRAME_FACES (f) [facecode] != 0)
	    merge_faces (FRAME_FACES (f) [facecode], &face);

	  oend = OVERLAY_END (overlay_vec[i]);
	  oendpos = OVERLAY_POSITION (oend);
	  if (oendpos > endpos)
	    endpos = oendpos;
	}
    }

  xfree (overlay_vec);

  *endptr = endpos;

  return get_display_face (f, &face);
}

int
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

/* Return the display face to use to display a special glyph
   which selects FACE_CODE as the face ID,
   assuming that ordinarily the face would be BASIC_FACE.
   F is the frame.  */

struct face *
compute_glyph_face (f, basic_face, face_code)
     struct frame *f;
     struct face *basic_face;
     int face_code;
{
  struct face face;

  bcopy (basic_face, &face, sizeof (struct face));

  if (face_code >= 0 && face_code < FRAME_N_FACES (f)
      && FRAME_FACES (f) [face_code] != 0)
    merge_faces (FRAME_FACES (f) [face_code], &face);

  return get_display_face (f, &face);
}

/* Given a frame face, return an equivalent display face
   (one which has a graphics context).  */

static struct face *
get_display_face (f, face)
     struct frame *f;
     struct face *face;
{
  struct face *result;

  /* Does the face have a GC already?  */
  if (face->gc)
    return face;
  
  /* If it's equivalent to the default face, use that.  */
  if (face->font == FRAME_DEFAULT_FACE (f)->font
      && face->foreground == FRAME_DEFAULT_FACE (f)->foreground
      && face->background == FRAME_DEFAULT_FACE (f)->background
      && face->stipple == FRAME_DEFAULT_FACE (f)->stipple
      && face->underline == FRAME_DEFAULT_FACE (f)->underline)
    {
      if (!FRAME_DEFAULT_FACE (f)->gc)
	build_face (f, FRAME_DEFAULT_FACE (f));
      return FRAME_DEFAULT_FACE (f);
    }

  /* If it's equivalent to the mode line face, use that.  */
  if (face->font == FRAME_MODE_LINE_FACE (f)->font
      && face->foreground == FRAME_MODE_LINE_FACE (f)->foreground
      && face->background == FRAME_MODE_LINE_FACE (f)->background
      && face->stipple == FRAME_MODE_LINE_FACE (f)->stipple
      && face->underline == FRAME_MODE_LINE_FACE (f)->underline)
    {
      if (!FRAME_MODE_LINE_FACE (f)->gc)
	build_face (f, FRAME_MODE_LINE_FACE (f));
      return FRAME_MODE_LINE_FACE (f);
    }

  /* Get a specialized display face.  */
  return get_cached_face (f, face);
}


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

/* Make face id ID valid on frame F.  */

void
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

/* Allocating, freeing, and duplicating fonts, colors, and pixmaps.  */

#ifdef HAVE_X_WINDOWS

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
  if (pixel == FACE_DEFAULT)
    return;
  cmap = DefaultColormapOfScreen (DefaultScreenOfDisplay (x_current_display));
  BLOCK_INPUT;
  XFreeColors (dpy, cmap, &pixel, 1, 0);
  UNBLOCK_INPUT;
}

#endif /* HAVE_X_WINDOWS */


/* frames */

void
init_frame_faces (f)
     struct frame *f;
{
  struct frame *other_frame = 0;
  Lisp_Object rest;

  for (rest = Vframe_list; !NILP (rest); rest = Fcdr (rest))
    {
      struct frame *f2 = XFRAME (Fcar (rest));
      if (f2 != f && FRAME_X_P (f2))
	{
	  other_frame = f2;
	  break;
	}
    }

  if (other_frame)
    {
      /* Make sure this frame's face vector is as big as the others.  */
      FRAME_N_FACES (f) = FRAME_N_FACES (other_frame);
      FRAME_FACES (f)
	= (struct face **) xmalloc (FRAME_N_FACES (f) * sizeof (struct face *));

      /* Make sure the frame has the two basic faces.  */
      FRAME_DEFAULT_FACE (f)
	= copy_face (FRAME_DEFAULT_FACE (other_frame));
      FRAME_MODE_LINE_FACE (f)
	= copy_face (FRAME_MODE_LINE_FACE (other_frame));
    }
}


/* Called from Fdelete_frame?  */

void
free_screen_faces (f)
     struct frame *f;
{
  Display *dpy = x_current_display;
  int i;

  for (i = 0; i < FRAME_N_FACES (f); i++)
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


/* Lisp interface */

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
  CHECK_VECTOR (tem, 0);
  tem = XVECTOR (tem)->contents[2];
  CHECK_NUMBER (tem, 0);
  return XINT (tem);
}

void
syms_of_xfaces ()
{
  Qwindow = intern ("window");
  staticpro (&Qwindow);
  Qface = intern ("face");
  staticpro (&Qface);
  Qpriority = intern ("priority");
  staticpro (&Qpriority);

  defsubr (&Sframe_face_alist);
  defsubr (&Sset_frame_face_alist);
  defsubr (&Smake_face_internal);
  defsubr (&Sset_face_attribute_internal);
  defsubr (&Sinternal_next_face_id);
}
