/* Must define frame->faces, frame->n_faces,
   FRAME_NORMAL_FACE, FRAME_MODELINE_FACE.  */

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

struct face
{
  unsigned char underline;
  unsigned char hilited;
  unsigned char modif;
  GC 		facegc;
  XFontStruct *	font;
  unsigned long	foreground;
  unsigned long	background;
  Pixmap	back_pixmap;
  unsigned int	pixmap_w, pixmap_h /* , pixmap_depth */;
};

#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"
#include "lisp.h"

#include "xterm.h"
#include "buffer.h"
#include "frame.h"
#include "window.h"
#include "indent.h"

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xos.h>

/* We use face structures in two ways:
   At the frame level, each frame has a vector of faces.  (f->faces).
   Face number 0 is the normal face (for normal text).
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

static struct face *allocate_face ();
static void build_face ();

/* Make a new face that's a copy of an existing one.  */

static struct face *
copy_face (face)
     struct face *face;
{
  struct face *result = allocate_face ();

  result->font = face->font;
  result->foreground = face->foreground;
  result->background = face->background;
  result->back_pixmap = face->back_pixmap;
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
	  && face1->back_pixmap == face2->back_pixmap
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

  BLOCK_INPUT;
  /* Free the display faces in the face_vector.  */
  for (i = 0; i < nfaces; i++)
    {
      struct face *face = face_vector[i];
      if (face->facegc)
	XFreeGC (dpy, face->facegc);
      xfree (face);
    }
  nfaces = 0;

  UNBLOCK_INPUT;
}

/* Make a graphics context for face FACE, which is on frame F.  */

static void
build_face (f, face)
     struct frame* f;
     struct face* face;
{
  GC gc;
  XGCValues xgcv;
  unsigned long mask;

  xgcv.foreground = face->foreground;
  xgcv.background = face->background;
  xgcv.font = face->font->fid;
  xgcv.graphics_exposures = 0;
  mask = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
  gc = XCreateGC (x_current_display, FRAME_X_WINDOW (f),
		  mask, &xgcv);
#if 0
  if (face->back_pixmap && face->back_pixmap != FACE_DEFAULT)
    XSetStipple (XtDisplay (f->display.x->widget), gc, face->back_pixmap);
#endif
  face->facegc = gc;
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
  if (from->back_pixmap != FACE_DEFAULT)
    to->back_pixmap = from->back_pixmap;
  if (from->underline)
    to->underline = from->underline;
}

/* Return the display face associated with a buffer position POS.
   Store into *ENDPTR the position at which a different face is needed.
   This does not take account of glyphs that specify their own face codes.
   F is the frame in use for display.  */

struct face *
compute_char_face (f, pos, endptr)
     struct frame *f;
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

  XFASTINT (position) = pos;
  prop = Fget_text_property (position, Qface);

  len = 10;
  overlay_vec = (Lisp_Object *) xmalloc (len * sizeof (Lisp_Object));
  noverlays = overlays_at (pos, &overlay_vec, &len, &endpos);

  /* Optimize the default case.  */
  if (noverlays == 0 && NILP (prop))
    return FRAME_NORMAL_FACE (f);

  bcopy (FRAME_NORMAL_FACE (f), &face, sizeof (struct face));

  if (!NILP (prop))
    {
      facecode = Fface_name_id_number (prop);
      if (facecode >= 0 && facecode < f->n_faces && f->faces[facecode] != 0)
	merge_faces (f->faces[facecode], &face);
    }

  /* Discard invalid overlays from the vector.  */
  for (i = 0, j = 0; i < noverlays; i++)
    {
      overlay = overlay_vec[i];

      if (OVERLAY_VALID (overlay)
	  && OVERLAY_POSITION (OVERLAY_START (overlay)) > 0
	  && OVERLAY_POSITION (OVERLAY_END (overlay)) > 0)
	overlay_vec[j++] = overlay;
    }
  noverlays = j;

  /* Sort the overlays into the proper order.  */

  /* Now merge the overlay data in that order.  */

  for (i = 0; i < noverlays; i++)
    {
      prop = Foverlay_get (overlay_vec[i], Qface);
      if (!NILP (prop))
	{
	  Lisp_Object oend;
	  int oendpos;

	  facecode = Fface_name_id_number (prop);
	  if (facecode >= 0 && facecode < f->n_faces
	      && f->faces[facecode] != 0)
	    merge_faces (f->faces[facecode], &face);

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

  if (face_code >= 0 && face_code < f->n_faces && f->faces[face_code] != 0)
    merge_faces (f->faces[face_code], &face);

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
  if (face->facegc)
    return face;
  
  /* If it's equivalent to the normal face, use that.  */
  if (face->font == FRAME_NORMAL_FACE (f)->font
      && face->foreground == FRAME_NORMAL_FACE (f)->foreground
      && face->background == FRAME_NORMAL_FACE (f)->background
      && face->back_pixmap == FRAME_NORMAL_FACE (f)->back_pixmap
      && face->underline == FRAME_NORMAL_FACE (f)->underline)
    {
      if (!FRAME_NORMAL_FACE (f)->framegc)
	build_frame (f, FRAME_NORMAL_FACE (f));
      return FRAME_NORMAL_FACE (f);
    }

  /* If it's equivalent to the mode line face, use that.  */
  if (face->font == FRAME_MODELINE_FACE (f)->font
      && face->foreground == FRAME_MODELINE_FACE (f)->foreground
      && face->background == FRAME_MODELINE_FACE (f)->background
      && face->back_pixmap == FRAME_MODELINE_FACE (f)->back_pixmap
      && face->underline == FRAME_MODELINE_FACE (f)->underline)
    {
      if (!FRAME_MODELINE_FACE (f)->framegc)
	build_frame (f, FRAME_MODELINE_FACE (f));
      return FRAME_MODELINE_FACE (f);
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
  result->back_pixmap = FACE_DEFAULT;
  return result;
}

/* Make face id ID valid on frame F.  */

void
ensure_face_ready (f, id)
     struct frame *f;
     int id;
{
  if (f->n_faces <= id)
    {
      int n = id + 10;
      int i;
      if (!f->n_faces)
	f->faces = (struct face **) xmalloc (sizeof (struct face *) * n);
      else
	f->faces
	  = (struct face **) xrealloc (f->faces, sizeof (struct face *) * n);

      f->n_faces = n;
    }

  f->faces[id] = allocate_face ();
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

  cmap = DefaultColormapOfScreen (x_screen);

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
  cmap = DefaultColormapOfScreen (x_screen);
  BLOCK_INPUT;
  XFreeColors (dpy, cmap, &pixel, 1, 0);
  UNBLOCK_INPUT;
}

#endif /* HAVE_X_WINDOWS */


/* frames */

void
init_frame_faces (f)
     struct frame f;
{
  struct frame *other_frame = 0;
  Lisp_Object rest;

  for (rest = Vframe_list; !NILP (rest); rest = Fcdr (rest))
    {
      struct frame *f2 = XFRAME (Fcar (rest));
      if (f2 != f && FRAME_IS_X (f2))
	{
	  other_frame = f2;
	  break;
	}
    }

  if (other_frame)
    {
      /* Make sure this frame's face vector is as big as the others.  */
      f->n_faces = other_frame->n_faces;
      f->faces = (struct face **) xmalloc (f->n_faces * sizeof (struct face *));

      /* Make sure the frame has the two basic faces.  */
      FRAME_NORMAL_FACE (f)
	= copy_face (FRAME_NORMAL_FACE (other_frame));
      FRAME_MODELINE_FACE (f)
	= copy_face (FRAME_MODELINE_FACE (other_frame));
    }
}


/* Called from Fdelete_frame?  */

void
free_screen_faces (f)
     struct frame *f;
{
  Display *dpy = x_current_display;
  int i;

  for (i = 0; i < f->n_faces; i++)
    {
      struct face *face = f->faces [i];
      if (! face)
        continue;
      if (face->facegc)
	XFreeGC (dpy, face->facegc);
      unload_font (f, face->font);
      unload_color (f, face->foreground);
      unload_color (f, face->background);
      unload_pixmap (f, face->back_pixmap);
      xfree (face);
    }
  xfree (f->faces);
  f->faces = 0;
  f->n_faces = 0;
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

  CHECK_FIXNUM (face_id, 0);
  if (id < 0)
    error ("Face id must be nonnegative");

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
  CHECK_FIXNUM (face_id, 0);
  CHECK_SYMBOL (attr_name, 0);

  f = XFRAME (frame);
  id = XINT (face_id);
  if (id < 0)
    Fsignal (Qerror, Fcons (build_string ("invalid face id"),
			    Fcons (face_id, Fcons (frame, Qnil))));

  ensure_face_ready (f, id);
  face = f->faces [XFASTINT (face_id)];
  if (! face) abort ();

  magic_p = (NILP (attr_value) && XFASTINT (face_id) <= 1);

  if (EQ (attr_name, intern ("font")))
    {
#ifdef HAVE_X_WINDOWS
      XFontStruct *font;
      if (magic_p)
	error ("font of the `normal' or `modeline' face may not be nil");
      font = load_font (f, attr_value);
      unload_font (f, face->font);
      face->font = font;
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("foreground")))
    {
#ifdef HAVE_X_WINDOWS
      unsigned long new_color;
      if (magic_p)
	error ("forground color of the `normal' or `modeline' face may not be nil");
      new_color = load_color (f, attr_value);
      unload_color (f, face->foreground);
      face->foreground = new_color;
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, intern ("background")))
    {
#ifdef HAVE_X_WINDOWS
      unsigned long new_color;
      if (magic_p)
	error ("background color of the `normal' or `modeline' face may not be nil");
      new_color = load_color (f, attr_value);
      unload_color (f, face->background);
      face->background = new_color;
#endif /* HAVE_X_WINDOWS */
    }
#if 0
  else if (EQ (attr_name, intern ("background-pixmap")))
    {
#ifdef HAVE_X_WINDOWS
      unsigned int w, h, d;
      unsigned long new_pixmap = load_pixmap (f, attr_value, &w, &h, &d, 0);
      unload_pixmap (f, face->back_pixmap);
      if (magic_p) new_pixmap = 0;
      face->back_pixmap = new_pixmap;
      face->pixmap_w = w;
      face->pixmap_h = h;
/*      face->pixmap_depth = d; */
#endif /* HAVE_X_WINDOWS */
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
      XFreeGC (dpy, FRAME_NORMAL_FACE (f)->facegc);
      build_face (f, FRAME_NORMAL_FACE (f));
      UNBLOCK_INPUT;
    }

  if (id == 1)
    {
      BLOCK_INPUT;
      XFreeGC (dpy, FRAME_NORMAL_FACE (f)->facegc);
      build_face (f, FRAME_NORMAL_FACE (f));
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

void
syms_of_faces ()
{
  defsubr (&Sframe_face_alist);
  defsubr (&Sset_frame_face_alist);
  defsubr (&Smake_face_internal);
  defsubr (&Sset_face_attribute_internal);
  defsubr (&Sinternal_next_face_id);
}
