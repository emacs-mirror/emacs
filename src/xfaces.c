/* "Face" primitives
This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"
#include "lisp.h"

#include "xterm.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "window.h"
#include "indent.h"

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xos.h>

/* A table of display faces.  */
struct face **face_vector;
/* The length in use of the table.  */
int nfaces;
/* The allocated length of the table.   */
int nfaces_allocated;

/* The number of face-id's in use (same for all frames).  */
int next_face_id;

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
get_vector_face (f, face)
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
