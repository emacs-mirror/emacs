/* "Face" primitives.
   Copyright (C) 1993 Free Software Foundation.

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

#include <config.h>
#include "lisp.h"

#ifdef HAVE_X_WINDOWS

#include "xterm.h"
#include "buffer.h"
#include "dispextern.h"
#include "frame.h"
#include "blockinput.h"
#include "window.h"

/* Compensate for bug in Xos.h on some systems, on which it requires
   time.h.  On some such systems, Xos.h tries to redefine struct
   timeval and struct timezone if USG is #defined while it is
   #included.  */
#ifdef XOS_NEEDS_TIME_H

#include <time.h>
#undef USG
#include <X11/Xos.h>
#define USG
#define __TIMEVAL__

#else

#include <X11/Xos.h>

#endif


/* An explanation of the face data structures.  */

/* ========================= Face Data Structures =========================

   Let FACE-NAME be a symbol naming a face.

   Let FACE-VECTOR be (assq FACE-NAME (frame-face-alist FRAME))
   FACE-VECTOR is either nil, or a vector of the form
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
   If any of these elements are nil, that parameter is considered
   unspecified; parameters from faces specified by lower-priority
   overlays or text properties, or the parameters of the frame itself,
   can show through.  (lisp/faces.el maintains these lists.)

   (assq FACE-NAME global-face-data) returns a vector describing the
   global parameters for that face.

   Let PARAM-FACE be FRAME->display.x->param_faces[Faref (FACE-VECTOR, 2)].
   PARAM_FACE is a struct face whose members are the Xlib analogues of
   the parameters in FACE-VECTOR.  If an element of FACE-VECTOR is
   nil, then the corresponding member of PARAM_FACE is FACE_DEFAULT.
   These faces are called "parameter faces", because they're the ones
   lisp manipulates to control what gets displayed.  Elements 0 and 1
   of FRAME->display.x->param_faces are special - they describe the
   default and mode line faces.  None of the faces in param_faces have
   GC's.  (See src/dispextern.h for the definiton of struct face.
   lisp/faces.el maintains the isomorphism between face_alist and
   param_faces.)

   The functions compute_char_face and compute_glyph_face find and
   combine the parameter faces associated with overlays and text
   properties.  The resulting faces are called "computed faces"; none
   of their members are FACE_DEFAULT; they are completely specified.
   They then call intern_compute_face to search
   FRAME->display.x->computed_faces for a matching face, add one if
   none is found, and return the index into
   FRAME->display.x->computed_faces.  FRAME's glyph matrices use these
   indices to record the faces of the matrix characters, and the X
   display hooks consult compute_faces to decide how to display these
   characters.  Elements 0 and 1 of computed_faces always describe the
   default and mode-line faces.

   Elements 0 and 1 of computed_faces have GC's; all the other faces
   in computed_faces do not.  The global array face_vector contains
   faces with their GC's set.  Given a computed_face, the function
   intern_face finds (or adds) an element of face_vector with
   equivalent parameters, and returns a pointer to that face, whose GC
   can then be used for display.

   Constraints:

   Symbols naming faces must have associations on all frames; for any
   FRAME, for all FACE-NAME, if (assq FACE-NAME (frame-face-alist
   FRAME)) is non-nil, it must be non-nil for all frames.

   Analogously, indices into param_faces must be valid on all frames;
   if param_faces[i] is a non-zero face pointer on one frame, then it
   must be filled in on all frames.  Code assumes that face ID's can
   be used on any frame.

   Some subtleties:
   
   Why do we keep param_faces and computed_faces separate?
   computed_faces contains an element for every combination of facial
   parameters we have ever displayed.  indices into param_faces have
   to be valid on all frames.  If they were the same array, then that
   array would grow very large on all frames, because any facial
   combination displayed on any frame would need to be a valid entry
   on all frames.

   Since face_vector is just a cache --- there are no pointers into it
   from the rest of the code, and everyone accesses it through
   intern_face --- we could just free its GC's and throw the whole
   thing away without breaking anything.  This gives us a simple way
   to garbage-collect old GC's nobody's using any more - we can just
   purge face_vector, and then let subsequent calls to intern_face
   refill it as needed.  The function clear_face_vector performs this
   purge.

   We're often applying intern_face to faces in computed_faces -
   for example, we do this while sending GLYPHs from a struct
   frame_glyphs to X during redisplay.  It would be nice to avoid
   searching all of face_vector every time we intern a frame's face.
   So, when intern_face finds a match for FACE in face_vector, it
   stores the index of the match in FACE's cached_index member, and
   checks there first next time.  */
   

/* Definitions and declarations.  */

/* A table of display faces.  */
static struct face **face_vector;
/* The length in use of the table.  */
static int nfaces;
/* The allocated length of the table.   */
static int nfaces_allocated;

/* The number of face-id's in use (same for all frames).  */
int next_face_id;

/* The number of the face to use to indicate the region.  */
int region_face;

/* This is what appears in a slot in a face to signify that the face
   does not specify that display aspect.  */
#define FACE_DEFAULT (~0)

Lisp_Object Qface, Qwindow, Qpriority;

static void build_face ( /* FRAME_PTR, struct face * */ );
int face_name_id_number ( /* FRAME_PTR, Lisp_Object name */ );

struct face *intern_face ( /* FRAME_PTR, struct face * */ );
static int new_computed_face ( /* FRAME_PTR, struct face * */ );
static int intern_computed_face ( /* FRAME_PTR, struct face * */ );
static void ensure_face_ready ( /* FRAME_PTR, int id */ );
void recompute_basic_faces ( /* FRAME_PTR f */ );

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

/* Given a computed face, return an equivalent display face
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

  BLOCK_INPUT;

  if (face->foreground != FACE_DEFAULT)
    xgcv.foreground = face->foreground;
  else
    xgcv.foreground = f->display.x->foreground_pixel;

  if (face->background != FACE_DEFAULT)
    xgcv.background = face->background;
  else
    xgcv.background = f->display.x->background_pixel;

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

  UNBLOCK_INPUT;
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

  BLOCK_INPUT;
  XFreeFont (x_current_display, font);
  UNBLOCK_INPUT;
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
     unsigned long pixel;
{
  /* Since faces get built by copying parameters from other faces, the
     allocation counts for the colors get all screwed up.  I don't see
     any solution that will take less than 10 minutes, and it's better
     to have a color leak than a crash, so I'm just dyking this out.
     This isn't really a color leak, anyway - if we ask for it again,
     we'll get the same pixel.  */
#if 0
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
#endif
}

/* Managing parameter face arrays for frames. */

void
init_frame_faces (f)
     FRAME_PTR f;
{
  ensure_face_ready (f, 0);
  ensure_face_ready (f, 1);

  new_computed_face (f, FRAME_PARAM_FACES (f)[0]);
  new_computed_face (f, FRAME_PARAM_FACES (f)[1]);
  recompute_basic_faces (f);

  /* Find another X frame.  */
  {
    Lisp_Object tail, frame, result;
    
    result = Qnil;
    FOR_EACH_FRAME (tail, frame)
      if (FRAME_X_P (XFRAME (frame))
	  && XFRAME (frame) != f)
	{
	  result = frame;
	  break;
	}

    /* If we didn't find any X frames other than f, then we don't need
       any faces other than 0 and 1, so we're okay.  Otherwise, make
       sure that all faces valid on the selected frame are also valid
       on this new frame.  */
    if (FRAMEP (result))
      {
	int i;
	int n_faces = FRAME_N_PARAM_FACES (XFRAME (result));
	struct face **faces = FRAME_PARAM_FACES (XFRAME (result));

	for (i = 2; i < n_faces; i++)
	  if (faces[i])
	    ensure_face_ready (f, i);
      }
  }
}


/* Called from Fdelete_frame.  */
void
free_frame_faces (f)
     struct frame *f;
{
  Display *dpy = x_current_display;
  int i;

  BLOCK_INPUT;

  for (i = 0; i < FRAME_N_PARAM_FACES (f); i++)
    {
      struct face *face = FRAME_PARAM_FACES (f) [i];
      if (face)
	{
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
    }
  xfree (FRAME_PARAM_FACES (f));
  FRAME_PARAM_FACES (f) = 0;
  FRAME_N_PARAM_FACES (f) = 0;

  /* All faces in FRAME_COMPUTED_FACES use resources copied from
     FRAME_PARAM_FACES; we can free them without fuss.  */
  xfree (FRAME_COMPUTED_FACES (f));
  FRAME_COMPUTED_FACES (f) = 0;
  FRAME_N_COMPUTED_FACES (f) = 0;

  UNBLOCK_INPUT;
}

/* Interning faces in a frame's face array.  */

static int
new_computed_face (f, new_face)
     struct frame *f;
     struct face *new_face;
{
  int i = FRAME_N_COMPUTED_FACES (f);

  if (i >= FRAME_SIZE_COMPUTED_FACES (f))
    {
      int new_size = i + 32;

      FRAME_COMPUTED_FACES (f)
	= (struct face **)
	  (FRAME_SIZE_COMPUTED_FACES (f) == 0
	   ? xmalloc (new_size * sizeof (struct face *))
	   : xrealloc (FRAME_COMPUTED_FACES (f),
		       new_size * sizeof (struct face *)));
      FRAME_SIZE_COMPUTED_FACES (f) = new_size;
    }

  i = FRAME_N_COMPUTED_FACES (f)++;
  FRAME_COMPUTED_FACES (f)[i] = copy_face (new_face);
  return i;
}


/* Find a match for NEW_FACE in a FRAME's computed face array, and add
   it if we don't find one.  */
static int
intern_computed_face (f, new_face)
     struct frame *f;
     struct face *new_face;
{
  int len = FRAME_N_COMPUTED_FACES (f);
  int i;

  /* Search for a computed face already on F equivalent to FACE.  */
  for (i = 0; i < len; i++)
    {
      if (! FRAME_COMPUTED_FACES (f)[i])
	abort ();
      if (face_eql (new_face, FRAME_COMPUTED_FACES (f)[i]))
	return i;
    }

  /* We didn't find one; add a new one.  */
  return new_computed_face (f, new_face);
}

/* Make parameter face id ID valid on frame F.  */

static void
ensure_face_ready (f, id)
     struct frame *f;
     int id;
{
  if (FRAME_N_PARAM_FACES (f) <= id)
    {
      int n = id + 10;
      int i;
      if (!FRAME_N_PARAM_FACES (f))
	FRAME_PARAM_FACES (f)
	  = (struct face **) xmalloc (sizeof (struct face *) * n);
      else
	FRAME_PARAM_FACES (f)
	  = (struct face **) xrealloc (FRAME_PARAM_FACES (f),
				       sizeof (struct face *) * n);

      bzero (FRAME_PARAM_FACES (f) + FRAME_N_PARAM_FACES (f),
	     (n - FRAME_N_PARAM_FACES (f)) * sizeof (struct face *));
      FRAME_N_PARAM_FACES (f) = n;
    }

  if (FRAME_PARAM_FACES (f) [id] == 0)
    FRAME_PARAM_FACES (f) [id] = allocate_face ();
}

/* Computing faces appropriate for a given piece of text in a buffer.  */

/* Return non-zero if FONT1 and FONT2 have the same size bounding box.
   We assume that they're both character-cell fonts.  */
int
same_size_fonts (font1, font2)
     XFontStruct *font1, *font2;
{
  XCharStruct *bounds1 = &font1->min_bounds;
  XCharStruct *bounds2 = &font2->min_bounds;

  return (bounds1->width == bounds2->width);
/* Checking the following caused bad results in some cases
   when fonts that should be the same size
   actually have very slightly different size.
   It is possible that this reintroduces the bug whereby line positions
   were not right.  However, the right way to fix that is to change xterm.c
   so that the vertical positions of lines
   depend only on the height of the frame's font.
	  && bounds1->ascent == bounds2->ascent
	  && bounds1->descent == bounds2->descent);  */
}

/* Modify face TO by copying from FROM all properties which have
   nondefault settings.  */
static void 
merge_faces (from, to)
     struct face *from, *to;
{
  /* Only merge the font if it's the same size as the base font.  */
  if (from->font != (XFontStruct *) FACE_DEFAULT
      && same_size_fonts (from->font, to->font))
    to->font = from->font;
  if (from->foreground != FACE_DEFAULT)
    to->foreground = from->foreground;
  if (from->background != FACE_DEFAULT)
    to->background = from->background;
  if (from->stipple != FACE_DEFAULT)
    to->stipple = from->stipple;
  if (from->underline)
    to->underline = from->underline;
}

/* Set up the basic set of facial parameters, based on the frame's
   data; all faces are deltas applied to this.  */
static void
compute_base_face (f, face)
     FRAME_PTR f;
     struct face *face;
{
  struct x_display *d = f->display.x;
  
  face->gc = 0;
  face->foreground = d->foreground_pixel;
  face->background = d->background_pixel;
  face->font = d->font;
  face->stipple = 0;
  face->underline = 0;

  /* Avoid a face comparison by making this invalid.  */
  face->cached_index = -1;
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

   REGION_BEG, REGION_END delimit the region, so it can be highlighted.

   LIMIT is a position not to scan beyond.  That is to limit
   the time this function can take.  */

int
compute_char_face (f, w, pos, region_beg, region_end, endptr, limit)
     struct frame *f;
     struct window *w;
     int pos;
     int region_beg, region_end;
     int *endptr;
     int limit;
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
    Lisp_Object limit1, end;

    XFASTINT (limit1) = (limit < endpos ? limit : endpos);
    end = Fnext_single_property_change (position, Qface, w->buffer, limit1);
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

  compute_base_face (f, &face);

  if (!NILP (prop))
    {
      facecode = face_name_id_number (f, prop);
      if (facecode >= 0 && facecode < FRAME_N_PARAM_FACES (f)
	  && FRAME_PARAM_FACES (f) [facecode] != 0)
	merge_faces (FRAME_PARAM_FACES (f) [facecode], &face);
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

	  facecode = face_name_id_number (f, prop);
	  if (facecode >= 0 && facecode < FRAME_N_PARAM_FACES (f)
	      && FRAME_PARAM_FACES (f) [facecode] != 0)
	    merge_faces (FRAME_PARAM_FACES (f) [facecode], &face);

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
	merge_faces (FRAME_PARAM_FACES (f) [region_face], &face);
    }

  *endptr = endpos;

  return intern_computed_face (f, &face);
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

  compute_base_face (f, &face);

  if (face_code >= 0 && face_code < FRAME_N_PARAM_FACES (f)
      && FRAME_PARAM_FACES (f) [face_code] != 0)
    merge_faces (FRAME_PARAM_FACES (f) [face_code], &face);

  return intern_computed_face (f, &face);
}


/* Recompute the GC's for the default and modeline faces.
   We call this after changing frame parameters on which those GC's
   depend.  */
void
recompute_basic_faces (f)
     FRAME_PTR f;
{
  /* If the frame's faces haven't been initialized yet, don't worry about
     this stuff.  */
  if (FRAME_N_PARAM_FACES (f) < 2)
    return;

  BLOCK_INPUT;

  if (FRAME_DEFAULT_FACE (f)->gc)
    XFreeGC (x_current_display, FRAME_DEFAULT_FACE (f)->gc);
  if (FRAME_MODE_LINE_FACE (f)->gc)
    XFreeGC (x_current_display, FRAME_MODE_LINE_FACE (f)->gc);

  compute_base_face (f, FRAME_DEFAULT_FACE (f));
  compute_base_face (f, FRAME_MODE_LINE_FACE (f));

  merge_faces (FRAME_DEFAULT_PARAM_FACE (f), FRAME_DEFAULT_FACE (f));
  merge_faces (FRAME_MODE_LINE_PARAM_FACE (f), FRAME_MODE_LINE_FACE (f));
  
  build_face (f, FRAME_DEFAULT_FACE (f));
  build_face (f, FRAME_MODE_LINE_FACE (f));

  UNBLOCK_INPUT;
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
  face = FRAME_PARAM_FACES (f) [XFASTINT (face_id)];

  if (EQ (attr_name, intern ("font")))
    {
      XFontStruct *font = load_font (f, attr_value);
      if (face->font != f->display.x->font)
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

  if (id == 0 || id == 1)
    recompute_basic_faces (f);

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

int
face_name_id_number (f, name)
     FRAME_PTR f;
     Lisp_Object name;
{
  Lisp_Object tem;

  tem = Fcdr (assq_no_quit (name, f->face_alist));
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

