/* "Face" primitives on the Microsoft W32 API.
   Copyright (C) 1993, 1994, 1995, 1999 Free Software Foundation.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Ported xfaces.c for w32 - Kevin Gallo */

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "lisp.h"

#include "w32term.h"
#include "buffer.h"
#include "dispextern.h"
#include "frame.h"
#include "blockinput.h"
#include "window.h"
#include "intervals.h"
#include "charset.h"
#include "fontset.h"


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
           to use with the face, FONT may name fontsets,
       BACKGROUND-PIXMAP is the name of an x bitmap filename, which we don't
           use right now, and
       UNDERLINE-P is non-nil if the face should be underlined.
   If any of these elements are nil, that parameter is considered
   unspecified; parameters from faces specified by lower-priority
   overlays or text properties, or the parameters of the frame itself,
   can show through.  (lisp/faces.el maintains these lists.)

   (assq FACE-NAME global-face-data) returns a vector describing the
   global parameters for that face.

   Let PARAM-FACE be FRAME->output_data.w32->param_faces[Faref(FACE-VECTOR,2)].
   PARAM_FACE is a struct face whose members are the Xlib analogues of
   the parameters in FACE-VECTOR.  If an element of FACE-VECTOR is
   nil, then the corresponding member of PARAM_FACE is FACE_DEFAULT.
   These faces are called "parameter faces", because they're the ones
   lisp manipulates to control what gets displayed.  Elements 0 and 1
   of FRAME->output_data.w32->param_faces are special - they describe the
   default and mode line faces.  None of the faces in param_faces have
   GC's.  (See src/dispextern.h for the definition of struct face.
   lisp/faces.el maintains the isomorphism between face_alist and
   param_faces.)

   The functions compute_char_face and compute_glyph_face find and
   combine the parameter faces associated with overlays and text
   properties.  The resulting faces are called "computed faces"; none
   of their members are FACE_DEFAULT; they are completely specified.
   They then call intern_compute_face to search
   FRAME->output_data.x->computed_faces for a matching face, add one if
   none is found, and return the index into
   FRAME->output_data.x->computed_faces.  FRAME's glyph matrices use these
   indices to record the faces of the matrix characters, and the X
   display hooks consult compute_faces to decide how to display these
   characters.  Elements 0 and 1 of computed_faces always describe the
   default and mode-line faces.

   Each computed face belongs to a particular frame.

   Computed faces have graphics contexts some of the time.
   intern_face builds a GC for a specified computed face
   if it doesn't have one already.
   clear_face_cache clears out the GCs of all computed faces.
   This is done from time to time so that we don't hold on to
   lots of GCs that are no longer needed.

   If a computed face has 0 as its font,
   it is unused, and can be reused by new_computed_face.

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
   on all frames.  */

/* Definitions and declarations.  */

/* The number of face-id's in use (same for all frames).  */
static int next_face_id;

/* The number of the face to use to indicate the region.  */
static int region_face;

/* This is what appears in a slot in a face to signify that the face
   does not specify that display aspect.  */
#define FACE_DEFAULT (~0)

Lisp_Object Qface;
Lisp_Object Qpixmap_spec_p;

extern Lisp_Object Qmouse_face;  /* In textprop.c.  */

int face_name_id_number ( /* FRAME_PTR, Lisp_Object name */ );

struct face *intern_face ( /* FRAME_PTR, struct face * */ );
static int new_computed_face ( /* FRAME_PTR, struct face * */ );
static int intern_computed_face ( /* FRAME_PTR, struct face * */ );
static void ensure_face_ready ( /* FRAME_PTR, int id */ );
void recompute_basic_faces ( /* FRAME_PTR f */ );
static void merge_face_list ( /* FRAME_PTR, struct face *, Lisp_Object */ );

extern Lisp_Object Qforeground_color, Qbackground_color;

/* Allocating, copying, and comparing struct faces.  */

/* Allocate a new face */
static struct face *
allocate_face ()
{
  struct face *result = (struct face *) xmalloc (sizeof (struct face));
  bzero (result, sizeof (struct face));
  result->font = (XFontStruct *) FACE_DEFAULT;
  result->fontset = -1;
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
  result->fontset = face->fontset;
  result->foreground = face->foreground;
  result->background = face->background;
  result->stipple = face->stipple;
  result->underline = face->underline;
  result->pixmap_h = face->pixmap_h;
  result->pixmap_w = face->pixmap_w;

  return result;
}

static int
face_eql (face1, face2)
     struct face *face1, *face2;
{
  return (   face1->font       == face2->font
          && face1->fontset    == face2->fontset
	  && face1->foreground == face2->foreground
	  && face1->background == face2->background
	  && face1->stipple    == face2->stipple
	  && face1->underline  == face2->underline);
}

/* Managing graphics contexts of faces.  */

/* Given a computed face, construct its graphics context if necessary.  */

struct face *
intern_face (f, face)
     struct frame *f;
     struct face *face;
{
  face->gc = NULL;

  return face;
}

/* Clear out all graphics contexts for all computed faces
   except for the default and mode line faces.
   This should be done from time to time just to avoid
   keeping too many graphics contexts that are no longer needed.  */

void
clear_face_cache ()
{
/* Nothing extra */
}

/* Allocating, freeing, and duplicating fonts, colors, and pixmaps.

   These functions operate on param faces only.
   Computed faces get their fonts, colors and pixmaps
   by merging param faces.  */

static XFontStruct *
load_font (f, name)
     struct frame *f;
     Lisp_Object name;
{
  struct font_info *fontinf;
  XFontStruct *font = NULL;

  if (NILP (name))
    return (XFontStruct *) FACE_DEFAULT;

  CHECK_STRING (name, 0);
  BLOCK_INPUT;
  fontinf = w32_load_font (f, (char *) XSTRING (name)->data, 0);
  UNBLOCK_INPUT;
  if (fontinf)
    font = (XFontStruct *)fontinf->font;

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
  w32_unload_font (FRAME_W32_DISPLAY_INFO (f), font);
  UNBLOCK_INPUT;
}

static unsigned long
load_color (f, name)
     struct frame *f;
     Lisp_Object name;
{
  COLORREF color;
  int result;

  if (NILP (name))
    return FACE_DEFAULT;

  CHECK_STRING (name, 0);
  /* if the colormap is full, defined_color will return a best match
     to the values in an an existing cell. */
  result = defined_color(f, (char *) XSTRING (name)->data, &color, 1);
  if (! result)
    Fsignal (Qerror, Fcons (build_string ("undefined color"),
			    Fcons (name, Qnil)));
  return (unsigned long) color;
}

static void
unload_color (f, pixel)
     struct frame *f;
     unsigned long pixel;
{
}

DEFUN ("pixmap-spec-p", Fpixmap_spec_p, Spixmap_spec_p, 1, 1, 0,
  "Return t if OBJECT is a valid pixmap specification.")
  (object)
     Lisp_Object object;
{
  Lisp_Object height, width;

  return ((STRINGP (object)
	   || (CONSP (object)
	       && CONSP (XCDR (object))
	       && CONSP (XCDR (XCDR (object)))
	       && NILP (XCDR (XCDR (XCDR (object))))
	       && (width = XCAR (object), INTEGERP (width))
	       && (height = XCAR (XCDR (object)), INTEGERP (height))
	       && STRINGP (XCAR (XCDR (XCDR (object))))
	       && XINT (width) > 0
	       && XINT (height) > 0
	       /* The string must have enough bits for width * height.  */
	       && ((XSTRING (XCAR (XCDR (XCDR (object))))->size
		    * (BITS_PER_INT / sizeof (int)))
		   >= XFASTINT (width) * XFASTINT (height))))
	  ? Qt : Qnil);
}

/* Load a bitmap according to NAME (which is either a file name
   or a pixmap spec).  Return the bitmap_id (see xfns.c)
   or get an error if NAME is invalid.

   Store the bitmap width in *W_PTR and height in *H_PTR.  */

static long
load_pixmap (f, name, w_ptr, h_ptr)
     FRAME_PTR f;
     Lisp_Object name;
     unsigned int *w_ptr, *h_ptr;
{
  int bitmap_id;
  Lisp_Object tem;

  if (NILP (name))
    return FACE_DEFAULT;

  tem = Fpixmap_spec_p (name);
  if (NILP (tem))
    wrong_type_argument (Qpixmap_spec_p, name);

  BLOCK_INPUT;

  if (CONSP (name))
    {
      /* Decode a bitmap spec into a bitmap.  */

      int h, w;
      Lisp_Object bits;

      w = XINT (Fcar (name));
      h = XINT (Fcar (Fcdr (name)));
      bits = Fcar (Fcdr (Fcdr (name)));

      bitmap_id = x_create_bitmap_from_data (f, XSTRING (bits)->data,
					     w, h);
    }
  else
    {
      /* It must be a string -- a file name.  */
      bitmap_id = x_create_bitmap_from_file (f, name);
    }
  UNBLOCK_INPUT;

  if (bitmap_id < 0)
    Fsignal (Qerror, Fcons (build_string ("invalid or undefined bitmap"),
			    Fcons (name, Qnil)));

  *w_ptr = x_bitmap_width (f, bitmap_id);
  *h_ptr = x_bitmap_height (f, bitmap_id);

  return bitmap_id;
}


/* Managing parameter face arrays for frames. */

void
init_frame_faces (f)
     FRAME_PTR f;
{
  ensure_face_ready (f, 0);
  ensure_face_ready (f, 1);

  FRAME_N_COMPUTED_FACES (f) = 0;
  FRAME_SIZE_COMPUTED_FACES (f) = 0;

  new_computed_face (f, FRAME_PARAM_FACES (f)[0]);
  new_computed_face (f, FRAME_PARAM_FACES (f)[1]);
  recompute_basic_faces (f);

  /* Find another frame.  */
  {
    Lisp_Object tail, frame, result;
    
    result = Qnil;
    FOR_EACH_FRAME (tail, frame)
      if (FRAME_W32_P (XFRAME (frame))
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
  int i;

  BLOCK_INPUT;

  for (i = 0; i < FRAME_N_PARAM_FACES (f); i++)
    {
      struct face *face = FRAME_PARAM_FACES (f) [i];
      if (face)
	{
          if (face->fontset < 0)
	  unload_font (f, face->font);
	  unload_color (f, face->foreground);
	  unload_color (f, face->background);
	  x_destroy_bitmap (f, face->stipple);
	  xfree (face);
	}
    }
  xfree (FRAME_PARAM_FACES (f));
  FRAME_PARAM_FACES (f) = 0;
  FRAME_N_PARAM_FACES (f) = 0;

  /* All faces in FRAME_COMPUTED_FACES use resources copied from
     FRAME_PARAM_FACES; we can free them without fuss.
     But we do free the GCs and the face objects themselves.  */
  for (i = 0; i < FRAME_N_COMPUTED_FACES (f); i++)
    {
      struct face *face = FRAME_COMPUTED_FACES (f) [i];
      if (face)
	{
	  xfree (face);
	}
    }
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
  int len = FRAME_N_COMPUTED_FACES (f);
  int i;

  /* Search for an unused computed face in the middle of the table.  */
  for (i = 0; i < len; i++)
    {
      struct face *face = FRAME_COMPUTED_FACES (f)[i];
      if (face->font == 0)
	{
	  FRAME_COMPUTED_FACES (f)[i] = copy_face (new_face);
	  return i;
	}
    }

  if (i >= FRAME_SIZE_COMPUTED_FACES (f))
    {
      int new_size = i + 32;

      FRAME_COMPUTED_FACES (f)
	= (struct face **) (FRAME_SIZE_COMPUTED_FACES (f) == 0
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

/* Return non-zero if FONT1 and FONT2 have the same width.
   We do not check the height, because we can now deal with
   different heights.
   We assume that they're both character-cell fonts.  */

int
same_size_fonts (font1, font2)
     XFontStruct *font1, *font2;
{
  return (FONT_WIDTH(font1) == FONT_WIDTH(font2));
}

/* Update the line_height of frame F according to the biggest font in
   any face.  Return nonzero if if line_height changes.  */

int
frame_update_line_height (f)
     FRAME_PTR f;
{
  int i;
  int fontset = FRAME_FONTSET (f);
  int biggest = (fontset > 0
                 ? FRAME_FONTSET_DATA (f)->fontset_table[fontset]->height
                 : FONT_HEIGHT (FRAME_FONT (f)));

  for (i = 0; i < FRAME_N_PARAM_FACES (f); i++)
    if (FRAME_PARAM_FACES (f)[i] != 0
	&& FRAME_PARAM_FACES (f)[i]->font != (XFontStruct *) FACE_DEFAULT)
      {
	int height = ((fontset =
                       FRAME_PARAM_FACES (f)[i]->fontset) > 0
                      ? FRAME_FONTSET_DATA (f)->fontset_table[fontset]->height
                      : FONT_HEIGHT
                          (FRAME_PARAM_FACES (f)[i]->font));
	if (height > biggest)
	  biggest = height;
      }

  if (biggest == FRAME_LINE_HEIGHT (f))
    return 0;

  FRAME_LINE_HEIGHT (f) = biggest;
  return 1;
}

/* Modify face TO by copying from FROM all properties which have
   nondefault settings.  */

static void 
merge_faces (from, to)
     struct face *from, *to;
{
  /* Only merge the font if it's the same width as the base font.
     Otherwise ignore it, since we can't handle it properly.  */
  if (from->font != (XFontStruct *) FACE_DEFAULT
      && same_size_fonts (from->font, to->font))
    to->font = from->font;
  if (from->fontset != -1)
    to->fontset = from->fontset;
  if (from->foreground != FACE_DEFAULT)
    to->foreground = from->foreground;
  if (from->background != FACE_DEFAULT)
    to->background = from->background;
  if (from->stipple != FACE_DEFAULT)
    {
      to->stipple = from->stipple;
      to->pixmap_h = from->pixmap_h;
      to->pixmap_w = from->pixmap_w;
    }
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
  face->gc = 0;
  face->foreground = FRAME_FOREGROUND_PIXEL (f);
  face->background = FRAME_BACKGROUND_PIXEL (f);
  face->font = FRAME_FONT (f);
  face->fontset = -1;
  face->stipple = 0;
  face->underline = 0;
}

/* Return the face ID to use to display a special glyph which selects
   FACE_CODE as the face ID, assuming that ordinarily the face would
   be CURRENT_FACE.  F is the frame.  */

int
compute_glyph_face (f, face_code, current_face)
     struct frame *f;
     int face_code, current_face;
{
  struct face face;

  face = *FRAME_COMPUTED_FACES (f)[current_face];

  if (face_code >= 0 && face_code < FRAME_N_PARAM_FACES (f)
      && FRAME_PARAM_FACES (f) [face_code] != 0)
    merge_faces (FRAME_PARAM_FACES (f) [face_code], &face);

  return intern_computed_face (f, &face);
}

/* Return the face ID to use to display a special glyph which selects
   FACE_CODE as the face ID, assuming that ordinarily the face would
   be CURRENT_FACE.  F is the frame.  */

int
compute_glyph_face_1 (f, face_name, current_face)
     struct frame *f;
     Lisp_Object face_name;
     int current_face;
{
  struct face face;

  face = *FRAME_COMPUTED_FACES (f)[current_face];

  if (!NILP (face_name))
    {
      int facecode = face_name_id_number (f, face_name);
      if (facecode >= 0 && facecode < FRAME_N_PARAM_FACES (f)
	  && FRAME_PARAM_FACES (f) [facecode] != 0)
	merge_faces (FRAME_PARAM_FACES (f) [facecode], &face);
    }

  return intern_computed_face (f, &face);
}

/* Return the face ID associated with a buffer position POS.
   Store into *ENDPTR the position at which a different face is needed.
   This does not take account of glyphs that specify their own face codes.
   F is the frame in use for display, and W is a window displaying
   the current buffer.

   REGION_BEG, REGION_END delimit the region, so it can be highlighted.

   LIMIT is a position not to scan beyond.  That is to limit
   the time this function can take.

   If MOUSE is nonzero, use the character's mouse-face, not its face.  */
int
compute_char_face (f, w, pos, region_beg, region_end, endptr, limit, mouse)
     struct frame *f;
     struct window *w;
     int pos;
     int region_beg, region_end;
     int *endptr;
     int limit;
     int mouse;
{
  struct face face;
  Lisp_Object prop, position;
  int i, j, noverlays;
  int facecode;
  Lisp_Object *overlay_vec;
  Lisp_Object frame;
  int endpos;
  Lisp_Object propname;

  /* W must display the current buffer.  We could write this function
     to use the frame and buffer of W, but right now it doesn't.  */
  if (XBUFFER (w->buffer) != current_buffer)
    abort ();

  XSETFRAME (frame, f);

  endpos = ZV;
  if (pos < region_beg && region_beg < endpos)
    endpos = region_beg;

  XSETFASTINT (position, pos);

  if (mouse)
    propname = Qmouse_face;
  else
    propname = Qface;

  prop = Fget_text_property (position, propname, w->buffer);

  {
    Lisp_Object limit1, end;

    XSETFASTINT (limit1, (limit < endpos ? limit : endpos));
    end = Fnext_single_property_change (position, propname, w->buffer, limit1);
    if (INTEGERP (end))
      endpos = XINT (end);
  }

  {
    int next_overlay;
    int len;

    /* First try with room for 40 overlays.  */
    len = 40;
    overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
    
    noverlays = overlays_at (pos, 0, &overlay_vec, &len,
			     &next_overlay, (int *) 0);

    /* If there are more than 40,
       make enough space for all, and try again.  */
    if (noverlays > len)
      {
	len = noverlays;
	overlay_vec = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
	noverlays = overlays_at (pos, 0, &overlay_vec, &len,
				 &next_overlay, (int *) 0);
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

  merge_face_list (f, &face, prop);

  noverlays = sort_overlays (overlay_vec, noverlays, w);

  /* Now merge the overlay data in that order.  */
  for (i = 0; i < noverlays; i++)
    {
      Lisp_Object oend;
      int oendpos;

      prop = Foverlay_get (overlay_vec[i], propname);
      merge_face_list (f, &face, prop);

      oend = OVERLAY_END (overlay_vec[i]);
      oendpos = OVERLAY_POSITION (oend);
      if (oendpos < endpos)
	endpos = oendpos;
    }

  if (pos >= region_beg && pos < region_end)
    {
      if (region_end < endpos)
	endpos = region_end;
      if (region_face >= 0 && region_face < next_face_id)
	merge_faces (FRAME_PARAM_FACES (f)[region_face], &face);
    }

  *endptr = endpos;

  return intern_computed_face (f, &face);
}

static void
merge_face_list (f, face, prop)
     FRAME_PTR f;
     struct face *face;
     Lisp_Object prop;
{
  Lisp_Object length;
  int len;
  Lisp_Object *faces;
  int j;

  if (CONSP (prop)
      && ! STRINGP (XCDR (prop)))
    {
      /* We have a list of faces, merge them in reverse order.  */

      length = Fsafe_length (prop);
      len = XFASTINT (length);

      /* Put them into an array.  */
      faces = (Lisp_Object *) alloca (len * sizeof (Lisp_Object));
      for (j = 0; j < len; j++)
	{
	  faces[j] = Fcar (prop);
	  prop = Fcdr (prop);
	}
      /* So that we can merge them in the reverse order.  */
    }
  else
    {
      faces = (Lisp_Object *) alloca (sizeof (Lisp_Object));
      faces[0] = prop;
      len = 1;
    }

  for (j = len - 1; j >= 0; j--)
    {
      if (CONSP (faces[j]))
	{
	  if (EQ (XCAR (faces[j]), Qbackground_color))
	    face->background = load_color (f, XCDR (faces[j]));
	  if (EQ (XCAR (faces[j]), Qforeground_color))
	    face->foreground = load_color (f, XCDR (faces[j]));
	}
      else
	{
	  int facecode = face_name_id_number (f, faces[j]);
	  if (facecode >= 0 && facecode < FRAME_N_PARAM_FACES (f)
	      && FRAME_PARAM_FACES (f) [facecode] != 0)
	    merge_faces (FRAME_PARAM_FACES (f) [facecode], face);
	}
    }
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

  compute_base_face (f, FRAME_DEFAULT_FACE (f));
  compute_base_face (f, FRAME_MODE_LINE_FACE (f));

  merge_faces (FRAME_DEFAULT_PARAM_FACE (f), FRAME_DEFAULT_FACE (f));
  merge_faces (FRAME_MODE_LINE_PARAM_FACE (f), FRAME_MODE_LINE_FACE (f));
  
  intern_face (f, FRAME_DEFAULT_FACE (f));
  intern_face (f, FRAME_MODE_LINE_FACE (f));

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
  Lisp_Object rest, frame;
  int id = XINT (face_id);

  CHECK_NUMBER (face_id, 0);
  if (id < 0 || id >= next_face_id)
    error ("Face id out of range");

  FOR_EACH_FRAME (rest, frame)
    {
      if (FRAME_W32_P (XFRAME (frame)))
	ensure_face_ready (XFRAME (frame), id);
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
  int garbaged = 0;

  CHECK_FRAME (frame, 0);
  CHECK_NUMBER (face_id, 0);
  CHECK_SYMBOL (attr_name, 0);

  f = XFRAME (frame);
  id = XINT (face_id);
  if (id < 0 || id >= next_face_id)
    error ("Face id out of range");

  if (! FRAME_WINDOW_P (f))
    return Qnil;

  ensure_face_ready (f, id);
  face = FRAME_PARAM_FACES (f) [XFASTINT (face_id)];

  if (EQ (attr_name, intern ("font")))
    {
      XFontStruct *font = NULL;
      int fontset;

      if (NILP (attr_value))
        {
          font = (XFontStruct *) FACE_DEFAULT;
          fontset = -1;
        }
      else
        {
          CHECK_STRING (attr_value, 0);
          fontset = fs_query_fontset (f, XSTRING (attr_value)->data);
          if (fontset >= 0)
            {
              struct font_info *fontp;

              if (!(fontp = FS_LOAD_FONT (f, FRAME_W32_FONT_TABLE (f),
                                          CHARSET_ASCII, NULL, fontset)))
                Fsignal (Qerror,
                         Fcons (build_string ("ASCII font can't be loaded"),
                                Fcons (attr_value, Qnil)));
              font = (XFontStruct *) (fontp->font);
            }
          else
            font = load_font (f, attr_value);
        }
      if (face->fontset == -1 && face->font != f->output_data.w32->font)
	unload_font (f, face->font);
      face->font = font;
      face->fontset = fontset;
      if (frame_update_line_height (f))
	x_set_window_size (f, 0, f->width, f->height);
      /* Must clear cache, since it might contain the font
	 we just got rid of.  */
      garbaged = 1;
    }
  else if (EQ (attr_name, intern ("foreground")))
    {
      unsigned long new_color = load_color (f, attr_value);
      unload_color (f, face->foreground);
      face->foreground = new_color;
      garbaged = 1;
    }
  else if (EQ (attr_name, intern ("background")))
    {
      unsigned long new_color = load_color (f, attr_value);
      unload_color (f, face->background);
      face->background = new_color;
      garbaged = 1;
    }
  else if (EQ (attr_name, intern ("background-pixmap")))
    {
      unsigned int w, h;
      unsigned long new_pixmap = load_pixmap (f, attr_value, &w, &h);
      x_destroy_bitmap (f, face->stipple);
      face->stipple = (Pixmap) new_pixmap;
      face->pixmap_w = w;
      face->pixmap_h = h;
      garbaged = 1;
    }
  else if (EQ (attr_name, intern ("underline")))
    {
      int new = !NILP (attr_value);
      face->underline = new;
    }
  else
    error ("unknown face attribute");

  if (id == 0 || id == 1)
    recompute_basic_faces (f);

  /* We must redraw the frame whenever any face font or color changes,
     because it's possible that a merged (display) face
     contains the font or color we just replaced.
     And we must inhibit any Expose events until the redraw is done,
     since they would try to use the invalid display faces.  */
  if (garbaged)
    SET_FRAME_GARBAGED (f);

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
syms_of_w32faces ()
{
  Qface = intern ("face");
  staticpro (&Qface);
  Qmouse_face = intern ("mouse-face");
  staticpro (&Qmouse_face);
  Qpixmap_spec_p = intern ("pixmap-spec-p");
  staticpro (&Qpixmap_spec_p);

  DEFVAR_INT ("region-face", &region_face,
    "Face number to use to highlight the region\n\
The region is highlighted with this face\n\
when Transient Mark mode is enabled and the mark is active.");

  defsubr (&Spixmap_spec_p);
  defsubr (&Sframe_face_alist);
  defsubr (&Sset_frame_face_alist);
  defsubr (&Smake_face_internal);
  defsubr (&Sset_face_attribute_internal);
  defsubr (&Sinternal_next_face_id);
}
