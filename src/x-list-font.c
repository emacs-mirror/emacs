DEFUN ("x-list-fonts", Fx_list_fonts, Sx_list_fonts, 1, 5, 0,
  "Return a list of the names of available fonts matching PATTERN.\n\
If optional arguments FACE and FRAME are specified, return only fonts\n\
the same size as FACE on FRAME.\n\
PATTERN is a string, perhaps with wildcard characters;\n\
  the * character matches any substring, and\n\
  the ? character matches any single character.\n\
  PATTERN is case-insensitive.\n\
FACE is a face name--a symbol.\n\
\n\
The return value is a list of strings, suitable as arguments to\n\
set-face-font.\n\
\n\
Fonts Emacs can't use (i.e. proportional fonts) may or may not be excluded\n\
even if they match PATTERN and FACE.\n\
The optional fourth argument MAXIMUM sets a limit on how many\n\
fonts to match.  The first MAXIMUM fonts are reported.\n\
The optional fifth argument WIDTH, if specified, is a number of columns\n\
occupied by a character of a font.  In that case, return only fonts\n\
the WIDTH times as wide as FACE on FRAME.")
  (pattern, face, frame, maximum, width)
    Lisp_Object pattern, face, frame, maximum, width;
{
  FRAME_PTR f;
  int size, cols;
  int maxnames;

  check_x ();
  CHECK_STRING (pattern, 0);
  if (!NILP (face))
    CHECK_SYMBOL (face, 1);

  if (NILP (maximum))
    maxnames = 2000;
  else
    {
      CHECK_NATNUM (maximum, 0);
      maxnames = XINT (maximum);
    }

  if (!NILP (width))
    CHECK_NUMBER (width, 4);

  /* We can't simply call check_x_frame because this function may be
     called before any frame is created.  */
  if (NILP (frame))
    f = selected_frame;
  else
    {
      CHECK_LIVE_FRAME (frame, 0);
      f = XFRAME (frame);
    }
  if (! FRAME_X_P (f))
    {
      /* Perhaps we have not yet created any frame.  */
      f = NULL;
      face = Qnil;
    }

  /* Determine the width standard for comparison with the fonts we find.  */

  if (NILP (face))
    size = 0;
  else
    {
      int face_id;

      face_id = face_name_id_number (f, face);

      if (face_id < 0 || face_id >= FRAME_N_PARAM_FACES (f)
	  || FRAME_PARAM_FACES (f) [face_id] == 0
	  || FRAME_PARAM_FACES (f) [face_id]->font == (XFontStruct *) (~0))
	size = f->output_data.x->font->max_bounds.width;
      else
	size = FRAME_PARAM_FACES (f) [face_id]->font->max_bounds.width;

      if (!NILP (width))
	  size *= XINT (width);
    }

  {
    Lisp_Object args[2];

    args[0] = x_list_fonts (f, pattern, size, maxnames);
    if (f == NULL)
      /* We don't have to check fontsets.  */
      return args[0];
    args[1] = list_fontsets (f, pattern, size);
    return Fnconc (2, args);
  }
}
