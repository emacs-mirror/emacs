/* Interface to Little CMS
   Copyright (C) 2017-2018 Free Software Foundation, Inc.

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

#ifdef HAVE_LCMS2

#include <lcms2.h>
#include <math.h>

#include "lisp.h"

typedef struct
{
  double J;
  double a;
  double b;
} lcmsJab_t;

#ifdef WINDOWSNT
# include <windows.h>
# include "w32.h"

DEF_DLL_FN (cmsFloat64Number, cmsCIE2000DeltaE,
	    (const cmsCIELab* Lab1, const cmsCIELab* Lab2, cmsFloat64Number Kl,
	     cmsFloat64Number Kc, cmsFloat64Number Kh));
DEF_DLL_FN (cmsHANDLE, cmsCIECAM02Init,
	    (cmsContext ContextID, const cmsViewingConditions* pVC));
DEF_DLL_FN (void, cmsCIECAM02Forward,
	    (cmsHANDLE hModel, const cmsCIEXYZ* pIn, cmsJCh* pOut));
DEF_DLL_FN (void, cmsCIECAM02Reverse,
	    (cmsHANDLE hModel, const cmsJCh* pIn, cmsCIEXYZ* pOut));
DEF_DLL_FN (void, cmsCIECAM02Done, (cmsHANDLE hModel));
DEF_DLL_FN (cmsBool, cmsWhitePointFromTemp,
	    (cmsCIExyY* WhitePoint, cmsFloat64Number TempK));
DEF_DLL_FN (void, cmsxyY2XYZ, (cmsCIEXYZ* Dest, const cmsCIExyY* Source));

static bool lcms_initialized;

static bool
init_lcms_functions (void)
{
  HMODULE library = w32_delayed_load (Qlcms2);

  if (!library)
    return false;

  LOAD_DLL_FN (library, cmsCIE2000DeltaE);
  LOAD_DLL_FN (library, cmsCIECAM02Init);
  LOAD_DLL_FN (library, cmsCIECAM02Forward);
  LOAD_DLL_FN (library, cmsCIECAM02Reverse);
  LOAD_DLL_FN (library, cmsCIECAM02Done);
  LOAD_DLL_FN (library, cmsWhitePointFromTemp);
  LOAD_DLL_FN (library, cmsxyY2XYZ);
  return true;
}

# undef cmsCIE2000DeltaE
# undef cmsCIECAM02Init
# undef cmsCIECAM02Forward
# undef cmsCIECAM02Reverse
# undef cmsCIECAM02Done
# undef cmsWhitePointFromTemp
# undef cmsxyY2XYZ

# define cmsCIE2000DeltaE      fn_cmsCIE2000DeltaE
# define cmsCIECAM02Init       fn_cmsCIECAM02Init
# define cmsCIECAM02Forward    fn_cmsCIECAM02Forward
# define cmsCIECAM02Reverse    fn_cmsCIECAM02Reverse
# define cmsCIECAM02Done       fn_cmsCIECAM02Done
# define cmsWhitePointFromTemp fn_cmsWhitePointFromTemp
# define cmsxyY2XYZ            fn_cmsxyY2XYZ

#endif	/* WINDOWSNT */

static bool
parse_lab_list (Lisp_Object lab_list, cmsCIELab *color)
{
#define PARSE_LAB_LIST_FIELD(field)					\
  if (CONSP (lab_list) && NUMBERP (XCAR (lab_list)))			\
    {									\
      color->field = XFLOATINT (XCAR (lab_list));			\
      lab_list = XCDR (lab_list);					\
    }									\
  else									\
    return false;

  PARSE_LAB_LIST_FIELD (L);
  PARSE_LAB_LIST_FIELD (a);
  PARSE_LAB_LIST_FIELD (b);

  return true;
}

/* http://www.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf> */

DEFUN ("lcms-cie-de2000", Flcms_cie_de2000, Slcms_cie_de2000, 2, 5, 0,
       doc: /* Compute CIEDE2000 metric distance between COLOR1 and COLOR2.
Each color is a list of L*a*b* coordinates, where the L* channel ranges from
0 to 100, and the a* and b* channels range from -128 to 128.
Optional arguments KL, KC, KH are weighting parameters for lightness,
chroma, and hue, respectively. The parameters each default to 1.  */)
  (Lisp_Object color1, Lisp_Object color2,
   Lisp_Object kL, Lisp_Object kC, Lisp_Object kH)
{
  cmsCIELab Lab1, Lab2;
  cmsFloat64Number Kl, Kc, Kh;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color1) && parse_lab_list (color1, &Lab1)))
    signal_error ("Invalid color", color1);
  if (!(CONSP (color2) && parse_lab_list (color2, &Lab2)))
    signal_error ("Invalid color", color1);
  if (NILP (kL))
    Kl = 1.0f;
  else if (!(NUMBERP (kL) && (Kl = XFLOATINT(kL))))
    wrong_type_argument(Qnumberp, kL);
  if (NILP (kC))
    Kc = 1.0f;
  else if (!(NUMBERP (kC) && (Kc = XFLOATINT(kC))))
    wrong_type_argument(Qnumberp, kC);
  if (NILP (kL))
    Kh = 1.0f;
  else if (!(NUMBERP (kH) && (Kh = XFLOATINT(kH))))
    wrong_type_argument(Qnumberp, kH);

  return make_float (cmsCIE2000DeltaE (&Lab1, &Lab2, Kl, Kc, Kh));
}

static double
deg2rad (double degrees)
{
  return M_PI * degrees / 180.0;
}

static double
rad2deg (double radians)
{
  return 180.0 * radians / M_PI;
}

static cmsCIEXYZ illuminant_d65 = { .X = 95.0455, .Y = 100.0, .Z = 108.8753 };

static void
default_viewing_conditions (const cmsCIEXYZ *wp, cmsViewingConditions *vc)
{
  vc->whitePoint.X = wp->X;
  vc->whitePoint.Y = wp->Y;
  vc->whitePoint.Z = wp->Z;
  vc->Yb = 20;
  vc->La = 100;
  vc->surround = AVG_SURROUND;
  vc->D_value = 1.0;
}

/* FIXME: code duplication */

static bool
parse_xyz_list (Lisp_Object xyz_list, cmsCIEXYZ *color)
{
#define PARSE_XYZ_LIST_FIELD(field)					\
  if (CONSP (xyz_list) && NUMBERP (XCAR (xyz_list)))			\
    {									\
      color->field = 100.0 * XFLOATINT (XCAR (xyz_list));		\
      xyz_list = XCDR (xyz_list);					\
    }									\
  else									\
    return false;

  PARSE_XYZ_LIST_FIELD (X);
  PARSE_XYZ_LIST_FIELD (Y);
  PARSE_XYZ_LIST_FIELD (Z);

  return true;
}

static bool
parse_jch_list (Lisp_Object jch_list, cmsJCh *color)
{
#define PARSE_JCH_LIST_FIELD(field)					\
  if (CONSP (jch_list) && NUMBERP (XCAR (jch_list)))			\
    {									\
      color->field = XFLOATINT (XCAR (jch_list));			\
      jch_list = XCDR (jch_list);					\
    }									\
  else									\
    return false;

  PARSE_JCH_LIST_FIELD (J);
  PARSE_JCH_LIST_FIELD (C);
  PARSE_JCH_LIST_FIELD (h);

  if (! NILP (jch_list))
    return false;
  return true;
}

static bool
parse_jab_list (Lisp_Object jab_list, lcmsJab_t *color)
{
#define PARSE_JAB_LIST_FIELD(field)					\
  if (CONSP (jab_list) && NUMBERP (XCAR (jab_list)))			\
    {									\
      color->field = XFLOATINT (XCAR (jab_list));			\
      jab_list = XCDR (jab_list);					\
    }									\
  else									\
    return false;

  PARSE_JAB_LIST_FIELD (J);
  PARSE_JAB_LIST_FIELD (a);
  PARSE_JAB_LIST_FIELD (b);

  return true;
}

static bool
parse_viewing_conditions (Lisp_Object view, const cmsCIEXYZ *wp,
                          cmsViewingConditions *vc)
{
#define PARSE_VIEW_CONDITION_FLOAT(field)				\
  if (CONSP (view) && NUMBERP (XCAR (view)))				\
    {									\
      vc->field = XFLOATINT (XCAR (view));				\
      view = XCDR (view);						\
    }									\
  else									\
    return false;
#define PARSE_VIEW_CONDITION_INT(field)					\
  if (CONSP (view) && NATNUMP (XCAR (view)))				\
    {									\
      CHECK_RANGED_INTEGER (XCAR (view), 1, 4);				\
      vc->field = XINT (XCAR (view));					\
      view = XCDR (view);						\
    }									\
  else									\
    return false;

  PARSE_VIEW_CONDITION_FLOAT (Yb);
  PARSE_VIEW_CONDITION_FLOAT (La);
  PARSE_VIEW_CONDITION_INT (surround);
  PARSE_VIEW_CONDITION_FLOAT (D_value);

  if (! NILP (view))
    return false;

  vc->whitePoint.X = wp->X;
  vc->whitePoint.Y = wp->Y;
  vc->whitePoint.Z = wp->Z;
  return true;
}

static void
xyz_to_jch (const cmsCIEXYZ *xyz, cmsJCh *jch, const cmsViewingConditions *vc)
{
  cmsHANDLE h;

  h = cmsCIECAM02Init (0, vc);
  cmsCIECAM02Forward (h, xyz, jch);
  cmsCIECAM02Done (h);
}

static void
jch_to_xyz (const cmsJCh *jch, cmsCIEXYZ *xyz, const cmsViewingConditions *vc)
{
  cmsHANDLE h;

  h = cmsCIECAM02Init (0, vc);
  cmsCIECAM02Reverse (h, jch, xyz);
  cmsCIECAM02Done (h);
}

static void
jch_to_jab (const cmsJCh *jch, lcmsJab_t *jab, double FL, double c1, double c2)
{
  double Mp = 43.86 * log (1.0 + c2 * (jch->C * sqrt (sqrt (FL))));
  jab->J = 1.7 * jch->J / (1.0 + (c1 * jch->J));
  jab->a = Mp * cos (deg2rad (jch->h));
  jab->b = Mp * sin (deg2rad (jch->h));
}

static void
jab_to_jch (const lcmsJab_t *jab, cmsJCh *jch, double FL, double c1, double c2)
{
  jch->J = jab->J / (1.0 + c1 * (100.0 - jab->J));
  jch->h = atan2 (jab->b, jab->a);
  double Mp = hypot (jab->a, jab->b);
  jch->h = rad2deg (jch->h);
  if (jch->h < 0.0)
    jch->h += 360.0;
  jch->C = (exp (c2 * Mp) - 1.0) / (c2 * sqrt (sqrt (FL)));
}

DEFUN ("lcms-xyz->jch", Flcms_xyz_to_jch, Slcms_xyz_to_jch, 1, 3, 0,
       doc: /* Convert CIE CAM02 JCh to CIE XYZ.
COLOR is a list (X Y Z), with Y scaled about unity.
Optional arguments WHITEPOINT and VIEW are the same as in `lcms-cam02-ucs',
which see.  */)
  (Lisp_Object color, Lisp_Object whitepoint, Lisp_Object view)
{
  cmsViewingConditions vc;
  cmsJCh jch;
  cmsCIEXYZ xyz, xyzw;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color) && parse_xyz_list (color, &xyz)))
    signal_error ("Invalid color", color);
  if (NILP (whitepoint))
    xyzw = illuminant_d65;
  else if (!(CONSP (whitepoint) && parse_xyz_list (whitepoint, &xyzw)))
    signal_error ("Invalid white point", whitepoint);
  if (NILP (view))
    default_viewing_conditions (&xyzw, &vc);
  else if (!(CONSP (view) && parse_viewing_conditions (view, &xyzw, &vc)))
    signal_error ("Invalid viewing conditions", view);

  xyz_to_jch(&xyz, &jch, &vc);
  return list3 (make_float (jch.J), make_float (jch.C), make_float (jch.h));
}

DEFUN ("lcms-jch->xyz", Flcms_jch_to_xyz, Slcms_jch_to_xyz, 1, 3, 0,
       doc: /* Convert CIE XYZ to CIE CAM02 JCh.
COLOR is a list (J C h), where lightness of white is equal to 100, and hue
is given in degrees.
Optional arguments WHITEPOINT and VIEW are the same as in `lcms-cam02-ucs',
which see.  */)
  (Lisp_Object color, Lisp_Object whitepoint, Lisp_Object view)
{
  cmsViewingConditions vc;
  cmsJCh jch;
  cmsCIEXYZ xyz, xyzw;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color) && parse_jch_list (color, &jch)))
    signal_error ("Invalid color", color);
  if (NILP (whitepoint))
    xyzw = illuminant_d65;
  else if (!(CONSP (whitepoint) && parse_xyz_list (whitepoint, &xyzw)))
    signal_error ("Invalid white point", whitepoint);
  if (NILP (view))
    default_viewing_conditions (&xyzw, &vc);
  else if (!(CONSP (view) && parse_viewing_conditions (view, &xyzw, &vc)))
    signal_error ("Invalid viewing conditions", view);

  jch_to_xyz(&jch, &xyz, &vc);
  return list3 (make_float (xyz.X / 100.0),
                make_float (xyz.Y / 100.0),
                make_float (xyz.Z / 100.0));
}

DEFUN ("lcms-jch->jab", Flcms_jch_to_jab, Slcms_jch_to_jab, 1, 3, 0,
       doc: /* Convert CIE CAM02 JCh to CAM02-UCS J'a'b'.
COLOR is a list (J C h) as described in `lcms-jch->xyz', which see.
Optional arguments WHITEPOINT and VIEW are the same as in `lcms-cam02-ucs',
which see.  */)
  (Lisp_Object color, Lisp_Object whitepoint, Lisp_Object view)
{
  cmsViewingConditions vc;
  lcmsJab_t jab;
  cmsJCh jch;
  cmsCIEXYZ xyzw;
  double FL, k, k4;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color) && parse_jch_list (color, &jch)))
    signal_error ("Invalid color", color);
  if (NILP (whitepoint))
    xyzw = illuminant_d65;
  else if (!(CONSP (whitepoint) && parse_xyz_list (whitepoint, &xyzw)))
    signal_error ("Invalid white point", whitepoint);
  if (NILP (view))
    default_viewing_conditions (&xyzw, &vc);
  else if (!(CONSP (view) && parse_viewing_conditions (view, &xyzw, &vc)))
    signal_error ("Invalid viewing conditions", view);

  k = 1.0 / (1.0 + (5.0 * vc.La));
  k4 = k * k * k * k;
  FL = vc.La * k4 + 0.1 * (1 - k4) * (1 - k4) * cbrt (5.0 * vc.La);
  jch_to_jab (&jch, &jab, FL, 0.007, 0.0228);
  return list3 (make_float (jab.J), make_float (jab.a), make_float (jab.b));
}

DEFUN ("lcms-jab->jch", Flcms_jab_to_jch, Slcms_jab_to_jch, 1, 3, 0,
       doc: /* Convert CAM02-UCS J'a'b' to CIE CAM02 JCh.
COLOR is a list (J' a' b'), where white corresponds to lightness J equal to 100.
Optional arguments WHITEPOINT and VIEW are the same as in `lcms-cam02-ucs',
which see.  */)
  (Lisp_Object color, Lisp_Object whitepoint, Lisp_Object view)
{
  cmsViewingConditions vc;
  cmsJCh jch;
  lcmsJab_t jab;
  cmsCIEXYZ xyzw;
  double FL, k, k4;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color) && parse_jab_list (color, &jab)))
    signal_error ("Invalid color", color);
  if (NILP (whitepoint))
    xyzw = illuminant_d65;
  else if (!(CONSP (whitepoint) && parse_xyz_list (whitepoint, &xyzw)))
    signal_error ("Invalid white point", whitepoint);
  if (NILP (view))
    default_viewing_conditions (&xyzw, &vc);
  else if (!(CONSP (view) && parse_viewing_conditions (view, &xyzw, &vc)))
    signal_error ("Invalid viewing conditions", view);

  k = 1.0 / (1.0 + (5.0 * vc.La));
  k4 = k * k * k * k;
  FL = vc.La * k4 + 0.1 * (1 - k4) * (1 - k4) * cbrt (5.0 * vc.La);
  jab_to_jch (&jab, &jch, FL, 0.007, 0.0228);
  return list3 (make_float (jch.J), make_float (jch.C), make_float (jch.h));
}

/* References:
   Li, Luo et al. "The CRI-CAM02UCS colour rendering index." COLOR research
   and application, 37 No.3, 2012.
   Luo et al. "Uniform colour spaces based on CIECAM02 colour appearance
   model." COLOR research and application, 31 No.4, 2006. */

DEFUN ("lcms-cam02-ucs", Flcms_cam02_ucs, Slcms_cam02_ucs, 2, 4, 0,
       doc: /* Compute CAM02-UCS metric distance between COLOR1 and COLOR2.
Each color is a list of XYZ tristimulus values, with Y scaled about unity.
Optional argument WHITEPOINT is the XYZ white point, which defaults to
illuminant D65.
Optional argument VIEW is a list containing the viewing conditions, and
is of the form (YB LA SURROUND DVALUE) where SURROUND corresponds to
  1   AVG_SURROUND
  2   DIM_SURROUND
  3   DARK_SURROUND
  4   CUTSHEET_SURROUND
The default viewing conditions are (20 100 1 1).  */)
  (Lisp_Object color1, Lisp_Object color2, Lisp_Object whitepoint,
   Lisp_Object view)
{
  cmsViewingConditions vc;
  cmsJCh jch1, jch2;
  cmsCIEXYZ xyz1, xyz2, xyzw;
  lcmsJab_t jab1, jab2;
  double FL, k, k4;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  if (!(CONSP (color1) && parse_xyz_list (color1, &xyz1)))
    signal_error ("Invalid color", color1);
  if (!(CONSP (color2) && parse_xyz_list (color2, &xyz2)))
    signal_error ("Invalid color", color2);
  if (NILP (whitepoint))
    xyzw = illuminant_d65;
  else if (!(CONSP (whitepoint) && parse_xyz_list (whitepoint, &xyzw)))
    signal_error ("Invalid white point", whitepoint);
  if (NILP (view))
    default_viewing_conditions (&xyzw, &vc);
  else if (!(CONSP (view) && parse_viewing_conditions (view, &xyzw, &vc)))
    signal_error ("Invalid view conditions", view);

  xyz_to_jch (&xyz1, &jch1, &vc);
  xyz_to_jch (&xyz2, &jch2, &vc);

  k = 1.0 / (1.0 + (5.0 * vc.La));
  k4 = k * k * k * k;
  FL = vc.La * k4 + 0.1 * (1 - k4) * (1 - k4) * cbrt (5.0 * vc.La);
  jch_to_jab (&jch1, &jab1, FL, 0.007, 0.0228);
  jch_to_jab (&jch2, &jab2, FL, 0.007, 0.0228);

  return make_float (hypot (jab2.J - jab1.J,
                            hypot (jab2.a - jab1.a, jab2.b - jab1.b)));
}

DEFUN ("lcms-temp->white-point", Flcms_temp_to_white_point, Slcms_temp_to_white_point, 1, 1, 0,
       doc: /* Return XYZ black body chromaticity from TEMPERATURE given in K.
Valid range of TEMPERATURE is from 4000K to 25000K.  */)
  (Lisp_Object temperature)
{
  cmsFloat64Number tempK;
  cmsCIExyY whitepoint;
  cmsCIEXYZ wp;

#ifdef WINDOWSNT
  if (!lcms_initialized)
    lcms_initialized = init_lcms_functions ();
  if (!lcms_initialized)
    {
      message1 ("lcms2 library not found");
      return Qnil;
    }
#endif

  CHECK_NUMBER_OR_FLOAT (temperature);

  tempK = XFLOATINT (temperature);
  if (!(cmsWhitePointFromTemp (&whitepoint, tempK)))
    signal_error("Invalid temperature", temperature);
  cmsxyY2XYZ (&wp, &whitepoint);
  return list3 (make_float (wp.X), make_float (wp.Y), make_float (wp.Z));
}

DEFUN ("lcms2-available-p", Flcms2_available_p, Slcms2_available_p, 0, 0, 0,
       doc: /* Return t if lcms2 color calculations are available in this instance of Emacs.  */)
     (void)
{
#ifdef WINDOWSNT
  Lisp_Object found = Fassq (Qlcms2, Vlibrary_cache);
  if (CONSP (found))
    return XCDR (found);
  else
    {
      Lisp_Object status;
      lcms_initialized = init_lcms_functions ();
      status = lcms_initialized ? Qt : Qnil;
      Vlibrary_cache = Fcons (Fcons (Qlcms2, status), Vlibrary_cache);
      return status;
    }
#else  /* !WINDOWSNT */
  return Qt;
#endif
}


/* Initialization */
void
syms_of_lcms2 (void)
{
  defsubr (&Slcms_cie_de2000);
  defsubr (&Slcms_xyz_to_jch);
  defsubr (&Slcms_jch_to_xyz);
  defsubr (&Slcms_jch_to_jab);
  defsubr (&Slcms_jab_to_jch);
  defsubr (&Slcms_cam02_ucs);
  defsubr (&Slcms2_available_p);
  defsubr (&Slcms_temp_to_white_point);

  Fprovide (intern_c_string ("lcms2"), Qnil);
}

#endif /* HAVE_LCMS2 */
