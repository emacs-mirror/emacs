/* MS-DOS specific Lisp utilities.  Coded by Manabu Higashida, 1991.
   Major changes May-July 1993 Morten Welinder (only 10% original code left)
   Copyright (C) 1991, 1993 Free Software Foundation, Inc.

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


#include <config.h>

#ifdef MSDOS
/* The entire file is within this conditional */

#include <stdio.h>
#include <dos.h>
#include "lisp.h"
#include "buffer.h"
#include "termchar.h"
#include "termhooks.h"
#include "frame.h"
#include "dosfns.h"
#include "msdos.h"

DEFUN ("mode25", Fmode25, Smode25, 0, 0, "", "\
Changes the number of rows to 25.")
  ()
{
  union REGS regs;

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system)
    return Qnil;
#endif
  if (have_mouse) mouse_off ();
  regs.x.ax = 3;
  int86 (0x10, &regs, &regs);
  regs.x.ax = 0x1101;
  regs.h.bl = 0;
  int86 (0x10, &regs, &regs);
  regs.x.ax = 0x1200;
  regs.h.bl = 32;
  int86 (0x10, &regs, &regs);
  regs.x.ax = 3;
  int86 (0x10, &regs, &regs);
  Fset_frame_size (Fselected_frame (), ScreenCols (), ScreenRows ());
  Frecenter (Qnil);
  Fredraw_display ();
  if (have_mouse) mouse_init ();
  return Qnil;
}

DEFUN ("mode4350", Fmode4350, Smode4350, 0, 0, "", "\
Changes the number of rows to 43 (EGA) or 50 (VGA).")
  ()
{
  union REGS regs;

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system)
    return Qnil;
#endif
  if (have_mouse) mouse_off ();
  regs.x.ax = 3;
  int86 (0x10, &regs, &regs);
  regs.x.ax = 0x1112;
  regs.h.bl = 0;
  int86 (0x10, &regs, &regs);
  regs.x.ax = 0x1200;
  regs.h.bl = 32;
  int86 (0x10, &regs, &regs);
  regs.x.ax = 0x0100;
  regs.x.cx = 7;
  int86 (0x10, &regs, &regs);
  Fset_frame_size (Fselected_frame (), ScreenCols (), ScreenRows ());
  Frecenter (Qnil);
  Fredraw_display ();
  if (have_mouse) mouse_init ();
  return Qnil;
}

DEFUN ("int86", Fint86, Sint86, 2, 2, 0,
  "Call specific MSDOS interrupt number INTERRUPT with REGISTERS.\n\
Return the updated REGISTER vector.\n\
\n\
INTERRUPT should be an integer in the range 0 to 255.\n\
REGISTERS should be a vector produced by `make-register' and\n\
`set-register-value'.")
  (intno, regs)
  Lisp_Object intno, regs;
{
  register int i;
  int no;
  union REGS inregs, outregs;
  Lisp_Object val;

  CHECK_NUMBER (intno, 0);
  no = (unsigned long) XINT (intno);
  CHECK_VECTOR (regs, 1);
  if (no < 0 || no > 0xff || XVECTOR (regs)-> size != 8) 
    return Qnil;
  for (i = 0; i < 8; i++)
    CHECK_NUMBER (XVECTOR (regs)->contents[i], 1);

  inregs.x.ax    = (unsigned long) XFASTINT (XVECTOR (regs)->contents[0]);
  inregs.x.bx    = (unsigned long) XFASTINT (XVECTOR (regs)->contents[1]);
  inregs.x.cx    = (unsigned long) XFASTINT (XVECTOR (regs)->contents[2]);
  inregs.x.dx    = (unsigned long) XFASTINT (XVECTOR (regs)->contents[3]);
  inregs.x.si    = (unsigned long) XFASTINT (XVECTOR (regs)->contents[4]);
  inregs.x.di    = (unsigned long) XFASTINT (XVECTOR (regs)->contents[5]);
  inregs.x.cflag = (unsigned long) XFASTINT (XVECTOR (regs)->contents[6]);
  inregs.x.flags = (unsigned long) XFASTINT (XVECTOR (regs)->contents[7]);

  int86 (no, &inregs, &outregs);

  XVECTOR (regs)->contents[0] = make_number (outregs.x.ax);
  XVECTOR (regs)->contents[1] = make_number (outregs.x.bx);
  XVECTOR (regs)->contents[2] = make_number (outregs.x.cx);
  XVECTOR (regs)->contents[3] = make_number (outregs.x.dx);
  XVECTOR (regs)->contents[4] = make_number (outregs.x.si);
  XVECTOR (regs)->contents[5] = make_number (outregs.x.di);
  XVECTOR (regs)->contents[6] = make_number (outregs.x.cflag);
  XVECTOR (regs)->contents[7] = make_number (outregs.x.flags);

  return regs;
}

#ifndef HAVE_X_WINDOWS
/* Later we might want to control the mouse interface with this function,
   e.g., with respect to non-80 column screen modes.  */

DEFUN ("msdos-mouse-p", Fmsdos_mouse_p, Smsdos_mouse_p, 0, 0, 0, "\
Report whether a mouse is present.")
     ()
{
  if (have_mouse)
    return Qt;
  else
    return Qnil;
}

DEFUN ("set-mouse-position", Fset_mouse_position, Sset_mouse_position, 3, 3, 0,
  "Move the mouse pointer to the center of character cell (X,Y) in FRAME.\n\
WARNING:  If you use this under X windows,\n\
you should call `unfocus-frame' afterwards.")
  (frame, x, y)
     Lisp_Object frame, x, y;
{
  mouse_moveto (XINT (x), XINT (y));
}

/* Function to translate colour names to integers.  See lisp/term/pc-win.el
   for its definition.  */

Lisp_Object Qmsdos_color_translate;
#endif


int dos_country_code;
int dos_codepage;
Lisp_Object Vdos_version;

void
init_dosfns ()
{
  union REGS regs;
  _go32_dpmi_seginfo info;
  _go32_dpmi_registers dpmiregs;

#ifndef SYSTEM_MALLOC
  get_lim_data (); /* why the hell isn't this called elsewhere? */
#endif

  regs.x.ax = 0x3000;
  intdos (&regs, &regs);
  Vdos_version = Fcons (make_number (regs.h.al), make_number (regs.h.ah));

  /* Obtain the country code by calling Dos via Dpmi.  Don't rely on GO32.  */
  info.size = (34 + 15) / 16;
  if (_go32_dpmi_allocate_dos_memory (&info))
    dos_country_code = 1;
  else
    {
      dpmiregs.x.ax = 0x3800;
      dpmiregs.x.ds = info.rm_segment;
      dpmiregs.x.dx = 0;
      dpmiregs.x.ss = dpmiregs.x.sp = 0;
      _go32_dpmi_simulate_int (0x21, &dpmiregs);
      dos_country_code = dpmiregs.x.bx;
      _go32_dpmi_free_dos_memory (&info);
    }

  regs.x.ax = 0x6601;
  intdos (&regs, &regs);
  if (regs.x.cflag)
    /* Estimate code page from country code */
    switch (dos_country_code) 
      {
      case 45: /* Denmark */
      case 47: /* Norway */
	dos_codepage = 865;
	break;
      default:
	/* US */
	dos_codepage = 437;
      }
  else
    dos_codepage = regs.x.bx & 0xffff;
}

/*
 *	Define everything
 */
syms_of_dosfns ()
{
  defsubr (&Smode25);
  defsubr (&Smode4350);
  defsubr (&Sint86);
#ifndef HAVE_X_WINDOWS
  defsubr (&Smsdos_mouse_p);
  defsubr (&Sset_mouse_position);

  Qmsdos_color_translate = intern ("msdos-color-translate");
  staticpro (&Qmsdos_color_translate);
#endif

  DEFVAR_INT ("dos-country-code", &dos_country_code,
    "The country code returned by Dos when Emacs was started.\n\
Usually this is the international telephone prefix.");

  DEFVAR_INT ("dos-codepage", &dos_codepage,
	      "The codepage active when Emacs was started.\n\
The following are known:\n\
	437	United States\n\
	850	Multilingual (Latin I)\n\
	852	Slavic (Latin II)\n\
	857	Turkish\n\
	860	Portugal\n\
	861	Iceland\n\
	863	Canada (French)\n\
	865	Norway/Denmark");

  DEFVAR_LISP ("dos-version", &Vdos_version,
    "The (MAJOR . MINOR) Dos version (subject to modification with setver).");
}
#endif /* MSDOS */
