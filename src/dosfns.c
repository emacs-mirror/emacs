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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


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
#include <go32.h>
#include <dirent.h>

DEFUN ("int86", Fint86, Sint86, 2, 2, 0,
  "Call specific MSDOS interrupt number INTERRUPT with REGISTERS.\n\
Return the updated REGISTER vector.\n\
\n\
INTERRUPT should be an integer in the range 0 to 255.\n\
REGISTERS should be a vector produced by `make-register' and\n\
`set-register-value'.")
  (interrupt, registers)
  Lisp_Object interrupt, registers;
{
  register int i;
  int no;
  union REGS inregs, outregs;
  Lisp_Object val;

  CHECK_NUMBER (interrupt, 0);
  no = (unsigned long) XINT (interrupt);
  CHECK_VECTOR (registers, 1);
  if (no < 0 || no > 0xff || XVECTOR (registers)-> size != 8) 
    return Qnil;
  for (i = 0; i < 8; i++)
    CHECK_NUMBER (XVECTOR (registers)->contents[i], 1);

  inregs.x.ax    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[0]);
  inregs.x.bx    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[1]);
  inregs.x.cx    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[2]);
  inregs.x.dx    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[3]);
  inregs.x.si    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[4]);
  inregs.x.di    = (unsigned long) XFASTINT (XVECTOR (registers)->contents[5]);
  inregs.x.cflag = (unsigned long) XFASTINT (XVECTOR (registers)->contents[6]);
  inregs.x.flags = (unsigned long) XFASTINT (XVECTOR (registers)->contents[7]);

  int86 (no, &inregs, &outregs);

  XVECTOR (registers)->contents[0] = make_number (outregs.x.ax);
  XVECTOR (registers)->contents[1] = make_number (outregs.x.bx);
  XVECTOR (registers)->contents[2] = make_number (outregs.x.cx);
  XVECTOR (registers)->contents[3] = make_number (outregs.x.dx);
  XVECTOR (registers)->contents[4] = make_number (outregs.x.si);
  XVECTOR (registers)->contents[5] = make_number (outregs.x.di);
  XVECTOR (registers)->contents[6] = make_number (outregs.x.cflag);
  XVECTOR (registers)->contents[7] = make_number (outregs.x.flags);

  return registers;
}

DEFUN ("msdos-memget", Fdos_memget, Sdos_memget, 2, 2, 0,
  "Read DOS memory at offset ADDRESS into VECTOR.\n\
Return the updated VECTOR.")
  (address, vector)
  Lisp_Object address, vector;
{
  register int i;
  int offs, len;
  char *buf;
  Lisp_Object val;

  CHECK_NUMBER (address, 0);
  offs = (unsigned long) XINT (address);
  CHECK_VECTOR (vector, 1);
  len = XVECTOR (vector)-> size;
  if (len < 1 || len > 2048 || address < 0 || address > 0xfffff - len) 
    return Qnil;
  buf = alloca (len);
  dosmemget (offs, len, buf);
  
  for (i = 0; i < len; i++)
    XVECTOR (vector)->contents[i] = make_number (buf[i]);

  return vector;
}

DEFUN ("msdos-memput", Fdos_memput, Sdos_memput, 2, 2, 0,
  "Write DOS memory at offset ADDRESS from VECTOR.")
  (address, vector)
  Lisp_Object address, vector;
{
  register int i;
  int offs, len;
  char *buf;
  Lisp_Object val;

  CHECK_NUMBER (address, 0);
  offs = (unsigned long) XINT (address);
  CHECK_VECTOR (vector, 1);
  len = XVECTOR (vector)-> size;
  if (len < 1 || len > 2048 || address < 0 || address > 0xfffff - len) 
    return Qnil;
  buf = alloca (len);

  for (i = 0; i < len; i++)
    {
      CHECK_NUMBER (XVECTOR (vector)->contents[i], 1);
      buf[i] = (unsigned char) XFASTINT (XVECTOR (vector)->contents[i]) & 0xFF;
    }

  dosmemput (buf, len, offs);
  return Qt;
}

DEFUN ("msdos-set-keyboard", Fmsdos_set_keyboard, Smsdos_set_keyboard, 1, 2, 0,
  "Set keyboard layout according to COUNTRY-CODE.\n\
If the optional argument ALLKEYS is non-nil, the keyboard is mapped for\n\
all keys; otherwise it is only used when the ALT key is pressed.\n\
The current keyboard layout is available in dos-keyboard-code.")
  (country_code, allkeys)
  Lisp_Object country_code;
{
  CHECK_NUMBER (country_code, 0);
  if (!dos_set_keyboard (XINT (country_code), !NILP (allkeys)))
    return Qnil;
  return Qt;
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

/* Function to translate colour names to integers.  See lisp/term/pc-win.el
   for its definition.  */

Lisp_Object Qmsdos_color_translate;
#endif


DEFUN ("msdos-mouse-init", Fmsdos_mouse_init, Smsdos_mouse_init, 0, 0, "",
  "Initialize and enable mouse if available.")
  ()
{
  if (have_mouse)
    {
      have_mouse = 1;
      mouse_init ();
      return Qt;
    }
  return Qnil;
}

DEFUN ("msdos-mouse-enable", Fmsdos_mouse_enable, Smsdos_mouse_enable, 0, 0, "",
  "Enable mouse if available.")
  ()
{
  if (have_mouse)
    {
      have_mouse = 1;
      mouse_on ();
    }
  return have_mouse ? Qt : Qnil;
}

DEFUN ("msdos-mouse-disable", Fmsdos_mouse_disable, Smsdos_mouse_disable, 0, 0, "",
  "Disable mouse if available.")
  ()
{
  mouse_off ();
  if (have_mouse) have_mouse = -1;
  return Qnil;
}

DEFUN ("insert-startup-screen", Finsert_startup_screen, Sinsert_startup_screen, 0, 0, "", "\
Insert copy of screen contents prior to starting emacs.\n\
Return nil if startup screen is not available.")
  ()
{
  char *s;
  int rows, cols;
  int i, j;
  
  if (!dos_get_saved_screen (&s, &rows, &cols))
    return Qnil;
  
  for (i = 0; i < rows; i++)
    {
      for (j = 0; j < cols; j++)
	{
	  insert_char (*s, 1);
	  s += 2;
	}
      insert_char ('\n', 1);
    }

  return Qt;
}

/* country info */
int dos_country_code;
int dos_codepage;
int dos_timezone_offset;
int dos_decimal_point;
int dos_keyboard_layout;
unsigned char dos_country_info[DOS_COUNTRY_INFO];

int dos_hyper_key;
int dos_super_key;
int dos_keypad_mode;

Lisp_Object Vdos_version;
Lisp_Object Vdos_display_scancodes;
  
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
  info.size = (sizeof(dos_country_info) + 15) / 16;
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
      dosmemget (info.rm_segment * 16, DOS_COUNTRY_INFO, dos_country_info);
      _go32_dpmi_free_dos_memory (&info);
    }
  dos_set_keyboard (dos_country_code, 0);

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

#if __DJGPP__ >= 2

  /* Without this, we never see hidden files.  */
  __opendir_flags |= __OPENDIR_FIND_HIDDEN;

  /* Under LFN, preserve the case of files as recorded in the directory.  */
  if (!NILP (Fmsdos_long_file_names ()))
    __opendir_flags |= __OPENDIR_PRESERVE_CASE;

#endif
}

/*
 *	Define everything
 */
syms_of_dosfns ()
{
  defsubr (&Sint86);
  defsubr (&Sdos_memget);
  defsubr (&Sdos_memput);
  defsubr (&Smsdos_mouse_init);
  defsubr (&Smsdos_mouse_enable);
  defsubr (&Smsdos_set_keyboard);
  defsubr (&Sinsert_startup_screen);
  defsubr (&Smsdos_mouse_disable);
#ifndef HAVE_X_WINDOWS
  defsubr (&Smsdos_mouse_p);
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

  DEFVAR_INT ("dos-timezone-offset", &dos_timezone_offset,
    "The current timezone offset to UTC in minutes.
Implicitly modified when the TZ variable is changed.");
  
  DEFVAR_LISP ("dos-version", &Vdos_version,
    "The (MAJOR . MINOR) Dos version (subject to modification with setver).");

  DEFVAR_LISP ("dos-display-scancodes", &Vdos_display_scancodes,
    "*When non-nil, the keyboard scan-codes are displayed at the bottom right\n\
corner of the display (typically at the end of the mode line).\n\
The output format is: scan code:char code*modifiers.");
  Vdos_display_scancodes = Qnil;
  
  DEFVAR_INT ("dos-hyper-key", &dos_hyper_key,
    "*If set to 1, use right ALT key as hyper key.\n\
If set to 2, use right CTRL key as hyper key.");
  dos_hyper_key = 0;
  
  DEFVAR_INT ("dos-super-key", &dos_super_key,
    "*If set to 1, use right ALT key as super key.\n\
If set to 2, use right CTRL key as super key.");
  dos_super_key = 0;
  
  DEFVAR_INT ("dos-keypad-mode", &dos_keypad_mode,
    "*Controls what key code is returned by a key in the numeric keypad.\n\
The `numlock ON' action is only taken if no modifier keys are pressed.\n\
The value is an integer constructed by adding the following bits together:\n\
 \n\
  0x00	Digit key returns digit    (if numlock ON)\n\
  0x01	Digit key returns kp-digit (if numlock ON)\n\
  0x02	Digit key returns M-digit  (if numlock ON)\n\
  0x03	Digit key returns edit key (if numlock ON)\n\
 \n\
  0x00	Grey key returns char      (if numlock ON)\n\
  0x04	Grey key returns kp-key    (if numlock ON)\n\
 \n\
  0x00	Digit key returns digit    (if numlock OFF)\n\
  0x10	Digit key returns kp-digit (if numlock OFF)\n\
  0x20	Digit key returns M-digit  (if numlock OFF)\n\
  0x30	Digit key returns edit key (if numlock OFF)\n\
 \n\
  0x00	Grey key returns char      (if numlock OFF)\n\
  0x40	Grey key returns kp-key    (if numlock OFF)\n\
 \n\
  0x200	ALT-0..ALT-9 in top-row produces shifted codes.");
  dos_keypad_mode = 0x75;
  
  DEFVAR_INT ("dos-keyboard-layout", &dos_keyboard_layout,
    "Contains the country code for the current keyboard layout.\n\
Use msdos-set-keyboard to select another keyboard layout.");
  dos_keyboard_layout = 1;	/* US */
  
  DEFVAR_INT ("dos-decimal-point", &dos_decimal_point,
    "If non-zero, it contains the character to be returned when the\n\
decimal point key in the numeric keypad is pressed when Num Lock is on.\n\
If zero, the decimal point key returns the country code specific value.");
  dos_decimal_point = 0;
}
#endif /* MSDOS */
