/* Replacement termio.h file for building GNU Emacs on the Macintosh.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef _SYS_TERMIO_H
#define _SYS_TERMIO_H

typedef unsigned char cc_t;
typedef unsigned short tcflag_t;

#define NCCS 32

struct termio {
  tcflag_t c_iflag;		/* input modes */
  tcflag_t c_oflag;		/* output modes */
  tcflag_t c_cflag;		/* control modes */
  tcflag_t c_lflag;		/* local modes */
  cc_t c_cc[NCCS];		/* control chars */
};

/* c_cc subscript names */
#define VINTR 1
#define VQUIT 2
#define VERASE 3
#define VTIME 4
#define VMIN 5

/* c_iflag fields */
#define IGNBRK 0x1		/* ignore break condition */
#define ICRNL 0x2		/* map CR to NL on input */
#define IXON 0x4		/* enable start/stop output control */

/* c_oflag fields */
#define ONLCR 0x1		/* map CR to NL on output */
#define TABDLY 0x2		/* horizontal tab delays */
#define TAB3 0x4		/* expand tab to spaces */

/* c_cflag fields */
#define CBAUD 0x1
#define B9600 0x2

/* c_lflag fields */
#define ISIG 0x1		/* enable signals */
#define ICANON 0x2		/* canonical input (erase and kill processing) */
#define ECHO 0x3		/* enable echo */

#define TCSETAW 4
#define TCSETAF 5

#endif /* _SYS_TERMIO_H */
