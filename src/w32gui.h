/* Definitions and headers for communication on the Microsoft W32 API.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

#ifndef EMACS_W32GUI_H
#define EMACS_W32GUI_H
#include <windows.h>

#include "w32bdf.h"

/* Emulate XCharStruct.  */
typedef struct _XCharStruct
{
  short rbearing;
  short lbearing;
  short width;
  short ascent;
  short descent;
} XCharStruct;

enum w32_char_font_type
{
  UNKNOWN_FONT,
  ANSI_FONT,
  UNICODE_FONT,
  BDF_1D_FONT,
  BDF_2D_FONT
};

typedef struct W32FontStruct {
  enum w32_char_font_type font_type;
  TEXTMETRIC tm;
  HFONT hfont;
  bdffont *bdf;
  int double_byte_p;
  XCharStruct max_bounds;
  XCharStruct scratch;
  /* Only store info for ascii chars, if not fixed pitch.  */
  XCharStruct * per_char;
} W32FontStruct;

typedef struct W32FontStruct XFontStruct;

/* Emulate X GC's by keeping color and font info in a structure.  */
typedef struct _XGCValues
{
  COLORREF foreground;
  COLORREF background;
  XFontStruct * font;
} XGCValues;

#define GCForeground 0x01
#define GCBackground 0x02
#define GCFont 0x03

typedef HBITMAP Pixmap;
typedef HBITMAP Bitmap;

typedef XGCValues * GC;
typedef COLORREF Color;
typedef DWORD Time;
typedef HWND Window;
typedef HCURSOR Cursor;

/* Windows equivalent of XImage.  */
typedef struct _XImage
{
  unsigned char * data;
  BITMAPINFO info;
  /* Optional RGBQUAD array for palette follows (see BITMAPINFO docs).  */
} XImage;

#define FACE_DEFAULT (~0)

extern HINSTANCE hinst;
extern HINSTANCE hprevinst;
extern LPSTR lpCmdLine;
extern int nCmdShow;

/* Bit Gravity */

#define ForgetGravity		0
#define NorthWestGravity	1
#define NorthGravity		2
#define NorthEastGravity	3
#define WestGravity		4
#define CenterGravity		5
#define EastGravity		6
#define SouthWestGravity	7
#define SouthGravity		8
#define SouthEastGravity	9
#define StaticGravity		10

#define NoValue		0x0000
#define XValue  	0x0001
#define YValue		0x0002
#define WidthValue  	0x0004
#define HeightValue  	0x0008
#define AllValues 	0x000F
#define XNegative 	0x0010
#define YNegative 	0x0020

#define USPosition	(1L << 0) /* user specified x, y */
#define USSize		(1L << 1) /* user specified width, height */

#define PPosition	(1L << 2) /* program specified position */
#define PSize		(1L << 3) /* program specified size */
#define PMinSize	(1L << 4) /* program specified minimum size */
#define PMaxSize	(1L << 5) /* program specified maximum size */
#define PResizeInc	(1L << 6) /* program specified resize increments */
#define PAspect		(1L << 7) /* program specified min and max aspect ratios */
#define PBaseSize	(1L << 8) /* program specified base for incrementing */
#define PWinGravity	(1L << 9) /* program specified window gravity */

extern int XParseGeometry ();

#endif /* EMACS_W32GUI_H */
