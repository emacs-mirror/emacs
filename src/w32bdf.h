/* Definitions and header for handling BDF fonts on the Microsoft W32 API.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* Based heavily on code by H. Miyashita for Meadow (a descendant of
   MULE for W32). */

#ifndef EMACS_W32BDF_H
#define EMACS_W32BDF_H

#define BDF_FIRST_OFFSET_TABLE 0x200
#define BDF_SECOND_OFFSET_TABLE 0x80
#define BDF_SECOND_OFFSET(x) ((x) & 0x7f)
#define BDF_FIRST_OFFSET(x) (((x) >> 8) | (((x) & 0x80) << 1))

#define BDF_FONT_CACHE_SIZE 5000
#define BDF_FONT_CLEAR_SIZE 1000

/* Structure of glyph information of one character.  */
typedef struct
{
  int dwidth;			/* width in pixels */
  int bbw, bbh, bbox, bboy;	/* bounding box in pixels */
} glyph_metric;

typedef struct
{
  glyph_metric metric;
  int bitmap_size;		/* byte lengh of the following slots */
  unsigned char *bitmap;	/*  */
} glyph_struct;

typedef struct fchar *pfont_char;

typedef struct
{
  glyph_metric metric;
  pfont_char psrc;
  HBITMAP hbmp;
} cache_bitmap;

typedef struct fchar
{
  unsigned char *offset;
  cache_bitmap *pcbmp;
} font_char;

typedef struct
{
  char *filename;
  HANDLE hfile;
  HANDLE hfilemap;
  unsigned char *font;
  unsigned char *seeked;
  DWORD size;

  font_char *chtbl[BDF_FIRST_OFFSET_TABLE];
  int llx, lly, urx, ury;	/* Font bounding box */

  int yoffset;
  int relative_compose;
  int default_ascent;

  unsigned char *registry;
  unsigned char *encoding;
  unsigned char *slant;
/*  unsigned char *width; */

  int width;
  int height;
  int pixsz;
} bdffont;


#define BDF_FILE_SIZE_MAX 256*1024*1024 /* 256Mb */
#define BDF_FONT_FILE(font) (((bdffont*)(font))->filename)
#define MAKELENDSHORT(c1, c2) (unsigned short)((c1) | ((c2) << 8))

bdffont *w32_init_bdf_font (char *filename);
void w32_free_bdf_font (bdffont *fontp);
int w32_get_bdf_glyph (bdffont *fontp, int index, int size,
                       glyph_struct *glyph);
int w32_BDF_TextOut (bdffont *fontp, HDC hdc, int left,
                     int top, unsigned char *text, int dim,
                     int bytelen, int fixed_pitch_size);
int w32_BDF_to_x_font (char *file, char* xstr, int len);

#endif  /* EMACS_W32BDF_H */
