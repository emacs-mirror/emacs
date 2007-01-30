/* Definitions and header for handling BDF fonts on the Microsoft W32 API.
   Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007  Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Based heavily on code by H. Miyashita for Meadow (a descendant of
   MULE for W32). */

#ifndef EMACS_W32BDF_H
#define EMACS_W32BDF_H

#define BDF_FIRST_OFFSET_TABLE 0x200
#define BDF_SECOND_OFFSET_TABLE 0x80
#define BDF_SECOND_OFFSET(x) ((x) & 0x7f)
#define BDF_FIRST_OFFSET(x) (((x) >> 8) | (((x) & 0x80) << 1))
#define BDF_CODEPOINT_MAX (BDF_FIRST_OFFSET_TABLE * BDF_SECOND_OFFSET_TABLE)
#define BDF_CODEPOINT_RANGE_COVER_P(x) (((x) >= 0) && ((x) <= BDF_CODEPOINT_MAX))

#define BDF_FONT_CACHE_SIZE 3000
#define BDF_FONT_CLEAR_SIZE 600

/*
   GLYPH METRIC (# ... character's reference point)
   ^
 y |              (urx, ury)
   |  ^ +----------------+
 a | b| |character       | <- font bounding Box
 x | b| |                |
 i | h| | #(bbox, bboy)  |
 s |  v +----------------+
   |   (llx, lly)
   |    <---------------->
   |           bbw
   +----------------------->
   origin     x axis
 */



/* Structure of glyph information of one character.  */
typedef struct
{
  int dwidth;			/* width in pixels */
  int bbw, bbh, bbox, bboy;	/* bounding box in pixels */
} glyph_metric;

typedef struct
{
  glyph_metric metric;
  int row_byte_size;            /* size in bytes occupied by one row of the bitmap */
  int bitmap_size;		/* size in bytes of the following slots */
  unsigned char *bitmap;	/*  */
} glyph_struct;

typedef struct fchar *pfont_char;

typedef struct
{
  glyph_metric metric;
  pfont_char psrc;
  int row_byte_size;
  int bitmap_size;
  unsigned char *pbmp;
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
  int nchars;
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

/* arch-tag: 7499e9f2-197e-44cc-9274-373f00b51eec
   (do not change this comment) */
