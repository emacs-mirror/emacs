/* Implementation of BDF font handling on the Microsoft W32 API.
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

#include <windows.h>
#include "config.h"
#include "lisp.h"
#include "charset.h"
#include "fontset.h"
#include "blockinput.h"
#include "w32gui.h"
#include "w32term.h"
#include "w32bdf.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

void w32_free_bdf_font(bdffont *fontp);
bdffont *w32_init_bdf_font(char *filename);

static int 
search_file_line(char *key, char *start, int len, char **val, char **next)
{
  int linelen;
  unsigned char *p, *q;

  p = memchr(start, '\n', len);
  if (!p) return -1;
  for (;start < p;start++)
    {
      if ((*start != ' ') || (*start != '\t')) break;
    }
  linelen = p - start + 1;
  *next = p + 1;
  if (strncmp(start, key, min(strlen(key), linelen)) == 0)
    {
      *val = start + strlen(key);
      return 1;
    }
  
  return 0;
}

static int
proceed_file_line(char *key, char *start, int *len, char **val, char **next)
{
  int flag = 0;

  do {
    flag = search_file_line(key, start, *len, val, next);
    *len -= (int)(*next - start);
    start = *next;
  }while(flag == 0);

  if (flag == -1) return 0;
  return 1;
}
   
static int
set_bdf_font_info(bdffont *fontp)
{
  unsigned char *start, *p, *q;
  int len, flag;
  int bbw, bbh, bbx, bby;
  int val1;

  len = fontp->size;
  start = fontp->font;

  fontp->yoffset = 0;
  fontp->relative_compose = 0;
  fontp->default_ascent = 0;

  flag = proceed_file_line("FONTBOUNDINGBOX", start, &len, &p, &q);
  if (!flag) return 0;
  bbw = strtol(p, &start, 10);
  p = start;
  bbh = strtol(p, &start, 10);
  p = start;
  bbx = strtol(p, &start, 10);
  p = start;
  bby = strtol(p, &start, 10);

  fontp->llx = bbx;
  fontp->lly = bby;
  fontp->urx = bbw + bbx;
  fontp->ury = bbh + bby;
  fontp->width = bbw;
  fontp->height = bbh;
  start = q;
  flag = proceed_file_line("STARTPROPERTIES", start, &len, &p, &q);
  if (!flag) return 1;

  do {
    start = q;
    if (search_file_line("PIXEL_SIZE", start, len, &p, &q) == 1)
      {
	val1 = atoi(p);
        fontp->pixsz = val1;
      }
    else if (search_file_line("FONT_ASCENT", start, len, &p, &q) == 1)
      {
	val1 = atoi(p);
	fontp->ury = val1;
      }
    else if (search_file_line("FONT_DESCENT", start, len, &p, &q) == 1)
      {
	val1 = atoi(p);
	fontp->lly = -val1;
      }
    else if (search_file_line("_MULE_BASELINE_OFFSET", start, len, &p, &q) == 1)
      {
	val1 = atoi(p);
	fontp->yoffset = val1;
      }
    else if (search_file_line("_MULE_RELATIVE_COMPOSE", start, len, &p, &q) == 1)
      {
	val1 = atoi(p);
	fontp->relative_compose = val1;
      }
    else if (search_file_line("_MULE_DEFAULT_ASCENT", start, len, &p, &q) == 1)
      {
	val1 = atoi(p);
	fontp->default_ascent = val1;
      }
    else
      {
	flag = search_file_line("ENDPROPERTIES", start, len, &p, &q);
      }
    if (flag == -1) return 0;
    len -= (q - start);
  }while(flag == 0);
  start = q;
  flag = proceed_file_line("CHARS", start, &len, &p, &q);
  if (!flag) return 0;
  fontp->seeked = q;

  return 1;
}

bdffont*
w32_init_bdf_font(char *filename)
{
  HANDLE hfile, hfilemap;
  bdffont *bdffontp;
  unsigned char *font;
  BY_HANDLE_FILE_INFORMATION fileinfo;
  int i;

  hfile = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hfile == INVALID_HANDLE_VALUE) return NULL;
  if (!GetFileInformationByHandle(hfile, &fileinfo) ||
      (fileinfo.nFileSizeHigh != 0) ||
      (fileinfo.nFileSizeLow > BDF_FILE_SIZE_MAX))
    {
      CloseHandle(hfile);
      error("Fail to open BDF file.");
    }
  hfilemap = CreateFileMapping(hfile, NULL, PAGE_READONLY, 0, 0, NULL);
  if (hfilemap == INVALID_HANDLE_VALUE)
    {
      CloseHandle(hfile);
      error("Can't map font.");
    }

  font = MapViewOfFile(hfilemap, FILE_MAP_READ, 0, 0, 0);

  if (!font)
    {
      CloseHandle(hfile);
      CloseHandle(hfilemap);
      error("Can't view font.");
    }

  bdffontp = (bdffont *) xmalloc(sizeof(bdffont));
  
  for(i = 0;i < BDF_FIRST_OFFSET_TABLE;i++)
    bdffontp->offset[i] = NULL;
  bdffontp->size = fileinfo.nFileSizeLow;
  bdffontp->font = font;
  bdffontp->hfile = hfile;
  bdffontp->hfilemap = hfilemap;
  bdffontp->filename = (char*) xmalloc(strlen(filename) + 1);
  strcpy(bdffontp->filename, filename);
  
  if (!set_bdf_font_info(bdffontp))
    {
      w32_free_bdf_font(bdffontp);
      error("Invalid BDF font!");
    }
  return bdffontp;
}

void
w32_free_bdf_font(bdffont *fontp)
{
  int i;

  UnmapViewOfFile(fontp->hfilemap);
  CloseHandle(fontp->hfilemap);
  CloseHandle(fontp->hfile);
  xfree(fontp->filename);
  for(i = 0;i < BDF_FIRST_OFFSET_TABLE;i++)
    {
      if (fontp->offset[i]) xfree(fontp->offset[i]);
    }
  xfree(fontp);
}

static unsigned char*
get_cached_char_offset(bdffont *fontp, int index)
{
  unsigned char **offset1;
  unsigned char *offset2;
  int i;

  if (index > 0xffff)
    return NULL;

  offset1 = fontp->offset[BDF_FIRST_OFFSET(index)];
  if (!offset1)
    return NULL;
  offset2 = offset1[BDF_SECOND_OFFSET(index)];

  if (offset2) return offset2;

  return NULL;
}

static void
cache_char_offset(bdffont *fontp, int index, unsigned char *offset)
{
  unsigned char **offset1;
  int i;

  if (index > 0xffff)
    return;

  offset1 = fontp->offset[BDF_FIRST_OFFSET(index)];
  if (!offset1)
    {
      offset1 = fontp->offset[BDF_FIRST_OFFSET(index)] =
	(unsigned char **) xmalloc(sizeof(unsigned char*) *
				   BDF_SECOND_OFFSET_TABLE);
      memset(offset1, 0, sizeof(unsigned char*) * BDF_SECOND_OFFSET_TABLE);
    }
  offset1[BDF_SECOND_OFFSET(index)] = offset;

  return;
}

static unsigned char*
seek_char_offset(bdffont *fontp, int index)
{
  int len, flag, font_index;
  unsigned char *start, *p, *q;

  if (!fontp->seeked) return NULL;

  start = fontp->seeked;
  len = fontp->size - (start - fontp->font);

  do {
    flag = proceed_file_line("ENCODING", start, &len, &p, &q);
    if (!flag)
      {
	fontp->seeked = NULL;
	return NULL;
      }
    font_index = atoi(p);
    cache_char_offset(fontp, font_index, q);
    start = q;
  } while (font_index != index);
  fontp->seeked = q;

  return q;
}

#define GET_HEX_VAL(x) ((isdigit(x)) ? ((x) - '0') : \
			(((x) >= 'A') && ((x) <= 'Z')) ? ((x) - 'A' + 10) : \
			(((x) >= 'a') && ((x) <= 'z')) ? ((x) - 'a' + 10) : \
			(-1))

int
w32_get_bdf_glyph(bdffont *fontp, int index, int size, glyph_struct *glyph)
{
  unsigned char *start, *p, *q, *bitmapp;
  unsigned char val1, val2;
  int i, j, len, flag;

  start = get_cached_char_offset(fontp, index);
  if (!start)
    start = seek_char_offset(fontp, index);
  if (!start)
    return 0;

  len = fontp->size - (start - fontp->font);

  flag = proceed_file_line("DWIDTH", start, &len, &p, &q);
  if (!flag)
    return 0;
  glyph->dwidth = atoi(p);

  start = q;
  flag = proceed_file_line("BBX", start, &len, &p, &q);
  if (!flag)
    return 0;
  glyph->bbw = strtol(p, &start, 10);
  p = start;
  glyph->bbh = strtol(p, &start, 10);
  p = start;
  glyph->bbox = strtol(p, &start, 10);
  p = start;
  glyph->bboy = strtol(p, &start, 10);

  if (size == 0) return 1;

  start = q;
  flag = proceed_file_line("BITMAP", start, &len, &p, &q);
  if (!flag)
    return 0;

  p = q;
  bitmapp = glyph->bitmap;
  for(i = 0;i < glyph->bbh;i++)
    {
      q = memchr(p, '\n', len);
      if (!q) return 0;
      for(j = 0;((q > p) && (j < ((glyph->bbw + 7) / 8 )));j++)
	{
	  val1 = GET_HEX_VAL(*p);
	  if (val1 == -1) return 0;
	  p++;
	  val2 = GET_HEX_VAL(*p);
	  if (val2 == -1) return 0;
	  p++;
	  size--;
	  if (size <= 0) return 0;
	  /* NAND Operation.  */
	  *bitmapp++ = (unsigned char)~((val1 << 4) | val2);
	}
      /* CreateBitmap requires WORD alignment.  */
      if (j % 2)
	{
	  *bitmapp++ = 0xff;
	}
      p = q + 1;
    }

  return 1;
}

int
w32_BDF_TextOut(bdffont *fontp, HDC hdc, int left,
		 int top, unsigned char *text, int dim, int bytelen,
		 int fixed_pitch_size)
{
  int bitmap_size, index, btop;
  unsigned char *textp;
  glyph_struct glyph;
  HDC hCompatDC = 0;
  HBITMAP hBMP;
  HBRUSH hFgBrush, hOrgBrush;
  HANDLE holdobj, horgobj = 0;
  UINT textalign;
  int flag = 0;

  bitmap_size = ((fontp->urx - fontp->llx) / 8 + 2) * (fontp->ury - fontp->lly)
    + 256;

  glyph.bitmap = (unsigned char*) alloca(sizeof(unsigned char) * bitmap_size);

  hCompatDC = CreateCompatibleDC(hdc);

  textalign = GetTextAlign(hdc);
  
  SaveDC(hdc);

  hFgBrush = CreateSolidBrush(GetTextColor(hdc));
  hOrgBrush = SelectObject(hdc, hFgBrush);
  SetTextColor(hdc, RGB(0, 0, 0));
  SetBkColor(hdc, RGB(0xff, 0xff, 0xff));

  textp = text;
  while(bytelen > 0)
    {
      if (dim == 1)
	{
	  index = *textp++;
	  bytelen--;
	}
      else
	{
	  bytelen -= 2;
	  if (bytelen < 0) break;
	  index = MAKELENDSHORT(textp[1], textp[0]);
	  textp += 2;
	}
      if (!w32_get_bdf_glyph(fontp, index, bitmap_size, &glyph))
	{
	  if (horgobj)
	    {
	      SelectObject(hCompatDC, horgobj);
	      DeleteObject(hBMP);
	    }
	  DeleteDC(hCompatDC);
	  return 0;
	}
      hBMP = CreateBitmap(glyph.bbw, glyph.bbh, 1, 1, glyph.bitmap);
      if (textalign & TA_BASELINE)
	{
	  btop = top - (glyph.bbh + glyph.bboy);
	}
      else if (textalign & TA_BOTTOM)
	{
	  btop = top - glyph.bbh;
	}
      else
	{
	  btop = top;
	}

      if (horgobj)
	{
	  SelectObject(hCompatDC, hBMP);
	  DeleteObject(holdobj);
	  holdobj = hBMP;
	}
      else
	{
	  horgobj = SelectObject(hCompatDC, hBMP);
	  holdobj = hBMP;
	}
#if 0
      BitBlt(hdc, left, btop, glyph.bbw, glyph.bbh, hCompatDC, 0, 0, SRCCOPY);
#else
      BitBlt(hdc, left, btop, glyph.bbw, glyph.bbh, hCompatDC, 0, 0, 0xB8074A);
#endif
      if (fixed_pitch_size)
	left += fixed_pitch_size;
      else
	left += glyph.dwidth;
    }
  SelectObject(hCompatDC, horgobj);
  SelectObject(hdc, hOrgBrush);
  DeleteObject(hFgBrush);
  DeleteObject(hBMP);
  DeleteDC(hCompatDC);
  RestoreDC(hdc, -1);

  return 1;
}

struct font_info *w32_load_bdf_font (struct frame *f, char *fontname,
                                     int size, char* filename)
{
  struct w32_display_info *dpyinfo = FRAME_W32_DISPLAY_INFO (f);
  struct font_info *fontp;
  XFontStruct *font;
  bdffont* bdf_font;

  bdf_font = w32_init_bdf_font (filename);

  if (!bdf_font) return NULL;

  font = (XFontStruct *) xmalloc (sizeof (XFontStruct));

  font->bdf = bdf_font;
  font->hfont = 0;

  /* Do we need to create the table?  */
  if (dpyinfo->font_table_size == 0)
    {
      dpyinfo->font_table_size = 16;
      dpyinfo->font_table
        = (struct font_info *) xmalloc (dpyinfo->font_table_size
                                        * sizeof (struct font_info));
    }
  /* Do we need to grow the table?  */
  else if (dpyinfo->n_fonts
           >= dpyinfo->font_table_size)
    {
      dpyinfo->font_table_size *= 2;
      dpyinfo->font_table
        = (struct font_info *) xrealloc (dpyinfo->font_table,
                                         (dpyinfo->font_table_size
                                          * sizeof (struct font_info)));
    }

  fontp = dpyinfo->font_table + dpyinfo->n_fonts;

  /* Now fill in the slots of *FONTP.  */
  BLOCK_INPUT;
  fontp->font = font;
  fontp->font_idx = dpyinfo->n_fonts;
  fontp->name = (char *) xmalloc (strlen (fontname) + 1);
  bcopy (fontname, fontp->name, strlen (fontname) + 1);
  fontp->full_name = fontp->name;
  fontp->size = FONT_WIDTH (font);
  fontp->height = FONT_HEIGHT (font);

    /* The slot `encoding' specifies how to map a character
       code-points (0x20..0x7F or 0x2020..0x7F7F) of each charset to
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF, 0:0x2020..0x7F7F,
       the font code-points (0:0x20..0x7F, 1:0xA0..0xFF,
       0:0x2020..0x7F7F, 1:0xA0A0..0xFFFF, 3:0x20A0..0x7FFF, or
       2:0xA020..0xFF7F).  For the moment, we don't know which charset
       uses this font.  So, we set informatoin in fontp->encoding[1]
       which is never used by any charset.  If mapping can't be
       decided, set FONT_ENCODING_NOT_DECIDED.  */
    fontp->encoding[1] = FONT_ENCODING_NOT_DECIDED;
    fontp->baseline_offset = bdf_font->yoffset;
    fontp->relative_compose = bdf_font->relative_compose;
    fontp->default_ascent = bdf_font->default_ascent;

    UNBLOCK_INPUT;
    dpyinfo->n_fonts++;
    return fontp;
}

/* Check a file for an XFLD string describing it.  */
int w32_BDF_to_x_font (char *file, char* xstr, int len)
{
  HANDLE hfile, hfilemap;
  BY_HANDLE_FILE_INFORMATION fileinfo;
  unsigned char *font, *start, *p, *q;
  int flag, size, retval = 0;

  hfile = CreateFile (file, GENERIC_READ, FILE_SHARE_READ, NULL,
                      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hfile == INVALID_HANDLE_VALUE) return 0;
  if (!GetFileInformationByHandle(hfile, &fileinfo) ||
      (fileinfo.nFileSizeHigh != 0) ||
      (fileinfo.nFileSizeLow > BDF_FILE_SIZE_MAX))
    {
      CloseHandle (hfile);
      return 0;
    }
  size = fileinfo.nFileSizeLow;

  hfilemap = CreateFileMapping (hfile, NULL, PAGE_READONLY, 0, 0, NULL);
  if (hfilemap == INVALID_HANDLE_VALUE)
    {
      CloseHandle (hfile);
      return 0;
    }

  font = MapViewOfFile (hfilemap, FILE_MAP_READ, 0, 0, 0);
  if (!font)
    {
      CloseHandle (hfile);
      CloseHandle (hfilemap);
      return 0;
    }
  start = font;

  flag = proceed_file_line ("FONT ", start, &size, &p, &q);
  if (flag)
    {
      /* If font provides a description of itself, check it is a
         full XLFD before accepting it.  */
      int count = 0;
      char *s;

      for (s = p; s < q; s++)
        if (*s == '\n')
          break;
        else if (*s == '-')
          count++;
      if (count == 14 && q - p - 1 <= len)
        {
          strncpy (xstr, p, q-p-1);
          xstr[q-p-1] = '\0';
          /* Files may have DOS line ends (ie still ^M on end).  */
          if (iscntrl(xstr[q-p-2]))
            xstr[q-p-2] = '\0';

          retval = 1;
        }
    }
  CloseHandle (hfile);
  CloseHandle (hfilemap);
  return retval;
}
