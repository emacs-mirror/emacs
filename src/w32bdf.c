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
#include "keyboard.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "blockinput.h"
#include "w32gui.h"
#include "w32term.h"
#include "w32bdf.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* 10 planes */
#define BDF_CODEPOINT_HEAP_INITIAL_SIZE (96 * 10)
/* about 96 characters */
#define BDF_BITMAP_HEAP_INITIAL_SIZE    (64 * 96)

HANDLE hbdf_cp_heap = INVALID_HANDLE_VALUE;
HANDLE hbdf_bmp_heap = INVALID_HANDLE_VALUE;

void w32_free_bdf_font(bdffont *fontp);
bdffont *w32_init_bdf_font(char *filename);

cache_bitmap cached_bitmap_slots[BDF_FONT_CACHE_SIZE];
cache_bitmap *pcached_bitmap_latest = cached_bitmap_slots;

#define FONT_CACHE_SLOT_OVER_P(p) ((p) >= cached_bitmap_slots + BDF_FONT_CACHE_SIZE)

static int 
search_file_line(char *key, char *start, int len, char **val, char **next)
{
  unsigned int linelen;
  unsigned char *p;

  p = memchr(start, '\n', len);
  if (!p) return -1;
  for (;(unsigned char *)start < p;start++)
    {
      if ((*start != ' ') && (*start != '\t')) break;
    }
  linelen = (char *) p - start + 1;
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

char*
get_quoted_string(char *start, char *end)
{
  char *p, *q, *result;

  p = memchr(start, '\"', end - start);
  if (!p) return NULL;
  p++;
  q = memchr(p, '\"', end - p);
  if (!q) return NULL;

  result = (char*) xmalloc(q - p + 1);

  memcpy(result, p, q - p);
  result[q - p] = '\0';

  return result;
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

  fontp->registry = NULL;
  fontp->encoding = NULL;
  fontp->slant = NULL;
/*  fontp->width = NULL; */

  flag = proceed_file_line("FONTBOUNDINGBOX", start, &len,
			   (char **)&p, (char **)&q);
  if (!flag) return 0;
  bbw = strtol(p, (char **)&start, 10);
  p = start;
  bbh = strtol(p, (char **)&start, 10);
  p = start;
  bbx = strtol(p, (char **)&start, 10);
  p = start;
  bby = strtol(p, (char **)&start, 10);

  fontp->llx = bbx;
  fontp->lly = bby;
  fontp->urx = bbw + bbx;
  fontp->ury = bbh + bby;
  fontp->width = bbw;
  fontp->height = bbh;
  start = q;
  flag = proceed_file_line("STARTPROPERTIES", start, &len,
			   (char **)&p, (char **)&q);
  if (!flag) return 1;

  flag = 0;

  do {
    start = q;
    if (search_file_line("PIXEL_SIZE", start, len,
			 (char **)&p, (char **)&q) == 1)
      {
	val1 = atoi(p);
        fontp->pixsz = val1;
      }
    else if (search_file_line("FONT_ASCENT", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
	val1 = atoi(p);
	fontp->ury = val1;
      }
    else if (search_file_line("FONT_DESCENT", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
	val1 = atoi(p);
	fontp->lly = -val1;
      }
    else if (search_file_line("_MULE_BASELINE_OFFSET", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
	val1 = atoi(p);
	fontp->yoffset = -val1;
      }
    else if (search_file_line("_MULE_RELATIVE_COMPOSE", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
	val1 = atoi(p);
	fontp->relative_compose = val1;
      }
    else if (search_file_line("_MULE_DEFAULT_ASCENT", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
	val1 = atoi(p);
	fontp->default_ascent = val1;
      }
    else if (search_file_line("CHARSET_REGISTRY", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
        fontp->registry = get_quoted_string(p, q);
      }
    else if (search_file_line("CHARSET_ENCODING", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
        fontp->encoding = get_quoted_string(p, q);
      }
    else if (search_file_line("SLANT", start, len,
			      (char **)&p, (char **)&q) == 1)
      {
        fontp->slant = get_quoted_string(p, q);
      }
/*
    else if (search_file_line("SETWIDTH_NAME", start, len,
    			      (char **)&p, (char **)&q) == 1)
      {
        fontp->width = get_quoted_string(p, q);
      }
*/
    else
      {
	flag = search_file_line("ENDPROPERTIES", start, len,
				(char **)&p, (char **)&q);
      }
    if (flag == -1) return 0;
    len -= (q - start);
  }while(flag == 0);
  start = q;
  flag = proceed_file_line("CHARS", start, &len, (char **)&p, (char **)&q);
  if (!flag) return 0;
  fontp->nchars = atoi(p);
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

  if (hbdf_cp_heap == INVALID_HANDLE_VALUE)
    hbdf_cp_heap = HeapCreate(0, BDF_CODEPOINT_HEAP_INITIAL_SIZE, 0);
  if (hbdf_bmp_heap == INVALID_HANDLE_VALUE)
    hbdf_bmp_heap = HeapCreate(0, BDF_BITMAP_HEAP_INITIAL_SIZE, 0);

  if (!hbdf_cp_heap || !hbdf_bmp_heap)
    error("Fail to create heap for BDF.");

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
    bdffontp->chtbl[i] = NULL;
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
  int i, j;
  font_char *pch;
  cache_bitmap *pcb;

  UnmapViewOfFile(fontp->hfilemap);
  CloseHandle(fontp->hfilemap);
  CloseHandle(fontp->hfile);

  if (fontp->registry) xfree(fontp->registry);
  if (fontp->encoding) xfree(fontp->encoding);
  if (fontp->slant) xfree(fontp->slant);
/*  if (fontp->width) xfree(fontp->width); */

  xfree(fontp->filename);
  for(i = 0;i < BDF_FIRST_OFFSET_TABLE;i++)
    {
      pch = fontp->chtbl[i];
      if (pch)
	{
	  for (j = 0;j < BDF_SECOND_OFFSET_TABLE;j++)
	    {
	      pcb = pch[j].pcbmp;
 	      if (pcb)
 		{
		  if (pcb->pbmp)
		    HeapFree(hbdf_bmp_heap, 0, pcb->pbmp);
 		  pcb->psrc = NULL;
	    }
	    }
	  HeapFree(hbdf_cp_heap, 0, pch);
        }
    }
  xfree(fontp);
}

static font_char*
get_cached_font_char(bdffont *fontp, int index)
{
  font_char *pch, *result;

  if (!BDF_CODEPOINT_RANGE_COVER_P(index))
    return NULL;

  pch = fontp->chtbl[BDF_FIRST_OFFSET(index)];
  if (!pch)
    return NULL;

  result = &pch[BDF_SECOND_OFFSET(index)];

  if (!result->offset) return NULL;

  return result;
}

static font_char*
cache_char_offset(bdffont *fontp, int index, unsigned char *offset)
{
  font_char *pch, *result;

  if (!BDF_CODEPOINT_RANGE_COVER_P(index))
    return NULL;

  pch = fontp->chtbl[BDF_FIRST_OFFSET(index)];
  if (!pch)
    {
      pch = fontp->chtbl[BDF_FIRST_OFFSET(index)] =
	(font_char*) HeapAlloc(hbdf_cp_heap,
			       HEAP_ZERO_MEMORY, 
			       sizeof(font_char) *
                               BDF_SECOND_OFFSET_TABLE);
      if (!pch) return NULL;
      /* memset(pch, 0, sizeof(font_char) * BDF_SECOND_OFFSET_TABLE); */
    }

  result = &pch[BDF_SECOND_OFFSET(index)];
  result->offset = offset;

  return result;
}

static font_char*
seek_char(bdffont *fontp, int index)
{
  font_char *result;
  int len, flag, font_index;
  unsigned char *start, *p, *q;

  if (!fontp->seeked) return NULL;

  start = fontp->seeked;
  len = fontp->size - (start - fontp->font);

  do {
    flag = proceed_file_line("ENCODING", start, &len,
			     (char **)&p, (char **)&q);
    if (!flag)
      {
	fontp->seeked = NULL;
	return NULL;
      }
    font_index = atoi(p);
    result = cache_char_offset(fontp, font_index, q);
    if (!result) return NULL;

    start = result->offset;
  } while (font_index != index);
  fontp->seeked = start;

  return result;
}

static void
clear_cached_bitmap_slots()
{
  int i;
  cache_bitmap *p;

  p = pcached_bitmap_latest;
  for (i = 0;i < BDF_FONT_CLEAR_SIZE;i++)
    {
      if (p->psrc)
	{
	  if (p->pbmp)
	    HeapFree(hbdf_bmp_heap, 0, p->pbmp);
	  p->psrc->pcbmp = NULL;
	  p->psrc = NULL;
	}
      p++;
      if (FONT_CACHE_SLOT_OVER_P(p))
	p = cached_bitmap_slots;
    }
}

#define GET_HEX_VAL(x) ((isdigit(x)) ? ((x) - '0') : \
			(((x) >= 'A') && ((x) <= 'F')) ? ((x) - 'A' + 10) : \
			(((x) >= 'a') && ((x) <= 'f')) ? ((x) - 'a' + 10) : \
			(-1))

int
w32_get_bdf_glyph(bdffont *fontp, int index, int size, glyph_struct *glyph)
{
  font_char *pch;
  unsigned char *start, *p, *q, *bitmapp;
  unsigned char val, val1, val2;
  int i, j, len, flag, consumed;
  int align, rowbytes;

  pch = get_cached_font_char(fontp, index);
  if (!pch)
    {
      pch = seek_char(fontp, index);
      if (!pch)
        return 0;
    }

  start = pch->offset;

  if ((size == 0) && pch->pcbmp)
    {
      glyph->metric = pch->pcbmp->metric;
      return 1;
    }

  len = fontp->size - (start - fontp->font);

  flag = proceed_file_line("DWIDTH", start, &len, (char **)&p, (char **)&q);
  if (!flag)
    return 0;
  glyph->metric.dwidth = atoi(p);

  start = q;
  flag = proceed_file_line("BBX", start, &len, (char **)&p, (char **)&q);
  if (!flag)
    return 0;
  glyph->metric.bbw = strtol(p, (char **)&start, 10);
  p = start;
  glyph->metric.bbh = strtol(p, (char **)&start, 10);
  p = start;
  glyph->metric.bbox = strtol(p, (char **)&start, 10);
  p = start;
  glyph->metric.bboy = strtol(p, (char **)&start, 10);

  if (size == 0) return 1;

  start = q;
  flag = proceed_file_line("BITMAP", start, &len, (char **)&p, (char **)&q);
  if (!flag)
    return 0;

  consumed = 0;
  flag = 0;
  p = q;
  bitmapp = glyph->bitmap;
  rowbytes = (glyph->metric.bbw + 7) / 8;
  /* DIB requires DWORD alignment.  */
  align = sizeof(DWORD) - rowbytes % sizeof(DWORD);
  consumed = glyph->metric.bbh * (rowbytes + align);
  glyph->bitmap_size = consumed;
  glyph->row_byte_size = rowbytes;
  if (size < consumed) return 0;

  for(i = 0;i < glyph->metric.bbh;i++)
    {
      q = memchr(p, '\n', len);
      if (!q) return 0;
      for(j = 0;((q > p) && (j < rowbytes));j++)
	{
	  int ival = GET_HEX_VAL(*p);

	  if (ival == -1) return 0;
	  val1 = ival;
	  p++;
	  ival = GET_HEX_VAL(*p);
	  if (ival == -1) return 0;
	  val2 = ival;
	  p++;
	  val = (unsigned char)((val1 << 4) | val2);
	  if (val) flag = 1;
	  *bitmapp++ = val;
	}
      for(j = 0;j < align;j++)
	*bitmapp++ = 0x00;
      p = q + 1;
    }

  /* If this glyph is white space, return -1. */
  if (flag == 0) return -1;

  return consumed;
}

static
cache_bitmap*
get_bitmap_with_cache(bdffont *fontp, int index)
{
  int bitmap_size, bitmap_real_size;
  font_char *pch;
  cache_bitmap* pcb;
  unsigned char *pbmp;
  glyph_struct glyph;

  pch = get_cached_font_char(fontp, index);
  if (pch)
    {
      pcb = pch->pcbmp;
      if (pcb) return pcb;
    }

  bitmap_size = ((fontp->urx - fontp->llx) / 8 + 3) * (fontp->ury - fontp->lly)
    + 256;
  glyph.bitmap = (unsigned char*) alloca(sizeof(unsigned char) * bitmap_size);

  bitmap_real_size = w32_get_bdf_glyph(fontp, index, bitmap_size, &glyph);

  if (bitmap_real_size == 0)
    return NULL;

  pch = get_cached_font_char(fontp, index);
  if (!pch) return NULL;

  if (bitmap_real_size > 0)
    {
       pbmp = (unsigned char*) HeapAlloc(hbdf_bmp_heap, 0,
					 bitmap_real_size);
       if (!pbmp) return NULL;
       memcpy(pbmp, glyph.bitmap, bitmap_real_size);
    }
  else
    pbmp = NULL; /* white space character */

  pcb = pcached_bitmap_latest;
  if (pcb->psrc)
    clear_cached_bitmap_slots();

  pcb->psrc = pch;
  pcb->metric = glyph.metric;
  pcb->pbmp = pbmp;
  pcb->bitmap_size = glyph.bitmap_size;
  pcb->row_byte_size = glyph.row_byte_size;

  pch->pcbmp = pcb;
  
  pcached_bitmap_latest++;
  if (FONT_CACHE_SLOT_OVER_P(pcached_bitmap_latest))
    pcached_bitmap_latest = cached_bitmap_slots;

  return pcb;
}

static HBITMAP
create_offscreen_bitmap(HDC hdc, int width, int height, unsigned char **bitsp)
{
  struct {
    BITMAPINFOHEADER h;
    RGBQUAD c[2];
  } info;

  memset(&info, 0, sizeof(info));
  info.h.biSize = sizeof(BITMAPINFOHEADER);
  info.h.biWidth = width;
  info.h.biHeight = -height;
  info.h.biPlanes = 1;
  info.h.biBitCount = 1;
  info.h.biCompression = BI_RGB;
  info.c[1].rgbRed = info.c[1].rgbGreen = info.c[1].rgbBlue = 255;

  return CreateDIBSection(hdc, (LPBITMAPINFO)&info,
			  DIB_RGB_COLORS, bitsp, NULL, 0);
}

glyph_metric *
w32_BDF_TextMetric(bdffont *fontp, unsigned char *text, int dim)
{
  int index;
  cache_bitmap *pcb;

  if (dim == 1)
    index = *text;
  else
    index = MAKELENDSHORT(text[1], text[0]);

  pcb = get_bitmap_with_cache(fontp, index);
  if (!pcb)
    return NULL;

  return &(pcb->metric);
}

int
w32_BDF_TextOut(bdffont *fontp, HDC hdc, int left,
		 int top, unsigned char *text, int dim, int bytelen,
		 int fixed_pitch_size)
{
  int index, btop;
  unsigned char *textp;
  cache_bitmap *pcb;
  HBRUSH hFgBrush, hOrgBrush;
  HANDLE horgobj;
  UINT textalign;
  int width, height;
  HDC hCompatDC;
  int ret = 1;
  static HBITMAP hBMP = 0;
  static HDC DIBsection_hdc = 0;
  static int DIBsection_width, DIBsection_height;
  static unsigned char *bits;

  hCompatDC = CreateCompatibleDC(hdc);
  if (!hCompatDC)
    return 0;

  textalign = GetTextAlign(hdc);
  
  hFgBrush = CreateSolidBrush(GetTextColor(hdc));
  hOrgBrush = SelectObject(hdc, hFgBrush);

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
	  index = MAKELENDSHORT(textp[0], textp[1]);
	  textp += 2;
	}
      pcb = get_bitmap_with_cache(fontp, index);
      if (!pcb)
	{
	  ret = 0;
	  break;
	    }
      if (pcb->pbmp)
	{
	  width = pcb->metric.bbw;
	  height = pcb->metric.bbh;
	  
	  if (!(hBMP
		&& (DIBsection_hdc == hdc)
		&& (DIBsection_width == width)
		&& (DIBsection_height == height)))
	    {
	      if (hBMP) DeleteObject(hBMP);
	      hBMP = create_offscreen_bitmap(hdc, width, height, &bits);
	      DIBsection_hdc = hdc;
	      DIBsection_width = width;
	      DIBsection_height = height;
	      if (!hBMP) return 0;
	}

	  memcpy(bits, pcb->pbmp, pcb->bitmap_size);

      if (textalign & TA_BASELINE)
	  btop = top - (pcb->metric.bbh + pcb->metric.bboy);
      else if (textalign & TA_BOTTOM)
	  btop = top - pcb->metric.bbh;
      else
	  btop = top;

	  horgobj = SelectObject(hCompatDC, hBMP);
	  BitBlt(hdc, left, btop, width, height, hCompatDC, 0, 0, 0xE20746);
	  SelectObject(hCompatDC, horgobj);
	}

      if (fixed_pitch_size)
	left += fixed_pitch_size;
      else
	left += pcb->metric.dwidth;
    }

  DeleteDC(hCompatDC);

  SelectObject(hdc, hOrgBrush);
  DeleteObject(hFgBrush);

  return ret;
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
  bzero (font, sizeof (*font));

  font->bdf = bdf_font;
  font->hfont = 0;

  /* NTEMACS_TODO: Better way of determining if a font is double byte
     or not. */
  font->double_byte_p = bdf_font->nchars > 255 ? 1 : 0;

  w32_cache_char_metrics (font);

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

    /* Set global flag fonts_changed_p to non-zero if the font loaded
       has a character with a smaller width than any other character
       before, or if the font loaded has a smaller height than any
       other font loaded before.  If this happens, it will make a
       glyph matrix reallocation necessary.  */
    fonts_changed_p |= x_compute_min_glyph_bounds (f);

    UNBLOCK_INPUT;
    dpyinfo->n_fonts++;
    return fontp;
}

/* Check a file for an XLFD string describing it.  */
int w32_BDF_to_x_font (char *file, char* xstr, int len)
{
  HANDLE hfile, hfilemap;
  BY_HANDLE_FILE_INFORMATION fileinfo;
  char *font, *start, *p, *q;
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
