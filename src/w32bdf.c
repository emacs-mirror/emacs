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
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "blockinput.h"
#include "w32gui.h"
#include "w32term.h"
#include "w32bdf.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* Portion of GDI Objects which the font cache is allowed to use. This
   can be quite high, since the font cache is the only part of Emacs
   that uses a large number of GDI objects, but there should still be
   some GDI objects reserved for other uses.  */
#define CACHE_GDI_ALLOWANCE 9 / 10

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
   
char*
get_quoted_string(char *start, char *end)
{
  char *p, *q, *result;

  p = memchr(start, '\"', end - start);
  q = 0;

  if (!p) return NULL;
  p++;
  q = memchr(p, '\"', end - q);
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

  flag = 0;

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
    else if (search_file_line("CHARSET_REGISTRY", start, len, &p, &q) == 1)
      {
        fontp->registry = get_quoted_string(p, q);
      }
    else if (search_file_line("CHARSET_ENCODING", start, len, &p, &q) == 1)
      {
        fontp->encoding = get_quoted_string(p, q);
      }
    else if (search_file_line("SLANT", start, len, &p, &q) == 1)
      {
        fontp->slant = get_quoted_string(p, q);
      }
/*
    else if (search_file_line("SETWIDTH_NAME", start, len, &p, &q) == 1)
      {
        fontp->width = get_quoted_string(p, q);
      }
*/
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
	      if (pcb) pcb->psrc = NULL;
	    }
	  xfree(pch);
        }
    }
  xfree(fontp);
}

static font_char*
get_cached_font_char(bdffont *fontp, int index)
{
  font_char *pch, *result;
  int i;

  if (index > 0xffff)
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
  int i;

  if (index > 0xffff)
    return NULL;

  pch = fontp->chtbl[BDF_FIRST_OFFSET(index)];
  if (!pch)
    {
      pch = fontp->chtbl[BDF_FIRST_OFFSET(index)] =
	(font_char*) xmalloc(sizeof(font_char) *
				   BDF_SECOND_OFFSET_TABLE);
      memset(pch, 0, sizeof(font_char) * BDF_SECOND_OFFSET_TABLE);
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
    flag = proceed_file_line("ENCODING", start, &len, &p, &q);
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

#define GET_HEX_VAL(x) ((isdigit(x)) ? ((x) - '0') : \
			(((x) >= 'A') && ((x) <= 'Z')) ? ((x) - 'A' + 10) : \
			(((x) >= 'a') && ((x) <= 'z')) ? ((x) - 'a' + 10) : \
			(-1))

int
w32_get_bdf_glyph(bdffont *fontp, int index, int size, glyph_struct *glyph)
{
  font_char *pch;
  unsigned char *start, *p, *q, *bitmapp;
  unsigned char val1, val2;
  int i, j, len, flag;

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

  flag = proceed_file_line("DWIDTH", start, &len, &p, &q);
  if (!flag)
    return 0;
  glyph->metric.dwidth = atoi(p);

  start = q;
  flag = proceed_file_line("BBX", start, &len, &p, &q);
  if (!flag)
    return 0;
  glyph->metric.bbw = strtol(p, &start, 10);
  p = start;
  glyph->metric.bbh = strtol(p, &start, 10);
  p = start;
  glyph->metric.bbox = strtol(p, &start, 10);
  p = start;
  glyph->metric.bboy = strtol(p, &start, 10);

  if (size == 0) return 1;

  start = q;
  flag = proceed_file_line("BITMAP", start, &len, &p, &q);
  if (!flag)
    return 0;

  p = q;
  bitmapp = glyph->bitmap;
  for(i = 0;i < glyph->metric.bbh;i++)
    {
      q = memchr(p, '\n', len);
      if (!q) return 0;
      for(j = 0;((q > p) && (j < ((glyph->metric.bbw + 7) / 8 )));j++)
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

#define NEXT_CACHE_SLOT(n) (((n) + 1 >= BDF_FONT_CACHE_SIZE) ? 0 : ((n) + 1))

static
cache_bitmap*
get_bitmap_with_cache(bdffont *fontp, int index)
{
  int bitmap_size;
  font_char *pch;
  cache_bitmap* pcb;
  HBITMAP hbmp;
  glyph_struct glyph;
  static cache_bitmap cached_bitmap_slots[BDF_FONT_CACHE_SIZE];
  static int cache_in_slot = 0;		/* the next slot to use */
  static int cache_out_slot = 0;	/* the last slot allocated */
  static int cache_occupancy = 0;	/* current cache occupancy */
  static int cache_limit = BDF_FONT_CACHE_SIZE; /* allowed maximum occupancy */

  pch = get_cached_font_char(fontp, index);
  if (pch)
    {
      pcb = pch->pcbmp;
      if (pcb) return pcb;
    }

  bitmap_size = ((fontp->urx - fontp->llx) / 8 + 2) * (fontp->ury - fontp->lly)
    + 256;
  glyph.bitmap = (unsigned char*) alloca(sizeof(unsigned char) * bitmap_size);

  if (!w32_get_bdf_glyph(fontp, index, bitmap_size, &glyph))
    return NULL;

  pch = get_cached_font_char(fontp, index);
  if (!pch) return NULL;

  hbmp = CreateBitmap(glyph.metric.bbw, glyph.metric.bbh, 1, 1, glyph.bitmap);

  /* if bitmap allocation fails reduce the limit of the occupancy so
     that we can hope it will not happen again.  */
  if (hbmp == NULL)
    cache_limit = cache_occupancy * CACHE_GDI_ALLOWANCE;

  /* if cache occupancy reaches at the limit release some cache slots */
  if (cache_occupancy >= cache_limit)
    {
      register int size_to_clear = cache_limit * BDF_FONT_CLEAR_SIZE
                                   / BDF_FONT_CACHE_SIZE;
      for (; size_to_clear; size_to_clear--,
                            cache_out_slot = NEXT_CACHE_SLOT(cache_out_slot))
        {
          register cache_bitmap *p = &cached_bitmap_slots[cache_out_slot];
          if (p->psrc)
            {
              DeleteObject(p->hbmp);
              p->psrc->pcbmp = NULL;
              p->psrc = NULL;
              cache_occupancy--;
            }
        }
    }

  if (hbmp == NULL)
    hbmp = CreateBitmap (glyph.metric.bbw, glyph.metric.bbh,
                         1, 1, glyph.bitmap);

  pcb = &cached_bitmap_slots[cache_in_slot];

  pcb->psrc = pch;
  pcb->metric = glyph.metric;
  pcb->hbmp = hbmp;

  pch->pcbmp = pcb;
  
  cache_in_slot = NEXT_CACHE_SLOT(cache_in_slot);
  cache_occupancy++;

  return pcb;
}

int
w32_BDF_TextOut(bdffont *fontp, HDC hdc, int left,
		 int top, unsigned char *text, int dim, int bytelen,
		 int fixed_pitch_size)
{
  int index, btop;
  unsigned char *textp;
  HDC hCompatDC = 0;
  cache_bitmap *pcb;
  HBITMAP hBMP;
  HBRUSH hFgBrush, hOrgBrush;
  HANDLE horgobj = 0;
  UINT textalign;
  int flag = 0;

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
      pcb = get_bitmap_with_cache(fontp, index);
      if (!pcb)
	{
	  if (horgobj)
	    {
	      SelectObject(hCompatDC, horgobj);
	      DeleteObject(hBMP);
	    }
	  DeleteDC(hCompatDC);
	  return 0;
	}
      hBMP = pcb->hbmp;

      if (textalign & TA_BASELINE)
	  btop = top - (pcb->metric.bbh + pcb->metric.bboy);
      else if (textalign & TA_BOTTOM)
	  btop = top - pcb->metric.bbh;
      else
	  btop = top;

      if (horgobj)
	  SelectObject(hCompatDC, hBMP);
      else
	  horgobj = SelectObject(hCompatDC, hBMP);
#if 0
      BitBlt(hdc, left, btop, pcb->metric.bbw, pcb->metric.bbh, hCompatDC, 0, 0, SRCCOPY);
#else
      BitBlt(hdc, left, btop, pcb->metric.bbw, pcb->metric.bbh, hCompatDC, 0, 0, 0xB8074A);
#endif
      if (fixed_pitch_size)
	left += fixed_pitch_size;
      else
	left += pcb->metric.dwidth;
    }
  SelectObject(hCompatDC, horgobj);
  SelectObject(hdc, hOrgBrush);
  DeleteObject(hFgBrush);
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

  /* NTEMACS_TODO: Recognize DBCS fonts. */
  font->double_byte_p = 0;

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
