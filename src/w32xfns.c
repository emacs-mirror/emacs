/* Functions taken directly from X sources
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation.

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

#include <signal.h>
#include <config.h>
#include <stdio.h>
#include "lisp.h"
#include "frame.h"
#include "blockinput.h"
#include "w32term.h"
#include "windowsx.h"

#define myalloc(cb) GlobalAllocPtr (GPTR, cb)
#define myfree(lp) GlobalFreePtr (lp)

CRITICAL_SECTION critsect[ CRIT_TOTAL ];
extern HANDLE keyboard_handle;
HANDLE h_input_available = NULL;

void 
init_crit ()
{
  int i;

  for (i = 0; i < CRIT_TOTAL; i++)
    InitializeCriticalSection (&critsect[i]);
  /* For safety, h_input_available should only be reset by get_next_msg
     when the input queue is empty, so make it a manual reset event. */
  keyboard_handle = h_input_available = CreateEvent (NULL, TRUE, FALSE, NULL);
}

void 
delete_crit ()
{
  int i;

  for (i = 0; i < CRIT_TOTAL; i++)
    DeleteCriticalSection (&critsect[i]);
  if (h_input_available)
    {
      CloseHandle (h_input_available);
      h_input_available = NULL;
    }
}

/* Get a DC for frame and select palette for drawing; force an update of
   all frames if palette's mapping changes.  */
HDC
GetFrameDC (FRAME_PTR f)
{
  HDC hDC;

  enter_crit (CRIT_GDI);

  hDC = GetDC (f->output_data.win32->window_desc);

  if (!NILP (Vwin32_enable_palette))
    f->output_data.win32->h_old_palette =
      SelectPalette (hDC, one_win32_display_info.h_palette, FALSE);
  else
    f->output_data.win32->h_old_palette = NULL;

  if (RealizePalette (hDC))
    {
      Lisp_Object frame, framelist;
      FOR_EACH_FRAME (framelist, frame)
	{
	  SET_FRAME_GARBAGED (XFRAME (frame));
	}
    }

  return hDC;
}

int
ReleaseFrameDC (FRAME_PTR f, HDC hDC)
{
  int ret;

  if (f->output_data.win32->h_old_palette)
    SelectPalette (hDC, f->output_data.win32->h_old_palette, FALSE);

  ret = ReleaseDC (f->output_data.win32->window_desc, hDC);

  leave_crit (CRIT_GDI);

  return ret;
}

typedef struct int_msg
{
  Win32Msg w32msg;
  struct int_msg *lpNext;
} int_msg;

int_msg *lpHead = NULL;
int_msg *lpTail = NULL;
int nQueue = 0;

BOOL 
get_next_msg (lpmsg, bWait)
     Win32Msg * lpmsg;
     BOOL bWait;
{
  BOOL bRet = FALSE;
  
  enter_crit (CRIT_MSG);
  
  /* The while loop takes care of multiple sets */
  
  while (!nQueue && bWait)
    {
      leave_crit (CRIT_MSG);
      WaitForSingleObject (h_input_available, INFINITE);
      enter_crit (CRIT_MSG);
    }
  
  if (nQueue)
    {
      bcopy (&(lpHead->w32msg), lpmsg, sizeof (Win32Msg));

      {
	int_msg * lpCur = lpHead;
	    
	lpHead = lpHead->lpNext;
	    
	myfree (lpCur);
      }

      nQueue--;

      bRet = TRUE;
    }

  if (nQueue == 0)
    ResetEvent (h_input_available);
  
  leave_crit (CRIT_MSG);
  
  return (bRet);
}

BOOL 
post_msg (lpmsg)
     Win32Msg * lpmsg;
{
  int_msg * lpNew = (int_msg *) myalloc (sizeof (int_msg));

  if (!lpNew)
    return (FALSE);

  bcopy (lpmsg, &(lpNew->w32msg), sizeof (Win32Msg));
  lpNew->lpNext = NULL;

  enter_crit (CRIT_MSG);

  if (nQueue++)
    lpTail->lpNext = lpNew;
  else 
    lpHead = lpNew;

  lpTail = lpNew;
  SetEvent (h_input_available);
    
  leave_crit (CRIT_MSG);

  return (TRUE);
}

/*
 *    XParseGeometry parses strings of the form
 *   "=<width>x<height>{+-}<xoffset>{+-}<yoffset>", where
 *   width, height, xoffset, and yoffset are unsigned integers.
 *   Example:  "=80x24+300-49"
 *   The equal sign is optional.
 *   It returns a bitmask that indicates which of the four values
 *   were actually found in the string.  For each value found,
 *   the corresponding argument is updated;  for each value
 *   not found, the corresponding argument is left unchanged. 
 */

static int
read_integer (string, NextString)
     register char *string;
     char **NextString;
{
  register int Result = 0;
  int Sign = 1;
  
  if (*string == '+')
    string++;
  else if (*string == '-')
    {
      string++;
      Sign = -1;
    }
  for (; (*string >= '0') && (*string <= '9'); string++)
    {
      Result = (Result * 10) + (*string - '0');
    }
  *NextString = string;
  if (Sign >= 0)
    return (Result);
  else
    return (-Result);
}

int 
XParseGeometry (string, x, y, width, height)
     char *string;
     int *x, *y;
     unsigned int *width, *height;    /* RETURN */
{
  int mask = NoValue;
  register char *strind;
  unsigned int tempWidth, tempHeight;
  int tempX, tempY;
  char *nextCharacter;
  
  if ((string == NULL) || (*string == '\0')) return (mask);
  if (*string == '=')
    string++;  /* ignore possible '=' at beg of geometry spec */
  
  strind = (char *)string;
  if (*strind != '+' && *strind != '-' && *strind != 'x') 
    {
      tempWidth = read_integer (strind, &nextCharacter);
      if (strind == nextCharacter) 
	return (0);
      strind = nextCharacter;
      mask |= WidthValue;
    }
  
  if (*strind == 'x' || *strind == 'X') 
    {	
      strind++;
      tempHeight = read_integer (strind, &nextCharacter);
      if (strind == nextCharacter)
	return (0);
      strind = nextCharacter;
      mask |= HeightValue;
    }
  
  if ((*strind == '+') || (*strind == '-')) 
    {
      if (*strind == '-') 
	{
	  strind++;
	  tempX = -read_integer (strind, &nextCharacter);
	  if (strind == nextCharacter)
	    return (0);
	  strind = nextCharacter;
	  mask |= XNegative;

	}
      else
	{	
	  strind++;
	  tempX = read_integer (strind, &nextCharacter);
	  if (strind == nextCharacter)
	    return (0);
	  strind = nextCharacter;
	}
      mask |= XValue;
      if ((*strind == '+') || (*strind == '-')) 
	{
	  if (*strind == '-') 
	    {
	      strind++;
	      tempY = -read_integer (strind, &nextCharacter);
	      if (strind == nextCharacter)
		return (0);
	      strind = nextCharacter;
	      mask |= YNegative;

	    }
	  else
	    {
	      strind++;
	      tempY = read_integer (strind, &nextCharacter);
	      if (strind == nextCharacter)
		return (0);
	      strind = nextCharacter;
	    }
	  mask |= YValue;
	}
    }
  
  /* If strind isn't at the end of the string the it's an invalid
     geometry specification. */
  
  if (*strind != '\0') return (0);
  
  if (mask & XValue)
    *x = tempX;
  if (mask & YValue)
    *y = tempY;
  if (mask & WidthValue)
    *width = tempWidth;
  if (mask & HeightValue)
    *height = tempHeight;
  return (mask);
}

/* We can use mouse menus when we wish.  */
int
have_menus_p (void)
{
  return 1;
}

/* x_sync is a no-op on Win32.  */
void
x_sync (f)
     void *f;
{
}

