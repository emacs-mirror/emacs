/* Win32 Selection processing for emacs
   Copyright (C) 1993, 1994 Free Software Foundation.
   
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

/* Written by Kevin Gallo */

#include <config.h>
#include "lisp.h"
#include "w32term.h"	/* for all of the win32 includes */
#include "dispextern.h"	/* frame.h seems to want this */
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"

#if 0
DEFUN ("win32-open-clipboard", Fwin32_open_clipboard, Swin32_open_clipboard, 0, 1, 0,
       "This opens the clipboard with the given frame pointer.")
     (frame)
     Lisp_Object frame;
{
  BOOL ok = FALSE;
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 0);
  
  BLOCK_INPUT;
  
  ok = OpenClipboard ((!NILP (frame) && FRAME_WIN32_P (XFRAME (frame))) ? FRAME_WIN32_WINDOW (XFRAME (frame)) : NULL);
  
  UNBLOCK_INPUT;
  
  return (ok ? frame : Qnil);
}

DEFUN ("win32-empty-clipboard", Fwin32_empty_clipboard, Swin32_empty_clipboard, 0, 0, 0,
       "This empties the clipboard and assigns ownership to the window which opened the clipboard.")
     ()
{
  BOOL ok = FALSE;
  
  BLOCK_INPUT;
  
  ok = EmptyClipboard ();
  
  UNBLOCK_INPUT;
  
  return (ok ? Qt : Qnil);
}

DEFUN ("win32-close-clipboard", Fwin32_close_clipboard, Swin32_close_clipboard, 0, 0, 0,
       "This closes the clipboard.")
     ()
{
  BOOL ok = FALSE;
  
  BLOCK_INPUT;
  
  ok = CloseClipboard ();
  
  UNBLOCK_INPUT;
  
  return (ok ? Qt : Qnil);
}

#endif

DEFUN ("win32-set-clipboard-data", Fwin32_set_clipboard_data, Swin32_set_clipboard_data, 1, 2, 0,
       "This sets the clipboard data to the given text.")
    (string, frame)
    Lisp_Object string, frame;
{
  BOOL ok = TRUE;
  HANDLE htext;
  
  CHECK_STRING (string, 0);
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 0);
  
  BLOCK_INPUT;
  
  /* Allocate twice the amount so we can convert lf to cr-lf */
  
  if ((htext = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, (2 * XSTRING (string)->size) + 1)) == NULL)
    goto error;
  
  {
    unsigned char *lptext;
    
    if ((lptext = (unsigned char *)GlobalLock (htext)) == NULL)
      goto error;
	    
    {
      int i = XSTRING (string)->size;
      int newsize = XSTRING (string)->size;
      register char *p1 = XSTRING (string)->data;
      register char *p2 = lptext;
      
      while (i--)
	{
	  if (*p1 == '\n')
	    {
	      newsize++;
	      *p2++ = '\r';
	    }
	  
	  *p2++ = *p1++;
	}
      
      *p2 = 0;
    }
    
    GlobalUnlock (htext);
  }
  
  if (!OpenClipboard ((!NILP (frame) && FRAME_WIN32_P (XFRAME (frame))) ? FRAME_WIN32_WINDOW (XFRAME (frame)) : NULL))
    goto error;
  
  ok = EmptyClipboard () && SetClipboardData (CF_TEXT, htext);
  
  CloseClipboard ();
  
  if (ok) goto done;

 error:
  
  ok = FALSE;
  if (htext) GlobalFree (htext);
  
 done:
  UNBLOCK_INPUT;
  
  return (ok ? string : Qnil);
}

DEFUN ("win32-get-clipboard-data", Fwin32_get_clipboard_data, Swin32_get_clipboard_data, 0, 1, 0,
       "This gets the clipboard data in text format.")
     (frame)
     Lisp_Object frame;
{
  HANDLE htext;
  Lisp_Object ret = Qnil;
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 0);
  
  BLOCK_INPUT;
  
  if (!OpenClipboard ((!NILP (frame) && FRAME_WIN32_P (XFRAME (frame))) ? FRAME_WIN32_WINDOW (XFRAME (frame)) : NULL))
    goto done;
  
  if ((htext = GetClipboardData (CF_TEXT)) == NULL)
    goto closeclip;

	    
  {
    unsigned char *lptext;
    int nbytes;
    
    if ((lptext = (unsigned char *)GlobalLock (htext)) == NULL)
      goto closeclip;
    
    nbytes = strlen (lptext);
    
    {
      char *buf = (char *) xmalloc (nbytes);
      register char *p1 = lptext;
      register char *p2 = buf;
      int i = nbytes;
      
      if (buf == NULL) goto closeclip;
      
      while (i--)
	{
	  if (p1[0] == '\r' && i && p1[1] == '\n')
	    {
	      p1++;
	      i--;
	      nbytes--;
	    }

	  *p2++ = *p1++;
	}
      
      ret = make_string (buf, nbytes);
      
      xfree (buf);
    }
    
    GlobalUnlock (htext);
  }

 closeclip:
  CloseClipboard ();
  
 done:
  UNBLOCK_INPUT;
  
  return (ret);
}

void 
syms_of_win32select ()
{
#if 0
  defsubr (&Swin32_open_clipboard);
  defsubr (&Swin32_empty_clipboard);
  defsubr (&Swin32_close_clipboard);
#endif
  defsubr (&Swin32_set_clipboard_data);
  defsubr (&Swin32_get_clipboard_data);
}
