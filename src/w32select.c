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

Lisp_Object QCLIPBOARD;

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
  int nbytes;
  int truelen;
  unsigned char *src;
  unsigned char *dst;
  
  CHECK_STRING (string, 0);
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 0);
  
  BLOCK_INPUT;

  nbytes = XSTRING (string)->size + 1;
  src = XSTRING (string)->data;

  /* need to know final size after '\r' chars are inserted (the
     standard CF_TEXT clipboard format uses CRLF line endings,
     while Emacs uses just LF internally) */

  truelen = nbytes;
  dst = src;
  /* avoid using strchr because it recomputes the length everytime */
  while ((dst = memchr (dst, '\n', nbytes - (dst - src))) != NULL)
    {
      truelen++;
      dst++;
    }

  if ((htext = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, truelen)) == NULL)
    goto error;

  if ((dst = (unsigned char *) GlobalLock (htext)) == NULL)
    goto error;
    
  /* convert to CRLF line endings expected by clipboard */
  while (1)
    {
      unsigned char *next;
      /* copy next line or remaining bytes including '\0' */
      next = _memccpy (dst, src, '\n', nbytes);
      if (next)
	{
	  /* copied one line ending with '\n' */
	  int copied = next - dst;
	  nbytes -= copied;
	  src += copied;
	  /* insert '\r' before '\n' */
	  next[-1] = '\r';
	  next[0] = '\n';
	  dst = next + 1;
	}	    
      else
	/* copied remaining partial line -> now finished */
	break;
    }
    
  GlobalUnlock (htext);
  
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
    unsigned char *src;
    unsigned char *dst;
    int nbytes;
    int truelen;
    
    if ((src = (unsigned char *) GlobalLock (htext)) == NULL)
      goto closeclip;
    
    nbytes = strlen (src);

    /* need to know final size after '\r' chars are removed because
       we can't change the string size manually, and doing an extra
       copy is silly */

    truelen = nbytes;
    dst = src;
    /* avoid using strchr because it recomputes the length everytime */
    while ((dst = memchr (dst, '\r', nbytes - (dst - src))) != NULL)
      {
	truelen--;
	dst++;
      }

    ret = make_uninit_string (truelen);

    /* convert CRLF line endings (the standard CF_TEXT clipboard
       format) to LF endings as used internally by Emacs */

    dst = XSTRING (ret)->data;
    while (1)
      {
	unsigned char *next;
	/* copy next line or remaining bytes excluding '\0' */
	next = _memccpy (dst, src, '\r', nbytes);
	if (next)
	  {
	    /* copied one line ending with '\r' */
	    int copied = next - dst;
	    nbytes -= copied;
	    dst += copied - 1;		/* overwrite '\r' */
	    src += copied;
	  }	    
	else
	  /* copied remaining partial line -> now finished */
	  break;
      }

    GlobalUnlock (htext);
  }

 closeclip:
  CloseClipboard ();
  
 done:
  UNBLOCK_INPUT;
  
  return (ret);
}

/* Support checking for a clipboard selection. */

DEFUN ("x-selection-exists-p", Fx_selection_exists_p, Sx_selection_exists_p,
  0, 1, 0,
  "Whether there is an owner for the given X Selection.\n\
The arg should be the name of the selection in question, typically one of\n\
the symbols `PRIMARY', `SECONDARY', or `CLIPBOARD'.\n\
\(Those are literal upper-case symbol names, since that's what X expects.)\n\
For convenience, the symbol nil is the same as `PRIMARY',\n\
and t is the same as `SECONDARY'.")
  (selection)
     Lisp_Object selection;
{
  CHECK_SYMBOL (selection, 0);

  /* Return nil for PRIMARY and SECONDARY selections; for CLIPBOARD, check
     if the clipboard currently has valid text format contents. */

  if (EQ (selection, QCLIPBOARD))
    {
      Lisp_Object val = Qnil;

      if (OpenClipboard (NULL))
	{
	  int format = 0;
	  while (format = EnumClipboardFormats (format))
	    if (format == CF_TEXT)
	      {
		val = Qt;
		break;
	      }
	  CloseClipboard ();
	}
      return val;
    }
  return Qnil;
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
  defsubr (&Sx_selection_exists_p);

  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
}
