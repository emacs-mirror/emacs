/* Selection processing for Emacs on the Microsoft W32 API.
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
#include "w32term.h"	/* for all of the w32 includes */
#include "dispextern.h"	/* frame.h seems to want this */
#include "keyboard.h"
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"
#include "buffer.h"
#include "charset.h"
#include "coding.h"

Lisp_Object QCLIPBOARD;

/* Coding system for communicating with other Windows programs via the
   clipboard.  */
static Lisp_Object Vselection_coding_system;

/* Coding system for the next communicating with other Windows programs.  */
static Lisp_Object Vnext_selection_coding_system;

/* The last text we put into the clipboard.  This is used to prevent
   passing back our own text from the clipboard, instead of using the
   kill ring.  The former is undesirable because the clipboard data
   could be MULEtilated by inappropriately chosen
   (next-)selection-coding-system.  For this reason, we must store the
   text *after* it was encoded/Unix-to-DOS-converted.  */
static unsigned char *last_clipboard_text = NULL;
static size_t clipboard_storage_size = 0;

#if 0
DEFUN ("w32-open-clipboard", Fw32_open_clipboard, Sw32_open_clipboard, 0, 1, 0,
       "This opens the clipboard with the given frame pointer.")
     (frame)
     Lisp_Object frame;
{
  BOOL ok = FALSE;
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);
  
  BLOCK_INPUT;
  
  ok = OpenClipboard ((!NILP (frame) && FRAME_W32_P (XFRAME (frame))) ? FRAME_W32_WINDOW (XFRAME (frame)) : NULL);
  
  UNBLOCK_INPUT;
  
  return (ok ? frame : Qnil);
}

DEFUN ("w32-empty-clipboard", Fw32_empty_clipboard, Sw32_empty_clipboard, 0, 0, 0,
       "This empties the clipboard and assigns ownership to the window which opened the clipboard.")
     ()
{
  BOOL ok = FALSE;
  
  BLOCK_INPUT;
  
  ok = EmptyClipboard ();
  
  UNBLOCK_INPUT;
  
  return (ok ? Qt : Qnil);
}

DEFUN ("w32-close-clipboard", Fw32_close_clipboard, Sw32_close_clipboard, 0, 0, 0,
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

DEFUN ("w32-set-clipboard-data", Fw32_set_clipboard_data, Sw32_set_clipboard_data, 1, 2, 0,
       "This sets the clipboard data to the given text.")
    (string, frame)
    Lisp_Object string, frame;
{
  BOOL ok = TRUE;
  HANDLE htext;
  int nbytes;
  int truelen, nlines = 0;
  unsigned char *src;
  unsigned char *dst;

  CHECK_STRING (string);
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);
  
  BLOCK_INPUT;

  nbytes = STRING_BYTES (XSTRING (string)) + 1;
  src = XSTRING (string)->data;
  dst = src;

  /* We need to know how many lines there are, since we need CRLF line
     termination for compatibility with other Windows Programs.
     avoid using strchr because it recomputes the length every time */
  while ((dst = memchr (dst, '\n', nbytes - (dst - src))) != NULL)
    {
      nlines++;
      dst++;
    }

  {
    /* Since we are now handling multilingual text, we must consider
       encoding text for the clipboard.  */
    int charset_info = find_charset_in_text (src, XSTRING (string)->size,
					     nbytes, NULL, Qnil);

    if (charset_info == 0)
      {
	/* No multibyte character in OBJ.  We need not encode it.  */

	/* Need to know final size after CR chars are inserted (the
	   standard CF_TEXT clipboard format uses CRLF line endings,
	   while Emacs uses just LF internally).  */

	truelen = nbytes + nlines;

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

	Vlast_coding_system_used = Qraw_text;
      }
    else
      {
	/* We must encode contents of OBJ to the selection coding
           system. */
	int bufsize;
	struct coding_system coding;
	HANDLE htext2;

	if (NILP (Vnext_selection_coding_system))
	  Vnext_selection_coding_system = Vselection_coding_system;
	setup_coding_system
	  (Fcheck_coding_system (Vnext_selection_coding_system), &coding);
	coding.src_multibyte = 1;
	coding.dst_multibyte = 0;
	Vnext_selection_coding_system = Qnil;
	coding.mode |= CODING_MODE_LAST_BLOCK;
	bufsize = encoding_buffer_size (&coding, nbytes);
	if ((htext = GlobalAlloc (GMEM_MOVEABLE | GMEM_DDESHARE, bufsize)) == NULL)
	  goto error;
	if ((dst = (unsigned char *) GlobalLock (htext)) == NULL)
	  goto error;
	encode_coding (&coding, src, dst, nbytes, bufsize);
	Vlast_coding_system_used = coding.symbol;

        /* Stash away the data we are about to put into the clipboard, so we
           could later check inside Fw32_get_clipboard_data whether
           the clipboard still holds our data.  */
        if (clipboard_storage_size < coding.produced)
          {
            clipboard_storage_size = coding.produced + 100;
            last_clipboard_text = (char *) xrealloc (last_clipboard_text,
                                                     clipboard_storage_size);
          }
        if (last_clipboard_text)
          memcpy (last_clipboard_text, dst, coding.produced);

	GlobalUnlock (htext);

	/* Shrink data block to actual size.  */
	htext2 = GlobalReAlloc (htext, coding.produced,
                                GMEM_MOVEABLE | GMEM_DDESHARE);
	if (htext2 != NULL) htext = htext2;
      }
  }
  
  if (!OpenClipboard ((!NILP (frame) && FRAME_W32_P (XFRAME (frame))) ? FRAME_W32_WINDOW (XFRAME (frame)) : NULL))
    goto error;

  ok = EmptyClipboard () && SetClipboardData (CF_TEXT, htext);
  
  CloseClipboard ();
  
  if (ok) goto done;

 error:
  
  ok = FALSE;
  if (htext) GlobalFree (htext);
  if (last_clipboard_text)
    *last_clipboard_text = '\0';

 done:
  UNBLOCK_INPUT;
  
  return (ok ? string : Qnil);
}

DEFUN ("w32-get-clipboard-data", Fw32_get_clipboard_data, Sw32_get_clipboard_data, 0, 1, 0,
       "This gets the clipboard data in text format.")
     (frame)
     Lisp_Object frame;
{
  HANDLE htext;
  Lisp_Object ret = Qnil;
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame);
  
  BLOCK_INPUT;
  
  if (!OpenClipboard ((!NILP (frame) && FRAME_W32_P (XFRAME (frame))) ? FRAME_W32_WINDOW (XFRAME (frame)) : NULL))
    goto done;
  
  if ((htext = GetClipboardData (CF_TEXT)) == NULL)
    goto closeclip;

  {
    unsigned char *src;
    unsigned char *dst;
    int nbytes;
    int truelen;
    int require_decoding = 0;
    
    if ((src = (unsigned char *) GlobalLock (htext)) == NULL)
      goto closeclip;
    
    nbytes = strlen (src);

    /* If the text in clipboard is identical to what we put there
       last time w32_set_clipboard_data was called, pretend there's no
       data in the clipboard.  This is so we don't pass our own text
       from the clipboard (which might be troublesome if the killed
       text includes null characters).  */
    if (last_clipboard_text
        && clipboard_storage_size >= nbytes
        && memcmp(last_clipboard_text, src, nbytes) == 0)
      goto closeclip;

    if (
#if 1
	1
#else
	! NILP (buffer_defaults.enable_multibyte_characters)
#endif
	)
      {
	/* If the clipboard data contains any non-ascii code, we
	   need to decode it.  */
	int i;

	for (i = 0; i < nbytes; i++)
	  {
	    if (src[i] >= 0x80)
	      {
		require_decoding = 1;
		break;
	      }
	  }
      }
    
    if (require_decoding)
      {
	int bufsize;
	unsigned char *buf;
	struct coding_system coding;

	if (NILP (Vnext_selection_coding_system))
	  Vnext_selection_coding_system = Vselection_coding_system;
	setup_coding_system
	  (Fcheck_coding_system (Vnext_selection_coding_system), &coding);
	coding.src_multibyte = 0;
	coding.dst_multibyte = 1;
	Vnext_selection_coding_system = Qnil;
	coding.mode |= CODING_MODE_LAST_BLOCK;
	bufsize = decoding_buffer_size (&coding, nbytes);
	buf = (unsigned char *) xmalloc (bufsize);
	decode_coding (&coding, src, buf, nbytes, bufsize);
	Vlast_coding_system_used = coding.symbol;
        ret = make_string_from_bytes ((char *) buf,
                                      coding.produced_char, coding.produced);
	xfree (buf);
      }
    else
      {
	/* Need to know final size after CR chars are removed because we
	   can't change the string size manually, and doing an extra
	   copy is silly.  Note that we only remove CR when it appears
	   as part of CRLF.  */

	truelen = nbytes;
	dst = src;
	/* avoid using strchr because it recomputes the length everytime */
	while ((dst = memchr (dst, '\r', nbytes - (dst - src))) != NULL)
	  {
	    if (dst[1] == '\n')	/* safe because of trailing '\0' */
	      truelen--;
	    dst++;
	  }

	ret = make_uninit_string (truelen);

	/* Convert CRLF line endings (the standard CF_TEXT clipboard
	   format) to LF endings as used internally by Emacs.  */

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
		dst += copied;
		src += copied;
		if (*src == '\n')
		  dst--;	/* overwrite '\r' with '\n' */
	      }
	    else
	      /* copied remaining partial line -> now finished */
	      break;
	  }

	Vlast_coding_system_used = Qraw_text;
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
  CHECK_SYMBOL (selection);

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
syms_of_w32select ()
{
#if 0
  defsubr (&Sw32_open_clipboard);
  defsubr (&Sw32_empty_clipboard);
  defsubr (&Sw32_close_clipboard);
#endif
  defsubr (&Sw32_set_clipboard_data);
  defsubr (&Sw32_get_clipboard_data);
  defsubr (&Sx_selection_exists_p);

  DEFVAR_LISP ("selection-coding-system", &Vselection_coding_system,
    "Coding system for communicating with other X clients.\n\
When sending or receiving text via cut_buffer, selection, and clipboard,\n\
the text is encoded or decoded by this coding system.\n\
A default value is `compound-text'");
  Vselection_coding_system=intern ("iso-latin-1-dos");

  DEFVAR_LISP ("next-selection-coding-system", &Vnext_selection_coding_system,
    "Coding system for the next communication with other X clients.\n\
Usually, `selection-coding-system' is used for communicating with\n\
other X clients.   But, if this variable is set, it is used for the\n\
next communication only.   After the communication, this variable is\n\
set to nil.");
  Vnext_selection_coding_system = Qnil;

  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
}
