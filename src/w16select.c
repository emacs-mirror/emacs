/* Win16 Selection processing for emacs on MS-Windows
   Copyright (C) 1996, 1997 Free Software Foundation.
   
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

/* These functions work by using WinOldAp interface.  WinOldAp
   (WINOLDAP.MOD) is a Microsoft Windows extension supporting
   "old" (character-mode) application access to Dynamic Data Exchange,
   menus, and the Windows clipboard.  */

/* Written by Dale P. Smith <dpsm@en.com>  */
/* Adapted to DJGPP v1 by Eli Zaretskii <eliz@is.elta.co.il>  */

#ifdef MSDOS

#include <config.h>
#include <string.h>
#include <dpmi.h>
#include <go32.h>
#include <sys/farptr.h>
#include "lisp.h"
#include "dispextern.h"	/* frame.h seems to want this */
#include "frame.h"	/* Need this to get the X window of selected_frame */
#include "blockinput.h"

/* If ever some function outside this file will need to call any
   clipboard-related function, the following prototypes and constants
   should be put on a header file.  Right now, nobody else uses them.  */

#define CF_TEXT      0x01
#define CF_BITMAP    0x02
#define CF_METAFILE  0x03
#define CF_SYLK	     0x04
#define CF_DIF	     0x05
#define CF_TIFF	     0x06
#define CF_OEMTEXT   0x07
#define CF_DIBBITMAP 0x08
#define CF_WINWRITE  0x80
#define CF_DSPTEXT   0x81
#define CF_DSPBITMAP 0x82

unsigned identify_winoldap_version (void);
unsigned open_clipboard (void);
unsigned empty_clipboard (void);
unsigned set_clipboard_data (unsigned, void *, unsigned);
unsigned get_clipboard_data_size (unsigned);
unsigned get_clipboard_data (unsigned, void *, unsigned);
unsigned close_clipboard (void);
unsigned clipboard_compact (unsigned);

Lisp_Object QCLIPBOARD, QPRIMARY;

/* The segment address and the size of the buffer in low
   memory used to move data between us and WinOldAp module.  */

static struct {
  unsigned long size;
  unsigned short rm_segment;
} clipboard_xfer_buf_info;

/* Emulation of `__dpmi_int' and friends for DJGPP v1.x  */

#if __DJGPP__ < 2

typedef _go32_dpmi_registers __dpmi_regs;
#define __tb      _go32_info_block.linear_address_of_transfer_buffer
#define _dos_ds	  _go32_info_block.selector_for_linear_memory

static int
__dpmi_int (intno, regs)
     int intno;
     __dpmi_regs *regs;
{
  regs->x.ss = regs->x.sp = regs->x.flags = 0;
  return _go32_dpmi_simulate_int (intno, regs);
}

#endif /* __DJGPP__ < 2 */

/* C functions to access the Windows 3.1x clipboard from DOS apps.

   The information was obtained from the Microsoft Knowledge Base,
   article Q67675 and can be found at:
   http://www.microsoft.com/kb/developr/win_dk/q67675.htm  */

/* See also Ralf Brown's Interrupt List.

   I also seem to remember reading about this in Dr. Dobbs Journal a
   while ago, but if you knew my memory...  :-)

   Dale P. Smith <dpsm@en.com> */

/* Return the WinOldAp support version, or 0x1700 if not supported.  */
unsigned
identify_winoldap_version ()
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1700h
     Return Values   AX == 1700H: Clipboard functions not available
                        <> 1700H: AL = Major version number
				  AH = Minor version number */
  regs.x.ax = 0x1700;
  __dpmi_int(0x2f, &regs);
  return regs.x.ax;
}

/* Open the clipboard, return non-zero if successfull.  */
unsigned
open_clipboard ()
{
  __dpmi_regs regs;

  /* Is WINOLDAP supported?  */
  /* Kludge alert!!  If WinOldAp is not supported, we return a 0,
     which is the same as ``Clipboard already open''.  Currently,
     this is taken as an error by all the functions that use
     `open_clipboard', but if somebody someday will use that ``open''
     clipboard, they will have interesting time debugging it...  */
  if (identify_winoldap_version () == 0x1700)
    return 0;

  /* Calls Int 2Fh/AX=1701h
     Return Values   AX == 0: Clipboard already open
			<> 0: Clipboard opened */
  regs.x.ax = 0x1701;
  __dpmi_int(0x2f, &regs);
  return regs.x.ax;
}

/* Empty clipboard, return non-zero if successfull.  */
unsigned
empty_clipboard ()
{
  __dpmi_regs regs;
  
  /* Calls Int 2Fh/AX=1702h
     Return Values   AX == 0: Error occurred
			<> 0: OK, Clipboard emptied */
  regs.x.ax = 0x1702;
  __dpmi_int(0x2f, &regs);
  return regs.x.ax;
}

/* Ensure we have a buffer in low memory with enough memory for data
   of size WANT_SIZE.  Return the linear address of the buffer.  */
static unsigned long
alloc_xfer_buf (want_size)
     unsigned want_size;
{
  __dpmi_regs regs;

  /* If the usual DJGPP transfer buffer is large enough, use that.  */
  if (want_size <= _go32_info_block.size_of_transfer_buffer)
    return __tb & 0xfffff;

  /* Need size rounded up to the nearest paragraph, and in
     paragraph units (1 paragraph = 16 bytes).  */
  clipboard_xfer_buf_info.size = (want_size + 15) >> 4;

  /* The NT DPMI host crashes us if we free DOS memory via the
     DPMI service.  Work around by calling DOS allocate/free block.  */
  regs.h.ah = 0x48;
  regs.x.bx = clipboard_xfer_buf_info.size;
  __dpmi_int (0x21, &regs);
  if (regs.x.flags & 1)
    {
      clipboard_xfer_buf_info.size = 0;
      return 0;
    }

  clipboard_xfer_buf_info.rm_segment = regs.x.ax;
  return (((int)clipboard_xfer_buf_info.rm_segment) << 4) & 0xfffff;
}

/* Free our clipboard buffer.  We always free it after use, because
   keeping it leaves less free conventional memory for subprocesses.
   The clipboard buffer tends to be large in size, because for small
   clipboard data sizes we use the DJGPP transfer buffer.  */
static void
free_xfer_buf ()
{
  /* If the size is 0, we used DJGPP transfer buffer, so don't free.  */
  if (clipboard_xfer_buf_info.size)
    {
      __dpmi_regs regs;

      /* The NT DPMI host crashes us if we free DOS memory via
	 the DPMI service.  Work around by calling DOS free block.  */
      regs.h.ah = 0x49;
      regs.x.es = clipboard_xfer_buf_info.rm_segment;
      __dpmi_int (0x21, &regs);
      clipboard_xfer_buf_info.size = 0;
    }
}

/* Copy data into the clipboard, return non-zero if successfull.  */
unsigned
set_clipboard_data (Format, Data, Size)
     unsigned Format;
     void *Data;
     unsigned Size;
{
  __dpmi_regs regs;
  unsigned truelen;
  unsigned long xbuf_addr, buf_offset;
  unsigned char *dp = Data, *dstart = dp;

  if (Format != CF_TEXT)
    return 0;

  /* need to know final size after '\r' chars are inserted (the
     standard CF_TEXT clipboard format uses CRLF line endings,
     while Emacs uses just LF internally).  */
  truelen = Size;
  /* avoid using strchr because it recomputes the length everytime */
  while ((dp = memchr (dp, '\n', Size - (dp - dstart))) != 0)
    {
      truelen++;
      dp++;
    }

  if (clipboard_compact (truelen) < truelen)
    return 0;

  if ((xbuf_addr = alloc_xfer_buf (truelen)) == 0)
    return 0;

  /* Move the buffer into the low memory, convert LF into CR-LF pairs.  */
  dp = Data;
  buf_offset = xbuf_addr;
  _farsetsel (_dos_ds);
  while (Size--)
    {
      if (*dp == '\n')
	_farnspokeb (buf_offset++, '\r');
      _farnspokeb (buf_offset++, *dp++);
    }

  /* Calls Int 2Fh/AX=1703h with:
	             DX = WinOldAp-Supported Clipboard format
                     ES:BX = Pointer to data
                     SI:CX = Size of data in bytes
     Return Values   AX == 0: Error occurred
			<> 0: OK.  Data copied into the Clipboard.  */
  regs.x.ax = 0x1703;
  regs.x.dx = Format;
  regs.x.si = truelen >> 16;
  regs.x.cx = truelen & 0xffff;
  regs.x.es = xbuf_addr >> 4;
  regs.x.bx = xbuf_addr & 15;
  __dpmi_int(0x2f, &regs);

  free_xfer_buf ();

  return regs.x.ax;
}

/* Return the size of the clipboard data of format FORMAT.  */
unsigned
get_clipboard_data_size (Format)
     unsigned Format;
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1704h with:
		     DX = WinOldAp-Supported Clipboard format
     Return Values   DX:AX == Size of the data in bytes, including any
                              headers.
                           == 0 If data in this format is not in
			   the clipboard.  */
  regs.x.ax = 0x1704;
  regs.x.dx = Format;
  __dpmi_int(0x2f, &regs);
  return ( (((unsigned)regs.x.dx) << 16) | regs.x.ax);
}

/* Get clipboard data, return its length.
   Warning: this doesn't check whether DATA has enough space to hold
   SIZE bytes.  */
unsigned
get_clipboard_data (Format, Data, Size)
     unsigned Format;
     void *Data;
     unsigned Size;
{
  __dpmi_regs regs;
  unsigned datalen = 0;
  unsigned long xbuf_addr;
  unsigned char *dp = Data;

  if (Format != CF_TEXT)
    return 0;

  if (Size == 0)
    return 0;

  if ((xbuf_addr = alloc_xfer_buf (Size)) == 0)
    return 0;

  /* Calls Int 2Fh/AX=1705h with:
		     DX = WinOldAp-Supported Clipboard format
		     ES:BX = Pointer to data buffer to hold data
     Return Values   AX == 0: Error occurred (or data in this format is not
                              in the clipboard)
                        <> 0: OK  */
  regs.x.ax = 0x1705;
  regs.x.dx = Format;
  regs.x.es = xbuf_addr >> 4;
  regs.x.bx = xbuf_addr & 15;
  __dpmi_int(0x2f, &regs);
  if (regs.x.ax != 0)
    {
      /* Copy data from low memory, remove CR characters if before LF.  */
      _farsetsel (_dos_ds);
      while (Size--)
	{
	  register unsigned char c = _farnspeekb (xbuf_addr++);

	  if ((*dp++ = c) == '\r' && _farnspeekb (xbuf_addr) == '\n')
	    {
	      dp--;
	      *dp++ = '\n';
	      xbuf_addr++;
	    }
	  /* Windows reportedly rounds up the size of clipboard data
	     (passed in SIZE) to a multiple of 32.  We therefore bail
	     out when we see the first null character.  */
	  else if (c == '\0')
	    {
	      datalen = dp - (unsigned char *)Data - 1;
	      break;
	    }
	}
    }

  free_xfer_buf ();

  return datalen;
}

/* Close clipboard, return non-zero if successfull.  */
unsigned
close_clipboard ()
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1708h
     Return Values   AX == 0: Error occurred
                        <> 0: OK */
  regs.x.ax = 0x1708;
  __dpmi_int(0x2f, &regs);
  return regs.x.ax;
}

/* Compact clipboard data so that at least SIZE bytes is available.  */
unsigned
clipboard_compact (Size)
     unsigned Size;
{
  __dpmi_regs regs;

  /* Calls Int 2Fh/AX=1709H with:
                     SI:CX = Desired memory size in bytes.
     Return Values   DX:AX == Number of bytes of largest block of free memory.
                           == 0 if error or no memory  */
  regs.x.ax = 0x1709;
  regs.x.si = Size >> 16;
  regs.x.cx = Size & 0xffff;
  __dpmi_int(0x2f, &regs);
  return ((unsigned)regs.x.dx << 16) | regs.x.ax;
}

static char no_mem_msg[] =
  "(Not enough DOS memory to put saved text into clipboard.)";

DEFUN ("win16-set-clipboard-data", Fwin16_set_clipboard_data, Swin16_set_clipboard_data, 1, 2, 0,
       "This sets the clipboard data to the given text.")
    (string, frame)
    Lisp_Object string, frame;
{
  int ok = 1, ok1 = 1;
  int nbytes;
  unsigned char *src;

  CHECK_STRING (string, 0);
  
  if (NILP (frame))
    frame = Fselected_frame ();

  CHECK_LIVE_FRAME (frame, 0);
  if ( !FRAME_MSDOS_P (XFRAME (frame)))
    goto done;
  
  BLOCK_INPUT;

  nbytes = XSTRING (string)->size + 1;
  src = XSTRING (string)->data;
    
  if (!open_clipboard ())
    goto error;
  
  ok = empty_clipboard () && (ok1 = set_clipboard_data (CF_TEXT, src, nbytes));
  
  close_clipboard ();
  
  if (ok) goto done;

 error:
  
  ok = 0;

  /* Notify user if the text is too large to fit into DOS memory.
     (This will happen somewhere after 600K bytes (470K in DJGPP v1.x),
     depending on user system configuration.)  If we just silently
     fail the function, people might wonder why their text sometimes
     doesn't make it to the clipboard.  */
  if (ok1 == 0)
    {
      message2 (no_mem_msg, sizeof (no_mem_msg) - 1);
      sit_for (2, 0, 0, 1, 1);
    }
  
 done:
  UNBLOCK_INPUT;

  return (ok ? string : Qnil);
}

DEFUN ("win16-get-clipboard-data", Fwin16_get_clipboard_data, Swin16_get_clipboard_data, 0, 1, 0,
       "This gets the clipboard data in text format.")
     (frame)
     Lisp_Object frame;
{
  unsigned data_size, truelen;
  unsigned char *htext;
  Lisp_Object ret = Qnil;
  
  if (!NILP (frame))
    CHECK_LIVE_FRAME (frame, 0);
  
  if (NILP (frame))
    frame = Fselected_frame ();

  CHECK_LIVE_FRAME (frame, 0);
  if ( !FRAME_MSDOS_P (XFRAME (frame)))
    goto done;
  
  BLOCK_INPUT;
  
  if (!open_clipboard ())
    goto done;

  if ((data_size = get_clipboard_data_size (CF_TEXT)) == 0 ||
      (htext = (unsigned char *)xmalloc (data_size)) == 0)
    goto closeclip;

  /* need to know final size after '\r' chars are removed because
     we can't change the string size manually, and doing an extra
     copy is silly */
  if ((truelen = get_clipboard_data (CF_TEXT, htext, data_size)) == 0)
    goto closeclip;

  ret = make_string (htext, truelen);
  xfree (htext);

 closeclip:
  close_clipboard ();
  
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

  /* Return nil for SECONDARY selection.  For PRIMARY (or nil)
     selection, check if there is some text on the kill-ring;
     for CLIPBOARD, check if the clipboard currently has valid
     text format contents.

     The test for killed text on the kill-ring emulates the Emacs
     behavior on X, where killed text is also put into X selection
     by the X interface code.  (On MSDOS, killed text is only put
     into the clipboard if we run under Windows, so we cannot check
     the clipboard alone.)  */
  if ((EQ (selection, Qnil) || EQ (selection, QPRIMARY))
      && ! NILP (XSYMBOL (Fintern_soft (build_string ("kill-ring"),
					Qnil))->value))
    return Qt;

  if (EQ (selection, QCLIPBOARD))
    {
      Lisp_Object val = Qnil;

      if (open_clipboard ())
	{
	  if (get_clipboard_data_size (CF_TEXT))
	    val = Qt;
	  close_clipboard ();
	}
      return val;
    }
  return Qnil;
}

void 
syms_of_win16select ()
{
  defsubr (&Swin16_set_clipboard_data);
  defsubr (&Swin16_get_clipboard_data);
  defsubr (&Sx_selection_exists_p);

  QPRIMARY   = intern ("PRIMARY");	staticpro (&QPRIMARY);
  QCLIPBOARD = intern ("CLIPBOARD");	staticpro (&QCLIPBOARD);
}

#endif /* MSDOS */
