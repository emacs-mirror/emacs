/* blockinput.h - interface to blocking complicated interrupt-driven input.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* When Emacs is using signal-driven input, the processing of those
   input signals can get pretty hairy.  For example, when Emacs is
   running under X windows, handling an input signal can entail
   retrieving events from the X event queue, or making other X calls.

   If an input signal occurs while Emacs is in the midst of some
   non-reentrant code, and the signal processing invokes that same
   code, we lose.  For example, malloc and the Xlib functions aren't
   usually re-entrant, and both are used by the X input signal handler
   - if we try to process an input signal in the midst of executing
   any of these functions, we'll lose.

   To avoid this, we make the following requirements:

   * Everyone must evaluate BLOCK_INPUT before entering these functions,
   and then call UNBLOCK_INPUT after performing them.  Calls
   BLOCK_INPUT and UNBLOCK_INPUT may be nested.

   * Any complicated interrupt handling code should test
   interrupt_input_blocked, and put off its work until later.  

   * If the interrupt handling code wishes, it may set
   interrupt_input_pending to a non-zero value.  If that flag is set
   when input becomes unblocked, UNBLOCK_INPUT will send a new SIGIO.  */

extern unsigned int interrupt_input_blocked;

/* Nonzero means an input interrupt has arrived
   during the current critical section.  */
extern int interrupt_input_pending;

/* Begin critical section. */
#define BLOCK_INPUT (interrupt_input_blocked++)

/* End critical section. */
#ifdef SIGIO
/* If doing interrupt input, and an interrupt came in when input was blocked,
   reinvoke the interrupt handler now to deal with it.  */
#define UNBLOCK_INPUT \
  (interrupt_input_blocked--, \
   (interrupt_input_blocked < 0 ? (abort (), 0) : 0), \
   ((interrupt_input_blocked == 0 && interrupt_input_pending != 0) \
    ? (kill (0, SIGIO), 0) \
    : 0))
#else
#define UNBLOCK_INPUT \
  (interrupt_input_blocked--, \
   (interrupt_input_blocked < 0 ? (abort (), 0) : 0))
#endif

#define TOTALLY_UNBLOCK_INPUT (interrupt_input_blocked = 0)
#define UNBLOCK_INPUT_RESIGNAL UNBLOCK_INPUT
