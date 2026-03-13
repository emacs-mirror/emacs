/* blockinput.h - interface to blocking complicated interrupt-driven input.
   Copyright (C) 1989, 1993, 2001-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_BLOCKINPUT_H
#define EMACS_BLOCKINPUT_H

INLINE_HEADER_BEGIN

/* Emacs should avoid doing anything hairy in a signal handler, because
   so many system functions are non-reentrant.  For example, malloc
   and the Xlib functions aren't usually re-entrant, so if they were
   used by the SIGIO handler, we'd lose.

   To avoid this, we make the following requirements:

   * Everyone must call block_input before performing actions that
   might conflict with a signal handler, and then call unblock_input
   after performing them.  Calls to block_input and unblock_input
   may be nested.

   * Any complicated interrupt handling code should test
   input_blocked_p, and put off its work until later.

   * If the interrupt handling code wishes, it may set
   pending_signals to a non-zero value.  If that flag is set
   when input becomes unblocked, unblock_input will then read
   input and process timers.

   Historically, Emacs signal handlers did much more than they do now,
   and this caused many block_input calls to be sprinkled around the code.
   FIXME: Remove calls that aren't needed now.  */

extern volatile int interrupt_input_blocked;

/* Begin critical section. */

INLINE void
block_input (void)
{
  interrupt_input_blocked++;
}

extern void unblock_input (void);
extern void totally_unblock_input (void);
extern void unblock_input_to (int);

/* In critical section?  */

INLINE bool
input_blocked_p (void)
{
  return interrupt_input_blocked > 0;
}

INLINE_HEADER_END

#endif /* EMACS_BLOCKINPUT_H */
