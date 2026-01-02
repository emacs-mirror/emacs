/* Program execution for Emacs.

Copyright (C) 2023-2026 Free Software Foundation, Inc.

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



#ifndef _MIPSEL_USER_H_
#define _MIPSEL_USER_H_

#include <sys/user.h>



/* This file defines a structure containing user mode general purpose
   registers on 32-bit mipsel systems.  */

struct mipsel_regs
{
  /* General purpose registers.  */
  uint64_t gregs[32];

  /* Saved special registers. */
  uint64_t lo;
  uint64_t hi;
  uint64_t cp0_epc;
  uint64_t cp0_badvaddr;
  uint64_t cp0_status;
  uint64_t cp0_cause;
};

#endif /* _MIPSEL_USER_H_ */
