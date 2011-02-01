/* Machine description file for IBM S390 in 32-bit mode

Copyright (C) 1985-1986, 2001-2011 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */
#define VIRT_ADDR_VARIES

