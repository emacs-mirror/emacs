/* Add two struct timespec values.

   Copyright (C) 2011-2025 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert.  */

/* Return the sum of two timespec values A and B.  On overflow, return
   an extremal value.  This assumes 0 <= tv_nsec < TIMESPEC_HZ.  */

#include <config.h>
#include "timespec.h"

#include <stdckdint.h>
#include "intprops.h"

struct timespec
timespec_add (struct timespec a, struct timespec b)
{
  int nssum = a.tv_nsec + b.tv_nsec;
  int carry = TIMESPEC_HZ <= nssum;
  time_t rs;
  int rns;
  bool v = ckd_add (&rs, a.tv_sec, b.tv_sec);
  if (v == ckd_add (&rs, rs, carry))
    rns = nssum - TIMESPEC_HZ * carry;
  else
    {
      if ((TYPE_MINIMUM (time_t) + TYPE_MAXIMUM (time_t)) / 2 < rs)
        {
          rs = TYPE_MINIMUM (time_t);
          rns = 0;
        }
      else
        {
          rs = TYPE_MAXIMUM (time_t);
          rns = TIMESPEC_HZ - 1;
        }
    }

  return make_timespec (rs, rns);
}
