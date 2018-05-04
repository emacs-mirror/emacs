/* The user ID that always has appropriate privileges in the POSIX sense.

   Copyright 2012-2018 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Written by Paul Eggert.  */

#ifndef ROOT_UID_H_
#define ROOT_UID_H_

/* The user ID that always has appropriate privileges in the POSIX sense.  */
#ifdef __TANDEM
# define ROOT_UID 65535
#else
# define ROOT_UID 0
#endif

#endif
