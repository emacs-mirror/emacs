/* POSIX compatible signal blocking for threads.
   Copyright (C) 2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

/* Specification.  */
#include <signal.h>

#include <errno.h>

int
pthread_sigmask (int how, const sigset_t *new_mask, sigset_t *old_mask)
{
  int ret = sigprocmask (how, new_mask, old_mask);
  return (ret < 0 ? errno : 0);
}
