/* Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2001, 2002, 2003,
                 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* This file implements the emacs_insque and emacs_remque functions,
   clones of the insque and remque functions of BSD.  They and all
   their callers have been renamed to emacs_mumble to allow us to
   include this file in the menu library on all systems.  */


struct qelem {
  struct    qelem *q_forw;
  struct    qelem *q_back;
  char q_data[1];
};

/* Insert ELEM into a doubly-linked list, after PREV.  */

void
emacs_insque (elem, prev)
     struct qelem *elem, *prev;
{
  struct qelem *next = prev->q_forw;
  prev->q_forw = elem;
  if (next)
    next->q_back = elem;
  elem->q_forw = next;
  elem->q_back = prev;
}

/* Unlink ELEM from the doubly-linked list that it is in.  */

emacs_remque (elem)
     struct qelem *elem;
{
  struct qelem *next = elem->q_forw;
  struct qelem *prev = elem->q_back;
  if (next)
    next->q_back = prev;
  if (prev)
    prev->q_forw = next;
}

/* arch-tag: a8719d1a-5c3f-4bce-b36b-173106d36165
   (do not change this comment) */
