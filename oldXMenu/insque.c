/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.  */

/* This file implements the emacs_insque and emacs_remque functions,
   copies of the insque and remque functions of BSD.  They and all
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
