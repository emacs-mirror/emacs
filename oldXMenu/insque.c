/* This file implements the insque and remque functions of BSD.
   It is not compiled by default, because that change would be too risky
   to install right now.  If you find that HAVE_X_MENU leads to linker errors
   because these functions are undefined, then compile this file
   and arrange to link it in.  */

struct qelem {
  struct    qelem *q_forw;
  struct    qelem *q_back;
  char q_data[1];
};

/* Insert ELEM into a doubly-linked list, after PREV.  */

void
insque (elem, prev) 
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

remque (elem)
     struct qelem *elem;
{
  struct qelem *next = elem->q_forw;
  struct qelem *prev = elem->q_back;
  if (next)
    next->q_back = prev;
  if (prev)
    prev->q_forw = next;
}
