/* Random utility Lisp functions.
   Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

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


#include <config.h>

/* Note on some machines this defines `vector' as a typedef,
   so make sure we don't use that name in this file.  */
#undef vector
#define vector *****

#include "lisp.h"
#include "commands.h"

#include "buffer.h"
#include "keyboard.h"
#include "intervals.h"

extern Lisp_Object Flookup_key ();

Lisp_Object Qstring_lessp, Qprovide, Qrequire;
Lisp_Object Qyes_or_no_p_history;

static int internal_equal ();

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
  "Return the argument unchanged.")
  (arg)
     Lisp_Object arg;
{
  return arg;
}

extern long get_random ();
extern void seed_random ();
extern long time ();

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
  "Return a pseudo-random number.\n\
All integers representable in Lisp are equally likely.\n\
  On most systems, this is 28 bits' worth.\n\
With positive integer argument N, return random number in interval [0,N).\n\
With argument t, set the random number seed from the current time and pid.")
  (limit)
     Lisp_Object limit;
{
  int val;
  unsigned long denominator;

  if (EQ (limit, Qt))
    seed_random (getpid () + time (0));
  if (NATNUMP (limit) && XFASTINT (limit) != 0)
    {
      /* Try to take our random number from the higher bits of VAL,
	 not the lower, since (says Gentzel) the low bits of `random'
	 are less random than the higher ones.  We do this by using the
	 quotient rather than the remainder.  At the high end of the RNG
	 it's possible to get a quotient larger than limit; discarding
	 these values eliminates the bias that would otherwise appear
	 when using a large limit.  */
      denominator = ((unsigned long)1 << VALBITS) / XFASTINT (limit);
      do
	val = get_random () / denominator;
      while (val >= XFASTINT (limit));
    }
  else
    val = get_random ();
  return make_number (val);
}

/* Random data-structure functions */

DEFUN ("length", Flength, Slength, 1, 1, 0,
  "Return the length of vector, list or string SEQUENCE.\n\
A byte-code function object is also allowed.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object tail, val;
  register int i;

 retry:
  if (STRINGP (obj))
    XSETFASTINT (val, XSTRING (obj)->size);
  else if (VECTORP (obj))
    XSETFASTINT (val, XVECTOR (obj)->size);
  else if (COMPILEDP (obj))
    XSETFASTINT (val, XVECTOR (obj)->size & PSEUDOVECTOR_SIZE_MASK);
  else if (CONSP (obj))
    {
      for (i = 0, tail = obj; !NILP (tail); i++)
	{
	  QUIT;
	  tail = Fcdr (tail);
	}

      XSETFASTINT (val, i);
    }
  else if (NILP (obj))
    XSETFASTINT (val, 0);
  else
    {
      obj = wrong_type_argument (Qsequencep, obj);
      goto retry;
    }
  return val;
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
  "T if two strings have identical contents.\n\
Case is significant, but text properties are ignored.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  if (SYMBOLP (s1))
    XSETSTRING (s1, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSETSTRING (s2, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  if (XSTRING (s1)->size != XSTRING (s2)->size ||
      bcmp (XSTRING (s1)->data, XSTRING (s2)->data, XSTRING (s1)->size))
    return Qnil;
  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
  "T if first arg string is less than second in lexicographic order.\n\
Case is significant.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  register int i;
  register unsigned char *p1, *p2;
  register int end;

  if (SYMBOLP (s1))
    XSETSTRING (s1, XSYMBOL (s1)->name);
  if (SYMBOLP (s2))
    XSETSTRING (s2, XSYMBOL (s2)->name);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  p1 = XSTRING (s1)->data;
  p2 = XSTRING (s2)->data;
  end = XSTRING (s1)->size;
  if (end > XSTRING (s2)->size)
    end = XSTRING (s2)->size;

  for (i = 0; i < end; i++)
    {
      if (p1[i] != p2[i])
	return p1[i] < p2[i] ? Qt : Qnil;
    }
  return i < XSTRING (s2)->size ? Qt : Qnil;
}

static Lisp_Object concat ();

/* ARGSUSED */
Lisp_Object
concat2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return concat (2, args, Lisp_String, 0);
#else
  return concat (2, &s1, Lisp_String, 0);
#endif /* NO_ARG_ARRAY */
}

/* ARGSUSED */
Lisp_Object
concat3 (s1, s2, s3)
     Lisp_Object s1, s2, s3;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[3];
  args[0] = s1;
  args[1] = s2;
  args[2] = s3;
  return concat (3, args, Lisp_String, 0);
#else
  return concat (3, &s1, Lisp_String, 0);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
  "Concatenate all the arguments and make the result a list.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.\n\
The last argument is not copied, just used as the tail of the new list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Cons, 1);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
  "Concatenate all the arguments and make the result a string.\n\
The result is a string whose elements are the elements of all the arguments.\n\
Each argument may be a string, a character (integer), or a list or vector\n\
of characters (integers).")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_String, 0);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
  "Concatenate all the arguments and make the result a vector.\n\
The result is a vector whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Vectorlike, 0);
}

DEFUN ("copy-sequence", Fcopy_sequence, Scopy_sequence, 1, 1, 0,
  "Return a copy of a list, vector or string.\n\
The elements of a list or vector are not copied; they are shared\n\
with the original.")
  (arg)
     Lisp_Object arg;
{
  if (NILP (arg)) return arg;
  if (!CONSP (arg) && !VECTORP (arg) && !STRINGP (arg))
    arg = wrong_type_argument (Qsequencep, arg);
  return concat (1, &arg, CONSP (arg) ? Lisp_Cons : XTYPE (arg), 0);
}

static Lisp_Object
concat (nargs, args, target_type, last_special)
     int nargs;
     Lisp_Object *args;
     enum Lisp_Type target_type;
     int last_special;
{
  Lisp_Object val;
  Lisp_Object len;
  register Lisp_Object tail;
  register Lisp_Object this;
  int toindex;
  register int leni;
  register int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;

  /* In append, the last arg isn't treated like the others */
  if (last_special && nargs > 0)
    {
      nargs--;
      last_tail = args[nargs];
    }
  else
    last_tail = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      if (!(CONSP (this) || NILP (this) || VECTORP (this) || STRINGP (this)
	    || COMPILEDP (this)))
	{
	  if (INTEGERP (this))
	    args[argnum] = Fcons (this, Qnil);
	  else
	    args[argnum] = wrong_type_argument (Qsequencep, this);
	}
    }

  for (argnum = 0, leni = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      len = Flength (this);
      leni += XFASTINT (len);
    }

  XSETFASTINT (len, leni);

  if (target_type == Lisp_Cons)
    val = Fmake_list (len, Qnil);
  else if (target_type == Lisp_Vectorlike)
    val = Fmake_vector (len, Qnil);
  else
    val = Fmake_string (len, len);

  /* In append, if all but last arg are nil, return last arg */
  if (target_type == Lisp_Cons && EQ (val, Qnil))
    return last_tail;

  if (CONSP (val))
    tail = val, toindex = -1;		/* -1 in toindex is flag we are making a list */
  else
    toindex = 0;

  prev = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object thislen;
      int thisleni;
      register int thisindex = 0;

      this = args[argnum];
      if (!CONSP (this))
	thislen = Flength (this), thisleni = XINT (thislen);

      if (STRINGP (this) && STRINGP (val)
	  && ! NULL_INTERVAL_P (XSTRING (this)->intervals))
	{
	  copy_text_properties (make_number (0), thislen, this,
				make_number (toindex), val, Qnil);
	}

      while (1)
	{
	  register Lisp_Object elt;

	  /* Fetch next element of `this' arg into `elt', or break if
             `this' is exhausted. */
	  if (NILP (this)) break;
	  if (CONSP (this))
	    elt = Fcar (this), this = Fcdr (this);
	  else
	    {
	      if (thisindex >= thisleni) break;
	      if (STRINGP (this))
		XSETFASTINT (elt, XSTRING (this)->data[thisindex++]);
	      else
		elt = XVECTOR (this)->contents[thisindex++];
	    }

	  /* Store into result */
	  if (toindex < 0)
	    {
	      XCONS (tail)->car = elt;
	      prev = tail;
	      tail = XCONS (tail)->cdr;
	    }
	  else if (VECTORP (val))
	    XVECTOR (val)->contents[toindex++] = elt;
	  else
	    {
	      while (!INTEGERP (elt))
		elt = wrong_type_argument (Qintegerp, elt);
	      {
#ifdef MASSC_REGISTER_BUG
		/* Even removing all "register"s doesn't disable this bug!
		   Nothing simpler than this seems to work. */
		unsigned char *p = & XSTRING (val)->data[toindex++];
		*p = XINT (elt);
#else
		XSTRING (val)->data[toindex++] = XINT (elt);
#endif
	      }
	    }
	}
    }
  if (!NILP (prev))
    XCONS (prev)->cdr = last_tail;

  return val;  
}

DEFUN ("copy-alist", Fcopy_alist, Scopy_alist, 1, 1, 0,
  "Return a copy of ALIST.\n\
This is an alist which represents the same mapping from objects to objects,\n\
but does not share the alist structure with ALIST.\n\
The objects mapped (cars and cdrs of elements of the alist)\n\
are shared, however.\n\
Elements of ALIST that are not conses are also shared.")
  (alist)
     Lisp_Object alist;
{
  register Lisp_Object tem;

  CHECK_LIST (alist, 0);
  if (NILP (alist))
    return alist;
  alist = concat (1, &alist, Lisp_Cons, 0);
  for (tem = alist; CONSP (tem); tem = XCONS (tem)->cdr)
    {
      register Lisp_Object car;
      car = XCONS (tem)->car;

      if (CONSP (car))
	XCONS (tem)->car = Fcons (XCONS (car)->car, XCONS (car)->cdr);
    }
  return alist;
}

DEFUN ("substring", Fsubstring, Ssubstring, 2, 3, 0,
  "Return a substring of STRING, starting at index FROM and ending before TO.\n\
TO may be nil or omitted; then the substring runs to the end of STRING.\n\
If FROM or TO is negative, it counts from the end.")
  (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  Lisp_Object res;

  CHECK_STRING (string, 0);
  CHECK_NUMBER (from, 1);
  if (NILP (to))
    to = Flength (string);
  else
    CHECK_NUMBER (to, 2);

  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + XSTRING (string)->size);
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + XSTRING (string)->size);
  if (!(0 <= XINT (from) && XINT (from) <= XINT (to)
        && XINT (to) <= XSTRING (string)->size))
    args_out_of_range_3 (string, from, to);

  res = make_string (XSTRING (string)->data + XINT (from),
		     XINT (to) - XINT (from));
  copy_text_properties (from, to, string, make_number (0), res, Qnil);
  return res;
}

DEFUN ("nthcdr", Fnthcdr, Snthcdr, 2, 2, 0,
  "Take cdr N times on LIST, returns the result.")
  (n, list)
     Lisp_Object n;
     register Lisp_Object list;
{
  register int i, num;
  CHECK_NUMBER (n, 0);
  num = XINT (n);
  for (i = 0; i < num && !NILP (list); i++)
    {
      QUIT;
      list = Fcdr (list);
    }
  return list;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
  "Return the Nth element of LIST.\n\
N counts from zero.  If LIST is not that long, nil is returned.")
  (n, list)
     Lisp_Object n, list;
{
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
  "Return element of SEQUENCE at index N.")
  (seq, n)
     register Lisp_Object seq, n;
{
  CHECK_NUMBER (n, 0);
  while (1)
    {
      if (CONSP (seq) || NILP (seq))
	return Fcar (Fnthcdr (n, seq));
      else if (STRINGP (seq) || VECTORP (seq))
	return Faref (seq, n);
      else
	seq = wrong_type_argument (Qsequencep, seq);
    }
}

DEFUN ("member", Fmember, Smember, 2, 2, 0,
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (! NILP (Fequal (elt, tem)))
	return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQ.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (elt, tem)) return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
  "Return non-nil if KEY is `eq' to the car of an element of LIST.\n\
The value is actually the element of LIST whose car is KEY.\n\
Elements of LIST that are not conses are ignored.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object
assq_no_quit (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
    }
  return Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 2, 0,
  "Return non-nil if KEY is `equal' to the car of an element of LIST.\n\
The value is actually the element of LIST whose car equals KEY.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fequal (Fcar (elt), key);
      if (!NILP (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
  "Return non-nil if ELT is `eq' to the cdr of an element of LIST.\n\
The value is actually the element of LIST whose cdr is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcdr (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("rassoc", Frassoc, Srassoc, 2, 2, 0,
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.\n\
The value is actually the element of LIST whose cdr equals KEY.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NILP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fequal (Fcdr (elt), key);
      if (!NILP (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
  "Delete by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.  Comparison is done with `eq'.\n\
If the first member of LIST is ELT, there is no way to remove it by side effect;\n\
therefore, write `(setq foo (delq element foo))'\n\
to be sure of changing the value of `foo'.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NILP (tail))
    {
      tem = Fcar (tail);
      if (EQ (elt, tem))
	{
	  if (NILP (prev))
	    list = Fcdr (tail);
	  else
	    Fsetcdr (prev, Fcdr (tail));
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }
  return list;
}

DEFUN ("delete", Fdelete, Sdelete, 2, 2, 0,
  "Delete by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.  Comparison is done with `equal'.\n\
If the first member of LIST is ELT, deleting it is not a side effect;\n\
it is simply using a different list.\n\
Therefore, write `(setq foo (delete element foo))'\n\
to be sure of changing the value of `foo'.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NILP (tail))
    {
      tem = Fcar (tail);
      if (! NILP (Fequal (elt, tem)))
	{
	  if (NILP (prev))
	    list = Fcdr (tail);
	  else
	    Fsetcdr (prev, Fcdr (tail));
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }
  return list;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
  "Reverse LIST by modifying cdr pointers.\n\
Returns the beginning of the reversed list.")
  (list)
     Lisp_Object list;
{
  register Lisp_Object prev, tail, next;

  if (NILP (list)) return list;
  prev = Qnil;
  tail = list;
  while (!NILP (tail))
    {
      QUIT;
      next = Fcdr (tail);
      Fsetcdr (tail, prev);
      prev = tail;
      tail = next;
    }
  return prev;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
  "Reverse LIST, copying.  Returns the beginning of the reversed list.\n\
See also the function `nreverse', which is used more often.")
  (list)
     Lisp_Object list;
{
  Lisp_Object length;
  register Lisp_Object *vec;
  register Lisp_Object tail;
  register int i;

  length = Flength (list);
  vec = (Lisp_Object *) alloca (XINT (length) * sizeof (Lisp_Object));
  for (i = XINT (length) - 1, tail = list; i >= 0; i--, tail = Fcdr (tail))
    vec[i] = Fcar (tail);

  return Flist (XINT (length), vec);
}

Lisp_Object merge ();

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
  "Sort LIST, stably, comparing elements using PREDICATE.\n\
Returns the sorted list.  LIST is modified by side effects.\n\
PREDICATE is called with two elements of LIST, and should return T\n\
if the first element is \"less\" than the second.")
  (list, pred)
     Lisp_Object list, pred;
{
  Lisp_Object front, back;
  register Lisp_Object len, tem;
  struct gcpro gcpro1, gcpro2;
  register int length;

  front = list;
  len = Flength (list);
  length = XINT (len);
  if (length < 2)
    return list;

  XSETINT (len, (length / 2) - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO2 (front, back);
  front = Fsort (front, pred);
  back = Fsort (back, pred);
  UNGCPRO;
  return merge (front, back, pred);
}

Lisp_Object
merge (org_l1, org_l2, pred)
     Lisp_Object org_l1, org_l2;
     Lisp_Object pred;
{
  Lisp_Object value;
  register Lisp_Object tail;
  Lisp_Object tem;
  register Lisp_Object l1, l2;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into the org_ vars.  */
  GCPRO4 (org_l1, org_l2, pred, value);

  while (1)
    {
      if (NILP (l1))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NILP (l2))
	{
	  UNGCPRO;
	  if (NILP (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}
      tem = call2 (pred, Fcar (l2), Fcar (l1));
      if (NILP (tem))
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;
	}
      if (NILP (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);
      tail = tem;
    }
}


DEFUN ("plist-get", Fplist_get, Splist_get, 2, 2, 0,
       "Extract a value from a property list.\n\
PLIST is a property list, which is a list of the form\n\
(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value\n\
corresponding to the given PROP, or nil if PROP is not\n\
one of the properties on the list.")
  (val, prop)
     Lisp_Object val;
     register Lisp_Object prop;
{
  register Lisp_Object tail;
  for (tail = val; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fcar (Fcdr (tail));
    }
  return Qnil;
}

DEFUN ("get", Fget, Sget, 2, 2, 0,
  "Return the value of SYMBOL's PROPNAME property.\n\
This is the last VALUE stored with `(put SYMBOL PROPNAME VALUE)'.")
  (sym, prop)
     Lisp_Object sym, prop;
{
  return Fplist_get (Fsymbol_plist (sym), prop);
}

DEFUN ("plist-put", Fplist_put, Splist_put, 3, 3, 0,
  "Change value in PLIST of PROP to VAL.\n\
PLIST is a property list, which is a list of the form\n\
(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.\n\
If PROP is already a property on the list, its value is set to VAL,\n\
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.\n\
The PLIST is modified by side effects.")
  (plist, prop, val)
    Lisp_Object plist;
    register Lisp_Object prop;
    Lisp_Object val;
{
  register Lisp_Object tail, prev;
  Lisp_Object newcell;
  prev = Qnil;
  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	{
	  Fsetcar (Fcdr (tail), val);
	  return plist;
	}
      prev = tail;
    }
  newcell = Fcons (prop, Fcons (val, Qnil));
  if (NILP (prev))
    return newcell;
  else
    Fsetcdr (Fcdr (prev), newcell);
  return plist;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
  "Store SYMBOL's PROPNAME property with value VALUE.\n\
It can be retrieved with `(get SYMBOL PROPNAME)'.")
  (sym, prop, val)
     Lisp_Object sym, prop, val;
{
  Fsetplist (sym, Fplist_put (Fsymbol_plist (sym), prop, val));
  return val;
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
  "T if two Lisp objects have similar structure and contents.\n\
They must have the same data type.\n\
Conses are compared by comparing the cars and the cdrs.\n\
Vectors and strings are compared element by element.\n\
Numbers are compared by value, but integers cannot equal floats.\n\
 (Use `=' if you want integers and floats to be able to be equal.)\n\
Symbols must match exactly.")
  (o1, o2)
     register Lisp_Object o1, o2;
{
  return internal_equal (o1, o2, 0) ? Qt : Qnil;
}

static int
internal_equal (o1, o2, depth)
     register Lisp_Object o1, o2;
     int depth;
{
  if (depth > 200)
    error ("Stack overflow in equal");

 tail_recurse:
  QUIT;
  if (EQ (o1, o2))
    return 1;
  if (XTYPE (o1) != XTYPE (o2))
    return 0;

  switch (XTYPE (o1))
    {
#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      return (extract_float (o1) == extract_float (o2));
#endif

    case Lisp_Cons:
      if (!internal_equal (XCONS (o1)->car, XCONS (o2)->car, depth + 1))
	return 0;
      o1 = XCONS (o1)->cdr;
      o2 = XCONS (o2)->cdr;
      goto tail_recurse;

    case Lisp_Misc:
      if (XMISC (o1)->type != XMISC (o2)->type)
	return 0;
      if (OVERLAYP (o1))
	{
	  if (!internal_equal (OVERLAY_START (o1), OVERLAY_START (o1),
			       depth + 1)
	      || !internal_equal (OVERLAY_END (o1), OVERLAY_END (o1),
				  depth + 1))
	    return 0;
	  o1 = XOVERLAY (o1)->plist;
	  o2 = XOVERLAY (o2)->plist;
	  goto tail_recurse;
	}
      if (MARKERP (o1))
	{
	  return (XMARKER (o1)->buffer == XMARKER (o2)->buffer
		  && (XMARKER (o1)->buffer == 0
		      || XMARKER (o1)->bufpos == XMARKER (o2)->bufpos));
	}
      break;

    case Lisp_Vectorlike:
      {
	register int i, size;
	size = XVECTOR (o1)->size;
	/* Pseudovectors have the type encoded in the size field, so this test
	   actually checks that the objects have the same type as well as the
	   same size.  */
	if (XVECTOR (o2)->size != size)
	  return 0;
	/* But only true vectors and compiled functions are actually sensible
	   to compare, so eliminate the others now.  */
	if (size & PSEUDOVECTOR_FLAG)
	  {
	    if (!(size & PVEC_COMPILED))
	      return 0;
	    size &= PSEUDOVECTOR_SIZE_MASK;
	  }
	for (i = 0; i < size; i++)
	  {
	    Lisp_Object v1, v2;
	    v1 = XVECTOR (o1)->contents [i];
	    v2 = XVECTOR (o2)->contents [i];
	    if (!internal_equal (v1, v2, depth + 1))
	      return 0;
	  }
	return 1;
      }
      break;

    case Lisp_String:
      if (XSTRING (o1)->size != XSTRING (o2)->size)
	return 0;
      if (bcmp (XSTRING (o1)->data, XSTRING (o2)->data,
		XSTRING (o1)->size))
	return 0;
#ifdef USE_TEXT_PROPERTIES
      /* If the strings have intervals, verify they match;
	 if not, they are unequal.  */
      if ((XSTRING (o1)->intervals != 0 || XSTRING (o2)->intervals != 0)
	  && ! compare_string_intervals (o1, o2))
	return 0;
#endif
      return 1;
    }
  return 0;
}

DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
  "Store each element of ARRAY with ITEM.  ARRAY is a vector or string.")
  (array, item)
     Lisp_Object array, item;
{
  register int size, index, charval;
 retry:
  if (VECTORP (array))
    {
      register Lisp_Object *p = XVECTOR (array)->contents;
      size = XVECTOR (array)->size;
      for (index = 0; index < size; index++)
	p[index] = item;
    }
  else if (STRINGP (array))
    {
      register unsigned char *p = XSTRING (array)->data;
      CHECK_NUMBER (item, 1);
      charval = XINT (item);
      size = XSTRING (array)->size;
      for (index = 0; index < size; index++)
	p[index] = charval;
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }
  return array;
}

/* ARGSUSED */
Lisp_Object
nconc2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return Fnconc (2, args);
#else
  return Fnconc (2, &s1);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("nconc", Fnconc, Snconc, 0, MANY, 0,
  "Concatenate any number of lists by altering them.\n\
Only the last argument is not altered, and need not be a list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tail, tem, val;

  val = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
      if (NILP (tem)) continue;

      if (NILP (val))
	val = tem;

      if (argnum + 1 == nargs) break;

      if (!CONSP (tem))
	tem = wrong_type_argument (Qlistp, tem);

      while (CONSP (tem))
	{
	  tail = tem;
	  tem = Fcdr (tail);
	  QUIT;
	}

      tem = args[argnum + 1];
      Fsetcdr (tail, tem);
      if (NILP (tem))
	args[argnum + 1] = tail;
    }

  return val;
}

/* This is the guts of all mapping functions.
 Apply fn to each element of seq, one by one,
 storing the results into elements of vals, a C vector of Lisp_Objects.
 leni is the length of vals, which should also be the length of seq. */

static void
mapcar1 (leni, vals, fn, seq)
     int leni;
     Lisp_Object *vals;
     Lisp_Object fn, seq;
{
  register Lisp_Object tail;
  Lisp_Object dummy;
  register int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* Don't let vals contain any garbage when GC happens.  */
  for (i = 0; i < leni; i++)
    vals[i] = Qnil;

  GCPRO3 (dummy, fn, seq);
  gcpro1.var = vals;
  gcpro1.nvars = leni;
  /* We need not explicitly protect `tail' because it is used only on lists, and
    1) lists are not relocated and 2) the list is marked via `seq' so will not be freed */

  if (VECTORP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = XVECTOR (seq)->contents[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (STRINGP (seq))
    {
      for (i = 0; i < leni; i++)
	{
	  XSETFASTINT (dummy, XSTRING (seq)->data[i]);
	  vals[i] = call1 (fn, dummy);
	}
    }
  else   /* Must be a list, since Flength did not get an error */
    {
      tail = seq;
      for (i = 0; i < leni; i++)
	{
	  vals[i] = call1 (fn, Fcar (tail));
	  tail = Fcdr (tail);
	}
    }

  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
  "Apply FN to each element of SEQ, and concat the results as strings.\n\
In between each pair of results, stick in SEP.\n\
Thus, \" \" as SEP results in spaces between the values returned by FN.")
  (fn, seq, sep)
     Lisp_Object fn, seq, sep;
{
  Lisp_Object len;
  register int leni;
  int nargs;
  register Lisp_Object *args;
  register int i;
  struct gcpro gcpro1;

  len = Flength (seq);
  leni = XINT (len);
  nargs = leni + leni - 1;
  if (nargs < 0) return build_string ("");

  args = (Lisp_Object *) alloca (nargs * sizeof (Lisp_Object));

  GCPRO1 (sep);
  mapcar1 (leni, args, fn, seq);
  UNGCPRO;

  for (i = leni - 1; i >= 0; i--)
    args[i + i] = args[i];
      
  for (i = 1; i < nargs; i += 2)
    args[i] = sep;

  return Fconcat (nargs, args);
}

DEFUN ("mapcar", Fmapcar, Smapcar, 2, 2, 0,
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.\n\
The result is a list just as long as SEQUENCE.\n\
SEQUENCE may be a list, a vector or a string.")
  (fn, seq)
     Lisp_Object fn, seq;
{
  register Lisp_Object len;
  register int leni;
  register Lisp_Object *args;

  len = Flength (seq);
  leni = XFASTINT (len);
  args = (Lisp_Object *) alloca (leni * sizeof (Lisp_Object));

  mapcar1 (leni, args, fn, seq);

  return Flist (leni, args);
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("y-or-n-p", Fy_or_n_p, Sy_or_n_p, 1, 1, 0,
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".\n\
Takes one argument, which is the string to display to ask the question.\n\
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.\n\
No confirmation of the answer is requested; a single character is enough.\n\
Also accepts Space to mean yes, or Delete to mean no.")
  (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object obj, key, def, answer_string, map;
  register int answer;
  Lisp_Object xprompt;
  Lisp_Object args[2];
  int ocech = cursor_in_echo_area;
  struct gcpro gcpro1, gcpro2;

  map = Fsymbol_value (intern ("query-replace-map"));

  CHECK_STRING (prompt, 0);
  xprompt = prompt;
  GCPRO2 (prompt, xprompt);

  while (1)
    {
#ifdef HAVE_X_MENU
      if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
	  && using_x_p ())
	{
	  Lisp_Object pane, menu;
	  redisplay_preserve_echo_area ();
	  pane = Fcons (Fcons (build_string ("Yes"), Qt),
			Fcons (Fcons (build_string ("No"), Qnil),
			       Qnil));
	  menu = Fcons (prompt, pane);
	  obj = Fx_popup_dialog (Qt, menu);
	  answer = !NILP (obj);
	  break;
	}
#endif
      cursor_in_echo_area = 1;
      message ("%s(y or n) ", XSTRING (xprompt)->data);

      obj = read_filtered_event (1, 0, 0);
      cursor_in_echo_area = 0;
      /* If we need to quit, quit with cursor_in_echo_area = 0.  */
      QUIT;

      key = Fmake_vector (make_number (1), obj);
      def = Flookup_key (map, key);
      answer_string = Fsingle_key_description (obj);

      if (EQ (def, intern ("skip")))
	{
	  answer = 0;
	  break;
	}
      else if (EQ (def, intern ("act")))
	{
	  answer = 1;
	  break;
	}
      else if (EQ (def, intern ("recenter")))
	{
	  Frecenter (Qnil);
	  xprompt = prompt;
	  continue;
	}
      else if (EQ (def, intern ("quit")))
	Vquit_flag = Qt;
      /* We want to exit this command for exit-prefix,
	 and this is the only way to do it.  */
      else if (EQ (def, intern ("exit-prefix")))
	Vquit_flag = Qt;

      QUIT;

      /* If we don't clear this, then the next call to read_char will
	 return quit_char again, and we'll enter an infinite loop.  */
      Vquit_flag = Qnil;

      Fding (Qnil);
      Fdiscard_input ();
      if (EQ (xprompt, prompt))
	{
	  args[0] = build_string ("Please answer y or n.  ");
	  args[1] = prompt;
	  xprompt = Fconcat (2, args);
	}
    }
  UNGCPRO;

  if (! noninteractive)
    {
      cursor_in_echo_area = -1;
      message ("%s(y or n) %c", XSTRING (xprompt)->data, answer ? 'y' : 'n');
      cursor_in_echo_area = ocech;
    }

  return answer ? Qt : Qnil;
}

/* This is how C code calls `yes-or-no-p' and allows the user
   to redefined it.

   Anything that calls this function must protect from GC!  */

Lisp_Object
do_yes_or_no_p (prompt)
     Lisp_Object prompt;
{
  return call1 (intern ("yes-or-no-p"), prompt);
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("yes-or-no-p", Fyes_or_no_p, Syes_or_no_p, 1, 1, 0,
  "Ask user a yes-or-no question.  Return t if answer is yes.\n\
Takes one argument, which is the string to display to ask the question.\n\
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.\n\
The user must confirm the answer with RET,\n\
and can edit it until it as been confirmed.")
  (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object ans;
  Lisp_Object args[2];
  struct gcpro gcpro1;
  Lisp_Object menu;

  CHECK_STRING (prompt, 0);

#ifdef HAVE_X_MENU
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && using_x_p ())
    {
      Lisp_Object pane, menu, obj;
      redisplay_preserve_echo_area ();
      pane = Fcons (Fcons (build_string ("Yes"), Qt),
		    Fcons (Fcons (build_string ("No"), Qnil),
			   Qnil));
      GCPRO1 (pane);
      menu = Fcons (prompt, pane);
      obj = Fx_popup_dialog (Qt, menu);
      UNGCPRO;
      return obj;
    }
#endif

  args[0] = prompt;
  args[1] = build_string ("(yes or no) ");
  prompt = Fconcat (2, args);

  GCPRO1 (prompt);

  while (1)
    {
      ans = Fdowncase (Fread_from_minibuffer (prompt, Qnil, Qnil, Qnil,
					      Qyes_or_no_p_history));
      if (XSTRING (ans)->size == 3 && !strcmp (XSTRING (ans)->data, "yes"))
	{
	  UNGCPRO;
	  return Qt;
	}
      if (XSTRING (ans)->size == 2 && !strcmp (XSTRING (ans)->data, "no"))
	{
	  UNGCPRO;
	  return Qnil;
	}

      Fding (Qnil);
      Fdiscard_input ();
      message ("Please answer yes or no.");
      Fsleep_for (make_number (2), Qnil);
    }
}

DEFUN ("load-average", Fload_average, Sload_average, 0, 0, 0,
  "Return list of 1 minute, 5 minute and 15 minute load averages.\n\
Each of the three load averages is multiplied by 100,\n\
then converted to integer.\n\
If the 5-minute or 15-minute load averages are not available, return a\n\
shortened list, containing only those averages which are available.")
  ()
{
  double load_ave[3];
  int loads = getloadavg (load_ave, 3);
  Lisp_Object ret;

  if (loads < 0)
    error ("load-average not implemented for this operating system");

  ret = Qnil;
  while (loads > 0)
    ret = Fcons (make_number ((int) (load_ave[--loads] * 100.0)), ret);

  return ret;
}

Lisp_Object Vfeatures;

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 1, 0,
  "Returns t if FEATURE is present in this Emacs.\n\
Use this to conditionalize execution of lisp code based on the presence or\n\
absence of emacs or environment extensions.\n\
Use `provide' to declare that a feature is available.\n\
This function looks at the value of the variable `features'.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  return (NILP (tem)) ? Qnil : Qt;
}

DEFUN ("provide", Fprovide, Sprovide, 1, 1, 0,
  "Announce that FEATURE is a feature of the current Emacs.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  if (!NILP (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (Vfeatures, Qnil), Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NILP (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qprovide, feature));
  return feature;
}

DEFUN ("require", Frequire, Srequire, 1, 2, 0,
  "If feature FEATURE is not loaded, load it from FILENAME.\n\
If FEATURE is not a member of the list `features', then the feature\n\
is not loaded; so load the file FILENAME.\n\
If FILENAME is omitted, the printname of FEATURE is used as the file name.")
     (feature, file_name)
     Lisp_Object feature, file_name;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  LOADHIST_ATTACH (Fcons (Qrequire, feature));
  if (NILP (tem))
    {
      int count = specpdl_ptr - specpdl;

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      Fload (NILP (file_name) ? Fsymbol_name (feature) : file_name,
	     Qnil, Qt, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NILP (tem))
	error ("Required feature %s was not provided",
	       XSYMBOL (feature)->name->data );

      /* Once loading finishes, don't undo it.  */
      Vautoload_queue = Qt;
      feature = unbind_to (count, feature);
    }
  return feature;
}

syms_of_fns ()
{
  Qstring_lessp = intern ("string-lessp");
  staticpro (&Qstring_lessp);
  Qprovide = intern ("provide");
  staticpro (&Qprovide);
  Qrequire = intern ("require");
  staticpro (&Qrequire);
  Qyes_or_no_p_history = intern ("yes-or-no-p-history");
  staticpro (&Qyes_or_no_p_history);

  DEFVAR_LISP ("features", &Vfeatures,
    "A list of symbols which are the features of the executing emacs.\n\
Used by `featurep' and `require', and altered by `provide'.");
  Vfeatures = Qnil;

  defsubr (&Sidentity);
  defsubr (&Srandom);
  defsubr (&Slength);
  defsubr (&Sstring_equal);
  defsubr (&Sstring_lessp);
  defsubr (&Sappend);
  defsubr (&Sconcat);
  defsubr (&Svconcat);
  defsubr (&Scopy_sequence);
  defsubr (&Scopy_alist);
  defsubr (&Ssubstring);
  defsubr (&Snthcdr);
  defsubr (&Snth);
  defsubr (&Selt);
  defsubr (&Smember);
  defsubr (&Smemq);
  defsubr (&Sassq);
  defsubr (&Sassoc);
  defsubr (&Srassq);
  defsubr (&Srassoc);
  defsubr (&Sdelq);
  defsubr (&Sdelete);
  defsubr (&Snreverse);
  defsubr (&Sreverse);
  defsubr (&Ssort);
  defsubr (&Splist_get);
  defsubr (&Sget);
  defsubr (&Splist_put);
  defsubr (&Sput);
  defsubr (&Sequal);
  defsubr (&Sfillarray);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapconcat);
  defsubr (&Sy_or_n_p);
  defsubr (&Syes_or_no_p);
  defsubr (&Sload_average);
  defsubr (&Sfeaturep);
  defsubr (&Srequire);
  defsubr (&Sprovide);
}
