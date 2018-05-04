// Template array classes with like-type math ops
/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma interface
#endif

#if !defined (octave_MDiagArray2_h)
#define octave_MDiagArray2_h 1

#include "DiagArray2.h"
#include "MArray2.h"

#if defined (LTGT)
#undef LTGT
#endif

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
#define LTGT
#else

#define LTGT <>

template <class T>
class MDiagArray2;

template <typename T> MDiagArray2<T>&
operator += (MDiagArray2<T>& a, const MDiagArray2<T>& b);

template <typename T> MDiagArray2<T>&
operator -= (MDiagArray2<T>& a, const MDiagArray2<T>& b);

template <typename T> MDiagArray2<T> 
operator * (const MDiagArray2<T>& a, const T& s);

template <typename T> MDiagArray2<T> 
operator / (const MDiagArray2<T>& a, const T& s);

template <typename T> MDiagArray2<T> 
operator * (const T& s, const MDiagArray2<T>& a);

template <typename T> MDiagArray2<T>
operator + (const MDiagArray2<T>& a, const MDiagArray2<T>& b); 

template <typename T> MDiagArray2<T>
operator - (const MDiagArray2<T>& a, const MDiagArray2<T>& b);

template <typename T> MDiagArray2<T>
product (const MDiagArray2<T>& a, const MDiagArray2<T>& b);

template <typename T> MDiagArray2<T> 
operator - (const MDiagArray2<T>& a);
#endif

// Two dimensional diagonal array with math ops.

template <class T>
class MDiagArray2 : public DiagArray2<T>
{
protected:

  MDiagArray2 (T *d, int r, int c) : DiagArray2<T> (d, r, c) { }

public:
  
  MDiagArray2 (void) : DiagArray2<T> () { }
  MDiagArray2 (int r, int c) : DiagArray2<T> (r, c) { }
  MDiagArray2 (int r, int c, const T& val) : DiagArray2<T> (r, c, val) { }
  MDiagArray2 (const Array<T>& a) : DiagArray2<T> (a) { }
  MDiagArray2 (const DiagArray2<T>& a) : DiagArray2<T> (a) { }
  MDiagArray2 (const MDiagArray2<T>& a) : DiagArray2<T> (a) { }

  ~MDiagArray2 (void) { }

  MDiagArray2<T>& operator = (const MDiagArray2<T>& a)
    {
      DiagArray2<T>::operator = (a);
      return *this;
    }

  operator MArray2<T> () const
    {
      MArray2<T> retval (nr, nc,  T (0));

      int len = nr < nc ? nr : nc;

      for (int i = 0; i < len; i++)
	retval.xelem (i, i) = xelem (i, i);

      return retval;
    }

  // element by element MDiagArray2 by MDiagArray2 ops

  friend MDiagArray2<T>&
  operator += LTGT (MDiagArray2<T>& a, const MDiagArray2<T>& b);

  friend MDiagArray2<T>&
  operator -= LTGT (MDiagArray2<T>& a, const MDiagArray2<T>& b);

  // element by element MDiagArray2 by scalar ops

  friend MDiagArray2<T> operator * LTGT (const MDiagArray2<T>& a, const T& s);
  friend MDiagArray2<T> operator / LTGT (const MDiagArray2<T>& a, const T& s);

  // element by element scalar by MDiagArray2 ops

  friend MDiagArray2<T> operator * LTGT (const T& s, const MDiagArray2<T>& a);

  // element by element MDiagArray2 by MDiagArray2 ops

  friend MDiagArray2<T>
  operator + LTGT (const MDiagArray2<T>& a, const MDiagArray2<T>& b); 

  friend MDiagArray2<T>
  operator - LTGT (const MDiagArray2<T>& a, const MDiagArray2<T>& b);

  friend MDiagArray2<T>
  product LTGT (const MDiagArray2<T>& a, const MDiagArray2<T>& b);

  friend MDiagArray2<T> operator - LTGT (const MDiagArray2<T>& a);
};

#undef LTGT

#define INSTANTIATE_MDIAGARRAY_FRIENDS(T) \
  template MDiagArray2<T>& operator += (MDiagArray2<T>& a, const MDiagArray2<T>& b); \
  template MDiagArray2<T>& operator -= (MDiagArray2<T>& a, const MDiagArray2<T>& b); \
  template MDiagArray2<T> operator * (const MDiagArray2<T>& a, const T& s); \
  template MDiagArray2<T> operator / (const MDiagArray2<T>& a, const T& s); \
  template MDiagArray2<T> operator * (const T& s, const MDiagArray2<T>& a); \
  template MDiagArray2<T> operator + (const MDiagArray2<T>& a, const MDiagArray2<T>& b); \
  template MDiagArray2<T> operator - (const MDiagArray2<T>& a, const MDiagArray2<T>& b); \
  template MDiagArray2<T> product (const MDiagArray2<T>& a, const MDiagArray2<T>& b); \
  template MDiagArray2<T> operator - (const MDiagArray2<T>& a);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
