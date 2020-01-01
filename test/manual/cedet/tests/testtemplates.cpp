// testtemplates.cpp --- semantic-ia-utest completion engine unit tests

// Copyright (C) 2008-2020 Free Software Foundation, Inc.

// Author: Eric M. Ludlam <zappo@gnu.org>

// This file is part of GNU Emacs.

// GNU Emacs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// GNU Emacs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


// TODO - this probably means can't be part of emacs, as I don't know who this guy is.
// Written by 'Raf'

template <class T, int U, class V>
class read_ref {
public:
  const T* read_ref_member_one( T);
  const V* read_ref_member_two();
};

namespace NS {
  template <class T, int U, class V>
  class ref {
  public:
    read_ref<T,10,V> operator->() {
      m_// -1-
	;
      // #1# ( "m_datas" )
    }

  private:
    T m_datas[U];
  };

}

class FooOne {
public:
  int fooOneMember();
};

class FooTwo {
public:
  int fooTwoMember();
};

class FooThree {
public:
  int fooThreeMember();

  FooOne * operator->();
};

typedef ref<FooOne, 10,FooTwo> Test;

using NS;

void
main(void) {
  ref<FooOne, 10, FooTwo> v;

  v->read_ref_member_one()-> // -2-
    ;
  // #2# ( "fooOneMember" )

  v->read_ref_member_two()-> // -3-
    ;
  // #3# ( "fooTwoMember" )

  v-> // -4-
    ;
  // #4# ( "read_ref_member_one" "read_ref_member_two" )

  Test t;

  t->read_ref_member_two()-> // -5-
    ;
  // #5# ( "fooTwoMember" )

  ref<FooOne, 10, FooThree> v2;

  v2->read_ref_member_two()-> // -6-
    ;
  // #6# ( "fooOneMember" )

  /* Try all these things by also specifying the namespace in the name. */
  NS::ref<FooOne, 10, FooTwo> v3;

  v3->read_ref_member_one()-> // -7-
    ;
  // #7# ( "fooOneMember" )

  v3->read_ref_member_two()-> // -8-
    ;
  // #8# ( "fooTwoMember" )

  v3->read_ref_member_two// @1@ 5
    ;

}

// More Namespace Magic using member constants.

template<typename T>
struct isFooLike {
  static const bool value = false;
};

template <>
struct isFooLike<int> {
  static const bool value = true;
};


template <typename T, bool isFoo>
class A {
public:
  A();
  void foo() {};
};


template <typename T>
class FooFour : public A<T, isPodLike<T>::value> {
public:
  bool bar() {}
};


int main2() {

  FooFour<int> ff;

  ff.// - 9- @ TODO - bring over patch from SF
    ; // #9# ( "bar" "foo" );

}
