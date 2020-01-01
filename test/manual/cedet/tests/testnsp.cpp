/* testnsp.cpp --- semantic-ia-utest completion engine unit tests

   Copyright (C) 2008-2020 Free Software Foundation, Inc.

   Author: Eric M. Ludlam <zappo@gnu.org>

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
   along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
*/

namespace nsp {

  class rootclass {
  public:
    int fromroot() {};
  };

}

namespace nsp {
  class childclass : public rootclass {
  public:
    int fromchild() {};
  };
}

void myfcn_not_in_ns (void) {
  nsp::childclass test;

  test.// -1-
    ; // #1# ( "fromchild" "fromroot" )
}

// Test a class declared in a class, where the contents
// are in a qualified name.
//
// Thanks Michael Reiher for the concise example.

class AAA
{
public:
  AAA();

  void aaa();

private:
    class Private;
    Private * const d;
};

class AAA::Private
{
    Private() : bbb(0) {
    }

    BBB* bbb;
};

void AAA::aaa()
{
  d->// -2-
    ; // #2# ( "bbb" )
}

// #include files inside a namespace
// David Engster <deng@randomsample.de>
// See revisions 8034-8037 which implement this.

namespace another {
  #include "testdoublens.hpp"
}

void foo(void) {

  another::// -3-
    ; // #3# ( "Name1" "a" "stage3_Foo" )

  another::Name1::Name2::Foo a;

  a.// -4-
    ; // #4# ( "Mumble" "get" )
}

// What happens if a type your looking for is scoped withing a type,
// but you are one level into the completion so the originating scope
// excludes the type of the variable you are completing through?
// Thanks Martin Stein for this nice example.

namespace ms_structs
{
   struct ms_aaa
   {
     int xx;
   };

   struct ms_bbb
   {
     struct ms_aaa yy;
   };
};

int fun()
{
   using namespace ms_structs;
   struct ms_bbb mszz;
   int uu = mszz.// -5-
     ; // #5# ( "yy" )
   int kk = mszz.yy.// - 6-  @TODO - bring in patch from SF
     ; // #6# ( "xx" )
}
