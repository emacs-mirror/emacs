// testdoublens.cpp --- semantic-ia-utest completion engine unit tests

// Copyright (C) 2008-2017 Free Software Foundation, Inc.

// Author: Eric M. Ludlam <eric@siege-engine.com>

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

#include "testdoublens.hpp"

namespace Name1 {
  namespace Name2 {

    Foo::Foo()
    {
      p// -1-
	// #1# ( "pMumble" "publishStuff" )
	;
    }

    int Foo::get() // ^1^
    {
      p// -2-
	// #2# ( "pMumble" "publishStuff" )
	;
      return 0;
    }

    void Foo::publishStuff(int /* a */, int /* b */) // ^2^
    {
    }

    void Foo::sendStuff(int /* a */, int /* b */) // ^3^
    {
    }

  } // namespace Name2
} // namespace Name1

// Test multiple levels of metatype expansion
int test_fcn () {
  stage3_Foo MyFoo;

  MyFoo.// -3-
    // #3# ( "Mumble" "get" )
    ;

  Name1::Name2::F//-4-
    // #4# ( "Foo" )
    ;

  // @TODO - get this working...
  Name1::stage2_Foo::M//-5-
    /// #5# ( "Mumble" )
    ;
}

stage3_Foo foo_fcn() {
  // Can we go "up" to foo with senator-go-to-up-reference?
}


// Second test from Ravikiran Rajagopal

namespace A {
  class foo {
  public:
    void aa();
    void bb();
  };
}
namespace A {
  class bar {
  public:
    void xx();
  public:
    foo myFoo;
  };

  void bar::xx()
  {
    myFoo.// -6- <--- cursor is here after the dot
      // #6# ( "aa" "bb" )
      ;
  }
}

// Double namespace example from Hannu Koivisto
//
// This is tricky because the parent class "Foo" is found within the
// scope of B, so the scope calculation needs to put that together
// before searching for parents in scope.
namespace a {
  namespace b {

    class Bar : public Foo
    {
      int baz();
    };

    int Bar::baz()
    {
      return dum// -7-
	// #7# ( "dumdum" )
	;
    }

  } // namespace b
} // namespace a

// Three namespace example from Hannu Koivisto
//
// This one is special in that the name e::Foo, where "e" is in
// the scope, and not referenced from the global namespace.  This
// wasn't previously handled, so the fullscope needed to be added
// to the list of things searched when in split-name decent search mode
// for scopes.

namespace d {
  namespace e {

    class Foo
    {
    public:
      int write();
    };

  } // namespace d
} // namespace e


namespace d {
  namespace f {

    class Bar
    {
    public:
      int baz();

    private:
      e::Foo &foo;
    };

    int Bar::baz()
    {
      return foo.w// -8-
	// #8# ( "write" )
	;
    }

  } // namespace f
} // namespace d
