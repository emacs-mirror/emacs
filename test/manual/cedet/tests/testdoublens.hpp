// testdoublens.hpp --- Header file used in one of the Semantic tests

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

namespace Name1 {
  namespace Name2 {

    class Foo
    {
      typedef unsigned int Mumble;
    public:
      Foo();
      ~Foo();
      int get();

    private:
      void publishStuff(int a, int b);

      void sendStuff(int a, int b);

      Mumble* pMumble;
    };

    typedef Foo stage1_Foo;

  } // namespace Name2

  typedef Name2::stage1_Foo stage2_Foo;

  typedef Name2::Foo decl_stage1_Foo;

} // namespace Name1

typedef Name1::stage2_Foo stage3_Foo;


// Double namespace from Hannu Koivisto
namespace a {
  namespace b {

    class Foo
    {
      struct Dum {
        int diDum;
      };

    protected:
      mutable a::b::Foo::Dum dumdum;
    };

  } // namespace b
} // namespace a
