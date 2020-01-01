// teststruct.cpp --- semantic-ia-utest completion engine unit tests

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


// Note: initially provided by by Alex Ott.

template <typename DerivedT>
struct grammar {
public:
  typedef grammar<DerivedT> self_t;
  typedef DerivedT const& embed_t;
  grammar() {}
  ~grammar() { }
  void use_parser() const { }
  void test1() { }
};

struct PDFbool_parser : public grammar<PDFbool_parser> {
  PDFbool_parser() {}
  template <typename scannerT> struct definition {
    typedef typename scannerT::iterator_t iterator_t;
    int top;
    definition(const PDFbool_parser& /*self*/) {
      return ;
    }
    const int start() const {
      return top;
    }
  };
};

int main(void) {
  PDFbool_parser PDFbool_p = PDFbool_parser();
  PDFbool_p.//-1-
    ;
  // #1# ("definition" "embed_t" "self_t" "test1" "use_parser")
}

// ----------------------------------------------------------------------

template <class Derived> struct Base {
public:
  void interface()
  {
    // ...
    static_cast<Derived*>(this)->implementation();
    // ...
  }

  static void static_func()
  {
    // ...
    Derived::static_sub_func();
    // ...
  }
};

struct Derived : Base<Derived> {
  void implementation() { }
  static void static_sub_func() { }
};

int foo () {
  Derived d;
  d.//-2-
    ;
  // #2# ("implementation" "interface" "static_func" "static_sub_func")
}
