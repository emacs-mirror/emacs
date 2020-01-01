// testusing.cpp --- semantic-ia-utest completion engine unit tests

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


#include <adstdio.h>

#include <testusing.hh>

namespace moose {

  class MyClass;
  class Point;

  typedef MyClass snerk;
}

namespace moose {

  class Point;
  class MyClass;

}

namespace {

  int global_variable = 0;

};

using moose::MyClass;

void someFcn() {

  MyClass f;

  f.//-1-
    ; //#1# ( "getVal" "setVal" )

}

// Code from Zhiqiu Kong

namespace panda {

  using namespace bread_name;

  int func()
  {
    bread test;
    test.//-2-
      ;// #2# ( "geta" )
    return 0;
  }
}

namespace togglemoose {

  MyOtherClass::testToggle1() { //^1^
    // Impl for testToggle1
  }
}

togglemoose::MyOtherClass::testToggle2() { //^3^
  // Impl for testToggle2
}

using togglemoose;

MyOtherClass::testToggle3() { //^3^
  // Impl for testToggle3
}

// Local using statements and aliased types
// Code from David Engster

void func2()
{
  using namespace somestuff;
  OneClass f;
  f.//-3-
    ; //#3# ( "aFunc" "anInt" )
}

void func3()
{
  using somestuff::OneClass;
  OneClass f;
  f.//-4-
    ; //#4# ( "aFunc" "anInt" )
}

// Dereferencing alias types created through 'using' statements

// Alias with fully qualified name
void func4()
{
  otherstuff::OneClass f;
  f. //-5-
    ; //#5# ( "aFunc" "anInt" )
}

// Alias through namespace directive
void func5()
{
  using namespace otherstuff;
  OneClass f;
  f. //-6-
    ; //#6# ( "aFunc" "anInt" )
}

// Check name hiding
void func6()
{
  using namespace morestuff;
  OneClass f;		// Alias for somestuff::OneClass
  f.  //-7-
    ; //#7# ( "aFunc" "anInt" )
  aStruct g;	// This however is morestuff::aStruct !
  g. //-8-
    ; //#8# ( "anotherBar" "anotherFoo" )
}

// Alias of an alias
// Currently doesn't work interactively for some reason.
void func6()
{
  using namespace evenmorestuff;
  OneClass f;
  f. //-7-
    ; //#7# ( "aFunc" "anInt" )
}

// Alias for struct in nested namespace, fully qualified
void func7()
{
  outer::StructNested f;
  f.//-8-
    ; //#8# ( "one" "two" )
}

// Alias for nested namespace
void func8()
{
  using namespace outerinner;
  StructNested f;
  AnotherStruct g;
  f.//-9-
    ; //#9# ( "one" "two" )
  g.//-10-
    ; //#10# ( "four" "three" )
}

// Check conventional namespace aliases
// - fully qualified -
void func9()
{
  alias_for_somestuff::OneClass c;
  c.//-11-
    ; //#11# ( "aFunc" "anInt" )
  alias_for_outerinner::AnotherStruct s;
  s. //-12-
    ; //#12# ( "four" "three" )
}

// - unqualified -
void func10()
{
  using namespace alias_for_somestuff;
  OneClass c2;
  c2.//-13-
    ; //#13# ( "aFunc" "anInt" )
  using namespace alias_for_outerinner;
  AnotherStruct s2;
  s2.//-14-
    ; //#14# ( "four" "three" )
}

// Completion on namespace aliases
void func11()
{
   alias_for_somestuff:://-15-
      ; //#15# ( "OneClass" "aStruct")
   alias_for_outerinner:://-16-
      ; //#16# ( "AnotherStruct" "StructNested" )
}

// make sure unfound using statements don't crash stuff.
using something::cantbe::Found;

void unfoundfunc()
{
  NotFound notfound; // Variable can't be found.

  notfound.//-17-
    ; //#17# ( )  Nothing here since this is an undefined class

}

// Using statements can depend on previous ones...

void acc_using()
{
  using namespace outer;
  // This is effectively like 'using namespace outer::inner'
  using namespace inner;

  StructNested sn;
  sn.//-18-
    ; //#18# ( "one" "two" )
}

// Check the same outside of function scope

using namespace outer;
using namespace inner;

void acc_using2()
{
  StructNested sn;
  sn.//-19-
    ; //#19# ( "one" "two" )
}

// Check if scope gets correctly generated, i.e., without us providing any
// hints in the form of an existing type

void check_scope()
{
  using namespace first;
  AAA//-20-
    ; //#20# ( "AAA1" "AAA2" )
}

void check_scope2()
{
  using namespace third;
  AAA//-21-
    ; //#21# ( "AAA1" "AAA2" "AAA3" )
}

// Make sure this also works not only in functions

namespace check_scope3 {
  using namespace first;
  AAA//-22-
  ; //#22# ( "AAA1" "AAA2" )
}
