// Test using statements in C++

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

// Check convetional namespace aliases
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
