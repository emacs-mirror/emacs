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
