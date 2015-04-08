// Test NSP (Name space parent)
//
// Test dereferencing parents based on local parent scope.
//
// Derived from data David Engster provided.

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
   struct aaa
   {
     int xx;
   };
   struct bbb
   {
     struct aaa yy;
   };
}
int fun()
{
   using namespace ms_structs;
   struct bbb zz;
   int uu = zz.// -5-
     ; // #5# ( "yy" )
   int kk = zz.yy.// -6-
     ; // #6# ( "xx" )
}