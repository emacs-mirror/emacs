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
