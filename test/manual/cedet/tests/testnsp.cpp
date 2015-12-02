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

