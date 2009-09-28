// Combinations of templates and structure inheritance.
//
// Created by Alex Ott.

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
