// test usings header file.

namespace moose {

  class Point;

  class MyClass;

}


namespace moose {

  class Point;

  class MyClass {
  public:
    MyClass() : fVal(0) {
    }

    ~MyClass() {};

    /**
     * fVal Accessors
     * @{
     */
    int getVal() const {
      return fVal;
    }
    void setVal(int Val) const {
      fVal = Val;
    }
    /**
     * @}
     */

  private:
    int fVal;
  };

}

namespace togglemoose {

  class MyOtherClass {
  public:
    int testToggle1();
    int testToggle2();
    int testToggle3();
  };
}

namespace deer {

  class Pickle;

};

// Code from Zhiqiu Kong

#ifndef BREAD_H
#define BREAD_H

namespace bread_name {
  class bread
  {
  public:
    void geta();
  private:
    int m_a;
    int m_b;
  };
}

#endif

// Code from David Engster
// Creating alias types through 'using' trickery

namespace somestuff {
  class OneClass {
  public:
    void aFunc();
    int anInt;
  };
  struct aStruct {
    int foo;
    int bar;
  };
}

namespace otherstuff {
  // make otherstuff::OneClass an alias for somestuff::OneClass
  using somestuff::OneClass;
}

namespace morestuff {
  // make morestuff an alias namespace for somestuff
  using namespace somestuff;
  // but hide aStruct with own type
  struct aStruct {
    int anotherFoo;
    int anotherBar;
  };
}

// We can also create an alias for an alias
namespace evenmorestuff {
  using otherstuff::OneClass;
}

// Now with nested namespaces
namespace outer {
  namespace inner {
    struct StructNested {
      int one;
      int two;
    };
    struct AnotherStruct {
      int three;
      int four;
    };
  }
}

// Elevate the first struct into 'outer'
// so that we can access it via 'outer::StructNested'
namespace outer {
  using outer::inner::StructNested;
}

// Create an alias for a nested namespace
namespace outerinner {
  // equivalent to 'namespace outerinner = outer::inner;'
  using namespace outer::inner;
}

// Create namespace alias
namespace alias_for_somestuff = somestuff;
// Same for nested namespace
namespace alias_for_outerinner = outer::inner;
