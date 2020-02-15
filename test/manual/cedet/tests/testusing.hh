// testusing.hh --- semantic-ia-utest completion engine unit tests

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

// Namespace which pulls in one of its own nested namespaces
namespace first {
  class AAA1;
  namespace second {
    class AAA2;
  }
  // Elevate nested namespace into first one
  using namespace second;
}

namespace third {
  using namespace first;
  class AAA3;
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
