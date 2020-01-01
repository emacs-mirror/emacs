/** testpolymorph.cpp --- A sequence of polymorphism examples.
 *
 * Copyright (C) 2009-2020 Free Software Foundation, Inc.
 *
 * Author: Eric M. Ludlam <zappo@gnu.org>
 *
 * This file is part of GNU Emacs.
 *
 * GNU Emacs is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GNU Emacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <cmath>

// Test 1 - Functions w/ prototypes
namespace proto {

  int pt_func1(int arg1);
  int pt_func1(int arg1) {
    return 0;
  }

}

// Test 2 - Functions w/ different arg lists.
namespace fcn_poly {

  int pm_func(void) {
    return 0;
  }
  int pm_func(int a) {
    return a;
  }
  int pm_func(char a) {
    return int(a);
  }
  int pm_func(double a) {
    return int(floor(a));
  }

}

// Test 3 - Methods w/ different arg lists.
class meth_poly {
public:
  int pm_meth(void) {
    return 0;
  }
  int pm_meth(int a) {
    return a;
  }
  int pm_meth(char a) {
    return int(a);
  }
  int pm_meth(double a) {
    return int(floor(a));
  }

};

// Test 4 - Templates w/ partial specifiers.
namespace template_partial_spec {
  template <typename T> class test
  {
  public:
    void doSomething(T t) { };
  };

  template <typename T> class test<T *>
  {
  public:
    void doSomething(T* t) { };
  };
}

// Test 5 - Templates w/ full specialization which may or may not share
// common functions.
namespace template_full_spec {
  template <typename T> class test
  {
  public:
    void doSomething(T t) { };
    void doSomethingElse(T t) { };
  };

  template <> class test<int>
  {
  public:
    void doSomethingElse(int t) { };
    void doSomethingCompletelyDifferent(int t) { };
  };
}

// Test 6 - Dto., but for templates with multiple parameters.
namespace template_multiple_spec {
  template <typename T1, typename T2> class test
  {
  public:
    void doSomething(T1 t) { };
    void doSomethingElse(T2 t) { };
  };

  template <typename T2> class test<int, T2>
  {
  public:
    void doSomething(int t) { };
    void doSomethingElse(T2 t) { };
  };

  template <> class test<float, int>
  {
  public:
    void doSomething(float t) { };
    void doSomethingElse(int t) { };
    void doNothing(void) { };
  };
}


// End of polymorphism test file.
