// testtypedefs.cpp --- Sample with some fake bits out of std::string

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

// Thanks Ming-Wei Chang for these examples.

namespace std {
  template <T>class basic_string {
  public:
    void resize(int);
  };
}

typedef std::basic_string<char> mstring;

using namespace std;
typedef basic_string<char> bstring;

int main(){
  mstring a;
  a.// -1-
    ;
  // #1# ( "resize" )
  bstring b;
  // It doesn't work here.
  b.// -2-
    ;
  // #2# ( "resize" )
  return 0;
}

// ------------------

class Bar
{
public:
     void someFunc() {}
};

typedef Bar new_Bar;

template <class mytype>
class TBar
{
public:
     void otherFunc() {}
};

typedef TBar<char> new_TBar;

int main()
{
  new_Bar nb;
  new_TBar ntb;

  nb.// -3-
    ;
  // #3# ("someFunc")
  ntb.// -4-
    ;
  // #4# ("otherFunc")

  return 0;
}

// ------------------
// Example from Yupeng.

typedef struct epd_info {
     int a;
} epd_info_t;

static int epd_probe(struct platform_device *pdev)
{
     struct epd_info *db;
     epd_info_t db1;

     db.// -5-
       ; // #5# ("a")
     db1.// -6-
       ;// #6# ("a")

     return 1;
}

// ------------------
// Example from Michel LAFON-PUYO

typedef enum
{
   ENUM1,
   ENUM2
} e_toto;

typedef struct
{
   int field_a;
   int field_b;
} t_toto;

// Note: Error condition from anonymous types in a typedef
//       was that the first (ie - the enum) would be used in
//       place of the struct.
int func(void)
{
   t_toto t;
   t. // -7-
     ; // #7# ( "field_a" "field_b" )
   return 0;
}


// ------------------
// Example from Dixon Ryan


namespace NS2 {
   class MyClass {

   public:
      void myFunction() { }
   };
}

typedef class NS2::MyClass* MyClassHandle;

int dixon ( void ) {
  MyClassHandle mch = getMyClassHandle();
  NS2::MyClass* mcptr = getMyClassHandle();

  mcptr-> // -8-
    ; // #8# ( "myFunction" )
  mch-> // -    9- TODO bring over patch from SF
    ; // #9# ( "myFunction" )
  deleteMyClassHandle(mch);

  return 0;
}
