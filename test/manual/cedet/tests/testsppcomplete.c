/* testesppcomplete.cpp --- semantic-ia-utest completion engine unit tests

   Copyright (C) 2008-2020 Free Software Foundation, Inc.

   Author: Eric M. Ludlam <zappo@gnu.org>

   This file is part of GNU Emacs.

   GNU Emacs is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU Emacs is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
*/

/* Example provided by Hannes Janetzek */

struct Test { int test; };

#define BLA(_type) \
  _type *bla = (_type*) malloc(sizeof(_type));

#define BLUB(_type)				\
  (_type*)malloc(sizeof(_type));

#define FOO(_type)				\
  _type *foo = BLUB(_type);

#define BAR(_type)				\
  _type *bar = (*_type)BLUB(_type);

int main(int argc, char *argv[]) {
  BLA(Test);
  bla->// -1-
    ; // #1# ( "test" )

  FOO(Test);
  foo->// -2-
    ; // #2# ( "test" )

  BAR(Test);
  bar->// -3-
    ; // #3# ( "test" )
}

/* Test symref and macros together. */

// This function exists only so we can have a comment in a tag with this name.
void function_with_macro_name ()
// %1% ( ( "testsppcomplete.c" ) ( "function_with_macro_name" "function_with_macro_name" "use_macro") )
// Note: fwmn is in twice, once for function, and once for the constant macro below.
{
}

#define function_with_macro_name 1

int use_macro () {
  int a = function_with_macro_name;
}
