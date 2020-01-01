/* testvarnames.cpp --- semantic-ia-utest completion engine unit tests

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

struct independent {
  int indep_1;
  int indep_2;
};

struct independent var_indep_struct;

struct {
  int unnamed_1;
  int unnamed_2;
} var_unnamed_struct;

struct {
  int unnamed_3;
  int unnamed_4;
} var_un_2, var_un_3;

struct inlinestruct {
  int named_1;
  int named_2;
} var_named_struct;

struct inline2struct {
  int named_3;
  int named_4;
} var_n_2, var_n_3;

/* Structures with names that then declare variables
 * should also be completable.
 *
 * Getting this to work is the bugfix in semantic-c.el CVS v 1.122
 */
struct inlinestruct in_var1;
struct inline2struct in_var2;

/*
 * Structures (or any types) could have the same name as a variable.
 * Make sure we complete vars over types.
 *
 * See cedet-devel mailing list Dec 23, 2013 for details.
 */
struct varorstruct {};
int varorstruct;

int assigntovarorstruct;

int test_1(int var_arg1) {

  var_// -1-
    ; // #1# ("var_arg1" "var_indep_struct" "var_n_2" "var_n_3" "var_named_struct" "var_un_2" "var_un_3" "var_unnamed_struct")

  var_indep_struct.// -2-
    ; // #2# ( "indep_1" "indep_2" )

  var_unnamed_struct.// -3-
    ; // #3# ( "unnamed_1" "unnamed_2" )

  var_named_struct.// -4-
    ; // #4# ( "named_1" "named_2" )

  var_un_2.// -5-
    ; // #5# ( "unnamed_3" "unnamed_4" )
  var_un_3.// -6-
    ; // #6# ( "unnamed_3" "unnamed_4" )

  var_n_2.// -7-
    ; // #7# ( "named_3" "named_4" )
  var_n_3.// -8-
    ; // #8# ( "named_3" "named_4" )

  in_// -9-
    ; // #9# ( "in_var1" "in_var2" )

  in_var1.// -10-
    ; // #10# ( "named_1" "named_2")
  in_var2.// -11-
    ; // #11# ( "named_3" "named_4")

  varorstruct = assign// -12-
    ; // #12# ( "assigntovarorstruct" )
}
