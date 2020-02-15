//  testlocalvars.java --- Semantic unit test for Java

// Copyright (C) 2009-2020 Free Software Foundation, Inc.

//  Author: Eric M. Ludlam <zappo@gnu.org>

//  This file is part of GNU Emacs.

//  GNU Emacs is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.

//  GNU Emacs is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.

//  You should have received a copy of the GNU General Public License
//  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


class foo {
  foo *member;
  char anArray[10];
};

void func()
{
  foo local1;
  foo* local2 = localvar.member;
  foo* local3 = new foo();
  foo local4[10];
  char local5[5] = {'1','2','3','4','5'};
  char *local6 = "12345";
  char local7 = local.anArray[0];
  char local8 = true ? 10 : 11 ;

  // Check that all of the above was parsed
  local//-1-
    ; //#1# ("local1" "local2" "local3" "local4" "local5" "local6" "local7" "local8" )

  local1.//-2-
    ; //#2# ("anArray" "member")

  local2->//-3-
    ; //#3# ("anArray" "member")

  local3->//-4-
    ; //#4# ("anArray" "member")

  local4[0].//-5-
    ; //#5# ("anArray" "member")
}
