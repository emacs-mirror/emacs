/* testspp.cpp --- Semantic unit test for the C preprocessor

   Copyright (C) 2007-2020 Free Software Foundation, Inc.

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

int some_fcn (){}


#ifndef MOOSE
int pre_show_moose(){}
#endif

#ifdef MOOSE
int pre_dont_show_moose(){}
#endif

#if !defined(MOOSE)
int pre_show_moose_if(){}
#endif

#if defined(MOOSE)
int pre_dont_show_moose_if(){}
#endif

#define MOOSE

#if 0
int dont_show_function_if_0(){}
#endif

#if 1
int show_function_if_1(){}
#endif

#ifdef MOOSE
int moose_function(){}
#endif

#ifndef MOOSE
int dont_show_moose(){}
#endif

#if defined(MOOSE)
int moose_function_if(){}
#endif

#if !defined(MOOSE)
int dont_show_moose_if() {}
#endif

#undef MOOSE

#ifdef MOOSE
int no_handy_moose(){}
#endif

#ifndef MOOSE
int show_moose_else() {}
#else
int no_show_moose_else(){}
#endif


#ifdef MOOSE
int no_show_moose_else_2() {}
#else
int show_moose_else_2() {}
#endif

#if defined(MOOSE)
int no_show_moose_elif() {}
#elif !defined(MOOSE)
int show_moose_elif() {}
#else
int no_show_moose_elif_else() {}
#endif

#if defined(MOOSE)
int no_show_moose_if_elif_2() {}
#elif defined(COW)
int no_show_moose_elif_2() {}
#else
int show_moose_elif_else() {}
#endif
