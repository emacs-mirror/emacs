/* testsppreplace.c --- unit test for CPP/SPP Replacement
   Copyright (C) 2007-2018 Free Software Foundation, Inc.

   Author: Eric M. Ludlam <eric@siege-engine.com>

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

/* TEST: The EMU keyword doesn't screw up the function defn. */
#define EMU
#define EMU2 /*comment*/
char EMU parse_around_emu EMU2 (EMU)
{
}

/* TEST: A simple word can be replaced in a definition. */
#define SUBFLOAT /* Some Float */ float
SUBFLOAT returnanfloat()
{
}

/* TEST: Punctuation an be replaced in a definition. */
#define COLON :
int foo COLON COLON bar ()
{
}

/* TEST: Multiple lexical characters in a definition */
#define SUPER mysuper::
int SUPER baz ()
{
}

/* TEST: Macro replacement. */
#define INT_FCN(name) int name (int in)

INT_FCN(increment) {
  return in+1;
}

/* TEST: Macro replacement with complex args */
#define P_(proto) ()

int myFcn1 P_((a,b));

#define P__(proto) proto

int myFcn2 P__((int a, int b));
int myFcn3 (int a, int b);

/* TEST: Multiple args to a macro. */
#define MULTI_ARGS(name, field1, field2, field3) struct name { int field1; int field2; int field3; }

MULTI_ARGS(ma_struct, moose, penguin, emu);

/* TEST: Macro w/ args, but no body. */
#define NO_BODY(name)

NO_BODY(Moose);

/* TEST: Not a macro with args, but close. */
#define NOT_WITH_ARGS     (moose)

int not_with_args_fcn NOT_WITH_ARGS
{
}

/* TEST: macro w/ continuation. */
#define WITH_CONT \
  continuation_symbol

int WITH_CONT () { };

/* TEST: macros in a macro - tail processing */
#define tail_with_args_and_long_name(a) (int a)
#define int_arg tail_with_args_and_long_name

int tail int_arg(q) {}

/* TEST: macros used improperly. */
#define tail_fail tail_with_args_and_long_name(q)

int tail_fcn tail_fail(q);

/* TEST: feature of CPP from LSD <lsdsgster@...> */
#define __gthrw_(name) __gthrw_ ## name

int __gthrw_(foo) (int arg1) { }

/* TEST: macros using macros */
#define macro_foo foo
#define mf_declare int macro_foo

mf_declare;

/* TEST: macros with args using macros */
#define Amacro(A) (int A)
#define mf_Amacro(B) int B Amacro(B)

mf_Amacro(noodle);

/* TEST: Double macro using the argument stack. */
#define MACRO0(name) int that_ ## name(int i);
#define MACRO1(name) int this_ ## name(int i);
#define MACRO2(name) MACRO0(name) MACRO1(name)

MACRO2(foo)

/* TEST: The G++ namespace macro hack.  Not really part of SPP. */
_GLIBCXX_BEGIN_NAMESPACE(baz)

  int bazfnc(int b) { }

_GLIBCXX_END_NAMESPACE;

_GLIBCXX_BEGIN_NESTED_NAMESPACE(foo,bar)

  int foo_bar_func(int a) { }

_GLIBCXX_END_NESTED_NAMESPACE;


/* TEST: The VC++ macro hack. */
_STD_BEGIN

  int inside_std_namespace(int a) { }

_STD_END

/* TEST: Recursion prevention.  CPP doesn't allow even 1 level of recursion. */
#define STARTMACRO MACROA
#define MACROA MACROB
#define MACROB MACROA

int STARTMACRO () {

}


/* END */
