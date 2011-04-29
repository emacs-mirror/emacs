/** testsppcond.cpp --- Test CPP Conditional handling via hideif
 *
 * Copyright (C) 2010 Eric M. Ludlam
 *
 * Author: Eric M. Ludlam <eric@siege-engine.com>
 * X-RCS: $Id: testsppcond.cpp,v 1.1 2010-01-23 02:36:28 zappo Exp $
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

/*
 * DEVELOPER NOTES:
 *
 * When working in this file, enable semantic-decoration-mode, and
 * always create small functions.  In this case, it will overline
 * everything labeled PASS, and nothing labeled FAIL.  This makes it
 * easy to see what the result will be without running the unit test.
 */

#define TRUE 1
#define FALSE 0
#define TRUE1 TRUE
#define FALSE1 FALSE
#define TRUE2 TRUE1

#define THREE 3

#define JUSTDEFINED
#define UNDEFINED
#undef UNDEFINED

#undef UNDEFUNDEFINED

// *** TEST SOME IFDEFS

// This could fail if FALSE was interpreted as false, not defined.
#ifdef FALSE
int pass_ifdef1() {}
#else
int fail_ifdef1() {}
#endif

#ifdef TRUE
int pass_ifdef2() {}
#else
int fail_ifdef2() {}
#endif

// This fails if JUSTDEFINED was interpreted as false
#ifdef JUSTDEFINED
int pass_ifdef3() {}
#else
int fail_ifdef3() {}
#endif

#ifdef NOTDEFINED
int fail_ifdef4() {}
#else
int pass_ifdef4() {}
#endif

#ifdef UNDEFINED
int fail_ifdef5() {}
#else
int pass_ifdef5() {}
#endif

// *** TEST SOME if defined() cases.
#if defined(FALSE)
int pass_if_def1() {}
#else
int fail_if_def1() {}
#endif

#if defined(TRUE)
int pass_if_def2() {}
#else
int fail_if_def2() {}
#endif

#if defined(JUSTDEFINED)
int pass_if_def3() {}
#else
int fail_if_def3() {}
#endif

#if defined(NOTDEFINED)
int fail_if_def4() {}
#else
int pass_if_def4() {}
#endif

#if defined(UNDEFINED)
int fail_if_def5() {}
#else
int pass_if_def5() {}
#endif

// *** TEST if evaluations to 0/1

#if TRUE
int pass_true() {}
#else
int fail_true() {}
#endif

#if TRUE1
int pass_true1() {}
#else
int fail_true1() {}
#endif

#if TRUE2
int pass_true2() {}
#else
int fail_true2() {}
#endif

#if FALSE
int fail_false() {}
#else
int pass_false() {}
#endif

#if FALSE1
int fail_false1() {}
#else
int pass_false1() {}
#endif

// *** TEST some if comparisons.
#if TRUE == TRUE1
int pass_eq_true1() {}
#else
int fail_eq_true1() {}
#endif

#if TRUE == TRUE2
int pass_eq_true2() {}
#else
int fail_eq_true2() {}
#endif

#if FALSE == FALSE1
int pass_eq_false1() {}
#else
int fail_eq_false1() {}
#endif

#if TRUE != FALSE
int pass_neq() {}
#else
int fail_neq() {}
#endif

// *** TEST simple math.
#if TRUE+TRUE==2
int pass_true_2() {}
#else
int fail_true_2() {}
#endif

#if TRUE+TRUE-FALSE==2
int pass_true_false_2() {}
#else
int fail_true_false_2() {}
#endif

#if TRUE+TRUE==3
int fail_true_n3() {}
#else
int pass_true_n3() {}
#endif

// *** TEST boolean &&
#if defined(FALSE) && TRUE
int pass_and1() {}
#else
int fail_and1() {}
#endif

#if defined(FALSE) && TRUE && TRUE+FALSE==1
int pass_and2() {}
#else
int fail_and2() {}
#endif

#if defined(TRUE) && FALSE && TRUE+FALSE==1
int fail_and3() {}
#else
int pass_and3() {}
#endif

// *** TEST boolean ||
#if defined(FALSE) || TRUE
int pass_or1() {}
#else
int fail_or1() {}
#endif

#if defined(FALSE) || TRUE || TRUE+FALSE==1
int pass_or2() {}
#else
int fail_or2() {}
#endif

#if defined(TRUE) || FALSE || TRUE+FALSE==1
int pass_or3() {}
#else
int fail_or3() {}
#endif


/* END */
