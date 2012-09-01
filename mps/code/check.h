/* check.h: ASSERTION INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .aver: This header defines a family of AVER and NOTREACHED macros.
 * These macros should be used to instrument and annotate code with
 * invariants, and so provide both interface and internal consistency
 * checks.
 *
 * .comment: Non-obvious AVER statements should always be accompanied by
 * a comment.
 *
 * .disable: When assertions are disabled, AVER expands to something
 * which contains the condition but discards the result. Compilers
 * will throw the code away, but check its syntax.
 *
 * .trans.level-check: CheckLevel itself is not checked anywhere.
 *
 * .careful: BE CAREFUL when changing this file.  It is easy to make mistakes
 * and change the checking level in a variety and thereby its performance
 * without realising it.  This has happened before.  Eyeball the preprocessor
 * output for each variety.  For example:
 *
 *   cc -E -DCONFIG_PROD_MPS -DCONFIG_VAR_WE trace.c
 *   cc -E -DCONFIG_PROD_MPS -DCONFIG_VAR_HE trace.c
 *   cc -E -DCONFIG_PROD_MPS -DCONFIG_VAR_CI trace.c
 *
 * Then look at TraceCheck to make sure checking is right, TraceAddWhite
 * for general assertions, and TraceFix for the critical path assertions.
 */

#ifndef check_h
#define check_h

#include "config.h"
#include "misc.h"
#include "mpslib.h"


/* ASSERT -- basic assertion
 *
 * The ASSERT macro is equivalent to the ISO C assert() except that it is
 * always defined, and uses the assertion handler from the MPS plinth, which
 * can be replaced by the client code.
 *
 * It is not intended for direct use within the MPS.  Use AVER and CHECK
 * macros, which can be controlled by both build and run-time configuration.
 */

#define ASSERT(cond, condstring) \
  BEGIN \
    if (cond) NOOP; else \
      mps_lib_assert_fail(condstring "\n" __FILE__ "\n" STR(__LINE__)); \
  END

#define ASSERT_TYPECHECK(type, val) \
  ASSERT(type ## Check(val), "TypeCheck " #type ": " #val)

#define ASSERT_NULLCHECK(type, val) \
  ASSERT((val) != NULL, "NullCheck " #type ": " #val)


/* CheckLevel -- control for check method behaviour
 *
 * When the MPS is build with AVER_AND_CHECK_ALL (in a "cool" variety) the
 * static variable CheckLevel controls the frequency and detail of
 * consistency checking on structures.
 *
 * FIXME: This should be initialised from an environment variable and have
 * an interface in mps.h.
 */

extern unsigned CheckLevel;

enum {
  CheckLevelMINIMAL = 0,  /* local sig check only */
  CheckLevelSHALLOW = 1,  /* local invariants, */
                          /* and adjacent (up, down) sig checks */
  CheckLevelDEEP = 2      /* local invariants, */
                          /* and adjacent up sig checks */
                          /* and recursive down full type checks */
};


/* AVER, AVERT -- MPM assertions
 *
 * AVER and AVERT are used to assert conditions in the code.  AVER checks
 * an expression.  AVERT checks that a value is of the correct type and
 * may perform consistency checks on the value.
 *
 * AVER and AVERT are on by default, and check conditions even in "hot"
 * varieties intended to work in production.  To avoid the cost of a check
 * in critical parts of the code, use AVER_CRITICAL and AVERT_CRITICAL,
 * but only when you've *proved* that this makes a difference to performance
 * that affects requirements.
 */

#if defined(AVER_AND_CHECK_NONE)

#define AVER(cond)                  DISCARD(cond)
#define AVERT(type, val)            DISCARD(type ## Check(val))

#else

#define AVER(cond)                  ASSERT(cond, #cond)
#define AVERT(type, val) \
  ASSERT(type ## Check(val), "TypeCheck " #type ": " #val)

#endif

#if defined(AVER_AND_CHECK_ALL)

/* FIXME: Find out whether these tests on checklevel have any performance
   impact and remove them if possible. */

#define AVER_CRITICAL(cond) \
  BEGIN \
    if (CheckLevel != CheckLevelMINIMAL) \
      ASSERT(cond, #cond); \
  END

#define AVERT_CRITICAL(type, val) \
  BEGIN \
    if (CheckLevel != CheckLevelMINIMAL) \
      ASSERT(type ## Check(val), "TypeCheck " #type ": " #val); \
  END

#else

#define AVER_CRITICAL               DISCARD
#define AVERT_CRITICAL(type, val)   DISCARD(type ## Check(val))

#endif


/* NOTREACHED -- control should never reach this statement
 *
 * This is a sort of AVER; it is equivalent to AVER(FALSE), but will produce
 * a more informative message.
 */

#if defined(AVER_AND_CHECK_NONE)

#define NOTREACHED NOOP

#else

#define NOTREACHED \
  BEGIN \
    mps_lib_assert_fail("unreachable code" "\n" __FILE__ "\n" STR(__LINE__)); \
  END

#endif


/* CHECKT -- check type simply
 *
 * Must be thread safe.  See <design/interface-c/#thread-safety>
 * and <design/interface-c/#check.space>.
 *
 * @@@@ This is a test, not a CHECK macro -- it does not assert.
 * It should be renamed TESTSIG.  RHSK 2006-12-13.
 */

#define CHECKT(type, val)       ((val) != NULL && (val)->sig == type ## Sig)


/* CHECKS -- Check Signature
 *
 * (if CheckLevel == CheckLevelMINIMAL, this is all we check)
 */

#if defined(AVER_AND_CHECK_NONE)
#define CHECKS(type, val)       DISCARD(CHECKT(type, val))
#else
#define CHECKS(type, val) \
  ASSERT(CHECKT(type, val), "SigCheck " #type ": " #val)
#endif


/* CHECKL, CHECKD, CHECKU -- local, "down", and "up" checks
 *
 * Each type should have a function defined called <type>Check that checks
 * the consistency of the type.  This function should return TRUE iff the
 * value passes consistency checks.  In general, it should assert otherwise,
 * but we allow for the possibility of returning FALSE in this case for
 * configuration adaptability.
 *
 * For structure types, the check function should:
 *
 *  - check its own signature with CHECKS
 *
 *  - check fields that it "owns" with CHECKL, like asserts
 *
 *  - check "down" values which are its "children" with CHEKCD
 *
 *  - check "up" values which are its "parents" with CHECKU.
 *
 * These various checks will be compiled out or compiled to be controlled
 * by CheckLevel.
 *
 * For example:
 *
 *     Bool MessageCheck(Message message)
 *     {
 *       CHECKS(Message, message);
 *       CHECKU(Arena, message->arena);
 *       CHECKD(MessageClass, message->class);
 *       CHECKL(RingCheck(&message->queueRing));
 *       CHECKL(MessageIsClocked(message) || (message->postedClock == 0));
 *       return TRUE;
 *     }
 *
 * The parent/child distinction depends on the structure, but in the MPS
 * the Arena has no parents, and has children which are Pools, which have
 * children which are Segments, etc.
 *
 * The important thing is to have a partial order on types so that recursive
 * checking will terminate.  When CheckLevel is set to DEEP, checking will
 * recurse into check methods for children, but will only do a shallow
 * signature check on parents, avoiding infinite regression.
 *
 * FIXME: Switching on every CHECK line doesn't compile very well, because
 * the compiler can't tell that CheckLevel won't change between function
 * calls and can't lift out the test.  Is there a better arrangement,
 * perhaps by reading CheckLevel into a local variable?
 */

#if defined(AVER_AND_CHECK_ALL)

#define CHECK_BY_LEVEL(minimal, shallow, deep) \
  BEGIN \
    switch (CheckLevel) { \
    case CheckLevelDEEP: deep; break; \
    case CheckLevelSHALLOW: shallow; break; \
    default: NOTREACHED; /* fall through */ \
    case CheckLevelMINIMAL: minimal; break; \
    } \
  END

#define CHECKL(cond) \
  CHECK_BY_LEVEL(NOOP, \
                 ASSERT(cond, #cond), \
                 ASSERT(cond, #cond))

#define CHECKD(type, val) \
  CHECK_BY_LEVEL(NOOP, \
                 CHECKS(type, val), \
                 ASSERT_TYPECHECK(type, val))

#define CHECKD_NOSIG(type, val) \
  CHECK_BY_LEVEL(NOOP, \
                 ASSERT_NULLCHECK(type, val), \
                 ASSERT_TYPECHECK(type, val))

#define CHECKU(type, val) \
  CHECK_BY_LEVEL(NOOP, \
                 CHECKS(type, val), \
                 CHECKS(type, val))

#define CHECKU_NOSIG(type, val) \
  CHECK_BY_LEVEL(NOOP, \
                 ASSERT_NULLCHECK(type, val), \
                 ASSERT_NULLCHECK(type, val))

#else /* AVER_AND_CHECK_ALL, not */

/* FIXME: This gives comparable performance to white-hot when compiling
   using mps.c and -O (to get check methods inlined), but is it a bit
   too minimal?  How much do we rely on check methods? */

#define CHECKL(cond)            DISCARD(cond)
#define CHECKD(type, val)       DISCARD(CHECKT(type, val))
#define CHECKD_NOSIG(type, val) DISCARD((val) != NULL)
#define CHECKU(type, val)       DISCARD(CHECKT(type, val))
#define CHECKU_NOSIG(type, val) DISCARD((val) != NULL)

#endif /* AVER_AND_CHECK_ALL */


/* CHECKLVALUE &c -- type compatibility checking
 *
 * .check.macros: The CHECK* macros use some C trickery to attempt to
 * verify that certain types and fields are equivalent.  They do not
 * do a complete job.  This trickery is justified by the security gained
 * in knowing that <code/mps.h> matches the MPM.  See also
 * mail.richard.1996-08-07.09-49.  [This paragraph is intended to
 * satisfy rule.impl.trick.]
 *
 * @@@@ These are tests, not CHECK macros -- they do not assert.
 * They should be renamed TESTTYPE etc.  RHSK 2006-12-13.
 */

/* compile-time check */
#define CHECKLVALUE(lv1, lv2) \
  ((void)sizeof((lv1) = (lv2)), (void)sizeof((lv2) = (lv1)), TRUE)

/* aims to test whether t1 and t2 are assignment-compatible */
#define CHECKTYPE(t1, t2) \
  (sizeof(t1) == sizeof(t2) && \
   CHECKLVALUE(*((t1 *)0), *((t2 *)0)))

#define CHECKFIELDAPPROX(s1, f1, s2, f2) \
  (sizeof(((s1 *)0)->f1) == sizeof(((s2 *)0)->f2) && \
   offsetof(s1, f1) == offsetof(s2, f2))

#define CHECKFIELD(s1, f1, s2, f2) \
  (CHECKFIELDAPPROX(s1, f1, s2, f2) && \
   CHECKLVALUE(((s1 *)0)->f1, ((s2 *)0)->f2))


#endif /* check_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
