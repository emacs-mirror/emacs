/* check.h: ASSERTION INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
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
 *   cc -E -DCONFIG_VAR_RASH trace.c
 *   cc -E -DCONFIG_VAR_HOT trace.c
 *   cc -E -DCONFIG_VAR_COOL trace.c
 *
 * Then look at TraceCheck to make sure checking is right, TraceAddWhite
 * for general assertions, and TraceFix for the critical path assertions.
 */

#ifndef check_h
#define check_h

#include "config.h"
#include "misc.h"
#include "mpslib.h"
#include "protocol.h"


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
    if (LIKELY(cond)) NOOP; else \
      mps_lib_assert_fail(MPS_FILE, __LINE__, (condstring)); \
  END

#define ASSERTP(cond, condstring, dflt) \
  (LIKELY(cond) ? (dflt) : \
   mps_lib_assert_fail_expr(MPS_FILE, __LINE__, condstring, dflt))

#define ASSERT_ISTYPE(type, val) (type ## Check(val))
#define ASSERT_TYPECHECK(type, val) \
  ASSERT(ASSERT_ISTYPE(type, val), "TypeCheck " #type ": " #val)

#define ASSERT_ISCLASS(class, val) (class ## Check(CouldBeA(class, val)))
#define ASSERT_CLASSCHECK(class, val) \
  ASSERT(ASSERT_ISCLASS(class, val), "ClassCheck " #class ": " #val)

#define ASSERT_NULLCHECK(type, val) \
  ASSERT((val) != NULL, "NullCheck " #type ": " #val)


/* CheckLevel -- control for check method behaviour
 *
 * When the MPS is build with AVER_AND_CHECK_ALL (in a "cool" variety) the
 * static variable CheckLevel controls the frequency and detail of
 * consistency checking on structures.
 *
 * By default, CHECKLEVEL is defined to a static value in config.h, though
 * it can be overridden on the compiler command line, e.g.
 *   cc -DCHECKLEVEL=CheckLevelSHALLOW ...
 *
 * However, if CHECKLEVEL_DYNAMIC is defined we use a variable to control
 * the level of checking.  The run-time overhead for this is quite high
 * (observed double run-time on amcss when the variable is set to SHALLOW).
 * CHECKLEVEL_DYNAMIC should be set to the initial level for the variable,
 * which is in mpm.c.
 *
 * In general, it's better to adjust the check level by defining CHECKLEVEL
 * but this is intended to meet the case where a run-time adjustable
 * checking level is required -- where recompilation or relinking is
 * undesirable or impossible.
 *
 * TODO: Should also allow the check level variable to come from an
 * environment variable.
 */

enum {
  CheckLevelMINIMAL = 0,  /* local sig check only */
  CheckLevelSHALLOW = 1,  /* local invariants, */
                          /* and adjacent (up, down) sig checks */
  CheckLevelDEEP = 2      /* local invariants, */
                          /* and adjacent up sig checks */
                          /* and recursive down full type checks */
};

#ifdef CHECKLEVEL_DYNAMIC
extern unsigned CheckLevel;
#undef CHECKLEVEL
#define CHECKLEVEL CheckLevel
#endif


/* AVER, AVERT, AVERC, AVERP -- MPM assertions
 *
 * AVER and friends are used to assert conditions in the code.
 *
 * AVER checks an expression.
 *
 * AVERT checks that a value is of the correct type and may perform
 * consistency checks on the value by calling a check function.
 *
 * AVERC checks that a value is of the correct class (including
 * subclasses) and may perform consistency checks on the value by
 * calling a check function.
 *
 * AVERP checks an expression but is itself a void * expression, and
 * so can be used in expression macros.
 *
 * AVER etc. are on by default, and check conditions even in "hot"
 * varieties intended to work in production.  To avoid the cost of a
 * check in critical parts of the code, use AVER_CRITICAL etc., but
 * only when you've *proved* that this makes a difference to
 * performance that affects requirements.
 */

#if defined(AVER_AND_CHECK_NONE)

#define AVER(cond)                  DISCARD(cond)
#define AVERT(type, val)            DISCARD(ASSERT_ISTYPE(type, val))
#define AVERC(class, val)           DISCARD(ASSERT_ISCLASS(class, val))
#define AVERP(cond, dflt)           (DISCARD_EXP(cond), dflt)
#define AVERPC(cond, condstring, dflt) (DISCARD_EXP(cond), dflt)

#else

#define AVER(cond)                  ASSERT(cond, #cond)
#define AVERT                       ASSERT_TYPECHECK
#define AVERC                       ASSERT_CLASSCHECK
#define AVERP(cond, dflt)           ASSERTP(cond, #cond, dflt)
#define AVERPC                      ASSERTP

#endif

#if defined(AVER_AND_CHECK_ALL)

#define AVER_CRITICAL(cond)         ASSERT(cond, #cond)
#define AVERT_CRITICAL              ASSERT_TYPECHECK
#define AVERC_CRITICAL              ASSERT_CLASSCHECK
#define AVERP_CRITICAL(cond, dflt)  ASSERTP(cond, #cond, dflt)
#define AVERPC_CRITICAL             ASSERTP

#else

#define AVER_CRITICAL               DISCARD
#define AVERT_CRITICAL(type, val)   DISCARD(ASSERT_ISTYPE(type, val))
#define AVERC_CRITICAL(class, val)  DISCARD(ASSERT_ISCLASS(class, val))
#define AVERP_CRITICAL(cond, dflt)  (DISCARD_EXP(cond), dflt)
#define AVERPC_CRITICAL(cond, condstring, dflt) (DISCARD_EXP(cond), dflt)

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
    mps_lib_assert_fail(__FILE__, __LINE__, "unreachable code"); \
  END

#endif


/* TESTT -- check type simply
 *
 * Must be thread safe.  See <design/interface-c/#thread-safety>
 * and <design/interface-c/#check.space>.
 */

#define TESTT(type, val)       ((val) != NULL && (val)->sig == type ## Sig)


/* TESTC -- check class simply
 *
 * TODO: Does this need to be thread safe like TESTT?
 */

#define TESTC(class, val)       ((val) != NULL && IsA(class, val))


/* CHECKS, CHECKC -- Check Signature, Check Class
 *
 * (if CHECKLEVEL == CheckLevelMINIMAL, this is all we check)
 */

#if defined(AVER_AND_CHECK_NONE)
#define CHECKS(type, val)       DISCARD(TESTT(type, val))
#define CHECKC(class, val)      DISCARD(MustBeA(class, val))
#else
#define CHECKS(type, val) \
  ASSERT(TESTT(type, val), "SigCheck " #type ": " #val)
#define CHECKC(class, val) \
  ASSERT(TESTC(class, val), "ClassCheck " #class ": " #val)
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
 *  - check "down" values which are its "children" with CHECKD
 *
 *  - check "up" values which are its "parents" with CHECKU.
 *
 * These various checks will be compiled out or compiled to be controlled
 * by CHECKLEVEL.
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
 * checking will terminate.  When CHECKLEVEL is set to DEEP, checking will
 * recurse into check methods for children, but will only do a shallow
 * signature check on parents, avoiding infinite regression.
 */

#if defined(AVER_AND_CHECK_ALL)

#define CHECK_BY_LEVEL(minimal, shallow, deep) \
  BEGIN \
    switch (CHECKLEVEL) { \
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

#define CHECKD_CLASS(class, val) \
  CHECK_BY_LEVEL(NOOP, \
                 CHECKC(class, val) \
                 ASSERT_CLASSCHECK(class, val))

#define CHECKU(type, val) \
  CHECK_BY_LEVEL(NOOP, \
                 CHECKS(type, val), \
                 CHECKS(type, val))

#define CHECKU_NOSIG(type, val) \
  CHECK_BY_LEVEL(NOOP, \
                 ASSERT_NULLCHECK(type, val), \
                 ASSERT_NULLCHECK(type, val))

#else /* AVER_AND_CHECK_ALL, not */

/* TODO: This gives comparable performance to RASH when compiling
   using mps.c and -O2 (to get check methods inlined), but is it a bit
   too minimal?  How much do we rely on check methods? */

#define CHECKL(cond)             DISCARD(cond)
#define CHECKD(type, val)        DISCARD(TESTT(type, val))
#define CHECKD_NOSIG(type, val)  DISCARD((val) != NULL)
#define CHECKD_CLASS(class, val) DISCARD((val) != NULL)
#define CHECKU(type, val)        DISCARD(TESTT(type, val))
#define CHECKU_NOSIG(type, val)  DISCARD((val) != NULL)

#endif /* AVER_AND_CHECK_ALL */


/* COMPAT* -- type compatibility checking
 *
 * .check.macros: The COMPAT* macros use some C trickery to attempt to
 * verify that certain types and fields are equivalent. They do not do
 * a complete job. This trickery is justified by the security gained
 * in knowing that <code/mps.h> matches the MPM. See
 * <design/interface-c/#check.types>. [This paragraph is intended to
 * satisfy rule.impl.trick.]
 */

/* compile-time check */
#define COMPATLVALUE(lv1, lv2) \
  ((void)sizeof((lv1) = (lv2)), (void)sizeof((lv2) = (lv1)), TRUE)

/* aims to test whether t1 and t2 are assignment-compatible */
#define COMPATTYPE(t1, t2) \
  (sizeof(t1) == sizeof(t2) && \
   COMPATLVALUE(*((t1 *)0), *((t2 *)0)))

#define COMPATFIELDAPPROX(s1, f1, s2, f2) \
  (sizeof(((s1 *)0)->f1) == sizeof(((s2 *)0)->f2) && \
   offsetof(s1, f1) == offsetof(s2, f2))

#define COMPATFIELD(s1, f1, s2, f2) \
  (COMPATFIELDAPPROX(s1, f1, s2, f2) && \
   COMPATLVALUE(((s1 *)0)->f1, ((s2 *)0)->f2))


/* NONNEGATIVE -- test that value is greater than or equal to zero
 *
 * We'd like to write "x >= 0" but when x belongs to an unsigned
 * integral type then this results in a "comparison of unsigned
 * expression >= 0 is always true" warning from GCC if -Wextra is
 * specified. We also don't want to remove these assertions because
 * they protect us against errors if the type of x should ever be
 * changed to a signed type on some platform.
 *
 * Note that this macro evaluates its argument twice.
 */
#define NONNEGATIVE(x) ((x) == 0 || (x) > 0)

#endif /* check_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
