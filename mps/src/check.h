/* impl.h.check: ASSERTION INTERFACE
 *
 * $HopeName: !check.h(trunk.13) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * This header defines a family of AVER and NOTREACHED macros. The
 * macros should be used to instrument and annotate code with
 * invariants, and so provide both interface and internal consistency
 * checks.
 *
 * Non-obvious AVER statements should always be accompanied by a
 * comment.
 *
 * .disable: When assertions are disabled, AVER expands to something
 * which evaluates the condition but discards the result. Compilers
 * will throw the code away, but check its syntax.
 */

#ifndef check_h
#define check_h

#include "config.h"
#include "misc.h"
#include "mpslib.h"


/* AVER, AVERT -- MPM assertions
 *
 * AVER and AVERT are used to assert conditions within the MPM.
 * In white-hot varieties, all assertions compile away to nothing.
 */

#if defined(MPS_HOT_WHITE)

#define AVER(cond)                  DISCARD(cond)
#define AVERT(type, val)            DISCARD(type ## Check(val))
#define AVER_CRITICAL(cond)         DISCARD(cond)
#define AVERT_CRITICAL(type, val)   DISCARD(type ## Check(val))

#elif defined(MPS_HOT_RED)

#define AVER(cond)                  ASSERT(cond, #cond)
#define AVERT(type, val)            ASSERT(type ## Check(val), \
        "TypeCheck " #type ": " #val)
#define AVER_CRITICAL(cond)         DISCARD(cond)
#define AVERT_CRITICAL(type, val)   DISCARD(type ## Check(val))

#elif defined(MPS_COOL)

#define AVER(cond)                  ASSERT(cond, #cond)
#define AVERT(type, val)            ASSERT(type ## Check(val), \
        "TypeCheck " #type ": " #val)
#define AVER_CRITICAL(cond)         ASSERT(cond, #cond)
#define AVERT_CRITICAL(type, val)   ASSERT(type ## Check(val), \
        "TypeCheck " #type ": " #val)

#else

#error "No heat defined."

#endif

typedef void (*AssertHandler)(const char *cond, const char *id,
                              const char *file, unsigned line);
extern AssertHandler AssertInstall(AssertHandler handler);
extern AssertHandler AssertDefault(void);

extern void AssertFail1(const char *s);


#define ASSERT(cond, condstring) \
  BEGIN \
    if(cond) NOOP; else \
      AssertFail1(condstring "\n" __FILE__ "\n" STR(__LINE__)); \
  END


#define NOTREACHED \
  BEGIN \
    AssertFail1("unreachable statement" "\n" __FILE__ "\n" STR(__LINE__)); \
  END

#define CHECKC(cond, condstring) \
  BEGIN \
    if(cond) NOOP; else \
      AssertFail1(condstring "\n" __FILE__ "\n" STR(__LINE__)); \
  END


/* CHECKT -- check type simply
 *
 * Must be thread safe.  See design.mps.interface.c.thread-safety
 * and design.mps.interface.c.check.space.
 */

#define CHECKT(type, val)       ((val) != NULL && (val)->sig == type ## Sig)

#if defined(MPS_HOT_WHITE)

/* In white hot varieties, check methods should never be called.
 * To verify this, we have NOTREACHED in the expansions.
 */

#define CHECKS(type, val) \
  BEGIN DISCARD(CHECKT(type, val)); NOTREACHED; END

#define CHECKL(cond) \
  BEGIN DISCARD(cond); NOTREACHED; END

#define CHECKD(type, val) \
  BEGIN DISCARD(CHECKT(type, val)); NOTREACHED; END

#define CHECKU(type, val) \
  BEGIN DISCARD(CHECKT(type, val)); NOTREACHED; END

#elif defined(MPS_HOT_RED)

/* CHECKS -- Check Signature */
#define CHECKS(type, val)       CHECKC(CHECKT(type, val), \
	"SigCheck " #type ": " #val)

#define CHECKL(cond)       DISCARD(cond)
#define CHECKD(type, val)  DISCARD(CHECKT(type, val))
#define CHECKU(type, val)  DISCARD(CHECKT(type, val))

#elif defined(MPS_COOL)

/* CHECKS -- Check Signature */
#define CHECKS(type, val)       CHECKC(CHECKT(type, val), \
	"SigCheck " #type ": " #val)

/* CHECKL -- Check Local Invariant */
/* Could make this an expression using ?: */
#define CHECKL(cond) \
  BEGIN \
    switch(CheckLevel) { \
    case CheckNONE: \
      NOOP; \
      break; \
    case CheckSHALLOW: \
    case CheckDEEP: \
      CHECKC(cond, #cond); \
      break; \
    default: \
      NOTREACHED; \
      break; \
    } \
  END

/* CHECKD -- Check Down */
#define CHECKD(type, val) \
  BEGIN \
    switch(CheckLevel) { \
    case CheckNONE: \
      NOOP; \
      break; \
    case CheckSHALLOW: \
      CHECKC(CHECKT(type, val), \
             "SigCheck " #type ": " #val); \
      break; \
    case CheckDEEP: \
      CHECKC(type ## Check(val), \
             "TypeCheck " #type ": " #val); \
      break; \
    default: \
      NOTREACHED; \
      break; \
    } \
  END

/* CHECKU -- Check Up */
#define CHECKU(type, val) \
  BEGIN \
    switch(CheckLevel) { \
    case CheckNONE: \
      NOOP; \
      break; \
    case CheckSHALLOW: \
    case CheckDEEP: \
      CHECKC(CHECKT(type, val), \
             "SigCheck " #type ": " #val); \
      break; \
    default: \
      NOTREACHED; \
      break; \
    } \
  END

#else
#error "No heat defined."
#endif


/* CHECKLVALUE &c -- type compatibility checking
 *
 * .check.macros: The CHECK* macros use some C trickery to attempt to
 * verify that certain types and fields are equivalent.  They do not
 * do a complete job.  This trickery is justified by the security gained
 * in knowing that impl.h.mps matches the MPM.  See also
 * mail.richard.1996-08-07.09-49.  [This paragraph is intended to
 * satisfy rule.impl.trick.]
 */

#define CHECKLVALUE(lv1, lv2) \
  ((void)sizeof((lv1) = (lv2)), (void)sizeof((lv2) = (lv1)), TRUE)

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
