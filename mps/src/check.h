/* impl.h.check: ASSERTION INTERFACE
 *
 * $HopeName: MMsrc!check.h(trunk.17) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
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
 * which evaluates the condition but discards the result. Compilers
 * will throw the code away, but check its syntax.
 *
 * .trans.level-check: CheckLevel itself is not checked anywhere.
 */

#ifndef check_h
#define check_h

#include "config.h"
#include "misc.h"
#include "mpslib.h"


/* CheckLevel -- Control check method behaviour; see impl.c.assert */

extern unsigned CheckLevel;

enum {
  CheckNONE = 0,
  CheckSHALLOW = 1,
  CheckDEEP = 2
};


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


/* AssertHandler -- the assert handler */

typedef void (*AssertHandler)(const char *cond, const char *id,
                              const char *file, unsigned line);
extern AssertHandler AssertInstall(AssertHandler handler);
extern AssertHandler AssertDefault(void);


/* internals for actually asserting */

extern void AssertFail1(const char *s);

#define ASSERT(cond, condstring) \
  BEGIN \
    if(cond) NOOP; else \
      AssertFail1(condstring "\n" __FILE__ "\n" STR(__LINE__)); \
  END


/* NOTREACHED -- control should never reach this statement */

#if defined(MPS_HOT_WHITE)

#define NOTREACHED NOOP

#else

#define NOTREACHED \
  BEGIN \
    AssertFail1("unreachable statement" "\n" __FILE__ "\n" STR(__LINE__)); \
  END

#endif


/* CHECKT -- check type simply
 *
 * Must be thread safe.  See design.mps.interface.c.thread-safety
 * and design.mps.interface.c.check.space.
 */

#define CHECKT(type, val)       ((val) != NULL && (val)->sig == type ## Sig)


#if defined(MPS_HOT_WHITE)


#define CHECKS(type, val) DISCARD(CHECKT(type, val))
#define CHECKL(cond) DISCARD(cond)
#define CHECKD(type, val) DISCARD(CHECKT(type, val))
#define CHECKD_NOSIG(type, val) DISCARD((val) != NULL)
#define CHECKU(type, val) DISCARD(CHECKT(type, val))
#define CHECKU_NOSIG(type, val) DISCARD((val) != NULL)


#elif defined(MPS_HOT_RED)


#define CHECKS(type, val)       ASSERT(CHECKT(type, val), \
	"SigCheck " #type ": " #val)

#define CHECKL(cond)       DISCARD(cond)
#define CHECKD(type, val)  DISCARD(CHECKT(type, val))
#define CHECKD_NOSIG(type, val) DISCARD((val) != NULL)
#define CHECKU(type, val)  DISCARD(CHECKT(type, val))
#define CHECKU_NOSIG(type, val) DISCARD((val) != NULL)


#elif defined(MPS_COOL)


/* CHECKS -- Check Signature */

#define CHECKS(type, val)       ASSERT(CHECKT(type, val), \
	"SigCheck " #type ": " #val)

/* CHECKL -- Check Local Invariant
 *
 * Could make this an expression using ?:
 */

#define CHECKL(cond) \
  BEGIN \
    switch(CheckLevel) { \
    case CheckNONE: \
      NOOP; \
      break; \
    case CheckSHALLOW: \
    case CheckDEEP: \
      ASSERT(cond, #cond); \
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
      ASSERT(CHECKT(type, val), \
             "SigCheck " #type ": " #val); \
      break; \
    case CheckDEEP: \
      ASSERT(type ## Check(val), \
             "TypeCheck " #type ": " #val); \
      break; \
    } \
  END


/* CHECKD_NOSIG -- Check Down for a type with no signature */

#define CHECKD_NOSIG(type, val) \
  BEGIN \
    switch(CheckLevel) { \
    case CheckNONE: \
      NOOP; \
      break; \
    case CheckSHALLOW: \
      ASSERT((val) != NULL, \
             "NullCheck " #type ": " #val); \
      break; \
    case CheckDEEP: \
      ASSERT(type ## Check(val), \
             "TypeCheck " #type ": " #val); \
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
      ASSERT(CHECKT(type, val), \
             "SigCheck " #type ": " #val); \
      break; \
    } \
  END


/* CHECKU_NOSIG -- Check Up for a type with no signature */

#define CHECKU_NOSIG(type, val) \
  BEGIN \
    switch(CheckLevel) { \
    case CheckNONE: \
      NOOP; \
      break; \
    case CheckSHALLOW: \
    case CheckDEEP: \
      ASSERT((val) != NULL, \
             "NullCheck " #type ": " #val); \
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
