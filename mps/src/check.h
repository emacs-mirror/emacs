/* impl.h.check: ASSERTION INTERFACE
 *
 * $HopeName: MMsrc!check.h(MMdevel_config.1) $
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


typedef void (*AssertHandler)(const char *cond, const char *id,
                              const char *file, unsigned line);
extern AssertHandler AssertInstall(AssertHandler handler);
extern AssertHandler AssertDefault(void);

extern void AssertFail(const char *cond, const char *id,
                       const char *file, unsigned line);

#define ASSERT(cond) \
  BEGIN \
    if(cond) NOOP; else \
      AssertFail(#cond, FileSrcIdStruct.hopename, \
                 FileSrcIdStruct.file, __LINE__); \
  END

#define NOCHECK(cond) \
  BEGIN \
    (void)sizeof(cond); \
  END

#define NOTREACHED \
  BEGIN \
    AssertFail("unreachable statement", \
               FileSrcIdStruct.hopename, FileSrcIdStruct.file, \
               __LINE__); \
  END

#ifdef CHECK_ASSERT
#define CHECKC(cond)    ASSERT(cond)
#else
#define CHECKC(cond)    BEGIN if(cond) NOOP; else return FALSE; END
#endif


/* CHECKT -- check type simply
 *
 * Must be thread safe.  See design.mps.interface.c.thread-safety
 * and design.mps.interface.c.check.space.
 */

#define CHECKT(type, val)       ((val) != NULL && (val)->sig == type ## Sig)


#if defined(CHECK_SHALLOW)
#define CHECKS(type, val)       CHECKC(CHECKT(type, val))
#define CHECKL(cond)            CHECKC(cond)
#define CHECKD(type, val)       CHECKC(CHECKT(type, val))
#define CHECKU(type, val)       CHECKC(CHECKT(type, val))
#elif defined(CHECK_DEEP)
#define CHECKS(type, val)       CHECKC(CHECKT(type, val))
#define CHECKL(cond)            CHECKC(cond)
#define CHECKD(type, val)       CHECKC(type ## Check(val))
#define CHECKU(type, val)       CHECKC(CHECKT(type, val))
#else /* neither CHECK_DEEP nor CHECK_SHALLOW */
#define CHECKS(type, val)       CHECKC(CHECKT(type, val))
#define CHECKL(cond)            NOCHECK(cond)
#define CHECKD(type, val)       NOCHECK(type ## Check(val))
#define CHECKU(type, val)       NOCHECK(CHECKT(type, val))
#endif /* CHECK_SHALLOW or CHECK_DEEP */

#endif /* check_h */
