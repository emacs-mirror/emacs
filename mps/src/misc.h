/* impl.h.misc: MISCELLANEOUS DEFINITIONS
 *
 * $HopeName: MMsrc!misc.h(trunk.4) $
 * Copyright (C) 1994,1995,1996 Harlequin Group, all rights reserved
 *
 * Small general things which are useful for C but aren't part of the
 * memory manager itself.  The only reason that this file exists is
 * that these things are too small and trivial to be put in their own
 * headers.  If they ever become non-trivial they should be moved out.
 */

#ifndef misc_h
#define misc_h

#include <stddef.h>


/* SrcId -- source identification
 *
 * Every C source file should start with a SRCID declaration to
 * create a local static source identification structure.  This
 * is used by other macros (particularly assertions) and can be
 * used to reverse engineer binary deliverables.
 */

typedef const struct SrcIdStruct *SrcId;
typedef const struct SrcIdStruct {
  const char *file;
  const char *hopename;
  const char *build_date;
  const char *build_time;
} SrcIdStruct;

#define SRCID(id, hopename) \
  static SrcIdStruct FileSrcIdStruct = \
  {__FILE__, hopename, __DATE__, __TIME__}; \
  SrcId id ## SrcId = &FileSrcIdStruct


/* BEGIN and END -- statement brackets
 *
 * BEGIN and END can be used to bracket multi-statement blocks which
 * will be followed by a semicolon, such as multi-statement macros.
 * BEGIN and END should be used to bracket ALL multi-statement macros.
 * The block, with its semicolon, still counts as a single statement.
 * This ensures that such macros can be used in all statement contexts,
 * including in the first branch of an if() statement which has an else
 * clause.
 */

#define BEGIN           do {
#define END             } while(0)


/* Bool -- boolean type
 *
 * Using a boolean type in C is a tricky thing.  Non-zero values are
 * "true" but are not all equal to TRUE.  The Bool type is therefore
 * mostly defined so that the intention of the code is clearer.
 * Use with care.
 */

typedef int Bool;               /* boolean type */
enum
{
  FALSE = 0,
  TRUE = 1
};


/* NOOP -- null statement
 *
 * Do not be tempted to use NULL, or just semicolon as the null
 * statement.  These items are dangerously ambigous and could cause
 * subtle bugs if misplaced.  NOOP is a macro which is guaranteed to
 * cause an error if it is not used in a statement context.
 */

#define NOOP            do {} while(0)


/* UNUSED -- declare parameter unused
 *
 * This macro supresses warnings about unused parameters.  It should be
 * applied to the parameter at the beginning of the body of the procedure.
 *
 * The cast to void appears to work for GCC, MSVC, and CodeWarrior.
 * It's a shame there's no way to ensure that the parameter won't be used.
 * We could scramble it, but that's undesirable in release versions.
 */

#define UNUSED(param)   ((void)param)


/* PARENT -- parent structure
 *
 * Given a pointer to a field of a structure this returns a pointer to
 * the main structure.  PARENT(foo_t, x, foo->x) == foo.
 *
 * This macro is thread-safe.  design.mps.misc.parent.thread-safe
 */

#define PARENT(type, field, p) \
  ((type *)((char *)(p) - offsetof(type, field)))


/* Object Signatures
 *
 * .sig: Signatures are magic numbers which are written into structures
 * when they are created and invalidated (by overwriting with
 * SigInvalid) when they are destroyed.  They provide a limited form
 * of run-time type checking and dynamic scope checking.
 *
 * .sig.form: The first three hex digits of signatures should be 519
 * (which resembles "SIG"), and should be chosen to be mnemonic when
 * viewed in hex, so that structures can be recognized visually when
 * debugging and dumping memory.
 */

typedef unsigned long Sig;
#define SigInvalid      ((Sig)0x51915BAD)


/* Res -- Result Code
 *
 * .res: See also impl.h.mps.res, impl.c.mpsi.check.res.
 */

typedef int Res;                /* result code type */
enum {
  ResOK = 0,                    /* success */
  ResFAIL,                      /* unspecified failure */
  ResRESOURCE,                  /* unable to obtain resources */
  ResMEMORY,                    /* unable to obtain memory */
  ResLIMIT,                     /* internal limitation reached */
  ResUNIMPL,                    /* unimplemented facility */
  ResIO                         /* system I/O error */
};


#endif /* misc_h */
