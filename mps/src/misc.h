/* impl.h.misc: MISCELLANEOUS DEFINITIONS
 *
 * $HopeName: MMsrc!misc.h(trunk.2) $
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

/* MACRO BRACKETS
 *
 * M_BEGIN and M_END should be used to bracked ALL multi-statement macros.
 * This ensures that such macros can be used in all statement contexts,
 * including in the first branch of an if() statement which has an else
 * clause.
 */

#define M_BEGIN		do {
#define M_END		} while(0)


/* STRINGIZING MACROS
 *
 * M_STRING(<s>) converts <s> into a quoted string "<s>"
 * M_STR(<s>) does the same after substituting macros into <s>.
 */

#define M_STRING(s)	#s
#define M_STR(s)	M_STRING(s)


/* NULL STATEMENT (NOOP)
 *
 * Do not be tempted to use NULL or just semicolon as the null statement.
 * These items are dangerously ambigous and could cause subtle bugs if
 * misplaced.  NOOP is a macro which is guaranteed to cause an error if it
 * is not used in a statement context.
 */

#define NOOP    do {} while(0)


/* DECLARE PARAMETER UNUSED
 *
 * This macro supresses warnings about unused parameters.  It should be
 * applied to the parameter at the beginning of the body of the procedure.
 *
 * The cast to void appears to work for GCC, MSVC, and CodeWarrior.
 * It's a shame there's no way to ensure that the parameter won't be used.
 * We could scramble it, but that's undesirable in release versions.
 */

#define UNUSED(param)	((void)param)


/* BOOLEAN
 *
 * Using a boolean type in C is a tricky thing.  Non-zero values are
 * "true" but are not all equal to TRUE.  The Bool type is therefore
 * mostly defined so that the intention of the code is clearer.
 * Use with care.
 */

enum
{
  FALSE = 0,
  TRUE = 1
};
typedef int Bool;


/* ALIGN ADDRESSES
 *
 * AlignUp rounds up an Addr to the nearest N-boundary with N is a positive
 * power of two.  
 * IsAligned determines whether an address wrt a modulus.
 * IsPoT tests whether an integer is a positive power of two.
 */

extern Addr (AlignUp)(Addr pot, Addr i);
extern Bool (IsAligned)(Addr pot, Addr i);
extern Bool (IsPoT)(Addr pot);


/* BINARY LOG
 *
 * Returns the binary log (to the base 2) of x.  x must be a
 * non-negative power of two.
 */

extern unsigned int ilog2(unsigned long x);


/* PARENT STRUCTURE
 *
 * Given a pointer to a field of a structure this returns a pointer
 * to the main structure.
 */

#define PARENT(type, field, p) \
  ((type *)((char *)(p) - offsetof(type, field)))


/* OBJECT SIGNATURE TYPE
 *
 * Object signatures are values which are written into structures
 * to ensure that they remain intact and valid.
 */

typedef unsigned long Sig;
#define SigInvalid	((Sig)0x51915BAD)


/* Note: 31051 is prime */
#define MISC_CHECK (                                      \
                    (AlignUp(2048, 0) == 0) &&              \
                    !IsAligned(64, (unsigned) -1) &&      \
                    IsAligned(32, 0) &&                   \
                    (AlignUp(16, 1024) == 1024) &&          \
                    IsAligned(256, AlignUp(256, 31051)) &&  \
                    IsAligned(512, AlignUp(512, 31051)) &&  \
                    !IsAligned(1024, 31051) &&            \
                    !IsPoT(0) &&                        \
                    IsPoT(128) &&                       \
		    ilog2(1L) == 0 &&			  \
		    ilog2(256L) == 8 &&                   \
		    ilog2(65536L) == 16 &&                \
		    ilog2(131072L) == 17                  \
                   )

#endif /* misc_h */
