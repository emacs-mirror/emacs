/*  ==== MISCELLANEOUS DEFINITIONS ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1994,1995 Harlequin Group, all rights reserved
 *
 *  Small general things which are useful for C but aren't part of the
 *  memory manager itself.
 *
 *  Note
 *   1. The only reason that this file exists is that these things are
 *      too small and trivial to be put in their own headers.  If they
 *      ever become non-trivial they should be moved out.  richard 1994-08-25
 */

#ifndef misc_h
#define misc_h

#include "arch.h"
#include "types.h"
#include <stddef.h>


/*  == Null Statement ==
 *
 *  Do not be tempted to use NULL, or just semicolon as the null statement.
 *  These items are dangerously ambigous and could cause subtle bugs if
 *  misplaced.  NOOP is a macro which is guaranteed to cause an error if it
 *  is not used in a statement context.
 */

#define NOOP    do {} while(0)


/*  == Declare parameter unused ==
 *
 *  This macro supresses warnings about unused parameters.  It should be
 *  applied to the parameter at the beginning of the body of the procedure.
 *
 *  The cast to void appears to work for GCC, MSVC, and CodeWarrior.
 *  It's a shame there's no way to ensure that the parameter won't be used.
 *  We could scramble it, but that's undesirable in release versions.
 */

#define UNUSED(param)	((void)param)


/*  == Syntactic sugar ==
 *
 *  The unless and until macros are useful to avoid confusing De Morgan
 *  transformed conditions.  Instead of if(!foo || !bar) use
 *  unless(foo && bar).
 */

#define unless(c)	if(!(c))	/* converse of if */
#define until(c)	while(!(c))	/* converse of while */
#define forever		for(;;)		/* loop indefinitely */


/*  == Align Address ==
 *
 *  AlignUp rounds up an Addr to the nearest N-boundary with N is a positive
 *  power of two.  
 *  IsAligned determines whether an address wrt a modulus
 *  IsPoT tests whether an integer is a positive power of
 *  two.
 */

#ifndef DEBUG_NOINLINE
#define AlignUp(pot, i)   (((Addr)(i)+(Addr)(pot)-1)&~((Addr)(pot)-1))
#define IsAligned(pot, i) (((Addr)(i) & ((Addr)(pot)-1)) == 0)
#define IsPoT(pot)        ((pot)>0 && ((pot)&((pot)-1))==0)
#endif

extern Addr (AlignUp)(Addr pot, Addr i);
extern Bool (IsAligned)(Addr pot, Addr i);
extern Bool (IsPoT)(Addr pot);


/*  == INTEGER LOG ==
 *
 *  Returns the integer log (to the base 2) of x.  x must be a
 *  non-negative power of two.
 */

extern unsigned int ilog2(unsigned long x);


/*  == Parent Structure ==
 *
 *  Given a pointer to a field of a structure this returns
 *  a pointer to the main structure.
 */

#define PARENT(type, field, p) \
  ((type *)((char *)(p) - offsetof(type, field)))


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
