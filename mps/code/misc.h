/* misc.h: MISCELLANEOUS DEFINITIONS
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 *
 * Small general things which are useful for C but aren't part of the
 * memory manager itself.  The only reason that this file exists is
 * that these things are too small and trivial to be put in their own
 * headers.  If they ever become non-trivial they should be moved out.
 */

#ifndef misc_h
#define misc_h


typedef int Bool;                       /* <design/type#.bool> */
enum BoolEnum {
  FALSE = 0,
  TRUE = 1
};


typedef int Compare;
enum CompareEnum {
  CompareLESS = -1,
  CompareEQUAL = 0,
  CompareGREATER = 1
};


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
  const char *scmid;
  const char *build_date;
  const char *build_time;
} SrcIdStruct;

#define SRCID(id, scmid) \
  static SrcIdStruct id ## FileSrcIdStruct = \
  {__FILE__, (scmid), __DATE__, __TIME__}; \
  extern SrcId id ## SrcId; \
  SrcId id ## SrcId = &id ## FileSrcIdStruct


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



/* RVALUE -- for method-style macros
 *
 * RVALUE is used to enclose the expansion of a macro that must not be
 * used as an lvalue, e.g. a getter method.
 */

#define RVALUE(expr) ((void)0, (expr))

/* NOOP -- null statement
 *
 * Do not be tempted to use NULL, or just semicolon as the null
 * statement.  These items are dangerously ambigous and could cause
 * subtle bugs if misplaced.  NOOP is a macro which is guaranteed to
 * cause an error if it is not used in a statement context.
 */

#define NOOP   do {} while(0)


/* STR -- expands into a string of the expansion of the argument
 *
 * E.g., if we have:
 *   #define a b
 * STR(a) will expand into "b".
 */

#define STR_(x) #x
#define STR(x) STR_(x)

/* NELEMS -- counts number of elements in an array
 *
 * NELEMS(a) expands into an expression that is the number
 * of elements in the array a.
 *
 * WARNING: expands a more than once (but only in the context of
 * sizeof, so does not cause double evaluation).
 */

#define NELEMS(a) (sizeof(a)/sizeof((a)[0]))


/* DISCARD_EXP -- discard an expression, but check syntax
 *
 * .discard: DISCARD_EXP uses sizeof so that the expression is not
 * evaluated and yet the compiler will check that it is a valid
 * expression. The conditional is compared with zero so it can
 * designate a bitfield object.
 */

#define DISCARD_EXP(expr) ((void)sizeof((expr)!=0))


/* DISCARD -- discards an expression in statement context, but checks syntax
 *
 * The argument is an expression; the expansion followed by a semicolon
 * is syntactically a statement (to avoid it being used in computation).
 */

#define DISCARD(expr) \
  BEGIN \
    DISCARD_EXP(expr); \
  END


/* DISCARD_STAT -- discards a statement, but checks syntax
 *
 * The argument is a statement; the expansion followed by a semicolon
 * is syntactically a statement.
 */

#define DISCARD_STAT(stat) \
  BEGIN \
    if (0) stat; \
  END


/* UNUSED -- declare parameter unused
 *
 * This macro supresses warnings about unused parameters.  It should be
 * applied to the parameter at the beginning of the body of the
 * procedure.
 *
 * The cast to void appears to work for GCC, MSVC, and CodeWarrior.
 * It's a shame there's no way to ensure that the parameter won't be
 * used.  We could scramble it, but that's undesirable in release
 * versions.
 */

#define UNUSED(param)   ((void)param)


/* UNUSED_POINTER, UNUSED_SIZE -- values for unused arguments
 *
 * Use these values for unused pointer, size closure arguments and
 * check them in the callback or visitor.
 *
 * Ensure that they have high bits set on 64-bit platforms for maximum
 * unusability.
 */
#define UNUSED_POINTER (Pointer)((Word)~0xFFFFFFFF | (Word)0xB60405ED) /* PointeR UNUSED */
#define UNUSED_SIZE ((Size)~0xFFFFFFFF | (Size)0x520405ED) /* SiZe UNUSED */


/* PARENT -- parent structure
 *
 * Given a pointer to a field of a structure this returns a pointer to
 * the main structure.  PARENT(foo_t, x, &(foo->x)) == foo.
 *
 * This macro is thread-safe, see <design/misc#.parent.thread-safe>.
 *
 * That intermediate (void *) is required to stop some compilers complaining
 * about alignment of 'type *' being greater than that of 'char *'.  Which
 * is true, but not a bug, since the result really is a pointer to a 'type'
 * struct.
 */

#define PARENT(type, field, p) \
  ((type *)(void *)((char *)(p) - offsetof(type, field)))



/* BOOLFIELD -- declare a Boolean bitfield
 *
 * A Boolean bitfield needs to be unsigned (not Bool), so that its
 * values are 0 and 1 (not 0 and -1), in order to avoid a sign
 * conversion (which would be a compiler error) when assigning TRUE to
 * the field.
 *
 * <design/type#.bool.bitfield>
 */
#define BOOLFIELD(name) unsigned name : 1


/* BITFIELD -- coerce a value into a bitfield
 *
 * This coerces value to the given width and type in a way that avoids
 * warnings from gcc -Wconversion about possible loss of data.
 */

#define BITFIELD(type, value, width) ((type)value & (((type)1 << (width)) - 1))
#define BOOLOF(v) BITFIELD(unsigned, (v), 1)


/* Bit Sets -- sets of integers in [0,N-1].
 *
 * Can be used on any unsigned integral type, ty.  These definitions
 * are _syntactic_, hence macroid, hence upper case
 * <design/guide.impl.c.naming#.capital.macro>.
 */

#define BS_EMPTY(ty)            ((ty)0)
#define BS_COMP(s)              (~(s))
#define BS_UNIV(ty)             BS_COMP(BS_EMPTY(ty))
#define BS_SINGLE(ty, i)        ((ty)1 << (i))
#define BS_IS_MEMBER(s, i)      (((s) >> (i)) & 1)
#define BS_UNION(s1, s2)        ((s1) | (s2))
#define BS_ADD(ty, s, i)        BS_UNION((s), BS_SINGLE(ty, (i)))
#define BS_INTER(s1, s2)        ((s1) & (s2))
#define BS_DIFF(s1, s2)         BS_INTER((s1), BS_COMP(s2))
#define BS_DEL(ty, s, i)        BS_DIFF((s), BS_SINGLE(ty, (i)))
#define BS_SUPER(s1, s2)        (BS_INTER((s1), (s2)) == (s2))
#define BS_SUB(s1, s2)          BS_SUPER((s2), (s1))
#define BS_IS_SINGLE(s)         (  ((s) != 0)  &&  (((s) & ((s)-1)) == 0)  )
#define BS_SYM_DIFF(s1, s2)     ((s1) ^ (s2))
#define BS_BITFIELD(ty, s)      BITFIELD(ty ## Set, (s), ty ## LIMIT)


#endif /* misc_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
