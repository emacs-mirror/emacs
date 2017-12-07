/* Fundamental definitions for GNU Emacs Lisp interpreter. -*- coding: utf-8 -*-

Copyright (C) 1985-1987, 1993-1995, 1997-2017 Free Software Foundation,
Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_LISP_H
#define EMACS_LISP_H

#include <alloca.h>
#include <setjmp.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>

#include <intprops.h>
#include <verify.h>

INLINE_HEADER_BEGIN

/* Define a TYPE constant ID as an externally visible name.  Use like this:

      DEFINE_GDB_SYMBOL_BEGIN (TYPE, ID)
      # define ID (some integer preprocessor expression of type TYPE)
      DEFINE_GDB_SYMBOL_END (ID)

   This hack is for the benefit of compilers that do not make macro
   definitions or enums visible to the debugger.  It's used for symbols
   that .gdbinit needs.  */

#define DECLARE_GDB_SYM(type, id) type const id EXTERNALLY_VISIBLE
#ifdef MAIN_PROGRAM
# define DEFINE_GDB_SYMBOL_BEGIN(type, id) DECLARE_GDB_SYM (type, id)
# define DEFINE_GDB_SYMBOL_END(id) = id;
#else
# define DEFINE_GDB_SYMBOL_BEGIN(type, id) extern DECLARE_GDB_SYM (type, id)
# define DEFINE_GDB_SYMBOL_END(val) ;
#endif

/* The ubiquitous max and min macros.  */
#undef min
#undef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

/* Number of elements in an array.  */
#define ARRAYELTS(arr) (sizeof (arr) / sizeof (arr)[0])

/* Number of bits in a Lisp_Object tag.  */
DEFINE_GDB_SYMBOL_BEGIN (int, GCTYPEBITS)
#define GCTYPEBITS 3
DEFINE_GDB_SYMBOL_END (GCTYPEBITS)

/* EMACS_INT - signed integer wide enough to hold an Emacs value
   EMACS_INT_WIDTH - width in bits of EMACS_INT
   EMACS_INT_MAX - maximum value of EMACS_INT; can be used in #if
   pI - printf length modifier for EMACS_INT
   EMACS_UINT - unsigned variant of EMACS_INT */
#ifndef EMACS_INT_MAX
# if INTPTR_MAX <= 0
#  error "INTPTR_MAX misconfigured"
# elif INTPTR_MAX <= INT_MAX && !defined WIDE_EMACS_INT
typedef int EMACS_INT;
typedef unsigned int EMACS_UINT;
enum { EMACS_INT_WIDTH = INT_WIDTH, EMACS_UINT_WIDTH = UINT_WIDTH };
#  define EMACS_INT_MAX INT_MAX
#  define pI ""
# elif INTPTR_MAX <= LONG_MAX && !defined WIDE_EMACS_INT
typedef long int EMACS_INT;
typedef unsigned long EMACS_UINT;
enum { EMACS_INT_WIDTH = LONG_WIDTH, EMACS_UINT_WIDTH = ULONG_WIDTH };
#  define EMACS_INT_MAX LONG_MAX
#  define pI "l"
# elif INTPTR_MAX <= LLONG_MAX
typedef long long int EMACS_INT;
typedef unsigned long long int EMACS_UINT;
enum { EMACS_INT_WIDTH = LLONG_WIDTH, EMACS_UINT_WIDTH = ULLONG_WIDTH };
#  define EMACS_INT_MAX LLONG_MAX
/* MinGW supports %lld only if __USE_MINGW_ANSI_STDIO is non-zero,
   which is arranged by config.h, and (for mingw.org) if GCC is 6.0 or
   later and the runtime version is 5.0.0 or later.  Otherwise,
   printf-like functions are declared with __ms_printf__ attribute,
   which will cause a warning for %lld etc.  */
#  if defined __MINGW32__						\
  && (!defined __USE_MINGW_ANSI_STDIO					\
      || (!defined MINGW_W64						\
	  && !(GNUC_PREREQ (6, 0, 0) && __MINGW32_MAJOR_VERSION >= 5)))
#   define pI "I64"
#  else	 /* ! MinGW */
#   define pI "ll"
#  endif
# else
#  error "INTPTR_MAX too large"
# endif
#endif

/* Number of bits to put in each character in the internal representation
   of bool vectors.  This should not vary across implementations.  */
enum {  BOOL_VECTOR_BITS_PER_CHAR =
#define BOOL_VECTOR_BITS_PER_CHAR 8
        BOOL_VECTOR_BITS_PER_CHAR
};

/* An unsigned integer type representing a fixed-length bit sequence,
   suitable for bool vector words, GC mark bits, etc.  Normally it is size_t
   for speed, but on weird platforms it is unsigned char and not all
   its bits are used.  */
#if BOOL_VECTOR_BITS_PER_CHAR == CHAR_BIT
typedef size_t bits_word;
# define BITS_WORD_MAX SIZE_MAX
enum { BITS_PER_BITS_WORD = SIZE_WIDTH };
#else
typedef unsigned char bits_word;
# define BITS_WORD_MAX ((1u << BOOL_VECTOR_BITS_PER_CHAR) - 1)
enum { BITS_PER_BITS_WORD = BOOL_VECTOR_BITS_PER_CHAR };
#endif
verify (BITS_WORD_MAX >> (BITS_PER_BITS_WORD - 1) == 1);

/* printmax_t and uprintmax_t are types for printing large integers.
   These are the widest integers that are supported for printing.
   pMd etc. are conversions for printing them.
   On C99 hosts, there's no problem, as even the widest integers work.
   Fall back on EMACS_INT on pre-C99 hosts.  */
#ifdef PRIdMAX
typedef intmax_t printmax_t;
typedef uintmax_t uprintmax_t;
# define pMd PRIdMAX
# define pMu PRIuMAX
#else
typedef EMACS_INT printmax_t;
typedef EMACS_UINT uprintmax_t;
# define pMd pI"d"
# define pMu pI"u"
#endif

/* Use pD to format ptrdiff_t values, which suffice for indexes into
   buffers and strings.  Emacs never allocates objects larger than
   PTRDIFF_MAX bytes, as they cause problems with pointer subtraction.
   In C99, pD can always be "t"; configure it here for the sake of
   pre-C99 libraries such as glibc 2.0 and Solaris 8.  */
#if PTRDIFF_MAX == INT_MAX
# define pD ""
#elif PTRDIFF_MAX == LONG_MAX
# define pD "l"
#elif PTRDIFF_MAX == LLONG_MAX
# define pD "ll"
#else
# define pD "t"
#endif

/* Extra internal type checking?  */

/* Define Emacs versions of <assert.h>'s 'assert (COND)' and <verify.h>'s
   'assume (COND)'.  COND should be free of side effects, as it may or
   may not be evaluated.

   'eassert (COND)' checks COND at runtime if ENABLE_CHECKING is
   defined and suppress_checking is false, and does nothing otherwise.
   Emacs dies if COND is checked and is false.  The suppress_checking
   variable is initialized to 0 in alloc.c.  Set it to 1 using a
   debugger to temporarily disable aborting on detected internal
   inconsistencies or error conditions.

   In some cases, a good compiler may be able to optimize away the
   eassert macro even if ENABLE_CHECKING is true, e.g., if XSTRING (x)
   uses eassert to test STRINGP (x), but a particular use of XSTRING
   is invoked only after testing that STRINGP (x) is true, making the
   test redundant.

   eassume is like eassert except that it also causes the compiler to
   assume that COND is true afterwards, regardless of whether runtime
   checking is enabled.  This can improve performance in some cases,
   though it can degrade performance in others.  It's often suboptimal
   for COND to call external functions or access volatile storage.  */

#ifndef ENABLE_CHECKING
# define eassert(cond) ((void) (false && (cond))) /* Check COND compiles.  */
# define eassume(cond) assume (cond)
#else /* ENABLE_CHECKING */

extern _Noreturn void die (const char *, const char *, int);

extern bool suppress_checking EXTERNALLY_VISIBLE;

# define eassert(cond)						\
   (suppress_checking || (cond) 				\
    ? (void) 0							\
    : die (# cond, __FILE__, __LINE__))
# define eassume(cond)						\
   (suppress_checking						\
    ? assume (cond)						\
    : (cond)							\
    ? (void) 0							\
    : die (# cond, __FILE__, __LINE__))
#endif /* ENABLE_CHECKING */


/* Use the configure flag --enable-check-lisp-object-type to make
   Lisp_Object use a struct type instead of the default int.  The flag
   causes CHECK_LISP_OBJECT_TYPE to be defined.  */

/***** Select the tagging scheme.  *****/
/* The following option controls the tagging scheme:
   - USE_LSB_TAG means that we can assume the least 3 bits of pointers are
     always 0, and we can thus use them to hold tag bits, without
     restricting our addressing space.

   If ! USE_LSB_TAG, then use the top 3 bits for tagging, thus
   restricting our possible address range.

   USE_LSB_TAG not only requires the least 3 bits of pointers returned by
   malloc to be 0 but also needs to be able to impose a mult-of-8 alignment
   on the few static Lisp_Objects used, all of which are aligned via
   'char alignas (GCALIGNMENT) gcaligned;' inside a union.  */

enum Lisp_Bits
  {
    /* 2**GCTYPEBITS.  This must be a macro that expands to a literal
       integer constant, for older versions of GCC (through at least 4.9).  */
#define GCALIGNMENT 8

    /* Number of bits in a Lisp_Object value, not counting the tag.  */
    VALBITS = EMACS_INT_WIDTH - GCTYPEBITS,

    /* Number of bits in a Lisp fixnum tag.  */
    INTTYPEBITS = GCTYPEBITS - 1,

    /* Number of bits in a Lisp fixnum value, not counting the tag.  */
    FIXNUM_BITS = VALBITS + 1
  };

#if GCALIGNMENT != 1 << GCTYPEBITS
# error "GCALIGNMENT and GCTYPEBITS are inconsistent"
#endif

/* The maximum value that can be stored in a EMACS_INT, assuming all
   bits other than the type bits contribute to a nonnegative signed value.
   This can be used in #if, e.g., '#if USE_LSB_TAG' below expands to an
   expression involving VAL_MAX.  */
#define VAL_MAX (EMACS_INT_MAX >> (GCTYPEBITS - 1))

/* Whether the least-significant bits of an EMACS_INT contain the tag.
   On hosts where pointers-as-ints do not exceed VAL_MAX / 2, USE_LSB_TAG is:
    a. unnecessary, because the top bits of an EMACS_INT are unused, and
    b. slower, because it typically requires extra masking.
   So, USE_LSB_TAG is true only on hosts where it might be useful.  */
DEFINE_GDB_SYMBOL_BEGIN (bool, USE_LSB_TAG)
#define USE_LSB_TAG (VAL_MAX / 2 < INTPTR_MAX)
DEFINE_GDB_SYMBOL_END (USE_LSB_TAG)

/* Mask for the value (as opposed to the type bits) of a Lisp object.  */
DEFINE_GDB_SYMBOL_BEGIN (EMACS_INT, VALMASK)
# define VALMASK (USE_LSB_TAG ? - (1 << GCTYPEBITS) : VAL_MAX)
DEFINE_GDB_SYMBOL_END (VALMASK)

#if !USE_LSB_TAG && !defined WIDE_EMACS_INT
# error "USE_LSB_TAG not supported on this platform; please report this." \
	"Try 'configure --with-wide-int' to work around the problem."
error !;
#endif

/* Some operations are so commonly executed that they are implemented
   as macros, not functions, because otherwise runtime performance would
   suffer too much when compiling with GCC without optimization.
   There's no need to inline everything, just the operations that
   would otherwise cause a serious performance problem.

   For each such operation OP, define a macro lisp_h_OP that contains
   the operation's implementation.  That way, OP can be implemented
   via a macro definition like this:

     #define OP(x) lisp_h_OP (x)

   and/or via a function definition like this:

     Lisp_Object (OP) (Lisp_Object x) { return lisp_h_OP (x); }

   without worrying about the implementations diverging, since
   lisp_h_OP defines the actual implementation.  The lisp_h_OP macros
   are intended to be private to this include file, and should not be
   used elsewhere.

   FIXME: Remove the lisp_h_OP macros, and define just the inline OP
   functions, once "gcc -Og" (new to GCC 4.8) works well enough for
   Emacs developers.  Maybe in the year 2020.  See Bug#11935.

   Commentary for these macros can be found near their corresponding
   functions, below.  */

#if CHECK_LISP_OBJECT_TYPE
# define lisp_h_XLI(o) ((o).i)
# define lisp_h_XIL(i) ((Lisp_Object) { i })
#else
# define lisp_h_XLI(o) (o)
# define lisp_h_XIL(i) (i)
#endif
#define lisp_h_CHECK_NUMBER(x) CHECK_TYPE (INTEGERP (x), Qintegerp, x)
#define lisp_h_CHECK_SYMBOL(x) CHECK_TYPE (SYMBOLP (x), Qsymbolp, x)
#define lisp_h_CHECK_TYPE(ok, predicate, x) \
   ((ok) ? (void) 0 : wrong_type_argument (predicate, x))
#define lisp_h_CONSP(x) (XTYPE (x) == Lisp_Cons)
#define lisp_h_EQ(x, y) (XLI (x) == XLI (y))
#define lisp_h_FLOATP(x) (XTYPE (x) == Lisp_Float)
#define lisp_h_INTEGERP(x) ((XTYPE (x) & (Lisp_Int0 | ~Lisp_Int1)) == Lisp_Int0)
#define lisp_h_MARKERP(x) (MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Marker)
#define lisp_h_MISCP(x) (XTYPE (x) == Lisp_Misc)
#define lisp_h_NILP(x) EQ (x, Qnil)
#define lisp_h_SET_SYMBOL_VAL(sym, v) \
   (eassert ((sym)->u.s.redirect == SYMBOL_PLAINVAL), \
    (sym)->u.s.val.value = (v))
#define lisp_h_SYMBOL_CONSTANT_P(sym) \
   (XSYMBOL (sym)->u.s.trapped_write == SYMBOL_NOWRITE)
#define lisp_h_SYMBOL_TRAPPED_WRITE_P(sym) (XSYMBOL (sym)->u.s.trapped_write)
#define lisp_h_SYMBOL_VAL(sym) \
   (eassert ((sym)->u.s.redirect == SYMBOL_PLAINVAL), (sym)->u.s.val.value)
#define lisp_h_SYMBOLP(x) (XTYPE (x) == Lisp_Symbol)
#define lisp_h_VECTORLIKEP(x) (XTYPE (x) == Lisp_Vectorlike)
#define lisp_h_XCAR(c) XCONS (c)->u.s.car
#define lisp_h_XCDR(c) XCONS (c)->u.s.u.cdr
#define lisp_h_XCONS(a) \
   (eassert (CONSP (a)), (struct Lisp_Cons *) XUNTAG (a, Lisp_Cons))
#define lisp_h_XHASH(a) XUINT (a)
#ifndef GC_CHECK_CONS_LIST
# define lisp_h_check_cons_list() ((void) 0)
#endif
#if USE_LSB_TAG
# define lisp_h_make_number(n) \
    XIL ((EMACS_INT) (((EMACS_UINT) (n) << INTTYPEBITS) + Lisp_Int0))
# define lisp_h_XFASTINT(a) XINT (a)
# define lisp_h_XINT(a) (XLI (a) >> INTTYPEBITS)
# define lisp_h_XSYMBOL(a) \
    (eassert (SYMBOLP (a)), \
     (struct Lisp_Symbol *) ((intptr_t) XLI (a) - Lisp_Symbol \
			     + (char *) lispsym))
# define lisp_h_XTYPE(a) ((enum Lisp_Type) (XLI (a) & ~VALMASK))
# define lisp_h_XUNTAG(a, type) \
    __builtin_assume_aligned ((void *) (intptr_t) (XLI (a) - (type)), \
			      GCALIGNMENT)
#endif

/* When compiling via gcc -O0, define the key operations as macros, as
   Emacs is too slow otherwise.  To disable this optimization, compile
   with -DINLINING=false.  */
#if (defined __NO_INLINE__ \
     && ! defined __OPTIMIZE__ && ! defined __OPTIMIZE_SIZE__ \
     && ! (defined INLINING && ! INLINING))
# define DEFINE_KEY_OPS_AS_MACROS true
#else
# define DEFINE_KEY_OPS_AS_MACROS false
#endif

#if DEFINE_KEY_OPS_AS_MACROS
# define XLI(o) lisp_h_XLI (o)
# define XIL(i) lisp_h_XIL (i)
# define CHECK_NUMBER(x) lisp_h_CHECK_NUMBER (x)
# define CHECK_SYMBOL(x) lisp_h_CHECK_SYMBOL (x)
# define CHECK_TYPE(ok, predicate, x) lisp_h_CHECK_TYPE (ok, predicate, x)
# define CONSP(x) lisp_h_CONSP (x)
# define EQ(x, y) lisp_h_EQ (x, y)
# define FLOATP(x) lisp_h_FLOATP (x)
# define INTEGERP(x) lisp_h_INTEGERP (x)
# define MARKERP(x) lisp_h_MARKERP (x)
# define MISCP(x) lisp_h_MISCP (x)
# define NILP(x) lisp_h_NILP (x)
# define SET_SYMBOL_VAL(sym, v) lisp_h_SET_SYMBOL_VAL (sym, v)
# define SYMBOL_CONSTANT_P(sym) lisp_h_SYMBOL_CONSTANT_P (sym)
# define SYMBOL_TRAPPED_WRITE_P(sym) lisp_h_SYMBOL_TRAPPED_WRITE_P (sym)
# define SYMBOL_VAL(sym) lisp_h_SYMBOL_VAL (sym)
# define SYMBOLP(x) lisp_h_SYMBOLP (x)
# define VECTORLIKEP(x) lisp_h_VECTORLIKEP (x)
# define XCAR(c) lisp_h_XCAR (c)
# define XCDR(c) lisp_h_XCDR (c)
# define XCONS(a) lisp_h_XCONS (a)
# define XHASH(a) lisp_h_XHASH (a)
# ifndef GC_CHECK_CONS_LIST
#  define check_cons_list() lisp_h_check_cons_list ()
# endif
# if USE_LSB_TAG
#  define make_number(n) lisp_h_make_number (n)
#  define XFASTINT(a) lisp_h_XFASTINT (a)
#  define XINT(a) lisp_h_XINT (a)
#  define XSYMBOL(a) lisp_h_XSYMBOL (a)
#  define XTYPE(a) lisp_h_XTYPE (a)
#  define XUNTAG(a, type) lisp_h_XUNTAG (a, type)
# endif
#endif


/* Define the fundamental Lisp data structures.  */

/* This is the set of Lisp data types.  If you want to define a new
   data type, read the comments after Lisp_Fwd_Type definition
   below.  */

/* Lisp integers use 2 tags, to give them one extra bit, thus
   extending their range from, e.g., -2^28..2^28-1 to -2^29..2^29-1.  */
#define INTMASK (EMACS_INT_MAX >> (INTTYPEBITS - 1))
#define case_Lisp_Int case Lisp_Int0: case Lisp_Int1

/* Idea stolen from GDB.  Pedantic GCC complains about enum bitfields,
   and xlc and Oracle Studio c99 complain vociferously about them.  */
#if (defined __STRICT_ANSI__ || defined __IBMC__ \
     || (defined __SUNPRO_C && __STDC__))
#define ENUM_BF(TYPE) unsigned int
#else
#define ENUM_BF(TYPE) enum TYPE
#endif


enum Lisp_Type
  {
    /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.  */
    Lisp_Symbol = 0,

    /* Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
       whose first member indicates the subtype.  */
    Lisp_Misc = 1,

    /* Integer.  XINT (obj) is the integer value.  */
    Lisp_Int0 = 2,
    Lisp_Int1 = USE_LSB_TAG ? 6 : 3,

    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein.  */
    Lisp_String = 4,

    /* Vector of Lisp objects, or something resembling it.
       XVECTOR (object) points to a struct Lisp_Vector, which contains
       the size and contents.  The size field also contains the type
       information, if it's not a real vector object.  */
    Lisp_Vectorlike = 5,

    /* Cons.  XCONS (object) points to a struct Lisp_Cons.  */
    Lisp_Cons = USE_LSB_TAG ? 3 : 6,

    Lisp_Float = 7
  };

/* This is the set of data types that share a common structure.
   The first member of the structure is a type code from this set.
   The enum values are arbitrary, but we'll use large numbers to make it
   more likely that we'll spot the error if a random word in memory is
   mistakenly interpreted as a Lisp_Misc.  */
enum Lisp_Misc_Type
  {
    Lisp_Misc_Free = 0x5eab,
    Lisp_Misc_Marker,
    Lisp_Misc_Overlay,
    Lisp_Misc_Save_Value,
    Lisp_Misc_Finalizer,
#ifdef HAVE_MODULES
    Lisp_Misc_User_Ptr,
#endif
    /* This is not a type code.  It is for range checking.  */
    Lisp_Misc_Limit
  };

/* These are the types of forwarding objects used in the value slot
   of symbols for special built-in variables whose value is stored in
   C variables.  */
enum Lisp_Fwd_Type
  {
    Lisp_Fwd_Int,		/* Fwd to a C `int' variable.  */
    Lisp_Fwd_Bool,		/* Fwd to a C boolean var.  */
    Lisp_Fwd_Obj,		/* Fwd to a C Lisp_Object variable.  */
    Lisp_Fwd_Buffer_Obj,	/* Fwd to a Lisp_Object field of buffers.  */
    Lisp_Fwd_Kboard_Obj		/* Fwd to a Lisp_Object field of kboards.  */
  };

/* If you want to define a new Lisp data type, here are some
   instructions.  See the thread at
   https://lists.gnu.org/r/emacs-devel/2012-10/msg00561.html
   for more info.

   First, there are already a couple of Lisp types that can be used if
   your new type does not need to be exposed to Lisp programs nor
   displayed to users.  These are Lisp_Save_Value, a Lisp_Misc
   subtype; and PVEC_OTHER, a kind of vectorlike object.  The former
   is suitable for temporarily stashing away pointers and integers in
   a Lisp object.  The latter is useful for vector-like Lisp objects
   that need to be used as part of other objects, but which are never
   shown to users or Lisp code (search for PVEC_OTHER in xterm.c for
   an example).

   These two types don't look pretty when printed, so they are
   unsuitable for Lisp objects that can be exposed to users.

   To define a new data type, add one more Lisp_Misc subtype or one
   more pseudovector subtype.  Pseudovectors are more suitable for
   objects with several slots that need to support fast random access,
   while Lisp_Misc types are for everything else.  A pseudovector object
   provides one or more slots for Lisp objects, followed by struct
   members that are accessible only from C.  A Lisp_Misc object is a
   wrapper for a C struct that can contain anything you like.

   Explicit freeing is discouraged for Lisp objects in general.  But if
   you really need to exploit this, use Lisp_Misc (check free_misc in
   alloc.c to see why).  There is no way to free a vectorlike object.

   To add a new pseudovector type, extend the pvec_type enumeration;
   to add a new Lisp_Misc, extend the Lisp_Misc_Type enumeration.

   For a Lisp_Misc, you will also need to add your entry to union
   Lisp_Misc, but make sure the first word has the same structure as
   the others, starting with a 16-bit member of the Lisp_Misc_Type
   enumeration and a 1-bit GC markbit.  Also make sure the overall
   size of the union is not increased by your addition.  The latter
   requirement is to keep Lisp_Misc objects small enough, so they
   are handled faster: since all Lisp_Misc types use the same space,
   enlarging any of them will affect all the rest.  If you really
   need a larger object, it is best to use Lisp_Vectorlike instead.

   For a new pseudovector, it's highly desirable to limit the size
   of your data type by VBLOCK_BYTES_MAX bytes (defined in alloc.c).
   Otherwise you will need to change sweep_vectors (also in alloc.c).

   Then you will need to add switch branches in print.c (in
   print_object, to print your object, and possibly also in
   print_preprocess) and to alloc.c, to mark your object (in
   mark_object) and to free it (in gc_sweep).  The latter is also the
   right place to call any code specific to your data type that needs
   to run when the object is recycled -- e.g., free any additional
   resources allocated for it that are not Lisp objects.  You can even
   make a pointer to the function that frees the resources a slot in
   your object -- this way, the same object could be used to represent
   several disparate C structures.  */

#ifdef CHECK_LISP_OBJECT_TYPE

typedef struct Lisp_Object { EMACS_INT i; } Lisp_Object;

#define LISP_INITIALLY(i) {i}

#undef CHECK_LISP_OBJECT_TYPE
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = true };
#else /* CHECK_LISP_OBJECT_TYPE */

/* If a struct type is not wanted, define Lisp_Object as just a number.  */

typedef EMACS_INT Lisp_Object;
#define LISP_INITIALLY(i) (i)
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = false };
#endif /* CHECK_LISP_OBJECT_TYPE */

/* Forward declarations.  */

/* Defined in this file.  */
INLINE void set_sub_char_table_contents (Lisp_Object, ptrdiff_t,
					      Lisp_Object);

/* Defined in chartab.c.  */
extern Lisp_Object char_table_ref (Lisp_Object, int);
extern void char_table_set (Lisp_Object, int, Lisp_Object);

/* Defined in data.c.  */
extern _Noreturn void wrong_type_argument (Lisp_Object, Lisp_Object);


#ifdef CANNOT_DUMP
enum { might_dump = false };
#elif defined DOUG_LEA_MALLOC
/* Defined in emacs.c.  */
extern bool might_dump;
#endif
/* True means Emacs has already been initialized.
   Used during startup to detect startup of dumped Emacs.  */
extern bool initialized;

/* Defined in floatfns.c.  */
extern double extract_float (Lisp_Object);


/* Low-level conversion and type checking.  */

/* Convert a Lisp_Object to the corresponding EMACS_INT and vice versa.
   At the machine level, these operations are no-ops.  */

INLINE EMACS_INT
(XLI) (Lisp_Object o)
{
  return lisp_h_XLI (o);
}

INLINE Lisp_Object
(XIL) (EMACS_INT i)
{
  return lisp_h_XIL (i);
}

/* Extract A's type.  */

INLINE enum Lisp_Type
(XTYPE) (Lisp_Object a)
{
#if USE_LSB_TAG
  return lisp_h_XTYPE (a);
#else
  EMACS_UINT i = XLI (a);
  return USE_LSB_TAG ? i & ~VALMASK : i >> VALBITS;
#endif
}

INLINE void
(CHECK_TYPE) (int ok, Lisp_Object predicate, Lisp_Object x)
{
  lisp_h_CHECK_TYPE (ok, predicate, x);
}

/* Extract A's pointer value, assuming A's type is TYPE.  */

INLINE void *
(XUNTAG) (Lisp_Object a, int type)
{
#if USE_LSB_TAG
  return lisp_h_XUNTAG (a, type);
#else
  intptr_t i = USE_LSB_TAG ? XLI (a) - type : XLI (a) & VALMASK;
  return (void *) i;
#endif
}


/* Interned state of a symbol.  */

enum symbol_interned
{
  SYMBOL_UNINTERNED = 0,
  SYMBOL_INTERNED = 1,
  SYMBOL_INTERNED_IN_INITIAL_OBARRAY = 2
};

enum symbol_redirect
{
  SYMBOL_PLAINVAL  = 4,
  SYMBOL_VARALIAS  = 1,
  SYMBOL_LOCALIZED = 2,
  SYMBOL_FORWARDED = 3
};

enum symbol_trapped_write
{
  SYMBOL_UNTRAPPED_WRITE = 0,
  SYMBOL_NOWRITE = 1,
  SYMBOL_TRAPPED_WRITE = 2
};

struct Lisp_Symbol
{
  union
  {
    struct
    {
      bool_bf gcmarkbit : 1;

      /* Indicates where the value can be found:
	 0 : it's a plain var, the value is in the `value' field.
	 1 : it's a varalias, the value is really in the `alias' symbol.
	 2 : it's a localized var, the value is in the `blv' object.
	 3 : it's a forwarding variable, the value is in `forward'.  */
      ENUM_BF (symbol_redirect) redirect : 3;

      /* 0 : normal case, just set the value
	 1 : constant, cannot set, e.g. nil, t, :keywords.
	 2 : trap the write, call watcher functions.  */
      ENUM_BF (symbol_trapped_write) trapped_write : 2;

      /* Interned state of the symbol.  This is an enumerator from
	 enum symbol_interned.  */
      unsigned interned : 2;

      /* True means that this variable has been explicitly declared
	 special (with `defvar' etc), and shouldn't be lexically bound.  */
      bool_bf declared_special : 1;

      /* True if pointed to from purespace and hence can't be GC'd.  */
      bool_bf pinned : 1;

      /* The symbol's name, as a Lisp string.  */
      Lisp_Object name;

      /* Value of the symbol or Qunbound if unbound.  Which alternative of the
	 union is used depends on the `redirect' field above.  */
      union {
	Lisp_Object value;
	struct Lisp_Symbol *alias;
	struct Lisp_Buffer_Local_Value *blv;
	union Lisp_Fwd *fwd;
      } val;

      /* Function value of the symbol or Qnil if not fboundp.  */
      Lisp_Object function;

      /* The symbol's property list.  */
      Lisp_Object plist;

      /* Next symbol in obarray bucket, if the symbol is interned.  */
      struct Lisp_Symbol *next;
    } s;
    char alignas (GCALIGNMENT) gcaligned;
  } u;
};
verify (alignof (struct Lisp_Symbol) % GCALIGNMENT == 0);

/* Declare a Lisp-callable function.  The MAXARGS parameter has the same
   meaning as in the DEFUN macro, and is used to construct a prototype.  */
/* We can use the same trick as in the DEFUN macro to generate the
   appropriate prototype.  */
#define EXFUN(fnname, maxargs) \
  extern Lisp_Object fnname DEFUN_ARGS_ ## maxargs

/* Note that the weird token-substitution semantics of ANSI C makes
   this work for MANY and UNEVALLED.  */
#define DEFUN_ARGS_MANY		(ptrdiff_t, Lisp_Object *)
#define DEFUN_ARGS_UNEVALLED	(Lisp_Object)
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(Lisp_Object)
#define DEFUN_ARGS_2	(Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_3	(Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_4	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_5	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object)
#define DEFUN_ARGS_6	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_7	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object, Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_8	(Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, \
			 Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object)

/* Yield a signed integer that contains TAG along with PTR.

   Sign-extend pointers when USE_LSB_TAG (this simplifies emacs-module.c),
   and zero-extend otherwise (that’s a bit faster here).
   Sign extension matters only when EMACS_INT is wider than a pointer.  */
#define TAG_PTR(tag, ptr) \
  (USE_LSB_TAG \
   ? (intptr_t) (ptr) + (tag) \
   : (EMACS_INT) (((EMACS_UINT) (tag) << VALBITS) + (uintptr_t) (ptr)))

/* Yield an integer that contains a symbol tag along with OFFSET.
   OFFSET should be the offset in bytes from 'lispsym' to the symbol.  */
#define TAG_SYMOFFSET(offset) TAG_PTR (Lisp_Symbol, offset)

/* XLI_BUILTIN_LISPSYM (iQwhatever) is equivalent to
   XLI (builtin_lisp_symbol (Qwhatever)),
   except the former expands to an integer constant expression.  */
#define XLI_BUILTIN_LISPSYM(iname) TAG_SYMOFFSET ((iname) * sizeof *lispsym)

/* LISPSYM_INITIALLY (Qfoo) is equivalent to Qfoo except it is
   designed for use as an initializer, even for a constant initializer.  */
#define LISPSYM_INITIALLY(name) LISP_INITIALLY (XLI_BUILTIN_LISPSYM (i##name))

/* Declare extern constants for Lisp symbols.  These can be helpful
   when using a debugger like GDB, on older platforms where the debug
   format does not represent C macros.  */
#define DEFINE_LISP_SYMBOL(name) \
  DEFINE_GDB_SYMBOL_BEGIN (Lisp_Object, name) \
  DEFINE_GDB_SYMBOL_END (LISPSYM_INITIALLY (name))

/* The index of the C-defined Lisp symbol SYM.
   This can be used in a static initializer.  */
#define SYMBOL_INDEX(sym) i##sym

/* By default, define macros for Qt, etc., as this leads to a bit
   better performance in the core Emacs interpreter.  A plugin can
   define DEFINE_NON_NIL_Q_SYMBOL_MACROS to be false, to be portable to
   other Emacs instances that assign different values to Qt, etc.  */
#ifndef DEFINE_NON_NIL_Q_SYMBOL_MACROS
# define DEFINE_NON_NIL_Q_SYMBOL_MACROS true
#endif

#include "globals.h"

/* Header of vector-like objects.  This documents the layout constraints on
   vectors and pseudovectors (objects of PVEC_xxx subtype).  It also prevents
   compilers from being fooled by Emacs's type punning: XSETPSEUDOVECTOR
   and PSEUDOVECTORP cast their pointers to union vectorlike_header *,
   because when two such pointers potentially alias, a compiler won't
   incorrectly reorder loads and stores to their size fields.  See
   Bug#8546.  */
union vectorlike_header
  {
    /* The main member contains various pieces of information:
       - The MSB (ARRAY_MARK_FLAG) holds the gcmarkbit.
       - The next bit (PSEUDOVECTOR_FLAG) indicates whether this is a plain
         vector (0) or a pseudovector (1).
       - If PSEUDOVECTOR_FLAG is 0, the rest holds the size (number
         of slots) of the vector.
       - If PSEUDOVECTOR_FLAG is 1, the rest is subdivided into three fields:
	 - a) pseudovector subtype held in PVEC_TYPE_MASK field;
	 - b) number of Lisp_Objects slots at the beginning of the object
	   held in PSEUDOVECTOR_SIZE_MASK field.  These objects are always
	   traced by the GC;
	 - c) size of the rest fields held in PSEUDOVECTOR_REST_MASK and
	   measured in word_size units.  Rest fields may also include
	   Lisp_Objects, but these objects usually needs some special treatment
	   during GC.
	 There are some exceptions.  For PVEC_FREE, b) is always zero.  For
	 PVEC_BOOL_VECTOR and PVEC_SUBR, both b) and c) are always zero.
	 Current layout limits the pseudovectors to 63 PVEC_xxx subtypes,
	 4095 Lisp_Objects in GC-ed area and 4095 word-sized other slots.  */
    ptrdiff_t size;
    char alignas (GCALIGNMENT) gcaligned;
  };
verify (alignof (union vectorlike_header) % GCALIGNMENT == 0);

INLINE bool
(SYMBOLP) (Lisp_Object x)
{
  return lisp_h_SYMBOLP (x);
}

INLINE struct Lisp_Symbol *
(XSYMBOL) (Lisp_Object a)
{
#if USE_LSB_TAG
  return lisp_h_XSYMBOL (a);
#else
  eassert (SYMBOLP (a));
  intptr_t i = (intptr_t) XUNTAG (a, Lisp_Symbol);
  void *p = (char *) lispsym + i;
  return p;
#endif
}

INLINE Lisp_Object
make_lisp_symbol (struct Lisp_Symbol *sym)
{
  Lisp_Object a = XIL (TAG_SYMOFFSET ((char *) sym - (char *) lispsym));
  eassert (XSYMBOL (a) == sym);
  return a;
}

INLINE Lisp_Object
builtin_lisp_symbol (int index)
{
  return make_lisp_symbol (&lispsym[index]);
}

INLINE void
(CHECK_SYMBOL) (Lisp_Object x)
{
  lisp_h_CHECK_SYMBOL (x);
}

/* In the size word of a vector, this bit means the vector has been marked.  */

DEFINE_GDB_SYMBOL_BEGIN (ptrdiff_t, ARRAY_MARK_FLAG)
# define ARRAY_MARK_FLAG PTRDIFF_MIN
DEFINE_GDB_SYMBOL_END (ARRAY_MARK_FLAG)

/* In the size word of a struct Lisp_Vector, this bit means it's really
   some other vector-like object.  */
DEFINE_GDB_SYMBOL_BEGIN (ptrdiff_t, PSEUDOVECTOR_FLAG)
# define PSEUDOVECTOR_FLAG (PTRDIFF_MAX - PTRDIFF_MAX / 2)
DEFINE_GDB_SYMBOL_END (PSEUDOVECTOR_FLAG)

/* In a pseudovector, the size field actually contains a word with one
   PSEUDOVECTOR_FLAG bit set, and one of the following values extracted
   with PVEC_TYPE_MASK to indicate the actual type.  */
enum pvec_type
{
  PVEC_NORMAL_VECTOR,
  PVEC_FREE,
  PVEC_PROCESS,
  PVEC_FRAME,
  PVEC_WINDOW,
  PVEC_BOOL_VECTOR,
  PVEC_BUFFER,
  PVEC_HASH_TABLE,
  PVEC_TERMINAL,
  PVEC_WINDOW_CONFIGURATION,
  PVEC_SUBR,
  PVEC_OTHER,            /* Should never be visible to Elisp code.  */
  PVEC_XWIDGET,
  PVEC_XWIDGET_VIEW,
  PVEC_THREAD,
  PVEC_MUTEX,
  PVEC_CONDVAR,
  PVEC_MODULE_FUNCTION,

  /* These should be last, check internal_equal to see why.  */
  PVEC_COMPILED,
  PVEC_CHAR_TABLE,
  PVEC_SUB_CHAR_TABLE,
  PVEC_RECORD,
  PVEC_FONT /* Should be last because it's used for range checking.  */
};

enum More_Lisp_Bits
  {
    /* For convenience, we also store the number of elements in these bits.
       Note that this size is not necessarily the memory-footprint size, but
       only the number of Lisp_Object fields (that need to be traced by GC).
       The distinction is used, e.g., by Lisp_Process, which places extra
       non-Lisp_Object fields at the end of the structure.  */
    PSEUDOVECTOR_SIZE_BITS = 12,
    PSEUDOVECTOR_SIZE_MASK = (1 << PSEUDOVECTOR_SIZE_BITS) - 1,

    /* To calculate the memory footprint of the pseudovector, it's useful
       to store the size of non-Lisp area in word_size units here.  */
    PSEUDOVECTOR_REST_BITS = 12,
    PSEUDOVECTOR_REST_MASK = (((1 << PSEUDOVECTOR_REST_BITS) - 1)
			      << PSEUDOVECTOR_SIZE_BITS),

    /* Used to extract pseudovector subtype information.  */
    PSEUDOVECTOR_AREA_BITS = PSEUDOVECTOR_SIZE_BITS + PSEUDOVECTOR_REST_BITS,
    PVEC_TYPE_MASK = 0x3f << PSEUDOVECTOR_AREA_BITS
  };

/* These functions extract various sorts of values from a Lisp_Object.
   For example, if tem is a Lisp_Object whose type is Lisp_Cons,
   XCONS (tem) is the struct Lisp_Cons * pointing to the memory for
   that cons.  */

/* Largest and smallest representable fixnum values.  These are the C
   values.  They are macros for use in static initializers.  */
#define MOST_POSITIVE_FIXNUM (EMACS_INT_MAX >> INTTYPEBITS)
#define MOST_NEGATIVE_FIXNUM (-1 - MOST_POSITIVE_FIXNUM)

#if USE_LSB_TAG

INLINE Lisp_Object
(make_number) (EMACS_INT n)
{
  return lisp_h_make_number (n);
}

INLINE EMACS_INT
(XINT) (Lisp_Object a)
{
  return lisp_h_XINT (a);
}

INLINE EMACS_INT
(XFASTINT) (Lisp_Object a)
{
  EMACS_INT n = lisp_h_XFASTINT (a);
  eassume (0 <= n);
  return n;
}

#else /* ! USE_LSB_TAG */

/* Although compiled only if ! USE_LSB_TAG, the following functions
   also work when USE_LSB_TAG; this is to aid future maintenance when
   the lisp_h_* macros are eventually removed.  */

/* Make a Lisp integer representing the value of the low order
   bits of N.  */
INLINE Lisp_Object
make_number (EMACS_INT n)
{
  EMACS_INT int0 = Lisp_Int0;
  if (USE_LSB_TAG)
    {
      EMACS_UINT u = n;
      n = u << INTTYPEBITS;
      n += int0;
    }
  else
    {
      n &= INTMASK;
      n += (int0 << VALBITS);
    }
  return XIL (n);
}

/* Extract A's value as a signed integer.  */
INLINE EMACS_INT
XINT (Lisp_Object a)
{
  EMACS_INT i = XLI (a);
  if (! USE_LSB_TAG)
    {
      EMACS_UINT u = i;
      i = u << INTTYPEBITS;
    }
  return i >> INTTYPEBITS;
}

/* Like XINT (A), but may be faster.  A must be nonnegative.
   If ! USE_LSB_TAG, this takes advantage of the fact that Lisp
   integers have zero-bits in their tags.  */
INLINE EMACS_INT
XFASTINT (Lisp_Object a)
{
  EMACS_INT int0 = Lisp_Int0;
  EMACS_INT n = USE_LSB_TAG ? XINT (a) : XLI (a) - (int0 << VALBITS);
  eassume (0 <= n);
  return n;
}

#endif /* ! USE_LSB_TAG */

/* Extract A's value as an unsigned integer.  */
INLINE EMACS_UINT
XUINT (Lisp_Object a)
{
  EMACS_UINT i = XLI (a);
  return USE_LSB_TAG ? i >> INTTYPEBITS : i & INTMASK;
}

/* Return A's (Lisp-integer sized) hash.  Happens to be like XUINT
   right now, but XUINT should only be applied to objects we know are
   integers.  */

INLINE EMACS_INT
(XHASH) (Lisp_Object a)
{
  return lisp_h_XHASH (a);
}

/* Like make_number (N), but may be faster.  N must be in nonnegative range.  */
INLINE Lisp_Object
make_natnum (EMACS_INT n)
{
  eassert (0 <= n && n <= MOST_POSITIVE_FIXNUM);
  EMACS_INT int0 = Lisp_Int0;
  return USE_LSB_TAG ? make_number (n) : XIL (n + (int0 << VALBITS));
}

/* Return true if X and Y are the same object.  */

INLINE bool
(EQ) (Lisp_Object x, Lisp_Object y)
{
  return lisp_h_EQ (x, y);
}

/* True if the possibly-unsigned integer I doesn't fit in a Lisp fixnum.  */

#define FIXNUM_OVERFLOW_P(i) \
  (! ((0 <= (i) || MOST_NEGATIVE_FIXNUM <= (i)) && (i) <= MOST_POSITIVE_FIXNUM))

INLINE ptrdiff_t
clip_to_bounds (ptrdiff_t lower, EMACS_INT num, ptrdiff_t upper)
{
  return num < lower ? lower : num <= upper ? num : upper;
}

/* Construct a Lisp_Object from a value or address.  */

INLINE Lisp_Object
make_lisp_ptr (void *ptr, enum Lisp_Type type)
{
  Lisp_Object a = XIL (TAG_PTR (type, ptr));
  eassert (XTYPE (a) == type && XUNTAG (a, type) == ptr);
  return a;
}

INLINE bool
(INTEGERP) (Lisp_Object x)
{
  return lisp_h_INTEGERP (x);
}

#define XSETINT(a, b) ((a) = make_number (b))
#define XSETFASTINT(a, b) ((a) = make_natnum (b))
#define XSETCONS(a, b) ((a) = make_lisp_ptr (b, Lisp_Cons))
#define XSETVECTOR(a, b) ((a) = make_lisp_ptr (b, Lisp_Vectorlike))
#define XSETSTRING(a, b) ((a) = make_lisp_ptr (b, Lisp_String))
#define XSETSYMBOL(a, b) ((a) = make_lisp_symbol (b))
#define XSETFLOAT(a, b) ((a) = make_lisp_ptr (b, Lisp_Float))
#define XSETMISC(a, b) ((a) = make_lisp_ptr (b, Lisp_Misc))

/* Pseudovector types.  */

#define XSETPVECTYPE(v, code)						\
  ((v)->header.size |= PSEUDOVECTOR_FLAG | ((code) << PSEUDOVECTOR_AREA_BITS))
#define XSETPVECTYPESIZE(v, code, lispsize, restsize)		\
  ((v)->header.size = (PSEUDOVECTOR_FLAG			\
		       | ((code) << PSEUDOVECTOR_AREA_BITS)	\
		       | ((restsize) << PSEUDOVECTOR_SIZE_BITS) \
		       | (lispsize)))

/* The cast to union vectorlike_header * avoids aliasing issues.  */
#define XSETPSEUDOVECTOR(a, b, code) \
  XSETTYPED_PSEUDOVECTOR (a, b,					\
			  (((union vectorlike_header *)	\
			    XUNTAG (a, Lisp_Vectorlike))	\
			   ->size),				\
			  code)
#define XSETTYPED_PSEUDOVECTOR(a, b, size, code)			\
  (XSETVECTOR (a, b),							\
   eassert ((size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK))		\
	    == (PSEUDOVECTOR_FLAG | (code << PSEUDOVECTOR_AREA_BITS))))

#define XSETWINDOW_CONFIGURATION(a, b) \
  (XSETPSEUDOVECTOR (a, b, PVEC_WINDOW_CONFIGURATION))
#define XSETPROCESS(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_PROCESS))
#define XSETWINDOW(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_WINDOW))
#define XSETTERMINAL(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_TERMINAL))
#define XSETSUBR(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_SUBR))
#define XSETCOMPILED(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_COMPILED))
#define XSETBUFFER(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_BUFFER))
#define XSETCHAR_TABLE(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_CHAR_TABLE))
#define XSETBOOL_VECTOR(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_BOOL_VECTOR))
#define XSETSUB_CHAR_TABLE(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_SUB_CHAR_TABLE))
#define XSETTHREAD(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_THREAD))
#define XSETMUTEX(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_MUTEX))
#define XSETCONDVAR(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_CONDVAR))

/* Efficiently convert a pointer to a Lisp object and back.  The
   pointer is represented as a Lisp integer, so the garbage collector
   does not know about it.  The pointer should not have both Lisp_Int1
   bits set, which makes this conversion inherently unportable.  */

INLINE void *
XINTPTR (Lisp_Object a)
{
  return XUNTAG (a, Lisp_Int0);
}

INLINE Lisp_Object
make_pointer_integer (void *p)
{
  Lisp_Object a = XIL (TAG_PTR (Lisp_Int0, p));
  eassert (INTEGERP (a) && XINTPTR (a) == p);
  return a;
}

/* See the macros in intervals.h.  */

typedef struct interval *INTERVAL;

struct Lisp_Cons
{
  union
  {
    struct
    {
      /* Car of this cons cell.  */
      Lisp_Object car;

      union
      {
	/* Cdr of this cons cell.  */
	Lisp_Object cdr;

	/* Used to chain conses on a free list.  */
	struct Lisp_Cons *chain;
      } u;
    } s;
    char alignas (GCALIGNMENT) gcaligned;
  } u;
};
verify (alignof (struct Lisp_Cons) % GCALIGNMENT == 0);

INLINE bool
(NILP) (Lisp_Object x)
{
  return lisp_h_NILP (x);
}

INLINE bool
(CONSP) (Lisp_Object x)
{
  return lisp_h_CONSP (x);
}

INLINE void
CHECK_CONS (Lisp_Object x)
{
  CHECK_TYPE (CONSP (x), Qconsp, x);
}

INLINE struct Lisp_Cons *
(XCONS) (Lisp_Object a)
{
  return lisp_h_XCONS (a);
}

/* Take the car or cdr of something known to be a cons cell.  */
/* The _addr functions shouldn't be used outside of the minimal set
   of code that has to know what a cons cell looks like.  Other code not
   part of the basic lisp implementation should assume that the car and cdr
   fields are not accessible.  (What if we want to switch to
   a copying collector someday?  Cached cons cell field addresses may be
   invalidated at arbitrary points.)  */
INLINE Lisp_Object *
xcar_addr (Lisp_Object c)
{
  return &XCONS (c)->u.s.car;
}
INLINE Lisp_Object *
xcdr_addr (Lisp_Object c)
{
  return &XCONS (c)->u.s.u.cdr;
}

/* Use these from normal code.  */

INLINE Lisp_Object
(XCAR) (Lisp_Object c)
{
  return lisp_h_XCAR (c);
}

INLINE Lisp_Object
(XCDR) (Lisp_Object c)
{
  return lisp_h_XCDR (c);
}

/* Use these to set the fields of a cons cell.

   Note that both arguments may refer to the same object, so 'n'
   should not be read after 'c' is first modified.  */
INLINE void
XSETCAR (Lisp_Object c, Lisp_Object n)
{
  *xcar_addr (c) = n;
}
INLINE void
XSETCDR (Lisp_Object c, Lisp_Object n)
{
  *xcdr_addr (c) = n;
}

/* Take the car or cdr of something whose type is not known.  */
INLINE Lisp_Object
CAR (Lisp_Object c)
{
  if (CONSP (c))
    return XCAR (c);
  if (!NILP (c))
    wrong_type_argument (Qlistp, c);
  return Qnil;
}
INLINE Lisp_Object
CDR (Lisp_Object c)
{
  if (CONSP (c))
    return XCDR (c);
  if (!NILP (c))
    wrong_type_argument (Qlistp, c);
  return Qnil;
}

/* Take the car or cdr of something whose type is not known.  */
INLINE Lisp_Object
CAR_SAFE (Lisp_Object c)
{
  return CONSP (c) ? XCAR (c) : Qnil;
}
INLINE Lisp_Object
CDR_SAFE (Lisp_Object c)
{
  return CONSP (c) ? XCDR (c) : Qnil;
}

/* In a string or vector, the sign bit of u.s.size is the gc mark bit.  */

struct Lisp_String
{
  union
  {
    struct
    {
      ptrdiff_t size;
      ptrdiff_t size_byte;
      INTERVAL intervals;	/* Text properties in this string.  */
      unsigned char *data;
    } s;
    struct Lisp_String *next;
    char alignas (GCALIGNMENT) gcaligned;
  } u;
};
verify (alignof (struct Lisp_String) % GCALIGNMENT == 0);

INLINE bool
STRINGP (Lisp_Object x)
{
  return XTYPE (x) == Lisp_String;
}

INLINE void
CHECK_STRING (Lisp_Object x)
{
  CHECK_TYPE (STRINGP (x), Qstringp, x);
}

INLINE struct Lisp_String *
XSTRING (Lisp_Object a)
{
  eassert (STRINGP (a));
  return XUNTAG (a, Lisp_String);
}

/* True if STR is a multibyte string.  */
INLINE bool
STRING_MULTIBYTE (Lisp_Object str)
{
  return 0 <= XSTRING (str)->u.s.size_byte;
}

/* An upper bound on the number of bytes in a Lisp string, not
   counting the terminating null.  This a tight enough bound to
   prevent integer overflow errors that would otherwise occur during
   string size calculations.  A string cannot contain more bytes than
   a fixnum can represent, nor can it be so long that C pointer
   arithmetic stops working on the string plus its terminating null.
   Although the actual size limit (see STRING_BYTES_MAX in alloc.c)
   may be a bit smaller than STRING_BYTES_BOUND, calculating it here
   would expose alloc.c internal details that we'd rather keep
   private.

   This is a macro for use in static initializers.  The cast to
   ptrdiff_t ensures that the macro is signed.  */
#define STRING_BYTES_BOUND  \
  ((ptrdiff_t) min (MOST_POSITIVE_FIXNUM, min (SIZE_MAX, PTRDIFF_MAX) - 1))

/* Mark STR as a unibyte string.  */
#define STRING_SET_UNIBYTE(STR)				\
  do {							\
    if (XSTRING (STR)->u.s.size == 0)			\
      (STR) = empty_unibyte_string;			\
    else						\
      XSTRING (STR)->u.s.size_byte = -1;		\
  } while (false)

/* Mark STR as a multibyte string.  Assure that STR contains only
   ASCII characters in advance.  */
#define STRING_SET_MULTIBYTE(STR)			\
  do {							\
    if (XSTRING (STR)->u.s.size == 0)			\
      (STR) = empty_multibyte_string;			\
    else						\
      XSTRING (STR)->u.s.size_byte = XSTRING (STR)->u.s.size; \
  } while (false)

/* Convenience functions for dealing with Lisp strings.  */

INLINE unsigned char *
SDATA (Lisp_Object string)
{
  return XSTRING (string)->u.s.data;
}
INLINE char *
SSDATA (Lisp_Object string)
{
  /* Avoid "differ in sign" warnings.  */
  return (char *) SDATA (string);
}
INLINE unsigned char
SREF (Lisp_Object string, ptrdiff_t index)
{
  return SDATA (string)[index];
}
INLINE void
SSET (Lisp_Object string, ptrdiff_t index, unsigned char new)
{
  SDATA (string)[index] = new;
}
INLINE ptrdiff_t
SCHARS (Lisp_Object string)
{
  ptrdiff_t nchars = XSTRING (string)->u.s.size;
  eassume (0 <= nchars);
  return nchars;
}

#ifdef GC_CHECK_STRING_BYTES
extern ptrdiff_t string_bytes (struct Lisp_String *);
#endif
INLINE ptrdiff_t
STRING_BYTES (struct Lisp_String *s)
{
#ifdef GC_CHECK_STRING_BYTES
  ptrdiff_t nbytes = string_bytes (s);
#else
  ptrdiff_t nbytes = s->u.s.size_byte < 0 ? s->u.s.size : s->u.s.size_byte;
#endif
  eassume (0 <= nbytes);
  return nbytes;
}

INLINE ptrdiff_t
SBYTES (Lisp_Object string)
{
  return STRING_BYTES (XSTRING (string));
}
INLINE void
STRING_SET_CHARS (Lisp_Object string, ptrdiff_t newsize)
{
  /* This function cannot change the size of data allocated for the
     string when it was created.  */
  eassert (STRING_MULTIBYTE (string)
	   ? 0 <= newsize && newsize <= SBYTES (string)
	   : newsize == SCHARS (string));
  XSTRING (string)->u.s.size = newsize;
}

/* A regular vector is just a header plus an array of Lisp_Objects.  */

struct Lisp_Vector
  {
    union vectorlike_header header;
    Lisp_Object contents[FLEXIBLE_ARRAY_MEMBER];
  };

INLINE bool
(VECTORLIKEP) (Lisp_Object x)
{
  return lisp_h_VECTORLIKEP (x);
}

INLINE struct Lisp_Vector *
XVECTOR (Lisp_Object a)
{
  eassert (VECTORLIKEP (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

INLINE ptrdiff_t
ASIZE (Lisp_Object array)
{
  ptrdiff_t size = XVECTOR (array)->header.size;
  eassume (0 <= size);
  return size;
}

INLINE ptrdiff_t
PVSIZE (Lisp_Object pv)
{
  return ASIZE (pv) & PSEUDOVECTOR_SIZE_MASK;
}

INLINE bool
VECTORP (Lisp_Object x)
{
  return VECTORLIKEP (x) && ! (ASIZE (x) & PSEUDOVECTOR_FLAG);
}

INLINE void
CHECK_VECTOR (Lisp_Object x)
{
  CHECK_TYPE (VECTORP (x), Qvectorp, x);
}


/* A pseudovector is like a vector, but has other non-Lisp components.  */

INLINE enum pvec_type
PSEUDOVECTOR_TYPE (struct Lisp_Vector *v)
{
  ptrdiff_t size = v->header.size;
  return (size & PSEUDOVECTOR_FLAG
          ? (size & PVEC_TYPE_MASK) >> PSEUDOVECTOR_AREA_BITS
          : PVEC_NORMAL_VECTOR);
}

/* Can't be used with PVEC_NORMAL_VECTOR.  */
INLINE bool
PSEUDOVECTOR_TYPEP (union vectorlike_header *a, enum pvec_type code)
{
  /* We don't use PSEUDOVECTOR_TYPE here so as to avoid a shift
   * operation when `code' is known.  */
  return ((a->size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK))
	  == (PSEUDOVECTOR_FLAG | (code << PSEUDOVECTOR_AREA_BITS)));
}

/* True if A is a pseudovector whose code is CODE.  */
INLINE bool
PSEUDOVECTORP (Lisp_Object a, int code)
{
  if (! VECTORLIKEP (a))
    return false;
  else
    {
      /* Converting to union vectorlike_header * avoids aliasing issues.  */
      union vectorlike_header *h = XUNTAG (a, Lisp_Vectorlike);
      return PSEUDOVECTOR_TYPEP (h, code);
    }
}

/* A boolvector is a kind of vectorlike, with contents like a string.  */

struct Lisp_Bool_Vector
  {
    /* HEADER.SIZE is the vector's size field.  It doesn't have the real size,
       just the subtype information.  */
    union vectorlike_header header;
    /* This is the size in bits.  */
    EMACS_INT size;
    /* The actual bits, packed into bytes.
       Zeros fill out the last word if needed.
       The bits are in little-endian order in the bytes, and
       the bytes are in little-endian order in the words.  */
    bits_word data[FLEXIBLE_ARRAY_MEMBER];
  };

/* Some handy constants for calculating sizes
   and offsets, mostly of vectorlike objects.   */

enum
  {
    header_size = offsetof (struct Lisp_Vector, contents),
    bool_header_size = offsetof (struct Lisp_Bool_Vector, data),
    word_size = sizeof (Lisp_Object)
  };

/* The number of data words and bytes in a bool vector with SIZE bits.  */

INLINE EMACS_INT
bool_vector_words (EMACS_INT size)
{
  eassume (0 <= size && size <= EMACS_INT_MAX - (BITS_PER_BITS_WORD - 1));
  return (size + BITS_PER_BITS_WORD - 1) / BITS_PER_BITS_WORD;
}

INLINE EMACS_INT
bool_vector_bytes (EMACS_INT size)
{
  eassume (0 <= size && size <= EMACS_INT_MAX - (BITS_PER_BITS_WORD - 1));
  return (size + BOOL_VECTOR_BITS_PER_CHAR - 1) / BOOL_VECTOR_BITS_PER_CHAR;
}

INLINE bool
BOOL_VECTOR_P (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_BOOL_VECTOR);
}

INLINE void
CHECK_BOOL_VECTOR (Lisp_Object x)
{
  CHECK_TYPE (BOOL_VECTOR_P (x), Qbool_vector_p, x);
}

INLINE struct Lisp_Bool_Vector *
XBOOL_VECTOR (Lisp_Object a)
{
  eassert (BOOL_VECTOR_P (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

INLINE EMACS_INT
bool_vector_size (Lisp_Object a)
{
  EMACS_INT size = XBOOL_VECTOR (a)->size;
  eassume (0 <= size);
  return size;
}

INLINE bits_word *
bool_vector_data (Lisp_Object a)
{
  return XBOOL_VECTOR (a)->data;
}

INLINE unsigned char *
bool_vector_uchar_data (Lisp_Object a)
{
  return (unsigned char *) bool_vector_data (a);
}

/* True if A's Ith bit is set.  */

INLINE bool
bool_vector_bitref (Lisp_Object a, EMACS_INT i)
{
  eassume (0 <= i && i < bool_vector_size (a));
  return !! (bool_vector_uchar_data (a)[i / BOOL_VECTOR_BITS_PER_CHAR]
	     & (1 << (i % BOOL_VECTOR_BITS_PER_CHAR)));
}

INLINE Lisp_Object
bool_vector_ref (Lisp_Object a, EMACS_INT i)
{
  return bool_vector_bitref (a, i) ? Qt : Qnil;
}

/* Set A's Ith bit to B.  */

INLINE void
bool_vector_set (Lisp_Object a, EMACS_INT i, bool b)
{
  unsigned char *addr;

  eassume (0 <= i && i < bool_vector_size (a));
  addr = &bool_vector_uchar_data (a)[i / BOOL_VECTOR_BITS_PER_CHAR];

  if (b)
    *addr |= 1 << (i % BOOL_VECTOR_BITS_PER_CHAR);
  else
    *addr &= ~ (1 << (i % BOOL_VECTOR_BITS_PER_CHAR));
}

/* Conveniences for dealing with Lisp arrays.  */

INLINE Lisp_Object
AREF (Lisp_Object array, ptrdiff_t idx)
{
  return XVECTOR (array)->contents[idx];
}

INLINE Lisp_Object *
aref_addr (Lisp_Object array, ptrdiff_t idx)
{
  return & XVECTOR (array)->contents[idx];
}

INLINE ptrdiff_t
gc_asize (Lisp_Object array)
{
  /* Like ASIZE, but also can be used in the garbage collector.  */
  return XVECTOR (array)->header.size & ~ARRAY_MARK_FLAG;
}

INLINE void
ASET (Lisp_Object array, ptrdiff_t idx, Lisp_Object val)
{
  eassert (0 <= idx && idx < ASIZE (array));
  XVECTOR (array)->contents[idx] = val;
}

INLINE void
gc_aset (Lisp_Object array, ptrdiff_t idx, Lisp_Object val)
{
  /* Like ASET, but also can be used in the garbage collector:
     sweep_weak_table calls set_hash_key etc. while the table is marked.  */
  eassert (0 <= idx && idx < gc_asize (array));
  XVECTOR (array)->contents[idx] = val;
}

/* True, since Qnil's representation is zero.  Every place in the code
   that assumes Qnil is zero should verify (NIL_IS_ZERO), to make it easy
   to find such assumptions later if we change Qnil to be nonzero.  */
enum { NIL_IS_ZERO = XLI_BUILTIN_LISPSYM (iQnil) == 0 };

/* Clear the object addressed by P, with size NBYTES, so that all its
   bytes are zero and all its Lisp values are nil.  */
INLINE void
memclear (void *p, ptrdiff_t nbytes)
{
  eassert (0 <= nbytes);
  verify (NIL_IS_ZERO);
  /* Since Qnil is zero, memset suffices.  */
  memset (p, 0, nbytes);
}

/* If a struct is made to look like a vector, this macro returns the length
   of the shortest vector that would hold that struct.  */

#define VECSIZE(type)						\
  ((sizeof (type) - header_size + word_size - 1) / word_size)

/* Like VECSIZE, but used when the pseudo-vector has non-Lisp_Object fields
   at the end and we need to compute the number of Lisp_Object fields (the
   ones that the GC needs to trace).  */

#define PSEUDOVECSIZE(type, nonlispfield)			\
  ((offsetof (type, nonlispfield) - header_size) / word_size)

/* Compute A OP B, using the unsigned comparison operator OP.  A and B
   should be integer expressions.  This is not the same as
   mathematical comparison; for example, UNSIGNED_CMP (0, <, -1)
   returns true.  For efficiency, prefer plain unsigned comparison if A
   and B's sizes both fit (after integer promotion).  */
#define UNSIGNED_CMP(a, op, b)						\
  (max (sizeof ((a) + 0), sizeof ((b) + 0)) <= sizeof (unsigned)	\
   ? ((a) + (unsigned) 0) op ((b) + (unsigned) 0)			\
   : ((a) + (uintmax_t) 0) op ((b) + (uintmax_t) 0))

/* True iff C is an ASCII character.  */
#define ASCII_CHAR_P(c) UNSIGNED_CMP (c, <, 0x80)

/* A char-table is a kind of vectorlike, with contents are like a
   vector but with a few other slots.  For some purposes, it makes
   sense to handle a char-table with type struct Lisp_Vector.  An
   element of a char table can be any Lisp objects, but if it is a sub
   char-table, we treat it a table that contains information of a
   specific range of characters.  A sub char-table is like a vector but
   with two integer fields between the header and Lisp data, which means
   that it has to be marked with some precautions (see mark_char_table
   in alloc.c).  A sub char-table appears only in an element of a char-table,
   and there's no way to access it directly from Emacs Lisp program.  */

enum CHARTAB_SIZE_BITS
  {
    CHARTAB_SIZE_BITS_0 = 6,
    CHARTAB_SIZE_BITS_1 = 4,
    CHARTAB_SIZE_BITS_2 = 5,
    CHARTAB_SIZE_BITS_3 = 7
  };

extern const int chartab_size[4];

struct Lisp_Char_Table
  {
    /* HEADER.SIZE is the vector's size field, which also holds the
       pseudovector type information.  It holds the size, too.
       The size counts the defalt, parent, purpose, ascii,
       contents, and extras slots.  */
    union vectorlike_header header;

    /* This holds a default value,
       which is used whenever the value for a specific character is nil.  */
    Lisp_Object defalt;

    /* This points to another char table, which we inherit from when the
       value for a specific character is nil.  The `defalt' slot takes
       precedence over this.  */
    Lisp_Object parent;

    /* This is a symbol which says what kind of use this char-table is
       meant for.  */
    Lisp_Object purpose;

    /* The bottom sub char-table for characters of the range 0..127.  It
       is nil if none of ASCII character has a specific value.  */
    Lisp_Object ascii;

    Lisp_Object contents[(1 << CHARTAB_SIZE_BITS_0)];

    /* These hold additional data.  It is a vector.  */
    Lisp_Object extras[FLEXIBLE_ARRAY_MEMBER];
  };

INLINE bool
CHAR_TABLE_P (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_CHAR_TABLE);
}

INLINE struct Lisp_Char_Table *
XCHAR_TABLE (Lisp_Object a)
{
  eassert (CHAR_TABLE_P (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

struct Lisp_Sub_Char_Table
  {
    /* HEADER.SIZE is the vector's size field, which also holds the
       pseudovector type information.  It holds the size, too.  */
    union vectorlike_header header;

    /* Depth of this sub char-table.  It should be 1, 2, or 3.  A sub
       char-table of depth 1 contains 16 elements, and each element
       covers 4096 (128*32) characters.  A sub char-table of depth 2
       contains 32 elements, and each element covers 128 characters.  A
       sub char-table of depth 3 contains 128 elements, and each element
       is for one character.  */
    int depth;

    /* Minimum character covered by the sub char-table.  */
    int min_char;

    /* Use set_sub_char_table_contents to set this.  */
    Lisp_Object contents[FLEXIBLE_ARRAY_MEMBER];
  };

INLINE bool
SUB_CHAR_TABLE_P (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_SUB_CHAR_TABLE);
}

INLINE struct Lisp_Sub_Char_Table *
XSUB_CHAR_TABLE (Lisp_Object a)
{
  eassert (SUB_CHAR_TABLE_P (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

INLINE Lisp_Object
CHAR_TABLE_REF_ASCII (Lisp_Object ct, ptrdiff_t idx)
{
  struct Lisp_Char_Table *tbl = NULL;
  Lisp_Object val;
  do
    {
      tbl = tbl ? XCHAR_TABLE (tbl->parent) : XCHAR_TABLE (ct);
      val = (! SUB_CHAR_TABLE_P (tbl->ascii) ? tbl->ascii
	     : XSUB_CHAR_TABLE (tbl->ascii)->contents[idx]);
      if (NILP (val))
	val = tbl->defalt;
    }
  while (NILP (val) && ! NILP (tbl->parent));

  return val;
}

/* Almost equivalent to Faref (CT, IDX) with optimization for ASCII
   characters.  Do not check validity of CT.  */
INLINE Lisp_Object
CHAR_TABLE_REF (Lisp_Object ct, int idx)
{
  return (ASCII_CHAR_P (idx)
	  ? CHAR_TABLE_REF_ASCII (ct, idx)
	  : char_table_ref (ct, idx));
}

/* Equivalent to Faset (CT, IDX, VAL) with optimization for ASCII and
   8-bit European characters.  Do not check validity of CT.  */
INLINE void
CHAR_TABLE_SET (Lisp_Object ct, int idx, Lisp_Object val)
{
  if (ASCII_CHAR_P (idx) && SUB_CHAR_TABLE_P (XCHAR_TABLE (ct)->ascii))
    set_sub_char_table_contents (XCHAR_TABLE (ct)->ascii, idx, val);
  else
    char_table_set (ct, idx, val);
}

/* This structure describes a built-in function.
   It is generated by the DEFUN macro only.
   defsubr makes it into a Lisp object.  */

struct Lisp_Subr
  {
    union vectorlike_header header;
    union {
      Lisp_Object (*a0) (void);
      Lisp_Object (*a1) (Lisp_Object);
      Lisp_Object (*a2) (Lisp_Object, Lisp_Object);
      Lisp_Object (*a3) (Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a4) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a5) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a6) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a7) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*a8) (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
      Lisp_Object (*aUNEVALLED) (Lisp_Object args);
      Lisp_Object (*aMANY) (ptrdiff_t, Lisp_Object *);
    } function;
    short min_args, max_args;
    const char *symbol_name;
    const char *intspec;
    EMACS_INT doc;
  };

INLINE bool
SUBRP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_SUBR);
}

INLINE struct Lisp_Subr *
XSUBR (Lisp_Object a)
{
  eassert (SUBRP (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

enum char_table_specials
  {
    /* This is the number of slots that every char table must have.  This
       counts the ordinary slots and the top, defalt, parent, and purpose
       slots.  */
    CHAR_TABLE_STANDARD_SLOTS = PSEUDOVECSIZE (struct Lisp_Char_Table, extras),

    /* This is an index of first Lisp_Object field in Lisp_Sub_Char_Table
       when the latter is treated as an ordinary Lisp_Vector.  */
    SUB_CHAR_TABLE_OFFSET = PSEUDOVECSIZE (struct Lisp_Sub_Char_Table, contents)
  };

/* Return the number of "extra" slots in the char table CT.  */

INLINE int
CHAR_TABLE_EXTRA_SLOTS (struct Lisp_Char_Table *ct)
{
  return ((ct->header.size & PSEUDOVECTOR_SIZE_MASK)
	  - CHAR_TABLE_STANDARD_SLOTS);
}

/* Make sure that sub char-table contents slot is where we think it is.  */
verify (offsetof (struct Lisp_Sub_Char_Table, contents)
	== (offsetof (struct Lisp_Vector, contents)
	    + SUB_CHAR_TABLE_OFFSET * sizeof (Lisp_Object)));


/* Save and restore the instruction and environment pointers,
   without affecting the signal mask.  */

#ifdef HAVE__SETJMP
typedef jmp_buf sys_jmp_buf;
# define sys_setjmp(j) _setjmp (j)
# define sys_longjmp(j, v) _longjmp (j, v)
#elif defined HAVE_SIGSETJMP
typedef sigjmp_buf sys_jmp_buf;
# define sys_setjmp(j) sigsetjmp (j, 0)
# define sys_longjmp(j, v) siglongjmp (j, v)
#else
/* A platform that uses neither _longjmp nor siglongjmp; assume
   longjmp does not affect the sigmask.  */
typedef jmp_buf sys_jmp_buf;
# define sys_setjmp(j) setjmp (j)
# define sys_longjmp(j, v) longjmp (j, v)
#endif

#include "thread.h"

/***********************************************************************
			       Symbols
 ***********************************************************************/

/* Value is name of symbol.  */

INLINE Lisp_Object
(SYMBOL_VAL) (struct Lisp_Symbol *sym)
{
  return lisp_h_SYMBOL_VAL (sym);
}

INLINE struct Lisp_Symbol *
SYMBOL_ALIAS (struct Lisp_Symbol *sym)
{
  eassume (sym->u.s.redirect == SYMBOL_VARALIAS && sym->u.s.val.alias);
  return sym->u.s.val.alias;
}
INLINE struct Lisp_Buffer_Local_Value *
SYMBOL_BLV (struct Lisp_Symbol *sym)
{
  eassume (sym->u.s.redirect == SYMBOL_LOCALIZED && sym->u.s.val.blv);
  return sym->u.s.val.blv;
}
INLINE union Lisp_Fwd *
SYMBOL_FWD (struct Lisp_Symbol *sym)
{
  eassume (sym->u.s.redirect == SYMBOL_FORWARDED && sym->u.s.val.fwd);
  return sym->u.s.val.fwd;
}

INLINE void
(SET_SYMBOL_VAL) (struct Lisp_Symbol *sym, Lisp_Object v)
{
  lisp_h_SET_SYMBOL_VAL (sym, v);
}

INLINE void
SET_SYMBOL_ALIAS (struct Lisp_Symbol *sym, struct Lisp_Symbol *v)
{
  eassume (sym->u.s.redirect == SYMBOL_VARALIAS && v);
  sym->u.s.val.alias = v;
}
INLINE void
SET_SYMBOL_BLV (struct Lisp_Symbol *sym, struct Lisp_Buffer_Local_Value *v)
{
  eassume (sym->u.s.redirect == SYMBOL_LOCALIZED && v);
  sym->u.s.val.blv = v;
}
INLINE void
SET_SYMBOL_FWD (struct Lisp_Symbol *sym, union Lisp_Fwd *v)
{
  eassume (sym->u.s.redirect == SYMBOL_FORWARDED && v);
  sym->u.s.val.fwd = v;
}

INLINE Lisp_Object
SYMBOL_NAME (Lisp_Object sym)
{
  return XSYMBOL (sym)->u.s.name;
}

/* Value is true if SYM is an interned symbol.  */

INLINE bool
SYMBOL_INTERNED_P (Lisp_Object sym)
{
  return XSYMBOL (sym)->u.s.interned != SYMBOL_UNINTERNED;
}

/* Value is true if SYM is interned in initial_obarray.  */

INLINE bool
SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (Lisp_Object sym)
{
  return XSYMBOL (sym)->u.s.interned == SYMBOL_INTERNED_IN_INITIAL_OBARRAY;
}

/* Value is non-zero if symbol cannot be changed through a simple set,
   i.e. it's a constant (e.g. nil, t, :keywords), or it has some
   watching functions.  */

INLINE int
(SYMBOL_TRAPPED_WRITE_P) (Lisp_Object sym)
{
  return lisp_h_SYMBOL_TRAPPED_WRITE_P (sym);
}

/* Value is non-zero if symbol cannot be changed at all, i.e. it's a
   constant (e.g. nil, t, :keywords).  Code that actually wants to
   write to SYM, should also check whether there are any watching
   functions.  */

INLINE int
(SYMBOL_CONSTANT_P) (Lisp_Object sym)
{
  return lisp_h_SYMBOL_CONSTANT_P (sym);
}

/* Placeholder for make-docfile to process.  The actual symbol
   definition is done by lread.c's defsym.  */
#define DEFSYM(sym, name) /* empty */


/***********************************************************************
			     Hash Tables
 ***********************************************************************/

/* The structure of a Lisp hash table.  */

struct hash_table_test
{
  /* Name of the function used to compare keys.  */
  Lisp_Object name;

  /* User-supplied hash function, or nil.  */
  Lisp_Object user_hash_function;

  /* User-supplied key comparison function, or nil.  */
  Lisp_Object user_cmp_function;

  /* C function to compare two keys.  */
  bool (*cmpfn) (struct hash_table_test *t, Lisp_Object, Lisp_Object);

  /* C function to compute hash code.  */
  EMACS_UINT (*hashfn) (struct hash_table_test *t, Lisp_Object);
};

struct Lisp_Hash_Table
{
  /* This is for Lisp; the hash table code does not refer to it.  */
  union vectorlike_header header;

  /* Nil if table is non-weak.  Otherwise a symbol describing the
     weakness of the table.  */
  Lisp_Object weak;

  /* Vector of hash codes.  If hash[I] is nil, this means that the
     I-th entry is unused.  */
  Lisp_Object hash;

  /* Vector used to chain entries.  If entry I is free, next[I] is the
     entry number of the next free item.  If entry I is non-free,
     next[I] is the index of the next entry in the collision chain,
     or -1 if there is such entry.  */
  Lisp_Object next;

  /* Bucket vector.  An entry of -1 indicates no item is present,
     and a nonnegative entry is the index of the first item in
     a collision chain.  This vector's size can be larger than the
     hash table size to reduce collisions.  */
  Lisp_Object index;

  /* Only the fields above are traced normally by the GC.  The ones below
     `count' are special and are either ignored by the GC or traced in
     a special way (e.g. because of weakness).  */

  /* Number of key/value entries in the table.  */
  ptrdiff_t count;

  /* Index of first free entry in free list, or -1 if none.  */
  ptrdiff_t next_free;

  /* True if the table can be purecopied.  The table cannot be
     changed afterwards.  */
  bool pure;

  /* Resize hash table when number of entries / table size is >= this
     ratio.  */
  float rehash_threshold;

  /* Used when the table is resized.  If equal to a negative integer,
     the user rehash-size is the integer -REHASH_SIZE, and the new
     size is the old size plus -REHASH_SIZE.  If positive, the user
     rehash-size is the floating-point value REHASH_SIZE + 1, and the
     new size is the old size times REHASH_SIZE + 1.  */
  float rehash_size;

  /* Vector of keys and values.  The key of item I is found at index
     2 * I, the value is found at index 2 * I + 1.
     This is gc_marked specially if the table is weak.  */
  Lisp_Object key_and_value;

  /* The comparison and hash functions.  */
  struct hash_table_test test;

  /* Next weak hash table if this is a weak hash table.  The head
     of the list is in weak_hash_tables.  */
  struct Lisp_Hash_Table *next_weak;
};


INLINE bool
HASH_TABLE_P (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_HASH_TABLE);
}

INLINE struct Lisp_Hash_Table *
XHASH_TABLE (Lisp_Object a)
{
  eassert (HASH_TABLE_P (a));
  return XUNTAG (a, Lisp_Vectorlike);
}

#define XSET_HASH_TABLE(VAR, PTR) \
     (XSETPSEUDOVECTOR (VAR, PTR, PVEC_HASH_TABLE))

/* Value is the key part of entry IDX in hash table H.  */
INLINE Lisp_Object
HASH_KEY (struct Lisp_Hash_Table *h, ptrdiff_t idx)
{
  return AREF (h->key_and_value, 2 * idx);
}

/* Value is the value part of entry IDX in hash table H.  */
INLINE Lisp_Object
HASH_VALUE (struct Lisp_Hash_Table *h, ptrdiff_t idx)
{
  return AREF (h->key_and_value, 2 * idx + 1);
}

/* Value is the hash code computed for entry IDX in hash table H.  */
INLINE Lisp_Object
HASH_HASH (struct Lisp_Hash_Table *h, ptrdiff_t idx)
{
  return AREF (h->hash, idx);
}

/* Value is the size of hash table H.  */
INLINE ptrdiff_t
HASH_TABLE_SIZE (struct Lisp_Hash_Table *h)
{
  return ASIZE (h->next);
}

/* Default size for hash tables if not specified.  */

enum DEFAULT_HASH_SIZE { DEFAULT_HASH_SIZE = 65 };

/* Default threshold specifying when to resize a hash table.  The
   value gives the ratio of current entries in the hash table and the
   size of the hash table.  */

static float const DEFAULT_REHASH_THRESHOLD = 0.8125;

/* Default factor by which to increase the size of a hash table, minus 1.  */

static float const DEFAULT_REHASH_SIZE = 1.5 - 1;

/* Combine two integers X and Y for hashing.  The result might not fit
   into a Lisp integer.  */

INLINE EMACS_UINT
sxhash_combine (EMACS_UINT x, EMACS_UINT y)
{
  return (x << 4) + (x >> (EMACS_INT_WIDTH - 4)) + y;
}

/* Hash X, returning a value that fits into a fixnum.  */

INLINE EMACS_UINT
SXHASH_REDUCE (EMACS_UINT x)
{
  return (x ^ x >> (EMACS_INT_WIDTH - FIXNUM_BITS)) & INTMASK;
}

/* These structures are used for various misc types.  */

struct Lisp_Misc_Any		/* Supertype of all Misc types.  */
{
  ENUM_BF (Lisp_Misc_Type) type : 16;		/* = Lisp_Misc_??? */
  bool_bf gcmarkbit : 1;
  unsigned spacer : 15;
};

INLINE bool
(MISCP) (Lisp_Object x)
{
  return lisp_h_MISCP (x);
}

INLINE struct Lisp_Misc_Any *
XMISCANY (Lisp_Object a)
{
  eassert (MISCP (a));
  return XUNTAG (a, Lisp_Misc);
}

INLINE enum Lisp_Misc_Type
XMISCTYPE (Lisp_Object a)
{
  return XMISCANY (a)->type;
}

struct Lisp_Marker
{
  ENUM_BF (Lisp_Misc_Type) type : 16;		/* = Lisp_Misc_Marker */
  bool_bf gcmarkbit : 1;
  unsigned spacer : 13;
  /* This flag is temporarily used in the functions
     decode/encode_coding_object to record that the marker position
     must be adjusted after the conversion.  */
  bool_bf need_adjustment : 1;
  /* True means normal insertion at the marker's position
     leaves the marker after the inserted text.  */
  bool_bf insertion_type : 1;
  /* This is the buffer that the marker points into, or 0 if it points nowhere.
     Note: a chain of markers can contain markers pointing into different
     buffers (the chain is per buffer_text rather than per buffer, so it's
     shared between indirect buffers).  */
  /* This is used for (other than NULL-checking):
     - Fmarker_buffer
     - Fset_marker: check eq(oldbuf, newbuf) to avoid unchain+rechain.
     - unchain_marker: to find the list from which to unchain.
     - Fkill_buffer: to only unchain the markers of current indirect buffer.
     */
  struct buffer *buffer;

  /* The remaining fields are meaningless in a marker that
     does not point anywhere.  */

  /* For markers that point somewhere,
     this is used to chain of all the markers in a given buffer.  */
  /* We could remove it and use an array in buffer_text instead.
     That would also allow us to preserve it ordered.  */
  struct Lisp_Marker *next;
  /* This is the char position where the marker points.  */
  ptrdiff_t charpos;
  /* This is the byte position.
     It's mostly used as a charpos<->bytepos cache (i.e. it's not directly
     used to implement the functionality of markers, but rather to (ab)use
     markers as a cache for char<->byte mappings).  */
  ptrdiff_t bytepos;
};

/* START and END are markers in the overlay's buffer, and
   PLIST is the overlay's property list.  */
struct Lisp_Overlay
/* An overlay's real data content is:
   - plist
   - buffer (really there are two buffer pointers, one per marker,
     and both points to the same buffer)
   - insertion type of both ends (per-marker fields)
   - start & start byte (of start marker)
   - end & end byte (of end marker)
   - next (singly linked list of overlays)
   - next fields of start and end markers (singly linked list of markers).
   I.e. 9words plus 2 bits, 3words of which are for external linked lists.
*/
  {
    ENUM_BF (Lisp_Misc_Type) type : 16;	/* = Lisp_Misc_Overlay */
    bool_bf gcmarkbit : 1;
    unsigned spacer : 15;
    struct Lisp_Overlay *next;
    Lisp_Object start;
    Lisp_Object end;
    Lisp_Object plist;
  };

/* Number of bits needed to store one of the values
   SAVE_UNUSED..SAVE_OBJECT.  */
enum { SAVE_SLOT_BITS = 3 };

/* Number of slots in a save value where save_type is nonzero.  */
enum { SAVE_VALUE_SLOTS = 4 };

/* Bit-width and values for struct Lisp_Save_Value's save_type member.  */

enum { SAVE_TYPE_BITS = SAVE_VALUE_SLOTS * SAVE_SLOT_BITS + 1 };

/* Types of data which may be saved in a Lisp_Save_Value.  */

enum Lisp_Save_Type
  {
    SAVE_UNUSED,
    SAVE_INTEGER,
    SAVE_FUNCPOINTER,
    SAVE_POINTER,
    SAVE_OBJECT,
    SAVE_TYPE_INT_INT = SAVE_INTEGER + (SAVE_INTEGER << SAVE_SLOT_BITS),
    SAVE_TYPE_INT_INT_INT
      = (SAVE_INTEGER + (SAVE_TYPE_INT_INT << SAVE_SLOT_BITS)),
    SAVE_TYPE_OBJ_OBJ = SAVE_OBJECT + (SAVE_OBJECT << SAVE_SLOT_BITS),
    SAVE_TYPE_OBJ_OBJ_OBJ = SAVE_OBJECT + (SAVE_TYPE_OBJ_OBJ << SAVE_SLOT_BITS),
    SAVE_TYPE_OBJ_OBJ_OBJ_OBJ
      = SAVE_OBJECT + (SAVE_TYPE_OBJ_OBJ_OBJ << SAVE_SLOT_BITS),
    SAVE_TYPE_PTR_INT = SAVE_POINTER + (SAVE_INTEGER << SAVE_SLOT_BITS),
    SAVE_TYPE_PTR_OBJ = SAVE_POINTER + (SAVE_OBJECT << SAVE_SLOT_BITS),
    SAVE_TYPE_PTR_PTR = SAVE_POINTER + (SAVE_POINTER << SAVE_SLOT_BITS),
    SAVE_TYPE_FUNCPTR_PTR_OBJ
      = SAVE_FUNCPOINTER + (SAVE_TYPE_PTR_OBJ << SAVE_SLOT_BITS),

    /* This has an extra bit indicating it's raw memory.  */
    SAVE_TYPE_MEMORY = SAVE_TYPE_PTR_INT + (1 << (SAVE_TYPE_BITS - 1))
  };

/* SAVE_SLOT_BITS must be large enough to represent these values.  */
verify (((SAVE_UNUSED | SAVE_INTEGER | SAVE_FUNCPOINTER
	  | SAVE_POINTER | SAVE_OBJECT)
	 >> SAVE_SLOT_BITS)
	== 0);

/* Special object used to hold a different values for later use.

   This is mostly used to package C integers and pointers to call
   record_unwind_protect when two or more values need to be saved.
   For example:

   ...
     struct my_data *md = get_my_data ();
     ptrdiff_t mi = get_my_integer ();
     record_unwind_protect (my_unwind, make_save_ptr_int (md, mi));
   ...

   Lisp_Object my_unwind (Lisp_Object arg)
   {
     struct my_data *md = XSAVE_POINTER (arg, 0);
     ptrdiff_t mi = XSAVE_INTEGER (arg, 1);
     ...
   }

   If ENABLE_CHECKING is in effect, XSAVE_xxx macros do type checking of the
   saved objects and raise eassert if type of the saved object doesn't match
   the type which is extracted.  In the example above, XSAVE_INTEGER (arg, 2)
   and XSAVE_OBJECT (arg, 0) are wrong because nothing was saved in slot 2 and
   slot 0 is a pointer.  */

typedef void (*voidfuncptr) (void);

struct Lisp_Save_Value
  {
    ENUM_BF (Lisp_Misc_Type) type : 16;	/* = Lisp_Misc_Save_Value */
    bool_bf gcmarkbit : 1;
    unsigned spacer : 32 - (16 + 1 + SAVE_TYPE_BITS);

    /* V->data may hold up to SAVE_VALUE_SLOTS entries.  The type of
       V's data entries are determined by V->save_type.  E.g., if
       V->save_type == SAVE_TYPE_PTR_OBJ, V->data[0] is a pointer,
       V->data[1] is an integer, and V's other data entries are unused.

       If V->save_type == SAVE_TYPE_MEMORY, V->data[0].pointer is the address of
       a memory area containing V->data[1].integer potential Lisp_Objects.  */
    ENUM_BF (Lisp_Save_Type) save_type : SAVE_TYPE_BITS;
    union {
      void *pointer;
      voidfuncptr funcpointer;
      ptrdiff_t integer;
      Lisp_Object object;
    } data[SAVE_VALUE_SLOTS];
  };

INLINE bool
SAVE_VALUEP (Lisp_Object x)
{
  return MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Save_Value;
}

INLINE struct Lisp_Save_Value *
XSAVE_VALUE (Lisp_Object a)
{
  eassert (SAVE_VALUEP (a));
  return XUNTAG (a, Lisp_Misc);
}

/* Return the type of V's Nth saved value.  */
INLINE int
save_type (struct Lisp_Save_Value *v, int n)
{
  eassert (0 <= n && n < SAVE_VALUE_SLOTS);
  return (v->save_type >> (SAVE_SLOT_BITS * n) & ((1 << SAVE_SLOT_BITS) - 1));
}

/* Get and set the Nth saved pointer.  */

INLINE void *
XSAVE_POINTER (Lisp_Object obj, int n)
{
  eassert (save_type (XSAVE_VALUE (obj), n) == SAVE_POINTER);
  return XSAVE_VALUE (obj)->data[n].pointer;
}
INLINE void
set_save_pointer (Lisp_Object obj, int n, void *val)
{
  eassert (save_type (XSAVE_VALUE (obj), n) == SAVE_POINTER);
  XSAVE_VALUE (obj)->data[n].pointer = val;
}
INLINE voidfuncptr
XSAVE_FUNCPOINTER (Lisp_Object obj, int n)
{
  eassert (save_type (XSAVE_VALUE (obj), n) == SAVE_FUNCPOINTER);
  return XSAVE_VALUE (obj)->data[n].funcpointer;
}

/* Likewise for the saved integer.  */

INLINE ptrdiff_t
XSAVE_INTEGER (Lisp_Object obj, int n)
{
  eassert (save_type (XSAVE_VALUE (obj), n) == SAVE_INTEGER);
  return XSAVE_VALUE (obj)->data[n].integer;
}
INLINE void
set_save_integer (Lisp_Object obj, int n, ptrdiff_t val)
{
  eassert (save_type (XSAVE_VALUE (obj), n) == SAVE_INTEGER);
  XSAVE_VALUE (obj)->data[n].integer = val;
}

/* Extract Nth saved object.  */

INLINE Lisp_Object
XSAVE_OBJECT (Lisp_Object obj, int n)
{
  eassert (save_type (XSAVE_VALUE (obj), n) == SAVE_OBJECT);
  return XSAVE_VALUE (obj)->data[n].object;
}

#ifdef HAVE_MODULES
struct Lisp_User_Ptr
{
  ENUM_BF (Lisp_Misc_Type) type : 16;	     /* = Lisp_Misc_User_Ptr */
  bool_bf gcmarkbit : 1;
  unsigned spacer : 15;

  void (*finalizer) (void *);
  void *p;
};
#endif

/* A finalizer sentinel.  */
struct Lisp_Finalizer
  {
    struct Lisp_Misc_Any base;

    /* Circular list of all active weak references.  */
    struct Lisp_Finalizer *prev;
    struct Lisp_Finalizer *next;

    /* Call FUNCTION when the finalizer becomes unreachable, even if
       FUNCTION contains a reference to the finalizer; i.e., call
       FUNCTION when it is reachable _only_ through finalizers.  */
    Lisp_Object function;
  };

INLINE bool
FINALIZERP (Lisp_Object x)
{
  return MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Finalizer;
}

INLINE struct Lisp_Finalizer *
XFINALIZER (Lisp_Object a)
{
  eassert (FINALIZERP (a));
  return XUNTAG (a, Lisp_Misc);
}

/* A miscellaneous object, when it's on the free list.  */
struct Lisp_Free
  {
    ENUM_BF (Lisp_Misc_Type) type : 16;	/* = Lisp_Misc_Free */
    bool_bf gcmarkbit : 1;
    unsigned spacer : 15;
    union Lisp_Misc *chain;
  };

/* To get the type field of a union Lisp_Misc, use XMISCTYPE.
   It uses one of these struct subtypes to get the type field.  */

union Lisp_Misc
  {
    struct Lisp_Misc_Any u_any;	   /* Supertype of all Misc types.  */
    struct Lisp_Free u_free;
    struct Lisp_Marker u_marker;
    struct Lisp_Overlay u_overlay;
    struct Lisp_Save_Value u_save_value;
    struct Lisp_Finalizer u_finalizer;
#ifdef HAVE_MODULES
    struct Lisp_User_Ptr u_user_ptr;
#endif
  };

INLINE union Lisp_Misc *
XMISC (Lisp_Object a)
{
  return XUNTAG (a, Lisp_Misc);
}

INLINE bool
(MARKERP) (Lisp_Object x)
{
  return lisp_h_MARKERP (x);
}

INLINE struct Lisp_Marker *
XMARKER (Lisp_Object a)
{
  eassert (MARKERP (a));
  return XUNTAG (a, Lisp_Misc);
}

INLINE bool
OVERLAYP (Lisp_Object x)
{
  return MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Overlay;
}

INLINE struct Lisp_Overlay *
XOVERLAY (Lisp_Object a)
{
  eassert (OVERLAYP (a));
  return XUNTAG (a, Lisp_Misc);
}

#ifdef HAVE_MODULES
INLINE bool
USER_PTRP (Lisp_Object x)
{
  return MISCP (x) && XMISCTYPE (x) == Lisp_Misc_User_Ptr;
}

INLINE struct Lisp_User_Ptr *
XUSER_PTR (Lisp_Object a)
{
  eassert (USER_PTRP (a));
  return XUNTAG (a, Lisp_Misc);
}
#endif


/* Forwarding pointer to an int variable.
   This is allowed only in the value cell of a symbol,
   and it means that the symbol's value really lives in the
   specified int variable.  */
struct Lisp_Intfwd
  {
    enum Lisp_Fwd_Type type;	/* = Lisp_Fwd_Int */
    EMACS_INT *intvar;
  };

/* Boolean forwarding pointer to an int variable.
   This is like Lisp_Intfwd except that the ostensible
   "value" of the symbol is t if the bool variable is true,
   nil if it is false.  */
struct Lisp_Boolfwd
  {
    enum Lisp_Fwd_Type type;	/* = Lisp_Fwd_Bool */
    bool *boolvar;
  };

/* Forwarding pointer to a Lisp_Object variable.
   This is allowed only in the value cell of a symbol,
   and it means that the symbol's value really lives in the
   specified variable.  */
struct Lisp_Objfwd
  {
    enum Lisp_Fwd_Type type;	/* = Lisp_Fwd_Obj */
    Lisp_Object *objvar;
  };

/* Like Lisp_Objfwd except that value lives in a slot in the
   current buffer.  Value is byte index of slot within buffer.  */
struct Lisp_Buffer_Objfwd
  {
    enum Lisp_Fwd_Type type;	/* = Lisp_Fwd_Buffer_Obj */
    int offset;
    /* One of Qnil, Qintegerp, Qsymbolp, Qstringp, Qfloatp or Qnumberp.  */
    Lisp_Object predicate;
  };

/* struct Lisp_Buffer_Local_Value is used in a symbol value cell when
   the symbol has buffer-local bindings.  (Exception:
   some buffer-local variables are built-in, with their values stored
   in the buffer structure itself.  They are handled differently,
   using struct Lisp_Buffer_Objfwd.)

   The `realvalue' slot holds the variable's current value, or a
   forwarding pointer to where that value is kept.  This value is the
   one that corresponds to the loaded binding.  To read or set the
   variable, you must first make sure the right binding is loaded;
   then you can access the value in (or through) `realvalue'.

   `buffer' and `frame' are the buffer and frame for which the loaded
   binding was found.  If those have changed, to make sure the right
   binding is loaded it is necessary to find which binding goes with
   the current buffer and selected frame, then load it.  To load it,
   first unload the previous binding, then copy the value of the new
   binding into `realvalue' (or through it).  Also update
   LOADED-BINDING to point to the newly loaded binding.

   `local_if_set' indicates that merely setting the variable creates a
   local binding for the current buffer.  Otherwise the latter, setting
   the variable does not do that; only make-local-variable does that.  */

struct Lisp_Buffer_Local_Value
  {
    /* True means that merely setting the variable creates a local
       binding for the current buffer.  */
    bool_bf local_if_set : 1;
    /* True means that the binding now loaded was found.
       Presumably equivalent to (defcell!=valcell).  */
    bool_bf found : 1;
    /* If non-NULL, a forwarding to the C var where it should also be set.  */
    union Lisp_Fwd *fwd;	/* Should never be (Buffer|Kboard)_Objfwd.  */
    /* The buffer or frame for which the loaded binding was found.  */
    Lisp_Object where;
    /* A cons cell that holds the default value.  It has the form
       (SYMBOL . DEFAULT-VALUE).  */
    Lisp_Object defcell;
    /* The cons cell from `where's parameter alist.
       It always has the form (SYMBOL . VALUE)
       Note that if `forward' is non-nil, VALUE may be out of date.
       Also if the currently loaded binding is the default binding, then
       this is `eq'ual to defcell.  */
    Lisp_Object valcell;
  };

/* Like Lisp_Objfwd except that value lives in a slot in the
   current kboard.  */
struct Lisp_Kboard_Objfwd
  {
    enum Lisp_Fwd_Type type;	/* = Lisp_Fwd_Kboard_Obj */
    int offset;
  };

union Lisp_Fwd
  {
    struct Lisp_Intfwd u_intfwd;
    struct Lisp_Boolfwd u_boolfwd;
    struct Lisp_Objfwd u_objfwd;
    struct Lisp_Buffer_Objfwd u_buffer_objfwd;
    struct Lisp_Kboard_Objfwd u_kboard_objfwd;
  };

INLINE enum Lisp_Fwd_Type
XFWDTYPE (union Lisp_Fwd *a)
{
  return a->u_intfwd.type;
}

INLINE bool
BUFFER_OBJFWDP (union Lisp_Fwd *a)
{
  return XFWDTYPE (a) == Lisp_Fwd_Buffer_Obj;
}

INLINE struct Lisp_Buffer_Objfwd *
XBUFFER_OBJFWD (union Lisp_Fwd *a)
{
  eassert (BUFFER_OBJFWDP (a));
  return &a->u_buffer_objfwd;
}

/* Lisp floating point type.  */
struct Lisp_Float
  {
    union
    {
      double data;
      struct Lisp_Float *chain;
    } u;
  };

INLINE bool
(FLOATP) (Lisp_Object x)
{
  return lisp_h_FLOATP (x);
}

INLINE struct Lisp_Float *
XFLOAT (Lisp_Object a)
{
  eassert (FLOATP (a));
  return XUNTAG (a, Lisp_Float);
}

INLINE double
XFLOAT_DATA (Lisp_Object f)
{
  return XFLOAT (f)->u.data;
}

/* Most hosts nowadays use IEEE floating point, so they use IEC 60559
   representations, have infinities and NaNs, and do not trap on
   exceptions.  Define IEEE_FLOATING_POINT if this host is one of the
   typical ones.  The C11 macro __STDC_IEC_559__ is close to what is
   wanted here, but is not quite right because Emacs does not require
   all the features of C11 Annex F (and does not require C11 at all,
   for that matter).  */
enum
  {
    IEEE_FLOATING_POINT
      = (FLT_RADIX == 2 && FLT_MANT_DIG == 24
	 && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128)
  };

/* A character, declared with the following typedef, is a member
   of some character set associated with the current buffer.  */
#ifndef _UCHAR_T  /* Protect against something in ctab.h on AIX.  */
#define _UCHAR_T
typedef unsigned char UCHAR;
#endif

/* Meanings of slots in a Lisp_Compiled:  */

enum Lisp_Compiled
  {
    COMPILED_ARGLIST = 0,
    COMPILED_BYTECODE = 1,
    COMPILED_CONSTANTS = 2,
    COMPILED_STACK_DEPTH = 3,
    COMPILED_DOC_STRING = 4,
    COMPILED_INTERACTIVE = 5
  };

/* Flag bits in a character.  These also get used in termhooks.h.
   Richard Stallman <rms@gnu.ai.mit.edu> thinks that MULE
   (MUlti-Lingual Emacs) might need 22 bits for the character value
   itself, so we probably shouldn't use any bits lower than 0x0400000.  */
enum char_bits
  {
    CHAR_ALT = 0x0400000,
    CHAR_SUPER = 0x0800000,
    CHAR_HYPER = 0x1000000,
    CHAR_SHIFT = 0x2000000,
    CHAR_CTL = 0x4000000,
    CHAR_META = 0x8000000,

    CHAR_MODIFIER_MASK =
      CHAR_ALT | CHAR_SUPER | CHAR_HYPER | CHAR_SHIFT | CHAR_CTL | CHAR_META,

    /* Actually, the current Emacs uses 22 bits for the character value
       itself.  */
    CHARACTERBITS = 22
  };

/* Data type checking.  */

INLINE bool
NUMBERP (Lisp_Object x)
{
  return INTEGERP (x) || FLOATP (x);
}
INLINE bool
NATNUMP (Lisp_Object x)
{
  return INTEGERP (x) && 0 <= XINT (x);
}

INLINE bool
RANGED_INTEGERP (intmax_t lo, Lisp_Object x, intmax_t hi)
{
  return INTEGERP (x) && lo <= XINT (x) && XINT (x) <= hi;
}

#define TYPE_RANGED_INTEGERP(type, x) \
  (INTEGERP (x)			      \
   && (TYPE_SIGNED (type) ? TYPE_MINIMUM (type) <= XINT (x) : 0 <= XINT (x)) \
   && XINT (x) <= TYPE_MAXIMUM (type))

INLINE bool
AUTOLOADP (Lisp_Object x)
{
  return CONSP (x) && EQ (Qautoload, XCAR (x));
}


/* Test for specific pseudovector types.  */

INLINE bool
WINDOW_CONFIGURATIONP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_WINDOW_CONFIGURATION);
}

INLINE bool
COMPILEDP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_COMPILED);
}

INLINE bool
FRAMEP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_FRAME);
}

INLINE bool
RECORDP (Lisp_Object a)
{
  return PSEUDOVECTORP (a, PVEC_RECORD);
}

INLINE void
CHECK_RECORD (Lisp_Object x)
{
  CHECK_TYPE (RECORDP (x), Qrecordp, x);
}

/* Test for image (image . spec)  */
INLINE bool
IMAGEP (Lisp_Object x)
{
  return CONSP (x) && EQ (XCAR (x), Qimage);
}

/* Array types.  */
INLINE bool
ARRAYP (Lisp_Object x)
{
  return VECTORP (x) || STRINGP (x) || CHAR_TABLE_P (x) || BOOL_VECTOR_P (x);
}

INLINE void
CHECK_LIST (Lisp_Object x)
{
  CHECK_TYPE (CONSP (x) || NILP (x), Qlistp, x);
}

INLINE void
CHECK_LIST_END (Lisp_Object x, Lisp_Object y)
{
  CHECK_TYPE (NILP (x), Qlistp, y);
}

INLINE void
(CHECK_NUMBER) (Lisp_Object x)
{
  lisp_h_CHECK_NUMBER (x);
}

INLINE void
CHECK_STRING_CAR (Lisp_Object x)
{
  CHECK_TYPE (STRINGP (XCAR (x)), Qstringp, XCAR (x));
}
/* This is a bit special because we always need size afterwards.  */
INLINE ptrdiff_t
CHECK_VECTOR_OR_STRING (Lisp_Object x)
{
  if (VECTORP (x))
    return ASIZE (x);
  if (STRINGP (x))
    return SCHARS (x);
  wrong_type_argument (Qarrayp, x);
}
INLINE void
CHECK_ARRAY (Lisp_Object x, Lisp_Object predicate)
{
  CHECK_TYPE (ARRAYP (x), predicate, x);
}
INLINE void
CHECK_NATNUM (Lisp_Object x)
{
  CHECK_TYPE (NATNUMP (x), Qwholenump, x);
}

#define CHECK_RANGED_INTEGER(x, lo, hi)					\
  do {									\
    CHECK_NUMBER (x);							\
    if (! ((lo) <= XINT (x) && XINT (x) <= (hi)))			\
      args_out_of_range_3						\
	(x,								\
	 make_number ((lo) < 0 && (lo) < MOST_NEGATIVE_FIXNUM		\
		      ? MOST_NEGATIVE_FIXNUM				\
		      : (lo)),						\
	 make_number (min (hi, MOST_POSITIVE_FIXNUM)));			\
  } while (false)
#define CHECK_TYPE_RANGED_INTEGER(type, x) \
  do {									\
    if (TYPE_SIGNED (type))						\
      CHECK_RANGED_INTEGER (x, TYPE_MINIMUM (type), TYPE_MAXIMUM (type)); \
    else								\
      CHECK_RANGED_INTEGER (x, 0, TYPE_MAXIMUM (type));			\
  } while (false)

#define CHECK_NUMBER_COERCE_MARKER(x)					\
  do {									\
    if (MARKERP ((x)))							\
      XSETFASTINT (x, marker_position (x));				\
    else								\
      CHECK_TYPE (INTEGERP (x), Qinteger_or_marker_p, x);		\
  } while (false)

INLINE double
XFLOATINT (Lisp_Object n)
{
  return FLOATP (n) ? XFLOAT_DATA (n) : XINT (n);
}

INLINE void
CHECK_NUMBER_OR_FLOAT (Lisp_Object x)
{
  CHECK_TYPE (NUMBERP (x), Qnumberp, x);
}

#define CHECK_NUMBER_OR_FLOAT_COERCE_MARKER(x)				\
  do {									\
    if (MARKERP (x))							\
      XSETFASTINT (x, marker_position (x));				\
    else								\
      CHECK_TYPE (NUMBERP (x), Qnumber_or_marker_p, x);			\
  } while (false)

/* Since we can't assign directly to the CAR or CDR fields of a cons
   cell, use these when checking that those fields contain numbers.  */
INLINE void
CHECK_NUMBER_CAR (Lisp_Object x)
{
  Lisp_Object tmp = XCAR (x);
  CHECK_NUMBER (tmp);
  XSETCAR (x, tmp);
}

INLINE void
CHECK_NUMBER_CDR (Lisp_Object x)
{
  Lisp_Object tmp = XCDR (x);
  CHECK_NUMBER (tmp);
  XSETCDR (x, tmp);
}

/* Define a built-in function for calling from Lisp.
 `lname' should be the name to give the function in Lisp,
    as a null-terminated C string.
 `fnname' should be the name of the function in C.
    By convention, it starts with F.
 `sname' should be the name for the C constant structure
    that records information on this function for internal use.
    By convention, it should be the same as `fnname' but with S instead of F.
    It's too bad that C macros can't compute this from `fnname'.
 `minargs' should be a number, the minimum number of arguments allowed.
 `maxargs' should be a number, the maximum number of arguments allowed,
    or else MANY or UNEVALLED.
    MANY means pass a vector of evaluated arguments,
	 in the form of an integer number-of-arguments
	 followed by the address of a vector of Lisp_Objects
	 which contains the argument values.
    UNEVALLED means pass the list of unevaluated arguments
 `intspec' says how interactive arguments are to be fetched.
    If the string starts with a `(', `intspec' is evaluated and the resulting
    list is the list of arguments.
    If it's a string that doesn't start with `(', the value should follow
    the one of the doc string for `interactive'.
    A null string means call interactively with no arguments.
 `doc' is documentation for the user.  */

/* This version of DEFUN declares a function prototype with the right
   arguments, so we can catch errors with maxargs at compile-time.  */
#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc)	\
   static struct Lisp_Subr sname =				\
     { { PVEC_SUBR << PSEUDOVECTOR_AREA_BITS },				\
       { .a ## maxargs = fnname },					\
       minargs, maxargs, lname, intspec, 0};				\
   Lisp_Object fnname

/* defsubr (Sname);
   is how we define the symbol for function `name' at start-up time.  */
extern void defsubr (struct Lisp_Subr *);

enum maxargs
  {
    MANY = -2,
    UNEVALLED = -1
  };

/* Call a function F that accepts many args, passing it ARRAY's elements.  */
#define CALLMANY(f, array) (f) (ARRAYELTS (array), array)

/* Call a function F that accepts many args, passing it the remaining args,
   E.g., 'return CALLN (Fformat, fmt, text);' is less error-prone than
   '{ Lisp_Object a[2]; a[0] = fmt; a[1] = text; return Fformat (2, a); }'.
   CALLN is overkill for simple usages like 'Finsert (1, &text);'.  */
#define CALLN(f, ...) CALLMANY (f, ((Lisp_Object []) {__VA_ARGS__}))

extern void defvar_lisp (struct Lisp_Objfwd *, const char *, Lisp_Object *);
extern void defvar_lisp_nopro (struct Lisp_Objfwd *, const char *, Lisp_Object *);
extern void defvar_bool (struct Lisp_Boolfwd *, const char *, bool *);
extern void defvar_int (struct Lisp_Intfwd *, const char *, EMACS_INT *);
extern void defvar_kboard (struct Lisp_Kboard_Objfwd *, const char *, int);

/* Macros we use to define forwarded Lisp variables.
   These are used in the syms_of_FILENAME functions.

   An ordinary (not in buffer_defaults, per-buffer, or per-keyboard)
   lisp variable is actually a field in `struct emacs_globals'.  The
   field's name begins with "f_", which is a convention enforced by
   these macros.  Each such global has a corresponding #define in
   globals.h; the plain name should be used in the code.

   E.g., the global "cons_cells_consed" is declared as "int
   f_cons_cells_consed" in globals.h, but there is a define:

      #define cons_cells_consed globals.f_cons_cells_consed

   All C code uses the `cons_cells_consed' name.  This is all done
   this way to support indirection for multi-threaded Emacs.  */

#define DEFVAR_LISP(lname, vname, doc)		\
  do {						\
    static struct Lisp_Objfwd o_fwd;		\
    defvar_lisp (&o_fwd, lname, &globals.f_ ## vname);		\
  } while (false)
#define DEFVAR_LISP_NOPRO(lname, vname, doc)	\
  do {						\
    static struct Lisp_Objfwd o_fwd;		\
    defvar_lisp_nopro (&o_fwd, lname, &globals.f_ ## vname);	\
  } while (false)
#define DEFVAR_BOOL(lname, vname, doc)		\
  do {						\
    static struct Lisp_Boolfwd b_fwd;		\
    defvar_bool (&b_fwd, lname, &globals.f_ ## vname);		\
  } while (false)
#define DEFVAR_INT(lname, vname, doc)		\
  do {						\
    static struct Lisp_Intfwd i_fwd;		\
    defvar_int (&i_fwd, lname, &globals.f_ ## vname);		\
  } while (false)

#define DEFVAR_KBOARD(lname, vname, doc)			\
  do {								\
    static struct Lisp_Kboard_Objfwd ko_fwd;			\
    defvar_kboard (&ko_fwd, lname, offsetof (KBOARD, vname ## _)); \
  } while (false)


/* Elisp uses several stacks:
   - the C stack.
   - the bytecode stack: used internally by the bytecode interpreter.
     Allocated from the C stack.
   - The specpdl stack: keeps track of active unwind-protect and
     dynamic-let-bindings.  Allocated from the `specpdl' array, a manually
     managed stack.
   - The handler stack: keeps track of active catch tags and condition-case
     handlers.  Allocated in a manually managed stack implemented by a
     doubly-linked list allocated via xmalloc and never freed.  */

/* Structure for recording Lisp call stack for backtrace purposes.  */

/* The special binding stack holds the outer values of variables while
   they are bound by a function application or a let form, stores the
   code to be executed for unwind-protect forms.

   NOTE: The specbinding union is defined here, because SPECPDL_INDEX is
   used all over the place, needs to be fast, and needs to know the size of
   union specbinding.  But only eval.c should access it.  */

enum specbind_tag {
  SPECPDL_UNWIND,		/* An unwind_protect function on Lisp_Object.  */
  SPECPDL_UNWIND_PTR,		/* Likewise, on void *.  */
  SPECPDL_UNWIND_INT,		/* Likewise, on int.  */
  SPECPDL_UNWIND_VOID,		/* Likewise, with no arg.  */
  SPECPDL_BACKTRACE,		/* An element of the backtrace.  */
  SPECPDL_LET,			/* A plain and simple dynamic let-binding.  */
  /* Tags greater than SPECPDL_LET must be "subkinds" of LET.  */
  SPECPDL_LET_LOCAL,		/* A buffer-local let-binding.  */
  SPECPDL_LET_DEFAULT		/* A global binding for a localized var.  */
};

union specbinding
  {
    ENUM_BF (specbind_tag) kind : CHAR_BIT;
    struct {
      ENUM_BF (specbind_tag) kind : CHAR_BIT;
      void (*func) (Lisp_Object);
      Lisp_Object arg;
    } unwind;
    struct {
      ENUM_BF (specbind_tag) kind : CHAR_BIT;
      void (*func) (void *);
      void *arg;
    } unwind_ptr;
    struct {
      ENUM_BF (specbind_tag) kind : CHAR_BIT;
      void (*func) (int);
      int arg;
    } unwind_int;
    struct {
      ENUM_BF (specbind_tag) kind : CHAR_BIT;
      void (*func) (void);
    } unwind_void;
    struct {
      ENUM_BF (specbind_tag) kind : CHAR_BIT;
      /* `where' is not used in the case of SPECPDL_LET.  */
      Lisp_Object symbol, old_value, where;
      /* Normally this is unused; but it is set to the symbol's
	 current value when a thread is swapped out.  */
      Lisp_Object saved_value;
    } let;
    struct {
      ENUM_BF (specbind_tag) kind : CHAR_BIT;
      bool_bf debug_on_exit : 1;
      Lisp_Object function;
      Lisp_Object *args;
      ptrdiff_t nargs;
    } bt;
  };

/* These 3 are defined as macros in thread.h.  */
/* extern union specbinding *specpdl; */
/* extern union specbinding *specpdl_ptr; */
/* extern ptrdiff_t specpdl_size; */

INLINE ptrdiff_t
SPECPDL_INDEX (void)
{
  return specpdl_ptr - specpdl;
}

/* This structure helps implement the `catch/throw' and `condition-case/signal'
   control structures.  A struct handler contains all the information needed to
   restore the state of the interpreter after a non-local jump.

   handler structures are chained together in a doubly linked list; the `next'
   member points to the next outer catchtag and the `nextfree' member points in
   the other direction to the next inner element (which is typically the next
   free element since we mostly use it on the deepest handler).

   A call like (throw TAG VAL) searches for a catchtag whose `tag_or_ch'
   member is TAG, and then unbinds to it.  The `val' member is used to
   hold VAL while the stack is unwound; `val' is returned as the value
   of the catch form.  If there is a handler of type CATCHER_ALL, it will
   be treated as a handler for all invocations of `throw'; in this case
   `val' will be set to (TAG . VAL).

   All the other members are concerned with restoring the interpreter
   state.

   Members are volatile if their values need to survive _longjmp when
   a 'struct handler' is a local variable.  */

enum handlertype { CATCHER, CONDITION_CASE, CATCHER_ALL };

struct handler
{
  enum handlertype type;
  Lisp_Object tag_or_ch;
  Lisp_Object val;
  struct handler *next;
  struct handler *nextfree;

  /* The bytecode interpreter can have several handlers active at the same
     time, so when we longjmp to one of them, it needs to know which handler
     this was and what was the corresponding internal state.  This is stored
     here, and when we longjmp we make sure that handlerlist points to the
     proper handler.  */
  Lisp_Object *bytecode_top;
  int bytecode_dest;

  /* Most global vars are reset to their value via the specpdl mechanism,
     but a few others are handled by storing their value here.  */
  sys_jmp_buf jmp;
  EMACS_INT f_lisp_eval_depth;
  ptrdiff_t pdlcount;
  int poll_suppress_count;
  int interrupt_input_blocked;
};

extern Lisp_Object memory_signal_data;

extern void maybe_quit (void);

/* True if ought to quit now.  */

#define QUITP (!NILP (Vquit_flag) && NILP (Vinhibit_quit))

/* Process a quit rarely, based on a counter COUNT, for efficiency.
   "Rarely" means once per USHRT_MAX + 1 times; this is somewhat
   arbitrary, but efficient.  */

INLINE void
rarely_quit (unsigned short int count)
{
  if (! count)
    maybe_quit ();
}

extern Lisp_Object Vascii_downcase_table;
extern Lisp_Object Vascii_canon_table;

/* Call staticpro (&var) to protect static variable `var'.  */

void staticpro (Lisp_Object *);

/* Forward declarations for prototypes.  */
struct window;
struct frame;

/* Copy COUNT Lisp_Objects from ARGS to contents of V starting from OFFSET.  */

INLINE void
vcopy (Lisp_Object v, ptrdiff_t offset, Lisp_Object *args, ptrdiff_t count)
{
  eassert (0 <= offset && 0 <= count && offset + count <= ASIZE (v));
  memcpy (XVECTOR (v)->contents + offset, args, count * sizeof *args);
}

/* Functions to modify hash tables.  */

INLINE void
set_hash_key_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->key_and_value, 2 * idx, val);
}

INLINE void
set_hash_value_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->key_and_value, 2 * idx + 1, val);
}

/* Use these functions to set Lisp_Object
   or pointer slots of struct Lisp_Symbol.  */

INLINE void
set_symbol_function (Lisp_Object sym, Lisp_Object function)
{
  XSYMBOL (sym)->u.s.function = function;
}

INLINE void
set_symbol_plist (Lisp_Object sym, Lisp_Object plist)
{
  XSYMBOL (sym)->u.s.plist = plist;
}

INLINE void
set_symbol_next (Lisp_Object sym, struct Lisp_Symbol *next)
{
  XSYMBOL (sym)->u.s.next = next;
}

INLINE void
make_symbol_constant (Lisp_Object sym)
{
  XSYMBOL (sym)->u.s.trapped_write = SYMBOL_NOWRITE;
}

/* Buffer-local variable access functions.  */

INLINE int
blv_found (struct Lisp_Buffer_Local_Value *blv)
{
  eassert (blv->found == !EQ (blv->defcell, blv->valcell));
  return blv->found;
}

/* Set overlay's property list.  */

INLINE void
set_overlay_plist (Lisp_Object overlay, Lisp_Object plist)
{
  XOVERLAY (overlay)->plist = plist;
}

/* Get text properties of S.  */

INLINE INTERVAL
string_intervals (Lisp_Object s)
{
  return XSTRING (s)->u.s.intervals;
}

/* Set text properties of S to I.  */

INLINE void
set_string_intervals (Lisp_Object s, INTERVAL i)
{
  XSTRING (s)->u.s.intervals = i;
}

/* Set a Lisp slot in TABLE to VAL.  Most code should use this instead
   of setting slots directly.  */

INLINE void
set_char_table_defalt (Lisp_Object table, Lisp_Object val)
{
  XCHAR_TABLE (table)->defalt = val;
}
INLINE void
set_char_table_purpose (Lisp_Object table, Lisp_Object val)
{
  XCHAR_TABLE (table)->purpose = val;
}

/* Set different slots in (sub)character tables.  */

INLINE void
set_char_table_extras (Lisp_Object table, ptrdiff_t idx, Lisp_Object val)
{
  eassert (0 <= idx && idx < CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (table)));
  XCHAR_TABLE (table)->extras[idx] = val;
}

INLINE void
set_char_table_contents (Lisp_Object table, ptrdiff_t idx, Lisp_Object val)
{
  eassert (0 <= idx && idx < (1 << CHARTAB_SIZE_BITS_0));
  XCHAR_TABLE (table)->contents[idx] = val;
}

INLINE void
set_sub_char_table_contents (Lisp_Object table, ptrdiff_t idx, Lisp_Object val)
{
  XSUB_CHAR_TABLE (table)->contents[idx] = val;
}

/* Defined in data.c.  */
extern _Noreturn void wrong_choice (Lisp_Object, Lisp_Object);
extern void notify_variable_watchers (Lisp_Object, Lisp_Object,
				      Lisp_Object, Lisp_Object);
extern Lisp_Object indirect_function (Lisp_Object);
extern Lisp_Object find_symbol_value (Lisp_Object);
enum Arith_Comparison {
  ARITH_EQUAL,
  ARITH_NOTEQUAL,
  ARITH_LESS,
  ARITH_GRTR,
  ARITH_LESS_OR_EQUAL,
  ARITH_GRTR_OR_EQUAL
};
extern Lisp_Object arithcompare (Lisp_Object num1, Lisp_Object num2,
                                 enum Arith_Comparison comparison);

/* Convert the integer I to an Emacs representation, either the integer
   itself, or a cons of two or three integers, or if all else fails a float.
   I should not have side effects.  */
#define INTEGER_TO_CONS(i)					    \
  (! FIXNUM_OVERFLOW_P (i)					    \
   ? make_number (i)						    \
   : EXPR_SIGNED (i) ? intbig_to_lisp (i) : uintbig_to_lisp (i))
extern Lisp_Object intbig_to_lisp (intmax_t);
extern Lisp_Object uintbig_to_lisp (uintmax_t);

/* Convert the Emacs representation CONS back to an integer of type
   TYPE, storing the result the variable VAR.  Signal an error if CONS
   is not a valid representation or is out of range for TYPE.  */
#define CONS_TO_INTEGER(cons, type, var)				\
 (TYPE_SIGNED (type)							\
  ? ((var) = cons_to_signed (cons, TYPE_MINIMUM (type), TYPE_MAXIMUM (type))) \
  : ((var) = cons_to_unsigned (cons, TYPE_MAXIMUM (type))))
extern intmax_t cons_to_signed (Lisp_Object, intmax_t, intmax_t);
extern uintmax_t cons_to_unsigned (Lisp_Object, uintmax_t);

extern struct Lisp_Symbol *indirect_variable (struct Lisp_Symbol *);
extern _Noreturn void args_out_of_range (Lisp_Object, Lisp_Object);
extern _Noreturn void args_out_of_range_3 (Lisp_Object, Lisp_Object,
					   Lisp_Object);
extern _Noreturn void circular_list (Lisp_Object);
extern Lisp_Object do_symval_forwarding (union Lisp_Fwd *);
enum Set_Internal_Bind {
  SET_INTERNAL_SET,
  SET_INTERNAL_BIND,
  SET_INTERNAL_UNBIND,
  SET_INTERNAL_THREAD_SWITCH
};
extern void set_internal (Lisp_Object, Lisp_Object, Lisp_Object,
                          enum Set_Internal_Bind);
extern void set_default_internal (Lisp_Object, Lisp_Object,
                                  enum Set_Internal_Bind bindflag);

extern void syms_of_data (void);
extern void swap_in_global_binding (struct Lisp_Symbol *);

/* Defined in cmds.c */
extern void syms_of_cmds (void);
extern void keys_of_cmds (void);

/* Defined in coding.c.  */
extern Lisp_Object detect_coding_system (const unsigned char *, ptrdiff_t,
                                         ptrdiff_t, bool, bool, Lisp_Object);
extern void init_coding (void);
extern void init_coding_once (void);
extern void syms_of_coding (void);

/* Defined in character.c.  */
extern ptrdiff_t chars_in_text (const unsigned char *, ptrdiff_t);
extern ptrdiff_t multibyte_chars_in_text (const unsigned char *, ptrdiff_t);
extern void syms_of_character (void);

/* Defined in charset.c.  */
extern void init_charset (void);
extern void init_charset_once (void);
extern void syms_of_charset (void);
/* Structure forward declarations.  */
struct charset;

/* Defined in syntax.c.  */
extern void init_syntax_once (void);
extern void syms_of_syntax (void);

/* Defined in fns.c.  */
enum { NEXT_ALMOST_PRIME_LIMIT = 11 };
extern EMACS_INT next_almost_prime (EMACS_INT) ATTRIBUTE_CONST;
extern Lisp_Object larger_vector (Lisp_Object, ptrdiff_t, ptrdiff_t);
extern void sweep_weak_hash_tables (void);
extern char *extract_data_from_object (Lisp_Object, ptrdiff_t *, ptrdiff_t *);
EMACS_UINT hash_string (char const *, ptrdiff_t);
EMACS_UINT sxhash (Lisp_Object, int);
Lisp_Object make_hash_table (struct hash_table_test, EMACS_INT, float, float,
			     Lisp_Object, bool);
ptrdiff_t hash_lookup (struct Lisp_Hash_Table *, Lisp_Object, EMACS_UINT *);
ptrdiff_t hash_put (struct Lisp_Hash_Table *, Lisp_Object, Lisp_Object,
		    EMACS_UINT);
void hash_remove_from_table (struct Lisp_Hash_Table *, Lisp_Object);
extern struct hash_table_test const hashtest_eq, hashtest_eql, hashtest_equal;
extern void validate_subarray (Lisp_Object, Lisp_Object, Lisp_Object,
			       ptrdiff_t, ptrdiff_t *, ptrdiff_t *);
extern Lisp_Object substring_both (Lisp_Object, ptrdiff_t, ptrdiff_t,
				   ptrdiff_t, ptrdiff_t);
extern Lisp_Object merge (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object do_yes_or_no_p (Lisp_Object);
extern Lisp_Object concat2 (Lisp_Object, Lisp_Object);
extern Lisp_Object concat3 (Lisp_Object, Lisp_Object, Lisp_Object);
extern bool equal_no_quit (Lisp_Object, Lisp_Object);
extern Lisp_Object nconc2 (Lisp_Object, Lisp_Object);
extern Lisp_Object assq_no_quit (Lisp_Object, Lisp_Object);
extern Lisp_Object assoc_no_quit (Lisp_Object, Lisp_Object);
extern void clear_string_char_byte_cache (void);
extern ptrdiff_t string_char_to_byte (Lisp_Object, ptrdiff_t);
extern ptrdiff_t string_byte_to_char (Lisp_Object, ptrdiff_t);
extern Lisp_Object string_to_multibyte (Lisp_Object);
extern Lisp_Object string_make_unibyte (Lisp_Object);
extern void syms_of_fns (void);

/* Defined in floatfns.c.  */
extern void syms_of_floatfns (void);
extern Lisp_Object fmod_float (Lisp_Object x, Lisp_Object y);

/* Defined in fringe.c.  */
extern void syms_of_fringe (void);
extern void init_fringe (void);
#ifdef HAVE_WINDOW_SYSTEM
extern void mark_fringe_data (void);
extern void init_fringe_once (void);
#endif /* HAVE_WINDOW_SYSTEM */

/* Defined in image.c.  */
extern int x_bitmap_mask (struct frame *, ptrdiff_t);
extern void reset_image_types (void);
extern void syms_of_image (void);

/* Defined in insdel.c.  */
extern void move_gap_both (ptrdiff_t, ptrdiff_t);
extern _Noreturn void buffer_overflow (void);
extern void make_gap (ptrdiff_t);
extern void make_gap_1 (struct buffer *, ptrdiff_t);
extern ptrdiff_t copy_text (const unsigned char *, unsigned char *,
			    ptrdiff_t, bool, bool);
extern int count_combining_before (const unsigned char *,
				   ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern int count_combining_after (const unsigned char *,
				  ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern void insert (const char *, ptrdiff_t);
extern void insert_and_inherit (const char *, ptrdiff_t);
extern void insert_1_both (const char *, ptrdiff_t, ptrdiff_t,
			   bool, bool, bool);
extern void insert_from_gap (ptrdiff_t, ptrdiff_t, bool text_at_gap_tail);
extern void insert_from_string (Lisp_Object, ptrdiff_t, ptrdiff_t,
				ptrdiff_t, ptrdiff_t, bool);
extern void insert_from_buffer (struct buffer *, ptrdiff_t, ptrdiff_t, bool);
extern void insert_char (int);
extern void insert_string (const char *);
extern void insert_before_markers (const char *, ptrdiff_t);
extern void insert_before_markers_and_inherit (const char *, ptrdiff_t);
extern void insert_from_string_before_markers (Lisp_Object, ptrdiff_t,
					       ptrdiff_t, ptrdiff_t,
					       ptrdiff_t, bool);
extern void del_range (ptrdiff_t, ptrdiff_t);
extern Lisp_Object del_range_1 (ptrdiff_t, ptrdiff_t, bool, bool);
extern void del_range_byte (ptrdiff_t, ptrdiff_t);
extern void del_range_both (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object del_range_2 (ptrdiff_t, ptrdiff_t,
				ptrdiff_t, ptrdiff_t, bool);
extern void modify_text (ptrdiff_t, ptrdiff_t);
extern void prepare_to_modify_buffer (ptrdiff_t, ptrdiff_t, ptrdiff_t *);
extern void prepare_to_modify_buffer_1 (ptrdiff_t, ptrdiff_t, ptrdiff_t *);
extern void invalidate_buffer_caches (struct buffer *, ptrdiff_t, ptrdiff_t);
extern void signal_after_change (ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern void adjust_after_insert (ptrdiff_t, ptrdiff_t, ptrdiff_t,
				 ptrdiff_t, ptrdiff_t);
extern void adjust_markers_for_delete (ptrdiff_t, ptrdiff_t,
				       ptrdiff_t, ptrdiff_t);
extern void adjust_markers_bytepos (ptrdiff_t, ptrdiff_t,
				    ptrdiff_t, ptrdiff_t, int);
extern void replace_range (ptrdiff_t, ptrdiff_t, Lisp_Object, bool, bool, bool, bool);
extern void replace_range_2 (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t,
			     const char *, ptrdiff_t, ptrdiff_t, bool);
extern void syms_of_insdel (void);

/* Defined in dispnew.c.  */
#if (defined PROFILING \
     && (defined __FreeBSD__ || defined GNU_LINUX || defined __MINGW32__))
_Noreturn void __executable_start (void);
#endif
extern Lisp_Object Vwindow_system;
extern Lisp_Object sit_for (Lisp_Object, bool, int);

/* Defined in xdisp.c.  */
extern bool noninteractive_need_newline;
extern Lisp_Object echo_area_buffer[2];
extern void add_to_log (char const *, ...);
extern void vadd_to_log (char const *, va_list);
extern void check_message_stack (void);
extern void setup_echo_area_for_printing (bool);
extern bool push_message (void);
extern void pop_message_unwind (void);
extern Lisp_Object restore_message_unwind (Lisp_Object);
extern void restore_message (void);
extern Lisp_Object current_message (void);
extern void clear_message (bool, bool);
extern void message (const char *, ...) ATTRIBUTE_FORMAT_PRINTF (1, 2);
extern void message1 (const char *);
extern void message1_nolog (const char *);
extern void message3 (Lisp_Object);
extern void message3_nolog (Lisp_Object);
extern void message_dolog (const char *, ptrdiff_t, bool, bool);
extern void message_with_string (const char *, Lisp_Object, bool);
extern void message_log_maybe_newline (void);
extern void update_echo_area (void);
extern void truncate_echo_area (ptrdiff_t);
extern void redisplay (void);

void set_frame_cursor_types (struct frame *, Lisp_Object);
extern void syms_of_xdisp (void);
extern void init_xdisp (void);
extern Lisp_Object safe_eval (Lisp_Object);
extern bool pos_visible_p (struct window *, ptrdiff_t, int *,
			   int *, int *, int *, int *, int *);

/* Defined in xsettings.c.  */
extern void syms_of_xsettings (void);

/* Defined in vm-limit.c.  */
extern void memory_warnings (void *, void (*warnfun) (const char *));

/* Defined in character.c.  */
extern void parse_str_as_multibyte (const unsigned char *, ptrdiff_t,
				    ptrdiff_t *, ptrdiff_t *);

/* Defined in alloc.c.  */
extern void *my_heap_start (void);
extern void check_pure_size (void);
extern void free_misc (Lisp_Object);
extern void allocate_string_data (struct Lisp_String *, EMACS_INT, EMACS_INT);
extern void malloc_warning (const char *);
extern _Noreturn void memory_full (size_t);
extern _Noreturn void buffer_memory_full (ptrdiff_t);
extern bool survives_gc_p (Lisp_Object);
extern void mark_object (Lisp_Object);
#if defined REL_ALLOC && !defined SYSTEM_MALLOC && !defined HYBRID_MALLOC
extern void refill_memory_reserve (void);
#endif
extern void alloc_unexec_pre (void);
extern void alloc_unexec_post (void);
extern void mark_stack (char *, char *);
extern void flush_stack_call_func (void (*func) (void *arg), void *arg);
extern const char *pending_malloc_warning;
extern Lisp_Object zero_vector;
extern EMACS_INT consing_since_gc;
extern EMACS_INT gc_relative_threshold;
extern EMACS_INT memory_full_cons_threshold;
extern Lisp_Object list1 (Lisp_Object);
extern Lisp_Object list2 (Lisp_Object, Lisp_Object);
extern Lisp_Object list3 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object list4 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object list5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object,
			  Lisp_Object);
enum constype {CONSTYPE_HEAP, CONSTYPE_PURE};
extern Lisp_Object listn (enum constype, ptrdiff_t, Lisp_Object, ...);

/* Build a frequently used 2/3/4-integer lists.  */

INLINE Lisp_Object
list2i (EMACS_INT x, EMACS_INT y)
{
  return list2 (make_number (x), make_number (y));
}

INLINE Lisp_Object
list3i (EMACS_INT x, EMACS_INT y, EMACS_INT w)
{
  return list3 (make_number (x), make_number (y), make_number (w));
}

INLINE Lisp_Object
list4i (EMACS_INT x, EMACS_INT y, EMACS_INT w, EMACS_INT h)
{
  return list4 (make_number (x), make_number (y),
		make_number (w), make_number (h));
}

extern Lisp_Object make_uninit_bool_vector (EMACS_INT);
extern Lisp_Object bool_vector_fill (Lisp_Object, Lisp_Object);
extern _Noreturn void string_overflow (void);
extern Lisp_Object make_string (const char *, ptrdiff_t);
extern Lisp_Object make_formatted_string (char *, const char *, ...)
  ATTRIBUTE_FORMAT_PRINTF (2, 3);
extern Lisp_Object make_unibyte_string (const char *, ptrdiff_t);

/* Make unibyte string from C string when the length isn't known.  */

INLINE Lisp_Object
build_unibyte_string (const char *str)
{
  return make_unibyte_string (str, strlen (str));
}

extern Lisp_Object make_multibyte_string (const char *, ptrdiff_t, ptrdiff_t);
extern Lisp_Object make_event_array (ptrdiff_t, Lisp_Object *);
extern Lisp_Object make_uninit_string (EMACS_INT);
extern Lisp_Object make_uninit_multibyte_string (EMACS_INT, EMACS_INT);
extern Lisp_Object make_string_from_bytes (const char *, ptrdiff_t, ptrdiff_t);
extern Lisp_Object make_specified_string (const char *,
					  ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object make_pure_string (const char *, ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object make_pure_c_string (const char *, ptrdiff_t);

/* Make a string allocated in pure space, use STR as string data.  */

INLINE Lisp_Object
build_pure_c_string (const char *str)
{
  return make_pure_c_string (str, strlen (str));
}

/* Make a string from the data at STR, treating it as multibyte if the
   data warrants.  */

INLINE Lisp_Object
build_string (const char *str)
{
  return make_string (str, strlen (str));
}

extern Lisp_Object pure_cons (Lisp_Object, Lisp_Object);
extern void make_byte_code (struct Lisp_Vector *);
extern struct Lisp_Vector *allocate_vector (EMACS_INT);

/* Make an uninitialized vector for SIZE objects.  NOTE: you must
   be sure that GC cannot happen until the vector is completely
   initialized.  E.g. the following code is likely to crash:

   v = make_uninit_vector (3);
   ASET (v, 0, obj0);
   ASET (v, 1, Ffunction_can_gc ());
   ASET (v, 2, obj1);  */

INLINE Lisp_Object
make_uninit_vector (ptrdiff_t size)
{
  Lisp_Object v;
  struct Lisp_Vector *p;

  p = allocate_vector (size);
  XSETVECTOR (v, p);
  return v;
}

/* Like above, but special for sub char-tables.  */

INLINE Lisp_Object
make_uninit_sub_char_table (int depth, int min_char)
{
  int slots = SUB_CHAR_TABLE_OFFSET + chartab_size[depth];
  Lisp_Object v = make_uninit_vector (slots);

  XSETPVECTYPE (XVECTOR (v), PVEC_SUB_CHAR_TABLE);
  XSUB_CHAR_TABLE (v)->depth = depth;
  XSUB_CHAR_TABLE (v)->min_char = min_char;
  return v;
}

extern struct Lisp_Vector *allocate_pseudovector (int, int, int,
						  enum pvec_type);

/* Allocate partially initialized pseudovector where all Lisp_Object
   slots are set to Qnil but the rest (if any) is left uninitialized.  */

#define ALLOCATE_PSEUDOVECTOR(type, field, tag)			       \
  ((type *) allocate_pseudovector (VECSIZE (type),		       \
				   PSEUDOVECSIZE (type, field),	       \
				   PSEUDOVECSIZE (type, field), tag))

/* Allocate fully initialized pseudovector where all Lisp_Object
   slots are set to Qnil and the rest (if any) is zeroed.  */

#define ALLOCATE_ZEROED_PSEUDOVECTOR(type, field, tag)		       \
  ((type *) allocate_pseudovector (VECSIZE (type),		       \
				   PSEUDOVECSIZE (type, field),	       \
				   VECSIZE (type), tag))

extern bool gc_in_progress;
extern Lisp_Object make_float (double);
extern void display_malloc_warning (void);
extern ptrdiff_t inhibit_garbage_collection (void);
extern Lisp_Object make_save_int_int_int (ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern Lisp_Object make_save_obj_obj_obj_obj (Lisp_Object, Lisp_Object,
					      Lisp_Object, Lisp_Object);
extern Lisp_Object make_save_ptr (void *);
extern Lisp_Object make_save_ptr_int (void *, ptrdiff_t);
extern Lisp_Object make_save_ptr_ptr (void *, void *);
extern Lisp_Object make_save_funcptr_ptr_obj (void (*) (void), void *,
					      Lisp_Object);
extern Lisp_Object make_save_memory (Lisp_Object *, ptrdiff_t);
extern void free_save_value (Lisp_Object);
extern Lisp_Object build_overlay (Lisp_Object, Lisp_Object, Lisp_Object);
extern void free_marker (Lisp_Object);
extern void free_cons (struct Lisp_Cons *);
extern void init_alloc_once (void);
extern void init_alloc (void);
extern void syms_of_alloc (void);
extern struct buffer * allocate_buffer (void);
extern int valid_lisp_object_p (Lisp_Object);
#ifdef GC_CHECK_CONS_LIST
extern void check_cons_list (void);
#else
INLINE void (check_cons_list) (void) { lisp_h_check_cons_list (); }
#endif

/* Defined in gmalloc.c.  */
#if !defined DOUG_LEA_MALLOC && !defined HYBRID_MALLOC && !defined SYSTEM_MALLOC
extern size_t __malloc_extra_blocks;
#endif
#if !HAVE_DECL_ALIGNED_ALLOC
extern void *aligned_alloc (size_t, size_t) ATTRIBUTE_MALLOC_SIZE ((2));
#endif
extern void malloc_enable_thread (void);

#ifdef REL_ALLOC
/* Defined in ralloc.c.  */
extern void *r_alloc (void **, size_t) ATTRIBUTE_ALLOC_SIZE ((2));
extern void r_alloc_free (void **);
extern void *r_re_alloc (void **, size_t) ATTRIBUTE_ALLOC_SIZE ((2));
extern void r_alloc_reset_variable (void **, void **);
extern void r_alloc_inhibit_buffer_relocation (int);
#endif

/* Defined in chartab.c.  */
extern Lisp_Object copy_char_table (Lisp_Object);
extern Lisp_Object char_table_ref_and_range (Lisp_Object, int,
                                             int *, int *);
extern void char_table_set_range (Lisp_Object, int, int, Lisp_Object);
extern void map_char_table (void (*) (Lisp_Object, Lisp_Object,
                            Lisp_Object),
                            Lisp_Object, Lisp_Object, Lisp_Object);
extern void map_char_table_for_charset (void (*c_function) (Lisp_Object, Lisp_Object),
					Lisp_Object, Lisp_Object,
					Lisp_Object, struct charset *,
					unsigned, unsigned);
extern Lisp_Object uniprop_table (Lisp_Object);
extern void syms_of_chartab (void);

/* Defined in print.c.  */
extern Lisp_Object Vprin1_to_string_buffer;
extern void debug_print (Lisp_Object) EXTERNALLY_VISIBLE;
extern void temp_output_buffer_setup (const char *);
extern int print_level;
extern void print_error_message (Lisp_Object, Lisp_Object, const char *,
				 Lisp_Object);
extern Lisp_Object internal_with_output_to_temp_buffer
        (const char *, Lisp_Object (*) (Lisp_Object), Lisp_Object);
#define FLOAT_TO_STRING_BUFSIZE 350
extern int float_to_string (char *, double);
extern void init_print_once (void);
extern void syms_of_print (void);

/* Defined in doprnt.c.  */
extern ptrdiff_t doprnt (char *, ptrdiff_t, const char *, const char *,
			 va_list);
extern ptrdiff_t esprintf (char *, char const *, ...)
  ATTRIBUTE_FORMAT_PRINTF (2, 3);
extern ptrdiff_t exprintf (char **, ptrdiff_t *, char const *, ptrdiff_t,
			   char const *, ...)
  ATTRIBUTE_FORMAT_PRINTF (5, 6);
extern ptrdiff_t evxprintf (char **, ptrdiff_t *, char const *, ptrdiff_t,
			    char const *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (5, 0);

/* Defined in lread.c.  */
extern Lisp_Object check_obarray (Lisp_Object);
extern Lisp_Object intern_1 (const char *, ptrdiff_t);
extern Lisp_Object intern_c_string_1 (const char *, ptrdiff_t);
extern Lisp_Object intern_driver (Lisp_Object, Lisp_Object, Lisp_Object);
extern void init_symbol (Lisp_Object, Lisp_Object);
extern Lisp_Object oblookup (Lisp_Object, const char *, ptrdiff_t, ptrdiff_t);
INLINE void
LOADHIST_ATTACH (Lisp_Object x)
{
  if (initialized)
    Vcurrent_load_list = Fcons (x, Vcurrent_load_list);
}
extern int openp (Lisp_Object, Lisp_Object, Lisp_Object,
                  Lisp_Object *, Lisp_Object, bool);
extern Lisp_Object string_to_number (char const *, int, bool);
extern void map_obarray (Lisp_Object, void (*) (Lisp_Object, Lisp_Object),
                         Lisp_Object);
extern void dir_warning (const char *, Lisp_Object);
extern void init_obarray (void);
extern void init_lread (void);
extern void syms_of_lread (void);

INLINE Lisp_Object
intern (const char *str)
{
  return intern_1 (str, strlen (str));
}

INLINE Lisp_Object
intern_c_string (const char *str)
{
  return intern_c_string_1 (str, strlen (str));
}

/* Defined in eval.c.  */
extern Lisp_Object Vautoload_queue;
extern Lisp_Object Vrun_hooks;
extern Lisp_Object Vsignaling_function;
extern Lisp_Object inhibit_lisp_code;

/* To run a normal hook, use the appropriate function from the list below.
   The calling convention:

   if (!NILP (Vrun_hooks))
     call1 (Vrun_hooks, Qmy_funny_hook);

   should no longer be used.  */
extern void run_hook (Lisp_Object);
extern void run_hook_with_args_2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object run_hook_with_args (ptrdiff_t nargs, Lisp_Object *args,
				       Lisp_Object (*funcall)
				       (ptrdiff_t nargs, Lisp_Object *args));
extern Lisp_Object quit (void);
INLINE _Noreturn void
xsignal (Lisp_Object error_symbol, Lisp_Object data)
{
  Fsignal (error_symbol, data);
}
extern _Noreturn void xsignal0 (Lisp_Object);
extern _Noreturn void xsignal1 (Lisp_Object, Lisp_Object);
extern _Noreturn void xsignal2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern _Noreturn void xsignal3 (Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object);
extern _Noreturn void signal_error (const char *, Lisp_Object);
extern bool FUNCTIONP (Lisp_Object);
extern Lisp_Object funcall_subr (struct Lisp_Subr *subr, ptrdiff_t numargs, Lisp_Object *arg_vector);
extern Lisp_Object eval_sub (Lisp_Object form);
extern Lisp_Object apply1 (Lisp_Object, Lisp_Object);
extern Lisp_Object call0 (Lisp_Object);
extern Lisp_Object call1 (Lisp_Object, Lisp_Object);
extern Lisp_Object call2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object call3 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object call4 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object call5 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object call6 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object call7 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object call8 (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object internal_catch (Lisp_Object, Lisp_Object (*) (Lisp_Object), Lisp_Object);
extern Lisp_Object internal_lisp_condition_case (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object internal_condition_case (Lisp_Object (*) (void), Lisp_Object, Lisp_Object (*) (Lisp_Object));
extern Lisp_Object internal_condition_case_1 (Lisp_Object (*) (Lisp_Object), Lisp_Object, Lisp_Object, Lisp_Object (*) (Lisp_Object));
extern Lisp_Object internal_condition_case_2 (Lisp_Object (*) (Lisp_Object, Lisp_Object), Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object (*) (Lisp_Object));
extern Lisp_Object internal_condition_case_n
    (Lisp_Object (*) (ptrdiff_t, Lisp_Object *), ptrdiff_t, Lisp_Object *,
     Lisp_Object, Lisp_Object (*) (Lisp_Object, ptrdiff_t, Lisp_Object *));
extern struct handler *push_handler (Lisp_Object, enum handlertype);
extern struct handler *push_handler_nosignal (Lisp_Object, enum handlertype);
extern void specbind (Lisp_Object, Lisp_Object);
extern void record_unwind_protect (void (*) (Lisp_Object), Lisp_Object);
extern void record_unwind_protect_ptr (void (*) (void *), void *);
extern void record_unwind_protect_int (void (*) (int), int);
extern void record_unwind_protect_void (void (*) (void));
extern void record_unwind_protect_nothing (void);
extern void clear_unwind_protect (ptrdiff_t);
extern void set_unwind_protect (ptrdiff_t, void (*) (Lisp_Object), Lisp_Object);
extern void set_unwind_protect_ptr (ptrdiff_t, void (*) (void *), void *);
extern Lisp_Object unbind_to (ptrdiff_t, Lisp_Object);
extern void rebind_for_thread_switch (void);
extern void unbind_for_thread_switch (struct thread_state *);
extern _Noreturn void error (const char *, ...) ATTRIBUTE_FORMAT_PRINTF (1, 2);
extern _Noreturn void verror (const char *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (1, 0);
extern Lisp_Object vformat_string (const char *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (1, 0);
extern void un_autoload (Lisp_Object);
extern Lisp_Object call_debugger (Lisp_Object arg);
extern void init_eval_once (void);
extern Lisp_Object safe_call (ptrdiff_t, Lisp_Object, ...);
extern Lisp_Object safe_call1 (Lisp_Object, Lisp_Object);
extern Lisp_Object safe_call2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern void init_eval (void);
extern void syms_of_eval (void);
extern void prog_ignore (Lisp_Object);
extern ptrdiff_t record_in_backtrace (Lisp_Object, Lisp_Object *, ptrdiff_t);
extern void mark_specpdl (union specbinding *first, union specbinding *ptr);
extern void get_backtrace (Lisp_Object array);
Lisp_Object backtrace_top_function (void);
extern bool let_shadows_buffer_binding_p (struct Lisp_Symbol *symbol);

/* Defined in unexmacosx.c.  */
#if defined DARWIN_OS && !defined CANNOT_DUMP
extern void unexec_init_emacs_zone (void);
extern void *unexec_malloc (size_t);
extern void *unexec_realloc (void *, size_t);
extern void unexec_free (void *);
#endif

#include "emacs-module.h"

/* Function prototype for the module Lisp functions.  */
typedef emacs_value (*emacs_subr) (emacs_env *, ptrdiff_t,
				   emacs_value [], void *);

/* Module function.  */

/* A function environment is an auxiliary structure returned by
   `module_make_function' to store information about a module
   function.  It is stored in a pseudovector.  Its members correspond
   to the arguments given to `module_make_function'.  */

struct Lisp_Module_Function
{
  union vectorlike_header header;

  /* Fields traced by GC; these must come first.  */
  Lisp_Object documentation;

  /* Fields ignored by GC.  */
  ptrdiff_t min_arity, max_arity;
  emacs_subr subr;
  void *data;
};

INLINE bool
MODULE_FUNCTIONP (Lisp_Object o)
{
  return PSEUDOVECTORP (o, PVEC_MODULE_FUNCTION);
}

INLINE struct Lisp_Module_Function *
XMODULE_FUNCTION (Lisp_Object o)
{
  eassert (MODULE_FUNCTIONP (o));
  return XUNTAG (o, Lisp_Vectorlike);
}

#ifdef HAVE_MODULES
/* Defined in alloc.c.  */
extern Lisp_Object make_user_ptr (void (*finalizer) (void *), void *p);

/* Defined in emacs-module.c.  */
extern Lisp_Object funcall_module (Lisp_Object, ptrdiff_t, Lisp_Object *);
extern Lisp_Object module_function_arity (const struct Lisp_Module_Function *);
extern void mark_modules (void);
extern void init_module_assertions (bool);
extern void syms_of_module (void);
#endif

/* Defined in thread.c.  */
extern void mark_threads (void);

/* Defined in editfns.c.  */
extern void insert1 (Lisp_Object);
extern Lisp_Object save_excursion_save (void);
extern Lisp_Object save_restriction_save (void);
extern void save_excursion_restore (Lisp_Object);
extern void save_restriction_restore (Lisp_Object);
extern _Noreturn void time_overflow (void);
extern Lisp_Object make_buffer_string (ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object make_buffer_string_both (ptrdiff_t, ptrdiff_t, ptrdiff_t,
					    ptrdiff_t, bool);
extern void init_editfns (bool);
extern void syms_of_editfns (void);

/* Defined in buffer.c.  */
extern bool mouse_face_overlay_overlaps (Lisp_Object);
extern Lisp_Object disable_line_numbers_overlay_at_eob (void);
extern _Noreturn void nsberror (Lisp_Object);
extern void adjust_overlays_for_insert (ptrdiff_t, ptrdiff_t);
extern void adjust_overlays_for_delete (ptrdiff_t, ptrdiff_t);
extern void fix_start_end_in_overlays (ptrdiff_t, ptrdiff_t);
extern void report_overlay_modification (Lisp_Object, Lisp_Object, bool,
                                         Lisp_Object, Lisp_Object, Lisp_Object);
extern bool overlay_touches_p (ptrdiff_t);
extern Lisp_Object other_buffer_safely (Lisp_Object);
extern Lisp_Object get_truename_buffer (Lisp_Object);
extern void init_buffer_once (void);
extern void init_buffer (int);
extern void syms_of_buffer (void);
extern void keys_of_buffer (void);

/* Defined in marker.c.  */

extern ptrdiff_t marker_position (Lisp_Object);
extern ptrdiff_t marker_byte_position (Lisp_Object);
extern void clear_charpos_cache (struct buffer *);
extern ptrdiff_t buf_charpos_to_bytepos (struct buffer *, ptrdiff_t);
extern ptrdiff_t buf_bytepos_to_charpos (struct buffer *, ptrdiff_t);
extern void unchain_marker (struct Lisp_Marker *marker);
extern Lisp_Object set_marker_restricted (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object set_marker_both (Lisp_Object, Lisp_Object, ptrdiff_t, ptrdiff_t);
extern Lisp_Object set_marker_restricted_both (Lisp_Object, Lisp_Object,
                                               ptrdiff_t, ptrdiff_t);
extern Lisp_Object build_marker (struct buffer *, ptrdiff_t, ptrdiff_t);
extern void syms_of_marker (void);

/* Defined in fileio.c.  */

extern Lisp_Object expand_and_dir_to_file (Lisp_Object);
extern Lisp_Object write_region (Lisp_Object, Lisp_Object, Lisp_Object,
				 Lisp_Object, Lisp_Object, Lisp_Object,
				 Lisp_Object, int);
extern void close_file_unwind (int);
extern void fclose_unwind (void *);
extern void restore_point_unwind (Lisp_Object);
extern _Noreturn void report_file_errno (const char *, Lisp_Object, int);
extern _Noreturn void report_file_error (const char *, Lisp_Object);
extern _Noreturn void report_file_notify_error (const char *, Lisp_Object);
extern bool internal_delete_file (Lisp_Object);
extern Lisp_Object emacs_readlinkat (int, const char *);
extern bool file_directory_p (const char *);
extern bool file_accessible_directory_p (Lisp_Object);
extern void init_fileio (void);
extern void syms_of_fileio (void);

/* Defined in search.c.  */
extern void shrink_regexp_cache (void);
extern void restore_search_regs (void);
extern void update_search_regs (ptrdiff_t oldstart,
                                ptrdiff_t oldend, ptrdiff_t newend);
extern void record_unwind_save_match_data (void);
struct re_registers;
extern struct re_pattern_buffer *compile_pattern (Lisp_Object,
						  struct re_registers *,
						  Lisp_Object, bool, bool);
extern ptrdiff_t fast_string_match_internal (Lisp_Object, Lisp_Object,
					     Lisp_Object);

INLINE ptrdiff_t
fast_string_match (Lisp_Object regexp, Lisp_Object string)
{
  return fast_string_match_internal (regexp, string, Qnil);
}

INLINE ptrdiff_t
fast_string_match_ignore_case (Lisp_Object regexp, Lisp_Object string)
{
  return fast_string_match_internal (regexp, string, Vascii_canon_table);
}

extern ptrdiff_t fast_c_string_match_ignore_case (Lisp_Object, const char *,
						  ptrdiff_t);
extern ptrdiff_t fast_looking_at (Lisp_Object, ptrdiff_t, ptrdiff_t,
                                  ptrdiff_t, ptrdiff_t, Lisp_Object);
extern ptrdiff_t find_newline (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t,
			       ptrdiff_t, ptrdiff_t *, ptrdiff_t *, bool);
extern ptrdiff_t scan_newline (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t,
			       ptrdiff_t, bool);
extern ptrdiff_t scan_newline_from_point (ptrdiff_t, ptrdiff_t *, ptrdiff_t *);
extern ptrdiff_t find_newline_no_quit (ptrdiff_t, ptrdiff_t,
				       ptrdiff_t, ptrdiff_t *);
extern ptrdiff_t find_before_next_newline (ptrdiff_t, ptrdiff_t,
					   ptrdiff_t, ptrdiff_t *);
extern void syms_of_search (void);
extern void clear_regexp_cache (void);

/* Defined in minibuf.c.  */

extern Lisp_Object Vminibuffer_list;
extern Lisp_Object last_minibuf_string;
extern Lisp_Object get_minibuffer (EMACS_INT);
extern void init_minibuf_once (void);
extern void syms_of_minibuf (void);

/* Defined in callint.c.  */

extern void syms_of_callint (void);

/* Defined in casefiddle.c.  */

extern void syms_of_casefiddle (void);
extern void keys_of_casefiddle (void);

/* Defined in casetab.c.  */

extern void init_casetab_once (void);
extern void syms_of_casetab (void);

/* Defined in keyboard.c.  */

extern Lisp_Object echo_message_buffer;
extern struct kboard *echo_kboard;
extern void cancel_echoing (void);
extern bool input_pending;
#ifdef HAVE_STACK_OVERFLOW_HANDLING
extern sigjmp_buf return_to_command_loop;
#endif
extern Lisp_Object menu_bar_items (Lisp_Object);
extern Lisp_Object tool_bar_items (Lisp_Object, int *);
extern void discard_mouse_events (void);
#ifdef USABLE_SIGIO
void handle_input_available_signal (int);
#endif
extern Lisp_Object pending_funcalls;
extern bool detect_input_pending (void);
extern bool detect_input_pending_ignore_squeezables (void);
extern bool detect_input_pending_run_timers (bool);
extern void safe_run_hooks (Lisp_Object);
extern void cmd_error_internal (Lisp_Object, const char *);
extern Lisp_Object command_loop_1 (void);
extern Lisp_Object read_menu_command (void);
extern Lisp_Object recursive_edit_1 (void);
extern void record_auto_save (void);
extern void force_auto_save_soon (void);
extern void init_keyboard (void);
extern void syms_of_keyboard (void);
extern void keys_of_keyboard (void);

/* Defined in indent.c.  */
extern ptrdiff_t current_column (void);
extern void invalidate_current_column (void);
extern bool indented_beyond_p (ptrdiff_t, ptrdiff_t, EMACS_INT);
extern void syms_of_indent (void);

/* Defined in frame.c.  */
extern void store_frame_param (struct frame *, Lisp_Object, Lisp_Object);
extern void store_in_alist (Lisp_Object *, Lisp_Object, Lisp_Object);
extern Lisp_Object do_switch_frame (Lisp_Object, int, int, Lisp_Object);
extern Lisp_Object get_frame_param (struct frame *, Lisp_Object);
extern void frames_discard_buffer (Lisp_Object);
extern void syms_of_frame (void);

/* Defined in emacs.c.  */
extern char **initial_argv;
extern int initial_argc;
#if defined (HAVE_X_WINDOWS) || defined (HAVE_NS)
extern bool display_arg;
#endif
extern Lisp_Object decode_env_path (const char *, const char *, bool);
extern Lisp_Object empty_unibyte_string, empty_multibyte_string;
extern _Noreturn void terminate_due_to_signal (int, int);
#ifdef WINDOWSNT
extern Lisp_Object Vlibrary_cache;
#endif
#if HAVE_SETLOCALE
void fixup_locale (void);
void synchronize_system_messages_locale (void);
void synchronize_system_time_locale (void);
#else
INLINE void fixup_locale (void) {}
INLINE void synchronize_system_messages_locale (void) {}
INLINE void synchronize_system_time_locale (void) {}
#endif
extern char *emacs_strerror (int);
extern void shut_down_emacs (int, Lisp_Object);

/* True means don't do interactive redisplay and don't change tty modes.  */
extern bool noninteractive;

/* True means remove site-lisp directories from load-path.  */
extern bool no_site_lisp;

/* True means put details like time stamps into builds.  */
extern bool build_details;

#ifndef WINDOWSNT
/* 0 not a daemon, 1 foreground daemon, 2 background daemon.  */
extern int daemon_type;
#define IS_DAEMON (daemon_type != 0)
#define DAEMON_RUNNING (daemon_type >= 0)
#else  /* WINDOWSNT */
extern void *w32_daemon_event;
#define IS_DAEMON (w32_daemon_event != NULL)
#define DAEMON_RUNNING (w32_daemon_event != INVALID_HANDLE_VALUE)
#endif

/* True if handling a fatal error already.  */
extern bool fatal_error_in_progress;

/* True means don't do use window-system-specific display code.  */
extern bool inhibit_window_system;
/* True means that a filter or a sentinel is running.  */
extern bool running_asynch_code;

/* Defined in process.c.  */
struct Lisp_Process;
extern void kill_buffer_processes (Lisp_Object);
extern int wait_reading_process_output (intmax_t, int, int, bool, Lisp_Object,
					struct Lisp_Process *, int);
/* Max value for the first argument of wait_reading_process_output.  */
#if GNUC_PREREQ (3, 0, 0) && ! GNUC_PREREQ (4, 6, 0)
/* Work around a bug in GCC 3.4.2, known to be fixed in GCC 4.6.0.
   The bug merely causes a bogus warning, but the warning is annoying.  */
# define WAIT_READING_MAX min (TYPE_MAXIMUM (time_t), INTMAX_MAX)
#else
# define WAIT_READING_MAX INTMAX_MAX
#endif
#ifdef HAVE_TIMERFD
extern void add_timer_wait_descriptor (int);
#endif
extern void add_keyboard_wait_descriptor (int);
extern void delete_keyboard_wait_descriptor (int);
#ifdef HAVE_GPM
extern void add_gpm_wait_descriptor (int);
extern void delete_gpm_wait_descriptor (int);
#endif
extern void init_process_emacs (int);
extern void syms_of_process (void);
extern void setup_process_coding_systems (Lisp_Object);

/* Defined in callproc.c.  */
#ifndef DOS_NT
# define CHILD_SETUP_TYPE _Noreturn void
#else
# define CHILD_SETUP_TYPE int
#endif
extern CHILD_SETUP_TYPE child_setup (int, int, int, char **, bool, Lisp_Object);
extern void init_callproc_1 (void);
extern void init_callproc (void);
extern void set_initial_environment (void);
extern void syms_of_callproc (void);

/* Defined in doc.c.  */
enum text_quoting_style
  {
    /* Use curved single quotes ‘like this’.  */
    CURVE_QUOTING_STYLE,

    /* Use grave accent and apostrophe  `like this'.  */
    GRAVE_QUOTING_STYLE,

    /* Use apostrophes 'like this'.  */
    STRAIGHT_QUOTING_STYLE
  };
extern enum text_quoting_style text_quoting_style (void);
extern Lisp_Object read_doc_string (Lisp_Object);
extern Lisp_Object get_doc_string (Lisp_Object, bool, bool);
extern void syms_of_doc (void);
extern int read_bytecode_char (bool);

/* Defined in bytecode.c.  */
extern void syms_of_bytecode (void);
extern Lisp_Object exec_byte_code (Lisp_Object, Lisp_Object, Lisp_Object,
				   Lisp_Object, ptrdiff_t, Lisp_Object *);
extern Lisp_Object get_byte_code_arity (Lisp_Object);

/* Defined in macros.c.  */
extern void init_macros (void);
extern void syms_of_macros (void);

/* Defined in undo.c.  */
extern void truncate_undo_list (struct buffer *);
extern void record_insert (ptrdiff_t, ptrdiff_t);
extern void record_delete (ptrdiff_t, Lisp_Object, bool);
extern void record_first_change (void);
extern void record_change (ptrdiff_t, ptrdiff_t);
extern void record_property_change (ptrdiff_t, ptrdiff_t,
				    Lisp_Object, Lisp_Object,
                                    Lisp_Object);
extern void syms_of_undo (void);

/* Defined in textprop.c.  */
extern void report_interval_modification (Lisp_Object, Lisp_Object);

/* Defined in menu.c.  */
extern void syms_of_menu (void);

/* Defined in xmenu.c.  */
extern void syms_of_xmenu (void);

/* Defined in termchar.h.  */
struct tty_display_info;

/* Defined in sysdep.c.  */
#ifdef HAVE_PERSONALITY_ADDR_NO_RANDOMIZE
extern bool disable_address_randomization (void);
#else
INLINE bool disable_address_randomization (void) { return false; }
#endif
extern int emacs_exec_file (char const *, char *const *, char *const *);
extern void init_standard_fds (void);
extern char *emacs_get_current_dir_name (void);
extern void stuff_char (char c);
extern void init_foreground_group (void);
extern void sys_subshell (void);
extern void sys_suspend (void);
extern void discard_tty_input (void);
extern void init_sys_modes (struct tty_display_info *);
extern void reset_sys_modes (struct tty_display_info *);
extern void init_all_sys_modes (void);
extern void reset_all_sys_modes (void);
extern void child_setup_tty (int);
extern void setup_pty (int);
extern int set_window_size (int, int, int);
extern EMACS_INT get_random (void);
extern void seed_random (void *, ptrdiff_t);
extern void init_random (void);
extern void emacs_backtrace (int);
extern _Noreturn void emacs_abort (void) NO_INLINE;
extern int emacs_open (const char *, int, int);
extern int emacs_pipe (int[2]);
extern int emacs_close (int);
extern ptrdiff_t emacs_read (int, void *, ptrdiff_t);
extern ptrdiff_t emacs_read_quit (int, void *, ptrdiff_t);
extern ptrdiff_t emacs_write (int, void const *, ptrdiff_t);
extern ptrdiff_t emacs_write_sig (int, void const *, ptrdiff_t);
extern ptrdiff_t emacs_write_quit (int, void const *, ptrdiff_t);
extern void emacs_perror (char const *);
extern int renameat_noreplace (int, char const *, int, char const *);
extern int str_collate (Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object);

/* Defined in filelock.c.  */
extern void lock_file (Lisp_Object);
extern void unlock_file (Lisp_Object);
extern void unlock_all_files (void);
extern void unlock_buffer (struct buffer *);
extern void syms_of_filelock (void);

/* Defined in sound.c.  */
extern void syms_of_sound (void);

/* Defined in category.c.  */
extern void init_category_once (void);
extern Lisp_Object char_category_set (int);
extern void syms_of_category (void);

/* Defined in ccl.c.  */
extern void syms_of_ccl (void);

/* Defined in dired.c.  */
extern void syms_of_dired (void);
extern Lisp_Object directory_files_internal (Lisp_Object, Lisp_Object,
                                             Lisp_Object, Lisp_Object,
                                             bool, Lisp_Object);

/* Defined in term.c.  */
extern int *char_ins_del_vector;
extern void syms_of_term (void);
extern _Noreturn void fatal (const char *msgid, ...)
  ATTRIBUTE_FORMAT_PRINTF (1, 2);

/* Defined in terminal.c.  */
extern void syms_of_terminal (void);

/* Defined in font.c.  */
extern void syms_of_font (void);
extern void init_font (void);

#ifdef HAVE_WINDOW_SYSTEM
/* Defined in fontset.c.  */
extern void syms_of_fontset (void);
#endif

/* Defined in inotify.c */
#ifdef HAVE_INOTIFY
extern void syms_of_inotify (void);
#endif

/* Defined in kqueue.c */
#ifdef HAVE_KQUEUE
extern void globals_of_kqueue (void);
extern void syms_of_kqueue (void);
#endif

/* Defined in gfilenotify.c */
#ifdef HAVE_GFILENOTIFY
extern void globals_of_gfilenotify (void);
extern void syms_of_gfilenotify (void);
#endif

#ifdef HAVE_W32NOTIFY
/* Defined on w32notify.c.  */
extern void syms_of_w32notify (void);
#endif

/* Defined in xfaces.c.  */
extern Lisp_Object Vface_alternative_font_family_alist;
extern Lisp_Object Vface_alternative_font_registry_alist;
extern void syms_of_xfaces (void);

#ifdef HAVE_X_WINDOWS
/* Defined in xfns.c.  */
extern void syms_of_xfns (void);

/* Defined in xsmfns.c.  */
extern void syms_of_xsmfns (void);

/* Defined in xselect.c.  */
extern void syms_of_xselect (void);

/* Defined in xterm.c.  */
extern void init_xterm (void);
extern void syms_of_xterm (void);
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_WINDOW_SYSTEM
/* Defined in xterm.c, nsterm.m, w32term.c.  */
extern char *x_get_keysym_name (int);
#endif /* HAVE_WINDOW_SYSTEM */

/* Defined in xml.c.  */
extern void syms_of_xml (void);
#ifdef HAVE_LIBXML2
extern void xml_cleanup_parser (void);
#endif

#ifdef HAVE_LCMS2
/* Defined in lcms.c.  */
extern void syms_of_lcms2 (void);
#endif

#ifdef HAVE_ZLIB
/* Defined in decompress.c.  */
extern void syms_of_decompress (void);
#endif

#ifdef HAVE_DBUS
/* Defined in dbusbind.c.  */
void init_dbusbind (void);
void syms_of_dbusbind (void);
#endif


/* Defined in profiler.c.  */
extern bool profiler_memory_running;
extern void malloc_probe (size_t);
extern void syms_of_profiler (void);


#ifdef DOS_NT
/* Defined in msdos.c, w32.c.  */
extern char *emacs_root_dir (void);
#endif /* DOS_NT */

/* Defined in lastfile.c.  */
extern char my_edata[];
extern char my_endbss[];
extern char *my_endbss_static;

extern void *xmalloc (size_t) ATTRIBUTE_MALLOC_SIZE ((1));
extern void *xzalloc (size_t) ATTRIBUTE_MALLOC_SIZE ((1));
extern void *xrealloc (void *, size_t) ATTRIBUTE_ALLOC_SIZE ((2));
extern void xfree (void *);
extern void *xnmalloc (ptrdiff_t, ptrdiff_t) ATTRIBUTE_MALLOC_SIZE ((1,2));
extern void *xnrealloc (void *, ptrdiff_t, ptrdiff_t)
  ATTRIBUTE_ALLOC_SIZE ((2,3));
extern void *xpalloc (void *, ptrdiff_t *, ptrdiff_t, ptrdiff_t, ptrdiff_t);

extern char *xstrdup (const char *) ATTRIBUTE_MALLOC;
extern char *xlispstrdup (Lisp_Object) ATTRIBUTE_MALLOC;
extern void dupstring (char **, char const *);

/* Make DEST a copy of STRING's data.  Return a pointer to DEST's terminating
   null byte.  This is like stpcpy, except the source is a Lisp string.  */

INLINE char *
lispstpcpy (char *dest, Lisp_Object string)
{
  ptrdiff_t len = SBYTES (string);
  memcpy (dest, SDATA (string), len + 1);
  return dest + len;
}

extern void xputenv (const char *);

extern char *egetenv_internal (const char *, ptrdiff_t);

INLINE char *
egetenv (const char *var)
{
  /* When VAR is a string literal, strlen can be optimized away.  */
  return egetenv_internal (var, strlen (var));
}

/* Set up the name of the machine we're running on.  */
extern void init_system_name (void);

/* Return the absolute value of X.  X should be a signed integer
   expression without side effects, and X's absolute value should not
   exceed the maximum for its promoted type.  This is called 'eabs'
   because 'abs' is reserved by the C standard.  */
#define eabs(x)         ((x) < 0 ? -(x) : (x))

/* Return a fixnum or float, depending on whether the integer VAL fits
   in a Lisp fixnum.  */

#define make_fixnum_or_float(val) \
   (FIXNUM_OVERFLOW_P (val) ? make_float (val) : make_number (val))

/* SAFE_ALLOCA normally allocates memory on the stack, but if size is
   larger than MAX_ALLOCA, use xmalloc to avoid overflowing the stack.  */

enum MAX_ALLOCA { MAX_ALLOCA = 16 * 1024 };

extern void *record_xmalloc (size_t) ATTRIBUTE_ALLOC_SIZE ((1));

#define USE_SAFE_ALLOCA			\
  ptrdiff_t sa_avail = MAX_ALLOCA;	\
  ptrdiff_t sa_count = SPECPDL_INDEX (); bool sa_must_free = false

#define AVAIL_ALLOCA(size) (sa_avail -= (size), alloca (size))

/* SAFE_ALLOCA allocates a simple buffer.  */

#define SAFE_ALLOCA(size) ((size) <= sa_avail				\
			   ? AVAIL_ALLOCA (size)			\
			   : (sa_must_free = true, record_xmalloc (size)))

/* SAFE_NALLOCA sets BUF to a newly allocated array of MULTIPLIER *
   NITEMS items, each of the same type as *BUF.  MULTIPLIER must
   positive.  The code is tuned for MULTIPLIER being a constant.  */

#define SAFE_NALLOCA(buf, multiplier, nitems)			 \
  do {								 \
    if ((nitems) <= sa_avail / sizeof *(buf) / (multiplier))	 \
      (buf) = AVAIL_ALLOCA (sizeof *(buf) * (multiplier) * (nitems)); \
    else							 \
      {								 \
	(buf) = xnmalloc (nitems, sizeof *(buf) * (multiplier)); \
	sa_must_free = true;					 \
	record_unwind_protect_ptr (xfree, buf);			 \
      }								 \
  } while (false)

/* SAFE_ALLOCA_STRING allocates a C copy of a Lisp string.  */

#define SAFE_ALLOCA_STRING(ptr, string)			\
  do {							\
    (ptr) = SAFE_ALLOCA (SBYTES (string) + 1);		\
    memcpy (ptr, SDATA (string), SBYTES (string) + 1);	\
  } while (false)

/* SAFE_FREE frees xmalloced memory and enables GC as needed.  */

#define SAFE_FREE()			\
  do {					\
    if (sa_must_free) {			\
      sa_must_free = false;		\
      unbind_to (sa_count, Qnil);	\
    }					\
  } while (false)

/* Set BUF to point to an allocated array of NELT Lisp_Objects,
   immediately followed by EXTRA spare bytes.  */

#define SAFE_ALLOCA_LISP_EXTRA(buf, nelt, extra)	       \
  do {							       \
    ptrdiff_t alloca_nbytes;				       \
    if (INT_MULTIPLY_WRAPV (nelt, word_size, &alloca_nbytes)   \
	|| INT_ADD_WRAPV (alloca_nbytes, extra, &alloca_nbytes) \
	|| SIZE_MAX < alloca_nbytes)			       \
      memory_full (SIZE_MAX);				       \
    else if (alloca_nbytes <= sa_avail)			       \
      (buf) = AVAIL_ALLOCA (alloca_nbytes);		       \
    else						       \
      {							       \
	Lisp_Object arg_;				       \
	(buf) = xmalloc (alloca_nbytes);		       \
	arg_ = make_save_memory (buf, nelt);		       \
	sa_must_free = true;				       \
	record_unwind_protect (free_save_value, arg_);	       \
      }							       \
  } while (false)

/* Set BUF to point to an allocated array of NELT Lisp_Objects.  */

#define SAFE_ALLOCA_LISP(buf, nelt) SAFE_ALLOCA_LISP_EXTRA (buf, nelt, 0)


/* If USE_STACK_LISP_OBJECTS, define macros that and functions that allocate
   block-scoped conses and strings.  These objects are not
   managed by the garbage collector, so they are dangerous: passing them
   out of their scope (e.g., to user code) results in undefined behavior.
   Conversely, they have better performance because GC is not involved.

   This feature is experimental and requires careful debugging.
   Build with CPPFLAGS='-DUSE_STACK_LISP_OBJECTS=0' to disable it.  */

#if (!defined USE_STACK_LISP_OBJECTS \
     && defined __GNUC__ && !defined __clang__ && ! GNUC_PREREQ (4, 3, 2))
  /* Work around GCC bugs 36584 and 35271, which were fixed in GCC 4.3.2.  */
# define USE_STACK_LISP_OBJECTS false
#endif
#ifndef USE_STACK_LISP_OBJECTS
# define USE_STACK_LISP_OBJECTS true
#endif

#ifdef GC_CHECK_STRING_BYTES
enum { defined_GC_CHECK_STRING_BYTES = true };
#else
enum { defined_GC_CHECK_STRING_BYTES = false };
#endif

/* True for stack-based cons and string implementations, respectively.
   Use stack-based strings only if stack-based cons also works.
   Otherwise, STACK_CONS would create heap-based cons cells that
   could point to stack-based strings, which is a no-no.  */

enum
  {
    USE_STACK_CONS = USE_STACK_LISP_OBJECTS,
    USE_STACK_STRING = (USE_STACK_CONS
			&& !defined_GC_CHECK_STRING_BYTES)
  };

/* Auxiliary macros used for auto allocation of Lisp objects.  Please
   use these only in macros like AUTO_CONS that declare a local
   variable whose lifetime will be clear to the programmer.  */
#define STACK_CONS(a, b) \
  make_lisp_ptr (&((struct Lisp_Cons) {{{a, {b}}}}), Lisp_Cons)
#define AUTO_CONS_EXPR(a, b) \
  (USE_STACK_CONS ? STACK_CONS (a, b) : Fcons (a, b))

/* Declare NAME as an auto Lisp cons or short list if possible, a
   GC-based one otherwise.  This is in the sense of the C keyword
   'auto'; i.e., the object has the lifetime of the containing block.
   The resulting object should not be made visible to user Lisp code.  */

#define AUTO_CONS(name, a, b) Lisp_Object name = AUTO_CONS_EXPR (a, b)
#define AUTO_LIST1(name, a)						\
  Lisp_Object name = (USE_STACK_CONS ? STACK_CONS (a, Qnil) : list1 (a))
#define AUTO_LIST2(name, a, b)						\
  Lisp_Object name = (USE_STACK_CONS					\
		      ? STACK_CONS (a, STACK_CONS (b, Qnil))		\
		      : list2 (a, b))
#define AUTO_LIST3(name, a, b, c)					\
  Lisp_Object name = (USE_STACK_CONS					\
		      ? STACK_CONS (a, STACK_CONS (b, STACK_CONS (c, Qnil))) \
		      : list3 (a, b, c))
#define AUTO_LIST4(name, a, b, c, d)					\
    Lisp_Object name							\
      = (USE_STACK_CONS							\
	 ? STACK_CONS (a, STACK_CONS (b, STACK_CONS (c,			\
						     STACK_CONS (d, Qnil)))) \
	 : list4 (a, b, c, d))

/* Declare NAME as an auto Lisp string if possible, a GC-based one if not.
   Take its unibyte value from the null-terminated string STR,
   an expression that should not have side effects.
   STR's value is not necessarily copied.  The resulting Lisp string
   should not be modified or made visible to user code.  */

#define AUTO_STRING(name, str) \
  AUTO_STRING_WITH_LEN (name, str, strlen (str))

/* Declare NAME as an auto Lisp string if possible, a GC-based one if not.
   Take its unibyte value from the null-terminated string STR with length LEN.
   STR may have side effects and may contain null bytes.
   STR's value is not necessarily copied.  The resulting Lisp string
   should not be modified or made visible to user code.  */

#define AUTO_STRING_WITH_LEN(name, str, len)				\
  Lisp_Object name =							\
    (USE_STACK_STRING							\
     ? (make_lisp_ptr							\
	((&(struct Lisp_String) {{{len, -1, 0, (unsigned char *) (str)}}}), \
	 Lisp_String))							\
     : make_unibyte_string (str, len))

/* Loop over conses of the list TAIL, signaling if a cycle is found,
   and possibly quitting after each loop iteration.  In the loop body,
   set TAIL to the current cons.  If the loop exits normally,
   set TAIL to the terminating non-cons, typically nil.  The loop body
   should not modify the list’s top level structure other than by
   perhaps deleting the current cons.  */

#define FOR_EACH_TAIL(tail) \
  FOR_EACH_TAIL_INTERNAL (tail, circular_list (tail), true)

/* Like FOR_EACH_TAIL (LIST), except do not signal or quit.
   If the loop exits due to a cycle, TAIL’s value is undefined.  */

#define FOR_EACH_TAIL_SAFE(tail) \
  FOR_EACH_TAIL_INTERNAL (tail, (void) ((tail) = Qnil), false)

/* Iterator intended for use only within FOR_EACH_TAIL_INTERNAL.  */
struct for_each_tail_internal
{
  Lisp_Object tortoise;
  intptr_t max, n;
  unsigned short int q;
};

/* Like FOR_EACH_TAIL (LIST), except evaluate CYCLE if a cycle is
   found, and check for quit if CHECK_QUIT.  This is an internal macro
   intended for use only by the above macros.

   Use Brent’s teleporting tortoise-hare algorithm.  See:
   Brent RP. BIT. 1980;20(2):176-84. doi:10.1007/BF01933190
   http://maths-people.anu.edu.au/~brent/pd/rpb051i.pdf

   This macro uses maybe_quit because of an excess of caution.  The
   call to maybe_quit should not be needed in practice, as a very long
   list, whether circular or not, will cause Emacs to be so slow in
   other uninterruptible areas (e.g., garbage collection) that there
   is little point to calling maybe_quit here.  */

#define FOR_EACH_TAIL_INTERNAL(tail, cycle, check_quit)			\
  for (struct for_each_tail_internal li = { tail, 2, 0, 2 };		\
       CONSP (tail);							\
       ((tail) = XCDR (tail),						\
	((--li.q != 0							\
	  || ((check_quit) ? maybe_quit () : (void) 0, 0 < --li.n)	\
	  || (li.q = li.n = li.max <<= 1, li.n >>= USHRT_WIDTH,		\
	      li.tortoise = (tail), false))				\
	 && EQ (tail, li.tortoise))					\
	? (cycle) : (void) 0))

/* Do a `for' loop over alist values.  */

#define FOR_EACH_ALIST_VALUE(head_var, list_var, value_var)		\
  for ((list_var) = (head_var);						\
       (CONSP (list_var) && ((value_var) = XCDR (XCAR (list_var)), true)); \
       (list_var) = XCDR (list_var))

/* Check whether it's time for GC, and run it if so.  */

INLINE void
maybe_gc (void)
{
  if ((consing_since_gc > gc_cons_threshold
       && consing_since_gc > gc_relative_threshold)
      || (!NILP (Vmemory_full)
	  && consing_since_gc > memory_full_cons_threshold))
    Fgarbage_collect ();
}

INLINE_HEADER_END

#endif /* EMACS_LISP_H */
