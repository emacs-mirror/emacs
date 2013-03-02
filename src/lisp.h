/* Fundamental definitions for GNU Emacs Lisp interpreter.

Copyright (C) 1985-1987, 1993-1995, 1997-2013 Free Software Foundation,
Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef EMACS_LISP_H
#define EMACS_LISP_H

#include <setjmp.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>

#include <intprops.h>

INLINE_HEADER_BEGIN
#ifndef LISP_INLINE
# define LISP_INLINE INLINE
#endif

/* The ubiquitous max and min macros.  */
#undef min
#undef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

/* EMACS_INT - signed integer wide enough to hold an Emacs value
   EMACS_INT_MAX - maximum value of EMACS_INT; can be used in #if
   pI - printf length modifier for EMACS_INT
   EMACS_UINT - unsigned variant of EMACS_INT */
#ifndef EMACS_INT_MAX
# if LONG_MAX < LLONG_MAX && defined WIDE_EMACS_INT
typedef long long int EMACS_INT;
typedef unsigned long long int EMACS_UINT;
#  define EMACS_INT_MAX LLONG_MAX
#  define pI "ll"
# elif INT_MAX < LONG_MAX
typedef long int EMACS_INT;
typedef unsigned long int EMACS_UINT;
#  define EMACS_INT_MAX LONG_MAX
#  define pI "l"
# else
typedef int EMACS_INT;
typedef unsigned int EMACS_UINT;
#  define EMACS_INT_MAX INT_MAX
#  define pI ""
# endif
#endif

/* Number of bits in some machine integer types.  */
enum
  {
    BITS_PER_CHAR      = CHAR_BIT,
    BITS_PER_SHORT     = CHAR_BIT * sizeof (short),
    BITS_PER_INT       = CHAR_BIT * sizeof (int),
    BITS_PER_LONG      = CHAR_BIT * sizeof (long int),
    BITS_PER_EMACS_INT = CHAR_BIT * sizeof (EMACS_INT)
  };

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

/* Define an Emacs version of 'assert (COND)', since some
   system-defined 'assert's are flaky.  COND should be free of side
   effects; it may or may not be evaluated.  */
#ifndef ENABLE_CHECKING
# define eassert(X) ((void) (0 && (X))) /* Check that X compiles.  */
#else /* ENABLE_CHECKING */

extern _Noreturn void die (const char *, const char *, int);

/* The suppress_checking variable is initialized to 0 in alloc.c.  Set
   it to 1 using a debugger to temporarily disable aborting on
   detected internal inconsistencies or error conditions.

   In some cases, a good compiler may be able to optimize away the
   eassert macro altogether, e.g., if XSTRING (x) uses eassert to test
   STRINGP (x), but a particular use of XSTRING is invoked only after
   testing that STRINGP (x) is true, making the test redundant.  */
extern bool suppress_checking EXTERNALLY_VISIBLE;

# define eassert(cond)						\
   ((cond) || suppress_checking					\
    ? (void) 0							\
    : die ("assertion failed: " # cond, __FILE__, __LINE__))
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
   on the few static Lisp_Objects used: all the defsubr as well
   as the two special buffers buffer_defaults and buffer_local_symbols.  */

enum Lisp_Bits
  {
    /* Number of bits in a Lisp_Object tag.  This can be used in #if,
       and for GDB's sake also as a regular symbol.  */
    GCTYPEBITS =
#define GCTYPEBITS 3
	GCTYPEBITS,

    /* 2**GCTYPEBITS.  This must also be a macro that expands to a
       literal integer constant, for MSVC.  */
    GCALIGNMENT =
#define GCALIGNMENT 8
	GCALIGNMENT,

    /* Number of bits in a Lisp_Object value, not counting the tag.  */
    VALBITS = BITS_PER_EMACS_INT - GCTYPEBITS,

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
   This can be used in #if, e.g., '#if VAL_MAX < UINTPTR_MAX' below.  */
#define VAL_MAX (EMACS_INT_MAX >> (GCTYPEBITS - 1))

/* Unless otherwise specified, use USE_LSB_TAG on systems where:  */
#ifndef USE_LSB_TAG
/* 1.  We know malloc returns a multiple of 8.  */
# if (defined GNU_MALLOC || defined DOUG_LEA_MALLOC || defined __GLIBC__ \
      || defined DARWIN_OS || defined __sun)
/* 2.  We can specify multiple-of-8 alignment on static variables.  */
#  ifdef alignas
/* 3.  Pointers-as-ints exceed VAL_MAX.
   On hosts where pointers-as-ints do not exceed VAL_MAX, USE_LSB_TAG is:
    a. unnecessary, because the top bits of an EMACS_INT are unused, and
    b. slower, because it typically requires extra masking.
   So, default USE_LSB_TAG to 1 only on hosts where it might be useful.  */
#   if VAL_MAX < UINTPTR_MAX
#    define USE_LSB_TAG 1
#   endif
#  endif
# endif
#endif
#ifdef USE_LSB_TAG
# undef USE_LSB_TAG
enum enum_USE_LSB_TAG { USE_LSB_TAG = 1 };
# define USE_LSB_TAG 1
#else
enum enum_USE_LSB_TAG { USE_LSB_TAG = 0 };
# define USE_LSB_TAG 0
#endif

#ifndef alignas
# define alignas(alignment) /* empty */
# if USE_LSB_TAG
#  error "USE_LSB_TAG requires alignas"
# endif
#endif


/* Define the fundamental Lisp data structures.  */

/* This is the set of Lisp data types.  If you want to define a new
   data type, read the comments after Lisp_Fwd_Type definition
   below.  */

/* Lisp integers use 2 tags, to give them one extra bit, thus
   extending their range from, e.g., -2^28..2^28-1 to -2^29..2^29-1.  */
static EMACS_INT const INTMASK = EMACS_INT_MAX >> (INTTYPEBITS - 1);
#define case_Lisp_Int case Lisp_Int0: case Lisp_Int1
#define LISP_INT_TAG_P(x) (((x) & ~Lisp_Int1) == 0)

/* Stolen from GDB.  The only known compiler that doesn't support
   enums in bitfields is MSVC.  */
#ifdef _MSC_VER
#define ENUM_BF(TYPE) unsigned int
#else
#define ENUM_BF(TYPE) enum TYPE
#endif


enum Lisp_Type
  {
    /* Integer.  XINT (obj) is the integer value.  */
    Lisp_Int0 = 0,
    Lisp_Int1 = USE_LSB_TAG ? 1 << INTTYPEBITS : 1,

    /* Symbol.  XSYMBOL (object) points to a struct Lisp_Symbol.  */
    Lisp_Symbol = 2,

    /* Miscellaneous.  XMISC (object) points to a union Lisp_Misc,
       whose first member indicates the subtype.  */
    Lisp_Misc = 3,

    /* String.  XSTRING (object) points to a struct Lisp_String.
       The length of the string, and its contents, are stored therein.  */
    Lisp_String = USE_LSB_TAG ? 1 : 1 << INTTYPEBITS,

    /* Vector of Lisp objects, or something resembling it.
       XVECTOR (object) points to a struct Lisp_Vector, which contains
       the size and contents.  The size field also contains the type
       information, if it's not a real vector object.  */
    Lisp_Vectorlike = 5,

    /* Cons.  XCONS (object) points to a struct Lisp_Cons.  */
    Lisp_Cons = 6,

    Lisp_Float = 7,
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
    /* Currently floats are not a misc type,
       but let's define this in case we want to change that.  */
    Lisp_Misc_Float,
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
    Lisp_Fwd_Kboard_Obj,	/* Fwd to a Lisp_Object field of kboards.  */
  };

/* If you want to define a new Lisp data type, here are some
   instructions.  See the thread at
   http://lists.gnu.org/archive/html/emacs-devel/2012-10/msg00561.html
   for more info.

   First, there are already a couple of Lisp types that can be used if
   your new type does not need to be exposed to Lisp programs nor
   displayed to users.  These are Lisp_Save_Value, a Lisp_Misc
   subtype; and PVEC_OTHER, a kind of vectorlike object.  The former
   is suitable for temporarily stashing away pointers and integers in
   a Lisp object (see the existing uses of make_save_value and
   XSAVE_VALUE).  The latter is useful for vector-like Lisp objects
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

   To add a new pseudovector type, extend the pvec_type enumeration;
   to add a new Lisp_Misc, extend the Lisp_Misc_Type enumeration.

   For a Lisp_Misc, you will also need to add your entry to union
   Lisp_Misc (but make sure the first word has the same structure as
   the others, starting with a 16-bit member of the Lisp_Misc_Type
   enumeration and a 1-bit GC markbit) and make sure the overall size
   of the union is not increased by your addition.

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

typedef struct { EMACS_INT i; } Lisp_Object;

#define XLI(o) (o).i
LISP_INLINE Lisp_Object
XIL (EMACS_INT i)
{
  Lisp_Object o = { i };
  return o;
}

LISP_INLINE Lisp_Object
LISP_MAKE_RVALUE (Lisp_Object o)
{
    return o;
}

#define LISP_INITIALLY_ZERO {0}

#undef CHECK_LISP_OBJECT_TYPE
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = 1 };
#else /* CHECK_LISP_OBJECT_TYPE */

/* If a struct type is not wanted, define Lisp_Object as just a number.  */

typedef EMACS_INT Lisp_Object;
#define XLI(o) (o)
#define XIL(i) (i)
#define LISP_MAKE_RVALUE(o) (0 + (o))
#define LISP_INITIALLY_ZERO 0
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = 0 };
#endif /* CHECK_LISP_OBJECT_TYPE */

/* In the size word of a vector, this bit means the vector has been marked.  */

static ptrdiff_t const ARRAY_MARK_FLAG
#define ARRAY_MARK_FLAG PTRDIFF_MIN
      = ARRAY_MARK_FLAG;

/* In the size word of a struct Lisp_Vector, this bit means it's really
   some other vector-like object.  */
static ptrdiff_t const PSEUDOVECTOR_FLAG
#define PSEUDOVECTOR_FLAG (PTRDIFF_MAX - PTRDIFF_MAX / 2)
      = PSEUDOVECTOR_FLAG;

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
  PVEC_OTHER,
  /* These last 4 are special because we OR them in fns.c:internal_equal,
     so they have to use a disjoint bit pattern:
     if (!(size & (PVEC_COMPILED | PVEC_CHAR_TABLE
                   | PVEC_SUB_CHAR_TABLE | PVEC_FONT))) */
  PVEC_COMPILED			= 0x10,
  PVEC_CHAR_TABLE		= 0x20,
  PVEC_SUB_CHAR_TABLE		= 0x30,
  PVEC_FONT			= 0x40
};

/* DATA_SEG_BITS forces extra bits to be or'd in with any pointers
   which were stored in a Lisp_Object.  */
#ifndef DATA_SEG_BITS
# define DATA_SEG_BITS 0
#endif
enum { gdb_DATA_SEG_BITS = DATA_SEG_BITS };
#undef DATA_SEG_BITS

enum More_Lisp_Bits
  {
    DATA_SEG_BITS = gdb_DATA_SEG_BITS,

    /* For convenience, we also store the number of elements in these bits.
       Note that this size is not necessarily the memory-footprint size, but
       only the number of Lisp_Object fields (that need to be traced by GC).
       The distinction is used, e.g., by Lisp_Process, which places extra
       non-Lisp_Object fields at the end of the structure.  */
    PSEUDOVECTOR_SIZE_BITS = 16,
    PSEUDOVECTOR_SIZE_MASK = (1 << PSEUDOVECTOR_SIZE_BITS) - 1,
    PVEC_TYPE_MASK = 0x0fff << PSEUDOVECTOR_SIZE_BITS,

    /* Number of bits to put in each character in the internal representation
       of bool vectors.  This should not vary across implementations.  */
    BOOL_VECTOR_BITS_PER_CHAR = 8
  };

/* These macros extract various sorts of values from a Lisp_Object.
 For example, if tem is a Lisp_Object whose type is Lisp_Cons,
 XCONS (tem) is the struct Lisp_Cons * pointing to the memory for that cons.  */

/* Return a perfect hash of the Lisp_Object representation.  */
#define XHASH(a) XLI (a)

#if USE_LSB_TAG

enum lsb_bits
  {
    TYPEMASK = (1 << GCTYPEBITS) - 1,
    VALMASK = ~ TYPEMASK
  };
#define XTYPE(a) ((enum Lisp_Type) (XLI (a) & TYPEMASK))
#define XINT(a) (XLI (a) >> INTTYPEBITS)
#define XUINT(a) ((EMACS_UINT) XLI (a) >> INTTYPEBITS)
#define make_number(N) XIL ((EMACS_INT) (N) << INTTYPEBITS)
#define make_lisp_ptr(ptr, type) \
  (eassert (XTYPE (XIL ((intptr_t) (ptr))) == 0), /* Check alignment.  */  \
   XIL ((type) | (intptr_t) (ptr)))

#define XPNTR(a) ((intptr_t) (XLI (a) & ~TYPEMASK))
#define XUNTAG(a, type) ((intptr_t) (XLI (a) - (type)))

#else  /* not USE_LSB_TAG */

static EMACS_INT const VALMASK
#define VALMASK VAL_MAX
      = VALMASK;

#define XTYPE(a) ((enum Lisp_Type) ((EMACS_UINT) XLI (a) >> VALBITS))

/* For integers known to be positive, XFASTINT provides fast retrieval
   and XSETFASTINT provides fast storage.  This takes advantage of the
   fact that Lisp integers have zero-bits in their tags.  */
#define XFASTINT(a) (XLI (a) + 0)
#define XSETFASTINT(a, b) ((a) = XIL (b))

/* Extract the value of a Lisp_Object as a (un)signed integer.  */

#define XINT(a) (XLI (a) << INTTYPEBITS >> INTTYPEBITS)
#define XUINT(a) ((EMACS_UINT) (XLI (a) & INTMASK))
#define make_number(N) XIL ((EMACS_INT) (N) & INTMASK)

#define make_lisp_ptr(ptr, type) \
  (XIL ((EMACS_INT) ((EMACS_UINT) (type) << VALBITS)  \
	+ ((intptr_t) (ptr) & VALMASK)))

/* DATA_SEG_BITS forces extra bits to be or'd in with any pointers
   which were stored in a Lisp_Object.  */
#define XPNTR(a) ((uintptr_t) ((XLI (a) & VALMASK) | DATA_SEG_BITS))

#endif /* not USE_LSB_TAG */

/* For integers known to be positive, XFASTINT sometimes provides
   faster retrieval and XSETFASTINT provides faster storage.
   If not, fallback on the non-accelerated path.  */
#ifndef XFASTINT
# define XFASTINT(a) (XINT (a))
# define XSETFASTINT(a, b) (XSETINT (a, b))
#endif

/* Extract the pointer value of the Lisp object A, under the
   assumption that A's type is TYPE.  This is a fallback
   implementation if nothing faster is available.  */
#ifndef XUNTAG
# define XUNTAG(a, type) XPNTR (a)
#endif

#define EQ(x, y) (XHASH (x) == XHASH (y))

/* Largest and smallest representable fixnum values.  These are the C
   values.  They are macros for use in static initializers, and
   constants for visibility to GDB.  */
static EMACS_INT const MOST_POSITIVE_FIXNUM =
#define MOST_POSITIVE_FIXNUM (EMACS_INT_MAX >> INTTYPEBITS)
	MOST_POSITIVE_FIXNUM;
static EMACS_INT const MOST_NEGATIVE_FIXNUM =
#define MOST_NEGATIVE_FIXNUM (-1 - MOST_POSITIVE_FIXNUM)
	MOST_NEGATIVE_FIXNUM;

/* Value is non-zero if I doesn't fit into a Lisp fixnum.  It is
   written this way so that it also works if I is of unsigned
   type or if I is a NaN.  */

#define FIXNUM_OVERFLOW_P(i) \
  (! ((0 <= (i) || MOST_NEGATIVE_FIXNUM <= (i)) && (i) <= MOST_POSITIVE_FIXNUM))

LISP_INLINE ptrdiff_t
clip_to_bounds (ptrdiff_t lower, EMACS_INT num, ptrdiff_t upper)
{
  return num < lower ? lower : num <= upper ? num : upper;
}

/* Extract a value or address from a Lisp_Object.  */

#define XCONS(a)   (eassert (CONSP (a)), \
		    (struct Lisp_Cons *) XUNTAG (a, Lisp_Cons))
#define XVECTOR(a) (eassert (VECTORLIKEP (a)), \
		    (struct Lisp_Vector *) XUNTAG (a, Lisp_Vectorlike))
#define XSTRING(a) (eassert (STRINGP (a)), \
		    (struct Lisp_String *) XUNTAG (a, Lisp_String))
#define XSYMBOL(a) (eassert (SYMBOLP (a)), \
		    (struct Lisp_Symbol *) XUNTAG (a, Lisp_Symbol))
#define XFLOAT(a)  (eassert (FLOATP (a)), \
		    (struct Lisp_Float *) XUNTAG (a, Lisp_Float))

/* Misc types.  */

#define XMISC(a)	((union Lisp_Misc *) XUNTAG (a, Lisp_Misc))
#define XMISCANY(a)	(eassert (MISCP (a)), &(XMISC (a)->u_any))
#define XMISCTYPE(a)   (XMISCANY (a)->type)
#define XMARKER(a)	(eassert (MARKERP (a)), &(XMISC (a)->u_marker))
#define XOVERLAY(a)	(eassert (OVERLAYP (a)), &(XMISC (a)->u_overlay))
#define XSAVE_VALUE(a)	(eassert (SAVE_VALUEP (a)), &(XMISC (a)->u_save_value))

/* Forwarding object types.  */

#define XFWDTYPE(a)     (a->u_intfwd.type)
#define XINTFWD(a)	(eassert (INTFWDP (a)), &((a)->u_intfwd))
#define XBOOLFWD(a)	(eassert (BOOLFWDP (a)), &((a)->u_boolfwd))
#define XOBJFWD(a)	(eassert (OBJFWDP (a)), &((a)->u_objfwd))
#define XBUFFER_OBJFWD(a) \
  (eassert (BUFFER_OBJFWDP (a)), &((a)->u_buffer_objfwd))
#define XKBOARD_OBJFWD(a) \
  (eassert (KBOARD_OBJFWDP (a)), &((a)->u_kboard_objfwd))

/* Pseudovector types.  */

#define XPROCESS(a) (eassert (PROCESSP (a)), \
		     (struct Lisp_Process *) XUNTAG (a, Lisp_Vectorlike))
#define XWINDOW(a) (eassert (WINDOWP (a)), \
		    (struct window *) XUNTAG (a, Lisp_Vectorlike))
#define XTERMINAL(a) (eassert (TERMINALP (a)), \
		      (struct terminal *) XUNTAG (a, Lisp_Vectorlike))
#define XSUBR(a) (eassert (SUBRP (a)), \
		  (struct Lisp_Subr *) XUNTAG (a, Lisp_Vectorlike))
#define XBUFFER(a) (eassert (BUFFERP (a)), \
		    (struct buffer *) XUNTAG (a, Lisp_Vectorlike))
#define XCHAR_TABLE(a) (eassert (CHAR_TABLE_P (a)), \
			(struct Lisp_Char_Table *) XUNTAG (a, Lisp_Vectorlike))
#define XSUB_CHAR_TABLE(a) (eassert (SUB_CHAR_TABLE_P (a)), \
			    ((struct Lisp_Sub_Char_Table *) \
			     XUNTAG (a, Lisp_Vectorlike)))
#define XBOOL_VECTOR(a) (eassert (BOOL_VECTOR_P (a)), \
			 ((struct Lisp_Bool_Vector *) \
			  XUNTAG (a, Lisp_Vectorlike)))

/* Construct a Lisp_Object from a value or address.  */

#define XSETINT(a, b) ((a) = make_number (b))
#define XSETCONS(a, b) ((a) = make_lisp_ptr (b, Lisp_Cons))
#define XSETVECTOR(a, b) ((a) = make_lisp_ptr (b, Lisp_Vectorlike))
#define XSETSTRING(a, b) ((a) = make_lisp_ptr (b, Lisp_String))
#define XSETSYMBOL(a, b) ((a) = make_lisp_ptr (b, Lisp_Symbol))
#define XSETFLOAT(a, b) ((a) = make_lisp_ptr (b, Lisp_Float))

/* Misc types.  */

#define XSETMISC(a, b) ((a) = make_lisp_ptr (b, Lisp_Misc))
#define XSETMARKER(a, b) (XSETMISC (a, b), XMISCTYPE (a) = Lisp_Misc_Marker)

/* Pseudovector types.  */

#define XSETPVECTYPE(v, code) XSETTYPED_PVECTYPE (v, header.size, code)
#define XSETTYPED_PVECTYPE(v, size_member, code) \
  ((v)->size_member |= PSEUDOVECTOR_FLAG | ((code) << PSEUDOVECTOR_SIZE_BITS))
#define XSETPVECTYPESIZE(v, code, sizeval) \
  ((v)->header.size = (PSEUDOVECTOR_FLAG			\
		       | ((code) << PSEUDOVECTOR_SIZE_BITS)	\
		       | (sizeval)))

/* The cast to struct vectorlike_header * avoids aliasing issues.  */
#define XSETPSEUDOVECTOR(a, b, code) \
  XSETTYPED_PSEUDOVECTOR (a, b,					\
			  (((struct vectorlike_header *)	\
			    XUNTAG (a, Lisp_Vectorlike))	\
			   ->size),				\
			  code)
#define XSETTYPED_PSEUDOVECTOR(a, b, size, code)			\
  (XSETVECTOR (a, b),							\
   eassert ((size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK))		\
	    == (PSEUDOVECTOR_FLAG | (code << PSEUDOVECTOR_SIZE_BITS))))

#define XSETWINDOW_CONFIGURATION(a, b) \
  (XSETPSEUDOVECTOR (a, b, PVEC_WINDOW_CONFIGURATION))
#define XSETPROCESS(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_PROCESS))
#define XSETWINDOW(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_WINDOW))
#define XSETTERMINAL(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_TERMINAL))
/* XSETSUBR is special since Lisp_Subr lacks struct vectorlike_header.  */
#define XSETSUBR(a, b) \
  XSETTYPED_PSEUDOVECTOR (a, b, XSUBR (a)->size, PVEC_SUBR)
#define XSETCOMPILED(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_COMPILED))
#define XSETBUFFER(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_BUFFER))
#define XSETCHAR_TABLE(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_CHAR_TABLE))
#define XSETBOOL_VECTOR(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_BOOL_VECTOR))
#define XSETSUB_CHAR_TABLE(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_SUB_CHAR_TABLE))

/* Convenience macros for dealing with Lisp arrays.  */

#define AREF(ARRAY, IDX)	XVECTOR ((ARRAY))->contents[IDX]
#define ASIZE(ARRAY)		XVECTOR ((ARRAY))->header.size
#define ASET(ARRAY, IDX, VAL)	\
  (eassert (0 <= (IDX) && (IDX) < ASIZE (ARRAY)),	\
   XVECTOR (ARRAY)->contents[IDX] = (VAL))

/* Convenience macros for dealing with Lisp strings.  */

#define SDATA(string)		(XSTRING (string)->data + 0)
#define SREF(string, index)	(SDATA (string)[index] + 0)
#define SSET(string, index, new) (SDATA (string)[index] = (new))
#define SCHARS(string)		(XSTRING (string)->size + 0)
#define SBYTES(string)		(STRING_BYTES (XSTRING (string)) + 0)

/* Avoid "differ in sign" warnings.  */
#define SSDATA(x)  ((char *) SDATA (x))

#define STRING_SET_CHARS(string, newsize) \
    (XSTRING (string)->size = (newsize))

#define STRING_COPYIN(string, index, new, count) \
    memcpy (SDATA (string) + index, new, count)

/* Type checking.  */

#define CHECK_TYPE(ok, Qxxxp, x) \
  do { if (!(ok)) wrong_type_argument (Qxxxp, (x)); } while (0)

/* Deprecated and will be removed soon.  */

#define INTERNAL_FIELD(field) field ## _

/* See the macros in intervals.h.  */

typedef struct interval *INTERVAL;

/* Complain if object is not string or buffer type.  */
#define CHECK_STRING_OR_BUFFER(x) \
  CHECK_TYPE (STRINGP (x) || BUFFERP (x), Qbuffer_or_string_p, x)

struct Lisp_Cons
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
  };

/* Take the car or cdr of something known to be a cons cell.  */
/* The _AS_LVALUE macros shouldn't be used outside of the minimal set
   of code that has to know what a cons cell looks like.  Other code not
   part of the basic lisp implementation should assume that the car and cdr
   fields are not accessible as lvalues.  (What if we want to switch to
   a copying collector someday?  Cached cons cell field addresses may be
   invalidated at arbitrary points.)  */
#define XCAR_AS_LVALUE(c) (XCONS (c)->car)
#define XCDR_AS_LVALUE(c) (XCONS (c)->u.cdr)

/* Use these from normal code.  */
#define XCAR(c)	LISP_MAKE_RVALUE (XCAR_AS_LVALUE (c))
#define XCDR(c) LISP_MAKE_RVALUE (XCDR_AS_LVALUE (c))

/* Use these to set the fields of a cons cell.

   Note that both arguments may refer to the same object, so 'n'
   should not be read after 'c' is first modified.  Also, neither
   argument should be evaluated more than once; side effects are
   especially common in the second argument.  */
#define XSETCAR(c,n) (XCAR_AS_LVALUE (c) = (n))
#define XSETCDR(c,n) (XCDR_AS_LVALUE (c) = (n))

/* Take the car or cdr of something whose type is not known.  */
#define CAR(c)					\
 (CONSP ((c)) ? XCAR ((c))			\
  : NILP ((c)) ? Qnil				\
  : wrong_type_argument (Qlistp, (c)))

#define CDR(c)					\
 (CONSP ((c)) ? XCDR ((c))			\
  : NILP ((c)) ? Qnil				\
  : wrong_type_argument (Qlistp, (c)))

/* Take the car or cdr of something whose type is not known.  */
#define CAR_SAFE(c)				\
  (CONSP ((c)) ? XCAR ((c)) : Qnil)

#define CDR_SAFE(c)				\
  (CONSP ((c)) ? XCDR ((c)) : Qnil)

/* True if STR is a multibyte string.  */
#define STRING_MULTIBYTE(STR)  \
  (XSTRING (STR)->size_byte >= 0)

/* Return the length in bytes of STR.  */

#ifdef GC_CHECK_STRING_BYTES

struct Lisp_String;
extern ptrdiff_t string_bytes (struct Lisp_String *);
#define STRING_BYTES(S) string_bytes ((S))

#else /* not GC_CHECK_STRING_BYTES */

#define STRING_BYTES(STR)  \
  ((STR)->size_byte < 0 ? (STR)->size : (STR)->size_byte)

#endif /* not GC_CHECK_STRING_BYTES */

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

   This is a macro for use in static initializers, and a constant for
   visibility to GDB.  The cast to ptrdiff_t ensures that
   the macro is signed.  */
static ptrdiff_t const STRING_BYTES_BOUND =
#define STRING_BYTES_BOUND  \
  ((ptrdiff_t) min (MOST_POSITIVE_FIXNUM, min (SIZE_MAX, PTRDIFF_MAX) - 1))
	STRING_BYTES_BOUND;

/* Mark STR as a unibyte string.  */
#define STRING_SET_UNIBYTE(STR)  \
  do { if (EQ (STR, empty_multibyte_string))  \
      (STR) = empty_unibyte_string;  \
    else XSTRING (STR)->size_byte = -1; } while (0)

/* Mark STR as a multibyte string.  Assure that STR contains only
   ASCII characters in advance.  */
#define STRING_SET_MULTIBYTE(STR)  \
  do { if (EQ (STR, empty_unibyte_string))  \
      (STR) = empty_multibyte_string;  \
    else XSTRING (STR)->size_byte = XSTRING (STR)->size; } while (0)

/* In a string or vector, the sign bit of the `size' is the gc mark bit.  */

struct Lisp_String
  {
    ptrdiff_t size;
    ptrdiff_t size_byte;
    INTERVAL intervals;		/* Text properties in this string.  */
    unsigned char *data;
  };

/* Header of vector-like objects.  This documents the layout constraints on
   vectors and pseudovectors other than struct Lisp_Subr.  It also prevents
   compilers from being fooled by Emacs's type punning: the XSETPSEUDOVECTOR
   and PSEUDOVECTORP macros cast their pointers to struct vectorlike_header *,
   because when two such pointers potentially alias, a compiler won't
   incorrectly reorder loads and stores to their size fields.  See
   <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=8546>.  */
struct vectorlike_header
  {
    /* This field contains various pieces of information:
       - The MSB (ARRAY_MARK_FLAG) holds the gcmarkbit.
       - The next bit (PSEUDOVECTOR_FLAG) indicates whether this is a plain
         vector (0) or a pseudovector (1).
       - If PSEUDOVECTOR_FLAG is 0, the rest holds the size (number
         of slots) of the vector.
       - If PSEUDOVECTOR_FLAG is 1, the rest is subdivided into
         a "pvec type" tag held in PVEC_TYPE_MASK and a size held in the lowest
         PSEUDOVECTOR_SIZE_BITS.  That size normally indicates the number of
         Lisp_Object slots at the beginning of the object that need to be
         traced by the GC, tho some types use it slightly differently.
       - E.g. if the pvec type is PVEC_FREE it means this is an unallocated
         vector on a free-list and PSEUDOVECTOR_SIZE_BITS indicates its size
         in bytes.  */
    ptrdiff_t size;

    /* When the vector is allocated from a vector block, NBYTES is used
       if the vector is not on a free list, and VECTOR is used otherwise.
       For large vector-like objects, BUFFER or VECTOR is used as a pointer
       to the next vector-like object.  It is generally a buffer or a
       Lisp_Vector alias, so for convenience it is a union instead of a
       pointer: this way, one can write P->next.vector instead of ((struct
       Lisp_Vector *) P->next).  */
    union {
      /* This is only needed for small vectors that are not free because the
	 `size' field only gives us the number of Lisp_Object slots, whereas we
	 need to know the total size, including non-Lisp_Object data.
	 FIXME: figure out a way to store this info elsewhere so we can
	 finally get rid of this extra word of overhead.  */
      ptrdiff_t nbytes;
      struct buffer *buffer;
      /* FIXME: This can be removed: For large vectors, this field could be
	 placed *before* the vector itself.  And for small vectors on a free
	 list, this field could be stored in the vector's bytes, since the
	 empty vector is handled specially anyway.  */
      struct Lisp_Vector *vector;
    } next;
  };

/* Regular vector is just a header plus array of Lisp_Objects.  */

struct Lisp_Vector
  {
    struct vectorlike_header header;
    Lisp_Object contents[1];
  };

/* A boolvector is a kind of vectorlike, with contents are like a string.  */

struct Lisp_Bool_Vector
  {
    /* HEADER.SIZE is the vector's size field.  It doesn't have the real size,
       just the subtype information.  */
    struct vectorlike_header header;
    /* This is the size in bits.  */
    EMACS_INT size;
    /* This contains the actual bits, packed into bytes.  */
    unsigned char data[1];
  };

/* Some handy constants for calculating sizes
   and offsets, mostly of vectorlike objects.   */

enum
  {
    header_size = offsetof (struct Lisp_Vector, contents),
    bool_header_size = offsetof (struct Lisp_Bool_Vector, data),
    word_size = sizeof (Lisp_Object)
  };

/* If a struct is made to look like a vector, this macro returns the length
   of the shortest vector that would hold that struct.  */

#define VECSIZE(type)						\
  ((sizeof (type) - header_size + word_size - 1) / word_size)

/* Like VECSIZE, but used when the pseudo-vector has non-Lisp_Object fields
   at the end and we need to compute the number of Lisp_Object fields (the
   ones that the GC needs to trace).  */

#define PSEUDOVECSIZE(type, nonlispfield)			\
  ((offsetof (type, nonlispfield) - header_size) / word_size)

/* A char-table is a kind of vectorlike, with contents are like a
   vector but with a few other slots.  For some purposes, it makes
   sense to handle a char-table with type struct Lisp_Vector.  An
   element of a char table can be any Lisp objects, but if it is a sub
   char-table, we treat it a table that contains information of a
   specific range of characters.  A sub char-table has the same
   structure as a vector.  A sub char table appears only in an element
   of a char-table, and there's no way to access it directly from
   Emacs Lisp program.  */

#ifdef __GNUC__

#define CHAR_TABLE_REF_ASCII(CT, IDX)					\
  ({struct Lisp_Char_Table *_tbl = NULL;				\
    Lisp_Object _val;							\
    do {								\
      _tbl = _tbl ? XCHAR_TABLE (_tbl->parent) : XCHAR_TABLE (CT);	\
      _val = (! SUB_CHAR_TABLE_P (_tbl->ascii) ? _tbl->ascii		\
	      : XSUB_CHAR_TABLE (_tbl->ascii)->contents[IDX]);		\
      if (NILP (_val))							\
	_val = _tbl->defalt;						\
    } while (NILP (_val) && ! NILP (_tbl->parent));			\
    _val; })

#else  /* not __GNUC__ */

#define CHAR_TABLE_REF_ASCII(CT, IDX)					  \
  (! NILP (XCHAR_TABLE (CT)->ascii)					  \
   ? (! SUB_CHAR_TABLE_P (XCHAR_TABLE (CT)->ascii)			  \
      ? XCHAR_TABLE (CT)->ascii						  \
      : ! NILP (XSUB_CHAR_TABLE (XCHAR_TABLE (CT)->ascii)->contents[IDX]) \
      ? XSUB_CHAR_TABLE (XCHAR_TABLE (CT)->ascii)->contents[IDX]	  \
      : char_table_ref ((CT), (IDX)))					  \
   :  char_table_ref ((CT), (IDX)))

#endif	/* not __GNUC__ */

/* Compute A OP B, using the unsigned comparison operator OP.  A and B
   should be integer expressions.  This is not the same as
   mathematical comparison; for example, UNSIGNED_CMP (0, <, -1)
   returns 1.  For efficiency, prefer plain unsigned comparison if A
   and B's sizes both fit (after integer promotion).  */
#define UNSIGNED_CMP(a, op, b)						\
  (max (sizeof ((a) + 0), sizeof ((b) + 0)) <= sizeof (unsigned)	\
   ? ((a) + (unsigned) 0) op ((b) + (unsigned) 0)			\
   : ((a) + (uintmax_t) 0) op ((b) + (uintmax_t) 0))

/* Nonzero iff C is an ASCII character.  */
#define ASCII_CHAR_P(c) UNSIGNED_CMP (c, <, 0x80)

/* Almost equivalent to Faref (CT, IDX) with optimization for ASCII
   characters.  Do not check validity of CT.  */
#define CHAR_TABLE_REF(CT, IDX)					\
  (ASCII_CHAR_P (IDX) ? CHAR_TABLE_REF_ASCII ((CT), (IDX))	\
   : char_table_ref ((CT), (IDX)))

/* Equivalent to Faset (CT, IDX, VAL) with optimization for ASCII and
   8-bit European characters.  Do not check validity of CT.  */
#define CHAR_TABLE_SET(CT, IDX, VAL)					\
  (ASCII_CHAR_P (IDX) && SUB_CHAR_TABLE_P (XCHAR_TABLE (CT)->ascii)	\
   ? set_sub_char_table_contents (XCHAR_TABLE (CT)->ascii, IDX, VAL)	\
   : char_table_set (CT, IDX, VAL))

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
    struct vectorlike_header header;

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
    Lisp_Object extras[1];
  };

struct Lisp_Sub_Char_Table
  {
    /* HEADER.SIZE is the vector's size field, which also holds the
       pseudovector type information.  It holds the size, too.  */
    struct vectorlike_header header;

    /* Depth of this sub char-table.  It should be 1, 2, or 3.  A sub
       char-table of depth 1 contains 16 elements, and each element
       covers 4096 (128*32) characters.  A sub char-table of depth 2
       contains 32 elements, and each element covers 128 characters.  A
       sub char-table of depth 3 contains 128 elements, and each element
       is for one character.  */
    Lisp_Object depth;

    /* Minimum character covered by the sub char-table.  */
    Lisp_Object min_char;

    /* Use set_sub_char_table_contents to set this.  */
    Lisp_Object contents[1];
  };

/* This structure describes a built-in function.
   It is generated by the DEFUN macro only.
   defsubr makes it into a Lisp object.

   This type is treated in most respects as a pseudovector,
   but since we never dynamically allocate or free them,
   we don't need a struct vectorlike_header and its 'next' field.  */

struct Lisp_Subr
  {
    ptrdiff_t size;
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
    const char *doc;
  };

/* This is the number of slots that every char table must have.  This
   counts the ordinary slots and the top, defalt, parent, and purpose
   slots.  */
enum CHAR_TABLE_STANDARD_SLOTS
  {
    CHAR_TABLE_STANDARD_SLOTS = VECSIZE (struct Lisp_Char_Table) - 1
  };

/* Return the number of "extra" slots in the char table CT.  */

#define CHAR_TABLE_EXTRA_SLOTS(CT)	\
  (((CT)->header.size & PSEUDOVECTOR_SIZE_MASK) - CHAR_TABLE_STANDARD_SLOTS)


/***********************************************************************
			       Symbols
 ***********************************************************************/

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

struct Lisp_Symbol
{
  unsigned gcmarkbit : 1;

  /* Indicates where the value can be found:
     0 : it's a plain var, the value is in the `value' field.
     1 : it's a varalias, the value is really in the `alias' symbol.
     2 : it's a localized var, the value is in the `blv' object.
     3 : it's a forwarding variable, the value is in `forward'.  */
  ENUM_BF (symbol_redirect) redirect : 3;

  /* Non-zero means symbol is constant, i.e. changing its value
     should signal an error.  If the value is 3, then the var
     can be changed, but only by `defconst'.  */
  unsigned constant : 2;

  /* Interned state of the symbol.  This is an enumerator from
     enum symbol_interned.  */
  unsigned interned : 2;

  /* Non-zero means that this variable has been explicitly declared
     special (with `defvar' etc), and shouldn't be lexically bound.  */
  unsigned declared_special : 1;

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

  /* Function value of the symbol or Qunbound if not fboundp.  */
  Lisp_Object function;

  /* The symbol's property list.  */
  Lisp_Object plist;

  /* Next symbol in obarray bucket, if the symbol is interned.  */
  struct Lisp_Symbol *next;
};

/* Value is name of symbol.  */

#define SYMBOL_VAL(sym)							\
  (eassert ((sym)->redirect == SYMBOL_PLAINVAL),  sym->val.value)
#define SYMBOL_ALIAS(sym)						\
  (eassert ((sym)->redirect == SYMBOL_VARALIAS),  (sym)->val.alias)
#define SYMBOL_BLV(sym)							\
  (eassert ((sym)->redirect == SYMBOL_LOCALIZED), (sym)->val.blv)
#define SYMBOL_FWD(sym)							\
  (eassert ((sym)->redirect == SYMBOL_FORWARDED), (sym)->val.fwd)
#define SET_SYMBOL_VAL(sym, v)						\
  (eassert ((sym)->redirect == SYMBOL_PLAINVAL),  (sym)->val.value = (v))
#define SET_SYMBOL_ALIAS(sym, v)					\
  (eassert ((sym)->redirect == SYMBOL_VARALIAS),  (sym)->val.alias = (v))
#define SET_SYMBOL_BLV(sym, v)						\
  (eassert ((sym)->redirect == SYMBOL_LOCALIZED), (sym)->val.blv = (v))
#define SET_SYMBOL_FWD(sym, v)						\
  (eassert ((sym)->redirect == SYMBOL_FORWARDED), (sym)->val.fwd = (v))

#define SYMBOL_NAME(sym) XSYMBOL (sym)->name

/* Value is non-zero if SYM is an interned symbol.  */

#define SYMBOL_INTERNED_P(sym)				\
  (XSYMBOL (sym)->interned != SYMBOL_UNINTERNED)

/* Value is non-zero if SYM is interned in initial_obarray.  */

#define SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P(sym)			\
  (XSYMBOL (sym)->interned == SYMBOL_INTERNED_IN_INITIAL_OBARRAY)

/* Value is non-zero if symbol is considered a constant, i.e. its
   value cannot be changed (there is an exception for keyword symbols,
   whose value can be set to the keyword symbol itself).  */

#define SYMBOL_CONSTANT_P(sym) XSYMBOL (sym)->constant

#define DEFSYM(sym, name)						\
  do { (sym) = intern_c_string ((name)); staticpro (&(sym)); } while (0)


/***********************************************************************
			     Hash Tables
 ***********************************************************************/

/* The structure of a Lisp hash table.  */

struct Lisp_Hash_Table
{
  /* This is for Lisp; the hash table code does not refer to it.  */
  struct vectorlike_header header;

  /* Function used to compare keys.  */
  Lisp_Object test;

  /* Nil if table is non-weak.  Otherwise a symbol describing the
     weakness of the table.  */
  Lisp_Object weak;

  /* When the table is resized, and this is an integer, compute the
     new size by adding this to the old size.  If a float, compute the
     new size by multiplying the old size with this factor.  */
  Lisp_Object rehash_size;

  /* Resize hash table when number of entries/ table size is >= this
     ratio, a float.  */
  Lisp_Object rehash_threshold;

  /* Vector of hash codes.. If hash[I] is nil, this means that that
     entry I is unused.  */
  Lisp_Object hash;

  /* Vector used to chain entries.  If entry I is free, next[I] is the
     entry number of the next free item.  If entry I is non-free,
     next[I] is the index of the next entry in the collision chain.  */
  Lisp_Object next;

  /* Index of first free entry in free list.  */
  Lisp_Object next_free;

  /* Bucket vector.  A non-nil entry is the index of the first item in
     a collision chain.  This vector's size can be larger than the
     hash table size to reduce collisions.  */
  Lisp_Object index;

  /* User-supplied hash function, or nil.  */
  Lisp_Object user_hash_function;

  /* User-supplied key comparison function, or nil.  */
  Lisp_Object user_cmp_function;

  /* Only the fields above are traced normally by the GC.  The ones below
     `count' are special and are either ignored by the GC or traced in
     a special way (e.g. because of weakness).  */

  /* Number of key/value entries in the table.  */
  ptrdiff_t count;

  /* Vector of keys and values.  The key of item I is found at index
     2 * I, the value is found at index 2 * I + 1.
     This is gc_marked specially if the table is weak.  */
  Lisp_Object key_and_value;

  /* Next weak hash table if this is a weak hash table.  The head
     of the list is in weak_hash_tables.  */
  struct Lisp_Hash_Table *next_weak;

  /* C function to compare two keys.  */
  bool (*cmpfn) (struct Lisp_Hash_Table *,
		 Lisp_Object, EMACS_UINT,
		 Lisp_Object, EMACS_UINT);

  /* C function to compute hash code.  */
  EMACS_UINT (*hashfn) (struct Lisp_Hash_Table *, Lisp_Object);
};


#define XHASH_TABLE(OBJ) \
     ((struct Lisp_Hash_Table *) XUNTAG (OBJ, Lisp_Vectorlike))

#define XSET_HASH_TABLE(VAR, PTR) \
     (XSETPSEUDOVECTOR (VAR, PTR, PVEC_HASH_TABLE))

#define HASH_TABLE_P(OBJ)  PSEUDOVECTORP (OBJ, PVEC_HASH_TABLE)

#define CHECK_HASH_TABLE(x) \
  CHECK_TYPE (HASH_TABLE_P (x), Qhash_table_p, x)

/* Value is the key part of entry IDX in hash table H.  */

#define HASH_KEY(H, IDX)   AREF ((H)->key_and_value, 2 * (IDX))

/* Value is the value part of entry IDX in hash table H.  */

#define HASH_VALUE(H, IDX) AREF ((H)->key_and_value, 2 * (IDX) + 1)

/* Value is the index of the next entry following the one at IDX
   in hash table H.  */

#define HASH_NEXT(H, IDX)  AREF ((H)->next, (IDX))

/* Value is the hash code computed for entry IDX in hash table H.  */

#define HASH_HASH(H, IDX)  AREF ((H)->hash, (IDX))

/* Value is the index of the element in hash table H that is the
   start of the collision list at index IDX in the index vector of H.  */

#define HASH_INDEX(H, IDX)  AREF ((H)->index, (IDX))

/* Value is the size of hash table H.  */

#define HASH_TABLE_SIZE(H) ASIZE ((H)->next)

/* Default size for hash tables if not specified.  */

enum DEFAULT_HASH_SIZE { DEFAULT_HASH_SIZE = 65 };

/* Default threshold specifying when to resize a hash table.  The
   value gives the ratio of current entries in the hash table and the
   size of the hash table.  */

static double const DEFAULT_REHASH_THRESHOLD = 0.8;

/* Default factor by which to increase the size of a hash table.  */

static double const DEFAULT_REHASH_SIZE = 1.5;

/* These structures are used for various misc types.  */

struct Lisp_Misc_Any		/* Supertype of all Misc types.  */
{
  ENUM_BF (Lisp_Misc_Type) type : 16;		/* = Lisp_Misc_??? */
  unsigned gcmarkbit : 1;
  int spacer : 15;
};

struct Lisp_Marker
{
  ENUM_BF (Lisp_Misc_Type) type : 16;		/* = Lisp_Misc_Marker */
  unsigned gcmarkbit : 1;
  int spacer : 13;
  /* This flag is temporarily used in the functions
     decode/encode_coding_object to record that the marker position
     must be adjusted after the conversion.  */
  unsigned int need_adjustment : 1;
  /* 1 means normal insertion at the marker's position
     leaves the marker after the inserted text.  */
  unsigned int insertion_type : 1;
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
     That would also allow to preserve it ordered.  */
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
    unsigned gcmarkbit : 1;
    int spacer : 15;
    struct Lisp_Overlay *next;
    Lisp_Object start;
    Lisp_Object end;
    Lisp_Object plist;
  };

/* Hold a C pointer for later use.
   This type of object is used in the arg to record_unwind_protect.  */
struct Lisp_Save_Value
  {
    ENUM_BF (Lisp_Misc_Type) type : 16;	/* = Lisp_Misc_Save_Value */
    unsigned gcmarkbit : 1;
    int spacer : 14;
    /* If DOGC is set, POINTER is the address of a memory
       area containing INTEGER potential Lisp_Objects.  */
    unsigned int dogc : 1;
    void *pointer;
    ptrdiff_t integer;
  };


/* A miscellaneous object, when it's on the free list.  */
struct Lisp_Free
  {
    ENUM_BF (Lisp_Misc_Type) type : 16;	/* = Lisp_Misc_Free */
    unsigned gcmarkbit : 1;
    int spacer : 15;
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
  };

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
   "value" of the symbol is t if the int variable is nonzero,
   nil if it is zero.  */
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
    Lisp_Object slottype; /* Qnil, Lisp_Int, Lisp_Symbol, or Lisp_String.  */
  };

/* struct Lisp_Buffer_Local_Value is used in a symbol value cell when
   the symbol has buffer-local or frame-local bindings.  (Exception:
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
    /* 1 means that merely setting the variable creates a local
       binding for the current buffer.  */
    unsigned int local_if_set : 1;
    /* 1 means this variable can have frame-local bindings, otherwise, it is
       can have buffer-local bindings.  The two cannot be combined.  */
    unsigned int frame_local : 1;
    /* 1 means that the binding now loaded was found.
       Presumably equivalent to (defcell!=valcell).  */
    unsigned int found : 1;
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

/* Lisp floating point type.  */
struct Lisp_Float
  {
    union
    {
      double data;
      struct Lisp_Float *chain;
    } u;
  };

#define XFLOAT_DATA(f) (0 ? XFLOAT (f)->u.data :  XFLOAT (f)->u.data)
#define XFLOAT_INIT(f, n) (XFLOAT (f)->u.data = (n))

/* Most hosts nowadays use IEEE floating point, so they use IEC 60559
   representations, have infinities and NaNs, and do not trap on
   exceptions.  Define IEEE_FLOATING_POINT if this host is one of the
   typical ones.  The C11 macro __STDC_IEC_559__ is close to what is
   wanted here, but is not quite right because Emacs does not require
   all the features of C11 Annex F (and does not require C11 at all,
   for that matter).  */
#define IEEE_FLOATING_POINT (FLT_RADIX == 2 && FLT_MANT_DIG == 24 \
			     && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128)

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




/* The glyph datatype, used to represent characters on the display.
   It consists of a char code and a face id.  */

typedef struct {
  int ch;
  int face_id;
} GLYPH;

/* Return a glyph's character code.  */
#define GLYPH_CHAR(glyph) ((glyph).ch)

/* Return a glyph's face ID.  */
#define GLYPH_FACE(glyph) ((glyph).face_id)

#define SET_GLYPH_CHAR(glyph, char) ((glyph).ch = (char))
#define SET_GLYPH_FACE(glyph, face) ((glyph).face_id = (face))
#define SET_GLYPH(glyph, char, face) ((glyph).ch = (char), (glyph).face_id = (face))

/* Return 1 if GLYPH contains valid character code.  */
#define GLYPH_CHAR_VALID_P(glyph) CHAR_VALID_P (GLYPH_CHAR (glyph))


/* Glyph Code from a display vector may either be an integer which
   encodes a char code in the lower CHARACTERBITS bits and a (very small)
   face-id in the upper bits, or it may be a cons (CHAR . FACE-ID).  */

#define GLYPH_CODE_P(gc)						\
  (CONSP (gc)								\
   ? (CHARACTERP (XCAR (gc))						\
      && RANGED_INTEGERP (0, XCDR (gc), MAX_FACE_ID))			\
   : (RANGED_INTEGERP							\
      (0, gc,								\
       (MAX_FACE_ID < TYPE_MAXIMUM (EMACS_INT) >> CHARACTERBITS		\
	? ((EMACS_INT) MAX_FACE_ID << CHARACTERBITS) | MAX_CHAR		\
	: TYPE_MAXIMUM (EMACS_INT)))))

/* The following are valid only if GLYPH_CODE_P (gc).  */

#define GLYPH_CODE_CHAR(gc) \
  (CONSP (gc) ? XINT (XCAR (gc)) : XINT (gc) & ((1 << CHARACTERBITS) - 1))

#define GLYPH_CODE_FACE(gc) \
  (CONSP (gc) ? XINT (XCDR (gc)) : XINT (gc) >> CHARACTERBITS)

#define SET_GLYPH_FROM_GLYPH_CODE(glyph, gc)				\
  do									\
    {									\
      if (CONSP (gc))							\
	SET_GLYPH (glyph, XINT (XCAR (gc)), XINT (XCDR (gc)));		\
      else								\
	SET_GLYPH (glyph, (XINT (gc) & ((1 << CHARACTERBITS)-1)),	\
		   (XINT (gc) >> CHARACTERBITS));			\
    }									\
  while (0)

/* Structure to hold mouse highlight data.  This is here because other
   header files need it for defining struct x_output etc.  */
typedef struct {
  /* These variables describe the range of text currently shown in its
     mouse-face, together with the window they apply to.  As long as
     the mouse stays within this range, we need not redraw anything on
     its account.  Rows and columns are glyph matrix positions in
     MOUSE_FACE_WINDOW.  */
  int mouse_face_beg_row, mouse_face_beg_col;
  int mouse_face_beg_x, mouse_face_beg_y;
  int mouse_face_end_row, mouse_face_end_col;
  int mouse_face_end_x, mouse_face_end_y;
  int mouse_face_past_end;
  Lisp_Object mouse_face_window;
  int mouse_face_face_id;
  Lisp_Object mouse_face_overlay;

  /* 1 if a mouse motion event came and we didn't handle it right away because
     gc was in progress.  */
  int mouse_face_deferred_gc;

  /* FRAME and X, Y position of mouse when last checked for
     highlighting.  X and Y can be negative or out of range for the frame.  */
  struct frame *mouse_face_mouse_frame;
  int mouse_face_mouse_x, mouse_face_mouse_y;

  /* Nonzero means defer mouse-motion highlighting.  */
  int mouse_face_defer;

  /* Nonzero means that the mouse highlight should not be shown.  */
  int mouse_face_hidden;

  int mouse_face_image_state;
} Mouse_HLInfo;

/* Data type checking.  */

#define NILP(x)  EQ (x, Qnil)

#define NUMBERP(x) (INTEGERP (x) || FLOATP (x))
#define NATNUMP(x) (INTEGERP (x) && XINT (x) >= 0)

#define RANGED_INTEGERP(lo, x, hi) \
  (INTEGERP (x) && (lo) <= XINT (x) && XINT (x) <= (hi))
#define TYPE_RANGED_INTEGERP(type, x) \
  (TYPE_SIGNED (type)							\
   ? RANGED_INTEGERP (TYPE_MINIMUM (type), x, TYPE_MAXIMUM (type))	\
   : RANGED_INTEGERP (0, x, TYPE_MAXIMUM (type)))

#define INTEGERP(x) (LISP_INT_TAG_P (XTYPE ((x))))
#define SYMBOLP(x) (XTYPE ((x)) == Lisp_Symbol)
#define MISCP(x) (XTYPE ((x)) == Lisp_Misc)
#define VECTORLIKEP(x) (XTYPE ((x)) == Lisp_Vectorlike)
#define STRINGP(x) (XTYPE ((x)) == Lisp_String)
#define CONSP(x) (XTYPE ((x)) == Lisp_Cons)

#define FLOATP(x) (XTYPE ((x)) == Lisp_Float)
#define VECTORP(x) (VECTORLIKEP (x) && !(ASIZE (x) & PSEUDOVECTOR_FLAG))
#define OVERLAYP(x) (MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Overlay)
#define MARKERP(x) (MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Marker)
#define SAVE_VALUEP(x) (MISCP (x) && XMISCTYPE (x) == Lisp_Misc_Save_Value)

#define INTFWDP(x) (XFWDTYPE (x) == Lisp_Fwd_Int)
#define BOOLFWDP(x) (XFWDTYPE (x) == Lisp_Fwd_Bool)
#define OBJFWDP(x) (XFWDTYPE (x) == Lisp_Fwd_Obj)
#define BUFFER_OBJFWDP(x) (XFWDTYPE (x) == Lisp_Fwd_Buffer_Obj)
#define KBOARD_OBJFWDP(x) (XFWDTYPE (x) == Lisp_Fwd_Kboard_Obj)

/* True if object X is a pseudovector whose code is CODE.  The cast to struct
   vectorlike_header * avoids aliasing issues.  */
#define PSEUDOVECTORP(x, code)					\
  TYPED_PSEUDOVECTORP (x, vectorlike_header, code)

#define PSEUDOVECTOR_TYPEP(v, code)					\
  (((v)->size & (PSEUDOVECTOR_FLAG | PVEC_TYPE_MASK))			\
   == (PSEUDOVECTOR_FLAG | ((code) << PSEUDOVECTOR_SIZE_BITS)))

/* True if object X, with internal type struct T *, is a pseudovector whose
   code is CODE.  */
#define TYPED_PSEUDOVECTORP(x, t, code)				\
  (VECTORLIKEP (x)						\
   && PSEUDOVECTOR_TYPEP ((struct t *) XUNTAG (x, Lisp_Vectorlike), code))

/* Test for specific pseudovector types.  */
#define WINDOW_CONFIGURATIONP(x) PSEUDOVECTORP (x, PVEC_WINDOW_CONFIGURATION)
#define PROCESSP(x) PSEUDOVECTORP (x, PVEC_PROCESS)
#define WINDOWP(x) PSEUDOVECTORP (x, PVEC_WINDOW)
#define TERMINALP(x) PSEUDOVECTORP (x, PVEC_TERMINAL)
/* SUBRP is special since Lisp_Subr lacks struct vectorlike_header.  */
#define SUBRP(x) TYPED_PSEUDOVECTORP (x, Lisp_Subr, PVEC_SUBR)
#define COMPILEDP(x) PSEUDOVECTORP (x, PVEC_COMPILED)
#define BUFFERP(x) PSEUDOVECTORP (x, PVEC_BUFFER)
#define CHAR_TABLE_P(x) PSEUDOVECTORP (x, PVEC_CHAR_TABLE)
#define SUB_CHAR_TABLE_P(x) PSEUDOVECTORP (x, PVEC_SUB_CHAR_TABLE)
#define BOOL_VECTOR_P(x) PSEUDOVECTORP (x, PVEC_BOOL_VECTOR)
#define FRAMEP(x) PSEUDOVECTORP (x, PVEC_FRAME)

/* Test for image (image . spec)  */
#define IMAGEP(x) (CONSP (x) && EQ (XCAR (x), Qimage))

/* Array types.  */

#define ARRAYP(x) \
  (VECTORP (x) || STRINGP (x) || CHAR_TABLE_P (x) || BOOL_VECTOR_P (x))

#define CHECK_LIST(x) \
  CHECK_TYPE (CONSP (x) || NILP (x), Qlistp, x)

#define CHECK_LIST_CONS(x, y) \
  CHECK_TYPE (CONSP (x), Qlistp, y)

#define CHECK_LIST_END(x, y) \
  CHECK_TYPE (NILP (x), Qlistp, y)

#define CHECK_STRING(x) \
  CHECK_TYPE (STRINGP (x), Qstringp, x)

#define CHECK_STRING_CAR(x) \
  CHECK_TYPE (STRINGP (XCAR (x)), Qstringp, XCAR (x))

#define CHECK_CONS(x) \
  CHECK_TYPE (CONSP (x), Qconsp, x)

#define CHECK_SYMBOL(x) \
  CHECK_TYPE (SYMBOLP (x), Qsymbolp, x)

#define CHECK_CHAR_TABLE(x) \
  CHECK_TYPE (CHAR_TABLE_P (x), Qchar_table_p, x)

#define CHECK_VECTOR(x) \
  CHECK_TYPE (VECTORP (x), Qvectorp, x)

#define CHECK_VECTOR_OR_STRING(x) \
  CHECK_TYPE (VECTORP (x) || STRINGP (x), Qarrayp, x)

#define CHECK_ARRAY(x, Qxxxp) \
  CHECK_TYPE (ARRAYP (x), Qxxxp, x)

#define CHECK_VECTOR_OR_CHAR_TABLE(x) \
  CHECK_TYPE (VECTORP (x) || CHAR_TABLE_P (x), Qvector_or_char_table_p, x)

#define CHECK_BUFFER(x) \
  CHECK_TYPE (BUFFERP (x), Qbufferp, x)

#define CHECK_WINDOW(x) \
  CHECK_TYPE (WINDOWP (x), Qwindowp, x)

#define CHECK_WINDOW_CONFIGURATION(x) \
  CHECK_TYPE (WINDOW_CONFIGURATIONP (x), Qwindow_configuration_p, x)

/* A window of any sort, leaf or interior, is "valid" if one of its
   buffer, vchild, or hchild members is non-nil.  */
#define CHECK_VALID_WINDOW(x)				\
  CHECK_TYPE (WINDOWP (x)				\
	      && (!NILP (XWINDOW (x)->buffer)		\
		  || !NILP (XWINDOW (x)->vchild)	\
		  || !NILP (XWINDOW (x)->hchild)),	\
	      Qwindow_valid_p, x)

/* A window is "live" if and only if it shows a buffer.  */
#define CHECK_LIVE_WINDOW(x)						\
  CHECK_TYPE (WINDOWP (x) && !NILP (XWINDOW (x)->buffer),		\
	      Qwindow_live_p, x)

#define CHECK_PROCESS(x) \
  CHECK_TYPE (PROCESSP (x), Qprocessp, x)

#define CHECK_SUBR(x) \
  CHECK_TYPE (SUBRP (x), Qsubrp, x)

#define CHECK_NUMBER(x) \
  CHECK_TYPE (INTEGERP (x), Qintegerp, x)

#define CHECK_NATNUM(x) \
  CHECK_TYPE (NATNUMP (x), Qwholenump, x)

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
  } while (0)
#define CHECK_TYPE_RANGED_INTEGER(type, x) \
  do {									\
    if (TYPE_SIGNED (type))						\
      CHECK_RANGED_INTEGER (x, TYPE_MINIMUM (type), TYPE_MAXIMUM (type)); \
    else								\
      CHECK_RANGED_INTEGER (x, 0, TYPE_MAXIMUM (type));			\
  } while (0)

#define CHECK_MARKER(x) \
  CHECK_TYPE (MARKERP (x), Qmarkerp, x)

#define CHECK_NUMBER_COERCE_MARKER(x) \
  do { if (MARKERP ((x))) XSETFASTINT (x, marker_position (x)); \
    else CHECK_TYPE (INTEGERP (x), Qinteger_or_marker_p, x); } while (0)

#define XFLOATINT(n) extract_float((n))

#define CHECK_FLOAT(x) \
  CHECK_TYPE (FLOATP (x), Qfloatp, x)

#define CHECK_NUMBER_OR_FLOAT(x) \
  CHECK_TYPE (FLOATP (x) || INTEGERP (x), Qnumberp, x)

#define CHECK_NUMBER_OR_FLOAT_COERCE_MARKER(x) \
  do { if (MARKERP (x)) XSETFASTINT (x, marker_position (x)); \
    else CHECK_TYPE (INTEGERP (x) || FLOATP (x), Qnumber_or_marker_p, x); } while (0)

#define CHECK_OVERLAY(x) \
  CHECK_TYPE (OVERLAYP (x), Qoverlayp, x)

/* Since we can't assign directly to the CAR or CDR fields of a cons
   cell, use these when checking that those fields contain numbers.  */
#define CHECK_NUMBER_CAR(x) \
  do {					\
    Lisp_Object tmp = XCAR (x);		\
    CHECK_NUMBER (tmp);			\
    XSETCAR ((x), tmp);			\
  } while (0)

#define CHECK_NUMBER_CDR(x) \
  do {					\
    Lisp_Object tmp = XCDR (x);		\
    CHECK_NUMBER (tmp);			\
    XSETCDR ((x), tmp);			\
  } while (0)

#define CHECK_NATNUM_CAR(x) \
  do {					\
    Lisp_Object tmp = XCAR (x);		\
    CHECK_NATNUM (tmp);			\
    XSETCAR ((x), tmp);			\
  } while (0)

#define CHECK_NATNUM_CDR(x) \
  do {					\
    Lisp_Object tmp = XCDR (x);		\
    CHECK_NATNUM (tmp);			\
    XSETCDR ((x), tmp);			\
  } while (0)

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
#ifdef _MSC_VER
#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc)	\
   Lisp_Object fnname DEFUN_ARGS_ ## maxargs ;				\
   static struct Lisp_Subr alignas (GCALIGNMENT) sname =		\
   { (PVEC_SUBR << PSEUDOVECTOR_SIZE_BITS)				\
     | (sizeof (struct Lisp_Subr) / sizeof (EMACS_INT)),		\
      { (Lisp_Object (__cdecl *)(void))fnname },                        \
       minargs, maxargs, lname, intspec, 0};				\
   Lisp_Object fnname
#else  /* not _MSC_VER */
#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc)	\
   Lisp_Object fnname DEFUN_ARGS_ ## maxargs ;				\
   static struct Lisp_Subr alignas (GCALIGNMENT) sname =		\
     { PVEC_SUBR << PSEUDOVECTOR_SIZE_BITS,				\
      { .a ## maxargs = fnname },					\
       minargs, maxargs, lname, intspec, 0};				\
   Lisp_Object fnname
#endif

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

/* Non-zero if OBJ is a Lisp function.  */
#define FUNCTIONP(OBJ) functionp(OBJ)

/* defsubr (Sname);
   is how we define the symbol for function `name' at start-up time.  */
extern void defsubr (struct Lisp_Subr *);

enum maxargs
  {
    MANY = -2,
    UNEVALLED = -1
  };

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
  } while (0)
#define DEFVAR_LISP_NOPRO(lname, vname, doc)	\
  do {						\
    static struct Lisp_Objfwd o_fwd;		\
    defvar_lisp_nopro (&o_fwd, lname, &globals.f_ ## vname);	\
  } while (0)
#define DEFVAR_BOOL(lname, vname, doc)		\
  do {						\
    static struct Lisp_Boolfwd b_fwd;		\
    defvar_bool (&b_fwd, lname, &globals.f_ ## vname);		\
  } while (0)
#define DEFVAR_INT(lname, vname, doc)		\
  do {						\
    static struct Lisp_Intfwd i_fwd;		\
    defvar_int (&i_fwd, lname, &globals.f_ ## vname);		\
  } while (0)

#define DEFVAR_BUFFER_DEFAULTS(lname, vname, doc)		\
  do {								\
    static struct Lisp_Objfwd o_fwd;				\
    defvar_lisp_nopro (&o_fwd, lname, &BVAR (&buffer_defaults, vname));	\
  } while (0)

#define DEFVAR_KBOARD(lname, vname, doc)			\
  do {								\
    static struct Lisp_Kboard_Objfwd ko_fwd;			\
    defvar_kboard (&ko_fwd, lname, offsetof (KBOARD, vname ## _)); \
  } while (0)

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


/* Structure for recording Lisp call stack for backtrace purposes.  */

/* The special binding stack holds the outer values of variables while
   they are bound by a function application or a let form, stores the
   code to be executed for Lisp unwind-protect forms, and stores the C
   functions to be called for record_unwind_protect.

   If func is non-zero, undoing this binding applies func to old_value;
      This implements record_unwind_protect.

   Otherwise, the element is a variable binding.

   If the symbol field is a symbol, it is an ordinary variable binding.

   Otherwise, it should be a structure (SYMBOL WHERE . CURRENT-BUFFER),
   which means having bound a local value while CURRENT-BUFFER was active.
   If WHERE is nil this means we saw the default value when binding SYMBOL.
   WHERE being a buffer or frame means we saw a buffer-local or frame-local
   value.  Other values of WHERE mean an internal error.  */

typedef Lisp_Object (*specbinding_func) (Lisp_Object);

struct specbinding
  {
    Lisp_Object symbol, old_value;
    specbinding_func func;
    Lisp_Object unused;		/* Dividing by 16 is faster than by 12.  */
  };

extern struct specbinding *specpdl;
extern struct specbinding *specpdl_ptr;
extern ptrdiff_t specpdl_size;

#define SPECPDL_INDEX()	(specpdl_ptr - specpdl)

struct backtrace
{
  struct backtrace *next;
  Lisp_Object function;
  Lisp_Object *args;	/* Points to vector of args.  */
  ptrdiff_t nargs;	/* Length of vector.  */
  /* Nonzero means call value of debugger when done with this operation.  */
  unsigned int debug_on_exit : 1;
};

extern struct backtrace *backtrace_list;

/* Everything needed to describe an active condition case.

   Members are volatile if their values need to survive _longjmp when
   a 'struct handler' is a local variable.  */
struct handler
  {
    /* The handler clauses and variable from the condition-case form.  */
    /* For a handler set up in Lisp code, this is always a list.
       For an internal handler set up by internal_condition_case*,
       this can instead be the symbol t or `error'.
       t: handle all conditions.
       error: handle all conditions, and errors can run the debugger
              or display a backtrace.  */
    Lisp_Object handler;

    Lisp_Object volatile var;

    /* Fsignal stores here the condition-case clause that applies,
       and Fcondition_case thus knows which clause to run.  */
    Lisp_Object volatile chosen_clause;

    /* Used to effect the longjump out to the handler.  */
    struct catchtag *tag;

    /* The next enclosing handler.  */
    struct handler *next;
  };

/* This structure helps implement the `catch' and `throw' control
   structure.  A struct catchtag contains all the information needed
   to restore the state of the interpreter after a non-local jump.

   Handlers for error conditions (represented by `struct handler'
   structures) just point to a catch tag to do the cleanup required
   for their jumps.

   catchtag structures are chained together in the C calling stack;
   the `next' member points to the next outer catchtag.

   A call like (throw TAG VAL) searches for a catchtag whose `tag'
   member is TAG, and then unbinds to it.  The `val' member is used to
   hold VAL while the stack is unwound; `val' is returned as the value
   of the catch form.

   All the other members are concerned with restoring the interpreter
   state.

   Members are volatile if their values need to survive _longjmp when
   a 'struct catchtag' is a local variable.  */
struct catchtag
{
  Lisp_Object tag;
  Lisp_Object volatile val;
  struct catchtag *volatile next;
  struct gcpro *gcpro;
  sys_jmp_buf jmp;
  struct backtrace *backlist;
  struct handler *handlerlist;
  EMACS_INT lisp_eval_depth;
  ptrdiff_t volatile pdlcount;
  int poll_suppress_count;
  int interrupt_input_blocked;
  struct byte_stack *byte_stack;
};

extern Lisp_Object memory_signal_data;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
extern char *stack_bottom;

/* Check quit-flag and quit if it is non-nil.
   Typing C-g does not directly cause a quit; it only sets Vquit_flag.
   So the program needs to do QUIT at times when it is safe to quit.
   Every loop that might run for a long time or might not exit
   ought to do QUIT at least once, at a safe place.
   Unless that is impossible, of course.
   But it is very desirable to avoid creating loops where QUIT is impossible.

   Exception: if you set immediate_quit to nonzero,
   then the handler that responds to the C-g does the quit itself.
   This is a good thing to do around a loop that has no side effects
   and (in particular) cannot call arbitrary Lisp code.

   If quit-flag is set to `kill-emacs' the SIGINT handler has received
   a request to exit Emacs when it is safe to do.  */

extern void process_pending_signals (void);
extern bool volatile pending_signals;

extern void process_quit_flag (void);
#define QUIT						\
  do {							\
    if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))	\
      process_quit_flag ();				\
    else if (pending_signals)				\
      process_pending_signals ();			\
  } while (0)


/* Nonzero if ought to quit now.  */

#define QUITP (!NILP (Vquit_flag) && NILP (Vinhibit_quit))

extern Lisp_Object Vascii_downcase_table;
extern Lisp_Object Vascii_canon_table;

/* Structure for recording stack slots that need marking.  */

/* This is a chain of structures, each of which points at a Lisp_Object
   variable whose value should be marked in garbage collection.
   Normally every link of the chain is an automatic variable of a function,
   and its `val' points to some argument or local variable of the function.
   On exit to the function, the chain is set back to the value it had on entry.
   This way, no link remains in the chain when the stack frame containing the
   link disappears.

   Every function that can call Feval must protect in this fashion all
   Lisp_Object variables whose contents will be used again.  */

extern struct gcpro *gcprolist;

struct gcpro
{
  struct gcpro *next;

  /* Address of first protected variable.  */
  volatile Lisp_Object *var;

  /* Number of consecutive protected variables.  */
  ptrdiff_t nvars;

#ifdef DEBUG_GCPRO
  int level;
#endif
};

/* Values of GC_MARK_STACK during compilation:

   0	Use GCPRO as before
   1	Do the real thing, make GCPROs and UNGCPRO no-ops.
   2    Mark the stack, and check that everything GCPRO'd is
	marked.
   3	Mark using GCPRO's, mark stack last, and count how many
	dead objects are kept alive.  */


#define GC_USE_GCPROS_AS_BEFORE		0
#define GC_MAKE_GCPROS_NOOPS		1
#define GC_MARK_STACK_CHECK_GCPROS	2
#define GC_USE_GCPROS_CHECK_ZOMBIES	3

#ifndef GC_MARK_STACK
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS
#endif

/* Whether we do the stack marking manually.  */
#define BYTE_MARK_STACK !(GC_MARK_STACK == GC_MAKE_GCPROS_NOOPS		\
			  || GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS)


#if GC_MARK_STACK == GC_MAKE_GCPROS_NOOPS

/* Do something silly with gcproN vars just so gcc shuts up.  */
/* You get warnings from MIPSPro...  */

#define GCPRO1(varname) ((void) gcpro1)
#define GCPRO2(varname1, varname2) ((void) gcpro2, (void) gcpro1)
#define GCPRO3(varname1, varname2, varname3) \
  ((void) gcpro3, (void) gcpro2, (void) gcpro1)
#define GCPRO4(varname1, varname2, varname3, varname4) \
  ((void) gcpro4, (void) gcpro3, (void) gcpro2, (void) gcpro1)
#define GCPRO5(varname1, varname2, varname3, varname4, varname5) \
  ((void) gcpro5, (void) gcpro4, (void) gcpro3, (void) gcpro2, (void) gcpro1)
#define GCPRO6(varname1, varname2, varname3, varname4, varname5, varname6) \
  ((void) gcpro6, (void) gcpro5, (void) gcpro4, (void) gcpro3, (void) gcpro2, \
   (void) gcpro1)
#define UNGCPRO ((void) 0)

#else /* GC_MARK_STACK != GC_MAKE_GCPROS_NOOPS */

#ifndef DEBUG_GCPRO

#define GCPRO1(varname) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname; gcpro1.nvars = 1; \
  gcprolist = &gcpro1; }

#define GCPRO2(varname1, varname2) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcprolist = &gcpro2; }

#define GCPRO3(varname1, varname2, varname3) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcprolist = &gcpro3; }

#define GCPRO4(varname1, varname2, varname3, varname4) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcprolist = &gcpro4; }

#define GCPRO5(varname1, varname2, varname3, varname4, varname5) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro5.next = &gcpro4; gcpro5.var = &varname5; gcpro5.nvars = 1; \
  gcprolist = &gcpro5; }

#define GCPRO6(varname1, varname2, varname3, varname4, varname5, varname6) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro5.next = &gcpro4; gcpro5.var = &varname5; gcpro5.nvars = 1; \
  gcpro6.next = &gcpro5; gcpro6.var = &varname6; gcpro6.nvars = 1; \
  gcprolist = &gcpro6; }

#define UNGCPRO (gcprolist = gcpro1.next)

#else

extern int gcpro_level;

#define GCPRO1(varname) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname; gcpro1.nvars = 1; \
  gcpro1.level = gcpro_level++; \
  gcprolist = &gcpro1; }

#define GCPRO2(varname1, varname2) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro1.level = gcpro_level; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro2.level = gcpro_level++; \
  gcprolist = &gcpro2; }

#define GCPRO3(varname1, varname2, varname3) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro1.level = gcpro_level; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro3.level = gcpro_level++; \
  gcprolist = &gcpro3; }

#define GCPRO4(varname1, varname2, varname3, varname4) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro1.level = gcpro_level; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro4.level = gcpro_level++; \
  gcprolist = &gcpro4; }

#define GCPRO5(varname1, varname2, varname3, varname4, varname5) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro1.level = gcpro_level; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro5.next = &gcpro4; gcpro5.var = &varname5; gcpro5.nvars = 1; \
  gcpro5.level = gcpro_level++; \
  gcprolist = &gcpro5; }

#define GCPRO6(varname1, varname2, varname3, varname4, varname5, varname6) \
 {gcpro1.next = gcprolist; gcpro1.var = &varname1; gcpro1.nvars = 1; \
  gcpro1.level = gcpro_level; \
  gcpro2.next = &gcpro1; gcpro2.var = &varname2; gcpro2.nvars = 1; \
  gcpro3.next = &gcpro2; gcpro3.var = &varname3; gcpro3.nvars = 1; \
  gcpro4.next = &gcpro3; gcpro4.var = &varname4; gcpro4.nvars = 1; \
  gcpro5.next = &gcpro4; gcpro5.var = &varname5; gcpro5.nvars = 1; \
  gcpro6.next = &gcpro5; gcpro6.var = &varname6; gcpro6.nvars = 1; \
  gcpro6.level = gcpro_level++; \
  gcprolist = &gcpro6; }

#define UNGCPRO					\
 ((--gcpro_level != gcpro1.level)		\
  ? (emacs_abort (), 0)				\
  : ((gcprolist = gcpro1.next), 0))

#endif /* DEBUG_GCPRO */
#endif /* GC_MARK_STACK != GC_MAKE_GCPROS_NOOPS */


/* Evaluate expr, UNGCPRO, and then return the value of expr.  */
#define RETURN_UNGCPRO(expr)			\
do						\
    {						\
      Lisp_Object ret_ungc_val;			\
      ret_ungc_val = (expr);			\
      UNGCPRO;					\
      return ret_ungc_val;			\
    }						\
while (0)

/* Call staticpro (&var) to protect static variable `var'.  */

void staticpro (Lisp_Object *);

/* Declare a Lisp-callable function.  The MAXARGS parameter has the same
   meaning as in the DEFUN macro, and is used to construct a prototype.  */
/* We can use the same trick as in the DEFUN macro to generate the
   appropriate prototype.  */
#define EXFUN(fnname, maxargs) \
  extern Lisp_Object fnname DEFUN_ARGS_ ## maxargs

/* Forward declarations for prototypes.  */
struct window;
struct frame;

/* Simple access functions.  */

LISP_INLINE Lisp_Object *
aref_addr (Lisp_Object array, ptrdiff_t idx)
{
  return & XVECTOR (array)->contents[idx];
}

LISP_INLINE void
gc_aset (Lisp_Object array, ptrdiff_t idx, Lisp_Object val)
{
  /* Like ASET, but also can be used in the garbage collector:
     sweep_weak_table calls set_hash_key etc. while the table is marked.  */
  eassert (0 <= idx && idx < (ASIZE (array) & ~ARRAY_MARK_FLAG));
  XVECTOR (array)->contents[idx] = val;
}

/* Copy COUNT Lisp_Objects from ARGS to contents of V starting from OFFSET.  */

LISP_INLINE void
vcopy (Lisp_Object v, ptrdiff_t offset, Lisp_Object *args, ptrdiff_t count)
{
  eassert (0 <= offset && 0 <= count && offset + count <= ASIZE (v));
  memcpy (XVECTOR (v)->contents + offset, args, count * sizeof *args);
}

/* Functions to modify hash tables.  */

LISP_INLINE void
set_hash_key_and_value (struct Lisp_Hash_Table *h, Lisp_Object key_and_value)
{
  h->key_and_value = key_and_value;
}

LISP_INLINE void
set_hash_key_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->key_and_value, 2 * idx, val);
}

LISP_INLINE void
set_hash_value_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->key_and_value, 2 * idx + 1, val);
}

LISP_INLINE void
set_hash_next (struct Lisp_Hash_Table *h, Lisp_Object next)
{
  h->next = next;
}

LISP_INLINE void
set_hash_next_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->next, idx, val);
}

LISP_INLINE void
set_hash_hash (struct Lisp_Hash_Table *h, Lisp_Object hash)
{
  h->hash = hash;
}

LISP_INLINE void
set_hash_hash_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->hash, idx, val);
}

LISP_INLINE void
set_hash_index (struct Lisp_Hash_Table *h, Lisp_Object index)
{
  h->index = index;
}

LISP_INLINE void
set_hash_index_slot (struct Lisp_Hash_Table *h, ptrdiff_t idx, Lisp_Object val)
{
  gc_aset (h->index, idx, val);
}

/* Use these functions to set Lisp_Object
   or pointer slots of struct Lisp_Symbol.  */

LISP_INLINE void
set_symbol_name (Lisp_Object sym, Lisp_Object name)
{
  XSYMBOL (sym)->name = name;
}

LISP_INLINE void
set_symbol_function (Lisp_Object sym, Lisp_Object function)
{
  XSYMBOL (sym)->function = function;
}

LISP_INLINE void
set_symbol_plist (Lisp_Object sym, Lisp_Object plist)
{
  XSYMBOL (sym)->plist = plist;
}

LISP_INLINE void
set_symbol_next (Lisp_Object sym, struct Lisp_Symbol *next)
{
  XSYMBOL (sym)->next = next;
}

/* Buffer-local (also frame-local) variable access functions.  */

LISP_INLINE int
blv_found (struct Lisp_Buffer_Local_Value *blv)
{
  eassert (blv->found == !EQ (blv->defcell, blv->valcell));
  return blv->found;
}

LISP_INLINE void
set_blv_found (struct Lisp_Buffer_Local_Value *blv, int found)
{
  eassert (found == !EQ (blv->defcell, blv->valcell));
  blv->found = found;
}

LISP_INLINE Lisp_Object
blv_value (struct Lisp_Buffer_Local_Value *blv)
{
  return XCDR (blv->valcell);
}

LISP_INLINE void
set_blv_value (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  XSETCDR (blv->valcell, val);
}

LISP_INLINE void
set_blv_where (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  blv->where = val;
}

LISP_INLINE void
set_blv_defcell (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  blv->defcell = val;
}

LISP_INLINE void
set_blv_valcell (struct Lisp_Buffer_Local_Value *blv, Lisp_Object val)
{
  blv->valcell = val;
}

/* Set overlay's property list.  */

LISP_INLINE void
set_overlay_plist (Lisp_Object overlay, Lisp_Object plist)
{
  XOVERLAY (overlay)->plist = plist;
}

/* Get text properties of S.  */

LISP_INLINE INTERVAL
string_intervals (Lisp_Object s)
{
  return XSTRING (s)->intervals;
}

/* Set text properties of S to I.  */

LISP_INLINE void
set_string_intervals (Lisp_Object s, INTERVAL i)
{
  XSTRING (s)->intervals = i;
}

/* Set a Lisp slot in TABLE to VAL.  Most code should use this instead
   of setting slots directly.  */

LISP_INLINE void
set_char_table_ascii (Lisp_Object table, Lisp_Object val)
{
  XCHAR_TABLE (table)->ascii = val;
}
LISP_INLINE void
set_char_table_defalt (Lisp_Object table, Lisp_Object val)
{
  XCHAR_TABLE (table)->defalt = val;
}
LISP_INLINE void
set_char_table_parent (Lisp_Object table, Lisp_Object val)
{
  XCHAR_TABLE (table)->parent = val;
}
LISP_INLINE void
set_char_table_purpose (Lisp_Object table, Lisp_Object val)
{
  XCHAR_TABLE (table)->purpose = val;
}

/* Set different slots in (sub)character tables.  */

LISP_INLINE void
set_char_table_extras (Lisp_Object table, ptrdiff_t idx, Lisp_Object val)
{
  eassert (0 <= idx && idx < CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (table)));
  XCHAR_TABLE (table)->extras[idx] = val;
}

LISP_INLINE void
set_char_table_contents (Lisp_Object table, ptrdiff_t idx, Lisp_Object val)
{
  eassert (0 <= idx && idx < (1 << CHARTAB_SIZE_BITS_0));
  XCHAR_TABLE (table)->contents[idx] = val;
}

LISP_INLINE void
set_sub_char_table_contents (Lisp_Object table, ptrdiff_t idx, Lisp_Object val)
{
  XSUB_CHAR_TABLE (table)->contents[idx] = val;
}

/* Defined in data.c.  */
extern Lisp_Object Qnil, Qt, Qquote, Qlambda, Qunbound;
extern Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
extern Lisp_Object Qerror, Qquit, Qargs_out_of_range;
extern Lisp_Object Qvoid_variable, Qvoid_function;
extern Lisp_Object Qinvalid_read_syntax;
extern Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
extern Lisp_Object Quser_error, Qend_of_file, Qarith_error, Qmark_inactive;
extern Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
extern Lisp_Object Qtext_read_only;
extern Lisp_Object Qinteractive_form;
extern Lisp_Object Qcircular_list;
extern Lisp_Object Qintegerp, Qwholenump, Qsymbolp, Qlistp, Qconsp;
extern Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
extern Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qvectorp;
extern Lisp_Object Qbuffer_or_string_p;
extern Lisp_Object Qfboundp;
extern Lisp_Object Qchar_table_p, Qvector_or_char_table_p;

extern Lisp_Object Qcdr;

extern Lisp_Object Qrange_error, Qoverflow_error;

extern Lisp_Object Qfloatp;
extern Lisp_Object Qnumberp, Qnumber_or_marker_p;

extern Lisp_Object Qbuffer, Qinteger, Qsymbol;

extern Lisp_Object Qfont_spec, Qfont_entity, Qfont_object;

EXFUN (Fbyteorder, 0) ATTRIBUTE_CONST;

/* Defined in frame.c.  */
extern Lisp_Object Qframep;

/* Defined in data.c.  */
extern Lisp_Object indirect_function (Lisp_Object);
extern Lisp_Object find_symbol_value (Lisp_Object);

/* Convert the integer I to an Emacs representation, either the integer
   itself, or a cons of two or three integers, or if all else fails a float.
   I should not have side effects.  */
#define INTEGER_TO_CONS(i)					    \
  (! FIXNUM_OVERFLOW_P (i)					    \
   ? make_number (i)						    \
   : ! ((FIXNUM_OVERFLOW_P (INTMAX_MIN >> 16)			    \
	 || FIXNUM_OVERFLOW_P (UINTMAX_MAX >> 16))		    \
	&& FIXNUM_OVERFLOW_P ((i) >> 16))			    \
   ? Fcons (make_number ((i) >> 16), make_number ((i) & 0xffff))    \
   : ! ((FIXNUM_OVERFLOW_P (INTMAX_MIN >> 16 >> 24)		    \
	 || FIXNUM_OVERFLOW_P (UINTMAX_MAX >> 16 >> 24))	    \
	&& FIXNUM_OVERFLOW_P ((i) >> 16 >> 24))			    \
   ? Fcons (make_number ((i) >> 16 >> 24),			    \
	    Fcons (make_number ((i) >> 16 & 0xffffff),		    \
		   make_number ((i) & 0xffff)))			    \
   : make_float (i))

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
extern _Noreturn Lisp_Object wrong_type_argument (Lisp_Object, Lisp_Object);
extern Lisp_Object do_symval_forwarding (union Lisp_Fwd *);
extern void set_internal (Lisp_Object, Lisp_Object, Lisp_Object, bool);
extern void syms_of_data (void);
extern void swap_in_global_binding (struct Lisp_Symbol *);

/* Defined in cmds.c */
extern void syms_of_cmds (void);
extern void keys_of_cmds (void);

/* Defined in coding.c.  */
extern Lisp_Object Qcharset;
extern Lisp_Object detect_coding_system (const unsigned char *, ptrdiff_t,
                                         ptrdiff_t, bool, bool, Lisp_Object);
extern void init_coding (void);
extern void init_coding_once (void);
extern void syms_of_coding (void);

/* Defined in character.c.  */
EXFUN (Fmax_char, 0) ATTRIBUTE_CONST;
extern ptrdiff_t chars_in_text (const unsigned char *, ptrdiff_t);
extern ptrdiff_t multibyte_chars_in_text (const unsigned char *, ptrdiff_t);
extern int multibyte_char_to_unibyte (int) ATTRIBUTE_CONST;
extern int multibyte_char_to_unibyte_safe (int) ATTRIBUTE_CONST;
extern void syms_of_character (void);

/* Defined in charset.c.  */
extern void init_charset (void);
extern void init_charset_once (void);
extern void syms_of_charset (void);
/* Structure forward declarations.  */
struct charset;

/* Defined in composite.c.  */
extern void syms_of_composite (void);

/* Defined in syntax.c.  */
extern void init_syntax_once (void);
extern void syms_of_syntax (void);

/* Defined in fns.c.  */
extern Lisp_Object QCrehash_size, QCrehash_threshold;
enum { NEXT_ALMOST_PRIME_LIMIT = 11 };
EXFUN (Fidentity, 1) ATTRIBUTE_CONST;
extern EMACS_INT next_almost_prime (EMACS_INT) ATTRIBUTE_CONST;
extern Lisp_Object larger_vector (Lisp_Object, ptrdiff_t, ptrdiff_t);
extern void sweep_weak_hash_tables (void);
extern Lisp_Object Qcursor_in_echo_area;
extern Lisp_Object Qstring_lessp;
extern Lisp_Object QCsize, QCtest, QCweakness, Qequal, Qeq, Qeql;
EMACS_UINT hash_string (char const *, ptrdiff_t);
EMACS_UINT sxhash (Lisp_Object, int);
Lisp_Object make_hash_table (Lisp_Object, Lisp_Object, Lisp_Object,
                             Lisp_Object, Lisp_Object, Lisp_Object,
                             Lisp_Object);
ptrdiff_t hash_lookup (struct Lisp_Hash_Table *, Lisp_Object, EMACS_UINT *);
ptrdiff_t hash_put (struct Lisp_Hash_Table *, Lisp_Object, Lisp_Object,
		    EMACS_UINT);

extern Lisp_Object substring_both (Lisp_Object, ptrdiff_t, ptrdiff_t,
				   ptrdiff_t, ptrdiff_t);
extern Lisp_Object do_yes_or_no_p (Lisp_Object);
extern Lisp_Object concat2 (Lisp_Object, Lisp_Object);
extern Lisp_Object concat3 (Lisp_Object, Lisp_Object, Lisp_Object);
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
extern double extract_float (Lisp_Object);
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
extern Lisp_Object QCascent, QCmargin, QCrelief;
extern Lisp_Object QCconversion;
extern int x_bitmap_mask (struct frame *, ptrdiff_t);
extern void reset_image_types (void);
extern void syms_of_image (void);

/* Defined in insdel.c.  */
extern Lisp_Object Qinhibit_modification_hooks;
extern void move_gap (ptrdiff_t);
extern void move_gap_both (ptrdiff_t, ptrdiff_t);
extern _Noreturn void buffer_overflow (void);
extern void make_gap (ptrdiff_t);
extern ptrdiff_t copy_text (const unsigned char *, unsigned char *,
			    ptrdiff_t, bool, bool);
extern int count_combining_before (const unsigned char *,
				   ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern int count_combining_after (const unsigned char *,
				  ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern void insert (const char *, ptrdiff_t);
extern void insert_and_inherit (const char *, ptrdiff_t);
extern void insert_1 (const char *, ptrdiff_t, bool, bool, bool);
extern void insert_1_both (const char *, ptrdiff_t, ptrdiff_t,
			   bool, bool, bool);
extern void insert_from_gap (ptrdiff_t, ptrdiff_t);
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
extern void del_range_byte (ptrdiff_t, ptrdiff_t, bool);
extern void del_range_both (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object del_range_2 (ptrdiff_t, ptrdiff_t,
				ptrdiff_t, ptrdiff_t, bool);
extern void modify_region (struct buffer *, ptrdiff_t, ptrdiff_t, bool);
extern void prepare_to_modify_buffer (ptrdiff_t, ptrdiff_t, ptrdiff_t *);
extern void signal_after_change (ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern void adjust_after_insert (ptrdiff_t, ptrdiff_t, ptrdiff_t,
				 ptrdiff_t, ptrdiff_t);
extern void adjust_markers_for_delete (ptrdiff_t, ptrdiff_t,
				       ptrdiff_t, ptrdiff_t);
extern void replace_range (ptrdiff_t, ptrdiff_t, Lisp_Object, bool, bool, bool);
extern void replace_range_2 (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t,
			     const char *, ptrdiff_t, ptrdiff_t, bool);
extern void syms_of_insdel (void);

/* Defined in dispnew.c.  */
#if (defined PROFILING \
     && (defined __FreeBSD__ || defined GNU_LINUX || defined __MINGW32__))
_Noreturn void __executable_start (void);
#endif
extern Lisp_Object selected_frame;
extern Lisp_Object Vwindow_system;
extern Lisp_Object sit_for (Lisp_Object, bool, int);
extern void init_display (void);
extern void syms_of_display (void);

/* Defined in xdisp.c.  */
extern Lisp_Object Qinhibit_point_motion_hooks;
extern Lisp_Object Qinhibit_redisplay, Qdisplay;
extern Lisp_Object Qmenu_bar_update_hook;
extern Lisp_Object Qwindow_scroll_functions;
extern Lisp_Object Qoverriding_local_map, Qoverriding_terminal_local_map;
extern Lisp_Object Qimage, Qtext, Qboth, Qboth_horiz, Qtext_image_horiz;
extern Lisp_Object Qspace, Qcenter, QCalign_to;
extern Lisp_Object Qbar, Qhbar, Qbox, Qhollow;
extern Lisp_Object Qleft_margin, Qright_margin;
extern Lisp_Object Qglyphless_char;
extern Lisp_Object QCdata, QCfile;
extern Lisp_Object QCmap;
extern Lisp_Object Qrisky_local_variable;
extern struct frame *last_glyphless_glyph_frame;
extern int last_glyphless_glyph_face_id;
extern int last_glyphless_glyph_merged_face_id;
extern int noninteractive_need_newline;
extern Lisp_Object echo_area_buffer[2];
extern void add_to_log (const char *, Lisp_Object, Lisp_Object);
extern void check_message_stack (void);
extern void setup_echo_area_for_printing (int);
extern bool push_message (void);
extern Lisp_Object pop_message_unwind (Lisp_Object);
extern Lisp_Object restore_message_unwind (Lisp_Object);
extern void restore_message (void);
extern Lisp_Object current_message (void);
extern void clear_message (int, int);
extern void message (const char *, ...) ATTRIBUTE_FORMAT_PRINTF (1, 2);
extern void message1 (const char *);
extern void message1_nolog (const char *);
extern void message2 (const char *, ptrdiff_t, int);
extern void message2_nolog (const char *, ptrdiff_t, int);
extern void message3 (Lisp_Object, ptrdiff_t, int);
extern void message3_nolog (Lisp_Object, ptrdiff_t, int);
extern void message_dolog (const char *, ptrdiff_t, int, int);
extern void message_with_string (const char *, Lisp_Object, int);
extern void message_log_maybe_newline (void);
extern void update_echo_area (void);
extern void truncate_echo_area (ptrdiff_t);
extern void redisplay (void);
extern void redisplay_preserve_echo_area (int);
extern void prepare_menu_bars (void);

void set_frame_cursor_types (struct frame *, Lisp_Object);
extern void syms_of_xdisp (void);
extern void init_xdisp (void);
extern Lisp_Object safe_eval (Lisp_Object);
extern int pos_visible_p (struct window *, ptrdiff_t, int *,
                          int *, int *, int *, int *, int *);

/* Defined in xsettings.c.  */
extern void syms_of_xsettings (void);

/* Defined in vm-limit.c.  */
extern void memory_warnings (void *, void (*warnfun) (const char *));

/* Defined in alloc.c.  */
extern void check_pure_size (void);
extern void allocate_string_data (struct Lisp_String *, EMACS_INT, EMACS_INT);
extern void malloc_warning (const char *);
extern _Noreturn void memory_full (size_t);
extern _Noreturn void buffer_memory_full (ptrdiff_t);
extern bool survives_gc_p (Lisp_Object);
extern void mark_object (Lisp_Object);
#if defined REL_ALLOC && !defined SYSTEM_MALLOC
extern void refill_memory_reserve (void);
#endif
extern const char *pending_malloc_warning;
extern Lisp_Object zero_vector;
extern Lisp_Object *stack_base;
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
extern _Noreturn void string_overflow (void);
extern Lisp_Object make_string (const char *, ptrdiff_t);
extern Lisp_Object make_formatted_string (char *, const char *, ...)
  ATTRIBUTE_FORMAT_PRINTF (2, 3);
extern Lisp_Object make_unibyte_string (const char *, ptrdiff_t);

/* Make unibyte string from C string when the length isn't known.  */

LISP_INLINE Lisp_Object
build_unibyte_string (const char *str)
{
  return make_unibyte_string (str, strlen (str));
}

extern Lisp_Object make_multibyte_string (const char *, ptrdiff_t, ptrdiff_t);
extern Lisp_Object make_event_array (int, Lisp_Object *);
extern Lisp_Object make_uninit_string (EMACS_INT);
extern Lisp_Object make_uninit_multibyte_string (EMACS_INT, EMACS_INT);
extern Lisp_Object make_string_from_bytes (const char *, ptrdiff_t, ptrdiff_t);
extern Lisp_Object make_specified_string (const char *,
					  ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object make_pure_string (const char *, ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object make_pure_c_string (const char *, ptrdiff_t);

/* Make a string allocated in pure space, use STR as string data.  */

LISP_INLINE Lisp_Object
build_pure_c_string (const char *str)
{
  return make_pure_c_string (str, strlen (str));
}

/* Make a string from the data at STR, treating it as multibyte if the
   data warrants.  */

LISP_INLINE Lisp_Object
build_string (const char *str)
{
  return make_string (str, strlen (str));
}

extern Lisp_Object pure_cons (Lisp_Object, Lisp_Object);
extern void make_byte_code (struct Lisp_Vector *);
extern Lisp_Object Qautomatic_gc;
extern Lisp_Object Qchar_table_extra_slots;
extern struct Lisp_Vector *allocate_vector (EMACS_INT);
extern struct Lisp_Vector *allocate_pseudovector (int memlen, int lisplen, int tag);
#define ALLOCATE_PSEUDOVECTOR(typ,field,tag)				\
  ((typ*)								\
   allocate_pseudovector						\
       (VECSIZE (typ), PSEUDOVECSIZE (typ, field), tag))
extern struct Lisp_Hash_Table *allocate_hash_table (void);
extern struct window *allocate_window (void);
extern struct frame *allocate_frame (void);
extern struct Lisp_Process *allocate_process (void);
extern struct terminal *allocate_terminal (void);
extern bool gc_in_progress;
extern bool abort_on_gc;
extern Lisp_Object make_float (double);
extern void display_malloc_warning (void);
extern ptrdiff_t inhibit_garbage_collection (void);
extern Lisp_Object make_save_value (void *, ptrdiff_t);
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
#define check_cons_list() ((void) 0)
#endif

#ifdef REL_ALLOC
/* Defined in ralloc.c.  */
extern void *r_alloc (void **, size_t);
extern void r_alloc_free (void **);
extern void *r_re_alloc (void **, size_t);
extern void r_alloc_reset_variable (void **, void **);
extern void r_alloc_inhibit_buffer_relocation (int);
#endif

/* Defined in chartab.c.  */
extern Lisp_Object copy_char_table (Lisp_Object);
extern Lisp_Object char_table_ref (Lisp_Object, int);
extern Lisp_Object char_table_ref_and_range (Lisp_Object, int,
                                             int *, int *);
extern void char_table_set (Lisp_Object, int, Lisp_Object);
extern void char_table_set_range (Lisp_Object, int, int, Lisp_Object);
extern int char_table_translate (Lisp_Object, int);
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
extern Lisp_Object Qstandard_output;
extern Lisp_Object Qexternal_debugging_output;
extern void temp_output_buffer_setup (const char *);
extern int print_level;
extern Lisp_Object Qprint_escape_newlines;
extern void write_string (const char *, int);
extern void print_error_message (Lisp_Object, Lisp_Object, const char *,
				 Lisp_Object);
extern Lisp_Object internal_with_output_to_temp_buffer
        (const char *, Lisp_Object (*) (Lisp_Object), Lisp_Object);
enum FLOAT_TO_STRING_BUFSIZE { FLOAT_TO_STRING_BUFSIZE = 350 };
extern int float_to_string (char *, double);
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
extern Lisp_Object Qvariable_documentation, Qstandard_input;
extern Lisp_Object Qbackquote, Qcomma, Qcomma_at, Qcomma_dot, Qfunction;
extern Lisp_Object Qlexical_binding;
extern Lisp_Object check_obarray (Lisp_Object);
extern Lisp_Object intern_1 (const char *, ptrdiff_t);
extern Lisp_Object intern_c_string_1 (const char *, ptrdiff_t);
extern Lisp_Object oblookup (Lisp_Object, const char *, ptrdiff_t, ptrdiff_t);
#define LOADHIST_ATTACH(x) \
  do {									\
    if (initialized) Vcurrent_load_list = Fcons (x, Vcurrent_load_list); \
  } while (0)
extern int openp (Lisp_Object, Lisp_Object, Lisp_Object,
                  Lisp_Object *, Lisp_Object);
extern Lisp_Object string_to_number (char const *, int, bool);
extern void map_obarray (Lisp_Object, void (*) (Lisp_Object, Lisp_Object),
                         Lisp_Object);
extern void dir_warning (const char *, Lisp_Object);
extern void close_load_descs (void);
extern void init_obarray (void);
extern void init_lread (void);
extern void syms_of_lread (void);

LISP_INLINE Lisp_Object
intern (const char *str)
{
  return intern_1 (str, strlen (str));
}

LISP_INLINE Lisp_Object
intern_c_string (const char *str)
{
  return intern_c_string_1 (str, strlen (str));
}

/* Defined in eval.c.  */
extern Lisp_Object Qautoload, Qexit, Qinteractive, Qcommandp, Qmacro;
extern Lisp_Object Qinhibit_quit, Qinternal_interpreter_environment, Qclosure;
extern Lisp_Object Qand_rest;
extern Lisp_Object Vautoload_queue;
extern Lisp_Object Vsignaling_function;
extern Lisp_Object inhibit_lisp_code;
#if BYTE_MARK_STACK
extern struct catchtag *catchlist;
extern struct handler *handlerlist;
#endif
/* To run a normal hook, use the appropriate function from the list below.
   The calling convention:

   if (!NILP (Vrun_hooks))
     call1 (Vrun_hooks, Qmy_funny_hook);

   should no longer be used.  */
extern Lisp_Object Vrun_hooks;
extern void run_hook_with_args_2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object run_hook_with_args (ptrdiff_t nargs, Lisp_Object *args,
				       Lisp_Object (*funcall)
				       (ptrdiff_t nargs, Lisp_Object *args));
extern _Noreturn void xsignal (Lisp_Object, Lisp_Object);
extern _Noreturn void xsignal0 (Lisp_Object);
extern _Noreturn void xsignal1 (Lisp_Object, Lisp_Object);
extern _Noreturn void xsignal2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern _Noreturn void xsignal3 (Lisp_Object, Lisp_Object, Lisp_Object,
				Lisp_Object);
extern _Noreturn void signal_error (const char *, Lisp_Object);
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
extern Lisp_Object internal_catch (Lisp_Object, Lisp_Object (*) (Lisp_Object), Lisp_Object);
extern Lisp_Object internal_lisp_condition_case (Lisp_Object, Lisp_Object, Lisp_Object);
extern Lisp_Object internal_condition_case (Lisp_Object (*) (void), Lisp_Object, Lisp_Object (*) (Lisp_Object));
extern Lisp_Object internal_condition_case_1 (Lisp_Object (*) (Lisp_Object), Lisp_Object, Lisp_Object, Lisp_Object (*) (Lisp_Object));
extern Lisp_Object internal_condition_case_2 (Lisp_Object (*) (Lisp_Object, Lisp_Object), Lisp_Object, Lisp_Object, Lisp_Object, Lisp_Object (*) (Lisp_Object));
extern Lisp_Object internal_condition_case_n
    (Lisp_Object (*) (ptrdiff_t, Lisp_Object *), ptrdiff_t, Lisp_Object *,
     Lisp_Object, Lisp_Object (*) (Lisp_Object, ptrdiff_t, Lisp_Object *));
extern void specbind (Lisp_Object, Lisp_Object);
extern void record_unwind_protect (Lisp_Object (*) (Lisp_Object), Lisp_Object);
extern Lisp_Object unbind_to (ptrdiff_t, Lisp_Object);
extern _Noreturn void error (const char *, ...) ATTRIBUTE_FORMAT_PRINTF (1, 2);
extern _Noreturn void verror (const char *, va_list)
  ATTRIBUTE_FORMAT_PRINTF (1, 0);
extern Lisp_Object un_autoload (Lisp_Object);
extern Lisp_Object call_debugger (Lisp_Object arg);
extern void init_eval_once (void);
extern Lisp_Object safe_call (ptrdiff_t, Lisp_Object, ...);
extern Lisp_Object safe_call1 (Lisp_Object, Lisp_Object);
extern Lisp_Object safe_call2 (Lisp_Object, Lisp_Object, Lisp_Object);
extern void init_eval (void);
#if BYTE_MARK_STACK
extern void mark_backtrace (void);
#endif
extern void syms_of_eval (void);

/* Defined in editfns.c.  */
extern Lisp_Object Qfield;
extern void insert1 (Lisp_Object);
extern Lisp_Object format2 (const char *, Lisp_Object, Lisp_Object);
extern Lisp_Object save_excursion_save (void);
extern Lisp_Object save_restriction_save (void);
extern Lisp_Object save_excursion_restore (Lisp_Object);
extern Lisp_Object save_restriction_restore (Lisp_Object);
extern _Noreturn void time_overflow (void);
extern Lisp_Object make_buffer_string (ptrdiff_t, ptrdiff_t, bool);
extern Lisp_Object make_buffer_string_both (ptrdiff_t, ptrdiff_t, ptrdiff_t,
					    ptrdiff_t, bool);
extern void init_editfns (void);
extern void syms_of_editfns (void);
extern void set_time_zone_rule (const char *);

/* Defined in buffer.c.  */
extern bool mouse_face_overlay_overlaps (Lisp_Object);
extern _Noreturn void nsberror (Lisp_Object);
extern void adjust_overlays_for_insert (ptrdiff_t, ptrdiff_t);
extern void adjust_overlays_for_delete (ptrdiff_t, ptrdiff_t);
extern void fix_start_end_in_overlays (ptrdiff_t, ptrdiff_t);
extern void report_overlay_modification (Lisp_Object, Lisp_Object, bool,
                                         Lisp_Object, Lisp_Object, Lisp_Object);
extern bool overlay_touches_p (ptrdiff_t);
extern Lisp_Object Vbuffer_alist;
extern Lisp_Object set_buffer_if_live (Lisp_Object);
extern Lisp_Object other_buffer_safely (Lisp_Object);
extern Lisp_Object Qpriority, Qwindow, Qbefore_string, Qafter_string;
extern Lisp_Object get_truename_buffer (Lisp_Object);
extern void init_buffer_once (void);
extern void init_buffer (void);
extern void syms_of_buffer (void);
extern void keys_of_buffer (void);

/* Defined in marker.c.  */

extern ptrdiff_t marker_position (Lisp_Object);
extern ptrdiff_t marker_byte_position (Lisp_Object);
extern void clear_charpos_cache (struct buffer *);
extern ptrdiff_t charpos_to_bytepos (ptrdiff_t);
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

extern Lisp_Object Qfile_error;
extern Lisp_Object Qfile_exists_p;
extern Lisp_Object Qfile_directory_p;
extern Lisp_Object Qinsert_file_contents;
extern Lisp_Object Qfile_name_history;
extern Lisp_Object expand_and_dir_to_file (Lisp_Object, Lisp_Object);
EXFUN (Fread_file_name, 6);     /* Not a normal DEFUN.  */
extern Lisp_Object close_file_unwind (Lisp_Object);
extern Lisp_Object restore_point_unwind (Lisp_Object);
extern _Noreturn void report_file_error (const char *, Lisp_Object);
extern void internal_delete_file (Lisp_Object);
extern void syms_of_fileio (void);
extern Lisp_Object make_temp_name (Lisp_Object, bool);
extern Lisp_Object Qdelete_file;
extern bool check_existing (const char *);

/* Defined in search.c.  */
extern void shrink_regexp_cache (void);
extern void restore_search_regs (void);
extern void record_unwind_save_match_data (void);
struct re_registers;
extern struct re_pattern_buffer *compile_pattern (Lisp_Object,
						  struct re_registers *,
						  Lisp_Object, int, int);
extern ptrdiff_t fast_string_match (Lisp_Object, Lisp_Object);
extern ptrdiff_t fast_c_string_match_ignore_case (Lisp_Object, const char *,
						  ptrdiff_t);
extern ptrdiff_t fast_string_match_ignore_case (Lisp_Object, Lisp_Object);
extern ptrdiff_t fast_looking_at (Lisp_Object, ptrdiff_t, ptrdiff_t,
                                  ptrdiff_t, ptrdiff_t, Lisp_Object);
extern ptrdiff_t scan_buffer (int, ptrdiff_t, ptrdiff_t, ptrdiff_t,
			      ptrdiff_t *, bool);
extern EMACS_INT scan_newline (ptrdiff_t, ptrdiff_t, ptrdiff_t, ptrdiff_t,
			       EMACS_INT, bool);
extern ptrdiff_t find_next_newline (ptrdiff_t, int);
extern ptrdiff_t find_next_newline_no_quit (ptrdiff_t, ptrdiff_t);
extern ptrdiff_t find_before_next_newline (ptrdiff_t, ptrdiff_t, ptrdiff_t);
extern void syms_of_search (void);
extern void clear_regexp_cache (void);

/* Defined in minibuf.c.  */

extern Lisp_Object Qcompletion_ignore_case;
extern Lisp_Object Vminibuffer_list;
extern Lisp_Object last_minibuf_string;
extern Lisp_Object get_minibuffer (EMACS_INT);
extern void init_minibuf_once (void);
extern void syms_of_minibuf (void);

/* Defined in callint.c.  */

extern Lisp_Object Qminus, Qplus;
extern Lisp_Object Qwhen;
extern Lisp_Object Qcall_interactively, Qmouse_leave_buffer_hook;
extern void syms_of_callint (void);

/* Defined in casefiddle.c.  */

extern Lisp_Object Qidentity;
extern void syms_of_casefiddle (void);
extern void keys_of_casefiddle (void);

/* Defined in casetab.c.  */

extern void init_casetab_once (void);
extern void syms_of_casetab (void);

/* Defined in keyboard.c.  */

extern Lisp_Object echo_message_buffer;
extern struct kboard *echo_kboard;
extern void cancel_echoing (void);
extern Lisp_Object Qdisabled, QCfilter;
extern Lisp_Object Qup, Qdown, Qbottom;
extern Lisp_Object Qtop;
extern Lisp_Object last_undo_boundary;
extern bool input_pending;
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
extern Lisp_Object Qonly, Qnone;
extern Lisp_Object Qvisible;
extern void store_frame_param (struct frame *, Lisp_Object, Lisp_Object);
extern void store_in_alist (Lisp_Object *, Lisp_Object, Lisp_Object);
extern Lisp_Object do_switch_frame (Lisp_Object, int, int, Lisp_Object);
#if HAVE_NS
extern Lisp_Object get_frame_param (struct frame *, Lisp_Object);
#endif
extern Lisp_Object frame_buffer_predicate (Lisp_Object);
extern void frames_discard_buffer (Lisp_Object);
extern void syms_of_frame (void);

/* Defined in emacs.c.  */
extern char **initial_argv;
extern int initial_argc;
#if defined (HAVE_X_WINDOWS) || defined (HAVE_NS)
extern bool display_arg;
#endif
extern Lisp_Object decode_env_path (const char *, const char *);
extern Lisp_Object empty_unibyte_string, empty_multibyte_string;
extern Lisp_Object Qfile_name_handler_alist;
extern _Noreturn void terminate_due_to_signal (int, int);
extern Lisp_Object Qkill_emacs;
#ifdef WINDOWSNT
extern Lisp_Object Vlibrary_cache;
#endif
#if HAVE_SETLOCALE
void fixup_locale (void);
void synchronize_system_messages_locale (void);
void synchronize_system_time_locale (void);
#else
#define setlocale(category, locale)
#define fixup_locale()
#define synchronize_system_messages_locale()
#define synchronize_system_time_locale()
#endif
extern void shut_down_emacs (int, Lisp_Object);

/* True means don't do interactive redisplay and don't change tty modes.  */
extern bool noninteractive;

/* True means remove site-lisp directories from load-path.  */
extern bool no_site_lisp;

/* Pipe used to send exit notification to the daemon parent at
   startup.  */
extern int daemon_pipe[2];
#define IS_DAEMON (daemon_pipe[1] != 0)

/* True if handling a fatal error already.  */
extern bool fatal_error_in_progress;

/* True means don't do use window-system-specific display code.  */
extern bool inhibit_window_system;
/* True means that a filter or a sentinel is running.  */
extern bool running_asynch_code;

/* Defined in process.c.  */
extern Lisp_Object QCtype, Qlocal;
extern Lisp_Object Qprocessp;
extern void kill_buffer_processes (Lisp_Object);
extern int wait_reading_process_output (intmax_t, int, int, bool,
                                        Lisp_Object,
                                        struct Lisp_Process *,
                                        int);
/* Max value for the first argument of wait_reading_process_output.  */
#if __GNUC__ == 3 || (__GNUC__ == 4 && __GNUC_MINOR__ <= 5)
/* Work around a bug in GCC 3.4.2, known to be fixed in GCC 4.6.3.
   The bug merely causes a bogus warning, but the warning is annoying.  */
# define WAIT_READING_MAX min (TYPE_MAXIMUM (time_t), INTMAX_MAX)
#else
# define WAIT_READING_MAX INTMAX_MAX
#endif
extern void add_keyboard_wait_descriptor (int);
extern void delete_keyboard_wait_descriptor (int);
#ifdef HAVE_GPM
extern void add_gpm_wait_descriptor (int);
extern void delete_gpm_wait_descriptor (int);
#endif
extern void close_process_descs (void);
extern void init_process_emacs (void);
extern void syms_of_process (void);
extern void setup_process_coding_systems (Lisp_Object);

#ifndef DOS_NT
 _Noreturn
#endif
extern int child_setup (int, int, int, char **, bool, Lisp_Object);
extern void init_callproc_1 (void);
extern void init_callproc (void);
extern void set_initial_environment (void);
extern void syms_of_callproc (void);

/* Defined in doc.c.  */
extern Lisp_Object Qfunction_documentation;
extern Lisp_Object read_doc_string (Lisp_Object);
extern Lisp_Object get_doc_string (Lisp_Object, bool, bool);
extern void syms_of_doc (void);
extern int read_bytecode_char (bool);

/* Defined in bytecode.c.  */
extern Lisp_Object Qbytecode;
extern void syms_of_bytecode (void);
extern struct byte_stack *byte_stack_list;
#if BYTE_MARK_STACK
extern void mark_byte_stack (void);
#endif
extern void unmark_byte_stack (void);
extern Lisp_Object exec_byte_code (Lisp_Object, Lisp_Object, Lisp_Object,
				   Lisp_Object, ptrdiff_t, Lisp_Object *);

/* Defined in macros.c.  */
extern Lisp_Object Qexecute_kbd_macro;
extern void init_macros (void);
extern void syms_of_macros (void);

/* Defined in undo.c.  */
extern Lisp_Object Qapply;
extern Lisp_Object Qinhibit_read_only;
extern void truncate_undo_list (struct buffer *);
extern void record_marker_adjustment (Lisp_Object, ptrdiff_t);
extern void record_insert (ptrdiff_t, ptrdiff_t);
extern void record_delete (ptrdiff_t, Lisp_Object);
extern void record_first_change (void);
extern void record_change (ptrdiff_t, ptrdiff_t);
extern void record_property_change (ptrdiff_t, ptrdiff_t,
				    Lisp_Object, Lisp_Object,
                                    Lisp_Object);
extern void syms_of_undo (void);
/* Defined in textprop.c.  */
extern Lisp_Object Qfont, Qmouse_face;
extern Lisp_Object Qinsert_in_front_hooks, Qinsert_behind_hooks;
extern Lisp_Object Qfront_sticky, Qrear_nonsticky;
extern Lisp_Object Qminibuffer_prompt;

extern void report_interval_modification (Lisp_Object, Lisp_Object);

/* Defined in menu.c.  */
extern void syms_of_menu (void);

/* Defined in xmenu.c.  */
extern void syms_of_xmenu (void);

/* Defined in termchar.h.  */
struct tty_display_info;

/* Defined in termhooks.h.  */
struct terminal;

/* Defined in sysdep.c.  */
#ifndef HAVE_GET_CURRENT_DIR_NAME
extern char *get_current_dir_name (void);
#endif
extern void stuff_char (char c);
extern void init_foreground_group (void);
extern void init_sigio (int);
extern void sys_subshell (void);
extern void sys_suspend (void);
extern void discard_tty_input (void);
extern void init_sys_modes (struct tty_display_info *);
extern void reset_sys_modes (struct tty_display_info *);
extern void init_all_sys_modes (void);
extern void reset_all_sys_modes (void);
extern void flush_pending_output (int) ATTRIBUTE_CONST;
extern void child_setup_tty (int);
extern void setup_pty (int);
extern int set_window_size (int, int, int);
extern EMACS_INT get_random (void);
extern void seed_random (void *, ptrdiff_t);
extern void init_random (void);
extern void emacs_backtrace (int);
extern _Noreturn void emacs_abort (void) NO_INLINE;
extern int emacs_open (const char *, int, int);
extern int emacs_close (int);
extern ptrdiff_t emacs_read (int, char *, ptrdiff_t);
extern ptrdiff_t emacs_write (int, const char *, ptrdiff_t);
enum { READLINK_BUFSIZE = 1024 };
extern char *emacs_readlink (const char *, char [READLINK_BUFSIZE]);

extern void unlock_all_files (void);
extern void lock_file (Lisp_Object);
extern void unlock_file (Lisp_Object);
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

/* Defined in xfns.c, w32fns.c, or macfns.c.  */
extern Lisp_Object Qfont_param;
#endif

/* Defined in xfaces.c.  */
extern Lisp_Object Qdefault, Qtool_bar, Qfringe;
extern Lisp_Object Qheader_line, Qscroll_bar, Qcursor;
extern Lisp_Object Qmode_line_inactive;
extern Lisp_Object Qface;
extern Lisp_Object Qnormal;
extern Lisp_Object QCfamily, QCweight, QCslant;
extern Lisp_Object QCheight, QCname, QCwidth, QCforeground, QCbackground;
extern Lisp_Object Qextra_light, Qlight, Qsemi_light, Qsemi_bold;
extern Lisp_Object Qbold, Qextra_bold, Qultra_bold;
extern Lisp_Object Qoblique, Qitalic;
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
extern void syms_of_xterm (void);
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_WINDOW_SYSTEM
/* Defined in xterm.c, nsterm.m, w32term.c.  */
extern char *x_get_keysym_name (int);
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_LIBXML2
/* Defined in xml.c.  */
extern void syms_of_xml (void);
extern void xml_cleanup_parser (void);
#endif

#ifdef HAVE_MENUS
/* Defined in (x|w32)fns.c, nsfns.m...  */
extern int have_menus_p (void);
#endif

#ifdef HAVE_DBUS
/* Defined in dbusbind.c.  */
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

/* True means Emacs has already been initialized.
   Used during startup to detect startup of dumped Emacs.  */
extern bool initialized;

/* True means ^G can quit instantly.  */
extern bool immediate_quit;

extern void *xmalloc (size_t);
extern void *xzalloc (size_t);
extern void *xrealloc (void *, size_t);
extern void xfree (void *);
extern void *xnmalloc (ptrdiff_t, ptrdiff_t);
extern void *xnrealloc (void *, ptrdiff_t, ptrdiff_t);
extern void *xpalloc (void *, ptrdiff_t *, ptrdiff_t, ptrdiff_t, ptrdiff_t);

extern char *xstrdup (const char *);

extern char *egetenv (const char *);

/* Set up the name of the machine we're running on.  */
extern void init_system_name (void);

/* We used to use `abs', but that clashes with system headers on some
   platforms, and using a name reserved by Standard C is a bad idea
   anyway.  */
#if !defined (eabs)
#define eabs(x)         ((x) < 0 ? -(x) : (x))
#endif

/* Return a fixnum or float, depending on whether VAL fits in a Lisp
   fixnum.  */

#define make_fixnum_or_float(val) \
   (FIXNUM_OVERFLOW_P (val) ? make_float (val) : make_number (val))

/* SAFE_ALLOCA normally allocates memory on the stack, but if size is
   larger than MAX_ALLOCA, use xmalloc to avoid overflowing the stack.  */

enum MAX_ALLOCA { MAX_ALLOCA = 16 * 1024 };

extern Lisp_Object safe_alloca_unwind (Lisp_Object);
extern void *record_xmalloc (size_t);

#define USE_SAFE_ALLOCA			\
  ptrdiff_t sa_count = SPECPDL_INDEX (); bool sa_must_free = 0

/* SAFE_ALLOCA allocates a simple buffer.  */

#define SAFE_ALLOCA(size) ((size) < MAX_ALLOCA	\
			   ? alloca (size)	\
			   : (sa_must_free = 1, record_xmalloc (size)))

/* SAFE_NALLOCA sets BUF to a newly allocated array of MULTIPLIER *
   NITEMS items, each of the same type as *BUF.  MULTIPLIER must
   positive.  The code is tuned for MULTIPLIER being a constant.  */

#define SAFE_NALLOCA(buf, multiplier, nitems)			\
  do {								\
    if ((nitems) <= MAX_ALLOCA / sizeof *(buf) / (multiplier))	\
      (buf) = alloca (sizeof *(buf) * (multiplier) * (nitems));	\
    else							\
      {								 \
	(buf) = xnmalloc (nitems, sizeof *(buf) * (multiplier)); \
	sa_must_free = 1;					 \
	record_unwind_protect (safe_alloca_unwind,		 \
			       make_save_value (buf, 0));	 \
      }								 \
  } while (0)

/* SAFE_FREE frees xmalloced memory and enables GC as needed.  */

#define SAFE_FREE()			\
  do {					\
    if (sa_must_free) {			\
      sa_must_free = 0;			\
      unbind_to (sa_count, Qnil);	\
    }					\
  } while (0)


/* SAFE_ALLOCA_LISP allocates an array of Lisp_Objects.  */

#define SAFE_ALLOCA_LISP(buf, nelt)			       \
  do {							       \
    if ((nelt) < MAX_ALLOCA / word_size)		       \
      buf = alloca ((nelt) * word_size);		       \
    else if ((nelt) < min (PTRDIFF_MAX, SIZE_MAX) / word_size) \
      {							       \
	Lisp_Object arg_;				       \
	buf = xmalloc ((nelt) * word_size);		       \
	arg_ = make_save_value (buf, nelt);		       \
	XSAVE_VALUE (arg_)->dogc = 1;			       \
	sa_must_free = 1;				       \
	record_unwind_protect (safe_alloca_unwind, arg_);      \
      }							       \
    else						       \
      memory_full (SIZE_MAX);				       \
  } while (0)


#include "globals.h"

/* Check whether it's time for GC, and run it if so.  */

LISP_INLINE void
maybe_gc (void)
{
  if ((consing_since_gc > gc_cons_threshold
       && consing_since_gc > gc_relative_threshold)
      || (!NILP (Vmemory_full)
	  && consing_since_gc > memory_full_cons_threshold))
    Fgarbage_collect ();
}

LISP_INLINE int
functionp (Lisp_Object object)
{
  if (SYMBOLP (object) && !NILP (Ffboundp (object)))
    {
      object = Findirect_function (object, Qt);

      if (CONSP (object) && EQ (XCAR (object), Qautoload))
	{
	  /* Autoloaded symbols are functions, except if they load
	     macros or keymaps.  */
	  int i;
	  for (i = 0; i < 4 && CONSP (object); i++)
	    object = XCDR (object);

	  return ! (CONSP (object) && !NILP (XCAR (object)));
	}
    }

  if (SUBRP (object))
    return XSUBR (object)->max_args != UNEVALLED;
  else if (COMPILEDP (object))
    return 1;
  else if (CONSP (object))
    {
      Lisp_Object car = XCAR (object);
      return EQ (car, Qlambda) || EQ (car, Qclosure);
    }
  else
    return 0;
}

INLINE_HEADER_END

#endif /* EMACS_LISP_H */
