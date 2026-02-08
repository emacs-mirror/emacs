/* Storage allocation and gc for GNU Emacs Lisp interpreter.

Copyright (C) 1985-2026 Free Software Foundation, Inc.

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

#include <config.h>

#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>		/* For CHAR_BIT.  */
#include <signal.h>		/* For SIGABRT, SIGDANGER.  */

#ifdef HAVE_PTHREAD
#include <pthread.h>
#endif

#include "lisp.h"
#include "bignum.h"
#include "dispextern.h"
#include "intervals.h"
#include "sysstdio.h"
#include "systime.h"
#include "character.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "pdumper.h"
#include "termhooks.h"		/* For struct terminal.  */
#include "itree.h"
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#if defined HAVE_ANDROID && !defined ANDROID_STUBIFY
#include "sfntfont.h"
#endif

#ifdef HAVE_TREE_SITTER
#include "treesit.h"
#endif

#include <flexmember.h>
#include <verify.h>
#include <execinfo.h>           /* For backtrace.  */

#ifdef HAVE_LINUX_SYSINFO
#include <sys/sysinfo.h>
#endif

#ifdef MSDOS
#include "dosfns.h"		/* For dos_memory_info.  */
#endif

#if (defined ENABLE_CHECKING \
     && defined HAVE_VALGRIND_VALGRIND_H && !defined USE_VALGRIND)
# define USE_VALGRIND 1
#endif

#if USE_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#endif

/* AddressSanitizer exposes additional functions for manually marking
   memory as poisoned/unpoisoned.  When ASan is enabled and the needed
   header is available, memory is poisoned when:

   * An ablock is freed (lisp_align_free), or ablocks are initially
   allocated (lisp_align_malloc).
   * An interval_block is initially allocated (make_interval).
   * A dead INTERVAL is put on the interval free list
   (sweep_intervals).
   * A sdata is marked as dead (sweep_strings, pin_string).
   * An sblock is initially allocated (allocate_string_data).
   * A string_block is initially allocated (allocate_string).
   * A dead string is put on string_free_list (sweep_strings).
   * A float_block is initially allocated (make_float).
   * A dead float is put on float_free_list.
   * A cons_block is initially allocated (Fcons).
   * A dead cons is put on cons_free_list (sweep_cons).
   * A dead vector is put on vector_free_list (setup_on_free_list),
   or a new vector block is allocated (allocate_vector_from_block).
   Accordingly, objects reused from the free list are unpoisoned.

   This feature can be disabled with the run-time flag
   `allow_user_poisoning' set to zero.  */
#if ADDRESS_SANITIZER && defined HAVE_SANITIZER_ASAN_INTERFACE_H \
  && !defined GC_ASAN_POISON_OBJECTS
# define GC_ASAN_POISON_OBJECTS 1
# include <sanitizer/asan_interface.h>
#else
# define GC_ASAN_POISON_OBJECTS 0
#endif

/* GC_CHECK_MARKED_OBJECTS means do sanity checks on allocated objects.
   We turn that on by default when ENABLE_CHECKING is defined;
   define GC_CHECK_MARKED_OBJECTS to zero to disable.  */

#if defined ENABLE_CHECKING && !defined GC_CHECK_MARKED_OBJECTS
# define GC_CHECK_MARKED_OBJECTS 1
#endif
#ifndef GC_CHECK_MARKED_OBJECTS
# define GC_CHECK_MARKED_OBJECTS 0
#endif

/* GC_MALLOC_CHECK defined means perform validity checks of malloc'd
   memory.  Can do this only if using gmalloc.c and if not checking
   marked objects.  */

#if (defined SYSTEM_MALLOC || defined DOUG_LEA_MALLOC \
     || GC_CHECK_MARKED_OBJECTS)
#undef GC_MALLOC_CHECK
#endif

#include <unistd.h>
#include <fcntl.h>

#ifdef USE_GTK
# include "gtkutil.h"
#endif
#ifdef WINDOWSNT
#include "w32.h"
#include "w32heap.h"	/* for sbrk */
#endif

/* A type with alignment at least as large as any object that Emacs
   allocates.  This is not max_align_t because some platforms (e.g.,
   mingw) have buggy malloc implementations that do not align for
   max_align_t.  This union contains types of all GCALIGNED_STRUCT
   components visible here.  */
union emacs_align_type
{
  struct frame frame;
  struct Lisp_Bignum Lisp_Bignum;
  struct Lisp_CondVar Lisp_CondVar;
  struct Lisp_Finalizer Lisp_Finalizer;
  struct Lisp_Float Lisp_Float;
  struct Lisp_Hash_Table Lisp_Hash_Table;
  struct Lisp_Marker Lisp_Marker;
  struct Lisp_Misc_Ptr Lisp_Misc_Ptr;
  struct Lisp_Mutex Lisp_Mutex;
  struct Lisp_Overlay Lisp_Overlay;
  struct Lisp_Subr Lisp_Subr;
  struct Lisp_Sqlite Lisp_Sqlite;
  struct Lisp_User_Ptr Lisp_User_Ptr;
  struct terminal terminal;
  struct thread_state thread_state;
  struct window window;

  /* Omit the following since they would require including process.h
     etc, or because they are defined with flexible array members, which
     are rejected by some C99 compilers when this union subsequently
     appears in an `alignof' expression.  In practice their alignments
     never exceed that of the structs already listed.  */
#if 0
  struct Lisp_Bool_Vector Lisp_Bool_Vector;
  struct Lisp_Char_Table Lisp_Char_Table;
  struct Lisp_Sub_Char_Table Lisp_Sub_Char_Table;
  struct Lisp_Module_Function Lisp_Module_Function;
  struct Lisp_Process Lisp_Process;
  struct Lisp_Vector Lisp_Vector;
  struct save_window_data save_window_data;
  struct scroll_bar scroll_bar;
  struct xwidget_view xwidget_view;
  struct xwidget xwidget;
#endif
};

/* MALLOC_SIZE_NEAR (N) is a good number to pass to malloc when
   allocating a block of memory with size close to N bytes.
   For best results N should be a power of 2.

   When calculating how much memory to allocate, GNU malloc (SIZE)
   adds sizeof (size_t) to SIZE for internal overhead, and then rounds
   up to a multiple of MALLOC_ALIGNMENT.  Emacs can improve
   performance a bit on GNU platforms by arranging for the resulting
   size to be a power of two.  This heuristic is good for glibc 2.26
   (2017) and later, and does not affect correctness on other
   platforms.  */

#define MALLOC_SIZE_NEAR(n) \
  (ROUNDUP (max (n, sizeof (size_t)), MALLOC_ALIGNMENT) - sizeof (size_t))
#ifdef __i386
enum { MALLOC_ALIGNMENT = 16 };
#else
enum { MALLOC_ALIGNMENT = max (2 * sizeof (size_t), alignof (long double)) };
#endif

#ifdef DOUG_LEA_MALLOC

/* Specify maximum number of areas to mmap.  It would be nice to use a
   value that explicitly means "no limit".  */

# define MMAP_MAX_AREAS 100000000

/* Restore the dumped malloc state.  Because malloc can be invoked
   even before main (e.g. by the dynamic linker), the dumped malloc
   state must be restored as early as possible using this special hook.  */
static void
malloc_initialize_hook (void)
{
  static bool malloc_using_checking;

  if (! initialized)
    {
      malloc_using_checking = getenv ("MALLOC_CHECK_") != NULL;
    }
  else
    {
      if (!malloc_using_checking)
	{
	  /* Work around a bug in glibc's malloc.  MALLOC_CHECK_ must be
	     ignored if the heap to be restored was constructed without
	     malloc checking.  Can't use unsetenv, since that calls malloc.  */
	  char **p = environ;
	  if (p)
	    for (; *p; p++)
	      if (strncmp (*p, "MALLOC_CHECK_=", 14) == 0)
		{
		  do
		    *p = p[1];
		  while (*++p);

		  break;
		}
	}
    }
}

/* Declare the malloc initialization hook, which runs before 'main' starts.
   EXTERNALLY_VISIBLE works around Bug#22522.  */
typedef void (*voidfuncptr) (void);
# ifndef __MALLOC_HOOK_VOLATILE
#  define __MALLOC_HOOK_VOLATILE
# endif
voidfuncptr __MALLOC_HOOK_VOLATILE __malloc_initialize_hook EXTERNALLY_VISIBLE
  = malloc_initialize_hook;

#endif

/* Mark, unmark, query mark bit of a Lisp string.  S must be a pointer
   to a struct Lisp_String.  */

#define XMARK_STRING(S)		((S)->u.s.size |= ARRAY_MARK_FLAG)
#define XUNMARK_STRING(S)	((S)->u.s.size &= ~ARRAY_MARK_FLAG)
#define XSTRING_MARKED_P(S)	(((S)->u.s.size & ARRAY_MARK_FLAG) != 0)

#define XMARK_VECTOR(V)		((V)->header.size |= ARRAY_MARK_FLAG)
#define XUNMARK_VECTOR(V)	((V)->header.size &= ~ARRAY_MARK_FLAG)
#define XVECTOR_MARKED_P(V)	(((V)->header.size & ARRAY_MARK_FLAG) != 0)

/* Default value of gc_cons_threshold (see below).  */

#define GC_DEFAULT_THRESHOLD (100000 * word_size)

/* Global variables.  */
struct emacs_globals globals;

/* maybe_gc collects garbage if this goes negative.  */

EMACS_INT consing_until_gc;

#ifdef HAVE_PDUMPER
/* Number of finalizers run: used to loop over GC until we stop
   generating garbage.  */
int number_finalizers_run;
#endif

/* True during GC.  */

bool gc_in_progress;

/* System byte and object counts reported by GC.  */

/* Assume byte counts fit in uintptr_t and object counts fit into
   intptr_t.  */
typedef uintptr_t byte_ct;
typedef intptr_t object_ct;

/* Large-magnitude value for a threshold count, which fits in EMACS_INT.
   Using only half the EMACS_INT range avoids overflow hassles.
   There is no need to fit these counts into fixnums.  */
#define HI_THRESHOLD (EMACS_INT_MAX / 2)

/* Number of live and free conses etc. counted by the most-recent GC.  */

static struct gcstat
{
  object_ct total_conses, total_free_conses;
  object_ct total_symbols, total_free_symbols;
  object_ct total_strings, total_free_strings;
  byte_ct total_string_bytes;
  object_ct total_vectors, total_vector_slots, total_free_vector_slots;
  object_ct total_floats, total_free_floats;
  object_ct total_intervals, total_free_intervals;
  object_ct total_buffers;

  /* Size of the ancillary arrays of live hash-table and obarray objects.
     The objects themselves are not included (counted as vectors above).  */
  byte_ct total_hash_table_bytes;
} gcstat;

/* Total size of ancillary arrays of all allocated hash-table and obarray
   objects, both dead and alive.  This number is always kept up-to-date.  */
static ptrdiff_t hash_table_allocated_bytes = 0;

/* Points to memory space allocated as "spare", to be freed if we run
   out of memory.  We keep one large block, four cons-blocks, and
   two string blocks.  */

static char *spare_memory[7];

/* Amount of spare memory to keep in large reserve block, or to see
   whether this much is available when malloc fails on a larger request.  */

#define SPARE_MEMORY (1 << 14)

/* If positive, garbage collection is inhibited.  Otherwise, zero.  */

intptr_t garbage_collection_inhibited;

/* The GC threshold in bytes, the last time it was calculated
   from gc-cons-threshold and gc-cons-percentage.  */
static EMACS_INT gc_threshold;

/* If nonzero, this is a warning delivered by malloc and not yet
   displayed.  */

const char *pending_malloc_warning;

/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 16000
#endif

/* Buffer in which we save a copy of the C stack at each GC.  */

#if MAX_SAVE_STACK > 0
static char *stack_copy;
static ptrdiff_t stack_copy_size;

/* Copy to DEST a block of memory from SRC of size SIZE bytes,
   avoiding any address sanitization.  */

static void * ATTRIBUTE_NO_SANITIZE_ADDRESS
no_sanitize_memcpy (void *dest, void const *src, size_t size)
{
  if (! ADDRESS_SANITIZER)
    return memcpy (dest, src, size);
  else
    {
      size_t i;
      char *d = dest;
      char const *s = src;
      for (i = 0; i < size; i++)
	d[i] = s[i];
      return dest;
    }
}

#endif /* MAX_SAVE_STACK > 0 */

static struct Lisp_Vector *allocate_clear_vector (ptrdiff_t, bool);
static void unchain_finalizer (struct Lisp_Finalizer *);
static void mark_terminals (void);
static void gc_sweep (void);
static void mark_buffer (struct buffer *);

#if !defined REL_ALLOC || defined SYSTEM_MALLOC
static void refill_memory_reserve (void);
#endif
static void compact_small_strings (void);
static void free_large_strings (void);
extern Lisp_Object which_symbols (Lisp_Object, EMACS_INT) EXTERNALLY_VISIBLE;

static bool vector_marked_p (struct Lisp_Vector const *);
static bool vectorlike_marked_p (union vectorlike_header const *);
static void set_vectorlike_marked (union vectorlike_header *);
static bool interval_marked_p (INTERVAL);
static void set_interval_marked (INTERVAL);

/* When scanning the C stack for live Lisp objects, Emacs keeps track of
   what memory allocated via lisp_malloc and lisp_align_malloc is intended
   for what purpose.  This enumeration specifies the type of memory.  */

enum mem_type
{
  MEM_TYPE_NON_LISP,
  MEM_TYPE_CONS,
  MEM_TYPE_STRING,
  MEM_TYPE_SYMBOL,
  MEM_TYPE_FLOAT,
  /* Since all non-bool pseudovectors are small enough to be
     allocated from vector blocks, this memory type denotes
     large regular vectors and large bool pseudovectors.  */
  MEM_TYPE_VECTORLIKE,
  /* Special type to denote vector blocks.  */
  MEM_TYPE_VECTOR_BLOCK,
  /* Special type to denote reserved memory.  */
  MEM_TYPE_SPARE
};

static bool
deadp (Lisp_Object x)
{
  return BASE_EQ (x, dead_object ());
}

#ifdef GC_MALLOC_CHECK

enum mem_type allocated_mem_type;

#endif /* GC_MALLOC_CHECK */

/* A node in the red-black tree describing allocated memory containing
   Lisp data.  Each such block is recorded with its start and end
   address when it is allocated, and removed from the tree when it
   is freed.

   A red-black tree is a balanced binary tree with the following
   properties:

   1. Every node is either red or black.
   2. Every leaf is black.
   3. If a node is red, then both of its children are black.
   4. Every simple path from a node to a descendant leaf contains
   the same number of black nodes.
   5. The root is always black.

   When nodes are inserted into the tree, or deleted from the tree,
   the tree is "fixed" so that these properties are always true.

   A red-black tree with N internal nodes has height at most 2
   log(N+1).  Searches, insertions and deletions are done in O(log N).
   Please see a text book about data structures for a detailed
   description of red-black trees.  Any book worth its salt should
   describe them.  */

struct mem_node
{
  /* Children of this node.  These pointers are never NULL.  When there
     is no child, the value is MEM_NIL, which points to a dummy node.  */
  struct mem_node *left, *right;

  /* The parent of this node.  In the root node, this is NULL.  */
  struct mem_node *parent;

  /* Start and end of allocated region.  */
  void *start, *end;

  /* Node color.  */
  enum {MEM_BLACK, MEM_RED} color;

  /* Memory type.  */
  enum mem_type type;
};

/* Root of the tree describing allocated Lisp memory.  */

static struct mem_node *mem_root;

/* Lowest and highest known address in the heap.  */

static void *min_heap_address, *max_heap_address;

/* Sentinel node of the tree.  */

static struct mem_node mem_z;
#define MEM_NIL &mem_z

static struct mem_node *mem_insert (void *, void *, enum mem_type);
static void mem_insert_fixup (struct mem_node *);
static void mem_rotate_left (struct mem_node *);
static void mem_rotate_right (struct mem_node *);
static void mem_delete (struct mem_node *);
static void mem_delete_fixup (struct mem_node *);
static struct mem_node *mem_find (void *);

/* Addresses of staticpro'd variables.  */

Lisp_Object const *staticvec[NSTATICS];

/* Index of next unused slot in staticvec.  */

int staticidx;

/* Extract the pointer hidden within O.  */

static ATTRIBUTE_NO_SANITIZE_UNDEFINED void *
XPNTR (Lisp_Object a)
{
  return (BARE_SYMBOL_P (a)
	  ? (char *) lispsym + (XLI (a) - LISP_WORD_TAG (Lisp_Symbol))
	  : (char *) XLP (a) - (XLI (a) & ~VALMASK));
}

static void
XFLOAT_INIT (Lisp_Object f, double n)
{
  XFLOAT (f)->u.data = n;
}

/* Account for allocation of NBYTES in the heap.  This is a separate
   function to avoid hassles with implementation-defined conversion
   from unsigned to signed types.  */
static void
tally_consing (ptrdiff_t nbytes)
{
  consing_until_gc -= nbytes;
}

#ifdef DOUG_LEA_MALLOC
static bool
pointers_fit_in_lispobj_p (void)
{
  return (UINTPTR_MAX <= VAL_MAX) || USE_LSB_TAG;
}

static bool
mmap_lisp_allowed_p (void)
{
  /* If we can't store all memory addresses in our lisp objects, it's
     risky to let the heap use mmap and give us addresses from all
     over our address space.  */
  return pointers_fit_in_lispobj_p ();
}
#endif

/* Head of a circularly-linked list of extant finalizers. */
struct Lisp_Finalizer finalizers;

/* Head of a circularly-linked list of finalizers that must be invoked
   because we deemed them unreachable.  This list must be global, and
   not a local inside garbage_collect, in case we GC again while
   running finalizers.  */
struct Lisp_Finalizer doomed_finalizers;


/************************************************************************
				Malloc
 ************************************************************************/

#if defined SIGDANGER || (!defined SYSTEM_MALLOC)

/* Function malloc calls this if it finds we are near exhausting storage.  */

void
malloc_warning (const char *str)
{
  pending_malloc_warning = str;
}

#endif

/* Display an already-pending malloc warning.  */

void
display_malloc_warning (void)
{
  calln (Qdisplay_warning,
	 Qalloc,
	 build_string (pending_malloc_warning),
	 QCemergency);
  pending_malloc_warning = 0;
}

/* Called if we can't allocate relocatable space for a buffer.  */

void
buffer_memory_full (ptrdiff_t nbytes)
{
  /* If buffers use the relocating allocator, no need to free
     spare_memory, because we may have plenty of malloc space left
     that we could get, and if we don't, the malloc that fails will
     itself cause spare_memory to be freed.  If buffers don't use the
     relocating allocator, treat this like any other failing
     malloc.  */

#ifndef REL_ALLOC
  memory_full (nbytes);
#else
  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  xsignal (Qnil, Vmemory_signal_data);
#endif
}

/* A common multiple of the positive integers A and B.  Ideally this
   would be the least common multiple, but there's no way to do that
   as a constant expression in C, so do the best that we can easily do.  */
#define COMMON_MULTIPLE(a, b) \
  ((a) % (b) == 0 ? (a) : (b) % (a) == 0 ? (b) : (a) * (b))

/* Alignment needed for memory blocks managed by the garbage collector.  */
enum { LISP_ALIGNMENT = alignof (union { union emacs_align_type x;
					 GCALIGNED_UNION_MEMBER }) };
static_assert (LISP_ALIGNMENT % GCALIGNMENT == 0);

/* Emacs assumes that malloc (N) returns storage suitably aligned for
   any Lisp object whenever N is a multiple of LISP_ALIGNMENT.
   This Emacs assumption holds for current Emacs porting targets.

   On all current Emacs porting targets, it also happens that
   alignof (max_align_t) is a multiple of LISP_ALIGNMENT.
   Check this with a static_assert.  If the static_assert fails on an
   unusual platform, Emacs may well not work, so inspect this module's
   source code carefully with the unusual platform's quirks in mind.

   In practice the static_assert works even for buggy platforms where
   malloc can yield an unaligned address if given a large but unaligned
   size; Emacs avoids the bug because it aligns the size before calling
   malloc.  The static_assert also works for MinGW circa 2020, where
   alignof (max_align_t) is 16 even though the malloc alignment is only 8;
   Emacs avoids the bug because on this platform it never does anything
   that requires an alignment of 16.  */
enum { MALLOC_IS_LISP_ALIGNED = alignof (max_align_t) % LISP_ALIGNMENT == 0 };
static_assert (MALLOC_IS_LISP_ALIGNED);

#define MALLOC_PROBE(size)			\
  do {						\
    if (profiler_memory_running)		\
      malloc_probe (size);			\
  } while (0)

/* Like malloc but check for no memory and block interrupt input.  */

void *
xmalloc (size_t size)
{
  void *val = malloc (size);
  if (!val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like the above, but zeroes out the memory just allocated.  */

void *
xzalloc (size_t size)
{
  void *val = calloc (1, size);
  if (!val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like realloc but check for no memory and block interrupt input.  */

void *
xrealloc (void *block, size_t size)
{
  void *val = realloc (block, size);
  if (!val)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}


/* Like free but block interrupt input.  */

void
xfree (void *block)
{
  if (!block)
    return;
  if (pdumper_object_p (block))
    return;
  free (block);
  /* We don't call refill_memory_reserve here
     because in practice the call in r_alloc_free seems to suffice.  */
}


/* Other parts of Emacs pass large int values to allocator functions
   expecting ptrdiff_t.  This is portable in practice, but check it to
   be safe.  */
static_assert (INT_MAX <= PTRDIFF_MAX);


/* Allocate an array of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion, and block interrupt input.  */

void *
xnmalloc (ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  ptrdiff_t nbytes;
  if (ckd_mul (&nbytes, nitems, item_size) || SIZE_MAX < nbytes)
    memory_full (SIZE_MAX);
  return xmalloc (nbytes);
}


/* Reallocate an array PA to make it of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion, and block interrupt input.  */

void *
xnrealloc (void *pa, ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  ptrdiff_t nbytes;
  if (ckd_mul (&nbytes, nitems, item_size) || SIZE_MAX < nbytes)
    memory_full (SIZE_MAX);
  return xrealloc (pa, nbytes);
}


/* Grow PA, which points to an array of *NITEMS items, and return the
   location of the reallocated array, updating *NITEMS to reflect its
   new size.  The new array will contain at least NITEMS_INCR_MIN more
   items, but will not contain more than NITEMS_MAX items total.
   ITEM_SIZE is the size of each item, in bytes.

   ITEM_SIZE and NITEMS_INCR_MIN must be positive.  *NITEMS must be
   nonnegative.  If NITEMS_MAX is -1, it is treated as if it were
   infinity.

   If PA is null, then allocate a new array instead of reallocating
   the old one.

   Block interrupt input as needed.  If memory exhaustion occurs, set
   *NITEMS to zero if PA is null, and signal an error (i.e., do not
   return).

   Thus, to grow an array A without saving its old contents, do
   { xfree (A); A = NULL; A = xpalloc (NULL, &AITEMS, ...); }.
   The A = NULL avoids a dangling pointer if xpalloc exhausts memory
   and signals an error, and later this code is reexecuted and
   attempts to free A.  */

void *
xpalloc (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
	 ptrdiff_t nitems_max, ptrdiff_t item_size)
{
  ptrdiff_t n0 = *nitems;
  eassume (0 < item_size && 0 < nitems_incr_min && 0 <= n0 && -1 <= nitems_max);

  /* The approximate size to use for initial small allocation
     requests.  This is the largest "small" request for the GNU C
     library malloc.  */
  enum { DEFAULT_MXFAST = 64 * sizeof (size_t) / 4 };

  /* If the array is tiny, grow it to about (but no greater than)
     DEFAULT_MXFAST bytes.  Otherwise, grow it by about 50%.
     Adjust the growth according to three constraints: NITEMS_INCR_MIN,
     NITEMS_MAX, and what the C language can represent safely.  */

  ptrdiff_t n, nbytes;
  if (ckd_add (&n, n0, n0 >> 1))
    n = PTRDIFF_MAX;
  if (0 <= nitems_max && nitems_max < n)
    n = nitems_max;

  ptrdiff_t adjusted_nbytes
    = ((ckd_mul (&nbytes, n, item_size) || SIZE_MAX < nbytes)
       ? min (PTRDIFF_MAX, SIZE_MAX)
       : nbytes < DEFAULT_MXFAST ? DEFAULT_MXFAST : 0);
  if (adjusted_nbytes)
    {
      n = adjusted_nbytes / item_size;
      nbytes = adjusted_nbytes - adjusted_nbytes % item_size;
    }

  if (! pa)
    *nitems = 0;
  if (n - n0 < nitems_incr_min
      && (ckd_add (&n, n0, nitems_incr_min)
	  || (0 <= nitems_max && nitems_max < n)
	  || ckd_mul (&nbytes, n, item_size)))
    memory_full (SIZE_MAX);
  pa = xrealloc (pa, nbytes);
  *nitems = n;
  return pa;
}


/* Like strdup, but uses xmalloc.  */

char *
xstrdup (const char *s)
{
  ptrdiff_t size;
  eassert (s);
  size = strlen (s) + 1;
  return memcpy (xmalloc (size), s, size);
}

/* Like above, but duplicates Lisp string to C string.  */

char *
xlispstrdup (Lisp_Object string)
{
  ptrdiff_t size = SBYTES (string) + 1;
  return memcpy (xmalloc (size), SSDATA (string), size);
}

/* Assign to *PTR a copy of STRING, freeing any storage *PTR formerly
   pointed to.  If STRING is null, assign it without copying anything.
   Allocate before freeing, to avoid a dangling pointer if allocation
   fails.  */

void
dupstring (char **ptr, char const *string)
{
  char *old = *ptr;
  *ptr = string ? xstrdup (string) : 0;
  xfree (old);
}


/* Like putenv, but (1) use the equivalent of xmalloc and (2) the
   argument is a const pointer.  */

void
xputenv (char const *string)
{
  if (putenv ((char *) string) != 0)
    memory_full (0);
}

/* Return a newly allocated memory block of SIZE bytes, remembering
   to free it when unwinding.  */
void *
record_xmalloc (size_t size)
{
  void *p = xmalloc (size);
  record_unwind_protect_ptr (xfree, p);
  return p;
}


#if ! USE_LSB_TAG
extern void *lisp_malloc_loser;
void *lisp_malloc_loser EXTERNALLY_VISIBLE;
#endif

/* Allocate memory for Lisp data.
   NBYTES is the number of bytes to allocate;
   it must be a multiple of LISP_ALIGNMENT.
   If CLEARIT, arrange for the allocated memory to be cleared
   by using calloc, which can be faster than malloc+memset.
   TYPE describes the intended use of the allocated memory block
   (for strings, for conses, ...).
   Return a null pointer if and only if allocation failed.

   Code allocating heap memory for Lisp should use this function to get
   a pointer P; that way, if T is an enum Lisp_Type value and
   L == make_lisp_ptr (P, T), then XPNTR (L) == P and XTYPE (L) == T.  */

static void *
lisp_malloc (size_t nbytes, bool clearit, enum mem_type type)
{
  register void *val;

#ifdef GC_MALLOC_CHECK
  allocated_mem_type = type;
#endif

  val = clearit ? calloc (1, nbytes) : malloc (nbytes);

#if ! USE_LSB_TAG
  /* If the memory just allocated cannot be addressed thru a Lisp
     object's pointer, and it needs to be,
     that's equivalent to running out of memory.  */
  if (val && type != MEM_TYPE_NON_LISP)
    {
      Lisp_Object tem;
      XSETCONS (tem, (char *) val + nbytes - 1);
      if ((char *) XCONS (tem) != (char *) val + nbytes - 1)
	{
	  lisp_malloc_loser = val;
	  free (val);
	  val = 0;
	}
    }
#endif

#ifndef GC_MALLOC_CHECK
  if (val && type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type);
#endif

  if (!val)
    memory_full (nbytes);
  MALLOC_PROBE (nbytes);
  return val;
}

/* Free BLOCK.  This must be called to free memory allocated with a
   call to lisp_malloc.  */

static void
lisp_free (void *block)
{
  if (pdumper_object_p (block))
    return;

#ifndef GC_MALLOC_CHECK
  struct mem_node *m = mem_find (block);
#endif
  free (block);
#ifndef GC_MALLOC_CHECK
  mem_delete (m);
#endif
}

/*****  Allocation of aligned blocks of memory to store Lisp data.  *****/

/* The entry point is lisp_align_malloc which returns blocks of at most
   BLOCK_BYTES and guarantees they are aligned on a BLOCK_ALIGN boundary.  */

/* Byte alignment of storage blocks.  */
# define BLOCK_ALIGN (1 << 15)
static_assert (POWER_OF_2 (BLOCK_ALIGN));

/* Use aligned_alloc if it or a simple substitute is available. */

#if (defined HAVE_ALIGNED_ALLOC					\
     || (!defined SYSTEM_MALLOC && !defined DOUG_LEA_MALLOC))
# define USE_ALIGNED_ALLOC 1
#elif defined HAVE_POSIX_MEMALIGN
# define USE_ALIGNED_ALLOC 1
# define aligned_alloc my_aligned_alloc /* Avoid collision with lisp.h.  */
static void *
aligned_alloc (size_t alignment, size_t size)
{
  /* POSIX says the alignment must be a power-of-2 multiple of sizeof (void *).
     Verify this for all arguments this function is given.  */
  static_assert (BLOCK_ALIGN % sizeof (void *) == 0
		 && POWER_OF_2 (BLOCK_ALIGN / sizeof (void *)));
  eassert (alignment == BLOCK_ALIGN);

  void *p;
  return posix_memalign (&p, alignment, size) == 0 ? p : 0;
}
#endif

/* Padding to leave at the end of a malloc'd block.  This is to give
   malloc a chance to minimize the amount of memory wasted to alignment.
   It should be tuned to the particular malloc library used.
   On glibc-2.3.2, malloc never tries to align, so a padding of 0 is best.
   aligned_alloc on the other hand would ideally prefer a value of 4
   because otherwise, there's 1020 bytes wasted between each ablocks.
   In Emacs, testing shows that those 1020 can most of the time be
   efficiently used by malloc to place other objects, so a value of 0 can
   still preferable unless you have a lot of aligned blocks and virtually
   nothing else.  */
#define BLOCK_PADDING 0
#define BLOCK_BYTES \
  (BLOCK_ALIGN - sizeof (struct ablocks *) - BLOCK_PADDING)

/* Internal data structures and constants.  */

#define ABLOCKS_SIZE 16

/* An aligned block of memory.  */
struct ablock
{
  union
  {
    char payload[BLOCK_BYTES];
    struct ablock *next_free;
  } x;

  /* ABASE is the aligned base of the ablocks.  It is overloaded to
     hold a virtual "busy" field that counts twice the number of used
     ablock values in the parent ablocks, plus one if the real base of
     the parent ablocks is ABASE (if the "busy" field is even, the
     word before the first ablock holds a pointer to the real base).
     The first ablock has a "busy" ABASE, and the others have an
     ordinary pointer ABASE.  To tell the difference, the code assumes
     that pointers, when cast to uintptr_t, are at least 2 *
     ABLOCKS_SIZE + 1.  */
  struct ablocks *abase;

  /* The padding of all but the last ablock is unused.  The padding of
     the last ablock in an ablocks is not allocated.  */
#if BLOCK_PADDING
  char padding[BLOCK_PADDING];
#endif
};

/* A bunch of consecutive aligned blocks.  */
struct ablocks
{
  struct ablock blocks[ABLOCKS_SIZE];
};

/* Size of the block requested from malloc or aligned_alloc.  */
#define ABLOCKS_BYTES (sizeof (struct ablocks) - BLOCK_PADDING)

#define ABLOCK_ABASE(block) \
  (((uintptr_t) (block)->abase) <= (1 + 2 * ABLOCKS_SIZE)	\
   ? (struct ablocks *) (block)					\
   : (block)->abase)

/* Virtual `busy' field.  */
#define ABLOCKS_BUSY(a_base) ((a_base)->blocks[0].abase)

/* Pointer to the (not necessarily aligned) malloc block.  */
#ifdef USE_ALIGNED_ALLOC
#define ABLOCKS_BASE(abase) (abase)
#else
#define ABLOCKS_BASE(abase) \
  (1 & (intptr_t) ABLOCKS_BUSY (abase) ? abase : ((void **) (abase))[-1])
#endif

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_ABLOCK(b) \
  __asan_poison_memory_region (&(b)->x, sizeof ((b)->x))
# define ASAN_UNPOISON_ABLOCK(b) \
  __asan_unpoison_memory_region (&(b)->x, sizeof ((b)->x))
#else
# define ASAN_POISON_ABLOCK(b) ((void) 0)
# define ASAN_UNPOISON_ABLOCK(b) ((void) 0)
#endif

/* The list of free ablock.   */
static struct ablock *free_ablock;

#if !USE_ALIGNED_ALLOC

static void *
pointer_align (void *ptr, int alignment)
{
  return (void *) ROUNDUP ((uintptr_t) ptr, alignment);
}

#endif /* !USE_ALIGNED_ALLOC */

/* Allocate an aligned block of nbytes.
   Alignment is on a multiple of BLOCK_ALIGN and `nbytes' has to be
   smaller or equal to BLOCK_BYTES.  */
static void *
lisp_align_malloc (size_t nbytes, enum mem_type type)
{
  void *base, *val;
  struct ablocks *abase;

  eassert (nbytes <= BLOCK_BYTES);

#ifdef GC_MALLOC_CHECK
  allocated_mem_type = type;
#endif

  if (!free_ablock)
    {
      int i;
      bool aligned;

#ifdef DOUG_LEA_MALLOC
      if (!mmap_lisp_allowed_p ())
        mallopt (M_MMAP_MAX, 0);
#endif

#ifdef USE_ALIGNED_ALLOC
      static_assert (ABLOCKS_BYTES % BLOCK_ALIGN == 0);
      abase = base = aligned_alloc (BLOCK_ALIGN, ABLOCKS_BYTES);
#else
      base = malloc (ABLOCKS_BYTES);
      abase = pointer_align (base, BLOCK_ALIGN);
#endif

      if (base == 0)
	memory_full (ABLOCKS_BYTES);

      aligned = (base == abase);
      if (!aligned)
	((void **) abase)[-1] = base;

#ifdef DOUG_LEA_MALLOC
      if (!mmap_lisp_allowed_p ())
          mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif

#if ! USE_LSB_TAG
      /* If the memory just allocated cannot be addressed thru a Lisp
	 object's pointer, and it needs to be, that's equivalent to
	 running out of memory.  */
      if (type != MEM_TYPE_NON_LISP)
	{
	  Lisp_Object tem;
	  char *end = (char *) base + ABLOCKS_BYTES - 1;
	  XSETCONS (tem, end);
	  if ((char *) XCONS (tem) != end)
	    {
	      lisp_malloc_loser = base;
	      free (base);
	      memory_full (SIZE_MAX);
	    }
	}
#endif

      /* Initialize the blocks and put them on the free list.
	 If `base' was not properly aligned, we can't use the last block.  */
      for (i = 0; i < (aligned ? ABLOCKS_SIZE : ABLOCKS_SIZE - 1); i++)
	{
	  abase->blocks[i].abase = abase;
	  abase->blocks[i].x.next_free = free_ablock;
	  ASAN_POISON_ABLOCK (&abase->blocks[i]);
	  free_ablock = &abase->blocks[i];
	}
      intptr_t ialigned = aligned;
      ABLOCKS_BUSY (abase) = (struct ablocks *) ialigned;

      eassert ((uintptr_t) abase % BLOCK_ALIGN == 0);
      eassert (ABLOCK_ABASE (&abase->blocks[3]) == abase); /* 3 is arbitrary */
      eassert (ABLOCK_ABASE (&abase->blocks[0]) == abase);
      eassert (ABLOCKS_BASE (abase) == base);
      eassert ((intptr_t) ABLOCKS_BUSY (abase) == aligned);
    }

  ASAN_UNPOISON_ABLOCK (free_ablock);
  abase = ABLOCK_ABASE (free_ablock);
  ABLOCKS_BUSY (abase)
    = (struct ablocks *) (2 + (intptr_t) ABLOCKS_BUSY (abase));
  val = free_ablock;
  free_ablock = free_ablock->x.next_free;

#ifndef GC_MALLOC_CHECK
  if (type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type);
#endif

  MALLOC_PROBE (nbytes);

  eassert (0 == ((uintptr_t) val) % BLOCK_ALIGN);
  return val;
}

static void
lisp_align_free (void *block)
{
  struct ablock *ablock = block;
  struct ablocks *abase = ABLOCK_ABASE (ablock);

#ifndef GC_MALLOC_CHECK
  mem_delete (mem_find (block));
#endif
  /* Put on free list.  */
  ablock->x.next_free = free_ablock;
  ASAN_POISON_ABLOCK (ablock);
  free_ablock = ablock;
  /* Update busy count.  */
  intptr_t busy = (intptr_t) ABLOCKS_BUSY (abase) - 2;
  eassume (0 <= busy && busy <= 2 * ABLOCKS_SIZE - 1);
  ABLOCKS_BUSY (abase) = (struct ablocks *) busy;

  if (busy < 2)
    { /* All the blocks are free.  */
      int i = 0;
      bool aligned = busy;
      struct ablock **tem = &free_ablock;
      struct ablock *atop = &abase->blocks[aligned ? ABLOCKS_SIZE : ABLOCKS_SIZE - 1];
      while (*tem)
	{
#if GC_ASAN_POISON_OBJECTS
	  __asan_unpoison_memory_region (&(*tem)->x,
					 sizeof ((*tem)->x));
#endif
	  if (*tem >= (struct ablock *) abase && *tem < atop)
	    {
	      i++;
	      *tem = (*tem)->x.next_free;
	    }
	  else
	    tem = &(*tem)->x.next_free;
	}
      eassert ((aligned & 1) == aligned);
      eassert (i == (aligned ? ABLOCKS_SIZE : ABLOCKS_SIZE - 1));
#ifdef USE_POSIX_MEMALIGN
      eassert ((uintptr_t) ABLOCKS_BASE (abase) % BLOCK_ALIGN == 0);
#endif
      free (ABLOCKS_BASE (abase));
    }
}


/***********************************************************************
			 Interval Allocation
 ***********************************************************************/

/* Number of intervals allocated in an interval_block structure.  */

enum { INTERVAL_BLOCK_SIZE
         = ((MALLOC_SIZE_NEAR (1024) - sizeof (struct interval_block *))
	    / sizeof (struct interval)) };

/* Intervals are allocated in chunks in the form of an interval_block
   structure.  */

struct interval_block
{
  /* Place `intervals' first, to preserve alignment.  */
  struct interval intervals[INTERVAL_BLOCK_SIZE];
  struct interval_block *next;
};

/* Current interval block.  Its `next' pointer points to older
   blocks.  */

static struct interval_block *interval_block;

/* Index in interval_block above of the next unused interval
   structure.  */

static int interval_block_index = INTERVAL_BLOCK_SIZE;

/* List of free intervals.  */

static INTERVAL interval_free_list;

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_INTERVAL_BLOCK(b)         \
  __asan_poison_memory_region ((b)->intervals, \
			       sizeof ((b)->intervals))
# define ASAN_UNPOISON_INTERVAL_BLOCK(b)         \
  __asan_unpoison_memory_region ((b)->intervals, \
				 sizeof ((b)->intervals))
# define ASAN_POISON_INTERVAL(i) \
  __asan_poison_memory_region (i, sizeof *(i))
# define ASAN_UNPOISON_INTERVAL(i) \
  __asan_unpoison_memory_region (i, sizeof *(i))
#else
# define ASAN_POISON_INTERVAL_BLOCK(b) ((void) 0)
# define ASAN_UNPOISON_INTERVAL_BLOCK(b) ((void) 0)
# define ASAN_POISON_INTERVAL(i) ((void) 0)
# define ASAN_UNPOISON_INTERVAL(i) ((void) 0)
#endif

/* Return a new interval.  */

INTERVAL
make_interval (void)
{
  INTERVAL val;

  if (interval_free_list)
    {
      val = interval_free_list;
      ASAN_UNPOISON_INTERVAL (val);
      interval_free_list = INTERVAL_PARENT (interval_free_list);
    }
  else
    {
      if (interval_block_index == INTERVAL_BLOCK_SIZE)
	{
	  struct interval_block *newi
	    = lisp_malloc (sizeof *newi, false, MEM_TYPE_NON_LISP);

	  newi->next = interval_block;
	  ASAN_POISON_INTERVAL_BLOCK (newi);
	  interval_block = newi;
	  interval_block_index = 0;
	}
      val = &interval_block->intervals[interval_block_index++];
      ASAN_UNPOISON_INTERVAL (val);
    }

  tally_consing (sizeof (struct interval));
  intervals_consed++;
  RESET_INTERVAL (val);
  val->gcmarkbit = 0;
  return val;
}


/* Mark Lisp objects in interval I.  */

static void
mark_interval_tree_1 (INTERVAL i, void *dummy)
{
  /* Intervals should never be shared.  So, if extra internal checking is
     enabled, GC aborts if it seems to have visited an interval twice.  */
  eassert (!interval_marked_p (i));
  set_interval_marked (i);
  mark_object (i->plist);
}

/* Mark the interval tree rooted in I.  */

static void
mark_interval_tree (INTERVAL i)
{
  if (i && !interval_marked_p (i))
    traverse_intervals_noorder (i, mark_interval_tree_1, NULL);
}

/***********************************************************************
			  String Allocation
 ***********************************************************************/

/* Lisp_Strings are allocated in string_block structures.  When a new
   string_block is allocated, all the Lisp_Strings it contains are
   added to a free-list string_free_list.  When a new Lisp_String is
   needed, it is taken from that list.  During the sweep phase of GC,
   string_blocks that are entirely free are freed, except two which
   we keep.

   String data is allocated from sblock structures.  Strings larger
   than LARGE_STRING_BYTES, get their own sblock, data for smaller
   strings is sub-allocated out of sblocks of size SBLOCK_SIZE.

   Sblocks consist internally of sdata structures, one for each
   Lisp_String.  The sdata structure points to the Lisp_String it
   belongs to.  The Lisp_String points back to the `u.data' member of
   its sdata structure.

   When a Lisp_String is freed during GC, it is put back on
   string_free_list, and its `data' member and its sdata's `string'
   pointer is set to null.  The size of the string is recorded in the
   `n.nbytes' member of the sdata.  So, sdata structures that are no
   longer used, can be easily recognized, and it's easy to compact the
   sblocks of small strings which we do in compact_small_strings.  */

/* Size in bytes of an sblock structure used for small strings.  */

enum { SBLOCK_SIZE = MALLOC_SIZE_NEAR (8192) };

/* Strings larger than this are considered large strings.  String data
   for large strings is allocated from individual sblocks.  */

#define LARGE_STRING_BYTES 1024

/* The layout of a nonnull string.  */

struct sdata
{
  /* Back-pointer to the string this sdata belongs to.  If null, this
     structure is free, and NBYTES (in this structure or in the union below)
     contains the string's byte size (the same value that STRING_BYTES
     would return if STRING were non-null).  If non-null, STRING_BYTES
     (STRING) is the size of the data, and DATA contains the string's
     contents.  */
  struct Lisp_String *string;

#ifdef GC_CHECK_STRING_BYTES
  ptrdiff_t nbytes;
#endif

  unsigned char data[FLEXIBLE_ARRAY_MEMBER];
};

/* A union describing string memory sub-allocated from an sblock.
   This is where the contents of Lisp strings are stored.  */

typedef union
{
  struct Lisp_String *string;

  /* When STRING is nonnull, this union is actually of type 'struct sdata',
     which has a flexible array member.  However, if implemented by
     giving this union a member of type 'struct sdata', the union
     could not be the last (flexible) member of 'struct sblock',
     because C99 prohibits a flexible array member from having a type
     that is itself a flexible array.  So, comment this member out here,
     but remember that the option's there when using this union.  */
#if 0
  struct sdata u;
#endif

  /* When STRING is null.  */
  struct
  {
    struct Lisp_String *string;
    ptrdiff_t nbytes;
  } n;
} sdata;

#define SDATA_NBYTES(S)	(S)->n.nbytes
#define SDATA_DATA(S)	((struct sdata *) (S))->data

enum { SDATA_DATA_OFFSET = offsetof (struct sdata, data) };

/* Structure describing a block of memory which is sub-allocated to
   obtain string data memory for strings.  Blocks for small strings
   are of fixed size SBLOCK_SIZE.  Blocks for large strings are made
   as large as needed.  */

struct sblock
{
  /* Next in list.  */
  struct sblock *next;

  /* Pointer to the next free sdata block.  This points past the end
     of the sblock if there isn't any space left in this block.  */
  sdata *next_free;

  /* String data.  */
  sdata data[FLEXIBLE_ARRAY_MEMBER];
};

/* Number of Lisp strings in a string_block structure.  */

enum { STRING_BLOCK_SIZE
         = ((MALLOC_SIZE_NEAR (1024) - sizeof (struct string_block *))
	    / sizeof (struct Lisp_String)) };

/* Structure describing a block from which Lisp_String structures
   are allocated.  */

struct string_block
{
  /* Place `strings' first, to preserve alignment.  */
  struct Lisp_String strings[STRING_BLOCK_SIZE];
  struct string_block *next;
};

/* Head and tail of the list of sblock structures holding Lisp string
   data.  We always allocate from current_sblock.  The NEXT pointers
   in the sblock structures go from oldest_sblock to current_sblock.  */

static struct sblock *oldest_sblock, *current_sblock;

/* List of sblocks for large strings.  */

static struct sblock *large_sblocks;

/* List of string_block structures.  */

static struct string_block *string_blocks;

/* Free-list of Lisp_Strings.  */

static struct Lisp_String *string_free_list;

/* Given a pointer to a Lisp_String S which is on the free-list
   string_free_list, return a pointer to its successor in the
   free-list.  */

#define NEXT_FREE_LISP_STRING(S) ((S)->u.next)

/* Return a pointer to the sdata structure belonging to Lisp string S.
   S must be live, i.e. S->data must not be null.  S->data is actually
   a pointer to the `u.data' member of its sdata structure; the
   structure starts at a constant offset in front of that.  */

#define SDATA_OF_STRING(S) ((sdata *) ((S)->u.s.data - SDATA_DATA_OFFSET))


#ifdef GC_CHECK_STRING_OVERRUN

/* Check for overrun in string data blocks by appending a small
   "cookie" after each allocated string data block, and check for the
   presence of this cookie during GC.  */
# define GC_STRING_OVERRUN_COOKIE_SIZE ROUNDUP (4, alignof (sdata))
static char const string_overrun_cookie[GC_STRING_OVERRUN_COOKIE_SIZE] =
  { '\xde', '\xad', '\xbe', '\xef', /* Perhaps some zeros here.  */ };

#else
# define GC_STRING_OVERRUN_COOKIE_SIZE 0
#endif

/* Return the size of an sdata structure large enough to hold N bytes
   of string data.  This counts the sdata structure, the N bytes, a
   terminating NUL byte, and alignment padding.  */

static ptrdiff_t
sdata_size (ptrdiff_t n)
{
  /* Reserve space for the nbytes union member even when N + 1 is less
     than the size of that member.  */
  ptrdiff_t unaligned_size = max (SDATA_DATA_OFFSET + n + 1,
				  sizeof (sdata));
  int sdata_align = max (FLEXALIGNOF (struct sdata), alignof (sdata));
  return (unaligned_size + sdata_align - 1) & ~(sdata_align - 1);
}

/* Extra bytes to allocate for each string.  */
#define GC_STRING_EXTRA GC_STRING_OVERRUN_COOKIE_SIZE

/* Exact bound on the number of bytes in a string, not counting the
   terminating null.  A string cannot contain more bytes than
   STRING_BYTES_BOUND, nor can it be so long that the size_t
   arithmetic in allocate_string_data would overflow while it is
   calculating a value to be passed to malloc.  */
static ptrdiff_t const STRING_BYTES_MAX =
  min (STRING_BYTES_BOUND,
       ((SIZE_MAX
	 - GC_STRING_EXTRA
	 - offsetof (struct sblock, data)
	 - SDATA_DATA_OFFSET)
	& ~(sizeof (EMACS_INT) - 1)));

/* Initialize string allocation.  Called from init_alloc_once.  */

static struct Lisp_String *allocate_string (void);
static void
allocate_string_data (struct Lisp_String *s,
		      EMACS_INT nchars, EMACS_INT nbytes, bool clearit,
		      bool immovable);

static void
init_strings (void)
{
  /* String allocation code will return one of 'empty_*ibyte_string'
     when asked to construct a new 0-length string, so in order to build
     those special cases, we have to do it "by hand".  */
  struct Lisp_String *ems = allocate_string ();
  struct Lisp_String *eus = allocate_string ();
  ems->u.s.intervals = NULL;
  eus->u.s.intervals = NULL;
  allocate_string_data (ems, 0, 0, false, false);
  allocate_string_data (eus, 0, 0, false, false);
  /* We can't use 'STRING_SET_UNIBYTE' because this one includes a hack
   * to redirect its arg to 'empty_unibyte_string' when nbytes == 0. */
  eus->u.s.size_byte = -1;
  XSETSTRING (empty_multibyte_string, ems);
  XSETSTRING (empty_unibyte_string, eus);
  staticpro (&empty_unibyte_string);
  staticpro (&empty_multibyte_string);
}

#if GC_ASAN_POISON_OBJECTS
/* Prepare s for denoting a free sdata struct, i.e, poison all bytes
   in the flexible array member, except the first SDATA_OFFSET bytes.
   This is only effective for strings of size n where n > sdata_size(n).
 */
# define ASAN_PREPARE_DEAD_SDATA(s, size)                          \
  do {                                                             \
    __asan_poison_memory_region (s, sdata_size (size));		   \
    __asan_unpoison_memory_region (&(s)->string,		   \
				   sizeof (struct Lisp_String *)); \
    __asan_unpoison_memory_region (&SDATA_NBYTES (s),		   \
				   sizeof SDATA_NBYTES (s));	   \
   } while (false)
/* Prepare s for storing string data for NBYTES bytes.  */
# define ASAN_PREPARE_LIVE_SDATA(s, nbytes) \
  __asan_unpoison_memory_region (s, sdata_size (nbytes))
# define ASAN_POISON_SBLOCK_DATA(b, size) \
  __asan_poison_memory_region ((b)->data, size)
# define ASAN_POISON_STRING_BLOCK(b) \
  __asan_poison_memory_region ((b)->strings, STRING_BLOCK_SIZE)
# define ASAN_UNPOISON_STRING_BLOCK(b) \
  __asan_unpoison_memory_region ((b)->strings, STRING_BLOCK_SIZE)
# define ASAN_POISON_STRING(s) \
  __asan_poison_memory_region (s, sizeof *(s))
# define ASAN_UNPOISON_STRING(s) \
  __asan_unpoison_memory_region (s, sizeof *(s))
#else
# define ASAN_PREPARE_DEAD_SDATA(s, size) ((void) 0)
# define ASAN_PREPARE_LIVE_SDATA(s, nbytes) ((void) 0)
# define ASAN_POISON_SBLOCK_DATA(b, size) ((void) 0)
# define ASAN_POISON_STRING_BLOCK(b) ((void) 0)
# define ASAN_UNPOISON_STRING_BLOCK(b) ((void) 0)
# define ASAN_POISON_STRING(s) ((void) 0)
# define ASAN_UNPOISON_STRING(s) ((void) 0)
#endif

#ifdef GC_CHECK_STRING_BYTES

static int check_string_bytes_count;

/* Like STRING_BYTES, but with debugging check.  Can be
   called during GC, so pay attention to the mark bit.  */

ptrdiff_t
string_bytes (struct Lisp_String *s)
{
  ptrdiff_t nbytes =
    (s->u.s.size_byte < 0 ? s->u.s.size & ~ARRAY_MARK_FLAG : s->u.s.size_byte);

  if (!pdumper_object_p (s) && s->u.s.data
      && nbytes != SDATA_NBYTES (SDATA_OF_STRING (s)))
    emacs_abort ();
  return nbytes;
}

/* Check validity of Lisp strings' string_bytes member in B.  */

static void
check_sblock (struct sblock *b)
{
  sdata *end = b->next_free;

  for (sdata *from = b->data; from < end; )
    {
      ptrdiff_t nbytes = sdata_size (from->string
				     ? string_bytes (from->string)
				     : SDATA_NBYTES (from));
      from = (sdata *) ((char *) from + nbytes + GC_STRING_EXTRA);
    }
}


/* Check validity of Lisp strings' string_bytes member.  ALL_P
   means check all strings, otherwise check only most
   recently allocated strings.  Used for hunting a bug.  */

static void
check_string_bytes (bool all_p)
{
  if (all_p)
    {
      struct sblock *b;

      for (b = large_sblocks; b; b = b->next)
	{
	  struct Lisp_String *s = b->data[0].string;
	  if (s)
	    string_bytes (s);
	}

      for (b = oldest_sblock; b; b = b->next)
	check_sblock (b);
    }
  else if (current_sblock)
    check_sblock (current_sblock);
}

#else /* not GC_CHECK_STRING_BYTES */

#define check_string_bytes(all) ((void) 0)

#endif /* GC_CHECK_STRING_BYTES */

#ifdef GC_CHECK_STRING_FREE_LIST

/* Walk through the string free list looking for bogus next pointers.
   This may catch buffer overrun from a previous string.  */

static void
check_string_free_list (void)
{
  struct Lisp_String *s;

  /* Pop a Lisp_String off the free-list.  */
  s = string_free_list;
  while (s != NULL)
    {
      if ((uintptr_t) s < 1024)
	emacs_abort ();
      s = NEXT_FREE_LISP_STRING (s);
    }
}
#else
#define check_string_free_list()
#endif

/* Return a new Lisp_String.  */

static struct Lisp_String *
allocate_string (void)
{
  struct Lisp_String *s;

  /* If the free-list is empty, allocate a new string_block, and
     add all the Lisp_Strings in it to the free-list.  */
  if (string_free_list == NULL)
    {
      struct string_block *b = lisp_malloc (sizeof *b, false, MEM_TYPE_STRING);
      int i;

      b->next = string_blocks;
      string_blocks = b;

      for (i = STRING_BLOCK_SIZE - 1; i >= 0; --i)
	{
	  s = b->strings + i;
	  /* Every string on a free list should have NULL data pointer.  */
	  s->u.s.data = NULL;
	  NEXT_FREE_LISP_STRING (s) = string_free_list;
	  string_free_list = s;
	}
      ASAN_POISON_STRING_BLOCK (b);
    }

  check_string_free_list ();

  /* Pop a Lisp_String off the free-list.  */
  s = string_free_list;
  ASAN_UNPOISON_STRING (s);
  string_free_list = NEXT_FREE_LISP_STRING (s);

  ++strings_consed;
  tally_consing (sizeof *s);

#ifdef GC_CHECK_STRING_BYTES
  if (!noninteractive)
    {
      if (++check_string_bytes_count == 200)
	{
	  check_string_bytes_count = 0;
	  check_string_bytes (1);
	}
      else
	check_string_bytes (0);
    }
#endif /* GC_CHECK_STRING_BYTES */

  return s;
}


/* Set up Lisp_String S for holding NCHARS characters, NBYTES bytes,
   plus a NUL byte at the end.  Allocate an sdata structure DATA for
   S, and set S->u.s.data to SDATA->u.data.  Store a NUL byte at the
   end of S->u.s.data.  Set S->u.s.size to NCHARS and S->u.s.size_byte
   to NBYTES.  Free S->u.s.data if it was initially non-null.

   If CLEARIT, also clear the other bytes of S->u.s.data.  */

static void
allocate_string_data (struct Lisp_String *s,
		      EMACS_INT nchars, EMACS_INT nbytes, bool clearit,
		      bool immovable)
{
  sdata *data;
  struct sblock *b;

  if (STRING_BYTES_MAX < nbytes)
    string_overflow ();

  /* Determine the number of bytes needed to store NBYTES bytes
     of string data.  */
  ptrdiff_t needed = sdata_size (nbytes);

  if (nbytes > LARGE_STRING_BYTES || immovable)
    {
      size_t size = FLEXSIZEOF (struct sblock, data, needed);

#ifdef DOUG_LEA_MALLOC
      if (!mmap_lisp_allowed_p ())
        mallopt (M_MMAP_MAX, 0);
#endif

      b = lisp_malloc (size + GC_STRING_EXTRA, clearit, MEM_TYPE_NON_LISP);
      ASAN_POISON_SBLOCK_DATA (b, size);

#ifdef DOUG_LEA_MALLOC
      if (!mmap_lisp_allowed_p ())
        mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif

      data = b->data;
      b->next = large_sblocks;
      b->next_free = data;
      large_sblocks = b;
    }
  else
    {
      b = current_sblock;

      if (b == NULL
	  || (SBLOCK_SIZE - GC_STRING_EXTRA
	      < (char *) b->next_free - (char *) b + needed))
	{
	  /* Not enough room in the current sblock.  */
	  b = lisp_malloc (SBLOCK_SIZE, false, MEM_TYPE_NON_LISP);
	  ASAN_POISON_SBLOCK_DATA (b, SBLOCK_SIZE);

	  data = b->data;
	  b->next = NULL;
	  b->next_free = data;

	  if (current_sblock)
	    current_sblock->next = b;
	  else
	    oldest_sblock = b;
	  current_sblock = b;
	}

      data = b->next_free;

      if (clearit)
	{
#if GC_ASAN_POISON_OBJECTS
	  /* We are accessing SDATA_DATA (data) before it gets
	   * normally unpoisoned, so do it manually.  */
	  __asan_unpoison_memory_region (SDATA_DATA (data), nbytes);
#endif
	  memset (SDATA_DATA (data), 0, nbytes);
	}
    }

  ASAN_PREPARE_LIVE_SDATA (data, nbytes);
  data->string = s;
  b->next_free = (sdata *) ((char *) data + needed + GC_STRING_EXTRA);
  eassert ((uintptr_t) b->next_free % alignof (sdata) == 0);

  s->u.s.data = SDATA_DATA (data);
#ifdef GC_CHECK_STRING_BYTES
  SDATA_NBYTES (data) = nbytes;
#endif
  s->u.s.size = nchars;
  s->u.s.size_byte = nbytes;
  s->u.s.data[nbytes] = '\0';
#ifdef GC_CHECK_STRING_OVERRUN
  memcpy ((char *) data + needed, string_overrun_cookie,
	  GC_STRING_OVERRUN_COOKIE_SIZE);
#endif

  tally_consing (needed);
}


/* Sweep and compact strings.  */

NO_INLINE /* For better stack traces */
static void
sweep_strings (void)
{
  struct string_block *b, *next;
  struct string_block *live_blocks = NULL;

  string_free_list = NULL;
  gcstat.total_strings = gcstat.total_free_strings = 0;
  gcstat.total_string_bytes = 0;

  /* Scan strings_blocks, free Lisp_Strings that aren't marked.  */
  for (b = string_blocks; b; b = next)
    {
      int i, nfree = 0;
      struct Lisp_String *free_list_before = string_free_list;

      ASAN_UNPOISON_STRING_BLOCK (b);

      next = b->next;

      for (i = 0; i < STRING_BLOCK_SIZE; ++i)
	{
	  struct Lisp_String *s = b->strings + i;

	  ASAN_UNPOISON_STRING (s);

	  if (s->u.s.data)
	    {
	      /* String was not on free-list before.  */
	      if (XSTRING_MARKED_P (s))
		{
		  /* String is live; unmark it and its intervals.  */
		  XUNMARK_STRING (s);

		  /* Do not use string_(set|get)_intervals here.  */
		  s->u.s.intervals = balance_intervals (s->u.s.intervals);

		  gcstat.total_strings++;
		  gcstat.total_string_bytes += STRING_BYTES (s);
		}
	      else
		{
		  /* String is dead.  Put it on the free-list.  */
		  sdata *data = SDATA_OF_STRING (s);

		  /* Save the size of S in its sdata so that we know
		     how large that is.  Reset the sdata's string
		     back-pointer so that we know it's free.  */
#ifdef GC_CHECK_STRING_BYTES
		  if (string_bytes (s) != SDATA_NBYTES (data))
		    emacs_abort ();
#else
		  data->n.nbytes = STRING_BYTES (s);
#endif
		  data->string = NULL;

		  /* Reset the strings's `data' member so that we
		     know it's free.  */
		  s->u.s.data = NULL;

		  /* Put the string on the free-list.  */
		  NEXT_FREE_LISP_STRING (s) = string_free_list;
		  ASAN_POISON_STRING (s);
		  ASAN_PREPARE_DEAD_SDATA (data, SDATA_NBYTES (data));
		  string_free_list = s;
		  ++nfree;
		}
	    }
	  else
	    {
	      /* S was on the free-list before.  Put it there again.  */
	      NEXT_FREE_LISP_STRING (s) = string_free_list;
	      ASAN_POISON_STRING (s);

	      string_free_list = s;
	      ++nfree;
	    }
	}

      /* Free blocks that contain free Lisp_Strings only, except
	 the first two of them.  */
      if (nfree == STRING_BLOCK_SIZE
	  && gcstat.total_free_strings > STRING_BLOCK_SIZE)
	{
	  lisp_free (b);
	  string_free_list = free_list_before;
	}
      else
	{
	  gcstat.total_free_strings += nfree;
	  b->next = live_blocks;
	  live_blocks = b;
	}
    }

  check_string_free_list ();

  string_blocks = live_blocks;
  free_large_strings ();
  compact_small_strings ();

  check_string_free_list ();
}


/* Free dead large strings.  */

static void
free_large_strings (void)
{
  struct sblock *b, *next;
  struct sblock *live_blocks = NULL;

  for (b = large_sblocks; b; b = next)
    {
      next = b->next;

      if (b->data[0].string == NULL)
	lisp_free (b);
      else
	{
	  b->next = live_blocks;
	  live_blocks = b;
	}
    }

  large_sblocks = live_blocks;
}


/* Compact data of small strings.  Free sblocks that don't contain
   data of live strings after compaction.  */

static void
compact_small_strings (void)
{
  /* TB is the sblock we copy to, TO is the sdata within TB we copy
     to, and TB_END is the end of TB.  */
  struct sblock *tb = oldest_sblock;
  if (tb)
    {
      sdata *tb_end = (sdata *) ((char *) tb + SBLOCK_SIZE);
      sdata *to = tb->data;

      /* Step through the blocks from the oldest to the youngest.  We
	 expect that old blocks will stabilize over time, so that less
	 copying will happen this way.  */
      struct sblock *b = tb;
      do
	{
	  sdata *end = b->next_free;
	  eassert ((char *) end <= (char *) b + SBLOCK_SIZE);

	  for (sdata *from = b->data; from < end; )
	    {
	      /* Compute the next FROM here because copying below may
		 overwrite data we need to compute it.  */
	      ptrdiff_t nbytes;
	      struct Lisp_String *s = from->string;

#ifdef GC_CHECK_STRING_BYTES
	      /* Check that the string size recorded in the string is the
		 same as the one recorded in the sdata structure.  */
	      if (s && string_bytes (s) != SDATA_NBYTES (from))
		emacs_abort ();
#endif /* GC_CHECK_STRING_BYTES */

	      nbytes = s ? STRING_BYTES (s) : SDATA_NBYTES (from);
	      eassert (nbytes <= LARGE_STRING_BYTES);

	      ptrdiff_t size = sdata_size (nbytes);
	      sdata *from_end = (sdata *) ((char *) from
					   + size + GC_STRING_EXTRA);

#ifdef GC_CHECK_STRING_OVERRUN
	      if (memcmp (string_overrun_cookie,
			  (char *) from_end - GC_STRING_OVERRUN_COOKIE_SIZE,
			  GC_STRING_OVERRUN_COOKIE_SIZE))
		emacs_abort ();
#endif

	      /* Non-NULL S means it's alive.  Copy its data.  */
	      if (s)
		{
		  /* If TB is full, proceed with the next sblock.  */
		  sdata *to_end = (sdata *) ((char *) to
					     + size + GC_STRING_EXTRA);
		  if (to_end > tb_end)
		    {
		      tb->next_free = to;
		      tb = tb->next;
		      tb_end = (sdata *) ((char *) tb + SBLOCK_SIZE);
		      to = tb->data;
		      to_end = (sdata *) ((char *) to + size + GC_STRING_EXTRA);
		    }

		  /* Copy, and update the string's `data' pointer.  */
		  if (from != to)
		    {
		      eassert (tb != b || to < from);
		      ASAN_PREPARE_LIVE_SDATA (to, nbytes);
		      memmove (to, from, size + GC_STRING_EXTRA);
		      to->string->u.s.data = SDATA_DATA (to);
		    }

		  /* Advance past the sdata we copied to.  */
		  to = to_end;
		}
	      from = from_end;
	    }
	  b = b->next;
	}
      while (b);

      /* The rest of the sblocks following TB don't contain live data, so
	 we can free them.  */
      for (b = tb->next; b; )
	{
	  struct sblock *next = b->next;
	  lisp_free (b);
	  b = next;
	}

      tb->next_free = to;
      tb->next = NULL;
    }

  current_sblock = tb;
}

void
string_overflow (void)
{
  error ("Maximum string size exceeded");
}

static Lisp_Object make_clear_string (EMACS_INT, bool);
static Lisp_Object make_clear_multibyte_string (EMACS_INT, EMACS_INT, bool);

DEFUN ("make-string", Fmake_string, Smake_string, 2, 3, 0,
       doc: /* Return a newly created string of length LENGTH, with INIT in each element.
LENGTH must be an integer.
INIT must be an integer that represents a character.
If optional argument MULTIBYTE is non-nil, the result will be
a multibyte string even if INIT is an ASCII character.  */)
  (Lisp_Object length, Lisp_Object init, Lisp_Object multibyte)
{
  Lisp_Object val;
  EMACS_INT nbytes;

  CHECK_FIXNAT (length);
  CHECK_CHARACTER (init);

  int c = XFIXNAT (init);
  bool clearit = !c;

  if (ASCII_CHAR_P (c) && NILP (multibyte))
    {
      nbytes = XFIXNUM (length);
      val = make_clear_string (nbytes, clearit);
      if (nbytes && !clearit)
	{
	  memset (SDATA (val), c, nbytes);
	  SDATA (val)[nbytes] = 0;
	}
    }
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      ptrdiff_t len = CHAR_STRING (c, str);
      EMACS_INT string_len = XFIXNUM (length);

      if (ckd_mul (&nbytes, len, string_len))
	string_overflow ();
      val = make_clear_multibyte_string (string_len, nbytes, clearit);
      if (!clearit)
	{
	  unsigned char *beg = SDATA (val), *end = beg + nbytes;
	  for (unsigned char *p = beg; p < end; p += len)
	    {
	      /* First time we just copy STR to the data of VAL.  */
	      if (p == beg)
		memcpy (p, str, len);
	      else
		{
		  /* Next time we copy largest possible chunk from
		     initialized to uninitialized part of VAL.  */
		  len = min (p - beg, end - p);
		  memcpy (p, beg, len);
		}
	    }
	}
    }

  return val;
}

/* Fill A with 1 bits if INIT is non-nil, and with 0 bits otherwise.
   Return A.  */

Lisp_Object
bool_vector_fill (Lisp_Object a, Lisp_Object init)
{
  EMACS_INT nbits = bool_vector_size (a);
  if (0 < nbits)
    {
      unsigned char *data = bool_vector_uchar_data (a);
      int pattern = NILP (init) ? 0 : (1 << BOOL_VECTOR_BITS_PER_CHAR) - 1;
      ptrdiff_t nbytes = bool_vector_bytes (nbits);
      int last_mask = ~ (~0u << ((nbits - 1) % BOOL_VECTOR_BITS_PER_CHAR + 1));
      memset (data, pattern, nbytes - 1);
      data[nbytes - 1] = pattern & last_mask;
    }
  return a;
}

/* Return a newly allocated, bool vector of size NBITS.  If CLEARIT,
   clear its slots; otherwise the vector's slots are uninitialized.  */

Lisp_Object
make_clear_bool_vector (EMACS_INT nbits, bool clearit)
{
  eassert (0 <= nbits && nbits <= BOOL_VECTOR_LENGTH_MAX);
  Lisp_Object val;
  ptrdiff_t words = bool_vector_words (nbits);
  ptrdiff_t word_bytes = words * sizeof (bits_word);
  ptrdiff_t needed_elements = ((bool_header_size - header_size + word_bytes
				+ word_size - 1)
			       / word_size);
  struct Lisp_Bool_Vector *p
    = (struct Lisp_Bool_Vector *) allocate_clear_vector (needed_elements,
							 clearit);
  /* Clear padding at end; but only if necessary, to avoid polluting the
     data cache.  */
  if (!clearit && nbits % BITS_PER_BITS_WORD != 0)
    p->data[words - 1] = 0;

  XSETVECTOR (val, p);
  XSETPVECTYPESIZE (XVECTOR (val), PVEC_BOOL_VECTOR, 0, 0);
  p->size = nbits;
  return val;
}

/* Return a newly allocated, uninitialized bool vector of size NBITS.  */

Lisp_Object
make_uninit_bool_vector (EMACS_INT nbits)
{
  return make_clear_bool_vector (nbits, false);
}

DEFUN ("make-bool-vector", Fmake_bool_vector, Smake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
  (Lisp_Object length, Lisp_Object init)
{
  CHECK_FIXNAT (length);
  EMACS_INT len = XFIXNAT (length);
  if (BOOL_VECTOR_LENGTH_MAX < len)
    memory_full (SIZE_MAX);
  Lisp_Object val = make_clear_bool_vector (len, NILP (init));
  return NILP (init) ? val : bool_vector_fill (val, init);
}

DEFUN ("bool-vector", Fbool_vector, Sbool_vector, 0, MANY, 0,
       doc: /* Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (bool-vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (BOOL_VECTOR_LENGTH_MAX < nargs)
    memory_full (SIZE_MAX);
  Lisp_Object vector = make_clear_bool_vector (nargs, true);
  for (ptrdiff_t i = 0; i < nargs; i++)
    if (!NILP (args[i]))
      bool_vector_set (vector, i, true);
  return vector;
}

/* Make a string from NBYTES bytes at CONTENTS, and compute the number
   of characters from the contents.  This string may be unibyte or
   multibyte, depending on the contents.  */

Lisp_Object
make_string (const char *contents, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  ptrdiff_t nchars, multibyte_nbytes;

  parse_str_as_multibyte ((const unsigned char *) contents, nbytes,
			  &nchars, &multibyte_nbytes);
  if (nbytes == nchars || nbytes != multibyte_nbytes)
    /* CONTENTS contains no multibyte sequences or contains an invalid
       multibyte sequence.  We must make unibyte string.  */
    val = make_unibyte_string (contents, nbytes);
  else
    val = make_multibyte_string (contents, nchars, nbytes);
  return val;
}

/* Make a unibyte string from LENGTH bytes at CONTENTS.  */

Lisp_Object
make_unibyte_string (const char *contents, ptrdiff_t length)
{
  register Lisp_Object val;
  val = make_uninit_string (length);
  memcpy (SDATA (val), contents, length);
  return val;
}


/* Make a multibyte string from NCHARS characters occupying NBYTES
   bytes at CONTENTS.  */

Lisp_Object
make_multibyte_string (const char *contents,
		       ptrdiff_t nchars, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  return val;
}


/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  It is a multibyte string if NBYTES != NCHARS.  */

Lisp_Object
make_string_from_bytes (const char *contents,
			ptrdiff_t nchars, ptrdiff_t nbytes)
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  if (SBYTES (val) == SCHARS (val))
    STRING_SET_UNIBYTE (val);
  return val;
}


/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  The argument MULTIBYTE controls whether to label the
   string as multibyte.  If NCHARS is negative, it counts the number of
   characters by itself.  */

Lisp_Object
make_specified_string (const char *contents,
		       ptrdiff_t nchars, ptrdiff_t nbytes, bool multibyte)
{
  Lisp_Object val;

  if (nchars < 0)
    {
      if (multibyte)
	nchars = multibyte_chars_in_text ((const unsigned char *) contents,
					  nbytes);
      else
	nchars = nbytes;
    }
  val = make_uninit_multibyte_string (nchars, nbytes);
  memcpy (SDATA (val), contents, nbytes);
  if (!multibyte)
    STRING_SET_UNIBYTE (val);
  return val;
}


/* Return a unibyte Lisp_String set up to hold LENGTH characters
   occupying LENGTH bytes.  If CLEARIT, clear its contents to null
   bytes; otherwise, the contents are uninitialized.  */

static Lisp_Object
make_clear_string (EMACS_INT length, bool clearit)
{
  Lisp_Object val;

  if (!length)
    return empty_unibyte_string;
  val = make_clear_multibyte_string (length, length, clearit);
  STRING_SET_UNIBYTE (val);
  return val;
}

/* Return a unibyte Lisp_String set up to hold LENGTH characters
   occupying LENGTH bytes.  */

Lisp_Object
make_uninit_string (EMACS_INT length)
{
  return make_clear_string (length, false);
}


/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  If CLEARIT, clear its contents to null
   bytes; otherwise, the contents are uninitialized.  */

static Lisp_Object
make_clear_multibyte_string (EMACS_INT nchars, EMACS_INT nbytes, bool clearit)
{
  Lisp_Object string;
  struct Lisp_String *s;

  if (nchars < 0)
    emacs_abort ();
  if (!nbytes)
    return empty_multibyte_string;

  s = allocate_string ();
  s->u.s.intervals = NULL;
  allocate_string_data (s, nchars, nbytes, clearit, false);
  XSETSTRING (string, s);
  string_chars_consed += nbytes;
  return string;
}

/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  */

Lisp_Object
make_uninit_multibyte_string (EMACS_INT nchars, EMACS_INT nbytes)
{
  return make_clear_multibyte_string (nchars, nbytes, false);
}

/* Return a Lisp_String according to a doprnt-style FORMAT and args.  */

Lisp_Object
make_formatted_string (const char *format, ...)
{
  char buf[MAX_ALLOCA];
  char *cstr = buf;
  ptrdiff_t bufsize = sizeof buf;
  va_list ap;

  va_start (ap, format);
  ptrdiff_t length = evxprintf (&cstr, &bufsize, buf, -1, format, ap);
  va_end (ap);
  Lisp_Object ret = make_string (cstr, length);
  if (cstr != buf)
    xfree (cstr);
  return ret;
}

/* Pin a unibyte string in place so that it won't move during GC.  */
void
pin_string (Lisp_Object string)
{
  eassert (STRINGP (string) && !STRING_MULTIBYTE (string));
  struct Lisp_String *s = XSTRING (string);
  ptrdiff_t size = STRING_BYTES (s);
  unsigned char *data = s->u.s.data;

  if (!(size > LARGE_STRING_BYTES
	|| pdumper_object_p (data)
	|| s->u.s.size_byte == -3))
    {
      eassert (s->u.s.size_byte == -1);
      sdata *old_sdata = SDATA_OF_STRING (s);
      allocate_string_data (s, size, size, false, true);
      memcpy (s->u.s.data, data, size);
      old_sdata->string = NULL;
      SDATA_NBYTES (old_sdata) = size;
      ASAN_PREPARE_DEAD_SDATA (old_sdata, size);
    }
  s->u.s.size_byte = -3;
}


/***********************************************************************
			   Float Allocation
 ***********************************************************************/

/* We store float cells inside of float_blocks, allocating a new
   float_block with malloc whenever necessary.  Float cells reclaimed
   by GC are put on a free list to be reallocated before allocating
   any new float cells from the latest float_block.  */

#define FLOAT_BLOCK_SIZE					\
  (((BLOCK_BYTES - sizeof (struct float_block *)		\
     /* The compiler might add padding at the end.  */		\
     - (sizeof (struct Lisp_Float) - sizeof (bits_word))) * CHAR_BIT) \
   / (sizeof (struct Lisp_Float) * CHAR_BIT + 1))

#define GETMARKBIT(block,n)				\
  (((block)->gcmarkbits[(n) / BITS_PER_BITS_WORD]	\
    >> ((n) % BITS_PER_BITS_WORD))			\
   & 1)

#define SETMARKBIT(block,n)				\
  ((block)->gcmarkbits[(n) / BITS_PER_BITS_WORD]	\
   |= (bits_word) 1 << ((n) % BITS_PER_BITS_WORD))

#define UNSETMARKBIT(block,n)				\
  ((block)->gcmarkbits[(n) / BITS_PER_BITS_WORD]	\
   &= ~((bits_word) 1 << ((n) % BITS_PER_BITS_WORD)))

#define FLOAT_BLOCK(fptr) \
  (eassert (!pdumper_object_p (fptr)),                                  \
   ((struct float_block *) (((uintptr_t) (fptr)) & ~(BLOCK_ALIGN - 1))))

#define FLOAT_INDEX(fptr) \
  ((((uintptr_t) (fptr)) & (BLOCK_ALIGN - 1)) / sizeof (struct Lisp_Float))

struct float_block
{
  /* Place `floats' at the beginning, to ease up FLOAT_INDEX's job.  */
  struct Lisp_Float floats[FLOAT_BLOCK_SIZE];
  bits_word gcmarkbits[1 + FLOAT_BLOCK_SIZE / BITS_PER_BITS_WORD];
  struct float_block *next;
};

#define XFLOAT_MARKED_P(fptr) \
  GETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX (fptr))

#define XFLOAT_MARK(fptr) \
  SETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX (fptr))

#define XFLOAT_UNMARK(fptr) \
  UNSETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX (fptr))

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_FLOAT_BLOCK(fblk)         \
  __asan_poison_memory_region ((fblk)->floats, \
			       sizeof ((fblk)->floats))
# define ASAN_UNPOISON_FLOAT_BLOCK(fblk)         \
  __asan_unpoison_memory_region ((fblk)->floats, \
				 sizeof ((fblk)->floats))
# define ASAN_POISON_FLOAT(p) \
  __asan_poison_memory_region (p, sizeof (struct Lisp_Float))
# define ASAN_UNPOISON_FLOAT(p) \
  __asan_unpoison_memory_region (p, sizeof (struct Lisp_Float))
#else
# define ASAN_POISON_FLOAT_BLOCK(fblk) ((void) 0)
# define ASAN_UNPOISON_FLOAT_BLOCK(fblk) ((void) 0)
# define ASAN_POISON_FLOAT(p) ((void) 0)
# define ASAN_UNPOISON_FLOAT(p) ((void) 0)
#endif

/* Current float_block.  */

static struct float_block *float_block;

/* Index of first unused Lisp_Float in the current float_block.  */

static int float_block_index = FLOAT_BLOCK_SIZE;

/* Free-list of Lisp_Floats.  */

static struct Lisp_Float *float_free_list;

/* Return a new float object with value FLOAT_VALUE.  */

Lisp_Object
make_float (double float_value)
{
  register Lisp_Object val;

  if (float_free_list)
    {
      XSETFLOAT (val, float_free_list);
      ASAN_UNPOISON_FLOAT (float_free_list);
      float_free_list = float_free_list->u.chain;
    }
  else
    {
      if (float_block_index == FLOAT_BLOCK_SIZE)
	{
	  struct float_block *new
	    = lisp_align_malloc (sizeof *new, MEM_TYPE_FLOAT);
	  new->next = float_block;
	  memset (new->gcmarkbits, 0, sizeof new->gcmarkbits);
	  ASAN_POISON_FLOAT_BLOCK (new);
	  float_block = new;
	  float_block_index = 0;
	}
      ASAN_UNPOISON_FLOAT (&float_block->floats[float_block_index]);
      XSETFLOAT (val, &float_block->floats[float_block_index]);
      float_block_index++;
    }

  XFLOAT_INIT (val, float_value);
  eassert (!XFLOAT_MARKED_P (XFLOAT (val)));
  tally_consing (sizeof (struct Lisp_Float));
  floats_consed++;
  return val;
}



/***********************************************************************
			   Cons Allocation
 ***********************************************************************/

/* We store cons cells inside of cons_blocks, allocating a new
   cons_block with malloc whenever necessary.  Cons cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new cons cells from the latest cons_block.  */

#define CONS_BLOCK_SIZE						\
  (((BLOCK_BYTES - sizeof (struct cons_block *)			\
     /* The compiler might add padding at the end.  */		\
     - (sizeof (struct Lisp_Cons) - sizeof (bits_word))) * CHAR_BIT)	\
   / (sizeof (struct Lisp_Cons) * CHAR_BIT + 1))

#define CONS_BLOCK(fptr) \
  (eassert (!pdumper_object_p (fptr)),                                  \
   ((struct cons_block *) ((uintptr_t) (fptr) & ~(BLOCK_ALIGN - 1))))

#define CONS_INDEX(fptr) \
  (((uintptr_t) (fptr) & (BLOCK_ALIGN - 1)) / sizeof (struct Lisp_Cons))

struct cons_block
{
  /* Place `conses' at the beginning, to ease up CONS_INDEX's job.  */
  struct Lisp_Cons conses[CONS_BLOCK_SIZE];
  bits_word gcmarkbits[1 + CONS_BLOCK_SIZE / BITS_PER_BITS_WORD];
  struct cons_block *next;
};

#define XCONS_MARKED_P(fptr) \
  GETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

#define XMARK_CONS(fptr) \
  SETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

#define XUNMARK_CONS(fptr) \
  UNSETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX (fptr))

/* Minimum number of bytes of consing since GC before next GC,
   when memory is full.  */

enum { memory_full_cons_threshold = sizeof (struct cons_block) };

/* Current cons_block.  */

static struct cons_block *cons_block;

/* Index of first unused Lisp_Cons in the current block.  */

static int cons_block_index = CONS_BLOCK_SIZE;

/* Free-list of Lisp_Cons structures.  */

static struct Lisp_Cons *cons_free_list;

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_CONS_BLOCK(b) \
  __asan_poison_memory_region ((b)->conses, sizeof ((b)->conses))
# define ASAN_POISON_CONS(p) \
  __asan_poison_memory_region (p, sizeof (struct Lisp_Cons))
# define ASAN_UNPOISON_CONS(p) \
  __asan_unpoison_memory_region (p, sizeof (struct Lisp_Cons))
#else
# define ASAN_POISON_CONS_BLOCK(b) ((void) 0)
# define ASAN_POISON_CONS(p) ((void) 0)
# define ASAN_UNPOISON_CONS(p) ((void) 0)
#endif

/* Explicitly free a cons cell by putting it on the free-list.  */

void
free_cons (struct Lisp_Cons *ptr)
{
  ptr->u.s.u.chain = cons_free_list;
  ptr->u.s.car = dead_object ();
  cons_free_list = ptr;
  ptrdiff_t nbytes = sizeof *ptr;
  tally_consing (-nbytes);
  ASAN_POISON_CONS (ptr);
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      ASAN_UNPOISON_CONS (cons_free_list);
      XSETCONS (val, cons_free_list);
      cons_free_list = cons_free_list->u.s.u.chain;
    }
  else
    {
      if (cons_block_index == CONS_BLOCK_SIZE)
	{
	  struct cons_block *new
	    = lisp_align_malloc (sizeof *new, MEM_TYPE_CONS);
	  memset (new->gcmarkbits, 0, sizeof new->gcmarkbits);
	  ASAN_POISON_CONS_BLOCK (new);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	}
      ASAN_UNPOISON_CONS (&cons_block->conses[cons_block_index]);
      XSETCONS (val, &cons_block->conses[cons_block_index]);
      cons_block_index++;
    }

  XSETCAR (val, car);
  XSETCDR (val, cdr);
  eassert (!XCONS_MARKED_P (XCONS (val)));
  consing_until_gc -= sizeof (struct Lisp_Cons);
  cons_cells_consed++;
  return val;
}

/* Make a list of 1, 2, 3, 4 or 5 specified objects.  */

Lisp_Object
list1 (Lisp_Object arg1)
{
  return Fcons (arg1, Qnil);
}

Lisp_Object
list2 (Lisp_Object arg1, Lisp_Object arg2)
{
  return Fcons (arg1, Fcons (arg2, Qnil));
}


Lisp_Object
list3 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Qnil)));
}

Lisp_Object
list4 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3, Lisp_Object arg4)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4, Qnil))));
}

Lisp_Object
list5 (Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3, Lisp_Object arg4,
       Lisp_Object arg5)
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4,
						       Fcons (arg5, Qnil)))));
}

/* Make a list of COUNT Lisp_Objects, where ARG is the first one.
   AP has any remaining args.  */
static Lisp_Object
cons_listn (ptrdiff_t count, Lisp_Object arg, va_list ap)
{
  eassume (0 < count);
  Lisp_Object val = Fcons (arg, Qnil);
  Lisp_Object tail = val;
  for (ptrdiff_t i = 1; i < count; i++)
    {
      Lisp_Object elem = Fcons (va_arg (ap, Lisp_Object), Qnil);
      XSETCDR (tail, elem);
      tail = elem;
    }
  return val;
}

/* Make a list of COUNT Lisp_Objects, where ARG1 is the first one.  */
Lisp_Object
listn (ptrdiff_t count, Lisp_Object arg1, ...)
{
  va_list ap;
  va_start (ap, arg1);
  Lisp_Object val = cons_listn (count, arg1, ap);
  va_end (ap);
  return val;
}

DEFUN ("list", Flist, Slist, 0, MANY, 0,
       doc: /* Return a newly created list with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (list &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  register Lisp_Object val;
  val = Qnil;

  while (nargs > 0)
    {
      nargs--;
      val = Fcons (args[nargs], val);
    }
  return val;
}


DEFUN ("make-list", Fmake_list, Smake_list, 2, 2, 0,
       doc: /* Return a newly created list of length LENGTH, with each element being INIT.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val = Qnil;
  CHECK_FIXNAT (length);

  for (EMACS_INT size = XFIXNAT (length); 0 < size; size--)
    {
      val = Fcons (init, val);
      rarely_quit (size);
    }

  return val;
}



/***********************************************************************
			   Vector Allocation
 ***********************************************************************/

/* Sometimes a vector's contents are merely a pointer internally used
   in vector allocation code.  On the rare platforms where a null
   pointer cannot be tagged, represent it with a Lisp 0.
   Usually you don't want to touch this.  */

static struct Lisp_Vector *
next_vector (struct Lisp_Vector *v)
{
  return XUNTAG (v->contents[0], Lisp_Int0, struct Lisp_Vector);
}

static void
set_next_vector (struct Lisp_Vector *v, struct Lisp_Vector *p)
{
  v->contents[0] = make_lisp_ptr (p, Lisp_Int0);
}

/* This value is balanced well enough to avoid too much internal overhead
   for the most common cases; it's not required to be a power of two, but
   it's expected to be a mult-of-ROUNDUP_SIZE (see below).  */

enum { VECTOR_BLOCK_SIZE = 4096 };

/* Vector size requests are a multiple of this.  */
enum { roundup_size = COMMON_MULTIPLE (LISP_ALIGNMENT, word_size) };

/* Verify assumption described above.  */
static_assert (VECTOR_BLOCK_SIZE % roundup_size == 0);

/* Round up X to nearest mult-of-ROUNDUP_SIZE --- use at compile time.  */
#define vroundup_ct(x) ROUNDUP (x, roundup_size)
/* Round up X to nearest mult-of-ROUNDUP_SIZE --- use at runtime.  */
#define vroundup(x) (eassume ((x) >= 0), vroundup_ct (x))

/* Rounding helps to maintain alignment constraints if USE_LSB_TAG.  */

enum {VECTOR_BLOCK_BYTES = VECTOR_BLOCK_SIZE - vroundup_ct (sizeof (void *))};

/* The current code expects to be able to represent an unused block by
   a single PVEC_FREE object, whose size is limited by the header word.
   (Of course we could use multiple such objects.)  */
static_assert (VECTOR_BLOCK_BYTES <= (word_size << PSEUDOVECTOR_REST_BITS));

/* Size of the minimal vector allocated from block.  */

enum { VBLOCK_BYTES_MIN = vroundup_ct (header_size + sizeof (Lisp_Object)) };

/* Size of the largest vector allocated from block.  */

enum { VBLOCK_BYTES_MAX = vroundup_ct ((VECTOR_BLOCK_BYTES / 2) - word_size) };

/* We maintain one free list for each possible block-allocated
   vector size, one for blocks one word bigger,
   and one for all free vectors larger than that.  */
enum { VECTOR_FREE_LIST_ARRAY_SIZE =
       (VBLOCK_BYTES_MAX - VBLOCK_BYTES_MIN) / roundup_size + 1 + 2 };

/* Common shortcut to advance vector pointer over a block data.  */

static struct Lisp_Vector *
ADVANCE (struct Lisp_Vector *v, ptrdiff_t nbytes)
{
  void *vv = v;
  char *cv = vv;
  void *p = cv + nbytes;
  return p;
}

/* Common shortcut to calculate NBYTES-vector index in VECTOR_FREE_LISTS.  */

static ptrdiff_t
VINDEX (ptrdiff_t nbytes)
{
  eassume (VBLOCK_BYTES_MIN <= nbytes);
  return (nbytes - VBLOCK_BYTES_MIN) / roundup_size;
}

/* This internal type is used to maintain the list of large vectors
   which are allocated at their own, e.g. outside of vector blocks.

   struct large_vector itself cannot contain a struct Lisp_Vector, as
   the latter contains a flexible array member and C99 does not allow
   such structs to be nested.  Instead, each struct large_vector
   object LV is followed by a struct Lisp_Vector, which is at offset
   large_vector_offset from LV, and whose address is therefore
   large_vector_vec (&LV).  */

struct large_vector
{
  struct large_vector *next;
};

enum
{
  large_vector_offset = ROUNDUP (sizeof (struct large_vector), LISP_ALIGNMENT)
};

static struct Lisp_Vector *
large_vector_vec (struct large_vector *p)
{
  return (struct Lisp_Vector *) ((char *) p + large_vector_offset);
}

/* This internal type is used to maintain an underlying storage
   for small vectors.  */

struct vector_block
{
  char data[VECTOR_BLOCK_BYTES];
  struct vector_block *next;
};

/* Chain of vector blocks.  */

static struct vector_block *vector_blocks;

/* Vector free lists, where NTH item points to a chain of free
   vectors of the same NBYTES size, so NTH == VINDEX (NBYTES),
   except for the last element which may contain larger vectors.

   I.e., for each vector V in vector_free_lists[I] the following holds:
   - V has type PVEC_FREE
   - V's total size in bytes, BS(V) = PVSIZE(V) * word_size + header_size
   - For I < VECTOR_FREE_LIST_ARRAY_SIZE-1, VINDEX(BS(V)) = I
   - For I = VECTOR_FREE_LIST_ARRAY_SIZE-1, VINDEX(BS(V)) ≥ I */
static struct Lisp_Vector *vector_free_lists[VECTOR_FREE_LIST_ARRAY_SIZE];

/* Index to the bucket in vector_free_lists into which we last inserted
   or split a free vector.  We use this as a heuristic telling us where
   to start looking for free vectors when the exact-size bucket is empty.  */
static ptrdiff_t last_inserted_vector_free_idx = VECTOR_FREE_LIST_ARRAY_SIZE;

/* Singly-linked list of large vectors.  */

static struct large_vector *large_vectors;

/* The only vector with 0 slots.  */

Lisp_Object zero_vector;

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_VECTOR_CONTENTS(v, bytes) \
  __asan_poison_memory_region ((v)->contents, bytes)
# define ASAN_UNPOISON_VECTOR_CONTENTS(v, bytes) \
  __asan_unpoison_memory_region ((v)->contents, bytes)
# define ASAN_UNPOISON_VECTOR_BLOCK(b) \
  __asan_unpoison_memory_region ((b)->data, sizeof (b)->data)
#else
# define ASAN_POISON_VECTOR_CONTENTS(v, bytes) ((void) 0)
# define ASAN_UNPOISON_VECTOR_CONTENTS(v, bytes) ((void) 0)
# define ASAN_UNPOISON_VECTOR_BLOCK(b) ((void) 0)
#endif

/* Common shortcut to setup vector on a free list.  */

static void
setup_on_free_list (struct Lisp_Vector *v, ptrdiff_t nbytes)
{
  eassume (header_size <= nbytes);
  ptrdiff_t nwords = (nbytes - header_size) / word_size;
  XSETPVECTYPESIZE (v, PVEC_FREE, 0, nwords);
  eassert (nbytes % roundup_size == 0);
  ptrdiff_t vindex = VINDEX (nbytes);
  /* Anything too large goes into the last slot (overflow bin).  */
  vindex = min(vindex, VECTOR_FREE_LIST_ARRAY_SIZE - 1);
  set_next_vector (v, vector_free_lists[vindex]);
  ASAN_POISON_VECTOR_CONTENTS (v, nbytes - header_size);
  vector_free_lists[vindex] = v;
  last_inserted_vector_free_idx = vindex;
}

/* Get a new vector block.  */

static struct vector_block *
allocate_vector_block (void)
{
  struct vector_block *block = xmalloc (sizeof *block);

#ifndef GC_MALLOC_CHECK
  mem_insert (block->data, block->data + VECTOR_BLOCK_BYTES,
	      MEM_TYPE_VECTOR_BLOCK);
#endif

  block->next = vector_blocks;
  vector_blocks = block;
  return block;
}

static struct Lisp_Vector *
allocate_vector_from_block (ptrdiff_t nbytes);

/* Memory footprint in bytes of a pseudovector other than a bool-vector.  */
static ptrdiff_t
pseudovector_nbytes (const union vectorlike_header *hdr)
{
  eassert (!PSEUDOVECTOR_TYPEP (hdr, PVEC_BOOL_VECTOR));
  ptrdiff_t nwords = ((hdr->size & PSEUDOVECTOR_SIZE_MASK)
		      + ((hdr->size & PSEUDOVECTOR_REST_MASK)
			 >> PSEUDOVECTOR_SIZE_BITS));
  return vroundup (header_size + word_size * nwords);
}

/* Called once to initialize vector allocation.  */

static void
init_vectors (void)
{
  /* The normal vector allocation code refuses to allocate a 0-length vector
     because we use the first field of vectors internally when they're on
     the free list, so we can't put a zero-length vector on the free list.
     This is not a problem for 'zero_vector' since it's always reachable.
     An alternative approach would be to allocate zero_vector outside of the
     normal heap, e.g. as a static object, and then to "hide" it from the GC,
     for example by marking it by hand at the beginning of the GC and unmarking
     it by hand at the end.  */
  struct vector_block *block = allocate_vector_block ();
  struct Lisp_Vector *zv = (struct Lisp_Vector *)block->data;
  zv->header.size = 0;
  ssize_t nbytes = pseudovector_nbytes (&zv->header);
  ssize_t restbytes = VECTOR_BLOCK_BYTES - nbytes;
  eassert (restbytes % roundup_size == 0);
  setup_on_free_list (ADVANCE (zv, nbytes), restbytes);

  zero_vector = make_lisp_ptr (zv, Lisp_Vectorlike);
  staticpro (&zero_vector);
}

/* Allocate vector from a vector block.  */

static struct Lisp_Vector *
allocate_vector_from_block (ptrdiff_t nbytes)
{
  struct Lisp_Vector *vector;
  struct vector_block *block;
  size_t index, restbytes;

  eassume (VBLOCK_BYTES_MIN <= nbytes && nbytes <= VBLOCK_BYTES_MAX);
  eassume (nbytes % roundup_size == 0);

  /* First, try to allocate from a free list
     containing vectors of the requested size.  */
  index = VINDEX (nbytes);
  if (vector_free_lists[index])
    {
      vector = vector_free_lists[index];
      ASAN_UNPOISON_VECTOR_CONTENTS (vector, nbytes - header_size);
      vector_free_lists[index] = next_vector (vector);
      return vector;
    }

  /* Next, check free lists containing larger vectors.  Since
     we will split the result, we should have remaining space
     large enough to use for one-slot vector at least.  */
  for (index = max (VINDEX (nbytes + VBLOCK_BYTES_MIN),
		    last_inserted_vector_free_idx);
       index < VECTOR_FREE_LIST_ARRAY_SIZE; index++)
    if (vector_free_lists[index])
      {
	/* This vector is larger than requested.  */
	vector = vector_free_lists[index];
	size_t vector_nbytes = pseudovector_nbytes (&vector->header);
	eassert (vector_nbytes > nbytes);
	ASAN_UNPOISON_VECTOR_CONTENTS (vector, nbytes - header_size);
	vector_free_lists[index] = next_vector (vector);

	/* Excess bytes are used for the smaller vector,
	   which should be set on an appropriate free list.  */
	restbytes = vector_nbytes - nbytes;
	eassert (restbytes % roundup_size == 0);
#if GC_ASAN_POISON_OBJECTS
	/* Ensure that accessing excess bytes does not trigger ASan.  */
	__asan_unpoison_memory_region (ADVANCE (vector, nbytes),
				       restbytes);
#endif
	setup_on_free_list (ADVANCE (vector, nbytes), restbytes);
	return vector;
      }

  /* Finally, need a new vector block.  */
  block = allocate_vector_block ();

  /* New vector will be at the beginning of this block.  */
  vector = (struct Lisp_Vector *) block->data;

  /* If the rest of space from this block is large enough
     for one-slot vector at least, set up it on a free list.  */
  restbytes = VECTOR_BLOCK_BYTES - nbytes;
  if (restbytes >= VBLOCK_BYTES_MIN)
    {
      eassert (restbytes % roundup_size == 0);
      setup_on_free_list (ADVANCE (vector, nbytes), restbytes);
    }
  return vector;
}

/* Nonzero if VECTOR pointer is valid pointer inside BLOCK.  */

#define VECTOR_IN_BLOCK(vector, block)		\
  ((char *) (vector) <= (block)->data		\
   + VECTOR_BLOCK_BYTES - VBLOCK_BYTES_MIN)

/* Return the memory footprint of V in bytes.  */

ptrdiff_t
vectorlike_nbytes (const union vectorlike_header *hdr)
{
  ptrdiff_t size = hdr->size & ~ARRAY_MARK_FLAG;
  ptrdiff_t nwords;

  if (size & PSEUDOVECTOR_FLAG)
    {
      if (PSEUDOVECTOR_TYPEP (hdr, PVEC_BOOL_VECTOR))
        {
          struct Lisp_Bool_Vector *bv = (struct Lisp_Bool_Vector *) hdr;
	  ptrdiff_t word_bytes = (bool_vector_words (bv->size)
				  * sizeof (bits_word));
	  ptrdiff_t boolvec_bytes = bool_header_size + word_bytes;
	  static_assert (header_size <= bool_header_size);
	  nwords = (boolvec_bytes - header_size + word_size - 1) / word_size;
        }
      else
	return pseudovector_nbytes (hdr);
    }
  else
    nwords = size;
  return vroundup (header_size + word_size * nwords);
}

/* Convert a pseudovector pointer P to its underlying struct T pointer.
   Verify that the struct is small, since cleanup_vector is called
   only on small vector-like objects.  */

#define PSEUDOVEC_STRUCT(p, t) \
  verify_expr ((header_size + VECSIZE (struct t) * word_size \
		<= VBLOCK_BYTES_MAX), \
	       (struct t *) (p))

/* Release extra resources still in use by VECTOR, which may be any
   small vector-like object.  */

static void
cleanup_vector (struct Lisp_Vector *vector)
{
  if ((vector->header.size & PSEUDOVECTOR_FLAG) == 0)
    return;  /* nothing more to do for plain vectors */
  switch (PSEUDOVECTOR_TYPE (vector))
    {
    case PVEC_BIGNUM:
      mpz_clear (PSEUDOVEC_STRUCT (vector, Lisp_Bignum)->value);
      break;
    case PVEC_OVERLAY:
      {
	struct Lisp_Overlay *ol = PSEUDOVEC_STRUCT (vector, Lisp_Overlay);
	xfree (ol->interval);
      }
      break;
    case PVEC_FINALIZER:
      unchain_finalizer (PSEUDOVEC_STRUCT (vector, Lisp_Finalizer));
      break;
    case PVEC_FONT:
      {
	if ((vector->header.size & PSEUDOVECTOR_SIZE_MASK) == FONT_OBJECT_MAX)
	  {
	    struct font *font = PSEUDOVEC_STRUCT (vector, font);
	    struct font_driver const *drv = font->driver;

	    /* The font driver might sometimes be NULL, e.g. if Emacs was
	       interrupted before it had time to set it up.  */
	    if (drv)
	      {
		/* Attempt to catch subtle bugs like Bug#16140.  */
		eassert (valid_font_driver (drv));
		drv->close_font (font);
	      }
	  }

#if defined HAVE_ANDROID && !defined ANDROID_STUBIFY
	/* The Android font driver needs the ability to associate extra
	   information with font entities.  */
	if (((vector->header.size & PSEUDOVECTOR_SIZE_MASK)
	     == FONT_ENTITY_MAX)
	    && PSEUDOVEC_STRUCT (vector, font_entity)->is_android)
	  android_finalize_font_entity (PSEUDOVEC_STRUCT (vector, font_entity));
#endif
      }
      break;
    case PVEC_THREAD:
      finalize_one_thread (PSEUDOVEC_STRUCT (vector, thread_state));
      break;
    case PVEC_MUTEX:
      finalize_one_mutex (PSEUDOVEC_STRUCT (vector, Lisp_Mutex));
      break;
    case PVEC_CONDVAR:
      finalize_one_condvar (PSEUDOVEC_STRUCT (vector, Lisp_CondVar));
      break;
    case PVEC_MARKER:
      /* sweep_buffer should already have unchained this from its buffer.  */
      eassert (! PSEUDOVEC_STRUCT (vector, Lisp_Marker)->buffer);
      break;
    case PVEC_USER_PTR:
      {
	struct Lisp_User_Ptr *uptr = PSEUDOVEC_STRUCT (vector, Lisp_User_Ptr);
	if (uptr->finalizer)
	  uptr->finalizer (uptr->p);
      }
      break;
    case PVEC_TS_PARSER:
#ifdef HAVE_TREE_SITTER
      treesit_delete_parser (PSEUDOVEC_STRUCT (vector, Lisp_TS_Parser));
#endif
      break;
    case PVEC_TS_COMPILED_QUERY:
#ifdef HAVE_TREE_SITTER
      treesit_delete_query (PSEUDOVEC_STRUCT (vector, Lisp_TS_Query));
#endif
      break;
    case PVEC_MODULE_FUNCTION:
#ifdef HAVE_MODULES
      {
	ATTRIBUTE_MAY_ALIAS struct Lisp_Module_Function *function
	  = (struct Lisp_Module_Function *) vector;
	module_finalize_function (function);
      }
#endif
      break;
    case PVEC_NATIVE_COMP_UNIT:
#ifdef HAVE_NATIVE_COMP
      {
	struct Lisp_Native_Comp_Unit *cu =
	  PSEUDOVEC_STRUCT (vector, Lisp_Native_Comp_Unit);
	unload_comp_unit (cu);
      }
#endif
      break;
    case PVEC_SUBR:
#ifdef HAVE_NATIVE_COMP
      {
	struct Lisp_Subr *subr = PSEUDOVEC_STRUCT (vector, Lisp_Subr);
	if (!NILP (subr->native_comp_u))
	  {
	    /* FIXME Alternative and non invasive solution to this cast?  */
	    xfree ((char *)subr->symbol_name);
	    xfree (subr->native_c_name);
	  }
      }
#endif
      break;
    case PVEC_HASH_TABLE:
      {
	struct Lisp_Hash_Table *h = PSEUDOVEC_STRUCT (vector, Lisp_Hash_Table);
	if (h->table_size > 0)
	  {
	    eassert (h->index_bits > 0);
	    xfree (h->index);
	    xfree (h->key_and_value);
	    xfree (h->next);
	    xfree (h->hash);
	    ptrdiff_t bytes = (h->table_size * (2 * sizeof *h->key_and_value
						+ sizeof *h->hash
						+ sizeof *h->next)
			       + hash_table_index_size (h) * sizeof *h->index);
	    hash_table_allocated_bytes -= bytes;
	  }
      }
      break;
    case PVEC_OBARRAY:
      {
	struct Lisp_Obarray *o = PSEUDOVEC_STRUCT (vector, Lisp_Obarray);
	xfree (o->buckets);
	ptrdiff_t bytes = obarray_size (o) * sizeof *o->buckets;
	hash_table_allocated_bytes -= bytes;
      }
      break;
    /* Keep the switch exhaustive.  */
    case PVEC_NORMAL_VECTOR:
    case PVEC_FREE:
    case PVEC_SYMBOL_WITH_POS:
    case PVEC_MISC_PTR:
    case PVEC_PROCESS:
    case PVEC_FRAME:
    case PVEC_WINDOW:
    case PVEC_BOOL_VECTOR:
    case PVEC_BUFFER:
    case PVEC_TERMINAL:
    case PVEC_WINDOW_CONFIGURATION:
    case PVEC_OTHER:
    case PVEC_XWIDGET:
    case PVEC_XWIDGET_VIEW:
    case PVEC_TS_NODE:
    case PVEC_SQLITE:
    case PVEC_CLOSURE:
    case PVEC_CHAR_TABLE:
    case PVEC_SUB_CHAR_TABLE:
    case PVEC_RECORD:
      break;
    }
}

/* Reclaim space used by unmarked vectors.  */

NO_INLINE /* For better stack traces */
static void
sweep_vectors (void)
{
  struct vector_block *block, **bprev = &vector_blocks;
  struct large_vector *lv, **lvprev = &large_vectors;
  struct Lisp_Vector *vector, *next;

  gcstat.total_vectors = 0;
  gcstat.total_vector_slots = gcstat.total_free_vector_slots = 0;
  memset (vector_free_lists, 0, sizeof (vector_free_lists));
  last_inserted_vector_free_idx = VECTOR_FREE_LIST_ARRAY_SIZE;

  /* Looking through vector blocks.  */

  for (block = vector_blocks; block; block = *bprev)
    {
      bool free_this_block = false;

      for (vector = (struct Lisp_Vector *) block->data;
	   VECTOR_IN_BLOCK (vector, block); vector = next)
	{
	  ASAN_UNPOISON_VECTOR_BLOCK (block);
	  if (XVECTOR_MARKED_P (vector))
	    {
	      XUNMARK_VECTOR (vector);
	      gcstat.total_vectors++;
	      ptrdiff_t nbytes = vector_nbytes (vector);
	      gcstat.total_vector_slots += nbytes / word_size;
	      next = ADVANCE (vector, nbytes);
	    }
	  else
	    {
	      ptrdiff_t total_bytes = 0;

	      /* While NEXT is not marked, try to coalesce with VECTOR,
		 thus making VECTOR of the largest possible size.  */

	      next = vector;
	      do
		{
		  cleanup_vector (next);
		  ptrdiff_t nbytes = vector_nbytes (next);
		  total_bytes += nbytes;
		  next = ADVANCE (next, nbytes);
		}
	      while (VECTOR_IN_BLOCK (next, block) && !vector_marked_p (next));

	      eassert (total_bytes % roundup_size == 0);

	      if (vector == (struct Lisp_Vector *) block->data
		  && !VECTOR_IN_BLOCK (next, block))
		/* This block should be freed because all of its
		   space was coalesced into the only free vector.  */
		free_this_block = true;
	      else
		{
		  setup_on_free_list (vector, total_bytes);
		  gcstat.total_free_vector_slots += total_bytes / word_size;
		}
	    }
	}

      if (free_this_block)
	{
	  *bprev = block->next;
#ifndef GC_MALLOC_CHECK
	  mem_delete (mem_find (block->data));
#endif
	  xfree (block);
	}
      else
	bprev = &block->next;
    }

  /* Sweep large vectors.  */

  for (lv = large_vectors; lv; lv = *lvprev)
    {
      vector = large_vector_vec (lv);
      if (XVECTOR_MARKED_P (vector))
	{
	  XUNMARK_VECTOR (vector);
	  gcstat.total_vectors++;
	  gcstat.total_vector_slots
	    += (vector->header.size & PSEUDOVECTOR_FLAG
		? vector_nbytes (vector) / word_size
		: header_size / word_size + vector->header.size);
	  lvprev = &lv->next;
	}
      else
	{
	  *lvprev = lv->next;
	  lisp_free (lv);
	}
    }

  gcstat.total_hash_table_bytes = hash_table_allocated_bytes;
}

/* Maximum number of elements in a vector.  This is a macro so that it
   can be used in an integer constant expression.  */

#define VECTOR_ELTS_MAX \
  ((ptrdiff_t) \
   min (((min (PTRDIFF_MAX, SIZE_MAX) - header_size - large_vector_offset) \
	 / word_size), \
	MOST_POSITIVE_FIXNUM))

/* Value is a pointer to a newly allocated Lisp_Vector structure
   with room for LEN Lisp_Objects.  LEN must be positive and
   at most VECTOR_ELTS_MAX.  */

static struct Lisp_Vector *
allocate_vectorlike (ptrdiff_t len, bool clearit)
{
  eassert (0 < len && len <= VECTOR_ELTS_MAX);
  ptrdiff_t nbytes = header_size + len * word_size;
  struct Lisp_Vector *p;

#ifdef DOUG_LEA_MALLOC
  if (!mmap_lisp_allowed_p ())
    mallopt (M_MMAP_MAX, 0);
#endif

  if (nbytes <= VBLOCK_BYTES_MAX)
    {
      p = allocate_vector_from_block (vroundup (nbytes));
      if (clearit)
	memclear (p, nbytes);
    }
  else
    {
      struct large_vector *lv = lisp_malloc (large_vector_offset + nbytes,
					     clearit, MEM_TYPE_VECTORLIKE);
      lv->next = large_vectors;
      large_vectors = lv;
      p = large_vector_vec (lv);
    }

#ifdef DOUG_LEA_MALLOC
  if (!mmap_lisp_allowed_p ())
    mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif

  tally_consing (nbytes);
  vector_cells_consed += len;

  return p;
}


/* Allocate a vector with LEN slots.  If CLEARIT, clear its slots;
   otherwise the vector's slots are uninitialized.  */

static struct Lisp_Vector *
allocate_clear_vector (ptrdiff_t len, bool clearit)
{
  if (len == 0)
    return XVECTOR (zero_vector);
  if (VECTOR_ELTS_MAX < len)
    memory_full (SIZE_MAX);
  struct Lisp_Vector *v = allocate_vectorlike (len, clearit);
  v->header.size = len;
  return v;
}

/* Allocate a vector with LEN uninitialized slots.  */

struct Lisp_Vector *
allocate_vector (ptrdiff_t len)
{
  return allocate_clear_vector (len, false);
}

/* Allocate a vector with LEN nil slots.  */

struct Lisp_Vector *
allocate_nil_vector (ptrdiff_t len)
{
  return allocate_clear_vector (len, true);
}


/* Allocate other vector-like structures.  */

struct Lisp_Vector *
allocate_pseudovector (int memlen, int lisplen,
		       int zerolen, enum pvec_type tag)
{
  /* Catch bogus values.  */
  enum { size_max = (1 << PSEUDOVECTOR_SIZE_BITS) - 1 };
  enum { rest_max = (1 << PSEUDOVECTOR_REST_BITS) - 1 };
  static_assert (size_max + rest_max <= VECTOR_ELTS_MAX);
  eassert (0 <= tag && tag <= PVEC_TAG_MAX);
  eassert (0 <= lisplen && lisplen <= zerolen && zerolen <= memlen);
  eassert (lisplen <= size_max);
  eassert (memlen <= size_max + rest_max);

  struct Lisp_Vector *v = allocate_vectorlike (memlen, false);
  /* Only the first LISPLEN slots will be traced normally by the GC.  */
  memclear (v->contents, zerolen * word_size);
  XSETPVECTYPESIZE (v, tag, lisplen, memlen - lisplen);
  return v;
}

struct buffer *
allocate_buffer (void)
{
  struct buffer *b
    = ALLOCATE_PSEUDOVECTOR (struct buffer, cursor_in_non_selected_windows_,
			     PVEC_BUFFER);
  BUFFER_PVEC_INIT (b);
  /* Note that the rest fields of B are not initialized.  */
  return b;
}


/* Allocate a record with COUNT slots.  COUNT must be positive, and
   includes the type slot.  */

static struct Lisp_Vector *
allocate_record (EMACS_INT count)
{
  if (count > PSEUDOVECTOR_SIZE_MASK)
    error ("Attempt to allocate a record of %"pI"d slots; max is %d",
	   count, PSEUDOVECTOR_SIZE_MASK);
  struct Lisp_Vector *p = allocate_vectorlike (count, false);
  p->header.size = count;
  XSETPVECTYPE (p, PVEC_RECORD);
  return p;
}


DEFUN ("make-record", Fmake_record, Smake_record, 3, 3, 0,
       doc: /* Create a new record.
TYPE is its type as returned by `type-of'; it should be either a
symbol or a type descriptor.  SLOTS is the number of non-type slots,
each initialized to INIT.  */)
  (Lisp_Object type, Lisp_Object slots, Lisp_Object init)
{
  CHECK_FIXNAT (slots);
  EMACS_INT size = XFIXNAT (slots) + 1;
  struct Lisp_Vector *p = allocate_record (size);
  p->contents[0] = type;
  for (ptrdiff_t i = 1; i < size; i++)
    p->contents[i] = init;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}


DEFUN ("record", Frecord, Srecord, 1, MANY, 0,
       doc: /* Create a new record.
TYPE is its type as returned by `type-of'; it should be either a
symbol or a type descriptor.  SLOTS is used to initialize the record
slots with shallow copies of the arguments.
usage: (record TYPE &rest SLOTS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct Lisp_Vector *p = allocate_record (nargs);
  memcpy (p->contents, args, nargs * sizeof *args);
  return make_lisp_ptr (p, Lisp_Vectorlike);
}


DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
       doc: /* Return a newly created vector of length LENGTH, with each element being INIT.
See also the function `vector'.  */)
  (Lisp_Object length, Lisp_Object init)
{
  CHECK_TYPE (FIXNATP (length) && XFIXNAT (length) <= PTRDIFF_MAX,
	      Qwholenump, length);
  return make_vector (XFIXNAT (length), init);
}

/* Return a new vector of length LENGTH with each element being INIT.  */

Lisp_Object
make_vector (ptrdiff_t length, Lisp_Object init)
{
  bool clearit = NIL_IS_ZERO && NILP (init);
  struct Lisp_Vector *p = allocate_clear_vector (length, clearit);
  if (!clearit)
    for (ptrdiff_t i = 0; i < length; i++)
      p->contents[i] = init;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
       doc: /* Return a newly created vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object val = make_uninit_vector (nargs);
  struct Lisp_Vector *p = XVECTOR (val);
  memcpy (p->contents, args, nargs * sizeof *args);
  return val;
}

DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
       doc: /* Create a byte-code object with specified arguments as elements.
The arguments should be the ARGLIST, bytecode-string BYTE-CODE, constant
vector CONSTANTS, maximum stack size DEPTH, (optional) DOCSTRING,
and (optional) INTERACTIVE-SPEC.
The first four arguments are required; at most six have any
significance.
The ARGLIST can be either like the one of `lambda', in which case the arguments
will be dynamically bound before executing the byte code, or it can be an
integer of the form NNNNNNNRMMMMMMM where the 7bit MMMMMMM specifies the
minimum number of arguments, the 7-bit NNNNNNN specifies the maximum number
of arguments (ignoring &rest) and the R bit specifies whether there is a &rest
argument to catch the left-over arguments.  If such an integer is used, the
arguments will not be dynamically bound but will be instead pushed on the
stack before executing the byte-code.
usage: (make-byte-code ARGLIST BYTE-CODE CONSTANTS DEPTH &optional DOCSTRING INTERACTIVE-SPEC &rest ELEMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  if (! ((FIXNUMP (args[CLOSURE_ARGLIST])
	  || CONSP (args[CLOSURE_ARGLIST])
	  || NILP (args[CLOSURE_ARGLIST]))
	 && STRINGP (args[CLOSURE_CODE])
	 && !STRING_MULTIBYTE (args[CLOSURE_CODE])
	 && VECTORP (args[CLOSURE_CONSTANTS])
	 && FIXNATP (args[CLOSURE_STACK_DEPTH])))
    error ("Invalid byte-code object");

  /* Bytecode must be immovable.  */
  pin_string (args[CLOSURE_CODE]);

  Lisp_Object val = Fvector (nargs, args);
  XSETPVECTYPE (XVECTOR (val), PVEC_CLOSURE);
  return val;
}

DEFUN ("make-closure", Fmake_closure, Smake_closure, 1, MANY, 0,
       doc: /* Create a byte-code closure from PROTOTYPE and CLOSURE-VARS.
Return a copy of PROTOTYPE, a byte-code object, with CLOSURE-VARS
replacing the elements in the beginning of the constant-vector.
usage: (make-closure PROTOTYPE &rest CLOSURE-VARS) */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object protofun = args[0];
  CHECK_TYPE (CLOSUREP (protofun), Qbyte_code_function_p, protofun);

  /* Create a copy of the constant vector, filling it with the closure
     variables in the beginning.  (The overwritten part should just
     contain placeholder values.) */
  Lisp_Object proto_constvec = AREF (protofun, CLOSURE_CONSTANTS);
  ptrdiff_t constsize = ASIZE (proto_constvec);
  ptrdiff_t nvars = nargs - 1;
  if (nvars > constsize)
    error ("Closure vars do not fit in constvec");
  Lisp_Object constvec = make_uninit_vector (constsize);
  memcpy (XVECTOR (constvec)->contents, args + 1, nvars * word_size);
  memcpy (XVECTOR (constvec)->contents + nvars,
	  XVECTOR (proto_constvec)->contents + nvars,
	  (constsize - nvars) * word_size);

  /* Return a copy of the prototype function with the new constant vector. */
  ptrdiff_t protosize = PVSIZE (protofun);
  struct Lisp_Vector *v = allocate_vectorlike (protosize, false);
  v->header = XVECTOR (protofun)->header;
  memcpy (v->contents, XVECTOR (protofun)->contents, protosize * word_size);
  v->contents[CLOSURE_CONSTANTS] = constvec;
  return make_lisp_ptr (v, Lisp_Vectorlike);
}


/***********************************************************************
			   Symbol Allocation
 ***********************************************************************/

/* Each symbol_block is just under 1020 bytes long, since malloc
   really allocates in units of powers of two and uses 4 bytes for its
   own overhead.  */

#define SYMBOL_BLOCK_SIZE \
  ((1020 - sizeof (struct symbol_block *)) / sizeof (struct Lisp_Symbol))

struct symbol_block
{
  /* Place `symbols' first, to preserve alignment.  */
  struct Lisp_Symbol symbols[SYMBOL_BLOCK_SIZE];
  struct symbol_block *next;
};

#if GC_ASAN_POISON_OBJECTS
# define ASAN_POISON_SYMBOL_BLOCK(s) \
  __asan_poison_memory_region ((s)->symbols, sizeof ((s)->symbols))
# define ASAN_UNPOISON_SYMBOL_BLOCK(s) \
  __asan_unpoison_memory_region ((s)->symbols, sizeof ((s)->symbols))
# define ASAN_POISON_SYMBOL(sym) \
  __asan_poison_memory_region (sym, sizeof *(sym))
# define ASAN_UNPOISON_SYMBOL(sym) \
  __asan_unpoison_memory_region (sym, sizeof *(sym))

#else
# define ASAN_POISON_SYMBOL_BLOCK(s) ((void) 0)
# define ASAN_UNPOISON_SYMBOL_BLOCK(s) ((void) 0)
# define ASAN_POISON_SYMBOL(sym) ((void) 0)
# define ASAN_UNPOISON_SYMBOL(sym) ((void) 0)
#endif

/* Current symbol block and index of first unused Lisp_Symbol
   structure in it.  */

static struct symbol_block *symbol_block;
static int symbol_block_index = SYMBOL_BLOCK_SIZE;

/* List of free symbols.  */

static struct Lisp_Symbol *symbol_free_list;

static void
set_symbol_name (Lisp_Object sym, Lisp_Object name)
{
  XBARE_SYMBOL (sym)->u.s.name = name;
}

void
init_symbol (Lisp_Object val, Lisp_Object name)
{
  struct Lisp_Symbol *p = XBARE_SYMBOL (val);
  set_symbol_name (val, name);
  set_symbol_plist (val, Qnil);
  p->u.s.redirect = SYMBOL_PLAINVAL;
  SET_SYMBOL_VAL (p, Qunbound);
  set_symbol_function (val, Qnil);
  set_symbol_next (val, NULL);
  p->u.s.gcmarkbit = false;
  p->u.s.interned = SYMBOL_UNINTERNED;
  p->u.s.trapped_write = SYMBOL_UNTRAPPED_WRITE;
  p->u.s.declared_special = false;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return a newly allocated uninterned symbol whose name is NAME.
Its value is void, and its function definition and property list are nil.  */)
  (Lisp_Object name)
{
  Lisp_Object val;

  CHECK_STRING (name);

  if (symbol_free_list)
    {
      ASAN_UNPOISON_SYMBOL (symbol_free_list);
      XSETSYMBOL (val, symbol_free_list);
      symbol_free_list = symbol_free_list->u.s.next;
    }
  else
    {
      if (symbol_block_index == SYMBOL_BLOCK_SIZE)
	{
	  struct symbol_block *new
	    = lisp_malloc (sizeof *new, false, MEM_TYPE_SYMBOL);
	  ASAN_POISON_SYMBOL_BLOCK (new);
	  new->next = symbol_block;
	  symbol_block = new;
	  symbol_block_index = 0;
	}

      ASAN_UNPOISON_SYMBOL (&symbol_block->symbols[symbol_block_index]);
      XSETSYMBOL (val, &symbol_block->symbols[symbol_block_index]);
      symbol_block_index++;
    }

  init_symbol (val, name);
  tally_consing (sizeof (struct Lisp_Symbol));
  symbols_consed++;
  return val;
}



Lisp_Object
make_misc_ptr (void *a)
{
  struct Lisp_Misc_Ptr *p = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Misc_Ptr,
							 PVEC_MISC_PTR);
  p->pointer = a;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

/* Return a new symbol with position with the specified SYMBOL and POSITION. */
Lisp_Object
build_symbol_with_pos (Lisp_Object symbol, Lisp_Object position)
{
  Lisp_Object val;
  struct Lisp_Symbol_With_Pos *p
    = (struct Lisp_Symbol_With_Pos *) allocate_vector (2);
  XSETVECTOR (val, p);
  XSETPVECTYPESIZE (XVECTOR (val), PVEC_SYMBOL_WITH_POS, 2, 0);
  p->sym = symbol;
  p->pos = position;

  return val;
}

/* Return a new (deleted) overlay with PLIST.  */

Lisp_Object
build_overlay (bool front_advance, bool rear_advance,
               Lisp_Object plist)
{
  struct Lisp_Overlay *p = ALLOCATE_PSEUDOVECTOR (struct Lisp_Overlay, plist,
						  PVEC_OVERLAY);
  Lisp_Object overlay = make_lisp_ptr (p, Lisp_Vectorlike);
  struct itree_node *node = xmalloc (sizeof (*node));
  itree_node_init (node, front_advance, rear_advance, overlay);
  p->interval = node;
  p->buffer = NULL;
  set_overlay_plist (overlay, plist);
  return overlay;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
       doc: /* Return a newly allocated marker which does not point at any place.  */)
  (void)
{
  struct Lisp_Marker *p = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Marker,
						       PVEC_MARKER);
  p->buffer = 0;
  p->bytepos = 0;
  p->charpos = 0;
  p->next = NULL;
  p->insertion_type = 0;
  p->need_adjustment = 0;
  return make_lisp_ptr (p, Lisp_Vectorlike);
}

/* Return a newly allocated marker which points into BUF
   at character position CHARPOS and byte position BYTEPOS.  */

Lisp_Object
build_marker (struct buffer *buf, ptrdiff_t charpos, ptrdiff_t bytepos)
{
  /* No dead buffers here.  */
  eassert (BUFFER_LIVE_P (buf));

  /* Every character is at least one byte.  */
  eassert (charpos <= bytepos);

  struct Lisp_Marker *m = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Marker,
						       PVEC_MARKER);
  m->buffer = buf;
  m->charpos = charpos;
  m->bytepos = bytepos;
  m->insertion_type = 0;
  m->need_adjustment = 0;
  m->next = BUF_MARKERS (buf);
  BUF_MARKERS (buf) = m;
  return make_lisp_ptr (m, Lisp_Vectorlike);
}


/* Return a newly created vector or string with specified arguments as
   elements.  If all the arguments are characters that can fit
   in a string of events, make a string; otherwise, make a vector.

   Allows any number of arguments, including zero.  */

Lisp_Object
make_event_array (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;

  for (i = 0; i < nargs; i++)
    /* The things that fit in a string
       are characters that are in 0...127,
       after discarding the meta bit and all the bits above it.  */
    if (!FIXNUMP (args[i])
	|| (XFIXNUM (args[i]) & ~(-CHAR_META)) >= 0200)
      return Fvector (nargs, args);

  /* Since the loop exited, we know that all the things in it are
     characters, so we can make a string.  */
  {
    Lisp_Object result;

    result = Fmake_string (make_fixnum (nargs), make_fixnum (0), Qnil);
    for (i = 0; i < nargs; i++)
      {
	SSET (result, i, XFIXNUM (args[i]));
	/* Move the meta bit to the right place for a string char.  */
	if (XFIXNUM (args[i]) & CHAR_META)
	  SSET (result, i, SREF (result, i) | 0x80);
      }

    return result;
  }
}

#ifdef HAVE_MODULES
/* Create a new module user ptr object.  */
Lisp_Object
make_user_ptr (void (*finalizer) (void *), void *p)
{
  struct Lisp_User_Ptr *uptr
    = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_User_Ptr, PVEC_USER_PTR);
  uptr->finalizer = finalizer;
  uptr->p = p;
  return make_lisp_ptr (uptr, Lisp_Vectorlike);
}
#endif

static void
init_finalizer_list (struct Lisp_Finalizer *head)
{
  head->prev = head->next = head;
}

/* Insert FINALIZER before ELEMENT.  */

static void
finalizer_insert (struct Lisp_Finalizer *element,
                  struct Lisp_Finalizer *finalizer)
{
  eassert (finalizer->prev == NULL);
  eassert (finalizer->next == NULL);
  finalizer->next = element;
  finalizer->prev = element->prev;
  finalizer->prev->next = finalizer;
  element->prev = finalizer;
}

static void
unchain_finalizer (struct Lisp_Finalizer *finalizer)
{
  if (finalizer->prev != NULL)
    {
      eassert (finalizer->next != NULL);
      finalizer->prev->next = finalizer->next;
      finalizer->next->prev = finalizer->prev;
      finalizer->prev = finalizer->next = NULL;
    }
}

static void
mark_finalizer_list (struct Lisp_Finalizer *head)
{
  for (struct Lisp_Finalizer *finalizer = head->next;
       finalizer != head;
       finalizer = finalizer->next)
    {
      set_vectorlike_marked (&finalizer->header);
      mark_object (finalizer->function);
    }
}

/* Move doomed finalizers to list DEST from list SRC.  A doomed
   finalizer is one that is not GC-reachable and whose
   finalizer->function is non-nil.  */

static void
queue_doomed_finalizers (struct Lisp_Finalizer *dest,
                         struct Lisp_Finalizer *src)
{
  struct Lisp_Finalizer *finalizer = src->next;
  while (finalizer != src)
    {
      struct Lisp_Finalizer *next = finalizer->next;
      if (!vectorlike_marked_p (&finalizer->header)
          && !NILP (finalizer->function))
        {
          unchain_finalizer (finalizer);
          finalizer_insert (dest, finalizer);
        }

      finalizer = next;
    }
}

static Lisp_Object
run_finalizer_handler (Lisp_Object args)
{
  add_to_log ("finalizer failed: %S", args);
  return Qnil;
}

static void
run_finalizer_function (Lisp_Object function)
{
  specpdl_ref count = SPECPDL_INDEX ();
#ifdef HAVE_PDUMPER
  ++number_finalizers_run;
#endif

  specbind (Qinhibit_quit, Qt);
  internal_condition_case_1 (call0, function, Qt, run_finalizer_handler);
  unbind_to (count, Qnil);
}

static void
run_finalizers (struct Lisp_Finalizer *finalizers)
{
  struct Lisp_Finalizer *finalizer;
  Lisp_Object function;

  while (finalizers->next != finalizers)
    {
      finalizer = finalizers->next;
      unchain_finalizer (finalizer);
      function = finalizer->function;
      if (!NILP (function))
	{
	  finalizer->function = Qnil;
	  run_finalizer_function (function);
	}
    }
}

DEFUN ("make-finalizer", Fmake_finalizer, Smake_finalizer, 1, 1, 0,
       doc: /* Make a finalizer that will run FUNCTION.
FUNCTION will be called after garbage collection when the returned
finalizer object becomes unreachable.  If the finalizer object is
reachable only through references from finalizer objects, it does not
count as reachable for the purpose of deciding whether to run
FUNCTION.  FUNCTION will be run once per finalizer object.  */)
  (Lisp_Object function)
{
  CHECK_TYPE (FUNCTIONP (function), Qfunctionp, function);
  struct Lisp_Finalizer *finalizer
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_Finalizer, function, PVEC_FINALIZER);
  finalizer->function = function;
  finalizer->prev = finalizer->next = NULL;
  finalizer_insert (&finalizers, finalizer);
  return make_lisp_ptr (finalizer, Lisp_Vectorlike);
}


/************************************************************************
                         Mark bit access functions
 ************************************************************************/

/* With the rare exception of functions implementing block-based
   allocation of various types, you should not directly test or set GC
   mark bits on objects.  Some objects might live in special memory
   regions (e.g., a dump image) and might store their mark bits
   elsewhere.  */

static bool
vector_marked_p (const struct Lisp_Vector *v)
{
  if (pdumper_object_p (v))
    {
      /* Look at cold_start first so that we don't have to fault in
         the vector header just to tell that it's a bool vector.  */
      if (pdumper_cold_object_p (v))
        {
          eassert (PSEUDOVECTOR_TYPE (v) == PVEC_BOOL_VECTOR);
          return true;
        }
      return pdumper_marked_p (v);
    }
  return XVECTOR_MARKED_P (v);
}

static void
set_vector_marked (struct Lisp_Vector *v)
{
  if (pdumper_object_p (v))
    {
      eassert (PSEUDOVECTOR_TYPE (v) != PVEC_BOOL_VECTOR);
      pdumper_set_marked (v);
    }
  else
    XMARK_VECTOR (v);
}

static bool
vectorlike_marked_p (const union vectorlike_header *header)
{
  return vector_marked_p ((const struct Lisp_Vector *) header);
}

static void
set_vectorlike_marked (union vectorlike_header *header)
{
  set_vector_marked ((struct Lisp_Vector *) header);
}

static bool
cons_marked_p (const struct Lisp_Cons *c)
{
  return pdumper_object_p (c)
    ? pdumper_marked_p (c)
    : XCONS_MARKED_P (c);
}

static void
set_cons_marked (struct Lisp_Cons *c)
{
  if (pdumper_object_p (c))
    pdumper_set_marked (c);
  else
    XMARK_CONS (c);
}

static bool
string_marked_p (const struct Lisp_String *s)
{
  return pdumper_object_p (s)
    ? pdumper_marked_p (s)
    : XSTRING_MARKED_P (s);
}

static void
set_string_marked (struct Lisp_String *s)
{
  if (pdumper_object_p (s))
    pdumper_set_marked (s);
  else
    XMARK_STRING (s);
}

static bool
symbol_marked_p (const struct Lisp_Symbol *s)
{
  return pdumper_object_p (s)
    ? pdumper_marked_p (s)
    : s->u.s.gcmarkbit;
}

static void
set_symbol_marked (struct Lisp_Symbol *s)
{
  if (pdumper_object_p (s))
    pdumper_set_marked (s);
  else
    s->u.s.gcmarkbit = true;
}

static bool
interval_marked_p (INTERVAL i)
{
  return pdumper_object_p (i)
    ? pdumper_marked_p (i)
    : i->gcmarkbit;
}

static void
set_interval_marked (INTERVAL i)
{
  if (pdumper_object_p (i))
    pdumper_set_marked (i);
  else
    i->gcmarkbit = true;
}


/************************************************************************
			   Memory Full Handling
 ************************************************************************/


/* Called if malloc (NBYTES) returns zero.  If NBYTES == SIZE_MAX,
   there may have been size_t overflow so that malloc was never
   called, or perhaps malloc was invoked successfully but the
   resulting pointer had problems fitting into a tagged EMACS_INT.  In
   either case this counts as memory being full even though malloc did
   not fail.  */

void
memory_full (size_t nbytes)
{
  if (!initialized)
    fatal ("memory exhausted");

  /* Do not go into hysterics merely because a large request failed.  */
  bool enough_free_memory = false;
  if (SPARE_MEMORY < nbytes)
    {
      void *p = malloc (SPARE_MEMORY);
      if (p)
	{
	  free (p);
	  enough_free_memory = true;
	}
    }

  if (! enough_free_memory)
    {
      Vmemory_full = Qt;
      consing_until_gc = min (consing_until_gc, memory_full_cons_threshold);

      /* The first time we get here, free the spare memory.  */
      for (int i = 0; i < ARRAYELTS (spare_memory); i++)
	if (spare_memory[i])
	  {
	    if (i == 0)
	      free (spare_memory[i]);
	    else if (i >= 1 && i <= 4)
	      lisp_align_free (spare_memory[i]);
	    else
	      lisp_free (spare_memory[i]);
	    spare_memory[i] = 0;
	  }
    }

  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  xsignal (Qnil, Vmemory_signal_data);
}

/* If we released our reserve (due to running out of memory),
   and we have a fair amount free once again,
   try to set aside another reserve in case we run out once more.

   This is called when a relocatable block is freed in ralloc.c,
   and also directly from this file, in case we're not using ralloc.c.  */

void
refill_memory_reserve (void)
{
#if !defined SYSTEM_MALLOC
  if (spare_memory[0] == 0)
    spare_memory[0] = malloc (SPARE_MEMORY);
  if (spare_memory[1] == 0)
    spare_memory[1] = lisp_align_malloc (sizeof (struct cons_block),
						  MEM_TYPE_SPARE);
  if (spare_memory[2] == 0)
    spare_memory[2] = lisp_align_malloc (sizeof (struct cons_block),
					 MEM_TYPE_SPARE);
  if (spare_memory[3] == 0)
    spare_memory[3] = lisp_align_malloc (sizeof (struct cons_block),
					 MEM_TYPE_SPARE);
  if (spare_memory[4] == 0)
    spare_memory[4] = lisp_align_malloc (sizeof (struct cons_block),
					 MEM_TYPE_SPARE);
  if (spare_memory[5] == 0)
    spare_memory[5] = lisp_malloc (sizeof (struct string_block),
				   false, MEM_TYPE_SPARE);
  if (spare_memory[6] == 0)
    spare_memory[6] = lisp_malloc (sizeof (struct string_block),
				   false, MEM_TYPE_SPARE);
  if (spare_memory[0] && spare_memory[1] && spare_memory[5])
    Vmemory_full = Qnil;
#endif
}

/************************************************************************
			   C Stack Marking
 ************************************************************************/

/* Conservative C stack marking requires a method to identify possibly
   live Lisp objects given a pointer value.  We do this by keeping
   track of blocks of Lisp data that are allocated in a red-black tree
   (see also the comment of mem_node which is the type of nodes in
   that tree).  Function lisp_malloc adds information for an allocated
   block to the red-black tree with calls to mem_insert, and function
   lisp_free removes it with mem_delete.  Functions live_string_p etc
   call mem_find to lookup information about a given pointer in the
   tree, and use that to determine if the pointer points into a Lisp
   object or not.  */

/* Initialize this part of alloc.c.  */

static void
mem_init (void)
{
  mem_z.left = mem_z.right = MEM_NIL;
  mem_z.parent = NULL;
  mem_z.color = MEM_BLACK;
  mem_z.start = mem_z.end = NULL;
  mem_root = MEM_NIL;
}


/* Value is a pointer to the mem_node containing START.  Value is
   MEM_NIL if there is no node in the tree containing START.  */

static struct mem_node *
mem_find (void *start)
{
  struct mem_node *p;

  if (start < min_heap_address || start > max_heap_address)
    return MEM_NIL;

  /* Make the search always successful to speed up the loop below.  */
  mem_z.start = start;
  mem_z.end = (char *) start + 1;

  p = mem_root;
  while (start < p->start || start >= p->end)
    p = start < p->start ? p->left : p->right;
  return p;
}


/* Insert a new node into the tree for a block of memory with start
   address START, end address END, and type TYPE.  Value is a
   pointer to the node that was inserted.  */

static struct mem_node *
mem_insert (void *start, void *end, enum mem_type type)
{
  struct mem_node *c, *parent, *x;

  if (min_heap_address == NULL || start < min_heap_address)
    min_heap_address = start;
  if (max_heap_address == NULL || end > max_heap_address)
    max_heap_address = end;

  /* See where in the tree a node for START belongs.  In this
     particular application, it shouldn't happen that a node is already
     present.  For debugging purposes, let's check that.  */
  c = mem_root;
  parent = NULL;

  while (c != MEM_NIL)
    {
      parent = c;
      c = start < c->start ? c->left : c->right;
    }

  /* Create a new node.  */
#ifdef GC_MALLOC_CHECK
  x = malloc (sizeof *x);
  if (x == NULL)
    emacs_abort ();
#else
  x = xmalloc (sizeof *x);
#endif
  x->start = start;
  x->end = end;
  x->type = type;
  x->parent = parent;
  x->left = x->right = MEM_NIL;
  x->color = MEM_RED;

  /* Insert it as child of PARENT or install it as root.  */
  if (parent)
    {
      if (start < parent->start)
	parent->left = x;
      else
	parent->right = x;
    }
  else
    mem_root = x;

  /* Re-establish red-black tree properties.  */
  mem_insert_fixup (x);

  return x;
}


/* Re-establish the red-black properties of the tree, and thereby
   balance the tree, after node X has been inserted; X is always red.  */

static void
mem_insert_fixup (struct mem_node *x)
{
  while (x != mem_root && x->parent->color == MEM_RED)
    {
      /* X is red and its parent is red.  This is a violation of
	 red-black tree property #3.  */

      if (x->parent == x->parent->parent->left)
	{
	  /* We're on the left side of our grandparent, and Y is our
	     "uncle".  */
	  struct mem_node *y = x->parent->parent->right;

	  if (y->color == MEM_RED)
	    {
	      /* Uncle and parent are red but should be black because
		 X is red.  Change the colors accordingly and proceed
		 with the grandparent.  */
	      x->parent->color = MEM_BLACK;
	      y->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      x = x->parent->parent;
            }
	  else
	    {
	      /* Parent and uncle have different colors; parent is
		 red, uncle is black.  */
	      if (x == x->parent->right)
		{
		  x = x->parent;
		  mem_rotate_left (x);
                }

	      x->parent->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      mem_rotate_right (x->parent->parent);
            }
        }
      else
	{
	  /* This is the symmetrical case of above.  */
	  struct mem_node *y = x->parent->parent->left;

	  if (y->color == MEM_RED)
	    {
	      x->parent->color = MEM_BLACK;
	      y->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      x = x->parent->parent;
            }
	  else
	    {
	      if (x == x->parent->left)
		{
		  x = x->parent;
		  mem_rotate_right (x);
		}

	      x->parent->color = MEM_BLACK;
	      x->parent->parent->color = MEM_RED;
	      mem_rotate_left (x->parent->parent);
            }
        }
    }

  /* The root may have been changed to red due to the algorithm.  Set
     it to black so that property #5 is satisfied.  */
  mem_root->color = MEM_BLACK;
}


/*   (x)                   (y)
     / \                   / \
    a   (y)      ===>    (x)  c
        / \              / \
       b   c            a   b  */

static void
mem_rotate_left (struct mem_node *x)
{
  struct mem_node *y;

  /* Turn y's left sub-tree into x's right sub-tree.  */
  y = x->right;
  x->right = y->left;
  if (y->left != MEM_NIL)
    y->left->parent = x;

  /* Y's parent was x's parent.  */
  if (y != MEM_NIL)
    y->parent = x->parent;

  /* Get the parent to point to y instead of x.  */
  if (x->parent)
    {
      if (x == x->parent->left)
	x->parent->left = y;
      else
	x->parent->right = y;
    }
  else
    mem_root = y;

  /* Put x on y's left.  */
  y->left = x;
  if (x != MEM_NIL)
    x->parent = y;
}


/*     (x)                (Y)
       / \                / \
     (y)  c      ===>    a  (x)
     / \                    / \
    a   b                  b   c  */

static void
mem_rotate_right (struct mem_node *x)
{
  struct mem_node *y = x->left;

  x->left = y->right;
  if (y->right != MEM_NIL)
    y->right->parent = x;

  if (y != MEM_NIL)
    y->parent = x->parent;
  if (x->parent)
    {
      if (x == x->parent->right)
	x->parent->right = y;
      else
	x->parent->left = y;
    }
  else
    mem_root = y;

  y->right = x;
  if (x != MEM_NIL)
    x->parent = y;
}


/* Delete node Z from the tree.  If Z is null or MEM_NIL, do nothing.  */

static void
mem_delete (struct mem_node *z)
{
  struct mem_node *x, *y;

  if (!z || z == MEM_NIL)
    return;

  if (z->left == MEM_NIL || z->right == MEM_NIL)
    y = z;
  else
    {
      y = z->right;
      while (y->left != MEM_NIL)
	y = y->left;
    }

  if (y->left != MEM_NIL)
    x = y->left;
  else
    x = y->right;

  x->parent = y->parent;
  if (y->parent)
    {
      if (y == y->parent->left)
	y->parent->left = x;
      else
	y->parent->right = x;
    }
  else
    mem_root = x;

  if (y != z)
    {
      z->start = y->start;
      z->end = y->end;
      z->type = y->type;
    }

  if (y->color == MEM_BLACK)
    mem_delete_fixup (x);

#ifdef GC_MALLOC_CHECK
  free (y);
#else
  xfree (y);
#endif
}


/* Re-establish the red-black properties of the tree, after a
   deletion.  */

static void
mem_delete_fixup (struct mem_node *x)
{
  while (x != mem_root && x->color == MEM_BLACK)
    {
      if (x == x->parent->left)
	{
	  struct mem_node *w = x->parent->right;

	  if (w->color == MEM_RED)
	    {
	      w->color = MEM_BLACK;
	      x->parent->color = MEM_RED;
	      mem_rotate_left (x->parent);
	      w = x->parent->right;
            }

	  if (w->left->color == MEM_BLACK && w->right->color == MEM_BLACK)
	    {
	      w->color = MEM_RED;
	      x = x->parent;
            }
	  else
	    {
	      if (w->right->color == MEM_BLACK)
		{
		  w->left->color = MEM_BLACK;
		  w->color = MEM_RED;
		  mem_rotate_right (w);
		  w = x->parent->right;
                }
	      w->color = x->parent->color;
	      x->parent->color = MEM_BLACK;
	      w->right->color = MEM_BLACK;
	      mem_rotate_left (x->parent);
	      x = mem_root;
            }
        }
      else
	{
	  struct mem_node *w = x->parent->left;

	  if (w->color == MEM_RED)
	    {
	      w->color = MEM_BLACK;
	      x->parent->color = MEM_RED;
	      mem_rotate_right (x->parent);
	      w = x->parent->left;
            }

	  if (w->right->color == MEM_BLACK && w->left->color == MEM_BLACK)
	    {
	      w->color = MEM_RED;
	      x = x->parent;
            }
	  else
	    {
	      if (w->left->color == MEM_BLACK)
		{
		  w->right->color = MEM_BLACK;
		  w->color = MEM_RED;
		  mem_rotate_left (w);
		  w = x->parent->left;
                }

	      w->color = x->parent->color;
	      x->parent->color = MEM_BLACK;
	      w->left->color = MEM_BLACK;
	      mem_rotate_right (x->parent);
	      x = mem_root;
            }
        }
    }

  x->color = MEM_BLACK;
}


/* If P is a pointer into a live Lisp string object on the heap,
   return the object's address.  Otherwise, return NULL.  M points to the
   mem_block for P.

   This and other *_holding functions look for a pointer anywhere into
   the object, not merely for a pointer to the start of the object,
   because some compilers sometimes optimize away the latter.  See
   Bug#28213.  */

static struct Lisp_String *
live_string_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_STRING);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  struct string_block *b = m->start;
  char *cp = p;
  ptrdiff_t offset = cp - (char *) &b->strings[0];

  /* P must point into a Lisp_String structure, and it
     must not be on the free-list.  */
  if (0 <= offset && offset < sizeof b->strings)
    {
      ptrdiff_t off = offset % sizeof b->strings[0];
      if (off == Lisp_String
	  || off == 0
	  || off == offsetof (struct Lisp_String, u.s.size_byte)
	  || off == offsetof (struct Lisp_String, u.s.intervals)
	  || off == offsetof (struct Lisp_String, u.s.data))
	{
	  struct Lisp_String *s = p = cp -= off;
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (s, sizeof (*s)))
	    return NULL;
#endif
	  if (s->u.s.data)
	    return s;
	}
    }
  return NULL;
}

static bool
live_string_p (struct mem_node *m, void *p)
{
  return live_string_holding (m, p) == p;
}

/* If P is a pointer into a live Lisp cons object on the heap, return
   the object's address.  Otherwise, return NULL.  M points to the
   mem_block for P.  */

static struct Lisp_Cons *
live_cons_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_CONS);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  struct cons_block *b = m->start;
  char *cp = p;
  ptrdiff_t offset = cp - (char *) &b->conses[0];

  /* P must point into a Lisp_Cons, not be
     one of the unused cells in the current cons block,
     and not be on the free-list.  */
  if (0 <= offset && offset < sizeof b->conses
      && (b != cons_block
	  || offset / sizeof b->conses[0] < cons_block_index))
    {
      ptrdiff_t off = offset % sizeof b->conses[0];
      if (off == Lisp_Cons
	  || off == 0
	  || off == offsetof (struct Lisp_Cons, u.s.u.cdr))
	{
	  struct Lisp_Cons *s = p = cp -= off;
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (s, sizeof (*s)))
	    return NULL;
#endif
	  if (!deadp (s->u.s.car))
	    return s;
	}
    }
  return NULL;
}

static bool
live_cons_p (struct mem_node *m, void *p)
{
  return live_cons_holding (m, p) == p;
}


/* If P is a pointer into a live Lisp symbol object on the heap,
   return the object's address.  Otherwise, return NULL.  M points to the
   mem_block for P.  */

static struct Lisp_Symbol *
live_symbol_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_SYMBOL);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif
  struct symbol_block *b = m->start;
  char *cp = p;
  ptrdiff_t offset = cp - (char *) &b->symbols[0];

  /* P must point into the Lisp_Symbol, not be
     one of the unused cells in the current symbol block,
     and not be on the free-list.  */
  if (0 <= offset && offset < sizeof b->symbols
      && (b != symbol_block
	  || offset / sizeof b->symbols[0] < symbol_block_index))
    {
      ptrdiff_t off = offset % sizeof b->symbols[0];
      if (off == Lisp_Symbol

	  /* Plain '|| off == 0' would run afoul of GCC 10.2
	     -Wlogical-op, as Lisp_Symbol happens to be zero.  */
	  || (Lisp_Symbol != 0 && off == 0)

	  || off == offsetof (struct Lisp_Symbol, u.s.name)
	  || off == offsetof (struct Lisp_Symbol, u.s.val)
	  || off == offsetof (struct Lisp_Symbol, u.s.function)
	  || off == offsetof (struct Lisp_Symbol, u.s.plist)
	  || off == offsetof (struct Lisp_Symbol, u.s.next))
	{
	  struct Lisp_Symbol *s = p = cp -= off;
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (s, sizeof (*s)))
	    return NULL;
#endif
	  if (!deadp (s->u.s.function))
	    return s;
	}
    }
  return NULL;
}

static bool
live_symbol_p (struct mem_node *m, void *p)
{
  return live_symbol_holding (m, p) == p;
}


/* If P is a (possibly-tagged) pointer to a live Lisp_Float on the
   heap, return the address of the Lisp_Float.  Otherwise, return NULL.
   M is a pointer to the mem_block for P.  */

static struct Lisp_Float *
live_float_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_FLOAT);
#if GC_ASAN_POISON_OBJECTS
  if (__asan_address_is_poisoned (p))
    return NULL;
#endif

  struct float_block *b = m->start;
  char *cp = p;
  ptrdiff_t offset = cp - (char *) &b->floats[0];

  /* P must point to (or be a tagged pointer to) the start of a
     Lisp_Float and not be one of the unused cells in the current
     float block.  */
  if (0 <= offset && offset < sizeof b->floats)
    {
      int off = offset % sizeof b->floats[0];
      if ((off == Lisp_Float || off == 0)
	  && (b != float_block
	      || offset / sizeof b->floats[0] < float_block_index))
	{
	  struct Lisp_Float *f = (struct Lisp_Float *) (cp - off);
#if GC_ASAN_POISON_OBJECTS
	  if (__asan_region_is_poisoned (f, sizeof (*f)))
	    return NULL;
#endif
	  return f;
	}
    }
  return NULL;
}

static bool
live_float_p (struct mem_node *m, void *p)
{
  return live_float_holding (m, p) == p;
}

/* Return VECTOR if P points within it, NULL otherwise.  */

static struct Lisp_Vector *
live_vector_pointer (struct Lisp_Vector *vector, void *p)
{
  void *vvector = vector;
  char *cvector = vvector;
  char *cp = p;
  ptrdiff_t offset = cp - cvector;
  return ((offset == Lisp_Vectorlike
	   || offset == 0
	   || (sizeof vector->header <= offset
	       && offset < vector_nbytes (vector)
	       && (! (vector->header.size & PSEUDOVECTOR_FLAG)
		   ? (offsetof (struct Lisp_Vector, contents) <= offset
		      && (((offset - offsetof (struct Lisp_Vector, contents))
			   % word_size)
			  == 0))
		   /* For non-bool-vector pseudovectors, treat any pointer
		      past the header as valid since it's too much of a pain
		      to write special-case code for every pseudovector.  */
		   : (! PSEUDOVECTOR_TYPEP (&vector->header, PVEC_BOOL_VECTOR)
		      || offset == offsetof (struct Lisp_Bool_Vector, size)
		      || (offsetof (struct Lisp_Bool_Vector, data) <= offset
			  && (((offset
				- offsetof (struct Lisp_Bool_Vector, data))
			       % sizeof (bits_word))
			      == 0))))))
	  ? vector : NULL);
}

/* If P is a pointer to a live, large vector-like object, return the object.
   Otherwise, return nil.
   M is a pointer to the mem_block for P.  */

static struct Lisp_Vector *
live_large_vector_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_VECTORLIKE);
  return live_vector_pointer (large_vector_vec (m->start), p);
}

static bool
live_large_vector_p (struct mem_node *m, void *p)
{
  return live_large_vector_holding (m, p) == p;
}

/* If P is a pointer to a live, small vector-like object, return the object.
   Otherwise, return NULL.
   M is a pointer to the mem_block for P.  */

static struct Lisp_Vector *
live_small_vector_holding (struct mem_node *m, void *p)
{
  eassert (m->type == MEM_TYPE_VECTOR_BLOCK);
  struct Lisp_Vector *vp = p;
  struct vector_block *block = m->start;
  struct Lisp_Vector *vector = (struct Lisp_Vector *) block->data;

  /* P is in the block's allocation range.  Scan the block
     up to P and see whether P points to the start of some
     vector which is not on a free list.  FIXME: check whether
     some allocation patterns (probably a lot of short vectors)
     may cause a substantial overhead of this loop.  */
  while (VECTOR_IN_BLOCK (vector, block) && vector <= vp)
    {
      struct Lisp_Vector *next = ADVANCE (vector, vector_nbytes (vector));
      if (vp < next && !PSEUDOVECTOR_TYPEP (&vector->header, PVEC_FREE))
	return live_vector_pointer (vector, vp);
      vector = next;
    }
  return NULL;
}

static bool
live_small_vector_p (struct mem_node *m, void *p)
{
  return live_small_vector_holding (m, p) == p;
}

/* If P points to Lisp data, mark that as live if it isn't already
   marked.  */

static void
mark_maybe_pointer (void *p, bool symbol_only)
{
  struct mem_node *m;

#if USE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED (&p, sizeof (p));
#endif

  /* If the pointer is in the dump image and the dump has a record
     of the object starting at the place where the pointer points, we
     definitely have an object.  If the pointer is in the dump image
     and the dump has no idea what the pointer is pointing at, we
     definitely _don't_ have an object.  */
  if (pdumper_object_p (p))
    {
      /* FIXME: This code assumes that every reachable pdumper object
	 is addressed either by a pointer to the object start, or by
	 the same pointer with an LSB-style tag.  This assumption
	 fails if a pdumper object is reachable only via machine
	 addresses of non-initial object components.  Although such
	 addressing is rare in machine code generated by C compilers
	 from Emacs source code, it can occur in some cases.  To fix
	 this problem, the pdumper code should grok non-initial
	 addresses, as the non-pdumper code does.  */
      uintptr_t mask = VALMASK & UINTPTR_MAX;
      uintptr_t masked_p = (uintptr_t) p & mask;
      void *po = (void *) masked_p;
      char *cp = p;
      char *cpo = po;
      /* Don't use pdumper_object_p_precise here! It doesn't check the
         tag bits. OBJ here might be complete garbage, so we need to
         verify both the pointer and the tag.  */
      int type = pdumper_find_object_type (po);
      if (pdumper_valid_object_type_p (type)
	  && (!USE_LSB_TAG || p == po || cp - cpo == type))
	{
	  if (type == Lisp_Symbol)
	    mark_object (make_lisp_symbol (po));
	  else if (!symbol_only)
	    mark_object (make_lisp_ptr (po, type));
	}
      return;
    }

  m = mem_find (p);
  if (m != MEM_NIL)
    {
      Lisp_Object obj;

      switch (m->type)
	{
	case MEM_TYPE_NON_LISP:
	case MEM_TYPE_SPARE:
	  /* Nothing to do; not a pointer to Lisp memory.  */
	  return;

	case MEM_TYPE_CONS:
	  {
	    if (symbol_only)
	      return;
	    struct Lisp_Cons *h = live_cons_holding (m, p);
	    if (!h)
	      return;
	    obj = make_lisp_ptr (h, Lisp_Cons);
	  }
	  break;

	case MEM_TYPE_STRING:
	  {
	    if (symbol_only)
	      return;
	    struct Lisp_String *h = live_string_holding (m, p);
	    if (!h)
	      return;
	    obj = make_lisp_ptr (h, Lisp_String);
	  }
	  break;

	case MEM_TYPE_SYMBOL:
	  {
	    struct Lisp_Symbol *h = live_symbol_holding (m, p);
	    if (!h)
	      return;
	    obj = make_lisp_symbol (h);
	  }
	  break;

	case MEM_TYPE_FLOAT:
	  {
	    if (symbol_only)
	      return;
	    struct Lisp_Float *h = live_float_holding (m, p);
	    if (!h)
	      return;
	    obj = make_lisp_ptr (h, Lisp_Float);
	  }
	  break;

	case MEM_TYPE_VECTORLIKE:
	  {
	    if (symbol_only)
	      return;
	    struct Lisp_Vector *h = live_large_vector_holding (m, p);
	    if (!h)
	      return;
	    obj = make_lisp_ptr (h, Lisp_Vectorlike);
	  }
	  break;

	case MEM_TYPE_VECTOR_BLOCK:
	  {
	    if (symbol_only)
	      return;
	    struct Lisp_Vector *h = live_small_vector_holding (m, p);
	    if (!h)
	      return;
	    obj = make_lisp_ptr (h, Lisp_Vectorlike);
	  }
	  break;

	default:
	  emacs_abort ();
	}

      mark_object (obj);
    }
}


/* Alignment of pointer values.  Use alignof, as it sometimes returns
   a smaller alignment than GCC's __alignof__ and mark_memory might
   miss objects if __alignof__ were used.  */
#define GC_POINTER_ALIGNMENT alignof (void *)

/* Mark Lisp objects referenced from the address range START..END
   or END..START.  */

void ATTRIBUTE_NO_SANITIZE_ADDRESS
mark_memory (void const *start, void const *end)
{
  char const *pp;

  /* Make START the pointer to the start of the memory region,
     if it isn't already.  */
  if (end < start)
    {
      void const *tem = start;
      start = end;
      end = tem;
    }

  eassert (((uintptr_t) start) % GC_POINTER_ALIGNMENT == 0);

  /* Mark Lisp data pointed to.  This is necessary because, in some
     situations, the C compiler optimizes Lisp objects away, so that
     only a pointer to them remains.  Example:

     DEFUN ("testme", Ftestme, Stestme, 0, 0, 0, "")
     ()
     {
       Lisp_Object obj = build_string ("test");
       struct Lisp_String *s = XSTRING (obj);
       garbage_collect ();
       fprintf (stderr, "test '%s'\n", s->u.s.data);
       return Qnil;
     }

     Here, `obj' isn't really used, and the compiler optimizes it
     away.  The only reference to the life string is through the
     pointer `s'.  */

  for (pp = start; (void const *) pp < end; pp += GC_POINTER_ALIGNMENT)
    {
      void *p = *(void *const *) pp;
      intptr_t ip;

#if !USE_LSB_TAG && !defined WIDE_EMACS_INT
      ip = (intptr_t) p;
      mark_maybe_pointer ((void *) (ip & VALMASK), false);
#else /* USE_LSB_TAG || WIDE_EMACS_INT */
      mark_maybe_pointer (p, false);
#endif /* USE_LSB_TAG || WIDE_EMACS_INT */

      /* Unmask any struct Lisp_Symbol pointer that make_lisp_symbol
	 previously disguised by adding the address of 'lispsym'.
	 On a host with 32-bit pointers and 64-bit Lisp_Objects,
	 a Lisp_Object might be split into registers saved into
	 non-adjacent words and P might be the low-order word's value.  */
      ckd_add (&ip, (intptr_t) p, (intptr_t) lispsym);
      mark_maybe_pointer ((void *) ip, true);
    }
}

#ifndef HAVE___BUILTIN_UNWIND_INIT

# ifdef GC_SETJMP_WORKS
static void
test_setjmp (void)
{
}
# else

static bool setjmp_tested_p;
static int longjmps_done;

#  define SETJMP_WILL_LIKELY_WORK "\
\n\
Emacs garbage collector has been changed to use conservative stack\n\
marking.  Emacs has determined that the method it uses to do the\n\
marking will likely work on your system, but this isn't sure.\n\
\n\
If you are a system-programmer, or can get the help of a local wizard\n\
who is, please take a look at the function mark_c_stack in alloc.c, and\n\
verify that the methods used are appropriate for your system.\n\
\n\
Please mail the result to <emacs-devel@gnu.org>.\n\
"

#  define SETJMP_WILL_NOT_WORK "\
\n\
Emacs garbage collector has been changed to use conservative stack\n\
marking.  Emacs has determined that the default method it uses to do the\n\
marking will not work on your system.  We will need a system-dependent\n\
solution for your system.\n\
\n\
Please take a look at the function mark_c_stack in alloc.c, and\n\
try to find a way to make it work on your system.\n\
\n\
Note that you may get false negatives, depending on the compiler.\n\
In particular, you need to use -O with GCC for this test.\n\
\n\
Please mail the result to <emacs-devel@gnu.org>.\n\
"


/* Perform a quick check if it looks like setjmp saves registers in a
   jmp_buf.  Print a message to stderr saying so.  When this test
   succeeds, this is _not_ a proof that setjmp is sufficient for
   conservative stack marking.  Only the sources or a disassembly
   can prove that.  */

static void
test_setjmp (void)
{
  if (setjmp_tested_p)
    return;
  setjmp_tested_p = true;
  char buf[10];
  register int x;
  sys_jmp_buf jbuf;

  /* Arrange for X to be put in a register.  */
  sprintf (buf, "1");
  x = strlen (buf);
  x = 2 * x - 1;

  sys_setjmp (jbuf);
  if (longjmps_done == 1)
    {
      /* Came here after the longjmp at the end of the function.

         If x == 1, the longjmp has restored the register to its
         value before the setjmp, and we can hope that setjmp
         saves all such registers in the jmp_buf, although that
	 isn't sure.

         For other values of X, either something really strange is
         taking place, or the setjmp just didn't save the register.  */

      if (x == 1)
	fputs (SETJMP_WILL_LIKELY_WORK, stderr);
      else
	{
	  fputs (SETJMP_WILL_NOT_WORK, stderr);
	  exit (1);
	}
    }

  ++longjmps_done;
  x = 2;
  if (longjmps_done == 1)
    sys_longjmp (jbuf, 1);
}
# endif /* ! GC_SETJMP_WORKS */
#endif /* ! HAVE___BUILTIN_UNWIND_INIT */

/* The type of an object near the stack top, whose address can be used
   as a stack scan limit.  */
typedef union
{
  /* Make sure stack_top and m_stack_bottom are properly aligned as GC
     expects.  */
  Lisp_Object o;
  void *p;
#ifndef HAVE___BUILTIN_UNWIND_INIT
  sys_jmp_buf j;
  char c;
#endif
} stacktop_sentry;

/* Set *P to the address of the top of the stack.  This must be a
   macro, not a function, so that it is executed in the caller's
   environment.  It is not inside a do-while so that its storage
   survives the macro.  Callers should be declared NO_INLINE.  */
#ifdef HAVE___BUILTIN_UNWIND_INIT
# define SET_STACK_TOP_ADDRESS(p)	\
   stacktop_sentry sentry;		\
   *(p) = NEAR_STACK_TOP (&sentry)
#else
# define SET_STACK_TOP_ADDRESS(p)		\
   stacktop_sentry sentry;			\
   test_setjmp ();				\
   sys_setjmp (sentry.j);			\
   *(p) = NEAR_STACK_TOP (&sentry + (stack_bottom < &sentry.c))
#endif

/* Mark live Lisp objects on the C stack.

   There are several system-dependent problems to consider when
   porting this to new architectures:

   Processor Registers

   We have to mark Lisp objects in CPU registers that can hold local
   variables or are used to pass parameters.

   If __builtin_unwind_init is available, it should suffice to save
   registers.

   Otherwise, assume that calling setjmp saves registers we need
   to see in a jmp_buf which itself lies on the stack.  This doesn't
   have to be true!  It must be verified for each system, possibly
   by taking a look at the source code of setjmp.

   Stack Layout

   Architectures differ in the way their processor stack is organized.
   For example, the stack might look like this

     +----------------+
     |  Lisp_Object   |  size = 4
     +----------------+
     | something else |  size = 2
     +----------------+
     |  Lisp_Object   |  size = 4
     +----------------+
     |	...	      |

   In such a case, not every Lisp_Object will be aligned equally.  To
   find all Lisp_Object on the stack it won't be sufficient to walk
   the stack in steps of 4 bytes.  Instead, two passes will be
   necessary, one starting at the start of the stack, and a second
   pass starting at the start of the stack + 2.  Likewise, if the
   minimal alignment of Lisp_Objects on the stack is 1, four passes
   would be necessary, each one starting with one byte more offset
   from the stack start.  */

void
mark_c_stack (char const *bottom, char const *end)
{
  /* This assumes that the stack is a contiguous region in memory.  If
     that's not the case, something has to be done here to iterate
     over the stack segments.  */
  mark_memory (bottom, end);

  /* Allow for marking a secondary stack, like the register stack on the
     ia64.  */
#ifdef GC_MARK_SECONDARY_STACK
  GC_MARK_SECONDARY_STACK ();
#endif
}

/* flush_stack_call_func is the trampoline function that flushes
   registers to the stack, and then calls FUNC.  ARG is passed through
   to FUNC verbatim.

   This function must be called whenever Emacs is about to release the
   global interpreter lock.  This lets the garbage collector easily
   find roots in registers on threads that are not actively running
   Lisp.

   It is invalid to run any Lisp code or to allocate any GC memory
   from FUNC.

   Note: all register spilling is done in flush_stack_call_func before
   flush_stack_call_func1 is activated.

   flush_stack_call_func1 is responsible for identifying the stack
   address range to be scanned.  It *must* be carefully kept as
   noinline to make sure that registers has been spilled before it is
   called, otherwise given __builtin_frame_address (0) typically
   returns the frame pointer (base pointer) and not the stack pointer
   [1] GC will miss to scan callee-saved registers content
   (Bug#41357).

   [1] <https://gcc.gnu.org/onlinedocs/gcc/Return-Address.html>.  */

NO_INLINE void
flush_stack_call_func1 (void (*func) (void *arg), void *arg)
{
  void *end;
  struct thread_state *self = current_thread;
  SET_STACK_TOP_ADDRESS (&end);
  self->stack_top = end;
  func (arg);
  eassert (current_thread == self);
}

/* Determine whether it is safe to access memory at address P.  */
static int
valid_pointer_p (void *p)
{
#ifdef WINDOWSNT
  return w32_valid_pointer_p (p, 16);
#else

  if (ADDRESS_SANITIZER)
    return p ? -1 : 0;

  int fd[2];
  static int under_rr_state;

  if (!under_rr_state)
    under_rr_state = getenv ("RUNNING_UNDER_RR") ? -1 : 1;
  if (under_rr_state < 0)
    return under_rr_state;

  /* Obviously, we cannot just access it (we would SEGV trying), so we
     trick the o/s to tell us whether p is a valid pointer.
     Unfortunately, we cannot use NULL_DEVICE here, as emacs_write may
     not validate p in that case.  */

  if (emacs_pipe (fd) == 0)
    {
      bool valid = emacs_write (fd[1], p, 16) == 16;
      emacs_close (fd[1]);
      emacs_close (fd[0]);
      return valid;
    }

  return -1;
#endif
}

/* Return 2 if OBJ is a killed or special buffer object, 1 if OBJ is a
   valid lisp object, 0 if OBJ is NOT a valid lisp object, or -1 if we
   cannot validate OBJ.  This function can be quite slow, and is used
   only in debugging.  */

int
valid_lisp_object_p (Lisp_Object obj)
{
  if (FIXNUMP (obj))
    return 1;

  void *p = XPNTR (obj);

  if (BARE_SYMBOL_P (obj) && c_symbol_p (p))
    return ((char *) p - (char *) lispsym) % sizeof lispsym[0] == 0;

  if (p == &buffer_defaults || p == &buffer_local_symbols)
    return 2;

  if (pdumper_object_p (p))
    return pdumper_object_p_precise (p) ? 1 : 0;

  struct mem_node *m = mem_find (p);

  if (m == MEM_NIL)
    {
      int valid = valid_pointer_p (p);
      if (valid <= 0)
	return valid;

      /* Strings and conses produced by AUTO_STRING etc. all get here.  */
      if (SUBRP (obj) || STRINGP (obj) || CONSP (obj))
	return 1;

      return 0;
    }

  switch (m->type)
    {
    case MEM_TYPE_NON_LISP:
    case MEM_TYPE_SPARE:
      return 0;

    case MEM_TYPE_CONS:
      return live_cons_p (m, p);

    case MEM_TYPE_STRING:
      return live_string_p (m, p);

    case MEM_TYPE_SYMBOL:
      return live_symbol_p (m, p);

    case MEM_TYPE_FLOAT:
      return live_float_p (m, p);

    case MEM_TYPE_VECTORLIKE:
      return live_large_vector_p (m, p);

    case MEM_TYPE_VECTOR_BLOCK:
      return live_small_vector_p (m, p);

    default:
      break;
    }

  return 0;
}

/* Like xmalloc, but makes allocation count toward the total consing
   and hash table or obarray usage.
   Return NULL for a zero-sized allocation.  */
void *
hash_table_alloc_bytes (ptrdiff_t nbytes)
{
  if (nbytes == 0)
    return NULL;
  tally_consing (nbytes);
  hash_table_allocated_bytes += nbytes;
  return xmalloc (nbytes);
}

/* Like xfree, but makes allocation count toward the total consing.  */
void
hash_table_free_bytes (void *p, ptrdiff_t nbytes)
{
  tally_consing (-nbytes);
  hash_table_allocated_bytes -= nbytes;
  xfree (p);
}


/***********************************************************************
			  Protection from GC
 ***********************************************************************/

/* Put an entry in staticvec, pointing at the variable with address
   VARADDRESS.  */

void
staticpro (Lisp_Object const *varaddress)
{
  for (int i = 0; i < staticidx; i++)
    eassert (staticvec[i] != varaddress);
  if (staticidx >= NSTATICS)
    fatal ("NSTATICS too small; try increasing and recompiling Emacs.");
  staticvec[staticidx++] = varaddress;
}


/***********************************************************************
			  Protection from GC
 ***********************************************************************/

/* Temporarily prevent garbage collection.  Temporarily bump
   consing_until_gc to speed up maybe_gc when GC is inhibited.  */

static void
allow_garbage_collection (intmax_t consing)
{
  consing_until_gc = consing - (HI_THRESHOLD - consing_until_gc);
  garbage_collection_inhibited--;
}

specpdl_ref
inhibit_garbage_collection (void)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_intmax (allow_garbage_collection, consing_until_gc);
  garbage_collection_inhibited++;
  consing_until_gc = HI_THRESHOLD;
  return count;
}

/* Return the number of bytes in N objects each of size S, guarding
   against overflow if size_t is narrower than byte_ct.  */

static byte_ct
object_bytes (object_ct n, size_t s)
{
  byte_ct b = s;
  return n * b;
}

/* Calculate total bytes of live objects.  */

static byte_ct
total_bytes_of_live_objects (void)
{
  byte_ct tot = 0;
  tot += object_bytes (gcstat.total_conses, sizeof (struct Lisp_Cons));
  tot += object_bytes (gcstat.total_symbols, sizeof (struct Lisp_Symbol));
  tot += gcstat.total_string_bytes;
  tot += object_bytes (gcstat.total_vector_slots, word_size);
  tot += object_bytes (gcstat.total_floats, sizeof (struct Lisp_Float));
  tot += object_bytes (gcstat.total_intervals, sizeof (struct interval));
  tot += object_bytes (gcstat.total_strings, sizeof (struct Lisp_String));
  tot += gcstat.total_hash_table_bytes;
  return tot;
}

#ifdef HAVE_WINDOW_SYSTEM

/* Remove unmarked font-spec and font-entity objects from ENTRY, which is
   (DRIVER-TYPE NUM-FRAMES FONT-CACHE-DATA ...), and return changed entry.  */

static Lisp_Object
compact_font_cache_entry (Lisp_Object entry)
{
  Lisp_Object tail, *prev = &entry;

  for (tail = entry; CONSP (tail); tail = XCDR (tail))
    {
      bool drop = 0;
      Lisp_Object obj = XCAR (tail);

      /* Consider OBJ if it is (font-spec . [font-entity font-entity ...]).  */
      if (CONSP (obj) && GC_FONT_SPEC_P (XCAR (obj))
	  && !vectorlike_marked_p (&GC_XFONT_SPEC (XCAR (obj))->header)
	  /* Don't use VECTORP here, as that calls ASIZE, which could
	     hit assertion violation during GC.  */
	  && (VECTORLIKEP (XCDR (obj))
	      && ! (gc_asize (XCDR (obj)) & PSEUDOVECTOR_FLAG)))
	{
	  ptrdiff_t i, size = gc_asize (XCDR (obj));
	  Lisp_Object obj_cdr = XCDR (obj);

	  /* If font-spec is not marked, most likely all font-entities
	     are not marked too.  But we must be sure that nothing is
	     marked within OBJ before we really drop it.  */
	  for (i = 0; i < size; i++)
            {
              Lisp_Object objlist;

              if (vectorlike_marked_p (
                    &GC_XFONT_ENTITY (AREF (obj_cdr, i))->header))
                break;

              objlist = AREF (AREF (obj_cdr, i), FONT_OBJLIST_INDEX);
              for (; CONSP (objlist); objlist = XCDR (objlist))
                {
                  Lisp_Object val = XCAR (objlist);
                  struct font *font = GC_XFONT_OBJECT (val);

                  if (!NILP (AREF (val, FONT_TYPE_INDEX))
                      && vectorlike_marked_p (&font->header))
                    break;
                }
              if (CONSP (objlist))
		{
		  /* Found a marked font, bail out.  */
		  break;
		}
            }

	  if (i == size)
	    {
	      /* No marked fonts were found, so this entire font
		 entity can be dropped.  */
	      drop = 1;
	    }
	}
      if (drop)
	*prev = XCDR (tail);
      else
	prev = xcdr_addr (tail);
    }
  return entry;
}

/* Compact font caches on all terminals and mark
   everything which is still here after compaction.  */

static void
compact_font_caches (void)
{
  struct terminal *t;

  for (t = terminal_list; t; t = t->next_terminal)
    {
      Lisp_Object cache = TERMINAL_FONT_CACHE (t);
      /* Inhibit compacting the caches if the user so wishes.  Some of
	 the users don't mind a larger memory footprint, but do mind
	 slower redisplay.  */
      if (!inhibit_compacting_font_caches
	  && CONSP (cache))
	{
	  Lisp_Object entry;

	  for (entry = XCDR (cache); CONSP (entry); entry = XCDR (entry))
	    XSETCAR (entry, compact_font_cache_entry (XCAR (entry)));
	}
      mark_object (cache);
    }
}

#else /* not HAVE_WINDOW_SYSTEM */

#define compact_font_caches() (void)(0)

#endif /* HAVE_WINDOW_SYSTEM */

/* Remove (MARKER . DATA) entries with unmarked MARKER
   from buffer undo LIST and return changed list.  */

static Lisp_Object
compact_undo_list (Lisp_Object list)
{
  Lisp_Object tail, *prev = &list;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    {
      if (CONSP (XCAR (tail))
	  && MARKERP (XCAR (XCAR (tail)))
	  && !vectorlike_marked_p (&XMARKER (XCAR (XCAR (tail)))->header))
	*prev = XCDR (tail);
      else
	prev = xcdr_addr (tail);
    }
  return list;
}

#if defined HAVE_ANDROID && !defined (__clang__)

/* The Android gcc is broken and needs the following version of
   make_lisp_symbol.  Otherwise a mysterious ICE pops up.  */

#define make_lisp_symbol android_make_lisp_symbol

static Lisp_Object
android_make_lisp_symbol (struct Lisp_Symbol *sym)
{
  intptr_t symoffset;

  symoffset = (intptr_t) sym;
  ckd_sub (&symoffset, symoffset, (intptr_t) &lispsym);

  {
    Lisp_Object a = TAG_PTR_INITIALLY (Lisp_Symbol, symoffset);
    return a;
  }
}

#endif

static void
visit_vectorlike_root (struct gc_root_visitor visitor,
                       struct Lisp_Vector *ptr,
                       enum gc_root_type type)
{
  ptrdiff_t size = ptr->header.size;
  ptrdiff_t i;

  if (size & PSEUDOVECTOR_FLAG)
    size &= PSEUDOVECTOR_SIZE_MASK;
  for (i = 0; i < size; i++)
    visitor.visit (&ptr->contents[i], type, visitor.data);
}

static void
visit_buffer_root (struct gc_root_visitor visitor,
                   struct buffer *buffer,
                   enum gc_root_type type)
{
  /* Buffers that are roots don't have intervals, an undo list, or
     other constructs that real buffers have.  */
  eassert (buffer->base_buffer == NULL);
  eassert (buffer->overlays == NULL);

  /* Visit the buffer-locals.  */
  visit_vectorlike_root (visitor, (struct Lisp_Vector *) buffer, type);
}

/* Visit GC roots stored in the Emacs data section.  Used by both core
   GC and by the portable dumping code.

   There are other GC roots of course, but these roots are dynamic
   runtime data structures that pdump doesn't care about and so we can
   continue to mark those directly in garbage_collect.  */
void
visit_static_gc_roots (struct gc_root_visitor visitor)
{
  visit_buffer_root (visitor,
                     &buffer_defaults,
                     GC_ROOT_BUFFER_LOCAL_DEFAULT);
  visit_buffer_root (visitor,
                     &buffer_local_symbols,
                     GC_ROOT_BUFFER_LOCAL_NAME);

  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    {
      Lisp_Object sptr = builtin_lisp_symbol (i);
      visitor.visit (&sptr, GC_ROOT_C_SYMBOL, visitor.data);
    }

  for (int i = 0; i < staticidx; i++)
    visitor.visit (staticvec[i], GC_ROOT_STATICPRO, visitor.data);
}

static void
mark_object_root_visitor (Lisp_Object const *root_ptr,
                          enum gc_root_type type,
                          void *data)
{
  mark_object (*root_ptr);
}

/* List of weak hash tables we found during marking the Lisp heap.
   NULL on entry to garbage_collect and after it returns.  */
static struct Lisp_Hash_Table *weak_hash_tables;

NO_INLINE /* For better stack traces */
static void
mark_and_sweep_weak_table_contents (void)
{
  struct Lisp_Hash_Table *h;
  bool marked;

  /* Mark all keys and values that are in use.  Keep on marking until
     there is no more change.  This is necessary for cases like
     value-weak table A containing an entry X -> Y, where Y is used in a
     key-weak table B, Z -> Y.  If B comes after A in the list of weak
     tables, X -> Y might be removed from A, although when looking at B
     one finds that it shouldn't.  */
  do
    {
      marked = false;
      for (h = weak_hash_tables; h; h = h->next_weak)
        marked |= sweep_weak_table (h, false);
    }
  while (marked);

  /* Remove hash table entries that aren't used.  */
  while (weak_hash_tables)
    {
      h = weak_hash_tables;
      weak_hash_tables = h->next_weak;
      h->next_weak = NULL;
      sweep_weak_table (h, true);
    }
}

/* Return the number of bytes to cons between GCs, given THRESHOLD and
   PERCENTAGE.  When calculating a threshold based on PERCENTAGE,
   assume SINCE_GC bytes have been allocated since the most recent GC.
   The returned value is positive and no greater than HI_THRESHOLD.  */
static EMACS_INT
consing_threshold (intmax_t threshold, Lisp_Object percentage,
		   intmax_t since_gc)
{
  if (!NILP (Vmemory_full))
    return memory_full_cons_threshold;
  else
    {
      threshold = max (threshold, GC_DEFAULT_THRESHOLD / 10);
      if (FLOATP (percentage))
	{
	  double tot = (XFLOAT_DATA (percentage)
			* (total_bytes_of_live_objects () + since_gc));
	  if (threshold < tot)
	    {
	      if (tot < HI_THRESHOLD)
		return tot;
	      else
		return HI_THRESHOLD;
	    }
	}
      return min (threshold, HI_THRESHOLD);
    }
}

/* Adjust consing_until_gc and gc_threshold, given THRESHOLD and PERCENTAGE.
   Return the updated consing_until_gc.  */

static EMACS_INT
bump_consing_until_gc (intmax_t threshold, Lisp_Object percentage)
{
  /* Guesstimate that half the bytes allocated since the most
     recent GC are still in use.  */
  EMACS_INT since_gc = (gc_threshold - consing_until_gc) >> 1;
  EMACS_INT new_gc_threshold = consing_threshold (threshold, percentage,
						  since_gc);
  consing_until_gc += new_gc_threshold - gc_threshold;
  gc_threshold = new_gc_threshold;
  return consing_until_gc;
}

/* Watch changes to gc-cons-threshold.  */
static Lisp_Object
watch_gc_cons_threshold (Lisp_Object symbol, Lisp_Object newval,
			 Lisp_Object operation, Lisp_Object where)
{
  intmax_t threshold;
  if (! (INTEGERP (newval) && integer_to_intmax (newval, &threshold)))
    return Qnil;
  bump_consing_until_gc (threshold, Vgc_cons_percentage);
  return Qnil;
}

/* Watch changes to gc-cons-percentage.  */
static Lisp_Object
watch_gc_cons_percentage (Lisp_Object symbol, Lisp_Object newval,
			  Lisp_Object operation, Lisp_Object where)
{
  bump_consing_until_gc (gc_cons_threshold, newval);
  return Qnil;
}

/* It may be time to collect garbage.  Recalculate consing_until_gc,
   since it might depend on current usage, and do the garbage
   collection if the recalculation says so.  */
void
maybe_garbage_collect (void)
{
  if (bump_consing_until_gc (gc_cons_threshold, Vgc_cons_percentage) < 0)
    garbage_collect ();
}

static inline bool mark_stack_empty_p (void);

/* Subroutine of Fgarbage_collect that does most of the work.  */
void
garbage_collect (void)
{
  Lisp_Object tail, buffer;
  char stack_top_variable;
  bool message_p;
  specpdl_ref count = SPECPDL_INDEX ();
  struct timespec start;

  eassert (weak_hash_tables == NULL);

  if (garbage_collection_inhibited)
    return;

  eassert(mark_stack_empty_p ());

  /* Record this function, so it appears on the profiler's backtraces.  */
  record_in_backtrace (QAutomatic_GC, 0, 0);

  /* Don't keep undo information around forever.
     Do this early on, so it is no problem if the user quits.  */
  FOR_EACH_LIVE_BUFFER (tail, buffer)
    compact_buffer (XBUFFER (buffer));

  byte_ct tot_before = (profiler_memory_running
			? total_bytes_of_live_objects ()
			: (byte_ct) -1);

  start = current_timespec ();

  /* In case user calls debug_print during GC,
     don't let that cause a recursive GC.  */
  consing_until_gc = HI_THRESHOLD;

  /* Save what's currently displayed in the echo area.  Don't do that
     if we are GC'ing because we've run out of memory, since
     push_message will cons, and we might have no memory for that.  */
  if (NILP (Vmemory_full))
    {
      message_p = push_message ();
      record_unwind_protect_void (pop_message_unwind);
    }
  else
    message_p = false;

  /* Save a copy of the contents of the stack, for debugging.  */
#if MAX_SAVE_STACK > 0
  if (NILP (Vpurify_flag))
    {
      char const *stack;
      ptrdiff_t stack_size;
      if (&stack_top_variable < stack_bottom)
	{
	  stack = &stack_top_variable;
	  stack_size = stack_bottom - &stack_top_variable;
	}
      else
	{
	  stack = stack_bottom;
	  stack_size = &stack_top_variable - stack_bottom;
	}
      if (stack_size <= MAX_SAVE_STACK)
	{
	  if (stack_copy_size < stack_size)
	    {
	      stack_copy = xrealloc (stack_copy, stack_size);
	      stack_copy_size = stack_size;
	    }
	  no_sanitize_memcpy (stack_copy, stack, stack_size);
	}
    }
#endif /* MAX_SAVE_STACK > 0 */

  if (garbage_collection_messages)
    message1_nolog ("Garbage collecting...");

  block_input ();

  shrink_regexp_cache ();

  gc_in_progress = 1;

  /* Mark all the special slots that serve as the roots of accessibility.  */

  struct gc_root_visitor visitor = { .visit = mark_object_root_visitor };
  visit_static_gc_roots (visitor);

  mark_lread ();
  mark_terminals ();
  mark_kboards ();
  mark_threads ();
  mark_charset ();
  mark_composite ();
  mark_profiler ();
#ifdef HAVE_PGTK
  mark_pgtkterm ();
#endif

#ifdef USE_GTK
  xg_mark_data ();
#endif

#ifdef HAVE_HAIKU
  mark_haiku_display ();
#endif

#ifdef HAVE_WINDOW_SYSTEM
  mark_fringe_data ();
#endif

#ifdef HAVE_X_WINDOWS
  mark_xterm ();
  mark_xselect ();
#endif

#ifdef HAVE_ANDROID
  mark_androidterm ();
#ifndef ANDROID_STUBIFY
  mark_sfntfont ();
#endif
#endif

#ifdef HAVE_NS
  mark_nsterm ();
#endif
  mark_fns ();

  /* Everything is now marked, except for the data in font caches,
     undo lists, and finalizers.  The first two are compacted by
     removing any items which aren't reachable otherwise.  */

  compact_font_caches ();

  FOR_EACH_LIVE_BUFFER (tail, buffer)
    {
      struct buffer *nextb = XBUFFER (buffer);
      if (!EQ (BVAR (nextb, undo_list), Qt))
	bset_undo_list (nextb, compact_undo_list (BVAR (nextb, undo_list)));
      /* Now that we have stripped the elements that need not be
	 in the undo_list any more, we can finally mark the list.  */
      mark_object (BVAR (nextb, undo_list));
    }

  /* Now pre-sweep finalizers.  Here, we add any unmarked finalizers
     to doomed_finalizers so we can run their associated functions
     after GC.  It's important to scan finalizers at this stage so
     that we can be sure that unmarked finalizers are really
     unreachable except for references from their associated functions
     and from other finalizers.  */

  queue_doomed_finalizers (&doomed_finalizers, &finalizers);
  mark_finalizer_list (&doomed_finalizers);

  /* Must happen after all other marking and before gc_sweep.  */
  mark_and_sweep_weak_table_contents ();
  eassert (weak_hash_tables == NULL);

  eassert (mark_stack_empty_p ());

  gc_sweep ();

  unmark_main_thread ();

  gc_in_progress = 0;

  consing_until_gc = gc_threshold
    = consing_threshold (gc_cons_threshold, Vgc_cons_percentage, 0);

  /* Unblock *after* re-setting `consing_until_gc` in case `unblock_input`
     signals an error (see bug#43389).  */
  unblock_input ();

  if (garbage_collection_messages && NILP (Vmemory_full))
    {
      if (message_p || minibuf_level > 0)
	restore_message ();
      else
	message1_nolog ("Garbage collecting...done");
    }

  unbind_to (count, Qnil);

  /* GC is complete: now we can run our finalizer callbacks.  */
  run_finalizers (&doomed_finalizers);

#ifdef HAVE_WINDOW_SYSTEM
  /* Eject unused image cache entries.  */
  image_prune_animation_caches (false);
#endif

  /* Accumulate statistics.  */
  if (FLOATP (Vgc_elapsed))
    {
      static struct timespec gc_elapsed;
      gc_elapsed = timespec_add (gc_elapsed,
				 timespec_sub (current_timespec (), start));
      Vgc_elapsed = make_float (timespectod (gc_elapsed));
    }

  gcs_done++;

  /* Collect profiling data.  */
  if (tot_before != (byte_ct) -1)
    {
      byte_ct tot_after = total_bytes_of_live_objects ();
      if (tot_after < tot_before)
	malloc_probe (min (tot_before - tot_after, SIZE_MAX));
    }

  if (!NILP (Vpost_gc_hook))
    {
      specpdl_ref gc_count = inhibit_garbage_collection ();
      safe_run_hooks (Qpost_gc_hook);
      unbind_to (gc_count, Qnil);
    }
}

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
       doc: /* Reclaim storage for Lisp objects no longer needed.
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.
It returns the same info as `garbage-collect-heapsize'.

Note that calling this function does not guarantee that absolutely all
unreachable objects will be garbage-collected.  Emacs uses a
mark-and-sweep garbage collector, but is conservative when it comes to
collecting objects in some circumstances.

For further details, see Info node `(elisp)Garbage Collection'.  */)
  (void)
{
  if (garbage_collection_inhibited)
    return Qnil;

  specpdl_ref count = SPECPDL_INDEX ();
  specbind (Qsymbols_with_pos_enabled, Qnil);
  garbage_collect ();
  unbind_to (count, Qnil);
  return Fgarbage_collect_heapsize ();
}

DEFUN ("garbage-collect-heapsize", Fgarbage_collect_heapsize,
       Sgarbage_collect_heapsize, 0, 0, 0,
       doc: /* Return a list with info on amount of space in use.
This info may not be fully up to date unless it is called right after
a full garbage collection cycle.
Each entry has the form (NAME SIZE USED FREE), where:
- NAME is a symbol describing the kind of objects this entry represents,
- SIZE is the number of bytes used by each one,
- USED is the number of those objects that were found live in the heap,
- FREE is the number of those objects that are not live but that Emacs
  keeps around for future allocations (maybe because it does not know how
  to return them to the OS).  */)
  (void)
{
  struct gcstat gcst = gcstat;

  /* FIXME: Maybe we could/should add a field countaing the approximate
     amount of memory allocated since the last GC, such as
     'gc_threshold - consing_until_gc'.   */

  Lisp_Object total[] = {
    list4 (Qconses, make_fixnum (sizeof (struct Lisp_Cons)),
	   make_int (gcst.total_conses),
	   make_int (gcst.total_free_conses)),
    list4 (Qsymbols, make_fixnum (sizeof (struct Lisp_Symbol)),
	   make_int (gcst.total_symbols),
	   make_int (gcst.total_free_symbols)),
    list4 (Qstrings, make_fixnum (sizeof (struct Lisp_String)),
	   make_int (gcst.total_strings),
	   make_int (gcst.total_free_strings)),
    list3 (Qstring_bytes, make_fixnum (1),
	   make_int (gcst.total_string_bytes)),
    list3 (Qvectors,
	   make_fixnum (header_size + sizeof (Lisp_Object)),
	   make_int (gcst.total_vectors)),
    list4 (Qvector_slots, make_fixnum (word_size),
	   make_int (gcst.total_vector_slots),
	   make_int (gcst.total_free_vector_slots)),
    list4 (Qfloats, make_fixnum (sizeof (struct Lisp_Float)),
	   make_int (gcst.total_floats),
	   make_int (gcst.total_free_floats)),
    list4 (Qintervals, make_fixnum (sizeof (struct interval)),
	   make_int (gcst.total_intervals),
	   make_int (gcst.total_free_intervals)),
    list3 (Qbuffers, make_fixnum (sizeof (struct buffer)),
	   make_int (gcst.total_buffers)),

#ifdef DOUG_LEA_MALLOC
    list4 (Qheap, make_fixnum (1024),
	   make_int ((mallinfo ().uordblks + 1023) >> 10),
	   make_int ((mallinfo ().fordblks + 1023) >> 10)),
#endif
  };
  return CALLMANY (Flist, total);
}

DEFUN ("garbage-collect-maybe", Fgarbage_collect_maybe,
Sgarbage_collect_maybe, 1, 1, 0,
       doc: /* Call `garbage-collect' if enough allocation happened.
FACTOR determines what "enough" means here:
If FACTOR is a positive number N, it means to run GC if more than
1/Nth of the allocations needed to trigger automatic allocation took
place.
Therefore, as N gets higher, this is more likely to perform a GC.
Returns non-nil if GC happened, and nil otherwise.  */)
  (Lisp_Object factor)
{
  CHECK_FIXNAT (factor);
  EMACS_INT fact = XFIXNAT (factor);

  EMACS_INT since_gc = gc_threshold - consing_until_gc;
  if (fact >= 1 && since_gc > gc_threshold / fact)
    {
      garbage_collect ();
      return Qt;
    }
  else
    return Qnil;
}

/* Mark Lisp objects in glyph matrix MATRIX.  Currently the
   only interesting objects referenced from glyphs are strings.  */

static void
mark_glyph_matrix (struct glyph_matrix *matrix)
{
  struct glyph_row *row = matrix->rows;
  struct glyph_row *end = row + matrix->nrows;

  for (; row < end; ++row)
    if (row->enabled_p)
      {
	int area;
	for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	  {
	    struct glyph *glyph = row->glyphs[area];
	    struct glyph *end_glyph = glyph + row->used[area];

	    for (; glyph < end_glyph; ++glyph)
	      {
		if (STRINGP (glyph->object)
		    && !string_marked_p (XSTRING (glyph->object)))
		  mark_object (glyph->object);
	      }
	  }
      }
}

#if GC_REMEMBER_LAST_MARKED
/* Remember a few of the last marked values for debugging purposes.  */
enum { LAST_MARKED_SIZE = 1 << 9 }; /* Must be a power of 2.  */
extern Lisp_Object last_marked[LAST_MARKED_SIZE];
Lisp_Object last_marked[LAST_MARKED_SIZE] EXTERNALLY_VISIBLE;
static int last_marked_index;
#endif

/* Whether to enable the mark_object_loop_halt debugging feature.  */
#define GC_CDR_COUNT 0

#if GC_CDR_COUNT
/* For debugging--call abort when we cdr down this many
   links of a list, in mark_object.  In debugging,
   the call to abort will hit a breakpoint.
   Normally this is zero and the check never goes off.  */
ptrdiff_t mark_object_loop_halt EXTERNALLY_VISIBLE;
#endif

static void
mark_vectorlike (union vectorlike_header *header)
{
  struct Lisp_Vector *ptr = (struct Lisp_Vector *) header;
  ptrdiff_t size = ptr->header.size;

  eassert (!vector_marked_p (ptr));

  /* Bool vectors have a different case in mark_object.  */
  eassert (PSEUDOVECTOR_TYPE (ptr) != PVEC_BOOL_VECTOR);

  set_vector_marked (ptr); /* Else mark it.  */
  if (size & PSEUDOVECTOR_FLAG)
    size &= PSEUDOVECTOR_SIZE_MASK;

  /* Note that this size is not the memory-footprint size, but only
     the number of Lisp_Object fields that we should trace.
     The distinction is used e.g. by Lisp_Process which places extra
     non-Lisp_Object fields at the end of the structure...  */
  mark_objects (ptr->contents, size);
}

/* Like mark_vectorlike but optimized for char-tables (and
   sub-char-tables) assuming that the contents are mostly integers or
   symbols.  */

static void
mark_char_table (struct Lisp_Vector *ptr, enum pvec_type pvectype)
{
  int size = ptr->header.size & PSEUDOVECTOR_SIZE_MASK;
  /* Consult the Lisp_Sub_Char_Table layout before changing this.  */
  int i, idx = (pvectype == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0);

  eassert (!vector_marked_p (ptr));
  set_vector_marked (ptr);
  for (i = idx; i < size; i++)
    {
      Lisp_Object val = ptr->contents[i];

      if (FIXNUMP (val) ||
          (BARE_SYMBOL_P (val) && symbol_marked_p (XBARE_SYMBOL (val))))
	continue;
      if (SUB_CHAR_TABLE_P (val))
	{
	  if (! vector_marked_p (XVECTOR (val)))
	    mark_char_table (XVECTOR (val), PVEC_SUB_CHAR_TABLE);
	}
      else
	mark_object (val);
    }
}

/* Mark the chain of overlays starting at PTR.  */

static void
mark_overlay (struct Lisp_Overlay *ov)
{
  /* We don't mark the `itree_node` object, because it is managed manually
     rather than by the GC.  */
  eassert (BASE_EQ (ov->interval->data, make_lisp_ptr (ov, Lisp_Vectorlike)));
  set_vectorlike_marked (&ov->header);
  mark_object (ov->plist);
}

/* Mark the overlay subtree rooted at NODE.  */

static void
mark_overlays (struct itree_node *node)
{
  if (node == NULL)
    return;
  mark_object (node->data);
  mark_overlays (node->left);
  mark_overlays (node->right);
}

/* Mark Lisp_Objects and special pointers in BUFFER.  */

static void
mark_buffer (struct buffer *buffer)
{
  /* This is handled much like other pseudovectors...  */
  mark_vectorlike (&buffer->header);

  /* ...but there are some buffer-specific things.  */

  mark_interval_tree (buffer_intervals (buffer));

  /* For now, we just don't mark the undo_list.  It's done later in
     a special way just before the sweep phase, and after stripping
     some of its elements that are not needed any more.
     Note: this later processing is only done for live buffers, so
     for dead buffers, the undo_list should be nil (set by Fkill_buffer),
     but just to be on the safe side, we mark it here.  */
  if (!BUFFER_LIVE_P (buffer))
      mark_object (BVAR (buffer, undo_list));

  if (!itree_empty_p (buffer->overlays))
    mark_overlays (buffer->overlays->root);

  /* If this is an indirect buffer, mark its base buffer.  */
  if (buffer->base_buffer &&
      !vectorlike_marked_p (&buffer->base_buffer->header))
    mark_buffer (buffer->base_buffer);
}

/* Mark Lisp faces in the face cache C.  */

NO_INLINE /* To reduce stack depth in mark_object.  */
static void
mark_face_cache (struct face_cache *c)
{
  if (c)
    {
      for (int i = 0; i < c->used; i++)
	{
	  struct face *face = FACE_FROM_ID_OR_NULL (c->f, i);

	  if (face)
	    {
	      if (face->font && !vectorlike_marked_p (&face->font->header))
		mark_vectorlike (&face->font->header);

	      mark_objects (face->lface, LFACE_VECTOR_SIZE);
	    }
	}
    }
}

static void
mark_frame (struct Lisp_Vector *ptr)
{
  struct frame *f = (struct frame *) ptr;
#ifdef HAVE_TEXT_CONVERSION
  struct text_conversion_action *tem;
#endif


  mark_vectorlike (&ptr->header);
  mark_face_cache (f->face_cache);
#ifdef HAVE_WINDOW_SYSTEM
  if (FRAME_WINDOW_P (f) && FRAME_OUTPUT_DATA (f))
    {
      struct font *font = FRAME_FONT (f);

      if (font && !vectorlike_marked_p (&font->header))
        mark_vectorlike (&font->header);
    }
#endif

#ifdef HAVE_TEXT_CONVERSION
  mark_object (f->conversion.compose_region_start);
  mark_object (f->conversion.compose_region_end);
  mark_object (f->conversion.compose_region_overlay);
  mark_object (f->conversion.field);

  for (tem = f->conversion.actions; tem; tem = tem->next)
    mark_object (tem->data);
#endif

#ifdef HAVE_WINDOW_SYSTEM
  /* Mark this frame's image cache, though it might be common to several
     frames with the same font size.  */
  if (FRAME_IMAGE_CACHE (f))
    mark_image_cache (FRAME_IMAGE_CACHE (f));
#endif /* HAVE_WINDOW_SYSTEM */
}

static void
mark_window (struct Lisp_Vector *ptr)
{
  struct window *w = (struct window *) ptr;

  mark_vectorlike (&ptr->header);

  /* Mark glyph matrices, if any.  Marking window
     matrices is sufficient because frame matrices
     use the same glyph memory.  */
  if (w->current_matrix)
    {
      mark_glyph_matrix (w->current_matrix);
      mark_glyph_matrix (w->desired_matrix);
    }
}

/* Entry of the mark stack.  */
struct mark_entry
{
  ptrdiff_t n;		        /* number of values, or 0 if a single value */
  union {
    Lisp_Object value;		/* when n = 0 */
    Lisp_Object *values;	/* when n > 0 */
  } u;
};

/* This stack is used during marking for traversing data structures without
   using C recursion.  */
struct mark_stack
{
  struct mark_entry *stack;	/* base of stack */
  ptrdiff_t size;		/* allocated size in entries */
  ptrdiff_t sp;			/* current number of entries */
};

static struct mark_stack mark_stk = {NULL, 0, 0};

static inline bool
mark_stack_empty_p (void)
{
  return mark_stk.sp <= 0;
}

/* Pop and return a value from the mark stack (which must be nonempty).  */
static inline Lisp_Object
mark_stack_pop (void)
{
  eassume (!mark_stack_empty_p ());
  struct mark_entry *e = &mark_stk.stack[mark_stk.sp - 1];
  if (e->n == 0)		/* single value */
    {
      --mark_stk.sp;
      return e->u.value;
    }
  /* Array of values: pop them left to right, which seems to be slightly
     faster than right to left.  */
  e->n--;
  if (e->n == 0)
    --mark_stk.sp;		/* last value consumed */
  return (++e->u.values)[-1];
}

NO_INLINE static void
grow_mark_stack (void)
{
  struct mark_stack *ms = &mark_stk;
  eassert (ms->sp == ms->size);
  ptrdiff_t min_incr = ms->sp == 0 ? 8192 : 1;
  ms->stack = xpalloc (ms->stack, &ms->size, min_incr, -1, sizeof *ms->stack);
  eassert (ms->sp < ms->size);
}

/* Push VALUE onto the mark stack.  */
static inline void
mark_stack_push_value (Lisp_Object value)
{
  if (mark_stk.sp >= mark_stk.size)
    grow_mark_stack ();
  mark_stk.stack[mark_stk.sp++] = (struct mark_entry){.n = 0, .u.value = value};
}

/* Push the N values at VALUES onto the mark stack.  */
static inline void
mark_stack_push_values (Lisp_Object *values, ptrdiff_t n)
{
  eassume (n >= 0);
  if (n == 0)
    return;
  if (mark_stk.sp >= mark_stk.size)
    grow_mark_stack ();
  mark_stk.stack[mark_stk.sp++] = (struct mark_entry){.n = n,
						      .u.values = values};
}

/* When GC_CHECK_MARKED_OBJECTS is set, perform some sanity checks on
   the objects marked here.  Abort if we encounter an object we know is
   bogus.  This increases GC time by ~80%.  */

/* Check that the object pointed to by PO is alive, using predicate
   function LIVEP.  */
static inline void
check_live (bool (*livep) (struct mem_node *m, void *p), enum mem_type mtype,
	    void *po, struct mem_node *m)
{
  if (GC_CHECK_MARKED_OBJECTS)
    {
      if (pdumper_object_p (po))
	return;
      if (!(m->type == mtype && livep (m, po)))
	emacs_abort ();
    }
}

/* Check that the object pointed to by PO is known to be a Lisp
   structure allocated from the heap, and that it is alive.  */
static inline void
check_allocated_and_live (bool (*livep) (struct mem_node *m, void *p),
			  enum mem_type mtype,
			  void *po)
{
  if (GC_CHECK_MARKED_OBJECTS)
    {
      if (pdumper_object_p (po))
	{
	  if (!pdumper_object_p_precise (po))
	    emacs_abort ();
	  return;
	}
      struct mem_node *m = mem_find (po);
      if (m == MEM_NIL)
	emacs_abort ();
      check_live (livep, mtype, po, m);
    }
}

/* Like check_allocated_and_live but for symbols.  */
static inline void
check_allocated_and_live_symbol (void *po, struct Lisp_Symbol *sym)
{
  if (GC_CHECK_MARKED_OBJECTS)
    if (!c_symbol_p (sym))
      check_allocated_and_live (live_symbol_p, MEM_TYPE_SYMBOL, po);
}

/* Like check_allocated_and_live but for vectorlike.  */
static inline void
check_allocated_and_live_vectorlike (void *po, Lisp_Object obj)
{
  if (GC_CHECK_MARKED_OBJECTS)
    {
      if (!pdumper_object_p (po) && !SUBRP (obj) && !main_thread_p (po))
	{
	  struct mem_node *m = mem_find (po);
	  if (m == MEM_NIL)
	    emacs_abort ();
	  if (m->type == MEM_TYPE_VECTORLIKE)
	    check_live (live_large_vector_p, MEM_TYPE_VECTORLIKE, po, m);
	  else
	    check_live (live_small_vector_p, MEM_TYPE_VECTOR_BLOCK, po, m);
	}
    }
}

/* Traverse and mark objects on the mark stack above BASE_SP.

   Traversal is depth-first using the mark stack for most common
   object types.  Recursion is used for other types, in the hope that
   they are rare enough that C stack usage is kept low.  */
static void
process_mark_stack (ptrdiff_t base_sp)
{
#if GC_CDR_COUNT
  ptrdiff_t cdr_count = 0;
#endif

  eassume (mark_stk.sp >= base_sp && base_sp >= 0);

  while (mark_stk.sp > base_sp)
    {
      Lisp_Object obj = mark_stack_pop ();
    mark_obj: ;
      void *po = XPNTR (obj);
#if GC_REMEMBER_LAST_MARKED
      last_marked[last_marked_index++] = obj;
      last_marked_index &= LAST_MARKED_SIZE - 1;
#endif

      switch (XTYPE (obj))
	{
	case Lisp_String:
	  {
	    register struct Lisp_String *ptr = XSTRING (obj);
	    if (string_marked_p (ptr))
	      break;
	    check_allocated_and_live (live_string_p, MEM_TYPE_STRING, po);
	    set_string_marked (ptr);
	    mark_interval_tree (ptr->u.s.intervals);
#ifdef GC_CHECK_STRING_BYTES
	    /* Check that the string size recorded in the string is the
	       same as the one recorded in the sdata structure.  */
	    string_bytes (ptr);
#endif /* GC_CHECK_STRING_BYTES */
	  }
	  break;

	case Lisp_Vectorlike:
	  {
	    register struct Lisp_Vector *ptr = XVECTOR (obj);

	    if (vector_marked_p (ptr))
	      break;

	    enum pvec_type pvectype
	      = PSEUDOVECTOR_TYPE (ptr);

	    check_allocated_and_live_vectorlike (po, obj);

	    switch (pvectype)
	      {
	      case PVEC_BUFFER:
		mark_buffer ((struct buffer *) ptr);
		break;

	      case PVEC_FRAME:
		mark_frame (ptr);
		break;

	      case PVEC_WINDOW:
		mark_window (ptr);
		break;

	      case PVEC_HASH_TABLE:
		{
		  struct Lisp_Hash_Table *h = (struct Lisp_Hash_Table *)ptr;
		  set_vector_marked (ptr);
		  if (h->weakness == Weak_None)
		    /* The values pushed here may include
		       HASH_UNUSED_ENTRY_KEY, which this function must
		       cope with.  */
		    mark_stack_push_values (h->key_and_value,
					    2 * h->table_size);
		  else
		    {
		      /* For weak tables, don't mark the
			 contents --- that's what makes it weak.  */
		      eassert (h->next_weak == NULL);
		      h->next_weak = weak_hash_tables;
		      weak_hash_tables = h;
		    }
		  break;
		}

	      case PVEC_OBARRAY:
		{
		  struct Lisp_Obarray *o = (struct Lisp_Obarray *)ptr;
		  set_vector_marked (ptr);
		  mark_stack_push_values (o->buckets, obarray_size (o));
		  break;
		}

	      case PVEC_CHAR_TABLE:
	      case PVEC_SUB_CHAR_TABLE:
		mark_char_table (ptr, (enum pvec_type) pvectype);
		break;

	      case PVEC_BOOL_VECTOR:
		/* bool vectors in a dump are permanently "marked", since
		   they're in the old section and don't have mark bits.
		   If we're looking at a dumped bool vector, we should
		   have aborted above when we called vector_marked_p, so
		   we should never get here.  */
		eassert (!pdumper_object_p (ptr));
		set_vector_marked (ptr);
		break;

	      case PVEC_OVERLAY:
		mark_overlay (XOVERLAY (obj));
		break;

	      case PVEC_SUBR:
#ifdef HAVE_NATIVE_COMP
		if (NATIVE_COMP_FUNCTIONP (obj))
		  {
		    set_vector_marked (ptr);
		    struct Lisp_Subr *subr = XSUBR (obj);
		    mark_stack_push_value (subr->intspec.native);
		    mark_stack_push_value (subr->command_modes);
		    mark_stack_push_value (subr->native_comp_u);
		    mark_stack_push_value (subr->lambda_list);
		    mark_stack_push_value (subr->type);
		  }
#endif
		break;

	      case PVEC_FREE:
		emacs_abort ();

	      default:
		{
		  /* A regular vector or pseudovector needing no special
		     treatment.  */
		  ptrdiff_t size = ptr->header.size;
		  if (size & PSEUDOVECTOR_FLAG)
		    size &= PSEUDOVECTOR_SIZE_MASK;
		  set_vector_marked (ptr);
		  mark_stack_push_values (ptr->contents, size);
		}
		break;
	      }
	  }
	  break;

	case Lisp_Symbol:
	  {
	    struct Lisp_Symbol *ptr = XBARE_SYMBOL (obj);
	  nextsym:
	    if (symbol_marked_p (ptr))
	      break;
	    check_allocated_and_live_symbol (po, ptr);
	    set_symbol_marked (ptr);
	    /* Attempt to catch bogus objects.  */
	    eassert (valid_lisp_object_p (ptr->u.s.function));
	    mark_stack_push_value (ptr->u.s.function);
	    mark_stack_push_value (ptr->u.s.plist);
	    switch (ptr->u.s.redirect)
	      {
	      case SYMBOL_PLAINVAL:
		mark_stack_push_value (SYMBOL_VAL (ptr));
		break;
	      case SYMBOL_VARALIAS:
		{
		  Lisp_Object tem;
		  XSETSYMBOL (tem, SYMBOL_ALIAS (ptr));
		  mark_stack_push_value (tem);
		  break;
		}
	      case SYMBOL_LOCALIZED:
		{
		  struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (ptr);
		  Lisp_Object where = blv->where;
		  /* If the value is set up for a killed buffer,
		     restore its global binding.  */
		  if (BUFFERP (where) && !BUFFER_LIVE_P (XBUFFER (where)))
		    swap_in_global_binding (ptr);
		  mark_stack_push_value (blv->where);
		  mark_stack_push_value (blv->valcell);
		  mark_stack_push_value (blv->defcell);
		}
		break;
	      case SYMBOL_FORWARDED:
		/* If the value is forwarded to a buffer or keyboard field,
		   these are marked when we see the corresponding object.
		   And if it's forwarded to a C variable, either it's not
		   a Lisp_Object var, or it's staticpro'd already, or it's
		   reachable from font_style_table which is also
		   staticpro'd.  */
		break;
	      default: emacs_abort ();
	      }
	    set_string_marked (XSTRING (ptr->u.s.name));
	    mark_interval_tree (string_intervals (ptr->u.s.name));
	    /* Inner loop to mark next symbol in this bucket, if any.  */
	    ptr = ptr->u.s.next;
	    if (GC_CHECK_MARKED_OBJECTS)
	      po = ptr;
	    if (ptr)
	      goto nextsym;
	  }
	  break;

	case Lisp_Cons:
	  {
	    struct Lisp_Cons *ptr = XCONS (obj);
	    if (cons_marked_p (ptr))
	      break;
	    check_allocated_and_live (live_cons_p, MEM_TYPE_CONS, po);
	    set_cons_marked (ptr);
	    /* Avoid growing the stack if the cdr is nil.
	       In any case, make sure the car is expanded first.  */
	    if (!NILP (ptr->u.s.u.cdr))
	      {
		mark_stack_push_value (ptr->u.s.u.cdr);
#if GC_CDR_COUNT
		cdr_count++;
		if (cdr_count == mark_object_loop_halt)
		  emacs_abort ();
#endif
	      }
	    /* Speedup hack for the common case (successive list elements).  */
	    obj = ptr->u.s.car;
	    goto mark_obj;
	  }

	case Lisp_Float:
	  {
	    struct Lisp_Float *f = XFLOAT (obj);
	    if (!f)
	      break;		/* for HASH_UNUSED_ENTRY_KEY */
	    check_allocated_and_live (live_float_p, MEM_TYPE_FLOAT, po);
	    /* Do not mark floats stored in a dump image: these floats are
	       "cold" and do not have mark bits.  */
	    if (pdumper_object_p (f))
	      eassert (pdumper_cold_object_p (f));
	    else if (!XFLOAT_MARKED_P (f))
	      XFLOAT_MARK (f);
	    break;
	  }

	case Lisp_Int0:
	case Lisp_Int1:
	  break;

	default:
	  emacs_abort ();
	}
    }
}

void
mark_object (Lisp_Object obj)
{
  ptrdiff_t sp = mark_stk.sp;
  mark_stack_push_value (obj);
  process_mark_stack (sp);
}

void
mark_objects (Lisp_Object *objs, ptrdiff_t n)
{
  ptrdiff_t sp = mark_stk.sp;
  mark_stack_push_values (objs, n);
  process_mark_stack (sp);
}

/* Mark the Lisp pointers in the terminal objects.
   Called by Fgarbage_collect.  */

static void
mark_terminals (void)
{
  struct terminal *t;
  for (t = terminal_list; t; t = t->next_terminal)
    {
      eassert (t->name != NULL);
      if (!vectorlike_marked_p (&t->header))
	mark_vectorlike (&t->header);
    }
}

/* Value is non-zero if OBJ will survive the current GC because it's
   either marked or does not need to be marked to survive.  */

bool
survives_gc_p (Lisp_Object obj)
{
  bool survives_p;

  switch (XTYPE (obj))
    {
    case Lisp_Int0:
    case Lisp_Int1:
      survives_p = true;
      break;

    case Lisp_Symbol:
      survives_p = symbol_marked_p (XBARE_SYMBOL (obj));
      break;

    case Lisp_String:
      survives_p = string_marked_p (XSTRING (obj));
      break;

    case Lisp_Vectorlike:
      survives_p =
	(SUBRP (obj) && !NATIVE_COMP_FUNCTIONP (obj)) ||
	vector_marked_p (XVECTOR (obj));
      break;

    case Lisp_Cons:
      survives_p = cons_marked_p (XCONS (obj));
      break;

    case Lisp_Float:
      survives_p =
        XFLOAT_MARKED_P (XFLOAT (obj)) ||
        pdumper_object_p (XFLOAT (obj));
      break;

    default:
      emacs_abort ();
    }

  return survives_p;
}




NO_INLINE /* For better stack traces */
static void
sweep_conses (void)
{
  struct cons_block **cprev = &cons_block;
  int lim = cons_block_index;
  object_ct num_free = 0, num_used = 0;

  cons_free_list = 0;

  for (struct cons_block *cblk; (cblk = *cprev); )
    {
      int i = 0;
      int this_free = 0;
      int ilim = (lim + BITS_PER_BITS_WORD - 1) / BITS_PER_BITS_WORD;

      /* Scan the mark bits an int at a time.  */
      for (i = 0; i < ilim; i++)
        {
          if (cblk->gcmarkbits[i] == BITS_WORD_MAX)
            {
              /* Fast path - all cons cells for this int are marked.  */
              cblk->gcmarkbits[i] = 0;
              num_used += BITS_PER_BITS_WORD;
            }
          else
            {
              /* Some cons cells for this int are not marked.
                 Find which ones, and free them.  */
              int start, pos, stop;

              start = i * BITS_PER_BITS_WORD;
              stop = lim - start;
              if (stop > BITS_PER_BITS_WORD)
                stop = BITS_PER_BITS_WORD;
              stop += start;

              for (pos = start; pos < stop; pos++)
                {
		  struct Lisp_Cons *acons = &cblk->conses[pos];
		  if (!XCONS_MARKED_P (acons))
                    {
		      ASAN_UNPOISON_CONS (&cblk->conses[pos]);
                      this_free++;
                      cblk->conses[pos].u.s.u.chain = cons_free_list;
                      cons_free_list = &cblk->conses[pos];
                      cons_free_list->u.s.car = dead_object ();
		      ASAN_POISON_CONS (&cblk->conses[pos]);
		    }
                  else
                    {
                      num_used++;
		      XUNMARK_CONS (acons);
                    }
                }
            }
        }

      lim = CONS_BLOCK_SIZE;
      /* If this block contains only free conses and we have already
         seen more than two blocks worth of free conses then deallocate
         this block.  */
      if (this_free == CONS_BLOCK_SIZE && num_free > CONS_BLOCK_SIZE)
        {
          *cprev = cblk->next;
          /* Unhook from the free list.  */
	  ASAN_UNPOISON_CONS (&cblk->conses[0]);
          cons_free_list = cblk->conses[0].u.s.u.chain;
          lisp_align_free (cblk);
        }
      else
        {
          num_free += this_free;
          cprev = &cblk->next;
        }
    }
  gcstat.total_conses = num_used;
  gcstat.total_free_conses = num_free;
}

NO_INLINE /* For better stack traces */
static void
sweep_floats (void)
{
  struct float_block **fprev = &float_block;
  int lim = float_block_index;
  object_ct num_free = 0, num_used = 0;

  float_free_list = 0;

  for (struct float_block *fblk; (fblk = *fprev); )
    {
      int this_free = 0;
      ASAN_UNPOISON_FLOAT_BLOCK (fblk);
      for (int i = 0; i < lim; i++)
	{
	  struct Lisp_Float *afloat = &fblk->floats[i];
	  if (!XFLOAT_MARKED_P (afloat))
	    {
	      this_free++;
	      fblk->floats[i].u.chain = float_free_list;
	      ASAN_POISON_FLOAT (&fblk->floats[i]);
	      float_free_list = &fblk->floats[i];
	    }
	  else
	    {
	      num_used++;
	      XFLOAT_UNMARK (afloat);
	    }
	}
      lim = FLOAT_BLOCK_SIZE;
      /* If this block contains only free floats and we have already
         seen more than two blocks worth of free floats then deallocate
         this block.  */
      if (this_free == FLOAT_BLOCK_SIZE && num_free > FLOAT_BLOCK_SIZE)
        {
          *fprev = fblk->next;
          /* Unhook from the free list.  */
	  ASAN_UNPOISON_FLOAT (&fblk->floats[0]);
	  float_free_list = fblk->floats[0].u.chain;
          lisp_align_free (fblk);
        }
      else
        {
          num_free += this_free;
          fprev = &fblk->next;
        }
    }
  gcstat.total_floats = num_used;
  gcstat.total_free_floats = num_free;
}

NO_INLINE /* For better stack traces */
static void
sweep_intervals (void)
{
  struct interval_block **iprev = &interval_block;
  int lim = interval_block_index;
  object_ct num_free = 0, num_used = 0;

  interval_free_list = 0;

  for (struct interval_block *iblk; (iblk = *iprev); )
    {
      int this_free = 0;
      ASAN_UNPOISON_INTERVAL_BLOCK (iblk);
      for (int i = 0; i < lim; i++)
        {
          if (!iblk->intervals[i].gcmarkbit)
            {
              set_interval_parent (&iblk->intervals[i], interval_free_list);
              interval_free_list = &iblk->intervals[i];
	      ASAN_POISON_INTERVAL (&iblk->intervals[i]);
              this_free++;
            }
          else
            {
              num_used++;
              iblk->intervals[i].gcmarkbit = 0;
            }
        }
      lim = INTERVAL_BLOCK_SIZE;
      /* If this block contains only free intervals and we have already
         seen more than two blocks worth of free intervals then
         deallocate this block.  */
      if (this_free == INTERVAL_BLOCK_SIZE && num_free > INTERVAL_BLOCK_SIZE)
        {
          *iprev = iblk->next;
          /* Unhook from the free list.  */
	  ASAN_UNPOISON_INTERVAL (&iblk->intervals[0]);
          interval_free_list = INTERVAL_PARENT (&iblk->intervals[0]);
          lisp_free (iblk);
        }
      else
        {
          num_free += this_free;
          iprev = &iblk->next;
        }
    }
  gcstat.total_intervals = num_used;
  gcstat.total_free_intervals = num_free;
}

NO_INLINE /* For better stack traces */
static void
sweep_symbols (void)
{
  struct symbol_block *sblk;
  struct symbol_block **sprev = &symbol_block;
  int lim = symbol_block_index;
  object_ct num_free = 0, num_used = ARRAYELTS (lispsym);

  symbol_free_list = NULL;

  for (int i = 0; i < ARRAYELTS (lispsym); i++)
    lispsym[i].u.s.gcmarkbit = 0;

  for (sblk = symbol_block; sblk; sblk = *sprev)
    {
      ASAN_UNPOISON_SYMBOL_BLOCK (sblk);

      int this_free = 0;
      struct Lisp_Symbol *sym = sblk->symbols;
      struct Lisp_Symbol *end = sym + lim;

      for (; sym < end; ++sym)
        {
          if (!sym->u.s.gcmarkbit)
            {
              if (sym->u.s.redirect == SYMBOL_LOCALIZED)
		{
                  xfree (SYMBOL_BLV (sym));
                  /* At every GC we sweep all symbol_blocks and rebuild the
                     symbol_free_list, so those symbols which stayed unused
                     between the two will be re-swept.
                     So we have to make sure we don't re-free this blv next
                     time we sweep this symbol_block (bug#29066).  */
                  sym->u.s.redirect = SYMBOL_PLAINVAL;
                }
              sym->u.s.next = symbol_free_list;
              symbol_free_list = sym;
              symbol_free_list->u.s.function = dead_object ();
	      ASAN_POISON_SYMBOL (sym);
	      ++this_free;
            }
          else
            {
              ++num_used;
              sym->u.s.gcmarkbit = 0;
              /* Attempt to catch bogus objects.  */
              eassert (valid_lisp_object_p (sym->u.s.function));
            }
        }

      lim = SYMBOL_BLOCK_SIZE;
      /* If this block contains only free symbols and we have already
         seen more than two blocks worth of free symbols then deallocate
         this block.  */
      if (this_free == SYMBOL_BLOCK_SIZE && num_free > SYMBOL_BLOCK_SIZE)
        {
          *sprev = sblk->next;
          /* Unhook from the free list.  */
	  ASAN_UNPOISON_SYMBOL (&sblk->symbols[0]);
          symbol_free_list = sblk->symbols[0].u.s.next;
          lisp_free (sblk);
        }
      else
        {
          num_free += this_free;
          sprev = &sblk->next;
        }
    }
  gcstat.total_symbols = num_used;
  gcstat.total_free_symbols = num_free;
}

/* Remove BUFFER's markers that are due to be swept.  This is needed since
   we treat BUF_MARKERS and markers's `next' field as weak pointers.  */
static void
unchain_dead_markers (struct buffer *buffer)
{
  struct Lisp_Marker *this, **prev = &BUF_MARKERS (buffer);

  while ((this = *prev))
    if (vectorlike_marked_p (&this->header))
      prev = &this->next;
    else
      {
        this->buffer = NULL;
        *prev = this->next;
      }
}

NO_INLINE /* For better stack traces */
static void
sweep_buffers (void)
{
  Lisp_Object tail, buf;

  gcstat.total_buffers = 0;
  FOR_EACH_LIVE_BUFFER (tail, buf)
    {
      struct buffer *buffer = XBUFFER (buf);
      /* Do not use buffer_(set|get)_intervals here.  */
      buffer->text->intervals = balance_intervals (buffer->text->intervals);
      unchain_dead_markers (buffer);
      gcstat.total_buffers++;
    }
}

/* Sweep: find all structures not marked, and free them.  */
static void
gc_sweep (void)
{
  sweep_strings ();
  check_string_bytes (!noninteractive);
  sweep_conses ();
  sweep_floats ();
  sweep_intervals ();
  sweep_symbols ();
  sweep_buffers ();
  sweep_vectors ();
  pdumper_clear_marks ();
  check_string_bytes (!noninteractive);
}

DEFUN ("memory-info", Fmemory_info, Smemory_info, 0, 0, 0,
       doc: /* Return a list of (TOTAL-RAM FREE-RAM TOTAL-SWAP FREE-SWAP).
All values are in Kbytes.  If there is no swap space,
last two values are zero.  If the system is not supported
or memory information can't be obtained, return nil.
If `default-directory' is remote, return memory information of the
respective remote host.  */)
  (void)
{
  Lisp_Object handler
    = Ffind_file_name_handler (BVAR (current_buffer, directory),
			       Qmemory_info);
  if (!NILP (handler))
    return calln (handler, Qmemory_info);

#if defined HAVE_LINUX_SYSINFO
  struct sysinfo si;
  uintmax_t units;

  if (sysinfo (&si))
    return Qnil;
#ifdef LINUX_SYSINFO_UNIT
  units = si.mem_unit;
#else
  units = 1;
#endif
  return list4i ((uintmax_t) si.totalram * units / 1024,
		 (uintmax_t) si.freeram * units / 1024,
		 (uintmax_t) si.totalswap * units / 1024,
		 (uintmax_t) si.freeswap * units / 1024);
#elif defined WINDOWSNT
  unsigned long long totalram, freeram, totalswap, freeswap;

  if (w32_memory_info (&totalram, &freeram, &totalswap, &freeswap) == 0)
    return list4i ((uintmax_t) totalram / 1024,
		   (uintmax_t) freeram / 1024,
		   (uintmax_t) totalswap / 1024,
		   (uintmax_t) freeswap / 1024);
  else
    return Qnil;
#elif defined MSDOS
  unsigned long totalram, freeram, totalswap, freeswap;

  if (dos_memory_info (&totalram, &freeram, &totalswap, &freeswap) == 0)
    return list4i ((uintmax_t) totalram / 1024,
		   (uintmax_t) freeram / 1024,
		   (uintmax_t) totalswap / 1024,
		   (uintmax_t) freeswap / 1024);
  else
    return Qnil;
#else /* not HAVE_LINUX_SYSINFO, not WINDOWSNT, not MSDOS */
  /* FIXME: add more systems.  */
  return Qnil;
#endif /* HAVE_LINUX_SYSINFO, not WINDOWSNT, not MSDOS */
}

/* Debugging aids.  */

DEFUN ("memory-use-counts", Fmemory_use_counts, Smemory_use_counts, 0, 0, 0,
       doc: /* Return a list of counters that measure how much consing there has been.
Each of these counters increments for a certain kind of object.
The counters wrap around from the largest positive integer to zero.
Garbage collection does not decrease them.
The elements of the value are as follows:
  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)
All are in units of 1 = one object consed
except for VECTOR-CELLS and STRING-CHARS, which count the total length of
objects consed.
Frames, windows, buffers, and subprocesses count as vectors
  (but the contents of a buffer's text do not count here).  */)
  (void)
{
  return  list (make_int (cons_cells_consed),
		make_int (floats_consed),
		make_int (vector_cells_consed),
		make_int (symbols_consed),
		make_int (string_chars_consed),
		make_int (intervals_consed),
		make_int (strings_consed));
}

#if defined GNU_LINUX && defined __GLIBC__ && \
  (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 10)
DEFUN ("malloc-info", Fmalloc_info, Smalloc_info, 0, 0, "",
       doc: /* Report malloc information to stderr.
This function outputs to stderr an XML-formatted
description of the current state of the memory-allocation
arenas.  */)
  (void)
{
  if (malloc_info (0, stderr))
    error ("malloc_info failed: %s", emacs_strerror (errno));
  return Qnil;
}
#endif

#ifdef HAVE_MALLOC_TRIM
DEFUN ("malloc-trim", Fmalloc_trim, Smalloc_trim, 0, 1, "",
       doc: /* Release free heap memory to the OS.
This function asks libc to return unused heap memory back to the operating
system.  This function isn't guaranteed to do anything, and is mainly
meant as a debugging tool.

If LEAVE_PADDING is given, ask the system to leave that much unused
space in the heap of the Emacs process.  This should be an integer, and if
not given, it defaults to 0.

This function returns nil if no memory could be returned to the
system, and non-nil if some memory could be returned.  */)
  (Lisp_Object leave_padding)
{
  int pad = 0;

  if (! NILP (leave_padding))
    {
      CHECK_FIXNAT (leave_padding);
      pad = XFIXNUM (leave_padding);
    }

  /* 1 means that memory was released to the system.  */
  if (malloc_trim (pad) == 1)
    return Qt;
  else
    return Qnil;
}
#endif

static bool
symbol_uses_obj (Lisp_Object symbol, Lisp_Object obj)
{
  struct Lisp_Symbol *sym = XBARE_SYMBOL (symbol);
  Lisp_Object val = find_symbol_value (symbol);
  return (EQ (val, obj)
	  || EQ (sym->u.s.function, obj)
	  || (!NILP (sym->u.s.function)
	      && CLOSUREP (sym->u.s.function)
	      && EQ (AREF (sym->u.s.function, CLOSURE_CODE), obj))
	  || (!NILP (val)
	      && CLOSUREP (val)
	      && EQ (AREF (val, CLOSURE_CODE), obj)));
}

/* Find at most FIND_MAX symbols which have OBJ as their value or
   function.  This is used in gdbinit's `xwhichsymbols' command.  */

Lisp_Object
which_symbols (Lisp_Object obj, EMACS_INT find_max)
{
   struct symbol_block *sblk;
   specpdl_ref gc_count = inhibit_garbage_collection ();
   Lisp_Object found = Qnil;

   if (! deadp (obj))
     {
       for (int i = 0; i < ARRAYELTS (lispsym); i++)
	 {
	   Lisp_Object sym = builtin_lisp_symbol (i);
	   if (symbol_uses_obj (sym, obj))
	     {
	       found = Fcons (sym, found);
	       if (--find_max == 0)
		 goto out;
	     }
	 }

       for (sblk = symbol_block; sblk; sblk = sblk->next)
	 {
	   struct Lisp_Symbol *asym = sblk->symbols;
	   int bn;

	   for (bn = 0; bn < SYMBOL_BLOCK_SIZE; bn++, asym++)
	     {
	       if (sblk == symbol_block && bn >= symbol_block_index)
		 break;

	       Lisp_Object sym = make_lisp_symbol (asym);
	       if (symbol_uses_obj (sym, obj))
		 {
		   found = Fcons (sym, found);
		   if (--find_max == 0)
		     goto out;
		 }
	     }
	 }
     }

  out:
   return unbind_to (gc_count, found);
}

#ifdef ENABLE_CHECKING

bool suppress_checking;

void
die (const char *msg, const char *file, int line)
{
  fprintf (stderr, "\r\n%s:%d: Emacs fatal error: assertion failed: %s\r\n",
	   file, line, msg);
  terminate_due_to_signal (SIGABRT, INT_MAX);
}

#endif /* ENABLE_CHECKING */

#if defined (ENABLE_CHECKING) && USE_STACK_LISP_OBJECTS

/* Stress alloca with inconveniently sized requests and check
   whether all allocated areas may be used for Lisp_Object.  */

NO_INLINE static void
verify_alloca (void)
{
  int i;
  enum { ALLOCA_CHECK_MAX = 256 };
  /* Start from size of the smallest Lisp object.  */
  for (i = sizeof (struct Lisp_Cons); i <= ALLOCA_CHECK_MAX; i++)
    {
      void *ptr = alloca (i);
      make_lisp_ptr (ptr, Lisp_Cons);
    }
}

#else /* not ENABLE_CHECKING && USE_STACK_LISP_OBJECTS */

#define verify_alloca() ((void) 0)

#endif /* ENABLE_CHECKING && USE_STACK_LISP_OBJECTS */

/* Initialization.  */

static void init_alloc_once_for_pdumper (void);

void
init_alloc_once (void)
{
  gc_cons_threshold = GC_DEFAULT_THRESHOLD;
  /* Even though Qt's contents are not set up, its address is known.  */
  Vpurify_flag = Qt;

  PDUMPER_REMEMBER_SCALAR (buffer_defaults.header);
  PDUMPER_REMEMBER_SCALAR (buffer_local_symbols.header);

  /* Call init_alloc_once_for_pdumper now so we run mem_init early.
     Keep in mind that when we reload from a dump, we'll run _only_
     init_alloc_once_for_pdumper and not init_alloc_once at all.  */
  pdumper_do_now_and_after_load (init_alloc_once_for_pdumper);

  verify_alloca ();

  init_strings ();
  init_vectors ();
}

static void
init_alloc_once_for_pdumper (void)
{
  mem_init ();

#ifdef DOUG_LEA_MALLOC
  mallopt (M_TRIM_THRESHOLD, 128 * 1024); /* Trim threshold.  */
  mallopt (M_MMAP_THRESHOLD, 64 * 1024);  /* Mmap threshold.  */
  mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);   /* Max. number of mmap'ed areas.  */
#endif


  init_finalizer_list (&finalizers);
  init_finalizer_list (&doomed_finalizers);
  refill_memory_reserve ();
}

void
init_alloc (void)
{
  Vgc_elapsed = make_float (0.0);
  gcs_done = 0;
}

void
syms_of_alloc (void)
{
  DEFVAR_INT ("gc-cons-threshold", gc_cons_threshold,
	      doc: /* Number of bytes of consing between garbage collections.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically only when `eval' is called.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.  But be
sure to get back to the normal value soon enough, to avoid system-wide
memory pressure, and never use a too-high value for prolonged periods
of time.
See also `gc-cons-percentage'.  */);

  DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
	       doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.  But be
sure to get back to the normal value soon enough, to avoid system-wide
memory pressure, and never use a too-high value for prolonged periods
of time.

If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
  Vgc_cons_percentage = make_float (0.1);

  DEFVAR_INT ("pure-bytes-used", pure_bytes_used,
	      doc: /* No longer used.  */);

  DEFVAR_INT ("cons-cells-consed", cons_cells_consed,
	      doc: /* Number of cons cells that have been consed so far.  */);

  DEFVAR_INT ("floats-consed", floats_consed,
	      doc: /* Number of floats that have been consed so far.  */);

  DEFVAR_INT ("vector-cells-consed", vector_cells_consed,
	      doc: /* Number of vector cells that have been consed so far.  */);

  DEFVAR_INT ("symbols-consed", symbols_consed,
	      doc: /* Number of symbols that have been consed so far.  */);
  symbols_consed += ARRAYELTS (lispsym);

  DEFVAR_INT ("string-chars-consed", string_chars_consed,
	      doc: /* Number of string characters that have been consed so far.  */);

  DEFVAR_INT ("intervals-consed", intervals_consed,
	      doc: /* Number of intervals that have been consed so far.  */);

  DEFVAR_INT ("strings-consed", strings_consed,
	      doc: /* Number of strings that have been consed so far.  */);

  DEFVAR_LISP ("purify-flag", Vpurify_flag,
	       doc: /* Non-nil means loading Lisp code in order to dump an executable.
This used to mean that certain objects should be allocated in shared
(pure) space, but objects are not allocated in pure storage any more.
This flag is still used in a few places, not to decide where objects are
allocated, but to know if we're in the preload phase of Emacs's
build.  */);

  DEFVAR_BOOL ("garbage-collection-messages", garbage_collection_messages,
	       doc: /* Non-nil means display messages at start and end of garbage collection.  */);
  garbage_collection_messages = 0;

  DEFVAR_LISP ("post-gc-hook", Vpost_gc_hook,
	       doc: /* Hook run after garbage collection has finished.  */);
  Vpost_gc_hook = Qnil;
  DEFSYM (Qpost_gc_hook, "post-gc-hook");

  DEFVAR_LISP ("memory-signal-data", Vmemory_signal_data,
	       doc: /* Precomputed `signal' argument for memory-full error.  */);
  /* We build this in advance because if we wait until we need it, we might
     not be able to allocate the memory to hold it.  */
  Vmemory_signal_data
    = list (Qerror,
	    build_string ("Memory exhausted--use"
			  " M-x save-some-buffers then"
			  " exit and restart Emacs"));

  DEFVAR_LISP ("memory-full", Vmemory_full,
	       doc: /* Non-nil means Emacs cannot get much more Lisp memory.  */);
  Vmemory_full = Qnil;

  DEFSYM (Qmemory_info, "memory-info");

  DEFSYM (Qconses, "conses");
  DEFSYM (Qsymbols, "symbols");
  DEFSYM (Qstrings, "strings");
  DEFSYM (Qvectors, "vectors");
  DEFSYM (Qfloats, "floats");
  DEFSYM (Qintervals, "intervals");
  DEFSYM (Qbuffers, "buffers");
  DEFSYM (Qstring_bytes, "string-bytes");
  DEFSYM (Qvector_slots, "vector-slots");
  DEFSYM (Qheap, "heap");
  DEFSYM (QAutomatic_GC, "Automatic GC");

  DEFSYM (Qgc_cons_percentage, "gc-cons-percentage");
  DEFSYM (Qgc_cons_threshold, "gc-cons-threshold");
  DEFSYM (Qchar_table_extra_slots, "char-table-extra-slots");

  DEFVAR_LISP ("gc-elapsed", Vgc_elapsed,
	       doc: /* Accumulated time elapsed in garbage collections.
The time is in seconds as a floating point value.  */);
  DEFVAR_INT ("gcs-done", gcs_done,
              doc: /* Accumulated number of garbage collections done.  */);

  DEFVAR_INT ("integer-width", integer_width,
	      doc: /* Maximum number N of bits in safely-calculated integers.
Integers with absolute values less than 2**N do not signal a range error.
N should be nonnegative.  */);

  defsubr (&Scons);
  defsubr (&Slist);
  defsubr (&Svector);
  defsubr (&Srecord);
  defsubr (&Sbool_vector);
  defsubr (&Smake_byte_code);
  defsubr (&Smake_closure);
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_record);
  defsubr (&Smake_string);
  defsubr (&Smake_bool_vector);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Smake_finalizer);
  defsubr (&Sgarbage_collect);
  defsubr (&Sgarbage_collect_maybe);
  defsubr (&Sgarbage_collect_heapsize);
  defsubr (&Smemory_info);
  defsubr (&Smemory_use_counts);
#if defined GNU_LINUX && defined __GLIBC__ && \
  (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 10)

  defsubr (&Smalloc_info);
#endif
#ifdef HAVE_MALLOC_TRIM
  defsubr (&Smalloc_trim);
#endif

  Lisp_Object watcher;

  static union Aligned_Lisp_Subr Swatch_gc_cons_threshold =
     {{{ PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) },
       { .a4 = watch_gc_cons_threshold },
       4, 4, "watch_gc_cons_threshold", {0}, lisp_h_Qnil}};
  XSETSUBR (watcher, &Swatch_gc_cons_threshold.s);
  Fadd_variable_watcher (Qgc_cons_threshold, watcher);

  static union Aligned_Lisp_Subr Swatch_gc_cons_percentage =
     {{{ PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) },
       { .a4 = watch_gc_cons_percentage },
       4, 4, "watch_gc_cons_percentage", {0}, lisp_h_Qnil}};
  XSETSUBR (watcher, &Swatch_gc_cons_percentage.s);
  Fadd_variable_watcher (Qgc_cons_percentage, watcher);
  DEFSYM (Qalloc, "alloc");
  DEFSYM (QCemergency, ":emergency");
}

/* The below is for being able to do platform-specific stuff in .gdbinit
   without risking error messages from GDB about missing types and
   variables on other platforms.  */
#ifdef HAVE_X_WINDOWS
enum defined_HAVE_X_WINDOWS { defined_HAVE_X_WINDOWS = true };
#else
enum defined_HAVE_X_WINDOWS { defined_HAVE_X_WINDOWS = false };
#endif

#ifdef HAVE_PGTK
enum defined_HAVE_PGTK { defined_HAVE_PGTK = true };
#else
enum defined_HAVE_PGTK { defined_HAVE_PGTK = false };
#endif

#ifdef WINDOWSNT
enum defined_WINDOWSNT { defined_WINDOWSNT = true };
#else
enum defined_WINDOWSNT { defined_WINDOWSNT = false };
#endif

/* When compiled with GCC, GDB might say "No enum type named
   pvec_type" if we don't have at least one symbol with that type, and
   then xbacktrace could fail.  Similarly for the other enums and
   their values.  Some non-GCC compilers don't like these constructs.  */
#ifdef __GNUC__
extern union enums_for_gdb
{
  enum CHARTAB_SIZE_BITS CHARTAB_SIZE_BITS;
  enum char_table_specials char_table_specials;
  enum char_bits char_bits;
  enum CHECK_LISP_OBJECT_TYPE CHECK_LISP_OBJECT_TYPE;
  enum DEFAULT_HASH_SIZE DEFAULT_HASH_SIZE;
  enum Lisp_Bits Lisp_Bits;
  enum Lisp_Closure Lisp_Closure;
  enum maxargs maxargs;
  enum MAX_ALLOCA MAX_ALLOCA;
  enum More_Lisp_Bits More_Lisp_Bits;
  enum pvec_type pvec_type;
  enum defined_HAVE_X_WINDOWS defined_HAVE_X_WINDOWS;
  enum defined_HAVE_PGTK defined_HAVE_PGTK;
  enum defined_WINDOWSNT defined_WINDOWSNT;
} const gdb_make_enums_visible;
union enums_for_gdb const EXTERNALLY_VISIBLE gdb_make_enums_visible = {0};
#endif	/* __GNUC__ */
