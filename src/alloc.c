/* Storage allocation and gc for GNU Emacs Lisp interpreter.

Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2020 Free Software
Foundation, Inc.

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
#include "ptr-bounds.h"
#include "puresize.h"
#include "sheap.h"
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
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#include <flexmember.h>
#include <verify.h>
#include <execinfo.h>           /* For backtrace.  */

#ifdef HAVE_LINUX_SYSINFO
#include <sys/sysinfo.h>
#endif

#ifdef MSDOS
#include "dosfns.h"		/* For dos_memory_info.  */
#endif

#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif

#if defined HAVE_VALGRIND_VALGRIND_H && !defined USE_VALGRIND
# define USE_VALGRIND 1
#endif

#if USE_VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#endif

/* GC_CHECK_MARKED_OBJECTS means do sanity checks on allocated objects.
   We turn that on by default when ENABLE_CHECKING is defined;
   define GC_CHECK_MARKED_OBJECTS to zero to disable.  */

#if defined ENABLE_CHECKING && !defined GC_CHECK_MARKED_OBJECTS
# define GC_CHECK_MARKED_OBJECTS 1
#endif

/* GC_MALLOC_CHECK defined means perform validity checks of malloc'd
   memory.  Can do this only if using gmalloc.c and if not checking
   marked objects.  */

#if (defined SYSTEM_MALLOC || defined DOUG_LEA_MALLOC \
     || defined HYBRID_MALLOC || GC_CHECK_MARKED_OBJECTS)
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

#ifdef DOUG_LEA_MALLOC

/* Specify maximum number of areas to mmap.  It would be nice to use a
   value that explicitly means "no limit".  */

# define MMAP_MAX_AREAS 100000000

/* A pointer to the memory allocated that copies that static data
   inside glibc's malloc.  */
static void *malloc_state_ptr;

/* Restore the dumped malloc state.  Because malloc can be invoked
   even before main (e.g. by the dynamic linker), the dumped malloc
   state must be restored as early as possible using this special hook.  */
static void
malloc_initialize_hook (void)
{
  static bool malloc_using_checking;

  if (! initialized)
    {
# ifdef GNU_LINUX
      my_heap_start ();
# endif
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

      if (malloc_set_state (malloc_state_ptr) != 0)
	emacs_abort ();
      alloc_unexec_post ();
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

#if defined DOUG_LEA_MALLOC || defined HAVE_UNEXEC

/* Allocator-related actions to do just before and after unexec.  */

void
alloc_unexec_pre (void)
{
# ifdef DOUG_LEA_MALLOC
  malloc_state_ptr = malloc_get_state ();
  if (!malloc_state_ptr)
    fatal ("malloc_get_state: %s", strerror (errno));
# endif
}

void
alloc_unexec_post (void)
{
# ifdef DOUG_LEA_MALLOC
  free (malloc_state_ptr);
# endif
}

# ifdef GNU_LINUX

/* The address where the heap starts.  */
void *
my_heap_start (void)
{
  static void *start;
  if (! start)
    start = sbrk (0);
  return start;
}
# endif

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
} gcstat;

/* Points to memory space allocated as "spare", to be freed if we run
   out of memory.  We keep one large block, four cons-blocks, and
   two string blocks.  */

static char *spare_memory[7];

/* Amount of spare memory to keep in large reserve block, or to see
   whether this much is available when malloc fails on a larger request.  */

#define SPARE_MEMORY (1 << 14)

/* Initialize it to a nonzero value to force it into data space
   (rather than bss space).  That way unexec will remap it into text
   space (pure), on some systems.  We have not implemented the
   remapping on more recent systems because this is less important
   nowadays than in the days of small memories and timesharing.  */

EMACS_INT pure[(PURESIZE + sizeof (EMACS_INT) - 1) / sizeof (EMACS_INT)] = {1,};
#define PUREBEG (char *) pure

/* Pointer to the pure area, and its size.  */

static char *purebeg;
static ptrdiff_t pure_size;

/* Number of bytes of pure storage used before pure storage overflowed.
   If this is non-zero, this implies that an overflow occurred.  */

static ptrdiff_t pure_bytes_used_before_overflow;

/* Index in pure at which next pure Lisp object will be allocated..  */

static ptrdiff_t pure_bytes_used_lisp;

/* Number of bytes allocated for non-Lisp objects in pure storage.  */

static ptrdiff_t pure_bytes_used_non_lisp;

/* If positive, garbage collection is inhibited.  Otherwise, zero.  */

static intptr_t garbage_collection_inhibited;

/* The GC threshold in bytes, the last time it was calculated
   from gc-cons-threshold and gc-cons-percentage.  */
static EMACS_INT gc_threshold;

/* If nonzero, this is a warning delivered by malloc and not yet
   displayed.  */

const char *pending_malloc_warning;

/* Pointer sanity only on request.  FIXME: Code depending on
   SUSPICIOUS_OBJECT_CHECKING is obsolete; remove it entirely.  */
#ifdef ENABLE_CHECKING
#define SUSPICIOUS_OBJECT_CHECKING 1
#endif

#ifdef SUSPICIOUS_OBJECT_CHECKING
struct suspicious_free_record
{
  void *suspicious_object;
  void *backtrace[128];
};
static void *suspicious_objects[32];
static int suspicious_object_index;
struct suspicious_free_record suspicious_free_history[64] EXTERNALLY_VISIBLE;
static int suspicious_free_history_index;
/* Find the first currently-monitored suspicious pointer in range
   [begin,end) or NULL if no such pointer exists.  */
static void *find_suspicious_object_in_range (void *begin, void *end);
static void detect_suspicious_free (void *ptr);
#else
# define find_suspicious_object_in_range(begin, end) ((void *) NULL)
# define detect_suspicious_free(ptr) ((void) 0)
#endif

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

static void unchain_finalizer (struct Lisp_Finalizer *);
static void mark_terminals (void);
static void gc_sweep (void);
static Lisp_Object make_pure_vector (ptrdiff_t);
static void mark_buffer (struct buffer *);

#if !defined REL_ALLOC || defined SYSTEM_MALLOC || defined HYBRID_MALLOC
static void refill_memory_reserve (void);
#endif
static void compact_small_strings (void);
static void free_large_strings (void);
extern Lisp_Object which_symbols (Lisp_Object, EMACS_INT) EXTERNALLY_VISIBLE;

/* Forward declare mark accessor functions: they're used all over the
   place.  */

inline static bool vector_marked_p (const struct Lisp_Vector *v);
inline static void set_vector_marked (struct Lisp_Vector *v);

inline static bool vectorlike_marked_p (const union vectorlike_header *v);
inline static void set_vectorlike_marked (union vectorlike_header *v);

inline static bool cons_marked_p (const struct Lisp_Cons *c);
inline static void set_cons_marked (struct Lisp_Cons *c);

inline static bool string_marked_p (const struct Lisp_String *s);
inline static void set_string_marked (struct Lisp_String *s);

inline static bool symbol_marked_p (const struct Lisp_Symbol *s);
inline static void set_symbol_marked (struct Lisp_Symbol *s);

inline static bool interval_marked_p (INTERVAL i);
inline static void set_interval_marked (INTERVAL i);

/* When scanning the C stack for live Lisp objects, Emacs keeps track of
   what memory allocated via lisp_malloc and lisp_align_malloc is intended
   for what purpose.  This enumeration specifies the type of memory.  */

enum mem_type
{
  MEM_TYPE_NON_LISP,
  MEM_TYPE_BUFFER,
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
  return EQ (x, dead_object ());
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

/* Addresses of staticpro'd variables.  Initialize it to a nonzero
   value if we might unexec; otherwise some compilers put it into
   BSS.  */

Lisp_Object const *staticvec[NSTATICS]
#ifdef HAVE_UNEXEC
= {&Vpurify_flag}
#endif
  ;

/* Index of next unused slot in staticvec.  */

int staticidx;

static void *pure_alloc (size_t, int);

/* Return PTR rounded up to the next multiple of ALIGNMENT.  */

static void *
pointer_align (void *ptr, int alignment)
{
  return (void *) ROUNDUP ((uintptr_t) ptr, alignment);
}

/* Extract the pointer hidden within O.  */

static ATTRIBUTE_NO_SANITIZE_UNDEFINED void *
XPNTR (Lisp_Object a)
{
  return (SYMBOLP (a)
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
     over our address space.  We also can't use mmap for lisp objects
     if we might dump: unexec doesn't preserve the contents of mmapped
     regions.  */
  return pointers_fit_in_lispobj_p () && !will_dump_with_unexec_p ();
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

#if defined SIGDANGER || (!defined SYSTEM_MALLOC && !defined HYBRID_MALLOC)

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
  call3 (intern ("display-warning"),
	 intern ("alloc"),
	 build_string (pending_malloc_warning),
	 intern ("emergency"));
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

/* LISP_ALIGNMENT is the alignment of Lisp objects.  It must be at
   least GCALIGNMENT so that pointers can be tagged.  It also must be
   at least as strict as the alignment of all the C types used to
   implement Lisp objects; since pseudovectors can contain any C type,
   this is max_align_t.  On recent GNU/Linux x86 and x86-64 this can
   often waste up to 8 bytes, since alignof (max_align_t) is 16 but
   typical vectors need only an alignment of 8.  Although shrinking
   the alignment to 8 would save memory, it cost a 20% hit to Emacs
   CPU performance on Fedora 28 x86-64 when compiled with gcc -m32.  */
enum { LISP_ALIGNMENT = alignof (union { max_align_t x;
					 GCALIGNED_UNION_MEMBER }) };
verify (LISP_ALIGNMENT % GCALIGNMENT == 0);

/* True if malloc (N) is known to return storage suitably aligned for
   Lisp objects whenever N is a multiple of LISP_ALIGNMENT.  In
   practice this is true whenever alignof (max_align_t) is also a
   multiple of LISP_ALIGNMENT.  This works even for x86, where some
   platform combinations (e.g., GCC 7 and later, glibc 2.25 and
   earlier) have bugs where alignof (max_align_t) is 16 even though
   the malloc alignment is only 8, and where Emacs still works because
   it never does anything that requires an alignment of 16.  */
enum { MALLOC_IS_LISP_ALIGNED = alignof (max_align_t) % LISP_ALIGNMENT == 0 };

/* If compiled with XMALLOC_BLOCK_INPUT_CHECK, define a symbol
   BLOCK_INPUT_IN_MEMORY_ALLOCATORS that is visible to the debugger.
   If that variable is set, block input while in one of Emacs's memory
   allocation functions.  There should be no need for this debugging
   option, since signal handlers do not allocate memory, but Emacs
   formerly allocated memory in signal handlers and this compile-time
   option remains as a way to help debug the issue should it rear its
   ugly head again.  */
#ifdef XMALLOC_BLOCK_INPUT_CHECK
bool block_input_in_memory_allocators EXTERNALLY_VISIBLE;
static void
malloc_block_input (void)
{
  if (block_input_in_memory_allocators)
    block_input ();
}
static void
malloc_unblock_input (void)
{
  if (block_input_in_memory_allocators)
    unblock_input ();
}
# define MALLOC_BLOCK_INPUT malloc_block_input ()
# define MALLOC_UNBLOCK_INPUT malloc_unblock_input ()
#else
# define MALLOC_BLOCK_INPUT ((void) 0)
# define MALLOC_UNBLOCK_INPUT ((void) 0)
#endif

#define MALLOC_PROBE(size)			\
  do {						\
    if (profiler_memory_running)		\
      malloc_probe (size);			\
  } while (0)

static void *lmalloc (size_t) ATTRIBUTE_MALLOC_SIZE ((1));
static void *lrealloc (void *, size_t);

/* Like malloc but check for no memory and block interrupt input.  */

void *
xmalloc (size_t size)
{
  void *val;

  MALLOC_BLOCK_INPUT;
  val = lmalloc (size);
  MALLOC_UNBLOCK_INPUT;

  if (!val && size)
    memory_full (size);
  MALLOC_PROBE (size);
  return val;
}

/* Like the above, but zeroes out the memory just allocated.  */

void *
xzalloc (size_t size)
{
  void *val;

  MALLOC_BLOCK_INPUT;
  val = lmalloc (size);
  MALLOC_UNBLOCK_INPUT;

  if (!val && size)
    memory_full (size);
  memset (val, 0, size);
  MALLOC_PROBE (size);
  return val;
}

/* Like realloc but check for no memory and block interrupt input.  */

void *
xrealloc (void *block, size_t size)
{
  void *val;

  MALLOC_BLOCK_INPUT;
  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  if (! block)
    val = lmalloc (size);
  else
    val = lrealloc (block, size);
  MALLOC_UNBLOCK_INPUT;

  if (!val && size)
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
  MALLOC_BLOCK_INPUT;
  free (block);
  MALLOC_UNBLOCK_INPUT;
  /* We don't call refill_memory_reserve here
     because in practice the call in r_alloc_free seems to suffice.  */
}


/* Other parts of Emacs pass large int values to allocator functions
   expecting ptrdiff_t.  This is portable in practice, but check it to
   be safe.  */
verify (INT_MAX <= PTRDIFF_MAX);


/* Allocate an array of NITEMS items, each of size ITEM_SIZE.
   Signal an error on memory exhaustion, and block interrupt input.  */

void *
xnmalloc (ptrdiff_t nitems, ptrdiff_t item_size)
{
  eassert (0 <= nitems && 0 < item_size);
  ptrdiff_t nbytes;
  if (INT_MULTIPLY_WRAPV (nitems, item_size, &nbytes) || SIZE_MAX < nbytes)
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
  if (INT_MULTIPLY_WRAPV (nitems, item_size, &nbytes) || SIZE_MAX < nbytes)
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
  if (INT_ADD_WRAPV (n0, n0 >> 1, &n))
    n = PTRDIFF_MAX;
  if (0 <= nitems_max && nitems_max < n)
    n = nitems_max;

  ptrdiff_t adjusted_nbytes
    = ((INT_MULTIPLY_WRAPV (n, item_size, &nbytes) || SIZE_MAX < nbytes)
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
      && (INT_ADD_WRAPV (n0, nitems_incr_min, &n)
	  || (0 <= nitems_max && nitems_max < n)
	  || INT_MULTIPLY_WRAPV (n, item_size, &nbytes)))
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


/* Like malloc but used for allocating Lisp data.  NBYTES is the
   number of bytes to allocate, TYPE describes the intended use of the
   allocated memory block (for strings, for conses, ...).  */

#if ! USE_LSB_TAG
void *lisp_malloc_loser EXTERNALLY_VISIBLE;
#endif

static void *
lisp_malloc (size_t nbytes, enum mem_type type)
{
  register void *val;

  MALLOC_BLOCK_INPUT;

#ifdef GC_MALLOC_CHECK
  allocated_mem_type = type;
#endif

  val = lmalloc (nbytes);

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

  MALLOC_UNBLOCK_INPUT;
  if (!val && nbytes)
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

  MALLOC_BLOCK_INPUT;
  free (block);
#ifndef GC_MALLOC_CHECK
  mem_delete (mem_find (block));
#endif
  MALLOC_UNBLOCK_INPUT;
}

/*****  Allocation of aligned blocks of memory to store Lisp data.  *****/

/* The entry point is lisp_align_malloc which returns blocks of at most
   BLOCK_BYTES and guarantees they are aligned on a BLOCK_ALIGN boundary.  */

/* Byte alignment of storage blocks.  */
#define BLOCK_ALIGN (1 << 10)
verify (POWER_OF_2 (BLOCK_ALIGN));

/* Use aligned_alloc if it or a simple substitute is available.
   Aligned allocation is incompatible with unexmacosx.c, so don't use
   it on Darwin if HAVE_UNEXEC.  */

#if ! (defined DARWIN_OS && defined HAVE_UNEXEC)
# if (defined HAVE_ALIGNED_ALLOC					\
      || (defined HYBRID_MALLOC						\
	  ? defined HAVE_POSIX_MEMALIGN					\
	  : !defined SYSTEM_MALLOC && !defined DOUG_LEA_MALLOC))
#  define USE_ALIGNED_ALLOC 1
# elif !defined HYBRID_MALLOC && defined HAVE_POSIX_MEMALIGN
#  define USE_ALIGNED_ALLOC 1
#  define aligned_alloc my_aligned_alloc /* Avoid collision with lisp.h.  */
static void *
aligned_alloc (size_t alignment, size_t size)
{
  /* POSIX says the alignment must be a power-of-2 multiple of sizeof (void *).
     Verify this for all arguments this function is given.  */
  verify (BLOCK_ALIGN % sizeof (void *) == 0
	  && POWER_OF_2 (BLOCK_ALIGN / sizeof (void *)));
  verify (MALLOC_IS_LISP_ALIGNED
	  || (LISP_ALIGNMENT % sizeof (void *) == 0
	      && POWER_OF_2 (LISP_ALIGNMENT / sizeof (void *))));
  eassert (alignment == BLOCK_ALIGN
	   || (!MALLOC_IS_LISP_ALIGNED && alignment == LISP_ALIGNMENT));

  void *p;
  return posix_memalign (&p, alignment, size) == 0 ? p : 0;
}
# endif
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

/* The list of free ablock.   */
static struct ablock *free_ablock;

/* Allocate an aligned block of nbytes.
   Alignment is on a multiple of BLOCK_ALIGN and `nbytes' has to be
   smaller or equal to BLOCK_BYTES.  */
static void *
lisp_align_malloc (size_t nbytes, enum mem_type type)
{
  void *base, *val;
  struct ablocks *abase;

  eassert (nbytes <= BLOCK_BYTES);

  MALLOC_BLOCK_INPUT;

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
      verify (ABLOCKS_BYTES % BLOCK_ALIGN == 0);
      abase = base = aligned_alloc (BLOCK_ALIGN, ABLOCKS_BYTES);
#else
      base = malloc (ABLOCKS_BYTES);
      abase = pointer_align (base, BLOCK_ALIGN);
#endif

      if (base == 0)
	{
	  MALLOC_UNBLOCK_INPUT;
	  memory_full (ABLOCKS_BYTES);
	}

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
	      MALLOC_UNBLOCK_INPUT;
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

  abase = ABLOCK_ABASE (free_ablock);
  ABLOCKS_BUSY (abase)
    = (struct ablocks *) (2 + (intptr_t) ABLOCKS_BUSY (abase));
  val = free_ablock;
  free_ablock = free_ablock->x.next_free;

#ifndef GC_MALLOC_CHECK
  if (type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type);
#endif

  MALLOC_UNBLOCK_INPUT;

  MALLOC_PROBE (nbytes);

  eassert (0 == ((uintptr_t) val) % BLOCK_ALIGN);
  return val;
}

static void
lisp_align_free (void *block)
{
  struct ablock *ablock = block;
  struct ablocks *abase = ABLOCK_ABASE (ablock);

  MALLOC_BLOCK_INPUT;
#ifndef GC_MALLOC_CHECK
  mem_delete (mem_find (block));
#endif
  /* Put on free list.  */
  ablock->x.next_free = free_ablock;
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
  MALLOC_UNBLOCK_INPUT;
}

/* True if a malloc-returned pointer P is suitably aligned for SIZE,
   where Lisp object alignment may be needed if SIZE is a multiple of
   LISP_ALIGNMENT.  */

static bool
laligned (void *p, size_t size)
{
  return (MALLOC_IS_LISP_ALIGNED || (intptr_t) p % LISP_ALIGNMENT == 0
	  || size % LISP_ALIGNMENT != 0);
}

/* Like malloc and realloc except that if SIZE is Lisp-aligned, make
   sure the result is too, if necessary by reallocating (typically
   with larger and larger sizes) until the allocator returns a
   Lisp-aligned pointer.  Code that needs to allocate C heap memory
   for a Lisp object should use one of these functions to obtain a
   pointer P; that way, if T is an enum Lisp_Type value and L ==
   make_lisp_ptr (P, T), then XPNTR (L) == P and XTYPE (L) == T.

   On typical modern platforms these functions' loops do not iterate.
   On now-rare (and perhaps nonexistent) platforms, the loops in
   theory could repeat forever.  If an infinite loop is possible on a
   platform, a build would surely loop and the builder can then send
   us a bug report.  Adding a counter to try to detect any such loop
   would complicate the code (and possibly introduce bugs, in code
   that's never really exercised) for little benefit.  */

static void *
lmalloc (size_t size)
{
#ifdef USE_ALIGNED_ALLOC
  if (! MALLOC_IS_LISP_ALIGNED && size % LISP_ALIGNMENT == 0)
    return aligned_alloc (LISP_ALIGNMENT, size);
#endif

  while (true)
    {
      void *p = malloc (size);
      if (laligned (p, size))
	return p;
      free (p);
      size_t bigger = size + LISP_ALIGNMENT;
      if (size < bigger)
	size = bigger;
    }
}

static void *
lrealloc (void *p, size_t size)
{
  while (true)
    {
      p = realloc (p, size);
      if (laligned (p, size))
	return p;
      size_t bigger = size + LISP_ALIGNMENT;
      if (size < bigger)
	size = bigger;
    }
}


/***********************************************************************
			 Interval Allocation
 ***********************************************************************/

/* Number of intervals allocated in an interval_block structure.
   The 1020 is 1024 minus malloc overhead.  */

#define INTERVAL_BLOCK_SIZE \
  ((1020 - sizeof (struct interval_block *)) / sizeof (struct interval))

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

/* Return a new interval.  */

INTERVAL
make_interval (void)
{
  INTERVAL val;

  MALLOC_BLOCK_INPUT;

  if (interval_free_list)
    {
      val = interval_free_list;
      interval_free_list = INTERVAL_PARENT (interval_free_list);
    }
  else
    {
      if (interval_block_index == INTERVAL_BLOCK_SIZE)
	{
	  struct interval_block *newi
	    = lisp_malloc (sizeof *newi, MEM_TYPE_NON_LISP);

	  newi->next = interval_block;
	  interval_block = newi;
	  interval_block_index = 0;
	}
      val = &interval_block->intervals[interval_block_index++];
    }

  MALLOC_UNBLOCK_INPUT;

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

/* Size in bytes of an sblock structure used for small strings.  This
   is 8192 minus malloc overhead.  */

#define SBLOCK_SIZE 8188

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

/* Number of Lisp strings in a string_block structure.  The 1020 is
   1024 minus malloc overhead.  */

#define STRING_BLOCK_SIZE \
  ((1020 - sizeof (struct string_block *)) / sizeof (struct Lisp_String))

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

#define SDATA_OF_STRING(S) ((sdata *) ptr_bounds_init ((S)->u.s.data \
						       - SDATA_DATA_OFFSET))


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
   terminating NUL.  A string cannot contain more bytes than
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

static void
init_strings (void)
{
  empty_unibyte_string = make_pure_string ("", 0, 0, 0);
  staticpro (&empty_unibyte_string);
  empty_multibyte_string = make_pure_string ("", 0, 0, 1);
  staticpro (&empty_multibyte_string);
}


#ifdef GC_CHECK_STRING_BYTES

static int check_string_bytes_count;

/* Like STRING_BYTES, but with debugging check.  Can be
   called during GC, so pay attention to the mark bit.  */

ptrdiff_t
string_bytes (struct Lisp_String *s)
{
  ptrdiff_t nbytes =
    (s->u.s.size_byte < 0 ? s->u.s.size & ~ARRAY_MARK_FLAG : s->u.s.size_byte);

  if (!PURE_P (s) && !pdumper_object_p (s) && s->u.s.data
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

  MALLOC_BLOCK_INPUT;

  /* If the free-list is empty, allocate a new string_block, and
     add all the Lisp_Strings in it to the free-list.  */
  if (string_free_list == NULL)
    {
      struct string_block *b = lisp_malloc (sizeof *b, MEM_TYPE_STRING);
      int i;

      b->next = string_blocks;
      string_blocks = b;

      for (i = STRING_BLOCK_SIZE - 1; i >= 0; --i)
	{
	  s = b->strings + i;
	  /* Every string on a free list should have NULL data pointer.  */
	  s->u.s.data = NULL;
	  NEXT_FREE_LISP_STRING (s) = string_free_list;
	  string_free_list = ptr_bounds_clip (s, sizeof *s);
	}
    }

  check_string_free_list ();

  /* Pop a Lisp_String off the free-list.  */
  s = string_free_list;
  string_free_list = NEXT_FREE_LISP_STRING (s);

  MALLOC_UNBLOCK_INPUT;

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
   to NBYTES.  Free S->u.s.data if it was initially non-null.  */

void
allocate_string_data (struct Lisp_String *s,
		      EMACS_INT nchars, EMACS_INT nbytes)
{
  sdata *data, *old_data;
  struct sblock *b;
  ptrdiff_t old_nbytes;

  if (STRING_BYTES_MAX < nbytes)
    string_overflow ();

  /* Determine the number of bytes needed to store NBYTES bytes
     of string data.  */
  ptrdiff_t needed = sdata_size (nbytes);
  if (s->u.s.data)
    {
      old_data = SDATA_OF_STRING (s);
      old_nbytes = STRING_BYTES (s);
    }
  else
    old_data = NULL;

  MALLOC_BLOCK_INPUT;

  if (nbytes > LARGE_STRING_BYTES)
    {
      size_t size = FLEXSIZEOF (struct sblock, data, needed);

#ifdef DOUG_LEA_MALLOC
      if (!mmap_lisp_allowed_p ())
        mallopt (M_MMAP_MAX, 0);
#endif

      b = lisp_malloc (size + GC_STRING_EXTRA, MEM_TYPE_NON_LISP);

#ifdef DOUG_LEA_MALLOC
      if (!mmap_lisp_allowed_p ())
        mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif

      data = b->data;
      b->next = large_sblocks;
      b->next_free = data;
      large_sblocks = b;
    }
  else if (current_sblock == NULL
	   || (((char *) current_sblock + SBLOCK_SIZE
		- (char *) current_sblock->next_free)
	       < (needed + GC_STRING_EXTRA)))
    {
      /* Not enough room in the current sblock.  */
      b = lisp_malloc (SBLOCK_SIZE, MEM_TYPE_NON_LISP);
      data = b->data;
      b->next = NULL;
      b->next_free = data;

      if (current_sblock)
	current_sblock->next = b;
      else
	oldest_sblock = b;
      current_sblock = b;
    }
  else
    {
      b = current_sblock;
      data = b->next_free;
    }

  data->string = s;
  b->next_free = (sdata *) ((char *) data + needed + GC_STRING_EXTRA);
  eassert ((uintptr_t) b->next_free % alignof (sdata) == 0);

  MALLOC_UNBLOCK_INPUT;

  s->u.s.data = ptr_bounds_clip (SDATA_DATA (data), nbytes + 1);
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

  /* Note that Faset may call to this function when S has already data
     assigned.  In this case, mark data as free by setting it's string
     back-pointer to null, and record the size of the data in it.  */
  if (old_data)
    {
      SDATA_NBYTES (old_data) = old_nbytes;
      old_data->string = NULL;
    }

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

      next = b->next;

      for (i = 0; i < STRING_BLOCK_SIZE; ++i)
	{
	  struct Lisp_String *s = b->strings + i;

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
		  string_free_list = ptr_bounds_clip (s, sizeof *s);
		  ++nfree;
		}
	    }
	  else
	    {
	      /* S was on the free-list before.  Put it there again.  */
	      NEXT_FREE_LISP_STRING (s) = string_free_list;
	      string_free_list = ptr_bounds_clip (s, sizeof *s);
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
		      memmove (to, from, size + GC_STRING_EXTRA);
		      to->string->u.s.data
			= ptr_bounds_clip (SDATA_DATA (to), nbytes + 1);
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

DEFUN ("make-string", Fmake_string, Smake_string, 2, 3, 0,
       doc: /* Return a newly created string of length LENGTH, with INIT in each element.
LENGTH must be an integer.
INIT must be an integer that represents a character.
If optional argument MULTIBYTE is non-nil, the result will be
a multibyte string even if INIT is an ASCII character.  */)
  (Lisp_Object length, Lisp_Object init, Lisp_Object multibyte)
{
  register Lisp_Object val;
  int c;
  EMACS_INT nbytes;

  CHECK_FIXNAT (length);
  CHECK_CHARACTER (init);

  c = XFIXNAT (init);
  if (ASCII_CHAR_P (c) && NILP (multibyte))
    {
      nbytes = XFIXNUM (length);
      val = make_uninit_string (nbytes);
      if (nbytes)
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
      unsigned char *p, *beg, *end;

      if (INT_MULTIPLY_WRAPV (len, string_len, &nbytes))
	string_overflow ();
      val = make_uninit_multibyte_string (string_len, nbytes);
      for (beg = SDATA (val), p = beg, end = beg + nbytes; p < end; p += len)
	{
	  /* First time we just copy `str' to the data of `val'.  */
	  if (p == beg)
	    memcpy (p, str, len);
	  else
	    {
	      /* Next time we copy largest possible chunk from
		 initialized to uninitialized part of `val'.  */
	      len = min (p - beg, end - p);
	      memcpy (p, beg, len);
	    }
	}
      if (nbytes)
	*p = 0;
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

/* Return a newly allocated, uninitialized bool vector of size NBITS.  */

Lisp_Object
make_uninit_bool_vector (EMACS_INT nbits)
{
  Lisp_Object val;
  EMACS_INT words = bool_vector_words (nbits);
  EMACS_INT word_bytes = words * sizeof (bits_word);
  EMACS_INT needed_elements = ((bool_header_size - header_size + word_bytes
				+ word_size - 1)
			       / word_size);
  if (PTRDIFF_MAX < needed_elements)
    memory_full (SIZE_MAX);
  struct Lisp_Bool_Vector *p
    = (struct Lisp_Bool_Vector *) allocate_vector (needed_elements);
  XSETVECTOR (val, p);
  XSETPVECTYPESIZE (XVECTOR (val), PVEC_BOOL_VECTOR, 0, 0);
  p->size = nbits;

  /* Clear padding at the end.  */
  if (words)
    p->data[words - 1] = 0;

  return val;
}

DEFUN ("make-bool-vector", Fmake_bool_vector, Smake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
  (Lisp_Object length, Lisp_Object init)
{
  Lisp_Object val;

  CHECK_FIXNAT (length);
  val = make_uninit_bool_vector (XFIXNAT (length));
  return bool_vector_fill (val, init);
}

DEFUN ("bool-vector", Fbool_vector, Sbool_vector, 0, MANY, 0,
       doc: /* Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (bool-vector &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i;
  Lisp_Object vector;

  vector = make_uninit_bool_vector (nargs);
  for (i = 0; i < nargs; i++)
    bool_vector_set (vector, i, !NILP (args[i]));

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
   occupying LENGTH bytes.  */

Lisp_Object
make_uninit_string (EMACS_INT length)
{
  Lisp_Object val;

  if (!length)
    return empty_unibyte_string;
  val = make_uninit_multibyte_string (length, length);
  STRING_SET_UNIBYTE (val);
  return val;
}


/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  */

Lisp_Object
make_uninit_multibyte_string (EMACS_INT nchars, EMACS_INT nbytes)
{
  Lisp_Object string;
  struct Lisp_String *s;

  if (nchars < 0)
    emacs_abort ();
  if (!nbytes)
    return empty_multibyte_string;

  s = allocate_string ();
  s->u.s.intervals = NULL;
  allocate_string_data (s, nchars, nbytes);
  XSETSTRING (string, s);
  string_chars_consed += nbytes;
  return string;
}

/* Print arguments to BUF according to a FORMAT, then return
   a Lisp_String initialized with the data from BUF.  */

Lisp_Object
make_formatted_string (char *buf, const char *format, ...)
{
  va_list ap;
  int length;

  va_start (ap, format);
  length = vsprintf (buf, format, ap);
  va_end (ap);
  return make_string (buf, length);
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
  GETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX ((fptr)))

#define XFLOAT_MARK(fptr) \
  SETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX ((fptr)))

#define XFLOAT_UNMARK(fptr) \
  UNSETMARKBIT (FLOAT_BLOCK (fptr), FLOAT_INDEX ((fptr)))

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

  MALLOC_BLOCK_INPUT;

  if (float_free_list)
    {
      XSETFLOAT (val, float_free_list);
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
	  float_block = new;
	  float_block_index = 0;
	}
      XSETFLOAT (val, &float_block->floats[float_block_index]);
      float_block_index++;
    }

  MALLOC_UNBLOCK_INPUT;

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
  GETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX ((fptr)))

#define XMARK_CONS(fptr) \
  SETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX ((fptr)))

#define XUNMARK_CONS(fptr) \
  UNSETMARKBIT (CONS_BLOCK (fptr), CONS_INDEX ((fptr)))

/* Minimum number of bytes of consing since GC before next GC,
   when memory is full.  */

enum { memory_full_cons_threshold = sizeof (struct cons_block) };

/* Current cons_block.  */

static struct cons_block *cons_block;

/* Index of first unused Lisp_Cons in the current block.  */

static int cons_block_index = CONS_BLOCK_SIZE;

/* Free-list of Lisp_Cons structures.  */

static struct Lisp_Cons *cons_free_list;

/* Explicitly free a cons cell by putting it on the free-list.  */

void
free_cons (struct Lisp_Cons *ptr)
{
  ptr->u.s.u.chain = cons_free_list;
  ptr->u.s.car = dead_object ();
  cons_free_list = ptr;
  ptrdiff_t nbytes = sizeof *ptr;
  tally_consing (-nbytes);
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
  (Lisp_Object car, Lisp_Object cdr)
{
  register Lisp_Object val;

  MALLOC_BLOCK_INPUT;

  if (cons_free_list)
    {
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
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	}
      XSETCONS (val, &cons_block->conses[cons_block_index]);
      cons_block_index++;
    }

  MALLOC_UNBLOCK_INPUT;

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
   Use CONS to construct the pairs.  AP has any remaining args.  */
static Lisp_Object
cons_listn (ptrdiff_t count, Lisp_Object arg,
	    Lisp_Object (*cons) (Lisp_Object, Lisp_Object), va_list ap)
{
  eassume (0 < count);
  Lisp_Object val = cons (arg, Qnil);
  Lisp_Object tail = val;
  for (ptrdiff_t i = 1; i < count; i++)
    {
      Lisp_Object elem = cons (va_arg (ap, Lisp_Object), Qnil);
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
  Lisp_Object val = cons_listn (count, arg1, Fcons, ap);
  va_end (ap);
  return val;
}

/* Make a pure list of COUNT Lisp_Objects, where ARG1 is the first one.  */
Lisp_Object
pure_listn (ptrdiff_t count, Lisp_Object arg1, ...)
{
  va_list ap;
  va_start (ap, arg1);
  Lisp_Object val = cons_listn (count, arg1, pure_cons, ap);
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

/* Verify assumptions described above.  */
verify (VECTOR_BLOCK_SIZE % roundup_size == 0);
verify (VECTOR_BLOCK_SIZE <= (1 << PSEUDOVECTOR_SIZE_BITS));

/* Round up X to nearest mult-of-ROUNDUP_SIZE --- use at compile time.  */
#define vroundup_ct(x) ROUNDUP (x, roundup_size)
/* Round up X to nearest mult-of-ROUNDUP_SIZE --- use at runtime.  */
#define vroundup(x) (eassume ((x) >= 0), vroundup_ct (x))

/* Rounding helps to maintain alignment constraints if USE_LSB_TAG.  */

enum {VECTOR_BLOCK_BYTES = VECTOR_BLOCK_SIZE - vroundup_ct (sizeof (void *))};

/* Size of the minimal vector allocated from block.  */

enum { VBLOCK_BYTES_MIN = vroundup_ct (header_size + sizeof (Lisp_Object)) };

/* Size of the largest vector allocated from block.  */

enum { VBLOCK_BYTES_MAX = vroundup_ct ((VECTOR_BLOCK_BYTES / 2) - word_size) };

/* We maintain one free list for each possible block-allocated
   vector size, and this is the number of free lists we have.  */

enum { VECTOR_MAX_FREE_LIST_INDEX =
       (VECTOR_BLOCK_BYTES - VBLOCK_BYTES_MIN) / roundup_size + 1 };

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
   vectors of the same NBYTES size, so NTH == VINDEX (NBYTES).  */

static struct Lisp_Vector *vector_free_lists[VECTOR_MAX_FREE_LIST_INDEX];

/* Singly-linked list of large vectors.  */

static struct large_vector *large_vectors;

/* The only vector with 0 slots, allocated from pure space.  */

Lisp_Object zero_vector;

/* Common shortcut to setup vector on a free list.  */

static void
setup_on_free_list (struct Lisp_Vector *v, ptrdiff_t nbytes)
{
  v = ptr_bounds_clip (v, nbytes);
  eassume (header_size <= nbytes);
  ptrdiff_t nwords = (nbytes - header_size) / word_size;
  XSETPVECTYPESIZE (v, PVEC_FREE, 0, nwords);
  eassert (nbytes % roundup_size == 0);
  ptrdiff_t vindex = VINDEX (nbytes);
  eassert (vindex < VECTOR_MAX_FREE_LIST_INDEX);
  set_next_vector (v, vector_free_lists[vindex]);
  vector_free_lists[vindex] = v;
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

/* Called once to initialize vector allocation.  */

static void
init_vectors (void)
{
  zero_vector = make_pure_vector (0);
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
      vector_free_lists[index] = next_vector (vector);
      return vector;
    }

  /* Next, check free lists containing larger vectors.  Since
     we will split the result, we should have remaining space
     large enough to use for one-slot vector at least.  */
  for (index = VINDEX (nbytes + VBLOCK_BYTES_MIN);
       index < VECTOR_MAX_FREE_LIST_INDEX; index++)
    if (vector_free_lists[index])
      {
	/* This vector is larger than requested.  */
	vector = vector_free_lists[index];
	vector_free_lists[index] = next_vector (vector);

	/* Excess bytes are used for the smaller vector,
	   which should be set on an appropriate free list.  */
	restbytes = index * roundup_size + VBLOCK_BYTES_MIN - nbytes;
	eassert (restbytes % roundup_size == 0);
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
	  verify (header_size <= bool_header_size);
	  nwords = (boolvec_bytes - header_size + word_size - 1) / word_size;
        }
      else
	nwords = ((size & PSEUDOVECTOR_SIZE_MASK)
		  + ((size & PSEUDOVECTOR_REST_MASK)
		     >> PSEUDOVECTOR_SIZE_BITS));
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
  detect_suspicious_free (vector);

  if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_BIGNUM))
    mpz_clear (PSEUDOVEC_STRUCT (vector, Lisp_Bignum)->value);
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_FINALIZER))
    unchain_finalizer (PSEUDOVEC_STRUCT (vector, Lisp_Finalizer));
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_FONT))
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
    }
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_THREAD))
    finalize_one_thread (PSEUDOVEC_STRUCT (vector, thread_state));
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_MUTEX))
    finalize_one_mutex (PSEUDOVEC_STRUCT (vector, Lisp_Mutex));
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_CONDVAR))
    finalize_one_condvar (PSEUDOVEC_STRUCT (vector, Lisp_CondVar));
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_MARKER))
    {
      /* sweep_buffer should already have unchained this from its buffer.  */
      eassert (! PSEUDOVEC_STRUCT (vector, Lisp_Marker)->buffer);
    }
  else if (PSEUDOVECTOR_TYPEP (&vector->header, PVEC_USER_PTR))
    {
      struct Lisp_User_Ptr *uptr = PSEUDOVEC_STRUCT (vector, Lisp_User_Ptr);
      if (uptr->finalizer)
	uptr->finalizer (uptr->p);
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

  /* Looking through vector blocks.  */

  for (block = vector_blocks; block; block = *bprev)
    {
      bool free_this_block = false;

      for (vector = (struct Lisp_Vector *) block->data;
	   VECTOR_IN_BLOCK (vector, block); vector = next)
	{
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
allocate_vectorlike (ptrdiff_t len)
{
  eassert (0 < len && len <= VECTOR_ELTS_MAX);
  ptrdiff_t nbytes = header_size + len * word_size;
  struct Lisp_Vector *p;

  MALLOC_BLOCK_INPUT;

#ifdef DOUG_LEA_MALLOC
  if (!mmap_lisp_allowed_p ())
    mallopt (M_MMAP_MAX, 0);
#endif

  if (nbytes <= VBLOCK_BYTES_MAX)
    p = allocate_vector_from_block (vroundup (nbytes));
  else
    {
      struct large_vector *lv = lisp_malloc (large_vector_offset + nbytes,
					     MEM_TYPE_VECTORLIKE);
      lv->next = large_vectors;
      large_vectors = lv;
      p = large_vector_vec (lv);
    }

#ifdef DOUG_LEA_MALLOC
  if (!mmap_lisp_allowed_p ())
    mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif

  if (find_suspicious_object_in_range (p, (char *) p + nbytes))
    emacs_abort ();

  tally_consing (nbytes);
  vector_cells_consed += len;

  MALLOC_UNBLOCK_INPUT;

  return ptr_bounds_clip (p, nbytes);
}


/* Allocate a vector with LEN slots.  */

struct Lisp_Vector *
allocate_vector (ptrdiff_t len)
{
  if (len == 0)
    return XVECTOR (zero_vector);
  if (VECTOR_ELTS_MAX < len)
    memory_full (SIZE_MAX);
  struct Lisp_Vector *v = allocate_vectorlike (len);
  v->header.size = len;
  return v;
}


/* Allocate other vector-like structures.  */

struct Lisp_Vector *
allocate_pseudovector (int memlen, int lisplen,
		       int zerolen, enum pvec_type tag)
{
  /* Catch bogus values.  */
  enum { size_max = (1 << PSEUDOVECTOR_SIZE_BITS) - 1 };
  enum { rest_max = (1 << PSEUDOVECTOR_REST_BITS) - 1 };
  verify (size_max + rest_max <= VECTOR_ELTS_MAX);
  eassert (0 <= tag && tag <= PVEC_FONT);
  eassert (0 <= lisplen && lisplen <= zerolen && zerolen <= memlen);
  eassert (lisplen <= size_max);
  eassert (memlen <= size_max + rest_max);

  struct Lisp_Vector *v = allocate_vectorlike (memlen);
  /* Only the first LISPLEN slots will be traced normally by the GC.  */
  memclear (v->contents, zerolen * word_size);
  XSETPVECTYPESIZE (v, tag, lisplen, memlen - lisplen);
  return v;
}

struct buffer *
allocate_buffer (void)
{
  struct buffer *b = lisp_malloc (sizeof *b, MEM_TYPE_BUFFER);

  BUFFER_PVEC_INIT (b);
  /* Put B on the chain of all buffers including killed ones.  */
  b->next = all_buffers;
  all_buffers = b;
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
  struct Lisp_Vector *p = allocate_vectorlike (count);
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
  struct Lisp_Vector *p = allocate_vector (length);
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

void
make_byte_code (struct Lisp_Vector *v)
{
  /* Don't allow the global zero_vector to become a byte code object.  */
  eassert (0 < v->header.size);

  if (v->header.size > 1 && STRINGP (v->contents[1])
      && STRING_MULTIBYTE (v->contents[1]))
    /* BYTECODE-STRING must have been produced by Emacs 20.2 or the
       earlier because they produced a raw 8-bit string for byte-code
       and now such a byte-code string is loaded as multibyte while
       raw 8-bit characters converted to multibyte form.  Thus, now we
       must convert them back to the original unibyte form.  */
    v->contents[1] = Fstring_as_unibyte (v->contents[1]);
  XSETPVECTYPE (v, PVEC_COMPILED);
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
  Lisp_Object val = make_uninit_vector (nargs);
  struct Lisp_Vector *p = XVECTOR (val);

  /* We used to purecopy everything here, if purify-flag was set.  This worked
     OK for Emacs-23, but with Emacs-24's lexical binding code, it can be
     dangerous, since make-byte-code is used during execution to build
     closures, so any closure built during the preload phase would end up
     copied into pure space, including its free variables, which is sometimes
     just wasteful and other times plainly wrong (e.g. those free vars may want
     to be setcar'd).  */

  memcpy (p->contents, args, nargs * sizeof *args);
  make_byte_code (p);
  XSETCOMPILED (val, p);
  return val;
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

/* Current symbol block and index of first unused Lisp_Symbol
   structure in it.  */

static struct symbol_block *symbol_block;
static int symbol_block_index = SYMBOL_BLOCK_SIZE;
/* Pointer to the first symbol_block that contains pinned symbols.
   Tests for 24.4 showed that at dump-time, Emacs contains about 15K symbols,
   10K of which are pinned (and all but 250 of them are interned in obarray),
   whereas a "typical session" has in the order of 30K symbols.
   `symbol_block_pinned' lets mark_pinned_symbols scan only 15K symbols rather
   than 30K to find the 10K symbols we need to mark.  */
static struct symbol_block *symbol_block_pinned;

/* List of free symbols.  */

static struct Lisp_Symbol *symbol_free_list;

static void
set_symbol_name (Lisp_Object sym, Lisp_Object name)
{
  XSYMBOL (sym)->u.s.name = name;
}

void
init_symbol (Lisp_Object val, Lisp_Object name)
{
  struct Lisp_Symbol *p = XSYMBOL (val);
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
  p->u.s.pinned = false;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return a newly allocated uninterned symbol whose name is NAME.
Its value is void, and its function definition and property list are nil.  */)
  (Lisp_Object name)
{
  Lisp_Object val;

  CHECK_STRING (name);

  MALLOC_BLOCK_INPUT;

  if (symbol_free_list)
    {
      XSETSYMBOL (val, symbol_free_list);
      symbol_free_list = symbol_free_list->u.s.next;
    }
  else
    {
      if (symbol_block_index == SYMBOL_BLOCK_SIZE)
	{
	  struct symbol_block *new
	    = lisp_malloc (sizeof *new, MEM_TYPE_SYMBOL);
	  new->next = symbol_block;
	  symbol_block = new;
	  symbol_block_index = 0;
	}
      XSETSYMBOL (val, &symbol_block->symbols[symbol_block_index]);
      symbol_block_index++;
    }

  MALLOC_UNBLOCK_INPUT;

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

/* Return a new overlay with specified START, END and PLIST.  */

Lisp_Object
build_overlay (Lisp_Object start, Lisp_Object end, Lisp_Object plist)
{
  struct Lisp_Overlay *p = ALLOCATE_PSEUDOVECTOR (struct Lisp_Overlay, plist,
						  PVEC_OVERLAY);
  Lisp_Object overlay = make_lisp_ptr (p, Lisp_Vectorlike);
  OVERLAY_START (overlay) = start;
  OVERLAY_END (overlay) = end;
  set_overlay_plist (overlay, plist);
  p->next = NULL;
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
  ptrdiff_t count = SPECPDL_INDEX ();
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
      void *p;

      MALLOC_BLOCK_INPUT;
      p = malloc (SPARE_MEMORY);
      if (p)
	{
	  free (p);
	  enough_free_memory = true;
	}
      MALLOC_UNBLOCK_INPUT;
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
#if !defined SYSTEM_MALLOC && !defined HYBRID_MALLOC
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
				   MEM_TYPE_SPARE);
  if (spare_memory[6] == 0)
    spare_memory[6] = lisp_malloc (sizeof (struct string_block),
				   MEM_TYPE_SPARE);
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
   return the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.

   This and other *_holding functions look for a pointer anywhere into
   the object, not merely for a pointer to the start of the object,
   because some compilers sometimes optimize away the latter.  See
   Bug#28213.  */

static Lisp_Object
live_string_holding (struct mem_node *m, void *p)
{
  if (m->type == MEM_TYPE_STRING)
    {
      struct string_block *b = m->start;
      char *cp = p;
      ptrdiff_t offset = cp - (char *) &b->strings[0];

      /* P must point into a Lisp_String structure, and it
	 must not be on the free-list.  */
      if (0 <= offset && offset < STRING_BLOCK_SIZE * sizeof b->strings[0])
	{
	  cp = ptr_bounds_copy (cp, b);
	  struct Lisp_String *s = p = cp -= offset % sizeof b->strings[0];
	  if (s->u.s.data)
	    return make_lisp_ptr (s, Lisp_String);
	}
    }
  return Qnil;
}

static bool
live_string_p (struct mem_node *m, void *p)
{
  return !NILP (live_string_holding (m, p));
}

/* If P is a pointer into a live Lisp cons object on the heap, return
   the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.  */

static Lisp_Object
live_cons_holding (struct mem_node *m, void *p)
{
  if (m->type == MEM_TYPE_CONS)
    {
      struct cons_block *b = m->start;
      char *cp = p;
      ptrdiff_t offset = cp - (char *) &b->conses[0];

      /* P must point into a Lisp_Cons, not be
	 one of the unused cells in the current cons block,
	 and not be on the free-list.  */
      if (0 <= offset && offset < CONS_BLOCK_SIZE * sizeof b->conses[0]
	  && (b != cons_block
	      || offset / sizeof b->conses[0] < cons_block_index))
	{
	  cp = ptr_bounds_copy (cp, b);
	  struct Lisp_Cons *s = p = cp -= offset % sizeof b->conses[0];
	  if (!deadp (s->u.s.car))
	    return make_lisp_ptr (s, Lisp_Cons);
	}
    }
  return Qnil;
}

static bool
live_cons_p (struct mem_node *m, void *p)
{
  return !NILP (live_cons_holding (m, p));
}


/* If P is a pointer into a live Lisp symbol object on the heap,
   return the object.  Otherwise, return nil.  M is a pointer to the
   mem_block for P.  */

static Lisp_Object
live_symbol_holding (struct mem_node *m, void *p)
{
  if (m->type == MEM_TYPE_SYMBOL)
    {
      struct symbol_block *b = m->start;
      char *cp = p;
      ptrdiff_t offset = cp - (char *) &b->symbols[0];

      /* P must point into the Lisp_Symbol, not be
	 one of the unused cells in the current symbol block,
	 and not be on the free-list.  */
      if (0 <= offset && offset < SYMBOL_BLOCK_SIZE * sizeof b->symbols[0]
	  && (b != symbol_block
	      || offset / sizeof b->symbols[0] < symbol_block_index))
	{
	  cp = ptr_bounds_copy (cp, b);
	  struct Lisp_Symbol *s = p = cp -= offset % sizeof b->symbols[0];
	  if (!deadp (s->u.s.function))
	    return make_lisp_symbol (s);
	}
    }
  return Qnil;
}

static bool
live_symbol_p (struct mem_node *m, void *p)
{
  return !NILP (live_symbol_holding (m, p));
}


/* Return true if P is a pointer to a live Lisp float on
   the heap.  M is a pointer to the mem_block for P.  */

static bool
live_float_p (struct mem_node *m, void *p)
{
  if (m->type == MEM_TYPE_FLOAT)
    {
      struct float_block *b = m->start;
      char *cp = p;
      ptrdiff_t offset = cp - (char *) &b->floats[0];

      /* P must point to the start of a Lisp_Float and not be
	 one of the unused cells in the current float block.  */
      return (offset >= 0
	      && offset % sizeof b->floats[0] == 0
	      && offset < (FLOAT_BLOCK_SIZE * sizeof b->floats[0])
	      && (b != float_block
		  || offset / sizeof b->floats[0] < float_block_index));
    }
  else
    return 0;
}

/* If P is a pointer to a live vector-like object, return the object.
   Otherwise, return nil.
   M is a pointer to the mem_block for P.  */

static Lisp_Object
live_vector_holding (struct mem_node *m, void *p)
{
  struct Lisp_Vector *vp = p;

  if (m->type == MEM_TYPE_VECTOR_BLOCK)
    {
      /* This memory node corresponds to a vector block.  */
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
	    return make_lisp_ptr (vector, Lisp_Vectorlike);
	  vector = next;
	}
    }
  else if (m->type == MEM_TYPE_VECTORLIKE)
    {
      /* This memory node corresponds to a large vector.  */
      struct Lisp_Vector *vector = large_vector_vec (m->start);
      struct Lisp_Vector *next = ADVANCE (vector, vector_nbytes (vector));
      if (vector <= vp && vp < next)
	return make_lisp_ptr (vector, Lisp_Vectorlike);
    }
  return Qnil;
}

static bool
live_vector_p (struct mem_node *m, void *p)
{
  return !NILP (live_vector_holding (m, p));
}

/* If P is a pointer into a live buffer, return the buffer.
   Otherwise, return nil.  M is a pointer to the mem_block for P.  */

static Lisp_Object
live_buffer_holding (struct mem_node *m, void *p)
{
  /* P must point into the block, and the buffer
     must not have been killed.  */
  if (m->type == MEM_TYPE_BUFFER)
    {
      struct buffer *b = m->start;
      char *cb = m->start;
      char *cp = p;
      ptrdiff_t offset = cp - cb;
      if (0 <= offset && offset < sizeof *b && !NILP (b->name_))
	{
	  Lisp_Object obj;
	  XSETBUFFER (obj, b);
	  return obj;
	}
    }
  return Qnil;
}

static bool
live_buffer_p (struct mem_node *m, void *p)
{
  return !NILP (live_buffer_holding (m, p));
}

/* Mark OBJ if we can prove it's a Lisp_Object.  */

static void
mark_maybe_object (Lisp_Object obj)
{
#if USE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED (&obj, sizeof (obj));
#endif

  if (FIXNUMP (obj))
    return;

  void *po = XPNTR (obj);

  /* If the pointer is in the dump image and the dump has a record
     of the object starting at the place where the pointer points, we
     definitely have an object.  If the pointer is in the dump image
     and the dump has no idea what the pointer is pointing at, we
     definitely _don't_ have an object.  */
  if (pdumper_object_p (po))
    {
      /* Don't use pdumper_object_p_precise here! It doesn't check the
         tag bits. OBJ here might be complete garbage, so we need to
         verify both the pointer and the tag.  */
      if (XTYPE (obj) == pdumper_find_object_type (po))
        mark_object (obj);
      return;
    }

  struct mem_node *m = mem_find (po);

  if (m != MEM_NIL)
    {
      bool mark_p = false;

      switch (XTYPE (obj))
	{
	case Lisp_String:
	  mark_p = EQ (obj, live_string_holding (m, po));
	  break;

	case Lisp_Cons:
	  mark_p = EQ (obj, live_cons_holding (m, po));
	  break;

	case Lisp_Symbol:
	  mark_p = EQ (obj, live_symbol_holding (m, po));
	  break;

	case Lisp_Float:
	  mark_p = live_float_p (m, po);
	  break;

	case Lisp_Vectorlike:
	  mark_p = (EQ (obj, live_vector_holding (m, po))
		    || EQ (obj, live_buffer_holding (m, po)));
	  break;

	default:
	  break;
	}

      if (mark_p)
	mark_object (obj);
    }
}

void
mark_maybe_objects (Lisp_Object const *array, ptrdiff_t nelts)
{
  for (Lisp_Object const *lim = array + nelts; array < lim; array++)
    mark_maybe_object (*array);
}

/* If P points to Lisp data, mark that as live if it isn't already
   marked.  */

static void
mark_maybe_pointer (void *p)
{
  struct mem_node *m;

#ifdef USE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED (&p, sizeof (p));
#endif

  if (pdumper_object_p (p))
    {
      int type = pdumper_find_object_type (p);
      if (pdumper_valid_object_type_p (type))
        mark_object (type == Lisp_Symbol
                     ? make_lisp_symbol (p)
                     : make_lisp_ptr (p, type));
      /* See mark_maybe_object for why we can confidently return.  */
      return;
    }

  m = mem_find (p);
  if (m != MEM_NIL)
    {
      Lisp_Object obj = Qnil;

      switch (m->type)
	{
	case MEM_TYPE_NON_LISP:
	case MEM_TYPE_SPARE:
	  /* Nothing to do; not a pointer to Lisp memory.  */
	  break;

	case MEM_TYPE_BUFFER:
	  obj = live_buffer_holding (m, p);
	  break;

	case MEM_TYPE_CONS:
	  obj = live_cons_holding (m, p);
	  break;

	case MEM_TYPE_STRING:
	  obj = live_string_holding (m, p);
	  break;

	case MEM_TYPE_SYMBOL:
	  obj = live_symbol_holding (m, p);
	  break;

	case MEM_TYPE_FLOAT:
	  if (live_float_p (m, p))
	    obj = make_lisp_ptr (p, Lisp_Float);
	  break;

	case MEM_TYPE_VECTORLIKE:
	case MEM_TYPE_VECTOR_BLOCK:
	  obj = live_vector_holding (m, p);
	  break;

	default:
	  emacs_abort ();
	}

      if (!NILP (obj))
	mark_object (obj);
    }
}


/* Alignment of pointer values.  Use alignof, as it sometimes returns
   a smaller alignment than GCC's __alignof__ and mark_memory might
   miss objects if __alignof__ were used.  */
#define GC_POINTER_ALIGNMENT alignof (void *)

/* Mark Lisp objects referenced from the address range START+OFFSET..END
   or END+OFFSET..START.  */

static void ATTRIBUTE_NO_SANITIZE_ADDRESS
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
      char *p = *(char *const *) pp;
      mark_maybe_pointer (p);

      /* Unmask any struct Lisp_Symbol pointer that make_lisp_symbol
	 previously disguised by adding the address of 'lispsym'.
	 On a host with 32-bit pointers and 64-bit Lisp_Objects,
	 a Lisp_Object might be split into registers saved into
	 non-adjacent words and P might be the low-order word's value.  */
      p += (intptr_t) lispsym;
      mark_maybe_pointer (p);

      verify (alignof (Lisp_Object) % GC_POINTER_ALIGNMENT == 0);
      if (alignof (Lisp_Object) == GC_POINTER_ALIGNMENT
	  || (uintptr_t) pp % alignof (Lisp_Object) == 0)
	mark_maybe_object (*(Lisp_Object const *) pp);
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
who is, please take a look at the function mark_stack in alloc.c, and\n\
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
Please take a look at the function mark_stack in alloc.c, and\n\
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
  /* Align the stack top properly.  Even if !HAVE___BUILTIN_UNWIND_INIT,
     jmp_buf may not be aligned enough on darwin-ppc64.  */
  max_align_t o;
#ifndef HAVE___BUILTIN_UNWIND_INIT
  sys_jmp_buf j;
  char c;
#endif
} stacktop_sentry;

/* Force callee-saved registers and register windows onto the stack.
   Use the platform-defined __builtin_unwind_init if available,
   obviating the need for machine dependent methods.  */
#ifndef HAVE___BUILTIN_UNWIND_INIT
# ifdef __sparc__
   /* This trick flushes the register windows so that all the state of
      the process is contained in the stack.
      FreeBSD does not have a ta 3 handler, so handle it specially.
      FIXME: Code in the Boehm GC suggests flushing (with 'flushrs') is
      needed on ia64 too.  See mach_dep.c, where it also says inline
      assembler doesn't work with relevant proprietary compilers.  */
#  if defined __sparc64__ && defined __FreeBSD__
#   define __builtin_unwind_init() asm ("flushw")
#  else
#   define __builtin_unwind_init() asm ("ta 3")
#  endif
# else
#  define __builtin_unwind_init() ((void) 0)
# endif
#endif

/* Yield an address close enough to the top of the stack that the
   garbage collector need not scan above it.  Callers should be
   declared NO_INLINE.  */
#ifdef HAVE___BUILTIN_FRAME_ADDRESS
# define NEAR_STACK_TOP(addr) ((void) (addr), __builtin_frame_address (0))
#else
# define NEAR_STACK_TOP(addr) (addr)
#endif

/* Set *P to the address of the top of the stack.  This must be a
   macro, not a function, so that it is executed in the caller's
   environment.  It is not inside a do-while so that its storage
   survives the macro.  Callers should be declared NO_INLINE.  */
#ifdef HAVE___BUILTIN_UNWIND_INIT
# define SET_STACK_TOP_ADDRESS(p)	\
   stacktop_sentry sentry;		\
   __builtin_unwind_init ();		\
   *(p) = NEAR_STACK_TOP (&sentry)
#else
# define SET_STACK_TOP_ADDRESS(p)		\
   stacktop_sentry sentry;			\
   __builtin_unwind_init ();			\
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

   This code assumes that calling setjmp saves registers we need
   to see in a jmp_buf which itself lies on the stack.  This doesn't
   have to be true!  It must be verified for each system, possibly
   by taking a look at the source code of setjmp.

   If __builtin_unwind_init is available (defined by GCC >= 2.8) we
   can use it as a machine independent method to store all registers
   to the stack.  In this case the macros described in the previous
   two paragraphs are not used.

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
mark_stack (char const *bottom, char const *end)
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

/* This is a trampoline function that flushes registers to the stack,
   and then calls FUNC.  ARG is passed through to FUNC verbatim.

   This function must be called whenever Emacs is about to release the
   global interpreter lock.  This lets the garbage collector easily
   find roots in registers on threads that are not actively running
   Lisp.

   It is invalid to run any Lisp code or to allocate any GC memory
   from FUNC.  */

NO_INLINE void
flush_stack_call_func (void (*func) (void *arg), void *arg)
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
  if (PURE_P (p))
    return 1;

  if (SYMBOLP (obj) && c_symbol_p (p))
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

      if (SUBRP (obj))
	return 1;

      return 0;
    }

  switch (m->type)
    {
    case MEM_TYPE_NON_LISP:
    case MEM_TYPE_SPARE:
      return 0;

    case MEM_TYPE_BUFFER:
      return live_buffer_p (m, p) ? 1 : 2;

    case MEM_TYPE_CONS:
      return live_cons_p (m, p);

    case MEM_TYPE_STRING:
      return live_string_p (m, p);

    case MEM_TYPE_SYMBOL:
      return live_symbol_p (m, p);

    case MEM_TYPE_FLOAT:
      return live_float_p (m, p);

    case MEM_TYPE_VECTORLIKE:
    case MEM_TYPE_VECTOR_BLOCK:
      return live_vector_p (m, p);

    default:
      break;
    }

  return 0;
}

/***********************************************************************
		       Pure Storage Management
 ***********************************************************************/

/* Allocate room for SIZE bytes from pure Lisp storage and return a
   pointer to it.  TYPE is the Lisp type for which the memory is
   allocated.  TYPE < 0 means it's not used for a Lisp object,
   and that the result should have an alignment of -TYPE.

   The bytes are initially zero.

   If pure space is exhausted, allocate space from the heap.  This is
   merely an expedient to let Emacs warn that pure space was exhausted
   and that Emacs should be rebuilt with a larger pure space.  */

static void *
pure_alloc (size_t size, int type)
{
  void *result;

 again:
  if (type >= 0)
    {
      /* Allocate space for a Lisp object from the beginning of the free
	 space with taking account of alignment.  */
      result = pointer_align (purebeg + pure_bytes_used_lisp, LISP_ALIGNMENT);
      pure_bytes_used_lisp = ((char *)result - (char *)purebeg) + size;
    }
  else
    {
      /* Allocate space for a non-Lisp object from the end of the free
	 space.  */
      ptrdiff_t unaligned_non_lisp = pure_bytes_used_non_lisp + size;
      char *unaligned = purebeg + pure_size - unaligned_non_lisp;
      int decr = (intptr_t) unaligned & (-1 - type);
      pure_bytes_used_non_lisp = unaligned_non_lisp + decr;
      result = unaligned - decr;
    }
  pure_bytes_used = pure_bytes_used_lisp + pure_bytes_used_non_lisp;

  if (pure_bytes_used <= pure_size)
    return ptr_bounds_clip (result, size);

  /* Don't allocate a large amount here,
     because it might get mmap'd and then its address
     might not be usable.  */
  int small_amount = 10000;
  eassert (size <= small_amount - LISP_ALIGNMENT);
  purebeg = xzalloc (small_amount);
  pure_size = small_amount;
  pure_bytes_used_before_overflow += pure_bytes_used - size;
  pure_bytes_used = 0;
  pure_bytes_used_lisp = pure_bytes_used_non_lisp = 0;

  /* Can't GC if pure storage overflowed because we can't determine
     if something is a pure object or not.  */
  garbage_collection_inhibited++;
  goto again;
}


#ifdef HAVE_UNEXEC

/* Print a warning if PURESIZE is too small.  */

void
check_pure_size (void)
{
  if (pure_bytes_used_before_overflow)
    message (("emacs:0:Pure Lisp storage overflow (approx. %"pI"d"
	      " bytes needed)"),
	     pure_bytes_used + pure_bytes_used_before_overflow);
}
#endif


/* Find the byte sequence {DATA[0], ..., DATA[NBYTES-1], '\0'} from
   the non-Lisp data pool of the pure storage, and return its start
   address.  Return NULL if not found.  */

static char *
find_string_data_in_pure (const char *data, ptrdiff_t nbytes)
{
  int i;
  ptrdiff_t skip, bm_skip[256], last_char_skip, infinity, start, start_max;
  const unsigned char *p;
  char *non_lisp_beg;

  if (pure_bytes_used_non_lisp <= nbytes)
    return NULL;

  /* Set up the Boyer-Moore table.  */
  skip = nbytes + 1;
  for (i = 0; i < 256; i++)
    bm_skip[i] = skip;

  p = (const unsigned char *) data;
  while (--skip > 0)
    bm_skip[*p++] = skip;

  last_char_skip = bm_skip['\0'];

  non_lisp_beg = purebeg + pure_size - pure_bytes_used_non_lisp;
  start_max = pure_bytes_used_non_lisp - (nbytes + 1);

  /* See the comments in the function `boyer_moore' (search.c) for the
     use of `infinity'.  */
  infinity = pure_bytes_used_non_lisp + 1;
  bm_skip['\0'] = infinity;

  p = (const unsigned char *) non_lisp_beg + nbytes;
  start = 0;
  do
    {
      /* Check the last character (== '\0').  */
      do
	{
	  start += bm_skip[*(p + start)];
	}
      while (start <= start_max);

      if (start < infinity)
	/* Couldn't find the last character.  */
	return NULL;

      /* No less than `infinity' means we could find the last
	 character at `p[start - infinity]'.  */
      start -= infinity;

      /* Check the remaining characters.  */
      if (memcmp (data, non_lisp_beg + start, nbytes) == 0)
	/* Found.  */
	return ptr_bounds_clip (non_lisp_beg + start, nbytes + 1);

      start += last_char_skip;
    }
  while (start <= start_max);

  return NULL;
}


/* Return a string allocated in pure space.  DATA is a buffer holding
   NCHARS characters, and NBYTES bytes of string data.  MULTIBYTE
   means make the result string multibyte.

   Must get an error if pure storage is full, since if it cannot hold
   a large string it may be able to hold conses that point to that
   string; then the string is not protected from gc.  */

Lisp_Object
make_pure_string (const char *data,
		  ptrdiff_t nchars, ptrdiff_t nbytes, bool multibyte)
{
  Lisp_Object string;
  struct Lisp_String *s = pure_alloc (sizeof *s, Lisp_String);
  s->u.s.data = (unsigned char *) find_string_data_in_pure (data, nbytes);
  if (s->u.s.data == NULL)
    {
      s->u.s.data = pure_alloc (nbytes + 1, -1);
      memcpy (s->u.s.data, data, nbytes);
      s->u.s.data[nbytes] = '\0';
    }
  s->u.s.size = nchars;
  s->u.s.size_byte = multibyte ? nbytes : -1;
  s->u.s.intervals = NULL;
  XSETSTRING (string, s);
  return string;
}

/* Return a string allocated in pure space.  Do not
   allocate the string data, just point to DATA.  */

Lisp_Object
make_pure_c_string (const char *data, ptrdiff_t nchars)
{
  Lisp_Object string;
  struct Lisp_String *s = pure_alloc (sizeof *s, Lisp_String);
  s->u.s.size = nchars;
  s->u.s.size_byte = -2;
  s->u.s.data = (unsigned char *) data;
  s->u.s.intervals = NULL;
  XSETSTRING (string, s);
  return string;
}

static Lisp_Object purecopy (Lisp_Object obj);

/* Return a cons allocated from pure space.  Give it pure copies
   of CAR as car and CDR as cdr.  */

Lisp_Object
pure_cons (Lisp_Object car, Lisp_Object cdr)
{
  Lisp_Object new;
  struct Lisp_Cons *p = pure_alloc (sizeof *p, Lisp_Cons);
  XSETCONS (new, p);
  XSETCAR (new, purecopy (car));
  XSETCDR (new, purecopy (cdr));
  return new;
}


/* Value is a float object with value NUM allocated from pure space.  */

static Lisp_Object
make_pure_float (double num)
{
  Lisp_Object new;
  struct Lisp_Float *p = pure_alloc (sizeof *p, Lisp_Float);
  XSETFLOAT (new, p);
  XFLOAT_INIT (new, num);
  return new;
}

/* Value is a bignum object with value VALUE allocated from pure
   space.  */

static Lisp_Object
make_pure_bignum (Lisp_Object value)
{
  mpz_t const *n = xbignum_val (value);
  size_t i, nlimbs = mpz_size (*n);
  size_t nbytes = nlimbs * sizeof (mp_limb_t);
  mp_limb_t *pure_limbs;
  mp_size_t new_size;

  struct Lisp_Bignum *b = pure_alloc (sizeof *b, Lisp_Vectorlike);
  XSETPVECTYPESIZE (b, PVEC_BIGNUM, 0, VECSIZE (struct Lisp_Bignum));

  int limb_alignment = alignof (mp_limb_t);
  pure_limbs = pure_alloc (nbytes, - limb_alignment);
  for (i = 0; i < nlimbs; ++i)
    pure_limbs[i] = mpz_getlimbn (*n, i);

  new_size = nlimbs;
  if (mpz_sgn (*n) < 0)
    new_size = -new_size;

  mpz_roinit_n (b->value, pure_limbs, new_size);

  return make_lisp_ptr (b, Lisp_Vectorlike);
}

/* Return a vector with room for LEN Lisp_Objects allocated from
   pure space.  */

static Lisp_Object
make_pure_vector (ptrdiff_t len)
{
  Lisp_Object new;
  size_t size = header_size + len * word_size;
  struct Lisp_Vector *p = pure_alloc (size, Lisp_Vectorlike);
  XSETVECTOR (new, p);
  XVECTOR (new)->header.size = len;
  return new;
}

/* Copy all contents and parameters of TABLE to a new table allocated
   from pure space, return the purified table.  */
static struct Lisp_Hash_Table *
purecopy_hash_table (struct Lisp_Hash_Table *table)
{
  eassert (NILP (table->weak));
  eassert (table->purecopy);

  struct Lisp_Hash_Table *pure = pure_alloc (sizeof *pure, Lisp_Vectorlike);
  struct hash_table_test pure_test = table->test;

  /* Purecopy the hash table test.  */
  pure_test.name = purecopy (table->test.name);
  pure_test.user_hash_function = purecopy (table->test.user_hash_function);
  pure_test.user_cmp_function = purecopy (table->test.user_cmp_function);

  pure->header = table->header;
  pure->weak = purecopy (Qnil);
  pure->hash = purecopy (table->hash);
  pure->next = purecopy (table->next);
  pure->index = purecopy (table->index);
  pure->count = table->count;
  pure->next_free = table->next_free;
  pure->purecopy = table->purecopy;
  eassert (!pure->mutable);
  pure->rehash_threshold = table->rehash_threshold;
  pure->rehash_size = table->rehash_size;
  pure->key_and_value = purecopy (table->key_and_value);
  pure->test = pure_test;

  return pure;
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
       doc: /* Make a copy of object OBJ in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.  Copies strings without text properties.  */)
  (register Lisp_Object obj)
{
  if (NILP (Vpurify_flag))
    return obj;
  else if (MARKERP (obj) || OVERLAYP (obj) || SYMBOLP (obj))
    /* Can't purify those.  */
    return obj;
  else
    return purecopy (obj);
}

/* Pinned objects are marked before every GC cycle.  */
static struct pinned_object
{
  Lisp_Object object;
  struct pinned_object *next;
} *pinned_objects;

static Lisp_Object
purecopy (Lisp_Object obj)
{
  if (FIXNUMP (obj)
      || (! SYMBOLP (obj) && PURE_P (XPNTR (obj)))
      || SUBRP (obj))
    return obj;    /* Already pure.  */

  if (STRINGP (obj) && XSTRING (obj)->u.s.intervals)
    message_with_string ("Dropping text-properties while making string `%s' pure",
			 obj, true);

  if (HASH_TABLE_P (Vpurify_flag)) /* Hash consing.  */
    {
      Lisp_Object tmp = Fgethash (obj, Vpurify_flag, Qnil);
      if (!NILP (tmp))
	return tmp;
    }

  if (CONSP (obj))
    obj = pure_cons (XCAR (obj), XCDR (obj));
  else if (FLOATP (obj))
    obj = make_pure_float (XFLOAT_DATA (obj));
  else if (STRINGP (obj))
    obj = make_pure_string (SSDATA (obj), SCHARS (obj),
			    SBYTES (obj),
			    STRING_MULTIBYTE (obj));
  else if (HASH_TABLE_P (obj))
    {
      struct Lisp_Hash_Table *table = XHASH_TABLE (obj);
      /* Do not purify hash tables which haven't been defined with
         :purecopy as non-nil or are weak - they aren't guaranteed to
         not change.  */
      if (!NILP (table->weak) || !table->purecopy)
        {
          /* Instead, add the hash table to the list of pinned objects,
             so that it will be marked during GC.  */
          struct pinned_object *o = xmalloc (sizeof *o);
          o->object = obj;
          o->next = pinned_objects;
          pinned_objects = o;
          return obj; /* Don't hash cons it.  */
        }

      struct Lisp_Hash_Table *h = purecopy_hash_table (table);
      XSET_HASH_TABLE (obj, h);
    }
  else if (COMPILEDP (obj) || VECTORP (obj) || RECORDP (obj))
    {
      struct Lisp_Vector *objp = XVECTOR (obj);
      ptrdiff_t nbytes = vector_nbytes (objp);
      struct Lisp_Vector *vec = pure_alloc (nbytes, Lisp_Vectorlike);
      register ptrdiff_t i;
      ptrdiff_t size = ASIZE (obj);
      if (size & PSEUDOVECTOR_FLAG)
	size &= PSEUDOVECTOR_SIZE_MASK;
      memcpy (vec, objp, nbytes);
      for (i = 0; i < size; i++)
	vec->contents[i] = purecopy (vec->contents[i]);
      XSETVECTOR (obj, vec);
    }
  else if (SYMBOLP (obj))
    {
      if (!XSYMBOL (obj)->u.s.pinned && !c_symbol_p (XSYMBOL (obj)))
	{ /* We can't purify them, but they appear in many pure objects.
	     Mark them as `pinned' so we know to mark them at every GC cycle.  */
	  XSYMBOL (obj)->u.s.pinned = true;
	  symbol_block_pinned = symbol_block;
	}
      /* Don't hash-cons it.  */
      return obj;
    }
  else if (BIGNUMP (obj))
    obj = make_pure_bignum (obj);
  else
    {
      AUTO_STRING (fmt, "Don't know how to purify: %S");
      Fsignal (Qerror, list1 (CALLN (Fformat, fmt, obj)));
    }

  if (HASH_TABLE_P (Vpurify_flag)) /* Hash consing.  */
    Fputhash (obj, obj, Vpurify_flag);

  return obj;
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

ptrdiff_t
inhibit_garbage_collection (void)
{
  ptrdiff_t count = SPECPDL_INDEX ();
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
                      && vectorlike_marked_p(&font->header))
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

static void
mark_pinned_objects (void)
{
  for (struct pinned_object *pobj = pinned_objects; pobj; pobj = pobj->next)
    mark_object (pobj->object);
}

static void
mark_pinned_symbols (void)
{
  struct symbol_block *sblk;
  int lim = (symbol_block_pinned == symbol_block
	     ? symbol_block_index : SYMBOL_BLOCK_SIZE);

  for (sblk = symbol_block_pinned; sblk; sblk = sblk->next)
    {
      struct Lisp_Symbol *sym = sblk->symbols, *end = sym + lim;
      for (; sym < end; ++sym)
	if (sym->u.s.pinned)
	  mark_object (make_lisp_symbol (sym));

      lim = SYMBOL_BLOCK_SIZE;
    }
}

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
  eassert (buffer->overlays_before == NULL);
  eassert (buffer->overlays_after == NULL);

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

/* Subroutine of Fgarbage_collect that does most of the work.  */
void
garbage_collect (void)
{
  struct buffer *nextb;
  char stack_top_variable;
  bool message_p;
  ptrdiff_t count = SPECPDL_INDEX ();
  struct timespec start;

  eassert (weak_hash_tables == NULL);

  if (garbage_collection_inhibited)
    return;

  /* Record this function, so it appears on the profiler's backtraces.  */
  record_in_backtrace (QAutomatic_GC, 0, 0);

  /* Don't keep undo information around forever.
     Do this early on, so it is no problem if the user quits.  */
  FOR_EACH_BUFFER (nextb)
    compact_buffer (nextb);

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
	  stack = ptr_bounds_set (stack, stack_size);
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

  mark_pinned_objects ();
  mark_pinned_symbols ();
  mark_terminals ();
  mark_kboards ();
  mark_threads ();

#ifdef USE_GTK
  xg_mark_data ();
#endif

#ifdef HAVE_WINDOW_SYSTEM
  mark_fringe_data ();
#endif

#ifdef HAVE_MODULES
  mark_modules ();
#endif

  /* Everything is now marked, except for the data in font caches,
     undo lists, and finalizers.  The first two are compacted by
     removing an items which aren't reachable otherwise.  */

  compact_font_caches ();

  FOR_EACH_BUFFER (nextb)
    {
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

  gc_sweep ();

  unmark_main_thread ();

  gc_in_progress = 0;

  unblock_input ();

  consing_until_gc = gc_threshold
    = consing_threshold (gc_cons_threshold, Vgc_cons_percentage, 0);

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

  if (!NILP (Vpost_gc_hook))
    {
      ptrdiff_t gc_count = inhibit_garbage_collection ();
      safe_run_hooks (Qpost_gc_hook);
      unbind_to (gc_count, Qnil);
    }

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
}

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
       doc: /* Reclaim storage for Lisp objects no longer needed.
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.
`garbage-collect' normally returns a list with info on amount of space in use,
where each entry has the form (NAME SIZE USED FREE), where:
- NAME is a symbol describing the kind of objects this entry represents,
- SIZE is the number of bytes used by each one,
- USED is the number of those objects that were found live in the heap,
- FREE is the number of those objects that are not live but that Emacs
  keeps around for future allocations (maybe because it does not know how
  to return them to the OS).
However, if there was overflow in pure space, and Emacs was dumped
using the 'unexec' method, `garbage-collect' returns nil, because
real GC can't be done.
See Info node `(elisp)Garbage Collection'.  */)
  (void)
{
  if (garbage_collection_inhibited)
    return Qnil;

  garbage_collect ();
  struct gcstat gcst = gcstat;

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
	      if (STRINGP (glyph->object)
		  && !string_marked_p (XSTRING (glyph->object)))
		mark_object (glyph->object);
	  }
      }
}

enum { LAST_MARKED_SIZE = 1 << 9 }; /* Must be a power of 2.  */
Lisp_Object last_marked[LAST_MARKED_SIZE] EXTERNALLY_VISIBLE;
static int last_marked_index;

/* For debugging--call abort when we cdr down this many
   links of a list, in mark_object.  In debugging,
   the call to abort will hit a breakpoint.
   Normally this is zero and the check never goes off.  */
ptrdiff_t mark_object_loop_halt EXTERNALLY_VISIBLE;

static void
mark_vectorlike (union vectorlike_header *header)
{
  struct Lisp_Vector *ptr = (struct Lisp_Vector *) header;
  ptrdiff_t size = ptr->header.size;
  ptrdiff_t i;

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
  for (i = 0; i < size; i++) /* ...and then mark its elements.  */
    mark_object (ptr->contents[i]);
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
          (SYMBOLP (val) && symbol_marked_p (XSYMBOL (val))))
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

NO_INLINE /* To reduce stack depth in mark_object.  */
static Lisp_Object
mark_compiled (struct Lisp_Vector *ptr)
{
  int i, size = ptr->header.size & PSEUDOVECTOR_SIZE_MASK;

  set_vector_marked (ptr);
  for (i = 0; i < size; i++)
    if (i != COMPILED_CONSTANTS)
      mark_object (ptr->contents[i]);
  return size > COMPILED_CONSTANTS ? ptr->contents[COMPILED_CONSTANTS] : Qnil;
}

/* Mark the chain of overlays starting at PTR.  */

static void
mark_overlay (struct Lisp_Overlay *ptr)
{
  for (; ptr && !vectorlike_marked_p (&ptr->header); ptr = ptr->next)
    {
      set_vectorlike_marked (&ptr->header);
      /* These two are always markers and can be marked fast.  */
      set_vectorlike_marked (&XMARKER (ptr->start)->header);
      set_vectorlike_marked (&XMARKER (ptr->end)->header);
      mark_object (ptr->plist);
    }
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
     some of its elements that are not needed any more.  */

  mark_overlay (buffer->overlays_before);
  mark_overlay (buffer->overlays_after);

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
      int i, j;
      for (i = 0; i < c->used; ++i)
	{
	  struct face *face = FACE_FROM_ID_OR_NULL (c->f, i);

	  if (face)
	    {
	      if (face->font && !vectorlike_marked_p (&face->font->header))
		mark_vectorlike (&face->font->header);

	      for (j = 0; j < LFACE_VECTOR_SIZE; ++j)
		mark_object (face->lface[j]);
	    }
	}
    }
}

NO_INLINE /* To reduce stack depth in mark_object.  */
static void
mark_localized_symbol (struct Lisp_Symbol *ptr)
{
  struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (ptr);
  Lisp_Object where = blv->where;
  /* If the value is set up for a killed buffer restore its global binding.  */
  if ((BUFFERP (where) && !BUFFER_LIVE_P (XBUFFER (where))))
    swap_in_global_binding (ptr);
  mark_object (blv->where);
  mark_object (blv->valcell);
  mark_object (blv->defcell);
}

/* Remove killed buffers or items whose car is a killed buffer from
   LIST, and mark other items.  Return changed LIST, which is marked.  */

static Lisp_Object
mark_discard_killed_buffers (Lisp_Object list)
{
  Lisp_Object tail, *prev = &list;

  for (tail = list; CONSP (tail) && !cons_marked_p (XCONS (tail));
       tail = XCDR (tail))
    {
      Lisp_Object tem = XCAR (tail);
      if (CONSP (tem))
	tem = XCAR (tem);
      if (BUFFERP (tem) && !BUFFER_LIVE_P (XBUFFER (tem)))
	*prev = XCDR (tail);
      else
	{
	  set_cons_marked (XCONS (tail));
	  mark_object (XCAR (tail));
	  prev = xcdr_addr (tail);
	}
    }
  mark_object (tail);
  return list;
}

static void
mark_frame (struct Lisp_Vector *ptr)
{
  struct frame *f = (struct frame *) ptr;
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

  /* Filter out killed buffers from both buffer lists
     in attempt to help GC to reclaim killed buffers faster.
     We can do it elsewhere for live windows, but this is the
     best place to do it for dead windows.  */
  wset_prev_buffers
    (w, mark_discard_killed_buffers (w->prev_buffers));
  wset_next_buffers
    (w, mark_discard_killed_buffers (w->next_buffers));
}

static void
mark_hash_table (struct Lisp_Vector *ptr)
{
  struct Lisp_Hash_Table *h = (struct Lisp_Hash_Table *) ptr;

  mark_vectorlike (&h->header);
  mark_object (h->test.name);
  mark_object (h->test.user_hash_function);
  mark_object (h->test.user_cmp_function);
  /* If hash table is not weak, mark all keys and values.  For weak
     tables, mark only the vector and not its contents --- that's what
     makes it weak.  */
  if (NILP (h->weak))
    mark_object (h->key_and_value);
  else
    {
      eassert (h->next_weak == NULL);
      h->next_weak = weak_hash_tables;
      weak_hash_tables = h;
      set_vector_marked (XVECTOR (h->key_and_value));
    }
}

/* Determine type of generic Lisp_Object and mark it accordingly.

   This function implements a straightforward depth-first marking
   algorithm and so the recursion depth may be very high (a few
   tens of thousands is not uncommon).  To minimize stack usage,
   a few cold paths are moved out to NO_INLINE functions above.
   In general, inlining them doesn't help you to gain more speed.  */

void
mark_object (Lisp_Object arg)
{
  register Lisp_Object obj;
  void *po;
#if GC_CHECK_MARKED_OBJECTS
  struct mem_node *m = NULL;
#endif
  ptrdiff_t cdr_count = 0;

  obj = arg;
 loop:

  po = XPNTR (obj);
  if (PURE_P (po))
    return;

  last_marked[last_marked_index++] = obj;
  last_marked_index &= LAST_MARKED_SIZE - 1;

  /* Perform some sanity checks on the objects marked here.  Abort if
     we encounter an object we know is bogus.  This increases GC time
     by ~80%.  */
#if GC_CHECK_MARKED_OBJECTS

  /* Check that the object pointed to by PO is known to be a Lisp
     structure allocated from the heap.  */
#define CHECK_ALLOCATED()			\
  do {						\
    if (pdumper_object_p(po))                   \
      {                                         \
        if (!pdumper_object_p_precise (po))     \
          emacs_abort ();                       \
        break;                                  \
      }                                         \
    m = mem_find (po);				\
    if (m == MEM_NIL)				\
      emacs_abort ();				\
  } while (0)

  /* Check that the object pointed to by PO is live, using predicate
     function LIVEP.  */
#define CHECK_LIVE(LIVEP)			\
  do {						\
    if (pdumper_object_p(po))                   \
      break;                                    \
    if (!LIVEP (m, po))				\
      emacs_abort ();				\
  } while (0)

  /* Check both of the above conditions, for non-symbols.  */
#define CHECK_ALLOCATED_AND_LIVE(LIVEP)		\
  do {						\
    CHECK_ALLOCATED ();				\
    CHECK_LIVE (LIVEP);				\
  } while (false)

  /* Check both of the above conditions, for symbols.  */
#define CHECK_ALLOCATED_AND_LIVE_SYMBOL()	\
  do {						\
    if (!c_symbol_p (ptr))			\
      {						\
	CHECK_ALLOCATED ();			\
	CHECK_LIVE (live_symbol_p);		\
      }						\
  } while (false)

#else /* not GC_CHECK_MARKED_OBJECTS */

#define CHECK_LIVE(LIVEP)			((void) 0)
#define CHECK_ALLOCATED_AND_LIVE(LIVEP)		((void) 0)
#define CHECK_ALLOCATED_AND_LIVE_SYMBOL()	((void) 0)

#endif /* not GC_CHECK_MARKED_OBJECTS */

  switch (XTYPE (obj))
    {
    case Lisp_String:
      {
	register struct Lisp_String *ptr = XSTRING (obj);
        if (string_marked_p (ptr))
          break;
	CHECK_ALLOCATED_AND_LIVE (live_string_p);
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

#ifdef GC_CHECK_MARKED_OBJECTS
        if (!pdumper_object_p(po))
          {
	    m = mem_find (po);
            if (m == MEM_NIL && !SUBRP (obj) && !main_thread_p (po))
	      emacs_abort ();
          }
#endif /* GC_CHECK_MARKED_OBJECTS */

        enum pvec_type pvectype
          = PSEUDOVECTOR_TYPE (ptr);

        if (pvectype != PVEC_SUBR &&
            pvectype != PVEC_BUFFER &&
            !main_thread_p (po))
          CHECK_LIVE (live_vector_p);

	switch (pvectype)
	  {
	  case PVEC_BUFFER:
#if GC_CHECK_MARKED_OBJECTS
	    {
	      struct buffer *b;
	      FOR_EACH_BUFFER (b)
		if (b == po)
		  break;
	      if (b == NULL)
		emacs_abort ();
	    }
#endif /* GC_CHECK_MARKED_OBJECTS */
	    mark_buffer ((struct buffer *) ptr);
            break;

          case PVEC_COMPILED:
            /* Although we could treat this just like a vector, mark_compiled
               returns the COMPILED_CONSTANTS element, which is marked at the
               next iteration of goto-loop here.  This is done to avoid a few
               recursive calls to mark_object.  */
            obj = mark_compiled (ptr);
            if (!NILP (obj))
              goto loop;
            break;

          case PVEC_FRAME:
            mark_frame (ptr);
            break;

          case PVEC_WINDOW:
            mark_window (ptr);
            break;

	  case PVEC_HASH_TABLE:
            mark_hash_table (ptr);
	    break;

	  case PVEC_CHAR_TABLE:
	  case PVEC_SUB_CHAR_TABLE:
	    mark_char_table (ptr, (enum pvec_type) pvectype);
	    break;

          case PVEC_BOOL_VECTOR:
            /* bool vectors in a dump are permanently "marked", since
               they're in the old section and don't have mark bits.
               If we're looking at a dumped bool vector, we should
               have aborted above when we called vector_marked_p(), so
               we should never get here.  */
            eassert (!pdumper_object_p (ptr));
            set_vector_marked (ptr);
            break;

          case PVEC_OVERLAY:
	    mark_overlay (XOVERLAY (obj));
	    break;

	  case PVEC_SUBR:
	    break;

	  case PVEC_FREE:
	    emacs_abort ();

	  default:
	    /* A regular vector, or a pseudovector needing no special
	       treatment.  */
	    mark_vectorlike (&ptr->header);
	  }
      }
      break;

    case Lisp_Symbol:
      {
	struct Lisp_Symbol *ptr = XSYMBOL (obj);
      nextsym:
        if (symbol_marked_p (ptr))
          break;
        CHECK_ALLOCATED_AND_LIVE_SYMBOL ();
        set_symbol_marked(ptr);
	/* Attempt to catch bogus objects.  */
	eassert (valid_lisp_object_p (ptr->u.s.function));
	mark_object (ptr->u.s.function);
	mark_object (ptr->u.s.plist);
	switch (ptr->u.s.redirect)
	  {
	  case SYMBOL_PLAINVAL: mark_object (SYMBOL_VAL (ptr)); break;
	  case SYMBOL_VARALIAS:
	    {
	      Lisp_Object tem;
	      XSETSYMBOL (tem, SYMBOL_ALIAS (ptr));
	      mark_object (tem);
	      break;
	    }
	  case SYMBOL_LOCALIZED:
	    mark_localized_symbol (ptr);
	    break;
	  case SYMBOL_FORWARDED:
	    /* If the value is forwarded to a buffer or keyboard field,
	       these are marked when we see the corresponding object.
	       And if it's forwarded to a C variable, either it's not
	       a Lisp_Object var, or it's staticpro'd already.  */
	    break;
	  default: emacs_abort ();
	  }
	if (!PURE_P (XSTRING (ptr->u.s.name)))
          set_string_marked (XSTRING (ptr->u.s.name));
        mark_interval_tree (string_intervals (ptr->u.s.name));
	/* Inner loop to mark next symbol in this bucket, if any.  */
	po = ptr = ptr->u.s.next;
	if (ptr)
	  goto nextsym;
      }
      break;

    case Lisp_Cons:
      {
	struct Lisp_Cons *ptr = XCONS (obj);
	if (cons_marked_p (ptr))
	  break;
	CHECK_ALLOCATED_AND_LIVE (live_cons_p);
        set_cons_marked (ptr);
	/* If the cdr is nil, avoid recursion for the car.  */
	if (NILP (ptr->u.s.u.cdr))
	  {
	    obj = ptr->u.s.car;
	    cdr_count = 0;
	    goto loop;
	  }
	mark_object (ptr->u.s.car);
	obj = ptr->u.s.u.cdr;
	cdr_count++;
	if (cdr_count == mark_object_loop_halt)
	  emacs_abort ();
	goto loop;
      }

    case Lisp_Float:
      CHECK_ALLOCATED_AND_LIVE (live_float_p);
      /* Do not mark floats stored in a dump image: these floats are
         "cold" and do not have mark bits.  */
      if (pdumper_object_p (XFLOAT (obj)))
        eassert (pdumper_cold_object_p (XFLOAT (obj)));
      else if (!XFLOAT_MARKED_P (XFLOAT (obj)))
        XFLOAT_MARK (XFLOAT (obj));
      break;

    case_Lisp_Int:
      break;

    default:
      emacs_abort ();
    }

#undef CHECK_LIVE
#undef CHECK_ALLOCATED
#undef CHECK_ALLOCATED_AND_LIVE
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
#ifdef HAVE_WINDOW_SYSTEM
      /* If a terminal object is reachable from a stacpro'ed object,
	 it might have been marked already.  Make sure the image cache
	 gets marked.  */
      mark_image_cache (t->image_cache);
#endif /* HAVE_WINDOW_SYSTEM */
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
    case_Lisp_Int:
      survives_p = true;
      break;

    case Lisp_Symbol:
      survives_p = symbol_marked_p (XSYMBOL (obj));
      break;

    case Lisp_String:
      survives_p = string_marked_p (XSTRING (obj));
      break;

    case Lisp_Vectorlike:
      survives_p = SUBRP (obj) || vector_marked_p (XVECTOR (obj));
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

  return survives_p || PURE_P (XPNTR (obj));
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
		  struct Lisp_Cons *acons
		    = ptr_bounds_copy (&cblk->conses[pos], cblk);
		  if (!XCONS_MARKED_P (acons))
                    {
                      this_free++;
                      cblk->conses[pos].u.s.u.chain = cons_free_list;
                      cons_free_list = &cblk->conses[pos];
                      cons_free_list->u.s.car = dead_object ();
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
      for (int i = 0; i < lim; i++)
	{
	  struct Lisp_Float *afloat = ptr_bounds_copy (&fblk->floats[i], fblk);
	  if (!XFLOAT_MARKED_P (afloat))
	    {
	      this_free++;
	      fblk->floats[i].u.chain = float_free_list;
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

      for (int i = 0; i < lim; i++)
        {
          if (!iblk->intervals[i].gcmarkbit)
            {
              set_interval_parent (&iblk->intervals[i], interval_free_list);
              interval_free_list = &iblk->intervals[i];
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
  struct buffer *buffer, **bprev = &all_buffers;

  gcstat.total_buffers = 0;
  for (buffer = all_buffers; buffer; buffer = *bprev)
    if (!vectorlike_marked_p (&buffer->header))
      {
        *bprev = buffer->next;
        lisp_free (buffer);
      }
    else
      {
        if (!pdumper_object_p (buffer))
          XUNMARK_VECTOR (buffer);
        /* Do not use buffer_(set|get)_intervals here.  */
        buffer->text->intervals = balance_intervals (buffer->text->intervals);
        unchain_dead_markers (buffer);
	gcstat.total_buffers++;
        bprev = &buffer->next;
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
or memory information can't be obtained, return nil.  */)
  (void)
{
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

static bool
symbol_uses_obj (Lisp_Object symbol, Lisp_Object obj)
{
  struct Lisp_Symbol *sym = XSYMBOL (symbol);
  Lisp_Object val = find_symbol_value (symbol);
  return (EQ (val, obj)
	  || EQ (sym->u.s.function, obj)
	  || (!NILP (sym->u.s.function)
	      && COMPILEDP (sym->u.s.function)
	      && EQ (AREF (sym->u.s.function, COMPILED_BYTECODE), obj))
	  || (!NILP (val)
	      && COMPILEDP (val)
	      && EQ (AREF (val, COMPILED_BYTECODE), obj)));
}

/* Find at most FIND_MAX symbols which have OBJ as their value or
   function.  This is used in gdbinit's `xwhichsymbols' command.  */

Lisp_Object
which_symbols (Lisp_Object obj, EMACS_INT find_max)
{
   struct symbol_block *sblk;
   ptrdiff_t gc_count = inhibit_garbage_collection ();
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

#ifdef SUSPICIOUS_OBJECT_CHECKING

static void *
find_suspicious_object_in_range (void *begin, void *end)
{
  char *begin_a = begin;
  char *end_a = end;
  int i;

  for (i = 0; i < ARRAYELTS (suspicious_objects); ++i)
    {
      char *suspicious_object = suspicious_objects[i];
      if (begin_a <= suspicious_object && suspicious_object < end_a)
	return suspicious_object;
    }

  return NULL;
}

static void
note_suspicious_free (void *ptr)
{
  struct suspicious_free_record *rec;

  rec = &suspicious_free_history[suspicious_free_history_index++];
  if (suspicious_free_history_index ==
      ARRAYELTS (suspicious_free_history))
    {
      suspicious_free_history_index = 0;
    }

  memset (rec, 0, sizeof (*rec));
  rec->suspicious_object = ptr;
  backtrace (&rec->backtrace[0], ARRAYELTS (rec->backtrace));
}

static void
detect_suspicious_free (void *ptr)
{
  int i;

  eassert (ptr != NULL);

  for (i = 0; i < ARRAYELTS (suspicious_objects); ++i)
    if (suspicious_objects[i] == ptr)
      {
        note_suspicious_free (ptr);
        suspicious_objects[i] = NULL;
      }
}

#endif /* SUSPICIOUS_OBJECT_CHECKING */

DEFUN ("suspicious-object", Fsuspicious_object, Ssuspicious_object, 1, 1, 0,
       doc: /* Return OBJ, maybe marking it for extra scrutiny.
If Emacs is compiled with suspicious object checking, capture
a stack trace when OBJ is freed in order to help track down
garbage collection bugs.  Otherwise, do nothing and return OBJ.   */)
   (Lisp_Object obj)
{
#ifdef SUSPICIOUS_OBJECT_CHECKING
  /* Right now, we care only about vectors.  */
  if (VECTORLIKEP (obj))
    {
      suspicious_objects[suspicious_object_index++] = XVECTOR (obj);
      if (suspicious_object_index == ARRAYELTS (suspicious_objects))
	suspicious_object_index = 0;
    }
#endif
  return obj;
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
  purebeg = PUREBEG;
  pure_size = PURESIZE;
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
prevent garbage collection during a part of the program.
See also `gc-cons-percentage'.  */);

  DEFVAR_LISP ("gc-cons-percentage", Vgc_cons_percentage,
	       doc: /* Portion of the heap used for allocation.
Garbage collection can happen automatically once this portion of the heap
has been allocated since the last garbage collection.
If this portion is smaller than `gc-cons-threshold', this is ignored.  */);
  Vgc_cons_percentage = make_float (0.1);

  DEFVAR_INT ("pure-bytes-used", pure_bytes_used,
	      doc: /* Number of bytes of shareable Lisp data allocated so far.  */);

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
This means that certain objects should be allocated in shared (pure) space.
It can also be set to a hash-table, in which case this table is used to
do hash-consing of the objects allocated to pure space.  */);

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
    = pure_list (Qerror,
		 build_pure_c_string ("Memory exhausted--use"
				      " M-x save-some-buffers then"
				      " exit and restart Emacs"));

  DEFVAR_LISP ("memory-full", Vmemory_full,
	       doc: /* Non-nil means Emacs cannot get much more Lisp memory.  */);
  Vmemory_full = Qnil;

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
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_record);
  defsubr (&Smake_string);
  defsubr (&Smake_bool_vector);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Smake_finalizer);
  defsubr (&Spurecopy);
  defsubr (&Sgarbage_collect);
  defsubr (&Smemory_info);
  defsubr (&Smemory_use_counts);
  defsubr (&Ssuspicious_object);

  Lisp_Object watcher;

  static union Aligned_Lisp_Subr Swatch_gc_cons_threshold =
     {{{ PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) },
       { .a4 = watch_gc_cons_threshold },
       4, 4, "watch_gc_cons_threshold", 0, 0}};
  XSETSUBR (watcher, &Swatch_gc_cons_threshold.s);
  Fadd_variable_watcher (Qgc_cons_threshold, watcher);

  static union Aligned_Lisp_Subr Swatch_gc_cons_percentage =
     {{{ PSEUDOVECTOR_FLAG | (PVEC_SUBR << PSEUDOVECTOR_AREA_BITS) },
       { .a4 = watch_gc_cons_percentage },
       4, 4, "watch_gc_cons_percentage", 0, 0}};
  XSETSUBR (watcher, &Swatch_gc_cons_percentage.s);
  Fadd_variable_watcher (Qgc_cons_percentage, watcher);
}

#ifdef HAVE_X_WINDOWS
enum defined_HAVE_X_WINDOWS { defined_HAVE_X_WINDOWS = true };
#else
enum defined_HAVE_X_WINDOWS { defined_HAVE_X_WINDOWS = false };
#endif

/* When compiled with GCC, GDB might say "No enum type named
   pvec_type" if we don't have at least one symbol with that type, and
   then xbacktrace could fail.  Similarly for the other enums and
   their values.  Some non-GCC compilers don't like these constructs.  */
#ifdef __GNUC__
union
{
  enum CHARTAB_SIZE_BITS CHARTAB_SIZE_BITS;
  enum char_table_specials char_table_specials;
  enum char_bits char_bits;
  enum CHECK_LISP_OBJECT_TYPE CHECK_LISP_OBJECT_TYPE;
  enum DEFAULT_HASH_SIZE DEFAULT_HASH_SIZE;
  enum Lisp_Bits Lisp_Bits;
  enum Lisp_Compiled Lisp_Compiled;
  enum maxargs maxargs;
  enum MAX_ALLOCA MAX_ALLOCA;
  enum More_Lisp_Bits More_Lisp_Bits;
  enum pvec_type pvec_type;
  enum defined_HAVE_X_WINDOWS defined_HAVE_X_WINDOWS;
} const EXTERNALLY_VISIBLE gdb_make_enums_visible = {0};
#endif	/* __GNUC__ */
