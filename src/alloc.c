/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 86, 88, 93, 94, 95, 97, 98, 1999, 2000, 2001, 2002
      Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <config.h>
#include <stdio.h>

/* Note that this declares bzero on OSF/1.  How dumb.  */

#include <signal.h>

/* GC_MALLOC_CHECK defined means perform validity checks of malloc'd
   memory.  Can do this only if using gmalloc.c.  */

#if defined SYSTEM_MALLOC || defined DOUG_LEA_MALLOC
#undef GC_MALLOC_CHECK
#endif

/* This file is part of the core Lisp implementation, and thus must
   deal with the real data structures.  If the Lisp implementation is
   replaced, this file likely will not be used.  */

#undef HIDE_LISP_IMPLEMENTATION
#include "lisp.h"
#include "process.h"
#include "intervals.h"
#include "puresize.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "frame.h"
#include "blockinput.h"
#include "charset.h"
#include "syssignal.h"
#include <setjmp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
extern POINTER_TYPE *sbrk ();
#endif

#ifdef DOUG_LEA_MALLOC

#include <malloc.h>
/* malloc.h #defines this as size_t, at least in glibc2.  */
#ifndef __malloc_size_t
#define __malloc_size_t int
#endif

/* Specify maximum number of areas to mmap.  It would be nice to use a
   value that explicitly means "no limit".  */

#define MMAP_MAX_AREAS 100000000

#else /* not DOUG_LEA_MALLOC */

/* The following come from gmalloc.c.  */

#define	__malloc_size_t		size_t
extern __malloc_size_t _bytes_used;
extern __malloc_size_t __malloc_extra_blocks;

#endif /* not DOUG_LEA_MALLOC */

/* Macro to verify that storage intended for Lisp objects is not
   out of range to fit in the space for a pointer.
   ADDRESS is the start of the block, and SIZE
   is the amount of space within which objects can start.  */

#define VALIDATE_LISP_STORAGE(address, size)			\
do								\
  {								\
    Lisp_Object val;						\
    XSETCONS (val, (char *) address + size);		\
    if ((char *) XCONS (val) != (char *) address + size)	\
      {								\
	xfree (address);					\
	memory_full ();						\
      }								\
  } while (0)

/* Value of _bytes_used, when spare_memory was freed.  */

static __malloc_size_t bytes_used_when_full;

/* Mark, unmark, query mark bit of a Lisp string.  S must be a pointer
   to a struct Lisp_String.  */

#define MARK_STRING(S)		((S)->size |= MARKBIT)
#define UNMARK_STRING(S)	((S)->size &= ~MARKBIT)
#define STRING_MARKED_P(S)	((S)->size & MARKBIT)

/* Value is the number of bytes/chars of S, a pointer to a struct
   Lisp_String.  This must be used instead of STRING_BYTES (S) or
   S->size during GC, because S->size contains the mark bit for
   strings.  */

#define GC_STRING_BYTES(S)	(STRING_BYTES (S) & ~MARKBIT)
#define GC_STRING_CHARS(S)	((S)->size & ~MARKBIT)

/* Number of bytes of consing done since the last gc.  */

int consing_since_gc;

/* Count the amount of consing of various sorts of space.  */

EMACS_INT cons_cells_consed;
EMACS_INT floats_consed;
EMACS_INT vector_cells_consed;
EMACS_INT symbols_consed;
EMACS_INT string_chars_consed;
EMACS_INT misc_objects_consed;
EMACS_INT intervals_consed;
EMACS_INT strings_consed;

/* Number of bytes of consing since GC before another GC should be done. */

EMACS_INT gc_cons_threshold;

/* Nonzero during GC.  */

int gc_in_progress;

/* Nonzero means display messages at beginning and end of GC.  */

int garbage_collection_messages;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
int malloc_sbrk_used;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
int malloc_sbrk_unused;

/* Two limits controlling how much undo information to keep.  */

EMACS_INT undo_limit;
EMACS_INT undo_strong_limit;

/* Number of live and free conses etc.  */

static int total_conses, total_markers, total_symbols, total_vector_size;
static int total_free_conses, total_free_markers, total_free_symbols;
static int total_free_floats, total_floats;

/* Points to memory space allocated as "spare", to be freed if we run
   out of memory.  */

static char *spare_memory;

/* Amount of spare memory to keep in reserve.  */

#define SPARE_MEMORY (1 << 14)

/* Number of extra blocks malloc should get when it needs more core.  */

static int malloc_hysteresis;

/* Non-nil means defun should do purecopy on the function definition.  */

Lisp_Object Vpurify_flag;

#ifndef HAVE_SHM

/* Force it into data space! */

EMACS_INT pure[PURESIZE / sizeof (EMACS_INT)] = {0,};
#define PUREBEG (char *) pure

#else /* HAVE_SHM */

#define pure PURE_SEG_BITS   /* Use shared memory segment */
#define PUREBEG (char *)PURE_SEG_BITS

#endif /* HAVE_SHM */

/* Pointer to the pure area, and its size.  */

static char *purebeg;
static size_t pure_size;

/* Number of bytes of pure storage used before pure storage overflowed.
   If this is non-zero, this implies that an overflow occurred.  */

static size_t pure_bytes_used_before_overflow;

/* Value is non-zero if P points into pure space.  */

#define PURE_POINTER_P(P)					\
     (((PNTR_COMPARISON_TYPE) (P)				\
       < (PNTR_COMPARISON_TYPE) ((char *) purebeg + pure_size))	\
      && ((PNTR_COMPARISON_TYPE) (P)				\
	  >= (PNTR_COMPARISON_TYPE) purebeg))

/* Index in pure at which next pure object will be allocated.. */

EMACS_INT pure_bytes_used;

/* If nonzero, this is a warning delivered by malloc and not yet
   displayed.  */

char *pending_malloc_warning;

/* Pre-computed signal argument for use when memory is exhausted.  */

Lisp_Object memory_signal_data;

/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 16000
#endif

/* Buffer in which we save a copy of the C stack at each GC.  */

char *stack_copy;
int stack_copy_size;

/* Non-zero means ignore malloc warnings.  Set during initialization.
   Currently not used.  */

int ignore_warnings;

Lisp_Object Qgc_cons_threshold, Qchar_table_extra_slots;

/* Hook run after GC has finished.  */

Lisp_Object Vpost_gc_hook, Qpost_gc_hook;

static void mark_buffer P_ ((Lisp_Object));
static void mark_kboards P_ ((void));
static void gc_sweep P_ ((void));
static void mark_glyph_matrix P_ ((struct glyph_matrix *));
static void mark_face_cache P_ ((struct face_cache *));

#ifdef HAVE_WINDOW_SYSTEM
static void mark_image P_ ((struct image *));
static void mark_image_cache P_ ((struct frame *));
#endif /* HAVE_WINDOW_SYSTEM */

static struct Lisp_String *allocate_string P_ ((void));
static void compact_small_strings P_ ((void));
static void free_large_strings P_ ((void));
static void sweep_strings P_ ((void));

extern int message_enable_multibyte;

/* When scanning the C stack for live Lisp objects, Emacs keeps track
   of what memory allocated via lisp_malloc is intended for what
   purpose.  This enumeration specifies the type of memory.  */

enum mem_type
{
  MEM_TYPE_NON_LISP,
  MEM_TYPE_BUFFER,
  MEM_TYPE_CONS,
  MEM_TYPE_STRING,
  MEM_TYPE_MISC,
  MEM_TYPE_SYMBOL,
  MEM_TYPE_FLOAT,
  /* Keep the following vector-like types together, with
     MEM_TYPE_WINDOW being the last, and MEM_TYPE_VECTOR the
     first.  Or change the code of live_vector_p, for instance.  */
  MEM_TYPE_VECTOR,
  MEM_TYPE_PROCESS,
  MEM_TYPE_HASH_TABLE,
  MEM_TYPE_FRAME,
  MEM_TYPE_WINDOW
};

#if GC_MARK_STACK || defined GC_MALLOC_CHECK

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES
#include <stdio.h>		/* For fprintf.  */
#endif

/* A unique object in pure space used to make some Lisp objects
   on free lists recognizable in O(1).  */

Lisp_Object Vdead;

#ifdef GC_MALLOC_CHECK

enum mem_type allocated_mem_type;
int dont_register_blocks;

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
  struct mem_node *left, *right, *parent;

  /* Start and end of allocated region.  */
  void *start, *end;

  /* Node color.  */
  enum {MEM_BLACK, MEM_RED} color;
  
  /* Memory type.  */
  enum mem_type type;
};

/* Base address of stack.  Set in main.  */

Lisp_Object *stack_base;

/* Root of the tree describing allocated Lisp memory.  */

static struct mem_node *mem_root;

/* Lowest and highest known address in the heap.  */

static void *min_heap_address, *max_heap_address;

/* Sentinel node of the tree.  */

static struct mem_node mem_z;
#define MEM_NIL &mem_z

static POINTER_TYPE *lisp_malloc P_ ((size_t, enum mem_type));
static struct Lisp_Vector *allocate_vectorlike P_ ((EMACS_INT, enum mem_type));
static void lisp_free P_ ((POINTER_TYPE *));
static void mark_stack P_ ((void));
static int live_vector_p P_ ((struct mem_node *, void *));
static int live_buffer_p P_ ((struct mem_node *, void *));
static int live_string_p P_ ((struct mem_node *, void *));
static int live_cons_p P_ ((struct mem_node *, void *));
static int live_symbol_p P_ ((struct mem_node *, void *));
static int live_float_p P_ ((struct mem_node *, void *));
static int live_misc_p P_ ((struct mem_node *, void *));
static void mark_maybe_object P_ ((Lisp_Object));
static void mark_memory P_ ((void *, void *));
static void mem_init P_ ((void));
static struct mem_node *mem_insert P_ ((void *, void *, enum mem_type));
static void mem_insert_fixup P_ ((struct mem_node *));
static void mem_rotate_left P_ ((struct mem_node *));
static void mem_rotate_right P_ ((struct mem_node *));
static void mem_delete P_ ((struct mem_node *));
static void mem_delete_fixup P_ ((struct mem_node *));
static INLINE struct mem_node *mem_find P_ ((void *));

#if GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS
static void check_gcpros P_ ((void));
#endif

#endif /* GC_MARK_STACK || GC_MALLOC_CHECK */

/* Recording what needs to be marked for gc.  */

struct gcpro *gcprolist;

/* Addresses of staticpro'd variables.  */

#define NSTATICS 1280
Lisp_Object *staticvec[NSTATICS] = {0};

/* Index of next unused slot in staticvec.  */

int staticidx = 0;

static POINTER_TYPE *pure_alloc P_ ((size_t, int));


/* Value is SZ rounded up to the next multiple of ALIGNMENT.
   ALIGNMENT must be a power of 2.  */

#define ALIGN(SZ, ALIGNMENT) \
  (((SZ) + (ALIGNMENT) - 1) & ~((ALIGNMENT) - 1))



/************************************************************************
				Malloc
 ************************************************************************/

/* Write STR to Vstandard_output plus some advice on how to free some
   memory.  Called when memory gets low.  */

Lisp_Object
malloc_warning_1 (str)
     Lisp_Object str;
{
  Fprinc (str, Vstandard_output);
  write_string ("\nKilling some buffers may delay running out of memory.\n", -1);
  write_string ("However, certainly by the time you receive the 95% warning,\n", -1);
  write_string ("you should clean up, kill this Emacs, and start a new one.", -1);
  return Qnil;
}


/* Function malloc calls this if it finds we are near exhausting
   storage.  */

void
malloc_warning (str)
     char *str;
{
  pending_malloc_warning = str;
}


/* Display a malloc warning in buffer *Danger*.  */

void
display_malloc_warning ()
{
  register Lisp_Object val;

  val = build_string (pending_malloc_warning);
  pending_malloc_warning = 0;
  internal_with_output_to_temp_buffer (" *Danger*", malloc_warning_1, val);
}


#ifdef DOUG_LEA_MALLOC
#  define BYTES_USED (mallinfo ().arena)
#else
#  define BYTES_USED _bytes_used
#endif


/* Called if malloc returns zero.  */

void
memory_full ()
{
#ifndef SYSTEM_MALLOC
  bytes_used_when_full = BYTES_USED;
#endif

  /* The first time we get here, free the spare memory.  */
  if (spare_memory)
    {
      free (spare_memory);
      spare_memory = 0;
    }

  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  while (1)
    Fsignal (Qnil, memory_signal_data);
}


/* Called if we can't allocate relocatable space for a buffer.  */

void
buffer_memory_full ()
{
  /* If buffers use the relocating allocator, no need to free
     spare_memory, because we may have plenty of malloc space left
     that we could get, and if we don't, the malloc that fails will
     itself cause spare_memory to be freed.  If buffers don't use the
     relocating allocator, treat this like any other failing
     malloc.  */

#ifndef REL_ALLOC
  memory_full ();
#endif

  /* This used to call error, but if we've run out of memory, we could
     get infinite recursion trying to build the string.  */
  while (1)
    Fsignal (Qerror, memory_signal_data);
}


/* Like malloc but check for no memory and block interrupt input..  */

POINTER_TYPE *
xmalloc (size)
     size_t size;
{
  register POINTER_TYPE *val;

  BLOCK_INPUT;
  val = (POINTER_TYPE *) malloc (size);
  UNBLOCK_INPUT;

  if (!val && size)
    memory_full ();
  return val;
}


/* Like realloc but check for no memory and block interrupt input..  */

POINTER_TYPE *
xrealloc (block, size)
     POINTER_TYPE *block;
     size_t size;
{
  register POINTER_TYPE *val;

  BLOCK_INPUT;
  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  if (! block)
    val = (POINTER_TYPE *) malloc (size);
  else
    val = (POINTER_TYPE *) realloc (block, size);
  UNBLOCK_INPUT;

  if (!val && size) memory_full ();
  return val;
}


/* Like free but block interrupt input..  */

void
xfree (block)
     POINTER_TYPE *block;
{
  BLOCK_INPUT;
  free (block);
  UNBLOCK_INPUT;
}


/* Like strdup, but uses xmalloc.  */

char *
xstrdup (s)
     char *s;
{
  size_t len = strlen (s) + 1;
  char *p = (char *) xmalloc (len);
  bcopy (s, p, len);
  return p;
}


/* Like malloc but used for allocating Lisp data.  NBYTES is the
   number of bytes to allocate, TYPE describes the intended use of the
   allcated memory block (for strings, for conses, ...).  */

static POINTER_TYPE *
lisp_malloc (nbytes, type)
     size_t nbytes;
     enum mem_type type;
{
  register void *val;

  BLOCK_INPUT;

#ifdef GC_MALLOC_CHECK
  allocated_mem_type = type;
#endif
  
  val = (void *) malloc (nbytes);

#if GC_MARK_STACK && !defined GC_MALLOC_CHECK
  if (val && type != MEM_TYPE_NON_LISP)
    mem_insert (val, (char *) val + nbytes, type);
#endif
   
  UNBLOCK_INPUT;
  if (!val && nbytes)
    memory_full ();
  return val;
}


/* Return a new buffer structure allocated from the heap with
   a call to lisp_malloc.  */

struct buffer *
allocate_buffer ()
{
  struct buffer *b 
    = (struct buffer *) lisp_malloc (sizeof (struct buffer),
				     MEM_TYPE_BUFFER);
  VALIDATE_LISP_STORAGE (b, sizeof *b);
  return b;
}


/* Free BLOCK.  This must be called to free memory allocated with a
   call to lisp_malloc.  */

static void
lisp_free (block)
     POINTER_TYPE *block;
{
  BLOCK_INPUT;
  free (block);
#if GC_MARK_STACK && !defined GC_MALLOC_CHECK
  mem_delete (mem_find (block));
#endif
  UNBLOCK_INPUT;
}


/* Arranging to disable input signals while we're in malloc.

   This only works with GNU malloc.  To help out systems which can't
   use GNU malloc, all the calls to malloc, realloc, and free
   elsewhere in the code should be inside a BLOCK_INPUT/UNBLOCK_INPUT
   pairs; unfortunately, we have no idea what C library functions
   might call malloc, so we can't really protect them unless you're
   using GNU malloc.  Fortunately, most of the major operating can use
   GNU malloc.  */

#ifndef SYSTEM_MALLOC
#ifndef DOUG_LEA_MALLOC
extern void * (*__malloc_hook) P_ ((size_t));
extern void * (*__realloc_hook) P_ ((void *, size_t));
extern void (*__free_hook) P_ ((void *));
/* Else declared in malloc.h, perhaps with an extra arg.  */
#endif /* DOUG_LEA_MALLOC */
static void * (*old_malloc_hook) ();
static void * (*old_realloc_hook) ();
static void (*old_free_hook) ();

/* This function is used as the hook for free to call.  */

static void
emacs_blocked_free (ptr)
     void *ptr;
{
  BLOCK_INPUT;

#ifdef GC_MALLOC_CHECK
  if (ptr)
    {
      struct mem_node *m;
  
      m = mem_find (ptr);
      if (m == MEM_NIL || m->start != ptr)
	{
	  fprintf (stderr,
		   "Freeing `%p' which wasn't allocated with malloc\n", ptr);
	  abort ();
	}
      else
	{
	  /* fprintf (stderr, "free %p...%p (%p)\n", m->start, m->end, ptr); */
	  mem_delete (m);
	}
    }
#endif /* GC_MALLOC_CHECK */
  
  __free_hook = old_free_hook;
  free (ptr);
  
  /* If we released our reserve (due to running out of memory),
     and we have a fair amount free once again,
     try to set aside another reserve in case we run out once more.  */
  if (spare_memory == 0
      /* Verify there is enough space that even with the malloc
	 hysteresis this call won't run out again.
	 The code here is correct as long as SPARE_MEMORY
	 is substantially larger than the block size malloc uses.  */
      && (bytes_used_when_full
	  > BYTES_USED + max (malloc_hysteresis, 4) * SPARE_MEMORY))
    spare_memory = (char *) malloc ((size_t) SPARE_MEMORY);

  __free_hook = emacs_blocked_free;
  UNBLOCK_INPUT;
}


/* If we released our reserve (due to running out of memory),
   and we have a fair amount free once again,
   try to set aside another reserve in case we run out once more.

   This is called when a relocatable block is freed in ralloc.c.  */

void
refill_memory_reserve ()
{
  if (spare_memory == 0)
    spare_memory = (char *) malloc ((size_t) SPARE_MEMORY);
}


/* This function is the malloc hook that Emacs uses.  */

static void *
emacs_blocked_malloc (size)
     size_t size;
{
  void *value;

  BLOCK_INPUT;
  __malloc_hook = old_malloc_hook;
#ifdef DOUG_LEA_MALLOC
    mallopt (M_TOP_PAD, malloc_hysteresis * 4096);
#else
    __malloc_extra_blocks = malloc_hysteresis;
#endif

  value = (void *) malloc (size);

#ifdef GC_MALLOC_CHECK
  {
    struct mem_node *m = mem_find (value);
    if (m != MEM_NIL)
      {
	fprintf (stderr, "Malloc returned %p which is already in use\n",
		 value);
	fprintf (stderr, "Region in use is %p...%p, %u bytes, type %d\n",
		 m->start, m->end, (char *) m->end - (char *) m->start,
		 m->type);
	abort ();
      }

    if (!dont_register_blocks)
      {
	mem_insert (value, (char *) value + max (1, size), allocated_mem_type);
	allocated_mem_type = MEM_TYPE_NON_LISP;
      }
  }
#endif /* GC_MALLOC_CHECK */
  
  __malloc_hook = emacs_blocked_malloc;
  UNBLOCK_INPUT;

  /* fprintf (stderr, "%p malloc\n", value); */
  return value;
}


/* This function is the realloc hook that Emacs uses.  */

static void *
emacs_blocked_realloc (ptr, size)
     void *ptr;
     size_t size;
{
  void *value;

  BLOCK_INPUT;
  __realloc_hook = old_realloc_hook;

#ifdef GC_MALLOC_CHECK
  if (ptr)
    {
      struct mem_node *m = mem_find (ptr);
      if (m == MEM_NIL || m->start != ptr)
	{
	  fprintf (stderr,
		   "Realloc of %p which wasn't allocated with malloc\n",
		   ptr);
	  abort ();
	}

      mem_delete (m);
    }
  
  /* fprintf (stderr, "%p -> realloc\n", ptr); */
  
  /* Prevent malloc from registering blocks.  */
  dont_register_blocks = 1;
#endif /* GC_MALLOC_CHECK */

  value = (void *) realloc (ptr, size);

#ifdef GC_MALLOC_CHECK
  dont_register_blocks = 0;

  {
    struct mem_node *m = mem_find (value);
    if (m != MEM_NIL)
      {
	fprintf (stderr, "Realloc returns memory that is already in use\n");
	abort ();
      }

    /* Can't handle zero size regions in the red-black tree.  */
    mem_insert (value, (char *) value + max (size, 1), MEM_TYPE_NON_LISP);
  }
  
  /* fprintf (stderr, "%p <- realloc\n", value); */
#endif /* GC_MALLOC_CHECK */
  
  __realloc_hook = emacs_blocked_realloc;
  UNBLOCK_INPUT;

  return value;
}


/* Called from main to set up malloc to use our hooks.  */

void
uninterrupt_malloc ()
{
  if (__free_hook != emacs_blocked_free)
    old_free_hook = __free_hook;
  __free_hook = emacs_blocked_free;

  if (__malloc_hook != emacs_blocked_malloc)
    old_malloc_hook = __malloc_hook;
  __malloc_hook = emacs_blocked_malloc;

  if (__realloc_hook != emacs_blocked_realloc)
    old_realloc_hook = __realloc_hook;
  __realloc_hook = emacs_blocked_realloc;
}

#endif /* not SYSTEM_MALLOC */



/***********************************************************************
			 Interval Allocation
 ***********************************************************************/

/* Number of intervals allocated in an interval_block structure.
   The 1020 is 1024 minus malloc overhead.  */

#define INTERVAL_BLOCK_SIZE \
  ((1020 - sizeof (struct interval_block *)) / sizeof (struct interval))

/* Intervals are allocated in chunks in form of an interval_block
   structure.  */

struct interval_block
{
  struct interval_block *next;
  struct interval intervals[INTERVAL_BLOCK_SIZE];
};

/* Current interval block.  Its `next' pointer points to older
   blocks.  */

struct interval_block *interval_block;

/* Index in interval_block above of the next unused interval
   structure.  */

static int interval_block_index;

/* Number of free and live intervals.  */

static int total_free_intervals, total_intervals;

/* List of free intervals.  */

INTERVAL interval_free_list;

/* Total number of interval blocks now in use.  */

int n_interval_blocks;


/* Initialize interval allocation.  */

static void
init_intervals ()
{
  interval_block
    = (struct interval_block *) lisp_malloc (sizeof *interval_block,
					     MEM_TYPE_NON_LISP);
  interval_block->next = 0;
  bzero ((char *) interval_block->intervals, sizeof interval_block->intervals);
  interval_block_index = 0;
  interval_free_list = 0;
  n_interval_blocks = 1;
}


/* Return a new interval.  */

INTERVAL
make_interval ()
{
  INTERVAL val;

  if (interval_free_list)
    {
      val = interval_free_list;
      interval_free_list = INTERVAL_PARENT (interval_free_list);
    }
  else
    {
      if (interval_block_index == INTERVAL_BLOCK_SIZE)
	{
	  register struct interval_block *newi;

	  newi = (struct interval_block *) lisp_malloc (sizeof *newi,
							MEM_TYPE_NON_LISP);

	  VALIDATE_LISP_STORAGE (newi, sizeof *newi);
	  newi->next = interval_block;
	  interval_block = newi;
	  interval_block_index = 0;
	  n_interval_blocks++;
	}
      val = &interval_block->intervals[interval_block_index++];
    }
  consing_since_gc += sizeof (struct interval);
  intervals_consed++;
  RESET_INTERVAL (val);
  return val;
}


/* Mark Lisp objects in interval I. */

static void
mark_interval (i, dummy)
     register INTERVAL i;
     Lisp_Object dummy;
{
  if (XMARKBIT (i->plist))
    abort ();
  mark_object (&i->plist);
  XMARK (i->plist);
}


/* Mark the interval tree rooted in TREE.  Don't call this directly;
   use the macro MARK_INTERVAL_TREE instead.  */

static void
mark_interval_tree (tree)
     register INTERVAL tree;
{
  /* No need to test if this tree has been marked already; this
     function is always called through the MARK_INTERVAL_TREE macro,
     which takes care of that.  */

  /* XMARK expands to an assignment; the LHS of an assignment can't be
     a cast.  */
  XMARK (tree->up.obj);

  traverse_intervals_noorder (tree, mark_interval, Qnil);
}


/* Mark the interval tree rooted in I.  */

#define MARK_INTERVAL_TREE(i)				\
  do {							\
    if (!NULL_INTERVAL_P (i)				\
	&& ! XMARKBIT (i->up.obj))			\
      mark_interval_tree (i);				\
  } while (0)


/* The oddity in the call to XUNMARK is necessary because XUNMARK
   expands to an assignment to its argument, and most C compilers
   don't support casts on the left operand of `='.  */

#define UNMARK_BALANCE_INTERVALS(i)			\
  do {							\
   if (! NULL_INTERVAL_P (i))				\
     {							\
       XUNMARK ((i)->up.obj);				\
       (i) = balance_intervals (i);			\
     }							\
  } while (0)


/* Number support.  If NO_UNION_TYPE isn't in effect, we
   can't create number objects in macros.  */
#ifndef make_number
Lisp_Object
make_number (n)
     int n;
{
  Lisp_Object obj;
  obj.s.val = n;
  obj.s.type = Lisp_Int;
  return obj;
}
#endif

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
   `u.nbytes' member of the sdata.  So, sdata structures that are no
   longer used, can be easily recognized, and it's easy to compact the
   sblocks of small strings which we do in compact_small_strings.  */

/* Size in bytes of an sblock structure used for small strings.  This
   is 8192 minus malloc overhead.  */

#define SBLOCK_SIZE 8188

/* Strings larger than this are considered large strings.  String data
   for large strings is allocated from individual sblocks.  */

#define LARGE_STRING_BYTES 1024

/* Structure describing string memory sub-allocated from an sblock.
   This is where the contents of Lisp strings are stored.  */

struct sdata
{
  /* Back-pointer to the string this sdata belongs to.  If null, this
     structure is free, and the NBYTES member of the union below
     contains the string's byte size (the same value that STRING_BYTES
     would return if STRING were non-null).  If non-null, STRING_BYTES
     (STRING) is the size of the data, and DATA contains the string's
     contents.  */
  struct Lisp_String *string;

#ifdef GC_CHECK_STRING_BYTES
  
  EMACS_INT nbytes;
  unsigned char data[1];
  
#define SDATA_NBYTES(S)	(S)->nbytes
#define SDATA_DATA(S)	(S)->data
  
#else /* not GC_CHECK_STRING_BYTES */

  union
  {
    /* When STRING in non-null.  */
    unsigned char data[1];

    /* When STRING is null.  */
    EMACS_INT nbytes;
  } u;
  

#define SDATA_NBYTES(S)	(S)->u.nbytes
#define SDATA_DATA(S)	(S)->u.data

#endif /* not GC_CHECK_STRING_BYTES */
};


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
  struct sdata *next_free;

  /* Start of data.  */
  struct sdata first_data;
};

/* Number of Lisp strings in a string_block structure.  The 1020 is
   1024 minus malloc overhead.  */

#define STRINGS_IN_STRING_BLOCK \
  ((1020 - sizeof (struct string_block *)) / sizeof (struct Lisp_String))

/* Structure describing a block from which Lisp_String structures
   are allocated.  */

struct string_block
{
  struct string_block *next;
  struct Lisp_String strings[STRINGS_IN_STRING_BLOCK];
};

/* Head and tail of the list of sblock structures holding Lisp string
   data.  We always allocate from current_sblock.  The NEXT pointers
   in the sblock structures go from oldest_sblock to current_sblock.  */

static struct sblock *oldest_sblock, *current_sblock;

/* List of sblocks for large strings.  */

static struct sblock *large_sblocks;

/* List of string_block structures, and how many there are.  */

static struct string_block *string_blocks;
static int n_string_blocks;

/* Free-list of Lisp_Strings.  */

static struct Lisp_String *string_free_list;

/* Number of live and free Lisp_Strings.  */

static int total_strings, total_free_strings;

/* Number of bytes used by live strings.  */

static int total_string_size;

/* Given a pointer to a Lisp_String S which is on the free-list
   string_free_list, return a pointer to its successor in the
   free-list.  */

#define NEXT_FREE_LISP_STRING(S) (*(struct Lisp_String **) (S))

/* Return a pointer to the sdata structure belonging to Lisp string S.
   S must be live, i.e. S->data must not be null.  S->data is actually
   a pointer to the `u.data' member of its sdata structure; the
   structure starts at a constant offset in front of that.  */
   
#ifdef GC_CHECK_STRING_BYTES

#define SDATA_OF_STRING(S) \
     ((struct sdata *) ((S)->data - sizeof (struct Lisp_String *) \
			- sizeof (EMACS_INT)))

#else /* not GC_CHECK_STRING_BYTES */

#define SDATA_OF_STRING(S) \
     ((struct sdata *) ((S)->data - sizeof (struct Lisp_String *)))

#endif /* not GC_CHECK_STRING_BYTES */

/* Value is the size of an sdata structure large enough to hold NBYTES
   bytes of string data.  The value returned includes a terminating
   NUL byte, the size of the sdata structure, and padding.  */

#ifdef GC_CHECK_STRING_BYTES

#define SDATA_SIZE(NBYTES)			\
     ((sizeof (struct Lisp_String *)		\
       + (NBYTES) + 1				\
       + sizeof (EMACS_INT)			\
       + sizeof (EMACS_INT) - 1)		\
      & ~(sizeof (EMACS_INT) - 1))

#else /* not GC_CHECK_STRING_BYTES */

#define SDATA_SIZE(NBYTES)			\
     ((sizeof (struct Lisp_String *)		\
       + (NBYTES) + 1				\
       + sizeof (EMACS_INT) - 1)		\
      & ~(sizeof (EMACS_INT) - 1))

#endif /* not GC_CHECK_STRING_BYTES */

/* Initialize string allocation.  Called from init_alloc_once.  */

void
init_strings ()
{
  total_strings = total_free_strings = total_string_size = 0;
  oldest_sblock = current_sblock = large_sblocks = NULL;
  string_blocks = NULL;
  n_string_blocks = 0;
  string_free_list = NULL;
}


#ifdef GC_CHECK_STRING_BYTES

static int check_string_bytes_count;

void check_string_bytes P_ ((int));
void check_sblock P_ ((struct sblock *));

#define CHECK_STRING_BYTES(S)	STRING_BYTES (S)


/* Like GC_STRING_BYTES, but with debugging check.  */

int
string_bytes (s)
     struct Lisp_String *s;
{
  int nbytes = (s->size_byte < 0 ? s->size : s->size_byte) & ~MARKBIT;
  if (!PURE_POINTER_P (s)
      && s->data
      && nbytes != SDATA_NBYTES (SDATA_OF_STRING (s)))
    abort ();
  return nbytes;
}
    
/* Check validity Lisp strings' string_bytes member in B.  */

void
check_sblock (b)
     struct sblock *b;
{
  struct sdata *from, *end, *from_end;
      
  end = b->next_free;
      
  for (from = &b->first_data; from < end; from = from_end)
    {
      /* Compute the next FROM here because copying below may
	 overwrite data we need to compute it.  */
      int nbytes;
      
      /* Check that the string size recorded in the string is the
	 same as the one recorded in the sdata structure. */
      if (from->string)
	CHECK_STRING_BYTES (from->string);
      
      if (from->string)
	nbytes = GC_STRING_BYTES (from->string);
      else
	nbytes = SDATA_NBYTES (from);
      
      nbytes = SDATA_SIZE (nbytes);
      from_end = (struct sdata *) ((char *) from + nbytes);
    }
}


/* Check validity of Lisp strings' string_bytes member.  ALL_P
   non-zero means check all strings, otherwise check only most
   recently allocated strings.  Used for hunting a bug.  */

void
check_string_bytes (all_p)
     int all_p;
{
  if (all_p)
    {
      struct sblock *b;

      for (b = large_sblocks; b; b = b->next)
	{
	  struct Lisp_String *s = b->first_data.string;
	  if (s)
	    CHECK_STRING_BYTES (s);
	}
      
      for (b = oldest_sblock; b; b = b->next)
	check_sblock (b);
    }
  else
    check_sblock (current_sblock);
}

#endif /* GC_CHECK_STRING_BYTES */


/* Return a new Lisp_String.  */

static struct Lisp_String *
allocate_string ()
{
  struct Lisp_String *s;

  /* If the free-list is empty, allocate a new string_block, and
     add all the Lisp_Strings in it to the free-list.  */
  if (string_free_list == NULL)
    {
      struct string_block *b;
      int i;

      b = (struct string_block *) lisp_malloc (sizeof *b, MEM_TYPE_STRING);
      VALIDATE_LISP_STORAGE (b, sizeof *b);
      bzero (b, sizeof *b);
      b->next = string_blocks;
      string_blocks = b;
      ++n_string_blocks;

      for (i = STRINGS_IN_STRING_BLOCK - 1; i >= 0; --i)
	{
	  s = b->strings + i;
	  NEXT_FREE_LISP_STRING (s) = string_free_list;
	  string_free_list = s;
	}

      total_free_strings += STRINGS_IN_STRING_BLOCK;
    }

  /* Pop a Lisp_String off the free-list.  */
  s = string_free_list;
  string_free_list = NEXT_FREE_LISP_STRING (s);

  /* Probably not strictly necessary, but play it safe.  */
  bzero (s, sizeof *s);

  --total_free_strings;
  ++total_strings;
  ++strings_consed;
  consing_since_gc += sizeof *s;

#ifdef GC_CHECK_STRING_BYTES
  if (!noninteractive
#ifdef macintosh
      && current_sblock
#endif
     )
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
   plus a NUL byte at the end.  Allocate an sdata structure for S, and
   set S->data to its `u.data' member.  Store a NUL byte at the end of
   S->data.  Set S->size to NCHARS and S->size_byte to NBYTES.  Free
   S->data if it was initially non-null.  */

void
allocate_string_data (s, nchars, nbytes)
     struct Lisp_String *s;
     int nchars, nbytes;
{
  struct sdata *data, *old_data;
  struct sblock *b;
  int needed, old_nbytes;

  /* Determine the number of bytes needed to store NBYTES bytes
     of string data.  */
  needed = SDATA_SIZE (nbytes);

  if (nbytes > LARGE_STRING_BYTES)
    {
      size_t size = sizeof *b - sizeof (struct sdata) + needed;

#ifdef DOUG_LEA_MALLOC
      /* Prevent mmap'ing the chunk.  Lisp data may not be mmap'ed
	 because mapped region contents are not preserved in
	 a dumped Emacs.  */
      mallopt (M_MMAP_MAX, 0);
#endif

      b = (struct sblock *) lisp_malloc (size, MEM_TYPE_NON_LISP);
      
#ifdef DOUG_LEA_MALLOC
      /* Back to a reasonable maximum of mmap'ed areas. */
      mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif
  
      b->next_free = &b->first_data;
      b->first_data.string = NULL;
      b->next = large_sblocks;
      large_sblocks = b;
    }
  else if (current_sblock == NULL
	   || (((char *) current_sblock + SBLOCK_SIZE
		- (char *) current_sblock->next_free)
	       < needed))
    {
      /* Not enough room in the current sblock.  */
      b = (struct sblock *) lisp_malloc (SBLOCK_SIZE, MEM_TYPE_NON_LISP);
      b->next_free = &b->first_data;
      b->first_data.string = NULL;
      b->next = NULL;

      if (current_sblock)
	current_sblock->next = b;
      else
	oldest_sblock = b;
      current_sblock = b;
    }
  else
    b = current_sblock;

  old_data = s->data ? SDATA_OF_STRING (s) : NULL;
  old_nbytes = GC_STRING_BYTES (s);
  
  data = b->next_free;
  data->string = s;
  s->data = SDATA_DATA (data);
#ifdef GC_CHECK_STRING_BYTES
  SDATA_NBYTES (data) = nbytes;
#endif
  s->size = nchars;
  s->size_byte = nbytes;
  s->data[nbytes] = '\0';
  b->next_free = (struct sdata *) ((char *) data + needed);
  
  /* If S had already data assigned, mark that as free by setting its
     string back-pointer to null, and recording the size of the data
     in it.  */
  if (old_data)
    {
      SDATA_NBYTES (old_data) = old_nbytes;
      old_data->string = NULL;
    }

  consing_since_gc += needed;
}


/* Sweep and compact strings.  */

static void
sweep_strings ()
{
  struct string_block *b, *next;
  struct string_block *live_blocks = NULL;
  
  string_free_list = NULL;
  total_strings = total_free_strings = 0;
  total_string_size = 0;

  /* Scan strings_blocks, free Lisp_Strings that aren't marked.  */
  for (b = string_blocks; b; b = next)
    {
      int i, nfree = 0;
      struct Lisp_String *free_list_before = string_free_list;

      next = b->next;

      for (i = 0; i < STRINGS_IN_STRING_BLOCK; ++i)
	{
	  struct Lisp_String *s = b->strings + i;

	  if (s->data)
	    {
	      /* String was not on free-list before.  */
	      if (STRING_MARKED_P (s))
		{
		  /* String is live; unmark it and its intervals.  */
		  UNMARK_STRING (s);
		  
		  if (!NULL_INTERVAL_P (s->intervals))
		    UNMARK_BALANCE_INTERVALS (s->intervals);

		  ++total_strings;
		  total_string_size += STRING_BYTES (s);
		}
	      else
		{
		  /* String is dead.  Put it on the free-list.  */
		  struct sdata *data = SDATA_OF_STRING (s);

		  /* Save the size of S in its sdata so that we know
		     how large that is.  Reset the sdata's string
		     back-pointer so that we know it's free.  */
#ifdef GC_CHECK_STRING_BYTES
		  if (GC_STRING_BYTES (s) != SDATA_NBYTES (data))
		    abort ();
#else
		  data->u.nbytes = GC_STRING_BYTES (s);
#endif
		  data->string = NULL;

		  /* Reset the strings's `data' member so that we
		     know it's free.  */
		  s->data = NULL;

		  /* Put the string on the free-list.  */
		  NEXT_FREE_LISP_STRING (s) = string_free_list;
		  string_free_list = s;
		  ++nfree;
		}
	    }
	  else
	    {
	      /* S was on the free-list before.  Put it there again.  */
	      NEXT_FREE_LISP_STRING (s) = string_free_list;
	      string_free_list = s;
	      ++nfree;
	    }
	}

      /* Free blocks that contain free Lisp_Strings only, except
	 the first two of them.  */
      if (nfree == STRINGS_IN_STRING_BLOCK
	  && total_free_strings > STRINGS_IN_STRING_BLOCK)
	{
	  lisp_free (b);
	  --n_string_blocks;
	  string_free_list = free_list_before;
	}
      else
	{
	  total_free_strings += nfree;
	  b->next = live_blocks;
	  live_blocks = b;
	}
    }

  string_blocks = live_blocks;
  free_large_strings ();
  compact_small_strings ();
}


/* Free dead large strings.  */

static void
free_large_strings ()
{
  struct sblock *b, *next;
  struct sblock *live_blocks = NULL;
  
  for (b = large_sblocks; b; b = next)
    {
      next = b->next;

      if (b->first_data.string == NULL)
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
compact_small_strings ()
{
  struct sblock *b, *tb, *next;
  struct sdata *from, *to, *end, *tb_end;
  struct sdata *to_end, *from_end;

  /* TB is the sblock we copy to, TO is the sdata within TB we copy
     to, and TB_END is the end of TB.  */
  tb = oldest_sblock;
  tb_end = (struct sdata *) ((char *) tb + SBLOCK_SIZE);
  to = &tb->first_data;

  /* Step through the blocks from the oldest to the youngest.  We
     expect that old blocks will stabilize over time, so that less
     copying will happen this way.  */
  for (b = oldest_sblock; b; b = b->next)
    {
      end = b->next_free;
      xassert ((char *) end <= (char *) b + SBLOCK_SIZE);
      
      for (from = &b->first_data; from < end; from = from_end)
	{
	  /* Compute the next FROM here because copying below may
	     overwrite data we need to compute it.  */
	  int nbytes;

#ifdef GC_CHECK_STRING_BYTES
	  /* Check that the string size recorded in the string is the
	     same as the one recorded in the sdata structure. */
	  if (from->string
	      && GC_STRING_BYTES (from->string) != SDATA_NBYTES (from))
	    abort ();
#endif /* GC_CHECK_STRING_BYTES */
	  
	  if (from->string)
	    nbytes = GC_STRING_BYTES (from->string);
	  else
	    nbytes = SDATA_NBYTES (from);
	  
	  nbytes = SDATA_SIZE (nbytes);
	  from_end = (struct sdata *) ((char *) from + nbytes);
	  
	  /* FROM->string non-null means it's alive.  Copy its data.  */
	  if (from->string)
	    {
	      /* If TB is full, proceed with the next sblock.  */
	      to_end = (struct sdata *) ((char *) to + nbytes);
	      if (to_end > tb_end)
		{
		  tb->next_free = to;
		  tb = tb->next;
		  tb_end = (struct sdata *) ((char *) tb + SBLOCK_SIZE);
		  to = &tb->first_data;
		  to_end = (struct sdata *) ((char *) to + nbytes);
		}
	      
	      /* Copy, and update the string's `data' pointer.  */
	      if (from != to)
		{
		  xassert (tb != b || to <= from);
		  safe_bcopy ((char *) from, (char *) to, nbytes);
		  to->string->data = SDATA_DATA (to);
		}

	      /* Advance past the sdata we copied to.  */
	      to = to_end;
	    }
	}
    }

  /* The rest of the sblocks following TB don't contain live data, so
     we can free them.  */
  for (b = tb->next; b; b = next)
    {
      next = b->next;
      lisp_free (b);
    }

  tb->next_free = to;
  tb->next = NULL;
  current_sblock = tb;
}


DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
       doc: /* Return a newly created string of length LENGTH, with each element being INIT.
Both LENGTH and INIT must be numbers.  */)
     (length, init)
     Lisp_Object length, init;
{
  register Lisp_Object val;
  register unsigned char *p, *end;
  int c, nbytes;

  CHECK_NATNUM (length);
  CHECK_NUMBER (init);

  c = XINT (init);
  if (SINGLE_BYTE_CHAR_P (c))
    {
      nbytes = XINT (length);
      val = make_uninit_string (nbytes);
      p = XSTRING (val)->data;
      end = p + XSTRING (val)->size;
      while (p != end)
	*p++ = c;
    }
  else
    {
      unsigned char str[MAX_MULTIBYTE_LENGTH];
      int len = CHAR_STRING (c, str);

      nbytes = len * XINT (length);
      val = make_uninit_multibyte_string (XINT (length), nbytes);
      p = XSTRING (val)->data;
      end = p + nbytes;
      while (p != end)
	{
	  bcopy (str, p, len);
	  p += len;
	}
    }
  
  *p = 0;
  return val;
}


DEFUN ("make-bool-vector", Fmake_bool_vector, Smake_bool_vector, 2, 2, 0,
       doc: /* Return a new bool-vector of length LENGTH, using INIT for as each element.
LENGTH must be a number.  INIT matters only in whether it is t or nil.  */)
     (length, init)
     Lisp_Object length, init;
{
  register Lisp_Object val;
  struct Lisp_Bool_Vector *p;
  int real_init, i;
  int length_in_chars, length_in_elts, bits_per_value;

  CHECK_NATNUM (length);

  bits_per_value = sizeof (EMACS_INT) * BITS_PER_CHAR;

  length_in_elts = (XFASTINT (length) + bits_per_value - 1) / bits_per_value;
  length_in_chars = ((XFASTINT (length) + BITS_PER_CHAR - 1) / BITS_PER_CHAR);

  /* We must allocate one more elements than LENGTH_IN_ELTS for the
     slot `size' of the struct Lisp_Bool_Vector.  */
  val = Fmake_vector (make_number (length_in_elts + 1), Qnil);
  p = XBOOL_VECTOR (val);
  
  /* Get rid of any bits that would cause confusion.  */
  p->vector_size = 0;
  XSETBOOL_VECTOR (val, p);
  p->size = XFASTINT (length);
  
  real_init = (NILP (init) ? 0 : -1);
  for (i = 0; i < length_in_chars ; i++)
    p->data[i] = real_init;
  
  /* Clear the extraneous bits in the last byte.  */
  if (XINT (length) != length_in_chars * BITS_PER_CHAR)
    XBOOL_VECTOR (val)->data[length_in_chars - 1]
      &= (1 << (XINT (length) % BITS_PER_CHAR)) - 1;

  return val;
}


/* Make a string from NBYTES bytes at CONTENTS, and compute the number
   of characters from the contents.  This string may be unibyte or
   multibyte, depending on the contents.  */

Lisp_Object
make_string (contents, nbytes)
     char *contents;
     int nbytes;
{
  register Lisp_Object val;
  int nchars, multibyte_nbytes;

  parse_str_as_multibyte (contents, nbytes, &nchars, &multibyte_nbytes);
  if (nbytes == nchars || nbytes != multibyte_nbytes)
    /* CONTENTS contains no multibyte sequences or contains an invalid
       multibyte sequence.  We must make unibyte string.  */
    val = make_unibyte_string (contents, nbytes);
  else
    val = make_multibyte_string (contents, nchars, nbytes);
  return val;
}


/* Make an unibyte string from LENGTH bytes at CONTENTS.  */

Lisp_Object
make_unibyte_string (contents, length)
     char *contents;
     int length;
{
  register Lisp_Object val;
  val = make_uninit_string (length);
  bcopy (contents, XSTRING (val)->data, length);
  SET_STRING_BYTES (XSTRING (val), -1);
  return val;
}


/* Make a multibyte string from NCHARS characters occupying NBYTES
   bytes at CONTENTS.  */

Lisp_Object
make_multibyte_string (contents, nchars, nbytes)
     char *contents;
     int nchars, nbytes;
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  bcopy (contents, XSTRING (val)->data, nbytes);
  return val;
}


/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  It is a multibyte string if NBYTES != NCHARS.  */

Lisp_Object
make_string_from_bytes (contents, nchars, nbytes)
     char *contents;
     int nchars, nbytes;
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  bcopy (contents, XSTRING (val)->data, nbytes);
  if (STRING_BYTES (XSTRING (val)) == XSTRING (val)->size)
    SET_STRING_BYTES (XSTRING (val), -1);
  return val;
}


/* Make a string from NCHARS characters occupying NBYTES bytes at
   CONTENTS.  The argument MULTIBYTE controls whether to label the
   string as multibyte.  */

Lisp_Object
make_specified_string (contents, nchars, nbytes, multibyte)
     char *contents;
     int nchars, nbytes;
     int multibyte;
{
  register Lisp_Object val;
  val = make_uninit_multibyte_string (nchars, nbytes);
  bcopy (contents, XSTRING (val)->data, nbytes);
  if (!multibyte)
    SET_STRING_BYTES (XSTRING (val), -1);
  return val;
}


/* Make a string from the data at STR, treating it as multibyte if the
   data warrants.  */

Lisp_Object
build_string (str)
     char *str;
{
  return make_string (str, strlen (str));
}


/* Return an unibyte Lisp_String set up to hold LENGTH characters
   occupying LENGTH bytes.  */

Lisp_Object
make_uninit_string (length)
     int length;
{
  Lisp_Object val;
  val = make_uninit_multibyte_string (length, length);
  SET_STRING_BYTES (XSTRING (val), -1);
  return val;
}


/* Return a multibyte Lisp_String set up to hold NCHARS characters
   which occupy NBYTES bytes.  */

Lisp_Object
make_uninit_multibyte_string (nchars, nbytes)
     int nchars, nbytes;
{
  Lisp_Object string;
  struct Lisp_String *s;

  if (nchars < 0)
    abort ();

  s = allocate_string ();
  allocate_string_data (s, nchars, nbytes);
  XSETSTRING (string, s);
  string_chars_consed += nbytes;
  return string;
}



/***********************************************************************
			   Float Allocation
 ***********************************************************************/

/* We store float cells inside of float_blocks, allocating a new
   float_block with malloc whenever necessary.  Float cells reclaimed
   by GC are put on a free list to be reallocated before allocating
   any new float cells from the latest float_block.

   Each float_block is just under 1020 bytes long, since malloc really
   allocates in units of powers of two and uses 4 bytes for its own
   overhead. */

#define FLOAT_BLOCK_SIZE \
  ((1020 - sizeof (struct float_block *)) / sizeof (struct Lisp_Float))

struct float_block
{
  struct float_block *next;
  struct Lisp_Float floats[FLOAT_BLOCK_SIZE];
};

/* Current float_block.  */

struct float_block *float_block;

/* Index of first unused Lisp_Float in the current float_block.  */

int float_block_index;

/* Total number of float blocks now in use.  */

int n_float_blocks;

/* Free-list of Lisp_Floats.  */

struct Lisp_Float *float_free_list;


/* Initialize float allocation.  */

void
init_float ()
{
  float_block = (struct float_block *) lisp_malloc (sizeof *float_block,
						    MEM_TYPE_FLOAT);
  float_block->next = 0;
  bzero ((char *) float_block->floats, sizeof float_block->floats);
  float_block_index = 0;
  float_free_list = 0;
  n_float_blocks = 1;
}


/* Explicitly free a float cell by putting it on the free-list.  */

void
free_float (ptr)
     struct Lisp_Float *ptr;
{
  *(struct Lisp_Float **)&ptr->data = float_free_list;
#if GC_MARK_STACK
  ptr->type = Vdead;
#endif
  float_free_list = ptr;
}


/* Return a new float object with value FLOAT_VALUE.  */

Lisp_Object
make_float (float_value)
     double float_value;
{
  register Lisp_Object val;

  if (float_free_list)
    {
      /* We use the data field for chaining the free list
	 so that we won't use the same field that has the mark bit.  */
      XSETFLOAT (val, float_free_list);
      float_free_list = *(struct Lisp_Float **)&float_free_list->data;
    }
  else
    {
      if (float_block_index == FLOAT_BLOCK_SIZE)
	{
	  register struct float_block *new;

	  new = (struct float_block *) lisp_malloc (sizeof *new,
						    MEM_TYPE_FLOAT);
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = float_block;
	  float_block = new;
	  float_block_index = 0;
	  n_float_blocks++;
	}
      XSETFLOAT (val, &float_block->floats[float_block_index++]);
    }
  
  XFLOAT_DATA (val) = float_value;
  XSETFASTINT (XFLOAT (val)->type, 0);	/* bug chasing -wsr */
  consing_since_gc += sizeof (struct Lisp_Float);
  floats_consed++;
  return val;
}



/***********************************************************************
			   Cons Allocation
 ***********************************************************************/

/* We store cons cells inside of cons_blocks, allocating a new
   cons_block with malloc whenever necessary.  Cons cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new cons cells from the latest cons_block.

   Each cons_block is just under 1020 bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#define CONS_BLOCK_SIZE \
  ((1020 - sizeof (struct cons_block *)) / sizeof (struct Lisp_Cons))

struct cons_block
{
  struct cons_block *next;
  struct Lisp_Cons conses[CONS_BLOCK_SIZE];
};

/* Current cons_block.  */

struct cons_block *cons_block;

/* Index of first unused Lisp_Cons in the current block.  */

int cons_block_index;

/* Free-list of Lisp_Cons structures.  */

struct Lisp_Cons *cons_free_list;

/* Total number of cons blocks now in use.  */

int n_cons_blocks;


/* Initialize cons allocation.  */

void
init_cons ()
{
  cons_block = (struct cons_block *) lisp_malloc (sizeof *cons_block,
						  MEM_TYPE_CONS);
  cons_block->next = 0;
  bzero ((char *) cons_block->conses, sizeof cons_block->conses);
  cons_block_index = 0;
  cons_free_list = 0;
  n_cons_blocks = 1;
}


/* Explicitly free a cons cell by putting it on the free-list.  */

void
free_cons (ptr)
     struct Lisp_Cons *ptr;
{
  *(struct Lisp_Cons **)&ptr->cdr = cons_free_list;
#if GC_MARK_STACK
  ptr->car = Vdead;
#endif
  cons_free_list = ptr;
}


DEFUN ("cons", Fcons, Scons, 2, 2, 0,
       doc: /* Create a new cons, give it CAR and CDR as components, and return it.  */)
     (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      /* We use the cdr for chaining the free list
	 so that we won't use the same field that has the mark bit.  */
      XSETCONS (val, cons_free_list);
      cons_free_list = *(struct Lisp_Cons **)&cons_free_list->cdr;
    }
  else
    {
      if (cons_block_index == CONS_BLOCK_SIZE)
	{
	  register struct cons_block *new;
	  new = (struct cons_block *) lisp_malloc (sizeof *new,
						   MEM_TYPE_CONS);
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	  n_cons_blocks++;
	}
      XSETCONS (val, &cons_block->conses[cons_block_index++]);
    }
  
  XSETCAR (val, car);
  XSETCDR (val, cdr);
  consing_since_gc += sizeof (struct Lisp_Cons);
  cons_cells_consed++;
  return val;
}


/* Make a list of 2, 3, 4 or 5 specified objects.  */

Lisp_Object
list2 (arg1, arg2)
     Lisp_Object arg1, arg2;
{
  return Fcons (arg1, Fcons (arg2, Qnil));
}


Lisp_Object
list3 (arg1, arg2, arg3)
     Lisp_Object arg1, arg2, arg3;
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Qnil)));
}


Lisp_Object
list4 (arg1, arg2, arg3, arg4)
     Lisp_Object arg1, arg2, arg3, arg4;
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4, Qnil))));
}


Lisp_Object
list5 (arg1, arg2, arg3, arg4, arg5)
     Lisp_Object arg1, arg2, arg3, arg4, arg5;
{
  return Fcons (arg1, Fcons (arg2, Fcons (arg3, Fcons (arg4,
						       Fcons (arg5, Qnil)))));
}


DEFUN ("list", Flist, Slist, 0, MANY, 0,
       doc: /* Return a newly created list with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
usage: (list &rest OBJECTS)  */)
     (nargs, args)
     int nargs;
     register Lisp_Object *args;
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
     (length, init)
     register Lisp_Object length, init;
{
  register Lisp_Object val;
  register int size;

  CHECK_NATNUM (length);
  size = XFASTINT (length);

  val = Qnil;
  while (size > 0)
    {
      val = Fcons (init, val);
      --size;

      if (size > 0)
	{
	  val = Fcons (init, val);
	  --size;
      
	  if (size > 0)
	    {
	      val = Fcons (init, val);
	      --size;
      
	      if (size > 0)
		{
		  val = Fcons (init, val);
		  --size;
      
		  if (size > 0)
		    {
		      val = Fcons (init, val);
		      --size;
		    }
		}
	    }
	}

      QUIT;
    }
  
  return val;
}



/***********************************************************************
			   Vector Allocation
 ***********************************************************************/

/* Singly-linked list of all vectors.  */

struct Lisp_Vector *all_vectors;

/* Total number of vector-like objects now in use.  */

int n_vectors;


/* Value is a pointer to a newly allocated Lisp_Vector structure
   with room for LEN Lisp_Objects.  */

static struct Lisp_Vector *
allocate_vectorlike (len, type)
     EMACS_INT len;
     enum mem_type type;
{
  struct Lisp_Vector *p;
  size_t nbytes;

#ifdef DOUG_LEA_MALLOC
  /* Prevent mmap'ing the chunk.  Lisp data may not be mmap'ed
     because mapped region contents are not preserved in
     a dumped Emacs.  */
  mallopt (M_MMAP_MAX, 0);
#endif
  
  nbytes = sizeof *p + (len - 1) * sizeof p->contents[0];
  p = (struct Lisp_Vector *) lisp_malloc (nbytes, type);
  
#ifdef DOUG_LEA_MALLOC
  /* Back to a reasonable maximum of mmap'ed areas.  */
  mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif
  
  VALIDATE_LISP_STORAGE (p, 0);
  consing_since_gc += nbytes;
  vector_cells_consed += len;

  p->next = all_vectors;
  all_vectors = p;
  ++n_vectors;
  return p;
}


/* Allocate a vector with NSLOTS slots.  */

struct Lisp_Vector *
allocate_vector (nslots)
     EMACS_INT nslots;
{
  struct Lisp_Vector *v = allocate_vectorlike (nslots, MEM_TYPE_VECTOR);
  v->size = nslots;
  return v;
}


/* Allocate other vector-like structures.  */

struct Lisp_Hash_Table *
allocate_hash_table ()
{
  EMACS_INT len = VECSIZE (struct Lisp_Hash_Table);
  struct Lisp_Vector *v = allocate_vectorlike (len, MEM_TYPE_HASH_TABLE);
  EMACS_INT i;
  
  v->size = len;
  for (i = 0; i < len; ++i)
    v->contents[i] = Qnil;
  
  return (struct Lisp_Hash_Table *) v;
}


struct window *
allocate_window ()
{
  EMACS_INT len = VECSIZE (struct window);
  struct Lisp_Vector *v = allocate_vectorlike (len, MEM_TYPE_WINDOW);
  EMACS_INT i;
  
  for (i = 0; i < len; ++i)
    v->contents[i] = Qnil;
  v->size = len;
  
  return (struct window *) v;
}


struct frame *
allocate_frame ()
{
  EMACS_INT len = VECSIZE (struct frame);
  struct Lisp_Vector *v = allocate_vectorlike (len, MEM_TYPE_FRAME);
  EMACS_INT i;
  
  for (i = 0; i < len; ++i)
    v->contents[i] = make_number (0);
  v->size = len;
  return (struct frame *) v;
}


struct Lisp_Process *
allocate_process ()
{
  EMACS_INT len = VECSIZE (struct Lisp_Process);
  struct Lisp_Vector *v = allocate_vectorlike (len, MEM_TYPE_PROCESS);
  EMACS_INT i;
  
  for (i = 0; i < len; ++i)
    v->contents[i] = Qnil;
  v->size = len;
  
  return (struct Lisp_Process *) v;
}


struct Lisp_Vector *
allocate_other_vector (len)
     EMACS_INT len;
{
  struct Lisp_Vector *v = allocate_vectorlike (len, MEM_TYPE_VECTOR);
  EMACS_INT i;
  
  for (i = 0; i < len; ++i)
    v->contents[i] = Qnil;
  v->size = len;
  
  return v;
}


DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
       doc: /* Return a newly created vector of length LENGTH, with each element being INIT.
See also the function `vector'.  */)
     (length, init)
     register Lisp_Object length, init;
{
  Lisp_Object vector;
  register EMACS_INT sizei;
  register int index;
  register struct Lisp_Vector *p;

  CHECK_NATNUM (length);
  sizei = XFASTINT (length);

  p = allocate_vector (sizei);
  for (index = 0; index < sizei; index++)
    p->contents[index] = init;

  XSETVECTOR (vector, p);
  return vector;
}


DEFUN ("make-char-table", Fmake_char_table, Smake_char_table, 1, 2, 0,
       doc: /* Return a newly created char-table, with purpose PURPOSE.
Each element is initialized to INIT, which defaults to nil.
PURPOSE should be a symbol which has a `char-table-extra-slots' property.
The property's value should be an integer between 0 and 10.  */)
     (purpose, init)
     register Lisp_Object purpose, init;
{
  Lisp_Object vector;
  Lisp_Object n;
  CHECK_SYMBOL (purpose);
  n = Fget (purpose, Qchar_table_extra_slots);
  CHECK_NUMBER (n);
  if (XINT (n) < 0 || XINT (n) > 10)
    args_out_of_range (n, Qnil);
  /* Add 2 to the size for the defalt and parent slots.  */
  vector = Fmake_vector (make_number (CHAR_TABLE_STANDARD_SLOTS + XINT (n)),
			 init);
  XCHAR_TABLE (vector)->top = Qt;
  XCHAR_TABLE (vector)->parent = Qnil;
  XCHAR_TABLE (vector)->purpose = purpose;
  XSETCHAR_TABLE (vector, XCHAR_TABLE (vector));
  return vector;
}


/* Return a newly created sub char table with default value DEFALT.
   Since a sub char table does not appear as a top level Emacs Lisp
   object, we don't need a Lisp interface to make it.  */

Lisp_Object
make_sub_char_table (defalt)
     Lisp_Object defalt;
{
  Lisp_Object vector
    = Fmake_vector (make_number (SUB_CHAR_TABLE_STANDARD_SLOTS), Qnil);
  XCHAR_TABLE (vector)->top = Qnil;
  XCHAR_TABLE (vector)->defalt = defalt;
  XSETCHAR_TABLE (vector, XCHAR_TABLE (vector));
  return vector;
}


DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
       doc: /* Return a newly created vector with specified arguments as elements.
Any number of arguments, even zero arguments, are allowed.
usage: (vector &rest OBJECTS)  */)
     (nargs, args)
     register int nargs;
     Lisp_Object *args;
{
  register Lisp_Object len, val;
  register int index;
  register struct Lisp_Vector *p;

  XSETFASTINT (len, nargs);
  val = Fmake_vector (len, Qnil);
  p = XVECTOR (val);
  for (index = 0; index < nargs; index++)
    p->contents[index] = args[index];
  return val;
}


DEFUN ("make-byte-code", Fmake_byte_code, Smake_byte_code, 4, MANY, 0,
       doc: /* Create a byte-code object with specified arguments as elements.
The arguments should be the arglist, bytecode-string, constant vector,
stack size, (optional) doc string, and (optional) interactive spec.
The first four arguments are required; at most six have any
significance.
usage: (make-byte-code &rest ELEMENTS)  */)
     (nargs, args)
     register int nargs;
     Lisp_Object *args;
{
  register Lisp_Object len, val;
  register int index;
  register struct Lisp_Vector *p;

  XSETFASTINT (len, nargs);
  if (!NILP (Vpurify_flag))
    val = make_pure_vector ((EMACS_INT) nargs);
  else
    val = Fmake_vector (len, Qnil);

  if (STRINGP (args[1]) && STRING_MULTIBYTE (args[1]))
    /* BYTECODE-STRING must have been produced by Emacs 20.2 or the
       earlier because they produced a raw 8-bit string for byte-code
       and now such a byte-code string is loaded as multibyte while
       raw 8-bit characters converted to multibyte form.  Thus, now we
       must convert them back to the original unibyte form.  */
    args[1] = Fstring_as_unibyte (args[1]);

  p = XVECTOR (val);
  for (index = 0; index < nargs; index++)
    {
      if (!NILP (Vpurify_flag))
	args[index] = Fpurecopy (args[index]);
      p->contents[index] = args[index];
    }
  XSETCOMPILED (val, p);
  return val;
}



/***********************************************************************
			   Symbol Allocation
 ***********************************************************************/

/* Each symbol_block is just under 1020 bytes long, since malloc
   really allocates in units of powers of two and uses 4 bytes for its
   own overhead. */

#define SYMBOL_BLOCK_SIZE \
  ((1020 - sizeof (struct symbol_block *)) / sizeof (struct Lisp_Symbol))

struct symbol_block
{
  struct symbol_block *next;
  struct Lisp_Symbol symbols[SYMBOL_BLOCK_SIZE];
};

/* Current symbol block and index of first unused Lisp_Symbol
   structure in it.  */

struct symbol_block *symbol_block;
int symbol_block_index;

/* List of free symbols.  */

struct Lisp_Symbol *symbol_free_list;

/* Total number of symbol blocks now in use.  */

int n_symbol_blocks;


/* Initialize symbol allocation.  */

void
init_symbol ()
{
  symbol_block = (struct symbol_block *) lisp_malloc (sizeof *symbol_block,
						      MEM_TYPE_SYMBOL);
  symbol_block->next = 0;
  bzero ((char *) symbol_block->symbols, sizeof symbol_block->symbols);
  symbol_block_index = 0;
  symbol_free_list = 0;
  n_symbol_blocks = 1;
}


DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
       doc: /* Return a newly allocated uninterned symbol whose name is NAME.
Its value and function definition are void, and its property list is nil.  */)
     (name)
     Lisp_Object name;
{
  register Lisp_Object val;
  register struct Lisp_Symbol *p;

  CHECK_STRING (name);

  if (symbol_free_list)
    {
      XSETSYMBOL (val, symbol_free_list);
      symbol_free_list = *(struct Lisp_Symbol **)&symbol_free_list->value;
    }
  else
    {
      if (symbol_block_index == SYMBOL_BLOCK_SIZE)
	{
	  struct symbol_block *new;
	  new = (struct symbol_block *) lisp_malloc (sizeof *new,
						     MEM_TYPE_SYMBOL);
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = symbol_block;
	  symbol_block = new;
	  symbol_block_index = 0;
	  n_symbol_blocks++;
	}
      XSETSYMBOL (val, &symbol_block->symbols[symbol_block_index++]);
    }
  
  p = XSYMBOL (val);
  p->name = XSTRING (name);
  p->plist = Qnil;
  p->value = Qunbound;
  p->function = Qunbound;
  p->next = NULL;
  p->interned = SYMBOL_UNINTERNED;
  p->constant = 0;
  p->indirect_variable = 0;
  consing_since_gc += sizeof (struct Lisp_Symbol);
  symbols_consed++;
  return val;
}



/***********************************************************************
		       Marker (Misc) Allocation
 ***********************************************************************/

/* Allocation of markers and other objects that share that structure.
   Works like allocation of conses. */

#define MARKER_BLOCK_SIZE \
  ((1020 - sizeof (struct marker_block *)) / sizeof (union Lisp_Misc))

struct marker_block
{
  struct marker_block *next;
  union Lisp_Misc markers[MARKER_BLOCK_SIZE];
};

struct marker_block *marker_block;
int marker_block_index;

union Lisp_Misc *marker_free_list;

/* Total number of marker blocks now in use.  */

int n_marker_blocks;

void
init_marker ()
{
  marker_block = (struct marker_block *) lisp_malloc (sizeof *marker_block,
						      MEM_TYPE_MISC);
  marker_block->next = 0;
  bzero ((char *) marker_block->markers, sizeof marker_block->markers);
  marker_block_index = 0;
  marker_free_list = 0;
  n_marker_blocks = 1;
}

/* Return a newly allocated Lisp_Misc object, with no substructure.  */

Lisp_Object
allocate_misc ()
{
  Lisp_Object val;

  if (marker_free_list)
    {
      XSETMISC (val, marker_free_list);
      marker_free_list = marker_free_list->u_free.chain;
    }
  else
    {
      if (marker_block_index == MARKER_BLOCK_SIZE)
	{
	  struct marker_block *new;
	  new = (struct marker_block *) lisp_malloc (sizeof *new,
						     MEM_TYPE_MISC);
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = marker_block;
	  marker_block = new;
	  marker_block_index = 0;
	  n_marker_blocks++;
	}
      XSETMISC (val, &marker_block->markers[marker_block_index++]);
    }
  
  consing_since_gc += sizeof (union Lisp_Misc);
  misc_objects_consed++;
  return val;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
       doc: /* Return a newly allocated marker which does not point at any place.  */)
     ()
{
  register Lisp_Object val;
  register struct Lisp_Marker *p;

  val = allocate_misc ();
  XMISCTYPE (val) = Lisp_Misc_Marker;
  p = XMARKER (val);
  p->buffer = 0;
  p->bytepos = 0;
  p->charpos = 0;
  p->chain = Qnil;
  p->insertion_type = 0;
  return val;
}

/* Put MARKER back on the free list after using it temporarily.  */

void
free_marker (marker)
     Lisp_Object marker;
{
  unchain_marker (marker);

  XMISC (marker)->u_marker.type = Lisp_Misc_Free;
  XMISC (marker)->u_free.chain = marker_free_list;
  marker_free_list = XMISC (marker);

  total_free_markers++;
}


/* Return a newly created vector or string with specified arguments as
   elements.  If all the arguments are characters that can fit
   in a string of events, make a string; otherwise, make a vector.

   Any number of arguments, even zero arguments, are allowed.  */

Lisp_Object
make_event_array (nargs, args)
     register int nargs;
     Lisp_Object *args;
{
  int i;

  for (i = 0; i < nargs; i++)
    /* The things that fit in a string
       are characters that are in 0...127,
       after discarding the meta bit and all the bits above it.  */
    if (!INTEGERP (args[i])
	|| (XUINT (args[i]) & ~(-CHAR_META)) >= 0200)
      return Fvector (nargs, args);

  /* Since the loop exited, we know that all the things in it are
     characters, so we can make a string.  */
  {
    Lisp_Object result;
    
    result = Fmake_string (make_number (nargs), make_number (0));
    for (i = 0; i < nargs; i++)
      {
	XSTRING (result)->data[i] = XINT (args[i]);
	/* Move the meta bit to the right place for a string char.  */
	if (XINT (args[i]) & CHAR_META)
	  XSTRING (result)->data[i] |= 0x80;
      }
    
    return result;
  }
}



/************************************************************************
			   C Stack Marking
 ************************************************************************/

#if GC_MARK_STACK || defined GC_MALLOC_CHECK

/* Conservative C stack marking requires a method to identify possibly
   live Lisp objects given a pointer value.  We do this by keeping
   track of blocks of Lisp data that are allocated in a red-black tree
   (see also the comment of mem_node which is the type of nodes in
   that tree).  Function lisp_malloc adds information for an allocated
   block to the red-black tree with calls to mem_insert, and function
   lisp_free removes it with mem_delete.  Functions live_string_p etc
   call mem_find to lookup information about a given pointer in the
   tree, and use that to determine if the pointer points to a Lisp
   object or not.  */

/* Initialize this part of alloc.c.  */

static void
mem_init ()
{
  mem_z.left = mem_z.right = MEM_NIL;
  mem_z.parent = NULL;
  mem_z.color = MEM_BLACK;
  mem_z.start = mem_z.end = NULL;
  mem_root = MEM_NIL;
}


/* Value is a pointer to the mem_node containing START.  Value is
   MEM_NIL if there is no node in the tree containing START.  */

static INLINE struct mem_node *
mem_find (start)
     void *start;
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
mem_insert (start, end, type)
     void *start, *end;
     enum mem_type type;
{
  struct mem_node *c, *parent, *x;

  if (start < min_heap_address)
    min_heap_address = start;
  if (end > max_heap_address)
    max_heap_address = end;

  /* See where in the tree a node for START belongs.  In this
     particular application, it shouldn't happen that a node is already
     present.  For debugging purposes, let's check that.  */
  c = mem_root;
  parent = NULL;

#if GC_MARK_STACK != GC_MAKE_GCPROS_NOOPS
     
  while (c != MEM_NIL)
    {
      if (start >= c->start && start < c->end)
	abort ();
      parent = c;
      c = start < c->start ? c->left : c->right;
    }
     
#else /* GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS */
     
  while (c != MEM_NIL)
    {
      parent = c;
      c = start < c->start ? c->left : c->right;
    }
     
#endif /* GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS */

  /* Create a new node.  */
#ifdef GC_MALLOC_CHECK
  x = (struct mem_node *) _malloc_internal (sizeof *x);
  if (x == NULL)
    abort ();
#else
  x = (struct mem_node *) xmalloc (sizeof *x);
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
mem_insert_fixup (x)
     struct mem_node *x;
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
mem_rotate_left (x)
     struct mem_node *x;
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
mem_rotate_right (x)
     struct mem_node *x;
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
mem_delete (z)
     struct mem_node *z;
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
  _free_internal (y);
#else
  xfree (y);
#endif
}


/* Re-establish the red-black properties of the tree, after a
   deletion.  */

static void
mem_delete_fixup (x)
     struct mem_node *x;
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


/* Value is non-zero if P is a pointer to a live Lisp string on
   the heap.  M is a pointer to the mem_block for P.  */

static INLINE int
live_string_p (m, p)
     struct mem_node *m;
     void *p;
{
  if (m->type == MEM_TYPE_STRING)
    {
      struct string_block *b = (struct string_block *) m->start;
      int offset = (char *) p - (char *) &b->strings[0];

      /* P must point to the start of a Lisp_String structure, and it
	 must not be on the free-list.  */
      return (offset >= 0
	      && offset % sizeof b->strings[0] == 0
	      && ((struct Lisp_String *) p)->data != NULL);
    }
  else
    return 0;
}


/* Value is non-zero if P is a pointer to a live Lisp cons on
   the heap.  M is a pointer to the mem_block for P.  */

static INLINE int
live_cons_p (m, p)
     struct mem_node *m;
     void *p;
{
  if (m->type == MEM_TYPE_CONS)
    {
      struct cons_block *b = (struct cons_block *) m->start;
      int offset = (char *) p - (char *) &b->conses[0];

      /* P must point to the start of a Lisp_Cons, not be
	 one of the unused cells in the current cons block,
	 and not be on the free-list.  */
      return (offset >= 0
	      && offset % sizeof b->conses[0] == 0
	      && (b != cons_block
		  || offset / sizeof b->conses[0] < cons_block_index)
	      && !EQ (((struct Lisp_Cons *) p)->car, Vdead));
    }
  else
    return 0;
}


/* Value is non-zero if P is a pointer to a live Lisp symbol on
   the heap.  M is a pointer to the mem_block for P.  */

static INLINE int
live_symbol_p (m, p)
     struct mem_node *m;
     void *p;
{
  if (m->type == MEM_TYPE_SYMBOL)
    {
      struct symbol_block *b = (struct symbol_block *) m->start;
      int offset = (char *) p - (char *) &b->symbols[0];
      
      /* P must point to the start of a Lisp_Symbol, not be
	 one of the unused cells in the current symbol block,
	 and not be on the free-list.  */
      return (offset >= 0
	      && offset % sizeof b->symbols[0] == 0
	      && (b != symbol_block
		  || offset / sizeof b->symbols[0] < symbol_block_index)
	      && !EQ (((struct Lisp_Symbol *) p)->function, Vdead));
    }
  else
    return 0;
}


/* Value is non-zero if P is a pointer to a live Lisp float on
   the heap.  M is a pointer to the mem_block for P.  */

static INLINE int
live_float_p (m, p)
     struct mem_node *m;
     void *p;
{
  if (m->type == MEM_TYPE_FLOAT)
    {
      struct float_block *b = (struct float_block *) m->start;
      int offset = (char *) p - (char *) &b->floats[0];
      
      /* P must point to the start of a Lisp_Float, not be
	 one of the unused cells in the current float block,
	 and not be on the free-list.  */
      return (offset >= 0
	      && offset % sizeof b->floats[0] == 0
	      && (b != float_block
		  || offset / sizeof b->floats[0] < float_block_index)
	      && !EQ (((struct Lisp_Float *) p)->type, Vdead));
    }
  else
    return 0;
}


/* Value is non-zero if P is a pointer to a live Lisp Misc on
   the heap.  M is a pointer to the mem_block for P.  */

static INLINE int
live_misc_p (m, p)
     struct mem_node *m;
     void *p;
{
  if (m->type == MEM_TYPE_MISC)
    {
      struct marker_block *b = (struct marker_block *) m->start;
      int offset = (char *) p - (char *) &b->markers[0];
      
      /* P must point to the start of a Lisp_Misc, not be
	 one of the unused cells in the current misc block,
	 and not be on the free-list.  */
      return (offset >= 0
	      && offset % sizeof b->markers[0] == 0
	      && (b != marker_block
		  || offset / sizeof b->markers[0] < marker_block_index)
	      && ((union Lisp_Misc *) p)->u_marker.type != Lisp_Misc_Free);
    }
  else
    return 0;
}


/* Value is non-zero if P is a pointer to a live vector-like object.
   M is a pointer to the mem_block for P.  */

static INLINE int
live_vector_p (m, p)
     struct mem_node *m;
     void *p;
{
  return (p == m->start
	  && m->type >= MEM_TYPE_VECTOR
	  && m->type <= MEM_TYPE_WINDOW);
}


/* Value is non-zero of P is a pointer to a live buffer.  M is a
   pointer to the mem_block for P.  */

static INLINE int
live_buffer_p (m, p)
     struct mem_node *m;
     void *p;
{
  /* P must point to the start of the block, and the buffer
     must not have been killed.  */
  return (m->type == MEM_TYPE_BUFFER
	  && p == m->start
	  && !NILP (((struct buffer *) p)->name));
}

#endif /* GC_MARK_STACK || defined GC_MALLOC_CHECK */

#if GC_MARK_STACK

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES

/* Array of objects that are kept alive because the C stack contains
   a pattern that looks like a reference to them .  */

#define MAX_ZOMBIES 10
static Lisp_Object zombies[MAX_ZOMBIES];

/* Number of zombie objects.  */

static int nzombies;

/* Number of garbage collections.  */

static int ngcs;

/* Average percentage of zombies per collection.  */

static double avg_zombies;

/* Max. number of live and zombie objects.  */

static int max_live, max_zombies;

/* Average number of live objects per GC.  */

static double avg_live;

DEFUN ("gc-status", Fgc_status, Sgc_status, 0, 0, "",
       doc: /* Show information about live and zombie objects.  */)
     ()
{
  Lisp_Object args[7];
  args[0] = build_string ("%d GCs, avg live/zombies = %.2f/%.2f (%f%%), max %d/%d");
  args[1] = make_number (ngcs);
  args[2] = make_float (avg_live);
  args[3] = make_float (avg_zombies);
  args[4] = make_float (avg_zombies / avg_live / 100);
  args[5] = make_number (max_live);
  args[6] = make_number (max_zombies);
  return Fmessage (7, args);
}

#endif /* GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES */


/* Mark OBJ if we can prove it's a Lisp_Object.  */

static INLINE void
mark_maybe_object (obj)
     Lisp_Object obj;
{
  void *po = (void *) XPNTR (obj);
  struct mem_node *m = mem_find (po);
      
  if (m != MEM_NIL)
    {
      int mark_p = 0;

      switch (XGCTYPE (obj))
	{
	case Lisp_String:
	  mark_p = (live_string_p (m, po)
		    && !STRING_MARKED_P ((struct Lisp_String *) po));
	  break;

	case Lisp_Cons:
	  mark_p = (live_cons_p (m, po)
		    && !XMARKBIT (XCONS (obj)->car));
	  break;

	case Lisp_Symbol:
	  mark_p = (live_symbol_p (m, po)
		    && !XMARKBIT (XSYMBOL (obj)->plist));
	  break;

	case Lisp_Float:
	  mark_p = (live_float_p (m, po)
		    && !XMARKBIT (XFLOAT (obj)->type));
	  break;

	case Lisp_Vectorlike:
	  /* Note: can't check GC_BUFFERP before we know it's a
	     buffer because checking that dereferences the pointer
	     PO which might point anywhere.  */
	  if (live_vector_p (m, po))
	    mark_p = (!GC_SUBRP (obj)
		      && !(XVECTOR (obj)->size & ARRAY_MARK_FLAG));
	  else if (live_buffer_p (m, po))
	    mark_p = GC_BUFFERP (obj) && !XMARKBIT (XBUFFER (obj)->name);
	  break;

	case Lisp_Misc:
	  if (live_misc_p (m, po))
	    {
	      switch (XMISCTYPE (obj))
		{
		case Lisp_Misc_Marker:
		  mark_p = !XMARKBIT (XMARKER (obj)->chain);
		  break;
		      
		case Lisp_Misc_Buffer_Local_Value:
		case Lisp_Misc_Some_Buffer_Local_Value:
		  mark_p = !XMARKBIT (XBUFFER_LOCAL_VALUE (obj)->realvalue);
		  break;
		      
		case Lisp_Misc_Overlay:
		  mark_p = !XMARKBIT (XOVERLAY (obj)->plist);
		  break;
		}
	    }
	  break;

	case Lisp_Int:
	case Lisp_Type_Limit:
	  break;
	}

      if (mark_p)
	{
#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES
	  if (nzombies < MAX_ZOMBIES)
	    zombies[nzombies] = *p;
	  ++nzombies;
#endif
	  mark_object (&obj);
	}
    }
}


/* If P points to Lisp data, mark that as live if it isn't already
   marked.  */

static INLINE void
mark_maybe_pointer (p)
     void *p;
{
  struct mem_node *m;

  /* Quickly rule out some values which can't point to Lisp data.  We
     assume that Lisp data is aligned on even addresses.  */
  if ((EMACS_INT) p & 1)
    return;
      
  m = mem_find (p);
  if (m != MEM_NIL)
    {
      Lisp_Object obj = Qnil;
      
      switch (m->type)
	{
	case MEM_TYPE_NON_LISP:
	  /* Nothing to do; not a pointer to Lisp memory.  */
	  break;
	  
	case MEM_TYPE_BUFFER:
	  if (live_buffer_p (m, p)
	      && !XMARKBIT (((struct buffer *) p)->name))
	    XSETVECTOR (obj, p);
	  break;
	  
	case MEM_TYPE_CONS:
	  if (live_cons_p (m, p)
	      && !XMARKBIT (((struct Lisp_Cons *) p)->car))
	    XSETCONS (obj, p);
	  break;
	  
	case MEM_TYPE_STRING:
	  if (live_string_p (m, p)
	      && !STRING_MARKED_P ((struct Lisp_String *) p))
	    XSETSTRING (obj, p);
	  break;

	case MEM_TYPE_MISC:
	  if (live_misc_p (m, p))
	    {
	      Lisp_Object tem;
	      XSETMISC (tem, p);
	      
	      switch (XMISCTYPE (tem))
		{
		case Lisp_Misc_Marker:
		  if (!XMARKBIT (XMARKER (tem)->chain))
		    obj = tem;
		  break;
		      
		case Lisp_Misc_Buffer_Local_Value:
		case Lisp_Misc_Some_Buffer_Local_Value:
		  if (!XMARKBIT (XBUFFER_LOCAL_VALUE (tem)->realvalue))
		    obj = tem;
		  break;
		      
		case Lisp_Misc_Overlay:
		  if (!XMARKBIT (XOVERLAY (tem)->plist))
		    obj = tem;
		  break;
		}
	    }
	  break;
	  
	case MEM_TYPE_SYMBOL:
	  if (live_symbol_p (m, p)
	      && !XMARKBIT (((struct Lisp_Symbol *) p)->plist))
	    XSETSYMBOL (obj, p);
	  break;
	  
	case MEM_TYPE_FLOAT:
	  if (live_float_p (m, p)
	      && !XMARKBIT (((struct Lisp_Float *) p)->type))
	    XSETFLOAT (obj, p);
	  break;
	  
	case MEM_TYPE_VECTOR:
	case MEM_TYPE_PROCESS:
	case MEM_TYPE_HASH_TABLE:
	case MEM_TYPE_FRAME:
	case MEM_TYPE_WINDOW:
	  if (live_vector_p (m, p))
	    {
	      Lisp_Object tem;
	      XSETVECTOR (tem, p);
	      if (!GC_SUBRP (tem)
		  && !(XVECTOR (tem)->size & ARRAY_MARK_FLAG))
		obj = tem;
	    }
	  break;

	default:
	  abort ();
	}

      if (!GC_NILP (obj))
	mark_object (&obj);
    }
}


/* Mark Lisp objects referenced from the address range START..END.  */

static void 
mark_memory (start, end)
     void *start, *end;
{
  Lisp_Object *p;
  void **pp;

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES
  nzombies = 0;
#endif

  /* Make START the pointer to the start of the memory region,
     if it isn't already.  */
  if (end < start)
    {
      void *tem = start;
      start = end;
      end = tem;
    }

  /* Mark Lisp_Objects.  */
  for (p = (Lisp_Object *) start; (void *) p < end; ++p)
    mark_maybe_object (*p);

  /* Mark Lisp data pointed to.  This is necessary because, in some
     situations, the C compiler optimizes Lisp objects away, so that
     only a pointer to them remains.  Example:

     DEFUN ("testme", Ftestme, Stestme, 0, 0, 0, "")
     ()
     {
       Lisp_Object obj = build_string ("test");
       struct Lisp_String *s = XSTRING (obj);
       Fgarbage_collect ();
       fprintf (stderr, "test `%s'\n", s->data);
       return Qnil;
     }

     Here, `obj' isn't really used, and the compiler optimizes it
     away.  The only reference to the life string is through the
     pointer `s'.  */
  
  for (pp = (void **) start; (void *) pp < end; ++pp)
    mark_maybe_pointer (*pp);
}


#if !defined GC_SAVE_REGISTERS_ON_STACK && !defined GC_SETJMP_WORKS

static int setjmp_tested_p, longjmps_done;

#define SETJMP_WILL_LIKELY_WORK "\
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

#define SETJMP_WILL_NOT_WORK "\
\n\
Emacs garbage collector has been changed to use conservative stack\n\
marking.  Emacs has determined that the default method it uses to do the\n\
marking will not work on your system.  We will need a system-dependent\n\
solution for your system.\n\
\n\
Please take a look at the function mark_stack in alloc.c, and\n\
try to find a way to make it work on your system.\n\
Please mail the result to <emacs-devel@gnu.org>.\n\
"


/* Perform a quick check if it looks like setjmp saves registers in a
   jmp_buf.  Print a message to stderr saying so.  When this test
   succeeds, this is _not_ a proof that setjmp is sufficient for
   conservative stack marking.  Only the sources or a disassembly
   can prove that.  */

static void
test_setjmp ()
{
  char buf[10];
  register int x;
  jmp_buf jbuf;
  int result = 0;

  /* Arrange for X to be put in a register.  */
  sprintf (buf, "1");
  x = strlen (buf);
  x = 2 * x - 1;

  setjmp (jbuf);
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
	fprintf (stderr, SETJMP_WILL_LIKELY_WORK);
      else
	{
	  fprintf (stderr, SETJMP_WILL_NOT_WORK);
	  exit (1);
	}
    }

  ++longjmps_done;
  x = 2;
  if (longjmps_done == 1)
    longjmp (jbuf, 1);
}

#endif /* not GC_SAVE_REGISTERS_ON_STACK && not GC_SETJMP_WORKS */


#if GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS

/* Abort if anything GCPRO'd doesn't survive the GC.  */

static void
check_gcpros ()
{
  struct gcpro *p;
  int i;

  for (p = gcprolist; p; p = p->next)
    for (i = 0; i < p->nvars; ++i)
      if (!survives_gc_p (p->var[i]))
	abort ();
}

#elif GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES

static void
dump_zombies ()
{
  int i;

  fprintf (stderr, "\nZombies kept alive = %d:\n", nzombies);
  for (i = 0; i < min (MAX_ZOMBIES, nzombies); ++i)
    {
      fprintf (stderr, "  %d = ", i);
      debug_print (zombies[i]);
    }
}

#endif /* GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES */


/* Mark live Lisp objects on the C stack.

   There are several system-dependent problems to consider when
   porting this to new architectures:

   Processor Registers

   We have to mark Lisp objects in CPU registers that can hold local
   variables or are used to pass parameters.

   If GC_SAVE_REGISTERS_ON_STACK is defined, it should expand to
   something that either saves relevant registers on the stack, or
   calls mark_maybe_object passing it each register's contents.

   If GC_SAVE_REGISTERS_ON_STACK is not defined, the current
   implementation assumes that calling setjmp saves registers we need
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
   from the stack start.

   The current code assumes by default that Lisp_Objects are aligned
   equally on the stack.  */

static void
mark_stack ()
{
  int i;
  jmp_buf j;
  volatile int stack_grows_down_p = (char *) &j > (char *) stack_base;
  void *end;

  /* This trick flushes the register windows so that all the state of
     the process is contained in the stack.  */
#ifdef sparc
  asm ("ta 3");
#endif
  
  /* Save registers that we need to see on the stack.  We need to see
     registers used to hold register variables and registers used to
     pass parameters.  */
#ifdef GC_SAVE_REGISTERS_ON_STACK
  GC_SAVE_REGISTERS_ON_STACK (end);
#else /* not GC_SAVE_REGISTERS_ON_STACK */
  
#ifndef GC_SETJMP_WORKS  /* If it hasn't been checked yet that
			    setjmp will definitely work, test it
			    and print a message with the result
			    of the test.  */
  if (!setjmp_tested_p)
    {
      setjmp_tested_p = 1;
      test_setjmp ();
    }
#endif /* GC_SETJMP_WORKS */
  
  setjmp (j);
  end = stack_grows_down_p ? (char *) &j + sizeof j : (char *) &j;
#endif /* not GC_SAVE_REGISTERS_ON_STACK */

  /* This assumes that the stack is a contiguous region in memory.  If
     that's not the case, something has to be done here to iterate
     over the stack segments.  */
#ifndef GC_LISP_OBJECT_ALIGNMENT
#define GC_LISP_OBJECT_ALIGNMENT sizeof (Lisp_Object)
#endif
  for (i = 0; i < sizeof (Lisp_Object); i += GC_LISP_OBJECT_ALIGNMENT)
    mark_memory ((char *) stack_base + i, end);

#if GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS
  check_gcpros ();
#endif
}


#endif /* GC_MARK_STACK != 0 */



/***********************************************************************
		       Pure Storage Management
 ***********************************************************************/

/* Allocate room for SIZE bytes from pure Lisp storage and return a
   pointer to it.  TYPE is the Lisp type for which the memory is
   allocated.  TYPE < 0 means it's not used for a Lisp object.

   If store_pure_type_info is set and TYPE is >= 0, the type of
   the allocated object is recorded in pure_types.  */

static POINTER_TYPE *
pure_alloc (size, type)
     size_t size;
     int type;
{
  size_t nbytes;
  POINTER_TYPE *result;
  char *beg = purebeg;

  /* Give Lisp_Floats an extra alignment.  */
  if (type == Lisp_Float)
    {
      size_t alignment;
#if defined __GNUC__ && __GNUC__ >= 2
      alignment = __alignof (struct Lisp_Float);
#else
      alignment = sizeof (struct Lisp_Float);
#endif
      pure_bytes_used = ALIGN (pure_bytes_used, alignment);
    }
    
  nbytes = ALIGN (size, sizeof (EMACS_INT));
  
  if (pure_bytes_used + nbytes > pure_size)
    {
      /* Don't allocate a large amount here,
	 because it might get mmap'd and then its address
	 might not be usable.  */
      beg = purebeg = (char *) xmalloc (10000);
      pure_size = 10000;
      pure_bytes_used_before_overflow += pure_bytes_used;
      pure_bytes_used = 0;
    }

  result = (POINTER_TYPE *) (beg + pure_bytes_used);
  pure_bytes_used += nbytes;
  return result;
}


/* Signal an error if PURESIZE is too small.  */

void
check_pure_size ()
{
  if (pure_bytes_used_before_overflow)
    error ("Pure Lisp storage overflow (approx. %d bytes needed)",
	   (int) (pure_bytes_used + pure_bytes_used_before_overflow));
}


/* Return a string allocated in pure space.  DATA is a buffer holding
   NCHARS characters, and NBYTES bytes of string data.  MULTIBYTE
   non-zero means make the result string multibyte.

   Must get an error if pure storage is full, since if it cannot hold
   a large string it may be able to hold conses that point to that
   string; then the string is not protected from gc.  */

Lisp_Object
make_pure_string (data, nchars, nbytes, multibyte)
     char *data;
     int nchars, nbytes;
     int multibyte;
{
  Lisp_Object string;
  struct Lisp_String *s;

  s = (struct Lisp_String *) pure_alloc (sizeof *s, Lisp_String);
  s->data = (unsigned char *) pure_alloc (nbytes + 1, -1);
  s->size = nchars;
  s->size_byte = multibyte ? nbytes : -1;
  bcopy (data, s->data, nbytes);
  s->data[nbytes] = '\0';
  s->intervals = NULL_INTERVAL;
  XSETSTRING (string, s);
  return string;
}


/* Return a cons allocated from pure space.  Give it pure copies
   of CAR as car and CDR as cdr.  */

Lisp_Object
pure_cons (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object new;
  struct Lisp_Cons *p;

  p = (struct Lisp_Cons *) pure_alloc (sizeof *p, Lisp_Cons);
  XSETCONS (new, p);
  XSETCAR (new, Fpurecopy (car));
  XSETCDR (new, Fpurecopy (cdr));
  return new;
}


/* Value is a float object with value NUM allocated from pure space.  */

Lisp_Object
make_pure_float (num)
     double num;
{
  register Lisp_Object new;
  struct Lisp_Float *p;

  p = (struct Lisp_Float *) pure_alloc (sizeof *p, Lisp_Float);
  XSETFLOAT (new, p);
  XFLOAT_DATA (new) = num;
  return new;
}


/* Return a vector with room for LEN Lisp_Objects allocated from
   pure space.  */

Lisp_Object
make_pure_vector (len)
     EMACS_INT len;
{
  Lisp_Object new;
  struct Lisp_Vector *p;
  size_t size = sizeof *p + (len - 1) * sizeof (Lisp_Object);

  p = (struct Lisp_Vector *) pure_alloc (size, Lisp_Vectorlike);
  XSETVECTOR (new, p);
  XVECTOR (new)->size = len;
  return new;
}


DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
       doc: /* Make a copy of OBJECT in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.  Copies strings without text properties.  */)
     (obj)
     register Lisp_Object obj;
{
  if (NILP (Vpurify_flag))
    return obj;

  if (PURE_POINTER_P (XPNTR (obj)))
    return obj;

  if (CONSP (obj))
    return pure_cons (XCAR (obj), XCDR (obj));
  else if (FLOATP (obj))
    return make_pure_float (XFLOAT_DATA (obj));
  else if (STRINGP (obj))
    return make_pure_string (XSTRING (obj)->data, XSTRING (obj)->size,
			     STRING_BYTES (XSTRING (obj)),
			     STRING_MULTIBYTE (obj));
  else if (COMPILEDP (obj) || VECTORP (obj))
    {
      register struct Lisp_Vector *vec;
      register int i, size;

      size = XVECTOR (obj)->size;
      if (size & PSEUDOVECTOR_FLAG)
	size &= PSEUDOVECTOR_SIZE_MASK;
      vec = XVECTOR (make_pure_vector ((EMACS_INT) size));
      for (i = 0; i < size; i++)
	vec->contents[i] = Fpurecopy (XVECTOR (obj)->contents[i]);
      if (COMPILEDP (obj))
	XSETCOMPILED (obj, vec);
      else
	XSETVECTOR (obj, vec);
      return obj;
    }
  else if (MARKERP (obj))
    error ("Attempt to copy a marker to pure storage");

  return obj;
}



/***********************************************************************
			  Protection from GC
 ***********************************************************************/

/* Put an entry in staticvec, pointing at the variable with address
   VARADDRESS.  */

void
staticpro (varaddress)
     Lisp_Object *varaddress;
{
  staticvec[staticidx++] = varaddress;
  if (staticidx >= NSTATICS)
    abort ();
}

struct catchtag
{
    Lisp_Object tag;
    Lisp_Object val;
    struct catchtag *next;
};

struct backtrace
{
  struct backtrace *next;
  Lisp_Object *function;
  Lisp_Object *args;	/* Points to vector of args.  */
  int nargs;		/* Length of vector.  */
  /* If nargs is UNEVALLED, args points to slot holding list of
     unevalled args.  */
  char evalargs;
};



/***********************************************************************
			  Protection from GC
 ***********************************************************************/

/* Temporarily prevent garbage collection.  */

int
inhibit_garbage_collection ()
{
  int count = specpdl_ptr - specpdl;
  int nbits = min (VALBITS, BITS_PER_INT);

  specbind (Qgc_cons_threshold, make_number (((EMACS_INT) 1 << (nbits - 1)) - 1));
  return count;
}


DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
       doc: /* Reclaim storage for Lisp objects no longer needed.
Returns info on amount of space in use:
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS
  (USED-FLOATS . FREE-FLOATS) (USED-INTERVALS . FREE-INTERVALS)
  (USED-STRINGS . FREE-STRINGS))
Garbage collection happens automatically if you cons more than
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.  */)
     ()
{
  register struct gcpro *tail;
  register struct specbinding *bind;
  struct catchtag *catch;
  struct handler *handler;
  register struct backtrace *backlist;
  char stack_top_variable;
  register int i;
  int message_p;
  Lisp_Object total[8];
  int count = BINDING_STACK_SIZE ();

  /* Can't GC if pure storage overflowed because we can't determine
     if something is a pure object or not.  */
  if (pure_bytes_used_before_overflow)
    return Qnil;

  /* In case user calls debug_print during GC,
     don't let that cause a recursive GC.  */
  consing_since_gc = 0;

  /* Save what's currently displayed in the echo area.  */
  message_p = push_message ();
  record_unwind_protect (push_message_unwind, Qnil);

  /* Save a copy of the contents of the stack, for debugging.  */
#if MAX_SAVE_STACK > 0
  if (NILP (Vpurify_flag))
    {
      i = &stack_top_variable - stack_bottom;
      if (i < 0) i = -i;
      if (i < MAX_SAVE_STACK)
	{
	  if (stack_copy == 0)
	    stack_copy = (char *) xmalloc (stack_copy_size = i);
	  else if (stack_copy_size < i)
	    stack_copy = (char *) xrealloc (stack_copy, (stack_copy_size = i));
	  if (stack_copy)
	    {
	      if ((EMACS_INT) (&stack_top_variable - stack_bottom) > 0)
		bcopy (stack_bottom, stack_copy, i);
	      else
		bcopy (&stack_top_variable, stack_copy, i);
	    }
	}
    }
#endif /* MAX_SAVE_STACK > 0 */

  if (garbage_collection_messages)
    message1_nolog ("Garbage collecting...");

  BLOCK_INPUT;

  shrink_regexp_cache ();

  /* Don't keep undo information around forever.  */
  {
    register struct buffer *nextb = all_buffers;

    while (nextb)
      {
	/* If a buffer's undo list is Qt, that means that undo is
	   turned off in that buffer.  Calling truncate_undo_list on
	   Qt tends to return NULL, which effectively turns undo back on.
	   So don't call truncate_undo_list if undo_list is Qt.  */
	if (! EQ (nextb->undo_list, Qt))
	  nextb->undo_list 
	    = truncate_undo_list (nextb->undo_list, undo_limit,
				  undo_strong_limit);

	/* Shrink buffer gaps, but skip indirect and dead buffers.  */
	if (nextb->base_buffer == 0 && !NILP (nextb->name))
	  {
	    /* If a buffer's gap size is more than 10% of the buffer
	       size, or larger than 2000 bytes, then shrink it
	       accordingly.  Keep a minimum size of 20 bytes.  */
	    int size = min (2000, max (20, (nextb->text->z_byte / 10)));

	    if (nextb->text->gap_size > size)
	      {
		struct buffer *save_current = current_buffer;
		current_buffer = nextb;
		make_gap (-(nextb->text->gap_size - size));
		current_buffer = save_current;
	      }
	  }

	nextb = nextb->next;
      }
  }

  gc_in_progress = 1;

  /* clear_marks (); */

  /* Mark all the special slots that serve as the roots of accessibility.

     Usually the special slots to mark are contained in particular structures.
     Then we know no slot is marked twice because the structures don't overlap.
     In some cases, the structures point to the slots to be marked.
     For these, we use MARKBIT to avoid double marking of the slot.  */

  for (i = 0; i < staticidx; i++)
    mark_object (staticvec[i]);

#if (GC_MARK_STACK == GC_MAKE_GCPROS_NOOPS \
     || GC_MARK_STACK == GC_MARK_STACK_CHECK_GCPROS)
  mark_stack ();
#else
  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      if (!XMARKBIT (tail->var[i]))
	{
	  /* Explicit casting prevents compiler warning about
	     discarding the `volatile' qualifier.  */
	  mark_object ((Lisp_Object *)&tail->var[i]);
	  XMARK (tail->var[i]);
	}
#endif
  
  mark_byte_stack ();
  for (bind = specpdl; bind != specpdl_ptr; bind++)
    {
      mark_object (&bind->symbol);
      mark_object (&bind->old_value);
    }
  for (catch = catchlist; catch; catch = catch->next)
    {
      mark_object (&catch->tag);
      mark_object (&catch->val);
    }  
  for (handler = handlerlist; handler; handler = handler->next)
    {
      mark_object (&handler->handler);
      mark_object (&handler->var);
    }  
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      if (!XMARKBIT (*backlist->function))
	{
	  mark_object (backlist->function);
	  XMARK (*backlist->function);
	}
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	i = 0;
      else
	i = backlist->nargs - 1;
      for (; i >= 0; i--)
	if (!XMARKBIT (backlist->args[i]))
	  {
	    mark_object (&backlist->args[i]);
	    XMARK (backlist->args[i]);
	  }
    }  
  mark_kboards ();

  /* Look thru every buffer's undo list
     for elements that update markers that were not marked,
     and delete them.  */
  {
    register struct buffer *nextb = all_buffers;

    while (nextb)
      {
	/* If a buffer's undo list is Qt, that means that undo is
	   turned off in that buffer.  Calling truncate_undo_list on
	   Qt tends to return NULL, which effectively turns undo back on.
	   So don't call truncate_undo_list if undo_list is Qt.  */
	if (! EQ (nextb->undo_list, Qt))
	  {
	    Lisp_Object tail, prev;
	    tail = nextb->undo_list;
	    prev = Qnil;
	    while (CONSP (tail))
	      {
		if (GC_CONSP (XCAR (tail))
		    && GC_MARKERP (XCAR (XCAR (tail)))
		    && ! XMARKBIT (XMARKER (XCAR (XCAR (tail)))->chain))
		  {
		    if (NILP (prev))
		      nextb->undo_list = tail = XCDR (tail);
		    else
		      {
			tail = XCDR (tail);
			XSETCDR (prev, tail);
		      }
		  }
		else
		  {
		    prev = tail;
		    tail = XCDR (tail);
		  }
	      }
	  }

	nextb = nextb->next;
      }
  }

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES
  mark_stack ();
#endif

  gc_sweep ();

  /* Clear the mark bits that we set in certain root slots.  */

#if (GC_MARK_STACK == GC_USE_GCPROS_AS_BEFORE \
     || GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES)
  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      XUNMARK (tail->var[i]);
#endif
  
  unmark_byte_stack ();
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      XUNMARK (*backlist->function);
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	i = 0;
      else
	i = backlist->nargs - 1;
      for (; i >= 0; i--)
	XUNMARK (backlist->args[i]);
    }  
  XUNMARK (buffer_defaults.name);
  XUNMARK (buffer_local_symbols.name);

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES && 0
  dump_zombies ();
#endif

  UNBLOCK_INPUT;

  /* clear_marks (); */
  gc_in_progress = 0;

  consing_since_gc = 0;
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;

  if (garbage_collection_messages)
    {
      if (message_p || minibuf_level > 0)
	restore_message ();
      else
	message1_nolog ("Garbage collecting...done");
    }

  unbind_to (count, Qnil);

  total[0] = Fcons (make_number (total_conses),
		    make_number (total_free_conses));
  total[1] = Fcons (make_number (total_symbols),
		    make_number (total_free_symbols));
  total[2] = Fcons (make_number (total_markers),
		    make_number (total_free_markers));
  total[3] = make_number (total_string_size);
  total[4] = make_number (total_vector_size);
  total[5] = Fcons (make_number (total_floats),
		    make_number (total_free_floats));
  total[6] = Fcons (make_number (total_intervals),
		    make_number (total_free_intervals));
  total[7] = Fcons (make_number (total_strings),
		    make_number (total_free_strings));

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES
  {
    /* Compute average percentage of zombies.  */
    double nlive = 0;
      
    for (i = 0; i < 7; ++i)
      nlive += XFASTINT (XCAR (total[i]));

    avg_live = (avg_live * ngcs + nlive) / (ngcs + 1);
    max_live = max (nlive, max_live);
    avg_zombies = (avg_zombies * ngcs + nzombies) / (ngcs + 1);
    max_zombies = max (nzombies, max_zombies);
    ++ngcs;
    }
#endif

  if (!NILP (Vpost_gc_hook))
    {
      int count = inhibit_garbage_collection ();
      safe_run_hooks (Qpost_gc_hook);
      unbind_to (count, Qnil);
    }
  
  return Flist (sizeof total / sizeof *total, total);
}


/* Mark Lisp objects in glyph matrix MATRIX.  Currently the
   only interesting objects referenced from glyphs are strings.  */

static void
mark_glyph_matrix (matrix)
     struct glyph_matrix *matrix;
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
	      if (GC_STRINGP (glyph->object)
		  && !STRING_MARKED_P (XSTRING (glyph->object)))
		mark_object (&glyph->object);
	  }
      }
}


/* Mark Lisp faces in the face cache C.  */

static void
mark_face_cache (c)
     struct face_cache *c;
{
  if (c)
    {
      int i, j;
      for (i = 0; i < c->used; ++i)
	{
	  struct face *face = FACE_FROM_ID (c->f, i);

	  if (face)
	    {
	      for (j = 0; j < LFACE_VECTOR_SIZE; ++j)
		mark_object (&face->lface[j]);
	    }
	}
    }
}


#ifdef HAVE_WINDOW_SYSTEM

/* Mark Lisp objects in image IMG.  */

static void
mark_image (img)
     struct image *img;
{
  mark_object (&img->spec);
  
  if (!NILP (img->data.lisp_val))
    mark_object (&img->data.lisp_val);
}


/* Mark Lisp objects in image cache of frame F.  It's done this way so
   that we don't have to include xterm.h here.  */

static void
mark_image_cache (f)
     struct frame *f;
{
  forall_images_in_image_cache (f, mark_image);
}

#endif /* HAVE_X_WINDOWS */



/* Mark reference to a Lisp_Object.
   If the object referred to has not been seen yet, recursively mark
   all the references contained in it.  */

#define LAST_MARKED_SIZE 500
Lisp_Object *last_marked[LAST_MARKED_SIZE];
int last_marked_index;

void
mark_object (argptr)
     Lisp_Object *argptr;
{
  Lisp_Object *objptr = argptr;
  register Lisp_Object obj;
#ifdef GC_CHECK_MARKED_OBJECTS
  void *po;
  struct mem_node *m;
#endif

 loop:
  obj = *objptr;
 loop2:
  XUNMARK (obj);

  if (PURE_POINTER_P (XPNTR (obj)))
    return;

  last_marked[last_marked_index++] = objptr;
  if (last_marked_index == LAST_MARKED_SIZE)
    last_marked_index = 0;

  /* Perform some sanity checks on the objects marked here.  Abort if
     we encounter an object we know is bogus.  This increases GC time
     by ~80%, and requires compilation with GC_MARK_STACK != 0.  */
#ifdef GC_CHECK_MARKED_OBJECTS

  po = (void *) XPNTR (obj);

  /* Check that the object pointed to by PO is known to be a Lisp
     structure allocated from the heap.  */
#define CHECK_ALLOCATED()			\
  do {						\
    m = mem_find (po);				\
    if (m == MEM_NIL)				\
      abort ();					\
  } while (0)

  /* Check that the object pointed to by PO is live, using predicate
     function LIVEP.  */
#define CHECK_LIVE(LIVEP)			\
  do {						\
    if (!LIVEP (m, po))				\
      abort ();					\
  } while (0)

  /* Check both of the above conditions.  */
#define CHECK_ALLOCATED_AND_LIVE(LIVEP)		\
  do {						\
    CHECK_ALLOCATED ();				\
    CHECK_LIVE (LIVEP);				\
  } while (0)					\
  
#else /* not GC_CHECK_MARKED_OBJECTS */
  
#define CHECK_ALLOCATED()		(void) 0
#define CHECK_LIVE(LIVEP)		(void) 0
#define CHECK_ALLOCATED_AND_LIVE(LIVEP)	(void) 0
  
#endif /* not GC_CHECK_MARKED_OBJECTS */

  switch (SWITCH_ENUM_CAST (XGCTYPE (obj)))
    {
    case Lisp_String:
      {
	register struct Lisp_String *ptr = XSTRING (obj);
	CHECK_ALLOCATED_AND_LIVE (live_string_p);
	MARK_INTERVAL_TREE (ptr->intervals);
	MARK_STRING (ptr);
#ifdef GC_CHECK_STRING_BYTES
	/* Check that the string size recorded in the string is the
	   same as the one recorded in the sdata structure. */
	CHECK_STRING_BYTES (ptr);
#endif /* GC_CHECK_STRING_BYTES */
      }
      break;

    case Lisp_Vectorlike:
#ifdef GC_CHECK_MARKED_OBJECTS
      m = mem_find (po);
      if (m == MEM_NIL && !GC_SUBRP (obj)
	  && po != &buffer_defaults
	  && po != &buffer_local_symbols)
	abort ();
#endif /* GC_CHECK_MARKED_OBJECTS */
      
      if (GC_BUFFERP (obj))
	{
	  if (!XMARKBIT (XBUFFER (obj)->name))
	    {
#ifdef GC_CHECK_MARKED_OBJECTS
	      if (po != &buffer_defaults && po != &buffer_local_symbols)
		{
		  struct buffer *b;
		  for (b = all_buffers; b && b != po; b = b->next)
		    ;
		  if (b == NULL)
		    abort ();
		}
#endif /* GC_CHECK_MARKED_OBJECTS */
	      mark_buffer (obj);
	    }
	}
      else if (GC_SUBRP (obj))
	break;
      else if (GC_COMPILEDP (obj))
	/* We could treat this just like a vector, but it is better to
	   save the COMPILED_CONSTANTS element for last and avoid
	   recursion there.  */
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  register EMACS_INT size = ptr->size;
	  register int i;

	  if (size & ARRAY_MARK_FLAG)
	    break;   /* Already marked */
	  
	  CHECK_LIVE (live_vector_p);
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	  size &= PSEUDOVECTOR_SIZE_MASK;
	  for (i = 0; i < size; i++) /* and then mark its elements */
	    {
	      if (i != COMPILED_CONSTANTS)
		mark_object (&ptr->contents[i]);
	    }
	  /* This cast should be unnecessary, but some Mips compiler complains
	     (MIPS-ABI + SysVR4, DC/OSx, etc).  */
	  objptr = (Lisp_Object *) &ptr->contents[COMPILED_CONSTANTS];
	  goto loop;
	}
      else if (GC_FRAMEP (obj))
	{
	  register struct frame *ptr = XFRAME (obj);
	  register EMACS_INT size = ptr->size;

	  if (size & ARRAY_MARK_FLAG) break;   /* Already marked */
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */

	  CHECK_LIVE (live_vector_p);
	  mark_object (&ptr->name);
	  mark_object (&ptr->icon_name);
	  mark_object (&ptr->title);
	  mark_object (&ptr->focus_frame);
	  mark_object (&ptr->selected_window);
	  mark_object (&ptr->minibuffer_window);
	  mark_object (&ptr->param_alist);
	  mark_object (&ptr->scroll_bars);
	  mark_object (&ptr->condemned_scroll_bars);
	  mark_object (&ptr->menu_bar_items);
	  mark_object (&ptr->face_alist);
	  mark_object (&ptr->menu_bar_vector);
	  mark_object (&ptr->buffer_predicate);
	  mark_object (&ptr->buffer_list);
	  mark_object (&ptr->menu_bar_window);
	  mark_object (&ptr->tool_bar_window);
	  mark_face_cache (ptr->face_cache);
#ifdef HAVE_WINDOW_SYSTEM
	  mark_image_cache (ptr);
	  mark_object (&ptr->tool_bar_items);
	  mark_object (&ptr->desired_tool_bar_string);
	  mark_object (&ptr->current_tool_bar_string);
#endif /* HAVE_WINDOW_SYSTEM */
	}
      else if (GC_BOOL_VECTOR_P (obj))
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);

	  if (ptr->size & ARRAY_MARK_FLAG)
	    break;   /* Already marked */
	  CHECK_LIVE (live_vector_p);
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	}
      else if (GC_WINDOWP (obj))
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  struct window *w = XWINDOW (obj);
	  register EMACS_INT size = ptr->size;
	  register int i;

	  /* Stop if already marked.  */
	  if (size & ARRAY_MARK_FLAG)
	    break;

	  /* Mark it.  */
	  CHECK_LIVE (live_vector_p);
	  ptr->size |= ARRAY_MARK_FLAG;

	  /* There is no Lisp data above The member CURRENT_MATRIX in
	     struct WINDOW.  Stop marking when that slot is reached.  */
	  for (i = 0;
	       (char *) &ptr->contents[i] < (char *) &w->current_matrix;
	       i++)
	    mark_object (&ptr->contents[i]);

	  /* Mark glyphs for leaf windows.  Marking window matrices is
	     sufficient because frame matrices use the same glyph
	     memory.  */
	  if (NILP (w->hchild)
	      && NILP (w->vchild)
	      && w->current_matrix)
	    {
	      mark_glyph_matrix (w->current_matrix);
	      mark_glyph_matrix (w->desired_matrix);
	    }
	}
      else if (GC_HASH_TABLE_P (obj))
	{
	  struct Lisp_Hash_Table *h = XHASH_TABLE (obj);
	  EMACS_INT size = h->size;
	  
	  /* Stop if already marked.  */
	  if (size & ARRAY_MARK_FLAG)
	    break;
	  
	  /* Mark it.  */
	  CHECK_LIVE (live_vector_p);
	  h->size |= ARRAY_MARK_FLAG;

	  /* Mark contents.  */
	  /* Do not mark next_free or next_weak.
	     Being in the next_weak chain 
	     should not keep the hash table alive.
	     No need to mark `count' since it is an integer.  */
	  mark_object (&h->test);
	  mark_object (&h->weak);
	  mark_object (&h->rehash_size);
	  mark_object (&h->rehash_threshold);
	  mark_object (&h->hash);
	  mark_object (&h->next);
	  mark_object (&h->index);
	  mark_object (&h->user_hash_function);
	  mark_object (&h->user_cmp_function);

	  /* If hash table is not weak, mark all keys and values.
	     For weak tables, mark only the vector.  */
	  if (GC_NILP (h->weak))
	    mark_object (&h->key_and_value);
	  else
	    XVECTOR (h->key_and_value)->size |= ARRAY_MARK_FLAG;
	    
	}
      else
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  register EMACS_INT size = ptr->size;
	  register int i;

	  if (size & ARRAY_MARK_FLAG) break; /* Already marked */
	  CHECK_LIVE (live_vector_p);
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	  if (size & PSEUDOVECTOR_FLAG)
	    size &= PSEUDOVECTOR_SIZE_MASK;

	  for (i = 0; i < size; i++) /* and then mark its elements */
	    mark_object (&ptr->contents[i]);
	}
      break;

    case Lisp_Symbol:
      {
	register struct Lisp_Symbol *ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (XMARKBIT (ptr->plist)) break;
	CHECK_ALLOCATED_AND_LIVE (live_symbol_p);
	XMARK (ptr->plist);
	mark_object ((Lisp_Object *) &ptr->value);
	mark_object (&ptr->function);
	mark_object (&ptr->plist);

	if (!PURE_POINTER_P (ptr->name))
	  MARK_STRING (ptr->name);
	MARK_INTERVAL_TREE (ptr->name->intervals);
	
	/* Note that we do not mark the obarray of the symbol.
	   It is safe not to do so because nothing accesses that
	   slot except to check whether it is nil.  */
	ptr = ptr->next;
	if (ptr)
	  {
	    /* For the benefit of the last_marked log.  */
	    objptr = (Lisp_Object *)&XSYMBOL (obj)->next;
	    ptrx = ptr;		/* Use of ptrx avoids compiler bug on Sun */
	    XSETSYMBOL (obj, ptrx);
	    /* We can't goto loop here because *objptr doesn't contain an
	       actual Lisp_Object with valid datatype field.  */
	    goto loop2;
	  }
      }
      break;

    case Lisp_Misc:
      CHECK_ALLOCATED_AND_LIVE (live_misc_p);
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  XMARK (XMARKER (obj)->chain);
	  /* DO NOT mark thru the marker's chain.
	     The buffer's markers chain does not preserve markers from gc;
	     instead, markers are removed from the chain when freed by gc.  */
	  break;

	case Lisp_Misc_Buffer_Local_Value:
	case Lisp_Misc_Some_Buffer_Local_Value:
	  {
	    register struct Lisp_Buffer_Local_Value *ptr
	      = XBUFFER_LOCAL_VALUE (obj);
	    if (XMARKBIT (ptr->realvalue)) break;
	    XMARK (ptr->realvalue);
	    /* If the cdr is nil, avoid recursion for the car.  */
	    if (EQ (ptr->cdr, Qnil))
	      {
		objptr = &ptr->realvalue;
		goto loop;
	      }
	    mark_object (&ptr->realvalue);
	    mark_object (&ptr->buffer);
	    mark_object (&ptr->frame);
	    objptr = &ptr->cdr;
	    goto loop;
	  }

	case Lisp_Misc_Intfwd:
	case Lisp_Misc_Boolfwd:
	case Lisp_Misc_Objfwd:
	case Lisp_Misc_Buffer_Objfwd:
	case Lisp_Misc_Kboard_Objfwd:
	  /* Don't bother with Lisp_Buffer_Objfwd,
	     since all markable slots in current buffer marked anyway.  */
	  /* Don't need to do Lisp_Objfwd, since the places they point
	     are protected with staticpro.  */
	  break;

	case Lisp_Misc_Overlay:
	  {
	    struct Lisp_Overlay *ptr = XOVERLAY (obj);
	    if (!XMARKBIT (ptr->plist))
	      {
		XMARK (ptr->plist);
		mark_object (&ptr->start);
		mark_object (&ptr->end);
		objptr = &ptr->plist;
		goto loop;
	      }
	  }
	  break;

	default:
	  abort ();
	}
      break;

    case Lisp_Cons:
      {
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (XMARKBIT (ptr->car)) break;
	CHECK_ALLOCATED_AND_LIVE (live_cons_p);
	XMARK (ptr->car);
	/* If the cdr is nil, avoid recursion for the car.  */
	if (EQ (ptr->cdr, Qnil))
	  {
	    objptr = &ptr->car;
	    goto loop;
	  }
	mark_object (&ptr->car);
	objptr = &ptr->cdr;
	goto loop;
      }

    case Lisp_Float:
      CHECK_ALLOCATED_AND_LIVE (live_float_p);
      XMARK (XFLOAT (obj)->type);
      break;

    case Lisp_Int:
      break;

    default:
      abort ();
    }

#undef CHECK_LIVE
#undef CHECK_ALLOCATED
#undef CHECK_ALLOCATED_AND_LIVE
}

/* Mark the pointers in a buffer structure.  */

static void
mark_buffer (buf)
     Lisp_Object buf;
{
  register struct buffer *buffer = XBUFFER (buf);
  register Lisp_Object *ptr;
  Lisp_Object base_buffer;

  /* This is the buffer's markbit */
  mark_object (&buffer->name);
  XMARK (buffer->name);

  MARK_INTERVAL_TREE (BUF_INTERVALS (buffer));

  if (CONSP (buffer->undo_list))
    {
      Lisp_Object tail;
      tail = buffer->undo_list;

      while (CONSP (tail))
	{
	  register struct Lisp_Cons *ptr = XCONS (tail);

	  if (XMARKBIT (ptr->car))
	    break;
	  XMARK (ptr->car);
	  if (GC_CONSP (ptr->car)
	      && ! XMARKBIT (XCAR (ptr->car))
	      && GC_MARKERP (XCAR (ptr->car)))
	    {
	      XMARK (XCAR_AS_LVALUE (ptr->car));
	      mark_object (&XCDR_AS_LVALUE (ptr->car));
	    }
	  else
	    mark_object (&ptr->car);

	  if (CONSP (ptr->cdr))
	    tail = ptr->cdr;
	  else
	    break;
	}

      mark_object (&XCDR_AS_LVALUE (tail));
    }
  else
    mark_object (&buffer->undo_list);

  for (ptr = &buffer->name + 1;
       (char *)ptr < (char *)buffer + sizeof (struct buffer);
       ptr++)
    mark_object (ptr);

  /* If this is an indirect buffer, mark its base buffer.  */
  if (buffer->base_buffer && !XMARKBIT (buffer->base_buffer->name))
    {
      XSETBUFFER (base_buffer, buffer->base_buffer); 
      mark_buffer (base_buffer);
    }
}


/* Mark the pointers in the kboard objects.  */

static void
mark_kboards ()
{
  KBOARD *kb;
  Lisp_Object *p;
  for (kb = all_kboards; kb; kb = kb->next_kboard)
    {
      if (kb->kbd_macro_buffer)
	for (p = kb->kbd_macro_buffer; p < kb->kbd_macro_ptr; p++)
	  mark_object (p);
      mark_object (&kb->Voverriding_terminal_local_map);
      mark_object (&kb->Vlast_command);
      mark_object (&kb->Vreal_last_command);
      mark_object (&kb->Vprefix_arg);
      mark_object (&kb->Vlast_prefix_arg);
      mark_object (&kb->kbd_queue);
      mark_object (&kb->defining_kbd_macro);
      mark_object (&kb->Vlast_kbd_macro);
      mark_object (&kb->Vsystem_key_alist);
      mark_object (&kb->system_key_syms);
      mark_object (&kb->Vdefault_minibuffer_frame);
      mark_object (&kb->echo_string);
    }
}


/* Value is non-zero if OBJ will survive the current GC because it's
   either marked or does not need to be marked to survive.  */

int
survives_gc_p (obj)
     Lisp_Object obj;
{
  int survives_p;
  
  switch (XGCTYPE (obj))
    {
    case Lisp_Int:
      survives_p = 1;
      break;

    case Lisp_Symbol:
      survives_p = XMARKBIT (XSYMBOL (obj)->plist);
      break;

    case Lisp_Misc:
      switch (XMISCTYPE (obj))
	{
	case Lisp_Misc_Marker:
	  survives_p = XMARKBIT (obj);
	  break;
	  
	case Lisp_Misc_Buffer_Local_Value:
	case Lisp_Misc_Some_Buffer_Local_Value:
	  survives_p = XMARKBIT (XBUFFER_LOCAL_VALUE (obj)->realvalue);
	  break;
	  
	case Lisp_Misc_Intfwd:
	case Lisp_Misc_Boolfwd:
	case Lisp_Misc_Objfwd:
	case Lisp_Misc_Buffer_Objfwd:
	case Lisp_Misc_Kboard_Objfwd:
	  survives_p = 1;
	  break;
	  
	case Lisp_Misc_Overlay:
	  survives_p = XMARKBIT (XOVERLAY (obj)->plist);
	  break;

	default:
	  abort ();
	}
      break;

    case Lisp_String:
      {
	struct Lisp_String *s = XSTRING (obj);
	survives_p = STRING_MARKED_P (s);
      }
      break;

    case Lisp_Vectorlike:
      if (GC_BUFFERP (obj))
	survives_p = XMARKBIT (XBUFFER (obj)->name);
      else if (GC_SUBRP (obj))
	survives_p = 1;
      else
	survives_p = XVECTOR (obj)->size & ARRAY_MARK_FLAG;
      break;

    case Lisp_Cons:
      survives_p = XMARKBIT (XCAR (obj));
      break;

    case Lisp_Float:
      survives_p = XMARKBIT (XFLOAT (obj)->type);
      break;

    default:
      abort ();
    }

  return survives_p || PURE_POINTER_P ((void *) XPNTR (obj));
}



/* Sweep: find all structures not marked, and free them. */

static void
gc_sweep ()
{
  /* Remove or mark entries in weak hash tables.
     This must be done before any object is unmarked.  */
  sweep_weak_hash_tables ();

  sweep_strings ();
#ifdef GC_CHECK_STRING_BYTES
  if (!noninteractive)
    check_string_bytes (1);
#endif

  /* Put all unmarked conses on free list */
  {
    register struct cons_block *cblk;
    struct cons_block **cprev = &cons_block;
    register int lim = cons_block_index;
    register int num_free = 0, num_used = 0;

    cons_free_list = 0;
  
    for (cblk = cons_block; cblk; cblk = *cprev)
      {
	register int i;
	int this_free = 0;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (cblk->conses[i].car))
	    {
	      this_free++;
	      *(struct Lisp_Cons **)&cblk->conses[i].cdr = cons_free_list;
	      cons_free_list = &cblk->conses[i];
#if GC_MARK_STACK
	      cons_free_list->car = Vdead;
#endif
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (cblk->conses[i].car);
	    }
	lim = CONS_BLOCK_SIZE;
	/* If this block contains only free conses and we have already
	   seen more than two blocks worth of free conses then deallocate
	   this block.  */
	if (this_free == CONS_BLOCK_SIZE && num_free > CONS_BLOCK_SIZE)
	  {
	    *cprev = cblk->next;
	    /* Unhook from the free list.  */
	    cons_free_list = *(struct Lisp_Cons **) &cblk->conses[0].cdr;
	    lisp_free (cblk);
	    n_cons_blocks--;
	  }
	else
	  {
	    num_free += this_free;
	    cprev = &cblk->next;
	  }
      }
    total_conses = num_used;
    total_free_conses = num_free;
  }

  /* Put all unmarked floats on free list */
  {
    register struct float_block *fblk;
    struct float_block **fprev = &float_block;
    register int lim = float_block_index;
    register int num_free = 0, num_used = 0;

    float_free_list = 0;
  
    for (fblk = float_block; fblk; fblk = *fprev)
      {
	register int i;
	int this_free = 0;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (fblk->floats[i].type))
	    {
	      this_free++;
	      *(struct Lisp_Float **)&fblk->floats[i].data = float_free_list;
	      float_free_list = &fblk->floats[i];
#if GC_MARK_STACK
	      float_free_list->type = Vdead;
#endif
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (fblk->floats[i].type);
	    }
	lim = FLOAT_BLOCK_SIZE;
	/* If this block contains only free floats and we have already
	   seen more than two blocks worth of free floats then deallocate
	   this block.  */
	if (this_free == FLOAT_BLOCK_SIZE && num_free > FLOAT_BLOCK_SIZE)
	  {
	    *fprev = fblk->next;
	    /* Unhook from the free list.  */
	    float_free_list = *(struct Lisp_Float **) &fblk->floats[0].data;
	    lisp_free (fblk);
	    n_float_blocks--;
	  }
	else
	  {
	    num_free += this_free;
	    fprev = &fblk->next;
	  }
      }
    total_floats = num_used;
    total_free_floats = num_free;
  }

  /* Put all unmarked intervals on free list */
  {
    register struct interval_block *iblk;
    struct interval_block **iprev = &interval_block;
    register int lim = interval_block_index;
    register int num_free = 0, num_used = 0;

    interval_free_list = 0;

    for (iblk = interval_block; iblk; iblk = *iprev)
      {
	register int i;
	int this_free = 0;

	for (i = 0; i < lim; i++)
	  {
	    if (! XMARKBIT (iblk->intervals[i].plist))
	      {
		SET_INTERVAL_PARENT (&iblk->intervals[i], interval_free_list);
		interval_free_list = &iblk->intervals[i];
		this_free++;
	      }
	    else
	      {
		num_used++;
		XUNMARK (iblk->intervals[i].plist);
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
	    n_interval_blocks--;
	  }
	else
	  {
	    num_free += this_free;
	    iprev = &iblk->next;
	  }
      }
    total_intervals = num_used;
    total_free_intervals = num_free;
  }

  /* Put all unmarked symbols on free list */
  {
    register struct symbol_block *sblk;
    struct symbol_block **sprev = &symbol_block;
    register int lim = symbol_block_index;
    register int num_free = 0, num_used = 0;

    symbol_free_list = NULL;
  
    for (sblk = symbol_block; sblk; sblk = *sprev)
      {
	int this_free = 0;
	struct Lisp_Symbol *sym = sblk->symbols;
	struct Lisp_Symbol *end = sym + lim;

	for (; sym < end; ++sym)
	  {
	    /* Check if the symbol was created during loadup.  In such a case
	       it might be pointed to by pure bytecode which we don't trace,
	       so we conservatively assume that it is live.  */
	    int pure_p = PURE_POINTER_P (sym->name);
	    
	    if (!XMARKBIT (sym->plist) && !pure_p)
	      {
		*(struct Lisp_Symbol **) &sym->value = symbol_free_list;
		symbol_free_list = sym;
#if GC_MARK_STACK
		symbol_free_list->function = Vdead;
#endif
		++this_free;
	      }
	    else
	      {
		++num_used;
		if (!pure_p)
		  UNMARK_STRING (sym->name);
		XUNMARK (sym->plist);
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
	    symbol_free_list = *(struct Lisp_Symbol **)&sblk->symbols[0].value;
	    lisp_free (sblk);
	    n_symbol_blocks--;
	  }
	else
	  {
	    num_free += this_free;
	    sprev = &sblk->next;
	  }
      }
    total_symbols = num_used;
    total_free_symbols = num_free;
  }

  /* Put all unmarked misc's on free list.
     For a marker, first unchain it from the buffer it points into.  */
  {
    register struct marker_block *mblk;
    struct marker_block **mprev = &marker_block;
    register int lim = marker_block_index;
    register int num_free = 0, num_used = 0;

    marker_free_list = 0;
  
    for (mblk = marker_block; mblk; mblk = *mprev)
      {
	register int i;
	int this_free = 0;
	EMACS_INT already_free = -1;

	for (i = 0; i < lim; i++)
	  {
	    Lisp_Object *markword;
	    switch (mblk->markers[i].u_marker.type)
	      {
	      case Lisp_Misc_Marker:
		markword = &mblk->markers[i].u_marker.chain;
		break;
	      case Lisp_Misc_Buffer_Local_Value:
	      case Lisp_Misc_Some_Buffer_Local_Value:
		markword = &mblk->markers[i].u_buffer_local_value.realvalue;
		break;
	      case Lisp_Misc_Overlay:
		markword = &mblk->markers[i].u_overlay.plist;
		break;
	      case Lisp_Misc_Free:
		/* If the object was already free, keep it
		   on the free list.  */
		markword = (Lisp_Object *) &already_free;
		break;
	      default:
		markword = 0;
		break;
	      }
	    if (markword && !XMARKBIT (*markword))
	      {
		Lisp_Object tem;
		if (mblk->markers[i].u_marker.type == Lisp_Misc_Marker)
		  {
		    /* tem1 avoids Sun compiler bug */
		    struct Lisp_Marker *tem1 = &mblk->markers[i].u_marker;
		    XSETMARKER (tem, tem1);
		    unchain_marker (tem);
		  }
		/* Set the type of the freed object to Lisp_Misc_Free.
		   We could leave the type alone, since nobody checks it,
		   but this might catch bugs faster.  */
		mblk->markers[i].u_marker.type = Lisp_Misc_Free;
		mblk->markers[i].u_free.chain = marker_free_list;
		marker_free_list = &mblk->markers[i];
		this_free++;
	      }
	    else
	      {
		num_used++;
		if (markword)
		  XUNMARK (*markword);
	      }
	  }
	lim = MARKER_BLOCK_SIZE;
	/* If this block contains only free markers and we have already
	   seen more than two blocks worth of free markers then deallocate
	   this block.  */
	if (this_free == MARKER_BLOCK_SIZE && num_free > MARKER_BLOCK_SIZE)
	  {
	    *mprev = mblk->next;
	    /* Unhook from the free list.  */
	    marker_free_list = mblk->markers[0].u_free.chain;
	    lisp_free (mblk);
	    n_marker_blocks--;
	  }
	else
	  {
	    num_free += this_free;
	    mprev = &mblk->next;
	  }
      }

    total_markers = num_used;
    total_free_markers = num_free;
  }

  /* Free all unmarked buffers */
  {
    register struct buffer *buffer = all_buffers, *prev = 0, *next;

    while (buffer)
      if (!XMARKBIT (buffer->name))
	{
	  if (prev)
	    prev->next = buffer->next;
	  else
	    all_buffers = buffer->next;
	  next = buffer->next;
	  lisp_free (buffer);
	  buffer = next;
	}
      else
	{
	  XUNMARK (buffer->name);
	  UNMARK_BALANCE_INTERVALS (BUF_INTERVALS (buffer));
	  prev = buffer, buffer = buffer->next;
	}
  }

  /* Free all unmarked vectors */
  {
    register struct Lisp_Vector *vector = all_vectors, *prev = 0, *next;
    total_vector_size = 0;

    while (vector)
      if (!(vector->size & ARRAY_MARK_FLAG))
	{
	  if (prev)
	    prev->next = vector->next;
	  else
	    all_vectors = vector->next;
	  next = vector->next;
	  lisp_free (vector);
	  n_vectors--;
	  vector = next;

	}
      else
	{
	  vector->size &= ~ARRAY_MARK_FLAG;
	  if (vector->size & PSEUDOVECTOR_FLAG)
	    total_vector_size += (PSEUDOVECTOR_SIZE_MASK & vector->size);
	  else
	    total_vector_size += vector->size;
	  prev = vector, vector = vector->next;
	}
  }
  
#ifdef GC_CHECK_STRING_BYTES
  if (!noninteractive)
    check_string_bytes (1);
#endif
}




/* Debugging aids.  */

DEFUN ("memory-limit", Fmemory_limit, Smemory_limit, 0, 0, 0,
       doc: /* Return the address of the last byte Emacs has allocated, divided by 1024.
This may be helpful in debugging Emacs's memory usage.
We divide the value by 1024 to make sure it fits in a Lisp integer.  */)
     ()
{
  Lisp_Object end;

  XSETINT (end, (EMACS_INT) sbrk (0) / 1024);

  return end;
}

DEFUN ("memory-use-counts", Fmemory_use_counts, Smemory_use_counts, 0, 0, 0,
       doc: /* Return a list of counters that measure how much consing there has been.
Each of these counters increments for a certain kind of object.
The counters wrap around from the largest positive integer to zero.
Garbage collection does not decrease them.
The elements of the value are as follows:
  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS MISCS INTERVALS STRINGS)
All are in units of 1 = one object consed
except for VECTOR-CELLS and STRING-CHARS, which count the total length of
objects consed.
MISCS include overlays, markers, and some internal types.
Frames, windows, buffers, and subprocesses count as vectors
  (but the contents of a buffer's text do not count here).  */)
     ()
{
  Lisp_Object consed[8];

  consed[0] = make_number (min (MOST_POSITIVE_FIXNUM, cons_cells_consed));
  consed[1] = make_number (min (MOST_POSITIVE_FIXNUM, floats_consed));
  consed[2] = make_number (min (MOST_POSITIVE_FIXNUM, vector_cells_consed));
  consed[3] = make_number (min (MOST_POSITIVE_FIXNUM, symbols_consed));
  consed[4] = make_number (min (MOST_POSITIVE_FIXNUM, string_chars_consed));
  consed[5] = make_number (min (MOST_POSITIVE_FIXNUM, misc_objects_consed));
  consed[6] = make_number (min (MOST_POSITIVE_FIXNUM, intervals_consed));
  consed[7] = make_number (min (MOST_POSITIVE_FIXNUM, strings_consed));

  return Flist (8, consed);
}

int suppress_checking;
void
die (msg, file, line)
     const char *msg;
     const char *file;
     int line;
{
  fprintf (stderr, "\r\nEmacs fatal error: %s:%d: %s\r\n",
	   file, line, msg);
  abort ();
}

/* Initialization */

void
init_alloc_once ()
{
  /* Used to do Vpurify_flag = Qt here, but Qt isn't set up yet!  */
  purebeg = PUREBEG;
  pure_size = PURESIZE;
  pure_bytes_used = 0;
  pure_bytes_used_before_overflow = 0;

#if GC_MARK_STACK || defined GC_MALLOC_CHECK
  mem_init ();
  Vdead = make_pure_string ("DEAD", 4, 4, 0);
#endif

  all_vectors = 0;
  ignore_warnings = 1;
#ifdef DOUG_LEA_MALLOC
  mallopt (M_TRIM_THRESHOLD, 128*1024); /* trim threshold */
  mallopt (M_MMAP_THRESHOLD, 64*1024); /* mmap threshold */
  mallopt (M_MMAP_MAX, MMAP_MAX_AREAS); /* max. number of mmap'ed areas */
#endif
  init_strings ();
  init_cons ();
  init_symbol ();
  init_marker ();
  init_float ();
  init_intervals ();

#ifdef REL_ALLOC
  malloc_hysteresis = 32;
#else
  malloc_hysteresis = 0;
#endif

  spare_memory = (char *) malloc (SPARE_MEMORY);

  ignore_warnings = 0;
  gcprolist = 0;
  byte_stack_list = 0;
  staticidx = 0;
  consing_since_gc = 0;
  gc_cons_threshold = 100000 * sizeof (Lisp_Object);
#ifdef VIRT_ADDR_VARIES
  malloc_sbrk_unused = 1<<22;	/* A large number */
  malloc_sbrk_used = 100000;	/* as reasonable as any number */
#endif /* VIRT_ADDR_VARIES */
}

void
init_alloc ()
{
  gcprolist = 0;
  byte_stack_list = 0;
#if GC_MARK_STACK
#if !defined GC_SAVE_REGISTERS_ON_STACK && !defined GC_SETJMP_WORKS
  setjmp_tested_p = longjmps_done = 0;
#endif
#endif
}

void
syms_of_alloc ()
{
  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold,
	      doc: /* *Number of bytes of consing between garbage collections.
Garbage collection can happen automatically once this many bytes have been
allocated since the last garbage collection.  All data types count.

Garbage collection happens automatically only when `eval' is called.

By binding this temporarily to a large number, you can effectively
prevent garbage collection during a part of the program.  */);

  DEFVAR_INT ("pure-bytes-used", &pure_bytes_used,
	      doc: /* Number of bytes of sharable Lisp data allocated so far.  */);

  DEFVAR_INT ("cons-cells-consed", &cons_cells_consed,
	      doc: /* Number of cons cells that have been consed so far.  */);

  DEFVAR_INT ("floats-consed", &floats_consed,
	      doc: /* Number of floats that have been consed so far.  */);

  DEFVAR_INT ("vector-cells-consed", &vector_cells_consed,
	      doc: /* Number of vector cells that have been consed so far.  */);

  DEFVAR_INT ("symbols-consed", &symbols_consed,
	      doc: /* Number of symbols that have been consed so far.  */);

  DEFVAR_INT ("string-chars-consed", &string_chars_consed,
	      doc: /* Number of string characters that have been consed so far.  */);

  DEFVAR_INT ("misc-objects-consed", &misc_objects_consed,
	      doc: /* Number of miscellaneous objects that have been consed so far.  */);

  DEFVAR_INT ("intervals-consed", &intervals_consed,
	      doc: /* Number of intervals that have been consed so far.  */);

  DEFVAR_INT ("strings-consed", &strings_consed,
	      doc: /* Number of strings that have been consed so far.  */);

  DEFVAR_LISP ("purify-flag", &Vpurify_flag,
	       doc: /* Non-nil means loading Lisp code in order to dump an executable.
This means that certain objects should be allocated in shared (pure) space.  */);

  DEFVAR_INT ("undo-limit", &undo_limit,
	      doc: /* Keep no more undo information once it exceeds this size.
This limit is applied when garbage collection happens.
The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_limit = 20000;

  DEFVAR_INT ("undo-strong-limit", &undo_strong_limit,
	      doc: /* Don't keep more than this much size of undo information.
A command which pushes past this size is itself forgotten.
This limit is applied when garbage collection happens.
The size is counted as the number of bytes occupied,
which includes both saved text and other data.  */);
  undo_strong_limit = 30000;

  DEFVAR_BOOL ("garbage-collection-messages", &garbage_collection_messages,
	       doc: /* Non-nil means display messages at start and end of garbage collection.  */);
  garbage_collection_messages = 0;

  DEFVAR_LISP ("post-gc-hook", &Vpost_gc_hook,
	       doc: /* Hook run after garbage collection has finished.  */);
  Vpost_gc_hook = Qnil;
  Qpost_gc_hook = intern ("post-gc-hook");
  staticpro (&Qpost_gc_hook);

  /* We build this in advance because if we wait until we need it, we might
     not be able to allocate the memory to hold it.  */
  memory_signal_data
    = Fcons (Qerror, Fcons (build_string ("Memory exhausted--use M-x save-some-buffers RET"), Qnil));
  staticpro (&memory_signal_data);

  staticpro (&Qgc_cons_threshold);
  Qgc_cons_threshold = intern ("gc-cons-threshold");

  staticpro (&Qchar_table_extra_slots);
  Qchar_table_extra_slots = intern ("char-table-extra-slots");

  defsubr (&Scons);
  defsubr (&Slist);
  defsubr (&Svector);
  defsubr (&Smake_byte_code);
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_char_table);
  defsubr (&Smake_string);
  defsubr (&Smake_bool_vector);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Spurecopy);
  defsubr (&Sgarbage_collect);
  defsubr (&Smemory_limit);
  defsubr (&Smemory_use_counts);

#if GC_MARK_STACK == GC_USE_GCPROS_CHECK_ZOMBIES
  defsubr (&Sgc_status);
#endif
}
