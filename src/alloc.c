/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 86, 88, 93, 94, 95, 97, 1998
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

/* Note that this declares bzero on OSF/1.  How dumb.  */
#include <signal.h>

#include <config.h>
#include "lisp.h"
#include "intervals.h"
#include "puresize.h"
#ifndef standalone
#include "buffer.h"
#include "window.h"
#include "frame.h"
#include "blockinput.h"
#include "keyboard.h"
#include "charset.h"
#endif

#include "syssignal.h"

extern char *sbrk ();

#ifdef DOUG_LEA_MALLOC
#include <malloc.h>
#define __malloc_size_t int

/* Specify maximum number of areas to mmap.
   It would be nice to use a value that explicitly
   means "no limit".  */
#define MMAP_MAX_AREAS 100000000

#else
/* The following come from gmalloc.c.  */

#if defined (__STDC__) && __STDC__
#include <stddef.h>
#define	__malloc_size_t		size_t
#else
#define	__malloc_size_t		unsigned int
#endif
extern __malloc_size_t _bytes_used;
extern int __malloc_extra_blocks;
#endif /* !defined(DOUG_LEA_MALLOC) */

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) < (B) ? (A) : (B))

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

/* Number of bytes of consing done since the last gc */
int consing_since_gc;

/* Count the amount of consing of various sorts of space.  */
int cons_cells_consed;
int floats_consed;
int vector_cells_consed;
int symbols_consed;
int string_chars_consed;
int misc_objects_consed;
int intervals_consed;

/* Number of bytes of consing since gc before another gc should be done. */
int gc_cons_threshold;

/* Nonzero during gc */
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
int undo_limit;
int undo_strong_limit;

int total_conses, total_markers, total_symbols, total_string_size, total_vector_size;
int total_free_conses, total_free_markers, total_free_symbols;
#ifdef LISP_FLOAT_TYPE
int total_free_floats, total_floats;
#endif /* LISP_FLOAT_TYPE */

/* Points to memory space allocated as "spare",
   to be freed if we run out of memory.  */
static char *spare_memory;

/* Amount of spare memory to keep in reserve.  */
#define SPARE_MEMORY (1 << 14)

/* Number of extra blocks malloc should get when it needs more core.  */
static int malloc_hysteresis;

/* Nonzero when malloc is called for allocating Lisp object space.  */
int allocating_for_lisp;

/* Non-nil means defun should do purecopy on the function definition */
Lisp_Object Vpurify_flag;

#ifndef HAVE_SHM
EMACS_INT pure[PURESIZE / sizeof (EMACS_INT)] = {0,};   /* Force it into data space! */
#define PUREBEG (char *) pure
#else
#define pure PURE_SEG_BITS   /* Use shared memory segment */
#define PUREBEG (char *)PURE_SEG_BITS

/* This variable is used only by the XPNTR macro when HAVE_SHM is
   defined.  If we used the PURESIZE macro directly there, that would
   make most of emacs dependent on puresize.h, which we don't want -
   you should be able to change that without too much recompilation.
   So map_in_data initializes pure_size, and the dependencies work
   out.  */
EMACS_INT pure_size;
#endif /* not HAVE_SHM */

/* Index in pure at which next pure object will be allocated. */
int pureptr;

/* If nonzero, this is a warning delivered by malloc and not yet displayed.  */
char *pending_malloc_warning;

/* Pre-computed signal argument for use when memory is exhausted.  */
Lisp_Object memory_signal_data;

/* Maximum amount of C stack to save when a GC happens.  */

#ifndef MAX_SAVE_STACK
#define MAX_SAVE_STACK 16000
#endif

/* Define DONT_COPY_FLAG to be some bit which will always be zero in a
   pointer to a Lisp_Object, when that pointer is viewed as an integer.
   (On most machines, pointers are even, so we can use the low bit.
   Word-addressable architectures may need to override this in the m-file.)
   When linking references to small strings through the size field, we
   use this slot to hold the bit that would otherwise be interpreted as
   the GC mark bit.  */
#ifndef DONT_COPY_FLAG
#define DONT_COPY_FLAG 1
#endif /* no DONT_COPY_FLAG  */

/* Buffer in which we save a copy of the C stack at each GC.  */

char *stack_copy;
int stack_copy_size;

/* Non-zero means ignore malloc warnings.  Set during initialization.  */
int ignore_warnings;

Lisp_Object Qgc_cons_threshold, Qchar_table_extra_slots;

static void mark_buffer (), mark_kboards ();
static void clear_marks (), gc_sweep ();
static void compact_strings ();
static void mark_glyph_matrix P_ ((struct glyph_matrix *));
static void mark_face_cache P_ ((struct face_cache *));

#ifdef HAVE_WINDOW_SYSTEM
static void mark_image P_ ((struct image *));
static void mark_image_cache P_ ((struct frame *));
#endif /* HAVE_WINDOW_SYSTEM */


extern int message_enable_multibyte;

/* Versions of malloc and realloc that print warnings as memory gets full.  */

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

/* malloc calls this if it finds we are near exhausting storage */

void
malloc_warning (str)
     char *str;
{
  pending_malloc_warning = str;
}

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

/* Called if malloc returns zero */

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

  /* This used to call error, but if we've run out of memory, we could get
     infinite recursion trying to build the string.  */
  while (1)
    Fsignal (Qnil, memory_signal_data);
}

/* Called if we can't allocate relocatable space for a buffer.  */

void
buffer_memory_full ()
{
  /* If buffers use the relocating allocator,
     no need to free spare_memory, because we may have plenty of malloc
     space left that we could get, and if we don't, the malloc that fails
     will itself cause spare_memory to be freed.
     If buffers don't use the relocating allocator,
     treat this like any other failing malloc.  */

#ifndef REL_ALLOC
  memory_full ();
#endif

  /* This used to call error, but if we've run out of memory, we could get
     infinite recursion trying to build the string.  */
  while (1)
    Fsignal (Qerror, memory_signal_data);
}

/* Like malloc routines but check for no memory and block interrupt input.  */

long *
xmalloc (size)
     int size;
{
  register long *val;

  BLOCK_INPUT;
  val = (long *) malloc (size);
  UNBLOCK_INPUT;

  if (!val && size) memory_full ();
  return val;
}

long *
xrealloc (block, size)
     long *block;
     int size;
{
  register long *val;

  BLOCK_INPUT;
  /* We must call malloc explicitly when BLOCK is 0, since some
     reallocs don't do this.  */
  if (! block)
    val = (long *) malloc (size);
  else
    val = (long *) realloc (block, size);
  UNBLOCK_INPUT;

  if (!val && size) memory_full ();
  return val;
}

void
xfree (block)
     long *block;
{
  BLOCK_INPUT;
  free (block);
  UNBLOCK_INPUT;
}

/* Like malloc but used for allocating Lisp data.  */

long *
lisp_malloc (size)
     int size;
{
  register long *val;

  BLOCK_INPUT;
  allocating_for_lisp++;
  val = (long *) malloc (size);
  allocating_for_lisp--;
  UNBLOCK_INPUT;

  if (!val && size) memory_full ();
  return val;
}

void
lisp_free (block)
     long *block;
{
  BLOCK_INPUT;
  allocating_for_lisp++;
  free (block);
  allocating_for_lisp--;
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
extern void * (*__malloc_hook) ();
static void * (*old_malloc_hook) ();
extern void * (*__realloc_hook) ();
static void * (*old_realloc_hook) ();
extern void (*__free_hook) ();
static void (*old_free_hook) ();

/* This function is used as the hook for free to call.  */

static void
emacs_blocked_free (ptr)
     void *ptr;
{
  BLOCK_INPUT;
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
    spare_memory = (char *) malloc (SPARE_MEMORY);

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
    spare_memory = (char *) malloc (SPARE_MEMORY);
}

/* This function is the malloc hook that Emacs uses.  */

static void *
emacs_blocked_malloc (size)
     unsigned size;
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
  __malloc_hook = emacs_blocked_malloc;
  UNBLOCK_INPUT;

  return value;
}

static void *
emacs_blocked_realloc (ptr, size)
     void *ptr;
     unsigned size;
{
  void *value;

  BLOCK_INPUT;
  __realloc_hook = old_realloc_hook;
  value = (void *) realloc (ptr, size);
  __realloc_hook = emacs_blocked_realloc;
  UNBLOCK_INPUT;

  return value;
}

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
#endif

/* Interval allocation.  */

#ifdef USE_TEXT_PROPERTIES
#define INTERVAL_BLOCK_SIZE \
  ((1020 - sizeof (struct interval_block *)) / sizeof (struct interval))

struct interval_block
  {
    struct interval_block *next;
    struct interval intervals[INTERVAL_BLOCK_SIZE];
  };

struct interval_block *interval_block;
static int interval_block_index;

INTERVAL interval_free_list;

/* Total number of interval blocks now in use.  */
int n_interval_blocks;

static void
init_intervals ()
{
  interval_block
    = (struct interval_block *) lisp_malloc (sizeof (struct interval_block));
  interval_block->next = 0;
  bzero ((char *) interval_block->intervals, sizeof interval_block->intervals);
  interval_block_index = 0;
  interval_free_list = 0;
  n_interval_blocks = 1;
}

#define INIT_INTERVALS init_intervals ()

INTERVAL
make_interval ()
{
  INTERVAL val;

  if (interval_free_list)
    {
      val = interval_free_list;
      interval_free_list = interval_free_list->parent;
    }
  else
    {
      if (interval_block_index == INTERVAL_BLOCK_SIZE)
	{
	  register struct interval_block *newi;

	  newi = (struct interval_block *) lisp_malloc (sizeof (struct interval_block));

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

static int total_free_intervals, total_intervals;

/* Mark the pointers of one interval. */

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

static void
mark_interval_tree (tree)
     register INTERVAL tree;
{
  /* No need to test if this tree has been marked already; this
     function is always called through the MARK_INTERVAL_TREE macro,
     which takes care of that.  */

  /* XMARK expands to an assignment; the LHS of an assignment can't be
     a cast.  */
  XMARK (* (Lisp_Object *) &tree->parent);

  traverse_intervals (tree, 1, 0, mark_interval, Qnil);
}

#define MARK_INTERVAL_TREE(i)				\
  do {							\
    if (!NULL_INTERVAL_P (i)				\
	&& ! XMARKBIT (*(Lisp_Object *) &i->parent))	\
      mark_interval_tree (i);				\
  } while (0)

/* The oddity in the call to XUNMARK is necessary because XUNMARK
   expands to an assignment to its argument, and most C compilers don't
   support casts on the left operand of `='.  */
#define UNMARK_BALANCE_INTERVALS(i) 				\
{                                   				\
   if (! NULL_INTERVAL_P (i))       				\
     {                              				\
       XUNMARK (* (Lisp_Object *) (&(i)->parent));		\
       (i) = balance_intervals (i);				\
     } 								\
}

#else  /* no interval use */

#define INIT_INTERVALS

#define UNMARK_BALANCE_INTERVALS(i)
#define MARK_INTERVAL_TREE(i)

#endif /* no interval use */

/* Floating point allocation.  */

#ifdef LISP_FLOAT_TYPE
/* Allocation of float cells, just like conses */
/* We store float cells inside of float_blocks, allocating a new
   float_block with malloc whenever necessary.  Float cells reclaimed by
   GC are put on a free list to be reallocated before allocating
   any new float cells from the latest float_block.

   Each float_block is just under 1020 bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#define FLOAT_BLOCK_SIZE \
  ((1020 - sizeof (struct float_block *)) / sizeof (struct Lisp_Float))

struct float_block
  {
    struct float_block *next;
    struct Lisp_Float floats[FLOAT_BLOCK_SIZE];
  };

struct float_block *float_block;
int float_block_index;

/* Total number of float blocks now in use.  */
int n_float_blocks;

struct Lisp_Float *float_free_list;

void
init_float ()
{
  float_block = (struct float_block *) lisp_malloc (sizeof (struct float_block));
  float_block->next = 0;
  bzero ((char *) float_block->floats, sizeof float_block->floats);
  float_block_index = 0;
  float_free_list = 0;
  n_float_blocks = 1;
}

/* Explicitly free a float cell.  */
void
free_float (ptr)
     struct Lisp_Float *ptr;
{
  *(struct Lisp_Float **)&ptr->data = float_free_list;
  float_free_list = ptr;
}

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

	  new = (struct float_block *) lisp_malloc (sizeof (struct float_block));
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = float_block;
	  float_block = new;
	  float_block_index = 0;
	  n_float_blocks++;
	}
      XSETFLOAT (val, &float_block->floats[float_block_index++]);
    }
  XFLOAT (val)->data = float_value;
  XSETFASTINT (XFLOAT (val)->type, 0);	/* bug chasing -wsr */
  consing_since_gc += sizeof (struct Lisp_Float);
  floats_consed++;
  return val;
}

#endif /* LISP_FLOAT_TYPE */

/* Allocation of cons cells */
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

struct cons_block *cons_block;
int cons_block_index;

struct Lisp_Cons *cons_free_list;

/* Total number of cons blocks now in use.  */
int n_cons_blocks;

void
init_cons ()
{
  cons_block = (struct cons_block *) lisp_malloc (sizeof (struct cons_block));
  cons_block->next = 0;
  bzero ((char *) cons_block->conses, sizeof cons_block->conses);
  cons_block_index = 0;
  cons_free_list = 0;
  n_cons_blocks = 1;
}

/* Explicitly free a cons cell.  */

void
free_cons (ptr)
     struct Lisp_Cons *ptr;
{
  *(struct Lisp_Cons **)&ptr->cdr = cons_free_list;
  cons_free_list = ptr;
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
  "Create a new cons, give it CAR and CDR as components, and return it.")
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
	  new = (struct cons_block *) lisp_malloc (sizeof (struct cons_block));
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	  n_cons_blocks++;
	}
      XSETCONS (val, &cons_block->conses[cons_block_index++]);
    }
  XCONS (val)->car = car;
  XCONS (val)->cdr = cdr;
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
  "Return a newly created list with specified arguments as elements.\n\
Any number of arguments, even zero arguments, are allowed.")
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
  "Return a newly created list of length LENGTH, with each element being INIT.")
  (length, init)
     register Lisp_Object length, init;
{
  register Lisp_Object val;
  register int size;

  CHECK_NATNUM (length, 0);
  size = XFASTINT (length);

  val = Qnil;
  while (size-- > 0)
    val = Fcons (init, val);
  return val;
}

/* Allocation of vectors */

struct Lisp_Vector *all_vectors;

/* Total number of vectorlike objects now in use.  */
int n_vectors;

struct Lisp_Vector *
allocate_vectorlike (len)
     EMACS_INT len;
{
  struct Lisp_Vector *p;

#ifdef DOUG_LEA_MALLOC
  /* Prevent mmap'ing the chunk (which is potentially very large). */
  mallopt (M_MMAP_MAX, 0);
#endif
  p = (struct Lisp_Vector *)lisp_malloc (sizeof (struct Lisp_Vector)
				     + (len - 1) * sizeof (Lisp_Object));
#ifdef DOUG_LEA_MALLOC
  /* Back to a reasonable maximum of mmap'ed areas. */
  mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif
  VALIDATE_LISP_STORAGE (p, 0);
  consing_since_gc += (sizeof (struct Lisp_Vector)
		       + (len - 1) * sizeof (Lisp_Object));
  vector_cells_consed += len;
  n_vectors++;

  p->next = all_vectors;
  all_vectors = p;
  return p;
}

DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
  "Return a newly created vector of length LENGTH, with each element being INIT.\n\
See also the function `vector'.")
  (length, init)
     register Lisp_Object length, init;
{
  Lisp_Object vector;
  register EMACS_INT sizei;
  register int index;
  register struct Lisp_Vector *p;

  CHECK_NATNUM (length, 0);
  sizei = XFASTINT (length);

  p = allocate_vectorlike (sizei);
  p->size = sizei;
  for (index = 0; index < sizei; index++)
    p->contents[index] = init;

  XSETVECTOR (vector, p);
  return vector;
}

DEFUN ("make-char-table", Fmake_char_table, Smake_char_table, 1, 2, 0,
  "Return a newly created char-table, with purpose PURPOSE.\n\
Each element is initialized to INIT, which defaults to nil.\n\
PURPOSE should be a symbol which has a `char-table-extra-slots' property.\n\
The property's value should be an integer between 0 and 10.")
  (purpose, init)
     register Lisp_Object purpose, init;
{
  Lisp_Object vector;
  Lisp_Object n;
  CHECK_SYMBOL (purpose, 1);
  n = Fget (purpose, Qchar_table_extra_slots);
  CHECK_NUMBER (n, 0);
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
  "Return a newly created vector with specified arguments as elements.\n\
Any number of arguments, even zero arguments, are allowed.")
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
  "Create a byte-code object with specified arguments as elements.\n\
The arguments should be the arglist, bytecode-string, constant vector,\n\
stack size, (optional) doc string, and (optional) interactive spec.\n\
The first four arguments are required; at most six have any\n\
significance.")
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

/* Allocation of symbols.
   Just like allocation of conses!

   Each symbol_block is just under 1020 bytes long,
   since malloc really allocates in units of powers of two
   and uses 4 bytes for its own overhead. */

#define SYMBOL_BLOCK_SIZE \
  ((1020 - sizeof (struct symbol_block *)) / sizeof (struct Lisp_Symbol))

struct symbol_block
  {
    struct symbol_block *next;
    struct Lisp_Symbol symbols[SYMBOL_BLOCK_SIZE];
  };

struct symbol_block *symbol_block;
int symbol_block_index;

struct Lisp_Symbol *symbol_free_list;

/* Total number of symbol blocks now in use.  */
int n_symbol_blocks;

void
init_symbol ()
{
  symbol_block = (struct symbol_block *) lisp_malloc (sizeof (struct symbol_block));
  symbol_block->next = 0;
  bzero ((char *) symbol_block->symbols, sizeof symbol_block->symbols);
  symbol_block_index = 0;
  symbol_free_list = 0;
  n_symbol_blocks = 1;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
  "Return a newly allocated uninterned symbol whose name is NAME.\n\
Its value and function definition are void, and its property list is nil.")
  (name)
     Lisp_Object name;
{
  register Lisp_Object val;
  register struct Lisp_Symbol *p;

  CHECK_STRING (name, 0);

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
	  new = (struct symbol_block *) lisp_malloc (sizeof (struct symbol_block));
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
  p->obarray = Qnil;
  p->plist = Qnil;
  p->value = Qunbound;
  p->function = Qunbound;
  p->next = 0;
  consing_since_gc += sizeof (struct Lisp_Symbol);
  symbols_consed++;
  return val;
}

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
  marker_block = (struct marker_block *) lisp_malloc (sizeof (struct marker_block));
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
	  new = (struct marker_block *) lisp_malloc (sizeof (struct marker_block));
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
  "Return a newly allocated marker which does not point at any place.")
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

/* Allocation of strings */

/* Strings reside inside of string_blocks.  The entire data of the string,
 both the size and the contents, live in part of the `chars' component of a string_block.
 The `pos' component is the index within `chars' of the first free byte.

 first_string_block points to the first string_block ever allocated.
 Each block points to the next one with its `next' field.
 The `prev' fields chain in reverse order.
 The last one allocated is the one currently being filled.
 current_string_block points to it.

 The string_blocks that hold individual large strings
 go in a separate chain, started by large_string_blocks.  */


/* String blocks contain this many useful bytes.
   8188 is power of 2, minus 4 for malloc overhead. */
#define STRING_BLOCK_SIZE (8188 - sizeof (struct string_block_head))

/* A string bigger than this gets its own specially-made string block
 if it doesn't fit in the current one. */
#define STRING_BLOCK_OUTSIZE 1024

struct string_block_head
  {
    struct string_block *next, *prev;
    EMACS_INT pos;
  };

struct string_block
  {
    struct string_block *next, *prev;
    EMACS_INT pos;
    char chars[STRING_BLOCK_SIZE];
  };

/* This points to the string block we are now allocating strings.  */

struct string_block *current_string_block;

/* This points to the oldest string block, the one that starts the chain.  */

struct string_block *first_string_block;

/* Last string block in chain of those made for individual large strings.  */

struct string_block *large_string_blocks;

/* If SIZE is the length of a string, this returns how many bytes
   the string occupies in a string_block (including padding).  */

#define STRING_FULLSIZE(size) (((size) + 1 + STRING_BASE_SIZE + STRING_PAD - 1) \
			       & ~(STRING_PAD - 1))
     /* Add 1 for the null terminator,
	and add STRING_PAD - 1 as part of rounding up.  */

#define STRING_PAD (sizeof (EMACS_INT))
/* Size of the stuff in the string not including its data.  */
#define STRING_BASE_SIZE (((sizeof (struct Lisp_String) - 1) / STRING_PAD) * STRING_PAD)

#if 0
#define STRING_FULLSIZE(SIZE)   \
(((SIZE) + 2 * sizeof (EMACS_INT)) & ~(sizeof (EMACS_INT) - 1))
#endif

/* Total number of string blocks now in use.  */
int n_string_blocks;

void
init_strings ()
{
  current_string_block = (struct string_block *) lisp_malloc (sizeof (struct string_block));
  first_string_block = current_string_block;
  consing_since_gc += sizeof (struct string_block);
  current_string_block->next = 0;
  current_string_block->prev = 0;
  current_string_block->pos = 0;
  large_string_blocks = 0;
  n_string_blocks = 1;
}

DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
  "Return a newly created string of length LENGTH, with each element being INIT.\n\
Both LENGTH and INIT must be numbers.")
  (length, init)
     Lisp_Object length, init;
{
  register Lisp_Object val;
  register unsigned char *p, *end;
  int c, nbytes;

  CHECK_NATNUM (length, 0);
  CHECK_NUMBER (init, 1);

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
      unsigned char work[4], *str;
      int len = CHAR_STRING (c, work, str);

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
  "Return a new bool-vector of length LENGTH, using INIT for as each element.\n\
LENGTH must be a number.  INIT matters only in whether it is t or nil.")
  (length, init)
     Lisp_Object length, init;
{
  register Lisp_Object val;
  struct Lisp_Bool_Vector *p;
  int real_init, i;
  int length_in_chars, length_in_elts, bits_per_value;

  CHECK_NATNUM (length, 0);

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

/* Make a string from NBYTES bytes at CONTENTS,
   and compute the number of characters from the contents.
   This string may be unibyte or multibyte, depending on the contents.  */

Lisp_Object
make_string (contents, nbytes)
     char *contents;
     int nbytes;
{
  register Lisp_Object val;
  int nchars = chars_in_text (contents, nbytes);
  val = make_uninit_multibyte_string (nchars, nbytes);
  bcopy (contents, XSTRING (val)->data, nbytes);
  if (STRING_BYTES (XSTRING (val)) == XSTRING (val)->size)
    SET_STRING_BYTES (XSTRING (val), -1);
  return val;
}

/* Make a unibyte string from LENGTH bytes at CONTENTS.  */

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

/* Make a multibyte string from NCHARS characters
   occupying NBYTES bytes at CONTENTS.  */

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

/* Make a string from NCHARS characters
   occupying NBYTES bytes at CONTENTS.
   It is a multibyte string if NBYTES != NCHARS.  */

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

/* Make a multibyte string from NCHARS characters
   occupying NBYTES bytes at CONTENTS.  */

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

/* Make a string from the data at STR,
   treating it as multibyte if the data warrants.  */

Lisp_Object
build_string (str)
     char *str;
{
  return make_string (str, strlen (str));
}

Lisp_Object
make_uninit_string (length)
     int length;
{
  Lisp_Object val;
  val = make_uninit_multibyte_string (length, length);
  SET_STRING_BYTES (XSTRING (val), -1);
  return val;
}

Lisp_Object
make_uninit_multibyte_string (length, length_byte)
     int length, length_byte;
{
  register Lisp_Object val;
  register int fullsize = STRING_FULLSIZE (length_byte);

  if (length < 0) abort ();

  if (fullsize <= STRING_BLOCK_SIZE - current_string_block->pos)
    /* This string can fit in the current string block */
    {
      XSETSTRING (val,
		  ((struct Lisp_String *)
		   (current_string_block->chars + current_string_block->pos)));
      current_string_block->pos += fullsize;
    }
  else if (fullsize > STRING_BLOCK_OUTSIZE)
    /* This string gets its own string block */
    {
      register struct string_block *new;
#ifdef DOUG_LEA_MALLOC
      /* Prevent mmap'ing the chunk (which is potentially very large).  */
      mallopt (M_MMAP_MAX, 0);
#endif
      new = (struct string_block *) lisp_malloc (sizeof (struct string_block_head) + fullsize);
#ifdef DOUG_LEA_MALLOC
      /* Back to a reasonable maximum of mmap'ed areas. */
      mallopt (M_MMAP_MAX, MMAP_MAX_AREAS);
#endif
      n_string_blocks++;
      VALIDATE_LISP_STORAGE (new, 0);
      consing_since_gc += sizeof (struct string_block_head) + fullsize;
      new->pos = fullsize;
      new->next = large_string_blocks;
      large_string_blocks = new;
      XSETSTRING (val,
		  ((struct Lisp_String *)
		   ((struct string_block_head *)new + 1)));
    }
  else
    /* Make a new current string block and start it off with this string */
    {
      register struct string_block *new;
      new = (struct string_block *) lisp_malloc (sizeof (struct string_block));
      n_string_blocks++;
      VALIDATE_LISP_STORAGE (new, sizeof *new);
      consing_since_gc += sizeof (struct string_block);
      current_string_block->next = new;
      new->prev = current_string_block;
      new->next = 0;
      current_string_block = new;
      new->pos = fullsize;
      XSETSTRING (val,
		  (struct Lisp_String *) current_string_block->chars);
    }
    
  string_chars_consed += fullsize;
  XSTRING (val)->size = length;
  SET_STRING_BYTES (XSTRING (val), length_byte);
  XSTRING (val)->data[length_byte] = 0;
  INITIALIZE_INTERVAL (XSTRING (val), NULL_INTERVAL);

  return val;
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

/* Pure storage management.  */

/* Must get an error if pure storage is full,
 since if it cannot hold a large string
 it may be able to hold conses that point to that string;
 then the string is not protected from gc. */

Lisp_Object
make_pure_string (data, length, length_byte, multibyte)
     char *data;
     int length;
     int length_byte;
     int multibyte;
{

  register Lisp_Object new;
  register int size = STRING_FULLSIZE (length_byte);

  if (pureptr + size > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSETSTRING (new, PUREBEG + pureptr);
  XSTRING (new)->size = length;
  SET_STRING_BYTES (XSTRING (new), (multibyte ? length_byte : -1));
  bcopy (data, XSTRING (new)->data, length_byte);
  XSTRING (new)->data[length_byte] = 0;

  /* We must give strings in pure storage some kind of interval.  So we
     give them a null one.  */
#if defined (USE_TEXT_PROPERTIES)
  XSTRING (new)->intervals = NULL_INTERVAL;
#endif
  pureptr += size;
  return new;
}

Lisp_Object
pure_cons (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object new;

  if (pureptr + sizeof (struct Lisp_Cons) > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSETCONS (new, PUREBEG + pureptr);
  pureptr += sizeof (struct Lisp_Cons);
  XCONS (new)->car = Fpurecopy (car);
  XCONS (new)->cdr = Fpurecopy (cdr);
  return new;
}

#ifdef LISP_FLOAT_TYPE

Lisp_Object
make_pure_float (num)
     double num;
{
  register Lisp_Object new;

  /* Make sure that PUREBEG + pureptr is aligned on at least a sizeof
     (double) boundary.  Some architectures (like the sparc) require
     this, and I suspect that floats are rare enough that it's no
     tragedy for those that do.  */
  {
    int alignment;
    char *p = PUREBEG + pureptr;

#ifdef __GNUC__
#if __GNUC__ >= 2
    alignment = __alignof (struct Lisp_Float);
#else
    alignment = sizeof (struct Lisp_Float);
#endif
#else
    alignment = sizeof (struct Lisp_Float);
#endif  
    p = (char *) (((unsigned long) p + alignment - 1) & - alignment);
    pureptr = p - PUREBEG;
  }

  if (pureptr + sizeof (struct Lisp_Float) > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSETFLOAT (new, PUREBEG + pureptr);
  pureptr += sizeof (struct Lisp_Float);
  XFLOAT (new)->data = num;
  XSETFASTINT (XFLOAT (new)->type, 0);	/* bug chasing -wsr */
  return new;
}

#endif /* LISP_FLOAT_TYPE */

Lisp_Object
make_pure_vector (len)
     EMACS_INT len;
{
  register Lisp_Object new;
  register EMACS_INT size = sizeof (struct Lisp_Vector) + (len - 1) * sizeof (Lisp_Object);

  if (pureptr + size > PURESIZE)
    error ("Pure Lisp storage exhausted");

  XSETVECTOR (new, PUREBEG + pureptr);
  pureptr += size;
  XVECTOR (new)->size = len;
  return new;
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
  "Make a copy of OBJECT in pure storage.\n\
Recursively copies contents of vectors and cons cells.\n\
Does not copy symbols.")
  (obj)
     register Lisp_Object obj;
{
  if (NILP (Vpurify_flag))
    return obj;

  if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE)
      && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)
    return obj;

  if (CONSP (obj))
    return pure_cons (XCONS (obj)->car, XCONS (obj)->cdr);
#ifdef LISP_FLOAT_TYPE
  else if (FLOATP (obj))
    return make_pure_float (XFLOAT (obj)->data);
#endif /* LISP_FLOAT_TYPE */
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
  else
    return obj;
}

/* Recording what needs to be marked for gc.  */

struct gcpro *gcprolist;

#define NSTATICS 1024

Lisp_Object *staticvec[NSTATICS] = {0};

int staticidx = 0;

/* Put an entry in staticvec, pointing at the variable whose address is given */

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
#if 0 /* We don't need this for GC purposes */
    jmp_buf jmp;
#endif
  };

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;	/* Points to vector of args. */
    int nargs;		/* length of vector */
	       /* if nargs is UNEVALLED, args points to slot holding list of unevalled args */
    char evalargs;
  };

/* Garbage collection!  */

/* Temporarily prevent garbage collection.  */

int
inhibit_garbage_collection ()
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object number;
  int nbits = min (VALBITS, BITS_PER_INT);

  XSETINT (number, ((EMACS_INT) 1 << (nbits - 1)) - 1);

  specbind (Qgc_cons_threshold, number);

  return count;
}

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
  "Reclaim storage for Lisp objects no longer needed.\n\
Returns info on amount of space in use:\n\
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)\n\
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS\n\
  (USED-FLOATS . FREE-FLOATS) (USED-INTERVALS . FREE-INTERVALS))\n\
Garbage collection happens automatically if you cons more than\n\
`gc-cons-threshold' bytes of Lisp data since previous garbage collection.")
  ()
{
  register struct gcpro *tail;
  register struct specbinding *bind;
  struct catchtag *catch;
  struct handler *handler;
  register struct backtrace *backlist;
  register Lisp_Object tem;
  char stack_top_variable;
  register int i;
  int message_p;

  /* In case user calls debug_print during GC,
     don't let that cause a recursive GC.  */
  consing_since_gc = 0;

  /* Save what's currently displayed in the echo area.  */
  message_p = push_message ();

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
	nextb = nextb->next;
      }
  }

  gc_in_progress = 1;

  /* clear_marks (); */

  /* In each "large string", set the MARKBIT of the size field.
     That enables mark_object to recognize them.  */
  {
    register struct string_block *b;
    for (b = large_string_blocks; b; b = b->next)
      ((struct Lisp_String *)(&b->chars[0]))->size |= MARKBIT;
  }

  /* Mark all the special slots that serve as the roots of accessibility.

     Usually the special slots to mark are contained in particular structures.
     Then we know no slot is marked twice because the structures don't overlap.
     In some cases, the structures point to the slots to be marked.
     For these, we use MARKBIT to avoid double marking of the slot.  */

  for (i = 0; i < staticidx; i++)
    mark_object (staticvec[i]);
  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      if (!XMARKBIT (tail->var[i]))
	{
	  mark_object (&tail->var[i]);
	  XMARK (tail->var[i]);
	}
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
		if (GC_CONSP (XCONS (tail)->car)
		    && GC_MARKERP (XCONS (XCONS (tail)->car)->car)
		    && ! XMARKBIT (XMARKER (XCONS (XCONS (tail)->car)->car)->chain))
		  {
		    if (NILP (prev))
		      nextb->undo_list = tail = XCONS (tail)->cdr;
		    else
		      tail = XCONS (prev)->cdr = XCONS (tail)->cdr;
		  }
		else
		  {
		    prev = tail;
		    tail = XCONS (tail)->cdr;
		  }
	      }
	  }

	nextb = nextb->next;
      }
  }

  gc_sweep ();

  /* Clear the mark bits that we set in certain root slots.  */

  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      XUNMARK (tail->var[i]);
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

  pop_message ();
  
  return Fcons (Fcons (make_number (total_conses),
		       make_number (total_free_conses)),
		Fcons (Fcons (make_number (total_symbols),
			      make_number (total_free_symbols)),
		       Fcons (Fcons (make_number (total_markers),
				     make_number (total_free_markers)),
			      Fcons (make_number (total_string_size),
				     Fcons (make_number (total_vector_size),
	 Fcons (Fcons
#ifdef LISP_FLOAT_TYPE
		(make_number (total_floats),
		 make_number (total_free_floats)),
#else /* not LISP_FLOAT_TYPE */
		(make_number (0), make_number (0)),
#endif /* not LISP_FLOAT_TYPE */
		Fcons (Fcons
#ifdef USE_TEXT_PROPERTIES
		       (make_number (total_intervals),
			make_number (total_free_intervals)),
#else /* not USE_TEXT_PROPERTIES */
		       (make_number (0), make_number (0)),
#endif /* not USE_TEXT_PROPERTIES */
		       Qnil)))))));
}

#if 0
static void
clear_marks ()
{
  /* Clear marks on all conses */
  {
    register struct cons_block *cblk;
    register int lim = cons_block_index;
  
    for (cblk = cons_block; cblk; cblk = cblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  XUNMARK (cblk->conses[i].car);
	lim = CONS_BLOCK_SIZE;
      }
  }
  /* Clear marks on all symbols */
  {
    register struct symbol_block *sblk;
    register int lim = symbol_block_index;
  
    for (sblk = symbol_block; sblk; sblk = sblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  {
	    XUNMARK (sblk->symbols[i].plist);
	  }
	lim = SYMBOL_BLOCK_SIZE;
      }
  }
  /* Clear marks on all markers */
  {
    register struct marker_block *sblk;
    register int lim = marker_block_index;
  
    for (sblk = marker_block; sblk; sblk = sblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (sblk->markers[i].u_marker.type == Lisp_Misc_Marker)
	    XUNMARK (sblk->markers[i].u_marker.chain);
	lim = MARKER_BLOCK_SIZE;
      }
  }
  /* Clear mark bits on all buffers */
  {
    register struct buffer *nextb = all_buffers;

    while (nextb)
      {
	XUNMARK (nextb->name);
	nextb = nextb->next;
      }
  }
}
#endif

/* Mark Lisp objects in glyph matrix MATRIX.  */

static void
mark_glyph_matrix (matrix)
     struct glyph_matrix *matrix;
{
  struct glyph_row *row = matrix->rows;
  struct glyph_row *end = row + matrix->nrows;

  while (row < end)
    {
      if (row->enabled_p)
	{
	  int area;
	  for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	    {
	      struct glyph *glyph = row->glyphs[area];
	      struct glyph *end_glyph = glyph + row->used[area];
	      
	      while (glyph < end_glyph)
		{
		  if (/* OBJECT Is zero for face extending glyphs, padding
			 spaces and such.  */
		      glyph->object
		      /* Marking the buffer itself should not be necessary.  */
		      && !BUFFERP (glyph->object))
		    mark_object (&glyph->object);
		  ++glyph;
		}
	    }
	}
      
      ++row;
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
	      mark_object (&face->registry);
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
  all the references contained in it.

   If the object referenced is a short string, the referencing slot
   is threaded into a chain of such slots, pointed to from
   the `size' field of the string.  The actual string size
   lives in the last slot in the chain.  We recognize the end
   because it is < (unsigned) STRING_BLOCK_SIZE.  */

#define LAST_MARKED_SIZE 500
Lisp_Object *last_marked[LAST_MARKED_SIZE];
int last_marked_index;

void
mark_object (argptr)
     Lisp_Object *argptr;
{
  Lisp_Object *objptr = argptr;
  register Lisp_Object obj;

 loop:
  obj = *objptr;
 loop2:
  XUNMARK (obj);

  if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE)
      && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)
    return;

  last_marked[last_marked_index++] = objptr;
  if (last_marked_index == LAST_MARKED_SIZE)
    last_marked_index = 0;

  switch (SWITCH_ENUM_CAST (XGCTYPE (obj)))
    {
    case Lisp_String:
      {
	register struct Lisp_String *ptr = XSTRING (obj);

	MARK_INTERVAL_TREE (ptr->intervals);
	if (ptr->size & MARKBIT)
	  /* A large string.  Just set ARRAY_MARK_FLAG.  */
	  ptr->size |= ARRAY_MARK_FLAG;
	else
	  {
	    /* A small string.  Put this reference
	       into the chain of references to it.
	       If the address includes MARKBIT, put that bit elsewhere
	       when we store OBJPTR into the size field.  */

	    if (XMARKBIT (*objptr))
	      {
		XSETFASTINT (*objptr, ptr->size);
		XMARK (*objptr);
	      }
	    else
	      XSETFASTINT (*objptr, ptr->size);

	    if ((EMACS_INT) objptr & DONT_COPY_FLAG)
	      abort ();
	    ptr->size = (EMACS_INT) objptr;
	    if (ptr->size & MARKBIT)
	      ptr->size ^= MARKBIT | DONT_COPY_FLAG;
	  }
      }
      break;

    case Lisp_Vectorlike:
      if (GC_BUFFERP (obj))
	{
	  if (!XMARKBIT (XBUFFER (obj)->name))
	    mark_buffer (obj);
	}
      else if (GC_SUBRP (obj))
	break;
      else if (GC_COMPILEDP (obj))
	/* We could treat this just like a vector, but it is better
	   to save the COMPILED_CONSTANTS element for last and avoid recursion
	   there.  */
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  register EMACS_INT size = ptr->size;
	  /* See comment above under Lisp_Vector.  */
	  struct Lisp_Vector *volatile ptr1 = ptr;
	  register int i;

	  if (size & ARRAY_MARK_FLAG)
	    break;   /* Already marked */
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	  size &= PSEUDOVECTOR_SIZE_MASK;
	  for (i = 0; i < size; i++) /* and then mark its elements */
	    {
	      if (i != COMPILED_CONSTANTS)
		mark_object (&ptr1->contents[i]);
	    }
	  /* This cast should be unnecessary, but some Mips compiler complains
	     (MIPS-ABI + SysVR4, DC/OSx, etc).  */
	  objptr = (Lisp_Object *) &ptr1->contents[COMPILED_CONSTANTS];
	  goto loop;
	}
      else if (GC_FRAMEP (obj))
	{
	  /* See comment above under Lisp_Vector for why this is volatile.  */
	  register struct frame *volatile ptr = XFRAME (obj);
	  register EMACS_INT size = ptr->size;

	  if (size & ARRAY_MARK_FLAG) break;   /* Already marked */
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */

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
	  mark_object (&ptr->toolbar_window);
	  mark_face_cache (ptr->face_cache);
#ifdef HAVE_WINDOW_SYSTEM
	  mark_image_cache (ptr);
	  mark_object (&ptr->desired_toolbar_items);
	  mark_object (&ptr->current_toolbar_items);
	  mark_object (&ptr->desired_toolbar_string);
	  mark_object (&ptr->current_toolbar_string);
#endif /* HAVE_WINDOW_SYSTEM */
	}
      else if (GC_BOOL_VECTOR_P (obj))
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);

	  if (ptr->size & ARRAY_MARK_FLAG)
	    break;   /* Already marked */
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	}
      else if (GC_WINDOWP (obj))
	{
	  register struct Lisp_Vector *ptr = XVECTOR (obj);
	  struct window *w = XWINDOW (obj);
	  register EMACS_INT size = ptr->size;
	  /* The reason we use ptr1 is to avoid an apparent hardware bug
	     that happens occasionally on the FSF's HP 300s.
	     The bug is that a2 gets clobbered by recursive calls to mark_object.
	     The clobberage seems to happen during function entry,
	     perhaps in the moveml instruction.
	     Yes, this is a crock, but we have to do it.  */
	  struct Lisp_Vector *volatile ptr1 = ptr;
	  register int i;

	  /* Stop if already marked.  */
	  if (size & ARRAY_MARK_FLAG)
	    break;

	  /* Mark it.  */
	  ptr->size |= ARRAY_MARK_FLAG;

	  /* There is no Lisp data above The member CURRENT_MATRIX in
	     struct WINDOW.  Stop marking when that slot is reached.  */
	  for (i = 0;
	       (char *) &ptr1->contents[i] < (char *) &w->current_matrix;
	       i++)
	    mark_object (&ptr1->contents[i]);

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
	  h->size |= ARRAY_MARK_FLAG;

	  /* Mark contents.  */
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
	  /* The reason we use ptr1 is to avoid an apparent hardware bug
	     that happens occasionally on the FSF's HP 300s.
	     The bug is that a2 gets clobbered by recursive calls to mark_object.
	     The clobberage seems to happen during function entry,
	     perhaps in the moveml instruction.
	     Yes, this is a crock, but we have to do it.  */
	  struct Lisp_Vector *volatile ptr1 = ptr;
	  register int i;

	  if (size & ARRAY_MARK_FLAG) break; /* Already marked */
	  ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	  if (size & PSEUDOVECTOR_FLAG)
	    size &= PSEUDOVECTOR_SIZE_MASK;

	  for (i = 0; i < size; i++) /* and then mark its elements */
	    mark_object (&ptr1->contents[i]);
	}
      break;

    case Lisp_Symbol:
      {
	/* See comment above under Lisp_Vector for why this is volatile.  */
	register struct Lisp_Symbol *volatile ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (XMARKBIT (ptr->plist)) break;
	XMARK (ptr->plist);
	mark_object ((Lisp_Object *) &ptr->value);
	mark_object (&ptr->function);
	mark_object (&ptr->plist);
	XSETTYPE (*(Lisp_Object *) &ptr->name, Lisp_String);
	mark_object ((Lisp_Object *) &ptr->name);
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
	    /* See comment above under Lisp_Vector for why not use ptr here.  */
	    objptr = &XBUFFER_LOCAL_VALUE (obj)->cdr;
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
	XMARK (ptr->car);
	/* If the cdr is nil, avoid recursion for the car.  */
	if (EQ (ptr->cdr, Qnil))
	  {
	    objptr = &ptr->car;
	    goto loop;
	  }
	mark_object (&ptr->car);
	/* See comment above under Lisp_Vector for why not use ptr here.  */
	objptr = &XCONS (obj)->cdr;
	goto loop;
      }

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      XMARK (XFLOAT (obj)->type);
      break;
#endif /* LISP_FLOAT_TYPE */

    case Lisp_Int:
      break;

    default:
      abort ();
    }
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
	      && ! XMARKBIT (XCONS (ptr->car)->car)
	      && GC_MARKERP (XCONS (ptr->car)->car))
	    {
	      XMARK (XCONS (ptr->car)->car);
	      mark_object (&XCONS (ptr->car)->cdr);
	    }
	  else
	    mark_object (&ptr->car);

	  if (CONSP (ptr->cdr))
	    tail = ptr->cdr;
	  else
	    break;
	}

      mark_object (&XCONS (tail)->cdr);
    }
  else
    mark_object (&buffer->undo_list);

#if 0
  mark_object (buffer->syntax_table);

  /* Mark the various string-pointers in the buffer object.
     Since the strings may be relocated, we must mark them
     in their actual slots.  So gc_sweep must convert each slot
     back to an ordinary C pointer.  */
  XSETSTRING (*(Lisp_Object *)&buffer->upcase_table, buffer->upcase_table);
  mark_object ((Lisp_Object *)&buffer->upcase_table);
  XSETSTRING (*(Lisp_Object *)&buffer->downcase_table, buffer->downcase_table);
  mark_object ((Lisp_Object *)&buffer->downcase_table);

  XSETSTRING (*(Lisp_Object *)&buffer->sort_table, buffer->sort_table);
  mark_object ((Lisp_Object *)&buffer->sort_table);
  XSETSTRING (*(Lisp_Object *)&buffer->folding_sort_table, buffer->folding_sort_table);
  mark_object ((Lisp_Object *)&buffer->folding_sort_table);
#endif

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

	if (s->size & MARKBIT)
	  survives_p = s->size & ARRAY_MARK_FLAG;
	else
	  survives_p = (s->size & ~DONT_COPY_FLAG) > STRING_BLOCK_SIZE;
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

#ifdef LISP_FLOAT_TYPE
    case Lisp_Float:
      survives_p = XMARKBIT (XFLOAT (obj)->type);
      break;
#endif /* LISP_FLOAT_TYPE */

    default:
      abort ();
    }

  return survives_p;
}



/* Sweep: find all structures not marked, and free them. */

static void
gc_sweep ()
{
  /* Remove or mark entries in weak hash tables.
     This must be done before any object is unmarked.  */
  sweep_weak_hash_tables ();

  total_string_size = 0;
  compact_strings ();

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

#ifdef LISP_FLOAT_TYPE
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
#endif /* LISP_FLOAT_TYPE */

#ifdef USE_TEXT_PROPERTIES
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
		iblk->intervals[i].parent = interval_free_list;
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
	    interval_free_list = iblk->intervals[0].parent;
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
#endif /* USE_TEXT_PROPERTIES */

  /* Put all unmarked symbols on free list */
  {
    register struct symbol_block *sblk;
    struct symbol_block **sprev = &symbol_block;
    register int lim = symbol_block_index;
    register int num_free = 0, num_used = 0;

    symbol_free_list = 0;
  
    for (sblk = symbol_block; sblk; sblk = *sprev)
      {
	register int i;
	int this_free = 0;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (sblk->symbols[i].plist))
	    {
	      *(struct Lisp_Symbol **)&sblk->symbols[i].value = symbol_free_list;
	      symbol_free_list = &sblk->symbols[i];
	      this_free++;
	    }
	  else
	    {
	      num_used++;
	      sblk->symbols[i].name
		= XSTRING (*(Lisp_Object *) &sblk->symbols[i].name);
	      XUNMARK (sblk->symbols[i].plist);
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

#ifndef standalone
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
	  xfree (buffer);
	  buffer = next;
	}
      else
	{
	  XUNMARK (buffer->name);
	  UNMARK_BALANCE_INTERVALS (BUF_INTERVALS (buffer));

#if 0
	  /* Each `struct Lisp_String *' was turned into a Lisp_Object
	     for purposes of marking and relocation.
	     Turn them back into C pointers now.  */
	  buffer->upcase_table
	    = XSTRING (*(Lisp_Object *)&buffer->upcase_table);
	  buffer->downcase_table
	    = XSTRING (*(Lisp_Object *)&buffer->downcase_table);
	  buffer->sort_table
	    = XSTRING (*(Lisp_Object *)&buffer->sort_table);
	  buffer->folding_sort_table
	    = XSTRING (*(Lisp_Object *)&buffer->folding_sort_table);
#endif

	  prev = buffer, buffer = buffer->next;
	}
  }

#endif /* standalone */

  /* Free all unmarked vectors */
  {
    register struct Lisp_Vector *vector = all_vectors, *prev = 0, *next;
    total_vector_size = 0;

    while (vector)
      if (!(vector->size & ARRAY_MARK_FLAG))
	{
#if 0
	  if ((vector->size & (PSEUDOVECTOR_FLAG | PVEC_HASH_TABLE))
	      == (PSEUDOVECTOR_FLAG | PVEC_HASH_TABLE))
	    fprintf (stderr, "Freeing hash table %p\n", vector);
#endif
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

  /* Free all "large strings" not marked with ARRAY_MARK_FLAG.  */
  {
    register struct string_block *sb = large_string_blocks, *prev = 0, *next;
    struct Lisp_String *s;

    while (sb)
      {
	s = (struct Lisp_String *) &sb->chars[0];
	if (s->size & ARRAY_MARK_FLAG)
	  {
	    ((struct Lisp_String *)(&sb->chars[0]))->size
	      &= ~ARRAY_MARK_FLAG & ~MARKBIT;
	    UNMARK_BALANCE_INTERVALS (s->intervals);
	    total_string_size += ((struct Lisp_String *)(&sb->chars[0]))->size;
	    prev = sb, sb = sb->next;
	  }
	else
	  {
	    if (prev)
	      prev->next = sb->next;
	    else
	      large_string_blocks = sb->next;
	    next = sb->next;
	    lisp_free (sb);
	    sb = next;
	    n_string_blocks--;
	  }
      }
  }
}

/* Compactify strings, relocate references, and free empty string blocks.  */

static void
compact_strings ()
{
  /* String block of old strings we are scanning.  */
  register struct string_block *from_sb;
  /* A preceding string block (or maybe the same one)
     where we are copying the still-live strings to.  */
  register struct string_block *to_sb;
  int pos;
  int to_pos;

  to_sb = first_string_block;
  to_pos = 0;

  /* Scan each existing string block sequentially, string by string.  */
  for (from_sb = first_string_block; from_sb; from_sb = from_sb->next)
    {
      pos = 0;
      /* POS is the index of the next string in the block.  */
      while (pos < from_sb->pos)
	{
	  register struct Lisp_String *nextstr
	    = (struct Lisp_String *) &from_sb->chars[pos];

	  register struct Lisp_String *newaddr;
	  register EMACS_INT size = nextstr->size;
	  EMACS_INT size_byte = nextstr->size_byte;

	  /* NEXTSTR is the old address of the next string.
	     Just skip it if it isn't marked.  */
	  if (((EMACS_UINT) size & ~DONT_COPY_FLAG) > STRING_BLOCK_SIZE)
	    {
	      /* It is marked, so its size field is really a chain of refs.
		 Find the end of the chain, where the actual size lives.  */
	      while (((EMACS_UINT) size & ~DONT_COPY_FLAG) > STRING_BLOCK_SIZE)
		{
		  if (size & DONT_COPY_FLAG)
		    size ^= MARKBIT | DONT_COPY_FLAG;
		  size = *(EMACS_INT *)size & ~MARKBIT;
		}

	      if (size_byte < 0)
		size_byte = size;

	      total_string_size += size_byte;

	      /* If it won't fit in TO_SB, close it out,
		 and move to the next sb.  Keep doing so until
		 TO_SB reaches a large enough, empty enough string block.
		 We know that TO_SB cannot advance past FROM_SB here
		 since FROM_SB is large enough to contain this string.
		 Any string blocks skipped here
		 will be patched out and freed later.  */
	      while (to_pos + STRING_FULLSIZE (size_byte)
		     > max (to_sb->pos, STRING_BLOCK_SIZE))
		{
		  to_sb->pos = to_pos;
		  to_sb = to_sb->next;
		  to_pos = 0;
		}
	      /* Compute new address of this string
		 and update TO_POS for the space being used.  */
	      newaddr = (struct Lisp_String *) &to_sb->chars[to_pos];
	      to_pos += STRING_FULLSIZE (size_byte);

	      /* Copy the string itself to the new place.  */
	      if (nextstr != newaddr)
		bcopy (nextstr, newaddr, STRING_FULLSIZE (size_byte));

	      /* Go through NEXTSTR's chain of references
		 and make each slot in the chain point to
		 the new address of this string.  */
	      size = newaddr->size;
	      while (((EMACS_UINT) size & ~DONT_COPY_FLAG) > STRING_BLOCK_SIZE)
		{
		  register Lisp_Object *objptr;
		  if (size & DONT_COPY_FLAG)
		    size ^= MARKBIT | DONT_COPY_FLAG;
		  objptr = (Lisp_Object *)size;

		  size = XFASTINT (*objptr) & ~MARKBIT;
		  if (XMARKBIT (*objptr))
		    {
		      XSETSTRING (*objptr, newaddr);
		      XMARK (*objptr);
		    }
		  else
		    XSETSTRING (*objptr, newaddr);
		}
	      /* Store the actual size in the size field.  */
	      newaddr->size = size;

#ifdef USE_TEXT_PROPERTIES
	      /* Now that the string has been relocated, rebalance its
                 interval tree, and update the tree's parent pointer. */
	      if (! NULL_INTERVAL_P (newaddr->intervals))
		{
		  UNMARK_BALANCE_INTERVALS (newaddr->intervals);
		  XSETSTRING (* (Lisp_Object *) &newaddr->intervals->parent,
			      newaddr);
		}
#endif /* USE_TEXT_PROPERTIES */
	    }
	  else if (size_byte < 0)
	    size_byte = size;

	  pos += STRING_FULLSIZE (size_byte);
	}
    }

  /* Close out the last string block still used and free any that follow.  */
  to_sb->pos = to_pos;
  current_string_block = to_sb;

  from_sb = to_sb->next;
  to_sb->next = 0;
  while (from_sb)
    {
      to_sb = from_sb->next;
      lisp_free (from_sb);
      n_string_blocks--;
      from_sb = to_sb;
    }

  /* Free any empty string blocks further back in the chain.
     This loop will never free first_string_block, but it is very
     unlikely that that one will become empty, so why bother checking?  */

  from_sb = first_string_block;
  while (to_sb = from_sb->next)
    {
      if (to_sb->pos == 0)
	{
	  if (from_sb->next = to_sb->next)
	    from_sb->next->prev = from_sb;
	  lisp_free (to_sb);
	  n_string_blocks--;
	}
      else
	from_sb = to_sb;
    }
}

/* Debugging aids.  */

DEFUN ("memory-limit", Fmemory_limit, Smemory_limit, 0, 0, 0,
  "Return the address of the last byte Emacs has allocated, divided by 1024.\n\
This may be helpful in debugging Emacs's memory usage.\n\
We divide the value by 1024 to make sure it fits in a Lisp integer.")
  ()
{
  Lisp_Object end;

  XSETINT (end, (EMACS_INT) sbrk (0) / 1024);

  return end;
}

DEFUN ("memory-use-counts", Fmemory_use_counts, Smemory_use_counts, 0, 0, 0,
  "Return a list of counters that measure how much consing there has been.\n\
Each of these counters increments for a certain kind of object.\n\
The counters wrap around from the largest positive integer to zero.\n\
Garbage collection does not decrease them.\n\
The elements of the value are as follows:\n\
  (CONSES FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS MISCS INTERVALS)\n\
All are in units of 1 = one object consed\n\
except for VECTOR-CELLS and STRING-CHARS, which count the total length of\n\
objects consed.\n\
MISCS include overlays, markers, and some internal types.\n\
Frames, windows, buffers, and subprocesses count as vectors\n\
  (but the contents of a buffer's text do not count here).")
  ()
{
  Lisp_Object lisp_cons_cells_consed;
  Lisp_Object lisp_floats_consed;
  Lisp_Object lisp_vector_cells_consed;
  Lisp_Object lisp_symbols_consed;
  Lisp_Object lisp_string_chars_consed;
  Lisp_Object lisp_misc_objects_consed;
  Lisp_Object lisp_intervals_consed;

  XSETINT (lisp_cons_cells_consed,
	   cons_cells_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));
  XSETINT (lisp_floats_consed,
	   floats_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));
  XSETINT (lisp_vector_cells_consed,
	   vector_cells_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));
  XSETINT (lisp_symbols_consed,
	   symbols_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));
  XSETINT (lisp_string_chars_consed,
	   string_chars_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));
  XSETINT (lisp_misc_objects_consed,
	   misc_objects_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));
  XSETINT (lisp_intervals_consed,
	   intervals_consed & ~(((EMACS_INT) 1) << (VALBITS - 1)));

  return Fcons (lisp_cons_cells_consed,
		Fcons (lisp_floats_consed,
		       Fcons (lisp_vector_cells_consed,
			      Fcons (lisp_symbols_consed,
				     Fcons (lisp_string_chars_consed,
					    Fcons (lisp_misc_objects_consed,
						   Fcons (lisp_intervals_consed,
							  Qnil)))))));
}

/* Initialization */

void
init_alloc_once ()
{
  /* Used to do Vpurify_flag = Qt here, but Qt isn't set up yet!  */
  pureptr = 0;
#ifdef HAVE_SHM
  pure_size = PURESIZE;
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
#ifdef LISP_FLOAT_TYPE
  init_float ();
#endif /* LISP_FLOAT_TYPE */
  INIT_INTERVALS;

#ifdef REL_ALLOC
  malloc_hysteresis = 32;
#else
  malloc_hysteresis = 0;
#endif

  spare_memory = (char *) malloc (SPARE_MEMORY);

  ignore_warnings = 0;
  gcprolist = 0;
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
}

void
syms_of_alloc ()
{
  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold,
    "*Number of bytes of consing between garbage collections.\n\
Garbage collection can happen automatically once this many bytes have been\n\
allocated since the last garbage collection.  All data types count.\n\n\
Garbage collection happens automatically only when `eval' is called.\n\n\
By binding this temporarily to a large number, you can effectively\n\
prevent garbage collection during a part of the program.");

  DEFVAR_INT ("pure-bytes-used", &pureptr,
    "Number of bytes of sharable Lisp data allocated so far.");

  DEFVAR_INT ("cons-cells-consed", &cons_cells_consed,
    "Number of cons cells that have been consed so far.");

  DEFVAR_INT ("floats-consed", &floats_consed,
    "Number of floats that have been consed so far.");

  DEFVAR_INT ("vector-cells-consed", &vector_cells_consed,
    "Number of vector cells that have been consed so far.");

  DEFVAR_INT ("symbols-consed", &symbols_consed,
    "Number of symbols that have been consed so far.");

  DEFVAR_INT ("string-chars-consed", &string_chars_consed,
    "Number of string characters that have been consed so far.");

  DEFVAR_INT ("misc-objects-consed", &misc_objects_consed,
    "Number of miscellaneous objects that have been consed so far.");

  DEFVAR_INT ("intervals-consed", &intervals_consed,
    "Number of intervals that have been consed so far.");

#if 0
  DEFVAR_INT ("data-bytes-used", &malloc_sbrk_used,
    "Number of bytes of unshared memory allocated in this session.");

  DEFVAR_INT ("data-bytes-free", &malloc_sbrk_unused,
    "Number of bytes of unshared memory remaining available in this session.");
#endif

  DEFVAR_LISP ("purify-flag", &Vpurify_flag,
    "Non-nil means loading Lisp code in order to dump an executable.\n\
This means that certain objects should be allocated in shared (pure) space.");

  DEFVAR_INT ("undo-limit", &undo_limit,
    "Keep no more undo information once it exceeds this size.\n\
This limit is applied when garbage collection happens.\n\
The size is counted as the number of bytes occupied,\n\
which includes both saved text and other data.");
  undo_limit = 20000;

  DEFVAR_INT ("undo-strong-limit", &undo_strong_limit,
    "Don't keep more than this much size of undo information.\n\
A command which pushes past this size is itself forgotten.\n\
This limit is applied when garbage collection happens.\n\
The size is counted as the number of bytes occupied,\n\
which includes both saved text and other data.");
  undo_strong_limit = 30000;

  DEFVAR_BOOL ("garbage-collection-messages", &garbage_collection_messages,
    "Non-nil means display messages at start and end of garbage collection.");
  garbage_collection_messages = 0;

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
}
