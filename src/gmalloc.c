/* Declarations for `malloc' and friends.
   Copyright (C) 1990-1993, 1995-1996, 1999, 2002-2007, 2013-2018 Free
   Software Foundation, Inc.
		  Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#include <config.h>

#if defined HAVE_PTHREAD && !defined HYBRID_MALLOC
#define USE_PTHREAD
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <unistd.h>

#ifdef USE_PTHREAD
#include <pthread.h>
#endif

#ifdef emacs
# include "lisp.h"
#endif

#include "ptr-bounds.h"

#ifdef HAVE_MALLOC_H
# if GNUC_PREREQ (4, 2, 0)
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
# endif
# include <malloc.h>
#endif
#ifndef __MALLOC_HOOK_VOLATILE
# define __MALLOC_HOOK_VOLATILE volatile
#endif
#ifndef HAVE_MALLOC_H
extern void (*__MALLOC_HOOK_VOLATILE __after_morecore_hook) (void);
extern void (*__MALLOC_HOOK_VOLATILE __malloc_initialize_hook) (void);
extern void *(*__morecore) (ptrdiff_t);
#endif

/* If HYBRID_MALLOC is defined, then temacs will use malloc,
   realloc... as defined in this file (and renamed gmalloc,
   grealloc... via the macros that follow).  The dumped emacs,
   however, will use the system malloc, realloc....  In other source
   files, malloc, realloc... are renamed hybrid_malloc,
   hybrid_realloc... via macros in conf_post.h.  hybrid_malloc and
   friends are wrapper functions defined later in this file.  */
#undef malloc
#undef realloc
#undef calloc
#undef aligned_alloc
#undef free
#define malloc gmalloc
#define realloc grealloc
#define calloc gcalloc
#define aligned_alloc galigned_alloc
#define free gfree
#define malloc_info gmalloc_info

#ifdef HYBRID_MALLOC
# include "sheap.h"
# define DUMPED bss_sbrk_did_unexec
#endif

#ifdef	__cplusplus
extern "C"
{
#endif

#ifdef HYBRID_MALLOC
#define extern static
#endif

/* Allocate SIZE bytes of memory.  */
extern void *malloc (size_t size) ATTRIBUTE_MALLOC_SIZE ((1));
/* Re-allocate the previously allocated block
   in ptr, making the new block SIZE bytes long.  */
extern void *realloc (void *ptr, size_t size) ATTRIBUTE_ALLOC_SIZE ((2));
/* Allocate NMEMB elements of SIZE bytes each, all initialized to 0.  */
extern void *calloc (size_t nmemb, size_t size) ATTRIBUTE_MALLOC_SIZE ((1,2));
/* Free a block.  */
extern void free (void *ptr);

/* Allocate SIZE bytes allocated to ALIGNMENT bytes.  */
extern void *aligned_alloc (size_t, size_t);
#ifdef MSDOS
extern void *memalign (size_t, size_t);
extern int posix_memalign (void **, size_t, size_t);
#endif

/* The allocator divides the heap into blocks of fixed size; large
   requests receive one or more whole blocks, and small requests
   receive a fragment of a block.  Fragment sizes are powers of two,
   and all fragments of a block are the same size.  When all the
   fragments in a block have been freed, the block itself is freed.  */
#define BLOCKLOG	(INT_WIDTH > 16 ? 12 : 9)
#define BLOCKSIZE	(1 << BLOCKLOG)
#define BLOCKIFY(SIZE)	(((SIZE) + BLOCKSIZE - 1) / BLOCKSIZE)

/* Determine the amount of memory spanned by the initial heap table
   (not an absolute limit).  */
#define HEAP		(INT_WIDTH > 16 ? 4194304 : 65536)

/* Number of contiguous free blocks allowed to build up at the end of
   memory before they will be returned to the system.  */
#define FINAL_FREE_BLOCKS	8

/* Data structure giving per-block information.  */
typedef union
  {
    /* Heap information for a busy block.  */
    struct
      {
	/* Zero for a block that is not one of ours (typically,
	   allocated by system malloc), positive for the log base 2 of
	   the fragment size of a fragmented block, -1 for the first
	   block of a multiblock object, and unspecified for later
	   blocks of that object.  Type-0 blocks can be present
	   because the system malloc can be invoked by library
	   functions in an undumped Emacs.  */
	int type;
	union
	  {
	    struct
	      {
		size_t nfree; /* Free frags in a fragmented block.  */
		size_t first; /* First free fragment of the block.  */
	      } frag;
	    /* For a large object, in its first block, this has the number
	       of blocks in the object.  */
	    ptrdiff_t size;
	  } info;
      } busy;
    /* Heap information for a free block
       (that may be the first of a free cluster).  */
    struct
      {
	size_t size;	/* Size (in blocks) of a free cluster.  */
	size_t next;	/* Index of next free cluster.  */
	size_t prev;	/* Index of previous free cluster.  */
      } free;
  } malloc_info;

/* Pointer to first block of the heap.  */
extern char *_heapbase;

/* Table indexed by block number giving per-block information.  */
extern malloc_info *_heapinfo;

/* Address to block number and vice versa.  */
#define BLOCK(A)	((size_t) ((char *) (A) - _heapbase) / BLOCKSIZE + 1)
#define ADDRESS(B)	((void *) (((B) - 1) * BLOCKSIZE + _heapbase))

/* Current search index for the heap table.  */
extern size_t _heapindex;

/* Limit of valid info table indices.  */
extern size_t _heaplimit;

/* Doubly linked lists of free fragments.  */
struct list
  {
    struct list *next;
    struct list *prev;
  };

/* Free list headers for each fragment size.  */
extern struct list _fraghead[];

/* List of blocks allocated with aligned_alloc and friends.  */
struct alignlist
  {
    struct alignlist *next;
    void *aligned;		/* The address that aligned_alloc returned.  */
    void *exact;		/* The address that malloc returned.  */
  };
extern struct alignlist *_aligned_blocks;

/* Instrumentation.  */
extern size_t _chunks_used;
extern size_t _bytes_used;
extern size_t _chunks_free;
extern size_t _bytes_free;

/* Internal versions of `malloc', `realloc', and `free'
   used when these functions need to call each other.
   They are the same but don't call the hooks
   and don't bound the resulting pointers.  */
extern void *_malloc_internal (size_t);
extern void *_realloc_internal (void *, size_t);
extern void _free_internal (void *);
extern void *_malloc_internal_nolock (size_t);
extern void *_realloc_internal_nolock (void *, size_t);
extern void _free_internal_nolock (void *);

#ifdef USE_PTHREAD
extern pthread_mutex_t _malloc_mutex, _aligned_blocks_mutex;
extern int _malloc_thread_enabled_p;
#define LOCK()					\
  do {						\
    if (_malloc_thread_enabled_p)		\
      pthread_mutex_lock (&_malloc_mutex);	\
  } while (0)
#define UNLOCK()				\
  do {						\
    if (_malloc_thread_enabled_p)		\
      pthread_mutex_unlock (&_malloc_mutex);	\
  } while (0)
#define LOCK_ALIGNED_BLOCKS()				\
  do {							\
    if (_malloc_thread_enabled_p)			\
      pthread_mutex_lock (&_aligned_blocks_mutex);	\
  } while (0)
#define UNLOCK_ALIGNED_BLOCKS()				\
  do {							\
    if (_malloc_thread_enabled_p)			\
      pthread_mutex_unlock (&_aligned_blocks_mutex);	\
  } while (0)
#else
#define LOCK()
#define UNLOCK()
#define LOCK_ALIGNED_BLOCKS()
#define UNLOCK_ALIGNED_BLOCKS()
#endif

/* Nonzero if `malloc' has been called and done its initialization.  */
extern int __malloc_initialized;
/* Function called to initialize malloc data structures.  */
extern int __malloc_initialize (void);

#ifdef GC_MCHECK

/* Return values for `mprobe': these are the kinds of inconsistencies that
   `mcheck' enables detection of.  */
enum mcheck_status
  {
    MCHECK_DISABLED = -1,	/* Consistency checking is not turned on.  */
    MCHECK_OK,			/* Block is fine.  */
    MCHECK_FREE,		/* Block freed twice.  */
    MCHECK_HEAD,		/* Memory before the block was clobbered.  */
    MCHECK_TAIL			/* Memory after the block was clobbered.  */
  };

/* Activate a standard collection of debugging hooks.  This must be called
   before `malloc' is ever called.  ABORTFUNC is called with an error code
   (see enum above) when an inconsistency is detected.  If ABORTFUNC is
   null, the standard function prints on stderr and then calls `abort'.  */
extern int mcheck (void (*abortfunc) (enum mcheck_status));

/* Check for aberrations in a particular malloc'd block.  You must have
   called `mcheck' already.  These are the same checks that `mcheck' does
   when you free or reallocate a block.  */
extern enum mcheck_status mprobe (void *ptr);

/* Activate a standard collection of tracing hooks.  */
extern void mtrace (void);
extern void muntrace (void);

/* Statistics available to the user.  */
struct mstats
  {
    size_t bytes_total;	/* Total size of the heap. */
    size_t chunks_used;	/* Chunks allocated by the user. */
    size_t bytes_used;	/* Byte total of user-allocated chunks. */
    size_t chunks_free;	/* Chunks in the free list. */
    size_t bytes_free;	/* Byte total of chunks in the free list. */
  };

/* Pick up the current statistics. */
extern struct mstats mstats (void);

#endif

#undef extern

#ifdef	__cplusplus
}
#endif

/* Memory allocator `malloc'.
   Copyright 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
		  Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#include <errno.h>

/* Debugging hook for 'malloc'.  */
static void *(*__MALLOC_HOOK_VOLATILE gmalloc_hook) (size_t);

/* Replacements for traditional glibc malloc hooks, for platforms that
   do not already have these hooks.  Platforms with these hooks all
   used relaxed ref/def, so it is OK to define them here too.  */
void (*__MALLOC_HOOK_VOLATILE __malloc_initialize_hook) (void);
void (*__MALLOC_HOOK_VOLATILE __after_morecore_hook) (void);
void *(*__morecore) (ptrdiff_t);

#ifndef HYBRID_MALLOC

/* Pointer to the base of the first block.  */
char *_heapbase;

/* Block information table.  Allocated with align/__free (not malloc/free).  */
malloc_info *_heapinfo;

/* Search index in the info table.  */
size_t _heapindex;

/* Limit of valid info table indices.  */
size_t _heaplimit;

/* Free lists for each fragment size.  */
struct list _fraghead[BLOCKLOG];

/* Instrumentation.  */
size_t _chunks_used;
size_t _bytes_used;
size_t _chunks_free;
size_t _bytes_free;

/* Are you experienced?  */
int __malloc_initialized;

#else

static struct list _fraghead[BLOCKLOG];

#endif /* HYBRID_MALLOC */

/* Number of extra blocks to get each time we ask for more core.
   This reduces the frequency of calling `(*__morecore)'.  */
#if defined DOUG_LEA_MALLOC || defined HYBRID_MALLOC || defined SYSTEM_MALLOC
static
#endif
size_t __malloc_extra_blocks;

/* Number of info entries.  */
static size_t heapsize;

#if defined GC_MALLOC_CHECK && defined GC_PROTECT_MALLOC_STATE

/* Some code for hunting a bug writing into _heapinfo.

   Call this macro with argument PROT non-zero to protect internal
   malloc state against writing to it, call it with a zero argument to
   make it readable and writable.

   Note that this only works if BLOCKSIZE == page size, which is
   the case on the i386.  */

#include <sys/types.h>
#include <sys/mman.h>

static int state_protected_p;
static size_t last_state_size;
static malloc_info *last_heapinfo;

void
protect_malloc_state (int protect_p)
{
  /* If _heapinfo has been relocated, make sure its old location
     isn't left read-only; it will be reused by malloc.  */
  if (_heapinfo != last_heapinfo
      && last_heapinfo
      && state_protected_p)
    mprotect (last_heapinfo, last_state_size, PROT_READ | PROT_WRITE);

  last_state_size = _heaplimit * sizeof *_heapinfo;
  last_heapinfo   = _heapinfo;

  if (protect_p != state_protected_p)
    {
      state_protected_p = protect_p;
      if (mprotect (_heapinfo, last_state_size,
		    protect_p ? PROT_READ : PROT_READ | PROT_WRITE) != 0)
	abort ();
    }
}

#define PROTECT_MALLOC_STATE(PROT) protect_malloc_state (PROT)

#else
#define PROTECT_MALLOC_STATE(PROT)	/* empty */
#endif


/* Aligned allocation.  */
static void *
align (size_t size)
{
  void *result;
  ptrdiff_t adj;

  /* align accepts an unsigned argument, but __morecore accepts a
     signed one.  This could lead to trouble if SIZE overflows the
     ptrdiff_t type accepted by __morecore.  We just punt in that
     case, since they are requesting a ludicrous amount anyway.  */
  if (PTRDIFF_MAX < size)
    result = 0;
  else
    result = (*__morecore) (size);
  adj = (uintptr_t) result % BLOCKSIZE;
  if (adj != 0)
    {
      adj = BLOCKSIZE - adj;
      (*__morecore) (adj);
      result = (char *) result + adj;
    }

  if (__after_morecore_hook)
    (*__after_morecore_hook) ();

  return result;
}

/* Get SIZE bytes, if we can get them starting at END.
   Return the address of the space we got.
   If we cannot get space at END, fail and return 0.  */
static void *
get_contiguous_space (ptrdiff_t size, void *position)
{
  void *before;
  void *after;

  before = (*__morecore) (0);
  /* If we can tell in advance that the break is at the wrong place,
     fail now.  */
  if (before != position)
    return 0;

  /* Allocate SIZE bytes and get the address of them.  */
  after = (*__morecore) (size);
  if (!after)
    return 0;

  /* It was not contiguous--reject it.  */
  if (after != position)
    {
      (*__morecore) (- size);
      return 0;
    }

  return after;
}


/* This is called when `_heapinfo' and `heapsize' have just
   been set to describe a new info table.  Set up the table
   to describe itself and account for it in the statistics.  */
static void
register_heapinfo (void)
{
  size_t block, blocks;

  block = BLOCK (_heapinfo);
  blocks = BLOCKIFY (heapsize * sizeof (malloc_info));

  /* Account for the _heapinfo block itself in the statistics.  */
  _bytes_used += blocks * BLOCKSIZE;
  ++_chunks_used;

  /* Describe the heapinfo block itself in the heapinfo.  */
  _heapinfo[block].busy.type = -1;
  _heapinfo[block].busy.info.size = blocks;
}

#ifdef USE_PTHREAD
pthread_mutex_t _malloc_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t _aligned_blocks_mutex = PTHREAD_MUTEX_INITIALIZER;
int _malloc_thread_enabled_p;

static void
malloc_atfork_handler_prepare (void)
{
  LOCK ();
  LOCK_ALIGNED_BLOCKS ();
}

static void
malloc_atfork_handler_parent (void)
{
  UNLOCK_ALIGNED_BLOCKS ();
  UNLOCK ();
}

static void
malloc_atfork_handler_child (void)
{
  UNLOCK_ALIGNED_BLOCKS ();
  UNLOCK ();
}

/* Set up mutexes and make malloc etc. thread-safe.  */
void
malloc_enable_thread (void)
{
  if (_malloc_thread_enabled_p)
    return;

  /* Some pthread implementations call malloc for statically
     initialized mutexes when they are used first.  To avoid such a
     situation, we initialize mutexes here while their use is
     disabled in malloc etc.  */
  pthread_mutex_init (&_malloc_mutex, NULL);
  pthread_mutex_init (&_aligned_blocks_mutex, NULL);
  pthread_atfork (malloc_atfork_handler_prepare,
		  malloc_atfork_handler_parent,
		  malloc_atfork_handler_child);
  _malloc_thread_enabled_p = 1;
}
#endif	/* USE_PTHREAD */

static void
malloc_initialize_1 (void)
{
#ifdef GC_MCHECK
  mcheck (NULL);
#endif

  if (__malloc_initialize_hook)
    (*__malloc_initialize_hook) ();

  heapsize = HEAP / BLOCKSIZE;
  _heapinfo = align (heapsize * sizeof (malloc_info));
  if (_heapinfo == NULL)
    return;
  memset (_heapinfo, 0, heapsize * sizeof (malloc_info));
  _heapinfo[0].free.size = 0;
  _heapinfo[0].free.next = _heapinfo[0].free.prev = 0;
  _heapindex = 0;
  _heapbase = (char *) ptr_bounds_init (_heapinfo);
  _heaplimit = BLOCK (_heapbase + heapsize * sizeof (malloc_info));

  register_heapinfo ();

  __malloc_initialized = 1;
  PROTECT_MALLOC_STATE (1);
  return;
}

/* Set everything up and remember that we have.
   main will call malloc which calls this function.  That is before any threads
   or signal handlers has been set up, so we don't need thread protection.  */
int
__malloc_initialize (void)
{
  if (__malloc_initialized)
    return 0;

  malloc_initialize_1 ();

  return __malloc_initialized;
}

static int morecore_recursing;

/* Get neatly aligned memory, initializing or
   growing the heap info table as necessary. */
static void *
morecore_nolock (size_t size)
{
  void *result;
  malloc_info *newinfo, *oldinfo;
  size_t newsize;

  if (morecore_recursing)
    /* Avoid recursion.  The caller will know how to handle a null return.  */
    return NULL;

  result = align (size);
  if (result == NULL)
    return NULL;

  PROTECT_MALLOC_STATE (0);

  /* Check if we need to grow the info table.  */
  if (heapsize < BLOCK ((char *) result + size))
    {
      /* Calculate the new _heapinfo table size.  We do not account for the
	 added blocks in the table itself, as we hope to place them in
	 existing free space, which is already covered by part of the
	 existing table.  */
      newsize = heapsize;
      do
	newsize *= 2;
      while (newsize < BLOCK ((char *) result + size));

      /* We must not reuse existing core for the new info table when called
	 from realloc in the case of growing a large block, because the
	 block being grown is momentarily marked as free.  In this case
	 _heaplimit is zero so we know not to reuse space for internal
	 allocation.  */
      if (_heaplimit != 0)
	{
	  /* First try to allocate the new info table in core we already
	     have, in the usual way using realloc.  If realloc cannot
	     extend it in place or relocate it to existing sufficient core,
	     we will get called again, and the code above will notice the
	     `morecore_recursing' flag and return null.  */
	  int save = errno;	/* Don't want to clobber errno with ENOMEM.  */
	  morecore_recursing = 1;
	  newinfo = _realloc_internal_nolock (_heapinfo,
					      newsize * sizeof (malloc_info));
	  morecore_recursing = 0;
	  if (newinfo == NULL)
	    errno = save;
	  else
	    {
	      /* We found some space in core, and realloc has put the old
		 table's blocks on the free list.  Now zero the new part
		 of the table and install the new table location.  */
	      memset (&newinfo[heapsize], 0,
		      (newsize - heapsize) * sizeof (malloc_info));
	      _heapinfo = newinfo;
	      heapsize = newsize;
	      goto got_heap;
	    }
	}

      /* Allocate new space for the malloc info table.  */
      while (1)
  	{
 	  newinfo = align (newsize * sizeof (malloc_info));

 	  /* Did it fail?  */
 	  if (newinfo == NULL)
 	    {
 	      (*__morecore) (-size);
 	      return NULL;
 	    }

 	  /* Is it big enough to record status for its own space?
 	     If so, we win.  */
	  if (BLOCK ((char *) newinfo + newsize * sizeof (malloc_info))
 	      < newsize)
 	    break;

 	  /* Must try again.  First give back most of what we just got.  */
 	  (*__morecore) (- newsize * sizeof (malloc_info));
 	  newsize *= 2;
  	}

      /* Copy the old table to the beginning of the new,
	 and zero the rest of the new table.  */
      memcpy (newinfo, _heapinfo, heapsize * sizeof (malloc_info));
      memset (&newinfo[heapsize], 0,
	      (newsize - heapsize) * sizeof (malloc_info));
      oldinfo = _heapinfo;
      _heapinfo = newinfo;
      heapsize = newsize;

      register_heapinfo ();

      /* Reset _heaplimit so _free_internal never decides
	 it can relocate or resize the info table.  */
      _heaplimit = 0;
      _free_internal_nolock (oldinfo);
      PROTECT_MALLOC_STATE (0);

      /* The new heap limit includes the new table just allocated.  */
      _heaplimit = BLOCK ((char *) newinfo + heapsize * sizeof (malloc_info));
      return result;
    }

 got_heap:
  _heaplimit = BLOCK ((char *) result + size);
  return result;
}

/* Allocate memory from the heap.  */
void *
_malloc_internal_nolock (size_t size)
{
  void *result;
  size_t block, blocks, lastblocks, start;
  register size_t i;
  struct list *next;

  /* ANSI C allows `malloc (0)' to either return NULL, or to return a
     valid address you can realloc and free (though not dereference).

     It turns out that some extant code (sunrpc, at least Ultrix's version)
     expects `malloc (0)' to return non-NULL and breaks otherwise.
     Be compatible.  */

#if	0
  if (size == 0)
    return NULL;
#endif

  PROTECT_MALLOC_STATE (0);

  if (size < sizeof (struct list))
    size = sizeof (struct list);

  /* Determine the allocation policy based on the request size.  */
  if (size <= BLOCKSIZE / 2)
    {
      /* Small allocation to receive a fragment of a block.
	 Determine the logarithm to base two of the fragment size. */
      register size_t log = 1;
      --size;
      while ((size /= 2) != 0)
	++log;

      /* Look in the fragment lists for a
	 free fragment of the desired size. */
      next = _fraghead[log].next;
      if (next != NULL)
	{
	  /* There are free fragments of this size.
	     Pop a fragment out of the fragment list and return it.
	     Update the block's nfree and first counters. */
	  result = next;
	  next->prev->next = next->next;
	  if (next->next != NULL)
	    next->next->prev = next->prev;
	  block = BLOCK (result);
	  if (--_heapinfo[block].busy.info.frag.nfree != 0)
	    _heapinfo[block].busy.info.frag.first =
	      (uintptr_t) next->next % BLOCKSIZE >> log;

	  /* Update the statistics.  */
	  ++_chunks_used;
	  _bytes_used += 1 << log;
	  --_chunks_free;
	  _bytes_free -= 1 << log;
	}
      else
	{
	  /* No free fragments of the desired size, so get a new block
	     and break it into fragments, returning the first.  */
#ifdef GC_MALLOC_CHECK
	  result = _malloc_internal_nolock (BLOCKSIZE);
	  PROTECT_MALLOC_STATE (0);
#elif defined (USE_PTHREAD)
	  result = _malloc_internal_nolock (BLOCKSIZE);
#else
	  result = malloc (BLOCKSIZE);
#endif
	  if (result == NULL)
	    {
	      PROTECT_MALLOC_STATE (1);
	      goto out;
	    }

	  /* Link all fragments but the first into the free list.  */
	  next = (struct list *) ((char *) result + (1 << log));
	  next->next = NULL;
	  next->prev = &_fraghead[log];
	  _fraghead[log].next = next;

	  for (i = 2; i < (size_t) (BLOCKSIZE >> log); ++i)
	    {
	      next = (struct list *) ((char *) result + (i << log));
	      next->next = _fraghead[log].next;
	      next->prev = &_fraghead[log];
	      next->prev->next = next;
	      next->next->prev = next;
	    }

	  /* Initialize the nfree and first counters for this block.  */
	  block = BLOCK (result);
	  _heapinfo[block].busy.type = log;
	  _heapinfo[block].busy.info.frag.nfree = i - 1;
	  _heapinfo[block].busy.info.frag.first = i - 1;

	  _chunks_free += (BLOCKSIZE >> log) - 1;
	  _bytes_free += BLOCKSIZE - (1 << log);
	  _bytes_used -= BLOCKSIZE - (1 << log);
	}
    }
  else
    {
      /* Large allocation to receive one or more blocks.
	 Search the free list in a circle starting at the last place visited.
	 If we loop completely around without finding a large enough
	 space we will have to get more memory from the system.  */
      blocks = BLOCKIFY (size);
      start = block = _heapindex;
      while (_heapinfo[block].free.size < blocks)
	{
	  block = _heapinfo[block].free.next;
	  if (block == start)
	    {
	      /* Need to get more from the system.  Get a little extra.  */
	      size_t wantblocks = blocks + __malloc_extra_blocks;
	      block = _heapinfo[0].free.prev;
	      lastblocks = _heapinfo[block].free.size;
	      /* Check to see if the new core will be contiguous with the
		 final free block; if so we don't need to get as much.  */
	      if (_heaplimit != 0 && block + lastblocks == _heaplimit &&
		  /* We can't do this if we will have to make the heap info
                     table bigger to accommodate the new space.  */
		  block + wantblocks <= heapsize &&
		  get_contiguous_space ((wantblocks - lastblocks) * BLOCKSIZE,
					ADDRESS (block + lastblocks)))
		{
 		  /* We got it contiguously.  Which block we are extending
		     (the `final free block' referred to above) might have
		     changed, if it got combined with a freed info table.  */
 		  block = _heapinfo[0].free.prev;
  		  _heapinfo[block].free.size += (wantblocks - lastblocks);
		  _bytes_free += (wantblocks - lastblocks) * BLOCKSIZE;
 		  _heaplimit += wantblocks - lastblocks;
		  continue;
		}
	      result = morecore_nolock (wantblocks * BLOCKSIZE);
	      if (result == NULL)
		goto out;
	      block = BLOCK (result);
	      /* Put the new block at the end of the free list.  */
	      _heapinfo[block].free.size = wantblocks;
	      _heapinfo[block].free.prev = _heapinfo[0].free.prev;
	      _heapinfo[block].free.next = 0;
	      _heapinfo[0].free.prev = block;
	      _heapinfo[_heapinfo[block].free.prev].free.next = block;
	      ++_chunks_free;
	      /* Now loop to use some of that block for this allocation.  */
	    }
	}

      /* At this point we have found a suitable free list entry.
	 Figure out how to remove what we need from the list. */
      result = ADDRESS (block);
      if (_heapinfo[block].free.size > blocks)
	{
	  /* The block we found has a bit left over,
	     so relink the tail end back into the free list. */
	  _heapinfo[block + blocks].free.size
	    = _heapinfo[block].free.size - blocks;
	  _heapinfo[block + blocks].free.next
	    = _heapinfo[block].free.next;
	  _heapinfo[block + blocks].free.prev
	    = _heapinfo[block].free.prev;
	  _heapinfo[_heapinfo[block].free.prev].free.next
	    = _heapinfo[_heapinfo[block].free.next].free.prev
	    = _heapindex = block + blocks;
	}
      else
	{
	  /* The block exactly matches our requirements,
	     so just remove it from the list. */
	  _heapinfo[_heapinfo[block].free.next].free.prev
	    = _heapinfo[block].free.prev;
	  _heapinfo[_heapinfo[block].free.prev].free.next
	    = _heapindex = _heapinfo[block].free.next;
	  --_chunks_free;
	}

      _heapinfo[block].busy.type = -1;
      _heapinfo[block].busy.info.size = blocks;
      ++_chunks_used;
      _bytes_used += blocks * BLOCKSIZE;
      _bytes_free -= blocks * BLOCKSIZE;
    }

  PROTECT_MALLOC_STATE (1);
 out:
  return result;
}

void *
_malloc_internal (size_t size)
{
  void *result;

  LOCK ();
  result = _malloc_internal_nolock (size);
  UNLOCK ();

  return result;
}

void *
malloc (size_t size)
{
  void *(*hook) (size_t);

  if (!__malloc_initialized && !__malloc_initialize ())
    return NULL;

  /* Copy the value of gmalloc_hook to an automatic variable in case
     gmalloc_hook is modified in another thread between its
     NULL-check and the use.

     Note: Strictly speaking, this is not a right solution.  We should
     use mutexes to access non-read-only variables that are shared
     among multiple threads.  We just leave it for compatibility with
     glibc malloc (i.e., assignments to gmalloc_hook) for now.  */
  hook = gmalloc_hook;
  void *result = (hook ? hook : _malloc_internal) (size);
  return ptr_bounds_clip (result, size);
}

#if !(defined (_LIBC) || defined (HYBRID_MALLOC))

/* On some ANSI C systems, some libc functions call _malloc, _free
   and _realloc.  Make them use the GNU functions.  */

extern void *_malloc (size_t);
extern void _free (void *);
extern void *_realloc (void *, size_t);

void *
_malloc (size_t size)
{
  return malloc (size);
}

void
_free (void *ptr)
{
  free (ptr);
}

void *
_realloc (void *ptr, size_t size)
{
  return realloc (ptr, size);
}

#endif
/* Free a block of memory allocated by `malloc'.
   Copyright 1990, 1991, 1992, 1994, 1995 Free Software Foundation, Inc.
		  Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

/* Debugging hook for free.  */
static void (*__MALLOC_HOOK_VOLATILE gfree_hook) (void *);

#ifndef HYBRID_MALLOC

/* List of blocks allocated by aligned_alloc.  */
struct alignlist *_aligned_blocks = NULL;
#endif

/* Return memory to the heap.
   Like `_free_internal' but don't lock mutex.  */
void
_free_internal_nolock (void *ptr)
{
  int type;
  size_t block, blocks;
  register size_t i;
  struct list *prev, *next;
  void *curbrk;
  const size_t lesscore_threshold
    /* Threshold of free space at which we will return some to the system.  */
    = FINAL_FREE_BLOCKS + 2 * __malloc_extra_blocks;

  register struct alignlist *l;

  if (ptr == NULL)
    return;
  ptr = ptr_bounds_init (ptr);

  PROTECT_MALLOC_STATE (0);

  LOCK_ALIGNED_BLOCKS ();
  for (l = _aligned_blocks; l != NULL; l = l->next)
    if (l->aligned == ptr)
      {
	l->aligned = NULL;	/* Mark the slot in the list as free.  */
	ptr = l->exact;
	break;
      }
  UNLOCK_ALIGNED_BLOCKS ();

  block = BLOCK (ptr);

  type = _heapinfo[block].busy.type;
  switch (type)
    {
    case -1:
      /* Get as many statistics as early as we can.  */
      --_chunks_used;
      _bytes_used -= _heapinfo[block].busy.info.size * BLOCKSIZE;
      _bytes_free += _heapinfo[block].busy.info.size * BLOCKSIZE;

      /* Find the free cluster previous to this one in the free list.
	 Start searching at the last block referenced; this may benefit
	 programs with locality of allocation.  */
      i = _heapindex;
      if (i > block)
	while (i > block)
	  i = _heapinfo[i].free.prev;
      else
	{
	  do
	    i = _heapinfo[i].free.next;
	  while (i > 0 && i < block);
	  i = _heapinfo[i].free.prev;
	}

      /* Determine how to link this block into the free list.  */
      if (block == i + _heapinfo[i].free.size)
	{
	  /* Coalesce this block with its predecessor.  */
	  _heapinfo[i].free.size += _heapinfo[block].busy.info.size;
	  block = i;
	}
      else
	{
	  /* Really link this block back into the free list.  */
	  _heapinfo[block].free.size = _heapinfo[block].busy.info.size;
	  _heapinfo[block].free.next = _heapinfo[i].free.next;
	  _heapinfo[block].free.prev = i;
	  _heapinfo[i].free.next = block;
	  _heapinfo[_heapinfo[block].free.next].free.prev = block;
	  ++_chunks_free;
	}

      /* Now that the block is linked in, see if we can coalesce it
	 with its successor (by deleting its successor from the list
	 and adding in its size).  */
      if (block + _heapinfo[block].free.size == _heapinfo[block].free.next)
	{
	  _heapinfo[block].free.size
	    += _heapinfo[_heapinfo[block].free.next].free.size;
	  _heapinfo[block].free.next
	    = _heapinfo[_heapinfo[block].free.next].free.next;
	  _heapinfo[_heapinfo[block].free.next].free.prev = block;
	  --_chunks_free;
	}

      /* How many trailing free blocks are there now?  */
      blocks = _heapinfo[block].free.size;

      /* Where is the current end of accessible core?  */
      curbrk = (*__morecore) (0);

      if (_heaplimit != 0 && curbrk == ADDRESS (_heaplimit))
	{
	  /* The end of the malloc heap is at the end of accessible core.
	     It's possible that moving _heapinfo will allow us to
	     return some space to the system.  */

 	  size_t info_block = BLOCK (_heapinfo);
 	  size_t info_blocks = _heapinfo[info_block].busy.info.size;
 	  size_t prev_block = _heapinfo[block].free.prev;
 	  size_t prev_blocks = _heapinfo[prev_block].free.size;
 	  size_t next_block = _heapinfo[block].free.next;
 	  size_t next_blocks = _heapinfo[next_block].free.size;

	  if (/* Win if this block being freed is last in core, the info table
		 is just before it, the previous free block is just before the
		 info table, and the two free blocks together form a useful
		 amount to return to the system.  */
	      (block + blocks == _heaplimit &&
	       info_block + info_blocks == block &&
	       prev_block != 0 && prev_block + prev_blocks == info_block &&
	       blocks + prev_blocks >= lesscore_threshold) ||
	      /* Nope, not the case.  We can also win if this block being
		 freed is just before the info table, and the table extends
		 to the end of core or is followed only by a free block,
		 and the total free space is worth returning to the system.  */
	      (block + blocks == info_block &&
	       ((info_block + info_blocks == _heaplimit &&
		 blocks >= lesscore_threshold) ||
		(info_block + info_blocks == next_block &&
		 next_block + next_blocks == _heaplimit &&
		 blocks + next_blocks >= lesscore_threshold)))
	      )
	    {
	      malloc_info *newinfo;
	      size_t oldlimit = _heaplimit;

	      /* Free the old info table, clearing _heaplimit to avoid
		 recursion into this code.  We don't want to return the
		 table's blocks to the system before we have copied them to
		 the new location.  */
	      _heaplimit = 0;
	      _free_internal_nolock (_heapinfo);
	      _heaplimit = oldlimit;

	      /* Tell malloc to search from the beginning of the heap for
		 free blocks, so it doesn't reuse the ones just freed.  */
	      _heapindex = 0;

	      /* Allocate new space for the info table and move its data.  */
	      newinfo = _malloc_internal_nolock (info_blocks * BLOCKSIZE);
	      PROTECT_MALLOC_STATE (0);
	      memmove (newinfo, _heapinfo, info_blocks * BLOCKSIZE);
	      _heapinfo = newinfo;

	      /* We should now have coalesced the free block with the
		 blocks freed from the old info table.  Examine the entire
		 trailing free block to decide below whether to return some
		 to the system.  */
	      block = _heapinfo[0].free.prev;
	      blocks = _heapinfo[block].free.size;
 	    }

	  /* Now see if we can return stuff to the system.  */
	  if (block + blocks == _heaplimit && blocks >= lesscore_threshold)
	    {
	      register size_t bytes = blocks * BLOCKSIZE;
	      _heaplimit -= blocks;
	      (*__morecore) (-bytes);
	      _heapinfo[_heapinfo[block].free.prev].free.next
		= _heapinfo[block].free.next;
	      _heapinfo[_heapinfo[block].free.next].free.prev
		= _heapinfo[block].free.prev;
	      block = _heapinfo[block].free.prev;
	      --_chunks_free;
	      _bytes_free -= bytes;
	    }
	}

      /* Set the next search to begin at this block.  */
      _heapindex = block;
      break;

    default:
      /* Do some of the statistics.  */
      --_chunks_used;
      _bytes_used -= 1 << type;
      ++_chunks_free;
      _bytes_free += 1 << type;

      /* Get the address of the first free fragment in this block.  */
      prev = (struct list *) ((char *) ADDRESS (block) +
			      (_heapinfo[block].busy.info.frag.first << type));

      if (_heapinfo[block].busy.info.frag.nfree == (BLOCKSIZE >> type) - 1)
	{
	  /* If all fragments of this block are free, remove them
	     from the fragment list and free the whole block.  */
	  next = prev;
	  for (i = 1; i < (size_t) (BLOCKSIZE >> type); ++i)
	    next = next->next;
	  prev->prev->next = next;
	  if (next != NULL)
	    next->prev = prev->prev;
	  _heapinfo[block].busy.type = -1;
	  _heapinfo[block].busy.info.size = 1;

	  /* Keep the statistics accurate.  */
	  ++_chunks_used;
	  _bytes_used += BLOCKSIZE;
	  _chunks_free -= BLOCKSIZE >> type;
	  _bytes_free -= BLOCKSIZE;

#if defined (GC_MALLOC_CHECK) || defined (USE_PTHREAD)
	  _free_internal_nolock (ADDRESS (block));
#else
	  free (ADDRESS (block));
#endif
	}
      else if (_heapinfo[block].busy.info.frag.nfree != 0)
	{
	  /* If some fragments of this block are free, link this
	     fragment into the fragment list after the first free
	     fragment of this block. */
	  next = ptr;
	  next->next = prev->next;
	  next->prev = prev;
	  prev->next = next;
	  if (next->next != NULL)
	    next->next->prev = next;
	  ++_heapinfo[block].busy.info.frag.nfree;
	}
      else
	{
	  /* No fragments of this block are free, so link this
	     fragment into the fragment list and announce that
	     it is the first free fragment of this block. */
	  prev = ptr;
	  _heapinfo[block].busy.info.frag.nfree = 1;
	  _heapinfo[block].busy.info.frag.first =
	    (uintptr_t) ptr % BLOCKSIZE >> type;
	  prev->next = _fraghead[type].next;
	  prev->prev = &_fraghead[type];
	  prev->prev->next = prev;
	  if (prev->next != NULL)
	    prev->next->prev = prev;
	}
      break;
    }

  PROTECT_MALLOC_STATE (1);
}

/* Return memory to the heap.
   Like 'free' but don't call a hook if there is one.  */
void
_free_internal (void *ptr)
{
  LOCK ();
  _free_internal_nolock (ptr);
  UNLOCK ();
}

/* Return memory to the heap.  */

void
free (void *ptr)
{
  void (*hook) (void *) = gfree_hook;

  if (hook != NULL)
    (*hook) (ptr);
  else
    _free_internal (ptr);
}

#ifndef HYBRID_MALLOC
/* Define the `cfree' alias for `free'.  */
#ifdef weak_alias
weak_alias (free, cfree)
#else
void
cfree (void *ptr)
{
  free (ptr);
}
#endif
#endif
/* Change the size of a block allocated by `malloc'.
   Copyright 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
		     Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

/* Debugging hook for realloc.  */
static void *(*grealloc_hook) (void *, size_t);

/* Resize the given region to the new size, returning a pointer
   to the (possibly moved) region.  This is optimized for speed;
   some benchmarks seem to indicate that greater compactness is
   achieved by unconditionally allocating and copying to a
   new region.  This module has incestuous knowledge of the
   internals of both free and malloc. */
void *
_realloc_internal_nolock (void *ptr, size_t size)
{
  void *result;
  int type;
  size_t block, blocks, oldlimit;

  if (size == 0)
    {
      _free_internal_nolock (ptr);
      return _malloc_internal_nolock (0);
    }
  else if (ptr == NULL)
    return _malloc_internal_nolock (size);

  ptr = ptr_bounds_init (ptr);
  block = BLOCK (ptr);

  PROTECT_MALLOC_STATE (0);

  type = _heapinfo[block].busy.type;
  switch (type)
    {
    case -1:
      /* Maybe reallocate a large block to a small fragment.  */
      if (size <= BLOCKSIZE / 2)
	{
	  result = _malloc_internal_nolock (size);
	  if (result != NULL)
	    {
	      memcpy (result, ptr, size);
	      _free_internal_nolock (ptr);
	      goto out;
	    }
	}

      /* The new size is a large allocation as well;
	 see if we can hold it in place. */
      blocks = BLOCKIFY (size);
      if (blocks < _heapinfo[block].busy.info.size)
	{
	  /* The new size is smaller; return
	     excess memory to the free list. */
	  _heapinfo[block + blocks].busy.type = -1;
	  _heapinfo[block + blocks].busy.info.size
	    = _heapinfo[block].busy.info.size - blocks;
	  _heapinfo[block].busy.info.size = blocks;
	  /* We have just created a new chunk by splitting a chunk in two.
	     Now we will free this chunk; increment the statistics counter
	     so it doesn't become wrong when _free_internal decrements it.  */
	  ++_chunks_used;
	  _free_internal_nolock (ADDRESS (block + blocks));
	  result = ptr;
	}
      else if (blocks == _heapinfo[block].busy.info.size)
	/* No size change necessary.  */
	result = ptr;
      else
	{
	  /* Won't fit, so allocate a new region that will.
	     Free the old region first in case there is sufficient
	     adjacent free space to grow without moving. */
	  blocks = _heapinfo[block].busy.info.size;
	  /* Prevent free from actually returning memory to the system.  */
	  oldlimit = _heaplimit;
	  _heaplimit = 0;
	  _free_internal_nolock (ptr);
	  result = _malloc_internal_nolock (size);
	  PROTECT_MALLOC_STATE (0);
	  if (_heaplimit == 0)
	    _heaplimit = oldlimit;
	  if (result == NULL)
	    {
	      /* Now we're really in trouble.  We have to unfree
		 the thing we just freed.  Unfortunately it might
		 have been coalesced with its neighbors.  */
	      if (_heapindex == block)
	        (void) _malloc_internal_nolock (blocks * BLOCKSIZE);
	      else
		{
		  void *previous
		    = _malloc_internal_nolock ((block - _heapindex) * BLOCKSIZE);
		  (void) _malloc_internal_nolock (blocks * BLOCKSIZE);
		  _free_internal_nolock (previous);
		}
	      goto out;
	    }
	  if (ptr != result)
	    memmove (result, ptr, blocks * BLOCKSIZE);
	}
      break;

    default:
      /* Old size is a fragment; type is logarithm
	 to base two of the fragment size.  */
      if (size > (size_t) (1 << (type - 1)) &&
	  size <= (size_t) (1 << type))
	/* The new size is the same kind of fragment.  */
	result = ptr;
      else
	{
	  /* The new size is different; allocate a new space,
	     and copy the lesser of the new size and the old. */
	  result = _malloc_internal_nolock (size);
	  if (result == NULL)
	    goto out;
	  memcpy (result, ptr, min (size, (size_t) 1 << type));
	  _free_internal_nolock (ptr);
	}
      break;
    }

  PROTECT_MALLOC_STATE (1);
 out:
  return result;
}

void *
_realloc_internal (void *ptr, size_t size)
{
  void *result;

  LOCK ();
  result = _realloc_internal_nolock (ptr, size);
  UNLOCK ();

  return result;
}

void *
realloc (void *ptr, size_t size)
{
  void *(*hook) (void *, size_t);

  if (!__malloc_initialized && !__malloc_initialize ())
    return NULL;

  hook = grealloc_hook;
  void *result = (hook ? hook : _realloc_internal) (ptr, size);
  return ptr_bounds_clip (result, size);
}
/* Copyright (C) 1991, 1992, 1994 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

/* Allocate an array of NMEMB elements each SIZE bytes long.
   The entire array is initialized to zeros.  */
void *
calloc (size_t nmemb, size_t size)
{
  void *result;
  size_t bytes = nmemb * size;

  if (size != 0 && bytes / size != nmemb)
    {
      errno = ENOMEM;
      return NULL;
    }

  result = malloc (bytes);
  if (result)
    return memset (result, 0, bytes);
  return result;
}
/* Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU C Library.  If not, see <https://www.gnu.org/licenses/>.  */

/* uClibc defines __GNU_LIBRARY__, but it is not completely
   compatible.  */
#if !defined (__GNU_LIBRARY__) || defined (__UCLIBC__)
#define	__sbrk	sbrk
#else /* __GNU_LIBRARY__ && ! defined (__UCLIBC__) */
/* It is best not to declare this and cast its result on foreign operating
   systems with potentially hostile include files.  */

extern void *__sbrk (ptrdiff_t increment);
#endif /* __GNU_LIBRARY__ && ! defined (__UCLIBC__) */

/* Allocate INCREMENT more bytes of data space,
   and return the start of data space, or NULL on errors.
   If INCREMENT is negative, shrink data space.  */
static void *
gdefault_morecore (ptrdiff_t increment)
{
#ifdef HYBRID_MALLOC
  if (!DUMPED)
    {
      return bss_sbrk (increment);
    }
#endif
#ifdef HAVE_SBRK
  void *result = (void *) __sbrk (increment);
  if (result != (void *) -1)
    return result;
#endif
  return NULL;
}

void *(*__morecore) (ptrdiff_t) = gdefault_morecore;

/* Copyright (C) 1991, 92, 93, 94, 95, 96 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.  */

void *
aligned_alloc (size_t alignment, size_t size)
{
  void *result;
  size_t adj, lastadj;

  /* Allocate a block with enough extra space to pad the block with up to
     (ALIGNMENT - 1) bytes if necessary.  */
  if (- size < alignment)
    {
      errno = ENOMEM;
      return NULL;
    }
  result = malloc (size + alignment - 1);
  if (result == NULL)
    return NULL;

  /* Figure out how much we will need to pad this particular block
     to achieve the required alignment.  */
  adj = alignment - (uintptr_t) result % alignment;
  if (adj == alignment)
    adj = 0;

  if (adj != alignment - 1)
    {
      do
	{
	  /* Reallocate the block with only as much excess as it
	     needs.  */
	  free (result);
	  result = malloc (size + adj);
	  if (result == NULL)	/* Impossible unless interrupted.  */
	    return NULL;

	  lastadj = adj;
	  adj = alignment - (uintptr_t) result % alignment;
	  if (adj == alignment)
	    adj = 0;
	  /* It's conceivable we might have been so unlucky as to get
	     a different block with weaker alignment.  If so, this
	     block is too short to contain SIZE after alignment
	     correction.  So we must try again and get another block,
	     slightly larger.  */
	} while (adj > lastadj);
    }

  if (adj != 0)
    {
      /* Record this block in the list of aligned blocks, so that `free'
	 can identify the pointer it is passed, which will be in the middle
	 of an allocated block.  */

      struct alignlist *l;
      LOCK_ALIGNED_BLOCKS ();
      for (l = _aligned_blocks; l != NULL; l = l->next)
	if (l->aligned == NULL)
	  /* This slot is free.  Use it.  */
	  break;
      if (l == NULL)
	{
	  l = malloc (sizeof *l);
	  if (l != NULL)
	    {
	      l->next = _aligned_blocks;
	      _aligned_blocks = l;
	    }
	}
      if (l != NULL)
	{
	  l->exact = result;
	  result = l->aligned = (char *) result + adj;
	  result = ptr_bounds_clip (result, size);
	}
      UNLOCK_ALIGNED_BLOCKS ();
      if (l == NULL)
	{
	  free (result);
	  result = NULL;
	}
    }

  return result;
}

/* Note that memalign and posix_memalign are not used in Emacs.  */
#ifndef HYBRID_MALLOC
/* An obsolete alias for aligned_alloc, for any old libraries that use
   this alias.  */

void *
memalign (size_t alignment, size_t size)
{
  return aligned_alloc (alignment, size);
}

/* If HYBRID_MALLOC is defined, we may want to use the system
   posix_memalign below.  */
int
posix_memalign (void **memptr, size_t alignment, size_t size)
{
  void *mem;

  if (alignment == 0
      || alignment % sizeof (void *) != 0
      || (alignment & (alignment - 1)) != 0)
    return EINVAL;

  mem = aligned_alloc (alignment, size);
  if (mem == NULL)
    return ENOMEM;

  *memptr = mem;

  return 0;
}
#endif

/* Allocate memory on a page boundary.
   Copyright (C) 1991, 92, 93, 94, 96 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#ifndef HYBRID_MALLOC

# ifndef HAVE_MALLOC_H
/* Allocate SIZE bytes on a page boundary.  */
extern void *valloc (size_t);
# endif

# if defined _SC_PAGESIZE || !defined HAVE_GETPAGESIZE
#  include "getpagesize.h"
# elif !defined getpagesize
extern int getpagesize (void);
# endif

static size_t pagesize;

void *
valloc (size_t size)
{
  if (pagesize == 0)
    pagesize = getpagesize ();

  return aligned_alloc (pagesize, size);
}
#endif /* HYBRID_MALLOC */

#undef malloc
#undef realloc
#undef calloc
#undef aligned_alloc
#undef free

#ifdef HYBRID_MALLOC
/* Declare system malloc and friends.  */
extern void *malloc (size_t size);
extern void *realloc (void *ptr, size_t size);
extern void *calloc (size_t nmemb, size_t size);
extern void free (void *ptr);
#ifdef HAVE_ALIGNED_ALLOC
extern void *aligned_alloc (size_t alignment, size_t size);
#elif defined HAVE_POSIX_MEMALIGN
extern int posix_memalign (void **memptr, size_t alignment, size_t size);
#endif

/* Assuming PTR was allocated via the hybrid malloc, return true if
   PTR was allocated via gmalloc, not the system malloc.  Also, return
   true if _heaplimit is zero; this can happen temporarily when
   gmalloc calls itself for internal use, and in that case PTR is
   already known to be allocated via gmalloc.  */

static bool
allocated_via_gmalloc (void *ptr)
{
  size_t block = BLOCK (ptr);
  size_t blockmax = _heaplimit - 1;
  return block <= blockmax && _heapinfo[block].busy.type != 0;
}

/* See the comments near the beginning of this file for explanations
   of the following functions. */

void *
hybrid_malloc (size_t size)
{
  if (DUMPED)
    return malloc (size);
  return gmalloc (size);
}

void *
hybrid_calloc (size_t nmemb, size_t size)
{
  if (DUMPED)
    return calloc (nmemb, size);
  return gcalloc (nmemb, size);
}

void
hybrid_free (void *ptr)
{
  if (allocated_via_gmalloc (ptr))
    gfree (ptr);
  else
    free (ptr);
}

#if defined HAVE_ALIGNED_ALLOC || defined HAVE_POSIX_MEMALIGN
void *
hybrid_aligned_alloc (size_t alignment, size_t size)
{
  if (!DUMPED)
    return galigned_alloc (alignment, size);
  /* The following is copied from alloc.c */
#ifdef HAVE_ALIGNED_ALLOC
  return aligned_alloc (alignment, size);
#else  /* HAVE_POSIX_MEMALIGN */
  void *p;
  return posix_memalign (&p, alignment, size) == 0 ? p : 0;
#endif
}
#endif

void *
hybrid_realloc (void *ptr, size_t size)
{
  void *result;
  int type;
  size_t block, oldsize;

  if (!ptr)
    return hybrid_malloc (size);
  if (!allocated_via_gmalloc (ptr))
    return realloc (ptr, size);
  if (!DUMPED)
    return grealloc (ptr, size);

  /* The dumped emacs is trying to realloc storage allocated before
     dumping via gmalloc.  Allocate new space and copy the data.  Do
     not bother with gfree (ptr), as that would just waste time.  */
  block = BLOCK (ptr);
  type = _heapinfo[block].busy.type;
  oldsize =
    type < 0 ? _heapinfo[block].busy.info.size * BLOCKSIZE
    : (size_t) 1 << type;
  result = malloc (size);
  if (result)
    return memcpy (result, ptr, min (oldsize, size));
  return result;
}

#else	/* ! HYBRID_MALLOC */

void *
malloc (size_t size)
{
  return gmalloc (size);
}

void *
calloc (size_t nmemb, size_t size)
{
  return gcalloc (nmemb, size);
}

void
free (void *ptr)
{
  gfree (ptr);
}

void *
aligned_alloc (size_t alignment, size_t size)
{
  return galigned_alloc (alignment, size);
}

void *
realloc (void *ptr, size_t size)
{
  return grealloc (ptr, size);
}

#endif	/* HYBRID_MALLOC */

#ifdef GC_MCHECK

/* Standard debugging hooks for `malloc'.
   Copyright 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
   Written May 1989 by Mike Haertel.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library.  If not, see <https://www.gnu.org/licenses/>.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation.  */

#include <stdio.h>

/* Old hook values.  */
static void (*old_free_hook) (void *ptr);
static void *(*old_malloc_hook) (size_t size);
static void *(*old_realloc_hook) (void *ptr, size_t size);

/* Function to call when something awful happens.  */
static void (*abortfunc) (enum mcheck_status);

/* Arbitrary magical numbers.  */
#define MAGICWORD	(SIZE_MAX / 11 ^ SIZE_MAX / 13 << 3)
#define MAGICFREE	(SIZE_MAX / 17 ^ SIZE_MAX / 19 << 4)
#define MAGICBYTE	((char) 0xd7)
#define MALLOCFLOOD	((char) 0x93)
#define FREEFLOOD	((char) 0x95)

struct hdr
  {
    size_t size;	/* Exact size requested by user.  */
    size_t magic;	/* Magic number to check header integrity.  */
  };

static enum mcheck_status
checkhdr (const struct hdr *hdr)
{
  enum mcheck_status status;
  switch (hdr->magic)
    {
    default:
      status = MCHECK_HEAD;
      break;
    case MAGICFREE:
      status = MCHECK_FREE;
      break;
    case MAGICWORD:
      if (((char *) &hdr[1])[hdr->size] != MAGICBYTE)
	status = MCHECK_TAIL;
      else
	status = MCHECK_OK;
      break;
    }
  if (status != MCHECK_OK)
    (*abortfunc) (status);
  return status;
}

static void
freehook (void *ptr)
{
  struct hdr *hdr;

  if (ptr)
    {
      struct alignlist *l;

      /* If the block was allocated by aligned_alloc, its real pointer
	 to free is recorded in _aligned_blocks; find that.  */
      PROTECT_MALLOC_STATE (0);
      LOCK_ALIGNED_BLOCKS ();
      for (l = _aligned_blocks; l != NULL; l = l->next)
	if (l->aligned == ptr)
	  {
	    l->aligned = NULL;	/* Mark the slot in the list as free.  */
	    ptr = l->exact;
	    break;
	  }
      UNLOCK_ALIGNED_BLOCKS ();
      PROTECT_MALLOC_STATE (1);

      hdr = ((struct hdr *) ptr) - 1;
      checkhdr (hdr);
      hdr->magic = MAGICFREE;
      memset (ptr, FREEFLOOD, hdr->size);
    }
  else
    hdr = NULL;

  gfree_hook = old_free_hook;
  free (hdr);
  gfree_hook = freehook;
}

static void *
mallochook (size_t size)
{
  struct hdr *hdr;

  gmalloc_hook = old_malloc_hook;
  hdr = malloc (sizeof *hdr + size + 1);
  gmalloc_hook = mallochook;
  if (hdr == NULL)
    return NULL;

  hdr->size = size;
  hdr->magic = MAGICWORD;
  ((char *) &hdr[1])[size] = MAGICBYTE;
  return memset (hdr + 1, MALLOCFLOOD, size);
}

static void *
reallochook (void *ptr, size_t size)
{
  struct hdr *hdr = NULL;
  size_t osize = 0;

  if (ptr)
    {
      hdr = ((struct hdr *) ptr) - 1;
      osize = hdr->size;

      checkhdr (hdr);
      if (size < osize)
	memset ((char *) ptr + size, FREEFLOOD, osize - size);
    }

  gfree_hook = old_free_hook;
  gmalloc_hook = old_malloc_hook;
  grealloc_hook = old_realloc_hook;
  hdr = realloc (hdr, sizeof *hdr + size + 1);
  gfree_hook = freehook;
  gmalloc_hook = mallochook;
  grealloc_hook = reallochook;
  if (hdr == NULL)
    return NULL;

  hdr->size = size;
  hdr->magic = MAGICWORD;
  ((char *) &hdr[1])[size] = MAGICBYTE;
  if (size > osize)
    memset ((char *) (hdr + 1) + osize, MALLOCFLOOD, size - osize);
  return hdr + 1;
}

static void
mabort (enum mcheck_status status)
{
  const char *msg;
  switch (status)
    {
    case MCHECK_OK:
      msg = "memory is consistent, library is buggy";
      break;
    case MCHECK_HEAD:
      msg = "memory clobbered before allocated block";
      break;
    case MCHECK_TAIL:
      msg = "memory clobbered past end of allocated block";
      break;
    case MCHECK_FREE:
      msg = "block freed twice";
      break;
    default:
      msg = "bogus mcheck_status, library is buggy";
      break;
    }
#ifdef __GNU_LIBRARY__
  __libc_fatal (msg);
#else
  fprintf (stderr, "mcheck: %s\n", msg);
  fflush (stderr);
# ifdef emacs
  emacs_abort ();
# else
  abort ();
# endif
#endif
}

static int mcheck_used = 0;

int
mcheck (void (*func) (enum mcheck_status))
{
  abortfunc = (func != NULL) ? func : &mabort;

  /* These hooks may not be safely inserted if malloc is already in use.  */
  if (!__malloc_initialized && !mcheck_used)
    {
      old_free_hook = gfree_hook;
      gfree_hook = freehook;
      old_malloc_hook = gmalloc_hook;
      gmalloc_hook = mallochook;
      old_realloc_hook = grealloc_hook;
      grealloc_hook = reallochook;
      mcheck_used = 1;
    }

  return mcheck_used ? 0 : -1;
}

enum mcheck_status
mprobe (void *ptr)
{
  return mcheck_used ? checkhdr (ptr) : MCHECK_DISABLED;
}

#endif /* GC_MCHECK */
