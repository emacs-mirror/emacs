/* Block-relocating memory allocator. 
   Copyright (C) 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This package works by allocating blocks from a zone of memory
   above that used by malloc ().  When malloc needs more space that
   would enter our zone, we relocate blocks upward.  The bottom of
   our zone is kept in the variable `virtual_break_value'.  The top
   of our zone is indicated by `real_break_value'.

   As blocks are freed, a free list is maintained and we attempt
   to satisfy further requests for space using a first-fit policy.
   If there are holes, but none fit, memory is compacted and a new
   block is obtained at the top of the zone.

   NOTE that our blocks are always rounded to page boundaries. */

/*
   NOTES:

   Once this is stable, I can speed things up by intially leaving a large
   gap between real_break_value and true_break_value, or maybe making
   a large hole before the first block.

   If we also kept track of size_wanted, we could gain some
   extra space upon compactification.

   Perhaps we should just note a hole when malloc does doing sbrk(-n)?

   Relocating downward upon freeing the first block would simplify
   other things.

   When r_alloc places a block in a hole, we could easily check if there's
   much more than required, and leave a hole.
 */

#include "mem_limits.h"

static POINTER r_alloc_sbrk ();
static POINTER sbrk ();
static POINTER brk ();

/* Variable `malloc' uses for the function which gets more space
   from the system.  */
extern POINTER (*__morecore) ();

/* List of variables which point into the associated data block. */
struct other_pointer
{
  POINTER *location;
  struct other_pointer *next;
};

/* List describing all the user's pointers to relocatable blocks.  */
typedef struct rel_pointers
{
  struct rel_pointers *next;
  struct rel_pointers *prev;
  struct other_pointer *others;  /* Other variables which use this block. */
  POINTER *location;		  /* Location of the block's pointer. */
  POINTER block;		  /* Address of the actual data. */
  int size;			  /* The size of the block.  */
} relocatable_pointer; 

#define REL_NIL ((struct rel_pointers *) 0)

static relocatable_pointer *pointer_list;
static relocatable_pointer *last_pointer;

#define MAX_HOLES 2

/* Vector of available holes among allocated blocks.  This can include
   a hole at the beginning of the list, but never the end. */
typedef struct
{
  POINTER address;
  unsigned int size;
} hole_descriptor;

static hole_descriptor r_alloc_holes[MAX_HOLES];

/* Number of holes currently available. */
static int holes;

/* The process break value (i.e., curbrk) */
static POINTER real_break_value;

/* The REAL (i.e., page aligned) break value. */
static POINTER true_break_value;

/* Address of start of data space in use by relocatable blocks.
   This is what `malloc' thinks is the process break value. */
static POINTER virtual_break_value;

/* Nonzero if we have told `malloc' to start using `r_alloc_sbrk'
   instead of calling `sbrk' directly.  */
int r_alloc_in_use;

#define PAGE (getpagesize ())
#define ALIGNED(addr) (((unsigned int) (addr) & (PAGE - 1)) == 0)
#define ROUNDUP(size) (((unsigned int) (size) + PAGE) & ~(PAGE - 1))

/*
  Level number of warnings already issued.
  0 -- no warnings issued.
  1 -- 75% warning already issued.
  2 -- 85% warning already issued.
*/
static int warnlevel;

/* Function to call to issue a warning;
   0 means don't issue them.  */
static void (*warnfunction) ();

/* Call this to start things off.  It determines the current process
   break value, as well as the `true' break value--because the system
   allocates memory in page increments, if the break value is not page
   aligned it means that space up to the next page boundary is actually
   available. */

void
malloc_init (start, warn_func)
     POINTER start;
     void (*warn_func) ();
{
  r_alloc_in_use = 1;
  __morecore = r_alloc_sbrk;

  virtual_break_value = real_break_value = sbrk (0);
  if (ALIGNED (real_break_value))
    true_break_value = real_break_value;
  else
    true_break_value = (POINTER) ROUNDUP (real_break_value);

  if (start)
    data_space_start = start;
  lim_data = 0;
  warnlevel = 0;
  warnfunction = warn_func;

  get_lim_data ();
}

/* Get more space for us to use.  Return a pointer to SIZE more
   bytes of space.  SIZE is internally rounded up to a page boundary,
   and requests for integral pages prefetch an extra page. */

static POINTER
get_more_space (size)
     unsigned int size;
{
  unsigned int margin = true_break_value - real_break_value;
  unsigned int get;
  POINTER old_break = real_break_value;

  if (size == 0)
    return real_break_value;

  if (size <= margin)
    {
      real_break_value += size;
      return old_break;
    }

  get = ROUNDUP (size - margin);
  if (sbrk (get) < (POINTER) 0)
    return NULL;

  true_break_value += get;
  real_break_value = (old_break + size);

  return old_break;
}

/* Relinquish size bytes of space to the system.  Space is only returned
   in page increments.  If successful, return real_break_value. */

static POINTER
return_space (size)
     unsigned int size;
{
  unsigned int margin = (true_break_value - real_break_value) + size;
  unsigned int to_return = (margin / PAGE) * PAGE;
  unsigned new_margin = margin % PAGE;

  true_break_value -= to_return;
  if (! brk (true_break_value))
    return NULL;

  real_break_value = true_break_value - new_margin;
  return real_break_value;
}

/* Record a new hole in memory beginning at ADDRESS of size SIZE.
   Holes are ordered by location.   Adjacent holes are merged.
   Holes are zero filled before being noted. */

static void
note_hole (address, size)
     POINTER address;
     int size;
{
  register int this_hole = holes - 1;    /* Start at the last hole. */
  register POINTER end = address + size; /* End of the hole. */
  register int i;

  if (holes)
    {
      /* Find the hole which should precede this new one. */
      while (this_hole >= 0 && r_alloc_holes[this_hole].address > address)
	this_hole--;

       /* Can we merge with preceding? */
      if (this_hole >= 0
	  && r_alloc_holes[this_hole].address + r_alloc_holes[this_hole].size
	     == address)
	{
	  r_alloc_holes[this_hole].size += size;

	  if (this_hole == holes - 1)
	    return;

	  /* Can we also merge with following? */
	  if (end == r_alloc_holes[this_hole + 1].address)
	    {
	      r_alloc_holes[this_hole].size
		+= r_alloc_holes[this_hole + 1].size;

	      for (i = this_hole + 1; i < holes - 1; i++)
		r_alloc_holes[i] = r_alloc_holes[i + 1];
	      holes--;
	    }

	  return;
	}

      if (this_hole < holes - 1) /* there are following holes */
	{
	  register int next_hole = this_hole + 1;

	  /* Can we merge with the next hole? */
	  if (end == r_alloc_holes[next_hole].address)
	    {
	      r_alloc_holes[next_hole].address = address;
	      r_alloc_holes[next_hole].size += size;
	      return;
	    }

	  /* Can't merge, so insert. */
	  for (i = holes; i > next_hole; i--)
	    r_alloc_holes[i] = r_alloc_holes[i - 1];
	  r_alloc_holes[next_hole].address = address;
	  r_alloc_holes[next_hole].size = size;
	  holes++;

	  return;
	}
      else			/* Simply add this hole at the end. */
	{
	  r_alloc_holes[holes].address = address;
	  r_alloc_holes[holes].size = size;
	  holes++;

	  return;
	}

      abort ();
    }
  else			/* Make the first hole. */
    {
      holes = 1;
      r_alloc_holes[0].address = address;
      r_alloc_holes[0].size = size;
    }
}

/* Mark hole HOLE as no longer available by re-organizing the vector.
   HOLE is the Nth hole, beginning with 0. This doesn *not* affect memory
   organization. */

static void
delete_hole (hole)
     int hole;
{
  register int i;

  for (i = hole; i < holes - 1; i++)
    r_alloc_holes[i] = r_alloc_holes[i + 1];

  holes--;
}

/* Insert a newly allocated pointer, NEW_PTR, at the appropriate
   place in our list. */

static void
insert (new_ptr)
     register relocatable_pointer *new_ptr;
{
  register relocatable_pointer *this_ptr = pointer_list;

  while (this_ptr != REL_NIL && this_ptr->block < new_ptr->block)
    this_ptr = this_ptr->next;

  if (this_ptr == REL_NIL)
    abort ();			/* Use `attach' for appending. */

  new_ptr->next = this_ptr;
  new_ptr->prev = this_ptr->prev;
  this_ptr->prev = new_ptr;

  if (this_ptr == pointer_list)
    pointer_list = new_ptr;
  else
    new_ptr->prev->next = new_ptr;
}

/* Attach a newly allocated pointer, NEW_PTR, to the end of our list. */

static void
attach (new_ptr)
     relocatable_pointer *new_ptr;
{
  if (pointer_list == REL_NIL)
    {
      pointer_list = new_ptr;
      last_pointer = new_ptr;
      new_ptr->next = new_ptr->prev = REL_NIL;
    }
  else
    {
      new_ptr->next = REL_NIL;
      last_pointer->next = new_ptr;
      new_ptr->prev = last_pointer;
      last_pointer = new_ptr;
    }
}

static relocatable_pointer *
find_block (block)
     POINTER block;
{
  register relocatable_pointer *this_ptr = pointer_list;

  while (this_ptr != REL_NIL && this_ptr->block != block)
    this_ptr = this_ptr->next;

  return this_ptr;
}

static relocatable_pointer *
find_location (address)
     POINTER *address;
{
  register relocatable_pointer *this_ptr = pointer_list;

  while (this_ptr != REL_NIL && this_ptr->location != address)
    {
      struct other_pointer *op = this_ptr->others;

      while (op != (struct other_pointer *) 0)
	{
	  if (op->location == address)
	    return this_ptr;

	  op = op->next;
	}

      this_ptr = this_ptr->next;
    }

  return this_ptr;
}


static void compactify ();

/* Record of last new block allocated. */
static relocatable_pointer *last_record;

/* Allocate a block of size SIZE and record that PTR points to it.
   If successful, store the address of the block in *PTR and return
   it as well.   Otherwise return NULL.  */

POINTER
r_alloc (ptr, size)
     POINTER *ptr;
     int size;
{
  register relocatable_pointer *record
    = (relocatable_pointer *) malloc (sizeof (relocatable_pointer));
  register POINTER block;

  /* If we can't get space to record this pointer, fail.  */
  if (record == 0)
    return NULL;

  last_record = record;

  if (holes)			/* Search for a hole the right size. */
    {
      int i;

      for (i = 0; i < holes; i++)
	if (r_alloc_holes[i].size >= size)
	  {
	    record->location = ptr;
	    record->others = (struct other_pointer *) 0;
	    record->block = *ptr = r_alloc_holes[i].address;
	    if (r_alloc_holes[i].size > ROUNDUP (size))
	      {
		record->size = ROUNDUP (size);
		r_alloc_holes[i].size -= ROUNDUP (size);
		r_alloc_holes[i].address += ROUNDUP (size);
	      }
	    else
	      {
		record->size = r_alloc_holes[i].size;
		delete_hole (i);
	      }
	    insert (record);

	    *ptr = record->block;
	    return record->block;
	  }

      /* No holes large enough.  Burp. */
      compactify ();
    }

  /* No holes: grow the process. */
  block = get_more_space (size);
  if (block == NULL)
    {
      free (record);
      return NULL;
    }

  /* Return the address of the block.  */
  *ptr = block;

  /* Record and append this pointer to our list. */
  record->location = ptr;
  record->others = (struct other_pointer *) 0;
  record->block = block;
  record->size = size;
  attach (record);

  return block;
}

/* Declare VAR to be a pointer which points into the block of r_alloc'd
   memory at BLOCK.

   If VAR is already delcared for this block, simply return.
   If VAR currently points to some other block, remove that declaration
   of it, then install the new one.

   Return 0 if successful, -1 otherwise. */

int
r_alloc_declare (var, block)
     POINTER *var;
     register POINTER block;
{
  register relocatable_pointer *block_ptr = find_block (block);
  relocatable_pointer *var_ptr = find_location (var);
  register struct other_pointer *other;

  if (block_ptr == REL_NIL)
    abort ();

  if (var_ptr != REL_NIL)	/* Var already declared somewhere. */
    {
      register struct other_pointer *po;

      if (var_ptr == block_ptr) /* Var already points to this block. */
	return 0;

      po = (struct other_pointer *) 0;
      other = var_ptr->others;
      while (other && other->location != var)
	{
	  po = other;
	  other = other->next;
	}

      if (!other)		/* This only happens if the location is */
	abort ();		/* the main pointer and not an `other' */

      if (po)			/* In the chain */
	{
	  po->next = other->next;
	  free (other);
	}
      else			/* Only element of the chain */
	{
	  free (var_ptr->others);
	  var_ptr->others = (struct other_pointer *) 0;
	}
    }

  /* Install this variable as an `other' element */

  other = (struct other_pointer *) malloc (sizeof (struct other_pointer));

  if (other == 0)
    return -1;

  /* If the malloc relocated this data block, adjust this variable. */
  if (block != block_ptr->block)
    {
      int offset = block_ptr->block - block;

      *var += offset;
    }

  other->location = var;
  other->next = (struct other_pointer *) 0;

  if (block_ptr->others == (struct other_pointer *) 0)
    block_ptr->others = other;
  else
    {
      register struct other_pointer *op = block_ptr->others;

      while (op->next != (struct other_pointer *) 0)
	op = op->next;
      op->next = other;
    }

  return 0;
}

/* Recursively free the linked list of `other' pointers to a block. */

static void
free_others (another)
     struct other_pointer *another;
{
  if (another == (struct other_pointer *) 0)
    return;

  free_others (another->next);
  free (another);
}

/* Remove the element pointed to by PTR from the doubly linked list.
   Record the newly freed space in `holes', unless it was at the end,
   in which case return that space to the system.  Return 0 if successful,
   -1 otherwise. */

int
r_alloc_free (ptr)
     register POINTER *ptr;
{
  register relocatable_pointer *this_ptr = find_block (*ptr);

  if (this_ptr == REL_NIL)
    return -1;
  else
    {
      register relocatable_pointer *prev = this_ptr->prev;
      register relocatable_pointer *next = this_ptr->next;
      if (next && prev)		/* Somewhere in the middle */
	{
	  next->prev = prev;
	  prev->next = next;
	}
      else if (prev)		/* Last block */
	{
	  prev->next = REL_NIL;
	  last_pointer = prev;
	  return_space (this_ptr->size);
	  free_others (this_ptr->others);
	  free (this_ptr);

	  return 0;
	}
      else if (next)		/* First block */
	{
	  next->prev = REL_NIL;
	  pointer_list = next;
	}
      else if (this_ptr = pointer_list) /* ONLY block */
	{
	  pointer_list = REL_NIL;
	  last_pointer = REL_NIL;
	  if (holes)		/* A hole precedes this block. */
	    {
	      holes = 0;
	      return_space (real_break_value - virtual_break_value);
	    }
	  else
	    return_space (this_ptr->size);

	  if (real_break_value != virtual_break_value)
	    abort ();

	  free_others (this_ptr->others);
	  free (this_ptr);
	  /* Turn off r_alloc_in_use? */

	  return 0;
	}
      else
	abort ();		/* Weird shit */

      free_others (this_ptr->others);
      free (this_ptr);
      bzero (this_ptr->block, this_ptr->size);
      note_hole (this_ptr->block, this_ptr->size);

      if (holes == MAX_HOLES)
	compactify ();
    }

  return 0;
}

/* Change the size of the block pointed to by the thing in PTR.
   If neccessary, r_alloc a new block and copy the data there.
   Return a pointer to the block if successfull, NULL otherwise.

   Note that if the size requested is less than the actual bloc size,
   nothing is done and the pointer is simply returned. */

POINTER
r_re_alloc (ptr, size)
     POINTER *ptr;
     int size;
{
  register relocatable_pointer *this_ptr = find_block (*ptr);
  POINTER block;

  if (! this_ptr)
    return NULL;

  if (this_ptr->size >= size)	/* Already have enough space. */
    return *ptr;

  /* Here we could try relocating the blocks just above... */
  block = r_alloc (ptr, size);
  if (block)
    {
      bcopy (this_ptr->block, block, this_ptr->size);
      if (this_ptr->others)
	last_record->others = this_ptr->others;

      if (! r_alloc_free (this_ptr->block))
	abort ();

      *ptr = block;
      return block;
    }

  return NULL;
}


/* Move and relocate all blocks from FIRST_PTR to LAST_PTR, inclusive,
   downwards to space starting at ADDRESS. */

static int
move_blocks_downward (first_ptr, last_ptr, address)
     relocatable_pointer *first_ptr, *last_ptr;
     POINTER address;
{
  int size = (last_ptr->block + last_ptr->size) - first_ptr->block;
  register relocatable_pointer *this_ptr = first_ptr;
  register offset = first_ptr->block - address;
  register struct other_pointer *op;

  /* Move all the data. */
  bcopy (first_ptr->block, address, size);

  /* Now relocate all the pointers to those blocks. */
  while (1)
    {
      this_ptr->block -= offset;
      *this_ptr->location = this_ptr->block;

      op = this_ptr->others; 
      while (op != (struct other_pointer *) 0)
	{
	  *op->location -= offset;
	  op = op->next;
	}

      if (this_ptr == last_ptr)
	return;
      else
	this_ptr = this_ptr->next;
    }

  return size;
}

/* Burp our memory zone. */

static void
compactify ()
{
  register relocatable_pointer *this_ptr = pointer_list;
  relocatable_pointer *first_to_move;
  register relocatable_pointer *last_to_move;
  hole_descriptor *this_hole = &r_alloc_holes[0];
  register hole_descriptor *next_hole;
  register POINTER end;		/* First address after hole */
  unsigned int space_regained = 0;

  while (holes)	 /* While there are holes */
    {
      /* Find the first block after this hole. */
      end = this_hole->address + this_hole->size;
      while (this_ptr && this_ptr->block != end)
	this_ptr = this_ptr->next;

      if (! this_ptr)
	abort ();

      next_hole = this_hole + 1;
      last_to_move = first_to_move = this_ptr;
      this_ptr = this_ptr->next;

      /* Note all blocks located before the next hole. */
      while (this_ptr && this_ptr->block < next_hole->address)
	{
	  last_to_move = this_ptr;
	  this_ptr = this_ptr->next;
	}
      space_regained +=
	move_blocks_downward (first_to_move, last_to_move, this_hole->address);

      holes--;
      this_hole = next_hole;
    }

  return_space (space_regained);
}

/* Relocate the list elements from the beginning of the list up to and
   including UP_TO_THIS_PTR to the area beginning at FREE_SPACE, which is
   after all current blocks.

   First copy all the data, then adjust the pointers and reorganize
   the list.  NOTE that this *only* works for contiguous blocks. */

static unsigned int
relocate_to_end (up_to_this_ptr, free_space)
     register relocatable_pointer *up_to_this_ptr;
     POINTER free_space;
{
  register relocatable_pointer *this_ptr;
  POINTER block_start = pointer_list->block;
  POINTER block_end = up_to_this_ptr->block + up_to_this_ptr->size;
  unsigned int total_size = block_end - block_start;
  unsigned int offset = (int) (free_space - block_start);

  bcopy (block_start, free_space, total_size);
  for (this_ptr = up_to_this_ptr; this_ptr; this_ptr = this_ptr->prev)
    {
      struct other_pointer *op = this_ptr->others;

      *this_ptr->location += offset;
      this_ptr->block += offset;

      while (op != (struct other_pointer *) 0)
	{
	  *op->location += offset;
	  op = op->next;
	}
    }

  /* Connect the head to the tail. */
  last_pointer->next = pointer_list;
  pointer_list->prev = last_pointer;

  /* Disconnect */
  up_to_this_ptr->next->prev = REL_NIL;
  pointer_list = up_to_this_ptr->next;
  up_to_this_ptr->next = REL_NIL;
  last_pointer = up_to_this_ptr;

  return total_size;		/* of space relocated. */
}

/* Relocate the list elements from FROM_THIS_PTR to (and including)
   the last to the zone beginning at FREE_SPACE, which is located
   before any blocks. 

   First copy all the data, then adjust the pointers and reorganize
   the list.  NOTE that this *only* works for contiguous blocks.  */

static unsigned int
relocate_to_beginning (from_this_ptr, free_space)
     register relocatable_pointer *from_this_ptr;
     POINTER free_space;
{
  POINTER block_start = from_this_ptr->block;
  POINTER block_end = last_pointer->block + last_pointer->size;
  unsigned int total_size = (int) (block_end - block_start);
  unsigned int offset = (int) (from_this_ptr->block - free_space);
  register relocatable_pointer *this_ptr;

  bcopy (block_start, free_space, total_size);
  for (this_ptr = from_this_ptr; this_ptr; this_ptr = this_ptr->next)
    {
      struct other_pointer *op = this_ptr->others;

      *this_ptr->location -= offset;
      this_ptr->block -= offset;

      while (op != (struct other_pointer *) 0)
	{
	  *op->location -= offset;
	  op = op->next;
	}
    }

  /* Connect the end to the beginning. */
  last_pointer->next = pointer_list;
  pointer_list->prev = last_pointer;

  /* Disconnect and reset first and last. */
  from_this_ptr->prev->next = REL_NIL;
  last_pointer = from_this_ptr->prev;
  pointer_list = from_this_ptr;
  pointer_list->prev = REL_NIL;

  return total_size;		/* of space moved. */
}

/* Relocate any blocks neccessary, either upwards or downwards,
   to obtain a space of SIZE bytes.  Assumes we have at least one block. */

static unsigned int
relocate (size)
     register int size;
{
  register relocatable_pointer *ptr;
  register int got = 0;

  if (size > 0)			/* Up: Relocate enough blocs to get SIZE. */
    {
      register POINTER new_space;

      for (ptr = pointer_list; got < size && ptr; ptr = ptr->next)
	got += ptr->size;

      if (ptr == REL_NIL)
	ptr = last_pointer;

      new_space = get_more_space (size);
      if (!new_space)
	return 0;

      return (relocate_to_end (ptr, pointer_list->block + size));
    }

  if (size < 0)			/* Down: relocate as many blocs as will
				   fit in SIZE bytes of space. */
    {
      register POINTER to_zone;
      unsigned int moved;

      for (ptr = last_pointer; got >= size && ptr; ptr = ptr->prev)
	got -= ptr->size;

      if (ptr == REL_NIL)
	ptr = pointer_list;
      else
	{
	  /* Back off one block to be <= size */
	  got += ptr->size;
	  ptr = ptr->next;
	}

      if (got >= size)
	{
	  to_zone = virtual_break_value - size + got;
	  moved = relocate_to_beginning (ptr, to_zone);
	  if (moved)
	    return_space (moved);

	  return moved;
	}

      return 0;
    }

  abort ();
}

/* This function encapsulates `sbrk' to preserve the relocatable blocks.
   It is called just like `sbrk'.  When relocatable blocks are in use,
   `malloc' must use this function instead of `sbrk'.  */

POINTER 
r_alloc_sbrk (size)
     unsigned int size;
{
  POINTER new_zone;		/* Start of the zone we will return. */

#if 0
  if (! r_alloc_in_use)
    return (POINTER) sbrk (size);
#endif

  if (size == 0)
    return virtual_break_value;

  if (size > 0)			/* Get more space */
    {
      register unsigned int space;

      if (pointer_list == REL_NIL)
	{
	  POINTER space = get_more_space (size);

	  virtual_break_value = real_break_value;
	  return space;
	}

      new_zone = virtual_break_value;

      /* Check if there is a hole just before the buffer zone. */
      if (holes && r_alloc_holes[0].address == virtual_break_value)
	{
	  if (r_alloc_holes[0].size > size)
	    {
	      /* Adjust the hole size. */
	      r_alloc_holes[0].size -= size;
	      r_alloc_holes[0].address += size;
	      virtual_break_value += size;

	      return new_zone;
	    }

	  if (r_alloc_holes[0].size == size)
	    {
	      virtual_break_value += size;
	      delete_hole (0);

	      return new_zone;
	    }

	  /* Adjust the size requested by space
	     already available in this hole. */
	  size -= r_alloc_holes[0].size;
	  virtual_break_value += r_alloc_holes[0].size;
	  delete_hole (0);
	}

      space = relocate (size);
      if (!space)
	return (POINTER) -1;

#ifdef REL_ALLOC_SAVE_SPACE
      move_blocks_downward
#else
      bzero (new_zone, space);
      if (space > size)
	note_hole (new_zone + size, space - size);
#endif	/* REL_ALLOC_SAVE_SPACE */

      virtual_break_value += size;
      return new_zone;
    }
  else				/* Return space to system */
    {
      int moved;
      int left_over;
      POINTER old_break_value;

      if (pointer_list == REL_NIL)
	{
	  POINTER space = return_space (-size);
	  virtual_break_value = real_break_value;

	  return space;
	}

      if (holes && r_alloc_holes[0].address == virtual_break_value)
	{
	  size -= r_alloc_holes[0].size;
	  delete_hole (0);
	}

      moved = relocate (size);
      old_break_value = virtual_break_value;

      if (!moved)
	return (POINTER) -1;

      left_over = moved + size;
      virtual_break_value += size;

      if (left_over)
	{
#ifdef REL_ALLOC_SAVE_SPACE
	  move_blocks_downward
#else
	  bzero (virtual_break_value, left_over);
	  note_hole (virtual_break_value, left_over);
#endif	/* not REL_ALLOC_SAVE_SPACE */
	}

      return old_break_value;
    }
}

/* For debugging */

#include <stdio.h>

void
memory_trace ()
{
  relocatable_pointer *ptr;
  int i;

  fprintf (stderr, "virtual: 0x%x\n   real: 0x%x\n   true: 0x%x\n\n",
	   virtual_break_value, real_break_value, true_break_value);
  fprintf (stderr, "Blocks:\n");
  for (ptr = pointer_list; ptr; ptr = ptr->next)
    {
      fprintf (stderr, "     address: 0x%x\n", ptr->block);
      fprintf (stderr, "        size: 0x%x\n", ptr->size);
      if (ptr->others)
	{
	  struct other_pointer *op = ptr->others;
	  fprintf (stderr, "      others:", ptr->size);
	  while (op)
	    {
	      fprintf (stderr, " 0x%x", op->location);
	      op = op->next;
	    }
	  fprintf (stderr, "\n");
	}
    }

  if (holes)
    {
      fprintf (stderr, "\nHoles:\n");
      for (i = 0; i < holes; i++)
	{
	  fprintf (stderr, "     address: 0x%x\n", r_alloc_holes[i].address);
	  fprintf (stderr, "        size: 0x%x\n", r_alloc_holes[i].size);
	}
    }

  fprintf (stderr, "\n\n");
}
