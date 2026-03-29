/* Timsort for sequences.

Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

/* This is a version of the cpython code implementing the TIMSORT
   sorting algorithm described in
   https://github.com/python/cpython/blob/main/Objects/listsort.txt.
   This algorithm identifies and pushes naturally ordered sublists of
   the original list, or "runs", onto a stack, and merges them
   periodically according to a merge strategy called "powersort".
   State is maintained during the sort in a merge_state structure,
   which is passed around as an argument to all the subroutines.  A
   "stretch" structure includes a pointer to the run BASE of length
   LEN along with its POWER (a computed integer used by the powersort
   merge strategy that depends on this run and the succeeding run.)  */


#include <config.h>
#include "lisp.h"


/* Reverse a slice of a vector in place, from lo up to (exclusive) hi. */
static void
reverse_slice (Lisp_Object *lo, Lisp_Object *hi)
{
  --hi;
  while (lo < hi)
    {
      Lisp_Object t = *lo;
      *lo = *hi;
      *hi = t;
      ++lo;
      --hi;
    }
}

/* A sortslice contains a pointer to an array of keys and a pointer to
   an array of corresponding values.  In other words, keys[i]
   corresponds with values[i].  If values == NULL, then the keys are
   also the values.

   Several convenience routines are provided here, so that keys and
   values are always moved in sync.  */

typedef struct
{
  Lisp_Object *keys;
  Lisp_Object *values;
} sortslice;

/* FIXME: Instead of values=NULL, can we set values=keys, so that they
   are both moved in lockstep and we avoid a lot of branches?
   We may do some useless work but it might be cheaper overall. */

static inline void
sortslice_copy (sortslice *s1, ptrdiff_t i, sortslice *s2, ptrdiff_t j)
{
  s1->keys[i] = s2->keys[j];
  if (s1->values != NULL)
    s1->values[i] = s2->values[j];
}

static inline void
sortslice_copy_incr (sortslice *dst, sortslice *src)
{
  *dst->keys++ = *src->keys++;
  if (dst->values != NULL)
    *dst->values++ = *src->values++;
}

static inline void
sortslice_copy_decr (sortslice *dst, sortslice *src)
{
  *dst->keys-- = *src->keys--;
  if (dst->values != NULL)
    *dst->values-- = *src->values--;
}


static inline void
sortslice_memcpy (sortslice *s1, ptrdiff_t i, sortslice *s2, ptrdiff_t j,
		  ptrdiff_t n)
{
  memcpy (&s1->keys[i], &s2->keys[j], sizeof s1->keys[0] * n);
  if (s1->values != NULL)
    memcpy (&s1->values[i], &s2->values[j], sizeof s1->values[0] * n);
}

static inline void
sortslice_memmove (sortslice *s1, ptrdiff_t i, sortslice *s2, ptrdiff_t j,
		   ptrdiff_t n)
{
  memmove (&s1->keys[i], &s2->keys[j], sizeof s1->keys[0] * n);
  if (s1->values != NULL)
    memmove (&s1->values[i], &s2->values[j], sizeof s1->values[0] * n);
}

static inline void
sortslice_advance (sortslice *slice, ptrdiff_t n)
{
  slice->keys += n;
  if (slice->values != NULL)
    slice->values += n;
}

/* MAX_MERGE_PENDING is the maximum number of entries in merge_state's
   pending-stretch stack.  For a list with n elements, this needs at most
   floor(log2(n)) + 1 entries even if we didn't force runs to a
   minimal length.  So the number of bits in a ptrdiff_t is plenty large
   enough for all cases.  */

#define MAX_MERGE_PENDING PTRDIFF_WIDTH

/* Once we get into galloping mode, we stay there as long as both runs
   win at least GALLOP_WIN_MIN consecutive times.  */

#define GALLOP_WIN_MIN 7

/* A small temp array of size MERGESTATE_TEMP_SIZE is used to avoid
   malloc when merging small lists.  */

#define MERGESTATE_TEMP_SIZE 256

struct stretch
{
  sortslice base;
  ptrdiff_t len;
  int power;
};

struct reloc
{
  sortslice *src;
  sortslice *dst;
  ptrdiff_t *size;
  int order; /* -1 while in merge_lo; +1 while in merg_hi; 0 otherwise.  */
};


typedef struct merge_state
{
  Lisp_Object *basekeys;
  Lisp_Object *allocated_keys;	/* heap-alloc'ed key array or NULL */
  ptrdiff_t listlen;

  /* PENDING is a stack of N pending stretches yet to be merged.
     Stretch #i starts at address base[i] and extends for len[i]
     elements.  */

  int n;
  struct stretch pending[MAX_MERGE_PENDING];

  /* The variable MIN_GALLOP, initialized to GALLOP_WIN_MIN, controls
     when we get *into* galloping mode.  merge_lo and merge_hi tend to
     nudge it higher for random data, and lower for highly structured
     data.  */

  ptrdiff_t min_gallop;

  /* 'A' is temporary storage, able to hold ALLOCED elements, to help
     with merges.  'A' initially points to TEMPARRAY, and subsequently
     to newly allocated memory if needed.  */

  sortslice a;
  ptrdiff_t alloced;
  specpdl_ref count;
  Lisp_Object temparray[MERGESTATE_TEMP_SIZE];

  /* If an exception is thrown while merging we might have to relocate
     some list elements from temporary storage back into the list.
     RELOC keeps track of the information needed to do this.  */

  struct reloc reloc;

  /* The C ordering (less-than) predicate.  */
  bool (*pred_fun) (struct merge_state *ms, Lisp_Object a, Lisp_Object b);

  /* The Lisp ordering predicate; Qnil means value<.  */
  Lisp_Object predicate;
} merge_state;


static bool
order_pred_lisp (merge_state *ms, Lisp_Object a, Lisp_Object b)
{
  return !NILP (calln (ms->predicate, a, b));
}

static bool
order_pred_valuelt (merge_state *ms, Lisp_Object a, Lisp_Object b)
{
  return !NILP (Fvaluelt (a, b));
}

/* Return true iff A < B according to the order predicate.  */
static inline bool
inorder (merge_state *ms, Lisp_Object a, Lisp_Object b)
{
  return ms->pred_fun (ms, a, b);
}

/* Sort the list starting at LO and ending at HI using a stable binary
   insertion sort algorithm. On entry the sublist [LO, START) (with
   START between LO and HIGH) is known to be sorted (pass START == LO
   if you are unsure).  Even in case of error, the output will be some
   permutation of the input (nothing is lost or duplicated).  */

static void
binarysort (merge_state *ms, sortslice lo, const Lisp_Object *hi,
	    Lisp_Object *start)
{
  eassume (lo.keys <= start && start <= hi);
  if (lo.keys == start)
    ++start;
  for (; start < hi; ++start)
    {
      Lisp_Object *l = lo.keys;
      Lisp_Object *r = start;
      Lisp_Object pivot = *r;

      eassume (l < r);
      do {
	Lisp_Object *p = l + ((r - l) >> 1);
	if (inorder (ms, pivot, *p))
	  r = p;
	else
	  l = p + 1;
      } while (l < r);
      eassume (l == r);
      for (Lisp_Object *p = start; p > l; --p)
	p[0] = p[-1];
      *l = pivot;

      if (lo.values != NULL)
	{
	  ptrdiff_t offset = lo.values - lo.keys;
	  Lisp_Object *p = start + offset;
	  pivot = *p;
	  l += offset;
	  for (Lisp_Object *p = start + offset; p > l; --p)
	    p[0] = p[-1];
	  *l = pivot;
	}
    }
}


/*  Find and return the length of the "run" (the longest
    non-decreasing sequence or the longest strictly decreasing
    sequence, with the Boolean *DESCENDING set to 0 in the former
    case, or to 1 in the latter) beginning at LO, in the slice [LO,
    HI) with LO < HI.  The strictness of the definition of
    "descending" ensures there are no equal elements to get out of
    order so the caller can safely reverse a descending sequence
    without violating stability.  */

static ptrdiff_t
count_run (merge_state *ms, Lisp_Object *lo, const Lisp_Object *hi,
	   bool *descending)
{
  eassume (lo < hi);
  *descending = 0;
  ++lo;
  ptrdiff_t n = 1;
  if (lo == hi)
    return n;

  n = 2;
  if (inorder (ms, lo[0], lo[-1]))
    {
      *descending = 1;
      for (lo = lo + 1; lo < hi; ++lo, ++n)
	{
	  if (!inorder (ms, lo[0], lo[-1]))
	    break;
	}
    }
  else
    {
      for (lo = lo + 1; lo < hi; ++lo, ++n)
	{
	  if (inorder (ms, lo[0], lo[-1]))
	    break;
	}
    }

  return n;
}


/*  Locate and return the proper insertion position of KEY in a sorted
    vector: if the vector contains an element equal to KEY, return the
    position immediately to the left of the leftmost equal element.
    [GALLOP_RIGHT does the same except it returns the position to the
    right of the rightmost equal element (if any).]

    'A' is a sorted vector of N elements. N must be > 0.

    Elements preceding HINT, a non-negative index less than N, are
    skipped.  The closer HINT is to the final result, the faster this
    runs.

    The return value is the int k in [0, N] such that

    A[k-1] < KEY <= a[k]

    pretending that *(A-1) precedes all values and *(A+N) succeeds all
    values.  In other words, the first k elements of A should precede
    KEY, and the last N-k should follow KEY.  */

static ptrdiff_t
gallop_left (merge_state *ms, const Lisp_Object key, Lisp_Object *a,
	     const ptrdiff_t n, const ptrdiff_t hint)
{
  eassume (a && n > 0 && hint >= 0 && hint < n);

  a += hint;
  ptrdiff_t lastofs = 0;
  ptrdiff_t ofs = 1;
  if (inorder (ms, *a, key))
    {
      /* When a[hint] < key, gallop right until
	 a[hint + lastofs] < key <= a[hint + ofs].  */
      const ptrdiff_t maxofs = n - hint; /* This is one after the end of a.  */
      while (ofs < maxofs)
	{
	  if (inorder (ms, a[ofs], key))
	    {
	      lastofs = ofs;
	      eassume (ofs <= (PTRDIFF_MAX - 1) / 2);
	      ofs = (ofs << 1) + 1;
	    }
	  else
	    break; /* Here key <= a[hint+ofs].  */
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to offsets relative to &a[0].  */
      lastofs += hint;
      ofs += hint;
    }
  else
    {
      /* When key <= a[hint], gallop left, until
	 a[hint - ofs] < key <= a[hint - lastofs].  */
      const ptrdiff_t maxofs = hint + 1;        /* Here &a[0] is lowest.  */
      while (ofs < maxofs)
	{
	  if (inorder (ms, a[-ofs], key))
	    break;
	  /* Here key <= a[hint - ofs].  */
	  lastofs = ofs;
	  eassume (ofs <= (PTRDIFF_MAX - 1) / 2);
	  ofs = (ofs << 1) + 1;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to use positive offsets relative to &a[0].  */
      ptrdiff_t k = lastofs;
      lastofs = hint - ofs;
      ofs = hint - k;
    }
  a -= hint;

  eassume (-1 <= lastofs && lastofs < ofs && ofs <= n);
  /* Now a[lastofs] < key <= a[ofs], so key belongs somewhere to the
     right of lastofs but no farther right than ofs.  Do a binary
     search, with invariant a[lastofs-1] < key <= a[ofs].  */
  ++lastofs;
  while (lastofs < ofs)
    {
      ptrdiff_t m = lastofs + ((ofs - lastofs) >> 1);

      if (inorder (ms, a[m], key))
	lastofs = m + 1;            /* Here a[m] < key.  */
      else
	ofs = m;                    /* Here key <= a[m].  */
    }
  eassume (lastofs == ofs);         /* Then a[ofs-1] < key <= a[ofs].  */
  return ofs;
}


/*  Locate and return the proper position of KEY in a sorted vector
    exactly like GALLOP_LEFT, except that if KEY already exists in
    A[0:N] find the position immediately to the right of the rightmost
    equal value.

    The return value is the int k in [0, N] such that

    A[k-1] <= KEY < A[k].  */

static ptrdiff_t
gallop_right (merge_state *ms, const Lisp_Object key, Lisp_Object *a,
	      const ptrdiff_t n, const ptrdiff_t hint)
{
  eassume (a && n > 0 && hint >= 0 && hint < n);

  a += hint;
  ptrdiff_t lastofs = 0;
  ptrdiff_t ofs = 1;
  if (inorder (ms, key, *a))
    {
      /* When key < a[hint], gallop left until
	 a[hint - ofs] <= key < a[hint - lastofs].  */
      const ptrdiff_t maxofs = hint + 1;        /* Here &a[0] is lowest.  */
      while (ofs < maxofs)
	{
	  if (inorder (ms, key, a[-ofs]))
	    {
	      lastofs = ofs;
	      eassume (ofs <= (PTRDIFF_MAX - 1) / 2);
	      ofs = (ofs << 1) + 1;
	    }
	  else                /* Here a[hint - ofs] <= key.  */
	    break;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to use positive offsets relative to &a[0].  */
      ptrdiff_t k = lastofs;
      lastofs = hint - ofs;
      ofs = hint - k;
    }
  else
    {
      /* When a[hint] <= key, gallop right, until
	 a[hint + lastofs] <= key < a[hint + ofs].  */
      const ptrdiff_t maxofs = n - hint;        /* Here &a[n-1] is highest.  */
      while (ofs < maxofs)
	{
	  if (inorder (ms, key, a[ofs]))
	    break;
	  /* Here a[hint + ofs] <= key.  */
	  lastofs = ofs;
	  eassume (ofs <= (PTRDIFF_MAX - 1) / 2);
	  ofs = (ofs << 1) + 1;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to use offsets relative to &a[0].  */
      lastofs += hint;
      ofs += hint;
    }
  a -= hint;

  eassume (-1 <= lastofs && lastofs < ofs && ofs <= n);
  /* Now a[lastofs] <= key < a[ofs], so key belongs somewhere to the
     right of lastofs but no farther right than ofs.  Do a binary
     search, with invariant a[lastofs-1] <= key < a[ofs].  */
  ++lastofs;
  while (lastofs < ofs)
    {
      ptrdiff_t m = lastofs + ((ofs - lastofs) >> 1);

      if (inorder (ms, key, a[m]))
	ofs = m;                    /* Here key < a[m].  */
      else
	lastofs = m + 1;            /* Here a[m] <= key.  */
    }
  eassume (lastofs == ofs);         /* Now  a[ofs-1] <= key < a[ofs].  */
  return ofs;
}


static void merge_register_cleanup (merge_state *ms);

static void
merge_init (merge_state *ms, const ptrdiff_t list_size,
	    Lisp_Object *allocated_keys, sortslice *lo, Lisp_Object predicate)
{
  eassume (ms != NULL);

  if (lo->values != NULL)
    {
      /* The temporary space for merging will need at most half the list
	 size rounded up.  Use the minimum possible space so we can use the
	 rest of temparray for other things.  In particular, if there is
	 enough extra space, if will be used to store the keys.  */
      ms->alloced = (list_size + 1) / 2;

      /* ms->alloced describes how many keys will be stored at
	 ms->temparray, but we also need to store the values.  Hence,
	 ms->alloced is capped at half of MERGESTATE_TEMP_SIZE.  */
      if (MERGESTATE_TEMP_SIZE / 2 < ms->alloced)
	ms->alloced = MERGESTATE_TEMP_SIZE / 2;
      ms->a.values = &ms->temparray[ms->alloced];
    }
  else
    {
      ms->alloced = MERGESTATE_TEMP_SIZE;
      ms->a.values = NULL;
    }
  ms->a.keys = ms->temparray;

  ms->n = 0;
  ms->min_gallop = GALLOP_WIN_MIN;
  ms->listlen = list_size;
  ms->basekeys = lo->keys;
  ms->allocated_keys = allocated_keys;
  ms->pred_fun = NILP (predicate) ? order_pred_valuelt : order_pred_lisp;
  ms->predicate = predicate;
  ms->reloc = (struct reloc){NULL, NULL, NULL, 0};
  ms->count = make_invalid_specpdl_ref ();
  if (allocated_keys != NULL)
    merge_register_cleanup (ms);
}


/* The dynamically allocated memory may hold lisp objects during
   merging.  MERGE_MARKMEM marks them so they aren't reaped during
   GC.  */

static void
merge_markmem (void *arg)
{
  merge_state *ms = arg;
  eassume (ms != NULL);

  if (ms->allocated_keys != NULL)
    mark_objects (ms->allocated_keys, ms->listlen);

  if (ms->reloc.size != NULL && *ms->reloc.size > 0)
    {
      Lisp_Object *src = (ms->reloc.src->values
			  ? ms->reloc.src->values : ms->reloc.src->keys);
      eassume (src != NULL);
      mark_objects (src, *ms->reloc.size);
    }
}


/* Free all temp storage.  If an exception occurs while merging,
   relocate any lisp elements in temp storage back to the original
   array.  */

static void
cleanup_mem (void *arg)
{
  merge_state *ms = arg;
  eassume (ms != NULL);

  /* If we have an exception while merging, some of the list elements
     might only live in temp storage; we copy everything remaining in
     the temp storage back into the original list.  This ensures that
     the original list has all of the original elements, although
     their order is unpredictable.  */

  if (ms->reloc.order != 0 && *ms->reloc.size > 0)
    {
      Lisp_Object *src = (ms->reloc.src->values
			  ? ms->reloc.src->values : ms->reloc.src->keys);
      Lisp_Object *dst = (ms->reloc.dst->values
			  ? ms->reloc.dst->values : ms->reloc.dst->keys);
      eassume (src != NULL && dst != NULL);
      ptrdiff_t n = *ms->reloc.size;
      ptrdiff_t shift = ms->reloc.order == -1 ? 0 : n - 1;
      memcpy (dst - shift, src, n * word_size);
    }

  /* Free any remaining temp storage.  */
  if (ms->a.keys != ms->temparray)
    {
      xfree (ms->a.keys);
      ms->a.keys = NULL;
    }

  if (ms->allocated_keys != NULL)
    {
      xfree (ms->allocated_keys);
      ms->allocated_keys = NULL;
    }
}

static void
merge_register_cleanup (merge_state *ms)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_ptr_mark (cleanup_mem, ms, merge_markmem);
  ms->count = count;
}

/* Allocate enough temp memory for NEED array slots.  Any previously
   allocated memory is first freed, and a cleanup routine is
   registered to free memory at the very end of the sort, or on
   exception.  */

static void
merge_getmem (merge_state *ms, const ptrdiff_t need)
{
  eassume (ms != NULL);

  if (ms->a.keys == ms->temparray)
    {
      /* We only get here if alloc is needed and this is the first
	 time, so we set up the unwind protection.  */
      if (!specpdl_ref_valid_p (ms->count))
	merge_register_cleanup (ms);
    }
  else
    {
      /* We have previously alloced storage.  Since we don't care
         what's in the block we don't use realloc which would waste
         cycles copying the old data.  We just free and alloc
         again.  */
      xfree (ms->a.keys);
    }
  ptrdiff_t bytes = (need * word_size) << (ms->a.values != NULL ? 1 : 0);
  ms->a.keys = xmalloc (bytes);
  ms->alloced = need;
  if (ms->a.values != NULL)
    ms->a.values = &ms->a.keys[need];
}


static inline void
needmem (merge_state *ms, ptrdiff_t na)
{
  if (na > ms->alloced)
    merge_getmem (ms, na);
}


/* Stably merge (in-place) the NA elements starting at SSA with the NB
   elements starting at SSB = SSA + NA.  NA and NB must be positive.
   Require that SSA[NA-1] belongs at the end of the merge, and NA <=
   NB.  */

static void
merge_lo (merge_state *ms, sortslice ssa, ptrdiff_t na,
	  sortslice ssb, ptrdiff_t nb)
{
  eassume (ms && ssa.keys && ssb.keys && na > 0 && nb > 0);
  eassume (ssa.keys + na == ssb.keys);
  needmem (ms, na);
  sortslice_memcpy (&ms->a, 0, &ssa, 0, na);
  sortslice dest = ssa;
  ssa = ms->a;

  ms->reloc = (struct reloc){&ssa, &dest, &na, -1};

  sortslice_copy_incr (&dest, &ssb);
  --nb;
  if (nb == 0)
    goto Succeed;
  if (na == 1)
    goto CopyB;

  ptrdiff_t min_gallop = ms->min_gallop;
  for (;;)
    {
      ptrdiff_t acount = 0;   /* The # of consecutive times A won.  */

      ptrdiff_t bcount = 0;   /* The # of consecutive times B won.  */

      for (;;)
	{
	  eassume (na > 1 && nb > 0);
	  if (inorder (ms, ssb.keys[0], ssa.keys[0]))
	    {
	      sortslice_copy_incr (&dest, &ssb);
	      ++bcount;
	      acount = 0;
	      --nb;
	      if (nb == 0)
		goto Succeed;
	      if (bcount >= min_gallop)
		break;
	    }
	  else
	    {
	      sortslice_copy_incr (&dest, &ssa);
	      ++acount;
	      bcount = 0;
	      --na;
	      if (na == 1)
		goto CopyB;
	      if (acount >= min_gallop)
		break;
	    }
	}

      /* One run is winning so consistently that galloping may be a
	 huge speedup.  We try that, and continue galloping until (if
	 ever) neither run appears to be winning consistently
	 anymore.  */
      ++min_gallop;
      do {
	eassume (na > 1 && nb > 0);
	min_gallop -= min_gallop > 1;
	ms->min_gallop = min_gallop;
	ptrdiff_t k = gallop_right (ms, ssb.keys[0], ssa.keys, na, 0);
	acount = k;
	if (k)
	  {
	    sortslice_memcpy (&dest, 0, &ssa, 0, k);
	    sortslice_advance (&dest, k);
	    sortslice_advance (&ssa, k);
	    na -= k;
	    if (na == 1)
	      goto CopyB;
	    /* While na==0 is impossible for a consistent comparison
	       function, we shouldn't assume that it is.  */
	    if (na == 0)
	      goto Succeed;
	  }
	sortslice_copy_incr (&dest, &ssb);
	--nb;
	if (nb == 0)
	  goto Succeed;

	k = gallop_left (ms, ssa.keys[0], ssb.keys, nb, 0);
	bcount = k;
	if (k)
	  {
	    sortslice_memmove (&dest, 0, &ssb, 0, k);
	    sortslice_advance (&dest, k);
	    sortslice_advance (&ssb, k);
	    nb -= k;
	    if (nb == 0)
	      goto Succeed;
	  }
	sortslice_copy_incr (&dest, &ssa);
	--na;
	if (na == 1)
	  goto CopyB;
      } while (acount >= GALLOP_WIN_MIN || bcount >= GALLOP_WIN_MIN);
      ++min_gallop;   /* Apply a penalty for leaving galloping mode.  */
      ms->min_gallop = min_gallop;
    }
 Succeed:
  ms->reloc = (struct reloc){NULL, NULL, NULL, 0};

  if (na)
    sortslice_memcpy(&dest, 0, &ssa, 0, na);
  return;
 CopyB:
  eassume (na == 1 && nb > 0);
  ms->reloc = (struct reloc){NULL, NULL, NULL, 0};

  /* The last element of ssa belongs at the end of the merge.  */
  sortslice_memmove (&dest, 0, &ssb, 0, nb);
  sortslice_copy (&dest, nb, &ssa, 0);
}


/* Stably merge (in-place) the NA elements starting at SSA with the NB
   elements starting at SSB = SSA + NA.  NA and NB must be positive.
   Require that SSA[NA-1] belongs at the end of the merge, and NA >=
   NB.  */

static void
merge_hi (merge_state *ms, sortslice ssa, ptrdiff_t na,
	  sortslice ssb, ptrdiff_t nb)
{
  eassume (ms && ssa.keys && ssb.keys && na > 0 && nb > 0);
  eassume (ssa.keys + na == ssb.keys);
  needmem (ms, nb);
  sortslice dest = ssb;
  sortslice_advance (&dest, nb-1);
  sortslice_memcpy (&ms->a, 0, &ssb, 0, nb);
  sortslice basea = ssa;
  sortslice baseb = ms->a;
  ssb.keys = ms->a.keys + nb - 1;
  if (ssb.values != NULL)
    ssb.values = ms->a.values + nb - 1;
  sortslice_advance (&ssa, na - 1);

  ms->reloc = (struct reloc){&baseb, &dest, &nb, 1};

  sortslice_copy_decr (&dest, &ssa);
  --na;
  if (na == 0)
    goto Succeed;
  if (nb == 1)
    goto CopyA;

  ptrdiff_t min_gallop = ms->min_gallop;
  for (;;) {
    ptrdiff_t acount = 0;   /* The # of consecutive times A won.  */
    ptrdiff_t bcount = 0;   /* The # of consecutive times B won.  */

    for (;;) {
      eassume (na > 0 && nb > 1);
      if (inorder (ms, ssb.keys[0], ssa.keys[0]))
	{
	  sortslice_copy_decr (&dest, &ssa);
	  ++acount;
	  bcount = 0;
	  --na;
	  if (na == 0)
	    goto Succeed;
	  if (acount >= min_gallop)
	    break;
	}
      else
	{
	  sortslice_copy_decr (&dest, &ssb);
	  ++bcount;
	  acount = 0;
	  --nb;
	  if (nb == 1)
	    goto CopyA;
	  if (bcount >= min_gallop)
	    break;
	}
    }

    /* One run is winning so consistently that galloping may be a huge
       speedup.  Try that, and continue galloping until (if ever)
       neither run appears to be winning consistently anymore.  */
    ++min_gallop;
    do {
      eassume (na > 0 && nb > 1);
      min_gallop -= min_gallop > 1;
      ms->min_gallop = min_gallop;
      ptrdiff_t k = gallop_right (ms, ssb.keys[0], basea.keys, na, na - 1);
      k = na - k;
      acount = k;
      if (k)
	{
	  sortslice_advance (&dest, -k);
	  sortslice_advance (&ssa, -k);
	  sortslice_memmove (&dest, 1, &ssa, 1, k);
	  na -= k;
	  if (na == 0)
	    goto Succeed;
	}
      sortslice_copy_decr(&dest, &ssb);
      --nb;
      if (nb == 1)
	goto CopyA;

      k = gallop_left (ms, ssa.keys[0], baseb.keys, nb, nb - 1);
      k = nb - k;
      bcount = k;
      if (k)
	{
	  sortslice_advance (&dest, -k);
	  sortslice_advance (&ssb, -k);
	  sortslice_memcpy (&dest, 1, &ssb, 1, k);
	  nb -= k;
	  if (nb == 1)
	    goto CopyA;
	  /* While nb==0 is impossible for a consistent comparison
	      function we shouldn't assume that it is.  */
	  if (nb == 0)
	    goto Succeed;
	}
      sortslice_copy_decr (&dest, &ssa);
      --na;
      if (na == 0)
	goto Succeed;
    } while (acount >= GALLOP_WIN_MIN || bcount >= GALLOP_WIN_MIN);
    ++min_gallop;      /* Apply a penalty for leaving galloping mode.  */
    ms->min_gallop = min_gallop;
  }
 Succeed:
  ms->reloc = (struct reloc){NULL, NULL, NULL, 0};
  if (nb)
    sortslice_memcpy (&dest, -(nb-1), &baseb, 0, nb);
  return;
 CopyA:
  eassume (nb == 1 && na > 0);
  ms->reloc = (struct reloc){NULL, NULL, NULL, 0};
  /* The first element of ssb belongs at the front of the merge.  */
  sortslice_memmove (&dest, 1-na, &ssa, 1-na, na);
  sortslice_advance (&dest, -na);
  sortslice_advance (&ssa, -na);
  sortslice_copy (&dest, 0, &ssb, 0);
}


/* Merge the two runs at stack indices I and I+1.  */

static void
merge_at (merge_state *ms, const ptrdiff_t i)
{
  eassume (ms != NULL);
  eassume (ms->n >= 2);
  eassume (i >= 0);
  eassume (i == ms->n - 2 || i == ms->n - 3);

  sortslice ssa = ms->pending[i].base;
  ptrdiff_t na = ms->pending[i].len;
  sortslice ssb = ms->pending[i + 1].base;
  ptrdiff_t nb = ms->pending[i + 1].len;
  eassume (na > 0 && nb > 0);
  eassume (ssa.keys + na == ssb.keys);

  /* Record the length of the combined runs. The current run i+1 goes
     away after the merge.  If i is the 3rd-last run now, slide the
     last run (which isn't involved in this merge) over to i+1.  */
  ms->pending[i].len = na + nb;
  if (i == ms->n - 3)
    ms->pending[i + 1] = ms->pending[i + 2];
  --ms->n;

  /* Where does b start in a?  Elements in a before that can be
     ignored (they are already in place).  */
  ptrdiff_t k = gallop_right (ms, *ssb.keys, ssa.keys, na, 0);
  eassume (k >= 0);
  sortslice_advance (&ssa, k);
  na -= k;
  if (na == 0)
    return;

  /* Where does a end in b?  Elements in b after that can be ignored
     (they are already in place).  */
  nb = gallop_left (ms, ssa.keys[na - 1], ssb.keys, nb, nb - 1);
  if (nb == 0)
    return;
  eassume (nb > 0);
  /* Merge what remains of the runs using a temp array with size
     min(na, nb) elements.  */
  if (na <= nb)
    merge_lo (ms, ssa, na, ssb, nb);
  else
    merge_hi (ms, ssa, na, ssb, nb);
}


/* Compute the "power" of the first of two adjacent runs beginning at
   index S1, with the first having length N1 and the second (starting
   at index S1+N1) having length N2.  The run has total length N.  */

static int
powerloop (const ptrdiff_t s1, const ptrdiff_t n1, const ptrdiff_t n2,
	   const ptrdiff_t n)
{
  eassume (s1 >= 0);
  eassume (n1 > 0 && n2 > 0);
  eassume (s1 + n1 + n2 <= n);
  /* The midpoints a and b are
     a = s1 + n1/2
     b = s1 + n1 + n2/2 = a + (n1 + n2)/2

     These may not be integers because of the "/2", so we work with
     2*a and 2*b instead.  It makes no difference to the outcome,
     since the bits in the expansion of (2*i)/n are merely shifted one
     position from those of i/n.  */
  ptrdiff_t a = 2 * s1 + n1;
  ptrdiff_t b = a + n1 + n2;
  int result = 0;
  /* Emulate a/n and b/n one bit a time, until their bits differ.  */
  for (;;)
    {
      ++result;
      if (a >= n)
	{  /* Both quotient bits are now 1.  */
	  eassume (b >= a);
	  a -= n;
	  b -= n;
	}
      else if (b >= n)
	{  /* a/n bit is 0 and b/n bit is 1.  */
	  break;
	} /* Otherwise both quotient bits are 0.  */
      eassume (a < b && b < n);
      a <<= 1;
      b <<= 1;
    }
  return result;
}


/* Update the state upon identifying a run of length N2.  If there's
   already a stretch on the stack, apply the "powersort" merge
   strategy: compute the topmost stretch's "power" (depth in a
   conceptual binary merge tree) and merge adjacent runs on the stack
   with greater power.  */

static void
found_new_run (merge_state *ms, const ptrdiff_t n2)
{
  eassume (ms != NULL);
  if (ms->n)
    {
      eassume (ms->n > 0);
      struct stretch *p = ms->pending;
      ptrdiff_t s1 = p[ms->n - 1].base.keys - ms->basekeys;
      ptrdiff_t n1 = p[ms->n - 1].len;
      int power = powerloop (s1, n1, n2, ms->listlen);
      while (ms->n > 1 && p[ms->n - 2].power > power)
	{
	  merge_at (ms, ms->n - 2);
	}
      eassume (ms->n < 2 || p[ms->n - 2].power < power);
      p[ms->n - 1].power = power;
    }
}


/* Unconditionally merge all stretches on the stack until only one
   remains.  */

static void
merge_force_collapse (merge_state *ms)
{
  struct stretch *p = ms->pending;

  eassume (ms != NULL);
  while (ms->n > 1)
    {
      ptrdiff_t n = ms->n - 2;
      if (n > 0 && p[n - 1].len < p[n + 1].len)
	--n;
      merge_at (ms, n);
    }
}


/* Compute a good value for the minimum run length; natural runs
   shorter than this are boosted artificially via binary insertion.

   If N < 64, return N (it's too small to bother with fancy stuff).
   Otherwise if N is an exact power of 2, return 32.  Finally, return
   an int k, 32 <= k <= 64, such that N/k is close to, but strictly
   less than, an exact power of 2.  */

static ptrdiff_t
merge_compute_minrun (ptrdiff_t n)
{
  ptrdiff_t r = 0;           /* r will become 1 if any non-zero bits are
				shifted off.  */

  eassume (n >= 0);
  while (n >= 64)
    {
      r |= n & 1;
      n >>= 1;
    }
  return n + r;
}


static void
reverse_sortslice (sortslice *s, const ptrdiff_t n)
{
  reverse_slice (s->keys, &s->keys[n]);
  if (s->values != NULL)
    reverse_slice (s->values, &s->values[n]);
}

static Lisp_Object
resolve_fun (Lisp_Object fun)
{
  if (SYMBOLP (fun))
    {
      /* Attempt to resolve the function as far as possible ahead of time,
	 to avoid having to do it for each call.  */
      Lisp_Object f = XSYMBOL (fun)->u.s.function;
      if (SYMBOLP (f))
	/* Function was an alias; use slow-path resolution.  */
	f = indirect_function (f);
      /* Don't resolve to an autoload spec; that would be very slow.  */
      if (!NILP (f) && !(CONSP (f) && EQ (XCAR (f), Qautoload)))
	fun = f;
    }
  return fun;
}

/* Sort the array SEQ with LENGTH elements in the order determined by
   PREDICATE (where Qnil means value<) and KEYFUNC (where Qnil means identity),
   optionally reversed.  */
void
tim_sort (Lisp_Object predicate, Lisp_Object keyfunc,
	  Lisp_Object *seq, const ptrdiff_t length, bool reverse)
{
  /* FIXME: hoist this to the caller? */
  if (EQ (predicate, Qvaluelt))
    predicate = Qnil;
  if (!NILP (predicate))
    predicate = resolve_fun (predicate);
  if (EQ (keyfunc, Qidentity))
    keyfunc = Qnil;

  sortslice lo;
  Lisp_Object *keys;
  Lisp_Object *allocated_keys = NULL;
  merge_state ms;

  if (reverse && 0 < length)
    reverse_slice (seq, seq + length);    /* preserve stability */

  if (NILP (keyfunc))
    {
      keys = NULL;
      lo.keys = seq;
      lo.values = NULL;
    }
  else
    {
      keyfunc = resolve_fun (keyfunc);
      if (length < MERGESTATE_TEMP_SIZE / 2)
	keys = &ms.temparray[length + 1];
      else
	{
	  /* Fill with valid Lisp values in case a GC occurs before all
	     keys have been computed.  */
	  static_assert (NIL_IS_ZERO);
	  keys = allocated_keys = xzalloc (length * word_size);
	}

      lo.keys = keys;
      lo.values = seq;
    }

  merge_init (&ms, length, allocated_keys, &lo, predicate);

  /* Compute keys after merge_markmem has been registered by merge_init
     (any call to keyfunc might trigger a GC).  */
  if (!NILP (keyfunc))
    for (ptrdiff_t i = 0; i < length; i++)
      keys[i] = calln (keyfunc, seq[i]);

  /* FIXME: This is where we would check the keys for interesting
     properties for more optimized comparison (such as all being fixnums
     etc).  */

  /* March over the array once, left to right, finding natural runs,
     and extending short natural runs to minrun elements.  */
  const ptrdiff_t minrun = merge_compute_minrun (length);
  ptrdiff_t nremaining = length;
  do {
    bool descending;

    /* Identify the next run.  */
    ptrdiff_t n = count_run (&ms, lo.keys, lo.keys + nremaining, &descending);
    if (descending)
      reverse_sortslice (&lo, n);
    /* If the run is short, extend it to min(minrun, nremaining).  */
    if (n < minrun)
      {
	const ptrdiff_t force = min (nremaining, minrun);
	binarysort (&ms, lo, lo.keys + force, lo.keys + n);
	n = force;
      }
    eassume (ms.n == 0
	     || (ms.pending[ms.n - 1].base.keys + ms.pending[ms.n - 1].len
		 == lo.keys));
    found_new_run (&ms, n);
    /* Push the new run on to the stack.  */
    eassume (ms.n < MAX_MERGE_PENDING);
    ms.pending[ms.n].base = lo;
    ms.pending[ms.n].len = n;
    ++ms.n;
    /* Advance to find the next run.  */
    sortslice_advance (&lo, n);
    nremaining -= n;
  } while (nremaining);

  merge_force_collapse (&ms);
  eassume (ms.n == 1);
  eassume (ms.pending[0].len == length);
  lo = ms.pending[0].base;

  if (reverse)
    reverse_slice (seq, seq + length);

  if (ms.a.keys != ms.temparray || allocated_keys != NULL)
    unbind_to (ms.count, Qnil);
}
