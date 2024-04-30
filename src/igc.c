/* Incremental, generational, concurrent GC using MPS.
   Copyright (C) 2024 Free Software Foundation, Inc.

This file is part of GNU Emacs.

Author: Gerd Moellmann <gerd@gnu.org>

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

// clang-format on

#include <config.h>
#include <limits.h>
#include <mps.h>
#include <mpsavm.h>
#include <mpscamc.h>
#include <mpscawl.h>
#include <mpslib.h>
#include <stdlib.h>
#include "lisp.h"
#include "comp.h"
#include "igc.h"
#include "bignum.h"
#include "buffer.h"
#include "coding.h"
#include "dispextern.h"
#include "emacs-module.h"
#include "font.h"
#include "frame.h"
#include "intervals.h"
#include "itree.h"
#include "pdumper.h"
#include "termhooks.h"
#include "thread.h"
#include "treesit.h"
#include "puresize.h"
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */

#ifndef USE_LSB_TAG
# error "USE_LSB_TAG required"
#endif
#ifdef WIDE_EMACS_INT
# error "WIDE_EMACS_INT not supported"
#endif
#if USE_STACK_LISP_OBJECTS
# error "USE_STACK_LISP_OBJECTS not supported"
#endif
#ifndef HAVE_PDUMPER
# error "HAVE_PDUMPER required"
#endif
#ifdef HAVE_TEXT_CONVERSION
//# error "HAVE_TEXT_CONVERSION not supported"
# warning "HAVE_TEXT_CONVERSION not supported"
#endif

struct Lisp_Weak_Ref
{
  union vectorlike_header header;
  Lisp_Object ref;
} GCALIGNED_STRUCT;

/* Note: Emacs will call allocation functions whlle aborting. This leads
   to all sorts of interesting phenomena when an assertion fails inside
   a function called from MPS.

   Example: We assert while MPS holds a lock, and MPS finds that it
   already holds the lock while Emacs is handling the assertion failure.

   The function signature must be that of mps_lib_assert_fail_t.  */

static void
igc_assert_fail (const char *file, unsigned line, const char *msg)
{
  die (msg, file, line);
}

#ifdef IGC_DEBUG

#define igc_assert(expr)				\
  do							\
    {							\
      if (!(expr))					\
	igc_assert_fail (__FILE__, __LINE__, #expr);	\
    }							\
  while (0)						\

#else
# define igc_assert(expr) (void) 9
#endif

#define igc_static_assert(x) verify (x)
#define IGC_TAG_MASK (~VALMASK)

/* Using mps_arena_has_addr is expensive. so try to do something that is
   "good enough". This can return true for malloc'd memory. */

static mps_addr_t min_addr, max_addr;

static bool
is_pure (const mps_addr_t addr)
{
#ifdef IN_MY_FORK
  return false;
#else
  return PURE_P (addr);
#endif
}

static bool
is_mps (const mps_addr_t addr)
{
  return addr >= min_addr && addr < max_addr && !pdumper_object_p (addr)
    && !c_symbol_p (addr) && !is_pure (addr);
}

enum
{
  IGC_ALIGN = GCALIGNMENT,
  IGC_ALIGN_DFLT = IGC_ALIGN,
};

static bool
is_aligned (const mps_addr_t addr)
{
  return ((mps_word_t) addr & IGC_TAG_MASK) == 0;
}

#define IGC_CHECK_RES(res)			\
  do						\
    {						\
      if ((res) != MPS_RES_OK)			\
	emacs_abort ();				\
    }						\
  while (0)					\

#define IGC_WITH_PARKED(gc)                        \
  for (int i = (mps_arena_park (gc->arena), 1); i; \
       i = (mps_arena_release (gc->arena), 0))

#define IGC_DEFINE_LIST(data)                                                  \
  typedef struct data##_list                                                   \
  {                                                                            \
    struct data##_list *next, *prev;                                           \
    data d;                                                                    \
  } data##_list;                                                               \
                                                                               \
  static data##_list *data##_list_push (data##_list **head, data *d)           \
  {                                                                            \
    data##_list *r = xzalloc (sizeof *r);                                      \
    r->d = *d;                                                                 \
    r->next = *head;                                                           \
    r->prev = NULL;                                                            \
    if (r->next)                                                               \
      r->next->prev = r;                                                       \
    *head = r;                                                                 \
    return r;                                                                  \
  }                                                                            \
                                                                               \
  static void data##_list_remove (data *d, data##_list **head, data##_list *r) \
  {                                                                            \
    if (r->next)                                                               \
      r->next->prev = r->prev;                                                 \
    if (r->prev)                                                               \
      r->prev->next = r->next;                                                 \
    else                                                                       \
      *head = r->next;                                                         \
    *d = r->d;                                                                 \
    xfree (r);                                                                 \
  }

enum igc_obj_type
{
  IGC_OBJ_INVALID,
  IGC_OBJ_PAD,
  IGC_OBJ_FWD,
  IGC_OBJ_CONS,
  IGC_OBJ_SYMBOL,
  IGC_OBJ_INTERVAL,
  IGC_OBJ_STRING,
  IGC_OBJ_STRING_DATA,
  IGC_OBJ_VECTOR,
  IGC_OBJ_ITREE_TREE,
  IGC_OBJ_ITREE_NODE,
  IGC_OBJ_IMAGE,
  IGC_OBJ_FACE,
  IGC_OBJ_FACE_CACHE,
  IGC_OBJ_FLOAT,
  IGC_OBJ_BLV,
  IGC_OBJ_WEAK,
  IGC_OBJ_LAST
};

static const char *obj_type_names[] = {
  "IGC_OBJ_INVALID",	"IGC_OBJ_PAD",	       "IGC_OBJ_FWD",
  "IGC_OBJ_CONS",	"IGC_OBJ_SYMBOL",      "IGC_OBJ_INTERVAL",
  "IGC_OBJ_STRING",	"IGC_OBJ_STRING_DATA", "IGC_OBJ_VECTOR",
  "IGC_OBJ_ITREE_TREE", "IGC_OBJ_ITREE_NODE",  "IGC_OBJ_IMAGE",
  "IGC_OBJ_FACE",	"IGC_OBJ_FACE_CACHE",  "IGC_OBJ_FLOAT",
  "IGC_OBJ_BLV",	"IGC_OBJ_WEAK",
};

igc_static_assert (ARRAYELTS (obj_type_names) == IGC_OBJ_LAST);

struct igc_stats
{
  struct
  {
    size_t nwords;
    size_t nobjs;
  } obj[IGC_OBJ_LAST];

  struct
  {
    size_t nwords;
    size_t nobjs;
  } pvec[PVEC_TAG_MAX + 1];
};

/* Always having a header makes it possible to have an
   address-independant hash, which is (a) much easier to handle than MPS
   location dependencies, and (b) makes it possible to implement sxhash
   variants in a way that works as expected even if GCs happen between
   calls.  */

enum
{
  IGC_TYPE_BITS = 5,
  IGC_PVEC_BITS = 6,
  IGC_HASH_BITS = 21,
  IGC_SIZE_BITS = 32
};

igc_static_assert (IGC_OBJ_LAST - 1 < (1 << IGC_TYPE_BITS));
igc_static_assert (PVEC_TAG_MAX < (1 << IGC_PVEC_BITS));

struct igc_header
{
  enum igc_obj_type obj_type : IGC_TYPE_BITS;
  enum pvec_type pvec_type : IGC_PVEC_BITS;
  mps_word_t hash : IGC_HASH_BITS;
  mps_word_t nwords : IGC_SIZE_BITS;
};

igc_static_assert (sizeof (struct igc_header) == 8);

static mps_word_t
to_words (mps_word_t nbytes)
{
  igc_assert (nbytes % sizeof (mps_word_t) == 0);
  return nbytes / sizeof (mps_word_t);
}

static mps_word_t
to_bytes (mps_word_t nwords)
{
  return nwords * sizeof (mps_word_t);
}

struct igc_fwd
{
  struct igc_header header;
  mps_addr_t new_base_addr;
};

static mps_addr_t
client_to_base (mps_addr_t client_addr)
{
  return (char *) client_addr - sizeof (struct igc_header);
}

static mps_addr_t
base_to_client (mps_addr_t base_addr)
{
  return (char *) base_addr + sizeof (struct igc_header);
}

static size_t
igc_round (size_t nbytes, size_t align)
{
  return ROUNDUP (nbytes, align);
}

/* Value is the size in bytes that we need to allocate from MPS
   for a client object of size NBYTES. */

static size_t
obj_size (size_t nbytes)
{
  nbytes += sizeof (struct igc_header);
  nbytes = max (nbytes, sizeof (struct igc_fwd));
  nbytes = igc_round (nbytes, IGC_ALIGN_DFLT);
  return nbytes;
}

struct igc_root
{
  struct igc *gc;
  mps_root_t root;
  void *start, *end;
  bool ambig;
};

typedef struct igc_root igc_root;
IGC_DEFINE_LIST (igc_root);

struct igc_thread
{
  struct igc *gc;
  mps_thr_t thr;
  mps_ap_t dflt_ap;
  mps_ap_t leaf_ap;
  mps_ap_t weak_strong_ap;
  mps_ap_t weak_weak_ap;
  igc_root_list *specpdl_root;
  igc_root_list *bc_root;
  struct thread_state *ts;
};

typedef struct igc_thread igc_thread;
IGC_DEFINE_LIST (igc_thread);

struct igc
{
  mps_arena_t arena;
  mps_chain_t chain;
  mps_fmt_t dflt_fmt;
  mps_pool_t dflt_pool;
  mps_fmt_t leaf_fmt;
  mps_pool_t leaf_pool;
  mps_fmt_t weak_fmt;
  mps_pool_t weak_pool;
  struct igc_root_list *roots;
  struct igc_thread_list *threads;
};

static struct igc *global_igc;

static struct igc_root_list *
register_root (struct igc *gc, mps_root_t root, void *start, void *end,
	       bool ambig)
{
  struct igc_root r
    = { .gc = gc, .root = root, .start = start, .end = end, .ambig = ambig };
  return igc_root_list_push (&gc->roots, &r);
}

static mps_root_t
deregister_root (struct igc_root_list *r)
{
  struct igc_root root;
  igc_root_list_remove (&root, &r->d.gc->roots, r);
  return root.root;
}

static void
destroy_root (struct igc_root_list **r)
{
  mps_root_destroy (deregister_root (*r));
  *r = NULL;
}

static struct igc_thread_list *
register_thread (struct igc *gc, mps_thr_t thr, struct thread_state *ts)
{
  struct igc_thread t = { .gc = gc, .thr = thr, .ts = ts };
  return igc_thread_list_push (&gc->threads, &t);
}

static mps_thr_t
deregister_thread (struct igc_thread_list *t)
{
  struct igc_thread thread;
  igc_thread_list_remove (&thread, &t->d.gc->threads, t);
  return thread.thr;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"

static mps_res_t
fix_lisp_obj (mps_ss_t ss, Lisp_Object *pobj)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_word_t *p = (mps_word_t *) pobj;
    mps_word_t word = *p;
    mps_word_t tag = word & IGC_TAG_MASK;

    if (tag == Lisp_Int0 || tag == Lisp_Int1)
      return MPS_RES_OK;
    else if (tag == Lisp_Type_Unused0)
      emacs_abort ();

    if (tag == Lisp_Symbol)
      {
	ptrdiff_t off = word ^ tag;
	mps_addr_t client = (mps_addr_t) ((char *) lispsym + off);
	if (is_mps (client))
	  {
	    mps_addr_t base = client_to_base (client);
	    if (MPS_FIX1 (ss, base))
	      {
		mps_res_t res = MPS_FIX2 (ss, &base);
		if (res != MPS_RES_OK)
		  return res;
		client = base_to_client (base);
		ptrdiff_t new_off = (char *) client - (char *) lispsym;
		*p = new_off | tag;
	      }
	  }
      }
    else
      {
	/* I have encountered cases where MPS_FIX1 returns true, but the
	   reference is somewhere completely off, so that MPS_FIX2 asserts.
	   IOW, MPS_FIX1 has undefined behavior if called on an address that
	   is not in the arena.  */
	mps_addr_t client = (mps_addr_t) (word ^ tag);
	if (is_mps (client))
	  {
	    mps_addr_t base = client_to_base (client);
	    if (MPS_FIX1 (ss, base))
	      {
		mps_res_t res = MPS_FIX2 (ss, &base);
		if (res != MPS_RES_OK)
		  return res;
		client = base_to_client (base);
		*p = (mps_word_t) client | tag;
	      }
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_raw (mps_ss_t ss, mps_addr_t *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_addr_t client = *p;
    if (is_mps (client) && is_aligned (client))
      {
	mps_addr_t base = client_to_base (client);
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    *p = base_to_client (base);
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#define IGC_FIX12_OBJ(ss, p)                           \
  do                                                   \
    {                                                  \
      mps_res_t res;                                   \
      MPS_FIX_CALL (ss, res = fix_lisp_obj (ss, (p))); \
      if (res != MPS_RES_OK)                           \
	return res;                                    \
    }                                                  \
  while (0)

#define IGC_FIX12_RAW(ss, p)                                     \
  do                                                             \
    {                                                            \
      mps_res_t res;                                             \
      MPS_FIX_CALL (ss, res = fix_raw (ss, (mps_addr_t *) (p))); \
      if (res != MPS_RES_OK)                                     \
	return res;                                              \
    }                                                            \
  while (0)

#define IGC_FIX12_NOBJS(ss, a, n)                            \
  do                                                         \
    {                                                        \
      mps_res_t res;                                         \
      MPS_FIX_CALL ((ss), res = fix_array ((ss), (a), (n))); \
      if (res != MPS_RES_OK)                                 \
	return res;                                          \
    }                                                        \
  while (0)

#define IGC_FIX_CALL(ss, expr)         \
  do                                   \
    {                                  \
      mps_res_t res;                   \
      MPS_FIX_CALL (ss, res = (expr)); \
      if (res != MPS_RES_OK)           \
	return res;                    \
    }                                  \
  while (0)

#define IGC_FIX_CALL_FN(ss, type, client_addr, fn) \
  do                                               \
    {                                              \
      type *obj_ = (type *) client_addr;           \
      mps_res_t res;                               \
      MPS_FIX_CALL (ss, res = fn (ss, obj_));      \
      if (res != MPS_RES_OK)                       \
	return res;                                \
    }                                              \
  while (0)

static mps_res_t
fix_array (mps_ss_t ss, Lisp_Object *array, size_t n)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (size_t i = 0; i < n; ++i)
      IGC_FIX12_OBJ (ss, &array[i]);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_staticvec (mps_ss_t ss, void *start, void *end, void *closure)
{
  igc_assert (start == staticvec);
  MPS_SCAN_BEGIN (ss)
  {
    for (Lisp_Object **p = start; (void *) p < end; ++p)
      if (*p)
	IGC_FIX12_OBJ (ss, *p);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_fwd (mps_ss_t ss, lispfwd fwd)
{
  MPS_SCAN_BEGIN (ss)
  {
    switch (XFWDTYPE (fwd))
      {
      case Lisp_Fwd_Int:
      case Lisp_Fwd_Bool:
      case Lisp_Fwd_Kboard_Obj:
	break;

      case Lisp_Fwd_Obj:
	{
	  /* It is not guaranteed that we see all of these when
	     scanning staticvec because of DEFVAR_LISP_NOPRO.  */
	  struct Lisp_Objfwd *o = (void *) fwd.fwdptr;
	  IGC_FIX12_OBJ (ss, o->objvar);
	}
	break;

      case Lisp_Fwd_Buffer_Obj:
	{
	  struct Lisp_Buffer_Objfwd *b = (void *) fwd.fwdptr;
	  IGC_FIX12_OBJ (ss, &b->predicate);
	}
	break;
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_symbol (mps_ss_t ss, struct Lisp_Symbol *sym)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &sym->u.s.name);
    IGC_FIX12_OBJ (ss, &sym->u.s.function);
    IGC_FIX12_OBJ (ss, &sym->u.s.plist);
#ifdef IN_MY_FORK
    IGC_FIX12_OBJ (ss, &sym->u.s.package);
#else
    IGC_FIX12_RAW (ss, &sym->u.s.next);
#endif
    switch (sym->u.s.redirect)
      {
      case SYMBOL_PLAINVAL:
	IGC_FIX12_OBJ (ss, &sym->u.s.val.value);
	break;

      case SYMBOL_VARALIAS:
	IGC_FIX12_RAW (ss, &sym->u.s.val.alias);
	break;

      case SYMBOL_LOCALIZED:
	IGC_FIX12_RAW (ss, &sym->u.s.val.blv);
	break;

      case SYMBOL_FORWARDED:
	IGC_FIX_CALL (ss, fix_fwd (ss, sym->u.s.val.fwd));
	break;
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* This exists because we need access to a threads' current specpdl
   pointer, which means we need access to the thread_state, which can
   move in memory. */

static mps_res_t
scan_igc (mps_ss_t ss, void *start, void *end, void *closure)
{
  igc_assert (start == (void *) global_igc);
  MPS_SCAN_BEGIN (ss)
  {
    struct igc *gc = start;
    for (struct igc_thread_list *t = gc->threads; t; t = t->next)
      IGC_FIX12_RAW (ss, &t->d.ts);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_lispsym (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (struct Lisp_Symbol *sym = start; (void *) sym < end; ++sym)
      IGC_FIX_CALL (ss, fix_symbol (ss, sym));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_rdstack (mps_ss_t ss, void *start, void *end, void *closure)
{
  if (rdstack.stack == NULL)
    return MPS_RES_OK;

  MPS_SCAN_BEGIN (ss)
  {
    for (ptrdiff_t i = 0; i < rdstack.sp; i++)
      {
	struct read_stack_entry *e = &rdstack.stack[i];
	switch (e->type)
	  {
	  case RE_list_start:
	    break;
	  case RE_list:
	  case RE_list_dot:
	    IGC_FIX12_OBJ (ss, &e->u.list.head);
	    IGC_FIX12_OBJ (ss, &e->u.list.tail);
	    break;
	  case RE_vector:
	  case RE_record:
	  case RE_char_table:
	  case RE_sub_char_table:
	  case RE_byte_code:
	  case RE_string_props:
	    IGC_FIX12_OBJ (ss, &e->u.vector.elems);
	    break;
	  case RE_special:
	    IGC_FIX12_OBJ (ss, &e->u.special.symbol);
	    break;
	  case RE_numbered:
	    IGC_FIX12_OBJ (ss, &e->u.numbered.number);
	    IGC_FIX12_OBJ (ss, &e->u.numbered.placeholder);
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_specpdl (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    /* MPS docs say that root scanning functions have exclusive access to
       what is being scanned, the same way format scanning functions
       do. That means I can use the thread's specpdl_ptr here. */
    struct igc_thread_list *t = closure;
    igc_assert (start == (void *) t->d.ts->m_specpdl);
    igc_assert (end == (void *) t->d.ts->m_specpdl_end);
    end = t->d.ts->m_specpdl_ptr;

    for (union specbinding *pdl = start; (void *) pdl < end; ++pdl)
      {
	switch (pdl->kind)
	  {
	  case SPECPDL_UNWIND:
	    IGC_FIX12_OBJ (ss, &pdl->unwind.arg);
	    break;

	    /* This is used by SAFE_ALLOCA/malloc. */
	  case SPECPDL_UNWIND_ARRAY:
	    IGC_FIX12_NOBJS (ss, pdl->unwind_array.array,
			     pdl->unwind_array.nelts);
	    break;

	  case SPECPDL_UNWIND_EXCURSION:
	    IGC_FIX12_OBJ (ss, &pdl->unwind_excursion.marker);
	    IGC_FIX12_OBJ (ss, &pdl->unwind_excursion.window);
	    break;

	    /* The bt.args member either points to something on a
	       thread's control stack, or to something in the bytecode
	       stack. Both should already be ambiguous roots.  */
	  case SPECPDL_BACKTRACE:
	    break;

#ifdef HAVE_MODULES
	  case SPECPDL_MODULE_RUNTIME:
	    break;

	    // If I am not mistaken, the emacs_env in this binding
	    // actually lives on the stack (see module-load e.g.).
	    // So, we don't have to do something here for the Lisp
	    // objects in emacs_env.
	  case SPECPDL_MODULE_ENVIRONMENT:
	    break;
#endif

	  case SPECPDL_LET_DEFAULT:
	  case SPECPDL_LET_LOCAL:
	    IGC_FIX12_OBJ (ss, &pdl->let.where);
	    FALLTHROUGH;
	  case SPECPDL_LET:
	    IGC_FIX12_OBJ (ss, &pdl->let.symbol);
	    IGC_FIX12_OBJ (ss, &pdl->let.old_value);
	    break;

	  case SPECPDL_UNWIND_PTR:
	    /* This defines a mark function of its own, which
	       is of no use to us.  Only user is sort.c. */
	    if (pdl->unwind_ptr.mark)
	      {
		igc_assert (!"SPECPDL_UNWIND_PTR with mark function");
	      }
	    break;

	  case SPECPDL_UNWIND_INT:
	  case SPECPDL_UNWIND_INTMAX:
	  case SPECPDL_UNWIND_VOID:
	  case SPECPDL_NOP:
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Scan the area of memory [START, END) ambiguously. In general,
   references may be either tagged words or pointers. This is used for
   blocks allocated with malloc and thread stacks. */

static mps_res_t
scan_ambig (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (mps_word_t *p = start; p < (mps_word_t *) end; ++p)
      {
	mps_word_t word = *p;
	mps_word_t tag = word & IGC_TAG_MASK;

	/* If the references in the object being scanned are
	   ambiguous then MPS_FIX2() does not update the
	   reference (because it can't know if it's a
	   genuine reference). The MPS handles an ambiguous
	   reference by pinning the block pointed to so that
	   it cannot move. */
	mps_addr_t ref = (mps_addr_t) word;
	mps_res_t res = MPS_FIX12 (ss, &ref);
	if (res != MPS_RES_OK)
	  return res;

	switch (tag)
	  {
	  case Lisp_Int0:
	  case Lisp_Int1:
	  case Lisp_Type_Unused0:
	    break;

	  case Lisp_Symbol:
	    {
	      ptrdiff_t off = word ^ tag;
	      ref = (mps_addr_t) ((char *) lispsym + off);
	      res = MPS_FIX12 (ss, &ref);
	      if (res != MPS_RES_OK)
		return res;
	    }
	    break;

	  default:
	    ref = (mps_addr_t) (word ^ tag);
	    res = MPS_FIX12 (ss, &ref);
	    if (res != MPS_RES_OK)
	      return res;
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#ifndef IN_MY_FORK
static mps_res_t
scan_pure (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    igc_assert (start == (void *) pure);
    end = (char *) pure + pure_bytes_used_lisp;
    if (end > start)
      IGC_FIX_CALL (ss, scan_ambig (ss, start, end, NULL));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}
#endif

static mps_res_t
scan_bc (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    struct igc_thread_list *t = closure;
    struct bc_thread_state *bc = &t->d.ts->bc;
    igc_assert (start == (void *) bc->stack);
    igc_assert (end == (void *) bc->stack_end);
    end = bc_next_frame (bc->fp);
    if (end > start)
      IGC_FIX_CALL (ss, scan_ambig (ss, start, end, NULL));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_exact (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (Lisp_Object *p = start; (void *) p < end; ++p)
      IGC_FIX12_OBJ (ss, p);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
scan_ptr_exact (mps_ss_t ss, void *start, void *end, void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (void **p = start; (void *) p < end; ++p)
      if (*p)
	IGC_FIX12_RAW (ss, p);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/***********************************************************************
			 Default pad, fwd, ...
 ***********************************************************************/

static void
dflt_pad (mps_addr_t base_addr, size_t nbytes)
{
  igc_assert (nbytes >= sizeof (struct igc_header));
  struct igc_header *h = base_addr;
  h->obj_type = IGC_OBJ_PAD;
  h->nwords = to_words (nbytes);
}

static void
dflt_fwd (mps_addr_t old_base_addr, mps_addr_t new_base_addr)
{
  struct igc_header *h = old_base_addr;
  igc_assert (to_bytes (h->nwords) >= sizeof (struct igc_fwd));
  igc_assert (h->obj_type != IGC_OBJ_PAD);
  struct igc_fwd *f = old_base_addr;
  f->header.obj_type = IGC_OBJ_FWD;
  f->new_base_addr = new_base_addr;
}

static mps_addr_t
is_dflt_fwd (mps_addr_t base_addr)
{
  struct igc_fwd *f = base_addr;
  if (f->header.obj_type == IGC_OBJ_FWD)
    return f->new_base_addr;
  return NULL;
}

static mps_addr_t
dflt_skip (mps_addr_t base_addr)
{
  struct igc_header *h = base_addr;
  mps_addr_t next = (char *) base_addr + to_bytes (h->nwords);
  igc_assert (to_bytes (h->nwords) >= sizeof (struct igc_header));
  return next;
}

static mps_res_t
fix_string (mps_ss_t ss, struct Lisp_String *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &s->u.s.data);
    IGC_FIX12_RAW (ss, &s->u.s.intervals);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_interval (mps_ss_t ss, struct interval *iv)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &iv->left);
    IGC_FIX12_RAW (ss, &iv->right);
    if (iv->up_obj)
      IGC_FIX12_OBJ (ss, &iv->up.obj);
    else if (iv->up.interval)
      IGC_FIX12_RAW (ss, &iv->up.interval);
    IGC_FIX12_OBJ (ss, &iv->plist);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_itree_tree (mps_ss_t ss, struct itree_tree *t)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (t->root)
      IGC_FIX12_RAW (ss, &t->root);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_itree_node (mps_ss_t ss, struct itree_node *n)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (n->parent)
      IGC_FIX12_RAW (ss, &n->parent);
    if (n->left)
      IGC_FIX12_RAW (ss, &n->left);
    if (n->right)
      IGC_FIX12_RAW (ss, &n->right);
    IGC_FIX12_OBJ (ss, &n->data);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_image (mps_ss_t ss, struct image *i)
{
  MPS_SCAN_BEGIN (ss)
  {
#ifdef HAVE_WINDOW_SYSTEM
    IGC_FIX12_OBJ (ss, &i->spec);
    IGC_FIX12_OBJ (ss, &i->dependencies);
    IGC_FIX12_OBJ (ss, &i->lisp_data);
    IGC_FIX12_RAW (ss, &i->next);
    IGC_FIX12_RAW (ss, &i->prev);
#endif
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_face (mps_ss_t ss, struct face *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_NOBJS (ss, f->lface, ARRAYELTS (f->lface));
    IGC_FIX12_RAW (ss, &f->font);
    IGC_FIX12_RAW (ss, &f->next);
    IGC_FIX12_RAW (ss, &f->prev);
    IGC_FIX12_RAW (ss, &f->ascii_face);
#if defined HAVE_XFT || defined HAVE_FREETYPE
    IGC_FIX12_RAW (ss, &f->extra);
#endif
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_face_cache (mps_ss_t ss, struct face_cache *c)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (c->faces_by_id)
      for (int i = 0; i < c->used; ++i)
	{
	  igc_assert (c->faces_by_id[i] != NULL);
	  IGC_FIX12_RAW (ss, &c->faces_by_id[i]);
	}

    if (c->buckets)
      for (int i = 0; i < FACE_CACHE_BUCKETS_SIZE; ++i)
	if (c->buckets[i])
	  IGC_FIX12_RAW (ss, &c->buckets[i]);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_weak_ref (mps_ss_t ss, struct Lisp_Weak_Ref *wref)
{
  MPS_SCAN_BEGIN (ss)
  {
    /* FIXME: The below explicitly assumes Lisp_Object is of the same
       width as mps_word_t!  */
    const mps_word_t tagged_word = (ptrdiff_t) igc_weak_ref_deref (wref);
    const enum Lisp_Type tag = tagged_word & IGC_TAG_MASK;

    switch (tag)
      {
      case Lisp_Int0:
      case Lisp_Int1:
	return MPS_RES_OK;

      case Lisp_Type_Unused0:
	emacs_abort ();

      case Lisp_Symbol:
	{
	  ptrdiff_t off = tagged_word ^ Lisp_Symbol;
	  mps_addr_t client = (mps_addr_t)((char *)lispsym + off);
	  if (is_mps (client))
	    {
	      mps_addr_t base = client_to_base (client);
	      if (MPS_FIX1 (ss, base))
		{
		  mps_res_t res = MPS_FIX2 (ss, &base);
		  if (res != MPS_RES_OK)
		    return res;
		  if (base == NULL)
		    {
		      wref->ref = Qnil;
		    }
		  else
		    {
		      client = base_to_client (base);
		      ptrdiff_t new_off = (char *)client - (char *)lispsym;
		      wref->ref = (Lisp_Object)(new_off | tag);
		    }
		}
	    }
	}
	break;

      default:
	{
	  const mps_addr_t client = (mps_addr_t)(tagged_word ^ tag);
	  if (is_mps (client))
	    {
	      mps_addr_t base = client_to_base (client);
	      if (MPS_FIX1 (ss, base))
		{
		  const mps_res_t res = MPS_FIX2 (ss, &base);
		  if (res != MPS_RES_OK)
		    return res;
		  if (base == NULL)
		    {
		      wref->ref = Qnil;
		    }
		  else
		    {
		      const mps_addr_t client2 = base_to_client (base);
		      wref->ref = (Lisp_Object)((mps_word_t)client2 | tag);
		    }
		}
	    }
	}
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_weak (mps_ss_t ss, struct igc_header* base)
{
  MPS_SCAN_BEGIN (ss) {
    const mps_addr_t client = base_to_client(base);
    switch (base->pvec_type)
      {
      case PVEC_WEAK_REF:
	IGC_FIX_CALL_FN (ss, struct Lisp_Weak_Ref, client, fix_weak_ref);
	break;
      default:
	igc_assert (!"fix_weak");
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_cons (mps_ss_t ss, struct Lisp_Cons *cons)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &cons->u.s.car);
    IGC_FIX12_OBJ (ss, &cons->u.s.u.cdr);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_blv (mps_ss_t ss, struct Lisp_Buffer_Local_Value *blv)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &blv->where);
    IGC_FIX12_OBJ (ss, &blv->defcell);
    IGC_FIX12_OBJ (ss, &blv->valcell);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t fix_vector (mps_ss_t ss, struct Lisp_Vector *v);

static mps_res_t
dflt_scanx (mps_ss_t ss, mps_addr_t base_start, mps_addr_t base_limit,
	    void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (mps_addr_t base = base_start; base < base_limit;
	 base = dflt_skip (base))
      {
	mps_addr_t client = base_to_client (base);
	struct igc_header *header = base;

	if (closure)
	  {
	    struct igc_stats *st = closure;
	    st->obj[header->obj_type].nwords += header->nwords;
	    st->obj[header->obj_type].nobjs += 1;
	    st->obj[header->pvec_type].nwords += header->nwords;
	    st->obj[header->pvec_type].nobjs += 1;
	  }

	switch (header->obj_type)
	  {
	  case IGC_OBJ_INVALID:
	    emacs_abort ();

	  case IGC_OBJ_PAD:
	  case IGC_OBJ_FWD:
	    continue;

	  case IGC_OBJ_CONS:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Cons, client, fix_cons);
	    break;

	  case IGC_OBJ_STRING_DATA:
	  case IGC_OBJ_FLOAT:
	  case IGC_OBJ_LAST:
	    emacs_abort ();

	  case IGC_OBJ_SYMBOL:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Symbol, client, fix_symbol);
	    break;

	  case IGC_OBJ_INTERVAL:
	    IGC_FIX_CALL_FN (ss, struct interval, client, fix_interval);
	    break;

	  case IGC_OBJ_STRING:
	    IGC_FIX_CALL_FN (ss, struct Lisp_String, client, fix_string);
	    break;

	  case IGC_OBJ_VECTOR:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, client, fix_vector);
	    break;

	  case IGC_OBJ_ITREE_TREE:
	    IGC_FIX_CALL_FN (ss, struct itree_tree, client, fix_itree_tree);
	    break;

	  case IGC_OBJ_ITREE_NODE:
	    IGC_FIX_CALL_FN (ss, struct itree_node, client, fix_itree_node);
	    break;

	  case IGC_OBJ_IMAGE:
	    IGC_FIX_CALL_FN (ss, struct image, client, fix_image);
	    break;

	  case IGC_OBJ_FACE:
	    IGC_FIX_CALL_FN (ss, struct face, client, fix_face);
	    break;

	  case IGC_OBJ_FACE_CACHE:
	    IGC_FIX_CALL_FN (ss, struct face_cache, client, fix_face_cache);
	    break;

	  case IGC_OBJ_BLV:
	    IGC_FIX_CALL_FN (ss, struct Lisp_Buffer_Local_Value, client,
			     fix_blv);
	    break;

	  case IGC_OBJ_WEAK:
	    IGC_FIX_CALL_FN (ss, struct igc_header, base, fix_weak);
	    break;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
dflt_scan (mps_ss_t ss, mps_addr_t base_start, mps_addr_t base_limit)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL (ss, dflt_scanx (ss, base_start, base_limit, NULL));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static enum pvec_type
pseudo_vector_type (const struct Lisp_Vector *v)
{
  return PSEUDOVECTOR_TYPE (v);
}

static mps_res_t
fix_vectorlike (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    ptrdiff_t size = v->header.size;
    if (size & PSEUDOVECTOR_FLAG)
      size &= PSEUDOVECTOR_SIZE_MASK;
#ifdef IGC_DEBUG
    /* V can be something like &main_thread */
    if (is_mps (v))
      {
	struct igc_header *h = client_to_base (v);
	igc_assert (to_bytes (h->nwords) >= size * word_size);
      }
#endif
    IGC_FIX12_NOBJS (ss, v->contents, size);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_buffer (mps_ss_t ss, struct buffer *b)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, b, fix_vectorlike);
    IGC_FIX12_RAW (ss, &b->own_text.intervals);
    IGC_FIX12_RAW (ss, &b->own_text.markers);
    IGC_FIX12_RAW (ss, &b->overlays);
    IGC_FIX12_RAW (ss, &b->own_text.markers);

    IGC_FIX12_RAW (ss, &b->base_buffer);
    if (b->base_buffer)
      b->text = &b->base_buffer->own_text;
    else
      b->text = &b->own_text;

    // FIXME: special handling of undo_list?
    IGC_FIX12_OBJ (ss, &b->undo_list_);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_glyph_matrix (mps_ss_t ss, struct glyph_matrix *matrix)
{
  MPS_SCAN_BEGIN (ss)
  {
    struct glyph_row *row = matrix->rows;
    struct glyph_row *end = row + matrix->nrows;

    for (; row < end; ++row)
      if (row->enabled_p)
	{
	  for (int area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	    {
	      struct glyph *glyph = row->glyphs[area];
	      struct glyph *end_glyph = glyph + row->used[area];
	      for (; glyph < end_glyph; ++glyph)
		{
		  Lisp_Object *obj_ptr = &glyph->object;
		  if (STRINGP (*obj_ptr))
		    IGC_FIX12_OBJ (ss, obj_ptr);
		}
	    }
	}
    IGC_FIX12_RAW (ss, &matrix->buffer);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_frame (mps_ss_t ss, struct frame *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    // FIXME
    // output_data;
    // terminal
    // glyph_pool
    // glyph matrices
    // struct font_driver_list *font_driver_list;
    // struct text_conversion_state conversion;
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, f, fix_vectorlike);
    IGC_FIX12_RAW (ss, &f->face_cache);
    if (f->terminal)
      IGC_FIX12_RAW (ss, &f->terminal);
#ifdef HAVE_WINDOW_SYSTEM
    if (FRAME_WINDOW_P (f) && FRAME_OUTPUT_DATA (f))
      {
	struct font **font_ptr = &FRAME_FONT (f);
	if (*font_ptr)
	  IGC_FIX12_RAW (ss, font_ptr);
	Lisp_Object *nle = &FRAME_DISPLAY_INFO(f)->name_list_element;
	IGC_FIX12_OBJ (ss, nle);
      }
#endif
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_window (mps_ss_t ss, struct window *w)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, w, fix_vectorlike);
    if (w->current_matrix)
      IGC_FIX_CALL (ss, fix_glyph_matrix (ss, w->current_matrix));
    if (w->desired_matrix)
      IGC_FIX_CALL (ss, fix_glyph_matrix (ss, w->desired_matrix));

    /* FIXME: window.h syas the following two are "marked specially", so
       they are not seen by fix_vectorlike. That's of course a no-go
       with MPS. What ever is special about these, we have to find
       another way to accomplish that with MPS. */
    IGC_FIX12_OBJ (ss, &w->prev_buffers);
    IGC_FIX12_OBJ (ss, &w->next_buffers);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_hash_table (mps_ss_t ss, struct Lisp_Hash_Table *h)
{
  MPS_SCAN_BEGIN (ss)
  {
    // FIXME: weak
    IGC_FIX12_NOBJS (ss, h->key, h->table_size);
    IGC_FIX12_NOBJS (ss, h->value, h->table_size);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_char_table (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    int size = v->header.size & PSEUDOVECTOR_SIZE_MASK;
    enum pvec_type type = pseudo_vector_type (v);
    int idx = type == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0;
    for (int i = idx; i < size; ++i)
      IGC_FIX12_OBJ (ss, &v->contents[i]);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_overlay (mps_ss_t ss, struct Lisp_Overlay *o)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &o->buffer);
    IGC_FIX12_OBJ (ss, &o->plist);
    IGC_FIX12_RAW (ss, &o->interval);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_subr (mps_ss_t ss, struct Lisp_Subr *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &s->command_modes);
#ifdef HAVE_NATIVE_COMP
    IGC_FIX12_OBJ (ss, &s->intspec.native);
    IGC_FIX12_OBJ (ss, &s->command_modes);
    IGC_FIX12_OBJ (ss, &s->native_comp_u);
    IGC_FIX12_OBJ (ss, &s->lambda_list);
    IGC_FIX12_OBJ (ss, &s->type);
#endif
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_misc_ptr (mps_ss_t ss, struct Lisp_Misc_Ptr *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, p, fix_vectorlike);
    IGC_FIX12_RAW (ss, &p->pointer);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_user_ptr (mps_ss_t ss, struct Lisp_User_Ptr *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, p, fix_vectorlike);
    IGC_FIX12_RAW (ss, &p->p);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_handler (mps_ss_t ss, struct handler *h)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &h->tag_or_ch);
    IGC_FIX12_OBJ (ss, &h->val);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_thread (mps_ss_t ss, struct thread_state *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, s, fix_vectorlike);
    IGC_FIX12_RAW (ss, &s->m_current_buffer);
    IGC_FIX12_RAW (ss, &s->next_thread);
    for (struct handler *h = s->m_handlerlist; h; h = h->next)
      IGC_FIX_CALL_FN (ss, struct handler, h, fix_handler);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* This is here because main_thread is, for some reason, a variable in
   the data segment, and not like other threads. */

static mps_res_t
scan_main_thread (mps_ss_t ss, void *start, void *end, void *closure)
{
  igc_assert (start == (void *) &main_thread);
  MPS_SCAN_BEGIN (ss)
  {
    struct thread_state *s = start;
    IGC_FIX_CALL (ss, fix_thread (ss, s));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_mutex (mps_ss_t ss, struct Lisp_Mutex *m)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, m, fix_vectorlike);
    IGC_FIX12_RAW (ss, &m->name);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_coding (mps_ss_t ss, struct coding_system *c)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (c)
      {
	IGC_FIX12_OBJ (ss, &c->src_object);
	IGC_FIX12_OBJ (ss, &c->dst_object);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_terminal (mps_ss_t ss, struct terminal *t)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, t, fix_vectorlike);
    IGC_FIX12_RAW (ss, &t->next_terminal);
    // These are malloc'd, so they can be accessed.
    IGC_FIX_CALL_FN (ss, struct coding_system, t->keyboard_coding, fix_coding);
    IGC_FIX_CALL_FN (ss, struct coding_system, t->terminal_coding, fix_coding);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_marker (mps_ss_t ss, struct Lisp_Marker *m)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_RAW (ss, &m->buffer);
    IGC_FIX12_RAW (ss, &m->next);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_finalizer (mps_ss_t ss, struct Lisp_Finalizer *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, f, fix_vectorlike);
    IGC_FIX12_RAW (ss, &f->next);
    IGC_FIX12_RAW (ss, &f->prev);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_comp_unit (mps_ss_t ss, struct Lisp_Native_Comp_Unit *u)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, u, fix_vectorlike);
    if (u->data_imp_relocs)
      {
	size_t n = ASIZE (u->data_impure_vec);
	IGC_FIX12_NOBJS (ss, u->data_imp_relocs, n);
      }
    if (u->data_relocs)
      {
	size_t n = ASIZE (u->data_vec);
	IGC_FIX12_NOBJS (ss, u->data_relocs, n);
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#ifdef HAVE_XWIDGETS

static mps_res_t
fix_xwidget (mps_ss_t ss, struct xwidget *w)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, w, fix_vectorlike);
    igc_assert (!"xwidget");
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_xwidget_view (mps_ss_t ss, struct xwidget_view *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
    igc_assert (!"xwidget_view");
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#endif // HAVE_XWIDGETS

#ifdef HAVE_MODULES
static mps_res_t
fix_global_ref (mps_ss_t ss, struct module_global_reference *r)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, r, fix_vectorlike);
    IGC_FIX12_OBJ (ss, &r->value.v);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}
#endif

#ifndef IN_MY_FORK
static mps_res_t
fix_obarray (mps_ss_t ss, struct Lisp_Obarray *o)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (o->buckets)
      IGC_FIX12_NOBJS (ss, o->buckets, obarray_size (o));
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}
#endif

static mps_res_t
fix_font (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
    /* See font.h for the magic numbers. */
    switch (v->header.size & PSEUDOVECTOR_SIZE_MASK)
      {
      case FONT_SPEC_MAX:
      case FONT_ENTITY_MAX:
	break;
      case FONT_OBJECT_MAX:
	{
	  struct font *f = (struct font *)v;
	  Lisp_Object const *type = &f->driver->type;
	  IGC_FIX12_OBJ (ss, (Lisp_Object *)type);
	}
	break;
      default:
	emacs_abort ();
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Note that there is a small window after committing a vectorlike
   allocation where the object is zeroed, and so the vector header is
   also zero.  This doesn't have an adverse effect. */

static mps_res_t
fix_vector (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    switch (pseudo_vector_type (v))
      {
#ifndef IN_MY_FORK
      case PVEC_OBARRAY:
	IGC_FIX_CALL_FN (ss, struct Lisp_Obarray, v, fix_obarray);
	break;
#endif

      case PVEC_BUFFER:
	IGC_FIX_CALL_FN (ss, struct buffer, v, fix_buffer);
	break;

      case PVEC_FRAME:
	IGC_FIX_CALL_FN (ss, struct frame, v, fix_frame);
	break;

      case PVEC_WINDOW:
	IGC_FIX_CALL_FN (ss, struct window, v, fix_window);
	break;

      case PVEC_HASH_TABLE:
	IGC_FIX_CALL_FN (ss, struct Lisp_Hash_Table, v, fix_hash_table);
	break;

      case PVEC_CHAR_TABLE:
      case PVEC_SUB_CHAR_TABLE:
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_char_table);
	break;

      case PVEC_BOOL_VECTOR:
	break;

      case PVEC_OVERLAY:
	IGC_FIX_CALL_FN (ss, struct Lisp_Overlay, v, fix_overlay);
	break;

      case PVEC_SUBR:
	IGC_FIX_CALL_FN (ss, struct Lisp_Subr, v, fix_subr);
	break;

      case PVEC_FREE:
	emacs_abort ();

      case PVEC_FINALIZER:
	IGC_FIX_CALL_FN (ss, struct Lisp_Finalizer, v, fix_finalizer);
	break;

      case PVEC_MISC_PTR:
	IGC_FIX_CALL_FN (ss, struct Lisp_Misc_Ptr, v, fix_misc_ptr);
	break;

      case PVEC_USER_PTR:
	IGC_FIX_CALL_FN (ss, struct Lisp_User_Ptr, v, fix_user_ptr);
	break;

#ifdef HAVE_XWIDGETS
      case PVEC_XWIDGET:
	IGC_FIX_CALL_FN (ss, struct xwidget, v, fix_xwidget);
	break;

      case PVEC_XWIDGET_VIEW:
	IGC_FIX_CALL_FN (ss, struct xwidget_view, v, fix_xwidget_view);
	break;
#endif

      case PVEC_THREAD:
	IGC_FIX_CALL_FN (ss, struct thread_state, v, fix_thread);
	break;

      case PVEC_MUTEX:
	IGC_FIX_CALL_FN (ss, struct Lisp_Mutex, v, fix_mutex);
	break;

      case PVEC_TERMINAL:
	IGC_FIX_CALL_FN (ss, struct terminal, v, fix_terminal);
	break;

      case PVEC_MARKER:
	IGC_FIX_CALL_FN (ss, struct Lisp_Marker, v, fix_marker);
	break;

      case PVEC_BIGNUM:
	break;

      case PVEC_NATIVE_COMP_UNIT:
	IGC_FIX_CALL_FN (ss, struct Lisp_Native_Comp_Unit, v, fix_comp_unit);
	break;

      case PVEC_MODULE_GLOBAL_REFERENCE:
#ifdef HAVE_MODULES
	IGC_FIX_CALL_FN (ss, struct module_global_reference, v, fix_global_ref);
#endif
	break;

      case PVEC_FONT:
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_font);
	break;

      case PVEC_NORMAL_VECTOR:
      case PVEC_SYMBOL_WITH_POS:
      case PVEC_PROCESS:
      case PVEC_WINDOW_CONFIGURATION:
      case PVEC_XWIDGET:
      case PVEC_XWIDGET_VIEW:
      case PVEC_MODULE_FUNCTION:
      case PVEC_CONDVAR:
      case PVEC_TS_COMPILED_QUERY:
      case PVEC_TS_NODE:
      case PVEC_TS_PARSER:
      case PVEC_SQLITE:
      case PVEC_COMPILED:
      case PVEC_RECORD:
      case PVEC_OTHER:
#ifdef IN_MY_FORK
      case PVEC_PACKAGE:
#endif
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
	break;

      case PVEC_WEAK_REF:
	emacs_abort ();
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#pragma GCC diagnostic pop

static igc_root_list *
root_create (struct igc *gc, void *start, void *end, mps_rank_t rank,
	     mps_area_scan_t scan, bool ambig)
{
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, rank, 0, start, end, scan, 0);
  IGC_CHECK_RES (res);
  return register_root (gc, root, start, end, ambig);
}

static igc_root_list *
root_create_ambig (struct igc *gc, void *start, void *end)
{
  return root_create (gc, start, end, mps_rank_ambig (), scan_ambig, true);
}

static igc_root_list *
root_create_exact (struct igc *gc, void *start, void *end, mps_area_scan_t scan)
{
  return root_create (gc, start, end, mps_rank_exact (), scan, false);
}

static void
root_create_staticvec (struct igc *gc)
{
  root_create_exact (gc, staticvec, staticvec + ARRAYELTS (staticvec),
		     scan_staticvec);
}

static void
root_create_lispsym (struct igc *gc)
{
  root_create_exact (gc, lispsym, lispsym + ARRAYELTS (lispsym), scan_lispsym);
}

static void
root_create_buffer (struct igc *gc, struct buffer *b)
{
  void *start = &b->name_, *end = &b->own_text;
  root_create_ambig (gc, start, end);
}

static void
root_create_terminal_list (struct igc *gc)
{
  void *start = &terminal_list;
  void *end = (char *) start + sizeof (terminal_list);
  root_create_ambig (gc, start, end);
}

static void
root_create_main_thread (struct igc *gc)
{
  void *start = &main_thread;
  void *end = (char *) &main_thread + sizeof (main_thread);
  root_create_exact (gc, start, end, scan_main_thread);
}

void
igc_root_create_ambig (void *start, void *end)
{
  root_create_ambig (global_igc, start, end);
}

void
igc_root_create_exact (Lisp_Object *start, Lisp_Object *end)
{
  root_create_exact (global_igc, start, end, scan_exact);
}

void
igc_root_create_exact_ptr (void *var_addr)
{
  void *start = var_addr;
  void *end = (char *) start + sizeof (void *);
  root_create_exact (global_igc, start, end, scan_ptr_exact);
}

static void
root_create_specpdl (struct igc_thread_list *t)
{
  struct igc *gc = t->d.gc;
  struct thread_state *ts = t->d.ts;
  igc_assert (ts->m_specpdl != NULL);
  igc_assert (t->d.specpdl_root == NULL);
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, mps_rank_exact (), 0,
			    ts->m_specpdl, ts->m_specpdl_end, scan_specpdl, t);
  IGC_CHECK_RES (res);
  t->d.specpdl_root
    = register_root (gc, root, ts->m_specpdl, ts->m_specpdl_end, false);
}

static void
root_create_bc (struct igc_thread_list *t)
{
  struct igc *gc = t->d.gc;
  struct bc_thread_state *bc = &t->d.ts->bc;
  igc_assert (bc->stack != NULL);
  mps_root_t root;
  mps_res_t res = mps_root_create_area (&root, gc->arena, mps_rank_ambig (), 0,
					bc->stack, bc->stack_end, scan_bc, t);
  IGC_CHECK_RES (res);
  igc_assert (t->d.bc_root == NULL);
  t->d.bc_root = register_root (gc, root, bc->stack, bc->stack_end, true);
}

static void
root_create_igc (struct igc *gc)
{
  root_create (gc, gc, gc + 1, mps_rank_exact (), scan_igc, false);
}

#ifndef IN_MY_FORK
static void
root_create_pure (struct igc *gc)
{
  void *start = &pure[0];
  void *end = &pure[PURESIZE];
  root_create (gc, start, end, mps_rank_ambig (), scan_pure, true);
}
#endif

static void
root_create_thread (struct igc_thread_list *t)
{
  struct igc *gc = t->d.gc;
  mps_root_t root;
  void *cold = (void *) t->d.ts->m_stack_bottom;
  mps_res_t res
    = mps_root_create_thread_scanned (&root, gc->arena, mps_rank_ambig (), 0,
				      t->d.thr, scan_ambig, 0, cold);
  IGC_CHECK_RES (res);
  register_root (gc, root, cold, NULL, true);
}

static void
root_create_rdstack (struct igc *gc)
{
  root_create_exact (gc, &rdstack, &rdstack + 1, scan_rdstack);
}

void
igc_on_grow_specpdl (void)
{
  /* Note that no two roots may overlap, so we have to temporarily
     stop the collector while replacing one root with another (xpalloc
     may realloc). Alternatives: (1) don't realloc, (2) alloc specpdl
     from MPS pool that is scanned. */
  struct igc_thread_list *t = current_thread->gc_info;
  IGC_WITH_PARKED (t->d.gc)
  {
    destroy_root (&t->d.specpdl_root);
    root_create_specpdl (t);
  }
}

static mps_res_t
create_weak_ap (mps_ap_t *ap, struct igc_thread *t, bool weak)
{
  struct igc *gc = t->gc;
  mps_res_t res;
  mps_pool_t pool = gc->weak_pool;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_RANK,
		  weak ? mps_rank_weak () : mps_rank_exact ());
    res = mps_ap_create_k (ap, pool, args);
  }
  MPS_ARGS_END (args);
  return res;
}

static void
create_thread_aps (struct igc_thread *t)
{
  struct igc *gc = t->gc;
  mps_res_t res;
  res = mps_ap_create_k (&t->dflt_ap, gc->dflt_pool, mps_args_none);
  IGC_CHECK_RES (res);
  res = mps_ap_create_k (&t->leaf_ap, gc->leaf_pool, mps_args_none);
  IGC_CHECK_RES (res);
  res = create_weak_ap (&t->weak_strong_ap, t, false);
  IGC_CHECK_RES (res);
  res = create_weak_ap (&t->weak_weak_ap, t, true);
  IGC_CHECK_RES (res);
}

static struct igc_thread_list *
thread_add (struct thread_state *ts)
{
  mps_thr_t thr;
  mps_res_t res = mps_thread_reg (&thr, global_igc->arena);
  IGC_CHECK_RES (res);
  struct igc_thread_list *t = register_thread (global_igc, thr, ts);
  root_create_thread (t);
  create_thread_aps (&t->d);
  return t;
}

void *
igc_thread_add (struct thread_state *ts)
{
  struct igc_thread_list *t = thread_add (ts);
  root_create_specpdl (t);
  root_create_bc (t);
  return t;
}

void
igc_on_alloc_main_thread_specpdl (void)
{
  root_create_specpdl (current_thread->gc_info);
}

void
igc_on_alloc_main_thread_bc (void)
{
  root_create_bc (current_thread->gc_info);
}

static void
add_main_thread (void)
{
  igc_assert (current_thread->gc_info == NULL);
  igc_assert (current_thread->m_stack_bottom == stack_bottom);
  current_thread->gc_info = thread_add (current_thread);
}

void
igc_thread_remove (void *info)
{
  struct igc_thread_list *t = info;
  mps_ap_destroy (t->d.dflt_ap);
  mps_ap_destroy (t->d.leaf_ap);
  mps_ap_destroy (t->d.weak_strong_ap);
  mps_ap_destroy (t->d.weak_weak_ap);
  mps_thread_dereg (t->d.thr);
  destroy_root (&t->d.specpdl_root);
  destroy_root (&t->d.bc_root);
  deregister_thread (t);
}

static void
release_arena (void)
{
  mps_arena_release (global_igc->arena);
}

specpdl_ref
igc_park_arena (void)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_void (release_arena);
  mps_arena_park (global_igc->arena);
  return count;
}

void
igc_on_pdump_loaded (void *start, void *end)
{
  root_create_ambig (global_igc, start, end);
}

static igc_root_list *
root_find (void *start)
{
  for (igc_root_list *r = global_igc->roots; r; r = r->next)
    if (r->d.start == start)
      return r;
  return NULL;
}

Lisp_Object *
igc_xalloc_lisp_objs_exact (size_t n)
{
  size_t size = n * sizeof (Lisp_Object);
  void *p = xzalloc (size);
  root_create_exact (global_igc, p, (char *) p + size, scan_exact);
  return p;
}

void *
igc_xzalloc_ambig (size_t size)
{
  /* Not sure if xzalloc can ever return NULL here, depending on all the
     config options involved. Also not sure when it returns non-null for
     size 0. It does for me on macOS. */
  void *p = xzalloc (size);
  if (p == NULL)
    return NULL;

  /* Can't make a root that has zero length. Want one to be able to
     detect calling igc_free on something not having a root. */
  void *end = (char *) p + size;
  if (end == p)
    end = (char *) p + IGC_ALIGN_DFLT;
  root_create_ambig (global_igc, p, end);
  return p;
}

void
igc_xfree (void *p)
{
  if (p == NULL || pdumper_object_p (p))
    return;
  struct igc_root_list *r = root_find (p);
  igc_assert (r != NULL);
  destroy_root (&r);
  xfree (p);
}

void *
igc_xpalloc_ambig (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
		   ptrdiff_t nitems_max, ptrdiff_t item_size)
{
  IGC_WITH_PARKED (global_igc)
  {
    if (pa)
      {
	struct igc_root_list *r = root_find (pa);
	igc_assert (r != NULL);
	destroy_root (&r);
      }
    pa = xpalloc (pa, nitems, nitems_incr_min, nitems_max, item_size);
    char *end = (char *) pa + *nitems * item_size;
    root_create_ambig (global_igc, pa, end);
  }
  return pa;
}

void *
igc_xnrealloc_ambig (void *pa, ptrdiff_t nitems, ptrdiff_t item_size)
{
  IGC_WITH_PARKED (global_igc)
  {
    if (pa)
      {
	struct igc_root_list *r = root_find (pa);
	igc_assert (r != NULL);
	destroy_root (&r);
      }
    pa = xnrealloc (pa, nitems, item_size);
    char *end = (char *) pa + nitems * item_size;
    root_create_ambig (global_igc, pa, end);
  }
  return pa;
}

void
igc_create_charset_root (void *table, size_t size)
{
  root_create_ambig (global_igc, table, (char *) table + size);
}

static void
finalize_hash_table (struct Lisp_Hash_Table *h)
{
  if (h->table_size)
    {
      /* Set the table size to 0 so that we don't further scan a hash
	 table after it has been finalized. Also, keep in mind that
	 xfree works with objects in a loaded dump. */
      h->table_size = 0;
      xfree (h->index);
      xfree (h->key);
      xfree (h->value);
      xfree (h->next);
      xfree (h->hash);
    }
}

#ifndef IN_MY_FORK
static void
finalize_obarray (struct Lisp_Obarray *o)
{
  if (o->buckets)
    {
      void *b = o->buckets;
      o->buckets = NULL;
      xfree (b);
    }
}
#endif

static void
finalize_bignum (struct Lisp_Bignum *n)
{
  mpz_clear (n->value);
}

static void
finalize_font (struct font *font)
{
  struct Lisp_Vector *v = (void *) font;
  if ((v->header.size & PSEUDOVECTOR_SIZE_MASK) == FONT_OBJECT_MAX)
    {
      /* The font driver might sometimes be NULL, e.g. if Emacs was
	 interrupted before it had time to set it up.  */
      const struct font_driver *drv = font->driver;
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
  if (((vector->header.size & PSEUDOVECTOR_SIZE_MASK) == FONT_ENTITY_MAX)
      && PSEUDOVEC_STRUCT (vector, font_entity)->is_android)
    android_finalize_font_entity (PSEUDOVEC_STRUCT (vector, font_entity));
#endif
}

static void
finalize_user_ptr (struct Lisp_User_Ptr *p)
{
  if (p->finalizer)
    p->finalizer (p->p);
}

#ifdef HAVE_TREE_SITTER
static void
finalize_ts_parser (struct Lisp_TS_Parser *p)
{
  treesit_delete_parser (p);
}

static void
finalize_ts_query (struct Lisp_TS_Query *q)
{
  treesit_delete_query (q);
}
#endif	/* HAVE_TREE_SITTER */

#ifdef HAVE_MODULES
static void
finalize_module_function (struct Lisp_Module_Function *f)
{
  module_finalize_function (f);
}
#endif

#ifdef HAVE_NATIVE_COMP
static void
finalize_comp_unit (struct Lisp_Native_Comp_Unit *u)
{
  unload_comp_unit (u);
  u->data_imp_relocs = NULL;
  u->data_relocs = NULL;
}

static void
finalize_subr (struct Lisp_Subr *subr)
{
  if (!NILP (subr->native_comp_u))
    {
      subr->native_comp_u = Qnil;
      xfree ((char *) subr->symbol_name);
      xfree (subr->native_c_name);
    }
}
#endif

static Lisp_Object
finalizer_handler (Lisp_Object args)
{
  add_to_log ("finalizer failed: %S", args);
  return Qnil;
}

static void
finalize_finalizer (struct Lisp_Finalizer *f)
{
  Lisp_Object fun = f->function;
  if (!NILP (fun))
    {
      f->function = Qnil;
      unchain_finalizer (f);
      specpdl_ref count = SPECPDL_INDEX ();
      ++number_finalizers_run;
      specbind (Qinhibit_quit, Qt);
      internal_condition_case_1 (call0, fun, Qt, finalizer_handler);
      unbind_to (count, Qnil);
    }
}

static void
finalize_vector (mps_addr_t v)
{
  switch (pseudo_vector_type (v))
    {
    case PVEC_FREE:
      emacs_abort ();

    case PVEC_HASH_TABLE:
      finalize_hash_table (v);
      break;

    case PVEC_BIGNUM:
      finalize_bignum (v);
      break;

    case PVEC_FONT:
      finalize_font (v);
      break;

    case PVEC_THREAD:
      finalize_one_thread (v);
      break;

    case PVEC_MUTEX:
      finalize_one_mutex (v);
      break;

    case PVEC_CONDVAR:
      finalize_one_condvar (v);
      break;

    case PVEC_USER_PTR:
      finalize_user_ptr (v);
      break;

    case PVEC_TS_PARSER:
#ifdef HAVE_TREE_SITTER
      finalize_ts_parser (v);
#endif
      break;

    case PVEC_TS_COMPILED_QUERY:
#ifdef HAVE_TREE_SITTER
      finalize_ts_query (v);
#endif
      break;

    case PVEC_MODULE_FUNCTION:
#ifdef HAVE_MODULES
      finalize_module_function (v);
#endif
      break;

    case PVEC_NATIVE_COMP_UNIT:
#ifdef HAVE_NATIVE_COMP
      finalize_comp_unit (v);
#endif
      break;

    case PVEC_SUBR:
#ifdef HAVE_NATIVE_COMP
      finalize_subr (v);
#endif
      break;

    case PVEC_FINALIZER:
      finalize_finalizer (v);
      break;

#ifndef IN_MY_FORK
    case PVEC_OBARRAY:
      finalize_obarray (v);
      break;
#endif

    case PVEC_SYMBOL_WITH_POS:
    case PVEC_PROCESS:
    case PVEC_RECORD:
    case PVEC_COMPILED:
    case PVEC_SQLITE:
    case PVEC_TS_NODE:
    case PVEC_NORMAL_VECTOR:
#ifdef IN_MY_FORK
    case PVEC_PACKAGE:
#endif
    case PVEC_WINDOW_CONFIGURATION:
    case PVEC_BUFFER:
    case PVEC_FRAME:
    case PVEC_WINDOW:
    case PVEC_CHAR_TABLE:
    case PVEC_SUB_CHAR_TABLE:
    case PVEC_BOOL_VECTOR:
    case PVEC_OVERLAY:
    case PVEC_OTHER:
    case PVEC_MISC_PTR:
    case PVEC_XWIDGET:
    case PVEC_XWIDGET_VIEW:
    case PVEC_TERMINAL:
    case PVEC_MARKER:
    case PVEC_WEAK_REF:
    case PVEC_MODULE_GLOBAL_REFERENCE:
      igc_assert (!"finalization not implemented");
      break;
    }
}

static void
finalize (struct igc *gc, mps_addr_t base)
{
  mps_addr_t client = base_to_client (base);
  struct igc_header *h = base;
  switch (h->obj_type)
    {
    case IGC_OBJ_INVALID:
    case IGC_OBJ_PAD:
    case IGC_OBJ_FWD:
      emacs_abort ();

    case IGC_OBJ_CONS:
    case IGC_OBJ_SYMBOL:
    case IGC_OBJ_INTERVAL:
    case IGC_OBJ_STRING:
    case IGC_OBJ_STRING_DATA:
    case IGC_OBJ_ITREE_TREE:
    case IGC_OBJ_ITREE_NODE:
    case IGC_OBJ_IMAGE:
    case IGC_OBJ_FACE:
    case IGC_OBJ_FACE_CACHE:
    case IGC_OBJ_FLOAT:
    case IGC_OBJ_WEAK:
    case IGC_OBJ_BLV:
    case IGC_OBJ_LAST:
      igc_assert (!"finalize not implemented");
      break;

    case IGC_OBJ_VECTOR:
      finalize_vector (client);
      break;
    }
}

static void
maybe_finalize (mps_addr_t client, enum pvec_type tag)
{
  mps_addr_t ref = client_to_base (client);
  switch (tag)
    {
    case PVEC_HASH_TABLE:
    case PVEC_BIGNUM:
    case PVEC_FONT:
    case PVEC_THREAD:
    case PVEC_MUTEX:
    case PVEC_CONDVAR:
    case PVEC_USER_PTR:
    case PVEC_TS_PARSER:
    case PVEC_TS_COMPILED_QUERY:
    case PVEC_MODULE_FUNCTION:
    case PVEC_NATIVE_COMP_UNIT:
    case PVEC_SUBR:
    case PVEC_FINALIZER:
#ifndef IN_MY_FORK
    case PVEC_OBARRAY:
#endif
      mps_finalize (global_igc->arena, &ref);
      break;

    case PVEC_NORMAL_VECTOR:
    case PVEC_FREE:
    case PVEC_MARKER:
    case PVEC_OVERLAY:
    case PVEC_SYMBOL_WITH_POS:
    case PVEC_MISC_PTR:
    case PVEC_PROCESS:
    case PVEC_FRAME:
    case PVEC_WINDOW:
    case PVEC_BOOL_VECTOR:
    case PVEC_BUFFER:
    case PVEC_TERMINAL:
    case PVEC_XWIDGET:
    case PVEC_XWIDGET_VIEW:
    case PVEC_OTHER:
    case PVEC_WINDOW_CONFIGURATION:
    case PVEC_TS_NODE:
    case PVEC_SQLITE:
    case PVEC_COMPILED:
    case PVEC_CHAR_TABLE:
    case PVEC_SUB_CHAR_TABLE:
    case PVEC_RECORD:
#ifdef IN_MY_FORK
    case PVEC_PACKAGE:
#endif
    case PVEC_WEAK_REF:
    case PVEC_MODULE_GLOBAL_REFERENCE:
      break;
    }
}

/* Process MPS messages. This should be extended to handle messages only
   for a certain amoount of time. See mps_clock_t, mps_clock, and
   mps_clocks_per_sec functions.  */

static void
process_messages (struct igc *gc)
{
  mps_message_type_t type;
  while (mps_message_queue_type (&type, gc->arena))
    {
      mps_message_t msg;
      if (!mps_message_get (&msg, gc->arena, type))
	continue;

      if (type == mps_message_type_finalization ())
	{
	  mps_addr_t base_addr;
	  mps_message_finalization_ref (&base_addr, gc->arena, msg);
	  finalize (gc, base_addr);
	}
      else if (type == mps_message_type_gc_start ())
	{
	  if (garbage_collection_messages)
	    {
	      message1_nolog ("Garbage collecting...");
	      const char *why = mps_message_gc_start_why (gc->arena, msg);
	      message1_nolog (why);
	    }
	}

      mps_message_discard (gc->arena, msg);
    }
}

static void
enable_messages (struct igc *gc, bool enable)
{
  void (*fun) (mps_arena_t, mps_message_type_t)
    = enable ? mps_message_type_enable : mps_message_type_disable;
  fun (gc->arena, mps_message_type_finalization ());
  fun (gc->arena, mps_message_type_gc_start ());
}

void
igc_process_messages (void)
{
  process_messages (global_igc);
}

void
igc_on_idle (void)
{
  process_messages (global_igc);
  mps_arena_step (global_igc->arena, 0.1, 0);
}

static mps_ap_t
thread_ap (enum igc_obj_type type)
{
  struct igc_thread_list *t = current_thread->gc_info;
  switch (type)
    {
    case IGC_OBJ_INVALID:
    case IGC_OBJ_PAD:
    case IGC_OBJ_FWD:
    case IGC_OBJ_LAST:
      emacs_abort ();

    case IGC_OBJ_WEAK:
      return t->d.weak_weak_ap;

    case IGC_OBJ_CONS:
    case IGC_OBJ_SYMBOL:
    case IGC_OBJ_INTERVAL:
    case IGC_OBJ_STRING:
    case IGC_OBJ_VECTOR:
    case IGC_OBJ_ITREE_TREE:
    case IGC_OBJ_ITREE_NODE:
    case IGC_OBJ_IMAGE:
    case IGC_OBJ_FACE:
    case IGC_OBJ_FACE_CACHE:
    case IGC_OBJ_BLV:
      return t->d.dflt_ap;

    case IGC_OBJ_STRING_DATA:
    case IGC_OBJ_FLOAT:
      return t->d.leaf_ap;
    }
  emacs_abort ();
}

/* Conditional breakpoints can be so slow that it is often more
   effective to instrument code. This fucntion is for such cases. */
void
igc_break (void)
{
}

void
igc_collect (void)
{
  struct igc *gc = global_igc;
  mps_arena_collect (gc->arena);
  mps_arena_release (gc->arena);
}

DEFUN ("igc--collect", Figc__collect, Sigc__collect, 0, 0, 0, doc
       : /* */)
(void)
{
  igc_collect ();
  return Qnil;
}

static unsigned
obj_hash (void)
{
  static unsigned obj_count = 0;
  if (obj_count == (1 << IGC_HASH_BITS))
    obj_count = 0;
  return obj_count++;
}

size_t
igc_hash (Lisp_Object key)
{
  mps_word_t word = XLI (key);
  mps_word_t tag = word & IGC_TAG_MASK;
  mps_addr_t client = NULL;
  switch (tag)
    {
    case Lisp_Type_Unused0:
      emacs_abort ();

    case Lisp_Int0:
    case Lisp_Int1:
      return word;

    case Lisp_Symbol:
      {
	ptrdiff_t off = word ^ tag;
	client = (mps_addr_t) ((char *) lispsym + off);
      }
      break;

    case Lisp_String:
    case Lisp_Vectorlike:
    case Lisp_Cons:
    case Lisp_Float:
      client = (mps_addr_t) (word ^ tag);
      break;
    }

  if (is_mps (client))
    {
      // The following assertion is very expensive.
      // igc_assert (mps_arena_has_addr (global_igc->arena, client));
      struct igc_header *h = client_to_base (client);
      return h->hash;
    }

  return word;
}

static mps_addr_t
alloc (size_t size, enum igc_obj_type type, enum pvec_type pvec_type)
{
  mps_ap_t ap = thread_ap (type);
  mps_addr_t p, obj;
  size = obj_size (size);
  do
    {
      mps_res_t res = mps_reserve (&p, ap, size);
      if (res)
	memory_full (0);
      // Object _must_ have valid contents before commit
      memclear (p, size);
      struct igc_header *h = p;
      h->obj_type = type;
      h->pvec_type = pvec_type;
      h->hash = obj_hash ();
#if IGC_SIZE_BITS >= 32 && INTPTR_MAX > INT_MAX
      /* On 32-bit architecture the assertion below is redudnant and
         causes compiler warnings.  */
      igc_assert (size < ((size_t) 1 << IGC_SIZE_BITS));
#endif
      h->nwords = to_words (size);
      obj = base_to_client (p);
    }
  while (!mps_commit (ap, p, size));
  return obj;
}

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  struct Lisp_Cons *cons = alloc (sizeof *cons, IGC_OBJ_CONS, PVEC_FREE);
  cons->u.s.car = car;
  cons->u.s.u.cdr = cdr;
  return make_lisp_ptr (cons, Lisp_Cons);
}

Lisp_Object
igc_alloc_symbol (void)
{
  struct Lisp_Symbol *sym = alloc (sizeof *sym, IGC_OBJ_SYMBOL, PVEC_FREE);
  return make_lisp_symbol (sym);
}

Lisp_Object
igc_make_float (double val)
{
  struct Lisp_Float *f = alloc (sizeof *f, IGC_OBJ_FLOAT, PVEC_FREE);
  f->u.data = val;
  return make_lisp_ptr (f, Lisp_Float);
}

static unsigned char *
alloc_string_data (size_t nbytes, bool clear)
{
  unsigned char *data = alloc (nbytes + 1, IGC_OBJ_STRING_DATA, PVEC_FREE);
  data[nbytes] = 0;
  return data;
}

/* Reallocate multibyte STRING data when a single character is
   replaced. The character is at byte offset BYTE_POS in the string.
   The character being replaced is CHAR_LEN bytes long, and the
   character that will replace it is NEW_CLEN bytes long.  Return the
   address where the caller should store the new character.  */
unsigned char *
igc_replace_char (Lisp_Object string, ptrdiff_t at_byte_pos,
		  ptrdiff_t old_char_len, ptrdiff_t new_char_len)
{
  struct Lisp_String *s = XSTRING (string);

  // Replacing caaracters of the same length.
  if (old_char_len == new_char_len)
    return s->u.s.data + at_byte_pos;

  ptrdiff_t old_nbytes = SBYTES (string);
  ptrdiff_t nbytes_needed = old_nbytes + (new_char_len - old_char_len);
  struct igc_header *old_header = client_to_base (s->u.s.data);
  ptrdiff_t capacity = to_bytes (old_header->nwords) - sizeof *old_header;
  if (capacity < nbytes_needed)
    {
      unsigned char *new_data = alloc_string_data (nbytes_needed, false);
      memcpy (new_data, SDATA (string), old_nbytes);
      s->u.s.data = new_data;
    }

  // Set up string as if the character had been inserted.
  s->u.s.size_byte = nbytes_needed;
  unsigned char *insertion_addr = s->u.s.data + at_byte_pos;
  memmove (insertion_addr + new_char_len, insertion_addr + old_char_len,
	   eabs (new_char_len - old_char_len));
  return insertion_addr;
}

Lisp_Object
igc_make_string (size_t nchars, size_t nbytes, bool unibyte, bool clear)
{
  struct Lisp_String *s = alloc (sizeof *s, IGC_OBJ_STRING, PVEC_FREE);
  s->u.s.size = nchars;
  s->u.s.size_byte = unibyte ? -1 : nbytes;
  s->u.s.data = alloc_string_data (nbytes, clear);
  return make_lisp_ptr (s, Lisp_String);
}

Lisp_Object
igc_make_multibyte_string (size_t nchars, size_t nbytes, bool clear)
{
  return igc_make_string (nchars, nbytes, false, clear);
}

Lisp_Object
igc_make_unibyte_string (size_t nchars, size_t nbytes, bool clear)
{
  return igc_make_string (nchars, nbytes, true, clear);
}

struct interval *
igc_make_interval (void)
{
  return alloc (sizeof (struct interval), IGC_OBJ_INTERVAL, PVEC_FREE);
}

struct Lisp_Vector *
igc_alloc_pseudovector (size_t nwords_mem, size_t nwords_lisp,
			size_t nwords_zero, enum pvec_type tag)
{
  struct Lisp_Vector *v
    = alloc (header_size + nwords_mem * word_size, IGC_OBJ_VECTOR, tag);
  XSETPVECTYPESIZE (v, tag, nwords_lisp, nwords_mem - nwords_lisp);
  maybe_finalize (v, tag);
  return v;
}

struct Lisp_Vector *
igc_alloc_vector (ptrdiff_t len)
{
  struct Lisp_Vector *v
    = alloc (header_size + len * word_size, IGC_OBJ_VECTOR, PVEC_NORMAL_VECTOR);
  v->header.size = len;
  return v;
}

struct Lisp_Vector *
igc_alloc_record (ptrdiff_t len)
{
  struct Lisp_Vector *v
    = alloc (header_size + len * word_size, IGC_OBJ_VECTOR, PVEC_RECORD);
  v->header.size = len;
  XSETPVECTYPE (v, PVEC_RECORD);
  return v;
}

struct itree_tree *
igc_make_itree_tree (void)
{
  struct itree_tree *t = alloc (sizeof *t, IGC_OBJ_ITREE_TREE, PVEC_FREE);
  return t;
}

struct itree_node *
igc_make_itree_node (void)
{
  struct itree_node *n = alloc (sizeof *n, IGC_OBJ_ITREE_NODE, PVEC_FREE);
  return n;
}

#ifdef HAVE_WINDOW_SYSTEM
struct image *
igc_make_image (void)
{
  struct image *img = alloc (sizeof *img, IGC_OBJ_IMAGE, PVEC_FREE);
  return img;
}
#endif

struct face *
igc_make_face (void)
{
  struct face *face = alloc (sizeof *face, IGC_OBJ_FACE, PVEC_FREE);
  return face;
}

struct face_cache *
igc_make_face_cache (void)
{
  struct face_cache *c = alloc (sizeof *c, IGC_OBJ_FACE_CACHE, PVEC_FREE);
  return c;
}

DEFUN ("igc-make-weak-ref", Figc_make_weak_ref, Sigc_make_weak_ref, 1, 1, 0,
       doc
       : /* todo */)
(Lisp_Object target)
{
  const enum pvec_type type = PVEC_WEAK_REF;
  struct Lisp_Weak_Ref *wref = alloc (sizeof *wref, IGC_OBJ_WEAK, type);
  int nwords_lisp = VECSIZE (struct Lisp_Weak_Ref);
  XSETPVECTYPESIZE (wref, type, nwords_lisp, 0);
  wref->ref = target;
  Lisp_Object obj = make_lisp_ptr (wref, Lisp_Vectorlike);
  return obj;
}

static void
CHECK_WEAK_REF_P (Lisp_Object x)
{
  CHECK_TYPE (WEAK_REF_P (x), Qweak_ref_p, x);
}

Lisp_Object
igc_weak_ref_deref (struct Lisp_Weak_Ref *wref)
{
  return wref->ref;
}

DEFUN ("igc-weak-ref-deref", Figc_weak_reaf_deref, Sigc_weak_ref_deref, 1, 1,
       0, doc
       : /* todo */)
(Lisp_Object obj)
{
  CHECK_WEAK_REF_P (obj);
  return igc_weak_ref_deref (XWEAK_REF (obj));
}

struct Lisp_Buffer_Local_Value *igc_alloc_blv (void)
{
  struct Lisp_Buffer_Local_Value *blv
      = alloc (sizeof *blv, IGC_OBJ_BLV, PVEC_FREE);
  return blv;
}

int
igc_valid_lisp_object_p (Lisp_Object obj)
{
  return 1;
}

DEFUN ("igc-info", Figc_info, Sigc_info, 0, 0, 0, doc : /* */)
(void)
{
  struct igc *gc = global_igc;
  struct igc_stats st = { 0 };
  mps_res_t res;
  IGC_WITH_PARKED (gc) { res = mps_pool_walk (gc->dflt_pool, dflt_scanx, &st); }
  if (res != MPS_RES_OK)
    error ("Error %d walking memory", res);

  Lisp_Object result = Qnil;
  for (int i = 0; i < IGC_OBJ_LAST; ++i)
    {
      Lisp_Object e
	= list3 (build_string (obj_type_names[i]), make_int (st.obj[i].nobjs),
		 make_int (st.obj[i].nwords));
      result = Fcons (e, result);
    }

  return result;
}

DEFUN ("igc-roots", Figc_roots, Sigc_roots, 0, 0, 0, doc : /* */)
(void)
{
  struct igc *gc = global_igc;
  Lisp_Object roots = Qnil;

  for (igc_root_list *r = gc->roots; r; r = r->next)
    {
      Lisp_Object type = r->d.ambig ? Qambig : Qexact;
      Lisp_Object e = list3 (type,
			     make_int ((intmax_t) (ptrdiff_t) r->d.start),
			     make_int ((intmax_t) (ptrdiff_t) r->d.end));
      roots = Fcons (e, roots);
    }

  return roots;
}

static void
arena_extended (mps_arena_t arena, void *base, size_t size)
{
  if (min_addr == NULL || base < min_addr)
    min_addr = base;
  mps_addr_t end = (char *) base + size;
  if (max_addr == NULL || end > max_addr)
    max_addr = end;
}

static void
arena_contracted (mps_arena_t arena, void *base, size_t size)
{
  /* Can MPS free something that is in the middle? */
  mps_addr_t end = (char *) base + size;
  if (end == max_addr)
    max_addr = base;
  if (base == min_addr)
    min_addr = end;
}

static void
make_arena (struct igc *gc)
{
  mps_res_t res;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_ARENA_EXTENDED, (mps_fun_t) &arena_extended);
    MPS_ARGS_ADD (args, MPS_KEY_ARENA_CONTRACTED,
		  (mps_fun_t) &arena_contracted);
    res = mps_arena_create_k (&gc->arena, mps_arena_class_vm (), args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);

  mps_gen_param_s gens[] = { { 128000, 0.8 }, { 5 * 128000, 0.4 } };
  res = mps_chain_create (&gc->chain, gc->arena, ARRAYELTS (gens), gens);
  IGC_CHECK_RES (res);
}

static mps_fmt_t
make_dflt_fmt (struct igc *gc)
{
  mps_res_t res;
  mps_fmt_t fmt;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ALIGN, IGC_ALIGN);
    /* Don't use in-band headers. I suspect they ahve problems,
       specifically amcSegScanNailedRange calls NailboardGet with a
       client address, which calls NailboardGet, and one can see that
       the the board contains base addresses which leads to an assertion
       failure. */
    MPS_ARGS_ADD (args, MPS_KEY_FMT_HEADER_SIZE, 0);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_SCAN, dflt_scan);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_SKIP, dflt_skip);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_FWD, dflt_fwd);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_ISFWD, is_dflt_fwd);
    MPS_ARGS_ADD (args, MPS_KEY_FMT_PAD, dflt_pad);
    res = mps_fmt_create_k (&fmt, gc->arena, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
  return fmt;
}

static mps_pool_t
make_pool_with_class (struct igc *gc, mps_fmt_t fmt, mps_class_t cls)
{
  mps_res_t res;
  mps_pool_t pool;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_FORMAT, fmt);
    MPS_ARGS_ADD (args, MPS_KEY_CHAIN, gc->chain);
    // Must use interior pointers, bc we hand out client pointers.
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, true);
    res = mps_pool_create_k (&pool, gc->arena, cls, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
  return pool;
}

static mps_pool_t
make_pool_amc (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_amc ());
}

static mps_pool_t
make_pool_awl (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_awl ());
}

static mps_pool_t
make_pool_amcz (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_amcz ());
}

static struct igc *
make_igc (void)
{
  struct igc *gc = xzalloc (sizeof *gc);
  make_arena (gc);

  gc->dflt_fmt = make_dflt_fmt (gc);
  gc->dflt_pool = make_pool_amc (gc, gc->dflt_fmt);
  gc->leaf_fmt = make_dflt_fmt (gc);
  gc->leaf_pool = make_pool_amcz (gc, gc->leaf_fmt);
  gc->weak_fmt = make_dflt_fmt (gc);
  gc->weak_pool = make_pool_awl (gc, gc->weak_fmt);

  root_create_igc (gc);
#ifndef IN_MY_FORK
  root_create_pure (gc);
#endif
  root_create_buffer (gc, &buffer_defaults);
  root_create_buffer (gc, &buffer_local_symbols);
  root_create_staticvec (gc);
  root_create_lispsym (gc);
  root_create_terminal_list (gc);
  root_create_main_thread (gc);
  root_create_rdstack (gc);

  enable_messages (gc, true);
  return gc;
}

/* To call from LLDB. */

void
igc_postmortem (void)
{
  mps_arena_postmortem (global_igc->arena);
}

void
init_igc (void)
{
  mps_lib_assert_fail_install (igc_assert_fail);
  global_igc = make_igc ();
  add_main_thread ();
}

void
syms_of_igc (void)
{
  defsubr (&Sigc_info);
  defsubr (&Sigc_roots);
  defsubr (&Sigc_make_weak_ref);
  defsubr (&Sigc_weak_ref_deref);
  defsubr (&Sigc__collect);
  DEFSYM (Qambig, "ambig");
  DEFSYM (Qexact, "exact");
  DEFSYM (Qweak_ref_p, "weak-ref-p");
  DEFSYM (Qweak_ref, "weak-ref");
  Fprovide (intern_c_string ("mps"), Qnil);
}
