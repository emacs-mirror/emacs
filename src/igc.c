/* Incremental and generational GC using MPS.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>. */

// clang-format on

#include <config.h>
#include <limits.h>
#include <signal.h>
#ifdef __clang__
/* You want to use this without -Wignored-attributes because it warns
   that it cannot add the attribute to functions returning void.  */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wignored-attributes"
# pragma clang attribute push(__attribute__((warn_unused_result)), \
			      apply_to = hasType(functionType))
#endif
#include <mps.h>
#include <mpsavm.h>
#include <mpscamc.h>
#include "mpscams.h"
#include <mpscawl.h>
#include <mpslib.h>
#ifdef __clang__
#pragma clang attribute pop
#pragma clang diagnostic pop
#endif
#include <stdlib.h>
#include "lisp.h"
#include "comp.h"
#include "igc.h"
#include "bignum.h"
#include "buffer.h"
#include "coding.h"
#include "charset.h"
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
#include "termchar.h"
#ifdef HAVE_WINDOW_SYSTEM
#include TERM_HEADER
#endif /* HAVE_WINDOW_SYSTEM */
#ifdef HAVE_XWIDGETS
# include "xwidget.h"
#endif

#if !USE_LSB_TAG
# error "USE_LSB_TAG required"
#endif
#if USE_STACK_LISP_OBJECTS
# error "USE_STACK_LISP_OBJECTS not supported"
#endif
#ifndef HAVE_PDUMPER
# error "HAVE_PDUMPER required"
#endif

#if 0 /* Not yet because that make transfer between GNU and my fork
	 painful.  */
#ifdef CHECK_STRUCTS
# include "dmpstruct.h"

/* If one of these fires, check what has changed, fix uses of the type
   in this file, and copy the new hash code from src/dmpstruct.h here.  */
# ifndef HASH_Lisp_Cons_8E09073155
#  error "struct Lisp_Cons changed"
# endif
# ifndef HASH_interval_DE36B11AE7
#  error "struct interval changed"
# endif
# ifndef HASH_Lisp_String_39D7E563BF
#  error "struct Lisp_String changed"
# endif
# ifndef HASH_Lisp_Marker_D850433F19
#  error "struct Lisp_Marker changed"
# endif
# ifndef HASH_itree_node_8AF9E94FBA
#  error "struct itree_node changed"
# endif
# ifndef HASH_Lisp_Overlay_AF021DC256
#  error "struct Lisp_Overlay changed"
# endif
# ifndef HASH_Lisp_Finalizer_7DACDD23C5
#  error "struct Lisp_Finalizer changed"
# endif
# ifndef HASH_Lisp_Bignum_8732048B98
#  error "struct Lisp_Bignum changed"
# endif
# ifndef HASH_Lisp_Float_4F10F019A4
#  error "struct Lisp_Float changed"
# endif
# ifndef HASH_Lisp_Buffer_Local_Value_B1EFDA61AE
#  error "struct Lisp_Buffer_Local_Value changed"
# endif
# ifndef HASH_Lisp_Symbol_78773EECA2
#  error "struct Lisp_Symbol changed"
# endif
# ifndef HASH_symbol_redirect_EA72E4BFF5
#  error "enum symbol_redirect changed"
# endif
# ifndef HASH_vectorlike_header_AF1B22D957
#  error "struct vectorlike_header changed"
# endif
# ifndef HASH_Lisp_Hash_Table_18BE341ECB
#  error "struct Lisp_Hash_Table changed"
# endif
# ifndef HASH_buffer_DF486266DF
#  error "struct buffer changed"
# endif
# ifndef HASH_Lisp_Vector_64AF2E46CE
#  error "struct Lisp_Vector changed"
# endif
# ifndef HASH_Lisp_Subr_6FC9E39686
#  error "struct Lisp_Subr changed"
# endif
# ifndef HASH_pvec_type_1C9DBCD69F
#  error "enum pvec_type changed"
# endif
# ifndef HASH_Lisp_Type_45F0582FD7
#  error "enum Lisp_Type changed"
# endif
# ifndef HASH_charset_84DCEA663B
#  error "struct charset changed"
# endif
# ifndef HASH_itree_tree_A8CE87B78A
#  error "struct itree_tree changed"
# endif
# endif // 0

/* Still missing; face, font, frame, thread, and probably a lot of
   others.  */
#endif /* CHECK_STRUCTS */

/* If igc can currently can be used.

   Initial state is IGC_STATE_INITIAL, until everything needed has been
   successfully initialized.

   State does from IGC_STATE_INITIAL to IGC_STATE_USABLE_PARKED where
   everything is usable, but GC is not done.

   State then goes from there to IGC_STATE_USABLE when everything is
   fully usable and GCs are done.

   It goes from usable to IGC_STATE_DEAD if an error happens or
   something is detected that forces us to terminate the process
   early.  While terminating in this state, fallbacks are implemented
   that let Emacs do its thing while terminating.  */

enum igc_state
{
  IGC_STATE_INITIAL,
  IGC_STATE_USABLE_PARKED,
  IGC_STATE_USABLE,
  IGC_STATE_DEAD,
};

static enum igc_state igc_state = IGC_STATE_INITIAL;
static void set_state (enum igc_state state);

static void
check_res (const char *file, unsigned line, mps_res_t res)
{
  if (res != MPS_RES_OK)
    {
      fprintf (stderr, "\r\n%s:%u: Emacs fatal error: MPS error code %d\r\n",
	       file, line, res);
      set_state (IGC_STATE_DEAD);
    }
}

#define IGC_CHECK_RES(res) check_res (__FILE__, __LINE__, (res))

/* Function called from MPS and from igc on assertion failures.
   The function signature must be that of mps_lib_assert_fail_t.  */

static void
igc_assert_fail (const char *file, unsigned line, const char *msg)
{
  fprintf (stderr, "\r\n%s:%u: Emacs fatal error: assertion failed: %s\r\n",
	   file, line, msg);
  set_state (IGC_STATE_DEAD);
}

#ifdef IGC_DEBUG
# define igc_assert(expr)				\
  do							\
    {							\
      if (!(expr))					\
	igc_assert_fail (__FILE__, __LINE__, #expr);	\
    }							\
  while (0)
#else
# define igc_assert(expr) ((void) (true || (expr)))
#endif

#define IGC_NOT_IMPLEMENTED() \
  igc_assert_fail (__FILE__, __LINE__, "not implemented")

/* An enum for telemetry event categories seems to be missing from MOS.
   The docs only mention the bare numbers.  */

enum igc_event_category
{
  IGC_EVC_ARENA,
  IGC_EVC_POOL,
  IGC_EVC_TRACE,
  IGC_EVC_SEG,
  IGC_EVC_REF,
  IGC_EVC_OBJECT,
  IGC_EVC_USER,
};

/* Return true if the current MPS telemetry event filter has the bit set
   for the given event category C.  */

static bool
is_in_telemetry_filter (enum igc_event_category c)
{
  return (mps_telemetry_get () & (1 << c)) != 0;
}

#if 0	/* unused */
#ifdef __cplusplus
#define igc_const_cast(type, expr) const_cast<type>(expr))
#else
#define igc_const_cast(type, expr) ((type) (expr))
#endif
#endif	/* unused */

enum
{
  IGC_TAG_MASK = (~VALMASK),
  IGC_TAG_BITS = GCTYPEBITS,
  IGC_ALIGN = GCALIGNMENT,
  IGC_ALIGN_DFLT = IGC_ALIGN,
};

static bool
is_pure (const mps_addr_t addr)
{
#ifdef IN_MY_FORK
  return false;
#else
  return PURE_P (addr);
#endif
}

static enum pvec_type
pseudo_vector_type (const struct Lisp_Vector *v)
{
  return PSEUDOVECTOR_TYPE (v);
}

static bool
is_builtin_subr (enum igc_obj_type type, void *client)
{
  if (type == IGC_OBJ_VECTOR && pseudo_vector_type (client) == PVEC_SUBR)
    {
      Lisp_Object subr = make_lisp_ptr (client, Lisp_Vectorlike);
      return !NATIVE_COMP_FUNCTIONP (subr);
    }
  return false;
}

static bool
is_aligned (const mps_addr_t addr)
{
  return ((mps_word_t) addr & IGC_TAG_MASK) == 0;
}

#define IGC_WITH_PARKED(gc) \
  for (int i = (arena_park (gc), 1); i; i = (arena_release (gc), 0))

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

static const char *obj_type_names[] = {
  "IGC_OBJ_INVALID",
  "IGC_OBJ_PAD",
  "IGC_OBJ_FWD",
  "IGC_OBJ_CONS",
  "IGC_OBJ_SYMBOL",
  "IGC_OBJ_INTERVAL",
  "IGC_OBJ_STRING",
  "IGC_OBJ_STRING_DATA",
  "IGC_OBJ_VECTOR",
  "IGC_OBJ_MARKER_VECTOR",
  "IGC_OBJ_ITREE_TREE",
  "IGC_OBJ_ITREE_NODE",
  "IGC_OBJ_IMAGE",
  "IGC_OBJ_IMAGE_CACHE",
  "IGC_OBJ_FACE",
  "IGC_OBJ_FACE_CACHE",
  "IGC_OBJ_FLOAT",
  "IGC_OBJ_BLV",
  "IGC_OBJ_HANDLER",
  "IGC_OBJ_BYTES",
  "IGC_OBJ_BUILTIN_SYMBOL",
  "IGC_OBJ_BUILTIN_THREAD",
  "IGC_OBJ_BUILTIN_SUBR",
  "IGC_OBJ_DUMPED_CHARSET_TABLE",
  "IGC_OBJ_DUMPED_CODE_SPACE_MASKS",
  "IGC_OBJ_DUMPED_BUFFER_TEXT",
  "IGC_OBJ_DUMPED_BIGNUM_DATA",
  "IGC_OBJ_DUMPED_BYTES",
  "IGC_OBJ_WEAK_HASH_TABLE_WEAK_PART",
  "IGC_OBJ_WEAK_HASH_TABLE_STRONG_PART",
};

static_assert (ARRAYELTS (obj_type_names) == IGC_OBJ_NUM_TYPES);

static const char *
obj_type_name (enum igc_obj_type type)
{
  igc_assert (0 <= type && type < IGC_OBJ_NUM_TYPES);
  return obj_type_names[type];
}

static const char *pvec_type_names[] = {
  "PVEC_NORMAL_VECTOR",
  "PVEC_FREE",
  "PVEC_BIGNUM",
  "PVEC_MARKER",
  "PVEC_OVERLAY",
  "PVEC_FINALIZER",
  "PVEC_SYMBOL_WITH_POS",
  "PVEC_MISC_PTR",
  "PVEC_USER_PTR",
  "PVEC_PROCESS",
  "PVEC_FRAME",
  "PVEC_WINDOW",
  "PVEC_BOOL_VECTOR",
  "PVEC_BUFFER",
  "PVEC_HASH_TABLE",
  "PVEC_WEAK_HASH_TABLE",
#ifndef IN_MY_FORK
  "PVEC_OBARRAY",
#endif
  "PVEC_TERMINAL",
  "PVEC_WINDOW_CONFIGURATION",
  "PVEC_SUBR",
#ifdef IN_MY_FORK
  "PVEC_PACKAGE",
#endif
  "PVEC_OTHER",
  "PVEC_XWIDGET",
  "PVEC_XWIDGET_VIEW",
  "PVEC_THREAD",
  "PVEC_MUTEX",
  "PVEC_CONDVAR",
  "PVEC_MODULE_FUNCTION",
  "PVEC_MODULE_GLOBAL_REFERENCE",
  "PVEC_NATIVE_COMP_UNIT",
  "PVEC_TS_PARSER",
  "PVEC_TS_NODE",
  "PVEC_TS_COMPILED_QUERY",
  "PVEC_SQLITE",
  "PVEC_CLOSURE",
  "PVEC_CHAR_TABLE",
  "PVEC_SUB_CHAR_TABLE",
  "PVEC_RECORD",
  "PVEC_FONT",
};

static_assert (ARRAYELTS (pvec_type_names) == PVEC_TAG_MAX + 1);

static const char *
pvec_type_name (enum pvec_type type)
{
  igc_assert (0 <= type && type <= PVEC_TAG_MAX);
  return pvec_type_names[type];
}

struct igc_stats
{
  struct
  {
    size_t nbytes;
    size_t nobjs;
    size_t largest;
  } obj[IGC_OBJ_NUM_TYPES];

  struct
  {
    size_t nbytes;
    size_t nobjs;
    size_t largest;
  } pvec[PVEC_TAG_MAX + 1];
};

/* Always having a header makes it possible to have an
   address-independent hash, which is (a) much easier to handle than MPS
   location dependencies, and (b) makes it possible to implement sxhash
   variants in a way that works as expected even if GCs happen between
   calls.

   The reason for the strange layout of this header is that on IA-32,
   MPS requires headers for objects with weak references to consist of
   two unaligned 32-bit words.  It's easiest to implement that for all
   objects.  */

enum
{
  IGC_HEADER_TAG_BITS = 2,
  IGC_HEADER_TYPE_BITS = 6,
  IGC_HEADER_HASH_BITS = 24,
#if INTPTR_MAX <= INT_MAX
  IGC_HEADER_NWORDS_BITS = 31,
#else
  IGC_HEADER_NWORDS_BITS = 32,
#endif
  IGC_HEADER_TAG_MASK = (1 << IGC_HEADER_TAG_BITS) - 1,
  IGC_HEADER_TYPE_MASK = (1 << IGC_HEADER_TYPE_BITS) - 1,
  IGC_HEADER_HASH_MASK = (1 << IGC_HEADER_HASH_BITS) - 1,

  IGC_HEADER_TAG_SHIFT = 0,
  IGC_HEADER_TYPE_SHIFT = IGC_HEADER_TAG_SHIFT + IGC_HEADER_TAG_BITS,
  IGC_HEADER_HASH_SHIFT = IGC_HEADER_TYPE_SHIFT + IGC_HEADER_TYPE_BITS,
  IGC_HEADER_NWORDS_SHIFT = 64 - IGC_HEADER_NWORDS_BITS,
};

static_assert (IGC_OBJ_NUM_TYPES - 1 < (1 << IGC_HEADER_TYPE_BITS));

struct igc_exthdr
{
  EMACS_UINT nwords;
  EMACS_UINT hash;
  enum igc_obj_type obj_type;
  Lisp_Object extra_dependency;
};

enum igc_tag
{
  IGC_TAG_NULL = 0, /* entire value must be 0 to avoid MPS issues */
  IGC_TAG_OBJ = 1, /* IGC object */
  IGC_TAG_EXTHDR = 2, /* pointer to aligned external header */
};

#ifdef IN_MY_FORK

/* After Pip Cet's header changes, Lisp objects include a gc_header
   union, which has an uint64_t member 'v'. struct igc_header then
   contained the same 'v', and not bitfields anymore. This makes things
   inconvenient in LLDB.  */

struct igc_header
{
  enum igc_tag tag : IGC_HEADER_TAG_BITS;
  enum igc_obj_type obj_type : IGC_HEADER_TYPE_BITS;
  mps_word_t hash : IGC_HEADER_HASH_BITS;
  mps_word_t nwords : IGC_HEADER_NWORDS_BITS;
};

static unsigned
header_nwords (const struct igc_header *h)
{
  return h->nwords;
}

static unsigned
header_hash (const struct igc_header *h)
{
  return h->hash;
}

static enum igc_obj_type
header_type (const struct igc_header *h)
{
  return h->obj_type;
}

static uint64_t
header_tag (const struct igc_header *h)
{
  return h->tag;
}

static struct igc_exthdr *
header_exthdr (const struct igc_header *h)
{
  uint64_t v = *(uint64_t *) h;
  return ((struct igc_exthdr *)(intptr_t)(v & ~IGC_HEADER_TAG_MASK));
}

#else

struct igc_header
{
  uint64_t v;
};

static unsigned
header_nwords (const struct igc_header *h)
{
  return h->v >> IGC_HEADER_NWORDS_SHIFT;
}

static unsigned
header_hash (const struct igc_header *h)
{
  return (h->v >> IGC_HEADER_HASH_SHIFT) & IGC_HEADER_HASH_MASK;
}

static enum igc_obj_type
header_type (const struct igc_header *h)
{
  return (h->v >> IGC_HEADER_TYPE_SHIFT) & IGC_HEADER_TYPE_MASK;
}

static uint64_t
header_tag (const struct igc_header *h)
{
  return h->v & IGC_HEADER_TAG_MASK;
}

static struct igc_exthdr *
header_exthdr (const struct igc_header *h)
{
  return ((struct igc_exthdr *)(intptr_t)(h->v & ~IGC_HEADER_TAG_MASK));
}

#endif /* not IN_MY_FORK */

unsigned igc_header_hash (struct igc_header *h);
size_t igc_header_nwords (const struct igc_header *h);
enum igc_obj_type igc_header_type (struct igc_header *h);

enum igc_obj_type
igc_header_type (struct igc_header *h)
{
  if (header_tag (h) == IGC_TAG_EXTHDR)
    return header_exthdr (h)->obj_type;
  return header_type (h);
}

unsigned
igc_header_hash (struct igc_header *h)
{
  if (header_tag (h) == IGC_TAG_EXTHDR)
    return header_exthdr (h)->hash;
  return header_hash (h);
}

size_t
igc_header_nwords (const struct igc_header *h)
{
  if (header_tag (h) == IGC_TAG_EXTHDR)
    return header_exthdr (h)->nwords;
  return header_nwords (h);
}

struct igc_fwd
{
  struct igc_header header;
  mps_addr_t new_base_addr;
};

static_assert (sizeof (struct igc_header) == 8);

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

/* Value is the size in bytes of the object described by header H.
   This includes the header itself.  */

static mps_word_t
obj_size (const struct igc_header *h)
{
  mps_word_t nbytes = to_bytes (igc_header_nwords (h));
  igc_assert (header_type (h) == IGC_OBJ_PAD || nbytes >= sizeof (struct igc_fwd));
  return nbytes;
}

/* Value is the size in bytes of the client area of the object described
   by header H.  */

static mps_word_t
obj_client_size (const struct igc_header *h)
{
  return obj_size (h) - sizeof *h;
}

/* Set the fields of header H to the given values.  Use this instead of
   setting the fields directly to make it easy to add assertions.  */

static void
set_header (struct igc_header *h, enum igc_obj_type type,
	    mps_word_t nbytes, mps_word_t hash)
{
#if IGC_HEADER_NWORDS_BITS >= 32 && INTPTR_MAX > INT_MAX
  /* On 32-bit architecture the assertion below is redundant and
     causes compiler warnings.  */
  igc_assert (nbytes < ((size_t) 1 << IGC_HEADER_NWORDS_BITS));
#endif
  igc_assert (type == IGC_OBJ_PAD || nbytes >= sizeof (struct igc_fwd));
  uint64_t tag = IGC_TAG_OBJ;
  /* Make sure upper 32-bit word is unaligned on IA-32.  */
  if (INTPTR_MAX <= INT_MAX)
    tag += (1LL << 32);
  uint64_t v = (((uint64_t) to_words (nbytes) << IGC_HEADER_NWORDS_SHIFT)
		+ ((hash & IGC_HEADER_HASH_MASK) << (IGC_HEADER_HASH_SHIFT))
		+ (type << IGC_HEADER_TYPE_SHIFT) + tag);
  *(uint64_t *) h = v;
}

static unsigned alloc_hash (void);
static size_t igc_round (size_t nbytes, size_t align);

/* Called throughout Emacs to initialize the GC header of an object
   which does not live in GC-managed memory, such as pure objects and
   builtin symbols.  */
void gc_init_header (union gc_header *header, enum igc_obj_type type)
{
  struct igc_header *h = (struct igc_header *)header;
  switch (type)
    {
    case IGC_OBJ_CONS:
      set_header (h, IGC_OBJ_CONS, sizeof (struct Lisp_Cons), alloc_hash ());
      break;
    case IGC_OBJ_STRING:
      set_header (h, IGC_OBJ_STRING, sizeof (struct Lisp_String), alloc_hash ());
      break;
    case IGC_OBJ_FLOAT:
      set_header (h, IGC_OBJ_FLOAT, sizeof (struct Lisp_Float), alloc_hash ());
      break;
    case IGC_OBJ_SYMBOL:
      set_header (h, IGC_OBJ_SYMBOL, sizeof (struct Lisp_Symbol), alloc_hash ());
      break;
    case IGC_OBJ_VECTOR:
      {
	ssize_t nbytes;
	ptrdiff_t size = ((struct Lisp_Vector *)header)->header.size;
	if (size & PSEUDOVECTOR_FLAG)
	  {
	    /* Correct some incorrect pseudovector headers:
	     * - lisp.h sets the pseudovector tag of builtin subrs to
	     *   PVEC_SUBR, but doesn't set the pseudovector flag or the
	     *   lispsize/restsize fields.
	     * - thread.c uses VECSIZE (struct thread_state) for the
	     *   restsize without subtracting the lispsize.
	     */
	    if (PSEUDOVECTOR_TYPE ((struct Lisp_Vector *)header) == PVEC_SUBR)
	      nbytes = sizeof (struct Lisp_Subr);
	    else if (PSEUDOVECTOR_TYPE ((struct Lisp_Vector *)header) == PVEC_THREAD)
	      nbytes = sizeof (struct thread_state);
	    else
	      nbytes = vectorlike_nbytes (&((struct Lisp_Vector *)header)->header);
	  }
	else
	  nbytes = size * sizeof (Lisp_Object) + header_size;
	set_header (h, IGC_OBJ_VECTOR, nbytes, alloc_hash ());
	break;
      }
    case IGC_OBJ_DUMPED_CHARSET_TABLE:
      break;
    default:
      emacs_abort ();
    }
}

void gc_init_header_bytes (union gc_header *header, enum igc_obj_type type,
			   size_t nbytes)
{
  struct igc_header *h = (struct igc_header *)header;
  switch (type)
    {
    case IGC_OBJ_STRING_DATA:
      set_header (h, IGC_OBJ_STRING_DATA, sizeof (struct Lisp_String_Data) +
		  igc_round (nbytes, IGC_ALIGN), alloc_hash());
      break;
    default:
      emacs_abort ();
    }
}

/* Round NBYTES to the next multiple of ALIGN.  */

static size_t
igc_round (size_t nbytes, size_t align)
{
  return ROUNDUP (nbytes, align);
}

/* Value is the size in bytes that we need to allocate from MPS
   for a client object of size NBYTES.  */

static size_t
alloc_size (size_t nbytes)
{
  nbytes = max (nbytes, sizeof (struct igc_fwd));
  nbytes = igc_round (nbytes, IGC_ALIGN_DFLT);
  return nbytes;
}

/* Value is a hash to use for a newly allocated object.  */

static unsigned
alloc_hash (void)
{
  static unsigned count = 0;
  return count++ & IGC_HEADER_HASH_MASK;
}

/* This runs in various places for --enable-checking=igc_check_fwd.  See
   lisp.h, like XSYMBOL, XSTRING and alike.  */

#ifdef IGC_CHECK_FWD
void
igc_check_fwd (void *client, bool is_vector)
{
  struct igc_header *h = client;
  igc_assert (header_type (h) != IGC_OBJ_FWD);
  igc_assert (obj_size (h) >= sizeof (struct igc_fwd));
}
#endif

/************************************************************************
                        Registry of MPS objects
 ************************************************************************/

/* Registry entry for an MPS root mps_root_t.  */

struct igc_root
{
  struct igc *gc;
  mps_root_t root;
  void *start, *end;
  const char *label;
  bool ambig;
};

typedef struct igc_root igc_root;
IGC_DEFINE_LIST (igc_root);

/* Registry entry for an MPS thread mps_thr_t.  */

struct igc_thread
{
  struct igc *gc;
  mps_thr_t thr;

  /* Allocation points for the thread.  */
  mps_ap_t dflt_ap;
  mps_ap_t leaf_ap;
  mps_ap_t weak_strong_ap;
  mps_ap_t weak_weak_ap;
  mps_ap_t weak_hash_strong_ap;
  mps_ap_t weak_hash_weak_ap;
  mps_ap_t immovable_ap;

  /* Quick access to the roots used for specpdl, bytecode stack and
     control stack.  */
  igc_root_list *specpdl_root;
  igc_root_list *bc_root;
  igc_root_list *stack_root;

  /* Back pointer to Emacs' thread object.  Allocated so that it doesn't
     move in memory.  */
  struct thread_state *ts;
};

typedef struct igc_thread igc_thread;
IGC_DEFINE_LIST (igc_thread);

/* The registry for an MPS arena.  There is only one arena used.  */

struct igc
{
  /* The MPS arena.  */
  mps_arena_t arena;

  /* Used to allow nested parking/releasing of the arena.  */
  int park_count;

  /* The MPS generation chain.  */
  mps_chain_t chain;

  /* Object formats and pools used.  */
  mps_fmt_t dflt_fmt;
  mps_pool_t dflt_pool;
  mps_fmt_t leaf_fmt;
  mps_pool_t leaf_pool;
  mps_fmt_t weak_fmt;
  mps_pool_t weak_pool;
  mps_fmt_t weak_hash_fmt;
  mps_pool_t weak_hash_pool;
  mps_fmt_t immovable_fmt;
  mps_pool_t immovable_pool;

  /* Registered roots.  */
  struct igc_root_list *roots;

  /* Registered threads.  */
  struct igc_thread_list *threads;
};

static bool process_one_message (struct igc *gc);

/* The global registry.  */

static struct igc *global_igc;

static void
arena_park (struct igc *gc)
{
  if (gc->park_count == 0)
    mps_arena_park (gc->arena);
  ++gc->park_count;
}

static void
arena_release (struct igc *gc)
{
  --gc->park_count;
  igc_assert (gc->park_count >= 0);
  if (gc->park_count == 0)
    mps_arena_release (gc->arena);
}

static void
set_state (enum igc_state state)
{
  enum igc_state old_state = igc_state;
  igc_state = state;
  switch (igc_state)
    {
    case IGC_STATE_INITIAL:
      emacs_abort ();

    case IGC_STATE_USABLE_PARKED:
      igc_assert (old_state == IGC_STATE_INITIAL);
      break;

    case IGC_STATE_USABLE:
      igc_assert (old_state == IGC_STATE_USABLE_PARKED);
      arena_release (global_igc);
      igc_assert (global_igc->park_count == 0);
      break;

    case IGC_STATE_DEAD:
      igc_postmortem ();
      terminate_due_to_signal (SIGABRT, INT_MAX);
      break;
    }
}

/* Register the root ROOT in registry GC with additional info.  START
   and END are the area of memory covered by the root.  END being NULL
   means not known.  AMBIG true means the root is scanned ambiguously, as
   opposed to being scanned exactly.

   LABEL is a name under which the root appears on the MPS telemetry
   stream, if user events are in the telemetry filter.  This allows
   mapping roots to useful names.  */

static struct igc_root_list *
register_root (struct igc *gc, mps_root_t root, void *start, void *end,
	       bool ambig, const char *label)
{
  igc_assert (label != NULL);
  if (is_in_telemetry_filter (IGC_EVC_USER))
    {
      mps_label_t l = mps_telemetry_intern (label);
      mps_telemetry_label (root, l);
    }
  struct igc_root r =
  {
    .gc = gc,
    .root = root,
    .start = start,
    .end = end,
    .ambig = ambig,
    .label = label,
  };
  return igc_root_list_push (&gc->roots, &r);
}

/* Remove the root described by R from the list of known roots
   of its registry.  Value is the MPS root.  */

static mps_root_t
deregister_root (struct igc_root_list *r)
{
  struct igc_root root;
  igc_root_list_remove (&root, &r->d.gc->roots, r);
  return root.root;
}

/* Destroy the root described by *R and remove it from its registry.
   Set *R to NULL when done so that it cannot be destroyed twice.  */

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

static size_t
vector_size (const struct Lisp_Vector *v)
{
  size_t size = v->header.size;
  if (size & PSEUDOVECTOR_FLAG)
    size &= PSEUDOVECTOR_SIZE_MASK;
  return size;
}

static size_t
vector_start (const struct Lisp_Vector *v)
{
  enum pvec_type type = pseudo_vector_type (v);
  return type == PVEC_SUB_CHAR_TABLE ? SUB_CHAR_TABLE_OFFSET : 0;
}

static mps_res_t
fix_lisp_obj (mps_ss_t ss, Lisp_Object *pobj)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_word_t *p = (mps_word_t *) pobj;
    mps_word_t word = *p;

    /* Quickly rule out Qnil, and prevent subtracting from a
       null pointer.  */
    if (word == 0)
      return MPS_RES_OK;

    mps_word_t tag = word & IGC_TAG_MASK;
    if (tag == Lisp_Int0 || tag == Lisp_Int1)
      return MPS_RES_OK;
    else if (tag == Lisp_Type_Unused0)
      emacs_abort ();

    if (tag == Lisp_Symbol)
      {
	ptrdiff_t off = word ^ tag;
	mps_addr_t client = (mps_addr_t) ((char *) lispsym + off);
	mps_addr_t base = client;
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    if (base == NULL)
	      *(Lisp_Object *) p = Qnil;
	    else
	      {
		client = base;
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
	mps_addr_t base = client;
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    if (base == NULL)
	      *(Lisp_Object *) p = Qnil;
	    else
	      {;
		client = base;
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
    if (client == NULL)
      return MPS_RES_OK;
    if (is_aligned (client))
      {
	mps_addr_t base = client;
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    if (base == NULL)
	      *p = NULL;
	    else
	      *p = base;
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Fix an object that's actually just a Lisp_Vector, but where P points
   to the data contents rather than the header.  */
static mps_res_t
fix_wrapped_vec (mps_ss_t ss, mps_addr_t *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_addr_t client = *p;
    if (client == NULL)
      return MPS_RES_OK;
    if (is_aligned (client))
      {
	client = (char *)client - sizeof (struct Lisp_Vector);
	mps_addr_t base = client;
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    if (base == NULL)
	      *p = NULL;
	    else
	      *p = (char *)base + sizeof (struct Lisp_Vector);
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* Fix an object of type struct Lisp_String_Data specified by a pointer
   P to its data contents.  */
static mps_res_t
fix_wrapped_bytes (mps_ss_t ss, mps_addr_t *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_addr_t client = *p;
    if (client == NULL)
      return MPS_RES_OK;
    if (is_aligned (client))
      {
	client = (char *)client - sizeof (struct Lisp_String_Data);
	mps_addr_t base = client;
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, &base);
	    if (res != MPS_RES_OK)
	      return res;
	    if (base == NULL)
	      *p = NULL;
	    else
	      *p = (char *)base + sizeof (struct Lisp_String_Data);
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_base (mps_ss_t ss, mps_addr_t *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_addr_t base = *p;
    if (base == NULL)
      return MPS_RES_OK;
    if (is_aligned (base))
      {
	if (MPS_FIX1 (ss, base))
	  {
	    mps_res_t res = MPS_FIX2 (ss, p);
	    if (res != MPS_RES_OK)
	      return res;
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

#define IGC_FIX12_HEADER(ss, p)						\
  do									\
    {									\
      mps_res_t res;							\
      igc_assert ((mps_addr_t) (*p) == (mps_addr_t) & (*p)->gc_header);	\
      MPS_FIX_CALL (ss, res = fix_raw (ss, (mps_addr_t *) (p)));	\
      if (res != MPS_RES_OK)						\
	return res;							\
    }									\
  while (0)

#define IGC_FIX12_PVEC(ss, p)						\
  do									\
    {									\
      mps_res_t res;							\
      igc_assert ((mps_addr_t) (*p) == (mps_addr_t) & (*p)->header.gc_header); \
      MPS_FIX_CALL (ss, res = fix_raw (ss, (mps_addr_t *) (p)));	\
      if (res != MPS_RES_OK)						\
	return res;							\
    }									\
  while (0)

#define IGC_FIX12_RAW(ss, p)						\
  do									\
    {									\
      mps_res_t res;							\
      MPS_FIX_CALL (ss, res = fix_raw (ss, (mps_addr_t *) (p)));	\
      if (res != MPS_RES_OK)						\
	return res;							\
    }									\
  while (0)

#define IGC_FIX12_WRAPPED_VEC(ss, p)					\
  do									\
    {									\
      mps_res_t res;							\
      MPS_FIX_CALL (ss, res = fix_wrapped_vec (ss, (mps_addr_t *) (p))); \
      if (res != MPS_RES_OK)						\
	return res;							\
    }									\
  while (0)

#define IGC_FIX12_WRAPPED_BYTES(ss, p)					\
  do									\
    {									\
      mps_res_t res;							\
      MPS_FIX_CALL (ss, res = fix_wrapped_bytes (ss, (mps_addr_t *) (p))); \
      if (res != MPS_RES_OK)						\
	return res;							\
    }									\
  while (0)

#define IGC_FIX12_BASE(ss, p)						\
  do									\
    {									\
      mps_res_t res;							\
      MPS_FIX_CALL (ss, res = fix_base (ss, (mps_addr_t *) (p)));	\
      if (res != MPS_RES_OK)						\
	return res;							\
    }									\
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
    IGC_FIX12_HEADER (ss, &sym->u.s.next);
#endif
    switch (sym->u.s.redirect)
      {
      case SYMBOL_PLAINVAL:
	IGC_FIX12_OBJ (ss, &sym->u.s.val.value);
	break;

      case SYMBOL_VARALIAS:
	IGC_FIX12_HEADER (ss, &sym->u.s.val.alias);
	break;

      case SYMBOL_LOCALIZED:
	IGC_FIX12_HEADER (ss, &sym->u.s.val.blv);
	break;

      case SYMBOL_FORWARDED:
	break;
      }
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
  MPS_SCAN_BEGIN (ss)
  {
    for (struct read_stack_entry *e = start; (void *) e < end; ++e)
      {
	if (e->type == RE_free)
	    break;
	switch (e->type)
	  {
	  case RE_free:
	    emacs_abort ();

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
    /* MPS docs say that root scanning functions have exclusive access
       to what is being scanned, the same way format scanning functions
       do.  That does not mean one can rely on the thread's specpdl_ptr
       here because it may be updated after this function runs.  */
    struct igc_thread_list *t = closure;
    igc_assert (start == (void *) t->d.ts->m_specpdl);
    igc_assert (end == (void *) t->d.ts->m_specpdl_end);

    for (union specbinding *pdl = start;
	 (void *) pdl < end && pdl->kind != SPECPDL_FREE; ++pdl)
      {
	switch (pdl->kind)
	  {
	  case SPECPDL_FREE:
	    emacs_abort ();

	  case SPECPDL_UNWIND:
	    IGC_FIX12_OBJ (ss, &pdl->unwind.arg);
	    break;

	    /* This is used by SAFE_ALLOCA/malloc.  */
	  case SPECPDL_UNWIND_ARRAY:
	    IGC_FIX12_WRAPPED_VEC (ss, &pdl->unwind_array.array);
	    break;

	  case SPECPDL_UNWIND_EXCURSION:
	    IGC_FIX12_OBJ (ss, &pdl->unwind_excursion.marker);
	    IGC_FIX12_OBJ (ss, &pdl->unwind_excursion.window);
	    break;

	    /* The bt.args member either points to something on a
	       thread's control stack, or to something in the bytecode
	       stack.  Both should already be ambiguous roots.  */
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
	    IGC_FIX12_OBJ (ss, &pdl->let.where.buf);
	    FALLTHROUGH;
	  case SPECPDL_LET:
	    IGC_FIX12_OBJ (ss, &pdl->let.symbol);
	    IGC_FIX12_OBJ (ss, &pdl->let.old_value);
	    break;

	  case SPECPDL_UNWIND_PTR:
	    /* This can contain a mark function of its own, which is of
	       no use to us.  Only user is sort.c.  */
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

/* Scan the area of memory [START, END) ambiguously.  In general,
   references may be either tagged words or pointers.  This is used for
   blocks allocated with malloc and thread stacks.  */

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
	   genuine reference).  The MPS handles an ambiguous
	   reference by pinning the block pointed to so that
	   it cannot move.  */
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
    /* FIXME/igc: AFAIU the current top frame starts at
       bc->fp->next_stack and has a maximum length that is given by the
       bytecode being executed (COMPILED_STACK_DEPTH).  So, we need to
       scan upto bc->fo->next_stack + that max depth to be safe.  Since
       I don't have that number ATM, I'm using an arbitrary estimate for
       now.

       This must be changed to something better.  Note that Mattias said
       the bc stack marking will be changed in the future.  */
    const size_t HORRIBLE_ESTIMATE = 1024;
    char *scan_end = bc_next_frame (bc->fp);
    scan_end += HORRIBLE_ESTIMATE;
    end = min (end, (void *) scan_end);
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

static mps_res_t
scan_tty_list (mps_ss_t ss, void *start, void *end, void *closure)
{
  igc_assert (start == &tty_list);
  igc_assert (end == (&tty_list) + 1);
  MPS_SCAN_BEGIN (ss)
  {
    for (struct tty_display_info *tty = tty_list; tty; tty = tty->next)
      {
	IGC_FIX12_PVEC (ss, &tty->terminal);
	IGC_FIX12_OBJ (ss, &tty->top_frame);
	IGC_FIX12_PVEC (ss, &tty->previous_frame);
      }
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
  set_header (h, IGC_OBJ_PAD, nbytes, 0);
}

static void
dflt_fwd (mps_addr_t old_base_addr, mps_addr_t new_base_addr)
{
  struct igc_header *h = old_base_addr;
  igc_assert (obj_size (h) >= sizeof (struct igc_fwd));
  igc_assert (header_type (h) != IGC_OBJ_PAD);
  struct igc_fwd *f = old_base_addr;
  set_header (&f->header, IGC_OBJ_FWD, to_bytes (igc_header_nwords (h)), 0);
  f->new_base_addr = new_base_addr;
}

static mps_addr_t
is_dflt_fwd (mps_addr_t base_addr)
{
  struct igc_fwd *f = base_addr;
  if (header_type (&f->header) == IGC_OBJ_FWD)
    return f->new_base_addr;
  return NULL;
}

static mps_addr_t
dflt_skip (mps_addr_t base_addr)
{
  struct igc_header *h = base_addr;
  mps_addr_t next = (char *) base_addr + obj_size (h);
  igc_assert (next > base_addr);
  return next;
}

static mps_res_t
fix_string (mps_ss_t ss, struct Lisp_String *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    struct Lisp_String_Data *ptr =
      (void *) (s->u.s.data - sizeof (*ptr));
    IGC_FIX12_HEADER (ss, &ptr);
    s->u.s.data = ptr->data;
    IGC_FIX12_HEADER (ss, &s->u.s.intervals);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_interval (mps_ss_t ss, struct interval *iv)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_HEADER (ss, &iv->left);
    IGC_FIX12_HEADER (ss, &iv->right);
    if (iv->up_obj)
      IGC_FIX12_OBJ (ss, &iv->up.obj);
    else if (iv->up.interval)
      IGC_FIX12_HEADER (ss, &iv->up.interval);
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
      IGC_FIX12_HEADER (ss, &t->root);
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
      IGC_FIX12_HEADER (ss, &n->parent);
    if (n->left)
      IGC_FIX12_HEADER (ss, &n->left);
    if (n->right)
      IGC_FIX12_HEADER (ss, &n->right);
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
    IGC_FIX12_HEADER (ss, &i->next);
    IGC_FIX12_HEADER (ss, &i->prev);
#endif
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_image_cache (mps_ss_t ss, struct image_cache *c)
{
  MPS_SCAN_BEGIN (ss)
  {
#ifdef HAVE_WINDOW_SYSTEM
    if (c->images)
      for (ptrdiff_t i = 0; i < c->used; ++i)
	IGC_FIX12_RAW (ss, &c->images[i]);

    if (c->buckets)
      for (ptrdiff_t i = 0; i < IMAGE_CACHE_BUCKETS_SIZE; ++i)
	if (c->buckets[i])
	  IGC_FIX12_RAW (ss, &c->buckets[i]);
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
    IGC_FIX12_PVEC (ss, &f->font);
    IGC_FIX12_HEADER (ss, &f->next);
    IGC_FIX12_HEADER (ss, &f->prev);
    IGC_FIX12_HEADER (ss, &f->ascii_face);
#if defined HAVE_XFT || defined HAVE_FREETYPE
    IGC_FIX12_HEADER (ss, (struct vectorlike_header **)&f->extra);
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
    IGC_FIX12_PVEC (ss, &c->f);

    if (c->faces_by_id)
      for (ptrdiff_t i = 0; i < c->used; ++i)
	IGC_FIX12_RAW (ss, &c->faces_by_id[i]);

    if (c->buckets)
      for (ptrdiff_t i = 0; i < FACE_CACHE_BUCKETS_SIZE; ++i)
	if (c->buckets[i])
	  IGC_FIX12_RAW (ss, &c->buckets[i]);
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

static mps_res_t
fix_handler (mps_ss_t ss, struct handler *h)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_OBJ (ss, &h->tag_or_ch);
    IGC_FIX12_OBJ (ss, &h->val);
    IGC_FIX12_HEADER (ss, &h->next);
    IGC_FIX12_HEADER (ss, &h->nextfree);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_charset_table (mps_ss_t ss, struct charset *table, size_t nbytes)
{
  igc_assert (table == charset_table);
  igc_assert (nbytes == (charset_table_size * sizeof (struct charset)));
  MPS_SCAN_BEGIN (ss)
  {
    for (size_t i = 0, len = nbytes / sizeof (struct charset); i < len; i++)
      IGC_FIX12_OBJ (ss, &table[i].attributes);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t fix_vector (mps_ss_t ss, struct Lisp_Vector *v);
static mps_res_t fix_marker_vector (mps_ss_t ss, struct Lisp_Vector *v);
static mps_res_t fix_weak_hash_table_strong_part (mps_ss_t ss, struct Lisp_Weak_Hash_Table_Strong_Part *t);
static mps_res_t fix_weak_hash_table_weak_part (mps_ss_t ss, struct Lisp_Weak_Hash_Table_Weak_Part *w);

static mps_res_t
dflt_scan_obj (mps_ss_t ss, mps_addr_t base_start, mps_addr_t base_limit,
	       void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    mps_addr_t base = base_start;
    mps_addr_t client = base;
    struct igc_header *header = base;

    if (closure)
      {
	struct igc_stats *st = closure;
	mps_word_t obj_type = igc_header_type (header);
	igc_assert (obj_type < IGC_OBJ_NUM_TYPES);
	size_t size = obj_size (header);
	st->obj[obj_type].nbytes += size;
	st->obj[obj_type].nobjs += 1;
	if (size > st->obj[obj_type].largest)
	  st->obj[obj_type].largest = size;
	if (obj_type == IGC_OBJ_VECTOR)
	  {
	    struct Lisp_Vector* v = (struct Lisp_Vector*) client;
	    enum pvec_type pvec_type = pseudo_vector_type (v);
	    igc_assert (0 <= pvec_type && pvec_type <= PVEC_TAG_MAX);
	    st->pvec[pvec_type].nbytes += size;
	    st->pvec[pvec_type].nobjs += 1;
	    if (size > st->pvec[pvec_type].largest)
	      st->pvec[pvec_type].largest = size;
	  }
      }

    if (header_tag (header) == IGC_TAG_EXTHDR)
      {
	struct igc_exthdr *exthdr = header_exthdr (header);
	IGC_FIX12_OBJ (ss, &exthdr->extra_dependency);
      }

    switch (igc_header_type (header))
      {
      case IGC_OBJ_INVALID:
      case IGC_OBJ_BUILTIN_SYMBOL:
      case IGC_OBJ_BUILTIN_THREAD:
      case IGC_OBJ_BUILTIN_SUBR:
	emacs_abort ();

      case IGC_OBJ_PAD:
      case IGC_OBJ_FWD:
	continue;

      case IGC_OBJ_HANDLER:
	IGC_FIX_CALL_FN (ss, struct handler, client, fix_handler);
	break;

      case IGC_OBJ_CONS:
	IGC_FIX_CALL_FN (ss, struct Lisp_Cons, client, fix_cons);
	break;

      case IGC_OBJ_STRING_DATA:
      case IGC_OBJ_FLOAT:
      case IGC_OBJ_BYTES:
      case IGC_OBJ_DUMPED_CODE_SPACE_MASKS:
      case IGC_OBJ_DUMPED_BUFFER_TEXT:
      case IGC_OBJ_DUMPED_BIGNUM_DATA:
      case IGC_OBJ_DUMPED_BYTES:
	/* Can occur in the dump.  */
	break;

      case IGC_OBJ_NUM_TYPES:
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

      case IGC_OBJ_MARKER_VECTOR:
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, client, fix_marker_vector);
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

      case IGC_OBJ_IMAGE_CACHE:
	IGC_FIX_CALL_FN (ss, struct image_cache, client, fix_image_cache);
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

      case IGC_OBJ_DUMPED_CHARSET_TABLE:
	IGC_FIX_CALL (ss, fix_charset_table (ss, (struct charset *)client,
					     obj_size (header)));
	break;

      case IGC_OBJ_WEAK_HASH_TABLE_STRONG_PART:
	IGC_FIX_CALL_FN (ss, struct Lisp_Weak_Hash_Table_Strong_Part, client,
			 fix_weak_hash_table_strong_part);
	break;
      case IGC_OBJ_WEAK_HASH_TABLE_WEAK_PART:
	IGC_FIX_CALL_FN (ss, struct Lisp_Weak_Hash_Table_Weak_Part, client,
			 fix_weak_hash_table_weak_part);
	break;
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
dflt_scanx (mps_ss_t ss, mps_addr_t base_start, mps_addr_t base_limit,
	    void *closure)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (mps_addr_t base = base_start; base < base_limit;
	 base = dflt_skip (base))
      IGC_FIX_CALL (ss, dflt_scan_obj (ss, base, base_limit, closure));
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

static mps_res_t
fix_vectorlike (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    size_t size = vector_size (v);
    IGC_FIX12_NOBJS (ss, v->contents, size);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_marker_vector (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (size_t i = 0, n = vector_size (v); i < n; ++i)
      {
	Lisp_Object old = v->contents[i];
	IGC_FIX12_OBJ (ss, &v->contents[i]);
	/* FIXME/igc: this is right for marker vectors only.  */
	if (NILP (v->contents[i]) && !NILP (old))
	  {
	    v->contents[i] = v->contents[0];
	    v->contents[0] = make_fixnum (i);
	  }
      }
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
    IGC_FIX12_HEADER (ss, &b->own_text.intervals);
    IGC_FIX12_OBJ (ss, &b->own_text.markers);
    IGC_FIX12_HEADER (ss, &b->overlays);
    IGC_FIX12_OBJ (ss, &b->undo_list_);

    IGC_FIX12_PVEC (ss, &b->base_buffer);
    if (b->base_buffer)
      b->text = &b->base_buffer->own_text;
    else
      b->text = &b->own_text;
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_glyph_pool (mps_ss_t ss, struct glyph_pool *pool)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (ptrdiff_t i = 0; i < pool->nglyphs; ++i)
      {
	IGC_FIX12_OBJ (ss, &pool->glyphs[i].object);
      }
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
		IGC_FIX12_OBJ (ss, &glyph->object);
	    }
	}
    IGC_FIX12_PVEC (ss, &matrix->buffer);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_frame (mps_ss_t ss, struct frame *f)
{
  MPS_SCAN_BEGIN (ss)
  {
    // FIXME/igc: Check these
    // output_data;
    // terminal
    // glyph_pool
    // glyph matrices
    // struct font_driver_list *font_driver_list;
    // struct text_conversion_state conversion;
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, f, fix_vectorlike);
    IGC_FIX12_HEADER (ss, &f->face_cache);
    if (f->terminal)
      IGC_FIX12_PVEC (ss, &f->terminal);

    if (!FRAME_INITIAL_P (f) && FRAME_LIVE_P (f))
      {
	/* This is typically stored in the display_info, e.g.
	   ns_display_info.  Check for being NULL anyway.  */
	Mouse_HLInfo *hlinfo = MOUSE_HL_INFO (f);
	if (hlinfo)
	  {
	    IGC_FIX12_OBJ (ss, &hlinfo->mouse_face_window);
	    IGC_FIX12_OBJ (ss, &hlinfo->mouse_face_overlay);
	    IGC_FIX12_RAW (ss, &hlinfo->mouse_face_mouse_frame);
	  }
      }

#ifdef HAVE_WINDOW_SYSTEM
    if (f->image_cache)
      IGC_FIX12_RAW (ss, &f->image_cache);
    if (FRAME_WINDOW_P (f) && FRAME_OUTPUT_DATA (f))
      {
	struct font **font_ptr = &FRAME_FONT (f);
	if (*font_ptr)
	  IGC_FIX12_PVEC (ss, font_ptr);
	Lisp_Object *nle = &FRAME_DISPLAY_INFO (f)->name_list_element;
	IGC_FIX12_OBJ (ss, nle);
      }
#endif /* HAVE_WINDOW_SYSTEM */

#ifdef HAVE_TEXT_CONVERSION
    IGC_FIX12_OBJ (ss, &f->conversion.compose_region_start);
    IGC_FIX12_OBJ (ss, &f->conversion.compose_region_end);
    IGC_FIX12_OBJ (ss, &f->conversion.compose_region_overlay);
    IGC_FIX12_OBJ (ss, &f->conversion.field);
#endif

    if (f->current_pool)
      IGC_FIX_CALL (ss, fix_glyph_pool (ss, f->current_pool));
    if (f->desired_pool)
      IGC_FIX_CALL (ss, fix_glyph_pool (ss, f->desired_pool));
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
    if (w->current_matrix && !w->current_matrix->pool)
      IGC_FIX_CALL (ss, fix_glyph_matrix (ss, w->current_matrix));
    if (w->desired_matrix && !w->desired_matrix->pool)
      IGC_FIX_CALL (ss, fix_glyph_matrix (ss, w->desired_matrix));
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
    // FIXME/igc: weak hash tables
    IGC_FIX12_WRAPPED_VEC (ss, &h->key);
    IGC_FIX12_WRAPPED_VEC (ss, &h->value);
    IGC_FIX12_WRAPPED_BYTES (ss, &h->hash);
    IGC_FIX12_WRAPPED_BYTES (ss, &h->next);
    IGC_FIX12_WRAPPED_BYTES (ss, &h->index);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_weak_hash_table (mps_ss_t ss, struct Lisp_Weak_Hash_Table *h)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_HEADER (ss, &h->strong);
    IGC_FIX12_HEADER (ss, &h->weak);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_weak_hash_table_strong_part (mps_ss_t ss, struct Lisp_Weak_Hash_Table_Strong_Part *t)
{
  MPS_SCAN_BEGIN (ss)
  {
    if (t->weak && t->weak->strong == t)
      {
	igc_assert (FIXNUMP (t->table_size));
	ssize_t limit;
	switch (t->weakness)
	  {
	  case Weak_Key:
	    limit = 3 * XFIXNUM (t->table_size);
	    break;
	  case Weak_Value:
	    limit = 3 * XFIXNUM (t->table_size);
	    break;
	  case Weak_Key_And_Value:
	  case Weak_Key_Or_Value:
	    limit = 2 * XFIXNUM (t->table_size);
	    break;
	  default:
	    emacs_abort ();
	  }
	for (ssize_t i = 2 * XFIXNUM (t->table_size); i < limit; i++)
	  {
	    intptr_t off = 0;
#ifdef WORDS_BIGENDIAN
	    off = sizeof (t->entries[i].intptr) - sizeof (mps_word_t);
#endif
	    IGC_FIX12_BASE (ss, ((char *)&t->entries[i].intptr) + off);
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_weak_hash_table_weak_part (mps_ss_t ss, struct Lisp_Weak_Hash_Table_Weak_Part *w)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX12_HEADER (ss, &w->strong);
    struct Lisp_Weak_Hash_Table_Strong_Part *t = w->strong;
    if (t && t->weak == w)
      {
	igc_assert (FIXNUMP (t->table_size));
	ssize_t limit;
	switch (t->weakness)
	  {
	  case Weak_Key:
	    limit = XFIXNUM (t->table_size);
	    break;
	  case Weak_Value:
	    limit = XFIXNUM (t->table_size);
	    break;
	  case Weak_Key_And_Value:
	  case Weak_Key_Or_Value:
	    limit = 2 * XFIXNUM (t->table_size);
	    break;
	  default:
	    emacs_abort ();
	  }

	for (ssize_t i = 0; i < limit; i++)
	  {
	    if (w->entries[i].intptr & 1)
	      eassert (((mps_word_t)w->entries[i].intptr ^ w->entries[i].intptr) == 0);
	    bool was_nil = (w->entries[i].intptr) == 0;
	    intptr_t off = 0;
#ifdef WORDS_BIGENDIAN
	    off = sizeof (w->entries[i].intptr) - sizeof (mps_word_t);
#endif
	    IGC_FIX12_BASE (ss, ((char *)&w->entries[i].intptr) + off);
	    bool is_now_nil = w->entries[i].intptr == 0;

	    if (is_now_nil && !was_nil)
	      {
		struct Lisp_Weak_Hash_Table pseudo_h =
		  {
		    .strong = t,
		    .weak = w,
		  };
		weak_hash_splat_from_table
		  (&pseudo_h, ((t->weakness == Weak_Key_And_Value ||
				t->weakness == Weak_Key_Or_Value) ?
			       (i % XFIXNUM (t->table_size)) : i));
	      }
	  }
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_char_table (mps_ss_t ss, struct Lisp_Vector *v)
{
  MPS_SCAN_BEGIN (ss)
  {
    for (size_t i = vector_start (v), n = vector_size (v); i < n; ++i)
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
    IGC_FIX12_PVEC (ss, &o->buffer);
    IGC_FIX12_OBJ (ss, &o->plist);
    IGC_FIX12_HEADER (ss, &o->interval);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

static mps_res_t
fix_subr (mps_ss_t ss, struct Lisp_Subr *s)
{
  MPS_SCAN_BEGIN (ss)
  {
#ifdef HAVE_NATIVE_COMP
    if (!NILP (s->native_comp_u))
      {
	IGC_FIX12_OBJ (ss, &s->native_comp_u);
	IGC_FIX12_OBJ (ss, &s->command_modes);
	IGC_FIX12_OBJ (ss, &s->intspec.native);
	IGC_FIX12_OBJ (ss, &s->lambda_list);
	IGC_FIX12_OBJ (ss, &s->type);
      }
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
fix_thread (mps_ss_t ss, struct thread_state *s)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, s, fix_vectorlike);
    IGC_FIX12_PVEC (ss, &s->m_current_buffer);
    IGC_FIX12_PVEC (ss, &s->next_thread);
    IGC_FIX12_HEADER (ss, &s->m_handlerlist);
    IGC_FIX12_HEADER (ss, &s->m_handlerlist_sentinel);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

/* This is here because main_thread is, for some reason, a variable in
   the data segment, and not like other threads.  */

static mps_res_t
scan_main_thread (mps_ss_t ss, void *start, void *end, void *closure)
{
  igc_assert (start == (void *) &main_thread.s);
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
    IGC_FIX12_OBJ (ss, &m->name);
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
    IGC_FIX12_PVEC (ss, &t->next_terminal);
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
    if (m->buffer)
      IGC_FIX12_PVEC (ss, &m->buffer);
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
    IGC_FIX12_PVEC (ss, &f->next);
    IGC_FIX12_PVEC (ss, &f->prev);
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
    /* FIXME/igc: Cannot scan things within the shared object because we
       don't have exclusive (synchronized) access to them.  Instead of
       storing Lisp_Object references in vectors in the dylib data
       segment it would be much nicer to store them in MPS and give the
       dylib a pointer to them.  */
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
# if defined (NS_IMPL_COCOA)
    IGC_FIX12_PVEC (ss, &w->xv);
# elif defined USE_GTK
    IGC_FIX12_PVEC (ss, &w->embedder);
    IGC_FIX12_PVEC (ss, &w->embedder_view);
# endif
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
# ifdef USE_GTK
    IGC_FIX12_PVEC (ss, &v->frame);
# endif
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
    IGC_FIX12_WRAPPED_VEC (ss, &o->buckets);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}
#endif

#ifdef HAVE_TREE_SITTER
static mps_res_t
fix_ts_parser (mps_ss_t ss, struct Lisp_TS_Parser *p)
{
  MPS_SCAN_BEGIN (ss)
  {
    IGC_FIX_CALL_FN (ss, struct Lisp_Vector, p, fix_vectorlike);
    IGC_FIX12_RAW (ss, &p->input.payload);
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}
#endif

/* Note that there is a small window after committing a vectorlike
   allocation where the object is zeroed, and so the vector header is
   also zero.  This doesn't have an adverse effect.  */

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

      case PVEC_WEAK_HASH_TABLE:
	IGC_FIX_CALL_FN (ss, struct Lisp_Weak_Hash_Table, v, fix_weak_hash_table);
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
	break;

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
#else
      case PVEC_XWIDGET:
      case PVEC_XWIDGET_VIEW:
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
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

      case PVEC_TS_PARSER:
#ifdef HAVE_TREE_SITTER
	IGC_FIX_CALL_FN (ss, struct Lisp_TS_Parser, v, fix_ts_parser);
#else
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
#endif
	break;

      case PVEC_FONT:
      case PVEC_NORMAL_VECTOR:
      case PVEC_SYMBOL_WITH_POS:
      case PVEC_PROCESS:
      case PVEC_WINDOW_CONFIGURATION:
      case PVEC_MODULE_FUNCTION:
      case PVEC_CONDVAR:
      case PVEC_TS_COMPILED_QUERY:
      case PVEC_TS_NODE:
      case PVEC_SQLITE:
      case PVEC_CLOSURE:
      case PVEC_RECORD:
      case PVEC_OTHER:
#ifdef IN_MY_FORK
      case PVEC_PACKAGE:
#endif
	IGC_FIX_CALL_FN (ss, struct Lisp_Vector, v, fix_vectorlike);
	break;
      }
  }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

igc_scan_result_t
igc_fix12_obj (struct igc_ss *ssp, Lisp_Object *addr)
{
  mps_ss_t ss = (mps_ss_t)ssp;
  MPS_SCAN_BEGIN (ss) { IGC_FIX12_OBJ (ss, addr); }
  MPS_SCAN_END (ss);
  return MPS_RES_OK;
}

#pragma GCC diagnostic pop

static igc_root_list *
root_create (struct igc *gc, void *start, void *end, mps_rank_t rank,
	     mps_area_scan_t scan, void *closure, bool ambig,
	     const char *label)
{
  mps_root_t root;
  mps_res_t res
    = mps_root_create_area (&root, gc->arena, rank, 0, start, end, scan,
			    closure);
  IGC_CHECK_RES (res);
  return register_root (gc, root, start, end, ambig, label);
}

static igc_root_list *
root_create_ambig (struct igc *gc, void *start, void *end,
		   const char *label)
{
  return root_create (gc, start, end, mps_rank_ambig (), scan_ambig, NULL,
		      true, label);
}

static igc_root_list *
root_create_exact (struct igc *gc, void *start, void *end,
		   mps_area_scan_t scan, const char *label)
{
  return root_create (gc, start, end, mps_rank_exact (), scan, NULL, false,
		      label);
}

static void
root_create_staticvec (struct igc *gc)
{
  root_create_exact (gc, staticvec, staticvec + ARRAYELTS (staticvec),
		     scan_staticvec, "staticvec");
}

static void
root_create_lispsym (struct igc *gc)
{
  root_create_exact (gc, lispsym, lispsym + ARRAYELTS (lispsym),
		     scan_lispsym, "lispsym");
}

static void
root_create_buffer (struct igc *gc, struct buffer *b)
{
  void *start = &b->name_, *end = &b->own_text;
  root_create_ambig (gc, start, end, "buffer");
}

static void
root_create_terminal_list (struct igc *gc)
{
  void *start = &terminal_list;
  void *end = (char *) start + sizeof (terminal_list);
  root_create_ambig (gc, start, end, "terminal-list");
}

static void
root_create_tty_list (struct igc *gc)
{
  root_create_exact (gc, &tty_list, (&tty_list) + 1,
		     scan_tty_list, "tty-list");
}

static void
root_create_main_thread (struct igc *gc)
{
  void *start = &main_thread.s;
  void *end = (char *) &main_thread.s + sizeof (main_thread.s);
  root_create_exact (gc, start, end, scan_main_thread, "main-thread");
}

void
igc_root_create_ambig (void *start, void *end, const char* label)
{
  root_create_ambig (global_igc, start, end, label);
}

void
igc_root_create_exact (Lisp_Object *start, Lisp_Object *end)
{
  root_create_exact (global_igc, start, end, scan_exact, "exact");
}

static void
root_create_exact_ptr (struct igc *gc, void *var_addr)
{
  char *start = var_addr;
  char *end = start + sizeof (void *);
  root_create_exact (gc, start, end, scan_ptr_exact, "exact-ptr");
}

void
igc_root_create_exact_ptr (void *var_addr)
{
  root_create_exact_ptr (global_igc, var_addr);
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
    = register_root (gc, root, ts->m_specpdl, ts->m_specpdl_end, false,
		     "specpdl");
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
  t->d.bc_root = register_root (gc, root, bc->stack, bc->stack_end, true,
				"bc-stack");
}

static void
root_create_charset_table (struct igc *gc)
{
  root_create_ambig (gc, charset_table_init,
		     charset_table_init + ARRAYELTS (charset_table_init),
		     "charset-table");
}

#ifndef IN_MY_FORK
static void
root_create_pure (struct igc *gc)
{
  void *start = &pure[0];
  void *end = &pure[pure_dim];
  root_create (gc, start, end, mps_rank_ambig (), scan_pure, NULL, true,
	       "pure");
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
  t->d.stack_root = register_root (gc, root, cold, NULL, true, "control stack");
}

void
igc_on_grow_specpdl (void)
{
  /* Note that no two roots may overlap, so we have to temporarily
     stop the collector while replacing one root with another (xpalloc
     may realloc).  Alternatives: (1) don't realloc, (2) alloc specpdl
     from MPS pool that is scanned.  */
  struct igc_thread_list *t = current_thread->gc_info;
  IGC_WITH_PARKED (t->d.gc)
  {
    destroy_root (&t->d.specpdl_root);
    root_create_specpdl (t);
  }
}

static igc_root_list *
root_create_exact_n (Lisp_Object *start, size_t n)
{
  igc_assert (start != NULL);
  igc_assert (n > 0);
  return root_create_exact (global_igc, start, start + n, scan_exact,
			    "exact-n");
}

void *
igc_root_create_n (Lisp_Object start[], size_t n)
{
  return root_create_exact_n (start, n);
}

static igc_root_list *
root_find (void *start)
{
  for (igc_root_list *r = global_igc->roots; r; r = r->next)
    if (r->d.start == start)
      return r;
  return NULL;
}

void
igc_destroy_root_with_start (void *start)
{
  if (start)
    {
      struct igc_root_list *r = root_find (start);
      igc_assert (r != NULL);
      destroy_root (&r);
    }
}

static void
maybe_destroy_root (struct igc_root_list **root)
{
  if (*root)
    destroy_root (root);
}

void
igc_root_destroy_comp_unit (struct Lisp_Native_Comp_Unit *u)
{
  maybe_destroy_root (&u->data_relocs_root);
  maybe_destroy_root (&u->data_imp_relocs_root);
  maybe_destroy_root (&u->data_eph_relocs_root);
  maybe_destroy_root (&u->comp_unit_root);
}

void
igc_root_destroy_comp_unit_eph (struct Lisp_Native_Comp_Unit *u)
{
  maybe_destroy_root (&u->data_eph_relocs_root);
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

static mps_res_t
create_weak_hash_ap (mps_ap_t *ap, struct igc_thread *t, bool weak)
{
  struct igc *gc = t->gc;
  mps_res_t res;
  mps_pool_t pool = gc->weak_hash_pool;
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
  res = mps_ap_create_k (&t->immovable_ap, gc->immovable_pool, mps_args_none);
  IGC_CHECK_RES (res);
  res = create_weak_ap (&t->weak_strong_ap, t, false);
  res = create_weak_hash_ap (&t->weak_hash_strong_ap, t, false);
  IGC_CHECK_RES (res);
  res = create_weak_ap (&t->weak_weak_ap, t, true);
  res = create_weak_hash_ap (&t->weak_hash_weak_ap, t, true);
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
  igc_assert (current_thread == &main_thread.s);
  igc_assert (current_thread->gc_info == NULL);
  igc_assert (current_thread->m_stack_bottom == stack_bottom);
  struct igc_thread_list *t = thread_add (current_thread);
  current_thread->gc_info = t;
  igc_assert (t->d.ts == current_thread);
}

void
igc_thread_remove (void **pinfo)
{
  struct igc_thread_list *t = *pinfo;
  *pinfo = NULL;
  destroy_root (&t->d.stack_root);
  destroy_root (&t->d.specpdl_root);
  destroy_root (&t->d.bc_root);
  mps_ap_destroy (t->d.dflt_ap);
  mps_ap_destroy (t->d.leaf_ap);
  mps_ap_destroy (t->d.weak_strong_ap);
  mps_ap_destroy (t->d.weak_weak_ap);
  mps_ap_destroy (t->d.weak_hash_strong_ap);
  mps_ap_destroy (t->d.weak_hash_weak_ap);
  mps_ap_destroy (t->d.immovable_ap);
  mps_thread_dereg (deregister_thread (t));
}

static void
_release_arena (void)
{
  arena_release (global_igc);
}

specpdl_ref
igc_park_arena (void)
{
  specpdl_ref count = SPECPDL_INDEX ();
  record_unwind_protect_void (_release_arena);
  arena_park (global_igc);
  return count;
}

void
igc_grow_rdstack (struct read_stack *rs)
{
  struct igc *gc = global_igc;
  IGC_WITH_PARKED (gc)
  {
    igc_destroy_root_with_start (rs->stack);
    ptrdiff_t old_nitems = rs->size;
    rs->stack = xpalloc (rs->stack, &rs->size, 1, -1, sizeof *rs->stack);
    for (ptrdiff_t i = old_nitems; i < rs->size; ++i)
      rs->stack[i].type = RE_free;
    root_create_exact (gc, rs->stack, rs->stack + rs->size, scan_rdstack,
		       "rdstack");
  }
}

Lisp_Object *
igc_xalloc_lisp_objs_exact (size_t n)
{
  size_t size = n * sizeof (Lisp_Object);
  void *p = xzalloc (size);
  root_create_exact (global_igc, p, (char *) p + size, scan_exact,
		     "xalloc-exact");
  return p;
}

void *
igc_xalloc_raw_exact (size_t n)
{
  size_t size = n * sizeof (void *);
  void *p = xzalloc (size);
  root_create_exact (global_igc, p, (char *) p + size, scan_ptr_exact,
		     "xalloc-raw-exact");
  return p;
}

void *
igc_xzalloc_ambig (size_t size)
{
  /* Not sure if xzalloc can ever return NULL here, depending on all the
     config options involved.  Also not sure when it returns non-null for
     size 0.  It does for me on macOS.  */
  while (size % IGC_ALIGN_DFLT)
    size++;
  void *p = xzalloc (size);
  if (p == NULL)
    return NULL;

  /* Can't make a root that has zero length.  Want one to be able to
     detect calling igc_free on something not having a root.  */
  void *end = (char *) p + size;
  if (end == p)
    end = (char *) p + IGC_ALIGN_DFLT;
  root_create_ambig (global_igc, p, end, "xzalloc-ambig");
  return p;
}

void *
igc_xnmalloc_ambig (ptrdiff_t nitems, ptrdiff_t item_size)
{
  return igc_xzalloc_ambig (nitems * item_size);
}

void *
igc_realloc_ambig (void *block, size_t size)
{
  struct igc *gc = global_igc;
  void *p;
  IGC_WITH_PARKED (gc)
  {
    igc_destroy_root_with_start (block);
    /* Can't make a root that has zero length.  Want one to be able to
       detect calling igc_free on something not having a root.  */
    size_t new_size = (size == 0 ? IGC_ALIGN_DFLT : size);
    p = xrealloc (block, new_size);
    void *end = (char *)p + new_size;
    root_create_ambig (global_igc, p, end, "realloc-ambig");
  }
  return p;
}


void
igc_xfree (void *p)
{
  /* Check for pdumper_object_p here because xfree does the same.  Means
     that freeing something that is actually in the dump is not an
     error.  Make the same true if the dump is loaded into MPS memory.  */
  if (p == NULL || pdumper_object_p (p))
    return;
  igc_destroy_root_with_start (p);
  xfree (p);
}

void *
igc_xpalloc_ambig (void *pa, ptrdiff_t *nitems, ptrdiff_t nitems_incr_min,
		   ptrdiff_t nitems_max, ptrdiff_t item_size)
{
  IGC_WITH_PARKED (global_igc)
  {
    igc_destroy_root_with_start (pa);
    pa = xpalloc (pa, nitems, nitems_incr_min, nitems_max, item_size);
    char *end = (char *) pa + *nitems * item_size;
    root_create_ambig (global_igc, pa, end, "xpalloc-ambig");
  }
  return pa;
}

void
igc_xpalloc_exact (void **pa_cell, ptrdiff_t *nitems,
		   ptrdiff_t nitems_incr_min, ptrdiff_t nitems_max,
		   ptrdiff_t item_size, igc_scan_area_t scan_area,
		   void *closure)
{
  IGC_WITH_PARKED (global_igc)
  {
    void *pa = *pa_cell;
    igc_destroy_root_with_start (pa);
    pa = xpalloc (pa, nitems, nitems_incr_min, nitems_max, item_size);
    char *end = (char *)pa + *nitems * item_size;
    root_create (global_igc, pa, end, mps_rank_exact (), (mps_area_scan_t) scan_area,
		 closure, false, "xpalloc-exact");
    *pa_cell = pa;
  }
}

void *
igc_xnrealloc_ambig (void *pa, ptrdiff_t nitems, ptrdiff_t item_size)
{
  IGC_WITH_PARKED (global_igc)
  {
    igc_destroy_root_with_start (pa);
    pa = xnrealloc (pa, nitems, item_size);
    char *end = (char *) pa + nitems * item_size;
    root_create_ambig (global_igc, pa, end, "xnrealloc-ambig");
  }
  return pa;
}

static void
finalize_bignum (struct Lisp_Bignum *n)
{
  mpz_clear (n->value);
}

static void
finalize_font (struct font *font)
{
  struct Lisp_Vector *v = (void *) font;
  if (vector_size (v) == FONT_OBJECT_MAX)
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
  u->data_eph_relocs = NULL;
  u->data_imp_relocs = NULL;
  u->data_relocs = NULL;
  u->comp_unit = NULL;
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

/* Turn an existing pseudovector into a PVEC_FREE, keeping its size.  */

static void
splat_pvec (struct Lisp_Vector *v)
{
  v->header.size &= ~PVEC_TYPE_MASK;
  XSETPVECTYPE (v, PVEC_FREE);
}

static void
finalize_vector (mps_addr_t v)
{
  struct Lisp_Vector *vec = v;
  /* Please use exhaustive switches, just to do me a favor :-).  */
  switch (pseudo_vector_type (v))
    {
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
#endif
    case PVEC_HASH_TABLE:
    case PVEC_WEAK_HASH_TABLE:
    case PVEC_SYMBOL_WITH_POS:
    case PVEC_PROCESS:
    case PVEC_RECORD:
    case PVEC_CLOSURE:
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
    case PVEC_MODULE_GLOBAL_REFERENCE:
      igc_assert (!"finalization not implemented");
      break;

    case PVEC_FREE:
      break;
    }

  splat_pvec (vec);
}

static void
finalize (struct igc *gc, mps_addr_t base)
{
  mps_addr_t client = base;
  struct igc_header *h = base;
  if (header_tag (h) == IGC_TAG_EXTHDR)
    {
      struct igc_exthdr *exthdr = header_exthdr (h);
      set_header (h, exthdr->obj_type, to_bytes (exthdr->nwords), exthdr->hash);
      xfree (exthdr);
    }
  switch (igc_header_type (h))
    {
    case IGC_OBJ_VECTOR:
      finalize_vector (client);
      break;

    default:
      break;
    }
}

static void
maybe_process_messages (void)
{
  static int count = 0;
  if (++count > 1000)
    {
      count = 0;
      if (noninteractive)
	{
	  while (process_one_message (global_igc))
	    ;
	}
      else
	igc_process_messages ();
    }
}

static void
maybe_finalize (mps_addr_t client, enum pvec_type tag)
{
  mps_addr_t ref = client;
  struct igc_header *h = ref;
  if (header_tag (h) == IGC_TAG_EXTHDR)
    {
      mps_res_t res = mps_finalize (global_igc->arena, &ref);
      IGC_CHECK_RES (res);
      return;
    }
  switch (tag)
    {
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
      {
	mps_res_t res = mps_finalize (global_igc->arena, &ref);
	IGC_CHECK_RES (res);
	maybe_process_messages ();
      }
      break;

#ifndef IN_MY_FORK
    case PVEC_OBARRAY:
#endif
    case PVEC_HASH_TABLE:
    case PVEC_WEAK_HASH_TABLE:
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
    case PVEC_CLOSURE:
    case PVEC_CHAR_TABLE:
    case PVEC_SUB_CHAR_TABLE:
    case PVEC_RECORD:
#ifdef IN_MY_FORK
    case PVEC_PACKAGE:
#endif
    case PVEC_MODULE_GLOBAL_REFERENCE:
      break;
    }
}

struct igc_clock
{
  mps_clock_t expire;
};

static bool
clock_has_expired (struct igc_clock *clock)
{
  return mps_clock () > clock->expire;
}

static struct igc_clock
make_clock (double secs)
{
  /* FIXME/igc: what does this do on 32-bit systems? */
  mps_clock_t per_sec = mps_clocks_per_sec ();
  mps_clock_t expire = mps_clock () + secs * per_sec;
  return (struct igc_clock) { .expire = expire };
}

#define IGC_WITH_CLOCK(c, duration)							\
  for (struct igc_clock c = make_clock (duration); !clock_has_expired (&c);)

/* Process MPS messages.  This should be extended to handle messages only
   for a certain amount of time.  See mps_clock_t, mps_clock, and
   mps_clocks_per_sec functions.  */

static bool
process_one_message (struct igc *gc)
{
  mps_message_t msg;
  if (mps_message_get (&msg, gc->arena, mps_message_type_finalization ()))
    {
      mps_addr_t base_addr;
      mps_message_finalization_ref (&base_addr, gc->arena, msg);
      /* FIXME/igc: other threads should be suspended while finalizing objects.  */
      finalize (gc, base_addr);
    }
  else if (mps_message_get (&msg, gc->arena, mps_message_type_gc_start ()))
    {
      if (garbage_collection_messages)
	{
	  message1 ("Garbage collecting...");
	  const char *why = mps_message_gc_start_why (gc->arena, msg);
	  message1 (why);
	}
    }
  else
    return false;

  mps_message_discard (gc->arena, msg);
  return true;
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
  IGC_WITH_CLOCK (clock, 0.1)
  {
    if (!process_one_message (global_igc))
      break;
  }
}

/* Discard entries for killed buffers from LIST and return the resulting
   list.  Used in window-{next,prev}-buffers.  */

Lisp_Object
igc_discard_killed_buffers (Lisp_Object list)
{
  Lisp_Object *prev = &list;
  for (Lisp_Object tail = list; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object buf = XCAR (tail);
      if (CONSP (buf))
	buf = XCAR (buf);
      if (BUFFERP (buf) && !BUFFER_LIVE_P (XBUFFER (buf)))
	*prev = XCDR (tail);
      else
	prev = xcdr_addr (tail);
    }
  return list;
}

struct igc_buffer_it
{
  Lisp_Object alist;
  Lisp_Object buf;
};

static struct igc_buffer_it
make_buffer_it (void)
{
  return (struct igc_buffer_it) { .alist = Vbuffer_alist, .buf = Qnil };
}

static bool
is_buffer_it_valid (struct igc_buffer_it *it)
{
  if (!CONSP (it->alist))
    return false;
  Lisp_Object elt = XCAR (it->alist);
  igc_assert (CONSP (elt));
  it->buf = XCDR (elt);
  return true;
}

static void
buffer_it_next (struct igc_buffer_it *it)
{
  igc_assert (CONSP (it->alist));
  it->alist = XCDR (it->alist);
  it->buf = Qnil;
}

static bool
arena_step (void)
{
  /* mps_arena_step does not guarantee to return swiftly.  And it seems
     that it sometimes does an opportunistic full collection alleging
     the client predicted lots of idle time.  But it doesn't tell how
     it comes to that conclusion.  */
  if (!FIXNUMP (Vigc_step_interval)
      || XFIXNUM (Vigc_step_interval) != 0)
    {
      double interval = 0;
      if (NUMBERP (Vigc_step_interval))
	{
	  interval = XFLOATINT (Vigc_step_interval);
	  if (interval < 0)
	    interval = 0.05;
	}

      if (mps_arena_step (global_igc->arena, interval, 0))
	return true;
    }

  return false;
}

static bool
buffer_step (struct igc_buffer_it *it)
{
  if (is_buffer_it_valid (it))
    {
      Lisp_Object buf = it->buf;
      buffer_it_next (it);
      struct buffer *b = XBUFFER (buf);
      compact_buffer (b);

      /* Balancing intervals during GC was originally introduced in the
	 new redisplay because the interval tree was notorious for
	 degenerating.  The interval tree should have been improved a in
	 the 20 years since then.  */
      if (igc__balance_intervals)
	b->text->intervals = balance_intervals (b->text->intervals);

      return true;
    }
  return false;
}

void
igc_on_idle (void)
{
  shrink_regexp_cache ();

  struct igc_buffer_it buffer_it = make_buffer_it ();
  IGC_WITH_CLOCK (clock, 0.1)
  {
    bool work_done = false;
    switch (igc_state)
      {
      case IGC_STATE_INITIAL:
      case IGC_STATE_USABLE_PARKED:
      case IGC_STATE_DEAD:
	return;

      case IGC_STATE_USABLE:
	work_done |= process_one_message (global_igc);
	work_done |= buffer_step (&buffer_it);
	work_done |= arena_step ();
	break;
      }

    /* Don't always exhaust the max time we want to spend here.  */
    if (!work_done)
      break;
  }
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
    case IGC_OBJ_BUILTIN_SYMBOL:
    case IGC_OBJ_BUILTIN_THREAD:
    case IGC_OBJ_BUILTIN_SUBR:
    case IGC_OBJ_DUMPED_CHARSET_TABLE:
    case IGC_OBJ_DUMPED_CODE_SPACE_MASKS:
    case IGC_OBJ_DUMPED_BUFFER_TEXT:
    case IGC_OBJ_DUMPED_BIGNUM_DATA:
    case IGC_OBJ_DUMPED_BYTES:
    case IGC_OBJ_NUM_TYPES:
      emacs_abort ();

    case IGC_OBJ_MARKER_VECTOR:
      return t->d.weak_weak_ap;

    case IGC_OBJ_WEAK_HASH_TABLE_WEAK_PART:
      return t->d.weak_hash_weak_ap;

    case IGC_OBJ_WEAK_HASH_TABLE_STRONG_PART:
      return t->d.weak_hash_strong_ap;

    case IGC_OBJ_VECTOR:
    case IGC_OBJ_CONS:
    case IGC_OBJ_SYMBOL:
    case IGC_OBJ_INTERVAL:
    case IGC_OBJ_STRING:
    case IGC_OBJ_ITREE_TREE:
    case IGC_OBJ_ITREE_NODE:
    case IGC_OBJ_IMAGE:
    case IGC_OBJ_IMAGE_CACHE:
    case IGC_OBJ_FACE:
    case IGC_OBJ_FACE_CACHE:
    case IGC_OBJ_BLV:
    case IGC_OBJ_HANDLER:
      return t->d.dflt_ap;

    case IGC_OBJ_STRING_DATA:
    case IGC_OBJ_FLOAT:
    case IGC_OBJ_BYTES:
      return t->d.leaf_ap;
    }
  emacs_abort ();
}

/* Conditional breakpoints can be so slow that it is often more
   effective to instrument code.  This function is for such cases.  */

void
igc_break (void)
{
}

void
igc_collect (void)
{
  struct igc *gc = global_igc;
  if (gc->park_count == 0)
    {
      mps_res_t res = mps_arena_collect (gc->arena);
      IGC_CHECK_RES (res);
      mps_arena_release (gc->arena);
    }
}

DEFUN ("igc--collect", Figc__collect, Sigc__collect, 0, 0, 0,
       doc: /* Force an immediate arena garbage collection.  */)
  (void)
{
  igc_collect ();
  return Qnil;
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

  struct igc_header *h = client;
  return igc_header_hash (h);
}

/* Allocate an object of client size SIZE and of type TYPE from
   allocation point AP.  Value is a pointer to the client area of the new
   object.  */

static mps_addr_t
alloc_impl (size_t size, enum igc_obj_type type, mps_ap_t ap)
{
  mps_addr_t p UNINIT;
  size = alloc_size (size);
  switch (igc_state)
    {
    case IGC_STATE_USABLE_PARKED:
    case IGC_STATE_USABLE:
      do
	{
	  mps_res_t res = mps_reserve (&p, ap, size);
	  if (res != MPS_RES_OK)
	    memory_full (0);
	  /* Object _must_ have valid contents before commit.  */
	  memclear (p, size);
	  set_header (p, type, size, alloc_hash ());
	}
      while (!mps_commit (ap, p, size));
      break;

    case IGC_STATE_DEAD:
      p = xzalloc (size);
      set_header (p, type, size, alloc_hash ());
      break;

    case IGC_STATE_INITIAL:
      emacs_abort ();
    }
  return p;
}

/* Allocate an object of client size SIZE and of type TYPE from a
   type-dependent allocation point.  Value is a pointer to the client
   area of the new object.  */

static mps_addr_t
alloc (size_t size, enum igc_obj_type type)
{
  return alloc_impl (size, type, thread_ap (type));
}

/* Allocate an object of client size SIZE and of type TYPE from MPS in a
   way tnat ensure that the object will not move in memory.  Value is a
   pointer to the client area of the new object.  */

static mps_addr_t
alloc_immovable (size_t size, enum igc_obj_type type)
{
  struct igc_thread_list *t = current_thread->gc_info;
  return alloc_impl (size, type, t->d.immovable_ap);
}

#ifdef HAVE_MODULES
void *
igc_alloc_global_ref (void)
{
  size_t nwords_mem = VECSIZE (struct module_global_reference);
  struct Lisp_Vector *v
    = alloc_immovable (header_size + nwords_mem * word_size, IGC_OBJ_VECTOR);
  XSETPVECTYPESIZE (v, PVEC_MODULE_GLOBAL_REFERENCE, 0, nwords_mem);
  return v;
}
#endif

Lisp_Object
igc_make_cons (Lisp_Object car, Lisp_Object cdr)
{
  struct Lisp_Cons *cons = alloc (sizeof *cons, IGC_OBJ_CONS);
  cons->u.s.car = car;
  cons->u.s.u.cdr = cdr;
  return make_lisp_ptr (cons, Lisp_Cons);
}

Lisp_Object
igc_alloc_symbol (void)
{
  struct Lisp_Symbol *sym = alloc (sizeof *sym, IGC_OBJ_SYMBOL);
  return make_lisp_symbol (sym);
}

Lisp_Object
igc_make_float (double val)
{
  struct Lisp_Float *f = alloc (sizeof *f, IGC_OBJ_FLOAT);
  f->u.data = val;
  return make_lisp_ptr (f, Lisp_Float);
}

static unsigned char *
alloc_string_data (size_t nbytes, bool clear)
{
  struct Lisp_String_Data *data =
    alloc (sizeof (*data) + nbytes + 1, IGC_OBJ_STRING_DATA);
  data->data[nbytes] = 0;
  return data->data;
}

void *
igc_alloc_bytes (size_t nbytes)
{
  struct Lisp_String_Data *data =
    alloc (sizeof (*data) + nbytes, IGC_OBJ_STRING_DATA);
  return data->data;
}

/* Reallocate multibyte STRING data when a single character is
   replaced.  The character is at byte offset BYTE_POS in the string.
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
  struct igc_header *old_header =
    (struct igc_header *)(s->u.s.data - sizeof (struct igc_header));

  /* The capacity is the number of bytes the client has available,
     including the terminating NUL byte.  Sizes computed from Lisp
     strings don't include the NUL.  That's the 1 in the if.  */
  ptrdiff_t capacity = obj_client_size (old_header);
  if (capacity < nbytes_needed + 1)
    {
      unsigned char *new_data = alloc_string_data (nbytes_needed, false);
      memcpy (new_data, SDATA (string), old_nbytes + 1);
      s->u.s.data = new_data;
    }

  /* Set up string as if the character had been inserted.  */
  s->u.s.size_byte = nbytes_needed;
  unsigned char *insertion_addr = s->u.s.data + at_byte_pos;
  /* Need to move the rest of the string after the insertion point
     (old_nbytes - at_bytespos), but not the old char itself (-
     old_char_len), but do include the NUL at the end (+ 1).  */
  ptrdiff_t nbytes_to_move = old_nbytes - at_byte_pos - old_char_len + 1;
  memmove (insertion_addr + new_char_len, insertion_addr + old_char_len,
	   nbytes_to_move);
  return insertion_addr;
}

Lisp_Object
igc_make_string (size_t nchars, size_t nbytes, bool unibyte, bool clear)
{
  struct Lisp_String *s = alloc (sizeof *s, IGC_OBJ_STRING);
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
  return alloc (sizeof (struct interval), IGC_OBJ_INTERVAL);
}

struct Lisp_Vector *
igc_alloc_pseudovector (size_t nwords_mem, size_t nwords_lisp,
			size_t nwords_zero, enum pvec_type tag)
{
  /* header_size comes from lisp.h.  */
  size_t client_size = header_size + nwords_mem * sizeof (Lisp_Object);
  struct Lisp_Vector *v;
  if (tag == PVEC_THREAD)
    {
      /* Alloc thread_state immovable because we need access to it for
	 scanning the bytecode stack (scan_bc), and making thread_state
	 immovable simplifies the code.  */
      v = alloc_immovable (client_size, IGC_OBJ_VECTOR);
    }
  else
    v = alloc (client_size, IGC_OBJ_VECTOR);
  XSETPVECTYPESIZE (v, tag, nwords_lisp, nwords_mem - nwords_lisp);
  maybe_finalize (v, tag);
  return v;
}

struct Lisp_Vector *
igc_alloc_vector (ptrdiff_t len)
{
  struct Lisp_Vector *v
    = alloc (header_size + len * word_size, IGC_OBJ_VECTOR);
  v->header.size = len;
  return v;
}

struct Lisp_Vector *
igc_alloc_record (ptrdiff_t len)
{
  struct Lisp_Vector *v
    = alloc (header_size + len * word_size, IGC_OBJ_VECTOR);
  v->header.size = len;
  XSETPVECTYPE (v, PVEC_RECORD);
  return v;
}

struct itree_tree *
igc_make_itree_tree (void)
{
  struct itree_tree *t = alloc (sizeof *t, IGC_OBJ_ITREE_TREE);
  return t;
}

struct itree_node *
igc_make_itree_node (void)
{
  struct itree_node *n = alloc (sizeof *n, IGC_OBJ_ITREE_NODE);
  return n;
}

#ifdef HAVE_WINDOW_SYSTEM
struct image *
igc_make_image (void)
{
  struct image *img = alloc (sizeof *img, IGC_OBJ_IMAGE);
  return img;
}
#endif

struct face *
igc_make_face (void)
{
  struct face *face = alloc (sizeof *face, IGC_OBJ_FACE);
  return face;
}

struct face_cache *
igc_make_face_cache (void)
{
  struct face_cache *c = alloc (sizeof *c, IGC_OBJ_FACE_CACHE);
  return c;
}

/* Allocate a Lisp_Object vector with N elements.
   Currently only used by SAFE_ALLOCA_LISP.  */

Lisp_Object *
igc_alloc_lisp_obj_vec (size_t n)
{
  Lisp_Object v = make_vector (n, Qnil);
  return XVECTOR (v)->contents;
}

static mps_addr_t
weak_hash_find_dependent (mps_addr_t base)
{
  struct igc_header *h = base;
  switch (igc_header_type (h))
    {
    case IGC_OBJ_WEAK_HASH_TABLE_WEAK_PART:
      {
	mps_addr_t client = base;
	struct Lisp_Weak_Hash_Table_Weak_Part *w = client;
	return w->strong;
      }
    case IGC_OBJ_WEAK_HASH_TABLE_STRONG_PART:
      {
	mps_addr_t client = base;
	struct Lisp_Weak_Hash_Table_Strong_Part *w = client;
	return w->weak;
      }
    default:
      emacs_abort ();
    }

  return 0;
}

Lisp_Object *
igc_make_hash_table_vec (size_t n)
{
  return XVECTOR (make_vector (n, Qnil))->contents;
}

Lisp_Object
weak_hash_table_entry (struct Lisp_Weak_Hash_Table_Entry entry)
{
  intptr_t alignment = entry.intptr & 1;
  mps_addr_t client;

  if (alignment == 0)
    {
      client = (mps_addr_t)(uintptr_t)entry.intptr;
    }
  else
    {
      EMACS_UINT real_ptr = entry.intptr ^ alignment;
      client = (mps_addr_t)(uintptr_t)real_ptr;
    }

  switch (XFIXNUM (entry.fixnum))
    {
    case Lisp_Symbol:
      return make_lisp_symbol (client);
    case_Lisp_Int:
      return make_fixnum ((EMACS_INT)entry.intptr >> 1);
    default:
      return make_lisp_ptr (client, XFIXNUM (entry.fixnum));
    }
}

struct Lisp_Weak_Hash_Table_Entry
make_weak_hash_table_entry (Lisp_Object obj)
{
  struct Lisp_Weak_Hash_Table_Entry entry = { 0, };
  mps_addr_t client;
  entry.fixnum = make_fixnum (XTYPE (obj));

  if (FIXNUMP (obj))
    {
      entry.intptr = (XFIXNUM (obj) << 1) + 1;
      return entry;
    }
  else if (BARE_SYMBOL_P (obj))
    client = XBARE_SYMBOL (obj);
  else
    client = XUNTAG (obj, XTYPE (obj), void);

  entry.intptr = (EMACS_UINT)(uintptr_t)client;

  return entry;
}

struct Lisp_Weak_Hash_Table_Strong_Part *
igc_alloc_weak_hash_table_strong_part (hash_table_weakness_t weak,
				       size_t size, size_t index_bits)
{
  size_t total_size;
  switch (weak)
    {
    case Weak_Key:
      total_size = 3 * size + ((ptrdiff_t)1 << index_bits);
      break;
    case Weak_Value:
      total_size = 3 * size + ((ptrdiff_t)1 << index_bits);
      break;
    case Weak_Key_And_Value:
    case Weak_Key_Or_Value:
      total_size = 2 * size + ((ptrdiff_t)1 << index_bits);
      break;
    default:
      emacs_abort ();
    }
  return alloc (sizeof (struct Lisp_Weak_Hash_Table_Strong_Part) +
		total_size * sizeof (struct Lisp_Weak_Hash_Table_Entry),
		IGC_OBJ_WEAK_HASH_TABLE_STRONG_PART);
}

struct Lisp_Weak_Hash_Table_Weak_Part *
igc_alloc_weak_hash_table_weak_part (hash_table_weakness_t weak,
				     size_t size, size_t index_bits)
{
  size_t total_size;
  switch (weak)
    {
    case Weak_Key:
      total_size = size;
      break;
    case Weak_Value:
      total_size = size;
      break;
    case Weak_Key_And_Value:
    case Weak_Key_Or_Value:
      total_size = 2 * size;
      break;
    default:
      emacs_abort ();
    }
  return alloc (sizeof (struct Lisp_Weak_Hash_Table_Weak_Part) +
		total_size * sizeof (struct Lisp_Weak_Hash_Table_Entry),
		IGC_OBJ_WEAK_HASH_TABLE_WEAK_PART);
}

#ifdef HAVE_WINDOW_SYSTEM
struct image_cache *
igc_make_image_cache (void)
{
  struct image_cache *c = alloc (sizeof *c, IGC_OBJ_IMAGE_CACHE);
  return c;
}
#endif

struct Lisp_Buffer_Local_Value *
igc_alloc_blv (void)
{
  struct Lisp_Buffer_Local_Value *blv
    = alloc (sizeof *blv, IGC_OBJ_BLV);
  return blv;
}

void *
igc_alloc_handler (void)
{
  struct handler *h = alloc (sizeof *h, IGC_OBJ_HANDLER);
  return h;
}

int
igc_valid_lisp_object_p (Lisp_Object obj)
{
  return 1;
}

static Lisp_Object
alloc_marker_vector (ptrdiff_t len, Lisp_Object init)
{
  struct Lisp_Vector *v
    = alloc (header_size + len * word_size, IGC_OBJ_MARKER_VECTOR);
  v->header.size = len;
  for (ptrdiff_t i = 0; i < len; ++i)
    v->contents[i] = init;
  return make_lisp_ptr (v, Lisp_Vectorlike);
}

static Lisp_Object
larger_marker_vector (Lisp_Object v)
{
  igc_assert (NILP (v) || (VECTORP (v) && XFIXNUM (AREF (v, 0)) < 0));
  ptrdiff_t old_len = NILP (v) ? 0 : ASIZE (v);
  ptrdiff_t new_len = max (2, 2 * old_len);
  Lisp_Object new_v = alloc_marker_vector (new_len, Qnil);
  ptrdiff_t i = 0;
  if (VECTORP (v))
    for (i = 1; i < ASIZE (v); ++i)
      ASET (new_v, i, AREF (v, i));
  for (; i < ASIZE (new_v) - 1; ++i)
    ASET (new_v, i, make_fixnum (i + 1));
  ASET (new_v, i, make_fixnum (-1));
  ASET (new_v, 0, make_fixnum (NILP (v) ? 1 : ASIZE (v)));
  return new_v;
}

void
igc_add_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object v = BUF_MARKERS (b);
  igc_assert (NILP (v) || VECTORP (v));
  ptrdiff_t next_free = NILP (v) ? -1 : XFIXNUM (AREF (v, 0));
  if (next_free < 0)
    {
      v = BUF_MARKERS (b) = larger_marker_vector (v);
      next_free = XFIXNUM (AREF (v, 0));
    }
  ASET (v, 0, AREF (v, next_free));
  ASET (v, next_free, make_lisp_ptr (m, Lisp_Vectorlike));
  m->index = next_free;
  m->buffer = b;
}

void
igc_remove_marker (struct buffer *b, struct Lisp_Marker *m)
{
  Lisp_Object v = BUF_MARKERS (b);
  igc_assert (VECTORP (v));
  igc_assert (m->index >= 1 && m->index < ASIZE (v));
  igc_assert (MARKERP (AREF (v, m->index)) && XMARKER (AREF (v, m->index)) == m);
  ASET (v, m->index, AREF (v, 0));
  ASET (v, 0, make_fixnum (m->index));
  m->index = -1;
  m->buffer = NULL;
}

void
igc_remove_all_markers (struct buffer *b)
{
  Lisp_Object v = BUF_MARKERS (b);
  if (VECTORP (v))
    {
      for (ptrdiff_t i = 1; i < ASIZE (v); ++i)
	if (MARKERP (AREF (v, i)))
	  XMARKER (AREF (v, i))->buffer = NULL;
      BUF_MARKERS (b) = Qnil;
    }
}

static bool
weak_vector_p (Lisp_Object x)
{
  if (VECTORP (x))
    {
      struct igc *igc = global_igc;
      mps_pool_t pool = NULL;
      if (!mps_addr_pool (&pool, igc->arena, XVECTOR (x)))
	return false;
      return pool == igc->weak_pool;
    }
  else
    return false;
}

void
igc_resurrect_markers (struct buffer *b)
{
  Lisp_Object old = BUF_MARKERS (b);
  if (NILP (old))
    return;
  igc_assert (!weak_vector_p (old));
  size_t len = ASIZE (old);
  Lisp_Object new = alloc_marker_vector (len, Qnil);
  memcpy (XVECTOR (new)->contents, XVECTOR (old)->contents,
	  len * sizeof (Lisp_Object));
  BUF_MARKERS (b) = new;
  igc_assert (weak_vector_p (BUF_MARKERS (b)));
}

static void
walk_pool (struct igc *gc, mps_pool_t p, struct igc_stats *st)
{
  mps_res_t res;
  IGC_WITH_PARKED (gc)
    res = mps_pool_walk (p, dflt_scanx, st);
  if (res != MPS_RES_OK)
    error ("Error %d walking memory", res);
}

static Lisp_Object
make_entry (const char *s, intmax_t n, intmax_t bytes, intmax_t largest)
{
  return list4 (build_string (s), make_int (n), make_int (bytes), make_int (largest));
}

#ifndef __clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"
#endif

DEFUN ("igc-info", Figc_info, Sigc_info, 0, 0, 0,
       doc: /* Return information about incremental GC.
The return value is a list of elements describing the various
statistics of the incremental GC.  The elements are of the
form (NAME NOBJECTS NBYTES LARGEST), where:
- NAME is a string describing the kind of objects this entry represents,
- NOBJECTS is the number of objects of this type,
- NBYTES is the number of bytes used by objects of this type,
- LARGEST is the largest object of this type.

In addition, there are several pseudo-objects which provide overall
IGC statistics:
 - committed       -- the amount of committed memory in bytes
 - commit-limit    -- max amount of memory the arena is allowed to commit
 - spare-committed -- memory which remains committed and which the
     arena is managing as free memory
 - reserved        -- total address space reserved by the arena
 - spare           -- spare commit limit fraction
 - pause-time      -- max amount of time GC operations may pause Emacs.  */)
  (void)
{
  struct igc *gc = global_igc;
  struct igc_stats st = { 0 };
  walk_pool (gc, gc->dflt_pool, &st);
  walk_pool (gc, gc->leaf_pool, &st);
  walk_pool (gc, gc->weak_pool, &st);
  walk_pool (gc, gc->weak_hash_pool, &st);
  walk_pool (gc, gc->immovable_pool, &st);

  Lisp_Object result = Qnil, e;
  for (int i = 0; i < IGC_OBJ_NUM_TYPES; ++i)
    {
      e = make_entry (obj_type_name (i), st.obj[i].nobjs, st.obj[i].nbytes, st.obj[i].largest);
      result = Fcons (e, result);
    }
  for (enum pvec_type i = 0; i <= PVEC_TAG_MAX; ++i)
    {
      e = make_entry (pvec_type_name (i), st.pvec[i].nobjs, st.pvec[i].nbytes, st.pvec[i].largest),
	result = Fcons (e, result);
    }
  e = list4 (build_string ("pause-time"), Qnil, make_float (mps_arena_pause_time (gc->arena)),
	     Qnil);
  result = Fcons (e, result);
  e = list4 (build_string ("spare"), Qnil, make_float (mps_arena_spare (gc->arena)),
	     Qnil);
  result = Fcons (e, result);
  e = make_entry ("reserved", 1, mps_arena_reserved (gc->arena), 0);
  result = Fcons (e, result);
  e = make_entry ("spare-committed", 1, mps_arena_spare_committed (gc->arena), 0);
  result = Fcons (e, result);
  e = make_entry ("commit-limit", 1, mps_arena_commit_limit (gc->arena), 0);
  result = Fcons (e, result);
  e = make_entry ("committed", 1, mps_arena_committed (gc->arena), 0);
  result = Fcons (e, result);
  return result;
}

#ifndef __clang__
#pragma GCC diagnostic pop
#endif

DEFUN ("igc--roots", Figc__roots, Sigc__roots, 0, 0, 0,
       doc: /* Return the list of IGC roots.
The return value is a list of elements, one each for every
root.  Each element has the form (LABEL TYPE START END), where
 LABEL is the label of the root
 TYPE is either 'ambig' or 'exact'
 START is the start address
 END is either the end address or nil.  */)
  (void)
{
  struct igc *gc = global_igc;
  Lisp_Object roots = Qnil;

  for (igc_root_list *r = gc->roots; r; r = r->next)
    {
      Lisp_Object type = r->d.ambig ? Qambig : Qexact;
      Lisp_Object label = r->d.label ? build_string (r->d.label) : Qnil;
      Lisp_Object e = list4 (label, type, make_int ((uintptr_t) r->d.start),
			     r->d.end ? make_int ((uintptr_t) r->d.end) : Qnil);
      roots = Fcons (e, roots);
    }

  return roots;
}

static struct igc_exthdr *
igc_external_header (struct igc_header *h)
{
  if (header_tag (h) != IGC_TAG_EXTHDR)
    {
      struct igc_exthdr *exthdr = xmalloc (sizeof *exthdr);
      exthdr->nwords = header_nwords (h);
      exthdr->hash = header_hash (h);
      exthdr->obj_type = header_type (h);
      exthdr->extra_dependency = Qnil;
      /* On IA-32, the upper 32-bit word is 0 after this, which is okay.  */
      uint64_t v = (intptr_t)exthdr + IGC_TAG_EXTHDR;
      *(uint64_t *) h = v;
      mps_addr_t ref = (mps_addr_t) h;
      mps_res_t res = mps_finalize (global_igc->arena, &ref);
      IGC_CHECK_RES (res);
      return exthdr;
    }

  return header_exthdr (h);
}

static const char *
mps_res_to_string (mps_res_t res)
{
  switch (res)
    {
#define RES_CASE(prefix, id, doc)                                            \
  case prefix##id:                                                           \
    return #prefix #id " " doc;
      _mps_RES_ENUM (RES_CASE, MPS_RES_);
#undef RES_CASE
    default:
      return NULL;
    }
}

DEFUN ("igc--set-commit-limit", Figc__set_commit_limit,
       Sigc__set_commit_limit, 1, 1, 0,
       doc: /* Set the arena commit limit to LIMIT.
LIMIT can be an integer (number of bytes) or nil (no limit).

Do NOT use this for anything but testing, unless you
really know what you are doing!  */)
  (Lisp_Object limit)
{
  size_t nbytes
      = NILP (limit) ? ~0 : check_uinteger_max (limit, SIZE_MAX - 1);
  mps_res_t err = mps_arena_commit_limit_set (global_igc->arena, nbytes);
  if (err != MPS_RES_OK)
    {
      const char *msg = mps_res_to_string (err);
      xsignal3 (Qerror,
		intern_c_string (Sigc__set_commit_limit.s.symbol_name),
		make_fixnum (err), msg ? build_string (msg) : Qnil);
    }
  return Qnil;
}

static void
make_arena (struct igc *gc)
{
  mps_res_t res;
  MPS_ARGS_BEGIN (args)
  {
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
    /* Don't use in-band headers.  I suspect they have problems,
       specifically amcSegScanNailedRange calls NailboardGet with a
       client address, which calls NailboardGet, and one can see that
       the the board contains base addresses which leads to an assertion
       failure.  */
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
make_pool_with_class (struct igc *gc, mps_fmt_t fmt, mps_class_t cls, mps_awl_find_dependent_t find_dependent)
{
  mps_res_t res;
  mps_pool_t pool;
  MPS_ARGS_BEGIN (args)
  {
    MPS_ARGS_ADD (args, MPS_KEY_FORMAT, fmt);
    MPS_ARGS_ADD (args, MPS_KEY_CHAIN, gc->chain);
    MPS_ARGS_ADD (args, MPS_KEY_INTERIOR, true);
    if (find_dependent)
      MPS_ARGS_ADD (args, MPS_KEY_AWL_FIND_DEPENDENT, find_dependent);
    res = mps_pool_create_k (&pool, gc->arena, cls, args);
  }
  MPS_ARGS_END (args);
  IGC_CHECK_RES (res);
  return pool;
}

static mps_pool_t
make_pool_amc (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_amc (), NULL);
}

static mps_pool_t
make_pool_ams (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_ams (), NULL);
}

static mps_pool_t
make_pool_awl (struct igc *gc, mps_fmt_t fmt, mps_awl_find_dependent_t find_dependent)
{
  return make_pool_with_class (gc, fmt, mps_class_awl (), find_dependent);
}

static mps_pool_t
make_pool_amcz (struct igc *gc, mps_fmt_t fmt)
{
  return make_pool_with_class (gc, fmt, mps_class_amcz (), NULL);
}

static struct igc *
make_igc (void)
{
  struct igc *gc = xzalloc (sizeof *gc);
  make_arena (gc);

  /* We cannot let the GC run until at least all staticpros haven been
     processed.  Otherwise we might allocate objects that are not
     protected by anything.  */
  arena_park (gc);

  gc->dflt_fmt = make_dflt_fmt (gc);
  gc->dflt_pool = make_pool_amc (gc, gc->dflt_fmt);
  gc->leaf_fmt = make_dflt_fmt (gc);
  gc->leaf_pool = make_pool_amcz (gc, gc->leaf_fmt);
  gc->weak_fmt = make_dflt_fmt (gc);
  gc->weak_pool = make_pool_awl (gc, gc->weak_fmt, NULL);
  gc->weak_hash_fmt = make_dflt_fmt (gc);
  gc->weak_hash_pool = make_pool_awl (gc, gc->weak_hash_fmt, weak_hash_find_dependent);
  gc->immovable_fmt = make_dflt_fmt (gc);
  gc->immovable_pool = make_pool_ams (gc, gc->immovable_fmt);

#ifndef IN_MY_FORK
  root_create_pure (gc);
#endif
  root_create_charset_table (gc);
  root_create_buffer (gc, &buffer_defaults);
  root_create_buffer (gc, &buffer_local_symbols);
  root_create_staticvec (gc);
  root_create_lispsym (gc);
  root_create_terminal_list (gc);
  root_create_tty_list (gc);
  root_create_main_thread (gc);
  root_create_exact_ptr (gc, &current_thread);
  root_create_exact_ptr (gc, &all_threads);

  enable_messages (gc, true);
  return gc;
}

void
igc_on_staticpros_complete (void)
{
  set_state (IGC_STATE_USABLE);
}

/* To call from LLDB.  */

void
igc_postmortem (void)
{
  if (global_igc && global_igc->arena)
    mps_arena_postmortem (global_igc->arena);
}


/***********************************************************************
				Dumping
 ***********************************************************************/

size_t
igc_header_size (void)
{
  return sizeof (struct igc_header);
}

static enum igc_obj_type
builtin_obj_type_and_hash (size_t *hash, enum igc_obj_type type, void *client)
{
  if (c_symbol_p (client))
    {
      *hash = igc_hash (make_lisp_symbol (client));
      return IGC_OBJ_BUILTIN_SYMBOL;
    }

  if (client == &main_thread.s)
    {
      *hash = igc_hash (make_lisp_ptr (client, Lisp_Vectorlike));
      return IGC_OBJ_BUILTIN_THREAD;
    }

  if (is_builtin_subr (type, client))
    {
      *hash = igc_hash (make_lisp_ptr (client, Lisp_Vectorlike));
      return IGC_OBJ_BUILTIN_SUBR;
    }

  if (type == IGC_OBJ_DUMPED_CHARSET_TABLE
      || type == IGC_OBJ_DUMPED_CODE_SPACE_MASKS
      || type == IGC_OBJ_DUMPED_BUFFER_TEXT
      || type == IGC_OBJ_DUMPED_BIGNUM_DATA
      || type == IGC_OBJ_DUMPED_BYTES)
    {
      *hash = 0;
      return type;
    }

  emacs_abort ();
}

static enum igc_obj_type
pure_obj_type_and_hash (size_t *hash_o, enum igc_obj_type type, void *client)
{
  switch (type)
    {
    case IGC_OBJ_STRING:
      *hash_o = igc_hash (make_lisp_ptr (client, Lisp_String));
      return type;

    case IGC_OBJ_VECTOR:
      *hash_o = igc_hash (make_lisp_ptr (client, Lisp_Vectorlike));
      return type;

    case IGC_OBJ_CONS:
      *hash_o = igc_hash (make_lisp_ptr (client, Lisp_Cons));
      return type;

    case IGC_OBJ_STRING_DATA:
      *hash_o = (uintptr_t) client & IGC_HEADER_HASH_MASK;
      return type;

    case IGC_OBJ_FLOAT:
      *hash_o = igc_hash (make_lisp_ptr (client, Lisp_Float));
      return type;

    default:
      IGC_NOT_IMPLEMENTED ();
      emacs_abort ();
    }
}

/* Called from the dumper at the end of dumping an object.  This
   function is responsible for filling out the igc_header of the dumped
   object.  CLIENT points to the object being dumped.  TYPE is the type
   of object the pdumper intends to write.  BASE points to where in the
   dump CLIENT has been written, i.e. it is a pointer to its header in
   the dump.  END is the current end of the object whose start is BASE.
   Value is the address in the dump where the object should end which
   can be >= end for alignment purposes.  */

char *
igc_dump_finish_obj (void *client, enum igc_obj_type type,
		     char *base, char *end)
{
  if (client == NULL)
    return end;

  bool is_in_dump;
  switch (type)
    {
    case IGC_OBJ_DUMPED_CHARSET_TABLE:
    case IGC_OBJ_DUMPED_CODE_SPACE_MASKS:
    case IGC_OBJ_DUMPED_BUFFER_TEXT:
    case IGC_OBJ_DUMPED_BIGNUM_DATA:
    case IGC_OBJ_DUMPED_BYTES:
      is_in_dump = true;
      break;

    default:
      is_in_dump = false;
      break;
    }

  struct igc_header *out = (struct igc_header *) base;

  /* If the client object to be dumped has a header, copy that.  */
  if (type != IGC_OBJ_DUMPED_BYTES && type != IGC_OBJ_DUMPED_CODE_SPACE_MASKS
      && type != IGC_OBJ_DUMPED_BUFFER_TEXT)
    if (!is_in_dump)
      {
	struct igc_header *h = client;
	if (igc_header_type (h) == IGC_OBJ_MARKER_VECTOR)
	  igc_assert ((type == IGC_OBJ_VECTOR
		       && igc_header_type (h) == IGC_OBJ_MARKER_VECTOR)
		      || header_type (h) == type);
	igc_assert (base + obj_size (h) >= end);
	if (type != IGC_OBJ_DUMPED_BYTES &&
	    type != IGC_OBJ_DUMPED_CODE_SPACE_MASKS &&
	    type != IGC_OBJ_DUMPED_BUFFER_TEXT)
	  *out = *h;
	igc_assert (header_nwords (out) > 0);
	return base + obj_size (h);
      }

  /* We are dumping some non-MPS object, e.g. a built-in symbol.  */
  size_t client_size = end - base;
  size_t nbytes = alloc_size (client_size);
  size_t hash;
  type = (is_pure (client)
	  ? pure_obj_type_and_hash (&hash, type, client)
	  : builtin_obj_type_and_hash (&hash, type, client));
  set_header (out, type, nbytes, hash);
  return base + nbytes;
}

void
igc_dump_check_object_starts (Lisp_Object relocs, void *dump_base,
			      void *hot_start, void *hot_end,
			      void *cold_start, void *heap_end)
{
  eassert (is_aligned (dump_base));
  eassert (is_aligned (hot_start));
  eassert (is_aligned (hot_end));
  eassert (is_aligned (cold_start));
  eassert (is_aligned (hot_end));
  struct region
  {
    mps_addr_t start, end;
  } regions[] = {
    {hot_start, hot_end},
    {cold_start, heap_end},
  };
  for (size_t i = 0; i < ARRAYELTS (regions); i++)
    {
      struct region region = regions[i];
      mps_addr_t p = region.start;
      while (p != region.end)
	{
	  eassert (p < region.end);
	  Lisp_Object r = XCAR (relocs);
	  relocs = XCDR (relocs);
	  EMACS_INT start_off = XFIXNUM (XCAR (r));
	  EMACS_INT end_off = XFIXNUM (XCAR (XCDR (r)));
	  mps_addr_t start = (uint8_t *) dump_base + start_off;
	  mps_addr_t end = (uint8_t *) dump_base + end_off;
	  eassert (start == p);
	  p = dflt_skip (p);
	  eassert (end == p);
	}
    }
  eassert (NILP (relocs));
}

static bool
check_dump (mps_addr_t start, mps_addr_t end)
{
  struct pdumper_object_it it = { 0 };
  for (mps_addr_t p = start; p != end; p = dflt_skip (p))
    {
      eassert (p < end);
      struct igc_header *h = p;
      if (header_type (h) != IGC_OBJ_PAD)
	{
	  mps_addr_t obj = pdumper_next_object (&it);
	  eassert (p == obj);
	}
    }
  eassert (pdumper_next_object (&it) == NULL);
  return true;
}

static mps_addr_t pinned_objects_in_dump[3];

/* Called from pdumper_load.  [HOT_START, HOT_END) is the hot section of
   the dump.  [COL_START, COLD_END) is the cold section of the
   dump.  COLD_USER_DATA_START is where actual object memory starts.
   HEAP_END is the heap end as recorded in the dump header.  */

void
igc_on_pdump_loaded (void *dump_base, void *hot_start, void *hot_end,
		     void *cold_start, void *cold_end,
		     void *cold_user_data_start, void *heap_end)
{
  dump_base = (char *)dump_base - igc_header_size ();
  igc_assert (global_igc->park_count > 0);
  igc_assert (hot_start == charset_table);
  igc_assert (header_type ((struct igc_header *) hot_start)
	      == IGC_OBJ_DUMPED_CHARSET_TABLE);
  igc_assert (header_type ((struct igc_header *) cold_start)
	      == IGC_OBJ_DUMPED_CODE_SPACE_MASKS);
  igc_assert (header_type ((struct igc_header *) cold_user_data_start)
	      == IGC_OBJ_DUMPED_BYTES);
  igc_assert (header_type ((struct igc_header *) heap_end)
	      == IGC_OBJ_DUMPED_BYTES);

  size_t discardable_size = (uint8_t *)cold_start - (uint8_t *)hot_end;
  // size_t cold_size = (uint8_t *)cold_end - (uint8_t *)cold_start;
  size_t dump_header_size = (uint8_t *)hot_start - (uint8_t *)dump_base;
  size_t relocs_size = (uint8_t *)cold_end - (uint8_t *)heap_end;
  struct igc_header *h = dump_base;

  igc_assert (header_type (h) == IGC_OBJ_INVALID);
  igc_assert (obj_size (h)
	      == (uint8_t *)cold_end - (uint8_t *)dump_base);
  igc_assert (discardable_size > 2 * sizeof *h);
  /* Ignore dump_header */
  set_header (h, IGC_OBJ_PAD, dump_header_size, 0);
  /* Ignore discardable section */
  set_header (hot_end, IGC_OBJ_PAD, discardable_size, 0);
  /* Ignore relocs */
  set_header (heap_end, IGC_OBJ_PAD, relocs_size, 0);

  eassert (check_dump (h, cold_end));
  /* Pin some stuff in the dump  */
  mps_addr_t pinned_roots[] = {
    charset_table,
    cold_start, /* code_space_masks */
    cold_user_data_start,
  };
  static_assert (sizeof pinned_roots == sizeof pinned_objects_in_dump);
  memcpy (pinned_objects_in_dump, pinned_roots, sizeof pinned_roots);
  igc_root_create_ambig (pinned_objects_in_dump,
			 (uint8_t *) pinned_objects_in_dump
			   + sizeof pinned_objects_in_dump,
			 "dump-pins");
}

void *
igc_alloc_dump (size_t nbytes)
{
  igc_assert (global_igc->park_count > 0);
  mps_ap_t ap = thread_ap (IGC_OBJ_CONS);
  size_t block_size = igc_header_size () + nbytes;
  mps_addr_t block;
  do
    {
      mps_res_t res = mps_reserve (&block, ap, block_size);
      if (res != MPS_RES_OK)
	memory_full (0);
      set_header (block, IGC_OBJ_INVALID, block_size, 0);
    }
  while (!mps_commit (ap, block, block_size));
  return (char *) block + igc_header_size ();
}

bool
igc_busy_p (void)
{
  return mps_arena_busy (global_igc->arena);
}


DEFUN ("igc--add-extra-dependency", Figc__add_extra_dependency,
       Sigc__add_extra_dependency, 3, 3, 0,
       doc: /* Add an extra DEPENDENCY to object OBJ, associate it with KEY.
This dependency is kept alive for as long as the object is alive.
KEY is the key to associate with DEPENDENCY in a hash table.  */)
  (Lisp_Object obj, Lisp_Object dependency, Lisp_Object key)
{
  mps_word_t word = XLI (obj);
  mps_word_t tag = word & IGC_TAG_MASK;
  mps_addr_t client = NULL;
  switch (tag)
    {
    case Lisp_Type_Unused0:
      emacs_abort ();

    case Lisp_Int0:
    case Lisp_Int1:
      return Qnil;

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

  struct igc_header *h = client;
  struct igc_exthdr *exthdr = igc_external_header (h);
  Lisp_Object hash = exthdr->extra_dependency;
  if (!WEAK_HASH_TABLE_P (hash))
    {
      hash = exthdr->extra_dependency =
	CALLN (Fmake_hash_table, QCweakness, Qkey);
    }

  if (NILP (Fgethash (key, hash, Qnil)))
    {
      Fputhash (key, CALLN (Fmake_hash_table), hash);
    }
  Fputhash (dependency, Qt, Fgethash (key, hash, Qnil));
  return Qt;
}

DEFUN ("igc--remove-extra-dependency", Figc__remove_extra_dependency,
       Sigc__remove_extra_dependency, 3, 3, 0,
       doc: /* Remove DEPENDENCY associated with KEY from object OBJ.
KEY is the key associated with DEPENDENCY in a hash table.  */)
  (Lisp_Object obj, Lisp_Object dependency, Lisp_Object key)
{
  mps_word_t word = XLI (obj);
  mps_word_t tag = word & IGC_TAG_MASK;
  mps_addr_t client = NULL;
  switch (tag)
    {
    case Lisp_Type_Unused0:
      emacs_abort ();

    case Lisp_Int0:
    case Lisp_Int1:
      return Qnil;

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

  struct igc_header *h = client;
  struct igc_exthdr *exthdr = igc_external_header (h);
  Lisp_Object hash = exthdr->extra_dependency;
  if (!WEAK_HASH_TABLE_P (hash))
    hash = exthdr->extra_dependency =
      CALLN (Fmake_hash_table, QCweakness, Qkey);

  /* This might throw.  */
  Fremhash (dependency, Fgethash (key, hash, Qnil));

  return Qt;
}

/***********************************************************************
				  Init
 ***********************************************************************/

void
init_igc (void)
{
  /* Returns previous handler.  */
  (void) mps_lib_assert_fail_install (igc_assert_fail);
  global_igc = make_igc ();
  add_main_thread ();
  set_state (IGC_STATE_USABLE_PARKED);
}

void
syms_of_igc (void)
{
  defsubr (&Sigc_info);
  defsubr (&Sigc__roots);
  defsubr (&Sigc__collect);
  defsubr (&Sigc__set_commit_limit);
  defsubr (&Sigc__add_extra_dependency);
  defsubr (&Sigc__remove_extra_dependency);
  DEFSYM (Qambig, "ambig");
  DEFSYM (Qexact, "exact");
  Fprovide (intern_c_string ("mps"), Qnil);

  DEFVAR_LISP ("igc-step-interval", Vigc_step_interval,
    doc: /* How much time is MPS allowed to spend in GC when Emacs is idle.
The value is in seconds, and should be a non-negative number.  It can
be either an integer or a float.  The default value is 0 which means .
don't do something when idle.  Negative values and values that are not numbers
are handled as if they were the default value.  */);
  Vigc_step_interval = make_fixnum (0);

  DEFVAR_BOOL ("igc--balance-intervals", igc__balance_intervals,
     doc: /* Whether to balance buffer intervals when idle.  */);
  igc__balance_intervals = false;
}
