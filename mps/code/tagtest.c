/* tagtest.c: TAGGED POINTER TEST
 *
 * $Id$
 * Copyright (c) 2015 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: This test case checks that the MPS correctly handles
 * tagged pointers via the object format and via stack and register
 * scanning.
 */

#include <stdio.h> /* printf */

#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "testlib.h"

#define OBJCOUNT (1000)         /* Number of conses to allocate */

typedef struct cons_s {
  mps_word_t car, cdr;
} cons_s, *cons_t;

typedef mps_word_t imm_t;       /* Immediate value. */
typedef mps_word_t fwd_t;       /* Fowarding pointer. */

static mps_word_t tag_cons;     /* Tag bits indicating pointer to cons */
static mps_word_t tag_fwd;      /* Tag bits indicating forwarding pointer */
static mps_word_t tag_imm;      /* Tag bits indicating immediate value */
static mps_word_t tag_invalid;  /* Invalid tag bits */

#define TAG_BITS (3)            /* Number of tag bits */
#define TAG_COUNT ((mps_word_t)1 << TAG_BITS) /* Number of distinct tags */
#define TAG_MASK (TAG_COUNT - 1) /* Tag mask */
#define TAG(word) ((mps_word_t)(word) & TAG_MASK)
#define TAGGED(value, type) (((mps_word_t)(value) & ~TAG_MASK) + tag_ ## type)
#define UNTAGGED(word, type) ((type ## _t)((mps_word_t)(word) & ~TAG_MASK))

static mps_word_t make_cons(mps_ap_t ap, mps_word_t car, mps_word_t cdr)
{
  cons_t obj;
  mps_addr_t addr;
  size_t size = sizeof(cons_s);
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in cons");
    obj = addr;
    obj->car = car;
    obj->cdr = cdr;
  } while (!mps_commit(ap, addr, size));
  return TAGGED(obj, cons);
}

static void fwd(mps_addr_t old, mps_addr_t new)
{
  cons_t cons;
  Insist(TAG(old) == tag_cons);
  cons = UNTAGGED(old, cons);
  cons->car = TAGGED(0, fwd);
  cons->cdr = (mps_word_t)new;
}

static mps_addr_t isfwd(mps_addr_t addr)
{
  cons_t cons;
  if (TAG(addr) != tag_cons)
    return NULL;
  cons = UNTAGGED(addr, cons);
  if (TAG(cons->car) != tag_fwd)
    return NULL;
  return (mps_addr_t)cons->cdr;
}

static void pad(mps_addr_t addr, size_t size)
{
  mps_word_t *word = addr;
  mps_word_t *limit = (mps_word_t *)((char *)addr + size);
  while (word < limit) {
    *word = TAGGED(0, imm);
    ++ word;
  }
}

static mps_res_t scan(mps_ss_t ss, mps_addr_t client_base,
                      mps_addr_t client_limit)
{
  mps_addr_t *base, *limit;
  Insist(TAG(client_base) == tag_cons);
  Insist(TAG(client_limit) == tag_cons);
  base = (mps_addr_t *)UNTAGGED(client_base, cons);
  limit = (mps_addr_t *)UNTAGGED(client_limit, cons);
  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      if (MPS_FIX1(ss, *base)) {
        mps_addr_t addr = *base;
        mps_word_t tag = TAG(addr);
        if (tag == tag_cons) {
          mps_res_t res = MPS_FIX2(ss, &addr);
          if (res != MPS_RES_OK)
            return res;
          *base = addr;
        }
      }
      ++base;
    }
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

static mps_addr_t skip(mps_addr_t addr)
{
  return (mps_addr_t)((char *)addr + sizeof(cons_s));
}


/* alloc_list -- Allocate a linked list with 'count' entries. The
 * attribute prevents the compiler from inlining this function into
 * test, which might leave untagged temporaries on the stack,
 * confusing the test logic.
 */

ATTRIBUTE_NOINLINE
static mps_word_t alloc_list(mps_arena_t arena, mps_ap_t ap, size_t count)
{
  mps_word_t nil = TAGGED(NULL, cons);
  mps_word_t p = nil;
  size_t i;
  for (i = 0; i < count; ++i) {
    mps_addr_t addr;
    p = make_cons(ap, TAGGED(i << TAG_BITS, imm), p);
    Insist(TAG(p) == tag_cons);
    addr = (mps_addr_t)p;
    die(mps_finalize(arena, &addr), "finalize");
  }
  return p;
}


/* test -- Run the test case in the specified mode. The attribute
 * prevents the compiler from inlining this function into main, which
 * might end up with stack slots like 'p' being above 'marker' and so
 * not scanned.
 */

enum {
  MODE_DEFAULT,                 /* Use default scanner (tagged with 0). */
  MODE_CONS,                    /* Scan words tagged "cons". */
  MODE_INVALID,                 /* Scan words tagged "invalid". */
  MODE_LIMIT
};

ATTRIBUTE_NOINLINE
static void test(int mode, void *marker)
{
  mps_arena_t arena;
  mps_thr_t thread;
  mps_root_t root;
  mps_fmt_t fmt;
  mps_pool_t pool;
  mps_ap_t ap;
  mps_word_t p;
  size_t i;
  size_t finalized = 0;
  size_t expected_finalized = 0;

  die(mps_arena_create(&arena, mps_arena_class_vm(), mps_args_none), "arena");
  mps_message_type_enable(arena, mps_message_type_finalization());
  die(mps_thread_reg(&thread, arena), "thread");

  switch (mode) {
  case MODE_DEFAULT:
    /* Default stack scanner only recognizes words tagged with 0. */
    die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                            mps_stack_scan_ambig, marker, 0), "root");
    expected_finalized = (tag_cons != 0) * OBJCOUNT;
    break;
  case MODE_CONS:
    /* Scan words tagged "cons" -- everything will live. */
    die(mps_root_create_reg_masked(&root, arena, mps_rank_ambig(), 0, thread,
                                   TAG_MASK, tag_cons, marker), "root");
    expected_finalized = 0;
    break;
  case MODE_INVALID:
    /* Scan words tagged "invalid" -- everything will die. */
    die(mps_root_create_reg_masked(&root, arena, mps_rank_ambig(), 0, thread,
                                   TAG_MASK, tag_invalid, marker), "root");
    expected_finalized = OBJCOUNT;
    break;
  default:
    Insist(0);
    break;
  }

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_HEADER_SIZE, tag_cons);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, sizeof(cons_s));
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, pad);
    die(mps_fmt_create_k(&fmt, arena, args), "fmt");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&pool, arena, mps_class_amc(), args), "pool");
  } MPS_ARGS_END(args);

  die(mps_ap_create_k(&ap, pool, mps_args_none), "ap");

  p = alloc_list(arena, ap, OBJCOUNT);

  mps_arena_collect(arena);
  while (mps_message_poll(arena)) {
    mps_message_t message;
    mps_addr_t objaddr;
    cdie(mps_message_get(&message, arena, mps_message_type_finalization()),
         "message_get");
    mps_message_finalization_ref(&objaddr, arena, message);
    Insist(TAG(objaddr) == tag_cons);
    mps_message_discard(arena, message);
    ++ finalized;
  }

  Insist(finalized == expected_finalized);
  for (i = 0; i < OBJCOUNT - expected_finalized; ++i) {
    Insist(TAG(p) == tag_cons);
    p = UNTAGGED(p, cons)->cdr;
  }

  mps_arena_park(arena);
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(fmt);
  mps_root_destroy(root);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);
}

int main(int argc, char *argv[])
{
  void *marker = &marker;
  mps_word_t tags[TAG_COUNT];
  size_t i;
  int mode;

  testlib_init(argc, argv);

  /* Shuffle the tags. */
  for (i = 0; i < NELEMS(tags); ++i) {
    tags[i] = i;
  }
  for (i = 0; i < NELEMS(tags); ++i) {
    size_t j = i + rnd() % (NELEMS(tags) - i);
    mps_word_t t = tags[i];
    tags[i] = tags[j];
    tags[j] = t;
  }
  tag_cons = tags[0];
  tag_fwd = tags[1];
  tag_imm = tags[2];
  tag_invalid = tags[3];

  for (mode = 0; mode < MODE_LIMIT; ++mode) {
    test(mode, marker);
  }

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
