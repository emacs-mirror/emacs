/* tagtest.c: TAGGED POINTER TEST
 *
 * $Id$
 * Copyright (c) 2015 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: This test case checks that the MPS correctly handles
 * tagged pointers via the object format and tagged area scanning.
 */

#include <stdio.h>              /* printf */

#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "testlib.h"

#define OBJCOUNT 1000           /* Number of conses to allocate */

typedef struct cons_s {
  mps_word_t car, cdr;
} cons_s, *cons_t;

typedef mps_word_t imm_t;       /* Immediate value. */
typedef mps_word_t fwd_t;       /* Fowarding pointer. */

static mps_word_t tag_bits;     /* Number of tag bits */
static mps_word_t tag_cons;     /* Tag bits indicating pointer to cons */
static mps_word_t tag_fwd;      /* Tag bits indicating forwarding pointer */
static mps_word_t tag_imm;      /* Tag bits indicating immediate value */
static mps_word_t tag_invalid;  /* Invalid tag bits */
static mps_addr_t refs[OBJCOUNT]; /* Tagged references to objects */

#define TAG_COUNT ((mps_word_t)1 << tag_bits) /* Number of distinct tags */
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
  cons_t cons = old;
  cons->car = TAGGED(0, fwd);
  cons->cdr = (mps_word_t)new;
}

static mps_addr_t isfwd(mps_addr_t addr)
{
  cons_t cons = addr;
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

static mps_res_t scan(mps_ss_t ss, mps_addr_t base,
                      mps_addr_t limit)
{
  MPS_SCAN_BEGIN(ss) {
    mps_word_t *p = base;
    while (p < (mps_word_t *)limit) {
      mps_word_t word = *p;
      mps_word_t tag = TAG(word);
      if (tag == tag_cons) {
        mps_addr_t ref = UNTAGGED(word, cons);
        if (MPS_FIX1(ss, ref)) {
          mps_res_t res = MPS_FIX2(ss, &ref);
          if (res != MPS_RES_OK)
            return res;
          *p = TAGGED(ref, cons);
        }
      }
      ++p;
    }
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

static mps_addr_t skip(mps_addr_t addr)
{
  return (mps_addr_t)((char *)addr + sizeof(cons_s));
}


static void collect(mps_arena_t arena, size_t expected)
{
  size_t finalized = 0;
  mps_arena_collect(arena);
  while (mps_message_poll(arena)) {
    mps_message_t message;
    mps_addr_t objaddr;
    cdie(mps_message_get(&message, arena, mps_message_type_finalization()),
         "message_get");
    mps_message_finalization_ref(&objaddr, arena, message);
    Insist(TAG(objaddr) == 0);
    mps_message_discard(arena, message);
    ++ finalized;
  }
  printf("finalized=%lu expected=%lu\n",
         (unsigned long)finalized, (unsigned long)expected);
  Insist(finalized == expected);
}


/* test -- Run the test case in the specified mode. */

#define MODES(R, X) \
  R(X, CONS,    "Scan words tagged \"cons\".") \
  R(X, INVALID, "Scan words tagged \"invalid\".")

#define MODES_ENUM(X, id, comment) MODE_ ## id,

enum {
  MODES(MODES_ENUM, X)
  MODE_LIMIT
};

#define MODES_NAME(X, id, comment) #id,

static const char *mode_name[] = {
  MODES(MODES_NAME, X)
};


static void test(int mode)
{
  mps_arena_t arena;
  mps_thr_t thread;
  mps_root_t root;
  mps_fmt_t fmt;
  mps_pool_t pool;
  mps_ap_t ap;
  size_t expected = 0;
  size_t i;

  printf("test(%s)\n", mode_name[mode]);

  die(mps_arena_create(&arena, mps_arena_class_vm(), mps_args_none), "arena");
  mps_message_type_enable(arena, mps_message_type_finalization());
  die(mps_thread_reg(&thread, arena), "thread");

  switch (mode) {
  default:
    Insist(0);
    /* fall through */
  case MODE_CONS:
    /* Scan words tagged "cons" -- everything will live. */
    die(mps_root_create_area_tagged(&root, arena, mps_rank_ambig(), 0,
				    (void *)refs, (void *)&refs[OBJCOUNT],
				    mps_scan_area_tagged, TAG_MASK, tag_cons),
        "root");
    expected = 0;
    break;
  case MODE_INVALID:
    /* Scan words tagged "invalid" -- everything will die. */
    die(mps_root_create_area_tagged(&root, arena, mps_rank_ambig(), 0,
				    (void *)refs, (void *)&refs[OBJCOUNT],
				    mps_scan_area_tagged, TAG_MASK, tag_invalid),
        "root");
    expected = OBJCOUNT;
    break;
  }

  MPS_ARGS_BEGIN(args) {
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

  for (i = 0; i < OBJCOUNT; ++i) {
    mps_word_t p, r;
    mps_word_t q = TAGGED(i << tag_bits, imm);
    mps_addr_t addr;
    p = make_cons(ap, q, q);
    Insist(TAG(p) == tag_cons);
    r = TAGGED(p, imm);
    UNTAGGED(p, cons)->cdr = r;
    refs[i] = (mps_addr_t)p;
    addr = (mps_addr_t)UNTAGGED(p, cons);
    die(mps_finalize(arena, &addr), "finalize");
  }

  collect(arena, expected);

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
  mps_word_t tags[sizeof(mps_word_t)];
  size_t i;
  int mode;

  testlib_init(argc, argv);

  /* Work out how many tags to use. */
  tag_bits = SizeLog2(sizeof(mps_word_t));
  Insist(TAG_COUNT <= NELEMS(tags));

  /* Shuffle the tags. */
  for (i = 0; i < TAG_COUNT; ++i) {
    tags[i] = i;
  }
  for (i = 0; i < TAG_COUNT; ++i) {
    size_t j = i + rnd() % (TAG_COUNT - i);
    mps_word_t t = tags[i];
    tags[i] = tags[j];
    tags[j] = t;
  }
  tag_cons = tags[0];
  tag_fwd = tags[1];
  tag_imm = tags[2];
  tag_invalid = tags[3];

  printf("tags: cons = %u, fwd = %u, imm = %u, invalid = %u\n",
         (unsigned)tag_cons, (unsigned)tag_fwd,
         (unsigned)tag_imm, (unsigned)tag_invalid);

  for (mode = 0; mode < MODE_LIMIT; ++mode) {
    test(mode);
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
