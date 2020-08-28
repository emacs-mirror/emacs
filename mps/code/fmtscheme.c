/* fmtscheme.c: SCHEME OBJECT FORMAT IMPLEMENTATION
 *
 *  $Id$
 *  Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include <string.h>

#include "fmtscheme.h"
#include "testlib.h"


/* special objects */

static obj_t obj_true;          /* #t, boolean true */
static obj_t obj_false;         /* #f, boolean false */


/* MPS globals */

mps_arena_t scheme_arena; /* the arena */
mps_pool_t obj_pool;     /* pool for ordinary Scheme objects */
mps_ap_t obj_ap;         /* allocation point used to allocate objects */


/* make_* -- object constructors */

#define ALIGNMENT sizeof(mps_word_t)

/* Align size upwards to the next multiple of the word size. */
#define ALIGN_WORD(size) \
  (((size) + ALIGNMENT - 1) & ~(ALIGNMENT - 1))

/* Align size upwards to the next multiple of the word size, and
 * additionally ensure that it's big enough to store a forwarding
 * pointer. Evaluates its argument twice. */
#define ALIGN_OBJ(size)                                \
  (ALIGN_WORD(size) >= ALIGN_WORD(sizeof(fwd_s))       \
   ? ALIGN_WORD(size)                                  \
   : ALIGN_WORD(sizeof(fwd_s)))

obj_t scheme_make_bool(int condition)
{
  return condition ? obj_true : obj_false;
}

obj_t scheme_make_pair(mps_ap_t ap, obj_t car, obj_t cdr)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(sizeof(pair_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_pair");
    obj = addr;
    obj->pair.type = TYPE_PAIR;
    CAR(obj) = car;
    CDR(obj) = cdr;
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_integer(mps_ap_t ap, long integer)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(sizeof(integer_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_integer");
    obj = addr;
    obj->integer.type = TYPE_INTEGER;
    obj->integer.integer = integer;
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_symbol(mps_ap_t ap, size_t length, char string[])
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(offsetof(symbol_s, string) + length+1);
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_symbol");
    obj = addr;
    obj->symbol.type = TYPE_SYMBOL;
    obj->symbol.length = length;
    memcpy(obj->symbol.string, string, length+1);
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_string(mps_ap_t ap, size_t length, char string[])
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(offsetof(string_s, string) + length+1);
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_string");
    obj = addr;
    obj->string.type = TYPE_STRING;
    obj->string.length = length;
    if (string) memcpy(obj->string.string, string, length+1);
    else memset(obj->string.string, 0, length+1);
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_special(mps_ap_t ap, char *string)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(sizeof(special_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_special");
    obj = addr;
    obj->special.type = TYPE_SPECIAL;
    obj->special.name = string;
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_operator(mps_ap_t ap, char *name,
                           entry_t entry, obj_t arguments,
                           obj_t body, obj_t env, obj_t op_env)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(sizeof(operator_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_operator");
    obj = addr;
    obj->operator.type = TYPE_OPERATOR;
    obj->operator.name = name;
    obj->operator.entry = entry;
    obj->operator.arguments = arguments;
    obj->operator.body = body;
    obj->operator.env = env;
    obj->operator.op_env = op_env;
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_port(mps_ap_t ap, obj_t name, FILE *stream)
{
  mps_addr_t port_ref;
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(sizeof(port_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_port");
    obj = addr;
    obj->port.type = TYPE_PORT;
    obj->port.name = name;
    obj->port.stream = stream;
  } while(!mps_commit(ap, addr, size));
  port_ref = obj;
  mps_finalize(scheme_arena, &port_ref);
  return obj;
}

obj_t scheme_make_character(mps_ap_t ap, char c)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(sizeof(character_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_character");
    obj = addr;
    obj->character.type = TYPE_CHARACTER;
    obj->character.c = c;
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_vector(mps_ap_t ap, size_t length, obj_t fill)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(offsetof(vector_s, vector) + length * sizeof(obj_t));
  do {
    size_t i;
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_vector");
    obj = addr;
    obj->vector.type = TYPE_VECTOR;
    obj->vector.length = length;
    for(i = 0; i < length; ++i)
      obj->vector.vector[i] = fill;
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_buckets(mps_ap_t ap, size_t length)
{
  obj_t obj;
  mps_addr_t addr;
  size_t size = ALIGN_OBJ(offsetof(buckets_s, bucket) + length * sizeof(obj->buckets.bucket[0]));
  do {
    size_t i;
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_buckets");
    obj = addr;
    obj->buckets.type = TYPE_BUCKETS;
    obj->buckets.length = length;
    obj->buckets.used = 0;
    obj->buckets.deleted = 0;
    for(i = 0; i < length; ++i) {
      obj->buckets.bucket[i].key = NULL;
      obj->buckets.bucket[i].value = NULL;
    }
  } while(!mps_commit(ap, addr, size));
  return obj;
}

obj_t scheme_make_table(mps_ap_t ap, size_t length, hash_t hashf, cmp_t cmpf)
{
  obj_t obj;
  mps_addr_t addr;
  size_t l, size = ALIGN_OBJ(sizeof(table_s));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK) error("out of memory in make_table");
    obj = addr;
    obj->table.type = TYPE_TABLE;
    obj->table.buckets = NULL;
  } while(!mps_commit(ap, addr, size));
  obj->table.hash = hashf;
  obj->table.cmp = cmpf;
  /* round up to next power of 2 */
  for(l = 1; l < length; l *= 2);
  obj->table.buckets = scheme_make_buckets(ap, l);
  mps_ld_reset(&obj->table.ld, scheme_arena);
  return obj;
}


/* MPS Format */

static mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
#define FIX(ref) \
  do { \
    mps_addr_t _addr = (ref); /* copy to local to avoid type pun */ \
    mps_res_t res = MPS_FIX12(ss, &_addr); \
    if (res != MPS_RES_OK) return res; \
    (ref) = _addr; \
  } while(0)

  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      obj_t obj = base;
      switch (TYPE(obj)) {
      case TYPE_PAIR:
      case TYPE_PROMISE:
        FIX(CAR(obj));
        FIX(CDR(obj));
        base = (char *)base + ALIGN_OBJ(sizeof(pair_s));
        break;
      case TYPE_INTEGER:
        base = (char *)base + ALIGN_OBJ(sizeof(integer_s));
        break;
      case TYPE_SYMBOL:
        base = (char *)base +
               ALIGN_OBJ(offsetof(symbol_s, string) + obj->symbol.length + 1);
        break;
      case TYPE_SPECIAL:
        base = (char *)base + ALIGN_OBJ(sizeof(special_s));
        break;
      case TYPE_OPERATOR:
        FIX(obj->operator.arguments);
        FIX(obj->operator.body);
        FIX(obj->operator.env);
        FIX(obj->operator.op_env);
        base = (char *)base + ALIGN_OBJ(sizeof(operator_s));
        break;
      case TYPE_STRING:
        base = (char *)base +
               ALIGN_OBJ(offsetof(string_s, string) + obj->string.length + 1);
        break;
      case TYPE_PORT:
        FIX(obj->port.name);
        base = (char *)base + ALIGN_OBJ(sizeof(port_s));
        break;
      case TYPE_CHARACTER:
        base = (char *)base + ALIGN_OBJ(sizeof(character_s));
        break;
      case TYPE_VECTOR:
        {
          size_t i;
          for (i = 0; i < obj->vector.length; ++i)
            FIX(obj->vector.vector[i]);
        }
        base = (char *)base +
          ALIGN_OBJ(offsetof(vector_s, vector) +
                    obj->vector.length * sizeof(obj->vector.vector[0]));
        break;
      case TYPE_BUCKETS:
        {
          size_t i;
          for (i = 0; i < obj->buckets.length; ++i) {
            FIX(obj->buckets.bucket[i].key);
            FIX(obj->buckets.bucket[i].value);
          }
        }
        base = (char *)base +
          ALIGN_OBJ(offsetof(buckets_s, bucket) +
                    obj->buckets.length * sizeof(obj->buckets.bucket[0]));
        break;
      case TYPE_TABLE:
        FIX(obj->table.buckets);
        base = (char *)base + ALIGN_OBJ(sizeof(table_s));
        break;
      case TYPE_FWD2:
        base = (char *)base + ALIGN_WORD(sizeof(fwd2_s));
        break;
      case TYPE_FWD:
        base = (char *)base + ALIGN_WORD(obj->fwd.size);
        break;
      case TYPE_PAD1:
        base = (char *)base + ALIGN_WORD(sizeof(pad1_s));
        break;
      case TYPE_PAD:
        base = (char *)base + ALIGN_WORD(obj->pad.size);
        break;
      default:
        error("Unexpected object on the heap\n");
        return MPS_RES_FAIL;
      }
    }
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

static mps_addr_t obj_skip(mps_addr_t base)
{
  obj_t obj = base;
  switch (TYPE(obj)) {
  case TYPE_PAIR:
  case TYPE_PROMISE:
    base = (char *)base + ALIGN_OBJ(sizeof(pair_s));
    break;
  case TYPE_INTEGER:
    base = (char *)base + ALIGN_OBJ(sizeof(integer_s));
    break;
  case TYPE_SYMBOL:
    base = (char *)base +
      ALIGN_OBJ(offsetof(symbol_s, string) + obj->symbol.length + 1);
    break;
  case TYPE_SPECIAL:
    base = (char *)base + ALIGN_OBJ(sizeof(special_s));
    break;
  case TYPE_OPERATOR:
    base = (char *)base + ALIGN_OBJ(sizeof(operator_s));
    break;
  case TYPE_STRING:
    base = (char *)base +
      ALIGN_OBJ(offsetof(string_s, string) + obj->string.length + 1);
    break;
  case TYPE_PORT:
    base = (char *)base + ALIGN_OBJ(sizeof(port_s));
    break;
  case TYPE_CHARACTER:
    base = (char *)base + ALIGN_OBJ(sizeof(character_s));
    break;
  case TYPE_VECTOR:
    base = (char *)base +
      ALIGN_OBJ(offsetof(vector_s, vector) +
                obj->vector.length * sizeof(obj->vector.vector[0]));
    break;
  case TYPE_BUCKETS:
    base = (char *)base +
      ALIGN_OBJ(offsetof(buckets_s, bucket) +
                obj->buckets.length * sizeof(obj->buckets.bucket[0]));
    break;
  case TYPE_TABLE:
    base = (char *)base + ALIGN_OBJ(sizeof(table_s));
    break;
  case TYPE_FWD2:
    base = (char *)base + ALIGN_WORD(sizeof(fwd2_s));
    break;
  case TYPE_FWD:
    base = (char *)base + ALIGN_WORD(obj->fwd.size);
    break;
  case TYPE_PAD:
    base = (char *)base + ALIGN_WORD(obj->pad.size);
    break;
  case TYPE_PAD1:
    base = (char *)base + ALIGN_WORD(sizeof(pad1_s));
    break;
  default:
    error("Unexpected object on the heap\n");
    return NULL;
  }
  return base;
}

static mps_addr_t obj_isfwd(mps_addr_t addr)
{
  obj_t obj = addr;
  switch (TYPE(obj)) {
  case TYPE_FWD2:
    return obj->fwd2.fwd;
  case TYPE_FWD:
    return obj->fwd.fwd;
  default:
    return NULL;
  }
}

static void obj_fwd(mps_addr_t old, mps_addr_t new)
{
  obj_t obj = old;
  mps_addr_t limit = obj_skip(old);
  size_t size = (size_t)((char *)limit - (char *)old);
  cdie(size >= ALIGN_WORD(sizeof(fwd2_s)), "bad size in obj_fwd");
  if (size == ALIGN_WORD(sizeof(fwd2_s))) {
    TYPE(obj) = TYPE_FWD2;
    obj->fwd2.fwd = new;
  } else {
    TYPE(obj) = TYPE_FWD;
    obj->fwd.fwd = new;
    obj->fwd.size = size;
  }
}

static void obj_pad(mps_addr_t addr, size_t size)
{
  obj_t obj = addr;
  cdie(size >= ALIGN_WORD(sizeof(pad1_s)), "bad size in obj_pad");
  if (size == ALIGN_WORD(sizeof(pad1_s))) {
    TYPE(obj) = TYPE_PAD1;
  } else {
    TYPE(obj) = TYPE_PAD;
    obj->pad.size = size;
  }
}

void scheme_fmt(mps_fmt_t *fmt)
{
  mps_res_t res;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, ALIGNMENT);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, obj_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
    res = mps_fmt_create_k(fmt, scheme_arena, args);
  } MPS_ARGS_END(args);
  if (res != MPS_RES_OK) error("Couldn't create obj format");
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
