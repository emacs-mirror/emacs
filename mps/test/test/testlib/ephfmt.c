/* $Id$
ephfmt.c
   A format for the aeph and amc pools.
*/

#include "ephfmt.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpscawl.h"
#include <string.h>

static size_t aligned(size_t n)
{
  size_t alignment = sizeof(void*);
  return (n + alignment - 1) & ~(alignment - 1);
}

oop make_pair(mps_ap_t ap, oop car, oop cdr)
{
  oop obj;
  mps_addr_t addr;
  size_t size = aligned(sizeof(struct pair));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK)
      error("out of memory in make_pair");
    obj = addr;
    obj->pair.header.type = TYPE_PAIR;
    obj->pair.car = car;
    obj->pair.cdr = cdr;
  } while (!mps_commit(ap, addr, size));
  return obj;
}

static oop make_weak_pair_2(mps_ap_t ap,
                            oop key,
                            oop value,
                            enum type type)
{
  oop obj;
  mps_addr_t addr;
  size_t size = aligned(sizeof(struct weak_pair));
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK)
      error("out of memory in make_weak_pair");
    obj = addr;
    obj->weak_pair.header.type = type;
    obj->weak_pair.key = key;
    obj->weak_pair.value = value;
  } while (!mps_commit(ap, addr, size));
  return obj;
}

oop make_weak_pair(mps_ap_t ap, oop key, oop value)
{
  return make_weak_pair_2(ap, key, value, TYPE_WEAK_PAIR);
}

oop make_weak_or_pair(mps_ap_t ap, oop key, oop value)
{
  return make_weak_pair_2(ap, key, value, TYPE_WEAK_OR_PAIR);
}

oop make_weak_and_pair(mps_ap_t ap, oop key, oop value)
{
  return make_weak_pair_2(ap, key, value, TYPE_WEAK_AND_PAIR);
}

oop make_string(mps_ap_t ap, size_t length, const char string[])
{
  oop obj;
  mps_addr_t addr;
  size_t nbytes = offsetof(struct string, data) + length + 1;
  size_t size = aligned(nbytes);
  do {
    mps_res_t res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK)
      error("out of memory in make_string");
    obj = addr;
    obj->string.header.type = TYPE_STRING;
    obj->string.length = length;
    if (string)
      memcpy(obj->string.data, string, length + 1);
    else
      memset(obj->string.data, 0, length + 1);
  } while (!mps_commit(ap, addr, size));
  return obj;
}

oop string_from_cstr(mps_ap_t ap, const char* cstr)
{
  return make_string(ap, strlen(cstr), cstr);
}

static mps_res_t eph_scan(mps_ss_t ss,
                          mps_addr_t base,
                          mps_addr_t limit)
{
#define FIX(ref)                                                       \
  do {                                                                 \
    mps_addr_t _addr = (ref); /* copy to local to avoid type pun */    \
    mps_res_t res = MPS_FIX12(ss, &_addr);                             \
    if (res != MPS_RES_OK)                                             \
      return res;                                                      \
    (ref) = _addr;                                                     \
  } while (0)

  MPS_SCAN_BEGIN(ss)
  {
    while (base < limit) {
      oop obj = base;
      switch (obj->header.type) {
        case TYPE_PAIR:
          FIX(obj->pair.car);
          FIX(obj->pair.cdr);
          base = (char*)base + aligned(sizeof(struct pair));
          break;
        case TYPE_WEAK_PAIR: {
          mps_res_t res;
          mps_addr_t* key_ref = (void**)&obj->weak_pair.key;
          mps_addr_t* value_ref = (void**)&obj->weak_pair.value;
          MPS_FIX_CALL(
            ss,
            res = mps_fix_weak_pair(ss, base, key_ref, value_ref));
          if (res != MPS_RES_OK)
            return res;
          base = (char*)base + aligned(sizeof(struct weak_pair));
        } break;
        case TYPE_WEAK_OR_PAIR: {
          mps_res_t res;
          mps_addr_t* key_ref = (void**)&obj->weak_pair.key;
          mps_addr_t* value_ref = (void**)&obj->weak_pair.value;
          MPS_FIX_CALL(
            ss,
            res = mps_fix_weak_or_pair(ss, base, key_ref, value_ref));
          if (res != MPS_RES_OK)
            return res;
          base = (char*)base + aligned(sizeof(struct weak_pair));
        } break;
        case TYPE_WEAK_AND_PAIR: {
          mps_res_t res;
          mps_addr_t* key_ref = (void**)&obj->weak_pair.key;
          mps_addr_t* value_ref = (void**)&obj->weak_pair.value;
          MPS_FIX_CALL(
            ss,
            res = mps_fix_weak_and_pair(ss, base, key_ref, value_ref));
          if (res != MPS_RES_OK)
            return res;
          base = (char*)base + aligned(sizeof(struct weak_pair));
        } break;
        case TYPE_STRING:
          base = (char*)base + aligned(offsetof(struct string, data) +
                                       obj->string.length + 1);
          break;
        case TYPE_FWD:
          base = (char*)base + aligned(obj->fwd.size);
          break;
        case TYPE_PAD:
          base = (char*)base + aligned(obj->pad.size);
          break;
        default:
          error("Unexpected object on the heap\n");
          return MPS_RES_FAIL;
      }
    }
  }
  MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

static mps_addr_t eph_skip(mps_addr_t base)
{
  oop obj = base;
  switch (obj->header.type) {
    case TYPE_PAIR:
      base = (char*)base + aligned(sizeof(struct pair));
      break;
    case TYPE_WEAK_PAIR:
    case TYPE_WEAK_OR_PAIR:
    case TYPE_WEAK_AND_PAIR:
      base = (char*)base + aligned(sizeof(struct weak_pair));
      break;
    case TYPE_STRING:
      base = (char*)base + aligned(offsetof(struct string, data) +
                                   obj->string.length + 1);
      break;
    case TYPE_FWD:
      base = (char*)base + aligned(obj->fwd.size);
      break;
    case TYPE_PAD:
      base = (char*)base + aligned(obj->pad.size);
      break;
    default:
      error("Unexpected object on the heap\n");
      return NULL;
  }
  return base;
}

static mps_addr_t eph_isfwd(mps_addr_t addr)
{
  oop obj = addr;
  switch (obj->header.type) {
    case TYPE_FWD:
      return obj->fwd.fwd;
    default:
      return NULL;
  }
}

static void eph_fwd(mps_addr_t old, mps_addr_t new)
{
  oop obj = old;
  mps_addr_t limit = eph_skip(old);
  size_t size = (size_t)((char*)limit - (char*)old);
  asserts(size >= aligned(sizeof(struct fwd)),
          "size >= sizeof(struct fwd)");
  obj->fwd.header.type = TYPE_FWD;
  obj->fwd.fwd = new;
  obj->fwd.size = size;
}

static void eph_pad(mps_addr_t addr, size_t size)
{
  oop obj = addr;
  asserts(size >= aligned(sizeof(struct pad)),
          "size >= sizeof(struct pad)");
  obj->pad.header.type = TYPE_PAD;
  obj->pad.size = size;
}

mps_fmt_t eph_fmt(mps_arena_t arena)
{
  mps_res_t res;
  mps_fmt_t fmt;
  MPS_ARGS_BEGIN(args)
  {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, sizeof(void*));
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, eph_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, eph_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, eph_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, eph_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, eph_pad);
    res = mps_fmt_create_k(&fmt, arena, args);
  }
  MPS_ARGS_END(args);
  if (res != MPS_RES_OK)
    error("Couldn't create obj format");
  return fmt;
}

void check_string(oop s, const char* data)
{
  asserts(s != NULL, "s != NULL");
  asserts(s->header.type == TYPE_STRING,
          "s->header.type = TYPE_STRING");
  asserts(s->string.length == strlen(data),
          "s->string.length == strlen(data");
  asserts(strncmp(s->string.data, data, s->string.length) == 0,
          "strncmp (s->string.data, data, s->string.length) == 0");
}

static void setup(void* stack_bottom, eph_test_fun fun, void* closure)
{
  mps_chain_t chain;
  mps_fmt_t fmt;
  mps_pool_t amc_pool;
  mps_pool_t eph_pool;
  mps_thr_t thread;
  mps_root_t exact_root;
  static struct memory_manager mm;

  static mps_gen_param_s gen_params[] = {
    { 150, 0.85 },
  };

  die(
    mps_arena_create_k(&mm.arena, mps_arena_class_vm(), mps_args_none),
    "mps_arena_create_k");

  die(mps_chain_create(&chain,
                       mm.arena,
                       sizeof(gen_params) / sizeof(*gen_params),
                       gen_params),
      "msp_chain_create");

  fmt = eph_fmt(mm.arena);

  MPS_ARGS_BEGIN(args)
  {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&amc_pool, mm.arena, mps_class_amc(), args),
        "mps_pool_create_k");
  }
  MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args)
  {
    MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&eph_pool, mm.arena, mps_class_aeph(), args),
        "mps_pool_create_k");
  }
  MPS_ARGS_END(args);

  die(mps_ap_create_k(&mm.amc_ap, amc_pool, mps_args_none),
      "mps_ap_create_k");

  MPS_ARGS_BEGIN(args)
  {
    MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_ephemeron());
    die(mps_ap_create_k(&mm.eph_ap, eph_pool, args), "mps_ap_create_k");
  }
  MPS_ARGS_END(args);

  die(mps_thread_reg(&thread, mm.arena), "mps_thread_reg");

  {
    mps_addr_t* p = (void*)&mm.roots;
    die(mps_root_create_area(&exact_root,
                             mm.arena,
                             mps_rank_exact(),
                             0,
                             p,
                             p + ROOT_COUNT,
                             mps_scan_area,
                             NULL),
        "mps_root_create_table");
  }

  fun(&mm, closure);

  mps_arena_park(mm.arena);
  mps_root_destroy(exact_root);
  mps_thread_dereg(thread);
  mps_ap_destroy(mm.amc_ap);
  mps_ap_destroy(mm.eph_ap);
  mps_pool_destroy(amc_pool);
  mps_pool_destroy(eph_pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(fmt);
  mps_arena_destroy(mm.arena);
}

static struct
{
  eph_test_fun fun;
  void* closure;
} test_fun;

static void setup0(void* stack_bottom)
{
  setup(stack_bottom, test_fun.fun, test_fun.closure);
}

void run_eph_test(eph_test_fun fun, void* closure)
{
  test_fun.fun = fun;
  test_fun.closure = closure;
  run_test(setup0);
}
