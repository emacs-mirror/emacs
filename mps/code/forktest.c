/* forktest.c: FORK SAFETY TEST
 *
 * $Id: //info.ravenbrook.com/project/mps/branch/2018-06-13/fork/code/tagtest.c#1 $
 * Copyright (c) 2018-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: This test case is a regression test for job004062. It
 * checks that the MPS correctly runs in the child process after a
 * fork() on FreeBSD, Linux or macOS.
 *
 * .format: This test case uses a trivial object format in which each
 * object contains a single reference.
 */

#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "testlib.h"

enum {
  TYPE_REF,
  TYPE_FWD,
  TYPE_PAD
};

typedef struct obj_s {
  unsigned type;                /* One of the TYPE_ enums */
  union {
    struct obj_s *ref;          /* TYPE_REF */
    mps_addr_t fwd;             /* TYPE_FWD */
    size_t pad;                 /* TYPE_PAD */
  } u;
} obj_s, *obj_t;

static void obj_fwd(mps_addr_t old, mps_addr_t new)
{
  obj_t obj = old;
  obj->type = TYPE_FWD;
  obj->u.fwd = new;
}

static mps_addr_t obj_isfwd(mps_addr_t addr)
{
  obj_t obj = addr;
  if (obj->type == TYPE_FWD) {
    return obj->u.fwd;
  } else {
    return NULL;
  }
}

static void obj_pad(mps_addr_t addr, size_t size)
{
  obj_t obj = addr;
  obj->type = TYPE_PAD;
  obj->u.pad = size;
}

static mps_addr_t obj_skip(mps_addr_t addr)
{
  obj_t obj = addr;
  size_t size;
  if (obj->type == TYPE_PAD) {
    size = obj->u.pad;
  } else {
    size = sizeof(obj_s);
  }
  return (char *)addr + size;
}

static mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      obj_t obj = base;
      if (obj->type == TYPE_REF) {
        mps_addr_t p = obj->u.ref;
        mps_res_t res = MPS_FIX12(ss, &p);
        if (res != MPS_RES_OK) {
          return res;
        }
        obj->u.ref = p;
      }
      base = obj_skip(base);
    }
  } MPS_SCAN_END(ss);
  return MPS_RES_OK;
}

int main(int argc, char *argv[])
{
  void *marker = &marker;
  int pid;
  mps_arena_t arena;
  mps_fmt_t obj_fmt;
  mps_pool_t pool;
  mps_thr_t thread;
  mps_root_t stack_root;
  mps_ap_t obj_ap;
  size_t i;
  obj_t obj, first;

  testlib_init(argc, argv);

  /* Set the pause time to be very small so that the incremental
     collector (when it runs) will have to leave a read barrier in
     place for us to hit. */
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_PAUSE_TIME, 0.0);
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "mps_arena_create");
  } MPS_ARGS_END(args);
  mps_arena_park(arena);

  die(mps_thread_reg(&thread, arena), "Couldn't register thread");
  die(mps_root_create_thread(&stack_root, arena, thread, marker),
      "Couldn't create thread root");

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, sizeof(obj_s));
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, obj_scan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
    die(mps_fmt_create_k(&obj_fmt, arena, args), "Couldn't create obj format");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    die(mps_pool_create_k(&pool, arena, mps_class_amc(), args),
        "Couldn't create pool");
  } MPS_ARGS_END(args);

  die(mps_ap_create_k(&obj_ap, pool, mps_args_none),
      "Couldn't create obj allocation point");

  /* Create a linked list of objects. */
  first = NULL;
  for (i = 0; i < 100000; ++i) {
    size_t size = sizeof(obj_s);
    mps_addr_t addr;
    do {
      die(mps_reserve(&addr, obj_ap, size), "Couldn't allocate.");
      obj = addr;
      obj->type = TYPE_REF;
      obj->u.ref = NULL;
    } while (!mps_commit(obj_ap, addr, size));
    obj->u.ref = first;
    first = obj;
  }

  pid = fork();
  cdie(pid >= 0, "fork failed");

  /* Allow a collection to start, which will cause a read barrier to
     be applied to any segment containing live objects that was
     scanned. */
  mps_arena_release(arena);

  /* Read all the objects, so that if there is read barrier in place
     we will hit it. */
  for (obj = first; obj != NULL; obj = obj->u.ref) {
    Insist(obj->type == TYPE_REF);
  }

  mps_arena_park(arena);

  if (pid != 0) {
    /* Parent: wait for child and check that its exit status is zero. */
    int stat;
    cdie(pid == waitpid(pid, &stat, 0), "waitpid failed");
    cdie(WIFEXITED(stat), "child did not exit normally");
    cdie(WEXITSTATUS(stat) == 0, "child exited with nonzero status");
    printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  }

  mps_ap_destroy(obj_ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(obj_fmt);
  mps_root_destroy(stack_root);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2018-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
