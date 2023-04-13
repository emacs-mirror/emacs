/* extcon.c: ARENA EXTENDED AND CONTRACTED CALLBACK TEST
 *
 * $Id$
 * Copyright (c) 2014-2022 Ravenbrook Limited.  See end of file for license.
 *
 * .overview: This test case allocates a bunch of large objects, of a size 
 * similar to the size of the arena, to force the arena to extend. It then
 * discards the base pointers to those objects, and forces a collection.
 *
 * .limitations: This test checks only that the EXTENDED and CONTRACTED 
 * callbacks were called at least once. It does not check that the 
 * extensions and contractions themselves were performed correctly, nor
 * does it check that an appropriate number of extensions and contractions
 * took place, nor does it check that they took place at sensible times.
 */

#include "mps.h"
#include "testlib.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/* Number of test objects to allocate */
#define N_TESTOBJ 10
/* Number of integers in each test object */
#define N_INT_TESTOBJ 10000
/* This is the difference in size between */
#define SIZEDIFF 10

/* Set alignment to mps_word_ts */
#define ALIGNMENT sizeof(mps_word_t)

/* Align size upwards to the next multiple of the word size. */
#define ALIGN_WORD(size) \
  (((size) + ALIGNMENT - 1) & ~(ALIGNMENT - 1))

/* Object TYPE macro */
#define TYPE(obj) ((obj)->type.type)

/* Global objects*/
static mps_arena_t arena;       /* the arena */
static mps_pool_t obj_pool;     /* pool for ordinary objects */
static mps_ap_t obj_ap;         /* allocation point used to allocate objects */

/* Count of number of arena contractions and extensions */
static int n_contract = 0;
static int n_extend = 0;

/* Union of all object types */
typedef int type_t;
enum {
	TYPE_INTBOX,
	TYPE_FWD2,            /* two-word forwarding object */
	TYPE_FWD,             /* three words and up forwarding object */
	TYPE_PAD1,            /* one-word padding object */
	TYPE_PAD              /* two words and up padding object */
};

typedef struct type_s {
	type_t type;
} type_s;

typedef union obj_u *obj_t;

typedef struct fwd2_s {
	type_t type;                  /* TYPE_FWD2 */
	obj_t fwd;                    /* forwarded object */
} fwd2_s;

typedef struct fwd_s {
	type_t type;                  /* TYPE_FWD */
	obj_t fwd;                    /* forwarded object */
	size_t size;                  /* total size of this object */
} fwd_s;

/* Align size upwards to the next multiple of the word size, and
 * additionally ensure that it's big enough to store a forwarding
 * pointer. Evaluates its argument twice. */
#define ALIGN_OBJ(size)                                \
  (ALIGN_WORD(size) >= ALIGN_WORD(sizeof(fwd_s))       \
   ? ALIGN_WORD(size)                                  \
   : ALIGN_WORD(sizeof(fwd_s)))

typedef struct pad1_s {
	type_t type;                  /* TYPE_PAD1 */
} pad1_s;

typedef struct pad_s {
	type_t type;                  /* TYPE_PAD */
	size_t size;                  /* total size of this object */
} pad_s;


typedef struct test_alloc_obj
{
	type_t type;
	size_t size;
	int int_array[N_INT_TESTOBJ];
} test_alloc_obj_s;


typedef union obj_u {
	type_s type;
	test_alloc_obj_s int_box;
	fwd_s fwd;
	fwd2_s fwd2;
	pad1_s pad1;
	pad_s pad;
} obj_s;

/* Callback functions for arena extension and contraction */
void arena_extended_cb(mps_arena_t arena_in, mps_addr_t addr, size_t size)
{
  testlib_unused(arena_in);
  testlib_unused(addr);
  testlib_unused(size);
  n_extend++;
}

void arena_contracted_cb(mps_arena_t arena_in, mps_addr_t addr, size_t size)
{
  testlib_unused(arena_in);
  testlib_unused(addr);
  testlib_unused(size);
	n_contract++;
}


/* Format functions */

mps_addr_t obj_skip(mps_addr_t addr)
{
	obj_t obj = addr;

	switch (TYPE(obj))
	{
	case TYPE_INTBOX:
		addr = (char *)addr + ALIGN_OBJ(sizeof(test_alloc_obj_s));
		break;
	case TYPE_FWD2:
		addr = (char *)addr + ALIGN_WORD(sizeof(fwd2_s));
		break;
	case TYPE_FWD:
		addr = (char *)addr + ALIGN_WORD(obj->fwd.size);
		break;
	case TYPE_PAD1:
		addr = (char *)addr + ALIGN_WORD(sizeof(pad1_s));
		break;
	case TYPE_PAD:
		addr = (char *)addr + ALIGN_WORD(obj->pad.size);
		break;
	default:
		printf("invalid type");
		assert(0);
		break;
	}

	return addr;
}

void obj_pad(mps_addr_t addr, size_t size)
{

	obj_t obj = addr;

	assert(size >= ALIGN_WORD(sizeof(pad1_s)));
	if (size == ALIGN_WORD(sizeof(pad1_s))) {
		TYPE(obj) = TYPE_PAD1;
	} else {
		TYPE(obj) = TYPE_PAD;
		obj->pad.size = size;
	}
}

void obj_fwd(mps_addr_t old, mps_addr_t new)
{
	obj_t obj = old;
	mps_addr_t limit = obj_skip(old);
	size_t size = (size_t)((char*)limit - (char*)old);

	assert(size >= ALIGN_WORD(sizeof(fwd2_s)));
	if (size == ALIGN_WORD(sizeof(fwd2_s))) {
		TYPE(obj) = TYPE_FWD2;
		obj->fwd2.fwd = new;
	} else {
		TYPE(obj) = TYPE_FWD;
		obj->fwd.fwd = new;
		obj->fwd.size = size;
	}
}

mps_addr_t obj_isfwd(mps_addr_t addr)
{
	obj_t obj = addr;
	switch (TYPE(obj)) {
	case TYPE_FWD2:
		return obj->fwd2.fwd;
	case TYPE_FWD:
		return obj->fwd.fwd;
	}
	return NULL;
}

int main(int argc, char* argv[])
{
	mps_res_t res;
	mps_fmt_t obj_fmt;
	
	mps_thr_t thread;
	mps_root_t stack_root;
	size_t arena_size, obj_size;

	/* Cold end of stack pointer */
	void *cold = &cold;

	/* [FIXME: do we need a trampoline? is p a root?] */
	mps_addr_t p;
	
	int i;
	test_alloc_obj_s *testobj[N_TESTOBJ];

  testlib_init(argc, argv);

	/* Make initial arena size slightly bigger than the test object size to force and extension as early as possible */
	obj_size = ALIGN_OBJ(sizeof(test_alloc_obj_s));
	arena_size = ALIGN_OBJ(obj_size + SIZEDIFF);

	/* Create arena and register callbacks */
	MPS_ARGS_BEGIN(args) {
		MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, arena_size);
		MPS_ARGS_ADD(args, MPS_KEY_ARENA_EXTENDED, (mps_fun_t)&arena_extended_cb);
		MPS_ARGS_ADD(args, MPS_KEY_ARENA_CONTRACTED, (mps_fun_t)&arena_contracted_cb);
		res = mps_arena_create_k(&arena, mps_arena_class_vm(), args);
	} MPS_ARGS_END(args);

	/* Create new fmt */
	MPS_ARGS_BEGIN(args) {
		MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, ALIGNMENT);
		MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
		MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
		MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
		MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
		res = mps_fmt_create_k(&obj_fmt, arena, args);
	} MPS_ARGS_END(args);

	/* Create new pool */
	MPS_ARGS_BEGIN(args) {
		MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
		die(mps_pool_create_k(&obj_pool, arena, mps_class_amcz(), args),
			"mps_pool_create_k");
	} MPS_ARGS_END(args);

	/* Register thread */
	die(mps_thread_reg(&thread, arena), "Thread reg");

	/* Register stack roots */
	die(mps_root_create_thread(&stack_root, arena, thread, cold), "Create Stack root");

	/* Create allocation point */
	die(mps_ap_create_k(&obj_ap, obj_pool, mps_args_none), "Create Allocation point");

	/* Allocate objects and force arena extension */
	for (i = 0; i < N_TESTOBJ; i++) {
		int j;
		test_alloc_obj_s* p_test_obj;
		do {
			res = mps_reserve(&p, obj_ap, obj_size);
			if (res != MPS_RES_OK) 
				exit(EXIT_FAILURE);

			/* p is now an ambiguous reference to the reserved block */
			testobj[i] = p;

			/* initialize obj */
			p_test_obj = testobj[i];
			p_test_obj->type = TYPE_INTBOX;
			p_test_obj->size = obj_size;

			for (j = 0; j < N_INT_TESTOBJ; ++j) {
				p_test_obj->int_array[j] = j;
			}
		} while (!mps_commit(obj_ap, p, obj_size)); /* see note 2 [FIXME:what? where?] */
		/* obj is now valid and managed by the MPS */

		/* Destroy the local reference to test object*/
		p_test_obj = NULL;
		p = NULL;
	}

	/* destroy all the references to the objects*/
	for (i = 0; i < N_TESTOBJ; i++) {

		/* !!! JPH bonus test of mps_addr_object - move to separate testbench */
#if 0
    /* Comment this out as mps_addr_object is unavailable */
    mps_addr_t out;
		assert(N_TESTOBJ <= N_INT_TESTOBJ);

		/* [FIXME:explain this, especially use of i] */
		die(mps_addr_object(&out, arena, &(testobj[i])->int_array[i]), "Address object");

		assert(out == testobj[i]);

		/* end piggy back testbench */

#endif
		/* now overwrite the ref */
		testobj[i] = NULL;
	}

	/* Collect */
	mps_arena_collect(arena);

	printf("Arena extended %d times\n", n_extend);
	printf("Arena contracted %d times\n", n_contract);

	/* Clean up */
	/* [FIXME:is park needed] */
	mps_arena_park(arena);
	
	mps_root_destroy(stack_root);
	mps_thread_dereg(thread);
	mps_ap_destroy(obj_ap);
	mps_pool_destroy(obj_pool);
	mps_fmt_destroy(obj_fmt);
	mps_arena_destroy(arena);

	
	if (n_extend == 0) 
		printf("%s: No callbacks received upon arena extended!\n", argv[0]);
	if (n_contract == 0)
		printf("%s: No callbacks received upon arena contracted!\n", argv[0]);

	if (n_contract == 0 || n_extend == 0)
	{
		printf("%s: Conclusion: Test failed.\n", argv[0]);
		return 1;
	}

	printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
	return 0;
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2022-2023 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
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
