.. index::
   single: malloc; implementing
   single: free; implementing

.. _guide-malloc:

Implementing malloc and free
============================

The MPS function :c:func:`mps_free` is unlike the Standard C Library
function :c:func:`free` in that it takes a ``size`` argument. That's
because it's nearly always the case that either the size of an object
is known statically based on its type (for example, a structure), or
else the size of the object is easily computed from information that
needs to be stored anyway (for example, a vector), and so memory can
be saved by not storing the size separately.

But sometimes you need to interact with :term:`foreign code` which
requires :c:func:`malloc` and :c:func:`free` (or a pair of functions
with the same interface). How can you use the MPS to implement these
functions? By storing the size in a header adjacent to the object,
like this::

    #include "mps.h"

    static mps_pool_t malloc_pool;

    typedef union {
        size_t size;
        char alignment[MPS_PF_ALIGN]; /* see note below */
    } header_u;

    void *malloc(size_t size) {
        mps_res_t res;
        mps_addr_t p;
        header_u *header;
        size += sizeof *header;
        res = mps_alloc(&p, malloc_pool, size);
        if (res != MPS_RES_OK)
            return NULL;
        header = p;
        header->size = size;
        return header + 1;
    }

    void free(void *p) {
        if (p) {
            header_u *header = ((header_u *)p) - 1;
            mps_free(malloc_pool, header, header->size);
        }
    }

The ``alignment`` member of ``union header_u`` ensures that
allocations are aligned to the platform's :term:`natural alignment`
(see :ref:`guide-lang-alignment`).

The pool needs to belong to a :term:`manually managed <manual memory
management>` pool class, for example :ref:`pool-mvff` (or its
:ref:`debugging counterpart <topic-debugging>`)::

    #include "mpscmvff.h"

    void malloc_pool_init(mps_arena_t arena) {
        mps_res_t res;
        res = mps_pool_create_k(&malloc_pool, arena, mps_class_mvff(), mps_args_none);
        if (res != RES_OK)
            abort();
    }
