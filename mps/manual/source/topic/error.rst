.. _topic-error:

=============
Error handing
=============

There's some documentation at //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/assertion/index.html but it seems not to be true

::

    mps_addr_t p;
    switch (mps_alloc(&p, pool, size)) {
    case MPS_RES_LIMIT:
        bomb("The MPS has reached an internal limit");
        break;

      /* ... */
    }


::

    switch (res = mps_pool_create_v(&pool, arena, class, params)) {
    case MPS_RES_PARAM:
        bomb("Can't make a pool with those specifications");
        break;

        /* ... */
     }

::

    mps_addr_t p;
    mps_res_t res;

    res = mps_alloc(&p, pool, sizeof(struct spong));
    if (res != MPS_RES_OK) {
        handle_memory_error(res);
        abort();
    }

For more examples, see doc.mps.ref-man.if-conv.
