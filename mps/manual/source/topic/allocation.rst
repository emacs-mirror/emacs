.. _topic-allocation:

==========
Allocation
==========


This example seems to be wrong (no function "mps_pool_alloc" in the public interface).

::

    {
        mps_addr_t new_block;
        mps_res_t res;
        thingy *tp;

        res = mps_pool_alloc(&new_block, pool, sizeof(thingy));
        if (res != MPS_RES_OK) return res;
        tp = new_block;

        /* ... */
    }


Some :term:`pools <pool>` and allocation protocols accept an :term:`alignment` as an option. This
ensures that objects in the pool or objects allocated observe a
stricter alignment than that of the :term:`object format`.

::

    mps_res_t res;
    mps_addr_t p;

    res = mps_alloc(&p, pool, size);
    if (res != MPS_RES_OK) {
        /* p hasn't been touched in this case. */
        handle error;
    }

    /* p now contains the result, which is the address of the new block */
    /* in this case. */
