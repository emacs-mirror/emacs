.. _topic-error:

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

For more examples, see doc.mps.ref-man.if-conv. <https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/if-conv/>


Interface
---------

.. c:type:: mps_res_t

    The type of :term:`result codes <result code>`. It is a
    :term:`transparent alias <transparent type>` for ``int``, provided
    for convenience and clarity.

    A result code indicates the success or failure of an operation,
    along with the reason for failure. As with error numbers in Unix,
    the meaning of a result code depends on the call that returned it.
    Refer to the documentation of the function for the exact meaning
    of each result code.

    The result codes are:

    * :c:macro:`MPS_RES_OK`: operation succeeded.

    * :c:macro:`MPS_RES_FAIL`: operation failed.

    * :c:macro:`MPS_RES_IO`: an input/output error occurred.

    * :c:macro:`MPS_RES_LIMIT`: an internal limitation was exceeded.

    * :c:macro:`MPS_RES_MEMORY`: needed memory could not be obtained.

    * :c:macro:`MPS_RES_RESOURCE`: a needed resource could not be
      obtained.

    * :c:macro:`MPS_RES_UNIMPL`: operation is not implemented.

    * :c:macro:`MPS_RES_COMMIT_LIMIT`: the arena's :term:`commit
      limit` would be exceeded.

    * :c:macro:`MPS_RES_PARAM`: an invalid parameter was passed.


.. c:macro:: MPS_RES_COMMIT_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested without exceeding the :term:`commit limit`.

    You need to deallocate something to make more space, or increase
    the commit limit by calling :c:func:`mps_arena_commit_limit_set`.


.. c:macro:: MPS_RES_FAIL

    A :term:`result code` indicating that something went wrong that
    does not fall under the description of any other result code. The
    exact meaning depends on the function that returned this result
    code.


.. c:macro:: MPS_RES_IO

    A :term:`result code` indicating that an input/output error
    occurred. The exact meaning depends on the function that returned
    this result code.


.. c:macro:: MPS_RES_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested because of an internal limitation of the
    MPS. The exact meaning depends on the function that returned this
    result code.


.. c:macro:: MPS_RES_MEMORY

    A :term:`result code` indicating that an operation could not be
    completed because there wasn't enough memory available.

    You need to deallocate something or allow the :term:`garbage
    collector` to reclaim something to free enough memory, or expand
    the :term:`arena` (if you're using an arena for which that does
    not happen automatically).

    .. note::

        Failing to acquire enough memory because the :term:`commit
        limit` would have been exceeded is indicated by returning
        :c:macro:`MPS_RES_COMMIT_LIMIT`, not ``MPS_RES_MEMORY``.

        Running out of :term:`address space` (as might happen in
        :term:`virtual memory` systems) is indicated by returning
        :c:macro:`MPS_RES_RESOURCE`, not ``MPS_RES_MEMORY``.


.. c:macro:: MPS_RES_OK

    A :term:`result code` indicating that an operation succeeded.

    If a function takes an :term:`out parameter` or an :term:`in/out
    parameter`, this parameter will only be updated if
    :c:macro:`MPS_RES_OK` is returned. If any other result code is
    returned, the parameter will be left untouched by the function.

    :c:macro:`MPS_RES_OK` is zero.


.. c:macro:: MPS_RES_PARAM

    A :term:`result code` indicating that an operation could not be
    completed as requested because an invalid parameter was passed to
    the operation. The exact meaning depends on the function that
    returned this result code.


.. c:macro:: MPS_RES_RESOURCE

    A :term:`result code` indicating that an operation could not be
    completed as requested because the MPS could not obtain a needed
    resource. The resource in question depends on the operation.

    Two special cases have their own result codes: when the MPS runs
    out of committed memory, it returns :c:macro:`MPS_RES_MEMORY`, and
    when it cannot proceed without exceeding the :term:`commit limit`,
    it returns :c:macro:`MPS_RES_COMMIT_LIMIT`.

    This result code can be returned when the MPS runs out of
    :term:`virtual memory`. If this happens, you need to reclaim
    memory within your process (as for the result code
    :c:macro:`MPS_RES_MEMORY`), or terminate other processes running
    on the same machine.


.. c:macro:: MPS_RES_UNIMPL

    A :term:`result code` indicating that an operation, or some vital
    part of it, is not implemented.

    This might be returned by functions that are no longer supported,
    or by operations that are included for future expansion, but not
    yet supported.


