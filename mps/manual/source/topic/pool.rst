.. index::
   single: pool; creating

.. _topic-pool:

Pools
=====

Within an :term:`arena` a client program creates one or more pools. A
pool is responsible for requesting memory from the :term:`arena` and
making it available for allocation.

.. c:type:: mps_pool_t

    The type of :term:`pools`.

    A pool is responsible for requesting memory from the :term:`arena`
    and making it available to the :term:`client program` via
    :c:func:`mps_alloc` or via an :term:`allocation point`.


.. c:function:: mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, mps_class_t class, ...)

    Create a :term:`pool` in an :term:`arena`.

    ``pool_o`` points to a location that will hold a pointer to the new
    pool.

    ``arena`` is the arena in which to create the pool.

    ``class`` is the :term:`pool class` of the new pool.

    Some pool classes require additional arguments to be passed to
    :c:func:`mps_pool_create`. See the documentation for the pool
    class.

    Returns :c:macro:`MPS_RES_OK` if the pool is created successfully,
    or another :term:`result code` otherwise.

    The pool persists until it is destroyed by calling
    :c:func:`mps_pool_destroy`.

    .. note::

        There's an alternative function :c:func:`pool_create_v` that
        takes its extra arguments using the standard :term:`C`
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_pool_create_v(mps_pool_t *pool_o, mps_arena_t arena, mps_class_t class, va_list args)

    An alternative to :c:func:`mps_pool_create` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism.


.. c:function:: void mps_pool_destroy(mps_pool_t pool)

    Destroy a :term:`pool`.

    ``pool`` is the pool to destroy.

    This function checks the consistency of the pool, destroys the
    pool's internal control structures and causes the pool's memory to
    be returned to the :term:`arena` for reuse by other pools, or to
    be returned to the operating system.  Blocks allocated from the
    pool may no longer be used.

    It is an error to destroy a pool without first destroying all
    :term:`allocation points` and :term:`segregated allocation caches`
    created in the pool.


.. index::
   single: pool class

Pool classes
------------

Pools belong to :term:`pool classes` that specify policies for how
their memory is managed. Some pools are :term:`manually managed
<manual memory management>` (you must call :c:func:`mps_free` to
return a block of memory to the pool) and others are
:term:`automatically managed <automatic memory management>` (the
:term:`garbage collector` reclaims :term:`unreachable` blocks).

See :ref:`pool` for a list of pool classes.


.. c:type:: mps_class_t

    The type of :term:`pool classes`.

    .. note::

        This should really be called ``mps_pool_class_t`` but it is
        too late to change it now.


.. index::
   pair: pool; introspection

Pool introspection
------------------

.. c:function:: mps_bool_t mps_addr_pool(mps_pool_t *pool_o, mps_arena_t arena, mps_addr_t addr)

    Determine the :term:`pool` to which an address belongs.

    ``pool_o`` points to a location that will hold the address of the
    pool, if one is found.

    ``arena`` is the arena whose pools will be considered.

    ``addr`` is the address.

    If ``addr`` is the address of a location inside a block allocated
    from a pool in ``arena``, then update the location pointed to by
    ``pool_o`` with the address of the pool, and return true.

    If ``addr`` points to a location that is not managed by ``arena``,
    return false.

    If neither of the above conditions is satisfied,
    :c:func:`mps_addr_pool` may return either true or false.

    .. note::

        This function might return a false positive by returning true
        if you ask about an address that happens to be inside memory
        managed by a pool, but which is not inside a block allocated
        by that pool. It never returns a false negative.

        The result from this function is valid only at the instant at
        which the function returned. In some circumstances the result
        may immediately become invalidated. For reliable results call
        this function and interpret the result while the arena is in
        the :term:`parked state`.
