.. index::
   single: deprecated interfaces

.. _topic-deprecated:

Deprecated interfaces
=====================

This chapter documents the public symbols in the MPS interface that
are currently deprecated. These symbols may be removed in any future
release (see :ref:`topic-interface-support` for details). If you are
using one of these symbols, then you should update your code to use
the supported interface.

.. note::

    If you are relying on a deprecated interface, and there is no
    supported alternative, please :ref:`contact us <contact>`. It
    makes a difference if we know that someone is using a feature.



.. index::
   single: deprecated interfaces; in version 1.118

Deprecated in version 1.118
...........................

.. c:macro:: MPS_KEY_SPARE_COMMIT_LIMIT

    .. deprecated::

        Use :c:macro:`MPS_KEY_SPARE` instead.

    When supplied as a :term:`keyword argument` to
    :c:func:`mps_arena_create_k`, specifies the initial :term:`spare
    commit limit` in :term:`bytes (1)` relative to the arena's
    :term:`commit limit`. If the value is greater than the arena's
    commit limit then the spare commit limit is set to 1.0 exactly.


.. c:function:: size_t mps_arena_spare_commit_limit(mps_arena_t arena)

    .. deprecated::

        Use :c:func:`mps_arena_spare` instead.

    Return the current :term:`spare commit limit` for an :term:`arena`
    in :term:`bytes (1)`, that is, the product of the :term:`committed
    <mapped>` memory and the spare fraction.


.. c:function:: void mps_arena_spare_commit_limit_set(mps_arena_t arena, size_t limit)

    .. deprecated::

        Use :c:func:`mps_arena_spare_set` instead.

    Change the :term:`spare commit limit` for an :term:`arena` in
    terms of :term:`bytes (1)` relative to the current
    :term:`committed <mapped>` memory. If the ``limit`` argument is
    greater than the current committed memory then the spare commit
    limit is set to 1.0 exactly.


.. c:function:: void mps_arena_formatted_objects_walk(mps_arena_t arena, mps_formatted_objects_stepper_t f, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_pool_walk` instead.

    Visit all :term:`formatted objects` in an
    :term:`arena`.

    ``arena`` is the arena whose formatted objects you want to visit.

    ``f`` is a formatted objects stepper function. It will be called for
    each formatted object in the arena. See
    :c:type:`mps_formatted_objects_stepper_t`.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    Each :term:`pool class` determines for which objects the stepper
    function is called. Typically, all validly formatted objects are
    visited. :term:`Padding objects` may be visited at the pool
    class's discretion: the stepper function must handle this
    case.

    .. warning::

        The callback function must obey the restrictions documented
        under :c:type:`mps_formatted_objects_stepper_t`.

        If a garbage collection is currently in progress (that is, if
        the arena is in the :term:`clamped <clamped state>` or
        :term:`unclamped state`), then only objects that are known to
        be currently valid are visited.

        If you need to be certain that all objects are visited, or if
        the callback function needs to follow references from the
        object to automatically managed memory, you must ensure that
        the arena is in the :term:`parked state` by calling
        :c:func:`mps_arena_park` before calling this function (and
        release it by calling :c:func:`mps_arena_release` afterwards,
        if desired).

        If your application has requirements for introspection that
        can't be met under these restrictions, :ref:`contact us
        <contact>`.


.. c:type:: void (*mps_formatted_objects_stepper_t)(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_pool_walk` instead.

    The type of a :term:`formatted objects`
    :term:`stepper function`.

    A function of this type can be passed to
    :c:func:`mps_arena_formatted_objects_walk`, in which case it will
    be called for each formatted object in an :term:`arena`. It
    receives five arguments:

    ``addr`` is the address of the object.

    ``fmt`` is the :term:`object format` for that object.

    ``pool`` is the :term:`pool` to which the object belongs.

    ``p`` and ``s`` are the corresponding values that were passed to
    :c:func:`mps_arena_formatted_objects_walk`.

    The function may not call any function in the MPS. It may access:

    a. memory inside the object or block pointed to by ``addr``;

    b. memory managed by the MPS that is in pools that do not protect
       their contents;

    c. memory not managed by the MPS.

    It must not:

    d. access other memory managed by the MPS;

    e. modify any of the references in the object.


.. c:function:: void mps_amc_apply(mps_pool_t pool, mps_amc_apply_stepper_t f, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_pool_walk` instead.

    Visit all :term:`formatted objects` in an AMC pool.

    ``pool`` is the pool whose formatted objects you want to visit.

    ``f`` is a function that will be called for each formatted object in
    the pool.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    It is an error to call this function when the :term:`arena` is not
    in the :term:`parked state`. You need to call
    :c:func:`mps_arena_collect` or :c:func:`mps_arena_park` before
    calling :c:func:`mps_amc_apply`.

    The function ``f`` will be called on both :term:`client <client
    object>` and :term:`padding objects`. It is the job of ``f`` to
    distinguish, if necessary, between the two. It may also be called
    on :term:`dead` objects that the collector has not recycled or has
    been unable to recycle.

    .. note::

        There is no equivalent function for other pool classes, but
        there is a more general function
        :c:func:`mps_arena_formatted_objects_walk` that visits all
        formatted objects in the arena.

    .. note::

        This function is intended for heap analysis, tuning, and
        debugging, not for frequent use in production.


.. c:type:: void (*mps_amc_apply_stepper_t)(mps_addr_t addr, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_pool_walk` instead.

    The type of a :term:`stepper function` for :term:`formatted
    objects` in an AMC pool.

    ``addr`` is the address of an object in the pool.

    ``p`` and ``s`` are the corresponding arguments that were passed
    to :c:func:`mps_amc_apply`.

    The function may not call any function in the MPS. It may access:

    a. memory inside the object or block pointed to by ``addr``;

    b. memory managed by the MPS that is in pools that do not protect
       their contents;

    c. memory not managed by the MPS;

    It must not:

    d. access other memory managed by the MPS;

    e. modify any of the references in the object.


.. index::
   single: deprecated interfaces; in version 1.115

Deprecated in version 1.115
...........................

.. c:type:: typedef mps_pool_class_t mps_class_t

    .. deprecated::

        The former name for :c:type:`mps_pool_class_t`, chosen when
        pools were the only objects in the MPS that belonged to
        classes.


.. c:function:: size_t mps_mvff_free_size(mps_pool_t pool)

    .. deprecated::

        Use the generic function :c:func:`mps_pool_free_size` instead.

    Return the total amount of free space in an MVFF pool.

    ``pool`` is the MVFF pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mvff_size(mps_pool_t pool)

    .. deprecated::

        Use the generic function :c:func:`mps_pool_total_size`
        instead.

    Return the total size of an MVFF pool.

    ``pool`` is the MVFF pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.


.. c:function:: size_t mps_mvt_free_size(mps_pool_t pool)

    .. deprecated::

        Use the generic function :c:func:`mps_pool_free_size` instead.

    Return the total amount of free space in an MVT pool.

    ``pool`` is the MVT pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mvt_size(mps_pool_t pool)

    .. deprecated::

        Use the generic function :c:func:`mps_pool_total_size`
        instead.

    Return the total size of an MVT pool.

    ``pool`` is the MVT pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.


.. c:function:: mps_res_t mps_root_create_reg(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_thr_t thr, mps_reg_scan_t reg_scan, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_root_create_thread` instead.

    Register a :term:`root` that consists of the :term:`references`
    fixed in a :term:`thread's <thread>` registers and stack by a
    scanning function.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``thr`` is the thread.

    ``reg_scan`` is a scanning function. See :c:type:`mps_reg_scan_t`.

    ``p`` and ``s`` are arguments that will be passed to ``reg_scan`` each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    .. note::

        It is not supported for :term:`client programs` to pass their
        own scanning functions to this function. The built-in MPS
        function :c:func:`mps_stack_scan_ambig` must be used. In this
        case the ``p`` argument must be a pointer to the :term:`cold
        end` of the thread's stack (or the part of the stack
        containing references to memory managed by the MPS). The ``s``
        argument is ignored.

.. c:function:: mps_res_t mps_root_create_table(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count)

    .. deprecated::

        This function is equivalent to::

          mps_root_create_area(root_o, arena, rank, mode,
                               base, base + count,
                               mps_scan_area, NULL, 0)

    Register a :term:`root` that consists of a vector of
    :term:`references`.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``base`` points to a vector of references.

    ``count`` is the number of references in the vector.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    .. _topic-root-type-pun:

    .. warning::

        The ``base`` argument has type ``mps_addr_t *`` (a typedef for
        ``void **``) but the table of references most likely has some
        other pointer type, ``my_object *`` say. It is tempting to
        write::

            mps_root_create_table(..., (mps_addr_t *)my_table, ...)

        but this is :term:`type punning`, and its behaviour is not
        defined in ANSI/ISO Standard C. (GCC and Clang have a warning
        flag ``-Wstrict-aliasing`` which detects some errors of this
        form.)

        To ensure well-defined behaviour, the pointer must be
        converted via ``void *`` (or via :c:type:`mps_addr_t`, which
        is a typedef for ``void *``), like this::

            mps_addr_t base = my_table;
            mps_root_create_table(..., base, ...)

.. c:function:: mps_res_t mps_root_create_table_tagged(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count, mps_area_scan_t scan_area, mps_word_t mask, mps_word_t pattern)

    .. deprecated::

        This function is equivalent to::

            mps_root_create_area_tagged(root_o, arena, rank, mode,
                                        base, base + size,
                                        scan_area, mask, pattern)

    Register a :term:`root` that consists of a vector of :term:`tagged
    references`.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``base`` points to a vector of tagged references.

    ``count`` is the number of tagged references in the vector.

    ``scan_area`` is an tagged area scanning function that will be
    used to scan the table, for example :c:func:`mps_scan_area_tagged`
    or :c:func:`mps_scan_area_tagged_or_zero`.  See
    :ref:`topic-scanning-area`.

    ``mask`` is a :term:`bitmask` that is passed to ``scan_area`` to
    be applied to the words in the vector to locate the :term:`tag`.

    ``pattern`` is passed to ``scan_area`` to determine whether to
    consider a word as a reference.  For example,
    :c:func:`mps_scan_area_tagged` will not consider any word that is
    unequal to this (after masking with ``mask``) to be a reference.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    .. warning::

        See the warning for :c:func:`mps_root_create_table` above.

.. c:function:: mps_res_t mps_root_create_table_masked(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count, mps_word_t mask)

    .. deprecated::

        Use :c:func:`mps_root_create_area_tagged` instead, passing
        zero for the ``pattern`` argument. This function is equivalent
        to::

            mps_root_create_area_tagged(root_o, arena, rank, rm,
                                        base, base + size,
                                        mps_scan_area_tagged,
                                        mask, 0)

    Register a :term:`root` that consists of a vector of :term:`tagged
    references` whose pattern is zero.

.. c:type:: mps_res_t (*mps_reg_scan_t)(mps_ss_t ss, mps_thr_t thr, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_root_create_thread` instead.

    The type of a root scanning function for roots created with
    :c:func:`mps_root_create_reg`.

    ``ss`` is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    ``thr`` is the :term:`thread`.

    ``p`` and ``s`` are the corresponding values that were passed to
    :c:func:`mps_root_create_reg`.

    Returns a :term:`result code`. If a fix function returns a value
    other than :c:macro:`MPS_RES_OK`, the scan method must return that
    value, and may return without fixing any further references.
    Generally, it is better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.

    A root scan method is called whenever the MPS needs to scan the
    root. It must then indicate references within the root by calling
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

    .. seealso::

        :ref:`topic-scanning`.


.. c:function:: mps_reg_scan_t mps_stack_scan_ambig

    .. deprecated::

        Use :c:func:`mps_root_create_thread_tagged` instead, passing
        ``sizeof(mps_word_t) - 1`` for the ``mask`` argument, and
        ``0`` for the ``pattern`` argument.

    A root scanning function for :term:`ambiguous <ambiguous
    reference>` scanning of :term:`threads`, suitable for
    passing to :c:func:`mps_root_create_reg`.

    It scans all integer registers and everything on the stack of the
    thread given, and can therefore only be used with :term:`ambiguous
    roots`. It scans locations that are more recently added to the
    stack than the location that was passed in the ``p`` argument to
    :c:func:`mps_root_create_reg`.

    References are assumed to be represented as machine words, and are
    required to be word-aligned; unaligned values are ignored.


.. index::
   single: deprecated interfaces; in version 1.113

Deprecated in version 1.113
...........................

.. c:function:: MPS_ARGS_DONE(args)

    .. deprecated::

        Formerly this was used to finalize a list of :term:`keyword
        arguments` before passing it to a function. It is no longer
        needed.


.. index::
   single: deprecated interfaces; in version 1.112

Deprecated in version 1.112
...........................

.. c:function:: mps_res_t mps_arena_create(mps_arena_t *arena_o, mps_arena_class_t arena_class, ...)

    .. deprecated::

        Use :c:func:`mps_arena_create_k` instead.

    An alternative to :c:func:`mps_arena_create_k` that takes its
    extra arguments using the standard :term:`C` variable argument
    list mechanism.

    When creating an arena of class :c:func:`mps_arena_class_cl`, pass
    the values for the keyword arguments :c:macro:`MPS_KEY_ARENA_SIZE`
    and :c:macro:`MPS_KEY_ARENA_CL_BASE` like this::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t mps_arena_class_cl(),
                                   size_t arena_size,
                                   mps_addr_t cl_base)

    When creating an arena of class :c:func:`mps_arena_class_vm`, pass
    the value for the keyword argument :c:macro:`MPS_KEY_ARENA_SIZE`
    like this::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t mps_arena_class_vm(),
                                   size_t arena_size)


.. c:function:: mps_res_t mps_arena_create_v(mps_arena_t *arena_o, mps_arena_class_t arena_class, va_list args)

    .. deprecated::

        Use :c:func:`mps_arena_create_k` instead.

    An alternative to :c:func:`mps_arena_create_k` that takes its
    extra arguments using the standard :term:`C` ``va_list``
    mechanism. See :c:func:`mps_arena_create` for details of which
    arguments to pass for the different arena classes.


.. c:function:: mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, mps_pool_class_t pool_class, ...)

    .. deprecated::

        Use :c:func:`mps_pool_create_k` instead.

    An alternative to :c:func:`mps_pool_create_k` that takes its
    extra arguments using the standard :term:`C` variable argument
    list mechanism.

    When creating a pool of class :c:func:`mps_class_amc` or
    :c:func:`mps_class_amcz`, pass the values for the keyword
    arguments :c:macro:`MPS_KEY_FORMAT` and :c:macro:`MPS_KEY_CHAIN`
    like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_amc(),
                                  mps_fmt_t format,
                                  mps_chain_t chain)

    When creating a pool of class :c:func:`mps_class_ams`, pass the
    values for the keyword arguments :c:macro:`MPS_KEY_FORMAT`,
    :c:macro:`MPS_KEY_CHAIN` and ambiguous flag
    :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_ams(),
                                  mps_fmt_t format,
                                  mps_chain_t chain,
                                  mps_bool_t ams_support_ambiguous)

    When creating a pool of class :c:func:`mps_class_ams_debug`, pass
    the values for the keyword arguments
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS`, :c:macro:`MPS_KEY_FORMAT`,
    :c:macro:`MPS_KEY_CHAIN` and
    :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_ams_debug(),
                                  mps_pool_debug_option_s *pool_debug_options,
                                  mps_fmt_t format,
                                  mps_chain_t chain,
                                  mps_bool_t ams_support_ambiguous)

    When creating a pool of class :c:func:`mps_class_awl`, pass the
    values for the keyword arguments :c:macro:`MPS_KEY_FORMAT` and
    :c:macro:`MPS_KEY_AWL_FIND_DEPENDENT` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_awl(),
                                  mps_fmt_t format,
                                  mps_awl_find_dependent_t awl_find_dependent)

    When creating a pool of class :c:func:`mps_class_lo`, pass the
    value for the keyword argument :c:macro:`MPS_KEY_FORMAT` like
    this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_lo(),
                                  mps_fmt_t format)

    When creating a pool of class :c:func:`mps_class_mfs`, pass the
    values for the keyword arguments :c:macro:`MPS_KEY_EXTEND_BY` and
    :c:macro:`MPS_KEY_MFS_UNIT_SIZE` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_mfs(),
                                  size_t extend_by,
                                  size_t unit_size)

    When creating a pool of class :c:func:`mps_class_mvff`, pass the
    values for the keyword arguments :c:macro:`MPS_KEY_EXTEND_BY`,
    :c:macro:`MPS_KEY_MEAN_SIZE`, :c:macro:`MPS_KEY_ALIGN`,
    :c:macro:`MPS_KEY_MVFF_SLOT_HIGH`,
    :c:macro:`MPS_KEY_MVFF_ARENA_HIGH` and
    :c:macro:`MPS_KEY_MVFF_FIRST_FIT` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_mvff(),
                                  size_t extend_by,
                                  size_t mean_size,
                                  mps_align_t align,
                                  mps_bool_t mvff_slot_high,
                                  mps_bool_t mvff_arena_high,
                                  mps_bool_t mvff_first_fit)

    When creating a pool of class :c:func:`mps_class_mvff_debug`, pass
    the values for the keyword arguments
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS`,
    :c:macro:`MPS_KEY_EXTEND_BY`, :c:macro:`MPS_KEY_MEAN_SIZE`,
    :c:macro:`MPS_KEY_ALIGN`, :c:macro:`MPS_KEY_MVFF_SLOT_HIGH`,
    :c:macro:`MPS_KEY_MVFF_ARENA_HIGH`, and
    :c:macro:`MPS_KEY_MVFF_FIRST_FIT` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_mvff_debug(),
                                  mps_pool_debug_option_s *pool_debug_options,
                                  size_t extend_by,
                                  size_t mean_size,
                                  mps_align_t align,
                                  mps_bool_t mvff_slot_high,
                                  mps_bool_t mvff_arena_high,
                                  mps_bool_t mvff_first_fit)

    When creating a pool of class :c:func:`mps_class_mvt`, pass the
    values for the keyword arguments :c:macro:`MPS_KEY_MIN_SIZE`,
    :c:macro:`MPS_KEY_MEAN_SIZE`, :c:macro:`MPS_KEY_MAX_SIZE`,
    :c:macro:`MPS_KEY_MVT_RESERVE_DEPTH` and
    :c:macro:`MPS_KEY_MVT_FRAG_LIMIT` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_mvt(),
                                  size_t min_size,
                                  size_t mean_size,
                                  size_t max_size,
                                  mps_word_t mvt_reserve_depth,
                                  mps_word_t mvt_frag_limit)

    .. note::

       The ``mvt_frag_limit`` is a percentage from 0 to 100
       inclusive when passed to :c:func:`mps_pool_create`, not a
       double from 0.0 to 1.0 as in :c:func:`mps_pool_create_k`.

    When creating a pool of class :c:func:`mps_class_snc`, pass the
    value for the keyword argument :c:macro:`MPS_KEY_FORMAT` like
    this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_snc(),
                                  mps_fmt_t format)


.. c:function:: mps_res_t mps_pool_create_v(mps_pool_t *pool_o, mps_arena_t arena, mps_pool_class_t pool_class, va_list args)

    .. deprecated::

        Use :c:func:`mps_pool_create_k` instead.

    An alternative to :c:func:`mps_pool_create_k` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism. See
    :c:func:`mps_pool_create` for details of which arguments to pass
    for the different pool classes.


.. c:function:: mps_res_t mps_ap_create(mps_ap_t *ap_o, mps_pool_t pool, ...)

    .. deprecated::

        Use :c:func:`mps_ap_create_k` instead.

    An alternative to :c:func:`mps_ap_create_k` that takes its extra
    arguments using the standard :term:`C` variable argument list
    mechanism.

    When creating an allocation point on a pool of class
    :c:func:`mps_class_ams`, :c:func:`mps_class_ams_debug`,
    :c:func:`mps_class_awl` or :c:func:`mps_class_snc`, pass the
    keyword argument :c:macro:`MPS_KEY_RANK` like this::

            mps_res_t mps_ap_create(mps_ap_t *ap_o, mps_pool_t pool,
                                    mps_rank_t rank)


.. c:function:: mps_res_t mps_ap_create_v(mps_ap_t *ap_o, mps_pool_t pool, va_list args)

    .. deprecated::

        Use :c:func:`mps_ap_create_k` instead.

    An alternative to :c:func:`mps_ap_create_k` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism. See
    :c:func:`mps_ap_create` for details of which arguments to pass
    for the different pool classes.


.. c:type:: mps_fmt_A_s

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    The type of the structure used to create an :term:`object format`
    of variant A. ::

        typedef struct mps_fmt_A_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_copy_t  copy;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
        } mps_fmt_A_s;

    The fields of this structure correspond to the keyword arguments
    to :c:func:`mps_fmt_create_k`, except for ``copy``, which is not
    used. In older versions of the MPS this was a *copy method*
    that copied objects belonging to this format.


.. c:function:: mps_res_t mps_fmt_create_A(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_A_s *fmt_A)

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    Create an :term:`object format` based on a description of an
    object format of variant A.


.. c:type:: mps_fmt_B_s

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    The type of the structure used to create an :term:`object format`
    of variant B. ::

        typedef struct mps_fmt_B_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_copy_t  copy;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
            mps_fmt_class_t mps_class;
        } mps_fmt_B_s;

    Variant B is the same as variant A except for the addition of the
    ``mps_class`` method. See :c:type:`mps_fmt_A_s`.


.. c:function:: mps_res_t mps_fmt_create_B(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_B_s *fmt_B)

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    Create an :term:`object format` based on a description of an
    object format of variant B.


.. c:type:: mps_fmt_auto_header_s

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    The type of the structure used to create an :term:`object format`
    of variant auto-header. ::

        typedef struct mps_fmt_auto_header_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
            size_t          mps_headerSize;
        } mps_fmt_auto_header_s;

    Variant auto-header is the same as variant A except for the
    removal of the unused ``copy`` method, and the addition of the
    ``mps_headerSize`` field. See :c:type:`mps_fmt_A_s`.


.. c:function:: mps_res_t mps_fmt_create_auto_header(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_auto_header_s *fmt_ah)

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    Create an :term:`object format` based on a description of an
    object format of variant auto-header.


.. c:type:: mps_fmt_fixed_s

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    The type of the structure used to create an :term:`object format`
    of variant fixed. ::

        typedef struct mps_fmt_fixed_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
        } mps_fmt_fixed_s;

    Variant fixed is the same as variant A except for the removal of
    the unused ``copy`` method, and the lack of a ``skip`` method
    (this is not needed because the objects are fixed in size). See
    :c:type:`mps_fmt_A_s`.


.. c:function:: mps_res_t mps_fmt_create_fixed(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_fixed_s *fmt_fixed)

    .. deprecated::

        Use :c:func:`mps_fmt_create_k` instead.

    Create an :term:`object format` based on a description of an
    object format of variant fixed.
