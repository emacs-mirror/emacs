.. index::
   single: deprecated interfaces

.. _topic-deprecated:

Deprecated interfaces
=====================

This chapter documents the public symbols in the MPS interface that
are now deprecated. These symbols may be removed in any future release
(see :ref:`topic-interface-support` for details). If you are using one
of these symbols, then you should update your code to use the
supported interface.

.. note::

    If you are relying on a deprecated interface, and there is no
    supported alternative, please :ref:`contact us <contact>`. It
    makes a difference if we know that someone is using a feature.


.. index::
   single: deprecated interfaces; in version 1.115

Deprecated in version 1.115
...........................

.. c:type:: typedef mps_pool_class_t mps_class_t

    .. deprecated::

        The former name for :c:type:`mps_pool_class_t`, chosen when
        pools were the only objects in the MPS that belonged to
        classes.


.. c:function:: size_t mps_mv_free_size(mps_pool_t pool)

    .. deprecated::

        Use the generic function :c:func:`mps_pool_free_size` instead.

    Return the total amount of free space in an MV pool.

    ``pool`` is the MV pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mv_size(mps_pool_t pool)

    .. deprecated::

        Use the generic function :c:func:`mps_pool_total_size`
        instead.

    Return the total size of an MV pool.

    ``pool`` is the MV pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.

    
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

        Use :c:func:`mps_root_create_stack` instead, passing
        ``sizeof(mps_word_t) - 1`` for the ``mask`` argument, and
        ``0`` for the ``pattern`` argument.

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


.. c:type:: mps_res_t (*mps_reg_scan_t)(mps_ss_t ss, mps_thr_t thr, void *p, size_t s)

    .. deprecated::

        Use :c:func:`mps_root_create_stack` instead.

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

    .. note::

        :term:`Client programs` are not expected to
        write scanning functions of this type. The built-in MPS
        function :c:func:`mps_stack_scan_ambig` must be used.


.. c:function:: mps_reg_scan_t mps_stack_scan_ambig

    .. deprecated::

        Use :c:func:`mps_root_create_stack` instead, passing
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

    When creating a pool of class :c:func:`mps_class_mv`, pass the
    values for the keyword arguments :c:macro:`MPS_KEY_EXTEND_BY`,
    :c:macro:`MPS_KEY_MEAN_SIZE`, and :c:macro:`MPS_KEY_MAX_SIZE` like
    this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_mv(),
                                  size_t extend_by,
                                  size_t mean_size,
                                  size_t max_size)

    When creating a pool of class :c:func:`mps_class_mv_debug`, pass
    the values for the keyword arguments
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS`,
    :c:macro:`MPS_KEY_EXTEND_BY`, :c:macro:`MPS_KEY_MEAN_SIZE` and
    :c:macro:`MPS_KEY_MAX_SIZE` like this::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_pool_class_t mps_class_mv_debug(),
                                  mps_pool_debug_option_s *pool_debug_options,
                                  size_t extend_by,
                                  size_t mean_size,
                                  size_t max_size)

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


.. index::
   single: deprecated interfaces; in version 1.111

Deprecated in version 1.111
...........................

.. c:function:: mps_res_t mps_fix(mps_ss_t ss, mps_addr_t *ref_io)

    .. deprecated::

        Use :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` instead.

    :term:`Fix` a :term:`reference`.

    This is a function equivalent to::

        MPS_SCAN_BEGIN(ss);
        res = MPS_FIX12(ss, ref_io);
        MPS_SCAN_END(ss);
        return res;

    Because :term:`scanning <scan>` is an operation on the
    :term:`critical path`, we recommend that you use
    :c:func:`MPS_FIX12` (or :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`)
    to ensure that the "stage 1 fix" is inlined.

    .. note::

        If you call this between :c:func:`MPS_SCAN_BEGIN` and
        :c:func:`MPS_SCAN_END`, you must use :c:func:`MPS_FIX_CALL` to
        ensure that the scan state is passed correctly.


.. c:function:: mps_word_t mps_telemetry_control(mps_word_t reset_mask, mps_word_t flip_mask)

    .. deprecated::

        Use :c:func:`mps_telemetry_get`,
        :c:func:`mps_telemetry_reset`, and :c:func:`mps_telemetry_set`
        instead.

    Update and return the :term:`telemetry filter`.

    ``reset_mask`` is a :term:`bitmask` indicating the bits in the
    telemetry filter that should be reset.

    ``flip_mask`` is a bitmask indicating the bits in the telemetry
    filter whose value should be flipped after the resetting.

    Returns the previous value of the telemetry filter, prior to the
    reset and the flip.

    The parameters ``reset_mask`` and ``flip_mask`` allow the
    specification of any binary operation on the filter control. For
    typical operations, the parameters should be set as follows:

    ============  ==============  =============
    Operation     ``reset_mask``  ``flip_mask``
    ============  ==============  =============
    ``set(M)``    ``M``           ``M``
    ------------  --------------  -------------
    ``reset(M)``  ``M``           ``0``
    ------------  --------------  -------------
    ``flip(M)``   ``0``           ``M``
    ------------  --------------  -------------
    ``read()``    ``0``           ``0``
    ============  ==============  =============


.. c:function:: void mps_tramp(void **r_o, mps_tramp_t f, void *p, size_t s)

    .. deprecated::

        The MPS trampoline is no longer required on any operating
        system supported by the MPS.

    Call a function via the MPS trampoline.

    ``r_o`` points to a location that will store the result of calling
    ``f``.

    ``f`` is the function to call.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    The MPS relies on :term:`barriers (1)` to protect memory
    that is in an inconsistent state. On some operating systems,
    barrier hits generate exceptions that have to be caught by a
    handler that is on the stack. On these operating systems, any code
    that uses memory managed by the MPS must be called from inside
    such an exception handler, that is, inside a call to
    :c:func:`mps_tramp`.

    If you have multiple threads that run code that uses memory
    managed by the MPS, each thread must execute such code inside a
    call to :c:func:`mps_tramp`.


.. index::
   single: trampoline

.. c:type:: void *(*mps_tramp_t)(void *p, size_t s)

    .. deprecated::

        The MPS trampoline is no longer required on any operating
        system supported by the MPS.

    The type of a function called by :c:func:`mps_tramp`.

    ``p`` and ``s`` are the corresponding arguments that were passed
    to :c:func:`mps_tramp`.


.. c:function:: void mps_arena_expose(mps_arena_t arena)

    .. deprecated::

        If you need access to protected memory for debugging,
        :ref:`contact us <contact>`.

    Ensure that the MPS is not protecting any :term:`page` in the
    :term:`arena` with a :term:`read barrier` or :term:`write
    barrier`.

    ``arena`` is the arena to expose.

    This is expected to only be useful for debugging. The arena is
    left in the :term:`clamped state`.

    Since barriers are used during a collection, calling this function
    has the same effect as calling :c:func:`mps_arena_park`: all
    collections are run to completion, and the arena is clamped so
    that no new collections begin. The MPS also uses barriers to
    maintain :term:`remembered sets`, so calling this
    function will effectively destroy the remembered sets and any
    optimization gains from them.

    Calling this function is time-consuming: any active collections
    will be run to completion; and the next collection will have to
    recompute all the remembered sets by scanning the entire arena.

    The recomputation of the remembered sets can be avoided by calling
    :c:func:`mps_arena_unsafe_expose_remember_protection` instead of
    :c:func:`mps_arena_expose`, and by calling
    :c:func:`mps_arena_unsafe_restore_protection` before calling
    :c:func:`mps_arena_release`. Those functions have unsafe aspects
    and place restrictions on what the :term:`client program` can do
    (basically no exposed data can be changed).


.. c:function:: void mps_arena_unsafe_expose_remember_protection(mps_arena_t arena)

    .. deprecated::

        If you need access to protected memory for debugging,
        :ref:`contact us <contact>`.

    Ensure that the MPS is not protecting any :term:`page` in the
    :term:`arena` with a :term:`read barrier` or :term:`write
    barrier`. In addition, request the MPS to remember some parts of its
    internal state so that they can be restored later.

    ``arena`` is the arena to expose.

    This function is the same as :c:func:`mps_arena_expose`, but
    additionally causes the MPS to remember its protection state. The
    remembered protection state can optionally be restored later by
    calling the :c:func:`mps_arena_unsafe_restore_protection` function.
    This is an optimization that avoids the MPS having to recompute
    all the remembered sets by scanning the entire arena.

    However, restoring the remembered protections is only safe if the
    contents of the exposed pages have not been changed; therefore
    this function should only be used if you do not intend to change
    the pages, and the remembered protection must only be restored if
    the pages have not been changed.

    The MPS will only remember the protection state if resources
    (memory) are available. If memory is low then only some or
    possibly none of the protection state will be remembered, with a
    corresponding necessity to recompute it later. The MPS provides no
    mechanism for the :term:`client program` to determine whether the
    MPS has in fact remembered the protection state.

    The remembered protection state, if any, is discarded after
    calling :c:func:`mps_arena_unsafe_restore_protection`, or as soon
    as the arena leaves the :term:`clamped state` by calling
    :c:func:`mps_arena_release`.


.. c:function:: void mps_arena_unsafe_restore_protection(mps_arena_t arena)

    .. deprecated::

        If you need access to protected memory for debugging,
        :ref:`contact us <contact>`.

    Restore the remembered protection state for an :term:`arena`.

    ``arena`` is the arena to restore the protection state for.

    This function restores the protection state that the MPS has
    remembered when the :term:`client program` called
    :c:func:`mps_arena_unsafe_expose_remember_protection`. The purpose
    of remembering and restoring the protection state is to avoid the
    need for the MPS to recompute all the :term:`remembered sets` by
    scanning the entire arena, that occurs when
    :c:func:`mps_arena_expose` is used, and which causes the next
    :term:`garbage collection` to be slow.

    The client program must not change the exposed data between the
    call to :c:func:`mps_arena_unsafe_expose_remember_protection` and
    :c:func:`mps_arena_unsafe_restore_protection`. If the client
    program has changed the exposed data then
    :c:func:`mps_arena_unsafe_restore_protection` must not be called:
    in this case simply call :c:func:`mps_arena_release`.

    Calling this function does not release the arena from the clamped
    state: :c:func:`mps_arena_release` must be called to continue
    normal collections.

    Calling this function causes the MPS to forget the remembered
    protection state; as a consequence the same remembered state
    cannot be restored more than once.

