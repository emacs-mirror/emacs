.. index::
   pair: arguments; keyword

.. _topic-keyword:

Keyword arguments
-----------------

Some functions in the MPS interface take :term:`keyword arguments` in
order to pass values that might be optional, or are only required in
some circumstances. For example, the function
:c:func:`mps_arena_create_k` creates any class of :term:`arena`, but
:term:`client arenas` require you to specify a base address. These
arguments are passed in a keyword argument array, like this::

    mps_res_t res;
    mps_arena_t arena;
    mps_arg_s args[3];
    args[0].key = MPS_KEY_ARENA_SIZE;
    args[0].val.size = 6553600;
    args[1].key = MPS_KEY_ARENA_CL_BASE;
    args[1].val.addr = base_address;
    args[2].key = MPS_KEY_ARGS_END;
    res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);

Each keyword argument in the array is a structure of type
:c:type:`mps_arg_s`.

For convenience and robustness, the MPS interface includes macros to
help with forming keyword argument lists::

    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 6553600);
        MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_BASE, base_address);
        res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
    } MPS_ARGS_END(args);

The argument array must not be ``NULL``, and must end with
:c:macro:`MPS_KEY_ARGS_END`. If you don't want to pass any arguments,
you can pass :c:macro:`mps_args_none`.

When a function that takes keyword arguments returns, the keyword
argument array has been *modified* to remove any arguments that have
been used. If all arguments have been used, the first element key is
now :c:macro:`MPS_KEY_ARGS_END`.


.. c:type:: mps_arg_s

    The type of the structure used to represent a single
    :term:`keyword argument` to a function. ::

        typedef struct mps_arg_s {
            mps_key_t key;
            union { /* many fields; see table below */ } val;
        } mps_arg_s;

    ``key`` identifies the key. It must be one of the values listed in
    the documentation for the type :c:type:`mps_key_t`.

    ``val`` is the corresponding value. This union contains many
    fields: one for each keyword argument type. The table given in the
    documentation for :c:type:`mps_key_t` below indicates which
    structure field is used by each keyword.

    .. note::

        If you use the convenience macro :c:func:`MPS_ARGS_ADD` then
        you don't need to know the name of the field.


.. c:macro:: mps_args_none

    An array of :c:type:`mps_arg_s` representing the empty list of
    keyword arguments. Equivalent to::

        mps_arg_s mps_args_none[] = {{MPS_KEY_ARGS_END}};


.. c:type:: mps_key_t

    The type of :term:`keyword argument` keys. Must take one of the
    following values:

    ======================================== ========================================================= ==========================================================
    Keyword                                  Type & field in ``arg.val``                               See
    ======================================== ========================================================= ==========================================================
    :c:macro:`MPS_KEY_ARGS_END`              *none*                                                    *see above*
    :c:macro:`MPS_KEY_ALIGN`                 :c:type:`mps_align_t`             ``align``               :c:func:`mps_class_mv`, :c:func:`mps_class_mvff`, :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` :c:type:`mps_bool_t`              ``b``                   :c:func:`mps_class_ams`
    :c:macro:`MPS_KEY_ARENA_CL_BASE`         :c:type:`mps_addr_t`              ``addr``                :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_COMMIT_LIMIT`          :c:type:`size_t`                  ``size``                :c:func:`mps_arena_class_vm`, :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_ARENA_GRAIN_SIZE`      :c:type:`size_t`                  ``size``                :c:func:`mps_arena_class_vm`, :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_ARENA_SIZE`            :c:type:`size_t`                  ``size``                :c:func:`mps_arena_class_vm`, :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_SPARE_COMMIT_LIMIT`    :c:type:`size_t`                  ``size``                :c:func:`mps_arena_class_vm`, :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_AWL_FIND_DEPENDENT`    ``void *(*)(void *)``             ``addr_method``         :c:func:`mps_class_awl`
    :c:macro:`MPS_KEY_CHAIN`                 :c:type:`mps_chain_t`             ``chain``               :c:func:`mps_class_amc`, :c:func:`mps_class_amcz`, :c:func:`mps_class_ams`, :c:func:`mps_class_awl`, :c:func:`mps_class_lo`
    :c:macro:`MPS_KEY_EXTEND_BY`             :c:type:`size_t`                  ``size``                :c:func:`mps_class_amc`, :c:func:`mps_class_amcz`, :c:func:`mps_class_mfs`, :c:func:`mps_class_mv`, :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_FMT_ALIGN`             :c:type:`mps_align_t`             ``align``               :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_CLASS`             :c:type:`mps_fmt_class_t`         ``fmt_class``           :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_FWD`               :c:type:`mps_fmt_fwd_t`           ``fmt_fwd``             :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_HEADER_SIZE`       :c:type:`size_t`                  ``size``                :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_ISFWD`             :c:type:`mps_fmt_isfwd_t`         ``fmt_isfwd``           :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_PAD`               :c:type:`mps_fmt_pad_t`           ``fmt_pad``             :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_SCAN`              :c:type:`mps_fmt_scan_t`          ``fmt_scan``            :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FMT_SKIP`              :c:type:`mps_fmt_skip_t`          ``fmt_skip``            :c:func:`mps_fmt_create_k`
    :c:macro:`MPS_KEY_FORMAT`                :c:type:`mps_fmt_t`               ``format``              :c:func:`mps_class_amc`, :c:func:`mps_class_amcz`, :c:func:`mps_class_ams`, :c:func:`mps_class_awl`, :c:func:`mps_class_lo` , :c:func:`mps_class_snc`
    :c:macro:`MPS_KEY_GEN`                   :c:type:`unsigned`                ``u``                   :c:func:`mps_class_ams`, :c:func:`mps_class_awl`, :c:func:`mps_class_lo`
    :c:macro:`MPS_KEY_INTERIOR`              :c:type:`mps_bool_t`              ``b``                   :c:func:`mps_class_amc`, :c:func:`mps_class_amcz`
    :c:macro:`MPS_KEY_MAX_SIZE`              :c:type:`size_t`                  ``size``                :c:func:`mps_class_mv`
    :c:macro:`MPS_KEY_MEAN_SIZE`             :c:type:`size_t`                  ``size``                :c:func:`mps_class_mv`, :c:func:`mps_class_mvt`, :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MFS_UNIT_SIZE`         :c:type:`size_t`                  ``size``                :c:func:`mps_class_mfs`
    :c:macro:`MPS_KEY_MIN_SIZE`              :c:type:`size_t`                  ``size``                :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_MVFF_ARENA_HIGH`       :c:type:`mps_bool_t`              ``b``                   :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MVFF_FIRST_FIT`        :c:type:`mps_bool_t`              ``b``                   :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MVFF_SLOT_HIGH`        :c:type:`mps_bool_t`              ``b``                   :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MVT_FRAG_LIMIT`        :c:type:`mps_word_t`              ``count``               :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_MVT_RESERVE_DEPTH`     :c:type:`mps_word_t`              ``count``               :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS`    :c:type:`mps_pool_debug_option_s` ``*pool_debug_options`` :c:func:`mps_class_ams_debug`, :c:func:`mps_class_mv_debug`, :c:func:`mps_class_mvff_debug`
    :c:macro:`MPS_KEY_RANK`                  :c:type:`mps_rank_t`              ``rank``                :c:func:`mps_class_ams`, :c:func:`mps_class_awl`, :c:func:`mps_class_snc`
    :c:macro:`MPS_KEY_SPARE`                 :c:type:`double`                  ``d``                   :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_VMW3_TOP_DOWN`         :c:type:`mps_bool_t`              ``b``                   :c:func:`mps_arena_class_vm`
    ======================================== ========================================================= ==========================================================


.. c:function:: MPS_ARGS_BEGIN(args)

    Start construction of a list of keyword arguments. This macro must
    be used like this::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 6553600);
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_BASE, base_address);
            res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
        } MPS_ARGS_END(args);

    That is, you must call :c:func:`MPS_ARGS_ADD` (or
    :c:func:`MPS_ARGS_ADD_FIELD`) zero or more times, and then pass
    the arguments to a function.

    ``args`` is the name of the array that contains the keyword
    arguments. The array is stack-allocated, and exists between
    :c:macro:`MPS_ARGS_BEGIN` and :c:macro:`MPS_ARGS_END`.

    It is safe to nest blocks created by :c:macro:`MPS_ARGS_BEGIN` and
    :c:macro:`MPS_ARGS_END`.


.. c:function:: MPS_ARGS_ADD(mps_arg_s args[], mps_key_t key, value)

    Add an argument to a list of keyword arguments. This macro must be
    used only between :c:macro:`MPS_ARGS_BEGIN` and
    :c:macro:`MPS_ARGS_END`.

    ``args`` is the name of array that contains the keyword arguments.
    It must match the argument to the preceding call to
    :c:func:`MPS_ARGS_BEGIN`.

    ``key`` is the keyword identifying this argument. It must be one
    of the key names starting with ``MPS_KEY_`` that are listed in the
    table in the documentation for :c:type:`mps_key_t`.

    ``value`` is the value for this argument.


.. c:function:: MPS_ARGS_ADD_FIELD(mps_arg_s args[], mps_key_t key, field, value)

    Add an argument to a list of keyword arguments. This macro must be
    used only between :c:macro:`MPS_ARGS_BEGIN` and
    :c:macro:`MPS_ARGS_END`.

    ``args`` is the name of array that contains the keyword arguments.
    It must match the argument to the preceding call to
    :c:func:`MPS_ARGS_BEGIN`.

    ``key`` is the keyword identifying this argument.

    ``field`` is the name of the field in the ``val`` union in the
    structure :c:type:`mps_args_s`.

    ``value`` is the value for this argument.

    .. note::

        You should prefer to use :c:func:`MPS_ARGS_ADD`, because then
        you don't need to look up the name of the field.


.. c:function:: MPS_ARGS_END(args)

    Finish using a list of keyword arguments whose construction was
    started by :c:func:`MPS_ARGS_BEGIN`.

    ``args`` is the name of array that contains the keyword arguments.
    It must match the argument to the preceding call to
    :c:func:`MPS_ARGS_BEGIN`.
