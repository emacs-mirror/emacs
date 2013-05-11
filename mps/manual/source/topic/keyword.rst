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
    args[1].key = MPS_KEY_ARENA_CL_ADDR;
    args[1].val.addr = base_address;
    args[2].key = MPS_KEY_ARGS_END;
    res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);

Each keyword argument in the array is a structure of type
:c:type:`mps_arg_s`.

For convenience and robustness, the MPS interface includes macros to
help with forming keyword argument lists::

    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size, 6553600);
        MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_ADDR, addr, base_address);
        MPS_ARGS_DONE(args);
        res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
    } MPS_ARGS_END(args);
    
But if you are writing :term:`C99`, you can write this more concisely as::

    mps_res_t res;
    mps_arena_t arena;
    res = mps_arena_create_k(&arena, mps_arena_class_cl(),
           (mps_arg_s[]){{MPS_KEY_ARENA_SIZE,    .val.size = 6553600},
                         {MPS_KEY_ARENA_CL_ADDR, .val.addr = base_address},
                         {MPS_KEY_ARGS_END}});

The argument array must not be ``NULL``, and must end with
:c:macro:`MPS_KEY_ARGS_END`.  If you don't want to pass any arguments, you can
either call the equivalent function that does not take keyword arguments
(named without the ``_k``) or pass :c:macro:`mps_args_none`.

When a function that takes keyword arguments returns, the keyword
argument array has been *modified* to remove any arguments that have
been used. If all arguments have been used, the first element key is
now :c:macro:`MPS_KEY_ARGS_END`.


.. c:type:: mps_arg_s

    The type of the structure used to represent a single
    :term:`keyword argument` to a function. ::

        typedef struct mps_arg_s {
            mps_key_t key;
            union {
                mps_bool_t b;
                char c;
                const char *string;
                int i;
                unsigned u;
                long l;
                unsigned long ul;
                size_t size;
                mps_addr_t addr;
                mps_fmt_t format;
                mps_chain_t chain;
                struct mps_pool_debug_option_s *pool_debug_options;
                mps_addr_t (*addr_method)(mps_addr_t);
                mps_align_t align;
                mps_word_t count;
                void *p;
                mps_rank_t rank;
            } val;
        } mps_arg_s;

    ``key`` identifies the key. It must be one of the legal values
    of :c:type:`mps_key_t` listed in the documentation for that type.

    ``val`` is the corresponding value. The documentation for each
    value of the type :c:type:`mps_key_t` explains which structure
    member is used by that keyword.


.. c:macro:: mps_args_none

    An array of :c:type:`mps_arg_s` representing the empty list of
    keyword arguments. Equivalent to::

        mps_arg_s mps_args_none[] = {{MPS_KEY_ARGS_END}};


.. c:type:: mps_key_t

    The type of :term:`keyword argument` keys. Must take one of the
    following values:

    ======================================== ====================== ==========================================================
    Keyword                                  Value slot             See
    ======================================== ====================== ==========================================================
    :c:macro:`MPS_KEY_ARGS_END`              *none*                 *see above*
    :c:macro:`MPS_KEY_ALIGN`                 ``align``              :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` ``b``                  :c:func:`mps_class_ams`
    :c:macro:`MPS_KEY_ARENA_CL_ADDR`         ``addr``               :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_ARENA_SIZE`            ``size``               :c:func:`mps_arena_class_vm`, :c:func:`mps_arena_class_cl`
    :c:macro:`MPS_KEY_AWL_FIND_DEPENDENT`    ``addr_method``        :c:func:`mps_class_awl`
    :c:macro:`MPS_KEY_CHAIN`                 ``chain``              :c:func:`mps_class_amc`, :c:func:`mps_class_amcz`, :c:func:`mps_class_ams`
    :c:macro:`MPS_KEY_EXTEND_BY`             ``size``               :c:func:`mps_class_mfs`, :c:func:`mps_class_mv`, :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_FORMAT`                ``format``             :c:func:`mps_class_amc`, :c:func:`mps_class_amcz`, :c:func:`mps_class_ams`, :c:func:`mps_class_awl`, :c:func:`mps_class_lo` , :c:func:`mps_class_snc`
    :c:macro:`MPS_KEY_MAX_SIZE`              ``size``               :c:func:`mps_class_mv`
    :c:macro:`MPS_KEY_MEAN_SIZE`             ``size``               :c:func:`mps_class_mv`, :c:func:`mps_class_mvt`, :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MFS_UNIT_SIZE`         ``size``               :c:func:`mps_class_mfs`
    :c:macro:`MPS_KEY_MIN_SIZE`              ``size``               :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_MVFF_ARENA_HIGH`       ``b``                  :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MVFF_FIRST_FIT`        ``b``                  :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MVFF_SLOT_HIGH`        ``b``                  :c:func:`mps_class_mvff`
    :c:macro:`MPS_KEY_MVT_FRAG_LIMIT`        ``count``              :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_MVT_RESERVE_DEPTH`     ``count``              :c:func:`mps_class_mvt`
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS`    ``pool_debug_options`` :c:func:`mps_class_ams_debug`, :c:func:`mps_class_mv_debug`, :c:func:`mps_class_mvff_debug`
    :c:macro:`MPS_KEY_RANK`                  ``rank``               :c:func:`mps_class_awl`, :c:func:`mps_class_snc`
    :c:macro:`MPS_KEY_VMW3_TOP_DOWN`         ``b``                  :c:func:`mps_arena_class_vm`
    ======================================== ====================== ==========================================================


.. c:function:: MPS_ARGS_BEGIN(args)

    Start construction of a list of keyword arguments. This macro must
    be used like this::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size, 6553600);
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_ADDR, addr, base_address);
            MPS_ARGS_DONE(args);
            res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
        } MPS_ARGS_END(args);

    That is, you must call :c:func:`MPS_ARGS_ADD` zero or more times,
    and then call :c:func:`MPS_ARGS_DONE` before passing the arguments
    to a function.

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


.. c:function:: MPS_ARGS_DONE(args)

    Finalize a list of keyword arguments. This macro must be used only
    between :c:macro:`MPS_ARGS_BEGIN` and :c:macro:`MPS_ARGS_END`.

    ``args`` is the name of array that contains the keyword arguments.
    It must match the argument to the preceding call to
    :c:func:`MPS_ARGS_BEGIN`.

    After calling this macro, the array ``args`` is ready to pass to a
    function.


.. c:function:: MPS_ARGS_END(args)

    Finish using a list of keyword arguments whose construction was
    started by :c:func:`MPS_ARGS_BEGIN`.

    ``args`` is the name of array that contains the keyword arguments.
    It must match the argument to the preceding call to
    :c:func:`MPS_ARGS_BEGIN`.
