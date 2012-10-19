.. _topic-scanning:

Scanning
========

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/scanning/index.html



From the documentation for mps_fix:


Returned Values

Returns a result code, see :ref:`topic-error`.

If the :term:`rank` of the object being scanned is not :c:macro:`MPS_RANK_AMBIG` then the reference pointed to by *ref_io* may be modified by :c:func:`mps_fix`.


Description

This function is the part of the scanning protocol used to indicate references. Scanning functions apply it, or :c:func:`MPS_FIX12`, or :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` the references in the object being scanned.

It may only be called from within a scanning function. If it is called within a :c:func:`MPS_SCAN_BEGIN` block, :c:func:`MPS_FIX_CALL` must be used (yes, really).

This function does not perform any particular operation. The MPS may call scanning functions for a number of reasons, and :c:func:`mps_fix` may take different actions depending on those reasons.

::

    mps_res_t scan_array(mps_ss_t ss, mps_addr_t object, size_t length)
    {
        size_t i;
        mps_res_t res;
        mps_addr_t *array = (mps_addr_t *)object;

        for (i = 0; i < length; ++i) {
            res = mps_fix(ss, &array[i]);
            if (res != MPS_RES_OK)
                return res;
        }

        return res;
    }


Error Handling

The function returns :c:macro:`MPS_RES_OK` if it was successful, in which case the scanning function should continue to scan the rest of the object, applying :c:func:`mps_fix` to the remaining references. If :c:func:`mps_fix` returns a value other than :c:macro:`MPS_RES_OK`, the scanning function must return that value, and may return without scanning further references. Generally, it is better if it returns as soon as possible.

::

    mps_res_t scan_array(mps_ss_t ss, Array object, size_t length)
    {
        size_t i;
        mps_res_t res;
        mps_addr_t *array = (mps_addr_t *)object;
        MPS_SCAN_BEGIN(ss) {
            for (i = 0; i < length; ++i) {
                mps_addr_t ref = array[i];
                if (MPS_FIX1(ss, ref)) {
                  /* if (((Object*)ref)->type == ScannableType) { */
                  /* You can do something here, but in the end, you must call MPS_FIX2. */
                  res = MPS_FIX2(ss, &array[i]);
                  if (res != MPS_RES_OK)
                      return res;
                  /* } */
                }
            }
        } MPS_SCAN_END(ss);

        return res;
    }

::

    mps_res_t scan_array(mps_ss_t ss, mps_addr_t object, size_t length) {
        size_t i;
        mps_res_t res;
        mps_addr_t *array = (mps_addr_t *)object;

        MPS_SCAN_BEGIN(ss) {
            for (i = 0; i < length; ++i) {
                res = MPS_FIX(ss, &array[i]);
                if (res != MPS_RES_OK)
                    return res;
            }
        } MPS_SCAN_END(ss);

        return res;
    }


Error Handling

The macro returns :c:macro:`MPS_RES_OK` if it was successful, in which case the scanning function should continue to scan the rest of the object, fixing the remaining references. If :c:func:`MPS_FIX12` returns a value other than :c:macro:`MPS_RES_OK`, the scanning function must return that value, and may return without scanning further references. Generally, it is better if it returns as soon as possible.

::

    /* Scanner for a simple Scheme-like language with just two interesting types */

    mps_res_t scan_objs(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
    {
        mps_res_t res;
        mps_addr_t obj;

        MPS_SCAN_BEGIN(ss) {
            for (obj = base; obj < limit;) { /* obj maps over the objects to scan */
                switch(((Object*)obj)->type) {
                case ArrayType: {
                    size_t i;
                    Array *array = (Array *)obj;

                    for (i = 0; i < array->length; ++i) { /* fix each element */
                        res = MPS_FIX12(ss, &array->contents[i]);
                        if (res != MPS_RES_OK)
                            return res;
                    }

                    obj = AddrAdd(obj, ArraySize(array)); /* move to next object */
                    break;
                }
                case StackFrameType: {
                    StackFrame *frame = (StackFrame *)obj;
                    for (i = frame->size; i > 0; --i) { /* fix each local var */
                        res = MPS_FIX12(ss, &frame->locals[i]);
                        if (res != MPS_RES_OK)
                            return res;
                    }

                    res = MPS_FIX12(ss, &frame->next);
                    if (res != MPS_RES_OK)
                        return res;
                    obj = AddrAdd(obj, StackFrameSize(frame));
                    break;
                }
                default: /* other types don't contain references */
                    obj = AddrAdd(obj, DefaultSize(obj));
                    break;
                }
            }
        } MPS_SCAN_END(ss);

        return res;
    }


Interface
---------

.. c:function:: mps_res_t mps_fix(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This function must only be called from within a :term:`scan
    method`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated (see the topic
    :ref:`topic-moving`), and the scan method must continue to scan
    the :term:`block`. If it returns any other result, the scan method
    must return that result as soon as possible, without fixing any
    further references.

    .. deprecated:: 1.110

        Use :c:func:`MPS_SCAN_BEGIN`, :c:func:`MPS_FIX12`, and
        :c:func:`MPS_SCAN_END` instead.

    .. topics::

        :ref:`topic-scanning` and :ref:`topic-moving`.

    .. note::

        If your reference is :term:`tagged <tagged reference>`, you
        must remove the tag before calling :c:func:`mps_fix`, and
        restore the tag to the (possibly updated) reference
        afterwards. (There is an exception for references to objects
        belonging to a format of variant auto_header: these references
        must not subtract the header size.)

        If you want to call this between :c:func:`MPS_SCAN_BEGIN` and
        :c:func:`MPS_SCAN_END`, you must use :c:func:`MPS_FIX_CALL`
        to ensure that the scan state is passed correctly.


.. c:function:: mps_bool_t MPS_FIX1(mps_ss_t ss, mps_addr_t ref)

    Tell the MPS about a :term:`reference`. This macro must only be
    used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref`` is the reference.

    Returns a truth value (:c:type:`mps_bool_t`) indicating whether
    the reference is likely to be interesting to the MPS. If it
    returns false, the scan method must continue scanning the
    :term:`block`. If it returns true, the scan method must invoke
    :c:func:`MPS_FIX2`, to fix the reference.

    .. note::

        If your reference is :term:`tagged <tagged reference>`, you
        must remove the tag before calling :c:func:`MPS_FIX1`.

        In the common case where the scan method does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: MPS_FIX12(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated (see the topic
    :ref:`topic-moving`), and the scan method must continue to scan
    the :term:`block`. If it returns any other result, the scan method
    must return that result as soon as possible, without fixing any
    further references.

    .. note::

        If your reference is :term:`tagged <tagged reference>`, you
        must remove the tag before calling :c:func:`MPS_FIX2`, and
        restore the tag to the (possibly updated) reference
        afterwards. (There is an exception for references to objects
        belonging to a format of variant auto_header: these references
        must not subtract the header size.)

        The macro :c:func:`MPS_FIX12` is a convenience for the common
        case where :c:func:`MPS_FIX1` is immediately followed by
        :c:func:`MPS_FIX2`.


.. c:function:: MPS_FIX2(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan method`,
    between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated (see the topic
    :ref:`topic-moving`), and the scan method must continue to scan
    the :term:`block`. If it returns any other result, the scan method
    must return that result as soon as possible, without fixing any
    further references.

    .. note::

        If your reference is :term:`tagged <tagged reference>`, you
        must remove the tag before calling :c:func:`MPS_FIX2`, and
        restore the tag to the (possibly updated) reference
        afterwards. (There is an exception for references to objects
        belonging to a format of variant auto_header: these references
        must not subtract the header size.)

        In the common case where the scan method does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: MPS_FIX_CALL(ss, call)

    Call a function from within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, passing
    the :term:`scan state` correctly.

    ``ss`` is the scan state that was passed to the scan method.

    ``call`` is an expression containing a function call where ``ss``
    is one of the arguments.

    Returns the result of evaluating the expression ``call``.

    Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
    scan state is in a special state, and must not be passed to a
    function. If you really need to do so, for example because you
    have a structure shared between two :term:`object formats <object
    format>`, you must wrap the call with :c:func:`MPS_FIX_CALL` to
    ensure that the scan state is passed correctly.

    In example below, the scan method ``obj_scan`` fixes the object's
    ``left`` and ``right`` references, but delegates the scanning of
    references inside the object's ``data`` member to the function
    ``data_scan``. In order to ensure that the scan state is passed
    correctly to ``data_scan``, the call must be wrapped in
    :c:func:`MPS_FIX_CALL`. ::

        mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
        {
            Object *obj;
            mps_res_t res;
            MPS_SCAN_BEGIN(ss) {
                for (obj = base; obj < limit; obj++) {
                    if (MPS_FIX12(ss, &obj->left) != MPS_RES_OK)
                        return res;
                    MPS_FIX_CALL(ss, res = data_scan(ss, &obj->data));
                    if (res != MPS_RES_OK)
                        return res;
                    if (MPS_FIX12(ss, &obj->right) != MPS_RES_OK)
                        return res;
                }
            } MPS_SCAN_END(ss);
            return MPS_RES_OK;
        }


.. c:function:: MPS_SCAN_BEGIN(mps_ss_t ss)

    Within a :term:`scan method`, set up local information required
    by :c:func:`MPS_FIX1`, :c:func:`MPS_FIX2` and
    :c:func:`MPS_FIX12`. The local information persists until
    :c:func:`MPS_SCAN_END`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    .. note::

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`,
        the scan state is in a special state, and must not be passed
        to a function. If you really need to do so, for example
        because you have an embedded structure shared between two scan
        methods, you must wrap the call with :c:func:`MPS_FIX_CALL` to
        ensure that the scan state is passed correctly.


.. c:function:: MPS_SCAN_END(mps_ss_t ss)

    Within a :term:`scan method`, terminate a block started by
    :c:func:`MPS_SCAN_BEGIN`.

    ``ss`` is the :term:`scan state` that was passed to the scan
    method.

    .. note::

        :c:func:`MPS_SCAN_END` ensures that the scan is completed, so
        successful termination of a scan must invoke it. However, in
        case of an error it is allowed to return from the scan
        method without invoking :c:func:`MPS_SCAN_END`.

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
        scan state is in a special state, and must not be passed to a
        function. If you really need to do so, for example because you
        have an embedded structure shared between two scan methods, you
        must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
        scan state is passed correctly.


.. c:type:: mps_ss_t

    The type of :term:`scan states <scan state>`.

    A scan state represents the state of the current :term:`scan`. The
    MPS passes a scan state to the :term:`scan method` of an
    :term:`object format` when it needs to :term:`scan` for
    :term:`references <reference>` within a region of memory. The
    scan method must pass the scan state to :c:func:`MPS_SCAN_BEGIN`
    and :c:func:`MPS_SCAN_END` to delimit a sequence of fix
    operations, and to the functions :c:func:`MPS_FIX1` and
    :c:func:`MPS_FIX2` when fixing a :term:`reference`.


