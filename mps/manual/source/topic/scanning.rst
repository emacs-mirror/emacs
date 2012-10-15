.. _topic-scanning:

========
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
