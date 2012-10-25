.. _topic-scanning:

Scanning
========

:term:`Scanning <scan>` is the process of identifying the
:term:`references <reference>` in a block of memory and
:term:`"fixing" <fix>` them. It's the process at the heart of the
Memory Pool System, and the most critical of the memory management
functions that have to be implemented by the :term:`client program`.

Scanning is performed for two tasks: during :term:`tracing <trace>`
block are scanned in order to references and so determine which blocks
are :term:`reachable` and which are not. After objects have been moved
in memory, blocks are scanned in order to identify references that
need to be updated to point to the new locations of these objects.
Both tasks use the same scanning protocol, described here.


.. _topic-scanning-protocol:

Scanning protocol
-----------------

There are several types of scanning functions (the :term:`scan method`
in an :term:`object format`, of type :c:type:`mps_fmt_scan_t`, and
root scanning functions of various types) but all take a :term:`scan
state` argument of type :c:type:`mps_ss_t`, and a description of a
region to be scanned. They must carry out the following steps:

1. Call the macro :c:func:`MPS_SCAN_BEGIN` on the scan state.

2. For each reference in the region:

   1. Call :c:func:`MPS_FIX1`, passing the scan state and the
      reference.

   2. If :c:func:`MPS_FIX1` returns false, the reference is not of
      interest to the MPS. Proceed to the next reference in the
      region.

   3. If :c:func:`MPS_FIX1` returns true, the reference is of interest
      to the MPS. Call :c:func:`MPS_FIX2`, passing the scan state and
      a pointer to a location containing the reference.

   4. If :c:func:`MPS_FIX2` returns a :term:`result code` other than
      :c:func:`MPS_RES_OK`, return this result code from the scanning
      function as soon as practicable.

   5. If :c:func:`MPS_FIX2` returns :c:macro:`MPS_RES_OK`, it may have
      updated the reference. If necessary, make sure that the updated
      reference is stored back to the region being scanned.

3. Call the macro :c:func:`MPS_SCAN_END` on the scan state.

4. Return :c:macro:`MPS_RES_OK`.

This description of the protocol simplifies a number of important
details, which are covered in the following sections.


Tagged references
-----------------

If your references are :term:`tagged <tagged reference>` (or otherwise
"encrypted"), then you must remove the tag (or decrypt them) before
passing them to :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

The reference passed to :c:func:`MPS_FIX2` must be the address of the
base of the block referred to (unless the referent belongs to an
:term:`object format` of variant auto_header, in which case it must be
a reference to the address just after the header).

However, :c:func:`MPS_FIX1` allows some leeway: if you pass it a
reference to the interior of an allocated block, then
:c:func:`MPS_FIX1` correctly determines whether a reference to the
block is of interest to the MPS.

This means that if your tag is in the low bits of the reference, you
may not have to remove it before calling :c:func:`MPS_FIX1`. For
example, if you use three tag bits, then your reference is at most
*base* + 7, and if your objects are at least 8 bytes long, then the
reference is within the object and need not be stripped. So your code
might look like this::

    if (MPS_FIX1(ss, obj->ref)) {
        /* strip the tag */
        mps_addr_t p = obj->ref & ~0x7;
        mps_res_t res = MPS_FIX2(ss, &p);
        if (res != MPS_RES_OK) return res;
        /* restore the tag and update reference */
        mps_word_t tag = obj->ref & 0x7;
        obj->ref = (obj_t)((char *)p + tag);
    }

This saves the cost of stripping the tag in the case that ``obj->ref``
is not of interest to the MPS.

Similarly, if you use interior pointers, you do not need to convert
them to base pointers before calling :c:func:`MPS_FIX1` (or, indeed,
before calling :c:func:`MPS_FIX2`, if the target of the referent
belongs to an :term:`object format` of variant auto_header).


Critical path
-------------

Scanning is an operation on the critical path of the MPS and so it is
vital that it runs fast. The scanning protocol is designed to ensure
that as much of the scanning code can be run inline in the client
program as possible. In particular, the macro :c:func:`MPS_FIX1` does
not need to call into the MPS.

The purpose of :c:func:`MPS_FIX1` is to provide a fast check as to
whether a reference is "of interest" to the MPS. It is legitimate to
call this on any word: it does not even have to be an address. So if
you have a mixture of references and non-references, it might turn out
to be faster to call :c:func:`MPS_FIX1` on each word before you even
determine whether or not the word is a reference.

Whether this is in fact an optimization depends on the proportion of
references to non-references, on how often genuine references turn out
to be "of interest", and what kind of code the compiler has
generated. There is no substitute for measurement.

.. note::

    In one application with a high proportion of :term:`unboxed`
    values, it turned out to be fastest to check the tag and reject
    non-references before calling :c:func:`MPS_FIX1`.

.. warning::

    If you passed a word that might not be a reference to
    :c:func:`MPS_FIX1`, and it returned true, this might be a false
    positive. You must be certain that the alleged reference is
    genuine as well as "of interest" before passing it to
    :c:func:`MPS_FIX2`.

Another technique that can speed up scanning is to segregate objects
into pools whose object formats contain different scan methods. In
particular, if you can segregate objects that do not contain any
references into :term:`leaf object` pools like :ref:`pool-amcz`, these
objects do not need to be scanned at all.


Ambiguous references
--------------------

If the references in the object being scanned are :term:`ambiguous
<ambiguous reference>` then :c:func:`MPS_FIX2` does not update the
reference (because it can't know if it's a genuine reference). The MPS
handles an ambiguous reference by :term:`pinning` the block pointed to
so that it cannot move.

You could use this fact to optimize the scan by avoiding the need to
reassemble and store the updated reference after calling
:term:`MPS_FIX2`

.. note::

    The MPS currently has no pools that support ambiguous references,
    so this cannot arise for the :term:`scan method` in an
    :term:`object format`, but :term:`root` scanning functions may
    encounter this case.


Example: Scheme objects
-----------------------

Scanning tends to be a repetitive procedure and so you'll find it is
usually helpful to define macros to reduce the size of the source
code. The MPS provides a convenience macro :c:func:`MPS_FIX12` for the
common case of calling :c:func:`MPS_FIX1` and then immediately calling
:c:func:`MPS_FIX2` if the reference is "of interest".

.. warning::

    Some compilers generate better code if you use
    :c:func:`MPS_FIX12`, and some if you use :c:func:`MPS_FIX1` and
    :c:func:`MPS_FIX2`. There's no substitute for measurement.

Here's the macro ``FIX`` defined by the toy Scheme interpreter::

    #define FIX(ref)                                                        \
        do {                                                                \
            mps_addr_t _addr = (ref); /* copy to local to avoid type pun */ \
            mps_res_t res = MPS_FIX12(ss, &_addr);                          \
            if (res != MPS_RES_OK) return res;                              \
            (ref) = _addr;                                                  \
        } while(0)

.. note::

    The comment refers to a temptation to write non-portable code that
    presents itself here. :c:func:`MPS_FIX2` takes a pointer to a
    location containing the reference (an argument of type
    ``mps_addr_t *``). It is tempting to take the address of the
    reference and cast it to this type. This is undefined by the C
    standard. See :ref:`topic-interface-pun`.

Here's the Scheme scanner::

    static mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
    {
        MPS_SCAN_BEGIN(ss) {
            while (base < limit) {
                obj_t obj = base;
                switch (obj->type.type) {
                    case TYPE_PAIR:
                        FIX(obj->pair.car);
                        FIX(obj->pair.cdr);
                        base = (char *)base + ALIGN(sizeof(pair_s));
                        break;
                    case TYPE_VECTOR: {
                        size_t i;
                        for (i = 0; i < obj->vector.length; ++i)
                            FIX(obj->vector.vector[i]);
                        base = (char *)base +
                            ALIGN(offsetof(vector_s, vector) +
                                  obj->vector.length * sizeof(obj->vector.vector[0]));
                        break;
                    }
                    /* ... and so on for the other types ... */
                    default:
                        assert(0);
                        fprintf(stderr, "Unexpected object on the heap\n");
                        abort();
                        return MPS_RES_FAIL;
                }
            }
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }


Scanning interface
------------------

.. c:type:: mps_ss_t

    The type of :term:`scan states <scan state>`.

    A scan state represents the state of the current :term:`scan`. The
    MPS passes a scan state to the :term:`scan method` of an
    :term:`object format` when it needs to :term:`scan` for
    :term:`references <reference>` within a region of memory. The scan
    method must pass the scan state to :c:func:`MPS_SCAN_BEGIN` and
    :c:func:`MPS_SCAN_END` to delimit a sequence of fix operations,
    and to the functions :c:func:`MPS_FIX1`, :c:func:`MPS_FIX2` and
    :c:func:`MPS_FIX12` when fixing a :term:`reference`.


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

    .. note::

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
        scan state is in a special state, and must not be passed to a
        function. If you really need to do so, for example because you
        have an embedded structure shared between two scan methods, you
        must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
        scan state is passed correctly.


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
            obj_t obj;
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


Fixing interface
----------------

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

    .. note::

        In the common case where the scan method does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: mps_res_t MPS_FIX12(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated, and the scan method must continue
    to scan the :term:`block`. If it returns any other result, the
    scan method must return that result as soon as possible, without
    fixing any further references.

    .. note::

        If your reference is :term:`tagged <tagged reference>`, you
        must remove the tag before calling :c:func:`MPS_FIX2`, and
        restore the tag to the (possibly updated) reference
        afterwards. (There is an exception for references to objects
        belonging to a format of variant auto_header: these references
        must not subtract the header size.)

    .. note::

        The macro :c:func:`MPS_FIX12` is a convenience for the common
        case where :c:func:`MPS_FIX1` is immediately followed by
        :c:func:`MPS_FIX2`.


.. c:function:: mps_res_t MPS_FIX2(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan method`,
    between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated, and the scan method must continue
    to scan the :term:`block`. If it returns any other result, the
    scan method must return that result as soon as possible, without
    fixing any further references.

    .. note::

        If your reference is :term:`tagged <tagged reference>`, you
        must remove the tag before calling :c:func:`MPS_FIX2`, and
        restore the tag to the (possibly updated) reference
        afterwards. (There is an exception for references to objects
        belonging to a format of variant auto_header: these references
        must not subtract the header size.)

    .. note::

        In the common case where the scan method does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: mps_res_t mps_fix(mps_ss_t ss, mps_addr_t *ref_io)

    .. deprecated:: 1.111

        Use :c:func:`MPS_SCAN_BEGIN`, :c:func:`MPS_FIX12` (or
        :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`), and
        :c:func:`MPS_SCAN_END` instead.

    Tell the MPS about a :term:`reference`, and possibly update it.
    This function must only be called from within a :term:`scan
    method`.

    ``ss`` is the :term:`scan state` that was passed to the scan method.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated, and the scan method must continue
    to scan the :term:`block`. If it returns any other result, the
    scan method must return that result as soon as possible, without
    fixing any further references.

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
