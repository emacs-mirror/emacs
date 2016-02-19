.. Sources:

     `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/scanning/>`_

.. index::
   single: scanning; introduction
   single: object format; scan method

.. _topic-scanning:

Scanning
========

:term:`Scanning <scan>` is the process of identifying the
:term:`references` in a block of memory and
:term:`"fixing" <fix>` them. It's the process at the heart of the
Memory Pool System, and the most critical of the memory management
functions that have to be implemented by the :term:`client program`.

Scanning is performed for two tasks: during :term:`tracing <trace>`,
blocks are scanned in order to follow references, and so determine
which blocks are :term:`reachable` and which are not. After objects
have been moved in memory, blocks are scanned in order to identify
references that need to be updated to point to the new locations of
these objects. Both tasks use the same scanning protocol, described
here.


.. index::
   single: scanning; protocol

.. _topic-scanning-protocol:

Scanning protocol
-----------------

There are several types of scanning functions (the :term:`scan method`
in an :term:`object format`, of type :c:type:`mps_fmt_scan_t`, and
root scanning functions of various types) but all take a :term:`scan
state` argument of type :c:type:`mps_ss_t`, and a description of a
region to be scanned. They must carry out the following steps:

#. Call the macro :c:func:`MPS_SCAN_BEGIN` on the scan state.

#. For each reference in the region:

   #. Call :c:func:`MPS_FIX1`, passing the scan state and the
      reference.

   #. If :c:func:`MPS_FIX1` returns false, the reference is not of
      interest to the MPS. Proceed to the next reference in the
      region.

   #. If :c:func:`MPS_FIX1` returns true, the reference is of interest
      to the MPS. Call :c:func:`MPS_FIX2`, passing the scan state and
      a pointer to a location containing the reference.

   #. If :c:func:`MPS_FIX2` returns a :term:`result code` other than
      :c:macro:`MPS_RES_OK`, return this result code from the scanning
      function as soon as practicable.

   #. If :c:func:`MPS_FIX2` returns :c:macro:`MPS_RES_OK`, it may have
      updated the reference. Make sure that the updated reference is
      stored back into the region being scanned.

#. Call the macro :c:func:`MPS_SCAN_END` on the scan state.

#. Return :c:macro:`MPS_RES_OK`.

This description of the protocol simplifies a number of important
details, which are covered in the following sections.


.. index::
   pair: scanning; tagged reference

.. _topic-scanning-tag:

Tagged references
-----------------

If your references are :term:`tagged <tagged reference>` (or otherwise
"encrypted"), then you must remove the tag (or decrypt them) before
passing them to :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

The reference passed to :c:func:`MPS_FIX2` must be the address of the
base of the block referred to (unless the referent belongs to an
:term:`object format` with :term:`in-band headers`, in which case it
must be a reference to the address just after the header).

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
belongs to an :term:`object format` with :term:`in-band headers`).


.. index::
   pair: scanning; critical path

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

See :ref:`design-critical-path`.

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


.. index::
   pair: scanning; ambiguous reference

Ambiguous references
--------------------

If the references in the object being scanned are :term:`ambiguous
<ambiguous reference>` then :c:func:`MPS_FIX2` does not update the
reference (because it can't know if it's a genuine reference). The MPS
handles an ambiguous reference by :term:`pinning` the block pointed to
so that it cannot move.

You could use this fact to optimize the scan by avoiding the need to
reassemble and store the updated reference after calling
:c:func:`MPS_FIX2`.

.. note::

    The MPS currently has no pools that support ambiguous references,
    so this cannot arise for the :term:`scan method` in an
    :term:`object format`, but :term:`root` scanning functions may
    encounter this case.


.. index::
   pair: scanning; unfixed reference

Unfixed references
------------------

The MPS does not require you to :term:`fix` all your :term:`references`. But if a reference is not fixed:

#. it does not keep its target alive (this might be acceptable if you
   know that the target is being kept alive for another reason, for
   example if it is in a :term:`manually managed <manual memory
   management>` pool, or if there is always another reference to the
   target that *is* fixed);

#. it does not get updated if the target moves (this might be
   acceptable if you know that the target cannot move, for example if
   it is in a :term:`non-moving <non-moving memory manager>` pool, or
   if it is :term:`pinned <pinning>` by an :term:`ambiguous
   reference`).

These optimizations can be tricky to make correct, and can make the
system fragile (for example, it may break if you start using a
different :term:`pool class`), so it is usually safest to fix all
references.


.. index::
   single: scanning; example
   single: Scheme; scanning

Example: Scheme objects
-----------------------

Scanning tends to be a repetitive procedure and so you'll find it is
usually helpful to define macros to reduce the size of the source
code. The MPS provides a convenience macro :c:func:`MPS_FIX12` for the
common case of calling :c:func:`MPS_FIX1` and then immediately calling
:c:func:`MPS_FIX2` if the reference is "of interest".

.. note::

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
    reference and cast it to this type. The behaviour of such a cast
    is not defined by the C standard. See :ref:`topic-interface-pun`.

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

.. note::

    This scanner is a simple example intended to make the process
    clear to the reader. The scanning code and the object layout are
    not at all optimized.


.. index::
   single: scanning; interface

Scanning interface
------------------

.. c:type:: mps_ss_t

    The type of :term:`scan states`.

    A scan state represents the state of the current :term:`scan`. The
    MPS passes a scan state to the :term:`scan method` of an
    :term:`object format` when it needs to :term:`scan` for
    :term:`references` within a region of memory. The scan
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

    Call a function to do some scanning, from within a :term:`scan
    method`, between :c:func:`MPS_SCAN_BEGIN` and
    :c:func:`MPS_SCAN_END`, passing the :term:`scan state` correctly.

    ``ss`` is the scan state that was passed to the scan method.

    ``call`` is an expression containing a function call where ``ss``
    is one of the arguments.

    Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
    scan state is in a special state, and must not be passed to a
    function. If you really need to do so, for example because you
    have a structure shared between two :term:`object formats`, you
    must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
    scan state is passed correctly.

    The function being called must use :c:func:`MPS_SCAN_BEGIN` and
    :c:func:`MPS_SCAN_END` appropriately.

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
                    res = MPS_FIX12(ss, &obj->left);
                    if (res != MPS_RES_OK)
                        return res;
                    MPS_FIX_CALL(ss, res = data_scan(ss, &obj->data));
                    if (res != MPS_RES_OK)
                        return res;
                    res = MPS_FIX12(ss, &obj->right);
                    if (res != MPS_RES_OK)
                        return res;
                }
            } MPS_SCAN_END(ss);
            return MPS_RES_OK;
        }

    .. warning::

         Use of :c:func:`MPS_FIX_CALL` is best avoided, as it may
         force values out of registers (depending on compiler
         optimisations such as inlining). The gains in simplicity of
         the code ought to be measured against the loss in
         performance.


.. index::
   single: scanning; fixing
   single: fixing; interface

Fixing interface
----------------

.. c:function:: mps_bool_t MPS_FIX1(mps_ss_t ss, mps_addr_t ref)

    Determine whether a :term:`reference` needs to be passed to
    :c:func:`MPS_FIX2`.

    ``ss`` is the :term:`scan state` that was passed to the
    :term:`scan method`.

    ``ref`` is the reference.

    Returns a truth value (:c:type:`mps_bool_t`) indicating whether
    ``ref`` is "interesting" to the MPS. If it returns true, the scan
    method must invoke :c:func:`MPS_FIX2` to :term:`fix` ``ref``.

    This macro must only be used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    .. note::

        If your reference is :term:`tagged <tagged reference>` or
        otherwise "encrypted", you must ensure that it points to a
        location within the target block before calling
        :c:func:`MPS_FIX1`. (Therefore, a small tag in the low bits
        need not be stripped.)

    .. note::

        In the case where the scan method does not need to do anything
        between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`, you can use
        the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: mps_res_t MPS_FIX12(mps_ss_t ss, mps_addr_t *ref_io)

    :term:`Fix` a :term:`reference`.

    This macro is a convenience for the case where :c:func:`MPS_FIX1`
    is immediately followed by :c:func:`MPS_FIX2`. The interface is
    the same as :c:func:`MPS_FIX2`.


.. c:function:: mps_res_t MPS_FIX2(mps_ss_t ss, mps_addr_t *ref_io)

    :term:`Fix` a :term:`reference`.

    ``ss`` is the :term:`scan state` that was passed to the
    :term:`scan method`.

    ``ref_io`` points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful. In this case the
    reference may have been updated, and so the scan method must store
    the updated reference back to the region being scanned. The scan
    method must continue to scan the :term:`block`.

    If it returns any other result, the scan method must return that
    result as soon as possible, without fixing any further references.

    This macro must only be used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    .. note::

        If your reference is :term:`tagged <tagged reference>` (or
        otherwise "encrypted"), you must remove the tag (or otherwise
        decrypt the reference) before calling :c:func:`MPS_FIX2`, and
        restore the tag to the (possibly updated) reference
        afterwards.

        The only exception is for references to objects belonging to a
        format with :term:`in-band headers`: the header size must not
        be subtracted from these references.

    .. note::

        In the case where the scan method does not need to do anything
        between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`, you can use
        the convenience macro :c:func:`MPS_FIX12`.


.. index::
   single: scanning; area scanners

.. _topic-scanning-area:

Area scanners
-------------

An area scanner :term:`scans` an area of memory for
:term:`references <reference>`. Various functions in the MPS interface,
:such as :c:func:`mps_root_create_thread_tagged`, accept area scanners as
arguments so that the :term:`client program` can specify how to scan
special areas such as the :term:`control stack`.

The MPS provides some area scanners for common situations (such as an
area which is a vector of words with references identified by
:term:`tag bits <tag>`) but the :term:`client program` can provide
its own.

If you want to develop your own area scanner you can start by adapting
the scanners, found in ``scan.c`` in the MPS source code.

.. c:type:: mps_area_scan_t

    The type of area scanning functions, which are all of the form::
    
        mps_res_t scan(mps_ss_t ss,
                       mps_word_t *base, mps_word_t *limit,
                       void *closure, size_t closure_size);
    
    ``ss`` is the :term:`scan state`.
    
    ``base`` points to the first word to be scanned.
    
    ``limit`` points to the location just beyond the end of the area to be scanned.
    
    ``closure`` is a pointer to an arbitrary :term:`closure` object that
    contains parameters for the scan.  The object passed depends on the
    context.  For example, if the scanner was originally registered with
    :c:func:`mps_root_create_thread_tagged` then it is the value of
    the ``closure`` argument originally passed to that function.

    ``closure_size`` is an arbitrary size, conventionally used for the
    size of the :term:`closure` pointed to by ``closure``.  For example,
    if the scanner was originally registered with
    :c:func:`mps_root_create_thread_tagged` then it is the value of
    the ``closure_size`` argument originally passed to that function.

.. c:function:: mps_res_t mps_scan_area(mps_ss_t ss, mps_word_t *base, mps_word_t *limit, void *closure, size_t closure_size)

    Scan an area of memory :term:`fixing <fix>` every word.  ``closure`` and ``closure_size`` are ignored.
    
    This scanner is appropriate for use when all words in the area are simple untagged references.

.. c:type:: mps_scan_tag_t

    The type of a scan closure that is passed to the tagged area
    scanners in order to specify the format of the :term:`tagged
    references` in the area.
    
    It is a pointer to a :c:type:`mps_scan_tag_s` structure.

.. c:type:: mps_scan_tag_s

    The type of the structure used to represent :term:`tag bits <tag>` in :term:`tagged references` ::

        typedef struct mps_scan_tag_s {
            mps_word_t mask;
            mps_word_t pattern;
        } mps_scan_tag_s;

    ``mask`` is bit mask that is applied to words in the area to find
    the tag.  For example, a mask of 0b111 (decimal 7) specifies that
    the tag is stored in the least-significant three bits of the word.

    ``pattern`` is a bit pattern that is compared to the bits extracted
    by the ``mask`` to determine if the word is a reference.  The exact
    interpretation depends on which area scanner it is passed to.  See
    the documentation for the individual area scanners.

.. c:function:: mps_res_t mps_scan_area_masked(mps_ss_t ss, mps_word_t *base, mps_word_t *limit, void *closure, size_t closure_size)

    Scan an area of memory :term:`fixing <fix>` every word, but remove
    tag bits before fixing references, and restore them afterwards.
    ``closure`` should point to an :c:type:`mps_scan_tag_s`.
    
    For example, if ``mask`` is 0b111 (decimal 7), then this scanner
    will clear the bottom three bits of each word before fixing.  A word
    such as 0xC1374823 would be detagged to 0xC1374820 before fixing. 
    If it were fixed to 0xC812BC88 then it would be tagged back to
    0xC812BC8B before being stored.

    This scanner is useful when all words in the area must be treated as
    references no matter what tag they have.  This can be especially
    useful if you are debugging your tagging scheme.

.. c:function:: mps_res_t mps_scan_area_tagged(mps_ss_t ss, mps_word_t *base, mps_word_t *limit, void *closure, size_t closure_size)

    Scan an area of memory :term:`fixing <fix>` only words whose masked
    bits match a particular tag pattern.  ``closure`` should point to a
    :c:type:`mps_scan_tag_s`.
    
    For example, if ``mask`` is 7 and ``pattern`` is 5, then this
    scanner will only fix words whose low order bits are 0b101.

    Tags are masked off and restored as in :c:func:`mps_scan_area_masked`.

    This scanner is useful when you have a single tag pattern that
    distinguishes references, especially when that pattern is zero.

    .. warning::

        A risk of using tagged pointers in registers and on the stack is
        that in some circumstances, an optimizing compiler might
        optimize away the tagged pointer, keeping only the untagged
        version of the pointer.  See
        :c:func:`mps_root_create_thread_tagged`.

.. c:function:: mps_res_t mps_scan_area_tagged_or_zero(mps_ss_t ss, mps_word_t *base, mps_word_t *limit, void *closure, size_t closure_size)

    Scan an area of memory :term:`fixing <fix>` only words whose masked
    bits are zero or match a particular tag pattern.  ``closure`` should
    point to a
    :c:type:`mps_scan_tag_s`.

    For example, if ``mask`` is 7 and ``pattern`` is 3, then this
    scanner will fix words whose low order bits are 0b011 and words
    whose low order bits are 0b000, but not any others.

    This scanner is most useful for ambiguously scanning the stack and
    registers when using an optimising C compiler and non-zero tags on
    references, since the compiler is likely to leave untagged addresses
    of objects around which must not be ignored.
