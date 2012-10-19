.. _topic-format:

Object formats
==============

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/format/index.html

::

    mps_fmt_t create_format(mps_arena_t arena)
    {
        mps_fmt my_format;
        mps_res_t res;
        mps_fmt_A_s my_format_A = {
            my_alignment, &my_scan, &my_skip, &my_copy, &my_fwd,
            &my_isfwd, &my_pad
        };

        res = mps_fmt_create_A(&my_format, arena, &my_format_A);
        assert(res != MPS_RES_OK);

        return my_format;
    }

::

    mps_fmt_t create_format(mps_arena_t arena)
    {
        mps_fmt my_format;
        mps_res_t res;
        mps_fmt_B_s my_format_B = {
            my_alignment, &my_scan, &my_skip, &my_copy,
            &my_fwd, &my_isfwd, &my_pad, &my_class
        };

        res = mps_fmt_create_B(&my_format, arena, &my_format_B);
        assert(res != MPS_RES_OK);

        return my_format;
    }

::

    mps_fmt_t create_format(mps_arena_t arena)
    {
        mps_fmt format;
        mps_res_t res;
        mps_fmt_auto_header_s format_desc = { 
            my_alignment, &my_scan, &my_skip, &my_fwd,
            &my_isfwd, &my_pad, HEADER_SIZE
        };

        res = mps_fmt_create_auto_header(&format, arena, &format_desc);
        assert(res != MPS_RES_OK);

        return format;
    }

::

    mps_addr_t my_class_method(mps_addr_t object) {
        my_object_generic_t generic_object = object;
        return (mps_addr_t)(generic_object.class);
    }

::

    /* define the function */

    void example_fwd(mps_addr_t old, mps_addr_t new)
    {
        /* ... */
    }

    /* also define example_scan, example_skip, etc */
    /* store pointer to function in the format variant struct */
    struct mps_fmt_B_s example_fmt_B = {
        4, /* align */
        example_scan,
        example_skip,
        example_copy,
        example_fwd,
        example_isfwd,
        example_pad,
        example_class
    };

    /* The (address of the) example_fmt_B object can now be passed to */
    /* mps_fmt_create_B to create a format. */

::

    mps_addr_t my_skip_method(mps_addr_t object)
    {
        char *p = (char *)object;
        my_object_t my_object = (my_object_t)object;
        return ((mps_addr_t)(p + my_object->length));
    }

::

    #include "mps.h"
    #include "mpscamc.h"
    #include <stdlib.h>

    struct mps_fmt_A_s fmt_A_s = {
        (mps_align_t)4,
        scan, skip, copy, move, isMoved, pad
    };

    void go(mps_space_t space)
    {
        mps_fmt_t format;
        mps_res_t res;
        mps_pool_t pool;

        res = mps_fmt_create_A(&format, space, &mps_fmt_A_s);
        if (res != MPS_RES_OK)
            abort();

        res = mps_pool_create(&pool, space, mps_class_amc(), format);
        if (res != MPS_RES_OK)
            abort();

        /* do some stuff here */

        mps_pool_destroy(pool);
        mps_format_destroy(format);
    }


Interface
---------

.. c:function:: mps_bool_t mps_addr_fmt(mps_fmt_t *fmt_o, mps_arena_t arena, mps_addr_t addr)

    Determine the :term:`object format` to which an address belongs.

    ``fmt_o`` points to a location that will hold the address of the
    object format, if one is found.

    ``arena`` is the arena whose object formats will be considered.

    ``addr`` is the address.

    If ``addr`` is the address of a location inside a block allocated
    from a pool in ``arena``, and that pool has an object format, then
    update the location pointed to by ``fmt_o`` with the address of
    the object format, and return true.

    If ``addr`` is the address of a location inside a block allocated
    from a pool in ``arena``, but that pool has no object format,
    return false.

    If ``addr`` points to a location that is not managed by ``arena``,
    return false.

    If none of the above conditions is satisfied,
    :c:func:`mps_addr_fmt` may return either true or false.

    .. note::

        This function might return a false positive by returning true
        if you ask about an address that happens to be inside memory
        managed by a pool with an object format, but which is not
        inside a block allocated by that pool. It never returns a
        false negative.


.. c:function:: void mps_arena_formatted_objects_walk(mps_arena_t arena, mps_formatted_objects_stepper_t f, void *p, size_t s)

    Visit all :term:`formatted objects <formatted object>` in an
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
    visited. During a :term:`trace` this will in general be only the
    :term:`black` objects, though the :ref:`pool-lo` pool, for
    example, will walk all objects since they are validly formatted
    whether they are black or :term:`white`. :term:`Padding objects
    <padding object>` may be visited at the pool class's discretion:
    the :term:`client program` should handle this case.

    The function ``f`` may not allocate memory or access any
    automatically-managed memory except within ``object``.

    .. seealso::

        :ref:`topic-arena`.

    .. note::

        Walking the heap is "dodgy".



.. c:type:: mps_fmt_A_s

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

    Broadly speaking, object formats of variant A are suitable for use
    in :term:`copying <copying garbage collection>` or :term:`moving
    <moving garbage collector>` :term:`pools <pool>`.

    ``align`` is an integer value specifying the alignment of objects
    allocated with this format. It should be large enough to satisfy
    the alignment requirements of any field in the objects, and it
    must not be larger than the pool alignment.

    ``scan`` is a :term:`scan method` that identifies references
    within objects belonging to this format. See
    :c:type:`mps_fmt_scan_t`.

    ``skip`` is a :term:`skip method` that skips over objects
    belonging to this format. See :c:type:`mps_fmt_skip_t`.

    ``copy`` is not used. (In older versions of the MPS it was a
    :term:`copy method` that copied objects belonging to this
    format.)

    ``fwd`` is a :term:`forward method` that stores relocation
    information for an object belonging to this format that has moved.
    See :c:type:`mps_fmt_fwd_t`.

    ``isfwd`` is a :term:`is-forwarded method` that determines if an
    object belonging to this format has been moved. See
    :c:type:`mps_fmt_isfwd_t`.

    ``pad`` is a :term:`padding method` that creates :term:`padding
    objects <padding object>` belonging to this format. See
    :c:type:`mps_fmt_pad_t`.


.. c:type:: mps_fmt_auto_header_s

    The type of the structure used to create an :term:`object format`
    of variant auto_header. ::

        typedef struct mps_fmt_auto_header_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
            size_t          mps_headerSize;
        } mps_fmt_auto_header_s;

    Variant auto_header is the same as variant A except for the
    removal of the unused ``copy`` method, and the addition of the
    ``mps_headerSize`` field. See :c:type:`mps_fmt_A_s`.

    Broadly speaking, the object formats of this variant are suitable
    for use in :term:`automatic memory management` for objects with
    :term:`headers <in-band header>` (hence the name). More precisely,
    this variant is intended for formats where the :term:`client
    program's <client program>` pointers point some distance into the
    memory :term:`block` containing the object. This typically happens
    when the objects have a common header used for memory management
    or class system purposes, but this situation also arises when the
    low bits of a pointer are used for a tag. The MPS does not care
    what the reason is, only about the offset of the pointer in
    relation to the memory block.

    ``mps_headerSize`` is the size of the header, that is, the offset of
    a client pointer from the base of the memory block.

    .. note::

        For technical reasons, formatted objects must be longer than
        the header. In other words, objects consisting of only a
        header are not supported.

        Even if the header size is larger than or equal to
        :term:`alignment`, the :term:`padding method` must still be
        able to create :term:`padding objects <padding object>` down
        to the alignment size.

        Variant auto_header is only supported by :ref:`pool-amc` and
        :ref:`pool-amcz`.


.. c:type:: mps_fmt_B_s

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


.. c:type:: mps_addr_t (*mps_fmt_class_t)(mps_addr_t addr)

    The type of the class method of an :term:`object format`.

    ``addr`` is the address of the object whose class is of interest.

    Returns an address that is related to the class or type of the
    object, or a null pointer if this is not possible.

    It is recommended that a null pointer be returned for
    :term:`padding objects <padding object>` and :term:`forwarding
    objects <forwarding object>`.


.. c:function:: mps_res_t mps_fmt_create_A(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_A_s *fmt_A)

    Create an :term:`object format` of variant A.

    ``fmt_o`` points to a location that will hold the address of the new
    object format.

    ``arena`` is the arena in which to create the format.

    ``fmt_A`` points to a description of an object format of variant A.

    Returns :c:macro:`MPS_RES_OK` if successful. The MPS may exhaust
    some resource in the course of :c:func:`mps_fmt_create_A` and will
    return an appropriate :term:`result code` if so.

    After this function returns, the object format description pointed
    to be ``fmt_A`` is no longer needed and may be discarded. The object
    format pointed to by ``fmt_o`` persists until it is destroyed by
    calling :c:func:`mps_fmt_destroy`.


.. c:function:: mps_res_t mps_fmt_create_B(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_B_s *fmt_B)

    Create an :term:`object format` of variant B.

    ``fmt_o`` points to a location that will hold the address of the new
    object format.

    ``arena`` is the arena in which to create the format.

    ``fmt_B`` points to a description of an object format of variant B.

    Returns :c:macro:`MPS_RES_OK` if successful. The MPS may exhaust
    some resource in the course of :c:func:`mps_fmt_create_B` and will
    return an appropriate :term:`result code` if so.


.. c:function:: mps_res_t mps_fmt_create_auto_header(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_auto_header_s *fmt_ah)

    Create an :term:`object format` of variant auto_header.

    ``fmt_o`` points to a location that will hold the address of the new
    object format.

    ``arena`` is the arena in which to create the format.

    ``fmt_ah`` points to a description of an object format of variant
    auto_header.

    Returns :c:macro:`MPS_RES_OK` if successful. The MPS may exhaust
    some resource in the course of
    :c:func:`mps_fmt_create_auto_header` and will return an
    appropriate :term:`result code` if so.


.. c:function:: void mps_fmt_destroy(mps_fmt_t fmt)

    Destroy an :term:`object format`.

    ``fmt`` is the object format to destroy.

    It is an error to destroy an object format if there exists a
    :term:`pool` using the format. The pool must be destroyed first.
  

.. c:type:: void (*mps_fmt_fwd_t)(mps_addr_t old, mps_addr_t new)

    The type of the :term:`forward method` of an :term:`object format`.

    ``old`` is the address of an object.

    ``new`` is the address to where the object has been moved.

    The MPS calls the forward method for an object format when it has
    relocated an object belonging to that format. The forward method
    must replace the object at ``old`` with a :term:`forwarding marker`
    that points to the address 'new'. The forwarding marker must meet
    the following requirements:

    1. It must be possible for the MPS to call other methods in the
       object format (the :term:`scan method`, the :term:`skip method`
       and so on) with the address of a forwarding marker as the
       argument.

    2. The forwarding marker must not be bigger than the original
       object.

    3. It must be possible for the :term:`is-forwarded method` of the
       object format to distinguish the forwarding marker from
       ordinary objects, and the is-forwarded method method must
       return the address ``new``. See :c:type:`mps_fmt_isfwd_t`.

    .. note::

        This method is never invoked by the :term:`garbage collector`
        on an object in a :term:`non-moving <non-moving garbage
        collector>` :term:`pool`.


.. c:type:: mps_addr_t (*mps_fmt_isfwd_t)(mps_addr_t addr)

    The type of the :term:`is-forwarded method` of an :term:`object
    format`.

    ``addr`` is the address of a candidate object.

    If the ``addr`` is the address of a :term:`forwarding object`, return
    the address where the object was moved to. This must be the value
    of the ``new`` argument supplied to the :term:`forward method` when
    the object was moved. If not, return a null pointer.

    .. note::

        This method is never invoked by the :term:`garbage collector`
        on an object in a :term:`non-moving <non-moving garbage
        collector>` :term:`pool`.


.. c:type:: void (*mps_fmt_pad_t)(mps_addr_t addr, size_t size)

    The type of the :term:`padding method` of an :term:`object
    format`.

    ``addr`` is the address at which to create a :term:`padding object`.

    ``size`` is the :term:`size` of the padding object to be created.

    The MPS calls a padding method when it wants to create a padding
    object. Typically the MPS creates padding objects to fill in
    otherwise unused gaps in memory; they allow the MPS to pack
    objects into fixed-size units (such as operating system
    :term:`pages <page>`).

    The padding method must create a padding object of the specified
    size at the specified address. The size can be any aligned (to the
    format alignment) size. A padding object must be acceptable to
    other methods in the format (the :term:`scan method`, the
    :term:`skip method`, and so on).


.. c:type:: mps_res_t (*mps_fmt_scan_t)(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)

    The type of the :term:`scan method` of an :term:`object format`.

    ``ss`` is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    ``base`` points to the first :term:`formatted object` in the block
    of memory to be scanned.

    ``limit`` points to the location just beyond the end of the block to
    be scanned. Note that there might not be any object at this
    location.

    Returns a :term:`result code`. If a fix function returns a value
    other than :c:macro:`MPS_RES_OK`, the scan method must return that
    value, and may return without fixing any further references.
    Generally, itis better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.

    The scan method for an object format is called when the MPS needs
    to scan objects in a block of memory containing objects belonging
    to that format. The scan method is called with a scan state and
    the base and limit of the block of objects to scan. It must then
    indicate references within the objects by calling
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

    .. seealso::

        :ref:`topic-scanning`.


.. c:type:: mps_addr_t (*mps_fmt_skip_t)(mps_addr_t addr)

    The type of the :term:`skip method` of an :term:`object format`.

    ``addr`` is the address of the object to be skipped.

    Returns the address of the "next object". In an object format
    without headers (for example, a format of variant A), this is the
    address just past the end of this object. In an object format with
    headers (for example, a format of variant auto_header), it's the
    address just past where the header of next object would be, if
    there were one. It is always the case that the difference between
    ``addr`` and the return value is the size of the block containing
    the object.

    A skip method is not allowed to fail.

    .. note::

        The MPS uses this method to determine the size of objects (by
        subtracting ``addr`` from the result) as well as skipping over
        them.


.. c:type:: mps_fmt_t

    The type of an :term:`object format`.


.. c:type:: void (*mps_formatted_objects_stepper_t)(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool, void *p, size_t s)

    The type of a :term:`formatted objects <formatted object>`
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

    .. seealso::

        :ref:`topic-arena`.


