.. sources:

     `<http://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/format/index.html>`_

.. index::
   single: object format; introduction
   single: format; object

.. _topic-format:

Object formats
==============

The need for some means of describing objects in the :term:`client
program` comes from :term:`tracing <trace>` and :term:`moving <moving
memory manager>`. During tracing, when an object is :term:`scanned
<scan>`, all the :term:`references` in the object must be
identified so that the objects they point to can be scanned in their
turn. When an object has moved, references to that object must be
identified so that they can be updated to point to the new location of
the object.

In general, only the client program can say which fields in an object
are references, and only the client program knows how references are
represented (for example, are they tagged?). *Object formats* provide
the means by which the client program communicates this information to
the MPS.

An object format is a collection of :term:`format methods` and other
(usually scalar) values which together describe programatically the
layout of objects belonging to the format. Format methods include the
:term:`skip method` (which calculates an object's size), the
:term:`scan method` (which :term:`fixes <fix>` references in the
object), and the :term:`forward method` (which replaces an object that
has moved with a :term:`forwarding object`).

Not every :term:`pool class` supports :term:`formatted objects`.


.. index::
   single: object format; interface

Interface
---------

.. c:type:: mps_fmt_t

    The type of an :term:`object format`.


.. c:function:: void mps_fmt_create_k(mps_fmt_t *mps_fmt_o, mps_arena_t arena, mps_arg_s args[])

    Create an :term:`object format`.

    ``fmt_o`` points to a location that will hold the address of the new
    object format.

    ``arena`` is the arena in which to create the format.

    ``args`` are :term:`keyword arguments` describing the format. Each
    :term:`pool class` requires a particular subset of these keyword
    arguments: see the documentation for that pool class.

    * :c:macro:`MPS_KEY_FMT_ALIGN` (type :c:type:`mps_align_t`,
      default :c:macro:`MPS_PF_ALIGN`) is an integer value specifying
      the alignment of objects allocated with this format. It should
      be large enough to satisfy the alignment requirements of any
      field in the objects, and it must not be larger than the pool
      alignment.

    * :c:macro:`MPS_KEY_FMT_HEADER_SIZE` (type :c:type:`size_t`,
      default 0) is an integer value specifying the header size for
      objects with :term:`in-band headers`. See
      :ref:`topic-format-headers` below.

    * :c:macro:`MPS_KEY_FMT_SCAN` (type :c:type:`mps_fmt_scan_t`) is a
      :term:`scan method` that identifies references within objects
      belonging to this format. See :c:type:`mps_fmt_scan_t`.

    * :c:macro:`MPS_KEY_FMT_SKIP` (type :c:type:`mps_fmt_skip_t`) is a
      :term:`skip method` that skips over objects belonging to this
      format. See :c:type:`mps_fmt_skip_t`.

    * :c:macro:`MPS_KEY_FMT_FWD` (type :c:type:`mps_fmt_fwd_t`) is a
      :term:`forward method` that stores relocation information for an
      object belonging to this format that has moved. See
      :c:type:`mps_fmt_fwd_t`.

    * :c:macro:`MPS_KEY_FMT_ISFWD` (type :c:type:`mps_fmt_isfwd_t`) is
      a :term:`is-forwarded method` that determines if an object
      belonging to this format has been moved. See
      :c:type:`mps_fmt_isfwd_t`.

    * :c:macro:`MPS_KEY_FMT_PAD` (type :c:type:`mps_fmt_pad_t`) is a
      :term:`padding method` that creates :term:`padding objects`
      belonging to this format. See :c:type:`mps_fmt_pad_t`.

    * :c:macro:`MPS_KEY_FMT_CLASS` (type :c:type:`mps_fmt_class_t`) is
      a method that returns an address that is related to the class or
      type of the object, for inclusion in the :term:`telemetry
      stream` for some events relating to the object. See
      :c:type:`mps_fmt_class_t`.

    :c:func:`mps_fmt_create_k` returns :c:macro:`MPS_RES_OK` if
    successful. The MPS may exhaust some resource in the course of
    :c:func:`mps_fmt_create_k` and will return an appropriate
    :term:`result code` if so.

    The object format pointed to by ``fmt_o`` persists until it is
    destroyed by calling :c:func:`mps_fmt_destroy`.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, ALIGNMENT);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, obj_scan);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
            res = mps_fmt_create_k(&obj_fmt, arena, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) error("Couldn't create obj format");


.. c:function:: void mps_fmt_destroy(mps_fmt_t fmt)

    Destroy an :term:`object format`.

    ``fmt`` is the object format to destroy.

    It is an error to destroy an object format if there exists a
    :term:`pool` using the format. The pool must be destroyed first.


.. index::
   pair: object format; in-band headers
   pair: object format; headers

.. _topic-format-headers:

In-band headers
---------------

There are use cases in which it is convenient for the :term:`client
program's <client program>` pointers to point some distance into the
memory :term:`block` containing the object. This typically happens
when the objects have a common :term:`in-band header` used for memory
management or class system purposes, but this situation also arises
when the low bits of a pointer are used for a tag. The MPS does not
care what the reason is, only about the offset of the pointer in
relation to the memory block.

If you have one of these use cases, you should pass the
:c:macro:`MPS_KEY_FMT_HEADER_SIZE` :term:`keyword argument` to
:c:func:`mps_fmt_create_k`, specifying the size of the header: that
is, the offset of a :term:`client pointer` from the base of the memory
block.

There are some cautions to be observed when using in-band headers:

#. The format methods (other than the :term:`padding method`) receive
   :term:`client pointers` (that is, pointers past the header) but all
   other MPS functions expect to receive and return :term:`base
   pointers` (that is, pointers to the base of the block where the
   header is stored).

   In particular, :c:func:`mps_reserve` and :c:func:`mps_alloc` always
   hand out base pointers, and :c:func:`mps_free` expects to receive
   one.

#. Formatted objects must be longer than the header. In other words,
   objects consisting of only a header are not supported.

#. Even if the header size is larger than or equal to
   :term:`alignment`, the :term:`padding method` must still be able to
   create :term:`padding objects` down to the alignment size.

#. Not all :term:`pool classes` support objects with in-band headers.
   See the documentation for the pool class.


.. index::
   pair: object format; cautions

.. _topic-format-cautions:

Cautions
--------

#. The MPS guarantees that format methods have exclusive access to the
   object for the duration of the call. This guarantee may entail
   suspending arbitrary threads. The methods that manipulate the
   object must not perform any sort of inter-thread locking or
   communication.

#. The MPS may call format methods in the context of an exception
   handler or a signal handler. For example, the following sequence of
   events is common:

   a. the MPS places a :term:`read barrier` on a block of memory;

   b. the client program attempts to read from this block;

   c. the hardware raises a :term:`protection fault`;

   d. the MPS signal handler is called;

   e. the MPS ensures that the contents of the block are correct and
      consistent: this may involve inspection of formatted objects in
      the block (or indeed, elsewhere), and so

   f. the MPS calls format methods.

   Therefore, the format methods must be able to be run at any time,
   including asynchronously or in parallel with the rest of the
   program.

#. Format methods must be re-entrant.

#. Format methods must not:

   a. call library code;

   b. perform a non-local exit (for example, by calling ``longjmp``);

   c. call any functions in the MPS other than the fix functions
      (:c:func:`mps_fix`, :c:func:`MPS_FIX1`, :c:func:`MPS_FIX12`, and
      :c:func:`MPS_FIX2`).

   It's permissible to call other functions in the client program, but
   see :c:func:`MPS_FIX_CALL` for a restriction on passing the
   :term:`scan state`.

#. Subject to the above constraints, format methods can freely access:

   a. memory inside the object or block that they have been asked to
      look at;

   b. memory managed by the MPS that is in pools that do not protect
      their contents;

   c. memory not managed by the MPS;

   They must not access other memory managed by the MPS.


.. index::
   single: format method
   single: object format; format method

Format methods
--------------

.. c:type:: mps_addr_t (*mps_fmt_class_t)(mps_addr_t addr)

    The type of the class method of an :term:`object format`.

    ``addr`` is the address of the object whose class is of interest.

    Returns an address that is related to the class or type of the
    object, or a null pointer if this is not possible.

    It is recommended that a null pointer be returned for
    :term:`padding objects` and :term:`forwarding objects`.


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

    2. The forwarding marker must be the same size as the old object.
       That is, when the :term:`skip method` is called on the
       forwarding marker, it must return the same address as when it
       was called on the old object.

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
    :term:`pages`).

    The padding method must create a padding object of the specified
    size at the specified address. The size can be any aligned (to the
    format alignment) size. A padding object must be acceptable to
    other methods in the format (the :term:`scan method`, the
    :term:`skip method`, and so on).

    .. note::

        The padding method always receives a base pointer, even if the
        object format has a non-zero
        :c:macro:`MPS_KEY_FMT_HEADER_SIZE`.


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
    Generally, it is better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.

    The scan method for an object format is called when the MPS needs
    to scan objects in a block of memory containing objects belonging
    to that format. The scan method is called with a scan state and
    the base and limit of the block of objects to scan. It must then
    indicate references within the objects by calling
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

    If the object format is capable of creating forwarding objects or
    padding objects, the scan method must be able to scan these
    objects. (In the case of the forwarding object, the scan method
    should not fix the pointer to the new location.)

    .. seealso::

        :ref:`topic-scanning`.


.. c:type:: mps_addr_t (*mps_fmt_skip_t)(mps_addr_t addr)

    The type of the :term:`skip method` of an :term:`object format`.

    ``addr`` is the address of the object to be skipped.

    Returns the address of the "next object". In an object format
    without :term:`in-band headers`, this is the address just past the
    end of this object. In an object format with in-band headers, it's
    the address just past where the header of next object would be, if
    there were one.

    .. note::

        In either case, the result is the sum of ``addr`` and the size
        of the block containing the object.

    If the object format is capable of creating forwarding objects or
    padding objects, the skip method must be able to skip these
    objects.

    A skip method is not allowed to fail.

    .. note::

        The MPS uses this method to determine the size of objects (by
        subtracting ``addr`` from the result) as well as skipping over
        them.


.. index::
   pair: object format; introspection

Object format introspection
---------------------------

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

    .. note::

        This function is intended for heap analysis, tuning, and
        debugging, not for frequent use in production.

    .. warning::

        If a garbage collection is currently in progress (that is, if
        the arena is in the :term:`clamped <clamped state>` or
        :term:`unclamped state`), then only objects that are known to
        be currently valid are visited.

        For the most reliable results, ensure the arena is in the
        :term:`parked state` by calling :c:func:`mps_arena_park`
        before calling this function (and release it by calling
        :c:func:`mps_arena_release` afterwards, if desired).


.. c:type:: void (*mps_formatted_objects_stepper_t)(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool, void *p, size_t s)

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

    c. memory not managed by the MPS;

    It must not access other memory managed by the MPS.


Obsolete interface
------------------

.. deprecated:: starting with version 1.112.

    Use :c:func:`mps_ap_create_k` instead: the :term:`keyword
    arguments` interface is more flexible and easier to understand.

Formerly the only way to create object formats was to describe the
format in the form of a *format variant structure*.

There are four format variants.

* Variant A (:c:type:`mps_fmt_A_s`): for objects without
  :term:`in-band headers`.

* Variant B (:c:type:`mps_fmt_B_s`): as variant A, but with the
  addition of a class method.

* Variant auto-header (:c:type:`mps_fmt_auto_header_s`): for objects
  with :term:`in-band headers`.

* Variant fixed (:c:type:`mps_fmt_fixed_s`): for fixed-size objects.

The client program creates an object format by construct a format
variant structure and then calling the appropriate ``mps_fmt_create_``
function for the variant. The variant structure can then be disposed
of.


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

    The fields of this structure correspond to the keyword arguments
    to :c:func:`mps_fmt_create_k`, except for ``copy``, which is not
    used. In older versions of the MPS this was a *copy method*
    that copied objects belonging to this format.


.. c:function:: mps_res_t mps_fmt_create_A(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_A_s *fmt_A)

    Create an :term:`object format` based on a description of an
    object format of variant A.


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


.. c:function:: mps_res_t mps_fmt_create_B(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_B_s *fmt_B)

    Create an :term:`object format` based on a description of an
    object format of variant B.


.. c:type:: mps_fmt_auto_header_s

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

    Create an :term:`object format` based on a description of an
    object format of variant auto-header.


.. c:type:: mps_fmt_fixed_s

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

    Create an :term:`object format` based on a description of an
    object format of variant fixed.
