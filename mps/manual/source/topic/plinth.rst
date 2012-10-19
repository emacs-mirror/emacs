.. _topic-plinth:

The plinth
==========

From //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/concepts/index.html

A program module providing the MPS with all the support it needs from the execution environment. Mainly this includes simple I/O facilities to support debugging.

The plinth is provided by the client application; however, a sample implementation of the plinth using standard ANSI C library facilities is included with the MPS, and this is good enough for most applications.


From //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/interface/index.html

To perform its various duties, the MPS needs very little external support. Indeed, there is a way of using it so that it needs none at all, making it possible to use the MPS in embedded applications. There are two key components to this: the client arena and the plinth . This section concerns the plinth; for more information about the client arena, see mps_arena_class_cl in the Reference Manual.

The plinth also has another purpose: it gives the application programmer complete control of how interaction between the MPS and the user happens. This comprises things like debugging messages and logging. The two purposes are intertwined, because both relate to operating system support.

The plinth is a program module providing the MPS with all the support functions it needs from the execution environment. The plinth is provided by the application programmer; this is how the plinth removes the need for external libraries, by getting the support from the client application, and at the same time, gives the application programmer control over the implementation of its features.

However, before you panic, a sample implementation of a plinth using standard ISO C library facilities is provided with the MPS (mpsliban.c and mpsioan.c), and this is often adequate for your needs, so you don't have to write your own. Naturally, if you use the ISO plinth, you then need to link with the C library.

There are many reasons why you might want to write your own plinth. For embedded applications, the MPS will work in what the C standard calls a freestanding environment, as long as you provide it with a plinth that works in that environment, and use the client arena (virtual memory arenas contain OS-specific code that calls the VM interfaces of the OS). Programmers of GUI applications might want a plinth that directs debugging output to a convenient window.

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/appendix/plinth/index.html

The example ANSI plinth, ``mpsliban.c``, implements :c:func:`mps_clock` by calling the ISO C function ``clock`` in ``time.h``.  The difference between two of these clock values may be converted to seconds by dividing by the ``CLOCKS_PER_SEC`` conversion factor.


See also //info.ravenbrook.com/project/mps/master/design/io/index.html


Declared in ``mpsio.h``
-----------------------

.. c:type:: mps_io_t

    The type of the internal state of the I/O module.

    This is an alias for a pointer to the incomplete structure
    :c:type:`mps_io_s`, which the :term:`plinth` may define if it
    needs to. Alternatively, it may leave the structure type undefined
    and simply cast its own pointer to and from :c:type:`mps_io_t`.


.. c:function:: mps_res_t mps_io_create(mps_io_t *io_o)

    A :term:`plinth` function for setting up the I/O module.

    ``io_o`` points to a location which the plinth may update with a
    pointer to its internal state, if any.

    Returns :c:macro:`MPS_RES_OK` if successful.

    The MPS calls this function to set up the I/O module, for example
    if there are events in the :term:`telemetry stream` that need to
    be output.

    A typical plinth will use it to open a file for writing, or to
    connect to the system logging interface.


.. c:function:: void mps_io_destroy(mps_io_t io)

    A :term:`plinth` function for tearing down the I/O module.

    ``io`` is the value that the plinth wrote to ``io_o`` when the MPS
    called :c:func:`mps_io_create`. If the plinth wrote no value, this
    parameter is undefined.

    After calling this function, the MPS guarantees not to use the
    value ``io`` again.


.. c:function:: mps_res_t mps_io_write(mps_io_t io, void *buf, size_t size)

    A :term:`plinth` function for writing data via the I/O module.

    ``io`` is the value that the plinth wrote to ``io_o`` when the MPS
    called :c:func:`mps_io_create`. If the plinth wrote no value, this
    parameter is undefined.

    ``buf`` points to the data to write.

    ``size`` is the :term:`size` of the data in :term:`bytes <byte (1)>`.

    Returns :c:macro:`MPS_RES_OK` if successful.


.. c:function:: mps_res_t mps_io_flush(mps_io_t io)

    A :term:`plinth` function for flushing the I/O module.

    ``io`` is the value that the plinth wrote to ``io_o`` when the MPS
    called :c:func:`mps_io_create`. If the plinth wrote no value, this
    parameter is undefined.

    Returns :c:macro:`MPS_RES_OK` if successful.

    The MPS calls this function when it is done with the
    :term:`telemetry stream`, or when the :term:`client program` calls
    :c:func:`mps_telemetry_flush`. This function should ensure that
    the buffers of data passed to the latest calls to
    :c:func:`mps_io_write` are properly recorded, should the
    :term:`client program` terminate (uncontrollably as a result of a
    bug, for example) or some interactive tool require access to the
    event data.


Declared in ``mpslib.h``
------------------------

.. c:function:: int mps_lib_memcmp(const void *s1, const void *s2, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memcmp``.

    ``s1`` and ``s2`` point to :term:`blocks <block>` of memory to be
    compared.

    ``n`` is the :term:`size` of the blocks.

    Returns an integer that is greater than, equal to, or less than
    zero, accordingly as the block pointed to by ``s1`` is greater than,
    equal to, or less than the block pointed to by ``s2``.

    This function is intended to have the same semantics as the
    ``memcmp`` function of the [ANSI C Standard]_ (section 7.11.4.1).


.. c:function:: void *mps_lib_memcpy(void *dest, const void *source, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memcpy``.

    ``dest`` points to the destination.

    ``source`` points to the source.

    ``n`` is the number of bytes to copy from ``source`` to ``dest``.

    Returns ``dest``.

    This function is intended to have the same semantics as the
    ``memcpy`` function of the [ANSI C Standard]_ (section 7.11.2.1).

    The MPS never passes overlapping blocks to
    :c:func:`mps_lib_memcpy`.

.. c:function:: void *mps_lib_memset(void *s, int c, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memset``.

    ``s`` points to the :term:`block` to fill with the byte ``c``.

    ``c`` is the byte to fill with (when converted to ``unsigned char``).

    ``n`` is the :term:`size` of the block.

    Returns ``s``.

    This function is intended to have the same semantics as the
    ``memset`` function of the [ANSI C Standard]_ (section 7.11.6.1).


.. c:function:: unsigned long mps_lib_telemetry_control()

    A :term:`plinth` function to supply a default value for the
    :term:`telemetry filter` from the environment. See
    :c:func:`mps_telemetry_control` for more information on the
    significant of the value.

    Returns the default value of the telemetry filter, as derived from
    the environment. It is recommended that the environment be
    consulted for a symbol analogous to
    :c:macro:`MPS_TELEMETRY_CONTROL`, subject to local restrictions.

    In the absence of environmental data, a default of zero is
    recommended.


Undocumented in ``mpslib.h``
----------------------------

.. c:function:: int mps_lib_get_EOF(void)
.. c:type:: mps_lib_FILE
.. c:function:: mps_lib_FILE *mps_lib_get_stderr(void)
.. c:function:: mps_lib_FILE *mps_lib_get_stdout(void)
.. c:function:: int mps_lib_fputc(int c, mps_lib_FILE *stream)
.. c:function:: int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
.. c:function:: void mps_lib_assert_fail(const char *message)
.. c:function:: mps_clock_t mps_clock(void)
.. c:type:: mps_clock_t
.. c:function:: mps_clock_t mps_clocks_per_sec(void)


Undocumented in ``mpsw3.h``
---------------------------

.. c:function:: LONG mps_SEH_filter(LPEXCEPTION_POINTERS info, void **hp_o, size_t *hs_o)
.. c:function:: void mps_SEH_handler(void *p, size_t s)
