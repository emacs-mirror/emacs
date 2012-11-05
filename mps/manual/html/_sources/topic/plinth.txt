.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/io/>`_
    `<https://info.ravenbrook.com/project/mps/master/design/lib/>`_
    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/concepts/>`_
    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/interface/>`_
    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/appendix/plinth/>`_


.. index::
   single: plinth; introduction
   single: freestanding environment
   single: hosted environment
   single: porting; plinth

.. _topic-plinth:

Plinth
======

The :dfn:`plinth` is a program module providing the MPS with all the
support it needs from the execution environment. It serves two
purposes, both relating to operating system support:

1. The MPS is designed to be portable to systems that have only a
   *conforming freestanding implementation* of the C language: that
   is, systems which potentially lack facilities of the Standard C
   Library, such as Standard I/O. The plinth provides all the
   necessary facilities.

2. The plinth gives the :term:`client program` complete control of
   interaction between the MPS and the user, including
   :ref:`assertions <topic-error-assertion>` and :ref:`telemetry
   <topic-telemetry>`.

The plinth may be provided by the :term:`client program`; however, a
sample implementation of the plinth using ANSI Standard C Library
facilities is included with the MPS, and this is good enough for most
applications.

There are many reasons why you might want to write your own plinth.
You may be targetting a *freestanding environment* such as an embedded
system. You might need to write the telemetry stream to a system
logging facility, or transmit it over a serial port or network
connection. Or you might need to direct debugging output to a
convenient window in the user interface.

The plinth is divided into two parts:

1. The :ref:`topic-plinth-io` enables the MPS to write binary messages
   to an output stream.

2. The :ref:`topic-plinth-lib` provides miscellaneous functionality
   that would be available via the Standard C Library on a hosted
   platform, including functions for reporting errors and accessing
   a processor clock.


.. c:macro:: CONFIG_PLINTH_NONE

    If this preprocessor constant is defined, exclude the ANSI plinth
    (``mpsioan.c`` and ``mpsliban.c``) from the MPS. For example::

        cc -DCONFIG_PLINTH_NONE -c mps.c        (Unix/OS X)
        cl /Gs /DCONFIG_PLINTH_NONE /c mps.c    (Windows)

    Having excluded the ANSI plinth, you must of course supply your
    own.


.. index::
   single: plinth; I/O module
   single: I/O module
   single: telemetry; I/O module

.. _topic-plinth-io:

I/O module
----------

::

    #include "mpsio.h"


.. c:type:: mps_io_t

    The type of the internal state of the I/O module.

    This is an alias for a pointer to the incomplete structure
    :c:type:`mps_io_s`, which the :term:`plinth` may define if it
    needs to. Alternatively, it may leave the structure type undefined
    and simply cast its own pointer to and from :c:type:`mps_io_t`.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this is an alias for
        ``FILE *``.


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

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls ``fopen`` on
        the file named by the environment variable
        :envvar:`MPS_TELEMETRY_FILENAME`.


.. c:function:: void mps_io_destroy(mps_io_t io)

    A :term:`plinth` function for tearing down the I/O module.

    ``io`` is the value that the plinth wrote to ``io_o`` when the MPS
    called :c:func:`mps_io_create`. If the plinth wrote no value, this
    parameter is undefined.

    After calling this function, the MPS guarantees not to use the
    value ``io`` again.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls ``fclose``.


.. c:function:: mps_res_t mps_io_write(mps_io_t io, void *buf, size_t size)

    A :term:`plinth` function for writing data via the I/O module.

    ``io`` is the value that the plinth wrote to ``io_o`` when the MPS
    called :c:func:`mps_io_create`. If the plinth wrote no value, this
    parameter is undefined.

    ``buf`` points to the data to write.

    ``size`` is the :term:`size` of the data in :term:`bytes (1)`.

    Returns :c:macro:`MPS_RES_OK` if successful.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls ``fwrite``.


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

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls ``fflush``.


.. index::
   single: plinth; library module
   single: library module

.. _topic-plinth-lib:

Library module
--------------

::

    #include "mpslib.h"


.. c:function:: mps_clock_t mps_clock(void)

    Return the time since some epoch, in units given by
    :c:func:`mps_clocks_per_sec`.

    This should be a cheap, high-resolution processor timer. There is
    no requirement to be able to relate this time to wall clock time.

    .. note::

        The ANSI Library module, ``mpsliban.c``, calls ``clock``.


.. c:function:: mps_clock_t mps_clocks_per_sec(void)

    Return the number of clock units (as returned by
    :c:func:`mps_clock`) per second.

    .. note::

        The ANSI Library module, ``mpsliban.c``, returns
        ``CLOCKS_PER_SEC``.


.. c:function:: void mps_lib_assert_fail(const char *message)

    Report an assertion failure and abort.

    ``message`` is a NUL-terminated string describing the assertion
    failure.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this reports the
        failure using ``fprintf(stderr, "...%s...", message)`` and
        terminates the program by calling ``abort``.


.. c:type:: mps_lib_FILE

    The type of output streams provided by the plinth.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is an alias
        for ``FILE *``.


.. c:function:: int mps_lib_fputc(int c, mps_lib_FILE *stream)

    Write a character to an output stream.

    ``c`` is the character.

    ``stream`` is the stream.

    Return the character written if successful, or
    :c:func:`mps_lib_get_EOF` if not.

    This function is intended to have the same semantics as the
    ``fputc`` function of the ANSI C Standard (:ref:`ISO/IEC 9899:1990
    <ISO90>` §7.11.7.3).

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around ``fputc``.


.. c:function:: int mps_lib_fputs(const char *s, mps_lib_FILE *stream)

    Write a string to an output stream.

    ``s`` is the NUL-terminated string.

    ``stream`` is the stream.

    This function is intended to have the same semantics as the
    ``fputs`` function of the ANSI C Standard (:ref:`ISO/IEC 9899:1990
    <ISO90>` §7.11.7.4).

    Return a non-negative integer if successful, or
    :c:func:`mps_lib_get_EOF` if not.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around ``fputs``.


.. c:function:: int mps_lib_get_EOF(void)

    Return the value that is returned from :c:func:`mps_lib_fputc` and
    :c:func:`mps_lib_fputs` to indicate failure.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this returns
        ``EOF``.


.. c:function:: mps_lib_FILE *mps_lib_get_stderr(void)

    Returns an output stream suitable for reporting errors.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this returns
        ``stderr``.

    .. note::

        The MPS does not use this at present, but it may be required
        in future.


.. c:function:: mps_lib_FILE *mps_lib_get_stdout(void)

    Returns an output stream suitable for reporting informative
    output.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this returns
        ``stdout``.

    .. note::

        The MPS does not use this at present, but it may be required
        in future.


.. c:function:: int mps_lib_memcmp(const void *s1, const void *s2, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memcmp``.

    ``s1`` and ``s2`` point to :term:`blocks` of memory to be
    compared.

    ``n`` is the :term:`size` of the blocks.

    Returns an integer that is greater than, equal to, or less than
    zero, accordingly as the block pointed to by ``s1`` is greater than,
    equal to, or less than the block pointed to by ``s2``.

    This function is intended to have the same semantics as the
    ``memcmp`` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <ISO90>` §7.11.4.1).

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around ``memcmp``.


.. c:function:: void *mps_lib_memcpy(void *dest, const void *source, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memcpy``.

    ``dest`` points to the destination.

    ``source`` points to the source.

    ``n`` is the number of bytes to copy from ``source`` to ``dest``.

    Returns ``dest``.

    This function is intended to have the same semantics as the
    ``memcpy`` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <ISO90>` §7.11.2.1).

    The MPS never passes overlapping blocks to
    :c:func:`mps_lib_memcpy`.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around ``memcpy``.


.. c:function:: void *mps_lib_memset(void *s, int c, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memset``.

    ``s`` points to the :term:`block` to fill with the byte ``c``.

    ``c`` is the byte to fill with (when converted to ``unsigned char``).

    ``n`` is the :term:`size` of the block.

    Returns ``s``.

    This function is intended to have the same semantics as the
    ``memset`` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <ISO90>` §7.11.6.1).

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around ``memset``.


.. c:function:: unsigned long mps_lib_telemetry_control()

    A :term:`plinth` function to supply a default value for the
    :term:`telemetry filter` from the environment. See
    :c:func:`mps_telemetry_control` for more information on the
    significant of the value.

    Returns the default value of the telemetry filter, as derived from
    the environment. It is recommended that the environment be
    consulted for a symbol analogous to
    :envvar:`MPS_TELEMETRY_CONTROL`, subject to local restrictions.

    In the absence of environmental data, a default of zero is
    recommended.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this reads the
        environment variable :envvar:`MPS_TELEMETRY_CONTROL`.
