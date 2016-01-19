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

The :dfn:`plinth` is a program module that provides the MPS with the
support it needs from the execution environment. The MPS uses the plinth instead of (say) the Standard C Library because:

#. The MPS is designed to be portable to systems that have only a
   :term:`freestanding` implementation of the C language: that is,
   systems which potentially lack some of the facilities of the
   Standard C Library, such as standard I/O. The plinth provides a way
   to map MPS requirements to the facilities provided on the platform,
   whatever they are.

#. The plinth gives the :term:`client program` complete control of
   interaction between the MPS and the user, including
   :ref:`assertions <topic-error-assertion>` and :ref:`telemetry
   <topic-telemetry>`.

The plinth may be provided by the :term:`client program`; however, a
sample implementation of the plinth using ANSI Standard C Library
facilities is included with the MPS, and this is good enough for most
applications.

There are many reasons why you might want to write your own plinth.
You may be targeting an embedded system with only a
:term:`freestanding` implementation of the C language. You might need
to write the telemetry stream to a system logging facility, or
transmit it over a serial port or network connection. Or you might
need to direct debugging output to a convenient window in the user
interface.

The plinth is divided into two parts:

#. The :ref:`topic-plinth-io` provides general-purpose I/O
   functionality. It is used to output a :term:`telemetry stream` of
   events to assist with debugging and profiling.

#. The :ref:`topic-plinth-lib` provides miscellaneous functionality
   that would be available via the Standard C Library on a hosted
   platform, including functions for reporting errors and accessing
   a processor clock.

The functions in the plinth module may be called in the context of a
signal handler for a protection fault (or equivalent), so they must
not access memory that is managed by the MPS, and they need to take
into account the restrictions imposed by the operating system. (See
"`Defining Signal Handlers`_" in the GNU C Library Reference Manual
for useful advice.)

.. _Defining Signal Handlers: http://www.gnu.org/software/libc/manual/html_node/Defining-Handlers.html


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

    The type of an I/O stream.

    This is an alias for a pointer to the incomplete structure
    :c:type:`mps_io_s`, which the :term:`plinth` may define if it
    needs to. Alternatively, it may leave the structure type undefined
    and simply cast its own pointer to and from :c:type:`mps_io_t`.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this is an alias for
        ``FILE *``.


.. c:function:: mps_res_t mps_io_create(mps_io_t *io_o)

    A :term:`plinth` function for creating an I/O stream for the
    :term:`telemetry stream`.

    ``io_o`` points to a location suitable for storing a pointer to an
    I/O stream.

    If successful, the function must update this location with a
    suitable pointer for the telemetry stream and return
    :c:macro:`MPS_RES_OK`. Otherwise, it must return some other
    :term:`result code`.

    The MPS calls this function to create the I/O stream for telemetry
    output. A typical plinth will use it to open a file for writing,
    or to connect to the system logging interface.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls
        :c:func:`fopen` on the file named by the environment variable
        :envvar:`MPS_TELEMETRY_FILENAME`.


.. c:function:: void mps_io_destroy(mps_io_t io)

    A :term:`plinth` function for destroying an I/O stream.

    ``io`` is a pointer to the I/O stream to be destroyed. It was
    previously created by a call to :c:func:`mps_io_create`.

    After calling this function, the MPS guarantees not to use the
    value ``io`` again.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls
        :c:func:`fclose`.


.. c:function:: mps_res_t mps_io_write(mps_io_t io, void *buf, size_t size)

    A :term:`plinth` function for writing data to an I/O stream.

    ``io`` is the I/O stream.

    ``buf`` points to the data to write.

    ``size`` is the :term:`size` of the data in :term:`bytes (1)`.

    Returns :c:macro:`MPS_RES_OK` if successful.

    .. note::

        In the ANSI I/O module, ``mpsioan.c``, this calls
        :c:func:`fwrite`.


.. c:function:: mps_res_t mps_io_flush(mps_io_t io)

    A :term:`plinth` function for flushing an I/O stream.

    ``io`` is the I/O stream.

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

        In the ANSI I/O module, ``mpsioan.c``, this calls
        :c:func:`fflush`.


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

    .. note::

        The ANSI Library module, ``mpsliban.c``, calls ``clock``.

    The MPS calls this function to make scheduling decisions (see
    :ref:`topic-collection-schedule`), and to calibrate the time
    stamps on events in the :term:`telemetry stream`. If your platform
    has a low-resolution ``clock()``, and there are higher-resolution
    clocks readily available, then using one of those will improve MPS
    scheduling decisions and the quality of telemetry output. For
    instance, with ``getrusage()``::

        #include <sys/resource.h>

        mps_clock_t mps_clock(void) {
            struct rusage s;
            int res = getrusage(RUSAGE_SELF, &s);
            if (res != 0) {
                /* handle error */
            }
            return ((mps_clock_t)s.ru_utime.tv_sec) * 1000000 + s.ru_utime.tv_usec;
        }


.. c:function:: mps_clock_t mps_clocks_per_sec(void)

    Return the number of clock units per second, as returned by
    :c:func:`mps_clock`.

    .. note::

        The ANSI Library module, ``mpsliban.c``, returns
        ``CLOCKS_PER_SEC``.


.. c:function:: void mps_lib_assert_fail(const char *message)

    Report an assertion failure.

    ``message`` is a NUL-terminated string describing the assertion
    failure.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this reports the
        failure by calling ``fprintf(stderr, "...%s...", message)``,
        flushes the :term:`telemetry stream` by calling
        :c:func:`mps_telemetry_flush`, and, in the :term:`cool`
        :term:`variety`, terminates the program by calling
        :c:func:`abort`. You can change this behaviour with
        :c:func:`mps_lib_assert_fail_install`. For a discussion of the
        default behaviour, see :ref:`topic-error-assertion-handling`.

    .. warning::

        This function must not call any function in MPS, and it must
        not access memory managed by the MPS.

.. c:function:: extern mps_lib_assert_fail_t mps_lib_assert_fail_install(mps_lib_assert_fail_t handler)

    This function customises the behaviour of the default assertion handler
    in the ANSI Library module.  It is not otherwise required by the MPS
    and you need not implement it if you are providing an alternative plinth.
    
    If you're using the ANSI Library module, you can use this function
    to change the behaviour of the MPS when an assertion fails.  For
    example, you could terminate the program in the :term:`hot`
    :term:`variety` too.  (The MPS test programs do exactly that.)
    
    ``handler`` is the assertion handler to install.
    
    Returns the previously installed handler.

    .. warning::

        The installed assertion handler must not call any function in
        MPS, and it must not access memory managed by the MPS.

.. c:type:: typedef void (*mps_lib_assert_fail_t)(const char *, unsigned, const char *)

    The type of assertion handlers passed to and returned by
    :c:func:`mps_lib_assert_fail_install`.

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
    :c:func:`fputc` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <C1990>` §7.11.7.3).

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around :c:func:`fputc`.


.. c:function:: int mps_lib_fputs(const char *s, mps_lib_FILE *stream)

    Write a string to an output stream.

    ``s`` is the NUL-terminated string.

    ``stream`` is the stream.

    This function is intended to have the same semantics as the
    :c:func:`fputs` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <C1990>` §7.11.7.4).

    Return a non-negative integer if successful, or
    :c:func:`mps_lib_get_EOF` if not.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around :c:func:`fputs`.


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
    function :c:func:`memcmp`.

    ``s1`` and ``s2`` point to :term:`blocks` of memory to be
    compared.

    ``n`` is the :term:`size` of the blocks.

    Returns an integer that is greater than, equal to, or less than
    zero, accordingly as the block pointed to by ``s1`` is greater than,
    equal to, or less than the block pointed to by ``s2``.

    This function is intended to have the same semantics as the
    :c:func:`memcmp` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <C1990>` §7.11.4.1).

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around :c:func:`memcmp`.


.. c:function:: void *mps_lib_memcpy(void *dest, const void *source, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function :c:func:`memcpy`.

    ``dest`` points to the destination.

    ``source`` points to the source.

    ``n`` is the number of bytes to copy from ``source`` to ``dest``.

    Returns ``dest``.

    This function is intended to have the same semantics as the
    :c:func:`memcpy` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <C1990>` §7.11.2.1).

    The MPS never passes overlapping blocks to
    :c:func:`mps_lib_memcpy`.

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around :c:func:`memcpy`.


.. c:function:: void *mps_lib_memset(void *s, int c, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function :c:func:`memset`.

    ``s`` points to the :term:`block` to fill with the byte ``c``.

    ``c`` is the byte to fill with (when converted to ``unsigned char``).

    ``n`` is the :term:`size` of the block.

    Returns ``s``.

    This function is intended to have the same semantics as the
    :c:func:`memset` function of the ANSI C Standard (:ref:`ISO/IEC
    9899:1990 <C1990>` §7.11.6.1).

    .. note::

        In the ANSI Library module, ``mpsliban.c``, this is a simple
        wrapper around :c:func:`memset`.

    .. note::

        The MPS does not use this at present, but it may be required
        in future.


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
