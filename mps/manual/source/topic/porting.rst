.. _topic-porting:

Porting the MPS
===============

This chapter lists the steps involved in porting the MPS to a new
operating system, processor architecture, or compiler. It assumes that
you are familiar with :ref:`guide-build` and the :ref:`topic-platform`
chapter.


Platform code
-------------

Pick two-character codes for the new platform's operating system,
processor architecture, and compiler toolchain, as described under
:ref:`topic-platform`, and concatenate them to get a six-character
platform code "``osarct``".


Functional modules
------------------

The MPS requires platform-specific implementations of the functional
modules in the list below. You'll probably find that it's unnecessary
to port them all: unless the new platform is very exotic, some of the
existing implementations ought to be usable. In most cases there is a
generic ("ANSI") implementation of the module, that uses only the
features of the Standard C Library. These generic implementations are
partially functional or non-functional, but can be used as a starting
point for a new port if none of the existing implementations is
usable.

#. The **clock** module provides fast high-resolution clocks for use
   by the :term:`telemetry system`.

   See :ref:`design-clock` for the design, and ``clock.h`` for the
   interface. The interface consists only of type declarations and
   macro definitions, so there is no implementation.

   The header falls back to the clock functions from the
   :term:`plinth` if there is no platform-specific interface. See
   :c:func:`mps_clock` and :c:func:`mps_clocks_per_sec`.

#. The **lock** module provides binary locks that ensure that only a
   single :term:`thread` may be running with a lock held, and
   recursive locks, where the same thread may safely take the lock
   again without deadlocking.

   See :ref:`design-lock` for the design, and ``lock.h`` for the
   interface. There are implementations for POSIX in ``lockix.c``, and
   Windows in ``lockw3.c``.

   There is a generic implementation in ``lockan.c``, which cannot
   actually take any locks and so only works for a single thread.

#. The **memory protection** module applies :term:`protection` to
   areas of :term:`memory (2)`, ensuring that attempts to read or
   write from those areas cause :term:`protection faults`, and
   implements the means for the MPS to catch and handle these faults.

   See :ref:`design-prot` for the design, and ``prot.h`` for the
   interface. There are implementations for POSIX in ``protix.c`` plus
   ``protsgix.c``, Windows in ``protw3.c``, and macOS using Mach in
   ``protix.c`` plus ``protxc.c``.

   There is a generic implementation in ``protan.c``, which can't
   provide memory protection, so it forces memory to be scanned until
   there is no further need to protect it. This means it can't support
   incremental collection, and has no control over pause times.

#. The **mutator context** module figures out what the :term:`mutator`
   was doing when it caused a :term:`protection fault`, so that access
   to a protected region of memory can be handled, or when a thread
   was suspended, so that its :term:`registers` and :term:`control
   stack` can be scanned.

   See :ref:`design-prmc` for the design, and ``prmc.h`` for the
   interface. There are implementations on Unix, Windows, and macOS for
   IA-32 and x86-64.

   There is a generic implementation in ``prmcan.c``, which can't
   provide these features, and so only supports a single thread.

#. The **stack probe** module checks that there is enough space on the
   :term:`control stack` for the MPS to complete any operation that it
   might start. The purpose is to provoke a stack overflow exception,
   if necessary, before taking the arena lock.

   See :ref:`design-sp` for the design, and ``sp.h`` for the
   interface. There are implementations on Windows on IA-32 in
   ``spi3w3.c`` and x86-64 in ``spi6w3.c``.

   There is a generic implementation in ``span.c``, which can't
   provide this feature, and so is only suitable for use with a client
   program that does not handle stack overflow faults, or does not
   call into the MPS from the handler.

#. The **stack and register scanning** module :term:`scans` the
   :term:`registers` and :term:`control stack` of the thread that
   entered the MPS.

   See :ref:`design-stack-scan` for the design, ``ss.h`` for the
   interface, and ``ss.c`` for a generic implementation that makes
   assumptions about the platform (in particular, that the stack grows
   downwards and :c:func:`setjmp` reliably captures the registers; see
   the design for details).

#. The **thread manager** module suspends and resumes :term:`threads`,
   so that the MPS can gain exclusive access to :term:`memory (2)`,
   and so that it can scan the :term:`registers` and :term:`control
   stack` of suspended threads.

   See :ref:`design-thread-manager` for the design, and ``th.h`` for
   the interface. There are implementations for POSIX in ``thix.c``
   plus ``pthrdext.c``, macOS using Mach in ``thxc.c``, Windows in
   ``thw3.c``.

   There is a generic implementation in ``than.c``, which necessarily
   only supports a single thread.

#. The **virtual mapping** module reserves :term:`address space` from
   the operating system (and returns it), and :term:`maps <mapping>`
   address space to :term:`main memory` (and unmaps it).

   See :ref:`design-vm` for the design, and ``vm.h`` for the
   interface. There are implementations for POSIX in ``vmix.c``, and
   Windows in ``vmw3.c``. There is a generic implementation in
   ``vman.c``, which fakes virtual memory by calling :c:func:`malloc`.


Platform detection
------------------

The new platform must be detected in ``mpstd.h`` and preprocessor
constants like :c:macro:`MPS_WORD_WIDTH` defined. See
:ref:`design-config` for the design of this header, and
:ref:`topic-platform-interface` for the list of preprocessor constants
that may need to be defined. For example::

    /* "Predefined Macros" from "Visual Studio 2010" on MSDN
     * <http://msdn.microsoft.com/en-us/library/b0084kay(v=vs.100).aspx>.
     * Note that Win32 includes 64-bit Windows!
     * We use the same alignment as MS malloc: 16, which is used for XMM
     * operations.
     * See MSDN -> x64 Software Conventions -> Overview of x64 Calling Conventions
     * <http://msdn.microsoft.com/en-us/library/ms235286> 
     */

    #elif defined(_MSC_VER) && defined(_WIN32) && defined(_WIN64) && defined(_M_X64) && !defined(__POCC__)
    #if defined(CONFIG_PF_STRING) && ! defined(CONFIG_PF_W3I6MV)
    #error "specified CONFIG_PF_... inconsistent with detected w3i6mv"
    #endif
    #define MPS_PF_W3I6MV
    #define MPS_PF_STRING   "w3i6mv"
    #define MPS_OS_W3
    #define MPS_ARCH_I6
    #define MPS_BUILD_MV
    #define MPS_T_WORD      unsigned __int64
    #define MPS_T_ULONGEST  unsigned __int64
    #define MPS_WORD_WIDTH  64
    #define MPS_WORD_SHIFT  6
    #define MPS_PF_ALIGN    16

The comment should justify the platform test (with reference to
documentation or to the output of a command like ``gcc -E -dM``), and
explain any unusual definitions. For example, here we need to explain
the choice of 16 bytes for :c:macro:`MPS_PF_ALIGN`, since normally a
64-bit platform requires 8-byte :term:`alignment`.


Platform configuration
----------------------

The new platform may be configured, if necessary, in ``config.h``. See
:ref:`design-config` for the design of this header. Avoid
platform-specific configuration if possible, to reduce the risk of
errors being introduced on one platform and not detected when other
platforms are tested.


Module selection
----------------

In ``mps.c``, add a section for the new platform. This must test the
platform constant ``MPS_PF_OSARCT`` that is now defined in
``mpstd.h``, and then include all the module sources for the platform.
For example::

    /* Linux on x86-64 with GCC or Clang */

    #elif defined(MPS_PF_LII6GC) || defined(MPS_PF_LII6LL)

    #include "lockix.c"     /* Posix locks */
    #include "thix.c"       /* Posix threading */
    #include "pthrdext.c"   /* Posix thread extensions */
    #include "vmix.c"       /* Posix virtual memory */
    #include "protix.c"     /* Posix protection */
    #include "protsgix.c"   /* Posix signal handling */
    #include "prmci6.c"     /* x86-64 mutator context */
    #include "prmcix.c"     /* Posix mutator context */
    #include "prmclii6.c"   /* x86-64 for Linux mutator context */
    #include "span.c"       /* generic stack probe */


Makefile
--------

Add a makefile even if you expect to use an integrated development
environment (IDE) like Visual Studio or Xcode. Makefiles make it
easier to carry out continuous integration and delivery, and are less
likely to stop working because of incompatibilities between IDE
versions.

On Unix platforms, the makefile must be named ``osarct.gmk``, and must
define ``PFM`` to be the platform code, ``MPMPF`` to be the list of
platform modules (the same files included by ``mps.c``), and ``LIBS``
to be the linker options for any libraries required by the test cases.
Then it must include the compiler-specific makefile and ``comm.gmk``.
For example, ``lii6ll.gmk`` looks like this:

.. code-block:: make

    PFM = lii6ll

    MPMPF = \
        lockix.c \
        prmci6.c \
        prmcix.c \
        prmclii6.c \
        protix.c \
        protsgix.c \
        pthrdext.c \
        span.c \
        thix.c \
        vmix.c

    LIBS = -lm -lpthread

    include ll.gmk
    include comm.gmk

If the platform needs specific compilation options, then define
``PFMDEFS`` accordingly, but avoid this if at all possible. We
recommend in :ref:`guide-build` that users compile the MPS using a
simple command like ``cc -c mps.c``, and we suggest that they can
improve performance by compiling the MPS and their object format in
the same compilation unit. These steps would be more complicated if
the MPS required particular compilation options.

On Windows, the makefile must be named ``osarct.nmk``, and must define
``PFM`` to be the platform code, and ``MPMPF`` to be the list of
platform modules (the same files included by ``mps.c``) in square
brackets. Then it must include ``commpre.nmk``, the compiler-specific
makefile and ``commpost.nmk``. For example, ``w3i6mv.nmk`` looks like
this:

.. code-block:: none

    PFM = w3i6mv

    MPMPF = \
        [lockw3] \
        [mpsiw3] \
        [prmci6] \
        [prmcw3] \
        [prmcw3i6] \
        [protw3] \
        [spw3i6] \
        [thw3] \
        [vmw3]

    !INCLUDE commpre.nmk
    !INCLUDE mv.nmk
    !INCLUDE commpost.nmk


Porting strategy
----------------

Start the port by selecting existing implementations of the functional
modules, using the generic implementations where nothing else will do.
Then check that the "smoke tests" pass, by running:

.. code-block:: none

    make -f osarct.gmk testrun    # Unix
    nmake /f osarct.nmk testrun   # Windows

Most or all of the test cases should pass at this point. If you're
using the generic threading implementation, then the multi-threaded
test cases are expected to fail. If you're using the generic lock
implementation, then the lock utilization test case ``lockut`` is
expected to fail. If you're using the generic memory protection
implementation, all the tests that rely on incremental collection are
expected to fail. See ``tool/testcases.txt`` for a database of test
cases and the configurations in which they are expected to pass.

Now that there is a working system to build on, porting the necessary
modules to the new platform can be done incrementally. It's a good
idea to measure the performance as you go along (for example, using
the ``gcbench`` benchmark) to check that the new memory protection
module is effective.


Update the documentation
------------------------

These sections of the manual should be updated to mention the new
platform:

- :ref:`guide-build`
- :ref:`topic-platform`

In addition, if aspects of the port were especially tricky, then
consider writing a design document (see :ref:`design`) justifying the
implementation.


Contribute
----------

Consider contributing the new platform to the MPS. See
:ref:`contributing`.
