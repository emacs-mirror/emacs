.. _topic-porting:

Porting the MPS
===============

This chapter lists the steps involved in porting the MPS to a new
operating system, processor architecture, or compiler. It assumes that
you are familiar with :ref:`guide-build` and the :ref:`topic-platform`
chapter.


Platform code
-------------

Pick two-character codes for your operating system, processor
architecture, and compiler toolchain, as described under
:ref:`topic-platform`, and concatenate them to get a six-character
platform code "``osarct``".


Functional modules
------------------

The MPS requires platform-specific implementations of the functional
modules in the list below. You'll probably find that you don't need to
port them all: unless your platform is very exotic, some of the
existing implementations ought to be usable. In most cases there is an
"ANSI" implementation of the module, that uses only the features of
the Standard C Library. These "ANSI" implementations are partially
functional or non-functional, but can be used as a starting point for
a new port if none of the existing implementations is usable.

#. The **lock** module provides binary locks that ensure that only a
   single :term:`thread` may be running with a lock held, and
   recursive locks, where the same thread may safely take the lock
   again without deadlocking.

   See :ref:`design-lock` for the design, and ``lock.h`` for the
   interface. There are implementations for Linux in ``lockli.c``,
   POSIX in ``lockix.c``, and Windows in ``lockw3.c``. There is an
   "ANSI" implementation in ``lockan.c``, which cannot actually take
   any locks and so only works for a single thread.

#. The **threads** module suspends and resumes :term:`threads`, so
   that the MPS can gain exclusive access to :term:`memory (2)`.

   See :ref:`design-thread-manager` for the design, and ``th.h`` for
   the interface. There are implementations for POSIX in ``thix.c``
   plus ``pthrdext.c``, OS X using Mach in ``thxc.c``, Windows in
   ``thw3.c``. There is an "ANSI" implementation in ``than.c``, which
   necessarily only supports a single thread.

#. The **virtual mapping** module reserves :term:`address space` from
   the operating system (and returns it), and :term:`maps <mapping>`
   address space to :term:`main memory` (and unmaps it).

   See :ref:`design-vm` for the design, and ``vm.h`` for the
   interface. There are implementations for POSIX in ``vmix.c``, and
   Windows in ``vmw3.c``. There is an "ANSI" implementation in
   ``vman.c``, which fakes virtual memory by calling ``malloc``.

#. The **memory protection** module applies :term:`protection` to
   areas of :term:`memory (2)`, ensuring that attempts to read or
   write from those areas cause :term:`protection faults`, and
   implements the means for the MPS to catch and handle these faults.

   See :ref:`design-prot` for the design, and ``prot.h`` for the
   interface. There are implementations for POSIX in ``protix.c`` plus
   ``protsgix.c``, Linux in ``protli.c``, Windows in ``protw3.c``, and
   OS X using Mach in ``protxc.c``. There is an "ANSI" implementation
   in ``protan.c``, which can't provide memory protection, so it
   forces memory to be scanned until that there is no further need to
   protect it.

#. The **protection mutator context** module figures out what the
   :term:`mutator` was doing when it caused a :term:`protection
   fault`, so that the access can be emulated as described at
   :ref:`pool-awl-barrier`.

   See :ref:`design-prot` for the design, and ``prot.h`` for the
   interface. There are eight implementations, a typical example being
   ``prmci3w3.c`` for Windows on IA-32. There is an "ANSI"
   implementation in ``prmcan.c``, which can't provide this feature.

#. The **stack probe** module checks that there is enough space on the
   :term:`control stack` for the MPS to complete any operation that it
   might start. The purpose is to provoke a stack overflow exception,
   if necessary, before taking the arena lock.

   See ``sp.h`` for the interface. There are implementations on
   Windows on IA-32 in ``spi3w3.c`` and x86-64 in ``spi6w3.c``. There
   is an "ANSI" implementation in ``span.c``, which can't provide this
   feature.

#. The **stack and register scanning** module :term:`scans` the
   :term:`registers` and :term:`control stack` of a thread.

   See :ref:`design-thread-manager` for the design, and ``ss.h`` for
   the interface. There are implementations for POSIX on IA-32 in
   ``ssixi3.c`` and x86-64 in ``ssixi6.c``, and for Windows with
   Microsoft Visual C/C++ on IA-32 in ``ssw3i3mv.c`` and x86-64 in
   ``ssw3i6mv.c``. There is an "ANSI" implementation in ``ssan.c``,
   which calls ``setjmp`` to spill the registers.


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
:ref:`design-config` for the design of this header. You should try to
keep the amount of platform-specific configuration as small as you
can, to reduce the risk of errors being introduced in one platform and
so not detected when other platforms are tested.


Module selection
----------------

In ``mps.c``, add a section for your platform. This must test the
platform constant ``MPS_PF_OSARCT`` that you defined in ``mpstd.h``,
and then include all the module sources for your platform. For
example::

    /* Linux on 64-bit Intel with GCC or Clang */

    #elif defined(MPS_PF_LII6GC) || defined(MPS_PF_LII6LL)

    #include "lockli.c"     /* Linux locks */
    #include "thix.c"       /* Posix threading */
    #include "pthrdext.c"   /* Posix thread extensions */
    #include "vmix.c"       /* Posix virtual memory */
    #include "protix.c"     /* Posix protection */
    #include "protli.c"     /* Linux protection */
    #include "proti6.c"     /* 64-bit Intel mutator context */
    #include "prmci6li.c"   /* 64-bit Intel for Linux mutator context */
    #include "span.c"       /* generic stack probe */
    #include "ssixi6.c"     /* Posix on 64-bit Intel stack scan */


Makefile
--------

Add a makefile even if you expect to use an integrated development
environment like Visual Studio or Xcode. Makefiles make it easier to
carry out continuous integration and delivery.

The makefile must be named ``osarct.gmk``, and must define ``PFM`` to
be the platform code, ``MPMPF`` to be the list of platform modules
(the same files included by ``mps.c``), and ``LIBS`` to be the linker
options for the libraries required by your port. Then it must include
the compiler-specific makefile and ``comm.gmk``. For example,
``xci6ll.gmk`` looks like this::

    PFM = xci6ll

    MPMPF = \
        lockix.c \
        prmci6xc.c \
        proti6.c \
        protix.c \
        protxc.c \
        span.c \
        ssixi6.c \
        thxc.c \
        vmix.c

    LIBS = -lm

    include ll.gmk
    include comm.gmk

If you need platform-specific compilation options, then define
``PFMDEFS`` accordingly, but you should do your best to avoid this. We
recommend in :ref:`guide-build` that users compile the MPS using a
simple command like ``cc -c mps.c``, and we suggest that they can
improve performance by compiling the MPS and their object format in
the same compilation unit. These steps would be more complicated
if the MPS required particular compilation options.


Test
----

Check that the "smoke tests" pass on your platform::

    make -f osarct.gmk testrun


Update the documentation
------------------------

The following sections of the manual need to be updated to mention the
new platform:

- :ref:`guide-build`
- :ref:`topic-platforms`

In addition, if aspects of the port were especially tricky, then
consider writing a design document (see :ref:`design`) justifying the
implementation.


Contribute
----------

Consider contributing your new platform to the MPS. See
:ref:`contributing`.
