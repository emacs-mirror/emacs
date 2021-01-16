.. _topic-platform:

Platforms
=========

.. index::
   pair: platform; code

Platform codes
--------------

The MPS uses a six-character platform code to express a combination of
operating system, CPU architecture, and compiler toolchain. Each
six-character code breaks down into three pairs of characters:
``OSARCT``. The first pair of characters names the operating system:

======  ================  ====================
``OS``  Operating system  Constant
======  ================  ====================
``fr``  FreeBSD           :c:macro:`MPS_OS_FR`
``li``  Linux             :c:macro:`MPS_OS_LI`
``w3``  Windows           :c:macro:`MPS_OS_W3`
``xc``  macOS             :c:macro:`MPS_OS_XC`
======  ================  ====================

The second pair of characters names the processor architecture:

======  ======================  ======================
``AR``  Processor architecture  Constant
======  ======================  ======================
``a6``  ARM64                   :c:macro:`MPS_ARCH_A6`
``i3``  Intel/AMD IA-32         :c:macro:`MPS_ARCH_I3`
``i6``  Intel/AMD x86-64        :c:macro:`MPS_ARCH_I6`
======  ======================  ======================

The third pair of characters names the compiler toolchain:

======  =======================  =======================
``CT``  Compiler toolchain       Constant
======  =======================  =======================
``gc``  GNU Compiler collection  :c:macro:`MPS_BUILD_GC`
``ll``  Clang/LLVM               :c:macro:`MPS_BUILD_LL`
``mv``  Microsoft Visual C/C++   :c:macro:`MPS_BUILD_MV`
======  =======================  =======================

In each case the aspect of the platform can be tested by checking
whether the preprocessor constant in the third column in the table
is defined, and the full platform can be tested by checking
whether the corresponding ``MPS_PF_`` preprocessor constant is
defined. For example, "``xci6ll``" platform corresponds to the
:c:macro:`MPS_PF_XCI6LL` preprocessor constant.

Not all combinations of operating system, processor architecture,
and compiler are supported.


.. index::
   single: platform; interface

.. _topic-platform-interface:

Platform interface
------------------

::

    #include "mpstd.h"


.. c:macro:: MPS_ARCH_A6

    A :term:`C` preprocessor macro that indicates, if defined, that
    the target processor architecture of the compilation is a member
    of the ARM64 family of 64-bit processors.


.. c:macro:: MPS_ARCH_I3

    A :term:`C` preprocessor macro that indicates, if defined, that
    the target processor architecture of the compilation is a member
    of the IA-32 Intel/AMD family of 32-bit processors.


.. c:macro:: MPS_ARCH_I6

    A :term:`C` preprocessor macro that indicates, if defined, that
    the target processor architecture of the compilation is a member
    of the x86-64 Intel/AMD family of 64-bit processors.

    .. note::

        The MPS is not supported on IA-64 (Itanium).


.. c:macro:: MPS_BUILD_GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled by the C compiler from the GNU Compiler
    Collection (GCC).


.. c:macro:: MPS_BUILD_LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled by Clang, the C compiler from the LLVM (Low
    Level Virtual Machine) system.


.. c:macro:: MPS_BUILD_MV

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled by the C compiler from Microsoft Visual
    Studio.


.. c:macro:: MPS_OS_FR

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled on a FreeBSD operating system.


.. c:macro:: MPS_OS_LI

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled on a Linux operating system.


.. c:macro:: MPS_OS_W3

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled on a Windows operating system.


.. c:macro:: MPS_OS_XC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the MPS was compiled on an macOS operating system.


.. c:macro:: MPS_PF_ALIGN

    A :term:`C` preprocessor macro that expands to an integer giving
    the :term:`natural alignment` of the :term:`platform`.


.. c:macro:: MPS_PF_FRI3GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the FreeBSD operating system, the
    IA-32 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_FRI3LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the FreeBSD operating system, the
    IA-32 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_PF_FRI6GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the FreeBSD operating system, the
    x86-64 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_FRI6LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the FreeBSD operating system, the
    x86-64 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_PF_LIA6LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Linux operating system, the
    ARM64 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_PF_LII3GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Linux operating system, the
    IA-32 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_LII6GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Linux operating system, the
    x86-64 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_LII6LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Linux operating system, the
    x86-64 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_PF_STRING

    A :term:`C` preprocessor macro that names the :term:`platform` for
    which the MPS was built.


.. c:macro:: MPS_PF_W3I3MV

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Windows operating system, the
    IA-32 processor architecture, and the Microsoft Visual C/C++
    compiler.


.. c:macro:: MPS_PF_W3I6MV

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Windows operating system, the
    x86-64 processor architecture, and the Microsoft Visual C/C++
    compiler.


.. c:macro:: MPS_PF_XCA6LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the macOS operating system, the
    ARM64 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_PF_XCI3GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the macOS operating system, the
    IA-32 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_XCI3LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the macOS operating system, the
    IA-32 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_PF_XCI6GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the macOS operating system, the
    x86-64 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_XCI6LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the macOS operating system, the
    x86-64 processor architecture, and the Clang/LLVM compiler.


.. c:macro:: MPS_T_ULONGEST

    A :term:`C` preprocessor macro that expands to the name of the
    largest unsigned integral type.

    The exact identity of this type is
    :term:`platform`\-dependent. Typical identities are ``unsigned
    long`` and ``unsigned __int_64``.


.. c:macro:: MPS_T_WORD

    A :term:`C` preprocessor macro that expands to the name of an
    unsigned integral type that is the same size as an :term:`object
    pointer`, so that ``sizeof(MPS_T_WORD) == sizeof(void*)``.

    The exact identity of this type is
    :term:`platform`\-dependent. Typical identities are ``unsigned
    long`` and ``unsigned __int_64``.


.. c:macro:: MPS_WORD_SHIFT

    A :term:`C` preprocessor macro that expands to the logarithm to
    base 2 of the constant :c:macro:`MPS_WORD_WIDTH`, so that ``1 <<
    MPS_WORD_SHIFT == MPS_WORD_WIDTH``.

    The value is platform-dependent. Typical values are 5 and 6.


.. c:macro:: MPS_WORD_WIDTH

    A :term:`C` preprocessor macro that expands to the width in bits
    of the type :c:type:`MPS_T_WORD`, so that ``MPS_WORD_WIDTH ==
    sizeof(MPS_T_WORD) * CHAR_BIT``.

    This value is platform-dependent. It is always a power of 2:
    typical values are 32 and 64.


.. index::
   single: platform; historical codes

Historical platform codes
-------------------------

The platform codes in the tables below were in use in older versions
of the Memory Pool System, but are not currently supported.

Formerly supported operating systems:

======  ===========================  =============
``OS``  Operating system             Constant            
======  ===========================  =============
``i5``  Irix 5 or 6 (old ABI)        ``MPS_OS_I5``
``ia``  Irix 6 (new ABI)             ``MPS_OS_IA``
``o1``  OSF/1 aka Tru64              ``MPS_OS_O1``
``s7``  Macintosh System 7, 8, or 9  ``MPS_OS_S7``
``so``  Solaris                      ``MPS_OS_SO``
``su``  SunOS                        ``MPS_OS_SU``
======  ===========================  =============

Formerly supported processor architectures:

======  ======================  ===============
``AR``  Processor architecture  Constant     
======  ======================  ===============
``i4``  Intel/AMD IA-32 [1]_    ``MPS_ARCH_I4``
``al``  Digital Alpha           ``MPS_ARCH_AL``
``m2``  MIPS R2000              ``MPS_ARCH_M2``
``m4``  MIPS R4000              ``MPS_ARCH_M4``
``m6``  Motorola 68000          ``MPS_ARCH_M6``
``pp``  PowerPC                 ``MPS_ARCH_PP``
``s8``  SPARC V8                ``MPS_ARCH_S8``
``s9``  SPARC V9 (32-bit)       ``MPS_ARCH_S9``
======  ======================  ===============

Formerly supported compiler toolchains:

======  =======================================  ================
``CT``  Compiler toolchain                       Constant       
======  =======================================  ================
``ac``  Macintosh Programmer's Workshop C/C++    ``MPS_BUILD_AC``
``cc``  The "native" C compiler [2]_             ``MPS_BUILD_CC``
``cx``  SunPro C CXREF tool                      ``MPS_BUILD_CX``
``eg``  Experimental GNU Compiler System (EGCS)  ``MPS_BUILD_EG``
``gp``  GCC with profiling                       ``MPS_BUILD_GP``
``lc``  LCC                                      ``MPS_BUILD_LC``
``mw``  Metrowerks CodeWarrior                   ``MPS_BUILD_MW``
``pc``  Pelles C                                 ``MPS_BUILD_PC``
``sc``  SunPro C                                 ``MPS_BUILD_SC``
======  =======================================  ================

.. note::

    .. [1] Obsolete: the MPS used to make a distinction between the
           80386 and 80486 processor architectures.

    .. [2] This was the MIPSpro C compiler on IRIX; and the Digital C
           Compiler on OSF/1.


.. index::
   single: platform; historical list

Historical platform list
------------------------

This is the full list of platforms that have ever been supported by
the Memory Pool System, with their current status.

==========  =======================
Platform    Status
==========  =======================
``fri3gc``  Supported
``fri3ll``  Supported
``fri4gc``  Corrected to ``fri3gc``
``fri6gc``  Supported
``fri6ll``  Supported
``i5m2cc``  *Not supported*
``iam4cc``  *Not supported*
``lia6ll``  Supported
``lii3eg``  *Not supported*
``lii3gc``  Supported
``lii4gc``  Corrected to ``lii3gc``
``lii6gc``  Supported
``lii6ll``  Supported
``lippgc``  *Not supported*
``o1alcc``  *Not supported*
``o1algc``  *Not supported*
``s7m6mw``  *Not supported*
``s7ppac``  *Not supported*
``s7ppmw``  *Not supported*
``sos8cx``  *Not supported*
``sos8gc``  *Not supported*
``sos8gp``  *Not supported*
``sos9sc``  *Not supported*
``sus8gc``  *Not supported*
``w3almv``  *Not supported*
``w3i3m9``  *Not supported*
``w3i3mv``  Supported
``w3i3pc``  *Not supported*
``w3i6mv``  Supported
``w3i6pc``  *Not supported*
``w3ppmv``  *Not supported*
``xca6ll``  Supported
``xci3gc``  *Not supported*
``xci3ll``  Supported
``xci6gc``  *Not supported*
``xci6ll``  Supported
``xcppgc``  *Not supported*
==========  =======================
