.. _topic-platform:

================================
Supporting MPS on a new platform
================================

-----------------------
Declared in ``mpstd.h``
-----------------------

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
    the MPS was compiled by the C compiler from the LLVM (Low Level
    Virtual Machine) system.


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
    the MPS was compiled on an OS X operating system.


.. c:macro:: MPS_PF_ALIGN

    A :term:`C` preprocessor macro that expands to an integer giving
    the :term:`natural alignment` of the :term:`platform`.


.. c:macro:: MPS_PF_FRI3GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the FreeBSD operating system, the
    IA-32 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_FRI6GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the FreeBSD operating system, the
    x86-64 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_LII3GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Linux operating system, the
    IA-32 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_LII6GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the Linux operating system, the
    x86-64 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_STRING

    A :term:`C` preprocessor macro that names the :term:`platform` for
    which the MPS was built.

    It expands to a string "``abcdef``" consisting of six characters
    that are digits or lower-case letters. The first two characters
    name the operating system:

    ======  ================  ====================
    ``ab``  Operating system  Constant
    ======  ================  ====================
    ``fr``  FreeBSD           :c:macro:`MPS_OS_FR`
    ------  ----------------  --------------------
    ``li``  Linux             :c:macro:`MPS_OS_LI`
    ------  ----------------  --------------------
    ``w3``  Windows           :c:macro:`MPS_OS_W3`
    ------  ----------------  --------------------
    ``xc``  OS X              :c:macro:`MPS_OS_XC`
    ======  ================  ====================

    The second pair of characters name the processor architecture:

    ======  ======================  ======================
    ``cd``  Processor architecture  Constant
    ======  ======================  ======================
    ``i3``  Intel/AMD IA-32         :c:macro:`MPS_ARCH_I3`
    ------  ----------------------  ----------------------
    ``i6``  Intel/AMD x86-64        :c:macro:`MPS_ARCH_I6`
    ======  ======================  ======================

    The third pair of characters name the compiler:

    ======  ================  =======================
    ``ef``  Compiler          Constant
    ======  ================  =======================
    ``gc``  GCC               :c:macro:`MPS_BUILD_GC`
    ------  ----------------  -----------------------
    ``ll``  LLVM              :c:macro:`MPS_BUILD_LL`
    ------  ----------------  -----------------------
    ``mv``  Visual C/C++      :c:macro:`MPS_BUILD_MV`
    ======  ================  =======================

    In each case the aspect of the platform can be tested by checking
    whether the preprocessor constant in the third column in the table
    is defined, and the full platform can be tested by checking
    whether the corresponding ``MPS_PF_`` preprocessor constant is
    defined. For example, "``xci6ll``" platform corresponds to the
    :c:macro:`MPS_PF_XCI6LL` preprocessor constant.

    Not all combinations of operating system, processor architecture,
    and compiler are supported.


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


.. c:macro:: MPS_PF_XCI3GC

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the OS X operating system, the
    IA-32 processor architecture, and the GCC compiler.


.. c:macro:: MPS_PF_XCI3LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the OS X operating system, the
    IA-32 processor architecture, and the LLVM compiler.


.. c:macro:: MPS_PF_XCI6LL

    A :term:`C` preprocessor macro that indicates, if defined, that
    the :term:`platform` consists of the OS X operating system, the
    x86-64 processor architecture, and the LLVM compiler.


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


