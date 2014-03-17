.. _release-notes:

Release notes
=============

.. _release-notes-1.113:

Release 1.113.0
---------------

Interface changes
.................

#. When creating a list of keyword arguments, there is no longer any
   need to call :c:macro:`MPS_ARGS_DONE`. See :ref:`topic-keyword`.


.. _release-notes-1.112:

Release 1.112.0
---------------

New features
............

#. New supported platform ``lii6ll`` (Linux, x86-64, Clang/LLVM).

#. On Windows, you can now request that the MPS allocate address space
   from the top down, allowing a 32-bit executable linked with
   ``/LARGEADDRESSAWARE`` to use the top half of the address space.
   Use the keyword argument :c:macro:`MPS_KEY_VMW3_TOP_DOWN` when
   creating an arena of class :c:func:`mps_arena_class_vm`.

#. On OS X, multi-threaded programs are now supported. See
   :ref:`topic-thread`.

#. On OS X, you can now debug the MPS using ``lldb``.


Interface changes
.................

#. In the :term:`hot` (production) variety, the default assertion handler
   now prints messages to standard error but does *not* terminate the
   program. Even though assertions indicate serious problems in the
   program, an end-user does not always want an application to terminate when
   there is a chance to shut down safely and save work, or even to limp
   along indefinitely.  See :ref:`topic-error-assertion-handling`.

#. The behaviour when an assertion is triggered is now configurable in
   the standard ANSI :term:`plinth` by installing an assertion
   handler. See :c:func:`mps_lib_assert_fail_install`.

#. Functions that take a variable number of arguments
   (:c:func:`mps_arena_create`, :c:func:`mps_pool_create`,
   :c:func:`mps_ap_create`, :c:func:`mps_fmt_create_A`) and their
   ``va_list`` alternatives (:c:func:`mps_arena_create_v` etc.) are
   now deprecated in favour of functions that use a :term:`keyword
   argument` interface (:c:func:`mps_arena_create_k`,
   :c:func:`mps_pool_create_k`, :c:func:`mps_ap_create_k`,
   :c:func:`mps_fmt_create_k`). The new interface provides better
   reporting of errors, provides default values for arguments, and
   provides forward compatibility. See :ref:`topic-keyword`.

   The old interface continues to be supported, but new features will
   become available through the keyword interface only.

#. :ref:`pool-mfs` pools no longer refuse to manage blocks that are
   smaller than the platform alignment. They now round up smaller
   sizes internally if necessary.

#. :ref:`pool-mvt` pools now allow the client to specify the alignment
   of blocks. Use the keyword argument :c:macro:`MPS_KEY_ALIGN` when
   creating a pool of class :c:func:`mps_class_mvt`.

#. On OS X, signals are no longer used for handling memory protection
   exceptions. This means that programs are free to handle ``SIGBUS``,
   but must not install a thread-local Mach exception handler for
   ``EXC_BAD_ACCESS`` exceptions. See :ref:`topic-thread-signal`.

#. On OS X, when debugging with ``gdb``, you no longer need to turn on
   ``dont-handle-bad-access`` or to request special handling of
   ``SIGBUS``.


Other changes
.............

#. On Windows, an execute exception no longer triggers an assertion.
   See job003301_.

   .. _job003301: https://www.ravenbrook.com/project/mps/issue/job003301/

#. Rehashing of large address-based hash tables no longer provokes a
   nursery collection that immediately renders the hash table stale
   again. See job003435_.

   .. _job003435: https://www.ravenbrook.com/project/mps/issue/job003435/

#. :ref:`pool-mvt` no longer triggers an assertion failure when it
   runs out of space on its reserved block queue. See job003486_.

   .. _job003486: https://www.ravenbrook.com/project/mps/issue/job003486/

#. The ``-i`` and ``-o`` options no longer cause
   :program:`mpseventsql` to crash. See job003507_.

   .. _job003507: https://www.ravenbrook.com/project/mps/issue/job003507/

#. On Windows, telemetry files now have correct clock values.
   Previously the top 32 bits were incorrectly output as zero. See
   job003519_.

   .. _job003519: https://www.ravenbrook.com/project/mps/issue/job003519/

#. On 64-bit Windows, it's no longer possible to get a stack overflow
   exception while the MPS is holding the arena lock. See job003640_.

   .. _job003640: https://www.ravenbrook.com/project/mps/issue/job003640/
