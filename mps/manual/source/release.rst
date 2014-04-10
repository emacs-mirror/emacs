.. _release-notes:

Release notes
=============

.. _release-notes-1.114:

Release 1.114.0
---------------

New features
............

#. :term:`Ambiguous <ambiguous reference>` :term:`interior pointers`
   now keep objects in :ref:`pool-amc` and :ref:`pool-amcz` pools
   alive.

   This means that if the compiler optimizes away a pointer to the
   base of an object, leaving an interior pointer as the only
   reference keeping the object alive, this does not cause the object
   to be incorrectly collected. Or, if you are writing your own
   compiler, you can now perform such an optimization safely.

   If you require the old behaviour (in which ambiguous interior
   pointers were ignored) then you can set the
   :c:macro:`MPS_KEY_INTERIOR` keyword argument to ``FALSE`` when
   calling :c:func:`mps_pool_create_k`.


Other changes
.............

#. The :ref:`pool-ams` pool class no longer triggers the assertion
   ``!AMS_IS_INVALID_COLOUR(seg, i)`` under rare circumstances
   (namely, detaching an :term:`allocation point` from a :term:`grey`
   segment when :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` is
   ``FALSE``). See job001549_.

   .. _job001549: https://www.ravenbrook.com/project/mps/issue/job001549/

#. The alignment of :ref:`pool-awl` pools is now configurable via the
   object format, as documented, and is no longer always
   :c:macro:`MPS_PF_ALIGN`. See job003745_.

   .. _job003745: https://www.ravenbrook.com/project/mps/issue/job003745/


.. _release-notes-1.113:

Release 1.113.0
---------------

Interface changes
.................

#. When creating a list of keyword arguments, there is no longer any
   need to call :c:func:`MPS_ARGS_DONE`. See :ref:`topic-keyword`.

#. When creating an automatically managed pool using
   :c:func:`mps_pool_create_k`, it is no longer necessary to pass in a
   generation chain. The arena has a default generation chain and this
   is used by all automatically managed pools where no generation
   chain was specified.

#. It is now possible to specify a generation chain for
   :ref:`pool-awl` and :ref:`pool-lo` pool classes, by using the
   optional :c:macro:`MPS_KEY_CHAIN` keyword argument to
   :c:func:`mps_pool_create_k`.

#. It is now possible to specify which generation the :ref:`pool-ams`,
   :ref:`pool-awl`, and :ref:`pool-lo` pool classes allocate new
   objects into, using the optional :c:macro:`MPS_KEY_GEN` keyword
   argument to :c:func:`mps_pool_create_k`.


Other changes
.............

#. The MPS now retains some unused memory instead of returning it to
   the operating system. This reduces unnecessary overhead due to
   system calls, thrashing the operating system's page table, and
   zeroing memory when re-allocated. See job003700_.

   .. _job003700: https://www.ravenbrook.com/project/mps/issue/job003700/


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


.. _release-notes-1.111:

Release 1.111.0
---------------

New features
............

#. Reporting features have been removed from the :ref:`mpseventcnv
   <telemetry-mpseventcnv>` utility. Instead, the telemetry system
   comes with two new utility programs to assist with reporting and
   analysis: :ref:`mpseventtxt <telemetry-mpseventtxt>` converts an
   event stream into human-readable form, and :ref:`mpseventsql
   <telemetry-mpseventsql>` loads an event stream into a SQLite
   database for further analysis. See :ref:`topic-telemetry`.

#. The new pool class MFS provide manually managed allocation of
   fixed-size objects. See :ref:`pool-mfs`.

#. The new pool class MVT provide manually managed allocation of
   variable-size objects using a *temporal fit* allocation policy
   (that is, objects that are allocated togther are expected to be
   freed together). See :ref:`pool-mvt`.


Interface changes
.................

#. It is no longer necessary for client programs to use
   :c:func:`mps_tramp` to ensure that exceptions due to barrier hits
   are caught. This function is now deprecated.

#. You can set the environment variable
   :envvar:`MPS_TELEMETRY_CONTROL` to ``all`` to make the telemetry
   system output all events. See :ref:`topic-telemetry`.

#. New functions :c:func:`mps_telemetry_get`,
   :c:func:`mps_telemetry_set` and :c:func:`mps_telemetry_reset`
   provide a more convenient interface to telemetry control than
   :c:func:`mps_telemetry_control`, which is now deprecated. See
   :ref:`topic-telemetry`.

#. The pool classes :ref:`pool-mv` and :ref:`pool-snc` are now
   deprecated.

#. Allocation frames are now deprecated. See :ref:`topic-frame`.

#. Additionally, the functions :c:func:`mps_arena_expose`,
   :c:func:`mps_arena_unsafe_expose_remember_protection`,
   :c:func:`mps_arena_unsafe_restore_protection`,
   :c:func:`mps_arena_roots_walk`, and :c:func:`mps_fix` are now
   deprecated.


Other changes
.............

#. :c:func:`mps_arena_step` no longer unclamps the arena as a side
   effect. If the arena is clamped or parked before calling
   :c:func:`mps_arena_step`, it is clamped afterwards. See job003320_.

   .. _job003320: https://www.ravenbrook.com/project/mps/issue/job003320/

#. The ambiguous stack scanner, :c:func:`mps_stack_scan_ambig`, no
   longer asserts on Linux when there are multiple threads. See
   job003412_.

   .. _job003412: https://www.ravenbrook.com/project/mps/issue/job003412/

#. It is no longer possible for the "ramp" allocation pattern,
   :c:func:`mps_alloc_pattern_ramp()`, to get stuck. Now
   :c:func:`mps_ap_alloc_pattern_end` reliably clears this pattern.
   See job003454_.

   .. _job003454: https://www.ravenbrook.com/project/mps/issue/job003454/

#. The build system now correctly detects the FreeBSD operating system
   running on the x86-64 architecture, for FreeBSD version 9.1 or
   later. See job003473_.

   .. _job003473: https://www.ravenbrook.com/project/mps/issue/job003473/
