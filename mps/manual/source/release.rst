.. _release-notes:

Release notes
=============


.. _release-notes-1.118:

Release 1.118.0
---------------

New features
............

#. New supported platforms:

   * ``lia6gc`` (Linux, ARM64, GCC).
   * ``lia6ll`` (Linux, ARM64, Clang/LLVM).
   * ``xca6ll`` (macOS, ARM64, Clang/LLVM).

   See :ref:`topic-platform-limitations` for limitations in the
   support for Apple Hardened Runtime on ``xca6ll``.

#. Support removed for platform:

   * ``xci3ll`` (macOS, IA-32, Clang/LLVM).

   Support for this platform was removed in macOS 10.15 (Catalina),
   making it inconvenient to develop and test.

#. The arena's :term:`spare commit limit` is now expressed as a
   fraction of the :term:`committed <mapped>` memory (rather than a
   fixed size, as previously). This allows the :term:`spare committed
   memory` to scale with the :term:`working set` size. Set the spare
   commit limit using the keyword argument :c:macro:`MPS_KEY_SPARE` to
   :c:func:`mps_arena_create_k`, or the function
   :c:func:`mps_arena_spare_set`, and query it using the function
   :c:func:`mps_arena_spare`.

#. A new support tool, the **monitor**, implements a graphical user
   interface for analysis of :ref:`topic-telemetry`. This is
   experimental: the implementation is likely to change in future
   versions of the MPS. See :ref:`design-monitor`.

#. The new function :c:func:`mps_pool_walk` visits all areas of
   :term:`formatted objects` in a pool using the
   :ref:`topic-scanning-protocol`. This allows the client program to
   safely update references in the visited objects.

#. The new function :c:func:`mps_addr_object` allows clients to
   discover the base pointer of an object from a pointer to anywhere
   inside the object. This is intended to support stack tracing and
   debugging for client programs that allocate their code on the
   heap.

#. A :term:`virtual memory arena` can now be configured to call
   functions when it acquires a new chunk of :term:`address space`,
   and when it returns a chunk of address space to the operation
   system. This is intended to support dynamic function tables in
   Windows. See :ref:`topic-arena-extension`.


Interface changes
.................

#. The deprecated pool class MV (Manual Variable), and the deprecated
   functions ``mps_mv_free_size`` and ``mps_mv_size`` have been
   removed. Use :ref:`pool-mvff` and the generic functions
   :c:func:`mps_pool_free_size` and :c:func:`mps_pool_total_size`
   instead.

#. The deprecated function :c:func:`mps_tramp` has been removed. The
   MPS has had no need for a trampoline, and client programs have not
   needed to take any special precautions before calling functions in
   the MPS, since version 1.111.

#. The deprecated functions :c:func:`mps_arena_expose`,
   :c:func:`mps_arena_unsafe_expose_remember_protection` and
   :c:func:`mps_arena_unsafe_expose_restore_protection` have been
   removed. If you need access to protected memory for debugging a
   fatal error, use :c:func:`mps_arena_postmortem` instead.

#. The deprecated reservoir functions
   :c:func:`mps_ap_fill_with_reservoir_permit`,
   :c:func:`mps_reservoir_available`, :c:func:`mps_reservoir_limit`,
   :c:func:`mps_reservoir_limit_set` and
   :c:func:`mps_reserve_with_reservoir_permit`, have been removed.

#. The deprecated function :c:func:`mps_fix` has been removed. Use
   the macro :c:func:`MPS_FIX12` instead.

#. The deprecated function :c:func:`mps_telemetry_control` has been
   removed. Use :c:func:`mps_telemetry_get`,
   :c:func:`mps_telemetry_set` and :c:func:`mps_telemetry_reset`
   instead.

#. The keyword argument ``MPS_KEY_SPARE_COMMIT_LIMIT`` to
   :c:func:`mps_arena_create_k`, and the functions
   :c:func:`mps_arena_spare_commit_limit` and
   :c:func:`mps_arena_spare_commit_limit_set` are now deprecated. Use
   :c:macro:`MPS_KEY_SPARE`, :c:func:`mps_arena_spare` and
   :c:func:`mps_arena_spare_set` instead.

#. The format of the :term:`telemetry stream` has changed: Booleans
   are no longer packed into bitfields, but are emitted as unsigned
   bytes. This makes it possible to decode the telemetry stream using
   the Python function |unpack|_.

   .. |unpack| replace:: :py:func:`struct.unpack`
   .. _unpack: https://docs.python.org/3/library/struct.html#struct.unpack

#. The functions :c:func:`mps_formatted_objects_walk` and
   :c:func:`mps_amc_apply` are deprecated in favour of the new
   function :c:func:`mps_pool_walk`.


Other changes
.............

#. On FreeBSD and Linux, if the MPS handles a signal while the client
   program is blocked in a system call, the system call is
   automatically restarted and does not fail with ``EINTR``. See
   :ref:`topic-thread-signal`.

#. On FreeBSD and Linux, the MPS signal handlers no longer modify
   ``errno``. See `GitHub issue #10`_.

   .. _GitHub issue #10: https://github.com/Ravenbrook/mps/issues/10

#. The MPS now builds with Clang 10 and
   ``-Wimplicit-int-float-conversion``. See `GitHub issue #51`_.

   .. _GitHub issue #51: https://github.com/Ravenbrook/mps/issues/51

#. The MPS now builds with ``clang -Wcomma``. See `GitHub issue #47`_.

   .. _GitHub issue #47: https://github.com/Ravenbrook/mps/issues/47


.. _release-notes-1.117:

Release 1.117.0
---------------

New features
............

#. On FreeBSD, Linux and macOS, the MPS is now able to run in the
   child process after ``fork()``. See :ref:`topic-thread-fork`.

#. The MPS now supports Windows Vista or later; it no longer supports
   Windows XP. (Microsoft's own support for Windows XP `expired in
   April 2014`_.) This is so that we can use |InitOnceExecuteOnce|_ to
   ensure thread-safe initialization.

   .. _expired in April 2014: https://www.microsoft.com/en-gb/windowsforbusiness/end-of-xp-support
   .. |InitOnceExecuteOnce| replace:: ``InitOnceExecuteOnce()``
   .. _InitOnceExecuteOnce: https://docs.microsoft.com/en-us/windows/desktop/api/synchapi/nf-synchapi-initonceexecuteonce


Interface changes
.................

#. The pool class MV (Manual Variable) is now deprecated.


Other changes
.............

#. References from the MPS's own stack frames no longer :term:`pin
   <pinning>` objects allocated by the :term:`client program` in
   moving pools, which prevented them from moving. See job003525_.

   .. _job003525: https://www.ravenbrook.com/project/mps/issue/job003525/

#. Creation of :term:`arenas` is now thread-safe on Windows. See
   job004056_.

   .. _job004056: https://www.ravenbrook.com/project/mps/issue/job004056/

#. :ref:`pool-awl` and :ref:`pool-lo` pools now detect (and assert on)
   invalid :term:`exact references`. See job004070_.

   .. _job004070: https://www.ravenbrook.com/project/mps/issue/job004070/

#. The MPS now compiles without warnings on GCC version 7 with
   ``-Wextra``. See job004076_.

   .. _job004076: https://www.ravenbrook.com/project/mps/issue/job004076/

#. Deprecated function :c:func:`mps_arena_roots_walk` no longer causes
   :c:func:`mps_arena_formatted_objects_walk` to miss some objects. See
   job004090_.

   .. _job004090: https://www.ravenbrook.com/project/mps/issue/job004090/


.. _release-notes-1.116:

Release 1.116.0
---------------

New features
............

#. The MPS now measures the mortality of a :term:`generation` each
   time it is :term:`collected`, and maintains a moving average. This
   means that it is no longer important to provide an accurate
   estimate of the mortality when creating a :term:`generation chain`
   by calling :c:func:`mps_chain_create`.

#. The MPS no longer supports Linux 2.4 and 2.5. (These versions used
   LinuxThreads_ instead of POSIX threads; all major distributions
   have long since ceased to support these versions and so it is no
   longer convenient to test against them.) See
   :ref:`guide-overview-platforms`.

   .. _LinuxThreads: https://en.wikipedia.org/wiki/LinuxThreads

#. New function :c:func:`mps_arena_postmortem` assists with postmortem
   debugging.

#. New function :c:func:`mps_arena_busy` assists debugging of re-entry
   errors in dynamic function table callbacks on Windows on x86-64.


Interface changes
.................

#. The pool class :ref:`pool-snc` is no longer deprecated.

#. Allocation frames are no longer deprecated. See :ref:`topic-frame`.

#. On Linux and FreeBSD, it is now possible to configure the signals
   used to suspend and resume threads. See :ref:`topic-thread-signal`.


Other changes
.............

#. It is now possible to register a :term:`thread` with the MPS
   multiple times on OS X, thus supporting the use case where a
   program that does not use the MPS is calling into MPS-using code
   from multiple threads. (This was already supported on other
   platforms.) See job003559_.

   .. _job003559: https://www.ravenbrook.com/project/mps/issue/job003559/

#. The function :c:func:`mps_arena_formatted_objects_walk` walks the
   :term:`formatted objects` in all :term:`pools`. Previously this was
   not implemented for :ref:`pool-ams` pools. See job003738_.

   .. _job003738: https://www.ravenbrook.com/project/mps/issue/job003738/

#. Objects in :ref:`pool-snc` pools are no longer scanned after their
   :term:`allocation frame` is popped, and so do not keep objects in
   automatically managed pools alive. See job003883_.

   .. _job003883: https://www.ravenbrook.com/project/mps/issue/job003883/

#. When the MPS :term:`collects` a set of :term:`generations`, it
   :term:`condemns <condemned set>` only the :term:`blocks` in those
   generations. Previously, it also condemned blocks that happened to
   share a region of memory with blocks currently or formerly
   allocated in those generations. See job004000_.

   .. _job004000: https://www.ravenbrook.com/project/mps/issue/job004000/

#. Memory in :term:`allocation points` no longer contributes to the
   decision to start a :term:`garbage collection`, avoiding wasted
   work repeatedly collecting generations with very small capacities.
   See job004007_.

   .. _job004007: https://www.ravenbrook.com/project/mps/issue/job004007/

#. The MPS no longer considers :term:`collecting <collect>` the world
   again, without allowing the :term:`client program` to run first.
   See job004011_.

   .. _job004011: https://www.ravenbrook.com/project/mps/issue/job004011/

#. :term:`Roots` created by :c:func:`mps_root_create_thread_scanned`
   no longer cause an assertion failure. See job004036_.

   .. _job004036: https://www.ravenbrook.com/project/mps/issue/job004036/

#. The MPS test suite now compiles and passes with GCC 6.1. See job004037_.

   .. _job004037: https://www.ravenbrook.com/project/mps/issue/job004037/

#. The MPS no longer passes an uninitialized variable to
   :c:func:`thread_swap_exception_ports` on OS X. See job004040_.

   .. _job004040: https://www.ravenbrook.com/project/mps/issue/job004040/


.. _release-notes-1.115:

Release 1.115.0
---------------

New features
............

#. The MPS now provides control over the maximum time that operations
   within an arena may pause the :term:`client program` for. This can
   be specified by the new function :c:func:`mps_arena_pause_time_set`
   or by passing the new keyword argument
   :c:macro:`MPS_KEY_PAUSE_TIME` to :c:func:`mps_arena_create_k`. The
   current value can be retrieved by the new function
   :c:func:`mps_arena_pause_time`.

   The maximum pause time defaults to 0.1 seconds. For the old
   behaviour (whereby the MPS always returned to the :term:`client
   program` as soon as possible), set it to zero.

#. New supported platforms ``fri3ll`` (FreeBSD, IA-32, Clang/LLVM)
   and ``fri6ll`` (FreeBSD, x86-64, Clang/LLVM).

#. When creating an :ref:`pool-amc` pool, :c:func:`mps_pool_create_k`
   accepts the new keyword argument :c:macro:`MPS_KEY_EXTEND_BY`,
   specifying the minimum size of the memory segments that the pool
   requests from the :term:`arena`.

#. The function :c:func:`mps_arena_create_k` accepts two new
   :term:`keyword arguments`. :c:macro:`MPS_KEY_COMMIT_LIMIT`
   sets the :term:`commit limit` for the arena, and
   ``MPS_KEY_SPARE_COMMIT_LIMIT`` sets the :term:`spare
   commit limit` for the arena.

#. New area scanning functions :c:func:`mps_scan_area`,
   :c:func:`mps_scan_area_masked`, :c:func:`mps_scan_area_tagged`,
   :c:func:`mps_scan_area_tagged_or_zero` for use when scanning,
   especially when scanning threads and :term:`tagged references`.

#. New thread root functions :c:func:`mps_root_create_thread`,
   :c:func:`mps_root_create_thread_tagged`, and
   :c:func:`mps_root_create_thread_scanned` allow flexible scanning of
   thread stacks and registers in any format, with convenient
   implementations provided for :term:`tagged references`.

#. New function :c:func:`mps_root_create_table_tagged` for tables of roots
   containing :term:`tagged references`.

#. New area root functions :c:func:`mps_root_create_area` and
   :c:func:`mps_root_create_area_tagged` for areas of memory
   that can be scanned by area scanning functions.


Interface changes
.................

#. The pool class MV (Manual Variable) is no longer deprecated.

#. The type of pool classes is now :c:type:`mps_pool_class_t`. The old
   name :c:type:`mps_class_t` is still available via a ``typedef``,
   but is deprecated.

#. The functions ``mps_mv_free_size``, ``mps_mv_size``,
   :c:func:`mps_mvff_free_size`, :c:func:`mps_mvff_size`,
   :c:func:`mps_mvt_free_size` and :c:func:`mps_mvt_size` are now
   deprecated in favour of the generic functions
   :c:func:`mps_pool_free_size` and :c:func:`mps_pool_total_size`.

#. The function :c:func:`mps_root_create_reg` is deprecated in favour
   of :c:func:`mps_root_create_thread_tagged`.

#. The function :c:func:`mps_root_create_table_masked` is deprecated in
   favour of :c:func:`mps_root_create_table_tagged`.

#. The :ref:`pool-snc` pool class now implements
   :c:func:`mps_pool_total_size` and :c:func:`mps_pool_free_size`.

#. The (undocumented) reservoir functions
   :c:func:`mps_ap_fill_with_reservoir_permit`,
   :c:func:`mps_reservoir_available`, :c:func:`mps_reservoir_limit`,
   :c:func:`mps_reservoir_limit_set`, and
   :c:func:`mps_reserve_with_reservoir_permit`, together with the
   ``has_reservoir_permit`` arguments to :c:func:`mps_sac_alloc` and
   :c:func:`MPS_SAC_ALLOC_FAST` are now deprecated.


Other changes
.............

#. :c:func:`mps_arena_committed` now returns a meaningful value (the
   amount of memory marked as in use in the page tables) for
   :term:`client arenas`. See job001887_.

   .. _job001887: https://www.ravenbrook.com/project/mps/issue/job001887/

#. :ref:`pool-amc` pools now assert that exact references into the
   pool are aligned to the pool's alignment. See job002175_.

   .. _job002175: https://www.ravenbrook.com/project/mps/issue/job002175/

#. Internal calculation of the address space available to the MPS no
   longer takes time proportional to the number of times the arena has
   been extended, speeding up allocation when memory is tight. See
   job003814_.

   .. _job003814: https://www.ravenbrook.com/project/mps/issue/job003814/

#. Setting :c:macro:`MPS_KEY_SPARE` for a :ref:`pool-mvff` pool now
   works. See job003870_.
   
   .. _job003870: https://www.ravenbrook.com/project/mps/issue/job003870/

#. In the :term:`hot` (production) variety,
   :c:func:`mps_pool_free_size` now returns the correct result for
   :ref:`pool-awl` and :ref:`pool-lo` pools. See job003884_.

   .. _job003884: https://www.ravenbrook.com/project/mps/issue/job003884/

#. When the arena is out of memory and cannot be extended without
   hitting the :term:`commit limit`, the MPS now returns
   :c:macro:`MPS_RES_COMMIT_LIMIT` rather than substituting
   :c:macro:`MPS_RES_RESOURCE`. See job003899_.
   
   .. _job003899: https://www.ravenbrook.com/project/mps/issue/job003899/

#. Unfinalizable objects can no longer be registered for finalization.
   Previously the objects would be registered but never finalized. See
   job003865_.

   .. _job003865: https://www.ravenbrook.com/project/mps/issue/job003865/

#. :c:func:`mps_arena_has_addr` now returns the correct result for
   objects allocated from the :ref:`pool-mfs`, MV (Manual Variable),
   and :ref:`pool-mvff` pools. See job003866_.

   .. _job003866: https://www.ravenbrook.com/project/mps/issue/job003866/

#. The MPS can now make use of :term:`spare committed memory` even if
   it is :term:`mapped` at an unhelpful address, by unmapping it and
   remapping at a better address. See job003898_.

   .. _job003898: https://www.ravenbrook.com/project/mps/issue/job003898/

#. :c:func:`mps_arena_step` now always considers starting a new
   :term:`garbage collection` if the remaining idle time is long
   enough to complete it. (Previously, if there was already a
   collection in progress when :c:func:`mps_arena_step` was called, it
   would finish the collection but not consider starting a new one.)
   See job003934_.

   .. _job003934: https://www.ravenbrook.com/project/mps/issue/job003934/

#. The MPS no longer carries out :term:`garbage collections` when there
   is no collection work to be done. See job003938_.

   .. _job003938: https://www.ravenbrook.com/project/mps/issue/job003938/

#. The MPS is less aggressive in its use of hardware memory protection
   to maintain :term:`write barrier` to speed up future collections.
   This is particularly important for OS X, where memory protection
   operations are very expensive.  See job003371_ and job003975_.

#. The MPS coalesces memory protection, reducing the number of system
   calls. This markedly improves real run time on operating systems
   where memory protection operations are very expensive, such as OS
   X, but also has a significant effect on Linux. See job003371_ and
   job003975_.

   .. _job003371: https://www.ravenbrook.com/project/mps/issue/job003371/
   .. _job003975: https://www.ravenbrook.com/project/mps/issue/job003975/


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

#. The logic for deciding which generations should be collected has
   changed. Now, a chain may be scheduled for collection if the new
   size of *any* of its generations exceeds its capacity, and when a
   chain is collected, all generations are collected up to, and
   including, the highest generation whose new size exceeds its
   capacity. This ensures that all generations are collected reliably
   on chains where there is no allocation into the nursery generation.
   See :ref:`topic-collection-schedule`.

   (Previously, only the nursery generation in each chain
   was considered, and a chain was collected up to, but not including,
   the lowest generation whose new size was within its capacity.)

   As a result of this change, we recommend that you retune your
   generation sizes. (This is not necessary, but may improve
   performance.)

#. New pool introspection functions :c:func:`mps_pool_free_size` and
   :c:func:`mps_pool_total_size`.


Interface changes
.................

#. The granularity with which the arena manages memory can now be
   specified using the :c:macro:`MPS_KEY_ARENA_GRAIN_SIZE` keyword
   argument to :c:func:`mps_arena_create_k`. See
   :c:func:`mps_arena_class_cl` and :c:func:`mps_arena_class_vm`.

#. There is now a default value (currently 256 \ :term:`megabytes`)
   for the :c:macro:`MPS_KEY_ARENA_SIZE` keyword argument to
   :c:func:`mps_arena_create_k` when creating a virtual memory arena.
   See :c:func:`mps_arena_class_vm`.

#. The keyword argument :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` now
   defaults to ``TRUE`` in order to better support the general case:
   the value ``FALSE`` is appropriate only when you know that all
   references are exact. See :ref:`pool-ams`.

#. There is now a default value for the
   :c:macro:`MPS_KEY_AWL_FIND_DEPENDENT` keyword argument to
   :c:func:`mps_pool_create_k` when creating an :ref:`pool-awl` pool.
   The default value is a function that always returns ``NULL``
   (meaning that there is no dependent object).

#. It is now possible to configure the alignment of objects allocated
   in an MV (Manual Variable) pool, by passing the
   :c:macro:`MPS_KEY_ALIGN` keyword argument to
   :c:func:`mps_pool_create_k`.

#. The :ref:`pool-mvff` pool class takes a new keyword argument
   :c:macro:`MPS_KEY_SPARE`. This specifies the maximum proportion of
   memory that the pool will keep spare for future allocations.

#. The alignment requirements for :ref:`pool-mvff` and :ref:`pool-mvt`
   pools have been relaxed on the platforms ``w3i3mv`` and ``w3i6mv``.
   On all platforms it is now possible to specify alignments down to
   ``sizeof(void *)`` as the alignment for pools of these classes.

#. The sizes of the templates in a :c:type:`mps_pool_debug_option_s`
   structure no longer have to be related to the alignment of the
   pools that they are used with. This makes it easier to reuse these
   structures.


Other changes
.............

#. The :ref:`pool-ams` pool class no longer triggers the assertion
   ``!AMS_IS_INVALID_COLOUR(seg, i)`` under rare circumstances
   (namely, detaching an :term:`allocation point` from a :term:`grey`
   segment when :c:macro:`MPS_KEY_AMS_SUPPORT_AMBIGUOUS` is
   ``FALSE``). See job001549_.

   .. _job001549: https://www.ravenbrook.com/project/mps/issue/job001549/

#. :c:func:`mps_arena_roots_walk` no longer triggers an assertion
   failure when run twice in succession. See job003496_.

   .. _job003496: https://www.ravenbrook.com/project/mps/issue/job003496/

#. The alignment of :ref:`pool-awl` pools is now configurable via the
   object format, as documented, and is no longer always
   :c:macro:`MPS_PF_ALIGN`. See job003745_.

   .. _job003745: https://www.ravenbrook.com/project/mps/issue/job003745/

#. The debugging version of the :ref:`pool-mvff` pool class,
   :c:func:`mps_class_mvff_debug`, no longer triggers an assertion
   failure if you allocate a large object. See job003751_.

   .. _job003751: https://www.ravenbrook.com/project/mps/issue/job003751/

#. :program:`mpseventtxt` now successfully processes a telemetry log
   containing multiple labels associated with the same address. See
   job003756_.

   .. _job003756: https://www.ravenbrook.com/project/mps/issue/job003756/

#. :ref:`pool-ams`, :ref:`pool-awl` and :ref:`pool-lo` pools get
   reliably collected, even in the case where the pool is the only
   pool on its generation chain and is allocating into some generation
   other than the nursery. See job003771_.

   .. _job003771: https://www.ravenbrook.com/project/mps/issue/job003771/

#. Allocation into :ref:`pool-awl` pools again reliably provokes
   garbage collections of the generation that the pool belongs to. (In
   version 1.113, the generation would only be collected if a pool of
   some other class allocated into it.) See job003772_.

   .. _job003772: https://www.ravenbrook.com/project/mps/issue/job003772/

#. All unreachable objects in :ref:`pool-lo` pools are finalized.
   (Previously, objects on a segment attached to an allocation point
   were not finalized until the allocation point was full.) See
   job003773_.

   .. _job003773: https://www.ravenbrook.com/project/mps/issue/job003773/

#. The :ref:`pool-mvt` and :ref:`pool-mvff` pool classes are now
   around 25% faster (in our benchmarks) than they were in version
   1.113.

#. The default assertion handler in the default :term:`plinth` now
   flushes the telemetry stream before aborting. See
   :c:func:`mps_lib_assert_fail`.

#. Garbage collection performance is substantially improved in the
   situation where the arena has been extended many times. Critical
   operations now take time logarithmic in the number of times the
   arena has been extended (rather than linear, as in version 1.113
   and earlier). See job003554_.

   .. _job003554: https://www.ravenbrook.com/project/mps/issue/job003554/


.. _release-notes-1.113:

Release 1.113.0
---------------

New features
............

#. In previous releases there was an implicit connection between
   blocks allocated by :ref:`pool-awl` and :ref:`pool-lo` pools, and
   blocks allocated by other automatically managed pool classes.

   In particular, blocks allocated by AWL and LO pools were garbage
   collected together with blocks allocated by :ref:`pool-ams` pools,
   and blocks allocated by :ref:`pool-amc` pools in generation 1 of
   their chains.

   This is no longer the case: to arrange for blocks to be collected
   together you need to ensure that they are allocated in the *same*
   generation chain, using the :c:macro:`MPS_KEY_CHAIN` and
   :c:macro:`MPS_KEY_GEN` keyword arguments to
   :c:func:`mps_pool_create_k`.

   So if you have code like this::

       res = mps_pool_create(&my_amc, arena, mps_class_amc(), my_chain);
       res = mps_pool_create(&my_awl, arena, mps_class_awl());

   and you want to retain the connection between these pools, then you
   must ensure that they use the same generation chain::

       MPS_ARGS_BEGIN(args) {
         MPS_ARGS_ADD(args, MPS_KEY_CHAIN, my_chain);
         res = mps_pool_create_k(&my_amc, arena, mps_class_amc(), args);
       } MPS_ARGS_END(args);

       MPS_ARGS_BEGIN(args) {
         MPS_ARGS_ADD(args, MPS_KEY_CHAIN, my_chain);
         MPS_ARGS_ADD(args, MPS_KEY_GEN, 1);
         res = mps_pool_create_k(&my_awl, arena, mps_class_awl(), args);
       } MPS_ARGS_END(args);


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
   the default :term:`plinth` by installing an assertion handler. See
   :c:func:`mps_lib_assert_fail_install`.

#. Functions that take a variable number of arguments
   (:c:func:`mps_arena_create`, :c:func:`mps_pool_create`,
   :c:func:`mps_ap_create`) and their ``va_list`` alternatives
   (:c:func:`mps_arena_create_v` etc.) are now deprecated in favour of
   functions that use a :term:`keyword argument` interface
   (:c:func:`mps_arena_create_k`, :c:func:`mps_pool_create_k`,
   :c:func:`mps_ap_create_k`).

   Similarly, the object format variant structures
   (:c:type:`mps_fmt_A_s` etc.) and the functions that take them as
   arguments (:c:func:`mps_fmt_create_A` etc.) are now deprecated in
   favour of :c:func:`mps_fmt_create_k`.

   The new interfaces provide better reporting of errors, default
   values for arguments, and forward compatibility. See
   :ref:`topic-keyword`.

   The old interfaces continue to be supported for now, but new
   features will become available through the keyword interface only.

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

#. An :ref:`pool-mvt` pool no longer triggers an assertion failure
   when it runs out of space on its reserved block queue. See
   job003486_.

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

#. The new pool class :ref:`pool-mfs` provides manually managed
   allocation of fixed-size objects.

#. The new pool class :ref:`pool-mvt` provides manually managed
   allocation of variable-size objects using a *temporal fit*
   allocation policy (that is, objects that are allocated togther are
   expected to be freed together).


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

#. The pool classes MV (Manual Variable) and :ref:`pool-snc` are now
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


.. _release-notes-1.110:

Release 1.110.0
---------------

New features
............

#. New supported platforms:

   * ``fri6gc`` (FreeBSD, x86-64, GCC)
   * ``lii6gc`` (Linux, x86-64, GCC)
   * ``w3i6mv`` (Windows, x86-64, Microsoft Visual C)
   * ``xci3ll`` (OS X, IA-32, Clang/LLVM)
   * ``xci6gc`` (OS X, x86-64, GCC)
   * ``xci6ll`` (OS X, x86-64, Clang/LLVM)

#. Support removed for platforms:

   * ``iam4cc`` (Irix 6, MIPS R4000, MIPSpro C)
   * ``lii3eg`` (Linux, IA-32, EGCS)
   * ``lippgc`` (Linux, PowerPC, GCC)
   * ``o1alcc`` (OSF/1, Alpha, Digital C)
   * ``o1algc`` (OSF/1, Alpha, GCC)
   * ``s7ppmw`` (System 7, PowerPC, MetroWerks C)
   * ``sos8gc`` (Solaris, SPARC 8, GCC)
   * ``sos9sc`` (Solaris, SPARC 9, SunPro C)
   * ``sus8gc`` (SunOS, SPARC 8, GCC)
   * ``xcppgc`` (OS X, PowerPC, GCC)

#. On Unix platforms, the MPS can now be built and installed by
   running ``./configure && make install``. See :ref:`guide-build`.

#. The MPS can be compiled in a single step via the new source file
   ``mps.c``. This also allows you to compile the MPS in the same
   compilation unit as your object format, allowing the compiler to
   perform global optimizations between the two. See
   :ref:`guide-build`.

#. The set of build varieties has been reduced to three: the
   :term:`cool` variety for development and debugging, the :term:`hot`
   variety for production, and the :term:`rash` variety for people who
   like to live dangerously. See :ref:`topic-error-variety`.

#. The environment variable :envvar:`MPS_TELEMETRY_CONTROL` can now be
   set to a space-separated list of event kinds. See
   :ref:`topic-telemetry`.

#. Telemetry output is now emitted to the file named by the
   environment variable :envvar:`MPS_TELEMETRY_FILENAME`, if it is
   set. See :ref:`topic-telemetry`.


Interface changes
.................

#. Deprecated constants ``MPS_MESSAGE_TYPE_FINALIZATION``,
   ``MPS_MESSAGE_TYPE_GC`` and ``MPS_MESSAGE_TYPE_GC_START`` have been
   removed. Use :c:func:`mps_message_type_finalization`,
   :c:func:`mps_message_type_gc` and
   :c:func:`mps_message_type_gc_start` instead.

#. Deprecated constants ``MPS_RANK_AMBIG``, ``MPS_RANK_EXACT`` and
   ``MPS_RANK_WEAK`` have been removed. Use :c:func:`mps_rank_ambig`,
   :c:func:`mps_rank_exact` and :c:func:`mps_rank_weak` instead.

#. Deprecated functions with names starting ``mps_space_`` have been
   removed. Use the functions with names starting ``mps_arena_``
   instead.
