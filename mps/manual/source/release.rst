.. _release-notes:

Release notes
=============

.. _release-notes-1.112:

Release 1.112.0
---------------

Interface changes
.................

1. In the :term:`hot` (production) variety, the default assertion handler
   now prints messages to standard error but does *not* terminate the
   program. Even though assertions indicate serious problems in the
   program, an end-user does not want an application to terminate when
   there is a chance to shut down safely and save work, or even to limp
   along indefinitely.  See :ref:`topic-error-assertion-handling`.

2. The behaviour when an assertion is triggered is now configurable in
   the standard ANSI :term:`plinth` by installing an assertion
   handler. See :c:func:`mps_lib_assert_fail_install`.

3. Functions that take a variable number of arguments
   (:c:func:`mps_arena_create`, :c:func:`mps_pool_create`,
   :c:func:`mps_fmt_create` and :c:func:`mps_ap_create`) are now
   deprecated in favour of functions that use a :term:`keyword
   argument` interface (:c:func:`mps_arena_create_k`,
   :c:func:`mps_pool_create_k`, :c:func:`mps_fmt_create_k` and
   :c:func:`mps_ap_create_k`). The new interface provides better
   reporting of errors, provides default values for arguments, and
   provides forward compatibility. See :ref:`topic-keyword`.

   (The old interface continues to be supported, but new features will
   only be available through the keyword interface.)

4. :ref:`pool-mfs` no longer refuses to manage blocks that are smaller
   than the platform alignment. It now rounds up smaller sizes
   internally if necessary.

5. :ref:`pool-mvt` now allows the client to specify the alignment of
   blocks allocated from the pool. Use the keyword argument
   :c:macro:`MPS_KEY_ALIGN` when creating a pool of class
   :c:func:`mps_class_mvt`.

6. On Windows, you can now request that the MPS allocate address space
   from the top down, allowing a 32-bit executable linked with
   ``/LARGEADDRESSAWARE`` to use the top half of the address space.
   Use the keyword argument :c:macro:`MPS_KEY_VMW3_TOP_DOWN` when
   creating an arena of class :c:func:`mps_arena_class_vm`.


Other changes
.............

1. On Windows, an execute exception no longer triggers an assertion.
   See job003301_.

   .. _job003301: https://www.ravenbrook.com/project/mps/issue/job003301/

2. Rehashing of large address-based hash tables no longer provokes a
   nursery collection that immediately renders the hash table stale
   again. See job003435_.

   .. _job003435: https://www.ravenbrook.com/project/mps/issue/job003435/

3. :ref:`pool-mvt` no longer triggers an assertion failure when it
   runs out of space on its reserved block queue. See job003486_.

   .. _job003486: https://www.ravenbrook.com/project/mps/issue/job003486/

